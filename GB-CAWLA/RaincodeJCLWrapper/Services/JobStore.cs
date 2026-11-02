using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading.Channels;
using RaincodeJCLWrapper.Models;

namespace RaincodeJCLWrapper.Services;

public sealed class JobStore : IDisposable
{
    private readonly Dictionary<string, JobRecord> jobs = new(StringComparer.Ordinal);
    private readonly SemaphoreSlim gate = new(1, 1);
    private readonly string directory;
    private readonly ApplicationCatalog catalog;
    private readonly ILogger<JobStore> logger;
    private readonly FileStream instanceLock;
    private bool accepting = true;
    private static readonly JsonSerializerOptions JsonOptions = new(JsonSerializerDefaults.Web)
    {
        WriteIndented = true,
        Converters = { new JsonStringEnumConverter() }
    };

    public Channel<string> Queue { get; } = Channel.CreateUnbounded<string>(
        new UnboundedChannelOptions { SingleReader = false, SingleWriter = false });

    public JobStore(IConfiguration configuration, IHostEnvironment environment,
        ApplicationCatalog catalog, ILogger<JobStore> logger)
    {
        this.catalog = catalog;
        this.logger = logger;
        directory = Path.GetFullPath(configuration["Wrapper:StoragePath"] ?? "Data",
            environment.ContentRootPath);
        Directory.CreateDirectory(directory);
        instanceLock = new FileStream(Path.Combine(directory, ".instance.lock"),
            FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None);
        foreach (var file in Directory.EnumerateFiles(directory, "*.json"))
        {
            var job = JsonSerializer.Deserialize<JobRecord>(File.ReadAllText(file), JsonOptions)
                ?? throw new InvalidDataException($"Empty job record: {file}");
            if (!Guid.TryParseExact(job.JobId, "N", out _) ||
                Path.GetFileNameWithoutExtension(file) != job.JobId ||
                !ApplicationCatalog.IsValidName(job.Application) ||
                !ApplicationCatalog.IsValidName(job.JobName) || !Enum.IsDefined(job.Status))
                throw new InvalidDataException($"Invalid job record: {file}");
            if (job.IsActive)
            {
                job = job with
                {
                    Status = JobState.Interrupted,
                    EndTime = DateTimeOffset.UtcNow,
                    Error = "The wrapper restarted before completion was recorded. Verify Raincode before resubmitting."
                };
                Persist(job);
                logger.LogWarning("Recovered unfinished job {JobId} as Interrupted", job.JobId);
            }
            jobs.Add(job.JobId, job);
        }
    }

    public Task<JobRecord> SubmitAsync(SubmitRequest request) => EnqueueAsync(request);

    public async Task<JobRecord> RestartAsync(string id, RestartRequest request)
    {
        if (!ApplicationCatalog.IsValidRestartStep(request.RestartStep))
            throw new ApiException(400, "restartStep must be a step name or dot-separated step/PROC path, such as STEP1.MYPROC.");
        var original = await GetAsync(id);
        return await EnqueueAsync(new SubmitRequest(original.Application, original.JobName, request.RestartStep), id);
    }

    private async Task<JobRecord> EnqueueAsync(SubmitRequest request, string? parentJobId = null)
    {
        if (!ApplicationCatalog.IsValidName(request.Application) || !ApplicationCatalog.IsValidName(request.JobName))
            throw new ApiException(400, "Application and jobName must be 1-64 letters, digits, underscores or hyphens, starting with a letter or digit.");
        if (request.RestartStep is not null && !ApplicationCatalog.IsValidRestartStep(request.RestartStep))
            throw new ApiException(400, "restartStep must be a step name or dot-separated step/PROC path, such as STEP1.MYPROC.");

        var app = catalog.Get(request.Application);
        if (!Directory.Exists(app.RootPath))
            throw new ApiException(503, "The configured Raincode application root is unavailable.");
        var jclFile = ApplicationCatalog.ResolveChild(app.JclPath, request.JobName + ".jcl");
        if (!File.Exists(jclFile))
            throw new ApiException(404, $"JCL '{request.JobName}.jcl' was not found.");

        await gate.WaitAsync();
        try
        {
            if (!accepting)
                throw new ApiException(503, "The wrapper is shutting down.");
            if (parentJobId is not null && Find(parentJobId).Status is not (JobState.Failed or JobState.Completed))
                throw new ApiException(409, "Only failed or completed jobs can be restarted. Interrupted jobs require reconciliation first.");
            if (jobs.Values.Any(job => job.IsActive &&
                job.Application.Equals(app.Application, StringComparison.OrdinalIgnoreCase) &&
                job.JobName.Equals(request.JobName, StringComparison.OrdinalIgnoreCase)))
                throw new ApiException(409, "This application/job already has a queued or running submission.");
            if (jobs.Values.Any(job => job.Status == JobState.Interrupted &&
                job.Application.Equals(app.Application, StringComparison.OrdinalIgnoreCase) &&
                job.JobName.Equals(request.JobName, StringComparison.OrdinalIgnoreCase)))
                throw new ApiException(409, "An interrupted execution needs reconciliation. Verify Raincode has stopped, then acknowledge the interrupted job.");

            var id = Guid.NewGuid().ToString("N");
            var job = new JobRecord
            {
                JobId = id,
                Application = app.Application,
                JobName = request.JobName,
                RestartStep = request.RestartStep,
                ParentJobId = parentJobId,
                JclFile = jclFile,
                LogDirectory = Path.Combine(app.LogPath, request.JobName, id)
            };
            Directory.CreateDirectory(job.LogDirectory);
            File.WriteAllText(Path.Combine(job.LogDirectory, "API.log"),
                $"{DateTimeOffset.UtcNow:O} Submission accepted: {app.Application}/{request.JobName}; restartStep={request.RestartStep}; parentJobId={parentJobId}{Environment.NewLine}");
            Persist(job);
            jobs.Add(id, job);
            if (!Queue.Writer.TryWrite(id))
                throw new InvalidOperationException("The execution queue is unavailable.");
            logger.LogInformation("Accepted job {JobId}: {Application}/{JobName}", id, job.Application, job.JobName);
            return job;
        }
        finally { gate.Release(); }
    }

    public async Task<JobRecord> GetAsync(string id)
    {
        await gate.WaitAsync();
        try { return Find(id); }
        finally { gate.Release(); }
    }

    public async Task<JobRecord> UpdateAsync(string id, Func<JobRecord, JobRecord> update)
    {
        await gate.WaitAsync();
        try
        {
            var job = update(Find(id));
            Persist(job);
            jobs[id] = job;
            return job;
        }
        finally { gate.Release(); }
    }

    public async Task<JobRecord> AcknowledgeAsync(string id)
    {
        var job = await UpdateAsync(id, existing =>
        {
            if (existing.Status != JobState.Interrupted)
                throw new ApiException(409, "Only interrupted jobs can be acknowledged.");
            File.AppendAllText(Path.Combine(existing.LogDirectory, "API.log"),
                $"{DateTimeOffset.UtcNow:O} Interrupted execution acknowledged.{Environment.NewLine}");
            return existing with
            {
                Status = JobState.Failed,
                Error = "Interrupted execution acknowledged by an operator after external reconciliation."
            };
        });
        logger.LogWarning("Operator acknowledged interrupted job {JobId}", id);
        return job;
    }

    public async Task StopAcceptingAsync()
    {
        await gate.WaitAsync();
        try
        {
            accepting = false;
            Queue.Writer.TryComplete();
        }
        finally { gate.Release(); }
    }

    public async Task RemoveExpiredLogsAsync()
    {
        JobRecord[] snapshot;
        await gate.WaitAsync();
        try { snapshot = jobs.Values.ToArray(); }
        finally { gate.Release(); }
        foreach (var job in snapshot.Where(job => !job.IsActive && job.Status != JobState.Interrupted && job.EndTime.HasValue))
        {
            var app = catalog.Get(job.Application);
            if (job.EndTime < DateTimeOffset.UtcNow.AddDays(-app.RetentionDays) && Directory.Exists(job.LogDirectory))
            {
                var expected = Path.GetFullPath(Path.Combine(app.LogPath, job.JobName, job.JobId));
                if (!expected.Equals(Path.GetFullPath(job.LogDirectory), StringComparison.OrdinalIgnoreCase))
                    throw new InvalidDataException($"Unexpected log directory in job {job.JobId}; refusing deletion.");
                Directory.Delete(expected, recursive: true);
                logger.LogInformation("Removed expired wrapper logs for job {JobId}", job.JobId);
            }
        }
    }

    private JobRecord Find(string id) => jobs.TryGetValue(id, out var job)
        ? job : throw new ApiException(404, "Job not found.");

    private void Persist(JobRecord job)
    {
        var target = Path.Combine(directory, job.JobId + ".json");
        var temporary = target + ".tmp";
        File.WriteAllText(temporary, JsonSerializer.Serialize(job, JsonOptions));
        File.Move(temporary, target, overwrite: true);
    }

    public void Dispose()
    {
        instanceLock.Dispose();
        gate.Dispose();
    }
}
