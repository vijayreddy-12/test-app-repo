namespace RaincodeJCLWrapper.Models;

public sealed record SubmitRequest(string Application, string JobName, string? RestartStep = null);
public sealed record RestartRequest(string RestartStep);

public enum JobState
{
    Submitted,
    Running,
    Completed,
    Failed,
    Interrupted
}

public sealed record JobRecord
{
    public required string JobId { get; init; }
    public required string Application { get; init; }
    public required string JobName { get; init; }
    public required string JclFile { get; init; }
    public required string LogDirectory { get; init; }
    public JobState Status { get; init; } = JobState.Submitted;
    public DateTimeOffset SubmittedAt { get; init; } = DateTimeOffset.UtcNow;
    public DateTimeOffset? StartTime { get; init; }
    public DateTimeOffset? EndTime { get; init; }
    public string? RuntimeJobId { get; init; }
    public string? RuntimeDirectory { get; init; }
    public string? RestartStep { get; init; }
    public string? ParentJobId { get; init; }
    public int? ReturnCode { get; init; }
    public int? ProcessExitCode { get; init; }
    public string? Error { get; init; }

    public bool IsActive => Status is JobState.Submitted or JobState.Running;
}

public sealed record JobResponse(
    string JobId,
    string Application,
    string JobName,
    JobState Status,
    DateTimeOffset SubmittedAt,
    DateTimeOffset? StartTime,
    DateTimeOffset? EndTime,
    string? RuntimeJobId,
    string? RestartStep,
    string? ParentJobId,
    int? ReturnCode,
    int? ProcessExitCode,
    string? Error)
{
    public static JobResponse From(JobRecord job) => new(
        job.JobId, job.Application, job.JobName, job.Status, job.SubmittedAt, job.StartTime, job.EndTime,
        job.RuntimeJobId, job.RestartStep, job.ParentJobId, job.ReturnCode, job.ProcessExitCode, job.Error);
}

public sealed record LogFilesResponse(string JobId, string[] WrapperFiles, string[] RuntimeFiles);

public sealed class AppConfiguration
{
    public required string Application { get; init; }
    public required string RootPath { get; init; }
    public required string JclPath { get; init; }
    public required string LogPath { get; init; }
    public required string SysoutPath { get; init; }
    public string SubmitExecutable { get; init; } = "submit";
    public string[] SubmitArguments { get; init; } = [];
    public Dictionary<string, string> EnvironmentVariables { get; init; } = [];
    public int RetentionDays { get; init; } = 365;
    public int SuccessMaxReturnCode { get; init; }
}

public sealed class ApiException(int statusCode, string message) : Exception(message)
{
    public int StatusCode { get; } = statusCode;
}
