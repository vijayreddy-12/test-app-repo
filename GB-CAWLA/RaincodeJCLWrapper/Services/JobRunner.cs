using System.Diagnostics;
using System.Globalization;
using System.Text.RegularExpressions;
using RaincodeJCLWrapper.Models;

namespace RaincodeJCLWrapper.Services;

public sealed class JobRunner(JobStore store, ApplicationCatalog catalog,
    IConfiguration configuration, ILogger<JobRunner> logger)
    : BackgroundService
{
    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        var maximum = configuration.GetValue("Wrapper:MaxConcurrentJobs", 4);
        if (maximum is < 1 or > 64)
            throw new InvalidDataException("Wrapper:MaxConcurrentJobs must be between 1 and 64.");
        using var workersCancellation = CancellationTokenSource.CreateLinkedTokenSource(stoppingToken);
        async Task WorkerAsync()
        {
            try
            {
                await foreach (var id in store.Queue.Reader.ReadAllAsync(workersCancellation.Token))
                    await RunAsync(id, workersCancellation.Token);
            }
            catch
            {
                workersCancellation.Cancel();
                throw;
            }
        }
        try
        {
            await Task.WhenAll(Enumerable.Range(0, maximum).Select(_ => WorkerAsync()));
        }
        catch (OperationCanceledException) when (stoppingToken.IsCancellationRequested)
        {
            logger.LogInformation("Stopping Raincode execution workers");
        }
        finally
        {
            await store.StopAcceptingAsync();
        }
    }

    public override async Task StopAsync(CancellationToken cancellationToken)
    {
        await store.StopAcceptingAsync();
        await base.StopAsync(cancellationToken);
    }

    private async Task RunAsync(string id, CancellationToken cancellationToken)
    {
        try
        {
            cancellationToken.ThrowIfCancellationRequested();
            var job = await store.UpdateAsync(id, existing => existing with
            {
                Status = JobState.Running,
                StartTime = DateTimeOffset.UtcNow
            });
            var app = catalog.Get(job.Application);
            var startInfo = new ProcessStartInfo
            {
                FileName = app.SubmitExecutable,
                WorkingDirectory = app.RootPath,
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                CreateNoWindow = true
            };
            foreach (var argument in app.SubmitArguments)
                startInfo.ArgumentList.Add(argument);
            startInfo.ArgumentList.Add("-file=" + job.JclFile);
            if (job.RestartStep is not null)
                startInfo.ArgumentList.Add("-RESTART=" + job.RestartStep);
            foreach (var variable in app.EnvironmentVariables)
                startInfo.Environment[variable.Key] = variable.Value;

            using var process = new Process { StartInfo = startInfo };
            using var logStream = new FileStream(Path.Combine(job.LogDirectory, "Execution.log"),
                FileMode.Create, FileAccess.Write, FileShare.ReadWrite);
            await using var log = new StreamWriter(logStream) { AutoFlush = true };
            using var logGate = new SemaphoreSlim(1, 1);
            string? runtimeId = null;
            string? returnCodePath = null;
            if (!process.Start())
                throw new InvalidOperationException("Raincode submit could not be started.");

            async Task CaptureAsync(StreamReader reader, string source)
            {
                try
                {
                    while (await reader.ReadLineAsync() is { } line)
                    {
                        await logGate.WaitAsync();
                        try
                        {
                            await log.WriteLineAsync($"{DateTimeOffset.UtcNow:O} [{source}] {line}");
                            if (source == "stdout")
                            {
                                var idMatch = Regex.Match(line, @"^\s*(JOB\d+)\s*:");
                                if (idMatch.Success)
                                    runtimeId ??= idMatch.Groups[1].Value;
                                var pathMatch = Regex.Match(line, @"Setting RCRETURNCODEPATH\s*=\s*(.+)$");
                                if (pathMatch.Success)
                                    returnCodePath = pathMatch.Groups[1].Value.Trim();
                            }
                        }
                        finally { logGate.Release(); }
                    }
                }
                catch
                {
                    if (!process.HasExited)
                        process.Kill(entireProcessTree: true);
                    throw;
                }
            }

            var capture = Task.WhenAll(CaptureAsync(process.StandardOutput, "stdout"),
                CaptureAsync(process.StandardError, "stderr"));
            try
            {
                await process.WaitForExitAsync(cancellationToken);
            }
            catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
            {
                if (!process.HasExited)
                    process.Kill(entireProcessTree: true);
                await process.WaitForExitAsync();
                throw;
            }
            finally
            {
                await capture;
            }

            await store.UpdateAsync(id, existing => existing with
            {
                ProcessExitCode = process.ExitCode,
                RuntimeJobId = runtimeId
            });
            if (runtimeId is null || returnCodePath is null)
                throw new InvalidDataException("Raincode output did not identify a runtime job and RCRETURNCODEPATH.");
            var relative = Path.GetRelativePath(app.SysoutPath, returnCodePath);
            var safePath = ApplicationCatalog.ResolveChild(app.SysoutPath, relative);
            if (!Path.GetFileName(safePath).Equals("RETURNCODE.TXT", StringComparison.OrdinalIgnoreCase) ||
                !new DirectoryInfo(Path.GetDirectoryName(safePath)!).Name.Equals(runtimeId, StringComparison.OrdinalIgnoreCase))
                throw new InvalidDataException("Raincode reported an unexpected return-code file path.");
            await store.UpdateAsync(id, existing => existing with
            {
                RuntimeDirectory = Path.GetDirectoryName(safePath)
            });
            var text = (await File.ReadAllTextAsync(safePath, cancellationToken)).Trim();
            if (!int.TryParse(text, NumberStyles.None, CultureInfo.InvariantCulture, out var returnCode))
                throw new InvalidDataException("RETURNCODE.TXT must contain a non-negative integer maximum job CC.");
            var succeeded = process.ExitCode == returnCode && returnCode <= app.SuccessMaxReturnCode;
            await store.UpdateAsync(id, existing => existing with
            {
                Status = succeeded ? JobState.Completed : JobState.Failed,
                EndTime = DateTimeOffset.UtcNow,
                ReturnCode = returnCode,
                Error = succeeded ? null : $"Job finished with max CC {returnCode} and process exit code {process.ExitCode}."
            });
            logger.LogInformation("Job {JobId} finished: max CC {ReturnCode}, process exit {ProcessExitCode}",
                id, returnCode, process.ExitCode);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            await store.UpdateAsync(id, existing => existing with
            {
                Status = JobState.Interrupted,
                EndTime = DateTimeOffset.UtcNow,
                Error = "Wrapper shutdown interrupted execution. Verify Raincode and datasets before resubmitting."
            });
            logger.LogWarning("Job {JobId} interrupted during shutdown", id);
        }
        catch (Exception exception)
        {
            logger.LogError(exception, "Job {JobId} execution failed", id);
            await store.UpdateAsync(id, existing => existing with
            {
                Status = JobState.Failed,
                EndTime = DateTimeOffset.UtcNow,
                Error = exception.Message
            });
        }
    }
}
