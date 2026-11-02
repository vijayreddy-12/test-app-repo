namespace RaincodeJCLWrapper.Services;

public sealed class LogRetentionService(JobStore store, ILogger<LogRetentionService> logger) : BackgroundService
{
    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        using var timer = new PeriodicTimer(TimeSpan.FromDays(1));
        do
        {
            try
            {
                await store.RemoveExpiredLogsAsync();
            }
            catch (Exception exception) when (exception is IOException or UnauthorizedAccessException)
            {
                logger.LogError(exception, "Log retention cleanup failed; it will be retried at the next scheduled run");
            }
        } while (await timer.WaitForNextTickAsync(stoppingToken));
    }
}
