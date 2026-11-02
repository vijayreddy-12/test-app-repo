using Microsoft.AspNetCore.Mvc;
using RaincodeJCLWrapper.Models;
using RaincodeJCLWrapper.Services;

namespace RaincodeJCLWrapper.Controllers;

[ApiController]
[Route("api/jobs")]
[Produces("application/json")]
[ProducesResponseType<ProblemDetails>(StatusCodes.Status401Unauthorized)]
public sealed class JobsController(JobStore store) : ControllerBase
{
    /// <summary>Queues a JCL job for execution.</summary>
    /// <remarks>HTTP 202 only means the job was accepted; poll the returned job ID for the outcome.</remarks>
    [HttpPost("submit")]
    [ProducesResponseType<JobResponse>(StatusCodes.Status202Accepted)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status400BadRequest)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status404NotFound)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status409Conflict)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status503ServiceUnavailable)]
    public async Task<ActionResult<JobResponse>> Submit(SubmitRequest request)
    {
        var job = await store.SubmitAsync(request);
        return AcceptedAtAction(nameof(Get), new { jobId = job.JobId }, JobResponse.From(job));
    }

    /// <summary>Returns the current status of a job.</summary>
    [HttpGet("{jobId}")]
    [ProducesResponseType<JobResponse>(StatusCodes.Status200OK)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<JobResponse>> Get(string jobId) => JobResponse.From(await store.GetAsync(jobId));

    /// <summary>Creates a new job restarting a failed or completed job from the given step.</summary>
    [HttpPost("{jobId}/restart")]
    [ProducesResponseType<JobResponse>(StatusCodes.Status202Accepted)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status400BadRequest)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status404NotFound)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status409Conflict)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status503ServiceUnavailable)]
    public async Task<ActionResult<JobResponse>> Restart(string jobId, RestartRequest request)
    {
        var job = await store.RestartAsync(jobId, request);
        return AcceptedAtAction(nameof(Get), new { jobId = job.JobId }, JobResponse.From(job));
    }

    /// <summary>Marks an interrupted job as failed after operator reconciliation.</summary>
    [HttpPost("{jobId}/acknowledge-interruption")]
    [ProducesResponseType<JobResponse>(StatusCodes.Status200OK)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status404NotFound)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status409Conflict)]
    public async Task<ActionResult<JobResponse>> Acknowledge(string jobId) =>
        JobResponse.From(await store.AcknowledgeAsync(jobId));

    /// <summary>Streams a wrapper log or an original Raincode runtime file.</summary>
    /// <param name="jobId">Wrapper job ID.</param>
    /// <param name="file">Execution.log, API.log, or a runtime file path returned by logs/files.</param>
    [HttpGet("{jobId}/logs")]
    [ProducesResponseType<string>(StatusCodes.Status200OK, "text/plain")]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status400BadRequest)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status404NotFound)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status409Conflict)]
    public async Task<IActionResult> Logs(string jobId, [FromQuery] string file = "Execution.log")
    {
        var job = await store.GetAsync(jobId);
        var runtimeDirectory = job.RuntimeDirectory;
        var root = file.Equals("Execution.log", StringComparison.OrdinalIgnoreCase) ||
            file.Equals("API.log", StringComparison.OrdinalIgnoreCase)
            ? job.LogDirectory
            : runtimeDirectory ?? throw new ApiException(409, "Raincode log location is not yet available.");
        var path = ApplicationCatalog.ResolveChild(root, file);
        if (!System.IO.File.Exists(path))
            throw new ApiException(404, "Log file not found or already expired.");
        var stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
        return File(stream, "text/plain; charset=utf-8");
    }

    /// <summary>Lists the wrapper log files and the original Raincode runtime files of a job.</summary>
    [HttpGet("{jobId}/logs/files")]
    [ProducesResponseType<LogFilesResponse>(StatusCodes.Status200OK)]
    [ProducesResponseType<ProblemDetails>(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<LogFilesResponse>> LogFiles(string jobId)
    {
        var job = await store.GetAsync(jobId);
        var wrapperFiles = Directory.Exists(job.LogDirectory)
            ? Directory.EnumerateFiles(job.LogDirectory).Select(path => Path.GetFileName(path)).ToArray()
            : [];
        var runtimeDirectory = job.RuntimeDirectory;
        var runtimeFiles = runtimeDirectory is not null && Directory.Exists(runtimeDirectory)
            ? Directory.EnumerateFiles(runtimeDirectory, "*", SearchOption.AllDirectories)
                .Select(path => Path.GetRelativePath(runtimeDirectory, path)).ToArray()
            : [];
        return new LogFilesResponse(jobId, wrapperFiles, runtimeFiles);
    }
}
