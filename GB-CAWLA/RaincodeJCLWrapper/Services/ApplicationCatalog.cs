using System.Text.Json;
using System.Text.RegularExpressions;
using RaincodeJCLWrapper.Models;

namespace RaincodeJCLWrapper.Services;

public sealed class ApplicationCatalog
{
    private readonly Dictionary<string, AppConfiguration> applications = new(StringComparer.OrdinalIgnoreCase);

    public ApplicationCatalog(IConfiguration configuration, IHostEnvironment environment)
    {
        var directory = Path.GetFullPath(configuration["Wrapper:ConfigurationPath"] ?? "Config",
            environment.ContentRootPath);
        foreach (var file in Directory.EnumerateFiles(directory, "*.json"))
        {
            var app = JsonSerializer.Deserialize<AppConfiguration>(File.ReadAllText(file))
                ?? throw new InvalidDataException($"Empty application configuration: {file}");
            if (!IsValidName(app.Application) || app.RetentionDays < 1 || app.SuccessMaxReturnCode < 0 ||
                string.IsNullOrWhiteSpace(app.SubmitExecutable) || app.SubmitArguments is null ||
                app.EnvironmentVariables is null ||
                new[] { app.RootPath, app.JclPath, app.LogPath, app.SysoutPath }
                    .Any(path => string.IsNullOrWhiteSpace(path) || !Path.IsPathFullyQualified(path)))
            {
                throw new InvalidDataException($"Invalid application configuration: {file}");
            }
            if (!applications.TryAdd(app.Application, app))
                throw new InvalidDataException($"Duplicate application configuration: {app.Application}");
        }
        if (applications.Count == 0)
            throw new InvalidDataException($"No application configurations found in {directory}.");
    }

    public AppConfiguration Get(string application) =>
        applications.TryGetValue(application, out var app)
            ? app
            : throw new ApiException(404, $"Application '{application}' is not configured.");

    public static bool IsValidName(string? name) =>
        name is not null && Regex.IsMatch(name, @"\A[A-Za-z0-9][A-Za-z0-9_-]{0,63}\z");

    public static bool IsValidRestartStep(string? step) =>
        step is not null && Regex.IsMatch(step,
            @"\A[A-Za-z0-9@$#][A-Za-z0-9@$#_-]{0,63}(?:\.[A-Za-z0-9@$#][A-Za-z0-9@$#_-]{0,63})*\z") &&
        step.Length <= 255;

    public static string ResolveChild(string root, string relativePath)
    {
        if (string.IsNullOrWhiteSpace(relativePath) || Path.IsPathRooted(relativePath) || relativePath.Contains(':'))
            throw new ApiException(400, "A relative file path is required.");
        var fullRoot = Path.TrimEndingDirectorySeparator(Path.GetFullPath(root)) + Path.DirectorySeparatorChar;
        var fullPath = Path.GetFullPath(Path.Combine(fullRoot, relativePath));
        if (!fullPath.StartsWith(fullRoot, StringComparison.OrdinalIgnoreCase))
            throw new ApiException(400, "The file path must stay inside the configured directory.");
        return fullPath;
    }
}
