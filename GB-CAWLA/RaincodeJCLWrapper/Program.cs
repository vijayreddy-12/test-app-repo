using System.Security.Cryptography;
using System.Text;
using System.Text.Json.Serialization;
using Microsoft.AspNetCore.Diagnostics;
using Microsoft.OpenApi;
using RaincodeJCLWrapper.Models;
using RaincodeJCLWrapper.Services;

var builder = WebApplication.CreateBuilder(args);
var bypassApiKeyMarquer = builder.Configuration["Wrapper:BypassApiKey"];
bool bypassApiKey = !string.IsNullOrWhiteSpace(bypassApiKeyMarquer) && bypassApiKeyMarquer.Equals("true", StringComparison.OrdinalIgnoreCase);
byte[]? keyHash = null;
if (!bypassApiKey)
{
    var apiKey = builder.Configuration["Wrapper:ApiKey"];

    if (string.IsNullOrWhiteSpace(apiKey) || apiKey.Length < 32)
        throw new InvalidOperationException("Set Wrapper__ApiKey to a secret of at least 32 characters before starting the wrapper.");
    keyHash = SHA256.HashData(Encoding.UTF8.GetBytes(apiKey));
}

builder.Services.AddControllers().AddJsonOptions(options =>
    options.JsonSerializerOptions.Converters.Add(new JsonStringEnumConverter()));
builder.Services.ConfigureHttpJsonOptions(options =>
    options.SerializerOptions.Converters.Add(new JsonStringEnumConverter()));
builder.Services.AddOpenApi(options =>
{
    if (bypassApiKey)
        return;
    options.AddDocumentTransformer((document, _, _) =>
    {
        document.Components ??= new OpenApiComponents();
        document.Components.SecuritySchemes ??= new Dictionary<string, IOpenApiSecurityScheme>();
        document.Components.SecuritySchemes["ApiKey"] = new OpenApiSecurityScheme
        {
            Type = SecuritySchemeType.ApiKey,
            In = ParameterLocation.Header,
            Name = "X-Api-Key"
        };
        document.Security ??= [];
        document.Security.Add(new OpenApiSecurityRequirement
        {
            [new OpenApiSecuritySchemeReference("ApiKey", document)] = []
        });
        return Task.CompletedTask;
    });
});
builder.Services.AddProblemDetails();
builder.Services.AddSingleton<ApplicationCatalog>();
builder.Services.AddSingleton<JobStore>();
builder.Services.AddHostedService<JobRunner>();
builder.Services.AddHostedService<LogRetentionService>();

var app = builder.Build();
app.UseExceptionHandler(new ExceptionHandlerOptions
{
    AllowStatusCode404Response = true,
    ExceptionHandler = async context =>
    {
        var exception = context.Features.Get<IExceptionHandlerFeature>()?.Error;
        var status = exception is ApiException apiException ? apiException.StatusCode : 500;
        app.Logger.Log(status >= 500 ? LogLevel.Error : LogLevel.Warning, exception,
            "Request failed: {Method} {Path}", context.Request.Method, context.Request.Path);
        await Results.Problem(
            statusCode: status,
            title: exception is ApiException ? exception.Message : "An internal wrapper error occurred. Check the service logs.")
            .ExecuteAsync(context);
    }
});
app.UseSwaggerUI(options =>
{
    options.SwaggerEndpoint("/openapi/v1.json", "RaincodeJCLWrapper v1");
    options.DocumentTitle = "RaincodeJCLWrapper";
});
app.Use(async (context, next) =>
{
    var supplied = context.Request.Headers["X-Api-Key"];
    if (!bypassApiKey && !context.Request.Path.StartsWithSegments("/openapi") && (supplied.Count != 1 || string.IsNullOrWhiteSpace(supplied[0]) ||
        !CryptographicOperations.FixedTimeEquals(keyHash,
            SHA256.HashData(Encoding.UTF8.GetBytes(supplied[0]!)))))
    {
        app.Logger.LogWarning("Rejected unauthenticated request: {Method} {Path}",
            context.Request.Method, context.Request.Path);
        await Results.Problem(statusCode: 401, title: "A valid X-Api-Key header is required.").ExecuteAsync(context);
        return;
    }
    await next(context);
});
app.MapGet("/health", () => Results.Ok(new { status = "Healthy", service = "RaincodeJCLWrapper" }))
    .WithTags("Health");
app.MapOpenApi();
app.MapControllers();
app.Run();
