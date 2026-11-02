# RaincodeJCLWrapper

Initial .NET 10 REST service for CAWLA / ESP dSeries integration. Raincode remains
responsible for JCL interpretation, dataset mapping and utility execution.

**Validation status:** the previous .NET 8 version compiled successfully on the
server, but integration startup failed because ASP.NET Core 8 was absent.
The wrapper now targets .NET 10 to match the server's installed SDK 10.0.401 and
ASP.NET Core/.NET runtime 10.0.12. Rebuild and rerun the integration tests.
The preparation machine has no .NET SDK; installation was declined.
Do not treat this draft as production-ready until the build and tests below pass
and a real Raincode success/failure pair is verified.

Completed preparation checks: JSON and project XML syntax; PowerShell test-script
syntax; fake-submit CLI execution for success, max CC 12 with launcher exit 0,
missing maximum-CC file, and launcher exit 7. VS Code reported no diagnostics,
but without an SDK this does not establish C# compilation or API correctness.
Follow-up checks also passed for ETR's configured threshold/host and fake-submit
maximum CC 4, maximum CC 5, and intact `-RESTART=STEP1.MYPROC` argument delivery.
The HTTP restart flow and real Raincode execution remain untested.

## Implemented behavior

- Four parallel executions of different jobs; further submissions remain queued.
- HTTP 409 for the same application/job while submitted or running.
- Durable JSON status records, written using temporary-file replacement.
- Concurrent stdout/stderr capture in a separate `Execution.log` per execution.
- Final `returnCode` from Raincode's `RETURNCODE.TXT`, which contains maximum job CC.
- `processExitCode` recorded separately. Raincode's process exit code is confirmed
  to equal maximum job CC. ETR success requires max CC <= 4 and a matching process
  exit code. Both CC 0 and CC 4 are accepted; disagreement between the exit code
  and return-code file is reported as failure.
- Explicit failure with a null `returnCode` if the return-code file is missing,
  malformed or outside the application's configured SYSOUT root.
- Access to original SYSLOG, SYSOUT and SYSPRINT files without parsing/recreating them.
- Daily cleanup of wrapper logs after the application's retention period (365 days
  by default). Job records are retained; original Raincode SYSOUT is **not deleted**.
- API-key authentication for every endpoint, including health checks. Only the
  Swagger UI (`/swagger`) and the OpenAPI document (`/openapi/v1.json`) are public;
  use the Swagger **Authorize** button to send `X-Api-Key` from the UI.

The normal submit process must wait for completion, as confirmed for this environment.
Runtime job ID and `RCRETURNCODEPATH` are extracted from the supplied Raincode output
format. Those labels must remain present in stdout.

Step restart passes the confirmed `-RESTART=STEP1.MYPROC` syntax to Raincode.
The wrapper does not parse JCL, infer the failed step or implement its own
checkpoints. An operator/CAWLA must supply the appropriate restart step and confirm
that dataset side effects make restarting safe.

## Build and start on the Raincode server

Use the .NET 10 SDK for building and the x64 ASP.NET Core 10 and .NET 10 runtimes
for execution. The reported server installations already satisfy these requirements;
ASP.NET Core 8 is no longer needed for this wrapper. No major-version runtime
roll-forward override is required.

```powershell
dotnet build .\RaincodeJCLWrapper.csproj --configuration Release
$env:Wrapper__ApiKey = '<secret-from-your-approved-secret-store-at-least-32-characters>'
dotnet run --project .\RaincodeJCLWrapper.csproj --configuration Release --no-build
```

The target server is `AZWAPDFININFD01`; it is included in `AllowedHosts`.
The default development address remains `http://localhost:5080`.
No remote listener or HTTPS certificate is configured yet. Before CAWLA remote use,
host behind an HTTPS endpoint (for example IIS), configure an appropriate `Urls` /
`URLS` binding and `AllowedHosts`, and restrict network access to CAWLA.
**Do not send the API key over unencrypted remote HTTP.**

For an approved HTTPS binding on port 443, the intended CAWLA URLs are:

```text
POST https://AZWAPDFININFD01/api/jobs/submit
GET  https://AZWAPDFININFD01/api/jobs/{jobId}
POST https://AZWAPDFININFD01/api/jobs/{jobId}/restart
```

These are deployment templates, not verified live URLs. Use the actual configured
port and the DNS name covered by the certificate (which may need to be an FQDN).

Use a dedicated service account with access to Raincode, application/catalog files,
wrapper data and logs, and the original SYSOUT files. A working manual command under
your interactive login does not establish that the service account has the same
PATH, permissions or runtime environment.

The supplied Raincode installation directory is
`C:\Program Files\Raincode\Crossbow\net10.0\bin`; system environment variables
are already configured. ETR therefore retains `SubmitExecutable: "submit"` and
inherits the service process environment. No executable filename is guessed from
the directory. On the server, verify `Get-Command submit` under the intended service
account and confirm it resolves to the actual executable. Restart the host process
after changing system environment variables.

This draft does not install/register a Windows service. Use an approved IIS or
service hosting arrangement after validation. Run one wrapper instance against a
storage directory; an exclusive instance lock prevents another process sharing
that directory. Separate storage directories do not provide cross-instance
duplicate protection. Multi-server/replicated hosting is not supported.

## Application configuration

`Config\ETR.json` uses the paths confirmed in the supplied example. No EPF or EGS
configuration is activated without confirming its runtime paths.

To add your new application, create another JSON file in `Config` with:

| Setting | Meaning |
|---|---|
| `Application` | Name CAWLA sends, for example `ETR` |
| `RootPath` | Application root and submit working directory |
| `JclPath` | Directory containing `<jobName>.jcl` |
| `LogPath` | Wrapper logs: `<LogPath>\<jobName>\<wrapperJobId>` |
| `SysoutPath` | Allowed root for original Raincode output, currently `F:\Raincode\SYSOUT` |
| `SubmitExecutable` | Prefer the absolute path to the Raincode executable; `submit` needs service-account PATH |
| `SubmitArguments` | Fixed arguments placed before `-file=<JCL>`; normally empty |
| `EnvironmentVariables` | Required application-specific environment variables; do not store secrets here |
| `RetentionDays` | Wrapper-log retention, at least 1 |
| `SuccessMaxReturnCode` | Highest max CC considered successful; ETR is configured as 4 |

All configured directory paths must be absolute. Application and JCL names are
limited to 1-64 letters, digits, underscores and hyphens, beginning with a letter or
digit. Do not send `.jcl` as part of `jobName`. Configuration is loaded at startup;
restart the service after changing it.

Global settings are in `appsettings.json`. Environment variables override them:

```powershell
$env:Wrapper__StoragePath = 'F:\Raincode\WrapperData'
$env:Wrapper__MaxConcurrentJobs = '4' # supported range: 1-64
```

Keep API keys out of JSON/source control. The service refuses to start with a
missing key or a key shorter than 32 characters. ASP.NET Core built-in structured
logging is used rather than adding Serilog before a deployment log sink is chosen.
Submission and reconciliation audit events also appear in each job's `API.log`.

## CAWLA endpoints

Replace `<base-url>` with the confirmed HTTPS deployment address.
Every request requires `X-Api-Key: <secret>`.

| Method | URL | Purpose |
|---|---|---|
| POST | `<base-url>/api/jobs/submit` | Accept a submission; HTTP 202 |
| GET | `<base-url>/api/jobs/{jobId}` | Poll status and final max CC |
| GET | `<base-url>/api/jobs/{jobId}/logs` | Stream wrapper `Execution.log` |
| GET | `<base-url>/api/jobs/{jobId}/logs/files` | List wrapper and original runtime files |
| GET | `<base-url>/api/jobs/{jobId}/logs?file=SYSLOG.txt` | Stream an original runtime file |
| POST | `<base-url>/api/jobs/{jobId}/restart` | Create a linked step restart; HTTP 202 |
| POST | `<base-url>/api/jobs/{jobId}/acknowledge-interruption` | Operator reconciliation only |
| GET | `<base-url>/health` | Authenticated service health |
| GET | `<base-url>/swagger` | Swagger UI (no key required) |
| GET | `<base-url>/openapi/v1.json` | OpenAPI 3.1 document (no key required) |

Submit:

```json
{
  "application": "ETR",
  "jobName": "PTRTMNTM",
  "restartStep": null
}
```

Acceptance returns the wrapper's `jobId`, status `Submitted`, and
`returnCode: null`. A UUID wrapper ID is distinct from Raincode's `JOB0000000013`.
The latter becomes `runtimeJobId` once the process output has been drained.
Do not interpret HTTP 202 as job success.

### Step restart

To restart a **failed or completed** job:

```http
POST <base-url>/api/jobs/{jobId}/restart
Content-Type: application/json
X-Api-Key: <secret>
```

```json
{
  "restartStep": "STEP1.MYPROC"
}
```

This creates a new wrapper job ID with `parentJobId` referencing the original job.
The original record stays unchanged. Poll the **new** job ID. The process receives:

```text
submit -file=<absolute-JCL-path> -RESTART=STEP1.MYPROC
```

Arguments are passed separately without shell interpretation. A restart step must
contain nonempty dot-separated names made of letters, digits, `@`, `$`, `#`, `_`
or `-`, with a maximum of 64 characters per name and 255 total.
Raincode decides whether the requested step actually exists.

The submission route also accepts an explicit `restartStep` for a JCL without
linking a prior wrapper job; use the dedicated restart route when the prior wrapper
job ID is known. Both routes enforce active duplicate and interruption holds.
The dedicated route rejects queued/running/interrupted source jobs with HTTP 409;
an interrupted execution must be reconciled first.

Poll states:

- `Submitted`: queued.
- `Running`: a worker is preparing or executing the submit process.
- `Completed`: final maximum CC meets the configured threshold and process exit
  equals that CC.
- `Failed`: final maximum CC is too high, launcher failure, or execution/read failure.
  Inspect `returnCode`, `processExitCode` and `error`; a missing max CC stays null.
- `Interrupted`: completion cannot be certified; operator reconciliation required.

CAWLA should poll `Submitted`/`Running` until a terminal state, and map the final
maximum CC and errors using your approved scheduler policy. Do not blindly retry a
timed-out submission: it may already have been accepted. A server-generated ID is
not an idempotency key; automatic POST retries can rerun a job after it completes.

Use the log file list to discover actual step/PROC paths; URL-encode the returned
relative file name in the `file` query parameter. Runtime log locations become
available after submit exits, while wrapper output can be read during execution.
Original runtime files are streamed as stored; encoding may differ from UTF-8.

## Interruptions and recovery

The wrapper does not auto-replay queued/running records after a restart, because a
Raincode process or dataset changes may still exist. Those records become
`Interrupted` and block another submission of the same application/job.

Graceful shutdown attempts to stop active process trees and records interruptions.
A forced crash may leave Raincode processes alive. An operator must verify that
Raincode is no longer running and reconcile dataset side effects, then explicitly
POST to `acknowledge-interruption`. This changes the record to `Failed` and permits
a fresh submission. It does **not** prove the original job failed, roll back data,
or restart from a step. Do not put this endpoint in an automatic CAWLA retry loop.

## Repeatable integration tests

On a Windows machine with the SDK:

```powershell
.\Tests\Invoke-IntegrationTests.ps1
```

The script builds the service and uses an isolated fake submit process; no real
Raincode jobs are executed. It checks authentication, invalid names and traversal,
unknown applications/JCLs, parallel limits, active duplicates, success and max CC,
CC 4 success and CC 5 failure, launcher errors, missing/malformed/escaped return-code
files, logs, restart argument delivery, failed/completed restart eligibility,
invalid restart steps, persisted restart metadata and interruption reconciliation.

The default test port is 15080; use `-Port` if occupied. Test files and server logs
remain in the reported unique temporary directory for inspection. The script
restores environment variables and stops only the server PID it created. These
checks do not substitute for a real Raincode success/failure/shutdown test under
the intended service account.

## Details still needed before deployment

- New application name, root/JCL/log paths and SYSOUT root if different from ETR.
- Confirm that `Get-Command submit` resolves an executable in the supplied
  installation directory under the service account.
- `$LASTEXITCODE` for a completed successful job and a failed job; a failed sample's
  stdout and maximum CC file contents, with sensitive information redacted.
- Real Raincode restart output to verify the supplied command syntax and behavior.
- HTTPS binding/certificate for `AZWAPDFININFD01`, service account and CAWLA credential
  handling/polling/return-code policy.
