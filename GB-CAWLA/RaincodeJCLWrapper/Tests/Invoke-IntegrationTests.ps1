param(
    [int]$Port = 15080
)

$ErrorActionPreference = 'Stop'
$projectRoot = Split-Path $PSScriptRoot -Parent
$project = Join-Path $projectRoot 'RaincodeJCLWrapper.csproj'
$sdk = @(& dotnet --list-sdks)
if ($LASTEXITCODE -ne 0 -or -not ($sdk -match '^10\.')) {
    throw 'A .NET 10 SDK is required to build and run these integration tests.'
}
& dotnet build $project --configuration Release
if ($LASTEXITCODE -ne 0) { throw 'Build failed.' }

$testRoot = Join-Path ([System.IO.Path]::GetTempPath()) ('RaincodeJCLWrapper-tests-' + [guid]::NewGuid().ToString('N'))
$server = $null
$baseUrl = "http://localhost:$Port"
$apiKey = [guid]::NewGuid().ToString('N') + [guid]::NewGuid().ToString('N')
$names = @('Wrapper__ApiKey', 'Wrapper__StoragePath', 'Wrapper__ConfigurationPath',
    'Wrapper__MaxConcurrentJobs', 'URLS')
$originalEnvironment = @{}
foreach ($name in $names) {
    $originalEnvironment[$name] = [Environment]::GetEnvironmentVariable($name, 'Process')
}

function Assert-True($Condition, [string]$Message) {
    if (-not $Condition) { throw "Assertion failed: $Message" }
}

function Request([string]$Method, [string]$Path, $Body = $null, [switch]$WithoutKey) {
    $headers = @{}
    if (-not $WithoutKey) { $headers['X-Api-Key'] = $apiKey }
    $parameters = @{
        Uri = $baseUrl + $Path
        Method = $Method
        Headers = $headers
        UseBasicParsing = $true
    }
    if ($null -ne $Body) {
        $parameters['Body'] = ConvertTo-Json $Body -Compress
        $parameters['ContentType'] = 'application/json'
    }
    try {
        $response = Invoke-WebRequest @parameters
        return @{ Status = [int]$response.StatusCode; Body = $response.Content }
    }
    catch {
        if ($null -eq $_.Exception.Response) { throw }
        return @{ Status = [int]$_.Exception.Response.StatusCode; Body = $_.ErrorDetails.Message }
    }
}

function Submit([string]$Name) {
    $response = Request 'POST' '/api/jobs/submit' @{ application = 'TEST'; jobName = $Name }
    Assert-True ($response.Status -eq 202) "Submission of $Name returns 202, got $($response.Status): $($response.Body)"
    $job = $response.Body | ConvertFrom-Json
    Assert-True ($null -eq $job.returnCode) 'Accepted submission has no final return code'
    return $job
}

function Wait-ForState([string]$Id, [string[]]$States) {
    $deadline = [DateTime]::UtcNow.AddSeconds(40)
    do {
        $response = Request 'GET' "/api/jobs/$Id"
        Assert-True ($response.Status -eq 200) 'Status endpoint succeeds'
        $job = $response.Body | ConvertFrom-Json
        if ($States -contains $job.status) { return $job }
        Start-Sleep -Milliseconds 100
    } while ([DateTime]::UtcNow -lt $deadline)
    throw "Job $Id did not reach $($States -join ', '); last status: $($job.status)"
}

function Start-Server {
    $dll = Join-Path $projectRoot 'bin\Release\net10.0\RaincodeJCLWrapper.dll'
    $script:server = Start-Process -FilePath (Get-Command dotnet).Source `
        -ArgumentList ('"{0}" --contentRoot "{1}"' -f $dll, $projectRoot) `
        -WorkingDirectory $projectRoot -PassThru `
        -RedirectStandardOutput (Join-Path $testRoot 'server.stdout.log') `
        -RedirectStandardError (Join-Path $testRoot 'server.stderr.log')
    $deadline = [DateTime]::UtcNow.AddSeconds(20)
    do {
        $server.Refresh()
        if ($server.HasExited) {
            throw ('Server exited: ' + [System.IO.File]::ReadAllText((Join-Path $testRoot 'server.stderr.log')))
        }
        try {
            $health = Request 'GET' '/health'
            if ($health.Status -eq 200) { return }
        }
        catch [System.Net.WebException] {
            if ($_.Exception.Status -ne [System.Net.WebExceptionStatus]::ConnectFailure) { throw }
        }
        Start-Sleep -Milliseconds 100
    } while ([DateTime]::UtcNow -lt $deadline)
    throw 'Server did not become responsive.'
}

function Stop-Server {
    if ($null -ne $script:server) {
        $script:server.Refresh()
        if (-not $script:server.HasExited) {
            Stop-Process -Id $script:server.Id -Force
            $script:server.WaitForExit()
        }
        $script:server.Dispose()
        $script:server = $null
    }
}

try {
    foreach ($folder in @('Config', 'JCL', 'Logs', 'SYSOUT', 'Data')) {
        New-Item -ItemType Directory -Path (Join-Path $testRoot $folder) | Out-Null
    }
    foreach ($name in @('PARALLEL1', 'PARALLEL2', 'PARALLEL3', 'PARALLEL4', 'PARALLEL5',
        'FAIL', 'CC4', 'CC5', 'MISSING', 'INVALID', 'ESCAPE', 'LAUNCHFAIL', 'INTERRUPTED')) {
        [System.IO.File]::WriteAllText((Join-Path $testRoot "JCL\$name.jcl"), '// FAKE JOB')
    }
    $configuration = @{
        Application = 'TEST'
        RootPath = $testRoot
        JclPath = Join-Path $testRoot 'JCL'
        LogPath = Join-Path $testRoot 'Logs'
        SysoutPath = Join-Path $testRoot 'SYSOUT'
        SubmitExecutable = (Get-Command powershell.exe).Source
        SubmitArguments = @('-NoLogo', '-NoProfile', '-NonInteractive', '-ExecutionPolicy', 'Bypass',
            '-File', (Join-Path $PSScriptRoot 'FakeSubmit.ps1'))
        EnvironmentVariables = @{
            FAKE_SYSOUT = Join-Path $testRoot 'SYSOUT'
            FAKE_OUTSIDE_RC = Join-Path $testRoot 'outside-RETURNCODE.TXT'
        }
        RetentionDays = 365
        SuccessMaxReturnCode = 4
    }
    [System.IO.File]::WriteAllText((Join-Path $testRoot 'Config\TEST.json'), ($configuration | ConvertTo-Json -Depth 5))
    $env:Wrapper__ApiKey = $apiKey
    $env:Wrapper__StoragePath = Join-Path $testRoot 'Data'
    $env:Wrapper__ConfigurationPath = Join-Path $testRoot 'Config'
    $env:Wrapper__MaxConcurrentJobs = '4'
    $env:URLS = $baseUrl
    Start-Server

    Assert-True ((Request 'GET' '/health' -WithoutKey).Status -eq 401) 'Authentication is required'
    Assert-True ((Request 'POST' '/api/jobs/submit' @{ application = 'TEST'; jobName = '..\BAD' }).Status -eq 400) 'Traversal is rejected'
    Assert-True ((Request 'POST' '/api/jobs/submit' @{ application = 'UNKNOWN'; jobName = 'JOB' }).Status -eq 404) 'Unknown application is rejected'
    Assert-True ((Request 'POST' '/api/jobs/submit' @{ application = 'TEST'; jobName = 'NOTFOUND' }).Status -eq 404) 'Missing JCL is rejected'
    foreach ($invalidStep in @('', 'STEP1..MYPROC', 'STEP1 -file=other.jcl', '..\STEP1')) {
        Assert-True ((Request 'POST' '/api/jobs/submit' @{ application = 'TEST'; jobName = 'FAIL'; restartStep = $invalidStep }).Status -eq 400) 'Invalid restart step is rejected'
    }

    $parallel = @()
    foreach ($index in 1..4) { $parallel += Submit "PARALLEL$index" }
    foreach ($job in $parallel) { Wait-ForState $job.jobId @('Running') | Out-Null }
    Assert-True ((Request 'POST' "/api/jobs/$($parallel[0].jobId)/restart" @{ restartStep = 'STEP1.MYPROC' }).Status -eq 409) 'Running job cannot be restarted'
    Assert-True ((Request 'POST' '/api/jobs/submit' @{ application = 'test'; jobName = 'parallel1' }).Status -eq 409) 'Active duplicate is rejected case-insensitively'
    $queued = Submit 'PARALLEL5'
    $queuedState = (Request 'GET' "/api/jobs/$($queued.jobId)").Body | ConvertFrom-Json
    Assert-True ($queuedState.status -eq 'Submitted') 'Fifth job is queued at the four-worker limit'
    foreach ($job in ($parallel + @($queued))) {
        $finished = Wait-ForState $job.jobId @('Completed', 'Failed')
        Assert-True ($finished.status -eq 'Completed' -and $finished.returnCode -eq 0) 'Successful job retains max CC zero'
    }
    $id = $parallel[0].jobId
    Assert-True ((Request 'GET' "/api/jobs/$id/logs").Body.Contains('Setting RCRETURNCODEPATH')) 'Execution output is captured'
    Assert-True ((Request 'GET' "/api/jobs/$id/logs?file=SYSLOG.txt").Body.Contains('Fake log')) 'Runtime logs are accessible'
    Assert-True ((Request 'GET' "/api/jobs/$id/logs?file=..%5C..%5Coutside.txt").Status -eq 400) 'Log traversal is rejected'
    Assert-True ((Request 'POST' "/api/jobs/$id/restart" @{ restartStep = 'STEP1;BAD' }).Status -eq 400) 'Restart endpoint validates step'
    $restartResponse = Request 'POST' "/api/jobs/$id/restart" @{ restartStep = 'STEP1.MYPROC' }
    Assert-True ($restartResponse.Status -eq 202) 'Completed job can be restarted'
    $restarted = $restartResponse.Body | ConvertFrom-Json
    Assert-True ($restarted.jobId -ne $id -and $restarted.parentJobId -eq $id -and $restarted.restartStep -eq 'STEP1.MYPROC') 'Restart is a separate linked execution'
    Assert-True ((Wait-ForState $restarted.jobId @('Completed', 'Failed')).status -eq 'Completed') 'Completed-job restart succeeds'
    Assert-True ((Request 'GET' "/api/jobs/$($restarted.jobId)/logs").Body.Contains('-RESTART=STEP1.MYPROC')) 'Exact restart argument reaches process'
    foreach ($name in @('CC4', 'CC5')) {
        $accepted = Submit $name
        $finished = Wait-ForState $accepted.jobId @('Completed', 'Failed')
        $expectedState = 'Completed'
        $expectedCc = 4
        if ($name -eq 'CC5') { $expectedState = 'Failed'; $expectedCc = 5 }
        Assert-True ($finished.status -eq $expectedState -and $finished.returnCode -eq $expectedCc -and $finished.processExitCode -eq $expectedCc) 'Matching exit/max CC 4 succeeds and CC 5 fails'
    }
    foreach ($name in @('FAIL', 'MISSING', 'INVALID', 'ESCAPE', 'LAUNCHFAIL')) {
        $accepted = Submit $name
        $finished = Wait-ForState $accepted.jobId @('Completed', 'Failed')
        Assert-True ($finished.status -eq 'Failed') "$name is reported as failed"
        if ($name -eq 'FAIL') {
            Assert-True ($finished.returnCode -eq 12 -and $finished.processExitCode -eq 12) 'Failed job preserves matching maximum CC and process exit'
            $failedRestartResponse = Request 'POST' "/api/jobs/$($finished.jobId)/restart" @{ restartStep = 'STEP1.MYPROC' }
            Assert-True ($failedRestartResponse.Status -eq 202) 'Failed job can be restarted'
            $failedRestart = $failedRestartResponse.Body | ConvertFrom-Json
            Assert-True ((Wait-ForState $failedRestart.jobId @('Completed', 'Failed')).status -eq 'Completed') 'Failed-job step restart succeeds'
        }
        $directResponse = Request 'POST' '/api/jobs/submit' @{ application = 'TEST'; jobName = 'FAIL'; restartStep = 'STEP1.MYPROC' }
        Assert-True ($directResponse.Status -eq 202) 'Submit route accepts explicit restartStep'
        $directRestart = $directResponse.Body | ConvertFrom-Json
        Assert-True ((Wait-ForState $directRestart.jobId @('Completed', 'Failed')).status -eq 'Completed') 'Direct step restart succeeds'
        if ($name -eq 'LAUNCHFAIL') {
            Assert-True ($finished.returnCode -eq 0 -and $finished.processExitCode -eq 7) 'Launcher failure is preserved separately'
        }
        if ($name -in @('MISSING', 'INVALID', 'ESCAPE')) {
            Assert-True ($null -eq $finished.returnCode -and $finished.error) 'Missing or invalid CC is not success-shaped'
        }
    }

    Stop-Server
    $seed = Get-Content -LiteralPath (Join-Path $testRoot "Data\$id.json") -Raw | ConvertFrom-Json
    $seed.jobId = [guid]::NewGuid().ToString('N')
    $seed.jobName = 'INTERRUPTED'
    $seed.jclFile = Join-Path $testRoot 'JCL\INTERRUPTED.jcl'
    $seed.logDirectory = Join-Path $testRoot ("Logs\INTERRUPTED\" + $seed.jobId)
    $seed.status = 'Running'
    $seed.endTime = $null
    $seed.returnCode = $null
    $seed.processExitCode = $null
    New-Item -ItemType Directory -Path $seed.logDirectory -Force | Out-Null
    [System.IO.File]::WriteAllText((Join-Path $testRoot ("Data\" + $seed.jobId + '.json')), ($seed | ConvertTo-Json))
    Start-Server
    $persistedState = (Request 'GET' "/api/jobs/$id").Body | ConvertFrom-Json
    $recoveredState = (Request 'GET' "/api/jobs/$($seed.jobId)").Body | ConvertFrom-Json
    Assert-True ($persistedState.status -eq 'Completed') 'Completed status survives restart'
    Assert-True ($recoveredState.status -eq 'Interrupted') 'Unfinished stored job is marked interrupted'
    $persistedRestart = (Request 'GET' "/api/jobs/$($restarted.jobId)").Body | ConvertFrom-Json
    Assert-True ($persistedRestart.restartStep -eq 'STEP1.MYPROC' -and $persistedRestart.parentJobId -eq $id) 'Restart metadata survives service restart'
    Assert-True ((Request 'POST' "/api/jobs/$($seed.jobId)/restart" @{ restartStep = 'STEP1.MYPROC' }).Status -eq 409) 'Interrupted execution cannot be restarted before reconciliation'
    Assert-True ((Request 'POST' '/api/jobs/submit' @{ application = 'TEST'; jobName = 'INTERRUPTED' }).Status -eq 409) 'Interrupted execution blocks accidental resubmission'
    Assert-True ((Request 'POST' "/api/jobs/$($seed.jobId)/acknowledge-interruption").Status -eq 200) 'Operator reconciliation releases the hold'
    $retry = Submit 'INTERRUPTED'
    Assert-True ((Wait-ForState $retry.jobId @('Completed', 'Failed')).status -eq 'Completed') 'Acknowledged job can be resubmitted'
    Write-Output "Test artifacts: $testRoot"
    Write-Output 'PASS: authentication, validation, parallel limit, duplicates, max CC threshold, logs, step restart and persistence.'
}
catch {
    Write-Warning "Integration test failure. Artifacts retained at $testRoot"
    throw
}
finally {
    Stop-Server
    foreach ($name in $names) {
        [Environment]::SetEnvironmentVariable($name, $originalEnvironment[$name], 'Process')
    }
}
