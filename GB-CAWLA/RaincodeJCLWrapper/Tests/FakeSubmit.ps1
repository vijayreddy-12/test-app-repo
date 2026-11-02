$ErrorActionPreference = 'Stop'
$fileArgument = @($args | Where-Object { $_ -like '-file=*' })
if ($fileArgument.Count -ne 1) {
    throw 'Expected exactly one -file=<JCL> argument.'
}
$restartArguments = @($args | Where-Object { $_ -like '-RESTART=*' })
if ($restartArguments.Count -gt 1) { throw 'Expected at most one -RESTART argument.' }
if ($restartArguments.Count -eq 1) { Write-Output "Restart argument: $($restartArguments[0])" }
$jobName = [System.IO.Path]::GetFileNameWithoutExtension($fileArgument[0].Substring(6))
$runtimeId = 'JOB{0:D10}' -f $PID
$directory = Join-Path (Join-Path $env:FAKE_SYSOUT $jobName) $runtimeId
New-Item -ItemType Directory -Path $directory -Force | Out-Null
$returnFile = Join-Path $directory 'RETURNCODE.TXT'
Write-Output "$runtimeId : Scanning $($fileArgument[0].Substring(6))"
Write-Output "$runtimeId : Setting RCRETURNCODEPATH = $returnFile"
[System.IO.File]::WriteAllText((Join-Path $directory 'SYSLOG.txt'), "Fake log for $jobName")
if ($jobName -match '^PARALLEL') {
    Start-Sleep -Seconds 8
}
if ($jobName -eq 'MISSING') {
    Write-Output "$runtimeId : Job finished without a return-code file"
    exit 0
}
if ($jobName -eq 'INVALID') {
    [System.IO.File]::WriteAllText($returnFile, 'not-a-number')
    exit 0
}
if ($jobName -eq 'ESCAPE') {
    Write-Output "$runtimeId : Setting RCRETURNCODEPATH = $env:FAKE_OUTSIDE_RC"
    exit 0
}
$returnCode = 0
if ($jobName -eq 'FAIL') { $returnCode = 12 }
if ($jobName -eq 'CC4') { $returnCode = 4 }
if ($jobName -eq 'CC5') { $returnCode = 5 }
if ($jobName -eq 'FAIL' -and $restartArguments.Count -eq 1) { $returnCode = 0 }
[System.IO.File]::WriteAllText($returnFile, [string]$returnCode)
Write-Output "$runtimeId : Job finished with status: NORMAL - RC[$returnCode]/[$returnCode]"
if ($jobName -eq 'LAUNCHFAIL') { exit 7 }
exit $returnCode
