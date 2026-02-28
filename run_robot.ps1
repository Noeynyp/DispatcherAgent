# Start the FastAPI app, run Robot Framework tests, then stop the server
param(
    [int]$Port = 8000
)

$python = Join-Path -Path $PSScriptRoot -ChildPath ".venv\Scripts\python.exe"
$wrapper = Join-Path -Path $PSScriptRoot -ChildPath "tests\robot\server_wrapper.py"

Write-Output "Starting server with: $python $wrapper $Port"
$out = Join-Path -Path $PSScriptRoot -ChildPath "server_out.log"
$err = Join-Path -Path $PSScriptRoot -ChildPath "server_err.log"
 $proc = Start-Process -FilePath $python -ArgumentList ("`"$wrapper`"", "$Port") -PassThru -RedirectStandardOutput $out -RedirectStandardError $err

try {
    Write-Output "Waiting for server to start..."

    $maxWait = 15
    $waited = 0
    while ($waited -lt $maxWait) {
        if (Test-NetConnection -ComputerName 127.0.0.1 -Port $Port -WarningAction SilentlyContinue) {
            Write-Output "Server is accepting connections"
            break
        }
        Start-Sleep -Seconds 1
        $waited += 1
    }

    if ($waited -ge $maxWait) {
        Write-Output "Server did not become available within timeout. Dumping logs:"
        if (Test-Path $out) { Get-Content $out -Tail 200 }
        if (Test-Path $err) { Get-Content $err -Tail 200 }
        throw "Server failed to start"
    }

    Write-Output "Running Robot Framework tests..."
    & $python -m robot.run --outputdir results tests/robot
    # Also run the standalone visual demo (Playwright) so you can watch the UI click action
    Write-Output "Running standalone visual demo..."
    & $python tests\robot\visual_demo.py 10
} finally {
    if ($proc -and !$proc.HasExited) {
        Write-Output "Stopping server (Id=$($proc.Id))"
        Stop-Process -Id $proc.Id -Force
    }

    Write-Output "--- Server stdout (tail) ---"
    if (Test-Path $out) { Get-Content $out -Tail 200 }
    Write-Output "--- Server stderr (tail) ---"
    if (Test-Path $err) { Get-Content $err -Tail 200 }
}
