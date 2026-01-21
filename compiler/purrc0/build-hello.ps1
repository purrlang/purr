param(
    [string]$Source = "$PSScriptRoot\\..\\..\\examples\\hello.pu"
)

$ErrorActionPreference = "Stop"

Write-Host "Building purrc0..."
Push-Location $PSScriptRoot
cargo build
Pop-Location

Write-Host "Compiling Purr to C..."
& "$PSScriptRoot\\target\\debug\\purrc0.exe" $Source

$CFile = "$Source.c"
$OutExe = "$Source.exe"

Write-Host "Compiling C + runtime..."

$compiler = $null
if (Get-Command clang -ErrorAction SilentlyContinue) {
    $compiler = "clang"
} elseif (Get-Command gcc -ErrorAction SilentlyContinue) {
    $compiler = "gcc"
} elseif (Get-Command cl -ErrorAction SilentlyContinue) {
    $compiler = "cl"
}

if (-not $compiler) {
    Write-Error "No C compiler found in PATH (clang, gcc, or cl)."
    exit 1
}

if ($compiler -eq "cl") {
    & cl $CFile "$PSScriptRoot\\rt\\purr_runtime.c" /I"$PSScriptRoot\\rt" /Fe:$OutExe
} else {
    & $compiler $CFile "$PSScriptRoot\\rt\\purr_runtime.c" -I"$PSScriptRoot\\rt" -o $OutExe
}

Write-Host "Built: $OutExe"
