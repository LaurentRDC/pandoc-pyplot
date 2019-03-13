"Build executable and move here"
stack install pandoc-pyplot --local-bin-path ".\installer\executable"

Write-Host "Building setup using Inno Setup Compiler"
if ($ENV:PROCESSOR_ARCHITECTURE -eq "AMD64"){
    $iscc = get-item "C:\Program Files (x86)\Inno Setup 5\ISCC.exe"
}
else {
    $iscc = get-item "C:\Program Files\Inno Setup 5\ISCC.exe"
}
& $iscc "installer\pandoc-pyplot-setup.iss"