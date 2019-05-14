Write-Host "Running integration test on an example file"

# Don't forget to build the latest version of the executable
stack install ..
pandoc --filter pandoc-pyplot -i ./test/fixtures/integration.md -o ./generated/test.html

if ($LastExitCode -ne 0)
    {
        Write-Host "Integration test failed with exit code " + $LastExitCode
        $FinalExitCode=$LastExitCode
     }

# Cleanup
Remove-Item -Recurse -Force generated

return $FinalExitCode