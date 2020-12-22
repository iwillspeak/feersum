#! /usr/bin/env bash

set -euo pipefail

dotnet build

dotnet src/Feersum/bin/Debug/net5.0/Feersum.dll spec/chibi-r7rs-tests.scm -o spec/bin/chibi-r7rs-tests.exe

dotnet spec/bin/chibi-r7rs-tests.exe
