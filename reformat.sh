#! /usr/bin/env bash

set -eux


workspace="$( dirname "${BASH_SOURCE[0]}" )"

dotnet tool restore
dotnet tool run fantomas -r "${workspace}"
dotnet dotnet format "${workspace}"
