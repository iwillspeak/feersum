#! /usr/bin/env bash

if [ $# -ne 1 ]
then
    cat <<EOF
Usage: runspec.sh <specname>

Arguments:
  <specname>       The name of the specfile to compile and run.
EOF
    exit -1
fi

specname=`fd $1 spec/`
if [ $? -ne 0 ]
then
    echo >2 "Can't find spec file $1"
fi

specbin=spec/bin/$(basename ${specname/.scm/.exe})

cd "$( dirname "${BASH_SOURCE[0]}" )"

set -euo pipefail

dotnet build --configuration Release src/Feersum && \
    dotnet src/Feersum/bin/Release/net7.0/Feersum.dll ${specname} \
        --outputtype exe -o ${specbin} && \
    dotnet ${specbin}
