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

dotnet build --configuration Release Feersum.sln > /dev/null && \
    cp src/Feersum.Core/bin/Release/net5.0/Feersum.Core.dll spec/bin && \
    cp src/Serehfa/bin/Release/netstandard2.1/Serehfa.dll spec/bin && \
    dotnet src/Feersum/bin/Release/net5.0/Feersum.dll ${specname} --reference src/Feersum.Core/bin/Release/net5.0/Feersum.Core.dll --outputtype exe -o ${specbin} && \
    dotnet ${specbin}
