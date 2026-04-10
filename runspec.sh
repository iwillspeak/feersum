#!/usr/bin/env bash

#
# Compile and run a spec file.
#
# Usage:
#   runspec.sh [OPTIONS] <specname>
#
# Arguments:
#   <specname>             Name of the spec file (with or without .scm suffix).
#
# Options:
#   --no-build             Skip rebuilding the compiler.
#   --configuration <cfg>  Build configuration (Release/Debug). Default: Release.
#

config="Release"
no_build=false
specname=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --no-build)
            no_build=true
            shift
            ;;
        --configuration)
            config="$2"
            shift 2
            ;;
        -*)
            echo "Error: unknown option '$1'" >&2
            exit 1
            ;;
        *)
            if [[ -z "$specname" ]]; then
                specname="$1"
            else
                echo "Error: multiple spec names provided" >&2
                exit 1
            fi
            shift
            ;;
    esac
done

if [[ -z "$specname" ]]; then
    cat <<EOF
Usage: runspec.sh [OPTIONS] <specname>

Arguments:
  <specname>             Name of the spec file (with or without .scm suffix).

Options:
  --no-build             Skip rebuilding the compiler.
  --configuration <cfg>  Build configuration (Release/Debug). Default: Release.
EOF
    exit 1
fi

# Normalize specname: remove .scm suffix if present
specname="${specname%.scm}"

# Find exactly one matching .scm file
cd "$(dirname "${BASH_SOURCE[0]}")"
mapfile -t matches < <(fd "$specname" spec -e scm -d 1)

if [[ ${#matches[@]} -eq 0 ]]; then
    echo "Error: no spec file found matching '$specname'" >&2
    exit 1
fi

if [[ ${#matches[@]} -gt 1 ]]; then
    echo "Error: multiple spec files match '$specname':" >&2
    printf '  %s\n' "${matches[@]}" >&2
    exit 1
fi

specfile="${matches[0]}"
specbin="spec/bin/$(basename "${specfile%.scm}.exe")"

set -euo pipefail

# Build if not --no-build
if [[ "$no_build" == false ]]; then
    dotnet build --configuration "$config" /nologo /consoleloggerparameters:NoSummary -v:q src/Feersum
fi

# Compile and run
dotnet src/Feersum/bin/"$config"/net8.0/Feersum.dll "$specfile" \
    --outputtype exe -o "$specbin" && \
    dotnet "$specbin"
