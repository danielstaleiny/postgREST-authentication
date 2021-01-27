#!/usr/bin/env bash
set -euo pipefail

# export .env variables in the project
export $(grep -v '^#' .env | xargs -d '\n')
