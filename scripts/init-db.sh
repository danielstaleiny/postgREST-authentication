#!/usr/bin/env bash
set -euo pipefail

# export .env variables in the project
export $(grep -v '^#' .env | xargs -d '\n')

psql postgres://${SUPER_USER}:${SUPER_USER_PASSWORD}@localhost/${DB_NAME} -f db/src/init.sql
