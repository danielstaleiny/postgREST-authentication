#!/usr/bin/env bash
set -euo pipefail

# export .env variables in the project
./export-env

psql postgres://${SUPER_USER}:${SUPER_USER_PASSWORD}@localhost/${DB_NAME} -f db/src/init.sql
