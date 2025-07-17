#! /usr/bin/env bash

set -eou pipefail

# This script is used to set environment variables from a file or directly.
# It checks if the variable is set directly or if a corresponding _FILE variable is set.
# This is used as an entrypoint for Docker containers to securely provide sensitive information
# via docker secrets.
file_env() {
	local var="$1"
	local fileVar="${var}_FILE"
	if [ "${!var:-}" ] && [ "${!fileVar:-}" ]; then
		printf >&2 'error: both %s and %s are set (but are exclusive)\n' "$var" "$fileVar"
		exit 1
	fi
    unset="false"
	if [ "${!var:-}" ]; then
		val="${!var}"
	elif [ "${!fileVar:-}" ]; then
		val="$(< "${!fileVar}")"
	else
        unset="true"
    fi
    if [ "$unset" = "false" ]; then
	    export "$var"="$val"
	    unset "$fileVar"
    fi
}

file_env "AVATAX_ACCOUNT_ID"
file_env "AVATAX_COMPANY_ID"
file_env "AVATAX_LICENSE_KEY"

file_env "STONE_EDGE_PASS"
file_env "STONE_EDGE_CODE"

file_env "STRIPE_TOKEN"
file_env "HELCIM_TOKEN"

file_env "DB_PASS"
file_env "SMTP_PASS"

file_env "COOKIE_SECRET"

exec "$@"
