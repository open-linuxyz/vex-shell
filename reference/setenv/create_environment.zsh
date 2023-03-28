#!/usr/bin/env zsh
# Author: Michael Lloyd <micl_dev@protonmail.com>
# Modified: 28/3/23
# 
#
function _setenv_create_environment() {
    local env_name="$1"
    local env_file="${CMD_SETENV_DIR}/${env_name}"

    if [[ -f "${env_file}" ]]; then
        echo "Error: Environment '${env_name}' already exists."
    else
        # Save the current environment variables
        env > "${env_file}"
        echo "Environment '${env_name}' created."
    fi
}
