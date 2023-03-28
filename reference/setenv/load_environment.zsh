#!/usr/bin/env zsh
# Author: Michael Lloyd <micl_dev@protonmail.com>
# Modified: 28/3/23
# 
#
function _setenv_load_environment() {
    local env_name="$1"
    local env_file="${CMD_SETENV_DIR}/${env_name}"

    if [[ -f "${env_file}" ]]; then
        source "${env_file}"
        echo "Environment '${env_name}' loaded."
    else
        echo "Error: Environment '${env_name}' not found."
    fi
}
