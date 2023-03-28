#!/usr/bin/env zsh
# Author: Michael Lloyd <micl_dev@protonmail.com>
# Modified: 28/3/23
# 
#
function _setenv_list_environments() {
    local environments=("${CMD_SETENV_DIR}"/*)
    local env_name

    if [[ -n "$environments" ]]; then
        echo "Available environments:"
        for env_file in "${environments[@]}"; do
            env_name="$(basename "${env_file}")"
            echo "  ${env_name}"
        done
    else
        echo "No environments found."
    fi
}

