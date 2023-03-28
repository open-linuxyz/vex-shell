#!/usr/bin/env zsh
# Author: Michael Lloyd <micl_dev@protonmail.com>
# Modified: 28/3/23
# 
#
function _setenv_chooseenv_prompt() {
    local environments=("${CMD_SETENV_DIR}"/*)
    local env_name
    local choice
    local counter=1

    if [[ -n "$environments" ]]; then
        echo "Choose an environment:"
        for env_file in "${environments[@]}"; do
            env_name="$(basename "${env_file}")"
            echo "  ${counter}) ${env_name}"
            counter=$((counter + 1))
        done
        echo "  ${counter}) None"

        echo -n "Enter the number of your choice: "
        read choice

        if [[ $choice -gt 0 ]] && [[ $choice -lt $counter ]]; then
            env_name="$(basename "${environments[$((choice - 1))]}")"
            _setenv_load_environment "${env_name}"  # Updated function name
        fi
    else
        echo "No environments found."
    fi
}
