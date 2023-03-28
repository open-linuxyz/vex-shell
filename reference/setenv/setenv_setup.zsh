#!/usr/bin/env zsh
# Author: Michael Lloyd <micl_dev@protonmail.com>
# Modified: 28/3/23
# 
#
function _setenv_setup() {
    # Create the CMD_SETENV_DIR directory if it doesn't exist
    mkdir -p "${CMD_SETENV_DIR}"

    # Create the default.bak.env file in the CMD_SETENV_DIR directory
    local backup_file="${CMD_SETENV_DIR}/default.bak.env"

    # Check if the backup file already exists, if not, create it
    if [[ ! -e "${backup_file}" ]]; then
        # Get the current environment variables and save them in the backup file
        env | grep -v 'SETENV_' > "${backup_file}"
    fi
}
