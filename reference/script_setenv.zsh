#!/usr/bin/env zsh 

# Author: Michael Lloyd <micl_dev@protonmail.com>
# Modified: 28/3/23
# 
#



# Constants
export CMD_SETENV_DIR="$HOME/.cache/.zcustom/env"

source "$HOME/.config/.zcustom/autocomplete/_setenv"

function setenv() {

    # Source function files
    local script_dir="$HOME/.config/.zcustom"

    find "$script_dir/setenv" -type f -name "*.zsh" | while read -r function_file; do
        source "$function_file"
    done

    # Check if the environment directory exists, if not, create it
    mkdir -p "$CMD_SETENV_DIR"

    # Parse command line options
    case "$1" in
        -n|--new)
            if [ -n "$2" ]; then
                _setenv_create_environment "$2"
            else
                echo "Error: Please provide a name for the new environment."
                exit 1
            fi
            ;;
        -z)
            _setenv_reset_environment
            ;;
        -h|--help)
            _setenv_help
            ;;
        *)
            if [ -n "$1" ]; then
                _setenv_load_environment "$1"
            else
                _setenv_chooseenv_prompt
            fi
            ;;
    esac

    if [ -n "$SETENV_WORKDIR" ] && [ ! -t 0 ]; then
        cd "$SETENV_WORKDIR"
    fi
}


# Should be called once 
alias setenv="setenv"
compdef _setenv setenv
setenv
