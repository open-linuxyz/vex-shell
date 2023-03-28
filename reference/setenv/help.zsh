#!/usr/bin/env zsh
# Author: Michael Lloyd <micl_dev@protonmail.com>
# Modified: 28/3/23
# 
#
function _setenv_help() {
    echo "Usage:"
    echo "  setenv -n <ENVIRONMENT> | --new <ENVIRONMENT>  Create a new environment and store current settings."
    echo "  setenv -z                                     Reset the current environment and unset SETENV_ variables."
    echo "  setenv <ENVIRONMENT>                          Load the specified environment."
    echo "  setenv                                        Prompt the user to choose an environment from a list."
    echo "  setenv -h | --help                            Show this help message."
}
