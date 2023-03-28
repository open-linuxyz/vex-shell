#!/usr/bin/env zsh 
# Author: Michael Lloyd <micl_dev@protonmail.com>
# Modified: 28/3/23
# 
#
function _setenv_reset_environment() {
    for var in ${(k)parameters}; do
        if [[ $var == SETENV_* ]]; then
            unset $var
        fi
    done
}
