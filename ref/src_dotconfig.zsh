#!/bin/zsh --aliasfuncdef 


function config() { 
    /usr/bin/git --git-dir=$HOME/.cfg --work-tree=$HOME $@;
}


function config_update() { 
    #config commit -m \
    #    '(CRON) $HOST Ran an update on $(date)' -a; 
    #        config push 
}

function _dotconfig_bundle() { 
    

    export config_update 
    # ZSH autocomp applications 
    compdef _git config 
    compdef config=git
}
_dotconfig_bundle 

#unset _dotconfig_bundle

