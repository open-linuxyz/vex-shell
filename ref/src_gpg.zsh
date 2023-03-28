#!/usr/bin/env zsh

function showme() {
    case "$1" in
        gpg|gnupg)
            # call the function to show gpg information
            _showmegpg
            ;;
        gpt|openai-gpt)
            _decrypt_gpt 
            ;;
        help|h|-h|--help)
            echo "Showing help information"
            # call the function to show help information
            ;;
        homedir|home|~)
            echo "Showing home directory information"
            # call the function to show home directory information
            ;;
        hostname|computer)
            echo "Showing hostname information"
            # call the function to show hostname information
            ;;
        ssh)
            echo "Showing SSH information"
            # call the function to show ssh information
            local argflag="$2"
            if [[ ! -z $argflag ]]; then 
                select_public_key $argflag 
            else 
                select_public_key
            fi
            ;;
        *)
            echo "invalid option. use 'showme help' for more information."
            ;;
    esac
}

function _showmegpg() { 
            local gpgpubkey=( 
                "pub\tnistp521 2023-03-24 [S]"
                "uid\tMichael Lloyd (mwl) <micl_dev@protonmail.com>"
                "8A9126330BDC065C23A79C342F1E95D2CBB41C0C"
            )
            print -l "${gpgpubkey[@]}"

} 

function select_public_key() {
  local clipboard_flag=false
  local help_flag=false

  for arg in "$@"; do
    case $arg in
      -x|--clipboard|clipboard)
        clipboard_flag=true
        ;;
      -h|help|h|--help)
        help_flag=true
        ;;
    esac
  done

  if $help_flag; then
    echo "Usage: select_public_key [options]"
    echo "Options:"
    echo "  -x, --clipboard, clipboard   Copy the selected public key to the clipboard instead of displaying it"
    echo "  -h, help, h, --help          Show this help message"
    return
  fi

  local key_files=(~/.ssh/public/*.pub)
  local key_descriptions=()

  # Collect the key descriptions
  for key_file in $key_files; do
    local description=$(ssh-keygen -B -f $key_file | awk '{$1=$2=""; $NF=""; print substr($0, 3)}')
    key_descriptions+=("$description")
  done

  # List the keys with numbers
  local index=1
  for description in $key_descriptions; do
    echo "$index) $description"
    index=$((index + 1))
  done

  # Prompt the user to select a key
  local user_input
  echo -n "Enter the number of the key you'd like to display: "
  vared -p "" -c user_input

  # Validate the user input
  if [[ -z $user_input ]] || ! [[ $user_input =~ ^[0-9]+$ ]] || ((user_input < 1 || user_input > ${#key_descriptions[@]})); then
    echo "Invalid input. Please enter a valid number."
    return 1
  fi

  # Display the selected key and clear the screen
  clear
  if $clipboard_flag; then
    # Copy the public key to the clipboard
    if command -v xclip >/dev/null 2>&1; then
      cat ${key_files[$user_input]} | xclip -selection primary
      echo "The selected public key has been copied to your clipboard."
    else
      echo "xclip is not installed. Please install it to use clipboard functionality."
    fi
  else
    # Output the public key to the terminal
    cat ${key_files[$user_input]}
  fi
}

# export the function as an alias
alias showme="showme"
compdef _showme showme
