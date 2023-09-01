alias ,git="GIT_SSH_COMMAND=\"ssh -i ~/.ssh/id_ed255519_ojgit\" git $@"
# Stop from sleeping for an hour
alias caffeine="caffeinate -u -t 3600"
alias isca="ssh oj244@login.isca.ex.ac.uk"
alias lrt="ls -lrt"
alias d='dirs -v | head -10'
alias ,grep='grep  --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
alias paraview='/Applications/ParaView-5.10.0.app/Contents/MacOS/paraview'
# Use colors in coreutils utilities output
alias ls='ls -GF --color'
# ls aliases
alias ll='ls -lah --color'
alias la='ls -A --color'
alias l='ls --color'

# Aliases to protect against overwriting
alias cp='cp -i'
alias mv='mv -i'

# git related aliases
alias gag='git exec ag'
alias e='emacs -nw'
alias ec='emacsclient'
alias emacs_server='emacs --daemon'
alias ekill='emacsclient  -e "(save-buffers-kill-emacs)"'
alias condad='conda deactivate'
alias condaa='conda activate $1'
alias projects='cd ~/home/Projects/'
alias arm='env /usr/bin/arch -arm64 /bin/zsh --login'
alias intel='env /usr/bin/arch -x86_64 /bin/zsh --login'
alias ubu='ssh oj244@192.168.64.4'

keyring() {
    eval `keychain --eval $HOME/.ssh/id_ed25519`
}

# Update dotfiles
dfu() {
    (
        cd ~/.dotfiles && git pull --ff-only && ./install -q
    )
}

# Use pip without requiring virtualenv
syspip() {
    PIP_REQUIRE_VIRTUALENV="" pip "$@"
}

syspip2() {
    PIP_REQUIRE_VIRTUALENV="" pip2 "$@"
}

syspip3() {
    PIP_REQUIRE_VIRTUALENV="" pip3 "$@"
}

# cd to git root directory
alias cdgr='cd "$(git root)"'

# Create a directory and cd into it
mcd() {
    mkdir "${1}" && cd "${1}"
}

# Jump to directory containing file
jump() {
    cd "$(dirname ${1})"
}

# cd replacement for screen to track cwd (like tmux)
scr_cd()
{
    builtin cd $1
    screen -X chdir "$PWD"
}

if [[ -n $STY ]]; then
    alias cd=scr_cd
fi

# Go up [n] directories
up()
{
    local cdir="$(pwd)"
    if [[ "${1}" == "" ]]; then
        cdir="$(dirname "${cdir}")"
    elif ! [[ "${1}" =~ ^[0-9]+$ ]]; then
        echo "Error: argument must be a number"
    elif ! [[ "${1}" -gt "0" ]]; then
        echo "Error: argument must be positive"
    else
        for ((i=0; i<${1}; i++)); do
            local ncdir="$(dirname "${cdir}")"
            if [[ "${cdir}" == "${ncdir}" ]]; then
                break
            else
                cdir="${ncdir}"
            fi
        done
    fi
    cd "${cdir}"
}

# Execute a command in a specific directory
xin() {
    (
        cd "${1}" && shift && "${@}"
    )
}

# Check if a file contains non-ascii characters
nonascii() {
    LC_ALL=C grep -n '[^[:print:][:space:]]' "${1}"
}

# Fetch pull request

fpr() {
    if ! git rev-parse --git-dir > /dev/null 2>&1; then
        echo "error: fpr must be executed from within a git repository"
        return 1
    fi
    (
        cdgr
        if [ "$#" -eq 1 ]; then
            local repo="${PWD##*/}"
            local user="${1%%:*}"
            local branch="${1#*:}"
        elif [ "$#" -eq 2 ]; then
            local repo="${PWD##*/}"
            local user="${1}"
            local branch="${2}"
        elif [ "$#" -eq 3 ]; then
            local repo="${1}"
            local user="${2}"
            local branch="${3}"
        else
            echo "Usage: fpr [repo] username branch"
            return 1
        fi

        git fetch "git@github.com:${user}/${repo}" "${branch}:${user}/${branch}"
    )
}

# Serve current directory

serve() {
    ruby -run -e httpd . -p "${1:-8080}"
}

# Mirror a website
alias mirrorsite='wget -m -k -K -E -e robots=off'

# Mirror stdout to stderr, useful for seeing data going through a pipe
alias peek='tee >(cat 1>&2)'
