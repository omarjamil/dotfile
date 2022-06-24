path_remove() {
    PATH=$(echo -n "$PATH" | awk -v RS=: -v ORS=: "\$0 != \"$1\"" | sed 's/:$//')
}

path_append() {
    path_remove "$1"
    PATH="${PATH:+"$PATH:"}$1"
}

path_prepend() {
    path_remove "$1"
    PATH="$1${PATH:+":$PATH"}"
}

conda_path_remove() {
    PATH=${PATH_BACK}
}

conda_init() {
    PATH_BACK=$PATH
    . $HOME/.shell/mamba_init.sh 
}

mamba_init() {
    PATH_BACK=$PATH
    . $HOME/.shell/mamba_init.sh
}

isca_vim() {
    if [ "$#" -eq 1 ]; then
        vim scp://oj244@login.isca.ex.ac.uk//gpfs/ts0/home/oj244/$1
    else
        echo "Provide path (from home) to the file on ISCA"
    fi
}

remote_vim() {
    if [ "$#" -eq 1 ]; then
        vim scp://oj244@$1/
    else
        echo "Provide path server address"
    fi
}

here() {
    local loc
    if [ "$#" -eq 1 ]; then
        loc=$(realpath "$1")
    else
        loc=$(realpath ".")
    fi
    ln -sfn "${loc}" "$HOME/.shell.here"
    echo "here -> $(readlink $HOME/.shell.here)"
}

there="$HOME/.shell.here"

there() {
    cd "$(readlink "${there}")"
}

ncless() {
    ncdump $1 | less
}

lg()
{
    export LAZYGIT_NEW_DIR_FILE=~/.lazygit/newdir

    lazygit "$@"

    if [ -f $LAZYGIT_NEW_DIR_FILE ]; then
            cd "$(cat $LAZYGIT_NEW_DIR_FILE)"
            rm -f $LAZYGIT_NEW_DIR_FILE > /dev/null
    fi
}

