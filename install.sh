
cd 
# Backup existing configs
#mkdir config_backup
#mv .gitconfig .git_ignore .emacs .emacs.d .bashrc .bash_profile .vimrc .vim .bash config_backup
# Use this command to unlink everthing in the homedirectory
# find . -maxdepth 1 -type l -exec unlink {} \;

# create softlinks 
ln -s ~/dotfiles/bash ~/.bash
ln -s ~/dotfiles/bashrc ~/.bashrc
ln -s ~/dotfiles/zsh ~/.zsh
ln -s ~/dotfiles/zshrc ~/.zshrc
ln -s ~/dotfiles/bash_profile ~/.bash_profile
ln -s ~/dotfiles/vim ~/.vim
ln -s ~/dotfiles/vimrc ~/.vimrc
ln -s ~/dotfiles/shell ~/.shell
ln -s ~/dotfiles/tmux.conf ~/.tmux.conf
ln -s ~/dotfiles/nvim ~/.config/nvim
ln -s ~/dotfiles/emacs ~/.emacs
ln -s ~/dotfiles/emacs.d ~/.emacs.d


echo "To install neovim"
echo "curl -LO https://github.com/neovim/neovim/releases/latest/download/nvim.appimage && chmod u+x nvim.appimage"
