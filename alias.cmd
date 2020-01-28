@echo off

DOSKEY vs = code .
DOSKEY exp = explorer .
DOSKEY r = wsl ranger
DOSKEY dr = wsl . ranger
DOSKEY ps = powershell
DOSKEY psa = powershell Start-Process powershell -Verb runAs
DOSKEY ud = wsl bash svn-update-all
DOSKEY rc = reg delete HKEY_CURRENT_USER\Software\5DT\SBS /f
DOSKEY v = nvim g:\fd

DOSKEY back-up-configs = copy /Y "C:\Users\User\alias.cmd" "C:\Users\User\Desktop\configs\alias.cmd" $T copy /Y "C:\Users\User\emacs\.spacemacs" "C:\Users\User\Desktop\configs\.spacemacs" $T copy /Y "C:\Users\User\Desktop\vim_port\vim\_vimrc" "C:\Users\User\Desktop\configs\_vimrc" $T wsl cp ~/.bash_aliases /mnt/c/Users/User/Desktop/configs/.bash_aliases -f -T
DOSKEY restore-configs = copy /Y "C:\Users\User\Desktop\configs\alias.cmd" "C:\Users\User\alias.cmd" $T copy /Y "C:\Users\User\Desktop\configs\.spacemacs" "C:\Users\User\emacs\.spacemacs" $T copy /Y "C:\Users\User\Desktop\configs\_vimrc" "C:\Users\User\Desktop\vim_port\vim\_vimrc" $T wsl cp /mnt/c/Users/User/Desktop/configs/.bash_aliases ~/.bash_aliases -f -T