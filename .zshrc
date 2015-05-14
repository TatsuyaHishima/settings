############################################
#ZSH Setting File
#This file is written using [http://blog.blueblack.net/item_204]
#############################################

export LANG=ja_JP.UTF-8

## 履歴の保存先
HISTFILE=$HOME/.zsh-history
## メモリに展開する履歴の数
HISTSIZE=100000
## 保存する履歴の数
SAVEHIST=100000

## 補完機能の強化
autoload -U compinit
compinit

## コアダンプサイズを制限
limit coredumpsize 102400
## 出力の文字列末尾に改行コードが無い場合でも表示
unsetopt promptcr
## Emacsライクキーバインド設定
bindkey -e

## 色を使う
setopt prompt_subst
## ビープを鳴らさない
setopt nobeep
## 内部コマンド jobs の出力をデフォルトで jobs -l にする
setopt long_list_jobs
## 補完候補一覧でファイルの種別をマーク表示
setopt list_types
## サスペンド中のプロセスと同じコマンド名を実行した場合はリジューム
setopt auto_resume
## 補完候補を一覧表示
setopt auto_list
## 直前と同じコマンドをヒストリに追加しない
setopt hist_ignore_dups
## cd 時に自動で push
#setopt auto_pushd
## 同じディレクトリを pushd しない
setopt pushd_ignore_dups
## ファイル名で #, ~, ^ の 3 文字を正規表現として扱う
setopt extended_glob
## TAB で順に補完候補を切り替える
setopt auto_menu
## zsh の開始, 終了時刻をヒストリファイルに書き込む
setopt extended_history
## =command を command のパス名に展開する
setopt equals
## --prefix=/usr などの = 以降も補完
setopt magic_equal_subst
## ヒストリを呼び出してから実行する間に一旦編集
setopt hist_verify
## ファイル名の展開で辞書順ではなく数値的にソート
setopt numeric_glob_sort
## 出力時8ビットを通す
setopt print_eight_bit
## ヒストリを共有
setopt share_history
## 補完候補のカーソル選択を有効に
zstyle ':completion:*:default' menu select=1
## 補完候補の色づけ
eval `dircolors`
export ZLS_COLORS=$LS_COLORS
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
## ディレクトリ名だけで cd
setopt auto_cd
## カッコの対応などを自動的に補完
setopt auto_param_keys
## ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt auto_param_slash
## スペルチェック
setopt correct
## {a-c} を a b c に展開する機能を使えるようにする
setopt brace_ccl
## Ctrl+S/Ctrl+Q によるフロー制御を使わないようにする
setopt NO_flow_control
## コマンドラインの先頭がスペースで始まる場合ヒストリに追加しない
setopt hist_ignore_space
## コマンドラインでも # 以降をコメントと見なす
setopt interactive_comments
## ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt mark_dirs
## history (fc -l) コマンドをヒストリリストから取り除く。
setopt hist_no_store
## 補完候補を詰めて表示
setopt list_packed
## 最後のスラッシュを自動的に削除しない
setopt noautoremoveslash


##############################################
#ZSH Color Setting File
#This file is written using [http://d.hatena.ne.jp/sugyan/20090331/1238475537]
#############################################
#zstyle ':completion:*' list-colors 'di=36' 'ln=35'
#zstyle ':completion:*:default' menu select=1

#autoload predict-on
#predict-on

#precmd () {
#  PROMPT=$'%{\e[0;33m%}%* ('$(sysctl -n vm.loadavg | perl -anpe '$_=$F[1]')$') %%%{\e[m%} '
#}
#RPROMPT=$'%{\e[32m%}%/%{\e[m%}'

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data

# setopt auto_cd
#setopt auto_pushd
setopt correct

export PATH=/opt/local/bin:/opt/local/sbin:/usr/local/bin:$PATH
export MANPATH=$MANPATH:/opt/local/share/man
export LSCOLORS=gxfxxxxxcxxxxxxxxxxxxx

alias ls="ls -G"

##############################################
#ZSH Color Setting File
#This file is written using [http://ho-ki-boshi.blogspot.com/2007/12/zsh.html]
#############################################
#local LEFTC=$'%{\e[255;255;0m%}'
#local RIGHTC=$'%{\e[38;5;255m%}'
#local DEFAULTC=$'%{\e[m%}'
autoload colors
colors
export PROMPT="%{${fg[green]}%}%~%{${reset_color}%}
%{${fg[blue]}%}[%n@${HOST}]$%{${reset_color}%} "
PROMPT2="%{${fg[blue]}%}[%n]%{${reset_color}%}>" 
#export PROMPT='%{${fg[blue]}%}[%n@%m] %(!.#.$) %{${reset_color}%}[\n]'
#export PROMPT=$LEFTC"%U$USER%%%u "$DEFAULTC
#export RPROMPT=$RIGHTC"[%~]"$DEFAULTC
#local GREEN=$'%{\e[1;32m%}'
#local YELLOW=$'{\e[1;33m%}'
#local BLUE=$'%{\e[1;34m%}'
#local DEFAULT=$'%{\e[1;m}'
#PROMPT=$'\n'$GREEEN'${USER}@${HOSTNAME} '$YELLOW' %~ '$'\n'$DEFAULT'%(!.#.$) '

#Editted by jincun108 (Tomoshi IMAMURA)
#This is for start-up settings
#and can be modified by others.
alias minicom="LANG=C minicom"
#echo "Hello World"; echo "You've got NO time to sleep, you know? Anyway, Let's get started." 
alias ls="ls --color -B"
alias f="firefox"
alias e="emacs"
alias ec="emacsclient"
alias -g NL='/dev/null'
alias l="ls"
#alias sl="ls"
alias v="vim"
alias c="cat"
alias sshut="sshfs g140417@un001.ecc.u-tokyo.ac.jp:/home07/g140417 ~/eccs"
alias less="less -R"

#For executing mikutter 
alias mikutter="ruby1.9.3 ~/mikutter/mikutter.rb"
####################

#For executing kankore
alias kankore="/home/suigin/KanmusuMemory/bin/KanmusuMemory.sh"
###################

#For GTK+ Programming
#alias gtkcc='gcc -no-as-needed $(pkg-config --cflags --libs gtk+-3.0)'


#texをコンパイルしてacroreadで表示
texpdf()
{
    if [ "$1" = "" ]
    then
	echo "Usage: texpdf filename"
	return
    fi  
    echo "Now Compiling Texfile $1..."
    platex $1.tex 2> /dev/null
    dvipdfmx $1.dvi 2> /dev/null
    acroread $1.pdf 2> /dev/null &
    echo "Done"
}


#nkfでutf8に変換して同じファイルに書き込むNKF_Writeコマンド
#ただし、__tmp.dat__ファイルが存在しないことを仮定
nkfw()
{
    TARGET="$1"
    #引数が存在することを確認
    if [ "" =  "$TARGET" ]
    then
	echo -n "Usage: nkfw filename\n" >&2
	return 1
    fi
    #ターゲットの存在を確認
    if ! [ -f "$TARGET" ]
    then
        echo -n "Error: $TARGET does not exist!!\n" >&2 #エラー出力
        return 1
    fi
    #変換するファイルの属性をチェックする。バイナリならエラー
    FILE_TYPE=$(nkf -g "$TARGET")
    if [ "$FILE_TYPE" = BINARY ]
    then
        echo -n "Error: $TARGET is a binary file!!\n" >&2 #エラー出力
        return 1
    fi
    #一時ファイルに書き出して、変換
    TMP_FILE_NAME="__tmp.dat__"      #一時ファイルの名前
    nkf -w8 "$TARGET"  > $TMP_FILE_NAME
    cat $TMP_FILE_NAME > $TARGET
    rm $TMP_FILE_NAME
    echo "NKFW $TARGET Done."
}
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
