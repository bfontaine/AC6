# Bash completion for 'clap' (AC6).
# Source it somewhere in your .bashrc,
# or copy it in /etc/bash_completion.d/

_clap() {

    local cur

    cur=${COMP_WORDS[COMP_CWORD]}

    case "$cur" in
        -*)
        COMPREPLY=( $( compgen -W '-p --parse-only --bench --memo --type -help --help' -- $cur )) ;;

        *)
        COMPREPLY=( $( compgen -fX '!*.clap' -o plusdirs -- $cur ) ) ;;
    esac

    return 0

}

complete -F _clap -o filenames clap
