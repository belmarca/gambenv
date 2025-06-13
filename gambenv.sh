#!/usr/bin/env sh

# gambenv.sh
#
# (c) 2023 - 2025 Marc-André Bélanger
#
# This shell script is only intended to set up a Gambit environment.
# It should be invoked once, from this directory.

_gambenv_get_nproc() {
    if [ "$(uname)" = "Darwin" ]; then
        sysctl -n hw.ncpu
    elif command -v nproc >/dev/null 2>&1; then
        nproc
    elif [ -r /proc/cpuinfo ]; then
        grep -c "^processor" /proc/cpuinfo
    else
        echo 1
    fi
}

## Config

GB_VERSION=v0.1
GB_GAMBIT_VERSION=v4.9.6                                # Follow latest Gambit release
GB_GAMBIT_VERSION_ALIAS="${GB_GAMBIT_VERSION}"          # Same as latest release

GB_DOTGB="${GB_DOTGB:-$HOME/.gambit_env}"               # Override if you wish.
GB_GAMBIT_GIT_URL="https://github.com/gambit/gambit"    # Should stay hosted on GitHub for a while...

GB_GAMBIT_SOURCES_DIR="${GB_DOTGB}/sources"             # Don't override.
GB_GAMBIT_SOURCE_DIR="${GB_GAMBIT_SOURCES_DIR}/gambit"  # Don't override.
GB_GAMBIT_VERSIONS_DIR="${GB_DOTGB}/versions"           # Where to place Gambit installations
GB_GAMBIT_ENV_DIR="${GB_DOTGB}/env"                     # Where to place Gambit env scripts used by the 'gambit' function
GB_GAMBIT_CURRENT="${GB_DOTGB}/current"                 # Don't override. TODO: Use when upgrading base install.
GB_GAMBIT_BIN_DIR="${GB_DOTGB}/bin"                     # Where to place Gambit aliases e.g. gsi@4.9.4

GB_DEFAULT_CONFIGURE_OPTIONS="--enable-single-host"     # Use a common configure option
GB_DEFAULT_MAKE_FLAGS="-j$(_gambenv_get_nproc)"         # Default to all cores

# NOTE: Set GB_FORCE when you want to auto-accept

## Utils

_log() {
    printf "🩓 | \\033[0;33m%s\\033[0m" "$@"
}

log() {
    printf "🩓 | \\033[0;33m%s\\033[0m\\n" "$@"
}

success() {
    printf "🩓 | \\033[0;32m%s\\033[0m\\n" "$@"
}

err() {
    printf "!!! \\033[0;31m%s\\033[0m\\n" "$@"
    exit 1
}

mkdir_or_exit() {
    if mkdir -p "${1}"; then
        log "Created ${1}."
    else
        err "Something went wrong. Exiting."
    fi
}

clone_gambit_or_exit() {
    if git clone "${GB_GAMBIT_GIT_URL}" "${GB_GAMBIT_SOURCE_DIR}"; then
        log "Done cloning Gambit."
    else
        err "Something went wrong. Exiting."
    fi
}

keep_or_modify() {
    # NOTE: Not very useful because we control calls
    if [ ! $# -eq 1 ]; then
        err "keep_or_modify takes only one argument."
    fi

    while true; do
        _log "Do you wish to modify this value? (y/n): "
        read -r answer
        case "${answer}" in
            [Yy]* )
                log "Please enter a new value:"
                read -r "$1"
                break
                ;;
            [Nn]* )
                break
                ;;
            * )
                log "Please answer yes or no."
                ;;
        esac
    done
}

set_configure_options() {
    log "The default configure options are:"
    echo ""
    echo "${GB_DEFAULT_CONFIGURE_OPTIONS}"
    echo ""
    if [ "$GB_FORCE" != "1" ]; then
        keep_or_modify "GB_DEFAULT_CONFIGURE_OPTIONS"
    fi
}

set_make_flags() {
    log "The default make flags are:"
    echo ""
    echo "${GB_DEFAULT_MAKE_FLAGS}"
    echo ""
    if [ "$GB_FORCE" != "1" ]; then
        keep_or_modify "GB_DEFAULT_MAKE_FLAGS"
    fi
}

hello() {
    log "gambenv ${GB_VERSION}"
    log
    log "This script will set up a Gambit environment. It will:"
    log
    log "  1. Create the ${GB_DOTGB} directory and some"
    log "     subdirectories."
    log "  2. Clone Gambit Scheme from ${GB_GAMBIT_GIT_URL}."
    log "  3. Compile the latest Gambit release (${GB_GAMBIT_VERSION})."
    log
    _log "Do you want to proceed? (y/n): "
    if [ "$GB_FORCE" != "1" ]; then
        read -r choice
        case "$choice" in
            y|Y )
            ;;
            * )
                err "Exiting.";;
        esac
    fi

}

compile_and_install_gambit() {
    # NOTE: The first version used by Gambit will not be removable
    # from within gsi. It is not 'selectable' either by the 'gambit'
    # function in the 'gambit.sh' script.

    log "Configuring and compiling Gambit ${GB_GAMBIT_VERSION}..."

    # Allow the user to change the default alias
    log "The default Gambit version alias is:"
    echo ""
    echo "${GB_GAMBIT_VERSION_ALIAS}"
    echo ""
    if [ "$GB_FORCE" != "1" ]; then
        keep_or_modify "GB_GAMBIT_VERSION_ALIAS"
    fi

    # The sources will only serve to compile. We put them in
    # .gambit/sources/GB_GAMBIT_VERSION_ALIAS
    # and remove them after compilation.
    SOURCE_DIR="${GB_GAMBIT_SOURCES_DIR}/${GB_GAMBIT_VERSION_ALIAS}"
    cp -R "${GB_GAMBIT_SOURCE_DIR}" "${SOURCE_DIR}"

    INSTALL_DIR="${GB_GAMBIT_VERSIONS_DIR}/${GB_GAMBIT_VERSION_ALIAS}"
    mkdir_or_exit "${INSTALL_DIR}"

    # One userlib per installation
    USERLIB="${INSTALL_DIR}/userlib"
    mkdir_or_exit "${USERLIB}"

    cd "${SOURCE_DIR}" || err "Could not change directory."

    # Is --enable-shared present?
    RPATH="-Wl,-rpath,${INSTALL_DIR}/lib"
    RPATH_FLAG=""
    case "${GB_DEFAULT_CONFIGURE_OPTIONS}" in
        *"--enable-shared"*)
            RPATH_FLAG="--enable-ldflags-gambuild=${RPATH} LDFLAGS=${RPATH}"
            ;;
    esac

    if git checkout "${GB_GAMBIT_VERSION}"; then
        if ./configure $GB_DEFAULT_CONFIGURE_OPTIONS --prefix="${INSTALL_DIR}" --enable-default-runtime-options=~~userlib="${USERLIB}" $RPATH_FLAG; then
            log "Successfully configured Gambit."
            if make ${GB_DEFAULT_MAKE_FLAGS}; then
                log "Successfully built Gambit."
                if make install; then
                    log "Successfully installed Gambit in ${INSTALL_DIR}."
                    # Symlink current version
                    ln -s "${INSTALL_DIR}" "${GB_GAMBIT_CURRENT}"
                else
                    err "Error installing Gambit."
                fi
            else
                err "Error during make."
            fi
        else
            err "Error during configure."
        fi
    else
        err "Error checking out git tag ${GB_GAMBIT_VERSION}."
    fi

    # Create @version symlinks for easy access
    ln -s "${INSTALL_DIR}/bin/gsi" "${GB_GAMBIT_BIN_DIR}/gsi@${GB_GAMBIT_VERSION_ALIAS}"
    ln -s "${INSTALL_DIR}/bin/gsc" "${GB_GAMBIT_BIN_DIR}/gsc@${GB_GAMBIT_VERSION_ALIAS}"

    log "Done."
}

# TODO: is current/bin required?
goodbye() {
    success
    success "A Gambit environment has been successfully set up! You must add"
    success
    success "  ${GB_GAMBIT_BIN_DIR}"
    success
    success "to your PATH to start using Gambit. You must also add"
    success
    success "  source ${pwd}/gambit.sh"
    success
    success "to your shell's rc file to enable the Gambit Version Manager."
    success "If you compiled with '--enable-shared', Gambit will automatically"
    success "add the proper -rpath to your compiled executables. You should not"
    success "have to modify LD_LIBRARY_PATH or equivalent."
    success
}

create_bin_symlinks() {
    # Put symlinks for all Gambit binaries in .gambit_env/bin.
    # These binaries will be the default installation, i.e. the one
    # in your PATH by default, without using the Gambit Version Manager.
    # TODO: Figure out how to upgrade the base/default installation.
    for b in gambdoc gambuild-arm gambuild-C gambuild-java gambuild-js gambuild-php gambuild-python gambuild-riscv-32 gambuild-riscv-64 gambuild-ruby gambuild-x86 gambuild-x86-64 gambvcs gsc gsc-script gsi gsi-script scheme-ieee-1178-1990 scheme-r4rs scheme-r5rs scheme-srfi-0 six six-script
    do
        ln -s "${GB_GAMBIT_CURRENT}/bin/${b}" "${GB_GAMBIT_BIN_DIR}/${b}"
    done
}

## TODO: Platform detection

## Main

if [ -z "$GB_DOTGB" ]; then
    err "GB_DOTGB variable is not set. Exiting."
fi

# TODO: I turned this off to allow building in a container.
# The container will mount a volume at .gambit_env, thus this test will
# fail.
# if [ -d "$GB_DOTGB" ]; then
#     err "Directory $GB_DOTGB already exists. Exiting."
# fi

hello

mkdir_or_exit "${GB_DOTGB}"
mkdir_or_exit "${GB_GAMBIT_BIN_DIR}"
mkdir_or_exit "${GB_GAMBIT_SOURCES_DIR}"
mkdir_or_exit "${GB_GAMBIT_VERSIONS_DIR}"
mkdir_or_exit "${GB_GAMBIT_ENV_DIR}"

clone_gambit_or_exit

set_configure_options
set_make_flags

compile_and_install_gambit

create_bin_symlinks

goodbye
