#!/usr/bin/env sh

# gambdev.sh
#
# (c) 2023 - Marc-André Bélanger
#
# This shell script is only intended to bootstrap a Gambit environment.
# It should be invoked once, from this directory.


## Config

GB_VERSION=v0.0.1
GB_GAMBIT_VERSION=v4.9.5                                # Follow latest Gambit release

GB_DOTGB="${GB_DOTGB:-$HOME/.gambit}"                   # Override if you wish.
GB_GAMBIT_GIT_URL="https://github.com/gambit/gambit"    # Should stay hosted on GitHub for a while...

GB_GAMBIT_SOURCES_DIR="${GB_DOTGB}/sources"             # Don't override.
GB_GAMBIT_SOURCE_DIR="${GB_GAMBIT_SOURCES_DIR}/gambit"  # Don't override.
GB_GAMBIT_VERSIONS_DIR="${GB_DOTGB}/versions"           # Where to place Gambit installations
GB_GAMBIT_LATEST="${GB_GAMBIT_VERSIONS_DIR}/latest"     # Don't override.
GB_GAMBIT_CURRENT="${GB_GAMBIT_VERSIONS_DIR}/current"   # Don't override.
GB_GAMBIT_BIN_DIR="${GB_DOTGB}/bin"                     # WHere to place Gambit aliases e.g. gsi@4.9.4

GB_GAMBIT_GAMBEXT="${GB_DOTGB}/gambext.scm"             # Global gambext


GB_DEFAULT_CONFIGURE_OPTIONS="--enable-single-host"     # Use a common configure option
GB_DEFAULT_MAKE_FLAGS="-j1"                             # Default to single-core make

GB_GAMBIT_BUILD_CONFIG_FILE="${GB_DOTGB}/config.scm"    # The configuration file (contains an s-exp)


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
    keep_or_modify "GB_DEFAULT_CONFIGURE_OPTIONS"
}

set_make_flags() {
    log "The default make flags are:"
    echo ""
    echo "${GB_DEFAULT_MAKE_FLAGS}"
    echo ""
    keep_or_modify "GB_DEFAULT_MAKE_FLAGS"
}

hello() {
    log "gambdev ${GB_VERSION}"
    log
    log "This script will bootstrap a Gambit environment. It will:"
    log
    log "  1. Create the ${GB_DOTGB} directory and some subdirectories."
    log "  2. Clone the Gambit scheme sources from ${GB_GAMBIT_GIT_URL}."
    log "  3. Compile Gambit ${GB_GAMBIT_VERSION}."
    log
    _log "Do you want to proceed? (y/n): "
    read -r choice
    case "$choice" in
        y|Y )
        ;;
        * )
            err "Exiting.";;
    esac

}

compile_and_install_gambit() {
    # NOTE: We could compile gambit once for private use by Gambit, and then ask
    # for the user to compile the version of their liking. This is not quite
    # practical because we don't want to force the user to compile Gambit twice
    # to start using it. For now, use a single Gambit version that the user is
    # welcome to bork on their own.

    log "Configuring and compiling Gambit ${GB_GAMBIT_VERSION}..."

    # Where to put the source for compiling this version
    SOURCE_DIR="${GB_GAMBIT_SOURCES_DIR}/${GB_GAMBIT_VERSION}"
    cp -R "${GB_GAMBIT_SOURCE_DIR}" "${SOURCE_DIR}"

    # Where to install this version
    INSTALL_DIR="${GB_GAMBIT_VERSIONS_DIR}/${GB_GAMBIT_VERSION}"
    mkdir_or_exit "${INSTALL_DIR}"

    # NOTE: For the moment, we keep the default location for ~~userlib, which
    # should be in ~/.gambit_userlib. We could change it in configure like:
    # --enable-default-runtime-options=~~userlib="${GB_USERLIB_DIR}"

    cd "${SOURCE_DIR}" || err "Could not change directory."

    if git checkout "${GB_GAMBIT_VERSION}"; then
        if ./configure $GB_DEFAULT_CONFIGURE_OPTIONS --prefix="${INSTALL_DIR}"; then
            log "Successfully configured Gambit."
            if make ${GB_DEFAULT_MAKE_FLAGS}; then
                log "Successfully built Gambit."
                if make install; then
                    log "Successfully installed Gambit in ${INSTALL_DIR}."
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

    # NOTE: Should we keep the source directory around? Probably not for the
    # default latest install, but yes for dev builds. Handle this later in gsi.
    rm -rf "${SOURCE_DIR}"

    # Create version symlinks for easy access
    ln -s "${INSTALL_DIR}/bin/gsi" "${GB_GAMBIT_BIN_DIR}/gsi@${GB_GAMBIT_VERSION}"
    ln -s "${INSTALL_DIR}/bin/gsc" "${GB_GAMBIT_BIN_DIR}/gsc@${GB_GAMBIT_VERSION}"

    # Create symlink for global gambext
    ln -s "${GB_GAMBIT_GAMBEXT}" "${INSTALL_DIR}/lib/gambext"

    log "Done."
}

write_gambdev_config() {
    write() {
        echo "$@" >> ${GB_GAMBIT_BUILD_CONFIG_FILE}
    }

    log "Writing gambdev configuration to ${GB_GAMBIT_BUILD_CONFIG_FILE}..."
    touch "${GB_GAMBIT_BUILD_CONFIG_FILE}"
    write ";; generated by gambdev ${GB_VERSION}"
    write ""
    write "((gambdev-version \"${GB_VERSION}\")"
    write " (gambit-version \"${GB_GAMBIT_VERSION}\")"
    write " (configure-options \"${GB_DEFAULT_CONFIGURE_OPTIONS}\")"
    write " (make-flags \"${GB_DEFAULT_MAKE_FLAGS}\"))"
    log "Done."
}

symlink_current_and_latest() {
    # At first, current and latest are the same
    ln -s "${GB_GAMBIT_VERSIONS_DIR}/${GB_GAMBIT_VERSION}" "${GB_GAMBIT_LATEST}"
    ln -s "${GB_GAMBIT_VERSIONS_DIR}/${GB_GAMBIT_VERSION}" "${GB_GAMBIT_CURRENT}"
}

goodbye() {
    success
    success "A Gambit environment has been successfully bootstrapped!"
    success "Your configuration file lives in"
    success "${GB_GAMBIT_BUILD_CONFIG_FILE}."
    success "You can modify it to your liking."
    success "Remember to add"
    success "${GB_GAMBIT_CURRENT}/bin"
    success "and"
    success "${GB_GAMBIT_BIN_DIR}"
    success "to your PATH to start using Gambit."
}


## TODO: Platform detection

## Main

if [ -z "$GB_DOTGB" ]; then
    err "GB_DOTGB variable is not set. Exiting."
fi

if [ -d "$GB_DOTGB" ]; then
    err "Directory $GB_DOTGB already exists. Exiting."
fi

hello

mkdir_or_exit "${GB_DOTGB}"
mkdir_or_exit "${GB_GAMBIT_BIN_DIR}"
mkdir_or_exit "${GB_GAMBIT_SOURCES_DIR}"
mkdir_or_exit "${GB_GAMBIT_VERSIONS_DIR}"

cp gambext.scm "${GB_GAMBIT_GAMBEXT}"

clone_gambit_or_exit

set_configure_options
set_make_flags

compile_and_install_gambit

write_gambdev_config

symlink_current_and_latest

goodbye
