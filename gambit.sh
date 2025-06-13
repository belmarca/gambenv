#!/bin/sh

# gambit.sh - Gambit Version Manager

# This script must be sourced, not executed directly

# Try to detect if we're sourced by checking if we can return
# If we can return without error, we're being sourced
# If we can't return, we're being executed
(return 0 2>/dev/null) || {
    echo "Error: This script must be sourced, not executed directly." >&2
    echo "Usage: . /path/to/gambit.sh" >&2
    echo "   or: source /path/to/gambit.sh" >&2
    exit 1
}

# Configuration
GAMBIT_ENV_DIR="${GAMBIT_ENV_DIR:-$HOME/.gambit_env}"
BIN_DIR="$GAMBIT_ENV_DIR/bin"
ENV_DIR="$GAMBIT_ENV_DIR/env"
SOURCES_DIR="$GAMBIT_ENV_DIR/sources"
VERSIONS_DIR="$GAMBIT_ENV_DIR/versions"
LOGS_DIR="$GAMBIT_ENV_DIR/logs"

# Helper functions
_gambit_log() {
    printf "\033[33m%s\033[0m\n" "$1" >&2
}

_gambit_error() {
    printf "\033[91mError: %s\033[0m\n" "$1" >&2
}

_gambit_yes_no() {
    local prompt="$1"
    printf "%s (y/n): " "$prompt" >&2
    read -r yn || return 1  # Handle Ctrl-C/Ctrl-D immediately
    case $yn in
        [Yy]|[Yy][Ee][Ss])
            return 0
            ;;
        [Nn]|[Nn][Oo])
            return 1
            ;;
        *)
            echo "Please answer yes or no." >&2
            return 1  # Treat invalid input as "no"
            ;;
    esac
}

# Get available versions
_gambit_get_available_versions() {
    if [ -d "$VERSIONS_DIR" ]; then
        ls -1 "$VERSIONS_DIR" 2>/dev/null || true
    fi
}

# Check if version exists
_gambit_version_exists() {
    local version="$1"
    [ -d "$VERSIONS_DIR/$version" ]
}

# TODO: Have a set of allowed aliases (e.g. can't have 'master' or 'main')
_gambit_allowed_alias() {
    return 0
}

# Create environment script for a version
_gambit_create_env_script() {
    local version="$1"
    local script_path="$ENV_DIR/$version"
    local version_path="$VERSIONS_DIR/$version"

    # Check if the version path actually exists
    if [ ! -d "$version_path" ]; then
        _gambit_error "Version path $version_path does not exist."
        return 1
    fi

    cat > "$script_path" << EOF
#!/bin/sh
# Gambit environment script for version: $version

GB_OLD_PS1="\$PS1"
GB_OLD_PATH="\$PATH"
GB_OLD_LD_LIBRARY_PATH="\$LD_LIBRARY_PATH"

deactivate () {
    if [ -n "\$GB_OLD_PATH" ]; then
        PATH="\$GB_OLD_PATH"
        export PATH
        unset GB_OLD_PATH
    fi

    if [ -n "\$GB_OLD_LD_LIBRARY_PATH" ]; then
        LD_LIBRARY_PATH="\$GB_OLD_LD_LIBRARY_PATH"
        export LD_LIBRARY_PATH
        unset GB_OLD_LD_LIBRARY_PATH
    fi

    if [ -n "\$GB_OLD_PS1" ]; then
        PS1="\$GB_OLD_PS1"
        export PS1
        unset GB_OLD_PS1
    fi
}

export PS1="(🩓 $version) \$PS1"
export PATH="$version_path/bin:\$PATH"
export LD_LIBRARY_PATH="$version_path/lib:\$LD_LIBRARY_PATH"

EOF

    chmod +x "$script_path"
}

# Cleanup function for failed builds
_gambit_cleanup_failed_build() {
    local alias_name="$1"
    local source_dir="$SOURCES_DIR/$alias_name"
    local install_dir="$VERSIONS_DIR/$alias_name"
    local gsi_link="$BIN_DIR/gsi@$alias_name"
    local gsc_link="$BIN_DIR/gsc@$alias_name"
    local env_script="$ENV_DIR/$alias_name"

    _gambit_log "Cleaning up failed build..."

    [ -d "$install_dir" ] && rm -rf "$install_dir"
    [ -d "$source_dir" ] && rm -rf "$source_dir"
    [ -L "$gsi_link" ] && rm -f "$gsi_link"
    [ -L "$gsc_link" ] && rm -f "$gsc_link"
    [ -f "$env_script" ] && rm -f "$env_script"
}

# Check if string contains substring
_gambit_contains() {
    case "$1" in
        *"$2"*) return 0;;
        *) return 1;;
    esac
}

# Get number of processors
# TODO: Might want to parameterize this
_gambit_get_nproc() {
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

# Build and install Gambit version
_gambit_build_gambit_version() {
    local alias_name="$1"
    local hash_or_tag="$2"
    shift 2  # Remove first two arguments
    local configure_options="$*"  # All remaining arguments as configure options

    # _gambit_log "Debug: alias_name='$alias_name', hash_or_tag='$hash_or_tag', configure_options='$configure_options'"

    if ! _gambit_allowed_alias "$alias_name"; then
        _gambit_error "Version alias '$alias_name' is not allowed"
        return 1
    fi

    if _gambit_version_exists "$alias_name"; then
        _gambit_error "Version '$alias_name' already exists"
        return 1
    fi

    local source_dir="$SOURCES_DIR/$alias_name"
    local install_dir="$VERSIONS_DIR/$alias_name"
    local userlib_dir="$install_dir/userlib"

    # Create logs directory if it doesn't exist
    mkdir -p "$LOGS_DIR"

    local timestamp=$(date '+%Y%m%d_%H%M%S')
    local build_log="$LOGS_DIR/build_${alias_name}_${timestamp}.log"
    local install_log="$LOGS_DIR/install_${alias_name}_${timestamp}.log"

    # _gambit_log "About to prompt user for confirmation..."
    if ! _gambit_yes_no "You are about to build Gambit '$hash_or_tag' aliased to '$alias_name' with configure options '$configure_options'. Do you wish to proceed?"; then
        _gambit_log "User chose not to proceed. Aborting."
        return 0
    fi
    # _gambit_log "User confirmed. Proceeding with build..."

    _gambit_log "Copying Gambit sources..."
    if [ ! -d "$SOURCES_DIR/gambit" ]; then
        _gambit_error "Base Gambit sources not found at $SOURCES_DIR/gambit"
        return 1
    fi

    if ! cp -r "$SOURCES_DIR/gambit" "$source_dir"; then
        _gambit_error "Failed to copy Gambit sources"
        return 1
    fi

    # Run the build process in a subshell to avoid changing parent directory
    (
        if ! cd "$source_dir"; then
            _gambit_cleanup_failed_build "$alias_name"
            _gambit_error "Failed to change to source directory"
            return 1
        fi

        _gambit_log "Checking out $hash_or_tag..."
        if ! git rev-parse "$hash_or_tag" >/dev/null 2>&1; then
            _gambit_cleanup_failed_build "$alias_name"
            _gambit_error "Hash or tag '$hash_or_tag' does not exist"
            return 1
        fi

        if ! git checkout "$hash_or_tag"; then
            _gambit_cleanup_failed_build "$alias_name"
            _gambit_error "Failed to checkout $hash_or_tag"
            return 1
        fi

        # Create install directories
        if ! mkdir -p "$install_dir" "$userlib_dir"; then
            _gambit_cleanup_failed_build "$alias_name"
            _gambit_error "Failed to create install directories"
            return 1
        fi

        # Handle rpath for shared builds
        rpath_flag=""
        if _gambit_contains "$configure_options" "--enable-shared"; then
            rpath_flag="--enable-ldflags-gambuild=-Wl,-rpath,$install_dir/lib LDFLAGS=-Wl,-rpath,$install_dir/lib"
        fi

        _gambit_log "Configuring..."
        local configure_cmd="./configure --prefix=\"$install_dir\" --enable-default-runtime-options=\"~~userlib=$userlib_dir\" $configure_options $rpath_flag"

        if ! eval "$configure_cmd"; then
            _gambit_cleanup_failed_build "$alias_name"
            _gambit_error "Configuration failed"
            return 1
        fi

        _gambit_log "Compiling (this may take a while)..."
        _gambit_log "Build output will be logged to: $build_log"
        if ! make -j"$(_gambit_get_nproc)" </dev/null >"$build_log" 2>&1; then
            _gambit_cleanup_failed_build "$alias_name"
            _gambit_error "Compilation failed. Check _gambit_log file: $build_log"
            return 1
        fi

        _gambit_log "Installing..."
        _gambit_log "Install output will be logged to: $install_log"
        if ! make install </dev/null >"$install_log" 2>&1; then
            _gambit_log "Warning: Installation step failed, possibly due to documentation generation issues."
            _gambit_log "Check install _gambit_log file: $install_log"
            if ! _gambit_yes_no "Do you want to continue anyway?"; then
                _gambit_cleanup_failed_build "$alias_name"
                _gambit_log "Build aborted by user after installation failure"
                return 1
            fi
        fi

        # Verify essential binaries exist
        if [ ! -f "$install_dir/bin/gsi" ] || [ ! -f "$install_dir/bin/gsc" ]; then
            _gambit_cleanup_failed_build "$alias_name"
            _gambit_error "Essential Gambit binaries (gsi/gsc) were not properly installed"
            return 1
        fi
    ) || return 1  # Propagate subshell exit code

    # Create versioned symlinks (outside of subshell since these don't change directories)
    if ! ln -sf "$install_dir/bin/gsi" "$BIN_DIR/gsi@$alias_name"; then
        _gambit_cleanup_failed_build "$alias_name"
        _gambit_error "Failed to create gsi symlink"
        return 1
    fi

    if ! ln -sf "$install_dir/bin/gsc" "$BIN_DIR/gsc@$alias_name"; then
        _gambit_cleanup_failed_build "$alias_name"
        _gambit_error "Failed to create gsc symlink"
        return 1
    fi

    # Create gambext symlink if it exists
    if [ -f "$GAMBIT_ENV_DIR/gambext.scm" ]; then
        if ! ln -sf "$GAMBIT_ENV_DIR/gambext.scm" "$install_dir/lib/gambext"; then
            _gambit_log "Warning: Failed to create gambext symlink"
        fi
    fi

    # Create environment script
    if ! _gambit_create_env_script "$alias_name"; then
        _gambit_cleanup_failed_build "$alias_name"
        _gambit_error "Failed to create environment script"
        return 1
    fi

    _gambit_log "Gambit version '$alias_name' has been installed successfully."
    _gambit_log "Build log: $build_log"
    _gambit_log "Install log: $install_log"
}

# Remove Gambit version
_gambit_remove_gambit_version() {
    local version="$1"

    if ! _gambit_version_exists "$version"; then
        _gambit_error "Version '$version' does not exist"
        return 1
    fi

    if ! _gambit_yes_no "Do you want to remove the '$version' Gambit version?"; then
        _gambit_log "Aborting removal of version '$version'."
        return 0
    fi

    local install_dir="$VERSIONS_DIR/$version"
    local source_dir="$SOURCES_DIR/$version"
    local env_script="$ENV_DIR/$version"

    # Remove installed version
    if ! rm -rf "$install_dir"; then
        _gambit_error "Failed to remove installation directory for version '$version'"
        return 1
    fi
    _gambit_log "Removed installation directory for version '$version'"

    # Remove environment script
    if [ -f "$env_script" ] && ! rm -f "$env_script"; then
        _gambit_log "Warning: Failed to remove environment script"
    fi

    # Ask about sources
    if [ -d "$source_dir" ] && _gambit_yes_no "Do you want to remove the '$version' Gambit sources?"; then
        if ! rm -rf "$source_dir"; then
            _gambit_log "Warning: Failed to remove source directory for version '$version'"
        else
            _gambit_log "Removed source directory for version '$version'"
        fi
    fi

    # Remove symlinks
    if ! rm -f "$BIN_DIR/gsi@$version" "$BIN_DIR/gsc@$version"; then
        _gambit_log "Warning: Failed to remove some symlinks"
    fi

    _gambit_log "Removed Gambit version '$version'."
}

# List available versions
_gambit_list_versions() {
    _gambit_log "Available Gambit versions:"
    _gambit_log ""
    local versions=$(_gambit_get_available_versions)
    if [ -z "$versions" ]; then
        _gambit_log "No versions installed."
    else
        echo "$versions"
    fi
}

# Set environment for version
_gambit_set_env() {
    local version="$1"

    if ! _gambit_version_exists "$version"; then
        _gambit_log "Version '$version' is not available"
        return 1
    fi

    local script_path="$ENV_DIR/$version"
    if [ ! -f "$script_path" ]; then
        _gambit_log "Environment script not found, recreating..."
        if ! _gambit_create_env_script "$version"; then
            _gambit_log "Failed to create environment script"
            return 1
        fi
    fi

    # Source the environment script in the current shell
    . "$script_path"
}

# Handle environment management commands
_gambit_handle_env_command() {
    local subcommand="${1:-}"

    # _gambit_log "Debug: _gambit_handle_env_command called with: $*"

    case "$subcommand" in
        new)
            if [ $# -lt 3 ]; then
                _gambit_log "Usage: gambit env new ALIAS HASH_OR_TAG [CONFIGURE_OPTIONS...]"
                return 1
            fi
            shift  # Remove 'new'
            # _gambit_log "Debug: Calling _gambit_build_gambit_version with: $*"
            _gambit_build_gambit_version "$@"
            local result=$?
            # _gambit_log "Debug: _gambit_build_gambit_version returned: $result"
            return $result
            ;;
        remove)
            if [ -z "${2:-}" ]; then
                _gambit_log "Missing VERSION_ALIAS argument for 'gambit env remove'"
                return 1
            fi
            _gambit_remove_gambit_version "$2"
            ;;
        "")
            _gambit_list_versions
            ;;
        *)
            # Try to set environment for the given version
            if _gambit_version_exists "$subcommand"; then
                _gambit_set_env "$subcommand"
            else
                _gambit_log "Unknown sub-command or version: $subcommand"
                return 1
            fi
            ;;
    esac
}

# Show usage
_gambit_usage() {
    cat << 'EOF' >&2
gambit.sh - Gambit Scheme Version Manager

Available commands:

  gambit env
    Lists installed Gambit versions

  gambit env VERSION_ALIAS
    Activates the Gambit environment for VERSION_ALIAS

  gambit env new ALIAS HASH_OR_TAG [CONFIGURE_OPTIONS...]
    Builds and installs a new Gambit version

  gambit env remove VERSION_ALIAS
    Removes an installed Gambit version

  deactivate
    Deactivates the currently active Gambit environment

Setup:
  Add this to your ~/.bashrc, ~/.zshrc, or ~/.profile:
  . /path/to/gambit.sh

Examples:
  gambit env new v4.9.3 v4.9.3 --enable-shared
  gambit env new custom-build v4.9.5 --enable-single-host --enable-multiple-threaded-vms
  gambit env v4.9.3     # Activates v4.9.3 environment
  gambit env list       # Lists versions
  deactivate            # Deactivates current environment

Build logs are stored in: ~/.gambit_env/logs/
EOF
}

# Main function for the gambit command
gambit() {
    case "${1:-}" in
        env)
            shift
            _gambit_handle_env_command "$@"
            local result=$?
            return $result
            ;;
        -h|--help|help)
            _gambit_usage
            ;;
        "")
            _gambit_usage
            ;;
        *)
            _gambit_log "Unknown command: $1. Use 'gambit help' for usage information."
            return 1
            ;;
    esac
}
