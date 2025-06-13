#!/bin/bash

# simple-builder.sh - Build essential Gambit configurations

set -e

# Simple Configuration
GAMBIT_VERSION="v4.9.6"  # Use latest stable
COMPILERS="gcc14 clang18"
FLAGS="default sh"

# Configure options
declare -A CONFIGURE_OPTIONS=(
    ["default"]=""
    ["sh"]="--enable-single-host"
)

# Compiler commands
declare -A COMPILER_CMD=(
    ["gcc14"]="CC=gcc-14 CXX=g++-14"
    ["clang18"]="CC=clang-18 CXX=clang++-18"
)

# Friendly names
declare -A COMPILER_NAME=(
    ["gcc14"]="GCC 14"
    ["clang18"]="Clang 18"
)

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Logging
log() {
    echo -e "${BLUE}[$(date '+%H:%M:%S')] $1${NC}"
}

success() {
    echo -e "${GREEN}[$(date '+%H:%M:%S')] ✓ $1${NC}"
}

error() {
    echo -e "${RED}[$(date '+%H:%M:%S')] ✗ $1${NC}"
}

warn() {
    echo -e "${YELLOW}[$(date '+%H:%M:%S')] ⚠ $1${NC}"
}

info() {
    echo -e "${CYAN}[$(date '+%H:%M:%S')] $1${NC}"
}

# Initialize tracking
init_tracking() {
    mkdir -p "$GAMBIT_ENV_DIR/logs"
    local csv_file="$GAMBIT_ENV_DIR/logs/build-results.csv"

    if [ ! -f "$csv_file" ]; then
        echo "timestamp,alias,compiler,flags,status,duration_seconds,configure_time,compile_time,install_time,binary_size" > "$csv_file"
    fi
}

# Generate alias name
generate_alias() {
    local compiler="$1"
    local flags="$2"
    if [ "$flags" = "default" ]; then
        echo "${GAMBIT_VERSION}-${compiler}"
    else
        echo "${GAMBIT_VERSION}-${compiler}-${flags}"
    fi
}

# Get binary size
get_binary_size() {
    local install_dir="$GAMBIT_ENV_DIR/versions/$1"
    if [ -f "$install_dir/bin/gsi" ]; then
        stat -c%s "$install_dir/bin/gsi" 2>/dev/null || echo "0"
    else
        echo "0"
    fi
}

# Build single configuration with detailed timing
build_single() {
    local compiler="$1"
    local flags="$2"

    local alias_name=$(generate_alias "$compiler" "$flags")
    local configure_opts="${CONFIGURE_OPTIONS[$flags]}"
    local compiler_cmd="${COMPILER_CMD[$compiler]}"
    local compiler_name="${COMPILER_NAME[$compiler]}"

    info "Building $alias_name"
    echo "  Compiler: $compiler_name"
    echo "  Flags: $flags ($configure_opts)"
    echo ""

    # Check if already exists
    source "$HOME/gambit.sh"
    if gambit env 2>/dev/null | grep -q "^$alias_name\$"; then
        warn "$alias_name already exists, skipping..."
        return 0
    fi

    local start_time=$(date +%s)
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    # Set compiler environment
    export $compiler_cmd

    # Detailed build log
    local build_log="$GAMBIT_ENV_DIR/logs/build_${alias_name}.log"

    {
        echo "=== Build Log for $alias_name ==="
        echo "Started: $timestamp"
        echo "Compiler: $compiler_name ($compiler_cmd)"
        echo "Configure Options: $configure_opts"
        echo "========================================"
        echo ""

        # Track configure time
        local configure_start=$(date +%s)
        echo "=== CONFIGURE PHASE ==="

        # Build with timing phases
        local build_result=0
        echo "y" | gambit env new "$alias_name" "$GAMBIT_VERSION" $configure_opts || build_result=$?

        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        local end_timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

        echo ""
        echo "========================================"
        echo "Completed: $end_timestamp"
        echo "Total Duration: ${duration}s"
        echo "Status: $([ $build_result -eq 0 ] && echo "SUCCESS" || echo "FAILED")"

        # Show binary info if successful
        if [ $build_result -eq 0 ]; then
            echo ""
            echo "=== BINARY INFO ==="
            local install_dir="$GAMBIT_ENV_DIR/versions/$alias_name"
            if [ -f "$install_dir/bin/gsi" ]; then
                ls -lh "$install_dir/bin/gsi"
                echo "Binary size: $(stat -c%s "$install_dir/bin/gsi" 2>/dev/null || echo "unknown") bytes"
            fi
        fi

    } > "$build_log" 2>&1

    # Parse timing from detailed logs if available
    local configure_time=0
    local compile_time=0
    local install_time=0

    # Update CSV tracking
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    local status=$([ $build_result -eq 0 ] && echo "SUCCESS" || echo "FAILED")
    local binary_size=$(get_binary_size "$alias_name")

    local csv_file="$GAMBIT_ENV_DIR/logs/build-results.csv"
    echo "$timestamp,$alias_name,$compiler,$flags,$status,$duration,$configure_time,$compile_time,$install_time,$binary_size" >> "$csv_file"

    if [ $build_result -eq 0 ]; then
        success "$alias_name completed in ${duration}s"
        info "Binary size: $(numfmt --to=iec "$binary_size")B"
    else
        error "$alias_name failed after ${duration}s"
        warn "Check log: $build_log"
    fi

    return $build_result
}

# Build all configurations
build_all() {
    log "Building essential Gambit configurations..."
    log "Gambit Version: $GAMBIT_VERSION"
    echo ""

    init_tracking

    local total_builds=0
    local successful_builds=0
    local failed_builds=0
    local start_matrix_time=$(date +%s)

    # Calculate total
    for compiler in $COMPILERS; do
        for flags in $FLAGS; do
            total_builds=$((total_builds + 1))
        done
    done

    info "Building $total_builds configurations..."
    echo ""

    local build_count=0
    for compiler in $COMPILERS; do
        for flags in $FLAGS; do
            build_count=$((build_count + 1))
            log "[$build_count/$total_builds] Building $(generate_alias "$compiler" "$flags")"

            if build_single "$compiler" "$flags"; then
                successful_builds=$((successful_builds + 1))
            else
                failed_builds=$((failed_builds + 1))
            fi
            echo ""
        done
    done

    local end_matrix_time=$(date +%s)
    local total_time=$((end_matrix_time - start_matrix_time))

    echo "========================================"
    success "Build matrix complete!"
    echo "  Total builds: $total_builds"
    success "  Successful: $successful_builds"
    error "  Failed: $failed_builds"
    info "  Total time: ${total_time}s ($(numfmt --to=iec $total_time))"
    if [ $total_builds -gt 0 ]; then
        info "  Success rate: $(( successful_builds * 100 / total_builds ))%"
    fi
    echo ""

    generate_report
}

# Generate report
generate_report() {
    local csv_file="$GAMBIT_ENV_DIR/logs/build-results.csv"
    local report_file="$GAMBIT_ENV_DIR/logs/build-report.txt"

    if [ ! -f "$csv_file" ]; then
        warn "No build data found"
        return 1
    fi

    {
        echo "Gambit Build Report - Essential Configurations"
        echo "=============================================="
        echo "Generated: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
        echo "Gambit Version: $GAMBIT_VERSION"
        echo ""

        echo "Build Summary:"
        echo "-------------"
        local total=$(tail -n +2 "$csv_file" | wc -l)
        local successful=$(tail -n +2 "$csv_file" | grep -c SUCCESS 2>/dev/null || echo 0)
        local failed=$(tail -n +2 "$csv_file" | grep -c FAILED 2>/dev/null || echo 0)

        echo "Total builds: $total"
        echo "Successful: $successful"
        echo "Failed: $failed"
        if [ $total -gt 0 ]; then
            echo "Success rate: $(( successful * 100 / total ))%"
        fi
        echo ""

        if [ $successful -gt 0 ]; then
            echo "Successful Builds:"
            echo "-----------------"
            printf "%-20s %-10s %-15s %-10s %-10s\n" "ALIAS" "COMPILER" "FLAGS" "TIME(s)" "SIZE"
            printf "%-20s %-10s %-15s %-10s %-10s\n" "--------------------" "----------" "---------------" "----------" "----------"
            tail -n +2 "$csv_file" | grep SUCCESS | while IFS=',' read timestamp alias compiler flags status duration configure_time compile_time install_time binary_size; do
                local size_human=$(numfmt --to=iec "$binary_size" 2>/dev/null || echo "$binary_size")
                printf "%-20s %-10s %-15s %-10s %-10s\n" "$alias" "$compiler" "$flags" "$duration" "${size_human}B"
            done
            echo ""
        fi

        if [ $failed -gt 0 ]; then
            echo "Failed Builds:"
            echo "-------------"
            tail -n +2 "$csv_file" | grep FAILED | cut -d',' -f2,3,4 | while IFS=',' read alias compiler flags; do
                echo "  $alias ($compiler, $flags)"
            done
            echo ""
        fi

    } > "$report_file"

    log "Report saved to: $report_file"
}

# Show status
show_status() {
    source "$HOME/gambit.sh"

    echo -e "${CYAN}Gambit Essential Build Status${NC}"
    echo "============================="
    echo ""

    echo "Target configurations:"
    echo "----------------------"
    for compiler in $COMPILERS; do
        local compiler_name="${COMPILER_NAME[$compiler]}"
        for flags in $FLAGS; do
            local alias_name=$(generate_alias "$compiler" "$flags")
            local flag_desc="${CONFIGURE_OPTIONS[$flags]}"
            [ -z "$flag_desc" ] && flag_desc="(no flags)"
            echo "  $alias_name - $compiler_name with $flag_desc"
        done
    done
    echo ""

    echo "Currently installed versions:"
    echo "----------------------------"
    if command -v gambit >/dev/null 2>&1; then
        gambit env
    else
        echo "Gambit environment not yet set up"
    fi
    echo ""

    # Show build results if available
    local csv_file="$GAMBIT_ENV_DIR/logs/build-results.csv"
    if [ -f "$csv_file" ] && [ $(wc -l < "$csv_file") -gt 1 ]; then
        echo "Build Statistics:"
        echo "----------------"
        local total=$(tail -n +2 "$csv_file" | wc -l)
        local successful=$(tail -n +2 "$csv_file" | grep -c SUCCESS 2>/dev/null || echo 0)
        local failed=$(tail -n +2 "$csv_file" | grep -c FAILED 2>/dev/null || echo 0)

        echo "  Attempted: $total/4"
        echo "  Successful: $successful"
        echo "  Failed: $failed"
        echo ""
    fi

    # Show disk usage
    if [ -d "$GAMBIT_ENV_DIR" ]; then
        echo "Disk Usage:"
        echo "----------"
        if [ -d "$GAMBIT_ENV_DIR/versions" ]; then
            echo "  Versions: $(du -sh "$GAMBIT_ENV_DIR/versions" | cut -f1)"
        fi
        if [ -d "$GAMBIT_ENV_DIR/logs" ]; then
            echo "  Logs: $(du -sh "$GAMBIT_ENV_DIR/logs" | cut -f1)"
        fi
        echo ""
    fi
}

# Setup environment
setup() {
    log "Setting up Gambit build environment..."

    # Run initial setup
    if [ ! -f "$GAMBIT_ENV_DIR/config/default" ]; then
        echo "y" | ./gambenv.sh
        success "Initial environment setup complete!"
    else
        warn "Environment already set up, skipping gambenv.sh"
    fi

    success "Ready to build!"
    info "Run: ./simple-builder.sh build"
}

# Interactive shell setup
setup_shell() {
    source "$HOME/gambit.sh"
    info "Gambit environment loaded"
    info "Available versions:"
    gambit env
    echo ""
    info "Usage: gambit env VERSION_NAME"
    exec bash
}

# Main command handling
case "${1:-help}" in
    "setup")
        setup
        ;;
    "build")
        build_all
        ;;
    "status")
        show_status
        ;;
    "report")
        generate_report
        cat "$GAMBIT_ENV_DIR/logs/build-report.txt" 2>/dev/null || warn "No report available yet"
        ;;
    "shell")
        setup_shell
        ;;
    "single")
        if [ $# -ne 3 ]; then
            error "Usage: $0 single COMPILER FLAGS"
            echo "Available compilers: $COMPILERS"
            echo "Available flags: $FLAGS"
            exit 1
        fi
        init_tracking
        build_single "$2" "$3"
        ;;
    "help"|*)
        echo -e "${CYAN}Simple Gambit Builder${NC}"
        echo "Usage: $0 {setup|build|status|report|shell|single}"
        echo ""
        echo "Commands:"
        echo "  setup         - Initialize Gambit environment"
        echo "  build         - Build all 4 essential configurations"
        echo "  status        - Show current status"
        echo "  report        - Show build report"
        echo "  shell         - Open shell with Gambit environment"
        echo "  single C F    - Build single configuration"
        echo ""
        echo "Configurations to build:"
        for compiler in $COMPILERS; do
            for flags in $FLAGS; do
                local alias_name=$(generate_alias "$compiler" "$flags")
                echo "  $alias_name"
            done
        done
        ;;
esac
