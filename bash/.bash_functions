#!/bin/bash

function find_largest_files () {
    du -a . | sort -n -r | head -n 10
}

_lang_heading() {
    if [[ -t 1 ]]; then
        printf '\n\033[1;96m%s\033[0m\n' "$1"
    else
        printf '\n%s\n' "$1"
    fi
}

_lang_command() {
    if [[ -t 1 ]]; then
        printf '  \033[0;32m%s\033[0m\n' "$1"
    else
        printf '  %s\n' "$1"
    fi
}

_lang_status() {
    if [[ -t 1 ]]; then
        printf '  \033[2m%s\033[0m\n' "$1"
    else
        printf '  %s\n' "$1"
    fi
}

_lang_missing() {
    _lang_status "$1 is not available on PATH."
}

# Show language-specific project commands without changing the shell or project.
function lang() {
    local node_version_file node_version rust_toolchain_file rustup_version

    case "$1" in
        ""|-h|--help|help)
            printf 'Supported languages: nodejs, python, rust, haskell\n'
            printf 'Usage: lang <language>\n'
            ;;
        nodejs)
            _lang_heading "Node.js (fnm)"
            _lang_command 'fnm list                              # list installed Node versions'
            _lang_command 'fnm install <version> && fnm use <version>'
            _lang_command 'npm install && npm run <script>       # work on this project'
            _lang_command 'eval "$(fnm env)"                    # initialize fnm in this shell'
            _lang_command 'eval "$(fnm env --use-on-cd)"        # opt in to automatic version switching'

            if command -v fnm > /dev/null 2>&1; then
                _lang_status "$(fnm --version)"
            else
                _lang_missing "fnm"
            fi
            for node_version_file in .node-version .nvmrc; do
                if [[ -f "$node_version_file" ]]; then
                    printf '\n'
                    IFS= read -r node_version < "$node_version_file" || node_version=""
                    _lang_status "Project version ($node_version_file): ${node_version:-empty}"
                    break
                fi
            done
            ;;
        python)
            _lang_heading "Python (uv)"
            _lang_command 'uv tool install <tool>                # install an isolated user CLI'
            _lang_command 'uvx <tool>                            # run a tool without installing it'
            _lang_command 'uv venv                               # create a bare virtual environment'
            _lang_command 'uv python pin <version>               # record a project Python version'
            _lang_command 'uv sync                               # create or update the project environment'
            _lang_command 'uv run <command>                      # run without activating the project environment'
            _lang_command 'source .venv/bin/activate             # optional, after .venv exists'

            if command -v uv > /dev/null 2>&1; then
                _lang_status "$(uv --version)"
            else
                _lang_missing "uv"
            fi
            printf '\n'
            if [[ -f pyproject.toml ]]; then
                _lang_status "Project metadata: pyproject.toml"
            fi
            if [[ -x .venv/bin/python ]]; then
                _lang_status "Project virtual environment: $(.venv/bin/python --version 2>&1)"
            else
                _lang_status "No .venv/bin/python in the current directory."
            fi
            ;;
        rust)
            _lang_heading "Rust (rustup)"
            _lang_command 'rustup toolchain install <version>    # install a compiler toolchain'
            _lang_command 'cargo install <tool>                  # install a user-level Rust CLI'
            _lang_command 'rustup show                           # show the active toolchain'
            _lang_command 'cargo build && cargo test             # compile and test this project'
            _lang_command 'cargo run                             # build and run this project'

            if command -v rustup > /dev/null 2>&1; then
                IFS= read -r rustup_version < <(rustup --version 2>/dev/null)
                _lang_status "$rustup_version"
            else
                _lang_missing "rustup"
            fi
            if [[ -f Cargo.toml || -f rust-toolchain.toml || -f rust-toolchain ]]; then
                printf '\n'
                if [[ -f Cargo.toml ]]; then
                    _lang_status "Cargo project: Cargo.toml"
                fi
                for rust_toolchain_file in rust-toolchain.toml rust-toolchain; do
                    if [[ -f "$rust_toolchain_file" ]]; then
                        _lang_status "Project toolchain: $rust_toolchain_file"
                        break
                    fi
                done
            fi
            ;;
        haskell)
            _lang_heading "Haskell (stack)"
            _lang_command 'stack install <package>               # install a user-level executable'
            _lang_command 'stack setup                           # install the project compiler if needed'
            _lang_command 'stack build && stack test             # compile and test this project'
            _lang_command 'stack ghci                            # start a project REPL'
            _lang_command 'stack exec <command>                  # run a command in the project environment'

            if command -v stack > /dev/null 2>&1; then
                _lang_status "$(stack --version)"
            else
                _lang_missing "stack"
            fi
            if [[ -f stack.yaml ]]; then
                printf '\n'
                _lang_status "Project resolver: stack.yaml"
                eval "$(stack --bash-completion-script stack)"
            fi
            ;;
        *)
            printf 'Unsupported language: %s\n' "$1" >&2
            printf 'Supported languages: nodejs, python, rust, haskell\n' >&2
            return 2
            ;;
    esac
}
