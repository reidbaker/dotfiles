# Read by every zsh process, including non-interactive shells such as scripts
# and agent commands. Keep it small, fast, and silent.

# Disable zsh '=' command-path expansion so unquoted `==`/`===` tokens
# (e.g. `echo ===`) don't fail with "zsh: == not found".
unsetopt equals

# Machine-specific or secret overrides (not checked into git).
[[ -f "$HOME/.zshenv.local" ]] && source "$HOME/.zshenv.local"
