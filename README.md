# Dotfiles & Agent Tooling

Personal dotfiles, development environment configuration, and AI agent skills maintained by Reid Baker.

This repository synchronizes developer shell configurations, editor settings, git tools, and AI agent skills across machines.

---

## 🤖 Agent Infrastructure & Skills

This repository serves as the single source of truth for agent configurations and skills across development machines.

### Conceptual Layout

* **Personally Owned & Maintained Skills (`.agents/skills/`)**:
  * Encode personal workflow standards, high-rigor engineering practices, and platform-specific tooling rules authored and maintained directly in this repository.

* **Curated Third-Party Skills (`third_party/`)**:
  * Skills created and maintained in external upstream repositories that provide useful tooling and framework capabilities.
  * Organized by contributing repository (e.g. `superpowers`, `dash_skills`), with each vendor directory tracking its upstream source, license, and skills snapshot.

* **Agent Persona (`.agents/agents/reidbaker-agent/`)**:
  * Reid Baker's primary agent configuration.

### Validation & CI Enforcement

Skills are statically analyzed using the [`skills_lint`](https://pub.dev/packages/skills_lint) Dart package according to [`skills_lint.yaml`](skills_lint.yaml).

Validation and unit tests are automatically enforced on every push and pull request via GitHub Actions:
* **Workflow**: [`.github/workflows/skills_lint.yaml`](.github/workflows/skills_lint.yaml)

To run validation and tests locally:

```bash
dart run skills_lint
dart test
```

---

## 🛠 Machine Setup & Installation

### 1. Synchronize Agent Skills & Personas

Link all custom and third-party skills and personas into your global `~/.agents` directory (enabling them across all repositories on this machine):

```bash
dart run bin/sync_agents.dart
```

### 2. Shell & Configuration Setup

Symlink dotfiles into your home directory:

```bash
ln -sf ~/dotfiles/.zshrc ~/.zshrc
ln -sf ~/dotfiles/.gitconfig ~/.gitconfig
ln -sf ~/dotfiles/.bash_aliases ~/.bash_aliases
ln -sf ~/dotfiles/.vimrc ~/.vimrc
```

### 3. Machine Secrets & Authentication Setup

Secrets and private tokens are excluded from version control and should be configured per machine:

1. **GitHub Authentication**:
   * Authenticate the GitHub CLI:
     ```bash
     gh auth login
     ```
   * Or export a personal access token in `~/.zshrc.local`:
     ```bash
     export GITHUB_PERSONAL_ACCESS_TOKEN="..."
     ```

2. **Gemini API Key**:
   * Export in `~/.zshrc.local`:
     ```bash
     export GEMINI_API_KEY="..."
     ```

3. **SSH Keys**:
   * Ensure SSH keys exist in `~/.ssh/` (`id_ed25519` or `id_rsa`) and are added to GitHub.
