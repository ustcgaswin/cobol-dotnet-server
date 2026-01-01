# Contributing

## Branch Strategy

```
main
├── codegen/feature-name     (codegen team)
├── req/feature-name         (requirements team)
```

- **Codegen team**: Branch prefix `codegen/`
- **Requirements team**: Branch prefix `req/`
- Use rebases, not merge commits
- Anyone can merge to `main`

---

## Setup

```bash
# Install dependencies
uv sync

# Run database migrations
alembic upgrade head

# Start the server
python server.py
```

---

## Daily Workflow

```bash
# 1. Pull latest main
git checkout main
git pull origin main

# 2. Sync dependencies (if pyproject.toml changed)
uv sync

# 3. Apply migrations (if schema changed)
alembic upgrade head

# 4. Create your feature branch
git checkout -b codegen/your-feature   # or req/your-feature

# 5. Make changes, then commit
git add .
git commit -m "feat: your feature description"

# 6. Before pushing, rebase on latest main
git fetch origin main
git rebase origin/main

# 7. Push your branch
git push origin codegen/your-feature

# 8. Create PR on GitHub and merge to main
```

> **Important:**
> - Never rebase `main` branch directly
> - If you get conflicts during rebase (step 6), resolve them before pushing
> - After resolving conflicts: `git rebase --continue`

---

## Commit Messages

Use conventional commits:
- `feat:` new feature
- `fix:` bug fix
- `refactor:` code change (no new feature or fix)
- `docs:` documentation only
