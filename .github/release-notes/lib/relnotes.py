"""Shared helpers for the release-notes and security skills.

Stdlib only so the scripts run anywhere Python 3.8+ is available (CI runners,
the dev VM, Copilot agent sandboxes) without a virtualenv.
"""
import fnmatch
import json
import os
import re
import subprocess
import sys

LIB_DIR = os.path.dirname(os.path.abspath(__file__))
PRODUCTS_DIR = os.path.join(os.path.dirname(LIB_DIR), "products")


def repo_root():
    try:
        out = subprocess.run(["git", "rev-parse", "--show-toplevel"], capture_output=True,
                             text=True, check=True)
        return out.stdout.strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return os.getcwd()


def load_product(name_or_path):
    """Load a product config by name (products/<name>.json) or explicit path."""
    path = name_or_path
    if not os.path.isfile(path):
        path = os.path.join(PRODUCTS_DIR, f"{name_or_path}.json")
    if not os.path.isfile(path):
        available = sorted(f[:-5] for f in os.listdir(PRODUCTS_DIR)
                           if f.endswith(".json") and not f.startswith("_"))
        sys.exit(f"error: unknown product '{name_or_path}'. Available: {', '.join(available)}")
    with open(path, encoding="utf-8") as fh:
        return json.load(fh)


def read_at_ref(path, ref=None, root=None):
    """Return file contents at a git ref (or the working tree when ref is None).

    Returns None when the file does not exist at that ref.
    """
    root = root or repo_root()
    if ref is None:
        full = os.path.join(root, path)
        if not os.path.isfile(full):
            return None
        with open(full, encoding="utf-8", errors="replace") as fh:
            return fh.read()
    if not git_ref_exists(ref, root):
        sys.exit(f"error: git ref '{ref}' not found. Run `git fetch --tags --unshallow` "
                 f"(or `git fetch origin tag {ref}`) first.")
    proc = subprocess.run(["git", "-C", root, "show", f"{ref}:{path}"],
                          capture_output=True, text=True)
    if proc.returncode != 0:
        return None
    return proc.stdout


def git_ref_exists(ref, root=None):
    proc = subprocess.run(["git", "-C", root or repo_root(), "rev-parse", "--verify", "--quiet",
                           f"{ref}^{{commit}}"], capture_output=True, text=True)
    return proc.returncode == 0


def glob_match(path, pattern):
    """fnmatch with a leading `**/` also matching zero directories."""
    if fnmatch.fnmatch(path, pattern):
        return True
    return pattern.startswith("**/") and fnmatch.fnmatch(path, pattern[3:])


def any_glob(path, patterns):
    return any(glob_match(path, p) for p in patterns)


def write_json(data, path):
    if path in (None, "-"):
        json.dump(data, sys.stdout, indent=2)
        sys.stdout.write("\n")
        return
    os.makedirs(os.path.dirname(os.path.abspath(path)), exist_ok=True)
    with open(path, "w", encoding="utf-8") as fh:
        json.dump(data, fh, indent=2)
        fh.write("\n")


def read_json(path):
    with open(path, encoding="utf-8") as fh:
        return json.load(fh)


_VERSION_SPLIT = re.compile(r"(\d+)")


def version_key(v):
    """Loose natural-sort key for versions like 15.10.127 or 1.3.20-tuxcare-1.0.3."""
    return [(0, int(p), "") if p.isdigit() else (1, 0, p)
            for p in _VERSION_SPLIT.split(str(v)) if p]
