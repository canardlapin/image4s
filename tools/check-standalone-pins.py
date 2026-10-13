#!/usr/bin/env python3
"""Check default Ravel/Locus source pins against fetched canonical main refs.

Pass clean canonical checkouts after fetching origin/main. This command is read-only;
the JSON receipt records the exact main refs checked, not a claim of future reachability.
"""
import argparse
import json
from pathlib import Path
import re
import subprocess


def git(checkout, *arguments):
    return subprocess.check_output(["git", "-C", str(checkout), *arguments], text=True).strip()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ravel-checkout", type=Path, required=True)
    parser.add_argument("--locus4s-checkout", type=Path, required=True)
    args = parser.parse_args()
    build = (Path(__file__).resolve().parents[1] / "build.sbt").read_text()
    rows = []
    for name in ("ravel", "locus4s"):
        checkout = getattr(args, name + "_checkout").resolve()
        url = "https://github.com/canardlapin/" + name + ".git"
        if git(checkout, "remote", "get-url", "origin") != url:
            raise ValueError("Expected canonical origin " + url)
        matches = re.findall(r'lazy val ' + name + r'Revision = "([0-9a-f]{40})"', build)
        if len(matches) != 1:
            raise ValueError("Expected one immutable default revision for " + name)
        revision = matches[0]
        git(checkout, "cat-file", "-e", revision + "^{commit}")
        main_ref = git(checkout, "rev-parse", "origin/main")
        status = subprocess.run(["git", "-C", str(checkout), "merge-base", "--is-ancestor", revision, main_ref]).returncode
        if status not in (0, 1):
            raise ValueError("Git ancestry check failed for " + name)
        rows.append({"provider": name, "url": url, "revision": revision,
                     "canonical_main": main_ref, "reachable": status == 0})
    passed = all(row["reachable"] for row in rows)
    print(json.dumps({"status": "passed" if passed else "failed", "pins": rows}, indent=2))
    return 0 if passed else 1


if __name__ == "__main__":
    raise SystemExit(main())
