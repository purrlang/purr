import os
import shutil
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
PURRC0_DIR = REPO_ROOT / "compiler" / "purrc0"
DEFAULT_SOURCE = REPO_ROOT / "examples" / "hello.pu"


def find_compiler():
    for name in ("clang", "gcc", "cl"):
        if shutil.which(name):
            return name
    return None


def run(cmd, cwd=None):
    print("+", " ".join(cmd))
    subprocess.check_call(cmd, cwd=cwd)


def main():
    source = Path(sys.argv[1]) if len(sys.argv) > 1 else DEFAULT_SOURCE
    source = source.resolve()
    if not source.exists():
        print(f"source not found: {source}")
        return 1

    print("Building purrc0...")
    # Use dune for OCaml project
    run(["dune", "build"], cwd=str(PURRC0_DIR))

    print("Compiling Purr to C...")
    # OCaml dune project executable
    purrc0 = PURRC0_DIR / "_build" / "default" / "bin" / "main.exe"
    run([str(purrc0), str(source)])

    c_file = Path(str(source) + ".c")
    out_exe = Path(str(source) + (".exe" if os.name == "nt" else ""))

    print("Compiling C + runtime...")
    compiler = find_compiler()
    if not compiler:
        print("No C compiler found in PATH (clang, gcc, or cl).")
        return 1

    if compiler == "cl":
        run([
            "cl",
            str(c_file),
            str(PURRC0_DIR / "rt" / "purr_runtime.c"),
            f"/I{PURRC0_DIR / 'rt'}",
            f"/Fe:{out_exe}",
        ])
    else:
        run([
            compiler,
            str(c_file),
            str(PURRC0_DIR / "rt" / "purr_runtime.c"),
            f"-I{PURRC0_DIR / 'rt'}",
            "-o",
            str(out_exe),
        ])

    print(f"Built: {out_exe}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
