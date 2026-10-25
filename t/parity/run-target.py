#!/usr/bin/env python3
import os
import signal
import subprocess
import sys


def write(path, payload):
    with open(path, "wb") as output:
        output.write(payload)


def main():
    if len(sys.argv) < 7:
        raise SystemExit("usage: run-target.py DIRECTORY STDOUT STDERR EXIT PROCESS COMMAND...")
    directory, stdout_path, stderr_path, exit_path, process_path = sys.argv[1:6]
    command = sys.argv[6:]
    with open(stdout_path, "wb") as stdout, open(stderr_path, "wb") as stderr:
        try:
            result = subprocess.run(command, cwd=directory, stdout=stdout, stderr=stderr)
        except OSError as error:
            write(os.path.join(directory, "target.status"), b"spawn-failure\n")
            write(process_path, f"spawn-error={error}\n".encode())
            return 0
    if result.returncode < 0:
        signal_name = signal.Signals(-result.returncode).name
        write(os.path.join(directory, "target.status"), b"run-failure\n")
        write(process_path, f"signal={signal_name}\n".encode())
    else:
        write(exit_path, f"{result.returncode}\n".encode())
        write(process_path, f"exit={result.returncode}\n".encode())
        # Parity fixtures have no expected stderr channel, even when their value is nonzero.
        if os.path.getsize(stderr_path) > 0:
            write(os.path.join(directory, "target.status"), b"run-failure\n")
        else:
            write(os.path.join(directory, "target.status"), b"exited\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
