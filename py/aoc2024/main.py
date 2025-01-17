"""
Advent of Code 2024 - my answers in Python
"""

import os
import sys
from importlib import metadata
from pathlib import Path

from natsort import natsorted


def main():
    # pylint: disable=missing-function-docstring
    names = sys.argv[1:]
    days = metadata.entry_points().select(group="aoc2024.days")
    for entry in natsorted(days, key=lambda entry: entry.name):
        day = "".join(c for c in entry.name if c.isdigit())
        if names and entry.name.removeprefix("day") not in names:
            continue
        print(f"Day {entry.name.removeprefix('day')}")
        with (
            Path(os.environ.get("AOC2024_DATADIR") or ".")
            .joinpath(f"day{day}.txt")
            .open(encoding="utf-8") as file
        ):
            data = file.read()
        for part in entry.load():
            print(part(data))
        print()


if __name__ == "__main__":
    main()
