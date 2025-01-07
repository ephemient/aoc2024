"""
Day 19: Linen Layout
"""

from aoc2024.day19c import solve as _solve

SAMPLE_INPUT = """\
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
"""


def solve(data: str) -> str:
    """
    >>> print(solve(SAMPLE_INPUT))
    6
    16
    """
    keys, *targets = data.splitlines()
    keys = keys.split(", ")
    part1, part2 = _solve(keys, targets)
    return f"{part1}\n{part2}"


parts = (solve,)
