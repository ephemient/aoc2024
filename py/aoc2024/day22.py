"""
Day 22: Monkey Market
"""

from array import array

from aoc2024.day22c import part1 as _part1, part2 as _part2

SAMPLE_INPUT_1 = """\
1
10
100
2024
"""
SAMPLE_INPUT_2 = """\
1
2
3
2024
"""


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT_1)
    37327623
    """
    return _part1(array("I", (int(line) for line in data.splitlines())))


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT_2)
    23
    """
    return _part2(array("I", (int(line) for line in data.splitlines())))


parts = (part1, part2)
