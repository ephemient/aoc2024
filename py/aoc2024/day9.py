"""
Day 9: Disk Defragmenter
"""

from aoc2024.day9c import part1 as _part1, part2 as _part2
from array import array

SAMPLE_INPUT = """
2333133121414131402
"""


def _rangesum(start: int, size: int) -> int:
    return (2 * start + size - 1) * size // 2


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    1928
    """
    return _part1(array("i", (int(c) for c in data if c.isdigit())))


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT)
    2858
    """
    return _part2(array("i", (int(c) for c in data if c.isdigit())))


parts = (part1, part2)
