"""
Day 8: Resonant Collinearity
"""

from collections import defaultdict
from collections.abc import Iterable
from itertools import count

SAMPLE_INPUT = """
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"""


class _Count(Iterable[int]):
    def __iter__(self):
        return count()


def _solve(data: str, multiples: Iterable[int]) -> int:
    lines = data.strip("\n").splitlines()
    height = len(lines)
    width = height and max(map(len, lines))
    antennae = defaultdict(list)
    for y, line in enumerate(lines):
        for x, char in enumerate(line):
            if char != ".":
                antennae[char].append((y, x))
    antinodes = set()
    for points in antennae.values():
        for y0, x0 in points:
            for y1, x1 in points:
                if y0 == y1 and x0 == x1:
                    continue
                dy, dx = y1 - y0, x1 - x0
                for i in multiples:
                    y2, x2 = y1 + i * dy, x1 + i * dx
                    if y2 not in range(height) or x2 not in range(width):
                        break
                    antinodes.add((y2, x2))
    return len(antinodes)


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    14
    """
    return _solve(data, (1,))


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT)
    34
    """
    return _solve(data, _Count())


parts = (part1, part2)
