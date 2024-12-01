"""
Day 1: Historial Hysteria
"""

from collections import Counter

SAMPLE_INPUT = """
3   4
4   3
2   5
1   3
3   9
3   3
"""


def _parse(data: str) -> tuple[list[int], list[int]]:
    left, right = [], []
    for line in data.splitlines():
        line = line.strip()
        if not line:
            continue
        first, second = line.split()
        left.append(int(first))
        right.append(int(second))
    return left, right


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    11
    """
    left, right = _parse(data)
    return sum(abs(x - y) for (x, y) in zip(sorted(left), sorted(right)))


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT)
    31
    """
    left, right = _parse(data)
    right = Counter(right)
    return sum(x * right[x] for x in left)


parts = (part1, part2)
