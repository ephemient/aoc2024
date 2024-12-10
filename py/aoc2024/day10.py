"""
Day 10: Hoof It
"""

from operator import add, or_
from typing import Callable

SAMPLE_INPUT = """
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
"""


def _parse(data: str) -> list[set[tuple[int, int]]]:
    elevations = [set() for _ in range(10)]
    for y, line in enumerate(data.splitlines()):
        for x, char in enumerate(line):
            if "0" <= char <= "9":
                elevations[int(char)].add((y, x))
    return elevations


def _adj(point: tuple[int, int]) -> list[tuple[int, int]]:
    y, x = point
    return [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]


def _bfs[T](
    elevations: list[set[tuple[int, int]]],
    start: Callable[[tuple[int, int]], T],
    plus: Callable[[T, T], T],
):
    acc = {key: start(key) for key in elevations[0]}
    for points in elevations[1:]:
        tmp = {}
        for key, value in acc.items():
            for point in _adj(key):
                if point in points:
                    tmp[point] = plus(value, tmp[point]) if point in tmp else value
        acc = tmp
    return acc


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    36
    """
    return sum(
        len(value) for value in _bfs(_parse(data), lambda point: {point}, or_).values()
    )


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT)
    81
    """
    return sum(_bfs(_parse(data), lambda _: 1, add).values())


parts = (part1, part2)
