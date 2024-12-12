"""
Day 12: Garden Groups
"""

from collections import Counter, defaultdict
import textwrap
from typing import Generator

SAMPLE_INPUT_1 = textwrap.dedent(
    """\
    AAAA
    BBCD
    BBCC
    EEEC
    """
)
SAMPLE_INPUT_2 = textwrap.dedent(
    """\
    OOOOO
    OXOXO
    OOOOO
    OXOXO
    OOOOO
    """
)
SAMPLE_INPUT_3 = textwrap.dedent(
    """\
    RRRRIICCFF
    RRRRIICCCF
    VVRRRCCFFF
    VVRCCCJFFF
    VVVVCJJCFE
    VVIVCCJJEE
    VVIIICJJEE
    MIIIIIJJEE
    MIIISIJEEE
    MMMISSJEEE
    """
)
SAMPLE_INPUT_4 = textwrap.dedent(
    """\
    EEEEE
    EXXXX
    EEEEE
    EXXXX
    EEEEE
    """
)
SAMPLE_INPUT_5 = textwrap.dedent(
    """\
    AAAAAA
    AAABBA
    AAABBA
    ABBAAA
    ABBAAA
    AAAAAA
    """
)


def _groups(data: str) -> Generator[set[tuple[int, int]]]:
    lines = data.splitlines()
    visited = set()
    for y, line in enumerate(lines):
        for x, char in enumerate(line):
            group = set()
            stack = [(y, x)]
            while stack:
                y2, x2 = pos = stack.pop()
                if lines[y2][x2] != char or pos in visited:
                    continue
                visited.add(pos)
                group.add(pos)
                if 0 < y2:
                    stack.append((y2 - 1, x2))
                if 0 < x2:
                    stack.append((y2, x2 - 1))
                if x2 < len(lines[y2]) - 1:
                    stack.append((y2, x2 + 1))
                if y2 < len(lines) - 1:
                    stack.append((y2 + 1, x2))
            if group:
                yield (group)


def _perimeter1(group: set[tuple[int, int]]) -> int:
    edges = Counter()
    for y, x in group:
        edges.update(
            (
                (2 * y - 1, 2 * x),
                (2 * y, 2 * x - 1),
                (2 * y, 2 * x + 1),
                (2 * y + 1, 2 * x),
            )
        )
    return sum(value == 1 for value in edges.values())


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT_1)
    140
    >>> part1(SAMPLE_INPUT_2)
    772
    >>> part1(SAMPLE_INPUT_3)
    1930
    """
    return sum(len(group) * _perimeter1(group) for group in _groups(data))


def _perimeter2(group: set[tuple[int, int]]) -> int:
    edges = defaultdict(set)
    for y, x in group:
        edges[(2 * y - 1, 2 * x)].add(0)
        edges[(2 * y, 2 * x - 1)].add(1)
        edges[(2 * y, 2 * x + 1)].add(3)
        edges[(2 * y + 1, 2 * x)].add(2)
    lines = defaultdict(set)
    for (y, x), value in edges.items():
        if len(value) == 1:
            value = next(iter(value))
            lines[x if value % 2 else y + 1].add((x + y, value))
    return sum(
        1 + sum(abs(q - p) > 2 or b != d for (q, b), (p, d) in zip(line, line[1:]))
        for line in map(sorted, lines.values())
    )


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT_1)
    80
    >>> part2(SAMPLE_INPUT_2)
    436
    >>> part2(SAMPLE_INPUT_4)
    236
    >>> part2(SAMPLE_INPUT_5)
    368
    >>> part2(SAMPLE_INPUT_3)
    1206
    """
    return sum(len(group) * _perimeter2(group) for group in _groups(data))


parts = (part1, part2)
