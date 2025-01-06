"""
Day 6: Guard Gallivant
"""

from aoc2024.day6c import part1 as _part1, part2 as _part2

SAMPLE_INPUT = """
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"""


def _parse(data: str) -> tuple[tuple[int, int], tuple[int, int], set[tuple[int, int]]]:
    obstructions = set()
    for y, line in enumerate(filter(None, data.splitlines())):
        for x, char in enumerate(line):
            match char:
                case "^":
                    initial_y, initial_x = y, x
                case "#":
                    obstructions.add((y, x))
    return ((initial_y, initial_x), (y + 1, len(line)), obstructions)


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    41
    """
    initial_pos, max_bounds, obstructions = _parse(data)
    return _part1(initial_pos, max_bounds, obstructions)


def part2(data: str, concurrency: int = None) -> int:
    """
    >>> part2(SAMPLE_INPUT)
    6
    """
    initial_pos, max_bounds, obstructions = _parse(data)
    return _part2(
        initial_pos,
        max_bounds,
        obstructions,
    )


parts = (part1, part2)
