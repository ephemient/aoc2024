"""
Day 6: Guard Gallivant
"""

from functools import partial
from multiprocessing import Pool
from typing import Generator, Iterable, Set, Tuple

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


def _parse(data: str) -> Tuple[Tuple[int, int], Tuple[int, int], Set[Tuple[int, int]]]:
    obstructions = []
    for y, line in enumerate(filter(None, data.splitlines())):
        for x, char in enumerate(line):
            match char:
                case "^":
                    initial_y, initial_x = y, x
                case "#":
                    obstructions.append((y, x))
    return (initial_y, initial_x), (y + 1, len(line)), frozenset(obstructions)


def _visit(
    initial_pos: Tuple[int, int],
    max_bounds: Tuple[int, int],
    obstructions: Iterable[Tuple[int, int]],
) -> Generator[Tuple[int, int], None, Set[Tuple[int, int]]]:
    y, x = initial_pos
    dy, dx = -1, 0
    max_y, max_x = max_bounds
    visited = set()
    while True:
        yield (y, x), (dy, dx)
        visited.add((y, x))
        next_y, next_x = y + dy, x + dx
        if next_y not in range(max_y) or next_x not in range(max_x):
            break
        if (next_y, next_x) in obstructions:
            dy, dx = dx, -dy
        else:
            y, x = next_y, next_x
    return visited


def _part1(
    initial_pos: Tuple[int, int],
    max_bounds: Tuple[int, int],
    obstructions: Iterable[Tuple[int, int]],
) -> Set[Tuple[int, int]]:
    visitor = _visit(initial_pos, max_bounds, obstructions)
    while True:
        try:
            next(visitor)
        except StopIteration as error:
            return error.value


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    41
    """
    initial_pos, max_bounds, obstructions = _parse(data)
    return len(_part1(initial_pos, max_bounds, obstructions))


def _part2(
    initial_pos: Tuple[int, int],
    max_bounds: Tuple[int, int],
    obstructions: Iterable[Tuple[int, int]],
) -> bool:
    visited = set()
    for key in _visit(initial_pos, max_bounds, obstructions):
        if key in visited:
            return True
        visited.add(key)
    return False


def part2(data: str, concurrency: int = None) -> int:
    """
    >>> part2(SAMPLE_INPUT)
    6
    """
    initial_pos, max_bounds, obstructions = _parse(data)
    candidates = _part1(initial_pos, max_bounds, obstructions) - {initial_pos}
    with Pool(concurrency) as pool:
        return sum(
            pool.imap_unordered(
                partial(_part2, initial_pos, max_bounds),
                (obstructions | {candidate} for candidate in candidates),
            )
        )


parts = (part1, part2)
