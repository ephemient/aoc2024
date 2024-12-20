"""
Day 20: Race Condition
"""

from functools import partial
from itertools import takewhile
from operator import gt

SAMPLE_INPUT = """
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
"""


def _getpath(data: str) -> list[tuple[tuple[int, int], int]]:
    data = data.splitlines()
    stack = [
        ((y, x), {(y, x): 0})
        for y, line in enumerate(data)
        for x, char in enumerate(line)
        if char == "S"
    ]
    while True:
        (y, x), path = stack.pop()
        if data[y][x] == "E":
            return sorted(path.items())
        for y, x in ((y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)):
            if data[y][x] != "#" and (y, x) not in path:
                stack.append(((y, x), path | {(y, x): len(path)}))


def _solve(path: list[tuple[tuple[int, int], int]], cheats: int, time: int) -> int:
    return sum(
        (distance := abs(y2 - y1) + abs(x2 - x1)) <= cheats
        and distance + time <= abs(t2 - t1)
        for i, ((y1, x1), t1) in enumerate(path)
        for (y2, x2), t2 in takewhile(
            partial(gt, ((y1 + cheats, x1 + 1),)), path[i + 1 :]
        )
    )


def part1(data: str, time: int = 100) -> int:
    """
    >>> counts = [part1(SAMPLE_INPUT, time) for time in (2, 4, 6, 8, 10, 12, 20, 36, 38, 40, 64)]
    >>> [x - y for x, y in zip(counts, counts[1 :] + [0])]
    [14, 14, 2, 4, 2, 3, 1, 1, 1, 1, 1]
    """
    return _solve(_getpath(data), 2, time)


def part2(data: str, time: int = 100) -> int:
    """
    >>> counts = [part2(SAMPLE_INPUT, time) for time in (50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76)]
    >>> [x - y for x, y in zip(counts, counts[1 :] + [0])]
    [32, 31, 29, 39, 25, 23, 20, 19, 12, 14, 12, 22, 4, 3]
    """
    return _solve(_getpath(data), 20, time)


parts = (part1, part2)
