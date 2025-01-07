"""
Day 20: Race Condition
"""

from aoc2024.day20c import solve as _solve

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
    pos = next(
        (y, x)
        for y, line in enumerate(data)
        for x, char in enumerate(line)
        if char == "S"
    )
    path = [(pos, 0)]
    last = None
    while True:
        y, x = pos
        if data[y][x] == "E":
            return sorted(path)
        for pos2 in ((y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)):
            y, x = pos2
            if data[y][x] != "#" and pos2 != last:
                path.append((pos2, len(path)))
                last, pos = pos, pos2
                break
        else:
            return None


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
