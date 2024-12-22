"""
Day 21: Keyboard Conundrum
"""

from functools import cache

SAMPLE_INPUT = """
029A
980A
179A
456A
379A
"""

_keys = {
    "0": (1, 0),
    "1": (0, 1),
    "2": (1, 1),
    "3": (2, 1),
    "4": (0, 2),
    "5": (1, 2),
    "6": (2, 2),
    "7": (0, 3),
    "8": (1, 3),
    "9": (2, 3),
    "^": (1, 0),
    "A": (2, 0),
    "<": (0, -1),
    "v": (1, -1),
    ">": (2, -1),
}


@cache
def _robot_moves(src: tuple[int, int], dst: tuple[int, int], depth: int) -> int:
    x1, y1 = src
    x2, y2 = dst
    if not depth:
        return abs(x2 - x1) + abs(y2 - y1) + 1

    def inner(this: tuple[int, int], that: tuple[int, int]) -> int:
        if this == dst:
            return _robot_moves(that, _keys["A"], depth - 1)
        x, y = this
        return min(
            _robot_moves(that, that2, depth - 1) + inner(this2, that2)
            for this2, that2 in filter(
                None,
                [
                    ((x + 1, y), _keys[">"]) if x < x2 else None,
                    ((x - 1, y), _keys["<"]) if x > x2 and (y or x != 1) else None,
                    ((x, y + 1), _keys["^"]) if y < y2 and (x or y != -1) else None,
                    ((x, y - 1), _keys["v"]) if y > y2 and (x or y != 1) else None,
                ],
            )
        )

    return inner(src, _keys["A"])


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    126384
    """
    return _solve(data, 2)


def part2(data: str) -> int:
    return _solve(data, 25)


def _solve(data: str, depth: int) -> int:
    total = 0
    for line in data.splitlines():
        num, moves, pos = 0, 0, _keys["A"]
        for char in line:
            if char.isdigit():
                num = 10 * num + int(char)
            pos2 = _keys[char]
            moves += _robot_moves(pos, pos2, depth)
            pos = pos2
        total += num * moves
    return total


parts = (part1, part2)
