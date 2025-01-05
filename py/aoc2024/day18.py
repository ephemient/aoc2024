"""
Day 18: RAM Run
"""

from collections import deque
from itertools import islice
from typing import Generator

SAMPLE_INPUT = """
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
"""


def _parse(data: str) -> Generator[tuple[int, int]]:
    for line in data.splitlines():
        if "," not in line:
            continue
        x, y = line.split(",", maxsplit=1)
        yield int(x), int(y)


def part1(data: str, size: int = 70, n: int = 1024) -> int | None:
    """
    >>> part1(SAMPLE_INPUT, 6, 12)
    22
    """
    visited, queue = set(islice(_parse(data), n)), deque((((0, 0), 0),))
    visited.add((0, 0))
    while queue:
        (x, y), _ = pos, t = queue.popleft()
        if x == size and y == size:
            return t
        for pos in ((x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)):
            x, y = pos
            if 0 <= x <= size and 0 <= y <= size and pos not in visited:
                visited.add(pos)
                queue.append((pos, t + 1))
    return None


def _root[T](sets: dict[T, T], key: T) -> T:
    value = sets.setdefault(key, key)
    while key != value:
        sets[key], _ = key, value = value, sets.setdefault(value, value)
    return value


def part2(data: str, size: int = 70) -> str | None:
    """
    >>> part2(SAMPLE_INPUT, 6)
    '6,1'
    """
    obstacles, sets = {}, {}
    for pos in _parse(data):
        if pos not in obstacles:
            obstacles[pos] = None
    for x in range(size + 1):
        for y in range(size + 1):
            pos = x, y
            if pos in obstacles:
                continue
            _root(sets, pos)
            for pos2 in ((x, y + 1), (x + 1, y)):
                x2, y2 = pos2
                if 0 <= x2 <= size and 0 <= y2 <= size and pos2 not in obstacles:
                    sets[_root(sets, pos)] = _root(sets, pos2)
    for pos in list(reversed(obstacles.keys())):
        del obstacles[pos]
        x, y = pos
        for pos2 in ((x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)):
            x2, y2 = pos2
            if 0 <= x2 <= size and 0 <= y2 <= size and pos2 not in obstacles:
                sets[_root(sets, pos)] = _root(sets, pos2)
        if _root(sets, (0, 0)) == _root(sets, (size, size)):
            return f"{x},{y}"
    return None


parts = (part1, part2)
