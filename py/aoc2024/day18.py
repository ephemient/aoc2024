"""
Day 18: RAM Run
"""

from collections import deque
from typing import Iterable

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


def _parse(data: str) -> list[tuple[int, int]]:
    return [
        (int(line[: (i := line.index(","))]), int(line[i + 1 :]))
        for line in data.splitlines()
        if "," in line
    ]


def findpath(obstacles: Iterable[tuple[int, int]], size: int) -> list[tuple[int, int]]:
    visited, queue = set(obstacles), deque(([(0, 0)],))
    while queue:
        path = queue.popleft()
        x, y = pos = path[-1]
        if x == size and y == size:
            return path
        if pos in visited:
            continue
        visited.add(pos)
        for pos in ((x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)):
            x, y = pos
            if 0 <= x <= size and 0 <= y <= size:
                queue.append(path + [(x, y)])
    return None


def part1(data: str, size: int = 70, n: int = 1024) -> int:
    """
    >>> part1(SAMPLE_INPUT, 6, 12)
    22
    """
    return len(findpath(_parse(data)[:n], size)) - 1


def part2(data: str, size: int = 70) -> str:
    """
    >>> part2(SAMPLE_INPUT, 6)
    '6,1'
    """
    obstacles, i = _parse(data), 0
    while True:
        path = findpath(obstacles[: i + 1], size)
        if path is None:
            x, y = obstacles[i]
            return f"{x},{y}"
        path = set(path)
        while obstacles[i] not in path:
            i += 1


parts = (part1, part2)
