"""
Day 14: Restroom Redoubt
"""

from collections import namedtuple
from functools import partial
from itertools import zip_longest
from math import lcm
from typing import Generator

SAMPLE_INPUT = """
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
"""

WIDTH, HEIGHT = 101, 103


class _Robot(namedtuple("Robot", ("x0", "y0", "vx", "vy"))):
    def __getitem__(
        self, t: int, width: int = WIDTH, height: int = HEIGHT
    ) -> tuple[int, int]:
        return (self.x0 + t * self.vx) % width, (self.y0 + t * self.vy) % height


def _parse(data: str) -> Generator[_Robot]:
    for line in filter(None, data.splitlines()):
        p, v = line.split(maxsplit=1)
        p = p[p.index("=") + 1 :]
        v = v[v.index("=") + 1 :]
        yield _Robot(
            int(p[: p.index(",")]),
            int(p[p.index(",") + 1 :]),
            int(v[: v.index(",")]),
            int(v[v.index(",") + 1 :]),
        )


def part1(data: str, width: int = WIDTH, height: int = HEIGHT) -> int:
    """
    >>> part1(SAMPLE_INPUT, width=11, height=7)
    12
    """
    midpoint_x, midpoint_y = width // 2, height // 2
    q1, q2, q3, q4 = 0, 0, 0, 0
    for robot in _parse(data):
        x, y = robot.__getitem__(100, width=width, height=height)
        if x > midpoint_x and y > midpoint_y:
            q1 += 1
        if x < midpoint_x and y > midpoint_y:
            q2 += 1
        if x > midpoint_x and y < midpoint_y:
            q3 += 1
        if x < midpoint_x and y < midpoint_y:
            q4 += 1
    return q1 * q2 * q3 * q4


def _part2_key(robots: tuple[_Robot], t: int) -> int:
    positions = sorted((robot[t] for robot in robots))
    line, max_line = 1, 0
    for (x, y), next in zip_longest(positions, positions[1:]):
        if (x, y + 1) == next:
            line += 1
        else:
            line, max_line = 1, max(line, max_line)
    return -max_line, t


def part2(data: str) -> int:
    return min(range(lcm(WIDTH * HEIGHT)), key=partial(_part2_key, tuple(_parse(data))))


parts = (part1, part2)
