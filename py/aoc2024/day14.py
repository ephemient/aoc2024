"""
Day 14: Restroom Redoubt
"""

from collections import namedtuple
from functools import partial
from math import lcm
from typing import Generator, Iterable

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


class _Robot(namedtuple("Robot1", ("p0", "v", "m"))):
    def __getitem__(self, t: int) -> int:
        return (self.p0 + t * self.v) % self.m


def _parse(
    data: str, width: int = WIDTH, height: int = HEIGHT
) -> Generator[tuple[_Robot, _Robot]]:
    for line in filter(None, data.splitlines()):
        p, v = line.split(maxsplit=1)
        p = p[p.index("=") + 1 :]
        v = v[v.index("=") + 1 :]
        yield (
            _Robot(int(p[: p.index(",")]), int(v[: v.index(",")]), width),
            _Robot(int(p[p.index(",") + 1 :]), int(v[v.index(",") + 1 :]), height),
        )


def part1(data: str, width: int = WIDTH, height: int = HEIGHT) -> int:
    """
    >>> part1(SAMPLE_INPUT, width=11, height=7)
    12
    """
    midpoint_x, midpoint_y = width // 2, height // 2
    q1, q2, q3, q4 = 0, 0, 0, 0
    for xrobot, yrobot in _parse(data, width, height):
        x, y = xrobot[100], yrobot[100]
        if x > midpoint_x and y > midpoint_y:
            q1 += 1
        if x < midpoint_x and y > midpoint_y:
            q2 += 1
        if x > midpoint_x and y < midpoint_y:
            q3 += 1
        if x < midpoint_x and y < midpoint_y:
            q4 += 1
    return q1 * q2 * q3 * q4


def _part2_key(robots: Iterable[_Robot], t: int) -> int:
    h1, h2 = 0, 0
    for robot in robots:
        p = robot[t]
        if p < robot.m // 2:
            h1 += 1
        elif p > robot.m // 2:
            h2 += 1
    return max(h1, h2)


def part2(data: str) -> int:
    xrobots = []
    yrobots = []
    for xrobot, yrobot in _parse(data):
        xrobots.append(xrobot)
        yrobots.append(yrobot)
    x = max(range(WIDTH), key=partial(_part2_key, xrobots))
    y = max(range(HEIGHT), key=partial(_part2_key, yrobots))
    return (x + (y - x) * pow(WIDTH, -1, HEIGHT) * WIDTH) % lcm(WIDTH, HEIGHT)


parts = (part1, part2)
