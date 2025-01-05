"""
Day 22: Monkey Market
"""

from collections import deque
from ctypes import c_bool, c_int, memset
from functools import reduce

SAMPLE_INPUT_1 = """\
1
10
100
2024
"""
SAMPLE_INPUT_2 = """\
1
2
3
2024
"""


def _step(num: int) -> int:
    num = num ^ num << 6 & 16777215
    num = num ^ num >> 5 & 16777215
    num = num ^ num << 11 & 16777215
    return num


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT_1)
    37327623
    """
    return sum(
        reduce(lambda num, _: _step(num), range(2000), int(line))
        for line in data.splitlines()
    )


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT_2)
    23
    """
    output = (c_int * (19 * 19 * 19 * 19))()
    seen = (c_bool * (19 * 19 * 19 * 19))()
    best = 0
    for line in data.splitlines():
        num = int(line)
        memset(seen, False, len(seen))
        window = deque((num % 10,), 5)
        for _ in range(2001):
            if len(window) == window.maxlen:
                window.popleft()
            num = _step(num)
            window.append(num % 10)
            if len(window) == window.maxlen:
                a, b, c, d, e = window
                key = (((a + 9 - b) * 19 + b + 9 - c) * 19 + c + 9 - d) * 19 + d + 9 - e
                if not seen[key]:
                    value = output[key] + e
                    output[key] = value
                    if best < value:
                        best = value
                    seen[key] = True
    return best


parts = (part1, part2)
