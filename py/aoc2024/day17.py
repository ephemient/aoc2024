"""
Day 17: Chronospatial Computer
"""

import re
from typing import Generator

SAMPLE_INPUT_1 = """
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
"""
SAMPLE_INPUT_2 = """
Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
"""

_pattern = re.compile(r"A: (\d+)|B: (\d+)|C: (\d+)|Program: ([\d,]*)")


def _parse(data: str) -> tuple[int, int, int, list[int]]:
    a, b, c, program = 0, 0, 0, []
    for match in _pattern.finditer(data):
        if capture := match.group(1):
            a = int(capture)
        elif capture := match.group(2):
            b = int(capture)
        elif capture := match.group(3):
            c = int(capture)
        else:
            program = [int(num) for num in match.group(4).split(",")]
    return a, b, c, program


def _run(a: int, b: int, c: int, program: list[int]) -> Generator[int]:
    ip = 0
    while 0 <= ip and ip + 1 < len(program):
        instruction = program[ip]
        operand = program[ip + 1]
        combo = (0, 1, 2, 3, a, b, c)[operand] if 0 <= operand < 7 else None
        match instruction:
            case 0:
                a = a >> combo
            case 1:
                b = b ^ operand
            case 2:
                b = combo & 7
            case 3:
                if a:
                    ip = operand
                    continue
            case 4:
                b = b ^ c
            case 5:
                yield combo & 7
            case 6:
                b = a >> combo
            case 7:
                c = a >> combo
            case _:
                raise NotImplementedError()
        ip += 2


def part1(data: str) -> str:
    """
    >>> part1(SAMPLE_INPUT_1)
    '4,6,3,5,6,3,5,2,1,0'
    """
    a, b, c, program = _parse(data)
    return ",".join(map(str, _run(a, b, c, program)))


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT_2)
    117440
    """
    a, b, c, program = _parse(data)
    expected = ",".join(map(str, program))
    candidates = [0]
    while candidates:
        keys = [8 * a + b for a in candidates for b in range(8)]
        candidates.clear()
        for a in keys:
            value = ",".join(map(str, _run(a, b, c, program)))
            if value == expected:
                return a
            if expected.endswith("," + value):
                candidates.append(a)
    return None


parts = (part1, part2)
