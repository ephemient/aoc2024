"""
Day 9: Disk Defragmenter
"""

from itertools import accumulate, zip_longest

SAMPLE_INPUT = """
2333133121414131402
"""


def _rangesum(start: int, size: int) -> int:
    return (2 * start + size - 1) * size // 2


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    1928
    """
    data = list(zip_longest(*((int(c) for c in data if c.isdigit()),) * 2, fillvalue=0))
    end, offset, total = len(data), 0, 0
    for i, (used, free) in enumerate(data):
        if i >= end:
            break
        total += i * _rangesum(offset, used)
        offset += used
        for j in range(end - 1, i, -1):
            used2, free2 = data[j]
            moved = min(free, used2)
            total += j * _rangesum(offset, moved)
            offset += moved
            free -= moved
            if not free:
                data[j] = used2 - moved, free2 + moved
                break
            end = j
        offset += free
    return total


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT)
    2858
    """
    data = list(zip_longest(*((int(c) for c in data if c.isdigit()),) * 2, fillvalue=0))
    offsets = list(accumulate(used + free for used, free in data))
    starts, total = [0 for _ in range(10)], 0
    for i in reversed(range(len(data))):
        offset = offsets[i - 1] if i else 0
        used, _ = data[i]
        for j in range(starts[used], i):
            used2, free = data[j]
            if used <= free:
                offset = offsets[j] - free
                data[j] = used2, free - used
                break
        else:
            j = i
        total += i * _rangesum(offset, used)
        for used in range(used, len(starts)):
            if starts[used] >= j:
                break
            starts[used] = j
    return total


parts = (part1, part2)
