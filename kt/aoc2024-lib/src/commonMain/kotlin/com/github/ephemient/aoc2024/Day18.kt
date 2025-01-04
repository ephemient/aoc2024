package com.github.ephemient.aoc2024

class Day18(input: String, private val size: Int = 70) {
    private val coords = input.lineSequence().mapNotNull { line ->
        val x = line.substringBefore(',').toIntOrNull() ?: return@mapNotNull null
        val y = line.substringAfter(',').toIntOrNull() ?: return@mapNotNull null
        x to y
    }.toList()

    fun part1(limit: Int = 1024): Int? {
        val visited = coords.subList(0, limit).toMutableSet().apply { add(0 to 0) }
        val queue = ArrayDeque<IndexedValue<IntPair>>()
        queue.add(IndexedValue(0, 0 to 0))
        while (queue.isNotEmpty()) {
            val (t, pos) = queue.removeFirst()
            val (x, y) = pos
            if (x == size && y == size) return t
            if (x > 0) (x - 1 to y).let { if (visited.add(it)) queue.addLast(IndexedValue(t + 1, it)) }
            if (y > 0) (x to y - 1).let { if (visited.add(it)) queue.addLast(IndexedValue(t + 1, it)) }
            if (y < size) (x to y + 1).let { if (visited.add(it)) queue.addLast(IndexedValue(t + 1, it)) }
            if (x < size) (x + 1 to y).let { if (visited.add(it)) queue.addLast(IndexedValue(t + 1, it)) }
        }
        return null
    }

    fun part2(): String? {
        val coords = coords.toMutableSet()
        val sets = UnionFind<IntPair>()
        for (x in 0..size) {
            for (y in 0..size) {
                val pos = x to y
                if (pos in coords) continue
                sets[pos]
                if (x < size) (x + 1 to y).takeIf { it !in coords }?.let { sets[pos] = it }
                if (y < size) (x to y + 1).takeIf { it !in coords }?.let { sets[pos] = it }
            }
        }
        val src = 0 to 0
        val dst = size to size
        val (x, y) = coords.toList().asReversed().firstOrNull { pos ->
            coords.remove(pos)
            sets[pos]
            val (x, y) = pos
            if (x > 0) (x - 1 to y).takeIf { it !in coords }?.let { sets[pos] = it }
            if (y > 0) (x to y - 1).takeIf { it !in coords }?.let { sets[pos] = it }
            if (y < size) (x to y + 1).takeIf { it !in coords }?.let { sets[pos] = it }
            if (x < size) (x + 1 to y).takeIf { it !in coords }?.let { sets[pos] = it }
            sets[src] == sets[dst]
        } ?: return null
        return "$x,$y"
    }

    private class UnionFind<T> {
        private val sets = mutableMapOf<T, T>()

        operator fun get(key: T): T {
            var key = key
            var value = sets.getOrPut(key) { key }
            while (key != value) {
                val next = sets.getOrPut(value) { value }
                sets[key] = next
                key = value
                value = next
            }
            return value
        }

        operator fun set(key: T, value: T) {
            sets[get(key)] = get(value)
        }
    }
}
