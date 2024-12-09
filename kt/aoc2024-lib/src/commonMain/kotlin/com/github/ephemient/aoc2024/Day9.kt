package com.github.ephemient.aoc2024

class Day9(input: String) {
    private val input = input.mapNotNull { it.digitToIntOrNull() }

    fun part1(): Long {
        val chunks = input.toIntArray()
        var total = 0L
        var i = 0
        var j = chunks.lastIndex
        var offset = 0
        while (i <= j) {
            if (i % 2 == 0) {
                val size = chunks[i]
                total += i / 2L * triRange(offset, size)
                offset += size
                i++
            } else if (j % 2 == 0) {
                val free = chunks[i]
                val size = chunks[j]
                total += j / 2L * triRange(offset, minOf(free, size))
                offset += minOf(free, size)
                if (free <= size) i++ else chunks[i] = free - size
                if (free >= size) j-- else chunks[j] = size - free
            } else {
                j--
            }
        }
        return total
    }

    fun part2(): Long {
        val files = IntArray((input.size + 1) / 2) { input[2 * it] }
        val frees = IntArray(input.size / 2) { input[2 * it + 1] }
        val offsets = IntArray(input.size)
        for (i in 0..offsets.size - 2) offsets[i + 1] = offsets[i] + input[i]
        var total = 0L
        outer@for (i in files.lastIndex downTo 0) {
            val size = files[i]
            for (j in 0 until i) {
                if (frees[j] >= size) {
                    total += i.toLong() * triRange(offsets[2 * j + 1], size)
                    frees[j] -= size
                    offsets[2 * j + 1] += size
                    continue@outer
                }
            }
            total += i.toLong() * triRange(offsets[2 * i], size)
        }
        return total
    }

    companion object {
        private fun triRange(offset: Int, size: Int) = (2 * offset + size - 1) * size / 2
    }
}
