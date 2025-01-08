package com.github.ephemient.aoc2024

class Day9(private val input: String) {
    private fun files(): List<File> {
        var offset = 0
        return input.mapNotNull { it.digitToIntOrNull() }.chunked(2) {
            val size = it[0]
            val free = it.getOrElse(1) { 0 }
            File(offset, size, free).also { offset += size + free }
        }
    }

    fun part1(): Long {
        val files = files()
        var end = files.size
        var total = 0L
        for ((i, file) in files.withIndex()) {
            if (i >= end) break
            var (offset, size, free) = file
            total += i * triRange(offset, size)
            offset += size
            while (i + 1 < end && free > 0) {
                val j = end - 1
                val file = files[j]
                val moved = minOf(free, file.size)
                total += j * triRange(offset, moved)
                offset += moved
                free -= moved
                file.size -= moved
                if (file.size > 0) break
                end = j
            }
        }
        return total
    }

    fun part2(): Long {
        val files = files()
        val starts = IntArray(10)
        var total = 0L
        for (i in files.indices.reversed()) {
            var (offset, size, _) = files[i]
            var j = starts[size]
            while (j < i) {
                val file = files[j]
                if (size <= file.free) {
                    offset = files[j + 1].offset - file.free
                    file.free -= size
                    break
                }
                j++
            }
            total += i * triRange(offset, size)
            while (size < starts.size) {
                if (starts[size] >= j) break
                starts[size++] = j
            }
        }
        return total
    }

    private data class File(val offset: Int, var size: Int, var free: Int)

    companion object {
        private fun triRange(offset: Int, size: Int) = (2 * offset + size - 1) * size.toLong() / 2
    }
}
