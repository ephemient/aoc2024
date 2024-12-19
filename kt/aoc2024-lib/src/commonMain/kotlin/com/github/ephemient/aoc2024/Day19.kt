package com.github.ephemient.aoc2024

class Day19(input: String) {
    private val keys = input.substringBefore('\n').split(", ")
    private val targets = input.substringAfter('\n').trim('\n').lines()

    fun solve(): Solution {
        var part1 = 0
        var part2 = 0L
        for (target in targets) {
            val count = counts(keys, target)
            if (count != 0) {
                part1++
                part2 += count
            }
        }
        return Solution(part1, part2)
    }

    data class Solution(val part1: Int, val part2: Long) {
        override fun toString(): String = "$part1\n$part2"
    }

    companion object {
        fun counts(keys: Iterable<String>, target: String): Int {
            val counts = IntArray(target.length + 1)
            counts[0] = 1
            for (i in target.indices) {
                for (key in keys) {
                    if (i + key.length <= target.length && target.regionMatches(i, key, 0, key.length)) {
                        counts[i + key.length] += counts[i]
                    }
                }
            }
            return counts[target.length]
        }
    }
}
