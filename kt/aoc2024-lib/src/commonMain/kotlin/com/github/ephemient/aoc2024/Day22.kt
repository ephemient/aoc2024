package com.github.ephemient.aoc2024

class Day22(input: String) {
    private val seeds = input.lines().mapNotNull { it.toIntOrNull() }

    fun part1() = seeds.sumOf { secret ->
        var result = 0
        for ((i, bits) in Day22Constants.part1.withIndex()) if (1 shl i and secret != 0) result = result xor bits
        result.toLong()
    }

    fun part2(): Int {
        val acc = IntArray(19 * 19 * 19 * 19)
        var best = Int.MIN_VALUE
        for (seed in seeds) {
            var secret = seed
            val prices = intArrayOf(seed % 10, 0, 0, 0)
            val seen = BooleanArray(19 * 19 * 19 * 19)
            for (j in 1..2000) {
                val one = secret shl 6 xor secret and 16777215
                val two = one shr 5 xor one and 16777215
                secret = two shl 11 xor two and 16777215
                val price = secret % 10
                if (j >= 4) {
                    // 19 * (19 * (19 * (p0 - p1 + 9) + p1 - p2 + 9) + p2 - p3 + 9) + p3 - p4 + 9
                    val key = 6859 * prices[j % 4] - 6498 * prices[(j + 1) % 4] -
                        342 * prices[(j + 2) % 4] - 18 * prices[(j + 3) % 4] - price + 65160
                    if (!seen[key]) {
                        seen[key] = true
                        acc[key] += price
                        if (acc[key] > best) best = acc[key]
                    }
                }
                prices[j % 4] = price
            }
        }
        return best
    }
}
