package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day23
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day23Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(23)
    }

    @Benchmark
    fun part1() = Day23(input).part1()

    @Benchmark
    fun part2() = Day23(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day23 = Day23(input)
        bh.consume(day23.part1())
        bh.consume(day23.part2())
    }
}
