package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day7
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day7Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(7)
    }

    @Benchmark
    fun part1() = Day7(input).part1()

    @Benchmark
    fun part2() = Day7(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day7 = Day7(input)
        bh.consume(day7.part1())
        bh.consume(day7.part2())
    }
}
