package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day4
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day4Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(4)
    }

    @Benchmark
    fun part1() = Day4(input).part1()

    @Benchmark
    fun part2() = Day4(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day4 = Day4(input)
        bh.consume(day4.part1())
        bh.consume(day4.part2())
    }
}
