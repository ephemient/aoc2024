package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day10
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day10Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(10)
    }

    @Benchmark
    fun part1() = Day10(input).part1()

    @Benchmark
    fun part2() = Day10(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day10 = Day10(input)
        bh.consume(day10.part1())
        bh.consume(day10.part2())
    }
}
