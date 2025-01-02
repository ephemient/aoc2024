@file:JvmName("Main")

package com.github.ephemient.aoc2024.codegen

import java.nio.file.Files
import kotlin.io.path.ExperimentalPathApi
import kotlin.io.path.Path
import kotlin.io.path.deleteRecursively

@OptIn(ExperimentalPathApi::class)
fun main(argv: Array<String>) {
    val outputDir = Path(argv.single())
    outputDir.deleteRecursively()
    Files.createDirectory(outputDir)
    day21(outputDir)
    day22(outputDir)
}
