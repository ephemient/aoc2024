import io.gitlab.arturbosch.detekt.Detekt

plugins {
    alias(libs.plugins.kotlin.multiplatform)
    alias(libs.plugins.detekt)
}

kotlin {
    jvm {
        mainRun {
            mainClass = "com.github.ephemient.aoc2024.exe.Main"
        }
    }

    sourceSets {
        commonMain {
            dependencies {
                implementation(projects.aoc2024Lib)
                implementation(libs.kotlinx.coroutines)
            }
        }
    }
}

dependencies {
    detektPlugins(libs.bundles.detekt.plugins)
}
