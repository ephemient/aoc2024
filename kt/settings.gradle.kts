enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")

pluginManagement {
    repositories {
        mavenCentral()
        gradlePluginPortal()
    }
}

gradle.afterProject {
    repositories {
        mavenCentral()
    }
}

rootProject.name = "aoc2024"
include("aoc2024-exe", "aoc2024-lib", "graalvm")
