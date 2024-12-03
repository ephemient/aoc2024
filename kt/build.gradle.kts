plugins {
    base
    alias(libs.plugins.kotlin.multiplatform) apply false
    alias(libs.plugins.kotlin.plugin.allopen) apply false
    alias(libs.plugins.detekt)
    alias(libs.plugins.kotlinx.benchmark) apply false
}

dependencies {
    detektPlugins(libs.bundles.detekt.plugins)
}

detekt {
    source.from(allprojects.map { it.buildFile })
}
