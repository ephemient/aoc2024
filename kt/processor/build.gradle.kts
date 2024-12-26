plugins {
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.detekt)
}

dependencies {
    implementation(libs.ksp)
    implementation(libs.kotlinpoet.ksp)
}
