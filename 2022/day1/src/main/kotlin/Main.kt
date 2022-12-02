import java.io.File

fun main(args: Array<String>) {
    // Try adding program arguments via Run/Debug configuration.
    // Learn more about running applications: https://www.jetbrains.com/help/idea/running-applications.html.
    println("Program arguments: ${args.joinToString()}")

    val day1Input = File("src/main/resources/inputs/day01.txt").readText()

    println(part1(day1Input))
    println(part2(day1Input))
}