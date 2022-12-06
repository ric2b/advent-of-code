package day06

fun part1(input: String): Int {
    val characters = input.trimEnd().toList()

    for (i in 3 until characters.size) {
        if (characters.slice(i - 3.. i).toSet().size == 4) {
            return i + 1
        }
    }

    throw IllegalArgumentException()
}

fun part2(input: String): Int {
    val characters = input.trimEnd().toList()

    for (i in 13 until characters.size) {
        if (characters.slice(i - 13.. i).toSet().size == 14) {
            return i + 1
        }
    }

    throw IllegalArgumentException()
}