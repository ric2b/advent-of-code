package day02

data class Game(val opponent: Shape, val me: Shape) {
    fun score(): Int {
        val gameScore = 3 * (((me.score() - 1) - (opponent.score() - 1) + 1) % 3)
        return me.score() + gameScore
    }
}

enum class Shape {
    Rock { override fun score() = 1 },
    Paper { override fun score() = 2 },
    Scissors { override fun score() = 3 };

    abstract fun score(): Int
}

val signMappings = mapOf(
    'A' to Shape.Rock, 'B' to Shape.Paper, 'C' to Shape.Scissors,
    'X' to Shape.Rock, 'Y' to Shape.Paper, 'Z' to Shape.Scissors
)

fun part1(input: String): Int {
//    val games = input.trimEnd().lines().map { it.toList() }.map { (opponent, _, me) ->
//        Game(signMappings.getValue(opponent), signMappings.getValue(me))
//    }

//     15388 too low
//    return games.sumOf { it.score() };

    val scores = mapOf(
        "A X" to 3 + 1,
        "A Y" to 6 + 2,
        "A Z" to 0 + 3,
        "B X" to 0 + 1,
        "B Y" to 3 + 2,
        "B Z" to 6 + 3,
        "C X" to 6 + 1,
        "C Y" to 0 + 2,
        "C Z" to 3 + 3,
    )

    return input.trimEnd().lines().sumOf { scores.getValue(it) }
}

fun part2(input: String): Int {
    val scores = mapOf(
        "A X" to 0 + 3,
        "A Y" to 3 + 1,
        "A Z" to 6 + 2,
        "B X" to 0 + 1,
        "B Y" to 3 + 2,
        "B Z" to 6 + 3,
        "C X" to 0 + 2,
        "C Y" to 3 + 3,
        "C Z" to 6 + 1,
    )

    return input.trimEnd().lines().sumOf { scores.getValue(it) }
}