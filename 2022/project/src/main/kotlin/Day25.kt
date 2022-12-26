package day25

import kotlin.math.pow

fun fromSnafu(snafuNumber: String): Long {
    return snafuNumber.reversed().mapIndexed { i, c ->
        5.0.pow(i.toDouble()) * (c.digitToIntOrNull() ?: (if (c == '-') -1 else -2))
    }.sum().toLong()
}

fun toSnafuMine(number: Long): String {
    var value = number
    var out = ""
    while (value > 0) {
        val digit = value.mod(5)
        value /= 5
        when (digit) {
            in 0..2 -> out += digit.toString()
            3 -> { out += '='; value += 1 }
            4 -> { out += '-'; value += 1 }
        }
    }
    return out.reversed()
}

fun part1(input: String) = toSnafuMine(input.trimEnd().lines().sumOf(::fromSnafu))
