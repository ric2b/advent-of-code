package day11

data class Monkey(val initialItems: List<Long>, val operation: (Long) -> Long, val divisor: Long, val whenTrue: Int, val whenFalse: Int) {
    companion object {
        //Monkey 0:
        fun fromString(rawMonkey: String) : Monkey {
            val lines = rawMonkey.lines()
            val items = lines[1].removePrefix("  Starting items: ").split(", ").map { it.toLong() }
            val operation = { old: Long ->
                val rawOperation = lines[2].removePrefix("  Operation: new = old ")
                val factor = rawOperation.drop(2).toLongOrNull() ?: old
                when (rawOperation[0]) {
                    '*' -> old * factor
                    '+' -> old + factor
                    else -> throw IllegalArgumentException("Unknown operation: ${rawOperation[0]}")
                }
            }
            val divisor = lines[3].removePrefix("  Test: divisible by ").toLong()
            val whenTrue = lines[4].removePrefix("    If true: throw to monkey ").toInt()
            val whenFalse = lines[5].removePrefix("    If false: throw to monkey ").toInt()
            return Monkey(items, operation, divisor, whenTrue, whenFalse)
        }
    }
}

fun part1(input: String): Long {
    val monkeys: List<Monkey> = input.trimEnd().split("\n\n").map { Monkey.fromString(it) }

    val monkeyItems: List<MutableList<Long>> = monkeys.map { it.initialItems.toMutableList() }
    val monkeyInspectCount: MutableList<Long> = MutableList(monkeys.size) { 0 }

    repeat(20) {
        monkeys.forEachIndexed { i, monkey ->
            val items = monkeyItems[i]
            while (items.isNotEmpty()) {
                val newWorryLevel = monkey.operation(items.removeFirst()) / 3
                val passTo = if (newWorryLevel % monkey.divisor == 0L) monkey.whenTrue else monkey.whenFalse
                monkeyItems[passTo].add(newWorryLevel)
                monkeyInspectCount[i] += 1L
            }
        }
    }

    val monkeyBusiness = monkeyInspectCount.sortedDescending().take(2).reduce { a, b -> a * b }
    return monkeyBusiness
}

fun part2(input: String): Long {
    val monkeys: List<Monkey> = input.trimEnd().split("\n\n").map { Monkey.fromString(it) }

    val monkeyItems: List<MutableList<Long>> = monkeys.map { it.initialItems.toMutableList() }
    val monkeyInspectCount: MutableList<Long> = MutableList(monkeys.size) { 0 }
    val monkeyDivisors = monkeys.map { it.divisor }.reduce { a, b -> a * b }

    repeat(10000) {
        monkeys.forEachIndexed { i, monkey ->
            val items = monkeyItems[i]
            while (items.isNotEmpty()) {
                val newWorryLevel = monkey.operation(items.removeFirst()) % monkeyDivisors
                val passTo = if (newWorryLevel % monkey.divisor == 0L) monkey.whenTrue else monkey.whenFalse
                monkeyItems[passTo].add(newWorryLevel)
                monkeyInspectCount[i] += 1L
            }
        }
    }

    val monkeyBusiness = monkeyInspectCount.sortedDescending().take(2).reduce { a, b -> a * b }
    return monkeyBusiness
}