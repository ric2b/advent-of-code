package day21

fun part1(input: String): Long {
    val monkeyNumbers: MutableMap<String, Long> = mutableMapOf()
    var monkeys: Map<String, () -> Long> = mapOf()
    monkeys = input.trimEnd().lines().associate {
        val x = Regex("(\\w+): (?:(-?\\d+)|(?:(\\w+) ([+*\\-/]) (\\w+)))").findAll(it).first().groupValues.toList()
        val (name, number, a, operation, b) = x.drop(1)

        val monkeyBusiness: () -> Long = if (number != "") {
            { number.toLong().apply { monkeyNumbers[name] = this } }
        } else {
            {
                val justA = monkeyNumbers.getOrPut(a) { monkeys.getValue(a).invoke() }
                val justB = monkeyNumbers.getOrPut(b) { monkeys.getValue(b).invoke() }

                when(operation) {
                    "+" -> justA + justB
                    "-" -> justA - justB
                    "*" -> justA * justB
                    "/" -> justA / justB
                    else -> throw NotImplementedError()
                }.apply { monkeyNumbers[name] = this }
            }
        }

        name to monkeyBusiness
    }

    return monkeyNumbers.getOrPut("root") { monkeys.getValue("root").invoke() }
}

data class Complex(val real: Double, val imaginary: Double) {
    operator fun plus(other: Complex) = Complex(this.real + other.real, this.imaginary + other.imaginary)
    operator fun minus(other: Complex) = Complex(this.real - other.real, this.imaginary - other.imaginary)
    operator fun times(other: Complex) = Complex(
        this.real * other.real - this.imaginary * other.imaginary,
        this.real * other.imaginary + this.imaginary * other.real,
    )
    operator fun div(other: Complex): Complex {
        val (a, b) = this
        val (c, d) = other
        return Complex((a*c + b*d) / (c*c + d*d), (b*c - a*d) / (c*c + d*d))
    }
}

fun part2(input: String): Long {
    var monkeys: Map<String, () -> Complex> = mapOf()

    monkeys = input.trimEnd().lines().associate {
        val x = Regex("(\\w+): (?:(-?\\d+)|(?:(\\w+) ([+*\\-/]) (\\w+)))").findAll(it).first().groupValues.toList()
        val (name, number, a, operation, b) = x.drop(1)

        val monkeyBusiness: () -> Complex = when {
            name == "humn" -> { { Complex(0.0, 1.0) } }
            number != "" -> { { Complex(number.toDouble(), 0.0) } }
            else -> {
                {
                    val justA = monkeys.getValue(a).invoke()
                    val justB = monkeys.getValue(b).invoke()

                    if (name == "root") {
                        // a+bNi = c
                        // Ni = (c - a) / b
                        if (justA.imaginary != 0.0) {
                            Complex((justB.real - justA.real) / justA.imaginary, 0.0)
                        } else {
                            Complex((justA.real - justB.real) / justB.imaginary, 0.0)
                        }
                    } else {
                        when (operation) {
                            "+" -> justA + justB
                            "-" -> justA - justB
                            "*" -> justA * justB
                            "/" -> justA / justB
                            else -> throw NotImplementedError()
                        }
                    }
                }
            }
        }

        name to monkeyBusiness
    }

    return monkeys.get("root")!!.invoke().real.let(Math::round)
}
