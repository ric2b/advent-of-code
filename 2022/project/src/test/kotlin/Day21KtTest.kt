import com.github.h0tk3y.betterParse.combinators.*
import com.github.h0tk3y.betterParse.grammar.Grammar
import com.github.h0tk3y.betterParse.grammar.parseToEnd
import com.github.h0tk3y.betterParse.grammar.parser
import com.github.h0tk3y.betterParse.lexer.literalToken
import com.github.h0tk3y.betterParse.lexer.regexToken
import com.github.h0tk3y.betterParse.parser.Parser
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test

@Disabled
class Day21KtTest {
    private val testInput =
        """
            root: pppw + sjmn
            dbpl: 5
            cczh: sllz + lgvd
            zczc: 2
            ptdq: humn - dvpt
            dvpt: 3
            lfqf: 4
            humn: 5
            ljgn: 2
            sjmn: drzm * dbpl
            sllz: 4
            pppw: cczh / lfqf
            lgvd: ljgn * ptdq
            drzm: hmdt - zczc
            hmdt: 32
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day21.txt")!!.readText()


//    class InputParser : Grammar<List<String, (Map<String, Long>) -> Long>>() {
//        val monkeyName by regexToken("\\w+")
//        val colon by regexToken(":\\s*")
//        val space by regexToken("\\s*")
//        val number: Parser<Long> by regexToken("\\d+").use { text.toLong() }
//
//        val plus: Parser<(Long, Long) -> Long> by literalToken(" + ").map { (a, b) -> a + b }
//        val multiply: Parser<(Long, Long) -> Long> by literalToken(" + ").map { (a, b) -> a + b }
//        val plus: Parser<(Long, Long) -> Long> by literalToken(" + ").map { (a, b) -> a + b }
//        val plus: Parser<(Long, Long) -> Long> by literalToken(" + ").map { (a, b) -> a + b }
//
//        val operation: Parser<(Map<String, Long>) -> Long> by
//        val monkeyParser: Parser<List<String, () -> Long>> by
//        (monkeyName and skip(colon) and number or (monkeyName and operation and monkeyName))
//
//        override val rootParser by monkeyParser
//    }
//
//    @Test
//    fun parsing() {
//        @Test
//        fun parsing_nested() {
//            // https://github.com/silmeth/jsonParser/blob/master/src/main/kotlin/com/github/silmeth/json/SimpleJsonGrammar.kt
//
//            // Use the parser to parse a string according to the defined rules
//            assertEquals(listOf<Any>(), InputParser().parseToEnd("[]"))
//            assertEquals(listOf<Any>(listOf<Any>(), listOf<Any>()), InputParser().parseToEnd("[[], []]"))
//            assertEquals(listOf(listOf(1), 4), InputParser().parseToEnd("[[1],4]"))
//            assertEquals(listOf(1, listOf(2, listOf(3, listOf(4, listOf(5, 6, 7)))), 8, 9), InputParser().parseToEnd("[1,[2,[3,[4,[5,6,7]]]],8,9]"))
//        }
//    }

    @Test
    fun `test part 1 with example`() = assertEquals(152, day21.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(158661812617812, day21.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(301, day21.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(3352886133831, day21.part2(myInput))
}
