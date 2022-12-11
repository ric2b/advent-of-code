import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class Day11KtTest {
    private val testInput =
        """
            Monkey 0:
              Starting items: 79, 98
              Operation: new = old * 19
              Test: divisible by 23
                If true: throw to monkey 2
                If false: throw to monkey 3
            
            Monkey 1:
              Starting items: 54, 65, 75, 74
              Operation: new = old + 6
              Test: divisible by 19
                If true: throw to monkey 2
                If false: throw to monkey 0
            
            Monkey 2:
              Starting items: 79, 60, 97
              Operation: new = old * old
              Test: divisible by 13
                If true: throw to monkey 1
                If false: throw to monkey 3
            
            Monkey 3:
              Starting items: 74
              Operation: new = old + 3
              Test: divisible by 17
                If true: throw to monkey 0
                If false: throw to monkey 1
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day11.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(10605, day11.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(50172, day11.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(2713310158, day11.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(11614682178, day11.part2(myInput))
}
