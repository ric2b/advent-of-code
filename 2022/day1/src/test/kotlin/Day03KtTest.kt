import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class Day03KtTest {
    private val testInput =
        """
            vJrwpWtwJgWrhcsFMMfFFhFp
            jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
            PmmdzqPrVvPwwTWBwg
            wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
            ttgJtRGJQctTZtZT
            CrZsJsPPZsGzwwsLwLmpwMDw
        """.trimIndent()
    private val myInput = this::class.java.classLoader.getResource("day03.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(157, day03.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(8243, day03.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(70, day03.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(2631, day03.part2(myInput))
}