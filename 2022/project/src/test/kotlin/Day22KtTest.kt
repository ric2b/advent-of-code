import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test

@Disabled
class Day22KtTest {
    private val testInput =
        """

        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day22.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(3, day22.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(7153, day22.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(1623178306L, day22.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(6146976244822, day22.part2(myInput))
}
