import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Day25KtTest {
    private val testInput =
        """
            1=-0-2
            12111
            2=0=
            21
            2=01
            111
            20012
            112
            1=-1=
            1-12
            12
            1=
            122
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day25.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals("2=-1=0", day25.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals("2==0=0===02--210---1", day25.part1(myInput))
}
