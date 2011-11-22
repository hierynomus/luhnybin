import java.io.{ByteArrayOutputStream, StringReader, BufferedReader}
import org.specs2.mutable.Specification
import LuhnCheck._

class LuhnCheckTest extends Specification {

  "The luhny checker should" should {
    "not mask a line with no luhn number" in {
      checkLuhn("This is a line without a number") must equalTo("This is a line without a number")
    }

    "mask a 14# luhn number" in {
      checkLuhn("56613959932537") must equalTo("XXXXXXXXXXXXXX")
    }
//
//
//    "be false for an invalid luhn number" in {
//      isLuhn("6789") must beFalse
//    }
//
//    "be true for a valid luhn number" in {
//      isLuhn("0000 0000 0000 0000") must beTrue
//    }
  }

  def checkLuhn(s: String) : String = {
    Console.setIn(new BufferedReader(new StringReader(s + "\n\n")))
    val out: ByteArrayOutputStream = new ByteArrayOutputStream()
    Console.setOut(out)

    main(Array())

    new String(out.toByteArray).trim
  }
}