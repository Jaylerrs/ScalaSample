import org.scalatest.{FlatSpec, Matchers}

class MyProb extends FlatSpec with Matchers {
  it should "porb 1-10" in {
    1 + 1 shouldBe 2

    implicit class SSS(int: Int) {
      def isPrime:Boolean = true
    }
    7.isPrime shouldBe true
    import com.traition.ScalaTutorial.manager.ListManager._
    val a = List(1,2,3,4)
    last(a) shouldBe 4
    last(List(1, 1, 2, 3, 5, 8)) shouldBe 8
  }
}