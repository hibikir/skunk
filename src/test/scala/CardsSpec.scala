import org.scalatest.FunSpec

class CardsSpec extends FunSpec{
  describe("A deck of skunk"){
    it("should have all the cards"){
      assert(Deck().size==60)//19+15+13+9+4)
    }
  }
}