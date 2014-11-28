import org.scalatest.FunSpec

class Montecarlo extends FunSpec{

  val players = 0.to(10).map(x=>Player(x.toString,x))
  describe("Testing ideal Shoot The Moon conditions") {
    it("count how many tricks are face down per player"){
      val tricks  = 0.to(100000).par.flatMap { x =>
        val results = new Game(players.take(3)).play
        //assert(results.size == 4)
        //assert(results.exists(_.hand.size == 0))
        //assert((results.map(_.hand.size).sum - 2) % 3 == 0)
        results.map(_.tricks.size)
      }

      val tMap = tricks.seq.groupBy(x=>x)
      println(tMap.mapValues(x=>x.size).toSeq.sortBy(_._1))
    }
  }
}
