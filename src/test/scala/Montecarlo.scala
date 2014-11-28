import org.scalatest.FunSpec

class Montecarlo extends FunSpec{

  val players = 0.to(10).map(x=>Player(x.toString,x))
  describe("Testing ideal Shoot The Moon conditions") {
    it("count how many tricks are face down per player"){
      val tricks  = 0.to(100000).par.flatMap { x =>
        val results = new Game(players.take(4)).play
        //assert(results.size == 4)
        //assert(results.exists(_.hand.size == 0))
        //assert((results.map(_.hand.size).sum - 2) % 3 == 0)
        results.map(x=>(x.tricks.size,x.hand.size))
      }

      val tMap = tricks.seq.groupBy(x=>x._1)
      tMap.mapValues(x=>(x.size,x.map(_._2).sum)).toSeq.sortBy(_._1).foreach(x=>
        println(x._1 + "\t" +x._2._1 +"\t" + x._2._2)
      )
    }
  }
}
