import scala.annotation.tailrec

object test_obj {
  def main(args: Array[String]) {
    dikstra(sampleGraph).map(println)
    val line = new readGraphFile("file.txt").readFile
    println(line)
  }

  case class Graph(es: Map[Vertex, Seq[Edge]])
  case class Vertex(name: Int)
  case class Edge(v: Vertex, weight: Int, from: Option[Edge] = None) extends Ordered[Edge] {
    def compare(e: Edge): Int = e.weight - weight
  }
  type Path = Map[Vertex, Edge]

  def sampleGraph: Graph = Graph(
    Map(
      Vertex(3) -> Seq(Edge(Vertex(1), 2), Edge(Vertex(2), 1)),
      Vertex(1) -> Seq(Edge(Vertex(5), 2), Edge(Vertex(4), 1), Edge(Vertex(6), 1)),
      Vertex(2) -> Seq(Edge(Vertex(6), 6))
    )
  )

  def dikstra(g: Graph): Seq[Vertex] = {
    val start = Vertex(3)
    val goal = Vertex(6)
    val path = dikstra(g, start, goal)
    path.get(goal).map(shortestPath(_)).getOrElse(Nil) // startからgoalまでの最短距離を取得
  }
  @tailrec
  def shortestPath(e: Edge, route: Seq[Vertex] = Nil): Seq[Vertex] = e.from match {
    case Some(ef) => shortestPath(ef, e.v +: route)
    case None => e.v +: route
  }

  def dikstra(g: Graph, start: Vertex, goal: Vertex): Path = {
    //初期化
    val q = scala.collection.mutable.PriorityQueue[Edge]() //優先度付きキューQ
    q += Edge(start, 0)

    @tailrec
    def search(path: Path = Map()): Path = {
      if (q.isEmpty) path
      else {
        //QからQ(u)が最小である頂点vを始点とする辺edgeが取り出される
        val edge = q.dequeue()
        //vに接続している各辺
        g.es.get(edge.v).map { _.foreach { e =>
          if (!path.contains(e.v)) q += Edge(e.v, edge.weight + e.weight, Some(edge)) // 遷移先を登録
        }}
        val ne = path.get(edge.v).map(e => if (edge.weight < e.weight) edge else e).getOrElse(edge) // 訪問済みなら小さいほう優先
        search(path + (edge.v -> ne))
      }
    }
    search(Map(start -> Edge(start, 0)))
  }

}
