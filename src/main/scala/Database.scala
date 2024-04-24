case class Database(tables: List[Table]) {
  override def toString: String = tables.mkString("\n")

  def create(tableName: String): Database = {
    val alreadyThere = tables.map(_.tableName).contains(tableName)
    val newTables = if (alreadyThere) tables else tables.appended(new Table(tableName, List()))
    Database(newTables)
  }

  def drop(tableName: String): Database = Database(tables.filterNot(_.tableName.eq(tableName)))

  def selectTables(tableNames: List[String]): Option[Database] = {
    if (tables.exists(table => tableNames.contains(table.tableName)))
      Some(Database(tables.filter(table => tableNames.contains(table.tableName))))
    else
      None
  }

  def join(table1: String, c1: String, table2: String, c2: String): Option[Table] = {
    val firstTable = tables.find(_.name == table1)
    val secondTable = tables.find(_.name == table2)

    (firstTable, secondTable) match {
      case (Some(t1), Some(t2)) => {
        val td1 = t1.data
        val td2 = t2.data.map(row => row - c2 + (c1 -> row(c2)))

        def tuple(r1: Row, acc: List[(Row, Row)]): List[(Row, Row)] =
          td2.filter(_(c1) contains r1(c1)) match
            case Nil => acc
            case r2 :: _ => (r1, r2) :: acc

        def merge(r1: Row, r2: Row): Row = {
          def helper(tuple: (String, String), acc: List[(String, String)]): List[(String, String)] =
            r2.get(tuple._1) match {
              case Some(str) => if str == tuple._2 then (tuple._1, str) :: acc else (tuple._1, tuple._2 + ';' + str) :: acc
              case _ => tuple :: acc
            }

          r1.foldRight(Nil)(helper).toMap ++ r2.filterNot(r1 contains _(0))
        }

//        def replace(tuple: (String, String)): (String, String) = if tuple._1 != c2 then tuple else (c1, tuple._2)

        val header = (td1.head ++ td2.head).map(_(0) -> "")

        val merged = td1.foldRight(Nil)(tuple).map(merge)
        val first = td1.filterNot(merged.map(_(c1)) contains _(c1))
        val second = td2.filterNot(merged.map(_(c1)) contains _(c1))

        Some(Table(t1.name, (merged ::: first ::: second).map(header ++ _)))
      }
      case (Some(t1), _) => Some(t1)
      case (_, Some(t2)) => Some(t2)
      case (_, _) => None
    }
  }

  // Implement indexing here
  def apply(i: Int): Table = tables(i)
}
