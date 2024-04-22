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

  def join (table1: String, c1: String, table2: String, c2: String): Option[Table] =
    (tables.filter(_.name == table1), tables.filter(_.name == table2)) match
      case (Nil, Nil) => None
      case (table1 :: _, Nil) => Some(table1)
      case (Nil, table2 :: _) => Some(table2)
      case (table1 :: _, table2 :: _) =>
        val tableData1 = table1.data
        val tableData2 = table2.data.map(row => row - c2 + (c1 -> row(c2)))
        val header = (tableData1.head ++ tableData2.head).map(_(0) -> "")

        def aux(row1: Row, acc: Tabular): Tabular = tableData2.filter(_(c1) contains row1(c1)) match
          case Nil => acc
          case row2 :: _ =>
            def merge(pairs1: List[(String, String)], pairs2: List[(String, String)]): List[(String, String)] =
              (pairs1, pairs2) match
                case (Nil, Nil) => Nil
                case (Nil, y :: ys) =>
                  if row1 contains y(0) then merge(Nil, ys) else y :: merge(Nil, ys)
                case (x :: xs, Nil) => x :: merge(xs, row2.toList)
                case (x :: xs, y :: ys) =>
                  if x(0) != y(0) then merge(pairs1, ys) else (if x(1) == y(1) then x else (x(0), x(1) + ';' + y(1))) :: merge(xs, row2.toList)
            merge(row1.toList, row2.toList).toMap :: acc
        val part1 = tableData1.foldRight(Nil)(aux)
        val part2 = tableData1.filterNot(part1.map(_(c1)) contains _(c1))
        val part3 = tableData2.filterNot(part1.map(_(c1)) contains _(c1))
        Some(Table(table1.name, (part1 ::: part2 ::: part3).map(header ++ _)))


  // Implement indexing here
  def apply(i: Int): Table = tables(i)
}
