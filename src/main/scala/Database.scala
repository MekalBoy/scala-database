case class Database(tables: List[Table]) {
  override def toString: String = tables.mkString("\n")

  def create(tableName: String): Database = {
    val alreadyThere = tables.map(_.tableName).contains(tableName)
    val newTables = if (alreadyThere) tables else new Table(tableName, List()) :: tables
    Database(newTables)
  }

  def drop(tableName: String): Database = Database(tables.filterNot(_.tableName.eq(tableName)))

  def selectTables(tableNames: List[String]): Option[Database] = ???

  def join(table1: String, c1: String, table2: String, c2: String): Option[Table] = ???

  // Implement indexing here
  def apply(i: Int): Table = tables(i)
}
