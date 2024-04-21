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

    // TODO
    None
  }

  // Implement indexing here
  def apply(i: Int): Table = tables(i)
}
