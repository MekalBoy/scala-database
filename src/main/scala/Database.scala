case class Database(tables: List[Table]) {
  override def toString: String = ???

  def create(tableName: String): Database = ???

  def drop(tableName: String): Database = ???

  def selectTables(tableNames: List[String]): Option[Database] = ???

  def join(table1: String, c1: String, table2: String, c2: String): Option[Table] = ???

  // Implement indexing here
}
