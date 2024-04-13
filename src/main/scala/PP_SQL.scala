import scala.language.implicitConversions

trait PP_SQL_DB{
  def eval: Option[Database]
}

case class CreateTable(database: Database, tableName: String) extends PP_SQL_DB{
  def eval: Option[Database] = ???
}

case class DropTable(database: Database, tableName: String) extends PP_SQL_DB{
  def eval: Option[Database] = ???
}

implicit def PP_SQL_DB_Create_Drop(t: (Option[Database], String, String)): Option[PP_SQL_DB] = ???

case class SelectTables(database: Database, tableNames: List[String]) extends PP_SQL_DB{
  def eval: Option[Database] = ???
}

implicit def PP_SQL_DB_Select(t: (Option[Database], String, List[String])): Option[PP_SQL_DB] = ???

case class JoinTables(database: Database, table1: String, column1: String, table2: String, column2: String) extends PP_SQL_DB{
  def eval: Option[Database] = ???
}

implicit def PP_SQL_DB_Join(t: (Option[Database], String, String, String, String, String)): Option[PP_SQL_DB] = ???

trait PP_SQL_Table{
  def eval: Option[Table]
}

case class InsertRow(table:Table, values: Tabular) extends PP_SQL_Table{
  def eval: Option[Table] = ???
}

implicit def PP_SQL_Table_Insert(t: (Option[Table], String, Tabular)): Option[PP_SQL_Table] = ???

case class UpdateRow(table: Table, condition: FilterCond, updates: Map[String, String]) extends PP_SQL_Table{
  def eval: Option[Table] = ???
}

implicit def PP_SQL_Table_Update(t: (Option[Table], String, FilterCond, Map[String, String])): Option[PP_SQL_Table] = ???

case class SortTable(table: Table, column: String) extends PP_SQL_Table{
  def eval: Option[Table] = ???
}

implicit def PP_SQL_Table_Sort(t: (Option[Table], String, String)): Option[PP_SQL_Table] = ???

case class DeleteRow(table: Table, row: Row) extends PP_SQL_Table{
  def eval: Option[Table] = ???
}

implicit def PP_SQL_Table_Delete(t: (Option[Table], String, Row)): Option[PP_SQL_Table] = ???

case class FilterRows(table: Table, condition: FilterCond) extends PP_SQL_Table{
  def eval: Option[Table] = ???
}

implicit def PP_SQL_Table_Filter(t: (Option[Table], String, FilterCond)): Option[PP_SQL_Table] = ???

case class SelectColumns(table: Table, columns: List[String]) extends PP_SQL_Table{
  def eval: Option[Table] = ???
}

implicit def PP_SQL_Table_Select(t: (Option[Table], String, List[String])): Option[PP_SQL_Table] = ???

def queryT(p: Option[PP_SQL_Table]): Option[Table] = ???
def queryDB(p: Option[PP_SQL_DB]): Option[Database] = ???