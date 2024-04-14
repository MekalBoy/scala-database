type Row = Map[String, String]
type Tabular = List[Row]

case class Table (tableName: String, tableData: Tabular) {

  override def toString: String = {
    val header = tableData.headOption.getOrElse(Map.empty).keys.mkString(",")
    val rows = tableData.map(row => row.values.mkString(",")).mkString("\n")
    s"$tableName\n$header\n$rows"
  }

  def insert(row: Row): Table = new Table(tableName, tableData.appended(row))

  def delete(row: Row): Table = new Table(tableName, tableData.filterNot(data => data == row))

  def sort(column: String): Table = new Table(tableName, tableData.sortBy(_.get(column)))

  def update(f: FilterCond, updates: Map[String, String]): Table = ???

  def filter(f: FilterCond): Table = ???

  def select(columns: List[String]): Table =
    new Table(tableName, tableData.map(row => row.view.filterKeys(key => columns.contains(key)).toMap))

  def header: List[String] = tableData.headOption.map(_.keys.toList).getOrElse(List.empty)
  def data: Tabular = tableData
  def name: String = tableName
}

object Table {
  def apply(name: String, s: String): Table = {
    val rows = s.split("\n").map(_.trim).map(row =>
      val pairs = row.split(",").map(_.trim.split("="))
      pairs.map(pair => pair(0) -> pair(1)).toMap
    ).toList
    new Table(name, rows)
  }
}

extension (table: Table) {
  def apply(i: Int): Table = new Table(table.name, List(table.data(i))) // Implement indexing here, find the right function to override
}

val tabel = new Table("People", List(
  Map("id" -> "1", "name" -> "John", "age" -> "23", "CNP" -> "1234567890123"),
  Map("id" -> "2", "name" -> "Jane", "age" -> "25", "CNP" -> "1234567890124"),
  Map("id" -> "3", "name" -> "Jack", "age" -> "27", "CNP" -> "1234567890125"),
  Map("id" -> "4", "name" -> "Jill", "age" -> "29", "CNP" -> "1234567890126"),
))
tabel(1)
tabel.insert(Map("id" -> "5", "name" -> "Jess", "age" -> "31", "CNP" -> "1234567890127"));
tabel.delete(Map("id" -> "4", "name" -> "Jill", "age" -> "29", "CNP" -> "1234567890126"));
tabel.sort("name")
tabel.select(List("id", "name"))