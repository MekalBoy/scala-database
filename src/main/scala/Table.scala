type Row = Map[String, String]
type Tabular = List[Row]

case class Table (tableName: String, tableData: Tabular) {
  override def toString: String = {
    val header = tableData.headOption.getOrElse(Map.empty).keys.mkString(",")
    val rows = tableData.map(row => row.values.mkString(",")).mkString("\n")
    s"$header\n$rows"
  }

  def insert(row: Row): Table = {
    val newTables = if (data.contains(row)) data else data.appended(row)
    new Table(tableName, newTables)
  }

  def delete(row: Row): Table = new Table(tableName, tableData.filterNot(data => data == row))

  def sort(column: String): Table = new Table(tableName, tableData.sortBy(_.get(column)))

  def update(f: FilterCond, updates: Map[String, String]): Table = {
    val updateData = data.map(row =>
      if (filter(f).data.contains(row))
        row ++ updates
      else
        row
    )
    new Table(name, updateData)
  }

  def filter(f: FilterCond): Table = new Table(name, data.filter(row => f.eval(row).contains(true)))

  def select(columns: List[String]): Table =
    new Table(tableName, tableData.map(row => row.view.filterKeys(key => columns.contains(key)).toMap))

  def header: List[String] = tableData.headOption.map(_.keys.toList).getOrElse(List.empty)
  def data: Tabular = tableData
  def name: String = tableName
}

object Table {
  def apply(name: String, s: String): Table = {
    val lines = s.split("\n")
    val header = lines.head.split(",")
    val rows = lines.tail.map(line =>
      val values = line.split(",")
      header.zip(values).toMap
    ).toList
    new Table(name, rows)
  }
}

extension (table: Table) {
  // Implement indexing here, find the right function to override
  def apply(i: Int): Table = new Table(table.name, List(table.data(i)))
}