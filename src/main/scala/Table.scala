type Row = Map[String, String]
type Tabular = List[Row]

case class Table (tableName: String, tableData: Tabular) {

  override def toString: String = ???

  def insert(row: Row): Table = ???

  def delete(row: Row): Table = ???

  def sort(column: String): Table = ???

  def update(f: FilterCond, updates: Map[String, String]): Table = ???

  def filter(f: FilterCond): Table = ???

  def select(columns: List[String]): Table = ???

  def header: List[String] = ???
  def data: Tabular = ???
  def name: String = ???
}

object Table {
  def apply(name: String, s: String): Table = ???
}

extension (table: Table) {
  def todo(i: Int): Table = ??? // Implement indexing here, find the right function to override
}
