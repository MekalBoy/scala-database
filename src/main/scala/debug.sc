// Table.scala

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
    val lines = s.split("\n").map(_.trim)
    val header = lines.head.split(",").map(_.trim)
    val rows = lines.tail.map(line =>
      val values = line.split(",").map(_.trim)
      header.zip(values).toMap
    ).toList
    new Table(name, rows)
  }
}

extension (table: Table) {
  def apply(i: Int): Table = new Table(table.name, List(table.data(i))) // Implement indexing here, find the right function to override
}

val peopleStr: String =
  """|id,name,age,address
     |1,John,23,123 Main St
     |2,Jane,27,456 Elm St
     |3,Joe,30,789 Maple St
     |4,Jill,25,101 Oak St
     |5,Jack,27,112 Pine St
     |6,Jen,24,131 Cedar St
     |7,Jim,26,141 Birch St
     |8,Jesse,29,151 Spruce St
     |9,Jenny,23,161 Fir St
     |10,Jerry,28,171 Larch St""".stripMargin

val tabelStr = Table("People", peopleStr)

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


// FilterCond.scala

import scala.language.implicitConversions

trait FilterCond {def eval(r: Row): Option[Boolean]}

case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =
    r.get(colName) match {
      case Some(str) => Some(predicate(str))
      case _ => None
    }
}

case class Compound(op: (Boolean, Boolean) => Boolean, conditions: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =
    if (!conditions.map(_.eval(r)).exists(_.isEmpty)) // all good
      Some(conditions.map(_.eval(r)).map(_.get).reduce(op))
    else // at least one cannot be eval'd
      None
}

case class Not(f: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =
    f.eval(r) match {
      case Some(value) => Some(!value)
      case _ => None
    }
}

def And(f1: FilterCond, f2: FilterCond): FilterCond = Compound(_ && _, List(f1, f2))
def Or(f1: FilterCond, f2: FilterCond): FilterCond = Compound(_ || _, List(f1, f2))
def Equal(f1: FilterCond, f2: FilterCond): FilterCond = Compound(_ == _, List(f1, f2))

case class Any(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    val results = fs.map(_.eval(r))
    Some(results.exists(_.get))
  }
}

case class All(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    val results = fs.map(_.eval(r))
    if (!results.exists(_.isEmpty))
      Some(results.forall(_.get))
    else
      None // At least one condition couldn't be evaluated
  }
}

implicit def tuple2Field(t: (String, String => Boolean)): Field = Field(t._1, t._2)

extension (f: FilterCond) {
  def ===(other: FilterCond) = Equal(f, other)
  def &&(other: FilterCond) = And(f, other)
  def ||(other: FilterCond) = Or(f, other)
  def !! = Not(f)
}


// Database.scala

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

val people: Table = Table("People",
  List(
    Map("name" -> "John", "age" -> "23", "address" -> "123 Main St"),
    Map("name" -> "Jane", "age" -> "27", "address" -> "456 Elm St")
  )
)

val jobs: Table = Table("Jobs",
  List(
    Map("title" -> "Engineer", "salary" -> "100000", "person_name" -> "John", "address" -> "654 Oak St"),
    Map("title" -> "Manager", "salary" -> "200000", "person_name" -> "Jane", "address" -> "123 Main St")
  )
)

val db: Database = Database(List(people, jobs))
val resultJoin: Option[Table] = db.join("People", "name", "Jobs", "person_name")