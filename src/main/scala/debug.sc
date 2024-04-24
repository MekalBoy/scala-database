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
        val first = td1.filterNot(td2.map(_(c1)) contains _(c1))
        val second = td2.filterNot(td1.map(_(c1)) contains _(c1))

        println("Merged")
        println(merged)
        println("First")
        println(first)
        println("Second")
        println(second)

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

val people1: Table = Table("People",
  List(
    Map("name" -> "John", "age" -> "23", "address" -> "123 Main St"),
    Map("name" -> "Jane", "age" -> "27", "address" -> "456 Elm St")
  )
)

val jobs1: Table = Table("Jobs",
  List(
    Map("title" -> "Engineer", "salary" -> "100000", "person_name" -> "John", "address" -> "654 Oak St"),
    Map("title" -> "Manager", "salary" -> "200000", "person_name" -> "Jane", "address" -> "123 Main St")
  )
)

val people : Table = Table(
  "People",
  List(
    Map("name" -> "John", "age" -> "23", "address" -> "123 Main St"),
    Map("name" -> "Jane", "age" -> "27", "address" -> "456 Elm St"),
    Map("name" -> "Joe", "age" -> "30", "address" -> "789 Maple St"),
    Map("name" -> "Jill", "age" -> "25", "address" -> "101 Oak St"),
    Map("name" -> "Jack", "age" -> "27", "address" -> "112 Pine St"),
    Map("name" -> "Jen", "age" -> "24", "address" -> "131 Cedar St"),
    Map("name" -> "Jim", "age" -> "26", "address" -> "141 Birch St"),
    Map("name" -> "Jesse", "age" -> "29", "address" -> "151 Spruce St"),
    Map("name" -> "Jenny", "age" -> "23", "address" -> "161 Fir St"),
    Map("name" -> "Jerry", "age" -> "28", "address" -> "171 Larch St")
  )
)

val jobs: Table = Table(
  "Jobs",
  List(
    Map("title" -> "Engineer", "salary" -> "100000", "person_name" -> "John", "address" -> "654 Oak St"),
    Map("title" -> "Manager", "salary" -> "200000", "person_name" -> "Jane", "address" -> "123 Main St"),
    Map("title" -> "CEO", "salary" -> "300000", "person_name" -> "Joe", "address" -> "789 Maple St"),
    Map("title" -> "Banker", "salary" -> "150000", "person_name" -> "Mona", "address" -> "789 Maple St"),
    Map("title" -> "Doctor", "salary" -> "200000", "person_name" -> "Jack", "address" -> "112 Pine St"),
    Map("title" -> "Nurse", "salary" -> "100000", "person_name" -> "Jen", "address" -> "132 Cedar St"),
    Map("title" -> "Teacher", "salary" -> "80000", "person_name" -> "Jill", "address" -> "888 Elm St"),
    Map("title" -> "Engineer", "salary" -> "120000", "person_name" -> "Mimi", "address" -> "141 Birch St"),
    Map("title" -> "Programmer", "salary" -> "250000", "person_name" -> "Jenny", "address" -> "161 Fir St"),
    Map("title" -> "Teacher", "salary" -> "400000", "person_name" -> "Jerry", "address" -> "171 Larch St")
  )
)

val db: Database = Database(List(people, jobs))
val resultJoin: Option[Table] = db.join("People", "name", "Jobs", "person_name")