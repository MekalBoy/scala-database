import scala.language.implicitConversions

trait FilterCond {def eval(r: Row): Option[Boolean]}

case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = ???
}

case class Compound(op: (Boolean, Boolean) => Boolean, conditions: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = ???
}

case class Not(f: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = ???
}

def And(f1: FilterCond, f2: FilterCond): FilterCond = ???
def Or(f1: FilterCond, f2: FilterCond): FilterCond = ???
def Equal(f1: FilterCond, f2: FilterCond): FilterCond = ???

case class Any(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = ???
}

case class All(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = ???
}

implicit def tuple2Field(t: (String, String => Boolean)): Field = ???

extension (f: FilterCond) {
  def ===(other: FilterCond) = ???
  def &&(other: FilterCond) = ???
  def ||(other: FilterCond) = ???
  def !! = ???
}