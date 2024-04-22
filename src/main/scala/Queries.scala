object Queries {

  def killJackSparrow(t: Table): Option[Table] = queryT(Some(t), "FILTER", !!(Field("name", _ == "Jack")))

  def insertLinesThenSort(db: Database): Option[Table] =
    queryT(
      queryT(
        Some(
          queryDB(
            queryDB(Some(db), "CREATE", "Inserted Fellas"), "SELECT", List("Inserted Fellas")
          ).get(0)
        ),
        "INSERT",
        List(Map("name" -> "Ana", "age" -> "93", "CNP" -> "455550555"),
          Map("name" -> "Diana", "age" -> "33", "CNP" -> "255532142"),
          Map("name" -> "Tatiana", "age" -> "55", "CNP" -> "655532132"),
          Map("name" -> "Rosmaria", "age" -> "12", "CNP" -> "855532172")
        )
      ),
      "SORT",
      "age"
    )

  def youngAdultHobbiesJ(db: Database): Option[Table] =
    queryT(
      queryT(
        queryT(
          queryT(
          Some(
            queryDB(
              queryDB(Some(db), "JOIN", "People", "name", "Hobbies", "name"), "SELECT", List("People")
            ).get(0)
          ),
            "FILTER",
            Field("age", _ < "25")
          ),
          "FILTER",
          Field("name", _.charAt(0) == 'J')
        ),
        "FILTER",
        !!(Field("hobby", _.isBlank))
      ),
      "EXTRACT",
      List("name", "hobby")
    )
}
