val t =
    """123
      |4 5
      |678
    """.trim().stripMargin.split("\n").flatMap(_.split("")).toList

t.slice(0, 1)
t.slice (1, 2)