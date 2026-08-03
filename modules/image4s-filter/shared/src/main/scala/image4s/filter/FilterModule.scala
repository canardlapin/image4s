package image4s.filter

/** Scaffold marker for the filter artifact.
  *
  * Real filter operations land in later tasks; this object exists so the module compiles and the
  * package boundary is enforceable from the first commit.
  */
object FilterModule:
  val name: String = "image4s-filter"
