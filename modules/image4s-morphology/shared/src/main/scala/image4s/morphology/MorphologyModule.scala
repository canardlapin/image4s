package image4s.morphology

/** Scaffold marker for the morphology artifact.
  *
  * Real morphological operations land in later tasks; this object exists so the module compiles and
  * the package boundary is enforceable from the first commit.
  */
object MorphologyModule:
  val name: String = "image4s-morphology"
