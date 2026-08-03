package image4s.ops

/** Algorithm selection for filter execution. Meaning lives in the operation value; this only
  * chooses how it is computed.
  */
enum FilterMethod derives CanEqual:
  case Auto, Direct, Separable, Fft

enum Parallelism derives CanEqual:
  case Auto
  case Sequential
  case Threads(count: Int)

enum LayoutPolicy derives CanEqual:
  case Auto, PreferCanonical, PreserveInput

final case class ExecutionPolicy(
    method: FilterMethod = FilterMethod.Auto,
    parallelism: Parallelism = Parallelism.Auto,
    layout: LayoutPolicy = LayoutPolicy.Auto
)

enum SelectedMethod derives CanEqual:
  case Direct, Separable, Fft

final case class PlanReport(
    method: SelectedMethod,
    passes: Int,
    inputMaterialized: Boolean,
    outputShape: Vector[Int],
    workspaceBytes: Long
)

/** Opaque workspace owned by the caller for reusable prepared execution. */
opaque type Workspace = Array[Byte]

object Workspace:
  def allocate(bytes: Int): Workspace =
    new Array[Byte](bytes.max(0))

  extension (workspace: Workspace)
    def size: Int =
      workspace.length

/** Prepared execution schedule for an operation.
  *
  * Must not capture reusable mutable scratch if it claims to be thread-safe. Callers allocate
  * workspace explicitly via [[allocateWorkspace]].
  */
trait PreparedPlan[-In, +Out]:
  def report: PlanReport

  def allocateWorkspace(): Workspace

  def run(input: In): Either[OpError, Out]

  def runWith(input: In, workspace: Workspace): Either[OpError, Out]

/** Operation value describing meaning, not schedule. */
trait ImageOperation[-In, +Out]:
  def prepare(
      input: In,
      policy: ExecutionPolicy = ExecutionPolicy()
  ): Either[OpError, PreparedPlan[In, Out]]

  def run(
      input: In,
      policy: ExecutionPolicy = ExecutionPolicy()
  ): Either[OpError, Out] =
    prepare(input, policy).flatMap(_.run(input))
