package ndpc

object TestPaths {
    private val root = os.Path(sys.props.getOrElse("ndpc.repoRoot", os.pwd.toString), os.pwd)

    def path(rel: String): os.Path = root / os.RelPath(rel)
}
