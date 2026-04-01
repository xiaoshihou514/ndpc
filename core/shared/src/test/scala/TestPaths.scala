package ndpc

object TestPaths {
    private val root =
        os.Path(Option(System.getProperty("ndpc.repoRoot")).getOrElse(os.pwd.toString), os.pwd)

    def path(rel: String): os.Path = root / os.RelPath(rel)
}
