format:
	sbt scalafmtAll

compile:
	sbt compile

test:
	sbt test

run:
	sbt "cli/run -- $(ARGS)"

release:
	sbt cli/assembly cli/graalNativeImage cliNative/rootNativeLink

clean:
	sbt clean
