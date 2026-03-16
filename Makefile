format:
	sbt scalafmtAll

compile:
	sbt compile

test:
	sbt test

run:
	sbt "cli/run $(ARGS)"

release:
	sbt cli/assembly cli/graalNativeImage cliNative/rootNativeLink

jar:
	sbt cli/assembly

graal:
	sbt cli/graalNativeImage

native:
	sbt cliNative/rootNativeLink

clean:
	sbt clean
