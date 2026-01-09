format:
	scala format src/

release:
	scala --power package . --native --native-mode release-full --force -o ndpc
	scala --power package . --assembly --force -o ndpc.jar
