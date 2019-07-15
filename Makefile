all: tageditor album

tageditor:
	elm make src/TagEditor.elm --output=output/tag_editor.html

album:
	elm make src/Main.elm --output=output/album.html



clean:
	rm -r elm-stuff
