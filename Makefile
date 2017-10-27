all:
	make tageditor
	make css
	make album

tageditor:
	elm-make src/TagEditor.elm --output=output/tag_editor.js

css:
	elm-css src/Style.elm --module=Style

album:
	elm-make src/Main.elm --output=output/album.js



clean:
	rm -r elm-stuff
