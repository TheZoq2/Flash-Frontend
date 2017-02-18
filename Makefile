all:
	make tageditor
	make css
	make album
	make imageviewer

tageditor:
	elm-make src/TagEditor.elm --output=output/tag_editor.js

css:
	elm-css src/Style.elm --module=Style

album:
	elm-make src/Main.elm --output=output/album.js


imageviewer:
	elm-make src/ImageViewer.elm --output=output/image_viewer.html


clean:
	rm -r elm-stuff
