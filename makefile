games.js:
	elm make --optimize --output=docs/assets/js/games.js src/Main.elm

serve:
	cd docs && bundle exec jekyll serve
