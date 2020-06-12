
.PHONY: docs-build
docs-build:
	git checkout master
	dune build @doc
	git checkout -

.PHONY: docs-update
docs-update: docs-build
	git checkout gh-pages
	rm -rf ./streaming
	cp -r ./_build/default/_doc/_html/streaming/ ./streaming
	@if git diff-index --quiet HEAD --; then \
		echo "No changes."; \
	else \
		git add ./streaming; \
		git commit -m "Update docs."; \
		git push origin gh-pages; \
	fi
	git checkout -


