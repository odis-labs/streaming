
.PHONY: docs-build
docs-build:
	git checkout master
	dune build @doc
	git checkout -

.PHONY: git-check-uncommited
git-check-uncommited:
	@git diff-index --quiet HEAD -- || (echo "Error: uncommited changes"; exit 1)

.PHONY: docs-update
docs-update: git-check-uncommited docs-build
	git checkout gh-pages
	rm -rf ./streaming
	cp -r ./_build/default/_doc/_html/streaming/ ./streaming
	if git diff-index --quiet HEAD -- ./streaming; then \
		echo "No changes."; \
	else \
		git add ./streaming; \
		git commit -m "Update docs."; \
		git push origin gh-pages; \
	fi
	git checkout -

.PHONY: bench
bench:
	@dune exec -- ./bench/Bench_main.exe --format=json