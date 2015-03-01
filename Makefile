TMP_DIR = /tmp/blog_build

site:
	.cabal-sandbox/bin/cabal exec ghc -- --make site.hs

build: site
	site build

release: build
	mkdir -p $(TMP_DIR)
	cp -r _site/* $(TMP_DIR)
	git checkout master
	cp -r $(TMP_DIR)/* .
	git commit -am "Build from source branch commit $(shell git rev-parse source | cut -c 1-6)"
	git checkout source
