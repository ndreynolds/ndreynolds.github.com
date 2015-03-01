site:
	.cabal-sandbox/bin/cabal exec ghc -- --make site.hs

build: site
	site build

release: build
	cp -r _site /tmp/blog_build
	git checkout master
	cp -r /tmp/blog_build/* .
	git commit -am "Build from source branch commit $(shell git rev-parse source | cut -c 1-6)"
	git checkout source
