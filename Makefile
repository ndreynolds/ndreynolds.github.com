TMP_DIR = /tmp/blog_build

site: Site.hs
	stack build

build: site
	stack exec site build

release: build
	mkdir -p $(TMP_DIR)
	cp -r _site/* $(TMP_DIR)
	git checkout master
	cp -r $(TMP_DIR)/* .
	git add posts/ images/ fonts/ css/ js/
	git commit -am "Build from source branch commit $(shell git rev-parse source | cut -c 1-6)"
	git checkout source
