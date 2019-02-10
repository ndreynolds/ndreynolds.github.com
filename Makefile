TMP_DIR = /tmp/blog_build

SITE_PATHS = posts/ images/ fonts/ css/ js/ atom.xml rss.xml

site: Site.hs
	stack build

build: site
	stack exec site rebuild

release: build
	mkdir -p $(TMP_DIR)
	cp -r _site/* $(TMP_DIR)
	git checkout master
	cp -r $(TMP_DIR)/* .
	git add $(SITE_PATHS)
	git commit -am "Build from source branch commit $(shell git rev-parse source | cut -c 1-6)"
	git checkout source
