CORE_COUNT := $(shell grep -P '^core id\t' /proc/cpuinfo | wc -l)

run: build
	stack -j${CORE_COUNT} exec sese-website-exe

build: ghc sese-website.cabal src app
	stack -j${CORE_COUNT} build --pedantic --ghc-options="-j${CORE_COUNT}"

watch: build
	stack -j${CORE_COUNT} build --pedantic --file-watch --ghc-options="-j${CORE_COUNT}"

ghc: stack.yaml
	stack -j${CORE_COUNT} setup
