ifeq ($(MODE),)
MODE := production # development
endif

all: liquidity.node.js liquidity.browser.js

node_modules: package.json
	@npm install --only=dev

../../_obuild/liquidity-js/liquidity-js.js: ../../_obuild
	@ocp-build build liquidity-js

liquidity.node.js: node_modules ../../_obuild/liquidity-js/liquidity-js.js wrapper/liquidity-js-wrapper.js
	@echo --- Building NodeJS library -------------------------
	@npx webpack --mode $(MODE) --target node wrapper/liquidity-js-wrapper.js -o $@

liquidity.browser.js: node_modules ../../_obuild/liquidity-js/liquidity-js.js wrapper/liquidity-js-wrapper.js
	@echo --- Building browser library ------------------------
	@npx webpack --mode $(MODE) --target web wrapper/liquidity-js-wrapper.js -o $@
