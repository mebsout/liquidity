ifeq ($(MODE),)
MODE := development # production
endif

all: liquidity.js

liquidity.js: ../../_obuild/liquidity-js/liquidity-js.js
	@ocp-build build liquidity-js
	@npm install --only=dev
	@echo --- Building NodeJS library -------------------------
	@npx webpack --mode $(MODE) --target node liquidity-js-node.js -o liquidity.node.js
	@echo --- Building browser library ------------------------
	@npx webpack --mode $(MODE) --target web liquidity-js-node.js -o liquidity.browser.js

.PHONY: liquidity.js
