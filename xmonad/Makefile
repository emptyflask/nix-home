build:
	nix-build release.nix

run: build
	result/bin/xmonad-x86_64-linux

deps:
	cabal2nix . > build.nix

shell:
	nix-shell shell.nix

shell-pure:
	nix-shell --pure shell.nix

external-shell:
	nix-shell external.nix

.PHONY: build run repl shell shell-pure external-shell
