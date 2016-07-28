main:
	make -C src

clean:
	make clean -C src

format:
	find src | grep ".hs" | xargs -n 1 -I {} echo 'stylish-haskell {} > {}r && mv {}r {}' | sh
