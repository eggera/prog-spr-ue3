
all:
	ghc Main

less_dirty:
	rm -rf *.hi *.o

clean: less_dirty
	rm -rf Main

.PHONY : less_dirty
.PHONY : clean

