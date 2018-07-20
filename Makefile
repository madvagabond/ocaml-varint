.PHONY: all clean 

all:
	jbuilder build @install --dev

clean:
	jbuilder clean

test:
	jbuilder runtest --dev

