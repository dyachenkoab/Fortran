FC=ifort
CCFLAGS=-W1 -nozero #-Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto
FOPT=-O3 

all:
	$(FC) $(CCFLAGS) -c src/environment.f90 -o obj/environment.o
	$(FC) $(CCFLAGS) $(FOPT) -c src/main.f90 -o obj/main.o
	$(FC) $(CCFLAGS) $(FOPT) -o bin/app ./obj/environment.o obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/output.txt
