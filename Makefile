# Where things are and where they should be
SRC_DIRS = external/fortran-regex/src src/instruments src
EXE = compose

# FC = the compiler to use
FC=gfortran

# Compiler options
debug: FFLAGS = -O0 -g -std=f2008 -Wall -fcheck=all -ffpe-trap=invalid,zero

debug: FFLAGS = -O0 -g -std=f2008 -Wall -fcheck=all -ffpe-trap=invalid,zero
debug: $(EXE)

fast: FFLAGS = -O2 -march=native
fast: $(EXE)

include depend.mk

build/%.o: %.f90
	mkdir -p $(@D)
	$(FC) -J build -c $(FFLAGS) $< -o $@ 

depend depend.mk:
	makedepf90 -B build -o $(EXE) $(foreach d,$(SRC_DIRS),$d/*.f90) > depend.mk

clean:
	rm -rf build $(EXE) depend.mk

