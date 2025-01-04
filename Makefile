# FC = the compiler to use
FC=gfortran

# Compiler options
FFLAGS = -O0 -g -std=f2008 -Wall -fcheck=all -ffpe-trap=invalid,zero

include depend.mk

build/%.o: %.f90
	mkdir -p $(@D)
	$(FC) -J build -c $(FFLAGS) $< -o $@ 

depend depend.mk:
	makedepf90 -B build -o compose external/*/src/*f90 src/instruments/*f90 src/*f90 > depend.mk

clean:
	rm -rf build compose depend.mk

