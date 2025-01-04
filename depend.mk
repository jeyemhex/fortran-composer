FOBJ=build/external/fortran-regex/src/nfa_mod.o build/external/fortran-regex/src/postfix_mod.o build/external/fortran-regex/src/regex.o build/external/fortran-regex/src/states_mod.o build/external/fortran-regex/src/utils_mod.o build/src/instruments/instruments.o build/src/instruments/saw.o build/src/instruments/sine.o build/src/instruments/square.o build/src/composition.o build/src/constants.o build/src/main.o build/src/notes.o build/src/parameters.o build/src/parser.o build/src/tracks.o 

compose: $(FOBJ)
	$(FC) -o $@ $(FFLAGS) $(LDFLAGS) $(FOBJ) $(LIBS)

build/external/fortran-regex/src/nfa_mod.o : external/fortran-regex/src/nfa_mod.f90 build/external/fortran-regex/src/postfix_mod.o build/external/fortran-regex/src/states_mod.o build/external/fortran-regex/src/utils_mod.o 
build/external/fortran-regex/src/postfix_mod.o : external/fortran-regex/src/postfix_mod.f90 build/external/fortran-regex/src/states_mod.o build/external/fortran-regex/src/utils_mod.o 
build/external/fortran-regex/src/regex.o : external/fortran-regex/src/regex.f90 build/external/fortran-regex/src/nfa_mod.o build/external/fortran-regex/src/postfix_mod.o build/external/fortran-regex/src/states_mod.o build/external/fortran-regex/src/utils_mod.o 
build/external/fortran-regex/src/states_mod.o : external/fortran-regex/src/states_mod.f90 build/external/fortran-regex/src/utils_mod.o 
build/external/fortran-regex/src/utils_mod.o : external/fortran-regex/src/utils_mod.f90 
build/src/instruments/instruments.o : src/instruments/instruments.f90 build/src/constants.o build/src/parameters.o build/src/notes.o 
build/src/instruments/saw.o : src/instruments/saw.f90 build/src/notes.o build/src/parameters.o build/src/constants.o build/src/instruments/instruments.o 
build/src/instruments/sine.o : src/instruments/sine.f90 build/src/notes.o build/src/parameters.o build/src/constants.o build/src/instruments/instruments.o 
build/src/instruments/square.o : src/instruments/square.f90 build/src/notes.o build/src/parameters.o build/src/constants.o build/src/instruments/instruments.o 
build/src/composition.o : src/composition.f90 build/external/fortran-regex/src/regex.o build/src/tracks.o build/src/instruments/square.o build/src/instruments/saw.o build/src/instruments/sine.o build/src/instruments/instruments.o build/src/parameters.o build/src/constants.o 
build/src/constants.o : src/constants.f90 
build/src/main.o : src/main.f90 build/src/parser.o build/src/composition.o build/src/parameters.o 
build/src/notes.o : src/notes.f90 build/src/constants.o 
build/src/parameters.o : src/parameters.f90 build/src/constants.o 
build/src/parser.o : src/parser.f90 build/external/fortran-regex/src/regex.o build/src/tracks.o build/src/composition.o build/src/instruments/square.o build/src/instruments/saw.o build/src/instruments/sine.o build/src/instruments/instruments.o build/src/parameters.o build/src/constants.o 
build/src/tracks.o : src/tracks.f90 build/src/notes.o build/src/parameters.o build/src/instruments/instruments.o build/src/constants.o 
