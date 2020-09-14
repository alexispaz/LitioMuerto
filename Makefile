.SUFFIXES:
FC=gfortran
COMPILE.f08 = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c


SOURCES=main.f90 constants.f90 types.f90


main: $(subst .f90,.o,$(SOURCES))
	$(FC) -o $@ $+


%.o %.mod %.smod: %.f90
	$(COMPILE.f08) -o $*.o $<
	@touch $@


main.o: constants.o types.o
types.o: constants.o

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod main	
