DEBUG ?= 1
FFLAGS=-Wall -Wextra -Wconversion -Werror -std=gnu -pedantic-errors -fimplicit-none -Wno-unused-dummy-argument -Wno-unused-parameter -fmax-errors=1
ifeq ($(DEBUG),1)
	FFLAGS += -g3 -O0 -fbacktrace -fbounds-check -ffpe-trap=zero,overflow,underflow -Warray-temporaries -Wcharacter-truncation
else
	FFLAGS += -O3 -ffrontend-optimize -Wfunction-elimination
endif

F90 = gfortran $(FFLAGS)

objects = euler-method.o mathtools.o electromagnetism.o
modfiles = mathtools.mod electromagnetism.mod
EXE = euler

euler: $(objects)
	$(F90) -o $@ $^

euler-method.o: euler-method.f90 mathtools.mod electromagnetism.mod
	$(F90) -c $<

mathTools.o mathtools.mod: mathtools.f90
	$(F90) -c $<

Electromagnetism.o electromagnetism.mod: electromagnetism.f90 mathtools.mod
	$(F90) -c $<

clean:
	rm -f $(objects) $(modfiles) $(EXE)
