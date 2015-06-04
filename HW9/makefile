DEBUG ?= 1
FFLAGS=-Wall -Wextra -Wconversion -pedantic
ifeq ($(DEBUG),1)
	FFLAGS += -g3
else
	FFLAGS += -O3
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
