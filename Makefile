
SRC := \
	file.f90 \
	string.f90 \
	parser.f90

OBJECTS := $(SRC:%.f90=%.o)

%.o: %.f90
	gfortran -c $< -o $@

all: index

index: index.f90 $(OBJECTS)
	gfortran -o $@ index.f90 $(OBJECTS)

CLEAN := $(OBJECTS) index
clean:
	rm -f $(CLEAN)

