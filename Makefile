
GFLAGS := -Wall -Wextra -Wno-tabs -g

SRC := \
	file.f90 \
	string.f90 \
	parser.f90 \
	dynarray.f90 \
	dynarray_string.f90 \
	posting.f90 \
	bst.f90

OBJECTS := $(SRC:%.f90=%.o)

%.o: %.f90
	gfortran $(GFLAGS) -c $< -o $@

all: index search

index: index.f90 $(OBJECTS)
	gfortran $(GFLAGS) -o $@ index.f90 $(OBJECTS)

search: search.f90 $(OBJECTS)
	gfortran $(GFLAGS) -o $@ search.f90 $(OBJECTS)

CLEAN := $(OBJECTS) index
clean:
	rm -f $(CLEAN)

