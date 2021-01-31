target  = ../main
object  = \
  variablesMod.o \
  utilitiesMod.o \
  shapeFuncMod.o \
  ioUtilityMod.o \
  pBoundaryMod.o \
  fBoundaryMod.o \
  rkgSolverMod.o \
  ebfSolverMod.o \
  initiatorMod.o \
  diagnosisMod.o \
  main.o


# --- for gfortran --- # 
FC      = gfortran
FFLAGS  = -O3
LDFLAGS = 

# --- for ifort    --- #
# FC      = ifort
# FFLAGS  = -O4 -qopenmp -parallel -xhost -ipo
# LDFLAGS = 


.SUFFIXES: .o .f90

%.o: %.f90
	$(FC) -c $< $(FFLAGS) $(LDFLAGS)

%.mod: %.f90 %.o
	@:

$(target): $(object)
	$(FC) -o $@ $(object) $(FFLAGS) $(LDFLAGS)


.PHONY:clean all

clean:
	$(RM) $(target) $(object) *.mod

all: clean $(target)
