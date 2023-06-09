SHELL = /bin/sh

# note: parallel builts are currently only supported for a single binary
# This is due to object files compiling differently, e.g., for the analyzer

# Five input variables have to be defined:
# - MODEL_HEADER
# - MODEL_HEADER_DEFINES
# - MODEL_IMPL
# - FILTER_PARAMETERS
# - OBSERVATION_MODEL_HEADER
# - OBSERVATION_MODEL_IMPL

# Optionally, you can control various directories
# - srcdir: where the CCBM source files can be found
# - BINDIR: generated binaries
# - OBJDIR: generated object files (defaults to BINDIR)
# - BINPFX: prefix for binary names

# You may set compiler / compiler options:
# - CXX
# - CXXFLAGS
# - LDFLAGS

# Per default, only short compilation command lines will be printed
# Set Q=n to print full command lines

ifeq ($(strip $(MODEL_HEADER)),)
$(error MODEL_HEADER not defined)
endif
ifeq ($(strip $(MODEL_HEADER_DEFINES)),)
$(error MODEL_HEADER_DEFINES not defined)
endif
ifeq ($(strip $(MODEL_IMPL)),)
$(error MODEL_IMPL not defined)
endif
ifeq ($(strip $(FILTER_PARAMETERS)),)
$(error FILTER_PARAMETERS not defined)
endif
ifeq ($(strip $(OBSERVATION_MODEL_HEADER)),)
$(error OBSERVATION_MODEL_HEADER not defined)
endif
ifeq ($(strip $(OBSERVATION_MODEL_IMPL)),)
$(error OBSERVATION_MODEL_IMPL not defined)
endif

# if Q is not set, use default value @ (do not print commands)
ifndef Q
	Q = @
else
	# otherwise, set Q empty and print full commands
	override Q =
endif


MAKEFILE := $(realpath $(lastword $(MAKEFILE_LIST)))
srcdir := .
BINDIR := .
OBJDIR := $(BINDIR)
BINPFX :=

BIN := $(BINDIR)/$(BINPFX)

CXX := g++
CC := gcc
CXXFLAGS := -Wall -Wextra

ALL_CXXFLAGS := -I. -I$(srcdir) -I$(srcdir)/lib -isystem $(srcdir)/lib/boost -I$(srcdir)/lib/fann/src/include \
	"-DMODEL_HEADER=<$(MODEL_HEADER)>" "-DMODEL_HEADER_DEFINES=<$(MODEL_HEADER_DEFINES)>" "-DMODEL_IMPL=<$(MODEL_IMPL)>" \
	"-DFILTER_PARAMETERS=<$(FILTER_PARAMETERS)>" \
	"-DOBSERVATION_MODEL_HEADER=<$(OBSERVATION_MODEL_HEADER)>" "-DOBSERVATION_MODEL_IMPL=<$(OBSERVATION_MODEL_IMPL)>" \
	$(CXXFLAGS) \
    -Wno-missing-braces

ALL_LDFLAGS := $(LDFLAGS)

OBJS_BINS := marginal filter analyzer validator hmm simulator observationtest ann-train
BINS := $(patsubst %,$(BIN)%,$(OBJS_BINS))

# all objects common to all binaries
OBJS_COMMON := obsmodel.o duration.o util/allocator.o util/parameter.o
OBJS_ANN := fann/floatfann.o
# common object files required by the filters programs
OBJS_FILTER := model.o global.o state.o stateFunctions.o heuristics.o
OBJS_MARGINAL := util/linear_model.o marginal/particle_single.o marginal/particle_single_sequential.o marginal/particle_obsEquiv.o marginal/particle_startTimes.o
# the analyzer changes only some source files
OBJS_ANALYZER := model.o global.o state.o stateFunctions.o
OBJS_BOOST_GZIP := boost/zlib.o boost/gzip.o

OBJS_BINS := $(patsubst %,$(OBJDIR)/%.o,$(OBJS_BINS)) $(OBJDIR)/main.o
OBJS_COMMON := $(patsubst %.o,$(OBJDIR)/%.o,$(OBJS_COMMON))
OBJS_ANN := $(patsubst %.o,$(OBJDIR)/%.o,$(OBJS_ANN))
OBJS_FILTER := $(patsubst %.o,$(OBJDIR)/%.o,$(OBJS_FILTER))
OBJS_MARGINAL := $(patsubst %.o,$(OBJDIR)/%.o,$(OBJS_MARGINAL))
OBJS_ANALYZER := $(patsubst %.o,$(OBJDIR)/%-analyzer.o,$(OBJS_ANALYZER))
OBJS_BOOST_GZIP := $(patsubst %.o,$(OBJDIR)/%.o,$(OBJS_BOOST_GZIP))

ALL_OBJS := $(OBJS_BINS) $(OBJS_COMMON) $(OBJS_ANN) $(OBJS_FILTER) $(OBJS_MARGINAL) $(OBJS_ANALYZER) $(OBJS_BOOST_GZIP)

# headers required by all, i.e. required when using global.cpp/global.h
SRC_HEADERS := $(srcdir)/*.h
ALL_HEADERS := $(SRC_HEADERS) $(OBSERVATION_MODEL_HEADER) $(MODEL_HEADER) $(MODEL_HEADER_DEFINES)

# Targets

all: $(BIN)marginal $(BIN)filter $(BIN)analyzer $(BIN)validator $(BIN)simulator

# variables specific to binaries
$(OBJDIR)/hmm.o: ALL_CXXFLAGS += -DUSE_EXACT_FILTER=1
$(OBJDIR)/%-analyzer.o $(OBJDIR)/analyzer.o: ALL_CXXFLAGS += -DHAS_STATE_SPACE_ANALYZER=1

# include dependencies generated by makedepend.sh script
include $(srcdir)/.depend


# Binaries

$(BIN)marginal: $(OBJDIR)/marginal.o $(OBJS_COMMON) $(OBJS_FILTER) $(OBJS_MARGINAL) $(OBJS_ANN) $(OBJS_BOOST_GZIP)
ifeq ($(Q),@)
	@echo $(CXX) -o "$@"
endif
	+$(Q)$(CXX) -o "$@" $+ -lz $(ALL_CXXFLAGS) $(ALL_LDFLAGS)

$(BIN)filter: $(OBJDIR)/main.o $(OBJS_COMMON) $(OBJS_FILTER) $(OBJS_ANN) $(OBJS_BOOST_GZIP)
ifeq ($(Q),@)
	@echo $(CXX) -o "$@"
endif
	+$(Q)$(CXX) -o "$@" $+ -lz $(ALL_CXXFLAGS) $(ALL_LDFLAGS)

$(BIN)hmm: $(OBJDIR)/hmm.o $(OBJS_COMMON) $(OBJS_FILTER) $(OBJS_ANN)
ifeq ($(Q),@)
	@echo $(CXX) -o "$@"
endif
	+$(Q)$(CXX) -o "$@" $+ $(ALL_CXXFLAGS) $(ALL_LDFLAGS)

$(BIN)analyzer: $(OBJDIR)/analyzer.o $(OBJS_COMMON) $(OBJS_ANALYZER)
ifeq ($(Q),@)
	@echo $(CXX) -o "$@"
endif
	+$(Q)$(CXX) -o "$@" $+ $(ALL_CXXFLAGS) $(ALL_LDFLAGS)

$(BIN)validator: $(OBJDIR)/validator.o $(OBJS_COMMON) $(OBJS_FILTER) $(OBJS_ANN)
ifeq ($(Q),@)
	@echo $(CXX) -o "$@"
endif
	+$(Q)$(CXX) -o "$@" $+ $(ALL_CXXFLAGS) $(ALL_LDFLAGS)

$(BIN)simulator: $(OBJDIR)/simulator.o $(OBJS_COMMON) $(OBJS_FILTER) $(OBJS_ANN)
ifeq ($(Q),@)
	@echo $(CXX) -o "$@"
endif
	+$(Q)$(CXX) -o "$@" $+ $(ALL_CXXFLAGS) $(ALL_LDFLAGS)

$(BIN)observationtest: $(OBJDIR)/observationtest.o $(OBJS_COMMON) $(OBJS_FILTER) $(OBJS_ANN)
ifeq ($(Q),@)
	@echo $(CXX) -o "$@"
endif
	+$(Q)$(CXX) -o "$@" $+ $(ALL_CXXFLAGS) $(ALL_LDFLAGS)

$(BIN)ann-train: $(OBJDIR)/ann-train.o $(OBJS_COMMON) $(OBJS_FILTER) $(OBJS_ANN)
ifeq ($(Q),@)
	@echo $(CXX) -o "$@"
endif
	+$(Q)$(CXX) -o "$@" $+ $(ALL_CXXFLAGS) $(ALL_LDFLAGS)


# object files for the main binaries


$(OBJDIR)/marginal.o: $(srcdir)/marginal.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/main.o: $(srcdir)/main.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/analyzer.o: $(srcdir)/analyzer.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/validator.o: $(srcdir)/validator.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/hmm.o: $(srcdir)/hmm.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/simulator.o: $(srcdir)/simulator.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/observationtest.o: $(srcdir)/observationtest.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/ann-train.o: $(srcdir)/ann_train.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

# object files for the modules
# the patterns % are used to be able to build binary-specific files
# e.g. model.o for the marginal filter, and model-analyzer.o for the analyzer
# Because patterns must be non-empty, we write global%o instead of global%.o, as global.o
# is the default
# Note: if multiple patterns match (state vs stateFunctions), old versions use the first rule
#   	Newer versions use the most specific.

$(OBJDIR)/obsmodel%o: $(srcdir)/obsmodel.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/model%o: $(srcdir)/model.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/stateFunctions%o: $(srcdir)/stateFunctions.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/state%o: $(srcdir)/state.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/global%o: $(srcdir)/global.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/heuristics%o: $(srcdir)/heuristics.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/duration%o: $(srcdir)/duration.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)


$(OBJDIR)/marginal/particle_single.o: $(srcdir)/marginal/particle_single.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	@mkdir -p "$(OBJDIR)/marginal/"
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/marginal/particle_single_sequential.o: $(srcdir)/marginal/particle_single_sequential.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	@mkdir -p "$(OBJDIR)/marginal/"
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/marginal/particle_obsEquiv.o: $(srcdir)/marginal/particle_obsEquiv.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	@mkdir -p "$(OBJDIR)/marginal/"
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/marginal/particle_startTimes.o: $(srcdir)/marginal/particle_startTimes.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	@mkdir -p "$(OBJDIR)/marginal/"
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)


$(OBJDIR)/util/allocator.o: $(srcdir)/util/allocator.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	@mkdir -p "$(OBJDIR)/util/"
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/util/linear_model.o: $(srcdir)/util/linear_model.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	@mkdir -p "$(OBJDIR)/util/"
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

$(OBJDIR)/util/parameter.o: $(srcdir)/util/parameter.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $<
endif
	@mkdir -p "$(OBJDIR)/util/"
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS)

# libraries

$(OBJDIR)/fann/floatfann.o: $(srcdir)/lib/fann/src/floatfann.c
ifeq ($(Q),@)
	@echo $(CC) -c -o "$@" $< -w -std=c99
endif
	@mkdir -p "$(OBJDIR)/fann/"
	$(Q)$(CC) -c -o "$@" "$<" $(ALL_CXXFLAGS) -w -std=c99

$(srcdir)/lib/boost/libs/iostreams/src/gzip.cpp $(srcdir)/lib/boost/libs/iostreams/src/zlib.cpp:
	@echo
	@echo "Error:"
	@echo "Boost libraries not found in their right place."
	@echo "Try deleting src/lib/boost and running ./getboost.sh"
	@echo
	@false

$(OBJDIR)/boost/zlib.o: $(srcdir)/lib/boost/libs/iostreams/src/zlib.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $< -w
endif
	@mkdir -p "$(OBJDIR)/boost/"
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS) -w

$(OBJDIR)/boost/gzip.o: $(srcdir)/lib/boost/libs/iostreams/src/gzip.cpp
ifeq ($(Q),@)
	@echo $(CXX) -c -o "$@" $< -w
endif
	@mkdir -p "$(OBJDIR)/boost/"
	$(Q)$(CXX) -c -o "$@" "$<" $(ALL_CXXFLAGS) -w

clean:
	rm -f $(ALL_OBJS) $(BINS)

.PHONY: all clean
