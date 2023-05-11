#!/bin/bash

# for every source and header file, generate a list of all header files that are dependencies
# g++ -MM generates a list of dependencies, the /dev/null defines is required for correct parsing
# the sed script
# * removes the artifical /dev/null dependencies
# * concatenates all broken lines (\ at the end of line)
# * reqrites the targets according to the Makefile, e.g. state.o -> $(OBJDIR)/state%o
# * add dependencies on model files, e.g. model.o depends not only on model.cpp, but also on MODEL_IMPL

suf=cpp

for folder in "" estimate/ marginal/ util/; do

if [[ $(find "./$folder" -maxdepth 1 -name \*.$suf | wc -l) -eq 0 ]]; then
    # no files in this folder, continue
    continue
fi

g++ -std=c++11 -D{OBSERVATION_,}MODEL_{HEADER,IMPL}='</dev/null>' -DMODEL_HEADER_DEFINES='</dev/null>' -DFILTER_PARAMETERS='</dev/null>' -Ilib -MM ${folder}*.$suf | sed '
:a
# collect all lines
N
$!ba

# Remove /dev/null
s@ */dev/null *@ @g

# concatenate all broken lines
s/ *\\\n */ /g
# remove trailing space
s/ *\n/\n/g
' | sed '
# add additional dependencies
s/ model\.h/& \$(FILTER_PARAMETERS) \$(MODEL_HEADER) \$(MODEL_HEADER_DEFINES)/g
s/obsmodel\.h/& \$(OBSERVATION_MODEL_HEADER)/g
s/ model\.cpp/& \$(MODEL_IMPL)/g
s/obsmodel\.cpp/& \$(OBSERVATION_MODEL_IMPL)/g

# Add OBJDIR prefix
s@\(^\|\n\)\([^ .]\+\)\.o:@\1\$(OBJDIR)/'$folder'\2.o:@g
# Add srcdir prefix
s@ \([^ ]\+\)\.\(cpp\|h\|hpp\)@ \$(srcdir)/\1.\2@g

# Make variable definitions out of rule, e.g. model.o: ...h -> DEPEND_model_'$suf' := ...h
#s@\(^\|\n\)\([^.]\+\)\.o:@\1DEPEND_\2_'$suf' :=@g

# copy dependencies for model.o, ... to model-analyzer.o, ...
s@^\(\$(OBJDIR)/\(model\|global\|state\|stateFunctions\)\)\(\.o:.*$\)@&\n\1-analyzer\3@
'

done
