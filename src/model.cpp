#include <cstdio>
#include <cmath>
#include <climits>

#include "obsmodel.h"
#include "model.h"
#include "duration.h"

#include <boost/array.hpp>
#include <boost/algorithm/string.hpp>

#ifndef MODEL_IMPL
#error "MODEL_IMPL not defined"
#endif

namespace model {

#include MODEL_IMPL


int getnops() {
    return NOPS;
}

#if DO_MULTI_AGENT
char const * const magMode = "multi";
#else
char const * const magMode = "single";
#endif

bool StateRec_Equal::operator()(StateRec const & a, StateRec const & b) const {
    return memcmp(&a, &b, sizeof(StateRec)) == 0;
}

size_t StateRec_Hash::operator()(StateRec const & a) const {
    // first try specialized hashers for small StateRecs
    if (sizeof(StateRec) == sizeof(char)) {
        boost::hash<char> hasher;
        return hasher(*((char const*) &a));
    } else if (sizeof(StateRec) == sizeof(short)) {
        boost::hash<short> hasher;
        return hasher(*((short const*) &a));
    } else if (sizeof(StateRec) == sizeof(int)) {
        boost::hash<int> hasher;
        return hasher(*((int const*) &a));
    } else if (sizeof(StateRec) == sizeof(long)) {
        boost::hash<long> hasher;
        return hasher(*((long const*) &a));
    } else if (sizeof(StateRec) == sizeof(long long)) {
        boost::hash<long long> hasher;
        return hasher(*((long long const*) &a));
    } else {
        // otherwise hash in chunks of size int
        size_t h = 1;
        boost::hash<int> hasher_int;
        boost::hash<char> hasher_char;

        size_t i = 0;
        for (; i < sizeof(StateRec)/sizeof(int); ++i)
            h = 31 * h + hasher_int(((int*) &a)[i]);
        i*=sizeof(int);
        for (; i < sizeof(StateRec); ++i)
            h = 31 * h + hasher_char(((char*) &a)[i]);

        return h;
    }
}

bool StatePtr_Equal::operator()(StatePtr const s1, StatePtr const s2) const {
    return stateEquals(s1, s2);
}

// binary versions of write state / read state

void writeStateBin(std::ostream &o, StatePtr x, double wt) {
    o.write((char*)x, sizeof(StateRec));
    o.write((char*)&wt, sizeof(double));
}

bool readStateBin(std::istream &i, StateRec *x, double &wt) {
    i.read((char*)x, sizeof(StateRec));
    i.read((char*)&wt, sizeof(double));
    return i.good();
}

// Actions

int getActionIndexFromName(const std::string& name) {
    for (const Action *action: __actions) {
        if (action->name == name)
            return action->id;
    }
    throw std::runtime_error("Action \""+name+"\" impossible or doesn't exist!");
}

const std::string getActionNameFromIndex(int index) {
    return actions[index]->name;
}

bool tableName(std::string cmdLine, std::string stateFiles[NUM_GOALS]) {

    std::string parameter;
    int goalIndex = NUM_GOALS;
    if (!getGoalIndex(cmdLine,goalIndex,parameter)){
      return false;
    }

    if (-1 == goalIndex){
      stateFiles[0] = parameter;
    }else{
      stateFiles[goalIndex] = parameter;
    }

    return true;
}

bool getGoalIndex(std::string cmdLine, int& goalIndex, std::string& parameter){
  size_t n = std::count(cmdLine.begin(), cmdLine.end(), ':');
  if (n==0) { // must be filename
      goalIndex = -1;
      parameter = cmdLine;
      return true;
  } else if (n!=1) { // has wrong format
      std::cerr << cmdLine << " cannot be interpreted." << std::endl;
      return false;
  }

  std::vector<std::string> strs;
  boost::split(strs, cmdLine, boost::is_any_of(":"));
  int g=0;
  char *p;

  g = strtol(strs[0].c_str(), &p, 10);
  if (*p) {
      for (; g<NUM_GOALS; g++) {
          if (strs[0] == model::goalNames[g]) {
              break;
          }
      }
  }

  if (g < 0 || g >= NUM_GOALS) {
      std::cerr << "goal " << strs[0] << "(" << g << ") is out of bounds" << std::endl;
      goalIndex = NUM_GOALS;
      return false;
  }
  goalIndex = g;
  parameter = strs[1];
  return true;
}

bool getInitialStateIndex(std::string cmdLine, int& intialIndex, std::string& parameter){
  size_t n = std::count(cmdLine.begin(), cmdLine.end(), ':');
  if (n==0) { // must be filename
      intialIndex = -1;
      parameter = cmdLine;
      return true;
  } else if (n!=1) { // has wrong format
      std::cerr << cmdLine << " cannot be interpreted." << std::endl;
      return false;
  }

  std::vector<std::string> strs;
  boost::split(strs, cmdLine, boost::is_any_of(":"));
  int g=0;
  char *p;

  g = strtol(strs[0].c_str(), &p, 10);
  if (*p) {
      for (; g<NUM_INITIAL_STATES; g++) {
          if (strs[0] == model::initialStateNames[g]) {
              break;
          }
      }
  }

  if (g < 0 || g >= NUM_INITIAL_STATES) {
      std::cerr << "initial state " << strs[0] << "(" << g << ") is out of bounds" << std::endl;
      intialIndex = NUM_INITIAL_STATES;
      return false;
  }
  intialIndex = g;
  parameter = strs[1];
  return true;
}


} // namespace model
