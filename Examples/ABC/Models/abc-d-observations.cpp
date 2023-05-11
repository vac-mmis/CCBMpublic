char data[20];

const char *agtNames[] = {"alice","bob"};
const char *locNames[] = {"door","printer","coffee-machine","paper-stack","water-tap","coffee-jar","outside"};

const int NSense = 6;

int sensorState[NSense];
int sensorObs[NSense];

int agentLocs[NAgnt][NLoc];

int otime = 0;

bool fetchObservation() {
  if(otime == 0)
    std::cin.ignore(100000,'\n'); // skip first line

  for(int i=0;i<NSense;i++)
    if(!(std::cin >> sensorObs[i]))
        return false;

  std::cin.ignore(100000,'\n');

  /*
  cout << otime << " : ";
  for(int i=0;i<NSense;i++)
    cout << sensorObs[i] << ' ';
    cout << endl; */

  otime++;
  return true;
}

void signalExpectedFromSensor(int sid) {
  sensorState[sid] = 1;
}

void setAgentLoc(int agent, int locid) {
    agentLocs[agent][locid] = 1;
}

void pre_observe_action(size_t /*agid*/) { }

void pre_observe_state(StatePtr /*x*/) {
  for(int i=0;i<NSense;i++)
    sensorState[i] = 0;
}

Probability post_observe_action(size_t /*agid*/) { return 1; }

Probability post_observe_state(StatePtr /*x*/) {
  for(int i=0;i<NSense;i++)
    if(sensorState[i] != sensorObs[i]) return ZeroProbability;
  //clog << "[" << data << "]" << endl;
  return Probability(1);
}

#if SUPPORTS_OBS_EQUIV
bool observationEquivalent(const StateRec &s1, const Model::opid_t &opid1, const StateRec &s2, const Model::opid_t &opid2) {
    // states can be distinguished if the agents are at different positions
    // as a first approximation, simply say different states can be distinguished
    if (!StateRec_Equal()(s1, s2))
        return false;

    (void) opid1;
    (void) opid2;
    // different actions cannot be distinguished, we have only state observations
    return true;
}
#endif

#if SUPPORTS_OBS_HASH
size_t observation_hash_value(const StateRec &s, const Model::opid_t &opid) {
    // each model state is different, the opid does not influence observation_equivalence
    (void) opid;
    return StateRec_Hash()(s);
}
#endif
