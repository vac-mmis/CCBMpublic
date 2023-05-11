char data[20];


const int NSense = 6;

int sensorState[NSense];
int sensorObs[NSense];

const int NLoc = 7;
const int NAgnt = 2;

int agentLocs[NAgnt][NLoc];

int otime = 0;

void *fetchObservation() {
  if(otime == 0)  cin.ignore(100000,'\n'); // skip first line

  for(int i=0;i<NSense;i++)
    if(!(cin >> sensorObs[i])) return NULL;

  cin.ignore(100000,'\n');

  /*
  cout << otime << " : ";
  for(int i=0;i<NSense;i++)
    cout << sensorObs[i] << ' ';
    cout << endl; */

  otime++;
  return (void *)true;
}

void signalExpectedFromSensor(int sid) {
  sensorState[sid] = 1;
}

void setAgentLoc(int agent, int locid) { agentLocs[agent][locid] = 1; }

double observe(StatePtr x) {
  for(int i=0;i<NSense;i++) sensorState[i] = 0;
  stateObservation(x);

  for(int i=0;i<NSense;i++)
    if(sensorState[i] != sensorObs[i]) return 0;
  //clog << "[" << data << "]" << endl;
  return 1;
}

#define HAS_ESTIMATOR

class Estimate {
  double time;
  double loglik;
public:
  void start(double _time) {
    time = _time;
    loglik = theLoglik();
  }
  void collect(StatePtr, double) {}
  void finish() {}
  std::ostream &print(std::ostream &os) {
    os << time << ' ' << loglik << endl;
    return os;
  }
};

