#!/bin/sh

DOMAINF=`mktemp /tmp/xyrouter-XXXXXX` || exit 1
PROBLEMF=Models/xyrouterp.pddl
OBSERVEF=`mktemp /tmp/xyrouter-XXXXXX` || exit 1
DATAF=Data/r218-ur-xy.dat

cat <<EOF >$DOMAINF
(define (domain xyrouter)
	(:types position)
	(:action step
		 :parameters (?x ?y - position)
;		 :saliency (x-pos ?y)
		 :agent TheAgent
		 :duration (exponential 0.1)
		 :precondition (and (connected ?x ?y) (at ?x))
		 :effect (and (not (at ?x)) (at ?y))
		 :callbacks (and (when (at ?y) (frobboz ?x ?y))
				 (foobar (y-pos ?y))))
	(:callbacks (forall (?l - position)
			      (when (at ?l)
				(setXYcoordinates (x-pos ?l) (y-pos ?l))))))
EOF

cat <<EOF >$OBSERVEF
#include <math.h>

const double itopi = 1/sqrt(2*M_PI);

double gaussian(double x, double mu, double sd) {
  double dd = x-mu;

  return (itopi/sd)*exp(-dd*dd/(2*sd*sd));
}

#define HAS_CALLBACKS

extern char const * const * const actionNames;

// required action, called for special ops
void specialOpCallback(int _agid, int _opid) {
  clog << _agid << " Special: " << actionNames[_opid] << endl;
}

void frobboz(int _agid, int x, int y) {
  clog << _agid << " Frobboz: " << x << ' ' << y << endl;
}

void foobar(int _agid, double what) {
  clog << _agid << " Foobar: " << what << endl;
}

//////////////////////////////////////////////////////////////////////


typedef struct {
  double x, y;
} Observation;

static Observation theObservation = {3,3};
static Observation theState;


void * fetchObservation() {
  return (cin >> theObservation.x >> theObservation.y);
}


void setXYcoordinates(double x, double y) {
  theState.x = x; theState.y = y;
}

double observe(StatePtr x) {
  stateObservation(x);
  return gaussian(theObservation.x,theState.x,1) * gaussian(theObservation.y,theState.y,1);
}


//////////////////////////////////////////////////////////////////////
// More or less fancy estimation stuff ...

#define HAS_ESTIMATOR

#define CELLS_NX 5
#define CELLS_NY 5

class Estimate {
  static int collection;
  double time;
  double cells[CELLS_NX][CELLS_NY];
  double estX, estY;
public:
  void start(double _time) {
    time = _time;
    estX = estY = 0;
    
    for(int i=0;i<CELLS_NX; i++)
      for(int j=0;j<CELLS_NY;j++)
	cells[i][j] = 0;
  }
  
  void collect(StatePtr x, double w) {
    int xbin, ybin;
    
    stateObservation(x);
    
    xbin = floor(theState.x / 1.4);
    ybin = floor(theState.y / 1.4);
    cells[xbin][ybin] += w;
    estX += theState.x * w;
    estY += theState.y * w;
  }
  
  void finish() { collection++; }
  
  std::ostream &print(std::ostream &os) {
    os << time << ' ' << estX << ' ' << estY;
    
    for(int i=0;i<CELLS_NX; i++)
      for(int j=0;j<CELLS_NY;j++)
	os << ' ' << cells[i][j];
    os << endl;
    return os;
  }
};
int Estimate::collection = 0;
EOF

TOOLPFX=`basename $PROBLEMF .pddl`

rc -d $DOMAINF -p $PROBLEMF -o $OBSERVEF -n 10 -w /tmp

rm -f ${OBSERVEF} /tmp/${TOOLPFX}-actions.h /tmp/${TOOLPFX}-model.h /tmp/___model.h

FILTER=${TOOLPFX}-filter
ANALYZER=${TOOLPFX}-analyzer

./${ANALYZER} -o xyrouter.states
./${FILTER} -l xyrouter.states <${DATAF} >/dev/null
./${FILTER} <${DATAF} >/dev/null

rm -f ${ANALYZER} ${FILTER} ${DOMAINF}
