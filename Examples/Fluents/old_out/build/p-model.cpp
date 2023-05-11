const char * DOMAIN_NAME = "fluent";
const char * PROBLEM_NAME = "fluent";

unsigned int objectUIDs_object[] = {0,1,2,3,4,5};
unsigned int objectIDs_object[] = {0,1,2,3,4,5};
unsigned int objectUIDs_num[] = {3,4,5};
unsigned int objectIDs_num[] = {UINT_MAX,UINT_MAX,UINT_MAX,0,1,2};
char const * const objnames_object[] = {"a","b","c","0","1","2"};
char const * const objnames_num[] = {"0","1","2"};

std::ostream& operator<<(std::ostream &o, StatePtr x) {
	if(x->Sbla_a) o << " (bla a)"; else o<< " (not (bla a))"; 
	o << " (f)=" << objnames_num[x->Ff];
	o << " (f' 1)=" << objnames_num[x->Ff_1];
	o << " (g 0)=" << objnames_object[x->Fg_0];
	return o;
}
void writeState(std::ostream &o, StatePtr x, double wt) {
	char buf[16 + 3*3 + 4]; char *s = (char*) buf;
	if (std::isinf(wt)) strcpy(s, "inf"), s += 3; else s += sprintf(s, "%g", wt);
	*s = ' '; s++; *s =x->Sbla_a + '0'; s++;
	*s = ' '; s++; s += sprintf(s, "%u", x->Ff);
	*s = ' '; s++; s += sprintf(s, "%u", x->Ff_1);
	*s = ' '; s++; s += sprintf(s, "%u", x->Fg_0);
	*s = '\n'; *(++s) = '\0'; o << (char*) buf;
}
bool readState(std::istream &i, StateRec *x, double &wt) {
	char buf[4096]; char *s = (char*) buf;
	do i.getline(s, 4096); while (i.gcount() < 2 && i.good());
	if (!i.good()) return false;
	if (!memcmp(s, "inf", 3)) { wt = infinity, s+=3; } else wt = strtod(s, &s);
	char c;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sbla_a = c, s++; else return false;
	int n; unsigned int v; float vf; (void) v; (void) vf;
	while (isspace(*s)) {s++;} sscanf(s, "%u%n", &v,&n); s+=n; x->Ff = v;
	while (isspace(*s)) {s++;} sscanf(s, "%u%n", &v,&n); s+=n; x->Ff_1 = v;
	while (isspace(*s)) {s++;} sscanf(s, "%u%n", &v,&n); s+=n; x->Fg_0 = v;
	return true;
}
void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr) { // rt: Recency Vector (ACT-R Heuristic); t: ist the global time
	if (prev->Sbla_a ^ curr->Sbla_a) rT[0] = t;
	if (prev->Ff != curr->Ff) rT[1] = t;
	if (prev->Ff_1 != curr->Ff_1) rT[2] = t;
	if (prev->Fg_0 != curr->Fg_0) rT[3] = t;
}
double getRefract(StatePtr a1, StatePtr a2) { // compare two action preconditions and returns the rerfractoriness 
	double r=0; int n=0;
	if (a1->Sbla_a) {n++; if (a2->Sbla_a) r++;}
	if (a1->Ff) {n++; if (a2->Ff) r++;}
	if (a1->Ff_1) {n++; if (a2->Ff_1) r++;}
	if (a1->Fg_0) {n++; if (a2->Fg_0) r++;}
	return (n==0) ? 0 : r/n;
}

char const * const initialStateNames[] = {"fluent"};
char const * const goalNames[] = {"fluent"};

// no :non-repeating has been used
bool canFollowAfter(int next, int prev) { (void)next; (void)prev; return true; }

int const FinishedOpId = -3;
int const NoOpId = -2;
int const InitOpId = -1;
char const * const __actionNames[NOPS+3] = {
	"(FINISHED)",
	"(BLOCKED)",
	"(INITIALIZE)",
	"(bla)" // (:action bla )
};
char const * const * const actionNames = &__actionNames[3];


/* (bla), id 0 */
double action_bla(StatePtr x, StateRec *x1, double *specificity) {
	if(true) *specificity=0.00; 

		*x1 = *x;
		x1->Sbla_a = 1;
		return 1;
}

bool finishedImmediate(double, double* p) { if (p)*p=1.0; return true; }
double pStopcdfImmediate(double startTime, double curTime){ return (curTime > startTime) ? 1.0 : 0.0; }
double pStoppdfImmediate(double startTime, double curTime){ return (curTime > startTime) ? 1.0 : 0.0; }

// Implementations for the Refractoriness and Recency (ACT-R Heuristics)
// The first paramter is the current state and the second one is the state for the Refractoriness
double getRecOrRefr0 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	(void) rT; (void) t;
	if (r != NULL) {
		
	} else 
		 return (0)/1;
	return -1;
}


namespace ActionSchemes {
const ActionScheme s_FINISHED("FINISHED", std::vector<paramType>());
const ActionScheme s_BLOCKED("BLOCKED", std::vector<paramType>());
const ActionScheme s_INITIALIZE("INITIALIZE", std::vector<paramType>());
boost::array<paramType,0> __params_bla = {};
const ActionScheme s_bla("bla", std::vector<paramType>(__params_bla.begin(), __params_bla.end()));
} // namespace ActionSchemes

ActionScheme const * __actionSchemes[1 + 3] = {
	&ActionSchemes::s_FINISHED,
	&ActionSchemes::s_BLOCKED,
	&ActionSchemes::s_INITIALIZE,
	&ActionSchemes::s_bla
};
ActionScheme const * *actionSchemes = &__actionSchemes[3];

namespace Actions {
const ActionSchemes::S_FINISHED a_FINISHED(FinishedOpId, "FINISHED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_BLOCKED a_BLOCKED(NoOpId, "BLOCKED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_INITIALIZE a_INITIALIZE(InitOpId, "INITIALIZE", NULL, NULL, NULL, finishedImmediate , pStopcdfImmediate, pStoppdfImmediate, 0.0 /* Initial */);
const ActionSchemes::S_bla a_bla(0, "(bla)", action_bla, NULL, getRecOrRefr0, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 0 */);
} // namespace Actions

Action const * __actions[NOPS+3] = {
	&Actions::a_FINISHED,
	&Actions::a_BLOCKED,
	&Actions::a_INITIALIZE,
	&Actions::a_bla
};
Action const * *actions = &__actions[3];

const int agentOps0[1] = {0};
char const * const agentNames[NAGENTS] = {"NULL"};
int const nOpsForAgent[NAGENTS] = {1};
int const * const opsForAgent[NAGENTS] = {agentOps0};

bool isGoalState(size_t goal, StatePtr x) { (void) x;
	switch(goal) {
	case 0: /*fluent*/
		return (x->Ff==1 && x->Ff==x->Ff_1 && x->Fg_0==1 && x->Sbla_a);
	default: throw std::runtime_error("isGoalState: Unknown goal"); break;
	}
}

void sampleInitial(size_t initialState, StateRec *x1) {
	bzero(x1,sizeof(StateRec));
	switch(initialState) {
	case 0: /*fluent*/
		x1->Sbla_a = 1;
		x1->Ff = 0;
		x1->Ff_1 = 0;
		x1->Fg_0 = 0;
		break;
	default: throw std::runtime_error("sampleInitial: Unknown initial state"); break;
	}
}

void stateObservation(StatePtr) { }


int landmarkCountHeuristic(size_t initialState, size_t goal, StatePtr s, StateRec *parentAccepted) {
	(void) s;
	StateRec accepted;
	int count = 0;
	switch(initialState) {
	case 0: /*fluent*/
		switch(goal) {
		case 0: /*fluent*/
			if (parentAccepted == 0) {
				//accepted = StateRec();accepted.S1=1;
				accepted.Sbla_a=1;
			} else {
				accepted= *parentAccepted;
		
				if (s->Sbla_a==1) accepted.Sbla_a=1;
			} // compute the heuristic
			if (accepted.Sbla_a== 0) count++;
				if (accepted.Sbla_a== 1 && s->Sbla_a== 0) count++;
			break;
		default: throw std::runtime_error("landmarkCountHeuristic: Unknown goal state");
		}
		break;
	default: throw std::runtime_error("landmarkCountHeuristic: Unknown initial state");
	}
	*parentAccepted = accepted;
	return count;
}
