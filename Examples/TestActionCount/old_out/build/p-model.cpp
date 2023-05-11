const char * DOMAIN_NAME = "test";
const char * PROBLEM_NAME = "test";

char const * const objnames_object[] = {"a","b"};
char const * const objnames_task[] = {"a","b"};

std::ostream& operator<<(std::ostream &o, StatePtr x) {
	if(x->Sdone_a) o << " (done a)"; else o<< " (not (done a))"; 
	if(x->Sdone_b) o << " (done b)"; else o<< " (not (done b))"; 
	return o;
}
void writeState(std::ostream &o, StatePtr x, double wt) {
	char buf[16 + 3*0 + 0]; char *s = (char*) buf;
	if (std::isinf(wt)) strcpy(s, "inf"), s += 3; else s += sprintf(s, "%g", wt);
	*s = ' '; s++; *s =x->Sdone_a + '0'; s++;
	*s = ' '; s++; *s =x->Sdone_b + '0'; s++;
	*s = '\n'; *(++s) = '\0'; o << (char*) buf;
}
bool readState(std::istream &i, StateRec *x, double &wt) {
	char buf[4096]; char *s = (char*) buf;
	do i.getline(s, 4096); while (i.gcount() < 2 && i.good());
	if (!i.good()) return false;
	if (!memcmp(s, "inf", 3)) { wt = infinity, s+=3; } else wt = strtod(s, &s);
	char c;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sdone_a = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sdone_b = c, s++; else return false;
	return true;
}
void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr) { // rt: Recency Vector (ACT-R Heuristic); t: ist the global time
	if (prev->Sdone_a ^ curr->Sdone_a) rT[0] = t;
	if (prev->Sdone_b ^ curr->Sdone_b) rT[1] = t;
}
double getRefract(StatePtr a1, StatePtr a2) { // compare two action preconditions and returns the rerfractoriness 
	double r=0; int n=0;
	if (a1->Sdone_a) {n++; if (a2->Sdone_a) r++;}
	if (a1->Sdone_b) {n++; if (a2->Sdone_b) r++;}
	return (n==0) ? 0 : r/n;
}

char const * const initialStateNames[] = {"test"};
char const * const goalNames[] = {"test"};

// no :non-repeating has been used
bool canFollowAfter(int next, int prev) { (void)next; (void)prev; return true; }

int const FinishedOpId = -3;
int const NoOpId = -2;
int const InitOpId = -1;
char const * const __actionNames[NOPS+3] = {
	"(FINISHED)",
	"(BLOCKED)",
	"(INITIALIZE)",
	"(do a)", // (:action do ?t - task)
	"(do b)", // (:action do ?t - task)
	"(strange-b)" // (:action strange-b )
};
char const * const * const actionNames = &__actionNames[3];


/* (do a), id 0 */
double action_do_a(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sdone_a)) *specificity=1.00; 

	if(!(x->Sdone_a)) {
		*x1 = *x;
		x1->Sdone_a = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (do b), id 1 */
double action_do_b(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sdone_b)) *specificity=0.50; 

	if(!(x->Sdone_b)) {
		*x1 = *x;
		x1->Sdone_b = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (strange-b), id 2 */
double action_strange_b(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sdone_a && !(x->Sdone_b))) *specificity=1.50; 

	if((x->Sdone_a && !(x->Sdone_b))) {
		*x1 = *x;
		x1->Sdone_b = 1;
		return 1;
	} else {
		return 0;
	}
}

bool finishedImmediate(double, double* p) { if (p)*p=1.0; return true; }
double pStopcdfImmediate(double startTime, double curTime){ return (curTime > startTime) ? 1.0 : 0.0; }
double pStoppdfImmediate(double startTime, double curTime){ return (curTime > startTime) ? 1.0 : 0.0; }

// Implementations for the Refractoriness and Recency (ACT-R Heuristics)
// The first paramter is the current state and the second one is the state for the Refractoriness
double getRecOrRefr0 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sdone_a=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr1 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sdone_b=1; 
	} else 
		 return (exp(rT[1]-t))/1;
	return -1;
}
double getRecOrRefr2 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sdone_a=1; r->Sdone_b=1; 
	} else 
		 return (exp(rT[0]-t)+exp(rT[1]-t))/2;
	return -1;
}


namespace ActionSchemes {
const ActionScheme s_FINISHED("FINISHED", std::vector<paramType>());
const ActionScheme s_BLOCKED("BLOCKED", std::vector<paramType>());
const ActionScheme s_INITIALIZE("INITIALIZE", std::vector<paramType>());
boost::array<paramType,1> __params_do = {std::make_pair("t","task")};
const ActionScheme s_do("do", std::vector<paramType>(__params_do.begin(), __params_do.end()));
boost::array<paramType,0> __params_strange_b = {};
const ActionScheme s_strange_b("strange-b", std::vector<paramType>(__params_strange_b.begin(), __params_strange_b.end()));
} // namespace ActionSchemes

ActionScheme const * __actionSchemes[2 + 3] = {
	&ActionSchemes::s_FINISHED,
	&ActionSchemes::s_BLOCKED,
	&ActionSchemes::s_INITIALIZE,
	&ActionSchemes::s_do,
	&ActionSchemes::s_strange_b
};
ActionScheme const * *actionSchemes = &__actionSchemes[3];

namespace Actions {
const ActionSchemes::S_FINISHED a_FINISHED(FinishedOpId, "FINISHED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_BLOCKED a_BLOCKED(NoOpId, "BLOCKED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_INITIALIZE a_INITIALIZE(InitOpId, "INITIALIZE", NULL, NULL, NULL, finishedImmediate , pStopcdfImmediate, pStoppdfImmediate, 0.0 /* Initial */);
const ActionSchemes::S_do a_do_a(0, "(do a)", action_do_a, NULL, getRecOrRefr0, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 0 */);
const ActionSchemes::S_do a_do_b(1, "(do b)", action_do_b, NULL, getRecOrRefr1, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 1 */);
const ActionSchemes::S_strange_b a_strange_b(2, "(strange-b)", action_strange_b, NULL, getRecOrRefr2, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 2 */);
} // namespace Actions

Action const * __actions[NOPS+3] = {
	&Actions::a_FINISHED,
	&Actions::a_BLOCKED,
	&Actions::a_INITIALIZE,
	&Actions::a_do_a,
	&Actions::a_do_b,
	&Actions::a_strange_b
};
Action const * *actions = &__actions[3];

const int agentOps0[3] = {0,1,2};
char const * const agentNames[NAGENTS] = {"NULL"};
int const nOpsForAgent[NAGENTS] = {3};
int const * const opsForAgent[NAGENTS] = {agentOps0};

bool isGoalState(size_t goal, StatePtr x) { (void) x;
	switch(goal) {
	case 0: /*test*/
		return (x->Sdone_a && x->Sdone_b);
	default: throw std::runtime_error("isGoalState: Unknown goal"); break;
	}
}

void sampleInitial(size_t initialState, StateRec *x1) {
	bzero(x1,sizeof(StateRec));
	switch(initialState) {
	case 0: /*test*/
		x1->Sdone_a = 0;
		x1->Sdone_b = 0;
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
	case 0: /*test*/
		switch(goal) {
		case 0: /*test*/
			if (parentAccepted == 0) {
				//accepted = StateRec();accepted.S1=1;
			} else {
				accepted= *parentAccepted;
		
				if (s->Sdone_a==1) accepted.Sdone_a=1;
				if (s->Sdone_b==1) accepted.Sdone_b=1;
			} // compute the heuristic
			if (accepted.Sdone_a== 0) count++;
			if (accepted.Sdone_b== 0) count++;
				if (accepted.Sdone_a== 1 && s->Sdone_a== 0) count++;
				if (accepted.Sdone_b== 1 && s->Sdone_b== 0) count++;
			break;
		default: throw std::runtime_error("landmarkCountHeuristic: Unknown goal state");
		}
		break;
	default: throw std::runtime_error("landmarkCountHeuristic: Unknown initial state");
	}
	*parentAccepted = accepted;
	return count;
}
