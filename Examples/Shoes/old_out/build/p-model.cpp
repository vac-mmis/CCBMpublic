const char * DOMAIN_NAME = "shoes";
const char * PROBLEM_NAME = "shoes";

char const * const objnames_object[] = {"left","right"};
char const * const objnames_foot[] = {"left","right"};

std::ostream& operator<<(std::ostream &o, StatePtr x) {
	if(x->ShasSock_right) o << " (hasSock right)"; else o<< " (not (hasSock right))"; 
	if(x->ShasSock_left) o << " (hasSock left)"; else o<< " (not (hasSock left))"; 
	if(x->ShasShoe_right) o << " (hasShoe right)"; else o<< " (not (hasShoe right))"; 
	if(x->ShasShoe_left) o << " (hasShoe left)"; else o<< " (not (hasShoe left))"; 
	return o;
}
void writeState(std::ostream &o, StatePtr x, double wt) {
	char buf[16 + 3*0 + 0]; char *s = (char*) buf;
	if (std::isinf(wt)) strcpy(s, "inf"), s += 3; else s += sprintf(s, "%g", wt);
	*s = ' '; s++; *s =x->ShasSock_right + '0'; s++;
	*s = ' '; s++; *s =x->ShasSock_left + '0'; s++;
	*s = ' '; s++; *s =x->ShasShoe_right + '0'; s++;
	*s = ' '; s++; *s =x->ShasShoe_left + '0'; s++;
	*s = '\n'; *(++s) = '\0'; o << (char*) buf;
}
bool readState(std::istream &i, StateRec *x, double &wt) {
	char buf[4096]; char *s = (char*) buf;
	do i.getline(s, 4096); while (i.gcount() < 2 && i.good());
	if (!i.good()) return false;
	if (!memcmp(s, "inf", 3)) { wt = infinity, s+=3; } else wt = strtod(s, &s);
	char c;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->ShasSock_right = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->ShasSock_left = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->ShasShoe_right = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->ShasShoe_left = c, s++; else return false;
	return true;
}
void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr) { // rt: Recency Vector (ACT-R Heuristic); t: ist the global time
	if (prev->ShasSock_right ^ curr->ShasSock_right) rT[0] = t;
	if (prev->ShasSock_left ^ curr->ShasSock_left) rT[1] = t;
	if (prev->ShasShoe_right ^ curr->ShasShoe_right) rT[2] = t;
	if (prev->ShasShoe_left ^ curr->ShasShoe_left) rT[3] = t;
}
double getRefract(StatePtr a1, StatePtr a2) { // compare two action preconditions and returns the rerfractoriness 
	double r=0; int n=0;
	if (a1->ShasSock_right) {n++; if (a2->ShasSock_right) r++;}
	if (a1->ShasSock_left) {n++; if (a2->ShasSock_left) r++;}
	if (a1->ShasShoe_right) {n++; if (a2->ShasShoe_right) r++;}
	if (a1->ShasShoe_left) {n++; if (a2->ShasShoe_left) r++;}
	return (n==0) ? 0 : r/n;
}

char const * const initialStateNames[] = {"shoes"};
char const * const goalNames[] = {"shoes"};

// no :non-repeating has been used
bool canFollowAfter(int next, int prev) { (void)next; (void)prev; return true; }

int const FinishedOpId = -3;
int const NoOpId = -2;
int const InitOpId = -1;
char const * const __actionNames[NOPS+3] = {
	"(FINISHED)",
	"(BLOCKED)",
	"(INITIALIZE)",
	"(putOnSock right)", // (:action putOnSock ?f - foot)
	"(putOnSock left)", // (:action putOnSock ?f - foot)
	"(putOnShoe right)", // (:action putOnShoe ?f - foot)
	"(putOnShoe left)" // (:action putOnShoe ?f - foot)
};
char const * const * const actionNames = &__actionNames[3];


/* (putOnSock right), id 0 */
double action_putOnSock_right(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->ShasSock_right)) *specificity=1.00; 

	if(!(x->ShasSock_right)) {
		*x1 = *x;
		x1->ShasSock_right = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (putOnSock left), id 1 */
double action_putOnSock_left(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->ShasSock_left)) *specificity=1.00; 

	if(!(x->ShasSock_left)) {
		*x1 = *x;
		x1->ShasSock_left = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (putOnShoe right), id 2 */
double action_putOnShoe_right(StatePtr x, StateRec *x1, double *specificity) {
	if((x->ShasSock_right && !(x->ShasShoe_right))) *specificity=2.00; 

	if((x->ShasSock_right && !(x->ShasShoe_right))) {
		*x1 = *x;
		x1->ShasShoe_right = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (putOnShoe left), id 3 */
double action_putOnShoe_left(StatePtr x, StateRec *x1, double *specificity) {
	if((x->ShasSock_left && !(x->ShasShoe_left))) *specificity=2.00; 

	if((x->ShasSock_left && !(x->ShasShoe_left))) {
		*x1 = *x;
		x1->ShasShoe_left = 1;
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
		r->ShasSock_right=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr1 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->ShasSock_left=1; 
	} else 
		 return (exp(rT[1]-t))/1;
	return -1;
}
double getRecOrRefr2 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->ShasSock_right=1; r->ShasShoe_right=1; 
	} else 
		 return (exp(rT[0]-t)+exp(rT[2]-t))/2;
	return -1;
}
double getRecOrRefr3 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->ShasSock_left=1; r->ShasShoe_left=1; 
	} else 
		 return (exp(rT[1]-t)+exp(rT[3]-t))/2;
	return -1;
}

/*** BEGIN CALLBACKS ***/
void callback_putOnSock_right(StatePtr x, int _agid) { (void) x; setSide(_agid,right);}
void callback_putOnSock_left(StatePtr x, int _agid) { (void) x; setSide(_agid,left);}
void callback_putOnShoe_right(StatePtr x, int _agid) { (void) x; setSide(_agid,right);}
void callback_putOnShoe_left(StatePtr x, int _agid) { (void) x; setSide(_agid,left);}
/*** END CALLBACKS ****/

namespace ActionSchemes {
const ActionScheme s_FINISHED("FINISHED", std::vector<paramType>());
const ActionScheme s_BLOCKED("BLOCKED", std::vector<paramType>());
const ActionScheme s_INITIALIZE("INITIALIZE", std::vector<paramType>());
boost::array<paramType,1> __params_putOnSock = {std::make_pair("f","foot")};
const ActionScheme s_putOnSock("putOnSock", std::vector<paramType>(__params_putOnSock.begin(), __params_putOnSock.end()));
boost::array<paramType,1> __params_putOnShoe = {std::make_pair("f","foot")};
const ActionScheme s_putOnShoe("putOnShoe", std::vector<paramType>(__params_putOnShoe.begin(), __params_putOnShoe.end()));
} // namespace ActionSchemes

ActionScheme const * __actionSchemes[2 + 3] = {
	&ActionSchemes::s_FINISHED,
	&ActionSchemes::s_BLOCKED,
	&ActionSchemes::s_INITIALIZE,
	&ActionSchemes::s_putOnSock,
	&ActionSchemes::s_putOnShoe
};
ActionScheme const * *actionSchemes = &__actionSchemes[3];

namespace Actions {
const ActionSchemes::S_FINISHED a_FINISHED(FinishedOpId, "FINISHED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_BLOCKED a_BLOCKED(NoOpId, "BLOCKED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_INITIALIZE a_INITIALIZE(InitOpId, "INITIALIZE", NULL, NULL, NULL, finishedImmediate , pStopcdfImmediate, pStoppdfImmediate, 0.0 /* Initial */);
const ActionSchemes::S_putOnSock a_putOnSock_right(0, "(putOnSock right)", action_putOnSock_right, callback_putOnSock_right, getRecOrRefr0, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 0 */);
const ActionSchemes::S_putOnSock a_putOnSock_left(1, "(putOnSock left)", action_putOnSock_left, callback_putOnSock_left, getRecOrRefr1, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 1 */);
const ActionSchemes::S_putOnShoe a_putOnShoe_right(2, "(putOnShoe right)", action_putOnShoe_right, callback_putOnShoe_right, getRecOrRefr2, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 2 */);
const ActionSchemes::S_putOnShoe a_putOnShoe_left(3, "(putOnShoe left)", action_putOnShoe_left, callback_putOnShoe_left, getRecOrRefr3, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 3 */);
} // namespace Actions

Action const * __actions[NOPS+3] = {
	&Actions::a_FINISHED,
	&Actions::a_BLOCKED,
	&Actions::a_INITIALIZE,
	&Actions::a_putOnSock_right,
	&Actions::a_putOnSock_left,
	&Actions::a_putOnShoe_right,
	&Actions::a_putOnShoe_left
};
Action const * *actions = &__actions[3];

const int agentOps0[4] = {0,1,2,3};
char const * const agentNames[NAGENTS] = {"NULL"};
int const nOpsForAgent[NAGENTS] = {4};
int const * const opsForAgent[NAGENTS] = {agentOps0};

bool isGoalState(size_t goal, StatePtr x) { (void) x;
	switch(goal) {
	case 0: /*shoes*/
		return (x->ShasShoe_left && x->ShasShoe_right);
	default: throw std::runtime_error("isGoalState: Unknown goal"); break;
	}
}

void sampleInitial(size_t initialState, StateRec *x1) {
	bzero(x1,sizeof(StateRec));
	switch(initialState) {
	case 0: /*shoes*/
		x1->ShasSock_right = 0;
		x1->ShasSock_left = 0;
		x1->ShasShoe_right = 0;
		x1->ShasShoe_left = 0;
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
	case 0: /*shoes*/
		switch(goal) {
		case 0: /*shoes*/
			if (parentAccepted == 0) {
				//accepted = StateRec();accepted.S1=1;
			} else {
				accepted= *parentAccepted;
		
				if (s->ShasSock_right==1) accepted.ShasSock_right=1;
				if (s->ShasSock_left==1) accepted.ShasSock_left=1;
				if (s->ShasShoe_left==1) if (parentAccepted->ShasSock_left==1)  accepted.ShasShoe_left=1;
				if (s->ShasShoe_right==1) if (parentAccepted->ShasSock_right==1)  accepted.ShasShoe_right=1;
			} // compute the heuristic
			if (accepted.ShasSock_right== 0) count++;
			if (accepted.ShasSock_left== 0) count++;
			if (accepted.ShasShoe_left== 0) count++;
			if (accepted.ShasShoe_right== 0) count++;
				if (accepted.ShasShoe_left== 1 && s->ShasShoe_left== 0) count++;
				if (accepted.ShasShoe_right== 1 && s->ShasShoe_right== 0) count++;
			if (accepted.ShasSock_right== 1 && s->ShasSock_right== 0  && (accepted.ShasShoe_right==0)) count++;
			if (accepted.ShasSock_left== 1 && s->ShasSock_left== 0  && (accepted.ShasShoe_left==0)) count++;
			break;
		default: throw std::runtime_error("landmarkCountHeuristic: Unknown goal state");
		}
		break;
	default: throw std::runtime_error("landmarkCountHeuristic: Unknown initial state");
	}
	*parentAccepted = accepted;
	return count;
}
