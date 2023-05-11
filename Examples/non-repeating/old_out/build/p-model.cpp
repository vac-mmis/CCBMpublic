const char * DOMAIN_NAME = "non-repeating-test";
const char * PROBLEM_NAME = "from-l1-to-l2";

char const * const objnames_object[] = {"l1","l2","revolving-door","p1","p2"};
char const * const objnames_person[] = {"p1","p2"};
char const * const objnames_location[] = {"l1","l2","revolving-door"};

std::ostream& operator<<(std::ostream &o, StatePtr x) {
	if(x->Sat_p1_l1) o << " (at p1 l1)"; else o<< " (not (at p1 l1))"; 
	if(x->Sat_p1_l2) o << " (at p1 l2)"; else o<< " (not (at p1 l2))"; 
	if(x->Sat_p2_l1) o << " (at p2 l1)"; else o<< " (not (at p2 l1))"; 
	if(x->Sat_p2_l2) o << " (at p2 l2)"; else o<< " (not (at p2 l2))"; 
	if(x->Swaited_at_p1_l2) o << " (waited-at p1 l2)"; else o<< " (not (waited-at p1 l2))"; 
	if(x->Swaited_at_p2_l1) o << " (waited-at p2 l1)"; else o<< " (not (waited-at p2 l1))"; 
	return o;
}
void writeState(std::ostream &o, StatePtr x, double wt) {
	char buf[16 + 3*0 + 0]; char *s = (char*) buf;
	if (std::isinf(wt)) strcpy(s, "inf"), s += 3; else s += sprintf(s, "%g", wt);
	*s = ' '; s++; *s =x->Sat_p1_l1 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_p1_l2 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_p2_l1 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_p2_l2 + '0'; s++;
	*s = ' '; s++; *s =x->Swaited_at_p1_l2 + '0'; s++;
	*s = ' '; s++; *s =x->Swaited_at_p2_l1 + '0'; s++;
	*s = '\n'; *(++s) = '\0'; o << (char*) buf;
}
bool readState(std::istream &i, StateRec *x, double &wt) {
	char buf[4096]; char *s = (char*) buf;
	do i.getline(s, 4096); while (i.gcount() < 2 && i.good());
	if (!i.good()) return false;
	if (!memcmp(s, "inf", 3)) { wt = infinity, s+=3; } else wt = strtod(s, &s);
	char c;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_p1_l1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_p1_l2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_p2_l1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_p2_l2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Swaited_at_p1_l2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Swaited_at_p2_l1 = c, s++; else return false;
	return true;
}
void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr) { // rt: Recency Vector (ACT-R Heuristic); t: ist the global time
	if (prev->Sat_p1_l1 ^ curr->Sat_p1_l1) rT[0] = t;
	if (prev->Sat_p1_l2 ^ curr->Sat_p1_l2) rT[1] = t;
	if (prev->Sat_p2_l1 ^ curr->Sat_p2_l1) rT[2] = t;
	if (prev->Sat_p2_l2 ^ curr->Sat_p2_l2) rT[3] = t;
	if (prev->Swaited_at_p1_l2 ^ curr->Swaited_at_p1_l2) rT[4] = t;
	if (prev->Swaited_at_p2_l1 ^ curr->Swaited_at_p2_l1) rT[5] = t;
}
double getRefract(StatePtr a1, StatePtr a2) { // compare two action preconditions and returns the rerfractoriness 
	double r=0; int n=0;
	if (a1->Sat_p1_l1) {n++; if (a2->Sat_p1_l1) r++;}
	if (a1->Sat_p1_l2) {n++; if (a2->Sat_p1_l2) r++;}
	if (a1->Sat_p2_l1) {n++; if (a2->Sat_p2_l1) r++;}
	if (a1->Sat_p2_l2) {n++; if (a2->Sat_p2_l2) r++;}
	if (a1->Swaited_at_p1_l2) {n++; if (a2->Swaited_at_p1_l2) r++;}
	if (a1->Swaited_at_p2_l1) {n++; if (a2->Swaited_at_p2_l1) r++;}
	return (n==0) ? 0 : r/n;
}

char const * const initialStateNames[] = {"from-l1-to-l2"};
char const * const goalNames[] = {"from-l1-to-l2"};

// association from action id to bindingIndex for canFollowAfter
int const nonRepeatBindIndex[NOPS] = {
	1,0,2,3
};
// canFollowAfterT[a][b] is true if action with bindIndex a may follow after action with bindIndex b
bool canFollowAfterT[4][4] = {
	{false,true,true,true} /* 0 */
,	{true,false,true,true} /* 1 */
,	{true,true,false,true} /* 2 */
,	{true,true,true,false} /* 3 */
};

// function for accessing canFollowAfterT with action id
bool canFollowAfter(int next, int prev) {
	if (0 > next || 0 > prev) { return true; }
	return canFollowAfterT[nonRepeatBindIndex[next]][nonRepeatBindIndex[prev]];
}

int const FinishedOpId = -3;
int const NoOpId = -2;
int const InitOpId = -1;
char const * const __actionNames[NOPS+3] = {
	"(FINISHED)",
	"(BLOCKED)",
	"(INITIALIZE)",
	"(go-to p1 l2)", // (:action go-to ?p - person ?l - location)
	"(go-to p2 l1)", // (:action go-to ?p - person ?l - location)
	"(wait p1 l2)", // (:action wait ?p - person ?l - location)
	"(wait p2 l1)" // (:action wait ?p - person ?l - location)
};
char const * const * const actionNames = &__actionNames[3];


/* (go-to p1 l2), id 0 */
double action_go_to_p1_l2(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sat_p1_l2)) *specificity=1.00; 

	if(!(x->Sat_p1_l2)) {
		*x1 = *x;
		x1->Sat_p1_l2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (go-to p2 l1), id 1 */
double action_go_to_p2_l1(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sat_p2_l1)) *specificity=1.00; 

	if(!(x->Sat_p2_l1)) {
		*x1 = *x;
		x1->Sat_p2_l1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (wait p1 l2), id 2 */
double action_wait_p1_l2(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_p1_l2) *specificity=1.00; 

	if(x->Sat_p1_l2) {
		*x1 = *x;
		x1->Swaited_at_p1_l2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (wait p2 l1), id 3 */
double action_wait_p2_l1(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_p2_l1) *specificity=1.00; 

	if(x->Sat_p2_l1) {
		*x1 = *x;
		x1->Swaited_at_p2_l1 = 1;
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
		r->Sat_p1_l2=1; 
	} else 
		 return (exp(rT[1]-t))/1;
	return -1;
}
double getRecOrRefr1 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_p2_l1=1; 
	} else 
		 return (exp(rT[2]-t))/1;
	return -1;
}
double getRecOrRefr2 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_p1_l2=1; 
	} else 
		 return (exp(rT[1]-t))/1;
	return -1;
}
double getRecOrRefr3 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_p2_l1=1; 
	} else 
		 return (exp(rT[2]-t))/1;
	return -1;
}


namespace ActionSchemes {
const ActionScheme s_FINISHED("FINISHED", std::vector<paramType>());
const ActionScheme s_BLOCKED("BLOCKED", std::vector<paramType>());
const ActionScheme s_INITIALIZE("INITIALIZE", std::vector<paramType>());
boost::array<paramType,2> __params_go_to = {std::make_pair("p","person"),std::make_pair("l","location")};
const ActionScheme s_go_to("go-to", std::vector<paramType>(__params_go_to.begin(), __params_go_to.end()));
boost::array<paramType,2> __params_wait = {std::make_pair("p","person"),std::make_pair("l","location")};
const ActionScheme s_wait("wait", std::vector<paramType>(__params_wait.begin(), __params_wait.end()));
} // namespace ActionSchemes

ActionScheme const * __actionSchemes[2 + 3] = {
	&ActionSchemes::s_FINISHED,
	&ActionSchemes::s_BLOCKED,
	&ActionSchemes::s_INITIALIZE,
	&ActionSchemes::s_go_to,
	&ActionSchemes::s_wait
};
ActionScheme const * *actionSchemes = &__actionSchemes[3];

namespace Actions {
const ActionSchemes::S_FINISHED a_FINISHED(FinishedOpId, "FINISHED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_BLOCKED a_BLOCKED(NoOpId, "BLOCKED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_INITIALIZE a_INITIALIZE(InitOpId, "INITIALIZE", NULL, NULL, NULL, finishedImmediate , pStopcdfImmediate, pStoppdfImmediate, 0.0 /* Initial */);
const ActionSchemes::S_go_to a_go_to_p1_l2(0, "(go-to p1 l2)", action_go_to_p1_l2, NULL, getRecOrRefr0, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 0 */);
const ActionSchemes::S_go_to a_go_to_p2_l1(1, "(go-to p2 l1)", action_go_to_p2_l1, NULL, getRecOrRefr1, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 1 */);
const ActionSchemes::S_wait a_wait_p1_l2(2, "(wait p1 l2)", action_wait_p1_l2, NULL, getRecOrRefr2, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 2 */);
const ActionSchemes::S_wait a_wait_p2_l1(3, "(wait p2 l1)", action_wait_p2_l1, NULL, getRecOrRefr3, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 3 */);
} // namespace Actions

Action const * __actions[NOPS+3] = {
	&Actions::a_FINISHED,
	&Actions::a_BLOCKED,
	&Actions::a_INITIALIZE,
	&Actions::a_go_to_p1_l2,
	&Actions::a_go_to_p2_l1,
	&Actions::a_wait_p1_l2,
	&Actions::a_wait_p2_l1
};
Action const * *actions = &__actions[3];

const int agentOps0[4] = {0,1,2,3};
char const * const agentNames[NAGENTS] = {"NULL"};
int const nOpsForAgent[NAGENTS] = {4};
int const * const opsForAgent[NAGENTS] = {agentOps0};

bool isGoalState(size_t goal, StatePtr x) { (void) x;
	switch(goal) {
	case 0: /*from-l1-to-l2*/
		return (x->Sat_p1_l2 && x->Swaited_at_p1_l2 && x->Sat_p2_l1 && x->Swaited_at_p2_l1);
	default: throw std::runtime_error("isGoalState: Unknown goal"); break;
	}
}

void sampleInitial(size_t initialState, StateRec *x1) {
	bzero(x1,sizeof(StateRec));
	switch(initialState) {
	case 0: /*from-l1-to-l2*/
		x1->Sat_p2_l2 = 1;
		x1->Sat_p1_l1 = 1;
		x1->Sat_p1_l2 = 0;
		x1->Sat_p2_l1 = 0;
		x1->Swaited_at_p1_l2 = 0;
		x1->Swaited_at_p2_l1 = 0;
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
	case 0: /*from-l1-to-l2*/
		switch(goal) {
		case 0: /*from-l1-to-l2*/
			if (parentAccepted == 0) {
				//accepted = StateRec();accepted.S1=1;
			} else {
				accepted= *parentAccepted;
		
				if (s->Sat_p1_l2==1) accepted.Sat_p1_l2=1;
				if (s->Swaited_at_p1_l2==1) if (parentAccepted->Sat_p1_l2==1)  accepted.Swaited_at_p1_l2=1;
				if (s->Sat_p2_l1==1) accepted.Sat_p2_l1=1;
				if (s->Swaited_at_p2_l1==1) if (parentAccepted->Sat_p2_l1==1)  accepted.Swaited_at_p2_l1=1;
			} // compute the heuristic
			if (accepted.Sat_p1_l2== 0) count++;
			if (accepted.Swaited_at_p1_l2== 0) count++;
			if (accepted.Sat_p2_l1== 0) count++;
			if (accepted.Swaited_at_p2_l1== 0) count++;
				if (accepted.Sat_p1_l2== 1 && s->Sat_p1_l2== 0) count++;
				if (accepted.Swaited_at_p1_l2== 1 && s->Swaited_at_p1_l2== 0) count++;
				if (accepted.Sat_p2_l1== 1 && s->Sat_p2_l1== 0) count++;
				if (accepted.Swaited_at_p2_l1== 1 && s->Swaited_at_p2_l1== 0) count++;
			if (accepted.Sat_p1_l2== 1 && s->Sat_p1_l2== 0  && (accepted.Swaited_at_p1_l2==0)) count++;
			if (accepted.Sat_p2_l1== 1 && s->Sat_p2_l1== 0  && (accepted.Swaited_at_p2_l1==0)) count++;
			break;
		default: throw std::runtime_error("landmarkCountHeuristic: Unknown goal state");
		}
		break;
	default: throw std::runtime_error("landmarkCountHeuristic: Unknown initial state");
	}
	*parentAccepted = accepted;
	return count;
}
