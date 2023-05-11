const char * DOMAIN_NAME = "day";
const char * PROBLEM_NAME = "day";

char const * const objnames_object[] = {"bob"};
char const * const objnames_user[] = {"bob"};

std::ostream& operator<<(std::ostream &o, StatePtr x) {
	if(x->SisDressed_bob) o << " (isDressed bob)"; else o<< " (not (isDressed bob))"; 
	if(x->SdrankCoffee_bob) o << " (drankCoffee bob)"; else o<< " (not (drankCoffee bob))"; 
	if(x->ShadBreakfast_bob) o << " (hadBreakfast bob)"; else o<< " (not (hadBreakfast bob))"; 
	if(x->SisClean_bob) o << " (isClean bob)"; else o<< " (not (isClean bob))"; 
	if(x->ShasLeft_bob) o << " (hasLeft bob)"; else o<< " (not (hasLeft bob))"; 
	return o;
}
void writeState(std::ostream &o, StatePtr x, double wt) {
	char buf[16 + 3*0 + 0]; char *s = (char*) buf;
	if (std::isinf(wt)) strcpy(s, "inf"), s += 3; else s += sprintf(s, "%g", wt);
	*s = ' '; s++; *s =x->SisDressed_bob + '0'; s++;
	*s = ' '; s++; *s =x->SdrankCoffee_bob + '0'; s++;
	*s = ' '; s++; *s =x->ShadBreakfast_bob + '0'; s++;
	*s = ' '; s++; *s =x->SisClean_bob + '0'; s++;
	*s = ' '; s++; *s =x->ShasLeft_bob + '0'; s++;
	*s = '\n'; *(++s) = '\0'; o << (char*) buf;
}
bool readState(std::istream &i, StateRec *x, double &wt) {
	char buf[4096]; char *s = (char*) buf;
	do i.getline(s, 4096); while (i.gcount() < 2 && i.good());
	if (!i.good()) return false;
	if (!memcmp(s, "inf", 3)) { wt = infinity, s+=3; } else wt = strtod(s, &s);
	char c;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->SisDressed_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->SdrankCoffee_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->ShadBreakfast_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->SisClean_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->ShasLeft_bob = c, s++; else return false;
	return true;
}
void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr) { // rt: Recency Vector (ACT-R Heuristic); t: ist the global time
	if (prev->SisDressed_bob ^ curr->SisDressed_bob) rT[0] = t;
	if (prev->SdrankCoffee_bob ^ curr->SdrankCoffee_bob) rT[1] = t;
	if (prev->ShadBreakfast_bob ^ curr->ShadBreakfast_bob) rT[2] = t;
	if (prev->SisClean_bob ^ curr->SisClean_bob) rT[3] = t;
	if (prev->ShasLeft_bob ^ curr->ShasLeft_bob) rT[4] = t;
}
double getRefract(StatePtr a1, StatePtr a2) { // compare two action preconditions and returns the rerfractoriness 
	double r=0; int n=0;
	if (a1->SisDressed_bob) {n++; if (a2->SisDressed_bob) r++;}
	if (a1->SdrankCoffee_bob) {n++; if (a2->SdrankCoffee_bob) r++;}
	if (a1->ShadBreakfast_bob) {n++; if (a2->ShadBreakfast_bob) r++;}
	if (a1->SisClean_bob) {n++; if (a2->SisClean_bob) r++;}
	if (a1->ShasLeft_bob) {n++; if (a2->ShasLeft_bob) r++;}
	return (n==0) ? 0 : r/n;
}

char const * const initialStateNames[] = {"day"};
char const * const goalNames[] = {"day"};

// no :non-repeating has been used
bool canFollowAfter(int next, int prev) { (void)next; (void)prev; return true; }

int const FinishedOpId = -3;
int const NoOpId = -2;
int const InitOpId = -1;
char const * const __actionNames[NOPS+3] = {
	"(FINISHED)",
	"(BLOCKED)",
	"(INITIALIZE)",
	"(dress bob)", // (:action dress ?u - user)
	"(coffee bob)", // (:action coffee ?u - user)
	"(breakfast bob)", // (:action breakfast ?u - user)
	"(clean bob)", // (:action clean ?u - user)
	"(leave bob)" // (:action leave ?u - user)
};
char const * const * const actionNames = &__actionNames[3];


/* (dress bob), id 0 */
double action_dress_bob(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->SisDressed_bob)) *specificity=1.00; 

	if(!(x->SisDressed_bob)) {
		*x1 = *x;
		x1->SisDressed_bob = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (coffee bob), id 1 */
double action_coffee_bob(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->SdrankCoffee_bob)) *specificity=1.00; 

	if(!(x->SdrankCoffee_bob)) {
		*x1 = *x;
		x1->SdrankCoffee_bob = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (breakfast bob), id 2 */
double action_breakfast_bob(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->ShadBreakfast_bob)) *specificity=1.00; 

	if(!(x->ShadBreakfast_bob)) {
		*x1 = *x;
		x1->ShadBreakfast_bob = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (clean bob), id 3 */
double action_clean_bob(StatePtr x, StateRec *x1, double *specificity) {
	if((x->ShadBreakfast_bob && x->SdrankCoffee_bob && !(x->SisClean_bob))) *specificity=2.00; 

	if((x->ShadBreakfast_bob && x->SdrankCoffee_bob && !(x->SisClean_bob))) {
		*x1 = *x;
		x1->SisClean_bob = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (leave bob), id 4 */
double action_leave_bob(StatePtr x, StateRec *x1, double *specificity) {
	if((x->SisClean_bob && x->SisDressed_bob && x->ShadBreakfast_bob && x->SdrankCoffee_bob && !(x->ShasLeft_bob))) *specificity=4.00; 

	if((x->SisClean_bob && x->SisDressed_bob && x->ShadBreakfast_bob && x->SdrankCoffee_bob && !(x->ShasLeft_bob))) {
		*x1 = *x;
		x1->ShasLeft_bob = 1;
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
		r->SisDressed_bob=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr1 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->SdrankCoffee_bob=1; 
	} else 
		 return (exp(rT[1]-t))/1;
	return -1;
}
double getRecOrRefr2 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->ShadBreakfast_bob=1; 
	} else 
		 return (exp(rT[2]-t))/1;
	return -1;
}
double getRecOrRefr3 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->ShadBreakfast_bob=1; r->SdrankCoffee_bob=1; r->SisClean_bob=1; 
	} else 
		 return (exp(rT[2]-t)+exp(rT[1]-t)+exp(rT[3]-t))/3;
	return -1;
}
double getRecOrRefr4 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->SisClean_bob=1; r->SisDressed_bob=1; r->ShadBreakfast_bob=1; r->SdrankCoffee_bob=1; r->ShasLeft_bob=1; 
	} else 
		 return (exp(rT[3]-t)+exp(rT[0]-t)+exp(rT[2]-t)+exp(rT[1]-t)+exp(rT[4]-t))/5;
	return -1;
}

/*** BEGIN CALLBACKS ***/
void callback_dress_bob(StatePtr x, int _agid) { (void) x; setActivity(_agid,1);}
void callback_coffee_bob(StatePtr x, int _agid) { (void) x; setActivity(_agid,2);}
void callback_breakfast_bob(StatePtr x, int _agid) { (void) x; setActivity(_agid,3);}
void callback_clean_bob(StatePtr x, int _agid) { (void) x; setActivity(_agid,4);}
void callback_leave_bob(StatePtr x, int _agid) { (void) x; setActivity(_agid,5);}
/*** END CALLBACKS ****/

namespace ActionSchemes {
const ActionScheme s_FINISHED("FINISHED", std::vector<paramType>());
const ActionScheme s_BLOCKED("BLOCKED", std::vector<paramType>());
const ActionScheme s_INITIALIZE("INITIALIZE", std::vector<paramType>());
boost::array<paramType,1> __params_dress = {std::make_pair("u","user")};
const ActionScheme s_dress("dress", std::vector<paramType>(__params_dress.begin(), __params_dress.end()));
boost::array<paramType,1> __params_coffee = {std::make_pair("u","user")};
const ActionScheme s_coffee("coffee", std::vector<paramType>(__params_coffee.begin(), __params_coffee.end()));
boost::array<paramType,1> __params_breakfast = {std::make_pair("u","user")};
const ActionScheme s_breakfast("breakfast", std::vector<paramType>(__params_breakfast.begin(), __params_breakfast.end()));
boost::array<paramType,1> __params_clean = {std::make_pair("u","user")};
const ActionScheme s_clean("clean", std::vector<paramType>(__params_clean.begin(), __params_clean.end()));
boost::array<paramType,1> __params_leave = {std::make_pair("u","user")};
const ActionScheme s_leave("leave", std::vector<paramType>(__params_leave.begin(), __params_leave.end()));
} // namespace ActionSchemes

ActionScheme const * __actionSchemes[5 + 3] = {
	&ActionSchemes::s_FINISHED,
	&ActionSchemes::s_BLOCKED,
	&ActionSchemes::s_INITIALIZE,
	&ActionSchemes::s_dress,
	&ActionSchemes::s_coffee,
	&ActionSchemes::s_breakfast,
	&ActionSchemes::s_clean,
	&ActionSchemes::s_leave
};
ActionScheme const * *actionSchemes = &__actionSchemes[3];

namespace Actions {
const ActionSchemes::S_FINISHED a_FINISHED(FinishedOpId, "FINISHED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_BLOCKED a_BLOCKED(NoOpId, "BLOCKED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_INITIALIZE a_INITIALIZE(InitOpId, "INITIALIZE", NULL, NULL, NULL, finishedImmediate , pStopcdfImmediate, pStoppdfImmediate, 0.0 /* Initial */);
const ActionSchemes::S_dress a_dress_bob(0, "(dress bob)", action_dress_bob, callback_dress_bob, getRecOrRefr0, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 0 */);
const ActionSchemes::S_coffee a_coffee_bob(1, "(coffee bob)", action_coffee_bob, callback_coffee_bob, getRecOrRefr1, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 1 */);
const ActionSchemes::S_breakfast a_breakfast_bob(2, "(breakfast bob)", action_breakfast_bob, callback_breakfast_bob, getRecOrRefr2, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 2 */);
const ActionSchemes::S_clean a_clean_bob(3, "(clean bob)", action_clean_bob, callback_clean_bob, getRecOrRefr3, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 3 */);
const ActionSchemes::S_leave a_leave_bob(4, "(leave bob)", action_leave_bob, callback_leave_bob, getRecOrRefr4, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 4 */);
} // namespace Actions

Action const * __actions[NOPS+3] = {
	&Actions::a_FINISHED,
	&Actions::a_BLOCKED,
	&Actions::a_INITIALIZE,
	&Actions::a_dress_bob,
	&Actions::a_coffee_bob,
	&Actions::a_breakfast_bob,
	&Actions::a_clean_bob,
	&Actions::a_leave_bob
};
Action const * *actions = &__actions[3];

const int agentOps0[5] = {0,1,2,3,4};
char const * const agentNames[NAGENTS] = {"NULL"};
int const nOpsForAgent[NAGENTS] = {5};
int const * const opsForAgent[NAGENTS] = {agentOps0};

bool isGoalState(size_t goal, StatePtr x) { (void) x;
	switch(goal) {
	case 0: /*day*/
		return x->ShasLeft_bob;
	default: throw std::runtime_error("isGoalState: Unknown goal"); break;
	}
}

void sampleInitial(size_t initialState, StateRec *x1) {
	bzero(x1,sizeof(StateRec));
	switch(initialState) {
	case 0: /*day*/
		x1->SisDressed_bob = 0;
		x1->SdrankCoffee_bob = 0;
		x1->ShadBreakfast_bob = 0;
		x1->SisClean_bob = 0;
		x1->ShasLeft_bob = 0;
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
	case 0: /*day*/
		switch(goal) {
		case 0: /*day*/
			if (parentAccepted == 0) {
				//accepted = StateRec();accepted.S1=1;
			} else {
				accepted= *parentAccepted;
		
				if (s->SdrankCoffee_bob==1) accepted.SdrankCoffee_bob=1;
				if (s->ShadBreakfast_bob==1) accepted.ShadBreakfast_bob=1;
				if (s->SisDressed_bob==1) accepted.SisDressed_bob=1;
				if (s->SisClean_bob==1) if (parentAccepted->ShadBreakfast_bob==1)  if (parentAccepted->SdrankCoffee_bob==1)  accepted.SisClean_bob=1;
				if (s->ShasLeft_bob==1) if (parentAccepted->SisClean_bob==1)  if (parentAccepted->SisDressed_bob==1)  if (parentAccepted->ShadBreakfast_bob==1)  if (parentAccepted->SdrankCoffee_bob==1)  accepted.ShasLeft_bob=1;
			} // compute the heuristic
			if (accepted.SdrankCoffee_bob== 0) count++;
			if (accepted.ShadBreakfast_bob== 0) count++;
			if (accepted.SisDressed_bob== 0) count++;
			if (accepted.SisClean_bob== 0) count++;
			if (accepted.ShasLeft_bob== 0) count++;
				if (accepted.ShasLeft_bob== 1 && s->ShasLeft_bob== 0) count++;
			if (accepted.SdrankCoffee_bob== 1 && s->SdrankCoffee_bob== 0  && (accepted.ShasLeft_bob==0 || accepted.SisClean_bob==0)) count++;
			if (accepted.ShadBreakfast_bob== 1 && s->ShadBreakfast_bob== 0  && (accepted.ShasLeft_bob==0 || accepted.SisClean_bob==0)) count++;
			if (accepted.SisDressed_bob== 1 && s->SisDressed_bob== 0  && (accepted.ShasLeft_bob==0)) count++;
			if (accepted.SisClean_bob== 1 && s->SisClean_bob== 0  && (accepted.ShasLeft_bob==0)) count++;
			break;
		default: throw std::runtime_error("landmarkCountHeuristic: Unknown goal state");
		}
		break;
	default: throw std::runtime_error("landmarkCountHeuristic: Unknown initial state");
	}
	*parentAccepted = accepted;
	return count;
}
