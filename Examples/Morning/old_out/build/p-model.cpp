const char * DOMAIN_NAME = "morning";
const char * PROBLEM_NAME = "morning";

char const * const objnames_object[] = {"max","hall","bathroom"};
char const * const objnames_user[] = {"max"};
char const * const objnames_location[] = {"hall","bathroom"};

std::ostream& operator<<(std::ostream &o, StatePtr x) {
	if(x->Slocation_max_hall) o << " (location max hall)"; else o<< " (not (location max hall))"; 
	if(x->Slocation_max_bathroom) o << " (location max bathroom)"; else o<< " (not (location max bathroom))"; 
	if(x->Sis_dirty_max) o << " (is_dirty max)"; else o<< " (not (is_dirty max))"; 
	return o;
}
void writeState(std::ostream &o, StatePtr x, double wt) {
	char buf[16 + 3*0 + 0]; char *s = (char*) buf;
	if (std::isinf(wt)) strcpy(s, "inf"), s += 3; else s += sprintf(s, "%g", wt);
	*s = ' '; s++; *s =x->Slocation_max_hall + '0'; s++;
	*s = ' '; s++; *s =x->Slocation_max_bathroom + '0'; s++;
	*s = ' '; s++; *s =x->Sis_dirty_max + '0'; s++;
	*s = '\n'; *(++s) = '\0'; o << (char*) buf;
}
bool readState(std::istream &i, StateRec *x, double &wt) {
	char buf[4096]; char *s = (char*) buf;
	do i.getline(s, 4096); while (i.gcount() < 2 && i.good());
	if (!i.good()) return false;
	if (!memcmp(s, "inf", 3)) { wt = infinity, s+=3; } else wt = strtod(s, &s);
	char c;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Slocation_max_hall = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Slocation_max_bathroom = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_dirty_max = c, s++; else return false;
	return true;
}
void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr) { // rt: Recency Vector (ACT-R Heuristic); t: ist the global time
	if (prev->Slocation_max_hall ^ curr->Slocation_max_hall) rT[0] = t;
	if (prev->Slocation_max_bathroom ^ curr->Slocation_max_bathroom) rT[1] = t;
	if (prev->Sis_dirty_max ^ curr->Sis_dirty_max) rT[2] = t;
}
double getRefract(StatePtr a1, StatePtr a2) { // compare two action preconditions and returns the rerfractoriness 
	double r=0; int n=0;
	if (a1->Slocation_max_hall) {n++; if (a2->Slocation_max_hall) r++;}
	if (a1->Slocation_max_bathroom) {n++; if (a2->Slocation_max_bathroom) r++;}
	if (a1->Sis_dirty_max) {n++; if (a2->Sis_dirty_max) r++;}
	return (n==0) ? 0 : r/n;
}

char const * const initialStateNames[] = {"morning"};
char const * const goalNames[] = {"morning"};

// no :non-repeating has been used
bool canFollowAfter(int next, int prev) { (void)next; (void)prev; return true; }

int const FinishedOpId = -3;
int const NoOpId = -2;
int const InitOpId = -1;
char const * const __actionNames[NOPS+3] = {
	"(FINISHED)",
	"(BLOCKED)",
	"(INITIALIZE)",
	"(visit_bath max)" // (:action visit_bath ?u - user)
};
char const * const * const actionNames = &__actionNames[3];


/* (visit_bath max), id 0 */
double action_visit_bath_max(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Slocation_max_hall && x->Sis_dirty_max)) *specificity=2.00; 

	if((x->Slocation_max_hall && x->Sis_dirty_max)) {
		*x1 = *x;
		x1->Slocation_max_hall = 0;
		x1->Slocation_max_bathroom = 1;
		x1->Sis_dirty_max = 0;
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
		r->Slocation_max_hall=1; r->Sis_dirty_max=1; 
	} else 
		 return (exp(rT[0]-t)+exp(rT[2]-t))/2;
	return -1;
}

/*** BEGIN CALLBACKS ***/
void callback_visit_bath_max(StatePtr x, int _agid) { (void) x; setLocation(_agid,bathroom);}
/*** END CALLBACKS ****/

namespace ActionSchemes {
const ActionScheme s_FINISHED("FINISHED", std::vector<paramType>());
const ActionScheme s_BLOCKED("BLOCKED", std::vector<paramType>());
const ActionScheme s_INITIALIZE("INITIALIZE", std::vector<paramType>());
boost::array<paramType,1> __params_visit_bath = {std::make_pair("u","user")};
const ActionScheme s_visit_bath("visit_bath", std::vector<paramType>(__params_visit_bath.begin(), __params_visit_bath.end()));
} // namespace ActionSchemes

ActionScheme const * __actionSchemes[1 + 3] = {
	&ActionSchemes::s_FINISHED,
	&ActionSchemes::s_BLOCKED,
	&ActionSchemes::s_INITIALIZE,
	&ActionSchemes::s_visit_bath
};
ActionScheme const * *actionSchemes = &__actionSchemes[3];

namespace Actions {
const ActionSchemes::S_FINISHED a_FINISHED(FinishedOpId, "FINISHED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_BLOCKED a_BLOCKED(NoOpId, "BLOCKED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_INITIALIZE a_INITIALIZE(InitOpId, "INITIALIZE", NULL, NULL, NULL, finishedImmediate , pStopcdfImmediate, pStoppdfImmediate, 0.0 /* Initial */);
const ActionSchemes::S_visit_bath a_visit_bath_max(0, "(visit_bath max)", action_visit_bath_max, callback_visit_bath_max, getRecOrRefr0, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 0 */);
} // namespace Actions

Action const * __actions[NOPS+3] = {
	&Actions::a_FINISHED,
	&Actions::a_BLOCKED,
	&Actions::a_INITIALIZE,
	&Actions::a_visit_bath_max
};
Action const * *actions = &__actions[3];

const int agentOps0[1] = {0};
char const * const agentNames[NAGENTS] = {"NULL"};
int const nOpsForAgent[NAGENTS] = {1};
int const * const opsForAgent[NAGENTS] = {agentOps0};

bool isGoalState(size_t goal, StatePtr x) { (void) x;
	switch(goal) {
	case 0: /*morning*/
		return !(x->Sis_dirty_max);
	default: throw std::runtime_error("isGoalState: Unknown goal"); break;
	}
}

void sampleInitial(size_t initialState, StateRec *x1) {
	bzero(x1,sizeof(StateRec));
	switch(initialState) {
	case 0: /*morning*/
		x1->Sis_dirty_max = 1;
		x1->Slocation_max_hall = 1;
		x1->Slocation_max_bathroom = 0;
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
	case 0: /*morning*/
		switch(goal) {
		case 0: /*morning*/
			if (parentAccepted == 0) {
				//accepted = StateRec();accepted.S1=1;
				accepted.Sis_dirty_max=1;
				accepted.Slocation_max_hall=1;
			} else {
				accepted= *parentAccepted;
		
				if (s->Sis_dirty_max==1) accepted.Sis_dirty_max=1;
				if (s->Slocation_max_hall==1) accepted.Slocation_max_hall=1;
				if (s->Sis_dirty_max==1) if (parentAccepted->Slocation_max_hall==1)  if (parentAccepted->Sis_dirty_max==1)  accepted.Sis_dirty_max=1;
			} // compute the heuristic
			if (accepted.Sis_dirty_max== 0) count++;
			if (accepted.Slocation_max_hall== 0) count++;
			if (accepted.Sis_dirty_max== 0) count++;
				if (accepted.Sis_dirty_max== 1 && s->Sis_dirty_max== 0) count++;
			if (accepted.Sis_dirty_max== 1 && s->Sis_dirty_max== 0  && (accepted.Sis_dirty_max==0)) count++;
			if (accepted.Slocation_max_hall== 1 && s->Slocation_max_hall== 0  && (accepted.Sis_dirty_max==0)) count++;
			break;
		default: throw std::runtime_error("landmarkCountHeuristic: Unknown goal state");
		}
		break;
	default: throw std::runtime_error("landmarkCountHeuristic: Unknown initial state");
	}
	*parentAccepted = accepted;
	return count;
}
