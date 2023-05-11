const char * DOMAIN_NAME = "carrots";
const char * PROBLEM_NAME = "carrots2";

char const * const objnames_object[] = {"sink","counter","carrot","knife"};
char const * const objnames_location[] = {"sink","counter"};
char const * const objnames_takeable[] = {"carrot","knife"};

std::ostream& operator<<(std::ostream &o, StatePtr x) {
	if(x->Sis_at_sink) o << " (is-at sink)"; else o<< " (not (is-at sink))"; 
	if(x->Sis_at_counter) o << " (is-at counter)"; else o<< " (not (is-at counter))"; 
	if(x->Staken_carrot) o << " (taken carrot)"; else o<< " (not (taken carrot))"; 
	if(x->Staken_knife) o << " (taken knife)"; else o<< " (not (taken knife))"; 
	if(x->Sat_carrot_sink) o << " (at carrot sink)"; else o<< " (not (at carrot sink))"; 
	if(x->Sat_carrot_counter) o << " (at carrot counter)"; else o<< " (not (at carrot counter))"; 
	if(x->Sat_knife_sink) o << " (at knife sink)"; else o<< " (not (at knife sink))"; 
	if(x->Sat_knife_counter) o << " (at knife counter)"; else o<< " (not (at knife counter))"; 
	if(x->Shandsfree) o << " handsfree"; else o<< " (not handsfree)"; 
	return o;
}
void writeState(std::ostream &o, StatePtr x, double wt) {
	char buf[16 + 3*0 + 0]; char *s = (char*) buf;
	if (std::isinf(wt)) strcpy(s, "inf"), s += 3; else s += sprintf(s, "%g", wt);
	*s = ' '; s++; *s =x->Sis_at_sink + '0'; s++;
	*s = ' '; s++; *s =x->Sis_at_counter + '0'; s++;
	*s = ' '; s++; *s =x->Staken_carrot + '0'; s++;
	*s = ' '; s++; *s =x->Staken_knife + '0'; s++;
	*s = ' '; s++; *s =x->Sat_carrot_sink + '0'; s++;
	*s = ' '; s++; *s =x->Sat_carrot_counter + '0'; s++;
	*s = ' '; s++; *s =x->Sat_knife_sink + '0'; s++;
	*s = ' '; s++; *s =x->Sat_knife_counter + '0'; s++;
	*s = ' '; s++; *s =x->Shandsfree + '0'; s++;
	*s = '\n'; *(++s) = '\0'; o << (char*) buf;
}
bool readState(std::istream &i, StateRec *x, double &wt) {
	char buf[4096]; char *s = (char*) buf;
	do i.getline(s, 4096); while (i.gcount() < 2 && i.good());
	if (!i.good()) return false;
	if (!memcmp(s, "inf", 3)) { wt = infinity, s+=3; } else wt = strtod(s, &s);
	char c;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_at_sink = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_at_counter = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Staken_carrot = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Staken_knife = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_carrot_sink = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_carrot_counter = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_knife_sink = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_knife_counter = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Shandsfree = c, s++; else return false;
	return true;
}
void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr) { // rt: Recency Vector (ACT-R Heuristic); t: ist the global time
	if (prev->Sis_at_sink ^ curr->Sis_at_sink) rT[0] = t;
	if (prev->Sis_at_counter ^ curr->Sis_at_counter) rT[1] = t;
	if (prev->Staken_carrot ^ curr->Staken_carrot) rT[2] = t;
	if (prev->Staken_knife ^ curr->Staken_knife) rT[3] = t;
	if (prev->Sat_carrot_sink ^ curr->Sat_carrot_sink) rT[4] = t;
	if (prev->Sat_carrot_counter ^ curr->Sat_carrot_counter) rT[5] = t;
	if (prev->Sat_knife_sink ^ curr->Sat_knife_sink) rT[6] = t;
	if (prev->Sat_knife_counter ^ curr->Sat_knife_counter) rT[7] = t;
	if (prev->Shandsfree ^ curr->Shandsfree) rT[8] = t;
}
double getRefract(StatePtr a1, StatePtr a2) { // compare two action preconditions and returns the rerfractoriness 
	double r=0; int n=0;
	if (a1->Sis_at_sink) {n++; if (a2->Sis_at_sink) r++;}
	if (a1->Sis_at_counter) {n++; if (a2->Sis_at_counter) r++;}
	if (a1->Staken_carrot) {n++; if (a2->Staken_carrot) r++;}
	if (a1->Staken_knife) {n++; if (a2->Staken_knife) r++;}
	if (a1->Sat_carrot_sink) {n++; if (a2->Sat_carrot_sink) r++;}
	if (a1->Sat_carrot_counter) {n++; if (a2->Sat_carrot_counter) r++;}
	if (a1->Sat_knife_sink) {n++; if (a2->Sat_knife_sink) r++;}
	if (a1->Sat_knife_counter) {n++; if (a2->Sat_knife_counter) r++;}
	if (a1->Shandsfree) {n++; if (a2->Shandsfree) r++;}
	return (n==0) ? 0 : r/n;
}

char const * const initialStateNames[] = {"carrots2"};
char const * const goalNames[] = {"carrots2"};

// no :non-repeating has been used
bool canFollowAfter(int next, int prev) { (void)next; (void)prev; return true; }

int const FinishedOpId = -3;
int const NoOpId = -2;
int const InitOpId = -1;
char const * const __actionNames[NOPS+3] = {
	"(FINISHED)",
	"(BLOCKED)",
	"(INITIALIZE)",
	"(move sink counter)", // (:action move ?from ?to - location)
	"(move counter sink)", // (:action move ?from ?to - location)
	"(take carrot sink)", // (:action take ?t - takeable ?from - location)
	"(take carrot counter)", // (:action take ?t - takeable ?from - location)
	"(take knife sink)", // (:action take ?t - takeable ?from - location)
	"(take knife counter)", // (:action take ?t - takeable ?from - location)
	"(put carrot sink)", // (:action put ?t - takeable ?to - location)
	"(put carrot counter)", // (:action put ?t - takeable ?to - location)
	"(put knife sink)", // (:action put ?t - takeable ?to - location)
	"(put knife counter)" // (:action put ?t - takeable ?to - location)
};
char const * const * const actionNames = &__actionNames[3];


/* (move sink counter), id 0 */
double action_move_sink_counter(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_at_sink) *specificity=0.20; 

	if(x->Sis_at_sink) {
		*x1 = *x;
		x1->Sis_at_sink = 0;
		x1->Sis_at_counter = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (move counter sink), id 1 */
double action_move_counter_sink(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_at_counter) *specificity=0.20; 

	if(x->Sis_at_counter) {
		*x1 = *x;
		x1->Sis_at_counter = 0;
		x1->Sis_at_sink = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take carrot sink), id 2 */
double action_take_carrot_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_carrot) && x->Sat_carrot_sink && x->Sis_at_sink && x->Shandsfree)) *specificity=1.95; 

	if((!(x->Staken_carrot) && x->Sat_carrot_sink && x->Sis_at_sink && x->Shandsfree)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_carrot_sink = 0;
		x1->Staken_carrot = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take carrot counter), id 3 */
double action_take_carrot_counter(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_carrot) && x->Sat_carrot_counter && x->Sis_at_counter && x->Shandsfree)) *specificity=1.95; 

	if((!(x->Staken_carrot) && x->Sat_carrot_counter && x->Sis_at_counter && x->Shandsfree)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_carrot_counter = 0;
		x1->Staken_carrot = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife sink), id 4 */
double action_take_knife_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_knife) && x->Sat_knife_sink && x->Sis_at_sink && x->Shandsfree)) *specificity=1.95; 

	if((!(x->Staken_knife) && x->Sat_knife_sink && x->Sis_at_sink && x->Shandsfree)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_knife_sink = 0;
		x1->Staken_knife = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter), id 5 */
double action_take_knife_counter(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_knife) && x->Sat_knife_counter && x->Sis_at_counter && x->Shandsfree)) *specificity=1.95; 

	if((!(x->Staken_knife) && x->Sat_knife_counter && x->Sis_at_counter && x->Shandsfree)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_knife_counter = 0;
		x1->Staken_knife = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (put carrot sink), id 6 */
double action_put_carrot_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_carrot && !(x->Sat_carrot_sink) && x->Sis_at_sink)) *specificity=1.70; 

	if((x->Staken_carrot && !(x->Sat_carrot_sink) && x->Sis_at_sink)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_carrot_sink = 1;
		x1->Staken_carrot = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put carrot counter), id 7 */
double action_put_carrot_counter(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_carrot && !(x->Sat_carrot_counter) && x->Sis_at_counter)) *specificity=1.70; 

	if((x->Staken_carrot && !(x->Sat_carrot_counter) && x->Sis_at_counter)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_carrot_counter = 1;
		x1->Staken_carrot = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put knife sink), id 8 */
double action_put_knife_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_knife && !(x->Sat_knife_sink) && x->Sis_at_sink)) *specificity=1.70; 

	if((x->Staken_knife && !(x->Sat_knife_sink) && x->Sis_at_sink)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_knife_sink = 1;
		x1->Staken_knife = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put knife counter), id 9 */
double action_put_knife_counter(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_knife && !(x->Sat_knife_counter) && x->Sis_at_counter)) *specificity=1.70; 

	if((x->Staken_knife && !(x->Sat_knife_counter) && x->Sis_at_counter)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_knife_counter = 1;
		x1->Staken_knife = 0;
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
		r->Sis_at_sink=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr1 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_at_counter=1; 
	} else 
		 return (exp(rT[1]-t))/1;
	return -1;
}
double getRecOrRefr2 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_carrot=1; r->Sat_carrot_sink=1; r->Sis_at_sink=1; r->Shandsfree=1; 
	} else 
		 return (exp(rT[2]-t)+exp(rT[4]-t)+exp(rT[0]-t)+exp(rT[8]-t))/4;
	return -1;
}
double getRecOrRefr3 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_carrot=1; r->Sat_carrot_counter=1; r->Sis_at_counter=1; r->Shandsfree=1; 
	} else 
		 return (exp(rT[2]-t)+exp(rT[5]-t)+exp(rT[1]-t)+exp(rT[8]-t))/4;
	return -1;
}
double getRecOrRefr4 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_knife=1; r->Sat_knife_sink=1; r->Sis_at_sink=1; r->Shandsfree=1; 
	} else 
		 return (exp(rT[3]-t)+exp(rT[6]-t)+exp(rT[0]-t)+exp(rT[8]-t))/4;
	return -1;
}
double getRecOrRefr5 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_knife=1; r->Sat_knife_counter=1; r->Sis_at_counter=1; r->Shandsfree=1; 
	} else 
		 return (exp(rT[3]-t)+exp(rT[7]-t)+exp(rT[1]-t)+exp(rT[8]-t))/4;
	return -1;
}
double getRecOrRefr6 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_carrot=1; r->Sat_carrot_sink=1; r->Sis_at_sink=1; 
	} else 
		 return (exp(rT[2]-t)+exp(rT[4]-t)+exp(rT[0]-t))/3;
	return -1;
}
double getRecOrRefr7 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_carrot=1; r->Sat_carrot_counter=1; r->Sis_at_counter=1; 
	} else 
		 return (exp(rT[2]-t)+exp(rT[5]-t)+exp(rT[1]-t))/3;
	return -1;
}
double getRecOrRefr8 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_knife=1; r->Sat_knife_sink=1; r->Sis_at_sink=1; 
	} else 
		 return (exp(rT[3]-t)+exp(rT[6]-t)+exp(rT[0]-t))/3;
	return -1;
}
double getRecOrRefr9 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_knife=1; r->Sat_knife_counter=1; r->Sis_at_counter=1; 
	} else 
		 return (exp(rT[3]-t)+exp(rT[7]-t)+exp(rT[1]-t))/3;
	return -1;
}


namespace ActionSchemes {
const ActionScheme s_FINISHED("FINISHED", std::vector<paramType>());
const ActionScheme s_BLOCKED("BLOCKED", std::vector<paramType>());
const ActionScheme s_INITIALIZE("INITIALIZE", std::vector<paramType>());
boost::array<paramType,2> __params_move = {std::make_pair("from","location"),std::make_pair("to","location")};
const ActionScheme s_move("move", std::vector<paramType>(__params_move.begin(), __params_move.end()));
boost::array<paramType,2> __params_take = {std::make_pair("t","takeable"),std::make_pair("from","location")};
const ActionScheme s_take("take", std::vector<paramType>(__params_take.begin(), __params_take.end()));
boost::array<paramType,2> __params_put = {std::make_pair("t","takeable"),std::make_pair("to","location")};
const ActionScheme s_put("put", std::vector<paramType>(__params_put.begin(), __params_put.end()));
} // namespace ActionSchemes

ActionScheme const * __actionSchemes[3 + 3] = {
	&ActionSchemes::s_FINISHED,
	&ActionSchemes::s_BLOCKED,
	&ActionSchemes::s_INITIALIZE,
	&ActionSchemes::s_move,
	&ActionSchemes::s_take,
	&ActionSchemes::s_put
};
ActionScheme const * *actionSchemes = &__actionSchemes[3];

namespace Actions {
const ActionSchemes::S_FINISHED a_FINISHED(FinishedOpId, "FINISHED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_BLOCKED a_BLOCKED(NoOpId, "BLOCKED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_INITIALIZE a_INITIALIZE(InitOpId, "INITIALIZE", NULL, NULL, NULL, finishedImmediate , pStopcdfImmediate, pStoppdfImmediate, 0.0 /* Initial */);
const ActionSchemes::S_move a_move_sink_counter(0, "(move sink counter)", action_move_sink_counter, NULL, getRecOrRefr0, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 0 */);
const ActionSchemes::S_move a_move_counter_sink(1, "(move counter sink)", action_move_counter_sink, NULL, getRecOrRefr1, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 1 */);
const ActionSchemes::S_take a_take_carrot_sink(2, "(take carrot sink)", action_take_carrot_sink, NULL, getRecOrRefr2, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 2 */);
const ActionSchemes::S_take a_take_carrot_counter(3, "(take carrot counter)", action_take_carrot_counter, NULL, getRecOrRefr3, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 3 */);
const ActionSchemes::S_take a_take_knife_sink(4, "(take knife sink)", action_take_knife_sink, NULL, getRecOrRefr4, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 4 */);
const ActionSchemes::S_take a_take_knife_counter(5, "(take knife counter)", action_take_knife_counter, NULL, getRecOrRefr5, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 5 */);
const ActionSchemes::S_put a_put_carrot_sink(6, "(put carrot sink)", action_put_carrot_sink, NULL, getRecOrRefr6, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 6 */);
const ActionSchemes::S_put a_put_carrot_counter(7, "(put carrot counter)", action_put_carrot_counter, NULL, getRecOrRefr7, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 7 */);
const ActionSchemes::S_put a_put_knife_sink(8, "(put knife sink)", action_put_knife_sink, NULL, getRecOrRefr8, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 8 */);
const ActionSchemes::S_put a_put_knife_counter(9, "(put knife counter)", action_put_knife_counter, NULL, getRecOrRefr9, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 9 */);
} // namespace Actions

Action const * __actions[NOPS+3] = {
	&Actions::a_FINISHED,
	&Actions::a_BLOCKED,
	&Actions::a_INITIALIZE,
	&Actions::a_move_sink_counter,
	&Actions::a_move_counter_sink,
	&Actions::a_take_carrot_sink,
	&Actions::a_take_carrot_counter,
	&Actions::a_take_knife_sink,
	&Actions::a_take_knife_counter,
	&Actions::a_put_carrot_sink,
	&Actions::a_put_carrot_counter,
	&Actions::a_put_knife_sink,
	&Actions::a_put_knife_counter
};
Action const * *actions = &__actions[3];

const int agentOps0[10] = {0,1,2,3,4,5,6,7,8,9};
char const * const agentNames[NAGENTS] = {"NULL"};
int const nOpsForAgent[NAGENTS] = {10};
int const * const opsForAgent[NAGENTS] = {agentOps0};

bool isGoalState(size_t goal, StatePtr x) { (void) x;
	switch(goal) {
	case 0: /*carrots2*/
		return (x->Sat_carrot_sink && x->Sat_knife_sink);
	default: throw std::runtime_error("isGoalState: Unknown goal"); break;
	}
}

void sampleInitial(size_t initialState, StateRec *x1) {
	bzero(x1,sizeof(StateRec));
	switch(initialState) {
	case 0: /*carrots2*/
		x1->Sat_knife_counter = 1;
		x1->Sat_carrot_counter = 1;
		x1->Shandsfree = 1;
		x1->Sis_at_sink = 1;
		x1->Sis_at_counter = 0;
		x1->Staken_carrot = 0;
		x1->Staken_knife = 0;
		x1->Sat_carrot_sink = 0;
		x1->Sat_knife_sink = 0;
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
	case 0: /*carrots2*/
		switch(goal) {
		case 0: /*carrots2*/
			if (parentAccepted == 0) {
				//accepted = StateRec();accepted.S1=1;
				accepted.Shandsfree=1;
				accepted.Sis_at_sink=1;
			} else {
				accepted= *parentAccepted;
		
				if (s->Shandsfree==1) accepted.Shandsfree=1;
				if (s->Staken_knife==1) if (parentAccepted->Shandsfree==1)  accepted.Staken_knife=1;
				if (s->Sis_at_sink==1) accepted.Sis_at_sink=1;
				if (s->Staken_carrot==1) if (parentAccepted->Shandsfree==1)  accepted.Staken_carrot=1;
				if (s->Sat_carrot_sink==1) if (parentAccepted->Staken_carrot==1)  if (parentAccepted->Sis_at_sink==1)  accepted.Sat_carrot_sink=1;
				if (s->Sat_knife_sink==1) if (parentAccepted->Staken_knife==1)  if (parentAccepted->Sis_at_sink==1)  accepted.Sat_knife_sink=1;
			} // compute the heuristic
			if (accepted.Shandsfree== 0) count++;
			if (accepted.Staken_knife== 0) count++;
			if (accepted.Sis_at_sink== 0) count++;
			if (accepted.Staken_carrot== 0) count++;
			if (accepted.Sat_carrot_sink== 0) count++;
			if (accepted.Sat_knife_sink== 0) count++;
				if (accepted.Sat_carrot_sink== 1 && s->Sat_carrot_sink== 0) count++;
				if (accepted.Sat_knife_sink== 1 && s->Sat_knife_sink== 0) count++;
			if (accepted.Shandsfree== 1 && s->Shandsfree== 0  && (accepted.Staken_carrot==0 || accepted.Staken_knife==0)) count++;
			if (accepted.Staken_knife== 1 && s->Staken_knife== 0  && (accepted.Sat_knife_sink==0)) count++;
			if (accepted.Sis_at_sink== 1 && s->Sis_at_sink== 0  && (accepted.Sat_carrot_sink==0 || accepted.Sat_knife_sink==0)) count++;
			if (accepted.Staken_carrot== 1 && s->Staken_carrot== 0  && (accepted.Sat_carrot_sink==0)) count++;
			break;
		default: throw std::runtime_error("landmarkCountHeuristic: Unknown goal state");
		}
		break;
	default: throw std::runtime_error("landmarkCountHeuristic: Unknown initial state");
	}
	*parentAccepted = accepted;
	return count;
}
