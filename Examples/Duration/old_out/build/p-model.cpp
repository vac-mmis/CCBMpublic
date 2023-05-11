const char * DOMAIN_NAME = "carrots";
const char * PROBLEM_NAME = "carrots2";

char const * const objnames_object[] = {"counter","knife","2","5","8","10","15"};
char const * const objnames_tool[] = {"knife"};
char const * const objnames_location[] = {"counter"};
char const * const objnames_time[] = {"2","5","8","10","15"};

std::ostream& operator<<(std::ostream &o, StatePtr x) {
	if(x->Sat_knife_counter) o << " (at knife counter)"; else o<< " (not (at knife counter))"; 
	return o;
}
void writeState(std::ostream &o, StatePtr x, double wt) {
	char buf[16 + 3*0 + 0]; char *s = (char*) buf;
	if (std::isinf(wt)) strcpy(s, "inf"), s += 3; else s += sprintf(s, "%g", wt);
	*s = ' '; s++; *s =x->Sat_knife_counter + '0'; s++;
	*s = '\n'; *(++s) = '\0'; o << (char*) buf;
}
bool readState(std::istream &i, StateRec *x, double &wt) {
	char buf[4096]; char *s = (char*) buf;
	do i.getline(s, 4096); while (i.gcount() < 2 && i.good());
	if (!i.good()) return false;
	if (!memcmp(s, "inf", 3)) { wt = infinity, s+=3; } else wt = strtod(s, &s);
	char c;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_knife_counter = c, s++; else return false;
	return true;
}
void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr) { // rt: Recency Vector (ACT-R Heuristic); t: ist the global time
	if (prev->Sat_knife_counter ^ curr->Sat_knife_counter) rT[0] = t;
}
double getRefract(StatePtr a1, StatePtr a2) { // compare two action preconditions and returns the rerfractoriness 
	double r=0; int n=0;
	if (a1->Sat_knife_counter) {n++; if (a2->Sat_knife_counter) r++;}
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
	"(take knife counter 2 2)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 2 5)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 2 8)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 2 10)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 2 15)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 5 2)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 5 5)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 5 8)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 5 10)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 5 15)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 8 2)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 8 5)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 8 8)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 8 10)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 8 15)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 10 2)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 10 5)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 10 8)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 10 10)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 10 15)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 15 2)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 15 5)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 15 8)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 15 10)", // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
	"(take knife counter 15 15)" // (:action take ?t - tool ?from - location ?durmean - time ?dursd - time)
};
char const * const * const actionNames = &__actionNames[3];


/* (take knife counter 2 2), id 0 */
double action_take_knife_counter_2_2(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 2 5), id 1 */
double action_take_knife_counter_2_5(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 2 8), id 2 */
double action_take_knife_counter_2_8(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 2 10), id 3 */
double action_take_knife_counter_2_10(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 2 15), id 4 */
double action_take_knife_counter_2_15(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 5 2), id 5 */
double action_take_knife_counter_5_2(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 5 5), id 6 */
double action_take_knife_counter_5_5(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 5 8), id 7 */
double action_take_knife_counter_5_8(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 5 10), id 8 */
double action_take_knife_counter_5_10(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 5 15), id 9 */
double action_take_knife_counter_5_15(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 8 2), id 10 */
double action_take_knife_counter_8_2(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 8 5), id 11 */
double action_take_knife_counter_8_5(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 8 8), id 12 */
double action_take_knife_counter_8_8(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 8 10), id 13 */
double action_take_knife_counter_8_10(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 8 15), id 14 */
double action_take_knife_counter_8_15(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 10 2), id 15 */
double action_take_knife_counter_10_2(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 10 5), id 16 */
double action_take_knife_counter_10_5(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 10 8), id 17 */
double action_take_knife_counter_10_8(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 10 10), id 18 */
double action_take_knife_counter_10_10(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 10 15), id 19 */
double action_take_knife_counter_10_15(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 15 2), id 20 */
double action_take_knife_counter_15_2(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 15 5), id 21 */
double action_take_knife_counter_15_5(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 15 8), id 22 */
double action_take_knife_counter_15_8(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 15 10), id 23 */
double action_take_knife_counter_15_10(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife counter 15 15), id 24 */
double action_take_knife_counter_15_15(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_knife_counter) *specificity=0.04; 

	if(x->Sat_knife_counter) {
		*x1 = *x;
		x1->Sat_knife_counter = 0;
		return 1;
	} else {
		return 0;
	}
}

bool finished0(double _1, double *p=NULL) { return normalModel(_1,2,2,p); }
bool finished1(double _1, double *p=NULL) { return normalModel(_1,2,5,p); }
bool finished2(double _1, double *p=NULL) { return normalModel(_1,2,8,p); }
bool finished3(double _1, double *p=NULL) { return normalModel(_1,2,10,p); }
bool finished4(double _1, double *p=NULL) { return normalModel(_1,2,15,p); }
bool finished5(double _1, double *p=NULL) { return normalModel(_1,5,2,p); }
bool finished6(double _1, double *p=NULL) { return normalModel(_1,5,5,p); }
bool finished7(double _1, double *p=NULL) { return normalModel(_1,5,8,p); }
bool finished8(double _1, double *p=NULL) { return normalModel(_1,5,10,p); }
bool finished9(double _1, double *p=NULL) { return normalModel(_1,5,15,p); }
bool finished10(double _1, double *p=NULL) { return normalModel(_1,8,2,p); }
bool finished11(double _1, double *p=NULL) { return normalModel(_1,8,5,p); }
bool finished12(double _1, double *p=NULL) { return normalModel(_1,8,8,p); }
bool finished13(double _1, double *p=NULL) { return normalModel(_1,8,10,p); }
bool finished14(double _1, double *p=NULL) { return normalModel(_1,8,15,p); }
bool finished15(double _1, double *p=NULL) { return normalModel(_1,10,2,p); }
bool finished16(double _1, double *p=NULL) { return normalModel(_1,10,5,p); }
bool finished17(double _1, double *p=NULL) { return normalModel(_1,10,8,p); }
bool finished18(double _1, double *p=NULL) { return normalModel(_1,10,10,p); }
bool finished19(double _1, double *p=NULL) { return normalModel(_1,10,15,p); }
bool finished20(double _1, double *p=NULL) { return normalModel(_1,15,2,p); }
bool finished21(double _1, double *p=NULL) { return normalModel(_1,15,5,p); }
bool finished22(double _1, double *p=NULL) { return normalModel(_1,15,8,p); }
bool finished23(double _1, double *p=NULL) { return normalModel(_1,15,10,p); }
bool finished24(double _1, double *p=NULL) { return normalModel(_1,15,15,p); }
bool finishedImmediate(double, double* p) { if (p)*p=1.0; return true; }
double pStopcdf0(double startTime, double curTime) { return normalcdf(curTime- startTime,2,2); }
double pStopcdf1(double startTime, double curTime) { return normalcdf(curTime- startTime,2,5); }
double pStopcdf2(double startTime, double curTime) { return normalcdf(curTime- startTime,2,8); }
double pStopcdf3(double startTime, double curTime) { return normalcdf(curTime- startTime,2,10); }
double pStopcdf4(double startTime, double curTime) { return normalcdf(curTime- startTime,2,15); }
double pStopcdf5(double startTime, double curTime) { return normalcdf(curTime- startTime,5,2); }
double pStopcdf6(double startTime, double curTime) { return normalcdf(curTime- startTime,5,5); }
double pStopcdf7(double startTime, double curTime) { return normalcdf(curTime- startTime,5,8); }
double pStopcdf8(double startTime, double curTime) { return normalcdf(curTime- startTime,5,10); }
double pStopcdf9(double startTime, double curTime) { return normalcdf(curTime- startTime,5,15); }
double pStopcdf10(double startTime, double curTime) { return normalcdf(curTime- startTime,8,2); }
double pStopcdf11(double startTime, double curTime) { return normalcdf(curTime- startTime,8,5); }
double pStopcdf12(double startTime, double curTime) { return normalcdf(curTime- startTime,8,8); }
double pStopcdf13(double startTime, double curTime) { return normalcdf(curTime- startTime,8,10); }
double pStopcdf14(double startTime, double curTime) { return normalcdf(curTime- startTime,8,15); }
double pStopcdf15(double startTime, double curTime) { return normalcdf(curTime- startTime,10,2); }
double pStopcdf16(double startTime, double curTime) { return normalcdf(curTime- startTime,10,5); }
double pStopcdf17(double startTime, double curTime) { return normalcdf(curTime- startTime,10,8); }
double pStopcdf18(double startTime, double curTime) { return normalcdf(curTime- startTime,10,10); }
double pStopcdf19(double startTime, double curTime) { return normalcdf(curTime- startTime,10,15); }
double pStopcdf20(double startTime, double curTime) { return normalcdf(curTime- startTime,15,2); }
double pStopcdf21(double startTime, double curTime) { return normalcdf(curTime- startTime,15,5); }
double pStopcdf22(double startTime, double curTime) { return normalcdf(curTime- startTime,15,8); }
double pStopcdf23(double startTime, double curTime) { return normalcdf(curTime- startTime,15,10); }
double pStopcdf24(double startTime, double curTime) { return normalcdf(curTime- startTime,15,15); }
double pStopcdfImmediate(double startTime, double curTime){ return (curTime > startTime) ? 1.0 : 0.0; }
double pStoppdf0(double startTime, double curTime) { return normalpdf(curTime - startTime,2,2); }
double pStoppdf1(double startTime, double curTime) { return normalpdf(curTime - startTime,2,5); }
double pStoppdf2(double startTime, double curTime) { return normalpdf(curTime - startTime,2,8); }
double pStoppdf3(double startTime, double curTime) { return normalpdf(curTime - startTime,2,10); }
double pStoppdf4(double startTime, double curTime) { return normalpdf(curTime - startTime,2,15); }
double pStoppdf5(double startTime, double curTime) { return normalpdf(curTime - startTime,5,2); }
double pStoppdf6(double startTime, double curTime) { return normalpdf(curTime - startTime,5,5); }
double pStoppdf7(double startTime, double curTime) { return normalpdf(curTime - startTime,5,8); }
double pStoppdf8(double startTime, double curTime) { return normalpdf(curTime - startTime,5,10); }
double pStoppdf9(double startTime, double curTime) { return normalpdf(curTime - startTime,5,15); }
double pStoppdf10(double startTime, double curTime) { return normalpdf(curTime - startTime,8,2); }
double pStoppdf11(double startTime, double curTime) { return normalpdf(curTime - startTime,8,5); }
double pStoppdf12(double startTime, double curTime) { return normalpdf(curTime - startTime,8,8); }
double pStoppdf13(double startTime, double curTime) { return normalpdf(curTime - startTime,8,10); }
double pStoppdf14(double startTime, double curTime) { return normalpdf(curTime - startTime,8,15); }
double pStoppdf15(double startTime, double curTime) { return normalpdf(curTime - startTime,10,2); }
double pStoppdf16(double startTime, double curTime) { return normalpdf(curTime - startTime,10,5); }
double pStoppdf17(double startTime, double curTime) { return normalpdf(curTime - startTime,10,8); }
double pStoppdf18(double startTime, double curTime) { return normalpdf(curTime - startTime,10,10); }
double pStoppdf19(double startTime, double curTime) { return normalpdf(curTime - startTime,10,15); }
double pStoppdf20(double startTime, double curTime) { return normalpdf(curTime - startTime,15,2); }
double pStoppdf21(double startTime, double curTime) { return normalpdf(curTime - startTime,15,5); }
double pStoppdf22(double startTime, double curTime) { return normalpdf(curTime - startTime,15,8); }
double pStoppdf23(double startTime, double curTime) { return normalpdf(curTime - startTime,15,10); }
double pStoppdf24(double startTime, double curTime) { return normalpdf(curTime - startTime,15,15); }
double pStoppdfImmediate(double startTime, double curTime){ return (curTime > startTime) ? 1.0 : 0.0; }

// Implementations for the Refractoriness and Recency (ACT-R Heuristics)
// The first paramter is the current state and the second one is the state for the Refractoriness
double getRecOrRefr0 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr1 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr2 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr3 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr4 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr5 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr6 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr7 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr8 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr9 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr10 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr11 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr12 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr13 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr14 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr15 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr16 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr17 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr18 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr19 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr20 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr21 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr22 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr23 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr24 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_knife_counter=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}


namespace ActionSchemes {
const ActionScheme s_FINISHED("FINISHED", std::vector<paramType>());
const ActionScheme s_BLOCKED("BLOCKED", std::vector<paramType>());
const ActionScheme s_INITIALIZE("INITIALIZE", std::vector<paramType>());
boost::array<paramType,4> __params_take = {std::make_pair("t","tool"),std::make_pair("from","location"),std::make_pair("durmean","time"),std::make_pair("dursd","time")};
const ActionScheme s_take("take", std::vector<paramType>(__params_take.begin(), __params_take.end()));
} // namespace ActionSchemes

ActionScheme const * __actionSchemes[1 + 3] = {
	&ActionSchemes::s_FINISHED,
	&ActionSchemes::s_BLOCKED,
	&ActionSchemes::s_INITIALIZE,
	&ActionSchemes::s_take
};
ActionScheme const * *actionSchemes = &__actionSchemes[3];

namespace Actions {
const ActionSchemes::S_FINISHED a_FINISHED(FinishedOpId, "FINISHED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_BLOCKED a_BLOCKED(NoOpId, "BLOCKED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_INITIALIZE a_INITIALIZE(InitOpId, "INITIALIZE", NULL, NULL, NULL, finishedImmediate , pStopcdfImmediate, pStoppdfImmediate, 0.0 /* Initial */);
const ActionSchemes::S_take a_take_knife_counter_2_2(0, "(take knife counter 2 2)", action_take_knife_counter_2_2, NULL, getRecOrRefr0, finished0, pStopcdf0, pStoppdf0, -1 /* bad time model (normal 2 2) in 0 */);
const ActionSchemes::S_take a_take_knife_counter_2_5(1, "(take knife counter 2 5)", action_take_knife_counter_2_5, NULL, getRecOrRefr1, finished1, pStopcdf1, pStoppdf1, -1 /* bad time model (normal 2 5) in 1 */);
const ActionSchemes::S_take a_take_knife_counter_2_8(2, "(take knife counter 2 8)", action_take_knife_counter_2_8, NULL, getRecOrRefr2, finished2, pStopcdf2, pStoppdf2, -1 /* bad time model (normal 2 8) in 2 */);
const ActionSchemes::S_take a_take_knife_counter_2_10(3, "(take knife counter 2 10)", action_take_knife_counter_2_10, NULL, getRecOrRefr3, finished3, pStopcdf3, pStoppdf3, -1 /* bad time model (normal 2 10) in 3 */);
const ActionSchemes::S_take a_take_knife_counter_2_15(4, "(take knife counter 2 15)", action_take_knife_counter_2_15, NULL, getRecOrRefr4, finished4, pStopcdf4, pStoppdf4, -1 /* bad time model (normal 2 15) in 4 */);
const ActionSchemes::S_take a_take_knife_counter_5_2(5, "(take knife counter 5 2)", action_take_knife_counter_5_2, NULL, getRecOrRefr5, finished5, pStopcdf5, pStoppdf5, -1 /* bad time model (normal 5 2) in 5 */);
const ActionSchemes::S_take a_take_knife_counter_5_5(6, "(take knife counter 5 5)", action_take_knife_counter_5_5, NULL, getRecOrRefr6, finished6, pStopcdf6, pStoppdf6, -1 /* bad time model (normal 5 5) in 6 */);
const ActionSchemes::S_take a_take_knife_counter_5_8(7, "(take knife counter 5 8)", action_take_knife_counter_5_8, NULL, getRecOrRefr7, finished7, pStopcdf7, pStoppdf7, -1 /* bad time model (normal 5 8) in 7 */);
const ActionSchemes::S_take a_take_knife_counter_5_10(8, "(take knife counter 5 10)", action_take_knife_counter_5_10, NULL, getRecOrRefr8, finished8, pStopcdf8, pStoppdf8, -1 /* bad time model (normal 5 10) in 8 */);
const ActionSchemes::S_take a_take_knife_counter_5_15(9, "(take knife counter 5 15)", action_take_knife_counter_5_15, NULL, getRecOrRefr9, finished9, pStopcdf9, pStoppdf9, -1 /* bad time model (normal 5 15) in 9 */);
const ActionSchemes::S_take a_take_knife_counter_8_2(10, "(take knife counter 8 2)", action_take_knife_counter_8_2, NULL, getRecOrRefr10, finished10, pStopcdf10, pStoppdf10, -1 /* bad time model (normal 8 2) in 10 */);
const ActionSchemes::S_take a_take_knife_counter_8_5(11, "(take knife counter 8 5)", action_take_knife_counter_8_5, NULL, getRecOrRefr11, finished11, pStopcdf11, pStoppdf11, -1 /* bad time model (normal 8 5) in 11 */);
const ActionSchemes::S_take a_take_knife_counter_8_8(12, "(take knife counter 8 8)", action_take_knife_counter_8_8, NULL, getRecOrRefr12, finished12, pStopcdf12, pStoppdf12, -1 /* bad time model (normal 8 8) in 12 */);
const ActionSchemes::S_take a_take_knife_counter_8_10(13, "(take knife counter 8 10)", action_take_knife_counter_8_10, NULL, getRecOrRefr13, finished13, pStopcdf13, pStoppdf13, -1 /* bad time model (normal 8 10) in 13 */);
const ActionSchemes::S_take a_take_knife_counter_8_15(14, "(take knife counter 8 15)", action_take_knife_counter_8_15, NULL, getRecOrRefr14, finished14, pStopcdf14, pStoppdf14, -1 /* bad time model (normal 8 15) in 14 */);
const ActionSchemes::S_take a_take_knife_counter_10_2(15, "(take knife counter 10 2)", action_take_knife_counter_10_2, NULL, getRecOrRefr15, finished15, pStopcdf15, pStoppdf15, -1 /* bad time model (normal 10 2) in 15 */);
const ActionSchemes::S_take a_take_knife_counter_10_5(16, "(take knife counter 10 5)", action_take_knife_counter_10_5, NULL, getRecOrRefr16, finished16, pStopcdf16, pStoppdf16, -1 /* bad time model (normal 10 5) in 16 */);
const ActionSchemes::S_take a_take_knife_counter_10_8(17, "(take knife counter 10 8)", action_take_knife_counter_10_8, NULL, getRecOrRefr17, finished17, pStopcdf17, pStoppdf17, -1 /* bad time model (normal 10 8) in 17 */);
const ActionSchemes::S_take a_take_knife_counter_10_10(18, "(take knife counter 10 10)", action_take_knife_counter_10_10, NULL, getRecOrRefr18, finished18, pStopcdf18, pStoppdf18, -1 /* bad time model (normal 10 10) in 18 */);
const ActionSchemes::S_take a_take_knife_counter_10_15(19, "(take knife counter 10 15)", action_take_knife_counter_10_15, NULL, getRecOrRefr19, finished19, pStopcdf19, pStoppdf19, -1 /* bad time model (normal 10 15) in 19 */);
const ActionSchemes::S_take a_take_knife_counter_15_2(20, "(take knife counter 15 2)", action_take_knife_counter_15_2, NULL, getRecOrRefr20, finished20, pStopcdf20, pStoppdf20, -1 /* bad time model (normal 15 2) in 20 */);
const ActionSchemes::S_take a_take_knife_counter_15_5(21, "(take knife counter 15 5)", action_take_knife_counter_15_5, NULL, getRecOrRefr21, finished21, pStopcdf21, pStoppdf21, -1 /* bad time model (normal 15 5) in 21 */);
const ActionSchemes::S_take a_take_knife_counter_15_8(22, "(take knife counter 15 8)", action_take_knife_counter_15_8, NULL, getRecOrRefr22, finished22, pStopcdf22, pStoppdf22, -1 /* bad time model (normal 15 8) in 22 */);
const ActionSchemes::S_take a_take_knife_counter_15_10(23, "(take knife counter 15 10)", action_take_knife_counter_15_10, NULL, getRecOrRefr23, finished23, pStopcdf23, pStoppdf23, -1 /* bad time model (normal 15 10) in 23 */);
const ActionSchemes::S_take a_take_knife_counter_15_15(24, "(take knife counter 15 15)", action_take_knife_counter_15_15, NULL, getRecOrRefr24, finished24, pStopcdf24, pStoppdf24, -1 /* bad time model (normal 15 15) in 24 */);
} // namespace Actions

Action const * __actions[NOPS+3] = {
	&Actions::a_FINISHED,
	&Actions::a_BLOCKED,
	&Actions::a_INITIALIZE,
	&Actions::a_take_knife_counter_2_2,
	&Actions::a_take_knife_counter_2_5,
	&Actions::a_take_knife_counter_2_8,
	&Actions::a_take_knife_counter_2_10,
	&Actions::a_take_knife_counter_2_15,
	&Actions::a_take_knife_counter_5_2,
	&Actions::a_take_knife_counter_5_5,
	&Actions::a_take_knife_counter_5_8,
	&Actions::a_take_knife_counter_5_10,
	&Actions::a_take_knife_counter_5_15,
	&Actions::a_take_knife_counter_8_2,
	&Actions::a_take_knife_counter_8_5,
	&Actions::a_take_knife_counter_8_8,
	&Actions::a_take_knife_counter_8_10,
	&Actions::a_take_knife_counter_8_15,
	&Actions::a_take_knife_counter_10_2,
	&Actions::a_take_knife_counter_10_5,
	&Actions::a_take_knife_counter_10_8,
	&Actions::a_take_knife_counter_10_10,
	&Actions::a_take_knife_counter_10_15,
	&Actions::a_take_knife_counter_15_2,
	&Actions::a_take_knife_counter_15_5,
	&Actions::a_take_knife_counter_15_8,
	&Actions::a_take_knife_counter_15_10,
	&Actions::a_take_knife_counter_15_15
};
Action const * *actions = &__actions[3];

const int agentOps0[25] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24};
char const * const agentNames[NAGENTS] = {"NULL"};
int const nOpsForAgent[NAGENTS] = {25};
int const * const opsForAgent[NAGENTS] = {agentOps0};

bool isGoalState(size_t goal, StatePtr x) { (void) x;
	switch(goal) {
	case 0: /*carrots2*/
		return !(x->Sat_knife_counter);
	default: throw std::runtime_error("isGoalState: Unknown goal"); break;
	}
}

void sampleInitial(size_t initialState, StateRec *x1) {
	bzero(x1,sizeof(StateRec));
	switch(initialState) {
	case 0: /*carrots2*/
		x1->Sat_knife_counter = 1;
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
				accepted.Sat_knife_counter=1;
			} else {
				accepted= *parentAccepted;
		
				if (s->Sat_knife_counter==1) accepted.Sat_knife_counter=1;
				if (s->Sat_knife_counter==1) if (parentAccepted->Sat_knife_counter==1)  accepted.Sat_knife_counter=1;
			} // compute the heuristic
			if (accepted.Sat_knife_counter== 0) count++;
			if (accepted.Sat_knife_counter== 0) count++;
				if (accepted.Sat_knife_counter== 1 && s->Sat_knife_counter== 0) count++;
			if (accepted.Sat_knife_counter== 1 && s->Sat_knife_counter== 0  && (accepted.Sat_knife_counter==0)) count++;
			break;
		default: throw std::runtime_error("landmarkCountHeuristic: Unknown goal state");
		}
		break;
	default: throw std::runtime_error("landmarkCountHeuristic: Unknown initial state");
	}
	*parentAccepted = accepted;
	return count;
}
