const char * DOMAIN_NAME = "xyrouter";
const char * PROBLEM_NAME = "xyrouter";

char const * const objnames_object[] = {"11","12","13","14","15","21","22","23","24","25","31","32","33","34","35","41","42","43","44","45","51","52","53","54","55","0.7","2.1","3.5","4.9","6.3"};
char const * const objnames_position[] = {"11","12","13","14","15","21","22","23","24","25","31","32","33","34","35","41","42","43","44","45","51","52","53","54","55"};
char const * const objnames_coordinate[] = {"0.7","2.1","3.5","4.9","6.3"};

std::ostream& operator<<(std::ostream &o, StatePtr x) {
	if(x->Sat_11) o << " (at 11)"; else o<< " (not (at 11))"; 
	if(x->Sat_12) o << " (at 12)"; else o<< " (not (at 12))"; 
	if(x->Sat_13) o << " (at 13)"; else o<< " (not (at 13))"; 
	if(x->Sat_14) o << " (at 14)"; else o<< " (not (at 14))"; 
	if(x->Sat_15) o << " (at 15)"; else o<< " (not (at 15))"; 
	if(x->Sat_21) o << " (at 21)"; else o<< " (not (at 21))"; 
	if(x->Sat_22) o << " (at 22)"; else o<< " (not (at 22))"; 
	if(x->Sat_23) o << " (at 23)"; else o<< " (not (at 23))"; 
	if(x->Sat_24) o << " (at 24)"; else o<< " (not (at 24))"; 
	if(x->Sat_25) o << " (at 25)"; else o<< " (not (at 25))"; 
	if(x->Sat_31) o << " (at 31)"; else o<< " (not (at 31))"; 
	if(x->Sat_32) o << " (at 32)"; else o<< " (not (at 32))"; 
	if(x->Sat_33) o << " (at 33)"; else o<< " (not (at 33))"; 
	if(x->Sat_34) o << " (at 34)"; else o<< " (not (at 34))"; 
	if(x->Sat_35) o << " (at 35)"; else o<< " (not (at 35))"; 
	if(x->Sat_41) o << " (at 41)"; else o<< " (not (at 41))"; 
	if(x->Sat_42) o << " (at 42)"; else o<< " (not (at 42))"; 
	if(x->Sat_43) o << " (at 43)"; else o<< " (not (at 43))"; 
	if(x->Sat_44) o << " (at 44)"; else o<< " (not (at 44))"; 
	if(x->Sat_45) o << " (at 45)"; else o<< " (not (at 45))"; 
	if(x->Sat_51) o << " (at 51)"; else o<< " (not (at 51))"; 
	if(x->Sat_52) o << " (at 52)"; else o<< " (not (at 52))"; 
	if(x->Sat_53) o << " (at 53)"; else o<< " (not (at 53))"; 
	if(x->Sat_54) o << " (at 54)"; else o<< " (not (at 54))"; 
	if(x->Sat_55) o << " (at 55)"; else o<< " (not (at 55))"; 
	return o;
}
void writeState(std::ostream &o, StatePtr x, double wt) {
	char buf[16 + 3*0 + 0]; char *s = (char*) buf;
	if (std::isinf(wt)) strcpy(s, "inf"), s += 3; else s += sprintf(s, "%g", wt);
	*s = ' '; s++; *s =x->Sat_11 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_12 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_13 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_14 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_15 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_21 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_22 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_23 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_24 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_25 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_31 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_32 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_33 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_34 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_35 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_41 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_42 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_43 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_44 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_45 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_51 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_52 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_53 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_54 + '0'; s++;
	*s = ' '; s++; *s =x->Sat_55 + '0'; s++;
	*s = '\n'; *(++s) = '\0'; o << (char*) buf;
}
bool readState(std::istream &i, StateRec *x, double &wt) {
	char buf[4096]; char *s = (char*) buf;
	do i.getline(s, 4096); while (i.gcount() < 2 && i.good());
	if (!i.good()) return false;
	if (!memcmp(s, "inf", 3)) { wt = infinity, s+=3; } else wt = strtod(s, &s);
	char c;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_11 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_12 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_13 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_14 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_15 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_21 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_22 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_23 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_24 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_25 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_31 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_32 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_33 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_34 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_35 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_41 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_42 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_43 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_44 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_45 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_51 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_52 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_53 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_54 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_55 = c, s++; else return false;
	return true;
}
void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr) { // rt: Recency Vector (ACT-R Heuristic); t: ist the global time
	if (prev->Sat_11 ^ curr->Sat_11) rT[0] = t;
	if (prev->Sat_12 ^ curr->Sat_12) rT[1] = t;
	if (prev->Sat_13 ^ curr->Sat_13) rT[2] = t;
	if (prev->Sat_14 ^ curr->Sat_14) rT[3] = t;
	if (prev->Sat_15 ^ curr->Sat_15) rT[4] = t;
	if (prev->Sat_21 ^ curr->Sat_21) rT[5] = t;
	if (prev->Sat_22 ^ curr->Sat_22) rT[6] = t;
	if (prev->Sat_23 ^ curr->Sat_23) rT[7] = t;
	if (prev->Sat_24 ^ curr->Sat_24) rT[8] = t;
	if (prev->Sat_25 ^ curr->Sat_25) rT[9] = t;
	if (prev->Sat_31 ^ curr->Sat_31) rT[10] = t;
	if (prev->Sat_32 ^ curr->Sat_32) rT[11] = t;
	if (prev->Sat_33 ^ curr->Sat_33) rT[12] = t;
	if (prev->Sat_34 ^ curr->Sat_34) rT[13] = t;
	if (prev->Sat_35 ^ curr->Sat_35) rT[14] = t;
	if (prev->Sat_41 ^ curr->Sat_41) rT[15] = t;
	if (prev->Sat_42 ^ curr->Sat_42) rT[16] = t;
	if (prev->Sat_43 ^ curr->Sat_43) rT[17] = t;
	if (prev->Sat_44 ^ curr->Sat_44) rT[18] = t;
	if (prev->Sat_45 ^ curr->Sat_45) rT[19] = t;
	if (prev->Sat_51 ^ curr->Sat_51) rT[20] = t;
	if (prev->Sat_52 ^ curr->Sat_52) rT[21] = t;
	if (prev->Sat_53 ^ curr->Sat_53) rT[22] = t;
	if (prev->Sat_54 ^ curr->Sat_54) rT[23] = t;
	if (prev->Sat_55 ^ curr->Sat_55) rT[24] = t;
}
double getRefract(StatePtr a1, StatePtr a2) { // compare two action preconditions and returns the rerfractoriness 
	double r=0; int n=0;
	if (a1->Sat_11) {n++; if (a2->Sat_11) r++;}
	if (a1->Sat_12) {n++; if (a2->Sat_12) r++;}
	if (a1->Sat_13) {n++; if (a2->Sat_13) r++;}
	if (a1->Sat_14) {n++; if (a2->Sat_14) r++;}
	if (a1->Sat_15) {n++; if (a2->Sat_15) r++;}
	if (a1->Sat_21) {n++; if (a2->Sat_21) r++;}
	if (a1->Sat_22) {n++; if (a2->Sat_22) r++;}
	if (a1->Sat_23) {n++; if (a2->Sat_23) r++;}
	if (a1->Sat_24) {n++; if (a2->Sat_24) r++;}
	if (a1->Sat_25) {n++; if (a2->Sat_25) r++;}
	if (a1->Sat_31) {n++; if (a2->Sat_31) r++;}
	if (a1->Sat_32) {n++; if (a2->Sat_32) r++;}
	if (a1->Sat_33) {n++; if (a2->Sat_33) r++;}
	if (a1->Sat_34) {n++; if (a2->Sat_34) r++;}
	if (a1->Sat_35) {n++; if (a2->Sat_35) r++;}
	if (a1->Sat_41) {n++; if (a2->Sat_41) r++;}
	if (a1->Sat_42) {n++; if (a2->Sat_42) r++;}
	if (a1->Sat_43) {n++; if (a2->Sat_43) r++;}
	if (a1->Sat_44) {n++; if (a2->Sat_44) r++;}
	if (a1->Sat_45) {n++; if (a2->Sat_45) r++;}
	if (a1->Sat_51) {n++; if (a2->Sat_51) r++;}
	if (a1->Sat_52) {n++; if (a2->Sat_52) r++;}
	if (a1->Sat_53) {n++; if (a2->Sat_53) r++;}
	if (a1->Sat_54) {n++; if (a2->Sat_54) r++;}
	if (a1->Sat_55) {n++; if (a2->Sat_55) r++;}
	return (n==0) ? 0 : r/n;
}

char const * const initialStateNames[] = {"xyrouter"};
char const * const goalNames[] = {"xyrouter"};

// no :non-repeating has been used
bool canFollowAfter(int next, int prev) { (void)next; (void)prev; return true; }

int const FinishedOpId = -3;
int const NoOpId = -2;
int const InitOpId = -1;
char const * const __actionNames[NOPS+3] = {
	"(FINISHED)",
	"(BLOCKED)",
	"(INITIALIZE)",
	"(step 11 12)", // (:action step ?x ?y - position)
	"(step 11 21)", // (:action step ?x ?y - position)
	"(step 12 11)", // (:action step ?x ?y - position)
	"(step 12 13)", // (:action step ?x ?y - position)
	"(step 12 22)", // (:action step ?x ?y - position)
	"(step 13 12)", // (:action step ?x ?y - position)
	"(step 13 14)", // (:action step ?x ?y - position)
	"(step 13 23)", // (:action step ?x ?y - position)
	"(step 14 13)", // (:action step ?x ?y - position)
	"(step 14 15)", // (:action step ?x ?y - position)
	"(step 14 24)", // (:action step ?x ?y - position)
	"(step 15 14)", // (:action step ?x ?y - position)
	"(step 15 25)", // (:action step ?x ?y - position)
	"(step 21 11)", // (:action step ?x ?y - position)
	"(step 21 22)", // (:action step ?x ?y - position)
	"(step 21 31)", // (:action step ?x ?y - position)
	"(step 22 12)", // (:action step ?x ?y - position)
	"(step 22 21)", // (:action step ?x ?y - position)
	"(step 22 23)", // (:action step ?x ?y - position)
	"(step 22 32)", // (:action step ?x ?y - position)
	"(step 23 13)", // (:action step ?x ?y - position)
	"(step 23 22)", // (:action step ?x ?y - position)
	"(step 23 24)", // (:action step ?x ?y - position)
	"(step 23 33)", // (:action step ?x ?y - position)
	"(step 24 14)", // (:action step ?x ?y - position)
	"(step 24 23)", // (:action step ?x ?y - position)
	"(step 24 25)", // (:action step ?x ?y - position)
	"(step 24 34)", // (:action step ?x ?y - position)
	"(step 25 15)", // (:action step ?x ?y - position)
	"(step 25 24)", // (:action step ?x ?y - position)
	"(step 25 35)", // (:action step ?x ?y - position)
	"(step 31 21)", // (:action step ?x ?y - position)
	"(step 31 32)", // (:action step ?x ?y - position)
	"(step 31 41)", // (:action step ?x ?y - position)
	"(step 32 22)", // (:action step ?x ?y - position)
	"(step 32 31)", // (:action step ?x ?y - position)
	"(step 32 33)", // (:action step ?x ?y - position)
	"(step 32 42)", // (:action step ?x ?y - position)
	"(step 33 23)", // (:action step ?x ?y - position)
	"(step 33 32)", // (:action step ?x ?y - position)
	"(step 33 34)", // (:action step ?x ?y - position)
	"(step 33 43)", // (:action step ?x ?y - position)
	"(step 34 24)", // (:action step ?x ?y - position)
	"(step 34 33)", // (:action step ?x ?y - position)
	"(step 34 35)", // (:action step ?x ?y - position)
	"(step 34 44)", // (:action step ?x ?y - position)
	"(step 35 25)", // (:action step ?x ?y - position)
	"(step 35 34)", // (:action step ?x ?y - position)
	"(step 35 45)", // (:action step ?x ?y - position)
	"(step 41 31)", // (:action step ?x ?y - position)
	"(step 41 42)", // (:action step ?x ?y - position)
	"(step 41 51)", // (:action step ?x ?y - position)
	"(step 42 32)", // (:action step ?x ?y - position)
	"(step 42 41)", // (:action step ?x ?y - position)
	"(step 42 43)", // (:action step ?x ?y - position)
	"(step 42 52)", // (:action step ?x ?y - position)
	"(step 43 33)", // (:action step ?x ?y - position)
	"(step 43 42)", // (:action step ?x ?y - position)
	"(step 43 44)", // (:action step ?x ?y - position)
	"(step 43 53)", // (:action step ?x ?y - position)
	"(step 44 34)", // (:action step ?x ?y - position)
	"(step 44 43)", // (:action step ?x ?y - position)
	"(step 44 45)", // (:action step ?x ?y - position)
	"(step 44 54)", // (:action step ?x ?y - position)
	"(step 45 35)", // (:action step ?x ?y - position)
	"(step 45 44)", // (:action step ?x ?y - position)
	"(step 45 55)", // (:action step ?x ?y - position)
	"(step 51 41)", // (:action step ?x ?y - position)
	"(step 51 52)", // (:action step ?x ?y - position)
	"(step 52 42)", // (:action step ?x ?y - position)
	"(step 52 51)", // (:action step ?x ?y - position)
	"(step 52 53)", // (:action step ?x ?y - position)
	"(step 53 43)", // (:action step ?x ?y - position)
	"(step 53 52)", // (:action step ?x ?y - position)
	"(step 53 54)", // (:action step ?x ?y - position)
	"(step 54 44)", // (:action step ?x ?y - position)
	"(step 54 53)", // (:action step ?x ?y - position)
	"(step 54 55)", // (:action step ?x ?y - position)
	"(step 55 45)", // (:action step ?x ?y - position)
	"(step 55 54)" // (:action step ?x ?y - position)
};
char const * const * const actionNames = &__actionNames[3];


/* (step 11 12), id 0 */
double action_step_11_12(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_11) *specificity=0.50; 

	if(x->Sat_11) {
		*x1 = *x;
		x1->Sat_12 = 1;
		x1->Sat_11 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 11 21), id 1 */
double action_step_11_21(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_11) *specificity=0.50; 

	if(x->Sat_11) {
		*x1 = *x;
		x1->Sat_21 = 1;
		x1->Sat_11 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 12 11), id 2 */
double action_step_12_11(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_12) *specificity=0.33; 

	if(x->Sat_12) {
		*x1 = *x;
		x1->Sat_11 = 1;
		x1->Sat_12 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 12 13), id 3 */
double action_step_12_13(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_12) *specificity=0.33; 

	if(x->Sat_12) {
		*x1 = *x;
		x1->Sat_13 = 1;
		x1->Sat_12 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 12 22), id 4 */
double action_step_12_22(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_12) *specificity=0.33; 

	if(x->Sat_12) {
		*x1 = *x;
		x1->Sat_22 = 1;
		x1->Sat_12 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 13 12), id 5 */
double action_step_13_12(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_13) *specificity=0.33; 

	if(x->Sat_13) {
		*x1 = *x;
		x1->Sat_12 = 1;
		x1->Sat_13 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 13 14), id 6 */
double action_step_13_14(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_13) *specificity=0.33; 

	if(x->Sat_13) {
		*x1 = *x;
		x1->Sat_14 = 1;
		x1->Sat_13 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 13 23), id 7 */
double action_step_13_23(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_13) *specificity=0.33; 

	if(x->Sat_13) {
		*x1 = *x;
		x1->Sat_23 = 1;
		x1->Sat_13 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 14 13), id 8 */
double action_step_14_13(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_14) *specificity=0.33; 

	if(x->Sat_14) {
		*x1 = *x;
		x1->Sat_13 = 1;
		x1->Sat_14 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 14 15), id 9 */
double action_step_14_15(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_14) *specificity=0.33; 

	if(x->Sat_14) {
		*x1 = *x;
		x1->Sat_15 = 1;
		x1->Sat_14 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 14 24), id 10 */
double action_step_14_24(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_14) *specificity=0.33; 

	if(x->Sat_14) {
		*x1 = *x;
		x1->Sat_24 = 1;
		x1->Sat_14 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 15 14), id 11 */
double action_step_15_14(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_15) *specificity=0.50; 

	if(x->Sat_15) {
		*x1 = *x;
		x1->Sat_14 = 1;
		x1->Sat_15 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 15 25), id 12 */
double action_step_15_25(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_15) *specificity=0.50; 

	if(x->Sat_15) {
		*x1 = *x;
		x1->Sat_25 = 1;
		x1->Sat_15 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 21 11), id 13 */
double action_step_21_11(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_21) *specificity=0.33; 

	if(x->Sat_21) {
		*x1 = *x;
		x1->Sat_11 = 1;
		x1->Sat_21 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 21 22), id 14 */
double action_step_21_22(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_21) *specificity=0.33; 

	if(x->Sat_21) {
		*x1 = *x;
		x1->Sat_22 = 1;
		x1->Sat_21 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 21 31), id 15 */
double action_step_21_31(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_21) *specificity=0.33; 

	if(x->Sat_21) {
		*x1 = *x;
		x1->Sat_31 = 1;
		x1->Sat_21 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 22 12), id 16 */
double action_step_22_12(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_22) *specificity=0.25; 

	if(x->Sat_22) {
		*x1 = *x;
		x1->Sat_12 = 1;
		x1->Sat_22 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 22 21), id 17 */
double action_step_22_21(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_22) *specificity=0.25; 

	if(x->Sat_22) {
		*x1 = *x;
		x1->Sat_21 = 1;
		x1->Sat_22 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 22 23), id 18 */
double action_step_22_23(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_22) *specificity=0.25; 

	if(x->Sat_22) {
		*x1 = *x;
		x1->Sat_23 = 1;
		x1->Sat_22 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 22 32), id 19 */
double action_step_22_32(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_22) *specificity=0.25; 

	if(x->Sat_22) {
		*x1 = *x;
		x1->Sat_32 = 1;
		x1->Sat_22 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 23 13), id 20 */
double action_step_23_13(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_23) *specificity=0.25; 

	if(x->Sat_23) {
		*x1 = *x;
		x1->Sat_13 = 1;
		x1->Sat_23 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 23 22), id 21 */
double action_step_23_22(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_23) *specificity=0.25; 

	if(x->Sat_23) {
		*x1 = *x;
		x1->Sat_22 = 1;
		x1->Sat_23 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 23 24), id 22 */
double action_step_23_24(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_23) *specificity=0.25; 

	if(x->Sat_23) {
		*x1 = *x;
		x1->Sat_24 = 1;
		x1->Sat_23 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 23 33), id 23 */
double action_step_23_33(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_23) *specificity=0.25; 

	if(x->Sat_23) {
		*x1 = *x;
		x1->Sat_33 = 1;
		x1->Sat_23 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 24 14), id 24 */
double action_step_24_14(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_24) *specificity=0.25; 

	if(x->Sat_24) {
		*x1 = *x;
		x1->Sat_14 = 1;
		x1->Sat_24 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 24 23), id 25 */
double action_step_24_23(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_24) *specificity=0.25; 

	if(x->Sat_24) {
		*x1 = *x;
		x1->Sat_23 = 1;
		x1->Sat_24 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 24 25), id 26 */
double action_step_24_25(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_24) *specificity=0.25; 

	if(x->Sat_24) {
		*x1 = *x;
		x1->Sat_25 = 1;
		x1->Sat_24 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 24 34), id 27 */
double action_step_24_34(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_24) *specificity=0.25; 

	if(x->Sat_24) {
		*x1 = *x;
		x1->Sat_34 = 1;
		x1->Sat_24 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 25 15), id 28 */
double action_step_25_15(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_25) *specificity=0.33; 

	if(x->Sat_25) {
		*x1 = *x;
		x1->Sat_15 = 1;
		x1->Sat_25 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 25 24), id 29 */
double action_step_25_24(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_25) *specificity=0.33; 

	if(x->Sat_25) {
		*x1 = *x;
		x1->Sat_24 = 1;
		x1->Sat_25 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 25 35), id 30 */
double action_step_25_35(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_25) *specificity=0.33; 

	if(x->Sat_25) {
		*x1 = *x;
		x1->Sat_35 = 1;
		x1->Sat_25 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 31 21), id 31 */
double action_step_31_21(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_31) *specificity=0.33; 

	if(x->Sat_31) {
		*x1 = *x;
		x1->Sat_21 = 1;
		x1->Sat_31 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 31 32), id 32 */
double action_step_31_32(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_31) *specificity=0.33; 

	if(x->Sat_31) {
		*x1 = *x;
		x1->Sat_32 = 1;
		x1->Sat_31 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 31 41), id 33 */
double action_step_31_41(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_31) *specificity=0.33; 

	if(x->Sat_31) {
		*x1 = *x;
		x1->Sat_41 = 1;
		x1->Sat_31 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 32 22), id 34 */
double action_step_32_22(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_32) *specificity=0.25; 

	if(x->Sat_32) {
		*x1 = *x;
		x1->Sat_22 = 1;
		x1->Sat_32 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 32 31), id 35 */
double action_step_32_31(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_32) *specificity=0.25; 

	if(x->Sat_32) {
		*x1 = *x;
		x1->Sat_31 = 1;
		x1->Sat_32 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 32 33), id 36 */
double action_step_32_33(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_32) *specificity=0.25; 

	if(x->Sat_32) {
		*x1 = *x;
		x1->Sat_33 = 1;
		x1->Sat_32 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 32 42), id 37 */
double action_step_32_42(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_32) *specificity=0.25; 

	if(x->Sat_32) {
		*x1 = *x;
		x1->Sat_42 = 1;
		x1->Sat_32 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 33 23), id 38 */
double action_step_33_23(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_33) *specificity=0.25; 

	if(x->Sat_33) {
		*x1 = *x;
		x1->Sat_23 = 1;
		x1->Sat_33 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 33 32), id 39 */
double action_step_33_32(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_33) *specificity=0.25; 

	if(x->Sat_33) {
		*x1 = *x;
		x1->Sat_32 = 1;
		x1->Sat_33 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 33 34), id 40 */
double action_step_33_34(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_33) *specificity=0.25; 

	if(x->Sat_33) {
		*x1 = *x;
		x1->Sat_34 = 1;
		x1->Sat_33 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 33 43), id 41 */
double action_step_33_43(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_33) *specificity=0.25; 

	if(x->Sat_33) {
		*x1 = *x;
		x1->Sat_43 = 1;
		x1->Sat_33 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 34 24), id 42 */
double action_step_34_24(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_34) *specificity=0.25; 

	if(x->Sat_34) {
		*x1 = *x;
		x1->Sat_24 = 1;
		x1->Sat_34 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 34 33), id 43 */
double action_step_34_33(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_34) *specificity=0.25; 

	if(x->Sat_34) {
		*x1 = *x;
		x1->Sat_33 = 1;
		x1->Sat_34 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 34 35), id 44 */
double action_step_34_35(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_34) *specificity=0.25; 

	if(x->Sat_34) {
		*x1 = *x;
		x1->Sat_35 = 1;
		x1->Sat_34 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 34 44), id 45 */
double action_step_34_44(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_34) *specificity=0.25; 

	if(x->Sat_34) {
		*x1 = *x;
		x1->Sat_44 = 1;
		x1->Sat_34 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 35 25), id 46 */
double action_step_35_25(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_35) *specificity=0.33; 

	if(x->Sat_35) {
		*x1 = *x;
		x1->Sat_25 = 1;
		x1->Sat_35 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 35 34), id 47 */
double action_step_35_34(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_35) *specificity=0.33; 

	if(x->Sat_35) {
		*x1 = *x;
		x1->Sat_34 = 1;
		x1->Sat_35 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 35 45), id 48 */
double action_step_35_45(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_35) *specificity=0.33; 

	if(x->Sat_35) {
		*x1 = *x;
		x1->Sat_45 = 1;
		x1->Sat_35 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 41 31), id 49 */
double action_step_41_31(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_41) *specificity=0.33; 

	if(x->Sat_41) {
		*x1 = *x;
		x1->Sat_31 = 1;
		x1->Sat_41 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 41 42), id 50 */
double action_step_41_42(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_41) *specificity=0.33; 

	if(x->Sat_41) {
		*x1 = *x;
		x1->Sat_42 = 1;
		x1->Sat_41 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 41 51), id 51 */
double action_step_41_51(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_41) *specificity=0.33; 

	if(x->Sat_41) {
		*x1 = *x;
		x1->Sat_51 = 1;
		x1->Sat_41 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 42 32), id 52 */
double action_step_42_32(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_42) *specificity=0.25; 

	if(x->Sat_42) {
		*x1 = *x;
		x1->Sat_32 = 1;
		x1->Sat_42 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 42 41), id 53 */
double action_step_42_41(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_42) *specificity=0.25; 

	if(x->Sat_42) {
		*x1 = *x;
		x1->Sat_41 = 1;
		x1->Sat_42 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 42 43), id 54 */
double action_step_42_43(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_42) *specificity=0.25; 

	if(x->Sat_42) {
		*x1 = *x;
		x1->Sat_43 = 1;
		x1->Sat_42 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 42 52), id 55 */
double action_step_42_52(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_42) *specificity=0.25; 

	if(x->Sat_42) {
		*x1 = *x;
		x1->Sat_52 = 1;
		x1->Sat_42 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 43 33), id 56 */
double action_step_43_33(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_43) *specificity=0.25; 

	if(x->Sat_43) {
		*x1 = *x;
		x1->Sat_33 = 1;
		x1->Sat_43 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 43 42), id 57 */
double action_step_43_42(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_43) *specificity=0.25; 

	if(x->Sat_43) {
		*x1 = *x;
		x1->Sat_42 = 1;
		x1->Sat_43 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 43 44), id 58 */
double action_step_43_44(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_43) *specificity=0.25; 

	if(x->Sat_43) {
		*x1 = *x;
		x1->Sat_44 = 1;
		x1->Sat_43 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 43 53), id 59 */
double action_step_43_53(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_43) *specificity=0.25; 

	if(x->Sat_43) {
		*x1 = *x;
		x1->Sat_53 = 1;
		x1->Sat_43 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 44 34), id 60 */
double action_step_44_34(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_44) *specificity=0.25; 

	if(x->Sat_44) {
		*x1 = *x;
		x1->Sat_34 = 1;
		x1->Sat_44 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 44 43), id 61 */
double action_step_44_43(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_44) *specificity=0.25; 

	if(x->Sat_44) {
		*x1 = *x;
		x1->Sat_43 = 1;
		x1->Sat_44 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 44 45), id 62 */
double action_step_44_45(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_44) *specificity=0.25; 

	if(x->Sat_44) {
		*x1 = *x;
		x1->Sat_45 = 1;
		x1->Sat_44 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 44 54), id 63 */
double action_step_44_54(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_44) *specificity=0.25; 

	if(x->Sat_44) {
		*x1 = *x;
		x1->Sat_54 = 1;
		x1->Sat_44 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 45 35), id 64 */
double action_step_45_35(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_45) *specificity=0.33; 

	if(x->Sat_45) {
		*x1 = *x;
		x1->Sat_35 = 1;
		x1->Sat_45 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 45 44), id 65 */
double action_step_45_44(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_45) *specificity=0.33; 

	if(x->Sat_45) {
		*x1 = *x;
		x1->Sat_44 = 1;
		x1->Sat_45 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 45 55), id 66 */
double action_step_45_55(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_45) *specificity=0.33; 

	if(x->Sat_45) {
		*x1 = *x;
		x1->Sat_55 = 1;
		x1->Sat_45 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 51 41), id 67 */
double action_step_51_41(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_51) *specificity=0.50; 

	if(x->Sat_51) {
		*x1 = *x;
		x1->Sat_41 = 1;
		x1->Sat_51 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 51 52), id 68 */
double action_step_51_52(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_51) *specificity=0.50; 

	if(x->Sat_51) {
		*x1 = *x;
		x1->Sat_52 = 1;
		x1->Sat_51 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 52 42), id 69 */
double action_step_52_42(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_52) *specificity=0.33; 

	if(x->Sat_52) {
		*x1 = *x;
		x1->Sat_42 = 1;
		x1->Sat_52 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 52 51), id 70 */
double action_step_52_51(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_52) *specificity=0.33; 

	if(x->Sat_52) {
		*x1 = *x;
		x1->Sat_51 = 1;
		x1->Sat_52 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 52 53), id 71 */
double action_step_52_53(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_52) *specificity=0.33; 

	if(x->Sat_52) {
		*x1 = *x;
		x1->Sat_53 = 1;
		x1->Sat_52 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 53 43), id 72 */
double action_step_53_43(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_53) *specificity=0.33; 

	if(x->Sat_53) {
		*x1 = *x;
		x1->Sat_43 = 1;
		x1->Sat_53 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 53 52), id 73 */
double action_step_53_52(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_53) *specificity=0.33; 

	if(x->Sat_53) {
		*x1 = *x;
		x1->Sat_52 = 1;
		x1->Sat_53 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 53 54), id 74 */
double action_step_53_54(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_53) *specificity=0.33; 

	if(x->Sat_53) {
		*x1 = *x;
		x1->Sat_54 = 1;
		x1->Sat_53 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 54 44), id 75 */
double action_step_54_44(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_54) *specificity=0.33; 

	if(x->Sat_54) {
		*x1 = *x;
		x1->Sat_44 = 1;
		x1->Sat_54 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 54 53), id 76 */
double action_step_54_53(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_54) *specificity=0.33; 

	if(x->Sat_54) {
		*x1 = *x;
		x1->Sat_53 = 1;
		x1->Sat_54 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 54 55), id 77 */
double action_step_54_55(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_54) *specificity=0.33; 

	if(x->Sat_54) {
		*x1 = *x;
		x1->Sat_55 = 1;
		x1->Sat_54 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 55 45), id 78 */
double action_step_55_45(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_55) *specificity=0.50; 

	if(x->Sat_55) {
		*x1 = *x;
		x1->Sat_45 = 1;
		x1->Sat_55 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (step 55 54), id 79 */
double action_step_55_54(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_55) *specificity=0.50; 

	if(x->Sat_55) {
		*x1 = *x;
		x1->Sat_54 = 1;
		x1->Sat_55 = 0;
		return 1;
	} else {
		return 0;
	}
}

bool finishedInitial(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished0(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished1(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished2(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished3(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished4(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished5(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished6(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished7(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished8(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished9(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished10(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished11(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished12(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished13(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished14(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished15(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished16(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished17(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished18(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished19(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished20(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished21(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished22(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished23(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished24(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished25(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished26(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished27(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished28(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished29(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished30(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished31(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished32(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished33(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished34(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished35(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished36(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished37(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished38(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished39(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished40(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished41(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished42(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished43(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished44(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished45(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished46(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished47(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished48(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished49(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished50(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished51(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished52(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished53(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished54(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished55(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished56(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished57(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished58(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished59(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished60(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished61(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished62(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished63(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished64(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished65(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished66(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished67(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished68(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished69(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished70(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished71(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished72(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished73(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished74(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished75(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished76(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished77(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished78(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finished79(double _1, double *p=NULL) { return exponentialModel(_1,0.1,p); }
bool finishedImmediate(double, double* p) { if (p)*p=1.0; return true; }
double pStopcdfInitial(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf0(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf1(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf2(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf3(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf4(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf5(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf6(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf7(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf8(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf9(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf10(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf11(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf12(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf13(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf14(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf15(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf16(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf17(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf18(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf19(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf20(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf21(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf22(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf23(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf24(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf25(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf26(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf27(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf28(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf29(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf30(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf31(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf32(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf33(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf34(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf35(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf36(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf37(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf38(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf39(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf40(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf41(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf42(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf43(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf44(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf45(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf46(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf47(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf48(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf49(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf50(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf51(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf52(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf53(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf54(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf55(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf56(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf57(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf58(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf59(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf60(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf61(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf62(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf63(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf64(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf65(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf66(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf67(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf68(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf69(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf70(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf71(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf72(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf73(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf74(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf75(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf76(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf77(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf78(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdf79(double startTime, double curTime) { return exponentialcdf(curTime- startTime,0.1); }
double pStopcdfImmediate(double startTime, double curTime){ return (curTime > startTime) ? 1.0 : 0.0; }
double pStoppdfInitial(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf0(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf1(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf2(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf3(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf4(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf5(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf6(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf7(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf8(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf9(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf10(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf11(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf12(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf13(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf14(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf15(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf16(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf17(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf18(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf19(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf20(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf21(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf22(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf23(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf24(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf25(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf26(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf27(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf28(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf29(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf30(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf31(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf32(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf33(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf34(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf35(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf36(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf37(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf38(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf39(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf40(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf41(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf42(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf43(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf44(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf45(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf46(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf47(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf48(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf49(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf50(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf51(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf52(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf53(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf54(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf55(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf56(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf57(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf58(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf59(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf60(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf61(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf62(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf63(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf64(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf65(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf66(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf67(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf68(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf69(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf70(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf71(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf72(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf73(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf74(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf75(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf76(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf77(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf78(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdf79(double startTime, double curTime) { return exponentialpdf(curTime - startTime,0.1); }
double pStoppdfImmediate(double startTime, double curTime){ return (curTime > startTime) ? 1.0 : 0.0; }

// Implementations for the Refractoriness and Recency (ACT-R Heuristics)
// The first paramter is the current state and the second one is the state for the Refractoriness
double getRecOrRefr0 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_11=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr1 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_11=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr2 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_12=1; 
	} else 
		 return (exp(rT[1]-t))/1;
	return -1;
}
double getRecOrRefr3 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_12=1; 
	} else 
		 return (exp(rT[1]-t))/1;
	return -1;
}
double getRecOrRefr4 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_12=1; 
	} else 
		 return (exp(rT[1]-t))/1;
	return -1;
}
double getRecOrRefr5 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_13=1; 
	} else 
		 return (exp(rT[2]-t))/1;
	return -1;
}
double getRecOrRefr6 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_13=1; 
	} else 
		 return (exp(rT[2]-t))/1;
	return -1;
}
double getRecOrRefr7 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_13=1; 
	} else 
		 return (exp(rT[2]-t))/1;
	return -1;
}
double getRecOrRefr8 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_14=1; 
	} else 
		 return (exp(rT[3]-t))/1;
	return -1;
}
double getRecOrRefr9 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_14=1; 
	} else 
		 return (exp(rT[3]-t))/1;
	return -1;
}
double getRecOrRefr10 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_14=1; 
	} else 
		 return (exp(rT[3]-t))/1;
	return -1;
}
double getRecOrRefr11 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_15=1; 
	} else 
		 return (exp(rT[4]-t))/1;
	return -1;
}
double getRecOrRefr12 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_15=1; 
	} else 
		 return (exp(rT[4]-t))/1;
	return -1;
}
double getRecOrRefr13 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_21=1; 
	} else 
		 return (exp(rT[5]-t))/1;
	return -1;
}
double getRecOrRefr14 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_21=1; 
	} else 
		 return (exp(rT[5]-t))/1;
	return -1;
}
double getRecOrRefr15 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_21=1; 
	} else 
		 return (exp(rT[5]-t))/1;
	return -1;
}
double getRecOrRefr16 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_22=1; 
	} else 
		 return (exp(rT[6]-t))/1;
	return -1;
}
double getRecOrRefr17 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_22=1; 
	} else 
		 return (exp(rT[6]-t))/1;
	return -1;
}
double getRecOrRefr18 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_22=1; 
	} else 
		 return (exp(rT[6]-t))/1;
	return -1;
}
double getRecOrRefr19 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_22=1; 
	} else 
		 return (exp(rT[6]-t))/1;
	return -1;
}
double getRecOrRefr20 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_23=1; 
	} else 
		 return (exp(rT[7]-t))/1;
	return -1;
}
double getRecOrRefr21 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_23=1; 
	} else 
		 return (exp(rT[7]-t))/1;
	return -1;
}
double getRecOrRefr22 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_23=1; 
	} else 
		 return (exp(rT[7]-t))/1;
	return -1;
}
double getRecOrRefr23 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_23=1; 
	} else 
		 return (exp(rT[7]-t))/1;
	return -1;
}
double getRecOrRefr24 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_24=1; 
	} else 
		 return (exp(rT[8]-t))/1;
	return -1;
}
double getRecOrRefr25 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_24=1; 
	} else 
		 return (exp(rT[8]-t))/1;
	return -1;
}
double getRecOrRefr26 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_24=1; 
	} else 
		 return (exp(rT[8]-t))/1;
	return -1;
}
double getRecOrRefr27 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_24=1; 
	} else 
		 return (exp(rT[8]-t))/1;
	return -1;
}
double getRecOrRefr28 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_25=1; 
	} else 
		 return (exp(rT[9]-t))/1;
	return -1;
}
double getRecOrRefr29 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_25=1; 
	} else 
		 return (exp(rT[9]-t))/1;
	return -1;
}
double getRecOrRefr30 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_25=1; 
	} else 
		 return (exp(rT[9]-t))/1;
	return -1;
}
double getRecOrRefr31 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_31=1; 
	} else 
		 return (exp(rT[10]-t))/1;
	return -1;
}
double getRecOrRefr32 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_31=1; 
	} else 
		 return (exp(rT[10]-t))/1;
	return -1;
}
double getRecOrRefr33 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_31=1; 
	} else 
		 return (exp(rT[10]-t))/1;
	return -1;
}
double getRecOrRefr34 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_32=1; 
	} else 
		 return (exp(rT[11]-t))/1;
	return -1;
}
double getRecOrRefr35 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_32=1; 
	} else 
		 return (exp(rT[11]-t))/1;
	return -1;
}
double getRecOrRefr36 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_32=1; 
	} else 
		 return (exp(rT[11]-t))/1;
	return -1;
}
double getRecOrRefr37 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_32=1; 
	} else 
		 return (exp(rT[11]-t))/1;
	return -1;
}
double getRecOrRefr38 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_33=1; 
	} else 
		 return (exp(rT[12]-t))/1;
	return -1;
}
double getRecOrRefr39 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_33=1; 
	} else 
		 return (exp(rT[12]-t))/1;
	return -1;
}
double getRecOrRefr40 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_33=1; 
	} else 
		 return (exp(rT[12]-t))/1;
	return -1;
}
double getRecOrRefr41 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_33=1; 
	} else 
		 return (exp(rT[12]-t))/1;
	return -1;
}
double getRecOrRefr42 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_34=1; 
	} else 
		 return (exp(rT[13]-t))/1;
	return -1;
}
double getRecOrRefr43 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_34=1; 
	} else 
		 return (exp(rT[13]-t))/1;
	return -1;
}
double getRecOrRefr44 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_34=1; 
	} else 
		 return (exp(rT[13]-t))/1;
	return -1;
}
double getRecOrRefr45 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_34=1; 
	} else 
		 return (exp(rT[13]-t))/1;
	return -1;
}
double getRecOrRefr46 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_35=1; 
	} else 
		 return (exp(rT[14]-t))/1;
	return -1;
}
double getRecOrRefr47 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_35=1; 
	} else 
		 return (exp(rT[14]-t))/1;
	return -1;
}
double getRecOrRefr48 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_35=1; 
	} else 
		 return (exp(rT[14]-t))/1;
	return -1;
}
double getRecOrRefr49 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_41=1; 
	} else 
		 return (exp(rT[15]-t))/1;
	return -1;
}
double getRecOrRefr50 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_41=1; 
	} else 
		 return (exp(rT[15]-t))/1;
	return -1;
}
double getRecOrRefr51 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_41=1; 
	} else 
		 return (exp(rT[15]-t))/1;
	return -1;
}
double getRecOrRefr52 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_42=1; 
	} else 
		 return (exp(rT[16]-t))/1;
	return -1;
}
double getRecOrRefr53 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_42=1; 
	} else 
		 return (exp(rT[16]-t))/1;
	return -1;
}
double getRecOrRefr54 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_42=1; 
	} else 
		 return (exp(rT[16]-t))/1;
	return -1;
}
double getRecOrRefr55 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_42=1; 
	} else 
		 return (exp(rT[16]-t))/1;
	return -1;
}
double getRecOrRefr56 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_43=1; 
	} else 
		 return (exp(rT[17]-t))/1;
	return -1;
}
double getRecOrRefr57 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_43=1; 
	} else 
		 return (exp(rT[17]-t))/1;
	return -1;
}
double getRecOrRefr58 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_43=1; 
	} else 
		 return (exp(rT[17]-t))/1;
	return -1;
}
double getRecOrRefr59 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_43=1; 
	} else 
		 return (exp(rT[17]-t))/1;
	return -1;
}
double getRecOrRefr60 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_44=1; 
	} else 
		 return (exp(rT[18]-t))/1;
	return -1;
}
double getRecOrRefr61 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_44=1; 
	} else 
		 return (exp(rT[18]-t))/1;
	return -1;
}
double getRecOrRefr62 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_44=1; 
	} else 
		 return (exp(rT[18]-t))/1;
	return -1;
}
double getRecOrRefr63 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_44=1; 
	} else 
		 return (exp(rT[18]-t))/1;
	return -1;
}
double getRecOrRefr64 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_45=1; 
	} else 
		 return (exp(rT[19]-t))/1;
	return -1;
}
double getRecOrRefr65 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_45=1; 
	} else 
		 return (exp(rT[19]-t))/1;
	return -1;
}
double getRecOrRefr66 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_45=1; 
	} else 
		 return (exp(rT[19]-t))/1;
	return -1;
}
double getRecOrRefr67 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_51=1; 
	} else 
		 return (exp(rT[20]-t))/1;
	return -1;
}
double getRecOrRefr68 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_51=1; 
	} else 
		 return (exp(rT[20]-t))/1;
	return -1;
}
double getRecOrRefr69 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_52=1; 
	} else 
		 return (exp(rT[21]-t))/1;
	return -1;
}
double getRecOrRefr70 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_52=1; 
	} else 
		 return (exp(rT[21]-t))/1;
	return -1;
}
double getRecOrRefr71 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_52=1; 
	} else 
		 return (exp(rT[21]-t))/1;
	return -1;
}
double getRecOrRefr72 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_53=1; 
	} else 
		 return (exp(rT[22]-t))/1;
	return -1;
}
double getRecOrRefr73 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_53=1; 
	} else 
		 return (exp(rT[22]-t))/1;
	return -1;
}
double getRecOrRefr74 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_53=1; 
	} else 
		 return (exp(rT[22]-t))/1;
	return -1;
}
double getRecOrRefr75 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_54=1; 
	} else 
		 return (exp(rT[23]-t))/1;
	return -1;
}
double getRecOrRefr76 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_54=1; 
	} else 
		 return (exp(rT[23]-t))/1;
	return -1;
}
double getRecOrRefr77 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_54=1; 
	} else 
		 return (exp(rT[23]-t))/1;
	return -1;
}
double getRecOrRefr78 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_55=1; 
	} else 
		 return (exp(rT[24]-t))/1;
	return -1;
}
double getRecOrRefr79 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_55=1; 
	} else 
		 return (exp(rT[24]-t))/1;
	return -1;
}


namespace ActionSchemes {
const ActionScheme s_FINISHED("FINISHED", std::vector<paramType>());
const ActionScheme s_BLOCKED("BLOCKED", std::vector<paramType>());
const ActionScheme s_INITIALIZE("INITIALIZE", std::vector<paramType>());
boost::array<paramType,2> __params_step = {std::make_pair("x","position"),std::make_pair("y","position")};
const ActionScheme s_step("step", std::vector<paramType>(__params_step.begin(), __params_step.end()));
} // namespace ActionSchemes

ActionScheme const * __actionSchemes[1 + 3] = {
	&ActionSchemes::s_FINISHED,
	&ActionSchemes::s_BLOCKED,
	&ActionSchemes::s_INITIALIZE,
	&ActionSchemes::s_step
};
ActionScheme const * *actionSchemes = &__actionSchemes[3];

namespace Actions {
const ActionSchemes::S_FINISHED a_FINISHED(FinishedOpId, "FINISHED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_BLOCKED a_BLOCKED(NoOpId, "BLOCKED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_INITIALIZE a_INITIALIZE(InitOpId, "INITIALIZE", NULL, NULL, NULL, finishedInitial , pStopcdfInitial, pStoppdfInitial, 1-(0.1) /* Initial */);
const ActionSchemes::S_step a_step_11_12(0, "(step 11 12)", action_step_11_12, NULL, getRecOrRefr0, finished0, pStopcdf0, pStoppdf0, 1-(0.1) /* 0 */);
const ActionSchemes::S_step a_step_11_21(1, "(step 11 21)", action_step_11_21, NULL, getRecOrRefr1, finished1, pStopcdf1, pStoppdf1, 1-(0.1) /* 1 */);
const ActionSchemes::S_step a_step_12_11(2, "(step 12 11)", action_step_12_11, NULL, getRecOrRefr2, finished2, pStopcdf2, pStoppdf2, 1-(0.1) /* 2 */);
const ActionSchemes::S_step a_step_12_13(3, "(step 12 13)", action_step_12_13, NULL, getRecOrRefr3, finished3, pStopcdf3, pStoppdf3, 1-(0.1) /* 3 */);
const ActionSchemes::S_step a_step_12_22(4, "(step 12 22)", action_step_12_22, NULL, getRecOrRefr4, finished4, pStopcdf4, pStoppdf4, 1-(0.1) /* 4 */);
const ActionSchemes::S_step a_step_13_12(5, "(step 13 12)", action_step_13_12, NULL, getRecOrRefr5, finished5, pStopcdf5, pStoppdf5, 1-(0.1) /* 5 */);
const ActionSchemes::S_step a_step_13_14(6, "(step 13 14)", action_step_13_14, NULL, getRecOrRefr6, finished6, pStopcdf6, pStoppdf6, 1-(0.1) /* 6 */);
const ActionSchemes::S_step a_step_13_23(7, "(step 13 23)", action_step_13_23, NULL, getRecOrRefr7, finished7, pStopcdf7, pStoppdf7, 1-(0.1) /* 7 */);
const ActionSchemes::S_step a_step_14_13(8, "(step 14 13)", action_step_14_13, NULL, getRecOrRefr8, finished8, pStopcdf8, pStoppdf8, 1-(0.1) /* 8 */);
const ActionSchemes::S_step a_step_14_15(9, "(step 14 15)", action_step_14_15, NULL, getRecOrRefr9, finished9, pStopcdf9, pStoppdf9, 1-(0.1) /* 9 */);
const ActionSchemes::S_step a_step_14_24(10, "(step 14 24)", action_step_14_24, NULL, getRecOrRefr10, finished10, pStopcdf10, pStoppdf10, 1-(0.1) /* 10 */);
const ActionSchemes::S_step a_step_15_14(11, "(step 15 14)", action_step_15_14, NULL, getRecOrRefr11, finished11, pStopcdf11, pStoppdf11, 1-(0.1) /* 11 */);
const ActionSchemes::S_step a_step_15_25(12, "(step 15 25)", action_step_15_25, NULL, getRecOrRefr12, finished12, pStopcdf12, pStoppdf12, 1-(0.1) /* 12 */);
const ActionSchemes::S_step a_step_21_11(13, "(step 21 11)", action_step_21_11, NULL, getRecOrRefr13, finished13, pStopcdf13, pStoppdf13, 1-(0.1) /* 13 */);
const ActionSchemes::S_step a_step_21_22(14, "(step 21 22)", action_step_21_22, NULL, getRecOrRefr14, finished14, pStopcdf14, pStoppdf14, 1-(0.1) /* 14 */);
const ActionSchemes::S_step a_step_21_31(15, "(step 21 31)", action_step_21_31, NULL, getRecOrRefr15, finished15, pStopcdf15, pStoppdf15, 1-(0.1) /* 15 */);
const ActionSchemes::S_step a_step_22_12(16, "(step 22 12)", action_step_22_12, NULL, getRecOrRefr16, finished16, pStopcdf16, pStoppdf16, 1-(0.1) /* 16 */);
const ActionSchemes::S_step a_step_22_21(17, "(step 22 21)", action_step_22_21, NULL, getRecOrRefr17, finished17, pStopcdf17, pStoppdf17, 1-(0.1) /* 17 */);
const ActionSchemes::S_step a_step_22_23(18, "(step 22 23)", action_step_22_23, NULL, getRecOrRefr18, finished18, pStopcdf18, pStoppdf18, 1-(0.1) /* 18 */);
const ActionSchemes::S_step a_step_22_32(19, "(step 22 32)", action_step_22_32, NULL, getRecOrRefr19, finished19, pStopcdf19, pStoppdf19, 1-(0.1) /* 19 */);
const ActionSchemes::S_step a_step_23_13(20, "(step 23 13)", action_step_23_13, NULL, getRecOrRefr20, finished20, pStopcdf20, pStoppdf20, 1-(0.1) /* 20 */);
const ActionSchemes::S_step a_step_23_22(21, "(step 23 22)", action_step_23_22, NULL, getRecOrRefr21, finished21, pStopcdf21, pStoppdf21, 1-(0.1) /* 21 */);
const ActionSchemes::S_step a_step_23_24(22, "(step 23 24)", action_step_23_24, NULL, getRecOrRefr22, finished22, pStopcdf22, pStoppdf22, 1-(0.1) /* 22 */);
const ActionSchemes::S_step a_step_23_33(23, "(step 23 33)", action_step_23_33, NULL, getRecOrRefr23, finished23, pStopcdf23, pStoppdf23, 1-(0.1) /* 23 */);
const ActionSchemes::S_step a_step_24_14(24, "(step 24 14)", action_step_24_14, NULL, getRecOrRefr24, finished24, pStopcdf24, pStoppdf24, 1-(0.1) /* 24 */);
const ActionSchemes::S_step a_step_24_23(25, "(step 24 23)", action_step_24_23, NULL, getRecOrRefr25, finished25, pStopcdf25, pStoppdf25, 1-(0.1) /* 25 */);
const ActionSchemes::S_step a_step_24_25(26, "(step 24 25)", action_step_24_25, NULL, getRecOrRefr26, finished26, pStopcdf26, pStoppdf26, 1-(0.1) /* 26 */);
const ActionSchemes::S_step a_step_24_34(27, "(step 24 34)", action_step_24_34, NULL, getRecOrRefr27, finished27, pStopcdf27, pStoppdf27, 1-(0.1) /* 27 */);
const ActionSchemes::S_step a_step_25_15(28, "(step 25 15)", action_step_25_15, NULL, getRecOrRefr28, finished28, pStopcdf28, pStoppdf28, 1-(0.1) /* 28 */);
const ActionSchemes::S_step a_step_25_24(29, "(step 25 24)", action_step_25_24, NULL, getRecOrRefr29, finished29, pStopcdf29, pStoppdf29, 1-(0.1) /* 29 */);
const ActionSchemes::S_step a_step_25_35(30, "(step 25 35)", action_step_25_35, NULL, getRecOrRefr30, finished30, pStopcdf30, pStoppdf30, 1-(0.1) /* 30 */);
const ActionSchemes::S_step a_step_31_21(31, "(step 31 21)", action_step_31_21, NULL, getRecOrRefr31, finished31, pStopcdf31, pStoppdf31, 1-(0.1) /* 31 */);
const ActionSchemes::S_step a_step_31_32(32, "(step 31 32)", action_step_31_32, NULL, getRecOrRefr32, finished32, pStopcdf32, pStoppdf32, 1-(0.1) /* 32 */);
const ActionSchemes::S_step a_step_31_41(33, "(step 31 41)", action_step_31_41, NULL, getRecOrRefr33, finished33, pStopcdf33, pStoppdf33, 1-(0.1) /* 33 */);
const ActionSchemes::S_step a_step_32_22(34, "(step 32 22)", action_step_32_22, NULL, getRecOrRefr34, finished34, pStopcdf34, pStoppdf34, 1-(0.1) /* 34 */);
const ActionSchemes::S_step a_step_32_31(35, "(step 32 31)", action_step_32_31, NULL, getRecOrRefr35, finished35, pStopcdf35, pStoppdf35, 1-(0.1) /* 35 */);
const ActionSchemes::S_step a_step_32_33(36, "(step 32 33)", action_step_32_33, NULL, getRecOrRefr36, finished36, pStopcdf36, pStoppdf36, 1-(0.1) /* 36 */);
const ActionSchemes::S_step a_step_32_42(37, "(step 32 42)", action_step_32_42, NULL, getRecOrRefr37, finished37, pStopcdf37, pStoppdf37, 1-(0.1) /* 37 */);
const ActionSchemes::S_step a_step_33_23(38, "(step 33 23)", action_step_33_23, NULL, getRecOrRefr38, finished38, pStopcdf38, pStoppdf38, 1-(0.1) /* 38 */);
const ActionSchemes::S_step a_step_33_32(39, "(step 33 32)", action_step_33_32, NULL, getRecOrRefr39, finished39, pStopcdf39, pStoppdf39, 1-(0.1) /* 39 */);
const ActionSchemes::S_step a_step_33_34(40, "(step 33 34)", action_step_33_34, NULL, getRecOrRefr40, finished40, pStopcdf40, pStoppdf40, 1-(0.1) /* 40 */);
const ActionSchemes::S_step a_step_33_43(41, "(step 33 43)", action_step_33_43, NULL, getRecOrRefr41, finished41, pStopcdf41, pStoppdf41, 1-(0.1) /* 41 */);
const ActionSchemes::S_step a_step_34_24(42, "(step 34 24)", action_step_34_24, NULL, getRecOrRefr42, finished42, pStopcdf42, pStoppdf42, 1-(0.1) /* 42 */);
const ActionSchemes::S_step a_step_34_33(43, "(step 34 33)", action_step_34_33, NULL, getRecOrRefr43, finished43, pStopcdf43, pStoppdf43, 1-(0.1) /* 43 */);
const ActionSchemes::S_step a_step_34_35(44, "(step 34 35)", action_step_34_35, NULL, getRecOrRefr44, finished44, pStopcdf44, pStoppdf44, 1-(0.1) /* 44 */);
const ActionSchemes::S_step a_step_34_44(45, "(step 34 44)", action_step_34_44, NULL, getRecOrRefr45, finished45, pStopcdf45, pStoppdf45, 1-(0.1) /* 45 */);
const ActionSchemes::S_step a_step_35_25(46, "(step 35 25)", action_step_35_25, NULL, getRecOrRefr46, finished46, pStopcdf46, pStoppdf46, 1-(0.1) /* 46 */);
const ActionSchemes::S_step a_step_35_34(47, "(step 35 34)", action_step_35_34, NULL, getRecOrRefr47, finished47, pStopcdf47, pStoppdf47, 1-(0.1) /* 47 */);
const ActionSchemes::S_step a_step_35_45(48, "(step 35 45)", action_step_35_45, NULL, getRecOrRefr48, finished48, pStopcdf48, pStoppdf48, 1-(0.1) /* 48 */);
const ActionSchemes::S_step a_step_41_31(49, "(step 41 31)", action_step_41_31, NULL, getRecOrRefr49, finished49, pStopcdf49, pStoppdf49, 1-(0.1) /* 49 */);
const ActionSchemes::S_step a_step_41_42(50, "(step 41 42)", action_step_41_42, NULL, getRecOrRefr50, finished50, pStopcdf50, pStoppdf50, 1-(0.1) /* 50 */);
const ActionSchemes::S_step a_step_41_51(51, "(step 41 51)", action_step_41_51, NULL, getRecOrRefr51, finished51, pStopcdf51, pStoppdf51, 1-(0.1) /* 51 */);
const ActionSchemes::S_step a_step_42_32(52, "(step 42 32)", action_step_42_32, NULL, getRecOrRefr52, finished52, pStopcdf52, pStoppdf52, 1-(0.1) /* 52 */);
const ActionSchemes::S_step a_step_42_41(53, "(step 42 41)", action_step_42_41, NULL, getRecOrRefr53, finished53, pStopcdf53, pStoppdf53, 1-(0.1) /* 53 */);
const ActionSchemes::S_step a_step_42_43(54, "(step 42 43)", action_step_42_43, NULL, getRecOrRefr54, finished54, pStopcdf54, pStoppdf54, 1-(0.1) /* 54 */);
const ActionSchemes::S_step a_step_42_52(55, "(step 42 52)", action_step_42_52, NULL, getRecOrRefr55, finished55, pStopcdf55, pStoppdf55, 1-(0.1) /* 55 */);
const ActionSchemes::S_step a_step_43_33(56, "(step 43 33)", action_step_43_33, NULL, getRecOrRefr56, finished56, pStopcdf56, pStoppdf56, 1-(0.1) /* 56 */);
const ActionSchemes::S_step a_step_43_42(57, "(step 43 42)", action_step_43_42, NULL, getRecOrRefr57, finished57, pStopcdf57, pStoppdf57, 1-(0.1) /* 57 */);
const ActionSchemes::S_step a_step_43_44(58, "(step 43 44)", action_step_43_44, NULL, getRecOrRefr58, finished58, pStopcdf58, pStoppdf58, 1-(0.1) /* 58 */);
const ActionSchemes::S_step a_step_43_53(59, "(step 43 53)", action_step_43_53, NULL, getRecOrRefr59, finished59, pStopcdf59, pStoppdf59, 1-(0.1) /* 59 */);
const ActionSchemes::S_step a_step_44_34(60, "(step 44 34)", action_step_44_34, NULL, getRecOrRefr60, finished60, pStopcdf60, pStoppdf60, 1-(0.1) /* 60 */);
const ActionSchemes::S_step a_step_44_43(61, "(step 44 43)", action_step_44_43, NULL, getRecOrRefr61, finished61, pStopcdf61, pStoppdf61, 1-(0.1) /* 61 */);
const ActionSchemes::S_step a_step_44_45(62, "(step 44 45)", action_step_44_45, NULL, getRecOrRefr62, finished62, pStopcdf62, pStoppdf62, 1-(0.1) /* 62 */);
const ActionSchemes::S_step a_step_44_54(63, "(step 44 54)", action_step_44_54, NULL, getRecOrRefr63, finished63, pStopcdf63, pStoppdf63, 1-(0.1) /* 63 */);
const ActionSchemes::S_step a_step_45_35(64, "(step 45 35)", action_step_45_35, NULL, getRecOrRefr64, finished64, pStopcdf64, pStoppdf64, 1-(0.1) /* 64 */);
const ActionSchemes::S_step a_step_45_44(65, "(step 45 44)", action_step_45_44, NULL, getRecOrRefr65, finished65, pStopcdf65, pStoppdf65, 1-(0.1) /* 65 */);
const ActionSchemes::S_step a_step_45_55(66, "(step 45 55)", action_step_45_55, NULL, getRecOrRefr66, finished66, pStopcdf66, pStoppdf66, 1-(0.1) /* 66 */);
const ActionSchemes::S_step a_step_51_41(67, "(step 51 41)", action_step_51_41, NULL, getRecOrRefr67, finished67, pStopcdf67, pStoppdf67, 1-(0.1) /* 67 */);
const ActionSchemes::S_step a_step_51_52(68, "(step 51 52)", action_step_51_52, NULL, getRecOrRefr68, finished68, pStopcdf68, pStoppdf68, 1-(0.1) /* 68 */);
const ActionSchemes::S_step a_step_52_42(69, "(step 52 42)", action_step_52_42, NULL, getRecOrRefr69, finished69, pStopcdf69, pStoppdf69, 1-(0.1) /* 69 */);
const ActionSchemes::S_step a_step_52_51(70, "(step 52 51)", action_step_52_51, NULL, getRecOrRefr70, finished70, pStopcdf70, pStoppdf70, 1-(0.1) /* 70 */);
const ActionSchemes::S_step a_step_52_53(71, "(step 52 53)", action_step_52_53, NULL, getRecOrRefr71, finished71, pStopcdf71, pStoppdf71, 1-(0.1) /* 71 */);
const ActionSchemes::S_step a_step_53_43(72, "(step 53 43)", action_step_53_43, NULL, getRecOrRefr72, finished72, pStopcdf72, pStoppdf72, 1-(0.1) /* 72 */);
const ActionSchemes::S_step a_step_53_52(73, "(step 53 52)", action_step_53_52, NULL, getRecOrRefr73, finished73, pStopcdf73, pStoppdf73, 1-(0.1) /* 73 */);
const ActionSchemes::S_step a_step_53_54(74, "(step 53 54)", action_step_53_54, NULL, getRecOrRefr74, finished74, pStopcdf74, pStoppdf74, 1-(0.1) /* 74 */);
const ActionSchemes::S_step a_step_54_44(75, "(step 54 44)", action_step_54_44, NULL, getRecOrRefr75, finished75, pStopcdf75, pStoppdf75, 1-(0.1) /* 75 */);
const ActionSchemes::S_step a_step_54_53(76, "(step 54 53)", action_step_54_53, NULL, getRecOrRefr76, finished76, pStopcdf76, pStoppdf76, 1-(0.1) /* 76 */);
const ActionSchemes::S_step a_step_54_55(77, "(step 54 55)", action_step_54_55, NULL, getRecOrRefr77, finished77, pStopcdf77, pStoppdf77, 1-(0.1) /* 77 */);
const ActionSchemes::S_step a_step_55_45(78, "(step 55 45)", action_step_55_45, NULL, getRecOrRefr78, finished78, pStopcdf78, pStoppdf78, 1-(0.1) /* 78 */);
const ActionSchemes::S_step a_step_55_54(79, "(step 55 54)", action_step_55_54, NULL, getRecOrRefr79, finished79, pStopcdf79, pStoppdf79, 1-(0.1) /* 79 */);
} // namespace Actions

Action const * __actions[NOPS+3] = {
	&Actions::a_FINISHED,
	&Actions::a_BLOCKED,
	&Actions::a_INITIALIZE,
	&Actions::a_step_11_12,
	&Actions::a_step_11_21,
	&Actions::a_step_12_11,
	&Actions::a_step_12_13,
	&Actions::a_step_12_22,
	&Actions::a_step_13_12,
	&Actions::a_step_13_14,
	&Actions::a_step_13_23,
	&Actions::a_step_14_13,
	&Actions::a_step_14_15,
	&Actions::a_step_14_24,
	&Actions::a_step_15_14,
	&Actions::a_step_15_25,
	&Actions::a_step_21_11,
	&Actions::a_step_21_22,
	&Actions::a_step_21_31,
	&Actions::a_step_22_12,
	&Actions::a_step_22_21,
	&Actions::a_step_22_23,
	&Actions::a_step_22_32,
	&Actions::a_step_23_13,
	&Actions::a_step_23_22,
	&Actions::a_step_23_24,
	&Actions::a_step_23_33,
	&Actions::a_step_24_14,
	&Actions::a_step_24_23,
	&Actions::a_step_24_25,
	&Actions::a_step_24_34,
	&Actions::a_step_25_15,
	&Actions::a_step_25_24,
	&Actions::a_step_25_35,
	&Actions::a_step_31_21,
	&Actions::a_step_31_32,
	&Actions::a_step_31_41,
	&Actions::a_step_32_22,
	&Actions::a_step_32_31,
	&Actions::a_step_32_33,
	&Actions::a_step_32_42,
	&Actions::a_step_33_23,
	&Actions::a_step_33_32,
	&Actions::a_step_33_34,
	&Actions::a_step_33_43,
	&Actions::a_step_34_24,
	&Actions::a_step_34_33,
	&Actions::a_step_34_35,
	&Actions::a_step_34_44,
	&Actions::a_step_35_25,
	&Actions::a_step_35_34,
	&Actions::a_step_35_45,
	&Actions::a_step_41_31,
	&Actions::a_step_41_42,
	&Actions::a_step_41_51,
	&Actions::a_step_42_32,
	&Actions::a_step_42_41,
	&Actions::a_step_42_43,
	&Actions::a_step_42_52,
	&Actions::a_step_43_33,
	&Actions::a_step_43_42,
	&Actions::a_step_43_44,
	&Actions::a_step_43_53,
	&Actions::a_step_44_34,
	&Actions::a_step_44_43,
	&Actions::a_step_44_45,
	&Actions::a_step_44_54,
	&Actions::a_step_45_35,
	&Actions::a_step_45_44,
	&Actions::a_step_45_55,
	&Actions::a_step_51_41,
	&Actions::a_step_51_52,
	&Actions::a_step_52_42,
	&Actions::a_step_52_51,
	&Actions::a_step_52_53,
	&Actions::a_step_53_43,
	&Actions::a_step_53_52,
	&Actions::a_step_53_54,
	&Actions::a_step_54_44,
	&Actions::a_step_54_53,
	&Actions::a_step_54_55,
	&Actions::a_step_55_45,
	&Actions::a_step_55_54
};
Action const * *actions = &__actions[3];

const int agentOps0[80] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79};
char const * const agentNames[NAGENTS] = {"TheAgent"};
int const nOpsForAgent[NAGENTS] = {80};
int const * const opsForAgent[NAGENTS] = {agentOps0};

bool isGoalState(size_t goal, StatePtr x) { (void) x;
	switch(goal) {
	case 0: /*xyrouter*/
		return x->Sat_55;
	default: throw std::runtime_error("isGoalState: Unknown goal"); break;
	}
}

void sampleInitial(size_t initialState, StateRec *x1) {
	bzero(x1,sizeof(StateRec));
	switch(initialState) {
	case 0: /*xyrouter*/
		x1->Sat_21 = 1;
		x1->Sat_11 = 0;
		x1->Sat_12 = 0;
		x1->Sat_13 = 0;
		x1->Sat_14 = 0;
		x1->Sat_15 = 0;
		x1->Sat_22 = 0;
		x1->Sat_23 = 0;
		x1->Sat_24 = 0;
		x1->Sat_25 = 0;
		x1->Sat_31 = 0;
		x1->Sat_32 = 0;
		x1->Sat_33 = 0;
		x1->Sat_34 = 0;
		x1->Sat_35 = 0;
		x1->Sat_41 = 0;
		x1->Sat_42 = 0;
		x1->Sat_43 = 0;
		x1->Sat_44 = 0;
		x1->Sat_45 = 0;
		x1->Sat_51 = 0;
		x1->Sat_52 = 0;
		x1->Sat_53 = 0;
		x1->Sat_54 = 0;
		x1->Sat_55 = 0;
		break;
	default: throw std::runtime_error("sampleInitial: Unknown initial state"); break;
	}
}

void stateObservation(StatePtr x) {
	if(x->Sat_11) { setXYcoordinates(0.7,0.7); }
	if(x->Sat_12) { setXYcoordinates(0.7,2.1); }
	if(x->Sat_13) { setXYcoordinates(0.7,3.5); }
	if(x->Sat_14) { setXYcoordinates(0.7,4.9); }
	if(x->Sat_15) { setXYcoordinates(0.7,6.3); }
	if(x->Sat_21) { setXYcoordinates(2.1,0.7); }
	if(x->Sat_22) { setXYcoordinates(2.1,2.1); }
	if(x->Sat_23) { setXYcoordinates(2.1,3.5); }
	if(x->Sat_24) { setXYcoordinates(2.1,4.9); }
	if(x->Sat_25) { setXYcoordinates(2.1,6.3); }
	if(x->Sat_31) { setXYcoordinates(3.5,0.7); }
	if(x->Sat_32) { setXYcoordinates(3.5,2.1); }
	if(x->Sat_33) { setXYcoordinates(3.5,3.5); }
	if(x->Sat_34) { setXYcoordinates(3.5,4.9); }
	if(x->Sat_35) { setXYcoordinates(3.5,6.3); }
	if(x->Sat_41) { setXYcoordinates(4.9,0.7); }
	if(x->Sat_42) { setXYcoordinates(4.9,2.1); }
	if(x->Sat_43) { setXYcoordinates(4.9,3.5); }
	if(x->Sat_44) { setXYcoordinates(4.9,4.9); }
	if(x->Sat_45) { setXYcoordinates(4.9,6.3); }
	if(x->Sat_51) { setXYcoordinates(6.3,0.7); }
	if(x->Sat_52) { setXYcoordinates(6.3,2.1); }
	if(x->Sat_53) { setXYcoordinates(6.3,3.5); }
	if(x->Sat_54) { setXYcoordinates(6.3,4.9); }
	if(x->Sat_55) { setXYcoordinates(6.3,6.3); }
}

int landmarkCountHeuristic(size_t initialState, size_t goal, StatePtr s, StateRec *parentAccepted) {
	(void) s;
	StateRec accepted;
	int count = 0;
	switch(initialState) {
	case 0: /*xyrouter*/
		switch(goal) {
		case 0: /*xyrouter*/
			if (parentAccepted == 0) {
				//accepted = StateRec();accepted.S1=1;
			} else {
				accepted= *parentAccepted;
		
				if (s->Sat_55==1) accepted.Sat_55=1;
			} // compute the heuristic
			if (accepted.Sat_55== 0) count++;
				if (accepted.Sat_55== 1 && s->Sat_55== 0) count++;
			break;
		default: throw std::runtime_error("landmarkCountHeuristic: Unknown goal state");
		}
		break;
	default: throw std::runtime_error("landmarkCountHeuristic: Unknown initial state");
	}
	*parentAccepted = accepted;
	return count;
}
