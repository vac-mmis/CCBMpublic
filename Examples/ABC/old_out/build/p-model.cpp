const char * DOMAIN_NAME = "abc";
const char * PROBLEM_NAME = "abc1";

char const * const objnames_object[] = {"door","printer","paper-stack","coffee-machine","water-tap","coffee-jar","outside","water","paper","ground-coffee","coffee","a","b","c","alice","bob"};
char const * const objnames_glocation[] = {"door","printer","paper-stack","coffee-machine","water-tap","coffee-jar"};
char const * const objnames_location[] = {"outside","door","printer","paper-stack","coffee-machine","water-tap","coffee-jar"};
char const * const objnames_agent[] = {"alice","bob"};
char const * const objnames_job[] = {"a","b","c"};
char const * const objnames_item[] = {"water","paper","ground-coffee","coffee"};

std::ostream& operator<<(std::ostream &o, StatePtr x) {
	if(x->Sprinted_a) o << " (printed a)"; else o<< " (not (printed a))"; 
	if(x->Sprinted_b) o << " (printed b)"; else o<< " (not (printed b))"; 
	if(x->Sprinted_c) o << " (printed c)"; else o<< " (not (printed c))"; 
	if(x->Sprinter_jammed) o << " printer-jammed"; else o<< " (not printer-jammed)"; 
	if(x->Shas_water_coffee_machine) o << " (has water coffee-machine)"; else o<< " (not (has water coffee-machine))"; 
	if(x->Shas_paper_printer) o << " (has paper printer)"; else o<< " (not (has paper printer))"; 
	if(x->Shas_ground_coffee_coffee_machine) o << " (has ground-coffee coffee-machine)"; else o<< " (not (has ground-coffee coffee-machine))"; 
	if(x->Sat_door_alice) o << " (at door alice)"; else o<< " (not (at door alice))"; 
	if(x->Sat_door_bob) o << " (at door bob)"; else o<< " (not (at door bob))"; 
	if(x->Sat_printer_alice) o << " (at printer alice)"; else o<< " (not (at printer alice))"; 
	if(x->Sat_printer_bob) o << " (at printer bob)"; else o<< " (not (at printer bob))"; 
	if(x->Sat_paper_stack_alice) o << " (at paper-stack alice)"; else o<< " (not (at paper-stack alice))"; 
	if(x->Sat_paper_stack_bob) o << " (at paper-stack bob)"; else o<< " (not (at paper-stack bob))"; 
	if(x->Sat_coffee_machine_alice) o << " (at coffee-machine alice)"; else o<< " (not (at coffee-machine alice))"; 
	if(x->Sat_coffee_machine_bob) o << " (at coffee-machine bob)"; else o<< " (not (at coffee-machine bob))"; 
	if(x->Sat_water_tap_alice) o << " (at water-tap alice)"; else o<< " (not (at water-tap alice))"; 
	if(x->Sat_water_tap_bob) o << " (at water-tap bob)"; else o<< " (not (at water-tap bob))"; 
	if(x->Sat_coffee_jar_alice) o << " (at coffee-jar alice)"; else o<< " (not (at coffee-jar alice))"; 
	if(x->Sat_coffee_jar_bob) o << " (at coffee-jar bob)"; else o<< " (not (at coffee-jar bob))"; 
	if(x->Sat_outside_alice) o << " (at outside alice)"; else o<< " (not (at outside alice))"; 
	if(x->Sat_outside_bob) o << " (at outside bob)"; else o<< " (not (at outside bob))"; 
	if(x->Shands_free_alice) o << " (hands-free alice)"; else o<< " (not (hands-free alice))"; 
	if(x->Shands_free_bob) o << " (hands-free bob)"; else o<< " (not (hands-free bob))"; 
	if(x->Sholds_water_alice) o << " (holds water alice)"; else o<< " (not (holds water alice))"; 
	if(x->Sholds_water_bob) o << " (holds water bob)"; else o<< " (not (holds water bob))"; 
	if(x->Sholds_paper_alice) o << " (holds paper alice)"; else o<< " (not (holds paper alice))"; 
	if(x->Sholds_paper_bob) o << " (holds paper bob)"; else o<< " (not (holds paper bob))"; 
	if(x->Sholds_ground_coffee_alice) o << " (holds ground-coffee alice)"; else o<< " (not (holds ground-coffee alice))"; 
	if(x->Sholds_ground_coffee_bob) o << " (holds ground-coffee bob)"; else o<< " (not (holds ground-coffee bob))"; 
	if(x->Sholds_coffee_alice) o << " (holds coffee alice)"; else o<< " (not (holds coffee alice))"; 
	if(x->Sholds_coffee_bob) o << " (holds coffee bob)"; else o<< " (not (holds coffee bob))"; 
	return o;
}
void writeState(std::ostream &o, StatePtr x, double wt) {
	char buf[16 + 3*0 + 0]; char *s = (char*) buf;
	if (std::isinf(wt)) strcpy(s, "inf"), s += 3; else s += sprintf(s, "%g", wt);
	*s = ' '; s++; *s =x->Sprinted_a + '0'; s++;
	*s = ' '; s++; *s =x->Sprinted_b + '0'; s++;
	*s = ' '; s++; *s =x->Sprinted_c + '0'; s++;
	*s = ' '; s++; *s =x->Sprinter_jammed + '0'; s++;
	*s = ' '; s++; *s =x->Shas_water_coffee_machine + '0'; s++;
	*s = ' '; s++; *s =x->Shas_paper_printer + '0'; s++;
	*s = ' '; s++; *s =x->Shas_ground_coffee_coffee_machine + '0'; s++;
	*s = ' '; s++; *s =x->Sat_door_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sat_door_bob + '0'; s++;
	*s = ' '; s++; *s =x->Sat_printer_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sat_printer_bob + '0'; s++;
	*s = ' '; s++; *s =x->Sat_paper_stack_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sat_paper_stack_bob + '0'; s++;
	*s = ' '; s++; *s =x->Sat_coffee_machine_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sat_coffee_machine_bob + '0'; s++;
	*s = ' '; s++; *s =x->Sat_water_tap_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sat_water_tap_bob + '0'; s++;
	*s = ' '; s++; *s =x->Sat_coffee_jar_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sat_coffee_jar_bob + '0'; s++;
	*s = ' '; s++; *s =x->Sat_outside_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sat_outside_bob + '0'; s++;
	*s = ' '; s++; *s =x->Shands_free_alice + '0'; s++;
	*s = ' '; s++; *s =x->Shands_free_bob + '0'; s++;
	*s = ' '; s++; *s =x->Sholds_water_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sholds_water_bob + '0'; s++;
	*s = ' '; s++; *s =x->Sholds_paper_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sholds_paper_bob + '0'; s++;
	*s = ' '; s++; *s =x->Sholds_ground_coffee_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sholds_ground_coffee_bob + '0'; s++;
	*s = ' '; s++; *s =x->Sholds_coffee_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sholds_coffee_bob + '0'; s++;
	*s = '\n'; *(++s) = '\0'; o << (char*) buf;
}
bool readState(std::istream &i, StateRec *x, double &wt) {
	char buf[4096]; char *s = (char*) buf;
	do i.getline(s, 4096); while (i.gcount() < 2 && i.good());
	if (!i.good()) return false;
	if (!memcmp(s, "inf", 3)) { wt = infinity, s+=3; } else wt = strtod(s, &s);
	char c;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sprinted_a = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sprinted_b = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sprinted_c = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sprinter_jammed = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Shas_water_coffee_machine = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Shas_paper_printer = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Shas_ground_coffee_coffee_machine = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_door_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_door_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_printer_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_printer_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_paper_stack_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_paper_stack_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_coffee_machine_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_coffee_machine_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_water_tap_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_water_tap_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_coffee_jar_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_coffee_jar_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_outside_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_outside_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Shands_free_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Shands_free_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sholds_water_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sholds_water_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sholds_paper_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sholds_paper_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sholds_ground_coffee_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sholds_ground_coffee_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sholds_coffee_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sholds_coffee_bob = c, s++; else return false;
	return true;
}
void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr) { // rt: Recency Vector (ACT-R Heuristic); t: ist the global time
	if (prev->Sprinted_a ^ curr->Sprinted_a) rT[0] = t;
	if (prev->Sprinted_b ^ curr->Sprinted_b) rT[1] = t;
	if (prev->Sprinted_c ^ curr->Sprinted_c) rT[2] = t;
	if (prev->Sprinter_jammed ^ curr->Sprinter_jammed) rT[3] = t;
	if (prev->Shas_water_coffee_machine ^ curr->Shas_water_coffee_machine) rT[4] = t;
	if (prev->Shas_paper_printer ^ curr->Shas_paper_printer) rT[5] = t;
	if (prev->Shas_ground_coffee_coffee_machine ^ curr->Shas_ground_coffee_coffee_machine) rT[6] = t;
	if (prev->Sat_door_alice ^ curr->Sat_door_alice) rT[7] = t;
	if (prev->Sat_door_bob ^ curr->Sat_door_bob) rT[8] = t;
	if (prev->Sat_printer_alice ^ curr->Sat_printer_alice) rT[9] = t;
	if (prev->Sat_printer_bob ^ curr->Sat_printer_bob) rT[10] = t;
	if (prev->Sat_paper_stack_alice ^ curr->Sat_paper_stack_alice) rT[11] = t;
	if (prev->Sat_paper_stack_bob ^ curr->Sat_paper_stack_bob) rT[12] = t;
	if (prev->Sat_coffee_machine_alice ^ curr->Sat_coffee_machine_alice) rT[13] = t;
	if (prev->Sat_coffee_machine_bob ^ curr->Sat_coffee_machine_bob) rT[14] = t;
	if (prev->Sat_water_tap_alice ^ curr->Sat_water_tap_alice) rT[15] = t;
	if (prev->Sat_water_tap_bob ^ curr->Sat_water_tap_bob) rT[16] = t;
	if (prev->Sat_coffee_jar_alice ^ curr->Sat_coffee_jar_alice) rT[17] = t;
	if (prev->Sat_coffee_jar_bob ^ curr->Sat_coffee_jar_bob) rT[18] = t;
	if (prev->Sat_outside_alice ^ curr->Sat_outside_alice) rT[19] = t;
	if (prev->Sat_outside_bob ^ curr->Sat_outside_bob) rT[20] = t;
	if (prev->Shands_free_alice ^ curr->Shands_free_alice) rT[21] = t;
	if (prev->Shands_free_bob ^ curr->Shands_free_bob) rT[22] = t;
	if (prev->Sholds_water_alice ^ curr->Sholds_water_alice) rT[23] = t;
	if (prev->Sholds_water_bob ^ curr->Sholds_water_bob) rT[24] = t;
	if (prev->Sholds_paper_alice ^ curr->Sholds_paper_alice) rT[25] = t;
	if (prev->Sholds_paper_bob ^ curr->Sholds_paper_bob) rT[26] = t;
	if (prev->Sholds_ground_coffee_alice ^ curr->Sholds_ground_coffee_alice) rT[27] = t;
	if (prev->Sholds_ground_coffee_bob ^ curr->Sholds_ground_coffee_bob) rT[28] = t;
	if (prev->Sholds_coffee_alice ^ curr->Sholds_coffee_alice) rT[29] = t;
	if (prev->Sholds_coffee_bob ^ curr->Sholds_coffee_bob) rT[30] = t;
}
double getRefract(StatePtr a1, StatePtr a2) { // compare two action preconditions and returns the rerfractoriness 
	double r=0; int n=0;
	if (a1->Sprinted_a) {n++; if (a2->Sprinted_a) r++;}
	if (a1->Sprinted_b) {n++; if (a2->Sprinted_b) r++;}
	if (a1->Sprinted_c) {n++; if (a2->Sprinted_c) r++;}
	if (a1->Sprinter_jammed) {n++; if (a2->Sprinter_jammed) r++;}
	if (a1->Shas_water_coffee_machine) {n++; if (a2->Shas_water_coffee_machine) r++;}
	if (a1->Shas_paper_printer) {n++; if (a2->Shas_paper_printer) r++;}
	if (a1->Shas_ground_coffee_coffee_machine) {n++; if (a2->Shas_ground_coffee_coffee_machine) r++;}
	if (a1->Sat_door_alice) {n++; if (a2->Sat_door_alice) r++;}
	if (a1->Sat_door_bob) {n++; if (a2->Sat_door_bob) r++;}
	if (a1->Sat_printer_alice) {n++; if (a2->Sat_printer_alice) r++;}
	if (a1->Sat_printer_bob) {n++; if (a2->Sat_printer_bob) r++;}
	if (a1->Sat_paper_stack_alice) {n++; if (a2->Sat_paper_stack_alice) r++;}
	if (a1->Sat_paper_stack_bob) {n++; if (a2->Sat_paper_stack_bob) r++;}
	if (a1->Sat_coffee_machine_alice) {n++; if (a2->Sat_coffee_machine_alice) r++;}
	if (a1->Sat_coffee_machine_bob) {n++; if (a2->Sat_coffee_machine_bob) r++;}
	if (a1->Sat_water_tap_alice) {n++; if (a2->Sat_water_tap_alice) r++;}
	if (a1->Sat_water_tap_bob) {n++; if (a2->Sat_water_tap_bob) r++;}
	if (a1->Sat_coffee_jar_alice) {n++; if (a2->Sat_coffee_jar_alice) r++;}
	if (a1->Sat_coffee_jar_bob) {n++; if (a2->Sat_coffee_jar_bob) r++;}
	if (a1->Sat_outside_alice) {n++; if (a2->Sat_outside_alice) r++;}
	if (a1->Sat_outside_bob) {n++; if (a2->Sat_outside_bob) r++;}
	if (a1->Shands_free_alice) {n++; if (a2->Shands_free_alice) r++;}
	if (a1->Shands_free_bob) {n++; if (a2->Shands_free_bob) r++;}
	if (a1->Sholds_water_alice) {n++; if (a2->Sholds_water_alice) r++;}
	if (a1->Sholds_water_bob) {n++; if (a2->Sholds_water_bob) r++;}
	if (a1->Sholds_paper_alice) {n++; if (a2->Sholds_paper_alice) r++;}
	if (a1->Sholds_paper_bob) {n++; if (a2->Sholds_paper_bob) r++;}
	if (a1->Sholds_ground_coffee_alice) {n++; if (a2->Sholds_ground_coffee_alice) r++;}
	if (a1->Sholds_ground_coffee_bob) {n++; if (a2->Sholds_ground_coffee_bob) r++;}
	if (a1->Sholds_coffee_alice) {n++; if (a2->Sholds_coffee_alice) r++;}
	if (a1->Sholds_coffee_bob) {n++; if (a2->Sholds_coffee_bob) r++;}
	return (n==0) ? 0 : r/n;
}

char const * const initialStateNames[] = {"abc1"};
char const * const goalNames[] = {"abc1"};

// no :non-repeating has been used
bool canFollowAfter(int next, int prev) { (void)next; (void)prev; return true; }

int const FinishedOpId = -3;
int const NoOpId = -2;
int const InitOpId = -1;
char const * const __actionNames[NOPS+3] = {
	"(FINISHED)",
	"(BLOCKED)",
	"(INITIALIZE)",
	"(print a)", // (:action print ?j - job)
	"(print b)", // (:action print ?j - job)
	"(print c)", // (:action print ?j - job)
	"(*****JAM-PRINTER******)", // (:action *****JAM-PRINTER****** )
	"(repair-printer alice)", // (:action repair-printer ?a - agent)
	"(repair-printer bob)", // (:action repair-printer ?a - agent)
	"(get-coffee alice)", // (:action get-coffee ?a - agent)
	"(get-coffee bob)", // (:action get-coffee ?a - agent)
	"(fetch alice water water-tap)", // (:action fetch ?a - agent ?i - item ?l - location)
	"(fetch alice paper paper-stack)", // (:action fetch ?a - agent ?i - item ?l - location)
	"(fetch alice ground-coffee coffee-jar)", // (:action fetch ?a - agent ?i - item ?l - location)
	"(fetch bob water water-tap)", // (:action fetch ?a - agent ?i - item ?l - location)
	"(fetch bob paper paper-stack)", // (:action fetch ?a - agent ?i - item ?l - location)
	"(fetch bob ground-coffee coffee-jar)", // (:action fetch ?a - agent ?i - item ?l - location)
	"(replenish alice water coffee-machine)", // (:action replenish ?a - agent ?i - item ?l - location)
	"(replenish alice paper printer)", // (:action replenish ?a - agent ?i - item ?l - location)
	"(replenish alice ground-coffee coffee-machine)", // (:action replenish ?a - agent ?i - item ?l - location)
	"(replenish bob water coffee-machine)", // (:action replenish ?a - agent ?i - item ?l - location)
	"(replenish bob paper printer)", // (:action replenish ?a - agent ?i - item ?l - location)
	"(replenish bob ground-coffee coffee-machine)", // (:action replenish ?a - agent ?i - item ?l - location)
	"(goto alice door printer)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice door paper-stack)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice door coffee-machine)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice door water-tap)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice door coffee-jar)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice printer door)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice printer paper-stack)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice printer coffee-machine)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice printer water-tap)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice printer coffee-jar)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice paper-stack door)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice paper-stack printer)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice paper-stack coffee-machine)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice paper-stack water-tap)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice paper-stack coffee-jar)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice coffee-machine door)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice coffee-machine printer)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice coffee-machine paper-stack)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice coffee-machine water-tap)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice coffee-machine coffee-jar)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice water-tap door)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice water-tap printer)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice water-tap paper-stack)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice water-tap coffee-machine)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice water-tap coffee-jar)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice coffee-jar door)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice coffee-jar printer)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice coffee-jar paper-stack)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice coffee-jar coffee-machine)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto alice coffee-jar water-tap)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob door printer)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob door paper-stack)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob door coffee-machine)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob door water-tap)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob door coffee-jar)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob printer door)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob printer paper-stack)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob printer coffee-machine)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob printer water-tap)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob printer coffee-jar)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob paper-stack door)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob paper-stack printer)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob paper-stack coffee-machine)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob paper-stack water-tap)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob paper-stack coffee-jar)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob coffee-machine door)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob coffee-machine printer)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob coffee-machine paper-stack)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob coffee-machine water-tap)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob coffee-machine coffee-jar)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob water-tap door)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob water-tap printer)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob water-tap paper-stack)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob water-tap coffee-machine)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob water-tap coffee-jar)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob coffee-jar door)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob coffee-jar printer)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob coffee-jar paper-stack)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob coffee-jar coffee-machine)", // (:action goto ?a - agent ?x ?y - glocation)
	"(goto bob coffee-jar water-tap)", // (:action goto ?a - agent ?x ?y - glocation)
	"(enter alice)", // (:action enter ?a - agent)
	"(enter bob)", // (:action enter ?a - agent)
	"(exit alice)", // (:action exit ?a - agent)
	"(exit bob)" // (:action exit ?a - agent)
};
char const * const * const actionNames = &__actionNames[3];


/* (print a), id 0 */
double action_print_a(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Sprinted_a) && !(x->Sprinter_jammed) && x->Shas_paper_printer)) *specificity=1.58; 

	if((!(x->Sprinted_a) && !(x->Sprinter_jammed) && x->Shas_paper_printer)) {
		*x1 = *x;
		x1->Sprinted_a = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (print b), id 1 */
double action_print_b(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Sprinted_b) && !(x->Sprinter_jammed) && x->Shas_paper_printer)) *specificity=1.58; 

	if((!(x->Sprinted_b) && !(x->Sprinter_jammed) && x->Shas_paper_printer)) {
		*x1 = *x;
		x1->Sprinted_b = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (print c), id 2 */
double action_print_c(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Sprinted_c) && !(x->Sprinter_jammed) && x->Shas_paper_printer)) *specificity=1.58; 

	if((!(x->Sprinted_c) && !(x->Sprinter_jammed) && x->Shas_paper_printer)) {
		*x1 = *x;
		x1->Sprinted_c = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (*****JAM-PRINTER******), id 3 */
double action__JAM_PRINTER_(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sprinter_jammed)) *specificity=0.25; 

	if(!(x->Sprinter_jammed)) {
		*x1 = *x;
		x1->Sprinter_jammed = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (repair-printer alice), id 4 */
double action_repair_printer_alice(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sat_printer_alice && x->Shands_free_alice && x->Sprinter_jammed)) *specificity=0.84; 

	if((x->Sat_printer_alice && x->Shands_free_alice && x->Sprinter_jammed)) {
		*x1 = *x;
		x1->Sprinter_jammed = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (repair-printer bob), id 5 */
double action_repair_printer_bob(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sat_printer_bob && x->Shands_free_bob && x->Sprinter_jammed)) *specificity=0.84; 

	if((x->Sat_printer_bob && x->Shands_free_bob && x->Sprinter_jammed)) {
		*x1 = *x;
		x1->Sprinter_jammed = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (get-coffee alice), id 6 */
double action_get_coffee_alice(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sat_coffee_machine_alice && x->Shas_water_coffee_machine && x->Shas_ground_coffee_coffee_machine && x->Shands_free_alice)) *specificity=1.32; 

	if((x->Sat_coffee_machine_alice && x->Shas_water_coffee_machine && x->Shas_ground_coffee_coffee_machine && x->Shands_free_alice)) {
		*x1 = *x;
		x1->Shands_free_alice = 0;
		x1->Sholds_coffee_alice = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (get-coffee bob), id 7 */
double action_get_coffee_bob(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sat_coffee_machine_bob && x->Shas_water_coffee_machine && x->Shas_ground_coffee_coffee_machine && x->Shands_free_bob)) *specificity=1.32; 

	if((x->Sat_coffee_machine_bob && x->Shas_water_coffee_machine && x->Shas_ground_coffee_coffee_machine && x->Shands_free_bob)) {
		*x1 = *x;
		x1->Shands_free_bob = 0;
		x1->Sholds_coffee_bob = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (fetch alice water water-tap), id 8 */
double action_fetch_alice_water_water_tap(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Shands_free_alice && x->Sat_water_tap_alice)) *specificity=0.37; 

	if((x->Shands_free_alice && x->Sat_water_tap_alice)) {
		*x1 = *x;
		x1->Sholds_water_alice = 1;
		x1->Shands_free_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (fetch alice paper paper-stack), id 9 */
double action_fetch_alice_paper_paper_stack(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Shands_free_alice && x->Sat_paper_stack_alice)) *specificity=0.37; 

	if((x->Shands_free_alice && x->Sat_paper_stack_alice)) {
		*x1 = *x;
		x1->Sholds_paper_alice = 1;
		x1->Shands_free_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (fetch alice ground-coffee coffee-jar), id 10 */
double action_fetch_alice_ground_coffee_coffee_jar(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Shands_free_alice && x->Sat_coffee_jar_alice)) *specificity=0.37; 

	if((x->Shands_free_alice && x->Sat_coffee_jar_alice)) {
		*x1 = *x;
		x1->Sholds_ground_coffee_alice = 1;
		x1->Shands_free_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (fetch bob water water-tap), id 11 */
double action_fetch_bob_water_water_tap(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Shands_free_bob && x->Sat_water_tap_bob)) *specificity=0.37; 

	if((x->Shands_free_bob && x->Sat_water_tap_bob)) {
		*x1 = *x;
		x1->Sholds_water_bob = 1;
		x1->Shands_free_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (fetch bob paper paper-stack), id 12 */
double action_fetch_bob_paper_paper_stack(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Shands_free_bob && x->Sat_paper_stack_bob)) *specificity=0.37; 

	if((x->Shands_free_bob && x->Sat_paper_stack_bob)) {
		*x1 = *x;
		x1->Sholds_paper_bob = 1;
		x1->Shands_free_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (fetch bob ground-coffee coffee-jar), id 13 */
double action_fetch_bob_ground_coffee_coffee_jar(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Shands_free_bob && x->Sat_coffee_jar_bob)) *specificity=0.37; 

	if((x->Shands_free_bob && x->Sat_coffee_jar_bob)) {
		*x1 = *x;
		x1->Sholds_ground_coffee_bob = 1;
		x1->Shands_free_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (replenish alice water coffee-machine), id 14 */
double action_replenish_alice_water_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sholds_water_alice && x->Sat_coffee_machine_alice && !(x->Shas_water_coffee_machine))) *specificity=1.62; 

	if((x->Sholds_water_alice && x->Sat_coffee_machine_alice && !(x->Shas_water_coffee_machine))) {
		*x1 = *x;
		x1->Shands_free_alice = 1;
		x1->Sholds_water_alice = 0;
		x1->Shas_water_coffee_machine = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (replenish alice paper printer), id 15 */
double action_replenish_alice_paper_printer(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sholds_paper_alice && x->Sat_printer_alice && !(x->Shas_paper_printer))) *specificity=1.64; 

	if((x->Sholds_paper_alice && x->Sat_printer_alice && !(x->Shas_paper_printer))) {
		*x1 = *x;
		x1->Shands_free_alice = 1;
		x1->Sholds_paper_alice = 0;
		x1->Shas_paper_printer = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (replenish alice ground-coffee coffee-machine), id 16 */
double action_replenish_alice_ground_coffee_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sholds_ground_coffee_alice && x->Sat_coffee_machine_alice && !(x->Shas_ground_coffee_coffee_machine))) *specificity=1.62; 

	if((x->Sholds_ground_coffee_alice && x->Sat_coffee_machine_alice && !(x->Shas_ground_coffee_coffee_machine))) {
		*x1 = *x;
		x1->Shands_free_alice = 1;
		x1->Sholds_ground_coffee_alice = 0;
		x1->Shas_ground_coffee_coffee_machine = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (replenish bob water coffee-machine), id 17 */
double action_replenish_bob_water_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sholds_water_bob && x->Sat_coffee_machine_bob && !(x->Shas_water_coffee_machine))) *specificity=1.62; 

	if((x->Sholds_water_bob && x->Sat_coffee_machine_bob && !(x->Shas_water_coffee_machine))) {
		*x1 = *x;
		x1->Shands_free_bob = 1;
		x1->Sholds_water_bob = 0;
		x1->Shas_water_coffee_machine = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (replenish bob paper printer), id 18 */
double action_replenish_bob_paper_printer(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sholds_paper_bob && x->Sat_printer_bob && !(x->Shas_paper_printer))) *specificity=1.64; 

	if((x->Sholds_paper_bob && x->Sat_printer_bob && !(x->Shas_paper_printer))) {
		*x1 = *x;
		x1->Shands_free_bob = 1;
		x1->Sholds_paper_bob = 0;
		x1->Shas_paper_printer = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (replenish bob ground-coffee coffee-machine), id 19 */
double action_replenish_bob_ground_coffee_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sholds_ground_coffee_bob && x->Sat_coffee_machine_bob && !(x->Shas_ground_coffee_coffee_machine))) *specificity=1.62; 

	if((x->Sholds_ground_coffee_bob && x->Sat_coffee_machine_bob && !(x->Shas_ground_coffee_coffee_machine))) {
		*x1 = *x;
		x1->Shands_free_bob = 1;
		x1->Sholds_ground_coffee_bob = 0;
		x1->Shas_ground_coffee_coffee_machine = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice door printer), id 20 */
double action_goto_alice_door_printer(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_door_alice) *specificity=0.17; 

	if(x->Sat_door_alice) {
		*x1 = *x;
		x1->Sat_printer_alice = 1;
		x1->Sat_door_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice door paper-stack), id 21 */
double action_goto_alice_door_paper_stack(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_door_alice) *specificity=0.17; 

	if(x->Sat_door_alice) {
		*x1 = *x;
		x1->Sat_paper_stack_alice = 1;
		x1->Sat_door_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice door coffee-machine), id 22 */
double action_goto_alice_door_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_door_alice) *specificity=0.17; 

	if(x->Sat_door_alice) {
		*x1 = *x;
		x1->Sat_coffee_machine_alice = 1;
		x1->Sat_door_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice door water-tap), id 23 */
double action_goto_alice_door_water_tap(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_door_alice) *specificity=0.17; 

	if(x->Sat_door_alice) {
		*x1 = *x;
		x1->Sat_water_tap_alice = 1;
		x1->Sat_door_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice door coffee-jar), id 24 */
double action_goto_alice_door_coffee_jar(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_door_alice) *specificity=0.17; 

	if(x->Sat_door_alice) {
		*x1 = *x;
		x1->Sat_coffee_jar_alice = 1;
		x1->Sat_door_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice printer door), id 25 */
double action_goto_alice_printer_door(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_printer_alice) *specificity=0.14; 

	if(x->Sat_printer_alice) {
		*x1 = *x;
		x1->Sat_door_alice = 1;
		x1->Sat_printer_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice printer paper-stack), id 26 */
double action_goto_alice_printer_paper_stack(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_printer_alice) *specificity=0.14; 

	if(x->Sat_printer_alice) {
		*x1 = *x;
		x1->Sat_paper_stack_alice = 1;
		x1->Sat_printer_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice printer coffee-machine), id 27 */
double action_goto_alice_printer_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_printer_alice) *specificity=0.14; 

	if(x->Sat_printer_alice) {
		*x1 = *x;
		x1->Sat_coffee_machine_alice = 1;
		x1->Sat_printer_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice printer water-tap), id 28 */
double action_goto_alice_printer_water_tap(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_printer_alice) *specificity=0.14; 

	if(x->Sat_printer_alice) {
		*x1 = *x;
		x1->Sat_water_tap_alice = 1;
		x1->Sat_printer_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice printer coffee-jar), id 29 */
double action_goto_alice_printer_coffee_jar(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_printer_alice) *specificity=0.14; 

	if(x->Sat_printer_alice) {
		*x1 = *x;
		x1->Sat_coffee_jar_alice = 1;
		x1->Sat_printer_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice paper-stack door), id 30 */
double action_goto_alice_paper_stack_door(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_paper_stack_alice) *specificity=0.17; 

	if(x->Sat_paper_stack_alice) {
		*x1 = *x;
		x1->Sat_door_alice = 1;
		x1->Sat_paper_stack_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice paper-stack printer), id 31 */
double action_goto_alice_paper_stack_printer(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_paper_stack_alice) *specificity=0.17; 

	if(x->Sat_paper_stack_alice) {
		*x1 = *x;
		x1->Sat_printer_alice = 1;
		x1->Sat_paper_stack_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice paper-stack coffee-machine), id 32 */
double action_goto_alice_paper_stack_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_paper_stack_alice) *specificity=0.17; 

	if(x->Sat_paper_stack_alice) {
		*x1 = *x;
		x1->Sat_coffee_machine_alice = 1;
		x1->Sat_paper_stack_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice paper-stack water-tap), id 33 */
double action_goto_alice_paper_stack_water_tap(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_paper_stack_alice) *specificity=0.17; 

	if(x->Sat_paper_stack_alice) {
		*x1 = *x;
		x1->Sat_water_tap_alice = 1;
		x1->Sat_paper_stack_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice paper-stack coffee-jar), id 34 */
double action_goto_alice_paper_stack_coffee_jar(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_paper_stack_alice) *specificity=0.17; 

	if(x->Sat_paper_stack_alice) {
		*x1 = *x;
		x1->Sat_coffee_jar_alice = 1;
		x1->Sat_paper_stack_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice coffee-machine door), id 35 */
double action_goto_alice_coffee_machine_door(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_machine_alice) *specificity=0.12; 

	if(x->Sat_coffee_machine_alice) {
		*x1 = *x;
		x1->Sat_door_alice = 1;
		x1->Sat_coffee_machine_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice coffee-machine printer), id 36 */
double action_goto_alice_coffee_machine_printer(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_machine_alice) *specificity=0.12; 

	if(x->Sat_coffee_machine_alice) {
		*x1 = *x;
		x1->Sat_printer_alice = 1;
		x1->Sat_coffee_machine_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice coffee-machine paper-stack), id 37 */
double action_goto_alice_coffee_machine_paper_stack(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_machine_alice) *specificity=0.12; 

	if(x->Sat_coffee_machine_alice) {
		*x1 = *x;
		x1->Sat_paper_stack_alice = 1;
		x1->Sat_coffee_machine_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice coffee-machine water-tap), id 38 */
double action_goto_alice_coffee_machine_water_tap(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_machine_alice) *specificity=0.12; 

	if(x->Sat_coffee_machine_alice) {
		*x1 = *x;
		x1->Sat_water_tap_alice = 1;
		x1->Sat_coffee_machine_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice coffee-machine coffee-jar), id 39 */
double action_goto_alice_coffee_machine_coffee_jar(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_machine_alice) *specificity=0.12; 

	if(x->Sat_coffee_machine_alice) {
		*x1 = *x;
		x1->Sat_coffee_jar_alice = 1;
		x1->Sat_coffee_machine_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice water-tap door), id 40 */
double action_goto_alice_water_tap_door(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_water_tap_alice) *specificity=0.17; 

	if(x->Sat_water_tap_alice) {
		*x1 = *x;
		x1->Sat_door_alice = 1;
		x1->Sat_water_tap_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice water-tap printer), id 41 */
double action_goto_alice_water_tap_printer(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_water_tap_alice) *specificity=0.17; 

	if(x->Sat_water_tap_alice) {
		*x1 = *x;
		x1->Sat_printer_alice = 1;
		x1->Sat_water_tap_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice water-tap paper-stack), id 42 */
double action_goto_alice_water_tap_paper_stack(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_water_tap_alice) *specificity=0.17; 

	if(x->Sat_water_tap_alice) {
		*x1 = *x;
		x1->Sat_paper_stack_alice = 1;
		x1->Sat_water_tap_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice water-tap coffee-machine), id 43 */
double action_goto_alice_water_tap_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_water_tap_alice) *specificity=0.17; 

	if(x->Sat_water_tap_alice) {
		*x1 = *x;
		x1->Sat_coffee_machine_alice = 1;
		x1->Sat_water_tap_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice water-tap coffee-jar), id 44 */
double action_goto_alice_water_tap_coffee_jar(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_water_tap_alice) *specificity=0.17; 

	if(x->Sat_water_tap_alice) {
		*x1 = *x;
		x1->Sat_coffee_jar_alice = 1;
		x1->Sat_water_tap_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice coffee-jar door), id 45 */
double action_goto_alice_coffee_jar_door(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_jar_alice) *specificity=0.17; 

	if(x->Sat_coffee_jar_alice) {
		*x1 = *x;
		x1->Sat_door_alice = 1;
		x1->Sat_coffee_jar_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice coffee-jar printer), id 46 */
double action_goto_alice_coffee_jar_printer(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_jar_alice) *specificity=0.17; 

	if(x->Sat_coffee_jar_alice) {
		*x1 = *x;
		x1->Sat_printer_alice = 1;
		x1->Sat_coffee_jar_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice coffee-jar paper-stack), id 47 */
double action_goto_alice_coffee_jar_paper_stack(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_jar_alice) *specificity=0.17; 

	if(x->Sat_coffee_jar_alice) {
		*x1 = *x;
		x1->Sat_paper_stack_alice = 1;
		x1->Sat_coffee_jar_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice coffee-jar coffee-machine), id 48 */
double action_goto_alice_coffee_jar_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_jar_alice) *specificity=0.17; 

	if(x->Sat_coffee_jar_alice) {
		*x1 = *x;
		x1->Sat_coffee_machine_alice = 1;
		x1->Sat_coffee_jar_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto alice coffee-jar water-tap), id 49 */
double action_goto_alice_coffee_jar_water_tap(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_jar_alice) *specificity=0.17; 

	if(x->Sat_coffee_jar_alice) {
		*x1 = *x;
		x1->Sat_water_tap_alice = 1;
		x1->Sat_coffee_jar_alice = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob door printer), id 50 */
double action_goto_bob_door_printer(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_door_bob) *specificity=0.17; 

	if(x->Sat_door_bob) {
		*x1 = *x;
		x1->Sat_printer_bob = 1;
		x1->Sat_door_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob door paper-stack), id 51 */
double action_goto_bob_door_paper_stack(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_door_bob) *specificity=0.17; 

	if(x->Sat_door_bob) {
		*x1 = *x;
		x1->Sat_paper_stack_bob = 1;
		x1->Sat_door_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob door coffee-machine), id 52 */
double action_goto_bob_door_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_door_bob) *specificity=0.17; 

	if(x->Sat_door_bob) {
		*x1 = *x;
		x1->Sat_coffee_machine_bob = 1;
		x1->Sat_door_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob door water-tap), id 53 */
double action_goto_bob_door_water_tap(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_door_bob) *specificity=0.17; 

	if(x->Sat_door_bob) {
		*x1 = *x;
		x1->Sat_water_tap_bob = 1;
		x1->Sat_door_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob door coffee-jar), id 54 */
double action_goto_bob_door_coffee_jar(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_door_bob) *specificity=0.17; 

	if(x->Sat_door_bob) {
		*x1 = *x;
		x1->Sat_coffee_jar_bob = 1;
		x1->Sat_door_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob printer door), id 55 */
double action_goto_bob_printer_door(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_printer_bob) *specificity=0.14; 

	if(x->Sat_printer_bob) {
		*x1 = *x;
		x1->Sat_door_bob = 1;
		x1->Sat_printer_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob printer paper-stack), id 56 */
double action_goto_bob_printer_paper_stack(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_printer_bob) *specificity=0.14; 

	if(x->Sat_printer_bob) {
		*x1 = *x;
		x1->Sat_paper_stack_bob = 1;
		x1->Sat_printer_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob printer coffee-machine), id 57 */
double action_goto_bob_printer_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_printer_bob) *specificity=0.14; 

	if(x->Sat_printer_bob) {
		*x1 = *x;
		x1->Sat_coffee_machine_bob = 1;
		x1->Sat_printer_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob printer water-tap), id 58 */
double action_goto_bob_printer_water_tap(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_printer_bob) *specificity=0.14; 

	if(x->Sat_printer_bob) {
		*x1 = *x;
		x1->Sat_water_tap_bob = 1;
		x1->Sat_printer_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob printer coffee-jar), id 59 */
double action_goto_bob_printer_coffee_jar(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_printer_bob) *specificity=0.14; 

	if(x->Sat_printer_bob) {
		*x1 = *x;
		x1->Sat_coffee_jar_bob = 1;
		x1->Sat_printer_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob paper-stack door), id 60 */
double action_goto_bob_paper_stack_door(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_paper_stack_bob) *specificity=0.17; 

	if(x->Sat_paper_stack_bob) {
		*x1 = *x;
		x1->Sat_door_bob = 1;
		x1->Sat_paper_stack_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob paper-stack printer), id 61 */
double action_goto_bob_paper_stack_printer(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_paper_stack_bob) *specificity=0.17; 

	if(x->Sat_paper_stack_bob) {
		*x1 = *x;
		x1->Sat_printer_bob = 1;
		x1->Sat_paper_stack_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob paper-stack coffee-machine), id 62 */
double action_goto_bob_paper_stack_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_paper_stack_bob) *specificity=0.17; 

	if(x->Sat_paper_stack_bob) {
		*x1 = *x;
		x1->Sat_coffee_machine_bob = 1;
		x1->Sat_paper_stack_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob paper-stack water-tap), id 63 */
double action_goto_bob_paper_stack_water_tap(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_paper_stack_bob) *specificity=0.17; 

	if(x->Sat_paper_stack_bob) {
		*x1 = *x;
		x1->Sat_water_tap_bob = 1;
		x1->Sat_paper_stack_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob paper-stack coffee-jar), id 64 */
double action_goto_bob_paper_stack_coffee_jar(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_paper_stack_bob) *specificity=0.17; 

	if(x->Sat_paper_stack_bob) {
		*x1 = *x;
		x1->Sat_coffee_jar_bob = 1;
		x1->Sat_paper_stack_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob coffee-machine door), id 65 */
double action_goto_bob_coffee_machine_door(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_machine_bob) *specificity=0.12; 

	if(x->Sat_coffee_machine_bob) {
		*x1 = *x;
		x1->Sat_door_bob = 1;
		x1->Sat_coffee_machine_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob coffee-machine printer), id 66 */
double action_goto_bob_coffee_machine_printer(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_machine_bob) *specificity=0.12; 

	if(x->Sat_coffee_machine_bob) {
		*x1 = *x;
		x1->Sat_printer_bob = 1;
		x1->Sat_coffee_machine_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob coffee-machine paper-stack), id 67 */
double action_goto_bob_coffee_machine_paper_stack(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_machine_bob) *specificity=0.12; 

	if(x->Sat_coffee_machine_bob) {
		*x1 = *x;
		x1->Sat_paper_stack_bob = 1;
		x1->Sat_coffee_machine_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob coffee-machine water-tap), id 68 */
double action_goto_bob_coffee_machine_water_tap(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_machine_bob) *specificity=0.12; 

	if(x->Sat_coffee_machine_bob) {
		*x1 = *x;
		x1->Sat_water_tap_bob = 1;
		x1->Sat_coffee_machine_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob coffee-machine coffee-jar), id 69 */
double action_goto_bob_coffee_machine_coffee_jar(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_machine_bob) *specificity=0.12; 

	if(x->Sat_coffee_machine_bob) {
		*x1 = *x;
		x1->Sat_coffee_jar_bob = 1;
		x1->Sat_coffee_machine_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob water-tap door), id 70 */
double action_goto_bob_water_tap_door(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_water_tap_bob) *specificity=0.17; 

	if(x->Sat_water_tap_bob) {
		*x1 = *x;
		x1->Sat_door_bob = 1;
		x1->Sat_water_tap_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob water-tap printer), id 71 */
double action_goto_bob_water_tap_printer(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_water_tap_bob) *specificity=0.17; 

	if(x->Sat_water_tap_bob) {
		*x1 = *x;
		x1->Sat_printer_bob = 1;
		x1->Sat_water_tap_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob water-tap paper-stack), id 72 */
double action_goto_bob_water_tap_paper_stack(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_water_tap_bob) *specificity=0.17; 

	if(x->Sat_water_tap_bob) {
		*x1 = *x;
		x1->Sat_paper_stack_bob = 1;
		x1->Sat_water_tap_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob water-tap coffee-machine), id 73 */
double action_goto_bob_water_tap_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_water_tap_bob) *specificity=0.17; 

	if(x->Sat_water_tap_bob) {
		*x1 = *x;
		x1->Sat_coffee_machine_bob = 1;
		x1->Sat_water_tap_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob water-tap coffee-jar), id 74 */
double action_goto_bob_water_tap_coffee_jar(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_water_tap_bob) *specificity=0.17; 

	if(x->Sat_water_tap_bob) {
		*x1 = *x;
		x1->Sat_coffee_jar_bob = 1;
		x1->Sat_water_tap_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob coffee-jar door), id 75 */
double action_goto_bob_coffee_jar_door(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_jar_bob) *specificity=0.17; 

	if(x->Sat_coffee_jar_bob) {
		*x1 = *x;
		x1->Sat_door_bob = 1;
		x1->Sat_coffee_jar_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob coffee-jar printer), id 76 */
double action_goto_bob_coffee_jar_printer(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_jar_bob) *specificity=0.17; 

	if(x->Sat_coffee_jar_bob) {
		*x1 = *x;
		x1->Sat_printer_bob = 1;
		x1->Sat_coffee_jar_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob coffee-jar paper-stack), id 77 */
double action_goto_bob_coffee_jar_paper_stack(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_jar_bob) *specificity=0.17; 

	if(x->Sat_coffee_jar_bob) {
		*x1 = *x;
		x1->Sat_paper_stack_bob = 1;
		x1->Sat_coffee_jar_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob coffee-jar coffee-machine), id 78 */
double action_goto_bob_coffee_jar_coffee_machine(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_jar_bob) *specificity=0.17; 

	if(x->Sat_coffee_jar_bob) {
		*x1 = *x;
		x1->Sat_coffee_machine_bob = 1;
		x1->Sat_coffee_jar_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (goto bob coffee-jar water-tap), id 79 */
double action_goto_bob_coffee_jar_water_tap(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_coffee_jar_bob) *specificity=0.17; 

	if(x->Sat_coffee_jar_bob) {
		*x1 = *x;
		x1->Sat_water_tap_bob = 1;
		x1->Sat_coffee_jar_bob = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (enter alice), id 80 */
double action_enter_alice(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_outside_alice) *specificity=1.00; 

	if(x->Sat_outside_alice) {
		*x1 = *x;
		x1->Sat_outside_alice = 0;
		x1->Sat_door_alice = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (enter bob), id 81 */
double action_enter_bob(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_outside_bob) *specificity=1.00; 

	if(x->Sat_outside_bob) {
		*x1 = *x;
		x1->Sat_outside_bob = 0;
		x1->Sat_door_bob = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (exit alice), id 82 */
double action_exit_alice(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_door_alice) *specificity=0.17; 

	if(x->Sat_door_alice) {
		*x1 = *x;
		x1->Sat_door_alice = 0;
		x1->Sat_outside_alice = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (exit bob), id 83 */
double action_exit_bob(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sat_door_bob) *specificity=0.17; 

	if(x->Sat_door_bob) {
		*x1 = *x;
		x1->Sat_door_bob = 0;
		x1->Sat_outside_bob = 1;
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
		r->Sprinted_a=1; r->Sprinter_jammed=1; r->Shas_paper_printer=1; 
	} else 
		 return (exp(rT[0]-t)+exp(rT[3]-t)+exp(rT[5]-t))/3;
	return -1;
}
double getRecOrRefr1 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sprinted_b=1; r->Sprinter_jammed=1; r->Shas_paper_printer=1; 
	} else 
		 return (exp(rT[1]-t)+exp(rT[3]-t)+exp(rT[5]-t))/3;
	return -1;
}
double getRecOrRefr2 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sprinted_c=1; r->Sprinter_jammed=1; r->Shas_paper_printer=1; 
	} else 
		 return (exp(rT[2]-t)+exp(rT[3]-t)+exp(rT[5]-t))/3;
	return -1;
}
double getRecOrRefr3 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sprinter_jammed=1; 
	} else 
		 return (exp(rT[3]-t))/1;
	return -1;
}
double getRecOrRefr4 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_printer_alice=1; r->Shands_free_alice=1; r->Sprinter_jammed=1; 
	} else 
		 return (exp(rT[9]-t)+exp(rT[21]-t)+exp(rT[3]-t))/3;
	return -1;
}
double getRecOrRefr5 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_printer_bob=1; r->Shands_free_bob=1; r->Sprinter_jammed=1; 
	} else 
		 return (exp(rT[10]-t)+exp(rT[22]-t)+exp(rT[3]-t))/3;
	return -1;
}
double getRecOrRefr6 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_machine_alice=1; r->Shas_water_coffee_machine=1; r->Shas_ground_coffee_coffee_machine=1; r->Shands_free_alice=1; 
	} else 
		 return (exp(rT[13]-t)+exp(rT[4]-t)+exp(rT[6]-t)+exp(rT[21]-t))/4;
	return -1;
}
double getRecOrRefr7 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_machine_bob=1; r->Shas_water_coffee_machine=1; r->Shas_ground_coffee_coffee_machine=1; r->Shands_free_bob=1; 
	} else 
		 return (exp(rT[14]-t)+exp(rT[4]-t)+exp(rT[6]-t)+exp(rT[22]-t))/4;
	return -1;
}
double getRecOrRefr8 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Shands_free_alice=1; r->Sat_water_tap_alice=1; 
	} else 
		 return (exp(rT[21]-t)+exp(rT[15]-t))/2;
	return -1;
}
double getRecOrRefr9 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Shands_free_alice=1; r->Sat_paper_stack_alice=1; 
	} else 
		 return (exp(rT[21]-t)+exp(rT[11]-t))/2;
	return -1;
}
double getRecOrRefr10 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Shands_free_alice=1; r->Sat_coffee_jar_alice=1; 
	} else 
		 return (exp(rT[21]-t)+exp(rT[17]-t))/2;
	return -1;
}
double getRecOrRefr11 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Shands_free_bob=1; r->Sat_water_tap_bob=1; 
	} else 
		 return (exp(rT[22]-t)+exp(rT[16]-t))/2;
	return -1;
}
double getRecOrRefr12 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Shands_free_bob=1; r->Sat_paper_stack_bob=1; 
	} else 
		 return (exp(rT[22]-t)+exp(rT[12]-t))/2;
	return -1;
}
double getRecOrRefr13 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Shands_free_bob=1; r->Sat_coffee_jar_bob=1; 
	} else 
		 return (exp(rT[22]-t)+exp(rT[18]-t))/2;
	return -1;
}
double getRecOrRefr14 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sholds_water_alice=1; r->Sat_coffee_machine_alice=1; r->Shas_water_coffee_machine=1; 
	} else 
		 return (exp(rT[23]-t)+exp(rT[13]-t)+exp(rT[4]-t))/3;
	return -1;
}
double getRecOrRefr15 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sholds_paper_alice=1; r->Sat_printer_alice=1; r->Shas_paper_printer=1; 
	} else 
		 return (exp(rT[25]-t)+exp(rT[9]-t)+exp(rT[5]-t))/3;
	return -1;
}
double getRecOrRefr16 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sholds_ground_coffee_alice=1; r->Sat_coffee_machine_alice=1; r->Shas_ground_coffee_coffee_machine=1; 
	} else 
		 return (exp(rT[27]-t)+exp(rT[13]-t)+exp(rT[6]-t))/3;
	return -1;
}
double getRecOrRefr17 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sholds_water_bob=1; r->Sat_coffee_machine_bob=1; r->Shas_water_coffee_machine=1; 
	} else 
		 return (exp(rT[24]-t)+exp(rT[14]-t)+exp(rT[4]-t))/3;
	return -1;
}
double getRecOrRefr18 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sholds_paper_bob=1; r->Sat_printer_bob=1; r->Shas_paper_printer=1; 
	} else 
		 return (exp(rT[26]-t)+exp(rT[10]-t)+exp(rT[5]-t))/3;
	return -1;
}
double getRecOrRefr19 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sholds_ground_coffee_bob=1; r->Sat_coffee_machine_bob=1; r->Shas_ground_coffee_coffee_machine=1; 
	} else 
		 return (exp(rT[28]-t)+exp(rT[14]-t)+exp(rT[6]-t))/3;
	return -1;
}
double getRecOrRefr20 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_door_alice=1; 
	} else 
		 return (exp(rT[7]-t))/1;
	return -1;
}
double getRecOrRefr21 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_door_alice=1; 
	} else 
		 return (exp(rT[7]-t))/1;
	return -1;
}
double getRecOrRefr22 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_door_alice=1; 
	} else 
		 return (exp(rT[7]-t))/1;
	return -1;
}
double getRecOrRefr23 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_door_alice=1; 
	} else 
		 return (exp(rT[7]-t))/1;
	return -1;
}
double getRecOrRefr24 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_door_alice=1; 
	} else 
		 return (exp(rT[7]-t))/1;
	return -1;
}
double getRecOrRefr25 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_printer_alice=1; 
	} else 
		 return (exp(rT[9]-t))/1;
	return -1;
}
double getRecOrRefr26 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_printer_alice=1; 
	} else 
		 return (exp(rT[9]-t))/1;
	return -1;
}
double getRecOrRefr27 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_printer_alice=1; 
	} else 
		 return (exp(rT[9]-t))/1;
	return -1;
}
double getRecOrRefr28 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_printer_alice=1; 
	} else 
		 return (exp(rT[9]-t))/1;
	return -1;
}
double getRecOrRefr29 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_printer_alice=1; 
	} else 
		 return (exp(rT[9]-t))/1;
	return -1;
}
double getRecOrRefr30 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_paper_stack_alice=1; 
	} else 
		 return (exp(rT[11]-t))/1;
	return -1;
}
double getRecOrRefr31 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_paper_stack_alice=1; 
	} else 
		 return (exp(rT[11]-t))/1;
	return -1;
}
double getRecOrRefr32 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_paper_stack_alice=1; 
	} else 
		 return (exp(rT[11]-t))/1;
	return -1;
}
double getRecOrRefr33 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_paper_stack_alice=1; 
	} else 
		 return (exp(rT[11]-t))/1;
	return -1;
}
double getRecOrRefr34 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_paper_stack_alice=1; 
	} else 
		 return (exp(rT[11]-t))/1;
	return -1;
}
double getRecOrRefr35 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_machine_alice=1; 
	} else 
		 return (exp(rT[13]-t))/1;
	return -1;
}
double getRecOrRefr36 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_machine_alice=1; 
	} else 
		 return (exp(rT[13]-t))/1;
	return -1;
}
double getRecOrRefr37 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_machine_alice=1; 
	} else 
		 return (exp(rT[13]-t))/1;
	return -1;
}
double getRecOrRefr38 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_machine_alice=1; 
	} else 
		 return (exp(rT[13]-t))/1;
	return -1;
}
double getRecOrRefr39 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_machine_alice=1; 
	} else 
		 return (exp(rT[13]-t))/1;
	return -1;
}
double getRecOrRefr40 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_water_tap_alice=1; 
	} else 
		 return (exp(rT[15]-t))/1;
	return -1;
}
double getRecOrRefr41 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_water_tap_alice=1; 
	} else 
		 return (exp(rT[15]-t))/1;
	return -1;
}
double getRecOrRefr42 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_water_tap_alice=1; 
	} else 
		 return (exp(rT[15]-t))/1;
	return -1;
}
double getRecOrRefr43 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_water_tap_alice=1; 
	} else 
		 return (exp(rT[15]-t))/1;
	return -1;
}
double getRecOrRefr44 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_water_tap_alice=1; 
	} else 
		 return (exp(rT[15]-t))/1;
	return -1;
}
double getRecOrRefr45 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_jar_alice=1; 
	} else 
		 return (exp(rT[17]-t))/1;
	return -1;
}
double getRecOrRefr46 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_jar_alice=1; 
	} else 
		 return (exp(rT[17]-t))/1;
	return -1;
}
double getRecOrRefr47 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_jar_alice=1; 
	} else 
		 return (exp(rT[17]-t))/1;
	return -1;
}
double getRecOrRefr48 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_jar_alice=1; 
	} else 
		 return (exp(rT[17]-t))/1;
	return -1;
}
double getRecOrRefr49 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_jar_alice=1; 
	} else 
		 return (exp(rT[17]-t))/1;
	return -1;
}
double getRecOrRefr50 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_door_bob=1; 
	} else 
		 return (exp(rT[8]-t))/1;
	return -1;
}
double getRecOrRefr51 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_door_bob=1; 
	} else 
		 return (exp(rT[8]-t))/1;
	return -1;
}
double getRecOrRefr52 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_door_bob=1; 
	} else 
		 return (exp(rT[8]-t))/1;
	return -1;
}
double getRecOrRefr53 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_door_bob=1; 
	} else 
		 return (exp(rT[8]-t))/1;
	return -1;
}
double getRecOrRefr54 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_door_bob=1; 
	} else 
		 return (exp(rT[8]-t))/1;
	return -1;
}
double getRecOrRefr55 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_printer_bob=1; 
	} else 
		 return (exp(rT[10]-t))/1;
	return -1;
}
double getRecOrRefr56 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_printer_bob=1; 
	} else 
		 return (exp(rT[10]-t))/1;
	return -1;
}
double getRecOrRefr57 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_printer_bob=1; 
	} else 
		 return (exp(rT[10]-t))/1;
	return -1;
}
double getRecOrRefr58 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_printer_bob=1; 
	} else 
		 return (exp(rT[10]-t))/1;
	return -1;
}
double getRecOrRefr59 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_printer_bob=1; 
	} else 
		 return (exp(rT[10]-t))/1;
	return -1;
}
double getRecOrRefr60 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_paper_stack_bob=1; 
	} else 
		 return (exp(rT[12]-t))/1;
	return -1;
}
double getRecOrRefr61 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_paper_stack_bob=1; 
	} else 
		 return (exp(rT[12]-t))/1;
	return -1;
}
double getRecOrRefr62 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_paper_stack_bob=1; 
	} else 
		 return (exp(rT[12]-t))/1;
	return -1;
}
double getRecOrRefr63 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_paper_stack_bob=1; 
	} else 
		 return (exp(rT[12]-t))/1;
	return -1;
}
double getRecOrRefr64 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_paper_stack_bob=1; 
	} else 
		 return (exp(rT[12]-t))/1;
	return -1;
}
double getRecOrRefr65 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_machine_bob=1; 
	} else 
		 return (exp(rT[14]-t))/1;
	return -1;
}
double getRecOrRefr66 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_machine_bob=1; 
	} else 
		 return (exp(rT[14]-t))/1;
	return -1;
}
double getRecOrRefr67 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_machine_bob=1; 
	} else 
		 return (exp(rT[14]-t))/1;
	return -1;
}
double getRecOrRefr68 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_machine_bob=1; 
	} else 
		 return (exp(rT[14]-t))/1;
	return -1;
}
double getRecOrRefr69 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_machine_bob=1; 
	} else 
		 return (exp(rT[14]-t))/1;
	return -1;
}
double getRecOrRefr70 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_water_tap_bob=1; 
	} else 
		 return (exp(rT[16]-t))/1;
	return -1;
}
double getRecOrRefr71 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_water_tap_bob=1; 
	} else 
		 return (exp(rT[16]-t))/1;
	return -1;
}
double getRecOrRefr72 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_water_tap_bob=1; 
	} else 
		 return (exp(rT[16]-t))/1;
	return -1;
}
double getRecOrRefr73 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_water_tap_bob=1; 
	} else 
		 return (exp(rT[16]-t))/1;
	return -1;
}
double getRecOrRefr74 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_water_tap_bob=1; 
	} else 
		 return (exp(rT[16]-t))/1;
	return -1;
}
double getRecOrRefr75 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_jar_bob=1; 
	} else 
		 return (exp(rT[18]-t))/1;
	return -1;
}
double getRecOrRefr76 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_jar_bob=1; 
	} else 
		 return (exp(rT[18]-t))/1;
	return -1;
}
double getRecOrRefr77 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_jar_bob=1; 
	} else 
		 return (exp(rT[18]-t))/1;
	return -1;
}
double getRecOrRefr78 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_jar_bob=1; 
	} else 
		 return (exp(rT[18]-t))/1;
	return -1;
}
double getRecOrRefr79 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_coffee_jar_bob=1; 
	} else 
		 return (exp(rT[18]-t))/1;
	return -1;
}
double getRecOrRefr80 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_outside_alice=1; 
	} else 
		 return (exp(rT[19]-t))/1;
	return -1;
}
double getRecOrRefr81 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_outside_bob=1; 
	} else 
		 return (exp(rT[20]-t))/1;
	return -1;
}
double getRecOrRefr82 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_door_alice=1; 
	} else 
		 return (exp(rT[7]-t))/1;
	return -1;
}
double getRecOrRefr83 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sat_door_bob=1; 
	} else 
		 return (exp(rT[8]-t))/1;
	return -1;
}


namespace ActionSchemes {
const ActionScheme s_FINISHED("FINISHED", std::vector<paramType>());
const ActionScheme s_BLOCKED("BLOCKED", std::vector<paramType>());
const ActionScheme s_INITIALIZE("INITIALIZE", std::vector<paramType>());
boost::array<paramType,1> __params_print = {std::make_pair("j","job")};
const ActionScheme s_print("print", std::vector<paramType>(__params_print.begin(), __params_print.end()));
boost::array<paramType,0> __params__JAM_PRINTER_ = {};
const ActionScheme s__JAM_PRINTER_("*****JAM-PRINTER******", std::vector<paramType>(__params__JAM_PRINTER_.begin(), __params__JAM_PRINTER_.end()));
boost::array<paramType,1> __params_repair_printer = {std::make_pair("a","agent")};
const ActionScheme s_repair_printer("repair-printer", std::vector<paramType>(__params_repair_printer.begin(), __params_repair_printer.end()));
boost::array<paramType,1> __params_get_coffee = {std::make_pair("a","agent")};
const ActionScheme s_get_coffee("get-coffee", std::vector<paramType>(__params_get_coffee.begin(), __params_get_coffee.end()));
boost::array<paramType,3> __params_fetch = {std::make_pair("a","agent"),std::make_pair("i","item"),std::make_pair("l","location")};
const ActionScheme s_fetch("fetch", std::vector<paramType>(__params_fetch.begin(), __params_fetch.end()));
boost::array<paramType,3> __params_replenish = {std::make_pair("a","agent"),std::make_pair("i","item"),std::make_pair("l","location")};
const ActionScheme s_replenish("replenish", std::vector<paramType>(__params_replenish.begin(), __params_replenish.end()));
boost::array<paramType,3> __params_goto = {std::make_pair("a","agent"),std::make_pair("x","glocation"),std::make_pair("y","glocation")};
const ActionScheme s_goto("goto", std::vector<paramType>(__params_goto.begin(), __params_goto.end()));
boost::array<paramType,1> __params_enter = {std::make_pair("a","agent")};
const ActionScheme s_enter("enter", std::vector<paramType>(__params_enter.begin(), __params_enter.end()));
boost::array<paramType,1> __params_exit = {std::make_pair("a","agent")};
const ActionScheme s_exit("exit", std::vector<paramType>(__params_exit.begin(), __params_exit.end()));
} // namespace ActionSchemes

ActionScheme const * __actionSchemes[9 + 3] = {
	&ActionSchemes::s_FINISHED,
	&ActionSchemes::s_BLOCKED,
	&ActionSchemes::s_INITIALIZE,
	&ActionSchemes::s_print,
	&ActionSchemes::s__JAM_PRINTER_,
	&ActionSchemes::s_repair_printer,
	&ActionSchemes::s_get_coffee,
	&ActionSchemes::s_fetch,
	&ActionSchemes::s_replenish,
	&ActionSchemes::s_goto,
	&ActionSchemes::s_enter,
	&ActionSchemes::s_exit
};
ActionScheme const * *actionSchemes = &__actionSchemes[3];

namespace Actions {
const ActionSchemes::S_FINISHED a_FINISHED(FinishedOpId, "FINISHED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_BLOCKED a_BLOCKED(NoOpId, "BLOCKED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_INITIALIZE a_INITIALIZE(InitOpId, "INITIALIZE", NULL, NULL, NULL, finishedImmediate , pStopcdfImmediate, pStoppdfImmediate, 0.0 /* Initial */);
const ActionSchemes::S_print a_print_a(0, "(print a)", action_print_a, NULL, getRecOrRefr0, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 0 */);
const ActionSchemes::S_print a_print_b(1, "(print b)", action_print_b, NULL, getRecOrRefr1, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 1 */);
const ActionSchemes::S_print a_print_c(2, "(print c)", action_print_c, NULL, getRecOrRefr2, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 2 */);
const ActionSchemes::S__JAM_PRINTER_ a__JAM_PRINTER_(3, "(*****JAM-PRINTER******)", action__JAM_PRINTER_, NULL, getRecOrRefr3, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 3 */);
const ActionSchemes::S_repair_printer a_repair_printer_alice(4, "(repair-printer alice)", action_repair_printer_alice, NULL, getRecOrRefr4, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 4 */);
const ActionSchemes::S_repair_printer a_repair_printer_bob(5, "(repair-printer bob)", action_repair_printer_bob, NULL, getRecOrRefr5, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 5 */);
const ActionSchemes::S_get_coffee a_get_coffee_alice(6, "(get-coffee alice)", action_get_coffee_alice, NULL, getRecOrRefr6, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 6 */);
const ActionSchemes::S_get_coffee a_get_coffee_bob(7, "(get-coffee bob)", action_get_coffee_bob, NULL, getRecOrRefr7, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 7 */);
const ActionSchemes::S_fetch a_fetch_alice_water_water_tap(8, "(fetch alice water water-tap)", action_fetch_alice_water_water_tap, NULL, getRecOrRefr8, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 8 */);
const ActionSchemes::S_fetch a_fetch_alice_paper_paper_stack(9, "(fetch alice paper paper-stack)", action_fetch_alice_paper_paper_stack, NULL, getRecOrRefr9, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 9 */);
const ActionSchemes::S_fetch a_fetch_alice_ground_coffee_coffee_jar(10, "(fetch alice ground-coffee coffee-jar)", action_fetch_alice_ground_coffee_coffee_jar, NULL, getRecOrRefr10, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 10 */);
const ActionSchemes::S_fetch a_fetch_bob_water_water_tap(11, "(fetch bob water water-tap)", action_fetch_bob_water_water_tap, NULL, getRecOrRefr11, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 11 */);
const ActionSchemes::S_fetch a_fetch_bob_paper_paper_stack(12, "(fetch bob paper paper-stack)", action_fetch_bob_paper_paper_stack, NULL, getRecOrRefr12, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 12 */);
const ActionSchemes::S_fetch a_fetch_bob_ground_coffee_coffee_jar(13, "(fetch bob ground-coffee coffee-jar)", action_fetch_bob_ground_coffee_coffee_jar, NULL, getRecOrRefr13, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 13 */);
const ActionSchemes::S_replenish a_replenish_alice_water_coffee_machine(14, "(replenish alice water coffee-machine)", action_replenish_alice_water_coffee_machine, NULL, getRecOrRefr14, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 14 */);
const ActionSchemes::S_replenish a_replenish_alice_paper_printer(15, "(replenish alice paper printer)", action_replenish_alice_paper_printer, NULL, getRecOrRefr15, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 15 */);
const ActionSchemes::S_replenish a_replenish_alice_ground_coffee_coffee_machine(16, "(replenish alice ground-coffee coffee-machine)", action_replenish_alice_ground_coffee_coffee_machine, NULL, getRecOrRefr16, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 16 */);
const ActionSchemes::S_replenish a_replenish_bob_water_coffee_machine(17, "(replenish bob water coffee-machine)", action_replenish_bob_water_coffee_machine, NULL, getRecOrRefr17, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 17 */);
const ActionSchemes::S_replenish a_replenish_bob_paper_printer(18, "(replenish bob paper printer)", action_replenish_bob_paper_printer, NULL, getRecOrRefr18, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 18 */);
const ActionSchemes::S_replenish a_replenish_bob_ground_coffee_coffee_machine(19, "(replenish bob ground-coffee coffee-machine)", action_replenish_bob_ground_coffee_coffee_machine, NULL, getRecOrRefr19, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 19 */);
const ActionSchemes::S_goto a_goto_alice_door_printer(20, "(goto alice door printer)", action_goto_alice_door_printer, NULL, getRecOrRefr20, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 20 */);
const ActionSchemes::S_goto a_goto_alice_door_paper_stack(21, "(goto alice door paper-stack)", action_goto_alice_door_paper_stack, NULL, getRecOrRefr21, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 21 */);
const ActionSchemes::S_goto a_goto_alice_door_coffee_machine(22, "(goto alice door coffee-machine)", action_goto_alice_door_coffee_machine, NULL, getRecOrRefr22, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 22 */);
const ActionSchemes::S_goto a_goto_alice_door_water_tap(23, "(goto alice door water-tap)", action_goto_alice_door_water_tap, NULL, getRecOrRefr23, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 23 */);
const ActionSchemes::S_goto a_goto_alice_door_coffee_jar(24, "(goto alice door coffee-jar)", action_goto_alice_door_coffee_jar, NULL, getRecOrRefr24, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 24 */);
const ActionSchemes::S_goto a_goto_alice_printer_door(25, "(goto alice printer door)", action_goto_alice_printer_door, NULL, getRecOrRefr25, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 25 */);
const ActionSchemes::S_goto a_goto_alice_printer_paper_stack(26, "(goto alice printer paper-stack)", action_goto_alice_printer_paper_stack, NULL, getRecOrRefr26, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 26 */);
const ActionSchemes::S_goto a_goto_alice_printer_coffee_machine(27, "(goto alice printer coffee-machine)", action_goto_alice_printer_coffee_machine, NULL, getRecOrRefr27, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 27 */);
const ActionSchemes::S_goto a_goto_alice_printer_water_tap(28, "(goto alice printer water-tap)", action_goto_alice_printer_water_tap, NULL, getRecOrRefr28, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 28 */);
const ActionSchemes::S_goto a_goto_alice_printer_coffee_jar(29, "(goto alice printer coffee-jar)", action_goto_alice_printer_coffee_jar, NULL, getRecOrRefr29, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 29 */);
const ActionSchemes::S_goto a_goto_alice_paper_stack_door(30, "(goto alice paper-stack door)", action_goto_alice_paper_stack_door, NULL, getRecOrRefr30, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 30 */);
const ActionSchemes::S_goto a_goto_alice_paper_stack_printer(31, "(goto alice paper-stack printer)", action_goto_alice_paper_stack_printer, NULL, getRecOrRefr31, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 31 */);
const ActionSchemes::S_goto a_goto_alice_paper_stack_coffee_machine(32, "(goto alice paper-stack coffee-machine)", action_goto_alice_paper_stack_coffee_machine, NULL, getRecOrRefr32, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 32 */);
const ActionSchemes::S_goto a_goto_alice_paper_stack_water_tap(33, "(goto alice paper-stack water-tap)", action_goto_alice_paper_stack_water_tap, NULL, getRecOrRefr33, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 33 */);
const ActionSchemes::S_goto a_goto_alice_paper_stack_coffee_jar(34, "(goto alice paper-stack coffee-jar)", action_goto_alice_paper_stack_coffee_jar, NULL, getRecOrRefr34, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 34 */);
const ActionSchemes::S_goto a_goto_alice_coffee_machine_door(35, "(goto alice coffee-machine door)", action_goto_alice_coffee_machine_door, NULL, getRecOrRefr35, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 35 */);
const ActionSchemes::S_goto a_goto_alice_coffee_machine_printer(36, "(goto alice coffee-machine printer)", action_goto_alice_coffee_machine_printer, NULL, getRecOrRefr36, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 36 */);
const ActionSchemes::S_goto a_goto_alice_coffee_machine_paper_stack(37, "(goto alice coffee-machine paper-stack)", action_goto_alice_coffee_machine_paper_stack, NULL, getRecOrRefr37, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 37 */);
const ActionSchemes::S_goto a_goto_alice_coffee_machine_water_tap(38, "(goto alice coffee-machine water-tap)", action_goto_alice_coffee_machine_water_tap, NULL, getRecOrRefr38, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 38 */);
const ActionSchemes::S_goto a_goto_alice_coffee_machine_coffee_jar(39, "(goto alice coffee-machine coffee-jar)", action_goto_alice_coffee_machine_coffee_jar, NULL, getRecOrRefr39, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 39 */);
const ActionSchemes::S_goto a_goto_alice_water_tap_door(40, "(goto alice water-tap door)", action_goto_alice_water_tap_door, NULL, getRecOrRefr40, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 40 */);
const ActionSchemes::S_goto a_goto_alice_water_tap_printer(41, "(goto alice water-tap printer)", action_goto_alice_water_tap_printer, NULL, getRecOrRefr41, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 41 */);
const ActionSchemes::S_goto a_goto_alice_water_tap_paper_stack(42, "(goto alice water-tap paper-stack)", action_goto_alice_water_tap_paper_stack, NULL, getRecOrRefr42, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 42 */);
const ActionSchemes::S_goto a_goto_alice_water_tap_coffee_machine(43, "(goto alice water-tap coffee-machine)", action_goto_alice_water_tap_coffee_machine, NULL, getRecOrRefr43, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 43 */);
const ActionSchemes::S_goto a_goto_alice_water_tap_coffee_jar(44, "(goto alice water-tap coffee-jar)", action_goto_alice_water_tap_coffee_jar, NULL, getRecOrRefr44, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 44 */);
const ActionSchemes::S_goto a_goto_alice_coffee_jar_door(45, "(goto alice coffee-jar door)", action_goto_alice_coffee_jar_door, NULL, getRecOrRefr45, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 45 */);
const ActionSchemes::S_goto a_goto_alice_coffee_jar_printer(46, "(goto alice coffee-jar printer)", action_goto_alice_coffee_jar_printer, NULL, getRecOrRefr46, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 46 */);
const ActionSchemes::S_goto a_goto_alice_coffee_jar_paper_stack(47, "(goto alice coffee-jar paper-stack)", action_goto_alice_coffee_jar_paper_stack, NULL, getRecOrRefr47, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 47 */);
const ActionSchemes::S_goto a_goto_alice_coffee_jar_coffee_machine(48, "(goto alice coffee-jar coffee-machine)", action_goto_alice_coffee_jar_coffee_machine, NULL, getRecOrRefr48, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 48 */);
const ActionSchemes::S_goto a_goto_alice_coffee_jar_water_tap(49, "(goto alice coffee-jar water-tap)", action_goto_alice_coffee_jar_water_tap, NULL, getRecOrRefr49, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 49 */);
const ActionSchemes::S_goto a_goto_bob_door_printer(50, "(goto bob door printer)", action_goto_bob_door_printer, NULL, getRecOrRefr50, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 50 */);
const ActionSchemes::S_goto a_goto_bob_door_paper_stack(51, "(goto bob door paper-stack)", action_goto_bob_door_paper_stack, NULL, getRecOrRefr51, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 51 */);
const ActionSchemes::S_goto a_goto_bob_door_coffee_machine(52, "(goto bob door coffee-machine)", action_goto_bob_door_coffee_machine, NULL, getRecOrRefr52, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 52 */);
const ActionSchemes::S_goto a_goto_bob_door_water_tap(53, "(goto bob door water-tap)", action_goto_bob_door_water_tap, NULL, getRecOrRefr53, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 53 */);
const ActionSchemes::S_goto a_goto_bob_door_coffee_jar(54, "(goto bob door coffee-jar)", action_goto_bob_door_coffee_jar, NULL, getRecOrRefr54, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 54 */);
const ActionSchemes::S_goto a_goto_bob_printer_door(55, "(goto bob printer door)", action_goto_bob_printer_door, NULL, getRecOrRefr55, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 55 */);
const ActionSchemes::S_goto a_goto_bob_printer_paper_stack(56, "(goto bob printer paper-stack)", action_goto_bob_printer_paper_stack, NULL, getRecOrRefr56, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 56 */);
const ActionSchemes::S_goto a_goto_bob_printer_coffee_machine(57, "(goto bob printer coffee-machine)", action_goto_bob_printer_coffee_machine, NULL, getRecOrRefr57, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 57 */);
const ActionSchemes::S_goto a_goto_bob_printer_water_tap(58, "(goto bob printer water-tap)", action_goto_bob_printer_water_tap, NULL, getRecOrRefr58, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 58 */);
const ActionSchemes::S_goto a_goto_bob_printer_coffee_jar(59, "(goto bob printer coffee-jar)", action_goto_bob_printer_coffee_jar, NULL, getRecOrRefr59, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 59 */);
const ActionSchemes::S_goto a_goto_bob_paper_stack_door(60, "(goto bob paper-stack door)", action_goto_bob_paper_stack_door, NULL, getRecOrRefr60, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 60 */);
const ActionSchemes::S_goto a_goto_bob_paper_stack_printer(61, "(goto bob paper-stack printer)", action_goto_bob_paper_stack_printer, NULL, getRecOrRefr61, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 61 */);
const ActionSchemes::S_goto a_goto_bob_paper_stack_coffee_machine(62, "(goto bob paper-stack coffee-machine)", action_goto_bob_paper_stack_coffee_machine, NULL, getRecOrRefr62, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 62 */);
const ActionSchemes::S_goto a_goto_bob_paper_stack_water_tap(63, "(goto bob paper-stack water-tap)", action_goto_bob_paper_stack_water_tap, NULL, getRecOrRefr63, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 63 */);
const ActionSchemes::S_goto a_goto_bob_paper_stack_coffee_jar(64, "(goto bob paper-stack coffee-jar)", action_goto_bob_paper_stack_coffee_jar, NULL, getRecOrRefr64, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 64 */);
const ActionSchemes::S_goto a_goto_bob_coffee_machine_door(65, "(goto bob coffee-machine door)", action_goto_bob_coffee_machine_door, NULL, getRecOrRefr65, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 65 */);
const ActionSchemes::S_goto a_goto_bob_coffee_machine_printer(66, "(goto bob coffee-machine printer)", action_goto_bob_coffee_machine_printer, NULL, getRecOrRefr66, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 66 */);
const ActionSchemes::S_goto a_goto_bob_coffee_machine_paper_stack(67, "(goto bob coffee-machine paper-stack)", action_goto_bob_coffee_machine_paper_stack, NULL, getRecOrRefr67, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 67 */);
const ActionSchemes::S_goto a_goto_bob_coffee_machine_water_tap(68, "(goto bob coffee-machine water-tap)", action_goto_bob_coffee_machine_water_tap, NULL, getRecOrRefr68, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 68 */);
const ActionSchemes::S_goto a_goto_bob_coffee_machine_coffee_jar(69, "(goto bob coffee-machine coffee-jar)", action_goto_bob_coffee_machine_coffee_jar, NULL, getRecOrRefr69, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 69 */);
const ActionSchemes::S_goto a_goto_bob_water_tap_door(70, "(goto bob water-tap door)", action_goto_bob_water_tap_door, NULL, getRecOrRefr70, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 70 */);
const ActionSchemes::S_goto a_goto_bob_water_tap_printer(71, "(goto bob water-tap printer)", action_goto_bob_water_tap_printer, NULL, getRecOrRefr71, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 71 */);
const ActionSchemes::S_goto a_goto_bob_water_tap_paper_stack(72, "(goto bob water-tap paper-stack)", action_goto_bob_water_tap_paper_stack, NULL, getRecOrRefr72, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 72 */);
const ActionSchemes::S_goto a_goto_bob_water_tap_coffee_machine(73, "(goto bob water-tap coffee-machine)", action_goto_bob_water_tap_coffee_machine, NULL, getRecOrRefr73, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 73 */);
const ActionSchemes::S_goto a_goto_bob_water_tap_coffee_jar(74, "(goto bob water-tap coffee-jar)", action_goto_bob_water_tap_coffee_jar, NULL, getRecOrRefr74, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 74 */);
const ActionSchemes::S_goto a_goto_bob_coffee_jar_door(75, "(goto bob coffee-jar door)", action_goto_bob_coffee_jar_door, NULL, getRecOrRefr75, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 75 */);
const ActionSchemes::S_goto a_goto_bob_coffee_jar_printer(76, "(goto bob coffee-jar printer)", action_goto_bob_coffee_jar_printer, NULL, getRecOrRefr76, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 76 */);
const ActionSchemes::S_goto a_goto_bob_coffee_jar_paper_stack(77, "(goto bob coffee-jar paper-stack)", action_goto_bob_coffee_jar_paper_stack, NULL, getRecOrRefr77, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 77 */);
const ActionSchemes::S_goto a_goto_bob_coffee_jar_coffee_machine(78, "(goto bob coffee-jar coffee-machine)", action_goto_bob_coffee_jar_coffee_machine, NULL, getRecOrRefr78, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 78 */);
const ActionSchemes::S_goto a_goto_bob_coffee_jar_water_tap(79, "(goto bob coffee-jar water-tap)", action_goto_bob_coffee_jar_water_tap, NULL, getRecOrRefr79, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 79 */);
const ActionSchemes::S_enter a_enter_alice(80, "(enter alice)", action_enter_alice, NULL, getRecOrRefr80, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 80 */);
const ActionSchemes::S_enter a_enter_bob(81, "(enter bob)", action_enter_bob, NULL, getRecOrRefr81, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 81 */);
const ActionSchemes::S_exit a_exit_alice(82, "(exit alice)", action_exit_alice, NULL, getRecOrRefr82, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 82 */);
const ActionSchemes::S_exit a_exit_bob(83, "(exit bob)", action_exit_bob, NULL, getRecOrRefr83, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 83 */);
} // namespace Actions

Action const * __actions[NOPS+3] = {
	&Actions::a_FINISHED,
	&Actions::a_BLOCKED,
	&Actions::a_INITIALIZE,
	&Actions::a_print_a,
	&Actions::a_print_b,
	&Actions::a_print_c,
	&Actions::a__JAM_PRINTER_,
	&Actions::a_repair_printer_alice,
	&Actions::a_repair_printer_bob,
	&Actions::a_get_coffee_alice,
	&Actions::a_get_coffee_bob,
	&Actions::a_fetch_alice_water_water_tap,
	&Actions::a_fetch_alice_paper_paper_stack,
	&Actions::a_fetch_alice_ground_coffee_coffee_jar,
	&Actions::a_fetch_bob_water_water_tap,
	&Actions::a_fetch_bob_paper_paper_stack,
	&Actions::a_fetch_bob_ground_coffee_coffee_jar,
	&Actions::a_replenish_alice_water_coffee_machine,
	&Actions::a_replenish_alice_paper_printer,
	&Actions::a_replenish_alice_ground_coffee_coffee_machine,
	&Actions::a_replenish_bob_water_coffee_machine,
	&Actions::a_replenish_bob_paper_printer,
	&Actions::a_replenish_bob_ground_coffee_coffee_machine,
	&Actions::a_goto_alice_door_printer,
	&Actions::a_goto_alice_door_paper_stack,
	&Actions::a_goto_alice_door_coffee_machine,
	&Actions::a_goto_alice_door_water_tap,
	&Actions::a_goto_alice_door_coffee_jar,
	&Actions::a_goto_alice_printer_door,
	&Actions::a_goto_alice_printer_paper_stack,
	&Actions::a_goto_alice_printer_coffee_machine,
	&Actions::a_goto_alice_printer_water_tap,
	&Actions::a_goto_alice_printer_coffee_jar,
	&Actions::a_goto_alice_paper_stack_door,
	&Actions::a_goto_alice_paper_stack_printer,
	&Actions::a_goto_alice_paper_stack_coffee_machine,
	&Actions::a_goto_alice_paper_stack_water_tap,
	&Actions::a_goto_alice_paper_stack_coffee_jar,
	&Actions::a_goto_alice_coffee_machine_door,
	&Actions::a_goto_alice_coffee_machine_printer,
	&Actions::a_goto_alice_coffee_machine_paper_stack,
	&Actions::a_goto_alice_coffee_machine_water_tap,
	&Actions::a_goto_alice_coffee_machine_coffee_jar,
	&Actions::a_goto_alice_water_tap_door,
	&Actions::a_goto_alice_water_tap_printer,
	&Actions::a_goto_alice_water_tap_paper_stack,
	&Actions::a_goto_alice_water_tap_coffee_machine,
	&Actions::a_goto_alice_water_tap_coffee_jar,
	&Actions::a_goto_alice_coffee_jar_door,
	&Actions::a_goto_alice_coffee_jar_printer,
	&Actions::a_goto_alice_coffee_jar_paper_stack,
	&Actions::a_goto_alice_coffee_jar_coffee_machine,
	&Actions::a_goto_alice_coffee_jar_water_tap,
	&Actions::a_goto_bob_door_printer,
	&Actions::a_goto_bob_door_paper_stack,
	&Actions::a_goto_bob_door_coffee_machine,
	&Actions::a_goto_bob_door_water_tap,
	&Actions::a_goto_bob_door_coffee_jar,
	&Actions::a_goto_bob_printer_door,
	&Actions::a_goto_bob_printer_paper_stack,
	&Actions::a_goto_bob_printer_coffee_machine,
	&Actions::a_goto_bob_printer_water_tap,
	&Actions::a_goto_bob_printer_coffee_jar,
	&Actions::a_goto_bob_paper_stack_door,
	&Actions::a_goto_bob_paper_stack_printer,
	&Actions::a_goto_bob_paper_stack_coffee_machine,
	&Actions::a_goto_bob_paper_stack_water_tap,
	&Actions::a_goto_bob_paper_stack_coffee_jar,
	&Actions::a_goto_bob_coffee_machine_door,
	&Actions::a_goto_bob_coffee_machine_printer,
	&Actions::a_goto_bob_coffee_machine_paper_stack,
	&Actions::a_goto_bob_coffee_machine_water_tap,
	&Actions::a_goto_bob_coffee_machine_coffee_jar,
	&Actions::a_goto_bob_water_tap_door,
	&Actions::a_goto_bob_water_tap_printer,
	&Actions::a_goto_bob_water_tap_paper_stack,
	&Actions::a_goto_bob_water_tap_coffee_machine,
	&Actions::a_goto_bob_water_tap_coffee_jar,
	&Actions::a_goto_bob_coffee_jar_door,
	&Actions::a_goto_bob_coffee_jar_printer,
	&Actions::a_goto_bob_coffee_jar_paper_stack,
	&Actions::a_goto_bob_coffee_jar_coffee_machine,
	&Actions::a_goto_bob_coffee_jar_water_tap,
	&Actions::a_enter_alice,
	&Actions::a_enter_bob,
	&Actions::a_exit_alice,
	&Actions::a_exit_bob
};
Action const * *actions = &__actions[3];

const int agentOps0[84] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83};
char const * const agentNames[NAGENTS] = {"NULL"};
int const nOpsForAgent[NAGENTS] = {84};
int const * const opsForAgent[NAGENTS] = {agentOps0};

bool isGoalState(size_t goal, StatePtr x) { (void) x;
	switch(goal) {
	case 0: /*abc1*/
		return (x->Sprinted_a && x->Sprinted_b && x->Sprinted_c && x->Sholds_coffee_alice && x->Sholds_coffee_bob && x->Sat_outside_alice && x->Sat_outside_bob);
	default: throw std::runtime_error("isGoalState: Unknown goal"); break;
	}
}

void sampleInitial(size_t initialState, StateRec *x1) {
	bzero(x1,sizeof(StateRec));
	switch(initialState) {
	case 0: /*abc1*/
		x1->Sprinter_jammed = 1;
		x1->Shands_free_bob = 1;
		x1->Shands_free_alice = 1;
		x1->Sat_outside_bob = 1;
		x1->Sat_outside_alice = 1;
		x1->Sprinted_a = 0;
		x1->Sprinted_b = 0;
		x1->Sprinted_c = 0;
		x1->Shas_water_coffee_machine = 0;
		x1->Shas_paper_printer = 0;
		x1->Shas_ground_coffee_coffee_machine = 0;
		x1->Sat_door_alice = 0;
		x1->Sat_door_bob = 0;
		x1->Sat_printer_alice = 0;
		x1->Sat_printer_bob = 0;
		x1->Sat_paper_stack_alice = 0;
		x1->Sat_paper_stack_bob = 0;
		x1->Sat_coffee_machine_alice = 0;
		x1->Sat_coffee_machine_bob = 0;
		x1->Sat_water_tap_alice = 0;
		x1->Sat_water_tap_bob = 0;
		x1->Sat_coffee_jar_alice = 0;
		x1->Sat_coffee_jar_bob = 0;
		x1->Sholds_water_alice = 0;
		x1->Sholds_water_bob = 0;
		x1->Sholds_paper_alice = 0;
		x1->Sholds_paper_bob = 0;
		x1->Sholds_ground_coffee_alice = 0;
		x1->Sholds_ground_coffee_bob = 0;
		x1->Sholds_coffee_alice = 0;
		x1->Sholds_coffee_bob = 0;
		break;
	default: throw std::runtime_error("sampleInitial: Unknown initial state"); break;
	}
}

void stateObservation(StatePtr x) {
	if(x->Sat_door_bob || x->Sat_door_alice) { signalExpectedFromSensor(0); }
	if(x->Sat_printer_bob || x->Sat_printer_alice) { signalExpectedFromSensor(1); }
	if(x->Sat_paper_stack_bob || x->Sat_paper_stack_alice) { signalExpectedFromSensor(3); }
	if(x->Sat_coffee_machine_bob || x->Sat_coffee_machine_alice) { signalExpectedFromSensor(2); }
	if(x->Sat_water_tap_bob || x->Sat_water_tap_alice) { signalExpectedFromSensor(4); }
	if(x->Sat_coffee_jar_bob || x->Sat_coffee_jar_alice) { signalExpectedFromSensor(5); }
	if(x->Sat_door_alice) { setAgentLoc(0,0); }
	if(x->Sat_door_bob) { setAgentLoc(1,0); }
	if(x->Sat_printer_alice) { setAgentLoc(0,1); }
	if(x->Sat_printer_bob) { setAgentLoc(1,1); }
	if(x->Sat_paper_stack_alice) { setAgentLoc(0,3); }
	if(x->Sat_paper_stack_bob) { setAgentLoc(1,3); }
	if(x->Sat_coffee_machine_alice) { setAgentLoc(0,2); }
	if(x->Sat_coffee_machine_bob) { setAgentLoc(1,2); }
	if(x->Sat_water_tap_alice) { setAgentLoc(0,4); }
	if(x->Sat_water_tap_bob) { setAgentLoc(1,4); }
	if(x->Sat_coffee_jar_alice) { setAgentLoc(0,5); }
	if(x->Sat_coffee_jar_bob) { setAgentLoc(1,5); }
	if(x->Sat_outside_alice) { setAgentLoc(0,6); }
	if(x->Sat_outside_bob) { setAgentLoc(1,6); }
}

int landmarkCountHeuristic(size_t initialState, size_t goal, StatePtr s, StateRec *parentAccepted) {
	(void) s;
	StateRec accepted;
	int count = 0;
	switch(initialState) {
	case 0: /*abc1*/
		switch(goal) {
		case 0: /*abc1*/
			if (parentAccepted == 0) {
				//accepted = StateRec();accepted.S1=1;
				accepted.Shands_free_bob=1;
				accepted.Shands_free_alice=1;
				accepted.Sat_outside_alice=1;
				accepted.Sat_outside_bob=1;
			} else {
				accepted= *parentAccepted;
		
				if (s->Shands_free_bob==1) accepted.Shands_free_bob=1;
				if (s->Sat_coffee_machine_bob==1) accepted.Sat_coffee_machine_bob=1;
				if (s->Shands_free_alice==1) accepted.Shands_free_alice=1;
				if (s->Shas_ground_coffee_coffee_machine==1) accepted.Shas_ground_coffee_coffee_machine=1;
				if (s->Shas_water_coffee_machine==1) accepted.Shas_water_coffee_machine=1;
				if (s->Sat_coffee_machine_alice==1) accepted.Sat_coffee_machine_alice=1;
				if (s->Shas_paper_printer==1) accepted.Shas_paper_printer=1;
				if (s->Sprinted_a==1) if (parentAccepted->Shas_paper_printer==1)  accepted.Sprinted_a=1;
				if (s->Sprinted_b==1) if (parentAccepted->Shas_paper_printer==1)  accepted.Sprinted_b=1;
				if (s->Sprinted_c==1) if (parentAccepted->Shas_paper_printer==1)  accepted.Sprinted_c=1;
				if (s->Sholds_coffee_alice==1) if (parentAccepted->Sat_coffee_machine_alice==1)  if (parentAccepted->Shas_water_coffee_machine==1)  if (parentAccepted->Shas_ground_coffee_coffee_machine==1)  if (parentAccepted->Shands_free_alice==1)  accepted.Sholds_coffee_alice=1;
				if (s->Sholds_coffee_bob==1) if (parentAccepted->Sat_coffee_machine_bob==1)  if (parentAccepted->Shas_water_coffee_machine==1)  if (parentAccepted->Shas_ground_coffee_coffee_machine==1)  if (parentAccepted->Shands_free_bob==1)  accepted.Sholds_coffee_bob=1;
				if (s->Sat_outside_alice==1) accepted.Sat_outside_alice=1;
				if (s->Sat_outside_bob==1) accepted.Sat_outside_bob=1;
			} // compute the heuristic
			if (accepted.Shands_free_bob== 0) count++;
			if (accepted.Sat_coffee_machine_bob== 0) count++;
			if (accepted.Shands_free_alice== 0) count++;
			if (accepted.Shas_ground_coffee_coffee_machine== 0) count++;
			if (accepted.Shas_water_coffee_machine== 0) count++;
			if (accepted.Sat_coffee_machine_alice== 0) count++;
			if (accepted.Shas_paper_printer== 0) count++;
			if (accepted.Sprinted_a== 0) count++;
			if (accepted.Sprinted_b== 0) count++;
			if (accepted.Sprinted_c== 0) count++;
			if (accepted.Sholds_coffee_alice== 0) count++;
			if (accepted.Sholds_coffee_bob== 0) count++;
			if (accepted.Sat_outside_alice== 0) count++;
			if (accepted.Sat_outside_bob== 0) count++;
				if (accepted.Sprinted_a== 1 && s->Sprinted_a== 0) count++;
				if (accepted.Sprinted_b== 1 && s->Sprinted_b== 0) count++;
				if (accepted.Sprinted_c== 1 && s->Sprinted_c== 0) count++;
				if (accepted.Sholds_coffee_alice== 1 && s->Sholds_coffee_alice== 0) count++;
				if (accepted.Sholds_coffee_bob== 1 && s->Sholds_coffee_bob== 0) count++;
				if (accepted.Sat_outside_alice== 1 && s->Sat_outside_alice== 0) count++;
				if (accepted.Sat_outside_bob== 1 && s->Sat_outside_bob== 0) count++;
			if (accepted.Shands_free_bob== 1 && s->Shands_free_bob== 0  && (accepted.Sholds_coffee_bob==0)) count++;
			if (accepted.Sat_coffee_machine_bob== 1 && s->Sat_coffee_machine_bob== 0  && (accepted.Sholds_coffee_bob==0)) count++;
			if (accepted.Shands_free_alice== 1 && s->Shands_free_alice== 0  && (accepted.Sholds_coffee_alice==0)) count++;
			if (accepted.Shas_ground_coffee_coffee_machine== 1 && s->Shas_ground_coffee_coffee_machine== 0  && (accepted.Sholds_coffee_alice==0 || accepted.Sholds_coffee_bob==0)) count++;
			if (accepted.Shas_water_coffee_machine== 1 && s->Shas_water_coffee_machine== 0  && (accepted.Sholds_coffee_alice==0 || accepted.Sholds_coffee_bob==0)) count++;
			if (accepted.Sat_coffee_machine_alice== 1 && s->Sat_coffee_machine_alice== 0  && (accepted.Sholds_coffee_alice==0)) count++;
			if (accepted.Shas_paper_printer== 1 && s->Shas_paper_printer== 0  && (accepted.Sprinted_a==0 || accepted.Sprinted_b==0 || accepted.Sprinted_c==0)) count++;
			break;
		default: throw std::runtime_error("landmarkCountHeuristic: Unknown goal state");
		}
		break;
	default: throw std::runtime_error("landmarkCountHeuristic: Unknown initial state");
	}
	*parentAccepted = accepted;
	return count;
}
