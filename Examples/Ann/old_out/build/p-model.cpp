const char * DOMAIN_NAME = "carrots";
const char * PROBLEM_NAME = "carrots2";

char const * const objnames_object[] = {"plate","cutting_board","sink","counter","carrot","knife","wooden_spoon","spoon","sponge","cupboard"};
char const * const objnames_place[] = {"plate","cutting_board","sink","cupboard"};
char const * const objnames_takeable[] = {"plate","cutting_board","carrot","knife","wooden_spoon","spoon","sponge"};
char const * const objnames_container[] = {"plate","cutting_board"};
char const * const objnames_fixedplace[] = {"sink","cupboard"};
char const * const objnames_tool[] = {"carrot","knife","wooden_spoon","spoon","sponge"};
char const * const objnames_location[] = {"sink","counter"};

std::ostream& operator<<(std::ostream &o, StatePtr x) {
	if(x->Sreachable_sink_plate) o << " (reachable sink plate)"; else o<< " (not (reachable sink plate))"; 
	if(x->Sreachable_sink_cutting_board) o << " (reachable sink cutting_board)"; else o<< " (not (reachable sink cutting_board))"; 
	if(x->Sreachable_sink_sink) o << " (reachable sink sink)"; else o<< " (not (reachable sink sink))"; 
	if(x->Sreachable_counter_plate) o << " (reachable counter plate)"; else o<< " (not (reachable counter plate))"; 
	if(x->Sreachable_counter_cutting_board) o << " (reachable counter cutting_board)"; else o<< " (not (reachable counter cutting_board))"; 
	if(x->Sreachable_counter_counter) o << " (reachable counter counter)"; else o<< " (not (reachable counter counter))"; 
	if(x->Sreachable_counter_cupboard) o << " (reachable counter cupboard)"; else o<< " (not (reachable counter cupboard))"; 
	if(x->Sreachable_counter_stove) o << " (reachable counter stove)"; else o<< " (not (reachable counter stove))"; 
	if(x->Sreachable_table_table) o << " (reachable table table)"; else o<< " (not (reachable table table))"; 
	if(x->Sisat_sink) o << " (isat sink)"; else o<< " (not (isat sink))"; 
	if(x->Sisat_counter) o << " (isat counter)"; else o<< " (not (isat counter))"; 
	if(x->Sat_plate_sink) o << " (at plate sink)"; else o<< " (not (at plate sink))"; 
	if(x->Sat_plate_cupboard) o << " (at plate cupboard)"; else o<< " (not (at plate cupboard))"; 
	if(x->Sat_cutting_board_sink) o << " (at cutting_board sink)"; else o<< " (not (at cutting_board sink))"; 
	if(x->Sat_cutting_board_cupboard) o << " (at cutting_board cupboard)"; else o<< " (not (at cutting_board cupboard))"; 
	if(x->Sat_carrot_plate) o << " (at carrot plate)"; else o<< " (not (at carrot plate))"; 
	if(x->Sat_carrot_cutting_board) o << " (at carrot cutting_board)"; else o<< " (not (at carrot cutting_board))"; 
	if(x->Sat_carrot_sink) o << " (at carrot sink)"; else o<< " (not (at carrot sink))"; 
	if(x->Sat_carrot_cupboard) o << " (at carrot cupboard)"; else o<< " (not (at carrot cupboard))"; 
	if(x->Sat_knife_plate) o << " (at knife plate)"; else o<< " (not (at knife plate))"; 
	if(x->Sat_knife_cutting_board) o << " (at knife cutting_board)"; else o<< " (not (at knife cutting_board))"; 
	if(x->Sat_knife_sink) o << " (at knife sink)"; else o<< " (not (at knife sink))"; 
	if(x->Sat_knife_cupboard) o << " (at knife cupboard)"; else o<< " (not (at knife cupboard))"; 
	if(x->Sat_wooden_spoon_plate) o << " (at wooden_spoon plate)"; else o<< " (not (at wooden_spoon plate))"; 
	if(x->Sat_wooden_spoon_cutting_board) o << " (at wooden_spoon cutting_board)"; else o<< " (not (at wooden_spoon cutting_board))"; 
	if(x->Sat_wooden_spoon_sink) o << " (at wooden_spoon sink)"; else o<< " (not (at wooden_spoon sink))"; 
	if(x->Sat_wooden_spoon_cupboard) o << " (at wooden_spoon cupboard)"; else o<< " (not (at wooden_spoon cupboard))"; 
	if(x->Sat_spoon_plate) o << " (at spoon plate)"; else o<< " (not (at spoon plate))"; 
	if(x->Sat_spoon_cutting_board) o << " (at spoon cutting_board)"; else o<< " (not (at spoon cutting_board))"; 
	if(x->Sat_spoon_sink) o << " (at spoon sink)"; else o<< " (not (at spoon sink))"; 
	if(x->Sat_spoon_cupboard) o << " (at spoon cupboard)"; else o<< " (not (at spoon cupboard))"; 
	if(x->Sat_sponge_plate) o << " (at sponge plate)"; else o<< " (not (at sponge plate))"; 
	if(x->Sat_sponge_cutting_board) o << " (at sponge cutting_board)"; else o<< " (not (at sponge cutting_board))"; 
	if(x->Sat_sponge_sink) o << " (at sponge sink)"; else o<< " (not (at sponge sink))"; 
	if(x->Sat_sponge_cupboard) o << " (at sponge cupboard)"; else o<< " (not (at sponge cupboard))"; 
	if(x->Staken_plate) o << " (taken plate)"; else o<< " (not (taken plate))"; 
	if(x->Staken_cutting_board) o << " (taken cutting_board)"; else o<< " (not (taken cutting_board))"; 
	if(x->Staken_carrot) o << " (taken carrot)"; else o<< " (not (taken carrot))"; 
	if(x->Staken_knife) o << " (taken knife)"; else o<< " (not (taken knife))"; 
	if(x->Staken_wooden_spoon) o << " (taken wooden_spoon)"; else o<< " (not (taken wooden_spoon))"; 
	if(x->Staken_spoon) o << " (taken spoon)"; else o<< " (not (taken spoon))"; 
	if(x->Staken_sponge) o << " (taken sponge)"; else o<< " (not (taken sponge))"; 
	if(x->Shandsfree) o << " handsfree"; else o<< " (not handsfree)"; 
	return o;
}
void writeState(std::ostream &o, StatePtr x, double wt) {
	char buf[16 + 3*0 + 0]; char *s = (char*) buf;
	if (std::isinf(wt)) strcpy(s, "inf"), s += 3; else s += sprintf(s, "%g", wt);
	*s = ' '; s++; *s =x->Sreachable_sink_plate + '0'; s++;
	*s = ' '; s++; *s =x->Sreachable_sink_cutting_board + '0'; s++;
	*s = ' '; s++; *s =x->Sreachable_sink_sink + '0'; s++;
	*s = ' '; s++; *s =x->Sreachable_counter_plate + '0'; s++;
	*s = ' '; s++; *s =x->Sreachable_counter_cutting_board + '0'; s++;
	*s = ' '; s++; *s =x->Sreachable_counter_counter + '0'; s++;
	*s = ' '; s++; *s =x->Sreachable_counter_cupboard + '0'; s++;
	*s = ' '; s++; *s =x->Sreachable_counter_stove + '0'; s++;
	*s = ' '; s++; *s =x->Sreachable_table_table + '0'; s++;
	*s = ' '; s++; *s =x->Sisat_sink + '0'; s++;
	*s = ' '; s++; *s =x->Sisat_counter + '0'; s++;
	*s = ' '; s++; *s =x->Sat_plate_sink + '0'; s++;
	*s = ' '; s++; *s =x->Sat_plate_cupboard + '0'; s++;
	*s = ' '; s++; *s =x->Sat_cutting_board_sink + '0'; s++;
	*s = ' '; s++; *s =x->Sat_cutting_board_cupboard + '0'; s++;
	*s = ' '; s++; *s =x->Sat_carrot_plate + '0'; s++;
	*s = ' '; s++; *s =x->Sat_carrot_cutting_board + '0'; s++;
	*s = ' '; s++; *s =x->Sat_carrot_sink + '0'; s++;
	*s = ' '; s++; *s =x->Sat_carrot_cupboard + '0'; s++;
	*s = ' '; s++; *s =x->Sat_knife_plate + '0'; s++;
	*s = ' '; s++; *s =x->Sat_knife_cutting_board + '0'; s++;
	*s = ' '; s++; *s =x->Sat_knife_sink + '0'; s++;
	*s = ' '; s++; *s =x->Sat_knife_cupboard + '0'; s++;
	*s = ' '; s++; *s =x->Sat_wooden_spoon_plate + '0'; s++;
	*s = ' '; s++; *s =x->Sat_wooden_spoon_cutting_board + '0'; s++;
	*s = ' '; s++; *s =x->Sat_wooden_spoon_sink + '0'; s++;
	*s = ' '; s++; *s =x->Sat_wooden_spoon_cupboard + '0'; s++;
	*s = ' '; s++; *s =x->Sat_spoon_plate + '0'; s++;
	*s = ' '; s++; *s =x->Sat_spoon_cutting_board + '0'; s++;
	*s = ' '; s++; *s =x->Sat_spoon_sink + '0'; s++;
	*s = ' '; s++; *s =x->Sat_spoon_cupboard + '0'; s++;
	*s = ' '; s++; *s =x->Sat_sponge_plate + '0'; s++;
	*s = ' '; s++; *s =x->Sat_sponge_cutting_board + '0'; s++;
	*s = ' '; s++; *s =x->Sat_sponge_sink + '0'; s++;
	*s = ' '; s++; *s =x->Sat_sponge_cupboard + '0'; s++;
	*s = ' '; s++; *s =x->Staken_plate + '0'; s++;
	*s = ' '; s++; *s =x->Staken_cutting_board + '0'; s++;
	*s = ' '; s++; *s =x->Staken_carrot + '0'; s++;
	*s = ' '; s++; *s =x->Staken_knife + '0'; s++;
	*s = ' '; s++; *s =x->Staken_wooden_spoon + '0'; s++;
	*s = ' '; s++; *s =x->Staken_spoon + '0'; s++;
	*s = ' '; s++; *s =x->Staken_sponge + '0'; s++;
	*s = ' '; s++; *s =x->Shandsfree + '0'; s++;
	*s = '\n'; *(++s) = '\0'; o << (char*) buf;
}
bool readState(std::istream &i, StateRec *x, double &wt) {
	char buf[4096]; char *s = (char*) buf;
	do i.getline(s, 4096); while (i.gcount() < 2 && i.good());
	if (!i.good()) return false;
	if (!memcmp(s, "inf", 3)) { wt = infinity, s+=3; } else wt = strtod(s, &s);
	char c;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sreachable_sink_plate = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sreachable_sink_cutting_board = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sreachable_sink_sink = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sreachable_counter_plate = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sreachable_counter_cutting_board = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sreachable_counter_counter = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sreachable_counter_cupboard = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sreachable_counter_stove = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sreachable_table_table = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sisat_sink = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sisat_counter = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_plate_sink = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_plate_cupboard = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_cutting_board_sink = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_cutting_board_cupboard = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_carrot_plate = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_carrot_cutting_board = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_carrot_sink = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_carrot_cupboard = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_knife_plate = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_knife_cutting_board = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_knife_sink = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_knife_cupboard = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_wooden_spoon_plate = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_wooden_spoon_cutting_board = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_wooden_spoon_sink = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_wooden_spoon_cupboard = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_spoon_plate = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_spoon_cutting_board = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_spoon_sink = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_spoon_cupboard = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_sponge_plate = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_sponge_cutting_board = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_sponge_sink = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sat_sponge_cupboard = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Staken_plate = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Staken_cutting_board = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Staken_carrot = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Staken_knife = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Staken_wooden_spoon = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Staken_spoon = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Staken_sponge = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Shandsfree = c, s++; else return false;
	return true;
}
void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr) { // rt: Recency Vector (ACT-R Heuristic); t: ist the global time
	if (prev->Sreachable_sink_plate ^ curr->Sreachable_sink_plate) rT[0] = t;
	if (prev->Sreachable_sink_cutting_board ^ curr->Sreachable_sink_cutting_board) rT[1] = t;
	if (prev->Sreachable_sink_sink ^ curr->Sreachable_sink_sink) rT[2] = t;
	if (prev->Sreachable_counter_plate ^ curr->Sreachable_counter_plate) rT[3] = t;
	if (prev->Sreachable_counter_cutting_board ^ curr->Sreachable_counter_cutting_board) rT[4] = t;
	if (prev->Sreachable_counter_counter ^ curr->Sreachable_counter_counter) rT[5] = t;
	if (prev->Sreachable_counter_cupboard ^ curr->Sreachable_counter_cupboard) rT[6] = t;
	if (prev->Sreachable_counter_stove ^ curr->Sreachable_counter_stove) rT[7] = t;
	if (prev->Sreachable_table_table ^ curr->Sreachable_table_table) rT[8] = t;
	if (prev->Sisat_sink ^ curr->Sisat_sink) rT[9] = t;
	if (prev->Sisat_counter ^ curr->Sisat_counter) rT[10] = t;
	if (prev->Sat_plate_sink ^ curr->Sat_plate_sink) rT[11] = t;
	if (prev->Sat_plate_cupboard ^ curr->Sat_plate_cupboard) rT[12] = t;
	if (prev->Sat_cutting_board_sink ^ curr->Sat_cutting_board_sink) rT[13] = t;
	if (prev->Sat_cutting_board_cupboard ^ curr->Sat_cutting_board_cupboard) rT[14] = t;
	if (prev->Sat_carrot_plate ^ curr->Sat_carrot_plate) rT[15] = t;
	if (prev->Sat_carrot_cutting_board ^ curr->Sat_carrot_cutting_board) rT[16] = t;
	if (prev->Sat_carrot_sink ^ curr->Sat_carrot_sink) rT[17] = t;
	if (prev->Sat_carrot_cupboard ^ curr->Sat_carrot_cupboard) rT[18] = t;
	if (prev->Sat_knife_plate ^ curr->Sat_knife_plate) rT[19] = t;
	if (prev->Sat_knife_cutting_board ^ curr->Sat_knife_cutting_board) rT[20] = t;
	if (prev->Sat_knife_sink ^ curr->Sat_knife_sink) rT[21] = t;
	if (prev->Sat_knife_cupboard ^ curr->Sat_knife_cupboard) rT[22] = t;
	if (prev->Sat_wooden_spoon_plate ^ curr->Sat_wooden_spoon_plate) rT[23] = t;
	if (prev->Sat_wooden_spoon_cutting_board ^ curr->Sat_wooden_spoon_cutting_board) rT[24] = t;
	if (prev->Sat_wooden_spoon_sink ^ curr->Sat_wooden_spoon_sink) rT[25] = t;
	if (prev->Sat_wooden_spoon_cupboard ^ curr->Sat_wooden_spoon_cupboard) rT[26] = t;
	if (prev->Sat_spoon_plate ^ curr->Sat_spoon_plate) rT[27] = t;
	if (prev->Sat_spoon_cutting_board ^ curr->Sat_spoon_cutting_board) rT[28] = t;
	if (prev->Sat_spoon_sink ^ curr->Sat_spoon_sink) rT[29] = t;
	if (prev->Sat_spoon_cupboard ^ curr->Sat_spoon_cupboard) rT[30] = t;
	if (prev->Sat_sponge_plate ^ curr->Sat_sponge_plate) rT[31] = t;
	if (prev->Sat_sponge_cutting_board ^ curr->Sat_sponge_cutting_board) rT[32] = t;
	if (prev->Sat_sponge_sink ^ curr->Sat_sponge_sink) rT[33] = t;
	if (prev->Sat_sponge_cupboard ^ curr->Sat_sponge_cupboard) rT[34] = t;
	if (prev->Staken_plate ^ curr->Staken_plate) rT[35] = t;
	if (prev->Staken_cutting_board ^ curr->Staken_cutting_board) rT[36] = t;
	if (prev->Staken_carrot ^ curr->Staken_carrot) rT[37] = t;
	if (prev->Staken_knife ^ curr->Staken_knife) rT[38] = t;
	if (prev->Staken_wooden_spoon ^ curr->Staken_wooden_spoon) rT[39] = t;
	if (prev->Staken_spoon ^ curr->Staken_spoon) rT[40] = t;
	if (prev->Staken_sponge ^ curr->Staken_sponge) rT[41] = t;
	if (prev->Shandsfree ^ curr->Shandsfree) rT[42] = t;
}
double getRefract(StatePtr a1, StatePtr a2) { // compare two action preconditions and returns the rerfractoriness 
	double r=0; int n=0;
	if (a1->Sreachable_sink_plate) {n++; if (a2->Sreachable_sink_plate) r++;}
	if (a1->Sreachable_sink_cutting_board) {n++; if (a2->Sreachable_sink_cutting_board) r++;}
	if (a1->Sreachable_sink_sink) {n++; if (a2->Sreachable_sink_sink) r++;}
	if (a1->Sreachable_counter_plate) {n++; if (a2->Sreachable_counter_plate) r++;}
	if (a1->Sreachable_counter_cutting_board) {n++; if (a2->Sreachable_counter_cutting_board) r++;}
	if (a1->Sreachable_counter_counter) {n++; if (a2->Sreachable_counter_counter) r++;}
	if (a1->Sreachable_counter_cupboard) {n++; if (a2->Sreachable_counter_cupboard) r++;}
	if (a1->Sreachable_counter_stove) {n++; if (a2->Sreachable_counter_stove) r++;}
	if (a1->Sreachable_table_table) {n++; if (a2->Sreachable_table_table) r++;}
	if (a1->Sisat_sink) {n++; if (a2->Sisat_sink) r++;}
	if (a1->Sisat_counter) {n++; if (a2->Sisat_counter) r++;}
	if (a1->Sat_plate_sink) {n++; if (a2->Sat_plate_sink) r++;}
	if (a1->Sat_plate_cupboard) {n++; if (a2->Sat_plate_cupboard) r++;}
	if (a1->Sat_cutting_board_sink) {n++; if (a2->Sat_cutting_board_sink) r++;}
	if (a1->Sat_cutting_board_cupboard) {n++; if (a2->Sat_cutting_board_cupboard) r++;}
	if (a1->Sat_carrot_plate) {n++; if (a2->Sat_carrot_plate) r++;}
	if (a1->Sat_carrot_cutting_board) {n++; if (a2->Sat_carrot_cutting_board) r++;}
	if (a1->Sat_carrot_sink) {n++; if (a2->Sat_carrot_sink) r++;}
	if (a1->Sat_carrot_cupboard) {n++; if (a2->Sat_carrot_cupboard) r++;}
	if (a1->Sat_knife_plate) {n++; if (a2->Sat_knife_plate) r++;}
	if (a1->Sat_knife_cutting_board) {n++; if (a2->Sat_knife_cutting_board) r++;}
	if (a1->Sat_knife_sink) {n++; if (a2->Sat_knife_sink) r++;}
	if (a1->Sat_knife_cupboard) {n++; if (a2->Sat_knife_cupboard) r++;}
	if (a1->Sat_wooden_spoon_plate) {n++; if (a2->Sat_wooden_spoon_plate) r++;}
	if (a1->Sat_wooden_spoon_cutting_board) {n++; if (a2->Sat_wooden_spoon_cutting_board) r++;}
	if (a1->Sat_wooden_spoon_sink) {n++; if (a2->Sat_wooden_spoon_sink) r++;}
	if (a1->Sat_wooden_spoon_cupboard) {n++; if (a2->Sat_wooden_spoon_cupboard) r++;}
	if (a1->Sat_spoon_plate) {n++; if (a2->Sat_spoon_plate) r++;}
	if (a1->Sat_spoon_cutting_board) {n++; if (a2->Sat_spoon_cutting_board) r++;}
	if (a1->Sat_spoon_sink) {n++; if (a2->Sat_spoon_sink) r++;}
	if (a1->Sat_spoon_cupboard) {n++; if (a2->Sat_spoon_cupboard) r++;}
	if (a1->Sat_sponge_plate) {n++; if (a2->Sat_sponge_plate) r++;}
	if (a1->Sat_sponge_cutting_board) {n++; if (a2->Sat_sponge_cutting_board) r++;}
	if (a1->Sat_sponge_sink) {n++; if (a2->Sat_sponge_sink) r++;}
	if (a1->Sat_sponge_cupboard) {n++; if (a2->Sat_sponge_cupboard) r++;}
	if (a1->Staken_plate) {n++; if (a2->Staken_plate) r++;}
	if (a1->Staken_cutting_board) {n++; if (a2->Staken_cutting_board) r++;}
	if (a1->Staken_carrot) {n++; if (a2->Staken_carrot) r++;}
	if (a1->Staken_knife) {n++; if (a2->Staken_knife) r++;}
	if (a1->Staken_wooden_spoon) {n++; if (a2->Staken_wooden_spoon) r++;}
	if (a1->Staken_spoon) {n++; if (a2->Staken_spoon) r++;}
	if (a1->Staken_sponge) {n++; if (a2->Staken_sponge) r++;}
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
	"(take plate sink)", // (:action take ?c - container ?from - fixedplace)
	"(take plate cupboard)", // (:action take ?c - container ?from - fixedplace)
	"(take cutting_board sink)", // (:action take ?c - container ?from - fixedplace)
	"(take cutting_board cupboard)", // (:action take ?c - container ?from - fixedplace)
	"(take carrot plate)", // (:action take ?t - tool ?from - place)
	"(take carrot cutting_board)", // (:action take ?t - tool ?from - place)
	"(take carrot sink)", // (:action take ?t - tool ?from - place)
	"(take carrot cupboard)", // (:action take ?t - tool ?from - place)
	"(take knife plate)", // (:action take ?t - tool ?from - place)
	"(take knife cutting_board)", // (:action take ?t - tool ?from - place)
	"(take knife sink)", // (:action take ?t - tool ?from - place)
	"(take knife cupboard)", // (:action take ?t - tool ?from - place)
	"(take wooden_spoon plate)", // (:action take ?t - tool ?from - place)
	"(take wooden_spoon cutting_board)", // (:action take ?t - tool ?from - place)
	"(take wooden_spoon sink)", // (:action take ?t - tool ?from - place)
	"(take wooden_spoon cupboard)", // (:action take ?t - tool ?from - place)
	"(take spoon plate)", // (:action take ?t - tool ?from - place)
	"(take spoon cutting_board)", // (:action take ?t - tool ?from - place)
	"(take spoon sink)", // (:action take ?t - tool ?from - place)
	"(take spoon cupboard)", // (:action take ?t - tool ?from - place)
	"(take sponge plate)", // (:action take ?t - tool ?from - place)
	"(take sponge cutting_board)", // (:action take ?t - tool ?from - place)
	"(take sponge sink)", // (:action take ?t - tool ?from - place)
	"(take sponge cupboard)", // (:action take ?t - tool ?from - place)
	"(put plate sink)", // (:action put ?c - container ?to - fixedplace)
	"(put plate cupboard)", // (:action put ?c - container ?to - fixedplace)
	"(put cutting_board sink)", // (:action put ?c - container ?to - fixedplace)
	"(put cutting_board cupboard)", // (:action put ?c - container ?to - fixedplace)
	"(put carrot plate)", // (:action put ?t - tool ?to - place)
	"(put carrot cutting_board)", // (:action put ?t - tool ?to - place)
	"(put carrot sink)", // (:action put ?t - tool ?to - place)
	"(put carrot cupboard)", // (:action put ?t - tool ?to - place)
	"(put knife plate)", // (:action put ?t - tool ?to - place)
	"(put knife cutting_board)", // (:action put ?t - tool ?to - place)
	"(put knife sink)", // (:action put ?t - tool ?to - place)
	"(put knife cupboard)", // (:action put ?t - tool ?to - place)
	"(put wooden_spoon plate)", // (:action put ?t - tool ?to - place)
	"(put wooden_spoon cutting_board)", // (:action put ?t - tool ?to - place)
	"(put wooden_spoon sink)", // (:action put ?t - tool ?to - place)
	"(put wooden_spoon cupboard)", // (:action put ?t - tool ?to - place)
	"(put spoon plate)", // (:action put ?t - tool ?to - place)
	"(put spoon cutting_board)", // (:action put ?t - tool ?to - place)
	"(put spoon sink)", // (:action put ?t - tool ?to - place)
	"(put spoon cupboard)", // (:action put ?t - tool ?to - place)
	"(put sponge plate)", // (:action put ?t - tool ?to - place)
	"(put sponge cutting_board)", // (:action put ?t - tool ?to - place)
	"(put sponge sink)", // (:action put ?t - tool ?to - place)
	"(put sponge cupboard)" // (:action put ?t - tool ?to - place)
};
char const * const * const actionNames = &__actionNames[3];


/* (move sink counter), id 0 */
double action_move_sink_counter(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sisat_sink) *specificity=0.01; 

	if(x->Sisat_sink) {
		*x1 = *x;
		x1->Sisat_sink = 0;
		x1->Sisat_counter = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (move counter sink), id 1 */
double action_move_counter_sink(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sisat_counter) *specificity=0.01; 

	if(x->Sisat_counter) {
		*x1 = *x;
		x1->Sisat_counter = 0;
		x1->Sisat_sink = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take plate sink), id 2 */
double action_take_plate_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_plate) && x->Sat_plate_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.49; 

	if((!(x->Staken_plate) && x->Sat_plate_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.49; 

	if((!(x->Staken_plate) && x->Sat_plate_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.49; 

	if((!(x->Staken_plate) && x->Sat_plate_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.49; 

	if((!(x->Staken_plate) && x->Sat_plate_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate) || (!(x->Staken_plate) && x->Sat_plate_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_plate) && x->Sat_plate_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_plate) && x->Sat_plate_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) {
		*x1 = *x;
		x1->Sreachable_counter_plate = 0;
		x1->Sreachable_sink_plate = 0;
		x1->Sat_plate_sink = 0;
		x1->Shandsfree = 0;
		x1->Staken_plate = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take plate cupboard), id 3 */
double action_take_plate_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_plate) && x->Sat_plate_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.49; 

	if((!(x->Staken_plate) && x->Sat_plate_cupboard && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.49; 

	if((!(x->Staken_plate) && x->Sat_plate_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.49; 

	if((!(x->Staken_plate) && x->Sat_plate_cupboard && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.49; 

	if((!(x->Staken_plate) && x->Sat_plate_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate) || (!(x->Staken_plate) && x->Sat_plate_cupboard && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_plate) && x->Sat_plate_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_plate) && x->Sat_plate_cupboard && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) {
		*x1 = *x;
		x1->Sreachable_counter_plate = 0;
		x1->Sreachable_sink_plate = 0;
		x1->Sat_plate_cupboard = 0;
		x1->Shandsfree = 0;
		x1->Staken_plate = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take cutting_board sink), id 4 */
double action_take_cutting_board_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_cutting_board) && x->Sat_cutting_board_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.49; 

	if((!(x->Staken_cutting_board) && x->Sat_cutting_board_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.49; 

	if((!(x->Staken_cutting_board) && x->Sat_cutting_board_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.49; 

	if((!(x->Staken_cutting_board) && x->Sat_cutting_board_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.49; 

	if((!(x->Staken_cutting_board) && x->Sat_cutting_board_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board) || (!(x->Staken_cutting_board) && x->Sat_cutting_board_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_cutting_board) && x->Sat_cutting_board_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_cutting_board) && x->Sat_cutting_board_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) {
		*x1 = *x;
		x1->Sreachable_counter_cutting_board = 0;
		x1->Sreachable_sink_cutting_board = 0;
		x1->Sat_cutting_board_sink = 0;
		x1->Shandsfree = 0;
		x1->Staken_cutting_board = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take cutting_board cupboard), id 5 */
double action_take_cutting_board_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_cutting_board) && x->Sat_cutting_board_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.49; 

	if((!(x->Staken_cutting_board) && x->Sat_cutting_board_cupboard && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.49; 

	if((!(x->Staken_cutting_board) && x->Sat_cutting_board_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.49; 

	if((!(x->Staken_cutting_board) && x->Sat_cutting_board_cupboard && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.49; 

	if((!(x->Staken_cutting_board) && x->Sat_cutting_board_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board) || (!(x->Staken_cutting_board) && x->Sat_cutting_board_cupboard && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_cutting_board) && x->Sat_cutting_board_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_cutting_board) && x->Sat_cutting_board_cupboard && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) {
		*x1 = *x;
		x1->Sreachable_counter_cutting_board = 0;
		x1->Sreachable_sink_cutting_board = 0;
		x1->Sat_cutting_board_cupboard = 0;
		x1->Shandsfree = 0;
		x1->Staken_cutting_board = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take carrot plate), id 6 */
double action_take_carrot_plate(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_carrot) && x->Sat_carrot_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.43; 

	if((!(x->Staken_carrot) && x->Sat_carrot_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.43; 

	if((!(x->Staken_carrot) && x->Sat_carrot_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.43; 

	if((!(x->Staken_carrot) && x->Sat_carrot_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.43; 

	if((!(x->Staken_carrot) && x->Sat_carrot_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate) || (!(x->Staken_carrot) && x->Sat_carrot_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_carrot) && x->Sat_carrot_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_carrot) && x->Sat_carrot_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_carrot_plate = 0;
		x1->Staken_carrot = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take carrot cutting_board), id 7 */
double action_take_carrot_cutting_board(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_carrot) && x->Sat_carrot_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.43; 

	if((!(x->Staken_carrot) && x->Sat_carrot_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.43; 

	if((!(x->Staken_carrot) && x->Sat_carrot_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.43; 

	if((!(x->Staken_carrot) && x->Sat_carrot_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.43; 

	if((!(x->Staken_carrot) && x->Sat_carrot_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board) || (!(x->Staken_carrot) && x->Sat_carrot_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_carrot) && x->Sat_carrot_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_carrot) && x->Sat_carrot_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_carrot_cutting_board = 0;
		x1->Staken_carrot = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take carrot sink), id 8 */
double action_take_carrot_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_carrot) && x->Sat_carrot_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter))) *specificity=0.39; 

	if((!(x->Staken_carrot) && x->Sat_carrot_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) *specificity=0.39; 

	if((!(x->Staken_carrot) && x->Sat_carrot_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter)) || (!(x->Staken_carrot) && x->Sat_carrot_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_carrot_sink = 0;
		x1->Staken_carrot = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take carrot cupboard), id 9 */
double action_take_carrot_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_carrot) && x->Sat_carrot_cupboard && x->Shandsfree && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard)) *specificity=0.39; 

	if((!(x->Staken_carrot) && x->Sat_carrot_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) *specificity=0.39; 

	if((!(x->Staken_carrot) && x->Sat_carrot_cupboard && x->Shandsfree && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard) || (!(x->Staken_carrot) && x->Sat_carrot_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_carrot_cupboard = 0;
		x1->Staken_carrot = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife plate), id 10 */
double action_take_knife_plate(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_knife) && x->Sat_knife_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.43; 

	if((!(x->Staken_knife) && x->Sat_knife_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.43; 

	if((!(x->Staken_knife) && x->Sat_knife_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.43; 

	if((!(x->Staken_knife) && x->Sat_knife_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.43; 

	if((!(x->Staken_knife) && x->Sat_knife_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate) || (!(x->Staken_knife) && x->Sat_knife_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_knife) && x->Sat_knife_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_knife) && x->Sat_knife_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_knife_plate = 0;
		x1->Staken_knife = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife cutting_board), id 11 */
double action_take_knife_cutting_board(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_knife) && x->Sat_knife_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.43; 

	if((!(x->Staken_knife) && x->Sat_knife_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.43; 

	if((!(x->Staken_knife) && x->Sat_knife_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.43; 

	if((!(x->Staken_knife) && x->Sat_knife_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.43; 

	if((!(x->Staken_knife) && x->Sat_knife_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board) || (!(x->Staken_knife) && x->Sat_knife_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_knife) && x->Sat_knife_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_knife) && x->Sat_knife_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_knife_cutting_board = 0;
		x1->Staken_knife = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife sink), id 12 */
double action_take_knife_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_knife) && x->Sat_knife_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter))) *specificity=0.39; 

	if((!(x->Staken_knife) && x->Sat_knife_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) *specificity=0.39; 

	if((!(x->Staken_knife) && x->Sat_knife_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter)) || (!(x->Staken_knife) && x->Sat_knife_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_knife_sink = 0;
		x1->Staken_knife = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take knife cupboard), id 13 */
double action_take_knife_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_knife) && x->Sat_knife_cupboard && x->Shandsfree && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard)) *specificity=0.39; 

	if((!(x->Staken_knife) && x->Sat_knife_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) *specificity=0.39; 

	if((!(x->Staken_knife) && x->Sat_knife_cupboard && x->Shandsfree && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard) || (!(x->Staken_knife) && x->Sat_knife_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_knife_cupboard = 0;
		x1->Staken_knife = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take wooden_spoon plate), id 14 */
double action_take_wooden_spoon_plate(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.43; 

	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.43; 

	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.43; 

	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.43; 

	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate) || (!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_wooden_spoon_plate = 0;
		x1->Staken_wooden_spoon = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take wooden_spoon cutting_board), id 15 */
double action_take_wooden_spoon_cutting_board(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.43; 

	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.43; 

	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.43; 

	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.43; 

	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board) || (!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_wooden_spoon_cutting_board = 0;
		x1->Staken_wooden_spoon = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take wooden_spoon sink), id 16 */
double action_take_wooden_spoon_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter))) *specificity=0.39; 

	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) *specificity=0.39; 

	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter)) || (!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_wooden_spoon_sink = 0;
		x1->Staken_wooden_spoon = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take wooden_spoon cupboard), id 17 */
double action_take_wooden_spoon_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_cupboard && x->Shandsfree && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard)) *specificity=0.39; 

	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) *specificity=0.39; 

	if((!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_cupboard && x->Shandsfree && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard) || (!(x->Staken_wooden_spoon) && x->Sat_wooden_spoon_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_wooden_spoon_cupboard = 0;
		x1->Staken_wooden_spoon = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take spoon plate), id 18 */
double action_take_spoon_plate(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_spoon) && x->Sat_spoon_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.43; 

	if((!(x->Staken_spoon) && x->Sat_spoon_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.43; 

	if((!(x->Staken_spoon) && x->Sat_spoon_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.43; 

	if((!(x->Staken_spoon) && x->Sat_spoon_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.43; 

	if((!(x->Staken_spoon) && x->Sat_spoon_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate) || (!(x->Staken_spoon) && x->Sat_spoon_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_spoon) && x->Sat_spoon_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_spoon) && x->Sat_spoon_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_spoon_plate = 0;
		x1->Staken_spoon = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take spoon cutting_board), id 19 */
double action_take_spoon_cutting_board(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_spoon) && x->Sat_spoon_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.43; 

	if((!(x->Staken_spoon) && x->Sat_spoon_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.43; 

	if((!(x->Staken_spoon) && x->Sat_spoon_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.43; 

	if((!(x->Staken_spoon) && x->Sat_spoon_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.43; 

	if((!(x->Staken_spoon) && x->Sat_spoon_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board) || (!(x->Staken_spoon) && x->Sat_spoon_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_spoon) && x->Sat_spoon_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_spoon) && x->Sat_spoon_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_spoon_cutting_board = 0;
		x1->Staken_spoon = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take spoon sink), id 20 */
double action_take_spoon_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_spoon) && x->Sat_spoon_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter))) *specificity=0.39; 

	if((!(x->Staken_spoon) && x->Sat_spoon_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) *specificity=0.39; 

	if((!(x->Staken_spoon) && x->Sat_spoon_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter)) || (!(x->Staken_spoon) && x->Sat_spoon_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_spoon_sink = 0;
		x1->Staken_spoon = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take spoon cupboard), id 21 */
double action_take_spoon_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_spoon) && x->Sat_spoon_cupboard && x->Shandsfree && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard)) *specificity=0.39; 

	if((!(x->Staken_spoon) && x->Sat_spoon_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) *specificity=0.39; 

	if((!(x->Staken_spoon) && x->Sat_spoon_cupboard && x->Shandsfree && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard) || (!(x->Staken_spoon) && x->Sat_spoon_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_spoon_cupboard = 0;
		x1->Staken_spoon = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take sponge plate), id 22 */
double action_take_sponge_plate(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_sponge) && x->Sat_sponge_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.43; 

	if((!(x->Staken_sponge) && x->Sat_sponge_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.43; 

	if((!(x->Staken_sponge) && x->Sat_sponge_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.43; 

	if((!(x->Staken_sponge) && x->Sat_sponge_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.43; 

	if((!(x->Staken_sponge) && x->Sat_sponge_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate) || (!(x->Staken_sponge) && x->Sat_sponge_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_sponge) && x->Sat_sponge_plate && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (!(x->Staken_sponge) && x->Sat_sponge_plate && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_sponge_plate = 0;
		x1->Staken_sponge = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take sponge cutting_board), id 23 */
double action_take_sponge_cutting_board(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_sponge) && x->Sat_sponge_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.43; 

	if((!(x->Staken_sponge) && x->Sat_sponge_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.43; 

	if((!(x->Staken_sponge) && x->Sat_sponge_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.43; 

	if((!(x->Staken_sponge) && x->Sat_sponge_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.43; 

	if((!(x->Staken_sponge) && x->Sat_sponge_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board) || (!(x->Staken_sponge) && x->Sat_sponge_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_sponge) && x->Sat_sponge_cutting_board && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (!(x->Staken_sponge) && x->Sat_sponge_cutting_board && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_sponge_cutting_board = 0;
		x1->Staken_sponge = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take sponge sink), id 24 */
double action_take_sponge_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_sponge) && x->Sat_sponge_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter))) *specificity=0.39; 

	if((!(x->Staken_sponge) && x->Sat_sponge_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) *specificity=0.39; 

	if((!(x->Staken_sponge) && x->Sat_sponge_sink && x->Shandsfree && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter)) || (!(x->Staken_sponge) && x->Sat_sponge_sink && x->Shandsfree && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_sponge_sink = 0;
		x1->Staken_sponge = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (take sponge cupboard), id 25 */
double action_take_sponge_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Staken_sponge) && x->Sat_sponge_cupboard && x->Shandsfree && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard)) *specificity=0.39; 

	if((!(x->Staken_sponge) && x->Sat_sponge_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) *specificity=0.39; 

	if((!(x->Staken_sponge) && x->Sat_sponge_cupboard && x->Shandsfree && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard) || (!(x->Staken_sponge) && x->Sat_sponge_cupboard && x->Shandsfree && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) {
		*x1 = *x;
		x1->Shandsfree = 0;
		x1->Sat_sponge_cupboard = 0;
		x1->Staken_sponge = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (put plate sink), id 26 */
double action_put_plate_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_plate && !(x->Sat_plate_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter))) *specificity=0.44; 

	if((x->Staken_plate && !(x->Sat_plate_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) *specificity=0.44; 

	if((x->Staken_plate && !(x->Sat_plate_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter)) || (x->Staken_plate && !(x->Sat_plate_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_plate_sink = 1;
		x1->Staken_plate = 0;
		if(x->Sisat_sink) {
			x1->Sreachable_sink_plate = 1;}
		if(x->Sisat_counter) {
			x1->Sreachable_counter_plate = 1;}
		return 1;
	} else {
		return 0;
	}
}

/* (put plate cupboard), id 27 */
double action_put_plate_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_plate && !(x->Sat_plate_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard)) *specificity=0.44; 

	if((x->Staken_plate && !(x->Sat_plate_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) *specificity=0.44; 

	if((x->Staken_plate && !(x->Sat_plate_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard) || (x->Staken_plate && !(x->Sat_plate_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_plate_cupboard = 1;
		x1->Staken_plate = 0;
		if(x->Sisat_sink) {
			x1->Sreachable_sink_plate = 1;}
		if(x->Sisat_counter) {
			x1->Sreachable_counter_plate = 1;}
		return 1;
	} else {
		return 0;
	}
}

/* (put cutting_board sink), id 28 */
double action_put_cutting_board_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_cutting_board && !(x->Sat_cutting_board_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter))) *specificity=0.44; 

	if((x->Staken_cutting_board && !(x->Sat_cutting_board_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) *specificity=0.44; 

	if((x->Staken_cutting_board && !(x->Sat_cutting_board_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter)) || (x->Staken_cutting_board && !(x->Sat_cutting_board_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_cutting_board_sink = 1;
		x1->Staken_cutting_board = 0;
		if(x->Sisat_sink) {
			x1->Sreachable_sink_cutting_board = 1;}
		if(x->Sisat_counter) {
			x1->Sreachable_counter_cutting_board = 1;}
		return 1;
	} else {
		return 0;
	}
}

/* (put cutting_board cupboard), id 29 */
double action_put_cutting_board_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_cutting_board && !(x->Sat_cutting_board_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard)) *specificity=0.44; 

	if((x->Staken_cutting_board && !(x->Sat_cutting_board_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) *specificity=0.44; 

	if((x->Staken_cutting_board && !(x->Sat_cutting_board_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard) || (x->Staken_cutting_board && !(x->Sat_cutting_board_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_cutting_board_cupboard = 1;
		x1->Staken_cutting_board = 0;
		if(x->Sisat_sink) {
			x1->Sreachable_sink_cutting_board = 1;}
		if(x->Sisat_counter) {
			x1->Sreachable_counter_cutting_board = 1;}
		return 1;
	} else {
		return 0;
	}
}

/* (put carrot plate), id 30 */
double action_put_carrot_plate(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_carrot && !(x->Sat_carrot_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.42; 

	if((x->Staken_carrot && !(x->Sat_carrot_plate) && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.42; 

	if((x->Staken_carrot && !(x->Sat_carrot_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.42; 

	if((x->Staken_carrot && !(x->Sat_carrot_plate) && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.42; 

	if((x->Staken_carrot && !(x->Sat_carrot_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate) || (x->Staken_carrot && !(x->Sat_carrot_plate) && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (x->Staken_carrot && !(x->Sat_carrot_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (x->Staken_carrot && !(x->Sat_carrot_plate) && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_carrot_plate = 1;
		x1->Staken_carrot = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put carrot cutting_board), id 31 */
double action_put_carrot_cutting_board(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_carrot && !(x->Sat_carrot_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.42; 

	if((x->Staken_carrot && !(x->Sat_carrot_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.42; 

	if((x->Staken_carrot && !(x->Sat_carrot_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.42; 

	if((x->Staken_carrot && !(x->Sat_carrot_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.42; 

	if((x->Staken_carrot && !(x->Sat_carrot_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board) || (x->Staken_carrot && !(x->Sat_carrot_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (x->Staken_carrot && !(x->Sat_carrot_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (x->Staken_carrot && !(x->Sat_carrot_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_carrot_cutting_board = 1;
		x1->Staken_carrot = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put carrot sink), id 32 */
double action_put_carrot_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_carrot && !(x->Sat_carrot_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter))) *specificity=0.37; 

	if((x->Staken_carrot && !(x->Sat_carrot_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) *specificity=0.38; 

	if((x->Staken_carrot && !(x->Sat_carrot_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter)) || (x->Staken_carrot && !(x->Sat_carrot_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_carrot_sink = 1;
		x1->Staken_carrot = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put carrot cupboard), id 33 */
double action_put_carrot_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_carrot && !(x->Sat_carrot_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard)) *specificity=0.37; 

	if((x->Staken_carrot && !(x->Sat_carrot_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) *specificity=0.38; 

	if((x->Staken_carrot && !(x->Sat_carrot_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard) || (x->Staken_carrot && !(x->Sat_carrot_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_carrot_cupboard = 1;
		x1->Staken_carrot = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put knife plate), id 34 */
double action_put_knife_plate(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_knife && !(x->Sat_knife_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.42; 

	if((x->Staken_knife && !(x->Sat_knife_plate) && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.42; 

	if((x->Staken_knife && !(x->Sat_knife_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.42; 

	if((x->Staken_knife && !(x->Sat_knife_plate) && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.42; 

	if((x->Staken_knife && !(x->Sat_knife_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate) || (x->Staken_knife && !(x->Sat_knife_plate) && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (x->Staken_knife && !(x->Sat_knife_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (x->Staken_knife && !(x->Sat_knife_plate) && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_knife_plate = 1;
		x1->Staken_knife = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put knife cutting_board), id 35 */
double action_put_knife_cutting_board(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_knife && !(x->Sat_knife_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.42; 

	if((x->Staken_knife && !(x->Sat_knife_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.42; 

	if((x->Staken_knife && !(x->Sat_knife_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.42; 

	if((x->Staken_knife && !(x->Sat_knife_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.42; 

	if((x->Staken_knife && !(x->Sat_knife_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board) || (x->Staken_knife && !(x->Sat_knife_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (x->Staken_knife && !(x->Sat_knife_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (x->Staken_knife && !(x->Sat_knife_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_knife_cutting_board = 1;
		x1->Staken_knife = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put knife sink), id 36 */
double action_put_knife_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_knife && !(x->Sat_knife_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter))) *specificity=0.37; 

	if((x->Staken_knife && !(x->Sat_knife_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) *specificity=0.38; 

	if((x->Staken_knife && !(x->Sat_knife_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter)) || (x->Staken_knife && !(x->Sat_knife_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_knife_sink = 1;
		x1->Staken_knife = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put knife cupboard), id 37 */
double action_put_knife_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_knife && !(x->Sat_knife_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard)) *specificity=0.37; 

	if((x->Staken_knife && !(x->Sat_knife_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) *specificity=0.38; 

	if((x->Staken_knife && !(x->Sat_knife_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard) || (x->Staken_knife && !(x->Sat_knife_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_knife_cupboard = 1;
		x1->Staken_knife = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put wooden_spoon plate), id 38 */
double action_put_wooden_spoon_plate(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.42; 

	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_plate) && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.42; 

	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.42; 

	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_plate) && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.42; 

	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate) || (x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_plate) && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_plate) && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_wooden_spoon_plate = 1;
		x1->Staken_wooden_spoon = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put wooden_spoon cutting_board), id 39 */
double action_put_wooden_spoon_cutting_board(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.42; 

	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.42; 

	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.42; 

	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.42; 

	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board) || (x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_wooden_spoon_cutting_board = 1;
		x1->Staken_wooden_spoon = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put wooden_spoon sink), id 40 */
double action_put_wooden_spoon_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter))) *specificity=0.37; 

	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) *specificity=0.38; 

	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter)) || (x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_wooden_spoon_sink = 1;
		x1->Staken_wooden_spoon = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put wooden_spoon cupboard), id 41 */
double action_put_wooden_spoon_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard)) *specificity=0.37; 

	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) *specificity=0.38; 

	if((x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard) || (x->Staken_wooden_spoon && !(x->Sat_wooden_spoon_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_wooden_spoon_cupboard = 1;
		x1->Staken_wooden_spoon = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put spoon plate), id 42 */
double action_put_spoon_plate(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_spoon && !(x->Sat_spoon_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.42; 

	if((x->Staken_spoon && !(x->Sat_spoon_plate) && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.42; 

	if((x->Staken_spoon && !(x->Sat_spoon_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.42; 

	if((x->Staken_spoon && !(x->Sat_spoon_plate) && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.42; 

	if((x->Staken_spoon && !(x->Sat_spoon_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate) || (x->Staken_spoon && !(x->Sat_spoon_plate) && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (x->Staken_spoon && !(x->Sat_spoon_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (x->Staken_spoon && !(x->Sat_spoon_plate) && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_spoon_plate = 1;
		x1->Staken_spoon = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put spoon cutting_board), id 43 */
double action_put_spoon_cutting_board(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_spoon && !(x->Sat_spoon_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.42; 

	if((x->Staken_spoon && !(x->Sat_spoon_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.42; 

	if((x->Staken_spoon && !(x->Sat_spoon_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.42; 

	if((x->Staken_spoon && !(x->Sat_spoon_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.42; 

	if((x->Staken_spoon && !(x->Sat_spoon_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board) || (x->Staken_spoon && !(x->Sat_spoon_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (x->Staken_spoon && !(x->Sat_spoon_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (x->Staken_spoon && !(x->Sat_spoon_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_spoon_cutting_board = 1;
		x1->Staken_spoon = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put spoon sink), id 44 */
double action_put_spoon_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_spoon && !(x->Sat_spoon_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter))) *specificity=0.37; 

	if((x->Staken_spoon && !(x->Sat_spoon_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) *specificity=0.38; 

	if((x->Staken_spoon && !(x->Sat_spoon_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter)) || (x->Staken_spoon && !(x->Sat_spoon_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_spoon_sink = 1;
		x1->Staken_spoon = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put spoon cupboard), id 45 */
double action_put_spoon_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_spoon && !(x->Sat_spoon_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard)) *specificity=0.37; 

	if((x->Staken_spoon && !(x->Sat_spoon_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) *specificity=0.38; 

	if((x->Staken_spoon && !(x->Sat_spoon_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard) || (x->Staken_spoon && !(x->Sat_spoon_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_spoon_cupboard = 1;
		x1->Staken_spoon = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put sponge plate), id 46 */
double action_put_sponge_plate(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_sponge && !(x->Sat_sponge_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.42; 

	if((x->Staken_sponge && !(x->Sat_sponge_plate) && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.42; 

	if((x->Staken_sponge && !(x->Sat_sponge_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate))) *specificity=0.42; 

	if((x->Staken_sponge && !(x->Sat_sponge_plate) && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) *specificity=0.42; 

	if((x->Staken_sponge && !(x->Sat_sponge_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && x->Sisat_counter && x->Sreachable_counter_plate) || (x->Staken_sponge && !(x->Sat_sponge_plate) && x->Sisat_sink && x->Sreachable_sink_plate && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (x->Staken_sponge && !(x->Sat_sponge_plate) && !(x->Sisat_sink) && !(x->Sreachable_sink_plate) && !(x->Sisat_counter) && !(x->Sreachable_counter_plate)) || (x->Staken_sponge && !(x->Sat_sponge_plate) && x->Sisat_sink && x->Sreachable_sink_plate && x->Sisat_counter && x->Sreachable_counter_plate)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_sponge_plate = 1;
		x1->Staken_sponge = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put sponge cutting_board), id 47 */
double action_put_sponge_cutting_board(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_sponge && !(x->Sat_sponge_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.42; 

	if((x->Staken_sponge && !(x->Sat_sponge_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.42; 

	if((x->Staken_sponge && !(x->Sat_sponge_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board))) *specificity=0.42; 

	if((x->Staken_sponge && !(x->Sat_sponge_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) *specificity=0.42; 

	if((x->Staken_sponge && !(x->Sat_sponge_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && x->Sisat_counter && x->Sreachable_counter_cutting_board) || (x->Staken_sponge && !(x->Sat_sponge_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (x->Staken_sponge && !(x->Sat_sponge_cutting_board) && !(x->Sisat_sink) && !(x->Sreachable_sink_cutting_board) && !(x->Sisat_counter) && !(x->Sreachable_counter_cutting_board)) || (x->Staken_sponge && !(x->Sat_sponge_cutting_board) && x->Sisat_sink && x->Sreachable_sink_cutting_board && x->Sisat_counter && x->Sreachable_counter_cutting_board)) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_sponge_cutting_board = 1;
		x1->Staken_sponge = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put sponge sink), id 48 */
double action_put_sponge_sink(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_sponge && !(x->Sat_sponge_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter))) *specificity=0.37; 

	if((x->Staken_sponge && !(x->Sat_sponge_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) *specificity=0.38; 

	if((x->Staken_sponge && !(x->Sat_sponge_sink) && x->Sisat_sink && x->Sreachable_sink_sink && !(x->Sisat_counter)) || (x->Staken_sponge && !(x->Sat_sponge_sink) && !(x->Sisat_sink) && !(x->Sreachable_sink_sink) && !(x->Sisat_counter))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_sponge_sink = 1;
		x1->Staken_sponge = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (put sponge cupboard), id 49 */
double action_put_sponge_cupboard(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Staken_sponge && !(x->Sat_sponge_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard)) *specificity=0.37; 

	if((x->Staken_sponge && !(x->Sat_sponge_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) *specificity=0.38; 

	if((x->Staken_sponge && !(x->Sat_sponge_cupboard) && !(x->Sisat_sink) && x->Sisat_counter && x->Sreachable_counter_cupboard) || (x->Staken_sponge && !(x->Sat_sponge_cupboard) && !(x->Sisat_sink) && !(x->Sisat_counter) && !(x->Sreachable_counter_cupboard))) {
		*x1 = *x;
		x1->Shandsfree = 1;
		x1->Sat_sponge_cupboard = 1;
		x1->Staken_sponge = 0;
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
		r->Sisat_sink=1; 
	} else 
		 return (exp(rT[9]-t))/1;
	return -1;
}
double getRecOrRefr1 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sisat_counter=1; 
	} else 
		 return (exp(rT[10]-t))/1;
	return -1;
}
double getRecOrRefr2 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_plate=1; r->Sat_plate_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_plate=1; r->Sat_plate_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_plate=1; r->Sat_plate_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_plate=1; r->Sat_plate_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1; 
	} else 
		 return (exp(rT[35]-t)+exp(rT[11]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[35]-t)+exp(rT[11]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[35]-t)+exp(rT[11]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[35]-t)+exp(rT[11]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t))/28;
	return -1;
}
double getRecOrRefr3 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_plate=1; r->Sat_plate_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_plate=1; r->Sat_plate_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_plate=1; r->Sat_plate_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_plate=1; r->Sat_plate_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1; 
	} else 
		 return (exp(rT[35]-t)+exp(rT[12]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[35]-t)+exp(rT[12]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[35]-t)+exp(rT[12]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[35]-t)+exp(rT[12]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t))/28;
	return -1;
}
double getRecOrRefr4 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_cutting_board=1; r->Sat_cutting_board_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_cutting_board=1; r->Sat_cutting_board_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_cutting_board=1; r->Sat_cutting_board_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_cutting_board=1; r->Sat_cutting_board_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1; 
	} else 
		 return (exp(rT[36]-t)+exp(rT[13]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[36]-t)+exp(rT[13]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[36]-t)+exp(rT[13]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[36]-t)+exp(rT[13]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t))/28;
	return -1;
}
double getRecOrRefr5 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_cutting_board=1; r->Sat_cutting_board_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_cutting_board=1; r->Sat_cutting_board_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_cutting_board=1; r->Sat_cutting_board_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_cutting_board=1; r->Sat_cutting_board_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1; 
	} else 
		 return (exp(rT[36]-t)+exp(rT[14]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[36]-t)+exp(rT[14]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[36]-t)+exp(rT[14]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[36]-t)+exp(rT[14]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t))/28;
	return -1;
}
double getRecOrRefr6 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_carrot=1; r->Sat_carrot_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_carrot=1; r->Sat_carrot_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_carrot=1; r->Sat_carrot_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_carrot=1; r->Sat_carrot_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1; 
	} else 
		 return (exp(rT[37]-t)+exp(rT[15]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[37]-t)+exp(rT[15]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[37]-t)+exp(rT[15]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[37]-t)+exp(rT[15]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t))/28;
	return -1;
}
double getRecOrRefr7 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_carrot=1; r->Sat_carrot_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_carrot=1; r->Sat_carrot_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_carrot=1; r->Sat_carrot_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_carrot=1; r->Sat_carrot_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1; 
	} else 
		 return (exp(rT[37]-t)+exp(rT[16]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[37]-t)+exp(rT[16]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[37]-t)+exp(rT[16]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[37]-t)+exp(rT[16]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t))/28;
	return -1;
}
double getRecOrRefr8 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_carrot=1; r->Sat_carrot_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1;  r->Staken_carrot=1; r->Sat_carrot_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1; 
	} else 
		 return (exp(rT[37]-t)+exp(rT[17]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t) +exp(rT[37]-t)+exp(rT[17]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t))/12;
	return -1;
}
double getRecOrRefr9 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_carrot=1; r->Sat_carrot_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1;  r->Staken_carrot=1; r->Sat_carrot_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1; 
	} else 
		 return (exp(rT[37]-t)+exp(rT[18]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t) +exp(rT[37]-t)+exp(rT[18]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t))/12;
	return -1;
}
double getRecOrRefr10 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_knife=1; r->Sat_knife_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_knife=1; r->Sat_knife_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_knife=1; r->Sat_knife_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_knife=1; r->Sat_knife_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1; 
	} else 
		 return (exp(rT[38]-t)+exp(rT[19]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[38]-t)+exp(rT[19]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[38]-t)+exp(rT[19]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[38]-t)+exp(rT[19]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t))/28;
	return -1;
}
double getRecOrRefr11 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_knife=1; r->Sat_knife_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_knife=1; r->Sat_knife_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_knife=1; r->Sat_knife_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_knife=1; r->Sat_knife_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1; 
	} else 
		 return (exp(rT[38]-t)+exp(rT[20]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[38]-t)+exp(rT[20]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[38]-t)+exp(rT[20]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[38]-t)+exp(rT[20]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t))/28;
	return -1;
}
double getRecOrRefr12 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_knife=1; r->Sat_knife_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1;  r->Staken_knife=1; r->Sat_knife_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1; 
	} else 
		 return (exp(rT[38]-t)+exp(rT[21]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t) +exp(rT[38]-t)+exp(rT[21]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t))/12;
	return -1;
}
double getRecOrRefr13 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_knife=1; r->Sat_knife_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1;  r->Staken_knife=1; r->Sat_knife_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1; 
	} else 
		 return (exp(rT[38]-t)+exp(rT[22]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t) +exp(rT[38]-t)+exp(rT[22]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t))/12;
	return -1;
}
double getRecOrRefr14 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1; 
	} else 
		 return (exp(rT[39]-t)+exp(rT[23]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[39]-t)+exp(rT[23]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[39]-t)+exp(rT[23]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[39]-t)+exp(rT[23]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t))/28;
	return -1;
}
double getRecOrRefr15 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1; 
	} else 
		 return (exp(rT[39]-t)+exp(rT[24]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[39]-t)+exp(rT[24]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[39]-t)+exp(rT[24]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[39]-t)+exp(rT[24]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t))/28;
	return -1;
}
double getRecOrRefr16 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1; 
	} else 
		 return (exp(rT[39]-t)+exp(rT[25]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t) +exp(rT[39]-t)+exp(rT[25]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t))/12;
	return -1;
}
double getRecOrRefr17 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1; 
	} else 
		 return (exp(rT[39]-t)+exp(rT[26]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t) +exp(rT[39]-t)+exp(rT[26]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t))/12;
	return -1;
}
double getRecOrRefr18 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_spoon=1; r->Sat_spoon_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_spoon=1; r->Sat_spoon_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_spoon=1; r->Sat_spoon_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_spoon=1; r->Sat_spoon_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1; 
	} else 
		 return (exp(rT[40]-t)+exp(rT[27]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[40]-t)+exp(rT[27]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[40]-t)+exp(rT[27]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[40]-t)+exp(rT[27]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t))/28;
	return -1;
}
double getRecOrRefr19 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_spoon=1; r->Sat_spoon_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_spoon=1; r->Sat_spoon_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_spoon=1; r->Sat_spoon_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_spoon=1; r->Sat_spoon_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1; 
	} else 
		 return (exp(rT[40]-t)+exp(rT[28]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[40]-t)+exp(rT[28]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[40]-t)+exp(rT[28]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[40]-t)+exp(rT[28]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t))/28;
	return -1;
}
double getRecOrRefr20 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_spoon=1; r->Sat_spoon_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1;  r->Staken_spoon=1; r->Sat_spoon_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1; 
	} else 
		 return (exp(rT[40]-t)+exp(rT[29]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t) +exp(rT[40]-t)+exp(rT[29]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t))/12;
	return -1;
}
double getRecOrRefr21 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_spoon=1; r->Sat_spoon_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1;  r->Staken_spoon=1; r->Sat_spoon_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1; 
	} else 
		 return (exp(rT[40]-t)+exp(rT[30]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t) +exp(rT[40]-t)+exp(rT[30]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t))/12;
	return -1;
}
double getRecOrRefr22 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_sponge=1; r->Sat_sponge_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_sponge=1; r->Sat_sponge_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_sponge=1; r->Sat_sponge_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_sponge=1; r->Sat_sponge_plate=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1; 
	} else 
		 return (exp(rT[41]-t)+exp(rT[31]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[41]-t)+exp(rT[31]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[41]-t)+exp(rT[31]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[41]-t)+exp(rT[31]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t))/28;
	return -1;
}
double getRecOrRefr23 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_sponge=1; r->Sat_sponge_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_sponge=1; r->Sat_sponge_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_sponge=1; r->Sat_sponge_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_sponge=1; r->Sat_sponge_cutting_board=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1; 
	} else 
		 return (exp(rT[41]-t)+exp(rT[32]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[41]-t)+exp(rT[32]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[41]-t)+exp(rT[32]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[41]-t)+exp(rT[32]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t))/28;
	return -1;
}
double getRecOrRefr24 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_sponge=1; r->Sat_sponge_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1;  r->Staken_sponge=1; r->Sat_sponge_sink=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1; 
	} else 
		 return (exp(rT[41]-t)+exp(rT[33]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t) +exp(rT[41]-t)+exp(rT[33]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t))/12;
	return -1;
}
double getRecOrRefr25 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_sponge=1; r->Sat_sponge_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1;  r->Staken_sponge=1; r->Sat_sponge_cupboard=1; r->Shandsfree=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1; 
	} else 
		 return (exp(rT[41]-t)+exp(rT[34]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t) +exp(rT[41]-t)+exp(rT[34]-t)+exp(rT[42]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t))/12;
	return -1;
}
double getRecOrRefr26 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_plate=1; r->Sat_plate_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1;  r->Staken_plate=1; r->Sat_plate_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1; 
	} else 
		 return (exp(rT[35]-t)+exp(rT[11]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t) +exp(rT[35]-t)+exp(rT[11]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t))/10;
	return -1;
}
double getRecOrRefr27 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_plate=1; r->Sat_plate_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1;  r->Staken_plate=1; r->Sat_plate_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1; 
	} else 
		 return (exp(rT[35]-t)+exp(rT[12]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t) +exp(rT[35]-t)+exp(rT[12]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t))/10;
	return -1;
}
double getRecOrRefr28 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_cutting_board=1; r->Sat_cutting_board_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1;  r->Staken_cutting_board=1; r->Sat_cutting_board_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1; 
	} else 
		 return (exp(rT[36]-t)+exp(rT[13]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t) +exp(rT[36]-t)+exp(rT[13]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t))/10;
	return -1;
}
double getRecOrRefr29 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_cutting_board=1; r->Sat_cutting_board_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1;  r->Staken_cutting_board=1; r->Sat_cutting_board_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1; 
	} else 
		 return (exp(rT[36]-t)+exp(rT[14]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t) +exp(rT[36]-t)+exp(rT[14]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t))/10;
	return -1;
}
double getRecOrRefr30 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_carrot=1; r->Sat_carrot_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_carrot=1; r->Sat_carrot_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_carrot=1; r->Sat_carrot_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_carrot=1; r->Sat_carrot_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1; 
	} else 
		 return (exp(rT[37]-t)+exp(rT[15]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[37]-t)+exp(rT[15]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[37]-t)+exp(rT[15]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[37]-t)+exp(rT[15]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t))/24;
	return -1;
}
double getRecOrRefr31 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_carrot=1; r->Sat_carrot_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_carrot=1; r->Sat_carrot_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_carrot=1; r->Sat_carrot_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_carrot=1; r->Sat_carrot_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1; 
	} else 
		 return (exp(rT[37]-t)+exp(rT[16]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[37]-t)+exp(rT[16]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[37]-t)+exp(rT[16]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[37]-t)+exp(rT[16]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t))/24;
	return -1;
}
double getRecOrRefr32 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_carrot=1; r->Sat_carrot_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1;  r->Staken_carrot=1; r->Sat_carrot_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1; 
	} else 
		 return (exp(rT[37]-t)+exp(rT[17]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t) +exp(rT[37]-t)+exp(rT[17]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t))/10;
	return -1;
}
double getRecOrRefr33 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_carrot=1; r->Sat_carrot_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1;  r->Staken_carrot=1; r->Sat_carrot_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1; 
	} else 
		 return (exp(rT[37]-t)+exp(rT[18]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t) +exp(rT[37]-t)+exp(rT[18]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t))/10;
	return -1;
}
double getRecOrRefr34 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_knife=1; r->Sat_knife_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_knife=1; r->Sat_knife_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_knife=1; r->Sat_knife_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_knife=1; r->Sat_knife_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1; 
	} else 
		 return (exp(rT[38]-t)+exp(rT[19]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[38]-t)+exp(rT[19]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[38]-t)+exp(rT[19]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[38]-t)+exp(rT[19]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t))/24;
	return -1;
}
double getRecOrRefr35 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_knife=1; r->Sat_knife_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_knife=1; r->Sat_knife_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_knife=1; r->Sat_knife_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_knife=1; r->Sat_knife_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1; 
	} else 
		 return (exp(rT[38]-t)+exp(rT[20]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[38]-t)+exp(rT[20]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[38]-t)+exp(rT[20]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[38]-t)+exp(rT[20]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t))/24;
	return -1;
}
double getRecOrRefr36 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_knife=1; r->Sat_knife_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1;  r->Staken_knife=1; r->Sat_knife_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1; 
	} else 
		 return (exp(rT[38]-t)+exp(rT[21]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t) +exp(rT[38]-t)+exp(rT[21]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t))/10;
	return -1;
}
double getRecOrRefr37 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_knife=1; r->Sat_knife_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1;  r->Staken_knife=1; r->Sat_knife_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1; 
	} else 
		 return (exp(rT[38]-t)+exp(rT[22]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t) +exp(rT[38]-t)+exp(rT[22]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t))/10;
	return -1;
}
double getRecOrRefr38 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1; 
	} else 
		 return (exp(rT[39]-t)+exp(rT[23]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[39]-t)+exp(rT[23]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[39]-t)+exp(rT[23]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[39]-t)+exp(rT[23]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t))/24;
	return -1;
}
double getRecOrRefr39 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1; 
	} else 
		 return (exp(rT[39]-t)+exp(rT[24]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[39]-t)+exp(rT[24]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[39]-t)+exp(rT[24]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[39]-t)+exp(rT[24]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t))/24;
	return -1;
}
double getRecOrRefr40 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1; 
	} else 
		 return (exp(rT[39]-t)+exp(rT[25]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t) +exp(rT[39]-t)+exp(rT[25]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t))/10;
	return -1;
}
double getRecOrRefr41 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1;  r->Staken_wooden_spoon=1; r->Sat_wooden_spoon_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1; 
	} else 
		 return (exp(rT[39]-t)+exp(rT[26]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t) +exp(rT[39]-t)+exp(rT[26]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t))/10;
	return -1;
}
double getRecOrRefr42 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_spoon=1; r->Sat_spoon_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_spoon=1; r->Sat_spoon_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_spoon=1; r->Sat_spoon_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_spoon=1; r->Sat_spoon_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1; 
	} else 
		 return (exp(rT[40]-t)+exp(rT[27]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[40]-t)+exp(rT[27]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[40]-t)+exp(rT[27]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[40]-t)+exp(rT[27]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t))/24;
	return -1;
}
double getRecOrRefr43 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_spoon=1; r->Sat_spoon_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_spoon=1; r->Sat_spoon_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_spoon=1; r->Sat_spoon_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_spoon=1; r->Sat_spoon_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1; 
	} else 
		 return (exp(rT[40]-t)+exp(rT[28]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[40]-t)+exp(rT[28]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[40]-t)+exp(rT[28]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[40]-t)+exp(rT[28]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t))/24;
	return -1;
}
double getRecOrRefr44 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_spoon=1; r->Sat_spoon_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1;  r->Staken_spoon=1; r->Sat_spoon_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1; 
	} else 
		 return (exp(rT[40]-t)+exp(rT[29]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t) +exp(rT[40]-t)+exp(rT[29]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t))/10;
	return -1;
}
double getRecOrRefr45 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_spoon=1; r->Sat_spoon_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1;  r->Staken_spoon=1; r->Sat_spoon_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1; 
	} else 
		 return (exp(rT[40]-t)+exp(rT[30]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t) +exp(rT[40]-t)+exp(rT[30]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t))/10;
	return -1;
}
double getRecOrRefr46 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_sponge=1; r->Sat_sponge_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_sponge=1; r->Sat_sponge_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_sponge=1; r->Sat_sponge_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1;  r->Staken_sponge=1; r->Sat_sponge_plate=1; r->Sisat_sink=1; r->Sreachable_sink_plate=1; r->Sisat_counter=1; r->Sreachable_counter_plate=1; 
	} else 
		 return (exp(rT[41]-t)+exp(rT[31]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[41]-t)+exp(rT[31]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[41]-t)+exp(rT[31]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t) +exp(rT[41]-t)+exp(rT[31]-t)+exp(rT[9]-t)+exp(rT[0]-t)+exp(rT[10]-t)+exp(rT[3]-t))/24;
	return -1;
}
double getRecOrRefr47 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_sponge=1; r->Sat_sponge_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_sponge=1; r->Sat_sponge_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_sponge=1; r->Sat_sponge_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1;  r->Staken_sponge=1; r->Sat_sponge_cutting_board=1; r->Sisat_sink=1; r->Sreachable_sink_cutting_board=1; r->Sisat_counter=1; r->Sreachable_counter_cutting_board=1; 
	} else 
		 return (exp(rT[41]-t)+exp(rT[32]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[41]-t)+exp(rT[32]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[41]-t)+exp(rT[32]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t) +exp(rT[41]-t)+exp(rT[32]-t)+exp(rT[9]-t)+exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[4]-t))/24;
	return -1;
}
double getRecOrRefr48 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_sponge=1; r->Sat_sponge_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1;  r->Staken_sponge=1; r->Sat_sponge_sink=1; r->Sisat_sink=1; r->Sreachable_sink_sink=1; r->Sisat_counter=1; 
	} else 
		 return (exp(rT[41]-t)+exp(rT[33]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t) +exp(rT[41]-t)+exp(rT[33]-t)+exp(rT[9]-t)+exp(rT[2]-t)+exp(rT[10]-t))/10;
	return -1;
}
double getRecOrRefr49 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Staken_sponge=1; r->Sat_sponge_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1;  r->Staken_sponge=1; r->Sat_sponge_cupboard=1; r->Sisat_sink=1; r->Sisat_counter=1; r->Sreachable_counter_cupboard=1; 
	} else 
		 return (exp(rT[41]-t)+exp(rT[34]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t) +exp(rT[41]-t)+exp(rT[34]-t)+exp(rT[9]-t)+exp(rT[10]-t)+exp(rT[6]-t))/10;
	return -1;
}


namespace ActionSchemes {
const ActionScheme s_FINISHED("FINISHED", std::vector<paramType>());
const ActionScheme s_BLOCKED("BLOCKED", std::vector<paramType>());
const ActionScheme s_INITIALIZE("INITIALIZE", std::vector<paramType>());
boost::array<paramType,2> __params_move = {std::make_pair("from","location"),std::make_pair("to","location")};
const ActionScheme s_move("move", std::vector<paramType>(__params_move.begin(), __params_move.end()));
boost::array<paramType,2> __params_take2 = {std::make_pair("c","container"),std::make_pair("from","fixedplace")};
const ActionScheme s_take2("take", std::vector<paramType>(__params_take2.begin(), __params_take2.end()));
boost::array<paramType,2> __params_take = {std::make_pair("t","tool"),std::make_pair("from","place")};
const ActionScheme s_take("take", std::vector<paramType>(__params_take.begin(), __params_take.end()));
boost::array<paramType,2> __params_put2 = {std::make_pair("c","container"),std::make_pair("to","fixedplace")};
const ActionScheme s_put2("put", std::vector<paramType>(__params_put2.begin(), __params_put2.end()));
boost::array<paramType,2> __params_put = {std::make_pair("t","tool"),std::make_pair("to","place")};
const ActionScheme s_put("put", std::vector<paramType>(__params_put.begin(), __params_put.end()));
} // namespace ActionSchemes

ActionScheme const * __actionSchemes[5 + 3] = {
	&ActionSchemes::s_FINISHED,
	&ActionSchemes::s_BLOCKED,
	&ActionSchemes::s_INITIALIZE,
	&ActionSchemes::s_move,
	&ActionSchemes::s_take2,
	&ActionSchemes::s_take,
	&ActionSchemes::s_put2,
	&ActionSchemes::s_put
};
ActionScheme const * *actionSchemes = &__actionSchemes[3];

namespace Actions {
const ActionSchemes::S_FINISHED a_FINISHED(FinishedOpId, "FINISHED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_BLOCKED a_BLOCKED(NoOpId, "BLOCKED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_INITIALIZE a_INITIALIZE(InitOpId, "INITIALIZE", NULL, NULL, NULL, finishedImmediate , pStopcdfImmediate, pStoppdfImmediate, 0.0 /* Initial */);
const ActionSchemes::S_move a_move_sink_counter(0, "(move sink counter)", action_move_sink_counter, NULL, getRecOrRefr0, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 0 */);
const ActionSchemes::S_move a_move_counter_sink(1, "(move counter sink)", action_move_counter_sink, NULL, getRecOrRefr1, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 1 */);
const ActionSchemes::S_take2 a_take_plate_sink(2, "(take plate sink)", action_take_plate_sink, NULL, getRecOrRefr2, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 2 */);
const ActionSchemes::S_take2 a_take_plate_cupboard(3, "(take plate cupboard)", action_take_plate_cupboard, NULL, getRecOrRefr3, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 3 */);
const ActionSchemes::S_take2 a_take_cutting_board_sink(4, "(take cutting_board sink)", action_take_cutting_board_sink, NULL, getRecOrRefr4, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 4 */);
const ActionSchemes::S_take2 a_take_cutting_board_cupboard(5, "(take cutting_board cupboard)", action_take_cutting_board_cupboard, NULL, getRecOrRefr5, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 5 */);
const ActionSchemes::S_take a_take_carrot_plate(6, "(take carrot plate)", action_take_carrot_plate, NULL, getRecOrRefr6, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 6 */);
const ActionSchemes::S_take a_take_carrot_cutting_board(7, "(take carrot cutting_board)", action_take_carrot_cutting_board, NULL, getRecOrRefr7, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 7 */);
const ActionSchemes::S_take a_take_carrot_sink(8, "(take carrot sink)", action_take_carrot_sink, NULL, getRecOrRefr8, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 8 */);
const ActionSchemes::S_take a_take_carrot_cupboard(9, "(take carrot cupboard)", action_take_carrot_cupboard, NULL, getRecOrRefr9, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 9 */);
const ActionSchemes::S_take a_take_knife_plate(10, "(take knife plate)", action_take_knife_plate, NULL, getRecOrRefr10, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 10 */);
const ActionSchemes::S_take a_take_knife_cutting_board(11, "(take knife cutting_board)", action_take_knife_cutting_board, NULL, getRecOrRefr11, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 11 */);
const ActionSchemes::S_take a_take_knife_sink(12, "(take knife sink)", action_take_knife_sink, NULL, getRecOrRefr12, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 12 */);
const ActionSchemes::S_take a_take_knife_cupboard(13, "(take knife cupboard)", action_take_knife_cupboard, NULL, getRecOrRefr13, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 13 */);
const ActionSchemes::S_take a_take_wooden_spoon_plate(14, "(take wooden_spoon plate)", action_take_wooden_spoon_plate, NULL, getRecOrRefr14, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 14 */);
const ActionSchemes::S_take a_take_wooden_spoon_cutting_board(15, "(take wooden_spoon cutting_board)", action_take_wooden_spoon_cutting_board, NULL, getRecOrRefr15, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 15 */);
const ActionSchemes::S_take a_take_wooden_spoon_sink(16, "(take wooden_spoon sink)", action_take_wooden_spoon_sink, NULL, getRecOrRefr16, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 16 */);
const ActionSchemes::S_take a_take_wooden_spoon_cupboard(17, "(take wooden_spoon cupboard)", action_take_wooden_spoon_cupboard, NULL, getRecOrRefr17, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 17 */);
const ActionSchemes::S_take a_take_spoon_plate(18, "(take spoon plate)", action_take_spoon_plate, NULL, getRecOrRefr18, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 18 */);
const ActionSchemes::S_take a_take_spoon_cutting_board(19, "(take spoon cutting_board)", action_take_spoon_cutting_board, NULL, getRecOrRefr19, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 19 */);
const ActionSchemes::S_take a_take_spoon_sink(20, "(take spoon sink)", action_take_spoon_sink, NULL, getRecOrRefr20, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 20 */);
const ActionSchemes::S_take a_take_spoon_cupboard(21, "(take spoon cupboard)", action_take_spoon_cupboard, NULL, getRecOrRefr21, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 21 */);
const ActionSchemes::S_take a_take_sponge_plate(22, "(take sponge plate)", action_take_sponge_plate, NULL, getRecOrRefr22, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 22 */);
const ActionSchemes::S_take a_take_sponge_cutting_board(23, "(take sponge cutting_board)", action_take_sponge_cutting_board, NULL, getRecOrRefr23, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 23 */);
const ActionSchemes::S_take a_take_sponge_sink(24, "(take sponge sink)", action_take_sponge_sink, NULL, getRecOrRefr24, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 24 */);
const ActionSchemes::S_take a_take_sponge_cupboard(25, "(take sponge cupboard)", action_take_sponge_cupboard, NULL, getRecOrRefr25, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 25 */);
const ActionSchemes::S_put2 a_put_plate_sink(26, "(put plate sink)", action_put_plate_sink, NULL, getRecOrRefr26, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 26 */);
const ActionSchemes::S_put2 a_put_plate_cupboard(27, "(put plate cupboard)", action_put_plate_cupboard, NULL, getRecOrRefr27, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 27 */);
const ActionSchemes::S_put2 a_put_cutting_board_sink(28, "(put cutting_board sink)", action_put_cutting_board_sink, NULL, getRecOrRefr28, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 28 */);
const ActionSchemes::S_put2 a_put_cutting_board_cupboard(29, "(put cutting_board cupboard)", action_put_cutting_board_cupboard, NULL, getRecOrRefr29, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 29 */);
const ActionSchemes::S_put a_put_carrot_plate(30, "(put carrot plate)", action_put_carrot_plate, NULL, getRecOrRefr30, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 30 */);
const ActionSchemes::S_put a_put_carrot_cutting_board(31, "(put carrot cutting_board)", action_put_carrot_cutting_board, NULL, getRecOrRefr31, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 31 */);
const ActionSchemes::S_put a_put_carrot_sink(32, "(put carrot sink)", action_put_carrot_sink, NULL, getRecOrRefr32, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 32 */);
const ActionSchemes::S_put a_put_carrot_cupboard(33, "(put carrot cupboard)", action_put_carrot_cupboard, NULL, getRecOrRefr33, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 33 */);
const ActionSchemes::S_put a_put_knife_plate(34, "(put knife plate)", action_put_knife_plate, NULL, getRecOrRefr34, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 34 */);
const ActionSchemes::S_put a_put_knife_cutting_board(35, "(put knife cutting_board)", action_put_knife_cutting_board, NULL, getRecOrRefr35, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 35 */);
const ActionSchemes::S_put a_put_knife_sink(36, "(put knife sink)", action_put_knife_sink, NULL, getRecOrRefr36, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 36 */);
const ActionSchemes::S_put a_put_knife_cupboard(37, "(put knife cupboard)", action_put_knife_cupboard, NULL, getRecOrRefr37, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 37 */);
const ActionSchemes::S_put a_put_wooden_spoon_plate(38, "(put wooden_spoon plate)", action_put_wooden_spoon_plate, NULL, getRecOrRefr38, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 38 */);
const ActionSchemes::S_put a_put_wooden_spoon_cutting_board(39, "(put wooden_spoon cutting_board)", action_put_wooden_spoon_cutting_board, NULL, getRecOrRefr39, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 39 */);
const ActionSchemes::S_put a_put_wooden_spoon_sink(40, "(put wooden_spoon sink)", action_put_wooden_spoon_sink, NULL, getRecOrRefr40, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 40 */);
const ActionSchemes::S_put a_put_wooden_spoon_cupboard(41, "(put wooden_spoon cupboard)", action_put_wooden_spoon_cupboard, NULL, getRecOrRefr41, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 41 */);
const ActionSchemes::S_put a_put_spoon_plate(42, "(put spoon plate)", action_put_spoon_plate, NULL, getRecOrRefr42, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 42 */);
const ActionSchemes::S_put a_put_spoon_cutting_board(43, "(put spoon cutting_board)", action_put_spoon_cutting_board, NULL, getRecOrRefr43, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 43 */);
const ActionSchemes::S_put a_put_spoon_sink(44, "(put spoon sink)", action_put_spoon_sink, NULL, getRecOrRefr44, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 44 */);
const ActionSchemes::S_put a_put_spoon_cupboard(45, "(put spoon cupboard)", action_put_spoon_cupboard, NULL, getRecOrRefr45, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 45 */);
const ActionSchemes::S_put a_put_sponge_plate(46, "(put sponge plate)", action_put_sponge_plate, NULL, getRecOrRefr46, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 46 */);
const ActionSchemes::S_put a_put_sponge_cutting_board(47, "(put sponge cutting_board)", action_put_sponge_cutting_board, NULL, getRecOrRefr47, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 47 */);
const ActionSchemes::S_put a_put_sponge_sink(48, "(put sponge sink)", action_put_sponge_sink, NULL, getRecOrRefr48, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 48 */);
const ActionSchemes::S_put a_put_sponge_cupboard(49, "(put sponge cupboard)", action_put_sponge_cupboard, NULL, getRecOrRefr49, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 49 */);
} // namespace Actions

Action const * __actions[NOPS+3] = {
	&Actions::a_FINISHED,
	&Actions::a_BLOCKED,
	&Actions::a_INITIALIZE,
	&Actions::a_move_sink_counter,
	&Actions::a_move_counter_sink,
	&Actions::a_take_plate_sink,
	&Actions::a_take_plate_cupboard,
	&Actions::a_take_cutting_board_sink,
	&Actions::a_take_cutting_board_cupboard,
	&Actions::a_take_carrot_plate,
	&Actions::a_take_carrot_cutting_board,
	&Actions::a_take_carrot_sink,
	&Actions::a_take_carrot_cupboard,
	&Actions::a_take_knife_plate,
	&Actions::a_take_knife_cutting_board,
	&Actions::a_take_knife_sink,
	&Actions::a_take_knife_cupboard,
	&Actions::a_take_wooden_spoon_plate,
	&Actions::a_take_wooden_spoon_cutting_board,
	&Actions::a_take_wooden_spoon_sink,
	&Actions::a_take_wooden_spoon_cupboard,
	&Actions::a_take_spoon_plate,
	&Actions::a_take_spoon_cutting_board,
	&Actions::a_take_spoon_sink,
	&Actions::a_take_spoon_cupboard,
	&Actions::a_take_sponge_plate,
	&Actions::a_take_sponge_cutting_board,
	&Actions::a_take_sponge_sink,
	&Actions::a_take_sponge_cupboard,
	&Actions::a_put_plate_sink,
	&Actions::a_put_plate_cupboard,
	&Actions::a_put_cutting_board_sink,
	&Actions::a_put_cutting_board_cupboard,
	&Actions::a_put_carrot_plate,
	&Actions::a_put_carrot_cutting_board,
	&Actions::a_put_carrot_sink,
	&Actions::a_put_carrot_cupboard,
	&Actions::a_put_knife_plate,
	&Actions::a_put_knife_cutting_board,
	&Actions::a_put_knife_sink,
	&Actions::a_put_knife_cupboard,
	&Actions::a_put_wooden_spoon_plate,
	&Actions::a_put_wooden_spoon_cutting_board,
	&Actions::a_put_wooden_spoon_sink,
	&Actions::a_put_wooden_spoon_cupboard,
	&Actions::a_put_spoon_plate,
	&Actions::a_put_spoon_cutting_board,
	&Actions::a_put_spoon_sink,
	&Actions::a_put_spoon_cupboard,
	&Actions::a_put_sponge_plate,
	&Actions::a_put_sponge_cutting_board,
	&Actions::a_put_sponge_sink,
	&Actions::a_put_sponge_cupboard
};
Action const * *actions = &__actions[3];

const int agentOps0[50] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49};
char const * const agentNames[NAGENTS] = {"NULL"};
int const nOpsForAgent[NAGENTS] = {50};
int const * const opsForAgent[NAGENTS] = {agentOps0};

bool isGoalState(size_t goal, StatePtr x) { (void) x;
	switch(goal) {
	case 0: /*carrots2*/
		return (x->Sat_plate_sink && x->Sat_cutting_board_sink && x->Sat_carrot_sink && x->Sat_knife_sink && x->Sat_wooden_spoon_sink && x->Sat_spoon_sink && x->Sat_sponge_sink);
	default: throw std::runtime_error("isGoalState: Unknown goal"); break;
	}
}

void sampleInitial(size_t initialState, StateRec *x1) {
	bzero(x1,sizeof(StateRec));
	switch(initialState) {
	case 0: /*carrots2*/
		x1->Sreachable_sink_sink = 1;
		x1->Sreachable_table_table = 1;
		x1->Sreachable_counter_cupboard = 1;
		x1->Sreachable_counter_stove = 1;
		x1->Sreachable_counter_counter = 1;
		x1->Sreachable_counter_cutting_board = 1;
		x1->Sreachable_counter_plate = 1;
		x1->Sat_cutting_board_cupboard = 1;
		x1->Sat_plate_cupboard = 1;
		x1->Sat_sponge_cupboard = 1;
		x1->Sat_spoon_cupboard = 1;
		x1->Sat_wooden_spoon_cupboard = 1;
		x1->Sat_knife_cupboard = 1;
		x1->Sat_carrot_cupboard = 1;
		x1->Shandsfree = 1;
		x1->Sisat_sink = 1;
		x1->Sreachable_sink_plate = 0;
		x1->Sreachable_sink_cutting_board = 0;
		x1->Sisat_counter = 0;
		x1->Sat_plate_sink = 0;
		x1->Sat_cutting_board_sink = 0;
		x1->Sat_carrot_plate = 0;
		x1->Sat_carrot_cutting_board = 0;
		x1->Sat_carrot_sink = 0;
		x1->Sat_knife_plate = 0;
		x1->Sat_knife_cutting_board = 0;
		x1->Sat_knife_sink = 0;
		x1->Sat_wooden_spoon_plate = 0;
		x1->Sat_wooden_spoon_cutting_board = 0;
		x1->Sat_wooden_spoon_sink = 0;
		x1->Sat_spoon_plate = 0;
		x1->Sat_spoon_cutting_board = 0;
		x1->Sat_spoon_sink = 0;
		x1->Sat_sponge_plate = 0;
		x1->Sat_sponge_cutting_board = 0;
		x1->Sat_sponge_sink = 0;
		x1->Staken_plate = 0;
		x1->Staken_cutting_board = 0;
		x1->Staken_carrot = 0;
		x1->Staken_knife = 0;
		x1->Staken_wooden_spoon = 0;
		x1->Staken_spoon = 0;
		x1->Staken_sponge = 0;
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
				accepted.Sreachable_counter_cutting_board=1;
				accepted.Sreachable_counter_plate=1;
				accepted.Shandsfree=1;
				accepted.Sreachable_sink_sink=1;
				accepted.Sisat_sink=1;
			} else {
				accepted= *parentAccepted;
		
				if (s->Sreachable_sink_cutting_board==1) if (parentAccepted->Staken_cutting_board==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  accepted.Sreachable_sink_cutting_board=1;
				if (s->Sreachable_counter_cutting_board==1) accepted.Sreachable_counter_cutting_board=1;
				if (s->Sreachable_sink_plate==1) if (parentAccepted->Staken_plate==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  accepted.Sreachable_sink_plate=1;
				if (s->Sreachable_counter_plate==1) accepted.Sreachable_counter_plate=1;
				if (s->Shandsfree==1) accepted.Shandsfree=1;
				if (s->Staken_sponge==1) if (parentAccepted->Shandsfree==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  accepted.Staken_sponge=1;
				if (s->Staken_spoon==1) if (parentAccepted->Shandsfree==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  accepted.Staken_spoon=1;
				if (s->Staken_wooden_spoon==1) if (parentAccepted->Shandsfree==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  accepted.Staken_wooden_spoon=1;
				if (s->Staken_knife==1) if (parentAccepted->Shandsfree==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  accepted.Staken_knife=1;
				if (s->Staken_carrot==1) if (parentAccepted->Shandsfree==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  accepted.Staken_carrot=1;
				if (s->Staken_cutting_board==1) if (parentAccepted->Shandsfree==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sreachable_counter_cutting_board==1)  if (parentAccepted->Sisat_sink==1)  if (parentAccepted->Sreachable_sink_cutting_board==1)  accepted.Staken_cutting_board=1;
				if (s->Sreachable_sink_sink==1) accepted.Sreachable_sink_sink=1;
				if (s->Sisat_sink==1) accepted.Sisat_sink=1;
				if (s->Sisat_counter==1) if (parentAccepted->Sisat_sink==1)  accepted.Sisat_counter=1;
				if (s->Staken_plate==1) if (parentAccepted->Shandsfree==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sreachable_counter_plate==1)  if (parentAccepted->Sisat_sink==1)  if (parentAccepted->Sreachable_sink_plate==1)  accepted.Staken_plate=1;
				if (s->Sat_plate_sink==1) if (parentAccepted->Staken_plate==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  if (parentAccepted->Sreachable_sink_sink==1)  accepted.Sat_plate_sink=1;
				if (s->Sat_cutting_board_sink==1) if (parentAccepted->Staken_cutting_board==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  if (parentAccepted->Sreachable_sink_sink==1)  accepted.Sat_cutting_board_sink=1;
				if (s->Sat_carrot_sink==1) if (parentAccepted->Staken_carrot==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  if (parentAccepted->Sreachable_sink_sink==1)  accepted.Sat_carrot_sink=1;
				if (s->Sat_knife_sink==1) if (parentAccepted->Staken_knife==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  if (parentAccepted->Sreachable_sink_sink==1)  accepted.Sat_knife_sink=1;
				if (s->Sat_wooden_spoon_sink==1) if (parentAccepted->Staken_wooden_spoon==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  if (parentAccepted->Sreachable_sink_sink==1)  accepted.Sat_wooden_spoon_sink=1;
				if (s->Sat_spoon_sink==1) if (parentAccepted->Staken_spoon==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  if (parentAccepted->Sreachable_sink_sink==1)  accepted.Sat_spoon_sink=1;
				if (s->Sat_sponge_sink==1) if (parentAccepted->Staken_sponge==1)  if (parentAccepted->Sisat_counter==1)  if (parentAccepted->Sisat_sink==1)  if (parentAccepted->Sreachable_sink_sink==1)  accepted.Sat_sponge_sink=1;
			} // compute the heuristic
			if (accepted.Sreachable_sink_cutting_board== 0) count++;
			if (accepted.Sreachable_counter_cutting_board== 0) count++;
			if (accepted.Sreachable_sink_plate== 0) count++;
			if (accepted.Sreachable_counter_plate== 0) count++;
			if (accepted.Shandsfree== 0) count++;
			if (accepted.Staken_sponge== 0) count++;
			if (accepted.Staken_spoon== 0) count++;
			if (accepted.Staken_wooden_spoon== 0) count++;
			if (accepted.Staken_knife== 0) count++;
			if (accepted.Staken_carrot== 0) count++;
			if (accepted.Staken_cutting_board== 0) count++;
			if (accepted.Sreachable_sink_sink== 0) count++;
			if (accepted.Sisat_sink== 0) count++;
			if (accepted.Sisat_counter== 0) count++;
			if (accepted.Staken_plate== 0) count++;
			if (accepted.Sat_plate_sink== 0) count++;
			if (accepted.Sat_cutting_board_sink== 0) count++;
			if (accepted.Sat_carrot_sink== 0) count++;
			if (accepted.Sat_knife_sink== 0) count++;
			if (accepted.Sat_wooden_spoon_sink== 0) count++;
			if (accepted.Sat_spoon_sink== 0) count++;
			if (accepted.Sat_sponge_sink== 0) count++;
				if (accepted.Sat_plate_sink== 1 && s->Sat_plate_sink== 0) count++;
				if (accepted.Sat_cutting_board_sink== 1 && s->Sat_cutting_board_sink== 0) count++;
				if (accepted.Sat_carrot_sink== 1 && s->Sat_carrot_sink== 0) count++;
				if (accepted.Sat_knife_sink== 1 && s->Sat_knife_sink== 0) count++;
				if (accepted.Sat_wooden_spoon_sink== 1 && s->Sat_wooden_spoon_sink== 0) count++;
				if (accepted.Sat_spoon_sink== 1 && s->Sat_spoon_sink== 0) count++;
				if (accepted.Sat_sponge_sink== 1 && s->Sat_sponge_sink== 0) count++;
			if (accepted.Sreachable_sink_cutting_board== 1 && s->Sreachable_sink_cutting_board== 0  && (accepted.Staken_cutting_board==0)) count++;
			if (accepted.Sreachable_counter_cutting_board== 1 && s->Sreachable_counter_cutting_board== 0  && (accepted.Staken_cutting_board==0)) count++;
			if (accepted.Sreachable_sink_plate== 1 && s->Sreachable_sink_plate== 0  && (accepted.Staken_plate==0)) count++;
			if (accepted.Sreachable_counter_plate== 1 && s->Sreachable_counter_plate== 0  && (accepted.Staken_plate==0)) count++;
			if (accepted.Shandsfree== 1 && s->Shandsfree== 0  && (accepted.Staken_plate==0 || accepted.Staken_cutting_board==0 || accepted.Staken_carrot==0 || accepted.Staken_knife==0 || accepted.Staken_wooden_spoon==0 || accepted.Staken_spoon==0 || accepted.Staken_sponge==0)) count++;
			if (accepted.Staken_sponge== 1 && s->Staken_sponge== 0  && (accepted.Sat_sponge_sink==0)) count++;
			if (accepted.Staken_spoon== 1 && s->Staken_spoon== 0  && (accepted.Sat_spoon_sink==0)) count++;
			if (accepted.Staken_wooden_spoon== 1 && s->Staken_wooden_spoon== 0  && (accepted.Sat_wooden_spoon_sink==0)) count++;
			if (accepted.Staken_knife== 1 && s->Staken_knife== 0  && (accepted.Sat_knife_sink==0)) count++;
			if (accepted.Staken_carrot== 1 && s->Staken_carrot== 0  && (accepted.Sat_carrot_sink==0)) count++;
			if (accepted.Staken_cutting_board== 1 && s->Staken_cutting_board== 0  && (accepted.Sat_cutting_board_sink==0 || accepted.Sreachable_sink_cutting_board==0)) count++;
			if (accepted.Sreachable_sink_sink== 1 && s->Sreachable_sink_sink== 0  && (accepted.Sat_plate_sink==0 || accepted.Sat_cutting_board_sink==0 || accepted.Sat_carrot_sink==0 || accepted.Sat_knife_sink==0 || accepted.Sat_wooden_spoon_sink==0 || accepted.Sat_spoon_sink==0 || accepted.Sat_sponge_sink==0)) count++;
			if (accepted.Sisat_sink== 1 && s->Sisat_sink== 0  && (accepted.Sat_plate_sink==0 || accepted.Sat_cutting_board_sink==0 || accepted.Sat_carrot_sink==0 || accepted.Sat_knife_sink==0 || accepted.Sat_wooden_spoon_sink==0 || accepted.Sat_spoon_sink==0 || accepted.Sat_sponge_sink==0 || accepted.Sisat_counter==0 || accepted.Staken_plate==0 || accepted.Staken_cutting_board==0 || accepted.Staken_carrot==0 || accepted.Staken_knife==0 || accepted.Staken_wooden_spoon==0 || accepted.Staken_spoon==0 || accepted.Staken_sponge==0 || accepted.Sreachable_sink_plate==0 || accepted.Sreachable_sink_cutting_board==0)) count++;
			if (accepted.Sisat_counter== 1 && s->Sisat_counter== 0  && (accepted.Sat_plate_sink==0 || accepted.Sat_cutting_board_sink==0 || accepted.Sat_carrot_sink==0 || accepted.Sat_knife_sink==0 || accepted.Sat_wooden_spoon_sink==0 || accepted.Sat_spoon_sink==0 || accepted.Sat_sponge_sink==0 || accepted.Staken_plate==0 || accepted.Staken_cutting_board==0 || accepted.Staken_carrot==0 || accepted.Staken_knife==0 || accepted.Staken_wooden_spoon==0 || accepted.Staken_spoon==0 || accepted.Staken_sponge==0 || accepted.Sreachable_sink_plate==0 || accepted.Sreachable_sink_cutting_board==0)) count++;
			if (accepted.Staken_plate== 1 && s->Staken_plate== 0  && (accepted.Sat_plate_sink==0 || accepted.Sreachable_sink_plate==0)) count++;
			break;
		default: throw std::runtime_error("landmarkCountHeuristic: Unknown goal state");
		}
		break;
	default: throw std::runtime_error("landmarkCountHeuristic: Unknown initial state");
	}
	*parentAccepted = accepted;
	return count;
}
