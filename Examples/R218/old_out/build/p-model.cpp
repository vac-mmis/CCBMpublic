const char * DOMAIN_NAME = "r218ddev";
const char * PROBLEM_NAME = "r218pdev-simple";

char const * const objnames_object[] = {"alice","bob","charlie","nb1","nb2","nb3","prj1","prj2","prj3","scr1","scr2","scr3","crossbar","doc1","doc2","doc3","vga-in-port","vga-out-port","vga-out-signal","picture","attention","seatA","seatB","seatC","door","i1","i2","i3","o1","o2","o3"};
char const * const objnames_projector[] = {"prj1","prj2","prj3"};
char const * const objnames_screen[] = {"scr1","scr2","scr3"};
char const * const objnames_notebook[] = {"nb1","nb2","nb3"};
char const * const objnames_device[] = {"crossbar","prj1","prj2","prj3","scr1","scr2","scr3","nb1","nb2","nb3"};
char const * const objnames_person[] = {"alice","bob","charlie"};
char const * const objnames_agent[] = {"alice","bob","charlie","crossbar","prj1","prj2","prj3","scr1","scr2","scr3","nb1","nb2","nb3"};
char const * const objnames_port[] = {"vga-in-port","vga-out-port"};
char const * const objnames_resource[] = {"vga-out-signal","picture","attention","vga-in-port","vga-out-port"};
char const * const objnames_seat[] = {"seatA","seatB","seatC"};
char const * const objnames_small_place[] = {"seatA","seatB","seatC"};
char const * const objnames_big_place[] = {"door"};
char const * const objnames_location[] = {"door","seatA","seatB","seatC"};
char const * const objnames_cb_in_port[] = {"i1","i2","i3"};
char const * const objnames_cb_out_port[] = {"o1","o2","o3"};
char const * const objnames_cb_port[] = {"i1","i2","i3","o1","o2","o3"};
char const * const objnames_document[] = {"doc1","doc2","doc3"};

std::ostream& operator<<(std::ostream &o, StatePtr x) {
	if(x->Sis_active_nb1) o << " (is-active nb1)"; else o<< " (not (is-active nb1))"; 
	if(x->Sis_active_nb2) o << " (is-active nb2)"; else o<< " (not (is-active nb2))"; 
	if(x->Sis_active_nb3) o << " (is-active nb3)"; else o<< " (not (is-active nb3))"; 
	if(x->Sis_active_prj1) o << " (is-active prj1)"; else o<< " (not (is-active prj1))"; 
	if(x->Sis_active_prj2) o << " (is-active prj2)"; else o<< " (not (is-active prj2))"; 
	if(x->Sis_active_prj3) o << " (is-active prj3)"; else o<< " (not (is-active prj3))"; 
	if(x->Sis_active_scr1) o << " (is-active scr1)"; else o<< " (not (is-active scr1))"; 
	if(x->Sis_active_scr2) o << " (is-active scr2)"; else o<< " (not (is-active scr2))"; 
	if(x->Sis_active_scr3) o << " (is-active scr3)"; else o<< " (not (is-active scr3))"; 
	if(x->Sin_use_nb1) o << " (in-use nb1)"; else o<< " (not (in-use nb1))"; 
	if(x->Sin_use_nb2) o << " (in-use nb2)"; else o<< " (not (in-use nb2))"; 
	if(x->Sin_use_nb3) o << " (in-use nb3)"; else o<< " (not (in-use nb3))"; 
	if(x->Sin_use_prj1) o << " (in-use prj1)"; else o<< " (not (in-use prj1))"; 
	if(x->Sin_use_prj2) o << " (in-use prj2)"; else o<< " (not (in-use prj2))"; 
	if(x->Sin_use_prj3) o << " (in-use prj3)"; else o<< " (not (in-use prj3))"; 
	if(x->Sin_use_scr1) o << " (in-use scr1)"; else o<< " (not (in-use scr1))"; 
	if(x->Sin_use_scr2) o << " (in-use scr2)"; else o<< " (not (in-use scr2))"; 
	if(x->Sin_use_scr3) o << " (in-use scr3)"; else o<< " (not (in-use scr3))"; 
	if(x->Sis_locked_by_alice_attention_alice) o << " (is-locked-by alice attention alice)"; else o<< " (not (is-locked-by alice attention alice))"; 
	if(x->Sis_locked_by_bob_attention_bob) o << " (is-locked-by bob attention bob)"; else o<< " (not (is-locked-by bob attention bob))"; 
	if(x->Sis_locked_by_charlie_attention_charlie) o << " (is-locked-by charlie attention charlie)"; else o<< " (not (is-locked-by charlie attention charlie))"; 
	if(x->Sis_locked_by_nb1_vga_out_port_crossbar) o << " (is-locked-by nb1 vga-out-port crossbar)"; else o<< " (not (is-locked-by nb1 vga-out-port crossbar))"; 
	if(x->Sis_locked_by_nb1_vga_out_signal_prj1) o << " (is-locked-by nb1 vga-out-signal prj1)"; else o<< " (not (is-locked-by nb1 vga-out-signal prj1))"; 
	if(x->Sis_locked_by_nb1_vga_out_signal_prj2) o << " (is-locked-by nb1 vga-out-signal prj2)"; else o<< " (not (is-locked-by nb1 vga-out-signal prj2))"; 
	if(x->Sis_locked_by_nb1_vga_out_signal_prj3) o << " (is-locked-by nb1 vga-out-signal prj3)"; else o<< " (not (is-locked-by nb1 vga-out-signal prj3))"; 
	if(x->Sis_locked_by_nb2_vga_out_port_crossbar) o << " (is-locked-by nb2 vga-out-port crossbar)"; else o<< " (not (is-locked-by nb2 vga-out-port crossbar))"; 
	if(x->Sis_locked_by_nb2_vga_out_signal_prj1) o << " (is-locked-by nb2 vga-out-signal prj1)"; else o<< " (not (is-locked-by nb2 vga-out-signal prj1))"; 
	if(x->Sis_locked_by_nb2_vga_out_signal_prj2) o << " (is-locked-by nb2 vga-out-signal prj2)"; else o<< " (not (is-locked-by nb2 vga-out-signal prj2))"; 
	if(x->Sis_locked_by_nb2_vga_out_signal_prj3) o << " (is-locked-by nb2 vga-out-signal prj3)"; else o<< " (not (is-locked-by nb2 vga-out-signal prj3))"; 
	if(x->Sis_locked_by_nb3_vga_out_port_crossbar) o << " (is-locked-by nb3 vga-out-port crossbar)"; else o<< " (not (is-locked-by nb3 vga-out-port crossbar))"; 
	if(x->Sis_locked_by_nb3_vga_out_signal_prj1) o << " (is-locked-by nb3 vga-out-signal prj1)"; else o<< " (not (is-locked-by nb3 vga-out-signal prj1))"; 
	if(x->Sis_locked_by_nb3_vga_out_signal_prj2) o << " (is-locked-by nb3 vga-out-signal prj2)"; else o<< " (not (is-locked-by nb3 vga-out-signal prj2))"; 
	if(x->Sis_locked_by_nb3_vga_out_signal_prj3) o << " (is-locked-by nb3 vga-out-signal prj3)"; else o<< " (not (is-locked-by nb3 vga-out-signal prj3))"; 
	if(x->Sis_locked_by_prj1_vga_in_port_prj1) o << " (is-locked-by prj1 vga-in-port prj1)"; else o<< " (not (is-locked-by prj1 vga-in-port prj1))"; 
	if(x->Sis_locked_by_prj1_vga_in_port_crossbar) o << " (is-locked-by prj1 vga-in-port crossbar)"; else o<< " (not (is-locked-by prj1 vga-in-port crossbar))"; 
	if(x->Sis_locked_by_prj1_picture_scr1) o << " (is-locked-by prj1 picture scr1)"; else o<< " (not (is-locked-by prj1 picture scr1))"; 
	if(x->Sis_locked_by_prj2_vga_in_port_prj2) o << " (is-locked-by prj2 vga-in-port prj2)"; else o<< " (not (is-locked-by prj2 vga-in-port prj2))"; 
	if(x->Sis_locked_by_prj2_vga_in_port_crossbar) o << " (is-locked-by prj2 vga-in-port crossbar)"; else o<< " (not (is-locked-by prj2 vga-in-port crossbar))"; 
	if(x->Sis_locked_by_prj2_picture_scr2) o << " (is-locked-by prj2 picture scr2)"; else o<< " (not (is-locked-by prj2 picture scr2))"; 
	if(x->Sis_locked_by_prj3_vga_in_port_prj3) o << " (is-locked-by prj3 vga-in-port prj3)"; else o<< " (not (is-locked-by prj3 vga-in-port prj3))"; 
	if(x->Sis_locked_by_prj3_vga_in_port_crossbar) o << " (is-locked-by prj3 vga-in-port crossbar)"; else o<< " (not (is-locked-by prj3 vga-in-port crossbar))"; 
	if(x->Sis_locked_by_prj3_picture_scr3) o << " (is-locked-by prj3 picture scr3)"; else o<< " (not (is-locked-by prj3 picture scr3))"; 
	if(x->Sis_locked_by_scr1_picture_alice) o << " (is-locked-by scr1 picture alice)"; else o<< " (not (is-locked-by scr1 picture alice))"; 
	if(x->Sis_locked_by_scr1_picture_charlie) o << " (is-locked-by scr1 picture charlie)"; else o<< " (not (is-locked-by scr1 picture charlie))"; 
	if(x->Sis_locked_by_scr2_picture_alice) o << " (is-locked-by scr2 picture alice)"; else o<< " (not (is-locked-by scr2 picture alice))"; 
	if(x->Sis_locked_by_scr2_picture_bob) o << " (is-locked-by scr2 picture bob)"; else o<< " (not (is-locked-by scr2 picture bob))"; 
	if(x->Sis_locked_by_scr2_picture_charlie) o << " (is-locked-by scr2 picture charlie)"; else o<< " (not (is-locked-by scr2 picture charlie))"; 
	if(x->Sis_locked_by_scr3_picture_alice) o << " (is-locked-by scr3 picture alice)"; else o<< " (not (is-locked-by scr3 picture alice))"; 
	if(x->Sis_locked_by_scr3_picture_bob) o << " (is-locked-by scr3 picture bob)"; else o<< " (not (is-locked-by scr3 picture bob))"; 
	if(x->Sis_sending_nb1_doc1_vga_out_signal) o << " (is-sending nb1 doc1 vga-out-signal)"; else o<< " (not (is-sending nb1 doc1 vga-out-signal))"; 
	if(x->Sis_sending_nb2_doc2_vga_out_signal) o << " (is-sending nb2 doc2 vga-out-signal)"; else o<< " (not (is-sending nb2 doc2 vga-out-signal))"; 
	if(x->Sis_sending_nb3_doc3_vga_out_signal) o << " (is-sending nb3 doc3 vga-out-signal)"; else o<< " (not (is-sending nb3 doc3 vga-out-signal))"; 
	if(x->Sis_connected_nb1_vga_out_port_prj1_vga_in_port) o << " (is-connected nb1 vga-out-port prj1 vga-in-port)"; else o<< " (not (is-connected nb1 vga-out-port prj1 vga-in-port))"; 
	if(x->Sis_connected_nb1_vga_out_port_prj2_vga_in_port) o << " (is-connected nb1 vga-out-port prj2 vga-in-port)"; else o<< " (not (is-connected nb1 vga-out-port prj2 vga-in-port))"; 
	if(x->Sis_connected_nb1_vga_out_port_prj3_vga_in_port) o << " (is-connected nb1 vga-out-port prj3 vga-in-port)"; else o<< " (not (is-connected nb1 vga-out-port prj3 vga-in-port))"; 
	if(x->Sis_connected_nb2_vga_out_port_prj1_vga_in_port) o << " (is-connected nb2 vga-out-port prj1 vga-in-port)"; else o<< " (not (is-connected nb2 vga-out-port prj1 vga-in-port))"; 
	if(x->Sis_connected_nb2_vga_out_port_prj2_vga_in_port) o << " (is-connected nb2 vga-out-port prj2 vga-in-port)"; else o<< " (not (is-connected nb2 vga-out-port prj2 vga-in-port))"; 
	if(x->Sis_connected_nb2_vga_out_port_prj3_vga_in_port) o << " (is-connected nb2 vga-out-port prj3 vga-in-port)"; else o<< " (not (is-connected nb2 vga-out-port prj3 vga-in-port))"; 
	if(x->Sis_connected_nb3_vga_out_port_prj1_vga_in_port) o << " (is-connected nb3 vga-out-port prj1 vga-in-port)"; else o<< " (not (is-connected nb3 vga-out-port prj1 vga-in-port))"; 
	if(x->Sis_connected_nb3_vga_out_port_prj2_vga_in_port) o << " (is-connected nb3 vga-out-port prj2 vga-in-port)"; else o<< " (not (is-connected nb3 vga-out-port prj2 vga-in-port))"; 
	if(x->Sis_connected_nb3_vga_out_port_prj3_vga_in_port) o << " (is-connected nb3 vga-out-port prj3 vga-in-port)"; else o<< " (not (is-connected nb3 vga-out-port prj3 vga-in-port))"; 
	if(x->Sis_projecting_by_prj1_doc1_nb1) o << " (is-projecting-by prj1 doc1 nb1)"; else o<< " (not (is-projecting-by prj1 doc1 nb1))"; 
	if(x->Sis_projecting_by_prj1_doc2_nb2) o << " (is-projecting-by prj1 doc2 nb2)"; else o<< " (not (is-projecting-by prj1 doc2 nb2))"; 
	if(x->Sis_projecting_by_prj1_doc3_nb3) o << " (is-projecting-by prj1 doc3 nb3)"; else o<< " (not (is-projecting-by prj1 doc3 nb3))"; 
	if(x->Sis_projecting_by_prj2_doc1_nb1) o << " (is-projecting-by prj2 doc1 nb1)"; else o<< " (not (is-projecting-by prj2 doc1 nb1))"; 
	if(x->Sis_projecting_by_prj2_doc2_nb2) o << " (is-projecting-by prj2 doc2 nb2)"; else o<< " (not (is-projecting-by prj2 doc2 nb2))"; 
	if(x->Sis_projecting_by_prj2_doc3_nb3) o << " (is-projecting-by prj2 doc3 nb3)"; else o<< " (not (is-projecting-by prj2 doc3 nb3))"; 
	if(x->Sis_projecting_by_prj3_doc1_nb1) o << " (is-projecting-by prj3 doc1 nb1)"; else o<< " (not (is-projecting-by prj3 doc1 nb1))"; 
	if(x->Sis_projecting_by_prj3_doc2_nb2) o << " (is-projecting-by prj3 doc2 nb2)"; else o<< " (not (is-projecting-by prj3 doc2 nb2))"; 
	if(x->Sis_projecting_by_prj3_doc3_nb3) o << " (is-projecting-by prj3 doc3 nb3)"; else o<< " (not (is-projecting-by prj3 doc3 nb3))"; 
	if(x->Sis_projecting_prj1_doc1) o << " (is-projecting prj1 doc1)"; else o<< " (not (is-projecting prj1 doc1))"; 
	if(x->Sis_projecting_prj1_doc2) o << " (is-projecting prj1 doc2)"; else o<< " (not (is-projecting prj1 doc2))"; 
	if(x->Sis_projecting_prj1_doc3) o << " (is-projecting prj1 doc3)"; else o<< " (not (is-projecting prj1 doc3))"; 
	if(x->Sis_projecting_prj2_doc1) o << " (is-projecting prj2 doc1)"; else o<< " (not (is-projecting prj2 doc1))"; 
	if(x->Sis_projecting_prj2_doc2) o << " (is-projecting prj2 doc2)"; else o<< " (not (is-projecting prj2 doc2))"; 
	if(x->Sis_projecting_prj2_doc3) o << " (is-projecting prj2 doc3)"; else o<< " (not (is-projecting prj2 doc3))"; 
	if(x->Sis_projecting_prj3_doc1) o << " (is-projecting prj3 doc1)"; else o<< " (not (is-projecting prj3 doc1))"; 
	if(x->Sis_projecting_prj3_doc2) o << " (is-projecting prj3 doc2)"; else o<< " (not (is-projecting prj3 doc2))"; 
	if(x->Sis_projecting_prj3_doc3) o << " (is-projecting prj3 doc3)"; else o<< " (not (is-projecting prj3 doc3))"; 
	if(x->Sis_showing_by_scr1_doc1_prj1) o << " (is-showing-by scr1 doc1 prj1)"; else o<< " (not (is-showing-by scr1 doc1 prj1))"; 
	if(x->Sis_showing_by_scr1_doc2_prj1) o << " (is-showing-by scr1 doc2 prj1)"; else o<< " (not (is-showing-by scr1 doc2 prj1))"; 
	if(x->Sis_showing_by_scr1_doc3_prj1) o << " (is-showing-by scr1 doc3 prj1)"; else o<< " (not (is-showing-by scr1 doc3 prj1))"; 
	if(x->Sis_showing_by_scr2_doc1_prj2) o << " (is-showing-by scr2 doc1 prj2)"; else o<< " (not (is-showing-by scr2 doc1 prj2))"; 
	if(x->Sis_showing_by_scr2_doc2_prj2) o << " (is-showing-by scr2 doc2 prj2)"; else o<< " (not (is-showing-by scr2 doc2 prj2))"; 
	if(x->Sis_showing_by_scr2_doc3_prj2) o << " (is-showing-by scr2 doc3 prj2)"; else o<< " (not (is-showing-by scr2 doc3 prj2))"; 
	if(x->Sis_showing_by_scr3_doc1_prj3) o << " (is-showing-by scr3 doc1 prj3)"; else o<< " (not (is-showing-by scr3 doc1 prj3))"; 
	if(x->Sis_showing_by_scr3_doc2_prj3) o << " (is-showing-by scr3 doc2 prj3)"; else o<< " (not (is-showing-by scr3 doc2 prj3))"; 
	if(x->Sis_showing_by_scr3_doc3_prj3) o << " (is-showing-by scr3 doc3 prj3)"; else o<< " (not (is-showing-by scr3 doc3 prj3))"; 
	if(x->Sis_showing_scr1_doc1) o << " (is-showing scr1 doc1)"; else o<< " (not (is-showing scr1 doc1))"; 
	if(x->Sis_showing_scr1_doc2) o << " (is-showing scr1 doc2)"; else o<< " (not (is-showing scr1 doc2))"; 
	if(x->Sis_showing_scr1_doc3) o << " (is-showing scr1 doc3)"; else o<< " (not (is-showing scr1 doc3))"; 
	if(x->Sis_showing_scr2_doc1) o << " (is-showing scr2 doc1)"; else o<< " (not (is-showing scr2 doc1))"; 
	if(x->Sis_showing_scr2_doc2) o << " (is-showing scr2 doc2)"; else o<< " (not (is-showing scr2 doc2))"; 
	if(x->Sis_showing_scr2_doc3) o << " (is-showing scr2 doc3)"; else o<< " (not (is-showing scr2 doc3))"; 
	if(x->Sis_showing_scr3_doc1) o << " (is-showing scr3 doc1)"; else o<< " (not (is-showing scr3 doc1))"; 
	if(x->Sis_showing_scr3_doc2) o << " (is-showing scr3 doc2)"; else o<< " (not (is-showing scr3 doc2))"; 
	if(x->Sis_showing_scr3_doc3) o << " (is-showing scr3 doc3)"; else o<< " (not (is-showing scr3 doc3))"; 
	if(x->Scan_see_alice_doc1) o << " (can-see alice doc1)"; else o<< " (not (can-see alice doc1))"; 
	if(x->Scan_see_alice_doc2) o << " (can-see alice doc2)"; else o<< " (not (can-see alice doc2))"; 
	if(x->Scan_see_alice_doc3) o << " (can-see alice doc3)"; else o<< " (not (can-see alice doc3))"; 
	if(x->Scan_see_bob_doc1) o << " (can-see bob doc1)"; else o<< " (not (can-see bob doc1))"; 
	if(x->Scan_see_charlie_doc3) o << " (can-see charlie doc3)"; else o<< " (not (can-see charlie doc3))"; 
	if(x->Sis_open_o1) o << " (is-open o1)"; else o<< " (not (is-open o1))"; 
	if(x->Sis_open_o2) o << " (is-open o2)"; else o<< " (not (is-open o2))"; 
	if(x->Sis_open_o3) o << " (is-open o3)"; else o<< " (not (is-open o3))"; 
	if(x->Sis_switched_i1_o1) o << " (is-switched i1 o1)"; else o<< " (not (is-switched i1 o1))"; 
	if(x->Sis_switched_i1_o2) o << " (is-switched i1 o2)"; else o<< " (not (is-switched i1 o2))"; 
	if(x->Sis_switched_i1_o3) o << " (is-switched i1 o3)"; else o<< " (not (is-switched i1 o3))"; 
	if(x->Sis_switched_i2_o1) o << " (is-switched i2 o1)"; else o<< " (not (is-switched i2 o1))"; 
	if(x->Sis_switched_i2_o2) o << " (is-switched i2 o2)"; else o<< " (not (is-switched i2 o2))"; 
	if(x->Sis_switched_i2_o3) o << " (is-switched i2 o3)"; else o<< " (not (is-switched i2 o3))"; 
	if(x->Sis_switched_i3_o1) o << " (is-switched i3 o1)"; else o<< " (not (is-switched i3 o1))"; 
	if(x->Sis_switched_i3_o2) o << " (is-switched i3 o2)"; else o<< " (not (is-switched i3 o2))"; 
	if(x->Sis_switched_i3_o3) o << " (is-switched i3 o3)"; else o<< " (not (is-switched i3 o3))"; 
	return o;
}
void writeState(std::ostream &o, StatePtr x, double wt) {
	char buf[16 + 3*0 + 0]; char *s = (char*) buf;
	if (std::isinf(wt)) strcpy(s, "inf"), s += 3; else s += sprintf(s, "%g", wt);
	*s = ' '; s++; *s =x->Sis_active_nb1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_active_nb2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_active_nb3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_active_prj1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_active_prj2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_active_prj3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_active_scr1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_active_scr2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_active_scr3 + '0'; s++;
	*s = ' '; s++; *s =x->Sin_use_nb1 + '0'; s++;
	*s = ' '; s++; *s =x->Sin_use_nb2 + '0'; s++;
	*s = ' '; s++; *s =x->Sin_use_nb3 + '0'; s++;
	*s = ' '; s++; *s =x->Sin_use_prj1 + '0'; s++;
	*s = ' '; s++; *s =x->Sin_use_prj2 + '0'; s++;
	*s = ' '; s++; *s =x->Sin_use_prj3 + '0'; s++;
	*s = ' '; s++; *s =x->Sin_use_scr1 + '0'; s++;
	*s = ' '; s++; *s =x->Sin_use_scr2 + '0'; s++;
	*s = ' '; s++; *s =x->Sin_use_scr3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_alice_attention_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_bob_attention_bob + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_charlie_attention_charlie + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_nb1_vga_out_port_crossbar + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_nb1_vga_out_signal_prj1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_nb1_vga_out_signal_prj2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_nb1_vga_out_signal_prj3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_nb2_vga_out_port_crossbar + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_nb2_vga_out_signal_prj1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_nb2_vga_out_signal_prj2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_nb2_vga_out_signal_prj3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_nb3_vga_out_port_crossbar + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_nb3_vga_out_signal_prj1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_nb3_vga_out_signal_prj2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_nb3_vga_out_signal_prj3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_prj1_vga_in_port_prj1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_prj1_vga_in_port_crossbar + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_prj1_picture_scr1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_prj2_vga_in_port_prj2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_prj2_vga_in_port_crossbar + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_prj2_picture_scr2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_prj3_vga_in_port_prj3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_prj3_vga_in_port_crossbar + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_prj3_picture_scr3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_scr1_picture_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_scr1_picture_charlie + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_scr2_picture_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_scr2_picture_bob + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_scr2_picture_charlie + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_scr3_picture_alice + '0'; s++;
	*s = ' '; s++; *s =x->Sis_locked_by_scr3_picture_bob + '0'; s++;
	*s = ' '; s++; *s =x->Sis_sending_nb1_doc1_vga_out_signal + '0'; s++;
	*s = ' '; s++; *s =x->Sis_sending_nb2_doc2_vga_out_signal + '0'; s++;
	*s = ' '; s++; *s =x->Sis_sending_nb3_doc3_vga_out_signal + '0'; s++;
	*s = ' '; s++; *s =x->Sis_connected_nb1_vga_out_port_prj1_vga_in_port + '0'; s++;
	*s = ' '; s++; *s =x->Sis_connected_nb1_vga_out_port_prj2_vga_in_port + '0'; s++;
	*s = ' '; s++; *s =x->Sis_connected_nb1_vga_out_port_prj3_vga_in_port + '0'; s++;
	*s = ' '; s++; *s =x->Sis_connected_nb2_vga_out_port_prj1_vga_in_port + '0'; s++;
	*s = ' '; s++; *s =x->Sis_connected_nb2_vga_out_port_prj2_vga_in_port + '0'; s++;
	*s = ' '; s++; *s =x->Sis_connected_nb2_vga_out_port_prj3_vga_in_port + '0'; s++;
	*s = ' '; s++; *s =x->Sis_connected_nb3_vga_out_port_prj1_vga_in_port + '0'; s++;
	*s = ' '; s++; *s =x->Sis_connected_nb3_vga_out_port_prj2_vga_in_port + '0'; s++;
	*s = ' '; s++; *s =x->Sis_connected_nb3_vga_out_port_prj3_vga_in_port + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_by_prj1_doc1_nb1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_by_prj1_doc2_nb2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_by_prj1_doc3_nb3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_by_prj2_doc1_nb1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_by_prj2_doc2_nb2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_by_prj2_doc3_nb3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_by_prj3_doc1_nb1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_by_prj3_doc2_nb2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_by_prj3_doc3_nb3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_prj1_doc1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_prj1_doc2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_prj1_doc3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_prj2_doc1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_prj2_doc2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_prj2_doc3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_prj3_doc1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_prj3_doc2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_projecting_prj3_doc3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_by_scr1_doc1_prj1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_by_scr1_doc2_prj1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_by_scr1_doc3_prj1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_by_scr2_doc1_prj2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_by_scr2_doc2_prj2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_by_scr2_doc3_prj2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_by_scr3_doc1_prj3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_by_scr3_doc2_prj3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_by_scr3_doc3_prj3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_scr1_doc1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_scr1_doc2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_scr1_doc3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_scr2_doc1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_scr2_doc2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_scr2_doc3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_scr3_doc1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_scr3_doc2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_showing_scr3_doc3 + '0'; s++;
	*s = ' '; s++; *s =x->Scan_see_alice_doc1 + '0'; s++;
	*s = ' '; s++; *s =x->Scan_see_alice_doc2 + '0'; s++;
	*s = ' '; s++; *s =x->Scan_see_alice_doc3 + '0'; s++;
	*s = ' '; s++; *s =x->Scan_see_bob_doc1 + '0'; s++;
	*s = ' '; s++; *s =x->Scan_see_charlie_doc3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_open_o1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_open_o2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_open_o3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_switched_i1_o1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_switched_i1_o2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_switched_i1_o3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_switched_i2_o1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_switched_i2_o2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_switched_i2_o3 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_switched_i3_o1 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_switched_i3_o2 + '0'; s++;
	*s = ' '; s++; *s =x->Sis_switched_i3_o3 + '0'; s++;
	*s = '\n'; *(++s) = '\0'; o << (char*) buf;
}
bool readState(std::istream &i, StateRec *x, double &wt) {
	char buf[4096]; char *s = (char*) buf;
	do i.getline(s, 4096); while (i.gcount() < 2 && i.good());
	if (!i.good()) return false;
	if (!memcmp(s, "inf", 3)) { wt = infinity, s+=3; } else wt = strtod(s, &s);
	char c;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_active_nb1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_active_nb2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_active_nb3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_active_prj1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_active_prj2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_active_prj3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_active_scr1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_active_scr2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_active_scr3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sin_use_nb1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sin_use_nb2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sin_use_nb3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sin_use_prj1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sin_use_prj2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sin_use_prj3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sin_use_scr1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sin_use_scr2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sin_use_scr3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_alice_attention_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_bob_attention_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_charlie_attention_charlie = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_nb1_vga_out_port_crossbar = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_nb1_vga_out_signal_prj1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_nb1_vga_out_signal_prj2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_nb1_vga_out_signal_prj3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_nb2_vga_out_port_crossbar = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_nb2_vga_out_signal_prj1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_nb2_vga_out_signal_prj2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_nb2_vga_out_signal_prj3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_nb3_vga_out_port_crossbar = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_nb3_vga_out_signal_prj1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_nb3_vga_out_signal_prj2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_nb3_vga_out_signal_prj3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_prj1_vga_in_port_prj1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_prj1_vga_in_port_crossbar = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_prj1_picture_scr1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_prj2_vga_in_port_prj2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_prj2_vga_in_port_crossbar = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_prj2_picture_scr2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_prj3_vga_in_port_prj3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_prj3_vga_in_port_crossbar = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_prj3_picture_scr3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_scr1_picture_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_scr1_picture_charlie = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_scr2_picture_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_scr2_picture_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_scr2_picture_charlie = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_scr3_picture_alice = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_locked_by_scr3_picture_bob = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_sending_nb1_doc1_vga_out_signal = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_sending_nb2_doc2_vga_out_signal = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_sending_nb3_doc3_vga_out_signal = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_connected_nb1_vga_out_port_prj1_vga_in_port = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_connected_nb1_vga_out_port_prj2_vga_in_port = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_connected_nb1_vga_out_port_prj3_vga_in_port = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_connected_nb2_vga_out_port_prj1_vga_in_port = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_connected_nb2_vga_out_port_prj2_vga_in_port = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_connected_nb2_vga_out_port_prj3_vga_in_port = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_connected_nb3_vga_out_port_prj1_vga_in_port = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_connected_nb3_vga_out_port_prj2_vga_in_port = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_connected_nb3_vga_out_port_prj3_vga_in_port = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_by_prj1_doc1_nb1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_by_prj1_doc2_nb2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_by_prj1_doc3_nb3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_by_prj2_doc1_nb1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_by_prj2_doc2_nb2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_by_prj2_doc3_nb3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_by_prj3_doc1_nb1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_by_prj3_doc2_nb2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_by_prj3_doc3_nb3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_prj1_doc1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_prj1_doc2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_prj1_doc3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_prj2_doc1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_prj2_doc2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_prj2_doc3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_prj3_doc1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_prj3_doc2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_projecting_prj3_doc3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_by_scr1_doc1_prj1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_by_scr1_doc2_prj1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_by_scr1_doc3_prj1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_by_scr2_doc1_prj2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_by_scr2_doc2_prj2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_by_scr2_doc3_prj2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_by_scr3_doc1_prj3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_by_scr3_doc2_prj3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_by_scr3_doc3_prj3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_scr1_doc1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_scr1_doc2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_scr1_doc3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_scr2_doc1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_scr2_doc2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_scr2_doc3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_scr3_doc1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_scr3_doc2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_showing_scr3_doc3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Scan_see_alice_doc1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Scan_see_alice_doc2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Scan_see_alice_doc3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Scan_see_bob_doc1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Scan_see_charlie_doc3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_open_o1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_open_o2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_open_o3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_switched_i1_o1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_switched_i1_o2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_switched_i1_o3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_switched_i2_o1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_switched_i2_o2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_switched_i2_o3 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_switched_i3_o1 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_switched_i3_o2 = c, s++; else return false;
	while (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->Sis_switched_i3_o3 = c, s++; else return false;
	return true;
}
void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr) { // rt: Recency Vector (ACT-R Heuristic); t: ist the global time
	if (prev->Sis_active_nb1 ^ curr->Sis_active_nb1) rT[0] = t;
	if (prev->Sis_active_nb2 ^ curr->Sis_active_nb2) rT[1] = t;
	if (prev->Sis_active_nb3 ^ curr->Sis_active_nb3) rT[2] = t;
	if (prev->Sis_active_prj1 ^ curr->Sis_active_prj1) rT[3] = t;
	if (prev->Sis_active_prj2 ^ curr->Sis_active_prj2) rT[4] = t;
	if (prev->Sis_active_prj3 ^ curr->Sis_active_prj3) rT[5] = t;
	if (prev->Sis_active_scr1 ^ curr->Sis_active_scr1) rT[6] = t;
	if (prev->Sis_active_scr2 ^ curr->Sis_active_scr2) rT[7] = t;
	if (prev->Sis_active_scr3 ^ curr->Sis_active_scr3) rT[8] = t;
	if (prev->Sin_use_nb1 ^ curr->Sin_use_nb1) rT[9] = t;
	if (prev->Sin_use_nb2 ^ curr->Sin_use_nb2) rT[10] = t;
	if (prev->Sin_use_nb3 ^ curr->Sin_use_nb3) rT[11] = t;
	if (prev->Sin_use_prj1 ^ curr->Sin_use_prj1) rT[12] = t;
	if (prev->Sin_use_prj2 ^ curr->Sin_use_prj2) rT[13] = t;
	if (prev->Sin_use_prj3 ^ curr->Sin_use_prj3) rT[14] = t;
	if (prev->Sin_use_scr1 ^ curr->Sin_use_scr1) rT[15] = t;
	if (prev->Sin_use_scr2 ^ curr->Sin_use_scr2) rT[16] = t;
	if (prev->Sin_use_scr3 ^ curr->Sin_use_scr3) rT[17] = t;
	if (prev->Sis_locked_by_alice_attention_alice ^ curr->Sis_locked_by_alice_attention_alice) rT[18] = t;
	if (prev->Sis_locked_by_bob_attention_bob ^ curr->Sis_locked_by_bob_attention_bob) rT[19] = t;
	if (prev->Sis_locked_by_charlie_attention_charlie ^ curr->Sis_locked_by_charlie_attention_charlie) rT[20] = t;
	if (prev->Sis_locked_by_nb1_vga_out_port_crossbar ^ curr->Sis_locked_by_nb1_vga_out_port_crossbar) rT[21] = t;
	if (prev->Sis_locked_by_nb1_vga_out_signal_prj1 ^ curr->Sis_locked_by_nb1_vga_out_signal_prj1) rT[22] = t;
	if (prev->Sis_locked_by_nb1_vga_out_signal_prj2 ^ curr->Sis_locked_by_nb1_vga_out_signal_prj2) rT[23] = t;
	if (prev->Sis_locked_by_nb1_vga_out_signal_prj3 ^ curr->Sis_locked_by_nb1_vga_out_signal_prj3) rT[24] = t;
	if (prev->Sis_locked_by_nb2_vga_out_port_crossbar ^ curr->Sis_locked_by_nb2_vga_out_port_crossbar) rT[25] = t;
	if (prev->Sis_locked_by_nb2_vga_out_signal_prj1 ^ curr->Sis_locked_by_nb2_vga_out_signal_prj1) rT[26] = t;
	if (prev->Sis_locked_by_nb2_vga_out_signal_prj2 ^ curr->Sis_locked_by_nb2_vga_out_signal_prj2) rT[27] = t;
	if (prev->Sis_locked_by_nb2_vga_out_signal_prj3 ^ curr->Sis_locked_by_nb2_vga_out_signal_prj3) rT[28] = t;
	if (prev->Sis_locked_by_nb3_vga_out_port_crossbar ^ curr->Sis_locked_by_nb3_vga_out_port_crossbar) rT[29] = t;
	if (prev->Sis_locked_by_nb3_vga_out_signal_prj1 ^ curr->Sis_locked_by_nb3_vga_out_signal_prj1) rT[30] = t;
	if (prev->Sis_locked_by_nb3_vga_out_signal_prj2 ^ curr->Sis_locked_by_nb3_vga_out_signal_prj2) rT[31] = t;
	if (prev->Sis_locked_by_nb3_vga_out_signal_prj3 ^ curr->Sis_locked_by_nb3_vga_out_signal_prj3) rT[32] = t;
	if (prev->Sis_locked_by_prj1_vga_in_port_prj1 ^ curr->Sis_locked_by_prj1_vga_in_port_prj1) rT[33] = t;
	if (prev->Sis_locked_by_prj1_vga_in_port_crossbar ^ curr->Sis_locked_by_prj1_vga_in_port_crossbar) rT[34] = t;
	if (prev->Sis_locked_by_prj1_picture_scr1 ^ curr->Sis_locked_by_prj1_picture_scr1) rT[35] = t;
	if (prev->Sis_locked_by_prj2_vga_in_port_prj2 ^ curr->Sis_locked_by_prj2_vga_in_port_prj2) rT[36] = t;
	if (prev->Sis_locked_by_prj2_vga_in_port_crossbar ^ curr->Sis_locked_by_prj2_vga_in_port_crossbar) rT[37] = t;
	if (prev->Sis_locked_by_prj2_picture_scr2 ^ curr->Sis_locked_by_prj2_picture_scr2) rT[38] = t;
	if (prev->Sis_locked_by_prj3_vga_in_port_prj3 ^ curr->Sis_locked_by_prj3_vga_in_port_prj3) rT[39] = t;
	if (prev->Sis_locked_by_prj3_vga_in_port_crossbar ^ curr->Sis_locked_by_prj3_vga_in_port_crossbar) rT[40] = t;
	if (prev->Sis_locked_by_prj3_picture_scr3 ^ curr->Sis_locked_by_prj3_picture_scr3) rT[41] = t;
	if (prev->Sis_locked_by_scr1_picture_alice ^ curr->Sis_locked_by_scr1_picture_alice) rT[42] = t;
	if (prev->Sis_locked_by_scr1_picture_charlie ^ curr->Sis_locked_by_scr1_picture_charlie) rT[43] = t;
	if (prev->Sis_locked_by_scr2_picture_alice ^ curr->Sis_locked_by_scr2_picture_alice) rT[44] = t;
	if (prev->Sis_locked_by_scr2_picture_bob ^ curr->Sis_locked_by_scr2_picture_bob) rT[45] = t;
	if (prev->Sis_locked_by_scr2_picture_charlie ^ curr->Sis_locked_by_scr2_picture_charlie) rT[46] = t;
	if (prev->Sis_locked_by_scr3_picture_alice ^ curr->Sis_locked_by_scr3_picture_alice) rT[47] = t;
	if (prev->Sis_locked_by_scr3_picture_bob ^ curr->Sis_locked_by_scr3_picture_bob) rT[48] = t;
	if (prev->Sis_sending_nb1_doc1_vga_out_signal ^ curr->Sis_sending_nb1_doc1_vga_out_signal) rT[49] = t;
	if (prev->Sis_sending_nb2_doc2_vga_out_signal ^ curr->Sis_sending_nb2_doc2_vga_out_signal) rT[50] = t;
	if (prev->Sis_sending_nb3_doc3_vga_out_signal ^ curr->Sis_sending_nb3_doc3_vga_out_signal) rT[51] = t;
	if (prev->Sis_connected_nb1_vga_out_port_prj1_vga_in_port ^ curr->Sis_connected_nb1_vga_out_port_prj1_vga_in_port) rT[52] = t;
	if (prev->Sis_connected_nb1_vga_out_port_prj2_vga_in_port ^ curr->Sis_connected_nb1_vga_out_port_prj2_vga_in_port) rT[53] = t;
	if (prev->Sis_connected_nb1_vga_out_port_prj3_vga_in_port ^ curr->Sis_connected_nb1_vga_out_port_prj3_vga_in_port) rT[54] = t;
	if (prev->Sis_connected_nb2_vga_out_port_prj1_vga_in_port ^ curr->Sis_connected_nb2_vga_out_port_prj1_vga_in_port) rT[55] = t;
	if (prev->Sis_connected_nb2_vga_out_port_prj2_vga_in_port ^ curr->Sis_connected_nb2_vga_out_port_prj2_vga_in_port) rT[56] = t;
	if (prev->Sis_connected_nb2_vga_out_port_prj3_vga_in_port ^ curr->Sis_connected_nb2_vga_out_port_prj3_vga_in_port) rT[57] = t;
	if (prev->Sis_connected_nb3_vga_out_port_prj1_vga_in_port ^ curr->Sis_connected_nb3_vga_out_port_prj1_vga_in_port) rT[58] = t;
	if (prev->Sis_connected_nb3_vga_out_port_prj2_vga_in_port ^ curr->Sis_connected_nb3_vga_out_port_prj2_vga_in_port) rT[59] = t;
	if (prev->Sis_connected_nb3_vga_out_port_prj3_vga_in_port ^ curr->Sis_connected_nb3_vga_out_port_prj3_vga_in_port) rT[60] = t;
	if (prev->Sis_projecting_by_prj1_doc1_nb1 ^ curr->Sis_projecting_by_prj1_doc1_nb1) rT[61] = t;
	if (prev->Sis_projecting_by_prj1_doc2_nb2 ^ curr->Sis_projecting_by_prj1_doc2_nb2) rT[62] = t;
	if (prev->Sis_projecting_by_prj1_doc3_nb3 ^ curr->Sis_projecting_by_prj1_doc3_nb3) rT[63] = t;
	if (prev->Sis_projecting_by_prj2_doc1_nb1 ^ curr->Sis_projecting_by_prj2_doc1_nb1) rT[64] = t;
	if (prev->Sis_projecting_by_prj2_doc2_nb2 ^ curr->Sis_projecting_by_prj2_doc2_nb2) rT[65] = t;
	if (prev->Sis_projecting_by_prj2_doc3_nb3 ^ curr->Sis_projecting_by_prj2_doc3_nb3) rT[66] = t;
	if (prev->Sis_projecting_by_prj3_doc1_nb1 ^ curr->Sis_projecting_by_prj3_doc1_nb1) rT[67] = t;
	if (prev->Sis_projecting_by_prj3_doc2_nb2 ^ curr->Sis_projecting_by_prj3_doc2_nb2) rT[68] = t;
	if (prev->Sis_projecting_by_prj3_doc3_nb3 ^ curr->Sis_projecting_by_prj3_doc3_nb3) rT[69] = t;
	if (prev->Sis_projecting_prj1_doc1 ^ curr->Sis_projecting_prj1_doc1) rT[70] = t;
	if (prev->Sis_projecting_prj1_doc2 ^ curr->Sis_projecting_prj1_doc2) rT[71] = t;
	if (prev->Sis_projecting_prj1_doc3 ^ curr->Sis_projecting_prj1_doc3) rT[72] = t;
	if (prev->Sis_projecting_prj2_doc1 ^ curr->Sis_projecting_prj2_doc1) rT[73] = t;
	if (prev->Sis_projecting_prj2_doc2 ^ curr->Sis_projecting_prj2_doc2) rT[74] = t;
	if (prev->Sis_projecting_prj2_doc3 ^ curr->Sis_projecting_prj2_doc3) rT[75] = t;
	if (prev->Sis_projecting_prj3_doc1 ^ curr->Sis_projecting_prj3_doc1) rT[76] = t;
	if (prev->Sis_projecting_prj3_doc2 ^ curr->Sis_projecting_prj3_doc2) rT[77] = t;
	if (prev->Sis_projecting_prj3_doc3 ^ curr->Sis_projecting_prj3_doc3) rT[78] = t;
	if (prev->Sis_showing_by_scr1_doc1_prj1 ^ curr->Sis_showing_by_scr1_doc1_prj1) rT[79] = t;
	if (prev->Sis_showing_by_scr1_doc2_prj1 ^ curr->Sis_showing_by_scr1_doc2_prj1) rT[80] = t;
	if (prev->Sis_showing_by_scr1_doc3_prj1 ^ curr->Sis_showing_by_scr1_doc3_prj1) rT[81] = t;
	if (prev->Sis_showing_by_scr2_doc1_prj2 ^ curr->Sis_showing_by_scr2_doc1_prj2) rT[82] = t;
	if (prev->Sis_showing_by_scr2_doc2_prj2 ^ curr->Sis_showing_by_scr2_doc2_prj2) rT[83] = t;
	if (prev->Sis_showing_by_scr2_doc3_prj2 ^ curr->Sis_showing_by_scr2_doc3_prj2) rT[84] = t;
	if (prev->Sis_showing_by_scr3_doc1_prj3 ^ curr->Sis_showing_by_scr3_doc1_prj3) rT[85] = t;
	if (prev->Sis_showing_by_scr3_doc2_prj3 ^ curr->Sis_showing_by_scr3_doc2_prj3) rT[86] = t;
	if (prev->Sis_showing_by_scr3_doc3_prj3 ^ curr->Sis_showing_by_scr3_doc3_prj3) rT[87] = t;
	if (prev->Sis_showing_scr1_doc1 ^ curr->Sis_showing_scr1_doc1) rT[88] = t;
	if (prev->Sis_showing_scr1_doc2 ^ curr->Sis_showing_scr1_doc2) rT[89] = t;
	if (prev->Sis_showing_scr1_doc3 ^ curr->Sis_showing_scr1_doc3) rT[90] = t;
	if (prev->Sis_showing_scr2_doc1 ^ curr->Sis_showing_scr2_doc1) rT[91] = t;
	if (prev->Sis_showing_scr2_doc2 ^ curr->Sis_showing_scr2_doc2) rT[92] = t;
	if (prev->Sis_showing_scr2_doc3 ^ curr->Sis_showing_scr2_doc3) rT[93] = t;
	if (prev->Sis_showing_scr3_doc1 ^ curr->Sis_showing_scr3_doc1) rT[94] = t;
	if (prev->Sis_showing_scr3_doc2 ^ curr->Sis_showing_scr3_doc2) rT[95] = t;
	if (prev->Sis_showing_scr3_doc3 ^ curr->Sis_showing_scr3_doc3) rT[96] = t;
	if (prev->Scan_see_alice_doc1 ^ curr->Scan_see_alice_doc1) rT[97] = t;
	if (prev->Scan_see_alice_doc2 ^ curr->Scan_see_alice_doc2) rT[98] = t;
	if (prev->Scan_see_alice_doc3 ^ curr->Scan_see_alice_doc3) rT[99] = t;
	if (prev->Scan_see_bob_doc1 ^ curr->Scan_see_bob_doc1) rT[100] = t;
	if (prev->Scan_see_charlie_doc3 ^ curr->Scan_see_charlie_doc3) rT[101] = t;
	if (prev->Sis_open_o1 ^ curr->Sis_open_o1) rT[102] = t;
	if (prev->Sis_open_o2 ^ curr->Sis_open_o2) rT[103] = t;
	if (prev->Sis_open_o3 ^ curr->Sis_open_o3) rT[104] = t;
	if (prev->Sis_switched_i1_o1 ^ curr->Sis_switched_i1_o1) rT[105] = t;
	if (prev->Sis_switched_i1_o2 ^ curr->Sis_switched_i1_o2) rT[106] = t;
	if (prev->Sis_switched_i1_o3 ^ curr->Sis_switched_i1_o3) rT[107] = t;
	if (prev->Sis_switched_i2_o1 ^ curr->Sis_switched_i2_o1) rT[108] = t;
	if (prev->Sis_switched_i2_o2 ^ curr->Sis_switched_i2_o2) rT[109] = t;
	if (prev->Sis_switched_i2_o3 ^ curr->Sis_switched_i2_o3) rT[110] = t;
	if (prev->Sis_switched_i3_o1 ^ curr->Sis_switched_i3_o1) rT[111] = t;
	if (prev->Sis_switched_i3_o2 ^ curr->Sis_switched_i3_o2) rT[112] = t;
	if (prev->Sis_switched_i3_o3 ^ curr->Sis_switched_i3_o3) rT[113] = t;
}
double getRefract(StatePtr a1, StatePtr a2) { // compare two action preconditions and returns the rerfractoriness 
	double r=0; int n=0;
	if (a1->Sis_active_nb1) {n++; if (a2->Sis_active_nb1) r++;}
	if (a1->Sis_active_nb2) {n++; if (a2->Sis_active_nb2) r++;}
	if (a1->Sis_active_nb3) {n++; if (a2->Sis_active_nb3) r++;}
	if (a1->Sis_active_prj1) {n++; if (a2->Sis_active_prj1) r++;}
	if (a1->Sis_active_prj2) {n++; if (a2->Sis_active_prj2) r++;}
	if (a1->Sis_active_prj3) {n++; if (a2->Sis_active_prj3) r++;}
	if (a1->Sis_active_scr1) {n++; if (a2->Sis_active_scr1) r++;}
	if (a1->Sis_active_scr2) {n++; if (a2->Sis_active_scr2) r++;}
	if (a1->Sis_active_scr3) {n++; if (a2->Sis_active_scr3) r++;}
	if (a1->Sin_use_nb1) {n++; if (a2->Sin_use_nb1) r++;}
	if (a1->Sin_use_nb2) {n++; if (a2->Sin_use_nb2) r++;}
	if (a1->Sin_use_nb3) {n++; if (a2->Sin_use_nb3) r++;}
	if (a1->Sin_use_prj1) {n++; if (a2->Sin_use_prj1) r++;}
	if (a1->Sin_use_prj2) {n++; if (a2->Sin_use_prj2) r++;}
	if (a1->Sin_use_prj3) {n++; if (a2->Sin_use_prj3) r++;}
	if (a1->Sin_use_scr1) {n++; if (a2->Sin_use_scr1) r++;}
	if (a1->Sin_use_scr2) {n++; if (a2->Sin_use_scr2) r++;}
	if (a1->Sin_use_scr3) {n++; if (a2->Sin_use_scr3) r++;}
	if (a1->Sis_locked_by_alice_attention_alice) {n++; if (a2->Sis_locked_by_alice_attention_alice) r++;}
	if (a1->Sis_locked_by_bob_attention_bob) {n++; if (a2->Sis_locked_by_bob_attention_bob) r++;}
	if (a1->Sis_locked_by_charlie_attention_charlie) {n++; if (a2->Sis_locked_by_charlie_attention_charlie) r++;}
	if (a1->Sis_locked_by_nb1_vga_out_port_crossbar) {n++; if (a2->Sis_locked_by_nb1_vga_out_port_crossbar) r++;}
	if (a1->Sis_locked_by_nb1_vga_out_signal_prj1) {n++; if (a2->Sis_locked_by_nb1_vga_out_signal_prj1) r++;}
	if (a1->Sis_locked_by_nb1_vga_out_signal_prj2) {n++; if (a2->Sis_locked_by_nb1_vga_out_signal_prj2) r++;}
	if (a1->Sis_locked_by_nb1_vga_out_signal_prj3) {n++; if (a2->Sis_locked_by_nb1_vga_out_signal_prj3) r++;}
	if (a1->Sis_locked_by_nb2_vga_out_port_crossbar) {n++; if (a2->Sis_locked_by_nb2_vga_out_port_crossbar) r++;}
	if (a1->Sis_locked_by_nb2_vga_out_signal_prj1) {n++; if (a2->Sis_locked_by_nb2_vga_out_signal_prj1) r++;}
	if (a1->Sis_locked_by_nb2_vga_out_signal_prj2) {n++; if (a2->Sis_locked_by_nb2_vga_out_signal_prj2) r++;}
	if (a1->Sis_locked_by_nb2_vga_out_signal_prj3) {n++; if (a2->Sis_locked_by_nb2_vga_out_signal_prj3) r++;}
	if (a1->Sis_locked_by_nb3_vga_out_port_crossbar) {n++; if (a2->Sis_locked_by_nb3_vga_out_port_crossbar) r++;}
	if (a1->Sis_locked_by_nb3_vga_out_signal_prj1) {n++; if (a2->Sis_locked_by_nb3_vga_out_signal_prj1) r++;}
	if (a1->Sis_locked_by_nb3_vga_out_signal_prj2) {n++; if (a2->Sis_locked_by_nb3_vga_out_signal_prj2) r++;}
	if (a1->Sis_locked_by_nb3_vga_out_signal_prj3) {n++; if (a2->Sis_locked_by_nb3_vga_out_signal_prj3) r++;}
	if (a1->Sis_locked_by_prj1_vga_in_port_prj1) {n++; if (a2->Sis_locked_by_prj1_vga_in_port_prj1) r++;}
	if (a1->Sis_locked_by_prj1_vga_in_port_crossbar) {n++; if (a2->Sis_locked_by_prj1_vga_in_port_crossbar) r++;}
	if (a1->Sis_locked_by_prj1_picture_scr1) {n++; if (a2->Sis_locked_by_prj1_picture_scr1) r++;}
	if (a1->Sis_locked_by_prj2_vga_in_port_prj2) {n++; if (a2->Sis_locked_by_prj2_vga_in_port_prj2) r++;}
	if (a1->Sis_locked_by_prj2_vga_in_port_crossbar) {n++; if (a2->Sis_locked_by_prj2_vga_in_port_crossbar) r++;}
	if (a1->Sis_locked_by_prj2_picture_scr2) {n++; if (a2->Sis_locked_by_prj2_picture_scr2) r++;}
	if (a1->Sis_locked_by_prj3_vga_in_port_prj3) {n++; if (a2->Sis_locked_by_prj3_vga_in_port_prj3) r++;}
	if (a1->Sis_locked_by_prj3_vga_in_port_crossbar) {n++; if (a2->Sis_locked_by_prj3_vga_in_port_crossbar) r++;}
	if (a1->Sis_locked_by_prj3_picture_scr3) {n++; if (a2->Sis_locked_by_prj3_picture_scr3) r++;}
	if (a1->Sis_locked_by_scr1_picture_alice) {n++; if (a2->Sis_locked_by_scr1_picture_alice) r++;}
	if (a1->Sis_locked_by_scr1_picture_charlie) {n++; if (a2->Sis_locked_by_scr1_picture_charlie) r++;}
	if (a1->Sis_locked_by_scr2_picture_alice) {n++; if (a2->Sis_locked_by_scr2_picture_alice) r++;}
	if (a1->Sis_locked_by_scr2_picture_bob) {n++; if (a2->Sis_locked_by_scr2_picture_bob) r++;}
	if (a1->Sis_locked_by_scr2_picture_charlie) {n++; if (a2->Sis_locked_by_scr2_picture_charlie) r++;}
	if (a1->Sis_locked_by_scr3_picture_alice) {n++; if (a2->Sis_locked_by_scr3_picture_alice) r++;}
	if (a1->Sis_locked_by_scr3_picture_bob) {n++; if (a2->Sis_locked_by_scr3_picture_bob) r++;}
	if (a1->Sis_sending_nb1_doc1_vga_out_signal) {n++; if (a2->Sis_sending_nb1_doc1_vga_out_signal) r++;}
	if (a1->Sis_sending_nb2_doc2_vga_out_signal) {n++; if (a2->Sis_sending_nb2_doc2_vga_out_signal) r++;}
	if (a1->Sis_sending_nb3_doc3_vga_out_signal) {n++; if (a2->Sis_sending_nb3_doc3_vga_out_signal) r++;}
	if (a1->Sis_connected_nb1_vga_out_port_prj1_vga_in_port) {n++; if (a2->Sis_connected_nb1_vga_out_port_prj1_vga_in_port) r++;}
	if (a1->Sis_connected_nb1_vga_out_port_prj2_vga_in_port) {n++; if (a2->Sis_connected_nb1_vga_out_port_prj2_vga_in_port) r++;}
	if (a1->Sis_connected_nb1_vga_out_port_prj3_vga_in_port) {n++; if (a2->Sis_connected_nb1_vga_out_port_prj3_vga_in_port) r++;}
	if (a1->Sis_connected_nb2_vga_out_port_prj1_vga_in_port) {n++; if (a2->Sis_connected_nb2_vga_out_port_prj1_vga_in_port) r++;}
	if (a1->Sis_connected_nb2_vga_out_port_prj2_vga_in_port) {n++; if (a2->Sis_connected_nb2_vga_out_port_prj2_vga_in_port) r++;}
	if (a1->Sis_connected_nb2_vga_out_port_prj3_vga_in_port) {n++; if (a2->Sis_connected_nb2_vga_out_port_prj3_vga_in_port) r++;}
	if (a1->Sis_connected_nb3_vga_out_port_prj1_vga_in_port) {n++; if (a2->Sis_connected_nb3_vga_out_port_prj1_vga_in_port) r++;}
	if (a1->Sis_connected_nb3_vga_out_port_prj2_vga_in_port) {n++; if (a2->Sis_connected_nb3_vga_out_port_prj2_vga_in_port) r++;}
	if (a1->Sis_connected_nb3_vga_out_port_prj3_vga_in_port) {n++; if (a2->Sis_connected_nb3_vga_out_port_prj3_vga_in_port) r++;}
	if (a1->Sis_projecting_by_prj1_doc1_nb1) {n++; if (a2->Sis_projecting_by_prj1_doc1_nb1) r++;}
	if (a1->Sis_projecting_by_prj1_doc2_nb2) {n++; if (a2->Sis_projecting_by_prj1_doc2_nb2) r++;}
	if (a1->Sis_projecting_by_prj1_doc3_nb3) {n++; if (a2->Sis_projecting_by_prj1_doc3_nb3) r++;}
	if (a1->Sis_projecting_by_prj2_doc1_nb1) {n++; if (a2->Sis_projecting_by_prj2_doc1_nb1) r++;}
	if (a1->Sis_projecting_by_prj2_doc2_nb2) {n++; if (a2->Sis_projecting_by_prj2_doc2_nb2) r++;}
	if (a1->Sis_projecting_by_prj2_doc3_nb3) {n++; if (a2->Sis_projecting_by_prj2_doc3_nb3) r++;}
	if (a1->Sis_projecting_by_prj3_doc1_nb1) {n++; if (a2->Sis_projecting_by_prj3_doc1_nb1) r++;}
	if (a1->Sis_projecting_by_prj3_doc2_nb2) {n++; if (a2->Sis_projecting_by_prj3_doc2_nb2) r++;}
	if (a1->Sis_projecting_by_prj3_doc3_nb3) {n++; if (a2->Sis_projecting_by_prj3_doc3_nb3) r++;}
	if (a1->Sis_projecting_prj1_doc1) {n++; if (a2->Sis_projecting_prj1_doc1) r++;}
	if (a1->Sis_projecting_prj1_doc2) {n++; if (a2->Sis_projecting_prj1_doc2) r++;}
	if (a1->Sis_projecting_prj1_doc3) {n++; if (a2->Sis_projecting_prj1_doc3) r++;}
	if (a1->Sis_projecting_prj2_doc1) {n++; if (a2->Sis_projecting_prj2_doc1) r++;}
	if (a1->Sis_projecting_prj2_doc2) {n++; if (a2->Sis_projecting_prj2_doc2) r++;}
	if (a1->Sis_projecting_prj2_doc3) {n++; if (a2->Sis_projecting_prj2_doc3) r++;}
	if (a1->Sis_projecting_prj3_doc1) {n++; if (a2->Sis_projecting_prj3_doc1) r++;}
	if (a1->Sis_projecting_prj3_doc2) {n++; if (a2->Sis_projecting_prj3_doc2) r++;}
	if (a1->Sis_projecting_prj3_doc3) {n++; if (a2->Sis_projecting_prj3_doc3) r++;}
	if (a1->Sis_showing_by_scr1_doc1_prj1) {n++; if (a2->Sis_showing_by_scr1_doc1_prj1) r++;}
	if (a1->Sis_showing_by_scr1_doc2_prj1) {n++; if (a2->Sis_showing_by_scr1_doc2_prj1) r++;}
	if (a1->Sis_showing_by_scr1_doc3_prj1) {n++; if (a2->Sis_showing_by_scr1_doc3_prj1) r++;}
	if (a1->Sis_showing_by_scr2_doc1_prj2) {n++; if (a2->Sis_showing_by_scr2_doc1_prj2) r++;}
	if (a1->Sis_showing_by_scr2_doc2_prj2) {n++; if (a2->Sis_showing_by_scr2_doc2_prj2) r++;}
	if (a1->Sis_showing_by_scr2_doc3_prj2) {n++; if (a2->Sis_showing_by_scr2_doc3_prj2) r++;}
	if (a1->Sis_showing_by_scr3_doc1_prj3) {n++; if (a2->Sis_showing_by_scr3_doc1_prj3) r++;}
	if (a1->Sis_showing_by_scr3_doc2_prj3) {n++; if (a2->Sis_showing_by_scr3_doc2_prj3) r++;}
	if (a1->Sis_showing_by_scr3_doc3_prj3) {n++; if (a2->Sis_showing_by_scr3_doc3_prj3) r++;}
	if (a1->Sis_showing_scr1_doc1) {n++; if (a2->Sis_showing_scr1_doc1) r++;}
	if (a1->Sis_showing_scr1_doc2) {n++; if (a2->Sis_showing_scr1_doc2) r++;}
	if (a1->Sis_showing_scr1_doc3) {n++; if (a2->Sis_showing_scr1_doc3) r++;}
	if (a1->Sis_showing_scr2_doc1) {n++; if (a2->Sis_showing_scr2_doc1) r++;}
	if (a1->Sis_showing_scr2_doc2) {n++; if (a2->Sis_showing_scr2_doc2) r++;}
	if (a1->Sis_showing_scr2_doc3) {n++; if (a2->Sis_showing_scr2_doc3) r++;}
	if (a1->Sis_showing_scr3_doc1) {n++; if (a2->Sis_showing_scr3_doc1) r++;}
	if (a1->Sis_showing_scr3_doc2) {n++; if (a2->Sis_showing_scr3_doc2) r++;}
	if (a1->Sis_showing_scr3_doc3) {n++; if (a2->Sis_showing_scr3_doc3) r++;}
	if (a1->Scan_see_alice_doc1) {n++; if (a2->Scan_see_alice_doc1) r++;}
	if (a1->Scan_see_alice_doc2) {n++; if (a2->Scan_see_alice_doc2) r++;}
	if (a1->Scan_see_alice_doc3) {n++; if (a2->Scan_see_alice_doc3) r++;}
	if (a1->Scan_see_bob_doc1) {n++; if (a2->Scan_see_bob_doc1) r++;}
	if (a1->Scan_see_charlie_doc3) {n++; if (a2->Scan_see_charlie_doc3) r++;}
	if (a1->Sis_open_o1) {n++; if (a2->Sis_open_o1) r++;}
	if (a1->Sis_open_o2) {n++; if (a2->Sis_open_o2) r++;}
	if (a1->Sis_open_o3) {n++; if (a2->Sis_open_o3) r++;}
	if (a1->Sis_switched_i1_o1) {n++; if (a2->Sis_switched_i1_o1) r++;}
	if (a1->Sis_switched_i1_o2) {n++; if (a2->Sis_switched_i1_o2) r++;}
	if (a1->Sis_switched_i1_o3) {n++; if (a2->Sis_switched_i1_o3) r++;}
	if (a1->Sis_switched_i2_o1) {n++; if (a2->Sis_switched_i2_o1) r++;}
	if (a1->Sis_switched_i2_o2) {n++; if (a2->Sis_switched_i2_o2) r++;}
	if (a1->Sis_switched_i2_o3) {n++; if (a2->Sis_switched_i2_o3) r++;}
	if (a1->Sis_switched_i3_o1) {n++; if (a2->Sis_switched_i3_o1) r++;}
	if (a1->Sis_switched_i3_o2) {n++; if (a2->Sis_switched_i3_o2) r++;}
	if (a1->Sis_switched_i3_o3) {n++; if (a2->Sis_switched_i3_o3) r++;}
	return (n==0) ? 0 : r/n;
}

char const * const initialStateNames[] = {"r218pdev-simple"};
char const * const goalNames[] = {"r218pdev-simple"};

// no :non-repeating has been used
bool canFollowAfter(int next, int prev) { (void)next; (void)prev; return true; }

int const FinishedOpId = -3;
int const NoOpId = -2;
int const InitOpId = -1;
char const * const __actionNames[NOPS+3] = {
	"(FINISHED)",
	"(BLOCKED)",
	"(INITIALIZE)",
	"(activate nb1)", // (:action activate ?d - device)
	"(activate nb2)", // (:action activate ?d - device)
	"(activate nb3)", // (:action activate ?d - device)
	"(activate prj1)", // (:action activate ?d - device)
	"(activate prj2)", // (:action activate ?d - device)
	"(activate prj3)", // (:action activate ?d - device)
	"(activate scr1)", // (:action activate ?d - device)
	"(activate scr2)", // (:action activate ?d - device)
	"(activate scr3)", // (:action activate ?d - device)
	"(deactivate nb1)", // (:action deactivate ?d - device)
	"(deactivate nb2)", // (:action deactivate ?d - device)
	"(deactivate nb3)", // (:action deactivate ?d - device)
	"(deactivate prj1)", // (:action deactivate ?d - device)
	"(deactivate prj2)", // (:action deactivate ?d - device)
	"(deactivate prj3)", // (:action deactivate ?d - device)
	"(deactivate scr1)", // (:action deactivate ?d - device)
	"(deactivate scr2)", // (:action deactivate ?d - device)
	"(deactivate scr3)", // (:action deactivate ?d - device)
	"(begin-send-document nb1 doc1)", // (:action begin-send-document ?n - notebook ?d - document)
	"(begin-send-document nb2 doc2)", // (:action begin-send-document ?n - notebook ?d - document)
	"(begin-send-document nb3 doc3)", // (:action begin-send-document ?n - notebook ?d - document)
	"(end-send-document nb1 doc1)", // (:action end-send-document ?n - notebook ?d - document)
	"(end-send-document nb2 doc2)", // (:action end-send-document ?n - notebook ?d - document)
	"(end-send-document nb3 doc3)", // (:action end-send-document ?n - notebook ?d - document)
	"(begin-connect nb1 prj1 i1 o1)", // (:action begin-connect ?n - notebook ?p - projector ?ip - cb-in-port ?op - cb-out-port)
	"(begin-connect nb1 prj2 i1 o2)", // (:action begin-connect ?n - notebook ?p - projector ?ip - cb-in-port ?op - cb-out-port)
	"(begin-connect nb1 prj3 i1 o3)", // (:action begin-connect ?n - notebook ?p - projector ?ip - cb-in-port ?op - cb-out-port)
	"(begin-connect nb2 prj1 i2 o1)", // (:action begin-connect ?n - notebook ?p - projector ?ip - cb-in-port ?op - cb-out-port)
	"(begin-connect nb2 prj2 i2 o2)", // (:action begin-connect ?n - notebook ?p - projector ?ip - cb-in-port ?op - cb-out-port)
	"(begin-connect nb2 prj3 i2 o3)", // (:action begin-connect ?n - notebook ?p - projector ?ip - cb-in-port ?op - cb-out-port)
	"(begin-connect nb3 prj1 i3 o1)", // (:action begin-connect ?n - notebook ?p - projector ?ip - cb-in-port ?op - cb-out-port)
	"(begin-connect nb3 prj2 i3 o2)", // (:action begin-connect ?n - notebook ?p - projector ?ip - cb-in-port ?op - cb-out-port)
	"(begin-connect nb3 prj3 i3 o3)", // (:action begin-connect ?n - notebook ?p - projector ?ip - cb-in-port ?op - cb-out-port)
	"(end-connect nb1 prj1)", // (:action end-connect ?n - notebook ?p - projector)
	"(end-connect nb1 prj2)", // (:action end-connect ?n - notebook ?p - projector)
	"(end-connect nb1 prj3)", // (:action end-connect ?n - notebook ?p - projector)
	"(end-connect nb2 prj1)", // (:action end-connect ?n - notebook ?p - projector)
	"(end-connect nb2 prj2)", // (:action end-connect ?n - notebook ?p - projector)
	"(end-connect nb2 prj3)", // (:action end-connect ?n - notebook ?p - projector)
	"(end-connect nb3 prj1)", // (:action end-connect ?n - notebook ?p - projector)
	"(end-connect nb3 prj2)", // (:action end-connect ?n - notebook ?p - projector)
	"(end-connect nb3 prj3)", // (:action end-connect ?n - notebook ?p - projector)
	"(xxx-device-crossbar-switch i1 o1)", // (:action xxx-device-crossbar-switch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-switch i1 o2)", // (:action xxx-device-crossbar-switch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-switch i1 o3)", // (:action xxx-device-crossbar-switch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-switch i2 o1)", // (:action xxx-device-crossbar-switch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-switch i2 o2)", // (:action xxx-device-crossbar-switch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-switch i2 o3)", // (:action xxx-device-crossbar-switch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-switch i3 o1)", // (:action xxx-device-crossbar-switch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-switch i3 o2)", // (:action xxx-device-crossbar-switch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-switch i3 o3)", // (:action xxx-device-crossbar-switch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-unswitch i1 o1)", // (:action xxx-device-crossbar-unswitch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-unswitch i1 o2)", // (:action xxx-device-crossbar-unswitch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-unswitch i1 o3)", // (:action xxx-device-crossbar-unswitch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-unswitch i2 o1)", // (:action xxx-device-crossbar-unswitch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-unswitch i2 o2)", // (:action xxx-device-crossbar-unswitch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-unswitch i2 o3)", // (:action xxx-device-crossbar-unswitch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-unswitch i3 o1)", // (:action xxx-device-crossbar-unswitch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-unswitch i3 o2)", // (:action xxx-device-crossbar-unswitch ?ip - cb-in-port ?op - cb-out-port)
	"(xxx-device-crossbar-unswitch i3 o3)", // (:action xxx-device-crossbar-unswitch ?ip - cb-in-port ?op - cb-out-port)
	"(begin-project-document prj1 nb1 doc1)", // (:action begin-project-document ?p - projector ?n - notebook ?d - document)
	"(begin-project-document prj1 nb2 doc2)", // (:action begin-project-document ?p - projector ?n - notebook ?d - document)
	"(begin-project-document prj1 nb3 doc3)", // (:action begin-project-document ?p - projector ?n - notebook ?d - document)
	"(begin-project-document prj2 nb1 doc1)", // (:action begin-project-document ?p - projector ?n - notebook ?d - document)
	"(begin-project-document prj2 nb2 doc2)", // (:action begin-project-document ?p - projector ?n - notebook ?d - document)
	"(begin-project-document prj2 nb3 doc3)", // (:action begin-project-document ?p - projector ?n - notebook ?d - document)
	"(begin-project-document prj3 nb1 doc1)", // (:action begin-project-document ?p - projector ?n - notebook ?d - document)
	"(begin-project-document prj3 nb2 doc2)", // (:action begin-project-document ?p - projector ?n - notebook ?d - document)
	"(begin-project-document prj3 nb3 doc3)", // (:action begin-project-document ?p - projector ?n - notebook ?d - document)
	"(end-project-document prj1 nb1 doc1)", // (:action end-project-document ?p - projector ?n - notebook ?d - document)
	"(end-project-document prj1 nb2 doc2)", // (:action end-project-document ?p - projector ?n - notebook ?d - document)
	"(end-project-document prj1 nb3 doc3)", // (:action end-project-document ?p - projector ?n - notebook ?d - document)
	"(end-project-document prj2 nb1 doc1)", // (:action end-project-document ?p - projector ?n - notebook ?d - document)
	"(end-project-document prj2 nb2 doc2)", // (:action end-project-document ?p - projector ?n - notebook ?d - document)
	"(end-project-document prj2 nb3 doc3)", // (:action end-project-document ?p - projector ?n - notebook ?d - document)
	"(end-project-document prj3 nb1 doc1)", // (:action end-project-document ?p - projector ?n - notebook ?d - document)
	"(end-project-document prj3 nb2 doc2)", // (:action end-project-document ?p - projector ?n - notebook ?d - document)
	"(end-project-document prj3 nb3 doc3)", // (:action end-project-document ?p - projector ?n - notebook ?d - document)
	"(begin-show-document scr1 prj1 doc1)", // (:action begin-show-document ?s - screen ?p - projector ?d - document)
	"(begin-show-document scr1 prj1 doc2)", // (:action begin-show-document ?s - screen ?p - projector ?d - document)
	"(begin-show-document scr1 prj1 doc3)", // (:action begin-show-document ?s - screen ?p - projector ?d - document)
	"(begin-show-document scr2 prj2 doc1)", // (:action begin-show-document ?s - screen ?p - projector ?d - document)
	"(begin-show-document scr2 prj2 doc2)", // (:action begin-show-document ?s - screen ?p - projector ?d - document)
	"(begin-show-document scr2 prj2 doc3)", // (:action begin-show-document ?s - screen ?p - projector ?d - document)
	"(begin-show-document scr3 prj3 doc1)", // (:action begin-show-document ?s - screen ?p - projector ?d - document)
	"(begin-show-document scr3 prj3 doc2)", // (:action begin-show-document ?s - screen ?p - projector ?d - document)
	"(begin-show-document scr3 prj3 doc3)", // (:action begin-show-document ?s - screen ?p - projector ?d - document)
	"(end-show-document scr1 prj1 doc1)", // (:action end-show-document ?s - screen ?p - projector ?d - document)
	"(end-show-document scr1 prj1 doc2)", // (:action end-show-document ?s - screen ?p - projector ?d - document)
	"(end-show-document scr1 prj1 doc3)", // (:action end-show-document ?s - screen ?p - projector ?d - document)
	"(end-show-document scr2 prj2 doc1)", // (:action end-show-document ?s - screen ?p - projector ?d - document)
	"(end-show-document scr2 prj2 doc2)", // (:action end-show-document ?s - screen ?p - projector ?d - document)
	"(end-show-document scr2 prj2 doc3)", // (:action end-show-document ?s - screen ?p - projector ?d - document)
	"(end-show-document scr3 prj3 doc1)", // (:action end-show-document ?s - screen ?p - projector ?d - document)
	"(end-show-document scr3 prj3 doc2)", // (:action end-show-document ?s - screen ?p - projector ?d - document)
	"(end-show-document scr3 prj3 doc3)", // (:action end-show-document ?s - screen ?p - projector ?d - document)
	"(begin-looking alice scr1 doc1 seatB)", // (:action begin-looking ?p - person ?s - screen ?d - document ?l - location)
	"(begin-looking alice scr1 doc2 seatB)", // (:action begin-looking ?p - person ?s - screen ?d - document ?l - location)
	"(begin-looking alice scr1 doc3 seatB)", // (:action begin-looking ?p - person ?s - screen ?d - document ?l - location)
	"(begin-looking alice scr2 doc1 seatB)", // (:action begin-looking ?p - person ?s - screen ?d - document ?l - location)
	"(begin-looking alice scr2 doc2 seatB)", // (:action begin-looking ?p - person ?s - screen ?d - document ?l - location)
	"(begin-looking alice scr2 doc3 seatB)", // (:action begin-looking ?p - person ?s - screen ?d - document ?l - location)
	"(begin-looking alice scr3 doc1 seatB)", // (:action begin-looking ?p - person ?s - screen ?d - document ?l - location)
	"(begin-looking alice scr3 doc2 seatB)", // (:action begin-looking ?p - person ?s - screen ?d - document ?l - location)
	"(begin-looking alice scr3 doc3 seatB)", // (:action begin-looking ?p - person ?s - screen ?d - document ?l - location)
	"(begin-looking bob scr2 doc1 seatA)", // (:action begin-looking ?p - person ?s - screen ?d - document ?l - location)
	"(begin-looking bob scr3 doc1 seatA)", // (:action begin-looking ?p - person ?s - screen ?d - document ?l - location)
	"(begin-looking charlie scr1 doc3 seatC)", // (:action begin-looking ?p - person ?s - screen ?d - document ?l - location)
	"(begin-looking charlie scr2 doc3 seatC)" // (:action begin-looking ?p - person ?s - screen ?d - document ?l - location)
};
char const * const * const actionNames = &__actionNames[3];


/* (activate nb1), id 0 */
double action_activate_nb1(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sis_active_nb1)) *specificity=1.00; 

	if(!(x->Sis_active_nb1)) {
		*x1 = *x;
		x1->Sis_active_nb1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (activate nb2), id 1 */
double action_activate_nb2(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sis_active_nb2)) *specificity=1.00; 

	if(!(x->Sis_active_nb2)) {
		*x1 = *x;
		x1->Sis_active_nb2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (activate nb3), id 2 */
double action_activate_nb3(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sis_active_nb3)) *specificity=1.00; 

	if(!(x->Sis_active_nb3)) {
		*x1 = *x;
		x1->Sis_active_nb3 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (activate prj1), id 3 */
double action_activate_prj1(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sis_active_prj1)) *specificity=1.00; 

	if(!(x->Sis_active_prj1)) {
		*x1 = *x;
		x1->Sis_active_prj1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (activate prj2), id 4 */
double action_activate_prj2(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sis_active_prj2)) *specificity=1.00; 

	if(!(x->Sis_active_prj2)) {
		*x1 = *x;
		x1->Sis_active_prj2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (activate prj3), id 5 */
double action_activate_prj3(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sis_active_prj3)) *specificity=1.00; 

	if(!(x->Sis_active_prj3)) {
		*x1 = *x;
		x1->Sis_active_prj3 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (activate scr1), id 6 */
double action_activate_scr1(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sis_active_scr1)) *specificity=1.00; 

	if(!(x->Sis_active_scr1)) {
		*x1 = *x;
		x1->Sis_active_scr1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (activate scr2), id 7 */
double action_activate_scr2(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sis_active_scr2)) *specificity=1.00; 

	if(!(x->Sis_active_scr2)) {
		*x1 = *x;
		x1->Sis_active_scr2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (activate scr3), id 8 */
double action_activate_scr3(StatePtr x, StateRec *x1, double *specificity) {
	if(!(x->Sis_active_scr3)) *specificity=1.00; 

	if(!(x->Sis_active_scr3)) {
		*x1 = *x;
		x1->Sis_active_scr3 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (deactivate nb1), id 9 */
double action_deactivate_nb1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_nb1 && !(x->Sin_use_nb1) && !(x->Sis_locked_by_nb1_vga_out_signal_prj3) && !(x->Sis_locked_by_nb1_vga_out_signal_prj1) && !(x->Sis_locked_by_nb1_vga_out_port_crossbar) && !(x->Sis_locked_by_nb1_vga_out_signal_prj2))) *specificity=3.50; 

	if((x->Sis_active_nb1 && !(x->Sin_use_nb1) && !(x->Sis_locked_by_nb1_vga_out_signal_prj3) && !(x->Sis_locked_by_nb1_vga_out_signal_prj1) && !(x->Sis_locked_by_nb1_vga_out_port_crossbar) && !(x->Sis_locked_by_nb1_vga_out_signal_prj2))) {
		*x1 = *x;
		x1->Sis_active_nb1 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (deactivate nb2), id 10 */
double action_deactivate_nb2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_nb2 && !(x->Sin_use_nb2) && !(x->Sis_locked_by_nb2_vga_out_signal_prj3) && !(x->Sis_locked_by_nb2_vga_out_signal_prj1) && !(x->Sis_locked_by_nb2_vga_out_port_crossbar) && !(x->Sis_locked_by_nb2_vga_out_signal_prj2))) *specificity=3.50; 

	if((x->Sis_active_nb2 && !(x->Sin_use_nb2) && !(x->Sis_locked_by_nb2_vga_out_signal_prj3) && !(x->Sis_locked_by_nb2_vga_out_signal_prj1) && !(x->Sis_locked_by_nb2_vga_out_port_crossbar) && !(x->Sis_locked_by_nb2_vga_out_signal_prj2))) {
		*x1 = *x;
		x1->Sis_active_nb2 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (deactivate nb3), id 11 */
double action_deactivate_nb3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_nb3 && !(x->Sin_use_nb3) && !(x->Sis_locked_by_nb3_vga_out_signal_prj3) && !(x->Sis_locked_by_nb3_vga_out_signal_prj1) && !(x->Sis_locked_by_nb3_vga_out_port_crossbar) && !(x->Sis_locked_by_nb3_vga_out_signal_prj2))) *specificity=3.50; 

	if((x->Sis_active_nb3 && !(x->Sin_use_nb3) && !(x->Sis_locked_by_nb3_vga_out_signal_prj3) && !(x->Sis_locked_by_nb3_vga_out_signal_prj1) && !(x->Sis_locked_by_nb3_vga_out_port_crossbar) && !(x->Sis_locked_by_nb3_vga_out_signal_prj2))) {
		*x1 = *x;
		x1->Sis_active_nb3 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (deactivate prj1), id 12 */
double action_deactivate_prj1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_prj1 && !(x->Sin_use_prj1) && !(x->Sis_locked_by_prj1_picture_scr1) && !(x->Sis_locked_by_prj1_vga_in_port_crossbar) && !(x->Sis_locked_by_prj1_vga_in_port_prj1))) *specificity=1.79; 

	if((x->Sis_active_prj1 && !(x->Sin_use_prj1) && !(x->Sis_locked_by_prj1_picture_scr1) && !(x->Sis_locked_by_prj1_vga_in_port_crossbar) && !(x->Sis_locked_by_prj1_vga_in_port_prj1))) {
		*x1 = *x;
		x1->Sis_active_prj1 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (deactivate prj2), id 13 */
double action_deactivate_prj2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_prj2 && !(x->Sin_use_prj2) && !(x->Sis_locked_by_prj2_vga_in_port_prj2) && !(x->Sis_locked_by_prj2_vga_in_port_crossbar) && !(x->Sis_locked_by_prj2_picture_scr2))) *specificity=1.79; 

	if((x->Sis_active_prj2 && !(x->Sin_use_prj2) && !(x->Sis_locked_by_prj2_vga_in_port_prj2) && !(x->Sis_locked_by_prj2_vga_in_port_crossbar) && !(x->Sis_locked_by_prj2_picture_scr2))) {
		*x1 = *x;
		x1->Sis_active_prj2 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (deactivate prj3), id 14 */
double action_deactivate_prj3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_prj3 && !(x->Sin_use_prj3) && !(x->Sis_locked_by_prj3_picture_scr3) && !(x->Sis_locked_by_prj3_vga_in_port_crossbar) && !(x->Sis_locked_by_prj3_vga_in_port_prj3))) *specificity=1.79; 

	if((x->Sis_active_prj3 && !(x->Sin_use_prj3) && !(x->Sis_locked_by_prj3_picture_scr3) && !(x->Sis_locked_by_prj3_vga_in_port_crossbar) && !(x->Sis_locked_by_prj3_vga_in_port_prj3))) {
		*x1 = *x;
		x1->Sis_active_prj3 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (deactivate scr1), id 15 */
double action_deactivate_scr1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_scr1 && !(x->Sin_use_scr1) && !(x->Sis_locked_by_scr1_picture_alice) && !(x->Sis_locked_by_scr1_picture_charlie))) *specificity=1.54; 

	if((x->Sis_active_scr1 && !(x->Sin_use_scr1) && !(x->Sis_locked_by_scr1_picture_alice) && !(x->Sis_locked_by_scr1_picture_charlie))) {
		*x1 = *x;
		x1->Sis_active_scr1 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (deactivate scr2), id 16 */
double action_deactivate_scr2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_scr2 && !(x->Sin_use_scr2) && !(x->Sis_locked_by_scr2_picture_bob) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_charlie))) *specificity=1.68; 

	if((x->Sis_active_scr2 && !(x->Sin_use_scr2) && !(x->Sis_locked_by_scr2_picture_bob) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_charlie))) {
		*x1 = *x;
		x1->Sis_active_scr2 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (deactivate scr3), id 17 */
double action_deactivate_scr3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_scr3 && !(x->Sin_use_scr3) && !(x->Sis_locked_by_scr3_picture_bob) && !(x->Sis_locked_by_scr3_picture_alice))) *specificity=1.54; 

	if((x->Sis_active_scr3 && !(x->Sin_use_scr3) && !(x->Sis_locked_by_scr3_picture_bob) && !(x->Sis_locked_by_scr3_picture_alice))) {
		*x1 = *x;
		x1->Sis_active_scr3 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-send-document nb1 doc1), id 18 */
double action_begin_send_document_nb1_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_nb1 && !(x->Sis_sending_nb1_doc1_vga_out_signal) && !(x->Sis_locked_by_nb1_vga_out_signal_prj3) && !(x->Sis_locked_by_nb1_vga_out_signal_prj1) && !(x->Sis_locked_by_nb1_vga_out_signal_prj2))) *specificity=2.50; 

	if((x->Sis_active_nb1 && !(x->Sis_sending_nb1_doc1_vga_out_signal) && !(x->Sis_locked_by_nb1_vga_out_signal_prj3) && !(x->Sis_locked_by_nb1_vga_out_signal_prj1) && !(x->Sis_locked_by_nb1_vga_out_signal_prj2))) {
		*x1 = *x;
		x1->Sis_sending_nb1_doc1_vga_out_signal = 1;
		x1->Sin_use_nb1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-send-document nb2 doc2), id 19 */
double action_begin_send_document_nb2_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_nb2 && !(x->Sis_sending_nb2_doc2_vga_out_signal) && !(x->Sis_locked_by_nb2_vga_out_signal_prj3) && !(x->Sis_locked_by_nb2_vga_out_signal_prj1) && !(x->Sis_locked_by_nb2_vga_out_signal_prj2))) *specificity=2.50; 

	if((x->Sis_active_nb2 && !(x->Sis_sending_nb2_doc2_vga_out_signal) && !(x->Sis_locked_by_nb2_vga_out_signal_prj3) && !(x->Sis_locked_by_nb2_vga_out_signal_prj1) && !(x->Sis_locked_by_nb2_vga_out_signal_prj2))) {
		*x1 = *x;
		x1->Sis_sending_nb2_doc2_vga_out_signal = 1;
		x1->Sin_use_nb2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-send-document nb3 doc3), id 20 */
double action_begin_send_document_nb3_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_nb3 && !(x->Sis_sending_nb3_doc3_vga_out_signal) && !(x->Sis_locked_by_nb3_vga_out_signal_prj3) && !(x->Sis_locked_by_nb3_vga_out_signal_prj1) && !(x->Sis_locked_by_nb3_vga_out_signal_prj2))) *specificity=2.50; 

	if((x->Sis_active_nb3 && !(x->Sis_sending_nb3_doc3_vga_out_signal) && !(x->Sis_locked_by_nb3_vga_out_signal_prj3) && !(x->Sis_locked_by_nb3_vga_out_signal_prj1) && !(x->Sis_locked_by_nb3_vga_out_signal_prj2))) {
		*x1 = *x;
		x1->Sis_sending_nb3_doc3_vga_out_signal = 1;
		x1->Sin_use_nb3 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (end-send-document nb1 doc1), id 21 */
double action_end_send_document_nb1_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_sending_nb1_doc1_vga_out_signal && !(x->Sis_locked_by_nb1_vga_out_signal_prj3) && !(x->Sis_locked_by_nb1_vga_out_signal_prj1) && !(x->Sis_locked_by_nb1_vga_out_signal_prj2))) *specificity=1.25; 

	if((x->Sis_sending_nb1_doc1_vga_out_signal && !(x->Sis_locked_by_nb1_vga_out_signal_prj3) && !(x->Sis_locked_by_nb1_vga_out_signal_prj1) && !(x->Sis_locked_by_nb1_vga_out_signal_prj2))) {
		*x1 = *x;
		x1->Sis_sending_nb1_doc1_vga_out_signal = 0;
		x1->Sin_use_nb1 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-send-document nb2 doc2), id 22 */
double action_end_send_document_nb2_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_sending_nb2_doc2_vga_out_signal && !(x->Sis_locked_by_nb2_vga_out_signal_prj3) && !(x->Sis_locked_by_nb2_vga_out_signal_prj1) && !(x->Sis_locked_by_nb2_vga_out_signal_prj2))) *specificity=1.25; 

	if((x->Sis_sending_nb2_doc2_vga_out_signal && !(x->Sis_locked_by_nb2_vga_out_signal_prj3) && !(x->Sis_locked_by_nb2_vga_out_signal_prj1) && !(x->Sis_locked_by_nb2_vga_out_signal_prj2))) {
		*x1 = *x;
		x1->Sis_sending_nb2_doc2_vga_out_signal = 0;
		x1->Sin_use_nb2 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-send-document nb3 doc3), id 23 */
double action_end_send_document_nb3_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_sending_nb3_doc3_vga_out_signal && !(x->Sis_locked_by_nb3_vga_out_signal_prj3) && !(x->Sis_locked_by_nb3_vga_out_signal_prj1) && !(x->Sis_locked_by_nb3_vga_out_signal_prj2))) *specificity=1.25; 

	if((x->Sis_sending_nb3_doc3_vga_out_signal && !(x->Sis_locked_by_nb3_vga_out_signal_prj3) && !(x->Sis_locked_by_nb3_vga_out_signal_prj1) && !(x->Sis_locked_by_nb3_vga_out_signal_prj2))) {
		*x1 = *x;
		x1->Sis_sending_nb3_doc3_vga_out_signal = 0;
		x1->Sin_use_nb3 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-connect nb1 prj1 i1 o1), id 24 */
double action_begin_connect_nb1_prj1_i1_o1(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Sis_connected_nb1_vga_out_port_prj1_vga_in_port) && !(x->Sis_locked_by_prj1_vga_in_port_prj1) && !(x->Sis_locked_by_prj1_vga_in_port_crossbar) && x->Sis_switched_i1_o1)) *specificity=1.89; 

	if((!(x->Sis_connected_nb1_vga_out_port_prj1_vga_in_port) && !(x->Sis_locked_by_prj1_vga_in_port_prj1) && !(x->Sis_locked_by_prj1_vga_in_port_crossbar) && x->Sis_switched_i1_o1)) {
		*x1 = *x;
		x1->Sis_locked_by_prj1_vga_in_port_crossbar = 1;
		x1->Sis_locked_by_nb1_vga_out_port_crossbar = 1;
		x1->Sis_connected_nb1_vga_out_port_prj1_vga_in_port = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-connect nb1 prj2 i1 o2), id 25 */
double action_begin_connect_nb1_prj2_i1_o2(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Sis_connected_nb1_vga_out_port_prj2_vga_in_port) && !(x->Sis_locked_by_prj2_vga_in_port_crossbar) && !(x->Sis_locked_by_prj2_vga_in_port_prj2) && x->Sis_switched_i1_o2)) *specificity=1.89; 

	if((!(x->Sis_connected_nb1_vga_out_port_prj2_vga_in_port) && !(x->Sis_locked_by_prj2_vga_in_port_crossbar) && !(x->Sis_locked_by_prj2_vga_in_port_prj2) && x->Sis_switched_i1_o2)) {
		*x1 = *x;
		x1->Sis_locked_by_prj2_vga_in_port_crossbar = 1;
		x1->Sis_locked_by_nb1_vga_out_port_crossbar = 1;
		x1->Sis_connected_nb1_vga_out_port_prj2_vga_in_port = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-connect nb1 prj3 i1 o3), id 26 */
double action_begin_connect_nb1_prj3_i1_o3(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Sis_connected_nb1_vga_out_port_prj3_vga_in_port) && !(x->Sis_locked_by_prj3_vga_in_port_prj3) && !(x->Sis_locked_by_prj3_vga_in_port_crossbar) && x->Sis_switched_i1_o3)) *specificity=1.89; 

	if((!(x->Sis_connected_nb1_vga_out_port_prj3_vga_in_port) && !(x->Sis_locked_by_prj3_vga_in_port_prj3) && !(x->Sis_locked_by_prj3_vga_in_port_crossbar) && x->Sis_switched_i1_o3)) {
		*x1 = *x;
		x1->Sis_locked_by_prj3_vga_in_port_crossbar = 1;
		x1->Sis_locked_by_nb1_vga_out_port_crossbar = 1;
		x1->Sis_connected_nb1_vga_out_port_prj3_vga_in_port = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-connect nb2 prj1 i2 o1), id 27 */
double action_begin_connect_nb2_prj1_i2_o1(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Sis_connected_nb2_vga_out_port_prj1_vga_in_port) && !(x->Sis_locked_by_prj1_vga_in_port_prj1) && !(x->Sis_locked_by_prj1_vga_in_port_crossbar) && x->Sis_switched_i2_o1)) *specificity=1.89; 

	if((!(x->Sis_connected_nb2_vga_out_port_prj1_vga_in_port) && !(x->Sis_locked_by_prj1_vga_in_port_prj1) && !(x->Sis_locked_by_prj1_vga_in_port_crossbar) && x->Sis_switched_i2_o1)) {
		*x1 = *x;
		x1->Sis_locked_by_prj1_vga_in_port_crossbar = 1;
		x1->Sis_locked_by_nb2_vga_out_port_crossbar = 1;
		x1->Sis_connected_nb2_vga_out_port_prj1_vga_in_port = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-connect nb2 prj2 i2 o2), id 28 */
double action_begin_connect_nb2_prj2_i2_o2(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Sis_connected_nb2_vga_out_port_prj2_vga_in_port) && !(x->Sis_locked_by_prj2_vga_in_port_crossbar) && !(x->Sis_locked_by_prj2_vga_in_port_prj2) && x->Sis_switched_i2_o2)) *specificity=1.89; 

	if((!(x->Sis_connected_nb2_vga_out_port_prj2_vga_in_port) && !(x->Sis_locked_by_prj2_vga_in_port_crossbar) && !(x->Sis_locked_by_prj2_vga_in_port_prj2) && x->Sis_switched_i2_o2)) {
		*x1 = *x;
		x1->Sis_locked_by_prj2_vga_in_port_crossbar = 1;
		x1->Sis_locked_by_nb2_vga_out_port_crossbar = 1;
		x1->Sis_connected_nb2_vga_out_port_prj2_vga_in_port = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-connect nb2 prj3 i2 o3), id 29 */
double action_begin_connect_nb2_prj3_i2_o3(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Sis_connected_nb2_vga_out_port_prj3_vga_in_port) && !(x->Sis_locked_by_prj3_vga_in_port_prj3) && !(x->Sis_locked_by_prj3_vga_in_port_crossbar) && x->Sis_switched_i2_o3)) *specificity=1.89; 

	if((!(x->Sis_connected_nb2_vga_out_port_prj3_vga_in_port) && !(x->Sis_locked_by_prj3_vga_in_port_prj3) && !(x->Sis_locked_by_prj3_vga_in_port_crossbar) && x->Sis_switched_i2_o3)) {
		*x1 = *x;
		x1->Sis_locked_by_prj3_vga_in_port_crossbar = 1;
		x1->Sis_locked_by_nb2_vga_out_port_crossbar = 1;
		x1->Sis_connected_nb2_vga_out_port_prj3_vga_in_port = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-connect nb3 prj1 i3 o1), id 30 */
double action_begin_connect_nb3_prj1_i3_o1(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Sis_connected_nb3_vga_out_port_prj1_vga_in_port) && !(x->Sis_locked_by_prj1_vga_in_port_prj1) && !(x->Sis_locked_by_prj1_vga_in_port_crossbar) && x->Sis_switched_i3_o1)) *specificity=1.89; 

	if((!(x->Sis_connected_nb3_vga_out_port_prj1_vga_in_port) && !(x->Sis_locked_by_prj1_vga_in_port_prj1) && !(x->Sis_locked_by_prj1_vga_in_port_crossbar) && x->Sis_switched_i3_o1)) {
		*x1 = *x;
		x1->Sis_locked_by_prj1_vga_in_port_crossbar = 1;
		x1->Sis_locked_by_nb3_vga_out_port_crossbar = 1;
		x1->Sis_connected_nb3_vga_out_port_prj1_vga_in_port = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-connect nb3 prj2 i3 o2), id 31 */
double action_begin_connect_nb3_prj2_i3_o2(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Sis_connected_nb3_vga_out_port_prj2_vga_in_port) && !(x->Sis_locked_by_prj2_vga_in_port_crossbar) && !(x->Sis_locked_by_prj2_vga_in_port_prj2) && x->Sis_switched_i3_o2)) *specificity=1.89; 

	if((!(x->Sis_connected_nb3_vga_out_port_prj2_vga_in_port) && !(x->Sis_locked_by_prj2_vga_in_port_crossbar) && !(x->Sis_locked_by_prj2_vga_in_port_prj2) && x->Sis_switched_i3_o2)) {
		*x1 = *x;
		x1->Sis_locked_by_prj2_vga_in_port_crossbar = 1;
		x1->Sis_locked_by_nb3_vga_out_port_crossbar = 1;
		x1->Sis_connected_nb3_vga_out_port_prj2_vga_in_port = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-connect nb3 prj3 i3 o3), id 32 */
double action_begin_connect_nb3_prj3_i3_o3(StatePtr x, StateRec *x1, double *specificity) {
	if((!(x->Sis_connected_nb3_vga_out_port_prj3_vga_in_port) && !(x->Sis_locked_by_prj3_vga_in_port_prj3) && !(x->Sis_locked_by_prj3_vga_in_port_crossbar) && x->Sis_switched_i3_o3)) *specificity=1.89; 

	if((!(x->Sis_connected_nb3_vga_out_port_prj3_vga_in_port) && !(x->Sis_locked_by_prj3_vga_in_port_prj3) && !(x->Sis_locked_by_prj3_vga_in_port_crossbar) && x->Sis_switched_i3_o3)) {
		*x1 = *x;
		x1->Sis_locked_by_prj3_vga_in_port_crossbar = 1;
		x1->Sis_locked_by_nb3_vga_out_port_crossbar = 1;
		x1->Sis_connected_nb3_vga_out_port_prj3_vga_in_port = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (end-connect nb1 prj1), id 33 */
double action_end_connect_nb1_prj1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_connected_nb1_vga_out_port_prj1_vga_in_port && !(x->Sis_locked_by_prj1_vga_in_port_prj1))) *specificity=0.64; 

	if((x->Sis_connected_nb1_vga_out_port_prj1_vga_in_port && !(x->Sis_locked_by_prj1_vga_in_port_prj1))) {
		*x1 = *x;
		x1->Sis_locked_by_prj1_vga_in_port_crossbar = 0;
		x1->Sis_locked_by_nb1_vga_out_port_crossbar = 0;
		x1->Sis_connected_nb1_vga_out_port_prj1_vga_in_port = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-connect nb1 prj2), id 34 */
double action_end_connect_nb1_prj2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_connected_nb1_vga_out_port_prj2_vga_in_port && !(x->Sis_locked_by_prj2_vga_in_port_prj2))) *specificity=0.64; 

	if((x->Sis_connected_nb1_vga_out_port_prj2_vga_in_port && !(x->Sis_locked_by_prj2_vga_in_port_prj2))) {
		*x1 = *x;
		x1->Sis_locked_by_prj2_vga_in_port_crossbar = 0;
		x1->Sis_locked_by_nb1_vga_out_port_crossbar = 0;
		x1->Sis_connected_nb1_vga_out_port_prj2_vga_in_port = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-connect nb1 prj3), id 35 */
double action_end_connect_nb1_prj3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_connected_nb1_vga_out_port_prj3_vga_in_port && !(x->Sis_locked_by_prj3_vga_in_port_prj3))) *specificity=0.64; 

	if((x->Sis_connected_nb1_vga_out_port_prj3_vga_in_port && !(x->Sis_locked_by_prj3_vga_in_port_prj3))) {
		*x1 = *x;
		x1->Sis_locked_by_prj3_vga_in_port_crossbar = 0;
		x1->Sis_locked_by_nb1_vga_out_port_crossbar = 0;
		x1->Sis_connected_nb1_vga_out_port_prj3_vga_in_port = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-connect nb2 prj1), id 36 */
double action_end_connect_nb2_prj1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_connected_nb2_vga_out_port_prj1_vga_in_port && !(x->Sis_locked_by_prj1_vga_in_port_prj1))) *specificity=0.64; 

	if((x->Sis_connected_nb2_vga_out_port_prj1_vga_in_port && !(x->Sis_locked_by_prj1_vga_in_port_prj1))) {
		*x1 = *x;
		x1->Sis_locked_by_prj1_vga_in_port_crossbar = 0;
		x1->Sis_locked_by_nb2_vga_out_port_crossbar = 0;
		x1->Sis_connected_nb2_vga_out_port_prj1_vga_in_port = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-connect nb2 prj2), id 37 */
double action_end_connect_nb2_prj2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_connected_nb2_vga_out_port_prj2_vga_in_port && !(x->Sis_locked_by_prj2_vga_in_port_prj2))) *specificity=0.64; 

	if((x->Sis_connected_nb2_vga_out_port_prj2_vga_in_port && !(x->Sis_locked_by_prj2_vga_in_port_prj2))) {
		*x1 = *x;
		x1->Sis_locked_by_prj2_vga_in_port_crossbar = 0;
		x1->Sis_locked_by_nb2_vga_out_port_crossbar = 0;
		x1->Sis_connected_nb2_vga_out_port_prj2_vga_in_port = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-connect nb2 prj3), id 38 */
double action_end_connect_nb2_prj3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_connected_nb2_vga_out_port_prj3_vga_in_port && !(x->Sis_locked_by_prj3_vga_in_port_prj3))) *specificity=0.64; 

	if((x->Sis_connected_nb2_vga_out_port_prj3_vga_in_port && !(x->Sis_locked_by_prj3_vga_in_port_prj3))) {
		*x1 = *x;
		x1->Sis_locked_by_prj3_vga_in_port_crossbar = 0;
		x1->Sis_locked_by_nb2_vga_out_port_crossbar = 0;
		x1->Sis_connected_nb2_vga_out_port_prj3_vga_in_port = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-connect nb3 prj1), id 39 */
double action_end_connect_nb3_prj1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_connected_nb3_vga_out_port_prj1_vga_in_port && !(x->Sis_locked_by_prj1_vga_in_port_prj1))) *specificity=0.64; 

	if((x->Sis_connected_nb3_vga_out_port_prj1_vga_in_port && !(x->Sis_locked_by_prj1_vga_in_port_prj1))) {
		*x1 = *x;
		x1->Sis_locked_by_prj1_vga_in_port_crossbar = 0;
		x1->Sis_locked_by_nb3_vga_out_port_crossbar = 0;
		x1->Sis_connected_nb3_vga_out_port_prj1_vga_in_port = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-connect nb3 prj2), id 40 */
double action_end_connect_nb3_prj2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_connected_nb3_vga_out_port_prj2_vga_in_port && !(x->Sis_locked_by_prj2_vga_in_port_prj2))) *specificity=0.64; 

	if((x->Sis_connected_nb3_vga_out_port_prj2_vga_in_port && !(x->Sis_locked_by_prj2_vga_in_port_prj2))) {
		*x1 = *x;
		x1->Sis_locked_by_prj2_vga_in_port_crossbar = 0;
		x1->Sis_locked_by_nb3_vga_out_port_crossbar = 0;
		x1->Sis_connected_nb3_vga_out_port_prj2_vga_in_port = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-connect nb3 prj3), id 41 */
double action_end_connect_nb3_prj3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_connected_nb3_vga_out_port_prj3_vga_in_port && !(x->Sis_locked_by_prj3_vga_in_port_prj3))) *specificity=0.64; 

	if((x->Sis_connected_nb3_vga_out_port_prj3_vga_in_port && !(x->Sis_locked_by_prj3_vga_in_port_prj3))) {
		*x1 = *x;
		x1->Sis_locked_by_prj3_vga_in_port_crossbar = 0;
		x1->Sis_locked_by_nb3_vga_out_port_crossbar = 0;
		x1->Sis_connected_nb3_vga_out_port_prj3_vga_in_port = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-switch i1 o1), id 42 */
double action_xxx_device_crossbar_switch_i1_o1(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_open_o1) *specificity=0.33; 

	if(x->Sis_open_o1) {
		*x1 = *x;
		x1->Sis_switched_i1_o1 = 1;
		x1->Sis_open_o1 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-switch i1 o2), id 43 */
double action_xxx_device_crossbar_switch_i1_o2(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_open_o2) *specificity=0.33; 

	if(x->Sis_open_o2) {
		*x1 = *x;
		x1->Sis_switched_i1_o2 = 1;
		x1->Sis_open_o2 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-switch i1 o3), id 44 */
double action_xxx_device_crossbar_switch_i1_o3(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_open_o3) *specificity=0.33; 

	if(x->Sis_open_o3) {
		*x1 = *x;
		x1->Sis_switched_i1_o3 = 1;
		x1->Sis_open_o3 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-switch i2 o1), id 45 */
double action_xxx_device_crossbar_switch_i2_o1(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_open_o1) *specificity=0.33; 

	if(x->Sis_open_o1) {
		*x1 = *x;
		x1->Sis_switched_i2_o1 = 1;
		x1->Sis_open_o1 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-switch i2 o2), id 46 */
double action_xxx_device_crossbar_switch_i2_o2(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_open_o2) *specificity=0.33; 

	if(x->Sis_open_o2) {
		*x1 = *x;
		x1->Sis_switched_i2_o2 = 1;
		x1->Sis_open_o2 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-switch i2 o3), id 47 */
double action_xxx_device_crossbar_switch_i2_o3(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_open_o3) *specificity=0.33; 

	if(x->Sis_open_o3) {
		*x1 = *x;
		x1->Sis_switched_i2_o3 = 1;
		x1->Sis_open_o3 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-switch i3 o1), id 48 */
double action_xxx_device_crossbar_switch_i3_o1(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_open_o1) *specificity=0.33; 

	if(x->Sis_open_o1) {
		*x1 = *x;
		x1->Sis_switched_i3_o1 = 1;
		x1->Sis_open_o1 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-switch i3 o2), id 49 */
double action_xxx_device_crossbar_switch_i3_o2(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_open_o2) *specificity=0.33; 

	if(x->Sis_open_o2) {
		*x1 = *x;
		x1->Sis_switched_i3_o2 = 1;
		x1->Sis_open_o2 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-switch i3 o3), id 50 */
double action_xxx_device_crossbar_switch_i3_o3(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_open_o3) *specificity=0.33; 

	if(x->Sis_open_o3) {
		*x1 = *x;
		x1->Sis_switched_i3_o3 = 1;
		x1->Sis_open_o3 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-unswitch i1 o1), id 51 */
double action_xxx_device_crossbar_unswitch_i1_o1(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_switched_i1_o1) *specificity=0.50; 

	if(x->Sis_switched_i1_o1) {
		*x1 = *x;
		x1->Sis_switched_i1_o1 = 0;
		x1->Sis_open_o1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-unswitch i1 o2), id 52 */
double action_xxx_device_crossbar_unswitch_i1_o2(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_switched_i1_o2) *specificity=0.50; 

	if(x->Sis_switched_i1_o2) {
		*x1 = *x;
		x1->Sis_switched_i1_o2 = 0;
		x1->Sis_open_o2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-unswitch i1 o3), id 53 */
double action_xxx_device_crossbar_unswitch_i1_o3(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_switched_i1_o3) *specificity=0.50; 

	if(x->Sis_switched_i1_o3) {
		*x1 = *x;
		x1->Sis_switched_i1_o3 = 0;
		x1->Sis_open_o3 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-unswitch i2 o1), id 54 */
double action_xxx_device_crossbar_unswitch_i2_o1(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_switched_i2_o1) *specificity=0.50; 

	if(x->Sis_switched_i2_o1) {
		*x1 = *x;
		x1->Sis_switched_i2_o1 = 0;
		x1->Sis_open_o1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-unswitch i2 o2), id 55 */
double action_xxx_device_crossbar_unswitch_i2_o2(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_switched_i2_o2) *specificity=0.50; 

	if(x->Sis_switched_i2_o2) {
		*x1 = *x;
		x1->Sis_switched_i2_o2 = 0;
		x1->Sis_open_o2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-unswitch i2 o3), id 56 */
double action_xxx_device_crossbar_unswitch_i2_o3(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_switched_i2_o3) *specificity=0.50; 

	if(x->Sis_switched_i2_o3) {
		*x1 = *x;
		x1->Sis_switched_i2_o3 = 0;
		x1->Sis_open_o3 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-unswitch i3 o1), id 57 */
double action_xxx_device_crossbar_unswitch_i3_o1(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_switched_i3_o1) *specificity=0.50; 

	if(x->Sis_switched_i3_o1) {
		*x1 = *x;
		x1->Sis_switched_i3_o1 = 0;
		x1->Sis_open_o1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-unswitch i3 o2), id 58 */
double action_xxx_device_crossbar_unswitch_i3_o2(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_switched_i3_o2) *specificity=0.50; 

	if(x->Sis_switched_i3_o2) {
		*x1 = *x;
		x1->Sis_switched_i3_o2 = 0;
		x1->Sis_open_o2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (xxx-device-crossbar-unswitch i3 o3), id 59 */
double action_xxx_device_crossbar_unswitch_i3_o3(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_switched_i3_o3) *specificity=0.50; 

	if(x->Sis_switched_i3_o3) {
		*x1 = *x;
		x1->Sis_switched_i3_o3 = 0;
		x1->Sis_open_o3 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-project-document prj1 nb1 doc1), id 60 */
double action_begin_project_document_prj1_nb1_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_prj1 && !(x->Sis_projecting_prj1_doc1) && !(x->Sis_locked_by_prj1_picture_scr1) && x->Sis_sending_nb1_doc1_vga_out_signal && x->Sis_connected_nb1_vga_out_port_prj1_vga_in_port)) *specificity=2.14; 

	if((x->Sis_active_prj1 && !(x->Sis_projecting_prj1_doc1) && !(x->Sis_locked_by_prj1_picture_scr1) && x->Sis_sending_nb1_doc1_vga_out_signal && x->Sis_connected_nb1_vga_out_port_prj1_vga_in_port)) {
		*x1 = *x;
		x1->Sis_projecting_prj1_doc1 = 1;
		x1->Sis_projecting_by_prj1_doc1_nb1 = 1;
		x1->Sis_locked_by_prj1_vga_in_port_prj1 = 1;
		x1->Sis_locked_by_nb1_vga_out_signal_prj1 = 1;
		x1->Sin_use_prj1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-project-document prj1 nb2 doc2), id 61 */
double action_begin_project_document_prj1_nb2_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_prj1 && !(x->Sis_projecting_prj1_doc2) && !(x->Sis_locked_by_prj1_picture_scr1) && x->Sis_sending_nb2_doc2_vga_out_signal && x->Sis_connected_nb2_vga_out_port_prj1_vga_in_port)) *specificity=2.14; 

	if((x->Sis_active_prj1 && !(x->Sis_projecting_prj1_doc2) && !(x->Sis_locked_by_prj1_picture_scr1) && x->Sis_sending_nb2_doc2_vga_out_signal && x->Sis_connected_nb2_vga_out_port_prj1_vga_in_port)) {
		*x1 = *x;
		x1->Sis_projecting_prj1_doc2 = 1;
		x1->Sis_projecting_by_prj1_doc2_nb2 = 1;
		x1->Sis_locked_by_prj1_vga_in_port_prj1 = 1;
		x1->Sis_locked_by_nb2_vga_out_signal_prj1 = 1;
		x1->Sin_use_prj1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-project-document prj1 nb3 doc3), id 62 */
double action_begin_project_document_prj1_nb3_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_prj1 && !(x->Sis_projecting_prj1_doc3) && !(x->Sis_locked_by_prj1_picture_scr1) && x->Sis_sending_nb3_doc3_vga_out_signal && x->Sis_connected_nb3_vga_out_port_prj1_vga_in_port)) *specificity=2.14; 

	if((x->Sis_active_prj1 && !(x->Sis_projecting_prj1_doc3) && !(x->Sis_locked_by_prj1_picture_scr1) && x->Sis_sending_nb3_doc3_vga_out_signal && x->Sis_connected_nb3_vga_out_port_prj1_vga_in_port)) {
		*x1 = *x;
		x1->Sis_projecting_prj1_doc3 = 1;
		x1->Sis_projecting_by_prj1_doc3_nb3 = 1;
		x1->Sis_locked_by_prj1_vga_in_port_prj1 = 1;
		x1->Sis_locked_by_nb3_vga_out_signal_prj1 = 1;
		x1->Sin_use_prj1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-project-document prj2 nb1 doc1), id 63 */
double action_begin_project_document_prj2_nb1_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_prj2 && !(x->Sis_projecting_prj2_doc1) && !(x->Sis_locked_by_prj2_picture_scr2) && x->Sis_sending_nb1_doc1_vga_out_signal && x->Sis_connected_nb1_vga_out_port_prj2_vga_in_port)) *specificity=2.14; 

	if((x->Sis_active_prj2 && !(x->Sis_projecting_prj2_doc1) && !(x->Sis_locked_by_prj2_picture_scr2) && x->Sis_sending_nb1_doc1_vga_out_signal && x->Sis_connected_nb1_vga_out_port_prj2_vga_in_port)) {
		*x1 = *x;
		x1->Sis_projecting_prj2_doc1 = 1;
		x1->Sis_projecting_by_prj2_doc1_nb1 = 1;
		x1->Sis_locked_by_prj2_vga_in_port_prj2 = 1;
		x1->Sis_locked_by_nb1_vga_out_signal_prj2 = 1;
		x1->Sin_use_prj2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-project-document prj2 nb2 doc2), id 64 */
double action_begin_project_document_prj2_nb2_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_prj2 && !(x->Sis_projecting_prj2_doc2) && !(x->Sis_locked_by_prj2_picture_scr2) && x->Sis_sending_nb2_doc2_vga_out_signal && x->Sis_connected_nb2_vga_out_port_prj2_vga_in_port)) *specificity=2.14; 

	if((x->Sis_active_prj2 && !(x->Sis_projecting_prj2_doc2) && !(x->Sis_locked_by_prj2_picture_scr2) && x->Sis_sending_nb2_doc2_vga_out_signal && x->Sis_connected_nb2_vga_out_port_prj2_vga_in_port)) {
		*x1 = *x;
		x1->Sis_projecting_prj2_doc2 = 1;
		x1->Sis_projecting_by_prj2_doc2_nb2 = 1;
		x1->Sis_locked_by_prj2_vga_in_port_prj2 = 1;
		x1->Sis_locked_by_nb2_vga_out_signal_prj2 = 1;
		x1->Sin_use_prj2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-project-document prj2 nb3 doc3), id 65 */
double action_begin_project_document_prj2_nb3_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_prj2 && !(x->Sis_projecting_prj2_doc3) && !(x->Sis_locked_by_prj2_picture_scr2) && x->Sis_sending_nb3_doc3_vga_out_signal && x->Sis_connected_nb3_vga_out_port_prj2_vga_in_port)) *specificity=2.14; 

	if((x->Sis_active_prj2 && !(x->Sis_projecting_prj2_doc3) && !(x->Sis_locked_by_prj2_picture_scr2) && x->Sis_sending_nb3_doc3_vga_out_signal && x->Sis_connected_nb3_vga_out_port_prj2_vga_in_port)) {
		*x1 = *x;
		x1->Sis_projecting_prj2_doc3 = 1;
		x1->Sis_projecting_by_prj2_doc3_nb3 = 1;
		x1->Sis_locked_by_prj2_vga_in_port_prj2 = 1;
		x1->Sis_locked_by_nb3_vga_out_signal_prj2 = 1;
		x1->Sin_use_prj2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-project-document prj3 nb1 doc1), id 66 */
double action_begin_project_document_prj3_nb1_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_prj3 && !(x->Sis_projecting_prj3_doc1) && !(x->Sis_locked_by_prj3_picture_scr3) && x->Sis_sending_nb1_doc1_vga_out_signal && x->Sis_connected_nb1_vga_out_port_prj3_vga_in_port)) *specificity=2.14; 

	if((x->Sis_active_prj3 && !(x->Sis_projecting_prj3_doc1) && !(x->Sis_locked_by_prj3_picture_scr3) && x->Sis_sending_nb1_doc1_vga_out_signal && x->Sis_connected_nb1_vga_out_port_prj3_vga_in_port)) {
		*x1 = *x;
		x1->Sis_projecting_prj3_doc1 = 1;
		x1->Sis_projecting_by_prj3_doc1_nb1 = 1;
		x1->Sis_locked_by_prj3_vga_in_port_prj3 = 1;
		x1->Sis_locked_by_nb1_vga_out_signal_prj3 = 1;
		x1->Sin_use_prj3 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-project-document prj3 nb2 doc2), id 67 */
double action_begin_project_document_prj3_nb2_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_prj3 && !(x->Sis_projecting_prj3_doc2) && !(x->Sis_locked_by_prj3_picture_scr3) && x->Sis_sending_nb2_doc2_vga_out_signal && x->Sis_connected_nb2_vga_out_port_prj3_vga_in_port)) *specificity=2.14; 

	if((x->Sis_active_prj3 && !(x->Sis_projecting_prj3_doc2) && !(x->Sis_locked_by_prj3_picture_scr3) && x->Sis_sending_nb2_doc2_vga_out_signal && x->Sis_connected_nb2_vga_out_port_prj3_vga_in_port)) {
		*x1 = *x;
		x1->Sis_projecting_prj3_doc2 = 1;
		x1->Sis_projecting_by_prj3_doc2_nb2 = 1;
		x1->Sis_locked_by_prj3_vga_in_port_prj3 = 1;
		x1->Sis_locked_by_nb2_vga_out_signal_prj3 = 1;
		x1->Sin_use_prj3 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-project-document prj3 nb3 doc3), id 68 */
double action_begin_project_document_prj3_nb3_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_prj3 && !(x->Sis_projecting_prj3_doc3) && !(x->Sis_locked_by_prj3_picture_scr3) && x->Sis_sending_nb3_doc3_vga_out_signal && x->Sis_connected_nb3_vga_out_port_prj3_vga_in_port)) *specificity=2.14; 

	if((x->Sis_active_prj3 && !(x->Sis_projecting_prj3_doc3) && !(x->Sis_locked_by_prj3_picture_scr3) && x->Sis_sending_nb3_doc3_vga_out_signal && x->Sis_connected_nb3_vga_out_port_prj3_vga_in_port)) {
		*x1 = *x;
		x1->Sis_projecting_prj3_doc3 = 1;
		x1->Sis_projecting_by_prj3_doc3_nb3 = 1;
		x1->Sis_locked_by_prj3_vga_in_port_prj3 = 1;
		x1->Sis_locked_by_nb3_vga_out_signal_prj3 = 1;
		x1->Sin_use_prj3 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (end-project-document prj1 nb1 doc1), id 69 */
double action_end_project_document_prj1_nb1_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_projecting_by_prj1_doc1_nb1 && !(x->Sis_locked_by_prj1_picture_scr1))) *specificity=1.14; 

	if((x->Sis_projecting_by_prj1_doc1_nb1 && !(x->Sis_locked_by_prj1_picture_scr1))) {
		*x1 = *x;
		x1->Sis_locked_by_prj1_vga_in_port_prj1 = 0;
		x1->Sis_locked_by_nb1_vga_out_signal_prj1 = 0;
		x1->Sis_projecting_prj1_doc1 = 0;
		x1->Sis_projecting_by_prj1_doc1_nb1 = 0;
		x1->Sin_use_prj1 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-project-document prj1 nb2 doc2), id 70 */
double action_end_project_document_prj1_nb2_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_projecting_by_prj1_doc2_nb2 && !(x->Sis_locked_by_prj1_picture_scr1))) *specificity=1.14; 

	if((x->Sis_projecting_by_prj1_doc2_nb2 && !(x->Sis_locked_by_prj1_picture_scr1))) {
		*x1 = *x;
		x1->Sis_locked_by_prj1_vga_in_port_prj1 = 0;
		x1->Sis_locked_by_nb2_vga_out_signal_prj1 = 0;
		x1->Sis_projecting_prj1_doc2 = 0;
		x1->Sis_projecting_by_prj1_doc2_nb2 = 0;
		x1->Sin_use_prj1 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-project-document prj1 nb3 doc3), id 71 */
double action_end_project_document_prj1_nb3_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_projecting_by_prj1_doc3_nb3 && !(x->Sis_locked_by_prj1_picture_scr1))) *specificity=1.14; 

	if((x->Sis_projecting_by_prj1_doc3_nb3 && !(x->Sis_locked_by_prj1_picture_scr1))) {
		*x1 = *x;
		x1->Sis_locked_by_prj1_vga_in_port_prj1 = 0;
		x1->Sis_locked_by_nb3_vga_out_signal_prj1 = 0;
		x1->Sis_projecting_prj1_doc3 = 0;
		x1->Sis_projecting_by_prj1_doc3_nb3 = 0;
		x1->Sin_use_prj1 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-project-document prj2 nb1 doc1), id 72 */
double action_end_project_document_prj2_nb1_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_projecting_by_prj2_doc1_nb1 && !(x->Sis_locked_by_prj2_picture_scr2))) *specificity=1.14; 

	if((x->Sis_projecting_by_prj2_doc1_nb1 && !(x->Sis_locked_by_prj2_picture_scr2))) {
		*x1 = *x;
		x1->Sis_locked_by_prj2_vga_in_port_prj2 = 0;
		x1->Sis_locked_by_nb1_vga_out_signal_prj2 = 0;
		x1->Sis_projecting_prj2_doc1 = 0;
		x1->Sis_projecting_by_prj2_doc1_nb1 = 0;
		x1->Sin_use_prj2 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-project-document prj2 nb2 doc2), id 73 */
double action_end_project_document_prj2_nb2_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_projecting_by_prj2_doc2_nb2 && !(x->Sis_locked_by_prj2_picture_scr2))) *specificity=1.14; 

	if((x->Sis_projecting_by_prj2_doc2_nb2 && !(x->Sis_locked_by_prj2_picture_scr2))) {
		*x1 = *x;
		x1->Sis_locked_by_prj2_vga_in_port_prj2 = 0;
		x1->Sis_locked_by_nb2_vga_out_signal_prj2 = 0;
		x1->Sis_projecting_prj2_doc2 = 0;
		x1->Sis_projecting_by_prj2_doc2_nb2 = 0;
		x1->Sin_use_prj2 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-project-document prj2 nb3 doc3), id 74 */
double action_end_project_document_prj2_nb3_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_projecting_by_prj2_doc3_nb3 && !(x->Sis_locked_by_prj2_picture_scr2))) *specificity=1.14; 

	if((x->Sis_projecting_by_prj2_doc3_nb3 && !(x->Sis_locked_by_prj2_picture_scr2))) {
		*x1 = *x;
		x1->Sis_locked_by_prj2_vga_in_port_prj2 = 0;
		x1->Sis_locked_by_nb3_vga_out_signal_prj2 = 0;
		x1->Sis_projecting_prj2_doc3 = 0;
		x1->Sis_projecting_by_prj2_doc3_nb3 = 0;
		x1->Sin_use_prj2 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-project-document prj3 nb1 doc1), id 75 */
double action_end_project_document_prj3_nb1_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_projecting_by_prj3_doc1_nb1 && !(x->Sis_locked_by_prj3_picture_scr3))) *specificity=1.14; 

	if((x->Sis_projecting_by_prj3_doc1_nb1 && !(x->Sis_locked_by_prj3_picture_scr3))) {
		*x1 = *x;
		x1->Sis_locked_by_prj3_vga_in_port_prj3 = 0;
		x1->Sis_locked_by_nb1_vga_out_signal_prj3 = 0;
		x1->Sis_projecting_prj3_doc1 = 0;
		x1->Sis_projecting_by_prj3_doc1_nb1 = 0;
		x1->Sin_use_prj3 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-project-document prj3 nb2 doc2), id 76 */
double action_end_project_document_prj3_nb2_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_projecting_by_prj3_doc2_nb2 && !(x->Sis_locked_by_prj3_picture_scr3))) *specificity=1.14; 

	if((x->Sis_projecting_by_prj3_doc2_nb2 && !(x->Sis_locked_by_prj3_picture_scr3))) {
		*x1 = *x;
		x1->Sis_locked_by_prj3_vga_in_port_prj3 = 0;
		x1->Sis_locked_by_nb2_vga_out_signal_prj3 = 0;
		x1->Sis_projecting_prj3_doc2 = 0;
		x1->Sis_projecting_by_prj3_doc2_nb2 = 0;
		x1->Sin_use_prj3 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-project-document prj3 nb3 doc3), id 77 */
double action_end_project_document_prj3_nb3_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_projecting_by_prj3_doc3_nb3 && !(x->Sis_locked_by_prj3_picture_scr3))) *specificity=1.14; 

	if((x->Sis_projecting_by_prj3_doc3_nb3 && !(x->Sis_locked_by_prj3_picture_scr3))) {
		*x1 = *x;
		x1->Sis_locked_by_prj3_vga_in_port_prj3 = 0;
		x1->Sis_locked_by_nb3_vga_out_signal_prj3 = 0;
		x1->Sis_projecting_prj3_doc3 = 0;
		x1->Sis_projecting_by_prj3_doc3_nb3 = 0;
		x1->Sin_use_prj3 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-show-document scr1 prj1 doc1), id 78 */
double action_begin_show_document_scr1_prj1_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_scr1 && !(x->Sis_showing_scr1_doc1) && x->Sis_projecting_prj1_doc1 && !(x->Sis_locked_by_scr1_picture_charlie) && !(x->Sis_locked_by_scr1_picture_alice))) *specificity=2.54; 

	if((x->Sis_active_scr1 && !(x->Sis_showing_scr1_doc1) && x->Sis_projecting_prj1_doc1 && !(x->Sis_locked_by_scr1_picture_charlie) && !(x->Sis_locked_by_scr1_picture_alice))) {
		*x1 = *x;
		x1->Sis_showing_scr1_doc1 = 1;
		x1->Sis_showing_by_scr1_doc1_prj1 = 1;
		x1->Sis_locked_by_prj1_picture_scr1 = 1;
		x1->Sin_use_scr1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-show-document scr1 prj1 doc2), id 79 */
double action_begin_show_document_scr1_prj1_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_scr1 && !(x->Sis_showing_scr1_doc2) && x->Sis_projecting_prj1_doc2 && !(x->Sis_locked_by_scr1_picture_charlie) && !(x->Sis_locked_by_scr1_picture_alice))) *specificity=2.54; 

	if((x->Sis_active_scr1 && !(x->Sis_showing_scr1_doc2) && x->Sis_projecting_prj1_doc2 && !(x->Sis_locked_by_scr1_picture_charlie) && !(x->Sis_locked_by_scr1_picture_alice))) {
		*x1 = *x;
		x1->Sis_showing_scr1_doc2 = 1;
		x1->Sis_showing_by_scr1_doc2_prj1 = 1;
		x1->Sis_locked_by_prj1_picture_scr1 = 1;
		x1->Sin_use_scr1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-show-document scr1 prj1 doc3), id 80 */
double action_begin_show_document_scr1_prj1_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_scr1 && !(x->Sis_showing_scr1_doc3) && x->Sis_projecting_prj1_doc3 && !(x->Sis_locked_by_scr1_picture_charlie) && !(x->Sis_locked_by_scr1_picture_alice))) *specificity=2.54; 

	if((x->Sis_active_scr1 && !(x->Sis_showing_scr1_doc3) && x->Sis_projecting_prj1_doc3 && !(x->Sis_locked_by_scr1_picture_charlie) && !(x->Sis_locked_by_scr1_picture_alice))) {
		*x1 = *x;
		x1->Sis_showing_scr1_doc3 = 1;
		x1->Sis_showing_by_scr1_doc3_prj1 = 1;
		x1->Sis_locked_by_prj1_picture_scr1 = 1;
		x1->Sin_use_scr1 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-show-document scr2 prj2 doc1), id 81 */
double action_begin_show_document_scr2_prj2_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_scr2 && !(x->Sis_showing_scr2_doc1) && x->Sis_projecting_prj2_doc1 && !(x->Sis_locked_by_scr2_picture_charlie) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_bob))) *specificity=2.68; 

	if((x->Sis_active_scr2 && !(x->Sis_showing_scr2_doc1) && x->Sis_projecting_prj2_doc1 && !(x->Sis_locked_by_scr2_picture_charlie) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_bob))) {
		*x1 = *x;
		x1->Sis_showing_scr2_doc1 = 1;
		x1->Sis_showing_by_scr2_doc1_prj2 = 1;
		x1->Sis_locked_by_prj2_picture_scr2 = 1;
		x1->Sin_use_scr2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-show-document scr2 prj2 doc2), id 82 */
double action_begin_show_document_scr2_prj2_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_scr2 && !(x->Sis_showing_scr2_doc2) && x->Sis_projecting_prj2_doc2 && !(x->Sis_locked_by_scr2_picture_charlie) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_bob))) *specificity=2.68; 

	if((x->Sis_active_scr2 && !(x->Sis_showing_scr2_doc2) && x->Sis_projecting_prj2_doc2 && !(x->Sis_locked_by_scr2_picture_charlie) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_bob))) {
		*x1 = *x;
		x1->Sis_showing_scr2_doc2 = 1;
		x1->Sis_showing_by_scr2_doc2_prj2 = 1;
		x1->Sis_locked_by_prj2_picture_scr2 = 1;
		x1->Sin_use_scr2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-show-document scr2 prj2 doc3), id 83 */
double action_begin_show_document_scr2_prj2_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_scr2 && !(x->Sis_showing_scr2_doc3) && x->Sis_projecting_prj2_doc3 && !(x->Sis_locked_by_scr2_picture_charlie) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_bob))) *specificity=2.68; 

	if((x->Sis_active_scr2 && !(x->Sis_showing_scr2_doc3) && x->Sis_projecting_prj2_doc3 && !(x->Sis_locked_by_scr2_picture_charlie) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_bob))) {
		*x1 = *x;
		x1->Sis_showing_scr2_doc3 = 1;
		x1->Sis_showing_by_scr2_doc3_prj2 = 1;
		x1->Sis_locked_by_prj2_picture_scr2 = 1;
		x1->Sin_use_scr2 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-show-document scr3 prj3 doc1), id 84 */
double action_begin_show_document_scr3_prj3_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_scr3 && !(x->Sis_showing_scr3_doc1) && x->Sis_projecting_prj3_doc1 && !(x->Sis_locked_by_scr3_picture_alice) && !(x->Sis_locked_by_scr3_picture_bob))) *specificity=2.54; 

	if((x->Sis_active_scr3 && !(x->Sis_showing_scr3_doc1) && x->Sis_projecting_prj3_doc1 && !(x->Sis_locked_by_scr3_picture_alice) && !(x->Sis_locked_by_scr3_picture_bob))) {
		*x1 = *x;
		x1->Sis_showing_scr3_doc1 = 1;
		x1->Sis_showing_by_scr3_doc1_prj3 = 1;
		x1->Sis_locked_by_prj3_picture_scr3 = 1;
		x1->Sin_use_scr3 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-show-document scr3 prj3 doc2), id 85 */
double action_begin_show_document_scr3_prj3_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_scr3 && !(x->Sis_showing_scr3_doc2) && x->Sis_projecting_prj3_doc2 && !(x->Sis_locked_by_scr3_picture_alice) && !(x->Sis_locked_by_scr3_picture_bob))) *specificity=2.54; 

	if((x->Sis_active_scr3 && !(x->Sis_showing_scr3_doc2) && x->Sis_projecting_prj3_doc2 && !(x->Sis_locked_by_scr3_picture_alice) && !(x->Sis_locked_by_scr3_picture_bob))) {
		*x1 = *x;
		x1->Sis_showing_scr3_doc2 = 1;
		x1->Sis_showing_by_scr3_doc2_prj3 = 1;
		x1->Sis_locked_by_prj3_picture_scr3 = 1;
		x1->Sin_use_scr3 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-show-document scr3 prj3 doc3), id 86 */
double action_begin_show_document_scr3_prj3_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_active_scr3 && !(x->Sis_showing_scr3_doc3) && x->Sis_projecting_prj3_doc3 && !(x->Sis_locked_by_scr3_picture_alice) && !(x->Sis_locked_by_scr3_picture_bob))) *specificity=2.54; 

	if((x->Sis_active_scr3 && !(x->Sis_showing_scr3_doc3) && x->Sis_projecting_prj3_doc3 && !(x->Sis_locked_by_scr3_picture_alice) && !(x->Sis_locked_by_scr3_picture_bob))) {
		*x1 = *x;
		x1->Sis_showing_scr3_doc3 = 1;
		x1->Sis_showing_by_scr3_doc3_prj3 = 1;
		x1->Sis_locked_by_prj3_picture_scr3 = 1;
		x1->Sin_use_scr3 = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (end-show-document scr1 prj1 doc1), id 87 */
double action_end_show_document_scr1_prj1_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_showing_by_scr1_doc1_prj1 && !(x->Sis_locked_by_scr1_picture_charlie) && !(x->Sis_locked_by_scr1_picture_alice))) *specificity=1.29; 

	if((x->Sis_showing_by_scr1_doc1_prj1 && !(x->Sis_locked_by_scr1_picture_charlie) && !(x->Sis_locked_by_scr1_picture_alice))) {
		*x1 = *x;
		x1->Sis_showing_scr1_doc1 = 0;
		x1->Sis_showing_by_scr1_doc1_prj1 = 0;
		x1->Sis_locked_by_prj1_picture_scr1 = 0;
		x1->Sin_use_scr1 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-show-document scr1 prj1 doc2), id 88 */
double action_end_show_document_scr1_prj1_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_showing_by_scr1_doc2_prj1 && !(x->Sis_locked_by_scr1_picture_charlie) && !(x->Sis_locked_by_scr1_picture_alice))) *specificity=1.29; 

	if((x->Sis_showing_by_scr1_doc2_prj1 && !(x->Sis_locked_by_scr1_picture_charlie) && !(x->Sis_locked_by_scr1_picture_alice))) {
		*x1 = *x;
		x1->Sis_showing_scr1_doc2 = 0;
		x1->Sis_showing_by_scr1_doc2_prj1 = 0;
		x1->Sis_locked_by_prj1_picture_scr1 = 0;
		x1->Sin_use_scr1 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-show-document scr1 prj1 doc3), id 89 */
double action_end_show_document_scr1_prj1_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_showing_by_scr1_doc3_prj1 && !(x->Sis_locked_by_scr1_picture_charlie) && !(x->Sis_locked_by_scr1_picture_alice))) *specificity=1.29; 

	if((x->Sis_showing_by_scr1_doc3_prj1 && !(x->Sis_locked_by_scr1_picture_charlie) && !(x->Sis_locked_by_scr1_picture_alice))) {
		*x1 = *x;
		x1->Sis_showing_scr1_doc3 = 0;
		x1->Sis_showing_by_scr1_doc3_prj1 = 0;
		x1->Sis_locked_by_prj1_picture_scr1 = 0;
		x1->Sin_use_scr1 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-show-document scr2 prj2 doc1), id 90 */
double action_end_show_document_scr2_prj2_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_showing_by_scr2_doc1_prj2 && !(x->Sis_locked_by_scr2_picture_charlie) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_bob))) *specificity=1.43; 

	if((x->Sis_showing_by_scr2_doc1_prj2 && !(x->Sis_locked_by_scr2_picture_charlie) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_bob))) {
		*x1 = *x;
		x1->Sis_showing_scr2_doc1 = 0;
		x1->Sis_showing_by_scr2_doc1_prj2 = 0;
		x1->Sis_locked_by_prj2_picture_scr2 = 0;
		x1->Sin_use_scr2 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-show-document scr2 prj2 doc2), id 91 */
double action_end_show_document_scr2_prj2_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_showing_by_scr2_doc2_prj2 && !(x->Sis_locked_by_scr2_picture_charlie) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_bob))) *specificity=1.43; 

	if((x->Sis_showing_by_scr2_doc2_prj2 && !(x->Sis_locked_by_scr2_picture_charlie) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_bob))) {
		*x1 = *x;
		x1->Sis_showing_scr2_doc2 = 0;
		x1->Sis_showing_by_scr2_doc2_prj2 = 0;
		x1->Sis_locked_by_prj2_picture_scr2 = 0;
		x1->Sin_use_scr2 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-show-document scr2 prj2 doc3), id 92 */
double action_end_show_document_scr2_prj2_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_showing_by_scr2_doc3_prj2 && !(x->Sis_locked_by_scr2_picture_charlie) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_bob))) *specificity=1.43; 

	if((x->Sis_showing_by_scr2_doc3_prj2 && !(x->Sis_locked_by_scr2_picture_charlie) && !(x->Sis_locked_by_scr2_picture_alice) && !(x->Sis_locked_by_scr2_picture_bob))) {
		*x1 = *x;
		x1->Sis_showing_scr2_doc3 = 0;
		x1->Sis_showing_by_scr2_doc3_prj2 = 0;
		x1->Sis_locked_by_prj2_picture_scr2 = 0;
		x1->Sin_use_scr2 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-show-document scr3 prj3 doc1), id 93 */
double action_end_show_document_scr3_prj3_doc1(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_showing_by_scr3_doc1_prj3 && !(x->Sis_locked_by_scr3_picture_alice) && !(x->Sis_locked_by_scr3_picture_bob))) *specificity=1.29; 

	if((x->Sis_showing_by_scr3_doc1_prj3 && !(x->Sis_locked_by_scr3_picture_alice) && !(x->Sis_locked_by_scr3_picture_bob))) {
		*x1 = *x;
		x1->Sis_showing_scr3_doc1 = 0;
		x1->Sis_showing_by_scr3_doc1_prj3 = 0;
		x1->Sis_locked_by_prj3_picture_scr3 = 0;
		x1->Sin_use_scr3 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-show-document scr3 prj3 doc2), id 94 */
double action_end_show_document_scr3_prj3_doc2(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_showing_by_scr3_doc2_prj3 && !(x->Sis_locked_by_scr3_picture_alice) && !(x->Sis_locked_by_scr3_picture_bob))) *specificity=1.29; 

	if((x->Sis_showing_by_scr3_doc2_prj3 && !(x->Sis_locked_by_scr3_picture_alice) && !(x->Sis_locked_by_scr3_picture_bob))) {
		*x1 = *x;
		x1->Sis_showing_scr3_doc2 = 0;
		x1->Sis_showing_by_scr3_doc2_prj3 = 0;
		x1->Sis_locked_by_prj3_picture_scr3 = 0;
		x1->Sin_use_scr3 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (end-show-document scr3 prj3 doc3), id 95 */
double action_end_show_document_scr3_prj3_doc3(StatePtr x, StateRec *x1, double *specificity) {
	if((x->Sis_showing_by_scr3_doc3_prj3 && !(x->Sis_locked_by_scr3_picture_alice) && !(x->Sis_locked_by_scr3_picture_bob))) *specificity=1.29; 

	if((x->Sis_showing_by_scr3_doc3_prj3 && !(x->Sis_locked_by_scr3_picture_alice) && !(x->Sis_locked_by_scr3_picture_bob))) {
		*x1 = *x;
		x1->Sis_showing_scr3_doc3 = 0;
		x1->Sis_showing_by_scr3_doc3_prj3 = 0;
		x1->Sis_locked_by_prj3_picture_scr3 = 0;
		x1->Sin_use_scr3 = 0;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-looking alice scr1 doc1 seatB), id 96 */
double action_begin_looking_alice_scr1_doc1_seatB(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_showing_scr1_doc1) *specificity=1.00; 

	if(x->Sis_showing_scr1_doc1) {
		*x1 = *x;
		x1->Scan_see_alice_doc1 = 1;
		x1->Sis_locked_by_alice_attention_alice = 1;
		x1->Sis_locked_by_scr1_picture_alice = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-looking alice scr1 doc2 seatB), id 97 */
double action_begin_looking_alice_scr1_doc2_seatB(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_showing_scr1_doc2) *specificity=1.00; 

	if(x->Sis_showing_scr1_doc2) {
		*x1 = *x;
		x1->Scan_see_alice_doc2 = 1;
		x1->Sis_locked_by_alice_attention_alice = 1;
		x1->Sis_locked_by_scr1_picture_alice = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-looking alice scr1 doc3 seatB), id 98 */
double action_begin_looking_alice_scr1_doc3_seatB(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_showing_scr1_doc3) *specificity=0.50; 

	if(x->Sis_showing_scr1_doc3) {
		*x1 = *x;
		x1->Scan_see_alice_doc3 = 1;
		x1->Sis_locked_by_alice_attention_alice = 1;
		x1->Sis_locked_by_scr1_picture_alice = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-looking alice scr2 doc1 seatB), id 99 */
double action_begin_looking_alice_scr2_doc1_seatB(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_showing_scr2_doc1) *specificity=0.50; 

	if(x->Sis_showing_scr2_doc1) {
		*x1 = *x;
		x1->Scan_see_alice_doc1 = 1;
		x1->Sis_locked_by_alice_attention_alice = 1;
		x1->Sis_locked_by_scr2_picture_alice = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-looking alice scr2 doc2 seatB), id 100 */
double action_begin_looking_alice_scr2_doc2_seatB(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_showing_scr2_doc2) *specificity=1.00; 

	if(x->Sis_showing_scr2_doc2) {
		*x1 = *x;
		x1->Scan_see_alice_doc2 = 1;
		x1->Sis_locked_by_alice_attention_alice = 1;
		x1->Sis_locked_by_scr2_picture_alice = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-looking alice scr2 doc3 seatB), id 101 */
double action_begin_looking_alice_scr2_doc3_seatB(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_showing_scr2_doc3) *specificity=0.50; 

	if(x->Sis_showing_scr2_doc3) {
		*x1 = *x;
		x1->Scan_see_alice_doc3 = 1;
		x1->Sis_locked_by_alice_attention_alice = 1;
		x1->Sis_locked_by_scr2_picture_alice = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-looking alice scr3 doc1 seatB), id 102 */
double action_begin_looking_alice_scr3_doc1_seatB(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_showing_scr3_doc1) *specificity=0.50; 

	if(x->Sis_showing_scr3_doc1) {
		*x1 = *x;
		x1->Scan_see_alice_doc1 = 1;
		x1->Sis_locked_by_alice_attention_alice = 1;
		x1->Sis_locked_by_scr3_picture_alice = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-looking alice scr3 doc2 seatB), id 103 */
double action_begin_looking_alice_scr3_doc2_seatB(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_showing_scr3_doc2) *specificity=1.00; 

	if(x->Sis_showing_scr3_doc2) {
		*x1 = *x;
		x1->Scan_see_alice_doc2 = 1;
		x1->Sis_locked_by_alice_attention_alice = 1;
		x1->Sis_locked_by_scr3_picture_alice = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-looking alice scr3 doc3 seatB), id 104 */
double action_begin_looking_alice_scr3_doc3_seatB(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_showing_scr3_doc3) *specificity=1.00; 

	if(x->Sis_showing_scr3_doc3) {
		*x1 = *x;
		x1->Scan_see_alice_doc3 = 1;
		x1->Sis_locked_by_alice_attention_alice = 1;
		x1->Sis_locked_by_scr3_picture_alice = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-looking bob scr2 doc1 seatA), id 105 */
double action_begin_looking_bob_scr2_doc1_seatA(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_showing_scr2_doc1) *specificity=0.50; 

	if(x->Sis_showing_scr2_doc1) {
		*x1 = *x;
		x1->Scan_see_bob_doc1 = 1;
		x1->Sis_locked_by_bob_attention_bob = 1;
		x1->Sis_locked_by_scr2_picture_bob = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-looking bob scr3 doc1 seatA), id 106 */
double action_begin_looking_bob_scr3_doc1_seatA(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_showing_scr3_doc1) *specificity=0.50; 

	if(x->Sis_showing_scr3_doc1) {
		*x1 = *x;
		x1->Scan_see_bob_doc1 = 1;
		x1->Sis_locked_by_bob_attention_bob = 1;
		x1->Sis_locked_by_scr3_picture_bob = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-looking charlie scr1 doc3 seatC), id 107 */
double action_begin_looking_charlie_scr1_doc3_seatC(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_showing_scr1_doc3) *specificity=0.50; 

	if(x->Sis_showing_scr1_doc3) {
		*x1 = *x;
		x1->Scan_see_charlie_doc3 = 1;
		x1->Sis_locked_by_charlie_attention_charlie = 1;
		x1->Sis_locked_by_scr1_picture_charlie = 1;
		return 1;
	} else {
		return 0;
	}
}

/* (begin-looking charlie scr2 doc3 seatC), id 108 */
double action_begin_looking_charlie_scr2_doc3_seatC(StatePtr x, StateRec *x1, double *specificity) {
	if(x->Sis_showing_scr2_doc3) *specificity=0.50; 

	if(x->Sis_showing_scr2_doc3) {
		*x1 = *x;
		x1->Scan_see_charlie_doc3 = 1;
		x1->Sis_locked_by_charlie_attention_charlie = 1;
		x1->Sis_locked_by_scr2_picture_charlie = 1;
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
		r->Sis_active_nb1=1; 
	} else 
		 return (exp(rT[0]-t))/1;
	return -1;
}
double getRecOrRefr1 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_nb2=1; 
	} else 
		 return (exp(rT[1]-t))/1;
	return -1;
}
double getRecOrRefr2 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_nb3=1; 
	} else 
		 return (exp(rT[2]-t))/1;
	return -1;
}
double getRecOrRefr3 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj1=1; 
	} else 
		 return (exp(rT[3]-t))/1;
	return -1;
}
double getRecOrRefr4 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj2=1; 
	} else 
		 return (exp(rT[4]-t))/1;
	return -1;
}
double getRecOrRefr5 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj3=1; 
	} else 
		 return (exp(rT[5]-t))/1;
	return -1;
}
double getRecOrRefr6 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr1=1; 
	} else 
		 return (exp(rT[6]-t))/1;
	return -1;
}
double getRecOrRefr7 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr2=1; 
	} else 
		 return (exp(rT[7]-t))/1;
	return -1;
}
double getRecOrRefr8 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr3=1; 
	} else 
		 return (exp(rT[8]-t))/1;
	return -1;
}
double getRecOrRefr9 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_nb1=1; r->Sin_use_nb1=1; r->Sis_locked_by_nb1_vga_out_signal_prj3=1; r->Sis_locked_by_nb1_vga_out_signal_prj1=1; r->Sis_locked_by_nb1_vga_out_port_crossbar=1; r->Sis_locked_by_nb1_vga_out_signal_prj2=1; 
	} else 
		 return (exp(rT[0]-t)+exp(rT[9]-t)+exp(rT[24]-t)+exp(rT[22]-t)+exp(rT[21]-t)+exp(rT[23]-t))/6;
	return -1;
}
double getRecOrRefr10 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_nb2=1; r->Sin_use_nb2=1; r->Sis_locked_by_nb2_vga_out_signal_prj3=1; r->Sis_locked_by_nb2_vga_out_signal_prj1=1; r->Sis_locked_by_nb2_vga_out_port_crossbar=1; r->Sis_locked_by_nb2_vga_out_signal_prj2=1; 
	} else 
		 return (exp(rT[1]-t)+exp(rT[10]-t)+exp(rT[28]-t)+exp(rT[26]-t)+exp(rT[25]-t)+exp(rT[27]-t))/6;
	return -1;
}
double getRecOrRefr11 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_nb3=1; r->Sin_use_nb3=1; r->Sis_locked_by_nb3_vga_out_signal_prj3=1; r->Sis_locked_by_nb3_vga_out_signal_prj1=1; r->Sis_locked_by_nb3_vga_out_port_crossbar=1; r->Sis_locked_by_nb3_vga_out_signal_prj2=1; 
	} else 
		 return (exp(rT[2]-t)+exp(rT[11]-t)+exp(rT[32]-t)+exp(rT[30]-t)+exp(rT[29]-t)+exp(rT[31]-t))/6;
	return -1;
}
double getRecOrRefr12 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj1=1; r->Sin_use_prj1=1; r->Sis_locked_by_prj1_picture_scr1=1; r->Sis_locked_by_prj1_vga_in_port_crossbar=1; r->Sis_locked_by_prj1_vga_in_port_prj1=1; 
	} else 
		 return (exp(rT[3]-t)+exp(rT[12]-t)+exp(rT[35]-t)+exp(rT[34]-t)+exp(rT[33]-t))/5;
	return -1;
}
double getRecOrRefr13 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj2=1; r->Sin_use_prj2=1; r->Sis_locked_by_prj2_vga_in_port_prj2=1; r->Sis_locked_by_prj2_vga_in_port_crossbar=1; r->Sis_locked_by_prj2_picture_scr2=1; 
	} else 
		 return (exp(rT[4]-t)+exp(rT[13]-t)+exp(rT[36]-t)+exp(rT[37]-t)+exp(rT[38]-t))/5;
	return -1;
}
double getRecOrRefr14 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj3=1; r->Sin_use_prj3=1; r->Sis_locked_by_prj3_picture_scr3=1; r->Sis_locked_by_prj3_vga_in_port_crossbar=1; r->Sis_locked_by_prj3_vga_in_port_prj3=1; 
	} else 
		 return (exp(rT[5]-t)+exp(rT[14]-t)+exp(rT[41]-t)+exp(rT[40]-t)+exp(rT[39]-t))/5;
	return -1;
}
double getRecOrRefr15 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr1=1; r->Sin_use_scr1=1; r->Sis_locked_by_scr1_picture_alice=1; r->Sis_locked_by_scr1_picture_charlie=1; 
	} else 
		 return (exp(rT[6]-t)+exp(rT[15]-t)+exp(rT[42]-t)+exp(rT[43]-t))/4;
	return -1;
}
double getRecOrRefr16 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr2=1; r->Sin_use_scr2=1; r->Sis_locked_by_scr2_picture_bob=1; r->Sis_locked_by_scr2_picture_alice=1; r->Sis_locked_by_scr2_picture_charlie=1; 
	} else 
		 return (exp(rT[7]-t)+exp(rT[16]-t)+exp(rT[45]-t)+exp(rT[44]-t)+exp(rT[46]-t))/5;
	return -1;
}
double getRecOrRefr17 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr3=1; r->Sin_use_scr3=1; r->Sis_locked_by_scr3_picture_bob=1; r->Sis_locked_by_scr3_picture_alice=1; 
	} else 
		 return (exp(rT[8]-t)+exp(rT[17]-t)+exp(rT[48]-t)+exp(rT[47]-t))/4;
	return -1;
}
double getRecOrRefr18 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_nb1=1; r->Sis_sending_nb1_doc1_vga_out_signal=1; r->Sis_locked_by_nb1_vga_out_signal_prj3=1; r->Sis_locked_by_nb1_vga_out_signal_prj1=1; r->Sis_locked_by_nb1_vga_out_signal_prj2=1; 
	} else 
		 return (exp(rT[0]-t)+exp(rT[49]-t)+exp(rT[24]-t)+exp(rT[22]-t)+exp(rT[23]-t))/5;
	return -1;
}
double getRecOrRefr19 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_nb2=1; r->Sis_sending_nb2_doc2_vga_out_signal=1; r->Sis_locked_by_nb2_vga_out_signal_prj3=1; r->Sis_locked_by_nb2_vga_out_signal_prj1=1; r->Sis_locked_by_nb2_vga_out_signal_prj2=1; 
	} else 
		 return (exp(rT[1]-t)+exp(rT[50]-t)+exp(rT[28]-t)+exp(rT[26]-t)+exp(rT[27]-t))/5;
	return -1;
}
double getRecOrRefr20 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_nb3=1; r->Sis_sending_nb3_doc3_vga_out_signal=1; r->Sis_locked_by_nb3_vga_out_signal_prj3=1; r->Sis_locked_by_nb3_vga_out_signal_prj1=1; r->Sis_locked_by_nb3_vga_out_signal_prj2=1; 
	} else 
		 return (exp(rT[2]-t)+exp(rT[51]-t)+exp(rT[32]-t)+exp(rT[30]-t)+exp(rT[31]-t))/5;
	return -1;
}
double getRecOrRefr21 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_sending_nb1_doc1_vga_out_signal=1; r->Sis_locked_by_nb1_vga_out_signal_prj3=1; r->Sis_locked_by_nb1_vga_out_signal_prj1=1; r->Sis_locked_by_nb1_vga_out_signal_prj2=1; 
	} else 
		 return (exp(rT[49]-t)+exp(rT[24]-t)+exp(rT[22]-t)+exp(rT[23]-t))/4;
	return -1;
}
double getRecOrRefr22 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_sending_nb2_doc2_vga_out_signal=1; r->Sis_locked_by_nb2_vga_out_signal_prj3=1; r->Sis_locked_by_nb2_vga_out_signal_prj1=1; r->Sis_locked_by_nb2_vga_out_signal_prj2=1; 
	} else 
		 return (exp(rT[50]-t)+exp(rT[28]-t)+exp(rT[26]-t)+exp(rT[27]-t))/4;
	return -1;
}
double getRecOrRefr23 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_sending_nb3_doc3_vga_out_signal=1; r->Sis_locked_by_nb3_vga_out_signal_prj3=1; r->Sis_locked_by_nb3_vga_out_signal_prj1=1; r->Sis_locked_by_nb3_vga_out_signal_prj2=1; 
	} else 
		 return (exp(rT[51]-t)+exp(rT[32]-t)+exp(rT[30]-t)+exp(rT[31]-t))/4;
	return -1;
}
double getRecOrRefr24 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb1_vga_out_port_prj1_vga_in_port=1; r->Sis_locked_by_prj1_vga_in_port_prj1=1; r->Sis_locked_by_prj1_vga_in_port_crossbar=1; r->Sis_switched_i1_o1=1; 
	} else 
		 return (exp(rT[52]-t)+exp(rT[33]-t)+exp(rT[34]-t)+exp(rT[105]-t))/4;
	return -1;
}
double getRecOrRefr25 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb1_vga_out_port_prj2_vga_in_port=1; r->Sis_locked_by_prj2_vga_in_port_crossbar=1; r->Sis_locked_by_prj2_vga_in_port_prj2=1; r->Sis_switched_i1_o2=1; 
	} else 
		 return (exp(rT[53]-t)+exp(rT[37]-t)+exp(rT[36]-t)+exp(rT[106]-t))/4;
	return -1;
}
double getRecOrRefr26 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb1_vga_out_port_prj3_vga_in_port=1; r->Sis_locked_by_prj3_vga_in_port_prj3=1; r->Sis_locked_by_prj3_vga_in_port_crossbar=1; r->Sis_switched_i1_o3=1; 
	} else 
		 return (exp(rT[54]-t)+exp(rT[39]-t)+exp(rT[40]-t)+exp(rT[107]-t))/4;
	return -1;
}
double getRecOrRefr27 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb2_vga_out_port_prj1_vga_in_port=1; r->Sis_locked_by_prj1_vga_in_port_prj1=1; r->Sis_locked_by_prj1_vga_in_port_crossbar=1; r->Sis_switched_i2_o1=1; 
	} else 
		 return (exp(rT[55]-t)+exp(rT[33]-t)+exp(rT[34]-t)+exp(rT[108]-t))/4;
	return -1;
}
double getRecOrRefr28 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb2_vga_out_port_prj2_vga_in_port=1; r->Sis_locked_by_prj2_vga_in_port_crossbar=1; r->Sis_locked_by_prj2_vga_in_port_prj2=1; r->Sis_switched_i2_o2=1; 
	} else 
		 return (exp(rT[56]-t)+exp(rT[37]-t)+exp(rT[36]-t)+exp(rT[109]-t))/4;
	return -1;
}
double getRecOrRefr29 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb2_vga_out_port_prj3_vga_in_port=1; r->Sis_locked_by_prj3_vga_in_port_prj3=1; r->Sis_locked_by_prj3_vga_in_port_crossbar=1; r->Sis_switched_i2_o3=1; 
	} else 
		 return (exp(rT[57]-t)+exp(rT[39]-t)+exp(rT[40]-t)+exp(rT[110]-t))/4;
	return -1;
}
double getRecOrRefr30 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb3_vga_out_port_prj1_vga_in_port=1; r->Sis_locked_by_prj1_vga_in_port_prj1=1; r->Sis_locked_by_prj1_vga_in_port_crossbar=1; r->Sis_switched_i3_o1=1; 
	} else 
		 return (exp(rT[58]-t)+exp(rT[33]-t)+exp(rT[34]-t)+exp(rT[111]-t))/4;
	return -1;
}
double getRecOrRefr31 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb3_vga_out_port_prj2_vga_in_port=1; r->Sis_locked_by_prj2_vga_in_port_crossbar=1; r->Sis_locked_by_prj2_vga_in_port_prj2=1; r->Sis_switched_i3_o2=1; 
	} else 
		 return (exp(rT[59]-t)+exp(rT[37]-t)+exp(rT[36]-t)+exp(rT[112]-t))/4;
	return -1;
}
double getRecOrRefr32 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb3_vga_out_port_prj3_vga_in_port=1; r->Sis_locked_by_prj3_vga_in_port_prj3=1; r->Sis_locked_by_prj3_vga_in_port_crossbar=1; r->Sis_switched_i3_o3=1; 
	} else 
		 return (exp(rT[60]-t)+exp(rT[39]-t)+exp(rT[40]-t)+exp(rT[113]-t))/4;
	return -1;
}
double getRecOrRefr33 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb1_vga_out_port_prj1_vga_in_port=1; r->Sis_locked_by_prj1_vga_in_port_prj1=1; 
	} else 
		 return (exp(rT[52]-t)+exp(rT[33]-t))/2;
	return -1;
}
double getRecOrRefr34 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb1_vga_out_port_prj2_vga_in_port=1; r->Sis_locked_by_prj2_vga_in_port_prj2=1; 
	} else 
		 return (exp(rT[53]-t)+exp(rT[36]-t))/2;
	return -1;
}
double getRecOrRefr35 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb1_vga_out_port_prj3_vga_in_port=1; r->Sis_locked_by_prj3_vga_in_port_prj3=1; 
	} else 
		 return (exp(rT[54]-t)+exp(rT[39]-t))/2;
	return -1;
}
double getRecOrRefr36 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb2_vga_out_port_prj1_vga_in_port=1; r->Sis_locked_by_prj1_vga_in_port_prj1=1; 
	} else 
		 return (exp(rT[55]-t)+exp(rT[33]-t))/2;
	return -1;
}
double getRecOrRefr37 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb2_vga_out_port_prj2_vga_in_port=1; r->Sis_locked_by_prj2_vga_in_port_prj2=1; 
	} else 
		 return (exp(rT[56]-t)+exp(rT[36]-t))/2;
	return -1;
}
double getRecOrRefr38 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb2_vga_out_port_prj3_vga_in_port=1; r->Sis_locked_by_prj3_vga_in_port_prj3=1; 
	} else 
		 return (exp(rT[57]-t)+exp(rT[39]-t))/2;
	return -1;
}
double getRecOrRefr39 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb3_vga_out_port_prj1_vga_in_port=1; r->Sis_locked_by_prj1_vga_in_port_prj1=1; 
	} else 
		 return (exp(rT[58]-t)+exp(rT[33]-t))/2;
	return -1;
}
double getRecOrRefr40 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb3_vga_out_port_prj2_vga_in_port=1; r->Sis_locked_by_prj2_vga_in_port_prj2=1; 
	} else 
		 return (exp(rT[59]-t)+exp(rT[36]-t))/2;
	return -1;
}
double getRecOrRefr41 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_connected_nb3_vga_out_port_prj3_vga_in_port=1; r->Sis_locked_by_prj3_vga_in_port_prj3=1; 
	} else 
		 return (exp(rT[60]-t)+exp(rT[39]-t))/2;
	return -1;
}
double getRecOrRefr42 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_open_o1=1; 
	} else 
		 return (exp(rT[102]-t))/1;
	return -1;
}
double getRecOrRefr43 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_open_o2=1; 
	} else 
		 return (exp(rT[103]-t))/1;
	return -1;
}
double getRecOrRefr44 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_open_o3=1; 
	} else 
		 return (exp(rT[104]-t))/1;
	return -1;
}
double getRecOrRefr45 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_open_o1=1; 
	} else 
		 return (exp(rT[102]-t))/1;
	return -1;
}
double getRecOrRefr46 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_open_o2=1; 
	} else 
		 return (exp(rT[103]-t))/1;
	return -1;
}
double getRecOrRefr47 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_open_o3=1; 
	} else 
		 return (exp(rT[104]-t))/1;
	return -1;
}
double getRecOrRefr48 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_open_o1=1; 
	} else 
		 return (exp(rT[102]-t))/1;
	return -1;
}
double getRecOrRefr49 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_open_o2=1; 
	} else 
		 return (exp(rT[103]-t))/1;
	return -1;
}
double getRecOrRefr50 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_open_o3=1; 
	} else 
		 return (exp(rT[104]-t))/1;
	return -1;
}
double getRecOrRefr51 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_switched_i1_o1=1; 
	} else 
		 return (exp(rT[105]-t))/1;
	return -1;
}
double getRecOrRefr52 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_switched_i1_o2=1; 
	} else 
		 return (exp(rT[106]-t))/1;
	return -1;
}
double getRecOrRefr53 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_switched_i1_o3=1; 
	} else 
		 return (exp(rT[107]-t))/1;
	return -1;
}
double getRecOrRefr54 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_switched_i2_o1=1; 
	} else 
		 return (exp(rT[108]-t))/1;
	return -1;
}
double getRecOrRefr55 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_switched_i2_o2=1; 
	} else 
		 return (exp(rT[109]-t))/1;
	return -1;
}
double getRecOrRefr56 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_switched_i2_o3=1; 
	} else 
		 return (exp(rT[110]-t))/1;
	return -1;
}
double getRecOrRefr57 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_switched_i3_o1=1; 
	} else 
		 return (exp(rT[111]-t))/1;
	return -1;
}
double getRecOrRefr58 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_switched_i3_o2=1; 
	} else 
		 return (exp(rT[112]-t))/1;
	return -1;
}
double getRecOrRefr59 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_switched_i3_o3=1; 
	} else 
		 return (exp(rT[113]-t))/1;
	return -1;
}
double getRecOrRefr60 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj1=1; r->Sis_projecting_prj1_doc1=1; r->Sis_locked_by_prj1_picture_scr1=1; r->Sis_sending_nb1_doc1_vga_out_signal=1; r->Sis_connected_nb1_vga_out_port_prj1_vga_in_port=1; 
	} else 
		 return (exp(rT[3]-t)+exp(rT[70]-t)+exp(rT[35]-t)+exp(rT[49]-t)+exp(rT[52]-t))/5;
	return -1;
}
double getRecOrRefr61 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj1=1; r->Sis_projecting_prj1_doc2=1; r->Sis_locked_by_prj1_picture_scr1=1; r->Sis_sending_nb2_doc2_vga_out_signal=1; r->Sis_connected_nb2_vga_out_port_prj1_vga_in_port=1; 
	} else 
		 return (exp(rT[3]-t)+exp(rT[71]-t)+exp(rT[35]-t)+exp(rT[50]-t)+exp(rT[55]-t))/5;
	return -1;
}
double getRecOrRefr62 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj1=1; r->Sis_projecting_prj1_doc3=1; r->Sis_locked_by_prj1_picture_scr1=1; r->Sis_sending_nb3_doc3_vga_out_signal=1; r->Sis_connected_nb3_vga_out_port_prj1_vga_in_port=1; 
	} else 
		 return (exp(rT[3]-t)+exp(rT[72]-t)+exp(rT[35]-t)+exp(rT[51]-t)+exp(rT[58]-t))/5;
	return -1;
}
double getRecOrRefr63 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj2=1; r->Sis_projecting_prj2_doc1=1; r->Sis_locked_by_prj2_picture_scr2=1; r->Sis_sending_nb1_doc1_vga_out_signal=1; r->Sis_connected_nb1_vga_out_port_prj2_vga_in_port=1; 
	} else 
		 return (exp(rT[4]-t)+exp(rT[73]-t)+exp(rT[38]-t)+exp(rT[49]-t)+exp(rT[53]-t))/5;
	return -1;
}
double getRecOrRefr64 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj2=1; r->Sis_projecting_prj2_doc2=1; r->Sis_locked_by_prj2_picture_scr2=1; r->Sis_sending_nb2_doc2_vga_out_signal=1; r->Sis_connected_nb2_vga_out_port_prj2_vga_in_port=1; 
	} else 
		 return (exp(rT[4]-t)+exp(rT[74]-t)+exp(rT[38]-t)+exp(rT[50]-t)+exp(rT[56]-t))/5;
	return -1;
}
double getRecOrRefr65 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj2=1; r->Sis_projecting_prj2_doc3=1; r->Sis_locked_by_prj2_picture_scr2=1; r->Sis_sending_nb3_doc3_vga_out_signal=1; r->Sis_connected_nb3_vga_out_port_prj2_vga_in_port=1; 
	} else 
		 return (exp(rT[4]-t)+exp(rT[75]-t)+exp(rT[38]-t)+exp(rT[51]-t)+exp(rT[59]-t))/5;
	return -1;
}
double getRecOrRefr66 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj3=1; r->Sis_projecting_prj3_doc1=1; r->Sis_locked_by_prj3_picture_scr3=1; r->Sis_sending_nb1_doc1_vga_out_signal=1; r->Sis_connected_nb1_vga_out_port_prj3_vga_in_port=1; 
	} else 
		 return (exp(rT[5]-t)+exp(rT[76]-t)+exp(rT[41]-t)+exp(rT[49]-t)+exp(rT[54]-t))/5;
	return -1;
}
double getRecOrRefr67 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj3=1; r->Sis_projecting_prj3_doc2=1; r->Sis_locked_by_prj3_picture_scr3=1; r->Sis_sending_nb2_doc2_vga_out_signal=1; r->Sis_connected_nb2_vga_out_port_prj3_vga_in_port=1; 
	} else 
		 return (exp(rT[5]-t)+exp(rT[77]-t)+exp(rT[41]-t)+exp(rT[50]-t)+exp(rT[57]-t))/5;
	return -1;
}
double getRecOrRefr68 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_prj3=1; r->Sis_projecting_prj3_doc3=1; r->Sis_locked_by_prj3_picture_scr3=1; r->Sis_sending_nb3_doc3_vga_out_signal=1; r->Sis_connected_nb3_vga_out_port_prj3_vga_in_port=1; 
	} else 
		 return (exp(rT[5]-t)+exp(rT[78]-t)+exp(rT[41]-t)+exp(rT[51]-t)+exp(rT[60]-t))/5;
	return -1;
}
double getRecOrRefr69 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_projecting_by_prj1_doc1_nb1=1; r->Sis_locked_by_prj1_picture_scr1=1; 
	} else 
		 return (exp(rT[61]-t)+exp(rT[35]-t))/2;
	return -1;
}
double getRecOrRefr70 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_projecting_by_prj1_doc2_nb2=1; r->Sis_locked_by_prj1_picture_scr1=1; 
	} else 
		 return (exp(rT[62]-t)+exp(rT[35]-t))/2;
	return -1;
}
double getRecOrRefr71 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_projecting_by_prj1_doc3_nb3=1; r->Sis_locked_by_prj1_picture_scr1=1; 
	} else 
		 return (exp(rT[63]-t)+exp(rT[35]-t))/2;
	return -1;
}
double getRecOrRefr72 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_projecting_by_prj2_doc1_nb1=1; r->Sis_locked_by_prj2_picture_scr2=1; 
	} else 
		 return (exp(rT[64]-t)+exp(rT[38]-t))/2;
	return -1;
}
double getRecOrRefr73 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_projecting_by_prj2_doc2_nb2=1; r->Sis_locked_by_prj2_picture_scr2=1; 
	} else 
		 return (exp(rT[65]-t)+exp(rT[38]-t))/2;
	return -1;
}
double getRecOrRefr74 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_projecting_by_prj2_doc3_nb3=1; r->Sis_locked_by_prj2_picture_scr2=1; 
	} else 
		 return (exp(rT[66]-t)+exp(rT[38]-t))/2;
	return -1;
}
double getRecOrRefr75 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_projecting_by_prj3_doc1_nb1=1; r->Sis_locked_by_prj3_picture_scr3=1; 
	} else 
		 return (exp(rT[67]-t)+exp(rT[41]-t))/2;
	return -1;
}
double getRecOrRefr76 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_projecting_by_prj3_doc2_nb2=1; r->Sis_locked_by_prj3_picture_scr3=1; 
	} else 
		 return (exp(rT[68]-t)+exp(rT[41]-t))/2;
	return -1;
}
double getRecOrRefr77 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_projecting_by_prj3_doc3_nb3=1; r->Sis_locked_by_prj3_picture_scr3=1; 
	} else 
		 return (exp(rT[69]-t)+exp(rT[41]-t))/2;
	return -1;
}
double getRecOrRefr78 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr1=1; r->Sis_showing_scr1_doc1=1; r->Sis_projecting_prj1_doc1=1; r->Sis_locked_by_scr1_picture_charlie=1; r->Sis_locked_by_scr1_picture_alice=1; 
	} else 
		 return (exp(rT[6]-t)+exp(rT[88]-t)+exp(rT[70]-t)+exp(rT[43]-t)+exp(rT[42]-t))/5;
	return -1;
}
double getRecOrRefr79 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr1=1; r->Sis_showing_scr1_doc2=1; r->Sis_projecting_prj1_doc2=1; r->Sis_locked_by_scr1_picture_charlie=1; r->Sis_locked_by_scr1_picture_alice=1; 
	} else 
		 return (exp(rT[6]-t)+exp(rT[89]-t)+exp(rT[71]-t)+exp(rT[43]-t)+exp(rT[42]-t))/5;
	return -1;
}
double getRecOrRefr80 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr1=1; r->Sis_showing_scr1_doc3=1; r->Sis_projecting_prj1_doc3=1; r->Sis_locked_by_scr1_picture_charlie=1; r->Sis_locked_by_scr1_picture_alice=1; 
	} else 
		 return (exp(rT[6]-t)+exp(rT[90]-t)+exp(rT[72]-t)+exp(rT[43]-t)+exp(rT[42]-t))/5;
	return -1;
}
double getRecOrRefr81 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr2=1; r->Sis_showing_scr2_doc1=1; r->Sis_projecting_prj2_doc1=1; r->Sis_locked_by_scr2_picture_charlie=1; r->Sis_locked_by_scr2_picture_alice=1; r->Sis_locked_by_scr2_picture_bob=1; 
	} else 
		 return (exp(rT[7]-t)+exp(rT[91]-t)+exp(rT[73]-t)+exp(rT[46]-t)+exp(rT[44]-t)+exp(rT[45]-t))/6;
	return -1;
}
double getRecOrRefr82 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr2=1; r->Sis_showing_scr2_doc2=1; r->Sis_projecting_prj2_doc2=1; r->Sis_locked_by_scr2_picture_charlie=1; r->Sis_locked_by_scr2_picture_alice=1; r->Sis_locked_by_scr2_picture_bob=1; 
	} else 
		 return (exp(rT[7]-t)+exp(rT[92]-t)+exp(rT[74]-t)+exp(rT[46]-t)+exp(rT[44]-t)+exp(rT[45]-t))/6;
	return -1;
}
double getRecOrRefr83 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr2=1; r->Sis_showing_scr2_doc3=1; r->Sis_projecting_prj2_doc3=1; r->Sis_locked_by_scr2_picture_charlie=1; r->Sis_locked_by_scr2_picture_alice=1; r->Sis_locked_by_scr2_picture_bob=1; 
	} else 
		 return (exp(rT[7]-t)+exp(rT[93]-t)+exp(rT[75]-t)+exp(rT[46]-t)+exp(rT[44]-t)+exp(rT[45]-t))/6;
	return -1;
}
double getRecOrRefr84 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr3=1; r->Sis_showing_scr3_doc1=1; r->Sis_projecting_prj3_doc1=1; r->Sis_locked_by_scr3_picture_alice=1; r->Sis_locked_by_scr3_picture_bob=1; 
	} else 
		 return (exp(rT[8]-t)+exp(rT[94]-t)+exp(rT[76]-t)+exp(rT[47]-t)+exp(rT[48]-t))/5;
	return -1;
}
double getRecOrRefr85 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr3=1; r->Sis_showing_scr3_doc2=1; r->Sis_projecting_prj3_doc2=1; r->Sis_locked_by_scr3_picture_alice=1; r->Sis_locked_by_scr3_picture_bob=1; 
	} else 
		 return (exp(rT[8]-t)+exp(rT[95]-t)+exp(rT[77]-t)+exp(rT[47]-t)+exp(rT[48]-t))/5;
	return -1;
}
double getRecOrRefr86 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_active_scr3=1; r->Sis_showing_scr3_doc3=1; r->Sis_projecting_prj3_doc3=1; r->Sis_locked_by_scr3_picture_alice=1; r->Sis_locked_by_scr3_picture_bob=1; 
	} else 
		 return (exp(rT[8]-t)+exp(rT[96]-t)+exp(rT[78]-t)+exp(rT[47]-t)+exp(rT[48]-t))/5;
	return -1;
}
double getRecOrRefr87 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_by_scr1_doc1_prj1=1; r->Sis_locked_by_scr1_picture_charlie=1; r->Sis_locked_by_scr1_picture_alice=1; 
	} else 
		 return (exp(rT[79]-t)+exp(rT[43]-t)+exp(rT[42]-t))/3;
	return -1;
}
double getRecOrRefr88 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_by_scr1_doc2_prj1=1; r->Sis_locked_by_scr1_picture_charlie=1; r->Sis_locked_by_scr1_picture_alice=1; 
	} else 
		 return (exp(rT[80]-t)+exp(rT[43]-t)+exp(rT[42]-t))/3;
	return -1;
}
double getRecOrRefr89 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_by_scr1_doc3_prj1=1; r->Sis_locked_by_scr1_picture_charlie=1; r->Sis_locked_by_scr1_picture_alice=1; 
	} else 
		 return (exp(rT[81]-t)+exp(rT[43]-t)+exp(rT[42]-t))/3;
	return -1;
}
double getRecOrRefr90 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_by_scr2_doc1_prj2=1; r->Sis_locked_by_scr2_picture_charlie=1; r->Sis_locked_by_scr2_picture_alice=1; r->Sis_locked_by_scr2_picture_bob=1; 
	} else 
		 return (exp(rT[82]-t)+exp(rT[46]-t)+exp(rT[44]-t)+exp(rT[45]-t))/4;
	return -1;
}
double getRecOrRefr91 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_by_scr2_doc2_prj2=1; r->Sis_locked_by_scr2_picture_charlie=1; r->Sis_locked_by_scr2_picture_alice=1; r->Sis_locked_by_scr2_picture_bob=1; 
	} else 
		 return (exp(rT[83]-t)+exp(rT[46]-t)+exp(rT[44]-t)+exp(rT[45]-t))/4;
	return -1;
}
double getRecOrRefr92 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_by_scr2_doc3_prj2=1; r->Sis_locked_by_scr2_picture_charlie=1; r->Sis_locked_by_scr2_picture_alice=1; r->Sis_locked_by_scr2_picture_bob=1; 
	} else 
		 return (exp(rT[84]-t)+exp(rT[46]-t)+exp(rT[44]-t)+exp(rT[45]-t))/4;
	return -1;
}
double getRecOrRefr93 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_by_scr3_doc1_prj3=1; r->Sis_locked_by_scr3_picture_alice=1; r->Sis_locked_by_scr3_picture_bob=1; 
	} else 
		 return (exp(rT[85]-t)+exp(rT[47]-t)+exp(rT[48]-t))/3;
	return -1;
}
double getRecOrRefr94 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_by_scr3_doc2_prj3=1; r->Sis_locked_by_scr3_picture_alice=1; r->Sis_locked_by_scr3_picture_bob=1; 
	} else 
		 return (exp(rT[86]-t)+exp(rT[47]-t)+exp(rT[48]-t))/3;
	return -1;
}
double getRecOrRefr95 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_by_scr3_doc3_prj3=1; r->Sis_locked_by_scr3_picture_alice=1; r->Sis_locked_by_scr3_picture_bob=1; 
	} else 
		 return (exp(rT[87]-t)+exp(rT[47]-t)+exp(rT[48]-t))/3;
	return -1;
}
double getRecOrRefr96 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_scr1_doc1=1; 
	} else 
		 return (exp(rT[88]-t))/1;
	return -1;
}
double getRecOrRefr97 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_scr1_doc2=1; 
	} else 
		 return (exp(rT[89]-t))/1;
	return -1;
}
double getRecOrRefr98 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_scr1_doc3=1; 
	} else 
		 return (exp(rT[90]-t))/1;
	return -1;
}
double getRecOrRefr99 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_scr2_doc1=1; 
	} else 
		 return (exp(rT[91]-t))/1;
	return -1;
}
double getRecOrRefr100 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_scr2_doc2=1; 
	} else 
		 return (exp(rT[92]-t))/1;
	return -1;
}
double getRecOrRefr101 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_scr2_doc3=1; 
	} else 
		 return (exp(rT[93]-t))/1;
	return -1;
}
double getRecOrRefr102 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_scr3_doc1=1; 
	} else 
		 return (exp(rT[94]-t))/1;
	return -1;
}
double getRecOrRefr103 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_scr3_doc2=1; 
	} else 
		 return (exp(rT[95]-t))/1;
	return -1;
}
double getRecOrRefr104 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_scr3_doc3=1; 
	} else 
		 return (exp(rT[96]-t))/1;
	return -1;
}
double getRecOrRefr105 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_scr2_doc1=1; 
	} else 
		 return (exp(rT[91]-t))/1;
	return -1;
}
double getRecOrRefr106 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_scr3_doc1=1; 
	} else 
		 return (exp(rT[94]-t))/1;
	return -1;
}
double getRecOrRefr107 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_scr1_doc3=1; 
	} else 
		 return (exp(rT[90]-t))/1;
	return -1;
}
double getRecOrRefr108 (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {
	if (r != NULL) {
		r->Sis_showing_scr2_doc3=1; 
	} else 
		 return (exp(rT[93]-t))/1;
	return -1;
}


namespace ActionSchemes {
const ActionScheme s_FINISHED("FINISHED", std::vector<paramType>());
const ActionScheme s_BLOCKED("BLOCKED", std::vector<paramType>());
const ActionScheme s_INITIALIZE("INITIALIZE", std::vector<paramType>());
boost::array<paramType,1> __params_activate = {std::make_pair("d","device")};
const ActionScheme s_activate("activate", std::vector<paramType>(__params_activate.begin(), __params_activate.end()));
boost::array<paramType,1> __params_deactivate = {std::make_pair("d","device")};
const ActionScheme s_deactivate("deactivate", std::vector<paramType>(__params_deactivate.begin(), __params_deactivate.end()));
boost::array<paramType,2> __params_begin_send_document = {std::make_pair("n","notebook"),std::make_pair("d","document")};
const ActionScheme s_begin_send_document("begin-send-document", std::vector<paramType>(__params_begin_send_document.begin(), __params_begin_send_document.end()));
boost::array<paramType,2> __params_end_send_document = {std::make_pair("n","notebook"),std::make_pair("d","document")};
const ActionScheme s_end_send_document("end-send-document", std::vector<paramType>(__params_end_send_document.begin(), __params_end_send_document.end()));
boost::array<paramType,4> __params_begin_connect = {std::make_pair("n","notebook"),std::make_pair("p","projector"),std::make_pair("ip","cb-in-port"),std::make_pair("op","cb-out-port")};
const ActionScheme s_begin_connect("begin-connect", std::vector<paramType>(__params_begin_connect.begin(), __params_begin_connect.end()));
boost::array<paramType,2> __params_end_connect = {std::make_pair("n","notebook"),std::make_pair("p","projector")};
const ActionScheme s_end_connect("end-connect", std::vector<paramType>(__params_end_connect.begin(), __params_end_connect.end()));
boost::array<paramType,2> __params_xxx_device_crossbar_switch = {std::make_pair("ip","cb-in-port"),std::make_pair("op","cb-out-port")};
const ActionScheme s_xxx_device_crossbar_switch("xxx-device-crossbar-switch", std::vector<paramType>(__params_xxx_device_crossbar_switch.begin(), __params_xxx_device_crossbar_switch.end()));
boost::array<paramType,2> __params_xxx_device_crossbar_unswitch = {std::make_pair("ip","cb-in-port"),std::make_pair("op","cb-out-port")};
const ActionScheme s_xxx_device_crossbar_unswitch("xxx-device-crossbar-unswitch", std::vector<paramType>(__params_xxx_device_crossbar_unswitch.begin(), __params_xxx_device_crossbar_unswitch.end()));
boost::array<paramType,3> __params_begin_project_document = {std::make_pair("p","projector"),std::make_pair("n","notebook"),std::make_pair("d","document")};
const ActionScheme s_begin_project_document("begin-project-document", std::vector<paramType>(__params_begin_project_document.begin(), __params_begin_project_document.end()));
boost::array<paramType,3> __params_end_project_document = {std::make_pair("p","projector"),std::make_pair("n","notebook"),std::make_pair("d","document")};
const ActionScheme s_end_project_document("end-project-document", std::vector<paramType>(__params_end_project_document.begin(), __params_end_project_document.end()));
boost::array<paramType,3> __params_begin_show_document = {std::make_pair("s","screen"),std::make_pair("p","projector"),std::make_pair("d","document")};
const ActionScheme s_begin_show_document("begin-show-document", std::vector<paramType>(__params_begin_show_document.begin(), __params_begin_show_document.end()));
boost::array<paramType,3> __params_end_show_document = {std::make_pair("s","screen"),std::make_pair("p","projector"),std::make_pair("d","document")};
const ActionScheme s_end_show_document("end-show-document", std::vector<paramType>(__params_end_show_document.begin(), __params_end_show_document.end()));
boost::array<paramType,4> __params_begin_looking = {std::make_pair("p","person"),std::make_pair("s","screen"),std::make_pair("d","document"),std::make_pair("l","location")};
const ActionScheme s_begin_looking("begin-looking", std::vector<paramType>(__params_begin_looking.begin(), __params_begin_looking.end()));
} // namespace ActionSchemes

ActionScheme const * __actionSchemes[13 + 3] = {
	&ActionSchemes::s_FINISHED,
	&ActionSchemes::s_BLOCKED,
	&ActionSchemes::s_INITIALIZE,
	&ActionSchemes::s_activate,
	&ActionSchemes::s_deactivate,
	&ActionSchemes::s_begin_send_document,
	&ActionSchemes::s_end_send_document,
	&ActionSchemes::s_begin_connect,
	&ActionSchemes::s_end_connect,
	&ActionSchemes::s_xxx_device_crossbar_switch,
	&ActionSchemes::s_xxx_device_crossbar_unswitch,
	&ActionSchemes::s_begin_project_document,
	&ActionSchemes::s_end_project_document,
	&ActionSchemes::s_begin_show_document,
	&ActionSchemes::s_end_show_document,
	&ActionSchemes::s_begin_looking
};
ActionScheme const * *actionSchemes = &__actionSchemes[3];

namespace Actions {
const ActionSchemes::S_FINISHED a_FINISHED(FinishedOpId, "FINISHED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_BLOCKED a_BLOCKED(NoOpId, "BLOCKED", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);
const ActionSchemes::S_INITIALIZE a_INITIALIZE(InitOpId, "INITIALIZE", NULL, NULL, NULL, finishedImmediate , pStopcdfImmediate, pStoppdfImmediate, 0.0 /* Initial */);
const ActionSchemes::S_activate a_activate_nb1(0, "(activate nb1)", action_activate_nb1, NULL, getRecOrRefr0, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 0 */);
const ActionSchemes::S_activate a_activate_nb2(1, "(activate nb2)", action_activate_nb2, NULL, getRecOrRefr1, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 1 */);
const ActionSchemes::S_activate a_activate_nb3(2, "(activate nb3)", action_activate_nb3, NULL, getRecOrRefr2, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 2 */);
const ActionSchemes::S_activate a_activate_prj1(3, "(activate prj1)", action_activate_prj1, NULL, getRecOrRefr3, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 3 */);
const ActionSchemes::S_activate a_activate_prj2(4, "(activate prj2)", action_activate_prj2, NULL, getRecOrRefr4, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 4 */);
const ActionSchemes::S_activate a_activate_prj3(5, "(activate prj3)", action_activate_prj3, NULL, getRecOrRefr5, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 5 */);
const ActionSchemes::S_activate a_activate_scr1(6, "(activate scr1)", action_activate_scr1, NULL, getRecOrRefr6, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 6 */);
const ActionSchemes::S_activate a_activate_scr2(7, "(activate scr2)", action_activate_scr2, NULL, getRecOrRefr7, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 7 */);
const ActionSchemes::S_activate a_activate_scr3(8, "(activate scr3)", action_activate_scr3, NULL, getRecOrRefr8, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 8 */);
const ActionSchemes::S_deactivate a_deactivate_nb1(9, "(deactivate nb1)", action_deactivate_nb1, NULL, getRecOrRefr9, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 9 */);
const ActionSchemes::S_deactivate a_deactivate_nb2(10, "(deactivate nb2)", action_deactivate_nb2, NULL, getRecOrRefr10, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 10 */);
const ActionSchemes::S_deactivate a_deactivate_nb3(11, "(deactivate nb3)", action_deactivate_nb3, NULL, getRecOrRefr11, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 11 */);
const ActionSchemes::S_deactivate a_deactivate_prj1(12, "(deactivate prj1)", action_deactivate_prj1, NULL, getRecOrRefr12, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 12 */);
const ActionSchemes::S_deactivate a_deactivate_prj2(13, "(deactivate prj2)", action_deactivate_prj2, NULL, getRecOrRefr13, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 13 */);
const ActionSchemes::S_deactivate a_deactivate_prj3(14, "(deactivate prj3)", action_deactivate_prj3, NULL, getRecOrRefr14, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 14 */);
const ActionSchemes::S_deactivate a_deactivate_scr1(15, "(deactivate scr1)", action_deactivate_scr1, NULL, getRecOrRefr15, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 15 */);
const ActionSchemes::S_deactivate a_deactivate_scr2(16, "(deactivate scr2)", action_deactivate_scr2, NULL, getRecOrRefr16, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 16 */);
const ActionSchemes::S_deactivate a_deactivate_scr3(17, "(deactivate scr3)", action_deactivate_scr3, NULL, getRecOrRefr17, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 17 */);
const ActionSchemes::S_begin_send_document a_begin_send_document_nb1_doc1(18, "(begin-send-document nb1 doc1)", action_begin_send_document_nb1_doc1, NULL, getRecOrRefr18, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 18 */);
const ActionSchemes::S_begin_send_document a_begin_send_document_nb2_doc2(19, "(begin-send-document nb2 doc2)", action_begin_send_document_nb2_doc2, NULL, getRecOrRefr19, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 19 */);
const ActionSchemes::S_begin_send_document a_begin_send_document_nb3_doc3(20, "(begin-send-document nb3 doc3)", action_begin_send_document_nb3_doc3, NULL, getRecOrRefr20, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 20 */);
const ActionSchemes::S_end_send_document a_end_send_document_nb1_doc1(21, "(end-send-document nb1 doc1)", action_end_send_document_nb1_doc1, NULL, getRecOrRefr21, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 21 */);
const ActionSchemes::S_end_send_document a_end_send_document_nb2_doc2(22, "(end-send-document nb2 doc2)", action_end_send_document_nb2_doc2, NULL, getRecOrRefr22, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 22 */);
const ActionSchemes::S_end_send_document a_end_send_document_nb3_doc3(23, "(end-send-document nb3 doc3)", action_end_send_document_nb3_doc3, NULL, getRecOrRefr23, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 23 */);
const ActionSchemes::S_begin_connect a_begin_connect_nb1_prj1_i1_o1(24, "(begin-connect nb1 prj1 i1 o1)", action_begin_connect_nb1_prj1_i1_o1, NULL, getRecOrRefr24, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 24 */);
const ActionSchemes::S_begin_connect a_begin_connect_nb1_prj2_i1_o2(25, "(begin-connect nb1 prj2 i1 o2)", action_begin_connect_nb1_prj2_i1_o2, NULL, getRecOrRefr25, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 25 */);
const ActionSchemes::S_begin_connect a_begin_connect_nb1_prj3_i1_o3(26, "(begin-connect nb1 prj3 i1 o3)", action_begin_connect_nb1_prj3_i1_o3, NULL, getRecOrRefr26, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 26 */);
const ActionSchemes::S_begin_connect a_begin_connect_nb2_prj1_i2_o1(27, "(begin-connect nb2 prj1 i2 o1)", action_begin_connect_nb2_prj1_i2_o1, NULL, getRecOrRefr27, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 27 */);
const ActionSchemes::S_begin_connect a_begin_connect_nb2_prj2_i2_o2(28, "(begin-connect nb2 prj2 i2 o2)", action_begin_connect_nb2_prj2_i2_o2, NULL, getRecOrRefr28, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 28 */);
const ActionSchemes::S_begin_connect a_begin_connect_nb2_prj3_i2_o3(29, "(begin-connect nb2 prj3 i2 o3)", action_begin_connect_nb2_prj3_i2_o3, NULL, getRecOrRefr29, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 29 */);
const ActionSchemes::S_begin_connect a_begin_connect_nb3_prj1_i3_o1(30, "(begin-connect nb3 prj1 i3 o1)", action_begin_connect_nb3_prj1_i3_o1, NULL, getRecOrRefr30, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 30 */);
const ActionSchemes::S_begin_connect a_begin_connect_nb3_prj2_i3_o2(31, "(begin-connect nb3 prj2 i3 o2)", action_begin_connect_nb3_prj2_i3_o2, NULL, getRecOrRefr31, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 31 */);
const ActionSchemes::S_begin_connect a_begin_connect_nb3_prj3_i3_o3(32, "(begin-connect nb3 prj3 i3 o3)", action_begin_connect_nb3_prj3_i3_o3, NULL, getRecOrRefr32, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 32 */);
const ActionSchemes::S_end_connect a_end_connect_nb1_prj1(33, "(end-connect nb1 prj1)", action_end_connect_nb1_prj1, NULL, getRecOrRefr33, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 33 */);
const ActionSchemes::S_end_connect a_end_connect_nb1_prj2(34, "(end-connect nb1 prj2)", action_end_connect_nb1_prj2, NULL, getRecOrRefr34, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 34 */);
const ActionSchemes::S_end_connect a_end_connect_nb1_prj3(35, "(end-connect nb1 prj3)", action_end_connect_nb1_prj3, NULL, getRecOrRefr35, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 35 */);
const ActionSchemes::S_end_connect a_end_connect_nb2_prj1(36, "(end-connect nb2 prj1)", action_end_connect_nb2_prj1, NULL, getRecOrRefr36, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 36 */);
const ActionSchemes::S_end_connect a_end_connect_nb2_prj2(37, "(end-connect nb2 prj2)", action_end_connect_nb2_prj2, NULL, getRecOrRefr37, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 37 */);
const ActionSchemes::S_end_connect a_end_connect_nb2_prj3(38, "(end-connect nb2 prj3)", action_end_connect_nb2_prj3, NULL, getRecOrRefr38, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 38 */);
const ActionSchemes::S_end_connect a_end_connect_nb3_prj1(39, "(end-connect nb3 prj1)", action_end_connect_nb3_prj1, NULL, getRecOrRefr39, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 39 */);
const ActionSchemes::S_end_connect a_end_connect_nb3_prj2(40, "(end-connect nb3 prj2)", action_end_connect_nb3_prj2, NULL, getRecOrRefr40, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 40 */);
const ActionSchemes::S_end_connect a_end_connect_nb3_prj3(41, "(end-connect nb3 prj3)", action_end_connect_nb3_prj3, NULL, getRecOrRefr41, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 41 */);
const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i1_o1(42, "(xxx-device-crossbar-switch i1 o1)", action_xxx_device_crossbar_switch_i1_o1, NULL, getRecOrRefr42, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 42 */);
const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i1_o2(43, "(xxx-device-crossbar-switch i1 o2)", action_xxx_device_crossbar_switch_i1_o2, NULL, getRecOrRefr43, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 43 */);
const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i1_o3(44, "(xxx-device-crossbar-switch i1 o3)", action_xxx_device_crossbar_switch_i1_o3, NULL, getRecOrRefr44, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 44 */);
const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i2_o1(45, "(xxx-device-crossbar-switch i2 o1)", action_xxx_device_crossbar_switch_i2_o1, NULL, getRecOrRefr45, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 45 */);
const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i2_o2(46, "(xxx-device-crossbar-switch i2 o2)", action_xxx_device_crossbar_switch_i2_o2, NULL, getRecOrRefr46, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 46 */);
const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i2_o3(47, "(xxx-device-crossbar-switch i2 o3)", action_xxx_device_crossbar_switch_i2_o3, NULL, getRecOrRefr47, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 47 */);
const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i3_o1(48, "(xxx-device-crossbar-switch i3 o1)", action_xxx_device_crossbar_switch_i3_o1, NULL, getRecOrRefr48, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 48 */);
const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i3_o2(49, "(xxx-device-crossbar-switch i3 o2)", action_xxx_device_crossbar_switch_i3_o2, NULL, getRecOrRefr49, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 49 */);
const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i3_o3(50, "(xxx-device-crossbar-switch i3 o3)", action_xxx_device_crossbar_switch_i3_o3, NULL, getRecOrRefr50, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 50 */);
const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i1_o1(51, "(xxx-device-crossbar-unswitch i1 o1)", action_xxx_device_crossbar_unswitch_i1_o1, NULL, getRecOrRefr51, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 51 */);
const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i1_o2(52, "(xxx-device-crossbar-unswitch i1 o2)", action_xxx_device_crossbar_unswitch_i1_o2, NULL, getRecOrRefr52, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 52 */);
const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i1_o3(53, "(xxx-device-crossbar-unswitch i1 o3)", action_xxx_device_crossbar_unswitch_i1_o3, NULL, getRecOrRefr53, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 53 */);
const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i2_o1(54, "(xxx-device-crossbar-unswitch i2 o1)", action_xxx_device_crossbar_unswitch_i2_o1, NULL, getRecOrRefr54, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 54 */);
const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i2_o2(55, "(xxx-device-crossbar-unswitch i2 o2)", action_xxx_device_crossbar_unswitch_i2_o2, NULL, getRecOrRefr55, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 55 */);
const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i2_o3(56, "(xxx-device-crossbar-unswitch i2 o3)", action_xxx_device_crossbar_unswitch_i2_o3, NULL, getRecOrRefr56, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 56 */);
const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i3_o1(57, "(xxx-device-crossbar-unswitch i3 o1)", action_xxx_device_crossbar_unswitch_i3_o1, NULL, getRecOrRefr57, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 57 */);
const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i3_o2(58, "(xxx-device-crossbar-unswitch i3 o2)", action_xxx_device_crossbar_unswitch_i3_o2, NULL, getRecOrRefr58, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 58 */);
const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i3_o3(59, "(xxx-device-crossbar-unswitch i3 o3)", action_xxx_device_crossbar_unswitch_i3_o3, NULL, getRecOrRefr59, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 59 */);
const ActionSchemes::S_begin_project_document a_begin_project_document_prj1_nb1_doc1(60, "(begin-project-document prj1 nb1 doc1)", action_begin_project_document_prj1_nb1_doc1, NULL, getRecOrRefr60, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 60 */);
const ActionSchemes::S_begin_project_document a_begin_project_document_prj1_nb2_doc2(61, "(begin-project-document prj1 nb2 doc2)", action_begin_project_document_prj1_nb2_doc2, NULL, getRecOrRefr61, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 61 */);
const ActionSchemes::S_begin_project_document a_begin_project_document_prj1_nb3_doc3(62, "(begin-project-document prj1 nb3 doc3)", action_begin_project_document_prj1_nb3_doc3, NULL, getRecOrRefr62, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 62 */);
const ActionSchemes::S_begin_project_document a_begin_project_document_prj2_nb1_doc1(63, "(begin-project-document prj2 nb1 doc1)", action_begin_project_document_prj2_nb1_doc1, NULL, getRecOrRefr63, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 63 */);
const ActionSchemes::S_begin_project_document a_begin_project_document_prj2_nb2_doc2(64, "(begin-project-document prj2 nb2 doc2)", action_begin_project_document_prj2_nb2_doc2, NULL, getRecOrRefr64, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 64 */);
const ActionSchemes::S_begin_project_document a_begin_project_document_prj2_nb3_doc3(65, "(begin-project-document prj2 nb3 doc3)", action_begin_project_document_prj2_nb3_doc3, NULL, getRecOrRefr65, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 65 */);
const ActionSchemes::S_begin_project_document a_begin_project_document_prj3_nb1_doc1(66, "(begin-project-document prj3 nb1 doc1)", action_begin_project_document_prj3_nb1_doc1, NULL, getRecOrRefr66, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 66 */);
const ActionSchemes::S_begin_project_document a_begin_project_document_prj3_nb2_doc2(67, "(begin-project-document prj3 nb2 doc2)", action_begin_project_document_prj3_nb2_doc2, NULL, getRecOrRefr67, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 67 */);
const ActionSchemes::S_begin_project_document a_begin_project_document_prj3_nb3_doc3(68, "(begin-project-document prj3 nb3 doc3)", action_begin_project_document_prj3_nb3_doc3, NULL, getRecOrRefr68, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 68 */);
const ActionSchemes::S_end_project_document a_end_project_document_prj1_nb1_doc1(69, "(end-project-document prj1 nb1 doc1)", action_end_project_document_prj1_nb1_doc1, NULL, getRecOrRefr69, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 69 */);
const ActionSchemes::S_end_project_document a_end_project_document_prj1_nb2_doc2(70, "(end-project-document prj1 nb2 doc2)", action_end_project_document_prj1_nb2_doc2, NULL, getRecOrRefr70, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 70 */);
const ActionSchemes::S_end_project_document a_end_project_document_prj1_nb3_doc3(71, "(end-project-document prj1 nb3 doc3)", action_end_project_document_prj1_nb3_doc3, NULL, getRecOrRefr71, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 71 */);
const ActionSchemes::S_end_project_document a_end_project_document_prj2_nb1_doc1(72, "(end-project-document prj2 nb1 doc1)", action_end_project_document_prj2_nb1_doc1, NULL, getRecOrRefr72, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 72 */);
const ActionSchemes::S_end_project_document a_end_project_document_prj2_nb2_doc2(73, "(end-project-document prj2 nb2 doc2)", action_end_project_document_prj2_nb2_doc2, NULL, getRecOrRefr73, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 73 */);
const ActionSchemes::S_end_project_document a_end_project_document_prj2_nb3_doc3(74, "(end-project-document prj2 nb3 doc3)", action_end_project_document_prj2_nb3_doc3, NULL, getRecOrRefr74, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 74 */);
const ActionSchemes::S_end_project_document a_end_project_document_prj3_nb1_doc1(75, "(end-project-document prj3 nb1 doc1)", action_end_project_document_prj3_nb1_doc1, NULL, getRecOrRefr75, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 75 */);
const ActionSchemes::S_end_project_document a_end_project_document_prj3_nb2_doc2(76, "(end-project-document prj3 nb2 doc2)", action_end_project_document_prj3_nb2_doc2, NULL, getRecOrRefr76, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 76 */);
const ActionSchemes::S_end_project_document a_end_project_document_prj3_nb3_doc3(77, "(end-project-document prj3 nb3 doc3)", action_end_project_document_prj3_nb3_doc3, NULL, getRecOrRefr77, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 77 */);
const ActionSchemes::S_begin_show_document a_begin_show_document_scr1_prj1_doc1(78, "(begin-show-document scr1 prj1 doc1)", action_begin_show_document_scr1_prj1_doc1, NULL, getRecOrRefr78, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 78 */);
const ActionSchemes::S_begin_show_document a_begin_show_document_scr1_prj1_doc2(79, "(begin-show-document scr1 prj1 doc2)", action_begin_show_document_scr1_prj1_doc2, NULL, getRecOrRefr79, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 79 */);
const ActionSchemes::S_begin_show_document a_begin_show_document_scr1_prj1_doc3(80, "(begin-show-document scr1 prj1 doc3)", action_begin_show_document_scr1_prj1_doc3, NULL, getRecOrRefr80, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 80 */);
const ActionSchemes::S_begin_show_document a_begin_show_document_scr2_prj2_doc1(81, "(begin-show-document scr2 prj2 doc1)", action_begin_show_document_scr2_prj2_doc1, NULL, getRecOrRefr81, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 81 */);
const ActionSchemes::S_begin_show_document a_begin_show_document_scr2_prj2_doc2(82, "(begin-show-document scr2 prj2 doc2)", action_begin_show_document_scr2_prj2_doc2, NULL, getRecOrRefr82, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 82 */);
const ActionSchemes::S_begin_show_document a_begin_show_document_scr2_prj2_doc3(83, "(begin-show-document scr2 prj2 doc3)", action_begin_show_document_scr2_prj2_doc3, NULL, getRecOrRefr83, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 83 */);
const ActionSchemes::S_begin_show_document a_begin_show_document_scr3_prj3_doc1(84, "(begin-show-document scr3 prj3 doc1)", action_begin_show_document_scr3_prj3_doc1, NULL, getRecOrRefr84, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 84 */);
const ActionSchemes::S_begin_show_document a_begin_show_document_scr3_prj3_doc2(85, "(begin-show-document scr3 prj3 doc2)", action_begin_show_document_scr3_prj3_doc2, NULL, getRecOrRefr85, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 85 */);
const ActionSchemes::S_begin_show_document a_begin_show_document_scr3_prj3_doc3(86, "(begin-show-document scr3 prj3 doc3)", action_begin_show_document_scr3_prj3_doc3, NULL, getRecOrRefr86, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 86 */);
const ActionSchemes::S_end_show_document a_end_show_document_scr1_prj1_doc1(87, "(end-show-document scr1 prj1 doc1)", action_end_show_document_scr1_prj1_doc1, NULL, getRecOrRefr87, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 87 */);
const ActionSchemes::S_end_show_document a_end_show_document_scr1_prj1_doc2(88, "(end-show-document scr1 prj1 doc2)", action_end_show_document_scr1_prj1_doc2, NULL, getRecOrRefr88, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 88 */);
const ActionSchemes::S_end_show_document a_end_show_document_scr1_prj1_doc3(89, "(end-show-document scr1 prj1 doc3)", action_end_show_document_scr1_prj1_doc3, NULL, getRecOrRefr89, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 89 */);
const ActionSchemes::S_end_show_document a_end_show_document_scr2_prj2_doc1(90, "(end-show-document scr2 prj2 doc1)", action_end_show_document_scr2_prj2_doc1, NULL, getRecOrRefr90, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 90 */);
const ActionSchemes::S_end_show_document a_end_show_document_scr2_prj2_doc2(91, "(end-show-document scr2 prj2 doc2)", action_end_show_document_scr2_prj2_doc2, NULL, getRecOrRefr91, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 91 */);
const ActionSchemes::S_end_show_document a_end_show_document_scr2_prj2_doc3(92, "(end-show-document scr2 prj2 doc3)", action_end_show_document_scr2_prj2_doc3, NULL, getRecOrRefr92, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 92 */);
const ActionSchemes::S_end_show_document a_end_show_document_scr3_prj3_doc1(93, "(end-show-document scr3 prj3 doc1)", action_end_show_document_scr3_prj3_doc1, NULL, getRecOrRefr93, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 93 */);
const ActionSchemes::S_end_show_document a_end_show_document_scr3_prj3_doc2(94, "(end-show-document scr3 prj3 doc2)", action_end_show_document_scr3_prj3_doc2, NULL, getRecOrRefr94, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 94 */);
const ActionSchemes::S_end_show_document a_end_show_document_scr3_prj3_doc3(95, "(end-show-document scr3 prj3 doc3)", action_end_show_document_scr3_prj3_doc3, NULL, getRecOrRefr95, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 95 */);
const ActionSchemes::S_begin_looking a_begin_looking_alice_scr1_doc1_seatB(96, "(begin-looking alice scr1 doc1 seatB)", action_begin_looking_alice_scr1_doc1_seatB, NULL, getRecOrRefr96, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 96 */);
const ActionSchemes::S_begin_looking a_begin_looking_alice_scr1_doc2_seatB(97, "(begin-looking alice scr1 doc2 seatB)", action_begin_looking_alice_scr1_doc2_seatB, NULL, getRecOrRefr97, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 97 */);
const ActionSchemes::S_begin_looking a_begin_looking_alice_scr1_doc3_seatB(98, "(begin-looking alice scr1 doc3 seatB)", action_begin_looking_alice_scr1_doc3_seatB, NULL, getRecOrRefr98, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 98 */);
const ActionSchemes::S_begin_looking a_begin_looking_alice_scr2_doc1_seatB(99, "(begin-looking alice scr2 doc1 seatB)", action_begin_looking_alice_scr2_doc1_seatB, NULL, getRecOrRefr99, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 99 */);
const ActionSchemes::S_begin_looking a_begin_looking_alice_scr2_doc2_seatB(100, "(begin-looking alice scr2 doc2 seatB)", action_begin_looking_alice_scr2_doc2_seatB, NULL, getRecOrRefr100, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 100 */);
const ActionSchemes::S_begin_looking a_begin_looking_alice_scr2_doc3_seatB(101, "(begin-looking alice scr2 doc3 seatB)", action_begin_looking_alice_scr2_doc3_seatB, NULL, getRecOrRefr101, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 101 */);
const ActionSchemes::S_begin_looking a_begin_looking_alice_scr3_doc1_seatB(102, "(begin-looking alice scr3 doc1 seatB)", action_begin_looking_alice_scr3_doc1_seatB, NULL, getRecOrRefr102, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 102 */);
const ActionSchemes::S_begin_looking a_begin_looking_alice_scr3_doc2_seatB(103, "(begin-looking alice scr3 doc2 seatB)", action_begin_looking_alice_scr3_doc2_seatB, NULL, getRecOrRefr103, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 103 */);
const ActionSchemes::S_begin_looking a_begin_looking_alice_scr3_doc3_seatB(104, "(begin-looking alice scr3 doc3 seatB)", action_begin_looking_alice_scr3_doc3_seatB, NULL, getRecOrRefr104, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 104 */);
const ActionSchemes::S_begin_looking a_begin_looking_bob_scr2_doc1_seatA(105, "(begin-looking bob scr2 doc1 seatA)", action_begin_looking_bob_scr2_doc1_seatA, NULL, getRecOrRefr105, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 105 */);
const ActionSchemes::S_begin_looking a_begin_looking_bob_scr3_doc1_seatA(106, "(begin-looking bob scr3 doc1 seatA)", action_begin_looking_bob_scr3_doc1_seatA, NULL, getRecOrRefr106, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 106 */);
const ActionSchemes::S_begin_looking a_begin_looking_charlie_scr1_doc3_seatC(107, "(begin-looking charlie scr1 doc3 seatC)", action_begin_looking_charlie_scr1_doc3_seatC, NULL, getRecOrRefr107, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 107 */);
const ActionSchemes::S_begin_looking a_begin_looking_charlie_scr2_doc3_seatC(108, "(begin-looking charlie scr2 doc3 seatC)", action_begin_looking_charlie_scr2_doc3_seatC, NULL, getRecOrRefr108, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0 /* 108 */);
} // namespace Actions

Action const * __actions[NOPS+3] = {
	&Actions::a_FINISHED,
	&Actions::a_BLOCKED,
	&Actions::a_INITIALIZE,
	&Actions::a_activate_nb1,
	&Actions::a_activate_nb2,
	&Actions::a_activate_nb3,
	&Actions::a_activate_prj1,
	&Actions::a_activate_prj2,
	&Actions::a_activate_prj3,
	&Actions::a_activate_scr1,
	&Actions::a_activate_scr2,
	&Actions::a_activate_scr3,
	&Actions::a_deactivate_nb1,
	&Actions::a_deactivate_nb2,
	&Actions::a_deactivate_nb3,
	&Actions::a_deactivate_prj1,
	&Actions::a_deactivate_prj2,
	&Actions::a_deactivate_prj3,
	&Actions::a_deactivate_scr1,
	&Actions::a_deactivate_scr2,
	&Actions::a_deactivate_scr3,
	&Actions::a_begin_send_document_nb1_doc1,
	&Actions::a_begin_send_document_nb2_doc2,
	&Actions::a_begin_send_document_nb3_doc3,
	&Actions::a_end_send_document_nb1_doc1,
	&Actions::a_end_send_document_nb2_doc2,
	&Actions::a_end_send_document_nb3_doc3,
	&Actions::a_begin_connect_nb1_prj1_i1_o1,
	&Actions::a_begin_connect_nb1_prj2_i1_o2,
	&Actions::a_begin_connect_nb1_prj3_i1_o3,
	&Actions::a_begin_connect_nb2_prj1_i2_o1,
	&Actions::a_begin_connect_nb2_prj2_i2_o2,
	&Actions::a_begin_connect_nb2_prj3_i2_o3,
	&Actions::a_begin_connect_nb3_prj1_i3_o1,
	&Actions::a_begin_connect_nb3_prj2_i3_o2,
	&Actions::a_begin_connect_nb3_prj3_i3_o3,
	&Actions::a_end_connect_nb1_prj1,
	&Actions::a_end_connect_nb1_prj2,
	&Actions::a_end_connect_nb1_prj3,
	&Actions::a_end_connect_nb2_prj1,
	&Actions::a_end_connect_nb2_prj2,
	&Actions::a_end_connect_nb2_prj3,
	&Actions::a_end_connect_nb3_prj1,
	&Actions::a_end_connect_nb3_prj2,
	&Actions::a_end_connect_nb3_prj3,
	&Actions::a_xxx_device_crossbar_switch_i1_o1,
	&Actions::a_xxx_device_crossbar_switch_i1_o2,
	&Actions::a_xxx_device_crossbar_switch_i1_o3,
	&Actions::a_xxx_device_crossbar_switch_i2_o1,
	&Actions::a_xxx_device_crossbar_switch_i2_o2,
	&Actions::a_xxx_device_crossbar_switch_i2_o3,
	&Actions::a_xxx_device_crossbar_switch_i3_o1,
	&Actions::a_xxx_device_crossbar_switch_i3_o2,
	&Actions::a_xxx_device_crossbar_switch_i3_o3,
	&Actions::a_xxx_device_crossbar_unswitch_i1_o1,
	&Actions::a_xxx_device_crossbar_unswitch_i1_o2,
	&Actions::a_xxx_device_crossbar_unswitch_i1_o3,
	&Actions::a_xxx_device_crossbar_unswitch_i2_o1,
	&Actions::a_xxx_device_crossbar_unswitch_i2_o2,
	&Actions::a_xxx_device_crossbar_unswitch_i2_o3,
	&Actions::a_xxx_device_crossbar_unswitch_i3_o1,
	&Actions::a_xxx_device_crossbar_unswitch_i3_o2,
	&Actions::a_xxx_device_crossbar_unswitch_i3_o3,
	&Actions::a_begin_project_document_prj1_nb1_doc1,
	&Actions::a_begin_project_document_prj1_nb2_doc2,
	&Actions::a_begin_project_document_prj1_nb3_doc3,
	&Actions::a_begin_project_document_prj2_nb1_doc1,
	&Actions::a_begin_project_document_prj2_nb2_doc2,
	&Actions::a_begin_project_document_prj2_nb3_doc3,
	&Actions::a_begin_project_document_prj3_nb1_doc1,
	&Actions::a_begin_project_document_prj3_nb2_doc2,
	&Actions::a_begin_project_document_prj3_nb3_doc3,
	&Actions::a_end_project_document_prj1_nb1_doc1,
	&Actions::a_end_project_document_prj1_nb2_doc2,
	&Actions::a_end_project_document_prj1_nb3_doc3,
	&Actions::a_end_project_document_prj2_nb1_doc1,
	&Actions::a_end_project_document_prj2_nb2_doc2,
	&Actions::a_end_project_document_prj2_nb3_doc3,
	&Actions::a_end_project_document_prj3_nb1_doc1,
	&Actions::a_end_project_document_prj3_nb2_doc2,
	&Actions::a_end_project_document_prj3_nb3_doc3,
	&Actions::a_begin_show_document_scr1_prj1_doc1,
	&Actions::a_begin_show_document_scr1_prj1_doc2,
	&Actions::a_begin_show_document_scr1_prj1_doc3,
	&Actions::a_begin_show_document_scr2_prj2_doc1,
	&Actions::a_begin_show_document_scr2_prj2_doc2,
	&Actions::a_begin_show_document_scr2_prj2_doc3,
	&Actions::a_begin_show_document_scr3_prj3_doc1,
	&Actions::a_begin_show_document_scr3_prj3_doc2,
	&Actions::a_begin_show_document_scr3_prj3_doc3,
	&Actions::a_end_show_document_scr1_prj1_doc1,
	&Actions::a_end_show_document_scr1_prj1_doc2,
	&Actions::a_end_show_document_scr1_prj1_doc3,
	&Actions::a_end_show_document_scr2_prj2_doc1,
	&Actions::a_end_show_document_scr2_prj2_doc2,
	&Actions::a_end_show_document_scr2_prj2_doc3,
	&Actions::a_end_show_document_scr3_prj3_doc1,
	&Actions::a_end_show_document_scr3_prj3_doc2,
	&Actions::a_end_show_document_scr3_prj3_doc3,
	&Actions::a_begin_looking_alice_scr1_doc1_seatB,
	&Actions::a_begin_looking_alice_scr1_doc2_seatB,
	&Actions::a_begin_looking_alice_scr1_doc3_seatB,
	&Actions::a_begin_looking_alice_scr2_doc1_seatB,
	&Actions::a_begin_looking_alice_scr2_doc2_seatB,
	&Actions::a_begin_looking_alice_scr2_doc3_seatB,
	&Actions::a_begin_looking_alice_scr3_doc1_seatB,
	&Actions::a_begin_looking_alice_scr3_doc2_seatB,
	&Actions::a_begin_looking_alice_scr3_doc3_seatB,
	&Actions::a_begin_looking_bob_scr2_doc1_seatA,
	&Actions::a_begin_looking_bob_scr3_doc1_seatA,
	&Actions::a_begin_looking_charlie_scr1_doc3_seatC,
	&Actions::a_begin_looking_charlie_scr2_doc3_seatC
};
Action const * *actions = &__actions[3];

const int agentOps0[109] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108};
char const * const agentNames[NAGENTS] = {"NULL"};
int const nOpsForAgent[NAGENTS] = {109};
int const * const opsForAgent[NAGENTS] = {agentOps0};

bool isGoalState(size_t goal, StatePtr x) { (void) x;
	switch(goal) {
	case 0: /*r218pdev-simple*/
		return (x->Scan_see_alice_doc1 && x->Scan_see_bob_doc1 && x->Scan_see_alice_doc2 && x->Scan_see_alice_doc3 && x->Scan_see_charlie_doc3);
	default: throw std::runtime_error("isGoalState: Unknown goal"); break;
	}
}

void sampleInitial(size_t initialState, StateRec *x1) {
	bzero(x1,sizeof(StateRec));
	switch(initialState) {
	case 0: /*r218pdev-simple*/
		x1->Sis_open_o3 = 1;
		x1->Sis_open_o2 = 1;
		x1->Sis_open_o1 = 1;
		x1->Sis_active_nb1 = 0;
		x1->Sis_active_nb2 = 0;
		x1->Sis_active_nb3 = 0;
		x1->Sis_active_prj1 = 0;
		x1->Sis_active_prj2 = 0;
		x1->Sis_active_prj3 = 0;
		x1->Sis_active_scr1 = 0;
		x1->Sis_active_scr2 = 0;
		x1->Sis_active_scr3 = 0;
		x1->Sin_use_nb1 = 0;
		x1->Sin_use_nb2 = 0;
		x1->Sin_use_nb3 = 0;
		x1->Sin_use_prj1 = 0;
		x1->Sin_use_prj2 = 0;
		x1->Sin_use_prj3 = 0;
		x1->Sin_use_scr1 = 0;
		x1->Sin_use_scr2 = 0;
		x1->Sin_use_scr3 = 0;
		x1->Sis_locked_by_alice_attention_alice = 0;
		x1->Sis_locked_by_bob_attention_bob = 0;
		x1->Sis_locked_by_charlie_attention_charlie = 0;
		x1->Sis_locked_by_nb1_vga_out_port_crossbar = 0;
		x1->Sis_locked_by_nb1_vga_out_signal_prj1 = 0;
		x1->Sis_locked_by_nb1_vga_out_signal_prj2 = 0;
		x1->Sis_locked_by_nb1_vga_out_signal_prj3 = 0;
		x1->Sis_locked_by_nb2_vga_out_port_crossbar = 0;
		x1->Sis_locked_by_nb2_vga_out_signal_prj1 = 0;
		x1->Sis_locked_by_nb2_vga_out_signal_prj2 = 0;
		x1->Sis_locked_by_nb2_vga_out_signal_prj3 = 0;
		x1->Sis_locked_by_nb3_vga_out_port_crossbar = 0;
		x1->Sis_locked_by_nb3_vga_out_signal_prj1 = 0;
		x1->Sis_locked_by_nb3_vga_out_signal_prj2 = 0;
		x1->Sis_locked_by_nb3_vga_out_signal_prj3 = 0;
		x1->Sis_locked_by_prj1_vga_in_port_prj1 = 0;
		x1->Sis_locked_by_prj1_vga_in_port_crossbar = 0;
		x1->Sis_locked_by_prj1_picture_scr1 = 0;
		x1->Sis_locked_by_prj2_vga_in_port_prj2 = 0;
		x1->Sis_locked_by_prj2_vga_in_port_crossbar = 0;
		x1->Sis_locked_by_prj2_picture_scr2 = 0;
		x1->Sis_locked_by_prj3_vga_in_port_prj3 = 0;
		x1->Sis_locked_by_prj3_vga_in_port_crossbar = 0;
		x1->Sis_locked_by_prj3_picture_scr3 = 0;
		x1->Sis_locked_by_scr1_picture_alice = 0;
		x1->Sis_locked_by_scr1_picture_charlie = 0;
		x1->Sis_locked_by_scr2_picture_alice = 0;
		x1->Sis_locked_by_scr2_picture_bob = 0;
		x1->Sis_locked_by_scr2_picture_charlie = 0;
		x1->Sis_locked_by_scr3_picture_alice = 0;
		x1->Sis_locked_by_scr3_picture_bob = 0;
		x1->Sis_sending_nb1_doc1_vga_out_signal = 0;
		x1->Sis_sending_nb2_doc2_vga_out_signal = 0;
		x1->Sis_sending_nb3_doc3_vga_out_signal = 0;
		x1->Sis_connected_nb1_vga_out_port_prj1_vga_in_port = 0;
		x1->Sis_connected_nb1_vga_out_port_prj2_vga_in_port = 0;
		x1->Sis_connected_nb1_vga_out_port_prj3_vga_in_port = 0;
		x1->Sis_connected_nb2_vga_out_port_prj1_vga_in_port = 0;
		x1->Sis_connected_nb2_vga_out_port_prj2_vga_in_port = 0;
		x1->Sis_connected_nb2_vga_out_port_prj3_vga_in_port = 0;
		x1->Sis_connected_nb3_vga_out_port_prj1_vga_in_port = 0;
		x1->Sis_connected_nb3_vga_out_port_prj2_vga_in_port = 0;
		x1->Sis_connected_nb3_vga_out_port_prj3_vga_in_port = 0;
		x1->Sis_projecting_by_prj1_doc1_nb1 = 0;
		x1->Sis_projecting_by_prj1_doc2_nb2 = 0;
		x1->Sis_projecting_by_prj1_doc3_nb3 = 0;
		x1->Sis_projecting_by_prj2_doc1_nb1 = 0;
		x1->Sis_projecting_by_prj2_doc2_nb2 = 0;
		x1->Sis_projecting_by_prj2_doc3_nb3 = 0;
		x1->Sis_projecting_by_prj3_doc1_nb1 = 0;
		x1->Sis_projecting_by_prj3_doc2_nb2 = 0;
		x1->Sis_projecting_by_prj3_doc3_nb3 = 0;
		x1->Sis_projecting_prj1_doc1 = 0;
		x1->Sis_projecting_prj1_doc2 = 0;
		x1->Sis_projecting_prj1_doc3 = 0;
		x1->Sis_projecting_prj2_doc1 = 0;
		x1->Sis_projecting_prj2_doc2 = 0;
		x1->Sis_projecting_prj2_doc3 = 0;
		x1->Sis_projecting_prj3_doc1 = 0;
		x1->Sis_projecting_prj3_doc2 = 0;
		x1->Sis_projecting_prj3_doc3 = 0;
		x1->Sis_showing_by_scr1_doc1_prj1 = 0;
		x1->Sis_showing_by_scr1_doc2_prj1 = 0;
		x1->Sis_showing_by_scr1_doc3_prj1 = 0;
		x1->Sis_showing_by_scr2_doc1_prj2 = 0;
		x1->Sis_showing_by_scr2_doc2_prj2 = 0;
		x1->Sis_showing_by_scr2_doc3_prj2 = 0;
		x1->Sis_showing_by_scr3_doc1_prj3 = 0;
		x1->Sis_showing_by_scr3_doc2_prj3 = 0;
		x1->Sis_showing_by_scr3_doc3_prj3 = 0;
		x1->Sis_showing_scr1_doc1 = 0;
		x1->Sis_showing_scr1_doc2 = 0;
		x1->Sis_showing_scr1_doc3 = 0;
		x1->Sis_showing_scr2_doc1 = 0;
		x1->Sis_showing_scr2_doc2 = 0;
		x1->Sis_showing_scr2_doc3 = 0;
		x1->Sis_showing_scr3_doc1 = 0;
		x1->Sis_showing_scr3_doc2 = 0;
		x1->Sis_showing_scr3_doc3 = 0;
		x1->Scan_see_alice_doc1 = 0;
		x1->Scan_see_alice_doc2 = 0;
		x1->Scan_see_alice_doc3 = 0;
		x1->Scan_see_bob_doc1 = 0;
		x1->Scan_see_charlie_doc3 = 0;
		x1->Sis_switched_i1_o1 = 0;
		x1->Sis_switched_i1_o2 = 0;
		x1->Sis_switched_i1_o3 = 0;
		x1->Sis_switched_i2_o1 = 0;
		x1->Sis_switched_i2_o2 = 0;
		x1->Sis_switched_i2_o3 = 0;
		x1->Sis_switched_i3_o1 = 0;
		x1->Sis_switched_i3_o2 = 0;
		x1->Sis_switched_i3_o3 = 0;
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
	case 0: /*r218pdev-simple*/
		switch(goal) {
		case 0: /*r218pdev-simple*/
			if (parentAccepted == 0) {
				//accepted = StateRec();accepted.S1=1;
			} else {
				accepted= *parentAccepted;
		
				if (s->Scan_see_alice_doc1==1) accepted.Scan_see_alice_doc1=1;
				if (s->Scan_see_bob_doc1==1) accepted.Scan_see_bob_doc1=1;
				if (s->Scan_see_alice_doc2==1) accepted.Scan_see_alice_doc2=1;
				if (s->Scan_see_alice_doc3==1) accepted.Scan_see_alice_doc3=1;
				if (s->Scan_see_charlie_doc3==1) accepted.Scan_see_charlie_doc3=1;
			} // compute the heuristic
			if (accepted.Scan_see_alice_doc1== 0) count++;
			if (accepted.Scan_see_bob_doc1== 0) count++;
			if (accepted.Scan_see_alice_doc2== 0) count++;
			if (accepted.Scan_see_alice_doc3== 0) count++;
			if (accepted.Scan_see_charlie_doc3== 0) count++;
				if (accepted.Scan_see_alice_doc1== 1 && s->Scan_see_alice_doc1== 0) count++;
				if (accepted.Scan_see_bob_doc1== 1 && s->Scan_see_bob_doc1== 0) count++;
				if (accepted.Scan_see_alice_doc2== 1 && s->Scan_see_alice_doc2== 0) count++;
				if (accepted.Scan_see_alice_doc3== 1 && s->Scan_see_alice_doc3== 0) count++;
				if (accepted.Scan_see_charlie_doc3== 1 && s->Scan_see_charlie_doc3== 0) count++;
			break;
		default: throw std::runtime_error("landmarkCountHeuristic: Unknown goal state");
		}
		break;
	default: throw std::runtime_error("landmarkCountHeuristic: Unknown initial state");
	}
	*parentAccepted = accepted;
	return count;
}
