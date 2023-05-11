#pragma once
extern char const * const agentNames[NAGENTS];
extern int const nOpsForAgent[NAGENTS];
extern int const * const opsForAgent[NAGENTS];

struct StateRec {
	 unsigned int Sis_active_nb1:1; /* (is-active nb1) */
	 unsigned int Sis_active_nb2:1; /* (is-active nb2) */
	 unsigned int Sis_active_nb3:1; /* (is-active nb3) */
	 unsigned int Sis_active_prj1:1; /* (is-active prj1) */
	 unsigned int Sis_active_prj2:1; /* (is-active prj2) */
	 unsigned int Sis_active_prj3:1; /* (is-active prj3) */
	 unsigned int Sis_active_scr1:1; /* (is-active scr1) */
	 unsigned int Sis_active_scr2:1; /* (is-active scr2) */
	 unsigned int Sis_active_scr3:1; /* (is-active scr3) */
	 unsigned int Sin_use_nb1:1; /* (in-use nb1) */
	 unsigned int Sin_use_nb2:1; /* (in-use nb2) */
	 unsigned int Sin_use_nb3:1; /* (in-use nb3) */
	 unsigned int Sin_use_prj1:1; /* (in-use prj1) */
	 unsigned int Sin_use_prj2:1; /* (in-use prj2) */
	 unsigned int Sin_use_prj3:1; /* (in-use prj3) */
	 unsigned int Sin_use_scr1:1; /* (in-use scr1) */
	 unsigned int Sin_use_scr2:1; /* (in-use scr2) */
	 unsigned int Sin_use_scr3:1; /* (in-use scr3) */
	 unsigned int Sis_locked_by_alice_attention_alice:1; /* (is-locked-by alice attention alice) */
	 unsigned int Sis_locked_by_bob_attention_bob:1; /* (is-locked-by bob attention bob) */
	 unsigned int Sis_locked_by_charlie_attention_charlie:1; /* (is-locked-by charlie attention charlie) */
	 unsigned int Sis_locked_by_nb1_vga_out_port_crossbar:1; /* (is-locked-by nb1 vga-out-port crossbar) */
	 unsigned int Sis_locked_by_nb1_vga_out_signal_prj1:1; /* (is-locked-by nb1 vga-out-signal prj1) */
	 unsigned int Sis_locked_by_nb1_vga_out_signal_prj2:1; /* (is-locked-by nb1 vga-out-signal prj2) */
	 unsigned int Sis_locked_by_nb1_vga_out_signal_prj3:1; /* (is-locked-by nb1 vga-out-signal prj3) */
	 unsigned int Sis_locked_by_nb2_vga_out_port_crossbar:1; /* (is-locked-by nb2 vga-out-port crossbar) */
	 unsigned int Sis_locked_by_nb2_vga_out_signal_prj1:1; /* (is-locked-by nb2 vga-out-signal prj1) */
	 unsigned int Sis_locked_by_nb2_vga_out_signal_prj2:1; /* (is-locked-by nb2 vga-out-signal prj2) */
	 unsigned int Sis_locked_by_nb2_vga_out_signal_prj3:1; /* (is-locked-by nb2 vga-out-signal prj3) */
	 unsigned int Sis_locked_by_nb3_vga_out_port_crossbar:1; /* (is-locked-by nb3 vga-out-port crossbar) */
	 unsigned int Sis_locked_by_nb3_vga_out_signal_prj1:1; /* (is-locked-by nb3 vga-out-signal prj1) */
	 unsigned int Sis_locked_by_nb3_vga_out_signal_prj2:1; /* (is-locked-by nb3 vga-out-signal prj2) */
	 unsigned int Sis_locked_by_nb3_vga_out_signal_prj3:1; /* (is-locked-by nb3 vga-out-signal prj3) */
	 unsigned int Sis_locked_by_prj1_vga_in_port_prj1:1; /* (is-locked-by prj1 vga-in-port prj1) */
	 unsigned int Sis_locked_by_prj1_vga_in_port_crossbar:1; /* (is-locked-by prj1 vga-in-port crossbar) */
	 unsigned int Sis_locked_by_prj1_picture_scr1:1; /* (is-locked-by prj1 picture scr1) */
	 unsigned int Sis_locked_by_prj2_vga_in_port_prj2:1; /* (is-locked-by prj2 vga-in-port prj2) */
	 unsigned int Sis_locked_by_prj2_vga_in_port_crossbar:1; /* (is-locked-by prj2 vga-in-port crossbar) */
	 unsigned int Sis_locked_by_prj2_picture_scr2:1; /* (is-locked-by prj2 picture scr2) */
	 unsigned int Sis_locked_by_prj3_vga_in_port_prj3:1; /* (is-locked-by prj3 vga-in-port prj3) */
	 unsigned int Sis_locked_by_prj3_vga_in_port_crossbar:1; /* (is-locked-by prj3 vga-in-port crossbar) */
	 unsigned int Sis_locked_by_prj3_picture_scr3:1; /* (is-locked-by prj3 picture scr3) */
	 unsigned int Sis_locked_by_scr1_picture_alice:1; /* (is-locked-by scr1 picture alice) */
	 unsigned int Sis_locked_by_scr1_picture_charlie:1; /* (is-locked-by scr1 picture charlie) */
	 unsigned int Sis_locked_by_scr2_picture_alice:1; /* (is-locked-by scr2 picture alice) */
	 unsigned int Sis_locked_by_scr2_picture_bob:1; /* (is-locked-by scr2 picture bob) */
	 unsigned int Sis_locked_by_scr2_picture_charlie:1; /* (is-locked-by scr2 picture charlie) */
	 unsigned int Sis_locked_by_scr3_picture_alice:1; /* (is-locked-by scr3 picture alice) */
	 unsigned int Sis_locked_by_scr3_picture_bob:1; /* (is-locked-by scr3 picture bob) */
	 unsigned int Sis_sending_nb1_doc1_vga_out_signal:1; /* (is-sending nb1 doc1 vga-out-signal) */
	 unsigned int Sis_sending_nb2_doc2_vga_out_signal:1; /* (is-sending nb2 doc2 vga-out-signal) */
	 unsigned int Sis_sending_nb3_doc3_vga_out_signal:1; /* (is-sending nb3 doc3 vga-out-signal) */
	 unsigned int Sis_connected_nb1_vga_out_port_prj1_vga_in_port:1; /* (is-connected nb1 vga-out-port prj1 vga-in-port) */
	 unsigned int Sis_connected_nb1_vga_out_port_prj2_vga_in_port:1; /* (is-connected nb1 vga-out-port prj2 vga-in-port) */
	 unsigned int Sis_connected_nb1_vga_out_port_prj3_vga_in_port:1; /* (is-connected nb1 vga-out-port prj3 vga-in-port) */
	 unsigned int Sis_connected_nb2_vga_out_port_prj1_vga_in_port:1; /* (is-connected nb2 vga-out-port prj1 vga-in-port) */
	 unsigned int Sis_connected_nb2_vga_out_port_prj2_vga_in_port:1; /* (is-connected nb2 vga-out-port prj2 vga-in-port) */
	 unsigned int Sis_connected_nb2_vga_out_port_prj3_vga_in_port:1; /* (is-connected nb2 vga-out-port prj3 vga-in-port) */
	 unsigned int Sis_connected_nb3_vga_out_port_prj1_vga_in_port:1; /* (is-connected nb3 vga-out-port prj1 vga-in-port) */
	 unsigned int Sis_connected_nb3_vga_out_port_prj2_vga_in_port:1; /* (is-connected nb3 vga-out-port prj2 vga-in-port) */
	 unsigned int Sis_connected_nb3_vga_out_port_prj3_vga_in_port:1; /* (is-connected nb3 vga-out-port prj3 vga-in-port) */
	 unsigned int Sis_projecting_by_prj1_doc1_nb1:1; /* (is-projecting-by prj1 doc1 nb1) */
	 unsigned int Sis_projecting_by_prj1_doc2_nb2:1; /* (is-projecting-by prj1 doc2 nb2) */
	 unsigned int Sis_projecting_by_prj1_doc3_nb3:1; /* (is-projecting-by prj1 doc3 nb3) */
	 unsigned int Sis_projecting_by_prj2_doc1_nb1:1; /* (is-projecting-by prj2 doc1 nb1) */
	 unsigned int Sis_projecting_by_prj2_doc2_nb2:1; /* (is-projecting-by prj2 doc2 nb2) */
	 unsigned int Sis_projecting_by_prj2_doc3_nb3:1; /* (is-projecting-by prj2 doc3 nb3) */
	 unsigned int Sis_projecting_by_prj3_doc1_nb1:1; /* (is-projecting-by prj3 doc1 nb1) */
	 unsigned int Sis_projecting_by_prj3_doc2_nb2:1; /* (is-projecting-by prj3 doc2 nb2) */
	 unsigned int Sis_projecting_by_prj3_doc3_nb3:1; /* (is-projecting-by prj3 doc3 nb3) */
	 unsigned int Sis_projecting_prj1_doc1:1; /* (is-projecting prj1 doc1) */
	 unsigned int Sis_projecting_prj1_doc2:1; /* (is-projecting prj1 doc2) */
	 unsigned int Sis_projecting_prj1_doc3:1; /* (is-projecting prj1 doc3) */
	 unsigned int Sis_projecting_prj2_doc1:1; /* (is-projecting prj2 doc1) */
	 unsigned int Sis_projecting_prj2_doc2:1; /* (is-projecting prj2 doc2) */
	 unsigned int Sis_projecting_prj2_doc3:1; /* (is-projecting prj2 doc3) */
	 unsigned int Sis_projecting_prj3_doc1:1; /* (is-projecting prj3 doc1) */
	 unsigned int Sis_projecting_prj3_doc2:1; /* (is-projecting prj3 doc2) */
	 unsigned int Sis_projecting_prj3_doc3:1; /* (is-projecting prj3 doc3) */
	 unsigned int Sis_showing_by_scr1_doc1_prj1:1; /* (is-showing-by scr1 doc1 prj1) */
	 unsigned int Sis_showing_by_scr1_doc2_prj1:1; /* (is-showing-by scr1 doc2 prj1) */
	 unsigned int Sis_showing_by_scr1_doc3_prj1:1; /* (is-showing-by scr1 doc3 prj1) */
	 unsigned int Sis_showing_by_scr2_doc1_prj2:1; /* (is-showing-by scr2 doc1 prj2) */
	 unsigned int Sis_showing_by_scr2_doc2_prj2:1; /* (is-showing-by scr2 doc2 prj2) */
	 unsigned int Sis_showing_by_scr2_doc3_prj2:1; /* (is-showing-by scr2 doc3 prj2) */
	 unsigned int Sis_showing_by_scr3_doc1_prj3:1; /* (is-showing-by scr3 doc1 prj3) */
	 unsigned int Sis_showing_by_scr3_doc2_prj3:1; /* (is-showing-by scr3 doc2 prj3) */
	 unsigned int Sis_showing_by_scr3_doc3_prj3:1; /* (is-showing-by scr3 doc3 prj3) */
	 unsigned int Sis_showing_scr1_doc1:1; /* (is-showing scr1 doc1) */
	 unsigned int Sis_showing_scr1_doc2:1; /* (is-showing scr1 doc2) */
	 unsigned int Sis_showing_scr1_doc3:1; /* (is-showing scr1 doc3) */
	 unsigned int Sis_showing_scr2_doc1:1; /* (is-showing scr2 doc1) */
	 unsigned int Sis_showing_scr2_doc2:1; /* (is-showing scr2 doc2) */
	 unsigned int Sis_showing_scr2_doc3:1; /* (is-showing scr2 doc3) */
	 unsigned int Sis_showing_scr3_doc1:1; /* (is-showing scr3 doc1) */
	 unsigned int Sis_showing_scr3_doc2:1; /* (is-showing scr3 doc2) */
	 unsigned int Sis_showing_scr3_doc3:1; /* (is-showing scr3 doc3) */
	 unsigned int Scan_see_alice_doc1:1; /* (can-see alice doc1) */
	 unsigned int Scan_see_alice_doc2:1; /* (can-see alice doc2) */
	 unsigned int Scan_see_alice_doc3:1; /* (can-see alice doc3) */
	 unsigned int Scan_see_bob_doc1:1; /* (can-see bob doc1) */
	 unsigned int Scan_see_charlie_doc3:1; /* (can-see charlie doc3) */
	 unsigned int Sis_open_o1:1; /* (is-open o1) */
	 unsigned int Sis_open_o2:1; /* (is-open o2) */
	 unsigned int Sis_open_o3:1; /* (is-open o3) */
	 unsigned int Sis_switched_i1_o1:1; /* (is-switched i1 o1) */
	 unsigned int Sis_switched_i1_o2:1; /* (is-switched i1 o2) */
	 unsigned int Sis_switched_i1_o3:1; /* (is-switched i1 o3) */
	 unsigned int Sis_switched_i2_o1:1; /* (is-switched i2 o1) */
	 unsigned int Sis_switched_i2_o2:1; /* (is-switched i2 o2) */
	 unsigned int Sis_switched_i2_o3:1; /* (is-switched i2 o3) */
	 unsigned int Sis_switched_i3_o1:1; /* (is-switched i3 o1) */
	 unsigned int Sis_switched_i3_o2:1; /* (is-switched i3 o2) */
	 unsigned int Sis_switched_i3_o3:1; /* (is-switched i3 o3) */

	StateRec() {bzero(this,sizeof(StateRec));}
};

namespace ActionSchemes {
extern const ActionScheme s_FINISHED;
extern const ActionScheme s_BLOCKED;
extern const ActionScheme s_INITIALIZE;
extern const ActionScheme s_activate;
extern const ActionScheme s_deactivate;
extern const ActionScheme s_begin_send_document;
extern const ActionScheme s_end_send_document;
extern const ActionScheme s_begin_connect;
extern const ActionScheme s_end_connect;
extern const ActionScheme s_xxx_device_crossbar_switch;
extern const ActionScheme s_xxx_device_crossbar_unswitch;
extern const ActionScheme s_begin_project_document;
extern const ActionScheme s_end_project_document;
extern const ActionScheme s_begin_show_document;
extern const ActionScheme s_end_show_document;
extern const ActionScheme s_begin_looking;
struct S_FINISHED : Action {
	S_FINISHED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_FINISHED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_FINISHED
struct S_BLOCKED : Action {
	S_BLOCKED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_BLOCKED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_BLOCKED
struct S_INITIALIZE : Action {
	S_INITIALIZE(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_INITIALIZE, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_INITIALIZE
struct S_activate : Action {
	S_activate(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_activate, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_activate
struct S_deactivate : Action {
	S_deactivate(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_deactivate, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_deactivate
struct S_begin_send_document : Action {
	S_begin_send_document(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_begin_send_document, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_begin_send_document
struct S_end_send_document : Action {
	S_end_send_document(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_end_send_document, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_end_send_document
struct S_begin_connect : Action {
	S_begin_connect(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_begin_connect, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_begin_connect
struct S_end_connect : Action {
	S_end_connect(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_end_connect, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_end_connect
struct S_xxx_device_crossbar_switch : Action {
	S_xxx_device_crossbar_switch(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_xxx_device_crossbar_switch, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_xxx_device_crossbar_switch
struct S_xxx_device_crossbar_unswitch : Action {
	S_xxx_device_crossbar_unswitch(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_xxx_device_crossbar_unswitch, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_xxx_device_crossbar_unswitch
struct S_begin_project_document : Action {
	S_begin_project_document(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_begin_project_document, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_begin_project_document
struct S_end_project_document : Action {
	S_end_project_document(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_end_project_document, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_end_project_document
struct S_begin_show_document : Action {
	S_begin_show_document(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_begin_show_document, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_begin_show_document
struct S_end_show_document : Action {
	S_end_show_document(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_end_show_document, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_end_show_document
struct S_begin_looking : Action {
	S_begin_looking(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_begin_looking, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_begin_looking
} // namespace ActionSchemes

namespace Actions {
extern const ActionSchemes::S_FINISHED a_FINISHED;
extern const ActionSchemes::S_BLOCKED a_BLOCKED;
extern const ActionSchemes::S_INITIALIZE a_INITIALIZE;
extern const ActionSchemes::S_activate a_activate_nb1;
extern const ActionSchemes::S_activate a_activate_nb2;
extern const ActionSchemes::S_activate a_activate_nb3;
extern const ActionSchemes::S_activate a_activate_prj1;
extern const ActionSchemes::S_activate a_activate_prj2;
extern const ActionSchemes::S_activate a_activate_prj3;
extern const ActionSchemes::S_activate a_activate_scr1;
extern const ActionSchemes::S_activate a_activate_scr2;
extern const ActionSchemes::S_activate a_activate_scr3;
extern const ActionSchemes::S_deactivate a_deactivate_nb1;
extern const ActionSchemes::S_deactivate a_deactivate_nb2;
extern const ActionSchemes::S_deactivate a_deactivate_nb3;
extern const ActionSchemes::S_deactivate a_deactivate_prj1;
extern const ActionSchemes::S_deactivate a_deactivate_prj2;
extern const ActionSchemes::S_deactivate a_deactivate_prj3;
extern const ActionSchemes::S_deactivate a_deactivate_scr1;
extern const ActionSchemes::S_deactivate a_deactivate_scr2;
extern const ActionSchemes::S_deactivate a_deactivate_scr3;
extern const ActionSchemes::S_begin_send_document a_begin_send_document_nb1_doc1;
extern const ActionSchemes::S_begin_send_document a_begin_send_document_nb2_doc2;
extern const ActionSchemes::S_begin_send_document a_begin_send_document_nb3_doc3;
extern const ActionSchemes::S_end_send_document a_end_send_document_nb1_doc1;
extern const ActionSchemes::S_end_send_document a_end_send_document_nb2_doc2;
extern const ActionSchemes::S_end_send_document a_end_send_document_nb3_doc3;
extern const ActionSchemes::S_begin_connect a_begin_connect_nb1_prj1_i1_o1;
extern const ActionSchemes::S_begin_connect a_begin_connect_nb1_prj2_i1_o2;
extern const ActionSchemes::S_begin_connect a_begin_connect_nb1_prj3_i1_o3;
extern const ActionSchemes::S_begin_connect a_begin_connect_nb2_prj1_i2_o1;
extern const ActionSchemes::S_begin_connect a_begin_connect_nb2_prj2_i2_o2;
extern const ActionSchemes::S_begin_connect a_begin_connect_nb2_prj3_i2_o3;
extern const ActionSchemes::S_begin_connect a_begin_connect_nb3_prj1_i3_o1;
extern const ActionSchemes::S_begin_connect a_begin_connect_nb3_prj2_i3_o2;
extern const ActionSchemes::S_begin_connect a_begin_connect_nb3_prj3_i3_o3;
extern const ActionSchemes::S_end_connect a_end_connect_nb1_prj1;
extern const ActionSchemes::S_end_connect a_end_connect_nb1_prj2;
extern const ActionSchemes::S_end_connect a_end_connect_nb1_prj3;
extern const ActionSchemes::S_end_connect a_end_connect_nb2_prj1;
extern const ActionSchemes::S_end_connect a_end_connect_nb2_prj2;
extern const ActionSchemes::S_end_connect a_end_connect_nb2_prj3;
extern const ActionSchemes::S_end_connect a_end_connect_nb3_prj1;
extern const ActionSchemes::S_end_connect a_end_connect_nb3_prj2;
extern const ActionSchemes::S_end_connect a_end_connect_nb3_prj3;
extern const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i1_o1;
extern const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i1_o2;
extern const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i1_o3;
extern const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i2_o1;
extern const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i2_o2;
extern const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i2_o3;
extern const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i3_o1;
extern const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i3_o2;
extern const ActionSchemes::S_xxx_device_crossbar_switch a_xxx_device_crossbar_switch_i3_o3;
extern const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i1_o1;
extern const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i1_o2;
extern const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i1_o3;
extern const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i2_o1;
extern const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i2_o2;
extern const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i2_o3;
extern const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i3_o1;
extern const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i3_o2;
extern const ActionSchemes::S_xxx_device_crossbar_unswitch a_xxx_device_crossbar_unswitch_i3_o3;
extern const ActionSchemes::S_begin_project_document a_begin_project_document_prj1_nb1_doc1;
extern const ActionSchemes::S_begin_project_document a_begin_project_document_prj1_nb2_doc2;
extern const ActionSchemes::S_begin_project_document a_begin_project_document_prj1_nb3_doc3;
extern const ActionSchemes::S_begin_project_document a_begin_project_document_prj2_nb1_doc1;
extern const ActionSchemes::S_begin_project_document a_begin_project_document_prj2_nb2_doc2;
extern const ActionSchemes::S_begin_project_document a_begin_project_document_prj2_nb3_doc3;
extern const ActionSchemes::S_begin_project_document a_begin_project_document_prj3_nb1_doc1;
extern const ActionSchemes::S_begin_project_document a_begin_project_document_prj3_nb2_doc2;
extern const ActionSchemes::S_begin_project_document a_begin_project_document_prj3_nb3_doc3;
extern const ActionSchemes::S_end_project_document a_end_project_document_prj1_nb1_doc1;
extern const ActionSchemes::S_end_project_document a_end_project_document_prj1_nb2_doc2;
extern const ActionSchemes::S_end_project_document a_end_project_document_prj1_nb3_doc3;
extern const ActionSchemes::S_end_project_document a_end_project_document_prj2_nb1_doc1;
extern const ActionSchemes::S_end_project_document a_end_project_document_prj2_nb2_doc2;
extern const ActionSchemes::S_end_project_document a_end_project_document_prj2_nb3_doc3;
extern const ActionSchemes::S_end_project_document a_end_project_document_prj3_nb1_doc1;
extern const ActionSchemes::S_end_project_document a_end_project_document_prj3_nb2_doc2;
extern const ActionSchemes::S_end_project_document a_end_project_document_prj3_nb3_doc3;
extern const ActionSchemes::S_begin_show_document a_begin_show_document_scr1_prj1_doc1;
extern const ActionSchemes::S_begin_show_document a_begin_show_document_scr1_prj1_doc2;
extern const ActionSchemes::S_begin_show_document a_begin_show_document_scr1_prj1_doc3;
extern const ActionSchemes::S_begin_show_document a_begin_show_document_scr2_prj2_doc1;
extern const ActionSchemes::S_begin_show_document a_begin_show_document_scr2_prj2_doc2;
extern const ActionSchemes::S_begin_show_document a_begin_show_document_scr2_prj2_doc3;
extern const ActionSchemes::S_begin_show_document a_begin_show_document_scr3_prj3_doc1;
extern const ActionSchemes::S_begin_show_document a_begin_show_document_scr3_prj3_doc2;
extern const ActionSchemes::S_begin_show_document a_begin_show_document_scr3_prj3_doc3;
extern const ActionSchemes::S_end_show_document a_end_show_document_scr1_prj1_doc1;
extern const ActionSchemes::S_end_show_document a_end_show_document_scr1_prj1_doc2;
extern const ActionSchemes::S_end_show_document a_end_show_document_scr1_prj1_doc3;
extern const ActionSchemes::S_end_show_document a_end_show_document_scr2_prj2_doc1;
extern const ActionSchemes::S_end_show_document a_end_show_document_scr2_prj2_doc2;
extern const ActionSchemes::S_end_show_document a_end_show_document_scr2_prj2_doc3;
extern const ActionSchemes::S_end_show_document a_end_show_document_scr3_prj3_doc1;
extern const ActionSchemes::S_end_show_document a_end_show_document_scr3_prj3_doc2;
extern const ActionSchemes::S_end_show_document a_end_show_document_scr3_prj3_doc3;
extern const ActionSchemes::S_begin_looking a_begin_looking_alice_scr1_doc1_seatB;
extern const ActionSchemes::S_begin_looking a_begin_looking_alice_scr1_doc2_seatB;
extern const ActionSchemes::S_begin_looking a_begin_looking_alice_scr1_doc3_seatB;
extern const ActionSchemes::S_begin_looking a_begin_looking_alice_scr2_doc1_seatB;
extern const ActionSchemes::S_begin_looking a_begin_looking_alice_scr2_doc2_seatB;
extern const ActionSchemes::S_begin_looking a_begin_looking_alice_scr2_doc3_seatB;
extern const ActionSchemes::S_begin_looking a_begin_looking_alice_scr3_doc1_seatB;
extern const ActionSchemes::S_begin_looking a_begin_looking_alice_scr3_doc2_seatB;
extern const ActionSchemes::S_begin_looking a_begin_looking_alice_scr3_doc3_seatB;
extern const ActionSchemes::S_begin_looking a_begin_looking_bob_scr2_doc1_seatA;
extern const ActionSchemes::S_begin_looking a_begin_looking_bob_scr3_doc1_seatA;
extern const ActionSchemes::S_begin_looking a_begin_looking_charlie_scr1_doc3_seatC;
extern const ActionSchemes::S_begin_looking a_begin_looking_charlie_scr2_doc3_seatC;
} // namespace ActionSchemes
