#pragma once
extern char const * const agentNames[NAGENTS];
extern int const nOpsForAgent[NAGENTS];
extern int const * const opsForAgent[NAGENTS];

struct StateRec {
	 unsigned int Sprinted_a:1; /* (printed a) */
	 unsigned int Sprinted_b:1; /* (printed b) */
	 unsigned int Sprinted_c:1; /* (printed c) */
	 unsigned int Sprinter_jammed:1; /* printer-jammed */
	 unsigned int Shas_water_coffee_machine:1; /* (has water coffee-machine) */
	 unsigned int Shas_paper_printer:1; /* (has paper printer) */
	 unsigned int Shas_ground_coffee_coffee_machine:1; /* (has ground-coffee coffee-machine) */
	 unsigned int Sat_door_alice:1; /* (at door alice) */
	 unsigned int Sat_door_bob:1; /* (at door bob) */
	 unsigned int Sat_printer_alice:1; /* (at printer alice) */
	 unsigned int Sat_printer_bob:1; /* (at printer bob) */
	 unsigned int Sat_paper_stack_alice:1; /* (at paper-stack alice) */
	 unsigned int Sat_paper_stack_bob:1; /* (at paper-stack bob) */
	 unsigned int Sat_coffee_machine_alice:1; /* (at coffee-machine alice) */
	 unsigned int Sat_coffee_machine_bob:1; /* (at coffee-machine bob) */
	 unsigned int Sat_water_tap_alice:1; /* (at water-tap alice) */
	 unsigned int Sat_water_tap_bob:1; /* (at water-tap bob) */
	 unsigned int Sat_coffee_jar_alice:1; /* (at coffee-jar alice) */
	 unsigned int Sat_coffee_jar_bob:1; /* (at coffee-jar bob) */
	 unsigned int Sat_outside_alice:1; /* (at outside alice) */
	 unsigned int Sat_outside_bob:1; /* (at outside bob) */
	 unsigned int Shands_free_alice:1; /* (hands-free alice) */
	 unsigned int Shands_free_bob:1; /* (hands-free bob) */
	 unsigned int Sholds_water_alice:1; /* (holds water alice) */
	 unsigned int Sholds_water_bob:1; /* (holds water bob) */
	 unsigned int Sholds_paper_alice:1; /* (holds paper alice) */
	 unsigned int Sholds_paper_bob:1; /* (holds paper bob) */
	 unsigned int Sholds_ground_coffee_alice:1; /* (holds ground-coffee alice) */
	 unsigned int Sholds_ground_coffee_bob:1; /* (holds ground-coffee bob) */
	 unsigned int Sholds_coffee_alice:1; /* (holds coffee alice) */
	 unsigned int Sholds_coffee_bob:1; /* (holds coffee bob) */

	StateRec() {bzero(this,sizeof(StateRec));}
};

namespace ActionSchemes {
extern const ActionScheme s_FINISHED;
extern const ActionScheme s_BLOCKED;
extern const ActionScheme s_INITIALIZE;
extern const ActionScheme s_print;
extern const ActionScheme s__JAM_PRINTER_;
extern const ActionScheme s_repair_printer;
extern const ActionScheme s_get_coffee;
extern const ActionScheme s_fetch;
extern const ActionScheme s_replenish;
extern const ActionScheme s_goto;
extern const ActionScheme s_enter;
extern const ActionScheme s_exit;
struct S_FINISHED : Action {
	S_FINISHED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_FINISHED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_FINISHED
struct S_BLOCKED : Action {
	S_BLOCKED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_BLOCKED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_BLOCKED
struct S_INITIALIZE : Action {
	S_INITIALIZE(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_INITIALIZE, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_INITIALIZE
struct S_print : Action {
	S_print(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_print, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_print
struct S__JAM_PRINTER_ : Action {
	S__JAM_PRINTER_(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s__JAM_PRINTER_, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S__JAM_PRINTER_
struct S_repair_printer : Action {
	S_repair_printer(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_repair_printer, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_repair_printer
struct S_get_coffee : Action {
	S_get_coffee(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_get_coffee, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_get_coffee
struct S_fetch : Action {
	S_fetch(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_fetch, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_fetch
struct S_replenish : Action {
	S_replenish(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_replenish, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_replenish
struct S_goto : Action {
	S_goto(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_goto, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_goto
struct S_enter : Action {
	S_enter(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_enter, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_enter
struct S_exit : Action {
	S_exit(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_exit, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_exit
} // namespace ActionSchemes

namespace Actions {
extern const ActionSchemes::S_FINISHED a_FINISHED;
extern const ActionSchemes::S_BLOCKED a_BLOCKED;
extern const ActionSchemes::S_INITIALIZE a_INITIALIZE;
extern const ActionSchemes::S_print a_print_a;
extern const ActionSchemes::S_print a_print_b;
extern const ActionSchemes::S_print a_print_c;
extern const ActionSchemes::S__JAM_PRINTER_ a__JAM_PRINTER_;
extern const ActionSchemes::S_repair_printer a_repair_printer_alice;
extern const ActionSchemes::S_repair_printer a_repair_printer_bob;
extern const ActionSchemes::S_get_coffee a_get_coffee_alice;
extern const ActionSchemes::S_get_coffee a_get_coffee_bob;
extern const ActionSchemes::S_fetch a_fetch_alice_water_water_tap;
extern const ActionSchemes::S_fetch a_fetch_alice_paper_paper_stack;
extern const ActionSchemes::S_fetch a_fetch_alice_ground_coffee_coffee_jar;
extern const ActionSchemes::S_fetch a_fetch_bob_water_water_tap;
extern const ActionSchemes::S_fetch a_fetch_bob_paper_paper_stack;
extern const ActionSchemes::S_fetch a_fetch_bob_ground_coffee_coffee_jar;
extern const ActionSchemes::S_replenish a_replenish_alice_water_coffee_machine;
extern const ActionSchemes::S_replenish a_replenish_alice_paper_printer;
extern const ActionSchemes::S_replenish a_replenish_alice_ground_coffee_coffee_machine;
extern const ActionSchemes::S_replenish a_replenish_bob_water_coffee_machine;
extern const ActionSchemes::S_replenish a_replenish_bob_paper_printer;
extern const ActionSchemes::S_replenish a_replenish_bob_ground_coffee_coffee_machine;
extern const ActionSchemes::S_goto a_goto_alice_door_printer;
extern const ActionSchemes::S_goto a_goto_alice_door_paper_stack;
extern const ActionSchemes::S_goto a_goto_alice_door_coffee_machine;
extern const ActionSchemes::S_goto a_goto_alice_door_water_tap;
extern const ActionSchemes::S_goto a_goto_alice_door_coffee_jar;
extern const ActionSchemes::S_goto a_goto_alice_printer_door;
extern const ActionSchemes::S_goto a_goto_alice_printer_paper_stack;
extern const ActionSchemes::S_goto a_goto_alice_printer_coffee_machine;
extern const ActionSchemes::S_goto a_goto_alice_printer_water_tap;
extern const ActionSchemes::S_goto a_goto_alice_printer_coffee_jar;
extern const ActionSchemes::S_goto a_goto_alice_paper_stack_door;
extern const ActionSchemes::S_goto a_goto_alice_paper_stack_printer;
extern const ActionSchemes::S_goto a_goto_alice_paper_stack_coffee_machine;
extern const ActionSchemes::S_goto a_goto_alice_paper_stack_water_tap;
extern const ActionSchemes::S_goto a_goto_alice_paper_stack_coffee_jar;
extern const ActionSchemes::S_goto a_goto_alice_coffee_machine_door;
extern const ActionSchemes::S_goto a_goto_alice_coffee_machine_printer;
extern const ActionSchemes::S_goto a_goto_alice_coffee_machine_paper_stack;
extern const ActionSchemes::S_goto a_goto_alice_coffee_machine_water_tap;
extern const ActionSchemes::S_goto a_goto_alice_coffee_machine_coffee_jar;
extern const ActionSchemes::S_goto a_goto_alice_water_tap_door;
extern const ActionSchemes::S_goto a_goto_alice_water_tap_printer;
extern const ActionSchemes::S_goto a_goto_alice_water_tap_paper_stack;
extern const ActionSchemes::S_goto a_goto_alice_water_tap_coffee_machine;
extern const ActionSchemes::S_goto a_goto_alice_water_tap_coffee_jar;
extern const ActionSchemes::S_goto a_goto_alice_coffee_jar_door;
extern const ActionSchemes::S_goto a_goto_alice_coffee_jar_printer;
extern const ActionSchemes::S_goto a_goto_alice_coffee_jar_paper_stack;
extern const ActionSchemes::S_goto a_goto_alice_coffee_jar_coffee_machine;
extern const ActionSchemes::S_goto a_goto_alice_coffee_jar_water_tap;
extern const ActionSchemes::S_goto a_goto_bob_door_printer;
extern const ActionSchemes::S_goto a_goto_bob_door_paper_stack;
extern const ActionSchemes::S_goto a_goto_bob_door_coffee_machine;
extern const ActionSchemes::S_goto a_goto_bob_door_water_tap;
extern const ActionSchemes::S_goto a_goto_bob_door_coffee_jar;
extern const ActionSchemes::S_goto a_goto_bob_printer_door;
extern const ActionSchemes::S_goto a_goto_bob_printer_paper_stack;
extern const ActionSchemes::S_goto a_goto_bob_printer_coffee_machine;
extern const ActionSchemes::S_goto a_goto_bob_printer_water_tap;
extern const ActionSchemes::S_goto a_goto_bob_printer_coffee_jar;
extern const ActionSchemes::S_goto a_goto_bob_paper_stack_door;
extern const ActionSchemes::S_goto a_goto_bob_paper_stack_printer;
extern const ActionSchemes::S_goto a_goto_bob_paper_stack_coffee_machine;
extern const ActionSchemes::S_goto a_goto_bob_paper_stack_water_tap;
extern const ActionSchemes::S_goto a_goto_bob_paper_stack_coffee_jar;
extern const ActionSchemes::S_goto a_goto_bob_coffee_machine_door;
extern const ActionSchemes::S_goto a_goto_bob_coffee_machine_printer;
extern const ActionSchemes::S_goto a_goto_bob_coffee_machine_paper_stack;
extern const ActionSchemes::S_goto a_goto_bob_coffee_machine_water_tap;
extern const ActionSchemes::S_goto a_goto_bob_coffee_machine_coffee_jar;
extern const ActionSchemes::S_goto a_goto_bob_water_tap_door;
extern const ActionSchemes::S_goto a_goto_bob_water_tap_printer;
extern const ActionSchemes::S_goto a_goto_bob_water_tap_paper_stack;
extern const ActionSchemes::S_goto a_goto_bob_water_tap_coffee_machine;
extern const ActionSchemes::S_goto a_goto_bob_water_tap_coffee_jar;
extern const ActionSchemes::S_goto a_goto_bob_coffee_jar_door;
extern const ActionSchemes::S_goto a_goto_bob_coffee_jar_printer;
extern const ActionSchemes::S_goto a_goto_bob_coffee_jar_paper_stack;
extern const ActionSchemes::S_goto a_goto_bob_coffee_jar_coffee_machine;
extern const ActionSchemes::S_goto a_goto_bob_coffee_jar_water_tap;
extern const ActionSchemes::S_enter a_enter_alice;
extern const ActionSchemes::S_enter a_enter_bob;
extern const ActionSchemes::S_exit a_exit_alice;
extern const ActionSchemes::S_exit a_exit_bob;
} // namespace ActionSchemes
