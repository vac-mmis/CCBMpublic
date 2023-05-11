#pragma once
extern char const * const agentNames[NAGENTS];
extern int const nOpsForAgent[NAGENTS];
extern int const * const opsForAgent[NAGENTS];

struct StateRec {
	 unsigned int Sreachable_sink_plate:1; /* (reachable sink plate) */
	 unsigned int Sreachable_sink_cutting_board:1; /* (reachable sink cutting_board) */
	 unsigned int Sreachable_sink_sink:1; /* (reachable sink sink) */
	 unsigned int Sreachable_counter_plate:1; /* (reachable counter plate) */
	 unsigned int Sreachable_counter_cutting_board:1; /* (reachable counter cutting_board) */
	 unsigned int Sreachable_counter_counter:1; /* (reachable counter counter) */
	 unsigned int Sreachable_counter_cupboard:1; /* (reachable counter cupboard) */
	 unsigned int Sreachable_counter_stove:1; /* (reachable counter stove) */
	 unsigned int Sreachable_table_table:1; /* (reachable table table) */
	 unsigned int Sisat_sink:1; /* (isat sink) */
	 unsigned int Sisat_counter:1; /* (isat counter) */
	 unsigned int Sat_plate_sink:1; /* (at plate sink) */
	 unsigned int Sat_plate_cupboard:1; /* (at plate cupboard) */
	 unsigned int Sat_cutting_board_sink:1; /* (at cutting_board sink) */
	 unsigned int Sat_cutting_board_cupboard:1; /* (at cutting_board cupboard) */
	 unsigned int Sat_carrot_plate:1; /* (at carrot plate) */
	 unsigned int Sat_carrot_cutting_board:1; /* (at carrot cutting_board) */
	 unsigned int Sat_carrot_sink:1; /* (at carrot sink) */
	 unsigned int Sat_carrot_cupboard:1; /* (at carrot cupboard) */
	 unsigned int Sat_knife_plate:1; /* (at knife plate) */
	 unsigned int Sat_knife_cutting_board:1; /* (at knife cutting_board) */
	 unsigned int Sat_knife_sink:1; /* (at knife sink) */
	 unsigned int Sat_knife_cupboard:1; /* (at knife cupboard) */
	 unsigned int Sat_wooden_spoon_plate:1; /* (at wooden_spoon plate) */
	 unsigned int Sat_wooden_spoon_cutting_board:1; /* (at wooden_spoon cutting_board) */
	 unsigned int Sat_wooden_spoon_sink:1; /* (at wooden_spoon sink) */
	 unsigned int Sat_wooden_spoon_cupboard:1; /* (at wooden_spoon cupboard) */
	 unsigned int Sat_spoon_plate:1; /* (at spoon plate) */
	 unsigned int Sat_spoon_cutting_board:1; /* (at spoon cutting_board) */
	 unsigned int Sat_spoon_sink:1; /* (at spoon sink) */
	 unsigned int Sat_spoon_cupboard:1; /* (at spoon cupboard) */
	 unsigned int Sat_sponge_plate:1; /* (at sponge plate) */
	 unsigned int Sat_sponge_cutting_board:1; /* (at sponge cutting_board) */
	 unsigned int Sat_sponge_sink:1; /* (at sponge sink) */
	 unsigned int Sat_sponge_cupboard:1; /* (at sponge cupboard) */
	 unsigned int Staken_plate:1; /* (taken plate) */
	 unsigned int Staken_cutting_board:1; /* (taken cutting_board) */
	 unsigned int Staken_carrot:1; /* (taken carrot) */
	 unsigned int Staken_knife:1; /* (taken knife) */
	 unsigned int Staken_wooden_spoon:1; /* (taken wooden_spoon) */
	 unsigned int Staken_spoon:1; /* (taken spoon) */
	 unsigned int Staken_sponge:1; /* (taken sponge) */
	 unsigned int Shandsfree:1; /* handsfree */

	StateRec() {bzero(this,sizeof(StateRec));}
};

namespace ActionSchemes {
extern const ActionScheme s_FINISHED;
extern const ActionScheme s_BLOCKED;
extern const ActionScheme s_INITIALIZE;
extern const ActionScheme s_move;
extern const ActionScheme s_take2;
extern const ActionScheme s_take;
extern const ActionScheme s_put2;
extern const ActionScheme s_put;
struct S_FINISHED : Action {
	S_FINISHED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_FINISHED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_FINISHED
struct S_BLOCKED : Action {
	S_BLOCKED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_BLOCKED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_BLOCKED
struct S_INITIALIZE : Action {
	S_INITIALIZE(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_INITIALIZE, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_INITIALIZE
struct S_move : Action {
	S_move(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_move, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_move
struct S_take2 : Action {
	S_take2(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_take2, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_take2
struct S_take : Action {
	S_take(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_take, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_take
struct S_put2 : Action {
	S_put2(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_put2, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_put2
struct S_put : Action {
	S_put(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_put, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_put
} // namespace ActionSchemes

namespace Actions {
extern const ActionSchemes::S_FINISHED a_FINISHED;
extern const ActionSchemes::S_BLOCKED a_BLOCKED;
extern const ActionSchemes::S_INITIALIZE a_INITIALIZE;
extern const ActionSchemes::S_move a_move_sink_counter;
extern const ActionSchemes::S_move a_move_counter_sink;
extern const ActionSchemes::S_take2 a_take_plate_sink;
extern const ActionSchemes::S_take2 a_take_plate_cupboard;
extern const ActionSchemes::S_take2 a_take_cutting_board_sink;
extern const ActionSchemes::S_take2 a_take_cutting_board_cupboard;
extern const ActionSchemes::S_take a_take_carrot_plate;
extern const ActionSchemes::S_take a_take_carrot_cutting_board;
extern const ActionSchemes::S_take a_take_carrot_sink;
extern const ActionSchemes::S_take a_take_carrot_cupboard;
extern const ActionSchemes::S_take a_take_knife_plate;
extern const ActionSchemes::S_take a_take_knife_cutting_board;
extern const ActionSchemes::S_take a_take_knife_sink;
extern const ActionSchemes::S_take a_take_knife_cupboard;
extern const ActionSchemes::S_take a_take_wooden_spoon_plate;
extern const ActionSchemes::S_take a_take_wooden_spoon_cutting_board;
extern const ActionSchemes::S_take a_take_wooden_spoon_sink;
extern const ActionSchemes::S_take a_take_wooden_spoon_cupboard;
extern const ActionSchemes::S_take a_take_spoon_plate;
extern const ActionSchemes::S_take a_take_spoon_cutting_board;
extern const ActionSchemes::S_take a_take_spoon_sink;
extern const ActionSchemes::S_take a_take_spoon_cupboard;
extern const ActionSchemes::S_take a_take_sponge_plate;
extern const ActionSchemes::S_take a_take_sponge_cutting_board;
extern const ActionSchemes::S_take a_take_sponge_sink;
extern const ActionSchemes::S_take a_take_sponge_cupboard;
extern const ActionSchemes::S_put2 a_put_plate_sink;
extern const ActionSchemes::S_put2 a_put_plate_cupboard;
extern const ActionSchemes::S_put2 a_put_cutting_board_sink;
extern const ActionSchemes::S_put2 a_put_cutting_board_cupboard;
extern const ActionSchemes::S_put a_put_carrot_plate;
extern const ActionSchemes::S_put a_put_carrot_cutting_board;
extern const ActionSchemes::S_put a_put_carrot_sink;
extern const ActionSchemes::S_put a_put_carrot_cupboard;
extern const ActionSchemes::S_put a_put_knife_plate;
extern const ActionSchemes::S_put a_put_knife_cutting_board;
extern const ActionSchemes::S_put a_put_knife_sink;
extern const ActionSchemes::S_put a_put_knife_cupboard;
extern const ActionSchemes::S_put a_put_wooden_spoon_plate;
extern const ActionSchemes::S_put a_put_wooden_spoon_cutting_board;
extern const ActionSchemes::S_put a_put_wooden_spoon_sink;
extern const ActionSchemes::S_put a_put_wooden_spoon_cupboard;
extern const ActionSchemes::S_put a_put_spoon_plate;
extern const ActionSchemes::S_put a_put_spoon_cutting_board;
extern const ActionSchemes::S_put a_put_spoon_sink;
extern const ActionSchemes::S_put a_put_spoon_cupboard;
extern const ActionSchemes::S_put a_put_sponge_plate;
extern const ActionSchemes::S_put a_put_sponge_cutting_board;
extern const ActionSchemes::S_put a_put_sponge_sink;
extern const ActionSchemes::S_put a_put_sponge_cupboard;
} // namespace ActionSchemes
