#pragma once
extern char const * const agentNames[NAGENTS];
extern int const nOpsForAgent[NAGENTS];
extern int const * const opsForAgent[NAGENTS];

struct StateRec {
	 unsigned int Sis_at_sink:1; /* (is-at sink) */
	 unsigned int Sis_at_counter:1; /* (is-at counter) */
	 unsigned int Staken_carrot:1; /* (taken carrot) */
	 unsigned int Staken_knife:1; /* (taken knife) */
	 unsigned int Sat_carrot_sink:1; /* (at carrot sink) */
	 unsigned int Sat_carrot_counter:1; /* (at carrot counter) */
	 unsigned int Sat_knife_sink:1; /* (at knife sink) */
	 unsigned int Sat_knife_counter:1; /* (at knife counter) */
	 unsigned int Shandsfree:1; /* handsfree */

	StateRec() {bzero(this,sizeof(StateRec));}
};

namespace ActionSchemes {
extern const ActionScheme s_FINISHED;
extern const ActionScheme s_BLOCKED;
extern const ActionScheme s_INITIALIZE;
extern const ActionScheme s_move;
extern const ActionScheme s_take;
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
struct S_take : Action {
	S_take(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_take, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_take
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
extern const ActionSchemes::S_take a_take_carrot_sink;
extern const ActionSchemes::S_take a_take_carrot_counter;
extern const ActionSchemes::S_take a_take_knife_sink;
extern const ActionSchemes::S_take a_take_knife_counter;
extern const ActionSchemes::S_put a_put_carrot_sink;
extern const ActionSchemes::S_put a_put_carrot_counter;
extern const ActionSchemes::S_put a_put_knife_sink;
extern const ActionSchemes::S_put a_put_knife_counter;
} // namespace ActionSchemes
