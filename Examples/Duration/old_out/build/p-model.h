#pragma once
extern char const * const agentNames[NAGENTS];
extern int const nOpsForAgent[NAGENTS];
extern int const * const opsForAgent[NAGENTS];

struct StateRec {
	 unsigned int Sat_knife_counter:1; /* (at knife counter) */

	StateRec() {bzero(this,sizeof(StateRec));}
};

namespace ActionSchemes {
extern const ActionScheme s_FINISHED;
extern const ActionScheme s_BLOCKED;
extern const ActionScheme s_INITIALIZE;
extern const ActionScheme s_take;
struct S_FINISHED : Action {
	S_FINISHED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_FINISHED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_FINISHED
struct S_BLOCKED : Action {
	S_BLOCKED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_BLOCKED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_BLOCKED
struct S_INITIALIZE : Action {
	S_INITIALIZE(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_INITIALIZE, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_INITIALIZE
struct S_take : Action {
	S_take(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_take, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_take
} // namespace ActionSchemes

namespace Actions {
extern const ActionSchemes::S_FINISHED a_FINISHED;
extern const ActionSchemes::S_BLOCKED a_BLOCKED;
extern const ActionSchemes::S_INITIALIZE a_INITIALIZE;
extern const ActionSchemes::S_take a_take_knife_counter_2_2;
extern const ActionSchemes::S_take a_take_knife_counter_2_5;
extern const ActionSchemes::S_take a_take_knife_counter_2_8;
extern const ActionSchemes::S_take a_take_knife_counter_2_10;
extern const ActionSchemes::S_take a_take_knife_counter_2_15;
extern const ActionSchemes::S_take a_take_knife_counter_5_2;
extern const ActionSchemes::S_take a_take_knife_counter_5_5;
extern const ActionSchemes::S_take a_take_knife_counter_5_8;
extern const ActionSchemes::S_take a_take_knife_counter_5_10;
extern const ActionSchemes::S_take a_take_knife_counter_5_15;
extern const ActionSchemes::S_take a_take_knife_counter_8_2;
extern const ActionSchemes::S_take a_take_knife_counter_8_5;
extern const ActionSchemes::S_take a_take_knife_counter_8_8;
extern const ActionSchemes::S_take a_take_knife_counter_8_10;
extern const ActionSchemes::S_take a_take_knife_counter_8_15;
extern const ActionSchemes::S_take a_take_knife_counter_10_2;
extern const ActionSchemes::S_take a_take_knife_counter_10_5;
extern const ActionSchemes::S_take a_take_knife_counter_10_8;
extern const ActionSchemes::S_take a_take_knife_counter_10_10;
extern const ActionSchemes::S_take a_take_knife_counter_10_15;
extern const ActionSchemes::S_take a_take_knife_counter_15_2;
extern const ActionSchemes::S_take a_take_knife_counter_15_5;
extern const ActionSchemes::S_take a_take_knife_counter_15_8;
extern const ActionSchemes::S_take a_take_knife_counter_15_10;
extern const ActionSchemes::S_take a_take_knife_counter_15_15;
} // namespace ActionSchemes
