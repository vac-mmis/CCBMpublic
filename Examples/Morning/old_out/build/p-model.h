#pragma once
extern char const * const agentNames[NAGENTS];
extern int const nOpsForAgent[NAGENTS];
extern int const * const opsForAgent[NAGENTS];

struct StateRec {
	 unsigned int Slocation_max_hall:1; /* (location max hall) */
	 unsigned int Slocation_max_bathroom:1; /* (location max bathroom) */
	 unsigned int Sis_dirty_max:1; /* (is_dirty max) */

	StateRec() {bzero(this,sizeof(StateRec));}
};

namespace ActionSchemes {
extern const ActionScheme s_FINISHED;
extern const ActionScheme s_BLOCKED;
extern const ActionScheme s_INITIALIZE;
extern const ActionScheme s_visit_bath;
struct S_FINISHED : Action {
	S_FINISHED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_FINISHED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_FINISHED
struct S_BLOCKED : Action {
	S_BLOCKED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_BLOCKED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_BLOCKED
struct S_INITIALIZE : Action {
	S_INITIALIZE(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_INITIALIZE, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_INITIALIZE
struct S_visit_bath : Action {
	S_visit_bath(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_visit_bath, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_visit_bath
} // namespace ActionSchemes

namespace Actions {
extern const ActionSchemes::S_FINISHED a_FINISHED;
extern const ActionSchemes::S_BLOCKED a_BLOCKED;
extern const ActionSchemes::S_INITIALIZE a_INITIALIZE;
extern const ActionSchemes::S_visit_bath a_visit_bath_max;
} // namespace ActionSchemes
