#pragma once
extern char const * const agentNames[NAGENTS];
extern int const nOpsForAgent[NAGENTS];
extern int const * const opsForAgent[NAGENTS];

struct StateRec {
	 unsigned int SisDressed_bob:1; /* (isDressed bob) */
	 unsigned int SdrankCoffee_bob:1; /* (drankCoffee bob) */
	 unsigned int ShadBreakfast_bob:1; /* (hadBreakfast bob) */
	 unsigned int SisClean_bob:1; /* (isClean bob) */
	 unsigned int ShasLeft_bob:1; /* (hasLeft bob) */

	StateRec() {bzero(this,sizeof(StateRec));}
};

namespace ActionSchemes {
extern const ActionScheme s_FINISHED;
extern const ActionScheme s_BLOCKED;
extern const ActionScheme s_INITIALIZE;
extern const ActionScheme s_dress;
extern const ActionScheme s_coffee;
extern const ActionScheme s_breakfast;
extern const ActionScheme s_clean;
extern const ActionScheme s_leave;
struct S_FINISHED : Action {
	S_FINISHED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_FINISHED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_FINISHED
struct S_BLOCKED : Action {
	S_BLOCKED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_BLOCKED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_BLOCKED
struct S_INITIALIZE : Action {
	S_INITIALIZE(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_INITIALIZE, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_INITIALIZE
struct S_dress : Action {
	S_dress(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_dress, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_dress
struct S_coffee : Action {
	S_coffee(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_coffee, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_coffee
struct S_breakfast : Action {
	S_breakfast(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_breakfast, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_breakfast
struct S_clean : Action {
	S_clean(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_clean, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_clean
struct S_leave : Action {
	S_leave(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_leave, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_leave
} // namespace ActionSchemes

namespace Actions {
extern const ActionSchemes::S_FINISHED a_FINISHED;
extern const ActionSchemes::S_BLOCKED a_BLOCKED;
extern const ActionSchemes::S_INITIALIZE a_INITIALIZE;
extern const ActionSchemes::S_dress a_dress_bob;
extern const ActionSchemes::S_coffee a_coffee_bob;
extern const ActionSchemes::S_breakfast a_breakfast_bob;
extern const ActionSchemes::S_clean a_clean_bob;
extern const ActionSchemes::S_leave a_leave_bob;
} // namespace ActionSchemes
