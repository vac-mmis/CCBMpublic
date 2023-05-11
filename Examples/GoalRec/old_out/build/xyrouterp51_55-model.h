#pragma once
extern char const * const agentNames[NAGENTS];
extern int const nOpsForAgent[NAGENTS];
extern int const * const opsForAgent[NAGENTS];

struct StateRec {
	 unsigned int Sconnected_55_54:1; /* (connected 55 54) */
	 unsigned int Sat_11:1; /* (at 11) */
	 unsigned int Sat_12:1; /* (at 12) */
	 unsigned int Sat_13:1; /* (at 13) */
	 unsigned int Sat_14:1; /* (at 14) */
	 unsigned int Sat_15:1; /* (at 15) */
	 unsigned int Sat_21:1; /* (at 21) */
	 unsigned int Sat_22:1; /* (at 22) */
	 unsigned int Sat_23:1; /* (at 23) */
	 unsigned int Sat_24:1; /* (at 24) */
	 unsigned int Sat_25:1; /* (at 25) */
	 unsigned int Sat_31:1; /* (at 31) */
	 unsigned int Sat_32:1; /* (at 32) */
	 unsigned int Sat_33:1; /* (at 33) */
	 unsigned int Sat_34:1; /* (at 34) */
	 unsigned int Sat_35:1; /* (at 35) */
	 unsigned int Sat_41:1; /* (at 41) */
	 unsigned int Sat_42:1; /* (at 42) */
	 unsigned int Sat_43:1; /* (at 43) */
	 unsigned int Sat_44:1; /* (at 44) */
	 unsigned int Sat_45:1; /* (at 45) */
	 unsigned int Sat_51:1; /* (at 51) */
	 unsigned int Sat_52:1; /* (at 52) */
	 unsigned int Sat_53:1; /* (at 53) */
	 unsigned int Sat_54:1; /* (at 54) */
	 unsigned int Sat_55:1; /* (at 55) */

	StateRec() {bzero(this,sizeof(StateRec));}
};

namespace ActionSchemes {
extern const ActionScheme s_FINISHED;
extern const ActionScheme s_BLOCKED;
extern const ActionScheme s_INITIALIZE;
extern const ActionScheme s_step;
struct S_FINISHED : Action {
	S_FINISHED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_FINISHED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_FINISHED
struct S_BLOCKED : Action {
	S_BLOCKED(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_BLOCKED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_BLOCKED
struct S_INITIALIZE : Action {
	S_INITIALIZE(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_INITIALIZE, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_INITIALIZE
struct S_step : Action {
	S_step(int id, const std::string &name, ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
		Action(id, name, &s_step, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}}; // struct S_step
} // namespace ActionSchemes

namespace Actions {
extern const ActionSchemes::S_FINISHED a_FINISHED;
extern const ActionSchemes::S_BLOCKED a_BLOCKED;
extern const ActionSchemes::S_INITIALIZE a_INITIALIZE;
extern const ActionSchemes::S_step a_step_11_12;
extern const ActionSchemes::S_step a_step_11_21;
extern const ActionSchemes::S_step a_step_12_11;
extern const ActionSchemes::S_step a_step_12_13;
extern const ActionSchemes::S_step a_step_12_22;
extern const ActionSchemes::S_step a_step_13_12;
extern const ActionSchemes::S_step a_step_13_14;
extern const ActionSchemes::S_step a_step_13_23;
extern const ActionSchemes::S_step a_step_14_13;
extern const ActionSchemes::S_step a_step_14_15;
extern const ActionSchemes::S_step a_step_14_24;
extern const ActionSchemes::S_step a_step_15_14;
extern const ActionSchemes::S_step a_step_15_25;
extern const ActionSchemes::S_step a_step_21_11;
extern const ActionSchemes::S_step a_step_21_22;
extern const ActionSchemes::S_step a_step_21_31;
extern const ActionSchemes::S_step a_step_22_12;
extern const ActionSchemes::S_step a_step_22_21;
extern const ActionSchemes::S_step a_step_22_23;
extern const ActionSchemes::S_step a_step_22_32;
extern const ActionSchemes::S_step a_step_23_13;
extern const ActionSchemes::S_step a_step_23_22;
extern const ActionSchemes::S_step a_step_23_24;
extern const ActionSchemes::S_step a_step_23_33;
extern const ActionSchemes::S_step a_step_24_14;
extern const ActionSchemes::S_step a_step_24_23;
extern const ActionSchemes::S_step a_step_24_25;
extern const ActionSchemes::S_step a_step_24_34;
extern const ActionSchemes::S_step a_step_25_15;
extern const ActionSchemes::S_step a_step_25_24;
extern const ActionSchemes::S_step a_step_25_35;
extern const ActionSchemes::S_step a_step_31_21;
extern const ActionSchemes::S_step a_step_31_32;
extern const ActionSchemes::S_step a_step_31_41;
extern const ActionSchemes::S_step a_step_32_22;
extern const ActionSchemes::S_step a_step_32_31;
extern const ActionSchemes::S_step a_step_32_33;
extern const ActionSchemes::S_step a_step_32_42;
extern const ActionSchemes::S_step a_step_33_23;
extern const ActionSchemes::S_step a_step_33_32;
extern const ActionSchemes::S_step a_step_33_34;
extern const ActionSchemes::S_step a_step_33_43;
extern const ActionSchemes::S_step a_step_34_24;
extern const ActionSchemes::S_step a_step_34_33;
extern const ActionSchemes::S_step a_step_34_35;
extern const ActionSchemes::S_step a_step_34_44;
extern const ActionSchemes::S_step a_step_35_25;
extern const ActionSchemes::S_step a_step_35_34;
extern const ActionSchemes::S_step a_step_35_45;
extern const ActionSchemes::S_step a_step_41_31;
extern const ActionSchemes::S_step a_step_41_42;
extern const ActionSchemes::S_step a_step_41_51;
extern const ActionSchemes::S_step a_step_42_32;
extern const ActionSchemes::S_step a_step_42_41;
extern const ActionSchemes::S_step a_step_42_43;
extern const ActionSchemes::S_step a_step_42_52;
extern const ActionSchemes::S_step a_step_43_33;
extern const ActionSchemes::S_step a_step_43_42;
extern const ActionSchemes::S_step a_step_43_44;
extern const ActionSchemes::S_step a_step_43_53;
extern const ActionSchemes::S_step a_step_44_34;
extern const ActionSchemes::S_step a_step_44_43;
extern const ActionSchemes::S_step a_step_44_45;
extern const ActionSchemes::S_step a_step_44_54;
extern const ActionSchemes::S_step a_step_45_35;
extern const ActionSchemes::S_step a_step_45_44;
extern const ActionSchemes::S_step a_step_45_55;
extern const ActionSchemes::S_step a_step_51_41;
extern const ActionSchemes::S_step a_step_51_52;
extern const ActionSchemes::S_step a_step_52_42;
extern const ActionSchemes::S_step a_step_52_51;
extern const ActionSchemes::S_step a_step_52_53;
extern const ActionSchemes::S_step a_step_53_43;
extern const ActionSchemes::S_step a_step_53_52;
extern const ActionSchemes::S_step a_step_53_54;
extern const ActionSchemes::S_step a_step_54_44;
extern const ActionSchemes::S_step a_step_54_53;
extern const ActionSchemes::S_step a_step_54_55;
extern const ActionSchemes::S_step a_step_55_45;
} // namespace ActionSchemes
