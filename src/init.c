#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _oppr_rcpp_add_raw_data(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_apply_decisions(SEXP, SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_apply_feature_weights(SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_apply_locked_action_constraints(SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_apply_locked_project_constraints(SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_apply_max_phylo_div_objective(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_apply_max_targets_met_objective(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_apply_min_set_objective(SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_branch_matrix(SEXP);
extern SEXP _oppr_rcpp_branch_order(SEXP);
extern SEXP _oppr_rcpp_compile_multi_obj_problem(SEXP);
extern SEXP _oppr_rcpp_convert_abs_constraint_approach(SEXP, SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_convert_pwlobj(SEXP);
extern SEXP _oppr_rcpp_convert_ref_point_method_step1(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_convert_ref_point_method_step2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_convert_wtd_goal_method(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_copy_optimization_problem(SEXP);
extern SEXP _oppr_rcpp_evaluate_max_phylo_div_objective(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_evaluate_max_targets_met_objective(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_evaluate_min_set_objective(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_expected_persistences(SEXP, SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_funded_projects(SEXP, SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_A(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_col_ids(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_lb(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_modelsense(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_ncell(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_ncol(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_nrow(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_number_of_actions(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_number_of_allocations(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_number_of_branches(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_number_of_features(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_number_of_projects(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_obj(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_pwlobj(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_rhs(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_row_ids(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_sense(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_ub(SEXP);
extern SEXP _oppr_rcpp_get_optimization_problem_vtype(SEXP);
extern SEXP _oppr_rcpp_heuristic_solution(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_new_optimization_problem(SEXP, SEXP, SEXP);
extern SEXP _oppr_rcpp_optimization_problem_as_list(SEXP);
extern SEXP _oppr_rcpp_predefined_optimization_problem(SEXP);
extern SEXP _oppr_rcpp_random_solution(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_oppr_rcpp_add_raw_data",                                   (DL_FUNC) &_oppr_rcpp_add_raw_data,                                    6},
    {"_oppr_rcpp_apply_decisions",                                (DL_FUNC) &_oppr_rcpp_apply_decisions,                                 4},
    {"_oppr_rcpp_apply_feature_weights",                          (DL_FUNC) &_oppr_rcpp_apply_feature_weights,                           3},
    {"_oppr_rcpp_apply_locked_action_constraints",                (DL_FUNC) &_oppr_rcpp_apply_locked_action_constraints,                 3},
    {"_oppr_rcpp_apply_locked_project_constraints",               (DL_FUNC) &_oppr_rcpp_apply_locked_project_constraints,                3},
    {"_oppr_rcpp_apply_max_phylo_div_objective",                  (DL_FUNC) &_oppr_rcpp_apply_max_phylo_div_objective,                   5},
    {"_oppr_rcpp_apply_max_targets_met_objective",                (DL_FUNC) &_oppr_rcpp_apply_max_targets_met_objective,                 5},
    {"_oppr_rcpp_apply_min_set_objective",                        (DL_FUNC) &_oppr_rcpp_apply_min_set_objective,                         3},
    {"_oppr_rcpp_branch_matrix",                                  (DL_FUNC) &_oppr_rcpp_branch_matrix,                                   1},
    {"_oppr_rcpp_branch_order",                                   (DL_FUNC) &_oppr_rcpp_branch_order,                                    1},
    {"_oppr_rcpp_compile_multi_obj_problem",                      (DL_FUNC) &_oppr_rcpp_compile_multi_obj_problem,                       1},
    {"_oppr_rcpp_convert_abs_constraint_approach",                (DL_FUNC) &_oppr_rcpp_convert_abs_constraint_approach,                 4},
    {"_oppr_rcpp_convert_pwlobj",                                 (DL_FUNC) &_oppr_rcpp_convert_pwlobj,                                  1},
    {"_oppr_rcpp_convert_ref_point_method_step1",                 (DL_FUNC) &_oppr_rcpp_convert_ref_point_method_step1,                  7},
    {"_oppr_rcpp_convert_ref_point_method_step2",                 (DL_FUNC) &_oppr_rcpp_convert_ref_point_method_step2,                  6},
    {"_oppr_rcpp_convert_wtd_goal_method",                        (DL_FUNC) &_oppr_rcpp_convert_wtd_goal_method,                         5},
    {"_oppr_rcpp_copy_optimization_problem",                      (DL_FUNC) &_oppr_rcpp_copy_optimization_problem,                       1},
    {"_oppr_rcpp_evaluate_max_phylo_div_objective",               (DL_FUNC) &_oppr_rcpp_evaluate_max_phylo_div_objective,                8},
    {"_oppr_rcpp_evaluate_max_targets_met_objective",             (DL_FUNC) &_oppr_rcpp_evaluate_max_targets_met_objective,              8},
    {"_oppr_rcpp_evaluate_min_set_objective",                     (DL_FUNC) &_oppr_rcpp_evaluate_min_set_objective,                      8},
    {"_oppr_rcpp_expected_persistences",                          (DL_FUNC) &_oppr_rcpp_expected_persistences,                           4},
    {"_oppr_rcpp_funded_projects",                                (DL_FUNC) &_oppr_rcpp_funded_projects,                                 2},
    {"_oppr_rcpp_get_optimization_problem_A",                     (DL_FUNC) &_oppr_rcpp_get_optimization_problem_A,                      1},
    {"_oppr_rcpp_get_optimization_problem_col_ids",               (DL_FUNC) &_oppr_rcpp_get_optimization_problem_col_ids,                1},
    {"_oppr_rcpp_get_optimization_problem_lb",                    (DL_FUNC) &_oppr_rcpp_get_optimization_problem_lb,                     1},
    {"_oppr_rcpp_get_optimization_problem_modelsense",            (DL_FUNC) &_oppr_rcpp_get_optimization_problem_modelsense,             1},
    {"_oppr_rcpp_get_optimization_problem_ncell",                 (DL_FUNC) &_oppr_rcpp_get_optimization_problem_ncell,                  1},
    {"_oppr_rcpp_get_optimization_problem_ncol",                  (DL_FUNC) &_oppr_rcpp_get_optimization_problem_ncol,                   1},
    {"_oppr_rcpp_get_optimization_problem_nrow",                  (DL_FUNC) &_oppr_rcpp_get_optimization_problem_nrow,                   1},
    {"_oppr_rcpp_get_optimization_problem_number_of_actions",     (DL_FUNC) &_oppr_rcpp_get_optimization_problem_number_of_actions,      1},
    {"_oppr_rcpp_get_optimization_problem_number_of_allocations", (DL_FUNC) &_oppr_rcpp_get_optimization_problem_number_of_allocations,  1},
    {"_oppr_rcpp_get_optimization_problem_number_of_branches",    (DL_FUNC) &_oppr_rcpp_get_optimization_problem_number_of_branches,     1},
    {"_oppr_rcpp_get_optimization_problem_number_of_features",    (DL_FUNC) &_oppr_rcpp_get_optimization_problem_number_of_features,     1},
    {"_oppr_rcpp_get_optimization_problem_number_of_projects",    (DL_FUNC) &_oppr_rcpp_get_optimization_problem_number_of_projects,     1},
    {"_oppr_rcpp_get_optimization_problem_obj",                   (DL_FUNC) &_oppr_rcpp_get_optimization_problem_obj,                    1},
    {"_oppr_rcpp_get_optimization_problem_pwlobj",                (DL_FUNC) &_oppr_rcpp_get_optimization_problem_pwlobj,                 1},
    {"_oppr_rcpp_get_optimization_problem_rhs",                   (DL_FUNC) &_oppr_rcpp_get_optimization_problem_rhs,                    1},
    {"_oppr_rcpp_get_optimization_problem_row_ids",               (DL_FUNC) &_oppr_rcpp_get_optimization_problem_row_ids,                1},
    {"_oppr_rcpp_get_optimization_problem_sense",                 (DL_FUNC) &_oppr_rcpp_get_optimization_problem_sense,                  1},
    {"_oppr_rcpp_get_optimization_problem_ub",                    (DL_FUNC) &_oppr_rcpp_get_optimization_problem_ub,                     1},
    {"_oppr_rcpp_get_optimization_problem_vtype",                 (DL_FUNC) &_oppr_rcpp_get_optimization_problem_vtype,                  1},
    {"_oppr_rcpp_heuristic_solution",                             (DL_FUNC) &_oppr_rcpp_heuristic_solution,                             14},
    {"_oppr_rcpp_new_optimization_problem",                       (DL_FUNC) &_oppr_rcpp_new_optimization_problem,                        3},
    {"_oppr_rcpp_optimization_problem_as_list",                   (DL_FUNC) &_oppr_rcpp_optimization_problem_as_list,                    1},
    {"_oppr_rcpp_predefined_optimization_problem",                (DL_FUNC) &_oppr_rcpp_predefined_optimization_problem,                 1},
    {"_oppr_rcpp_random_solution",                                (DL_FUNC) &_oppr_rcpp_random_solution,                                10},
    {NULL, NULL, 0}
};

void R_init_oppr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
