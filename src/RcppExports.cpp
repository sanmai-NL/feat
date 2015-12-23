// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// search_random_trails_in_linguistic_network
void search_random_trails_in_linguistic_network(const Rcpp::DataFrame& arcs_df, const unsigned long n_strides_to_take, const unsigned long N, const bool is_all_ngrams_up_to_n, const unsigned long n_iterations, const std::string& GraphViz_file_path, const std::string& SRILM_counts_file_path);
RcppExport SEXP feat_search_random_trails_in_linguistic_network(SEXP arcs_dfSEXP, SEXP n_strides_to_takeSEXP, SEXP NSEXP, SEXP is_all_ngrams_up_to_nSEXP, SEXP n_iterationsSEXP, SEXP GraphViz_file_pathSEXP, SEXP SRILM_counts_file_pathSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const Rcpp::DataFrame& >::type arcs_df(arcs_dfSEXP);
    Rcpp::traits::input_parameter< const unsigned long >::type n_strides_to_take(n_strides_to_takeSEXP);
    Rcpp::traits::input_parameter< const unsigned long >::type N(NSEXP);
    Rcpp::traits::input_parameter< const bool >::type is_all_ngrams_up_to_n(is_all_ngrams_up_to_nSEXP);
    Rcpp::traits::input_parameter< const unsigned long >::type n_iterations(n_iterationsSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type GraphViz_file_path(GraphViz_file_pathSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type SRILM_counts_file_path(SRILM_counts_file_pathSEXP);
    search_random_trails_in_linguistic_network(arcs_df, n_strides_to_take, N, is_all_ngrams_up_to_n, n_iterations, GraphViz_file_path, SRILM_counts_file_path);
    return R_NilValue;
END_RCPP
}
