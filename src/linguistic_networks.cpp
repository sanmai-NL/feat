#include <fstream>
#include <iostream>
#include <map>
#include <Rcpp.h>
// [[Rcpp::depends(BH)]]
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>
#include <boost/graph/iteration_macros.hpp>
#include <boost/graph/random.hpp>
#include <boost/range/iterator_range.hpp>
#include <format.h>

// TODO: using namespace std::literals::string_literals;

using Rcpp::CharacterVector;
using Rcpp::DataFrame;
using Rcpp::IntegerVector;
using Rcpp::NumericVector;
using Rcpp::Rcerr;
using Rcpp::stop;
using std::floor;
using std::map;
using std::pair;
using std::string;
using std::vector;
using boost::make_iterator_range;

using Weight = double;
using Feature = string;
using FeaturesMap = map<const ::Feature, ::Weight>;

constexpr ::Weight graphviz_scaling_factor = 100.0;
constexpr ::Weight null_weight{};
struct Vertex {
    // TODO:
    // string name;
};
struct Arc {
    string label{};
    ::Weight weight{};
    /* TODO: enforce positive integer weights. */
};
/* Linguistic network graphs */
// TODO: use compressed sparse row graph.
using LinguisticNetwork = boost::adjacency_list<boost::vecS, boost::vecS,
                                                boost::directedS, Vertex, Arc>;
using V = unsigned long;
using ArcDescriptor = LinguisticNetwork::edge_descriptor;
using Trail = vector<::ArcDescriptor>;
using Trails = vector<::Trail>;
using OutEdgeIterator =
    boost::graph_traits<::LinguisticNetwork>::out_edge_iterator;

template <class ArcLabel, class Weight> class edge_writer {
  public:
    edge_writer(ArcLabel label, Weight weight)
        : arc_label(label), arc_weight(weight) {}

    template <class Arc>
    void operator()(std::ostream& out, const Arc& edge_index) const {
        fmt::fprintf(out, "[label=\"%s\", penwidth=%f]\n",
                     arc_label[edge_index],
                     arc_weight[edge_index] * graphviz_scaling_factor);
    }

  private:
    ArcLabel arc_label;
    Weight arc_weight;
};

template <class ArcLabel, class Weight>
inline edge_writer<ArcLabel, Weight> make_edge_writer(ArcLabel label,
                                                      Weight weight) {
    return edge_writer<ArcLabel, Weight>(label, weight);
}

void write_SRILM_counts_file(const string& SRILM_counts_file_path,
                             const ::FeaturesMap&& features_map) {
    std::ofstream out(SRILM_counts_file_path, std::ofstream::out);
    for (const auto& feature : features_map) {
        // TODO: ofstream << feature.first << '\t' << std::scientific <<
        // std::setprecision(std::numeric_limits<double>::max_digits10) <<
        // TODO: specify precision
        fmt::fprintf(out, "%s\t%e\n", feature.first, feature.second);
    }
    out.close();
    if (!out) {
        stop(fmt::sprintf("Could not write SRILM counts file to to '%s'.\n",
                          SRILM_counts_file_path));
    }
#ifndef NDEBUG
    Rcerr << fmt::sprintf("SRILM counts file written to to '%s'.\n",
                          SRILM_counts_file_path);
#endif
}

void write_GraphViz_file(const string& GraphViz_file_path,
                         const ::LinguisticNetwork& g) {
    std::ofstream out(GraphViz_file_path, std::ofstream::out);
    boost::write_graphviz(out, g, boost::default_writer(),
                          make_edge_writer(boost::get(&Arc::label, g),
                                           boost::get(&Arc::weight, g)));
    out.close();
    if (!out) {
        stop(fmt::sprintf("Could not write GraphViz file to '%s'.\n",
                          GraphViz_file_path));
    }
#ifndef NDEBUG
    Rcerr << fmt::sprintf("GraphViz file written to '%s'.\n",
                          GraphViz_file_path);
#endif
}

::LinguisticNetwork create_linguistic_network(const DataFrame& arcs_df) {
    const IntegerVector parents = arcs_df[0];
    const IntegerVector children = arcs_df[1];
    const CharacterVector arc_labels = arcs_df.names();
    const long n_arcs = arcs_df.nrows();
    const long n_arc_labels_plus_2 = arcs_df.size();
    ::LinguisticNetwork g{};
    for (auto arc_label_index = 2l; arc_label_index < n_arc_labels_plus_2;
         ++arc_label_index) {
        // TODO: use reference?
        const NumericVector arc_label_prior_probability =
            arcs_df[arc_label_index];
        for (auto arc_index = 0l; arc_index < n_arcs; ++arc_index) {
            // TODO: use reference?
            const ::Weight weight = arc_label_prior_probability[arc_index];
            // TODO: What if no arcs have non-null weight?
            // TODO: assert that parent_id or child_id < 0
            // TODO: perhaps use std::isgreaterequal etc.
            if (weight != null_weight) {
                const auto parent_id = parents[arc_index];
                const auto child_id = children[arc_index];
                const pair<const ::ArcDescriptor, const bool> added_edge =
                    boost::add_edge(parent_id, child_id, g);
                const bool was_arc_inserted = added_edge.second;
#ifndef NDEBUG
                Rcerr << fmt::sprintf(
                    ":%s:(%d->%d@%f):%d:->",
                    Rcpp::as<string>(arc_labels[arc_label_index]), parent_id,
                    child_id, weight, was_arc_inserted);
#endif
                /* Do not accept skipping of parallel arcs. */
                assert(was_arc_inserted);

                const ::ArcDescriptor arc_descriptor = added_edge.first;
                g[arc_descriptor].label =
                    Rcpp::as<string>(arc_labels[arc_label_index]);
                g[arc_descriptor].weight = weight;
                // TODO: Rcerr << fmt::sprintf("weight is now %e\n", weight);
            }
        }
    }
#ifndef NDEBUG
    Rcerr << "Arcs and vertex neighbours:\n";
    for (const auto& vertex : make_iterator_range(boost::vertices(g))) {
        for (const auto& neighbor :
             make_iterator_range(boost::adjacent_vertices(vertex, g))) {
            Rcerr << fmt::sprintf("%d %d\n", vertex, neighbor);
        }
    }
#endif

    return g;
}

::FeaturesMap features_map_of_ngram_trails(const ::LinguisticNetwork& g,
                                           const ::Trails& trails) {
    const ::FeaturesMap features_map = [&] {
        ::FeaturesMap features_map{};
        for (const ::Trail& trail : trails) {
            {
                ::Feature feature{};
                ::Weight weight = null_weight;
                for (const ::ArcDescriptor& arc : trail) {
                    feature += boost::get(&Arc::label, g, arc);
                    feature += " ";
                    weight +=
                        static_cast<::Weight>(boost::get(&Arc::weight, g, arc));
                }

                assert(!feature.empty());
                /* If features_map has no key /feature/, the default trivial
                 * constructor is called for the map's value type
                 * (::Weight), meaning it will be initialized as zero.
                 * Adding, because the weights are assumed to be
                 * logarithmic. */
                // TODO: (weight / trail.size()); ?
                features_map[feature] += weight;
                // TODO: } else {
                //     stop("Empty feature, so empty trail. This ");
                // }
            }
        }
        return features_map;
    }();

    if (features_map.empty()) {
        stop("Failed to fill feature-weight map. ");
    }

    return features_map;
}

::Trails accumulate_random_trails(const unsigned long n_iterations,
                                  const unsigned long n_strides_to_take,
                                  const unsigned long N,
                                  const ::LinguisticNetwork& g) {
    ::Trails trails;
    trails.reserve(n_iterations);
    const unsigned long n_vertices = boost::num_vertices(g);

    for (auto trail_iteration = 0ul; trail_iteration < n_iterations;
         ++trail_iteration) {
        // TODO: use reference?
        ::V source_vertex = boost::vertex(
            static_cast<::V>(floor(R::runif(0ul, n_vertices))), g);
#ifndef NDEBUG
        Rcerr << fmt::sprintf("Randomly drew vertex %d.\n", source_vertex);
#endif
        ::Trail trail{};
        trail.reserve(n_strides_to_take);
        {
            unsigned long source_vertex_outdegree =
                boost::out_degree(source_vertex, g);
            for (auto stride = 0ul; (stride < n_strides_to_take) &&
                                    (source_vertex_outdegree > 0ul);
                 ++stride, source_vertex_outdegree =
                               boost::out_degree(source_vertex, g)) {
                /* To draw a pseudorandom number from an uniform distribution,
                 * and to make sure the PRNG is controlled through
                 * base::set.seed in R, the only option is the R::runif
                 * function. These generate real numbers, however, that must be
                 * rounded in some way. To make sure each integer in the
                 * interval [0, n] has approximately equal probability of being
                 * drawn, do not round but floor with expanded interval of [0.0,
                 * n
                 * + 1.0]. The outdegree does not equal the maximum arc index,
                 * as arcs have a zero-based index. This circumstance is
                 * exploited to expand the interval without performing any extra
                 * arithmetic. */
                const unsigned long pseudorandom_outgoing_arc_index =
                    source_vertex_outdegree == 1ul
                        ? 0ul
                        : static_cast<unsigned long>(
                              floor(R::runif(0ul, source_vertex_outdegree)));
                pair<OutEdgeIterator, OutEdgeIterator> outgoing_arcs =
                    boost::out_edges(source_vertex, g);
                /* Randomly select out arc. */
                std::advance(outgoing_arcs.first,
                             pseudorandom_outgoing_arc_index);
                const ::ArcDescriptor random_outgoing_arc =
                    *(outgoing_arcs).first;
                trail.push_back(random_outgoing_arc);
#ifndef NDEBUG
                const ::V target_vertex = boost::target(random_outgoing_arc, g);
                Rcerr << fmt::sprintf(
                    "+> Randomly went down arc (%d, %d) of %d outgoing arcs.\n",
                    source_vertex, target_vertex, source_vertex_outdegree);
                source_vertex = target_vertex;
#else
                source_vertex = boost::target(random_outgoing_arc, g);
#endif
            }
#ifndef NDEBUG
            /* Trail terminated before having taken n_strides_to_take
             * strides. */
            Rcerr << fmt::sprintf("-> Trail terminated at vertex %d\n",
                                  source_vertex);
#endif
        }
        // TODO: initialize trail lazily.
        if (!trail.empty()) {
            trail.shrink_to_fit();
            trails.push_back(trail);
#ifndef NDEBUG
            for (const ::Trail& trail : trails) {
                for (const ::ArcDescriptor& arc : trail)
                    Rcerr << fmt::sprintf("->%s",
                                          boost::get(&Arc::label, g, arc));
                Rcerr << fmt::sprintf(" (%d strides)", trail.size());
            }
#endif
        }
    }
    return trails;
}

::Trails
accumulate_ngram_trails(const ::Trails&& trails, const unsigned long N,
                        const unsigned long expected_ngram_count_per_trail,
                        const bool is_all_ngrams_up_to_n,
                        const ::LinguisticNetwork& g) {
    ::Trails ngram_trails{};
    ngram_trails.reserve(trails.size() * expected_ngram_count_per_trail);
    for (const ::Trail& trail : trails) {
        const unsigned long trail_size = trail.size();
#ifndef NDEBUG
        Rcerr << fmt::sprintf("Trail size: %d\n", trail_size);
#endif
        for (auto window_start = 0ul; window_start <= trail_size;
             ++window_start) {
#ifndef NDEBUG
            Rcerr << fmt::sprintf("\twindow start: %d\n");
#endif
            for (auto n = is_all_ngrams_up_to_n ? 1ul : N;
                 (n <= N) && ((window_start + n) <= trail_size); ++n) {
                const auto first_token_it = std::next(
                    trail.begin(), static_cast<signed long>(window_start));
                const auto last_token_it =
                    std::next(first_token_it, static_cast<signed long>(n));
                const ::Trail ngram_trail(first_token_it, last_token_it);
                assert(!ngram_trail.empty());
#ifndef NDEBUG
                for (const ::ArcDescriptor& arc : ngram_trail) {
                    Rcerr << fmt::sprintf("=>%s",
                                          boost::get(&Arc::label, g, arc));
                }
                Rcerr << '\n';
#endif
                ngram_trails.push_back(ngram_trail);
            }
        }
    }

#ifndef NDEBUG
    if (!ngram_trails.empty()) {
        ngram_trails.shrink_to_fit();
        Rcerr << fmt::sprintf("Accumulated %d n-grams of trails.\n",
                              ngram_trails.size());
    } else {
        Rcerr << "Could not accumulate any n-gram in any trail.\n";
    }
#endif

    return ngram_trails;
}
/* Calculate an expected maximum of the number of n-gram trails for
 * preallocation. */
inline unsigned long calculate_expected_ngram_count_per_trail(
    const unsigned long N, const unsigned long n_strides_to_take) {
    unsigned long expected_ngram_count_per_trail =
        N * (n_strides_to_take + 1ul);
    for (auto ngram_order = 1ul; ngram_order <= N; ++ngram_order)
        expected_ngram_count_per_trail -= ngram_order;
    return expected_ngram_count_per_trail;
}

// TODO: M: n_strides_to_take
// TODO: I: n_iterations
// TODO: implement use_boolean_instead_of_real_scoring
// [[Rcpp::export]]
void search_random_trails_in_linguistic_network(
    const DataFrame& arcs_df, const unsigned long M, const unsigned long N,
    const bool is_all_ngrams_up_to_n, const unsigned long I,
    const bool use_boolean_instead_of_real_scoring,
    const std::string& GraphViz_file_path,
    const std::string& SRILM_counts_file_path) {
    const ::LinguisticNetwork g = create_linguistic_network(arcs_df);
#ifndef NDEBUG
    Rcerr << fmt::sprintf(
        "Created a linguistic network of %d vertices and %d arcs.\n",
        boost::num_vertices(g), boost::num_edges(g));
#endif
    write_GraphViz_file(GraphViz_file_path, g);

    ::Trails trails = accumulate_random_trails(I, M, N, g);
    if (!trails.empty()) {
#ifndef NDEBUG
        Rcerr << fmt::sprintf("Accumulated %d trails.\n", trails.size());
#endif
        const unsigned long expected_ngram_count_per_trail =
            calculate_expected_ngram_count_per_trail(N, M);
#ifndef NDEBUG
        Rcerr << fmt::sprintf("Expecting at most %d n-grams in trails.\n",
                              trails.size() * expected_ngram_count_per_trail);
#endif
        // TODO: improve
        ::Trails ngram_trails = accumulate_ngram_trails(
            std::move(trails), N, expected_ngram_count_per_trail,
            is_all_ngrams_up_to_n, g);
        ::Trails().swap(trails);

        if (!ngram_trails.empty()) {
            const ::FeaturesMap FeaturesMap =
                features_map_of_ngram_trails(g, std::move(ngram_trails));
            ::Trails().swap(ngram_trails);
            write_SRILM_counts_file(SRILM_counts_file_path,
                                    std::move(FeaturesMap));
        } else {
            stop("Failed to accumulate any n-gram in the trails. ");
        }
    } else {
        stop("Failed to accumulate any trail. ");
    }
#ifndef NDEBUG
    Rcerr << "Completed search in a linguistic network.\n";
#endif
}