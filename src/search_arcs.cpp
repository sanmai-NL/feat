#include <cmath>
#include <fstream>
#include <iostream>
#include <iterator>
#include <limits>
#include <unordered_map>

#include <Rcpp.h>

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>
#include <boost/graph/random.hpp>

/* Assumes logarithmic weights. */
typedef double Weight;
const Weight NULL_WEIGHT = 0.0;
typedef std::unordered_map <std::string, Weight> Sequence_weight_map;
struct Vertex {
    // TODO:
    // std::string name;
};
struct Arc {
    std::string label;
    ::Weight weight;
};
/* Linguistic network graphs */
// TODO: use compressed sparse row graph.
// boost::allow_parallel_edge_tag
typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, Vertex, Arc> LinguisticNetwork;
typedef unsigned long V;
typedef LinguisticNetwork::edge_descriptor ArcDescriptor;
typedef std::vector<ArcDescriptor> Trail;
typedef std::vector<Trail> Trails;

template<class ArcLabel, class Weight>
class edge_writer {
public:
    edge_writer(ArcLabel label, Weight weight) : arc_label(label), arc_weight(weight) { }

    template<class Arc>
    void operator()(std::ostream & out, const Arc & edge_index) const {
        /* Undo logarithmic transformation, raise e to arc weight. */
        const float fractional_count = std::exp(arc_weight[edge_index]);
        out << "[label=\"" << arc_label[edge_index] << "\", penwidth=" <<  fractional_count * 10.0 << "]";
    }

private:
    ArcLabel arc_label;
    Weight arc_weight;
};

template<class ArcLabel, class Weight>
inline edge_writer<ArcLabel, Weight> make_edge_writer(ArcLabel label, Weight weight) {
    return edge_writer<ArcLabel, Weight>(label, weight);
}

void write_SRILM_counts_file(const std::string & SRILM_counts_file_path, const ::Sequence_weight_map & sequence_weight_map) {
  if (!SRILM_counts_file_path.empty()) {
        std::ofstream ofstream;
        ofstream.open(SRILM_counts_file_path, std::ofstream::out);
        std::cout.precision(std::numeric_limits<float>::max_digits10);
        for (const auto & item : sequence_weight_map) {
            /* Undo logarithmic transformation, raise e to arc weight. */
            const float fractional_count = std::exp(item.second);
            ofstream << item.first << "\t" << std::scientific << fractional_count << std::endl;
        }
        ofstream.close();
        if (!ofstream) {
            Rcpp::stop("Could not write SRILM counts file to \n'" + SRILM_counts_file_path + "'.");
            // TODO: use printf.
        }
        Rcpp::Rcout << "SRILM counts file written to \n'" << SRILM_counts_file_path << "'." << std::endl;;
    }
}

void write_GraphViz_file(const std::string & GraphViz_file_path, const ::LinguisticNetwork & g){
    if (!GraphViz_file_path.empty()) {
        std::ofstream ofstream;
        ofstream.open(GraphViz_file_path, std::ofstream::out);
        boost::write_graphviz(ofstream, g,
            boost::default_writer(),
            make_edge_writer(
                boost::get(&Arc::label, g),
                boost::get(&Arc::weight, g)));
        ofstream.close();
        if (!ofstream) {
             Rcpp::stop("Could not write GraphViz file to \n'" + GraphViz_file_path + "'.");
        }
        Rcpp::Rcout << "GraphViz file written to \n'" << GraphViz_file_path << "'." << std::endl;;
    }
}

::LinguisticNetwork create_linguistic_network(const Rcpp::DataFrame & arcs_list_df, const Rcpp::NumericMatrix & arcs_scores_dmat) {
    // TODO: do not copy.
    const Rcpp::IntegerVector parents = arcs_list_df["parent_id"];
    const Rcpp::IntegerVector children = arcs_list_df["child_id"];
    const Rcpp::CharacterVector arc_labels = Rcpp::colnames(arcs_scores_dmat);
    const unsigned long parents_size = parents.size();
    const unsigned long arcs_scores_dmat_ncol = arcs_scores_dmat.ncol();
    ::LinguisticNetwork g;
    for (unsigned long index = 0ul; index < parents_size; index++) {
        const unsigned long parent_id = parents[index];
        const unsigned long child_id = children[index];
        for (unsigned long arc_index = 0ul; arc_index < arcs_scores_dmat_ncol; arc_index++) {
            ::Weight weight = arcs_scores_dmat(index, arc_index);
            // TODO: What if no arcs have null weight?
            if (weight != NULL_WEIGHT) {
                /* Add arc to g. */
                std::pair<ArcDescriptor, bool> added_arc = boost::add_edge(parent_id, child_id, g);
                const std::string arc_label = Rcpp::as<std::string>(arc_labels[arc_index]);
                g[added_arc.first].label = arc_label;
                g[added_arc.first].weight = weight;
                /* Do not accept skipping of parallel arcs. */
                Rcpp::Rcout << ":" << arc_label << ":(" << parent_id << "->" << child_id << "@" << weight << "):" << added_arc.second << ":";
                // Nastily, in case the second vertex is equal to 1ul, it will be implicitly converted to false by !added_arc.second. Prevent an assertion failure for lack of a better solution with the following comparison.
                assert(added_arc.second != false);
            }
        }
    }
#ifndef NDEBUG
    Rcpp::Rcout << "Arcs and vertex neighbours: " << std::endl;
    auto vertices = boost::vertices(g);
    for (auto vertex_it = vertices.first; vertex_it != vertices.second; ++vertex_it) {
        auto neighbors = boost::adjacent_vertices(*vertex_it, g);
        for (auto neighbors_it = neighbors.first; neighbors_it != neighbors.second; ++neighbors_it) {
            Rcpp::Rcout << *vertex_it << " " << *neighbors_it << std::endl;
        }
    }
#endif

    return(g);
}

::Sequence_weight_map sequence_weight_map_of_ngram_trails(const ::LinguisticNetwork & g, const ::Trails & trails) {
    ::Sequence_weight_map sequence_weight_map;
    for (const ::Trail & trail : trails) {
        std::string sequence = "";
        ::Weight weight = 0.0;
        for (const ::ArcDescriptor & stride : trail) {
            sequence.append(boost::get(&Arc::label, g, stride));
            sequence.append(" ");
            weight += boost::get(&Arc::weight, g, stride);
        }
        if (!sequence.empty()) {
            sequence.pop_back();
            sequence.shrink_to_fit();
            /* If sequence_seight_map has no key sequence, the default trivial constructor is called for the map's value type (::Weight), meaning it will be initialized as zero. Adding, because the weights are assumed to be logarithmic. */
            sequence_weight_map[sequence] += weight;
        } else {
            Rcpp::Rcerr << "Empty sequence, so empty walk. " << std::endl;
            Rcpp::stop("Empty sequence, so empty walk. ");
        }
    }

    if (sequence_weight_map.empty())
        Rcpp::stop("Failed to fill sequence weight map. ");

    return(sequence_weight_map);
}

::Trails accumulate_random_trails(unsigned long n_iterations, const unsigned long n_strides_to_take, unsigned long N, const ::LinguisticNetwork & g) {
    ::Trails trails;
    trails.reserve(n_iterations);
    const unsigned long n_vertices = boost::num_vertices(g);

    for (unsigned long trail_iteration = 0ul; trail_iteration < n_iterations; trail_iteration++) {
        ::V source_vertex = boost::vertex(std::floor(R::runif(0ul, n_vertices)), g);
#ifndef NDEBUG
        Rcpp::Rcerr << "Randomly drew vertex " << source_vertex << ". " << std::endl;
#endif
        ::Trail trail;
        trail.reserve(n_strides_to_take);
        {
            unsigned long source_vertex_outdegree = boost::out_degree(source_vertex, g);
            for (unsigned long stride = 0ul; (stride < n_strides_to_take) && (source_vertex_outdegree > 0ul); stride++, source_vertex_outdegree = boost::out_degree(source_vertex, g)) {
                /*
                To draw a pseudorandom number from an uniform distribution,
                and to make sure the PRNG is controlled through
                base::set.seed in R, the only option is an Rcpp runif
                function. These generate real numbers, however, that must be
                rounded in some way. To make sure each integer in the
                interval [0, n] has approximately equal probability of being
                drawn, do not round but floor with expanded interval of
                [0.0, n + 1.0]. The out degree does not equal the maximum
                arc index, as arcs have a zero-based index. This
                circumstance is exploited to expand the interval without
                performing any extra arithmetic. Because the source vertex
                outdegree can differ between strides, efficiency
                optimization is not as straightforward using a vectorized
                runif() to limit the number of calls to a runif-like
                function. For now the scalar version is used.
                */
                const unsigned long pseudorandom_outgoing_arc_index = source_vertex_outdegree == 1ul ? 0ul : std::floor(R::runif(0ul, source_vertex_outdegree));
                // TODO: Remove auto, add const.
                auto outgoing_arcs = boost::out_edges(source_vertex, g);
                // TODO: why outgoing_arcs.first?
                /* Randomly select out arc. */
                std::advance(outgoing_arcs.first, pseudorandom_outgoing_arc_index);
                // TODO: problem, penalizes selection of last vertex?
                // auto random_outgoing_arc = *(outgoing_arcs).first;
                const auto random_outgoing_arc = *(outgoing_arcs).first;
                trail.push_back(random_outgoing_arc);
#ifndef NDEBUG
                const V target_vertex = boost::target(random_outgoing_arc, g);
                Rcpp::Rcerr << "+> Randomly went down arc (" << source_vertex << "," << target_vertex << ") of " << source_vertex_outdegree << " outgoing arcs. " << std::endl;;
                source_vertex = target_vertex;
#else
                source_vertex = boost::target(random_outgoing_arc, g);
#endif
            }
#ifndef NDEBUG
            if (source_vertex_outdegree == 0ul) {
                /* Trail terminated before having taken n_strides_to_take strides. */
                Rcpp::Rcerr << "-> Trail terminated at vertex " << source_vertex << std::endl;;
            }
#endif
        }
        // TODO: initialize trail lazily.
        if (!trail.empty()) {
            trail.shrink_to_fit();
            trails.push_back(trail);

#ifndef NDEBUG
            for (const ::Trail & trail : trails) {
                for (const ::ArcDescriptor & stride : trail)
                    Rcpp::Rcout << "->" << boost::get(&Arc::label, g, stride);
                Rcpp::Rcout << " (" << trail.size() << " strides). " << std::endl;
            }
#endif
        }
    }

    return(trails);
}

::Trails accumulate_ngram_trails(const ::Trails & trails, const Rcpp::NumericMatrix & arcs_scores_dmat, const unsigned long N, const unsigned long expected_n_strides, const bool is_all_ngrams_up_to_n, const ::LinguisticNetwork & g) {
    ::Trails ngram_trails;
    {
        /* Preallocate an expected maximum of the number of ngram trails. */
        unsigned long expected_ngram_count_per_trail = N * (expected_n_strides + 1ul);
        for (unsigned long ngram_order = 1ul; ngram_order <= N; ngram_order++) {
            expected_ngram_count_per_trail -= ngram_order;
        }

        ngram_trails.reserve(trails.size() * expected_ngram_count_per_trail);
#ifndef NDEBUG
        Rcpp::Rcerr << "Expecting at most " << trails.size() * expected_ngram_count_per_trail << " n-grams of trails. " << std::endl;
#endif
    }
    for (const ::Trail & trail : trails) {
        const unsigned long trail_size = trail.size();
        if (trail_size <= 0ul) {
            Rcpp::stop("trail_size <= 0ul ");
        }
        // TODO:
        Rcpp::Rcerr << "trail size: " << trail_size << std::endl;
        for (unsigned long window_start = 0ul; window_start <= trail_size; window_start++) {
            // TODO:
            Rcpp::Rcerr << "    window_start: " << window_start << std::endl;
            for (unsigned long n = is_all_ngrams_up_to_n ? 1ul : N;
                    (n <= N) && ((window_start + n) <= trail_size); n++) {
                const ::Trail ngram_trail(trail.begin() + window_start, trail.begin() + window_start + n);
                assert(!ngram_trail.empty());
#ifndef NDEBUG
                for (const ::ArcDescriptor & stride : ngram_trail)
                    Rcpp::Rcerr << "=>" << boost::get(&Arc::label, g, stride);
                Rcpp::Rcerr << std::endl;
#endif
                ngram_trails.push_back(ngram_trail);
            }
        }
    }


    if (!ngram_trails.empty()) {
        ngram_trails.shrink_to_fit();
        Rcpp::Rcout << "Collected " << ngram_trails.size() << " n-grams of trails. " << std::endl;
    } else {
        Rcpp::Rcerr << "Could not find any n-gram in any trail. " << std::endl;
    }

    return(ngram_trails);
}

// [[Rcpp::export]]
void enumerate_random_trails_on_linguistic_network(const Rcpp::DataFrame & arcs_list_df, const Rcpp::NumericMatrix & arcs_scores_dmat, const unsigned long n_strides_to_take, const unsigned long N, const bool is_all_ngrams_up_to_n, const unsigned long n_iterations, const std::string & GraphViz_file_path, const std::string & SRILM_counts_file_path) {
    const ::LinguisticNetwork g = create_linguistic_network(arcs_list_df, arcs_scores_dmat);
#ifndef NDEBUG
    const unsigned long n_vertices = boost::num_vertices(g);
    const unsigned long n_arcs = boost::num_edges(g);
    Rcpp::Rcout << "Created linguistic network of " << n_vertices << " vertices and " << n_arcs << " arcs. " <<
          std::endl;;
#endif
    write_GraphViz_file(GraphViz_file_path, g);

    ::Trails trails = accumulate_random_trails(n_iterations, n_strides_to_take, N, g);
    if (!trails.empty()) {
        Rcpp::Rcout << "Collected " << trails.size() << " trails. " << std::endl;
        ::Trails ngram_trails = accumulate_ngram_trails(std::move(trails), arcs_scores_dmat, N, n_strides_to_take, is_all_ngrams_up_to_n, g);
        /* Clear trails now that N-gram trails were obtained. A poor man's way to very poorly mimic the memory efficiency of the consumer-producer pattern. */
        trails.clear();

        if (!ngram_trails.empty()) {
            const ::Sequence_weight_map sequence_weight_map = sequence_weight_map_of_ngram_trails(g, ngram_trails);
            write_SRILM_counts_file(SRILM_counts_file_path, sequence_weight_map);
        }
    } else {
        Rcpp::Rcout << "Collected no trails. " << std::endl;
        // TODO: Replace. Some trail must be possible?
        const ::Sequence_weight_map sequence_weight_map {{"DUMMY", 0.0}};

        write_SRILM_counts_file(SRILM_counts_file_path, sequence_weight_map);
    }
#ifndef NDEBUG
    Rcpp::Rcerr << "Done walking randomly. " << std::endl;;
#endif
}