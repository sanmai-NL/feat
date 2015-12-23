#include <cmath>
#include <fstream>
#include <iostream>
#include <limits>
#include <unordered_map>

#include <Rcpp.h>

// [[Rcpp::depends(BH)]]
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>
#include <boost/graph/random.hpp>

/* Assumes logarithmic weights. */
typedef double Weight;
typedef std::string Feature;
const Weight NULL_WEIGHT = 0.0;
typedef std::unordered_map<Feature, Weight> FeaturesMap;
typedef std::pair<const Feature, const Weight> FeatureItem;
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
typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, Vertex, Arc> LinguisticNetwork;
typedef unsigned long V;
typedef LinguisticNetwork::edge_descriptor ArcDescriptor;
typedef boost::graph_traits<::LinguisticNetwork>::out_edge_iterator OutEdgeIterator;
typedef std::vector<::ArcDescriptor> Trail;
typedef std::vector<::Trail> Trails;


template<class ArcLabel, class Weight>
class edge_writer {
public:
    edge_writer(ArcLabel label, Weight weight) : arc_label(label), arc_weight(weight) { }

    template<class Arc>
    void operator()(std::ostream & out, const Arc & edge_index) const {
        /* Undo logarithmic transformation, raise e to arc weight. */
        const double fractional_count = std::exp(arc_weight[edge_index]);
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

void write_SRILM_counts_file(const std::string & SRILM_counts_file_path, const ::FeaturesMap & FeaturesMap) {
  if (!SRILM_counts_file_path.empty()) {
        std::ofstream ofstream;
        ofstream.open(SRILM_counts_file_path, std::ofstream::out);
        for (const FeatureItem & feature : FeaturesMap) {
            /* Undo logarithmic transformation, raise e to arc weight. */
            const double fractional_count = std::exp(feature.second);
            ofstream << feature.first << '\t' << std::scientific << std::setprecision(std::numeric_limits<double>::max_digits10) << fractional_count << std::endl;
        }
        ofstream.close();
        if (!ofstream) {
            Rcpp::stop("Could not write SRILM counts file to '" + SRILM_counts_file_path + "'.");
        }
#ifndef NDEBUG
        Rcpp::Rcerr << "SRILM counts file written to '" << SRILM_counts_file_path << "'." << std::endl;;
#endif
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
             Rcpp::stop("Could not write GraphViz file to '" + GraphViz_file_path + "'.");
        }
#ifndef NDEBUG
        Rcpp::Rcerr << "GraphViz file written to '" << GraphViz_file_path << "'." << std::endl;;
#endif
    }
}

::LinguisticNetwork create_linguistic_network(const Rcpp::DataFrame & arcs_df) {
    const Rcpp::IntegerVector parents = arcs_df[0];
    const Rcpp::IntegerVector children = arcs_df[1];
    const Rcpp::CharacterVector arc_labels = arcs_df.names();
    const int n_arcs = arcs_df.nrows();
    const long n_arc_labels_plus_2 = arcs_df.size();
    ::LinguisticNetwork g;
    for (long arc_label_index = 2l; arc_label_index < n_arc_labels_plus_2; arc_label_index++) {
        const Rcpp::NumericVector arc_label_scores = arcs_df[arc_label_index];
        for (long arc_index = 0l; arc_index < n_arcs; arc_index++) {
            const ::Weight weight = arc_label_scores[arc_index];
            // TODO: What if no arcs have null weight?
            // TODO: raise exception if parent_id or child_id < 0
            // TODO: perhaps use std::isgreaterequal etc.
            if (weight != NULL_WEIGHT) {
                const unsigned int parent_id = static_cast<unsigned int>(parents[arc_index]);
                const unsigned int child_id = static_cast<unsigned int>(children[arc_index]);
                std::pair<const ArcDescriptor, const bool> added_arc = boost::add_edge(parent_id, child_id, g);
                const std::string arc_label = Rcpp::as<std::string>(arc_labels[arc_label_index]);
                g[added_arc.first].label = arc_label;
                g[added_arc.first].weight = weight;
#ifndef NDEBUG
                Rcpp::Rcout << ":" << arc_label << ":(" << parent_id << "->" << child_id << "@" << weight << "):" << added_arc.second << ":";
#endif
                /* Do not accept skipping of parallel arcs. */
                assert(added_arc.second);
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

::FeaturesMap features_map_of_ngram_trails(const ::LinguisticNetwork & g, const ::Trails & trails) {
    ::FeaturesMap FeaturesMap;
    for (const ::Trail & trail : trails) {
        Feature feature = "";
        ::Weight weight = NULL_WEIGHT;
        for (const ::ArcDescriptor & stride : trail) {
            feature.append(boost::get(&Arc::label, g, stride));
            feature.append(" ");
            weight += static_cast<::Weight>(boost::get(&Arc::weight, g, stride));
        }
        if (!feature.empty()) {
            /* If FeaturesMap has no key /feature/, the default trivial constructor is called for the map's value type (::Weight), meaning it will be initialized as zero. Adding, because the weights are assumed to be logarithmic. */
            FeaturesMap[feature] += weight;
        } else {
            Rcpp::Rcerr << "Empty feature, so empty trail. " << std::endl;
            Rcpp::stop("Empty feature, so empty trail. ");
        }
    }

    if (FeaturesMap.empty()) {
        Rcpp::stop("Failed to fill feature-weight map. ");
    }

    return(FeaturesMap);
}

::Trails accumulate_random_trails(unsigned long n_iterations, const unsigned long n_strides_to_take, unsigned long N, const ::LinguisticNetwork & g) {
    ::Trails trails;
    trails.reserve(n_iterations);
    const unsigned long n_vertices = boost::num_vertices(g);

    for (unsigned long trail_iteration = 0ul; trail_iteration < n_iterations; trail_iteration++) {
        ::V source_vertex = boost::vertex(static_cast<::V>(std::floor(R::runif(0ul, n_vertices))), g);
#ifndef NDEBUG
        Rcpp::Rcerr << "Randomly drew vertex " << source_vertex << ". " << std::endl;
#endif
        ::Trail trail;
        trail.reserve(n_strides_to_take);
        {
            unsigned long source_vertex_outdegree = boost::out_degree(source_vertex, g);
            for (unsigned long stride = 0ul;
                (stride < n_strides_to_take) && (source_vertex_outdegree > 0ul);
                stride++, source_vertex_outdegree = boost::out_degree(source_vertex, g)) {
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
                const unsigned long pseudorandom_outgoing_arc_index = source_vertex_outdegree == 1ul ? 0ul : static_cast<unsigned long>(std::floor(R::runif(0ul, source_vertex_outdegree)));
                std::pair<OutEdgeIterator, OutEdgeIterator> outgoing_arcs = boost::out_edges(source_vertex, g);
                /* Randomly select out arc. */
                std::advance(outgoing_arcs.first, pseudorandom_outgoing_arc_index);
                // TODO: problem, penalizes selection of last vertex?
                const ::ArcDescriptor random_outgoing_arc = *(outgoing_arcs).first;
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
            // trail.shrink_to_fit();
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

::Trails accumulate_ngram_trails(const ::Trails & trails, const unsigned long N, const unsigned long expected_ngram_count_per_trail, const bool is_all_ngrams_up_to_n, const ::LinguisticNetwork & g) {
    ::Trails ngram_trails;
    {
        ngram_trails.reserve(trails.size() * expected_ngram_count_per_trail);
    }
    for (const ::Trail & trail : trails) {
        const unsigned long trail_size = trail.size();
        if (trail_size <= 0ul) {
            Rcpp::stop("trail_size <= 0ul ");
        }
#ifndef NDEBUG
        Rcpp::Rcerr << "Trail size: " << trail_size << std::endl;
#endif
        for (unsigned long window_start = 0ul; window_start <= trail_size; window_start++) {
#ifndef NDEBUG
            Rcpp::Rcerr << "\twindow_start: " << window_start << std::endl;
#endif
            for (unsigned long n = is_all_ngrams_up_to_n ? 1ul : N;
                    (n <= N) && ((window_start + n) <= trail_size); n++) {
                const auto first_token_it = std::next(trail.begin(), static_cast<signed long>(window_start));
                const auto last_token_it = std::next(first_token_it, static_cast<signed long>(n));
                const ::Trail ngram_trail(first_token_it, last_token_it);
                assert(!ngram_trail.empty());
#ifndef NDEBUG
                for (const ::ArcDescriptor & arc : ngram_trail)
                    Rcpp::Rcerr << "=>" << boost::get(&Arc::label, g, arc);
                Rcpp::Rcerr << std::endl;
#endif
                ngram_trails.push_back(ngram_trail);
            }
        }
    }

#ifndef NDEBUG
    if (!ngram_trails.empty()) {
        ngram_trails.shrink_to_fit();
        Rcpp::Rcerr << "Accumulated " << ngram_trails.size() << " n-grams of trails. " << std::endl;
    } else {
        Rcpp::Rcerr << "Could not accumulate any n-gram in any trail. " << std::endl;
    }
#endif

    return(ngram_trails);
}

// [[Rcpp::export]]
void search_random_trails_in_linguistic_network(const Rcpp::DataFrame & arcs_df, const unsigned long n_strides_to_take, const unsigned long N, const bool is_all_ngrams_up_to_n, const unsigned long n_iterations, const std::string & GraphViz_file_path, const std::string & SRILM_counts_file_path) {
    const ::LinguisticNetwork g = create_linguistic_network(arcs_df);
#ifndef NDEBUG
    const unsigned long n_vertices = boost::num_vertices(g);
    const unsigned long n_arcs = boost::num_edges(g);
    Rcpp::Rcout << "Created a linguistic network of " << n_vertices << " vertices and " << n_arcs << " arcs. " <<
          std::endl;;
#endif
    write_GraphViz_file(GraphViz_file_path, g);

    ::Trails trails = accumulate_random_trails(n_iterations, n_strides_to_take, N, g);
    if (!trails.empty()) {
#ifndef NDEBUG
        Rcpp::Rcout << "Accumulated " << trails.size() << " trails. " << std::endl;
#endif
        /* Calculate an expected maximum of the number of n-gram trails for preallocation. */
        const unsigned long expected_ngram_count_per_trail = [&] {
            unsigned long expected_ngram_count_per_trail = N * (n_strides_to_take + 1ul);
            for (unsigned long ngram_order = 1ul; ngram_order <= N; ngram_order++)
                expected_ngram_count_per_trail -= ngram_order;
            return(expected_ngram_count_per_trail);
        }();

        #ifndef NDEBUG
        Rcpp::Rcerr << "Expecting at most " << trails.size() * expected_ngram_count_per_trail << " n-grams in trails. " << std::endl;
        #endif

        ::Trails ngram_trails = accumulate_ngram_trails(std::move(trails), N, expected_ngram_count_per_trail, is_all_ngrams_up_to_n, g);
        /* Clear trails now that N-gram trails were obtained. A poor man's way to very poorly mimic the memory efficiency of the consumer-producer pattern. */
        trails.clear();

        if (!ngram_trails.empty()) {
            const ::FeaturesMap FeaturesMap = features_map_of_ngram_trails(g, ngram_trails);
            write_SRILM_counts_file(SRILM_counts_file_path, FeaturesMap);
        }
    } else {
#ifndef NDEBUG
        Rcpp::Rcout << "Accumulated zero trails. " << std::endl;
#endif
        // TODO: Replace. Some trail must be possible?
        const ::FeaturesMap FeaturesMap {{"DUMMY", NULL_WEIGHT}};

        write_SRILM_counts_file(SRILM_counts_file_path, FeaturesMap);
    }
#ifndef NDEBUG
    Rcpp::Rcerr << "Completed search in a linguistic network. " << std::endl;;
#endif
}