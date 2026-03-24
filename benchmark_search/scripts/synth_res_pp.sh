ls benchmark_search/synth_results/try_fail_catch/v6/*.csv | xargs -n1 awk '
$1 == "user" { user = $2 }
$1 == "name" { name = $2 }
$1 == "examples" { examples = $2 }
$1 == "syntax_nodes_first_commit" { snodes = $2 }
$1 == "elapsed_initial_prepare" { prep = $2 }
$1 == "elapsed_compute_examples_post" { search = $2 }
$1 == "wcc" { wcc = $2 }
$1 == "nodes" { nodes = $2 }
$1 == "tops" { tops = $2 }
$1 == "elapsed_group_reduce" { elapsed = $2 }


END {
    printf "\\href{%s/%s}{%s} & %s & %s & %.4f & %.4f & %s & %s & %s & %.4f \\\\\n", user, name, name, snodes, examples, prep, search-prep, nodes, wcc, tops, elapsed-search
}
' | sort -t'&' -k6nr
