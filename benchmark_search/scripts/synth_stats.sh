ls benchmark_search/synth_results/try_fail_catch/v6/*.csv | xargs -n1 awk '
$1 == "examples" { examples = $2 }
$1 == "syntax_nodes_first_commit" { snodes = $2 }
$1 == "elapsed_initial_prepare" { prep = $2 }
$1 == "elapsed_compute_examples_post" { search = $2 }
$1 == "nodes" { nodes = $2 }
$1 == "tops" { tops = $2 }
$1 == "elapsed_group_reduce" { elapsed = $2 }


END {
    printf "%s %s %s %.4f %.4f %s %s %.4f\n", FILENAME, snodes, examples, prep, search-prep, nodes, tops, elapsed-search
}
' \
| cut -d' ' -f2,3,4,5,6,7,8 \
| python3 -c '
import sys, numpy as np
from scipy.stats import spearmanr
np.set_printoptions(threshold=np.inf,linewidth=200,precision=3)
data = np.loadtxt(sys.stdin)
print("snodes, examples, prep, search-prep, nodes, tops, gen")
corr, p = spearmanr(data)
print(corr)
print(p)
'
