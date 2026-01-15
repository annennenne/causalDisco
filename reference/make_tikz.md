# Generate Latex tikz code for plotting a temporal DAG, PDAG or PAG.

Generate Latex tikz code for plotting a temporal DAG, PDAG or PAG.

## Usage

``` r
make_tikz(
  model,
  x_jit = 2,
  y_jit = 2,
  mark_periods = TRUE,
  period_gap = 4,
  annotate_edges = NULL,
  add_axis = TRUE,
  var_labels = NULL,
  period_labels = NULL,
  annotation_labels = NULL,
  clipboard = TRUE,
  raw_out = FALSE,
  color_annotate = NULL,
  bend_edges = FALSE
)
```

## Arguments

- model:

  `tpdag`, `tskeleton`, `tpag`, or `tamat` object to plot.

- x_jit:

  How much should nodes within a period be jittered horizontally.

- y_jit:

  Vertical distance between nodes within a period.

- mark_periods:

  If `TRUE`, gray boxes are drawn behind each period.

- period_gap:

  Horizontal gap between different periods.

- annotate_edges:

  If `TRUE`, add a text annotation to edges. If `annotation_labels` are
  supplied, these labels will be used. Otherwise, the value in the
  inputted adjacency matrix corresponding to the edge will be used.
  Cannot be used for `tpag` input objects (or `ag` amat types).

- add_axis:

  If `TRUE`, a horizontal axis with period labels are added.

- var_labels:

  Optional labels for nodes (variables). Should be given as a named
  list, where the name is the variable name, and the entry is the label,
  e.g. `list(vname = "Label for vname")`.

- period_labels:

  Optional labels for periods. Should be given as a named list, where
  the name is the period name (as stored in the `tamat`), and the entry
  is the label, e.g. `list(periodname = "Label for period")`.

- annotation_labels:

  Optional labels for edge annotations. Only used if
  `annotate_edges = TRUE`. Should be given as a named list, where the
  name is the edge annotation (as stored in the `tamat`), and the entry
  is the label, e.g. `list(h = "High")`.

- clipboard:

  If `TRUE`, the tikz code is not printed, but instead copied to the
  clipboard, so it can easily be pasted into a Latex document.

- raw_out:

  If `TRUE`, the tikz code is only returned as a character vector.

- color_annotate:

  Named list of colors to use to mark edge annotations instead of
  labels. This overrules `annotate_edges` and both are not available at
  the same time. The list should be given with annotations as names and
  colors as entries, e.g. `list(h = "blue")`. Cannot be used for `tpag`
  input objects (or `ag` amat types).

- bend_edges:

  If `TRUE`, all edges are bend 10 degrees to the right, thereby
  avoiding having edges exactly on top of each other.

## Value

Silently returns a character vector with lines of tikz code. The
function furthermore has a side-effect. If `clipboard = TRUE`, the
side-effect is that the tikz code is also copied to the clipboard. If
`clipboard = FALSE`, the tikz code is instead printed in the console.

## Details

Note that it is necessary to read in relevant tikz libraries in the
Latex preamble. The relevant lines of code are (depending a bit on
parameter settings):  
`\usepackage{tikz}`  
`\usetikzlibrary{arrows.meta,arrows,shapes,decorations,automata,backgrounds,petri}`  
`\usepackage{pgfplots}`

## Examples

``` r
# Make tikz figure code from tpdag, print code to screen
data(tpc_example)
kn <- knowledge(
  tpc_example,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    oldage ~ tidyselect::starts_with("oldage")
  )
)

tpdag_example <- tpc_run(tpc_example,
  kn,
  alpha = 0.01,
  test = cor_test,
  output = "tpdag"
)
make_tikz(tpdag_example, clipboard = FALSE)
#> %TIKZ FIG MADE BY CAUSALDISCO
#> \begin{tikzpicture}
#> [every node/.style={font=\small, align = center}, every edge/.append style={nodes={font=\itshape\scriptsize}}]
#> \node (1) at (2,1) {x2};
#> \node at (1,-0.5) {child};
#> \node (2) at (0,3) {x1};
#> \node (3) at (8,1) {x4};
#> \node at (7,-0.5) {youth};
#> \node (4) at (6,3) {x3};
#> \node (5) at (14,1) {x6};
#> \node at (13,-0.5) {oldage};
#> \node (6) at (12,3) {x5};
#> \draw [->] (1) edge (3);
#> \draw [->] (1) edge (6);
#> \draw [->] (3) edge (5);
#> \draw [->] (4) edge (6);
#> \draw [->] (6) edge (5);
#> \draw [-] (2) edge (1);
#> \draw [-] (-1,0) edge (15,0);
#> \begin{pgfonlayer}{background}
#> \filldraw [join=round,black!10]
#> (0,0) rectangle (2,4)
#> (6,0) rectangle (8,4)
#> (12,0) rectangle (14,4)
#> ; \end{pgfonlayer}
#> \end{tikzpicture}

# Make tikz figure code from tamat, copy code to clipboard
dag_example <- sim_dag(5)
rownames(dag_example) <- colnames(dag_example) <- c(
  "child_x", "child_y",
  "child_z", "adult_x",
  "adult_y"
)
output_tamat <- causalDisco:::tamat(dag_example, order = c("child", "adult"))
if (FALSE) { # \dontrun{
make_tikz(output_tamat)
} # }
```
