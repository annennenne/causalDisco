# Retrieve Nodes

This function retrieves the nodes from a `caugi` object as a tibble.

## Usage

``` r
nodes(cg)
```

## Arguments

- cg:

  A `caugi` object.

## Value

A tibble containing the nodes.

## Examples

``` r
data(tpc_example)
cd_tges <- tges(engine = "causalDisco", score = "tbic")
kn <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)
disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)
nodes(disco_cd_tges$caugi)
#> # A tibble: 6 × 1
#>   name     
#>   <chr>    
#> 1 child_x2 
#> 2 child_x1 
#> 3 youth_x4 
#> 4 youth_x3 
#> 5 oldage_x6
#> 6 oldage_x5
```
