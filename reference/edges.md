# Retrieve Edges

This function retrieves the edges from a `caugi` object as a tibble.

## Usage

``` r
edges(cg)
```

## Arguments

- cg:

  A `caugi` object.

## Value

A tibble containing the edges.

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
edges(disco_cd_tges$caugi)
#> # A tibble: 6 × 3
#>   from      edge  to       
#>   <chr>     <chr> <chr>    
#> 1 child_x1  ---   child_x2 
#> 2 child_x2  -->   oldage_x5
#> 3 child_x2  -->   youth_x4 
#> 4 oldage_x5 -->   oldage_x6
#> 5 youth_x3  -->   oldage_x5
#> 6 youth_x4  -->   oldage_x6
```
