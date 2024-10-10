
<!-- README.md is generated from README.Rmd. Please edit that file -->

# microCTr

<!-- badges: start -->
<!-- badges: end -->

This packages contains functions to read in and analyze microCT data as
currently performed in the Khosla Lab. Data come from a Scanco vivaCT
40. Sites analyzed include femoral metaphysis, femoral diaphysis, and
lumbar vertebra. Analyses include trabecular bone parameters, cortical
bone parameters, and finite element analysis.

## Installation

You can install the development version of microCTr from
[GitHub](https://github.com/ricompute/microCTr) with:

``` r
# install.packages("pak")
pak::pak("ricompute/microCTr")
```

## Example

Please see the [package website](https://ricompute.github.io/microCTr/)
for details and more examples, including a vignette showing a complete
analysis comparing two genotypes.

``` r
library(microCTr)

key <- read_key_excel(mctr_ex("example-genotype.xlsx"))

trab <- read_trabecular_excel(mctr_ex("example-genotype.xlsx"), key)

F.Met.Trab <- trab |> dplyr::filter(Site == "Met") |> compare_groups()
```

``` r
print_results(F.Met.Trab)
```

<div style="display: flex;">

<div>

**BV/TV**

<table class="kable_wrapper">
<tbody>
<tr>
<td>

| Sex | Genotype |   n |  Mean |   SEM |     P | Sig |
|:----|:---------|----:|------:|------:|------:|:----|
| M   | Cre      |   3 |  6.82 | 0.954 |       |     |
| M   | Cre;fl   |   3 | 11.27 | 2.427 | 0.163 |     |

</td>
</tr>
</tbody>
</table>

**SMI**

<table class="kable_wrapper">
<tbody>
<tr>
<td>

| Sex | Genotype |   n | Mean |   SEM |    P | Sig |
|:----|:---------|----:|-----:|------:|-----:|:----|
| M   | Cre      |   3 | 3.06 | 0.084 |      |     |
| M   | Cre;fl   |   3 | 2.54 | 0.464 | 0.33 |     |

</td>
</tr>
</tbody>
</table>

**Tb.N**

<table class="kable_wrapper">
<tbody>
<tr>
<td>

| Sex | Genotype |   n | Mean |   SEM |     P | Sig |
|:----|:---------|----:|-----:|------:|------:|:----|
| M   | Cre      |   3 | 2.62 | 0.103 |       |     |
| M   | Cre;fl   |   3 | 3.08 | 0.203 | 0.116 |     |

</td>
</tr>
</tbody>
</table>

**Tb.Th**

<table class="kable_wrapper">
<tbody>
<tr>
<td>

| Sex | Genotype |   n |  Mean |   SEM |     P | Sig |
|:----|:---------|----:|------:|------:|------:|:----|
| M   | Cre      |   3 | 0.065 | 0.004 |       |     |
| M   | Cre;fl   |   3 | 0.069 | 0.002 | 0.323 |     |

</td>
</tr>
</tbody>
</table>

**Tb.Sp**

<table class="kable_wrapper">
<tbody>
<tr>
<td>

| Sex | Genotype |   n |  Mean |   SEM |     P | Sig |
|:----|:---------|----:|------:|------:|------:|:----|
| M   | Cre      |   3 | 0.379 | 0.018 |       |     |
| M   | Cre;fl   |   3 | 0.314 | 0.020 | 0.074 |     |

</td>
</tr>
</tbody>
</table>

</div>

<div>

**BV/TV**

<table class="kable_wrapper">
<tbody>
<tr>
<td>

| Sex | Genotype |   n |  Mean |   SEM |     P | Sig |
|:----|:---------|----:|------:|------:|------:|:----|
| F   | Cre      |   3 | 2.690 | 0.930 |       |     |
| F   | Cre;fl   |   3 | 0.873 | 0.405 | 0.148 |     |

</td>
</tr>
</tbody>
</table>

**SMI**

<table class="kable_wrapper">
<tbody>
<tr>
<td>

| Sex | Genotype |   n | Mean |   SEM |     P | Sig |
|:----|:---------|----:|-----:|------:|------:|:----|
| F   | Cre      |   3 | 3.83 | 0.306 |       |     |
| F   | Cre;fl   |   3 | 3.90 | 0.233 | 0.871 |     |

</td>
</tr>
</tbody>
</table>

**Tb.N**

<table class="kable_wrapper">
<tbody>
<tr>
<td>

| Sex | Genotype |   n | Mean |   SEM |     P | Sig |
|:----|:---------|----:|-----:|------:|------:|:----|
| F   | Cre      |   3 | 2.30 | 0.375 |       |     |
| F   | Cre;fl   |   3 | 1.92 | 0.092 | 0.375 |     |

</td>
</tr>
</tbody>
</table>

**Tb.Th**

<table class="kable_wrapper">
<tbody>
<tr>
<td>

| Sex | Genotype |   n |  Mean |   SEM |     P | Sig |
|:----|:---------|----:|------:|------:|------:|:----|
| F   | Cre      |   3 | 0.056 | 0.011 |       |     |
| F   | Cre;fl   |   3 | 0.037 | 0.002 | 0.167 |     |

</td>
</tr>
</tbody>
</table>

**Tb.Sp**

<table class="kable_wrapper">
<tbody>
<tr>
<td>

| Sex | Genotype |   n |  Mean |   SEM |     P | Sig |
|:----|:---------|----:|------:|------:|------:|:----|
| F   | Cre      |   3 | 0.470 | 0.098 |       |     |
| F   | Cre;fl   |   3 | 0.522 | 0.022 | 0.627 |     |

</td>
</tr>
</tbody>
</table>

</div>

</div>
