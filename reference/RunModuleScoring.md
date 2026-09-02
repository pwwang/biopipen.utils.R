# Run module scoring for expression programs on a Seurat object

This function wraps the module scoring logic of the
`ModuleScoreCalculator` process in biopipen, and supports multiple
scoring tools benchmarked in the scPS paper (see references): Seurat's
[`Seurat::AddModuleScore()`](https://satijalab.org/seurat/reference/AddModuleScore.html)
/
[`Seurat::CellCycleScoring()`](https://satijalab.org/seurat/reference/CellCycleScoring.html),
UCell, AUCell, ssGSEA, JASMINE, SCSE and scPS.

## Usage

``` r
RunModuleScoring(
  object,
  modules,
  method = "seurat",
  nbin = 24,
  ctrl = 100,
  k = FALSE,
  assay = NULL,
  seed = 8525,
  search = FALSE,
  ...,
  log = NULL
)
```

## Arguments

- object:

  Seurat object

- modules:

  A named list of modules. The keys are the names of the modules and the
  values are lists of parameters for the modules, inherited from the
  function arguments (see details). At least `features` should be
  provided for each module.

- method:

  The default method to calculate the module scores.

  - seurat:
    [`Seurat::AddModuleScore()`](https://satijalab.org/seurat/reference/AddModuleScore.html)

  - ucell:
    [`UCell::AddModuleScore_UCell()`](https://rdrr.io/pkg/UCell/man/AddModuleScore_UCell.html)

  - aucell:
    [`AUCell::AUCell_buildRankings()`](https://rdrr.io/pkg/AUCell/man/AUCell_buildRankings.html)
    and
    [`AUCell::AUCell_calcAUC()`](https://rdrr.io/pkg/AUCell/man/AUCell_calcAUC.html)

  - ssgsea: [`GSVA::gsva()`](https://rdrr.io/pkg/GSVA/man/gsva.html)
    with `method = "ssgsea"`

  - jasmine: JASMINE scoring (see references)

  - scse: SCSE scoring (see references)

  - scps: scPS scoring (see references), using the bundled scPS
    implementation ported from the scPS script
    (<https://github.com/Thakar-Lab/scPS>).

- nbin, ctrl, k, assay, seed, search:

  The default parameters for
  [`Seurat::AddModuleScore()`](https://satijalab.org/seurat/reference/AddModuleScore.html).
  See
  [`Seurat::AddModuleScore()`](https://satijalab.org/seurat/reference/AddModuleScore.html)
  for details.

- ...:

  Additional default parameters for the tools, e.g. `maxRank`, `w_neg`
  and `slot` for UCell, `aucMaxRank` and `plotStats` for AUCell, `kcdf`
  and `verbose` for ssGSEA, `layer` for the matrix-based tools. These
  can also be set per module in `modules`.

- log:

  Logger

## Value

The Seurat object with the module scores added to the metadata.

## Details

The features of a module can be given as:

- A comma-separated string (e.g. `"HAVCR2,ENTPD1,LAYN,LAG3"`), treated
  as a single expression program.

- A character vector, also a single expression program.

- A list of character vectors, i.e. multiple programs. When the list is
  fully named, the names are used as the metadata column names of the
  scores (each program gets its own column, prefixed by the module key),
  e.g.
  `list(Exhaustion = c("HAVCR2", "ENTPD1"), Activation = c("IFNG", "GZMB"))`
  will add `{module key}_Exhaustion` and `{module key}_Activation`
  columns to the metadata. If the list is not named, the columns will be
  named `{module key}1`, `{module key}2`, etc. A single unnamed program
  will be named by the module key. Use one of the reserved keys `_`,
  `-`, `*` or `#` as the module key to skip the prefix.

For cell cycle scoring, use `"cc.genes"` or `"cc.genes.updated.2019"`
(human) or `"cc.genes.mouse"` as the features, or set `kind` to `"cc"`
(with `features` defaulting to `"cc.genes"`). Then
`{module key}_S.Score`, `{module key}_G2M.Score` and
`{module key}_Phase` columns will be added to the metadata (without the
prefix when the module key is one of the reserved no-prefix keys `_`,
`-`, `*` or `#`).

A module can also perform diffusion map (diffusion components) as a
reduction by setting `kind` to `"dm"`, `"diffmap"` or `"diffusion_map"`.
In this case, `features` is the number of components to keep (default
2), the first `features` components will be added to the metadata as
`{module key}_1`, `{module key}_2`, ..., and a reduction named
`{module key}` will be created. The diffusion map is calculated by
[`destiny::DiffusionMap()`](https://rdrr.io/pkg/destiny/man/DiffusionMap-class.html)
(see
<https://bioconductor.org/packages/release/bioc/html/destiny.html>),
neither Seurat nor UCell is used. Other key-value pairs of the module
will be passed to
[`destiny::DiffusionMap()`](https://rdrr.io/pkg/destiny/man/DiffusionMap-class.html).

The parameters (`method`, `nbin`, `ctrl`, `k`, `assay`, `seed`, `search`
and those in `...`) are the defaults inherited by all modules. They can
be overridden per module by putting the same keys in the module dict.
Irrelevant parameters for a tool are ignored, e.g. `nbin` and `ctrl` are
only used by
[`Seurat::AddModuleScore()`](https://satijalab.org/seurat/reference/AddModuleScore.html),
and `maxRank` is only used by UCell.

Note that the scores from different methods are NOT comparable with each
other (only the column names are consistent). Also, UCell imputes
missing genes with 0 while other methods drop them (with a warning from
Seurat). The `slot` parameter of UCell is `slot` (not `layer`), while
other matrix-based tools (AUCell, ssGSEA, JASMINE, SCSE) read the
expression data from the `data` layer (or the layer specified by
`layer`).

## References

Tirosh I, et al. 2016. Dissecting the multicellular ecosystem of
metastatic melanoma by single-cell RNA-seq. *Science* 352(6282):189-196.
doi:10.1126/science.aad0501.
<https://www.science.org/doi/10.1126/science.aad0501> — the `seurat`
method.

Andreatta M, Carmona SJ. 2021. UCell: Robust and scalable single-cell
gene signature scoring. *Comput Struct Biotechnol J* 19:3796-3798.
doi:10.1016/j.csbj.2021.06.043.
<https://doi.org/10.1016/j.csbj.2021.06.043> — the `ucell` method.

Aibar S, et al. 2017. SCENIC: single-cell regulatory network inference
and clustering. *Nat Methods* 14:1083-1086. doi:10.1038/nmeth.4463.
<https://doi.org/10.1038/nmeth.4463> — the `aucell` method.

Barbie DA, et al. 2009. Systematic RNA interference reveals that
oncogenic KRAS-driven cancers require TBK1. *Nature* 462:108-112.
doi:10.1038/nature08460. <https://doi.org/10.1038/nature08460> — the
`ssgsea` method.

Noureen N, et al. 2022. Integrated analysis of telomerase enzymatic
activity unravels an association with cancer stemness and proliferation.
*eLife* 11:e71994. doi:10.7554/eLife.71994.
<https://doi.org/10.7554/eLife.71994> — the `jasmine` method.

Pont F, et al. 2019. Single-cell signature explorer for personalized
transcriptomics studies and drug discovery. *Nucleic Acids Res*
47(19):e90. doi:10.1093/nar/gkz601. <https://doi.org/10.1093/nar/gkz601>
— the `scse` method.

The scPS paper (benchmarking the above methods, the `scps` method):
<https://academic.oup.com/nargab/article/6/3/lqae124/7770961>

## Examples

``` r
# \donttest{
data(pbmc_small, package = "SeuratObject")
obj <- Seurat::NormalizeData(pbmc_small)
# a comma-separated string of features: one score column per module key
obj <- RunModuleScoring(
    obj,
    modules = list(
        Exhaustion = list(features = "MS4A1,CD79A"),
        Prolif = list(features = "GZMB,GNLY")
    ),
    # small object needs small `nbin`/`ctrl` (see the hint on error)
    nbin = 10, ctrl = 5
)
#> INFO    [2026-09-02 20:31:43] Calculating module 'Exhaustion' with method 'seurat' ...
#> INFO    [2026-09-02 20:31:43] Calculating module 'Prolif' with method 'seurat' ...
head(obj@meta.data[, c("Exhaustion", "Prolif")])
#>                Exhaustion     Prolif
#> ATGCCAGAACGACT -1.0627307 -1.4906462
#> CATGGCCTGTGCAT -0.9552306 -0.9552306
#> GAACCTGATGAACC -0.9506189 -0.4753095
#> TGACTGGATTCTCA -0.9444404  1.3136320
#> AGTCAGACTGCACA -0.7641048 -1.3309813
#> TCTGATACACGTGT  0.0000000 -0.9937641

# cell cycle scores with UCell (needs the UCell package)
if (requireNamespace("UCell", quietly = TRUE)) {
    obj <- RunModuleScoring(
        obj,
        modules = list(CellCycle = list(features = "cc.genes.updated.2019")),
        method = "ucell", nbin = 10, ctrl = 5
    )
    head(obj@meta.data[, c("CellCycle_S.Score", "CellCycle_G2M.Score", "CellCycle_Phase")])
}
#> INFO    [2026-09-02 20:31:43] Calculating module 'CellCycle' with method 'ucell' ...
#> Warning: Over half of genes (100%) in specified signatures are missing from data. Check the integrity of your dataset.
#> Warning: The following genes were not found and will be
#>                         imputed to exp=0:
#> * MCM5,PCNA,TYMS,FEN1,MCM7,MCM4,RRM1,UNG,GINS2,MCM6,CDCA7,DTL,PRIM1,UHRF1,CENPU,HELLS,RFC2,POLR1B,NASP,RAD51AP1,GMNN,WDR76,SLBP,CCNE2,UBR7,POLD3,MSH2,ATAD2,RAD51,RRM2,CDC45,CDC6,EXO1,TIPIN,DSCC1,BLM,CASP8AP2,USP1,CLSPN,POLA1,CHAF1B,MRPL36,E2F8,HMGB2,CDK1,NUSAP1,UBE2C,BIRC5,TPX2,TOP2A,NDC80,CKS2,NUF2,CKS1B,MKI67,TMPO,CENPF,TACC3,PIMREG,SMC4,CCNB2,CKAP2L,CKAP2,AURKB,BUB1,KIF11,ANP32E,TUBB4B,GTSE1,KIF20B,HJURP,CDCA3,JPT1,CDC20,TTK,CDC25C,KIF2C,RANGAP1,NCAPD2,DLGAP5,CDCA2,CDCA8,ECT2,KIF23,HMMR,AURKA,PSRC1,ANLN,LBR,CKAP5,CENPE,CTCF,NEK2,G2E3,GAS2L3,CBX5,CENPA
#>                CellCycle_S.Score CellCycle_G2M.Score CellCycle_Phase
#> ATGCCAGAACGACT         0.4573770           0.4657763             G2M
#> CATGGCCTGTGCAT         0.4491803           0.4574290             G2M
#> GAACCTGATGAACC         0.4524590           0.4607679             G2M
#> TGACTGGATTCTCA         0.4426230           0.4507513             G2M
#> AGTCAGACTGCACA         0.4475410           0.4557596             G2M
#> TCTGATACACGTGT         0.4557377           0.4641068             G2M
# }
```
