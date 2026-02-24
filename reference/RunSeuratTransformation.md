# Run transformations on a Seurat object

Run transformations on a Seurat object

## Usage

``` r
RunSeuratTransformation(
  object,
  use_sct = FALSE,
  SCTransformArgs = list(),
  NormalizeDataArgs = list(),
  FindVariableFeaturesArgs = list(),
  ScaleDataArgs = list(),
  RunPCAArgs = list(),
  log = NULL,
  from_ccs = FALSE,
  cache = NULL
)
```

## Arguments

- object:

  Seurat object

- use_sct:

  Whether to use
  [Seurat::SCTransform](https://satijalab.org/seurat/reference/SCTransform.html)

- SCTransformArgs:

  Arguments to pass to
  [Seurat::SCTransform](https://satijalab.org/seurat/reference/SCTransform.html)

- NormalizeDataArgs:

  Arguments to pass to
  [Seurat::NormalizeData](https://satijalab.org/seurat/reference/NormalizeData.html)

- FindVariableFeaturesArgs:

  Arguments to pass to
  [Seurat::FindVariableFeatures](https://satijalab.org/seurat/reference/FindVariableFeatures.html)

- ScaleDataArgs:

  Arguments to pass to
  [Seurat::ScaleData](https://satijalab.org/seurat/reference/ScaleData.html)

- RunPCAArgs:

  Arguments to pass to
  [Seurat::RunPCA](https://satijalab.org/seurat/reference/RunPCA.html)

- log:

  Logger

- from_ccs:

  Whether this is called from
  [RunSeuratCellCycleScoring](https://pwwang.github.io/biopipen.utils.R/reference/RunSeuratCellCycleScoring.md).

- cache:

  Directory to cache the results. Set to `FALSE` to disable caching

## Value

The transformed Seurat object

## Examples

``` r
# \donttest{
RunSeuratTransformation(SeuratObject::pbmc_small)
#> INFO    [2026-02-24 23:15:56] Performing data transformation and scaling ...
#> INFO    [2026-02-24 23:15:56] - Running NormalizeData ...
#> INFO    [2026-02-24 23:15:57] - Running FindVariableFeatures ...
#> INFO    [2026-02-24 23:15:58] - Running ScaleData ...
#> Centering and scaling data matrix
#> INFO    [2026-02-24 23:15:59] - Running RunPCA (npcs=50) ...
#> Warning: You're computing too large a percentage of total singular values, use a standard svd instead.
#> PC_ 1 
#> Positive:  TYMP, LST1, AIF1, CST3, IFITM3, LYZ, FCGRT, GRN, BID, SERPINA1 
#>     S100A11, IFI30, CFP, S100A8, S100A9, HLA-DPA1, HLA-DPB1, HLA-DRB1, FCER1G, HCK 
#>     CTSS, FCN1, CFD, RNF130, TYROBP, HLA-DRB5, LINC00936, SMCO4, HLA-DRA, LGALS1 
#> Negative:  CCL5, LCK, CTSW, GZMA, GZMM, CST7, CD7, PRF1, CD3D, RARRES3 
#>     IL32, CD3E, CD247, FGFBP2, GNLY, LAMP1, GP9, NKG7, GZMH, NGFRAP1 
#>     TUBB1, GZMB, PF4, GNG11, KLRG1, HIST1H2AC, XBP1, CLU, PGRMC1, GYPC 
#> PC_ 2 
#> Positive:  GNG11, CLU, SDPR, SPARC, GP9, PF4, HIST1H2AC, PPBP, CD9, ITGA2B 
#>     CA2, NRGN, TUBB1, TREML1, TMEM40, NGFRAP1, MYL9, PTCRA, PGRMC1, NCOA4 
#>     RUFY1, ODC1, TSC22D1, ACRBP, GPX1, TPM4, PARVB, FERMT3, TALDO1, SAT1 
#> Negative:  IFITM2, NKG7, CST7, CTSW, GZMA, GZMM, LCK, CD7, GNLY, FGFBP2 
#>     SSR2, XBP1, GZMB, IL32, PRF1, CD247, EIF4A2, HNRNPA3, SRSF7, SPON2 
#>     CD3D, GZMH, HNRNPF, KLRD1, CCL4, LAMP1, YWHAB, GIMAP1, EIF3G, GYPC 
#> PC_ 3 
#> Positive:  MS4A1, CD79A, TCL1A, CD79B, LTB, FCER2, HLA-DQB1, HVCN1, PPAPDC1B, CYB561A3 
#>     CD180, HLA-DMB, FCRLA, LINC00926, KIAA0125, RP11-693J15.5, CXCR4, CD19, LY86, CD200 
#>     IGLL5, NCF1, SP100, HLA-DRB5, SNHG7, HLA-DQA1, NT5C, HLA-DRB1, HLA-DRA, BANK1 
#> Negative:  LGALS1, FCER1G, TYROBP, GZMB, PRF1, GNLY, CCL4, CCL5, FCGR3A, AKR1C3 
#>     GZMA, FGFBP2, KLRD1, PSAP, GZMH, TTC38, IGFBP7, NKG7, C12orf75, CST7 
#>     SPON2, RHOC, GZMM, CD247, PCMT1, LAMP1, CTSW, PTGDR, SAT1, GSTP1 
#> PC_ 4 
#> Positive:  FGFBP2, AKR1C3, GZMB, CCL4, MS4A1, CLIC3, HLA-DRB1, TTC38, CD79A, NT5C 
#>     TCL1A, IL2RB, GNLY, KLRD1, GZMH, HLA-DMB, PRF1, CTSW, HLA-DPA1, NKG7 
#>     HLA-DPB1, ARHGDIA, HLA-DQB1, IGFBP7, CST7, C12orf75, XCL2, LY86, CD79B, PTGDR 
#> Negative:  CD3D, MAL, SAFB2, IL7R, MPHOSPH6, THYN1, ASNSD1, TAGAP, LDHB, CD2 
#>     NOSIP, TMEM204, DNAJB1, CCR7, CCDC104, ACAP1, TAF7, TMUB1, LTB, CD3E 
#>     TMEM123, PIK3IP1, IL32, ACSM3, SRSF7, EPC1, EIF4A2, GYPC, TNFAIP8, BLOC1S4 
#> PC_ 5 
#> Positive:  FCGR3A, LILRA3, C5AR1, RP11-290F20.3, CD79B, TNFRSF1B, SMCO4, CFD, CD68, CTSS 
#>     ADAR, VSTM1, EPC1, HVCN1, RHOC, FCN1, SERPINA1, MPHOSPH6, MS4A7, NCF1 
#>     CLIC3, RBP7, WARS, FAM96A, STX10, FPR1, SCO2, AKR1C3, CD200, BLOC1S4 
#> Negative:  FCER1A, CLEC10A, RGS1, CD1C, LGALS2, IL1B, HLA-DMA, HLA-DQA2, FUOM, ZNF330 
#>     HLA-DQA1, MMADHC, RNF130, MS4A6A, POP7, HNRNPH1, RPL7L1, GPX1, ZFP36L1, LINC00936 
#>     HLA-DMB, LY86, SRSF7, HSP90AA1, GSTP1, SSR2, SATB1, GZMK, CCL5, HLA-DQB1 
#> Warning: Number of dimensions changing from 19 to 50
#> # A Seurat-tibble abstraction: 80 × 15
#> # Features=230 | Cells=80 | Active assay=RNA | Assays=RNA
#>    .cell orig.ident nCount_RNA nFeature_RNA RNA_snn_res.0.8 letter.idents groups
#>    <chr> <fct>           <dbl>        <int> <fct>           <fct>         <chr> 
#>  1 ATGC… SeuratPro…         70           47 0               A             g2    
#>  2 CATG… SeuratPro…         85           52 0               A             g1    
#>  3 GAAC… SeuratPro…         87           50 1               B             g2    
#>  4 TGAC… SeuratPro…        127           56 0               A             g2    
#>  5 AGTC… SeuratPro…        173           53 0               A             g2    
#>  6 TCTG… SeuratPro…         70           48 0               A             g1    
#>  7 TGGT… SeuratPro…         64           36 0               A             g1    
#>  8 GCAG… SeuratPro…         72           45 0               A             g1    
#>  9 GATA… SeuratPro…         52           36 0               A             g1    
#> 10 AATG… SeuratPro…        100           41 0               A             g1    
#> # ℹ 70 more rows
#> # ℹ 8 more variables: RNA_snn_res.1 <fct>, PC_1 <dbl>, PC_2 <dbl>, PC_3 <dbl>,
#> #   PC_4 <dbl>, PC_5 <dbl>, tSNE_1 <dbl>, tSNE_2 <dbl>
# }
```
