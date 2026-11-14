.onAttach <- function(libname, pkgname) {
    if (!requireNamespace("SeuratDisk", quietly = TRUE)) {
        return(invisible())
    }

    # SeuratDisk::LoadH5Seurat() will error, which passes slot instead layer to GetAssayData.Seurat() when assay is specified. This is because SeuratDisk::AssembleAssay() calls GetAssayData() with slot argument, which is not expected by SeuratObject::GetAssayData.Seurat()
    monkey_patch("SeuratDisk", "AssembleAssay", .AssembleAssay)
    # SeuratDisk::LoadH5Seurat() will error on R >= 4.5, which passes
    # index$global$images to unlist()'s `recursive` argument positionally
    # ("'recursive' must be a length-1 vector")
    monkey_patch("SeuratDisk", "GetImages", .GetImages)
    # SeuratDisk::SaveH5Seurat() will error, which passes slot instead layer to GetAssayData.Seurat() when assay is specified. This is because SeuratDisk::SaveH5Seurat() calls GetAssayData() with slot argument, which is not expected by SeuratObject::GetAssayData.Seurat()
    methods::setMethod(
        f = utils::getFromNamespace("WriteH5Group", "SeuratDisk"),
        signature = c('x' = 'Assay'),
        definition = WriteH5Group.Seurat,
        where = .GlobalEnv
    )
}
