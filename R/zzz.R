.onAttach <- function(libname, pkgname) {
    # SeuratDisk::LoadH5Seurat() will error, which passes slot instead layer to GetAssayData.Seurat() when assay is specified. This is because SeuratDisk::AssembleAssay() calls GetAssayData() with slot argument, which is not expected by SeuratObject::GetAssayData.Seurat()
    monkey_patch("SeuratDisk", "AssembleAssay", .AssembleAssay)
    # SeuratDisk::SaveH5Seurat() will error, which passes slot instead layer to GetAssayData.Seurat() when assay is specified. This is because SeuratDisk::SaveH5Seurat() calls GetAssayData() with slot argument, which is not expected by SeuratObject::GetAssayData.Seurat()
    methods::setMethod(
        f = utils::getFromNamespace("WriteH5Group", "SeuratDisk"),
        signature = c('x' = 'Assay'),
        definition = WriteH5Group.Seurat,
        where = .GlobalEnv
    )
}
