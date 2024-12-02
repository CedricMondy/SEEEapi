#' Title
#'
#' @param export_naiades
#' @param stations_types
#'
#' @return
#' @export
#'
#' @examples

#' @importFrom archive archive_extract
#' @importFrom dplyr pull distinct anti_join select filter inner_join
#' @importFrom vroom vroom
naiades2seee_mib <- function(export_naiades, stations_types) {
  extract_dir <- tempfile()
  archive::archive_extract(export_naiades, dir = extract_dir)

  listes <- vroom::vroom(file.path(extract_dir, "ListesFauneFlore.CSV"))

  non_traites <- listes |>
    dplyr::anti_join(
      stations_types |>
        dplyr::filter(!is.na(TypeCEStationMesureEauxSurface)) |>
        dplyr::select(CdStationMesureEauxSurface, TypeCEStationMesureEauxSurface),
      by = "CdStationMesureEauxSurface"
    ) |>
    dplyr::distinct(CdStationMesureEauxSurface) |>
    dplyr::pull(CdStationMesureEauxSurface)


  if (length(non_traites) > 0)
    warning("Les stations suivantes ne seront pas traitées car n'ayant pas de type définis:\n", paste(non_traites, collapse = ", "))

  unlink(extract_dir, recursive = TRUE, force = TRUE)

  listes |>
    dplyr::inner_join(
      stations_types |>
        dplyr::select(CdStationMesureEauxSurface, TypeCEStationMesureEauxSurface),
      by = "CdStationMesureEauxSurface"
    ) |>
    dplyr::select(
      CODE_OPERATION = RefOperationPrelBio,
      CODE_STATION = CdStationMesureEauxSurface,
      DATE = DateDebutOperationPrelBio,
      TYPO_NATIONALE = TypeCEStationMesureEauxSurface,
      CODE_PHASE = CdListeFauFlor,
      CODE_TAXON = CdAppelTaxon,
      RESULTAT = RsTaxRep,
      CODE_REMARQUE = CdRqNbrTaxRep
    )
}