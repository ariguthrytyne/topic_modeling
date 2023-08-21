# Diese Datei enthält den Code der VDKx für die Bestimmung der Segmente.
#
# Änderungshistorie
# Datum			  Bearbeiter				      Aktion
# 27.01.2022  Dr. Stephanie Dietrich  Initiale Erstellung

#' Hauptfunktion, die nacheinander die Segmentierungstabellen erstellt
#'
#' Lädt die Zulieferungstabellen und erstellt daraus die Segmentierungstabellen
#' der VDKx. Diese werden als Zwischenergebnisse für nachfolgende Sequenzen
#' ins Austausch-Verzeichnis geschrieben.
#'
#' @param path_exchange \code{String} mit dem Pfad des Austausch-Verzeichnis,
#' unter dem die erzeugten Zwischenergebnisse abgelegt werden und unter dem das
#' Environment vdkx abgelegt ist.
#' @param path_input \code{String} mit dem Pfad des Input-Verzeichnisses, in dem
#' die Zulieferungstabellen abgelegt sind.
#' @param vdkx_input_filename \code{String} mit den Dateinamen des
#' Environments vdkx, welches zu Beginn dieser Sequenz geladen werden soll.
#' @param vdkx_output_filename \code{String} mit den Dateinamen des
#' Environments vdkx, welches am Ende dieser Sequenz weggeschrieben werden soll.
#' @param sgmt_id \code{Integer} mit der Segmentierungs_ID, die bei der
#' Bestimmung der Segmente aus der Segmentierungsdatenbank verwendet werden
#' soll.
#' @param loglevel \code{String} der angibt, welche Meldungen in die Log-Datei
#' geschrieben werden sollen. Folgende Ausprägungen sind möglich:
#' 'DEBUG', 'INFO', 'WARNING', 'ERROR'
#' @param state_error \code{String} der das Status-Level bei einem Fehler
#' angibt.
#' @param logfile_path \code{String} mit dem Pfad, unter dem das Logfile
#' gespeichert werden soll.
#' @param logfile_prefix \code{String} mit dem Präfix, mit dem der Name des
#' Logfiles versehen wird.
#' @return Status der Ausführung
#' @family vdkx_segmentierung
#' @export
vdkx_segmentierung <- function(path_exchange = NULL,
                               path_input = NULL,
                               vdkx_input_filename = NULL,
                               vdkx_output_filename = NULL,
                               sgmt_id = NULL,
                               loglevel = "INFO",
                               state_error = "Failure",
                               logfile_path = file.path(getwd(), "output"),
                               logfile_prefix = "segmentierung"
                               ) {

  # Pfade prüfen / Logfile anlegen ####
  #____________________________________
  # Output-Ordner prüfen und Logfile erstellen
  if (!dir.exists(file.path(getwd(), "output"))) {
    # der Output-Ordner wird angelegt, falls er noch nicht vorhanden ist
    dir.create(file.path(getwd(), "output"))
  }

  # Erstellung Log-Datei für Segmentierungs-Sequenz
  logfile <- define_logging(path = logfile_path,
                            filename_prefix = logfile_prefix,
                            loglevel = loglevel)
  # Der Zeitzähler darf nur gesetzt werden, wenn auch die Ausgabe der Laufzeit
  # erfolgt. Dies ist somit abhängig vom verwendeten loglevel.
  if (loglevel %in% c("INFO", "DEBUG")) tictoc::tic(" vdkx_segmentierung")

  logging::loginfo(paste0("Beginn Abarbeitung Funktion: vdkx_segmentierung"))

  # Prüfung Austauch-Ordner
  logging::loginfo("Prüfung des Pfads für den Export von Zwischenergebnissen")
  path_exchange <- correct_path(path = path_exchange,
                                flag_path_replace = TRUE,
                                dir_new = "exchange")
  # Prüfung Input-Ordner
  logging::loginfo("Prüfung des Input-Pfads")
  path_input <- correct_path(path = path_input,
                             flag_path_replace = FALSE)

  # Falls einer der beiden Pfade nicht korrekt ist, wird die weitere
  # Verarbeitung abgebrochen
  if (is.null(path_exchange) | is.null(path_input)) {
    msg <- paste("Die Funktion wird abgebrochen, da mindestens ein ungültiger",
                 "Pfad übergeben wurde.")
    logging::logerror(msg)
    return(state_error)
  } else {
    # Prüfe, ob ein Environment im Austausch-Verzeichnis liegt
    if (is.null(vdkx_input_filename) |
        !file.exists(file.path(path_exchange, vdkx_input_filename))) {
      msg <- paste("Die Funktion wird abgebrochen, da kein Environment geladen",
                   "werden kann. Die Datei",
                   file.path(path_exchange, vdkx_input_filename),
                   "existiert nicht.")
      logging::logerror(msg)
      return(state_error)
    } else {
      # 1. Lade Environment vdkx ####
      #______________________________
      vdkx <- readRDS(file.path(path_exchange, vdkx_input_filename))
      vdkx$used_loglevel <- loglevel

      # 2. Aufruf Unterfunktionen zur Erstellung der Segmentierungstabellen ####
      #_________________________________________________________________________

      # CCF ####
      pre_result <- create_seg_ccf(vdkx,
                                   tablename = "T_8100_CCF_Segmentierung",
                                   path_exchange = path_exchange,
                                   path_input = path_input,
                                   sgmt_id = sgmt_id)

      # VQ ####
      pre_result <- create_seg_vq(vdkx,
                                  tablename = "T_8200_VQ_Segmentierung",
                                  path_exchange = path_exchange,
                                  path_input = path_input,
                                  sgmt_id = sgmt_id)

      # LGD ####
      pre_result <- create_seg_lgd(vdkx,
                                   tablename = "T_8300_LGD_Segmentierung",
                                   path_exchange = path_exchange,
                                   path_input = path_input,
                                   sgmt_id = sgmt_id)

      # 3. Speichern des aktuellen Environments ####
      #_____________________________________________
      saveRDS(vdkx, file.path(path_exchange, vdkx_output_filename))

      logging::loginfo(paste(vdkx$info_msg_function_end,
                    create_info_string(),
                    "vdkx_segmentierung"))

      # 4. R-Warnungen im logfile ergänzen ####
      sink(logfile, append = TRUE, type = "output")
      print(warnings())
      sink()

      return(vdkx$status)
    }
  }
}

#' Erstellt die Segmentierungstabelle fuer den CCF
#'
#' In dieser Funktion wird die Segmentierungstabelle für den CCF erstellt
#' und das Data-Dictionary für diese Tabelle wird befüllt.
#'
#' @param vdkx \code{Environment}, das Standardvariablen,
#' Schlüsselverzeichnisse und das Data-Dictionary enthält.
#' @param tablename \code{String}, mit dem Namen, mit dem die Tabelle ins Data-
#' Dictionary eingetragen werden soll.
#' @param path_exchange \code{String} mit dem Pfad des Austausch-Verzeichnis,
#' unter dem die erzeugten Zwischenergebnisse abgelegt werden und unter dem das
#' Environment vdkx abgelegt ist.
#' @param path_input \code{String} mit dem Pfad des Input-Verzeichnisses, in dem
#' die Zulieferungstabellen abgelegt sind.
#' @param sgmt_id \code{Integer} mit der Segmentierungs_ID, die bei der
#' Bestimmung der Segmente aus der Segmentierungsdatenbank verwendet werden
#' soll.
#' @family vdkx_segmentierung
create_seg_ccf <- function(vdkx,
                           tablename = NULL,
                           path_exchange = NULL,
                           path_input = NULL,
                           sgmt_id = NULL) {

  logging::loginfo("=========================================================")
  if (vdkx$used_loglevel %in% c("INFO", "DEBUG")) {
    tictoc::tic(" create_seg_ccf")
  }
  logging::loginfo(paste(vdkx$info_msg_function_begin,
                         "create_seg_ccf"))
  logging::loginfo("=========================================================")

  # Ausführungsstatus der Funktion setzen
  state_function <- vdkx$state_success

  # Pfade prüfen
  logging::loginfo("Prüfung der Pfade des Austausch- und des Input-Ordners")
  path_exchange <- correct_path(path = path_exchange,
                                flag_path_replace = TRUE,
                                dir_new = "exchange")
  # Prüfung Input-Ordner
  path_input <- correct_path(path = path_input,
                             flag_path_replace = FALSE)

  # Falls einer der beiden Pfade nicht korrekt ist, wird die weitere
  # Verarbeitung abgebrochen
  if (is.null(path_exchange) | is.null(path_input)) {
    logging::logerror(vdkx$error_msg_wrong_path)
    vdkx$status <- vdkx$state_error
    state_function <- vdkx$state_error

  } else {

    # 1. Schritt: Import Input-Dateien ####
    basis_kunde <- import_parquet_file(vdkx,
                                       path_exchange,
                                       file = "T_1104_Kunde.parquet",
                                       dq_string = "SEGMENTIERUNG_CCF")
    basis_afr_konto <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1220_Ausfallreihe_Konto.parquet",
                          dq_string = "SEGMENTIERUNG_CCF")
    param_ccf <- import_parquet_file(vdkx,
                                     path_exchange,
                                     file = "T_5200_CCF.parquet",
                                     dq_string = "SEGMENTIERUNG_CCF")
    seg_db <- import_rds_file(vdkx,
                              path_input,
                              file = "segmentation.rds",
                              dq_string = "SEGMENTIERUNG_CCF")

    if (state_function != vdkx$state_error
        & !is.null(basis_kunde)
        & !is.null(basis_afr_konto)
        & !is.null(param_ccf)
        & !is.null(seg_db)) {

      # 2. Schritt: Data-Dictionary für Input-Dateien ####
      fill_dd_input_table(vdkx,
                          data = basis_kunde,
                          tablename = "T_1104_Kunde")
      fill_dd_input_table(vdkx,
                          data = basis_afr_konto,
                          tablename = "T_1220_Ausfallreihe_Konto")
      fill_dd_input_table(vdkx,
                          data = param_ccf,
                          tablename = "T_5200_CCF")

      # 3. Schritt: Segmentierungstabelle erzeugen ####

      # 3.1 Loggen der verwendeten Version der Segmentierungs-DB ####

      # hole aus der Versionshistorientabelle der Segmentierungsdatenbank
      # die letzte Zeile
      seg_db_version_last_entry <- tail(seg_db[["T_000_Versionshistorie"]], 1)
      seg_db_version <- paste0(seg_db_version_last_entry$Version, ".",
                               seg_db_version_last_entry$Unterversion)
      logging::loginfo(paste("Verwendet wird die Segmentierungsdatenbank mit",
                             "der Version", seg_db_version, "vom",
                             seg_db_version_last_entry$Datum))

      # 3.2 Zusammenstellung Ausgangsspalten ####
      logging::loginfo(paste("Erstellung des Grundgerüsts aus PK-Spalten und",
                       "zulässigen Segmentierungskriterien."))

      seg_ccf_1 <- param_ccf %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, KTO_NR, KTO_QT, KTO_KLASSE,
                      KD_RISIKOVERFAHRENSSCHALTER, CCF_LINIE_VORJAHR,
                      KTO_ALTER, KD_BONI_NOTE, CCF_ANTEIL_OFFENE_LINIE) %>%
        dplyr::left_join(basis_kunde %>%
                           dplyr::select(PNR_KD, PNR_KD_QT,
                                         KD_KUNDENSYSTEMATIK),
                         by = c("PNR_KD", "PNR_KD_QT")) %>%
        dplyr::left_join(basis_afr_konto %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                         KD_KTO_KLASSE_INT) %>%
                           dplyr::distinct(),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::mutate(
          KD_KTO_KLASSE = int_to_bitmask_6(KD_KTO_KLASSE_INT)
        ) %>%
        dplyr::select(-KD_KTO_KLASSE_INT)


      # 3.3 Anspielen der ccF-Segmente ####
      seg_ccf <- seg_ccf_1 # TODO: hier muss der Funktionsaufruf kommen,
      # der die Segmente an den Dataframe seg_ccf_1 anspielt

      # 4. Schritt: Data-Dictionary für Segmentierungstabelle ####
      create_data_dictionary(vdkx,
                             data = seg_ccf,
                             data_description =
                               get_col_tab_description(vdkx, tablename),
                             tablename = tablename)

      # 5. Schritt: Speichern / Protokollierung Segmentierungstabelle ####
      arrow::write_parquet(seg_ccf,
                           file.path(path_exchange,
                                     paste0(tablename, ".parquet")))
      insert_table_protocol(vdkx, tablename, nrow(seg_ccf))
    } else {
      loginfo(paste(vdkx$info_msg_create_basis_not_done, tablename))
    }
  }
  logging::loginfo(paste(vdkx$info_msg_function_end,
                         create_info_string(),
                         "create_seg_ccf"))
}

#' Erstellt die Segmentierungstabelle fuer die VQ-Parameter
#'
#' In dieser Funktion wird die Segmentierungstabelle für die VQ-Parameter
#' erstellt und das Data-Dictionary für diese Tabelle wird befüllt.
#'
#' @param vdkx \code{Environment}, das Standardvariablen,
#' Schlüsselverzeichnisse und das Data-Dictionary enthält.
#' @param tablename \code{String}, mit dem Namen, mit dem die Tabelle ins Data-
#' Dictionary eingetragen werden soll.
#' @param path_exchange \code{String} mit dem Pfad des Austausch-Verzeichnis,
#' unter dem die erzeugten Zwischenergebnisse abgelegt werden und unter dem das
#' Environment vdkx abgelegt ist.
#' @param path_input \code{String} mit dem Pfad des Input-Verzeichnisses, in dem
#' die Zulieferungstabellen abgelegt sind.
#' @param sgmt_id \code{Integer} mit der Segmentierungs_ID, die bei der
#' Bestimmung der Segmente aus der Segmentierungsdatenbank verwendet werden
#' soll.
#' @family vdkx_segmentierung
create_seg_vq <- function(vdkx,
                          tablename = NULL,
                          path_exchange = NULL,
                          path_input = NULL,
                          sgmt_id = NULL) {

  logging::loginfo("=========================================================")
  if (vdkx$used_loglevel %in% c("INFO", "DEBUG")) {
    tictoc::tic(" create_seg_vq")
  }
  logging::loginfo(paste(vdkx$info_msg_function_begin,
                         "create_seg_vq"))
  logging::loginfo("=========================================================")

  # Ausführungsstatus der Funktion setzen
  state_function <- vdkx$state_success

  # Pfade prüfen
  logging::loginfo("Prüfung der Pfade des Austausch- und des Input-Ordners")
  path_exchange <- correct_path(path = path_exchange,
                                flag_path_replace = TRUE,
                                dir_new = "exchange")
  # Prüfung Input-Ordner
  path_input <- correct_path(path = path_input,
                             flag_path_replace = FALSE)

  # Falls einer der beiden Pfade nicht korrekt ist, wird die weitere
  # Verarbeitung abgebrochen
  if (is.null(path_exchange) | is.null(path_input)) {
    logging::logerror(vdkx$error_msg_wrong_path)
    vdkx$status <- vdkx$state_error
    state_function <- vdkx$state_error

  } else {

    # 1. Schritt: Import Input-Dateien ####
    basis_vmo <- import_parquet_file(vdkx,
                                     path_exchange,
                                     file = "T_1350_VMO.parquet",
                                     dq_string = "SEGMENTIERUNG_VQ")
    seg_db <- import_rds_file(vdkx,
                              path_input,
                              file = "segmentation.rds",
                              dq_string = "SEGMENTIERUNG_VQ")

    vdkx$mapPLZ <- import_parquet_file(vdkx,
                                       path_input,
                                       file = "INPUT_VMO_IMMO_PLZ_Mapping.parquet",
                                       dq_string = "MAPPING_PLZ")

    vdkx$mapSEG <- import_parquet_file(vdkx,
                                       path_input,
                                       file = "INPUT_Mapping_Segmentierung.parquet",
                                       dq_string = "MAPPING_SEG")


    if (state_function != vdkx$state_error
        & !is.null(basis_vmo)
        & !is.null(seg_db)
        & !is.null(vdkx$mapPLZ)
        & !is.null(vdkx[["v_VDH_Schluessel_VMO_Nutzungsart"]])) {

      # 2. Schritt: Data-Dictionary für Input-Dateien ####
      fill_dd_input_table(vdkx,
                          data = basis_vmo,
                          tablename = "T_1350_VMO")

      # 3. Schritt: Segmentierungstabelle erzeugen ####

      # 3.1 Loggen der verwendeten Version der Segmentierungs-DB ####

      # hole aus der Versionshistorientabelle der Segmentierungsdatenbank
      # die letzte Zeile
      seg_db_version_last_entry <- tail(seg_db[["T_000_Versionshistorie"]], 1)
      seg_db_version <- paste0(seg_db_version_last_entry$Version, ".",
                               seg_db_version_last_entry$Unterversion)
      logging::loginfo(paste("Verwendet wird die Segmentierungsdatenbank mit",
                             "der Version", seg_db_version, "vom",
                             seg_db_version_last_entry$Datum))

      # 3.2 Zusammenstellung Ausgangsspalten ####
      logging::loginfo(paste("Erstellung des Grundgerüsts aus PK-Spalten und",
                             "zulässigen Segmentierungskriterien."))

      seg_vq_1 <- basis_vmo %>%
        dplyr::select(VMO_CODE, VMO_QT, VMO_KLASSE, IMMO_NUTZART,VMO_V_ART,VMO_V_DAT, IMMO_PLZ) %>%
        dplyr::mutate(VMO_V_JAHR = as.integer(lubridate::year(VMO_V_DAT))) %>%
        dplyr::left_join(vdkx[["v_VDH_Schluessel_VMO_Nutzungsart"]],
                         by = c("IMMO_NUTZART" = "SiO_Nutzart_Bez"))


      # 3.3 Anspielen der Segmente für die VQ-Parameter ####
      vq_parameter <- c("NVQ", "TNVQ", "BVQ", "KQ", "TAD", "TDAA", "TDVZ",
                        "TKQ", "WFH", "AKQ_V")

      # Initialbefüllung von seg_vq
      seg_vq <- seg_vq_1
      for (param in vq_parameter) {
        # build Column Names
        colName1 <- paste0("Segment",param )
        colName2 <- paste0("Segment_Nr_",param )

        #seg_vq <- seg_vq # TODO: hier muss der Funktionsaufruf kommen,
        # der die Segmente an den Dataframe seg_vq_1 anspielt
        seg_vq <- AddSegmentsFile(vdkx, sgmt_table = seg_vq, sgmt_id  = sgmt_id, sgmt_db = sgmt_db,
                                  sgmt_param = param, colName1 = , colName2 = )
      }


      # 4. Schritt: Data-Dictionary für Segmentierungstabelle ####
      create_data_dictionary(vdkx,
                             data = seg_vq,
                             data_description =
                               get_col_tab_description(vdkx, tablename),
                             tablename = tablename)

      # 5. Schritt: Speichern / Protokollierung Segmentierungstabelle ####
      arrow::write_parquet(seg_vq,
                           file.path(path_exchange,
                                     paste0(tablename, ".parquet")))
      insert_table_protocol(vdkx, tablename, nrow(seg_vq))
    } else {
      loginfo(paste(vdkx$info_msg_create_basis_not_done, tablename))
    }
  }
  logging::loginfo(paste(vdkx$info_msg_function_end,
                         create_info_string(),
                         "create_seg_vq"))
}

#' Erstellt die Segmentierungstabelle fuer die LGD-Parameter
#'
#' In dieser Funktion wird die Segmentierungstabelle für die LGD-Parameter
#' erstellt und das Data-Dictionary für diese Tabelle wird befüllt.
#'
#' @param vdkx \code{Environment}, das Standardvariablen,
#' Schlüsselverzeichnisse und das Data-Dictionary enthält.
#' @param tablename \code{String}, mit dem Namen, mit dem die Tabelle ins Data-
#' Dictionary eingetragen werden soll.
#' @param path_exchange \code{String} mit dem Pfad des Austausch-Verzeichnis,
#' unter dem die erzeugten Zwischenergebnisse abgelegt werden und unter dem das
#' Environment vdkx abgelegt ist.
#' @param path_input \code{String} mit dem Pfad des Input-Verzeichnisses, in dem
#' die Zulieferungstabellen abgelegt sind.
#' @param sgmt_id \code{Integer} mit der Segmentierungs_ID, die bei der
#' Bestimmung der Segmente aus der Segmentierungsdatenbank verwendet werden
#' soll.
#' @family vdkx_segmentierung
create_seg_lgd <- function(vdkx,
                           tablename = NULL,
                           path_exchange = NULL,
                           path_input = NULL,
                           sgmt_id = NULL) {

  logging::loginfo("=========================================================")
  if (vdkx$used_loglevel %in% c("INFO", "DEBUG")) {
    tictoc::tic(" create_seg_lgd")
  }
  logging::loginfo(paste(vdkx$info_msg_function_begin,
                         "create_seg_lgd"))
  logging::loginfo("=========================================================")

  # Ausführungsstatus der Funktion setzen
  state_function <- vdkx$state_success

  # Pfade prüfen
  logging::loginfo("Prüfung der Pfade des Austausch- und des Input-Ordners")
  path_exchange <- correct_path(path = path_exchange,
                                flag_path_replace = TRUE,
                                dir_new = "exchange")
  # Prüfung Input-Ordner
  path_input <- correct_path(path = path_input,
                             flag_path_replace = FALSE)

  # Falls einer der beiden Pfade nicht korrekt ist, wird die weitere
  # Verarbeitung abgebrochen
  if (is.null(path_exchange) | is.null(path_input)) {
    logging::logerror(vdkx$error_msg_wrong_path)
    vdkx$status <- vdkx$state_error
    state_function <- vdkx$state_error

  } else {

    # 1. Schritt: Import Input-Dateien ####
    param_lgd <- import_parquet_file(vdkx,
                                     path_exchange,
                                     file = "T_5600_LGD.parquet",
                                     dq_string = "SEGMENTIERUNG_LGD")
    seg_db <- import_rds_file(vdkx,
                              path_input,
                              file = "segmentation.rds",
                              dq_string = "SEGMENTIERUNG_LGD")

    if (state_function != vdkx$state_error
        & !is.null(param_lgd)
        & !is.null(seg_db)) {

      # 2. Schritt: Data-Dictionary für Input-Dateien ####
      fill_dd_input_table(vdkx,
                          data = param_lgd,
                          tablename = "T_5600_LGD")

      # 3. Schritt: Segmentierungstabelle erzeugen ####

      # 3.1 Loggen der verwendeten Version der Segmentierungs-DB ####

      # hole aus der Versionshistorientabelle der Segmentierungsdatenbank
      # die letzte Zeile
      seg_db_version_last_entry <- tail(seg_db[["T_000_Versionshistorie"]], 1)
      seg_db_version <- paste0(seg_db_version_last_entry$Version, ".",
                               seg_db_version_last_entry$Unterversion)
      logging::loginfo(paste("Verwendet wird die Segmentierungsdatenbank mit",
                             "der Version", seg_db_version, "vom",
                             seg_db_version_last_entry$Datum))

      # 3.2 Zusammenstellung Ausgangsspalten ####
      logging::loginfo(paste("Erstellung des Grundgerüsts aus PK-Spalten und",
                             "zulässigen Segmentierungskriterien."))

      seg_lgd_1 <- param_lgd %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, KD_RISIKOVERFAHRENSSCHALTER,
                      KD_MIN_AFG_EAD, KD_BESICHERUNGS_GRAD_PROG,
                      KD_KUNDENSYSTEMATIK, KD_KTO_KLASSE_INT) %>%
        dplyr::mutate(
          KD_KTO_KLASSE = int_to_bitmask_6(KD_KTO_KLASSE_INT)
        ) %>%
        dplyr::select(-KD_KTO_KLASSE_INT)

      # 3.3 Anspielen der Segmente für die VQ-Parameter ####
      lgd_parameter <- c("AKQ_RM", "AKQ_FA", "NRQ", "TNR", "WRM")

      # Initialbefüllung von seg_lgd
      seg_lgd <- seg_lgd_1
      for (param in lgd_parameter) {
        seg_lgd <- seg_lgd # TODO: hier muss der Funktionsaufruf kommen,
        # der die Segmente an den Dataframe seg_lgd_1 anspielt
      }


      # 4. Schritt: Data-Dictionary für Segmentierungstabelle ####
      create_data_dictionary(vdkx,
                             data = seg_lgd,
                             data_description =
                               get_col_tab_description(vdkx, tablename),
                             tablename = tablename)

      # 5. Schritt: Speichern / Protokollierung Segmentierungstabelle ####
      arrow::write_parquet(seg_lgd,
                           file.path(path_exchange,
                                     paste0(tablename, ".parquet")))
      insert_table_protocol(vdkx, tablename, nrow(seg_lgd))
    } else {
      loginfo(paste(vdkx$info_msg_create_basis_not_done, tablename))
    }
  }
  logging::loginfo(paste(vdkx$info_msg_function_end,
                         create_info_string(),
                         "create_seg_lgd"))
}
