# Diese Datei enthält den Code der VDKx für die Berechnung der
# Risikoparameter.
#
# Änderungshistorie
# Datum			  Bearbeiter				      Aktion
# 15.09.-     Dr. Stephanie Dietrich/ Initiale Erstellung
# 22.12.2021	Juri Wiesner
# 11.01.2022  Dr. Stephanie Dietrich  - Anpassung Tabellennamen
#                                     - Finalisierung der LGD-Tabellen
# 14.01.2022  Dr. Stephanie Dietrich  Bug-Tickets: WARP-340, WARP-341, WARP-343,
#                                     WARP-346, WARP-348
# 17.01.2022  Dr. Stephanie Dietrich  Bug-Tickets: WARP-350
# 18.01.2022  Dr. Stephanie Dietrich  Bug-Tickets: WARP-361, WARP-362
# 21.01.2022  Dr. Stephanie Dietrich  - Korrektur: ZINS_ERFASSUNG_KNZ
#                                     - R-Warnungen ins logfile schreiben
# 24.01.2022  Juri Wiesner            Bug-Tickets: WARP-379
# 09.02.2022  Thierry Monthe          Erweiterung der Tabelle T_5400_VQ um Segmentierungsvariablen
#                                     IMMO_METROPOLE, IMMO_KREISFREI, IMMO_KAUFKRAFT (WARP-411)


#' Hauptfunktion, die nacheinander die Parametertabellen erstellt
#'
#' Lädt die Zulieferungstabellen und erstellt daraus die Parametertabellen der
#' VDKx. Diese werden als Zwischenergebnisse für nachfolgende Sequenzen
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
#' @param max_interval_ccf_vorjahr \code{Integer} der im Rahmen der
#' CCF-Berechnung die Anzahl an Tagen vor und nach des Datums ein Jahr vor
#' Ausfall festlegt. Es werden Vorjahresbewertungen von Kreditkarten
#' ausschließlich dann verwendet, wenn das Datum der Vorjahresbewertungen
#'  innerhalb dieses Intervals liegt.
#' @param data_reference_date \code{Date} mit dem Datum, welches als Stichtag
#' gilt und zur Überprüfung des Statuses eines Kontos (aktiv oder nicht)
#' verwendet wird.
#' @param fmp_reference_date \code{Date} mit dem Datum, welches als Stichtag
#' zur Bestimmung der FMP-Verlustdifferenz verwendet wird.
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
#' @family vdkx_parameter
#' @export
vdkx_parameter <- function(path_exchange = NULL,
                           path_input = NULL,
                           vdkx_input_filename = NULL,
                           vdkx_output_filename = NULL,
                           max_interval_ccf_vorjahr = NULL,
                           data_reference_date = NULL,
                           fmp_reference_date = NULL,
                           loglevel = "INFO",
                           state_error = "Failure",
                           logfile_path = file.path(getwd(), "output"),
                           logfile_prefix = "parameter"
                           ) {

  # Pfade prüfen / Logfile anlegen ####
  #____________________________________
  # Output-Ordner prüfen und Logfile erstellen
  if (!dir.exists(file.path(getwd(), "output"))) {
    # der Output-Ordner wird angelegt, falls er noch nicht vorhanden ist
    dir.create(file.path(getwd(), "output"))
  }

  # Erstellung Log-Datei für Parameter-Sequenz
  logfile <- define_logging(path = logfile_path,
                            filename_prefix = logfile_prefix,
                            loglevel = loglevel)
  # Der Zeitzähler darf nur gesetzt werden, wenn auch die Ausgabe der Laufzeit
  # erfolgt. Dies ist somit abhängig vom verwendeten loglevel.
  if (loglevel %in% c("INFO", "DEBUG")) tictoc::tic(" vdkx_parameter")

  logging::loginfo(paste0("Beginn Abarbeitung Funktion: vdkx_parameter"))

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

      # 2. Aufruf der Unterfunktionen zur Erstellung der Parametertabellen ####
      #________________________________________________________________________

      # CCF ####
      pre_result <- create_parameter_ccf(vdkx,
                                         tablename = "T_5200_CCF",
                                         path_exchange = path_exchange,
                                         path_input = path_input,
                                         max_interval_ccf_vorjahr =
                                           max_interval_ccf_vorjahr,
                                         data_reference_date =
                                           data_reference_date)
      # VQ ####
      pre_result <- create_parameter_vq(vdkx,
                                        tablename = "T_5400_VQ",
                                        path_exchange = path_exchange,
                                        path_input = path_input,
                                        fmp_reference_date =
                                          fmp_reference_date)
      # LGD Vorverarbeitung ####
      pre_result <-
        create_parameter_lgd_vv(vdkx,
                                tablename = "T_5510_LGD_Vorverarbeitung",
                                path_exchange = path_exchange,
                                path_input = path_input)
      # LGD Ergebnisse ####
      pre_result <-
        create_parameter_lgd(vdkx,
                             tablename = "T_5600_LGD",
                             path_exchange = path_exchange,
                             path_input = path_input)

      # 3. Speichern des aktuellen Environments ####
      #_____________________________________________
      saveRDS(vdkx, file.path(path_exchange, vdkx_output_filename))

      logging::loginfo(paste(vdkx$info_msg_function_end,
                    create_info_string(),
                    "vdkx_parameter"))

      # 4. R-Warnungen im logfile ergänzen ####
      sink(logfile, append = TRUE, type = "output")
      print(warnings())
      sink()

      return(vdkx$status)
    }
  }
}

#' Erstellt die Parametertabelle CCF
#'
#' In dieser Funktion wird die Parametertabelle CCF erstellt und das
#' Data-Dictionary für diese Tabelle wird befüllt.
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
#' @param max_interval_ccf_vorjahr \code{Integer} der im Rahmen der
#' CCF-Berechnung die Anzahl an Tagen vor und nach des Datums ein Jahr vor
#' Ausfall festlegt. Es werden Vorjahresbewertungen von Kreditkarten
#' ausschließlich dann verwendet, wenn das Datum der Vorjahresbewertungen
#' innerhalb dieses Intervals liegt.
#' @param data_reference_date \code{Date} mit dem Datum, welches als Stichtag
#' gilt und zur Überprüfung des Statuses eines Kontos (aktiv oder nicht)
#' verwendet wird.
#' @family vdkx_parameter
create_parameter_ccf <- function(vdkx,
                                 tablename = NULL,
                                 path_exchange = NULL,
                                 path_input = NULL,
                                 max_interval_ccf_vorjahr = NULL,
                                 data_reference_date = NULL) {

  logging::loginfo("=========================================================")
  if (vdkx$used_loglevel %in% c("INFO", "DEBUG")) {
    tictoc::tic(" create_parameter_ccf")
  }
  logging::loginfo(paste(vdkx$info_msg_function_begin,
                         "create_parameter_ccf"))
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
                                        dq_string = "CCF")
    basis_afr_kunde <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1140_Ausfallreihe_Kunde.parquet",
                          dq_string = "CCF")

    basis_kunde_buchungen <-
      import_parquet_file(vdkx,
                        path_exchange,
                        file = "T_1150_Kunde_Buchungen.parquet",
                        dq_string = "CCF")

    basis_afr_konto <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1220_Ausfallreihe_Konto.parquet",
                          dq_string = "CCF")

    if (state_function != vdkx$state_error
        & !is.null(basis_kunde)
        & !is.null(basis_afr_kunde)
        & !is.null(basis_kunde_buchungen)
        & !is.null(basis_afr_konto)
        & !is.null(vdkx[["SVZ_AVAL_AUSSTEUERUNG_ID"]])) {

      # 2. Schritt: Data-Dictionary für Input-Dateien ####
      fill_dd_input_table(vdkx,
                          data = basis_kunde,
                          tablename = "T_1104_Kunde")
      fill_dd_input_table(vdkx,
                          data = basis_afr_kunde,
                          tablename = "T_1140_Ausfallreihe_Kunde")
      fill_dd_input_table(vdkx,
                          data = basis_kunde_buchungen,
                          tablename = "T_1150_Kunde_Buchungen")
      fill_dd_input_table(vdkx,
                          data = basis_afr_konto,
                          tablename = "T_1220_Ausfallreihe_Konto")
      fill_dd_input_table(vdkx,
                          data = vdkx[["SVZ_AVAL_AUSSTEUERUNG_ID"]],
                          tablename = "SVZ_AVAL_AUSSTEUERUNG_ID")


      # 3. Schritt: Parametertabelle erzeugen ####


       # Selektion der Teilmenge an Kreditkarten zur späteren Berücksichtigung
      # im Saldo und der Linie der zugehörigen Abrechnungskonten
      gg_kreditkarten <- basis_afr_konto %>%
        dplyr::filter(KTO_KLASSE == 24) %>%
       # Anspielen des Referenzdatums des Abrechnungskontos
        dplyr::left_join(basis_afr_konto %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, KTO_NR,
                                         MIN_AFG_DAT_MINUS_1J_REFERENZ_DAT),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD",
                                "KTO_NR_ABRECHNUNGSKONTO_KK" = "KTO_NR"),
                         suffix = c("", ".abrKTO")
        )  %>%
        dplyr::mutate(
          # Nur positive Beträge, da keine Verrechnung von Guthaben bzw.
          # Überziehungen.
          # Für Vorjahresbewertungen wird vorausgesetzt dass ein zugehöriges
          # Referenzdatum und Bewertungsdatum vorliegt.
          # Vorjahres-Salden und -Linien deren Bewertungsdatum außerhalb
          # des Intervals um das Vorjahresreferenzdatum des Abrechnungskontos
          # liegen werden auf 0 gesetzt.
          #
          KARTE_CCF_SALDO_kto = pmax(dplyr::coalesce(MIN_AFG_SALDO, 0), 0),
          KARTE_CCF_LINIE_kto = pmax(dplyr::coalesce(MIN_AFG_LINIE, 0), 0),
          KARTE_CCF_SALDO_VORJAHR_kto =
            dplyr::if_else(
              is.na(MIN_AFG_DAT_MINUS_1J_REFERENZ_DAT.abrKTO) |
                is.na(MIN_AFG_DAT_MINUS_1J_BEW_DAT), 0,
              dplyr::if_else(
                abs(as.integer(MIN_AFG_DAT_MINUS_1J_REFERENZ_DAT.abrKTO
                               - MIN_AFG_DAT_MINUS_1J_BEW_DAT)) <=
                  max_interval_ccf_vorjahr,
                pmax(dplyr::coalesce(MIN_AFG_DAT_MINUS_1J_SALDO, 0), 0), 0)),
          KARTE_CCF_LINIE_VORJAHR_kto =
            dplyr::if_else(
              is.na(MIN_AFG_DAT_MINUS_1J_REFERENZ_DAT.abrKTO) |
                is.na(MIN_AFG_DAT_MINUS_1J_BEW_DAT), 0,
              dplyr::if_else(
                abs(as.integer(MIN_AFG_DAT_MINUS_1J_REFERENZ_DAT.abrKTO
                               - MIN_AFG_DAT_MINUS_1J_BEW_DAT)) <=
                  max_interval_ccf_vorjahr,
                pmax(dplyr::coalesce(MIN_AFG_DAT_MINUS_1J_LINIE, 0), 0),
                0))
          ) %>%
        dplyr::mutate(
          # Ermittlung der offenen Linie vor der Aggregation. Ist der Saldo
          # größer als die Linie ist, setze die Offene Linie gleich 0.
          KARTE_CCF_OFFENE_LINIE_kto = pmax(KARTE_CCF_LINIE_kto -
                                              KARTE_CCF_SALDO_kto, 0),
          KARTE_CCF_OFFENE_LINIE_VORJAHR_kto =
            pmax(KARTE_CCF_LINIE_VORJAHR_kto - KARTE_CCF_SALDO_VORJAHR_kto, 0),
        ) %>%
        # Aufsummieren der Saldo, Linie und Offene Linie Beträge je
        # Abrechnungskonto
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD,
                        KTO_NR_ABRECHNUNGSKONTO_KK) %>%
        dplyr::summarise(
          KARTE_ANZAHL = n(),
          KARTE_CCF_SALDO = sum(KARTE_CCF_SALDO_kto),
          KARTE_CCF_LINIE = sum(KARTE_CCF_LINIE_kto),
          KARTE_CCF_OFFENE_LINIE = sum(KARTE_CCF_OFFENE_LINIE_kto),
          KARTE_CCF_SALDO_VORJAHR = sum(KARTE_CCF_SALDO_VORJAHR_kto),
          KARTE_CCF_LINIE_VORJAHR = sum(KARTE_CCF_LINIE_VORJAHR_kto),
          KARTE_CCF_OFFENE_LINIE_VORJAHR =
            sum(KARTE_CCF_OFFENE_LINIE_VORJAHR_kto),
          KARTE_CCF_MIN_M1J_BEW_DAT = min(MIN_AFG_DAT_MINUS_1J_BEW_DAT),
          KARTE_CCF_MAX_M1J_BEW_DAT = max(MIN_AFG_DAT_MINUS_1J_BEW_DAT)) %>%
        dplyr::ungroup() %>%
        dplyr::select(
          PNR_KD, PNR_KD_QT, AFR_KD, KTO_NR_ABRECHNUNGSKONTO_KK,
          KARTE_ANZAHL, KARTE_CCF_SALDO, KARTE_CCF_LINIE,
          KARTE_CCF_OFFENE_LINIE, KARTE_CCF_SALDO_VORJAHR,
          KARTE_CCF_LINIE_VORJAHR, KARTE_CCF_OFFENE_LINIE_VORJAHR,
          KARTE_CCF_MIN_M1J_BEW_DAT, KARTE_CCF_MAX_M1J_BEW_DAT)

      # Bestimmung der Einzelaval Zahlungen (EAZ) für Einzelavale
      eaz_einzelaval <- basis_kunde_buchungen %>%
        dplyr::select(AVAL_KTO_NR, KTO_NR, KTO_QT, BU_ID, BU_QT,
                      BU_VALUTA, BU_DAT, BU_KLASSE, BU_BETRAG) %>%
        # Es werden nur Avalinanspruchnahmen berücksichtigt
        dplyr::filter(BU_KLASSE == "Avalinanspruchnahme") %>%
        # Anspielen der Einzelavale (KTO_Klasse 16) über die Avalkontonummer
        inner_join(dplyr::filter(basis_afr_konto, KTO_KLASSE == "16"),
                   by = c("AVAL_KTO_NR" = "KTO_NR")) %>%
        # Anspielen des Ende-Datums einer Ausfallreihe
        dplyr::left_join(basis_afr_kunde %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                         MAX_AFG_ENDE),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        # Es werden nur Zahlungen berücksichtigt, deren Valutadatum (alternativ
        # das Buchungsdatum) entweder in dem Zeitraum ein Jahr vor Ausfall
        # liegt, oder innerhalb der Ausfallreihe
        dplyr::filter(
          (dplyr::coalesce(BU_VALUTA, BU_DAT) < MIN_AFG_DAT &
             (MIN_AFG_DAT_MINUS_1J) < dplyr::coalesce(BU_VALUTA, BU_DAT)) |
            (MIN_AFG_DAT < dplyr::coalesce(BU_VALUTA, BU_DAT) &
               dplyr::coalesce(BU_VALUTA, BU_DAT)
             <= dplyr::coalesce(MAX_AFG_ENDE,
                                as.Date("01.01.9999", format = "%d.%m.%Y"))))

      # Bestimmung der Einzelaval Zahlungen (EAZ) für die Basiskonten
      # (nicht Avale, angejoint über KTO_NR nicht AVAL_KTO_NR)
      eaz_nicht_aval <- basis_kunde_buchungen %>%
        dplyr::select(AVAL_KTO_NR, KTO_NR, KTO_QT, BU_ID, BU_QT,
                      BU_VALUTA, BU_DAT, BU_KLASSE, BU_BETRAG) %>%
        # Es werden nur Avalinanspruchnahmen berücksichtigt
        dplyr::filter(BU_KLASSE == "Avalinanspruchnahme") %>%
        # Anspielen des Basiskontos, welches kein Aval ist
        inner_join(dplyr::filter(basis_afr_konto, KTO_TYP_1 != "Aval"),
                   by = c("KTO_NR", "KTO_QT")) %>%
        # Anspielen des Ende-Datums einer Ausfallreihe (hier nur informatorisch)
        dplyr::left_join(basis_afr_kunde %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        # Es werden nur Zahlungen berücksichtigt, deren Valutadatum (alternativ
        # das Buchungsdatum) in dem Zeitraum ein Jahr vor Ausfall liegt
        # (Hinweis: innerhalb der Ausfallreihe kein Kriterium für das
        # Basiskonto)
        dplyr::filter(dplyr::coalesce(BU_VALUTA, BU_DAT) < MIN_AFG_DAT &
                        (MIN_AFG_DAT_MINUS_1J)
                      < dplyr::coalesce(BU_VALUTA, BU_DAT)) %>%
        dplyr::mutate(
          aval_ia_ohne_bzgs_kto_zaehler = if_else(is.na(AVAL_KTO_NR), 1, 0)
        )

      # Summieren der EAZ je Einzelaval
      eaz_aggregiert_einzelaval <- eaz_einzelaval %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AVAL_KTO_NR, AFR_KD) %>%
        dplyr::summarise(CCF_EINZELAVAL_ZAHLUNGEN = sum(BU_BETRAG))
      # Summieren der EAZ je Basiskonto
      eaz_aggregiert_nicht_aval <- eaz_nicht_aval %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, KTO_NR, KTO_QT, AFR_KD) %>%
        dplyr::summarise(CCF_EINZELAVAL_ZAHLUNGEN = sum(BU_BETRAG))
      # Flag für Aval IA ohne Bezugskonto auf Kundenebene
      t_aval_ia_ohne_bzgs_kto <- eaz_nicht_aval %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT) %>%
        dplyr::summarise(
          aval_ia_ohne_bzgs_kto_zaehler =
            sum(dplyr::coalesce(aval_ia_ohne_bzgs_kto_zaehler), 0)
          )

      # Auswahl der Grundgesamtheit für die ein CCF bestimmt wird. Dies sind
      # alle Ausfallreiehen je Konten und Kunde. Kreditkarten (KTO_Klasse
      # 24 und 30) werden ausgesteuert, da diese im Abrechnungskonto
      # berücksichtigt werden.
      gg_ohne_aval_zahlungen <- basis_afr_konto %>%
        dplyr::filter(KTO_KLASSE != 24 & KTO_KLASSE != 30) %>%
        # Anspielen der Kundenbezogenen Informationen
        dplyr::left_join(basis_kunde %>%
                           select(PNR_KD, PNR_KD_QT, KD_KUNDENSYSTEMATIK,
                                  KD_DKB_TEAM, KD_RISIKOVERFAHRENSSCHALTER),
                         by = c("PNR_KD", "PNR_KD_QT")) %>%
        dplyr::left_join(basis_afr_kunde %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                         AFR_KD_ENDE,
                                         KD_BONI_NOTE,
                                         KD_BONI_NOTE_BEW_DAT,
                                         KD_BONI_NOTE_BEW_DIFF),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, AFR_KD_ENDE,
                      KTO_NR, KTO_QT, MIN_AFG_DAT,
                      MIN_AFG_LINIE_tmp = MIN_AFG_LINIE,
                      MIN_AFG_SALDO_tmp = MIN_AFG_SALDO,
                      MIN_AFG_BSVP, ANZAHL_BEW_MIN_AFG,
                      MIN_AFG_DAT_MINUS_1J_LINIE,
                      MIN_AFG_DAT_MINUS_1J_SALDO_tmp =
                      MIN_AFG_DAT_MINUS_1J_SALDO,
                      MIN_AFG_DAT_MINUS_1J_BSVP, MIN_AFG_DAT_MINUS_1J_BEW_DAT,
                      KTO_KLASSE, KTO_GPV, KTO_TILGUNG, KTO_TYP_1,
                      KTO_TYP_2, KTO_ALTER,  KD_KUNDENSYSTEMATIK, KD_DKB_TEAM,
                      KD_RISIKOVERFAHRENSSCHALTER, KTO_LOESCHUNG
                      , KD_BONI_NOTE,
                      KD_BONI_NOTE_BEW_DAT, KD_BONI_NOTE_BEW_DIFF
        ) %>%
        # Berücksichtigung von besonderen Fachvorgaben in den Saldo- und
        # Linienbeträgen, sowie Bestimmung der an den CCF angepassten
        # Kontotypen
        dplyr::mutate(
          # Nur positive Salden und Nullsetzenn des Saldos falls das Konto zum
          # Ausfallzeitpunkt bereits gelöscht wurde.
          MIN_AFG_SALDO = if_else(dplyr::coalesce(MIN_AFG_SALDO_tmp, 0) > 0
                                  & dplyr::coalesce(KTO_LOESCHUNG,
                                             as.Date("01.01.9999",
                                                     format = "%d.%m.%Y"))
                                  > MIN_AFG_DAT,
                                  dplyr::coalesce(MIN_AFG_SALDO_tmp, 0), 0),
          # Nullsetzenn der Linie falls das Konto zum
          # Ausfallzeitpunkt bereits gelöscht wurde
          MIN_AFG_LINIE = if_else(dplyr::coalesce(KTO_LOESCHUNG,
                                           as.Date("01.01.9999",
                                                   format = "%d.%m.%Y"))
                                  > MIN_AFG_DAT,
                                  dplyr::coalesce(MIN_AFG_LINIE_tmp, 0), 0),
          MIN_AFG_DAT_MINUS_1J_SALDO =
            if_else(dplyr::coalesce(MIN_AFG_DAT_MINUS_1J_SALDO_tmp, 0) > 0,
                    dplyr::coalesce(MIN_AFG_DAT_MINUS_1J_SALDO_tmp, 0), 0),
          # Nur Avale, Kontokorrente und Darlehen sind relevant, für sonstige
          # findet keine CCF_Berechnung statt. (Output relevant)
          CCF_KTO_TYP_1 = if_else(KTO_TYP_1 %in%
                                    c("Aval", "Kontokorrent", "Darlehen"),
                                  KTO_TYP_1, "Sonstiges"),
          # Differenzierung Einzelavele, Rahmenavale, widerrufliche und
          # unwiederrufliche Kontokorentkredite sowie unterschiedliche
          # Darlehentypen im Kontotyp 2 (Output relevant)
          CCF_KTO_TYP_2 = case_when(
            KTO_GPV %in% c("Stubifo", "PG_STAND_8", "DKB_AZ_6_05") ~ "Stubifo",
            KTO_KLASSE == "16" ~ "Einzelaval",
            KTO_KLASSE == "1" | KTO_KLASSE == "9" ~ "Sonstiges Darlehen",
            KTO_KLASSE %in% c("4", "28") ~ "widerruflich",
            KTO_KLASSE %in% c("11", "29") ~ "unwiderruflich",
            TRUE ~ KTO_TYP_2)) %>%
        # Anspielen der Kreditkarten
        dplyr::left_join(gg_kreditkarten, by =
                           c("PNR_KD", "PNR_KD_QT", "AFR_KD",
                             "KTO_NR" = "KTO_NR_ABRECHNUNGSKONTO_KK")
        )

      # Anspielen der ermittelten EAZ für Einzelavale und den Basiskonten
       grundgesamtheit_tmp <- gg_ohne_aval_zahlungen %>%
        dplyr::left_join(eaz_aggregiert_einzelaval,
                         by = c("PNR_KD", "PNR_KD_QT", "KTO_NR" = "AVAL_KTO_NR",
                                "AFR_KD")) %>%
        dplyr::left_join(eaz_aggregiert_nicht_aval,
                         by = c("PNR_KD", "PNR_KD_QT", "KTO_NR",
                                "KTO_QT" = "KTO_QT", "AFR_KD")
                         , suffix = c(".BKT", ".Aval"))  %>%
         dplyr::left_join(t_aval_ia_ohne_bzgs_kto,
                          by = c("PNR_KD", "PNR_KD_QT"))  %>%
        dplyr::mutate(
          CCF_EINZELAVAL_ZAHLUNGEN =
            dplyr::coalesce(CCF_EINZELAVAL_ZAHLUNGEN.Aval,
                            CCF_EINZELAVAL_ZAHLUNGEN.BKT),
          Plausi_CCF =
            if_else(CCF_KTO_TYP_2 == "Einzelaval",
                    if_else(dplyr::coalesce(MIN_AFG_DAT_MINUS_1J_BSVP, 0) > 0,
                            dplyr::coalesce(CCF_EINZELAVAL_ZAHLUNGEN, 0)
                            / MIN_AFG_DAT_MINUS_1J_BSVP,
                            0), NA_real_),
          drdate = data_reference_date) %>%
         # Zuweisung der AVAL Aussteuerungs-ID
         dplyr::mutate(
           AVAL_AUSSTEUERUNG_ID = 0,
           AVAL_AUSSTEUERUNG_ID = if_else(Plausi_CCF > 0.9 & Plausi_CCF < 1.1
                                  & dplyr::coalesce(KTO_TILGUNG, "")  == 702,
                                  1, AVAL_AUSSTEUERUNG_ID),
           AVAL_AUSSTEUERUNG_ID = if_else(Plausi_CCF > 0 & Plausi_CCF < 0.95
                                 & dplyr::coalesce(KTO_TILGUNG, "")  == 703, 2,
                                 AVAL_AUSSTEUERUNG_ID),
           AVAL_AUSSTEUERUNG_ID =
             dplyr::if_else(Plausi_CCF == 0
                            & dplyr::coalesce(KTO_TILGUNG, "") %in% c(700, 701),
                            3, AVAL_AUSSTEUERUNG_ID),
           AVAL_AUSSTEUERUNG_ID_Fall_4_flag =
             if_else(AVAL_AUSSTEUERUNG_ID %in% c(1, 2), 1, 0),
         )

       # Fall 4 AVAL_AUSSTEUERUNG_ID - Aufsummieren des Flags auf die Ebene
       # Kunde-Konto
       t_aval_aussteuerung_id_fall_4 <- grundgesamtheit_tmp %>%
         dplyr::filter(AVAL_AUSSTEUERUNG_ID_Fall_4_flag > 0) %>%
         dplyr::group_by(PNR_KD, PNR_KD_QT, KTO_NR, KTO_QT) %>%
         dplyr::summarise(
           AVAL_AUSSTEUERUNG_ID_Fall_4_sum =
             sum(dplyr::coalesce(AVAL_AUSSTEUERUNG_ID_Fall_4_flag, 0)))

       # Join der AVAL_AUSSTEUERUNG_ID Fall 4 Flag Summe und ggf.
       # überschreiben der AVAL_AUSSTEUERUNG_ID
       grundgesamtheit <- grundgesamtheit_tmp %>%
         dplyr::left_join(t_aval_aussteuerung_id_fall_4,
                          by = c("PNR_KD", "PNR_KD_QT", "KTO_NR",
                                 "KTO_QT")) %>%
         dplyr::mutate(
           AVAL_AUSSTEUERUNG_ID =
             dplyr::if_else(!(AVAL_AUSSTEUERUNG_ID %in% c(1, 2))
                            & AVAL_AUSSTEUERUNG_ID < 4
                            & dplyr::coalesce(
                              AVAL_AUSSTEUERUNG_ID_Fall_4_sum, 0)
                            > 0, 4, AVAL_AUSSTEUERUNG_ID),
           AVAL_AUSSTEUERUNG_ID =
             dplyr::if_else(Plausi_CCF == 0 &
                              dplyr::coalesce(KTO_TILGUNG, "")  == 704, 5,
                            AVAL_AUSSTEUERUNG_ID),
           AVAL_AUSSTEUERUNG_ID =
             dplyr::if_else(AVAL_AUSSTEUERUNG_ID == 0 &
                              Plausi_CCF == 0 &
                              dplyr::coalesce(KTO_LOESCHUNG, drdate + 1)
                            > drdate &
                              dplyr::coalesce(AFR_KD_ENDE, drdate) < drdate, 6,
                            AVAL_AUSSTEUERUNG_ID),
           AVAL_AUSSTEUERUNG_ID =
             dplyr::if_else(dplyr::coalesce(KTO_TILGUNG, "") == 799, 7,
                            AVAL_AUSSTEUERUNG_ID),
           AVAL_AUSSTEUERUNG_ID =
             dplyr::if_else(AVAL_AUSSTEUERUNG_ID == 0 &
                              Plausi_CCF == 0 &
                              dplyr::coalesce(KTO_LOESCHUNG, drdate + 1) >
                              drdate &
                              dplyr::coalesce(AFR_KD_ENDE, drdate + 1) >
                              drdate,
                            8, AVAL_AUSSTEUERUNG_ID),
           AVAL_AUSSTEUERUNG_ID =
             dplyr::if_else(dplyr::coalesce(KTO_TILGUNG, "001") == "001"
                            & dplyr::coalesce(KTO_LOESCHUNG, drdate) < drdate,
                            9, AVAL_AUSSTEUERUNG_ID),
           AVAL_AUSSTEUERUNG_ID =
             dplyr::if_else(dplyr::coalesce(KTO_TILGUNG, "") == "705"
                            & dplyr::coalesce(KTO_LOESCHUNG, drdate) < drdate,
                            10, AVAL_AUSSTEUERUNG_ID),
           AVAL_AUSSTEUERUNG_ID =
             if_else(AVAL_AUSSTEUERUNG_ID == 0, 11, AVAL_AUSSTEUERUNG_ID),
           AVAL_AUSSTEUERUNG_ID =
             dplyr::if_else(CCF_KTO_TYP_2 == "Einzelaval",
                            AVAL_AUSSTEUERUNG_ID, NA_real_),
           AVAL_IA_OHNE_BEZUGSKONTO = if_else(
             dplyr::coalesce(aval_ia_ohne_bzgs_kto_zaehler, 0) > 0, TRUE, FALSE)
         )




      ## Berechnugn des CCFs für die unterschiedlichen Kontotypen
      # ---------------------------------------------------------- #

      # Bestimmung des CCFs für Einzelavale
      #_______________________________________
      ccf_einzelavale <- grundgesamtheit %>%
        dplyr::filter(CCF_KTO_TYP_2 == "Einzelaval") %>%
        dplyr::mutate(
          CCF_SALDO = MIN_AFG_SALDO,
          CCF_LINIE = MIN_AFG_LINIE,
          CCF_OFFENE_LINIE = MIN_AFG_LINIE - MIN_AFG_SALDO,
          CCF_AVAL_BSVP = MIN_AFG_BSVP,
          CCF_SALDO_VORJAHR = MIN_AFG_DAT_MINUS_1J_SALDO,
          CCF_LINIE_VORJAHR = MIN_AFG_DAT_MINUS_1J_LINIE,
          CCF_OFFENE_LINIE_VORJAHR =
            MIN_AFG_DAT_MINUS_1J_LINIE - MIN_AFG_DAT_MINUS_1J_SALDO,
          CCF_AVAL_BSVP_VORJAHR = MIN_AFG_DAT_MINUS_1J_BSVP,
          CCF_EINZELAVAL_ZAHLUNGEN =
            dplyr::coalesce(CCF_EINZELAVAL_ZAHLUNGEN, 0)
        ) %>%
        # Sofern relevant Berechnung des Anteils der offenen Linie und/oder des
        # Faktors der Linienausweitung
        dplyr::mutate(
          CCF_ANTEIL_OFFENE_LINIE = NA,
          CCF_FAKTOR_LINIENAUSWEITUNG =
            if_else(CCF_AVAL_BSVP >= 0 & CCF_AVAL_BSVP_VORJAHR > 0,
                    round((CCF_AVAL_BSVP / CCF_AVAL_BSVP_VORJAHR), 3), 0)) %>%
        # CCF Berechnung für Einzelavale (CCF 2 nicht relevant)
        dplyr::mutate(
          KTO_CCF = if_else(CCF_AVAL_BSVP_VORJAHR > 0,
                            round((CCF_EINZELAVAL_ZAHLUNGEN) /
                                    (CCF_AVAL_BSVP_VORJAHR), 4),
                            NA_real_),
          KTO_CCF_2 = NA)


      # Bestimmung des CCFs für Rahmenavale
      ##_______________________________________
      ccf_rahmenavale <- grundgesamtheit %>%
        dplyr::filter(CCF_KTO_TYP_2 == "Rahmenaval") %>%
        dplyr::mutate(
          CCF_SALDO = MIN_AFG_SALDO,
          CCF_LINIE = MIN_AFG_LINIE,
          CCF_OFFENE_LINIE = MIN_AFG_LINIE - MIN_AFG_SALDO,
          CCF_AVAL_BSVP = MIN_AFG_BSVP,
          CCF_SALDO_VORJAHR = MIN_AFG_DAT_MINUS_1J_SALDO,
          CCF_LINIE_VORJAHR = MIN_AFG_DAT_MINUS_1J_LINIE,
          CCF_OFFENE_LINIE_VORJAHR =
            MIN_AFG_DAT_MINUS_1J_LINIE - MIN_AFG_DAT_MINUS_1J_SALDO,
          CCF_AVAL_BSVP_VORJAHR = MIN_AFG_DAT_MINUS_1J_BSVP
        ) %>%
        # Sofern relevant Berechnung des Anteils der offenen Linie und/oder des
        # Faktors der Linienausweitung
        dplyr::mutate(
          CCF_ANTEIL_OFFENE_LINIE = NA,
          CCF_FAKTOR_LINIENAUSWEITUNG =
            if_else(CCF_AVAL_BSVP >= 0 & CCF_AVAL_BSVP_VORJAHR > 0,
                    round((CCF_AVAL_BSVP / CCF_AVAL_BSVP_VORJAHR), 3), 0)) %>%
        # CCF Berechnung für Rahmenavale (CCF 2 nicht relevant)
        dplyr::mutate(
          KTO_CCF = if_else(CCF_AVAL_BSVP_VORJAHR > 0,
                            round((CCF_AVAL_BSVP - CCF_AVAL_BSVP_VORJAHR) /
                                    (CCF_AVAL_BSVP_VORJAHR), 4),
                            #(CCF_LINIE_VORJAHR - CCF_AVAL_BSVP_VORJAHR), 4),
                            NA_real_),
          KTO_CCF_2 = NA)


      # Bestimmung des CCFs für Darlehen
      #_______________________________________
      ccf_darlehen <- grundgesamtheit %>%
        dplyr::filter(CCF_KTO_TYP_1 == "Darlehen") %>%
        dplyr::mutate(
          CCF_SALDO = MIN_AFG_SALDO,
          CCF_LINIE = MIN_AFG_LINIE,
          CCF_OFFENE_LINIE = MIN_AFG_LINIE - MIN_AFG_SALDO,
          CCF_AVAL_BSVP = MIN_AFG_BSVP,
          CCF_SALDO_VORJAHR = MIN_AFG_DAT_MINUS_1J_SALDO,
          CCF_LINIE_VORJAHR = MIN_AFG_DAT_MINUS_1J_LINIE,
          CCF_OFFENE_LINIE_VORJAHR =
            MIN_AFG_DAT_MINUS_1J_LINIE - MIN_AFG_DAT_MINUS_1J_SALDO,
          CCF_AVAL_BSVP_VORJAHR = MIN_AFG_DAT_MINUS_1J_BSVP,
          CCF_EINZELAVAL_ZAHLUNGEN =
            dplyr::coalesce(CCF_EINZELAVAL_ZAHLUNGEN, 0)
        ) %>%
        # Sofern relevant Berechnung des Anteils der offenen Linie und/oder des
        # Faktors der Linienausweitung
        dplyr::mutate(
          CCF_ANTEIL_OFFENE_LINIE =
            if_else(CCF_LINIE_VORJAHR > 0,
                    round((CCF_OFFENE_LINIE_VORJAHR / CCF_LINIE_VORJAHR), 7)
                    , 0),
          CCF_FAKTOR_LINIENAUSWEITUNG =
            if_else(CCF_LINIE >= 0 & CCF_LINIE_VORJAHR > 0,
                    round((CCF_LINIE / CCF_LINIE_VORJAHR), 3), 0)) %>%
        # CCF Berechnung für Darlehen (CCF 2 nicht relevant)
        dplyr::mutate(
          KTO_CCF =
            dplyr::if_else(
              CCF_OFFENE_LINIE_VORJAHR > 0,
              round((CCF_SALDO - CCF_SALDO_VORJAHR -
                       dplyr::coalesce(CCF_EINZELAVAL_ZAHLUNGEN, 0)) /
                      (CCF_OFFENE_LINIE_VORJAHR), 4),
              NA_real_),
          KTO_CCF_2 = NA)

      # Bestimmung des CCFs für Kontokorrent (mit und ohne Linienausweitung)
      #_____________________________________________________________________
      ccf_kontokorrent <- grundgesamtheit %>%
        dplyr::filter(CCF_KTO_TYP_1 == "Kontokorrent") %>%
        dplyr::mutate(
          CCF_LINIE = MIN_AFG_LINIE + dplyr::coalesce(KARTE_CCF_LINIE, 0),
          CCF_SALDO = MIN_AFG_SALDO + dplyr::coalesce(KARTE_CCF_SALDO, 0),
          CCF_OFFENE_LINIE = pmax(MIN_AFG_LINIE - MIN_AFG_SALDO, 0)
          + dplyr::coalesce(KARTE_CCF_OFFENE_LINIE, 0),
          CCF_AVAL_BSVP = MIN_AFG_BSVP,
          CCF_LINIE_VORJAHR = MIN_AFG_DAT_MINUS_1J_LINIE +
            dplyr::coalesce(KARTE_CCF_LINIE_VORJAHR, 0),
          CCF_SALDO_VORJAHR = MIN_AFG_DAT_MINUS_1J_SALDO +
            dplyr::coalesce(KARTE_CCF_SALDO_VORJAHR, 0),
          CCF_OFFENE_LINIE_VORJAHR =
            pmax(MIN_AFG_DAT_MINUS_1J_LINIE - MIN_AFG_DAT_MINUS_1J_SALDO, 0)
          + dplyr::coalesce(KARTE_CCF_OFFENE_LINIE_VORJAHR, 0),
          CCF_AVAL_BSVP_VORJAHR = MIN_AFG_DAT_MINUS_1J_BSVP,
          CCF_EINZELAVAL_ZAHLUNGEN =
            dplyr::coalesce(CCF_EINZELAVAL_ZAHLUNGEN, 0)
        ) %>%
        # Sofern relevant Berechnung des Anteils der offenen Linie und/oder des
        # Faktors der Linienausweitung
        dplyr::mutate(
          CCF_ANTEIL_OFFENE_LINIE =
            if_else(CCF_LINIE_VORJAHR > 0,
                    round((CCF_OFFENE_LINIE_VORJAHR / CCF_LINIE_VORJAHR), 7)
                    , 0),
          CCF_FAKTOR_LINIENAUSWEITUNG =
            if_else(CCF_LINIE >= 0 & CCF_LINIE_VORJAHR > 0,
                    round((CCF_LINIE / CCF_LINIE_VORJAHR), 3), 0)) %>%
        # CCF Berechnung für Kontokorrent (CCF 2 relevant für
        # Kontokorrentkonten mit Linienausweitung)
        dplyr::mutate(
          KTO_CCF =
            dplyr::if_else(
              CCF_OFFENE_LINIE_VORJAHR > 0,
              round((CCF_SALDO - CCF_SALDO_VORJAHR -
                       dplyr::coalesce(CCF_EINZELAVAL_ZAHLUNGEN, 0)
                     ) / (CCF_OFFENE_LINIE_VORJAHR), 4),
              NA_real_),
          KTO_CCF_2 =
            dplyr::if_else(
              CCF_FAKTOR_LINIENAUSWEITUNG > 1,
              dplyr::if_else(
                CCF_OFFENE_LINIE_VORJAHR > 0,
                round((CCF_SALDO - CCF_SALDO_VORJAHR -
                         dplyr::coalesce(CCF_EINZELAVAL_ZAHLUNGEN, 0))
                      / (CCF_LINIE - CCF_SALDO_VORJAHR), 4),
                NA_real_)
              , NA_real_))
      # Zusammenführen indivudell nach Kontotyp bestimmten CCF-Berechnungen in
      # die Zieltabelle und Auswahl der Datenfelder
      parameter_ccf <- dplyr::union_all(ccf_einzelavale,
                                                ccf_rahmenavale) %>%
        dplyr::union_all(ccf_darlehen) %>%
        dplyr::union_all(ccf_kontokorrent) %>%
        # Update NA-Werte
        dplyr::mutate(
          KARTE_ANZAHL = tidyr::replace_na(KARTE_ANZAHL, 0)
        ) %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, KTO_NR, KTO_QT, MIN_AFG_DAT,
                      CCF_KTO_TYP_1, CCF_KTO_TYP_2, MIN_AFG_DAT, KTO_CCF,
                      KTO_CCF_2,
                      CCF_LINIE, CCF_SALDO, CCF_OFFENE_LINIE, CCF_AVAL_BSVP,
                      CCF_LINIE_VORJAHR, CCF_SALDO_VORJAHR,
                      CCF_OFFENE_LINIE_VORJAHR,
                      CCF_AVAL_BSVP_VORJAHR, CCF_EINZELAVAL_ZAHLUNGEN,
                      MIN_AFG_DAT_MINUS_1J_BEW_DAT,
                      KARTE_ANZAHL, KARTE_CCF_LINIE, KARTE_CCF_SALDO,
                      KARTE_CCF_OFFENE_LINIE, KARTE_CCF_LINIE_VORJAHR,
                      KARTE_CCF_SALDO_VORJAHR, KARTE_CCF_OFFENE_LINIE_VORJAHR,
                      KARTE_CCF_MIN_M1J_BEW_DAT, KARTE_CCF_MAX_M1J_BEW_DAT,
                      KD_RISIKOVERFAHRENSSCHALTER, KD_DKB_TEAM, KTO_TILGUNG,
                      KTO_ALTER,
                      KD_BONI_NOTE, KD_BONI_NOTE_BEW_DAT, KD_BONI_NOTE_BEW_DIFF,
                      KTO_KLASSE, KTO_TILGUNG, KTO_ALTER,
                      CCF_ANTEIL_OFFENE_LINIE, CCF_FAKTOR_LINIENAUSWEITUNG,
                      AVAL_AUSSTEUERUNG_ID, AVAL_IA_OHNE_BEZUGSKONTO
        )

      # 4. Schritt: Data-Dictionary für Parametertabelle ####
      create_data_dictionary(vdkx,
                             data = parameter_ccf,
                             data_description =
                               get_col_tab_description(vdkx, tablename),
                             tablename = tablename)

      # 5. Schritt: Speichern / Protokollierung Parametertabelle ####
      arrow::write_parquet(parameter_ccf,
                           file.path(path_exchange,
                                     paste0(tablename, ".parquet")))
      insert_table_protocol(vdkx, tablename, nrow(parameter_ccf))
    } else {
      loginfo(paste(vdkx$info_msg_create_basis_not_done, tablename))
    }
  }
  logging::loginfo(paste(vdkx$info_msg_function_end,
                         create_info_string(),
                         "create_parameter_ccf"))
}


#' Erstellt die Parametertabelle VQ
#'
#' In dieser Funktion wird die Parametertabelle VQ erstellt und das
#' Data-Dictionary für diese Tabelle wird befüllt.
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
#' @param fmp_reference_date \code{Date} mit dem Datum, welches als Stichtag
#' zur Bestimmung der FMP-Verlustdifferenz verwendet wird.
#' @family vdkx_parameter
create_parameter_vq <- function(vdkx,
                                tablename = NULL,
                                path_exchange = NULL,
                                path_input = NULL,
                                fmp_reference_date = NULL) {

  logging::loginfo("=========================================================")
  if (vdkx$used_loglevel %in% c("INFO", "DEBUG")) {
    tictoc::tic(" create_parameter_vq")
    }
  logging::loginfo(paste(vdkx$info_msg_function_begin,
                         "create_parameter_vq"))
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
    input_plz_mapping <-
      import_parquet_file(vdkx,
                          path_input,
                          filename = "INPUT_VMO_IMMO_PLZ_Mapping.parquet",
                          dq_string = "PARAMETER_VQ")
    input_mpzr <-
      import_parquet_file(vdkx,
                          path_input,
                          filename = "INPUT_VMO_IMMO_MPZR.parquet",
                          dq_string = "PARAMETER_VQ")
    vdkx_basis_vmo <-
      import_parquet_file(vdkx,
                          path_exchange,
                          filename = "T_1350_VMO.parquet",
                          dq_string = "PARAMETER_VQ")
    vdkx_basis_vmo_zahl <-
      import_parquet_file(vdkx,
                          path_exchange,
                          filename = "T_1330_VMO_Zahlungen.parquet",
                          dq_string = "PARAMETER_VQ")

    if (state_function != vdkx$state_error
        & !is.null(input_plz_mapping)
        & !is.null(input_mpzr)
        & !is.null(vdkx_basis_vmo)
        & !is.null(vdkx_basis_vmo_zahl)
        & !is.null(vdkx[["SVZ_VMO_ZAHL_KAT"]])
        & !is.null(vdkx[["v_VDH_Mapping_VMO_Klasse_zu_vdp_Typ"]])
        & !is.null(vdkx[["v_VDH_Schluessel_VMO_Klasse"]])) {
      # 2. Schritt: Data-Dictionary für Input-Dateien ####
      fill_dd_input_table(vdkx,
                          data = input_plz_mapping,
                          tablename = "INPUT_VMO_IMMO_PLZ_Mapping")
      fill_dd_input_table(vdkx,
                          data = input_mpzr,
                          tablename = "INPUT_VMO_IMMO_MPZR")
      fill_dd_input_table(vdkx,
                          data = vdkx_basis_vmo,
                          tablename = "T_1350_VMO")
      fill_dd_input_table(vdkx,
                          data = vdkx_basis_vmo_zahl,
                          tablename = "T_1330_VMO_Zahlungen")
      fill_dd_input_table(vdkx,
                          data = vdkx[["SVZ_VMO_ZAHL_KAT"]],
                          tablename = "SVZ_VMO_ZAHL_KAT")
      fill_dd_input_table(vdkx,
                          data = vdkx[["v_VDH_Mapping_VMO_Klasse_zu_vdp_Typ"]],
                          tablename = "v_VDH_Mapping_VMO_Klasse_zu_vdp_Typ")
      fill_dd_input_table(vdkx,
                          data = vdkx[["v_VDH_Schluessel_VMO_Klasse"]],
                          tablename = "v_VDH_Schluessel_VMO_Klasse")

      # 3. Schritt: Parametertabelle erzeugen ####

      # Vorberechnungen #
      #_________________#

      # 3.1 BVQ_IMMO: Bestimmung der Ausgangswerte ####
      #________________________________________________
      logging::loginfo("Bestimmung der Ausgangswerte für die BVQ_IMMO")

      # die nachfolgende Tabelle enthält nur noch den MPI des größten
      # Bewertungsjahres pro DSGV_Typen und Marktpreis_GKZ Kombination
      input_mpzr_max_bew_jahr <- input_mpzr %>%
        dplyr::select(DSGV_Typen, Marktpreis_GKZ,
                      Bewertungsjahr, MPI) %>%
        # Das größte Jahr steht vorne
        dplyr::arrange(DSGV_Typen, Marktpreis_GKZ,
                       desc(Bewertungsjahr)) %>%
        dplyr::group_by(DSGV_Typen, Marktpreis_GKZ) %>%
        dplyr::summarise(
          MPI_MAX_BEWERTUNGSJAHR = first(MPI)
        ) %>%
        dplyr::ungroup()

      # die nachfolgende Tabelle enthält nur noch das Bundesland und das
      # GKZ des größten Verwertungsjahres
      input_plz_mapping_max_v_jahr <- input_plz_mapping %>%
        # Das größte Jahr steht vorne
        dplyr::arrange(PLZ5stellig, desc(Verwertungsjahr)) %>%
        dplyr::group_by(PLZ5stellig) %>%
        dplyr::summarise(
          BUNDESLAND_MAX_V_JAHR = first(PLZ_Bundesland),
          GKZ_MAX_V_JAHR = first(PLZ_GKZeichen),
          PLZ_Klasse_MAX_V_JAHR = first(PLZ_Klasse)
        ) %>%
        dplyr::ungroup()

      bvq_immo <- vdkx_basis_vmo %>%
        # Bestimmung des Bewertungs- und Verwertungsjahres
        dplyr::mutate(
          VMO_V_O_BIS_DAT = dplyr::case_when(
            !is.na(.$VMO_V_DAT) ~ .$VMO_V_DAT,
            !is.na(.$VMO_BIS) ~ .$VMO_BIS,
            TRUE ~ NA_Date_)
        ) %>%
        dplyr::mutate(
          VMO_MW_JAHR = as.integer(lubridate::year(VMO_MW_DAT)),
          VMO_V_JAHR = as.integer(lubridate::year(VMO_V_O_BIS_DAT))
        ) %>%
        # Berechnung Alter des relevanten Marktwerts zum Verwertungsdatum
        dplyr::mutate(
          VMO_ALTER_MW = (VMO_V_O_BIS_DAT - VMO_MW_DAT) / 365 * 12
        ) %>%

        # Anspielen Bundesland und GKZ
        # a) VMO_V_JAHR befüllt
        dplyr::left_join(input_plz_mapping %>%
                           dplyr::rename(Bundesland_1 = PLZ_Bundesland,
                                         GKZ_1 = PLZ_GKZeichen),
                         by = c("IMMO_PLZ" = "PLZ5stellig",
                                "VMO_V_JAHR" = "Verwertungsjahr")) %>%
        # b) VMO_V_JAHR nicht befüllt
        dplyr::left_join(input_plz_mapping_max_v_jahr %>%
                           dplyr::rename(Bundesland_2 = BUNDESLAND_MAX_V_JAHR,
                                         GKZ_2 = GKZ_MAX_V_JAHR),
                         by = c("IMMO_PLZ" = "PLZ5stellig")) %>%
        dplyr::mutate(
          IMMO_BUNDESLAND = dplyr::case_when(
            .$VMO_TYP_1 == "IMMO" & !is.na(.$Bundesland_1) ~ .$Bundesland_1,
            .$VMO_TYP_1 == "IMMO" & !is.na(.$Bundesland_2) ~ .$Bundesland_2,
            TRUE ~ NA_character_
          ),
          GKZ = dplyr::case_when(
            .$VMO_TYP_1 == "IMMO" & !is.na(.$GKZ_2) ~ .$GKZ_2,
            TRUE ~ NA_character_
          )) %>%
        # Anspielen der Marktpreiszeitreihenindizes für VMO_MW_JAHR
        # a) VMO_MW_JAHR <= max(Bewertungsjahr)
        dplyr::left_join(input_mpzr %>%
                           dplyr::rename(MPZR_INDEX_MW_JAHR_1 = MPI),
                         by = c("VMO_KLASSE" = "DSGV_Typen",
                                "GKZ" = "Marktpreis_GKZ",
                                "VMO_MW_JAHR" = "Bewertungsjahr")) %>%
        # a) VMO_MW_JAHR > max(Bewertungsjahr)
        dplyr::left_join(input_mpzr_max_bew_jahr %>%
                           dplyr::rename(
                             MPZR_INDEX_MW_JAHR_2 = MPI_MAX_BEWERTUNGSJAHR),
                         by = c("VMO_KLASSE" = "DSGV_Typen",
                                "GKZ" = "Marktpreis_GKZ")) %>%
        dplyr::mutate(
          MPZR_INDEX_MW_JAHR = dplyr::case_when(
            (.$VMO_TYP_1 == "IMMO" & is.na(.$VMO_MW_JAHR)) |
              .$VMO_TYP_1 != "IMMO" ~ NA_real_,
            .$VMO_TYP_1 == "IMMO" & !is.na(.$MPZR_INDEX_MW_JAHR_1)
            ~ .$MPZR_INDEX_MW_JAHR_1,
            .$VMO_TYP_1 == "IMMO" & !is.na(.$MPZR_INDEX_MW_JAHR_2)
            ~ .$MPZR_INDEX_MW_JAHR_2
          )) %>%
        # Anspielen der Marktpreiszeitreihenindizes für VMO_V_JAHR
        # a) VMO_V_JAHR <= max(Bewertungsjahr)
        dplyr::left_join(input_mpzr %>%
                           dplyr::rename(MPZR_INDEX_V_JAHR_1 = MPI),
                         by = c("VMO_KLASSE" = "DSGV_Typen",
                                "GKZ" = "Marktpreis_GKZ",
                                "VMO_V_JAHR" = "Bewertungsjahr")) %>%
        # b) VMO_V_JAHR > max(Bewertungsjahr)
        dplyr::left_join(input_mpzr_max_bew_jahr %>%
                           dplyr::rename(
                             MPZR_INDEX_V_JAHR_2 = MPI_MAX_BEWERTUNGSJAHR),
                         by = c("VMO_KLASSE" = "DSGV_Typen",
                                "GKZ" = "Marktpreis_GKZ")) %>%
        dplyr::mutate(
          MPZR_INDEX_V_JAHR = dplyr::case_when(
            (.$VMO_TYP_1 == "IMMO" & is.na(.$VMO_V_JAHR)) |
              .$VMO_TYP_1 != "IMMO" ~ NA_real_,
            .$VMO_TYP_1 == "IMMO" & !is.na(.$MPZR_INDEX_V_JAHR_1)
            ~ .$MPZR_INDEX_V_JAHR_1,
            .$VMO_TYP_1 == "IMMO" & !is.na(.$MPZR_INDEX_V_JAHR_2)
            ~ .$MPZR_INDEX_V_JAHR_2
          )) %>%
        # Bestimmung VMO_BASISWERT
        dplyr::mutate(
          VMO_BASISWERT = dplyr::case_when(
            .$VMO_TYP_1 == "IMMO" & .$VMO_TYP_2 ==
              "nicht fertig gestelltes Objekt (Ziel-ImmoTyp zuordnen)" ~
              .$VMO_MW_BETRAG,
            .$VMO_TYP_1 == "IMMO" ~
              (.$MPZR_INDEX_V_JAHR / .$MPZR_INDEX_MW_JAHR) * .$VMO_MW_BETRAG,
            .$VMO_TYP_1 %in% c("BARS", "EVTF") ~ .$VMO_MW_BETRAG
          )
        ) %>%
        # Auswahl der neuen Spalten
        dplyr::select(VMO_CODE, VMO_QT, IMMO_BUNDESLAND, VMO_V_O_BIS_DAT,
                      VMO_MW_JAHR, VMO_ALTER_MW, MPZR_INDEX_MW_JAHR, VMO_V_JAHR,
                      MPZR_INDEX_V_JAHR, VMO_BASISWERT)

      # 3.2 KQ_IMMO / TKQ_IMMO / NVQ_BARS / TNVQ_BARS: ####
      # Bestimmung der relevanten Kosten und Erlöse
      #____________________________________________________
      msg <- paste("Bestimmung der relevanten Kosten und Erlöse für die",
                   "KQ_IMMO, TKQ_IMMO, NVQ_BARS und TNVQ_BARS")
      logging::loginfo(msg)

      kq_immo_nvq_bars <- vdkx_basis_vmo_zahl %>%
        # Anspielen der Verwendungsflags für den Risikoparameter
        dplyr::left_join(vdkx$SVZ_VMO_ZAHL_KAT,
                         by = c("VMO_ZAHL_KAT_ID" = "ID")) %>%
        # Anspielen des VMO_TYP_1 und VMO_MIN_AFG_DAT
        dplyr::left_join(vdkx_basis_vmo %>%
                           dplyr::select(VMO_CODE, VMO_QT, VMO_TYP_1,
                                         VMO_MIN_AFG_DAT),
                         by = c("VMO_CODE", "VMO_QT")) %>%
        # Einschränken, auf Zahlungen, die für die KQ_IMMO und NVQ_BARS
        # relevant sind
        dplyr::filter(
          (VMO_TYP_1 == "IMMO" & KQ_IMMO == TRUE) |
            (VMO_TYP_1 == "BARS" & NVQ_BARS == TRUE)) %>%
        # Einteilen in Kosten und Erlöse
        dplyr::mutate(
          VORZEICHEN = dplyr::if_else(
            (ERLOES == TRUE & VMO_ZAHL_S_H == "S") |
              (ERLOES == FALSE & VMO_ZAHL_S_H == "H"), -1, 1),
          KOSTEN = dplyr::if_else(ERLOES == FALSE, VORZEICHEN *
                                    VMO_ZAHL_BETRAG, 0),
          ERLOES = dplyr::if_else(ERLOES == TRUE, VORZEICHEN *
                                    VMO_ZAHL_BETRAG, 0),
          FLAG_IS_BEW = dplyr::if_else(
            VMO_TYP_1 == "IMMO" & VMO_ZAHL_KAT_ID %in% c(10, 11), 1, 0),
          BEW = dplyr::if_else(
            VMO_TYP_1 == "IMMO" & VMO_ZAHL_KAT_ID %in% c(10, 11),
            VORZEICHEN * VMO_ZAHL_BETRAG, NA_real_),
          ZAHL_ZEITPUNKT = dplyr::case_when(
            !is.na(.$VMO_ZAHL_VALUTA) ~ .$VMO_ZAHL_VALUTA,
            TRUE ~ .$VMO_ZAHL_DAT
          )
        ) %>%
        dplyr::group_by(VMO_CODE, VMO_QT) %>%
        dplyr::summarise(
          SUMME_KOSTEN = sum(KOSTEN),
          SUMME_ERLOESE = sum(ERLOES),
          SUMME_DAUER_KOSTEN_GEWICHTET = sum(KOSTEN * as.numeric(
            (ZAHL_ZEITPUNKT - VMO_MIN_AFG_DAT))),
          SUMME_DAUER_ERLOESE_GEWICHTET = sum(ERLOES * as.numeric(
            (ZAHL_ZEITPUNKT - VMO_MIN_AFG_DAT))),
          SUMME_BEW = sum(BEW, na.rm = TRUE),
          ANZ_BEW = sum(FLAG_IS_BEW),
          MIN_BEW = if (all(is.na(BEW))) NA_real_ else
            min(BEW, na.rm = TRUE),
          MAX_BEW = if (all(is.na(BEW))) NA_real_ else
            max(BEW, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          MAX_ABW_BEW = dplyr::if_else(ANZ_BEW > 1, MAX_BEW - MIN_BEW,
                                       NA_real_)
        )

      # 3.3 TDVZ_IMMO: Bestimmung der relevanten Zahlungen ####
      #________________________________________________________
      logging::loginfo("Bestimmung der relevanten Zahlungen für die TDVZ_IMMO")

      tdvz_immo <- vdkx_basis_vmo_zahl %>%
        # Anspielen der Verwendungsflags für den Risikoparameter
        dplyr::left_join(vdkx$SVZ_VMO_ZAHL_KAT,
                         by = c("VMO_ZAHL_KAT_ID" = "ID")) %>%
        # Anspielen des VMO_TYP_1
        dplyr::left_join(vdkx_basis_vmo %>%
                           dplyr::select(VMO_CODE, VMO_QT, VMO_TYP_1),
                         by = c("VMO_CODE", "VMO_QT")) %>%
        # Einschränken aus Immobilien und auf Zahlungen, die für die
        # TDVZ_IMMO relevant sind
        dplyr::filter(VMO_TYP_1 == "IMMO" & TDVZ_IMMO == TRUE) %>%
        dplyr::mutate(
          VORZEICHEN = dplyr::if_else(
            (ERLOES == TRUE & VMO_ZAHL_S_H == "S") |
              (ERLOES == FALSE & VMO_ZAHL_S_H == "H"), -1, 1),
          ZAHL_ZEITPUNKT = dplyr::case_when(
            !is.na(.$VMO_ZAHL_VALUTA) ~ .$VMO_ZAHL_VALUTA,
            TRUE ~ .$VMO_ZAHL_DAT
          )
          ) %>%
        dplyr::group_by(VMO_CODE, VMO_QT) %>%
        dplyr::summarise(
          SUMME_VZ = sum(VORZEICHEN * VMO_ZAHL_BETRAG),
          SUMME_VZ_GEW = sum(VORZEICHEN * VMO_ZAHL_BETRAG *
                               as.numeric(ZAHL_ZEITPUNKT))
        ) %>%
        dplyr::ungroup()

      # 3.4 FMP-Felder  ####
      #_____________________
      logging::loginfo("Bestimmung der FMP-Felder")

      # 3.4.1 Berechnung Zwischenergebnisse auf VMO-Ebene
      #__________________________________________________
      # - VMO_FMP_VD_AVG_ZAHL_DAT
      # - ZAHL_SUMME
      # - FMP_KOSTEN_DIREKT
      #
      fmp_vmo_zahl <- vdkx_basis_vmo_zahl %>%
        # Anspielen der VMO_V_ART
        dplyr::left_join(vdkx_basis_vmo %>%
                           dplyr::select(VMO_CODE, VMO_QT, VMO_V_ART),
                         by = c("VMO_CODE", "VMO_QT")) %>%
        # Anspielen der Info ob VMO-Zahlung Erlös oder Kosten
        dplyr::left_join(vdkx$SVZ_VMO_ZAHL_KAT,
                         by = c("VMO_ZAHL_KAT_ID" = "ID")) %>%
        dplyr::mutate(
          VORZEICHEN = dplyr::if_else(
            (ERLOES == TRUE & VMO_ZAHL_S_H == "S") |
              (ERLOES == FALSE & VMO_ZAHL_S_H == "H"), -1, 1),
          FMP_ZAHL_DAT = dplyr::if_else(
            VMO_ZAHL_KLASSE == 452, VMO_ZAHL_DAT, NA_Date_),
          FMP_ZAHL = dplyr::if_else(
            VMO_ZAHL_KLASSE == 452, VMO_ZAHL_BETRAG, NA_real_),
          FMP_KOSTEN = dplyr::if_else(
            VMO_ZAHL_KAT_ID == 11 & VMO_V_ART %in% c(11, 12, 13),
            VMO_ZAHL_BETRAG, NA_real_)
        ) %>%
        dplyr::group_by(VMO_CODE, VMO_QT) %>%
        dplyr::summarise(
          # Durchschnitt über VMO_ZAHL_DAT, NAs werden ignoriert
          VMO_FMP_VD_AVG_ZAHL_DAT = mean(FMP_ZAHL_DAT, na.rm = TRUE),
          ZAHL_SUMME = sum(FMP_ZAHL * VORZEICHEN, na.rm = TRUE),
          FMP_KOSTEN_DIREKT = sum(VORZEICHEN * FMP_KOSTEN, na.rm = TRUE)
          ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          # Formatierung als Datum
          VMO_FMP_VD_AVG_ZAHL_DAT = trunc(as.Date(
            dplyr::if_else(is.na(VMO_FMP_VD_AVG_ZAHL_DAT),
                           NA_Date_,  VMO_FMP_VD_AVG_ZAHL_DAT)))
        )

      # 3.4.2 Berechnung finale Ergebnisse
      #___________________________________
      #
      # - VMO_FMP_VD_AVG_ZAHL_DAT
      # - VMO_FMP_VD_BETRAG_REAL
      # - VMO_FMP_VD_ANMERK
      # - VMO_FMP_VD_BETRAG_PROG
      #
      fmp_vmo <- vdkx_basis_vmo %>%
        # Anspielen der Zwischenergebnisse
        dplyr::left_join(fmp_vmo_zahl,
                         by = c("VMO_CODE", "VMO_QT")) %>%
        # Anspielen VMO-Basiswert
        dplyr::left_join(bvq_immo %>%
                           dplyr::select(VMO_CODE, VMO_QT, VMO_BASISWERT),
                         by = c("VMO_CODE", "VMO_QT")) %>%
        dplyr::mutate(
          # Ersetze NA-Werte
          ZAHL_SUMME = tidyr::replace_na(ZAHL_SUMME, 0),
          VMO_BASISWERT = tidyr::replace_na(VMO_BASISWERT, 0),
          # Berechnung der realisierten FMP-VD
          FMP_KOSTEN_INTERN = 80 *
            (as.numeric(VMO_V_DAT - VMO_FMP_VD_AVG_ZAHL_DAT) / 365.25 * 12),
          VMO_FMP_VD_BETRAG_REAL = dplyr::case_when(
            VMO_V_ART %in% c(11, 12, 13, 19) & ZAHL_SUMME > 100 & VMO_GOVE >= 0
            ~ round(VMO_GOVE - FMP_KOSTEN_DIREKT - FMP_KOSTEN_INTERN -
                      ZAHL_SUMME, 2),
            TRUE ~ NA_real_),
          # Bestimmung der VMO_FMP_VD_ANMERK
          VMO_FMP_VD_ANMERK = dplyr::case_when(
            # Nur befüllt für Immobilien, die an die FMP verkauft wurden
            !(VMO_TYP_1 == "IMMO" & VMO_V_ART >= 10 & VMO_V_ART <= 19)
            ~ NA_character_,
            VMO_VORLAST_KNZ == TRUE &
              (VMO_V_ART == 10 |
                 (VMO_V_ART > 10 & VMO_V_ART <= 19 &
                    (VMO_GOVE < 0 | (VMO_GOVE > 0 & ZAHL_SUMME <= VMO_GOVE))))
            ~ "X: GS-Vorlast gegeben",
            VMO_V_ART %in% c(11, 12, 13) &
              ZAHL_SUMME > 100 & VMO_GOVE > 100
            ~ "R: Verwertungsfall",
            VMO_V_ART == 19 & ZAHL_SUMME > 100 &
              VMO_GOVE == 0
            ~ "R: ohne Verwertungserfolg",
            VMO_V_ART %in% c(11, 12,  13) & VMO_GOVE == -1
            ~ "X: GOVE unbekannt",
            tidyr::replace_na(VMO_BASISWERT, 0) > 100 & VMO_V_ART == 10
            ~ "S: Verwertung ausstehend",
            tidyr::replace_na(VMO_BASISWERT, 0) > 100 & VMO_V_ART == 14
            ~ "S: Forderungsverwertung",
            tidyr::replace_na(VMO_BASISWERT, 100) <= 100 &
              VMO_V_ART %in% c(10, 14)
            ~ "DQB: keine S - Basiswert fehlt",
            TRUE ~ "DQB: ohne Kennzeichnung")
        ) %>%
        dplyr::mutate(
          # DQB-Update der VMO_FMP_VD_ANMERK
          VMO_FMP_VD_ANMERK = dplyr::if_else(
            VMO_FMP_VD_ANMERK == "R: Verwertungsfall"
            & ((VMO_FMP_VD_BETRAG_REAL
                / dplyr::coalesce(VMO_BASISWERT, 0) > 0.5
                & dplyr::coalesce(VMO_BASISWERT, 0) > 100) |
                 (VMO_FMP_VD_BETRAG_REAL
                  / dplyr::coalesce(VMO_BASISWERT, 0) > 0.4
                  & dplyr::coalesce(VMO_BASISWERT, 0)
                  > 200000)),
            "DQB: FMP-VD vs. Basiswert", VMO_FMP_VD_ANMERK),
          # Bestimmung der FMP-VD-Quote
          #____________________________
          ref_dat_5_jahre =
            lubridate::floor_date(fmp_reference_date %m-%
                                    lubridate::years(5), "year"),
          ref_dat_4_jahre =
            lubridate::floor_date(fmp_reference_date %m-%
                                    lubridate::years(4), "year"),
          EINZELFALL_QUOTE = dplyr::if_else(
            !is.na(VMO_FMP_VD_ANMERK) &
              substr(VMO_FMP_VD_ANMERK, 1, 2) == "R:" & VMO_BASISWERT > 100 &
              VMO_FMP_VD_AVG_ZAHL_DAT > ref_dat_5_jahre &
              VMO_V_DAT > ref_dat_4_jahre, VMO_FMP_VD_BETRAG_REAL /
              VMO_BASISWERT, NA_real_)
        ) %>%
        dplyr::group_by() %>%
        dplyr::mutate(
          FMP_VD_QUOTE_REAL = mean(EINZELFALL_QUOTE, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        # Schätzung der FMP-VD
        #_____________________
        dplyr::mutate(
          VMO_FMP_VD_BETRAG_PROG = round(FMP_VD_QUOTE_REAL * VMO_BASISWERT, 2)
          ) %>%
        dplyr::select(VMO_CODE, VMO_QT,
                      VMO_FMP_VD_AVG_ZAHL_DAT, VMO_FMP_VD_BETRAG_REAL,
                      VMO_FMP_VD_ANMERK, VMO_FMP_VD_BETRAG_PROG)


      # 3.5 Erweiterung der vdkx-basis-Tabelle um die Segmentiereungskriterien (IMMO_METRO, IMMO_KREISFREI, IMMO_KAUFKRAFT ####
      #___________________________________________________________________________________________________________________
      logging::loginfo("Erweiterung der Basistabelle um Segmentierungskriterien: METROPOLE, KRESIFREI, KAUFKRAFT")

      sgmt_vmo <- vdkx_basis_vmo %>%
        # Bestimmung des Bewertungs- und Verwertungsjahres
        dplyr::mutate(
          VMO_V_O_BIS_DAT = dplyr::case_when(
            !is.na(.$VMO_V_DAT) ~ .$VMO_V_DAT,
            !is.na(.$VMO_BIS) ~ .$VMO_BIS,
            TRUE ~ NA_Date_)
        ) %>%
        dplyr::mutate(
          VMO_MW_JAHR = as.integer(lubridate::year(VMO_MW_DAT)),
          VMO_V_JAHR = as.integer(lubridate::year(VMO_V_O_BIS_DAT))
        ) %>%
        # Anspielen Bundesland und GKZ
        # a) VMO_V_JAHR befüllt
        dplyr::left_join(input_plz_mapping %>%
                           dplyr::rename(Bundesland_1 = PLZ_Bundesland,
                                         GKZ_1 = PLZ_GKZeichen),
                         by = c("IMMO_PLZ" = "PLZ5stellig",
                                "VMO_V_JAHR" = "Verwertungsjahr")) %>%
        # b) VMO_V_JAHR nicht befüllt
        dplyr::left_join(input_plz_mapping_max_v_jahr %>%
                           dplyr::rename(Bundesland_2 = BUNDESLAND_MAX_V_JAHR,
                                         GKZ_2 = GKZ_MAX_V_JAHR),
                         by = c("IMMO_PLZ" = "PLZ5stellig")) %>%

        # Anpassung der PLZ-Klasse fuer die Sätze ohne
        dplyr::mutate(
          PLZ_Klasse_new = case_when(
            is.na(PLZ_Klasse) == FALSE  ~ PLZ_Klasse,
            TRUE ~ PLZ_Klasse_MAX_V_JAHR)
        ) %>%

        dplyr::mutate(
          #METROPOLE = str_sub(PLZ_Klasse,start=1,end=1),
          IMMO_METROPOLE = case_when(
            VMO_TYP_1 == "IMMO" & str_sub(.$PLZ_Klasse_new,start=1,end=1) == 1 ~ "NonM",
            VMO_TYP_1 == "IMMO" & str_sub(.$PLZ_Klasse_new,start=1,end=1) == 2 ~ "M",
            TRUE ~ NA_character_),

          IMMO_KREISFREI = case_when(
            VMO_TYP_1 == "IMMO" & str_sub(.$PLZ_Klasse_new,start=2,end=2) == 5 ~ "nein",
            VMO_TYP_1 == "IMMO" & str_sub(.$PLZ_Klasse_new,start=2,end=2) == 6 ~ "ja",
            TRUE ~ NA_character_),

          IMMO_KAUFKRAFT = case_when(
            VMO_TYP_1 == "IMMO" & str_sub(.$PLZ_Klasse_new,start=3,end=3) == 7 ~ "niedrig",
            VMO_TYP_1 == "IMMO" & str_sub(.$PLZ_Klasse_new,start=3,end=3) == 8 ~ "mittel",
            VMO_TYP_1 == "IMMO" & str_sub(.$PLZ_Klasse_new,start=3,end=3) == 9 ~ "hoch",
            TRUE ~ NA_character_)
        ) %>%
        dplyr::select(VMO_CODE, VMO_QT, IMMO_METROPOLE, IMMO_KREISFREI, IMMO_KAUFKRAFT)


        #dplyr::select(-PLZ_Klasse,-PLZ_GKZeichen,-PLZ_Bundesland) %>%
        #dplyr::select(VMO_CODE, VMO_QT, VMO_TYP_1, VMO_V_JAHR, IMMO_PLZ, PLZ_Klasse, PLZ_Klasse_MAX_V_JAHR, PLZ_Klasse_new,
        #              Bundesland_1, Bundesland_2, IMMO_METROPOLE, IMMO_KREISFREI,IMMO_KAUFKRAFT)

      # 3.6 Erstellung VQ-Parametertabelle ####
      #________________________________________
      logging::loginfo("Erstellung VQ-Parametertabelle")

      parameter_vq <- vdkx_basis_vmo %>%
        # Anspielen der notwendigen Infos zur Parameterberechnung
        dplyr::left_join(bvq_immo,
                         by = c("VMO_CODE", "VMO_QT")) %>%
        dplyr::left_join(kq_immo_nvq_bars,
                         by = c("VMO_CODE", "VMO_QT")) %>%
        dplyr::left_join(tdvz_immo,
                         by = c("VMO_CODE", "VMO_QT")) %>%
        dplyr::left_join(fmp_vmo,
                         by = c("VMO_CODE", "VMO_QT")) %>%
        dplyr::left_join(sgmt_vmo,
                         by = c("VMO_CODE", "VMO_QT")) %>%

        # Ersetze bei fehlenden Werten NA durch 0
        dplyr::mutate(
          SUMME_KOSTEN = tidyr::replace_na(SUMME_KOSTEN, 0),
          SUMME_ERLOESE = tidyr::replace_na(SUMME_ERLOESE, 0),
          SUMME_DAUER_KOSTEN_GEWICHTET =
            tidyr::replace_na(SUMME_DAUER_KOSTEN_GEWICHTET, 0),
          SUMME_DAUER_ERLOESE_GEWICHTET =
            tidyr::replace_na(SUMME_DAUER_ERLOESE_GEWICHTET, 0),
          SUMME_VZ = tidyr::replace_na(SUMME_VZ, 0),
          SUMME_VZ_GEW = tidyr::replace_na(SUMME_VZ_GEW, 0),
          SUMME_BEW = dplyr::case_when(
            .$VMO_TYP_1 != "IMMO" | is.na(.$SUMME_BEW) ~ 0,
            TRUE ~ .$SUMME_BEW
          ),
          ANZ_BEW = dplyr::case_when(
            .$VMO_TYP_1 != "IMMO" | is.na(.$ANZ_BEW) ~ 0,
            TRUE ~ .$ANZ_BEW
          )) %>%
        # Parameterberechnung
        dplyr::mutate(
          # BVQ_IMMO
          #_________
          BVQ_IMMO = dplyr::case_when(
            .$VMO_TYP_1 == "IMMO" & !is.na(.$VMO_GOVE) &
              !is.na(.$VMO_BASISWERT) & .$VMO_BASISWERT != 0
            ~ .$VMO_GOVE / .$VMO_BASISWERT,
            TRUE ~ NA_real_
          ),
          # TAD_IMMO
          #_________
          TAD_IMMO = dplyr::case_when(
            # keine Immobilie
            .$VMO_TYP_1 != "IMMO" ~ NA_real_,
            # nicht verwertete Immobilie
            .$VMO_V_KNZ == FALSE ~ NA_real_,
            # verwertete Immobilie ohne Verwertungs- oder Abmeldedatum
            is.na(.$VMO_V_O_BIS_DAT) ~ NA_real_,
            # verwertete Immobilie mit Verwertungs- oder Abmeldedatum
            # a) kein Abwicklungsbeginn
            is.na(.$VMO_ABWICKLUNG_BEGINN) ~ as.numeric(
              (.$VMO_V_O_BIS_DAT - .$VMO_MIN_AFG_DAT) * (12 / 365.25)),
            # b) mit Abwicklungsbeginn
            TRUE ~ as.numeric(
              (.$VMO_V_O_BIS_DAT - .$VMO_ABWICKLUNG_BEGINN) * (12 / 365.25))
          ),
          # TDAA_IMMO
          #__________
          TDAA_IMMO = dplyr::case_when(
            # keine Immobilie
            .$VMO_TYP_1 != "IMMO" ~ NA_real_,
            # kein Abwicklungsbeginn o. kein Datum des 1. Ausfallereignis
            is.na(.$VMO_ABWICKLUNG_BEGINN) | is.na(.$VMO_MIN_AFG_DAT)
            ~ NA_real_,
            # Immobilien, die alle Voraussetzungen der Berechnung erfüllen
            TRUE ~ as.numeric(
              (.$VMO_ABWICKLUNG_BEGINN - .$VMO_MIN_AFG_DAT) * (12 / 365.25))
          ),
          # KQ_IMMO
          #__________
          KQ_IMMO = dplyr::case_when(
            # keine Immobilie
            .$VMO_TYP_1 != "IMMO" ~ NA_real_,
            # kein o. unzulässiger Marktwert
            is.na(.$VMO_MW_BETRAG) | .$VMO_MW_BETRAG == 0 ~ NA_real_,
            # Immobilien, die alle Voraussetzungen der Berechnung erfüllen
            TRUE ~ as.numeric(
              (.$SUMME_KOSTEN - .$SUMME_ERLOESE) / .$VMO_MW_BETRAG)
          ),
          # TKQ_IMMO
          #__________
          TKQ_IMMO = dplyr::case_when(
            # keine Immobilie
            .$VMO_TYP_1 != "IMMO" ~ NA_real_,
            # keine Kosten und Erlöse oder die Kosten minus Erlöse sind 0
            (is.na(.$SUMME_KOSTEN) & is.na(.$SUMME_ERLOESE)) |
              (.$SUMME_KOSTEN - .$SUMME_ERLOESE) == 0 ~ NA_real_,
            # Immobilien, die alle Voraussetzungen der Berechnung erfüllen
            TRUE ~ as.numeric(
              (.$SUMME_DAUER_KOSTEN_GEWICHTET -
                 .$SUMME_DAUER_ERLOESE_GEWICHTET) /
                (.$SUMME_KOSTEN - .$SUMME_ERLOESE) * (12 / 365.25))
          ),
          # TDVZ_IMMO
          #__________
          TDVZ_IMMO = dplyr::case_when(
            # keine Immobilie
            .$VMO_TYP_1 != "IMMO" ~ NA_real_,
            # keine Verwertungszahlungen oder Summe der Verwertungszahlungen
            # ergibt 0 Euro oder kein Verwertungs- oder Abmeldedatum
            is.na(.$SUMME_VZ) | .$SUMME_VZ == 0 |
              is.na(.$VMO_V_O_BIS_DAT) ~ NA_real_,
            # Immobilien, die alle Voraussetzungen der Berechnung erfüllen
            TRUE ~ as.numeric(
              ((.$SUMME_VZ_GEW / .$SUMME_VZ) - as.numeric(.$VMO_V_O_BIS_DAT)) *
                (12 / 365.25))
          ),
          # WFH_IMMO
          #_________
          WFH_IMMO = dplyr::case_when(
            .$VMO_TYP_1 == "IMMO" & .$VMO_V_ART %in% c(1, 11) ~ "FH",
            .$VMO_TYP_1 == "IMMO" & .$VMO_V_ART %in% c(2, 12) ~ "ZV",
            .$VMO_TYP_1 == "IMMO" & .$VMO_V_ART %in% c(21, 13) ~ "FordVerwert",
            .$VMO_TYP_1 == "IMMO" ~ "keine Angabe",
            TRUE ~ NA_character_
          ),
          # NVQ_BARS
          #_________
          NVQ_BARS = dplyr::case_when(
            # keine Barsicherheit
            .$VMO_TYP_1 != "BARS" ~ NA_real_,
            # Basiswert liegt nicht vor oder ist gleich 0
            is.na(.$VMO_BASISWERT) | .$VMO_BASISWERT == 0 ~ NA_real_,
            # es gibt weder Erlöse noch Kosten
            is.na(.$SUMME_ERLOESE) | is.na(.$SUMME_KOSTEN) ~ NA_real_,
            # Barsicherheiten, die alle Voraussetzungen der Berechnung erfüllen
            TRUE ~ as.numeric(
              (.$SUMME_ERLOESE - .$SUMME_KOSTEN) / .$VMO_BASISWERT)
          ),
          # TNVQ_BARS
          #_________
          TNVQ_BARS = dplyr::case_when(
            # keine Barsicherheit
            .$VMO_TYP_1 != "BARS" ~ NA_real_,
            # keine Kosten und Erlöse oder die Kosten minus Erlöse sind 0
            (is.na(.$SUMME_KOSTEN) & is.na(.$SUMME_ERLOESE)) |
              .$SUMME_ERLOESE - .$SUMME_KOSTEN == 0 ~ NA_real_,
            # Immobilien, die alle Voraussetzungen der Berechnung erfüllen
            TRUE ~ as.numeric(
              (.$SUMME_DAUER_ERLOESE_GEWICHTET -
                 .$SUMME_DAUER_KOSTEN_GEWICHTET) /
                (.$SUMME_ERLOESE - .$SUMME_KOSTEN) * (12 / 365.25))
          )) %>%
        # Spaltenreihenfolge festlegen
        dplyr::select(
          VMO_CODE, VMO_QT, VMO_TYP_1, BVQ_IMMO, TAD_IMMO, TDAA_IMMO, KQ_IMMO,
          TKQ_IMMO, TDVZ_IMMO, WFH_IMMO, NVQ_BARS, TNVQ_BARS, VMO_MIN_AFG_DAT,
          VMO_ABWICKLUNG_BEGINN, VMO_GOVE, VMO_MW_BETRAG, IMMO_BUNDESLAND,
          VMO_V_O_BIS_DAT, VMO_MW_JAHR, VMO_ALTER_MW,  MPZR_INDEX_MW_JAHR, VMO_V_JAHR,
          MPZR_INDEX_V_JAHR, VMO_BASISWERT, SUMME_ERLOESE, SUMME_KOSTEN,
          SUMME_VZ, SUMME_BEW, ANZ_BEW, MAX_ABW_BEW,
          VMO_FMP_VD_AVG_ZAHL_DAT, VMO_FMP_VD_BETRAG_REAL, VMO_FMP_VD_ANMERK,
          VMO_FMP_VD_BETRAG_PROG, IMMO_METROPOLE, IMMO_KREISFREI, IMMO_KAUFKRAFT)



      # 4. Schritt: Data-Dictionary für Parametertabelle ####
      create_data_dictionary(vdkx,
                             data = parameter_vq,
                             data_description =
                               get_col_tab_description(vdkx, tablename),
                             tablename = tablename)

      # 5. Schritt: Speichern / Protokollierung Parametertabelle ####
      arrow::write_parquet(parameter_vq,
                           file.path(path_exchange,
                                     paste0(tablename, ".parquet")))
      insert_table_protocol(vdkx, tablename, nrow(parameter_vq))
    } else {
      loginfo(paste(vdkx$info_msg_create_basis_not_done, tablename))
    }
  }
  logging::loginfo(paste(vdkx$info_msg_function_end,
                create_info_string(),
                "create_parameter_vq"))
}

#' Erstellt die Parametertabelle LGD Vorverarbeitung
#'
#' In dieser Funktion wird die Parametertabelle LGD Vorverarbeitung erstellt
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
#' @family vdkx_parameter
create_parameter_lgd_vv <- function(vdkx,
                                    tablename = NULL,
                                    path_exchange = NULL,
                                    path_input = NULL) {

  logging::loginfo("=========================================================")
  if (vdkx$used_loglevel %in% c("INFO", "DEBUG")) {
    tictoc::tic(" create_parameter_lgd_vv")
  }
  logging::loginfo(paste(vdkx$info_msg_function_begin,
                         "create_parameter_lgd_vv"))
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
    input_zins <-
      import_parquet_file(vdkx,
                          path_input,
                          file = "INPUT_ZINS.parquet",
                          dq_string = "PARAMETER_LGD_VV")
    basis_kunde_person <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1102_Person_Kunde.parquet",
                          dq_string = "PARAMETER_LGD_VV")
    basis_konto_kunde <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1108_Konto_Kunde.parquet",
                          dq_string = "PARAMETER_LGD_VV")
    basis_afr_kunde <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1140_Ausfallreihe_Kunde.parquet",
                          dq_string = "PARAMETER_LGD_VV")
    basis_kunde_buchungen <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1150_Kunde_Buchungen.parquet",
                          dq_string = "PARAMETER_LGD_VV")
    basis_afr_konto <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1220_Ausfallreihe_Konto.parquet",
                          dq_string = "PARAMETER_LGD_VV")
    basis_vmo_afr <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1320_VMO_Ausfallreihen.parquet",
                          dq_string = "PARAMETER_LGD_VV")
    basis_vmo_zahlungen <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1330_VMO_Zahlungen.parquet",
                          dq_string = "PARAMETER_LGD_VV")
    basis_vmo <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1350_VMO.parquet",
                          dq_string = "PARAMETER_LGD_VV")
    parameter_vq <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_5400_VQ.parquet",
                          dq_string = "PARAMETER_LGD_VV")

    if (state_function != vdkx$state_error
        & !is.null(input_zins)
        & !is.null(basis_kunde_person)
        & !is.null(basis_konto_kunde)
        & !is.null(basis_afr_kunde)
        & !is.null(basis_kunde_buchungen)
        & !is.null(basis_afr_konto)
        & !is.null(basis_vmo_afr)
        & !is.null(basis_vmo_zahlungen)
        & !is.null(basis_vmo)
        & !is.null(parameter_vq)
        & !is.null(vdkx[["SVZ_VMO_ZAHL_KAT"]])
        & !is.null(vdkx[["v_VDH_Schluessel_KTO_Klasse"]])
        ) {
      # 2. Schritt: Data-Dictionary für Input-Dateien ####
      fill_dd_input_table(vdkx,
                          data = input_zins,
                          tablename = "INPUT_ZINS")
      fill_dd_input_table(vdkx,
                        data = basis_kunde_person,
                        tablename = "T_1102_Person_Kunde")
      fill_dd_input_table(vdkx,
                          data = basis_konto_kunde,
                          tablename = "T_1108_Konto_Kunde")
      fill_dd_input_table(vdkx,
                          data = basis_afr_kunde,
                          tablename = "T_1140_Ausfallreihe_Kunde")
      fill_dd_input_table(vdkx,
                          data = basis_kunde_buchungen,
                          tablename = "T_1150_Kunde_Buchungen")
      fill_dd_input_table(vdkx,
                          data = basis_afr_konto,
                          tablename = "T_1220_Ausfallreihe_Konto")
      fill_dd_input_table(vdkx,
                          data = basis_vmo_afr,
                          tablename = "T_1320_VMO_Ausfallreihen")
      fill_dd_input_table(vdkx,
                          data = basis_vmo_zahlungen,
                          tablename = "T_1330_VMO_Zahlungen")
      fill_dd_input_table(vdkx,
                          data = basis_vmo,
                          tablename = "T_1350_VMO")
      fill_dd_input_table(vdkx,
                          data = vdkx[["SVZ_VMO_ZAHL_KAT"]],
                          tablename = "SVZ_VMO_ZAHL_KAT")
      fill_dd_input_table(vdkx,
                          data = vdkx[["v_VDH_Schluessel_KTO_Klasse"]],
                          tablename = "v_VDH_Schluessel_KTO_Klasse")


      # 3. Schritt: Parametertabelle erzeugen ####
      #___________________________________________

      # Teilschritte der Tabellenerstellung:
      #_____________________________________
      #
      #   3.1 Bestimmung der Konten mit Guthaben zum 1. C-Ausfall
      #     --> Output: guthaben_kto_min_c_afg
      #   3.2 Bestimmung des Quartalzinssatzes für die Diskontierung
      #     --> Output: disk_zins
      #   3.3 Kundenbuchungen EE / EK / AVAL_IA / ZINS / FVZ
      #     --> Output: kunde_afr_kd_bu_real, kunde_afr_kd_bu_synth
      #   3.4 Bestimmung der Salden (Ausfall, C, RM) und des EAD
      #     --> Output: salden_kd
      #   3.5 VMO Zahlungen VE / VK / VE_2 / VK_2
      #     --> Output: kunde_afr_vmo_zahl_real, kunde_afr_vmo_zahl_synth
      #   3.6 FMP-Verlustdifferenz
      #     --> Output: fmp_verlustdiff
      #   3.7 LGD Vorverarbeitung und finale Parameterbestimmung und -
      #     zusammenführung
      #     --> Output: lgd_vorverarbeitung

      # Migrationsdatum vom IZB Rechenzentrum zur FI
      izb_dat <- as.Date("18.11.2007", format = "%d.%m.%Y")

      # 3.1 Bestimmung der Konten mit Guthaben zum 1. C-Ausfall ####
      #_____________________________________________________________
      logging::loginfo("Bestimmung der Konten mit Guthaben zum 1. C-Ausfall")
      # Quelle: basis_afr_konto
      #
      # Folgende Spalte wird ermittelt:
      # - KD_MIN_C_AFG_SALDO_GUTHABEN_KTO
      # - FLAG_KTO_MIT_GUTH_C

      guthaben_kto_min_c_afg <- basis_afr_konto %>%
        # Einschränkung auf Konten mit einem Guthaben zum 1. C-Ausfall
        dplyr::filter(dplyr::coalesce(MIN_C_AFG_SALDO, 0) < 0) %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, KTO_NR, KTO_QT,
                      MIN_C_AFG_SALDO) %>%
        dplyr::mutate(FLAG_KTO_MIT_GUTH_C = 1) %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD) %>%
        dplyr::mutate(
          KD_MIN_C_AFG_SALDO_GUTHABEN_KTO = sum(MIN_C_AFG_SALDO)
        ) %>%
        dplyr::ungroup()

      # 3.2 Bestimmung des Quartalzinssatzes für die Diskontierung ####
      #________________________________________________________________
      msg <- "Bestimmung des Quartalzinssatzes für die Diskontierung"
      logging::loginfo(msg)
      # Quelle: INPUT_ZINS
      #
      # Folgende (Zwischen-)Parameter werden ermittelt:
      # - Quartal_Zins

      # Bestimmung des Min- und Max-Defaultzinssatzes, falls das MIN_AFG_DAT
      # über den Zeitraum der verfügbaren Quartalszinssätze in input_zins
      # hinausgeht oder davor liegt.
      # Ausschluss der Zeile 9999_Q0 (est. Schätzer aus Pflege 2021)
      input_zins_adj <-
        dplyr::filter(input_zins, substr(Ausfall_Quartal, 1, 4) != "9999")
      #Bestimmung des Min-Quartals und Max-Quartals
      max <- max(input_zins_adj$Ausfall_Quartal)
      min <- min(input_zins_adj$Ausfall_Quartal)
      # Bestimmung des Zinssatzes zum Min-Quartal und Max-Quartal, sowie das
      # jeweilige Min-Datum und Max-Datum.
      input_zins_def_min <-
        dplyr::filter(input_zins, Ausfall_Quartal == min) %>%
        dplyr::transmute(Quartal_Zins_min = Quartal_Zins,
                         qz_date_min =
                           as.Date(str_c("01.01.", substr(min, 1, 4)),
                                   format = "%d.%m.%Y"))
      input_zins_def_max <-
        dplyr::filter(input_zins, Ausfall_Quartal == max) %>%
        dplyr::transmute(Quartal_Zins_max = Quartal_Zins,
                         qz_date_max =
                           as.Date(str_c("31.12.", substr(max, 1, 4)),
                                   format = "%d.%m.%Y"))

      disk_zins <- basis_afr_kunde %>%
        dplyr::mutate(
          # "Quartals-Zeichenkette" für Join des Zinssatzes
          MIN_AFG_DAT_quartal = str_c(
            as.character(format(MIN_AFG_DAT, "%Y")), "_Q",
            as.character(ceiling(as.numeric(format(MIN_AFG_DAT, "%m"))
                                 / 3)))) %>%
        # Join der Quartals-Zinssätze, sowie der Defaultzinssätze
        dplyr::left_join(input_zins, by = c("MIN_AFG_DAT_quartal"
                                            = "Ausfall_Quartal")) %>%
        dplyr::full_join(input_zins_def_min, by = character()) %>%
        dplyr::left_join(input_zins_def_max, by = character()) %>%
        # Auswahl des korrekten Zinssatzes
        dplyr::mutate(Quartal_Zins = case_when(
          MIN_AFG_DAT < qz_date_min ~ Quartal_Zins_min,
          MIN_AFG_DAT > qz_date_max ~ Quartal_Zins_max,
          TRUE ~ Quartal_Zins
        )) %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                      MIN_AFG_DAT, MIN_AFG_DAT_quartal, Quartal_Zins,
                      qz_date_min, Quartal_Zins_min, qz_date_max,
                      Quartal_Zins_max)

      # 3.3 Kundenbuchungen ####
      #_________________________
      logging::loginfo("Verarbeitung der Kundenbuchungen")
      # In diesem Abschnitt wird erst ein großer Dataframe mit allen
      # aufsummierten Buchungen für die einzelnen Kundenausfallreihen
      # aufgeteilt nach den unterschiedlichen Zahlungsphasen und
      # Zahlungsarten gebildet (kunde_afr_bu_alle).
      # Dann werden aus diesem großen Dataframe verschiedene kleinere
      # Dataframes gebildet, die jeweils nur bestimmte aufsummierte Buchungen
      # im richtigen Format enthalten.
      #
      # Quelle: T_1150_Kunde_Buchungen
      #
      # Output: kunde_afr_kd_bu_real
      # Hinweis: In den aufsummierten Buchungen sind noch keine synthetischen
      # Zahlungen enthalten
      # - KD_AVAL_IA_SAN, KD_AVAL_IA_NACH_C
      # - KD_EE_NACH_C, KD_EE_NACH_C_GEW, KD_EE_NACH_C_DISK,
      # - KD_EK_NACH_C, KD_EK_NACH_C_GEW, KD_EK_NACH_C_DISK
      # - KD_EE_NACH_ENDE
      # - KD_EK_NACH_ENDE
      # - KD_FVZ_SAN, KD_FVZ_NACH_C, KD_FVZ_NACH_ENDE
      # - KD_ZINS_SAN, KD_ZINS_NACH_C, KD_ZINS_NACH_ENDE
      #
      # - KD_VE_OHNE_VMO
      # - KD_VK_OHNE_VMO
      #
      # - AFR_KD_MAX_BU_DAT
      #
      # Output: kunde_afr_kd_bu_synth
      # Hinweis: Hier sind Zwischenergebnisse zu den synthetischen Zahlungen
      # für die Kreditkarten und C-Guthaben-Konten enthalten.
      #
      # - K_KARTEN_NACH_C, K_KARTEN_NACH_C_GEW, K_KARTEN_NACH_C_DISK
      # - GUTH_C_NACH_C, GUTH_C_NACH_C_GEW, GUTH_C_NACH_C_DISK

      # Kreditkarten mit IA zum 1. C-Ausfall
      #_____________________________________
      # Bestimme die Summe der positiven Inanspruchnahmen zum 1. C-Ausfall
      # der Kreditkarten innerhalb einer Ausfallreihe
      kreditkarten_saldo_c <- basis_afr_konto %>%
        # Einschränkung auf Kreditkarten mit positiven Salden zum 1. C-Ausfall
        dplyr::filter(KTO_KLASSE == 24,
                      dplyr::coalesce(MIN_C_AFG_SALDO, 0) > 0) %>%
        # Bestimme die Summe der Kreditkarteninanspruchnahmen in der AFR
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD) %>%
        dplyr::summarise(
          SUM_K_KARTEN_SALDO_C = sum(MIN_C_AFG_SALDO)
        )

      # Dataframe mit allen Kundenbuchungen
      #____________________________________
      kunde_afr_buchungen <- basis_kunde_buchungen %>%
        # Anspielen der Kundenausfallreihen
        dplyr::left_join(basis_afr_kunde %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, MIN_AFG_DAT,
                                         MIN_C_AFG_DAT, MIN_AFG_KATEGORIE,
                                         AFR_KD_STATUS, AFR_KD_ENDE,
                                         MAX_AFG_ENDE),
                         by = c("PNR_KD", "PNR_KD_QT")) %>%
        # An jede Kundenausfallreihe wird das MIN_AFG_DAT der nächsten
        # AFR des Kunden angespielt
        dplyr::left_join(basis_afr_kunde %>%
                           dplyr::mutate(AFR_KD_PREV = AFR_KD - 1) %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD_PREV,
                                         MIN_AFG_DAT_NEXT_AFR = MIN_AFG_DAT),
                         by = c("PNR_KD", "PNR_KD_QT",
                                "AFR_KD" = "AFR_KD_PREV")) %>%
        # Grobe Filterung von Buchungssätzen die nicht zur Ausfallreihe
        # gehören
        # Hinweis: Buchungen zum MIN_AFG_DAT können nach diesem Schritt noch
        # an mehereren AFR hängen
        dplyr::filter(
          MIN_AFG_DAT <= BU_DAT,
          BU_DAT <= dplyr::coalesce(MIN_AFG_DAT_NEXT_AFR, BU_DAT)
          ) %>%
        # Anspielen der Diskontierungszinssätze
        dplyr::left_join(disk_zins %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                         Quartal_Zins),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        # Anspielen der Kreditkarten Inanspruchnahmen zum 1. C-Ausfall
        dplyr::left_join(kreditkarten_saldo_c,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::mutate(
          ENDE_DAT = dplyr::coalesce(AFR_KD_ENDE, MAX_AFG_ENDE)
          ) %>%
        # Bestimme die Zahlungsphase einer Buchung
        #_________________________________________
        dplyr::mutate(
          Z_PHASE = dplyr::case_when(
            # 1. Zahlungsdatum <> Ausfalldatum
            # 1.a Sanierung
            MIN_AFG_DAT < BU_DAT &
              BU_DAT < dplyr::coalesce(MIN_C_AFG_DAT, ENDE_DAT) ~ "SAN",
            # 1.b Nach C-Ausfall
            !is.na(MIN_C_AFG_DAT) &
              MIN_C_AFG_DAT < BU_DAT & BU_DAT <= ENDE_DAT ~ "NACH_C",
            # 1.c Nach Ende der AFR
            (is.na(MIN_AFG_DAT_NEXT_AFR) & ENDE_DAT < BU_DAT) |
              (!is.na(MIN_AFG_DAT_NEXT_AFR) & ENDE_DAT < BU_DAT &
                 BU_DAT < MIN_AFG_DAT_NEXT_AFR) ~ "NACH_ENDE",
            # 2. Zahlungsdatum == Ausfalldatum
            # 2.a Sanierung
            BU_DAT <= izb_dat & BU_DAT == MIN_AFG_DAT &
              (is.na(MIN_C_AFG_DAT) | MIN_AFG_DAT != MIN_C_AFG_DAT) ~ "SAN",
            BU_DAT > izb_dat & !is.na(MIN_C_AFG_DAT) &
              MIN_AFG_DAT != MIN_C_AFG_DAT & BU_DAT == MIN_C_AFG_DAT ~ "SAN",
            # 2.b Nach C-Ausfall
            BU_DAT <= izb_dat & !is.na(MIN_C_AFG_DAT) &
              BU_DAT == MIN_C_AFG_DAT ~ "NACH_C",
            # 2.c Nach Ende der AFR
            BU_DAT > izb_dat & !is.na(MIN_AFG_DAT_NEXT_AFR) &
              BU_DAT == MIN_AFG_DAT_NEXT_AFR ~ "NACH_ENDE",
            # Sonst
            TRUE ~ "NICHT_RELEVANT")
          ) %>%
        # Entferne die nicht relevanten Buchungen aus der Tabelle
        dplyr::filter(Z_PHASE != "NICHT_RELEVANT") %>%
        # Anspielen der Information, ob das jeweilige Konto zum ersten
        # C-Ausfall ein Guthaben besitzt
        dplyr::left_join(guthaben_kto_min_c_afg,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD", "KTO_NR",
                                "KTO_QT")) %>%
        dplyr::mutate(
          FLAG_KTO_MIT_GUTH_C = tidyr::replace_na(FLAG_KTO_MIT_GUTH_C, 0),
          SUM_K_KARTEN_SALDO_C = tidyr::replace_na(SUM_K_KARTEN_SALDO_C, 0.00)
          ) %>%
        dplyr::mutate(
          # Selektiere die Kunde-Buchungen von C-Guthaben-Konten
          #_____________________________________________________
          BETRAG_KTO_MIT_GUTH_C = dplyr::if_else(FLAG_KTO_MIT_GUTH_C == 1,
                                                 BU_BETRAG, 0),
          # Selektiere die Buchungen zu ungültigem VMO
          #___________________________________________
          VE_OHNE_VMO = dplyr::if_else(BU_KAT_ID == 6, BU_BETRAG, 0),
          VK_OHNE_VMO = dplyr::if_else(BU_KAT_ID == 7, BU_BETRAG, 0),
          # Festlegen ob Buchung EE, EK, AVAL_IS, FVZ oder ZINS ist
          #________________________________________________________
          Z_ART = dplyr::case_when(
            BU_KAT_ID %in% c(1, 3, 6) ~ "EE",
            BU_KAT_ID %in% c(2, 4, 7) ~ "EK",
            BU_KAT_ID == 5 ~ "AVAL_IA",
            BU_KAT_ID == 8 ~ "FVZ",
            BU_KAT_ID == 9 ~ "ZINS"),
          # Vorzeichen bestimmen
          #_____________________
          # Wird benötigt bei der Summenbildung innerhalb einer Z_ART.
          VZ = dplyr::case_when(
            # Forderungserhöhende Buchungen (Sollbuchungen)
            Z_ART %in% c("EK", "AVAL_IA", "ZINS") & BU_S_H == "H" ~ -1,
            # Forderungsreduzierende Buchungen (Habenbuchungen)
            Z_ART %in% c("EE", "FVZ") & BU_S_H == "S" ~ -1,
            # Sonst
            TRUE ~ 1),
          # Wird benötigt wenn man verschiedene Z_ART zusammenrechnet.
          VZ_S_H = dplyr::case_when(
            # Forderungserhöhende Buchungen (Sollbuchungen)
            Z_ART %in% c("EK", "AVAL_IA", "ZINS") ~ -1,
            # Forderungsreduzierende Buchungen (Habenbuchungen)
            Z_ART %in% c("EE", "FVZ") ~ 1,
            # Sonst
            TRUE ~ 0),
          # Bestimme die gewichteten und diskontierten Zahlungsbeträge
          #___________________________________________________________
          DISK_TAGE =
            as.numeric(dplyr::coalesce(BU_VALUTA, BU_DAT))
          - as.numeric(MIN_AFG_DAT),
          BETRAG_GEW = round(BU_BETRAG * (DISK_TAGE / 365.25), 2),
          BETRAG_DISK = round(BU_BETRAG * (1 / (1 + Quartal_Zins / 100))^
            (DISK_TAGE / 365.25), 2),
          DISK_TAGE_SYNTH_NACH_C =
            (as.numeric(dplyr::coalesce(MIN_C_AFG_DAT, MIN_AFG_DAT))
             - as.numeric(MIN_AFG_DAT)),
          BETRAG_KTO_MIT_GUTH_C_GEW =
            round(BETRAG_KTO_MIT_GUTH_C * (DISK_TAGE_SYNTH_NACH_C /
                                             365.25), 2),
          BETRAG_KTO_MIT_GUTH_C_DISK = round(BETRAG_KTO_MIT_GUTH_C *
                                  (1 / (1 + Quartal_Zins / 100))^
                                  (DISK_TAGE_SYNTH_NACH_C / 365.25), 2),
          SUM_K_KARTEN_SALDO_C_GEW =
            round(SUM_K_KARTEN_SALDO_C * (DISK_TAGE_SYNTH_NACH_C /
                                                365.25), 2),
          SUM_K_KARTEN_SALDO_C_DISK = round(SUM_K_KARTEN_SALDO_C *
            (1 / (1 + Quartal_Zins / 100))^
            (DISK_TAGE_SYNTH_NACH_C / 365.25), 2),

        ) %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD) %>%
        dplyr::mutate(
          AFR_KD_MAX_BU_DAT =
            if (all(is.na(BU_DAT[Z_PHASE %in% c("SAN", "NACH_C")])))
              NA_Date_ else
            max(BU_DAT[Z_PHASE %in% c("SAN", "NACH_C")], na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD, AFR_KD_MAX_BU_DAT, Z_ART,
                        Z_PHASE, VZ_S_H) %>%
        dplyr::summarise(
          # Summe der tatsächlich erfassten Buchungen
          SUM = sum(BU_BETRAG * VZ),
          SUM_GEW = sum(BETRAG_GEW * VZ),
          SUM_DISK = sum(BETRAG_DISK * VZ),
          # Summe der VMO-Zahlungen ohne VMO
          SUM_VE_OHNE_VMO = sum(VE_OHNE_VMO * VZ),
          SUM_VK_OHNE_VMO = sum(VK_OHNE_VMO * VZ),
          # Summe der Buchungen der C-Guthaben-Konten in der KD-AFR
          SUM_BETRAG_KTO_MIT_GUTH_C = sum(BETRAG_KTO_MIT_GUTH_C * VZ),
          SUM_BETRAG_KTO_MIT_GUTH_C_GEW = sum(BETRAG_KTO_MIT_GUTH_C_GEW * VZ),
          SUM_BETRAG_KTO_MIT_GUTH_C_DISK = sum(BETRAG_KTO_MIT_GUTH_C_DISK * VZ),
          # C-Salden der Kreditkarten
          # Ab hier keine Summe mehr bilden, da die folgenden Werte bereits auf
          # AFR-Ebene aggregiert sind
          SUM_K_KARTEN_SALDO_C = first(SUM_K_KARTEN_SALDO_C),
          SUM_K_KARTEN_SALDO_C_GEW = first(SUM_K_KARTEN_SALDO_C_GEW),
          SUM_K_KARTEN_SALDO_C_DISK = first(SUM_K_KARTEN_SALDO_C_DISK),
        ) %>%
        dplyr::ungroup()

      # Dataframe mit den tatsächlich gebuchten Zahlungssummen
      #_______________________________________________________
      kunde_afr_kd_bu_real_tmp <- kunde_afr_buchungen %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, AFR_KD_MAX_BU_DAT, Z_ART,
                      Z_PHASE, SUM, SUM_GEW, SUM_DISK, SUM_VE_OHNE_VMO,
                      SUM_VK_OHNE_VMO) %>%
        tidyr::pivot_wider(
          names_from = c(Z_ART, Z_PHASE),
          values_from = c(SUM, SUM_GEW, SUM_DISK, SUM_VE_OHNE_VMO,
                          SUM_VK_OHNE_VMO),
          values_fill = 0.00,
          names_glue = "KD_{Z_ART}_{Z_PHASE}_{.value}"
          ) %>%
        # Korrektur der Spaltennamen
        dplyr::rename_with(
          function(x) {
            stringr::str_replace(x, "_SUM", "")
            },
          dplyr::contains("SUM"))

      # Definiere die Spalten, die mindestens in der Zwischentabelle
      # enthalten sein müssen. Dies ist notwendig, da in
      # kunde_afr_kd_bu_real_tmp aktuell noch Spalten fehlen könnten, wenn
      # entsprechende Zahlungen im Datenbestand fehlen
      colnames_bu_real <-
        c("PNR_KD", "PNR_KD_QT", "AFR_KD", "AFR_KD_MAX_BU_DAT",
          "KD_AVAL_IA_SAN", "KD_AVAL_IA_NACH_C", "KD_EE_NACH_C",
          "KD_EE_NACH_C_GEW", "KD_EE_NACH_C_DISK", "KD_EK_NACH_C",
          "KD_EK_NACH_C_GEW", "KD_EK_NACH_C_DISK", "KD_EE_NACH_ENDE",
          "KD_EK_NACH_ENDE", "KD_FVZ_SAN", "KD_FVZ_NACH_C", "KD_FVZ_NACH_ENDE",
          "KD_ZINS_SAN", "KD_ZINS_NACH_C", "KD_ZINS_NACH_ENDE",
          "KD_VE_OHNE_VMO", "KD_VK_OHNE_VMO", "KD_EE_SAN_VE_OHNE_VMO",
          "KD_EE_NACH_C_VE_OHNE_VMO", "KD_EK_SAN_VK_OHNE_VMO",
          "KD_EK_NACH_C_VK_OHNE_VMO")

      kunde_afr_kd_bu_real <- suppressMessages(
        # Bilde einen leeren tibble mit den benötigten Spalten
        tibble::as_tibble(matrix(nrow = 0, ncol = length(colnames_bu_real),
                                 dimnames = list(NULL, colnames_bu_real))) %>%
          # Datentypen festlegen, damit der anschließende Join funktioniert
          dplyr::mutate(
            AFR_KD = as.numeric(AFR_KD),
            dplyr::across(tidyselect::contains(c("PNR_KD", "VE", "VK", "EE",
                                                 "EK", "AVAL", "FVZ", "ZINS")),
                          as.numeric),
            dplyr::across(tidyselect::contains(c("DAT")), as.Date),
            dplyr::across(c("PNR_KD_QT"), as.integer)
          ) %>%
          # Anspielen der tatsächlich vorliegenden aggregierten Zahlungen
          dplyr::full_join(kunde_afr_kd_bu_real_tmp) %>%
          # fehlende Werte in Zahlungsspalten durch 0 ersetzen
          dplyr::mutate(
            dplyr::across(
              tidyselect::contains(c("VE", "VK", "EE", "EK", "AVAL", "FVZ",
                                     "ZINS")),
              ~tidyr::replace_na(.x, 0.00))
          ) %>%
          # fehlende Spalten berechnen
          dplyr::mutate(
            KD_VE_OHNE_VMO = KD_EE_SAN_VE_OHNE_VMO + KD_EE_NACH_C_VE_OHNE_VMO,
            KD_VK_OHNE_VMO = KD_EK_SAN_VK_OHNE_VMO + KD_EK_NACH_C_VK_OHNE_VMO
          ) %>%
          # Einschränkung auf die benötigten Spalten
          dplyr::select(tidyselect::all_of(colnames_bu_real))
      )

      # Dataframe mit den synthetischen Zahlungssummen
      #_______________________________________________
      kunde_afr_kd_bu_synth <- kunde_afr_buchungen %>%
        # es werden nur die Zahlungen der Z-Phase "NACH_C" benötigt
        dplyr::filter(Z_PHASE == "NACH_C") %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD) %>%
        dplyr::summarise(
          # für Kreditkarten hat man bereits die Summe der pos. C-Salden pro
          # Kundenausfallreihe
          K_KARTEN_NACH_C = first(SUM_K_KARTEN_SALDO_C),
          K_KARTEN_NACH_C_GEW = first(SUM_K_KARTEN_SALDO_C_GEW),
          K_KARTEN_NACH_C_DISK = first(SUM_K_KARTEN_SALDO_C_DISK),
          # für die C-Guthabenkonten wird hier die Summe über alle
          # Kundenbuchungen (EE, EK, FVZ, ZINSEN, AVAL_IA) gebildet
          KU_GUTH_C_NACH_C = sum(SUM_BETRAG_KTO_MIT_GUTH_C * VZ_S_H),
          KU_GUTH_C_NACH_C_GEW = sum(SUM_BETRAG_KTO_MIT_GUTH_C_GEW * VZ_S_H),
          KU_GUTH_C_NACH_C_DISK = sum(SUM_BETRAG_KTO_MIT_GUTH_C_DISK * VZ_S_H)
        ) %>%
        dplyr::ungroup()

      # 3.4 Bestimmung der Salden (Ausfall, C, RM) und des EAD ####
      #____________________________________________________________
      logging::loginfo("Bestimmung der Salden")
      # Quelle: T_1220_Ausfallreihe_Konto
      #
      # Folgende (Zwischen-)Parameter werden ermittelt:
      # - KD_MIN_AFG_SALDO
      # - KD_MIN_C_AFG_SALDO
      # - KD_RM_SALDO
      # - KD_RM_SALDO_DISK
      # - KD_MIN_AFG_EAD
      salden_kto <- basis_afr_konto %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, KTO_NR, KTO_QT, KTO_KLASSE,
                      MIN_AFG_DAT, MIN_C_AFG_DAT, RM_DAT,
                      MIN_AFG_SALDO, MIN_C_AFG_SALDO, RM_SALDO) %>%
        # Anspielen der Info, ob ein Konto EAD-relevant ist oder nicht
        dplyr::left_join(vdkx[["v_VDH_Schluessel_KTO_Klasse"]],
                         by = c("KTO_KLASSE" = "KTO_Klasse_T")) %>%
        # Anspielen der Zinssätze
        dplyr::left_join(disk_zins %>%
                           dplyr::select(PNR_KD, PNR_KD_QT,
                                         AFR_KD, Quartal_Zins),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::mutate(
          DISK_TAGE_RM = as.numeric(dplyr::coalesce(RM_DAT, MIN_AFG_DAT))
          - as.numeric(MIN_AFG_DAT),
          FLAG_KTO_EAD_RELEVANT = dplyr::if_else(KTO_EAD_Knz == -1, 1, 0),
          FLAG_MIN_AFG_SALDO_POS =
            dplyr::if_else(!is.na(MIN_AFG_SALDO) & MIN_AFG_SALDO > 0, 1, 0),
          FLAG_MIN_C_AFG_SALDO_POS =
            dplyr::if_else(!is.na(MIN_C_AFG_SALDO) & MIN_C_AFG_SALDO > 0, 1, 0),
          RM_SALDO_DISK = round(RM_SALDO * (1 / (1 + Quartal_Zins / 100))^
                                        (DISK_TAGE_RM / 365.25), 2)
        )

      # Aggregation der Salden von Kunde-Konto auf Kundenebene
      salden_kd <- salden_kto %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD) %>%
        dplyr::summarise(
          KD_MIN_AFG_SALDO =
            round(sum(MIN_AFG_SALDO * FLAG_KTO_EAD_RELEVANT *
                        FLAG_MIN_AFG_SALDO_POS, na.rm = TRUE), 2),
          KD_MIN_C_AFG_SALDO =
            round(sum(MIN_C_AFG_SALDO * FLAG_KTO_EAD_RELEVANT *
                        FLAG_MIN_C_AFG_SALDO_POS, na.rm = TRUE), 2),
          KD_RM_SALDO = round(sum(RM_SALDO * FLAG_KTO_EAD_RELEVANT,
                                  na.rm = TRUE), 2),
          KD_RM_SALDO_DISK = round(sum(RM_SALDO_DISK * FLAG_KTO_EAD_RELEVANT,
                                       na.rm = TRUE), 2)
          ) %>%
        dplyr::ungroup() %>%
        # Anspielen der AVAL-IA um das Kunden-EAD berechnen zu können
        dplyr::left_join(kunde_afr_kd_bu_real %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                         KD_AVAL_IA_SAN, KD_AVAL_IA_NACH_C),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::mutate(
          KD_MIN_AFG_EAD = pmax(KD_MIN_AFG_SALDO +
                                  tidyr::replace_na(KD_AVAL_IA_SAN, 0) +
                                  tidyr::replace_na(KD_AVAL_IA_NACH_C, 0), 0)
        )

      # 3.5 VMO Zahlungen ####
      #_______________________
      logging::loginfo("Verarbeitung der VMO-Zahlungen")
      # Quelle: T_1330_VMO_Zahlungen
      #
      # Output: kunde_afr_vmo_zahl_real
      # - KD_VE_SAN, KD_VE_SAN_DISK
      # - KD_VK_SAN, KD_VK_SAN_DISK
      # - KD_VE_NACH_C, KD_VE_NACH_C_DISK
      # - KD_VK_NACH_C, KD_VK_NACH_C_DISK
      # - KD_VE_NACH_ENDE
      # - KD_VK_NACH_ENDE
      #
      # - KD_VE_SAN_IMMO, KD_VE_SAN_IMMO_DISK
      # - KD_VK_SAN_IMMO, KD_VK_SAN_IMMO_DISK
      # - KD_VE_NACH_C_IMMO, KD_VE_NACH_C_IMMO_DISK
      # - KD_VK_NACH_C_IMMO, KD_VK_NACH_C_IMMO_DISK
      # - KD_VE_NACH_ENDE_IMMO, KD_VK_NACH_ENDE_IMMO
      #
      # - AFR_KD_MAX_VMO_ZAHL_DAT
      #
      # Output: kunde_afr_vmo_zahl_synth
      # Hinweis: Hier sind Zwischenergebnisse zu den synthetischen Zahlungen
      # für die C-Guthaben-Konten enthalten.
      #
      # - GUTH_C_NACH_C, GUTH_C_NACH_C_GEW, GUTH_C_NACH_C_DISK

      # Bestimmung der Gesamtmenge an VMO-Zahlungen, die einer
      # Kundenausfallreihe zugeordnet werden müssen
      vmo_zahlungen_relevant <- basis_vmo_zahlungen %>%
        # Nur Zahlungen der Kategorien 1 bis 9
        dplyr::filter(VMO_ZAHL_KAT_ID >= 1, VMO_ZAHL_KAT_ID <= 9) %>%
        # Anspielen der Info, ob es sich um eine Erlös- oder Kostenbuchung
        # handelt
        dplyr::left_join(vdkx[["SVZ_VMO_ZAHL_KAT"]] %>%
                           dplyr::select(ID, ERLOES),
                         by = c("VMO_ZAHL_KAT_ID" = "ID")) %>%
        # Anspielen der basis_vmo für AIRBA_KNZ und VMO_TYP_1
        dplyr::left_join(basis_vmo %>%
                           dplyr::select(VMO_CODE, VMO_QT, VMO_TYP_1,
                                         AIRBA_KNZ),
                         by = c("VMO_CODE", "VMO_QT"))

      # i) Zuordnung einer VMO-Zahlung über die angegebene Personennummer
      #__________________________________________________________________
      vmo_zahlungen_via_pnr <- vmo_zahlungen_relevant %>%
        # Anspielen des Kunden über die evtl. vorhandene Personennummer
        dplyr::left_join(basis_kunde_person,
                         by = c("VMO_ZAHL_PNR" = "PNR",
                                "VMO_ZAHL_PNR_QT" = "PNR_QT")) %>%
        # Anspielen der Kundenausfallreihen über VMO_CODE und PNR_KD
        dplyr::left_join(basis_vmo_afr %>%
                           dplyr::filter(AFR_KD_KNZ != 0),
                         by = c("VMO_CODE", "VMO_QT", "PNR_KD",
                                "PNR_KD_QT")) %>%
        # Einschränken auf Fälle, bei denen eine Kundenausfallreihe mit
        # AFR_KD_KNZ <> 0 gefunden werden konnte
        dplyr::filter(!is.na(AFR_KD))

      # ii) Zuordnung einer VMO-Zahlung über den VMO-Code an der Zahlung
      #_________________________________________________________________
      vmo_zahlungen_via_vmo <- vmo_zahlungen_relevant %>%
        # Ausschluss der Zahlungen, denen bereits eine Kundenausfallreihe
        # zugeordnet wurde
        dplyr::anti_join(vmo_zahlungen_via_pnr,
                         by = c("VMO_CODE", "VMO_QT", "VMO_ZAHL_ID",
                                "VMO_ZAHL_QT", "VMO_ZAHL_KLASSE")) %>%
        # Anspielen der Kundenausfallreihen, die durch das VMO besichert sind
        dplyr::left_join(basis_vmo_afr,
                         by = c("VMO_CODE", "VMO_QT")) %>%
        # Entferne die Zahlungen, die an VMOs hängen, die keine
        # Kundenausfallreihe besichern und die Zeilen bei
        # denen die Zahlung vor der Ausfallreihe liegt
        dplyr::filter(!is.na(PNR_KD) & VMO_ZAHL_DAT >= MIN_AFG_DAT) %>%
        # Pro Kunde, an dem die Zahlung hängt wird auf die aktuellste AFR
        # eingeschränkt
        dplyr::group_by(VMO_CODE, VMO_QT, VMO_ZAHL_ID,
                        VMO_ZAHL_QT, VMO_ZAHL_KLASSE,
                        PNR_KD, PNR_KD_QT) %>%
        dplyr::filter(AFR_KD == max(AFR_KD)) %>%
        dplyr::ungroup() %>%
        # Anspielen des EAD des Kunden zum ersten Ausfallzeitpunkt
        dplyr::left_join(salden_kd %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                         KD_MIN_AFG_EAD),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        # Anspielen der Konto-Kunde-Gültigkeiten
        dplyr::left_join(
          basis_konto_kunde %>%
            # Falls das Ende der Kontogültigkeit am Kunden dem
            # Kontolöschungsdatum entspricht wird das Ende der
            # Kontogültigkeit auf unbegrenzt gültig gesetzt.
            dplyr::mutate(
              KTO_PNR_KD_GUELTIG_BIS = dplyr::if_else(
                !is.na(KTO_LOESCHUNG) &
                  KTO_PNR_KD_GUELTIG_BIS == KTO_LOESCHUNG,
                NA_Date_, KTO_PNR_KD_GUELTIG_BIS)),
          by = c("PNR_KD", "PNR_KD_QT", "VMO_ZAHL_KTO_NR" = "KTO_NR",
                 "VMO_ZAHL_KTO_QT" = "KTO_QT"),
          keep = TRUE, suffix = c("", "_KTO_KD")) %>%
        # Kennzeichnung der AFR, die sich mit der Konto-Kunde-Gültigkeit
        # überschneiden
        dplyr::mutate(
          FLAG_UEBERSCHNEIDUNG = case_when(
            # 1. Über den Join konnte keine Gültigkeit angespielt werden,
            # d.h. es wurde entweder keine Kontonummer an der Zahlung erfasst o.
            # das Konto hängt nicht an dem durch das VMO besicherten Kunden
            is.na(KTO_NR) ~ 0,
            # 2. Konto ist in beide Richtungen unbegrenzt gültig
            is.na(KTO_PNR_KD_GUELTIG_AB) & is.na(KTO_PNR_KD_GUELTIG_BIS)
            ~ 1,
            # 3. Konto ist nach vorne unbegrenzt gültig
            is.na(KTO_PNR_KD_GUELTIG_AB) &
              MIN_AFG_DAT <= KTO_PNR_KD_GUELTIG_BIS ~ 1,
            # 4. Konto ist nach hinten unbegrenzt gültig
            # 4a. Ausfallreihe ist nicht beendet
            is.na(KTO_PNR_KD_GUELTIG_BIS) & is.na(AFR_KD_ENDE) ~ 1,
            # 4b. Ausfallreihe ist beendet
            is.na(KTO_PNR_KD_GUELTIG_BIS) & !is.na(AFR_KD_ENDE) &
              KTO_PNR_KD_GUELTIG_AB <= AFR_KD_ENDE ~ 1,
            # 5. Konto ist in beide Richtungen begrenzt gültig
            # 5a. Ausfallreihe ist nicht beendet
            !is.na(KTO_PNR_KD_GUELTIG_AB) &
              !is.na(KTO_PNR_KD_GUELTIG_BIS) &
              is.na(AFR_KD_ENDE) &
              KTO_PNR_KD_GUELTIG_BIS >= MIN_AFG_DAT ~ 1,
            # 5b. Ausfallreihe ist beendet
            !is.na(KTO_PNR_KD_GUELTIG_AB) &
              !is.na(KTO_PNR_KD_GUELTIG_BIS) &
              !is.na(AFR_KD_ENDE) &
              KTO_PNR_KD_GUELTIG_BIS >= MIN_AFG_DAT &
              KTO_PNR_KD_GUELTIG_AB <= AFR_KD_ENDE ~ 1,
            # 6. Sonst
            TRUE ~ 0)) %>%
        # Bestimme benötigte Selektionskriterien pro VMO-Zahlung
        dplyr::group_by(VMO_CODE, VMO_QT, VMO_ZAHL_ID,
                        VMO_ZAHL_QT, VMO_ZAHL_KLASSE) %>%
        dplyr::mutate(
          # Anzahl unterschiedlicher Kunden
          ANZAHL_KD = n_distinct(PNR_KD),
          # Anzahl Überschneidungen von Kundenausfallreihen mit der
          # Konto-Kunde-Gültigkeit
          ANZAHL_UEBERSCHNEIDUNGEN = sum(FLAG_UEBERSCHNEIDUNG),
          # höchste EAD der Kundenausfallreihen
          MAX_KD_MIN_AFG_EAD = if (all(is.na(KD_MIN_AFG_EAD))) NA_real_ else
           max(KD_MIN_AFG_EAD, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        # Jetzt wird pro VMO-Zahlung die relevante Kundenausfallreihe
        # ausgewählt
        dplyr::filter(
          # VMO-Zahlung hängt nur an einem Kunden
          ANZAHL_KD == 1 |
            # VMO-Zahlung hängt an mehreren Kunden und es gibt nur eine
            # Überschneidung der Konto-Kunde-Gültigkeit mit einer AFR
            (ANZAHL_KD > 1 & ANZAHL_UEBERSCHNEIDUNGEN == 1 &
               FLAG_UEBERSCHNEIDUNG == 1) |
            # VMO-Zahlung hängt an mehreren Kunden und es mehrere oder keine
            # Überschneidungen der Konto-Kunde-Gültigkeit mit einer AFR
            (ANZAHL_KD > 1 & ANZAHL_UEBERSCHNEIDUNGEN != 1 &
               KD_MIN_AFG_EAD == MAX_KD_MIN_AFG_EAD)
        ) %>%
        # Nach diesem Schritt könnten pro VMO-Zahlung immernoch mehrere
        # Kundenausfallreihen vorliegen, falls mehrere Ausfallreihen ein
        # identisches EAD besitzen - hier soll jetzt die Kundenausfallreihe
        # mit der kleinsten Personennummer ausgewählt werden
        dplyr::group_by(VMO_CODE, VMO_QT, VMO_ZAHL_ID,
                        VMO_ZAHL_QT, VMO_ZAHL_KLASSE) %>%
        dplyr::filter(PNR_KD == min(PNR_KD)) %>%
        dplyr::mutate(
          ANZAHL_AFR_PRO_ZAHLUNG = n()
        ) %>%
        dplyr::ungroup()

      # iii) Zusammenführen der zu Kundenausfallreihen zugeordneten
      # VMO-Zahlungen
      #____________________________________________________________
      vmo_zahlungen <- vmo_zahlungen_relevant %>%
        dplyr::left_join(
          dplyr::union(
            # Zahlungen, die über die PNR einer AFR zugeordnet wurden
            vmo_zahlungen_via_pnr %>%
              dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, VMO_CODE, VMO_QT,
                            VMO_ZAHL_ID, VMO_ZAHL_QT, VMO_ZAHL_KLASSE),
            # Zahlungen, die über den VMO_CODE einer AFR zugeordnet wurden
            vmo_zahlungen_via_vmo %>%
              dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, VMO_CODE, VMO_QT,
                            VMO_ZAHL_ID, VMO_ZAHL_QT, VMO_ZAHL_KLASSE),
            ),
          by = c("VMO_CODE", "VMO_QT", "VMO_ZAHL_ID", "VMO_ZAHL_QT",
                 "VMO_ZAHL_KLASSE")) %>%
        # Entfenrne die VMO-Zahlungen, die keiner Kundenausfallreihe zugeordnet
        # werden konnten
        dplyr::filter(!is.na(PNR_KD)) %>%
        # Anspielen von Informationen zur Kundenausfallreihe
        dplyr::left_join(basis_afr_kunde %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                         MIN_AFG_DAT, MIN_C_AFG_DAT,
                                         MAX_AFG_ENDE, AFR_KD_ENDE),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        # Anspielen der Diskontierungszinssätze
        dplyr::left_join(disk_zins %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                         Quartal_Zins),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::mutate(
          ENDE_DAT = dplyr::coalesce(AFR_KD_ENDE, MAX_AFG_ENDE)
        ) %>%
        # Bestimmung der Zahlungsphase einer Zahlung
        #___________________________________________
        dplyr::mutate(
          Z_PHASE = dplyr::case_when(
            # 1. Zahlungsdatum <> Ausfalldatum
            # 1.a Vor Ausfall
            VMO_ZAHL_DAT < MIN_AFG_DAT ~ "VOR_AUSFALL",
            # 1.b Sanierung
            MIN_AFG_DAT < VMO_ZAHL_DAT &
              VMO_ZAHL_DAT < dplyr::coalesce(MIN_C_AFG_DAT, ENDE_DAT) ~ "SAN",
            # 1.c Nach C-Ausfall
            !is.na(MIN_C_AFG_DAT) &
              MIN_C_AFG_DAT < VMO_ZAHL_DAT &
              VMO_ZAHL_DAT <= ENDE_DAT ~ "NACH_C",
            # 1.d Nach Ende der AFR
            ENDE_DAT < VMO_ZAHL_DAT ~ "NACH_ENDE",
            # 2. Zahlungsdatum == Ausfalldatum
            # 2.a Vor Ausfall
            VMO_ZAHL_DAT > izb_dat &  VMO_ZAHL_DAT == MIN_AFG_DAT ~
              "VOR_AUSFALL",
            # 2.b Sanierung
            VMO_ZAHL_DAT <= izb_dat &  VMO_ZAHL_DAT == MIN_AFG_DAT &
              (is.na(MIN_C_AFG_DAT) | MIN_AFG_DAT != MIN_C_AFG_DAT) ~ "SAN",
            VMO_ZAHL_DAT > izb_dat & !is.na(MIN_C_AFG_DAT) &
              MIN_AFG_DAT != MIN_C_AFG_DAT & VMO_ZAHL_DAT == MIN_C_AFG_DAT
            ~ "SAN",
            # 2.c Nach C-Ausfall
             VMO_ZAHL_DAT <= izb_dat & !is.na(MIN_C_AFG_DAT) &
               VMO_ZAHL_DAT == MIN_C_AFG_DAT ~ "NACH_C",
            # Sonst
            TRUE ~ "NICHT_RELEVANT")
        ) %>%
        # Entferne die nicht relevanten Buchungen aus der Tabelle
        # (es sollte keine geben)
        dplyr::filter(Z_PHASE != "NICHT_RELEVANT") %>%
        # Anspielen der Information, ob das jeweilige Konto, dem die
        # VMO-Zahlung zugeordnet ist, zum ersten C-Ausfall ein Guthaben besitzt
        dplyr::left_join(guthaben_kto_min_c_afg,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD",
                                "VMO_ZAHL_KTO_NR" = "KTO_NR",
                                "VMO_ZAHL_KTO_QT" = "KTO_QT")) %>%
        dplyr::mutate(
          FLAG_KTO_MIT_GUTH_C = tidyr::replace_na(FLAG_KTO_MIT_GUTH_C, 0)
        ) %>%
        dplyr::mutate(
          # Selektiere die VMO-Zahlungen auf C-Guthaben-Konten
          #___________________________________________________
          BETRAG_KTO_MIT_GUTH_C = dplyr::if_else(FLAG_KTO_MIT_GUTH_C == 1,
                                                 VMO_ZAHL_BETRAG, 0),
          # Selektiere die IMMO-Zahlungen
          #______________________________
          IMMO_BETRAG = dplyr::if_else(VMO_TYP_1 == "IMMO",
                                       VMO_ZAHL_BETRAG, 0),
          # Festlegen ob VMO-Zahlung VE, VK, VE_2 oder VK_2 ist
          #____________________________________________________
          Z_ART = dplyr::case_when(
            AIRBA_KNZ == TRUE & ERLOES == TRUE ~ "VE",
            AIRBA_KNZ == TRUE & ERLOES == FALSE ~ "VK",
            AIRBA_KNZ == FALSE & ERLOES == TRUE ~ "VE_2",
            AIRBA_KNZ == FALSE & ERLOES == FALSE ~ "VK_2"),
          # Vorzeichen bestimmen
          #_____________________
          # Wird benötigt bei der Summenbildung innerhalb einer Z_ART.
          VZ = dplyr::case_when(
            # Forderungserhöhende Buchungen (Sollbuchungen)
            Z_ART %in% c("VK", "VK_2") & VMO_ZAHL_S_H == "H" ~ -1,
            # Forderungsreduzierende Buchungen (Habenbuchungen)
            Z_ART %in% c("VE", "VE_2") & VMO_ZAHL_S_H == "S" ~ -1,
            # Sonst
            TRUE ~ 1),
          # Wird benötigt wenn man verschiedene Z_ART zusammenrechnet.
          VZ_S_H = dplyr::case_when(
            # Forderungserhöhende Buchungen (Sollbuchungen)
            Z_ART %in% c("VK", "VK_2") ~ -1,
            # Forderungsreduzierende Buchungen (Habenbuchungen)
            Z_ART %in% c("VE", "VE_2") ~ 1,
            # Sonst
            TRUE ~ 0),
          # Bestimme die gewichteten und diskontierten Zahlungsbeträge
          #___________________________________________________________
          DISK_TAGE =
            as.numeric(dplyr::coalesce(VMO_ZAHL_VALUTA,  VMO_ZAHL_DAT))
          - as.numeric(MIN_AFG_DAT),
          BETRAG_GEW = round(VMO_ZAHL_BETRAG * (DISK_TAGE / 365.25), 2),
          BETRAG_DISK = round(VMO_ZAHL_BETRAG * (1 / (1 + Quartal_Zins / 100))^
                                   (DISK_TAGE / 365.25), 2),
          IMMO_BETRAG_DISK =
            round(IMMO_BETRAG * (1 / (1 + Quartal_Zins / 100))^
                                     (DISK_TAGE / 365.25), 2),
          DISK_TAGE_SYNTH_NACH_C =
            (as.numeric(dplyr::coalesce(MIN_C_AFG_DAT, MIN_AFG_DAT))
             - as.numeric(MIN_AFG_DAT)),
          BETRAG_KTO_MIT_GUTH_C_GEW =
            round(BETRAG_KTO_MIT_GUTH_C * (DISK_TAGE_SYNTH_NACH_C /
                                                365.25), 2),
          BETRAG_KTO_MIT_GUTH_C_DISK = round(BETRAG_KTO_MIT_GUTH_C *
                              (1 / (1 + Quartal_Zins / 100))^
                              (DISK_TAGE_SYNTH_NACH_C / 365.25), 2)
        ) %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD) %>%
        dplyr::mutate(
          AFR_KD_MAX_VMO_ZAHL_DAT =
            if (all(is.na(VMO_ZAHL_DAT[Z_PHASE %in% c("SAN", "NACH_C")])))
              NA_Date_
          else
            max(VMO_ZAHL_DAT[Z_PHASE %in% c("SAN", "NACH_C")], na.rm = TRUE)

        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD, AFR_KD_MAX_VMO_ZAHL_DAT,
                        Z_ART, Z_PHASE, VZ_S_H) %>%
        dplyr::summarise(
          # Summe der tatsächlich erfassten Zahlungen
          SUM = sum(VMO_ZAHL_BETRAG * VZ),
          SUM_GEW = sum(BETRAG_GEW * VZ),
          SUM_DISK = sum(BETRAG_DISK * VZ),
          # Summe der IMMO-Zahlungen
          SUM_IMMO = sum(IMMO_BETRAG * VZ),
          SUM_IMMO_DISK = sum(IMMO_BETRAG_DISK * VZ),
          # Summe der Buchungen der C-Guthaben-Konten in der KD-AFR
          SUM_BETRAG_KTO_MIT_GUTH_C = sum(BETRAG_KTO_MIT_GUTH_C * VZ),
          SUM_BETRAG_KTO_MIT_GUTH_C_GEW = sum(BETRAG_KTO_MIT_GUTH_C_GEW * VZ),
          SUM_BETRAG_KTO_MIT_GUTH_C_DISK = sum(BETRAG_KTO_MIT_GUTH_C_DISK * VZ)
          ) %>%
        dplyr::ungroup()


      # Dataframe mit den tatsächlich gebuchten Zahlungssummen
      #_______________________________________________________
      kunde_afr_vmo_zahl_real_tmp <- vmo_zahlungen %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, AFR_KD_MAX_VMO_ZAHL_DAT,
                      Z_ART, Z_PHASE, SUM, SUM_GEW, SUM_DISK, SUM_IMMO,
                      SUM_IMMO_DISK) %>%
        tidyr::pivot_wider(
          names_from = c(Z_ART, Z_PHASE),
          values_from = c(SUM, SUM_GEW, SUM_DISK, SUM_IMMO, SUM_IMMO_DISK),
          values_fill = as.numeric(0.00),
          names_glue = "KD_{Z_ART}_{Z_PHASE}_{.value}"
        ) %>%
        # Korrektur der Spaltennamen
        dplyr::rename_with(
          function(x) {
            stringr::str_replace(x, "_SUM", "")
            },
          dplyr::contains("SUM"))

      # Definiere die Spalten, die mindestens in der Zwischentabelle
      # enthalten sein müssen. Dies ist notwendig, da in
      # kunde_afr_vmo_zahl_real_tmp aktuell noch Spalten fehlen könnten, wenn
      # entsprechende Zahlungen im Datenbestand fehlen.
      colnames_vmo_zahl_real <-
        c("PNR_KD", "PNR_KD_QT", "AFR_KD", "AFR_KD_MAX_VMO_ZAHL_DAT",
          "KD_VE_SAN", "KD_VE_SAN_DISK", "KD_VK_SAN", "KD_VK_SAN_DISK",
          "KD_VE_NACH_C", "KD_VE_NACH_C_DISK", "KD_VK_NACH_C",
          "KD_VK_NACH_C_DISK", "KD_VE_NACH_ENDE", "KD_VK_NACH_ENDE",
          "KD_VE_SAN_IMMO", "KD_VE_SAN_IMMO_DISK", "KD_VK_SAN_IMMO",
          "KD_VK_SAN_IMMO_DISK", "KD_VE_NACH_C_IMMO", "KD_VE_NACH_C_IMMO_DISK",
          "KD_VK_NACH_C_IMMO", "KD_VK_NACH_C_IMMO_DISK",
          "KD_VE_NACH_ENDE_IMMO", "KD_VK_NACH_ENDE_IMMO", "KD_VE_2_NACH_C",
          "KD_VE_2_NACH_C_GEW", "KD_VE_2_NACH_C_DISK", "KD_VK_2_NACH_C",
          "KD_VK_2_NACH_C_GEW", "KD_VK_2_NACH_C_DISK", "KD_VE_2_NACH_ENDE",
          "KD_VK_2_NACH_ENDE")

      kunde_afr_vmo_zahl_real <- suppressMessages(
        # Bilde einen leeren tibble mit den benötigten Spalten
        tibble::as_tibble(matrix(nrow = 0,
                                 ncol = length(colnames_vmo_zahl_real),
                                 dimnames =
                                   list(NULL, colnames_vmo_zahl_real))) %>%
          # Datentypen festlegen, damit der anschließende Join funktioniert
          dplyr::mutate(
            AFR_KD = as.numeric(AFR_KD),
            dplyr::across(tidyselect::contains(c("PNR_KD", "VE", "VK")),
                          as.numeric),
            dplyr::across(tidyselect::contains(c("DAT")), as.Date),
            dplyr::across(c("PNR_KD_QT"), as.integer)
            ) %>%
          # Anspielen der tatsächlich vorliegenden aggregierten Zahlungen
          dplyr::full_join(kunde_afr_vmo_zahl_real_tmp) %>%
          # fehlende Werte in Zahlungsspalten durch 0 ersetzen
          dplyr::mutate(
            dplyr::across(
              tidyselect::contains(c("VE", "VK")),
              ~tidyr::replace_na(.x, 0.00))
            ) %>%
          # Einschränkung auf die benötigten Spalten
          dplyr::select(tidyselect::all_of(colnames_vmo_zahl_real))
      )
      # Dataframe mit den synthetischen Zahlungssummen
      #_______________________________________________
      kunde_afr_vmo_zahl_synth <- vmo_zahlungen %>%
        # es werden nur die Zahlungen der Z-Phase "NACH_C" benötigt
        dplyr::filter(Z_PHASE == "NACH_C") %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD) %>%
        dplyr::summarise(
          # für die C-Guthabenkonten wird hier die Summe über alle
          # VMO-Zahlungen (VE, VK, VE_2, VK_2) gebildet
          VMO_GUTH_C_NACH_C = sum(SUM_BETRAG_KTO_MIT_GUTH_C * VZ_S_H),
          VMO_GUTH_C_NACH_C_GEW = sum(SUM_BETRAG_KTO_MIT_GUTH_C_GEW * VZ_S_H),
          VMO_GUTH_C_NACH_C_DISK = sum(SUM_BETRAG_KTO_MIT_GUTH_C_DISK * VZ_S_H)
        ) %>%
        dplyr::ungroup()

      # 3.6 FMP-Verlustdifferenz ####
      #______________________________
      logging::loginfo("Berechnung der FMP-Verlustdifferenz")
      # Quelle: T_3000_VQ_Ergebnisse
      #
      # Folgende (Zwischen-)Parameter werden ermittelt:
      # - VMO_FMP_VD_BETRAG_REAL_disk
      # - VMO_FMP_VD_BETRAG_PROG_disk
      # - KD_FMP_VD_BETRAG_DISK
      #
      # - KD_FMP_VD_ANMERK

      # Hilfstabelle zur Priorisierung der FMP-Zahlungen am Objekt auf AFR und
      # Kunde (Bspw. für Fälle wie VMO: "9950-11-17-17.58.44.911107")
      t_prio_vmo_afr <- basis_vmo_afr %>%
        # Sortierung für Priosierung -> MIN_AFG_DAT absteigend, PNR_KD
        # aufsteigend
        dplyr::arrange(VMO_CODE, VMO_QT, desc(MIN_AFG_DAT), PNR_KD,
                       PNR_KD_QT) %>%
        # Gruppierung nach Objekt
        dplyr::group_by(VMO_CODE, VMO_QT) %>%
        # ID-Vergabe entsprechend der Sortierung nach MIN_AFG_DAT absteigend
        # und PNR_KD aufsteigend
        dplyr::mutate(id = row_number()) %>%
        dplyr::select(VMO_CODE, VMO_QT, id, PNR_KD, PNR_KD_QT, AFR_KD,
                      MIN_AFG_DAT) %>%
        # Filter auf ID 1 -> je VMO wird nur der Datensatz mit dem spätesten
        # MIN_AFG_DAT verwendet, gibt es identische MIN_AFG_DATs, so wird
        # der erste Kunde nach aufsteigender Kundennumer ausgewählt.
        dplyr::filter(id == 1) %>%
        dplyr::ungroup()

      # Hilfstabelle zur Priorisierung der FMP-Zahlungen am Objekt auf den
      # Kunde.
      t_prio_vmo_kd <- basis_vmo_afr %>%
        # Sortierung für Priosierung -> MIN_AFG_DAT absteigend, PNR_KD
        # aufsteigend
        dplyr::arrange(VMO_CODE, VMO_QT, PNR_KD, PNR_KD_QT) %>%
        # Gruppierung nach Objekt
        dplyr::group_by(VMO_CODE, VMO_QT) %>%
        # ID-Vergabe entsprechend der Sortierung nach MIN_AFG_DAT absteigend
        # und PNR_KD aufsteigend
        dplyr::mutate(prio_id_kd = row_number()) %>%
        dplyr::select(VMO_CODE, VMO_QT, prio_id_kd, PNR_KD, PNR_KD_QT
                      ) %>%
        # Filter auf ID 1 -> je VMO wird nur der Datensatz mit dem spätesten
        # MIN_AFG_DAT verwendet, gibt es identische MIN_AFG_DATs, so wird
        # der erste Kunde nach aufsteigender Kundennumer ausgewählt.
        dplyr::filter(prio_id_kd == 1) %>%
        dplyr::ungroup()

      fmp_verlustdiff_vmo <- basis_vmo_afr %>%
        # FMP-Input aus den VQ-Ergebnissen
        dplyr::left_join(parameter_vq, by = c("VMO_CODE", "VMO_QT")) %>%
        # Diskontierungszins am Kunden
        dplyr::left_join(disk_zins, by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::left_join(t_prio_vmo_afr, by = c("VMO_CODE", "VMO_QT", "PNR_KD",
                                               "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::left_join(t_prio_vmo_kd, by = c("VMO_CODE", "VMO_QT", "PNR_KD",
                                                "PNR_KD_QT")) %>%
        dplyr::mutate(
          # Ende einer Ausfallreihe
          ENDE_DAT = dplyr::coalesce(AFR_KD_ENDE, MAX_AFG_ENDE),
          # Flag ob FMP-Datenfeld innerhalb einer AFR liegt
          in_dat =
            dplyr::if_else(
              is.na(VMO_FMP_VD_AVG_ZAHL_DAT),  FALSE,
              dplyr::if_else(VMO_FMP_VD_AVG_ZAHL_DAT > MIN_AFG_DAT.x &
                               VMO_FMP_VD_AVG_ZAHL_DAT <= ENDE_DAT &
                               !is.na(prio_id_kd), TRUE, FALSE)),
          is_max_min_afg_dat =
            dplyr::if_else(is.na(VMO_FMP_VD_AVG_ZAHL_DAT) &
                             !is.na(id), TRUE, FALSE),
          # Nullsetzen der nach den disjunkten Kriterien "in_dat" und
          # "is_max_min_afg_dat" ausgeschlossenen Beträgen
          VMO_FMP_VD_BETRAG_REAL_rel =
            dplyr::if_else(in_dat | is_max_min_afg_dat,
                    dplyr::coalesce(VMO_FMP_VD_BETRAG_REAL, 0), 0),
          VMO_FMP_VD_BETRAG_PROG_rel =
            dplyr::if_else(in_dat | is_max_min_afg_dat,
                    dplyr::coalesce(VMO_FMP_VD_BETRAG_PROG, 0), 0),
          # setzen der Flags für die Aggregation des Feldes VMO_FMP_VD_ANMERK
          DQB_flag =
            dplyr::if_else(substr(VMO_FMP_VD_ANMERK, 1, 4) == "DQB:", 1, 0),
          X_flag =
            dplyr::if_else(substr(VMO_FMP_VD_ANMERK, 1, 2) == "X:", 1, 0),
          S_flag =
            dplyr::if_else(substr(VMO_FMP_VD_ANMERK, 1, 2) == "S:", 1, 0),
          R_flag =
            dplyr::if_else(substr(VMO_FMP_VD_ANMERK, 1, 2) == "R:", 1, 0),
          # Bestimmung der Diskontierungstage
          disk_tage = as.numeric(
            dplyr::coalesce(VMO_FMP_VD_AVG_ZAHL_DAT, MIN_C_AFG_DAT,
                               MIN_AFG_DAT.x) - MIN_AFG_DAT.x),
          # Diskontierung des realisierten Betrags der FMP-Verlustdifferenz
          VMO_FMP_VD_BETRAG_REAL_disk =
            as.numeric(VMO_FMP_VD_BETRAG_REAL_rel *
                         (1 / (1 + Quartal_Zins / 100))^
                         (disk_tage / 365.25)),
          # Diskontierung des geschätzten Betrags der FMP-Verlustdifferenz
          VMO_FMP_VD_BETRAG_PROG_disk =
            as.numeric(VMO_FMP_VD_BETRAG_PROG_rel *
                         (1 / (1 + Quartal_Zins / 100))^
                         (disk_tage / 365.25)))

      # Aggregation auf Kunden-AFR-Ebene
      fmp_verlustdiff <- fmp_verlustdiff_vmo %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD) %>%
        dplyr::summarise(
          DQB_flag_sum = sum(dplyr::coalesce(DQB_flag, 0)),
          X_flag_sum = sum(dplyr::coalesce(X_flag, 0)),
          S_flag_sum = sum(dplyr::coalesce(S_flag, 0)),
          R_flag_sum = sum(dplyr::coalesce(R_flag, 0)),
          KD_FMP_VD_BETRAG_DISK = sum(round(
            dplyr::if_else(dplyr::coalesce(R_flag, 0) == 1,
                    dplyr::coalesce(VMO_FMP_VD_BETRAG_REAL_disk, 0),
                    dplyr::if_else(dplyr::coalesce(S_flag, 0) == 1,
                            dplyr::coalesce(VMO_FMP_VD_BETRAG_PROG_disk, 0),
                            0)), 2))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          KD_FMP_VD_ANMERK = case_when(
            DQB_flag_sum > 0 ~ "DQB",
            X_flag_sum > 0 ~ "X",
            S_flag_sum > 0 ~ "S",
            R_flag_sum > 0 & DQB_flag_sum == 0 & X_flag_sum == 0 &
                    S_flag_sum == 0 ~ "R",
            TRUE ~ "-")
        ) %>%
        dplyr::select(-tidyselect::contains("flag"))

      # 3.7 LGD Vorverarbeitung ####
      #     und finale Parameterbestimmung und -zusammenführung
      #------------------------------------------------------------------------#
      msg <- "Zusammenführung der Zwischenergebnisse zur Parametertabelle"
      logging::loginfo(msg)
      # Zusammenführen der Zahlungen aus den Kundenbuchungen und den VMO-
      # Zahlungen und Bestimmung der restlichen Spalten
      #
      # Input: Zwischentabellen aus den Schritten 3.1 - 3.4
      #
      # Weitere Spalten, die in diesem Abschnitt bestimmt werden:
      # - KD_EZ_SAN
      # - KD_EZ_SAN_GEW
      # - KD_EZ_SAN_DISK
      #
      # - KD_SUM_U_NACH_C_GUTHABEN_KTO
      #
      # - Berücksichtigung der synthetischen Zahlungen von Kreditkarten und
      # Guthabenkonten in den EE_NACH_C

      lgd_vorverarbeitung <- basis_afr_kunde %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, AFR_KD_STATUS,
                      MIN_AFG_DAT, MIN_C_AFG_DAT, AFR_KD_ENDE,
                      MAX_AFG_ENDE) %>%
        # Anspielen der Diskontierungszinssätze
        dplyr::left_join(disk_zins %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                         Quartal_Zins),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        # Anspielen der Kundensalden
        dplyr::left_join(salden_kd %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                         tidyselect::contains("SALDO")),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        # Anspielen der VMO_Zahlungen
        dplyr::left_join(kunde_afr_vmo_zahl_real,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::left_join(kunde_afr_vmo_zahl_synth,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        # Anspielen der Kundenbuchungen
        dplyr::left_join(kunde_afr_kd_bu_real,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::left_join(kunde_afr_kd_bu_synth,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        # Anspielen der FMP-Verlustdifferenzen
        dplyr::left_join(fmp_verlustdiff,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        # Anspielen der Summe der C-Salden der C-Guthaben-Konten in der AFR
        dplyr::left_join(guthaben_kto_min_c_afg %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                         KD_MIN_C_AFG_SALDO_GUTHABEN_KTO) %>%
                           dplyr::distinct(),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        # Ersetze NA-Werte in den Zahlungsspalten durch 0.00
        dplyr::mutate(
          dplyr::across(
            tidyselect::contains(c("VE", "VK", "EE", "EK", "AVAL", "FVZ",
                                   "ZINS", "K_KARTEN", "FMP_VD_BETRAG",
                                   "GUTH_C", "SALDO")),
                                 ~tidyr::replace_na(.x, 0.00)),
          KD_FMP_VD_ANMERK = tidyr::replace_na(KD_FMP_VD_ANMERK, "-")
          ) %>%
        dplyr::mutate(
          # Berechnung der Einbringungszahlungen in Sanierung
          ENDE_DAT = dplyr::coalesce(AFR_KD_ENDE, MAX_AFG_ENDE),
          DISK_TAGE = dplyr::if_else(
            is.na(MIN_C_AFG_DAT),
            as.numeric((ENDE_DAT - MIN_AFG_DAT) / 2),
            as.numeric((MIN_C_AFG_DAT - MIN_AFG_DAT) / 2)
          ),
          KD_EZ_SAN = dplyr::case_when(
            # Es gibt keine Sanierungsphase
            !is.na(MIN_C_AFG_DAT) & MIN_AFG_DAT == MIN_C_AFG_DAT ~ 0,
            # Sanierungsphase durch C-Ausfall beendet
            !is.na(MIN_C_AFG_DAT) & MIN_AFG_DAT != MIN_C_AFG_DAT
            ~ round(KD_MIN_AFG_SALDO + KD_AVAL_IA_SAN - KD_MIN_C_AFG_SALDO -
              (KD_VE_SAN - KD_VK_SAN) - KD_FVZ_SAN + KD_ZINS_SAN, 2),
            # Sonst
            TRUE ~ round(KD_MIN_AFG_SALDO + KD_AVAL_IA_SAN - KD_FVZ_SAN +
              KD_ZINS_SAN, 2)
            ),
          KD_EZ_SAN_GEW = round(KD_EZ_SAN * DISK_TAGE / 365.25, 2),
          KD_EZ_SAN_DISK = round(KD_EZ_SAN * (1 / (1 + Quartal_Zins / 100))^
                                   (DISK_TAGE / 365.25), 2),
          # Berechnung der Umsatzsumme der C-Guthaben-Konten
          KD_SUM_U_NACH_C_GUTHABEN_KTO =
            KU_GUTH_C_NACH_C + VMO_GUTH_C_NACH_C,
          KD_SUM_U_NACH_C_GUTHABEN_KTO_GEW =
            KU_GUTH_C_NACH_C_GEW + VMO_GUTH_C_NACH_C_GEW,
          KD_SUM_U_NACH_C_GUTHABEN_KTO_DISK =
            KU_GUTH_C_NACH_C_DISK + VMO_GUTH_C_NACH_C_DISK,
          FLAG_GUTH_C_RELEVANT =
            dplyr::if_else(KD_SUM_U_NACH_C_GUTHABEN_KTO < 0, 1, 0)
        ) %>%
        dplyr::mutate(
          # Ersetze NA-Werte in den EZ_SAN durch 0.00
          dplyr::across(
            tidyselect::contains("EZ_SAN"), ~tidyr::replace_na(.x, 0.00)),
          KD_EE_NACH_C = KD_EE_NACH_C + K_KARTEN_NACH_C +
            (-1) * FLAG_GUTH_C_RELEVANT * KD_SUM_U_NACH_C_GUTHABEN_KTO,
          KD_EE_NACH_C_GEW = KD_EE_NACH_C_GEW + K_KARTEN_NACH_C_GEW +
            (-1) * FLAG_GUTH_C_RELEVANT * KD_SUM_U_NACH_C_GUTHABEN_KTO_GEW,
          KD_EE_NACH_C_DISK = KD_EE_NACH_C_DISK + K_KARTEN_NACH_C_DISK +
            (-1) * FLAG_GUTH_C_RELEVANT * KD_SUM_U_NACH_C_GUTHABEN_KTO_DISK,
        ) %>%
        # Spaltenreihenfolge festlegen
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, AFR_KD_STATUS,
                      KD_MIN_AFG_SALDO, KD_MIN_C_AFG_SALDO,
                      KD_RM_SALDO, KD_RM_SALDO_DISK,
                      KD_AVAL_IA_SAN, KD_AVAL_IA_NACH_C,
                      KD_VE_SAN, KD_VE_SAN_DISK,
                      KD_VK_SAN, KD_VK_SAN_DISK,
                      KD_VE_NACH_C, KD_VE_NACH_C_DISK,
                      KD_VK_NACH_C, KD_VK_NACH_C_DISK,
                      KD_VE_NACH_ENDE, KD_VK_NACH_ENDE,
                      KD_VE_SAN_IMMO, KD_VE_SAN_IMMO_DISK,
                      KD_VK_SAN_IMMO, KD_VK_SAN_IMMO_DISK,
                      KD_VE_NACH_C_IMMO, KD_VE_NACH_C_IMMO_DISK,
                      KD_VK_NACH_C_IMMO, KD_VK_NACH_C_IMMO_DISK,
                      KD_VE_NACH_ENDE_IMMO, KD_VK_NACH_ENDE_IMMO,
                      KD_EZ_SAN, KD_EZ_SAN_GEW, KD_EZ_SAN_DISK,
                      KD_EE_NACH_C, KD_EE_NACH_C_GEW, KD_EE_NACH_C_DISK,
                      KD_EK_NACH_C, KD_EK_NACH_C_GEW, KD_EK_NACH_C_DISK,
                      KD_EE_NACH_ENDE, KD_EK_NACH_ENDE,
                      KD_VE_2_NACH_C, KD_VE_2_NACH_C_GEW, KD_VE_2_NACH_C_DISK,
                      KD_VK_2_NACH_C, KD_VK_2_NACH_C_GEW, KD_VK_2_NACH_C_DISK,
                      KD_VE_2_NACH_ENDE, KD_VK_2_NACH_ENDE,
                      KD_FVZ_SAN, KD_FVZ_NACH_C, KD_FVZ_NACH_ENDE,
                      KD_ZINS_SAN, KD_ZINS_NACH_C, KD_ZINS_NACH_ENDE,
                      KD_VE_OHNE_VMO, KD_VK_OHNE_VMO,
                      KD_SUM_U_NACH_C_GUTHABEN_KTO,
                      KD_MIN_C_AFG_SALDO_GUTHABEN_KTO,
                      KD_FMP_VD_ANMERK, KD_FMP_VD_BETRAG_DISK,
                      AFR_KD_MAX_VMO_ZAHL_DAT, AFR_KD_MAX_BU_DAT)

      # 4. Schritt: Data-Dictionary für Parametertabelle ####
      logging::loginfo("Befüllung Data-Dictionary")
      create_data_dictionary(vdkx,
                             data = lgd_vorverarbeitung,
                             data_description =
                               get_col_tab_description(vdkx, tablename),
                             tablename = tablename)

      # 5. Schritt: Speichern / Protokollierung Parametertabelle ####
      arrow::write_parquet(lgd_vorverarbeitung,
                           file.path(path_exchange,
                                     paste0(tablename, ".parquet")))
      insert_table_protocol(vdkx, tablename, nrow(lgd_vorverarbeitung))
    } else {
      loginfo(paste(vdkx$info_msg_create_basis_not_done, tablename))
    }
  }
  logging::loginfo(paste(vdkx$info_msg_function_end,
                         create_info_string(),
                         "create_parameter_lgd_vv"))
}

#' Erstellt die finale LGD-Parametertabelle
#'
#' In dieser Funktion wird die LGD-Parametertabelle erstellt
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
#' @family vdkx_parameter
create_parameter_lgd <- function(vdkx,
                                 tablename = NULL,
                                 path_exchange = NULL,
                                 path_input = NULL) {

  logging::loginfo("=========================================================")
  if (vdkx$used_loglevel %in% c("INFO", "DEBUG")) {
    tictoc::tic(" create_parameter_lgd")
  }
  logging::loginfo(paste(vdkx$info_msg_function_begin,
                         "create_parameter_lgd"))
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
    input_kd_kusy_pu <-
      import_parquet_file(vdkx,
                          path_input,
                          file = "INPUT_KD_KuSy_1_Partial_Use.parquet",
                          dq_string = "PARAMETER_LGD_Ergebnisse")
    basis_kunde <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1104_Kunde.parquet",
                          dq_string = "PARAMETER_LGD_Ergebnisse")
    basis_kunde_konto <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1108_Konto_Kunde.parquet",
                          dq_string = "PARAMETER_LGD_Ergebnisse")
    basis_afr_kunde <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1140_Ausfallreihe_Kunde.parquet",
                          dq_string = "PARAMETER_LGD_Ergebnisse")
    basis_afr_konto <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1220_Ausfallreihe_Konto.parquet",
                          dq_string = "PARAMETER_LGD_Ergebnisse")
    basis_kunde_sire <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1310_Kunde_SIRE.parquet",
                          dq_string = "PARAMETER_LGD_Ergebnisse")
    basis_vmo_afr <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1320_VMO_Ausfallreihen.parquet",
                          dq_string = "PARAMETER_LGD_Ergebnisse")
    basis_vmo <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_1350_VMO.parquet",
                          dq_string = "PARAMETER_LGD_Ergebnisse")
    parameter_shver_afr <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_3100_SHVER_Ausfallreihe_Kunde.parquet",
                          dq_string = "PARAMETER_LGD_Ergebnisse")
    parameter_lgd_vv <-
      import_parquet_file(vdkx,
                          path_exchange,
                          file = "T_5510_LGD_Vorverarbeitung.parquet",
                          dq_string = "PARAMETER_LGD_Ergebnisse")

    if (state_function != vdkx$state_error
        & !is.null(vdkx$v_VDH_Schluessel_PNR_KuSy_9_13)
        & !is.null(vdkx$v_VDH_SVZ_PNR_KuSy_Ausland_Code)
        & !is.null(input_kd_kusy_pu)
        & !is.null(basis_kunde)
        & !is.null(basis_kunde_konto)
        & !is.null(basis_afr_kunde)
        & !is.null(basis_afr_konto)
        & !is.null(basis_kunde_sire)
        & !is.null(basis_vmo_afr)
        & !is.null(basis_vmo)
        & !is.null(parameter_shver_afr)
        & !is.null(parameter_lgd_vv)
    ) {
      # 2. Schritt: Data-Dictionary für Input-Dateien ####
      fill_dd_input_table(vdkx,
                          data = input_kd_kusy_pu,
                          tablename = "INPUT_KD_KuSy_1_Partial_Use")
      fill_dd_input_table(vdkx,
                          data = basis_kunde,
                          tablename = "T_1104_Kunde")
      fill_dd_input_table(vdkx,
                          data = basis_kunde_konto,
                          tablename = "T_1108_Konto_Kunde")
      fill_dd_input_table(vdkx,
                          data = basis_afr_kunde,
                          tablename = "T_1140_Ausfallreihe_Kunde")
      fill_dd_input_table(vdkx,
                          data = basis_afr_konto,
                          tablename = "T_1220_Ausfallreihe_Konto")
      fill_dd_input_table(vdkx,
                          data = basis_kunde_sire,
                          tablename = "T_1310_Kunde_SIRE")
      fill_dd_input_table(vdkx,
                          data = basis_vmo_afr,
                          tablename = "T_1320_VMO_Ausfallreihen")
      fill_dd_input_table(vdkx,
                          data = basis_vmo,
                          tablename = "T_1350_VMO")
      fill_dd_input_table(vdkx,
                          data = parameter_shver_afr,
                          tablename = "T_3100_SHVER_Ausfallreihe_Kunde")
      fill_dd_input_table(vdkx,
                          data = parameter_lgd_vv,
                          tablename = "T_5510_LGD_Vorverarbeitung")


      # 3. Schritt: Parametertabelle erzeugen ####
      #___________________________________________
      #
      # Teilschritte der Tabellenerstellung:
      #_____________________________________
      #
      # 3.1 Kundeninformationen (Typ, Branche, Herkunft, ...)
      #     --> Output: kunden_info, t_kein_vmo
      # 3.2 Ausfallreiheninformationen
      #     --> Output: t_abwicklung_ende, kto_umnhang_nach_c
      # 3.3 Kontoinformationen auf Kundenebene
      #     --> Output: afr_konto_info
      # 3.4 Erstellung der LGD-Parametertabelle
      #     --> Output: lgd_ergebnisse

      # 3.1 Kundeninformationen (Typ, Branche, Herkunft)  ####
      #_______________________________________________________
      logging::loginfo("Bestimmung der Kundeninformationen")
      # Output: kunden_info
      #
      # Folgende Spalten werden ermittelt:
      # - KD_TYP_UND_BRANCHE
      # - KD_HERKUNFT
      #
      # Output: t_kein_vmo
      #
      # Folgende Spalte wird ermittelt:
      # - KD_MIT_UNVOLLST_VMO

      # i) Bestimmung des Kundentyps, der Branche und der Herkunft
      #___________________________________________________________
      kunden_info <- basis_kunde %>%
        # Join input_kd_kusy_pu
        dplyr::left_join(input_kd_kusy_pu %>%
                           dplyr::select(PNR_KuSy, AIRBA_Segm_pu = AIRBA_Segm),
                         by = c("KD_KUNDENSYSTEMATIK" = "PNR_KuSy")) %>%
        # Relevante substrings für Joins und Ermittlungslogik
        dplyr::mutate(
          KuSy_9_13 = as.character(substr(KD_KUNDENSYSTEMATIK, 9, 13)),
          KuSy_2_4 = as.character(substr(KD_KUNDENSYSTEMATIK, 2, 4)),
          ks_1 = substr(KD_KUNDENSYSTEMATIK, 1, 1),
          ks_9_11 = substr(KD_KUNDENSYSTEMATIK, 9, 11),
          ks_6 = substr(KD_KUNDENSYSTEMATIK, 6, 6)
          ) %>%
        # Join v_VDH_Schluessel_PNR_KuSy_9_13
        dplyr::left_join(vdkx$v_VDH_Schluessel_PNR_KuSy_9_13 %>%
                           dplyr::select(KuSy_9_13,
                                         AIRBA_Segm_ku39 = AIRBA_Segm),
                         by = c("KuSy_9_13")) %>%
        # Join v_VDH_SVZ_PNR_KuSy_Ausland_Code
        dplyr::left_join(vdkx$v_VDH_SVZ_PNR_KuSy_Ausland_Code %>%
                           dplyr::select(KuSy_2_4, Land_kurz),
                         by = c("KuSy_2_4")) %>%
        dplyr::mutate(
          KD_TYP_UND_BRANCHE = dplyr::case_when(
            ((ks_1 == "0" &
                ks_9_11 %in% c("501", "502",  "503", "510", "000")) |
               (ks_1 == "6") |
               (ks_1 == "1" & ks_6 != "5" & KD_KUNDENSYSTEMATIK !=
                  "122222222222222" & is.na(AIRBA_Segm_pu)))
            ~ "ST - Staatsdienst",
            (ks_1 %in% c("0", "5")) ~ "KI - Kreditinstitut",
            (!is.na(AIRBA_Segm_pu)) ~ stringr::str_c("GK - ", AIRBA_Segm_pu),
            (dplyr::coalesce(KD_DKB_TEAM, "") %in%
               c("3301", "3311", "3312", "3313"))
              ~ "GK - Wohnungsunternehmen",
            (dplyr::coalesce(KD_DKB_TEAM, "") %in% c("2401", "3206") &
               dplyr::coalesce(KD_RISIKOVERFAHRENSSCHALTER, "") == "LBR")
            ~ "GK - Wind-Projektfinanzierung",
            (dplyr::coalesce(KD_RISIKOVERFAHRENSSCHALTER, "") == "IMO")
            ~ "GK - Grundstücks- u Wohnungswesen",
            !is.na(AIRBA_Segm_ku39) &
              ((dplyr::coalesce(KD_RISIKOVERFAHRENSSCHALTER, "") %in%
                  c("SDR", "KKR")
                & dplyr::coalesce(ks_1, "") %in% c("3", "4", "8", "9", ""))
               | (dplyr::coalesce(KD_RISIKOVERFAHRENSSCHALTER, "") == "LBR"
                  & dplyr::coalesce(ks_1, "") %in% c("3", "4", "8", "9")))
            ~ str_c("GK - ", AIRBA_Segm_ku39),
            ((dplyr::coalesce(KD_RISIKOVERFAHRENSSCHALTER, "") %in%
                c("SDR", "KKR") & ks_1 %in% c("2", "3", "4", "7", "8", "9")) |
                 ((is.na(KD_RISIKOVERFAHRENSSCHALTER) |
                     dplyr::coalesce(KD_RISIKOVERFAHRENSSCHALTER, "") == "") &
                    ks_1 %in% c("3", "4", "8", "9")))
              ~ "GK - nicht zuordenbar",
            (dplyr::coalesce(KD_RISIKOVERFAHRENSSCHALTER, "") == "SCR" &
               ks_1 %in% c("2", "3", "7", "8")) |
              ((is.na(KD_RISIKOVERFAHRENSSCHALTER) |
                  dplyr::coalesce(KD_RISIKOVERFAHRENSSCHALTER, "") == "") &
                 ks_1 %in% c("2", "7"))
            ~ "PK - Privatkunde",
            (dplyr::coalesce(KD_RISIKOVERFAHRENSSCHALTER, "") == "LEE" |
               substr(dplyr::coalesce(KD_RISIKOVERFAHRENSSCHALTER, ""), 1, 1)
             == "O")
            ~ "SO - ohne Zuordnung (KSA)",
            TRUE ~ NA_character_),
          KD_HERKUNFT = case_when(
            as.numeric(ks_1) < 5 ~ "Inland",
            !is.na(Land_kurz) ~ Land_kurz,
            TRUE ~ "Ausland")
          )

      # ii) Ermittlung der Kunden mit unvollständigem VMO
      #__________________________________________________
      t_kein_vmo <- basis_afr_kunde %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, MIN_AFG_DAT, AFR_KD_ENDE) %>%
        dplyr::left_join(basis_kunde_sire %>% dplyr::select(
          PNR_KD, PNR_KD_QT, SIRE_CODE, SIRE_QT, KTO_NR, KTO_QT,
          SIRE_PNR_KD_GUELTIG_AB, SIRE_PNR_KD_GUELTIG_BIS, ANZAHL_VMO
        ), by = c("PNR_KD", "PNR_KD_QT")) %>%
        # Kennzeichnung der Ausfallreihen-VMO-Kombinationen bei den sich die
        # Gültigkeiten überschneiden
        dplyr::mutate(
          counter_kein_vmo = if_else(dplyr::coalesce(ANZAHL_VMO, 0) == 0, 1, 0),
          FLAG_UEBERSCHNEIDUNG = case_when(
            # 1. SIRE in beide Richtungen unbegrenzt gültig
            is.na(SIRE_PNR_KD_GUELTIG_AB) & is.na(SIRE_PNR_KD_GUELTIG_BIS)
            ~ 1,
            # 2. SIRE_AB ist unbegrenzt, SIRE_BIS ist begrenzt
            is.na(SIRE_PNR_KD_GUELTIG_AB) &
              !is.na(SIRE_PNR_KD_GUELTIG_BIS) &
              SIRE_PNR_KD_GUELTIG_BIS >= MIN_AFG_DAT ~ 1,
            # 3. SIRE_AB ist begrenzt, SIRE_BIS ist unbegrenzt
            # 3a. Ausfallreihe ist nicht beendet
            !is.na(SIRE_PNR_KD_GUELTIG_AB) &
              is.na(SIRE_PNR_KD_GUELTIG_BIS) & is.na(AFR_KD_ENDE) ~ 1,
            # 3b. Ausfallreihe ist beendet
            !is.na(SIRE_PNR_KD_GUELTIG_AB) &
              is.na(SIRE_PNR_KD_GUELTIG_BIS) & !is.na(AFR_KD_ENDE) &
              SIRE_PNR_KD_GUELTIG_AB <= AFR_KD_ENDE
            ~ 1,
            # 4. SIRE_AB und SIRE_BIS ist begrenzt
            # 4a. Ausfallreihe ist nicht beendet
            !is.na(SIRE_PNR_KD_GUELTIG_AB) &
              !is.na(SIRE_PNR_KD_GUELTIG_BIS) & is.na(AFR_KD_ENDE) &
              SIRE_PNR_KD_GUELTIG_BIS >= MIN_AFG_DAT
            ~ 1,
            # 4b. Ausfallreihe ist beendet
            !is.na(SIRE_PNR_KD_GUELTIG_AB) &
              !is.na(SIRE_PNR_KD_GUELTIG_BIS) & !is.na(AFR_KD_ENDE) &
              SIRE_PNR_KD_GUELTIG_BIS >= MIN_AFG_DAT &
              SIRE_PNR_KD_GUELTIG_AB <= AFR_KD_ENDE ~ 1,
            # 5. Sonst
            TRUE ~ 0)) %>%
        # Ausschluss der AFR-VMO-Kombinationen ohne Überschneidung
        dplyr::filter(FLAG_UEBERSCHNEIDUNG == 1) %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD) %>%
        dplyr::summarise(
          sum_counter_kein_vmo = sum(counter_kein_vmo)

        ) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(basis_vmo_afr %>% dplyr::select(
          VMO_CODE, VMO_QT, PNR_KD, PNR_KD_QT, AFR_KD),
          by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::left_join(basis_vmo %>%
                           dplyr::select(VMO_CODE, VMO_QT, VMO_KLASSE),
                         by = c("VMO_CODE", "VMO_QT")) %>%
        dplyr::mutate(
          # Zähler ob NAs bei den Vermögensklassen vorliegen
          counter_vmo_klasse_na = if_else(is.na(VMO_KLASSE),
                                          1, 0)) %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD) %>%
        dplyr::summarise(
          sum_counter_kein_vmo = min(dplyr::coalesce(sum_counter_kein_vmo, 0)),
          sum_counter_vmo_klasse_na = sum(counter_vmo_klasse_na)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          KD_MIT_UNVOLLST_VMO = if_else(sum_counter_kein_vmo > 0
                                        | sum_counter_vmo_klasse_na > 0,
                                        TRUE, FALSE)
        )

      # 3.2 Ausfallreiheninformationen  ####
      #_____________________________________
      logging::loginfo("Bestimmung der Ausfallreiheninformationen")
      # Output: konto_kunde_umhang
      #
      # Folgende Spalten werden ermittelt:
      # - AFR_KD_MAX_KTO_UMHANG
      #
      # Output: t_abwicklung_ende
      #
      # Folgende Spalte wird ermittelt:
      # - ABWICKLUNG_ENDE
      #
      # Output: kto_umnhang_nach_c
      #
      # Folgende Spalte wird ermittelt:
      # - KD_MIT_KTO_UMHANG_NACH_C_KNZ

      # i) Bestimmung Ende der Abwicklung
      #__________________________________
      konto_kunde_umhang <- basis_kunde_konto %>%
        # Basisdaten am Konto je Kunde (Gültigkeiten)
        dplyr::select(PNR_KD, PNR_KD_QT, KTO_PNR_KD_GUELTIG_AB,
                      KTO_PNR_KD_GUELTIG_BIS,
                      KTO_EROEFFNUNG, KTO_LOESCHUNG) %>%
        # Basisdaten zur Ausfallreihe je Kunde
        dplyr::inner_join(basis_afr_kunde %>%
                            dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                          MIN_AFG_DAT, AFR_KD_ENDE,
                                          MAX_AFG_ENDE)
                          , by = c("PNR_KD", "PNR_KD_QT")) %>%
        dplyr::mutate(ENDE_DAT = dplyr::coalesce(AFR_KD_ENDE, MAX_AFG_ENDE),
                      # Default-Date falls NA um Datumssvergleich zu
                      # ermöglichen
                      gueltig_ab =
                        dplyr::coalesce(KTO_PNR_KD_GUELTIG_AB,
                                 as.Date("01.01.1111", format = "%d.%m.%Y")),
                      gueltig_bis =
                        dplyr::coalesce(KTO_PNR_KD_GUELTIG_BIS,
                                 as.Date("01.01.1111", format = "%d.%m.%Y"))
                      ) %>%
        # Filter auf potenzielle Kontumhänge in einer Ausfallreihe
        # d.h. entweder die Kontogültikait-Ab oder -bis muss vorliegen
        # und in der Ausfallreihe liegen.
        dplyr::filter(
          (MIN_AFG_DAT < gueltig_ab  & gueltig_ab <= ENDE_DAT) |
            (MIN_AFG_DAT < gueltig_bis & gueltig_bis <= ENDE_DAT)) %>%
        dplyr::mutate(
          umhang_dat = dplyr::case_when(
            # Konto wird in der Ausfallreihe weggehangen
            (MIN_AFG_DAT < gueltig_bis & gueltig_bis <= ENDE_DAT)
            & (gueltig_bis != KTO_LOESCHUNG) ~ KTO_PNR_KD_GUELTIG_BIS,
            # Konto wird in der Ausfallreihe hingehangen
            (MIN_AFG_DAT < gueltig_ab  & gueltig_ab <= ENDE_DAT) &
            (gueltig_ab != KTO_EROEFFNUNG)  ~ KTO_PNR_KD_GUELTIG_AB,
            # Default-Belegung zum späteren rausfiltern
            TRUE ~ as.Date("01.01.1111", format = "%d.%m.%Y"))) %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD, MIN_AFG_DAT,
                        ENDE_DAT) %>%
        dplyr::summarise(AFR_KD_MAX_KTO_UMHANG = max(umhang_dat)) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(PNR_KD, PNR_KD_QT, AFR_KD, ENDE_DAT,
                         AFR_KD_MAX_KTO_UMHANG =
                           if_else(AFR_KD_MAX_KTO_UMHANG ==
                                     as.Date("01.01.1111", format = "%d.%m.%Y"),
                                   NA_Date_, AFR_KD_MAX_KTO_UMHANG)) %>%
        dplyr::filter(!is.na(AFR_KD_MAX_KTO_UMHANG))

      t_abwicklung_ende <- basis_afr_kunde %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, AFR_KD_ENDE, MAX_AFG_ENDE,
                      ABWICKLUNG_BEGINN) %>%
        dplyr::left_join(konto_kunde_umhang,
                         dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                       AFR_KD_MAX_KTO_UMHANG),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::left_join(parameter_lgd_vv %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                                         AFR_KD_MAX_VMO_ZAHL_DAT,
                                         AFR_KD_MAX_BU_DAT)
                         , by = c("PNR_KD", "PNR_KD_QT",
                                  "AFR_KD")) %>%
        dplyr::mutate(
          PNR_KD, PNR_KD_QT, AFR_KD,
          ENDE_DAT = dplyr::coalesce(AFR_KD_ENDE, MAX_AFG_ENDE),
          ABWICKLUNG_ENDE_tmp = pmax(dplyr::coalesce(AFR_KD_MAX_KTO_UMHANG,
                                  as.Date("01.01.1111", format = "%d.%m.%Y")),
                         dplyr::coalesce(AFR_KD_MAX_VMO_ZAHL_DAT,
                                  as.Date("01.01.1111", format = "%d.%m.%Y")),
                         dplyr::coalesce(AFR_KD_MAX_BU_DAT,
                                  as.Date("01.01.1111", format = "%d.%m.%Y")))

        ) %>%
        dplyr::transmute(
          PNR_KD, PNR_KD_QT, AFR_KD, ABWICKLUNG_ENDE =
            dplyr::case_when(
              is.na(ABWICKLUNG_BEGINN) ~ NA_Date_,
              ABWICKLUNG_ENDE_tmp
              != as.Date("01.01.1111", format = "%d.%m.%Y") &
                ABWICKLUNG_ENDE_tmp < as.Date("01.01.2005", format = "%d.%m.%Y")
              ~ ENDE_DAT,
              ABWICKLUNG_ENDE_tmp
              != as.Date("01.01.1111", format = "%d.%m.%Y")
              ~ ABWICKLUNG_ENDE_tmp,
              TRUE ~ ENDE_DAT)
        )

      # ii) Bestimmung Kontoumhang nach C
      #__________________________________
      kto_umnhang_nach_c <- basis_afr_kunde %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD, MIN_C_AFG_DAT,
                      AFR_KD_ENDE) %>%
        dplyr::left_join(basis_afr_konto %>%
                           dplyr::select(
                             PNR_KD, PNR_KD_QT, AFR_KD, KTO_NR, KTO_QT,
                             KTO_EROEFFNUNG, KTO_LOESCHUNG,
                             KTO_PNR_KD_GUELTIG_AB, KTO_PNR_KD_GUELTIG_BIS),
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::mutate(
          FLAG_KTO_MIT_UMHANG_NACH_C = dplyr::case_when(
            # Konto unbegrenzt gültig
            is.na(KTO_PNR_KD_GUELTIG_BIS) ~ 0,
            # Konten ohne C-Ausfall
            is.na(MIN_C_AFG_DAT) ~ 0,
            # Konto wird nach C-Ausfall weggehangen
            KTO_LOESCHUNG != KTO_PNR_KD_GUELTIG_BIS &
              KTO_PNR_KD_GUELTIG_BIS > MIN_C_AFG_DAT &
              (is.na(AFR_KD_ENDE) | KTO_PNR_KD_GUELTIG_BIS <= AFR_KD_ENDE) ~ 1,
            # Konto wird nach C-Ausfall hingehangen
            KTO_EROEFFNUNG != KTO_PNR_KD_GUELTIG_AB &
              KTO_PNR_KD_GUELTIG_AB > MIN_C_AFG_DAT &
              (is.na(AFR_KD_ENDE) | KTO_PNR_KD_GUELTIG_AB <= AFR_KD_ENDE) ~ 1,
            TRUE ~ 0)) %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD, MIN_C_AFG_DAT) %>%
        dplyr::summarise(Zaehler_KTO_MIT_UMHANG_NACH_C
                         = sum(FLAG_KTO_MIT_UMHANG_NACH_C)) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(PNR_KD, PNR_KD_QT, AFR_KD,
          KD_MIT_KTO_UMHANG_NACH_C_KNZ =
            dplyr::if_else(!is.na(MIN_C_AFG_DAT) &
                      Zaehler_KTO_MIT_UMHANG_NACH_C > 0, TRUE, FALSE))

      # 3.3 Kontoinformationen auf Kundenebene  ####
      #_____________________________________________
      logging::loginfo("Bestimmung der Kontoinformationen auf Kundenebene")
      # Output: afr_konto_info
      #
      # Folgende (Zwischen-)Parameter werden ermittelt
      # - MAX_ANZAHL_BEW_MIN_AFG
      # - MAX_ANZAHL_BEW_MIN_C_AFG
      # - ANZ_KTO_MIN_AFG_SALDO_FEHLEND
      # - ANZ_KTO_MIN_C_AFG_SALDO_FEHLEND
      # - KD_MIT_KTO_FW_KNZ
      # - KD_MIT_STUBIFO

      afr_konto_info <- basis_afr_konto %>%
        dplyr::left_join(vdkx[["v_VDH_Schluessel_KTO_Klasse"]],
                         by = c("KTO_KLASSE" = "KTO_Klasse_T"))  %>%
        dplyr::mutate(
          # CCF-Relevanz, EAD-Relevanz und kein Saldo zum Ausfallzeitpunkt
          zaehler_saldo = dplyr::if_else(
            # CCF-Relevanz
            KTO_BEW_Knz == -1
            # EAD-Relevanz
            & KTO_EAD_Knz == -1
            # Kein Saldo vorhanden
            & is.na(MIN_AFG_SALDO), 1, 0),
          zaehler_c_saldo = dplyr::if_else(
            # CCF-Relevanz
            KTO_BEW_Knz == -1
            # EAD-Relevanz
            & KTO_EAD_Knz == -1
            # Kein Saldo vorhanden
            & is.na(MIN_C_AFG_SALDO), 1, 0),
          zaehler_fw = dplyr::if_else(KTO_FW_KNZ, 1, 0),
          zaehler_mit_stubifo = dplyr::if_else(
            KTO_GPV %in% c("Stubifo", "PG_STAND_8", "DKB_AZ_6_05"), 1, 0)
        ) %>%
        dplyr::group_by(PNR_KD, PNR_KD_QT, AFR_KD, KD_KTO_KLASSE_INT) %>%
        dplyr::summarise(
          MAX_ANZAHL_BEW_MIN_AFG = max(ANZAHL_BEW_MIN_AFG),
          MAX_ANZAHL_BEW_MIN_C_AFG = max(ANZAHL_BEW_MIN_C_AFG),
          ANZ_KTO_MIN_AFG_SALDO_FEHLEND = sum(zaehler_saldo),
          ANZ_KTO_MIN_C_AFG_SALDO_FEHLEND = sum(if_else(!is.na(MIN_C_AFG_DAT),
                                                        zaehler_c_saldo,
                                                        NA_real_)),
          KD_MIT_KTO_FW_KNZ = dplyr::if_else(sum(zaehler_fw) > 0, TRUE, FALSE),
          KD_MIT_STUBIFO = dplyr::if_else(
            sum(zaehler_mit_stubifo) > 0, TRUE, FALSE),

        ) %>%
        dplyr::ungroup()

      # 3.4 Erstellung LGD-Parametertabelle  ####
      #__________________________________________
      logging::loginfo("Erstellung LGD-Parametertabelle")
      # Es werden alle Datenfelder aus der LGD Vorverarbeitung und aus
      # dieser Sequenz zusammengeführt und alle verbleibenden berechnet,
      # die Vorverarbeitungsparameter als Input haben.
      #
      # Output: lgd_ergebnisse

      lgd_ergebnisse  <- basis_afr_kunde %>%
        dplyr::select(PNR_KD, PNR_KD_QT, AFR_KD,
                      AFR_KD_ENDE, MAX_AFG_ENDE,
                      MIN_AFG_DAT, MIN_C_AFG_DAT, ABWICKLUNG_BEGINN,
                      KD_EINDEUTIGER_AFR_STATUS) %>%
        # Anspielen der benötigten Informationen
        #_______________________________________
        dplyr::left_join(kunden_info %>%
                           dplyr::select(PNR_KD, PNR_KD_QT, KD_TYP_UND_BRANCHE,
                                         KD_HERKUNFT, KD_KUNDENSYSTEMATIK,
                                         KD_RISIKOVERFAHRENSSCHALTER,
                                         KD_FREIGABE, KD_VALIDE),
                         by = c("PNR_KD", "PNR_KD_QT")) %>%
        dplyr::left_join(afr_konto_info,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::left_join(parameter_lgd_vv,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::left_join(parameter_shver_afr,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::left_join(t_abwicklung_ende,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::left_join(kto_umnhang_nach_c,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD"))  %>%
        dplyr::left_join(konto_kunde_umhang,
                         by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        dplyr::left_join(t_kein_vmo,
                       by = c("PNR_KD", "PNR_KD_QT", "AFR_KD")) %>%
        # Auffüllen fehlender Werte
        #__________________________
        dplyr::mutate(
          # Logicals
          dplyr::across(c("KD_MIT_UNVOLLST_VMO", "KD_MIT_KTO_FW_KNZ",
                          "KD_MIT_STUBIFO", "KD_MIT_KTO_UMHANG_NACH_C_KNZ"),
            ~tidyr::replace_na(.x, FALSE)),
          # numerics or integer
          dplyr::across(c("KD_KTO_KLASSE_INT", "MAX_ANZAHL_BEW_MIN_AFG",
                          "MAX_ANZAHL_BEW_MIN_C_AFG",
                          "ANZ_KTO_MIN_AFG_SALDO_FEHLEND",
                          "ANZ_KTO_MIN_C_AFG_SALDO_FEHLEND",
                          "KD_NETTO_VE_PROG"),
            ~tidyr::replace_na(.x, as.integer(0)))

        ) %>%
        # Berechnung neuer Spalten
        #_________________________
        dplyr::mutate(
          ENDE_DAT = dplyr::coalesce(AFR_KD_ENDE, MAX_AFG_ENDE),
          KD_MIN_AFG_EAD = pmax(KD_MIN_AFG_SALDO + KD_AVAL_IA_SAN +
                                  KD_AVAL_IA_NACH_C, 0),
          KD_LOSS_REAL_LOGIK = dplyr::case_when(
            AFR_KD_STATUS != "RM" & AFR_KD_STATUS != "FA" ~ 1,
            AFR_KD_STATUS == "RM" ~ 2,
            KD_MIT_KTO_UMHANG_NACH_C_KNZ ~ 3,
            TRUE ~ 4),
          KD_TAD = dplyr::if_else(
            is.na(ABWICKLUNG_BEGINN) | AFR_KD_STATUS != "FA", NA_real_,
            as.numeric((ABWICKLUNG_ENDE - ABWICKLUNG_BEGINN) / (365.25 / 12))),
          KD_BEAD_PROG = KD_MIN_AFG_EAD - KD_NETTO_VE_PROG,
          KD_LOSS_REAL = dplyr::case_when(
            KD_LOSS_REAL_LOGIK == 1 ~ NA_real_,
            KD_LOSS_REAL_LOGIK == 2 ~ KD_MIN_AFG_EAD -
              (KD_EZ_SAN_DISK  + KD_RM_SALDO_DISK),
            KD_LOSS_REAL_LOGIK == 3 ~ KD_FVZ_SAN + KD_FVZ_NACH_C,
            KD_LOSS_REAL_LOGIK == 4 ~ KD_MIN_AFG_EAD
              # EZ SAN
              - (KD_EZ_SAN_DISK  + (KD_VE_SAN_DISK - KD_VK_SAN_DISK))
              # VZ nach C
              - (KD_VE_NACH_C_DISK - KD_VK_NACH_C_DISK)
              # EZ nach C
              - (KD_EE_NACH_C_DISK - KD_EK_NACH_C_DISK)
              # VZ nicht-AIRBA-VMO
              - (KD_VE_2_NACH_C_DISK - KD_VK_2_NACH_C_DISK)
            ),
          KD_LOSS_REAL_MIT_FMP_VD = KD_LOSS_REAL - KD_FMP_VD_BETRAG_DISK,
          KD_TDAA = dplyr::case_when(
            # die KD_AFR hat keine Abwicklungsphase und ist beendet
            is.na(ABWICKLUNG_BEGINN) & !is.na(AFR_KD_ENDE)
            ~ as.numeric((AFR_KD_ENDE - MIN_AFG_DAT) / (365.25 / 12)),
            # KD_AFR hat eine Abwicklungsphase und ist beendet
            !is.na(ABWICKLUNG_BEGINN) & !is.na(AFR_KD_ENDE)
            ~ as.numeric((ABWICKLUNG_BEGINN - MIN_AFG_DAT) / (365.25 / 12)),
            TRUE ~ NA_real_),
          KD_NRQ = dplyr::if_else(
            abs(KD_BEAD_PROG) > 0.1 & KD_LOSS_REAL_LOGIK == 4,
            (KD_EZ_SAN + KD_EE_NACH_C - KD_EK_NACH_C +
               KD_VE_2_NACH_C - KD_VK_2_NACH_C) / KD_BEAD_PROG, NA_real_),
          KD_TNR = dplyr::if_else(
            abs(KD_EZ_SAN + KD_EE_NACH_C - KD_EK_NACH_C + KD_VE_2_NACH_C -
                  KD_VK_2_NACH_C) > 0.1 & KD_LOSS_REAL_LOGIK == 4,
            (KD_EZ_SAN_GEW + KD_EE_NACH_C_GEW - KD_EK_NACH_C_GEW +
               KD_VE_2_NACH_C_GEW - KD_VK_2_NACH_C_GEW) /
              (KD_EZ_SAN + KD_EE_NACH_C - KD_EK_NACH_C + KD_VE_2_NACH_C -
                 KD_VK_2_NACH_C) * 12, NA_real_),
          KD_RZQ_RM = dplyr::if_else(
            KD_LOSS_REAL_LOGIK == 2 & KD_MIN_AFG_EAD != 0,
            (KD_EZ_SAN + KD_RM_SALDO) / KD_MIN_AFG_EAD, NA_real_),
          KD_TRZ_RM = dplyr::if_else(
            AFR_KD_STATUS == "RM" & (KD_EZ_SAN + KD_RM_SALDO) > 0.1,
            ((as.numeric((ENDE_DAT - MIN_AFG_DAT) / 2)) * KD_EZ_SAN
             + as.numeric((ENDE_DAT - MIN_AFG_DAT)) * KD_RM_SALDO)
            / (KD_EZ_SAN + KD_RM_SALDO) / (365.25 / 12), NA_real_),
          KD_BESICHERUNGS_GRAD_PROG = dplyr::if_else(
            KD_MIN_AFG_EAD > 0.1, KD_NETTO_VE_PROG / KD_MIN_AFG_EAD, NA_real_),
          KD_BEAD_REAL = dplyr::if_else(
            !is.na(MIN_C_AFG_DAT) & AFR_KD_STATUS == "FA",
            pmax(KD_MIN_AFG_EAD - (KD_VE_SAN_DISK - KD_VK_SAN_DISK +
                                     KD_VE_NACH_C_DISK - KD_VK_NACH_C_DISK),
                 0),
            NA_real_),
          KD_LOSS_NACH_C_ZAHLUNGEN = dplyr::if_else(
            KD_LOSS_REAL_LOGIK == 4,
            KD_MIN_C_AFG_SALDO + KD_AVAL_IA_NACH_C
            - KD_EE_NACH_C + KD_EK_NACH_C
            - KD_VE_NACH_C + KD_VK_NACH_C
            - KD_VE_2_NACH_C + KD_VK_2_NACH_C,
            NA_real_),
          KD_LOSS_NACH_C_ABSCHR_ZINS = dplyr::if_else(
            KD_LOSS_REAL_LOGIK == 4,
            KD_FVZ_NACH_C - KD_ZINS_NACH_C,
            NA_real_),
          KD_LOSS_ABSCHR_ZINS = dplyr::if_else(
            KD_LOSS_REAL_LOGIK == 4,
            KD_FVZ_SAN + KD_FVZ_NACH_C - KD_ZINS_SAN - KD_ZINS_NACH_C,
            NA_real_),
          ZINS_ERFASSUNG_KNZ = dplyr::if_else(
            KD_LOSS_REAL_LOGIK == 4,
            dplyr::case_when(
              MIN_C_AFG_DAT > as.Date("01.10.2009", format = "%d.%m.%Y") ~ 2,
              KD_ZINS_NACH_C != 0 ~ 1,
              TRUE ~ 0),
            NA_real_),
          KD_VZ_IMMO_KNZ = dplyr::if_else(
            (abs(KD_VE_SAN_IMMO) + abs(KD_VK_SAN_IMMO) + abs(KD_VE_NACH_C_IMMO)
             + abs(KD_VK_NACH_C_IMMO)) > 0, TRUE, FALSE)
          ) %>%
        # Spalten runden
        #_______________
        dplyr::mutate(
          # Dauern und Geldbeträge auf 2 Nachkommastellen
          dplyr::across(c("KD_MIN_AFG_EAD", "KD_LOSS_REAL",
                          "KD_LOSS_REAL_MIT_FMP_VD",
                           "KD_TDAA", "KD_TNR", "KD_TAD", "KD_TRZ_RM",
                          "KD_BEAD_PROG", "KD_BESICHERUNGS_GRAD_PROG",
                          "KD_BEAD_REAL", "KD_LOSS_NACH_C_ZAHLUNGEN",
                          "KD_LOSS_NACH_C_ABSCHR_ZINS", "KD_LOSS_ABSCHR_ZINS"),
            ~round(.x, 2)),
          # Quoten auf 6 Nachkommastellen
          dplyr::across(c("KD_NRQ", "KD_RZQ_RM"),
            ~round(.x, 6))
        ) %>%
        # Spaltenreihenfolge festlegen
        #_____________________________
        dplyr::select(
          PNR_KD, PNR_KD_QT, AFR_KD, AFR_KD_STATUS, KD_KUNDENSYSTEMATIK,
          KD_RISIKOVERFAHRENSSCHALTER, KD_TYP_UND_BRANCHE, KD_HERKUNFT,
          KD_KTO_KLASSE_INT, KD_FREIGABE, KD_VALIDE, MIN_AFG_DAT, ENDE_DAT,
          MIN_C_AFG_DAT, ABWICKLUNG_BEGINN, ABWICKLUNG_ENDE, KD_MIN_AFG_EAD,
          KD_MIN_AFG_SALDO, KD_MIN_C_AFG_SALDO, KD_LOSS_REAL_LOGIK,
          KD_LOSS_REAL, KD_LOSS_REAL_MIT_FMP_VD, KD_TDAA, KD_NRQ, KD_TNR,
          KD_TAD, KD_RZQ_RM, KD_TRZ_RM, KD_BEAD_PROG, KD_NETTO_VE_PROG,
          KD_BESICHERUNGS_GRAD_PROG, KD_BEAD_REAL, KD_LOSS_NACH_C_ZAHLUNGEN,
          KD_LOSS_NACH_C_ABSCHR_ZINS, KD_LOSS_ABSCHR_ZINS, ZINS_ERFASSUNG_KNZ,
          KD_VZ_IMMO_KNZ, KD_MIT_KTO_UMHANG_NACH_C_KNZ, AFR_KD_MAX_KTO_UMHANG,
          MAX_ANZAHL_BEW_MIN_AFG, MAX_ANZAHL_BEW_MIN_C_AFG,
          ANZ_KTO_MIN_AFG_SALDO_FEHLEND, ANZ_KTO_MIN_C_AFG_SALDO_FEHLEND,
          KD_MIT_KTO_FW_KNZ, KD_MIT_UNVOLLST_VMO, KD_MIT_STUBIFO,
          KD_EINDEUTIGER_AFR_STATUS)

      # 4. Schritt: Data-Dictionary für Parametertabelle ####
      logging::loginfo("Befüllung Data-Dictionary")
      create_data_dictionary(vdkx,
                             data = lgd_ergebnisse,
                             data_description =
                               get_col_tab_description(vdkx, tablename),
                             tablename = tablename)

      # 5. Schritt: Speichern / Protokollierung Parametertabelle ####
      arrow::write_parquet(lgd_ergebnisse,
                           file.path(path_exchange,
                                     paste0(tablename, ".parquet")))
      insert_table_protocol(vdkx, tablename, nrow(lgd_ergebnisse))
    } else {
      loginfo(paste(vdkx$info_msg_create_basis_not_done, tablename))
    }
  }
  logging::loginfo(paste(vdkx$info_msg_function_end,
                         create_info_string(),
                         "create_parameter_lgd"))
}
