*&---------------------------------------------------------------------*
*& Report ZCA_ARCHIVELINK_TEST                                         *
*&---------------------------------------------------------------------*
*& Date:   18.09.2024                                                  *
*& Author: Hannes Maisch (HANNESM)                                     *
*& Company:                                                            *
*& Requested from:                                                     *
*& Description: Testprogramm für die Ablage eines Dokumentes im Archiv *
*&              via ARCHIVELINK.                                       *
*&---------------------------------------------------------------------*
*& Change History                                                      *
*& Date        | Author   | CR &  Description                          *
*&---------------------------------------------------------------------*
REPORT zca_archivelink_test.
DATA lv_arc_doc_id TYPE saeardoid.
DATA lv_archiv_id  TYPE saearchivi.
DATA lv_doc_type   TYPE saedoktyp.

" ----------------------------------------------------------------------
" Selektionsbild
" ----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_file   TYPE saepfad LOWER CASE,
              p_sapobj TYPE toa01-sap_object,
              p_arobj  TYPE toa01-ar_object,
              p_objid  TYPE saeobjid LOWER CASE.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN PUSHBUTTON /2(38) save_doc USER-COMMAND save_doc.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS p_docid TYPE saeardoid.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN PUSHBUTTON /2(38) show_doc USER-COMMAND show_doc.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN PUSHBUTTON /2(38) smicm  USER-COMMAND smicm.
  SELECTION-SCREEN PUSHBUTTON /2(38) sicf   USER-COMMAND sicf.
  SELECTION-SCREEN PUSHBUTTON /2(38) strust USER-COMMAND strust.
  SELECTION-SCREEN PUSHBUTTON /2(38) sa38   USER-COMMAND sa38.
  SELECTION-SCREEN PUSHBUTTON /2(38) se11   USER-COMMAND se11.
  SELECTION-SCREEN PUSHBUTTON /2(38) oac0   USER-COMMAND oac0.
  SELECTION-SCREEN PUSHBUTTON /2(38) oac2   USER-COMMAND oac2.
  SELECTION-SCREEN PUSHBUTTON /2(38) oac3   USER-COMMAND oac3.
SELECTION-SCREEN END OF BLOCK b02.


" ----------------------------------------------------------------------
" Initialisierung
" ----------------------------------------------------------------------

INITIALIZATION.
  save_doc = |{ icon_system_save }Dokument via ArchiveLink ablegen|.
  show_doc = |{ icon_display }Dokument anzeigen|.
  smicm    = |{ icon_execute_object }HTTP-Port aktivieren (SMICM)|.
  sicf     = |{ icon_execute_object }Service aktivieren (SICF)|.
  strust   = |{ icon_execute_object }Zertifikat einrichten (STRUST)|.
  sa38     = |{ icon_execute_object }Austauschverzeichnis|.
  se11     = |{ icon_execute_object }Tabelle erstellen (SE11)|.
  oac0     = |{ icon_execute_object }Content-Repository (OAC0)|.
  oac2     = |{ icon_execute_object }Dokumentart erstellen (OAC2)|.
  oac3     = |{ icon_execute_object }Verknüpfung einrichten (OAC3)|.

  " ----------------------------------------------------------------------
  " AT SELECTION-SCREEN - Lokale Datei
  " ----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CLEAR p_file.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING  mask             = '*.*'
               title            = 'Datenbankablage via ArchiveLink'
    IMPORTING  filename         = p_file
    EXCEPTIONS inv_winsys       = 1
               no_batch         = 2
               selection_cancel = 3
               selection_error  = 4
               OTHERS           = 5.

  " ----------------------------------------------------------------------
  " AT SELECTION-SCREEN - Buttons
  " ----------------------------------------------------------------------

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'SAVE_DOC'.

      " ----------------------------------------------------------------------
      " Prüfe Customizing (OAC3)
      " ----------------------------------------------------------------------
      CLEAR lv_archiv_id.
      CLEAR lv_doc_type.
      SELECT SINGLE archiv_id doc_type
        FROM toaom
        INTO ( lv_archiv_id, lv_doc_type )
        WHERE sap_object = p_sapobj
          AND ar_object  = p_arobj.

      IF lv_archiv_id IS NOT INITIAL AND lv_doc_type IS NOT INITIAL.

        " ----------------------------------------------------------------------
        " Legt die Datei über logischen oder physischen Pfad ab
        " ----------------------------------------------------------------------
        CALL FUNCTION 'ARCHIVOBJECT_CREATE_FILE'
          EXPORTING  archiv_id                = lv_archiv_id
                     document_type            = lv_doc_type
                     path                     = p_file
          IMPORTING  archiv_doc_id            = lv_arc_doc_id
          EXCEPTIONS error_archiv             = 1
                     error_communicationtable = 2
                     error_upload             = 3
                     error_kernel             = 4
                     OTHERS                   = 5.

        IF sy-subrc = 0.
          p_docid = lv_arc_doc_id.
          " ----------------------------------------------------------------------
          " Fügt einen Satz in der Verknüpfungstabelle ein
          " ----------------------------------------------------------------------

          CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
            EXPORTING  archiv_id             = lv_archiv_id
                       arc_doc_id            = lv_arc_doc_id
                       ar_object             = p_arobj
                       object_id             = p_objid
                       sap_object            = p_sapobj
                       doc_type              = lv_doc_type
            EXCEPTIONS error_connectiontable = 1
                       OTHERS                = 2.

          IF sy-subrc = 0.
            MESSAGE 'Datenbankablage erfolgreich' TYPE 'S'.
          ELSE.

            " ----------------------------------------------------------------------
            " Löscht das abgelegte Dokument, falls kein Eintrag in der Verknüpfungstabelle erzeugt werden konnte
            " ----------------------------------------------------------------------
            CALL FUNCTION 'ARCHIVOBJECT_DELETE'
              EXPORTING  archiv_id                = lv_archiv_id
                         archiv_doc_id            = lv_arc_doc_id
              EXCEPTIONS error_archiv             = 1
                         error_communicationtable = 2
                         error_kernel             = 3
                         OTHERS                   = 4.

            MESSAGE 'Fehler bei Datenbankablage' TYPE 'E'.
          ENDIF.

        ELSE.
          MESSAGE 'Fehler bei Archivierung' TYPE 'E'.
        ENDIF.
      ELSE. " Fehler im Customizing in OAC3
        MESSAGE 'Fehler im Customizing - Bitte OAC3 prüfen.' TYPE 'E'.
      ENDIF.
    WHEN 'SHOW_DOC'.
      IF p_docid IS NOT INITIAL AND lv_archiv_id IS NOT INITIAL.

        " ----------------------------------------------------------------------
        " Zeige Dokument im Document Viewer an
        " ----------------------------------------------------------------------
        CALL FUNCTION 'ARCHIVOBJECT_DISPLAY'
          EXPORTING archiv_doc_id = p_docid
                    archiv_id     = lv_archiv_id.

        IF sy-subrc <> 0.
          MESSAGE 'Dokument kann nicht angezeigt werden' TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'Dokument-Id, Dokumenttyp oder Objekttyp fehlt' TYPE 'W'.
      ENDIF.
    WHEN 'SMICM'.
      CALL TRANSACTION 'SMICM'.
    WHEN 'SICF'.
      CALL TRANSACTION 'SICF'.

    WHEN 'STRUST'.
      CALL TRANSACTION 'STRUST'.

    WHEN 'SA38'.
      SET PARAMETER ID 'RID' FIELD 'RSPARAM'.
      CALL TRANSACTION 'SA38'.

    WHEN 'SE11'.
      SET PARAMETER ID 'DTB' FIELD 'SDOKCONT1'.
      CALL TRANSACTION 'SE11'.

    WHEN 'OAC0'.
      CALL TRANSACTION 'OAC0'.

    WHEN 'OAC2'.
      CALL TRANSACTION 'OAC2'.

    WHEN 'OAC3'.
      CALL TRANSACTION 'OAC3'.

  ENDCASE.
