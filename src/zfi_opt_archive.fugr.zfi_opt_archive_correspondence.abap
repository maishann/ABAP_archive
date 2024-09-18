FUNCTION zfi_opt_archive_correspondence.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BKORM) LIKE  BKORM STRUCTURE  BKORM
*"     VALUE(I_KOART) LIKE  BKORM-KOART
*"     VALUE(I_KNA1) LIKE  KNA1 STRUCTURE  KNA1 OPTIONAL
*"     VALUE(I_KNB1) LIKE  KNB1 STRUCTURE  KNB1 OPTIONAL
*"     VALUE(I_LFA1) LIKE  LFA1 STRUCTURE  LFA1 OPTIONAL
*"     VALUE(I_LFB1) LIKE  LFB1 STRUCTURE  LFB1 OPTIONAL
*"  TABLES
*"      T_FIMSG STRUCTURE  FIMSG
*"  CHANGING
*"     VALUE(C_FINAA) LIKE  FINAA STRUCTURE  FINAA
*"     VALUE(C_ITCPO) LIKE  ITCPO STRUCTURE  ITCPO
*"     VALUE(C_ARCHIVE_INDEX) LIKE  TOA_DARA STRUCTURE  TOA_DARA
*"       DEFAULT SPACE
*"     VALUE(C_ARCHIVE_PARAMS) LIKE  ARC_PARAMS STRUCTURE  ARC_PARAMS
*"       DEFAULT SPACE
*"----------------------------------------------------------------------
  " First print?
  IF i_bkorm-erldt <> '00000000'.
    EXIT.
  ENDIF.
  CASE i_bkorm-event.
    WHEN 'SAP19' OR 'Z_19'. " HANNESM
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  DATA  BEGIN OF toa_dara OCCURS 1.
  INCLUDE STRUCTURE toa_dara.
  DATA: END OF toa_dara.

  CLEAR toa_dara.

  CASE i_koart.

      " Debitor/Kreditor

    WHEN 'D'.
      toa_dara-function   = 'DARA'.
      toa_dara-mandant    = sy-mandt.
      toa_dara-sap_object = 'BKPF'. "'KNB1'.

      " Select Document Type
      CASE i_bkorm-event.
        WHEN 'SAP19' OR 'Z_19'. " Debitorenrechnung
          toa_dara-ar_object = 'ZFI_CORR'.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

      " --- Aufbau für BKPF ist immer Buchungskreis, Belegnummer und Geschäftsjahr
      toa_dara-object_id    = i_knb1-bukrs.
      toa_dara-object_id+4  = i_bkorm-belnr. " i_knb1-kunnr. "--> Ablage bzw. Archivierung soll an Beleg und nicht an Kunde erfolgen
      toa_dara-object_id+14 = i_bkorm-gjahr.
      toa_dara-reserve(6)   = 'COMMIT'.
      toa_dara-notiz        = 'Correspondence'.

      " Set Archive_Index
      c_archive_index = toa_dara.

      " Debitor/Kreditor
    WHEN 'K'.
      toa_dara-function   = 'DARA'.
      toa_dara-mandant    = sy-mandt.
      toa_dara-sap_object = 'LFB1'.
      " Select Document Type
      CASE i_bkorm-event.
        WHEN 'SAP01'.
          toa_dara-ar_object = 'FIOPAYCONF'.
        WHEN 'SAP08'.
          toa_dara-ar_object = 'FIOACCSTAT'.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

      toa_dara-object_id    = i_lfb1-lifnr.
      toa_dara-object_id+10 = i_lfb1-bukrs.
      toa_dara-reserve(6)   = 'COMMIT'.
      toa_dara-notiz        = 'Correspondence'.

      " Set Archive_Index
      c_archive_index = toa_dara.
  ENDCASE.

  " Set Archive Mode
  c_itcpo-tdarmod = '2'. " 1 - Nur Drucken | 2 - Nur Ablegen | 3 - Drucken und Ablegen
*  c_itcpo-tdimmed = 'X'.
*  c_itcpo-tdnewid = 'X'.

  " TODO: variable is assigned but never used (ABAP cleaner)
  DATA BEGIN OF p_parameters.
  INCLUDE STRUCTURE pri_params.
  DATA END OF p_parameters.
  DATA BEGIN OF a_parameters.
  INCLUDE STRUCTURE arc_params.
  DATA END OF a_parameters.
  DATA valid TYPE c LENGTH 1.

  " Collect Print- and Archive-Parameters
  BREAK hannesm.

  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
*     ARCHIVE_ID             = C_CHAR_UNKNOWN
      archive_info           = '000'
      archive_mode           = c_itcpo-tdarmod
      archive_text           = 'Korrespondenz'
      ar_object              = toa_dara-ar_object
*     ARCHIVE_REPORT         = C_CHAR_UNKNOWN
*     AUTHORITY              = C_CHAR_UNKNOWN
*     COPIES                 = C_NUM3_UNKNOWN
*     COVER_PAGE             = C_CHAR_UNKNOWN
*     DATA_SET               = C_CHAR_UNKNOWN
*     DEPARTMENT             = C_CHAR_UNKNOWN
*     destination            = C_CHAR_UNKNOWN
*     expiration             = 0
      immediately            = c_itcpo-tdimmed
*     IN_ARCHIVE_PARAMETERS  = ' '
*     IN_PARAMETERS          = ' '
*     LAYOUT                 = C_CHAR_UNKNOWN
*     LINE_COUNT             = C_INT_UNKNOWN
*     LINE_SIZE              = C_INT_UNKNOWN
*     list_name              = ' '
*     list_text              = ' '
*     mode                   = C_CHAR_UNKNOWN
      new_list_id            = c_itcpo-tdnewid
      no_dialog              = 'X'
      receiver               = sy-uname
*     RELEASE                = C_CHAR_UNKNOWN
*     REPORT                 = C_CHAR_UNKNOWN
*     SAP_COVER_PAGE         = C_CHAR_UNKNOWN
      sap_object             = toa_dara-sap_object
*     TYPE                   = C_CHAR_UNKNOWN
    IMPORTING
      out_archive_parameters = a_parameters
      out_parameters         = p_parameters
      valid                  = valid
    EXCEPTIONS
      archive_info_not_found = 1
      invalid_print_params   = 2
      invalid_archive_params = 3
      OTHERS                 = 4.
  IF sy-subrc <> 0.
    t_fimsg-msgid = sy-msgid.
    t_fimsg-msgty = sy-msgty.
    t_fimsg-msgno = sy-msgno.
    t_fimsg-msgv1 = sy-msgv1.
    t_fimsg-msgv2 = sy-msgv2.
    t_fimsg-msgv3 = sy-msgv3.
    t_fimsg-msgv4 = sy-msgv4.
    APPEND t_fimsg.

  ENDIF.

  BREAK hannesm.

  " Set Archive_Parameters
  c_archive_params = a_parameters.

  CLEAR a_parameters.
  CLEAR p_parameters.
ENDFUNCTION.
