*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM16I_6011F01                                          *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*/ Get End Part
  PERFORM get_endpart.
*/ Simulate - Display Material BOM(/nCS03) -
* This is the data for inerface.
  PERFORM get_cs03_data.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  get_endpart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_endpart.
  CLEAR: it_matnr_werks.
  SELECT marc~matnr AS matnr
         marc~werks AS werks
    INTO CORRESPONDING FIELDS OF TABLE it_matnr_werks
    FROM marc
    WHERE marc~werks IN s_werks AND  "Plant
          marc~sobsl =  '35'.        "End Part
**/begin of for test
*    WHERE marc~werks IN s_werks.
**/end of for test

ENDFORM.                    " get_endpart
*&---------------------------------------------------------------------*
*&      Form  get_cs03_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_cs03_data.
  DATA: lv_logno_h TYPE num10.  "Log No Header.
  DATA: lt_stpox LIKE TABLE OF stpox.
  "BOM Items (Extended for List Displays)
  FIELD-SYMBOLS: <fs_stpox> LIKE LINE OF lt_stpox.

* Get Log No Header.
  PERFORM number_get_next USING    c_nro_nr_00
                                   w_nro_object
                          CHANGING lv_logno_h.
  COMMIT WORK.
  CLEAR: it_ztmm_6011_01.

  LOOP AT it_matnr_werks ASSIGNING <fs_matnr_werks>.
    PERFORM cs_bom_expl_mat_v2   "similar to /nCS03 or /nCS13
             TABLES lt_stpox
             USING 'PP01'    "Application ID
                   sy-datum  "Validity date
                   '1' "EWahrHandl. ' 'kein,'1'modPosMg,'2'%ualeEMengen
                   1     "Required quantity
                   space "Multi-level explosion:'X'
                   '1'   "Memory use ('1'=on;'0'=off;' '=no reaction)
                   <fs_matnr_werks>-matnr  "Material
                   <fs_matnr_werks>-werks. "Plant

    LOOP AT lt_stpox ASSIGNING <fs_stpox>
            WHERE mtart = 'ROH'.  "Material Type, Reated to /nCS03

      MOVE-CORRESPONDING <fs_stpox> TO wa_ztmm_6011_01.
      MOVE lv_logno_h               TO wa_ztmm_6011_01-logno_h.
      MOVE <fs_matnr_werks>-matnr   TO wa_ztmm_6011_01-matnr.
      MOVE <fs_stpox>-ojtxp         TO wa_ztmm_6011_01-ktext.
      APPEND wa_ztmm_6011_01        TO it_ztmm_6011_01.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " get_cs03_data
*&---------------------------------------------------------------------*
*&      Form  cs_bom_expl_mat_v2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STPOX  text
*      -->P_0058   text
*      -->P_SY_DATUM  text
*      -->P_0060   text
*      -->P_1      text
*      -->P_SPACE  text
*      -->P_0063   text
*      -->P_<FS_MATNR_WERKS>_MATNR  text
*      -->P_<FS_MATNR_WERKS>_WERKS  text
*----------------------------------------------------------------------*
FORM cs_bom_expl_mat_v2
         TABLES ext_stpox STRUCTURE  stpox
         USING p_capid LIKE  tc04-capid  "Application ID
               p_datuv LIKE  stko-datuv  "Validity date
               p_ehndl LIKE  csdata-xfeld
               "EWahrHandl. ' 'kein,'1'modPosMg,'2'%ualeEMengen
               p_emeng LIKE  stko-bmeng  "Required quantity
               p_mehrs LIKE  csdata-xfeld  "Multi-level explosion
               "If 'X', then all lower level is exploded.
               p_mmory LIKE  csdata-xfeld
               "Memory use ('1'=on;'0'=off;' '=no reaction)
               p_mtnrv LIKE  mara-matnr   "Material
               p_werks LIKE  marc-werks.   "Plant
  CLEAR: ext_stpox, ext_stpox[].

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
   EXPORTING
*       FTREL                       = ' '
*       ALTVO                       = ' '
*       AUFSW                       = ' '
*       AUMGB                       = ' '
*       AUMNG                       = 0
*       AUSKZ                       = ' '
*       AMIND                       = ' '
*       BAGRP                       = ' '
*       BEIKZ                       = ' '
*       BESSL                       = ' '
*       BGIXO                       = ' '
*       BREMS                       = ' '
     capid                       = p_capid                  "'PP01'
*       CHLST                       = ' '
*       COSPR                       = ' '
*       CUOBJ                       = 000000000000000
*       CUOVS                       = 0
*       CUOLS                       = ' '
     datuv                       = p_datuv    " sy-datum
*       DELNL                       = ' '
*       DRLDT                       = ' '
     ehndl                       = p_ehndl    "'1'
     emeng                       = p_emeng    " 10
*       ERSKZ                       = ' '
*       ERSSL                       = ' '
*       FBSTP                       = ' '
*       KNFBA                       = ' '
*       KSBVO                       = ' '
*       MBWLS                       = ' '
*       MKTLS                       = 'X'
*       MDMPS                       = ' '
     mehrs                       = p_mehrs    "'X'
*       MKMAT                       = ' '
*       MMAPS                       = ' '
*       SALWW                       = ' '
*       SPLWW                       = ' '
     mmory                       = p_mmory    "'1'
     mtnrv                       = p_mtnrv                  "'T005'
*       NLINK                       = ' '
*       POSTP                       = ' '
*       RNDKZ                       = ' '
*       RVREL                       = ' '
*       SANFR                       = ' '
*       SANIN                       = ' '
*       SANKA                       = ' '
*       SANKO                       = ' '
*       SANVS                       = ' '
*       SCHGT                       = ' '
*       STKKZ                       = ' '
*       STLAL                       = ' '
*       STLAN                       = ' '
*       STPST                       = 0
*       SVWVO                       = 'X'
     werks                       = p_werks                  " 'P001'
*       NORVL                       = ' '
*       MDNOT                       = ' '
*       PANOT                       = ' '
*       QVERW                       = ' '
*       VERID                       = ' '
*       VRSVO                       = 'X'
*     IMPORTING
*       TOPMAT                      =
*       DSTST                       =
    TABLES
      stb                         = ext_stpox
*       MATCAT                      =
   EXCEPTIONS
     alt_not_found               = 1
     call_invalid                = 2
     material_not_found          = 3
     missing_authorization       = 4
     no_bom_found                = 5
     no_plant_data               = 6
     no_suitable_bom_found       = 7
     conversion_error            = 8
     OTHERS                      = 9.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_00  text
*      -->P_NRO_OBJECT  text
*      <--P_WA_ZTMM_6011_01_LOGNO_H  text
*----------------------------------------------------------------------*
FORM number_get_next
           USING    value(p_nro_interval) LIKE inri-nrrangenr
                    value(p_nro_object)   LIKE inri-object
           CHANGING value(p_nro_next).
  CLEAR: p_nro_next.
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = p_nro_interval
            object                  = p_nro_object
       IMPORTING
            number                  = p_nro_next
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "number_get_next
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: lv_logno_h TYPE num10.
  DATA: lv_zresult LIKE zsca_if_time_stamp_out-zresult.
  DATA: lv_message TYPE bapi_msg. "Message text (220)
  CONSTANTS : c_dest(10) VALUE 'WMMM01'.

*/ Call Outbound RFC FM
  CALL FUNCTION 'Z_FMM_6011_OUT_BOM'
    DESTINATION              c_dest
    TABLES
      ext_ztmm_6011_01      = it_ztmm_6011_01
    EXCEPTIONS
      communication_failure = 1 MESSAGE lv_message
      system_failure        = 2 MESSAGE lv_message.
  IF sy-subrc NE 0.
    lv_zresult = 'E'.  "Result of the Processing
    MESSAGE s999(zmmm) WITH lv_message.
  ELSE.
    lv_zresult = 'S'.  "Result of the Processing
    lv_message = 'Outbound RFC FM Succeeded!'(002).
    MESSAGE s999(zmmm) WITH lv_message.
  ENDIF.

*/ Modify it_ztmm_6011_01
* App. Doc. No.
  PERFORM number_get_next USING    c_nro_nr_09
                                   'ZMMNRO0002'
                          CHANGING w_zdocno.
  COMMIT WORK.

  LOOP AT it_ztmm_6011_01 ASSIGNING <fs_ztmm_6011_01>.
    PERFORM number_get_next USING    '00'
                                     'ZMMNRO0002'
                            CHANGING lv_logno_h.

    <fs_ztmm_6011_01>-zdocno  = w_zdocno.  "App. Doc. No.
    <fs_ztmm_6011_01>-logno_h = lv_logno_h."Logno Header

    <fs_ztmm_6011_01>-zuser   = sy-uname.  "User name
*    <fs_ztmm_6011_01>-zsdat   = .  "Send File Created Date
*    <fs_ztmm_6011_01>-zstim   = .  "Send file Created Time
    <fs_ztmm_6011_01>-zedat   = sy-datum.  "SAP Interface Date
    <fs_ztmm_6011_01>-zetim   = sy-uzeit.  "SAP Interface Time
    <fs_ztmm_6011_01>-zmode   = 'C'.       "Data Characteristic Flag
    <fs_ztmm_6011_01>-zresult = lv_zresult."Result of the Processing
    <fs_ztmm_6011_01>-zmsg    = lv_message."Message text
*    <fs_ztmm_6011_01>-zzret   = .  "Inerface Return Value
  ENDLOOP.

*/ Logging to it_ztmm_6011_01.
  INSERT ztmm_6011_01 FROM TABLE it_ztmm_6011_01.

ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  z_fca_eai_interface_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_fca_eai_interface_log.
*/ Function Module for Interface Log
*
*Where to be inserted:
* 1. Inbound: When interface table is updated after Standard BDC/BAPI
*             executed.
* 2. Outbound: After calling EAI
*
*====================================================================
*
*Function name : Z_FCA_EAI_INTERFACE_LOG
*
*Import/Export Parameter Structure : ZTCA_IF_LOG
*
*IFDOC   <= Serial No. for Log. Leave as empty
*TCODE   <= Present Transaction Code
*TOTAL   <= Total Execution number
*ZSUCC   <= Successful occurrences(number) for BDC/BAPI Processing
*ERROR   <= Failed occurrences(number) for BDC/BAPI Processing
*ERDAT   <= Created on.
*ERZET   <= Created time.
*ERNAM   <= Creator.
*AEDAT   <= Changed on.
*AEZET   <= Changed time
*AENAM   <= the person who change

  DATA: lv_total TYPE i.
  DESCRIBE TABLE it_ztmm_6011_01 LINES lv_total.

  CHECK NOT lv_total IS INITIAL.
  CLEAR: wa_ztca_if_log.
  LOOP AT it_ztmm_6011_01 ASSIGNING <fs_ztmm_6011_01>.
    IF <fs_ztmm_6011_01>-zzret = 'S'.
      wa_ztca_if_log-zsucc = wa_ztca_if_log-zsucc + 1.
    ELSEIF <fs_ztmm_6011_01>-zzret = 'E'.
      wa_ztca_if_log-error = wa_ztca_if_log-error + 1.
    ENDIF.
  ENDLOOP.

  wa_ztca_if_log-tcode = 'ZMMI70'. "Present Transaction Code
  wa_ztca_if_log-total = lv_total. "Total Execution number
  wa_ztca_if_log-erdat = sy-datum. "Created on.
  wa_ztca_if_log-erzet = sy-uname. "Created time.
  wa_ztca_if_log-ernam = sy-uname. "Created by.
  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log     = wa_ztca_if_log
* IMPORTING
*   E_ZTCA_IF_LOG              =
   EXCEPTIONS
     update_failed              = 1
     number_range_error         = 2
     tcode_does_not_exist       = 3
     OTHERS                     = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " z_fca_eai_interface_log
*&---------------------------------------------------------------------*
*&      Form  display_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log.
  CALL SCREEN 0100.  " Go to Screen 0100
ENDFORM.                    " display_log
*&---------------------------------------------------------------------*
*&      Form  mask_columns
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM mask_columns TABLES   p_it_fieldcat STRUCTURE it_fieldcat.
* Build the fieldcat according to DDIC structure ZTMM_6011_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = 'ZTMM_6011_01'
       CHANGING
            ct_fieldcat      = p_it_fieldcat[].

* Make Column header
  LOOP AT p_it_fieldcat.
    IF p_it_fieldcat-fieldname = 'ZDOCNO'.
      p_it_fieldcat-coltext = 'App.DocNo.'.
    ELSEIF p_it_fieldcat-fieldname = 'LOGNO_H'.
      p_it_fieldcat-coltext = 'Log No.'.
    ELSEIF p_it_fieldcat-fieldname = 'ZSDAT'.
      p_it_fieldcat-no_out = 'X'.
    ELSEIF p_it_fieldcat-fieldname = 'ZSTIM'.
      p_it_fieldcat-no_out = 'X'.
    ELSEIF p_IT_fieldcat-fieldname = 'IDNRK'.  "BOM Component
      p_IT_fieldcat-outputlen = 18.
    ELSEIF p_IT_fieldcat-fieldname = 'MATNR'.  "Material
      p_IT_fieldcat-outputlen = 18.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_DESC'.
*      p_IT_fieldcat-checkbox = space.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_REASON'.
*      p_IT_fieldcat-coltext = 'Reason Code Name'.
    ENDIF.
    MODIFY p_it_fieldcat.
  ENDLOOP.

ENDFORM.                    " mask_columns
