*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM26I_6020F01                                          *
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
  CLEAR: it_ztmm_6020_01.
*/Begin of Added by Hakchin(Purpose of All Vendor Transfer)(20040224)
  IF p_all = 'X'.
    SELECT lifnr     AS lifnr
*         MAX( udate ) AS udate
*         MAX( utime ) AS utime
      INTO CORRESPONDING FIELDS OF TABLE it_ztmm_6020_01
      FROM lfa1
      WHERE loevm = space. "Central Deletion Flag for Master Record

    LOOP AT it_ztmm_6020_01 ASSIGNING <fs_ztmm_6020_01>.
      <fs_ztmm_6020_01>-udate = sy-datum.
      <fs_ztmm_6020_01>-utime = sy-uzeit.
    ENDLOOP.
*/End of Added by Hakchin(Purpose of All Vendor Transfer)(20040224)
  ELSE.
*/ Get data for Vendor Master
*  related to last changed(including creation)
    SELECT objectid     AS lifnr
           MAX( udate ) AS udate
           MAX( utime ) AS utime
      INTO CORRESPONDING FIELDS OF TABLE it_ztmm_6020_01
      FROM cdhdr
    WHERE objectclas = 'KRED' AND  "Vendor Master
          udate IN s_udate
    GROUP BY OBJECTID.
  ENDIF.

*/ Get data for Vendor Master Interface(Outbound)
  LOOP AT it_ztmm_6020_01 ASSIGNING <fs_ztmm_6020_01>.
    CLEAR: wa_ztmm_6020_01.
    SELECT SINGLE
           lfa1~lifnr    "Account number of vendor or creditor
           lfa1~name1                                       "Name 1
           lfa1~land1    "Country
           adrc~street   "Street
           lfa1~ort01    "City
           lfa1~regio    "Region (State, Province, County)
           lfa1~pstlz    "Postal Code
           lfa1~ktokk    "Vendor account group
*/Begin of Added by Hakchin(20040326)
           adrc~name_co    "Order Street
           adrc~str_suppl1 "           City
           adrc~str_suppl2 "State,PostCode
*/End of Added by Hakchin(20040326)
      INTO CORRESPONDING FIELDS OF wa_ztmm_6020_01
      FROM lfa1
        INNER JOIN adrc
        ON adrc~addrnumber = lfa1~adrnr
      WHERE lfa1~lifnr      = <fs_ztmm_6020_01>-lifnr AND
            ( lfa1~ktokk      = 'Y020' OR
              lfa1~ktokk      = 'Y030' ).
    IF sy-subrc = 0.
      <fs_ztmm_6020_01>-name1  = wa_ztmm_6020_01-name1.
      <fs_ztmm_6020_01>-land1  = wa_ztmm_6020_01-land1.
      <fs_ztmm_6020_01>-street = wa_ztmm_6020_01-street.
      <fs_ztmm_6020_01>-ort01  = wa_ztmm_6020_01-ort01.
      <fs_ztmm_6020_01>-regio  = wa_ztmm_6020_01-regio.
      <fs_ztmm_6020_01>-pstlz  = wa_ztmm_6020_01-pstlz.
      <fs_ztmm_6020_01>-ktokk  = wa_ztmm_6020_01-ktokk.
*/Begin of Added by Hakchin(20040326)
      <fs_ztmm_6020_01>-name_co    = wa_ztmm_6020_01-name_co.
      <fs_ztmm_6020_01>-str_suppl1 = wa_ztmm_6020_01-str_suppl1.
      <fs_ztmm_6020_01>-str_suppl2 = wa_ztmm_6020_01-str_suppl2.
*/End of Added by Hakchin(20040326)
      CONCATENATE <fs_ztmm_6020_01>-udate
                  'T'
                  <fs_ztmm_6020_01>-utime
                  INTO <fs_ztmm_6020_01>-date_time.
    ENDIF.
  ENDLOOP.


*/ Delete useless data.
  IF NOT it_ztmm_6020_01 IS INITIAL.
    DELETE it_ztmm_6020_01
           WHERE ktokk <> 'Y020' AND
                 ktokk <> 'Y030'.
    "Not Foreign Vendor
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_00  text
*      -->P_NRO_OBJECT  text
*      <--P_WA_ZTMM_6020_01_LOGNO_H  text
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
  CONSTANTS : c_dest(10) VALUE 'WMGM01'.

*/ Call Outbound RFC FM
  CALL FUNCTION 'Z_FMM_6020_OUT_VENDOR'
    DESTINATION              c_dest
    TABLES
      ext_ztmm_6020_01      = it_ztmm_6020_01
    EXCEPTIONS
      communication_failure = 1 MESSAGE lv_message
      system_failure        = 2 MESSAGE lv_message.

  IF sy-subrc NE 0.
    lv_zresult = 'E'.  "Result of the Processing
    MESSAGE s999(zmmm) WITH lv_message.
  ELSE.
    lv_zresult = 'S'.  "Result of the Processing
    lv_message = 'Outbound RFC FM Connected!'(002).
    MESSAGE s999(zmmm) WITH lv_message.
  ENDIF.

*/ Modify it_ZTMM_6020_01
* App. Doc. No.
  PERFORM number_get_next USING    c_nro_nr_09
                                   'ZMMNRO0002'
                          CHANGING w_zdocno.
  COMMIT WORK.

  LOOP AT it_ztmm_6020_01 ASSIGNING <fs_ztmm_6020_01>.
    PERFORM number_get_next USING    '00'
                                     'ZMMNRO0002'
                            CHANGING lv_logno_h.
    <fs_ztmm_6020_01>-zdocno  = w_zdocno.  "App. Doc. No.
    <fs_ztmm_6020_01>-logno_h = lv_logno_h."Logno Header

    <fs_ztmm_6020_01>-zuser   = sy-uname.  "User name
*    <fs_ZTMM_6020_01>-zsdat   = .  "Send File Created Date
*    <fs_ZTMM_6020_01>-zstim   = .  "Send file Created Time
    <fs_ztmm_6020_01>-zedat   = sy-datum.  "SAP Interface Date
    <fs_ztmm_6020_01>-zetim   = sy-uzeit.  "SAP Interface Time
    <fs_ztmm_6020_01>-zmode   = 'C'.       "Data Characteristic Flag
    <fs_ztmm_6020_01>-zresult = lv_zresult."Result of the Processing
    <fs_ztmm_6020_01>-zmsg    = lv_message."Message text
*    <fs_ZTMM_6020_01>-zzret   = .  "Inerface Return Value
  ENDLOOP.

*/ Logging to it_ZTMM_6020_01.
  INSERT ztmm_6020_01 FROM TABLE it_ztmm_6020_01.
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
  DESCRIBE TABLE it_ztmm_6020_01 LINES lv_total.

  CHECK NOT lv_total IS INITIAL.
  CLEAR: wa_ztca_if_log.
  LOOP AT it_ztmm_6020_01 ASSIGNING <fs_ztmm_6020_01>.
    IF <fs_ztmm_6020_01>-zzret = 'S'.
      wa_ztca_if_log-zsucc = wa_ztca_if_log-zsucc + 1.
    ELSEIF <fs_ztmm_6020_01>-zzret = 'E'.
      wa_ztca_if_log-error = wa_ztca_if_log-error + 1.
    ENDIF.
  ENDLOOP.

  wa_ztca_if_log-tcode = 'ZMMI73'. "Present Transaction Code
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
* Build the fieldcat according to DDIC structure ZTMM_6020_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = 'ZTMM_6020_01'
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
*    ELSEIF p_IT_fieldcat-fieldname = 'MATNR'.
*      p_IT_fieldcat-outputlen = 18.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_DESC'.
*      p_IT_fieldcat-checkbox = space.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_REASON'.
*      p_IT_fieldcat-coltext = 'Reason Code Name'.
    ENDIF.
    MODIFY p_it_fieldcat.
  ENDLOOP.

ENDFORM.                    " mask_columns
