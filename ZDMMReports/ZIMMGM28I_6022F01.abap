*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM28I_6022F01                                          *
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
*/ Get data for Inventory Replication Interface(Outbound)
  DATA: lt_ztmm_6022_01 LIKE it_ztmm_6022_01.
  SELECT
         mara~matnr
         makt~maktx
         mara~meins
         mara~profl    "KD, MIP, LP
         mara~mtart
         SUM( mbew~lbkum ) AS lbkum   "Total valued stock
    INTO CORRESPONDING FIELDS OF TABLE lt_ztmm_6022_01
    FROM mara
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
      INNER JOIN mbew  "Material Valuation
      ON mbew~matnr = mara~matnr AND
*/Begin of Commented by Hakchin(20040205)
*         mbew~bwkey = 'P001'     AND "Valuation area
*/Begin of Commented by Hakchin(20040205)
         mbew~bwtar = space         "Valuation type
*/Begin of Commented by Hakchin(20040205)
*     WHERE mara~mtart = 'ROH' AND   "Production Materials
*           mara~profl = 'K'  OR     "KD
*           ( mara~mtart = 'FERT' OR   "Finished products
*             mara~mtart = 'HALB' ).  "Semifinished products
*/End of Commented by Hakchin(20040205)

*/Begin of Added by Hakchin(20040205)
    WHERE (
            (
*              ( mara~mtart ='ROH' OR   "Production Materials
*                mara~mtart ='ROH1'     "Raw / Sub Material
*               )                         AND
             (
               ( mara~mtart = 'ROH' AND mara~mstae = '12' ) OR
               ( mara~mtart = 'ROH' AND mara~mstae = '13' ) OR
               ( mara~mtart = 'ROH1' AND mara~mstae = '12' ) OR
               ( mara~mtart = 'ROH1' AND mara~mstae = '13' )
              ) AND
              ( mara~profl = 'K' OR
                mara~profl = 'V'
              )
            )                              OR
            ( mara~mtart = 'HALB'    AND   "Semifinished products
              ( mara~profl = 'M' OR  "MIP
                mara~profl = 'K'     "KD
              )
            )
          )                                         AND
          mara~lvorm = space
               "Flag Material for Deletion at Client Level
    GROUP BY mara~matnr  "Material
               makt~maktx
               mara~meins
               mara~profl
               mara~mtart .
*/End of Added by Hakchin(20040205)

**/Begin of Added by Hakchin(20040412)
** Exclude material that is marc-dispo = 'M02'.
*  DATA: lv_dispo LIKE marc-dispo.  "MRP Controller
*  CLEAR: it_ztmm_6022_01.
*  LOOP AT lt_ztmm_6022_01 ASSIGNING <fs_ztmm_6022_01>.
*    SELECT SINGLE dispo
*      INTO lv_dispo
*      FROM marc
*      WHERE matnr = <fs_ztmm_6022_01>-matnr AND
*            dispo = 'M02'.
*    IF sy-subrc <> 0.
*      APPEND <fs_ztmm_6022_01> TO it_ztmm_6022_01.
*    ENDIF.
*  ENDLOOP.
**/End of Added by Hakchin(20040412)
  it_ztmm_6022_01 = lt_ztmm_6022_01.  "Need by KBLEE 20040414


*/Begin of Added by Hakchin(20040326)

*Delete Color
  PERFORM delete_color
         USING it_ztmm_6022_01.

  SORT it_ztmm_6022_01 BY matnr.

*Group by matnr with qty in it_ztmm_6022_01
  PERFORM itable_group_by_fields
               USING it_ztmm_6022_01.
*/End of Added by Hakchin(20040326)

  DATA: lv_udate     LIKE ztmm_6022_01-udate.
  DATA: lv_yesterday LIKE sy-datum.
  DATA: lv_utime     LIKE ztmm_6022_01-utime.
  lv_udate     = sy-datum.
  lv_yesterday = lv_udate - 1.  "For TxnDate
  lv_utime     = sy-uzeit.

  LOOP AT it_ztmm_6022_01 ASSIGNING <fs_ztmm_6022_01>.
*/Begin of Commented by Hakchin(20040205)
*    IF <fs_ztmm_6022_01>-mtart = 'ROH' AND
*       <fs_ztmm_6022_01>-profl = 'K'.
*      <fs_ztmm_6022_01>-ptc = 'PC'.  "Purchase component
*    ENDIF.
*/End of Commented by Hakchin(20040205)

*/Begin of Added by Hakchin(20040205)
*/Product Type Code
*    IF <fs_ztmm_6022_01>-mtart = 'ROH'.
    <fs_ztmm_6022_01>-ptc = 'PC'.  "Purchase component
*    ENDIF.
*/End of Added by Hakchin(20040205)
    IF <fs_ztmm_6022_01>-mtart = 'FERT' OR
       <fs_ztmm_6022_01>-mtart = 'HALB'.
      <fs_ztmm_6022_01>-ptc = 'IM'.  "Inventoried manufactured part
    ENDIF.
    IF <fs_ztmm_6022_01>-profl = 'K' OR
       <fs_ztmm_6022_01>-profl = 'V'.
      <fs_ztmm_6022_01>-ptc = 'PC'.  "Purchase component
    ELSEIF <fs_ztmm_6022_01>-profl = 'M'.
      <fs_ztmm_6022_01>-ptc = 'IM'.  "Inventoried manufactured part
    ENDIF.

    <fs_ztmm_6022_01>-udate = lv_udate.
    <fs_ztmm_6022_01>-utime = lv_utime.

    CONCATENATE lv_yesterday
                'T'
                <fs_ztmm_6022_01>-utime
                INTO <fs_ztmm_6022_01>-date_time.
  ENDLOOP.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_00  text
*      -->P_NRO_OBJECT  text
*      <--P_WA_ZTMM_6022_01_LOGNO_H  text
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
  CALL FUNCTION 'Z_FMM_6022_OUT_INVREP'
    DESTINATION              c_dest
    TABLES
      ext_ztmm_6022_01      = it_ztmm_6022_01
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

*/ Modify it_ZTMM_6022_01
* App. Doc. No.
  PERFORM number_get_next USING    c_nro_nr_09
                                   'ZMMNRO0002'
                          CHANGING w_zdocno.
  COMMIT WORK.

  LOOP AT it_ztmm_6022_01 ASSIGNING <fs_ztmm_6022_01>.
*    PERFORM number_get_next USING    '00'
*                                     'ZMMNRO0002'
*                            CHANGING lv_logno_h.
    lv_logno_h = lv_logno_h + 1.

    <fs_ztmm_6022_01>-zdocno  = w_zdocno.  "App. Doc. No.
    <fs_ztmm_6022_01>-logno_h = lv_logno_h."Logno Header

    <fs_ztmm_6022_01>-zuser   = sy-uname.  "User name
*    <fs_ZTMM_6022_01>-zsdat   = .  "Send File Created Date
*    <fs_ZTMM_6022_01>-zstim   = .  "Send file Created Time
    <fs_ztmm_6022_01>-zedat   = sy-datum.  "SAP Interface Date
    <fs_ztmm_6022_01>-zetim   = sy-uzeit.  "SAP Interface Time
    <fs_ztmm_6022_01>-zmode   = 'C'.       "Data Characteristic Flag
    <fs_ztmm_6022_01>-zresult = lv_zresult."Result of the Processing
    <fs_ztmm_6022_01>-zmsg    = lv_message."Message text
*    <fs_ZTMM_6022_01>-zzret   = .  "Inerface Return Value
  ENDLOOP.

*/ Logging to it_ZTMM_6022_01.
  INSERT ztmm_6022_01 FROM TABLE it_ztmm_6022_01.
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
  DESCRIBE TABLE it_ztmm_6022_01 LINES lv_total.

  CHECK NOT lv_total IS INITIAL.
  CLEAR: wa_ztca_if_log.
  LOOP AT it_ztmm_6022_01 ASSIGNING <fs_ztmm_6022_01>.
    IF <fs_ztmm_6022_01>-zzret = 'S'.
      wa_ztca_if_log-zsucc = wa_ztca_if_log-zsucc + 1.
    ELSEIF <fs_ztmm_6022_01>-zzret = 'E'.
      wa_ztca_if_log-error = wa_ztca_if_log-error + 1.
    ENDIF.
  ENDLOOP.

  wa_ztca_if_log-tcode = 'ZMMI75'. "Present Transaction Code
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
* Build the fieldcat according to DDIC structure ZTMM_6022_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = 'ZTMM_6022_01'
       CHANGING
            ct_fieldcat      = p_it_fieldcat[].

* Make Column header
  LOOP AT p_it_fieldcat.
    IF p_it_fieldcat-fieldname = 'ZDOCNO'.
      p_it_fieldcat-coltext = 'App.DocNo.'.
    ELSEIF p_it_fieldcat-fieldname = 'LOGNO_H'.
      p_it_fieldcat-coltext = 'Log No.'.
    ELSEIF p_it_fieldcat-fieldname = 'PTC'.
      p_it_fieldcat-coltext = 'Product Type Code'.
    ELSEIF p_it_fieldcat-fieldname = 'DATE_TIME'.
      p_it_fieldcat-coltext = 'DATE_TIME'.
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
*&---------------------------------------------------------------------*
*&      Form  itable_group_by_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZTMM_6022_01  text
*----------------------------------------------------------------------*
FORM itable_group_by_fields
      USING iet_itable LIKE it_ztmm_6022_01.
*/For Summary WA & Itab
  FIELD-SYMBOLS: <lf_summary> LIKE LINE OF iet_itable.
  DATA: lt_summary LIKE TABLE OF <lf_summary>.
*/Field Symbol for iet_itable
  FIELD-SYMBOLS: <lf_itable> LIKE LINE OF iet_itable.

*/1.Make unique line by MATNR
  lt_summary = iet_itable.
  SORT lt_summary BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_summary
                   COMPARING matnr.

*/2.Grouping by MATNR
  SORT lt_summary BY matnr.
  LOOP AT lt_summary ASSIGNING <lf_summary>.
    CLEAR: <lf_summary>-lbkum. "<lf_summary>-serno.
    LOOP AT iet_itable ASSIGNING <lf_itable>
                  WHERE matnr = <lf_summary>-matnr.
*      <lf_summary>-serno = <lf_summary>-serno + 1.
*                         "Occurrence by MATNR
      <lf_summary>-lbkum = <lf_summary>-lbkum + <lf_itable>-lbkum.
    ENDLOOP.
  ENDLOOP.
  iet_itable = lt_summary.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ps_tb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ps_tb.
* Instanciate PF-STATUS & TITLEBAR.
  DATA: lv_numbering       TYPE i.
  DATA: lv_numbering_c(10) TYPE c.
  DESCRIBE TABLE it_ztmm_6022_01 LINES lv_numbering.
  lv_numbering_c = lv_numbering.

  IF w_title IS INITIAL.
    CONCATENATE 'Display Data Processing Log'
                'Select Entries'
                lv_numbering_c
      INTO w_title
      SEPARATED BY space.
  ENDIF.

  CREATE OBJECT crv_ps
    EXPORTING im_ps      = 'PS'                "PF-STATUS
              im_it_func = it_func             "Excluding func
              im_tb      = 'TB'                "TITLEBAR
              im_title   = w_title.            "TITLE
  CLEAR it_func.
** Dynamic Function Code Text
*  dynftext  = 'XXXX'.
ENDFORM.                    " ps_tb
*&---------------------------------------------------------------------*
*&      Form  delete_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTMM_6022_01  text
*----------------------------------------------------------------------*
FORM delete_color
     USING iet_itable LIKE it_ztmm_6022_01.
  FIELD-SYMBOLS: <lf_itable> LIKE LINE OF iet_itable.
  DATA: lv_strlen TYPE i.
  LOOP AT iet_itable ASSIGNING <lf_itable>.
    CLEAR: lv_strlen.
    lv_strlen = strlen( <lf_itable>-matnr ).
    IF ( lv_strlen = 12 OR lv_strlen = 13 ) AND
      <lf_itable>-mtart = 'ROH'                               AND
      <lf_itable>-matnr(1) <> 'B'. "Blank Mat
**Get Color
*      DATA: lv_rearcharacters_no TYPE i.
*      lv_rearcharacters_no = lv_strlen - 10.
*      PERFORM get_rearcharacters USING    <lf_itable>-matnr
*                                          lv_rearcharacters_no
*                                 CHANGING <lf_itable>-color.
*Delete Color
      <lf_itable>-matnr = <lf_itable>-matnr(10).
    ENDIF.
  ENDLOOP.
ENDFORM.                    " delete_color
