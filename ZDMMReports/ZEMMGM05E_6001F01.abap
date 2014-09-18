*----------------------------------------------------------------------*
*   INCLUDE ZRMMGM01R_6005F01                                          *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  make_tmp_messtab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_tmp_messtab.
  LOOP AT messtab.
    MOVE-CORRESPONDING messtab TO tmp_messtab.
    CLEAR: zbdcmsgcoll-logno_d.
    PERFORM make_logno USING    '01'                 "Interval
                                'ZMMNRO0002'         "Object
                       CHANGING tmp_messtab-logno_d. "Next
    MOVE: sy-uname TO tmp_messtab-uname,
          sy-datum TO tmp_messtab-datum,
          sy-uzeit TO tmp_messtab-uzeit.
    APPEND tmp_messtab.
  ENDLOOP.
ENDFORM.                    " make_tmp_messtab
*&---------------------------------------------------------------------*
*&      Form  get_data_from_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_from_table.
* DATA IIMPORT
*  IMPORT xekpo   FROM MEMORY ID 'XEKPO'.   "Item Data
*  IMPORT i_ekko  FROM MEMORY ID 'I_EKKO'.  "Header Data

  DATA: yesterday LIKE sy-datum.
  yesterday = sy-datum - 1.
*  yesterday = sy-datum.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE xekpo
    FROM ekpo
    WHERE
      aedat        = yesterday AND "Purchasing document item change date
      zzinforeccre = 'X' AND
      loekz        = space
    ORDER BY ebeln ebelp.

  DATA: tmp_xekpo LIKE xekpo.   "Internal table
  tmp_xekpo = xekpo.
  DELETE ADJACENT DUPLICATES FROM tmp_xekpo COMPARING ebeln.
  FIELD-SYMBOLS: <fs_xekpo> LIKE LINE OF xekpo.

  DATA: ls_i_ekko LIKE i_ekko.
  LOOP AT tmp_xekpo ASSIGNING <fs_xekpo>.
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_i_ekko
      FROM ekko
      WHERE ebeln = <fs_xekpo>-ebeln.
    APPEND ls_i_ekko TO it_i_ekko.
  ENDLOOP.
ENDFORM.                    " get_data_from_table
*&---------------------------------------------------------------------*
*&      Form  tmp_messtab_to_zbdcmsgcoll
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tmp_messtab_to_zbdcmsgcoll.
  DATA: lv_zdocno TYPE num10.     "Application Doc no.
  PERFORM number_get_next USING    nro_nr_09     "NRO Interval
                                   nro_object    "NRO Object
                          CHANGING lv_zdocno.    "NRO Next

  DATA: tmp_logno_h LIKE zbdcmsgcoll-logno_h.
  PERFORM make_logno USING    '00'          "Interval
                              'ZMMNRO0002'  "Object
                     CHANGING  tmp_logno_h. "Next
  LOOP AT tmp_messtab.
    MOVE-CORRESPONDING tmp_messtab TO zbdcmsgcoll.
    MOVE: tmp_logno_h TO zbdcmsgcoll-logno_h.

    DATA: lv_ztcode    TYPE tcode.
    DATA: lv_zprogramm TYPE programm.

    MOVE: lv_zdocno TO zbdcmsgcoll-zdocno.
    lv_ztcode    = 'ZMME94'.
    lv_zprogramm = 'ZEMMGM05E_6001'.

    INSERT zbdcmsgcoll.
  ENDLOOP.
ENDFORM.                    " tmp_messtab_to_zbdcmsgcoll
*---------------------------------------------------------------------*
*       FORM me12                                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(P_WA_XEKPO)                                             *
*  -->  VALUE(P_I_EKKO)                                               *
*---------------------------------------------------------------------*
FORM me12 USING value(p_wa_xekpo) LIKE uekpo
                value(p_i_ekko) LIKE ekko.
* Change Info Record
  PERFORM bdc_dynpro      USING 'SAPMM06I' '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.    "Enter

  PERFORM bdc_field       USING 'EINA-LIFNR'   "Vendor
                                 space.
  PERFORM bdc_field       USING 'EINA-MATNR'   "Material
                                 space.
  PERFORM bdc_field       USING 'EINE-EKORG'   "Purchasing Organization
                                 p_i_ekko-ekorg.
*Issue :MM-20040722
*Info-Record is created without data of plant (WERKS)
*-----Start
*No more consider plant
*  PERFORM bdc_field       USING 'EINE-WERKS'   "Plant
*                                 p_wa_xekpo-werks.
*-----End
  PERFORM bdc_field       USING 'EINA-INFNR'   "Info Record
                                 p_wa_xekpo-infnr.
  PERFORM bdc_field       USING 'RM06I-NORMB'   "
                                'X'.  "Info category: Standard

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=EINE'.   "Purch.org.data 1 button

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0102'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=TEXT'.   "Text Button
  PERFORM bdc_field       USING 'EINE-APLFZ' "Planned Delivery time
                                '10'.

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0103'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=KO'.   "Conditions Button
  PERFORM bdc_field       USING 'RM06I-LTEX1(02)'   "
                                p_i_ekko-ebeln.  "Related PO no.
*Issue :MM-20040722
*Info-Record is created without data of plant (WERKS)
*-----Start
*Progrma error : Changed BDC Flow
*  PERFORM bdc_dynpro      USING 'SAPLV14A' '0102'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=NEWD'.
*-----End
  PERFORM bdc_dynpro      USING 'SAPMV13A' '0201'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.

  DATA: lv_c_datab(8).    "Valid On

  PERFORM user_date_format USING    sy-uname
*                                    p_wa_xekpo-aedat
*                               "Purchasing document item change date
                                    p_i_ekko-aedat
                               "Purchasing document Creation date
                           CHANGING lv_c_datab.    "Valid On

  PERFORM bdc_field       USING 'RV13A-DATAB'
                                 lv_c_datab.     "Valid On


  DATA: lv_c_datbi(8).
  DATA: lv_datbi TYPE d.  "Valid To
* Calculate Valid_To Date
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            date      = p_i_ekko-aedat
            days      = '00'
            months    = '00'
            signum    = '+'
            years     = '02'  "Two years after
       IMPORTING
            calc_date = lv_datbi.

  PERFORM user_date_format USING    sy-uname
                                    lv_datbi
                           CHANGING lv_c_datbi.
  PERFORM bdc_field       USING 'RV13A-DATBI'
                                 lv_c_datbi.  "Valie To

* Condition for PB00
  DATA: kbetr LIKE konv-kbetr. CLEAR: kbetr.
  DATA: kbetr_char(16).        CLEAR: kbetr_char.
  DATA: kschl LIKE konv-kschl. CLEAR: kschl.
  DATA: kposn LIKE konv-kposn. CLEAR: kposn.
  DATA: knumv LIKE konv-knumv. CLEAR: knumv.

  PERFORM get_kbetr_by_kschl USING    'PB00'
                                      p_wa_xekpo-ebeln
                                      p_wa_xekpo-ebelp
                             CHANGING kbetr.
  IF kbetr IS INITIAL.
    PERFORM get_kbetr_by_kschl USING    'PBXX'
                                        p_wa_xekpo-ebeln
                                        p_wa_xekpo-ebelp
                               CHANGING kbetr.
  ENDIF.
  kbetr_char = kbetr.
  PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                 kbetr_char.


* Condition for FRA1
  kposn = p_wa_xekpo-ebelp.  "Type Conversion
  CLEAR kbetr.
  SELECT SINGLE kbetr INTO kbetr
    FROM konv
    WHERE knumv = knumv AND
          kinak = space AND   " KONV-KINAK : Condition is inactive
          kposn = kposn AND   " Correspondent to p_ebelp
          kschl = 'FRA1'.     " Condition type: Freight
  kbetr_char = kbetr.
  IF NOT kschl IS INITIAL.

    PERFORM bdc_field       USING 'KONP-KSCHL(02)'
                                   'FRA1'.
    PERFORM bdc_field       USING 'KONP-KBETR(02)'
                                   kbetr_char.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM me11                                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(P_WA_XEKPO)                                             *
*  -->  VALUE(P_I_EKKO)                                               *
*---------------------------------------------------------------------*
FORM me11 USING value(p_wa_xekpo) LIKE uekpo
                value(p_i_ekko) LIKE ekko.
* Create Info Record
  PERFORM bdc_dynpro      USING 'SAPMM06I' '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.    "Enter

  PERFORM bdc_field       USING 'EINA-LIFNR'   "Vendor
                                 space.
  PERFORM bdc_field       USING 'EINA-MATNR'   "Material
                                 space.
  PERFORM bdc_field       USING 'EINE-EKORG'   "Purchasing Organization
                                 p_i_ekko-ekorg.
*Issue :MM-20040722
*Info-Record is created without data of plant (WERKS)
*-----Start
*  PERFORM bdc_field       USING 'EINE-WERKS'   "Plant
*                                 p_wa_xekpo-werks.
*-----end
  PERFORM bdc_field       USING 'EINA-INFNR'   "Info Record
                                 p_wa_xekpo-infnr.
  PERFORM bdc_field       USING 'RM06I-NORMB'   "
                                'X'.

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=EINE'.   "Purch.org.data 1 button

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0102'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=TEXT'.   "Text Button
  PERFORM bdc_field       USING 'EINE-APLFZ' "Planned Delivery time
                                '10'.
*Issue :MM-20040722
*Error : Quantity missing
*Quantity of Info-Record is a quantity of PO item
*-----Start
  DATA: norbm_char(14).
  WRITE: p_wa_xekpo-peinh CURRENCY p_wa_xekpo-bprme TO
          norbm_char.   "Use Character when BDC

  PERFORM bdc_field       USING 'EINE-NORBM' "Standard purchase order Qu
                                 norbm_char.
*-----end
  DATA: netpr_char(14).
  WRITE: p_wa_xekpo-netpr CURRENCY p_i_ekko-waers TO
          netpr_char.   "Use Character when BDC

  PERFORM bdc_field       USING 'EINE-NETPR'   "
                                netpr_char.
  PERFORM bdc_field       USING 'EINE-WAERS'   "
                                p_i_ekko-waers.

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0103'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=KO'.   "Conditions Button
  PERFORM bdc_field       USING 'RM06I-LTEX1(02)'   "
                                p_i_ekko-ebeln.  "Related PO no.

  PERFORM bdc_dynpro      USING 'SAPMV13A' '0201'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.

  DATA: lv_c_datab(8).
  PERFORM user_date_format USING    sy-uname
*                                    p_wa_xekpo-aedat
*                               "Purchasing document item change date
                                    p_i_ekko-aedat
                               "Purchasing document Creation date
                           CHANGING lv_c_datab.
  PERFORM bdc_field       USING 'RV13A-DATAB'
                                 lv_c_datab.  "Valid On

  DATA: lv_c_datbi(8).
  DATA: lv_datbi TYPE d.  "Valid To
* Calculate Valid_To Date
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            date      = p_i_ekko-aedat
            days      = '00'
            months    = '00'
            signum    = '+'
            years     = '02'  "Two years after
       IMPORTING
            calc_date = lv_datbi.

  PERFORM user_date_format USING    sy-uname
                                    lv_datbi
                           CHANGING lv_c_datbi.
  PERFORM bdc_field       USING 'RV13A-DATBI'
                                 lv_c_datbi.  "Valie To

* Condition for PB00
  DATA: kbetr LIKE konv-kbetr. CLEAR: kbetr.
  DATA: kbetr_char(16).        CLEAR: kbetr_char.
  DATA: kschl LIKE konv-kschl. CLEAR: kschl.
  DATA: kposn LIKE konv-kposn. CLEAR: kposn.
  DATA: knumv LIKE konv-knumv. CLEAR: knumv.

  PERFORM get_kbetr_by_kschl USING    'PB00'
                                      p_wa_xekpo-ebeln
                                      p_wa_xekpo-ebelp
                             CHANGING kbetr.
  IF kbetr IS INITIAL.
    PERFORM get_kbetr_by_kschl USING    'PBXX'
                                        p_wa_xekpo-ebeln
                                        p_wa_xekpo-ebelp
                               CHANGING kbetr.
  ENDIF.
  kbetr_char = kbetr.
  PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                 kbetr_char.

* Condition for FRA1
  kposn = p_wa_xekpo-ebelp.  "Type Conversion
  CLEAR: kbetr.
  SELECT SINGLE kbetr INTO kbetr
    FROM konv
    WHERE knumv = knumv AND
          kinak = space AND   " KONV-KINAK : Condition is inactive
          kposn = kposn AND   " Correspondent to p_ebelp
          kschl = 'FRA1'.     " Condition type: Freight
  kbetr_char = kbetr.
  IF NOT kschl IS INITIAL.

    PERFORM bdc_field       USING 'KONP-KSCHL(02)'
                                   'FRA1'.
    PERFORM bdc_field       USING 'KONP-KBETR(02)'
                                   kbetr_char.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  user_date_format
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_SY_DATUM  text
*      <--P_LV_C_DATAB  text
*----------------------------------------------------------------------*
FORM user_date_format USING    value(p_user)     LIKE sy-uname
                               value(p_date)     LIKE sy-datum
                      CHANGING value(p_userdate) TYPE char8.
  CLEAR: p_userdate.
  DATA: yyyy(4).  "year
  DATA: mm(2).    "day
  DATA: dd(2).    "month
  DATA: datfm LIKE usr01-datfm.  "date format

  SELECT SINGLE datfm INTO datfm
    FROM usr01
    WHERE bname = p_user.
** datfm
*1 DD.MM.YYYY
*2 MM/DD/YYYY
*3 MM-DD-YYYY
*4 YYYY.MM.DD
*5 YYYY/MM/DD
*6 YYYY-MM-DD
  yyyy = p_date+0(4).
  mm   = p_date+4(2).
  dd   = p_date+6(2).

  CASE datfm.
    WHEN 1.
      p_userdate+0(2) = dd.
      p_userdate+2(2) = mm.
      p_userdate+4(4) = yyyy.
    WHEN 2 OR 3.
      p_userdate+0(2) = mm.
      p_userdate+2(2) = dd.
      p_userdate+4(4) = yyyy.
    WHEN 4 OR 5 OR 6.
      p_userdate+0(4) = yyyy.
      p_userdate+4(2) = mm.
      p_userdate+6(2) = dd.
  ENDCASE.


ENDFORM.                    " user_date_format
*&---------------------------------------------------------------------*
*&      Form  make_logno
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0015   text
*      -->P_0016   text
*      <--P_TMP_MESSTAB_LOGNO_D  text
*----------------------------------------------------------------------*
FORM make_logno  USING    value(p_nro_interval) LIKE inri-nrrangenr
                          value(p_nro_object)   LIKE inri-object
                 CHANGING value(p_nro_next).
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
ENDFORM.                    " make_logno
*&---------------------------------------------------------------------*
*&      Form  get_kbetr_by_kschl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0278   text
*      -->P_P_WA_XEKPO_EBELN  text
*      -->P_P_WA_XEKPO_EBELP  text
*      <--P_KBETR  text
*----------------------------------------------------------------------*
FORM get_kbetr_by_kschl
             USING    value(p_kschl) LIKE konv-kschl "Condition type
                      value(p_ebeln) LIKE ekko-ebeln "PO number
                      value(p_ebelp) LIKE ekpo-ebelp "PO Item Number
             CHANGING value(p_kbetr) LIKE konv-kbetr. "Rate
  CLEAR: p_kbetr.
  DATA: knumv LIKE ekko-knumv.
  DATA: kbetr LIKE konv-kbetr.
  DATA: kposn LIKE konv-kposn.
* konv is not a transparent table, so we don't use group by or subquery.
  SELECT SINGLE knumv INTO knumv
    FROM ekko
    WHERE ebeln = p_ebeln.

  CHECK NOT knumv IS INITIAL.
  IF NOT p_ebelp IS INITIAL. kposn = p_ebelp. ENDIF.  "Type Conversion
  SELECT SINGLE kbetr INTO p_kbetr
    FROM konv
    WHERE knumv = knumv AND
          kinak = space AND   " KONV-KINAK : Condition is inactive
          kposn = kposn AND   " Correspondent to p_ebelp
          kschl = p_kschl.    " Condition type

ENDFORM.                    " get_kbetr_by_kschl
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_09  text
*      -->P_NRO_OBJECT  text
*      <--P_LV_ZDOCNO  text
*----------------------------------------------------------------------*
FORM number_get_next
           USING    value(p_nro_interval) LIKE inri-nrrangenr
                    value(p_nro_object)   LIKE inri-object
           CHANGING value(p_nro_next).
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
*&      Form  bdc_log_to_ztlog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MESSTAB  text
*      -->P_IT_BAPIRET2  text
*      -->P_LOGNO_H  text
*      -->P_W_ZDOCNO  text
*      -->P_W_ZTCODE  text
*      -->P_W_ZPROGRAMM  text
*----------------------------------------------------------------------*
FORM bdc_log_to_ztlog
                        TABLES imt_bdcmsgcoll
                             STRUCTURE bdcmsgcoll
                        USING value(p_logno_h)
                              value(p_zdocno)
                              value(p_ztcode)
                              value(p_zprogramm).

  DATA: ls_ztlog LIKE ztlog.
  LOOP AT imt_bdcmsgcoll.
    PERFORM number_get_next USING    nro_nr_01   "NRO Interval:ItemPart
                                     nro_object  "NRO Object
                            CHANGING nro_number. "NRO Next
    ls_ztlog-logno_h       = p_logno_h.
    ls_ztlog-logno_d       = nro_number.
    ls_ztlog-zdocno        = p_zdocno.
    ls_ztlog-ztcode        = p_ztcode.
    ls_ztlog-zprogramm     = p_zprogramm.
    ls_ztlog-zuname        = sy-uname.
    ls_ztlog-zdatum        = sy-datum.
    ls_ztlog-zuzeit        = sy-uzeit.

    ls_ztlog-bb_tcode      = imt_bdcmsgcoll-tcode.
*    ls_ztlog-ba_fm_name    = imt_bdcmsgcoll-ba_fm_name.
    ls_ztlog-bb_msgtyp     = imt_bdcmsgcoll-msgtyp.
*    ls_ztlog-ba_message    = imt_bdcmsgcoll-message.
    ls_ztlog-bd_dyname     = imt_bdcmsgcoll-dyname.
    ls_ztlog-bd_dynumb     = imt_bdcmsgcoll-dynumb.
    ls_ztlog-bd_fldname    = imt_bdcmsgcoll-fldname.
    ls_ztlog-bb_msgspra    = imt_bdcmsgcoll-msgspra.
    ls_ztlog-bb_msgid      = imt_bdcmsgcoll-msgid.
    ls_ztlog-bb_msgnr      = imt_bdcmsgcoll-msgnr.
    ls_ztlog-bb_msgv1      = imt_bdcmsgcoll-msgv1.
    ls_ztlog-bb_msgv2      = imt_bdcmsgcoll-msgv2.
    ls_ztlog-bb_msgv3      = imt_bdcmsgcoll-msgv3.
    ls_ztlog-bb_msgv4      = imt_bdcmsgcoll-msgv4.
    ls_ztlog-bd_env        = imt_bdcmsgcoll-env.
*    ls_ztlog-ba_parameter  = imt_bdcmsgcoll-parameter.
*    ls_ztlog-ba_row        = imt_bdcmsgcoll-row.
*    ls_ztlog-ba_field      = imt_bdcmsgcoll-field.
*    ls_ztlog-ba_system     = imt_bdcmsgcoll-system.

    INSERT INTO ztlog VALUES ls_ztlog.
  ENDLOOP.
ENDFORM.
