*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM15E_6004F01                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  desc_bwlvs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTAK_BWLVS  text
*      <--P_T333T_LBWAT  text
*----------------------------------------------------------------------*
FORM desc_bwlvs USING    value(p_spras)   "Language key
                         value(p_lgnum)   "Warehouse Number
                         value(p_bwlvs)   "Movement type
                CHANGING value(p_lbwat).  "Movement type description
  CLEAR: p_lbwat.
  SELECT SINGLE lbwat INTO p_lbwat
    FROM t333t
    WHERE spras = p_spras AND
          lgnum = p_lgnum AND
          bwlvs = p_bwlvs.
ENDFORM.                    " desc_bwlvs
*&---------------------------------------------------------------------*
*&      Form  desc_letyp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_LANGU  text
*      -->P_LTAK_LGNUM  text
*      -->P_LEIN_LETYP  text
*      <--P_T307T_LETYP  text
*----------------------------------------------------------------------*
FORM desc_letyp USING    value(p_spras)
                         value(p_lgnum)
                         value(p_letyp)
                CHANGING value(p_desc_letyp).
  CLEAR: p_desc_letyp.
  SELECT SINGLE letyt INTO p_desc_letyp
    FROM t307t
    WHERE spras = p_spras AND
          lgnum = p_lgnum AND
          letyp = p_letyp.

ENDFORM.                    " desc_letyp
*&---------------------------------------------------------------------*
*&      Form  desc_nltyp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_LANGU  text
*      -->P_LTAK_LGNUM  text
*      -->P_LEIN_LETYP  text
*      <--P_T301T_LTYPT  text
*----------------------------------------------------------------------*
FORM desc_nltyp USING    value(p_spras)
                         value(p_lgnum)
                         value(p_nltyp)
                CHANGING value(p_ltypt).
  CLEAR: p_ltypt.
  SELECT SINGLE ltypt INTO p_ltypt
    FROM t301t
    WHERE spras = p_spras AND
          lgnum = p_lgnum AND
          lgtyp = p_nltyp.

ENDFORM.                    " desc_nltyp
*&---------------------------------------------------------------------*
*&      Form  bdc_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_processing TABLES   ta_bdcmsgcoll STRUCTURE bdcmsgcoll
                    USING    value(p_zdocno)
                    CHANGING value(p_subrc).
  CLEAR: ta_bdcmsgcoll, ta_bdcmsgcoll[], p_subrc.
  DATA: lv_lenum LIKE bdcdata-fval.
  DATA: lv_bwlvs LIKE bdcdata-fval.
  DATA: lv_letyp LIKE bdcdata-fval.
  DATA: lv_nltyp LIKE bdcdata-fval.
  DATA: lv_nlpla LIKE bdcdata-fval.
  lv_lenum = lein-lenum.
  lv_bwlvs = ltak-bwlvs.
  lv_letyp = lein-letyp.
  lv_nltyp = *ltap-nltyp.
  lv_nlpla = *ltap-nlpla.

* Get rear 10 character string
  PERFORM get_rearcharacters
                        USING    lein-lenum   "String
                                 10 "If you want to read rear 10 chars
                        CHANGING lv_lenum.   "rear 10 char string

  CALL FUNCTION 'Z_FMM_6004_01'
   EXPORTING
*   CTU             = 'X'
     mode            = 'N'
*   UPDATE          = 'L'
*   GROUP           =
*   USER            =
*   KEEP            =
*   HOLDDATE        =
*   NODATA          = '/'
     lenum_001       = lv_lenum    " '7000000063'
     bwlvs_002       = lv_bwlvs                             " '999'
     dunkl_003       = 'H'
     letyp_004       = lv_letyp    " 'BB'
     nltyp_005       = lv_nltyp                             " '100'
     nlpla_006       = lv_nlpla                             " 'AA-01'
   IMPORTING
     subrc           = p_subrc
   TABLES
     messtab         = ta_bdcmsgcoll[].




**** (Begin)BDC Log to the table ZBDCMSGCOLL
  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = p_zdocno
      im_ztcode            = sy-tcode
      im_zprogramm         = sy-cprog
*            IM_TCODE             =
*            IM_FM_NAME           =
   TABLES
     imt_bdcmsgcoll       = ta_bdcmsgcoll[]
*           IMT_BAPIRET2         =
            .
  COMMIT WORK.
**** (End)BDC Log to the table ZBDCMSGCOLL

ENDFORM.                    " bdc_processing
*&---------------------------------------------------------------------*
*&      Form  get_rearcharacters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LEIN_LENUM  text
*      -->P_10     text
*      <--P_LV_LENUM  text
*----------------------------------------------------------------------*
FORM get_rearcharacters USING    value(p_f)
                                 value(p_rearcharacters_no)
                        CHANGING value(p_rearcharacters).
* By Hakchin Kim
  DATA l_offset TYPE i.
  l_offset = strlen( p_f ) - p_rearcharacters_no.
  MOVE p_f+l_offset(p_rearcharacters_no) TO p_rearcharacters.
  WRITE:/ p_rearcharacters.
ENDFORM.                    "get_rearcharacters
*&---------------------------------------------------------------------*
*&      Form  check_lenum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LEIN_LENUM  text
*----------------------------------------------------------------------*
FORM check_lenum USING value(p_lenum).
  DATA: ls_lein LIKE lein.
  SELECT SINGLE * INTO ls_lein
    FROM lein
    WHERE lenum = p_lenum.
ENDFORM.                    " check_lenum
*&---------------------------------------------------------------------*
*&      Form  check_letyp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LEIN_LETYP  text
*----------------------------------------------------------------------*
FORM check_letyp USING    value(p_lgnum)
                          value(p_letyp).
  DATA: ls_t307 LIKE t307.
  SELECT SINGLE * INTO ls_t307
    FROM t307
    WHERE lgnum = p_lgnum    AND
          letyp = p_letyp.
ENDFORM.                    " check_letyp
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_01  text
*      -->P_NRO_OBJECT  text
*      <--P_APPLOGNO  text
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
