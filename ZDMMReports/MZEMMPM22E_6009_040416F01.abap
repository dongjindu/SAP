*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM22E_6009F01                                         *
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
FORM bdc_processing_gto_post TABLES ta_bdcmsgcoll
                               STRUCTURE bdcmsgcoll
                             USING    value(p_zdocno)
                             CHANGING value(p_subrc).
  CLEAR: ta_bdcmsgcoll, ta_bdcmsgcoll[], p_subrc.
  DATA: lv_lifex LIKE bdcdata-fval.
  lv_lifex = likp-lifex.   "Ext.delivery (Vendor batch)
  CALL FUNCTION 'Z_FMM_6009_01'
   EXPORTING
*     CTU             = 'X'
     mode            = 'N'
*     mode            = 'A'
*     UPDATE          = 'L'
*     GROUP           =
*     USER            =
*     KEEP            =
*     HOLDDATE        =
*     NODATA          = '/'
     lifex_001       = lv_lifex   " 'HMMA1234'
     im_zdocno       = p_zdocno   " App Doc No
   IMPORTING
     subrc           = p_subrc
   TABLES
     messtab         = ta_bdcmsgcoll[].

  IF p_subrc = 0.
    CALL FUNCTION 'Z_FMM_6009_02'
     EXPORTING
*     CTU             = 'X'
       mode            = 'N'
*       mode            = 'A'
*     UPDATE          = 'L'
*     GROUP           =
*     USER            =
*     KEEP            =
*     HOLDDATE        =
*     NODATA          = '/'
       lifex_001       = lv_lifex   " 'HMMA1234'
       im_zdocno       = p_zdocno   " App Doc No
     IMPORTING
       subrc           = p_subrc
     TABLES
       messtab         = ta_bdcmsgcoll[].
  ENDIF.

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
*&      Form  IDELIVERIES_WITHOUT_POST
*&---------------------------------------------------------------------*
FORM ideliveries_without_post
   TABLES ext_ideliveries_without_post
       STRUCTURE wa_ideliveries_without_post
   USING value(p_lifex) LIKE likp-lifex. "Ext.delivery (Vendor batch)

  SELECT likp~vbeln vbuk~kostk vbuk~wbstk
    INTO CORRESPONDING FIELDS OF TABLE ext_ideliveries_without_post
    FROM likp
      INNER JOIN vbuk
      ON vbuk~vbeln = likp~vbeln AND
         vbuk~kostk <> space     AND "Overall picking / putaway status
         vbuk~wbstk = 'A'         "TotalGdsMvtStat (Not Yet Processed)
    WHERE likp~verur = p_lifex.   " 'HMMA1234'.
*   Not relevant
*A  Not yet processed
*B  Partially processed
*C  Completely processed
ENDFORM.                    "IDELIVERIES_WITHOUT_POST
*&---------------------------------------------------------------------*
*&      Form  bdc_processing_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      <--P_w_subrc  text
*----------------------------------------------------------------------*
FORM bdc_processing_post
               TABLES   ext_bdcmsgcoll
                           STRUCTURE bdcmsgcoll
               USING    value(im_zdocno)
               CHANGING value(ex_subrc).
  CLEAR: ext_bdcmsgcoll, ext_bdcmsgcoll[], ex_subrc.

  DATA: lv_lifex LIKE bdcdata-fval.
  lv_lifex = likp-lifex.   "Ext.delivery (Vendor batch)
  CALL FUNCTION 'Z_FMM_6009_02'
   EXPORTING
*     CTU             = 'X'
     mode            = 'N'
*     mode            = 'A'
*     UPDATE          = 'L'
*     GROUP           =
*     USER            =
*     KEEP            =
*     HOLDDATE        =
*     NODATA          = '/'
     lifex_001       =  lv_lifex   " 'HMMA1234'
     im_zdocno       =  im_zdocno   " App. Doc. No.
   IMPORTING
     subrc           = ex_subrc
   TABLES
     messtab         = ext_bdcmsgcoll[].
ENDFORM.                    " bdc_processing
*&---------------------------------------------------------------------*
*&      Form  call_message_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_message_screen_nodeliv.
  CALL FUNCTION 'CALL_MESSAGE_SCREEN'
    EXPORTING
      i_msgid                = 'LF'   "'LF'
      i_lang                 = 'E'
      i_msgno                = '278'
*   I_MSGV1                =
*   I_MSGV2                =
*   I_MSGV3                =
*   I_MSGV4                =
*   I_SEPERATE             = ' '
     i_condense             = 'X'
     i_message_screen       = '0999'
     i_line_size            = '40'
     i_lines                = '4'
*   I_NON_LMOB_ENVT        =
* IMPORTING
*   O_ANSWER               =
* TABLES
*   T_MSG_TEXT             =
* EXCEPTIONS
*   INVALID_MESSAGE1       = 1
*   OTHERS                 = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "call_message_screen
*&---------------------------------------------------------------------*
*&      Form  qty_comparison
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_wa_lips_VGBEL  text
*      -->P_wa_lips_VGPOS  text
*      <--P_w_qty_message  text
*----------------------------------------------------------------------*
FORM qty_comparison USING    value(p_ebeln)
                             value(p_ebelp)
                    CHANGING value(p_w_qty_message) TYPE char2.
  CLEAR: p_w_qty_message.
  DATA: lv_ekpo_menge LIKE ekpo-menge.
  DATA: lv_ekes_menge LIKE ekes-menge.
  SELECT SINGLE menge INTO lv_ekpo_menge
    FROM ekpo
    WHERE ebeln = p_ebeln AND
          ebelp = p_ebelp.
  SELECT SINGLE menge INTO lv_ekes_menge
    FROM ekes
    WHERE ebeln = p_ebeln AND
          ebelp = p_ebelp AND
          etens = '0001'.
  IF lv_ekpo_menge = lv_ekes_menge.
    p_w_qty_message = 'EQ'.
  ELSE.
    p_w_qty_message = 'NE'.
  ENDIF.
ENDFORM.                    "qty_comparison
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_09  text
*      -->P_NRO_OBJECT  text
*      <--P_W_ZDOCNO  text
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
*&      Form  call_message_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0157   text
*      -->P_0158   text
*      -->P_0159   text
*      -->P_0160   text
*      -->P_0161   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM call_message_screen
              USING value(im_msgid)
                    value(im_lang)
                    value(im_msgno)
                    value(im_msgv1)
                    value(im_msgv2)
                    value(im_msgv3)
                    value(im_msgv4).

  CALL FUNCTION 'CALL_MESSAGE_SCREEN'
    EXPORTING
      i_msgid                = im_msgid  "'ZMMM'
      i_lang                 = im_lang   "'E'
      i_msgno                = im_msgno                     "'999'
      i_msgv1                = im_msgv1  "'Failure!'
      i_msgv2                = im_msgv2
      i_msgv3                = im_msgv3
      i_msgv4                = im_msgv4
*   I_SEPERATE             = ' '
     i_condense             = 'X'
     i_message_screen       = '0999'
     i_line_size            = '40'
     i_lines                = '4'
     i_non_lmob_envt        = 'X'
    "You can test this parameter with 'X' or space.
    "Space is default and this is especially for Tx /nLM00
* IMPORTING
*   O_ANSWER               =
* TABLES
*   T_MSG_TEXT             =
* EXCEPTIONS
*   INVALID_MESSAGE1       = 1
*   OTHERS                 = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " call_message_screen
