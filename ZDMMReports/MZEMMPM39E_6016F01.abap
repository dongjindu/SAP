*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM39E_6016F01                                         *
*----------------------------------------------------------------------*
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
*  WRITE:/ p_rearcharacters.
ENDFORM.                    "get_rearcharacters
*---------------------------------------------------------------------*
*       FORM check_REFNR                                              *
*---------------------------------------------------------------------*
FORM check_refnr USING value(im_refnr).
  DATA: ls_refnr LIKE ltak-refnr.
  SELECT SINGLE refnr INTO ls_refnr
    FROM ltak
    WHERE refnr = im_refnr.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_it_ltxx
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LTXX  text
*      -->P_W_REFNR  text
*----------------------------------------------------------------------*
FORM get_it_ltxx
        TABLES ext_ltxx
           STRUCTURE wa_ltxx
        USING  value(im_refnr) TYPE ltak-refnr.  "Group
  CLEAR: ext_ltxx, ext_ltxx[].

  IF ltak-stdat IS INITIAL OR
     ltak-stuzt IS INITIAL.

    SELECT ltak~lgnum  "Warehouse Number
           ltak~tanum  "Transfer order number
           ltap~matnr  "Material number
           makt~maktx  "Material description
           ltap~meins  "Base unit of measure
          ltap~altme  "Alternative unit of measure for stockkeeping unit
           ltap~nsolm  "Destination target quantity in stockkeeping unit
           ltap~nsola  "Destination target quantity in alternative unit
           ztmm_mast~works  "Workstation
           ztmm_mast~rh_lh  "RH/LH
           ltak~refnr  "Group
           ltak~stdat  "Start date of the transfer order
           ltak~stuzt  "Start time of the transfer order
           ltak~endat  "Transfer order end date
           ltak~enuzt  "Transfer order end time
           ltap~vltyp  "Source storage type
           ltap~vlpla  "Source storage bin
           ltap~nltyp  "Destination storage type
           ltap~nlpla  "Destination storage bin
           ltap~pvqui  "Indicator: Material pick has been confirmed
      INTO CORRESPONDING FIELDS OF TABLE ext_ltxx
      FROM ltak
      INNER JOIN ltap
        ON ltap~lgnum = ltak~lgnum AND "Warehouse Number
           ltap~tanum = ltak~tanum AND "T/O number
           ltap~pquit = space "Open TO(Indicator: confirmation complete)
      INNER JOIN makt  "For desc of matnr
        ON makt~spras = sy-langu AND  "Language
           makt~matnr = ltap~matnr    "Material
      LEFT OUTER JOIN ztmm_mast
        ON ztmm_mast~werks = 'P001' AND "Plant
           ztmm_mast~matnr = ltap~matnr "Material
      WHERE ltak~refnr = im_refnr.   "Group
  ELSE.
    SELECT ltak~lgnum  "Warehouse Number
           ltak~tanum  "Transfer order number
           ltap~matnr  "Material number
           makt~maktx  "Material description
           ltap~meins  "Base unit of measure
          ltap~altme  "Alternative unit of measure for stockkeeping unit
           ltap~nsolm  "Destination target quantity in stockkeeping unit
           ltap~nsola  "Destination target quantity in alternative unit
           ztmm_mast~works  "Workstation
           ztmm_mast~rh_lh  "RH/LH
           ltak~refnr  "Group
           ltak~stdat  "Start date of the transfer order
           ltak~stuzt  "Start time of the transfer order
           ltak~endat  "Transfer order end date
           ltak~enuzt  "Transfer order end time
           ltap~vltyp  "Source storage type
           ltap~vlpla  "Source storage bin
           ltap~nltyp  "Destination storage type
           ltap~nlpla  "Destination storage bin
           ltap~pvqui  "Indicator: Material pick has been confirmed
      INTO CORRESPONDING FIELDS OF TABLE ext_ltxx
      FROM ltak
      INNER JOIN ltap
        ON ltap~lgnum = ltak~lgnum AND "Warehouse Number
           ltap~tanum = ltak~tanum AND "T/O number
           ltap~pquit = space "Open TO(Indicator: confirmation complete)
      INNER JOIN makt  "For desc of matnr
        ON makt~spras = sy-langu AND  "Language
           makt~matnr = ltap~matnr    "Material
      LEFT OUTER JOIN ztmm_mast
        ON ztmm_mast~werks = 'P001' AND "Plant
           ztmm_mast~matnr = ltap~matnr "Material
      WHERE ltak~refnr = im_refnr   AND "Group
* Begin of For Test
           ltak~stdat = ltak-stdat AND "Start date of the transfer order
           ltak~stuzt = ltak-stuzt.    "Start time of the transfer order
* End of For Test
  ENDIF.


* For Tcode 'ZMME88', We need only Source storage type = '422'.
  IF sy-tcode = 'ZMME88'.  "Transfer Orders for each group(Pick)
    DELETE ext_ltxx WHERE vltyp <> '422'. "Source storage type
  ENDIF.

*/Begin of Added by Hakchin(20040209)
* For Tcode 'ZMME88', We don't need 'Pick Confirmed' Data.
  IF sy-tcode = 'ZMME88'.  "Transfer Orders for each group(Pick)
    DELETE ext_ltxx WHERE pvqui = 'X'.
    "Indicator: Material pick has been confirmed
  ENDIF.
*/End of Added by Hakchin(20040209)

*/ For Tcode 'ZMME89', We need Source storage type as Criteria.
*/Begin of Added by Hakchin(20040205) (Needed by Sunil)
  IF sy-tcode = 'ZMME89'.  "Transfer Orders for each group(Transfer)
    DELETE ext_ltxx WHERE vltyp <> ltap-vltyp.
* For Tcode 'ZMME89',
* We need Only 'Pick Confirmed' Data (Partial Open TO).
    DELETE ext_ltxx WHERE pvqui = space.
    "Indicator: Material pick has been confirmed
  ENDIF.
*/End of Added by Hakchin(20040205) (Needed by Sunil)

*/Begin of Added by Hakchin(20040211) (For Test Use)
* For Tcode 'ZMME88S', We don't need 'Pick Confirmed' Data.
  IF sy-tcode = 'ZMME88S'.  "Transfer Orders for each group(Pick)
    DELETE ext_ltxx WHERE pvqui = 'X'.
    "Indicator: Material pick has been confirmed
  ENDIF.
  IF sy-tcode = 'ZMME88S'.  "Transfer Orders for each group(Pick)
    DELETE ext_ltxx WHERE vltyp <> ltap-vltyp.
  ENDIF.
*/End of Added by Hakchin(20040211)

  SORT ext_ltxx BY lgnum tanum.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  write_it_ltxx
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_it_ltxx.
  CLEAR: wa_ltxx.
  LOOP AT it_ltxx INTO wa_ltxx.
    WRITE:/
*    wa_ltxx-lgnum, "Warehouse no.
*    wa_ltxx-tanum, "T/O no.
    wa_ltxx-stuzt, "Start time of the transfer order
    (18)wa_ltxx-matnr, "Material
    wa_ltxx-works, "Workstations
    wa_ltxx-nsolm  "Destination target quantity in stockkeeping unit
              UNIT wa_ltxx-meins,
    wa_ltxx-nsola  "Destination target quantity in alternative unit
              UNIT wa_ltxx-altme.
*    wa_ltxx-refnr, "Group
*    wa_ltxx-stdat, "Start date of the transfer order
*    wa_ltxx-endat, "Transfer order end date
*    wa_ltxx-enuzt. "Transfer order end time

    WRITE:/
    wa_ltxx-maktx UNDER wa_ltxx-matnr. "Material Descreiption
  ENDLOOP.
ENDFORM.                    " write_it_ltxx
*&---------------------------------------------------------------------*
*&      Form  make_col_heading
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_col_heading.
  WRITE:/
         'Time',
  10(18) 'Part No.',
  29     'W/S',
  35(16) 'Qty'          RIGHT-JUSTIFIED,
  53(16) 'No. of Boxes' RIGHT-JUSTIFIED.

  WRITE:/
  'Part Name' UNDER 'Part No.'.
ENDFORM.                    " make_col_heading
*&---------------------------------------------------------------------*
*&      Form  make_col_heading2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_col_heading2.
  WRITE:/(17)
          'Time',
          (18) 'Part No.'.
  WRITE:/
          'Part Name' UNDER 'Part No.'.
  WRITE:/
          'W/S'       UNDER 'Time',
          'RH/LH'     UNDER 'Part No.'.
  WRITE:/
          'Qty'  UNDER 'Time'  RIGHT-JUSTIFIED,
          'No. of Boxes' UNDER 'Part No.' RIGHT-JUSTIFIED.
ENDFORM.                    " make_col_heading2
*&---------------------------------------------------------------------*
*&      Form  write_it_ltxx2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_it_ltxx2.
  CLEAR: wa_ltxx.
  LOOP AT it_ltxx INTO wa_ltxx.
    WRITE:/(17)
        wa_ltxx-stuzt, "Start time of the transfer order
    (18)wa_ltxx-matnr. "Material
    WRITE:/
    wa_ltxx-maktx UNDER wa_ltxx-matnr. "Material Descreiption
    WRITE:/
    wa_ltxx-works UNDER wa_ltxx-stuzt, "Workstations
    wa_ltxx-rh_lh UNDER wa_ltxx-matnr. "RH/LH
    WRITE:/
    wa_ltxx-nsolm  "Destination target quantity in stockkeeping unit
              UNDER wa_ltxx-stuzt UNIT wa_ltxx-meins,
    wa_ltxx-nsola  "Destination target quantity in alternative unit
              UNDER wa_ltxx-matnr UNIT wa_ltxx-altme.
*    wa_ltxx-refnr, "Group
*    wa_ltxx-stdat, "Start date of the transfer order
*    wa_ltxx-endat, "Transfer order end date
*    wa_ltxx-enuzt. "Transfer order end time
    NEW-PAGE.
  ENDLOOP.
ENDFORM.                    " write_it_ltxx2
*---------------------------------------------------------------------*
*       FORM scrolling_in_steploop                                    *
*---------------------------------------------------------------------*
FORM scrolling_in_steploop USING value(im_ok_code) TYPE sy-ucomm.
  DATA: lv_limit TYPE i,
        lv_lines TYPE i.
  DESCRIBE TABLE it_ltxx LINES lv_lines.
  CASE im_ok_code.
    WHEN 'P--'.  "First Page
      w_top_line = 1.
    WHEN 'P-'.   "Prev Page
      w_top_line = w_top_line - w_loopc.
      IF w_top_line LE 0. w_top_line = 1. ENDIF.
    WHEN 'P+'.   "Next Page
      w_top_line = w_top_line + w_loopc.
      lv_limit   = lv_lines - w_loopc + 1.
      IF w_top_line > lv_limit.
        w_top_line = lv_limit.
      ENDIF.
    WHEN 'P++'.   "Last Page
      w_top_line = lv_lines - w_loopc + 1.
      IF w_top_line < 0. w_top_line = 1. ENDIF.
  ENDCASE.
*/Begin of Added by Hakchin(20050205) (Needed by Sunil)
*  PERFORM scrolling_more.
*/End of Added by Hakchin(20050205)
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  make_it_func
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_func.
  CLEAR it_func.
  wa_func = w_call_ceac_deletion_flg. APPEND wa_func TO it_func.
  " 'CALL':CALL FUNCTION KEY DELETED, 'CEAC':CEAC FUNCTION KEY DELETED,

  CASE sy-tcode.
    WHEN 'ZMME88' OR 'ZMME88S'.
      wa_func = 'CALL'. APPEND wa_func TO it_func.
  ENDCASE.

  CASE sy-dynnr.
    WHEN 0100.
    WHEN 0110.
      CASE save_ok_code.
        WHEN 'CALL'.
          wa_func = 'CEAC'. APPEND wa_func TO it_func.
        WHEN 'CEAC'.
          wa_func = 'CALL'. APPEND wa_func TO it_func.
      ENDCASE.
    WHEN 0120.   "Check Screen of material and qty
      wa_func = 'P++'. APPEND wa_func TO it_func.
      wa_func = 'P+'.  APPEND wa_func TO it_func.
      wa_func = 'P-'.  APPEND wa_func TO it_func.
      wa_func = 'P--'. APPEND wa_func TO it_func.
  ENDCASE.
ENDFORM.                    " make_it_func
*&---------------------------------------------------------------------*
*&      Form  bdc_processing_lt12
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM bdc_processing_lt12
               TABLES   ext_bdcmsgcoll
                           STRUCTURE bdcmsgcoll
               USING    value(im_zdocno)
                        value(im_confirmation)
               CHANGING value(ex_subrc).
  CLEAR: ext_bdcmsgcoll, ext_bdcmsgcoll[].
  CLEAR: ex_subrc.

  DATA: lv_tanum_001 LIKE bdcdata-fval. " T/O no.
  DATA: lv_lgnum_002 LIKE bdcdata-fval. " Warehouse no.
  DATA: lv_qentn_005 LIKE bdcdata-fval. " PICK
  DATA: lv_qtran_006 LIKE bdcdata-fval. " TRANSFER

  lv_tanum_001 = wa_ltxx-tanum. " T/O no.
  lv_lgnum_002 = wa_ltxx-lgnum. " Warehouse no.

  IF im_confirmation = 'PICK'.
    CLEAR: lv_qtran_006. lv_qentn_005 = 'X'.
  ELSEIF im_confirmation = 'TRANSFER'.
    CLEAR: lv_qentn_005. lv_qtran_006 = 'X'.
  ENDIF.


  CONDENSE:
     lv_tanum_001, " T/O no.
     lv_lgnum_002. " Warehouse no.

  CALL FUNCTION 'Z_FMM_60XX_LT12'
   EXPORTING
*   CTU             = 'X'
*   mode            = 'N'
*   UPDATE          = 'L'
*   GROUP           =
*   USER            =
*   KEEP            =
*   HOLDDATE        =
*   NODATA          = '/'
     tanum_001       = lv_tanum_001   "T/O No '302'
     lgnum_002       = lv_lgnum_002   "Warehouse No 'P01'
     dunkl_003       = 'D'      "Background
     "Foreground/background processing
     " Space:System-guided, D:Background, H:Foreground
     qentr_004       = ''  "Pick + transfer
   qentn_005       = lv_qentn_005    "Pick
   qtran_006       = lv_qtran_006    "Transfer
     IMPORTING
       subrc           = ex_subrc
     TABLES
       messtab         = ext_bdcmsgcoll[].
**** (Begin)BDC Log to the table ZTLOG
  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = im_zdocno
      im_ztcode            = sy-tcode
      im_zprogramm         = sy-cprog
*            IM_TCODE             =
*            IM_FM_NAME           =
   TABLES
     imt_bdcmsgcoll       = ext_bdcmsgcoll[]
*           IMT_BAPIRET2         =
            .
  COMMIT WORK.
**** (End)BDC Log to the table ZTLOG

*/ Logging to ZTMM_6016_01.
* Log No. Item.
  DATA: lv_logno_d TYPE num10.
  PERFORM number_get_next USING    nro_nr_01     "NRO Interval
                                   nro_object    "NRO Object
                          CHANGING lv_logno_d.   "Log. No. Item.
  COMMIT WORK.

  DATA: ls_ztmm_6016_01 LIKE ztmm_6016_01.
  MOVE:
   sy-tcode      TO ls_ztmm_6016_01-ztcode,
   im_zdocno     TO ls_ztmm_6016_01-zdocno,
   lv_logno_d    TO ls_ztmm_6016_01-logno_d,
   wa_ltxx-lgnum TO ls_ztmm_6016_01-lgnum,
   wa_ltxx-tanum TO ls_ztmm_6016_01-tanum,
   wa_ltxx-matnr TO ls_ztmm_6016_01-matnr,
   wa_ltxx-maktx TO ls_ztmm_6016_01-maktx,
   wa_ltxx-meins TO ls_ztmm_6016_01-meins,
   wa_ltxx-altme TO ls_ztmm_6016_01-altme,
   wa_ltxx-nsolm TO ls_ztmm_6016_01-nsolm,
   wa_ltxx-nsola TO ls_ztmm_6016_01-nsola,
   wa_ltxx-works TO ls_ztmm_6016_01-works,
   wa_ltxx-rh_lh TO ls_ztmm_6016_01-rh_lh,
   wa_ltxx-refnr TO ls_ztmm_6016_01-refnr,
   wa_ltxx-stdat TO ls_ztmm_6016_01-stdat,
   wa_ltxx-stuzt TO ls_ztmm_6016_01-stuzt,
   wa_ltxx-endat TO ls_ztmm_6016_01-endat,
   wa_ltxx-enuzt TO ls_ztmm_6016_01-enuzt,
   wa_ltxx-nltyp TO ls_ztmm_6016_01-nltyp,
   wa_ltxx-nlpla TO ls_ztmm_6016_01-nlpla.
* Time stamp
  CALL FUNCTION 'Z_FMM_6001_01_TIME_STAMP'
       CHANGING
            ch_erdat = ls_ztmm_6016_01-erdat
            ch_erzet = ls_ztmm_6016_01-erzet
            ch_ernam = ls_ztmm_6016_01-ernam
            ch_aedat = ls_ztmm_6016_01-aedat
            ch_aezet = ls_ztmm_6016_01-aezet
            ch_aenam = ls_ztmm_6016_01-aenam.
* Result
  IF ex_subrc = 0.
    ls_ztmm_6016_01-zzret = 'S'.
  ELSE.
    ls_ztmm_6016_01-zzret = 'E'.
  ENDIF.

  INSERT INTO ztmm_6016_01 VALUES ls_ztmm_6016_01.

ENDFORM.                    " bdc_processing_lt12
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
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_message_screen
              USING value(im_msgid)
                    value(im_lang)
                    value(im_msgno)
                    value(im_msgv1)
                    value(im_msgv2)
                    value(im_msgv3)
                    value(im_msgv4).

  DATA: lv_msgv1 LIKE sprot_u-var1.
  DATA: lv_msgv2 LIKE sprot_u-var2.
  DATA: lv_msgv3 LIKE sprot_u-var3.
  DATA: lv_msgv4 LIKE sprot_u-var4.

  MOVE: im_msgv1 TO lv_msgv1,
        im_msgv2 TO lv_msgv2,
        im_msgv3 TO lv_msgv3,
        im_msgv4 TO lv_msgv4.
  CONDENSE:
        lv_msgv1,
        lv_msgv2,
        lv_msgv3,
        lv_msgv4.

  CALL FUNCTION 'CALL_MESSAGE_SCREEN'
    EXPORTING
      i_msgid                = im_msgid  "'ZMMM'
      i_lang                 = im_lang   "'E'
      i_msgno                = im_msgno                     "'999'
      i_msgv1                = lv_msgv1  "'Failure!'
      i_msgv2                = lv_msgv2
      i_msgv3                = lv_msgv3
      i_msgv4                = lv_msgv4
*   I_SEPERATE             = ' '
     i_condense             = 'X'
     i_message_screen       = '0999'
     i_line_size            = '40'
     i_lines                = '4'
     i_non_lmob_envt        = 'X'
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
*&---------------------------------------------------------------------*
*&      Form  check_vltyp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTAP_VLTYP  text
*----------------------------------------------------------------------*
FORM check_vltyp USING value(im_vltyp).
  DATA: lv_vltyp LIKE ltap-vltyp.
  SELECT SINGLE vltyp INTO lv_vltyp
    FROM ltap
    WHERE lgnum = ltak-lgnum AND  "Warehouse number
          vltyp = im_vltyp   AND  "Source Storage unit number
          pquit = space.    "Open TO(Indicator: confirmation complete)


*  SELECT SINGLE lgtyp INTO lv_vltyp
*    FROM t301  "WM Storage Types
*    WHERE lgnum = ltak-lgnum AND  "Warehouse number
*          lgtyp = im_vltyp.       "Source Storage unit number
ENDFORM.                    " check_vltyp
*&---------------------------------------------------------------------*
*&      Form  scrolling_more
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scrolling_more.
  DATA: lv_lines TYPE i.
  DATA: lv_top_line LIKE w_top_line.
  DATA: lv_it_ltxx_idx TYPE i.
  DATA: ls_ltxx LIKE LINE OF it_ltxx.
  DESCRIBE TABLE it_ltxx LINES lv_lines.

  LOOP AT it_ltxx INTO ls_ltxx
                  FROM w_top_line
                  TO lv_lines.
    lv_it_ltxx_idx = lv_it_ltxx_idx + 1.
    IF ls_ltxx-executed_flg = space.
      lv_top_line = w_top_line + lv_it_ltxx_idx - 1.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF lv_top_line IS INITIAL.
    LOOP AT it_ltxx INTO ls_ltxx
                    FROM 1
                    TO w_top_line.
      lv_it_ltxx_idx = lv_it_ltxx_idx + 1.
      IF ls_ltxx-executed_flg = space.
        lv_top_line = w_top_line + lv_it_ltxx_idx - 1.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_top_line IS INITIAL.
      LEAVE TO SCREEN 0100.  "First Screen
    ELSE.
      w_top_line = lv_top_line.
    ENDIF.

  ELSE.
    w_top_line = lv_top_line.
  ENDIF.
ENDFORM.                    " scrolling_more
