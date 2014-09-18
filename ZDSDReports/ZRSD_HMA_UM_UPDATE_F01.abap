*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_F01                                        *
*----------------------------------------------------------------------*

FORM p2000_get_data.

  DATA : lt_zidc LIKE TABLE OF ztpp_po_idoc WITH HEADER LINE,
         lt_zseg2 LIKE TABLE OF gt_zposeg2 WITH HEADER LINE ,
         pr_zseg2 LIKE TABLE OF gt_zposeg2 WITH HEADER LINE,
         lv_matnr  LIKE mara-matnr,
         lv_matnrc LIKE mara-matnr,
         lt_return LIKE TABLE OF bapireturn,
         f_wohd(1), f_wocl(1), f_wosum(1).

  DATA: lv_tabix LIKE sy-tabix.

  PERFORM get_idoc TABLES lt_zseg2 .

  SORT gt_zposeg1 BY prdod natn dist wkexc wkinc.
  SORT lt_zseg2   BY prdod natn dist wkexc wkinc.

*  pr_zseg2[] = lt_zseg2[].

  LOOP AT gt_zposeg1.
    CONCATENATE gt_zposeg1-prdod gt_zposeg1-natn gt_zposeg1-dist
           INTO lv_matnr.
    CONCATENATE lv_matnr gt_zposeg1-wkexc gt_zposeg1-wkinc
           INTO lv_matnrc.

    PERFORM check_mara  USING lv_matnr  f_wohd 'WOHD'.
    PERFORM check_mara  USING lv_matnrc f_wocl 'WOCL'.

    READ TABLE lt_zseg2 WITH KEY prdod = gt_zposeg1-prdod
                                 natn  = gt_zposeg1-natn
                                 dist  = gt_zposeg1-dist
                                 wkexc = gt_zposeg1-wkexc
                                 wkinc = gt_zposeg1-wkinc.

    CHECK sy-subrc EQ 0.

    MOVE: sy-tabix TO lv_tabix.

    PERFORM check_wosum USING lt_zseg2  f_wosum.

    LOOP AT lt_zseg2 FROM lv_tabix.
      IF lt_zseg2-prdod NE gt_zposeg1-prdod OR
         lt_zseg2-natn  NE gt_zposeg1-natn OR
         lt_zseg2-dist  NE gt_zposeg1-dist OR
         lt_zseg2-wkexc NE gt_zposeg1-wkexc OR
         lt_zseg2-wkinc NE gt_zposeg1-wkinc.
        EXIT.
      ENDIF.

      CLEAR: gt_item.

      MOVE-CORRESPONDING lt_zseg2 TO gt_item.

      IF f_wohd EQ 'X'.
        gt_item-st_wohd = '@01@'. "Success
      ELSE.
        gt_item-st_wohd = '@02@'.
      ENDIF.
      IF f_wocl EQ 'X'.
        gt_item-st_wocl = '@01@'. "Success
      ELSE.
        gt_item-st_wocl = '@02@'.
      ENDIF.

      IF f_wosum EQ 'X'.
        gt_item-st_wosum = '@01@' ."Success
      ELSE.
        gt_item-st_wosum = '@02@'.
      ENDIF.

      IF f_wohd EQ 'X' AND f_wocl EQ 'X' AND f_wosum EQ 'X'  .
        gt_item-status = '@5B@'.
        CLEAR: pr_zseg2.
        MOVE: lt_zseg2 TO pr_zseg2.
        APPEND pr_zseg2.
      ELSEIF f_wohd NE 'X' AND f_wocl NE 'X' AND f_wosum NE 'X' .
        gt_item-status = '@5C@'.
      ELSE.
        gt_item-status = '@5D@'.
      ENDIF.

      APPEND gt_item.

    ENDLOOP.
  ENDLOOP.



*  pr_zseg2[] = lt_zseg2[].
*  PERFORM p1000_start_progressbar USING text-p01 '40'.
*  LOOP AT lt_zseg2.
*
*    CONCATENATE lt_zseg2-prdod
*                lt_zseg2-natn
*                lt_zseg2-dist
*                INTO lv_matnr.
*    CONCATENATE lv_matnr
*                lt_zseg2-wkexc
*                lt_zseg2-wkinc
*                INTO lv_matnrc.
*
*    PERFORM check_mara  USING lv_matnr  f_wohd 'WOHD'.
*    PERFORM check_mara  USING lv_matnrc f_wocl 'WOCL'.
*    PERFORM check_wosum USING lt_zseg2  f_wosum.
*
*    MOVE-CORRESPONDING lt_zseg2 TO gt_item.
*
*    IF f_wohd EQ 'X'.
*      gt_item-st_wohd = '@01@'. "Success
*    ELSE.
*      gt_item-st_wohd = '@02@'.
*      DELETE pr_zseg2 INDEX sy-tabix.
*    ENDIF.
*    IF f_wocl EQ 'X'.
*      gt_item-st_wocl = '@01@'. "Success
*    ELSE.
*      gt_item-st_wocl = '@02@'.
*      DELETE pr_zseg2 INDEX sy-tabix.
*    ENDIF.
*
*    IF f_wosum EQ 'X'.
*      gt_item-st_wosum = '@01@' ."Success
*    ELSE.
*      gt_item-st_wosum = '@02@'.
*      DELETE pr_zseg2 INDEX sy-tabix.
*    ENDIF.
*
*    IF f_wohd EQ 'X'  AND
*       f_wocl EQ 'X'  AND
*       f_wosum EQ 'X'  .
*      gt_item-status = '@5B@'.
*
*    ELSEIF f_wohd NE 'X' AND
*           f_wocl NE 'X' AND
*           f_wosum NE 'X' .
*      gt_item-status = '@5C@'.
*    ELSE.
*      gt_item-status = '@5D@'.
*    ENDIF.
*    APPEND gt_item.
*  ENDLOOP.

  PERFORM p1000_start_progressbar USING text-p01 '50'.
  SORT : gt_item BY natn zvin wkinc,
         pr_zseg2 BY docnum natn zvin wkinc.

  CHECK NOT gt_item[] IS INITIAL.

  DELETE ADJACENT DUPLICATES FROM gt_item COMPARING natn zvin.
  DELETE ADJACENT DUPLICATES FROM pr_zseg2 COMPARING docnum natn zvin.

  IF p_update EQ 'X'.
    CLEAR : lt_zseg2[] , lt_zseg2.
    DATA : p_zposeg2 LIKE TABLE OF zposeg2 WITH HEADER LINE.

    SORT gt_docno BY docnum .
    LOOP AT gt_docno.
      CLEAR : p_zposeg2, p_zposeg2[].

      LOOP AT pr_zseg2 WHERE docnum = gt_docno-docnum.
        MOVE-CORRESPONDING pr_zseg2 TO p_zposeg2.
        APPEND p_zposeg2.
      ENDLOOP.

      PERFORM p1000_start_progressbar USING 'Run Update UM' '60'.

      CALL FUNCTION 'Z_FPP_HMA_UPDATE_UM'
        EXPORTING
          docnum = gt_docno-docnum
        TABLES
          return = lt_return
          item   = p_zposeg2.

    ENDLOOP.
  ENDIF.

ENDFORM.                    "p2000_get_data
*&---------------------------------------------------------------------*
*&      Form  CHECK_WOHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_MATNR  text
*      -->P_F_WOHD  text
*----------------------------------------------------------------------*
FORM check_mara USING    p_matnr
                         p_wohd
                         p_mtart.

  SELECT SINGLE * FROM mara WHERE matnr = p_matnr
                              AND mtart = p_mtart.
  IF sy-subrc EQ 0 .
    p_wohd = 'X'.
  ELSE .
    p_wohd = 'E'.
  ENDIF.


ENDFORM.                    " CHECK_WOHD

*&      Form  CHECK_WOSUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZIDC  text
*----------------------------------------------------------------------*
FORM check_wosum USING    p_idoc STRUCTURE zposeg2
                          p_wosum.
  DATA : lw_wosum LIKE ztpp_wosum.

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_wosum
   FROM ztpp_wosum
  WHERE wo_ser = p_idoc-prdod
    AND nation = p_idoc-natn
    AND dealer = p_idoc-dist
    AND extc   = p_idoc-wkexc
    AND intc   = p_idoc-wkinc.
  IF sy-subrc EQ 0 .
    IF gt_zposeg1-moqty EQ lw_wosum-modqty .
      p_wosum = 'X'.
    ELSE.
      p_wosum = 'E'.
    ENDIF.
  ELSE.
    p_wosum = 'E'.
  ENDIF.


ENDFORM.                    " CHECK_WOSUM
*&---------------------------------------------------------------------*
*&      Form  GET_IDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_idoc TABLES lt_zseg2 STRUCTURE gt_zposeg2.

  DATA : lt_idoc LIKE TABLE OF ztpp_po_idoc WITH HEADER LINE,
          lt_edidc LIKE TABLE OF edidc WITH HEADER LINE,
          lt_edidd LIKE TABLE OF edidd WITH HEADER LINE,
          lt_zseg1 LIKE TABLE OF gt_zposeg1 WITH HEADER LINE.
  DATA : lv_model LIKE ztpp_wosum-extc.

  RANGES : r_status FOR edidc-status,
           s_docnum FOR edidc-docnum.

  r_status-sign = 'E'.
  r_status-option = 'EQ'.
  r_status-low = '30'. APPEND r_status.
  r_status-low = '02'. APPEND r_status.
  r_status-low = '04'. APPEND r_status.
  r_status-low = '05'. APPEND r_status.
  r_status-low = '25'. APPEND r_status.
  r_status-low = '29'. APPEND r_status.
  r_status-low = '26'. APPEND r_status.
  r_status-low = '32'. APPEND r_status.
*  R_STATUS-LOW = '51'. APPEND R_STATUS.
  r_status-low = '56'. APPEND r_status.
  r_status-low = '61'. APPEND r_status.
  r_status-low = '63'. APPEND r_status.
  r_status-low = '65'. APPEND r_status.
  r_status-low = '60'. APPEND r_status.
  r_status-low = '64'. APPEND r_status.
  r_status-low = '66'. APPEND r_status.
  r_status-low = '69'. APPEND r_status.

* # Interface
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_idoc
    FROM ztpp_po_idoc
    WHERE crdat  IN s_credat
*    AND wo_ser   = 'D11051044'   "temp
    AND zvin_stt EQ ''
    AND status   EQ 'S'
** On 03/18/13 for performance tuning
*%_HINTS ORACLE 'index("ZTPP_PO_IDOC","ZTPP_PO_IDOC~N1")'.
   %_HINTS ORACLE 'index("ZTPP_PO_IDOC","ZTPP_PO_IDOC~Z01")'.
** End on 03/18/13

  SORT lt_idoc BY docnum wo_ser natn dist wkexc wkinc.

  PERFORM p1000_start_progressbar USING text-p01 '20'.

  IF NOT lt_idoc[] IS INITIAL.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_edidc
    FROM edidc
    FOR ALL ENTRIES IN lt_idoc
    WHERE docnum EQ lt_idoc-docnum
      AND upddat IN s_credat
      AND mestyp EQ p_mestyp
      AND direct EQ '2'
      AND status IN r_status.

    CHECK  NOT lt_edidc[] IS INITIAL.
    PERFORM p1000_start_progressbar USING text-p01 '30'.
    SORT lt_edidc BY docnum.
    LOOP AT  lt_edidc .
      gt_docno-docnum = lt_edidc-docnum.
      AT NEW docnum.
        APPEND gt_docno.
      ENDAT.

      CALL FUNCTION 'IDOC_READ_COMPLETELY'
        EXPORTING
          document_number         = lt_edidc-docnum
        TABLES
          int_edidd               = lt_edidd
        EXCEPTIONS
          document_not_exist      = 1
          document_number_invalid = 2
          OTHERS                  = 3.
      IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        LOOP AT lt_edidd WHERE segnam = 'ZPOSEG2'.
          lt_zseg2 = lt_edidd-sdata.

          IF lt_zseg2-natn  <> 'B28'.   "Victor:Color conversion 3-> 2
            lv_model  =  lt_zseg2-mdinx+0(2).

            CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
              EXPORTING
                i_model = lv_model
                i_year  = lt_zseg2-mdyr
                i_gubn  = 'X'            "HAC/HMM -> HMMA
                i_extc  = lt_zseg2-wkexc
                i_intc  = lt_zseg2-wkinc
              IMPORTING
                e_extc  = lt_zseg2-wkexc
                e_intc  = lt_zseg2-wkinc.
          ENDIF.

          READ TABLE lt_idoc WITH KEY docnum = lt_edidd-docnum
                                      wo_ser = lt_zseg2-prdod
                                      natn   = lt_zseg2-natn
                                      dist   = lt_zseg2-dist
                                      wkexc  = lt_zseg2-wkexc
                                      wkinc  = lt_zseg2-wkinc
                             BINARY SEARCH.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.

          lt_zseg2-docnum = lt_edidd-docnum.
          APPEND lt_zseg2.
        ENDLOOP.

        LOOP AT lt_edidd WHERE segnam = 'ZPOSEG1'.
          lt_zseg1 = lt_edidd-sdata.

          IF lt_zseg1-natn  <> 'B28'.   "Victor:Color conversion 3-> 2
            lv_model  =  lt_zseg1-mdinx+0(2).

            CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
              EXPORTING
                i_model = lv_model
                i_year  = lt_zseg1-mdyr
                i_gubn  = 'X'            "HAC/HMM -> HMMA
                i_extc  = lt_zseg1-wkexc
                i_intc  = lt_zseg1-wkinc
              IMPORTING
                e_extc  = lt_zseg1-wkexc
                e_intc  = lt_zseg1-wkinc.
          ENDIF.

          READ TABLE lt_idoc WITH KEY docnum = lt_edidd-docnum
                                      wo_ser = lt_zseg1-prdod
                                      natn   = lt_zseg1-natn
                                      dist   = lt_zseg1-dist
                                      wkexc  = lt_zseg1-wkexc
                                      wkinc  = lt_zseg1-wkinc
                             BINARY SEARCH.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.

          lt_zseg1-docnum = lt_edidd-docnum.
          APPEND lt_zseg1.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.

*-temp
*  DELETE  lt_zseg1 WHERE prdod  <> 'D11051044'.
*  DELETE  lt_zseg2 WHERE prdod  <> 'D11051044'.


  gt_zposeg1[] = lt_zseg1[] .
ENDFORM.                    " GET_IDOC
*&---------------------------------------------------------------------*
*&      Form  EXEC_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0511   text
*----------------------------------------------------------------------*
FORM exec_update USING    p_ucomm.
  DATA :  pr_zseg2 LIKE TABLE OF zposeg2 WITH HEADER LINE,
          lt_return LIKE TABLE OF bapireturn.

  LOOP AT gt_item WHERE status EQ '@5C@'.
    MOVE-CORRESPONDING gt_item TO pr_zseg2.
    APPEND pr_zseg2.
  ENDLOOP.

  CASE p_ucomm.
    WHEN 'INP'.
      CALL FUNCTION 'Z_FPP_HMA_UPDATE_UM'
        TABLES
          return = lt_return
          item   = pr_zseg2.
    WHEN 'VP'.
      CLEAR : pr_zseg2 , pr_zseg2[].
      CALL FUNCTION 'Z_FPP_HMA_FILLVIN'
        EXPORTING
          i_zvin_init = p_zvin
        TABLES
          item        = pr_zseg2.
  ENDCASE.
ENDFORM.                    " EXEC_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P1000_START_PROGRESSBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p1000_start_progressbar USING pf_text
                                   value(pf_val).

  DATA: percent(3) TYPE n.

  MOVE: sy-index TO percent.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = pf_val
      text       = pf_text
    EXCEPTIONS
      OTHERS     = 1.

ENDFORM.                    " P1000_START_PROGRESSBAR
*&---------------------------------------------------------------------*
*&      Form  CHECK_MULTI_RUN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_multi_run .

  CLEAR: w_flag.
  DO 30 TIMES.
    CALL FUNCTION 'ENQUEUE_EPROG'
      EXPORTING
        mode_trdir     = 'E'
        programm       = sy-cprog
*       X_PROGRAMM     = ' '
        _scope         = '1'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      CLEAR: w_flag.
      EXIT.
    ELSE.
      w_flag = 'X'.
      WAIT UP TO 6 SECONDS.
    ENDIF.
  ENDDO.
  IF w_flag = 'X'.
    MESSAGE s001 WITH 'The Program is running by other'.
  ENDIF.
ENDFORM.                    " CHECK_MULTI_RUN
*&---------------------------------------------------------------------*
*&      Form  UNLOCK_PROG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM unlock_prog .
  CALL FUNCTION 'DEQUEUE_EPROG'
    EXPORTING
      mode_trdir = 'E'
      programm   = sy-cprog
      x_programm = ' '
      _scope     = '1'
*     _SYNCHRON  = ' '
*     _COLLECT   = ' '
    .
ENDFORM.                    " UNLOCK_PROG
*&---------------------------------------------------------------------*
*&      Form  CHECK_SCREEN
*&---------------------------------------------------------------------*
FORM check_screen .
  IF p_zvin = 'X' AND p_nation IS INITIAL.
    MESSAGE s001 WITH 'W/Order nation is required'.
    STOP.
  ENDIF.

  IF p_zvin = 'X'.
    SELECT SINGLE *
    FROM ztsd_um
    WHERE status    = 'F'
      AND wo_nation = p_nation.
    IF sy-subrc = 0.
      MESSAGE s003 WITH p_nation 'has already done ZVIN Marriage'.
      STOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_SCREEN
