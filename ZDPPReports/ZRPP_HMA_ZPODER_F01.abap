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
*&      Form  P2000_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p2000_get_data.

  DATA : lt_edidc LIKE TABLE OF edidc WITH HEADER LINE,
         lt_edidd LIKE TABLE OF edidd WITH HEADER LINE.

  DATA : lt_zseg1 TYPE TABLE OF t_zseg1 WITH HEADER LINE.

  DATA : lt_data LIKE TABLE OF gt_data ,
         wa_data LIKE LINE OF gt_data.

  DATA : lt_idoc LIKE TABLE OF ztpp_po_idoc WITH HEADER LINE.
  DATA : lv_model(3).

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

  CLEAR : gv_new .
  IF r_new EQ 'X' .
    gv_new = 'X'.
  ELSEIF r_old EQ 'X'.
    CLEAR : gv_new.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_idoc
    FROM ztpp_po_idoc
    WHERE crdat IN s_credat
  %_HINTS ORACLE 'index("ZTPP_PO_IDOC","ZTPP_PO_IDOC~N1")'.

  CHECK NOT lt_idoc[] IS INITIAL.
  PERFORM p1000_start_progressbar USING text-p01 '15'.
  SORT lt_idoc BY crdat crtim docnum DESCENDING wo_ser.

  IF r_new EQ 'X' .
    LOOP AT lt_idoc WHERE type EQ 'X'.
      APPEND lt_idoc TO gt_idoc.
    ENDLOOP.
  ELSEIF r_old EQ 'X'.
    LOOP AT lt_idoc WHERE type EQ ''.
      APPEND lt_idoc TO gt_idoc.
    ENDLOOP.
  ELSE.
    gt_idoc[] = lt_idoc[].
  ENDIF.


  lt_idoc[] = gt_idoc[].
  IF lt_idoc[] IS INITIAL.    "Victor 02.09.2012
    MESSAGE s000 WITH 'There is No data'.
    STOP.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_edidc
  FROM edidc
  FOR ALL ENTRIES IN lt_idoc
  WHERE docnum EQ lt_idoc-docnum
  AND mestyp EQ p_mestyp
  AND status IN r_status
  AND credat IN s_credat
  AND direct EQ '2'
 .

  CHECK  NOT lt_edidc[] IS INITIAL.
  PERFORM p1000_start_progressbar USING text-p01 '30'.

  LOOP AT  lt_edidc .

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

        lt_zseg1-upddat = lt_edidc-upddat.
        lt_zseg1-updtim = lt_edidc-updtim.
        lt_zseg1-docnum = lt_edidc-docnum.
        APPEND : lt_zseg1.
      ENDLOOP.
    ENDIF.
    CLEAR : lt_zseg1, lt_edidd, lt_edidd[].
  ENDLOOP.
  PERFORM p1000_start_progressbar USING text-p01 '40'.

  gt_zposeg1[] = lt_zseg1[] .

  SORT lt_zseg1 BY docnum prdod natn dist wkexc wkinc.
  DELETE ADJACENT DUPLICATES FROM lt_zseg1.

  LOOP AT lt_zseg1 .



    MOVE-CORRESPONDING lt_zseg1 TO gt_data.
    READ TABLE lt_idoc WITH KEY docnum = lt_zseg1-docnum
                                wo_ser = lt_zseg1-prdod
                                natn   = lt_zseg1-natn
                                dist   = lt_zseg1-dist
                                wkexc  = lt_zseg1-wkexc
                                wkinc  = lt_zseg1-wkinc.

    IF sy-subrc <> 0 . CLEAR lt_idoc. ENDIF.

    IF lt_idoc-status EQ 'E'. "Error
      gt_data-status = '@02@'.
    ELSEIF lt_idoc-status EQ 'S'. " Complete
      gt_data-status = '@01@'.
    ELSEIF lt_idoc-status EQ ''. "Ready
      gt_data-status = '@3C@'.
    ELSEIF lt_idoc-status EQ 'X'. "Complete
      gt_data-status = '@2W@'.

    ELSE.
      gt_data-status = lt_idoc-status.
    ENDIF.

    gt_data-upddat   = lt_zseg1-upddat.
    gt_data-updtim   = lt_zseg1-updtim.
    gt_data-docnum   = lt_zseg1-docnum.
    gt_data-wo_ser   = lt_zseg1-prdod .
    gt_data-nation   = lt_zseg1-natn  .
    gt_data-dealer   = lt_zseg1-dist  .
    gt_data-extc     = lt_zseg1-wkexc .
    gt_data-intc     = lt_zseg1-wkinc .

    gt_data-dest     = lt_zseg1-destn .
    gt_data-moye     = lt_zseg1-mdyr  .
    gt_data-bmdl     = lt_zseg1-mdinx .
    gt_data-ocnn     = lt_zseg1-occn  .

    gt_data-vers     = lt_zseg1-grade .
    gt_data-initqty  = lt_zseg1-ioqty .
    gt_data-modqty   = lt_zseg1-moqty .
* by Daniel {
    gt_data-woups   = lt_zseg1-woups .
* }
    gt_data-lcno     = lt_zseg1-lcldl .
    gt_data-lcnt     = lt_zseg1-lccnt .
    gt_data-flet     = lt_zseg1-fltfg .
    gt_data-req_date = lt_zseg1-rdd   .
    gt_data-crt_date = lt_zseg1-crdat .
    gt_data-chg_date = lt_zseg1-aedat .

    gt_data-ordqty    =  lt_zseg1-ioqty .
    gt_data-newqty    =  lt_zseg1-moqty .
    gt_data-zsdat	  = lt_zseg1-crdat .
    gt_data-zstim	  = lt_zseg1-aedat .
    gt_data-zuser	  = sy-uname.


    APPEND gt_data.
  ENDLOOP.
  PERFORM p1000_start_progressbar USING text-p01 '90'.
ENDFORM.                    " P2000_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  TEST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM test_data.
  IF gt_data[] IS INITIAL.
    DATA : lv_cnt    TYPE i,
           lv_dat(3) TYPE n,
           lv_wk(3)  TYPE n.
    DATA : lv_result TYPE i,
           lv_status TYPE i.

    lv_cnt = s_credat-high - s_credat-low .

    lv_dat = lv_wk = 0 .

    gt_data-upddat = s_credat-low + lv_dat.
    gt_data-updtim = sy-uzeit + lv_dat.

    DO lv_cnt TIMES.

      lv_result = lv_dat MOD 4 .
      IF lv_result EQ 0 .
        gt_data-upddat = s_credat-low + lv_dat.
      ENDIF.
      lv_status = lv_dat MOD 2 .
      IF lv_status EQ 0 .
        gt_data-status = 'S'.
      ENDIF.
      lv_dat = lv_dat + 1 .
      gt_data-docnum = lv_dat.
      gt_data-updtim = sy-uzeit + lv_dat.


      DO 10 TIMES.
        lv_wk = lv_wk + 1.
        gt_data-wo_ser = lv_wk.

        APPEND gt_data.
      ENDDO.
      CLEAR : gt_data-status.
    ENDDO.
  ENDIF.


ENDFORM.                    " TEST_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_PO_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p3000_po_proc.
  DATA : return LIKE TABLE OF bapireturn WITH HEADER LINE.
  DATA : lt_edidd LIKE TABLE OF edidd WITH HEADER LINE,
         lt_edidc LIKE TABLE OF edidc WITH HEADER LINE.

  DATA : lv_cnt TYPE i.

**** Create PO or Modify PO
  DATA : lv_new(1), lv_tran(1).
  CLEAR : lv_new , lv_tran.

  READ TABLE gt_idoc WITH KEY docnum = gv_docnum .
  lv_new = gt_idoc-type.

* by Daniel on 04/14/2011 {
*  CASE 'X'.
*    WHEN R_RUN .
*      LV_TRAN = 'X'.
*    WHEN R_SAV .
*      LV_TRAN = ''.
*  ENDCASE.
*
* run default with ZTPP_KSBOHMM_IF
  lv_tran = 'X'.
* }

  CALL FUNCTION 'Z_FPP_HMA_IF_PO'
    EXPORTING
      new     = lv_new
      idocnum = gv_docnum
      tran    = lv_tran
    TABLES
      return  = return
      header  = gt_send.

  CLEAR gt_data .

****
  READ TABLE return INDEX 1.
  IF return-type EQ 'S'.
    LOOP AT gt_send.
      READ TABLE  gt_data WITH KEY wo_ser = gt_send-prdod
                                   nation = gt_send-natn
                                   dealer = gt_send-dist
                                   extc   = gt_send-wkexc
                                   intc   = gt_send-wkinc.
      IF sy-subrc = 0 .
        gt_data-status = '@4A@'.
        gt_data_detail-status = '@4A@'.

        MODIFY gt_data TRANSPORTING status
            WHERE docnum EQ gv_docnum
            AND  wo_ser = gt_send-prdod
            AND nation = gt_send-natn
            AND dealer = gt_send-dist
            AND extc   = gt_send-wkexc
            AND intc   = gt_send-wkinc.

        MODIFY gt_data_detail TRANSPORTING status
            WHERE docnum EQ gv_docnum
            AND  wo_ser = gt_send-prdod
            AND nation = gt_send-natn
            AND dealer = gt_send-dist
            AND extc   = gt_send-wkexc
            AND intc   = gt_send-wkinc.
      ENDIF.
    ENDLOOP.

    DATA : lv_send TYPE i.

    DESCRIBE TABLE gt_send LINES lv_send .

    MESSAGE s003 WITH lv_send 'Counts processed . '.
  ELSEIF return-type EQ 'E'.
    gt_data-status = '@03@'.
    gt_data_detail-status = '@03@'.

    MODIFY gt_data TRANSPORTING status
      WHERE docnum EQ gv_docnum .

    MODIFY gt_data_detail TRANSPORTING status
      WHERE  docnum EQ gv_docnum.

  ENDIF.

*  PERFORM LVC_REFRESH_DISPLAY USING SCREEN_INIT=>LIST_VIEWER .
ENDFORM.                    " P3000_PO_PROC

*&---------------------------------------------------------------------*
*&      Form  P3100_PO_REJC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p3100_po_rejc.

  DATA : lt_edidd LIKE TABLE OF edidd WITH HEADER LINE,
         lt_edidc LIKE TABLE OF edidc WITH HEADER LINE.

  CLEAR gt_data .
  gt_data-status = '@5C@'.
  gt_zposeg1-status = 'REJC'.

  MODIFY gt_data TRANSPORTING status WHERE status IS INITIAL.
  MODIFY gt_zposeg1 TRANSPORTING status WHERE status IS INITIAL.

  LOOP AT gt_zposeg1.
    lt_edidd-segnam = c_zposeg1.
    lt_edidd-sdata  = gt_zposeg1.

    APPEND lt_edidd.

*    /for Test
    UPDATE ztpp_po_idoc SET status = 'X'
          WHERE wo_ser = gt_zposeg1-prdod.

  ENDLOOP.


  PERFORM lvc_refresh_display USING screen_init=>list_viewer .
ENDFORM.                    " P3100_PO_REJC


*---------------------------------------------------------------------*
*       FORM LVC_REFRESH_DISPLAY                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM lvc_refresh_display USING l_grid TYPE REF TO cl_gui_alv_grid.
*  SCROLL ????? ??.
  CALL METHOD cl_gui_cfw=>dispatch.
  CALL METHOD cl_gui_cfw=>flush.
  CALL METHOD cl_gui_cfw=>update_view.

  PERFORM p1000_create_grid USING l_grid
                          '' ''
                          gv_docnum.
*
*  CALL METHOD L_GRID->GET_SCROLL_INFO_VIA_ID
*    IMPORTING
*      ES_COL_INFO = GS_CURR_COL
*      ES_ROW_NO   = GS_CURR_ROW.
** Layout
*  CALL METHOD L_GRID->SET_FRONTEND_LAYOUT
*    EXPORTING
*      IS_LAYOUT = GS_LAYOUT.
**  REFRESH DISPLAY
  CALL METHOD l_grid->refresh_table_display.
**REFRESH ? SCROLL ??? ??.
*  CALL METHOD L_GRID->SET_SCROLL_INFO_VIA_ID
*    EXPORTING
*      IS_COL_INFO = GS_CURR_COL
*      IS_ROW_NO   = GS_CURR_ROW.
*
ENDFORM.                    "LVC_REFRESH_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CHANGE_SEL_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_sel_screen.
  IF r_new EQ 'X' .
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'OLD'.
          screen-active = 0.
        WHEN 'NEW'.
          screen-active = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ELSEIF r_old EQ 'X'.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'OLD'.
          screen-active = 1.
        WHEN 'NEW'.
          screen-active = 0.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " CHANGE_SEL_SCREEN
