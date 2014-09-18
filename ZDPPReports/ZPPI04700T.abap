*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZPPI04700T
*& Program Name   : Production Result to GEMS
*& Created by     : Victor Park
*& Created on     : 03.27.2012
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. :
*& Stru.     :
*&----------------------------------------------------------------------

REPORT zppi04700t MESSAGE-ID zmpp.

TABLES : ztpp04700_log, mseg.

*- ALV
TYPE-POOLS: slis.
DATA: gt_fieldcat         TYPE slis_t_fieldcat_alv,
      gs_layout           TYPE slis_layout_alv,
      gs_sort             TYPE slis_sortinfo_alv,
      gt_sort             TYPE slis_t_sortinfo_alv,
      gs_light            TYPE lvc_s_layo,
      gs_print            TYPE slis_print_alv,
      gt_sp_group         TYPE slis_t_sp_group_alv,
      gt_events           TYPE slis_t_event,
      gs_events           LIKE  LINE OF gt_events,
      g_save              VALUE 'A',
      gx_variant          LIKE disvariant,
      g_variant           LIKE disvariant.

DATA : ls_title         TYPE slis_listheader, "alv header
       alv_t_listheader TYPE slis_t_listheader.

DATA : g_extab          TYPE slis_t_extab,
       g_extab_ln       LIKE   LINE  OF  g_extab.

DATA : g_user_command  TYPE slis_formname VALUE 'USER_COMMAND'.
DATA : t_colinfo_table TYPE slis_t_specialcol_alv WITH HEADER LINE.
DATA : g_repid         LIKE sy-repid.


DATA : BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE zspp04700.
DATA : END OF it_data.


DATA : it_modify LIKE it_data OCCURS 0 WITH HEADER LINE.
DATA : it_save TYPE STANDARD TABLE OF  ztpp04700_log WITH HEADER LINE.

DATA : BEGIN OF it_date OCCURS 0,
       date TYPE sy-datum,
       END OF it_date.

DATA : BEGIN OF it_inqty OCCURS 0,
          budat LIKE ztpppr-rdate,
          erfmg LIKE ztpppr-pqty,
       END OF it_inqty.

DATA : BEGIN OF it_outqty OCCURS 0,
          budat LIKE ztpppr-rdate,
          erfmg LIKE ztpppr-pqty,
       END OF it_outqty.

DATA : BEGIN OF it_bodyout OCCURS 0,
          prod_date TYPE sy-datum,
          prod_qty  LIKE ztpp04700_log-prod_qty,
       END OF it_bodyout.

DATA : it_pbsin   LIKE it_bodyout   OCCURS 0 WITH HEADER LINE.
DATA : it_lineout LIKE it_bodyout OCCURS 0 WITH HEADER LINE.
DATA : it_signoff LIKE it_bodyout OCCURS 0 WITH HEADER LINE.

DATA : it_bodyout_q LIKE it_bodyout OCCURS 0 WITH HEADER LINE.
DATA : it_pbsin_q   LIKE it_pbsin   OCCURS 0 WITH HEADER LINE.
DATA : it_lineout_q LIKE it_lineout OCCURS 0 WITH HEADER LINE.
DATA : it_signoff_q LIKE it_signoff OCCURS 0 WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.

SELECT-OPTIONS : s_werks FOR mseg-werks MEMORY ID wrk OBLIGATORY
                                        DEFAULT 'P001',
                 s_date FOR sy-datum  NO-EXTENSION   OBLIGATORY
                                        DEFAULT sy-datum.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-t01.
PARAMETERS : p_send RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(5) text-t02.
PARAMETERS : p_alv  RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.
*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM init.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_data.
  PERFORM modify_data.

*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF p_send = 'X'.
    PERFORM  pro_batch.
  ELSE.
    PERFORM pro_alv.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .
  DATA : lv_date TYPE sy-datum.

  CLEAR : it_date[].

*-Get date information from selection screen
  READ TABLE s_date INDEX 1.
  IF s_date-high IS INITIAL.
    it_date-date  = s_date-low.
    APPEND it_date.
  ELSE.
    DO.
      lv_date  = s_date-low + sy-index - 1.
      IF lv_date <= s_date-high.
        it_date-date  = lv_date.
        APPEND it_date.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  SELECT a~rdate AS budat SUM( a~pqty ) AS erfmg
    INTO CORRESPONDING FIELDS OF TABLE it_inqty
  FROM ztpppr AS a
  WHERE a~prpid  = 'R03'
    AND a~rdate IN s_date
    GROUP BY a~rdate.

  SELECT a~rdate AS budat SUM( a~pqty ) AS erfmg
    INTO CORRESPONDING FIELDS OF TABLE it_outqty
  FROM ztpppr AS a
  WHERE a~prpid  = 'R12'
    AND a~rdate IN s_date
    GROUP BY a~rdate.

*-Body Out
  SELECT a~rp02_sdate AS prod_date
    INTO CORRESPONDING FIELDS OF TABLE it_bodyout
  FROM ztpp_rpid AS a
  WHERE  a~rp02_sdate IN s_date.

*-PBS In
  SELECT a~rp04_sdate AS prod_date
    INTO CORRESPONDING FIELDS OF TABLE it_pbsin
  FROM ztpp_rpid AS a
  WHERE  a~rp04_sdate IN s_date.

*-Line Out
  SELECT a~rp17_sdate AS prod_date
    INTO CORRESPONDING FIELDS OF TABLE it_lineout
  FROM ztpp_rpid AS a
  WHERE  a~rp17_sdate IN s_date.

*-Sign Off
  SELECT a~rp18_sdate AS prod_date
    INTO CORRESPONDING FIELDS OF TABLE it_signoff
  FROM ztpp_rpid AS a
  WHERE  a~rp18_sdate IN s_date.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  CLEAR : it_data[].

  LOOP AT it_bodyout.
    MOVE-CORRESPONDING it_bodyout TO it_bodyout_q.
    it_bodyout_q-prod_qty = 1.
    COLLECT it_bodyout_q.
  ENDLOOP.

  LOOP AT it_pbsin.
    MOVE-CORRESPONDING it_pbsin  TO it_pbsin_q.
    it_pbsin_q-prod_qty = 1.
    COLLECT it_pbsin_q.
  ENDLOOP.

  LOOP AT it_lineout.
    MOVE-CORRESPONDING it_lineout  TO it_lineout_q.
    it_lineout_q-prod_qty = 1.
    COLLECT it_lineout_q.
  ENDLOOP.

  LOOP AT it_signoff.
    MOVE-CORRESPONDING it_signoff  TO it_signoff_q.
    it_signoff_q-prod_qty = 1.
    COLLECT it_signoff_q.
  ENDLOOP.

  LOOP AT it_date.
    CLEAR : it_inqty, it_outqty, it_bodyout_q, it_pbsin_q,
            it_lineout_q, it_signoff_q, it_data.

    it_data-werks = 'HVA1'.
    it_data-corp  = 'HMMA'.
    it_data-prod_date = it_date-date.
    CONCATENATE sy-datum sy-uzeit INTO    it_data-zzcdate.

*-  Blank/Panel
    READ TABLE  it_inqty  WITH KEY budat = it_date-date.
    READ TABLE  it_outqty WITH KEY budat = it_date-date.

    it_data-in_qty    = it_inqty-erfmg.
    it_data-prod_qty  = it_outqty-erfmg.
    it_data-mdv01     = '11MXSX1B'.
    it_data-zcpdiv    = 'M'.
    APPEND it_data. CLEAR it_data-in_qty.

*-  Body Out
    READ TABLE  it_bodyout_q  WITH KEY prod_date = it_date-date.
    it_data-prod_qty  = it_bodyout_q-prod_qty.
    it_data-mdv01     = '11P02'.
    it_data-zcpdiv    = 'B'.
    APPEND it_data. CLEAR it_data-in_qty.

*-  PBS In
    READ TABLE  it_pbsin_q  WITH KEY prod_date = it_date-date.
    it_data-prod_qty  = it_pbsin_q-prod_qty.
    it_data-mdv01     = '11P37'.
    it_data-zcpdiv    = 'P'.
    APPEND it_data. CLEAR it_data-in_qty.

*-  Line Out
    READ TABLE  it_lineout_q  WITH KEY prod_date = it_date-date.
    it_data-prod_qty  = it_lineout_q-prod_qty.
    it_data-mdv01     = '11T11'.
    it_data-zcpdiv    = 'T'.
    APPEND it_data. CLEAR it_data-in_qty.

*-  Sign Off
    READ TABLE  it_signoff_q  WITH KEY prod_date = it_date-date.
    it_data-prod_qty  = it_signoff_q-prod_qty.
    it_data-mdv01     = '11T24'.
    it_data-zcpdiv    = 'S'.
    APPEND it_data. CLEAR it_data-in_qty.

  ENDLOOP.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
FORM pro_batch .
  data : v_dest(30) value 'WMPP01'.   "Interface Destination.

  CLEAR : e_return.

  CHECK NOT it_data[] IS INITIAL.

  CALL FUNCTION 'Z_PP_IF_OB_PROD_GEMS' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_data                = it_data
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF e_return-type = 'S' AND  sy-subrc = 0.   "Success
    PERFORM save_log  USING 'S' 'Success'    ''.
    MESSAGE s000 WITH 'Interface : Success'.
  ELSE.
    PERFORM save_log  USING 'E' e_return-message l_msgtxt.
    MESSAGE e000 WITH  e_return-message l_msgtxt.
  ENDIF.

ENDFORM.                    " PRO_BATCH

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
FORM save_log  USING    p_type p_msg1 p_msg2.
  DATA : l_zseq(10) TYPE n.

  CLEAR : it_save[], it_save.

  SELECT zseq INTO l_zseq
    FROM ztpp04700_log
    UP TO 1 ROWS
    WHERE zdate = sy-datum
    ORDER BY zseq DESCENDING.
  ENDSELECT.

  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_save.

    it_save-zdate = sy-datum.
    it_save-zseq  = l_zseq + sy-tabix.
    it_save-ztime = sy-uzeit.
    it_save-ernam = sy-uname.
    it_save-zif_res = p_type.
    IF p_type = 'E'.
      IF p_msg1 IS NOT INITIAL.
        it_save-zif_msg  = p_msg1.
      ELSE.
        it_save-zif_msg  = p_msg2.
      ENDIF.
    ENDIF.

    APPEND it_save.
    CLEAR : it_save.

  ENDLOOP.

  INSERT ztpp04700_log FROM TABLE it_save
                             ACCEPTING DUPLICATE KEYS .
  COMMIT WORK AND WAIT.

ENDFORM.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
FORM pro_alv .

  FIELD-SYMBOLS : <itab> TYPE STANDARD TABLE.
  DATA : lv_tab(30),
         lv_stru(30).

  lv_tab  = 'IT_DATA[]'.
  lv_stru = 'IT_DATA'.

  ASSIGN : (lv_tab) TO <itab>.

  PERFORM layout_build       USING   gs_layout.
  PERFORM sorttab_build      USING   gt_sort lv_stru.
  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   lv_stru.


  PERFORM list_header_write USING alv_t_listheader[].
  PERFORM append_alv_event  CHANGING   gt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
*     i_callback_pf_status_set = 'PF_STATUS'
      i_callback_user_command  = g_user_command
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      it_sort                  = gt_sort
      i_save                   = g_save
*     is_variant               = g_variant
      it_events                = gt_events[]
    TABLES
      t_outtab                 = <itab>
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.                    " PRO_ALV
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
FORM layout_build  USING  p_layout TYPE slis_layout_alv.

  p_layout-zebra             = 'X'.
  p_layout-colwidth_optimize = 'X'.
*  p_layout-key_hotspot = 'X'.
*  p_layout-box_fieldname  =    'CHK'.  "SELECTION FIELD
*  p_layout-coltab_fieldname = 'COL_COLOR'. "color field of itabe
*  p_layout-cell_merge        = 'X'.
*  p_layout-detail_popup      = 'X'.
*  p_layout-detail_titlebar   = sy-title.
*  p_layout-no_subtotals      = ''.

ENDFORM.                    " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  SORTTAB_BUILD
*&---------------------------------------------------------------------*
FORM sorttab_build  USING   p_sort TYPE slis_t_sortinfo_alv
                            p_name TYPE  slis_tabname.

  CLEAR: gs_sort, p_sort[].

  gs_sort-spos      = '1'.
  gs_sort-tabname   = p_name.
  gs_sort-fieldname = 'PROD_DATE'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.


ENDFORM.                    " SORTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
FORM fieldcat  TABLES   pt_fieldcat TYPE  slis_t_fieldcat_alv
               USING    p_name      TYPE  slis_tabname.

  DATA: l_datum(08).

  sy-datum = sy-datum + 1.
  MOVE sy-datum TO l_datum.
  SET PARAMETER ID 'ALVBUFFER' FIELD l_datum.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
*     i_structure_name   = p_name
      i_internal_tabname = p_name
      i_inclname         = g_repid
    CHANGING
      ct_fieldcat        = pt_fieldcat[].

  LOOP AT pt_fieldcat.
    CASE pt_fieldcat-fieldname.
      WHEN 'CORP'.
        pt_fieldcat-seltext_m    = 'Corporation'.
      WHEN 'MDV01'.
        pt_fieldcat-seltext_m    = 'Production Line'.
      WHEN 'ZCPDIV'.
        pt_fieldcat-seltext_m    = 'Factory Code'.
      WHEN 'IN_QTY'.
        pt_fieldcat-seltext_m    = 'Quantity put into production'.
      WHEN 'PROD_QTY'.
        pt_fieldcat-seltext_m    = 'Production quantity'.
*        pt_fieldcat-do_sum       = 'X'.   "Sub Total
      WHEN OTHERS.

    ENDCASE.
    pt_fieldcat-reptext_ddic =
    pt_fieldcat-seltext_s    =
    pt_fieldcat-seltext_l    =
    pt_fieldcat-seltext_m.

    MODIFY pt_fieldcat.

  ENDLOOP.

ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
FORM list_header_write  USING alv_t_listheader TYPE slis_t_listheader.

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  DATA : h_title(30), s_title(60),  a_title(60).

  ls_title-typ = 'H'. "(H:Header, S:Selection, A:Action)
  CONCATENATE '*Plant : '  s_werks-low  '~' s_werks-high
                                                INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'.
  CONCATENATE '*Production Date : ' s_date-low '~' s_date-high
                    INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.

ENDFORM.                    " LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
*&      Form  APPEND_ALV_EVENT
*&---------------------------------------------------------------------*
FORM append_alv_event  CHANGING p_alv_event TYPE slis_t_event.
* TOP-OF-PAGE Event

  DATA ls_events TYPE slis_alv_event.
  ls_events-name  =  'TOP_OF_PAGE'.
  ls_events-form  =  'TOP_OF_PAGE'.
  APPEND ls_events TO p_alv_event.

ENDFORM.                    " APPEND_ALV_EVENT

*&---------------------------------------------------------------------
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = alv_t_listheader.

ENDFORM. " TOP_OF_PAGE

*&--------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&--------------------------------------------------------------------*
FORM  user_command USING ucomm    LIKE sy-ucomm
                    p_selfield    TYPE slis_selfield.
* double click : UCOMM = &IC1
  CASE ucomm.

  ENDCASE.

  p_selfield-refresh = 'X'.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init .

  g_repid  = sy-repid.

ENDFORM.                    " INIT
