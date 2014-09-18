*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZPPI04300T
*& Program Name   : Production Result(MIP) Interface
*& Created by     : Victor Park
*& Created on     : 07.01.2011
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. :
*& Stru.     : ZSPPMIP_LOG
*&----------------------------------------------------------------------

REPORT zppi04300t MESSAGE-ID zmpp.

TABLES : mkpf, mseg, makt, ztppmip_log, ztpperm.

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

DATA : it_ztpperm LIKE ztpperm OCCURS 0 WITH HEADER LINE.

DATA : it_knmt    LIKE knmt    OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_data OCCURS 0.
DATA : chk(1).
        INCLUDE STRUCTURE zsppmip_log.
DATA :  shkzg LIKE mseg-shkzg,
        mblnr LIKE mseg-mblnr,
        mjahr LIKE mseg-mjahr,
        zeile LIKE mseg-zeile.
DATA :  prod_date  LIKE sy-datum.
DATA : END OF it_data.

DATA : it_modify LIKE zsppmip_log OCCURS 0 WITH HEADER LINE.
DATA : it_modify_tmp LIKE zsppmip_log OCCURS 0 WITH HEADER LINE.

DATA : wa_save TYPE ztppmip_log.
DATA : it_save TYPE STANDARD TABLE OF ztppmip_log WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).


*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS :   s_budat  FOR mkpf-budat,
                   s_werks for mseg-werks.
*PARAMETERS : p_werks LIKE t001w-werks OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-t01.
PARAMETERS : p_send RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(5) text-t02.
PARAMETERS : p_alv  RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-b03.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(7) text-t03.
PARAMETERS : p_daily RADIOBUTTON GROUP r2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(8) text-t04.
PARAMETERS : p_month  RADIOBUTTON GROUP r2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b3.

* by Daniel on 12/13/2011 {
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-b04.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(8) text-t05.
PARAMETERS : p_hist AS CHECKBOX.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b4.
* {
*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  g_repid      = sy-repid.
  PERFORM initial.

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

  IF p_send = 'X'.
    PERFORM  pro_batch.
  ELSE.
    PERFORM pro_alv.
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .
  DATA : l_zvin(18) TYPE n.

* by Daniel on 12/13/11 {
  IF p_hist IS INITIAL.
    SELECT DISTINCT eitem
      INTO CORRESPONDING FIELDS OF TABLE it_ztpperm
    FROM ztpperm
    WHERE erpid  =  'E05'
      AND prod_dt  IN s_budat
      AND zresult = 'S'
      AND zmsg LIKE 'GR%'.
  ELSE.
    SELECT DISTINCT eitem
        INTO CORRESPONDING FIELDS OF TABLE it_ztpperm
      FROM ztpperm_bk
      WHERE erpid  =  'E05'
        AND prod_dt  IN s_budat
        AND zresult = 'S'
        AND zmsg LIKE 'GR%'.
  ENDIF.
* }

  SORT it_ztpperm BY eitem.
  DELETE ADJACENT DUPLICATES FROM it_ztpperm COMPARING eitem.

  CHECK NOT it_ztpperm[] IS INITIAL.

  SELECT    a~werks c~matnr c~maktx a~menge a~shkzg b~budat AS prod_date
             a~mblnr a~mjahr a~zeile
         FROM mseg AS a  INNER JOIN makt AS c
                           ON a~matnr  = c~matnr
                         INNER JOIN mkpf AS b
                           ON a~mblnr  = b~mblnr
                          AND a~mjahr  = b~mjahr
             INTO CORRESPONDING FIELDS OF TABLE it_data
             FOR ALL ENTRIES IN it_ztpperm
         WHERE a~zbudat   IN s_budat
** for E002
*           AND werks       =  'E001'
** on 03/07/13
*           AND werks       =  p_werks
            AND werks    in s_werks
** end on 03/07/13
** end
           AND ( a~bwart   = '131' OR a~bwart  = '132' )
           AND c~spras     =  sy-langu
           AND a~matnr     =  it_ztpperm-eitem
    %_HINTS ORACLE 'ORDERED USE_NL(T_01 T_00) INDEX (T_00 "MSEG~Z03")'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_knmt
    FROM knmt
    WHERE kunnr =  'AKNH'.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .

  CLEAR : it_modify[].

  IF it_data[] IS INITIAL.
    MESSAGE s002 WITH 'There is No data'.
    STOP.
  ENDIF.

  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_modify.
    it_modify-budat  = it_data-prod_date.
    it_modify-corp   = 'HMMA'.
** On 03/07/13
*    it_modify-werks  = 'HEA1'.
    case it_data-WERKS.
      when 'E001'.
        it_modify-werks  = 'HEA1'.
      when 'E002'.
        it_modify-werks  = 'HEA2'.
    ENDCASE.
** End on 03/07/13
    IF it_data-shkzg = 'H'.
      it_modify-menge  =   it_modify-menge * -1.
    ENDIF.

    CLEAR : it_knmt.
    READ TABLE  it_knmt WITH KEY matnr  = it_modify-matnr.
    IF sy-subrc = 0.
      it_modify-part_no = it_knmt-kdmat.
    ENDIF.
    COLLECT it_modify.

  ENDLOOP.

  IF p_month = 'X'.
    LOOP AT it_modify.
      MOVE-CORRESPONDING it_modify TO it_modify_tmp.
      it_modify_tmp-budat  =  it_modify_tmp-budat+0(6).
      COLLECT it_modify_tmp.
    ENDLOOP.

    CLEAR : it_modify[].
    it_modify[]  =  it_modify_tmp[].
  ENDIF.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
FORM pro_batch .
  DATA : v_dest(30) VALUE 'WMHR01'.   "Interface Destination.

  CLEAR : e_return.

  CHECK NOT it_modify[] IS INITIAL.

  CALL FUNCTION 'Z_PP_IF_OB_PRDMIP_VAATZ' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_data                = it_modify
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF e_return-type = 'S' AND  sy-subrc = 0.   "Success
    PERFORM save_log  USING 'S' 'Success'    ''.
    WRITE : 'Interface : Success'.
    MESSAGE s003 WITH 'Interface : Success'.
  ELSE.
    PERFORM save_log  USING 'E' e_return-message l_msgtxt.
    WRITE :/ e_return-message, l_msgtxt.      "For Spool
    MESSAGE e003 WITH  e_return-message l_msgtxt.
  ENDIF.

ENDFORM.                    " PRO_BATCH

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
FORM save_log  USING    p_type p_msg1 p_msg2.
  DATA : l_zseq(10) TYPE n.

  CLEAR : it_save[], it_save.

  SELECT zseq INTO l_zseq
    FROM ztppmip_log
    UP TO 1 ROWS
    WHERE zdate = sy-datum
    ORDER BY zseq DESCENDING.
  ENDSELECT.

  LOOP AT it_modify.
    MOVE-CORRESPONDING it_modify TO it_save.

    it_save-zdate = sy-datum.
    it_save-zseq  = l_zseq + sy-tabix.
    it_save-ztime = sy-uzeit.
    it_save-ernam = sy-uname.
    it_save-zrslt = p_type.
    IF p_type = 'E'.
      IF NOT p_msg1 IS  INITIAL.
        it_save-zmsg  = p_msg1.
      ELSE.
        it_save-zmsg  = p_msg2.
      ENDIF.
    ENDIF.

    APPEND it_save.
    CLEAR : it_save.

  ENDLOOP.

  INSERT ztppmip_log FROM TABLE it_save
                             ACCEPTING DUPLICATE KEYS .
  COMMIT WORK AND WAIT.


ENDFORM.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  INITIAL
*&---------------------------------------------------------------------*
FORM initial .

ENDFORM.                    " INITIAL

*&---------------------------------------------------------------------*
*&      Form  pro_alv
*&---------------------------------------------------------------------*
FORM pro_alv.

  PERFORM layout_build       USING   gs_layout.
  PERFORM sorttab_build      USING   gt_sort.
  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   'IT_MODIFY'.

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
      t_outtab                 = it_modify[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.                    " pro_alv

*&---------------------------------------------------------------------*
*&      Form  layout_build
*&---------------------------------------------------------------------*
FORM layout_build USING  p_layout TYPE slis_layout_alv.

  p_layout-zebra             = 'X'.
  p_layout-colwidth_optimize = 'X'.
*  p_layout-key_hotspot = 'X'.
*  p_layout-box_fieldname  =    'CHK'.  "SELECTION FIELD
*  p_layout-coltab_fieldname = 'COL_COLOR'. "color field of itabe
*  p_layout-cell_merge        = 'X'.
*  p_layout-detail_popup      = 'X'.
*  p_layout-detail_titlebar   = sy-title.
*  p_layout-no_subtotals      = ''.

ENDFORM.                    " layout_build

*&---------------------------------------------------------------------*
*&      Form  sorttab_build
*&---------------------------------------------------------------------*
FORM sorttab_build USING   p_sort TYPE slis_t_sortinfo_alv.

  CLEAR: gs_sort, p_sort[].

  gs_sort-spos      = '1'.
  gs_sort-tabname   = 'IT_MODIFY'.
  gs_sort-fieldname = 'BUDAT'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '2'.
  gs_sort-tabname   = 'IT_MODIFY'.
  gs_sort-fieldname = 'MATNR'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

ENDFORM.                    " sorttab_build

*&---------------------------------------------------------------------*
*&      Form  fieldcat
*&---------------------------------------------------------------------*
FORM fieldcat TABLES   pt_fieldcat TYPE  slis_t_fieldcat_alv
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
        pt_fieldcat-seltext_m    = 'Company'.
*        pt_fieldcat-no_out       = 'X'.
      WHEN 'BUDAT'.
        pt_fieldcat-seltext_m    = 'Production Date'.
*        pt_fieldcat-no_out       = 'X'.
      WHEN OTHERS.

    ENDCASE.
    pt_fieldcat-reptext_ddic =
    pt_fieldcat-seltext_s    =
    pt_fieldcat-seltext_l    =
    pt_fieldcat-seltext_m.

    MODIFY pt_fieldcat.

  ENDLOOP.

ENDFORM.                    " fieldcat

*&---------------------------------------------------------------------*
*&      Form  list_header_write
*&---------------------------------------------------------------------*
FORM list_header_write USING alv_t_listheader TYPE slis_t_listheader.

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  DATA : h_title(30), s_title(60),  a_title(60).

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  CONCATENATE '*Production Date : ' s_budat-low ' ~ '  s_budat-high
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

ENDFORM.                    " list_header_write

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
