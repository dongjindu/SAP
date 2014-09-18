************************************************************************
* Program Name      : ZACO_AALA_PARTS_UPLOAD
* Author            : Chris Li
* Creation Date     : 01/10/2004
* Specifications By :
* Pattern           : Report 1-1
* Development Request No : UD1K913697
* Addl Documentation:
* Description       : UPLOAD THE EXCEL FILE WHICH INCLUDES THE AALA
*                     PARTS CONFIRMED PRICES.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT zaco_aala_parts_upload MESSAGE-ID zmco NO STANDARD PAGE HEADING .


TABLES: ztco_aala_source.

*---------------------------------------------------------------------*
*     VARIABLE DECLARATION
*---------------------------------------------------------------------*
DATA: g_container TYPE scrfname VALUE 'SC_CONTAINER',
      grid1  TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container.
DATA: g_color    TYPE slis_t_specialcol_alv.
DATA: wa_color  LIKE LINE OF g_color.
DATA: s_error.
DATA: s_no_match.
DATA: WA_FILE TYPE STRING.
*---------------------------------------------------------------------*
*     INTERNAL TABLE DECALARATION
*---------------------------------------------------------------------*

DATA: BEGIN OF it_aala1 OCCURS 0,
        kokrs(4)  ," LIKE    ztco_aala_source-kokrs,
        matnr(18)  ," LIKE    ztco_aala_source-matnr,
        gjahr(4)   ,
        versn(3)  ," LIKE    ztco_aala_source-versn,
        lifnr(10) ,"  LIKE    ztco_aala_source-lifnr,
        klvar(4)  ," LIKE    ztco_aala_source-klvar,
        kstar(10) ,"  LIKE    ztco_aala_source-kstar,
*        KOSTL()  ," LIKE    ZTCO_AALA_SOURCE-KOSTL,
        stprs(11) ,"  LIKE    ztco_aala_source-stprs,
        peinh(5)  ," LIKE    ztco_aala_source-peinh,
        MEEHT(3)  ," LIKE    ztco_aala_source-MEEHT,
        waers(5)  ," LIKE    ztco_aala_source-waers,
        netpr(11) ,"  LIKE    ztco_aala_source-netpr,
        aaprs(11) ,"  LIKE    ztco_aala_source-aaprs,
        zwaers(5) ," LIKE    ztco_aala_source-zwaers,
        urzla(3)  ," LIKE    ztco_aala_source-urzla,
        ekgrp(3)  ," LIKE    ztco_aala_source-ekgrp,
        mtart(4)  ," LIKE    ztco_aala_source-mtart,
        datbi(8)  ," LIKE    ztco_aala_source-datbi,
        datab(8)  ," LIKE    ztco_aala_source-datab,
        zver_des(40) ,"  LIKE ztco_aala_source-zver_des,
      END OF it_aala1.
DATA: BEGIN OF it_aala OCCURS 0,
        kokrs   LIKE    ztco_aala_source-kokrs,
        matnr   LIKE    ztco_aala_source-matnr,
        gjahr   like    ztco_aala_source-gjahr,
        versn   LIKE    ztco_aala_source-versn,
        lifnr   LIKE    ztco_aala_source-lifnr,
        klvar   LIKE    ztco_aala_source-klvar,
        kstar   LIKE    ztco_aala_source-kstar,
*        KOSTL   LIKE    ZTCO_AALA_SOURCE-KOSTL,
        stprs   LIKE    ztco_aala_source-stprs,
        peinh   LIKE    ztco_aala_source-peinh,
        meeht   LIKE    ztco_aala_source-meeht,
        waers   LIKE    ztco_aala_source-waers,
        netpr   LIKE    ztco_aala_source-netpr,
        aaprs   LIKE    ztco_aala_source-aaprs,
        zwaers  LIKE    ztco_aala_source-zwaers,
        urzla   LIKE    ztco_aala_source-urzla,
        ekgrp   LIKE    ztco_aala_source-ekgrp,
        mtart   LIKE    ztco_aala_source-mtart,
        datbi   LIKE    ztco_aala_source-datbi,
        datab   LIKE    ztco_aala_source-datab,
        zver_des   LIKE ztco_aala_source-zver_des,
      END OF it_aala.


DATA: intern TYPE alsmex_tabline OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF gt_message OCCURS 0,
       msg(100),
      END OF gt_message.
DATA: it_error_record LIKE it_aala OCCURS 0 WITH HEADER LINE.
DATA: lt_aala LIKE ZTCO_AALA_SOURCE OCCURS 0 WITH HEADER LINE.

*---------------------------------------------------------------------*
*     ALV VARIBLES DECALARATION
*---------------------------------------------------------------------*
INCLUDE: <icon>.
INCLUDE : <list>.
TYPE-POOLS: slis.

*********ALV DEFINITION****************
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gt_fc       TYPE slis_t_fieldcat_alv,
      g_fieldcat_s LIKE LINE OF gt_fieldcat,
      gs_layout   TYPE slis_layout_alv,
      gs_print    TYPE slis_print_alv,
      gt_sort     TYPE slis_t_sortinfo_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_header   TYPE slis_t_listheader,
*      gt_header1  TYPE slis_t_listheader,
      gt_colinfo_table TYPE slis_t_specialcol_alv. "line color.

* hierarchy(simple)
DATA : g_tabname_header       TYPE slis_tabname,       "header part
       g_tabname_item         TYPE slis_tabname,       "detail list
       gs_keyinfo             TYPE slis_keyinfo_alv,   "relation key
       g_repid                LIKE sy-repid.
DATA:  g_variant LIKE disvariant.
* return
DATA : g_exit_caused_by_caller  TYPE c,
       gs_exit_caused_by_user   TYPE slis_exit_by_user.

DATA : alv_grid               TYPE REF TO cl_gui_alv_grid,
       gs_custom_container    TYPE REF TO cl_gui_custom_container.



*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-100.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  p_file  LIKE rlgrap-filename DEFAULT 'C:\       .TXT' OBLIGATORY,
  p_filety LIKE rlgrap-filetype DEFAULT 'DAT' NO-DISPLAY.
*  P_TCODE LIKE TSTC-TCODE DEFAULT 'MM01'.
SELECTION-SCREEN END   OF BLOCK b1.
PARAMETERS: p_std as checkbox.

*---------------------------------------------------------------------*
*     EVENTS
*---------------------------------------------------------------------*

START-OF-SELECTION.
  PERFORM upload_process.
  PERFORM data_check.
  IF s_error = 'X' OR
     s_no_match = 'X'.
    PERFORM write_message.
  ELSE.
    PERFORM display_data.
  ENDIF.
*  PERFORM UPDATE_ZTCO_AALA_SOURCE.
END-OF-SELECTION.



AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file 'O'.
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*      -->P_0040   text
*----------------------------------------------------------------------*
FORM at_sel_screen_on_value_request USING def_path LIKE rlgrap-filename
                                          mode     TYPE c.

  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: fieldln TYPE i.
  FIELD-SYMBOLS: <tmp_sym>.


  fieldln = strlen( def_path ) - 1.
  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
    CLEAR <tmp_sym>.
  ENDIF.

  CALL FUNCTION 'F4_FILENAME'
       EXPORTING
            PROGRAM_NAME  = SY-CPROG
            DYNPRO_NUMBER = SY-DYNNR
            FIELD_NAME    = ' '
       IMPORTING
            FILE_NAME     = tmp_filename.

* CALL FUNCTION 'WS_FILENAME_GET'
*       EXPORTING
*            def_filename     = p_file
*            def_path         = def_path
*           MASK             = ',*.*,*.*.'
*            mask             = tmp_mask
*            mode             = mode
*           TITLE            = ' '
*       IMPORTING
*            filename         = tmp_filename
*         RC               =
*       EXCEPTIONS
*            inv_winsys       = 1
*            no_batch         = 2
*            selection_cancel = 3
*            selection_error  = 4.
*
  IF sy-subrc = 0.
    p_file = tmp_filename.
  ELSE.
    MESSAGE e000 WITH 'FILE SELECT WINDOW OPEN ERROR!'.
  ENDIF.


ENDFORM.                    " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_process.
  CLEAR it_aala. REFRESH it_aala.
  CLEAR intern. REFRESH intern.
  WA_FILE = P_FILE.
  call function 'WS_UPLOAD'
     EXPORTING
       FILENAME  = P_FILE
       FILETYPE  = 'DAT'
     TABLES
       DATA_TAB  = IT_AALA1
     EXCEPTIONS
      CONVERSION_ERROR = 1
      FILE_OPEN_ERROR  = 2
      FILE_READ_ERROR  = 3
      INVALID_TYPE     = 4
      NO_BATCH         = 5
*      BAD_DATA_FORMAT  = 6
      OTHERS           = 9.
  IF SY-SUBRC NE 0.
    S_ERROR = 'X'.
    IF SY-SUBRC = 1.
      GT_MESSAGE-MSG = 'FILE UPLOAD CONVERSION ERROR'.
      APPEND GT_MESSAGE.
    ELSEIF SY-SUBRC = 2.
      GT_MESSAGE-MSG = 'UPLOAD FILE OPEN ERROR'.
      APPEND GT_MESSAGE.
    ELSEIF SY-SUBRC = 3.
      GT_MESSAGE-MSG = 'UPLOAD FILE READ ERROR'.
      APPEND GT_MESSAGE.
    ELSEIF SY-SUBRC = 4.
      GT_MESSAGE-MSG = 'INVALID DATA TYPE!'.
      APPEND GT_MESSAGE.

    ENDIF.
  ENDIF.

  DELETE IT_AALA1 INDEX 1.
  CLEAR: IT_AALA, IT_AALA[].
  LOOP AT IT_AALA1.
    MOVE-CORRESPONDING IT_AALA1 TO IT_AALA.
    APPEND IT_AALA.
  ENDLOOP.
**
*  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
*       EXPORTING
*            filename                = p_file
*            i_begin_col             = 1
*            i_begin_row             = 2
*            i_end_col               = 19
*            i_end_row               = 3000
*       TABLES
*            intern                  = intern
*       EXCEPTIONS
*            inconsistent_parameters = 1
*            upload_ole              = 2
*            OTHERS                  = 3.
**
*
*  LOOP AT intern.
*    CASE intern-col.
*      WHEN 1. it_aala-kokrs = intern-value.
*      WHEN 2. it_aala-matnr = intern-value.
*      WHEN 3. it_aala-versn = intern-value.
*      WHEN 4. it_aala-lifnr = intern-value.
*      WHEN 5. it_aala-klvar = intern-value.
*      WHEN 6. it_aala-kstar = intern-value.
**      WHEN 7. IT_AALA-KOSTL = INTERN-VALUE.
*      WHEN 7. it_aala-stprs = intern-value.
*      WHEN 8. it_aala-peinh = intern-value.
*      WHEN 9. it_aala-waers = intern-value.
*      WHEN 10. it_aala-netpr = intern-value.
*      WHEN 11. it_aala-aaprs = intern-value.
*      WHEN 12. it_aala-zwaers = intern-value.
*      WHEN 13. it_aala-urzla = intern-value.
*      WHEN 14. it_aala-ekgrp = intern-value.
*      WHEN 15. it_aala-mtart = intern-value.
*      WHEN 16. it_aala-datbi = intern-value.
*      WHEN 17. it_aala-datab = intern-value.
*      WHEN 18.
*        it_aala-zver_des = intern-value.
*
*        APPEND it_aala. CLEAR it_aala.
*    ENDCASE.
*  ENDLOOP.

ENDFORM.                    " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM  build_events.
  PERFORM  build_fieldcat.
  PERFORM  build_layout      USING  'X'   'X'   space.
  PERFORM  build_comment     USING  gt_header[].
* ALV FUNCTION CALL
  PERFORM start_grid_viewer.

ENDFORM.                    " display_data

*&---------------------------------------------------------------------*
*&      Form  build_events
*&---------------------------------------------------------------------*
*       Building Events For ALV
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_events.
  CONSTANTS : c_pss TYPE slis_formname VALUE 'PF_STATUS_SET',
              c_uc  TYPE slis_formname VALUE 'USER_COMMAND',
              c_top TYPE slis_formname VALUE 'TOP_OF_PAGE'.
  REFRESH gt_events.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = gt_events.

  PERFORM modify_gt_events
          TABLES  gt_events
          USING :
            slis_ev_pf_status_set c_pss,
            slis_ev_user_command  c_uc.
*            SLIS_EV_TOP_OF_PAGE   C_TOP.
ENDFORM.                    " build_events
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
  g_repid = sy-repid.
  PERFORM build_fieldcat_1  USING  'IT_AALA'.
ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       Building Layout For ALV
*----------------------------------------------------------------------*
*      -->P_0278   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM build_layout USING p_cb p_color p_sum.
  CLEAR gs_layout.

  gs_layout-zebra             = 'X'.
  gs_layout-cell_merge        = space.
  gs_layout-colwidth_optimize = ' '.
  gs_layout-default_item      = 'X'.
* check box
*  IF P_CB = 'X'.
*    GS_LAYOUT-BOX_FIELDNAME    = 'CHKBOX'.
*  ENDIF.
* line color
*  IF P_COLOR = 'X'.
*    GS_LAYOUT-COLTAB_FIELDNAME = 'COLOR'.
*  ENDIF.
* sum
*  IF P_SUM = 'X'.
*    GS_LAYOUT-TOTALS_TEXT       = 'TOT'.
*  ENDIF.
ENDFORM.                    " build_layout
*&---------------------------------------------------------------------*
*       Building Comments For ALV
*----------------------------------------------------------------------*
*      -->P_GT_HEADER[]  text
*----------------------------------------------------------------------*
FORM build_comment USING    p_gt_header TYPE slis_t_listheader.
  DATA: ls_line  TYPE slis_listheader,
        ls_color TYPE slis_specialcol_alv,
        l_date(50).
  DATA: l_text(70) TYPE c.
  DATA: i_lines(5).
  DATA: i_count(5).

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-info = 'Confirmed AALA Parts Prices'.
  APPEND ls_line TO p_gt_header.

ENDFORM.                    " build_comment
*&---------------------------------------------------------------------*
*&      Form  start_grid_viewer
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_grid_viewer.

  PERFORM  start_grid_viewer_1 TABLES  it_aala.

ENDFORM.                    " start_grid_viewer

*&---------------------------------------------------------------------*
*&      Form  start_grid_viewer
*&---------------------------------------------------------------------*
*       Running GRID Viewer
*----------------------------------------------------------------------*
*      -->P_IT_DISPLAY  text
*----------------------------------------------------------------------*
FORM start_grid_viewer_1 TABLES p_intab.

*** print paramter   ****************************************
  gs_print-no_coverpage = 'X'.
  gs_print-no_print_listinfos = 'X'.
  gs_print-no_change_print_params = 'X'.
  gs_print-no_print_selinfos = 'X'.
*************************************************************

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer       = 'X'
*            i_background_id          = 'ALV_BACKGROUND'
            i_callback_program       = g_repid
            i_callback_pf_status_set = 'SET_STATUS'
*            I_CALLBACK_TOP_OF_PAGE   = 'TOP_OF_PAGE'
           i_callback_user_command  = 'USER_COMMAND'
*            is_layout                = gs_layout
            it_fieldcat              = gt_fieldcat[]
*            IT_SORT                  = GT_SORT[]
            i_save                   = 'A'
*            IS_VARIANT               = G_VARIANT
            it_events                = gt_events[]
            is_print                 = gs_print
*            IT_LIST_COMMENTARY       = GT_HEADER
       IMPORTING
            e_exit_caused_by_caller  = g_exit_caused_by_caller
            es_exit_caused_by_user   = gs_exit_caused_by_user
       TABLES
            t_outtab                 = p_intab.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " start_grid_viewer
*&---------------------------------------------------------------------*
*&      Form  modify_gt_events
*&---------------------------------------------------------------------*
*       Modification of Events For ALV
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*      -->P_SLIS_EV_PF_STATUS_SET  text
*      -->P_C_PSS  text
*----------------------------------------------------------------------*
FORM modify_gt_events TABLES p_events_t LIKE gt_events
                      USING  p_form p_value.

  DATA: ls_event TYPE slis_alv_event.

  READ TABLE  p_events_t  WITH KEY  name = p_form
                          INTO ls_event.
  IF sy-subrc EQ 0.
    MOVE     p_value     TO   ls_event-form.
    MODIFY   p_events_t  FROM ls_event INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " modify_gt_events
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       Building Field Categories For ALV
*----------------------------------------------------------------------*
*      -->P_0278   text
*----------------------------------------------------------------------*
FORM build_fieldcat_1 USING p_intab TYPE slis_tabname.
  DATA: l_pre_day         TYPE d ,
        l_pre(10) ,
        l_date(10).
  DATA: i TYPE i.
  DATA: gs_fieldcat LIKE LINE OF gt_fieldcat.
  CLEAR   : gt_fieldcat, gt_fc.
  REFRESH : gt_fieldcat, gt_fc.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = g_repid
            i_internal_tabname = p_intab
*           I_STRUCTURE_NAME   = 'ZTCO_AALA_PARTS_SOURCE'
            i_inclname         = g_repid
       CHANGING
            ct_fieldcat        = gt_fc
       EXCEPTIONS
        inconsistent_interface = 1
        program_error = 2
        OTHERS = 3.

  gt_fieldcat[] = gt_fc[].
*  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.
*     IF GS_FIELDCAT-FIELDNAME <> 'MATNR'.
*        CLEAR GS_FIELDCAT-KEY .
*     ENDIF.
*     IF GS_FIELDCAT-FIELDNAME = 'MATNR'.
*       GS_FIELDCAT-OUTPUTLEN = 22.
*     ELSEIF GS_FIELDCAT-FIELDNAME = 'AUFNR'.
*       GS_FIELDCAT-SELTEXT_L = 'PCC Number'.
*     ELSEIF GS_FIELDCAT-FIELDNAME = 'PLNNG'.
*       GS_FIELDCAT-SELTEXT_L = 'Routing'.
*     ENDIF.
*     MODIFY GT_FIELDCAT FROM GS_FIELDCAT.
*  ENDLOOP.
ENDFORM.

*-----------------------------------------------------------*
*   PF_STATUS_SET
*-----------------------------------------------------------*
FORM pf_status_set USING rt_extab TYPE slis_t_extab.

* RT_EXTAB CONTAIN THE FUNCTION CODES WHCH ARE HIDDEN
* IN THE STANDARD INTERFACE
  SET PF-STATUS 'MYSTATUS' EXCLUDING rt_extab.


ENDFORM.

*-----------------------------------------------------------*
*   USER_COMMAND
*-----------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
* R_UCOMM CONTAIN THE FUNCTION CODE
* R_UCOMM HAS DETAIL ABOUT THE CURRENT DETAIL CURSOR POSTION

  IF r_ucomm = 'SAVE'.
    PERFORM update_database.
    if S_ERROR IS INITIAL.
      MESSAGE i000 WITH 'DATA HAS BEEN SAVED!'.
    ELSE.
      MESSAGE I100 WITH 'DATA UPDATE FAILED.'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DATABASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_database.

  PERFORM UPDATE_ZTCO_AALA_SOURCE.

ENDFORM.                    " UPDATE_DATABASE
*&---------------------------------------------------------------------*
*&      Form  DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_check.

  DATA: wa_aala LIKE it_aala.

  CLEAR: S_ERROR, S_NO_MATCH.
* CHCEK THE FILE DATA KEY
  READ TABLE it_aala INTO wa_aala INDEX 1.
  LOOP AT it_aala.
    IF it_aala-kokrs NE wa_aala-kokrs OR
       it_aala-gjahr ne wa_aala-gjahr or
       it_aala-versn NE wa_aala-versn.
      s_error = 'X'.
      gt_message-msg = 'CHECK CTRL AREA OR VERSION'.
      APPEND gt_message.
      MESSAGE I000 WITH 'Ctrl area,year and version can not be changed'
.
      EXIT.
    ENDIF.
  ENDLOOP.

* COMPARE THE FILE DATA AND DATABASE DATA
* USER CAN ONLY CHANGE THE PRICE
* READING DATABASE DATA
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_aala
    FROM ztco_aala_source
    FOR ALL ENTRIES IN it_aala
    WHERE matnr = it_aala-matnr AND
          kokrs = it_aala-kokrs AND
          GJAHR = it_aala-GJAHR AND
          versn = it_aala-versn.
  IF sy-subrc NE 0.
    s_error = 'X'.
    gt_message-msg = 'NO CORRESPONDING DATA IN DATABASE'.
    APPEND gt_message.
  ENDIF.

* CHECK OTHER FIELDS.
  data: w_aala like it_aala,
        l_idx  like sy-tabix.
  LOOP AT it_aala into w_aala.
    l_idx = sy-tabix.
    READ TABLE lt_aala WITH KEY kokrs = w_aala-kokrs
                                matnr = w_aala-matnr
                                versn = w_aala-versn.

    IF sy-subrc EQ 0.
      move-corresponding lt_aala to it_aala.

      if p_std = 'X'.
        IT_AALA-STPRS = w_AALA-STPRS.
      endif.

      it_aala-netpr = w_aala-netpr.
      it_aala-aaprs = w_aala-aaprs.
      it_aala-urzla = w_aala-urzla.
      modify it_aala index l_idx.
    ENDIF.
  ENDLOOP.
*  IF S_NO_MATCH = 'X'.
*   GT_MESSAGE-MSG =
*   'Only gross price,AALA price and country can be changed'.
*   APPEND gt_message.
*  ENDIF.
ENDFORM.                    " DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  WRITE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_message.
  DATA: ODD.
  IF NOT s_error IS INITIAL OR
     NOT s_NO_MATCH IS INITIAL.
    WRITE: / 'THE UPLOADED DATA HAS SOME ERROR',
            'PLEASE CHECK AND CORRECT IT.'.
    WRITE: /(100) SY-ULINE.
    LOOP AT gt_message.
      WRITE: / gt_message-msg.
    ENDLOOP.
    WRITE: /(100) SY-ULINE.
    ODD = 'X'.
    LOOP at IT_ERROR_RECORD.
      WRITE:

         /(4) it_error_record-kokrs,
         5(18)  it_error_record-matnr,
         24(3) it_error_record-versn,
         28(10) it_error_record-lifnr,
         39(4) it_error_record-klvar,
         44(10) it_error_record-kstar,
         56(11) it_error_record-stprs,
         62(5) it_error_record-peinh,
         68(5) it_error_record-waers,
         79(11) it_error_record-netpr,
         91(11) it_error_record-aaprs,
         97(5) it_error_record-zwaers,
         101(3) it_error_record-urzla,
         105(3) it_error_record-ekgrp,
         110(4) it_error_record-mtart.
      IF ODD = 'X'.
        CLEAR ODD.
      ELSE.
        SKIP.
        ODD = 'X'.
      ENDIF.
    endloop.
  ENDIF..
ENDFORM.                    " WRITE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTCO_AALA_SOURCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form UPDATE_ZTCO_AALA_SOURCE.
  DATA: LT_SOURCE LIKE ZTCO_AALA_SOURCE OCCURS 0 WITH HEADER LINE.
  DATA: L_TEXT(50).
  CLEAR S_ERROR.
  LOOP AT LT_AALA.
    READ TABLE It_aala WITH KEY kokrs = Lt_aala-kokrs
                                matnr = Lt_aala-matnr
                                gjahr = lt_aala-gjahr
                                versn = Lt_aala-versn.
    IF SY-SUBRC EQ 0.
      if p_std = 'X'.
        LT_AALA-STPRS = IT_AALA-STPRS.
      endif.

      PERFORM ALPHA_CONVERSION USING IT_AALA-KSTAR .
      LT_AALA-KSTAR = IT_AALA-KSTAR.
      LT_AALA-NETPR = IT_AALA-NETPR.
      LT_AALA-AAPRS = IT_AALA-AAPRS.
      LT_AALA-URZLA = IT_AALA-URZLA.
      LT_AALA-AENAM = SY-UNAME.
      LT_AALA-AEDAT = SY-DATUM.
      LT_AALA-AEZET = SY-UZEIT.
      LT_AALA-VFLAG = 'X'.
      MODIFY LT_AALA.
    ENDIF.
  ENDLOOP.
  UPDATE ZTCO_AALA_SOURCE FROM TABLE LT_AALA.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.

  ELSE.
    S_ERROR = 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.


endform.                    " UPDATE_ZTCO_AALA_SOURCE
*&---------------------------------------------------------------------*
*&      Form  ALPHA_CONVERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_AALA_KSTAR  text
*----------------------------------------------------------------------*
form ALPHA_CONVERSION using    p_kstar.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = P_KSTAR
       IMPORTING
            OUTPUT = P_KSTAR.
endform.                    " ALPHA_CONVERSION
