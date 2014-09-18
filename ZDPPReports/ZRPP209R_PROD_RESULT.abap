************************************************************************
* Program Name      : ZRPP209R_PROD_RESULT
* Author            : BOBBY
* Creation Date     : 2004.02.10.
* Specifications By : MH Moon
* Pattern           : 2.1
* Development Request No : UD1K907187
* Addl Documentation:
* Description       : Monthly Production Result
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  ZRPP209R_PROD_RESULT          NO STANDARD PAGE HEADING
                                      MESSAGE-ID zmpp.

TABLES: ztpp_common_vals, "[PP] COMMON Information Table
        ztpp_ALC_PROD   . "ALC Code Summary - Monthly Prod. Result

INCLUDE <icon>.
INCLUDE <list>.
*****// ALV //**********************************************************
TYPE-POOLS: slis.
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
       gs_keyinfo             TYPE slis_keyinfo_alv.   "relation key

* return
DATA : g_exit_caused_by_caller  TYPE c,
       gs_exit_caused_by_user   TYPE slis_exit_by_user.

DATA: col_pos TYPE i,
      cnt     TYPE i,
      g_save  TYPE c,
      g_repid LIKE sy-repid,
      g_variant LIKE disvariant.
DATA: gt_extab TYPE slis_t_extab WITH HEADER LINE.

DATA: BEGIN OF it_data OCCURS 0.
DATA:   CHKBOX.
        INCLUDE STRUCTURE ztpp_alc_PROD   .
DATA: END OF it_data.

****************************************************
* Work-Area Variables Definition
****************************************************
DATA: alv_grid               TYPE REF TO cl_gui_alv_grid,
      gs_custom_container    TYPE REF TO cl_gui_custom_container,
      WA_DATE(6)             TYPE N                             ,
      wa_kalid               LIKE kako-kalid                    ,
      wa_container           TYPE scrfname VALUE 'CONTAINER'.

*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
DATA: C_JOBS(40)              VALUE 'ZAPP903R_INPUT_PLAN',
      C_KEY1(18)              VALUE 'SEQ_PROD'  .

****************************************************
* Selection-Screen
****************************************************
*SELECTION-SCREEN  BEGIN OF BLOCK blk1 WITH FRAME TITLE text-002.
*PARAMETERS: p_date  TYPE d   OBLIGATORY  DEFAULT sy-datum,
*            p_model TYPE zmodel                          .
*SELECTION-SCREEN  END OF BLOCK blk1.

***************************************************
INITIALIZATION.
***************************************************
  DATA: l_vals                LIKE ztpp_common_vals-item4.

  " Get the Date for the Production Reporting Date(Last Date)
  PERFORM get_start_day     USING L_VALS      .
  WA_DATE = L_VALS  .
  G_REPID = SY-REPID.

*************************************************
*AT SELECTION-SCREEN ON p_rp.
*************************************************


*************************************************
START-OF-SELECTION.
*************************************************
  PERFORM make_data_for_display.

*************************************************
END-OF-SELECTION.
*************************************************
  PERFORM  build_events.
  PERFORM  build_fieldcat    USING  'IT_DATA'.
  PERFORM  build_layout      USING  'X'   space   space.
  PERFORM  build_comment     USING  gt_header[].
* ALV FUNCTION CALL
  PERFORM  start_grid_viewer TABLES  it_data.

*************************************************
TOP-OF-PAGE.
*************************************************
  PERFORM top_of_page.

*&---------------------------------------------------------------------*
*&      Form  make_data_for_display
*&---------------------------------------------------------------------*
*       Modification of Data For Display
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_data_for_display.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_alc_PROD  .
    sort it_data by SERIAL.
ENDFORM.                    " make_data_for_display

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
*            slis_ev_pf_status_set c_pss,
*            slis_ev_user_command  c_uc,
            slis_ev_top_of_page   c_top.
ENDFORM.                    " build_events

*&---------------------------------------------------------------------*
*&      Form  modify_gt_events
*&---------------------------------------------------------------------*
*       Modification of Events For ALV
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_C_TOP  text
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
*      -->P_0441   text
*----------------------------------------------------------------------*
FORM build_fieldcat USING p_intab.
  CLEAR   : gt_fieldcat, gt_fc.
  REFRESH : gt_fieldcat, gt_fc.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = g_repid
            i_internal_tabname = p_intab
            i_inclname         = g_repid
       CHANGING
            ct_fieldcat        = gt_fc.

  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                  'S' 'KEY_CODE'  ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '30',
                                  'E' 'SELTEXT_L'   'Description',
*
*                                  'S' 'ALC_VALS'    ' ',
*                                  ' ' 'JUST'        'C',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '08',
*                                  'E' 'SELTEXT_L'   'ALC_VALS',
*
*                                  'S' 'MODEL'       ' ',
*                                  ' ' 'JUST'        'C',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   'MODEL' ,
*
*                                  'S' 'BODY_SER'    ' ',
*                                  ' ' 'JUST'        'C',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '08',
*                                  'E' 'SELTEXT_L'   'BODY_SER',

                                  'S' 'M_SOFF'      ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'M-S/OFF'   ,

                                  'S' 'M_TRIM'      ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'M-TRIM' ,

                                  'S' 'M_POUT'      ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'M-P.OUT'  ,

                                  'S' 'M_PIN'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'M-P.IN'  ,

                                  'S' 'M_BIN'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'M-BODY' ,

                                  'S' 'D_SOFF'      ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'D-S/OFF'   ,

                                  'S' 'D_TRIM'      ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'D-TRIM' ,

                                  'S' 'D_POUT'      ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'D-P.OUT'  ,

                                  'S' 'D_PIN'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'D-P.IN'  ,

                                  'S' 'D_BIN'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'D-BODY' ,

                                  'S' 'W_SOFF'      ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'D.S/OFF'   ,

                                  'S' 'W_TRIM'      ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'D.TRIM' ,

                                  'S' 'W_POUT'      ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'D.P.OUT'  ,

                                  'S' 'W_PIN'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'D.P.IN'  ,

                                  'S' 'W_BIN'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'D.BODY' ,

                                  'S' 'C_BIN'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   ' BODY  ' ,

                                  'S' 'C_PAINT'     ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   ' PAINT ' ,

                                  'S' 'C_TRIM'      ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   ' TRIM  ' ,

                                  'S' 'C_TOT'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   ' TOTAL '   ,

                                  'S' 'C_SEQ'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   ' SEQ  ' ,

                                  'S' 'P_SEQ'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   ' PLAN  ' ,

                                  'S' 'P_MITU'      ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   ' MITU  ' ,

                                  'S' 'P_TOT'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   ' TOTAL ' .
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       Setting Field Category For ALV
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_1259   text
*      -->P_1260   text
*      -->P_1261   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat LIKE gt_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.

  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR : g_fieldcat_s.
    READ TABLE gt_fc INTO g_fieldcat_s
                     WITH KEY fieldname  = p_field.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'G_FIELDCAT_S-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO cnt.
    g_fieldcat_s-col_pos = cnt.
    APPEND g_fieldcat_s TO p_fieldcat.
  ENDIF.

ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       Building Layout For ALV
*----------------------------------------------------------------------*
*      -->P_0445   text
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
  IF p_cb = 'X'.
    gs_layout-box_fieldname    = 'CHKBOX'.
  ENDIF.
* line color
  IF p_color = 'X'.
    gs_layout-coltab_fieldname = 'COLOR'.
  ENDIF.
* sum
  IF p_sum = 'X'.
    gs_layout-totals_text       = 'TOT'.
  ENDIF.
ENDFORM.                    " build_layout

*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       Building Comments For ALV
*----------------------------------------------------------------------*
*      -->P_GT_HEADER[]  text
*----------------------------------------------------------------------*
FORM build_comment USING    p_gt_header TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        ls_color type slis_specialcol_alv,
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_time(8),
        l_date(10),
        l_hdate(10).

* Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-001.
  APPEND ls_line TO p_gt_header.

* Sub-title of HEADER
  ls_line-typ  = 'S'.
  CONCATENATE WA_DATE+4(2) '/' WA_DATE(4) INTO L_DATE.
  CONCATENATE TEXT-030 L_DATE TEXT-040
         INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.

* Head-Line 1 Format
  ls_line-typ  = 'A'.
  ls_line-key  = 'HMMA: '.
  WRITE  SY-DATUM  TO  L_DATE   MM/DD/YYYY .
  CONCATENATE TEXT-010 L_DATE   INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.
ENDFORM.                    " build_comment

*&---------------------------------------------------------------------*
*&      Form  start_grid_viewer
*&---------------------------------------------------------------------*
*       Running GRID Viewer
*----------------------------------------------------------------------*
*      -->P_IT_DATA  text
*----------------------------------------------------------------------*
FORM start_grid_viewer TABLES p_intab.

*** print paramter   ****************************************
  gs_print-no_coverpage = 'X'.
  gs_print-no_print_listinfos = 'X'.
  gs_print-no_change_print_params = 'X'.
  gs_print-no_print_selinfos = 'X'.
*************************************************************

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
*            i_background_id          = 'ALV_BACKGROUND' "HEADER? ??
*            i_bypassing_buffer       = 'X'
            i_callback_program       = g_repid
            is_layout                = gs_layout
            it_fieldcat              = gt_fieldcat[]
            i_callback_top_of_page   = 'TOP_OF_PAGE'
*            i_callback_pf_status_set = 'SET_STATUS'
*            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*            IT_SORT                  = GT_SORT[]
            i_save                   = 'A'
            is_variant               = g_variant
            it_events                = gt_events[]
            is_print                 = gs_print
            it_list_commentary       = gt_header
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
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'Z_HYUNDAI_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
            it_list_commentary = gt_header.
ENDFORM.                    " TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  GET_START_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM get_start_day USING    pa_datum.
  CLEAR: ztpp_common_vals.
  SELECT SINGLE *
    FROM ztpp_common_vals
   WHERE jobs = c_jobs
     AND key2 = 'CLOSE' .

  pa_datum = ztpp_common_vals-ITEM4.
ENDFORM.                    " GET_START_DAY
