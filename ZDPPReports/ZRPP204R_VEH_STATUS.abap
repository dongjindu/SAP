************************************************************************
* Program Name      : ZRPP204R_VEH_STATUS
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2004.02.10.
* Specifications By : MH Moon
* Pattern           : 2.1
* Development Request No : UD1K907187
* Addl Documentation:
* Description       : Long Term's Sequenced Plan Per Code(ALC).
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  ZRPP204R_VEH_STATUS  NO STANDARD PAGE HEADING MESSAGE-ID zmpp.

TABLES: ZTPP_COMMON_VALS, "[PP] COMMON Information Table
        ztpp_INPUT_PLAN,  " Vehicle Status Table
        ztpp_status.

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
      gt_colinfo_table TYPE slis_t_specialcol_alv. "line color.

* FIELD SYMBOLS
FIELD-SYMBOLS: <fs> TYPE ANY.

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

****************************************************
* Definition of Internal Tables
****************************************************
DATA: it_alc_long TYPE TABLE OF  ztpp_input_plan       WITH HEADER LINE.
DATA: it_data     TYPE TABLE OF ZTPP_INPUT_PLAN        WITH HEADER LINE.

****************************************************
* Work-Area Variables Definition
****************************************************
DATA: alv_grid               TYPE REF TO cl_gui_alv_grid,
      gs_custom_container    TYPE REF TO cl_gui_custom_container,
      WA_DATE                TYPE D,
      wa_container           TYPE scrfname VALUE 'CONTAINER'.

*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
CONSTANTS: C_JOBS(40)              VALUE 'ZAPP903R_INPUT_PLAN',
           C_KEY2(18)              VALUE 'INPUT_PLAN'.


****************************************************
* Selection-Screen
****************************************************
SELECTION-SCREEN  BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
            S_MODL    FOR  ZTPP_INPUT_PLAN-MODL,
            S_BSER    FOR  ZTPP_INPUT_PLAN-BODY_SER,
            S_SER     FOR  ZTPP_INPUT_PLAN-SERIAL,
            S_STATUS  FOR  ZTPP_INPUT_PLAN-STATUS,
            S_WORDER  FOR  ZTPP_INPUT_PLAN-WORK_ORDER,
            S_EXTC    FOR  ZTPP_INPUT_PLAN-EXTC      ,
            S_INTC    FOR  ZTPP_INPUT_PLAN-INTC      ,
            S_MI      FOR  ZTPP_INPUT_PLAN-MI        ,
            S_OCNN    FOR  ZTPP_INPUT_PLAN-OCNN      ,
            S_VERS    FOR  ZTPP_INPUT_PLAN-VERS      ,
            S_RSNUM   FOR  ZTPP_INPUT_PLAN-RSNUM     ,
            S_PLNUM   FOR  ZTPP_INPUT_PLAN-PLNUM     ,
            S_RP01    FOR  ZTPP_INPUT_PLAN-RP01,
            S_RP02    FOR  ZTPP_INPUT_PLAN-RP02,
            S_RP03    FOR  ZTPP_INPUT_PLAN-RP03,
            S_RP04    FOR  ZTPP_INPUT_PLAN-RP04,
            S_RP05    FOR  ZTPP_INPUT_PLAN-RP05,
            S_RP06    FOR  ZTPP_INPUT_PLAN-RP06,
            S_RP07    FOR  ZTPP_INPUT_PLAN-RP07,
            S_RP08    FOR  ZTPP_INPUT_PLAN-RP08,
            S_RP09    FOR  ZTPP_INPUT_PLAN-RP09,
            S_RP10    FOR  ZTPP_INPUT_PLAN-RP10,
            S_RP11    FOR  ZTPP_INPUT_PLAN-RP11,
            S_RP12    FOR  ZTPP_INPUT_PLAN-RP12,
            S_RP13    FOR  ZTPP_INPUT_PLAN-RP13,
            S_RP14    FOR  ZTPP_INPUT_PLAN-RP14,
            S_RP15    FOR  ZTPP_INPUT_PLAN-RP15,
            S_RP16    FOR  ZTPP_INPUT_PLAN-RP16,
            S_RP17    FOR  ZTPP_INPUT_PLAN-RP17,
            S_RP18    FOR  ZTPP_INPUT_PLAN-RP18.
SELECTION-SCREEN  END OF BLOCK blk1.

***************************************************
INITIALIZATION.
***************************************************
  g_repid = sy-repid.
  wa_date = sy-datum.

*************************************************
*AT SELECTION-SCREEN ON p_rp.
*************************************************


*************************************************
START-OF-SELECTION.
*************************************************
  PERFORM initial_data.
  PERFORM make_data_for_display.

*************************************************
END-OF-SELECTION.
*************************************************
  PERFORM  build_events.
  PERFORM  build_fieldcat    USING  'ZTPP_INPUT_PLAN'  'IT_DATA'.
  PERFORM  build_layout      USING  space space   space.
  PERFORM  build_comment     USING  gt_header[].
* ALV FUNCTION CALL
  PERFORM  start_grid_viewer using  'ZTPP_INPUT_PLAN' .

*************************************************
TOP-OF-PAGE.
*************************************************
  PERFORM top_of_page.


*&---------------------------------------------------------------------*
*&      Form  initial_data
*&---------------------------------------------------------------------*
*       Searching Raw Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial_data.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_alc_long
    FROM ztpp_INPUT_PLAN
    WHERE MODL       IN S_MODL   AND
          BODY_SER   IN S_BSER   AND
          SERIAL     IN S_SER    AND
          STATUS     IN S_STATUS AND
          WORK_ORDER IN S_WORDER AND
          EXTC       IN S_EXTC   AND
          INTC       IN S_INTC   AND
          PLNUM      IN S_PLNUM  AND
          MI         IN S_MI     AND
          OCNN       IN S_OCNN   AND
          VERS       IN S_VERS   AND
          RSNUM      IN S_RSNUM  AND
          RP01       IN S_RP01   AND
          RP02       IN S_RP02   AND
          RP03       IN S_RP03   AND
          RP04       IN S_RP04   AND
          RP05       IN S_RP05   AND
          RP06       IN S_RP06   AND
          RP07       IN S_RP07   AND
          RP08       IN S_RP08   AND
          RP09       IN S_RP09   AND
          RP10       IN S_RP10   AND
          RP11       IN S_RP11   AND
          RP12       IN S_RP12   AND
          RP13       IN S_RP13   AND
          RP14       IN S_RP14   AND
          RP15       IN S_RP15   AND
          RP16       IN S_RP16   AND
          RP17       IN S_RP17   AND
          RP18       IN S_RP18   .
ENDFORM.                    " initial_data

*&---------------------------------------------------------------------*
*&      Form  make_data_for_display
*&---------------------------------------------------------------------*
*       Modification of Data For Display
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_data_for_display.
  SORT it_alc_long BY SERIAL .
  IT_DATA[] = IT_alc_long[]  .
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
FORM build_fieldcat USING P_STRUCTURE p_intab.
  CLEAR   : gt_fieldcat, gt_fc.
  REFRESH : gt_fieldcat, gt_fc.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
*           i_program_name     = g_repid
            I_STRUCTURE_NAME   = 'ZTPP_INPUT_PLAN'  " p_STRUCTURE
*           i_internal_tabname = p_intab
*           i_inclname         = g_repid
       CHANGING
            ct_fieldcat        = gt_fc  .  " gt_fieldcat .

  gt_fieldcat[] = gt_fc[].

  SELECT SINGLE VARIANT INTO g_variant
    FROM V_LTDX
   WHERE RELID   = 'LT'
     AND REPORT  = SY-REPID
     AND ERFNAME = SY-UNAME
     AND TYPE    = 'F'      .
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
*        L_MANAGER(50),
        l_date(50),
        l_list(50),
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_time(8),
*        l_succ(5) TYPE i,
        l_ldate(10),
        l_hdate(10).

* Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-001.
  APPEND ls_line TO p_gt_header.

* User
  ls_line-typ  = 'A'.
  ls_line-key  = 'User: '.
  ls_line-info = sy-uname.
  APPEND ls_line TO p_gt_header.

* Generation Date
  ls_line-typ  = 'A'.
  ls_line-key  = 'Genera: '.
  select single *
    from ZTPP_COMMON_VALS
   WHERE JOBS = C_JOBS
     AND KEY2 = C_KEY2 .

  WRITE  ZTPP_COMMON_VALS-datES TO ls_line-info.
  WRITE  ZTPP_COMMON_VALS-TIMES TO l_time      .
  CONCATENATE  'Generation Date: '  ls_line-info  l_time
         INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.

* today
  ls_line-typ  = 'A'.
  ls_line-key  = 'Today : '.
  WRITE  sy-datum TO ls_line-info.
  WRITE  sy-uzeit TO l_time      .
  CONCATENATE  'Current Date : '  ls_line-info  l_time
         INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.
ENDFORM.                    " build_comment

*&---------------------------------------------------------------------*
*&      Form  start_grid_viewer
*&---------------------------------------------------------------------*
*       Running GRID Viewer
*----------------------------------------------------------------------*
*      -->P_IT_DATA  text
*----------------------------------------------------------------------*
FORM start_grid_viewer using  p_table .
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
            I_STRUCTURE_NAME         = p_table
            it_fieldcat              = gt_fieldcat[]
            i_callback_top_of_page   = 'TOP_OF_PAGE'
*            i_callback_pf_status_set = 'SET_STATUS'
*            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
            i_save                   = 'A'
            is_variant               = g_variant
            it_events                = gt_events[]
            is_print                 = gs_print
            it_list_commentary       = gt_header
       IMPORTING
            e_exit_caused_by_caller  = g_exit_caused_by_caller
            es_exit_caused_by_user   = gs_exit_caused_by_user
       TABLES
            t_outtab                 = it_data .

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
*&      Form  call_workday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DATE  text
*----------------------------------------------------------------------*
FORM call_workday USING    pa_date.
  DATA: l_ident             LIKE t001w-fabkl.

  SELECT SINGLE fabkl  INTO l_ident
    FROM t001w
   WHERE werks = 'P001'   .

  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            correct_option               = '+'
            date                         = pa_date
            factory_calendar_id          = l_ident
       IMPORTING
            date                         = pa_date
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
ENDFORM.                    " call_workday
