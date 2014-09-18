************************************************************************
* Program Name      : ZRPP208R_TRIM_INPUT_PLAN
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2003.05.10.
* Specifications By : B. Choi
* Pattern           : 2.1
* Development Request No : UD1K910227
* Addl Documentation: YRPP208R_TRIM_INPUT_PLAN_OLD .
* Description       : Trim Input Plan
*
* Modification Logs
* Date       Developer    RequestNo    Description
*#1 03/22/2005 wskim        UD1K914858   add weekly data
* 08/03/2005   chris      UD1K917193   sort table it_7jb_date internal
*                                      table
************************************************************************
REPORT  zrpp208r_trim_input_plan  MESSAGE-ID zmpp .

TABLES: ztpp_common_vals, "[PP] COMMON Information Table
        ztpp_alc_tinput.  "[PP] Trim Input Plan Table

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
        INCLUDE STRUCTURE ztpp_alc_tinput .
DATA: END OF it_data.
DATA : it_7jb_date LIKE ztpp_pmt07jb_a OCCURS 0 WITH HEADER LINE,
       it_7jb_a LIKE it_7jb_date OCCURS 0 WITH HEADER LINE.

DATA : z_max_date LIKE sy-datum.

DATA : alv_grid               TYPE REF TO cl_gui_alv_grid,
       gs_custom_container    TYPE REF TO cl_gui_custom_container,
       wa_datum               TYPE d,
       wa_kalid               LIKE kako-kalid                ,
       wa_container           TYPE scrfname VALUE 'CONTAINER'.

*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
DATA: c_jobs(40)              VALUE 'ZAPP903R_INPUT_PLAN',
      c_key1(18)              VALUE 'TRIM_INPUT'.

************************************************************************
* SELECTION-SCREEN DEFINITION
************************************************************************
*SELECTION-SCREEN  BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
*PARAMETERS: p_flag  AS CHECKBOX  DEFAULT 'X'.
*SELECTION-SCREEN  END OF BLOCK blk1.

************************************************************************
INITIALIZATION.
************************************************************************
  g_repid     = sy-repid.
  wa_datum    = sy-datum.

************************************************************************
AT SELECTION-SCREEN.
************************************************************************


************************************************************************
START-OF-SELECTION.
************************************************************************
  PERFORM make_display_data.

************************************************************************
END-OF-SELECTION.
************************************************************************
  PERFORM  build_events.
  PERFORM  build_fieldcat    USING  'IT_DATA'.
  PERFORM  build_layout      USING  'X'   space   space.
  PERFORM  build_comment     USING  gt_header[].
* ALV FUNCTION CALL
  PERFORM  start_grid_viewer TABLES  it_data.
*
***********************************************************************
TOP-OF-PAGE.
***********************************************************************
  PERFORM top_of_page.

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
FORM build_fieldcat USING p_intab.
  DATA: l_pre_day         TYPE d ,
        l_pre(10) ,
        l_date(10).
  DATA  :7jb_date LIKE sy-datum,
         w_field(3),
         w_num(2) TYPE n.

  CLEAR   : gt_fieldcat, gt_fc.
  REFRESH : gt_fieldcat, gt_fc.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
      i_internal_tabname = p_intab
      i_inclname         = g_repid
    CHANGING
      ct_fieldcat        = gt_fc.

* IT_DISPLAY
  PERFORM read_start_day    USING wa_datum  .
  PERFORM read_shop_calid   USING wa_kalid  .
  l_pre_day = wa_datum - 1  .
  PERFORM read_working_date USING '-'  wa_kalid  l_pre_day.
  WRITE l_pre_day TO l_pre  DD/MM/YYYY .

  wa_datum = wa_datum + 1.
  PERFORM read_working_date USING '+'  wa_kalid  wa_datum .
  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                  'S' 'KEY_CODE'  ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '30',
                                  'E' 'SELTEXT_L'   'Description',

                                  'S' 'D_1'     ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '5',
                                  'E' 'SELTEXT_L'   l_pre(5) ,

                                  'S' 'D01'    ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '08',
                                  'E' 'SELTEXT_L'   'Shift 1',

                                  'S' 'D02'    ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '08',
                                  'E' 'SELTEXT_L'   'Shift 2',

** Furong on 06/08/12 for 3 shift
                                  'S' 'D03'    ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '08',
                                  'E' 'SELTEXT_L'   'Shift 3'.

** End
*---start#1 wskim 03/22/2005
  CLEAR w_num.
** Furong on 06/08/12 for 3 shift
*  w_num = 3.
  w_num = 4.
** End
  DO.
    CLEAR w_field.
    IF  wa_datum > z_max_date.
      EXIT.
    ENDIF.
    CONCATENATE 'D' w_num INTO w_field.
    WRITE wa_datum TO l_date  DD/MM/YYYY .
    wa_datum = wa_datum + 1.
    PERFORM read_working_date USING '+'  wa_kalid  wa_datum .
    PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                    'S' w_field       ' ',
                                    ' ' 'JUST'        'R',
                                    ' ' 'KEY'         ' ',
                                    ' ' 'DDICTXT'     'L',
                                    ' ' 'OUTPUTLEN'   '5',
                                    'E' 'SELTEXT_L'   l_date(5).
    w_num = w_num + 1.
  ENDDO.
  PERFORM setting_fieldcat TABLES gt_fieldcat USING :

                                    'S' 'REM'    ' ',
                                    ' ' 'JUST'        'R',
                                    ' ' 'KEY'         ' ',
                                    ' ' 'DDICTXT'     'L',
                                    ' ' 'OUTPUTLEN'   '06',
                                    'E' 'SELTEXT_L'   'REM'.
*---Start#1 wskim 03/22/2005
  PERFORM get_date_7jb_a.
  CLEAR w_num.
  LOOP AT it_7jb_date.
    CLEAR w_field.
    w_num = w_num + 1.
    WRITE it_7jb_date-sqdt TO l_date  DD/MM/YYYY .
    CONCATENATE 'W' w_num INTO w_field.
    PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                    'S' w_field       ' ',
                                    ' ' 'JUST'        'R',
                                    ' ' 'KEY'         ' ',
                                    ' ' 'DDICTXT'     'L',
                                    ' ' 'OUTPUTLEN'   '5',
                                    'E' 'SELTEXT_L'   l_date(5).
  ENDLOOP.
*---end

  PERFORM setting_fieldcat TABLES gt_fieldcat USING :

                                    'S' 'TOT'     ' ',
                                    ' ' 'JUST'        'R',
                                    ' ' 'KEY'         ' ',
                                    ' ' 'DDICTXT'     'L',
                                    ' ' 'OUTPUTLEN'   '06',
                                    'E' 'SELTEXT_L'   'Total'.

ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       Setting Field Categories For ALV
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_0386   text
*      -->P_0387   text
*      -->P_0388   text
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
*&      Form  start_grid_viewer
*&---------------------------------------------------------------------*
*       Running GRID Viewer
*----------------------------------------------------------------------*
*      -->P_IT_DISPLAY  text
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
      i_bypassing_buffer       = 'X'
*     i_background_id          = 'ALV_BACKGROUND' "HEADER? ??
      i_callback_program       = g_repid
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_top_of_page   = 'TOP_OF_PAGE'
*     I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*     is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat[]
*     IT_SORT                  = GT_SORT[]
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
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       Building Comments For ALV
*----------------------------------------------------------------------*
*      -->P_GT_HEADER[]  text
*----------------------------------------------------------------------*
FORM build_comment USING    p_gt_header TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        ls_color TYPE slis_specialcol_alv,
        l_date(50),
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_ldate(10),
        l_hdate(10).

  DATA: l_serial LIKE ztpp_input_plan-serial,
        l_rp06_serial(10),
        l_model LIKE ztpp_input_plan-modl,
        l_body LIKE ztpp_input_plan-body_ser,
        l_rp06 LIKE ztpp_input_plan-rp06,
        l_rp06_time LIKE sy-uzeit,
        l_rp06_date LIKE sy-datum,
        l_rp06_time_char(8),
        l_bodyno(9).

  WRITE sy-datum TO l_date .
* Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-001.
  APPEND ls_line TO p_gt_header.


** Furong on 02/27/13
*  ls_line-typ = 'S'.
*  CONCATENATE 'AS OF :'  l_date(10) INTO ls_line-info SEPARATED BY ' '.

 SELECT MAX( serial ) INTO l_serial
   FROM ztpp_input_plan
    WHERE status = '06'.
   select single modl body_ser rp06
     INTO (l_model, l_body, l_rp06)
    FROM ztpp_input_plan
    WHERE serial = l_serial.

  CONCATENATE l_model l_body INTO l_bodyno.

  select single atwrt into l_rp06_serial
    from ausp as a
    inner join cabn as b
    on a~atinn = b~atinn
    where objek = l_bodyno
      and klart = '002'
      and atnam = 'P_RP06_SERIAL'.

   write: l_rp06+8(6) to l_rp06_time.
   write: l_rp06+0(8) to l_rp06_date.

   WRITE l_rp06_date TO l_date .

  ls_line-typ = 'S'.
  CONCATENATE 'AS OF :'  l_date(10)
              INTO ls_line-info SEPARATED BY ' '.
  WRITE: l_rp06_time TO l_rp06_time_char.
  CONCATENATE ls_line-info l_rp06_time_char(8) INTO ls_line-info
              SEPARATED BY ' '.

  CONCATENATE ls_line-info l_bodyno 'SEQ'  l_rp06_serial
              INTO ls_line-info SEPARATED BY ' '.
** end on 02/27/13

  APPEND ls_line TO p_gt_header.
* User
  ls_line-typ  = 'A'.
  ls_line-key  = 'User: '.
  ls_line-info = sy-uname.
  APPEND ls_line TO p_gt_header.

* today
  ls_line-typ  = 'A'.
  ls_line-key  = 'Today : '.
  ls_line-info = l_date(10).
  APPEND ls_line TO p_gt_header.

* MESSAGE
  ls_line-typ  = 'A'.
  ls_line-key  = 'Message:'.
  CONCATENATE 'Message: ' ztpp_common_vals-item3
         INTO  ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.
ENDFORM.                    " build_comment

*---------------------------------------------------------------------*
*       FORM SET_STATUS                                               *
*---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STATUS' EXCLUDING rt_extab.
ENDFORM.                    "

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = 'Z_HYUNDAI_LOGO'
*     i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = gt_header.
ENDFORM.                    " TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  make_display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_display_data.
*---start wskim 03/22/2005
  DATA : z_date_count TYPE i,
          d_field(11),
          d_num(2) TYPE n.
  FIELD-SYMBOLS : <wa_d> TYPE any.
  REFRESH it_data.
*---end
  SELECT *
     INTO CORRESPONDING FIELDS OF TABLE it_data
     FROM ztpp_alc_tinput.
** Changed by Furong on 07/23/07
  SORT it_data BY serial.
** end of change
*---start wskim 03/22/2005
  PERFORM get_date_input CHANGING z_date_count.
*---End

  LOOP AT it_data.
    it_data-tot = it_data-d01 + it_data-d02 + it_data-d03 +
                  it_data-d04 + it_data-d05 + it_data-d06 +
                  it_data-d07 + it_data-d08 + it_data-d09 +
                  it_data-d10 + it_data-d11 + it_data-d12 +
                  it_data-d13 + it_data-d14 + it_data-d15 +
                  it_data-d16 + it_data-d17 + it_data-d18 +
                  it_data-d19 + it_data-d20 + it_data-d21 +
                  it_data-d22 + it_data-d23
*---start wskim 03/22/2005
                  + it_data-w01 + it_data-w02 + it_data-w03 +
                    it_data-w04 + it_data-w05 + it_data-w06 +
                    it_data-w07 + it_data-w08 + it_data-w09 +
                    it_data-w10 + it_data-w11 + it_data-w12 +
                    it_data-w13 + it_data-w14 + it_data-w15 +
                    it_data-w16 + it_data-w17 + it_data-w18 +
                    it_data-w19 + it_data-w20 + it_data-w21 +
                    it_data-w22 + it_data-w23.
** Furong on 08/07/12 for 3 shift - d03 as curr date
*    d_num = z_date_count + 3.
    d_num = z_date_count + 4.
** end on 08/07/12
    DO .
      IF d_num > 23.
        EXIT.
      ENDIF.
      CONCATENATE 'IT_DATA-D' d_num INTO d_field.
      ASSIGN (d_field) TO <wa_d>.
      it_data-rem =  it_data-rem + <wa_d>.
      d_num = d_num + 1.
    ENDDO.
*    it_data-rem = it_data-d17 + it_data-d18 + it_data-d19 +
*                  it_data-d20 + it_data-d21 + it_data-d22 +
*                  it_data-d23 .
*---end

    MODIFY it_data.
  ENDLOOP.
ENDFORM.                    " make_display_data

*&---------------------------------------------------------------------*
*&      Form  READ_START_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DATUM  text
*----------------------------------------------------------------------*
FORM read_start_day USING    pa_datum.
  CLEAR: ztpp_common_vals.
  SELECT SINGLE *
    FROM ztpp_common_vals
   WHERE jobs = c_jobs
     AND key2 = c_key1 .

  pa_datum = ztpp_common_vals-dates.
ENDFORM.                    " READ_START_DAY

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING    pa_kalid.
  SELECT SINGLE kalid INTO pa_kalid
    FROM zvpp_capacity
   WHERE arbpl = 'T'   .
ENDFORM.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM read_working_date USING  pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      correct_option               = pa_type
      date                         = pa_wdate
      factory_calendar_id          = pa_kalid
    IMPORTING
      date                         = pa_wdate
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      correct_option_invalid       = 2
      date_after_range             = 3
      date_before_range            = 4
      date_invalid                 = 5
      factory_calendar_not_found   = 6
      OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE
*&---------------------------------------------------------------------*
*&      Form  get_date_7jb_a
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_date_7jb_a.
  REFRESH it_7jb_date.

  SELECT * INTO TABLE it_7jb_a
** On 11/06/13 by Furong
  FROM ztpp_pmt07jb_a_h
*  FROM ztpp_pmt07jb_a
** End
   WHERE gubb EQ 'B'.

  it_7jb_date[] = it_7jb_a[].
  SORT it_7jb_date BY sqdt.                                 "UD1K917193
  DELETE ADJACENT DUPLICATES FROM it_7jb_date COMPARING sqdt.

ENDFORM.                    " get_date_7jb_a
*&---------------------------------------------------------------------*
*&      Form  get_date_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_Z_DATE_COUNT  text
*----------------------------------------------------------------------*
FORM get_date_input CHANGING p_date_count.

  CLEAR : z_max_date.
  SELECT MAX( sqdt ) INTO z_max_date
** On 11/06/13 by Furong
  FROM ztpp_pmt07jb_a_h
*   FROM ztpp_pmt07jb_a
** End
    WHERE gubb = 'A'.

  PERFORM read_start_day    USING wa_datum  .
  PERFORM read_shop_calid   USING wa_kalid  .
*  wa_datum = wa_datum + 1.

  DO.
    IF  wa_datum >= z_max_date.
      EXIT.
    ENDIF.
    wa_datum = wa_datum + 1.
    PERFORM read_working_date USING '+'  wa_kalid  wa_datum .
    p_date_count = p_date_count + 1.

  ENDDO.

ENDFORM.                    " get_date_input
