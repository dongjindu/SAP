*----------------------------------------------------------------------
* Program ID        : ZA_HR_CLOCK_IN
* Title             : [HR] Team Member Availablity Report
* Created on        : 2/27/2008
* Created by        : ig. Moon
* Specifications By : JT. Hu
* Description       : Display Clock-In Data
*----------------------------------------------------------------------
REPORT zi_hr_disp_clock_in MESSAGE-ID zmco.

TABLES : t001k, pa0001,pa0007,
          zthr_bhisthmma,
          zshrclockin ,sscrfields, t526, t508s,
          zthrclkauth .

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
       exporting
            titel = &1
            txt1  = &2
            txt2  = sy-subrc.
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE text-001.
PARAMETERS p_date LIKE zthr_bhisthmma-rdate OBLIGATORY DEFAULT
sy-datum.
SELECTION-SCREEN END OF BLOCK bl.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS s_pernr FOR pa0001-pernr MATCHCODE OBJECT prem
NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20)  text-x01 MODIF ID opt.
SELECTION-SCREEN POSITION 28.
PARAMETERS p_op0   RADIOBUTTON GROUP radi
                        USER-COMMAND mcom DEFAULT 'X'.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK bxx WITH FRAME." title text-0x3.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 32.
PARAMETERS p_all RADIOBUTTON GROUP rdt MODIF ID pll.
SELECTION-SCREEN COMMENT 35(26)  text-x11 MODIF ID bri.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 32.
PARAMETERS p_one RADIOBUTTON GROUP rdt MODIF ID pll.
SELECTION-SCREEN COMMENT 35(27)  text-x10 MODIF ID bri.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 32.
PARAMETERS p_ont RADIOBUTTON GROUP rdt MODIF ID pll.
SELECTION-SCREEN COMMENT 35(27)  text-x12 MODIF ID bri.
SELECTION-SCREEN END   OF LINE.

SELECT-OPTIONS s_tmpemp FOR zthr_bhisthmma-employeenumber NO INTERVALS
.

SELECTION-SCREEN END OF BLOCK bxx. " block for perm./temp. listing

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16)  text-x02 MODIF ID opt.
SELECTION-SCREEN POSITION 28.
PARAMETERS p_op1   RADIOBUTTON GROUP radi.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS s_kostl FOR pa0001-kostl NO INTERVALS
                                 MODIF ID op3..
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16)  text-x03 MODIF ID opt.
SELECTION-SCREEN POSITION 28.
PARAMETERS p_op2   RADIOBUTTON GROUP radi.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS s_orgeh FOR pa0001-orgeh NO INTERVALS
                                 MODIF ID op4..
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18)  text-x04 MODIF ID opt.
SELECTION-SCREEN POSITION 28.
PARAMETERS p_op3   RADIOBUTTON GROUP radi.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS s_tmcode  FOR t526-sachx NO INTERVALS
                                 MODIF ID op4..
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
SELECT-OPTIONS s_schkz FOR pa0007-schkz NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b4.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE text-010.
PARAMETERS p_l1   RADIOBUTTON GROUP radl USER-COMMAND ucom.
PARAMETERS p_l2   RADIOBUTTON GROUP radl.
PARAMETERS p_l3   RADIOBUTTON GROUP radl.
PARAMETERS p_l4   RADIOBUTTON GROUP radl.
PARAMETER  p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b6.

SELECTION-SCREEN BEGIN OF BLOCK b7 WITH FRAME TITLE text-011.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(26)  text-c01.
SELECTION-SCREEN POSITION 32.
PARAMETERS p_c1   AS CHECKBOX. " default 'X' .
SELECTION-SCREEN POSITION 35.
PARAMETERS p_days(2) TYPE n DEFAULT '1'.
SELECTION-SCREEN COMMENT 39(11)  text-c02.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(37)  text-ca0.
SELECTION-SCREEN POSITION 42.
PARAMETERS p_cas0   RADIOBUTTON GROUP casl.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(37)  text-ca1.
SELECTION-SCREEN POSITION 42.
PARAMETERS p_cas1   RADIOBUTTON GROUP casl.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(49)  text-ca2.
SELECTION-SCREEN POSITION 51.
PARAMETERS p_fill AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.

* Use recent emp# when emp# is null in In/Out table.
SELECTION-SCREEN END OF BLOCK b7.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*

DATA: BEGIN OF gt_emp OCCURS 0,
          badge LIKE zthr_bhisthmma-badge,
          employeenumber LIKE zthr_bhisthmma-employeenumber,
          pernr LIKE pa0001-pernr,
      END OF gt_emp.

TYPES: BEGIN OF ty_row_tab.
        INCLUDE STRUCTURE zshrclockin.
TYPES: END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES: END OF ty_out.

DATA: BEGIN OF itab OCCURS 0,
          employeenumber LIKE zthr_bhisthmma-employeenumber,
          pernr LIKE pa0001-pernr,
          cnt TYPE i,
          readerid LIKE zthr_bhisthmma-readerid,
          rdate LIKE zthr_bhisthmma-rdate,
          rtime LIKE zthr_bhisthmma-rtime,
          inout,
          $str(20),
          door_desc LIKE zthrdoor-zhdrds,
          badge LIKE zthr_bhisthmma-badge,
          flag(1),
      END OF itab.

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

DATA: g_error(1),
      g_repid  LIKE sy-repid,
      g_unavail TYPE i,
      g_avail TYPE i,
      g_total TYPE i,
      g_etc TYPE i,
      g_active TYPE i,
      g_inactive TYPE i.


DATA: BEGIN OF con_list OCCURS 0,
          werks LIKE t526-werks,
          sachx LIKE t526-sachx,
          sachn LIKE t526-sachn,
      END OF con_list.

DATA: BEGIN OF help_field OCCURS 0.
        INCLUDE STRUCTURE help_value.
DATA: END OF help_field.

DATA: BEGIN OF help_vtab OCCURS 0.
        INCLUDE STRUCTURE help_vtab.
DATA: END OF help_vtab.

DATA: BEGIN OF help_value OCCURS 0,
      value LIKE help_vtab-value,
      END OF help_value.

DATA: BEGIN OF dynpfields OCCURS 3.
        INCLUDE STRUCTURE dynpread.
DATA: END OF dynpfields.

DATA: w_sachn      LIKE t526-sachn            ,
      w_enddate(8) TYPE c VALUE '99991231'    .

DATA: BEGIN OF it_pernr OCCURS 0,
        pernr LIKE zshrclockin-pernr,
        employeenumber LIKE zthr_bhisthmma-employeenumber,
        sname  LIKE pa0001-sname,
        kostl  LIKE pa0001-kostl,
        orgeh  LIKE pa0001-orgeh,
        ename  LIKE pa0001-ename,
        sachz  LIKE pa0001-sachz,
        schkz  LIKE pa0007-schkz,
        stat2  LIKE pa0000-stat2,
        perflg,
        persg  LIKE pa0001-persg,
        persk  LIKE pa0001-persk,
END   OF it_pernr.

DATA it_pernr_2nd LIKE it_pernr OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF t_t526 OCCURS 0,
        sachx LIKE t526-sachx ,
        sachn LIKE t526-sachn ,
        admncode(100) TYPE c  ,
      END OF t_t526           .

DATA: BEGIN OF it_kostx OCCURS 0,
        kostl LIKE cskt-kostl ,
        kostx LIKE cskt-ktext ,
      END OF it_kostx           .

DATA: BEGIN OF it_orgtx OCCURS 0,
        orgeh LIKE t527x-orgeh ,
        orgtx LIKE t527x-orgtx ,
      END OF it_orgtx           .

DATA: BEGIN OF it_door OCCURS 0,
        zhdoor LIKE zthrdoor-zhdoor,
        zhdrio LIKE zthrdoor-zhdrio,
        zhdrds LIKE zthrdoor-zhdrds,
      END OF it_door           .

DATA: BEGIN OF it_ws OCCURS 0,
*        kokrs TYPE kokrs,
*        mosid TYPE mosid,
        schkz	TYPE schkn,
        zeity TYPE zeity,
        rtext	TYPE retext,
        anzsh TYPE anzschicht,
        tprog TYPE tprog,
      END OF it_ws           .

DATA: BEGIN OF it_status OCCURS 0,
        pernr LIKE pa0000-pernr,
        begda LIKE pa0000-begda,
        massn LIKE pa0000-massn,
        massg LIKE pa0000-massg,
        stat2 LIKE pa0000-stat2,
      END OF it_status           .

DATA g_kokrs TYPE kokrs VALUE 'H201'.

DATA $flag.

DATA $tprog TYPE  tprog.

INCLUDE <icon>.                        " icon
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title = '[HR] Team Member Availablity Report'.

  CHECK sy-uname NE : '103569' , '100553', 'HIS20065'.

  SELECT SINGLE * FROM zthrclkauth
          WHERE pernr EQ sy-uname
            AND begda <= sy-datum
            AND endda >= sy-datum.

  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'You are not authorized to use this program.'.
    LEAVE PROGRAM.
  ENDIF.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN .
  CASE sscrfields-ucomm.
    WHEN 'UCOM'.
      CASE true.
        WHEN p_l1.
          p_vari = space.
        WHEN p_l2.
          p_vari = '/TOTAL_CC'.
        WHEN p_l3.
          p_vari = '/TOTAL_ORG'.
        WHEN p_l4.
          p_vari = '/TOTAL_ADM'.
      ENDCASE.
*    when 'COP'.
*
*      if sy-uname eq '103569'.
*        delete from zthr_bhisthmma client specified
*        where mandt eq sy-mandt.
*        commit work.
*        select * from zthr_bhisthmma.
*          move-corresponding zthr_bhisthmma to zthr_bhisthmma.
*          insert zthr_bhisthmma.
*        endselect.
*        stop.
*      endif.

  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tmcode-low.
  PERFORM tmcode_input_help CHANGING s_tmcode-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tmcode-high.
  PERFORM tmcode_input_help CHANGING s_tmcode-high.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  DATA : date_low LIKE sy-datum,
         date_high LIKE sy-datum,
         $times TYPE i,
         $date LIKE sy-datum .

  PERFORM initialize.
  PERFORM get_info.

  CHECK g_error = false.

* {
*  if not s_date[] is initial.
*
*    date_low = s_date-low.
*
*    $times = s_date-high - s_date-low + 1.
*    date_low = s_date-low.
*
*    if s_date-high is initial.
*      $times = 1.
*    endif.
*
*    if s_date-low is initial.
*      $times = 1.
*      date_low = s_date-high.
*    endif.
*
*    perform show_progress using 'Gather Data...' ' 5'.
*
*    do $times times.
*      $date = date_low.
*
*      check $date in s_date.
*
*      perform  get_row_data using $date.
*      perform  set_row_data using $date.
*      perform  refine_row_itab.
*      add 1 to date_low.
*
*    enddo.
*
*  endif.
* }

  $date = p_date.

  PERFORM : get_stauts USING $date,
            get_row_data USING $date,
            set_row_data USING $date,
            refine_row_itab.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
  CHECK g_error EQ false.
  PERFORM set_output.
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output.

  PERFORM show_progress     USING 'Preparing screen...' '
                                                         95'.
  PERFORM init_alv_parm.
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].
  PERFORM alv_events_get    USING:  'P', 'T'.
  PERFORM alv_grid_display  TABLES  gt_out USING ''.

ENDFORM.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_PARM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_parm.

  __cls   :  gt_fieldcat, gt_sort, gt_events, gt_listheader,
             gt_sp_group.

  CLEAR   :  gs_layout.

  gs_layout-colwidth_optimize = 'X'.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " INIT_ALV_PARM
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i.

  __cls ft_fieldcat.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_s     = &3.        " Column heading
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :
    'X'  'RDATE'    'Date'            10  'DATS' '' '',
    'X'  'KOSTL'    'Cst.C'           10  'CHAR' '' '',
    'X'  'KOSTX'    'C.desc.'         20  'CHAR' '' '',
    'X'  'ORGEH'    'Org.unit'         8  'CHAR' '' '',
    'X'  'ORGTX'    'O.desc.'         25  'CHAR' '' '',
    'X'  'SACHZ'    'Adm'              3  'CHAR' '' '',
    'X'  'EMPLOYEENUMBER'   'Emply.#' 20  'CHAR' '' '',
    ' '  'SNAME'    'Name'            30  'CHAR' '' '',
    ' '  'CATEG'    'C'                1  'CHAR' '' '',
    ' '  'STAT2'    'S'                1  'CHAR' '' '',
    ' '  'ZCLKAVA'  'Av.'        4  'ICON' '' '',
*    ' '  'ZCLKUAVA' 'Unavailable'      4  'ICON' '' '',
    ' '  'ANZSHTXT' 'Sft'              3  'CHAR' '' '',
    ' '  'SCHKZ'    'Wrk.Schdl'        8  'CHAR' '' '',
    ' '  'RTEXT'    'WrS Schdl Desc.'  20 'CHAR' '' '',
    ' '  'ZHRLSTY'  'C'                1  'CHAR' '' '',
    ' '  'ZDOORIDI'  'Door In'          5  'NUMC' '' '',
    ' '  'ZDOORIDIT' 'Door In Text'    20  'CHAR' '' '',
    ' '  'RDATEI'   'In Date'          8  'DATS' '' '',
    ' '  'ZCLKIN'   'Clock In'         6  'TIMS' '' '',
    ' '  'ZDOORIDO'  'Door Out'         5  'NUMC' '' '',
    ' '  'ZDOORIDOT'  'Door Out Text'   20  'CHAR' '' '',
    ' '  'RDATEO'   'OutDate'          8  'DATS' '' '',
    ' '  'ZCLKOUT'  'Out'              4  'TIMS' '' '',
    ' '  'ZCNTA'    'A'                4  'INT2' '' '',
    ' '  'ZCNTU'    'U'                4  'INT2' '' '',
    ' '  'ZCNTT'    'T'                4  'INT2' '' ''.

  PERFORM change_fieldcat USING ft_fieldcat[] .

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING    ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-comp      = &5.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.


  sort_tab :
            'RDATE'   ' ' 'X' 'X' 'X',
            'KOSTL'   ' ' 'X' 'X' 'X',
            'KOSTX'   ' ' 'X' 'X' 'X',
            'ORGEH'   ' ' 'X' 'X' 'X',
            'ORGTX'   ' ' 'X' 'X' 'X',
            'SACHZ'   ' ' 'X' 'X' 'X',
*            'PERNR'   ' ' 'X' 'X' 'X'.
            'EMPLOYEENUMBER'   ' ' 'X' 'X' 'X'.


ENDFORM.                    " SORT_BUILD

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  DATA : $g_total(20),
         $g_avail(20),
         $g_unavail(20),
         $g_active(20),
         $g_inactive(20),
         $g_etc(20).

  REFRESH gt_listheader.

  l_text = 'Team Member Availablity Report'.
  PERFORM set_header_line USING :
          'P' 'H' '' l_text ''.

  PERFORM set_header_line USING :
          'D' 'S' 'Date' p_date      ''.

  IF p_op0 EQ true.
    PERFORM set_header_line USING :
            'S' 'S' 'Employee #'   s_pernr-low ' '.
  ENDIF.
  IF p_op1 EQ true.
    PERFORM set_header_line USING :
            'S' 'S' 'Cost Cnter'   s_kostl-low ' '.
  ENDIF.

  IF p_op2 EQ true.
    PERFORM set_header_line USING :
            'S' 'S' 'Org.
                          unit '   s_orgeh-low ' '.
  ENDIF.

  IF p_op3 EQ true.
    PERFORM set_header_line USING :
            'S' 'S' 'Adm.
                          code '   s_tmcode-low ' '.
  ENDIF.

  PERFORM set_header_line USING :
          'S' 'S' 'Wrk.
                        scdl '   s_schkz-low ' '.

  WRITE: g_total TO $g_total RIGHT-JUSTIFIED,
         g_avail TO $g_avail  RIGHT-JUSTIFIED,
         g_unavail TO $g_unavail RIGHT-JUSTIFIED,
         g_active TO $g_active RIGHT-JUSTIFIED,
         g_inactive TO $g_inactive RIGHT-JUSTIFIED,
         g_etc TO $g_etc RIGHT-JUSTIFIED.

  PERFORM set_header_line USING :
          'P' 'S' '' '-----------------' ''.

  DATA $string(60).
  DATA $string_l(120).
  CONCATENATE $g_inactive '/' $g_active '/' $g_etc INTO $string.
  CONDENSE $string.
  CONCATENATE $g_total '(' $string ')' INTO $string_l.
  CONDENSE $string_l.

  PERFORM set_header_line USING :
            'P' 'S' 'Total in S.Criteria'  $g_total ''.

  PERFORM set_header_line USING :
            'P' 'S' 'Available'  $g_avail '',
            'P' 'S' 'Unavailable'  $g_unavail ''.

  PERFORM set_header_line USING :
            'P' 'S' 'Inactive/Active/Tmp.'  $string ''.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page
*---------------------------------------------------------------------*
*       FORM PF_STATUS_SET
*---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.
  SET PF-STATUS '100'." excluding ft_extab.
ENDFORM.                    "PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.
  CLEAR : g_error.

  CASE fp_ucomm.
  ENDCASE.

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FT_FIELDCAT[]  text
*      -->P_ENDFORM  text
*----------------------------------------------------------------------*
FORM change_fieldcat USING    pt_fieldcat TYPE slis_t_fieldcat_alv.

  LOOP AT pt_fieldcat INTO gs_fieldcat.

    gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname.

    IF gs_fieldcat-fieldname EQ 'SCHKZ'.
      gs_fieldcat-ref_tabname = 'PA0007'.
    ELSE.
      gs_fieldcat-ref_tabname = 'PA0001'.
    ENDIF.

    IF gs_fieldcat-fieldname EQ 'ZCLKIN' OR
       gs_fieldcat-fieldname EQ 'ZCLKOUT' OR
       gs_fieldcat-fieldname EQ 'ZDOORIDI' OR
       gs_fieldcat-fieldname EQ 'ZDOORIDO' OR
       gs_fieldcat-fieldname EQ 'ZCNTA' OR
       gs_fieldcat-fieldname EQ 'ZCNTU' OR
       gs_fieldcat-fieldname EQ 'ZHRLSTY' OR
       gs_fieldcat-fieldname EQ 'ORGEH'.
      gs_fieldcat-no_zero = 'X'.
    ENDIF.

    IF gs_fieldcat-fieldname EQ 'ZDOORIDI' OR
       gs_fieldcat-fieldname EQ 'ZDOORIDO'.
      gs_fieldcat-just = 'L'.
    ENDIF.

    IF gs_fieldcat-fieldname EQ 'ZHRLSTY'.
      IF p_c1 NE true.
        gs_fieldcat-no_out = true.
      ENDIF.
    ENDIF.

    IF gs_fieldcat-fieldname EQ 'RDATE'.
      gs_fieldcat-no_out = true.
    ENDIF.
    IF gs_fieldcat-fieldname NE 'SCHKZ'.
      gs_fieldcat-ref_tabname = 'ZSHRCLOCKIN'.
    ENDIF.

    MODIFY pt_fieldcat FROM gs_fieldcat.

  ENDLOOP.

ENDFORM.                    " CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  refine_row_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refine_row_itab.

  __cls gt_out.

  LOOP AT it_row_tab.
    IF it_row_tab-employeenumber(2) EQ '00'.
      SHIFT it_row_tab-employeenumber LEFT BY 2 PLACES.
    ENDIF.

    IF it_row_tab-stat2 EQ '1'.
      IF it_row_tab-zdooridi IS INITIAL AND
         it_row_tab-zdoorido IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " refine_row_itab
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.

  __cls gt_out.
  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " MOVE_OUT_
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0811   text
*      -->P_0812   text
*      -->P_0813   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM pop_up USING    p_text p_text2 p_canc
            CHANGING p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = p_text
            textline2      = p_text2
            titel          = 'Check!'
            cancel_display = p_canc
       IMPORTING
            answer         = p_answer.

ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize.

  CLEAR : g_error.

  IF p_c1 EQ true.
    IF p_days IS INITIAL.
      MESSAGE s001 WITH 'Please enter the Days for check!'.
      g_error = true.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_op3 EQ true.
    IF s_tmcode[] IS INITIAL.
      MESSAGE s000 WITH 'Please enter Adm Code!'.
      g_error = true.
      STOP.
    ENDIF.
    __cls t_t526.

    w_sachn = '%NO LONGER VALID%'.
    SELECT sachx sachn
                 FROM t526
                 INTO TABLE t_t526
                 WHERE sachx IN s_tmcode AND
                       sachn NOT LIKE w_sachn.
  ENDIF.

  __cls : it_row_tab.

ENDFORM.                    " initialize

*&---------------------------------------------------------------------*
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data USING check_date.

  __cls : it_pernr.

  DATA $ix LIKE sy-tabix.
  RANGES r_employeenumber FOR zthr_bhisthmma-employeenumber.

  __cls itab.

  DATA y_date LIKE sy-datum.

  IF p_c1 EQ true.
    y_date = check_date - p_days.
  ELSE.
    y_date = check_date.
  ENDIF.

  IF p_op0 EQ false.
    __cls s_pernr  .
  ENDIF.

  PERFORM show_progress USING 'Gather Data...' 30.


  IF NOT s_pernr[] IS INITIAL.

    SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
           b~schkz a~persg a~persk
           INTO CORRESPONDING FIELDS OF TABLE it_pernr
             FROM pa0001 AS a INNER JOIN pa0007 AS b
               ON b~pernr = a~pernr
               WHERE a~pernr IN s_pernr
                 AND a~begda LE check_date
                 AND a~endda GE check_date
                 AND b~begda LE check_date
                 AND b~endda GE check_date
                 AND b~schkz IN s_schkz.
    LOOP AT s_pernr.
      r_employeenumber = s_pernr.
      r_employeenumber-low = s_pernr-low+2.
      r_employeenumber-high = s_pernr-high+2.
      APPEND r_employeenumber.
    ENDLOOP.

    __cls gt_emp.

    IF p_fill EQ true.
      SELECT badge employeenumber
    INTO CORRESPONDING FIELDS OF TABLE gt_emp
      FROM zthr_bhisthmma
      WHERE employeenumber IN r_employeenumber
      GROUP BY BADGE employeenumber.
      SORT gt_emp BY badge.
      DELETE ADJACENT DUPLICATES FROM gt_emp
        COMPARING badge.
    ENDIF.

  ELSE.

    __cls gt_emp.

    IF p_fill EQ true.
      SELECT DISTINCT badge employeenumber INTO TABLE gt_emp
      FROM zthr_bhisthmma
      WHERE ( employeenumber LIKE '1%' OR employeenumber LIKE '0%' )
      GROUP by badge employeenumber.

      SORT gt_emp BY badge employeenumber DESCENDING.
      DELETE ADJACENT DUPLICATES FROM gt_emp
        COMPARING badge.
    ENDIF.

    CASE true.
      WHEN p_op0.

        IF p_ont NE true.

* permanant emp.

          SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
                 b~schkz a~persg a~persk
                 INTO CORRESPONDING FIELDS OF TABLE it_pernr
                   FROM pa0001 AS a INNER JOIN pa0007 AS b
                     ON b~pernr = a~pernr
                     WHERE   a~begda LE check_date
                             AND a~endda GE check_date
                             AND b~begda LE check_date
                             AND b~endda GE check_date
                             AND b~schkz IN s_schkz.
        ENDIF.

      WHEN p_op1.
        IF s_kostl[] IS INITIAL.
          MESSAGE s000 WITH 'Please enter Cost Center!.'.
          g_error = true.
          STOP.
        ENDIF.

        SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
               b~schkz a~persg a~persk
               INTO CORRESPONDING FIELDS OF TABLE it_pernr
                 FROM pa0001 AS a INNER JOIN pa0007 AS b
                   ON b~pernr = a~pernr
                   WHERE a~kostl IN s_kostl
                       AND a~begda LE check_date
                       AND a~endda GE check_date
                       AND b~begda LE check_date
                       AND b~endda GE check_date
                       AND b~schkz IN s_schkz.
      WHEN p_op2.
        IF s_orgeh[] IS INITIAL.
          MESSAGE s000 WITH 'Please enter Org.
                                               unit!'.
          g_error = true.
          STOP.
        ENDIF.

        SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
               b~schkz a~persg a~persk
               INTO CORRESPONDING FIELDS OF TABLE it_pernr
                 FROM pa0001 AS a INNER JOIN pa0007 AS b
                   ON b~pernr = a~pernr
                   WHERE a~orgeh IN s_orgeh
                       AND a~begda LE check_date
                       AND a~endda GE check_date
                       AND b~begda LE check_date
                       AND b~endda GE check_date
                       AND b~schkz IN s_schkz.
      WHEN p_op3.

        IF NOT t_t526[] IS INITIAL.
          LOOP AT t_t526.
            IF t_t526-sachx IS INITIAL AND t_t526-sachn IS INITIAL.
              DELETE t_t526.
              CONTINUE.
            ENDIF.

            CONCATENATE t_t526-sachx t_t526-sachn INTO t_t526-admncode
                                                   SEPARATED BY space.
            MODIFY t_t526 INDEX sy-tabix TRANSPORTING admncode.
          ENDLOOP.

          SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
                    b~schkz a~persg a~persk
                    INTO CORRESPONDING FIELDS OF TABLE it_pernr
                       FROM pa0001 AS a INNER JOIN pa0007 AS b
                         ON b~pernr = a~pernr
                         FOR ALL ENTRIES IN t_t526
                         WHERE a~sachz = t_t526-sachx
                           AND a~begda LE check_date
                           AND a~endda GE check_date
                           AND b~begda LE check_date
                           AND b~endda GE check_date
                           AND b~schkz IN s_schkz.

        ELSE.
          g_error = true.
          MESSAGE s000 WITH 'No data has been found!'.
          STOP.
        ENDIF.
    ENDCASE.
  ENDIF.

*  if sy-subrc ne 0.
*    g_error = true.
*    message s000 with 'No data has been found!'.
*  endif.

  LOOP AT it_pernr.
    $ix = sy-tabix.
    it_pernr-employeenumber = it_pernr-pernr+2.
    MODIFY it_pernr INDEX $ix TRANSPORTING employeenumber.
  ENDLOOP.

  IF p_op0 EQ true.
    IF ( p_all EQ true OR p_ont EQ true ) AND s_pernr[] IS INITIAL.

      IF p_c1 EQ true.
        SELECT employeenumber
        FROM zthr_bhisthmma AS a
          INNER JOIN zthrdoor AS b
          ON b~zhdoor EQ a~readerid
         AND b~zhdrcns EQ true
          APPENDING CORRESPONDING FIELDS OF TABLE it_pernr
          WHERE ( rdate BETWEEN y_date AND check_date  )
          AND employeenumber NE space
          %_HINTS ORACLE 'FIRST_ROWS(10)'.
      ELSE.
        SELECT employeenumber
        FROM zthr_bhisthmma AS a
          INNER JOIN zthrdoor AS b
          ON b~zhdoor EQ a~readerid
         AND b~zhdrcns EQ true
          APPENDING CORRESPONDING FIELDS OF TABLE it_pernr
          WHERE rdate EQ check_date
          AND employeenumber NE space
          %_HINTS ORACLE 'FIRST_ROWS(10)'.
      ENDIF.

      SORT it_pernr BY employeenumber ASCENDING
                       sname DESCENDING.

      LOOP AT it_pernr.
        $ix  = sy-tabix.
        CHECK it_pernr-pernr IS INITIAL.
        CHECK NOT it_pernr-employeenumber IN s_tmpemp.
        DELETE it_pernr INDEX $ix.
      ENDLOOP.

     DELETE ADJACENT DUPLICATES FROM it_pernr COMPARING employeenumber .
    ENDIF.
  ENDIF.

  IF NOT r_employeenumber[] IS INITIAL.
    IF p_c1 EQ true.
      SELECT employeenumber readerid rdate rtime badge
      FROM zthr_bhisthmma
        INTO CORRESPONDING FIELDS OF TABLE itab
        WHERE ( rdate BETWEEN y_date AND check_date  )
          AND employeenumber IN r_employeenumber
          %_HINTS oracle 'FIRST_ROWS(10)'.

    ELSE.
      SELECT employeenumber readerid rdate rtime badge
      FROM zthr_bhisthmma
        INTO CORRESPONDING FIELDS OF TABLE itab
        WHERE rdate EQ check_date
          AND employeenumber IN r_employeenumber
          %_HINTS oracle 'FIRST_ROWS(10)'.

    ENDIF.
  ELSE.
    IF it_pernr[] IS INITIAL.
      g_error = true.
      MESSAGE s000 WITH 'No data has been found!'.
    ELSE.
      IF p_op0 EQ true.
        IF p_c1 EQ true.
          SELECT employeenumber readerid rdate rtime badge
            FROM zthr_bhisthmma
              INTO CORRESPONDING FIELDS OF TABLE itab
              WHERE rdate BETWEEN y_date AND check_date
              %_HINTS ORACLE 'FIRST_ROWS(10)'.

        ELSE.
          SELECT employeenumber readerid rdate rtime badge
            FROM zthr_bhisthmma
              INTO CORRESPONDING FIELDS OF TABLE itab
              WHERE rdate EQ check_date
             %_HINTS ORACLE 'FIRST_ROWS(10)'.
        ENDIF.
      ELSE.
        IF p_c1 EQ true.
          SELECT employeenumber readerid rdate rtime badge
            FROM zthr_bhisthmma
              INTO CORRESPONDING FIELDS OF TABLE itab
              FOR ALL ENTRIES IN it_pernr
              WHERE employeenumber EQ  it_pernr-employeenumber
                AND ( rdate BETWEEN y_date AND check_date  )
                %_HINTS ORACLE 'FIRST_ROWS(10)'.

        ELSE.
          SELECT employeenumber readerid rdate rtime badge
            FROM zthr_bhisthmma
              INTO CORRESPONDING FIELDS OF TABLE itab
              FOR ALL ENTRIES IN it_pernr
              WHERE employeenumber EQ  it_pernr-employeenumber
                AND rdate EQ check_date
              %_HINTS ORACLE 'FIRST_ROWS(10)'.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


  __cls it_pernr_2nd.

  SORT it_ws BY tprog.

  LOOP AT itab.
    AT NEW employeenumber.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    CLEAR $flag.

    READ TABLE it_pernr WITH KEY
                employeenumber = itab-employeenumber
                BINARY SEARCH.

    IF sy-subrc EQ 0.
      CALL FUNCTION 'Z_CO_GET_DWS_IG'
           EXPORTING
                schkz                          = it_pernr-schkz
                datum                          = check_date
           IMPORTING
                tprog                          = $tprog
           EXCEPTIONS
                not_found_work_schedule_rules  = 1
                invalid_date                   = 2
                not_found_period_work_schedule = 3
                OTHERS                         = 4.

      IF sy-subrc <> 0.
        $tprog = it_pernr-schkz.
      ENDIF.

      READ TABLE it_ws WITH KEY tprog = $tprog BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF it_ws-anzsh EQ '2'.
          it_pernr_2nd = it_pernr.
          APPEND it_pernr_2nd.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

  SORT it_pernr_2nd BY employeenumber.
  DELETE ADJACENT DUPLICATES FROM it_pernr_2nd COMPARING employeenumber.


  DATA  $check_date LIKE sy-datum.

  IF NOT it_pernr_2nd[] IS INITIAL.
    $check_date = check_date + 1.
    SELECT employeenumber readerid rdate rtime badge
      FROM zthr_bhisthmma
        APPENDING CORRESPONDING FIELDS OF TABLE itab
        FOR ALL ENTRIES IN it_pernr_2nd
        WHERE employeenumber EQ  it_pernr_2nd-employeenumber
          AND rdate EQ $check_date
        %_HINTS ORACLE 'FIRST_ROWS(10)'.
  ENDIF.
* }
  PERFORM modi_itab USING check_date.

  DATA: BEGIN OF it_new_pernr OCCURS 0,
          pernr   LIKE   pa0001-pernr,
        END   OF it_new_pernr.

  LOOP AT it_pernr.
    $ix = sy-tabix.
    READ TABLE it_status WITH KEY pernr = it_pernr-employeenumber
    BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF p_ont EQ true AND p_op0 EQ true.
        DELETE it_pernr INDEX $ix.
        CONTINUE.
      ENDIF.
      it_pernr-perflg = true.
      MODIFY it_pernr INDEX $ix TRANSPORTING perflg.
      IF it_pernr-orgeh IS INITIAL.
        it_new_pernr-pernr = it_pernr-employeenumber.
        APPEND it_new_pernr.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DATA  $it_pernr LIKE it_pernr OCCURS 0 WITH HEADER LINE.

  IF NOT it_new_pernr[] IS INITIAL.
    SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
           b~schkz a~persg a~persk
           INTO CORRESPONDING FIELDS OF TABLE $it_pernr
             FROM pa0001 AS a INNER JOIN pa0007 AS b
               ON b~pernr = a~pernr
             FOR ALL ENTRIES IN it_new_pernr
               WHERE a~pernr EQ it_new_pernr-pernr
                 AND a~begda LE check_date
                 AND a~endda GE check_date
                 AND b~begda LE check_date
                 AND b~endda GE check_date.

    SORT $it_pernr BY pernr.

    LOOP AT it_pernr.
      $ix = sy-tabix.
      READ TABLE $it_pernr WITH KEY pernr = it_pernr-employeenumber
 BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_pernr = $it_pernr.
        it_pernr-perflg = true.
        MODIFY it_pernr INDEX $ix.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT it_pernr.

    MOVE-CORRESPONDING it_pernr TO it_row_tab.

    IF NOT it_pernr-pernr IS INITIAL.
      READ TABLE it_status WITH KEY pernr = it_pernr-pernr
      BINARY SEARCH.
      IF sy-subrc EQ 0 AND
      ( it_status-stat2 EQ '1' OR it_status-stat2 EQ '3' ).
        it_row_tab-stat2 = it_status-stat2.
      ELSE.
        IF it_row_tab-kostl EQ '0000033301'.
          IF it_status-massn EQ 'ZX' AND it_status-massg EQ '17'.
            it_row_tab-stat2 = it_status-stat2.
          ELSE.
            CLEAR it_row_tab.
            CONTINUE.
          ENDIF.
        ELSE.
          CLEAR it_row_tab.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

    IF it_pernr-perflg EQ false.
      it_row_tab-zflgtmp = true. " It's a temp. emp.
    ENDIF.

    READ TABLE it_kostx WITH KEY kostl = it_pernr-kostl
                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-kostx = it_kostx-kostx.
    ENDIF.
    READ TABLE it_orgtx WITH KEY orgeh = it_pernr-orgeh
                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-orgtx = it_orgtx-orgtx.
    ENDIF.

*    READ TABLE it_ws WITH KEY schkz = it_row_tab-schkz BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      it_row_tab-rtext = it_ws-rtext.
*      it_row_tab-anzsh = it_ws-anzsh.
*    ENDIF.
    it_row_tab-rdate = check_date.
    APPEND it_row_tab.CLEAR it_row_tab.
  ENDLOOP.

*  sort it_row_tab by pernr.

  SORT it_row_tab BY employeenumber.

  DATA $cnt TYPE i.

  LOOP AT itab.
    AT NEW pernr.
      CLEAR $cnt.
    ENDAT.
    ADD 1 TO $cnt.

    READ TABLE it_row_tab WITH KEY employeenumber = itab-employeenumber
BINARY SEARCH.

    IF sy-subrc EQ 0 AND it_row_tab-anzsh EQ '2'.
    ELSE.
      CHECK $cnt LE 2.
    ENDIF.

    IF sy-subrc EQ 0.
      CHECK itab-inout EQ '0' OR itab-inout EQ '1'.
      IF itab-inout EQ '0'. " In
        IF it_row_tab-zclkin IS INITIAL.
          it_row_tab-rdatei  = itab-rdate.
          it_row_tab-zdooridi = itab-readerid.
          it_row_tab-zdooridit = itab-door_desc.
          it_row_tab-zclkin  = itab-rtime.
          IF $cnt EQ 1. " Last read is 'in'...
            $cnt = 10.
          ENDIF.
        ENDIF.
      ENDIF.
      IF itab-inout EQ '1'. " Out
        IF it_row_tab-zclkout IS INITIAL.
          it_row_tab-rdateo  = itab-rdate.
          it_row_tab-zdoorido = itab-readerid.
          it_row_tab-zdooridot = itab-door_desc.
          it_row_tab-zclkout = itab-rtime.
        ENDIF.
      ENDIF.
      MODIFY it_row_tab INDEX sy-tabix.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " get_row_data
*&---------------------------------------------------------------------*
*&      Form  tmcode_input_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_S_TMCODE_LOW  text
*----------------------------------------------------------------------*
FORM tmcode_input_help CHANGING p_tmcode.

  DATA j LIKE sy-index.
  CLEAR : con_list.

  SELECT
          werks
          sachx
          sachn
  INTO TABLE con_list
  FROM t526.

  SORT con_list BY werks sachx .
  DELETE con_list WHERE sachx EQ space.
  LOOP AT con_list.
    help_value-value = con_list-sachx.
    APPEND help_value.
    help_value-value = con_list-sachn.
    APPEND help_value.
    help_value-value = con_list-werks.
    APPEND help_value.
  ENDLOOP.

  PERFORM add_fields USING: 'T526'  'SACHX' 'X',
                            'T526'  'SACHN' ' ',
                            'T526'  'WERKS' ' '.

  PERFORM value_help CHANGING j.

  IF j > 0.
    READ TABLE con_list INDEX j.
    p_tmcode = con_list-sachx.
  ENDIF.

*  dynpfields-fieldname  = 'SACHX'.
*  dynpfields-fieldvalue = con_list-sachx.
*  append dynpfields.
*
*  call function 'DYNP_VALUES_UPDATE'
*       exporting
*            dyname     = sy-cprog
*            dynumb     = sy-dynnr
*       tables
*            dynpfields = dynpfields.
*
  CLEAR: dynpfields.
  REFRESH: con_list, help_field, help_vtab, help_value, dynpfields.

ENDFORM.                    " tmcode_input_help
*&---------------------------------------------------------------------*
*&      Form  add_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0864   text
*      -->P_0865   text
*      -->P_0866   text
*----------------------------------------------------------------------*
FORM add_fields USING  p_tabname p_fieldname p_flag.
  help_field-tabname = p_tabname.
  help_field-fieldname = p_fieldname.
  help_field-selectflag = p_flag.
  APPEND help_field.
  CLEAR help_field.
ENDFORM.                    " add_fields
*&---------------------------------------------------------------------*
*&      Form  value_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_J  text
*----------------------------------------------------------------------*
FORM value_help CHANGING p_j.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            display              = ' '
            title_in_values_list = 'Administrator Code'
       IMPORTING
            index                = p_j
       TABLES
            fields               = help_field
            select_values        = help_vtab
            valuetab             = help_value.

ENDFORM.                    " value_help
*&---------------------------------------------------------------------*
*&      Form  get_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info.


  SELECT kostl ktext INTO TABLE it_kostx FROM cskt
  WHERE spras EQ sy-langu.

  SELECT orgeh orgtx INTO TABLE it_orgtx FROM t527x
  WHERE sprsl EQ sy-langu.

  SELECT zhdoor zhdrio zhdrds INTO TABLE it_door FROM zthrdoor
  WHERE zhdrcns EQ true.

*  SELECT b~kokrs  a~mosid a~schkz a~rtext b~anzsh INTO TABLE it_ws
*    FROM t508s AS a
*    INNER JOIN ztco_mh_ws AS b
*    ON  b~kokrs EQ g_kokrs
*    AND b~mosid EQ a~mosid
*    AND b~schkz EQ a~schkz
*  WHERE a~sprsl EQ sy-langu.

  SELECT  schkz zeity rtext INTO TABLE it_ws
    FROM t508s
  WHERE sprsl EQ sy-langu.

  SORT it_ws BY schkz zeity .

  LOOP AT it_ws.
    AT NEW schkz.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    CLEAR $flag.

    CALL FUNCTION 'Z_CO_GET_DWS_IG'
         EXPORTING
              schkz                          = it_ws-schkz
              datum                          = p_date
         IMPORTING
              tprog                          = $tprog
         EXCEPTIONS
              not_found_work_schedule_rules  = 1
              invalid_date                   = 2
              not_found_period_work_schedule = 3
              OTHERS                         = 4.

    IF sy-subrc <> 0.
      $tprog = it_ws-schkz.
    ENDIF.

    it_ws-tprog = $tprog.

    CASE $tprog.
      WHEN '0002' OR '1003' OR '1002'.
        it_ws-anzsh = '2'.
      WHEN OTHERS.
        it_ws-anzsh = '1'.
    ENDCASE.
    MODIFY it_ws TRANSPORTING tprog anzsh WHERE schkz = it_ws-schkz.

  ENDLOOP.

  SORT : it_kostx BY kostl,
         it_orgtx BY orgeh,
         it_door BY zhdoor,
         it_ws BY tprog . "kokrs mosid .

ENDFORM.                    " get_text
*&---------------------------------------------------------------------*
*&      Form  modi_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modi_itab USING check_date.
  DATA $ix LIKE sy-tabix.

  LOOP AT itab.
    $ix = sy-tabix.

    READ TABLE it_door WITH KEY zhdoor = itab-readerid
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-inout = it_door-zhdrio.
      itab-pernr = itab-employeenumber.
      itab-door_desc = it_door-zhdrds.

      IF itab-employeenumber IS INITIAL.
        READ TABLE gt_emp WITH KEY badge = itab-badge BINARY SEARCH.
        IF sy-subrc EQ 0.
          itab-employeenumber = gt_emp-employeenumber.
        ENDIF.

      ENDIF.

      MODIFY itab INDEX $ix TRANSPORTING pernr inout door_desc
      employeenumber.
    ELSE.
      DELETE itab INDEX $ix.
      CONTINUE.
    ENDIF.

    READ TABLE it_pernr_2nd WITH KEY
        employeenumber = itab-employeenumber
        BINARY SEARCH. " 2nd shift.

    IF sy-subrc EQ 0.

      IF itab-inout EQ 1
        AND itab-rdate EQ check_date
        AND itab-rtime <= '120000'.
        DELETE itab INDEX $ix.
      ENDIF.

      IF itab-inout EQ 0
        AND itab-rdate > check_date
        AND itab-rtime >= '120000'.
        DELETE itab INDEX $ix.
      ENDIF.

    ENDIF.

  ENDLOOP.

  DELETE itab WHERE employeenumber EQ space.

  LOOP AT itab.
    CONCATENATE itab-rdate itab-rtime INTO itab-$str.
    MODIFY itab.
  ENDLOOP.

  SORT itab BY employeenumber ASCENDING
               $str DESCENDING .

*  sort itab by pernr ascending
*               $str descending .

  DATA : $flag,
         $cnt TYPE i.

*  delete itab where inout eq '1' and rdate < check_date.

  DATA $itab LIKE itab OCCURS 0 WITH HEADER LINE.
  DATA $fr LIKE sy-tabix.
  DATA delete_ok.
  $itab[] = itab[].

  SORT $itab BY employeenumber. " ascending

  DATA  : total_doc_cnt TYPE i,
          current_doc_cnt TYPE i.
  DATA : percentage TYPE p,$mod TYPE i,
         $current_cnt(10),$total_cnt(10),$text(30) .

*  describe table itab lines total_doc_cnt.

*  loop at itab.
*    if itab-inout eq '1' and itab-rdate < check_date.
*      add 1 to total_doc_cnt.
*    endif.
*  endloop.
*
*  $total_cnt = total_doc_cnt.

  percentage = 70.
  PERFORM show_progress USING 'Checking...' percentage.

  LOOP AT itab.

    $ix = sy-tabix.

*    add 1 to current_doc_cnt.
*    $mod = current_doc_cnt mod 100.
*    if $mod eq 0.
*      $current_cnt = current_doc_cnt.
*      concatenate 'Checking...'$current_cnt '/' $total_cnt
*      into $text.
*      condense $text.
*      percentage = current_doc_cnt / total_doc_cnt * 100.
*      perform show_progress using $text percentage.
*    endif.
*
    IF itab-inout EQ '1' AND itab-rdate < check_date.

      READ TABLE $itab WITH KEY employeenumber = itab-employeenumber
                                BINARY SEARCH.
      IF sy-subrc EQ 0.
        $fr = sy-tabix.
        CLEAR delete_ok.
        LOOP AT $itab FROM $fr.
          IF $itab-employeenumber NE itab-employeenumber.
            EXIT.
          ENDIF.
          IF $itab-inout = '0' AND $itab-rdate <= itab-rdate.
            delete_ok = true.
          ENDIF.

          IF $itab-inout = '1' AND $itab-rdate = itab-rdate.
            delete_ok = true.
          ENDIF.

        ENDLOOP.
      ENDIF.
      IF delete_ok = true.
        DELETE itab WHERE employeenumber = itab-employeenumber
                      AND rdate < check_date.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT itab.
    CONCATENATE itab-rdate itab-rtime INTO itab-$str.
    MODIFY itab.
  ENDLOOP.

  SORT itab BY employeenumber ASCENDING
               $str DESCENDING .

  LOOP AT itab.
    $ix = sy-tabix.
    AT NEW employeenumber. "pernr.
      CLEAR $cnt.
    ENDAT.
    ADD 1 TO $cnt.
    itab-cnt = $cnt.
    MODIFY itab INDEX $ix TRANSPORTING cnt.
    IF itab-inout EQ '0'.
      ADD 1 TO $cnt.
    ENDIF.
  ENDLOOP.

  CASE true.

    WHEN p_cas1.
      DELETE itab WHERE cnt > 2.
      SORT itab BY employeenumber ASCENDING
                   $str DESCENDING .

    WHEN p_cas0.
      SORT itab BY employeenumber cnt .

      LOOP AT itab.
        AT NEW employeenumber.
          CONTINUE.
        ENDAT.
        AT END OF employeenumber.
          CONTINUE.
        ENDAT.
        itab-flag = true.
        MODIFY itab INDEX sy-tabix TRANSPORTING flag.
      ENDLOOP.

      DELETE itab WHERE flag EQ true.

      __cls $itab.
      $itab[] = itab[].

      SORT $itab BY employeenumber inout ASCENDING
                    cnt DESCENDING .

      LOOP AT itab.
        $ix = sy-tabix.
        READ TABLE $itab WITH KEY employeenumber = itab-employeenumber
                                    inout = itab-inout BINARY SEARCH.
        IF sy-subrc EQ 0 AND $itab-cnt > itab-cnt.
          DELETE itab INDEX $ix.
        ENDIF.
      ENDLOOP.

  ENDCASE.

ENDFORM.                    " modi_itab
*&---------------------------------------------------------------------*
*&      Form  set_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_row_data USING check_date.

  SORT it_ws BY schkz zeity.

  DATA $ix LIKE sy-tabix.
  DATA $clr.
  DATA $zeity TYPE zeity.

  LOOP AT  it_row_tab.
    $ix = sy-tabix.
    it_row_tab-zcntt = 1.

    PERFORM get_emp_categ  USING    it_row_tab-persg it_row_tab-persk
                           CHANGING it_row_tab-categ.


    IF it_row_tab-categ EQ 'B'.
      $zeity = '1'.
    ELSE.
      $zeity = '2'.
    ENDIF.

    READ TABLE it_ws WITH KEY schkz = it_row_tab-schkz
                              zeity = $zeity
                              BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-rtext = it_ws-rtext.
      it_row_tab-anzsh = it_ws-anzsh.
    ELSE.
      READ TABLE it_ws WITH KEY  schkz = it_row_tab-schkz
                                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-rtext = it_ws-rtext.
        it_row_tab-anzsh = it_ws-anzsh.
      ENDIF.
    ENDIF.

    IF it_row_tab-anzsh IS INITIAL.
    ELSE.
      CASE it_row_tab-anzsh.
        WHEN '1'.
          it_row_tab-anzshtxt = '1ST'.

        WHEN '2'.
          it_row_tab-anzshtxt = '2ND'.

        WHEN '3'.
          it_row_tab-anzshtxt = '3RD'.

      ENDCASE.
    ENDIF.

    IF p_cas0 EQ true AND NOT it_row_tab-rdateo IS INITIAL.
      PERFORM check_time USING check_date
                               it_row_tab-anzsh
                               it_row_tab-rdateo
                               it_row_tab-zclkout
                      CHANGING $clr.
      IF $clr EQ true.
        CLEAR : it_row_tab-rdateo,
                it_row_tab-zdoorido,
                it_row_tab-zdooridot,
                it_row_tab-zclkout.
      ENDIF.
    ENDIF.

    IF p_c1 EQ true.
      IF NOT it_row_tab-rdatei IS INITIAL.
        it_row_tab-zhrlsty = check_date - it_row_tab-rdatei.
      ENDIF.
    ENDIF.

    IF it_row_tab-zclkin IS INITIAL.
      it_row_tab-zuaval = true. " red
      it_row_tab-zclkuava = icon_led_yellow    .
      it_row_tab-zclkava = icon_led_yellow.
* count for Unavailable
      it_row_tab-zcntu = 1.
    ENDIF.

    IF NOT it_row_tab-zclkin IS INITIAL AND
           it_row_tab-zclkout IS INITIAL.
      it_row_tab-zaval = true.
      it_row_tab-zclkava = icon_led_green.
* count for Available
      it_row_tab-zcnta = 1.
    ENDIF.

    IF NOT it_row_tab-zclkout IS INITIAL.
      it_row_tab-zuaval = true. " yellow
      it_row_tab-zclkuava = icon_led_red    .
      it_row_tab-zclkava = icon_led_red.
* count for Unavailable
      it_row_tab-zcntu = 1.
    ENDIF.
    IF it_row_tab-employeenumber IS INITIAL.
      WRITE it_row_tab-pernr TO it_row_tab-employeenumber.
    ENDIF.

    IF it_row_tab-zflgtmp EQ true.
      IF it_row_tab-rdatei IS INITIAL AND
         it_row_tab-rdateo IS INITIAL.
        DELETE  it_row_tab INDEX $ix.
        CONTINUE.
      ENDIF.
      it_row_tab-sname = 'Temp. Employee'.
      it_row_tab-kostl = 'Z'.
    ELSE.
      IF it_row_tab-orgeh IS INITIAL.
        it_row_tab-sname = 'No Org. Assignment'.
      ENDIF.
    ENDIF.

    MODIFY it_row_tab INDEX $ix.
  ENDLOOP.

  CLEAR : g_avail, g_unavail, g_total, g_etc, g_active, g_inactive.

  LOOP AT it_row_tab.
    IF it_row_tab-zcntu EQ 1.
      ADD 1 TO g_unavail.
    ENDIF.
    IF it_row_tab-zcnta EQ 1.
      ADD 1 TO g_avail.
    ENDIF.

    IF it_row_tab-zflgtmp EQ false.
      IF it_row_tab-stat2 EQ '3'.
        ADD 1 TO g_active.
      ELSE.
* by ig.moon{
        IF it_row_tab-kostl EQ '0000033301'.
          CLEAR it_status.
          READ TABLE it_status WITH KEY pernr = it_row_tab-pernr
          BINARY SEARCH.
          IF it_status-massn EQ 'ZX' AND it_status-massg EQ '17'.
            ADD 1 TO g_active.
          ELSE.
            ADD 1 TO g_inactive.
          ENDIF.
        ELSE.
          ADD 1 TO g_inactive.
        ENDIF.
      ENDIF.
* }
    ELSE.
      ADD 1 TO g_etc.
    ENDIF.
  ENDLOOP.

  g_total = g_active + g_inactive + g_etc.

ENDFORM.                    " set_row_data
*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen.

  LOOP AT SCREEN.
    IF screen-group1 = 'PLL'.
      IF p_op0 EQ true.
        screen-input = 1.
      ELSE.
        CLEAR p_all.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 = 'BRI'.
      screen-intensified = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  get_stauts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$DATE  text
*----------------------------------------------------------------------*
FORM get_stauts USING check_date .

  __cls it_status.
  SELECT pernr  begda massn massg stat2 INTO TABLE it_status
  FROM pa0000
  WHERE begda <= check_date.
*    and ( stat2 eq '1' or stat2 eq '3' ).

  SORT it_status BY pernr ASCENDING
                    begda DESCENDING .

  DELETE ADJACENT DUPLICATES FROM it_status
      COMPARING pernr.

ENDFORM.                    " get_stauts
*&---------------------------------------------------------------------*
*&      Form  get_emp_categ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ROW_TAB_PERSG  text
*      -->P_IT_ROW_TAB_PERSK  text
*      <--P_IT_ROW_TAB_CATEG  text
*----------------------------------------------------------------------*
FORM get_emp_categ USING    f_persg
                            f_persk
                   CHANGING f_categ.

*parameters: p_eg1(1)   type c default 'A' no-display,  "US-Salary
*            p_eg2(1)   type c default 'B' no-display,  "US-Wage
*            p_eg3(1)   type c default 'K' no-display.  "KR-Salary

  CONSTANTS:
   c_eg1(1)   TYPE c VALUE   'A',"US-Salary
   c_eg2(1)   TYPE c VALUE   'B',"US-Wage
   c_eg3(1)   TYPE c VALUE   'K'."KR-Salary

  IF f_persg = '9' AND f_persk = 'U2'.
    f_categ = c_eg3.
  ELSEIF ( ( f_persg = '1' AND f_persk = 'U2' ) OR
           ( f_persg = '1' AND f_persk = 'U3' ) ).
    f_categ = c_eg1.
  ELSE.
    f_categ = c_eg2.
  ENDIF.

ENDFORM.                    " get_emp_categ
*&---------------------------------------------------------------------*
*&      Form  check_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHECK_DATE  text
*      -->P_IT_ROW_TAB_ANZSH  text
*      -->P_IT_ROW_TAB_RDATEO  text
*      -->P_IT_ROW_TAB_ZCLKOUT  text
*      <--P_$CLR  text
*----------------------------------------------------------------------*
FORM check_time USING    p_check_date
                         p_anzsh
                         p_rdateo
                         p_zclkout
                CHANGING p_clr.

  DATA : start_date TYPE datum,
         end_date TYPE datum,
         start_time TYPE zclkout,
         end_time TYPE zclkout.


  DATA : $start(20),
         $end(20),
         $out(20),
         $gap TYPE i.

*M-F 06:30-15:15
*M-F 17:15-02:00
*M-F 22:00-06:45

  CLEAR p_clr.

  CASE p_anzsh.
    WHEN '1'.

      start_date = end_date = p_check_date.
      start_time = '063000'.
      end_time   = '151500'.

    WHEN '2'.

      start_date = p_check_date - 1.
      end_date = p_check_date.

      start_time = '171500'.
      end_time   = '020000'.

    WHEN '3'.

      start_date = p_check_date - 1.
      end_date = p_check_date.

      start_time = '220000'.
      end_time   = '064500'.

  ENDCASE.

  CONCATENATE start_date start_time INTO $start.
  CONCATENATE end_date end_time INTO $end.
  CONCATENATE p_rdateo p_zclkout INTO $out.

  IF $out BETWEEN $start AND $end.
    $gap = $end - $out.

    IF $gap < 10000.
    ELSE.
      p_clr = true.
    ENDIF.
  ENDIF.

ENDFORM.                    " check_time
