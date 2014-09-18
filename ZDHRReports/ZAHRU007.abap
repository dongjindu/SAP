*----------------------------------------------------------------------
* Program ID        : ZAHRU007
* Title             : [HR] Team Member Availability Report Rel 2
* Created on        : 4/22/2009
* Created by        : IG.MOON
* Specifications By : EUNA LEE
* Description       : Team Member Availablity Report Rel 2
*&--------------------------------------------------------------------&*
* Modification Logs
* Date       Developer Issue No    Description
* 03/09/2010 VALERIAN     Add option to select data based on date range
*            HIS20094     instead of only single day
* 08/13/2012 Valerian  UD1K955391  Correct Missing Entries in TM
*                                  Availability Report by fixing
*                                  sorting criteria
* 08/13/2012 Valerian  UD1K955395  Fix another sorting crit. as above
* 08/21/2012 Valerian  UD1K955457  Apply new logic to get the shft code
* 06/19/2013  T00303   UD1K957425  U1: Apply Archiving
*&--------------------------------------------------------------------&*
REPORT zahru007 MESSAGE-ID zmco .

TABLES : t001k, pa0001,pa0007,
         zthr_bhisthmma,
         zshrclockin ,sscrfields, t526, t508s,
         zthrclkauth,t501,t503k.

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
PARAMETERS p_date LIKE zthr_bhisthmma-rdate
DEFAULT sy-datum.

SELECTION-SCREEN END OF BLOCK bl.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS s_pernr FOR pa0001-pernr MATCHCODE OBJECT prem
NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b2.


*SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 10(15)  text-xe4 FOR FIELD p_nto.
*SELECTION-SCREEN POSITION 28.
*PARAMETERS p_nto   RADIOBUTTON GROUP rads USER-COMMAND mcom.
*SELECTION-SCREEN COMMENT 44(15)  text-xe5  FOR FIELD p_os1.
*SELECTION-SCREEN POSITION 60.
*PARAMETERS p_os1   RADIOBUTTON GROUP rads.
*SELECTION-SCREEN END   OF LINE.
*
*SELECTION-SCREEN SKIP 1.

*///////////////////////////////////////////////////////////*
*///////////////////////////////////////////////////////////* Type

SELECTION-SCREEN BEGIN OF BLOCK bxt WITH FRAME TITLE text-031.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 31.
PARAMETERS p_all RADIOBUTTON GROUP rdt MODIF ID cri.
SELECTION-SCREEN COMMENT 40(26)  text-x11 FOR FIELD p_all MODIF ID cri.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 31.
PARAMETERS p_one RADIOBUTTON GROUP rdt MODIF ID cri.
SELECTION-SCREEN COMMENT 40(27)  text-x10 FOR FIELD p_one MODIF ID cri.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 31.
PARAMETERS p_ont RADIOBUTTON GROUP rdt MODIF ID cri.
SELECTION-SCREEN COMMENT 40(27)  text-x12  FOR FIELD p_ont MODIF ID cri.
"pll.
SELECTION-SCREEN END   OF LINE.

SELECT-OPTIONS s_tmpemp FOR zthr_bhisthmma-employeenumber
NO INTERVALS MODIF ID cri.

SELECTION-SCREEN END OF BLOCK bxt. " block for Type
*///////////////////////////////////////////////////////////*
*SELECTION-SCREEN END OF BLOCK b3.
*///////////////////////////////////////////////////////////* Group

SELECTION-SCREEN BEGIN OF BLOCK bxx WITH FRAME TITLE text-003.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16)  text-x02 MODIF ID opt.
*SELECTION-SCREEN POSITION 28.
*PARAMETERS p_op1   RADIOBUTTON GROUP radi
*                        DEFAULT 'X' MODIF ID opt.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS s_kostl FOR pa0001-kostl NO INTERVALS
                                 MODIF ID opt.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16)  text-x03 MODIF ID opt.
*SELECTION-SCREEN POSITION 28.
*PARAMETERS p_op2   RADIOBUTTON GROUP radi MODIF ID opt.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS s_orgeh FOR pa0001-orgeh NO INTERVALS
                                 MODIF ID opt.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18)  text-x04 MODIF ID opt.
*SELECTION-SCREEN POSITION 28.
*PARAMETERS p_op3   RADIOBUTTON GROUP radi MODIF ID opt..
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS s_tmcode  FOR t526-sachx NO INTERVALS
                                 MODIF ID opt..
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18)  text-x08  MODIF ID opt.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS s_persg  FOR t501-persg NO INTERVALS
                                 MODIF ID opt..
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18)  text-x07  MODIF ID opt.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS s_persk  FOR t503k-persk NO INTERVALS
                                 MODIF ID opt..
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18)  text-x06  MODIF ID opt.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS s_stell  FOR pa0001-stell NO INTERVALS
                                 MODIF ID opt..
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN END OF BLOCK bxx. " block for group

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
SELECT-OPTIONS s_schkz FOR pa0007-schkz NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME.
PARAMETER  p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b5.

*START HIS20094-Date range selection screen
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS: s_date FOR zthr_bhisthmma-rdate.
PARAMETERS p_runp AS CHECKBOX.
*END HIS20094
*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End
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
TYPES: kdate TYPE sy-datum.            "HIS20094-Additional Column
TYPES: END OF ty_out.

RANGES r_employeenumber FOR zthr_bhisthmma-employeenumber.

DATA: BEGIN OF itab OCCURS 0,
          employeenumber LIKE zthr_bhisthmma-employeenumber,
          pernr LIKE pa0001-pernr,
          cnt TYPE i,
          readerid LIKE zthr_bhisthmma-readerid,
          rdate LIKE zthr_bhisthmma-rdate,
          rtime LIKE zthr_bhisthmma-rtime,
          rtimen(6) TYPE n,
          inout,
          $str(20),
          door_desc LIKE zthrdoor-zhdrds,
          badge LIKE zthr_bhisthmma-badge,
          flag(1),
      END OF itab.

DATA: BEGIN OF i_attn OCCURS 0,
          pernr LIKE pa0001-pernr,
          zhere LIKE zthrattncor-zhere,
          manupd LIKE zthrattncor-manupd,
      END OF i_attn.

DATA  itab_for_2nd LIKE itab OCCURS 0 WITH HEADER LINE.
DATA  itab_for_1st LIKE itab OCCURS 0 WITH HEADER LINE.

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

DATA  g_option.

DATA : date_low LIKE sy-datum,
       date_high LIKE sy-datum,
       $times TYPE i,
       $date LIKE sy-datum .

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
        btrtl  LIKE pa0001-btrtl,                           "UD1K955457
END   OF it_pernr.

DATA i_not_out LIKE it_pernr OCCURS 0 WITH HEADER LINE.

DATA  it_pr_tmcode LIKE it_pernr OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_new_pernr OCCURS 0,
        pernr   LIKE   pa0001-pernr,
      END   OF it_new_pernr.
DATA  $it_pernr LIKE it_pernr OCCURS 0 WITH HEADER LINE.
DATA $cnt TYPE i.
DATA  tmrrw_date TYPE datum.

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
        schkz	TYPE schkn,
        zeity TYPE zeity,
        rtext	TYPE retext,
        mosid type mosid,
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
DATA $ix TYPE i.

DATA $tprog TYPE  tprog.

*BEGIN HIS20094-Store date range
DATA: BEGIN OF t_rdate OCCURS 0,
        rdate TYPE zthr_bhisthmma-rdate,
      END OF t_rdate.
*END HIS20094

*- U1 Start
DATA: gt_zthrattncor_a TYPE TABLE OF zthrattncor WITH HEADER LINE,
      gt_zthr_bhisthmma_a TYPE TABLE OF zthr_bhisthmma WITH HEADER LINE.
*- U1 End

INCLUDE <icon>.                        " icon
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  sy-title = '[HR] Team Member Availability Report'.

*  IF g_option IS INITIAL.
*    p_nto = true.
*  ELSE.
*    IF g_option EQ 'C'.
*      p_nto = true.
*    ELSE.
*      p_os1 = true.
*    ENDIF.
*  ENDIF.

  CHECK sy-uname NE : 'HIS20036' , '100553' , 'HIS20065', 'HIS20037'.
  .
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

*BEGIN HIS20094-Check if date range selection is selected
  IF NOT p_runp IS INITIAL.
    IF s_date[] IS INITIAL.
      MESSAGE e000 WITH 'Please Enter Period'.
    ENDIF.
  ELSE.
    IF p_date IS INITIAL.
      MESSAGE e000 WITH 'Please Enter Date'.
    ENDIF.
  ENDIF.
*END HIS20094

*  CASE sscrfields-ucomm.
*    WHEN 'UCOM'.
*      CASE true.
*        WHEN p_l1.
*          p_vari = space.
*        WHEN p_l2.
*          p_vari = '/TOTAL_CC'.
*        WHEN p_l3.
*          p_vari = '/TOTAL_ORG'.
*        WHEN p_l4.
*          p_vari = '/TOTAL_ADM'.
*      ENDCASE.
*  ENDCASE.

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

  PERFORM initialize.
  PERFORM get_info.

  CHECK g_error = false.

  $date = p_date.

*BEGIN HIS20094-Get date from security data
  __cls : t_rdate.
  IF p_runp IS INITIAL.
    t_rdate = p_date.
    APPEND t_rdate.
  ELSE.
    SELECT DISTINCT rdate
      INTO TABLE t_rdate
      FROM zthr_bhisthmma
     WHERE rdate IN s_date.

*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_zthr_bhisthmma.
    ENDIF.
*- U1 End
  ENDIF.
*END HIS20094

*BEGIN HIS20094-Process for all relevant date
  LOOP AT t_rdate.
    $date = t_rdate-rdate.

    __cls : it_row_tab.

    PERFORM : get_stauts USING $date,
              get_row_data USING $date,
              set_row_data USING $date,
              refine_row_itab.
  ENDLOOP.
*END HIS20094

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

**S> 08/04/11 Paul
  PERFORM show_progress     USING 'Preparing screen...' '95'.
**E<
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
    gs_fieldcat-lowercase     = true.
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
    ' '  'KDATE'    'Check Date'      10  'DATS' '' '',     "HIS20094
    ' '  'ANZSHTXT' 'Sft'              3  'CHAR' '' '',
    ' '  'SCHKZ'    'Wrk.Schdl'        8  'CHAR' '' '',
    ' '  'RTEXT'    'WrS Schdl Desc.'  20 'CHAR' '' '',
    ' '  'ZCLKAVA'  'Av.'              4  'ICON' '' '',
    ' '  'ZHEREICON' 'Avail'           4  'ICON' '' '',
*    ' '  'ZHRLSTY'  'C'                1  'CHAR' '' '',
    ' '  'ZDOORIDI'  'Door In'         5  'NUMC' '' '',
    ' '  'ZDOORIDIT' 'Door In Text'   20  'CHAR' '' '',
    ' '  'RDATEI'   'In Date'          8  'DATS' '' '',
    ' '  'ZCLKIN'   'Clock In'         6  'TIMS' '' '',
    ' '  'ZDOORIDO'  'Door Out'        5  'NUMC' '' '',
    ' '  'ZDOORIDOT'  'Door Out Text' 20  'CHAR' '' '',
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
            'EMPLOYEENUMBER'   ' ' 'X' 'X' 'X',
            'SNAME'   ' ' 'X' 'X' 'X'. "HIS20094-Add additional sort

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

  IF p_runp IS INITIAL.                "HIS20094-Original Header
    PERFORM set_header_line USING :
            'D' 'S' 'Date' p_date      ''.

*BEGIN HIS20094-Compose header for selection by date range
  ELSE.
    DATA: ls_line  TYPE slis_listheader,
          l_ldate(10),
          l_hdate(10).

    MOVE  : 'S'    TO ls_line-typ,
            'Date' TO ls_line-key.

    LOOP AT s_date.
      WRITE : s_date-low  TO l_ldate,
              s_date-high TO l_hdate.

      IF s_date-high IS INITIAL.
        IF NOT ls_line-info IS INITIAL.
          CONCATENATE ls_line-info ',' l_ldate INTO ls_line-info
                                             SEPARATED BY space.
        ELSE.
          ls_line-info = l_ldate.
        ENDIF.
      ELSE.
        IF NOT ls_line-info IS INITIAL.
          CONCATENATE ls_line-info ',' l_ldate '~' l_hdate
                                         INTO ls_line-info
                                        SEPARATED BY space.
        ELSE.
          CONCATENATE l_ldate '~' l_hdate INTO ls_line-info
                                        SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.

    APPEND ls_line TO gt_listheader.
  ENDIF.
* END HIS20094

**  IF p_nto EQ true.
*  PERFORM set_header_line USING :
*          'S' 'S' 'Employee #'   s_pernr-low ' '.
**  ENDIF.
**  IF p_op1 EQ true.
*  PERFORM set_header_line USING :
*          'S' 'S' 'Cost Cnter'   s_kostl-low ' '.
**  ENDIF.
*
**  IF p_op2 EQ true.
*  PERFORM set_header_line USING :
*            'S' 'S' 'Org.
*                          unit '   s_orgeh-low ' '.
**  ENDIF.
*
**  IF p_op3 EQ true.
*  PERFORM set_header_line USING :
*            'S' 'S' 'Adm.
*                          code '   s_tmcode-low ' '.
**  ENDIF.
*
*  PERFORM set_header_line USING :
*          'S' 'S' 'Wrk.
*                        scdl '   s_schkz-low ' '.
*
*  WRITE: g_total TO $g_total RIGHT-JUSTIFIED,
*         g_avail TO $g_avail  RIGHT-JUSTIFIED,
*         g_unavail TO $g_unavail RIGHT-JUSTIFIED,
*         g_active TO $g_active RIGHT-JUSTIFIED,
*         g_inactive TO $g_inactive RIGHT-JUSTIFIED,
*         g_etc TO $g_etc RIGHT-JUSTIFIED.
*
*  PERFORM set_header_line USING :
*          'P' 'S' '' '-----------------' ''.
*
*  DATA $string(60).
*  DATA $string_l(120).
*  CONCATENATE $g_inactive '/' $g_active '/' $g_etc INTO $string.
*  CONDENSE $string.
*  CONCATENATE $g_total '(' $string ')' INTO $string_l.
*  CONDENSE $string_l.
*
*  PERFORM set_header_line USING :
*            'P' 'S' 'Total in S.Criteria'  $g_total ''.
*
*  PERFORM set_header_line USING :
*            'P' 'S' 'Available'  $g_avail '',
*            'P' 'S' 'Unavailable'  $g_unavail ''.
*
*  PERFORM set_header_line USING :
*            'P' 'S' 'Inactive/Active/Tmp.'  $string ''.

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

    IF gs_fieldcat-fieldname EQ 'RDATE'.
      gs_fieldcat-no_out = true.
    ENDIF.
    IF gs_fieldcat-fieldname NE 'SCHKZ'.
      gs_fieldcat-ref_tabname = 'ZSHRCLOCKIN'.
    ENDIF.
    IF gs_fieldcat-fieldname EQ 'ZHEREICON'.
      gs_fieldcat-seltext_l = 'Avail'.
      gs_fieldcat-seltext_m = 'Avail'.
      gs_fieldcat-seltext_s = 'Av.'..
      gs_fieldcat-ref_tabname = 'ZSHRCLOCKIN'.
    ENDIF.

    MODIFY pt_fieldcat FROM gs_fieldcat.

  ENDLOOP.

ENDFORM.                    " CHANGE_FIELDCAT
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

*  IF p_nto EQ true.
*    g_option = 'C'.
*  ELSE.
*    g_option = 'G'.
*  ENDIF.
*
  CLEAR : g_error.

*  IF p_op3 EQ true.
*    IF s_tmcode[] IS INITIAL.
*      MESSAGE s000 WITH 'Please enter Adm Code!'.
*      g_error = true.
*      STOP.
*    ENDIF.
  __cls t_t526.

  w_sachn = '%NO LONGER VALID%'.
  SELECT sachx sachn
               FROM t526
               INTO TABLE t_t526
               WHERE sachx IN s_tmcode AND
                     sachn NOT LIKE w_sachn.
*  ENDIF.

  __cls : it_row_tab.

ENDFORM.                    " initialize

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
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen.

  LOOP AT SCREEN.

*    IF screen-group1 = 'CRI'.
*      IF p_nto EQ true.
*        screen-active  = 1.
*      ELSE.
*        screen-active  = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-group1 = 'OPT'.
*      IF p_os1 EQ true.
*        screen-active  = 1.
*      ELSE.
*        screen-active  = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDIF.

    IF screen-group1 = 'BRI'.
      screen-intensified = 1.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " modify_screen
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
*&      Form  get_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info.

  SELECT kostl ktext INTO TABLE it_kostx FROM cskt
  WHERE spras EQ sy-langu
    AND datbi EQ '99991231'.

  SELECT orgeh orgtx INTO TABLE it_orgtx FROM t527x
  WHERE sprsl EQ sy-langu.

  SELECT zhdoor zhdrio zhdrds INTO TABLE it_door FROM zthrdoor
  WHERE zhdrcns EQ true.

  SELECT  schkz zeity rtext mosid INTO TABLE it_ws
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
      WHEN '0002' OR '1003' OR '1002' OR '1009'
           OR '2006' .
        it_ws-anzsh = '2'.
      WHEN OTHERS.
        it_ws-anzsh = '1'.
    ENDCASE.
    MODIFY it_ws TRANSPORTING tprog anzsh WHERE schkz = it_ws-schkz.

  ENDLOOP.

  SORT : it_kostx BY kostl,
         it_orgtx BY orgeh,
         it_door BY zhdoor,
         it_ws BY tprog.

ENDFORM.                    " get_info
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

  SORT it_status BY pernr ASCENDING
                    begda DESCENDING .

  DELETE ADJACENT DUPLICATES FROM it_status
      COMPARING pernr.

ENDFORM.                    " get_stauts
*&---------------------------------------------------------------------*
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$DATE  text
*----------------------------------------------------------------------*
FORM get_row_data USING check_date.

  __cls : it_pernr, r_employeenumber, itab, it_pernr_2nd.

  PERFORM show_progress USING 'Gather Data...' 30.
  PERFORM get_pernr USING check_date.
  PERFORM get_history TABLES itab
                       USING check_date.

  READ TABLE it_pernr_2nd INDEX 1.
  IF sy-subrc EQ 0.
    tmrrw_date = check_date + 1.
    PERFORM get_history_for_2nd TABLES itab_for_2nd
                        USING tmrrw_date.
  ENDIF.

  PERFORM modi_itab USING check_date false.

  PERFORM get_add_out_history_for_1st USING check_date.

  PERFORM get_from_attn_corr.

  PERFORM final_refine USING check_date.

ENDFORM.                    " get_row_data
*&---------------------------------------------------------------------*
*&      Form  modi_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHECK_DATE  text
*----------------------------------------------------------------------*
FORM modi_itab USING check_date flag_for_1st.

  DATA $ix LIKE sy-tabix.
  DATA $cnt TYPE i.
  DATA $itab LIKE itab OCCURS 0 WITH HEADER LINE.
  DATA $fr LIKE sy-tabix.
  DATA delete_ok.
  DATA  : total_doc_cnt TYPE i,
          current_doc_cnt TYPE i.
  DATA : percentage TYPE p,$mod TYPE i,
         $current_cnt(10),$total_cnt(10),$text(30) .

  IF flag_for_1st EQ false.
    APPEND LINES OF itab_for_2nd TO itab.
    DELETE itab WHERE rdate NE check_date AND rtimen IS INITIAL.
  ENDIF.

  LOOP AT itab.

    $ix = sy-tabix.

    itab-pernr = itab-employeenumber.

    READ TABLE it_door WITH KEY zhdoor = itab-readerid
                            BINARY SEARCH.
    IF sy-subrc EQ 0.

      IF it_door-zhdrio EQ '1'.
        itab-inout = 'o'.
      ELSE.
        itab-inout = 'i'.
      ENDIF.

      itab-door_desc = it_door-zhdrds.

    ENDIF.

    IF itab-employeenumber IS INITIAL.
      READ TABLE gt_emp WITH KEY badge = itab-badge BINARY SEARCH.
      IF sy-subrc EQ 0.
        itab-employeenumber = gt_emp-employeenumber.
      ENDIF.

    ENDIF.

    MODIFY itab INDEX $ix TRANSPORTING pernr inout door_desc
    employeenumber.

  ENDLOOP.

  DELETE itab WHERE employeenumber EQ space.
  DELETE itab WHERE inout EQ space.

  LOOP AT itab.
    CONCATENATE itab-rdate itab-rtimen INTO itab-$str.
    MODIFY itab.
  ENDLOOP.

  SORT itab BY employeenumber ASCENDING
               $str DESCENDING
               inout DESCENDING.                            "UD1K955395

  $itab[] = itab[].

  SORT $itab BY employeenumber. " ascending

  percentage = 70.
  PERFORM show_progress USING 'Checking...' percentage.

  DATA : $ldate TYPE datum,
         $ltime TYPE uzeit,
         $lemployeenumber LIKE itab-employeenumber.

  LOOP AT itab.

    $ix = sy-tabix.
    AT NEW employeenumber.
      $flag = true.
      $lemployeenumber = itab-employeenumber.
      CLEAR : $ldate,$ltime .
    ENDAT.

    CHECK itab-employeenumber EQ $lemployeenumber.

    IF itab-inout EQ 'i'.
      IF $ldate IS INITIAL.
        $ldate = itab-rdate.
        $ltime = itab-rtime.
      ENDIF.
    ELSE.
      IF $ldate <= itab-rdate AND
         $ltime >= itab-rtime AND
         NOT $ldate IS INITIAL.
        DELETE itab INDEX $ix.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT itab.

    $ix = sy-tabix.

    IF itab-inout EQ 'o' AND itab-rdate < check_date.

      READ TABLE $itab WITH KEY employeenumber = itab-employeenumber
                                BINARY SEARCH.
      IF sy-subrc EQ 0.
        $fr = sy-tabix.
        CLEAR delete_ok.
        LOOP AT $itab FROM $fr.
          IF $itab-employeenumber NE itab-employeenumber.
            EXIT.
          ENDIF.
          IF $itab-inout = 'i' AND $itab-rdate <= itab-rdate.
            delete_ok = true.
          ENDIF.

          IF $itab-inout = 'o' AND $itab-rdate = itab-rdate.
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
    CONCATENATE itab-rdate itab-rtimen INTO itab-$str.
    MODIFY itab.
  ENDLOOP.

  SORT itab BY employeenumber ASCENDING
               $str DESCENDING
               inout DESCENDING.                            "UD1K955391

  IF flag_for_1st EQ false.
    LOOP AT itab.
      $ix = sy-tabix.
      READ TABLE it_pernr_2nd WITH KEY
              employeenumber = itab-employeenumber BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF itab-rdate EQ check_date AND
            itab-inout EQ 'o' AND
            itab-rtime < '120000' AND
            itab-rtimen < '240000'.

          DELETE itab INDEX $ix.
          CONTINUE.
        ENDIF.
        IF itab-rdate EQ check_date AND
            itab-inout EQ 'i' AND
            itab-rtime < '120000' AND
            itab-rtimen < '240000'.
          DELETE itab INDEX $ix.
          CONTINUE.
        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT itab.
    $ix = sy-tabix.
    AT NEW employeenumber. "pernr.
      CLEAR $cnt.
    ENDAT.
    ADD 1 TO $cnt.
    itab-cnt = $cnt.
    MODIFY itab INDEX $ix TRANSPORTING cnt.
    IF itab-inout EQ 'i'.
      ADD 1 TO $cnt.
    ENDIF.
  ENDLOOP.

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

  LOOP AT itab.
    $ix = sy-tabix.
    IF itab-rtimen > '240000'.
      itab-rdate = itab-rdate + 1.
      MODIFY itab INDEX $ix TRANSPORTING rdate.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " modi_itab
*&---------------------------------------------------------------------*
*&      Form  refine_row_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refine_row_itab.

  PERFORM show_progress USING 'Refine Data...' 90.

*  __cls gt_out.                       "HIS20094-Append subsequent data

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

    READ TABLE i_attn WITH KEY pernr = it_row_tab-pernr
                      BINARY SEARCH.

*    IF sy-subrc EQ 0 AND i_attn-zhere EQ true.
*      gt_out-zhereicon = icon_led_green.
*    ELSE.
*      gt_out-zhereicon = icon_led_red.
*    ENDIF.

    IF sy-subrc EQ 0 AND i_attn-manupd EQ true.
      gt_out-zhereicon = icon_checked.
    ENDIF.

    IF NOT gt_out-zclkout IS INITIAL
      AND gt_out-zclkin IS INITIAL.
      CLEAR : gt_out-rdateo,
              gt_out-zdoorido,
              gt_out-zdooridot,
              gt_out-zclkout.
      gt_out-zclkava = icon_led_yellow.
      gt_out-zclkuava = icon_led_yellow.
    ENDIF.

    gt_out-kdate = $date.              "HIS20094-Insert Check Date

    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " refine_row_itab
*&---------------------------------------------------------------------*
*&      Form  set_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$DATE  text
*----------------------------------------------------------------------*
FORM set_row_data USING check_date.
* BEGIN OF UD1K955457
  DATA: l_tprog   TYPE tprog,
        $mosid(2) TYPE n.
* END OF UD1K955457

  SORT it_ws BY schkz zeity.

  DATA $ix LIKE sy-tabix.
  DATA $clr.
  DATA $zeity TYPE zeity.

  PERFORM show_progress USING 'Set Data...' 60.

** 02/06/14 add mosid (
  SORT it_ws BY schkz mosid zeity.
*  SORT it_ws BY schkz zeity.
** )
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


** 02/06/14 add subarea group as search condition(
      CLEAR: it_pernr-btrtl, $mosid.
      READ TABLE it_pernr WITH KEY pernr = it_row_tab-pernr
                               BINARY SEARCH.
      CASE it_pernr-btrtl.
        WHEN '0004' OR '0005' OR '0001' OR '0002'.
          $mosid = '10'.
        WHEN '0003'.
          $mosid = '9'.
        WHEN '0006'.
          $mosid = '8'.
        WHEN OTHERS.
      ENDCASE.

    READ TABLE it_ws WITH KEY schkz = it_row_tab-schkz
                              mosid = $mosid
                              zeity = $zeity
                              BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-rtext = it_ws-rtext.
    ENDIF.

*    READ TABLE it_ws WITH KEY schkz = it_row_tab-schkz
*                              zeity = $zeity
*                              BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      it_row_tab-rtext = it_ws-rtext.
*      it_row_tab-anzsh = it_ws-anzsh.
*    ELSE.
*      READ TABLE it_ws WITH KEY  schkz = it_row_tab-schkz
*                                 BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        it_row_tab-rtext = it_ws-rtext.
*        it_row_tab-anzsh = it_ws-anzsh.
*      ENDIF.
*    ENDIF.
** )

* BEGIN OF UD1K955457
* Overwrite Shift code with new logic
    CLEAR it_row_tab-anzsh.
    IF it_row_tab-schkz = '8000_A' OR
       it_row_tab-schkz = '8000_B' OR
       it_row_tab-schkz = '8000_C'.

      CALL FUNCTION 'Z_CO_GET_DWS_IG'
        EXPORTING
          schkz                          = it_row_tab-schkz
          datum                          = it_row_tab-rdate
        IMPORTING
          tprog                          = l_tprog
        EXCEPTIONS
          not_found_work_schedule_rules  = 1
          invalid_date                   = 2
          not_found_period_work_schedule = 3
          OTHERS                         = 4.

      CASE l_tprog.

        WHEN '0002' OR '1003' OR '1002' OR '1009' OR '2006'.

          it_row_tab-anzsh = '2'.

        WHEN OTHERS.

          it_row_tab-anzsh = '1'.

      ENDCASE.

    ELSE.

** 02/06/14 (
*      CLEAR it_pernr-btrtl.
*      READ TABLE it_pernr WITH KEY pernr = it_row_tab-pernr
*                               BINARY SEARCH.
*      CASE it_pernr-btrtl.
*        WHEN '0004' OR '0005' OR '0001' OR '0002'.
*          $mosid = '10'.
*        WHEN '0003'.
*          $mosid = '9'.
*        WHEN '0006'.
*          $mosid = '8'.
*        WHEN OTHERS.
*      ENDCASE.
** )
      SELECT SINGLE anzsh INTO it_row_tab-anzsh
         FROM ztco_mh_ws WHERE kokrs EQ 'H201'
                         AND   mosid EQ $mosid
                         AND   schkz EQ it_row_tab-schkz.

    ENDIF.
* END OF UD1K955457

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

*    IF NOT it_row_tab-rdateo IS INITIAL.
*      PERFORM check_time USING check_date
*                               it_row_tab-anzsh
*                               it_row_tab-rdateo
*                               it_row_tab-zclkout
*                      CHANGING $clr.
*      IF $clr EQ true.
*        CLEAR : it_row_tab-rdateo,
*                it_row_tab-zdoorido,
*                it_row_tab-zdooridot,
*                it_row_tab-zclkout.
*      ENDIF.
*    ENDIF.

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
*        IF it_row_tab-kostl EQ '0000033301'.
*
*          CLEAR it_status.
*          READ TABLE it_status WITH KEY pernr = it_row_tab-pernr
*          BINARY SEARCH.
*          IF it_status-massn EQ 'ZX' AND it_status-massg EQ '17'.
*            ADD 1 TO g_active.
*          ELSE.
*            ADD 1 TO g_inactive.
*          ENDIF.
*        ELSE.
*          ADD 1 TO g_inactive.
*        ENDIF.

      ENDIF.
* }
    ELSE.
      ADD 1 TO g_etc.
    ENDIF.
  ENDLOOP.

  g_total = g_active + g_inactive + g_etc.

ENDFORM.                    " set_row_data
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
*&---------------------------------------------------------------------*
*&      Form  get_history
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHECK_DATE  text
*----------------------------------------------------------------------*
FORM get_history TABLES p_tab STRUCTURE itab
                 USING p_check_date.

  IF NOT r_employeenumber[] IS INITIAL.

    SELECT employeenumber readerid rdate rtime badge
    FROM zthr_bhisthmma
      INTO CORRESPONDING FIELDS OF TABLE p_tab
      WHERE rdate EQ p_check_date
        AND employeenumber IN r_employeenumber
        %_HINTS ORACLE 'FIRST_ROWS(10)'.

*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_zthr_bhisthmma_2 TABLES p_tab
                                             USING p_check_date.
    ENDIF.
*- U1 End

  ELSE.

    IF it_pernr[] IS INITIAL.
      g_error = true.
      MESSAGE s000 WITH 'No data has been found!'.
    ELSE.

      PERFORM get_history_from_table TABLES p_tab
                                            it_pernr
                                      USING p_check_date false .


    ENDIF.
  ENDIF.

  SORT it_ws BY tprog.
  SORT p_tab BY rdate employeenumber.
  SORT it_pernr BY employeenumber.

  LOOP AT p_tab.

    AT NEW employeenumber.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    CLEAR $flag.

    READ TABLE it_pernr WITH KEY
                employeenumber = p_tab-employeenumber
                BINARY SEARCH.

    IF sy-subrc EQ 0.
      CALL FUNCTION 'Z_CO_GET_DWS_IG'
        EXPORTING
          schkz                          = it_pernr-schkz
          datum                          = p_check_date
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

  LOOP AT  p_tab.

    $ix = sy-tabix.

    PERFORM get_door_type USING p_tab-readerid
                       CHANGING p_tab-inout.

    IF p_tab-inout IS INITIAL.
      DELETE p_tab INDEX $ix.
      CONTINUE.
    ENDIF.

    READ TABLE it_pernr_2nd WITH KEY
            employeenumber = p_tab-employeenumber BINARY SEARCH.

    IF sy-subrc EQ 0.
*      IF p_tab-rtime <= '120000'.
*        DELETE p_tab INDEX $ix.
*        CONTINUE.
*      ENDIF.
    ENDIF.

    p_tab-rtimen = p_tab-rtime.
    MODIFY p_tab INDEX $ix TRANSPORTING rtimen inout.
  ENDLOOP.

ENDFORM.                    " get_history
*&---------------------------------------------------------------------*
*&      Form  supplement_pernr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM supplement_pernr USING p_check_date.

*  IF p_nto EQ true.
*
*    IF ( p_all EQ true OR p_ont EQ true ) AND s_pernr[] IS INITIAL.
*
*      SELECT employeenumber
*      FROM zthr_bhisthmma AS a
*        INNER JOIN zthrdoor AS b
*        ON b~zhdoor EQ a~readerid
*       AND b~zhdrcns EQ true
*        APPENDING CORRESPONDING FIELDS OF TABLE it_pernr
*        WHERE rdate EQ p_check_date
*        AND employeenumber NE space
*        %_HINTS ORACLE 'FIRST_ROWS(10)'.
*
*      SORT it_pernr BY employeenumber ASCENDING
*                       sname DESCENDING.
*
*      LOOP AT it_pernr.
*        $ix  = sy-tabix.
*        CHECK it_pernr-pernr IS INITIAL.
*        CHECK NOT it_pernr-employeenumber IN s_tmpemp.
*        DELETE it_pernr INDEX $ix.
*      ENDLOOP.
*
*      DELETE ADJACENT DUPLICATES FROM it_pernr COMPARING
*           employeenumber .
*    ENDIF.
*
*  ENDIF.

  IF ( p_all EQ true OR p_ont EQ true ) AND s_pernr[] IS INITIAL.

    SELECT employeenumber
    FROM zthr_bhisthmma AS a
      INNER JOIN zthrdoor AS b
      ON b~zhdoor EQ a~readerid
     AND b~zhdrcns EQ true
      APPENDING CORRESPONDING FIELDS OF TABLE it_pernr
      WHERE rdate EQ p_check_date
      AND employeenumber NE space
      %_HINTS ORACLE 'FIRST_ROWS(10)'.

*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_zthr_bhisthmma_4 USING p_check_date.
    ENDIF.
*- U1 End

  ENDIF.

ENDFORM.                    " supplement_pernr
*&---------------------------------------------------------------------*
*&      Form  get_history_for_2nd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB_FOR_2ND  text
*      -->P_TMRRW_DATE  text
*----------------------------------------------------------------------*
FORM get_history_for_2nd TABLES   p_tab STRUCTURE itab
                         USING    p_check_date.
  DATA : $hour TYPE i,
         $min  TYPE i,
         $sec  TYPE i,
         $time_sec TYPE i,
         $result_all TYPE p DECIMALS 4.

  DATA $string(20).

  __cls p_tab.

  SELECT employeenumber readerid rdate rtime badge
    FROM zthr_bhisthmma
      INTO CORRESPONDING FIELDS OF TABLE p_tab
      FOR ALL ENTRIES IN it_pernr_2nd
      WHERE employeenumber EQ it_pernr_2nd-employeenumber
        AND rdate EQ p_check_date
*        AND rtime LE '063000'
      %_HINTS ORACLE 'FIRST_ROWS(10)'.

  LOOP AT  p_tab.
    $ix = sy-tabix.
    p_tab-pernr = p_tab-employeenumber.
    MODIFY p_tab INDEX $ix TRANSPORTING pernr.
  ENDLOOP.

  SORT it_ws BY schkz zeity.

  DATA $end_time(20).
  DATA $start_chr LIKE sy-fdpos.
  DATA $pos LIKE sy-fdpos.
  DATA $strlen TYPE i.

  LOOP AT  p_tab.
    $ix = sy-tabix.
    PERFORM get_door_type USING p_tab-readerid
                       CHANGING p_tab-inout.


    IF p_tab-inout IS INITIAL.
      DELETE p_tab INDEX $ix.
      CONTINUE.
    ENDIF.

    IF p_tab-inout EQ 'o'.

      $hour =  p_tab-rtime(2).
      $min  =  p_tab-rtime+2(2).
      $sec  =  p_tab-rtime+4(2).
      $time_sec = ( $hour * 3600 ) + ( $min * 60 ) + $sec .
      $result_all = ( $time_sec + 86400 ) / 3600.
      $string = $result_all.
      SHIFT $string  : RIGHT UP TO '.',  RIGHT.
      $hour = $string.
      WRITE $hour TO p_tab-rtimen(2).
      WRITE p_tab-rtime+2(4) TO p_tab-rtimen+2.

      p_tab-rdate = p_tab-rdate - 1.

    ENDIF.

    MODIFY p_tab INDEX $ix TRANSPORTING rdate rtimen inout.
  ENDLOOP.

  LOOP AT p_tab.

    $ix = sy-tabix.

    CLEAR $end_time.
    READ TABLE it_pernr WITH KEY pernr = p_tab-pernr BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    READ TABLE it_ws WITH KEY schkz = it_pernr-schkz
                              zeity = '2'
                              BINARY SEARCH.
    IF sy-subrc EQ 0.
      SEARCH it_ws-rtext FOR ':'.
      IF sy-subrc EQ 0.
        $start_chr = sy-fdpos.
      ENDIF.
      SEARCH it_ws-rtext FOR '-' STARTING AT $start_chr.
      IF sy-subrc EQ 0.
        $pos = sy-fdpos + $start_chr + 1.
        $end_time = it_ws-rtext+$pos.
        REPLACE ':' WITH '' INTO $end_time .
        CONDENSE $end_time NO-GAPS .

        $strlen = strlen( $end_time ).
        IF $strlen < 4.
          CONCATENATE '0' $end_time INTO $end_time.
        ENDIF.
      ENDIF.
    ENDIF.

    IF $end_time IS INITIAL.
      $end_time = '0600'.
    ENDIF.

    CONDENSE $end_time NO-GAPS.

    $strlen = strlen( $end_time ).
    IF $strlen < 6.
      CONCATENATE $end_time '00' INTO $end_time .
    ENDIF.

    IF $end_time CN '0123456789. '.
      $end_time = '0600'.
    ENDIF.

    $end_time = $end_time + 120.

    CONDENSE $end_time NO-GAPS.

    IF p_tab-rtime > $end_time.
      DELETE p_tab INDEX $ix.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_history_for_2nd
*&---------------------------------------------------------------------*
*&      Form  get_pernr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pernr USING check_date.

*  IF NOT s_pernr[] IS INITIAL.
*
*    SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
*           b~schkz a~persg a~persk
*           INTO CORRESPONDING FIELDS OF TABLE it_pernr
*             FROM pa0001 AS a INNER JOIN pa0007 AS b
*               ON b~pernr = a~pernr
*               WHERE a~pernr IN s_pernr
*                 AND a~begda LE check_date
*                 AND a~endda GE check_date
*                 AND b~begda LE check_date
*                 AND b~endda GE check_date
*                 AND b~schkz IN s_schkz.
*
*    LOOP AT s_pernr.
*      r_employeenumber = s_pernr.
*      r_employeenumber-low = s_pernr-low+2.
*      r_employeenumber-high = s_pernr-high+2.
*      APPEND r_employeenumber.
*    ENDLOOP.
*
*  ENDIF.
*
  __cls gt_emp.


  SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
         b~schkz a~persg a~persk a~btrtl                    "UD1K955457
         INTO CORRESPONDING FIELDS OF TABLE it_pernr
           FROM pa0001 AS a INNER JOIN pa0007 AS b
             ON b~pernr = a~pernr
             WHERE a~begda LE check_date
               AND a~endda GE check_date
               AND b~begda LE check_date
               AND b~endda GE check_date
               AND a~kostl IN s_kostl
               AND b~schkz IN s_schkz
               AND a~orgeh IN s_orgeh
               AND a~pernr IN s_pernr
               AND a~persk IN s_persk
               AND a~persg IN s_persg
               AND a~stell IN s_stell.

  IF sy-subrc NE 0.
    g_error = true.
    MESSAGE s000 WITH 'No data has been found!'.
    STOP.
  ENDIF.

*    CASE true.
*      WHEN p_nto.
*
*        IF p_ont NE true.
*
*          SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
*                 b~schkz a~persg a~persk
*                 INTO CORRESPONDING FIELDS OF TABLE it_pernr
*                   FROM pa0001 AS a INNER JOIN pa0007 AS b
*                     ON b~pernr = a~pernr
*                     WHERE   a~begda LE check_date
*                       AND a~endda GE check_date
*                       AND b~begda LE check_date
*                       AND b~endda GE check_date
*                       AND b~schkz IN s_schkz.
*        ENDIF.
*
*      WHEN p_op1.
*        IF s_kostl[] IS INITIAL.
*          MESSAGE s000 WITH 'Please enter Cost Center!.'.
*          g_error = true.
*          STOP.
*        ENDIF.
*
*        SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
*               b~schkz a~persg a~persk
*               INTO CORRESPONDING FIELDS OF TABLE it_pernr
*                 FROM pa0001 AS a INNER JOIN pa0007 AS b
*                   ON b~pernr = a~pernr
*                   WHERE a~kostl IN s_kostl
*                       AND a~begda LE check_date
*                       AND a~endda GE check_date
*                       AND b~begda LE check_date
*                       AND b~endda GE check_date
*                       AND b~schkz IN s_schkz.
*      WHEN p_op2.
*        IF s_orgeh[] IS INITIAL.
*          MESSAGE s000 WITH 'Please enter Org.Unit!'.
*          g_error = true.
*          STOP.
*        ENDIF.
*
*        SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
*               b~schkz a~persg a~persk
*               INTO CORRESPONDING FIELDS OF TABLE it_pernr
*                 FROM pa0001 AS a INNER JOIN pa0007 AS b
*                   ON b~pernr = a~pernr
*                   WHERE a~orgeh IN s_orgeh
*                       AND a~begda LE check_date
*                       AND a~endda GE check_date
*                       AND b~begda LE check_date
*                       AND b~endda GE check_date
*                       AND b~schkz IN s_schkz.
*      WHEN p_op3.
*
*        IF NOT t_t526[] IS INITIAL.
*          LOOP AT t_t526.
*            IF t_t526-sachx IS INITIAL AND t_t526-sachn IS INITIAL.
*              DELETE t_t526.
*              CONTINUE.
*            ENDIF.
*
*            CONCATENATE t_t526-sachx t_t526-sachn INTO t_t526-admncode
*                                                   SEPARATED BY space.
*            MODIFY t_t526 INDEX sy-tabix TRANSPORTING admncode.
*          ENDLOOP.
*
*          SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
*                    b~schkz a~persg a~persk
*                    INTO CORRESPONDING FIELDS OF TABLE it_pernr
*                       FROM pa0001 AS a INNER JOIN pa0007 AS b
*                         ON b~pernr = a~pernr
*                         FOR ALL ENTRIES IN t_t526
*                         WHERE a~sachz = t_t526-sachx
*                           AND a~begda LE check_date
*                           AND a~endda GE check_date
*                           AND b~begda LE check_date
*                           AND b~endda GE check_date
*                           AND b~schkz IN s_schkz.
*        ELSE.
*          g_error = true.
*          MESSAGE s000 WITH 'No data has been found!'.
*          STOP.
*        ENDIF.
*    ENDCASE.
*ENDIF.


  PERFORM supplement_pernr USING check_date.

  IF NOT s_tmcode[] IS INITIAL.
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

      __cls it_pr_tmcode.

      SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
                b~schkz a~persg a~persk a~btrtl             "UD1K955457
                INTO CORRESPONDING FIELDS OF TABLE it_pr_tmcode
                   FROM pa0001 AS a INNER JOIN pa0007 AS b
                     ON b~pernr = a~pernr
                     FOR ALL ENTRIES IN t_t526
                     WHERE a~sachz = t_t526-sachx
                       AND a~begda LE check_date
                       AND a~endda GE check_date
                       AND b~begda LE check_date
                       AND b~endda GE check_date
                       AND b~schkz IN s_schkz.
    ENDIF.

    SORT it_pr_tmcode BY pernr.

    LOOP AT it_pernr.
      $ix  = sy-tabix.
      READ TABLE it_pr_tmcode WITH KEY pernr = it_pernr-pernr
       BINARY SEARCH.
      IF sy-subrc NE 0.
        DELETE it_pernr INDEX $ix.
      ENDIF.
    ENDLOOP.


  ENDIF.

  SORT it_pernr BY employeenumber ASCENDING
                   sname DESCENDING.

  LOOP AT it_pernr.
    $ix = sy-tabix.
    it_pernr-employeenumber = it_pernr-pernr+2.
    MODIFY it_pernr INDEX $ix TRANSPORTING employeenumber.
  ENDLOOP.

  LOOP AT it_pernr.
    $ix  = sy-tabix.
    CHECK it_pernr-pernr IS INITIAL.
    CHECK NOT it_pernr-employeenumber IN s_tmpemp.
    DELETE it_pernr INDEX $ix.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM it_pernr COMPARING
       employeenumber .

ENDFORM.                    " get_pernr
*&---------------------------------------------------------------------*
*&      Form  final_refine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM final_refine USING check_date.

  LOOP AT it_pernr.
    $ix = sy-tabix.
    READ TABLE it_status WITH KEY pernr = it_pernr-employeenumber
    BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF p_ont EQ true. " AND p_nto EQ true.
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

  IF NOT it_new_pernr[] IS INITIAL.
    SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
           b~schkz a~persg a~persk a~btrtl                  "UD1K955457
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
      READ TABLE $it_pernr WITH KEY
               pernr = it_pernr-employeenumber
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
*        IF it_row_tab-kostl EQ '0000033301'.
*          IF it_status-massn EQ 'ZX' AND it_status-massg EQ '17'.
*            it_row_tab-stat2 = it_status-stat2.
*          ELSE.
*            CLEAR it_row_tab.
*            CONTINUE.
*          ENDIF.
*        ELSE.
        CLEAR it_row_tab.
        CONTINUE.
*        ENDIF.
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

    it_row_tab-rdate = check_date.
    APPEND it_row_tab.CLEAR it_row_tab.
  ENDLOOP.

  SORT it_row_tab BY employeenumber.

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
      CHECK itab-inout EQ 'o' OR itab-inout EQ 'i'.
      IF itab-inout EQ 'i'. " In
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
      IF itab-inout EQ 'o'. " Out
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

ENDFORM.                    " final_refine
*&---------------------------------------------------------------------*
*&      Form  get_door_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ITAB_READERID  text
*      <--P_P_ITAB_INOUT  text
*----------------------------------------------------------------------*
FORM get_door_type USING    p_readerid
                   CHANGING p_inout.

  READ TABLE it_door WITH KEY zhdoor = p_readerid
                          BINARY SEARCH.
  IF sy-subrc EQ 0.
    IF it_door-zhdrio EQ '1'.
      p_inout = 'o'.
    ELSE.
      p_inout = 'i'.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_door_type
*&---------------------------------------------------------------------*
*&      Form  get_from_attn_corr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_from_attn_corr.

  __cls i_attn.

  IF NOT it_pernr[] IS INITIAL.
    SELECT pernr zhere manupd INTO TABLE i_attn
    FROM  zthrattncor
    FOR ALL ENTRIES IN it_pernr
    WHERE rdate EQ p_date
    AND pernr EQ it_pernr-pernr.
  ELSE.
    SELECT pernr zhere manupd INTO TABLE i_attn
    FROM  zthrattncor WHERE rdate EQ p_date.
  ENDIF.

  SORT i_attn BY pernr.

ENDFORM.                    " get_from_attn_corr
*&---------------------------------------------------------------------*
*&      Form  get_add_out_history_for_1st
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_add_out_history_for_1st USING check_date.

  DATA out_ok.
  DATA tomorrow TYPE datum.
  DATA lt_itab LIKE itab OCCURS 0 WITH HEADER LINE.

  __cls : itab_for_1st, i_not_out.

  CHECK sy-datum NE check_date.

  LOOP AT itab.
    AT NEW employeenumber.
      CLEAR out_ok.
    ENDAT.
    IF itab-inout EQ 'o'.
      out_ok = true.
    ENDIF.
    AT END OF employeenumber.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    IF out_ok EQ false.
      READ TABLE it_pernr_2nd WITH KEY
               employeenumber = itab-employeenumber BINARY SEARCH.
      IF sy-subrc NE 0.
        i_not_out-employeenumber = itab-employeenumber.
        COLLECT i_not_out.
      ENDIF.

    ENDIF.
    CLEAR $flag.
  ENDLOOP.

  READ TABLE i_not_out INDEX 1.

  CHECK sy-subrc EQ 0.

  LOOP AT i_not_out.
    DELETE itab WHERE employeenumber = i_not_out-employeenumber.
  ENDLOOP.


  PERFORM get_history_from_table TABLES itab_for_1st
                                        i_not_out
                                  USING check_date
                                  false.

  tomorrow = check_date + 1.

  PERFORM get_history_from_table TABLES itab_for_1st
                                        i_not_out
                                  USING tomorrow
                                  true.

  lt_itab[] = itab[].

  itab[] = itab_for_1st[].

  PERFORM modi_itab USING check_date true.

  APPEND LINES OF lt_itab TO itab.

ENDFORM.                    " get_add_out_history_for_1st
*&---------------------------------------------------------------------*
*&      Form  get_history_from_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TAB  text
*      -->P_IT_PERNR  text
*      -->P_P_CHECK_DATE  text
*----------------------------------------------------------------------*
FORM get_history_from_table TABLES   p_tab STRUCTURE itab
                                     p_it_pernr STRUCTURE it_pernr
                            USING    p_check_date
                                     f_add.

  IF f_add EQ true.
    SELECT employeenumber readerid rdate rtime badge
      FROM zthr_bhisthmma
        APPENDING CORRESPONDING FIELDS OF TABLE p_tab
        FOR ALL ENTRIES IN p_it_pernr
        WHERE employeenumber EQ p_it_pernr-employeenumber
          AND rdate EQ p_check_date
        %_HINTS ORACLE 'FIRST_ROWS(10)'.
  ELSE.
    SELECT employeenumber readerid rdate rtime badge
      FROM zthr_bhisthmma
        INTO CORRESPONDING FIELDS OF TABLE p_tab
        FOR ALL ENTRIES IN p_it_pernr
        WHERE employeenumber EQ p_it_pernr-employeenumber
          AND rdate EQ p_check_date
        %_HINTS ORACLE 'FIRST_ROWS(10)'.
  ENDIF.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_zthr_bhisthmma_3 TABLES p_tab
                                                 p_it_pernr
                                           USING p_check_date.
  ENDIF.
*- U1 End

ENDFORM.                    " get_history_from_table
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTHR_BHISTHMMA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_zthr_bhisthmma .

  TYPES: BEGIN OF ty_zthr_bhisthmma,
         rdate TYPE zdrdate,
         rtime TYPE zclksc,
         badge TYPE zbadge,
         readerid TYPE zhdoor,
         employeenumber TYPE zempnumber,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_zthr_bhisthmma.

  DATA: l_handle    TYPE sytabix,
        lt_zthr_bhisthmma TYPE TABLE OF zthr_bhisthmma WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_zthr_bhisthmma TYPE TABLE OF ty_zthr_bhisthmma,
        ls_inx_zthr_bhisthmma TYPE ty_zthr_bhisthmma.

  CONSTANTS: c_zthr_bhisthmma_001(14) VALUE 'ZTHR_BHIST_001'.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = c_zthr_bhisthmma_001.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_zthr_bhisthmma[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_zthr_bhisthmma
    FROM (l_gentab)
   WHERE rdate = p_date.

  CHECK NOT lt_inx_zthr_bhisthmma[] IS INITIAL.

  SORT lt_inx_zthr_bhisthmma BY rdate.
  DELETE ADJACENT DUPLICATES FROM lt_inx_zthr_bhisthmma COMPARING rdate.

  LOOP AT lt_inx_zthr_bhisthmma INTO ls_inx_zthr_bhisthmma.
    t_rdate-rdate = ls_inx_zthr_bhisthmma-rdate.
    APPEND t_rdate.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_ZTHR_BHISTHMMA
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTHR_BHISTHMMA_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TAB  text
*----------------------------------------------------------------------*
FORM archive_read_zthr_bhisthmma_2 TABLES p_tab STRUCTURE itab
                                    USING p_date.

  TYPES: BEGIN OF ty_zthr_bhisthmma,
         rdate TYPE zdrdate,
         rtime TYPE zclksc,
         badge TYPE zbadge,
         readerid TYPE zhdoor,
         employeenumber TYPE zempnumber,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_zthr_bhisthmma.

  DATA: l_handle    TYPE sytabix,
        lt_zthr_bhisthmma TYPE TABLE OF zthr_bhisthmma WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_zthr_bhisthmma TYPE TABLE OF ty_zthr_bhisthmma,
        ls_inx_zthr_bhisthmma TYPE ty_zthr_bhisthmma.

  CONSTANTS: c_zthr_bhisthmma_001(14) VALUE 'ZTHR_BHIST_001'.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = c_zthr_bhisthmma_001.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_zthr_bhisthmma[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_zthr_bhisthmma
    FROM (l_gentab)
   WHERE rdate = p_date
     AND employeenumber IN r_employeenumber.

  CHECK NOT lt_inx_zthr_bhisthmma[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_zthr_bhisthmma_a, gt_zthr_bhisthmma_a[].
  LOOP AT lt_inx_zthr_bhisthmma INTO ls_inx_zthr_bhisthmma.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'ZTHRATTNCO'
        archivkey                 = ls_inx_zthr_bhisthmma-archivekey
        offset                    = ls_inx_zthr_bhisthmma-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_zthr_bhisthmma, lt_zthr_bhisthmma[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'ZTHR_BHISTHMMA'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_zthr_bhisthmma
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_zthr_bhisthmma[] IS INITIAL.

*    DELETE lt_zthrattncor WHERE .

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_zthr_bhisthmma INTO TABLE gt_zthr_bhisthmma_a.
  ENDLOOP.

  SORT gt_zthr_bhisthmma_a.
  DELETE ADJACENT DUPLICATES FROM gt_zthr_bhisthmma_a COMPARING ALL FIELDS.

  LOOP AT gt_zthr_bhisthmma_a.
    MOVE-CORRESPONDING gt_zthr_bhisthmma_a TO p_tab.
    APPEND p_tab.  CLEAR p_tab.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_ZTHR_BHISTHMMA_2
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTHR_BHISTHMMA_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TAB  text
*      -->P_P_IT_PERNR  text
*      -->P_CHECK_DATE  text
*----------------------------------------------------------------------*
FORM archive_read_zthr_bhisthmma_3  TABLES pt_itab  STRUCTURE itab
                                           pt_pernr STRUCTURE it_pernr
                                     USING p_date.

  TYPES: BEGIN OF ty_zthr_bhisthmma,
         rdate TYPE zdrdate,
         rtime TYPE zclksc,
         badge TYPE zbadge,
         readerid TYPE zhdoor,
         employeenumber TYPE zempnumber,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_zthr_bhisthmma.

  DATA: l_handle    TYPE sytabix,
        lt_zthr_bhisthmma TYPE TABLE OF zthr_bhisthmma WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_zthr_bhisthmma TYPE TABLE OF ty_zthr_bhisthmma,
        ls_inx_zthr_bhisthmma TYPE ty_zthr_bhisthmma.

  CONSTANTS: c_zthr_bhisthmma_001(14) VALUE 'ZTHR_BHIST_001'.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = c_zthr_bhisthmma_001.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_zthr_bhisthmma[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_zthr_bhisthmma
    FROM (l_gentab)
    FOR ALL ENTRIES IN pt_pernr
   WHERE employeenumber = pt_pernr-employeenumber
     AND rdate          = p_date.

  CHECK NOT lt_inx_zthr_bhisthmma[] IS INITIAL.

  LOOP AT lt_inx_zthr_bhisthmma INTO ls_inx_zthr_bhisthmma.
    MOVE-CORRESPONDING ls_inx_zthr_bhisthmma TO pt_itab.
    APPEND pt_itab.  CLEAR pt_itab.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_ZTHR_BHISTHMMA_3
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTHR_BHISTHMMA_4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_CHECK_DATE  text
*----------------------------------------------------------------------*
FORM archive_read_zthr_bhisthmma_4  USING  p_date.

  TYPES: BEGIN OF ty_zthr_bhisthmma,
         rdate TYPE zdrdate,
         rtime TYPE zclksc,
         badge TYPE zbadge,
         readerid TYPE zhdoor,
         employeenumber TYPE zempnumber,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_zthr_bhisthmma.

  DATA: l_handle    TYPE sytabix,
        lt_zthr_bhisthmma TYPE TABLE OF zthr_bhisthmma WITH HEADER LINE,
        lt_zthrdoor TYPE TABLE OF zthrdoor WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_zthr_bhisthmma TYPE TABLE OF ty_zthr_bhisthmma,
        ls_inx_zthr_bhisthmma TYPE ty_zthr_bhisthmma.

  CONSTANTS: c_zthr_bhisthmma_001(14) VALUE 'ZTHR_BHIST_001'.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = c_zthr_bhisthmma_001.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_zthr_bhisthmma[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_zthr_bhisthmma
    FROM (l_gentab)
   WHERE rdate EQ p_date
      AND employeenumber NE space.

  CHECK NOT lt_inx_zthr_bhisthmma[] IS INITIAL.

  CLEAR: lt_zthr_bhisthmma, lt_zthr_bhisthmma[].
  LOOP AT lt_inx_zthr_bhisthmma INTO ls_inx_zthr_bhisthmma.
    MOVE-CORRESPONDING ls_inx_zthr_bhisthmma TO lt_zthr_bhisthmma.
    APPEND lt_zthr_bhisthmma.  CLEAR lt_zthr_bhisthmma.
  ENDLOOP.

  SORT lt_zthr_bhisthmma BY readerid.
  DELETE ADJACENT DUPLICATES FROM lt_zthr_bhisthmma COMPARING readerid.

  CLEAR: lt_zthrdoor, lt_zthrdoor[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_zthrdoor
    FROM zthrdoor
    FOR ALL ENTRIES IN lt_zthr_bhisthmma
   WHERE zhdoor = lt_zthr_bhisthmma-readerid
     AND zhdrcns = true.

  LOOP AT lt_inx_zthr_bhisthmma INTO ls_inx_zthr_bhisthmma.
    CLEAR lt_zthrdoor.
    READ TABLE lt_zthrdoor WITH KEY zhdoor = ls_inx_zthr_bhisthmma-readerid.
    CHECK sy-subrc = 0.

    CLEAR it_pernr.
    it_pernr-employeenumber = ls_inx_zthr_bhisthmma-employeenumber.
    APPEND it_pernr.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_ZTHR_BHISTHMMA_4
