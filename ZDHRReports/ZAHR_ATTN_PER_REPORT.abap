*----------------------------------------------------------------------
* Program ID        : ZAHR_ATTN_PER_REPORT
* Title             : [HR] Attendance Percentage Report
* Created on        : 11/18/2008
* Created by        : I.G.MOON
* Specifications By : Euna Lee
* Description       : Attendance Percentage report
*----------------------------------------------------------------------
* Modification Log
* Date        Developer Issue No    Description
*======================================================================
* 03/30/11    Valerian  UD1K951413  Apply major fix
* 05/12/11    Valerian  UD1K951656  Fix absent type logic
* 09/23/11    Valerian  UD1K953484  Apply changes for Kronos implement.
* 05/14/12    Valerian  UD1K954709  Add A/A type 3300 and 3303 in the
*                                   processing logic
* 06/12/12    Valerian  UD1K955008  Correct logic to process A/A Type
*                                   3303
* 09/07/12    Valerian  UD1K955513  Fix prog. logic because of change
*                                   planned working hours
*----------------------------------------------------------------------
REPORT zahr_attn_per_report MESSAGE-ID zmco
                      USING DATABASE pnp.

TABLES: pernr.
INFOTYPES: 0000,
           0001,
           0007,
           0041,
           2001 MODE n,                "Absences
           2002,
           2003 MODE n,                "Substitution
           2004 MODE n,                "On-Call Duty
           2005 MODE n.                "Overtime

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.

INCLUDE <icon>.                        " icon

DATA : sw_active VALUE 'X',
       switch_active TYPE i.

DATA :
      dayint      LIKE pwsdayint  OCCURS 0 WITH HEADER LINE,
      daygen      LIKE pwsdaygen  OCCURS 0 WITH HEADER LINE,
      i2003       LIKE p2003      OCCURS 0 WITH HEADER LINE.

TYPES: BEGIN OF ty_row_tab,
        pernr  TYPE persno,
        sname  TYPE smnam,
        kostl  TYPE kostl,
        orgeh  TYPE orgeh,
        orgtx  TYPE orgtx,
        ename  TYPE emnam,
        sachz  TYPE sachz,
        stat2  TYPE stat2,
        persg  TYPE persg,
        persk  TYPE persk,
        schkz  TYPE schkn,
        categ  TYPE zhrcatg,
        stell  TYPE stell,
        stltx  TYPE stltx,
        nachn  TYPE nachn,
        vorna  TYPE vorna,
        dat01  TYPE dat01,
        abcnt  TYPE dbnum,
        attnr  TYPE dbnum,
        plann  TYPE dbnum,
        text1(10),
        msg(60),
        ktext(20),
 END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES  icon  TYPE icon_d.
TYPES: END OF ty_out.

DATA: BEGIN OF it_orgtx OCCURS 0,
        orgeh LIKE pa0001-orgeh,
        orgtx TYPE orgtx,
      END OF it_orgtx           .

DATA: BEGIN OF it_kostl OCCURS 0,
        kostl LIKE cskt-kostl,
        ktext(20),
      END OF it_kostl.
DATA: BEGIN OF it_status OCCURS 0,
        pernr LIKE pa0000-pernr,
        begda LIKE pa0000-begda,
        massn LIKE pa0000-massn,
        massg LIKE pa0000-massg,
        stat2 LIKE pa0000-stat2,
      END OF it_status           .

DATA: BEGIN OF it_pernr OCCURS 0,
        pernr LIKE zshrclockin-pernr,
        sname  LIKE pa0001-sname,
        kostl  LIKE pa0001-kostl,
        orgeh  LIKE pa0001-orgeh,
        ename  LIKE pa0001-ename,
        sachz  LIKE pa0001-sachz,
        stat2  LIKE pa0000-stat2,
        persg  LIKE pa0001-persg,
        persk  LIKE pa0001-persk,
        schkz  LIKE pa0007-schkz,
        categ  TYPE zhrcatg,
        stell  TYPE stell,
        nachn  TYPE nachn,
        vorna  TYPE vorna,
        dat01  TYPE dat01,
        anzhl LIKE pc2b6-anzhl,
END   OF it_pernr.

DATA: it_abstx  LIKE t554t OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_513s OCCURS 0,
       stell   LIKE t513s-stell,
       stltx   LIKE t513s-stltx,
      END OF it_513s.

DATA: BEGIN OF it_p2001 OCCURS 0,
       pernr   LIKE pa2001-pernr,
       awart   LIKE pa2001-awart,
       atext   LIKE t554t-atext,
       begda   LIKE pa2001-begda,
       endda   LIKE pa2001-endda,
       abwtg   LIKE pa2001-abwtg,
       stdaz   LIKE pa2001-stdaz,
      END OF it_p2001.

* BEGIN OF UD1K951413
DATA: BEGIN OF it_p2002_1005 OCCURS 0,
       pernr   LIKE pa2002-pernr,
       awart   LIKE pa2002-awart,
       begda   LIKE pa2002-begda,
       endda   LIKE pa2002-endda,
       abwtg   LIKE pa2002-abwtg,
       stdaz   LIKE pa2002-stdaz,
      END OF it_p2002_1005.
* END OF UD1K951413

DATA it_p2002 LIKE it_p2001 OCCURS 0 WITH HEADER LINE.
DATA: it_ovrtx  LIKE t554t OCCURS 0 WITH HEADER LINE.

DATA: time_results    TYPE STANDARD TABLE OF ptm_time_results,
      time_results_wa LIKE LINE OF time_results.
DATA: error_tab LIKE hrerror OCCURS 0 WITH HEADER LINE.

DATA: zes_wa   LIKE pc2b6.
*      saldo LIKE pc2b5 OCCURS 0 WITH HEADER LINE,
*      zl    LIKE pc2bf OCCURS 0 WITH HEADER LINE.

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

RANGES: objid_range FOR objec-objid.

CONSTANTS: c_maxtables TYPE i VALUE 6,
           c_lowdate(8)  VALUE '18000101',
           c_highdate(8) VALUE '99991231',
           cutoff_date TYPE datum VALUE '20110725'.         "UD1K953009

* //////////////////////////////////////////////////// *
INCLUDE:
mpzdat02, " Work tables for daily work
rpppxd00, " R/3 data definition for PCL1 & pcl2 buffer
rpppxd10, " Data definition for PCL1, pcl2
pc2rxid0, " Data definition, cluster IS file pcl2
rpclst00,
rpc2b200,
rptsim00.
* //////////////////////////////////////////////////// *


*
SELECTION-SCREEN BEGIN OF BLOCK param WITH FRAME TITLE text-fra.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 2.
PARAMETERS: rdclust TYPE rdclst.
SELECTION-SCREEN COMMENT 5(25) text-ta0 FOR FIELD rdclust.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END OF BLOCK param.

*
*selection-screen begin of block active with frame title text-frb.
*selection-screen begin of line.
*parameters:
*activ1 like rptxxxxx-kr_feld1 radiobutton group act default 'X'.
*selection-screen comment 4(36) text-ta1.
*selection-screen end of line.
*selection-screen begin of line.
*parameters:
*activ2 like rptxxxxx-kr_feld1 radiobutton group act.
*selection-screen comment 4(36) text-ta2.
*selection-screen end of line.
*selection-screen begin of line.
*parameters:
*activ3 like rptxxxxx-kr_feld1 radiobutton group act.
*selection-screen comment 4(36) text-ta3.
*selection-screen end of line.
*selection-screen end of block active.

FIELD-SYMBOLS : <f_s>, <f_t> .

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE u_break.
  if p_debug eq true.
    break-point.
  endif.
END-OF-DEFINITION.
DEFINE __define_not_important.
* { not important
* Total Doc. Count to be created.
  data  : total_doc_cnt type i,
          current_doc_cnt type i.
  data : percentage type p,$mod type i,
         $current_cnt(10),$total_cnt(10),$text(100) .
  clear : total_doc_cnt,current_doc_cnt.
* }
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DATA $menge LIKE ptev_rep_h-itanz.
DATA $anzhl LIKE pc2b6-anzhl.
DATA $stdaz LIKE pa2003-stdaz.
DATA exp_day.


* //////////////////////////////////////////////////// *

CONSTANTS:
 c_eg1(1)   TYPE c VALUE   'A',"US-Salary
 c_eg2(1)   TYPE c VALUE   'B',"US-Wage
 c_eg3(1)   TYPE c VALUE   'K'."KR-Salary


DATA: g_error(1),
      g_repid  LIKE sy-repid.
RANGES s_date FOR sy-datum.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title = '[HR] Attendance Percentage Report'.

  PERFORM default_.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

START-OF-SELECTION.

  __cls it_pernr.
  __cls s_date.
  s_date = 'IBT'.
  s_date-low = pn-begda.
  s_date-high = pn-endda.
  APPEND s_date.

  switch_active = 1.
  sw_active = ' '.

  PERFORM show_progress     USING 'Initializing...' '5'.
  PERFORM get_stauts    USING sy-datum.
  CHECK g_error NE true.

GET pernr.

  PERFORM show_progress     USING pernr-pernr '20'.

  __cls psp.

  PERFORM build_psp.

  CLEAR $anzhl.

*  PROVIDE * FROM p2002 BETWEEN pn-begda AND pn-endda.
*    IF p2002-awart EQ '1001' OR
*       p2002-awart EQ '1003' OR
*       p2002-awart EQ '1004' OR
*       p2002-awart EQ '1005' OR
*       p2002-awart EQ '1006' OR
*       p2002-awart EQ '1007'.
*
*      CALL FUNCTION 'DATE_COMPUTE_DAY'
*           EXPORTING
*                date = p2002-begda
*           IMPORTING
*                day  = exp_day.
*
*      IF exp_day >= '6'.
*        IF p2002-stdaz < 4.
*          ADD '0.5' TO $anzhl.
*        ELSE.
*          ADD 1 TO $anzhl.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDPROVIDE.                                               "PSP

  SORT p0001 BY endda DESCENDING.
  LOOP AT p0001.
    IF p0001-begda <= sy-datum AND p0001-endda >= sy-datum.
      EXIT.
    ENDIF.
  ENDLOOP.

  CHECK : pernr-pernr IN pnppernr,
          pernr-btrtl IN pnpbtrtl,
          pernr-persk IN pnppersk,
          pernr-kostl IN pnpkostl,
          pernr-orgeh IN pnporgeh.

  MOVE-CORRESPONDING p0001 TO it_pernr.
  it_pernr-dat01 = p0041-dat01.
  it_pernr-schkz = p0007-schkz.
  it_pernr-anzhl = $anzhl.
  APPEND it_pernr.

END-OF-SELECTION.

  READ TABLE it_pernr INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No data was found!'.
    g_error = true.
  ENDIF.

  CHECK g_error NE true.
  SORT it_pernr BY pernr.

  PERFORM show_progress     USING 'Read Absence day...' '30'.
  PERFORM get_absence_day.
  PERFORM get_overtime_day.
  CHECK g_error NE true.

  PERFORM get_row_data.

  PERFORM move_out.
  PERFORM set_output .

*&---------------------------------------------------------------------*
*&      Form  set_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output.

  CHECK g_error IS INITIAL.

  PERFORM show_progress     USING 'Preparing screen...' '95'.
  PERFORM init_alv_parm.
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].
  PERFORM alv_events_get    USING:  'P', 'T'.
  PERFORM alv_grid_display  TABLES  gt_out USING ''.

ENDFORM.                    " set_output

*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FP_UCOMM                                                      *
*  -->  FS                                                            *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.
  CLEAR : g_error.

*  CASE fp_ucomm.
*    WHEN 'SAVE'.
*      CHECK g_error NE true.
*
*  ENDCASE.

ENDFORM.                    "USER_COMMAND

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  REFRESH gt_listheader.

  l_text = 'Attendance Percentage report'.
  PERFORM set_header_line USING:
          'P' 'H' ''      l_text       '',
          'D' 'S' 'Period'   s_date-low  s_date-high.

*  IF p_op1 EQ true.
*    PERFORM set_header_line USING:
*            'S' 'S' 'Cost.C'   s_kostl-low  ''.
*  ENDIF.
*
*  IF p_op2 EQ true.
*    PERFORM set_header_line USING:
*            'S' 'S' 'Cost.C'   s_orgeh-low  ''.
*  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page

**---------------------------------------------------------------------*
**       FORM PF_STATUS_SET
**---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.
  SET PF-STATUS '100'.
ENDFORM.                    "PF_STATUS_SET

*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1894   text
*      -->P_1895   text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = pf_val
      text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  init_alv_parm
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
*&      Form  fieldcat_init
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
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-no_zero       = &8.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.



  __catalog :
    'X'  'ORGEH'    'Org.Unit'          8  'NUMC' '' '' '',
    'X'  'ORGTX'    'Desc.'            25  'CHAR' '' '' '',
    'X'  'KOSTL'    'Cost.C'            8  'NUMC' '' '' '',
    'X'  'KTEXT'    'CCtr.Name'        20  'CHAR' '' '' '',
    'X'  'PERNR'    'Emp.#'             8  'NUMC' '' '' '',
    ' '  'SNAME'    'Name'             30  'CHAR' '' '' '',
*    ' '  'NACHN'    'Last Name'        20  'CHAR' '' '' '',
*    ' '  'VORNA'    'First Name'       20  'CHAR' '' '' '',
    ' '  'DAT01'    'Hire Date'         8  'DATS' '' '' '',
    ' '  'STLTX'    'Job Title'        20  'CHAR' '' '' '',
    ' '  'TEXT1'    'Status'           10  'CHAR' '' '' '',
    ' '  'ICON'     ' '                 4  'ICON' '' '' '',
    ' '  'SCHKZ'    'W.S'               3  'CHAR' '' '' '',
    ' '  'ABCNT'    'ABS.Cnt'          10  'DEC' '' '' 'X',
    ' '  'PLANN'    'Planned'          10  'DEC' '' '' 'X',
    ' '  'ATTNR'    'At.Rate(%)'       10  'DEC' '' '' 'X',
    ' '  'MSG'      'Message'          40  'CHAR' '' '' ''.

  LOOP AT gt_fieldcat INTO gs_fieldcat.
    IF gs_fieldcat-fieldname EQ 'SCHKZ'.
      gs_fieldcat-ref_tabname = 'PA0007'.
      gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname.
    ENDIF.
    IF gs_fieldcat-fieldname EQ 'ABCNT'.
      gs_fieldcat-decimals_out = '1'.
    ENDIF.
    IF gs_fieldcat-fieldname EQ 'PLANN'.
      gs_fieldcat-decimals_out = '1'.
    ENDIF.

    MODIFY gt_fieldcat FROM gs_fieldcat.
  ENDLOOP.

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  sort_build
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
     'ORGEH'        ' ' 'X' 'X' 'X',
     'ORGTX'        ' ' 'X' 'X' 'X',
     'KOSTL'        ' ' 'X' 'X' 'X',
     'KTEXT'        ' ' 'X' 'X' 'X',
     'PERNR'        ' ' 'X' 'X' 'X'.
ENDFORM.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  default_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_.

  s_date-low = sy-datum - 365.
  s_date-high = sy-datum.
  APPEND s_date.

  PERFORM default_variant.
ENDFORM.                    " default_

*---------------------------------------------------------------------*
*       FORM default_variant                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM default_variant.

  DATA: h_subrc   TYPE sysubrc,
        h_repid   TYPE syrepid,
        h_variant TYPE raldb_vari.

*  check sy-slset is initial
*  and   sy-calld is initial
*  and   sy-tcode <> 'SE38'
*  and   sy-tcode <> 'SA38'.

  h_repid = sy-repid.
  CLEAR h_variant.
  h_variant = 'U_'.
  WRITE sy-uname TO h_variant+2.

  h_variant = '_DEFAULT'.

  CALL FUNCTION 'RS_VARIANT_EXISTS'
    EXPORTING
      report  = h_repid
      variant = h_variant
    IMPORTING
      r_c     = h_subrc.

  IF NOT h_subrc IS INITIAL.
    CLEAR h_variant.
    h_variant = 'SAP_TCODE_'.
    WRITE sy-tcode TO h_variant+10.
    CALL FUNCTION 'RS_VARIANT_EXISTS'
      EXPORTING
        report  = h_repid
        variant = h_variant
      IMPORTING
        r_c     = h_subrc.

    IF NOT h_subrc IS INITIAL.
      CLEAR h_variant.
      h_variant = 'SAP&TCODE_'.
      WRITE sy-tcode TO h_variant+10.
      CALL FUNCTION 'RS_VARIANT_EXISTS'
        EXPORTING
          report  = h_repid
          variant = h_variant
        IMPORTING
          r_c     = h_subrc.
    ENDIF.
  ENDIF.

  IF h_subrc IS INITIAL.
    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
      EXPORTING
        report               = h_repid
        variant              = h_variant
      EXCEPTIONS
        variant_not_existent = 01
        variant_obsolete     = 02.
  ENDIF.


ENDFORM.                    " default_variant

*&---------------------------------------------------------------------*
*&      Form  view_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_.

  PERFORM  initialize            .

ENDFORM.                    " view_
*&---------------------------------------------------------------------*
*&      Form  initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize.
  CLEAR g_error.
ENDFORM.                    " initialize
*&---------------------------------------------------------------------*
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data.

  DATA $ix TYPE i.
  DATA $fr TYPE i.
  DATA abs_cnt TYPE dbnum.
  DATA : $atext LIKE it_p2001-atext,
         $tcnt(5).
  DATA $val1 LIKE it_row_tab-plann.
  DATA: i549q        LIKE t549q OCCURS 0 WITH HEADER LINE,
      reday_beg(2) TYPE n,
      reday_end(2) TYPE n.

  DATA iholiday TYPE STANDARD TABLE OF iscal_day WITH HEADER LINE.
  DATA : $from TYPE datum,
         $to TYPE datum,
         $hol_date TYPE datum.
  DATA $wotnr TYPE p.
  DATA $year(4) TYPE n.
  DATA   $pabrp LIKE i549q-pabrp.
  DATA $anzhl LIKE zes_wa-anzhl.
  DATA $tmpanzhl LIKE zes_wa-anzhl.
  DATA $1052_57 LIKE  zes OCCURS 0 WITH HEADER LINE.
  DATA $ovrcnt LIKE abs_cnt.
  DATA $it_p2002 LIKE it_p2002 OCCURS 0 WITH HEADER LINE.
  DATA $9501 LIKE $anzhl.


  DATA $found.

*... Begin{ added by hkyoon for unexcused absence 06/18/2012
  DATA: lrg_awart TYPE RANGE OF awart,
        ls_awart LIKE LINE OF lrg_awart.
*... }End

  __cls it_row_tab.

  __define_not_important.
  DESCRIBE TABLE it_pernr LINES total_doc_cnt.
  $total_cnt = total_doc_cnt.

  CALL FUNCTION 'HR_PAYROLL_PERIODS_GET'
    EXPORTING
      get_begda       = s_date-low
      get_endda       = s_date-high
    TABLES
      get_periods     = i549q
    EXCEPTIONS
      no_period_found = 1
      no_valid_permo  = 2.

  LOOP AT it_pernr.

    CLEAR :  $ovrcnt,$anzhl,$tmpanzhl,$9501.

    CLEAR it_row_tab.

    MOVE-CORRESPONDING it_pernr TO it_row_tab.

    ADD 1 TO current_doc_cnt.
    $current_cnt = current_doc_cnt.
    CONCATENATE 'Calc...' it_pernr-pernr ':' $current_cnt '/' $total_cnt
                                                              INTO $text.
    CONDENSE $text.
    percentage = current_doc_cnt MOD 10.
    IF percentage EQ 0.
      PERFORM show_progress USING $text 0.
    ENDIF.

    READ TABLE it_status WITH KEY pernr = it_pernr-pernr BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF it_status-stat2 NE '1' AND it_status-stat2 NE '3'.
        CONTINUE.
      ENDIF.
      it_row_tab-stat2 = it_status-stat2.
      CASE it_row_tab-stat2.
        WHEN '0'.
          it_row_tab-text1 = 'Withdrawn'.
        WHEN '1'.
          it_row_tab-text1 = 'Inactive'.
        WHEN '2'.
          it_row_tab-text1 = 'Retire'.
        WHEN '3'.
          it_row_tab-text1 = 'Active'.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    READ TABLE it_orgtx WITH KEY orgeh = it_row_tab-orgeh BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-orgtx = it_orgtx-orgtx.
    ENDIF.

    READ TABLE it_513s WITH KEY stell = it_row_tab-stell BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-stltx = it_513s-stltx.
    ENDIF.

    CLEAR $anzhl.
    CLEAR abs_cnt.


    LOOP AT i549q.

      b2-key-pabrj = i549q-pabrj.
      b2-key-pabrp = i549q-pabrp.
      b2-key-cltyp = '1'.
      b2-key-pernr = it_pernr-pernr.

      IMPORT zes FROM DATABASE pcl2(b2) ID b2-key.

      IF sy-subrc EQ 0.

        IF i549q-pabrj EQ s_date-low(4) AND
           i549q-pabrp EQ s_date-low+4(2).
          reday_beg = s_date-low+6(2).
          IF s_date-high IS INITIAL.
            DELETE zes WHERE reday < reday_beg
                          OR  reday > reday_beg.
          ELSE.
            DELETE zes WHERE reday < reday_beg.
          ENDIF.
        ENDIF.

        IF i549q-pabrj EQ s_date-high(4) AND
           i549q-pabrp EQ s_date-high+4(2).
          reday_end = s_date-high+6(2).
          DELETE zes WHERE reday > reday_end.
        ENDIF.

* BEGIN OF UD1K951413
*        __cls : $1052_57.
*
*        LOOP AT zes INTO zes_wa WHERE ztart EQ '1052' OR
*                                      ztart EQ '1057'.
*          $1052_57 = zes_wa.
*          APPEND $1052_57.
*        ENDLOOP.
*
*        SORT : $1052_57 BY reday ztart.

        DATA: zes_day LIKE zes OCCURS 0,
              holidayf(1) TYPE c,
              paiddayf(1) TYPE c,
              workedf(1)  TYPE c.                           "UD1K951413

        DATA: check_date TYPE datum.                        "UD1K951413

        SORT zes BY reday.
* END OF UD1K951413
        LOOP AT zes INTO zes_wa.

          APPEND zes_wa TO zes_day.                         "UD1K951413

          AT END OF reday.                                  "UD1K951413
            CLEAR: holidayf, paiddayf, workedf.             "UD1K951413
            LOOP AT zes_day INTO zes_wa.

              CONCATENATE i549q-pabrj i549q-pabrp zes_wa-reday
                    INTO check_date.                        "UD1K951413

*             Count Holiday
              IF check_date < cutoff_date.                  "UD1K953484
                IF zes_wa-ztart EQ '9501'.
* BEGIN OF UD1K951413
                  REFRESH it_p2002_1005.
                  SELECT pernr awart begda endda abwtg stdaz
                    INTO CORRESPONDING FIELDS OF TABLE it_p2002_1005
                    FROM pa2002
                   WHERE pernr = it_pernr-pernr
                     AND begda = check_date
                     AND ( awart = '1005' OR
                           awart = '1006' OR
                           awart = '1007' ).
                  IF it_p2002_1005[] IS INITIAL.
                    ADD '1' TO  $9501.
                    holidayf = 'X'.                         "UD1K951413
                    CONTINUE.
                  ENDIF.
*           ADD '1' TO  $9501.
*           CONTINUE.
* END OF UD1K951413
                ENDIF.
* BEGIN OF UD1K953484
              ELSE.
                IF zes_wa-ztart EQ '9500'.
                  ADD '1' TO  $9501.
                  holidayf = 'X'.
                  CONTINUE.
                ENDIF.
              ENDIF.
* END OF UD1K953484
*             Count paiddays on WS
              IF zes_wa-ztart EQ '0002'.
*               IF zes_wa-anzhl >= '8'.                     "UD1K955513
                  ADD 1 TO $anzhl.
                  paiddayf = 'X'.                           "UD1K951413
*               ENDIF.                                      "UD1K955513
              ENDIF.

            ENDLOOP.                                        "UD1K951413

* BEGIN OF UD1K951413
            LOOP AT zes_day INTO zes_wa WHERE ztart = '9301' OR
                                              ztart = '9302' OR
                                              ztart = '9303' OR
                                              ztart = '9304' OR
                                              ztart = '9407' OR
                                              ztart = '9502' OR
                                              ztart = '9503' OR
                                              ztart = '9504'.
              workedf = 'X'.
              EXIT.
            ENDLOOP.

            LOOP AT zes_day INTO zes_wa WHERE ztart = '9501'.
              IF holidayf IS INITIAL.
                workedf = 'X'.
              ENDIF.
              EXIT.
            ENDLOOP.

* BEGIN OF UD1K951413
            IF paiddayf IS INITIAL OR NOT holidayf IS INITIAL.
              IF NOT workedf IS INITIAL.
                $ovrcnt = $ovrcnt + 1.
              ELSE.
                READ TABLE zes_day WITH KEY ztart = '8099'
                                   TRANSPORTING NO FIELDS.
                IF sy-subrc = 0.
* BEGIN OF UD1K951656
                  LOOP AT it_p2001 WHERE begda = check_date
                                     AND pernr = it_pernr-pernr
                                   AND ( awart = '2000' OR  "UD1K954709
                                         awart = '3303' ).  "UD1K954709
*                                    AND awart = '2000'.    "UD1K954709
                    $ovrcnt = $ovrcnt + 1.
                    EXIT.
                  ENDLOOP.
* END OF UD1K951656
                ENDIF.
              ENDIF.
            ENDIF.
* END OF UD1K951413

*... Begin{ added by hkyoon for unexcused absence 06/18/2012
*...        get unexcused absence from zthr_absence.
            __cls lrg_awart.
            CLEAR ls_awart.

            SELECT awart AS low INTO CORRESPONDING FIELDS OF TABLE lrg_awart
              FROM zthr_absence
             WHERE begda <= check_date
               AND endda >= check_date.

            ls_awart-sign   = 'I'.
            ls_awart-option = 'EQ'.

            MODIFY lrg_awart FROM ls_awart TRANSPORTING sign option
             WHERE sign   IS INITIAL
               AND option IS INITIAL.

            SORT lrg_awart BY low.
*...}End

            LOOP AT it_p2001 WHERE begda = check_date
                               AND pernr = it_pernr-pernr   "UD1K951413
*... Begin{ added by hkyoon for unexcused absence 06/18/2012
*...        get unexcused absence from zthr_absence.
                               AND awart IN lrg_awart.
*                               AND ( awart = '2000'
*                                OR   awart = '1025'
*                                OR   awart = '1045'
*                                OR   awart = '1046'
*                                OR   awart = '1017'         "UD1K951413
*                                OR   awart = '1074'         "UD1K953484
*                                OR   awart = '3300'         "UD1K954709
*                                OR   awart = '3303'         "UD1K954709
*                                OR   awart = '1050' ).
*...}End
              EXIT.
            ENDLOOP.

            IF sy-subrc EQ 0.
*           Do nothing
            ELSE.
*              LOOP AT zes_day INTO zes_wa WHERE ztart = '9301' OR
*                                                ztart = '9302' OR
*                                                ztart = '9303' OR
*                                                ztart = '9304' OR
*                                                ztart = '9407' OR
*                                                ztart = '9502' OR
*                                                ztart = '9503' OR
*                                                ztart = '9504'.
*                EXIT.
*              ENDLOOP.
*              IF sy-subrc <> 0.

              IF workedf IS INITIAL.
                IF NOT paiddayf IS INITIAL AND holidayf IS INITIAL.
                  $tmpanzhl = $tmpanzhl + 1.                "UD1K951413
                ENDIF.
              ENDIF.

* END OF UD1K951413

            ENDIF.                                          "UD1K951413

            DATA: p2001_day TYPE reday.

            READ TABLE it_p2001 WITH KEY pernr = it_pernr-pernr
                                BINARY SEARCH.
            IF sy-subrc EQ 0.
              $fr = sy-tabix.

              LOOP AT it_p2001 FROM $fr.
                p2001_day = it_p2001-begda+6(2).

                IF it_p2001-pernr NE it_pernr-pernr OR
                  it_p2001-begda NE check_date.             "UD1K951413
                  CONTINUE.                                 "UD1K951413
*                 p2001_day NE check_date.                  "UD1K951413
*                 EXIT.                                     "UD1K951413
                ENDIF.

                $atext = it_p2001-atext.

*... Begin{ added by hkyoon for unexcused absence 06/18/2012
*...        get unexcused absence from zthr_absence.
                IF it_p2001-awart IN lrg_awart.
*                IF it_p2001-awart EQ '2000' OR
*                   it_p2001-awart EQ '1025' OR
*                   it_p2001-awart EQ '1045' OR
*                   it_p2001-awart EQ '1046' OR
*                   it_p2001-awart EQ '1017' OR
*                   it_p2001-awart EQ '1074' OR              "UD1K953484
*                   it_p2001-awart EQ '3300' OR              "UD1K954709
*                   it_p2001-awart EQ '3303' OR              "UD1K954709
*                   it_p2001-awart EQ '1050'.
*...}End
                  IF NOT workedf IS INITIAL.
                    ADD '0.5' TO abs_cnt.
                    EXIT.                                   "UD1K951413
                  ELSE.
                    ADD 1 TO abs_cnt.
                    EXIT.                                   "UD1K951413
                  ENDIF.

                ENDIF.

              ENDLOOP.
            ENDIF.

            REFRESH zes_day.                                "UD1K951413
          ENDAT.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

*break-point.

** Absence Count {
*    READ TABLE it_p2001 WITH KEY pernr = it_pernr-pernr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*
*      $fr = sy-tabix.
*
*      LOOP AT it_p2001 FROM $fr.
*        IF it_p2001-pernr NE it_pernr-pernr.
*          EXIT.
*        ENDIF.
*        $atext = it_p2001-atext.
*
** BEGIN OF UD1K951413
*        DATA: p2001_day TYPE reday.
*
*        IF it_p2001-awart EQ '2000'
*        OR it_p2001-awart EQ '1025'
*        OR it_p2001-awart EQ '1045'
*        OR it_p2001-awart EQ '1046'
*        OR it_p2001-awart EQ '1017'
*        OR it_p2001-awart EQ '1050'.
*
*          b2-key-pabrj = it_p2001-begda(4).
*          b2-key-pabrp = it_p2001-begda+4(2).
*          b2-key-cltyp = '1' .
*          b2-key-pernr = it_pernr-pernr .
*
*          IMPORT zes FROM DATABASE pcl2(b2) ID b2-key.
*
*          p2001_day = it_p2001-begda+6(2).
*
*          LOOP AT zes WHERE reday = p2001_day
*                        AND ( ztart = '9301' OR
*                              ztart = '9302' OR
*                              ztart = '9303' OR
*                              ztart = '9304' OR
*                              ztart = '9407' OR
**                             ztart = '9501' OR
*                              ztart = '9502' OR
*                              ztart = '9503' OR
*                              ztart = '9504' ).
*
*            ADD '0.5' TO abs_cnt.
*            EXIT.
*          ENDLOOP.
*
*          IF sy-subrc <> 0.
*            ADD 1 TO abs_cnt.
*          ENDIF.
*
*        ELSE.
** END OF UD1K951413
*
*          IF it_p2001-abwtg EQ 1.
*            ADD 1 TO : abs_cnt.
**          ADD 1 TO : abs_cnt, $anzhl.
*          ELSE.
*            ADD '0.5' TO : abs_cnt.
**          ADD '0.5' TO : abs_cnt, $anzhl.
*          ENDIF.
*
*        ENDIF.                                             "UD1K951413
*      ENDLOOP.
*
*    ENDIF.
** }

    it_row_tab-abcnt = abs_cnt.

    WRITE abs_cnt TO $tcnt DECIMALS 1.
    IF abs_cnt > 0.
      CONCATENATE $tcnt ' Absence(s)(' $atext '..)' INTO it_row_tab-msg
                            SEPARATED BY space.
    ENDIF.
* }

    __cls $it_p2002.

*break-point.

*    LOOP AT it_p2002 WHERE pernr = it_pernr-pernr.
*      $it_p2002 =  it_p2002.
*      $it_p2002-awart = '1001'.
*      CLEAR $it_p2002-atext.                               "UD1K951413
*      COLLECT $it_p2002.
*    ENDLOOP.

*    LOOP AT $it_p2002.
*      IF it_pernr-schkz = '4001' OR it_pernr-schkz(4) = '8000'.
**       IF $it_p2002-stdaz < '5'.                          "UD1K951413
*        IF $it_p2002-stdaz <= '5'.                         "UD1K951413
*          ADD '0.5' TO $ovrcnt.
*        ELSE.
*          ADD '1' TO $ovrcnt.
*        ENDIF.
*      ELSE.
**       IF $it_p2002-stdaz < '4'.                          "UD1K951413
*        IF $it_p2002-stdaz <= '4'.                         "UD1K951413
*          ADD '0.5' TO $ovrcnt.
*        ELSE.
*          ADD '1' TO $ovrcnt.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

*    break-point.

*    it_row_tab-plann = it_pernr-anzhl + $anzhl.
    it_row_tab-plann =    $ovrcnt
                        + $anzhl
                        - $tmpanzhl
                        - $9501 .

    $val1 = it_row_tab-plann - it_row_tab-abcnt.

    IF $val1 NE 0 AND it_row_tab-plann NE 0.
      it_row_tab-attnr = $val1 / it_row_tab-plann * 100.
    ELSE.
      it_row_tab-attnr = 0.
    ENDIF.

    APPEND it_row_tab.
  ENDLOOP.

ENDFORM.                    " get_row_data
*&---------------------------------------------------------------------*
*&      Form  get_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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

  __cls it_orgtx .

*  SELECT ORGEH ORGTX
*    INTO TABLE IT_ORGTX
*    FROM T527X
*    WHERE SPRSL EQ SY-LANGU
*      AND ENDDA GE SY-DATUM
*      AND BEGDA LE SY-DATUM.

  SELECT objid stext     INTO TABLE it_orgtx
  FROM hrp1000 WHERE plvar EQ '01'
                         AND otype EQ 'O'
                         AND istat EQ '1'
                         AND begda <=  sy-datum
                         AND endda >=  sy-datum
                         AND langu EQ sy-langu.

  SORT it_orgtx BY orgeh.

  __cls it_513s.

  SELECT stell stltx INTO TABLE it_513s
  FROM t513s
  WHERE sprsl EQ sy-langu.

  SORT it_513s BY stell.

ENDFORM.                     " get_status
*&---------------------------------------------------------------------*
*&      Form  get_it_pernr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_pernr  USING check_date.
*  __cls it_pernr.


*  IF p_op0 EQ true.
*    SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
*           a~persg a~persk a~stell b~nachn b~vorna c~dat01 d~schkz
*           INTO CORRESPONDING FIELDS OF TABLE it_pernr
*           FROM pa0001 AS a
*            INNER JOIN pa0002 AS b
*             ON b~pernr = a~pernr
*            INNER JOIN pa0041 AS c
*             ON c~pernr = b~pernr
*             AND  c~sprps = space
*            INNER JOIN pa0007 AS d
*             ON d~pernr = c~pernr
*             WHERE a~pernr IN s_pernr
*               AND a~begda LE check_date
*               AND a~endda GE check_date
*               AND b~begda LE check_date
*               AND b~endda GE check_date
*               AND c~begda LE check_date
*               AND c~endda GE check_date
*               AND d~begda LE check_date
*               AND d~endda GE check_date.
*  ELSE.
*    IF p_op1 EQ true.
*
*      IF s_kostl[] IS INITIAL.
*        MESSAGE s000 WITH 'Please enter Cost Center!.'.
*        g_error = true.
*        STOP.
*      ENDIF.
*
*      SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
*           a~persg a~persk a~stell b~nachn b~vorna c~dat01 d~schkz
*             INTO CORRESPONDING FIELDS OF TABLE it_pernr
*             FROM pa0001 AS a
*              INNER JOIN pa0002 AS b
*               ON b~pernr = a~pernr
*              INNER JOIN pa0041 AS c
*               ON c~pernr = b~pernr
*               AND  c~sprps = space
*            INNER JOIN pa0007 AS d
*             ON d~pernr = c~pernr
*               WHERE a~kostl IN s_kostl
*                 AND a~begda LE check_date
*                 AND a~endda GE check_date
*                 AND b~begda LE check_date
*                 AND b~endda GE check_date
*                 AND c~begda LE check_date
*                 AND c~endda GE check_date
*                 AND d~begda LE check_date
*                 AND d~endda GE check_date.
*
*    ENDIF.
*
*    IF p_op2 EQ true.
*      IF s_orgeh[] IS INITIAL.
*        MESSAGE s000 WITH 'Please enter Org. unit!'.
*        g_error = true.
*        STOP.
*      ENDIF.
*
*      SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
*           a~persg a~persk a~stell b~nachn b~vorna c~dat01 d~schkz
*             INTO CORRESPONDING FIELDS OF TABLE it_pernr
*             FROM pa0001 AS a
*              INNER JOIN pa0002 AS b
*               ON b~pernr = a~pernr
*              INNER JOIN pa0041 AS c
*               ON c~pernr = b~pernr
*               AND  c~sprps = space
*            INNER JOIN pa0007 AS d
*             ON d~pernr = c~pernr
*               WHERE a~orgeh IN s_orgeh
*                 AND a~begda LE check_date
*                 AND a~endda GE check_date
*                 AND b~begda LE check_date
*                 AND b~endda GE check_date
*                 AND c~begda LE check_date
*                 AND c~endda GE check_date
*                 AND d~begda LE check_date
*                 AND d~endda GE check_date.
*
*    ENDIF.
*
*  ENDIF.
*
*  DATA $ix TYPE i.
*
*  LOOP AT it_pernr.
*    $ix = sy-tabix.
*    PERFORM get_emp_categ  USING    it_pernr-persg it_pernr-persk
*                           CHANGING it_pernr-categ.
*    IF it_pernr-categ NE 'B'.
*      DELETE it_pernr INDEX $ix.
*    ENDIF.
*  ENDLOOP.
*
*  SORT it_pernr BY pernr.
*
*  READ TABLE it_pernr INDEX 1.
*  IF sy-subrc NE 0.
*    MESSAGE s000 WITH 'No data exists.'.
*    g_error = true.
*  ENDIF.
*
ENDFORM.                    " get_it_pernr

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
*&      Form  get_absence_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_absence_day.

  DATA $ix TYPE i.

  RANGES r_abstyp FOR t554s-subty.
  RANGES r_subty1 FOR t554s-subty.

*... Begin{ added by hkyoon for unexcused absence 06/18/2012
*...        get unexcused absence from zthr_absence.
*... Begin{ added by hkyoon for unexcused absence 06/18/2012
  DATA: lrg_awart TYPE RANGE OF awart,
        ls_awart LIKE LINE OF lrg_awart.
*... }End

  __cls lrg_awart.
  CLEAR ls_awart.

  SELECT awart AS low INTO CORRESPONDING FIELDS OF TABLE lrg_awart
    FROM zthr_absence
   WHERE begda <= sy-datum
     AND endda >= sy-datum.

  ls_awart-sign   = 'I'.
  ls_awart-option = 'EQ'.

  MODIFY lrg_awart FROM ls_awart TRANSPORTING sign option
   WHERE sign   IS INITIAL
     AND option IS INITIAL.

  SORT lrg_awart BY low.
*...}End

*  r_abstyp     = 'IEQ'.
*  r_abstyp-low = '1025'.  APPEND r_abstyp.
*  r_abstyp-low = '1045'.  APPEND r_abstyp.
*  r_abstyp-low = '1050'.  APPEND r_abstyp.
** by ig.moon 04/20/2009 {
*  r_abstyp-low = '1017'.  APPEND r_abstyp.
*  r_abstyp-low = '1074'.  APPEND r_abstyp.                  "UD1K953484
*  r_abstyp-low = '3300'.  APPEND r_abstyp.                  "UD1K954709
** }
*  r_subty1     = 'IEQ'.
*  r_subty1-low = '2000'.  APPEND r_subty1.
*  r_subty1-low = '3303'.  APPEND r_subty1.                  "UD1K955008

  __cls it_p2001.

  SELECT pernr awart begda endda abwtg stdaz
    INTO CORRESPONDING FIELDS OF TABLE it_p2001
    FROM pa2001
    FOR ALL ENTRIES IN it_pernr
        WHERE  pernr   = it_pernr-pernr
          AND ( begda IN s_date OR endda IN s_date )
*          AND awart  IN r_abstyp . "Commented T00281
         AND awart IN lrg_awart.

  SELECT pernr awart  begda endda abwtg stdaz
    APPENDING CORRESPONDING FIELDS OF TABLE it_p2001
    FROM pa2002
    FOR ALL ENTRIES IN it_pernr
            WHERE  pernr   = it_pernr-pernr
           AND ( begda IN s_date OR endda IN s_date )
*           AND awart  IN r_subty1 . "Commented T00281
         AND awart IN lrg_awart.

  READ TABLE it_p2001 INDEX 1.
  IF sy-subrc EQ 0.
    __cls it_abstx.
    SELECT * INTO TABLE it_abstx
      FROM t554t
      WHERE sprsl = 'EN'.
  ENDIF.

  SORT it_abstx BY awart.

  LOOP AT it_p2001.
    $ix = sy-tabix.
    CLEAR it_abstx.
    READ TABLE it_abstx WITH KEY awart = it_p2001-awart BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_p2001-atext = it_abstx-atext.
      MODIFY it_p2001 INDEX $ix TRANSPORTING atext.
    ENDIF.
  ENDLOOP.

  SORT it_p2001 BY pernr begda.

ENDFORM.                    " get_absence_day
*&---------------------------------------------------------------------*
*&      Form  move_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.

  PERFORM get_it_kostl.

  __cls gt_out.
  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.

    READ TABLE it_kostl WITH KEY kostl = it_row_tab-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-ktext = it_kostl-ktext.
    ENDIF.

    IF gt_out-stat2 EQ '3'.
      gt_out-icon = icon_led_green.
    ELSE.
      gt_out-icon = icon_led_red.
    ENDIF.
    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " move_out
*&---------------------------------------------------------------------*
*&      Form  error_handling
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PERNR_PERNR  text
*      -->P_1241   text
*      -->P_1242   text
*      -->P_1243   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM error_handling USING pernr arbgb msgty msgno
                          msgv1 msgv2 msgv3 msgv4.
  CLEAR error_tab.
  error_tab-pernr = pernr.
  error_tab-arbgb = arbgb.
  error_tab-msgty = msgty.
  error_tab-msgno = msgno.
  error_tab-msgv1 = msgv1.
  error_tab-msgv2 = msgv2.
  error_tab-msgv3 = msgv3.
  error_tab-msgv4 = msgv4.
  APPEND error_tab.
  REJECT.
ENDFORM.                    "error_handling
*&---------------------------------------------------------------------*
*&      Form  build_psp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_psp.

  CALL FUNCTION 'HR_PERSONAL_WORK_SCHEDULE'
    EXPORTING
      pernr             = pernr-pernr
      begda             = pn-begda
      endda             = pn-endda
      switch_activ      = switch_active
      i0001_i0007_error = '0'
      read_cluster      = rdclust
    TABLES
      i0000             = p0000
      i0001             = p0001
*     i0002             = p0002
      i0007             = p0007
      i2001             = p2001
      i2002             = p2002
      i2003             = p2003
      perws             = psp
    EXCEPTIONS
      error_occured     = 1
      abort_occured     = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
  ENDIF.

  CALL FUNCTION 'HR_WORK_SCHEDULE_TIMES'
    EXPORTING
      pernr         = pernr-pernr
      begda         = pn-begda
      endda         = pn-endda
    TABLES
      i0001         = p0001
      i0007         = p0007
      i2003         = p2003
      perws         = psp
      daygen        = daygen
      dayint        = dayint
    EXCEPTIONS
      error_occured = 1
      perws_error   = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " build_psp

*---------------------------------------------------------------------*
*       FORM count_time_infotypes                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PSP_DATUM                                                     *
*  -->  COUNTER                                                       *
*---------------------------------------------------------------------*
FORM count_time_infotypes USING    psp_datum
             CHANGING counter LIKE ptev_rep_h-itanz.
  DEFINE macro_count_200x.
    loop at &1 where begda le psp_datum  and
                    endda ge psp_datum.
      counter  = counter + 1.
    endloop.
  END-OF-DEFINITION.

  CLEAR counter.

  DESCRIBE TABLE i2003 LINES counter.  "XLTL9CK018334

  macro_count_200x p2001.
  macro_count_200x p2002.
* macro_count_200x i2003. "XLTL9CK018334
  macro_count_200x p2004.
  macro_count_200x p2005.

ENDFORM.                    "count_time_infotypes
*&---------------------------------------------------------------------*
*&      Form  get_it_kostl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_kostl.
  CHECK it_kostl[] IS INITIAL.


  SELECT kostl ktext INTO TABLE it_kostl
  FROM cskt
  WHERE spras EQ sy-langu
    AND datbi EQ '99991231'.

  SORT it_kostl BY kostl.

ENDFORM.                    " get_it_kostl
*&---------------------------------------------------------------------*
*&      Form  get_overtime_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_overtime_day.

  DATA $ix TYPE i.
  DATA $tprog TYPE  tprog.

  RANGES r_subty1 FOR t554s-subty.
  r_subty1     = 'IEQ'.
  r_subty1-low = '1001'.  APPEND r_subty1.
  r_subty1-low = '1003'.  APPEND r_subty1.
  r_subty1-low = '1004'.  APPEND r_subty1.
  r_subty1-low = '1005'.  APPEND r_subty1.
  r_subty1-low = '1006'.  APPEND r_subty1.
  r_subty1-low = '1007'.  APPEND r_subty1.

  __cls it_p2002.

  SELECT pernr awart  begda endda abwtg stdaz
    INTO CORRESPONDING FIELDS OF TABLE it_p2002
    FROM pa2002
    FOR ALL ENTRIES IN it_pernr
            WHERE  pernr   = it_pernr-pernr
           AND ( begda IN s_date OR endda IN s_date )
           AND awart  IN r_subty1 .

  READ TABLE it_p2002 INDEX 1.
  IF sy-subrc EQ 0.
    __cls it_ovrtx.
    SELECT * INTO TABLE it_ovrtx
      FROM t554t
      WHERE sprsl = 'EN'.
  ENDIF.

  DATA iholiday TYPE STANDARD TABLE OF iscal_day WITH HEADER LINE.
  __cls iholiday.

  CALL FUNCTION 'HOLIDAY_GET'
    EXPORTING
      holiday_calendar = 'U1'
      date_from        = pn-begda
      date_to          = pn-endda
    TABLES
      holidays         = iholiday.

  SORT iholiday BY date.
  SORT it_ovrtx BY awart.

  LOOP AT it_p2002.
    $ix = sy-tabix.

    READ TABLE it_pernr WITH KEY pernr = it_p2002-pernr BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE iholiday WITH KEY date = it_p2002-begda BINARY SEARCH.

    IF sy-subrc NE 0.

      CALL FUNCTION 'DATE_COMPUTE_DAY'
        EXPORTING
          date = it_p2002-begda
        IMPORTING
          day  = exp_day.

      IF it_pernr-schkz EQ '4001'.
        IF exp_day EQ 2 OR exp_day EQ 3 OR exp_day EQ 4.
        ELSE.
          DELETE it_p2002 INDEX $ix.
          CONTINUE.
        ENDIF.
      ELSEIF it_pernr-schkz(4) = '8000'.
        CALL FUNCTION 'Z_CO_GET_DWS_IG'
          EXPORTING
            schkz                          = it_pernr-schkz
            datum                          = it_p2002-begda
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
        IF $tprog EQ '1007'.
        ELSE.
          DELETE it_p2002 INDEX $ix.
          CONTINUE.
        ENDIF.
      ELSE.
        IF exp_day >= 6.
        ELSE.
          DELETE it_p2002 INDEX $ix.
          CONTINUE.
        ENDIF.
      ENDIF.

    ENDIF.

    " holiday or weekend.

    CLEAR it_ovrtx.
    READ TABLE it_ovrtx WITH KEY awart = it_p2002-awart BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_p2002-atext = it_ovrtx-atext.
      MODIFY it_p2002 INDEX $ix TRANSPORTING atext.
    ENDIF.

  ENDLOOP.


*  Planned working time
  SORT it_p2002 BY pernr begda.


ENDFORM.                    " get_overtime_day
