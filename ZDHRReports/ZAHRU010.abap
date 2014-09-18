*----------------------------------------------------------------------
* Program ID        : ZAHRU010
* Title             : [HR] No Punch No Absence Code Report...
* Created on        : 4/22/2009
* Created by        : I.G.MOON
* Specifications By : EUNA LEE
* Description       : [HR] No Punch No Absence Code Report
*----------------------------------------------------------------------
*& Date        User       Transport    Description
*  09/15/2010  Valerian   UD1K949696   Include Attendance or Absence
*                                      Type '1069' in data selection
*  07/27/2011  Valerian   UD1K952606   Repair NoPunchNoAbsenceCode
*                                      report regarding 8000 shifts
*  06/19/2013  T00303     UD1K957415   U1: Apply Archiving
*----------------------------------------------------------------------
REPORT zahru010 MESSAGE-ID zmco.

TABLES: pa0000,pa0001, t526, t527x, *zshrattncor, abdbg, sscrfields,
zthrattncor,catsdb, hrp1001,t501,t503k,pa0007.

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.

INCLUDE <icon>.                        " icon

DATA i_t526 LIKE t526 OCCURS 0 WITH HEADER LINE.
DATA i_t527x LIKE t527x OCCURS 0 WITH HEADER LINE.

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

DATA: BEGIN OF t_t526 OCCURS 0,
        sachx LIKE t526-sachx ,
        sachn LIKE t526-sachn ,
        admncode(100) TYPE c  ,
      END OF t_t526           .

DATA: BEGIN OF it_here_pernr OCCURS 0,
        pernr LIKE zshrattncor-pernr,
END   OF it_here_pernr.

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

DATA: BEGIN OF it_status OCCURS 0,
        pernr LIKE pa0000-pernr,
        begda LIKE pa0000-begda,
        massn LIKE pa0000-massn,
        massg LIKE pa0000-massg,
        stat2 LIKE pa0000-stat2,
      END OF it_status           .

DATA: BEGIN OF it_ws OCCURS 0,
        schkz	TYPE schkn,
        rtext	TYPE retext,
        anzsh TYPE anzschicht,
        tprog TYPE tprog,
      END OF it_ws           .

DATA: BEGIN OF it_kostx OCCURS 0,
        kostl LIKE cskt-kostl ,
        kostx LIKE cskt-ktext ,
      END OF it_kostx           .

DATA: BEGIN OF it_pernr OCCURS 0,
        pernr LIKE zshrattncor-pernr,
        employeenumber LIKE zthr_bhisthmma-employeenumber,
        sname  LIKE pa0001-sname,
        kostl  LIKE pa0001-kostl,
        orgeh  LIKE pa0001-orgeh,
        ename  LIKE pa0001-ename,
        sachz  LIKE pa0001-sachz,
        schkz  LIKE pa0007-schkz,
        stat2  LIKE pa0000-stat2,
        massn  LIKE pa0000-massn,
        perflg,
        persg  LIKE pa0001-persg,
        persk  LIKE pa0001-persk,
*        nachn  LIKE pa0002-nachn,
*        vorna  LIKE pa0002-vorna,
END   OF it_pernr.

DATA: BEGIN OF it_calc OCCURS 0,
        pernr(10),
        flag,
        begda LIKE pa0000-begda,
        endda LIKE pa0000-endda,
        massn TYPE massn,
        cnt TYPE i,
        massg TYPE massg,
      END OF it_calc.

DATA: BEGIN OF it_attendance OCCURS 0,
        pernr LIKE zshrattncor-pernr,
        awart TYPE awart,
END   OF it_attendance.

*DATA: BEGIN OF it_pernr_text OCCURS 0,
*        pernr LIKE zshrattncor-pernr,
*        nachn  LIKE pa0002-nachn,
*        vorna  LIKE pa0002-vorna,
*END   OF it_pernr_text.

TYPES: BEGIN OF ty_row_tab,
        pernr LIKE zshrattncor-pernr,
        cdate  TYPE datum,
        sname  LIKE pa0001-sname,
*        nachn  LIKE pa0002-nachn,
*        vorna  LIKE pa0002-vorna,
        sachz  LIKE pa0001-sachz,
        kostl  LIKE pa0001-kostl,
        kostx LIKE cskt-ktext ,
        orgeh  LIKE pa0001-orgeh,
        orgtx  TYPE orgtx,
        anzshtxt TYPE zanzshtxt,
        schkz  LIKE pa0007-schkz,
        sachn LIKE t526-sachn,
        $tprog TYPE  tprog,
        anzsh TYPE anzschicht,
       END OF ty_row_tab.

DATA: BEGIN OF it_pernr_for_name OCCURS 0,
        pernr LIKE zshrattncor-pernr,
END   OF it_pernr_for_name.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES: END OF ty_out.
DATA iholiday TYPE STANDARD TABLE OF iscal_day WITH HEADER LINE.
DATA : wa_fabkl    LIKE  t001w-fabkl. "Factory calendar key

DATA i_zthrattncor LIKE zthrattncor OCCURS 0 WITH HEADER LINE.
DATA  : gt_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

DATA $ix TYPE i.

DATA: w_sachn      LIKE t526-sachn            ,
      w_enddate(8) TYPE c VALUE '99991231'    .
DATA $tprog TYPE  tprog.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE u_break.
  if p_debug eq true.
    break-point.
  endif.
END-OF-DEFINITION.

DEFINE __change_desc.
  read table p_grouplist with key groupname = &1 binary search.
  if sy-subrc eq 0.
    p_grouplist-descript = &2.
    modify p_grouplist index sy-tabix transporting descript.
  endif.
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DATA: g_error(1).
DATA: g_count TYPE i.

DATA :top_line    TYPE i,
      line_count  TYPE i,
      tab_lines   TYPE i,
      bottom_line TYPE i.

DATA g_date TYPE datum.
DATA $text(50).
DATA $pernr(20). " TYPE pernr_d.
DATA : g_wotnr TYPE p.

*- U1 Start
DATA: gt_zthrattncor_a TYPE TABLE OF zthrattncor WITH HEADER LINE,
      gt_zthr_bhisthmma_a TYPE TABLE OF zthr_bhisthmma WITH HEADER LINE.
*- U1 End

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_sachx FOR t526-sachx NO INTERVALS.
SELECT-OPTIONS: s_orgeh FOR pa0001-orgeh NO INTERVALS.
SELECT-OPTIONS: s_date FOR sy-datum OBLIGATORY NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK block1.
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
SELECT-OPTIONS s_persg  FOR t501-persg NO INTERVALS
                                 MODIF ID opt..
SELECT-OPTIONS s_persk  FOR t503k-persk NO INTERVALS
                                 MODIF ID opt..
SELECT-OPTIONS: s_pernr FOR pa0001-pernr NO INTERVALS." NO-DISPLAY.
SELECT-OPTIONS: s_schkz FOR pa0007-schkz NO INTERVALS." NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK block2.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
*  PERFORM default_variant.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_sachx-low.
  PERFORM tmcode_input_help CHANGING s_sachx-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_sachx-high.
  PERFORM tmcode_input_help CHANGING s_sachx-high.

AT SELECTION-SCREEN OUTPUT.
  PERFORM CHECK_DATA.

START-OF-SELECTION.

  PERFORM initialize            .
  PERFORM show_progress USING 'Get pernr...' 10.
  PERFORM get_basic.

  g_date = s_date-low - 1.

  __cls : it_row_tab.
  __cls iholiday.

  IF s_date-high IS INITIAL.
    s_date-high = s_date-low.
  ENDIF.

*  CALL FUNCTION 'HOLIDAY_GET'
*       EXPORTING
*            holiday_calendar = 'U1'
*            date_from        = s_date-low
*            date_to          = s_date-high
*       TABLES
*            holidays         = iholiday.

  CALL FUNCTION 'HOLIDAY_GET'
    EXPORTING
      factory_calendar = wa_fabkl
      date_from        = s_date-low
      date_to          = s_date-high
    TABLES
      holidays         = iholiday.

  SORT iholiday BY date.

  DO 1000 TIMES.
    ADD 1 TO g_date.
    CHECK g_date IN s_date.

* Change check order from none-production day to absence to
* check none-production day in the case of no door history {

    CALL FUNCTION 'DAY_IN_WEEK'
      EXPORTING
        datum = g_date
      IMPORTING
        wotnr = g_wotnr.

*    READ TABLE iholiday WITH KEY date = g_date BINARY SEARCH.
*    IF sy-subrc EQ 0. " holiday.
**      IF g_wotnr < 6.
*      CONTINUE.
**      ENDIF.
*    ENDIF.
*

    CONCATENATE 'Now processing...'
    g_date INTO $text.
    PERFORM show_progress USING $text ''.

    PERFORM get_pernr USING g_date.

    PERFORM get_stauts USING g_date.


    PERFORM get_door_history USING g_date.
    PERFORM get_attendance USING g_date.

    PERFORM check_absence TABLES iholiday
                          USING g_date g_wotnr g_date.

  ENDDO.

  PERFORM show_progress USING 'Preparing display...' 80.
  PERFORM fill_table.
  READ TABLE it_row_tab INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No data found'.
    g_error = true.
  ENDIF.

END-OF-SELECTION.
  CHECK g_error EQ space.
  PERFORM move_out.

  PERFORM set_output.

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
*&      Form  initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize.

  CLEAR g_error.
  __cls : i_t526, i_t527x, it_kostx.

  SELECT * INTO TABLE i_t526 FROM t526 WHERE sachx IN s_sachx.
  SELECT * INTO TABLE i_t527x FROM t527x WHERE sprsl EQ sy-langu
                                           AND orgeh IN s_orgeh.

  SELECT kostl ktext INTO TABLE it_kostx FROM cskt
  WHERE spras EQ sy-langu
    AND datbi EQ '99991231'.

  SORT : i_t526 BY sachx,
         i_t527x BY orgeh,
         it_kostx BY kostl.

  CLEAR wa_fabkl.
  SELECT SINGLE fabkl
         INTO wa_fabkl
         FROM t001w
         WHERE werks EQ 'P001'.


ENDFORM.                    " initialize
*&---------------------------------------------------------------------*
*&      Form  move_out
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
    READ TABLE i_t526 WITH KEY sachx = it_row_tab-sachz BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-sachn = i_t526-sachn.
    ENDIF.
    READ TABLE i_t527x WITH KEY orgeh = it_row_tab-orgeh BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-orgtx = i_t527x-orgtx.
    ENDIF.
    READ TABLE it_kostx WITH KEY kostl = it_row_tab-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-kostx = it_kostx-kostx.
    ENDIF.
    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " move_out
*&---------------------------------------------------------------------*
*&      Form  default_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_variant.
  DATA: h_subrc   TYPE sysubrc,
        h_repid   TYPE syrepid,
        h_variant TYPE raldb_vari.

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
*&      Form  tmcode_input_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_S_SACHX_LOW  text
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

*---------------------------------------------------------------------*
*       FORM add_fields                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_TABNAME                                                     *
*  -->  P_FIELDNAME                                                   *
*  -->  P_FLAG                                                        *
*---------------------------------------------------------------------*
FORM add_fields USING  p_tabname p_fieldname p_flag.
  help_field-tabname = p_tabname.
  help_field-fieldname = p_fieldname.
  help_field-selectflag = p_flag.
  APPEND help_field.
  CLEAR help_field.
ENDFORM.                    " add_fields

*---------------------------------------------------------------------*
*       FORM value_help                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_J                                                           *
*---------------------------------------------------------------------*
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
*&      Form  get_pernr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pernr USING check_date.

  __cls it_pernr.

  SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
            b~schkz a~persg a~persk c~stat2 c~massn
            INTO CORRESPONDING FIELDS OF TABLE it_pernr
               FROM pa0001 AS a INNER JOIN pa0007 AS b
                 ON b~pernr EQ a~pernr
                 INNER JOIN pa0000 AS c
                 ON c~pernr EQ b~pernr
                 FOR ALL ENTRIES IN t_t526
                 WHERE a~sachz = t_t526-sachx
                   AND a~begda LE check_date
                   AND a~endda GE check_date
                   AND b~begda LE check_date
                   AND b~endda GE check_date
                   AND c~begda LE check_date
                   AND c~endda GE check_date
                   AND a~pernr IN s_pernr
                   AND c~stat2 NE '0'
                   AND a~pernr IN s_pernr
                   AND a~persg IN s_persg
                   AND a~persk IN s_persk
                   AND a~orgeh IN s_orgeh
                   AND b~schkz IN s_schkz
                   %_HINTS ORACLE 'FIRST_ROWS(10)'.

  LOOP AT it_pernr.
    $ix = sy-tabix.
    it_pernr-employeenumber = it_pernr-pernr+2.
    MODIFY it_pernr INDEX $ix TRANSPORTING employeenumber.
  ENDLOOP.


ENDFORM.                    " get_pernr
*&---------------------------------------------------------------------*
*&      Form  get_stauts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DATE  text
*----------------------------------------------------------------------*
FORM get_stauts USING check_date.


  CHECK NOT it_pernr[] IS INITIAL.
  __cls it_status.

  SELECT pernr  begda massn massg stat2 INTO TABLE it_status
  FROM pa0000
  FOR ALL ENTRIES IN it_pernr
  WHERE pernr EQ it_pernr-pernr
    AND begda <= check_date
  %_HINTS ORACLE 'FIRST_ROWS(10)'.

  SORT it_status BY pernr ASCENDING
                    begda DESCENDING .

  DELETE ADJACENT DUPLICATES FROM it_status
      COMPARING pernr.

  SORT it_status BY pernr.

ENDFORM.                    " get_stauts
*&---------------------------------------------------------------------*
*&      Form  get_attn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DATE  text
*----------------------------------------------------------------------*
FORM get_attn USING check_date.

  SELECT employeenumber readerid rdate rtime badge
    FROM zthr_bhisthmma
      INTO CORRESPONDING FIELDS OF TABLE itab
      FOR ALL ENTRIES IN it_pernr
      WHERE employeenumber EQ  it_pernr-employeenumber
        AND rdate EQ check_date
      %_HINTS ORACLE 'FIRST_ROWS(10)'.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_zthr_bhisthmma_2 USING check_date.
  ENDIF.
*- U1 End

ENDFORM.                    " get_attn
*&---------------------------------------------------------------------*
*&      Form  view_from_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_door_history USING check_date.

  READ TABLE it_pernr INDEX 1.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  __cls it_here_pernr.

  SELECT pernr INTO TABLE it_here_pernr
  FROM zthrattncor
  FOR ALL ENTRIES IN it_pernr
  WHERE rdate EQ check_date
    AND pernr EQ it_pernr-pernr
    AND zhere EQ true
    AND pernr IN s_pernr.

  SORT it_here_pernr BY pernr.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_zthrattncor  USING  check_date.
  ENDIF.
*- U1 End

ENDFORM.                    " view_from_table

*---------------------------------------------------------------------*
*       FORM fill_table                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fill_table.

  __cls it_pernr_for_name.

  LOOP AT it_row_tab.
    it_pernr_for_name-pernr = it_row_tab-pernr.
    APPEND it_pernr_for_name.
  ENDLOOP.

  SORT it_pernr_for_name.
  DELETE ADJACENT DUPLICATES FROM it_pernr_for_name
      COMPARING pernr.

  DESCRIBE TABLE it_pernr_for_name LINES g_count.

*  __cls it_pernr_text.

*  SELECT a~pernr b~nachn b~vorna
*            INTO CORRESPONDING FIELDS OF TABLE it_pernr_text
*               FROM pa0001 AS a INNER JOIN pa0002 AS b
*                 ON b~pernr EQ a~pernr
*                 FOR ALL ENTRIES IN it_pernr_for_name
*                 WHERE a~pernr EQ it_pernr_for_name-pernr
*                   AND a~begda LE s_date-low
*                   AND a~endda GE s_date-low
*                   AND b~begda LE s_date-low
*                   AND b~endda GE s_date-low
*                %_HINTS ORACLE 'FIRST_ROWS(10)'.
*
*  SORT it_pernr_text BY pernr.
*
*  LOOP AT it_row_tab.
*    $ix = sy-tabix.
*    READ TABLE it_pernr_text WITH KEY pernr = it_row_tab-pernr
*    BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      it_row_tab-nachn = it_pernr_text-nachn.
*      it_row_tab-vorna = it_pernr_text-vorna.
*      MODIFY it_row_tab INDEX $ix TRANSPORTING nachn vorna.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " view_from_table

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

    if  &2 eq 'ZHERE' or &2 eq 'ZNOTHERE'.
      gs_fieldcat-checkbox = 'X'.
    endif.

    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.


  __catalog :
    'X'  'SACHZ'     'TAdmin'        3  'CHAR' '' '' '',
    'X'  'KOSTL'     'Cst.C'        10  'CHAR' '' '' '',
    'X'  'KOSTX'     'Cost.C Desc'  20  'CHAR' '' '' '',
    ' '  'ANZSHTXT'  'Shift'         3  'CHAR' '' '' '',
    ' '  'ORGEH'     'Org.Unit'      8  'CHAR' '' '' '',
    ' '  'ORGTX'     'Org.Name'     25  'CHAR' '' '' '',
    ' '  'SCHKZ'     'WS rule'       8  'CHAR' '' '' '',
    ' '  'PERNR'     'Emp#'         10  'CHAR' '' '' '',
    ' '  'SNAME'     'Name'         30  'CHAR' '' '' '',
    ' '  'CDATE'     'Date'          8  'DATS' '' '' ''.


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
    'SACHZ'        ' ' 'X' 'X' 'X',
    'KOSTL'        ' ' 'X' 'X' 'X',
    'KOSTX'        ' ' 'X' 'X' 'X',
    'ANZSHTXT'     ' ' 'X' 'X' 'X',
    'ORGEH'        ' ' 'X' 'X' 'X',
    'ORGTX'        ' ' 'X' 'X' 'X',
    'SCHKZ'        ' ' 'X' 'X' 'X',
    'PERNR'        ' ' 'X' 'X' 'X',
    'SNAME'        ' ' 'X' 'X' 'X',
    'CDATE'        ' ' 'X' 'X' 'X'.

ENDFORM.                    " SORT_BUILD
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

ENDFORM.                    "USER_COMMAND

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  DATA $count(10).

  REFRESH gt_listheader.

  WRITE g_count TO $count LEFT-JUSTIFIED.

  l_text = '[HR] No Punch No Absence Code Report'.
  PERFORM set_header_line USING:
          'P' 'H' ''      l_text       ''.

  IF s_date-low EQ s_date-high OR s_date-high IS INITIAL.
    PERFORM set_header_line USING:
            'D' 'S' 'Date:' s_date-low ''.
  ELSE.
    PERFORM set_header_line USING:
            'D' 'S' 'Period:' s_date-low s_date-high.
  ENDIF.

  PERFORM set_header_line USING:
          'P' 'S' 'Report Total:' $count '',
          'D' 'S' 'Report Date:' sy-datum ''.

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
*&      Form  get_basic
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_basic.

  w_sachn = '%NO LONGER VALID%'.
  IF NOT s_sachx[] IS INITIAL.
    CLEAR w_sachn.
  ENDIF.

  SELECT sachx sachn
               FROM t526
               INTO TABLE t_t526
               WHERE sachx IN s_sachx
                 AND sachn NOT LIKE w_sachn.


ENDFORM.                    " get_basic
*&---------------------------------------------------------------------*
*&      Form  check_absence
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_DATE  text
*----------------------------------------------------------------------*
FORM check_absence  TABLES iholiday STRUCTURE iscal_day
                    USING check_date p_weekend g_date.


  LOOP AT it_pernr.
    $ix = sy-tabix.
    READ TABLE it_status WITH KEY pernr = it_pernr-pernr
    BINARY SEARCH.
    IF sy-subrc EQ 0 AND ( it_status-stat2 EQ '0' OR
                            it_status-stat2 EQ '2' ).
      DELETE it_pernr INDEX $ix. " not absence
    ENDIF.
  ENDLOOP.

  LOOP AT it_pernr.
    $ix = sy-tabix.
    READ TABLE it_here_pernr WITH KEY pernr = it_pernr-pernr
    BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE it_attendance WITH KEY pernr = it_pernr-pernr
      BINARY SEARCH.
      IF sy-subrc NE 0.
        DELETE it_pernr INDEX $ix. " not absence
      ENDIF.
    ENDIF.
  ENDLOOP.

  __cls it_ws.

  LOOP AT it_pernr.
    $ix = sy-tabix.
    it_ws-schkz = it_pernr-schkz.
    COLLECT it_ws.
  ENDLOOP.

  LOOP AT it_ws.
    $ix = sy-tabix.
    CALL FUNCTION 'Z_CO_GET_DWS_IG'
      EXPORTING
        schkz                          = it_ws-schkz
        datum                          = check_date
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
      WHEN '0002' OR '1003' OR '1002' OR '1009'.
        it_ws-anzsh = '2'.
      WHEN OTHERS.
        it_ws-anzsh = '1'.
    ENDCASE.
    MODIFY it_ws INDEX $ix TRANSPORTING tprog anzsh.

  ENDLOOP.

  SORT it_ws BY schkz.

  LOOP AT it_pernr.
    $ix = sy-tabix.

    IF it_pernr-schkz EQ '4001'.
      IF p_weekend EQ '2' OR p_weekend EQ '3' OR p_weekend EQ '4'.
        DELETE it_pernr INDEX $ix.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF it_pernr-schkz(4) EQ '8000'.

      READ TABLE it_ws WITH KEY schkz = it_pernr-schkz BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF it_ws-tprog EQ '1007'.
          DELETE it_pernr INDEX $ix.
          CONTINUE.
        ENDIF.
      ENDIF.
*   ENDIF.                                                  "UD1K952606
    ELSE.                                                   "UD1K952606
      IF p_weekend > 5. " SAT ? or SUN ?

        IF it_pernr-persk NE 'U0'.
          DELETE it_pernr INDEX $ix.
          CONTINUE.
        ENDIF.

        READ TABLE iholiday WITH KEY date = g_date BINARY SEARCH.
        IF sy-subrc EQ 0. " holiday.
          DELETE it_pernr INDEX $ix. " not absence
        ELSE.

          READ TABLE it_attendance WITH KEY pernr = it_pernr-pernr
          BINARY SEARCH.
          IF sy-subrc EQ 0.
            DELETE it_pernr INDEX $ix. " not absence
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.                                                  "UD1K952606
  ENDLOOP.

  LOOP AT it_pernr.
    $ix = sy-tabix.

* Action
    IF it_pernr-stat2 EQ '1'.
      CONTINUE.
    ENDIF.

    IF it_pernr-stat2 EQ '3' AND it_pernr-massn EQ 'ZB'.
      CONTINUE.
    ENDIF.

    SELECT SINGLE employeenumber INTO $pernr
    FROM zthr_bhisthmma
               WHERE rdate EQ check_date
                 AND rtime >= '060000'
                 AND employeenumber EQ it_pernr-employeenumber .
*- U1 Start
    IF p_arch EQ 'X' AND sy-subrc NE 0.
      PERFORM archive_read_zthr_bhisthmma USING check_date.
    ENDIF.
*- U1 End

    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

    SELECT SINGLE pernr INTO $pernr
    FROM pa2002 WHERE pernr EQ it_pernr-pernr
                 AND  begda EQ check_date
                 AND  endda EQ check_date
                 AND ( awart EQ '1061' OR awart EQ '1062' OR
                       awart EQ '1063' OR awart EQ '1065' OR
                       awart EQ '1066' OR awart EQ '1058' OR "UD1K949696
** Furong on 07/14/14
*                       awart EQ '1069' ) .                  "UD1K949696
                       awart EQ '1069'  or awart EQ '5018') .
** )
    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

* Absence
    SELECT SINGLE pernr INTO $pernr
    FROM pa2001 WHERE pernr EQ it_pernr-pernr
                 AND  begda EQ check_date
                 AND  endda EQ check_date
                 AND  sprps EQ space.
    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

* Training
    SELECT SINGLE * FROM hrp1001
    WHERE otype EQ 'P'
      AND objid EQ it_pernr-pernr
      AND subty EQ 'B034'
      AND begda EQ check_date
      AND varyf NE space.

    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

* LOA check again
    IF it_pernr-stat2 EQ '3'.
      __cls it_calc.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_calc
      FROM pa0000
      WHERE pernr EQ it_pernr-pernr
        AND stat2 NE '0'.

      PERFORM get_cnt USING check_date.

      READ TABLE it_calc INDEX 1.
      IF sy-subrc EQ 0.
        IF it_calc-massn EQ 'ZB'.
          CONTINUE.
        ENDIF.
      ENDIF.

    ENDIF.

    MOVE-CORRESPONDING it_pernr TO it_row_tab.
    it_row_tab-cdate = check_date.

    APPEND it_row_tab.

  ENDLOOP.

  LOOP AT it_row_tab.
    $ix = sy-tabix.
    READ TABLE it_ws WITH KEY schkz = it_row_tab-schkz BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-anzsh = it_ws-anzsh.
      IF it_row_tab-anzsh EQ '1'.
        it_row_tab-anzshtxt = '1st'.
      ENDIF.
      IF it_row_tab-anzsh EQ '2'.
        it_row_tab-anzshtxt = '2nd'.
      ENDIF.
      MODIFY it_row_tab INDEX $ix TRANSPORTING anzsh anzshtxt.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " check_absence
*&---------------------------------------------------------------------*
*&      Form  get_attendance
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_DATE  text
*----------------------------------------------------------------------*
FORM get_attendance USING p_date.
  __cls it_attendance.

  READ TABLE it_pernr INDEX 1.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  SELECT pernr awart INTO TABLE it_attendance
  FROM pa2002
  FOR ALL ENTRIES IN it_pernr
  WHERE pernr EQ it_pernr-pernr
  AND begda EQ p_date
  AND ( awart EQ '2000' OR awart EQ '2001'
     OR awart EQ '1019' OR awart EQ '1026' ).

  SORT it_attendance.

ENDFORM.                    " get_attendance


*---------------------------------------------------------------------*
*       FORM get_cnt                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_cnt USING check_date.

  DATA : $begda TYPE begda,
         $endda TYPE endda,
         $tmpda TYPE endda,
         $cnt TYPE i,
         $flag1,$flag2.

  DATA  $$begda TYPE begda.
  DATA $it_calc_pernr LIKE it_calc OCCURS 0 WITH HEADER LINE.


  DATA $ix TYPE i.
  DATA $ixp TYPE i.

  DELETE it_calc WHERE ( massn NE 'ZC' AND massn NE 'Z3' AND
massn NE 'ZB' ).

  SORT it_calc BY begda DESCENDING.

  READ TABLE it_calc INDEX 1.
  CHECK sy-subrc EQ 0.

  $begda = it_calc-begda.
  $endda = it_calc-endda.

  IF $endda < check_date.
    EXIT.
  ELSE.
  ENDIF.

  it_calc-flag = true.
  MODIFY it_calc INDEX 1 TRANSPORTING flag.

  LOOP AT it_calc FROM 2.

    $ix = sy-tabix.
    $tmpda = it_calc-endda + 1.

    IF $tmpda EQ $begda.
      it_calc-flag = true.
      MODIFY it_calc INDEX $ix TRANSPORTING flag.
      $begda = it_calc-begda.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.

  DELETE it_calc WHERE flag EQ false.

  __cls $it_calc_pernr.

  DATA: BEGIN OF it_final_reason OCCURS 0,
          massn TYPE massn,
          massg TYPE massg,
        END OF it_final_reason.

  LOOP AT it_calc.
    it_final_reason-massn = it_calc-massn.
    it_final_reason-massg = it_calc-massg.
    APPEND it_final_reason.
  ENDLOOP.

  SORT it_calc BY begda.

  DO 30 TIMES.
    READ TABLE it_calc INDEX 1.
    IF sy-subrc EQ 0.
      IF it_calc-massn EQ 'Z3'.
        DELETE it_calc INDEX 1.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  READ TABLE it_calc INDEX 1.
  $$begda = it_calc-begda.

  DO 30 TIMES.
    READ TABLE it_final_reason INDEX 1.
    IF sy-subrc EQ 0.

* Begin of HIS20094 - Fix the action type
*      IF it_final_reason-massn EQ 'Z3' AND (
*         it_final_reason-massg EQ '12' OR
*         it_final_reason-massg EQ '14' ).
* End of HIS20094

      IF it_final_reason-massn EQ 'Z3'.
        DELETE it_final_reason INDEX 1.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.

  ENDDO.

  LOOP AT it_calc.
    $it_calc_pernr  = it_calc.
    CLEAR : $it_calc_pernr-flag,
            $it_calc_pernr-begda,
            $it_calc_pernr-endda.
    READ TABLE it_final_reason INDEX 1.
    $it_calc_pernr-massn = it_final_reason-massn.
    $it_calc_pernr-massg = it_final_reason-massg.
    COLLECT  $it_calc_pernr.
  ENDLOOP.

  $it_calc_pernr-begda = $$begda.

  IF $it_calc_pernr-endda IS INITIAL OR $it_calc_pernr-endda >
check_date.
    $it_calc_pernr-cnt = check_date - $it_calc_pernr-begda.
  ELSE.
    $it_calc_pernr-cnt = $it_calc_pernr-endda - $it_calc_pernr-begda.
  ENDIF.

  MODIFY $it_calc_pernr INDEX 1 TRANSPORTING begda cnt.

  it_calc[] = $it_calc_pernr[].

ENDFORM.                    " get_cnt
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTHRATTNCOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_zthrattncor USING p_date.

  TYPES: BEGIN OF ty_zthrattncor,
         rdate TYPE zdrdate,
         sachz TYPE sachx,
         orgeh TYPE orgeh,
         pernr TYPE pernr_d,
         kostl TYPE kostl,
         btrtl TYPE btrtl,
         schkz TYPE schkn,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_zthrattncor.

  DATA: l_handle    TYPE sytabix,
        lt_zthrattncor TYPE TABLE OF zthrattncor WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_zthrattncor TYPE TABLE OF ty_zthrattncor,
        ls_inx_zthrattncor TYPE ty_zthrattncor.

  CONSTANTS: c_zthrattncor_001(14) VALUE 'ZTHRATTNCO_001'.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = c_zthrattncor_001.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_zthrattncor[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_zthrattncor
    FROM (l_gentab)
    FOR ALL ENTRIES IN it_pernr
   WHERE rdate EQ p_date
     AND pernr EQ it_pernr-pernr
     "AND zhere EQ true
     AND pernr IN s_pernr.

  CHECK NOT lt_inx_zthrattncor[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_zthrattncor_a, gt_zthrattncor_a[].
  LOOP AT lt_inx_zthrattncor INTO ls_inx_zthrattncor.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'ZTHRATTNCO'
        archivkey                 = ls_inx_zthrattncor-archivekey
        offset                    = ls_inx_zthrattncor-archiveofs
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
    CLEAR: lt_zthrattncor, lt_zthrattncor[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'ZTHRATTNCOR'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_zthrattncor
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_zthrattncor[] IS INITIAL.

    DELETE lt_zthrattncor WHERE zhere NE true.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_zthrattncor INTO TABLE gt_zthrattncor_a.
  ENDLOOP.

  SORT gt_zthrattncor_a.
  DELETE ADJACENT DUPLICATES FROM gt_zthrattncor_a COMPARING ALL FIELDS.

  LOOP AT gt_zthrattncor_a.
    MOVE-CORRESPONDING gt_zthrattncor_a TO it_here_pernr.
    APPEND it_here_pernr.  CLEAR it_here_pernr.
  ENDLOOP.

  SORT it_here_pernr BY pernr.

ENDFORM.                    " ARCHIVE_READ_ZTHRATTNCOR
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTHR_BHISTHMMA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHECK_DATE  text
*----------------------------------------------------------------------*
FORM archive_read_zthr_bhisthmma  USING  p_date.

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
  CLEAR ls_inx_zthr_bhisthmma.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_inx_zthr_bhisthmma
    FROM (l_gentab)
   WHERE rdate EQ p_date
     AND rtime >= '060000'
     AND employeenumber EQ it_pernr-employeenumber.

  CHECK sy-subrc = 0.

ENDFORM.                    " ARCHIVE_READ_ZTHR_BHISTHMMA
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTHR_BHISTHMMA_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHECK_DATE  text
*----------------------------------------------------------------------*
FORM archive_read_zthr_bhisthmma_2  USING  p_date.

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
    FOR ALL ENTRIES IN it_pernr
   WHERE employeenumber = it_pernr-employeenumber
     AND rdate          = p_date.

  CHECK NOT lt_inx_zthr_bhisthmma[] IS INITIAL.

  LOOP AT lt_inx_zthr_bhisthmma INTO ls_inx_zthr_bhisthmma.
    CLEAR itab.
    MOVE-CORRESPONDING ls_inx_zthr_bhisthmma to itab.
    APPEND itab.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_ZTHR_BHISTHMMA_2
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_DATA .
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_ARCH'.
       SCREEN-ACTIVE = 0.
       SCREEN-INPUT = 0.
       MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_DATA
