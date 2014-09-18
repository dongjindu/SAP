*----------------------------------------------------------------------
* Program ID        : ZRHR_LOA_TRACKING
* Title             : [HR] LOA Tracking Report (Date Range)
* Created on        : 02/14/14
* Created by        : Furong
* Specifications By : Copy from ZAHRU004
* Description       : LOA Tracking Report
*&--------------------------------------------------------------------&*
* Modification Logs
* Date       Developer  Issue No   Description
*
*&--------------------------------------------------------------------&*

REPORT zrhr_loa_tracking MESSAGE-ID zmco.

TABLES: pernr, pa0006.
INFOTYPES: 0000,
           0001,0002,
           0007 MODE n,
           0041 MODE n,
           2001 MODE n,                "Absences
           2002 MODE n,
           2003 MODE n,                "Substitution
           2004 MODE n,                "On-Call Duty
           2005 MODE n.                "Overtime

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.

TYPES: BEGIN OF ty_row_tab,
       groupname TYPE bapiset_groupname,
       descript TYPE bapiset_descript,
       kostl TYPE kostl,
       ktext TYPE ktext,
       pernr(10),
       sname(30),
       nachn  TYPE pad_nachn,
       vorna  TYPE pad_vorna,
       begda TYPE datum,
       subgroup(20),
       loa       TYPE i,
       loa_lt_30 TYPE i,
       loa_ge_30 TYPE i,

       orgeh TYPE orgeh,
       stext TYPE stext,
       massg TYPE massg,
       massn TYPE massn,
       mgtxt TYPE mgtxt,

       cnt(5),

* Feb. 2010 by ig.moon {
       addr(100),
       city(50),
       st(3),
       state(30),
       zip(10),
       country(15),
       sachz TYPE pa0001-sachz,                             "UD1K955714
       sachn TYPE t526-sachn,                               "UD1K955714
* }
END OF ty_row_tab.

DATA: BEGIN OF it_calc OCCURS 0,
        pernr(10),
        flag,
        begda LIKE pa0000-begda,
        endda LIKE pa0000-endda,
        massn TYPE massn,
        cnt TYPE i,
        kostl TYPE kostl,
        sname(30),
        nachn	TYPE pad_nachn,
        vorna	TYPE pad_vorna,
        categ TYPE zhrcatg,
        subgroup(20),
*
        orgeh TYPE orgeh,
        massg TYPE massg,
        sachz TYPE pa0001-sachz,                            "UD1K955714
        sachn TYPE t526-sachn,                              "UD1K955714
*
      END OF it_calc.

DATA it_calc_pernr LIKE it_calc OCCURS 0 WITH HEADER LINE.
DATA $it_calc_pernr LIKE it_calc_pernr OCCURS 0 WITH HEADER LINE.
DATA gt_calc_pernr LIKE it_calc OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_kostl OCCURS 0,
        kostl TYPE kostl,
        ktext TYPE ktext,
      END OF it_kostl .

DATA: BEGIN OF it_orgeh OCCURS 0,
        orgeh TYPE orgeh,
        stext TYPE stext,
      END OF it_orgeh .

DATA: BEGIN OF it_massg OCCURS 0,
        massn TYPE massn,
        massg TYPE massg,
        mgtxt TYPE mgtxt,
      END OF it_massg .

DATA: BEGIN OF it_result OCCURS 0,
        pernr TYPE p_pernr,
        cnt TYPE i,
      END OF it_result .

TYPES BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES END OF ty_out.

DATA: BEGIN OF it_group OCCURS 0,
        kostl TYPE kostl,
        groupname TYPE bapiset_groupname,
        descript TYPE bapiset_descript,
      END OF it_group .

DATA BEGIN OF grouplist OCCURS 0.
        INCLUDE STRUCTURE bapi1112_list.
DATA END OF grouplist.

DATA it503t LIKE t503t OCCURS 0 WITH HEADER LINE.

DATA categ TYPE zhrcatg.
DATA ptext(20).

CONSTANTS:
 c_eg1(1)   TYPE c VALUE   'A',"US-Salary
 c_eg2(1)   TYPE c VALUE   'B',"US-Wage
 c_eg3(1)   TYPE c VALUE   'K',"KR-Salary
 c_kokrs(4) VALUE 'H201'.

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

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

DATA: g_error(1),
      g_repid  LIKE sy-repid.
RANGES s_date FOR sy-datum.

DATA: l_stat2 LIKE p0000-stat2,
      l_massn LIKE p0000-massn,
      l_index LIKE sy-tabix.

*RANGES r_date00 FOR sy-datum.
* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title = '[HR] LOA Tracking Report'.

  PERFORM default_variant.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'PNPTIMR1' OR
       screen-name = 'PNPTIMR2' OR
      screen-name = 'PNPTIMR3' OR
      screen-name = 'PNPTIMR4' OR
      screen-name = 'PNPTIMR5'.
      screen-input = 0.
*      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.

  IF pnpbegda IS INITIAL OR pnpendda IS INITIAL
    OR pnpendda < pnpbegda.
    MESSAGE s000 WITH 'Please input valid dates' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  __cls it_row_tab.
  __cls it_calc.
  __cls it503t.

  SELECT * FROM  t503t INTO TABLE  it503t
  WHERE sprsl EQ sy-langu.
  SORT it503t BY persk.

*  s_date-option = 'BT'.
*  s_date-sign = 'I'.
*  s_date-low = pn-begda.
*  s_date-high = pn-endda.
*  APPEND s_date.

GET pernr.

  PROVIDE * FROM p0001 BETWEEN pn-begda AND pn-endda.
    CLEAR : categ, ptext.
*    CHECK P0001-PERNR IN PNPPERNR.
    PERFORM get_emp_categ  USING    p0001-persg p0001-persk
                           CHANGING categ.
    READ TABLE it503t
    WITH KEY persk = p0001-persk BINARY SEARCH.
    IF sy-subrc EQ 0.
      ptext = it503t-ptext.
    ENDIF.

    SORT p0000 BY pernr begda.
    LOOP AT p0000.
      IF p0000-massn EQ 'ZB' OR p0000-massn EQ 'ZC'.
        l_index = sy-tabix + 1.
        l_stat2 = p0000-stat2.
        l_massn = p0000-massn.
        READ TABLE p0000 INDEX l_index.
        IF sy-subrc = 0 AND p0000-stat2 = l_stat2
           AND p0000-massn <> 'ZD'.
          p0000-massn = l_massn.
          MODIFY p0000 INDEX l_index TRANSPORTING massn.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT p0000.
*      IF p0000-begda <= pn-begda. " AND p0000-endda >= pn-endda.
*      REFRESH: r_date00.
*
*      r_date00-option = 'BT'.
*      r_date00-sign = 'I'.
*      r_date00-low = p0000-begda.
*      r_date00-high = p0000-endda.
*      APPEND r_date00.

      IF pn-endda < p0000-begda OR  pn-begda > p0000-endda.
        CONTINUE.
      ELSE.
        it_calc-pernr = p0000-pernr.
        it_calc-begda = p0000-begda.
        it_calc-endda = p0000-endda.
        it_calc-massn = p0000-massn.
        it_calc-kostl = p0001-kostl.
        it_calc-sname = p0001-sname.
        it_calc-nachn = p0002-nachn.
        it_calc-vorna = p0002-vorna.
        it_calc-categ = categ.
        it_calc-subgroup = ptext.

        it_calc-orgeh = p0001-orgeh.
        it_calc-massg = p0000-massg.

* BEGIN OF UD1K955714
        it_calc-sachz = p0001-sachz.
        SELECT SINGLE sachn INTO it_calc-sachn
          FROM t526
         WHERE werks = 'HMMA'
           AND sachx = p0001-sachz.
* END OF UD1K955714

        IF p0000-massn EQ 'ZB' OR p0000-massn EQ 'ZC'.

          IF pn-begda >= p0000-begda AND pn-endda =< p0000-endda.
            it_calc-cnt = pn-endda -  pn-begda + 1.    " iun between
            APPEND it_calc.
            CLEAR it_calc.
          ELSEIF pn-begda >= p0000-begda AND pn-endda > p0000-endda.
            it_calc-cnt = p0000-endda -  pn-begda + 1.  " partial covered
            APPEND it_calc.
            CLEAR it_calc.
          ELSEIF pn-begda =< p0000-begda AND pn-endda =< p0000-endda
                AND pn-endda >= p0000-begda.
*            it_calc-cnt = p0000-endda -  pn-begda + 1.  " partial covered
            it_calc-cnt = pn-endda -  p0000-begda + 1.
            APPEND it_calc.
            CLEAR it_calc.
          ELSE.
            it_calc-cnt = p0000-endda -  p0000-begda + 1.
            APPEND it_calc.
            CLEAR it_calc.
          ENDIF.
*          IF p0000-endda > pn-endda.
*            it_calc-cnt = pn-endda - p0000-begda + 1.
*          ELSE.
*            it_calc-cnt = p0000-endda - p0000-begda + 1.
*          ENDIF.
*       APPEND it_calc.
*          CLEAR it_calc.


*        ELSE.
*          IF p0000-stat2 EQ '1'.
*            APPEND it_calc.
*            CLEAR it_calc.
*          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    EXIT.
  ENDPROVIDE.

END-OF-SELECTION.

*  DATA : $begda TYPE begda,
*         $endda TYPE endda,
*         $tmpda TYPE endda,
*         $cnt TYPE i,
*         $flag1,$flag2.

  DATA:  l_min_date LIKE sy-datum,
         l_max_date LIKE sy-datum,
         l_flag.

  __cls gt_calc_pernr.


  SORT it_calc BY pernr begda endda.

  LOOP AT it_calc.
    AT NEW pernr.
      l_flag = 'X'.
    ENDAT.
    MOVE-CORRESPONDING it_calc TO it_calc_pernr.
    IF l_flag IS NOT INITIAL.
      l_min_date = it_calc-begda.
      l_max_date = it_calc-endda.
      CLEAR: l_flag.
    ELSE.
      CLEAR: it_calc_pernr-begda,it_calc_pernr-endda.
    ENDIF.
    CLEAR: it_calc_pernr-begda,it_calc_pernr-endda.
    CLEAR: it_calc_pernr-massn, it_calc_pernr-massg.

    COLLECT it_calc_pernr.
    IF l_min_date > it_calc-begda.
      l_min_date = it_calc-begda.
    ENDIF.
    IF l_max_date < it_calc-endda.
      l_max_date = it_calc-endda.
    ENDIF.
    AT END OF pernr.
      READ TABLE it_calc_pernr WITH KEY pernr =  it_calc-pernr.
      IF sy-subrc = 0.
        l_index = sy-tabix.
        it_calc_pernr-begda = l_min_date.
        it_calc_pernr-endda = l_max_date.
        MODIFY it_calc_pernr INDEX l_index.
      ENDIF.
    ENDAT.

  ENDLOOP.
  gt_calc_pernr[] = it_calc_pernr[].

*  LOOP AT it_calc.
*    AT NEW pernr.
*      $flag1 = true.
*    ENDAT.
*
*    IF $flag1 EQ true.
*      CLEAR $flag1.
*      __cls it_calc_pernr.
*    ENDIF.
*
*    MOVE-CORRESPONDING it_calc TO it_calc_pernr.
*    APPEND it_calc_pernr.
*
*    AT END OF pernr.
*      PERFORM get_cnt TABLES it_calc_pernr.
*    ENDAT.
*
*  ENDLOOP.

  PERFORM get_kostl.

  PERFORM get_orgeh.

  __cls it_row_tab.

  LOOP AT it_kostl.

    READ TABLE it_group WITH KEY kostl = it_kostl-kostl
      BINARY SEARCH.
    IF sy-subrc EQ 0 AND it_group-groupname NE space.
      READ TABLE gt_calc_pernr WITH KEY kostl = it_kostl-kostl.
      IF sy-subrc NE 0.
        CLEAR gt_calc_pernr.
        gt_calc_pernr-kostl = it_kostl-kostl.
        APPEND gt_calc_pernr.
      ENDIF.

    ENDIF.

  ENDLOOP.


*  LOOP AT it_orgeh.
*
*    READ TABLE gt_calc_pernr WITH KEY orgeh = it_orgeh-orgeh.
*    IF sy-subrc NE 0.
*      CLEAR gt_calc_pernr.
*      gt_calc_pernr-orgeh = it_orgeh-orgeh.
*      APPEND gt_calc_pernr.
*    ENDIF.
*
*  ENDLOOP.

  SORT it_kostl BY kostl.
  SORT gt_calc_pernr BY pernr.

  LOOP AT gt_calc_pernr.
    CLEAR it_row_tab.
    MOVE-CORRESPONDING gt_calc_pernr TO it_row_tab.
    CLEAR it_row_tab-cnt .

    IF NOT gt_calc_pernr-cnt IS INITIAL.
      WRITE gt_calc_pernr-cnt TO it_row_tab-cnt RIGHT-JUSTIFIED.
    ENDIF.

* by ig.moon 8/13/2009 {
    READ TABLE it_group WITH KEY kostl = gt_calc_pernr-kostl
      BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-groupname = it_group-groupname.
      it_row_tab-descript = it_group-descript.
    ENDIF.
* }

    READ TABLE it_kostl WITH KEY kostl = gt_calc_pernr-kostl
      BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-ktext = it_kostl-ktext.
    ENDIF.

*    IF gt_calc_pernr-pernr NE space.
*      IF gt_calc_pernr-cnt < 30.
*        it_row_tab-loa_lt_30 = '1'.
*      ELSE.
*        it_row_tab-loa_ge_30 = '1'.
*      ENDIF.
*    ENDIF.
*    it_row_tab-loa = it_row_tab-loa_ge_30 + it_row_tab-loa_lt_30.
    APPEND it_row_tab.
  ENDLOOP.

  READ TABLE it_row_tab INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No data was found.'.
    EXIT.
  ENDIF.

  PERFORM move_out.
  PERFORM set_output.

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

ENDFORM.                    "USER_COMMAND

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  REFRESH gt_listheader.

  l_text = '[HR] LOA Tracking Report'.
  PERFORM set_header_line USING:
          'P' 'H' ''      l_text       '',
*         'D' 'S' 'Key Date:' pn-begda ''
          'D' 'S' 'Date:' pn-begda pn-endda.
  .

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

    if  &2 eq 'CNT'.
      gs_fieldcat-just = 'R'.
    endif.

    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :
    'X'  'DESCRIPT'  'Description'  30  'CHAR' '' '' '',
    ' '  'ORGEH'     'Org.'         10  'CHAR' '' '' 'X',
    ' '  'STEXT'     'Org.Text'     40  'CHAR' '' '' '',
    ' '  'MGTXT'     'Action Reason' 40  'CHAR' '' '' '',
    ' '  'PERNR'     'Emp#'         10  'CHAR' '' '' '',
    ' '  'NACHN'     'Last Name'    30  'CHAR' '' '' 'X',
    ' '  'VORNA'     'First Name'   30  'CHAR' '' '' 'X',
    ' '  'KOSTL'     'C.C'           4  'CHAR' '' '' '',
    ' '  'KTEXT'     'Text'         30  'CHAR' '' '' '',
    ' '  'SUBGROUP'  'Subgroup'     10  'CHAR' '' '' '',
*    ' '  'BEGDA'     'Start'         8  'DAT' '' '' 'X',
    ' '  'CNT'       'Days'          5  'DEC' '' '' '',
*    ' '  'LOA'       'LOA Total'    10  'DEC' '' '' '',
*    ' '  'LOA_LT_30' 'LOA  < 30'    10  'DEC' '' '' '',
*    ' '  'LOA_GE_30' 'LOA >= 30'    10  'DEC' '' '' '',
    ' '  'GROUPNAME' 'C.C.Group'    15  'CHAR' '' '' '',
    ' '  'ADDR'       'Address'      100  'CHAR' '' '' '',
    ' '  'CITY'       'City/County'  50  'CHAR' '' '' '',
    ' '  'ST'         'St.'           3  'CHAR' '' '' '',
    ' '  'STATE'      'State'        30  'CHAR' '' '' '',
    ' '  'ZIP'        'Zip Code'     10  'CHAR' '' '' '',
    ' '  'COUNTRY'    'Country'      15  'CHAR' '' '' '',
    ' '  'SACHZ'      'Adm'           3  'CHAR' '' '' '',   "UD1K955714
    ' '  'SACHN'      'Adm Name'     20  'CHAR' '' '' ''.   "UD1K955714

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
     'GROUPNAME'    ' ' 'X' 'X' 'X',
     'DESCRIPT'     ' ' 'X' 'X' 'X',
     'KOSTL'        ' ' 'X' 'X' 'X',
     'KTEXT'        ' ' 'X' 'X' 'X',
     'PERNR'        ' ' 'X' 'X' 'X',
     'NACHN'        ' ' 'X' 'X' 'X',
     'VORNA'        ' ' 'X' 'X' 'X',
     'SUBGROUP'     ' ' 'X' 'X' 'X'.

ENDFORM.                    " SORT_BUILD
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
*&      Form  move_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.

  DATA : $addr1(60),$addr2(40),$state(3),$land1(3).

  __cls gt_out.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.

    CLEAR : $addr1,$addr2,$state,$land1.

    READ TABLE it_orgeh WITH KEY orgeh = it_row_tab-orgeh BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-stext = it_orgeh-stext.
    ENDIF.

    READ TABLE it_massg WITH KEY massn = it_row_tab-massn
                                 massg = it_row_tab-massg
                                 BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-mgtxt = it_massg-mgtxt.
    ENDIF.

    SELECT SINGLE stras locat ort01 state pstlz land1
    INTO ($addr1,$addr2,gt_out-city,$state,gt_out-zip,$land1)
    FROM pa0006
    WHERE pernr EQ it_row_tab-pernr
      AND subty EQ '5'
      AND endda EQ '99991231'.
    IF sy-subrc EQ 0.
      CONCATENATE $addr1 $addr2 INTO gt_out-addr SEPARATED BY space.
    ENDIF.

    SELECT SINGLE bezei INTO gt_out-state FROM t005u
                        WHERE spras EQ sy-langu
                         AND land1 EQ $land1
                         AND bland EQ $state.

    SELECT SINGLE landx INTO gt_out-country FROM t005t
    WHERE spras EQ sy-langu
     AND land1 EQ $land1.

*       city(50),
*       state(30),
*       zip(10),
*       country(30),
    gt_out-st =  $state.
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
*&      Form  get_cnt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CALC_PERNR  text
*----------------------------------------------------------------------*
FORM get_cnt TABLES p_it_calc_pernr STRUCTURE it_calc_pernr.

*  DATA : $begda TYPE begda,
*         $endda TYPE endda,
  DATA:  $tmpda TYPE endda,
         $cnt TYPE i,
         $flag1,$flag2.

  DATA  $$begda TYPE begda.

  DATA $ix TYPE i.
  DATA $ixp TYPE i.

  SORT p_it_calc_pernr BY begda DESCENDING.

  READ TABLE p_it_calc_pernr INDEX 1.
  CHECK sy-subrc EQ 0.

*  $begda = p_it_calc_pernr-begda.
*  $endda = p_it_calc_pernr-endda.

*  IF $endda < pn-endda.
*    EXIT.
*  ELSE.
*  ENDIF.

*  p_it_calc_pernr-flag = true.
*  MODIFY p_it_calc_pernr INDEX 1 TRANSPORTING flag.
*
*  LOOP AT p_it_calc_pernr FROM 2.
*
*    $ix = sy-tabix.
*    $tmpda = p_it_calc_pernr-endda + 1.
*
*    IF $tmpda EQ $begda.
*      p_it_calc_pernr-flag = true.
*      MODIFY p_it_calc_pernr INDEX $ix TRANSPORTING flag.
*      $begda = p_it_calc_pernr-begda.
*    ELSE.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
*
*  DELETE p_it_calc_pernr WHERE flag EQ false.

  __cls $it_calc_pernr.

  DATA: BEGIN OF it_final_reason OCCURS 0,
          massn TYPE massn,
          massg TYPE massg,
        END OF it_final_reason.

  LOOP AT p_it_calc_pernr.
    it_final_reason-massn = p_it_calc_pernr-massn.
    it_final_reason-massg = p_it_calc_pernr-massg.
    APPEND it_final_reason.
  ENDLOOP.

  SORT p_it_calc_pernr BY begda.
  READ TABLE p_it_calc_pernr INDEX 1.
  $$begda = p_it_calc_pernr-begda.

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

  LOOP AT p_it_calc_pernr.
    $it_calc_pernr  = p_it_calc_pernr.
    CLEAR : $it_calc_pernr-flag,
            $it_calc_pernr-begda,
            $it_calc_pernr-endda.
    READ TABLE it_final_reason INDEX 1.
    $it_calc_pernr-massn = it_final_reason-massn.
    $it_calc_pernr-massg = it_final_reason-massg.
    COLLECT  $it_calc_pernr.
  ENDLOOP.

  $it_calc_pernr-begda = $$begda.

*  IF $it_calc_pernr-endda IS INITIAL OR $it_calc_pernr-endda > pn-endda.
*    $it_calc_pernr-cnt = pn-endda - $it_calc_pernr-begda.
*  ELSE.
*    $it_calc_pernr-cnt = $it_calc_pernr-endda - $it_calc_pernr-begda.
*  ENDIF.

  MODIFY $it_calc_pernr INDEX 1 TRANSPORTING begda cnt.

  APPEND LINES OF $it_calc_pernr TO gt_calc_pernr.

ENDFORM.                    " get_cnt
*&---------------------------------------------------------------------*
*&      Form  get_emp_categ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P0001_PERSG  text
*      -->P_P0001_PERSK  text
*      <--P_CATEG  text
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
*&      Form  get_kostl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_kostl.

  CHECK it_kostl[] IS INITIAL.

  SELECT a~kostl b~ktext INTO TABLE it_kostl
  FROM csks AS a INNER JOIN cskt AS b
  ON  b~kostl EQ a~kostl
  AND b~spras EQ sy-langu
  WHERE a~kokrs EQ c_kokrs
    AND a~datbi >= pn-begda
    AND a~datab <= pn-begda
    AND b~datbi EQ a~datbi.

  CALL FUNCTION 'BAPI_COSTCENTERGROUP_GETLIST'
    EXPORTING
      controllingareamask = 'H201'
      groupnamemask       = 'HMMA-*'
      topnodesonly        = ' '
    TABLES
      grouplist           = grouplist.


  CHECK sy-subrc EQ 0.
  SORT grouplist BY groupname.

  PERFORM change_grouplist TABLES grouplist.

  LOOP AT grouplist.

    PERFORM get_cc_group  TABLES it_group
                          USING
                          pn-begda
                          grouplist-groupname
                          grouplist-descript.
  ENDLOOP.

  PERFORM prd_group_exclude USING : 'HMMA-100',
                                    'HMMA-200',
                                    'HMMA-300',
                                    'HMMA-400'.
  LOOP AT it_group.
    IF it_group-kostl EQ '0000055104'.
      it_group-groupname = 'Z-UTL'.
      it_group-descript = 'Utilities'.
      MODIFY it_group INDEX sy-tabix.
    ENDIF.
    IF it_group-kostl EQ '0000055106'.
      it_group-groupname = 'Z-UTL'.
      it_group-descript = 'Engine Quality'.
      MODIFY it_group INDEX sy-tabix.
    ENDIF.
    IF it_group-groupname = 'HMMA-332'.
      it_group-groupname = 'HMMA-331'.
      it_group-descript = 'Engine'.
      MODIFY it_group INDEX sy-tabix.
    ENDIF.

* Begin of HIS20094 - Move cost center MXTX12 to Assembly
    IF it_group-kostl EQ 'MXTX12'.
      it_group-groupname = 'HMMA-324'.
      it_group-descript = 'Assembly'.
      MODIFY it_group INDEX sy-tabix.
    ENDIF.
* End of HIS20094

  ENDLOOP.

  SORT it_group BY  kostl ASCENDING
                    groupname DESCENDING.

  DELETE ADJACENT DUPLICATES FROM it_group COMPARING kostl.

  SORT it_group BY kostl.

ENDFORM.                    " get_kostl
*&---------------------------------------------------------------------*
*&      Form  get_cc_group
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_PRD  text
*      -->P_PN_BEGDA  text
*      -->P_GROUPLIST_GROUPNAME  text
*----------------------------------------------------------------------*
FORM get_cc_group TABLES cctable STRUCTURE it_group
                  USING check_date costcentergroup descript.

  DATA: BEGIN OF costcenter_list OCCURS 0.
          INCLUDE STRUCTURE bapi0012_2.
  DATA: END OF costcenter_list.

  CALL FUNCTION 'BAPI_COSTCENTER_GETLIST'
    EXPORTING
      controllingarea = 'H201'
      companycode     = 'H201'
      date            = check_date
      date_to         = '99991231'
      costcentergroup = costcentergroup
    TABLES
      costcenter_list = costcenter_list.

  LOOP AT costcenter_list.
    cctable-kostl     = costcenter_list-costcenter.
    cctable-groupname = costcentergroup.
    cctable-descript  = descript.
    APPEND cctable.
  ENDLOOP.

ENDFORM.                    " get_cc_group
*&---------------------------------------------------------------------*
*&      Form  change_grouplist
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GROUPLIST  text
*----------------------------------------------------------------------*
FORM change_grouplist TABLES p_grouplist STRUCTURE grouplist.

  DELETE p_grouplist WHERE
  groupname NE 'HMMA-100' AND
  groupname NE 'HMMA-110' AND
  groupname NE 'HMMA-120' AND
  groupname NE 'HMMA-130' AND
  groupname NE 'HMMA-140' AND
  groupname NE 'HMMA-150' AND
  groupname NE 'HMMA-160' AND
  groupname NE 'HMMA-170' AND
  groupname NE 'HMMA-180' AND
  groupname NE 'HMMA-190' AND
  groupname NE 'HMMA-200' AND
  groupname NE 'HMMA-210' AND
  groupname NE 'HMMA-220' AND
  groupname NE 'HMMA-300' AND
  groupname NE 'HMMA-310' AND
  groupname NE 'HMMA-321' AND
  groupname NE 'HMMA-322' AND
  groupname NE 'HMMA-323' AND
  groupname NE 'HMMA-324' AND
  groupname NE 'HMMA-330' AND
  groupname NE 'HMMA-331' AND
  groupname NE 'HMMA-332' AND
  groupname NE 'HMMA-340' AND
  groupname NE 'HMMA-350' AND
  groupname NE 'HMMA-370' AND
  groupname NE 'HMMA-380' AND
  groupname NE 'HMMA-400' AND
  groupname NE 'HMMA-410' AND
  groupname NE 'HMMA-420' AND
  groupname NE 'HMMA-430'.

  __change_desc :

  'HMMA-100'  'Administration' ,
  'HMMA-110'  'Human Resources',
  'HMMA-120'  'General Affairs',
  'HMMA-130'  'Safety/Security',
  'HMMA-140'  'Public Relations',
  'HMMA-150'  'Legal',
  'HMMA-160'  'IT',
  'HMMA-170'  'Education',
  'HMMA-180'  'Sales',
  'HMMA-190'  'Internal Audit',
  'HMMA-200'  'Finance',
  'HMMA-210'  'Finance',
  'HMMA-220'  'Finance',
  'HMMA-300'  'Production',
  'HMMA-310'  'Product Control',
  'HMMA-321'  'Stamping',
  'HMMA-322'  'Welding',
  'HMMA-323'  'Paint',
  'HMMA-324'  'Assembly',
  'HMMA-330'  'Engine Prod Support',
  'HMMA-331'  'Engine',
  'HMMA-332'  '2nd Engine',
  'HMMA-340'  'Production Eng',
  'HMMA-350'  'Quality',
  'HMMA-370'  'Plant Engineering',
  'HMMA-380'  'MES',
  'HMMA-400'  'Purchasing',
  'HMMA-410'  'Purchasing',
  'HMMA-420'  'Purchasing',
  'HMMA-430'  'Purchasing'.

ENDFORM.                    " change_grouplist
*&---------------------------------------------------------------------*
*&      Form  prd_group_exclude
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prd_group_exclude USING p_groupname.

  DATA $it_group LIKE it_group OCCURS 0 WITH HEADER LINE.

  SORT it_group BY kostl.

  __cls $it_group.

  $it_group[] = it_group[].

  DELETE $it_group WHERE groupname NE p_groupname.
  DELETE it_group WHERE groupname EQ p_groupname.

  DATA $ix TYPE i.
  LOOP AT $it_group.
    $ix = sy-tabix.
    READ TABLE it_group WITH KEY kostl = $it_group-kostl BINARY SEARCH.
    IF sy-subrc EQ 0 AND it_group-groupname NE $it_group-groupname.
      DELETE $it_group INDEX $ix.
    ENDIF.
  ENDLOOP.

  DELETE it_group WHERE groupname EQ p_groupname.

  APPEND LINES OF $it_group TO it_group.

ENDFORM.                    " prd_group_exclude
*&---------------------------------------------------------------------*
*&      Form  get_orgeh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_orgeh.

  IF it_orgeh[] IS INITIAL.

    SELECT objid stext INTO TABLE it_orgeh
    FROM hrp1000 WHERE plvar EQ '01'
                   AND otype EQ 'O'
                   AND endda EQ '99991231'
                   AND langu EQ sy-langu
                   AND uname NE 'SAP'.

    SORT it_orgeh BY orgeh.
  ENDIF.

  IF it_massg[] IS INITIAL.

    SELECT massn massg mgtxt INTO TABLE it_massg
    FROM t530t WHERE sprsl EQ sy-langu.

    SORT it_massg BY massn massg.
  ENDIF.

ENDFORM.                    " get_orgeh
