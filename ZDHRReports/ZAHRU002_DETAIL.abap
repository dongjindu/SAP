*----------------------------------------------------------------------
* Program ID        : ZAHRU002_DETAIL
* Title             : [HR] User Authorization Data Extraction Tool.
* Created on        : 2/10/2009
* Created by        : I.G.MOON
* Specifications By : JT.HU
* Description       : [HR] User Authorization Data Extraction Tool
*----------------------------------------------------------------------
REPORT zahru002_detail MESSAGE-ID zmco.

TABLES: usr01, usr02, str_agrs, tstc,
        agr_users.

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.

INCLUDE <icon>.                        " icon

TYPES: BEGIN OF ty_row_tab,
       agr_name TYPE	agr_name,
       tcode	TYPE pexreport,
       uname  TYPE uname,
       sname(50),
       from_dat  TYPE agr_fdate,
       to_dat  TYPE agr_fdate,
       ttext TYPE ttext_stct,
       flag,
END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES: END OF ty_out.

DATA: BEGIN OF it_user OCCURS 0,
        bname      LIKE usr21-bname,
        persnumber LIKE adcp-persnumber,
        addrnumber LIKE adcp-addrnumber,
        name_last  TYPE ad_namelas,
        name_first TYPE ad_namefir,
      END OF it_user .

DATA: BEGIN OF it_bname OCCURS 0,
        bname      LIKE usr21-bname,
      END OF it_bname .


DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

DATA : BEGIN OF iusr02 OCCURS 0,
        bname TYPE xubname,
        uflag TYPE xuuflag,
        END OF iusr02.

DATA: BEGIN OF it_roll OCCURS 0,
       agr_name TYPE	agr_name,
       uname  TYPE uname,
END OF it_roll.

DATA $it_row_tab LIKE it_row_tab OCCURS 0 WITH HEADER LINE.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE u_break.
  if p_debug eq true.
    break-point.
  endif.
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DATA: g_error(1),
      g_repid  LIKE sy-repid.
RANGES s_date FOR sy-datum.

SELECTION-SCREEN BEGIN OF BLOCK act WITH FRAME TITLE text-100
                             NO INTERVALS.
SELECT-OPTIONS actgrps  FOR str_agrs-agr_name MEMORY ID xur OBLIGATORY.
SELECTION-SCREEN END OF BLOCK act.

SELECTION-SCREEN BEGIN OF BLOCK opt WITH FRAME TITLE text-200
                             NO INTERVALS.
SELECT-OPTIONS s_bname  FOR usr02-bname.
SELECT-OPTIONS s_tcode  FOR tstc-tcode.
PARAMETERS unlock AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK opt.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010
                             NO INTERVALS.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title = '[HR] User Authorization Data Extraction Tool (Detail)'.

  __cls actgrps.

  actgrps-sign = 'I'.
  actgrps-option = 'CP'.
  actgrps-low = 'Z:HR*'.
  APPEND actgrps.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

START-OF-SELECTION.

  PERFORM show_progress     USING 'Initializing...' '5'.
  PERFORM get_row_data.
  CHECK g_error NE true.

  PERFORM move_out.

END-OF-SELECTION.

  CHECK g_error NE true.
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

  l_text = 'User Authorization Data Extraction (detail)'.
  PERFORM set_header_line USING:
          'P' 'H' ''      l_text       '',
          'S' 'S' 'Role:'   actgrps-low  actgrps-high.

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
    'X'  'AGR_NAME'  'Role'          30  'CHAR' '' '' '',
    ' '  'TCODE'     'Transaction'   48  'CHAR' '' '' '',
    ' '  'TTEXT'     'Text'          60  'CHAR' '' '' '',
    ' '  'UNAME'     'User ID'       12  'CHAR' '' '' '',
    ' '  'SNAME'     'User Name'     50  'CHAR' '' '' '',
    ' '  'FROM_DAT'  'Start Date'     8  'DATS' '' '' '',
    ' '  'TO_DAT'    'End Date'       8  'DATS' '' '' ''.

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
     'AGR_NAME'     ' ' 'X' 'X' 'X',
     'TCODE'        ' ' 'X' 'X' 'X',
     'TTEXT'        ' ' 'X' 'X' 'X',
     'UNAME'        ' ' 'X' 'X' 'X',
     'SNAME'        ' ' 'X' 'X' 'X'.


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
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data.
  CLEAR g_error.

  DATA iagr_users LIKE agr_users OCCURS 0 WITH HEADER LINE.
  DATA iagr_tcodes LIKE agr_tcodes OCCURS 0 WITH HEADER LINE.
  DATA itstct LIKE tstct OCCURS 0 WITH HEADER LINE.

  DATA $fx TYPE i.

  __cls : iagr_users, iagr_tcodes.

  SELECT * INTO TABLE iagr_tcodes
  FROM agr_tcodes
  WHERE agr_name IN actgrps
    AND tcode IN s_tcode.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No Role data was found.'.
    g_error = true.
    EXIT.
  ENDIF.

  SELECT * INTO TABLE iagr_users
  FROM agr_users
  FOR ALL ENTRIES IN iagr_tcodes
  WHERE agr_name EQ iagr_tcodes-agr_name
    AND uname IN s_bname.

*  IF sy-subrc NE 0.
*    MESSAGE s000 WITH 'No Role data was found.'.
*    g_error = true.
*    EXIT.
*  ENDIF.


  __cls itstct.

  SELECT * INTO TABLE itstct
  FROM tstct
  FOR ALL ENTRIES IN iagr_tcodes
  WHERE sprsl EQ sy-langu
    AND tcode EQ iagr_tcodes-tcode(20).

  SORT itstct BY tcode.

  __cls : it_user, it_bname.

  LOOP AT  iagr_users.
    it_bname-bname = iagr_users-uname.
    COLLECT it_bname.
  ENDLOOP.
  SORT it_bname.

  SELECT a~bname
         a~persnumber
         a~addrnumber
         b~name_last b~name_first
         INTO TABLE it_user
         FROM usr21 AS a
         INNER JOIN v_addr_usr AS b
         ON  b~addrnumber EQ a~addrnumber
         AND b~persnumber EQ a~persnumber
         FOR ALL ENTRIES IN it_bname
         WHERE a~bname EQ it_bname-bname .

  SORT it_user BY bname.
  __cls it_row_tab.

  SORT iagr_users BY agr_name.
  LOOP AT iagr_tcodes.
    CLEAR it_row_tab.
    it_row_tab-agr_name = iagr_tcodes-agr_name.
    it_row_tab-tcode = iagr_tcodes-tcode.
    READ TABLE itstct WITH KEY tcode = iagr_tcodes-tcode
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-ttext = itstct-ttext.
    ENDIF.

    READ TABLE  iagr_users WITH KEY agr_name = iagr_tcodes-agr_name
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      $fx = sy-tabix.
      LOOP AT iagr_users FROM $fx.
        IF iagr_users-agr_name NE iagr_tcodes-agr_name.
          EXIT.
        ENDIF.
        it_row_tab-uname = iagr_users-uname.
        it_row_tab-from_dat = iagr_users-from_dat.
        it_row_tab-to_dat = iagr_users-to_dat.
        APPEND it_row_tab.
      ENDLOOP.
    ELSE.
      APPEND it_row_tab.
    ENDIF.

  ENDLOOP.
  DATA $ix TYPE i.
  LOOP AT  it_row_tab.
    $ix = sy-tabix.
    READ TABLE it_user WITH KEY bname = it_row_tab-uname BINARY SEARCH.
    IF sy-subrc EQ 0.
      CONCATENATE it_user-name_last ',' it_user-name_first
      INTO it_row_tab-sname.
    ENDIF.
    MODIFY it_row_tab INDEX $ix TRANSPORTING sname.
  ENDLOOP.

  IF unlock EQ true.
    __cls iusr02.
    SELECT bname uflag INTO TABLE iusr02
      FROM usr02
      FOR ALL ENTRIES IN it_bname
      WHERE bname EQ it_bname-bname .
    SORT iusr02 BY bname.

    LOOP AT  it_row_tab.
      CHECK NOT it_row_tab-uname IS INITIAL.
      $ix = sy-tabix.
      READ TABLE iusr02 WITH KEY bname = it_row_tab-uname BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF iusr02-uflag EQ '64'.
          DELETE  it_row_tab INDEX $ix.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

*  SORT it_row_tab BY agr_name tcode uname.
*
*  __cls it_roll.
*
*  LOOP AT it_row_tab.
*    it_roll-agr_name = it_row_tab-agr_name.
*    it_roll-uname = it_row_tab-uname.
*    COLLECT it_roll.
*  ENDLOOP.
*  SORT it_roll BY agr_name uname.
*
*  __cls $it_row_tab.
*
*  LOOP AT it_row_tab.
*    CHECK NOT it_row_tab-uname IS INITIAL.
*    $ix = sy-tabix.
*    READ TABLE it_roll WITH KEY agr_name = it_row_tab-agr_name
*                                uname = it_row_tab-uname
*                                BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      DELETE it_roll INDEX sy-tabix.
*    ELSE.
*      $it_row_tab = it_row_tab.
*      CLEAR : $it_row_tab-uname,
*              $it_row_tab-sname,
*       $it_row_tab-from_dat,
*       $it_row_tab-to_dat.
*      $it_row_tab-sname = '(Same as above)'.
*      COLLECT $it_row_tab.
*      DELETE it_row_tab INDEX $ix.
*    ENDIF.
*  ENDLOOP.
*
*  LOOP AT $it_row_tab.
*    $ix = sy-tabix.
*    READ TABLE it_row_tab WITH KEY agr_name = $it_row_tab-agr_name
*                                   tcode = $it_row_tab-tcode
*                                   BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      DELETE $it_row_tab INDEX $ix.
*    ENDIF.
*  ENDLOOP.
*
*  APPEND LINES OF $it_row_tab TO it_row_tab.

ENDFORM.                    " get_row_data
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
    CHECK it_row_tab-uname IN s_bname.
    CHECK it_row_tab-tcode IN s_tcode.
*    IF it_row_tab-uname IS INITIAL
*     AND it_row_tab-sname NE '(Same as above)'.
*      it_row_tab-uname = '*****'.
*      it_row_tab-sname = '***************'.
*    ENDIF.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " move_out
