*----------------------------------------------------------------------
* Program ID        : ZAHRU003
* Title             : [HR] Report on Arrears
* Created on        : 2/10/2009
* Created by        : I.G.MOON
* Specifications By : JT.HU
* Description       : [HR] Report on Arrears
*----------------------------------------------------------------------
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  12/13/2011  Valerian  UD1K953879  Correct logic to get arrear
*                                    Add selection based on TM status
*                                    Add selection based on Pay. Area
*----------------------------------------------------------------------
REPORT zahru003 MESSAGE-ID zmco.
TABLES: pernr, pyorgscreen, pytimescreen.
TABLES: t512t,PA0000.

NODES: payroll TYPE pay99_result.
DATA: wa_arrrs LIKE LINE OF payroll-inter-arrrs.

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.

TYPES: BEGIN OF ty_row_tab,
       pernr TYPE p_pernr,
       sname(30),
       lgart  TYPE lgart,
       lgtxt  TYPE lgtxt,
       betrg TYPE betrg,
       pabrj TYPE pabrj,
       pabrp TYPE pabrp,
       waers TYPE waers,
END OF ty_row_tab.

TYPES BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES END OF ty_out.

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

data : begin of it_status occurs 0,
          pernr like pa0000-pernr,
          stat2 like pa0000-stat2,
          begda like pa0000-begda,
          endda like pa0000-endda,
          sname like pa0001-sname,
       end of it_status.

DATA: in_rgdir LIKE pc261 OCCURS 0 WITH HEADER LINE.

DATA : ws_rgdir LIKE LINE OF in_rgdir,
       l_relid LIKE  pcl2-relid,
       l_pernr LIKE pc200-pernr,
       l_code LIKE p0041-dar01,
       l_code1 LIKE p0041-dar01,
       t_date LIKE p0041-dat01,
       hire_date LIKE  p0041-dat01,
       l_edate  LIKE pa0169-begda,
       l_edt1  LIKE pa0169-begda,
       l_edate1(10) TYPE c,
       l_edate2(10) TYPE c,
       lv_molga TYPE molga,
       result TYPE pay99_result.

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

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME               "UD1K953879
                                  TITLE text-005.           "UD1K953879
SELECT-OPTIONS p_stat FOR pernr-stat2.                      "UD1K953879
PARAMETER p_vari TYPE slis_vari NO-DISPLAY.
SELECTION-SCREEN END   OF BLOCK b1.                         "UD1K953879
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title = '[HR] Report on Arrears'.

START-OF-SELECTION.

  __cls it_row_tab.

GET : pernr, payroll.

  CHECK : payroll-evp-fpper EQ payroll-evp-inper,
          payroll-evp-payty EQ space.
  CHECK : p_stat,                                           "UD1K953879
          payroll-evp-abkrs EQ pyxabkr.                     "UD1K953879

  LOOP AT payroll-inter-arrrs INTO wa_arrrs.
    CLEAR it_row_tab.
    MOVE-CORRESPONDING wa_arrrs TO it_row_tab.
    it_row_tab-pernr = pernr-pernr.
    SELECT SINGLE lgtxt INTO it_row_tab-lgtxt FROM t512t
    WHERE sprsl EQ sy-langu
      AND molga EQ payroll-inter-versc-molga
      AND lgart EQ wa_arrrs-lgart.
    it_row_tab-sname = pernr-sname.
    it_row_tab-waers = 'USD'.
    APPEND it_row_tab.
  ENDLOOP.

END-OF-SELECTION.

  __cls it_status.

  select a~pernr a~stat2 a~begda a~endda b~sname into table it_status
  from pa0000 as a inner join pa0001 as b
   on b~pernr eq a~pernr
  where a~pernr in PYPERNR
    and a~stat2 in p_stat                                   "UD1K953879
*   and a~stat2 ne '0'                                      "UD1K953879
    and a~endda eq '99991231'                               "UD1K953879
    and b~endda eq '99991231'
    and b~abkrs = pyxabkr.                                  "UD1K953879

  sort it_status by pernr ascending begda descending.
  DELETE ADJACENT DUPLICATES FROM it_status  COMPARING pernr.
  sort it_status by pernr.

  loop at it_row_tab.
    delete it_status where pernr = it_row_tab-pernr.
  endloop.

  sort it_status by pernr.

  SELECT SINGLE relid INTO l_relid
                FROM t500l
                WHERE molga = '10'.
  IF   l_relid IS INITIAL.
    l_relid = 'RU'.
  ENDIF.


  loop at it_status.

    __cls in_rgdir.

    CALL FUNCTION 'CU_READ_RGDIR'
         EXPORTING
              persnr          = it_status-pernr
         IMPORTING
              molga           = lv_molga
         TABLES
              in_rgdir        = in_rgdir
         EXCEPTIONS
              no_record_found = 1
              OTHERS          = 2.

    delete in_rgdir where INPER(4) <> PYBEGDA(4).
    sort in_rgdir by FPBEG descending.

    __cls result-inter-arrrs.                               "UD1K953879
    LOOP AT in_rgdir INTO ws_rgdir where ocrsn is initial.

      CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
           EXPORTING
                clusterid                    = l_relid
                employeenumber               = it_status-pernr
                sequencenumber               = ws_rgdir-seqnr
                read_only_international      = 'X'
           CHANGING
                payroll_result               = result
           EXCEPTIONS
                illegal_isocode_or_clusterid = 1
                error_generating_import      = 2
                import_mismatch_error        = 3
                subpool_dir_full             = 4
                no_read_authority            = 5
                no_record_found              = 6
                versions_do_not_match        = 7
                error_reading_archive        = 8
                error_reading_relid          = 9
                OTHERS                       = 10.
      exit.
    endloop.

    LOOP AT result-inter-arrrs INTO wa_arrrs.
      CLEAR it_row_tab.
      MOVE-CORRESPONDING wa_arrrs TO it_row_tab.
      it_row_tab-pernr = it_status-pernr.
      SELECT SINGLE lgtxt INTO it_row_tab-lgtxt FROM t512t
      WHERE sprsl EQ sy-langu
        AND molga EQ result-inter-versc-molga
        AND lgart EQ wa_arrrs-lgart.
      it_row_tab-sname = it_status-sname.
      it_row_tab-waers = 'USD'.
      APPEND it_row_tab.
    ENDLOOP.

  endloop.

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

  l_text = '[HR] Report on Arrears'.
  PERFORM set_header_line USING:
          'P' 'H' ''      l_text       ''.

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
    'X'  'PERNR'  'TM ID'        10  'CHAR' '' '' '',
    ' '  'SNAME'  'Name'         30  'CHAR' '' '' '',
    ' '  'LGART'  'W/T'           4  'CHAR' '' '' '',
    ' '  'LGTXT'  'W/T Text'     25  'CHAR' '' '' '',
    ' '  'PABRJ'  'Year'          4  'NUMC' '' '' '',
    ' '  'PABRP'  'Period'        2  'NUMC' '' '' '',
    ' '  'BETRG'  'Amount'       15  'CURR' '' 'WAERS' ''.

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
     'PERNR'        ' ' 'X' 'X' 'X',
     'SNAME'        ' ' 'X' 'X' 'X',
     'LGART'        ' ' 'X' 'X' 'X',
     'LGTXT'        ' ' 'X' 'X' 'X',
     'PABRJ'        ' ' 'X' 'X' 'X',
     'PABRP'        ' ' 'X' 'X' 'X'.

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

  __cls gt_out.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " move_out
*&---------------------------------------------------------------------*
*&      Form  read_rgdir
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PERNR_PERNR  text
*      <--P_I_RGDIR  text
*----------------------------------------------------------------------*
FORM read_rgdir TABLES fp_i_rgdir STRUCTURE pc261
                USING  fp_v_pernr fp_lv_molga.

  CLEAR : fp_i_rgdir[].

  CALL FUNCTION 'CU_READ_RGDIR'
       EXPORTING
            persnr          = fp_v_pernr
       IMPORTING
            molga           = fp_lv_molga
       TABLES
            in_rgdir        = fp_i_rgdir
       EXCEPTIONS
            no_record_found = 1
            OTHERS          = 2.

ENDFORM.                    " read_rgdir
