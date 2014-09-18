*----------------------------------------------------------------------
* Program ID        : ZAHRU017
* Title             : [ESS] Team Member Appreciation Day
* Created on        : 3/10/2011
* Created by        : I.G.MOON
* Specifications By : Grace Li
* Description       : [ESS] Team Member Appreciation Day
*----------------------------------------------------------------------
REPORT zahru017 MESSAGE-ID zmco.
* Tables
TABLES: pernr,t001p,t513s,t528t,t527x,sscrfields,zthrtmad,zthrdept,
        pa0000,pa0001,t501t.

INFOTYPES: 0000,0001,0002,
           0006 MODE n,
           0106 MODE n,
           0022 MODE n,
           0023 MODE n.

INCLUDE zhrr99990t.  "ALV Common Routine

INCLUDE <icon>.                        " icon

DATA:
  container TYPE REF TO cl_gui_custom_container,
  html_viewer TYPE REF TO cl_gui_html_viewer,
  ok_code LIKE sy-ucomm,g_import.

DATA pdpsp LIKE pdpsp OCCURS 0 WITH HEADER LINE.
DATA day_psp LIKE pdpsp OCCURS 0 WITH HEADER LINE.
DATA pdpnr LIKE pdpnr OCCURS 0 WITH HEADER LINE.
DATA izthrtmad LIKE zthrtmad OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
*-- Print option for preview (Smartforms)
DATA: st_print_option  TYPE ssfctrlop,
      st_control_param TYPE ssfctrlop,
      st_output_option TYPE ssfcompop,
      st_output_inform TYPE ssfcrescl,
      g_func_name      TYPE rs38l_fnam.
DATA  g_cnt TYPE i.

DATA: BEGIN OF it001p OCCURS 0,
          werks TYPE persa,
          btrtl TYPE btrtl_001p,
          btext TYPE btrtx,
      END OF it001p.

DATA: BEGIN OF it501t OCCURS 0,
          persg TYPE persg,
          ptext TYPE pgtxt,
      END OF it501t.

DATA: BEGIN OF izthrdept OCCURS 0,
          kostl TYPE kostl,
          division(50),
          department(50),
      END OF izthrdept.

DATA: BEGIN OF it_kostl OCCURS 0,
        kostl LIKE cskt-kostl,
        ktext LIKE cskt-ktext,
      END OF it_kostl.

TYPES: BEGIN OF ty_row_tab.
        INCLUDE STRUCTURE zthrtmadmaster.
TYPES: END OF ty_row_tab.

TYPES BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES celltab  TYPE lvc_t_styl.
TYPES tabcolor TYPE slis_t_specialcol_alv.
TYPES END OF ty_out.

DATA ibenefit LIKE zess_emp_benefit_data_health OCCURS 0
        WITH HEADER LINE.
DATA ibenefit_dep LIKE zess_emp_benefit_data_dep OCCURS 0
        WITH HEADER LINE.
DATA iperson LIKE zess_personal OCCURS 0 WITH HEADER LINE.

DATA ireturn LIKE bapireturn OCCURS 0 WITH HEADER LINE.

DATA w_style TYPE lvc_s_styl.

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.
DATA  $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.
DATA   w_gt_out    TYPE ty_out.

DATA  : it_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE.

DATA $ix TYPE i.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
DEFINE __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
END-OF-DEFINITION.

DEFINE __u_break.
  if err_brk eq true.
    break-point.
  endif.
END-OF-DEFINITION.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK view-result WITH FRAME TITLE text-t03.
PARAMETERS p_upd AS CHECKBOX."EFAULT 'X'.
SELECTION-SCREEN END OF BLOCK view-result.

************************************************************************
DATA  : flag_data_changed,
        info(80).
DATA: BEGIN OF ftab OCCURS 10,
        fcode(6),
      END OF ftab.

TYPE-POOLS: truxs.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

* internal table for uploaded file
DATA : BEGIN OF gt_tab  OCCURS 0,
         f1(20),
         f2(20),
         f3(20),
         f4(20),
       END   OF  gt_tab.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  PERFORM default_variant.

AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.
*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  __cls it_row_tab.

  __process 'Gather TeamMember...' '10'.

GET pernr.

  PROVIDE * FROM p0000 BETWEEN pn-begda AND pn-endda.
    IF ( p0000-stat2 EQ '1' OR p0000-stat2 EQ '3' )
        AND p0000-endda EQ '99991231'.
      it_row_tab-pernr = p0000-pernr.
      APPEND it_row_tab.
      CLEAR it_row_tab.
    ENDIF.
  ENDPROVIDE.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM data_process.

*---------------------------------------------------------------------*
*       FORM alv_data_changed                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RR_DATA_CHANGED                                               *
*---------------------------------------------------------------------*
FORM alv_data_changed USING rr_data_changed TYPE REF TO
                            cl_alv_changed_data_protocol.

  DATA : ls_mod_cells TYPE lvc_s_modi.

  LOOP AT rr_data_changed->mt_good_cells INTO ls_mod_cells.
    IF ls_mod_cells-fieldname = 'DELIVERED'.
      PERFORM chk_input USING ls_mod_cells-fieldname
                              rr_data_changed ls_mod_cells.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " ALV_DATA_CHANGED

*&---------------------------------------------------------------------
*         Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------
FORM alv_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.
  DATA: rt_extab TYPE slis_t_extab.

* Event 'ALV_DATA_CHANGED' is happened..
  DATA : l_valid TYPE char01.
  PERFORM alv_exec_data_changed CHANGING l_valid.

* If error is not happened...
*  CHECK NOT l_valid IS INITIAL.

  CASE r_ucomm.
*    WHEN '&IC1'.           "Double-Click
    WHEN 'RBACK' OR 'RCANC'.
      CLEAR : r_ucomm.
      SET SCREEN 0.

* All Selection
    WHEN 'SALL'.
      gt_out-delivered = 'X'.
      MODIFY gt_out TRANSPORTING delivered
                           WHERE delivered IS initial.

* All Deselection
    WHEN 'SSAL'.
      gt_out-delivered = ''.
      MODIFY gt_out TRANSPORTING delivered
                              WHERE NOT delivered IS initial.

    WHEN 'ZSAV'.
      PERFORM save.
    WHEN 'REXIT'.
      LEAVE  PROGRAM.
      CLEAR : r_ucomm.
  ENDCASE.

* Screen Refresh
  rs_selfield-refresh    = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-row_stable = 'X'.

ENDFORM.                    " SEL_OP

*---------------------------------------------------------------------*
*       FORM alv_pf_status_set                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM alv_pf_status_set USING rt_extab TYPE slis_t_extab.
  DATA: st_extab TYPE slis_extab.                           "0363012

* Button Exception
*  MOVE '&ALL' TO rt_extab_wa-fcode.                         "0363012
*  APPEND rt_extab_wa TO rt_extab.                           "0363012
*  MOVE '&SAL' TO rt_extab_wa-fcode.                         "0363012
*  APPEND rt_extab_wa TO rt_extab.                           "0363012

  SET PF-STATUS 'STANDARD'." EXCLUDING st_extab.

ENDFORM.                    " ALV_PF_STATUS_SET

*----------------------------------------------------------------------*
* Sub-Rutines
*----------------------------------------------------------------------*

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
*&      Form  MOVE_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.

  __process 'Preparing output...' '95'.

  __cls gt_out.
  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " MOVE_OUT
*&---------------------------------------------------------------------*
*&      Form  make_gt_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_gt_out.

  DATA $cnt TYPE i.
  __cls : it001p ,izthrdept,it501t,it_kostl.

  SELECT kostl ktext INTO TABLE it_kostl FROM cskt
         WHERE spras EQ sy-langu
         AND datbi EQ '99991231'.
  SORT it_kostl BY kostl.

  SELECT persg ptext
  INTO TABLE it501t
  FROM t501t WHERE sprsl = sy-langu.
  SORT it501t BY persg.

  SELECT werks btrtl btext
  INTO TABLE it001p
  FROM t001p.
  SORT it001p BY werks btrtl.

  SELECT  kostl division department
  INTO TABLE izthrdept
  FROM zthrdept.
  SORT izthrdept BY kostl.

  DATA $$ix(10).
  DATA $tot TYPE i.
  DATA $$tot(10).
  DATA $tmp_text(30).

  DESCRIBE TABLE it_row_tab LINES $tot.

  LOOP AT it_row_tab.

    $ix = sy-tabix.
    $$ix = $ix.
    $$tot = $tot.

    CONCATENATE $$ix '/' $$tot INTO $tmp_text.
    CONDENSE  $tmp_text NO-GAPS.

    __process  $tmp_text '0'.

    SELECT SINGLE nachn vorna INTO (it_row_tab-nachn,it_row_tab-vorna)
                          FROM pa0002 WHERE pernr = it_row_tab-pernr
                           AND endda = '99991231'.

    SELECT SINGLE *  FROM pa0001 WHERE pernr = it_row_tab-pernr
                           AND endda = '99991231'.

    IF sy-subrc EQ 0.
      it_row_tab-persg = pa0001-persg.
      READ TABLE it501t WITH KEY persg = pa0001-persg
                                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-ptext = it501t-ptext.
      ENDIF.

      READ TABLE it001p WITH KEY werks = pa0001-werks
                                 btrtl = pa0001-btrtl
                                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-btext = it001p-btext.
      ENDIF.

      READ TABLE izthrdept WITH KEY kostl = pa0001-kostl
                                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-sdept = izthrdept-department.
*        it_row_tab-ktext = izthrdept-kostlname.
      ELSE.
        SELECT SINGLE ktext INTO it_row_tab-ktext FROM cskt
          WHERE kostl = pa0001-kostl
          AND spras = sy-langu
          AND kokrs = 'K201'
          AND datbi = '9991231'.
      ENDIF.

      it_row_tab-kostl = pa0001-kostl.

      READ TABLE it_kostl WITH KEY   kostl = it_row_tab-kostl
                                   BINARY SEARCH.
      it_row_tab-ktext = it_kostl-ktext.
    ENDIF.

    SELECT SINGLE *  FROM pa0000 WHERE pernr = it_row_tab-pernr
                           AND endda = '99991231'.
    IF sy-subrc EQ 0.

      IF pa0000-stat2 = '3'.
        it_row_tab-statt = 'Active'.
      ELSEIF pa0000-stat2 = '1'.
        it_row_tab-statt = 'Inactive'.
      ENDIF.
    ENDIF.

    $cnt = 0.

    CALL FUNCTION 'Z_HR_ESS_GET_EMP_BEN_HEALTH'
         EXPORTING
              employee_number           = it_row_tab-pernr
         TABLES
              zess_emp_benefit_data     = ibenefit
              zess_emp_benefit_data_dep = ibenefit_dep
              return                    = ireturn.

    IF sy-subrc EQ 0.
      DELETE ibenefit_dep WHERE bplan NE 'BLAL'.
      DESCRIBE TABLE ibenefit_dep LINES $cnt.
    ENDIF.

    IF $cnt <= 1.
      $cnt = 2.
    ELSE.
      ADD 1 TO $cnt.
    ENDIF.

    it_row_tab-elitk = $cnt.
    CLEAR it_row_tab-rdate.

    SELECT SINGLE ticket aedat aenam
    INTO (it_row_tab-chotk,it_row_tab-rdate,it_row_tab-aenam)
    FROM zthrtmad WHERE pernr = it_row_tab-pernr.
    IF it_row_tab-aenam EQ 'RFCESS'.
      it_row_tab-ress = true.
    ENDIF.

    SELECT SINGLE delivered
    INTO it_row_tab-delivered
    FROM zthrtmadmaster WHERE pernr = it_row_tab-pernr.

    MODIFY it_row_tab INDEX $ix.
    CLEAR it_row_tab.

  ENDLOOP.

ENDFORM.                    " make_gt_out

*---------------------------------------------------------------------*
*       FORM default_variant                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
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
*&      Form  fieidcat_gathering
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieidcat_gathering.

  gs_layout-zebra      = 'X'.
  gs_layout-cwidth_opt = 'X'.
  gs_layout-stylefname = 'CELLTAB'.
  gs_layout-ctab_fname = 'TABCOLOR'.

  gs_layout-no_rowmark = 'X'.

  DATA: l_pos TYPE i.

  __cls gt_fieldcat_lvc.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat_lvc.
    gs_fieldcat_lvc-col_pos       = l_pos.
    gs_fieldcat_lvc-key           = &1.
    gs_fieldcat_lvc-fieldname     = &2.
    gs_fieldcat_lvc-seltext       = &3.        " Column heading
    gs_fieldcat_lvc-reptext       = &3.
    gs_fieldcat_lvc-outputlen     = &4.        " Column width
    gs_fieldcat_lvc-datatype      = &5.        " Data type
    gs_fieldcat_lvc-emphasize     = &6.
    append gs_fieldcat_lvc to  gt_fieldcat_lvc.
  END-OF-DEFINITION.

  __catalog :
    ' ' 'PERNR' 'Pers.No'                 8   'NUMC' '',
    ' ' 'NACHN' 'Last Name'              30   'CHAR' '',
    ' ' 'VORNA' 'First Name'             30   'CHAR' '',
    ' ' 'BTEXT' 'Pers.Subarea'           30   'CHAR' '',
    ' ' 'PTEXT' 'EE Group'               20   'CHAR' '',
    ' ' 'STATT' 'Empl.Status'            30   'CHAR' '',
    ' ' 'SDEPT' 'Department'              40   'CHAR' '',
    ' ' 'KOSTL' 'CstCntr'                10   'CHAR' '',
    ' ' 'KTEXT' 'CstCntr Text'           20   'CHAR' '',
    ' ' 'ELITK' 'Eligible'               10   'DEC' '',
    ' ' 'CHOTK' 'Chosen'                 10   'DEC' '',
    ' ' 'RESS' 'MyESS'                    1   'CHAR' '',
    ' ' 'DELIVERED' 'Delivered'     1   'CHAR' '',
    ' ' 'REMARKS' 'TM Signature'         50   'CHAR' ''.

  LOOP AT gt_fieldcat_lvc INTO gs_fieldcat_lvc.
*    if gs_fieldcat_lvc-fieldname ne 'DELIVERED'.
    gs_fieldcat_lvc-tabname = 'ZTHRTMADMASTER'.
    gs_fieldcat_lvc-ref_table = 'ZTHRTMADMASTER'.
    gs_fieldcat_lvc-ref_field = gs_fieldcat_lvc-fieldname.
    MODIFY gt_fieldcat_lvc FROM gs_fieldcat_lvc.
*    endif.
  ENDLOOP.

  LOOP AT gt_fieldcat_lvc INTO gs_fieldcat_lvc.
    CASE gs_fieldcat_lvc-fieldname.
      WHEN 'DELIVERED'.
        gs_fieldcat_lvc-edit     = 'X'.
        gs_fieldcat_lvc-checkbox = 'X'.
        gs_fieldcat_lvc-outputlen = 3.
        gs_fieldcat_lvc-emphasize = 'C200'.
      WHEN 'REMARKS'.
        gs_fieldcat_lvc-outputlen = 30.
    ENDCASE.
    MODIFY gt_fieldcat_lvc FROM gs_fieldcat_lvc.
  ENDLOOP.


  PERFORM set_color.

ENDFORM.
" fieidcat_gathering

*---------------------------------------------------------------------*
*       FORM alv_top_of_list                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM alv_top_of_list.

  DATA l_text(60).
  REFRESH gt_header.

  l_text = 'Team Member Appreciation Day'.
  PERFORM set_header_line USING:
          'P' 'H' ''      l_text       ''.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_header.

ENDFORM.                    " alv_top_of_list

*&---------------------------------------------------------------------*
*&      Form  call_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT  text
*      -->P_0478   text
*      -->P_0479   text
*----------------------------------------------------------------------*
FORM call_function TABLES p_gt_out
                    USING p_user_command  p_alv_pf_status_set.
  sy-lsind = sy-lsind - 1.

*   Set variant
  g_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'  "'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     i_interface_check                 = ' '
*     i_buffer_active                   = 'X'
     i_callback_program                = g_repid
     i_callback_pf_status_set          = p_alv_pf_status_set
     i_callback_user_command           = p_user_command
     i_callback_top_of_page            = 'ALV_TOP_OF_LIST'
     i_grid_settings                   = gt_gridset
     is_layout_lvc                     = gs_layout
     it_fieldcat_lvc                   = gt_fieldcat_lvc
     it_sort_lvc                       = gt_sort_lvc
*     it_filter                         = gt_filter
     it_events                         = gt_events_lvc[]
     i_save                            = 'A'
     is_variant                        = gs_variant
   TABLES
     t_outtab                          = p_gt_out
   EXCEPTIONS
     program_error                     = 1
     OTHERS                            = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " call_function
*&---------------------------------------------------------------------*
*&      Form  set_header_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1998   text
*      -->P_1999   text
*      -->P_2000   text
*      -->P_L_TEXT  text
*      -->P_2002   text
*----------------------------------------------------------------------*
FORM set_header_line  USING  fp_data
                             fp_type
                             fp_key
                             fp_low
                             fp_high.
  DATA  : ls_line  TYPE slis_listheader,
          l_ldate(10),
          l_hdate(10).

  CHECK NOT fp_low IS INITIAL.
  CLEAR : ls_line, l_ldate, l_hdate.

  MOVE  : fp_type    TO ls_line-typ,
          fp_key     TO ls_line-key.

  READ TABLE gt_header TRANSPORTING NO FIELDS
                               WITH KEY typ = ls_line-typ
                                        key = ls_line-key.
  IF sy-subrc NE 0.
    CASE fp_data.
      WHEN 'P'.    "Parameters'
        CONCATENATE fp_low fp_high        INTO ls_line-info
                                          SEPARATED BY space.
      WHEN 'S'.    "Select-options
        IF fp_high IS INITIAL.
          ls_line-info = fp_low.
        ELSE.
          CONCATENATE fp_low '~' fp_high    INTO ls_line-info
                                            SEPARATED BY space.
        ENDIF.
      WHEN 'D'.    "Date
        WRITE : fp_low  TO l_ldate,
                fp_high TO l_hdate.
        IF fp_high IS INITIAL.
          ls_line-info = l_ldate.
        ELSE.
          CONCATENATE l_ldate '~' l_hdate INTO ls_line-info
                                          SEPARATED BY space.
        ENDIF.
    ENDCASE.
    APPEND ls_line TO gt_header.
  ENDIF.
ENDFORM.                    " SET_HEADER_LINE
*&---------------------------------------------------------------------*
*&      Form  set_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_color.

  DATA : $ix(2) TYPE n,
         $mtxt(6).

  CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  DEFINE __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  END-OF-DEFINITION.

  __color :
          'ZMARK'      '1' 0,
          'PERNR'      '1' 0.

  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS initial.

ENDFORM.                    " set_color
*&---------------------------------------------------------------------*
*&      Form  remove_dash
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IZSHRTMPROFILE_H_TELWK  text
*----------------------------------------------------------------------*
FORM remove_dash CHANGING p_num.

  DO 5 TIMES.
    REPLACE '-' WITH '' INTO p_num.
  ENDDO.

ENDFORM.                    " remove_dash
*&---------------------------------------------------------------------*
*&      Form  reduce_line_history
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reduce_line_history TABLES itab STRUCTURE zshrtmprofile_i3.

  DATA: BEGIN OF $izshrtmprofile_i3 OCCURS 0.
  DATA  key(65).
          INCLUDE STRUCTURE zshrtmprofile_i3.
  DATA  ix.
  DATA: END OF $izshrtmprofile_i3.

  DATA $flag.

  LOOP AT itab.
    MOVE-CORRESPONDING itab TO $izshrtmprofile_i3.
    APPEND $izshrtmprofile_i3.
  ENDLOOP.

  LOOP AT $izshrtmprofile_i3.
    CONCATENATE $izshrtmprofile_i3-divis $izshrtmprofile_i3-stltx
    $izshrtmprofile_i3-jobre INTO $izshrtmprofile_i3-key.
    MODIFY $izshrtmprofile_i3 INDEX sy-tabix TRANSPORTING key.
  ENDLOOP.

  SORT $izshrtmprofile_i3 BY key begda.

  LOOP AT $izshrtmprofile_i3.
    AT NEW key.
      $flag = true.
    ENDAT.

    IF $flag EQ true.
      CLEAR $flag.
      MODIFY $izshrtmprofile_i3 TRANSPORTING begda
      WHERE key = $izshrtmprofile_i3-key.
    ENDIF.


  ENDLOOP.

  SORT $izshrtmprofile_i3 BY key endda  DESCENDING.

  LOOP AT $izshrtmprofile_i3.

    AT NEW key.
      $flag = true.
    ENDAT.

    IF $flag EQ true.
      CLEAR $flag.
      MODIFY $izshrtmprofile_i3 TRANSPORTING endda
      WHERE key = $izshrtmprofile_i3-key.
    ENDIF.

  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM $izshrtmprofile_i3
  COMPARING key.
  __cls itab.
  LOOP AT $izshrtmprofile_i3 .
    MOVE-CORRESPONDING $izshrtmprofile_i3 TO itab.
    APPEND itab.
  ENDLOOP.

ENDFORM.                    " reduce_line_history
*&---------------------------------------------------------------------*
*&      Form  data_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_process.

  IF it_row_tab[] IS INITIAL.
    EXIT.
  ENDIF.

  IF p_upd EQ true.
    PERFORM make_gt_out.
  ELSE.
    __cls it_tab.
    it_tab[] = it_row_tab[].
    __cls it_row_tab.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_row_tab
     FROM  zthrtmadmaster
      FOR ALL ENTRIES IN it_tab
      WHERE pernr = it_tab-pernr.

    LOOP AT it_row_tab.
      $ix = sy-tabix.
      SELECT SINGLE ticket aedat aenam
      INTO (it_row_tab-chotk,it_row_tab-rdate,it_row_tab-aenam)
      FROM zthrtmad WHERE pernr = it_row_tab-pernr.
      IF it_row_tab-aenam EQ 'RFCESS'.
        it_row_tab-ress = true.
      ENDIF.

      SELECT SINGLE delivered
      INTO it_row_tab-delivered
      FROM zthrtmadmaster WHERE pernr = it_row_tab-pernr.

      MODIFY it_row_tab INDEX $ix.
      CLEAR it_row_tab.
    ENDLOOP.
  ENDIF.

  PERFORM move_out.

* Defined Event
  PERFORM build_eventcat USING '1'.
  DELETE gt_events_lvc WHERE name = g_top_of_page.

  DESCRIBE TABLE gt_out LINES g_cnt.
  PERFORM fieidcat_gathering.
  PERFORM call_function TABLES gt_out
                        USING 'ALV_USER_COMMAND' 'ALV_PF_STATUS_SET'.
ENDFORM.                    " data_process
*&---------------------------------------------------------------------*
*&      Form  import_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM import_data.

ENDFORM.                    " import_data
*&---------------------------------------------------------------------*
*&      Form  alv_variant_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_VARI  text
*----------------------------------------------------------------------*
FORM alv_variant_f4 CHANGING p_vari.
  DATA: rs_variant LIKE disvariant,
        lv_nof4 TYPE c.

  CLEAR lv_nof4.
  LOOP AT SCREEN.
    IF screen-name = 'PA_VARI'.
      IF screen-input = 0.
        lv_nof4 = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR rs_variant.
  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            is_variant = rs_variant
            i_save     = 'A'
       IMPORTING
            es_variant = rs_variant
       EXCEPTIONS
            OTHERS     = 1.

  IF sy-subrc = 0 AND lv_nof4 = space.
    p_vari = rs_variant-variant.
  ENDIF.

ENDFORM.                    " ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*&      Form  save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save.

  DATA it_zthrtmadmaster LIKE zthrtmadmaster OCCURS 0 WITH HEADER LINE.
  clear: it_zthrtmadmaster, it_zthrtmadmaster[].

  __cls izthrtmad.

  LOOP AT gt_out.
    SELECT SINGLE * FROM zthrtmad WHERE pernr = gt_out-pernr.
    IF sy-subrc EQ 0.
    ELSE.
      izthrtmad-pernr = gt_out-pernr.
      izthrtmad-ticket = 0.
      izthrtmad-aenam = sy-uname.
      izthrtmad-aedat = sy-datum.
      izthrtmad-aezet = sy-uzeit.
      APPEND izthrtmad.
    ENDIF.
  ENDLOOP.

  MODIFY zthrtmad FROM TABLE izthrtmad.
  COMMIT WORK.

  IF p_upd EQ true.
*    DELETE FROM zthrtmadmaster CLIENT SPECIFIED WHERE mandt = sy-mandt.
  ENDIF.

*// === 2011.08.04 insert by Kim_yn.     for ECC6 upgrade === //*
*// === ERROR modification ===
***  MODIFY zthrtmadmaster FROM TABLE gt_out.
***  COMMIT WORK.
  loop at gt_out.
    move-corresponding gt_out to it_zthrtmadmaster.
    append it_zthrtmadmaster.
    clear: it_zthrtmadmaster.
  endloop.

  MODIFY zthrtmadmaster FROM TABLE it_zthrtmadmaster.
  COMMIT WORK.
  clear: it_zthrtmadmaster, it_zthrtmadmaster[].
*// ========================= END =========================== //*
ENDFORM.                    " save
*&---------------------------------------------------------------------*
*&      Form  chk_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_MOD_CELLS_FIELDNAME  text
*      -->P_RR_DATA_CHANGED  text
*      -->P_LS_MOD_CELLS  text
*----------------------------------------------------------------------*
FORM chk_input  USING   p_field   rr_data_changed TYPE REF TO
                                  cl_alv_changed_data_protocol
                                  rs_mod_cells TYPE lvc_s_modi.

  READ TABLE gt_out INDEX rs_mod_cells-row_id.

  CHECK sy-subrc = 0.

  CASE p_field.
    WHEN 'DELIVERD'.
      gt_out-delivered = rs_mod_cells-value.
  ENDCASE.

  MODIFY gt_out INDEX rs_mod_cells-row_id.

ENDFORM.                    " CHK_INPUT
