*----------------------------------------------------------------------
* Program ID        : ZAHRU01C
* Title             : [HR] TeamMember Security Report
* Created on        : 04/01/2010
* Created by        : Valerian Utama
* Specifications By : Grace Li
* Description       : Display data from security table ZTHR_BHISTHMMA
*                     based on employee selection provided by program
*                     ZAHRU013.
*
*----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Issue No    Description
* 04/01/2010 Valerian               Initial program development. Copy
*            HIS20094               from prog. ZAHRU013 and modify
*                                   based on requirement
* 09/04/2012 Valerian   UD1K955629  Include all badge holders in the
*                                   security report
* 06/19/2013  T00303    UD1K957447  U1: Apply Archiving
*----------------------------------------------------------------------

REPORT zahru01c MESSAGE-ID zmco.
* Tables
TABLES: pernr,t001p,t513s,t528t,t527x.
TABLES: zshrtmprofile_h ,zshrtmprofile_i1 ,zshrtmprofile_i2,
        zshrtmprofile_i3, zshrtmprofile_i4, sscrfields, stxbitmaps.

INFOTYPES: 0000,0001,0002,
           0006 MODE n,
           0106 MODE n,
           0022 MODE n,
           0023 MODE n.

INCLUDE zhrr99990t.  "ALV Common Routine

DATA:
  image_control TYPE REF TO cl_gui_picture,
  container TYPE REF TO cl_gui_custom_container,
  html_viewer TYPE REF TO cl_gui_html_viewer,
  ok_code LIKE sy-ucomm.

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
*-- Print option for preview (Smartforms)
DATA: st_print_option  TYPE ssfctrlop,
      st_control_param TYPE ssfctrlop,
      st_output_option TYPE ssfcompop,
      st_output_inform TYPE ssfcrescl,
      g_func_name      TYPE rs38l_fnam.

TYPES: BEGIN OF ty_row_tab,
        zmark.
        INCLUDE STRUCTURE zshrtmprofile_h.
        INCLUDE STRUCTURE zshrtmprofile_i1.
TYPES   employeenumber TYPE zempnumber.                     "HIS20094
TYPES: END OF ty_row_tab.
DATA  g_prev.
DATA  g_cnt.

TYPES BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES celltab  TYPE lvc_t_styl.
TYPES tabcolor TYPE slis_t_specialcol_alv.
TYPES END OF ty_out.

DATA w_style TYPE lvc_s_styl.

DATA: BEGIN OF izshrtmprofile_h OCCURS 0.
        INCLUDE STRUCTURE zshrtmprofile_h.
DATA: END OF izshrtmprofile_h.
DATA: BEGIN OF izshrtmprofile_i1 OCCURS 0.
        INCLUDE STRUCTURE zshrtmprofile_i1.
DATA: END OF izshrtmprofile_i1.
DATA: BEGIN OF izshrtmprofile_i2 OCCURS 0.
        INCLUDE STRUCTURE zshrtmprofile_i2.
DATA: END OF izshrtmprofile_i2.
DATA: BEGIN OF izshrtmprofile_i3 OCCURS 0.
        INCLUDE STRUCTURE zshrtmprofile_i3.
DATA: END OF izshrtmprofile_i3.
DATA: BEGIN OF izshrtmprofile_i4 OCCURS 0.
        INCLUDE STRUCTURE zshrtmprofile_i4.
DATA: END OF izshrtmprofile_i4.

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

* Begin of HIS20094
DATA: BEGIN OF gt_out1 OCCURS 0.
        INCLUDE STRUCTURE zthr_bhisthmma.
DATA: name(120) TYPE c,
      deptm LIKE zthr_badge-deptm,                          "UD1K955629
      zhdrds LIKE zthrdoor-zhdrds,
      END OF gt_out1.
* End of HIS20094

DATA  $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.
DATA   w_gt_out    TYPE ty_out.

DATA $ix TYPE i.
DATA it001p LIKE t001p OCCURS 0 WITH HEADER LINE.
DATA it513s LIKE t513s OCCURS 0 WITH HEADER LINE.

DATA it528t LIKE t528t OCCURS 0 WITH HEADER LINE.
DATA it527x LIKE t527x OCCURS 0 WITH HEADER LINE.
DATA it513c LIKE t513c OCCURS 0 WITH HEADER LINE.

DATA it519t LIKE t519t OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF idescript OCCURS 0,
        otype TYPE otype,
        objid TYPE hrobjid,
        stext TYPE stext,
       END OF idescript.

* smart form

DATA : v_otf LIKE itcoo OCCURS 0 WITH HEADER LINE,
       v_pdf TYPE  rcl_bag_tline,
       v_size TYPE i.
DATA : BEGIN OF it_pdf OCCURS 0,
       line(134),
       END OF it_pdf.

DATA : BEGIN OF email_data.
        INCLUDE STRUCTURE sodocchgi1.
DATA : END OF email_data.
DATA : BEGIN OF email_send OCCURS 10.
        INCLUDE STRUCTURE somlreci1.
DATA : END OF email_send.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
DEFINE __focus.
  call method cl_gui_control=>set_focus
    exporting
      control = &1.
END-OF-DEFINITION.

DEFINE __u_break.
  if err_brk eq true.
    break-point.
  endif.
END-OF-DEFINITION.

* BEGIN OF UD1K955629
DATA: g_file LIKE rlgrap-filename VALUE '\\hmmsac01\HR_Badge\SAP1.XLS',
      it_badge LIKE zthr_badge OCCURS 0 WITH HEADER LINE,
      t_security TYPE zthr_bhisthmma OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_badge_tmp OCCURS 0,
        badge LIKE it_badge-badge,
        vorna LIKE it_badge-vorna,
        nachn LIKE it_badge-nachn,
        empno LIKE it_badge-empno,
        descr LIKE it_badge-descr,
        deptm LIKE it_badge-deptm,
        hmma LIKE it_badge-hmma,
      END OF it_badge_tmp.

DATA: BEGIN OF it_sec_tmp OCCURS 0.
        INCLUDE STRUCTURE zthr_bhisthmma.
DATA: vorna LIKE it_badge-vorna,
      nachn LIKE it_badge-nachn,
      deptm LIKE it_badge-deptm,
      END OF it_sec_tmp.

DATA: BEGIN OF it_hmma OCCURS 0,
        empno TYPE zthr_badge-empno,
      END OF it_hmma.

DATA: it_name(40) OCCURS 0 WITH HEADER LINE.

*- U1 Start
DATA: gt_zthr_bhisthmma_a TYPE TABLE OF zthr_bhisthmma WITH HEADER LINE.
*- U1 End

SELECTION-SCREEN BEGIN OF BLOCK tm WITH FRAME TITLE text-s03.
PARAMETERS: p_hmma AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK tm.

SELECTION-SCREEN BEGIN OF BLOCK ntm WITH FRAME TITLE text-s02.
PARAMETERS: p_nhmma AS CHECKBOX.
SELECT-OPTIONS: s_vorna FOR it_badge-vorna NO INTERVALS,
                s_nachn FOR it_badge-nachn NO INTERVALS,
                s_deptm FOR it_badge-deptm NO INTERVALS.
SELECTION-SCREEN END OF BLOCK ntm.
* END OF UD1K955629

* Begin of HIS20094
SELECTION-SCREEN BEGIN OF BLOCK sec WITH FRAME TITLE text-s01.
TABLES: zthr_bhisthmma.

SELECT-OPTIONS: s_rdate  FOR zthr_bhisthmma-rdate
                         OBLIGATORY DEFAULT sy-datum,
                s_rtime  FOR zthr_bhisthmma-rtime,
                s_badge  FOR zthr_bhisthmma-badge,
                s_readid FOR zthr_bhisthmma-readerid,
** On 01/27/14
                s_empid FOR zthr_bhisthmma-employeenumber.
** End  .
SELECTION-SCREEN END OF BLOCK sec.
* End of HIS20094

** ON 03/12/14 Badge Master get from EAI
** BEGIN OF UD1K955629
*PARAMETERS: p_update AS CHECKBOX.
** END OF UD1K955629
** End

*PARAMETERS : p_prevw AS CHECKBOX DEFAULT 'X'.              "HIS20094
PARAMETERS : p_prevw NO-DISPLAY.                            "HIS20094

PARAMETERS:
  url LIKE sapb-uri NO-DISPLAY.

* Layout
*SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari NO-DISPLAY.
*SELECTION-SCREEN END OF BLOCK b4.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End

************************************************************************
DATA  : flag_data_changed,
        info(80).
DATA: BEGIN OF ftab OCCURS 10,
        fcode(6),
      END OF ftab.
****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DATA : numxx LIKE p0006-num01,
       comxx LIKE p0006-com01.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
*  PERFORM alv_variant_f4 CHANGING p_vari.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  PERFORM default_variant.

* BEGIN OF UD1K955629
AT SELECTION-SCREEN.
  IF p_hmma IS INITIAL AND p_nhmma IS INITIAL. " AND p_update IS INITIAL.
    MESSAGE e000 WITH 'Please select HMMA and/or Non-HMMA'.
  ENDIF.
* END OF UD1K955629
*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  __cls it_row_tab.
  __process 'Gather TeamMember...' '10'.

GET pernr.
  IF p_hmma IS INITIAL.                                     "UD1K955629
    STOP.                                                   "UD1K955629
  ENDIF.                                                    "UD1K955629

  PROVIDE * FROM p0001 BETWEEN pn-begda AND pn-endda.
    CHECK p0001-endda EQ '99991231'.
    MOVE-CORRESPONDING  p0001 TO it_row_tab.
    LOOP AT p0002 WHERE endda = '99991231'.
      it_row_tab-nachn = p0002-nachn.
      it_row_tab-vorna = p0002-vorna.
      it_row_tab-midnm = p0002-midnm.
    ENDLOOP.
    CLEAR it_row_tab-begda.
    LOOP AT p0000.
      IF p0000-massn EQ 'Z0' OR p0000-massn EQ 'Z9'.
        it_row_tab-begda = p0000-begda.
        EXIT.
      ENDIF.
    ENDLOOP.
    LOOP AT p0000 WHERE endda = '99991231'.
      it_row_tab-persg = p0001-persg.
      it_row_tab-stat2 = p0000-stat2.
    ENDLOOP.

* Begin of HIS20094
    it_row_tab-employeenumber = it_row_tab-pernr.
    SHIFT it_row_tab-employeenumber LEFT DELETING LEADING '0'.
* End of HIS20094

    APPEND it_row_tab.
  ENDPROVIDE.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

** On 03/12/14 change upload excel to EAI

  SELECT badge INTO it_badge-badge
    FROM zthr_badge
    UP TO 1 ROWS.
  ENDSELECT.
  IF sy-subrc <> 0.
    MESSAGE i000 WITH 'No Badge file has been found'.
    EXIT.
  ENDIF.
* BEGIN OF UD1K955629
*  SELECT badge INTO it_badge-badge
*    FROM zthr_badge
*    UP TO 1 ROWS.
*  ENDSELECT.
*
*  IF sy-subrc <> 0 OR NOT p_update IS INITIAL.
*    __process 'Update Bagde Information...' ' '.
*
*    SELECT pernr INTO TABLE it_hmma
*      FROM pa0000
*     WHERE endda = '99991231'.
*
*    SORT it_hmma BY empno.
*    LOOP AT it_hmma.
*      SHIFT it_hmma LEFT DELETING LEADING '0'.
*      MODIFY it_hmma INDEX sy-tabix.
*    ENDLOOP.
*
*    PERFORM upload_excel TABLES it_badge_tmp
*                     USING  g_file 'X'.
*
*    IF NOT it_badge_tmp[] IS INITIAL.
*      LOOP AT it_badge_tmp.
*        MOVE-CORRESPONDING it_badge_tmp TO it_badge.
*
*        TRANSLATE: it_badge-vorna TO UPPER CASE,
*                   it_badge-nachn TO UPPER CASE,
*                   it_badge-descr TO UPPER CASE,
*                   it_badge-deptm TO UPPER CASE.
*
*        READ TABLE it_hmma WITH KEY empno = it_badge-empno
*                                    BINARY SEARCH.
*        IF sy-subrc = 0.
*          IF it_badge-descr CS 'PERMANENT' OR
*             it_badge-descr CS 'HMMA INTERN'.
*            it_badge-hmma = 'X'.
*          ENDIF.
*        ENDIF.
*
*        APPEND it_badge.
*        CLEAR it_badge.
*      ENDLOOP.
*
*      FREE it_badge_tmp.
*      DELETE FROM zthr_badge WHERE badge <> ' '.
*      INSERT zthr_badge FROM TABLE it_badge.
*
*      IF p_hmma IS INITIAL AND p_nhmma IS INITIAL.
*        MESSAGE s000 WITH 'Update Completed'.
*        EXIT.
*      ENDIF.
*
*    ELSE.
*      MESSAGE i000 WITH 'No Badge file has been found'.
*** Furong on 05/23/13 not leave the program
*      EXIT.
**      LEAVE PROGRAM.
*** end
*    ENDIF.
*  ENDIF.

** ) End on 03/12/14

  __process 'Gather Security Data...' '50'.
  IF NOT it_row_tab[] IS INITIAL.
    SORT it_row_tab BY employeenumber.

    SELECT * INTO TABLE t_security
             FROM zthr_bhisthmma
             FOR ALL ENTRIES IN it_row_tab
             WHERE rdate IN s_rdate
               AND employeenumber = it_row_tab-employeenumber
               AND rtime IN s_rtime
               AND badge IN s_badge
               AND readerid IN s_readid
** On 01/27/14
               AND employeenumber IN s_empid.
** End
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_zthr_bhisthmma.
    ENDIF.
*- U1 End
  ENDIF.

  IF NOT p_nhmma IS INITIAL.
    SELECT a~rdate
           a~rtime
           a~badge
           a~readerid
           a~employeenumber
           b~vorna
           b~nachn
           b~deptm
           INTO CORRESPONDING FIELDS OF TABLE it_sec_tmp
      FROM zthr_bhisthmma AS a JOIN zthr_badge AS b
                          ON a~badge = b~badge
     WHERE a~rdate IN s_rdate
       AND a~rtime IN s_rtime
       AND a~badge IN s_badge
       AND a~readerid IN s_readid
** On 01/27/14
       AND a~employeenumber IN s_empid
** End
       AND b~vorna IN s_vorna
       AND b~nachn IN s_nachn
       AND b~deptm IN s_deptm
       AND b~hmma = ' '.
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_zthr_bhisthmma_2.
    ENDIF.
*- U1 End
  ENDIF.

  IF t_security[] IS INITIAL AND it_sec_tmp[] IS INITIAL.
    MESSAGE i000 WITH 'No Security Data has been found'.
** Furong on 05/23/13 not leave the program
    EXIT.
*    LEAVE PROGRAM.
** End
  ENDIF.

*  IF it_row_tab[] IS INITIAL.
*    MESSAGE i000 WITH 'No Employee Data has been found'.
*    LEAVE PROGRAM.
*  ELSE.
*    __process 'Gather Security Data...' '50'.
*    SORT it_row_tab BY employeenumber.
*
*    SELECT * INTO TABLE t_security
*             FROM zthr_bhisthmma
*             FOR ALL ENTRIES IN it_row_tab
*             WHERE rdate IN s_rdate
*               AND employeenumber = it_row_tab-employeenumber
*               AND rtime IN s_rtime
*               AND badge IN s_badge
*               AND readerid IN s_readid.
*  ENDIF.
*
*  IF t_security[] IS INITIAL.
*    MESSAGE i000 WITH 'No Security Data has been found'.
*    LEAVE PROGRAM.
*  ENDIF.
* END OF UD1K955629

*  APPEND LINES OF t_security TO gt_out1.
  LOOP AT t_security.
    MOVE-CORRESPONDING t_security TO gt_out1.
    gt_out1-deptm = 'HMMA'.
    READ TABLE it_row_tab WITH KEY pernr = t_security-employeenumber
                                           BINARY SEARCH.
    IF sy-subrc = 0.
      CONCATENATE it_row_tab-vorna
                  it_row_tab-midnm
                  it_row_tab-nachn
            INTO  gt_out1-name SEPARATED BY space.
      CONDENSE gt_out1-name.
    ENDIF.

    APPEND gt_out1.
  ENDLOOP.

  FREE: it_row_tab, t_security.

* BEGIN OF UD1K955629
  CLEAR gt_out1.
  LOOP AT it_sec_tmp.
    MOVE-CORRESPONDING it_sec_tmp TO gt_out1.
    SHIFT: it_sec_tmp-vorna LEFT DELETING LEADING space,
           it_sec_tmp-nachn LEFT DELETING LEADING space.

    CONCATENATE it_sec_tmp-vorna
                it_sec_tmp-nachn
           INTO gt_out1-name SEPARATED BY space.

    SPLIT gt_out1-name AT space INTO TABLE it_name.

    CLEAR gt_out1-name.
    LOOP AT it_name.
      TRANSLATE: it_name    TO LOWER CASE,
                 it_name(1) TO UPPER CASE.
      CONCATENATE gt_out1-name it_name
             INTO gt_out1-name SEPARATED BY space.
    ENDLOOP.
    CONDENSE gt_out1-name.
    APPEND gt_out1.
  ENDLOOP.

** Added by furong on 01/08/14
  DATA: lt_door LIKE TABLE OF zthrdoor WITH HEADER LINE.

  SELECT * INTO TABLE lt_door
     FROM zthrdoor
    FOR ALL ENTRIES IN gt_out1
    WHERE zhdoor = gt_out1-readerid.
  SORT lt_door BY zhdoor.

  LOOP AT gt_out1.
    READ TABLE lt_door WITH KEY zhdoor = gt_out1-readerid
         BINARY SEARCH.
    IF sy-subrc = 0.
      gt_out1-zhdrds = lt_door-zhdrds.
      MODIFY gt_out1.
    ENDIF.
  ENDLOOP.
  FREE lt_door.
** End

  FREE: it_sec_tmp.
* END OF UD1K955629

*  PERFORM make_gt_out.
*  IF p_prevw = 'X'.  "Preview for Smartforms
*    $gt_out[] = gt_out[].
*    PERFORM smartform_call USING p_prevw ''." g_begda g_endda.
*
*  ELSE.

* Defined Sort
  PERFORM built_sort.

* Defined Event
  PERFORM build_eventcat USING '1'.
  DELETE gt_events_lvc WHERE name = g_top_of_page.

  DESCRIBE TABLE gt_out LINES g_cnt.
  PERFORM fieidcat_gathering.
  PERFORM call_function TABLES gt_out1
                        USING 'ALV_USER_COMMAND' 'ALV_PF_STATUS_SET'.

*  ENDIF.

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
    IF ls_mod_cells-fieldname = 'ZMARK'.
      PERFORM chk_input USING ls_mod_cells-fieldname
                              rr_data_changed ls_mod_cells.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " ALV_DATA_CHANGED

*---------------------------------------------------------------------*
*       FORM chk_input                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELD                                                       *
*  -->  RR_DATA_CHANGED                                               *
*  -->  RS_MOD_CELLS                                                  *
*---------------------------------------------------------------------*
FORM chk_input  USING   p_field   rr_data_changed TYPE REF TO
                                  cl_alv_changed_data_protocol
                                  rs_mod_cells TYPE lvc_s_modi.

  READ TABLE gt_out INDEX rs_mod_cells-row_id.

  CHECK sy-subrc = 0.

  CASE p_field.
    WHEN 'ZMARK'.
      gt_out-zmark = rs_mod_cells-value.
  ENDCASE.

  MODIFY gt_out INDEX rs_mod_cells-row_id.

ENDFORM.                    " CHK_INPUT

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
    WHEN '&IC1'.           "Double-Click
      PERFORM get_photo USING  rs_selfield-tabindex.
      EXIT.
    WHEN 'RBACK' OR 'RCANC'.
      CLEAR : r_ucomm.
      SET SCREEN 0.

* All Selection
    WHEN 'SALL'.
      gt_out-zmark = 'X'.
      MODIFY gt_out TRANSPORTING zmark
                           WHERE zmark IS INITIAL.

* All Deselection
    WHEN 'SSAL'.
      gt_out-zmark = ''.
      MODIFY gt_out TRANSPORTING zmark
                              WHERE NOT zmark IS INITIAL.

* Print the evaluation form
    WHEN 'PRINT'.
      READ TABLE gt_out WITH KEY zmark = 'X'.
      IF sy-subrc NE 0.
        MESSAGE i000 WITH 'Please select  at least 1 line'.
      ELSE.
        REFRESH $gt_out.
        LOOP AT gt_out WHERE zmark = 'X'.
          APPEND gt_out TO $gt_out.
        ENDLOOP.
        PERFORM smartform_call USING 'X' ''." g_begda g_endda.
      ENDIF.

*    WHEN 'PICK'.
*      CLEAR gt_out.
*      CHECK NOT rs_selfield-value IS p_paydt.
*      READ TABLE gt_out INDEX rs_selfield-tabindex.
*      PERFORM read_detail USING gt_out-ansvh.

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
*&      Form  get_text_tables
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_text_tables.

  IF it001p[] IS INITIAL.
    SELECT * INTO TABLE it001p FROM t001p.
    SORT it001p BY werks btrtl.
  ENDIF.

  IF it513s[] IS INITIAL.
    SELECT * INTO TABLE it513s FROM t513s WHERE sprsl EQ sy-langu
                                AND endda = '99991231'.
    SORT it513s BY stell.
  ENDIF.

  IF it528t[] IS INITIAL.
    SELECT * INTO TABLE it528t FROM t528t WHERE sprsl EQ sy-langu
                                AND endda = '99991231'.
    SORT it528t BY otype plans.
  ENDIF.

  IF it527x[] IS INITIAL.
    SELECT * INTO TABLE it527x FROM t527x WHERE sprsl EQ sy-langu
                                AND endda = '99991231'.
    SORT it527x BY orgeh.
  ENDIF.

  IF it513c[] IS INITIAL.
    SELECT * INTO TABLE it513c FROM t513c WHERE spras EQ sy-langu.
    SORT it513c BY taete.
  ENDIF.

  IF idescript[] IS INITIAL.
    SELECT otype objid stext INTO TABLE idescript FROM hrp1000
        WHERE plvar EQ '01'
          AND otype IN ('O','C','S')
          AND istat EQ '1'
          AND endda EQ '99991231'
          AND langu EQ sy-langu.

    SORT idescript BY otype objid.
  ENDIF.

  IF it519t[] IS INITIAL.
    SELECT * INTO TABLE it519t FROM t519t WHERE sprsl EQ sy-langu.
    SORT it519t BY slabs.
  ENDIF.

ENDFORM.                    " get_text_tables
*&---------------------------------------------------------------------*
*&      Form  print_profile
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_profile  USING p_control_param
                          p_output_option.


  DATA month LIKE t247-ltx.
  DATA $cnt TYPE i.
  DATA w_ph_pernr(30).

  LOOP AT $gt_out.

    $ix = sy-tabix.

    __cls : izshrtmprofile_h,   " Header
            izshrtmprofile_i1.  " Personal Recoreds

    MOVE-CORRESPONDING $gt_out TO izshrtmprofile_h.


    PERFORM remove_dash CHANGING izshrtmprofile_h-telwk.

    WRITE izshrtmprofile_h-telwk TO izshrtmprofile_h-telwk
    USING EDIT MASK '___-___-____'.

    __cls : p0006.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr         = $gt_out-pernr
        infty         = '0006'
        begda         = sy-datum
        endda         = '99991231'
        bypass_buffer = 'X'
      TABLES
        infty_tab     = p0006.

    LOOP AT p0006.
      DO 6 TIMES VARYING numxx FROM p0006-num01 NEXT p0006-num02
                 VARYING comxx FROM p0006-com01 NEXT p0006-com02.
        IF comxx EQ 'WORK'.

          PERFORM remove_dash CHANGING numxx.

          WRITE numxx TO izshrtmprofile_h-telwk
          USING EDIT MASK '___-___-____'.
        ENDIF.

        IF comxx EQ 'CELL'.

          PERFORM remove_dash CHANGING numxx.

          WRITE numxx TO izshrtmprofile_i1-telce
          USING EDIT MASK '___-___-____'.
        ENDIF.
      ENDDO.

      izshrtmprofile_i1-stras  = p0006-stras.
      izshrtmprofile_i1-ort01  = p0006-ort01.
      izshrtmprofile_i1-state  = p0006-state.
      izshrtmprofile_i1-pstlz  = p0006-pstlz.

      PERFORM remove_dash CHANGING p0006-telnr.
      IF p0006-subty EQ '1'.
        WRITE p0006-telnr TO izshrtmprofile_i1-telhm
        USING EDIT MASK '___-___-____'.
      ENDIF.
    ENDLOOP.

    __cls : p0106.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr         = $gt_out-pernr
        infty         = '0106'
        begda         = sy-datum
        endda         = '99991231'
        bypass_buffer = 'X'
      TABLES
        infty_tab     = p0106.

    LOOP AT p0106.

      PERFORM remove_dash CHANGING p0106-telnr.

      WRITE p0106-telnr TO izshrtmprofile_i1-telem
        USING EDIT MASK '___-___-____'.

      EXIT.
    ENDLOOP.

    APPEND izshrtmprofile_i1.

    __cls : izshrtmprofile_i2,  " Education/Certificat
            izshrtmprofile_i3,  " Job Histories
            izshrtmprofile_i4.  " Previous Employment

    __cls : p0022.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr         = $gt_out-pernr
        infty         = '0022'
        bypass_buffer = 'X'
      TABLES
        infty_tab     = p0022.

    READ TABLE p0022 WITH KEY slart = 'U1'.
    IF sy-subrc NE 0.
      CLEAR p0022.
      p0022-slart = 'U1'.
      APPEND p0022.    CLEAR p0022.
    ENDIF.

    READ TABLE p0022 WITH KEY slart = 'U2'.
    IF sy-subrc NE 0.
      CLEAR p0022.
      p0022-slart = 'U2'.
      APPEND p0022.
    ENDIF.

    READ TABLE p0022 WITH KEY slart = 'U3'.
    IF sy-subrc NE 0.
      CLEAR p0022.
      p0022-slart = 'U3'.
      APPEND p0022.
    ENDIF.

    READ TABLE p0022 WITH KEY slart = 'U4'.
    IF sy-subrc NE 0.
      CLEAR p0022.
      p0022-slart = 'U4'.
      APPEND p0022.    CLEAR p0022.
    ENDIF.

    READ TABLE p0022 WITH KEY slart = 'U6'.
    IF sy-subrc NE 0.
      CLEAR p0022.
      p0022-slart = 'U6'.
      APPEND p0022.    CLEAR p0022.
    ENDIF.

    DATA $insti(80).
    __cls izshrtmprofile_i2.
    CLEAR $insti.

    DELETE p0022 WHERE slart = 'U5'.
    LOOP AT p0022.
      READ TABLE it519t WITH KEY slabs = p0022-slabs BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF p0022-insti NE space.
          CONCATENATE p0022-insti '' "',' "it519t-stext
            INTO izshrtmprofile_i2-insti SEPARATED BY space.
        ENDIF.
      ELSE.
        izshrtmprofile_i2-insti = p0022-insti.
      ENDIF.

      izshrtmprofile_i2-slart = p0022-slart.

      CASE p0022-slart.
        WHEN 'U1'.
          izshrtmprofile_i2-text = 'High School'.
        WHEN  'U2'.
          izshrtmprofile_i2-text = 'Technical School'.
        WHEN  'U3'.
          izshrtmprofile_i2-text = 'College'.
        WHEN 'U4'.
          izshrtmprofile_i2-text = 'Graduate School'.
        WHEN 'U6'.
          IF $insti IS INITIAL.
            MOVE izshrtmprofile_i2-insti TO $insti.
          ELSE.
            CONCATENATE $insti ',' izshrtmprofile_i2-insti INTO $insti.
          ENDIF.
          CLEAR izshrtmprofile_i2.
          CONTINUE.
      ENDCASE.
      APPEND izshrtmprofile_i2.CLEAR izshrtmprofile_i2.
    ENDLOOP.

    IF NOT $insti  IS INITIAL.
      izshrtmprofile_i2-slart = 'U6'.
      izshrtmprofile_i2-text = 'Certificate'.
      izshrtmprofile_i2-insti = $insti.
      APPEND izshrtmprofile_i2.
    ENDIF.
    SORT izshrtmprofile_i2 BY slart.

    __cls : p0001.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr         = $gt_out-pernr
        infty         = '0001'
        bypass_buffer = 'X'
      TABLES
        infty_tab     = p0001.

    LOOP AT p0001.

      izshrtmprofile_i3-begda = p0001-begda.
      izshrtmprofile_i3-endda = p0001-endda.
      READ TABLE it001p WITH KEY werks = p0001-werks
                                 btrtl = p0001-btrtl
                                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        izshrtmprofile_i3-divis = it001p-btext.
      ENDIF.

      READ TABLE it513s WITH KEY stell = p0001-stell
                                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        izshrtmprofile_i3-stltx = it513s-stltx .
      ENDIF.

      READ TABLE it528t WITH KEY otype = 'S'
                                 plans = p0001-plans
                                 BINARY SEARCH.

      IF sy-subrc EQ 0.
        izshrtmprofile_i3-jobre = it528t-plstx .
      ENDIF.

      izshrtmprofile_i3-stell = p0001-stell.
      izshrtmprofile_i3-plans = p0001-plans.
      APPEND izshrtmprofile_i3.

    ENDLOOP.

    PERFORM reduce_line_history TABLES izshrtmprofile_i3.
    SORT izshrtmprofile_i3 BY begda DESCENDING.

    LOOP AT izshrtmprofile_i3.
      $ix = sy-tabix.
      PERFORM re247 USING    izshrtmprofile_i3-begda+4(2)
                CHANGING month.
      CONCATENATE month '-' izshrtmprofile_i3-begda+2(2) INTO
izshrtmprofile_i3-begdatx.

      IF izshrtmprofile_i3-endda EQ '99991231'.
        izshrtmprofile_i3-enddatx = 'Present'.
      ELSE.
        PERFORM re247 USING    izshrtmprofile_i3-endda+4(2)
                      CHANGING month.
        CONCATENATE month '-' izshrtmprofile_i3-endda+2(2) INTO
  izshrtmprofile_i3-enddatx.
      ENDIF.

      SELECT SINGLE plstx INTO izshrtmprofile_i3-jobre
                        FROM t528t WHERE sprsl EQ sy-langu
                            AND otype EQ 'S'
                            AND plans EQ izshrtmprofile_i3-plans
                            AND endda >= izshrtmprofile_i3-endda
                            AND begda <= izshrtmprofile_i3-endda.

      MODIFY izshrtmprofile_i3 INDEX $ix TRANSPORTING begdatx
      enddatx jobre.
    ENDLOOP.

    __cls : p0023.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr         = $gt_out-pernr
        infty         = '0023'
        bypass_buffer = 'X'
      TABLES
        infty_tab     = p0023.

    LOOP AT p0023.
      izshrtmprofile_i4-begda = p0023-begda.
      izshrtmprofile_i4-endda = p0023-endda.
      izshrtmprofile_i4-arbgb = p0023-arbgb.
      izshrtmprofile_i4-taete = p0023-taete.
      READ TABLE it513c WITH KEY taete = p0023-taete
                                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        izshrtmprofile_i4-plstx = it513c-ltext .
      ENDIF.

      PERFORM re247 USING    izshrtmprofile_i4-begda+4(2)
                    CHANGING month.
      CONCATENATE month '-' izshrtmprofile_i4-begda+2(2) INTO
izshrtmprofile_i4-begdatx.

      IF izshrtmprofile_i4-endda EQ '99991231'.
        izshrtmprofile_i4-enddatx = 'Present'.
      ELSE.
        PERFORM re247 USING    izshrtmprofile_i4-endda+4(2)
                      CHANGING month.
        CONCATENATE month '-' izshrtmprofile_i4-endda+2(2) INTO
  izshrtmprofile_i4-enddatx.
      ENDIF.

      APPEND izshrtmprofile_i4.
    ENDLOOP.

* for test
*    do 20 times.
*    append  izshrtmprofile_i4.
*    enddo.

    SORT izshrtmprofile_i4 BY begda DESCENDING.

    DESCRIBE TABLE izshrtmprofile_i3 LINES $cnt.
    IF $cnt > 10.
      izshrtmprofile_h-jobhistorytext =
'(Note:Some records are not displayed here due to the limited space.)'.
    ELSE.
      izshrtmprofile_h-jobhistorytext = ''.
    ENDIF.

    DESCRIBE TABLE izshrtmprofile_i4 LINES $cnt.
    IF $cnt > 10.
      izshrtmprofile_h-prevhistorytext =
'(Note:Some records are not displayed here due to the limited space.)'.
    ELSE.
      izshrtmprofile_h-prevhistorytext = ''.
    ENDIF.

*    IF sy-sysid NE 'UP2'.
*      CALL FUNCTION 'HRWPC_RFC_EP_READ_PHOTO_URI'
*        DESTINATION 'UP2'
*        EXPORTING
*          pernr                  = izshrtmprofile_h-pernr
*          datum                  = sy-datum
*          tclas                  = 'A'
*        IMPORTING
*          uri                    = izshrtmprofile_h-uri
*       EXCEPTIONS
*         not_supported          = 1
*         nothing_found          = 2
*         no_authorization       = 3
*         internal_error         = 4
*         OTHERS                 = 5        .
*    ELSE.
*      CALL FUNCTION 'HRWPC_RFC_EP_READ_PHOTO_URI'
*           EXPORTING
*                pernr            = izshrtmprofile_h-pernr
*                datum            = sy-datum
*                tclas            = 'A'
*           IMPORTING
*                uri              = izshrtmprofile_h-uri
*           EXCEPTIONS
*                not_supported    = 1
*                nothing_found    = 2
*                no_authorization = 3
*                internal_error   = 4
*                OTHERS           = 5.
*    ENDIF.

    IF sy-subrc <> 0.
      CLEAR izshrtmprofile_h-uri.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


*EEGRP_TEXT
*EESTATUS

    SELECT SINGLE ptext INTO izshrtmprofile_h-eegrp_text
    FROM t501t
    WHERE sprsl EQ sy-langu
      AND persg EQ $gt_out-persg.

    SELECT SINGLE text1 INTO izshrtmprofile_h-eestatus
    FROM t529u
    WHERE sprsl EQ sy-langu
      AND statn EQ '2'
      AND statv EQ $gt_out-stat2.

    APPEND izshrtmprofile_h.

*    CLEAR : p_output_option.
*     clear p_control_param.

    CLEAR st_output_inform.
    CONCATENATE 'HR_TM_' izshrtmprofile_h-pernr+2 INTO w_ph_pernr.
    SELECT SINGLE * FROM stxbitmaps
                  WHERE tdobject EQ 'GRAPHICS'
                   AND  tdname EQ w_ph_pernr
                   AND  tdid EQ 'BMAP'.
    IF sy-subrc EQ 0.
    ELSE.
      w_ph_pernr = 'HR_NO_PHOTO'.
    ENDIF.

    CALL FUNCTION g_func_name
         EXPORTING
              control_parameters = p_control_param
              output_options     = p_output_option
              user_settings      = space
              it_dn_h            = izshrtmprofile_h
              w_ph_pernr         = w_ph_pernr
*         IMPORTING
*              JOB_OUTPUT_INFO =   st_output_inform
         TABLES
              it_dn_i1           = izshrtmprofile_i1
              it_dn_i2           = izshrtmprofile_i2
              it_dn_i3           = izshrtmprofile_i3
              it_dn_i4           = izshrtmprofile_i4
         EXCEPTIONS
              formatting_error   = 1
              internal_error     = 2
              send_error         = 3
              user_canceled      = 4
              OTHERS             = 5.


    IF sy-subrc <> 0.
      MESSAGE s000 WITH g_func_name 'Error' sy-subrc.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " print_profile

*---------------------------------------------------------------------*
*       FORM re247                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  MONTH_AUX                                                     *
*  -->  MONTH                                                         *
*---------------------------------------------------------------------*
FORM re247 USING    month_aux
           CHANGING month LIKE t247-ltx.

  DATA: BEGIN OF month_name OCCURS 12.
          INCLUDE STRUCTURE t247.
  DATA: END OF month_name.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
          language              = sy-langu
*    IMPORTING
*         RETURN_CODE           =
     TABLES
          month_names           = month_name
     EXCEPTIONS
          month_names_not_found = 1
          OTHERS                = 2.

  READ TABLE month_name WITH KEY
                        spras = sy-langu mnr = month_aux
                        BINARY SEARCH.
  IF sy-subrc EQ 0.
    MOVE month_name-ltx(3) TO month.
*  ELSE.
*    MESSAGE w071.
  ENDIF.
ENDFORM.                                                    "re247


*---------------------------------------------------------------------*
*       FORM set_parameter                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_PREVIEW                                                     *
*  -->  P_PAGE                                                        *
*  -->  CH_CONTROL_PARAM                                              *
*  -->  CH_OUTPUT_OPTION                                              *
*---------------------------------------------------------------------*
FORM set_parameter     USING  p_preview  p_page
                    CHANGING  ch_control_param TYPE ssfctrlop
                              ch_output_option TYPE ssfcompop.

  ch_control_param-no_dialog = 'X'.
  ch_control_param-preview   = p_preview.  "Preview or Print
  ch_control_param-langu     = sy-langu.

  ch_control_param-no_open   = 'X'.
  ch_control_param-no_close  = 'X'.

*  ch_control_param-device    = 'Qkrseop14404'.
*  ch_output_option-tdprinter     = 'POST2'.

  ch_output_option-tdnewid       = 'X'.
  ch_output_option-tdimmed       = 'X'.
  ch_output_option-tddelete      = 'X'.       " 'X' after tests
  ch_output_option-tdlifetime    = '0'.       " '0' days after test

*  ch_output_option-tdautority    = space.
*  ch_output_option-tdfinal       = 'X'.

  ch_output_option-tdcover       = ' '.       " no cover sheet
  ch_output_option-tdcopies      = '001'.
  ch_output_option-tdpageslct    = p_page.    " Page Select

ENDFORM.                    " set_parameter
*&---------------------------------------------------------------------*
*&      Form  make_gt_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_gt_out.

  __process 'Fill Text...' '50'.

  PERFORM get_text_tables.

  LOOP AT it_row_tab.
    $ix = sy-tabix.
    READ TABLE it001p WITH KEY werks = it_row_tab-werks
                               btrtl = it_row_tab-btrtl
                               BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-divis = it001p-btext.
    ENDIF.

*    READ TABLE it528t WITH KEY otype = 'S'
*                               plans = it_row_tab-plans
*                               BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      it_row_tab-jobre = it528t-plstx .
*    ENDIF.

*    READ TABLE it513s WITH KEY stell = it_row_tab-stell
*                               BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      it_row_tab-stltx = it513s-stltx .
*    ENDIF.

*    READ TABLE it527x WITH KEY orgeh = it_row_tab-orgeh
*                               BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      it_row_tab-secna = it527x-orgtx.
*    ENDIF.

    READ TABLE idescript WITH KEY otype = 'S'
                                  objid = it_row_tab-plans
                                  BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-jobre = idescript-stext.
    ENDIF.

    READ TABLE idescript WITH KEY otype = 'C'
                                  objid = it_row_tab-stell
                                  BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-stltx = idescript-stext.
    ENDIF.

    READ TABLE idescript WITH KEY otype = 'O'
                                  objid = it_row_tab-orgeh
                                  BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-secna = idescript-stext.
    ENDIF.

    MODIFY it_row_tab INDEX $ix TRANSPORTING divis jobre secna stltx.
  ENDLOOP.

  PERFORM move_out.

ENDFORM.                    " make_gt_out
*&---------------------------------------------------------------------*
*&      Form  smartform_call
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_gt_out  text
*      -->P_P_PREVW  text
*      -->P_0511   text
*----------------------------------------------------------------------*
FORM smartform_call USING p_prevw1 p_pages1." p_begda p_endda.

  CLEAR st_print_option.
  PERFORM set_parameter  USING p_prevw1 p_pages1
                      CHANGING st_control_param
                               st_output_option.

  PERFORM call_form_name USING 'ZHRFTMPROFILE'
                         g_func_name.
  PERFORM pro_open_ssf   USING st_control_param
                               st_output_option.
  PERFORM print_profile USING st_control_param st_output_option.
  PERFORM pro_close_ssf.

ENDFORM.                    " smartform_call
*&---------------------------------------------------------------------*
*&      Form  call_form_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2781   text
*      -->P_G_FUNC_NAME  text
*----------------------------------------------------------------------*
FORM call_form_name  USING  p_formname
                  CHANGING  ch_func_name.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = p_formname
    IMPORTING
      fm_name            = ch_func_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " call_form_name
*&---------------------------------------------------------------------*
*&      Form  pro_open_ssf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_CONTROL_PARAM  text
*      -->P_ST_OUTPUT_OPTION  text
*----------------------------------------------------------------------*
FORM pro_open_ssf  USING p_control_param LIKE ssfctrlop
                         p_output_option TYPE ssfcompop.

  DATA: %input     TYPE ssfcompin,
        %result_op TYPE ssfcresop.

*  CALL FUNCTION 'SSF_OPEN'
*    EXPORTING
*      control_parameters = p_control_param
*      output_options     = p_output_option
*      user_settings      = space
*    EXCEPTIONS
*      formatting_error   = 1
*      internal_error     = 2
*      send_error         = 3
*      user_canceled      = 4
*      OTHERS             = 5.
*
*  CHECK not sy-subrc IS INITIAL.
*
*  MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*


  CLEAR %input.

*  if user_settings <> space.
*    output_options-tddest   = '*'.
*    output_options-tdimmed  = '*'.
*    output_options-tddelete = '*'.
*  endif.

  MOVE-CORRESPONDING p_output_option TO %input.
  IF p_control_param-no_dialog = space.
    %input-dialog = 'X'.
  ENDIF.

  %input-device            = p_control_param-device.
  %input-tdpreview         = p_control_param-preview.
  %input-tdgetotf          = p_control_param-getotf.

  %input-tdprogram         = sy-repid.

  CALL FUNCTION 'SSFCOMP_OPEN'
    EXPORTING
      input  = %input
    IMPORTING
      result = %result_op
    EXCEPTIONS
      OTHERS = 1.

ENDFORM.                    " pro_open_ssf
*&---------------------------------------------------------------------*
*&      Form  pro_close_ssf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pro_close_ssf.

  CALL FUNCTION 'SSF_CLOSE'
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  CHECK  NOT sy-subrc IS INITIAL.

  MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

ENDFORM.                    " pro_close_ssf

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
** Changed by Furong on 10/29/12 for download file
*  gs_layout-stylefname = 'CELLTAB'.
*  gs_layout-ctab_fname = 'TABCOLOR'.
** End on 10/29/12

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
    gs_fieldcat_lvc-outputlen     = &4.        " Column width
    gs_fieldcat_lvc-datatype      = &5.        " Data type
    gs_fieldcat_lvc-emphasize     = &6.
    append gs_fieldcat_lvc to  gt_fieldcat_lvc.
  END-OF-DEFINITION.


  __catalog :
*    ' ' 'ZMARK'       ''                        1   'CHAR' '',
*    ' ' 'PERNR'       'Emp#'                     8   'NUMC' '',
*    ' ' 'NACHN'       'Last Name'                40  'CHAR' '',
*    ' ' 'VORNA'   'First Name'               40  'CHAR' '',
*    ' ' 'MIDNM'   'Middle Name'              40  'CHAR' '',
*    ' ' 'WERKS'   'Per.Area'           4   'CHAR' '',
*    ' ' 'BTRTL'   'Per.Subarea'        4   'CHAR' '',
*    ' ' 'DIVIS'   'Per.Sub.Text'           15  'CHAR' '',
*    ' ' 'ORGEH'   'Org.Unit'      8   'NUMC' '',
*    ' ' 'SECNA'   'Org.Unit Text' 25  'CHAR' '',
*    ' ' 'BEGDA'   'Start Date'               8   'DATS' '',
*    ' ' 'STELL'   'Job'                      8   'NUMC' '',
*    ' ' 'STLTX'   'Job title'                25  'CHAR' '',
*    ' ' 'PLANS'   'Position'                 8   'NUMC' '',
*    ' ' 'JOBRE'   'Position Name'            25  'CHAR' ''.

    ' ' 'EMPLOYEENUMBER' '[HR] Emp. # for Security History'
                                                    20  'CHAR' '',
    ' ' 'NAME'  'Name'                             120  'CHAR' '',
    ' ' 'BADGE' '[HR] Badge # for Security History' 20  'CHAR' '',
    ' ' 'RDATE' '[HR] Door read Date'                8  'DATS' '',
    ' ' 'RTIME' '[HR] Scan Time for Scurity History' 6  'TIMS' '',

    ' ' 'READERID' '[HR] Door Reader ID'             5  'NUMC' '',
    ' ' 'ZHDRDS'   '[HR] Door Description'          30  'CHAR' '',
    ' ' 'DEPTM' '[HR] Organization'                 40  'CHAR' ''.

  LOOP AT gt_fieldcat_lvc INTO gs_fieldcat_lvc.
*    CHECK gs_fieldcat_lvc-fieldname NE 'ZMARK'.

    gs_fieldcat_lvc-tabname = 'ZTHR_BHISTHMMA'.
    gs_fieldcat_lvc-ref_table = 'ZTHR_BHISTHMMA'.
    gs_fieldcat_lvc-ref_field = gs_fieldcat_lvc-fieldname.

    IF gs_fieldcat_lvc-fieldname = 'RDATE'.
      gs_fieldcat_lvc-scrtext_l = '[HR] Door read Date'.
    ENDIF.

    IF gs_fieldcat_lvc-fieldname = 'NAME'.
      gs_fieldcat_lvc-scrtext_l = 'Full Name'.
    ENDIF.

    IF gs_fieldcat_lvc-fieldname = 'ZHDRDS'.
      gs_fieldcat_lvc-scrtext_l = 'Door Description'.
    ENDIF.
** Changed by Furong on 10/29/12 for download file
* BEGIN OF UD1K955629
*    IF gs_fieldcat_lvc-fieldname = 'DEPTM'.
*      gs_fieldcat_lvc-tabname   = 'ZTHR_BADGE'.
*      gs_fieldcat_lvc-ref_table = 'ZTHR_BADGE'.
*      gs_fieldcat_lvc-scrtext_l = 'Organization'.
*    ENDIF.
* END OF UD1K955629
** End on 10/29/12

    MODIFY gt_fieldcat_lvc FROM gs_fieldcat_lvc.
  ENDLOOP.


*  LOOP AT gt_fieldcat_lvc INTO gs_fieldcat_lvc.
*
*    CASE gs_fieldcat_lvc-fieldname.
*      WHEN 'ZMARK'.
*        gs_fieldcat_lvc-edit     = 'X'.
*        gs_fieldcat_lvc-checkbox = 'X'.
*      WHEN 'PERNR' OR 'VORNA' OR 'NACHN' OR 'TITTX' OR 'ORGTX'.
*        gs_fieldcat_lvc-outputlen = 10.
*        gs_fieldcat_lvc-emphasize = 'C100'.
*
*** Title Code, Org. Unit,  etc...
**      WHEN OTHERS.
**        gs_fieldcat_lvc-no_out   = 'X'.
*
*    ENDCASE.
*
*    MODIFY gt_fieldcat_lvc FROM gs_fieldcat_lvc.
*  ENDLOOP.
*
*  PERFORM set_color.

ENDFORM.                    "fieidcat_gathering
" fieidcat_gathering

*---------------------------------------------------------------------*
*       FORM alv_top_of_list                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM alv_top_of_list.

  DATA l_text(60).
  REFRESH gt_header.

  l_text = '[HR] TeamMember Profile'.
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
  g_repid  = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'  "'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*    i_interface_check                 = ' '
*    i_buffer_active                   = 'X'
     i_callback_program                = g_repid
     i_callback_pf_status_set          = p_alv_pf_status_set
     i_callback_user_command           = p_user_command
*     i_callback_top_of_page           = 'ALV_TOP_OF_LIST'
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
          'PERNR'      '1' 0,
          'NACHN'      '2' 0,
          'VORNA'      '2' 0,
          'MIDNM'      '2' 0,
          'WERKS'      '2' 0,
     'BTRTL'      '2' 0,
     'DIVIS'     '2' 0,
     'ORGEH'     '2' 0,
     'SECNA'     '2' 0,
     'BEGDA'     '2' 0,
     'STELL'     '2' 0,
     'STLTX'       '2' 0,
     'PLANS'     '2' 0,
     'JOBRE'     '2' 0.

  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS INITIAL.

ENDFORM.                    " set_color
*&---------------------------------------------------------------------*
*&      Form  GET_PHOTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RS_SELFIELD_TABINDEX  text
*----------------------------------------------------------------------*
FORM get_photo USING pf_index.

  READ TABLE gt_out INDEX pf_index.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  CLEAR url.

  IF sy-sysid NE 'UP2'.
    CALL FUNCTION 'HRWPC_RFC_EP_READ_PHOTO_URI'
      DESTINATION 'UP2'
      EXPORTING
        pernr            = gt_out-pernr
        datum            = sy-datum
        tclas            = 'A'
      IMPORTING
        uri              = url
      EXCEPTIONS
        not_supported    = 1
        nothing_found    = 2
        no_authorization = 3
        internal_error   = 4
        OTHERS           = 5.
  ELSE.
    CALL FUNCTION 'HRWPC_RFC_EP_READ_PHOTO_URI'
      EXPORTING
        pernr            = gt_out-pernr
        datum            = sy-datum
        tclas            = 'A'
      IMPORTING
        uri              = url
      EXCEPTIONS
        not_supported    = 1
        nothing_found    = 2
        no_authorization = 3
        internal_error   = 4
        OTHERS           = 5.
  ENDIF.

  CHECK NOT url IS INITIAL.

  CALL FUNCTION 'HR_IMAGE_DETAIL'
    EXPORTING
*     P_PERNR =
      p_tclas = 'A'
      begda   = '18000101'
      endda   = '99991231'
      size_x  = 140
      size_y  = 100
      url     = url.

ENDFORM.                    " GET_PHOTO
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
*&      Form  BUILT_SORT
*&---------------------------------------------------------------------*
*       Build sort criteria
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM built_sort.
  PERFORM alv_sortcat_lvc USING  'EMPLOYEENUMBER'  'X'  ' '.
  PERFORM alv_sortcat_lvc USING  'NAME'            'X'  ' '.
  PERFORM alv_sortcat_lvc USING  'BADGE'           'X'  ' '.
  PERFORM alv_sortcat_lvc USING  'RDATE'           'X'  ' '.
  PERFORM alv_sortcat_lvc USING  'RTIME'           'X'  ' '.
ENDFORM.                    " BUILT_SORT

* BEGIN OF UD1K955629
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_EXCEL
*&---------------------------------------------------------------------*
*       Upload excel to internal table
*----------------------------------------------------------------------*
*      -->P_EXCEL  Excel internal table
*      -->P_FILE   File name
*      -->P_HEADER File with header
*----------------------------------------------------------------------*
FORM upload_excel  TABLES   p_excel
                   USING    p_file
                            p_header.

  DATA: l_begin_row TYPE i VALUE 1,
        l_end_col   TYPE i VALUE 100,
        l_end_row   TYPE i,
        l_read_rec  TYPE i VALUE 1,
        l_row       TYPE i,
        it_intern   TYPE kcde_cells OCCURS 0 WITH HEADER LINE.

  FIELD-SYMBOLS : <fs>.

  DO.
    ASSIGN COMPONENT sy-index OF STRUCTURE p_excel TO <fs>.
    IF sy-subrc <> 0.
      l_end_col = sy-index - 1.
      EXIT.
    ENDIF.
  ENDDO.

  CHECK NOT l_end_col IS INITIAL.

  IF NOT p_header IS INITIAL.
    l_begin_row = l_begin_row + 1.
  ENDIF.

  DO.
    IF l_read_rec < 9999.
      l_read_rec = l_read_rec * 10.
      IF l_read_rec > 9999.
        l_read_rec = 9999.
      ENDIF.
    ENDIF.

    l_end_row = l_begin_row + l_read_rec - 1.

    CLEAR: it_intern, it_intern[].

    CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
      EXPORTING
        filename                = p_file
        i_begin_col             = 1
        i_begin_row             = l_begin_row
        i_end_col               = l_end_col
        i_end_row               = l_end_row
      TABLES
        intern                  = it_intern
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE i002(fb) WITH g_file.
      EXIT.
    ELSEIF it_intern[] IS INITIAL.
      EXIT.
    ELSE.
      SORT it_intern BY row col.
      LOOP AT it_intern.
        ASSIGN COMPONENT it_intern-col OF STRUCTURE p_excel TO <fs>.
        IF sy-subrc = 0.
          <fs> = it_intern-value.
        ENDIF.

        AT END OF row.
          APPEND p_excel.
          CLEAR p_excel.
        ENDAT.
      ENDLOOP.

      DESCRIBE TABLE it_intern LINES l_row.
      READ TABLE it_intern INDEX l_row.
      l_row = it_intern-row.
      IF l_row < l_read_rec.
        EXIT.
      ENDIF.
    ENDIF.

    l_begin_row = l_end_row + 1.
  ENDDO.

ENDFORM.                    " UPLOAD_EXCEL
* END OF UD1K955629
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
    FOR ALL ENTRIES IN it_row_tab
   WHERE rdate IN s_rdate
     AND employeenumber = it_row_tab-employeenumber
     AND rtime IN s_rtime
     AND badge IN s_badge
     AND readerid IN s_readid
** On 01/27/14
     AND employeenumber IN s_empid.
** End

  CHECK NOT lt_inx_zthr_bhisthmma[] IS INITIAL.

  LOOP AT lt_inx_zthr_bhisthmma INTO ls_inx_zthr_bhisthmma.
    CLEAR t_security.
    MOVE-CORRESPONDING ls_inx_zthr_bhisthmma TO t_security.
    APPEND t_security.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_ZTHR_BHISTHMMA
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTHR_BHISTHMMA_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_zthr_bhisthmma_2 .

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
        lt_zthr_badge TYPE TABLE OF zthr_badge WITH HEADER LINE,
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
   WHERE rdate    IN s_rdate
     AND rtime    IN s_rtime
     AND badge    IN s_badge
     AND readerid IN s_readid
** On 01/27/14
     AND employeenumber IN s_empid.
** End

  CHECK NOT lt_inx_zthr_bhisthmma[] IS INITIAL.

  CLEAR: lt_zthr_bhisthmma, lt_zthr_bhisthmma[].
  LOOP AT lt_inx_zthr_bhisthmma INTO ls_inx_zthr_bhisthmma.
    MOVE-CORRESPONDING ls_inx_zthr_bhisthmma TO lt_zthr_bhisthmma.
    APPEND lt_zthr_bhisthmma.  CLEAR lt_zthr_bhisthmma.
  ENDLOOP.

  SORT lt_zthr_bhisthmma BY badge.
  DELETE ADJACENT DUPLICATES FROM lt_zthr_bhisthmma COMPARING badge.

  CLEAR: lt_zthr_badge, lt_zthr_badge[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_zthr_badge
    FROM zthr_badge
    FOR ALL ENTRIES IN lt_zthr_bhisthmma
   WHERE badge = lt_zthr_bhisthmma-badge
     AND vorna IN s_vorna
     AND nachn IN s_nachn
     AND deptm IN s_deptm
     AND hmma  = ' '.

  LOOP AT lt_inx_zthr_bhisthmma INTO ls_inx_zthr_bhisthmma.
    CLEAR it_sec_tmp.
    MOVE-CORRESPONDING ls_inx_zthr_bhisthmma TO it_sec_tmp.

    CLEAR lt_zthr_badge.
    READ TABLE lt_zthr_badge WITH KEY badge = ls_inx_zthr_bhisthmma-badge.

    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING lt_zthr_badge TO it_sec_tmp.
    APPEND it_sec_tmp.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_ZTHR_BHISTHMMA_2
