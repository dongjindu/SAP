*----------------------------------------------------------------------
* Program ID        : ZAHRU015
* Title             : Exceptions import to SAP for KRONOS data
* Created on        : 2/7/2011
* Created by        : I.G.MOON / Valerian Utama
* Specifications By : Euna.Lee
* Description       : Exceptions import to SAP on a daily basis
*
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  03/16/2011  Valerian  UD1K951043  Modify Program Logic
*  06/03/2011  Valerian  UD1K951876  Add Receiver Cost Center as a key
*                                    when compare with old records.
*                                    Automate UNIX Path selection
*                                    based on the run SAP system.
*  08/15/2011  Valerian  UD1K952747  Reject posting with hour more than
*                                    24 hours.
*                                    Reject posting with negative hours
*  08/31/2011  Valerian  UD1K952893  Update message only for selected
*                                    records in interactive mode.
*                                    Change the sequence of posting:
*                                    1st Absences then Attendances.
*  09/19/2011  Valerian  UD1K952993  Change back the sequence of pstng:
*                                    1st Attendance then Absence.
*  09/19/2011  Valerian  UD1K952995  Fix ABAP Dump
*  09/23/2011  Valerian  UD1K953127  Refresh data in the pay period.
*                                    for TMs in file and inactive TMs
*                                    Create additional record for wage
*                                    type: 'REG1', 'OT15' and 'OT20'.
*                                    Add selection screen to input
*                                    payroll period.
*  10/14/2011  Valerian  UD1K953159  Delete inactive TM records beyond
*                                    payroll period base on negative
*                                    hours records.
*  10/18/2011  Valerian  UD1K953181  Correct logic to delete inactive
*                                    TM records beyond payroll period
*  10/25/2011  Valerian  UD1K953214  Suppress ALV's Rows Selection
*                                    Change logic: Refresh all active
*                                    TM records within payroll period
*  10/26/2011  Valerian  UD1K953281  Substract hours for negative
*                                    records (instead of delete)
*                                    Change message for negative hours
*                                    Add hours for historical data
*                                    Transfer cost center for attn.
*                                    type: 1055 and 1056
*  11/16/2011  Valerian  UD1K953307  Adjust logic to prevent records
*                                    to be cleared for certain TM/date/
*                                    att.type for historical data
*  12/05/2011  Valerian  UD1K953377  Disable the logic to create attn.
*                                    type: 1055 and 1056
*                                    Include TMs recently terminate in
*                                    the current logic
*  04/03/2012  Valerian  UD1K954411  Enable the logic to create attn.
*                                    type: 1055 and 1056 (Reverse
*                                    of part-1 of CTS UD1K953377)
*  08/13/2012  Valerian  UD1K954795  Change sequence of IT Creation
*                                    First IT2002 Next IT2001
*  09/18/2012  Valerian  UD1K955557  Prevent record to be posted if the
*                                    hours exceed daily working hours
*  09/20/2012  Valerian  UD1K955579  Fix previous bug.
*----------------------------------------------------------------------

REPORT zahru015 MESSAGE-ID zmfi NO STANDARD PAGE HEADING LINE-SIZE 180.

TYPE-POOLS: truxs, slis.
INCLUDE: <icon>, <symbol>.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS:  true  VALUE 'X',
            c_bukrs TYPE bukrs VALUE 'H201',
            c_dfile(30) TYPE c VALUE 'SAP_Exceptions.CSV',
            c_unix_pre(30) TYPE c VALUE '/sapmnt/',         "UD1K951876
            c_unix_suf(30) TYPE c VALUE '/kronos/kronosftp'. "UD1K951876

*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES: pa0000, *pa2001,*pa2002.

*----------------------------------------------------------------------*
* ALV DATA
*----------------------------------------------------------------------*
CONSTANTS: gc_var_save TYPE c VALUE  'A'.

DATA: gs_variant  LIKE disvariant,
      gv_repid    LIKE sy-repid.

* for ALV Grid
DATA: gt_exclude  TYPE ui_functions,
      container   TYPE scrfname VALUE 'G_CUSTOM_CONTAINER',
      gs_fcat     TYPE lvc_s_fcat,
      gt_fcat     TYPE lvc_t_fcat,
      gs_layo     TYPE lvc_s_layo,
      gt_sort     TYPE lvc_t_sort.

DATA: stable      TYPE lvc_s_stbl.

DATA: ok_code     TYPE sy-ucomm.
DATA: w_status(1).
* reference to custom container: neccessary to bind ALV Control
CLASS cl_gui_resources DEFINITION LOAD.

DATA : g_custom_container  TYPE REF TO cl_gui_custom_container,
       g_grid              TYPE REF TO cl_gui_alv_grid.

* define internal table for BDC
DATA: gt_bdc TYPE TABLE OF bdcdata    WITH HEADER LINE,
      gt_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.

DEFINE __set_refresh_mode.
  stable-row = &1.
  stable-col = &1.
END-OF-DEFINITION.

*---------------------------------------------------------------------*
* Data
*---------------------------------------------------------------------*
DATA: rawdata   TYPE truxs_t_text_data,
      w_rawdata TYPE LINE OF truxs_t_text_data,
      izthru015 LIKE zthru015 OCCURS 0 WITH HEADER LINE,
      v_ans     TYPE i,
      i_cnt     TYPE i,
      u_cnt     TYPE i,
      d_cnt     TYPE i,
      n_cnt     TYPE i,
      e_cnt     TYPE i,
      t_cnt     TYPE i.

* internal table for uploaded file
DATA : BEGIN OF gt_out  OCCURS 0,
         f1(20)  TYPE c,     "Employee No.
         f2(20)  TYPE c,     "Date
         f3(20)  TYPE c,     "Hours
         f4(20)  TYPE c,     "A/A Type
         f5(20)  TYPE c,     "Sender CC
         f6(20)  TYPE c,     "Receiver CC
         f7(20)  TYPE c,     "Flag for 2001/2002
         chk(1)  TYPE c,
         msg(99) TYPE c,
         rec     TYPE i,
         flg(1)  TYPE c,
         mode(1) TYPE c,
         icon(4) TYPE c,
       END   OF  gt_out.

* internal table for deleted records.
DATA gt_outdel LIKE gt_out OCCURS 0 WITH HEADER LINE.

DATA $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
              IMPORTING er_data_changed,

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
              IMPORTING e_row
                        e_column
                        es_row_no.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

* Setting for Change data
  METHOD handle_data_changed.
    PERFORM data_changed USING er_data_changed.
  ENDMETHOD.                    " handle_data_changed

* Double Click
  METHOD handle_double_click.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_double_click

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __focus.
  call method cl_gui_control=>set_focus
    exporting
      control = &1.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
    exporting
      titel = &1
      txt1  = &2
      txt2  = sy-subrc.
END-OF-DEFINITION.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
* BEGIN OF UD1K953127
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) text-002.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: p_abkrs TYPE t569v-abkrs OBLIGATORY DEFAULT '11'.
SELECTION-SCREEN POSITION POS_LOW.                          "UD1K953281
SELECT-OPTIONS: s_cdate FOR sy-datum NO-EXTENSION MODIF ID dis.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_cprd RADIOBUTTON GROUP prd DEFAULT 'X'
                                    USER-COMMAND COM1.
SELECTION-SCREEN COMMENT (20) text-004.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: p_cprd TYPE t569v-pabrp MODIF ID dis,
            p_cyr  TYPE t569v-pabrj MODIF ID dis.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_oprd RADIOBUTTON GROUP prd.
SELECTION-SCREEN COMMENT (20) text-007.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: p_oprd TYPE t569v-pabrp,
            p_oyr  TYPE t569v-pabrj.
SELECTION-SCREEN END OF LINE.
* BEGIN OF UD1K953281
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_fprd RADIOBUTTON GROUP prd.
SELECTION-SCREEN COMMENT (20) text-008.
SELECTION-SCREEN POSITION POS_LOW.
SELECT-OPTIONS: s_fdate FOR sy-datum NO-EXTENSION.
SELECTION-SCREEN END OF LINE.
* END OF UD1K953281
** Furong on 06/25/14 (
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_hpvc RADIOBUTTON GROUP prd.
SELECTION-SCREEN COMMENT (25) text-012.
*SELECTION-SCREEN POSITION POS_LOW.
*SELECT-OPTIONS: s_hpvcur FOR sy-datum NO-EXTENSION.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_hpvp RADIOBUTTON GROUP prd.
SELECTION-SCREEN COMMENT (25) text-013.
*SELECTION-SCREEN POSITION POS_LOW.
*SELECT-OPTIONS: s_hpvpre FOR sy-datum NO-EXTENSION.
SELECTION-SCREEN END OF LINE.
** )
SELECTION-SCREEN END   OF BLOCK b2.
* END OF UD1K953127

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-005.
** Furong on 06/17/14 (
PARAMETERS: p_batch AS CHECKBOX  USER-COMMAND chal.
SELECTION-SCREEN SKIP.
** )
PARAMETERS: p_locl RADIOBUTTON GROUP serv USER-COMMAND rad DEFAULT 'X',
            p_serv RADIOBUTTON GROUP serv.

SELECTION-SCREEN SKIP.

PARAMETERS: p_file  LIKE rlgrap-filename.

SELECTION-SCREEN END   OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-006.
PARAMETERS: p_post AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK b5.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_day(3) TYPE n DEFAULT 90.
SELECTION-SCREEN END   OF BLOCK b1.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari NO-DISPLAY.
PARAMETERS p_mode DEFAULT 'N' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b4.

INITIALIZATION.                                             "UD1K953127
*  PERFORM get_pay_period.                                   "UD1K953127
  PERFORM init_data.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  IF NOT p_locl IS INITIAL.
    PERFORM browser CHANGING p_file v_ans.
  ELSE.
    PERFORM display_unix CHANGING p_file v_ans.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_DAY'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
* BEGIN OF UD1K953127
    IF screen-group1 = 'DIS'.
      IF screen-name CP '*S_CDATE*'.
        IF s_cdate IS INITIAL.
          screen-active = 0.
        ELSE.
          screen-input = 0.
        ENDIF.
      ELSE.
        IF p_cprd IS INITIAL.
          screen-active = 0.
        ELSE.
          screen-input = 0.
        ENDIF.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
* END OF UD1K953127
* Furong on 06/17/14 (
    IF p_batch IS NOT INITIAL.
      IF screen-name = 'P_LOCL'.
        screen-active = 0.
        p_serv = 'X'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
** )
    IF r_hpvc = 'X' OR r_hpvp = 'X'.
*      IF screen-name = 'R_FPRD' OR
      IF  screen-name = 'S_FDATE-LOW' OR
          screen-name = 'S_FDATE-HIGH' OR
*         screen-name = 'R_CPRD' OR
          screen-name = 'P_CPRD' OR
          screen-name = 'P_CYR' OR
*         screen-name = 'R_OPRD' OR
          screen-name = 'P_OPRD' OR
          screen-name = 'P_OYR'.
        CLEAR: p_cprd, p_cyr, p_oprd.
        screen-input = 0.
        screen-active = 0.
*        IF screen-name = 'R_FPRD' OR
*           screen-name = 'R_CPRD' OR
*           screen-name = 'R_OPRD'.
*           screen-active = 1.
*        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    CASE 'X'.
      WHEN r_fprd.
        IF screen-name = 'S_FDATE-LOW' OR
         screen-name = 'S_FDATE-HIGH'.
          screen-input = 1.
          screen-active = 1.
          MODIFY SCREEN.
        ELSE.
          IF screen-name = 'P_CPRD' OR
         screen-name = 'P_CYR' OR
            screen-name = 'P_OPRD' OR
        screen-name = 'P_OYR'.
            screen-input = 0.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
      WHEN r_cprd.
        IF screen-name = 'P_CPRD' OR
          screen-name = 'P_CYR'.
          screen-input = 1.
          screen-active = 1.
          MODIFY SCREEN.
        ELSE.
          IF screen-name = 'S_FDATE-LOW' OR
         screen-name = 'S_FDATE-HIGH' OR
         screen-name = 'P_OPRD' OR
         screen-name = 'P_OYR'.
            screen-input = 0.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
      WHEN r_oprd .
        IF screen-name = 'P_OPRD' OR
         screen-name = 'P_OYR'.
          screen-input = 1.
          screen-active = 1.
          MODIFY SCREEN.
        ELSE.
          IF screen-name = 'S_FDATE-LOW' OR
         screen-name = 'S_FDATE-HIGH' OR
            screen-name = 'P_CPRD' OR
          screen-name = 'P_CYR'.
            screen-input = 0.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDLOOP.

AT SELECTION-SCREEN.
  IF p_file IS INITIAL.
    MESSAGE e000 WITH 'Please Enter Filename'.
  ENDIF.
  PERFORM get_pay_period.                                   "UD1K953127

TOP-OF-PAGE.
  WRITE: / text-h01, text-h02, text-h03, text-h04,
           text-h05, text-h06, text-h07, text-h08.
  ULINE.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF r_hpvc IS NOT INITIAL OR r_hpvp IS NOT INITIAL.
    PERFORM check_payroll_status.
    IF w_status = 'X'.
      EXIT.
    ENDIF.
  ENDIF.
  PERFORM maintain_log.
  PERFORM upload_data.
  PERFORM process_data.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR '100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'POST'.
      PERFORM get_$gt_out.
      PERFORM save_data.
      PERFORM refresh_alv.
      __focus g_grid.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
FORM create_field_category.
  DATA: l_pos       TYPE i.
  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fcat.
    gs_fcat-col_pos       = l_pos.
    gs_fcat-key           = &1.
    gs_fcat-fieldname     = &2.
    gs_fcat-coltext       = &3.     " Column heading
    gs_fcat-outputlen     = &4.     " Column width
    gs_fcat-datatype      = &5.     " Data type
    gs_fcat-emphasize     = &6.
    append gs_fcat to gt_fcat.
  END-OF-DEFINITION.

  __catalog :
          'X'  'F1'   'TM#'            20 'CHAR' '',
          ' '  'F2'   'Date'           20 'CHAR' '',
          ' '  'F3'   'Hours'          20 'CHAR' '',
          ' '  'F4'   'A/A Type'       20 'CHAR' '',
          ' '  'F5'   'Sender CC'      20 'CHAR' '',
          ' '  'F6'   'Receiver CC'    20 'CHAR' '',
          ' '  'F7'   'Flag'           20 'CHAR' '',
          ' '  'ICON' 'Status'         04 'CHAR' '',
          ' '  'MSG'  'Message'        50 'CHAR' ''.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       Event of changed data
*----------------------------------------------------------------------*
*      -->RR_DATA_CHANGED  Log is Visible
*----------------------------------------------------------------------*
FORM data_changed USING rr_data_changed
                        TYPE REF TO cl_alv_changed_data_protocol.

  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = stable.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       Save data to table ZTCOU105
*----------------------------------------------------------------------*
FORM save_data.
  DATA: begda(10),
        $stdaz(10).

  __cls  izthru015.

  DATA : BEGIN OF ipa2001 OCCURS 0.
          INCLUDE STRUCTURE pa2001.
  DATA :  kostl TYPE kostl,
          rec TYPE i,
          mode(1) TYPE c,
         END   OF  ipa2001.

  DATA : BEGIN OF ipa2002 OCCURS 0.
          INCLUDE STRUCTURE pa2002.
  DATA :  kostl TYPE kostl,
          rec TYPE i,
          mode(1) TYPE c,
         END   OF  ipa2002.

  DATA: BEGIN OF t_pa2002_old OCCURS 0,
          pernr TYPE pa2002-pernr,
          begda TYPE pa2002-begda,
          awart TYPE pa2002-awart,
          kostl TYPE kostl,
          seqnr TYPE pa2002-seqnr,
          stdaz TYPE pa2002-stdaz,
         END OF t_pa2002_old.

  DATA: t_pa2002_new LIKE t_pa2002_old OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF t_pa2002_chg OCCURS 0.
          INCLUDE STRUCTURE t_pa2002_old.
  DATA: mode(1) TYPE c,
        END OF t_pa2002_chg.

  DATA: BEGIN OF t_asshr OCCURS 0,
          pernr TYPE asshr-pernr,
          begda TYPE asshr-begda,
          subty TYPE asshr-subty,
          seqnr TYPE asshr-seqnr,
          kostl TYPE assob-kostl,
        END OF t_asshr.

  DATA: BEGIN OF t_pa2001_old OCCURS 0,
          pernr TYPE pa2002-pernr,
          begda TYPE pa2002-begda,
          awart TYPE pa2002-awart,
          seqnr TYPE pa2002-seqnr,
          stdaz TYPE pa2002-stdaz,
        END OF t_pa2001_old.

  DATA: t_pa2001_new LIKE t_pa2001_old OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF t_pa2001_chg OCCURS 0.
          INCLUDE STRUCTURE t_pa2001_old.
  DATA: mode(1) TYPE c,
        END OF t_pa2001_chg.

  DATA: BEGIN OF t_pa0000 OCCURS 0,
          pernr TYPE pa0000-pernr,
*         stat2 TYPE pa0000-stat2,                          "UD1K953181
        END OF t_pa0000.

* BEGIN OF UD1K955557
  DATA: BEGIN OF t_pa0007 OCCURS 0,
          pernr TYPE pa0007-pernr,
          endda TYPE pa0007-endda,                          "UD1K955579
          begda TYPE pa0007-begda,                          "UD1K955579
          arbst TYPE pa0007-arbst,
        END OF t_pa0007.
* END OF UD1K955557

  DATA: BEGIN OF t_t554s OCCURS 0,
          subty TYPE t554s-subty,
          dynnr TYPE t554s-dynnr,
        END OF t_t554s.

  DATA: l_tabix TYPE sy-tabix,
        l_mode(4) TYPE c,
        l_subrc TYPE sy-subrc.

* BEGIN OF UD1K953127
* BEGIN OF UD1K953214
*  DATA: BEGIN OF t_emp_prd OCCURS 0,
*          pernr TYPE pa2002-pernr,
*        END OF t_emp_prd.
* END OF UD1K953214

  DATA: t_pa2002_add LIKE t_pa2002_old OCCURS 0 WITH HEADER LINE.

  DATA: l_begpp TYPE t549q-begda,
        l_endpp TYPE t549q-endda.

* Get pay period
  l_begpp = s_cdate-low.
  l_endpp = s_cdate-high.
* END OF UD1K953127

  __cls : ipa2001,ipa2002.

* SELECT DISTINCT pernr stat2 INTO TABLE t_pa0000           "UD1K953181
  SELECT DISTINCT pernr INTO TABLE t_pa0000                 "UD1K953181
    FROM pa0000
   WHERE endda = '99991231'.
  SORT t_pa0000 BY pernr.

* BEGIN OF UD1K955579
* BEGIN OF UD1K955557
*  SELECT pernr arbst INTO TABLE t_pa0007
*    FROM pa0007
*    FOR ALL ENTRIES IN t_pa0000
*   WHERE pernr = t_pa0000-pernr
*     AND endda = '99991231'.
*  SORT t_pa0007 BY pernr.
* END OF UD1K955557
* END OF UD1K955579

  SELECT DISTINCT subty dynnr INTO TABLE t_t554s
    FROM t554s
   WHERE endda = '99991231'.
  SORT t_t554s BY subty.

  LOOP AT $gt_out.
    READ TABLE t_pa0000 WITH KEY pernr = $gt_out-f1
                             BINARY SEARCH.

    IF sy-subrc <> 0.
      gt_out-msg = 'Invalid TM#'.
      gt_out-flg = 'E'.
      MODIFY gt_out INDEX $gt_out-rec TRANSPORTING msg flg.
      PERFORM append_log.                                   "UD1K952747
      CONTINUE.
    ENDIF.

* BEGIN OF UD1K952747
    IF $gt_out-f3 GT 24.
      gt_out-msg = 'Posting greater than 24 hours not allowed'.
      gt_out-flg = 'E'.
      MODIFY gt_out INDEX $gt_out-rec TRANSPORTING msg flg.
      PERFORM append_log.
      CONTINUE.
    ENDIF.

    IF $gt_out-f3 LT 0.
      gt_out-msg = 'Negative retro amount'.                 "UD1K953281
      gt_out-flg = 'W'.                                     "UD1K953281

*     gt_out-msg = 'Posting negative hours not allowed'.
*     gt_out-flg = 'E'.                                     "UD1K953127
*     gt_out-flg = 'S'.                                     "UD1K953127
      MODIFY gt_out INDEX $gt_out-rec TRANSPORTING msg flg.
      PERFORM append_log.
* BEGIN OF UD1K953181
*     IF gt_out-f2 BETWEEN l_begpp AND l_endpp OR           "UD1K953159
*        t_pa0000-stat2 <> 1.                               "UD1K953159
      IF gt_out-f2 BETWEEN l_begpp AND l_endpp.             "UD1K953159
* END OF UD1K953181
        CONTINUE.
      ENDIF.                                                "UD1K953159
    ENDIF.
* END OF UD1K952747

    CLEAR : *pa2001,*pa2002.

    IF $gt_out-f7 EQ 'P2002'.

       *pa2002-pernr = $gt_out-f1.
       *pa2002-begda = $gt_out-f2.
       *pa2002-endda = $gt_out-f2.
       *pa2002-subty = $gt_out-f4.
       *pa2002-stdaz = $gt_out-f3.
       *pa2002-awart = *pa2002-subty.

      MOVE-CORRESPONDING *pa2002 TO ipa2002.

      IF $gt_out-f5 <> $gt_out-f6.
        ipa2002-kostl = $gt_out-f6.
        SHIFT ipa2002-kostl LEFT DELETING LEADING '0'.
      ENDIF.
      ipa2002-rec = $gt_out-rec.
      APPEND ipa2002.
      CLEAR ipa2002.

    ELSEIF $gt_out-f7 EQ 'P2001'.

       *pa2001-pernr = $gt_out-f1.
       *pa2001-begda = $gt_out-f2.
       *pa2001-endda = $gt_out-f2.
       *pa2001-subty = $gt_out-f4.
       *pa2001-stdaz = $gt_out-f3.
       *pa2001-awart = *pa2001-subty.

      MOVE-CORRESPONDING *pa2001 TO ipa2001.

* BEGIN OF UD1K955579
* BEGIN OF UD1K955557
*      READ TABLE t_pa0007 WITH KEY pernr = ipa2001-pernr
*                               BINARY SEARCH.
*
*      IF sy-subrc = 0 AND ipa2001-stdaz > t_pa0007-arbst.
*        gt_out-msg = 'The entry exceed daily working hours'.
*        gt_out-flg = 'E'.
*        MODIFY gt_out INDEX $gt_out-rec TRANSPORTING msg flg.
*        PERFORM append_log.
*        CONTINUE.
*      ENDIF.
* END OF UD1K955557
* END OF UD1K955579

      IF $gt_out-f5 <> $gt_out-f6.
        ipa2001-kostl = $gt_out-f6.
        SHIFT ipa2001-kostl LEFT DELETING LEADING '0'.
      ENDIF.
      ipa2001-rec = $gt_out-rec.
      APPEND ipa2001.
      CLEAR ipa2001.

    ENDIF.
  ENDLOOP.
  FREE: $gt_out, t_pa0000.

* BEGIN OF UD1K955579
  SELECT pernr endda begda arbst INTO TABLE t_pa0007
    FROM pa0007
    FOR ALL ENTRIES IN ipa2001
   WHERE pernr  = ipa2001-pernr
     AND endda >= ipa2001-begda
     AND begda <= ipa2001-begda.
  SORT t_pa0007 BY pernr endda DESCENDING.

  LOOP AT ipa2001.
    l_tabix = sy-tabix.
    LOOP AT t_pa0007 WHERE pernr  = ipa2001-pernr
                       AND endda >= ipa2001-begda
                       AND begda <= ipa2001-begda.
      IF ipa2001-stdaz > t_pa0007-arbst AND
        t_pa0007-arbst > 0.
        $gt_out-rec = ipa2001-rec.
        gt_out-msg = 'The entry exceed daily working hours'.
        gt_out-flg = 'E'.
        MODIFY gt_out INDEX $gt_out-rec TRANSPORTING msg flg.
        PERFORM append_log.
        DELETE ipa2001 INDEX l_tabix.
        EXIT.
      ELSEIF t_pa0007-arbst = 0.
        $gt_out-rec = ipa2001-rec.
        gt_out-msg = 'Attendance/absence during non-working period'.
        gt_out-flg = 'E'.
        MODIFY gt_out INDEX $gt_out-rec TRANSPORTING msg flg.
        PERFORM append_log.
        DELETE ipa2001 INDEX l_tabix.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
* END OF UD1K955579

* Compare PA2001
  IF NOT ipa2001[] IS INITIAL.
    SELECT pernr begda awart seqnr stdaz                    "UD1K953127
           INTO CORRESPONDING FIELDS OF TABLE t_pa2001_old
      FROM pa2001
      FOR ALL ENTRIES IN ipa2001
     WHERE pernr = ipa2001-pernr
       AND endda = ipa2001-endda
       AND begda = ipa2001-begda
       AND ( endda < l_begpp OR endda > l_endpp )           "UD1K953127
       AND ( begda < l_begpp OR begda > l_endpp ).          "UD1K953127

* BEGIN OF UD1K953281
** Remove negative hours to trigger deletion
*   DELETE ipa2001 WHERE stdaz < 0.                         "UD1K953159
* END OF UD1K953281
  ENDIF.                                                    "UD1K953214

* BEGIN OF UD1K953127
* BEGIN OF UD1K953214
** Get all TMs in the file for the payperiod
*    CLEAR: t_emp_prd, t_emp_prd[].
*    LOOP AT ipa2001 WHERE endda BETWEEN l_begpp AND l_endpp
*                      AND begda BETWEEN l_begpp AND l_endpp.
*      t_emp_prd-pernr = ipa2001-pernr.
*      APPEND t_emp_prd.
*    ENDLOOP.
*
*    SORT t_emp_prd.
*    DELETE ADJACENT DUPLICATES FROM t_emp_prd.
*
*    IF NOT t_emp_prd[] IS INITIAL.
*      SELECT pernr begda awart seqnr stdaz
*             APPENDING CORRESPONDING FIELDS OF TABLE t_pa2001_old
*        FROM pa2001
*        FOR ALL ENTRIES IN t_emp_prd
*       WHERE pernr = t_emp_prd-pernr
*         AND endda BETWEEN l_begpp AND l_endpp
*         AND begda BETWEEN l_begpp AND l_endpp.
*
*      FREE t_emp_prd.
*    ENDIF.
* END OF UD1K953214

* Get all TMs which is active & inactive in payperiod (for deletion)
  SELECT a~pernr a~begda a~awart a~seqnr a~stdaz
         APPENDING CORRESPONDING FIELDS OF TABLE t_pa2001_old
    FROM pa2001 AS a JOIN pa0000 AS b
         ON a~pernr = b~pernr
   WHERE a~endda BETWEEN l_begpp AND l_endpp
     AND a~begda BETWEEN l_begpp AND l_endpp
     AND b~endda = '99991231'
     AND ( b~stat2 = '0' OR b~stat2 = '1' OR                "UD1K953377
           b~stat2 = '3' ).                                 "UD1K953377
*    AND ( b~stat2 = '1' OR b~stat2 = '3' ).                "UD1K953214
*    AND b~stat2 = '1'.                                     "UD1K953214
* END OF UD1K953127

  SORT t_pa2001_old BY pernr begda awart seqnr.
  DELETE ADJACENT DUPLICATES FROM t_pa2001_old
         COMPARING pernr begda awart.

  CLEAR: t_pa2001_old-seqnr, t_pa2001_old-pernr.
  MODIFY t_pa2001_old TRANSPORTING seqnr
                             WHERE pernr <> t_pa2001_old-pernr.

  LOOP AT ipa2001.
    MOVE-CORRESPONDING ipa2001 TO t_pa2001_new.
    APPEND t_pa2001_new.
  ENDLOOP.

  SORT: t_pa2001_new BY pernr begda awart,
        t_pa2001_old BY pernr begda awart.

  LOOP AT t_pa2001_new.
    CLEAR t_pa2001_chg.                                     "UD1K953281
    READ TABLE t_pa2001_old WITH KEY pernr = t_pa2001_new-pernr
                                     begda = t_pa2001_new-begda
                                     awart = t_pa2001_new-awart
                                     BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE t_pa2001_old INDEX sy-tabix.
      MOVE-CORRESPONDING t_pa2001_new TO t_pa2001_chg.      "UD1K953281
      t_pa2001_chg-mode = '2'.                              "UD1K953281
      IF t_pa2001_new-begda BETWEEN l_begpp AND l_endpp.    "UD1K953281

        IF t_pa2001_new <> t_pa2001_old.
*       MOVE-CORRESPONDING t_pa2001_new TO t_pa2001_chg.    "UD1K953281
*       t_pa2001_chg-mode = '2'.                            "UD1K953281
          APPEND t_pa2001_chg.
        ENDIF.
* BEGIN OF UD1K953281
      ELSE.
        t_pa2001_chg-stdaz = t_pa2001_chg-stdaz +
                             t_pa2001_old-stdaz.
        IF t_pa2001_chg-stdaz <= 0.
          CLEAR ipa2001.
          MOVE-CORRESPONDING t_pa2001_old TO ipa2001.
          ipa2001-subty = ipa2001-awart.
          ipa2001-mode = '1'.
          APPEND ipa2001.
          CONTINUE.
        ENDIF.
        APPEND t_pa2001_chg.
      ENDIF.
* END OF UD1K953281

    ELSE.
      MOVE-CORRESPONDING t_pa2001_new TO t_pa2001_chg.
      t_pa2001_chg-mode = '3'.
      APPEND t_pa2001_chg.
    ENDIF.
*   CLEAR t_pa2001_chg.                                     "UD1K953281
  ENDLOOP.
  FREE t_pa2001_new.

* Only refresh TM which is within the payroll period.
* LOOP AT t_pa2001_old.                                     "UD1K953307
  LOOP AT t_pa2001_old WHERE begda                          "UD1K953307
                     BETWEEN l_begpp AND l_endpp.           "UD1K953307
    CLEAR ipa2001.
    MOVE-CORRESPONDING t_pa2001_old TO ipa2001.
    ipa2001-subty = ipa2001-awart.
    ipa2001-mode = '1'.
    APPEND ipa2001.
  ENDLOOP.
  FREE t_pa2001_old.
* ENDIF.                                                    "UD1K953214

  SORT t_pa2001_chg BY pernr begda awart.
  LOOP AT ipa2001 WHERE mode = ' '.
    l_tabix = sy-tabix.
    READ TABLE t_pa2001_chg WITH KEY pernr = ipa2001-pernr
                                     begda = ipa2001-begda
                                     awart = ipa2001-awart
                                     BINARY SEARCH.
    IF sy-subrc = 0.
      ipa2001-mode = t_pa2001_chg-mode.
      ipa2001-stdaz = t_pa2001_chg-stdaz.                   "UD1K953281
      MODIFY ipa2001 INDEX l_tabix TRANSPORTING mode stdaz. "UD1K953281
*     MODIFY ipa2001 INDEX l_tabix TRANSPORTING mode.       "UD1K953281
    ENDIF.
  ENDLOOP.
  FREE t_pa2001_chg.

* BEGIN OF UD1K953281
* Filter creation record with negative hours
  LOOP AT ipa2001 WHERE mode = '3'
                    AND stdaz <= 0.
    DELETE ipa2001.
  ENDLOOP.
* END OF UD1K953281

* Post info type 2001
  SORT ipa2001 BY pernr begda mode.
  LOOP AT ipa2001 WHERE mode <> space.

    CLEAR t_t554s.
    READ TABLE t_t554s WITH KEY subty = ipa2001-subty
                                BINARY SEARCH.

    WRITE ipa2001-begda TO begda.
    WRITE ipa2001-stdaz TO $stdaz.
    CONDENSE $stdaz NO-GAPS.

    IF ipa2001-mode = '3'.
      l_mode = '=INS'.
    ELSEIF ipa2001-mode = '2'.
      l_mode = '=MOD'.
    ELSEIF ipa2001-mode = '1'.
      l_mode = '=DEL'.
    ENDIF.

    PERFORM dynpro USING:
        'X' 'SAPMP50A'        '1000',
        ' ' 'BDC_OKCODE'      l_mode,
        ' ' 'RP50G-PERNR'     ipa2001-pernr,
        ' ' 'BDC_SUBSCR'      'SAPMP50A',
        ' ' 'RP50G-TIMR6'     'X',
        ' ' 'RP50G-BEGDA'     begda,
        ' ' 'RP50G-ENDDA'     begda,
        ' ' 'RP50G-CHOIC'     '2001',
        ' ' 'RP50G-SUBTY'     ipa2001-subty.

    IF ipa2001-mode = '1'.
      PERFORM dynpro USING:
          'X' 'MP200000'        t_t554s-dynnr,
          ' ' 'BDC_OKCODE'      '=UPDL',
          ' ' 'P2001-BEGDA'     begda,
          ' ' 'P2001-ENDDA'     begda.
    ELSE.
      PERFORM dynpro USING:
          'X' 'MP200000'        t_t554s-dynnr,
          ' ' 'BDC_OKCODE'      '=UPD',
          ' ' 'P2001-BEGDA'     begda,
          ' ' 'P2001-ENDDA'     begda,
          ' ' 'P2001-STDAZ'     $stdaz.

    ENDIF.

    __cls gt_msg.

    CALL TRANSACTION 'PA30'   USING         gt_bdc
                              MODE p_mode
                              MESSAGES INTO gt_msg.
    l_subrc = sy-subrc.
    __cls gt_bdc.

    CLEAR gt_out.

* Get meaningful latest message
    DESCRIBE TABLE gt_msg LINES l_tabix.
    DO.
      READ TABLE gt_msg INDEX l_tabix.
      IF sy-subrc = 0.
        gt_out-flg = gt_msg-msgtyp.
        MESSAGE ID gt_msg-msgid TYPE gt_msg-msgtyp NUMBER gt_msg-msgnr
           WITH gt_msg-msgv1 gt_msg-msgv2 gt_msg-msgv3 gt_msg-msgv4
           INTO gt_out-msg.

        IF gt_msg-msgid = '00' AND gt_msg-msgnr = '344'.
          l_tabix = l_tabix - 1.
        ELSE.
          EXIT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    IF l_subrc = 0.
      IF gt_out-flg <> 'S'.
        gt_out-flg = 'S'.
        READ TABLE gt_msg WITH KEY msgtyp = 'S'.
        IF sy-subrc = 0.
          MESSAGE ID gt_msg-msgid TYPE gt_msg-msgtyp
           NUMBER gt_msg-msgnr
             WITH gt_msg-msgv1 gt_msg-msgv2 gt_msg-msgv3 gt_msg-msgv4
             INTO gt_out-msg.
        ENDIF.
      ENDIF.
    ELSE.
      IF gt_out-flg <> 'E'.
        gt_out-flg = 'E'.
        READ TABLE gt_msg WITH KEY msgtyp = 'E'.
        IF sy-subrc = 0.
          MESSAGE ID gt_msg-msgid TYPE gt_msg-msgtyp
           NUMBER gt_msg-msgnr
             WITH gt_msg-msgv1 gt_msg-msgv2 gt_msg-msgv3 gt_msg-msgv4
             INTO gt_out-msg.
        ENDIF.
      ENDIF.
    ENDIF.

    gt_out-mode = ipa2001-mode.

* If ipa2001-rec initial, it is deleted/new record, append record
    IF NOT ipa2001-rec IS INITIAL.
* BEGIN OF UD1K953127 - Update error message to corresponding record
*     MODIFY gt_out INDEX ipa2001-rec TRANSPORTING msg flg mode.
*     READ TABLE gt_out INDEX ipa2001-rec.
      READ TABLE gt_out WITH KEY rec = ipa2001-rec
                        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        MODIFY gt_out INDEX sy-tabix TRANSPORTING msg flg mode.
        READ TABLE gt_out INDEX sy-tabix.
      ENDIF.
* END OF UD1K953127
    ELSE.
      gt_out-f1 = ipa2001-pernr.
      gt_out-f2 = ipa2001-begda.
      gt_out-f3 = ipa2001-stdaz.
      gt_out-f4 = ipa2001-awart.
      gt_out-f6 = ipa2001-kostl.
      gt_out-f7 = 'P2001'.
      SHIFT gt_out-f3 LEFT DELETING LEADING space.
      SHIFT gt_out-f1 LEFT DELETING LEADING '0'.            "UD1K953127
      APPEND gt_out TO gt_outdel.
    ENDIF.

    izthru015-pernr  = ipa2001-pernr.
    izthru015-begda  = ipa2001-begda.
    izthru015-stdaz  = ipa2001-stdaz.
    izthru015-awart  = ipa2001-awart.
    izthru015-skostl = gt_out-f5.
    izthru015-rkostl = gt_out-f6.
    izthru015-itflag = gt_out-f7.
    izthru015-flag   = gt_out-flg.
    izthru015-msg    = gt_out-msg.
    izthru015-zuser  = sy-uname.
    izthru015-zsdat  = sy-datum.
    izthru015-zstim  = sy-uzeit.
    APPEND izthru015.
  ENDLOOP.
  FREE: ipa2001, t_t554s.


* Compare PA2002
  IF NOT ipa2002[] IS INITIAL.
    SELECT pernr begda awart seqnr stdaz                    "UD1K953127
           INTO CORRESPONDING FIELDS OF TABLE t_pa2002_old
      FROM pa2002
      FOR ALL ENTRIES IN ipa2002
     WHERE pernr = ipa2002-pernr
       AND endda = ipa2002-endda
       AND begda = ipa2002-begda
       AND ( endda < l_begpp OR endda > l_endpp )           "UD1K953127
       AND ( begda < l_begpp OR begda > l_endpp ).          "UD1K953127

* BEGIN OF UD1K953281
** Remove negative hours to trigger deletion
*    DELETE ipa2002 WHERE stdaz < 0.                         "UD1K953159
* END OF UD1K953281
  ENDIF.                                                    "UD1K953214

* BEGIN OF UD1K953127
* BEGIN OF UD1K953214
** Get all TMs in the file for the payperiod
*    CLEAR: t_emp_prd, t_emp_prd[].
*    LOOP AT ipa2002 WHERE endda BETWEEN l_begpp AND l_endpp
*                      AND begda BETWEEN l_begpp AND l_endpp.
*      t_emp_prd-pernr = ipa2002-pernr.
*      APPEND t_emp_prd.
*    ENDLOOP.
*
*    SORT t_emp_prd.
*    DELETE ADJACENT DUPLICATES FROM t_emp_prd.
*
*    IF NOT t_emp_prd[] IS INITIAL.
*      SELECT pernr begda awart seqnr stdaz
*             APPENDING CORRESPONDING FIELDS OF TABLE t_pa2002_old
*        FROM pa2002
*        FOR ALL ENTRIES IN t_emp_prd
*       WHERE pernr = t_emp_prd-pernr
*         AND endda BETWEEN l_begpp AND l_endpp
*         AND begda BETWEEN l_begpp AND l_endpp.
*
*      FREE t_emp_prd.
*    ENDIF.
* END OF UD1K953214

* Get all TMs which is active & inactive in payperiod (for deletion)
  SELECT a~pernr a~begda a~awart a~seqnr a~stdaz
         APPENDING CORRESPONDING FIELDS OF TABLE t_pa2002_old
    FROM pa2002 AS a JOIN pa0000 AS b
         ON a~pernr = b~pernr
   WHERE a~endda BETWEEN l_begpp AND l_endpp
     AND a~begda BETWEEN l_begpp AND l_endpp
     AND b~endda = '99991231'
     AND ( b~stat2 = '0' OR b~stat2 = '1' OR                "UD1K953377
           b~stat2 = '3' ).                                 "UD1K953377
*    AND ( b~stat2 = '1' OR b~stat2 = '3' ).                "UD1K953214
*    AND b~stat2 = '1'.                                     "UD1K953214

* BEGIN OF UD1K954411 - Reverse CTS UD1K953377
* BEGIN OF UD1K953377
* Insert additional attendace types
  LOOP AT ipa2002 WHERE kostl <> space
                    AND endda BETWEEN l_begpp AND l_endpp
                    AND begda BETWEEN l_begpp AND l_endpp
                    AND ( subty = 'REG1' OR subty = 'OT15' OR
                          subty = 'OT20' ).

    MOVE-CORRESPONDING ipa2002 TO t_pa2002_add.
*   CLEAR t_pa2002_add-kostl.                               "UD1K953281

    CASE ipa2002-subty.
      WHEN 'REG1'.
        t_pa2002_add-awart = '1055'.

      WHEN 'OT15' OR 'OT20'.
        t_pa2002_add-awart = '1056'.
    ENDCASE.

    APPEND t_pa2002_add.
  ENDLOOP.
* END OF UD1K953377
* END OF UD1K954411

  LOOP AT t_pa2002_add.
    CLEAR ipa2002.
    MOVE-CORRESPONDING t_pa2002_add TO ipa2002.
    ipa2002-endda = t_pa2002_add-begda.
    ipa2002-subty = ipa2002-awart.
    APPEND ipa2002.
  ENDLOOP.
  FREE t_pa2002_add.
* END OF UD1K953127

  SORT t_pa2002_old BY pernr begda awart seqnr.
  DELETE ADJACENT DUPLICATES FROM t_pa2002_old
         COMPARING pernr begda awart seqnr.

  IF NOT t_pa2002_old[] IS INITIAL.
    SELECT a~pernr a~begda a~subty a~seqnr b~kostl
      INTO CORRESPONDING FIELDS OF TABLE t_asshr
      FROM asshr AS a JOIN assob AS b
                      ON a~pdsnr = b~pdsnr
           FOR ALL ENTRIES IN t_pa2002_old
     WHERE  a~pernr = t_pa2002_old-pernr
       AND  a~infty = '2002'
       AND  a~subty = t_pa2002_old-awart
       AND  a~endda = t_pa2002_old-begda
       AND  a~begda = t_pa2002_old-begda
       AND  a~seqnr = t_pa2002_old-seqnr.
  ENDIF.

  SORT t_asshr BY pernr begda subty seqnr.
  LOOP AT t_pa2002_old.
    l_tabix = sy-tabix.
    READ TABLE t_asshr WITH KEY pernr = t_pa2002_old-pernr
                                begda = t_pa2002_old-begda
                                subty = t_pa2002_old-awart
                                seqnr = t_pa2002_old-seqnr
                                BINARY SEARCH.
    IF sy-subrc = 0.
      t_pa2002_old-kostl = t_asshr-kostl.
      SHIFT t_pa2002_old-kostl LEFT DELETING LEADING '0'.
    ENDIF.

    CLEAR t_pa2002_old-seqnr.
    MODIFY t_pa2002_old INDEX l_tabix TRANSPORTING seqnr kostl.
  ENDLOOP.
  FREE t_asshr.

  LOOP AT ipa2002.
    MOVE-CORRESPONDING ipa2002 TO t_pa2002_new.
    APPEND t_pa2002_new.
  ENDLOOP.

  SORT: t_pa2002_new BY pernr begda awart kostl,
        t_pa2002_old BY pernr begda awart kostl.

* Give Status '1'= delete '2'= update '3'= insert
  LOOP AT t_pa2002_new.
    CLEAR t_pa2002_chg.                                     "UD1K953281
    READ TABLE t_pa2002_old WITH KEY pernr = t_pa2002_new-pernr
                                     begda = t_pa2002_new-begda
                                     awart = t_pa2002_new-awart
                                     kostl = t_pa2002_new-kostl
                                     BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE t_pa2002_old INDEX sy-tabix.
      MOVE-CORRESPONDING t_pa2002_new TO t_pa2002_chg.      "UD1K953281
      t_pa2002_chg-mode = '2'.                              "UD1K953281
      IF t_pa2002_new-begda BETWEEN l_begpp AND l_endpp.    "UD1K953281

        IF t_pa2002_new <> t_pa2002_old.
*         MOVE-CORRESPONDING t_pa2002_new TO t_pa2002_chg.  "UD1K953281
*         t_pa2002_chg-mode = '2'.                          "UD1K953281
          APPEND t_pa2002_chg.
        ENDIF.
* BEGIN OF UD1K953281
      ELSE.
        t_pa2002_chg-stdaz = t_pa2002_chg-stdaz +
                             t_pa2002_old-stdaz.
        IF t_pa2002_chg-stdaz <= 0.
          CLEAR ipa2002.
          MOVE-CORRESPONDING t_pa2002_old TO ipa2002.
          ipa2002-subty = ipa2002-awart.
          ipa2002-mode = '1'.
          APPEND ipa2002.
          CONTINUE.
        ENDIF.
        APPEND t_pa2002_chg.
      ENDIF.
* END OF UD1K953281

    ELSE.
      MOVE-CORRESPONDING t_pa2002_new TO t_pa2002_chg.
      t_pa2002_chg-mode = '3'.
      APPEND t_pa2002_chg.
    ENDIF.
*   CLEAR t_pa2002_chg.                                     "UD1K953281
  ENDLOOP.
  FREE t_pa2002_new.

* Only refresh TM which is within the payroll period.
* LOOP AT t_pa2002_old.                                     "UD1K953307
  LOOP AT t_pa2002_old WHERE begda                          "UD1K953307
                     BETWEEN l_begpp AND l_endpp.           "UD1K953307
    CLEAR ipa2002.
    MOVE-CORRESPONDING t_pa2002_old TO ipa2002.
    ipa2002-subty = ipa2002-awart.
    ipa2002-mode = '1'.
    APPEND ipa2002.
  ENDLOOP.
  FREE t_pa2002_old.
* ENDIF.                                                    "UD1K953214

  SORT t_pa2002_chg BY pernr begda awart kostl.
  LOOP AT ipa2002 WHERE mode = ' '.
    l_tabix = sy-tabix.
    READ TABLE t_pa2002_chg WITH KEY pernr = ipa2002-pernr
                                     begda = ipa2002-begda
                                     awart = ipa2002-awart
                                     kostl = ipa2002-kostl
                                     BINARY SEARCH.
    IF sy-subrc = 0.
      ipa2002-mode = t_pa2002_chg-mode.
      ipa2002-stdaz = t_pa2002_chg-stdaz.                   "UD1K953281
      MODIFY ipa2002 INDEX l_tabix TRANSPORTING mode stdaz. "UD1K953281
*     MODIFY ipa2002 INDEX l_tabix TRANSPORTING mode.       "UD1K953281
    ENDIF.
  ENDLOOP.
  FREE t_pa2002_chg.

* BEGIN OF UD1K953281
* Filter creation record with negative hours
  LOOP AT ipa2002 WHERE mode = '3'
                    AND stdaz <= 0.
    DELETE ipa2002.
  ENDLOOP.
* END OF UD1K953281

* Post info type 2002
  __cls: gt_bdc, gt_msg, gt_outdel.
  SORT ipa2002 BY pernr begda mode.
  LOOP AT ipa2002 WHERE mode <> space.

    WRITE ipa2002-begda TO begda.
    WRITE ipa2002-stdaz TO $stdaz.
    CONDENSE $stdaz NO-GAPS.

    IF ipa2002-mode = '3'.
      l_mode = '=INS'.
    ELSEIF ipa2002-mode = '2'.
      l_mode = '=MOD'.
    ELSEIF ipa2002-mode = '1'.
      l_mode = '=DEL'.
    ENDIF.

    PERFORM dynpro USING:
        'X' 'SAPMP50A'        '1000',
        ' ' 'BDC_OKCODE'      l_mode,
        ' ' 'RP50G-PERNR'     ipa2002-pernr,
        ' ' 'BDC_SUBSCR'      'SAPMP50A',
        ' ' 'RP50G-TIMR6'     'X',
        ' ' 'RP50G-BEGDA'     begda,
        ' ' 'RP50G-ENDDA'     begda,
        ' ' 'RP50G-CHOIC'     '2002',
        ' ' 'RP50G-SUBTY'     ipa2002-subty.

    IF ipa2002-mode = '1'.
      PERFORM dynpro USING:
          'X' 'MP200000'        '2051',
          ' ' 'BDC_OKCODE'      '=UPDL',
          ' ' 'P2002-BEGDA'     begda,
          ' ' 'P2002-ENDDA'     begda.

    ELSE.
      IF NOT ipa2002-kostl IS INITIAL.

        PERFORM dynpro USING:
            'X' 'MP200000'        '2051',
            ' ' 'BDC_CURSOR'      'P2002-STDAZ',
            ' ' 'BDC_OKCODE'      '=PRIM',
            ' ' 'P2002-BEGDA'     begda,
            ' ' 'P2002-ENDDA'     begda,
            ' ' 'P2002-STDAZ'     $stdaz.

        PERFORM dynpro USING:
            'X' 'SAPLHRTV'        '0300',
            ' ' 'BDC_OKCODE'      '=GOON',
            ' ' 'BDC_SUBSCR'      'SAPLKACB',
            ' ' 'COBL-KOSTL'       ipa2002-kostl,
            ' ' 'COBL-BUKRS'       c_bukrs.

      ELSE.
        PERFORM dynpro USING:
            'X' 'MP200000'        '2051',
            ' ' 'BDC_CURSOR'      'P2002-STDAZ',
            ' ' 'BDC_OKCODE'      '=PRIM',
            ' ' 'P2002-BEGDA'     begda,
            ' ' 'P2002-ENDDA'     begda,
            ' ' 'P2002-STDAZ'     $stdaz.

        PERFORM dynpro USING:
            'X' 'SAPLHRTV'        '0300',
            ' ' 'BDC_OKCODE'      '/EEDEL'.

      ENDIF.

      PERFORM dynpro USING:
          'X' 'MP200000'        '2051',
          ' ' 'BDC_OKCODE'      '=UPD',
          ' ' 'P2002-BEGDA'     begda,
          ' ' 'P2002-ENDDA'     begda,
          ' ' 'P2002-STDAZ'     $stdaz.

      IF ipa2002-mode = '2'.
        PERFORM dynpro USING:
            'X' 'MP200000'        '2051',
            ' ' 'BDC_OKCODE'      '/EBCK'.
      ENDIF.

    ENDIF.

    __cls gt_msg.
    CALL TRANSACTION 'PA30'   USING         gt_bdc
                              MODE p_mode
                              MESSAGES INTO gt_msg.
    l_subrc = sy-subrc.
    __cls gt_bdc.

    CLEAR gt_out.

* Get meaningful latest message
    DESCRIBE TABLE gt_msg LINES l_tabix.
    DO.
      READ TABLE gt_msg INDEX l_tabix.
      IF sy-subrc = 0.
        gt_out-flg = gt_msg-msgtyp.
        MESSAGE ID gt_msg-msgid TYPE gt_msg-msgtyp NUMBER gt_msg-msgnr
           WITH gt_msg-msgv1 gt_msg-msgv2 gt_msg-msgv3 gt_msg-msgv4
           INTO gt_out-msg.

        IF gt_msg-msgid = '00' AND gt_msg-msgnr = '344'.
          l_tabix = l_tabix - 1.
        ELSE.
          EXIT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    IF l_subrc = 0.
      IF gt_out-flg <> 'S'.
        gt_out-flg = 'S'.
        READ TABLE gt_msg WITH KEY msgtyp = 'S'.
        IF sy-subrc = 0.
          MESSAGE ID gt_msg-msgid TYPE gt_msg-msgtyp
           NUMBER gt_msg-msgnr
             WITH gt_msg-msgv1 gt_msg-msgv2 gt_msg-msgv3 gt_msg-msgv4
             INTO gt_out-msg.
        ENDIF.
      ENDIF.
    ELSE.
      IF gt_out-flg <> 'E'.
        gt_out-flg = 'E'.
        READ TABLE gt_msg WITH KEY msgtyp = 'E'.
        IF sy-subrc = 0.
          MESSAGE ID gt_msg-msgid TYPE gt_msg-msgtyp
           NUMBER gt_msg-msgnr
             WITH gt_msg-msgv1 gt_msg-msgv2 gt_msg-msgv3 gt_msg-msgv4
             INTO gt_out-msg.
        ENDIF.
      ENDIF.
    ENDIF.

    gt_out-mode = ipa2002-mode.

* If ipa2002-rec initial, it is deleted/new record, append record
    IF NOT ipa2002-rec IS INITIAL.
* BEGIN OF UD1K953127 - Update error message to corresponding record
*     MODIFY gt_out INDEX ipa2002-rec TRANSPORTING msg flg mode.
*     READ TABLE gt_out INDEX ipa2002-rec.
      READ TABLE gt_out WITH KEY rec = ipa2002-rec
                        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        MODIFY gt_out INDEX sy-tabix TRANSPORTING msg flg mode.
        READ TABLE gt_out INDEX sy-tabix.
      ENDIF.
* END OF UD1K953127
    ELSE.
      gt_out-f1 = ipa2002-pernr.
      gt_out-f2 = ipa2002-begda.
      gt_out-f3 = ipa2002-stdaz.
      gt_out-f4 = ipa2002-awart.
      gt_out-f6 = ipa2002-kostl.
      gt_out-f7 = 'P2002'.
      SHIFT gt_out-f3 LEFT DELETING LEADING space.
      SHIFT gt_out-f1 LEFT DELETING LEADING '0'.            "UD1K953127
      APPEND gt_out TO gt_outdel.
    ENDIF.

    izthru015-pernr  = ipa2002-pernr.
    izthru015-begda  = ipa2002-begda.
    izthru015-stdaz  = ipa2002-stdaz.
    izthru015-awart  = ipa2002-awart.
    izthru015-skostl = gt_out-f5.
    izthru015-rkostl = gt_out-f6.
    izthru015-itflag = gt_out-f7.
    izthru015-flag   = gt_out-flg.
    izthru015-msg    = gt_out-msg.
    izthru015-zuser  = sy-uname.
    izthru015-zsdat  = sy-datum.
    izthru015-zstim  = sy-uzeit.
    APPEND izthru015.
  ENDLOOP.
  FREE ipa2002.


  LOOP AT gt_out.
    CASE gt_out-flg.
      WHEN 'S'.
        gt_out-icon = icon_green_light.
      WHEN 'E'.
        gt_out-icon = icon_red_light.
      WHEN 'W'.                                             "UD1K953281
        gt_out-icon = icon_yellow_light.                    "UD1K953281
      WHEN OTHERS.
        IF NOT p_post IS INITIAL.                           "UD1K952893
          gt_out-icon = icon_yellow_light.
          gt_out-msg = 'No Change Made'.
        ELSEIF NOT gt_out-chk IS INITIAL.                   "UD1K952893
          gt_out-icon = icon_yellow_light.                  "UD1K952893
          gt_out-msg = 'No Change Made'.                    "UD1K952893
        ENDIF.
    ENDCASE.

    MODIFY gt_out.
  ENDLOOP.

  LOOP AT gt_outdel.
    gt_outdel-rec = sy-tabix.

    CASE gt_outdel-flg.
      WHEN 'S'.
        gt_outdel-icon = icon_green_light.
      WHEN 'E'.
        gt_outdel-icon = icon_red_light.
    ENDCASE.

    MODIFY gt_outdel.
  ENDLOOP.

  MODIFY zthru015 FROM TABLE izthru015.
  COMMIT WORK.
  FREE izthru015.

  MESSAGE s000 WITH 'Processing Completed'.                 "UD1K953281
ENDFORM.                    " SAVE_DATA
*---------------------------------------------------------------------*
*       FORM refresh_alv                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM refresh_alv.
  PERFORM refresh_field.
ENDFORM.                    " REFRESH_ALV

*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
FORM set_lvc_layout.
  CLEAR gs_layo.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
  gs_layo-stylefname = 'HANDLE_STYLE'.
  gs_layo-no_rowmark = 'X'.                                 "UD1K953214

ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
FORM exclude_functions.
  PERFORM append_exclude_functions
           TABLES gt_exclude[]
           USING: cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_append_row,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.

ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_100 OUTPUT.
  IF g_custom_container IS INITIAL.
    PERFORM create_and_init_alv.

*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layo
        it_toolbar_excluding = gt_exclude
        i_save               = gc_var_save
        is_variant           = gs_variant
      CHANGING
        it_outtab            = gt_out[]
        it_fieldcatalog      = gt_fcat[]
        it_sort              = gt_sort[].
  ENDIF.
  __focus g_grid.

ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM double_click USING  e_row     TYPE lvc_s_row
                         e_column  TYPE lvc_s_col
                         es_row_no TYPE lvc_s_roid.

ENDFORM.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  create_and_init_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_alv.
* Create object
  PERFORM create_object.

* Exclude toolbar
  PERFORM exclude_functions.

* Set GUI Status
  SET PF-STATUS '100'.

* Create Object to verify input values.
  CREATE OBJECT g_event_receiver.
  SET: HANDLER g_event_receiver->handle_data_changed FOR g_grid,
       HANDLER g_event_receiver->handle_double_click FOR g_grid.

* Create field category
  PERFORM: create_field_category,
           set_lvc_layout.

* Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " create_and_init_alv
*&---------------------------------------------------------------------*
*&      Form  upload_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_file USING filename.
  DATA: l_filename TYPE string,
        l_rec      TYPE i,
        l_stdaz    TYPE pa2002-stdaz.                       "UD1K953127

  l_filename = filename.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = l_filename
    TABLES
      data_tab                = rawdata
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    IF sy-subrc = 13.
      MESSAGE s011 WITH 'File has been opened'.
    ELSEIF NOT sy-msgid IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE s011 WITH 'Error when reading the file'.
    ENDIF.

    STOP.
  ENDIF.

  LOOP AT rawdata INTO w_rawdata.
    CLEAR gt_out.
    SPLIT w_rawdata AT ',' INTO gt_out-f1
                                gt_out-f2
                                gt_out-f3
                                gt_out-f4
                                gt_out-f5
                                gt_out-f6
                                gt_out-f7.
    CHECK NOT gt_out IS INITIAL.
    l_stdaz = gt_out-f3.                                    "UD1K953127
    gt_out-f3 = l_stdaz.                                    "UD1K953127
    SHIFT gt_out-f3 LEFT DELETING LEADING space.            "UD1K953127

    l_rec = l_rec + 1.
    gt_out-rec = l_rec.
    APPEND gt_out.
  ENDLOOP.

  FREE rawdata.

ENDFORM.                    " get_data_from_dkbz_dkpo
*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM browser CHANGING filename answer.
  DATA: l_dfile TYPE string,
        lt_file TYPE filetable,
        l_rc    TYPE i.

  l_dfile = c_dfile.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select File Name'
      default_extension       = 'csv'
      default_filename        = l_dfile
      file_filter             = 'CSV (*.csv)|*.csv| All (*.*)|*.*'
      initial_directory       = '\\10.121.233.22\Data'
    CHANGING
      file_table              = lt_file
      rc                      = l_rc
      user_action             = answer
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      OTHERS                  = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSEIF answer = 0.
    READ TABLE lt_file INTO filename INDEX 1.
  ELSE.
    MESSAGE s118(ba).
  ENDIF.

ENDFORM.                    " BROWSER
*&---------------------------------------------------------------------*
*&      Form  get_selected_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*      -->P_LT_ROW_NO  text
*----------------------------------------------------------------------*
FORM get_selected_data TABLES pt_rows STRUCTURE lvc_s_row
                             pt_row_no STRUCTURE lvc_s_roid.

  __cls $gt_out.

  CLEAR gt_out-chk.
  MODIFY gt_out TRANSPORTING chk WHERE chk = 'X'.

* Selected Row by row selection
  LOOP AT pt_rows WHERE rowtype IS INITIAL.
    READ TABLE gt_out INDEX pt_rows-index.
    gt_out-chk = true .
    MODIFY gt_out INDEX pt_rows-index .
  ENDLOOP.

  LOOP AT gt_out WHERE chk EQ true.
    $gt_out = gt_out.
    APPEND $gt_out.
  ENDLOOP.

ENDFORM.                    " GET_POSTING_DATA
*&---------------------------------------------------------------------*
*&      Form  get_$gt_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_$gt_out.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "Numeric IDs of Selected Rows

  CLEAR gt_out-flg.
  MODIFY gt_out TRANSPORTING flg WHERE f1 <> space.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  PERFORM get_selected_data TABLES lt_rows
                                   lt_row_no .

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    gt_out-chk = true.                                      "UD1K952893
    MODIFY gt_out TRANSPORTING chk WHERE chk = space.       "UD1K952893

    __cls $gt_out.
    $gt_out[] = gt_out[].
  ENDIF.

ENDFORM.                    " get_$gt_out
*&---------------------------------------------------------------------*
*&      Form  get_from_unix
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_from_unix.
  DATA: l_rec TYPE i,
        l_stdaz TYPE pa2002-stdaz.                          "UD1K953127

  OPEN DATASET p_file FOR INPUT IN TEXT MODE.
  IF sy-subrc <> 0.
    IF sy-subrc = 8.
      MESSAGE s011 WITH 'File not Found'.
    ELSEIF NOT sy-msgid IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE s011 WITH 'Error when reading the file'.
    ENDIF.

    STOP.
  ENDIF.

  DO.
    READ DATASET p_file INTO w_rawdata.
    IF sy-subrc NE 0. EXIT. ENDIF.
    APPEND w_rawdata TO rawdata.
  ENDDO.

  CLOSE DATASET p_file.

  LOOP AT rawdata INTO w_rawdata.
    CLEAR gt_out.
    SPLIT w_rawdata AT ',' INTO gt_out-f1
                                gt_out-f2
                                gt_out-f3
                                gt_out-f4
                                gt_out-f5
                                gt_out-f6
                                gt_out-f7.
    CHECK NOT gt_out IS INITIAL.
    l_stdaz = gt_out-f3.                                    "UD1K953127
    gt_out-f3 = l_stdaz.                                    "UD1K953127
    SHIFT gt_out-f3 LEFT DELETING LEADING space.            "UD1K953127

    l_rec = l_rec + 1.
    gt_out-rec = l_rec.
    APPEND gt_out.
  ENDLOOP.

  FREE rawdata.

ENDFORM.                    " get_from_unix
*&---------------------------------------------------------------------*
*&      Form  display_unix
*&---------------------------------------------------------------------*
*       Display UNIX Directory
*----------------------------------------------------------------------*
*      <--filename  filename
*      <--answer    return code
*----------------------------------------------------------------------*
FORM display_unix CHANGING filename answer.
  DATA: BEGIN OF it_filename OCCURS 0,
          path(1024) TYPE c,
        END OF it_filename.

* BEGIN OF UD1K951876
** Furong on 06/17/14 -change directory (
*  CONCATENATE c_unix_pre sy-sysid c_unix_suf INTO it_filename-path.
  it_filename-path = '/usr/sap/EDI_SAP/HR/Kronos'.
** )
  APPEND it_filename.
*  SELECT dirname
*    FROM user_dir
*    INTO TABLE it_filename
*   WHERE aliass = 'DIR_KRONOS'.
* END OF UD1K951876

*  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_TEMP'
*                     ID 'VALUE' FIELD it_filename-path.
*  APPEND it_filename.

  CALL FUNCTION 'POPUP_WITH_TABLE'
    EXPORTING
      endpos_col   = '100'
      endpos_row   = '10'
      startpos_col = '1'
      startpos_row = '1'
      titletext    = 'Select UNIX Directory'
    IMPORTING
      choice       = filename
    TABLES
      valuetab     = it_filename
    EXCEPTIONS
      break_off    = 1
      OTHERS       = 2.

  answer = sy-subrc.

  IF sy-subrc = 0.
    CONCATENATE filename '/' c_dfile INTO filename.
  ELSE.
    MESSAGE s549(fibl).
  ENDIF.
ENDFORM.                    " display_unix
*&---------------------------------------------------------------------*
*&      Form  APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       Append excluding functions
*----------------------------------------------------------------------*
*      -->P_TABNAME   Table name
*      -->P_VALUE     Excluding value
*----------------------------------------------------------------------*
FORM append_exclude_functions TABLES p_table
                              USING p_value.
  DATA ls_exclude TYPE ui_func.

  ls_exclude = p_value.
  APPEND ls_exclude TO p_table.

ENDFORM.                    " APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------
*
*&      Form  REFRESH_FIELD
*&---------------------------------------------------------------------
*
*       Refresh for display
*----------------------------------------------------------------------
FORM refresh_field.
  CALL METHOD g_grid->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = gt_fcat.

  CALL METHOD g_grid->set_frontend_layout
    EXPORTING
      is_layout = gs_layo.

  __set_refresh_mode 'X'.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = stable.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " REFRESH_FIELD
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------*
*       Create custom container control & instance
*----------------------------------------------------------------------*
FORM create_object.
* create a custom container control for our alv control
  CREATE OBJECT g_custom_container
    EXPORTING
      container_name = container.

* Create an Instance of ALV Control
  CREATE OBJECT g_grid
    EXPORTING
      i_parent = g_custom_container.

ENDFORM.                    " CREATE_OBJECT
*---------------------------------------------------------------------*
*       Form DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM dynpro USING p_dynbegin p_name p_value.
  CLEAR gt_bdc.

  IF p_dynbegin = 'X'.
    gt_bdc-program = p_name.
    gt_bdc-dynpro = p_value.
    gt_bdc-dynbegin = p_dynbegin.
  ELSE.
    gt_bdc-fnam = p_name.
    gt_bdc-fval = p_value.
  ENDIF.

  APPEND gt_bdc.

ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  maintain_log
*&---------------------------------------------------------------------*
*       Maintain posting log
*----------------------------------------------------------------------*
FORM maintain_log.
  DATA: l_deldate TYPE sy-datum.

  l_deldate = sy-datum - p_day.

  DELETE FROM zthru015 WHERE begda <= l_deldate.
  COMMIT WORK.
ENDFORM.                    " maintain_log
*&---------------------------------------------------------------------*
*&      Form  upload_data
*&---------------------------------------------------------------------*
*       Upload Data from Local/UNIX
*----------------------------------------------------------------------*
FORM upload_data.
  IF  p_serv EQ true.
    PERFORM get_from_unix.
  ELSE.
    PERFORM upload_file USING p_file.
  ENDIF.
ENDFORM.                    " upload_data
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       Write data to screen
*----------------------------------------------------------------------*
FORM display_data.
* Write Changed Records Details
  LOOP AT gt_out.
    IF gt_out-flg EQ 'S'.
      IF gt_out-mode = '3'.
        ADD 1 TO i_cnt.
      ELSEIF gt_out-mode = space.
        ADD 1 TO n_cnt.
      ELSE.
        ADD 1 TO u_cnt.
      ENDIF.
    ELSEIF gt_out-flg EQ 'E'.
      ADD 1 TO e_cnt.
    ELSE.
      ADD 1 TO n_cnt.
    ENDIF.

    ADD 1 TO t_cnt.

    WRITE :/(10) gt_out-f1, (10) gt_out-f2, (10) gt_out-f3,
            (10) gt_out-f4, (10) gt_out-f5, (10) gt_out-f6,
            (10) gt_out-icon AS ICON,       (99) gt_out-msg.

  ENDLOOP.

* Give space
  IF NOT gt_out[] IS INITIAL AND NOT gt_outdel[] IS INITIAL.
    SKIP.
  ENDIF.

* Write Deleted Records Details
  LOOP AT gt_outdel.
    IF gt_outdel-mode = '3'.                                "UD1K953127
      ADD 1 TO i_cnt.                                       "UD1K953127
    ELSE.                                                   "UD1K953127
      ADD 1 TO d_cnt.
    ENDIF.                                                  "UD1K953127

    ADD 1 TO t_cnt.
    WRITE :/(10) gt_outdel-f1, (10) gt_outdel-f2, (10) gt_outdel-f3,
            (10) gt_outdel-f4, (10) gt_outdel-f5, (10) gt_outdel-f6,
            (10) gt_outdel-icon AS ICON,          (99) gt_outdel-msg.
  ENDLOOP.

* Write Summary
  SKIP.

  WRITE :/ 'Processed:' COLOR COL_TOTAL,
            t_cnt COLOR COL_TOTAL.
  WRITE :/ 'Created  :' COLOR COL_POSITIVE,
            i_cnt       COLOR COL_POSITIVE.
  WRITE :/ 'Changed  :' COLOR COL_POSITIVE,
            u_cnt       COLOR COL_POSITIVE.
  WRITE :/ 'Deleted  :' COLOR COL_POSITIVE,
            d_cnt       COLOR COL_POSITIVE.
  WRITE :/ 'Unchanged:' COLOR COL_POSITIVE,
            n_cnt       COLOR COL_POSITIVE.
  WRITE :/ 'Error    :' COLOR COL_NEGATIVE,
            e_cnt       COLOR COL_NEGATIVE.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       Process data: post and display the result.
*----------------------------------------------------------------------*
FORM process_data.
  IF p_post EQ true.
    $gt_out[] = gt_out[].

    PERFORM save_data.
    PERFORM display_data.
  ELSE.
    CALL SCREEN 100.
  ENDIF.
ENDFORM.                    " process_data
* BEGIN OF UD1K952747
*&---------------------------------------------------------------------*
*&      Form  APPEND_LOG
*&---------------------------------------------------------------------*
*       Append Execution Log
*----------------------------------------------------------------------*
FORM append_log.
  READ TABLE gt_out INDEX $gt_out-rec.

* BEGIN OF UD1K952995
  IF gt_out-f3 > 99.
    gt_out-msg = 'Hours too big (greater than 99)'.
    gt_out-flg = 'E'.
    CLEAR gt_out-f3.
    MODIFY gt_out INDEX $gt_out-rec TRANSPORTING msg flg f3.
  ENDIF.
* END OF UD1K952995

  izthru015-pernr  = gt_out-f1.
  izthru015-begda  = gt_out-f2.
  izthru015-stdaz  = gt_out-f3.
  izthru015-awart  = gt_out-f4.
  izthru015-skostl = gt_out-f5.
  izthru015-rkostl = gt_out-f6.
  izthru015-itflag = gt_out-f7.
  izthru015-flag   = gt_out-flg.
  izthru015-msg    = gt_out-msg.
  izthru015-zuser  = sy-uname.
  izthru015-zsdat  = sy-datum.
  izthru015-zstim  = sy-uzeit.
  APPEND izthru015.
ENDFORM.                    " APPEND_LOG
* END OF UD1K952747

* BEGIN OF UD1K953127
*&---------------------------------------------------------------------*
*&      Form  get_pay_period
*&---------------------------------------------------------------------*
*       Get Payroll Period
*----------------------------------------------------------------------*
FORM get_pay_period.
  DATA: l_period TYPE t569v-pabrp,
        l_year   TYPE t569v-pabrj.

  DATA: l_vabrp LIKE t549q-vabrp,
        l_vabrj LIKE t549q-vabrj.

  CLEAR: s_cdate, s_cdate[].

  CASE 'X'.
    WHEN r_cprd.
      CLEAR: l_period, l_year,
             p_oprd, p_oyr,
             s_fdate, s_fdate[].                            "UD1K953281
      CALL FUNCTION 'PA03_PERIODDATES_GET'
        EXPORTING
          f_abkrs               = p_abkrs
        IMPORTING
          f_current_begda       = s_cdate-low
          f_current_endda       = s_cdate-high
        CHANGING
          f_current_period      = l_period
          f_current_year        = l_year
        EXCEPTIONS
          pcr_does_not_exist    = 1
          abkrs_does_not_exist  = 2
          period_does_not_exist = 3
          OTHERS                = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        s_cdate-sign = 'I'.
        s_cdate-option = 'BT'.
        APPEND s_cdate.

        p_cprd = l_period.
        p_cyr  = l_year.

      ENDIF.

    WHEN r_oprd.
      IF p_oprd IS INITIAL OR p_oyr IS INITIAL.
        MESSAGE s016(pn) WITH 'Please enter a period'.
      ENDIF.
      CLEAR: p_cprd, p_cyr,
             s_fdate, s_fdate[],                            "UD1K953281
             s_cdate, s_cdate[].
      l_period = p_oprd.
      l_year   = p_oyr.
      IF NOT l_period IS INITIAL.
        CALL FUNCTION 'PA03_PERIODDATES_GET'
          EXPORTING
            f_abkrs               = p_abkrs
          IMPORTING
            f_current_begda       = s_cdate-low
            f_current_endda       = s_cdate-high
          CHANGING
            f_current_period      = l_period
            f_current_year        = l_year
          EXCEPTIONS
            pcr_does_not_exist    = 1
            abkrs_does_not_exist  = 2
            period_does_not_exist = 3
            OTHERS                = 4.
        s_cdate-sign = 'I'.
        s_cdate-option = 'BT'.
        APPEND s_cdate.
      ENDIF.
* BEGIN OF UD1K953281
    WHEN r_fprd.
      IF s_fdate-low IS INITIAL OR s_fdate-low IS INITIAL.
        MESSAGE s016(pn) WITH 'Please enter a date range'.
      ENDIF.
      CLEAR: p_cprd, p_cyr,
             p_oprd, p_oyr.
      s_cdate-low  = s_fdate-low.
      s_cdate-high = s_fdate-high.
        APPEND s_cdate.
* END OF UD1K953281
** Fuorng on 06/25/14 (
    WHEN r_hpvc.
      CLEAR: s_cdate.
      SELECT SINGLE begda endda INTO (s_cdate-low, s_cdate-high)
        FROM t549q
        WHERE permo  = '4'
          AND begda <= sy-datum
          AND endda >= sy-datum.
      IF sy-subrc = 0.
        REFRESH: s_cdate.
        s_cdate-sign = 'I'.
        s_cdate-option = 'BT'.
        APPEND s_cdate.
      ENDIF.
    WHEN r_hpvp.
      CLEAR: s_cdate.
      CLEAR: l_vabrp, l_vabrj.
      SELECT SINGLE vabrj vabrp
         INTO (l_vabrj, l_vabrp)
        FROM t549q
       WHERE permo  = '4'
         AND begda <= sy-datum
         AND endda >= sy-datum.
      IF l_vabrp IS NOT INITIAL.
        SELECT SINGLE begda endda INTO (s_cdate-low, s_cdate-high)
          FROM t549q
         WHERE permo = '4'
           AND pabrj = l_vabrj
           AND pabrp = l_vabrp.
        IF sy-subrc = 0.
          REFRESH: s_cdate.
          s_cdate-sign = 'I'.
          s_cdate-option = 'BT'.
          APPEND s_cdate.
        ENDIF.
      ENDIF.
** )
  ENDCASE.

*  IF r_fprd IS INITIAL.                                     "UD1K953281
** Furong on 07/28/14 (
*  IF r_fprd IS INITIAL AND r_oprd IS INITIAL.
*** END )
*    CALL FUNCTION 'PA03_PERIODDATES_GET'
*      EXPORTING
*        f_abkrs               = p_abkrs
*      IMPORTING
*        f_current_begda       = s_cdate-low
*        f_current_endda       = s_cdate-high
*      CHANGING
*        f_current_period      = l_period
*        f_current_year        = l_year
*      EXCEPTIONS
*        pcr_does_not_exist    = 1
*        abkrs_does_not_exist  = 2
*        period_does_not_exist = 3
*        OTHERS                = 4.
*                                                  "UD1K953281
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    s_cdate-sign = 'I'.
*    s_cdate-option = 'BT'.
*    APPEND s_cdate.
*
*    IF NOT r_cprd IS INITIAL.
*      p_cprd = l_period.
*      p_cyr  = l_year.
*    ENDIF.
*  ENDIF.
*  ENDIF.

ENDFORM.                    " get_pay_period
* END OF UD1K953127
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data .
  DATA: l_period TYPE t569v-pabrp,
       l_year   TYPE t569v-pabrj.

  DATA: l_vabrp LIKE t549q-vabrp,
        l_vabrj LIKE t549q-vabrj.

  CLEAR: s_cdate, s_cdate[].

  CASE 'X'.
    WHEN r_cprd.
*      CLEAR: l_period, l_year,
*             p_oprd, p_oyr,
*             s_fdate, s_fdate[].                            "UD1K953281
    WHEN r_oprd.
*      IF p_oprd IS INITIAL OR p_oyr IS INITIAL.
*        MESSAGE e016(pn) WITH 'Please enter a period'.
*      ENDIF.
      CLEAR: p_cprd, p_cyr,
             s_fdate, s_fdate[],                            "UD1K953281
             s_cdate, s_cdate[].
*      l_period = p_oprd.
*      l_year   = p_oyr.
** BEGIN OF UD1K953281
*    WHEN r_fprd.
*      IF s_fdate-low IS INITIAL OR s_fdate-low IS INITIAL.
*        MESSAGE e016(pn) WITH 'Please enter a date range'.
*      ENDIF.
*      CLEAR: p_cprd, p_cyr,
*             p_oprd, p_oyr.
*      s_cdate-low  = s_fdate-low.
*      s_cdate-high = s_fdate-high.
** END OF UD1K953281
** Fuorng on 06/25/14 (
    WHEN r_hpvc.
      CLEAR: s_cdate.
      SELECT SINGLE begda endda INTO (s_cdate-low, s_cdate-high)
        FROM t549q
        WHERE permo  = '4'
          AND begda <= sy-datum
          AND endda >= sy-datum.
      IF sy-subrc = 0.
        REFRESH: s_cdate.
        s_cdate-sign = 'I'.
        s_cdate-option = 'BT'.
        APPEND s_cdate.
      ENDIF.
    WHEN r_hpvp.
      CLEAR: s_cdate.
      CLEAR: l_vabrp, l_vabrj.
      SELECT SINGLE vabrj vabrp
         INTO (l_vabrj, l_vabrp)
        FROM t549q
       WHERE permo  = '4'
         AND begda <= sy-datum
         AND endda >= sy-datum.
      IF l_vabrp IS NOT INITIAL.
        SELECT SINGLE begda endda INTO (s_cdate-low, s_cdate-high)
          FROM t549q
         WHERE permo = '4'
           AND pabrj = l_vabrj
           AND pabrp = l_vabrp.
        IF sy-subrc = 0.
          REFRESH: s_cdate.
          s_cdate-sign = 'I'.
          s_cdate-option = 'BT'.
          APPEND s_cdate.
        ENDIF.
      ENDIF.
** )

  ENDCASE.

  IF  r_fprd IS INITIAL AND r_oprd IS INITIAL.

    CALL FUNCTION 'PA03_PERIODDATES_GET'
      EXPORTING
        f_abkrs               = p_abkrs
      IMPORTING
        f_current_begda       = s_cdate-low
        f_current_endda       = s_cdate-high
      CHANGING
        f_current_period      = l_period
        f_current_year        = l_year
      EXCEPTIONS
        pcr_does_not_exist    = 1
        abkrs_does_not_exist  = 2
        period_does_not_exist = 3
        OTHERS                = 4.
  ENDIF.                                                    "UD1K953281

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    s_cdate-sign = 'I'.
    s_cdate-option = 'BT'.
    APPEND s_cdate.

    IF NOT r_cprd IS INITIAL.
      p_cprd = l_period.
      p_cyr  = l_year.
    ENDIF.
  ENDIF.

ENDFORM.                    " INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_PAYROLL_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_payroll_status.
  DATA: l_state LIKE t569v-state.

  CLEAR: w_status.
  SELECT SINGLE state INTO l_state
    FROM t569v
    WHERE abkrs = p_abkrs.
  IF l_state <> 3.
    w_status = 'X'.
  ENDIF.
ENDFORM.                    " CHECK_PAYROLL_STATUS
