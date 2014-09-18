*----------------------------------------------------------------------
* Program ID        : ZAHRU014
* Title             : Payroll import to SAP for KRONOS data
* Created on        : 2/7/2010
* Created by        : I.G.MOON / Valerian Utama
* Specifications By : Euna.Lee
* Description       : Payroll will be imported to SAP on a weekly basis
*
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  03/23/2011  Valerian  UD1K951043  Modify Program Logic
*  07/27/2011  Valerian  UD1K952590  Automate UNIX Path selection
*                                    based on the run SAP system
*  08/19/2011  Valerian  UD1K952791  Collect records before posting
*  08/19/2011  Valerian  UD1K952797  Deactive cost center check logic
*  09/13/2011  Valerian  UD1K952959  Reject posting with negative hours
*                                    Update message only for selected
*                                    records in interactive mode
*----------------------------------------------------------------------

REPORT zahru014 MESSAGE-ID zmfi NO STANDARD PAGE HEADING.

TYPE-POOLS: truxs, slis.
INCLUDE: <icon>, <symbol>.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS:  true  VALUE 'X',
            c_bukrs TYPE bukrs VALUE 'H201',
            c_dfile(30) TYPE c VALUE 'SAP_Payroll.CSV',
            c_unix_pre(30) TYPE c VALUE '/sapmnt/',         "UD1K952590
            c_unix_suf(30) TYPE c VALUE '/kronos/kronosftp'."UD1K952590
*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES: pa0000, *pa2010.

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
      izthru014 LIKE zthru014 OCCURS 0 WITH HEADER LINE,
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
         f4(20)  TYPE c,     "Payroll Code
         f5(20)  TYPE c,     "Sender CC
         f6(20)  TYPE c,     "Receiver CC
         chk(1)  TYPE c,
         msg(50) TYPE c,
         rec     TYPE i,
         flg(1)  TYPE c,
         mode(1) TYPE c,
         icon(4) TYPE c,
         rec1    TYPE i,     "Collected rec.no.             "UD1K952791
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

ENDCLASS.

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
        control = &1 .
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
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-005.

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
  ENDLOOP.

AT SELECTION-SCREEN .
  IF p_file IS INITIAL.
    MESSAGE e000 WITH 'Please Enter Filename'.
  ENDIF.

TOP-OF-PAGE.
  WRITE: / text-h01, text-h02, text-h03, text-h04,
           text-h05, text-h06, text-h07, text-h08.
  ULINE.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
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
          ' '  'F4'   'Payroll Code'   20 'CHAR' '',
          ' '  'F5'   'Sender CC'      20 'CHAR' '',
          ' '  'F6'   'Receiver CC'    20 'CHAR' '',
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
       EXPORTING is_stable = stable.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       Save data to table ZTCOU105
*----------------------------------------------------------------------*
FORM save_data.
  DATA: begda(10),
        $anzhl(10).

  __cls  izthru014.

  DATA : BEGIN OF ipa2010 OCCURS 0.
          INCLUDE STRUCTURE pa2010.
  DATA :  kostl TYPE kostl,
          rec TYPE i,
          mode(1) TYPE c,
         END   OF  ipa2010.

  DATA $numc(4) TYPE n.


  DATA: BEGIN OF t_pa2010_old OCCURS 0,
          pernr TYPE pa2010-pernr,
          begda TYPE pa2010-begda,
          lgart TYPE pa2010-lgart,
          seqnr TYPE pa2010-seqnr,
          anzhl TYPE pa2010-anzhl,
          kostl TYPE kostl,
         END OF t_pa2010_old.

  DATA: t_pa2010_new LIKE t_pa2010_old OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF t_pa2010_chg OCCURS 0.
          INCLUDE STRUCTURE t_pa2010_old.
  DATA: mode(1) TYPE c,
        END OF t_pa2010_chg.

  DATA: BEGIN OF t_pa0000 OCCURS 0,
          pernr TYPE pa0000-pernr,
        END OF t_pa0000.

  DATA: BEGIN OF t_asshr OCCURS 0,
          pernr TYPE asshr-pernr,
          begda TYPE asshr-begda,
          subty TYPE asshr-subty,
          seqnr TYPE asshr-seqnr,
          kostl TYPE assob-kostl,
        END OF t_asshr.

  DATA: l_tabix TYPE sy-tabix,
        l_mode(4) TYPE c,
        l_subrc TYPE sy-subrc.

  __cls ipa2010.

  SELECT DISTINCT pernr INTO TABLE t_pa0000
    FROM pa0000
   WHERE endda = '99991231'.
  SORT t_pa0000 BY pernr.

  LOOP AT $gt_out.
    READ TABLE t_pa0000 WITH KEY pernr = $gt_out-f1
                             BINARY SEARCH.

    IF sy-subrc <> 0.
      gt_out-msg = 'Invalid TM#'.
      gt_out-flg = 'E'.
      MODIFY gt_out INDEX $gt_out-rec TRANSPORTING msg flg.
      PERFORM append_log.                                   "UD1K952791
      CONTINUE.
    ENDIF.

* BEGIN OF UD1K952959
    IF $gt_out-f3 LT 0.
      gt_out-msg = 'Posting negative hours not allowed'.
      gt_out-flg = 'S'.
      MODIFY gt_out INDEX $gt_out-rec TRANSPORTING msg flg.
      PERFORM append_log.
      CONTINUE.
    ENDIF.
* END OF UD1K952959

    CLEAR *pa2010.
     *pa2010-pernr = $gt_out-f1.
     *pa2010-begda = $gt_out-f2.
     *pa2010-endda = $gt_out-f2.
    $numc = $gt_out-f4.
     *pa2010-subty = $numc.
     *pa2010-anzhl = $gt_out-f3.
     *pa2010-lgart = *pa2010-subty.

    MOVE-CORRESPONDING *pa2010 TO ipa2010.

    IF $gt_out-f5 <> $gt_out-f6.
      ipa2010-kostl = $gt_out-f6.
      SHIFT ipa2010-kostl LEFT DELETING LEADING '0'.
    ENDIF.
*   ipa2010-rec = $gt_out-rec.                              "UD1K952791
    COLLECT ipa2010.                                        "UD1K952791
    gt_out-rec1 = sy-tabix.                                 "UD1K952791
    MODIFY gt_out INDEX $gt_out-rec TRANSPORTING rec1.      "UD1K952791
*   APPEND ipa2010.                                         "UD1K952791
    CLEAR ipa2010.
  ENDLOOP.
  FREE: $gt_out, t_pa0000.

* BEGIN OF UD1K952791
* Re-number the record count
  LOOP AT ipa2010.
    ipa2010-rec = sy-tabix.
    MODIFY ipa2010 INDEX sy-tabix TRANSPORTING rec.
  ENDLOOP.
* END OF UD1K952791

* Compare PA2010
  IF NOT ipa2010[] IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_pa2010_old
      FROM pa2010
      FOR ALL ENTRIES IN ipa2010
     WHERE pernr = ipa2010-pernr
       AND endda = ipa2010-endda
       AND begda = ipa2010-begda.

    SORT t_pa2010_old BY pernr begda lgart seqnr.
    DELETE ADJACENT DUPLICATES FROM t_pa2010_old
           COMPARING pernr begda lgart.

    IF NOT t_pa2010_old[] IS INITIAL.
      SELECT a~pernr a~begda a~subty a~seqnr b~kostl
        INTO CORRESPONDING FIELDS OF TABLE t_asshr
        FROM asshr AS a JOIN assob AS b
                        ON a~pdsnr = b~pdsnr
             FOR ALL ENTRIES IN t_pa2010_old
       WHERE  a~pernr = t_pa2010_old-pernr
         AND  a~infty = '2010'
         AND  a~subty = t_pa2010_old-lgart
         AND  a~endda = t_pa2010_old-begda
         AND  a~begda = t_pa2010_old-begda
         AND  a~seqnr = t_pa2010_old-seqnr.
    ENDIF.

    SORT t_asshr BY pernr begda subty.
    LOOP AT t_pa2010_old.
      l_tabix = sy-tabix.
      READ TABLE t_asshr WITH KEY pernr = t_pa2010_old-pernr
                                  begda = t_pa2010_old-begda
                                  subty = t_pa2010_old-lgart
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        t_pa2010_old-kostl = t_asshr-kostl.
        SHIFT t_pa2010_old-kostl LEFT DELETING LEADING '0'.
      ENDIF.

      CLEAR t_pa2010_old-seqnr.
      MODIFY t_pa2010_old INDEX l_tabix TRANSPORTING seqnr kostl.
    ENDLOOP.
    FREE t_asshr.

    LOOP AT ipa2010.
      MOVE-CORRESPONDING ipa2010 TO t_pa2010_new.
      APPEND t_pa2010_new.
    ENDLOOP.

    SORT: t_pa2010_new BY pernr begda lgart,
          t_pa2010_old BY pernr begda lgart.

* Give Status '1'= delete '2'= update '3'= insert
    LOOP AT t_pa2010_new.
      READ TABLE t_pa2010_old WITH KEY pernr = t_pa2010_new-pernr
                                       begda = t_pa2010_new-begda
                                       lgart = t_pa2010_new-lgart
                                       BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE t_pa2010_old INDEX sy-tabix.
        IF t_pa2010_new <> t_pa2010_old.
          MOVE-CORRESPONDING t_pa2010_new TO t_pa2010_chg.
          t_pa2010_chg-mode = '2'.
          APPEND t_pa2010_chg.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING t_pa2010_new TO t_pa2010_chg.
        t_pa2010_chg-mode = '3'.
        APPEND t_pa2010_chg.
      ENDIF.
      CLEAR t_pa2010_chg.
    ENDLOOP.
    FREE t_pa2010_new.

    LOOP AT t_pa2010_old.
      CLEAR ipa2010.
      MOVE-CORRESPONDING t_pa2010_old TO ipa2010.
      ipa2010-subty = ipa2010-lgart.
      ipa2010-mode = '1'.
      APPEND ipa2010.
    ENDLOOP.
    FREE t_pa2010_old.
  ENDIF.

  SORT t_pa2010_chg BY pernr begda lgart.
  LOOP AT ipa2010 WHERE mode = ' '.
    l_tabix = sy-tabix.
    READ TABLE t_pa2010_chg WITH KEY pernr = ipa2010-pernr
                                     begda = ipa2010-begda
                                     lgart = ipa2010-lgart
                                     BINARY SEARCH.
    IF sy-subrc = 0.
      ipa2010-mode = t_pa2010_chg-mode.
      MODIFY ipa2010 INDEX l_tabix TRANSPORTING mode.
    ENDIF.
  ENDLOOP.
  FREE t_pa2010_chg.

  __cls: gt_bdc, gt_msg, gt_outdel.

  SORT ipa2010 BY pernr begda mode.
  LOOP AT ipa2010 WHERE mode <> space.

    WRITE ipa2010-begda TO begda.
    WRITE ipa2010-anzhl TO $anzhl.
    CONDENSE $anzhl NO-GAPS.

    IF ipa2010-mode = '3'.
      l_mode = '=INS'.
    ELSEIF ipa2010-mode = '2'.
      l_mode = '=MOD'.
    ELSEIF ipa2010-mode = '1'.
      l_mode = '=DEL'.
    ENDIF.

    PERFORM dynpro USING:
        'X' 'SAPMP50A'        '1000',
        ' ' 'BDC_OKCODE'      l_mode,
        ' ' 'RP50G-PERNR'     ipa2010-pernr,
        ' ' 'BDC_SUBSCR'      'SAPMP50A',
        ' ' 'RP50G-TIMR6'     'X',
        ' ' 'RP50G-BEGDA'     begda,
        ' ' 'RP50G-ENDDA'     begda,
        ' ' 'RP50G-CHOIC'     '2010',
        ' ' 'RP50G-SUBTY'     ipa2010-subty.

    IF ipa2010-mode = '1'.
      PERFORM dynpro USING:
          'X' 'MP200000'        '2450',
          ' ' 'BDC_OKCODE'      '=UPDL',
          ' ' 'P2010-BEGDA'     begda.

    ELSE.
      IF NOT ipa2010-kostl IS INITIAL.

        PERFORM dynpro USING:
            'X' 'MP200000'        '2450',
            ' ' 'BDC_CURSOR'      'Q2010-EITXT',
            ' ' 'BDC_OKCODE'      '=PRIM',
            ' ' 'P2010-BEGDA'     begda,
            ' ' 'P2010-LGART'     ipa2010-lgart,
            ' ' 'P2010-ANZHL'     $anzhl,
            ' ' 'Q2010-EITXT'     'Hours',
            ' ' 'P2010-WAERS'     'USD'.

        PERFORM dynpro USING:
            'X' 'SAPLHRTV'        '0300',
            ' ' 'BDC_OKCODE'      '=GOON',
            ' ' 'BDC_SUBSCR'      'SAPLKACB',
            ' ' 'BDC_CURSOR'      'COBL-KOSTL',
            ' ' 'COBL-KOSTL'       ipa2010-kostl,
            ' ' 'COBL-BUKRS'       c_bukrs.

      ELSE.
        PERFORM dynpro USING:
            'X' 'MP200000'        '2450',
            ' ' 'BDC_CURSOR'      'Q2010-EITXT',
            ' ' 'BDC_OKCODE'      '=PRIM',
            ' ' 'P2010-BEGDA'     begda,
            ' ' 'P2010-LGART'     ipa2010-lgart,
            ' ' 'P2010-ANZHL'     $anzhl,
            ' ' 'Q2010-EITXT'     'Hours',
            ' ' 'P2010-WAERS'     'USD'.

        PERFORM dynpro USING:
            'X' 'SAPLHRTV'        '0300',
            ' ' 'BDC_OKCODE'      '/EEDEL'.

      ENDIF.

      PERFORM dynpro USING:
            'X' 'MP200000'        '2450',
            ' ' 'BDC_OKCODE'      '=UPD',
            ' ' 'P2010-BEGDA'     begda,
            ' ' 'P2010-LGART'     ipa2010-lgart,
            ' ' 'P2010-ANZHL'     $anzhl,
            ' ' 'Q2010-EITXT'     'Hours',
            ' ' 'P2010-WAERS'     'USD'.
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

    gt_out-mode = ipa2010-mode.

* If ipa2010-rec initial, it is deleted record, append record
    IF NOT ipa2010-rec IS INITIAL.
* BEGIN OF UD1K952791
* Modify corresponding record in gt_out (#rec different after collect)
      MODIFY gt_out TRANSPORTING msg flg mode WHERE rec1 = ipa2010-rec.
      READ TABLE gt_out WITH KEY rec1 = ipa2010-rec.
*     MODIFY gt_out INDEX ipa2010-rec TRANSPORTING msg flg mode.
*     READ TABLE gt_out INDEX ipa2010-rec.
* END OF UD1K952791
    ELSE.
      gt_out-f1 = ipa2010-pernr.
      gt_out-f2 = ipa2010-begda.
      gt_out-f3 = ipa2010-anzhl.
      gt_out-f4 = ipa2010-lgart.
      gt_out-f6 = ipa2010-kostl.
      SHIFT gt_out-f3 LEFT DELETING LEADING space.
      APPEND gt_out TO gt_outdel.
    ENDIF.

    izthru014-pernr  = ipa2010-pernr.
    izthru014-begda  = ipa2010-begda.
    izthru014-anzhl  = ipa2010-anzhl.
    izthru014-lgart  = ipa2010-lgart.
    izthru014-skostl = gt_out-f5.
    izthru014-rkostl = gt_out-f6.
    izthru014-flag   = gt_out-flg.
    izthru014-msg    = gt_out-msg.
    izthru014-zuser  = sy-uname.
    izthru014-zsdat  = sy-datum.
    izthru014-zstim  = sy-uzeit.
    APPEND izthru014.
  ENDLOOP.
  FREE ipa2010.

  LOOP AT gt_out.
    CASE gt_out-flg.
      WHEN 'S'.
        gt_out-icon = icon_green_light.
      WHEN 'E'.
        gt_out-icon = icon_red_light.
      WHEN OTHERS.
        IF NOT p_post IS INITIAL.                           "UD1K952959
          gt_out-icon = icon_yellow_light.
          gt_out-msg = 'No Change Made'.
        ELSEIF NOT gt_out-chk IS INITIAL.                   "UD1K952959
          gt_out-icon = icon_yellow_light.                  "UD1K952959
          gt_out-msg = 'No Change Made'.                    "UD1K952959
        ENDIF.                                              "UD1K952959
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

  MODIFY zthru014 FROM TABLE izthru014.
  COMMIT WORK.
  FREE izthru014.

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
         EXPORTING is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant
         CHANGING  it_outtab            = gt_out[]
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
        l_rec      TYPE i.

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
                                gt_out-f6.
    CHECK NOT gt_out IS INITIAL.
    CLEAR: gt_out-f5, gt_out-f6.                            "UD1K952797
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
      window_title      = 'Select File Name'
      default_extension = 'csv'
      default_filename  = l_dfile
      file_filter       = 'CSV (*.csv)|*.csv| All (*.*)|*.*'
      initial_directory = '\\10.121.233.22\Data'
    CHANGING
      file_table        = lt_file
      rc                = l_rc
      user_action       = answer
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      OTHERS                  = 4
          .
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
  LOOP AT pt_rows WHERE rowtype IS initial.
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
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  PERFORM get_selected_data TABLES lt_rows
                                   lt_row_no .

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    gt_out-chk = true.                                      "UD1K952959
    MODIFY gt_out TRANSPORTING chk WHERE chk = space.       "UD1K952959

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
  DATA: l_rec TYPE i.

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
                                gt_out-f6.
    CHECK NOT gt_out IS INITIAL.
    l_rec = l_rec + 1.
    gt_out-rec = l_rec.
    CLEAR: gt_out-f5, gt_out-f6.                            "UD1K952797
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

* BEGIN OF UD1K952590
  CONCATENATE c_unix_pre sy-sysid c_unix_suf INTO it_filename-path.
  APPEND it_filename.
*  SELECT dirname
*    FROM user_dir
*    INTO TABLE it_filename
*   WHERE aliass = 'DIR_KRONOS'.
* END OF UD1K952590

  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_TEMP'
                     ID 'VALUE' FIELD it_filename-path.
  APPEND it_filename.

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
*&---------------------------------------------------------------------*
*&      Form  REFRESH_FIELD
*&---------------------------------------------------------------------*
*       Refresh for display
*----------------------------------------------------------------------*
FORM refresh_field.
  CALL METHOD g_grid->set_frontend_fieldcatalog
       EXPORTING
         it_fieldcatalog = gt_fcat.

  CALL METHOD g_grid->set_frontend_layout
       EXPORTING
         is_layout = gs_layo.

  __set_refresh_mode 'X'.

  CALL METHOD g_grid->refresh_table_display
              EXPORTING is_stable = stable.

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
        EXPORTING i_parent = g_custom_container.

ENDFORM.                    " CREATE_OBJECT

* For BDC
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

  DELETE FROM zthru014 WHERE begda <= l_deldate.
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
            (10) gt_out-icon AS ICON,       (50) gt_out-msg.
  ENDLOOP.

* Give space
  IF NOT gt_out[] IS INITIAL AND NOT gt_outdel[] IS INITIAL.
    SKIP.
  ENDIF.

* Write Deleted Records Details
  LOOP AT gt_outdel.
    ADD 1 TO d_cnt.

    ADD 1 TO t_cnt.
    WRITE :/(10) gt_outdel-f1, (10) gt_outdel-f2, (10) gt_outdel-f3,
            (10) gt_outdel-f4, (10) gt_outdel-f5, (10) gt_outdel-f6,
            (10) gt_outdel-icon AS ICON,          (50) gt_outdel-msg.
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
* BEGIN OF UD1K952791
*&---------------------------------------------------------------------*
*&      Form  APPEND_LOG
*&---------------------------------------------------------------------*
*       Append Execution Log
*----------------------------------------------------------------------*
FORM append_log.
  READ TABLE gt_out INDEX $gt_out-rec.

  izthru014-pernr  = gt_out-f1.
  izthru014-begda  = gt_out-f2.
  izthru014-anzhl  = gt_out-f3.
  izthru014-lgart  = gt_out-f4.
  izthru014-skostl = gt_out-f5.
  izthru014-rkostl = gt_out-f6.
  izthru014-flag   = gt_out-flg.
  izthru014-msg    = gt_out-msg.
  izthru014-zuser  = sy-uname.
  izthru014-zsdat  = sy-datum.
  izthru014-zstim  = sy-uzeit.
  APPEND izthru014.
ENDFORM.                    " APPEND_LOG
* END OF UD1K952791
