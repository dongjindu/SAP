************************************************************************
* Program Name      : ZPPR102_DISCTN_MAT
* Creation Date     : 03/24/2006
* Developer         : Furong Wang
* Development Request No :
* Addl Documentation:
* Description       : Provide specific info for discontinued mat'l
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zppr102_disctn_mat NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmpp.

TYPE-POOLS slis .
TABLES: lfa1, marc, mdsm.

DATA : BEGIN OF it_tab OCCURS 0,
      matnr LIKE mara-matnr,
      dispo LIKE marc-dispo,
      lifnr LIKE lfa1-lifnr,
      bdter LIKE resb-bdter,
      nfmat LIKE marc-nfmat,
      ausdt like marc-ausdt,
      plnum like mdsm-plnum,
      END OF it_tab.

DATA : BEGIN OF it_MARC_eina OCCURS 0,
      matnr LIKE mara-matnr,
      dispo LIKE marc-dispo,
      lifnr LIKE lfa1-lifnr,
      nfmat LIKE marc-nfmat,
      ausdt like marc-ausdt,
      END OF it_marc_eina.

DATA : BEGIN OF it_resb_mdsm OCCURS 0,
      matnr LIKE mara-matnr,
      bdter LIKE resb-bdter,
      plnum like mdsm-plnum,
      END OF it_resb_mdsm.

DATA: ok_code      LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt       TYPE   i.

DATA:  l_kalid LIKE kako-kalid.

DATA: it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
      it_fieldcat_fi  TYPE lvc_t_fcat WITH HEADER LINE,
      it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
      it_fieldname    TYPE slis_t_fieldcat_alv,
      it_sort         TYPE lvc_t_sort WITH HEADER LINE,
      it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA: wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
      w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

*ALV
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

SELECT-OPTIONS : s_lifnr FOR lfa1-lifnr,
                 s_matnr FOR marc-matnr,
*                 s_matkl FOR mara-matkl,
                 s_dispo FOR marc-dispo,
                 s_bdter FOR mdsm-bdter.
SELECTION-SCREEN END OF BLOCK block1.

selection-screen begin of block block2 with frame title text-003.

selection-screen comment 1(70) text-004.
selection-screen end of block block2.


AT SELECTION-SCREEN OUTPUT.
*  PERFORM check_screen.

AT SELECTION-SCREEN.
*  PERFORM check_input_value.

**---
INITIALIZATION.

***---
*TOP-OF-PAGE.
*  PERFORM top_of_page.
**---

START-OF-SELECTION.
  PERFORM get_data.

  IF it_tab[] IS INITIAL.
    MESSAGE i000 WITH text-m01.
  ELSE.
    PERFORM display_data.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.

** added by furong check s_matnr
  IF s_matnr-low IS INITIAL AND s_matnr-high IS INITIAL.
    s_matnr-high = 'ZZZZZZZ'.
  ELSEIF s_matnr-high IS INITIAL.
    s_matnr-high = s_matnr-low.
  ENDIF.

  IF s_dispo-low IS INITIAL AND s_dispo-high IS INITIAL.
    s_dispo-high = 'ZZZ'.
  ELSEIF s_dispo-high IS INITIAL.
    s_dispo-high = s_dispo-low.
  ENDIF.

** end of addition
ENDFORM.                    " check_input_value
*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_screen.
  LOOP AT SCREEN.
    IF screen-name = 'P_EXCEL'.
      screen-input = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_screen
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  SELECT a~matnr a~dispo b~lifnr  a~nfmat
         a~ausdt
   INTO CORRESPONdING FIELDS OF TABLE it_marc_eina
   FROM marc AS a
   INNER JOIN eina AS b
   ON a~matnr = b~matnr
   WHERE a~matnr IN s_matnr
     AND a~dispo IN s_dispo
     and a~kzaus <> SPACE
     AND b~loekz = space
     AND b~lifnr IN s_lifnr.

  SELECT matnr bdter plnum into table it_resb_mdsm
    FROM MDSM
    for all entries in it_marc_eina
     WHERE MATNR = IT_marc_eina-MATNR
     AND nafkz = 'X'
     AND bdter IN s_bdter.

  SELECT matnr bdter plnum appending table it_resb_mdsm
    FROM resb
    for all entries in it_marc_eina
     WHERE MATNR = IT_marc_eina-MATNR
     AND nafkz = 'X'
     AND bdter IN s_bdter.


*  SELECT a~matnr a~dispo b~lifnr c~bdter a~nfmat
*         a~ausdt c~plnum
*   INTO TABLE it_tab
*   FROM marc AS a
*   INNER JOIN eina AS b
*   ON a~matnr = b~matnr
*   INNER JOIN mdsm AS c
*   ON a~matnr = c~matnr
*   WHERE a~matnr IN s_matnr
*     AND a~dispo IN s_dispo
*     AND b~loekz = space
*     AND b~lifnr IN s_lifnr
*     AND c~nafkz = 'X'
*     AND c~bdter IN s_bdter.
*
*  SELECT a~matnr a~dispo b~lifnr c~bdter a~nfmat
*         a~ausdt c~plnum
*  APPENDING TABLE it_tab
*  FROM marc AS a
*  INNER JOIN eina AS b
*  ON a~matnr = b~matnr
*  INNER JOIN resb AS c
*  ON a~matnr = c~matnr
* WHERE a~matnr IN s_matnr
*    AND a~dispo IN s_dispo
*    AND b~loekz = space
*    AND b~lifnr IN s_lifnr
*    AND c~nafkz = 'X'
*    AND c~bdter IN s_bdter.
*
 delete adjacent duplicates from it_marc_eina.
 delete adjacent duplicates from it_resb_mdsm.

 loop at it_marc_eina.
   read table it_resb_mdsm with key matnr = it_marc_eina-matnr.
   it_tab-matnr = it_marc_eina-matnr.
   it_tab-dispo = it_marc_eina-dispo.
   it_tab-lifnr = it_marc_eina-lifnr.
   it_tab-bdter = it_resb_mdsm-bdter.
   it_tab-nfmat = it_marc_eina-nfmat.
   it_tab-ausdt = it_marc_eina-ausdt.
   it_tab-plnum = it_resb_mdsm-plnum.
   append it_tab.
   clear: it_tab, it_marc_eina, it_resb_mdsm.
 endloop.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  SORT it_tab BY matnr dispo lifnr.

  PERFORM field_setting TABLES gt_fieldcat USING :
   'MATNR'  'Material'         '18' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'DISPO' 'MRP Controller'   '12' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'LIFNR' 'Vendor'           '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'BDTER' 'Disc. Date'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'NFMAT' 'Follow up Material'  '22' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'AUSDT' 'Eff Out Date'     '12' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'PLNUM' 'Plan order'       '12' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  PERFORM field_sort.

  g_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            it_sort            = gt_sorts[]
            i_save             = 'A'
       TABLES
            t_outtab           = it_tab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.

ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  field_setting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_0376   text
*      -->P_0377   text
*      -->P_0378   text
*      -->P_0379   text
*      -->P_0380   text
*      -->P_0381   text
*      -->P_0382   text
*      -->P_0383   text
*      -->P_0384   text
*      -->P_0385   text
*----------------------------------------------------------------------*
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat USING
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.                    " fill_field_category
*&---------------------------------------------------------------------*
*&      Form  field_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM field_sort.
  gt_sorts-spos = 1.
  gt_sorts-fieldname = 'MATNR'.
  gt_sorts-up = 'X'.
  APPEND gt_sorts.
ENDFORM.                    " field_sort
