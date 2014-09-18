************************************************************************
* Program Name      : ZAFIU137
* Author            : IG.Moon
* Creation Date     : 10/14/2009
* Specifications By : Michael Yoon, Changed by Andy Choi
* Description       : Maintain the AR Monthly Data
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
REPORT zafiu137 MESSAGE-ID zmco.

TABLES: ztfiu132, imak.

DATA  : gv_waers  TYPE waers,
        gv_cnt    TYPE i.

DATA  : BEGIN OF gt_itab OCCURS 0,
          posnr  LIKE imak-posnr,
          werks  LIKE imak-werks,
          vkostl LIKE imak-vkostl,
        END OF gt_itab.
RANGES: r_stratflg FOR imak-stratflg.
*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_bukrs LIKE bkpf-bukrs MEMORY ID buk OBLIGATORY,
            p_ayear TYPE ima_gjahr  MEMORY ID gjr OBLIGATORY,
            p_mvers LIKE imav-mvers,
            p_abp   TYPE im_stratflg.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
*ELECT-OPTIONS : s_prnam FOR ztfiu132-prnam.
SELECT-OPTIONS : s_posnr FOR ztfiu132-posnr,
*                 s_ivart for ztfiu131-ivart.
                 s_ivart  FOR imak-ivart,
*                 s_werks  FOR imak-werks,
                 s_vkostl FOR imak-vkostl.
SELECTION-SCREEN END OF BLOCK b2.

*selection-screen begin of block view-result with frame title text-t03.
*selection-screen pushbutton  1(34) timpr user-command timpr.
*SELECTION-SCREEN SKIP 1.
*selection-screen pushbutton  1(34) se16n user-command se16n.
*selection-screen end of block view-result.

PARAMETERS  p_call NO-DISPLAY.

SELECT SINGLE waers INTO gv_waers FROM t001
       WHERE bukrs = p_bukrs.
CHECK sy-subrc = 0.

IF p_abp = 'X'.
  r_stratflg-option = 'EQ'.
  r_stratflg-sign   = 'I'.
  r_stratflg-low = 'X'.
  APPEND r_stratflg.
ENDIF.


SELECT posnr werks vkostl INTO TABLE gt_itab
      FROM imak AS a
      WHERE a~posnr  IN s_posnr
        AND a~gjahr    = p_ayear
        AND a~stratflg IN r_stratflg
        AND a~ivart  IN s_ivart
*        AND a~werks  IN s_werks
        AND a~vkostl IN s_vkostl.
CHECK sy-subrc = 0.

*if monthly plan not exist, populate initial value
LOOP AT gt_itab.
  SELECT COUNT( * ) INTO gv_cnt FROM ztfiu132
        WHERE posnr   = gt_itab-posnr
          AND ayear   = p_ayear
          AND versi   = p_mvers.
  IF sy-subrc <> 0.
    ztfiu132-posnr  = gt_itab-posnr.
    ztfiu132-ayear  = p_ayear.
    ztfiu132-versi  = p_mvers.
    ztfiu132-gjahr  = p_ayear.
    ztfiu132-vkostl = gt_itab-vkostl.
    INSERT ztfiu132.
    COMMIT WORK.
  ENDIF.
ENDLOOP.

*show ALV editing screen
PERFORM show_se16n.

*&---------------------------------------------------------------------*
*&      Form  SHOW_SE16N
*&---------------------------------------------------------------------*
FORM show_se16n .
  DATA: lt_se16stab   LIKE se16n_seltab OCCURS 0 WITH HEADER LINE.
  DATA: lt_ofields    LIKE se16n_output OCCURS 0 WITH HEADER LINE.
  DATA: lt_se16event  TYPE TABLE OF se16n_events_type. " occurs 0.
  DATA: ls_se16event  TYPE se16n_events_type.


  REFRESH: lt_se16stab, lt_ofields.
  CLEAR lt_se16stab.

  CLEAR lt_se16stab.
  lt_se16stab-field  = 'AYEAR'.
  lt_se16stab-sign   = 'I'.
  lt_se16stab-option = 'EQ'.
  lt_se16stab-low    = p_ayear .
  APPEND lt_se16stab.

  lt_se16stab-field  = 'VERSI'.
  lt_se16stab-sign   = 'I'.
  lt_se16stab-option = 'EQ'.
  lt_se16stab-low    = p_mvers .
  APPEND lt_se16stab.

  LOOP AT gt_itab.
    lt_se16stab-field  = 'POSNR' .
    lt_se16stab-sign   = 'I'.  "s_posnr-sign.
    lt_se16stab-option = 'EQ'. "s_posnr-option.
    lt_se16stab-low    = gt_itab-posnr. "s_posnr-low.
    lt_se16stab-high   = ''.   "s_posnr-high.
    APPEND lt_se16stab.
  ENDLOOP.

* output fields
*  lt_ofields-FIELD  = 'AYEAR'.  append lt_ofields.

  lt_ofields-field  = 'POSNR'.  APPEND lt_ofields.
  lt_ofields-field  = 'GJAHR'.  APPEND lt_ofields.
  lt_ofields-field  = 'TOT'.    APPEND lt_ofields.
  lt_ofields-field  = 'WTP01'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP02'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP03'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP04'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP05'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP06'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP07'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP08'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP09'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP10'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP11'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP12'.  APPEND lt_ofields.
  lt_ofields-field  = 'TEXT'.   APPEND lt_ofields.

*  lt_ofields-FIELD  = 'VERSI'.  append lt_ofields.
*  lt_ofields-field  = 'VARNT'.  APPEND lt_ofields.
*  lt_ofields-field  = 'AKOSTL'. APPEND lt_ofields.
  lt_ofields-field  = 'VKOSTL'. APPEND lt_ofields.
*  lt_ofields-FIELD  = 'WAERS'.  append lt_ofields.

*call-back event - refer LSE16NF30
  ls_se16event-callback_program = sy-cprog.
  ls_se16event-callback_form    = 'SE16_CALLBACK'.
  ls_se16event-callback_event   = 'SAVE'.         "refer LSE16NTOP
  APPEND ls_se16event TO lt_se16event.

* copied from SE16N_INTERFACE
  CALL FUNCTION 'Z_SE16N_INTERFACE'
    EXPORTING
      i_tab                       = 'ZTFIU132'
      i_edit                      = 'X'
*   I_SAPEDIT                   = ' '
*   I_NO_TXT                    = 'X'
*   i_max_lines                 = 500
*   I_LINE_DET                  = ' '
*   I_DISPLAY                   = 'X'
*   I_CLNT_SPEZ                 = ' '
*   I_CLNT_DEP                  = ' '
*   I_VARIANT                   = ' '
*   I_OLD_ALV                   = ' '
*   I_CHECKKEY                  = ' '
*   I_TECH_NAMES                = ' '
*   I_CWIDTH_OPT_OFF            = ' '
*   I_SCROLL                    = ' '
*   I_NO_CONVEXIT               = ' '
*    i_layout_get                = 'X'
*   I_ADD_FIELD                 =
*   I_ADD_FIELDS_ON             =
*   I_UNAME                     =
* IMPORTING
*   E_LINE_NR                   =
*   E_DREF                      =
   TABLES
     it_selfields                = lt_se16stab
     it_output_fields            = lt_ofields
*   IT_OR_SELFIELDS             =
     it_callback_events          = lt_se16event
*   IT_ADD_UP_CURR_FIELDS       =
*   IT_ADD_UP_QUAN_FIELDS       =
* EXCEPTIONS
*   NO_VALUES                   = 1
*   OTHERS                      = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " SHOW_SE16N
*&---------------------------------------------------------------------*
* call back form from SE16N
*&---------------------------------------------------------------------*
FORM se16_callback USING   p_exit_point
                           p_add_info
                           p_tab
                  CHANGING p_tabref.

  FIELD-SYMBOLS: <wa> TYPE ztfiu132.
  FIELD-SYMBOLS: <field>.

  ASSIGN p_tabref->* TO <wa>.
  ASSIGN COMPONENT 'UNAME' OF STRUCTURE <wa> TO <field>.
  <field> = sy-uname.
  ASSIGN COMPONENT 'DDATE' OF STRUCTURE <wa> TO <field>.
  <field> = sy-datum.

  ASSIGN COMPONENT 'WAERS' OF STRUCTURE <wa> TO <field>.
  <field> = gv_waers.

*FIXME why not calculation?
  ASSIGN COMPONENT 'TOT' OF STRUCTURE <wa> TO <field>.
  <field> = <wa>-wtp01 + <wa>-wtp02 + <wa>-wtp03 +
            <wa>-wtp04 + <wa>-wtp05 + <wa>-wtp06 +
            <wa>-wtp07 + <wa>-wtp08 + <wa>-wtp09 +
            <wa>-wtp10 + <wa>-wtp11 + <wa>-wtp12.

* process data for insert/modify/delete record
  CASE p_add_info.
    WHEN 'MODIFY'.

* if insert, default value...
    WHEN 'INSERT'.
      if <wa>-gjahr is initial.
        message e000 with 'Please enter year'.
      endif.

      <wa>-ayear = p_ayear.
      <wa>-versi = p_mvers.

    WHEN 'DELETE'.

  ENDCASE.

ENDFORM.                    "SE16_CALLBACK
