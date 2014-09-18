*&---------------------------------------------------------------------*
*& Report  ZREMOVE_PRICE_LIMITER                                       *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZREMOVE_PRICE_LIMITER
        MESSAGE-ID ckmlmv
        NO STANDARD PAGE HEADING
        LINE-SIZE 132.

TABLES:
  ckmlpp.

TYPE-POOLS:
  ckmc.

*include <icon>.

*---------------------------------------------------------------------*
*       Selections                                                    *
*---------------------------------------------------------------------*

PARAMETERS:
   p_kalnr  LIKE ckmlhd-kalnr  no-display,
   p_matnr  LIKE ckmlhd-matnr  MEMORY ID mat,
   p_bwkey  LIKE ckmlhd-bwkey  MEMORY ID wrk,
   p_bwtar  LIKE ckmlhd-bwtar  MEMORY ID bwt,
   p_vbeln  LIKE ckmlhd-vbeln  MEMORY ID aun,
   p_posnr  LIKE ckmlhd-posnr,
   p_pspnr  LIKE ckmlhd-pspnr  MEMORY ID see,
   p_bdatj  LIKE ckmlpp-bdatj  MEMORY ID mlb,
   p_poper  LIKE ckmlpp-poper  MEMORY ID mlp,
   p_untper LIKE ckmlpp-untper DEFAULT '000' no-display.
selection-screen skip 1.
parameters:
   p_npopo like ckmlpp-pbpopo,
   p_cpopo type c as checkbox.

DATA:
* Excluding-Table for Fcodes
  BEGIN OF gd_exfcode_tbl OCCURS 0,
    fcode   LIKE rsmpe-func,
  END OF gd_exfcode_tbl,

* Hide-Variables
  hd_fcode        LIKE sy-ucomm,

* Fcodes
  BEGIN OF gd_fcode,
    detail        LIKE sy-ucomm VALUE '&IC1',
    f03           LIKE sy-ucomm VALUE '&F03',
    f12           LIKE sy-ucomm VALUE '&F12',
    f15           LIKE sy-ucomm VALUE '&F15',
  END OF gd_fcode,

* DB Extracts
  gd_ckmlhd        LIKE ckmlhd,
  gd_ckmlpp_tbl    TYPE cki_t_ckmlpp,
  gd_wa_ckmlpp     LIKE ckmlpp,
  status(40)       type c,
  gd_pbpopo        LIKE ckmlpp-abkumo.

at selection-screen.

* check authority
  AUTHORITY-CHECK OBJECT 'K_ML_VA'
           ID 'ACTVT' FIELD '02'
           ID 'BWKEY' FIELD p_bwkey.

  IF sy-subrc ne 0.
    MESSAGE e054 WITH p_bwkey.
* Execution not allowed in this valuation area
  ENDIF.

START-OF-SELECTION.

*-----------------------------------------------------------
* Excluding-Table
*-----------------------------------------------------------
  CLEAR gd_exfcode_tbl[].
  PERFORM exfcode_appendieren USING:
            '%PC',
            '&XXL',
            '&AQW',
            '&ALL',
            '&SAL',
            '&NFO',
            '&NTE',
            '&RNT',
            '&ABC',
            '&UMC',
            '%SL',
            '&SUM',
            '&CRL',
            '&CRR',
            '&CRE',
            '&CRB',
            'AUFT',
            '&OL0',
            '&OAD',
            '&LFO',
            '&AVE',
            '&EB3',
            '&ILT',
            '&ETA',
            '&ODN',
            '&OUP'.

*-------------------------------------
* set status
*-------------------------------------
  SET PF-STATUS 'STATUS_LIST' OF PROGRAM 'CKMLMV_ORDER_LIST'
      EXCLUDING gd_exfcode_tbl.

*---------------------------------------
* Selection
*---------------------------------------

* determine calculation number
  IF p_kalnr IS INITIAL.

*   .. from EBEW
    IF NOT p_vbeln IS INITIAL AND
       NOT p_posnr IS INITIAL.
      SELECT SINGLE kaln1 INTO p_kalnr
             FROM ebew
             WHERE bwkey EQ p_bwkey
             AND   matnr EQ p_matnr
             AND   bwtar EQ p_bwtar
             AND   vbeln EQ p_vbeln
             AND   posnr EQ p_posnr.

*   .. from QBEW
    ELSEIF NOT p_pspnr IS INITIAL.
      SELECT SINGLE kaln1 INTO p_kalnr
             FROM qbew
             WHERE bwkey EQ p_bwkey
             AND   matnr EQ p_matnr
             AND   bwtar EQ p_bwtar
             AND   pspnr EQ p_pspnr.

    ELSE.
      SELECT SINGLE kaln1 INTO p_kalnr
             FROM mbew
             WHERE bwkey EQ p_bwkey
             AND   matnr EQ p_matnr
             AND   bwtar EQ p_bwtar.
    ENDIF.
  ELSE.
    SELECT SINGLE kalnr INTO p_kalnr
           FROM ckmlhd
           WHERE kalnr EQ p_kalnr.
  ENDIF.
* no materials selected
  IF sy-subrc NE 0.
    MESSAGE e114(c+).
  ENDIF.
  SELECT SINGLE * FROM ckmlhd
         INTO gd_ckmlhd
         WHERE kalnr EQ p_kalnr.
  breakrc.

* reas period data (quantites)
  SELECT * FROM ckmlpp INTO TABLE gd_ckmlpp_tbl
           WHERE kalnr   EQ p_kalnr
           AND   bdatj   EQ p_bdatj
           AND   poper   EQ p_poper
           AND   untper  EQ p_untper.

* not data selected
  IF sy-subrc NE 0.
    MESSAGE e154(c+).
  ENDIF.
  READ TABLE gd_ckmlpp_tbl INTO gd_wa_ckmlpp INDEX 1.
  breakrc.
  gd_pbpopo = gd_wa_ckmlpp-pbpopo.

move 'Database has not been updated' to status.

if not p_cpopo is initial.
   gd_wa_ckmlpp-pbpopo = p_npopo.
   update ckmlpp from gd_wa_ckmlpp.
   move 'Database has been updated' to status.
endif.

write: /001  'Former price limiting quantitiy',
        040  gd_pbpopo,
       /001  'New price limiting quantitiy',
        040  p_npopo.
write: /001  status.

AT USER-COMMAND.
  PERFORM process_user_command.

FORM exfcode_appendieren USING
       r_fcode    LIKE rsmpe-func.
  MOVE r_fcode  TO gd_exfcode_tbl-fcode.
  APPEND gd_exfcode_tbl.
ENDFORM.

FORM process_user_command.

DATA:
   ld_fcode LIKE sy-ucomm.

   ld_fcode = sy-ucomm.
   CASE ld_fcode.

*    Return to the selection screen
     WHEN gd_fcode-f03
       OR gd_fcode-f12
       OR gd_fcode-f15.
       SUBMIT (sy-repid) VIA SELECTION-SCREEN.
   ENDCASE.
ENDFORM.




