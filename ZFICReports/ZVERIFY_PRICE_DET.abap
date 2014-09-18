*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120061532 0000189004                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 46B          All Support Package Levels                   $*
*$------------------------------------------------------------------$*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS ZVERIFY_PRICE_DET
*& Object Header   PROG ZVERIFY_PRICE_DET
*&-------------------------------------------------------------------*
*& PROGRAM ZVERIFY_PRICE_DET
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
*Title   explanation tool (ZVERIFY_PRICE_DET)
*
*&---------------------------------------------------------------------*
*& Report  ZVERIFY_PRICE_DET
*&
*&---------------------------------------------------------------------*
*& explanation for price determination
*&
*&---------------------------------------------------------------------*
REPORT zverify_price_det
       MESSAGE-ID ckmlmv
       NO STANDARD PAGE HEADING
       LINE-SIZE 132.

TABLES:
  ckmlpp,
  ckmlcr.

TYPE-POOLS:
  ckmc.

include <icon>.

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

TYPES:
*  currencies
   BEGIN OF ty_curtp_str,
     curtp     LIKE ckmlcr-curtp,
     waers     LIKE ckmlcr-waers,
   END OF ty_curtp_str,
   ty_curtp_tbl     TYPE STANDARD TABLE OF ty_curtp_str,

   ty_anteil   TYPE f.

* extension to read ML documents
TYPES:
BEGIN OF ty_belpp_str,
  belnr     LIKE mlpp-belnr,
  kjahr     LIKE mlppf-kjahr,
  posnr     LIKE mlpp-posnr,
  bdatj     LIKE mlppf-bdatj,
  poper     LIKE mlppf-poper,
  ptyp      like mlit-ptyp,
  kategorie LIKE mlit-kategorie,
  feldg     LIKE mlppf-feldg,
  menge     LIKE mlppf-menge,
END OF ty_belpp_str.

TYPES:
BEGIN OF ty_belout_str,
  belnr     LIKE mlpp-belnr,
  kjahr     LIKE mlppf-kjahr,
  posnr     LIKE mlpp-posnr,
  kategorie LIKE mlit-kategorie,
  feldg     LIKE mlppf-feldg,
  menge     LIKE mlppf-menge,
  prdif     LIKE mlcrf-prdif,
  krdif     LIKE mlcrf-krdif,
END OF ty_belout_str.

TYPES:
BEGIN OF ty_belcr_str,
  belnr     LIKE mlpp-belnr,
  kjahr     LIKE mlppf-kjahr,
  posnr     LIKE mlpp-posnr,
  bdatj     LIKE mlppf-bdatj,
  poper     LIKE mlppf-poper,
  curtp     LIKE mlcrf-curtp,
  ptyp      like mlit-ptyp,
  kategorie LIKE mlit-kategorie,
  feldg     LIKE mlppf-feldg,
  prdif     LIKE mlcrf-prdif,
  krdif     LIKE mlcrf-krdif,
END OF ty_belcr_str.

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
  gd_ckmlcr_tbl    TYPE cki_t_ckmlcr,
  gd_wa_ckmlpp     LIKE ckmlpp,
  gd_wa_ckmlcr     LIKE ckmlcr,
  gd_kumbst        TYPE ckmc_f_kumulierter_bestand,
  gd_curtp_tbl     TYPE ty_curtp_tbl,
  gd_wa_curtp      TYPE LINE OF ty_curtp_tbl,
  gd_curtp_index   LIKE sy-tabix,
  gd_modus(2)      TYPE c,
  gd_curtp         LIKE ckmlcr-curtp,
  lh_ddtext        like ckmlcur-ddtext,
  gd_waers         like ckmlcr-waers,

* work areas (KB)
  cumulative_inventory        LIKE ckmlpp-abkumo,
  price_limiting_quantity     LIKE ckmlpp-abkumo,
  fraction_distr            TYPE ty_anteil,

  gd_pbprd                  LIKE ckmlcr-zuprd_o,
  gd_kbprd                  LIKE ckmlcr-zuprd_o,
  gd_bereinigt_prd          LIKE ckmlcr-zuprd_o,
  gd_hinzu_prd              LIKE ckmlcr-zuprd_o,
  gd_summe_prd              LIKE ckmlcr-zuprd_o,
  gd_not_distributed_prd    LIKE ckmlcr-zuprd_o,

  gd_kbkdm                  LIKE ckmlcr-zuprd_o,
  gd_pbkdm                  LIKE ckmlcr-zuprd_o,
  gd_bereinigt_kdm          LIKE ckmlcr-zuprd_o,
  gd_hinzu_kdm              LIKE ckmlcr-zuprd_o,
  gd_summe_kdm              LIKE ckmlcr-zuprd_o,
  gd_not_distributed_kdm    LIKE ckmlcr-zuprd_o,

  gd_prelim                   LIKE ckmlcr-salkv,
  gd_summe_delta              LIKE ckmlcr-salkv,
  gd_kb_gesamt                LIKE ckmlcr-salkv,
  gd_kb_menge                 LIKE ckmlpp-abkumo,
  gd_kb_preis                 LIKE ckmlcr-stprs,

* work areas (ZU)
  gd_zupopo                 LIKE ckmlpp-abkumo,

  gd_zukumo                 LIKE ckmlpp-abkumo,
  gd_zuprd                  LIKE ckmlcr-zuprd_o,
  gd_zupbprd                LIKE ckmlcr-zuprd_o,

  gd_zukdm                  LIKE ckmlcr-zuprd_o,
  gd_zupbkdm                LIKE ckmlcr-zuprd_o,

  gd_zu_gesamt              LIKE ckmlcr-salkv,
  gd_zu_menge               LIKE ckmlpp-abkumo,
  gd_zu_preis               LIKE ckmlcr-stprs,

  gd_multi_prd              LIKE ckmlcr-salkv,
  gd_multi_kdm              LIKE ckmlcr-salkv,

* not distributed (ZU)
  gd_anteil_eb              TYPE ty_anteil,
  gd_anteil_vn              TYPE ty_anteil,
  gd_vnkumo                 LIKE ckmlpp-vnkumo,
  gd_lbkum                  LIKE ckmlpp-lbkum,
  gd_summe_prd_eb           LIKE ckmlcr-zuprd_o,
  gd_summe_prd_vn           LIKE ckmlcr-zuprd_o,
  gd_summe_kdm_vn           LIKE ckmlcr-zuprd_o,
  gd_summe_kdm_eb           LIKE ckmlcr-zuprd_o,
  gd_multi_prd_eb           LIKE ckmlcr-zuprd_o,
  gd_multi_prd_vn           LIKE ckmlcr-zuprd_o,
  gd_multi_kdm_vn           LIKE ckmlcr-zuprd_o,
  gd_multi_kdm_eb           LIKE ckmlcr-zuprd_o.

* UG extension to read ML documents
DATA: gw_mlbelpp TYPE ty_belpp_str,
      gt_mlbelpp TYPE TABLE OF ty_belpp_str,

      gw_mlbelcr TYPE ty_belcr_str,
      gt_mlbelcr TYPE TABLE OF ty_belcr_str.


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

* read period data (amounts)
  SELECT * FROM ckmlcr INTO TABLE gd_ckmlcr_tbl
           WHERE kalnr   EQ p_kalnr
           AND   bdatj   EQ p_bdatj
           AND   poper   EQ p_poper
           AND   untper  EQ p_untper.

* no data selected
  IF sy-subrc NE 0.
    MESSAGE e154(c+).
  ENDIF.

* UG extension to read ML documents
  SELECT a~belnr a~kjahr a~posnr a~poper a~bdatj
         b~kategorie a~feldg a~menge
   INTO CORRESPONDING FIELDS OF TABLE gt_mlbelpp
   FROM mlppf AS a
   INNER JOIN mlit AS b ON b~belnr = a~belnr
                       AND b~kjahr = a~kjahr
                       AND b~posnr = a~posnr
   WHERE b~kalnr = p_kalnr
     AND a~bdatj = p_bdatj
     AND a~poper = p_poper.
  SORT gt_mlbelpp BY kategorie feldg belnr kjahr posnr bdatj poper.

* UG extension to read ML documents
  SELECT c~belnr c~kjahr c~posnr c~poper c~bdatj
         c~curtp b~kategorie c~feldg c~prdif c~krdif
   INTO CORRESPONDING FIELDS OF TABLE gt_mlbelcr
   FROM mlcrf AS c
   INNER JOIN mlit AS b ON b~belnr = c~belnr
                       AND b~kjahr = c~kjahr
                       AND b~posnr = c~posnr
   WHERE b~kalnr = p_kalnr
     AND c~bdatj = p_bdatj
     AND c~poper = p_poper.

  SORT gt_mlbelcr BY kategorie feldg belnr kjahr posnr bdatj poper.
  DELETE gt_mlbelpp WHERE kategorie eq space.
  DELETE gt_mlbelcr WHERE kategorie eq space.


*-----------------------------------------
* buld currency table
*-----------------------------------------
  LOOP AT gd_ckmlcr_tbl INTO gd_wa_ckmlcr.
    MOVE-CORRESPONDING gd_wa_ckmlcr TO gd_wa_curtp.
    READ TABLE gd_curtp_tbl TRANSPORTING NO FIELDS
         WITH KEY curtp = gd_wa_curtp-curtp.
    IF sy-subrc NE 0.
      APPEND gd_wa_curtp TO gd_curtp_tbl.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.

*----------------------------------------
* initialize first currency
*----------------------------------------
  READ TABLE gd_curtp_tbl INDEX 1
             INTO gd_wa_curtp.
  breakrc.
  gd_curtp_index = 1.
  gd_curtp = gd_wa_curtp-curtp.
  gd_waers = gd_wa_curtp-waers.
  SELECT SINGLE ddtext FROM ckmlcur INTO lh_ddtext
                       WHERE sprsl = sy-langu
                       AND   curtp = gd_curtp.

  PERFORM verify_kb_price.
  PERFORM write_desc.
AT USER-COMMAND.
  PERFORM process_user_command.

*eject
*----------------------------------------------------------------------*
*       FORM PROCESS_USER_COMMAND
*----------------------------------------------------------------------*
*       execute user command
*----------------------------------------------------------------------*
FORM process_user_command.

DATA:
   ld_fcode LIKE sy-ucomm.

*  Hotspot
   IF sy-ucomm EQ gd_fcode-detail.
     ld_fcode = hd_fcode.
   ELSE.
     ld_fcode = sy-ucomm.
   ENDIF.

   CASE ld_fcode.

*    Return to the selection screen
     WHEN gd_fcode-f03
       OR gd_fcode-f12
       OR gd_fcode-f15.
       SUBMIT (sy-repid) VIA SELECTION-SCREEN.

*    Next currency
     WHEN 'W+'.
       ADD 1 TO gd_curtp_index.
       READ TABLE gd_curtp_tbl INDEX gd_curtp_index
             INTO gd_wa_curtp.
       IF sy-subrc NE 0.
         READ TABLE gd_curtp_tbl INDEX 1
             INTO gd_wa_curtp.
         breakrc.
         gd_curtp_index = 1.
       ENDIF.
   ENDCASE.
   sy-lsind = sy-lsind - 1.
   gd_curtp = gd_wa_curtp-curtp.
   gd_waers = gd_wa_curtp-waers.
   SELECT SINGLE ddtext FROM ckmlcur INTO lh_ddtext
                       WHERE sprsl = sy-langu
                       AND   curtp = gd_curtp.
   PERFORM verify_kb_price.
ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM VERIFY_KB_PRICE
*----------------------------------------------------------------------*
*       explain price (cumulative inventory)
*----------------------------------------------------------------------*
FORM verify_kb_price.

  PERFORM icon_write.

* quantities
  cumulative_inventory =    gd_wa_ckmlpp-abkumo
               + gd_wa_ckmlpp-umkumo
               + gd_wa_ckmlpp-zukumo
               + gd_wa_ckmlpp-vpkumo.
  price_limiting_quantity  = gd_wa_ckmlpp-pbpopo.
  IF price_limiting_quantity NE 0.
    fraction_distr  = abs( cumulative_inventory ) / ABS(
price_limiting_quantity ).
    IF fraction_distr GT 1.
      fraction_distr = 1.
    ENDIF.
  ELSE.
    fraction_distr = 1.
  ENDIF.
  FORMAT INTENSIFIED OFF.
  PERFORM write_feld USING:
*            'GD_CURTP',
            'cumulative_inventory',
            'price_limiting_quantity',
            'fraction_distr'.
  ULINE.


* currencies
  LOOP AT gd_ckmlcr_tbl INTO gd_wa_ckmlcr
          WHERE curtp EQ gd_curtp.

*   determine cumulative inventory
    PERFORM ckmc_cumulative_stock USING
               gd_wa_ckmlpp
               gd_wa_ckmlcr
             CHANGING
               gd_kumbst.

    gd_kbprd  =   gd_wa_ckmlcr-abprd_o
                + gd_wa_ckmlcr-zuprd_o
                + gd_wa_ckmlcr-vpprd_o.

    gd_kbkdm  =   gd_wa_ckmlcr-abkdm_o
                + gd_wa_ckmlcr-zukdm_o
                + gd_wa_ckmlcr-vpkdm_o.

    gd_pbprd  =   gd_wa_ckmlcr-pbprd_o.
    gd_pbkdm  =   gd_wa_ckmlcr-pbkdm_o.

*   pricedifferences
    gd_bereinigt_prd  =   gd_kbprd
                        - gd_pbprd.

    gd_hinzu_prd      =   gd_pbprd
                        * fraction_distr.

    gd_summe_prd      =   gd_bereinigt_prd
                        + gd_hinzu_prd.

    gd_not_distributed_prd   =   gd_pbprd
                               - gd_hinzu_prd.

*   exchange rate differences
    gd_bereinigt_kdm  =   gd_kbkdm
                        - gd_pbkdm.

    gd_hinzu_kdm      =   gd_pbkdm
                        * fraction_distr.

    gd_summe_kdm      =   gd_bereinigt_kdm
                        + gd_hinzu_kdm.

    gd_not_distributed_kdm   =   gd_pbkdm
                               - gd_hinzu_kdm.
*   price determination
    gd_prelim = gd_kumbst-salkv.
    gd_prelim =   gd_wa_ckmlcr-stprs
                * gd_kumbst-kbkum
                / gd_wa_ckmlcr-peinh.

    gd_summe_delta =    gd_kumbst-kbprd_mo
                     +  gd_kumbst-kbkdm_mo.
    gd_kb_gesamt =   gd_prelim
                   + gd_summe_prd
                   + gd_summe_kdm
                   + gd_summe_delta.
    gd_kb_menge = gd_kumbst-kbkum.
    IF gd_kb_menge NE 0.
      gd_kb_preis = gd_kb_gesamt / gd_kb_menge.
    ELSE.
      CLEAR gd_kb_preis.
    ENDIF.

*   output
    PERFORM write_feld USING:
              'GD_KBPRD',
              'GD_PBPRD',
              'GD_BEREINIGT_PRD',
              'GD_HINZU_PRD',
              'GD_SUMME_PRD',
              'GD_NOT_DISTRIBUTED_PRD'.
    SKIP 1.
    PERFORM write_feld USING:
              'GD_KBKDM',
              'GD_PBKDM',
              'GD_BEREINIGT_KDM',
              'GD_HINZU_KDM',
              'GD_SUMME_KDM',
              'GD_NOT_DISTRIBUTED_KDM'.
    SKIP 1.
    PERFORM write_feld USING:
              'GD_SUMME_PRD',
              'GD_SUMME_KDM',
              'GD_PRELIM',
              'GD_SUMME_DELTA',
              'GD_KB_GESAMT',
              'GD_KB_MENGE'.
    SKIP 1.
    FORMAT INTENSIFIED ON.
    PERFORM write_feld USING:
              'GD_KB_PREIS'.
    ULINE.
  ENDLOOP.

* UG extension to read ML documents
  FORMAT INTENSIFIED OFF.
  PERFORM get_docs.
  ULINE.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM WRITE_FELD
*----------------------------------------------------------------------*
*       output
*----------------------------------------------------------------------*
FORM write_feld USING
       v_feld.

DATA:
  ld_type(1)     TYPE c,
  ld_wert(10)    TYPE p DECIMALS 3.

FIELD-SYMBOLS <f>.
 ASSIGN (v_feld) TO <f>.
 DESCRIBE FIELD <f> TYPE ld_type.
 IF ld_type EQ 'F'.
   ld_wert = <f>.
    WRITE: /001 v_feld,
            025 ld_wert.
 ELSE.
   WRITE: /001 v_feld,
           025 <f>.
 ENDIF.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM CKMC_CUMULATIVE_STOCK
*----------------------------------------------------------------------*
*       build cumulative stock
*----------------------------------------------------------------------*
FORM ckmc_cumulative_stock USING
       value(if_ckmlpp) LIKE  ckmlpp
       value(if_ckmlcr) LIKE  ckmlcr
     CHANGING
       value(ef_kumbstd) TYPE  ckmc_f_kumulierter_bestand.

  DATA:
    kbkumo     LIKE ckmlpp-lbkum,
    kbkumo_abs LIKE ckmlpp-lbkum,
    pbpopo_abs LIKE ckmlpp-lbkum,

    hf_kumbst_kbprd_old LIKE ckmlcr-zuprd_o,
    hf_kumbst_kbkdm_old LIKE ckmlcr-zuprd_o.


* cumulative quantitiy
  kbkumo =   if_ckmlpp-abkumo
           + if_ckmlpp-umkumo
           + if_ckmlpp-zukumo
           + if_ckmlpp-vpkumo.

* determine cumulative line---------------------------------------
  IF kbkumo EQ 0.
    CLEAR ef_kumbstd.
  ELSE.
    ef_kumbstd-kbkum = kbkumo.
    ef_kumbstd-kbprd_o =   if_ckmlcr-abprd_o
                         + if_ckmlcr-zuprd_o
                         + if_ckmlcr-vpprd_o.
    ef_kumbstd-kbkdm_o =   if_ckmlcr-abkdm_o
                         + if_ckmlcr-zukdm_o
                         + if_ckmlcr-vpkdm_o.
    ef_kumbstd-kbprd_mo =   if_ckmlcr-abprd_mo
                          + if_ckmlcr-zuprd_mo.
    ef_kumbstd-kbkdm_mo =   if_ckmlcr-abkdm_mo
                          + if_ckmlcr-zukdm_mo.

    kbkumo_abs = abs( ef_kumbstd-kbkum ).
    pbpopo_abs = abs( if_ckmlpp-pbpopo ).
    IF kbkumo_abs < pbpopo_abs.
      hf_kumbst_kbprd_old = ef_kumbstd-kbprd_o.
      hf_kumbst_kbkdm_old = ef_kumbstd-kbkdm_o.
*     remove prd from subsequent debits
      ef_kumbstd-kbprd_o =   ef_kumbstd-kbprd_o
                           - if_ckmlcr-pbprd_o.
*     fraction belonging to cumulative line
      ef_kumbstd-kbprd_o =   ef_kumbstd-kbprd_o
                           + (   if_ckmlcr-pbprd_o
                               * kbkumo_abs
                               / pbpopo_abs ).
*     dto. exchange rate differences
      ef_kumbstd-kbkdm_o =   ef_kumbstd-kbkdm_o
                           - if_ckmlcr-pbkdm_o.
      ef_kumbstd-kbkdm_o =   ef_kumbstd-kbkdm_o
                           + (   if_ckmlcr-pbkdm_o
                               * kbkumo_abs
                               / pbpopo_abs ).
    ENDIF.
    ef_kumbstd-salkv = (   if_ckmlcr-stprs
                         * ef_kumbstd-kbkum
                         / if_ckmlcr-peinh )
                       + ef_kumbstd-kbprd_o
                       + ef_kumbstd-kbkdm_o
                       + ef_kumbstd-kbprd_mo
                       + ef_kumbstd-kbkdm_mo.
  ENDIF.
ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM ICON_WRITE
*----------------------------------------------------------------------*
*       Icon to change between currencies
*----------------------------------------------------------------------*
FORM icon_write.

* Headerline
  WRITE: /001(003)  p_poper,
          004(001)  '/',
          005(004)  p_bdatj,
          010(004)  gd_ckmlhd-bwkey,
          014(001)  '/',
          015(020)  gd_ckmlhd-matnr.
  ULINE.
* switch currencies
  WRITE: /001(003)  icon_convert AS ICON HOTSPOT,
          010       'CURR/VALUATION'.
  WRITE: lh_ddtext, gd_waers.
  hd_fcode = 'W+'.
  HIDE    hd_fcode.
  ULINE.
ENDFORM.
*----------------------------------------------------------------------*
*       FORM EXFCODE_APPENDIEREN
*----------------------------------------------------------------------*
*       append excluding Table
*----------------------------------------------------------------------*
FORM exfcode_appendieren USING
       r_fcode    LIKE rsmpe-func.
  MOVE r_fcode  TO gd_exfcode_tbl-fcode.
  APPEND gd_exfcode_tbl.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_docs
*&---------------------------------------------------------------------*
* UG extension to read ML documents
FORM get_docs.
  DATA: lt_out TYPE TABLE OF ty_belout_str,
        lw_out TYPE ty_belout_str.

*first get quantities
  LOOP AT gt_mlbelpp INTO gw_mlbelpp
    WHERE feldg = 'VPO'
*       or feldg = 'PBO'
       OR feldg = 'PPO'.
*   temporary if-clause due to collection of data in category VP
*   in future I should only take PBO
    if gw_mlbelpp-feldg = 'VPO' AND
       gw_mlbelpp-ptyp ne 'VK'.
       continue.
    endif.
     IF gw_mlbelpp-menge NE 0.
      MOVE-CORRESPONDING gw_mlbelpp TO lw_out.
      APPEND lw_out TO lt_out.
     ENDIF.
  ENDLOOP.
*now get values
  LOOP AT gt_mlbelcr INTO gw_mlbelcr
    WHERE curtp = gd_curtp.
    CLEAR lw_out.
*   temporary if-clause due to collection of data in category VP
*   in future I should only take PBO
    if gw_mlbelcr-feldg = 'VPO' AND
       gw_mlbelcr-ptyp ne 'VK'.
       continue.
    endif.
    IF gw_mlbelcr-feldg = 'VPO' OR gw_mlbelcr-feldg = 'PBO'.
      READ TABLE lt_out WITH KEY belnr = gw_mlbelcr-belnr
                                 kjahr = gw_mlbelcr-kjahr
                                 posnr = gw_mlbelcr-posnr
                        INTO lw_out.
      IF sy-subrc EQ 0.
        lw_out-prdif = gw_mlbelcr-prdif.
        lw_out-krdif = gw_mlbelcr-krdif.
        MODIFY lt_out FROM lw_out INDEX sy-tabix.
      ELSE.
        MOVE-CORRESPONDING gw_mlbelcr TO lw_out.
        APPEND lw_out TO lt_out.
      ENDIF.
    ENDIF.
  ENDLOOP.
  SORT lt_out BY kategorie DESCENDING
                 feldg     ASCENDING
                 kjahr     ASCENDING
                 belnr     ASCENDING
                 posnr     ASCENDING.

  PERFORM write_out TABLES lt_out.
ENDFORM.                               "get_docs

*&---------------------------------------------------------------------*
*&      Form  write_out
*&---------------------------------------------------------------------*
* UG extension to read ML documents
FORM write_out TABLES lt_out.

  DATA: lw_out TYPE ty_belout_str,
        ld_zumng LIKE mlppf-menge,
        ld_zuprd LIKE mlcrf-prdif,
        ld_zukdm LIKE mlcrf-krdif,
        ld_vpmng LIKE mlppf-menge,
        ld_vpprd LIKE mlcrf-prdif,
        ld_vpkdm LIKE mlcrf-krdif,
        ld_tomng LIKE mlppf-menge,
        ld_toprd LIKE mlcrf-prdif,
        ld_tokdm LIKE mlcrf-krdif,

        ld_katchg LIKE mlit-kategorie.

  FORMAT COLOR 1 ON.
  WRITE: /001       'PRICE LIMITER RELEVANT ML DOCUMENTS',
         /001(012)  'DOCUMENT',
          015       'POSITION',
          026       'CATEGORY',
*           036       'FELDG',
          041       'QUANTITY',
          062       'PRICEDIF',
          085       'EXCHRATEDIF'.
  FORMAT COLOR OFF.
  CLEAR: ld_katchg, ld_zumng, ld_zuprd, ld_zumng,
         ld_vpmng, ld_vpprd, ld_vpkdm.
  LOOP AT lt_out INTO lw_out.
    IF lw_out-kategorie EQ 'ZU'.
      ADD:  lw_out-menge TO ld_zumng,
            lw_out-prdif TO ld_zuprd,
            lw_out-krdif TO ld_zukdm.
    ELSEIF lw_out-kategorie EQ 'VP'.
      ADD:  lw_out-menge TO ld_vpmng,
            lw_out-prdif TO ld_vpprd,
            lw_out-krdif TO ld_vpkdm.
    ENDIF.
    IF lw_out-kategorie NE ld_katchg AND
       ( NOT ld_katchg IS INITIAL ).
      FORMAT COLOR 7 ON.
      WRITE: /005 '=> SUBTOTAL ZU',
              030    ld_zumng,
              050    ld_zuprd,
              075    ld_zukdm.
      FORMAT COLOR OFF.
    ENDIF.
    WRITE: /001(012)  lw_out-belnr,
            015       lw_out-posnr,
            026       lw_out-kategorie,
*           036       'FELDG',
            030       lw_out-menge,
            050       lw_out-prdif,
            075       lw_out-krdif.
    ld_katchg = lw_out-kategorie.
  ENDLOOP.
*write last two lines...
  FORMAT COLOR 7 ON.
  IF ld_katchg EQ 'VP'.
    WRITE: /005 '=> SUBTOTAL VP',
            030    ld_vpmng,
            050    ld_vpprd,
            075    ld_vpkdm.
  ELSE.
    WRITE: /005 '=> SUBTOTAL ZU',
            030    ld_zumng,
            050    ld_zuprd,
            075    ld_zukdm.
  ENDIF.
  ld_tomng = ld_zumng + ld_vpmng.
  ld_toprd = ld_zuprd + ld_vpprd.
  ld_tokdm = ld_zukdm + ld_vpkdm.
  FORMAT COLOR 3 ON.
  WRITE: /005 '=> TOTAL',
          030    ld_tomng,
          050    ld_toprd,
          075    ld_tokdm.
  FORMAT COLOR OFF.
ENDFORM.                               " write_out

*>>>> END OF INSERTION <<<<<<
*&---------------------------------------------------------------------*
*&      Form  write_desc
*&---------------------------------------------------------------------*
FORM write_desc.
write:
/'GD_KBPRD: Entire price difference of the accumulated amount',
/'GD_PBPRD:Price diff of accumulated inv relevant for price limiter',
/'GD_BEREINIGT_PRD = GD_KBPRD - GD_PBPRD',
/'GD_HINZU_PRD: Portion of non-distributed price differences',
/'GD_SUMME_PRD = GD_BEREINIGT_PRD + GD_HINZU_PRD',
/'       => total of distributed price differences',
/'GD_NOT_DISTRIBUTED_PRD: Non-distributed price differences',
/'GD_KBKDM: as GD_KBPRD, however, exchange rate differences',
/'GD_PBKDM: as GD_PBPRD, however, exchange rate differences',
/'GD_BEREINIGT_KDM: as GD_BEREINIGT_PRD, however exchange rate',
/'GD_HINZU_KDM: as GD_HINZU_PRD, however exchange rate differences',
/'GD_SUMME_KDM: as GD_SUMME_PRD, however exchange rate differences',
/'GD_NOT_DISTRIBUTED_KDM: as GD_NOT_DISTRIBUTED_PRD, however Kursdiff.',
/'GD_SUMME_KDM: as GD_SUMME_PRD, however exchange rate differences',
/'GD_PRELIM: Preliminary valuation',
/'GD_SUMME_MULTI_DELTA: Total of multi-level differences',
/'GD_KB_GESAMT: Total value of the cumulative inventory',
/'GD_KB_MENGE: Total quantity of the cumulative inventory',
/'GD_KB_PREIS: Actual price of the cumulative inventory'.

ENDFORM.                    " write_desc
