************************************************************************
*  OP-01: Note 309573                                                  *
************************************************************************


REPORT ZR01FIR_USTAX NO STANDARD PAGE HEADING LINE-SIZE 80 MESSAGE-ID fr
.

TABLES: kna1, lfa1, bkpf, bseg, bset,
        konp, t001, t005, t007a, t007b, t007s, ttxd, ttxjt, bhdgd.

RANGES: range_ktosl FOR t007b-ktosl.

DATA: gs_kna1 TYPE kna1.                                    "OP-01
DATA: gs_lfa1 TYPE lfa1.                                    "OP-01

DATA:   BEGIN OF x005 OCCURS 10,
          land1 LIKE t005-land1,
          kalsm LIKE t005-kalsm,
          waers LIKE t005-waers,
        END OF x005.

DATA:   BEGIN OF x007 OCCURS 20,
          kalsm LIKE t007a-kalsm,
          mwskz LIKE t007a-mwskz,
          mwart LIKE t007a-mwart,
          text1 LIKE t007s-text1,
        END OF x007.

DATA:   BEGIN OF xtxd OCCURS 5,
          kalsm LIKE ttxd-kalsm,
        END OF xtxd.

DATA:   BEGIN OF xtxjt OCCURS 20.
        INCLUDE STRUCTURE ttxjt.
DATA:   END OF xtxjt.

DATA:   BEGIN OF xbset OCCURS 10.
        INCLUDE STRUCTURE bset.
DATA:   END OF xbset.

DATA:   BEGIN OF ybset OCCURS 10.
        INCLUDE STRUCTURE bset.
DATA:   END OF ybset.

DATA:   kbetr TYPE p,
        ktonr LIKE lfa1-lifnr,
        name LIKE kna1-name1,
        land1 like lfa1-land1,
        REGIO like lfa1-REGIO,
        save_bukrs LIKE t001-bukrs,
        flag.


DATA: x_save,  "for parameter I_SAVE: modus for saving a layout
      x_layout TYPE disvariant,
        g_exit TYPE c,
      spec_layout TYPE disvariant.     "specific layout


begin_of_block 1.
SELECT-OPTIONS sel_ju FOR bset-txjcd.
SELECT-OPTIONS sel_tc FOR bset-mwskz.
end_of_block 1.

begin_of_block 2.
*PARAMETERS: header AS CHECKBOX DEFAULT 'X'.
PARAMETERS: header like rfpdo-headflag,
* PARAMETERS: listsep LIKE rfpdo-allglsep.
   p_vari    like disvariant-variant.
end_of_block 2.
data: listsep type rfpdo-allglsep.

* ALV and more
INCLUDE ZI_RFUTAX00_ALV.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**************************


*INCLUDE ztenn_rfutax_include_2.

INITIALIZATION.
  get_frame_title: 1, 2.


  clear x_layout.
  x_layout-report = sy-repid.

at selection-screen on value-request for p_vari.
  data: rs_variant like disvariant,
        nof4 type c.

  clear nof4.
  loop at screen.
    if screen-name = 'P_VARI'.
      if screen-input = 0.
        nof4 = 'X'.
      endif.
    endif.
  endloop.
  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

call function 'REUSE_ALV_VARIANT_F4'
       exporting
            is_variant = rs_variant
            i_save     = 'A'
       importing
            es_variant = rs_variant
       exceptions
            others     = 1.
  if sy-subrc = 0 and nof4 eq space.
    p_vari = rs_variant-variant.

  endif.



START-OF-SELECTION.
  PERFORM init_tables.
  MOVE: '0'     TO bhdgd-inifl,
       sy-linsz TO bhdgd-lines,     "Zeilenbreite der Liste (Reportanw.)
       sy-uname TO bhdgd-uname,     "Benutzername (SAP-Anmeldung)
       sy-repid TO bhdgd-repid,     "Name des ABAP-Programms
       sy-title TO bhdgd-line1,     "Titel des ABAP-Programms
       space    TO bhdgd-miffl,     "Mikro-Fiche Information
       listsep  TO bhdgd-separ,     "Listseparation
       'BUKRS'  TO bhdgd-domai.

GET bkpf.
  PERFORM read_tax_procedure.
  ktonr =  space.
  flag = space.

GET bseg.
  CHECK bseg-koart = 'D' or bseg-koart = 'K'.
  flag = 'X'.
  IF bseg-koart = 'D'.
*   A/R

    IF ktonr = space.
      ktonr = bseg-kunnr.
*      SELECT SINGLE * FROM kna1 WHERE kunnr = ktonr.
*      name = kna1-name1.
      CALL FUNCTION 'KNA1_READ_SINGLE'
           EXPORTING
                id_kunnr = ktonr
           IMPORTING
                es_kna1  = gs_kna1
           EXCEPTIONS
                OTHERS   = 1.                               "OP-01
      IF sy-subrc IS INITIAL.                               "OP-01
        name =  gs_kna1-name1.                              "OP-01
        land1 =  gs_kna1-land1.
        regio = gs_kna1-regio.
      ELSE.                                                 "OP-01
        CLEAR: name,land1,regio.                            "OP-01
      ENDIF.                                                "OP-01
    ELSE.
      IF ktonr <> bseg-kunnr.
*       different account numbers in document
        ktonr = text-010.
      ENDIF.
    ENDIF.
  ELSE.
*   A/P

    IF ktonr = space.
      ktonr = bseg-lifnr.
*      SELECT SINGLE * FROM lfa1 WHERE lifnr = ktonr.
*      name = lfa1-name1.
      CALL FUNCTION 'LFA1_READ_SINGLE'                      "OP01
             EXPORTING
                  id_lifnr            = ktonr
             IMPORTING
                  es_lfa1             = gs_lfa1
             EXCEPTIONS
                  OTHERS              = 1.                  "OP-01
      IF sy-subrc IS INITIAL.                               "OP-01
        name = gs_lfa1-name1.                               "OP-01
        land1 = gs_lfa1-land1.
        regio = gs_lfa1-regio.
      ELSE.                                                 "OP-01
        CLEAR: name,land1,regio.                            "OP-01
      ENDIF.                                                "OP-01

    ELSE.
      IF ktonr <> bseg-lifnr.
*       different account numbers in document
        ktonr = text-010.
      ENDIF.
    ENDIF.
  ENDIF.

GET bset.
  CHECK flag = 'X'.
  CHECK SELECT-OPTIONS.
  xbset = bset.
  APPEND xbset.

GET bkpf LATE.
  DESCRIBE TABLE xbset LINES sy-tabix.
  CHECK sy-tabix <> 0.
  copy xbset to ybset.
  LOOP AT xbset.
    CHECK xbset-ktosl in range_ktosl.
*   read X007-MWART for extract
    x007 = space. x007-kalsm = x005-kalsm. x007-mwskz = xbset-mwskz.
    READ TABLE x007.
    kbetr = xbset-kbetr.
    IF kbetr = 0.
*     KBETR not set in BSET -> access KONP
      SELECT SINGLE * FROM konp
        WHERE knumh = xbset-knumh
        AND   kopos = '01'.
      kbetr = konp-kbetr.
    ENDIF.
*   special treatment for -100%
    IF kbetr = '100000-'.
      LOOP AT ybset WHERE txjcd = xbset-txjcd AND
                          txgrp = xbset-txgrp AND
                          txjdp = xbset-txjdp AND           "492920
                          hwbas <> xbset-hwbas.
        EXIT.
      ENDLOOP.
      xbset-hwbas = ybset-hwbas.
    ENDIF.
    xbset-hwbas = ABS( xbset-hwbas ).
    IF xbset-shkzg = 'H'.
      xbset-hwbas = 0 - xbset-hwbas.
      xbset-hwste = 0 - xbset-hwste.
    ENDIF.
    PERFORM append_data.
  ENDLOOP.
  CLEAR:   xbset, ybset.
  REFRESH: xbset, ybset.

END-OF-SELECTION.

  PERFORM print_list.


AT LINE-SELECTION.
  IF bkpf-bukrs <> space.
    SET PARAMETER ID 'BUK' FIELD bkpf-bukrs.
    SET PARAMETER ID 'BLN' FIELD bkpf-belnr.
    SET PARAMETER ID 'GJR' FIELD bkpf-gjahr.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    bkpf-bukrs = space.
  ENDIF.


*---------------------------------------------------------------------*
*       FORM INIT_TABLES                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM init_tables.
  SELECT * FROM t005.
    MOVE-CORRESPONDING t005 TO x005.
    APPEND x005.
  ENDSELECT.
  SELECT * FROM t007a.
    MOVE-CORRESPONDING t007a TO x007.
    SELECT SINGLE * FROM t007s WHERE spras = sy-langu AND
                                     kalsm = t007a-kalsm AND
                                     mwskz = t007a-mwskz.
    IF sy-subrc = 0.
      x007-text1 = t007s-text1.
    ELSE.
      x007-text1 = space.
    ENDIF.
    APPEND x007.
  ENDSELECT.
  SELECT * FROM ttxd.
    MOVE-CORRESPONDING ttxd TO xtxd.
    APPEND xtxd.
  ENDSELECT.
  SELECT * FROM ttxjt.
    MOVE-CORRESPONDING ttxjt TO xtxjt.
    APPEND xtxjt.
  ENDSELECT.
  range_ktosl-option = 'EQ'. range_ktosl-sign = 'I'.
  SELECT * FROM t007b WHERE stbkz = '2'.
    range_ktosl-low = t007b-ktosl.
    APPEND range_ktosl.
  ENDSELECT.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM READ_TAX_PROCEDURE                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM read_tax_procedure.
  IF save_bukrs = t001-bukrs.
    EXIT.
  ENDIF.
  save_bukrs = t001-bukrs.
  READ TABLE x005 WITH KEY t001-land1.
  READ TABLE xtxd WITH KEY x005-kalsm.
  IF sy-subrc <> 0.
*   meSSAGE e334 WITH t001-bukrs. "kein Jurisdiction für Buchungskreis
  ENDIF.
ENDFORM.
