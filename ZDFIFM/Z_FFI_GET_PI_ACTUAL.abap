FUNCTION z_ffi_get_pi_actual.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(POSID) LIKE  IMPR-POSID
*"     REFERENCE(IPPOS) LIKE  IMZO-IPPOS DEFAULT ' '
*"     REFERENCE(AUFNR) LIKE  AUFK-AUFNR DEFAULT ''
*"  EXPORTING
*"     REFERENCE(AMT) LIKE  COSP-WTG001
*"  TABLES
*"      OUT STRUCTURE  ZFI_PI_ACTUAL
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------
  RANGES: r_wrttp FOR cosp-wrttp.
  DATA : BEGIN OF it_impr OCCURS 0,
             posnr LIKE impr-posnr,
             gjahr LIKE impr-gjahr,
         END OF it_impr.

  DATA : BEGIN OF it_imzo OCCURS 0,
             objnr LIKE imzo-objnr,
             gjahr LIKE imzo-gjahr,
             ippos LIKE imzo-ippos,
             baprz LIKE imzo-baprz,
             prozu LIKE imzo-prozu,
         END OF it_imzo.

  DATA : BEGIN OF it_aufk OCCURS 0,
             aufnr LIKE aufk-aufnr,
             objnr LIKE aufk-objnr,
         END OF it_aufk.
*------
  DATA : BEGIN OF it_rate OCCURS 0,
             aufnr LIKE aufk-aufnr,
             prozu LIKE imzo-prozu,
         END OF it_rate.
  DATA : it_tot LIKE zfi_pi_actual OCCURS 0 WITH HEADER LINE.
*-------ACTUAL MONTHLY-----------*
  DATA : it_cosp LIKE cosp OCCURS 0 WITH HEADER LINE.
*  data: it_cosp like ZFI_IO_ACTUAL occurs 0 with header line.
*-------
  DATA : wa_posnr LIKE impr-posnr,
         wa_cnt TYPE i,
         wa_ok,
         wa_aufnr LIKE aufk-aufnr,
         wa_baprz LIKE imzo-baprz,
         wa_tot(14) TYPE p DECIMALS 2, "   LIKE zfi_pi_actual,
         wa_wtg001(14) TYPE p DECIMALS 2, "   LIKE zfi_pi_actual,
         wa_wtg002(14) TYPE p DECIMALS 2, "   LIKE zfi_pi_actual,
         wa_wtg003(14) TYPE p DECIMALS 2, "   LIKE zfi_pi_actual,
         wa_wtg004(14) TYPE p DECIMALS 2, "   LIKE zfi_pi_actual,
         wa_wtg005(14) TYPE p DECIMALS 2, "   LIKE zfi_pi_actual,
         wa_wtg006(14) TYPE p DECIMALS 2, "   LIKE zfi_pi_actual,
         wa_wtg007(14) TYPE p DECIMALS 2, "   LIKE zfi_pi_actual,
         wa_wtg008(14) TYPE p DECIMALS 2, "   LIKE zfi_pi_actual,
         wa_wtg009(14) TYPE p DECIMALS 2, "   LIKE zfi_pi_actual,
         wa_wtg010(14) TYPE p DECIMALS 2, "   LIKE zfi_pi_actual,
         wa_wtg011(14) TYPE p DECIMALS 2, "   LIKE zfi_pi_actual,
         wa_wtg012(14) TYPE p DECIMALS 2, "   LIKE zfi_pi_actual,
         wa_amt(14) TYPE p DECIMALS 2. "   LIKE zfi_pi_actual.

  SELECT posnr gjahr INTO CORRESPONDING FIELDS OF TABLE it_impr
  FROM impr
  WHERE posid EQ posid.

  CLEAR wa_cnt.
  DESCRIBE  TABLE it_impr LINES wa_cnt.
  IF wa_cnt > 0.
    SELECT objnr ippos baprz prozu
    INTO CORRESPONDING FIELDS OF TABLE it_imzo
    FROM imzo
    FOR ALL ENTRIES IN it_impr
    WHERE  posnr EQ it_impr-posnr.
*   AND    GJAHR EQ IT_IMPR-GJAHR.
  ELSE.
    EXIT.
  ENDIF.
**----2004/03/31 overhead order delete
*  IF ippos <> ' '.
*    LOOP AT it_imzo.
*      IF it_imzo-ippos = ippos.
*        DELETE it_imzo.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

  CLEAR wa_cnt.
  DESCRIBE  TABLE it_imzo LINES wa_cnt.
  IF wa_cnt > 0.
    IF aufnr <> space.
      SELECT aufnr objnr INTO
        CORRESPONDING FIELDS OF TABLE it_aufk
      FROM aufk
        FOR ALL ENTRIES IN it_imzo
        WHERE objnr = it_imzo-objnr
          AND aufnr = aufnr.
    ELSE.
      SELECT aufnr objnr INTO
        CORRESPONDING FIELDS OF TABLE it_aufk
      FROM aufk
      FOR ALL ENTRIES IN it_imzo
        WHERE objnr = it_imzo-objnr.
    ENDIF.
  ELSE.
    EXIT.
  ENDIF.

  CLEAR wa_cnt.
  DESCRIBE  TABLE it_aufk LINES wa_cnt.
  IF wa_cnt < 1.
    EXIT.
  ENDIF.

*Read Order Actual
  REFRESH: it_cosp.
  r_wrttp-option = 'EQ'.    r_wrttp-sign = 'I'.
  r_wrttp-low = '04'.  APPEND r_wrttp.
  r_wrttp-low = '11'.  APPEND r_wrttp.
  r_wrttp-low = '12'.  APPEND r_wrttp.
  r_wrttp-low = '13'.  APPEND r_wrttp.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_cosp
  FROM cosp
  FOR ALL ENTRIES IN it_aufk
  WHERE objnr EQ it_aufk-objnr
    AND wrttp IN r_wrttp.

*---Main process
  REFRESH : out.

  LOOP AT it_aufk.
    CLEAR : wa_ok, out, amt.

*looping...
    LOOP AT it_cosp WHERE objnr = it_aufk-objnr.
      CHECK it_cosp-beknz = 'S'
         OR it_cosp-beknz = 'H'
         OR it_cosp-beknz = 'A'.
*        OR it_cosp-beknz = 'L'

      out-wtg001 = it_cosp-wtg001.
      out-wtg002 = it_cosp-wtg002.
      out-wtg003 = it_cosp-wtg003.
      out-wtg004 = it_cosp-wtg004.
      out-wtg005 = it_cosp-wtg005.
      out-wtg006 = it_cosp-wtg006.
      out-wtg007 = it_cosp-wtg007.
      out-wtg008 = it_cosp-wtg008.
      out-wtg009 = it_cosp-wtg009.
      out-wtg010 = it_cosp-wtg010.
      out-wtg011 = it_cosp-wtg011.
      out-wtg012 = it_cosp-wtg012.

*calc share%
      READ TABLE it_imzo WITH KEY objnr = it_aufk-objnr.
      IF sy-subrc = 0.
        IF it_imzo-prozu = 0.
          IF it_imzo-baprz <> 0.
            CLEAR wa_baprz.
            LOOP AT it_imzo WHERE objnr = it_aufk-objnr.
              wa_baprz = wa_baprz + it_imzo-baprz.
            ENDLOOP.
            out-wtg001 = ( out-wtg001 * wa_baprz ) / 100.
            out-wtg002 = ( out-wtg002 * wa_baprz ) / 100.
            out-wtg003 = ( out-wtg003 * wa_baprz ) / 100.
            out-wtg004 = ( out-wtg004 * wa_baprz ) / 100.
            out-wtg005 = ( out-wtg005 * wa_baprz ) / 100.
            out-wtg006 = ( out-wtg006 * wa_baprz ) / 100.
            out-wtg007 = ( out-wtg007 * wa_baprz ) / 100.
            out-wtg008 = ( out-wtg008 * wa_baprz ) / 100.
            out-wtg009 = ( out-wtg009 * wa_baprz ) / 100.
            out-wtg010 = ( out-wtg010 * wa_baprz ) / 100.
            out-wtg011 = ( out-wtg011 * wa_baprz ) / 100.
            out-wtg012 = ( out-wtg012 * wa_baprz ) / 100.
          ENDIF.
        ELSE.
          IF it_imzo-prozu <> 0.
            out-wtg001 = ( out-wtg001 * it_imzo-prozu ) / 100.
            out-wtg002 = ( out-wtg002 * it_imzo-prozu ) / 100.
            out-wtg003 = ( out-wtg003 * it_imzo-prozu ) / 100.
            out-wtg004 = ( out-wtg004 * it_imzo-prozu ) / 100.
            out-wtg005 = ( out-wtg005 * it_imzo-prozu ) / 100.
            out-wtg006 = ( out-wtg006 * it_imzo-prozu ) / 100.
            out-wtg007 = ( out-wtg007 * it_imzo-prozu ) / 100.
            out-wtg008 = ( out-wtg008 * it_imzo-prozu ) / 100.
            out-wtg009 = ( out-wtg009 * it_imzo-prozu ) / 100.
            out-wtg010 = ( out-wtg010 * it_imzo-prozu ) / 100.
            out-wtg011 = ( out-wtg011 * it_imzo-prozu ) / 100.
            out-wtg012 = ( out-wtg012 * it_imzo-prozu ) / 100.
          ENDIF.
        ENDIF.
      ENDIF.

*calc yearly total
      out-tot =
                + out-wtg001 + out-wtg002 + out-wtg003 + out-wtg004
                + out-wtg005 + out-wtg006 + out-wtg007 + out-wtg008
                + out-wtg009 + out-wtg010 + out-wtg011 + out-wtg012.


*Determine Type
      IF it_cosp-wrttp = '12' OR it_cosp-wrttp = '13'.
        out-wrttp = 'D'.  "downpay
      ELSE.
        out-wrttp = 'I'.  "invoice
      ENDIF.

      out-posid = posid.
      out-gjahr = it_cosp-gjahr.

*Determine Expense Type
      IF it_cosp-beknz = 'S' OR it_cosp-beknz = 'H'.
        IF (   it_cosp-kstar >= '0000901000'
           AND it_cosp-kstar <= '0000901999' )
           OR  it_cosp-kstar < '0000200000'.
          MOVE '1' TO out-ippos.
        ELSE.
          MOVE '2' TO out-ippos.
        ENDIF.

        COLLECT out.

*Settled...Change Sign
*Why Need?
      ELSEIF it_cosp-beknz = 'A'.
*        MOVE '1' TO out-ippos.
*        out-tot    = - out-tot.
*        out-wtg001 = - out-wtg001.
*        out-wtg002 = - out-wtg002.
*        out-wtg003 = - out-wtg003.
*        out-wtg004 = - out-wtg004.
*        out-wtg005 = - out-wtg005.
*        out-wtg006 = - out-wtg006.
*        out-wtg007 = - out-wtg007.
*        out-wtg008 = - out-wtg008.
*        out-wtg009 = - out-wtg009.
*        out-wtg010 = - out-wtg010.
*        out-wtg011 = - out-wtg011.
*        out-wtg012 = - out-wtg012.
      ENDIF.

    ENDLOOP. "COSP
  ENDLOOP.   "ORDER



* add additional record (total)
  CLEAR : it_tot[], amt.
  LOOP AT out.
    MOVE-CORRESPONDING out TO it_tot.
    amt = amt + out-tot.
    MOVE ' ' TO it_tot-ippos.
    COLLECT it_tot. CLEAR   it_tot.
  ENDLOOP.

  LOOP AT it_tot.
    MOVE-CORRESPONDING it_tot TO out.
    COLLECT out.    CLEAR   out.
  ENDLOOP.

*Grand Total
  CLEAR : it_tot[], amt.
  LOOP AT out where ippos = ' '.
    MOVE-CORRESPONDING out TO it_tot.
    amt = amt + out-tot.
    MOVE ' ' TO it_tot-ippos.
    MOVE ' ' TO it_tot-wrttp.
    COLLECT it_tot. CLEAR   it_tot.
  ENDLOOP.

  LOOP AT it_tot.
    MOVE-CORRESPONDING it_tot TO out.
    COLLECT out.    CLEAR   out.
  ENDLOOP.

ENDFUNCTION.
