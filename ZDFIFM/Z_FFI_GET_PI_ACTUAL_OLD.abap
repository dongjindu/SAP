FUNCTION Z_FFI_GET_PI_ACTUAL_OLD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             REFERENCE(POSID) LIKE  IMPR-POSID
*"             REFERENCE(IPPOS) LIKE  IMZO-IPPOS DEFAULT ' '
*"       EXPORTING
*"             REFERENCE(AMT) LIKE  COSP-WTG001
*"       TABLES
*"              OUT STRUCTURE  ZFI_PI_ACTUAL
*"       EXCEPTIONS
*"              NO_DATA
*"----------------------------------------------------------------------

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
    SELECT aufnr objnr INTO
      CORRESPONDING FIELDS OF TABLE it_aufk
    FROM aufk
    FOR ALL ENTRIES IN it_imzo
    WHERE objnr EQ it_imzo-objnr.
  ELSE.
    EXIT.
  ENDIF.

  CLEAR wa_cnt.
  DESCRIBE  TABLE it_aufk LINES wa_cnt.
  IF wa_cnt < 1.
    EXIT.
  ENDIF.
*except myself
*  DELETE it_aufk WHERE aufnr EQ wa_aufnr.
  CLEAR wa_cnt.
  DESCRIBE  TABLE it_aufk LINES wa_cnt.
  IF wa_cnt < 1.
    EXIT.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_cosp
    FROM cosp
    FOR ALL ENTRIES IN it_aufk
    WHERE objnr EQ it_aufk-objnr.
  ENDIF.
*---Main process
  REFRESH : out.
  CLEAR :   out, amt.

  LOOP AT it_aufk.
    CLEAR : wa_ok.

*settled...
    READ TABLE it_cosp WITH KEY objnr = it_aufk-objnr
                                beknz = 'A'.
    IF sy-subrc = 0.   "exist amount
      IF it_cosp-wtg001 <> 0 OR it_cosp-wtg002 <> 0 OR
         it_cosp-wtg003 <> 0 OR it_cosp-wtg004 <> 0 OR
         it_cosp-wtg005 <> 0 OR it_cosp-wtg006 <> 0 OR
         it_cosp-wtg007 <> 0 OR it_cosp-wtg008 <> 0 OR
         it_cosp-wtg009 <> 0 OR it_cosp-wtg010 <> 0 OR
         it_cosp-wtg011 <> 0 OR it_cosp-wtg012 <> 0.
        wa_ok = 'Q'.
      ENDIF.
    ENDIF.

    LOOP AT it_cosp WHERE objnr = it_aufk-objnr.
*
      CHECK it_cosp-wrttp = '04' OR it_cosp-wrttp = '11'.

      CHECK it_cosp-beknz = 'S' OR it_cosp-beknz = 'H' OR
            it_cosp-beknz = 'A'. " OR it_cosp-beknz = 'L'.

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
      wa_tot = "wa_tot
                + out-wtg001 + out-wtg002 + out-wtg003 + out-wtg004
                + out-wtg005 + out-wtg006 + out-wtg007 + out-wtg008
                + out-wtg009 + out-wtg010 + out-wtg011 + out-wtg012.
      wa_amt = wa_tot.

      wa_wtg001 = out-wtg001.
      wa_wtg002 = out-wtg002.
      wa_wtg003 = out-wtg003.
      wa_wtg004 = out-wtg004.
      wa_wtg005 = out-wtg005.
      wa_wtg006 = out-wtg006.
      wa_wtg007 = out-wtg007.
      wa_wtg008 = out-wtg008.
      wa_wtg009 = out-wtg009.
      wa_wtg010 = out-wtg010.
      wa_wtg011 = out-wtg011.
      wa_wtg012 = out-wtg012.

      MOVE posid             TO out-posid.
      MOVE it_cosp-gjahr     TO out-gjahr.
*========* investment only
      IF it_cosp-wrttp = '11'.
        MOVE '1' TO out-ippos.
        out-tot = wa_tot.
        amt     = wa_amt.
        COLLECT out.
        CLEAR   out.
        CONTINUE.
      ENDIF.
*=======*
      IF wa_ok = 'Q'.
        IF it_cosp-beknz = 'A'.
          MOVE posid            TO out-posid.
          MOVE it_cosp-gjahr     TO out-gjahr.
          MOVE '1' TO out-ippos.
          out-tot =  wa_tot.
          out-tot = - out-tot.
          amt     =  wa_amt.
          amt     = - amt.

          out-wtg001 = - wa_wtg001.
          out-wtg002 = - wa_wtg002.
          out-wtg003 = - wa_wtg003.
          out-wtg004 = - wa_wtg004.
          out-wtg005 = - wa_wtg005.
          out-wtg006 = - wa_wtg006.
          out-wtg007 = - wa_wtg007.
          out-wtg008 = - wa_wtg008.
          out-wtg009 = - wa_wtg009.
          out-wtg010 = - wa_wtg010.
          out-wtg011 = - wa_wtg011.
          out-wtg012 = - wa_wtg012.

          COLLECT out.
          CLEAR   out.
        ENDIF.
*---2
        MOVE posid            TO out-posid.
        MOVE it_cosp-gjahr     TO out-gjahr.
        MOVE '2' TO out-ippos.
        out-tot = wa_tot.
        amt     = wa_amt.
        out-wtg001 =  wa_wtg001.
        out-wtg002 =  wa_wtg002.
        out-wtg003 =  wa_wtg003.
        out-wtg004 =  wa_wtg004.
        out-wtg005 =  wa_wtg005.
        out-wtg006 =  wa_wtg006.
        out-wtg007 =  wa_wtg007.
        out-wtg008 =  wa_wtg008.
        out-wtg009 =  wa_wtg009.
        out-wtg010 =  wa_wtg010.
        out-wtg011 =  wa_wtg011.
        out-wtg012 =  wa_wtg012.
        COLLECT out.
        CLEAR   out.
      ELSE.
        CHECK it_cosp-beknz = 'S' OR it_cosp-beknz = 'H'.
        MOVE posid            TO out-posid.
        MOVE it_cosp-gjahr     TO out-gjahr.
        IF   ( it_cosp-kstar >= '0000901000' AND
             it_cosp-kstar <= '0000901999' )
              OR it_cosp-kstar < '0000200000'.
          MOVE '1' TO out-ippos.
        ELSE.
          MOVE '2' TO out-ippos.
        ENDIF.
        out-tot = wa_tot.
        amt     = wa_amt.
        out-wtg001 =  wa_wtg001.
        out-wtg002 =  wa_wtg002.
        out-wtg003 =  wa_wtg003.
        out-wtg004 =  wa_wtg004.
        out-wtg005 =  wa_wtg005.
        out-wtg006 =  wa_wtg006.
        out-wtg007 =  wa_wtg007.
        out-wtg008 =  wa_wtg008.
        out-wtg009 =  wa_wtg009.
        out-wtg010 =  wa_wtg010.
        out-wtg011 =  wa_wtg011.
        out-wtg012 =  wa_wtg012.
        COLLECT out.
        CLEAR   out.
      ENDIF.

      CLEAR : wa_tot, wa_amt.
      CLEAR : wa_wtg001, wa_wtg002, wa_wtg003, wa_wtg004,
              wa_wtg005, wa_wtg006, wa_wtg007, wa_wtg008,
              wa_wtg009, wa_wtg010, wa_wtg011, wa_wtg012.
    ENDLOOP.


  ENDLOOP.
  clear : it_tot[], amt.
  LOOP AT out.
      move-corresponding out to it_tot.
      amt = amt + out-tot.
      move ' ' to it_tot-ippos.
      collect it_tot.
      clear   it_tot.
  ENDLOOP.

  loop at it_tot.
      move-corresponding it_tot to out.
      collect out.
      clear   out.
  endloop.
ENDFUNCTION.
