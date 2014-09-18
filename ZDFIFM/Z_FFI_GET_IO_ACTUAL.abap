FUNCTION z_ffi_get_io_actual.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(AUFNR) LIKE  AUFK-AUFNR OPTIONAL
*"     REFERENCE(OBJNR) LIKE  AUFK-OBJNR OPTIONAL
*"     REFERENCE(L_IM) TYPE  C DEFAULT ' '
*"  EXPORTING
*"     REFERENCE(AMT) LIKE  COSP-WTG001
*"  TABLES
*"      OUT STRUCTURE  ZFI_IO_ACTUAL OPTIONAL
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------

*L_IM: exclude Co reposting
*Value Type
*4	Actual
*11	Statistical actual

*12	Down payment as operating expense
*13	Statistical downpayments as expense
*
*21	Purchase requisitions commitment
*22	Purchase order commitment
*60	Parked document


  DATA : BEGIN OF it_impr OCCURS 0,
             posnr LIKE impr-posnr,
             gjahr LIKE impr-gjahr,
         END OF it_impr.

  DATA : it_imzo TYPE TABLE OF imzo WITH HEADER LINE.

  DATA : BEGIN OF it_aufk OCCURS 0,
             aufnr LIKE aufk-aufnr,
             objnr LIKE aufk-objnr,
         END OF it_aufk.
*------
  DATA : BEGIN OF it_rate OCCURS 0,
             aufnr LIKE aufk-aufnr,
             prozu LIKE imzo-prozu,
         END OF it_rate.
*-------ACTUAL MONTHLY-----------*
  DATA : it_cosp LIKE cosp OCCURS 0 WITH HEADER LINE.
  DATA : it_coss LIKE cosp OCCURS 0 WITH HEADER LINE.
*-------
  DATA : wa_posnr LIKE impr-posnr,
         wa_cnt TYPE i,
         wa_aufnr LIKE aufk-aufnr,
         wa_objnr LIKE imzo-objnr.

  IF aufnr IS INITIAL.
    SELECT SINGLE aufnr objnr INTO (wa_aufnr, wa_objnr)
      FROM aufk
      WHERE objnr EQ objnr.
  ELSE.
    SELECT SINGLE aufnr objnr INTO (wa_aufnr, wa_objnr)
      FROM aufk
      WHERE aufnr EQ aufnr.
  ENDIF.

  SELECT * INTO TABLE it_imzo
    FROM imzo
    WHERE  objnr EQ wa_objnr.
*except myself
*  DELETE it_aufk WHERE aufnr EQ wa_aufnr.
  CLEAR wa_cnt.
  DESCRIBE  TABLE it_imzo LINES wa_cnt.
*  IF wa_cnt < 1.
*    EXIT.
*  ELSE.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_cosp
    FROM cosp
    WHERE objnr EQ wa_objnr.

    SELECT * APPENDING CORRESPONDING  FIELDS OF TABLE it_cosp
    FROM coss
    WHERE objnr EQ wa_objnr.

*  ENDIF.
*---Main process
  REFRESH : out.
  CLEAR :   out, amt.

  LOOP AT it_cosp.
    IF it_cosp-beknz <> 'H' AND
       it_cosp-beknz <> 'S'.
      CONTINUE.
    ENDIF.
    IF l_im = 'X' and it_cosp-vrgng = 'RKU1'.
      CONTINUE.
    ENDIF.

    MOVE aufnr             TO out-posid.
    MOVE it_cosp-gjahr     TO out-gjahr.
    MOVE it_cosp-wrttp     TO out-wrttp.

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
          out-wtg001 = ( out-wtg001 * it_imzo-baprz ) / 100.
          out-wtg002 = ( out-wtg002 * it_imzo-baprz ) / 100.
          out-wtg003 = ( out-wtg003 * it_imzo-baprz ) / 100.
          out-wtg004 = ( out-wtg004 * it_imzo-baprz ) / 100.
          out-wtg005 = ( out-wtg005 * it_imzo-baprz ) / 100.
          out-wtg006 = ( out-wtg006 * it_imzo-baprz ) / 100.
          out-wtg007 = ( out-wtg007 * it_imzo-baprz ) / 100.
          out-wtg008 = ( out-wtg008 * it_imzo-baprz ) / 100.
          out-wtg009 = ( out-wtg009 * it_imzo-baprz ) / 100.
          out-wtg010 = ( out-wtg010 * it_imzo-baprz ) / 100.
          out-wtg011 = ( out-wtg011 * it_imzo-baprz ) / 100.
          out-wtg012 = ( out-wtg012 * it_imzo-baprz ) / 100.
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

    out-tot = out-tot
               + out-wtg001 + out-wtg002 + out-wtg003 + out-wtg004
              + out-wtg005 + out-wtg006 + out-wtg007 + out-wtg008
              + out-wtg009 + out-wtg010 + out-wtg011 + out-wtg012.

    amt = amt + out-wtg001 + out-wtg002 + out-wtg003 + out-wtg004
              + out-wtg005 + out-wtg006 + out-wtg007 + out-wtg008
              + out-wtg009 + out-wtg010 + out-wtg011 + out-wtg012.
    COLLECT out.
    CLEAR   out.
  ENDLOOP.
***********************coss
*  LOOP AT it_coss.
*    IF it_coss-beknz <> 'H' AND
*       it_coss-beknz <> 'S'.
*      CONTINUE.
*    ENDIF.
*    IF it_coss-vrgng = 'RKU1'.
*      CONTINUE.
*    ENDIF.
*
*    MOVE aufnr             TO out-posid.
*    MOVE it_coss-gjahr     TO out-gjahr.
*    MOVE it_coss-wrttp     TO out-wrttp.
*
*      out-wtg001 = it_coss-wtg001.
*      out-wtg002 = it_coss-wtg002.
*      out-wtg003 = it_coss-wtg003.
*      out-wtg004 = it_coss-wtg004.
*      out-wtg005 = it_coss-wtg005.
*      out-wtg006 = it_coss-wtg006.
*      out-wtg007 = it_coss-wtg007.
*      out-wtg008 = it_coss-wtg008.
*      out-wtg009 = it_coss-wtg009.
*      out-wtg010 = it_coss-wtg010.
*      out-wtg011 = it_coss-wtg011.
*      out-wtg012 = it_coss-wtg012.
*
*    READ TABLE it_imzo WITH KEY objnr = it_aufk-objnr.
*
*    IF sy-subrc = 0.
*      IF it_imzo-prozu = 0.
*        IF it_imzo-baprz <> 0.
*          out-wtg001 = ( out-wtg001 * it_imzo-baprz ) / 100.
*          out-wtg002 = ( out-wtg002 * it_imzo-baprz ) / 100.
*          out-wtg003 = ( out-wtg003 * it_imzo-baprz ) / 100.
*          out-wtg004 = ( out-wtg004 * it_imzo-baprz ) / 100.
*          out-wtg005 = ( out-wtg005 * it_imzo-baprz ) / 100.
*          out-wtg006 = ( out-wtg006 * it_imzo-baprz ) / 100.
*          out-wtg007 = ( out-wtg007 * it_imzo-baprz ) / 100.
*          out-wtg008 = ( out-wtg008 * it_imzo-baprz ) / 100.
*          out-wtg009 = ( out-wtg009 * it_imzo-baprz ) / 100.
*          out-wtg010 = ( out-wtg010 * it_imzo-baprz ) / 100.
*          out-wtg011 = ( out-wtg011 * it_imzo-baprz ) / 100.
*          out-wtg012 = ( out-wtg012 * it_imzo-baprz ) / 100.
*        ENDIF.
*      ELSE.
*        IF it_imzo-prozu <> 0.
*          out-wtg001 = ( out-wtg001 * it_imzo-prozu ) / 100.
*          out-wtg002 = ( out-wtg002 * it_imzo-prozu ) / 100.
*          out-wtg003 = ( out-wtg003 * it_imzo-prozu ) / 100.
*          out-wtg004 = ( out-wtg004 * it_imzo-prozu ) / 100.
*          out-wtg005 = ( out-wtg005 * it_imzo-prozu ) / 100.
*          out-wtg006 = ( out-wtg006 * it_imzo-prozu ) / 100.
*          out-wtg007 = ( out-wtg007 * it_imzo-prozu ) / 100.
*          out-wtg008 = ( out-wtg008 * it_imzo-prozu ) / 100.
*          out-wtg009 = ( out-wtg009 * it_imzo-prozu ) / 100.
*          out-wtg010 = ( out-wtg010 * it_imzo-prozu ) / 100.
*          out-wtg011 = ( out-wtg011 * it_imzo-prozu ) / 100.
*          out-wtg012 = ( out-wtg012 * it_imzo-prozu ) / 100.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    out-tot = out-tot
*               + out-wtg001 + out-wtg002 + out-wtg003 + out-wtg004
*              + out-wtg005 + out-wtg006 + out-wtg007 + out-wtg008
*              + out-wtg009 + out-wtg010 + out-wtg011 + out-wtg012.
*
*    amt = amt + out-wtg001 + out-wtg002 + out-wtg003 + out-wtg004
*              + out-wtg005 + out-wtg006 + out-wtg007 + out-wtg008
*              + out-wtg009 + out-wtg010 + out-wtg011 + out-wtg012.
*    COLLECT out.
*    CLEAR   out.
*  ENDLOOP.
*
ENDFUNCTION.
