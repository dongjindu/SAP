FUNCTION Z_FFI_GET_PI_ACTUAL_ACT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(POSID) LIKE  IMPR-POSID
*"     REFERENCE(GJAHR) LIKE  IMPR-GJAHR
*"     REFERENCE(PRNAM) LIKE  IMPR-PRNAM OPTIONAL
*"     REFERENCE(IPPOS) LIKE  IMZO-IPPOS DEFAULT ' '
*"  EXPORTING
*"     REFERENCE(AMT) LIKE  COSP-WTG001
*"  TABLES
*"      OUT STRUCTURE  ZFI_PI_ACTUAL_ACT
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------
  DATA : BEGIN OF IT_IMPR OCCURS 0,
             POSNR LIKE IMPR-POSNR,
             GJAHR LIKE IMPR-GJAHR,
         END OF IT_IMPR.

  DATA : BEGIN OF IT_IMZO OCCURS 0,
             OBJNR LIKE IMZO-OBJNR,
             GJAHR LIKE IMZO-GJAHR,
             IPPOS LIKE IMZO-IPPOS,
             BAPRZ LIKE IMZO-BAPRZ,
             PROZU LIKE IMZO-PROZU,
         END OF IT_IMZO.

  DATA : BEGIN OF IT_AUFK OCCURS 0,
             AUFNR LIKE AUFK-AUFNR,
             OBJNR LIKE AUFK-OBJNR,
         END OF IT_AUFK.
*------
  DATA : BEGIN OF IT_RATE OCCURS 0,
             AUFNR LIKE AUFK-AUFNR,
             PROZU LIKE IMZO-PROZU,
         END OF IT_RATE.
  DATA : IT_TOT LIKE ZFI_PI_ACTUAL OCCURS 0 WITH HEADER LINE.
*-------ACTUAL MONTHLY-----------*
  DATA : IT_COSP LIKE COSP OCCURS 0 WITH HEADER LINE.
*-------
  DATA : WA_POSNR LIKE IMPR-POSNR,
         WA_CNT TYPE I,
         WA_AUFNR LIKE AUFK-AUFNR,
         WA_BAPRZ LIKE IMZO-BAPRZ,
*-------
         WA_OK,
         WA_TOT(14) TYPE P DECIMALS 2, "   LIKE zfi_pi_actual,
         WA_WTG001(14) TYPE P DECIMALS 2, "   LIKE zfi_pi_actual,
         WA_WTG002(14) TYPE P DECIMALS 2, "   LIKE zfi_pi_actual,
         WA_WTG003(14) TYPE P DECIMALS 2, "   LIKE zfi_pi_actual,
         WA_WTG004(14) TYPE P DECIMALS 2, "   LIKE zfi_pi_actual,
         WA_WTG005(14) TYPE P DECIMALS 2, "   LIKE zfi_pi_actual,
         WA_WTG006(14) TYPE P DECIMALS 2, "   LIKE zfi_pi_actual,
         WA_WTG007(14) TYPE P DECIMALS 2, "   LIKE zfi_pi_actual,
         WA_WTG008(14) TYPE P DECIMALS 2, "   LIKE zfi_pi_actual,
         WA_WTG009(14) TYPE P DECIMALS 2, "   LIKE zfi_pi_actual,
         WA_WTG010(14) TYPE P DECIMALS 2, "   LIKE zfi_pi_actual,
         WA_WTG011(14) TYPE P DECIMALS 2, "   LIKE zfi_pi_actual,
         WA_WTG012(14) TYPE P DECIMALS 2, "   LIKE zfi_pi_actual,
         WA_AMT(14) TYPE P DECIMALS 2. "   LIKE zfi_pi_actual.

*<< Start of addtion on 10.18.2006 by Michelle
  IF PRNAM IS INITIAL.
    SELECT POSNR GJAHR INTO TABLE IT_IMPR FROM IMPR
     WHERE POSID = POSID
       AND GJAHR = GJAHR.
  ELSE.
* End of addtion on 10.18.2006 by Michelle >>
    SELECT POSNR GJAHR INTO TABLE IT_IMPR
    FROM IMPR
    WHERE POSID = POSID
    AND   GJAHR = GJAHR
    AND   PRNAM = PRNAM.
*<< Start of addtion on 10.18.2006 by Michelle
  ENDIF.
* End of addtion on 10.18.2006 by Michelle >>

  CLEAR WA_CNT.
  DESCRIBE  TABLE IT_IMPR LINES WA_CNT.
  IF WA_CNT > 0.
    SELECT OBJNR GJAHR IPPOS BAPRZ PROZU
     INTO CORRESPONDING FIELDS OF TABLE IT_IMZO
    FROM IMZO
    FOR ALL ENTRIES IN IT_IMPR
    WHERE  POSNR EQ IT_IMPR-POSNR
   AND    GJAHR EQ IT_IMPR-GJAHR.
  ELSE.
    EXIT.
  ENDIF.

  CLEAR WA_CNT.
  DESCRIBE  TABLE IT_IMZO LINES WA_CNT.
  IF WA_CNT > 0.
    SELECT AUFNR OBJNR INTO CORRESPONDING FIELDS OF TABLE IT_AUFK
    FROM AUFK
    FOR ALL ENTRIES IN IT_IMZO
    WHERE OBJNR EQ IT_IMZO-OBJNR.
  ELSE.
    EXIT.
  ENDIF.

  CLEAR WA_CNT.
  DESCRIBE  TABLE IT_AUFK LINES WA_CNT.
  IF WA_CNT < 1.
    EXIT.
  ENDIF.
*except myself
*  DELETE it_aufk WHERE aufnr EQ wa_aufnr.
  CLEAR WA_CNT.
  DESCRIBE  TABLE IT_AUFK LINES WA_CNT.
  IF WA_CNT < 1.
    EXIT.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_COSP
    FROM COSP
    FOR ALL ENTRIES IN IT_AUFK
    WHERE OBJNR EQ IT_AUFK-OBJNR.

  ENDIF.
***********************************************************
*---Main process
  REFRESH : OUT.
  CLEAR :   OUT, AMT.

  LOOP AT IT_AUFK.
    CLEAR : WA_OK.

    READ TABLE IT_COSP WITH KEY OBJNR = IT_AUFK-OBJNR
                                BEKNZ = 'A'.
    IF SY-SUBRC = 0.   "exist amount
      IF IT_COSP-WTG001 <> 0 OR IT_COSP-WTG002 <> 0 OR
         IT_COSP-WTG003 <> 0 OR IT_COSP-WTG004 <> 0 OR
         IT_COSP-WTG005 <> 0 OR IT_COSP-WTG006 <> 0 OR
         IT_COSP-WTG007 <> 0 OR IT_COSP-WTG008 <> 0 OR
         IT_COSP-WTG009 <> 0 OR IT_COSP-WTG010 <> 0 OR
         IT_COSP-WTG011 <> 0 OR IT_COSP-WTG012 <> 0.
        WA_OK = 'Q'.
      ENDIF.
    ENDIF.

    LOOP AT IT_COSP WHERE OBJNR = IT_AUFK-OBJNR.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/11/30, by WSKIM
*---Start
*      CHECK it_cosp-wrttp = '04' OR it_cosp-wrttp = '11'.
*
*      CHECK it_cosp-beknz = 'S' OR  it_cosp-beknz = 'H' OR
*            it_cosp-beknz = 'A'. " OR it_cosp-beknz = 'L'.
      CHECK IT_COSP-BEKNZ = 'H' OR
            IT_COSP-BEKNZ = 'S'.
*---End

      OUT-WTG001 = IT_COSP-WTG001.
      OUT-WTG002 = IT_COSP-WTG002.
      OUT-WTG003 = IT_COSP-WTG003.
      OUT-WTG004 = IT_COSP-WTG004.
      OUT-WTG005 = IT_COSP-WTG005.
      OUT-WTG006 = IT_COSP-WTG006.
      OUT-WTG007 = IT_COSP-WTG007.
      OUT-WTG008 = IT_COSP-WTG008.
      OUT-WTG009 = IT_COSP-WTG009.
      OUT-WTG010 = IT_COSP-WTG010.
      OUT-WTG011 = IT_COSP-WTG011.
      OUT-WTG012 = IT_COSP-WTG012.

      READ TABLE IT_IMZO WITH KEY OBJNR = IT_AUFK-OBJNR.

      IF SY-SUBRC = 0.
        IF IT_IMZO-PROZU = 0.
          IF IT_IMZO-BAPRZ <> 0.
            CLEAR WA_BAPRZ.
            LOOP AT IT_IMZO WHERE OBJNR = IT_AUFK-OBJNR.
              WA_BAPRZ = WA_BAPRZ + IT_IMZO-BAPRZ.
            ENDLOOP.
            OUT-WTG001 = ( OUT-WTG001 * WA_BAPRZ ) / 100.
            OUT-WTG002 = ( OUT-WTG002 * WA_BAPRZ ) / 100.
            OUT-WTG003 = ( OUT-WTG003 * WA_BAPRZ ) / 100.
            OUT-WTG004 = ( OUT-WTG004 * WA_BAPRZ ) / 100.
            OUT-WTG005 = ( OUT-WTG005 * WA_BAPRZ ) / 100.
            OUT-WTG006 = ( OUT-WTG006 * WA_BAPRZ ) / 100.
            OUT-WTG007 = ( OUT-WTG007 * WA_BAPRZ ) / 100.
            OUT-WTG008 = ( OUT-WTG008 * WA_BAPRZ ) / 100.
            OUT-WTG009 = ( OUT-WTG009 * WA_BAPRZ ) / 100.
            OUT-WTG010 = ( OUT-WTG010 * WA_BAPRZ ) / 100.
            OUT-WTG011 = ( OUT-WTG011 * WA_BAPRZ ) / 100.
            OUT-WTG012 = ( OUT-WTG012 * WA_BAPRZ ) / 100.
          ENDIF.
        ELSE.
          IF IT_IMZO-PROZU <> 0.
            OUT-WTG001 = ( OUT-WTG001 * IT_IMZO-PROZU ) / 100.
            OUT-WTG002 = ( OUT-WTG002 * IT_IMZO-PROZU ) / 100.
            OUT-WTG003 = ( OUT-WTG003 * IT_IMZO-PROZU ) / 100.
            OUT-WTG004 = ( OUT-WTG004 * IT_IMZO-PROZU ) / 100.
            OUT-WTG005 = ( OUT-WTG005 * IT_IMZO-PROZU ) / 100.
            OUT-WTG006 = ( OUT-WTG006 * IT_IMZO-PROZU ) / 100.
            OUT-WTG007 = ( OUT-WTG007 * IT_IMZO-PROZU ) / 100.
            OUT-WTG008 = ( OUT-WTG008 * IT_IMZO-PROZU ) / 100.
            OUT-WTG009 = ( OUT-WTG009 * IT_IMZO-PROZU ) / 100.
            OUT-WTG010 = ( OUT-WTG010 * IT_IMZO-PROZU ) / 100.
            OUT-WTG011 = ( OUT-WTG011 * IT_IMZO-PROZU ) / 100.
            OUT-WTG012 = ( OUT-WTG012 * IT_IMZO-PROZU ) / 100.
          ENDIF.
        ENDIF.
      ENDIF.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/11/30, by WSKIM
*---Start
*      wa_tot = wa_tot
      WA_TOT = "wa_tot
*---End
                + OUT-WTG001 + OUT-WTG002 + OUT-WTG003 + OUT-WTG004
                + OUT-WTG005 + OUT-WTG006 + OUT-WTG007 + OUT-WTG008
                + OUT-WTG009 + OUT-WTG010 + OUT-WTG011 + OUT-WTG012.
      WA_AMT = WA_TOT.

      WA_WTG001 = OUT-WTG001.
      WA_WTG002 = OUT-WTG002.
      WA_WTG003 = OUT-WTG003.
      WA_WTG004 = OUT-WTG004.
      WA_WTG005 = OUT-WTG005.
      WA_WTG006 = OUT-WTG006.
      WA_WTG007 = OUT-WTG007.
      WA_WTG008 = OUT-WTG008.
      WA_WTG009 = OUT-WTG009.
      WA_WTG010 = OUT-WTG010.
      WA_WTG011 = OUT-WTG011.
      WA_WTG012 = OUT-WTG012.

      MOVE POSID             TO OUT-POSID.
      MOVE IT_COSP-GJAHR     TO OUT-GJAHR.
      MOVE IT_COSP-WRTTP     TO OUT-WRTTP.
*========* investment only

*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/11/30, by WSKIM
*---Start
*   IF it_cosp-wrttp = '11'.
      IF IT_COSP-WRTTP = '11' OR IT_COSP-WRTTP = '04'.
        MOVE '1' TO OUT-IPPOS.
        OUT-TOT = WA_TOT.
        AMT     = WA_AMT.
        COLLECT OUT.
        CLEAR   OUT.
        CONTINUE.
      ENDIF.
*=======*
      IF WA_OK = 'Q'.
        IF IT_COSP-BEKNZ = 'A'.
          MOVE POSID            TO OUT-POSID.
          MOVE IT_COSP-GJAHR     TO OUT-GJAHR.
          MOVE '1' TO OUT-IPPOS.
          OUT-TOT =  WA_TOT.
          OUT-TOT = - OUT-TOT.
          AMT     =  WA_AMT.
          AMT     = - AMT.

          OUT-WTG001 = - WA_WTG001.
          OUT-WTG002 = - WA_WTG002.
          OUT-WTG003 = - WA_WTG003.
          OUT-WTG004 = - WA_WTG004.
          OUT-WTG005 = - WA_WTG005.
          OUT-WTG006 = - WA_WTG006.
          OUT-WTG007 = - WA_WTG007.
          OUT-WTG008 = - WA_WTG008.
          OUT-WTG009 = - WA_WTG009.
          OUT-WTG010 = - WA_WTG010.
          OUT-WTG011 = - WA_WTG011.
          OUT-WTG012 = - WA_WTG012.

          COLLECT OUT.
          CLEAR   OUT.
        ENDIF.
*---2
        MOVE POSID            TO OUT-POSID.
        MOVE IT_COSP-GJAHR     TO OUT-GJAHR.
        MOVE '2' TO OUT-IPPOS.
        OUT-TOT = WA_TOT.
        AMT     = WA_AMT.
        OUT-WTG001 =  WA_WTG001.
        OUT-WTG002 =  WA_WTG002.
        OUT-WTG003 =  WA_WTG003.
        OUT-WTG004 =  WA_WTG004.
        OUT-WTG005 =  WA_WTG005.
        OUT-WTG006 =  WA_WTG006.
        OUT-WTG007 =  WA_WTG007.
        OUT-WTG008 =  WA_WTG008.
        OUT-WTG009 =  WA_WTG009.
        OUT-WTG010 =  WA_WTG010.
        OUT-WTG011 =  WA_WTG011.
        OUT-WTG012 =  WA_WTG012.
        COLLECT OUT.
        CLEAR   OUT.
      ELSE.
        CHECK IT_COSP-BEKNZ = 'S' OR IT_COSP-BEKNZ = 'H'.
        MOVE POSID            TO OUT-POSID.
        MOVE IT_COSP-GJAHR     TO OUT-GJAHR.
        IF   ( IT_COSP-KSTAR >= '0000901000' AND
             IT_COSP-KSTAR <= '0000901999' )
              OR IT_COSP-KSTAR < '0000200000'.
          MOVE '1' TO OUT-IPPOS.
        ELSE.
          MOVE '2' TO OUT-IPPOS.
        ENDIF.
        OUT-TOT = WA_TOT.
        AMT     = WA_AMT.
        OUT-WTG001 =  WA_WTG001.
        OUT-WTG002 =  WA_WTG002.
        OUT-WTG003 =  WA_WTG003.
        OUT-WTG004 =  WA_WTG004.
        OUT-WTG005 =  WA_WTG005.
        OUT-WTG006 =  WA_WTG006.
        OUT-WTG007 =  WA_WTG007.
        OUT-WTG008 =  WA_WTG008.
        OUT-WTG009 =  WA_WTG009.
        OUT-WTG010 =  WA_WTG010.
        OUT-WTG011 =  WA_WTG011.
        OUT-WTG012 =  WA_WTG012.
        COLLECT OUT.
        CLEAR   OUT.
      ENDIF.

      CLEAR : WA_TOT, WA_AMT.
      CLEAR : WA_WTG001, WA_WTG002, WA_WTG003, WA_WTG004,
              WA_WTG005, WA_WTG006, WA_WTG007, WA_WTG008,
              WA_WTG009, WA_WTG010, WA_WTG011, WA_WTG012.
    ENDLOOP.


  ENDLOOP.
  CLEAR : IT_TOT[], AMT.
  LOOP AT OUT.
    MOVE-CORRESPONDING OUT TO IT_TOT.
    AMT = AMT + OUT-TOT.
    MOVE ' ' TO IT_TOT-IPPOS.
    COLLECT IT_TOT.
    CLEAR   IT_TOT.
  ENDLOOP.

  LOOP AT IT_TOT.
    MOVE-CORRESPONDING IT_TOT TO OUT.
    COLLECT OUT.
    CLEAR   OUT.
  ENDLOOP.

ENDFUNCTION.
