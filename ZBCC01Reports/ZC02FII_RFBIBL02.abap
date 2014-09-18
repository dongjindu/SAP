*  INCLUDE RFBIBL02.
*-----------------------------------------------------------------------
*        Generated  Date      20000313
*                   Time      132242
*                   Release   46C
*                   System    P9C
*                   User      RECCIUS
*-----------------------------------------------------------------------



*eject
*----------------------------------------------------------
*        FORM WA_DATEN_UEBERTRAGEN.
*----------------------------------------------------------
FORM WA_DATEN_UEBERTRAGEN.
  CASE WA+1(1).
  WHEN 'B'.
    CASE WA+2(9).
      WHEN 'BSEG'.
        BBSEG = I_BBSEG.
        BBSEG = WA.
      WHEN 'WITH'.
        BWITH = I_BWITH.
        BWITH = WA.
      WHEN 'BTAX'.
        BBTAX = I_BBTAX.
        BBTAX = WA.
      WHEN 'SELK'.
        BSELK = I_BSELK.
        BSELK = WA.
      WHEN 'SELP'.
        BSELP = I_BSELP.
        BSELP = WA.
    ENDCASE.
  WHEN 'Z'.
    CASE WA+2(9).
    ENDCASE.
  ENDCASE.
ENDFORM.
*eject
*----------------------------------------------------------
*        FORM FILL_FTPOST_WITH_BBKPF_DATA.
*----------------------------------------------------------
FORM FILL_FTPOST_WITH_BBKPF_DATA.
  CHECK FL_CHECK = SPACE.
  CHECK FUNCTION NE 'D'.
  CLEAR FTPOST.
  FTPOST-STYPE = 'K'.
  FTPOST-COUNT = '001'.
  IF BBKPF-BLDAT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-BLDAT                       '.
    FTPOST-FVAL = BBKPF-BLDAT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-BLART(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-BLART                       '.
    FTPOST-FVAL = BBKPF-BLART                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-BUKRS(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-BUKRS                       '.
    FTPOST-FVAL = BBKPF-BUKRS                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-BUDAT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-BUDAT                       '.
    FTPOST-FVAL = BBKPF-BUDAT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-MONAT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-MONAT                       '.
    FTPOST-FVAL = BBKPF-MONAT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-WAERS(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-WAERS                       '.
    FTPOST-FVAL = BBKPF-WAERS                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-KURSF(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-KURSF                       '.
    CONCATENATE PREFIX_P
    BBKPF-KURSF                       INTO FTPOST-FVAL.
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-BELNR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-BELNR                       '.
    FTPOST-FVAL = BBKPF-BELNR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-WWERT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-WWERT                       '.
    FTPOST-FVAL = BBKPF-WWERT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-XBLNR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-XBLNR                       '.
    FTPOST-FVAL = BBKPF-XBLNR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-BVORG(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-BVORG                       '.
    FTPOST-FVAL = BBKPF-BVORG                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-BKTXT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-BKTXT                       '.
    FTPOST-FVAL = BBKPF-BKTXT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-PARGB(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'RF05A-PARGB                      '.
    FTPOST-FVAL = BBKPF-PARGB                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-VBUND(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-VBUND                       '.
    FTPOST-FVAL = BBKPF-VBUND                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-XMWST(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-XMWST                       '.
    FTPOST-FVAL = BBKPF-XMWST                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-DOCID(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'FS006-DOCID                      '.
    FTPOST-FVAL = BBKPF-DOCID                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-BARCD(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'FS006-BARCD                      '.
    FTPOST-FVAL = BBKPF-BARCD                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-STODT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-STODT                       '.
    FTPOST-FVAL = BBKPF-STODT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-BRNCH(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-BRNCH                       '.
    FTPOST-FVAL = BBKPF-BRNCH                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-NUMPG(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-NUMPG                       '.
    FTPOST-FVAL = BBKPF-NUMPG                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-STGRD(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-STGRD                       '.
    FTPOST-FVAL = BBKPF-STGRD                      .
    APPEND FTPOST.
  ENDIF.
  IF BBKPF-KURSF_M(1)                  NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BKPF-KURSF                       '.
    CONCATENATE PREFIX_M
    BBKPF-KURSF_M                     INTO FTPOST-FVAL.
    APPEND FTPOST.
  ENDIF.
ENDFORM.

*eject
*----------------------------------------------------------
*        FORM FILL_FTPOST_WITH_BBSEG_DATA USING COUNT
*----------------------------------------------------------
FORM FILL_FTPOST_WITH_BBSEG_DATA USING COUNT.
  CHECK FL_CHECK = SPACE.
  CHECK FUNCTION NE 'D'.
  CLEAR FTPOST.
  FTPOST-STYPE = 'P'.
  FTPOST-COUNT = COUNT.
  IF XTBSL-KOART = 'S'
  OR ( XTBSL-KOART CO 'DK' AND XTBSL-XSONU = 'X' ).
  ELSE.
    BBSEG-WENR  = NODATA.
    BBSEG-GENR  = NODATA.
    BBSEG-GRNR  = NODATA.
    BBSEG-MENR  = NODATA.
    BBSEG-MIVE  = NODATA.
    BBSEG-NKSL  = NODATA.
    BBSEG-EMPSL = NODATA.
    BBSEG-SVWNR = NODATA.
    BBSEG-SBERI = NODATA.
  ENDIF.
  IF BBSEG-NEWBS(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF BBKPF-TCODE = 'FBV1'.
      FTPOST-FNAM = 'RF05V-NEWBS                      '.
    ELSE.
      FTPOST-FNAM = 'RF05A-NEWBS                      '.
    ENDIF.
    FTPOST-FVAL = BBSEG-NEWBS                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-NEWUM(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF BBKPF-TCODE = 'FBV1'.
      FTPOST-FNAM = 'RF05V-NEWUM                      '.
    ELSE.
      FTPOST-FNAM = 'RF05A-NEWUM                      '.
    ENDIF.
    FTPOST-FVAL = BBSEG-NEWUM                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-NEWBK(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF BBKPF-TCODE = 'FBV1'.
      FTPOST-FNAM = 'RF05V-NEWBK                      '.
    ELSE.
      FTPOST-FNAM = 'RF05A-NEWBK                      '.
    ENDIF.
    FTPOST-FVAL = BBSEG-NEWBK                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WRBTR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-WRBTR                       '.
    FTPOST-FVAL = BBSEG-WRBTR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-DMBTR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-DMBTR                       '.
    FTPOST-FVAL = BBSEG-DMBTR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WMWST(1)                    NE NODATA.
   IF XTBSL-KOART NE 'S'.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-WMWST                       '.
    FTPOST-FVAL = BBSEG-WMWST                      .
    APPEND FTPOST.
   ENDIF.
  ENDIF.
  IF BBSEG-MWSTS(1)                    NE NODATA.
   IF XTBSL-KOART NE 'S'.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-MWSTS                       '.
    FTPOST-FVAL = BBSEG-MWSTS                      .
    APPEND FTPOST.
   ENDIF.
  ENDIF.
  IF BBSEG-MWSKZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-MWSKZ                       '.
    FTPOST-FVAL = BBSEG-MWSKZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-XSKRL(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-XSKRL                       '.
    FTPOST-FVAL = BBSEG-XSKRL                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-FWZUZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-FWZUZ                       '.
    FTPOST-FVAL = BBSEG-FWZUZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-HWZUZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-HWZUZ                       '.
    FTPOST-FVAL = BBSEG-HWZUZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-GSBER(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-GSBER                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-GSBER                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-GSBER                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-KOSTL(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-KOSTL                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-KOSTL                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-KOSTL                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-AUFNR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-AUFNR                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-AUFNR                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-AUFNR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-EBELN(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-EBELN                       '.
    FTPOST-FVAL = BBSEG-EBELN                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-EBELP(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-EBELP                       '.
    FTPOST-FVAL = BBSEG-EBELP                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-PROJN(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-PROJN                       '.
    FTPOST-FVAL = BBSEG-PROJN                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-MATNR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-MATNR                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-MATNR                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-MATNR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WERKS(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-WERKS                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-WERKS                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-WERKS                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-MENGE(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-MENGE                       '.
    FTPOST-FVAL = BBSEG-MENGE                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-MEINS(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-MEINS                       '.
    FTPOST-FVAL = BBSEG-MEINS                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-VBEL2(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-KDAUF                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-VBEL2                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-VBEL2                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-POSN2(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-KDPOS                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-POSN2                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-POSN2                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ETEN2(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-KDEIN                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-ETEN2                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-ETEN2                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-PERNR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-PERNR                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-PERNR                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-PERNR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-BEWAR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-RMVCT                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-BEWAR                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-BEWAR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-VALUT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-VALUT                       '.
    FTPOST-FVAL = BBSEG-VALUT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZFBDT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZFBDT                       '.
    FTPOST-FVAL = BBSEG-ZFBDT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZINKZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZINKZ                       '.
    FTPOST-FVAL = BBSEG-ZINKZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZUONR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZUONR                       '.
    FTPOST-FVAL = BBSEG-ZUONR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-FKONT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-FKONT                       '.
    FTPOST-FVAL = BBSEG-FKONT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-XAABG(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-XAABG                       '.
    FTPOST-FVAL = BBSEG-XAABG                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-SGTXT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-SGTXT                       '.
    FTPOST-FVAL = BBSEG-SGTXT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-BLNKZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-BLNKZ                       '.
    FTPOST-FVAL = BBSEG-BLNKZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-BLNBT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-BLNBT                       '.
    FTPOST-FVAL = BBSEG-BLNBT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-BLNPZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-BLNPZ                       '.
    FTPOST-FVAL = BBSEG-BLNPZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-MABER(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-MABER                       '.
    FTPOST-FVAL = BBSEG-MABER                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-SKFBT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-SKFBT                       '.
    FTPOST-FVAL = BBSEG-SKFBT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WSKTO(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-WSKTO                       '.
    FTPOST-FVAL = BBSEG-WSKTO                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZTERM(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZTERM                       '.
    FTPOST-FVAL = BBSEG-ZTERM                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZBD1T(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZBD1T                       '.
    FTPOST-FVAL = BBSEG-ZBD1T                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZBD1P(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZBD1P                       '.
    FTPOST-FVAL = BBSEG-ZBD1P                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZBD2T(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZBD2T                       '.
    FTPOST-FVAL = BBSEG-ZBD2T                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZBD2P(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZBD2P                       '.
    FTPOST-FVAL = BBSEG-ZBD2P                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZBD3T(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZBD3T                       '.
    FTPOST-FVAL = BBSEG-ZBD3T                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZLSPR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZLSPR                       '.
    FTPOST-FVAL = BBSEG-ZLSPR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-REBZG(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-REBZG                       '.
    FTPOST-FVAL = BBSEG-REBZG                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-REBZJ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-REBZJ                       '.
    FTPOST-FVAL = BBSEG-REBZJ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-REBZZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-REBZZ                       '.
    FTPOST-FVAL = BBSEG-REBZZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZLSCH(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZLSCH                       '.
    FTPOST-FVAL = BBSEG-ZLSCH                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-SAMNR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-SAMNR                       '.
    FTPOST-FVAL = BBSEG-SAMNR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZBFIX(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZBFIX                       '.
    FTPOST-FVAL = BBSEG-ZBFIX                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-QSSKZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-QSSKZ                       '.
    FTPOST-FVAL = BBSEG-QSSKZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-QSSHB(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-QSSHB                       '.
    FTPOST-FVAL = BBSEG-QSSHB                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-QSFBT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-QSFBT                       '.
    FTPOST-FVAL = BBSEG-QSFBT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ESRNR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ESRNR                       '.
    FTPOST-FVAL = BBSEG-ESRNR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ESRPZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ESRPZ                       '.
    FTPOST-FVAL = BBSEG-ESRPZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ESRRE(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ESRRE                       '.
    FTPOST-FVAL = BBSEG-ESRRE                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-FDTAG(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-FDTAG                       '.
    FTPOST-FVAL = BBSEG-FDTAG                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-FDLEV(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-FDLEV                       '.
    FTPOST-FVAL = BBSEG-FDLEV                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ANLN1(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-ANLN1                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-ANLN1                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-ANLN1                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ANLN2(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-ANLN2                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-ANLN2                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-ANLN2                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-BZDAT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-BZDAT                       '.
    FTPOST-FVAL = BBSEG-BZDAT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ANBWA(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ANBWA                       '.
    FTPOST-FVAL = BBSEG-ANBWA                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ABPER(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ABPER                       '.
    FTPOST-FVAL = BBSEG-ABPER                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-GBETR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-GBETR                       '.
    FTPOST-FVAL = BBSEG-GBETR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-KURSR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-KURSR                       '.
    CONCATENATE PREFIX_P
    BBSEG-KURSR                       INTO FTPOST-FVAL.
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-MANSP(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-MANSP                       '.
    FTPOST-FVAL = BBSEG-MANSP                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-MSCHL(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-MSCHL                       '.
    FTPOST-FVAL = BBSEG-MSCHL                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-HBKID(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-HBKID                       '.
    FTPOST-FVAL = BBSEG-HBKID                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-BVTYP(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-BVTYP                       '.
    FTPOST-FVAL = BBSEG-BVTYP                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ANFBN(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ANFBN                       '.
    FTPOST-FVAL = BBSEG-ANFBN                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ANFBU(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ANFBU                       '.
    FTPOST-FVAL = BBSEG-ANFBU                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ANFBJ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ANFBJ                       '.
    FTPOST-FVAL = BBSEG-ANFBJ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-LZBKZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-LZBKZ                       '.
    FTPOST-FVAL = BBSEG-LZBKZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-LANDL(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-LANDL                       '.
    FTPOST-FVAL = BBSEG-LANDL                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-DIEKZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-DIEKZ                       '.
    FTPOST-FVAL = BBSEG-DIEKZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZOLLD(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZOLLD                       '.
    FTPOST-FVAL = BBSEG-ZOLLD                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ZOLLT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-ZOLLT                       '.
    FTPOST-FVAL = BBSEG-ZOLLT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-VRSDT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-VRSDT                       '.
    FTPOST-FVAL = BBSEG-VRSDT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-VRSKZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-VRSKZ                       '.
    FTPOST-FVAL = BBSEG-VRSKZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-HZUON(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-HZUON                       '.
    FTPOST-FVAL = BBSEG-HZUON                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-REGUL(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-REGUL                       '.
    FTPOST-FVAL = BBSEG-REGUL                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-NAME1(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-NAME1                       '.
    FTPOST-FVAL = BBSEG-NAME1                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-NAME2(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-NAME2                       '.
    FTPOST-FVAL = BBSEG-NAME2                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-NAME3(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-NAME3                       '.
    FTPOST-FVAL = BBSEG-NAME3                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-NAME4(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-NAME4                       '.
    FTPOST-FVAL = BBSEG-NAME4                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-STRAS(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-STRAS                       '.
    FTPOST-FVAL = BBSEG-STRAS                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-ORT01(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-ORT01                       '.
    FTPOST-FVAL = BBSEG-ORT01                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-PSTLZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-PSTLZ                       '.
    FTPOST-FVAL = BBSEG-PSTLZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-LAND1(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-LAND1                       '.
    FTPOST-FVAL = BBSEG-LAND1                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-REGIO(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-REGIO                       '.
    FTPOST-FVAL = BBSEG-REGIO                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-BANKL(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-BANKL                       '.
    FTPOST-FVAL = BBSEG-BANKL                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-BANKS(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-BANKS                       '.
    FTPOST-FVAL = BBSEG-BANKS                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-BANKN(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-BANKN                       '.
    FTPOST-FVAL = BBSEG-BANKN                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-BKONT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-BKONT                       '.
    FTPOST-FVAL = BBSEG-BKONT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-STCD1(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-STCD1                       '.
    FTPOST-FVAL = BBSEG-STCD1                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-STCD2(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-STCD2                       '.
    FTPOST-FVAL = BBSEG-STCD2                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-MADAT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-MADAT                       '.
    FTPOST-FVAL = BBSEG-MADAT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-MANST(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-MANST                       '.
    FTPOST-FVAL = BBSEG-MANST                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-EGMLD(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-EGMLD                       '.
    FTPOST-FVAL = BBSEG-EGMLD                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-STCEG(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-STCEG                       '.
    FTPOST-FVAL = BBSEG-STCEG                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-STKZA(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-STKZA                       '.
    FTPOST-FVAL = BBSEG-STKZA                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-STKZU(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-STKZU                       '.
    FTPOST-FVAL = BBSEG-STKZU                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-PFACH(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-PFACH                       '.
    FTPOST-FVAL = BBSEG-PFACH                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-PSTL2(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-PSTL2                       '.
    FTPOST-FVAL = BBSEG-PSTL2                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-SPRAS(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-SPRAS                       '.
    FTPOST-FVAL = BBSEG-SPRAS                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-XINVE(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-XINVE                       '.
    FTPOST-FVAL = BBSEG-XINVE                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-NEWKO(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF BBKPF-TCODE = 'FBV1'.
      FTPOST-FNAM = 'RF05V-NEWKO                      '.
    ELSE.
      FTPOST-FNAM = 'RF05A-NEWKO                      '.
    ENDIF.
    FTPOST-FVAL = BBSEG-NEWKO                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-NEWBW(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF BBKPF-TCODE = 'FBV1'.
      FTPOST-FNAM = 'RF05V-NEWBW                      '.
    ELSE.
      FTPOST-FNAM = 'RF05A-NEWBW                      '.
    ENDIF.
    FTPOST-FVAL = BBSEG-NEWBW                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-KNRZE(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-KNRZE                       '.
    FTPOST-FVAL = BBSEG-KNRZE                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-HKONT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-HKONT                       '.
    FTPOST-FVAL = BBSEG-HKONT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-PRCTR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-PRCTR                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-PRCTR                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-PRCTR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-VERTN(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-VERTN                       '.
    FTPOST-FVAL = BBSEG-VERTN                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-VERTT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-VERTT                       '.
    FTPOST-FVAL = BBSEG-VERTT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-VBEWA(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-VBEWA                       '.
    FTPOST-FVAL = BBSEG-VBEWA                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-HWBAS(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-HWBAS                       '.
    FTPOST-FVAL = BBSEG-HWBAS                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-FWBAS(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-FWBAS                       '.
    FTPOST-FVAL = BBSEG-FWBAS                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-FIPOS(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-FIPOS                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-FIPOS                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-FIPOS                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-VNAME(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-VNAME                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-VNAME                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-VNAME                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-EGRUP(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-EGRUP                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-EGRUP                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-EGRUP                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-BTYPE(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-BTYPE                       '.
    FTPOST-FVAL = BBSEG-BTYPE                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-PAOBJNR(1)                  NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-PAOBJNR                     '.
    ELSE.
      FTPOST-FNAM = 'BSEG-PAOBJNR                     '.
    ENDIF.
    FTPOST-FVAL = BBSEG-PAOBJNR                    .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-KSTRG(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-KSTRG                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-KSTRG                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-KSTRG                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-IMKEY(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-IMKEY                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-IMKEY                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-IMKEY                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-VPTNR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-VPTNR                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-VPTNR                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-VPTNR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-NPLNR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-NPLNR                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-NPLNR                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-NPLNR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-VORNR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-VORNR                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-VORNR                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-VORNR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-XEGDR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-XEGDR                       '.
    FTPOST-FVAL = BBSEG-XEGDR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-RECID(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-RECID                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-RECID                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-RECID                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-PPRCT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-PPRCTR                      '.
    ELSE.
      FTPOST-FNAM = 'BSEG-PPRCT                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-PPRCT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-PROJK(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-PS_PSP_PNR                  '.
    ELSE.
      FTPOST-FNAM = 'BSEG-PROJK                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-PROJK                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-UZAWE(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-UZAWE                       '.
    FTPOST-FVAL = BBSEG-UZAWE                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-TXJCD(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-TXJCD                       '.
    FTPOST-FVAL = BBSEG-TXJCD                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-FISTL(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-FISTL                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-FISTL                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-FISTL                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-GEBER(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-GEBER                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-GEBER                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-GEBER                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-DMBE2(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-DMBE2                       '.
    FTPOST-FVAL = BBSEG-DMBE2                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-DMBE3(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-DMBE3                       '.
    FTPOST-FVAL = BBSEG-DMBE3                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-PARGB(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-PARGB                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-PARGB                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-PARGB                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-XREF1(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-XREF1                       '.
    FTPOST-FVAL = BBSEG-XREF1                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-XREF2(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-XREF2                       '.
    FTPOST-FVAL = BBSEG-XREF2                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-KBLNR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-KBLNR                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-KBLNR                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-KBLNR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-KBLPOS(1)                   NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-KBLPOS                      '.
    ELSE.
      FTPOST-FNAM = 'BSEG-KBLPOS                      '.
    ENDIF.
    FTPOST-FVAL = BBSEG-KBLPOS                     .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WDATE(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSED-WDATE                       '.
    FTPOST-FVAL = BBSEG-WDATE                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WGBKZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSED-WGBKZ                       '.
    FTPOST-FVAL = BBSEG-WGBKZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-XAKTZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSED-XAKTZ                       '.
    FTPOST-FVAL = BBSEG-XAKTZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WNAME(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSED-WNAME                       '.
    FTPOST-FVAL = BBSEG-WNAME                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WORT1(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSED-WORT1                       '.
    FTPOST-FVAL = BBSEG-WORT1                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WBZOG(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSED-WBZOG                       '.
    FTPOST-FVAL = BBSEG-WBZOG                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WORT2(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSED-WORT2                       '.
    FTPOST-FVAL = BBSEG-WORT2                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WBANK(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSED-WBANK                       '.
    FTPOST-FVAL = BBSEG-WBANK                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WLZBP(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-WLZBP                       '.
    FTPOST-FVAL = BBSEG-WLZBP                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-DISKP(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-DISKP                       '.
    FTPOST-FVAL = BBSEG-DISKP                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-DISKT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-DISKT                       '.
    FTPOST-FVAL = BBSEG-DISKT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WINFW(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-WINFW                       '.
    FTPOST-FVAL = BBSEG-WINFW                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WINHW(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-WINHW                       '.
    FTPOST-FVAL = BBSEG-WINHW                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WEVWV(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSED-WEVWV                       '.
    FTPOST-FVAL = BBSEG-WEVWV                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WSTAT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSED-WSTAT                       '.
    FTPOST-FVAL = BBSEG-WSTAT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WMWKZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-WMWKZ                       '.
    FTPOST-FVAL = BBSEG-WMWKZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WSTKZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-WSTKZ                       '.
    FTPOST-FVAL = BBSEG-WSTKZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-VBUND(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-VBUND                       '.
    FTPOST-FVAL = BBSEG-VBUND                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-FKBER(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-FKBER                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-FKBER                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-FKBER                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-DABRZ(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'LFVI9-DABRBEZ                    '.
    FTPOST-FVAL = BBSEG-DABRZ                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-XSTBA(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF BBKPF-TCODE = 'FBV1'.
      FTPOST-FNAM = 'RF05V-XSTBA                      '.
    ELSE.
      FTPOST-FNAM = 'RF05A-XSTBA                      '.
    ENDIF.
    FTPOST-FVAL = BBSEG-XSTBA                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-RSTGR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-RSTGR                       '.
    FTPOST-FVAL = BBSEG-RSTGR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-FIPEX(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-FIPOS                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-FIPOS                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-FIPEX                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-XNEGP(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-XNEGP                       '.
    FTPOST-FVAL = BBSEG-XNEGP                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-GRICD(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-GRICD                       '.
    FTPOST-FVAL = BBSEG-GRICD                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-GRIRG(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-GRIRG                       '.
    FTPOST-FVAL = BBSEG-GRIRG                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-GITYP(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-GITYP                       '.
    FTPOST-FVAL = BBSEG-GITYP                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-FITYP(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-FITYP                       '.
    FTPOST-FVAL = BBSEG-FITYP                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-STCDT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-STCDT                       '.
    FTPOST-FVAL = BBSEG-STCDT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-STKZN(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-STKZN                       '.
    FTPOST-FVAL = BBSEG-STKZN                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-STCD3(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-STCD3                       '.
    FTPOST-FVAL = BBSEG-STCD3                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-STCD4(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-STCD4                       '.
    FTPOST-FVAL = BBSEG-STCD4                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-XREF3(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-XREF3                       '.
    FTPOST-FVAL = BBSEG-XREF3                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-KIDNO(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-KIDNO                       '.
    FTPOST-FVAL = BBSEG-KIDNO                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-DTWS1(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-DTWS1                       '.
    FTPOST-FVAL = BBSEG-DTWS1                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-DTWS2(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-DTWS2                       '.
    FTPOST-FVAL = BBSEG-DTWS2                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-DTWS3(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-DTWS3                       '.
    FTPOST-FVAL = BBSEG-DTWS3                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-DTWS4(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-DTWS4                       '.
    FTPOST-FVAL = BBSEG-DTWS4                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-DTAWS(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-DTAWS                       '.
    FTPOST-FVAL = BBSEG-DTAWS                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-PYCUR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-PYCUR                       '.
    FTPOST-FVAL = BBSEG-PYCUR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-PYAMT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-PYAMT                       '.
    FTPOST-FVAL = BBSEG-PYAMT                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-BUPLA(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-BUPLA                       '.
    FTPOST-FVAL = BBSEG-BUPLA                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-SECCO(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-SECCO                       '.
    FTPOST-FVAL = BBSEG-SECCO                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-LSTAR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    IF XTBSL-KOART = 'S' OR XTBSL-KOART = 'A'.
      FTPOST-FNAM = 'COBL-LSTAR                       '.
    ELSE.
      FTPOST-FNAM = 'BSEG-LSTAR                       '.
    ENDIF.
    FTPOST-FVAL = BBSEG-LSTAR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-EGDEB(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = '*KNA1-KUNNR                      '.
    FTPOST-FVAL = BBSEG-EGDEB                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-WENR(1)                     NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'LFVI9-WENR                       '.
    FTPOST-FVAL = BBSEG-WENR                       .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-GENR(1)                     NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'LFVI9-GENR                       '.
    FTPOST-FVAL = BBSEG-GENR                       .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-GRNR(1)                     NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'LFVI9-GRNR                       '.
    FTPOST-FVAL = BBSEG-GRNR                       .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-MENR(1)                     NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'LFVI9-MENR                       '.
    FTPOST-FVAL = BBSEG-MENR                       .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-MIVE(1)                     NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'LFVI9-MIVE                       '.
    FTPOST-FVAL = BBSEG-MIVE                       .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-NKSL(1)                     NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'LFVI9-NKSL                       '.
    FTPOST-FVAL = BBSEG-NKSL                       .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-EMPSL(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'LFVI9-EMPSL                      '.
    FTPOST-FVAL = BBSEG-EMPSL                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-SVWNR(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'LFVI9-SVWNR                      '.
    FTPOST-FVAL = BBSEG-SVWNR                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-SBERI(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'LFVI9-SBERI                      '.
    FTPOST-FVAL = BBSEG-SBERI                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-KKBER(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-KKBER                       '.
    FTPOST-FVAL = BBSEG-KKBER                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-EMPFB(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-EMPFB                       '.
    FTPOST-FVAL = BBSEG-EMPFB                      .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-KURSR_M(1)                  NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEG-KURSR                       '.
    CONCATENATE PREFIX_M
    BBSEG-KURSR_M                     INTO FTPOST-FVAL.
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-J_1KFREPRE(1)               NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-J_1KFREPRE                  '.
    FTPOST-FVAL = BBSEG-J_1KFREPRE                 .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-J_1KFTBUS(1)                NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-J_1KFTBUS                   '.
    FTPOST-FVAL = BBSEG-J_1KFTBUS                  .
    APPEND FTPOST.
  ENDIF.
  IF BBSEG-J_1KFTIND(1)                NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'BSEC-J_1KFTIND                   '.
    FTPOST-FVAL = BBSEG-J_1KFTIND                  .
    APPEND FTPOST.
  ENDIF.
ENDFORM.

*eject
*----------------------------------------------------------
*        FORM FILL_FTTAX_WITH_BBTAX_DATA.
*----------------------------------------------------------
FORM FILL_FTTAX_WITH_BBTAX_DATA.
  CHECK FL_CHECK = SPACE.
  CHECK FUNCTION NE 'D'.
  CLEAR FTTAX.
  IF BBTAX-FWSTE(1)                    NE NODATA.
    FTTAX-FWSTE = BBTAX-FWSTE .
  ENDIF.
  IF BBTAX-MWSKZ(1)                    NE NODATA.
    FTTAX-MWSKZ = BBTAX-MWSKZ .
  ENDIF.
  IF BBTAX-BSCHL(1)                    NE NODATA.
    FTTAX-BSCHL = BBTAX-BSCHL .
  ENDIF.
  IF BBTAX-TXJCD(1)                    NE NODATA.
    FTTAX-TXJCD = BBTAX-TXJCD .
  ENDIF.
  IF BBTAX-HWSTE(1)                    NE NODATA.
    FTTAX-HWSTE = BBTAX-HWSTE .
  ENDIF.
  IF BBTAX-KSCHL(1)                    NE NODATA.
    FTTAX-KSCHL = BBTAX-KSCHL .
  ENDIF.
  IF BBTAX-H2STE(1)                    NE NODATA.
    FTTAX-H2STE = BBTAX-H2STE .
  ENDIF.
  IF BBTAX-H3STE(1)                    NE NODATA.
    FTTAX-H3STE = BBTAX-H3STE .
  ENDIF.
  IF BBTAX-SENDE(1)                    NE NODATA.
    FTTAX-SENDE = BBTAX-SENDE .
  ENDIF.
  IF NOT FTTAX IS INITIAL.
    APPEND FTTAX.
  ENDIF.
ENDFORM.

*eject
*----------------------------------------------------------
*        FORM FILL_FTCLEAR_WITH_BSELP_DATA.
*----------------------------------------------------------
FORM FILL_FTCLEAR_WITH_BSELP_DATA.
  CHECK FL_CHECK = SPACE.
  IF BSELP-FELDN_1(1)                  NE NODATA
  OR BSELP-SLVON_1(1)                  NE NODATA
  OR BSELP-SLBIS_1(1)                  NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_1(1)                  NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_1                    .
    ENDIF.
    IF BSELP-SLVON_1(1)                  NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_1                    .
    ENDIF.
    IF BSELP-SLBIS_1(1)                  NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_1                    .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_2(1)                  NE NODATA
  OR BSELP-SLVON_2(1)                  NE NODATA
  OR BSELP-SLBIS_2(1)                  NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_2(1)                  NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_2                    .
    ENDIF.
    IF BSELP-SLVON_2(1)                  NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_2                    .
    ENDIF.
    IF BSELP-SLBIS_2(1)                  NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_2                    .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_3(1)                  NE NODATA
  OR BSELP-SLVON_3(1)                  NE NODATA
  OR BSELP-SLBIS_3(1)                  NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_3(1)                  NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_3                    .
    ENDIF.
    IF BSELP-SLVON_3(1)                  NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_3                    .
    ENDIF.
    IF BSELP-SLBIS_3(1)                  NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_3                    .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_4(1)                  NE NODATA
  OR BSELP-SLVON_4(1)                  NE NODATA
  OR BSELP-SLBIS_4(1)                  NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_4(1)                  NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_4                    .
    ENDIF.
    IF BSELP-SLVON_4(1)                  NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_4                    .
    ENDIF.
    IF BSELP-SLBIS_4(1)                  NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_4                    .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_5(1)                  NE NODATA
  OR BSELP-SLVON_5(1)                  NE NODATA
  OR BSELP-SLBIS_5(1)                  NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_5(1)                  NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_5                    .
    ENDIF.
    IF BSELP-SLVON_5(1)                  NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_5                    .
    ENDIF.
    IF BSELP-SLBIS_5(1)                  NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_5                    .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_6(1)                  NE NODATA
  OR BSELP-SLVON_6(1)                  NE NODATA
  OR BSELP-SLBIS_6(1)                  NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_6(1)                  NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_6                    .
    ENDIF.
    IF BSELP-SLVON_6(1)                  NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_6                    .
    ENDIF.
    IF BSELP-SLBIS_6(1)                  NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_6                    .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_7(1)                  NE NODATA
  OR BSELP-SLVON_7(1)                  NE NODATA
  OR BSELP-SLBIS_7(1)                  NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_7(1)                  NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_7                    .
    ENDIF.
    IF BSELP-SLVON_7(1)                  NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_7                    .
    ENDIF.
    IF BSELP-SLBIS_7(1)                  NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_7                    .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_8(1)                  NE NODATA
  OR BSELP-SLVON_8(1)                  NE NODATA
  OR BSELP-SLBIS_8(1)                  NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_8(1)                  NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_8                    .
    ENDIF.
    IF BSELP-SLVON_8(1)                  NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_8                    .
    ENDIF.
    IF BSELP-SLBIS_8(1)                  NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_8                    .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_9(1)                  NE NODATA
  OR BSELP-SLVON_9(1)                  NE NODATA
  OR BSELP-SLBIS_9(1)                  NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_9(1)                  NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_9                    .
    ENDIF.
    IF BSELP-SLVON_9(1)                  NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_9                    .
    ENDIF.
    IF BSELP-SLBIS_9(1)                  NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_9                    .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_10(1)                 NE NODATA
  OR BSELP-SLVON_10(1)                 NE NODATA
  OR BSELP-SLBIS_10(1)                 NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_10(1)                 NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_10                   .
    ENDIF.
    IF BSELP-SLVON_10(1)                 NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_10                   .
    ENDIF.
    IF BSELP-SLBIS_10(1)                 NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_10                   .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_11(1)                 NE NODATA
  OR BSELP-SLVON_11(1)                 NE NODATA
  OR BSELP-SLBIS_11(1)                 NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_11(1)                 NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_11                   .
    ENDIF.
    IF BSELP-SLVON_11(1)                 NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_11                   .
    ENDIF.
    IF BSELP-SLBIS_11(1)                 NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_11                   .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_12(1)                 NE NODATA
  OR BSELP-SLVON_12(1)                 NE NODATA
  OR BSELP-SLBIS_12(1)                 NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_12(1)                 NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_12                   .
    ENDIF.
    IF BSELP-SLVON_12(1)                 NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_12                   .
    ENDIF.
    IF BSELP-SLBIS_12(1)                 NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_12                   .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_13(1)                 NE NODATA
  OR BSELP-SLVON_13(1)                 NE NODATA
  OR BSELP-SLBIS_13(1)                 NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_13(1)                 NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_13                   .
    ENDIF.
    IF BSELP-SLVON_13(1)                 NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_13                   .
    ENDIF.
    IF BSELP-SLBIS_13(1)                 NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_13                   .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_14(1)                 NE NODATA
  OR BSELP-SLVON_14(1)                 NE NODATA
  OR BSELP-SLBIS_14(1)                 NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_14(1)                 NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_14                   .
    ENDIF.
    IF BSELP-SLVON_14(1)                 NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_14                   .
    ENDIF.
    IF BSELP-SLBIS_14(1)                 NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_14                   .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_15(1)                 NE NODATA
  OR BSELP-SLVON_15(1)                 NE NODATA
  OR BSELP-SLBIS_15(1)                 NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_15(1)                 NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_15                   .
    ENDIF.
    IF BSELP-SLVON_15(1)                 NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_15                   .
    ENDIF.
    IF BSELP-SLBIS_15(1)                 NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_15                   .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_16(1)                 NE NODATA
  OR BSELP-SLVON_16(1)                 NE NODATA
  OR BSELP-SLBIS_16(1)                 NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_16(1)                 NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_16                   .
    ENDIF.
    IF BSELP-SLVON_16(1)                 NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_16                   .
    ENDIF.
    IF BSELP-SLBIS_16(1)                 NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_16                   .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_17(1)                 NE NODATA
  OR BSELP-SLVON_17(1)                 NE NODATA
  OR BSELP-SLBIS_17(1)                 NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_17(1)                 NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_17                   .
    ENDIF.
    IF BSELP-SLVON_17(1)                 NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_17                   .
    ENDIF.
    IF BSELP-SLBIS_17(1)                 NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_17                   .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
  IF BSELP-FELDN_18(1)                 NE NODATA
  OR BSELP-SLVON_18(1)                 NE NODATA
  OR BSELP-SLBIS_18(1)                 NE NODATA.
    CLEAR FTCLEAR.
    CLEAR XFTCLEAR.
    FTCLEAR = SAVE_FTCLEAR.
    IF BSELP-FELDN_18(1)                 NE NODATA.
      FTCLEAR-SELFD = BSELP-FELDN_18                   .
    ENDIF.
    IF BSELP-SLVON_18(1)                 NE NODATA.
      FTCLEAR-SELVON = BSELP-SLVON_18                   .
    ENDIF.
    IF BSELP-SLBIS_18(1)                 NE NODATA.
      FTCLEAR-SELBIS = BSELP-SLBIS_18                   .
    ENDIF.
    APPEND FTCLEAR.
  ENDIF.
ENDFORM.

*eject
*----------------------------------------------------------
*        FORM FILL_FTPOST_WITH_BWITH_DATA USING COUNT
*----------------------------------------------------------
FORM FILL_FTPOST_WITH_BWITH_DATA USING COUNT.
  CHECK FL_CHECK = SPACE.
  CHECK FUNCTION NE 'D'.
  CLEAR FTPOST.
  FTPOST-STYPE = 'P'.
  FTPOST-COUNT = COUNT.
  IF BWITH-WITHT(1)                    NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'WITH_ITEM-WITHT                  '.
    FTPOST-FVAL = BWITH-WITHT                      .
    APPEND FTPOST.
  ENDIF.
  IF BWITH-WT_WITHCD(1)                NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'WITH_ITEM-WT_WITHCD              '.
    FTPOST-FVAL = BWITH-WT_WITHCD                  .
    APPEND FTPOST.
  ENDIF.
  IF BWITH-WT_QSSHB(1)                 NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'WITH_DIALG-WT_BASE               '.
    FTPOST-FVAL = BWITH-WT_QSSHB                   .
    APPEND FTPOST.
  ENDIF.
  IF BWITH-WT_QBUIHB(1)                NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'WITH_DIALG-WT_AMOUNT             '.
    FTPOST-FVAL = BWITH-WT_QBUIHB                  .
    APPEND FTPOST.
  ENDIF.
  IF BWITH-WT_WWRBTR(1)                NE NODATA.
    CLEAR: FTPOST-FNAM, FTPOST-FVAL.
    FTPOST-FNAM = 'WITH_DIALG-WT_WITHH              '.
    FTPOST-FVAL = BWITH-WT_WWRBTR                  .
    APPEND FTPOST.
  ENDIF.
ENDFORM.
