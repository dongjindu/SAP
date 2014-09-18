*----------------------------------------------------------------------*
*   INCLUDE ZISD09U_MOBIS_ETD_F01                                      *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_DATA USING RC
                       US_SCREEN.
  REFRESH IT_DOWNFILE. CLEAR IT_DOWNFILE.
  CLEAR : W_NUMC_OR, W_NUMC_DL, W_NUMC_OP.

  SELECT SINGLE *
         FROM LIKP WHERE VBELN EQ NAST-OBJKY.
  IT_DOWNFILE-RECORD+0(10)  = LIKP-VBELN.      "1
  IT_DOWNFILE-RECORD+10(08) = LIKP-WADAT_IST.  "2

  SELECT *
         FROM LIPS WHERE VBELN EQ NAST-OBJKY.
    IT_DOWNFILE-RECORD+40(18) = LIPS-kdmat.    "6
    IT_DOWNFILE-RECORD+58(15) = LIPS-ARKTX.    "7

    CLEAR W_NUMC_DL.
    SELECT *
           FROM VBFA WHERE VBELV EQ LIPS-VGBEL
                     AND   POSNV EQ LIPS-VGPOS
                     AND   ( VBTYP_N EQ 'R' OR VBTYP_N EQ 'h' )
                     AND   VBTYP_V EQ 'C'.
      IF VBFA-VBTYP_N EQ 'R'.
        W_NUMC_DL = W_NUMC_DL + VBFA-RFMNG.
      ELSE.
        W_NUMC_DL = W_NUMC_DL - VBFA-RFMNG.
      ENDIF.
    ENDSELECT.
    IT_DOWNFILE-RECORD+80(07) = W_NUMC_DL.     "9

    SELECT SINGLE *
           FROM VBKD WHERE VBELN EQ LIPS-VGBEL
                     AND   POSNR EQ LIPS-VGPOS.
    IF SY-SUBRC <> 0.
      SELECT SINGLE *
             FROM VBKD WHERE VBELN EQ LIPS-VGBEL
                       AND   POSNR EQ '000000'.
    ENDIF.
    IT_DOWNFILE-RECORD+36(04) = VBKD-BSTKD.    "5

    SELECT SINGLE *
           FROM VBAK WHERE VBELN EQ LIPS-VGBEL.
    IT_DOWNFILE-RECORD+18(08) = VBAK-ERDAT.    "3
    IT_DOWNFILE-RECORD+26(10) = VBAK-BSTNK.    "4

    SELECT SINGLE *
           FROM VBAP WHERE VBELN EQ LIPS-VGBEL
                     AND   POSNR EQ LIPS-VGPOS.
    W_NUMC_OR = VBAP-KWMENG.
    IT_DOWNFILE-RECORD+73(07) = W_NUMC_OR.     "8

    W_NUMC_OP = W_NUMC_OR - W_NUMC_DL.
    IT_DOWNFILE-RECORD+87(07) = W_NUMC_OP.     "10

    APPEND IT_DOWNFILE.
  ENDSELECT.

  PERFORM MAKE_HEADER.
  LOOP AT IT_DOWNFILE.
    OPEN DATASET W_DSN IN TEXT MODE FOR APPENDING.
    TRANSFER IT_DOWNFILE TO W_DSN.
  ENDLOOP.

  CLOSE DATASET W_DSN.

  IF SY-SUBRC = 0.
    RC = 0.
  ENDIF.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_HEADER
*&---------------------------------------------------------------------*
FORM MAKE_HEADER.
  DATA : W_NUMC_8(8) TYPE N.
  data: w_month   type c,
        w_date(2) type c,
        w_ser     type c.

  case sy-datum+4(2).
    when '10'.
      w_month = 'A'.
    when '11'.
      w_month = 'B'.
    when '12'.
      w_month = 'C'.
    when others.
      w_month = sy-datum+5(1).
  endcase.
  w_date  = sy-datum+6(2).

  IT_HEADER-RECORD+0(5) = 'HMOSR'.
  IT_HEADER-RECORD+17(4) = SY-DATUM+4(4).
  IT_HEADER-RECORD+21(4) = SY-DATUM+0(4).

  IF IT_HEADER-RECORD+17(1) = '0'.
    IT_HEADER-RECORD+5(1) = IT_HEADER-RECORD+18(1).
  ELSEIF IT_HEADER-RECORD+17(2) = '10'.
    IT_HEADER-RECORD+5(1) = 'A'.
  ELSEIF IT_HEADER-RECORD+17(2) = '11'.
    IT_HEADER-RECORD+5(1) = 'B'.
  ELSEIF IT_HEADER-RECORD+17(2) = '12'.
    IT_HEADER-RECORD+5(1) = 'C'.
  ENDIF.
  IT_HEADER-RECORD+6(2) = IT_HEADER-RECORD+19(2).

  PERFORM MAKE_SEQ USING IT_HEADER-RECORD+8(1) changing w_ser.

  DESCRIBE TABLE IT_DOWNFILE LINES W_NUMC_8.

  IT_HEADER-RECORD+9(8) = W_NUMC_8.
  IT_HEADER-RECORD+25(1) = 'S'.

  APPEND IT_HEADER.

**  CONCATENATE  '/sapmnt/' SY-SYSID '/EDI/'
**               'm_osr_'  VBAK-BSTNK '_' VBFA-VBELN
**               '.txt'
**               INTO W_DSN.
  w_dsn = '/usr/sap/EDI_SAP/MOSR'.
  concatenate w_dsn w_month w_date w_ser '.txt' into w_dsn.
  LOOP AT IT_HEADER.
    OPEN DATASET W_DSN IN TEXT MODE FOR APPENDING.
    TRANSFER IT_HEADER TO W_DSN.
  ENDLOOP.
ENDFORM.                    " MAKE_HEADER
*&---------------------------------------------------------------------*
*&      Form  MAKE_SEQ
*&---------------------------------------------------------------------*
FORM MAKE_SEQ USING CNT changing p_ser.
  TABLES : ZTSD_SEQ.

  SELECT SINGLE *
         FROM ZTSD_SEQ
        WHERE ZKEY EQ IT_HEADER-RECORD+0(8).
  IF SY-SUBRC = 0.
    ZTSD_SEQ-ZSEQ = ZTSD_SEQ-ZSEQ + 1.
    MODIFY ZTSD_SEQ.
    CNT = ZTSD_SEQ-ZSEQ.
    p_ser = ZTSD_SEQ-ZSEQ.
  ELSE.
    ZTSD_SEQ-ZKEY = IT_HEADER-RECORD+0(8).
    ZTSD_SEQ-ZSEQ = '1'.
    INSERT ZTSD_SEQ.
    CNT = 1.
    p_ser = '1'.
  ENDIF.
ENDFORM.                    " MAKE_SEQ
