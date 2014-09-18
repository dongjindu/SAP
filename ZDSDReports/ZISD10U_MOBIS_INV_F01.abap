*----------------------------------------------------------------------*
*   INCLUDE ZISD10U_MOBIS_INV_F01                                      *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_DATA USING RC
                       US_SCREEN.

* READING THE INVOICE DOC INFORMATION

* READING THE PURCHASE ORDER NUMBER AND
* ITEM LEVEL INOFRMAITON

  SELECT B~VBELN B~POSNR A~BSTNK B~AUPOS
         B~FKIMG B~NTGEW C~NETPR
         B~ERDAT B~AUBEL B~NETWR C~KDMAT
         INTO CORRESPONDING FIELDS OF TABLE IT_FILE
    FROM  ( ( VBAK AS A
    INNER JOIN VBRP AS B
           ON A~VBELN = B~AUBEL )
    INNER JOIN VBAP AS C
           ON B~AUBEL = C~VBELN AND
              B~AUPOS = C~POSNR  )
    WHERE B~VBELN = NAST-OBJKY.

*


* GET THE DELIVIED QTY
  LOOP AT IT_FILE.
    SELECT VBELN VBELV POSNV RFMNG
     INTO TABLE LT_DEL
     FROM VBFA
     WHERE "VBELN = IT_FILE-VBELN  AND
           POSNV = IT_FILE-AUPOS  AND
*           VBTYP_V = 'C'          AND
           VBTYP_N = 'J'          AND
           VBELV = IT_FILE-AUBEL.
    IF SY-SUBRC EQ 0.
      CLEAR: L_DELQTY.
      LOOP AT LT_DEL.
        L_DELQTY = L_DELQTY + LT_DEL-RFMNG.
      ENDLOOP.
      IT_FILE-LFIMG = L_DELQTY.
    ENDIF.

    SELECT SINGLE KDMAT  INTO
      IT_FILE-MATNR
      FROM VBAP
      WHERE VBELN = IT_FILE-aubel and
            POSNR = IT_FILE-AUPOS.

*   GET THE CURRENCY AND TOTAL AMOUNT
    SELECT SINGLE WAERK  INTO
      IT_FILE-WAERK "IT_FILE-NETWR
      FROM VBRK
      WHERE VBELN = IT_FILE-VBELN.
*   GET THE P.O ITEM NUMBER
*   THIS NUMBER IS FROM BSTKD(P.O NUMBER) OF TABLE VBKD
*   Note:Mobis sales order creation program put the PO Item
*   number in this field
    SELECT SINGLE BSTKD INTO IT_FILE-BSTKD
      FROM VBKD
      WHERE VBELN = IT_FILE-AUBEL AND
            POSNR = IT_FILE-AUPOS.

    MODIFY IT_FILE.

  ENDLOOP.
* SUMARIZE THE DATA FOR SAME SALES ITEM
  SORT IT_FILE BY AUBEL AUPOS.
  clear: wa_file, wa_filet.
  LOOP AT IT_FILE.
    L_TABIX = SY-TABIX + 1.
    PERFORM ADD_RECORD.
    CLEAR: WA_FILE.
    READ TABLE IT_FILE INTO WA_FILE INDEX L_TABIX.
    IF IT_FILE-AUBEL = WA_FILE-AUBEL AND
       IT_FILE-AUPOS = WA_FILE-AUPOS AND
       IT_FILE-MATNR = WA_FILE-MATNR.
       delete it_file.
       continue.
    ELSE.
*       IT_FILE-LFIMG = WA_FILET-LFIMG.
       IT_FILE-FKIMG = WA_FILET-FKIMG.
       IT_FILE-INCST = WA_FILET-INCST.
       IT_FILE-FRCST = WA_FILET-FRCST.
       IT_FILE-NETWR = WA_FILET-NETWR.
       IT_FILE-NTGEW = WA_FILET-NTGEW.
       MODIFY IT_FILE.
       CLEAR: WA_FILET.
    ENDIF.

  ENDLOOP.
* transfer data to output format
  LOOP AT IT_FILE.
    IT_OUT-VBELN    =  IT_FILE-VBELN.
    IT_OUT-ERDAT    =  IT_FILE-ERDAT.
    IT_OUT-BSTNK    =  IT_FILE-BSTNK.
    IT_OUT-BSTKD    =  IT_FILE-BSTKD(4).
    IT_OUT-MATNR    =  IT_FILE-MATNR.
    WRITE IT_FILE-LFIMG TO IT_OUT-LFIMG USING EDIT MASK 'RR_______'.
    WRITE IT_FILE-FKIMG TO IT_OUT-FKIMG USING EDIT MASK 'RR_______'.
    IT_OUT-WAERK    =  IT_FILE-WAERK.
    WRITE IT_FILE-INCST TO IT_OUT-INCST USING EDIT MASK 'RR_________'.
    WRITE IT_FILE-FRCST TO IT_OUT-FRCST USING EDIT MASK 'RR_________'.
    WRITE IT_FILE-MWSBK TO IT_OUT-MWSBK USING EDIT MASK 'RR_________'.
    WRITE IT_FILE-NETPR TO IT_OUT-NETPR USING EDIT MASK 'RR_________'.
    WRITE IT_FILE-NETWR TO IT_OUT-NETWR USING EDIT MASK 'RR___________'
.
    WRITE IT_FILE-NTGEW TO IT_OUT-NTGEW USING EDIT MASK 'RR_______'.

    APPEND IT_OUT.

  ENDLOOP.
* ADD LEADING '0' FOR NUMERIC DATA
  LOOP AT IT_OUT.
    PERFORM ADD_LEADZERO USING IT_OUT-LFIMG.
    PERFORM ADD_LEADZERO USING IT_OUT-FKIMG.
    PERFORM ADD_LEADZERO USING IT_OUT-INCST.
    PERFORM ADD_LEADZERO USING IT_OUT-FRCST.
    PERFORM ADD_LEADZERO USING IT_OUT-MWSBK.
    PERFORM ADD_LEADZERO USING IT_OUT-NETPR.
    PERFORM ADD_LEADZERO USING IT_OUT-NETWR.
    PERFORM ADD_LEADZERO USING IT_OUT-NTGEW.
    MODIFY IT_OUT.
  ENDLOOP.
  PERFORM MAKE_HEADER.

  LOOP AT IT_OUT.
    OPEN DATASET W_DSN IN TEXT MODE FOR APPENDING.
    TRANSFER IT_OUT TO W_DSN.
  ENDLOOP.

  CLOSE DATASET W_DSN.

  IF SY-SUBRC = 0.
    RC = 0.
  ENDIF.

ENDFORM.                   " MODIFY_DATA

*FORM MODIFY_DATA USING RC
*                       US_SCREEN.
*  REFRESH IT_DOWNFILE. CLEAR IT_DOWNFILE.
*  CLEAR : W_NUMC_UP, W_NUMC_TX, W_NUMC_FC, W_NUMC_OA, W_NUMC_TA.
*
*  SELECT SINGLE *
*         FROM VBRK WHERE VBELN EQ NAST-OBJKY.
*  IT_DOWNFILE-RECORD+99(03) = VBRK-VBELN.  "1
*  IT_DOWNFILE-RECORD+99(03) = VBRK-ERDAT.  "2
*
*  SELECT *
*         FROM VBRP WHERE VBELN EQ NAST-OBJKY.
*    IT_DOWNFILE-RECORD+30(10) = VBRP-POSNR. "4
*    IT_DOWNFILE-RECORD+54(15) = VBRP-MATNR. "5
*    W_NUMC_IV = VBRP-FKIMG.
*    IT_DOWNFILE-RECORD+80(07) = W_NUMC_IV.  "7
*    IT_DOWNFILE-RECORD+80(07) = 'USD'.      "8
*    IT_DOWNFILE-RECORD+91(08) = VBRP-NTGEW. "14
*
*    CLEAR W_NUMC_GI.
*    SELECT *
*           FROM VBFA WHERE VBELV EQ VBRP-AUBEL
*                     AND   POSNV EQ VBRP-AUPOS
*                     AND   ( VBTYP_N EQ 'R' OR
*                             VBTYP_N EQ 'h' ).
*      IF VBFA-VBTYP_N = 'R'.
*        W_NUMC_GI = W_NUMC_GI + VBFA-RFMNG.
*      ELSE. "h
*        W_NUMC_GI = W_NUMC_GI - VBFA-RFMNG.
*      ENDIF.
*    ENDSELECT.
*    IT_DOWNFILE-RECORD+73(07) = W_NUMC_GI.    "6
*
*    SELECT SINGLE *
*           FROM VBAK WHERE VBELN EQ VBRP-AUBEL.
*    IT_DOWNFILE-RECORD+0(15) = VBAK-BSTNK.  "3
**
*    SELECT SINGLE *
*           FROM KONV WHERE KNUMV EQ VBRK-KNUMV
*                     AND   KPOSN EQ VBRP-POSNR
*                     AND   KSCHL EQ 'ZOPP'.
*    W_KWERT = KONV-KWERT.
*    SELECT SINGLE *
*           FROM KONV WHERE KNUMV EQ VBRK-KNUMV
*                     AND   KPOSN EQ VBRP-POSNR
*                     AND   KSCHL EQ 'ZP07'.
*    W_KWERT = ( W_KWERT * KONV-KWERT ) + W_KWERT.
*    IF KONV-WAERS IS INITIAL.
*      WRITE W_KWERT TO W_CHAR_9 CURRENCY 'USD'.
*    ELSE.
*      WRITE W_KWERT TO W_CHAR_9 CURRENCY KONV-WAERS.
*    ENDIF.
*    W_NUMC_UP = W_CHAR_9.
*    IT_DOWNFILE-RECORD+102(09) = W_NUMC_UP.    "9
*    IF VBRK-FKART = 'ZPS1'.
*      IT_DOWNFILE-RECORD+102(01) = '-'.        "
*    ENDIF.
*    SELECT SINGLE *
*           FROM KONV WHERE KNUMV EQ VBRK-KNUMV
*                     AND   KPOSN EQ VBRP-POSNR
*                     AND   KSCHL EQ 'ZTAX'.
*    IF KONV-WAERS IS INITIAL.
*      WRITE W_KWERT TO W_CHAR_9 CURRENCY 'USD'.
*    ELSE.
*      WRITE W_KWERT TO W_CHAR_9 CURRENCY KONV-WAERS.
*    ENDIF.
*    W_NUMC_TX = W_CHAR_9.
*    IT_DOWNFILE-RECORD+111(09) = W_NUMC_TX.    "10
*    IF VBRK-FKART = 'ZPS1'.
*      IT_DOWNFILE-RECORD+111(01) = '-'.        "
*    ENDIF.
*    SELECT SINGLE *
*           FROM KONV WHERE KNUMV EQ VBRK-KNUMV
*                     AND   KPOSN EQ VBRP-POSNR
*                     AND   KSCHL EQ 'ZFLC'.
*    IF KONV-WAERS IS INITIAL.
*      WRITE W_KWERT TO W_CHAR_9 CURRENCY 'USD'.
*    ELSE.
*      WRITE W_KWERT TO W_CHAR_9 CURRENCY KONV-WAERS.
*    ENDIF.
*    W_NUMC_FC = W_CHAR_9.
*    IT_DOWNFILE-RECORD+120(09) = W_NUMC_FC.    "11
*    IF VBRK-FKART = 'ZPS1'.
*      IT_DOWNFILE-RECORD+120(01) = '-'.        "
*    ENDIF.
*    SELECT SINGLE *
*           FROM KONV WHERE KNUMV EQ VBRK-KNUMV
*                     AND   KPOSN EQ VBRP-POSNR
*                     AND   KSCHL EQ 'ZOAM'.
*    IF KONV-WAERS IS INITIAL.
*      WRITE W_KWERT TO W_CHAR_9 CURRENCY 'USD'.
*    ELSE.
*      WRITE W_KWERT TO W_CHAR_9 CURRENCY KONV-WAERS.
*    ENDIF.
*    W_NUMC_OA = W_CHAR_9.
*    IT_DOWNFILE-RECORD+129(09) = W_NUMC_OA.    "12
*    IF VBRK-FKART = 'ZPS1'.
*      IT_DOWNFILE-RECORD+129(01) = '-'.        "
*    ENDIF.
*
*    W_NUMC_TA = ( W_NUMC_UP + W_NUMC_TX + W_NUMC_FC + W_NUMC_OA )
*                * W_NUMC_IV.
*    IT_DOWNFILE-RECORD+138(11) = W_NUMC_TA.    "13
*    IF VBRK-FKART = 'ZPS1'.
*      IT_DOWNFILE-RECORD+138(01) = '-'.        "
*    ENDIF.
*
*    APPEND IT_DOWNFILE.
*  ENDSELECT.
*
*  PERFORM MAKE_HEADER.
*  LOOP AT IT_DOWNFILE.
*    OPEN DATASET W_DSN IN TEXT MODE FOR APPENDING.
*    TRANSFER IT_DOWNFILE TO W_DSN.
*  ENDLOOP.
*
*  CLOSE DATASET W_DSN.
*
*  IF SY-SUBRC = 0.
*    RC = 0.
*  ENDIF.
*ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_HEADER
*&---------------------------------------------------------------------*
FORM MAKE_HEADER.
  DATA : W_NUMC_8(8) TYPE N.

  DATA: W_MONTH TYPE C,
        W_DATE(2) TYPE C,
        W_SER     TYPE C.

  CASE SY-DATUM+4(2).
    WHEN '10'.
      W_MONTH = 'A'.
    WHEN '11'.
      W_MONTH = 'B'.
    WHEN '12'.
      W_MONTH = 'C'.
    WHEN OTHERS.
      W_MONTH = SY-DATUM+5(1).
  ENDCASE.
  W_DATE  = SY-DATUM+6(2).

  IT_HEADER-RECORD+0(5) = 'HMINV'.
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

  PERFORM MAKE_SEQ USING IT_HEADER-RECORD+8(1) CHANGING W_SER.

  DESCRIBE TABLE IT_OUT LINES W_NUMC_8.

  IT_HEADER-RECORD+9(8) = W_NUMC_8.
  IT_HEADER-RECORD+25(1) = 'S'.

  APPEND IT_HEADER.

  SELECT SINGLE *
         FROM VBRK WHERE VBELN EQ NAST-OBJKY.


  IF VBRK-FKART = 'ZPS1'.
**    CONCATENATE  '/sapmnt/' SY-SYSID '/EDI/'
**                 'm_invc_' VBAK-BSTNK '_' VBRK-VBELN
**                 '.txt'
**                 INTO W_DSN.
    W_DSN = '/usr/sap/EDI_SAP/MINVC'.
  ELSE.
**    CONCATENATE  '/sapmnt/' SY-SYSID '/EDI/'
**                 'm_inv_'  VBAK-BSTNK '_' VBRK-VBELN
**                 '.txt'
**                 INTO W_DSN.
    W_DSN = '/usr/sap/EDI_SAP/MINV'.
  ENDIF.

  CONCATENATE W_DSN W_MONTH W_DATE W_SER '.txt' INTO W_DSN.

  LOOP AT IT_HEADER.
    OPEN DATASET W_DSN IN TEXT MODE FOR APPENDING.
    TRANSFER IT_HEADER TO W_DSN.
  ENDLOOP.
ENDFORM.                    " MAKE_HEADER
*&---------------------------------------------------------------------*
*&      Form  MAKE_SEQ
*&---------------------------------------------------------------------*
FORM MAKE_SEQ USING CNT CHANGING P_SER.
  TABLES : ZTSD_SEQ.

  SELECT SINGLE *
         FROM ZTSD_SEQ
        WHERE ZKEY EQ IT_HEADER-RECORD+0(8).
  IF SY-SUBRC = 0.
    ZTSD_SEQ-ZSEQ = ZTSD_SEQ-ZSEQ + 1.
    MODIFY ZTSD_SEQ.
    CNT = ZTSD_SEQ-ZSEQ.
    P_SER = ZTSD_SEQ-ZSEQ.
  ELSE.
    ZTSD_SEQ-ZKEY = IT_HEADER-RECORD+0(8).
    ZTSD_SEQ-ZSEQ = '1'.
    INSERT ZTSD_SEQ.
    CNT = 1.
    P_SER = '1'.
  ENDIF.
ENDFORM.                    " MAKE_SEQ

***********************************************************************
*   FORM MODIFY_DATA  --CREATED BY CHRIS ON 03/03/2005
***********************************************************************
*&---------------------------------------------------------------------*
*&      Form  ADD_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_RECORD.

   WA_FILET-LFIMG = WA_FILET-LFIMG + IT_FILE-LFIMG.
   WA_FILET-FKIMG = WA_FILET-FKIMG + IT_FILE-FKIMG.
   WA_FILET-INCST = WA_FILET-INCST + IT_FILE-INCST.
   WA_FILET-FRCST = WA_FILET-FRCST + IT_FILE-FRCST.
   WA_FILET-NETWR = WA_FILET-NETWR + IT_FILE-NETWR.
   WA_FILET-NTGEW = WA_FILET-NTGEW + IT_FILE-NTGEW.

ENDFORM.                    " ADD_RECORD
*&---------------------------------------------------------------------*
*&      Form  ADD_LEADZERO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_LFIMG  text
*----------------------------------------------------------------------*
FORM ADD_LEADZERO USING    P_FIELD.
   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
         INPUT  = P_FIELD
      IMPORTING
         OUTPUT = P_FIELD.

ENDFORM.                    " ADD_LEADZERO
