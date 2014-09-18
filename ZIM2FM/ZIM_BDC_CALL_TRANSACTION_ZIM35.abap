FUNCTION ZIM_BDC_CALL_TRANSACTION_ZIM35.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFREQNO) LIKE  ZTREQST-ZFREQNO
*"     VALUE(W_ZFAMDNO) LIKE  ZTREQST-ZFAMDNO
*"     VALUE(MODE) TYPE  C DEFAULT 'N'
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      REQ_ERROR
*"----------------------------------------------------------------------
DATA : L_KURSF(12),
       L_FFACT(11).

   SELECT SINGLE * FROM ZTREQHD
          WHERE ZFREQNO  EQ  W_ZFREQNO.
   IF SY-SUBRC NE 0.
      RAISE REQ_ERROR.
   ENDIF.


   SELECT SINGLE * FROM ZTREQST
          WHERE ZFREQNO  EQ  W_ZFREQNO
          AND   ZFAMDNO  EQ  W_ZFAMDNO.
   IF SY-SUBRC NE 0.
      RAISE REQ_ERROR.
   ENDIF.


   REFRESH : BDCDATA.
   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMZIM01'        '3500',
          ' ' 'ZSCIVHD-ZFPOYN'   'Y',
          ' ' 'ZSCIVHD-ZFPRPYN'  'Y',
          ' ' 'BDC_OKCODE'       '=ENTR'.

   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMZIM01'        '3514',
          ' ' 'ZSREQHD-ZFREQNO'  ZTREQST-ZFREQNO,
          ' ' 'ZSREQHD-ZFOPNNO'  SPACE,
          ' ' 'BDC_OKCODE'       '=YES'.


   WRITE : ZTREQHD-KURSF TO  L_KURSF,
           ZTREQHD-FFACT TO  L_FFACT.

   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMZIM01'        '3510',
          ' ' 'ZTCIVHD-ZFCIDT'   ZTREQST-ZFOPNDT,
          ' ' 'ZTCIVHD-ZFEXRT'   L_KURSF,
          ' ' 'ZTCIVHD-FFACT'    L_FFACT,
*          ' ' 'ZTCIVHD-BUPLA'    '1000',
*          ' ' 'ZTCIVHD-ZFPRTE'   ZTREQHD-ZFPREPAY,
          ' ' 'BDC_OKCODE'       'CALC'.

   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMZIM01'        '3510',
          ' ' 'BDC_OKCODE'       '=SAVE'.

   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMZIM01'        '0001',
          ' ' 'BDC_OKCODE'       '=YES'.

*>> BDC CALL.
   REFRESH:MESSTAB, RETURN.
   CALL TRANSACTION 'ZIM35'  USING       BDCDATA
*                             MODE        'N'
                             MODE        MODE
                             UPDATE      'V'
*                             UPDATE      'S'
                             MESSAGES    INTO   MESSTAB.

  W_SUBRC = SY-SUBRC.

  IF W_SUBRC NE 0.      ">> ERROR ¹ß»ý½Ã.
     LOOP AT MESSTAB.
        MOVE : MESSTAB-MSGTYP  TO     RETURN-TYPE,
               MESSTAB-MSGID   TO     RETURN-ID,
               MESSTAB-MSGNR   TO     RETURN-NUMBER,
               MESSTAB-MSGV1   TO     RETURN-MESSAGE_V1,
               MESSTAB-MSGV2   TO     RETURN-MESSAGE_V2,
               MESSTAB-MSGV3   TO     RETURN-MESSAGE_V3,
               MESSTAB-MSGV4   TO     RETURN-MESSAGE_V4.

        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                  EXPORTING
                         MSGID     = RETURN-ID
                         MSGNR     = RETURN-NUMBER
                         MSGV1     = RETURN-MESSAGE_V1
                         MSGV2     = RETURN-MESSAGE_V2
                         MSGV3     = RETURN-MESSAGE_V3
                         MSGV4     = RETURN-MESSAGE_V4
                 IMPORTING
                         MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
         APPEND  RETURN.
      ENDLOOP.
      RAISE   REQ_ERROR.
   ENDIF.

ENDFUNCTION.
