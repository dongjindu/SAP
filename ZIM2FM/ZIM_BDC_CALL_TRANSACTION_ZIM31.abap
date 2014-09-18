FUNCTION ZIM_BDC_CALL_TRANSACTION_ZIM31.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFBLNO) LIKE  ZTBL-ZFBLNO
*"     VALUE(MODE) TYPE  C DEFAULT 'N'
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      REQ_ERROR
*"----------------------------------------------------------------------
DATA : L_ZFCLCD LIKE ZSIV-ZFCLCD.

   SELECT SINGLE * FROM ZTBL
          WHERE ZFBLNO  EQ  W_ZFBLNO.

   CASE ZTBL-ZFRPTTY.
      WHEN 'B' OR 'T' OR 'Q' OR 'W'.
         L_ZFCLCD = 'A'.
      WHEN 'C' OR 'F'.
         L_ZFCLCD = 'C'.
      WHEN OTHERS.
         L_ZFCLCD = 'X'.
   ENDCASE.

   REFRESH : BDCDATA, RETURN.
   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMZIM01'   '3100',
          ' ' 'ZSREQHD-ZFBLNO'   W_ZFBLNO,
          ' ' 'ZSREQHD-ZFHBLNO'  SPACE,
          ' ' 'ZSIV-ZFCLCD'      L_ZFCLCD,
          ' ' 'BDC_OKCODE'       '=ENTR'.

   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMZIM01'        '3110',
          ' ' 'BDC_OKCODE'       '=CALC'.

   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMZIM01'        '3110',
          ' ' 'BDC_OKCODE'       '=SAVE'.

   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMZIM01'        '0001',
          ' ' 'BDC_OKCODE'       '=YES'.

*>> BDC CALL.
   REFRESH:MESSTAB.

   SET PARAMETER ID 'ZPBLNO'   FIELD ''.
   SET PARAMETER ID 'ZPCLSEQ'  FIELD ''.

   CALL TRANSACTION 'ZIM31'  USING       BDCDATA
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
   ELSE.
*      GET PARAMETER ID 'ZPBLNO'    FIELD ZTIDR-ZFBLNO.
*      GET PARAMETER ID 'ZPCLSEQ'   FIELD ZTIDR-ZFCLSEQ.
*      IF ZTIDR-ZFCLSEQ IS INITIAL.
*         RAISE   REQ_ERROR.
*      ENDIF.
   ENDIF.

ENDFUNCTION.
