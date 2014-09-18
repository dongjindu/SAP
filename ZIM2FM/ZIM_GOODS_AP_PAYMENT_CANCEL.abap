FUNCTION ZIM_GOODS_AP_PAYMENT_CANCEL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFPNNO) LIKE  ZTPMTHD-ZFPNNO
*"     VALUE(MODE) TYPE  C DEFAULT 'N'
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      POST_ERROR
*"----------------------------------------------------------------------
DATA : L_AWKEY   LIKE   BKPF-AWKEY.
DATA : L_WRBTR   LIKE   ZTBKPF-WRBTR.
DATA : L_XBLNR   LIKE   BKPF-XBLNR.
DATA : L_BKTXT   LIKE   BKPF-BKTXT.
DATA : L_NEWUM   LIKE   RF05A-NEWUM.
DATA : L_NEWBS   LIKE   RF05A-NEWBS.
DATA : L_BELNR   LIKE   BKPF-BELNR.
DATA : L_GJAHR   LIKE   BKPF-GJAHR.
DATA : L_SUBRC   LIKE   SY-SUBRC.
DATA : L_KURSF(09).
DATA : TEMP_WRBTR(16).

DATA : L_GUBUN.

DATA: CTU_PARAMS LIKE CTU_PARAMS.

    CTU_PARAMS-DISMODE  = MODE.
    CTU_PARAMS-UPDMODE  = 'V'.
    CTU_PARAMS-CATTMODE = ' '.
    CTU_PARAMS-DEFSIZE  = ' '.
    CTU_PARAMS-RACOMMIT = 'X'.
    CTU_PARAMS-NOBINPT  = 'X'.
    CTU_PARAMS-NOBIEND  = 'X'.

*> PAYMENT NOTICE HEADER SELECT.
   SELECT SINGLE * FROM ZTPMTHD
          WHERE    ZFPNNO  EQ   ZFPNNO.

   IF SY-SUBRC NE 0.
      RAISE POST_ERROR.
   ENDIF.
   *ZTPMTHD = ZTPMTHD.

*> PAYMENT NOTICE HISTORY SELECT.
   SELECT SINGLE * FROM ZTPMTHST
   WHERE  ZFPNNO   EQ   ZTPMTHD-ZFPNNO
   AND    PMCNYN   EQ   SPACE
   AND    ZFPMNO   NE   SPACE.

   IF SY-SUBRC NE 0.
      RAISE POST_ERROR.
   ENDIF.

*>>> BDC....
   REFRESH : BDCDATA.
* 초기화면 FIELD
   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMF05R'    '0100',
          ' ' 'RF05R-AUGBL' ZTPMTHD-ZFPMNO,       " 역분개할 전표.
          ' ' 'RF05R-BUKRS' ZTPMTHD-BUKRS,        " Company Code
          ' ' 'RF05R-GJAHR' ZTPMTHD-ZFPMYR,       " 회계연도.
          ' ' 'BDC_OKCODE'  '=RAGL'.              " 전기.

* NEXT FIELD
   PERFORM P2000_DYNPRO USING :
          'X' 'SAPLSPO2'    '0100',
          ' ' 'BDC_OKCODE'  '=OPT2'.               " 전기.

   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMF05R'    '0300',
          ' ' 'RF05R-STGRD' '02',               " 역분개할 전표.
          ' ' 'RF05R-BUDAT' SY-DATUM,           " Company Code
          ' ' 'BDC_OKCODE'  '=ENTR'.               " 전기.

   SET PARAMETER ID 'BLN' FIELD ''.        " 전표번호.
   SET PARAMETER ID 'GJR' FIELD ''.        " 회계년도.

*>> BDC CALL.
   CALL TRANSACTION 'FBRA'  USING          BDCDATA
                            OPTIONS FROM CTU_PARAMS
*                            UPDATE      'V'
                            MESSAGES    INTO   MESSTAB.

   L_SUBRC = SY-SUBRC.

   GET PARAMETER ID 'BLN' FIELD L_BELNR.
   GET PARAMETER ID 'GJR' FIELD L_GJAHR.   " 회계년도.

*   IF L_GUBUN EQ 'B'.
      IF L_BELNR NE ZTPMTHD-ZFPMNO AND
         NOT L_BELNR IS INITIAL.
         DESCRIBE TABLE MESSTAB LINES W_LINE.
         DELETE MESSTAB INDEX W_LINE.
         L_SUBRC = 0.
      ENDIF.
*   ENDIF.


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

   IF L_SUBRC NE 0.      ">> ERROR 발생시.
      L_SUBRC = 4.
      ROLLBACK WORK.
      RAISE   POST_ERROR.
   ENDIF.
*   ELSE.                 ">> SUCCESS 시.
*      MESSAGE S360(M8) WITH ZTPMTHD-BUKRS ZTPMTHD-GJAHR
*                                          ZTPMTHD-BELNR.
*
*      MOVE : SY-MSGTY   TO     RETURN-TYPE,
*             SY-MSGID   TO     RETURN-ID,
*             SY-MSGNO   TO     RETURN-NUMBER,
*             SY-MSGV1   TO     RETURN-MESSAGE_V1,
*             SY-MSGV2   TO     RETURN-MESSAGE_V2,
*             SY-MSGV3   TO     RETURN-MESSAGE_V3,
*             SY-MSGV4   TO     RETURN-MESSAGE_V4.
*
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*                   EXPORTING
*                         MSGID     = RETURN-ID
*                         MSGNR     = RETURN-NUMBER
*                         MSGV1     = RETURN-MESSAGE_V1
*                         MSGV2     = RETURN-MESSAGE_V2
*                         MSGV3     = RETURN-MESSAGE_V3
*                         MSGV4     = RETURN-MESSAGE_V4
*                  IMPORTING
*                         MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
*      APPEND  RETURN.
*   ENDIF.

**>>> BDC....
*   REFRESH : BDCDATA, MESSTAB.
*
*   PERFORM P2000_DYNPRO USING :
*             'X' 'SAPMF05A'    '0105',
*             ' ' 'RF05A-BELNS' ZTPMTHD-BELNR,       " 역분개할 전표.
*             ' ' 'BKPF-BUKRS'  ZTPMTHD-BUKRS,       " Company Code
*             ' ' 'RF05A-GJAHS' ZTPMTHD-GJAHR,       " 회계연도.
*             ' ' 'UF05A-STGRD' '02',                " 역분개사유.
**             ' ' 'BSIS-BUDAT'  SY-DATUM,            " 전표전기일.
*             ' ' 'BSIS-MONAT'  SPACE,               " 회계기간.
*             ' ' 'RF05A-VOIDR' SPACE,               " 수표폐기사유코드.
*             ' ' 'BDC_OKCODE'  '=BU'.               " 전기.
*
*   SET PARAMETER ID 'BLN' FIELD ''.        " 전표번호.
*   SET PARAMETER ID 'GJR' FIELD ''.        " 회계년도.
*
**>> BDC CALL.
*   CALL TRANSACTION 'FB08'  USING       BDCDATA
*                            MODE        MODE
*                            UPDATE      'V'
*                            MESSAGES    INTO   MESSTAB.
*   L_SUBRC = SY-SUBRC.
*
*   GET PARAMETER ID 'BLN' FIELD L_BELNR.
*   GET PARAMETER ID 'GJR' FIELD L_GJAHR.   " 회계년도.
*
*   IF L_SUBRC NE 0.      ">> ERROR 발생시.
*      LOOP AT MESSTAB.
*         MOVE : MESSTAB-MSGTYP  TO     RETURN-TYPE,
*                MESSTAB-MSGID   TO     RETURN-ID,
*                MESSTAB-MSGNR   TO     RETURN-NUMBER,
*                MESSTAB-MSGV1   TO     RETURN-MESSAGE_V1,
*                MESSTAB-MSGV2   TO     RETURN-MESSAGE_V2,
*                MESSTAB-MSGV3   TO     RETURN-MESSAGE_V3,
*                MESSTAB-MSGV4   TO     RETURN-MESSAGE_V4.
*
*         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*                   EXPORTING
*                         MSGID     = RETURN-ID
*                         MSGNR     = RETURN-NUMBER
*                         MSGV1     = RETURN-MESSAGE_V1
*                         MSGV2     = RETURN-MESSAGE_V2
*                         MSGV3     = RETURN-MESSAGE_V3
*                         MSGV4     = RETURN-MESSAGE_V4
*                  IMPORTING
*                         MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
*         APPEND  RETURN.
*      ENDLOOP.
*      L_SUBRC = 4.
*      ROLLBACK WORK.
*      RAISE   POST_ERROR.
*   ELSE.
*      MESSAGE S282(M8) WITH L_BELNR.
*      MOVE : SY-MSGTY   TO     RETURN-TYPE,
*             SY-MSGID   TO     RETURN-ID,
*             SY-MSGNO   TO     RETURN-NUMBER,
*             SY-MSGV1   TO     RETURN-MESSAGE_V1,
*             SY-MSGV2   TO     RETURN-MESSAGE_V2,
*             SY-MSGV3   TO     RETURN-MESSAGE_V3,
*             SY-MSGV4   TO     RETURN-MESSAGE_V4.
*
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*                   EXPORTING
*                         MSGID     = RETURN-ID
*                         MSGNR     = RETURN-NUMBER
*                         MSGV1     = RETURN-MESSAGE_V1
*                         MSGV2     = RETURN-MESSAGE_V2
*                         MSGV3     = RETURN-MESSAGE_V3
*                         MSGV4     = RETURN-MESSAGE_V4
*                  IMPORTING
*                         MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
*      APPEND  RETURN.

      MOVE : 'N'     TO      ZTPMTHD-ZFPMYN.
      CLEAR : ZTPMTHD-ZFPMNO, ZTPMTHD-ZFPMYR.

      CALL FUNCTION 'ZIM_PM_DOC_MODIFY'
              EXPORTING
                   ZFPNNO              =   ZTPMTHD-ZFPNNO
                   ZFSTATUS            =   'U'
                   W_ZTPMTHD_OLD       =   *ZTPMTHD
                   W_ZTPMTHD           =   ZTPMTHD
                   W_OK_CODE           =   'SAVE'
              TABLES
                   IT_ZSPMTIV_OLD      =   IT_ZSPMTIV
                   IT_ZSPMTIV          =   IT_ZSPMTIV
              EXCEPTIONS
                   OTHERS              =   4.

      IF SY-SUBRC EQ 0.
         SELECT SINGLE * FROM ZTPMTHST
                WHERE    BUKRS  EQ ZTPMTHD-BUKRS
                AND      ZFPMNO EQ ZTPMTHD-ZFPMNO
                AND      ZFPMYR EQ ZTPMTHD-ZFPMYR.
         IF SY-SUBRC EQ 0.
            MOVE : 'X'            TO  ZTPMTHST-PMCNYN,
                   SY-UNAME       TO  ZTPMTHST-UNAM,
                   SY-DATUM       TO  ZTPMTHST-UDAT,
                   SY-UZEIT       TO  ZTPMTHST-UTME.
            UPDATE ZTPMTHST.
            IF SY-SUBRC NE 0.
               ROLLBACK WORK.
               RAISE POST_ERROR.
            ENDIF.
         ENDIF.

         COMMIT WORK.
      ELSE.
         L_SUBRC = 4.
         ROLLBACK WORK.
         RAISE POST_ERROR.
      ENDIF.
*   ENDIF.

ENDFUNCTION.
