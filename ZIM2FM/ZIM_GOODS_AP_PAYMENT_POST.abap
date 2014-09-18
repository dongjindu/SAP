FUNCTION ZIM_GOODS_AP_PAYMENT_POST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFPNNO) LIKE  ZTPMTHD-ZFPNNO
*"     VALUE(ZFBLDT) LIKE  ZTPMTHD-ZFBLDT DEFAULT SY-DATUM
*"     VALUE(ZFBUDT) LIKE  ZTPMTHD-ZFBUDT DEFAULT SY-DATUM
*"     VALUE(BLART) LIKE  ZTPMTHD-BLART DEFAULT 'KZ'
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
DATA : L_SGTXT   LIKE   BSEG-SGTXT.
DATA : L_SUBRC   LIKE   SY-SUBRC.
DATA : L_NEWKO   LIKE   RF05A-NEWKO.
DATA : L_KURSF(09).
DATA : TEMP_WRBTR(16).

*> PAYMENT NOTICE HEADER SELECT.
   SELECT SINGLE * FROM ZTPMTHD
          WHERE    ZFPNNO  EQ   ZFPNNO.

   IF SY-SUBRC NE 0.
      RAISE POST_ERROR.
   ENDIF.
   *ZTPMTHD = ZTPMTHD.

   IF ZFBUDT  IS INITIAL.
      MOVE  SY-DATUM  TO  ZFBUDT.
   ENDIF.
   IF ZFBLDT  IS INITIAL.
      MOVE  SY-DATUM  TO  ZFBLDT.
   ENDIF.

   MOVE : ZFBUDT   TO    ZTPMTHD-ZFBUDT,
          ZFBLDT   TO    ZTPMTHD-ZFBLDT,
          BLART    TO    ZTPMTHD-BLART,
          'Y'      TO    ZTPMTHD-ZFPMYN.

*> PAYMENT NOTICE ITEM SELECT.
   SELECT SINGLE * FROM ZTPMTHST
   WHERE  ZFPNNO   EQ   ZTPMTHD-ZFPNNO
   AND    CNCLYN   EQ   SPACE.

   IF SY-SUBRC NE 0.
      RAISE POST_ERROR.
   ENDIF.

   L_NEWBS  =  '38'.

   CASE ZTPMTHD-ZFLCKN.
      WHEN '1'.    "> At Sight
         CLEAR : L_NEWUM .
         L_NEWKO = ZTPMTHD-ZFPNBN.
         IF SY-LANGU EQ '3'.
           L_SGTXT = 'AP외자 Bank로 지급처리'.
         ELSE.
           L_SGTXT = 'Payment A/P(Foreign Trade) to the bank'.
         ENDIF.

      WHEN '8'.    "> LOCAL L/C
            CLEAR : L_NEWUM.
            L_NEWKO = ZTPMTHD-ZFPNBN.
            L_SGTXT = 'Payment A/P(domestic) to the bank'.

      WHEN OTHERS.
   ENDCASE.

   WRITE : ZTPMTHD-ZFEXRT   TO  L_KURSF.

   REFRESH : BDCDATA.

*> 초기화면.
   IF SY-LANGU EQ '3'.
     CONCATENATE 'Paym' '''' 't Notice에 의한 지급 반제' INTO L_BKTXT.
   ELSE.
     CONCATENATE 'Repayment due to Paym' '''' 't Notice' INTO L_BKTXT.
   ENDIF.
   PERFORM P2000_DYNPRO USING :
           'X' 'SAPMF05A'    '0122',
           ' ' 'BKPF-BLDAT'  ZTPMTHD-ZFBLDT,
           ' ' 'BKPF-BUDAT'  ZTPMTHD-ZFBUDT,
           ' ' 'BKPF-BLART'  ZTPMTHD-BLART,
           ' ' 'BKPF-WAERS'  ZTPMTHD-ZFPNAMC,
           ' ' 'BKPF-KURSF'  L_KURSF,
           ' ' 'BKPF-BUKRS'  ZTPMTHD-BUKRS,
           ' ' 'BKPF-BKTXT'  L_BKTXT,
           ' ' 'RF05A-AUGTX' L_BKTXT,
           ' ' 'RF05A-XPOS1(1)' 'X',
           ' ' 'RF05A-NEWBS' L_NEWBS,
           ' ' 'RF05A-NEWKO' L_NEWKO,
           ' ' 'BDC_OKCODE'  '/00'.

   L_WRBTR = ZTPMTHD-ZFPNAM.
   WRITE : L_WRBTR TO TEMP_WRBTR CURRENCY ZTPMTHD-ZFPNAMC.

   PERFORM P2000_DYNPRO USING :
           'X' 'SAPMF05A'    '0302',
           ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
           ' ' 'BSEG-SGTXT'  L_SGTXT,
           ' ' 'BDC_OKCODE'  '=PA'.

   PERFORM P2000_DYNPRO USING :
              'X' 'SAPMF05A'    '0710',
              ' ' 'RF05A-AGBUK' ZTPMTHD-BUKRS,
              ' ' 'RF05A-AGKON' ZTPMTHD-ZFPNBN,
              ' ' 'RF05A-AGKOA' 'K',
              ' ' 'RF05A-XNOPS' 'X',
              ' ' 'RF05A-XPOS1(03)' 'X',
              ' '  TEMP_FNAM     'X',
              ' ' 'BDC_OKCODE'  '=PA'.

   PERFORM P2000_DYNPRO USING :
              'X' 'SAPMF05A'          '0731',
              ' ' 'RF05A-SEL01(01)'   ZTPMTHST-BELNR,
              ' ' 'BDC_OKCODE'        '=BU'.

    SET PARAMETER ID 'BLN' FIELD ''.        " 전표번호.
    SET PARAMETER ID 'GJR' FIELD ''.        " 회계년도.

*>> BDC CALL.
    REFRESH:MESSTAB.
    CALL TRANSACTION 'FB05'  USING       BDCDATA
*                             MODE        'A'
                             MODE        MODE
                             UPDATE      'V'
                             MESSAGES    INTO   MESSTAB.

    L_SUBRC = SY-SUBRC.

   IF L_SUBRC NE 0.      ">> ERROR 발생시.
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
      L_SUBRC = 4.
      ROLLBACK WORK.
      RAISE   POST_ERROR.
   ELSE.                 ">> SUCCESS 시.
      GET PARAMETER ID 'BLN' FIELD INVOICEDOCNUMBER.
      GET PARAMETER ID 'GJR' FIELD FISCALYEAR.   " 회계년도.
*>> 전표번호가 전달되지 않을 경우.
      IF INVOICEDOCNUMBER   IS INITIAL OR
         FISCALYEAR         IS INITIAL.

*>>> 오류..(사용자 종결 등....)
         L_SUBRC = 4.
         MESSAGE S494.
         MOVE : 'E'             TO     RETURN-TYPE,
                'ZIM'           TO     RETURN-ID,
                '494'           TO     RETURN-NUMBER,
                SPACE           TO     RETURN-MESSAGE_V1,
                SPACE           TO     RETURN-MESSAGE_V2,
                SPACE           TO     RETURN-MESSAGE_V3,
                SPACE           TO     RETURN-MESSAGE_V4.

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
         ROLLBACK WORK.
         RAISE   POST_ERROR.
      ELSE.
         MESSAGE S260(M8) WITH INVOICEDOCNUMBER.
         MOVE : SY-MSGTY   TO     RETURN-TYPE,
                SY-MSGID   TO     RETURN-ID,
                SY-MSGNO   TO     RETURN-NUMBER,
                SY-MSGV1   TO     RETURN-MESSAGE_V1,
                SY-MSGV2   TO     RETURN-MESSAGE_V2,
                SY-MSGV3   TO     RETURN-MESSAGE_V3,
                SY-MSGV4   TO     RETURN-MESSAGE_V4.

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


         MOVE : INVOICEDOCNUMBER TO ZTPMTHD-ZFPMNO,
                FISCALYEAR       TO ZTPMTHD-ZFPMYR.


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
            MOVE :  ZTPMTHD-ZFPMYR             TO  ZTPMTHST-ZFPMYR,
                    ZTPMTHD-ZFPMNO             TO  ZTPMTHST-ZFPMNO,
                    SY-UNAME                   TO  ZTPMTHST-UNAM,
                    SY-DATUM                   TO  ZTPMTHST-UDAT,
                    SY-UZEIT                   TO  ZTPMTHST-UTME.

            UPDATE  ZTPMTHST.

            IF SY-SUBRC NE 0.
               ROLLBACK WORK.
               RAISE POST_ERROR.
            ENDIF.

            COMMIT WORK.
         ELSE.
            L_SUBRC = 4.
            ROLLBACK WORK.
            RAISE POST_ERROR.
         ENDIF.
      ENDIF.
   ENDIF.

ENDFUNCTION.
