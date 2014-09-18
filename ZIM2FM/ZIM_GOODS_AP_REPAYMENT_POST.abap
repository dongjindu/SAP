FUNCTION ZIM_GOODS_AP_REPAYMENT_POST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFPNNO) LIKE  ZTPMTHD-ZFPNNO
*"     VALUE(BLDAT) LIKE  ZTPMTHD-BLDAT DEFAULT SY-DATUM
*"     VALUE(BUDAT) LIKE  ZTPMTHD-BUDAT DEFAULT SY-DATUM
*"     VALUE(BLART) LIKE  ZTPMTHD-BLART DEFAULT 'RE'
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

  MOVE : BUDAT   TO    ZTPMTHD-BUDAT,
         BLDAT   TO    ZTPMTHD-BLDAT,
         BLART   TO    ZTPMTHD-BLART,
         'Y'     TO    ZTPMTHD-ZFPYA.

*> PAYMENT NOTICE ITEM SELECT.
  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_ZSPMTIV
        FROM   ZTPMTIV
        WHERE  ZFPNNO  EQ  ZTPMTHD-ZFPNNO.

  IF SY-SUBRC NE 0.
    RAISE POST_ERROR.
  ENDIF.

  CASE ZTPMTHD-ZFLCKN.
    WHEN '1'.    "> At Sight
      CLEAR : L_NEWUM .
      L_NEWBS = '31'.
      L_NEWKO = ZTPMTHD-ZFPNBN.
      IF SY-LANGU EQ '3'.
        L_SGTXT = 'AP외자 Bank로 반제처리'.
      ELSE.
        L_SGTXT = 'Repayment A/P(Foreign Trade) to the bank'.
      ENDIF.

*>    USANCE 경우 USANCE차입임시계정(9113140) 처?
    WHEN '2' OR '3'.  "> USANCE
      CLEAR : L_NEWUM .
      L_NEWBS = '50'.
* Usance일 경우의 차입금 계정을 어떻게 가져가야 할지 결정해야 할 것.
      L_NEWKO = '225020'. " 임시계정..
      IF SY-LANGU EQ '3'.
        L_SGTXT = 'USANCE 차입전환'.
      ELSE.
        L_SGTXT = 'USANCE Loan Conversion'.
      ENDIF.

    WHEN '8'.    "> LOCAL L/C
      CLEAR : L_NEWUM.
      L_NEWBS = '31'.
      L_NEWKO = ZTPMTHD-ZFPNBN.
      IF SY-LANGU EQ '3'.
        L_SGTXT = 'AP내자 Bank로 반제처리'.
      ELSE.
        L_SGTXT = 'Repayment A/P(Domestic) to the bank'.
      ENDIF.

    WHEN OTHERS.
      L_NEWUM = 'K'.
      L_NEWBS = '39'.
      L_NEWKO = ZTPMTHD-ZFPNBN.
      IF SY-LANGU EQ '3'.
        L_SGTXT = 'AP외자 Bank로 반제처리'.
      ELSE.
        L_SGTXT = 'Repayment A/P(Foreign Trade) to the bank'.
      ENDIF.
  ENDCASE.

*> 전표번호 순서 SELECT.
  SELECT * FROM T021R
           WHERE EVENT EQ 'SL-AG'
           AND   FELDN EQ 'BELNR'.
  ENDSELECT.
  IF SY-SUBRC EQ 0.
    T021R-SELPS = T021R-SELPS + 1.
    MOVE : 'RF05A-XPOS1('  TO TEMP_FNAM,
           T021R-SELPS     TO TEMP_FNAM+12(2),
           ')'             TO TEMP_FNAM+14(1).
  ENDIF.

  REFRESH : BDCDATA.

  WRITE : ZTPMTHD-ZFEXRT   TO  L_KURSF.

  IF NOT ZTPMTHD-ZFHBLNO IS INITIAL.
    L_XBLNR =   ZTPMTHD-ZFHBLNO.
  ELSE.
    L_XBLNR =   ZTPMTHD-EBELN.
  ENDIF.

*> 초기화면.
  IF SY-LANGU EQ '3'.
    CONCATENATE 'Paym' '''' 't Notice에 의한 반제' INTO L_BKTXT.
  ELSE.
    CONCATENATE 'Repayment due to Paym' '''' 't Notice' INTO L_BKTXT.
  ENDIF.

  PERFORM  P2000_DATE_USER_CONVERT      USING ZTPMTHD-BLDAT
                                     CHANGING W_BLDAT.

  PERFORM  P2000_DATE_USER_CONVERT      USING ZTPMTHD-BUDAT
                                     CHANGING W_BUDAT.

  PERFORM P2000_DYNPRO USING :
          'X' 'SAPMF05A'    '0122',
          ' ' 'BKPF-BLDAT'  W_BLDAT,
          ' ' 'BKPF-BUDAT'  W_BUDAT,
          ' ' 'BKPF-BLART'  ZTPMTHD-BLART,
          ' ' 'BKPF-BUKRS'  ZTPMTHD-BUKRS,
          ' ' 'BKPF-WAERS'  ZTPMTHD-ZFPNAMC,
          ' ' 'BKPF-KURSF'  L_KURSF,
          ' ' 'BKPF-XBLNR'  L_XBLNR,
          ' ' 'BKPF-BKTXT'  L_BKTXT,
          ' ' 'RF05A-AUGTX' L_BKTXT,
          ' ' 'RF05A-XPOS1(4)' 'X',
          ' ' 'RF05A-NEWBS' L_NEWBS,
          ' ' 'RF05A-NEWKO' L_NEWKO,
          ' ' 'RF05A-NEWUM' L_NEWUM,
          ' ' 'BDC_OKCODE'  '/00'.

  L_WRBTR = ZTPMTHD-ZFPNAM.
  WRITE : L_WRBTR TO TEMP_WRBTR CURRENCY ZTPMTHD-ZFPNAMC.

  READ TABLE IT_ZSPMTIV INDEX 1.
  IF SY-SUBRC EQ 0.
    L_AWKEY    = IT_ZSPMTIV-ZFGFDNO.
    L_AWKEY+10 = IT_ZSPMTIV-ZFGFDYR.

    IF ZTPMTHD-ZFLCKN EQ '8'.
      SELECT SINGLE * FROM  BKPF
              WHERE BUKRS EQ ZTPMTHD-BUKRS
              AND   BELNR EQ IT_ZSPMTIV-ZFGFDNO
              AND   GJAHR EQ IT_ZSPMTIV-ZFGFDYR.

    ELSE.
      SELECT * FROM  BKPF UP TO 1 ROWS
           WHERE AWKEY EQ L_AWKEY.

      ENDSELECT.
    ENDIF.

    SELECT SINGLE * FROM BSEG
           WHERE BELNR   EQ  BKPF-BELNR
           AND   GJAHR   EQ  BKPF-GJAHR
           AND   BUKRS   EQ  BKPF-BUKRS
           AND   BUZEI   EQ  '001'.
  ENDIF.

*---------------------------------------------
*> USANCE일 경우...
*>--------------------------------------------
  IF L_NEWBS EQ '39'.

    PERFORM P2000_DYNPRO USING :
            'X' 'SAPMF05A'    '0304',
            ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
*>> 2003.10.28: NSH Inserted..
*            ' ' 'BSEG-BUPLA'  BSEG-BUPLA,
*            ' ' 'BSEG-GSBER'  BSEG-GSBER,
            ' ' 'BSEG-ZFBDT'  W_BLDAT,
            ' ' 'BSEG-ZUONR'  '',
            ' ' 'BSEG-SGTXT'  L_SGTXT,
            ' ' 'BDC_OKCODE'  '=PA'.
  ELSE.
    IF L_NEWUM EQ 'I' OR L_NEWUM EQ '2'.
      PERFORM P2000_DYNPRO USING :
           'X' 'SAPMF05A'    '0303',
           ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
           ' ' 'BSEG-BUPLA'  BSEG-BUPLA,
           ' ' 'BSEG-GSBER'  BSEG-GSBER,
           ' ' 'BSEG-ZFBDT'  W_BLDAT,
           ' ' 'BSEG-ZUONR'  '',
           ' ' 'BSEG-SGTXT'  L_SGTXT,
           ' ' 'BDC_OKCODE'  '=PA'.
    ELSE.
*>> 현대자동차 주석처리 2003.10.29 NSH..
*      PERFORM P2000_DYNPRO USING :
*           'X' 'SAPMF05A'    '0302',
*           ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
*           ' ' 'BSEG-SKFBT'  TEMP_WRBTR,
*           ' ' 'BSEG-BUPLA'  BSEG-BUPLA,
*           ' ' 'BSEG-GSBER'  BSEG-GSBER,
*           ' ' 'BSEG-ZFBDT'  W_BLDAT,
*           ' ' 'BSEG-ZUONR'  '',
*           ' ' 'BSEG-SGTXT'  L_SGTXT,
*           ' ' 'BDC_OKCODE'  '=PA'.
      PERFORM P2000_DYNPRO USING :
           'X' 'SAPMF05A'    '0300',
           ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
           ' ' 'BSEG-SKFBT'  TEMP_WRBTR,
           ' ' 'BSEG-BUPLA'  BSEG-BUPLA,
           ' ' 'BSEG-GSBER'  BSEG-GSBER,
           ' ' 'BSEG-ZFBDT'  W_BLDAT,
           ' ' 'BSEG-ZUONR'  '',
           ' ' 'BSEG-SGTXT'  L_SGTXT,
           ' ' 'BDC_OKCODE'  '=PA'.
*>> 2003.10.29 현대자동차 Coding Block 처리기능 추가.
      PERFORM P2000_DYNPRO USING :
           'X' 'SAPLKACB'    '0002',
           ' ' 'BDC_OKCODE'  '=ENTE'.
    ENDIF.
  ENDIF.

  PERFORM P2000_DYNPRO USING :
             'X' 'SAPMF05A'    '0710',
             ' ' 'RF05A-AGBUK' ZTPMTHD-BUKRS,
             ' ' 'RF05A-AGKON' ZTPMTHD-ZFBENI,
             ' ' 'RF05A-AGKOA' 'K',
             ' ' 'RF05A-XNOPS' 'X',
             ' ' 'RF05A-XPOS1(03)' 'X',
             ' '  TEMP_FNAM     'X',
             ' ' 'BDC_OKCODE'  '=PA'.

  DESCRIBE TABLE IT_ZSPMTIV LINES W_LINE.

  LOOP AT IT_ZSPMTIV.
    W_TABIX = SY-TABIX.

    L_AWKEY    = IT_ZSPMTIV-ZFGFDNO.
    L_AWKEY+10 = IT_ZSPMTIV-ZFGFDYR.

    SELECT * FROM  BKPF UP TO 1 ROWS
             WHERE AWKEY EQ L_AWKEY.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      SELECT SINGLE * FROM  BKPF
               WHERE BUKRS EQ ZTPMTHD-BUKRS
               AND   BELNR EQ IT_ZSPMTIV-ZFGFDNO
               AND   GJAHR EQ IT_ZSPMTIV-ZFGFDYR.
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    PERFORM P2000_DYNPRO USING :
               'X' 'SAPMF05A'          '0731',
               ' ' 'RF05A-SEL01(01)'   BKPF-BELNR.

    IF W_TABIX EQ  W_LINE.
      PERFORM P2000_DYNPRO USING :
           ' ' 'BDC_OKCODE'  '=PA'.
      PERFORM P2000_DYNPRO USING :
           'X' 'SAPDF05X'          '3100',
           ' ' 'BDC_OKCODE'  '=PART'.

    ELSE.
      PERFORM P2000_DYNPRO USING :
           ' ' 'BDC_OKCODE'  '=SL2'.

      PERFORM P2000_DYNPRO USING :
                 'X' 'SAPMF05A'          '0608',
                 ' ' 'RF05A-XPOS1(02)'   'X',
                 ' ' 'BDC_OKCODE'        '=ENTR'.
    ENDIF.
  ENDLOOP.

  PERFORM P2000_DYNPRO USING :
             'X' 'SAPDF05X'          '3100',
             ' ' 'BDC_OKCODE'        '=BU'.

  SET PARAMETER ID 'BLN' FIELD ''.        " 전표번호.
  SET PARAMETER ID 'GJR' FIELD ''.        " 회계년도.

*>> BDC CALL.
  REFRESH:MESSTAB.
  CALL TRANSACTION 'FB05'  USING       BDCDATA
                           MODE        'A' "MODE
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
                MSGID               = RETURN-ID
                MSGNR               = RETURN-NUMBER
                MSGV1               = RETURN-MESSAGE_V1
                MSGV2               = RETURN-MESSAGE_V2
                MSGV3               = RETURN-MESSAGE_V3
                MSGV4               = RETURN-MESSAGE_V4
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
                MSGID               = RETURN-ID
                MSGNR               = RETURN-NUMBER
                MSGV1               = RETURN-MESSAGE_V1
                MSGV2               = RETURN-MESSAGE_V2
                MSGV3               = RETURN-MESSAGE_V3
                MSGV4               = RETURN-MESSAGE_V4
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
                MSGID               = RETURN-ID
                MSGNR               = RETURN-NUMBER
                MSGV1               = RETURN-MESSAGE_V1
                MSGV2               = RETURN-MESSAGE_V2
                MSGV3               = RETURN-MESSAGE_V3
                MSGV4               = RETURN-MESSAGE_V4
           IMPORTING
                MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
      APPEND  RETURN.

      MOVE : INVOICEDOCNUMBER TO ZTPMTHD-BELNR,
             FISCALYEAR       TO ZTPMTHD-GJAHR.

      IF ZTPMTHD-ZFLCKN EQ '2'.
        SELECT SINGLE *
                 FROM ZTIMIMG11
                WHERE BUKRS = ZTPMTHD-BUKRS.
        IF NOT ZTIMIMG11-ZFIOCAC31 IS INITIAL.
*>> 2003.10.29: NSH Inserted..
*>> 이자계정에 대한 회계전표 발생을 위한 로직 추가..
          PERFORM P3000_INTEREST_BDC_POST.

          IF W_ERR_CHK EQ SPACE.
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
                      MSGID               = RETURN-ID
                      MSGNR               = RETURN-NUMBER
                      MSGV1               = RETURN-MESSAGE_V1
                      MSGV2               = RETURN-MESSAGE_V2
                      MSGV3               = RETURN-MESSAGE_V3
                      MSGV4               = RETURN-MESSAGE_V4
                 IMPORTING
                      MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
            APPEND  RETURN.

            MOVE  :  INVOICEDOCNUMBER   TO  ZTPMTHD-BELNR_TR,
                     FISCALYEAR         TO  ZTPMTHD-GJAHR_TR.
          ENDIF.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'ZIM_PM_DOC_MODIFY'
           EXPORTING
                ZFPNNO         = ZTPMTHD-ZFPNNO
                ZFSTATUS       = 'U'
                W_ZTPMTHD_OLD  = *ZTPMTHD
                W_ZTPMTHD      = ZTPMTHD
                W_OK_CODE      = 'SAVE'
           TABLES
                IT_ZSPMTIV_OLD = IT_ZSPMTIV
                IT_ZSPMTIV     = IT_ZSPMTIV
           EXCEPTIONS
                OTHERS         = 4.

      IF SY-SUBRC EQ 0.
        CLEAR : ZTPMTHST.
        MOVE-CORRESPONDING ZTPMTHD  TO   ZTPMTHST.
        MOVE :  SY-MANDT                   TO  ZTPMTHST-MANDT,
                SY-UNAME                   TO  ZTPMTHST-ERNAM,
                SY-DATUM                   TO  ZTPMTHST-CDAT,
                SY-UZEIT                   TO  ZTPMTHST-CTME,
                SY-UNAME                   TO  ZTPMTHST-UNAM,
                SY-DATUM                   TO  ZTPMTHST-UDAT,
                SY-UZEIT                   TO  ZTPMTHST-UTME.

        INSERT  ZTPMTHST.

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
