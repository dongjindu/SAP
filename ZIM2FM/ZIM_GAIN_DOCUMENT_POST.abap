FUNCTION ZIM_GAIN_DOCUMENT_POST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_ZFIVNO) LIKE  ZTIV-ZFIVNO
*"     VALUE(ZTBKPF) TYPE  ZTBKPF
*"     VALUE(MODE) TYPE  C DEFAULT 'N'
*"  EXPORTING
*"     VALUE(BUKRS) TYPE  BKPF-BUKRS
*"     VALUE(BELNR) TYPE  BKPF-BELNR
*"     VALUE(GJAHR) TYPE  BKPF-GJAHR
*"  TABLES
*"      XBSEG STRUCTURE  ZSBSEG OPTIONAL
*"      XZTIVHSTIT STRUCTURE  ZSIVHSTIT OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      POST_ERROR
*"----------------------------------------------------------------------
DATA : W_KURSF(12).
DATA: TEMP_WRBTR(16),
      TEMP_WMWST(16),
      TEMP_FWBAS(16),
      TEMP_DMBTR(16).

DATA : L_DYNNR       LIKE SY-DYNNR.

*>---------------------------------------------------------------
*> NOTE 적용.
DATA: CTU_PARAMS LIKE CTU_PARAMS.
    CTU_PARAMS-DISMODE  = MODE.
    CTU_PARAMS-UPDMODE  = 'V'.
    CTU_PARAMS-CATTMODE = ' '.
    CTU_PARAMS-DEFSIZE  = ' '.
    CTU_PARAMS-RACOMMIT = 'X'.
    CTU_PARAMS-NOBINPT  = 'X'.
    CTU_PARAMS-NOBIEND  = 'X'.
*>---------------------------------------------------------------

  REFRESH : BDCDATA, RETURN, MESSTAB.
  CLEAR : BELNR, GJAHR, BUKRS.

  SELECT SINGLE * FROM T001
         WHERE    BUKRS EQ ZTBKPF-BUKRS.


*> 환율.
  IF ZTBKPF-WAERS NE 'KRW'.
     WRITE :  ZTBKPF-KURSF TO  W_KURSF.
  ELSE.
     CLEAR : W_KURSF.
  ENDIF.

*> 초기화면.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A' '0100',
      ' ' 'BKPF-BLDAT'  ZTBKPF-BLDAT,          " Document Date
      ' ' 'BKPF-BLART'  ZTBKPF-BLART,          " Type
      ' ' 'BKPF-BUKRS'  ZTBKPF-BUKRS,          " Company Code
      ' ' 'BKPF-BUDAT'  ZTBKPF-BUDAT,          " Posting Date
      ' ' 'BKPF-WAERS'  ZTBKPF-WAERS,          " Currency
*      ' ' 'BKPF-KURSF'  W_KURSF,             " Exchange Rate.
      ' ' 'BKPF-XBLNR'  ZTBKPF-XBLNR,          " 참조.
      ' ' 'BKPF-BKTXT'  ZTBKPF-BKTXT.          " 헤더 TEXT.

*> 배부내용.
  LOOP AT XBSEG.
     PERFORM P2000_DYNPRO USING :
            ' ' 'RF05A-NEWBS' XBSEG-NEWBS,          " Posting Key
            ' ' 'RF05A-NEWKO' XBSEG-NEWKO,          " Account
            ' ' 'BDC_OKCODE'  '/00'.                " ENTER

     WRITE XBSEG-WRBTR CURRENCY  ZTBKPF-WAERS       TO TEMP_WRBTR.
     WRITE XBSEG-DMBTR CURRENCY  'KRW'              TO TEMP_DMBTR.
     CLEAR TEMP_WMWST.

     PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_WRBTR.
     PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_DMBTR.

     PERFORM P2000_DYNPRO USING :
          'X' 'SAPMF05A' '0300',
          ' ' 'BSEG-WRBTR' TEMP_WRBTR,            " Amount
*         ' ' 'BSEG-MWSKZ' XBSEG-MWSKZ,           " Tax Code
          ' ' 'BSEG-MWSKZ' SPACE,                 " Tax Code
          ' ' 'BSEG-BUPLA' XBSEG-BUPLA,           " Business Place
          ' ' 'COBL-GSBER' XBSEG-GSBER,           " Business Area
          ' ' 'COBL-KOSTL' XBSEG-KOSTL,           " Cost Center.
          ' ' 'COBL-PS_POSID' XBSEG-PS_POSID,
          ' ' 'BSEG-ZUONR' XBSEG-ZUONR,           " 지정.
          ' ' 'BSEG-SGTXT' XBSEG-SGTXT.           "TEXT.

      IF NOT XBSEG-MWSKZ IS INITIAL.
         PERFORM P2000_DYNPRO USING :
                 ' ' 'BSEG-MWSKZ' XBSEG-MWSKZ.           " Tax Code
      ENDIF.
   ENDLOOP.
* 저장.
   PERFORM P2000_DYNPRO USING :
      ' ' 'BDC_OKCODE' '=BU'.

   SET PARAMETER ID 'BLN' FIELD ''.        " 전표번호.
   SET PARAMETER ID 'GJR' FIELD ''.        " 회계년도.

*>> BDC CALL.
   REFRESH : MESSTAB.
   CALL TRANSACTION 'FB01'  USING       BDCDATA
                            OPTIONS FROM CTU_PARAMS
*                            MODE        'N'
*                             MODE        MODE
*                             UPDATE      'S'
                             MESSAGES    INTO   MESSTAB.
   W_SUBRC = SY-SUBRC.

   IF W_SUBRC NE 0.      ">> ERROR 발생시.
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
      W_SUBRC = 4.
      RAISE   POST_ERROR.
   ELSE.                 ">> SUCCESS 시.
      GET PARAMETER ID 'BLN' FIELD BELNR.
      GET PARAMETER ID 'GJR' FIELD GJAHR.   " 회계년도.
      MOVE BKPF-BUKRS   TO  BUKRS.
*>> 전표번호가 전달되지 않을 경우.
      IF BELNR  IS INITIAL OR
         GJAHR  IS INITIAL.

*>>> 오류..(사용자 종결 등....)
         W_SUBRC = 4.
         MESSAGE S494(ZIM).
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
         RAISE   POST_ERROR.
      ELSE.

         MESSAGE S260(M8) WITH    BELNR.
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
         W_SUBRC = 0.
      ENDIF.
  ENDIF.

  IF W_SUBRC = 0.
     MOVE : SY-MANDT           TO     ZTIVHST-MANDT,
            P_ZFIVNO           TO     ZTIVHST-ZFIVNO,
            'Y'                TO     ZTIVHST-ZFGRST,
            ZTBKPF-BLDAT       TO     ZTIVHST-BLDAT,
            ZTBKPF-BUDAT       TO     ZTIVHST-BUDAT,
            SPACE              TO     ZTIVHST-BWART,
            SY-UNAME           TO     ZTIVHST-ERNAM,
            'X'                TO     ZTIVHST-ZFGAIN,
            SY-DATUM           TO     ZTIVHST-CDAT,
            SY-UZEIT           TO     ZTIVHST-CTME,
            BELNR              TO     ZTIVHST-MBLNR,
            GJAHR              TO     ZTIVHST-MJAHR,
            'S'                TO     ZTIVHST-SHKZG.

         SELECT MAX( ZFIVHST ) INTO ZTIVHST-ZFIVHST
                FROM   ZTIVHST
                WHERE  ZFIVNO    EQ    P_ZFIVNO.

         ADD    1                 TO    ZTIVHST-ZFIVHST.
         INSERT   ZTIVHST.
         IF SY-SUBRC NE 0.
            MESSAGE E644   RAISING    MVT_ERROR.
         ENDIF.

         SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVIT
                  FROM ZTIVIT
                  FOR ALL ENTRIES IN XBSEG
                  WHERE ZFIVNO  EQ XBSEG-ZFIVNO
                  AND   ZFIVDNO EQ XBSEG-ZFIVDNO
                  AND   ZFPOTY  EQ 'S'
                  AND ( NDFTX   EQ SPACE
                  OR    NDFTX   IS NULL ).

         REFRESH : IT_ZSIVHSTIT.
         LOOP AT IT_ZSIVIT.
            W_TABIX = SY-TABIX.

*>통관요청 아이템 수정 작업.
            READ TABLE XBSEG WITH KEY ZFIVNO  = IT_ZSIVIT-ZFIVNO
                                      ZFIVDNO = IT_ZSIVIT-ZFIVDNO.
*--------------------------------------------------------------------
*>통관금액 아이템 조정 작업.
*--------------------------------------------------------------------
            IF SY-SUBRC EQ 0.
               IT_ZSIVIT-ZFIVAMK = XBSEG-WRBTR.
            ENDIF.
            IT_ZSIVIT-NDFTX = 'X'.

            MODIFY IT_ZSIVIT INDEX W_TABIX.

            MOVE-CORRESPONDING IT_ZSIVIT TO IT_ZSIVHSTIT.
            MOVE: ZTIVHST-ZFIVHST        TO IT_ZSIVHSTIT-ZFIVHST,
                  IT_ZSIVIT-ZFIVAMK      TO IT_ZSIVHSTIT-ZFIVAMK,

                  T001-WAERS             TO IT_ZSIVHSTIT-ZFKRW,
                  SY-MANDT               TO IT_ZSIVHSTIT-MANDT.
            APPEND IT_ZSIVHSTIT.
         ENDLOOP.

         MODIFY ZTIVIT FROM TABLE IT_ZSIVIT.
         IF SY-SUBRC NE 0.
            MESSAGE E646   RAISING    MVT_ERROR.
         ENDIF.

         INSERT ZTIVHSTIT FROM TABLE IT_ZSIVHSTIT.
         IF SY-SUBRC NE 0.
            MESSAGE E646   RAISING    MVT_ERROR.
         ENDIF.

*         W_ZFGRST = 'Y'.
*         LOOP AT IT_ZTIVIT  WHERE UMSON EQ 'X'.
*            W_TABIX = SY-TABIX.
*            IF IT_ZTIVIT-GRMENGE NE IT_ZTIVIT-GRTOTMN.
*               W_ZFGRST = 'P'.  EXIT.
*            ENDIF.
*         ENDLOOP.
*         MODIFY ZTIVIT FROM TABLE IT_ZTIVIT.
*         IF SY-SUBRC NE 0.
*            MESSAGE E646   RAISING    MVT_ERROR.
*         ENDIF.
*>
*         MOVE : ZTIVHST-ZFIVHST   TO   ZTIV-ZFIVHST,
*                W_ZFGRST          TO   ZTIV-ZFGRST,
*                SY-UNAME          TO   ZTIV-UNAM,
*                SY-DATUM          TO   ZTIV-UDAT.

         UPDATE  ZTIV SET: UNAM    = SY-UNAME,
                           ZFIVHST = ZTIVHST-ZFIVHST,
                           UDAT    = SY-DATUM
                      WHERE ZFIVNO EQ P_ZFIVNO.
*         UPDATE  ZTIV.
         IF SY-SUBRC NE 0.
            MESSAGE E952   RAISING    POST_ERROR.
         ENDIF.
*
  ENDIF.


ENDFUNCTION.
