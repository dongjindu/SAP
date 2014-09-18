FUNCTION ZIM_BAPI_ACC_GL_POSTING_POST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(DOCUMENTHEADER) LIKE  BAPIACHE08 STRUCTURE  BAPIACHE08
*"  EXPORTING
*"     VALUE(OBJ_TYPE) LIKE  BAPIACHE02-OBJ_TYPE
*"     VALUE(OBJ_KEY) LIKE  BAPIACHE02-OBJ_KEY
*"     VALUE(OBJ_SYS) LIKE  BAPIACHE02-OBJ_SYS
*"  TABLES
*"      ACCOUNTGL STRUCTURE  BAPIACGL08
*"      CURRENCYAMOUNT STRUCTURE  BAPIACCR08
*"      RETURN STRUCTURE  BAPIRET2
*"      EXTENSION1 STRUCTURE  BAPIEXTC OPTIONAL
*"  EXCEPTIONS
*"      NOT_POSTING
*"----------------------------------------------------------------------
TABLES : T000.

   REFRESH : ACCOUNTGL, CURRENCYAMOUNT, RETURN, EXTENSION1.
   CLEAR : DOCUMENTHEADER.

   SELECT SINGLE * FROM T000
                   WHERE MANDT EQ SY-MANDT.

   MOVE : 'IDOC'      TO DOCUMENTHEADER-OBJ_TYPE,
          '1'          TO DOCUMENTHEADER-OBJ_KEY,
          T000-LOGSYS  TO DOCUMENTHEADER-OBJ_SYS,
          SY-UNAME     TO DOCUMENTHEADER-USERNAME,
          'BAPIs Test' TO DOCUMENTHEADER-HEADER_TXT,
          SPACE        TO DOCUMENTHEADER-OBJ_KEY_R,
          'C100'       TO DOCUMENTHEADER-COMP_CODE,
          SPACE        TO DOCUMENTHEADER-AC_DOC_NO,
          '2001'       TO DOCUMENTHEADER-FISC_YEAR,
          '20010731'   TO DOCUMENTHEADER-DOC_DATE,
          '20010731'   TO DOCUMENTHEADER-PSTNG_DATE,
          SPACE        TO DOCUMENTHEADER-TRANS_DATE,
          '07'         TO DOCUMENTHEADER-FIS_PERIOD,
          'RE'         TO DOCUMENTHEADER-DOC_TYPE,
          ''           TO DOCUMENTHEADER-REF_DOC_NO,
          ''           TO DOCUMENTHEADER-COMPO_ACC,
          ''           TO DOCUMENTHEADER-REASON_REV.



   MOVE: 1             TO ACCOUNTGL-ITEMNO_ACC, "회계전표 개별항목번호
         '0000700003'      TO ACCOUNTGL-GL_ACCOUNT, "총계정원장계정
         'C100'        TO ACCOUNTGL-COMP_CODE,  "회사코드
         '20000731'    TO ACCOUNTGL-PSTNG_DATE, "전표전기일
         'RE'          TO ACCOUNTGL-DOC_TYPE,   "전표유형
         SPACE         TO ACCOUNTGL-AC_DOC_NO,  "회계전표번호
         '2001'        TO ACCOUNTGL-FISC_YEAR,  "회계연도
         '07'          TO ACCOUNTGL-FIS_PERIOD, "회계기간
         'X'           TO ACCOUNTGL-STAT_CON,   "통계용 개별항목지시자
         SPACE         TO ACCOUNTGL-REF_KEY_1,  "거래처참조키
         SPACE         TO ACCOUNTGL-REF_KEY_2,  "거래처참조키
         SPACE         TO ACCOUNTGL-REF_KEY_3,  "개별항목참조키
         SPACE         TO ACCOUNTGL-CUSTOMER,   "고객번호
         SPACE         TO ACCOUNTGL-VENDOR_NO,  "구매처.
         '지정'        TO ACCOUNTGL-ALLOC_NMBR, "지정번호
         '품목'        TO ACCOUNTGL-ITEM_TEXT,  "품목텍스트
         SPACE         TO ACCOUNTGL-BUS_AREA,   "사업영역
         SPACE         TO ACCOUNTGL-COSTCENTER, "코스트센터
         SPACE         TO ACCOUNTGL-ACTTYPE,    "액티비티유형
         SPACE         TO ACCOUNTGL-ORDERID,    "오더번호
         SPACE         TO ACCOUNTGL-ORIG_GROUP,
                                          "원가요소분할로서의 오리진그룹
         SPACE         TO ACCOUNTGL-COST_OBJ,   "원가집적대상
         SPACE         TO ACCOUNTGL-PROFIT_CTR, "손익센터
         SPACE         TO ACCOUNTGL-PART_PRCTR, "파트너 손익센터
         SPACE         TO ACCOUNTGL-WBS_ELEMENT,
                                            "작업분할구조요소 (WBS 요소)
         SPACE         TO ACCOUNTGL-NETWORK,    "계정지정 네트웍번호
         SPACE         TO ACCOUNTGL-ROUTING_NO, "오더의 작업공정번호
         SPACE         TO ACCOUNTGL-ORDER_ITNO. "오더품목번호
   APPEND ACCOUNTGL.


   MOVE: 2             TO ACCOUNTGL-ITEMNO_ACC, "회계전표 개별항목번호
         '0002151120'     TO ACCOUNTGL-GL_ACCOUNT, "총계정원장계정
         'C100'        TO ACCOUNTGL-COMP_CODE,  "회사코드
         '20000731'    TO ACCOUNTGL-PSTNG_DATE, "전표전기일
         'RE'          TO ACCOUNTGL-DOC_TYPE,   "전표유형
         SPACE         TO ACCOUNTGL-AC_DOC_NO,  "회계전표번호
         '2001'        TO ACCOUNTGL-FISC_YEAR,  "회계연도
         '07'          TO ACCOUNTGL-FIS_PERIOD, "회계기간
         'X'           TO ACCOUNTGL-STAT_CON,   "통계용 개별항목지시자
         SPACE         TO ACCOUNTGL-REF_KEY_1,  "거래처참조키
         SPACE         TO ACCOUNTGL-REF_KEY_2,  "거래처참조키
         SPACE         TO ACCOUNTGL-REF_KEY_3,  "개별항목참조키
         SPACE         TO ACCOUNTGL-CUSTOMER,   "고객번호
         SPACE         TO ACCOUNTGL-VENDOR_NO,  "구매처.
         '지정'        TO ACCOUNTGL-ALLOC_NMBR, "지정번호
         '품목'        TO ACCOUNTGL-ITEM_TEXT,  "품목텍스트
         SPACE         TO ACCOUNTGL-BUS_AREA,   "사업영역
         SPACE         TO ACCOUNTGL-COSTCENTER, "코스트센터
         SPACE         TO ACCOUNTGL-ACTTYPE,    "액티비티유형
         SPACE         TO ACCOUNTGL-ORDERID,    "오더번호
         SPACE         TO ACCOUNTGL-ORIG_GROUP,
                                          "원가요소분할로서의 오리진그룹
         SPACE         TO ACCOUNTGL-COST_OBJ,   "원가집적대상
         SPACE         TO ACCOUNTGL-PROFIT_CTR, "손익센터
         SPACE         TO ACCOUNTGL-PART_PRCTR, "파트너 손익센터
         SPACE         TO ACCOUNTGL-WBS_ELEMENT,
                                            "작업분할구조요소 (WBS 요소)
         SPACE         TO ACCOUNTGL-NETWORK,    "계정지정 네트웍번호
         SPACE         TO ACCOUNTGL-ROUTING_NO, "오더의 작업공정번호
         SPACE         TO ACCOUNTGL-ORDER_ITNO. "오더품목번호
   APPEND ACCOUNTGL.


   MOVE: 1       TO CURRENCYAMOUNT-ITEMNO_ACC,  "회계전표 개별항목번호
         '00'    TO CURRENCYAMOUNT-CURR_TYPE,   "통화유형 및 평가뷰
         'KRW'   TO CURRENCYAMOUNT-CURRENCY,    "통화키
         SPACE   TO CURRENCYAMOUNT-CURRENCY_ISO,"ISO 코드통화
         '139854.0000'  TO CURRENCYAMOUNT-AMT_DOCCUR,  "전표통화금액
         0 TO CURRENCYAMOUNT-EXCH_RATE,   "환율
         0 TO CURRENCYAMOUNT-EXCH_RATE_V. "간접호가환율
   APPEND CURRENCYAMOUNT.

   MOVE: 2       TO CURRENCYAMOUNT-ITEMNO_ACC,  "회계전표 개별항목번호
         '00'    TO CURRENCYAMOUNT-CURR_TYPE,   "통화유형 및 평가뷰
         'KRW'   TO CURRENCYAMOUNT-CURRENCY,    "통화키
         SPACE   TO CURRENCYAMOUNT-CURRENCY_ISO,"ISO 코드통화
         '139854.0000'  TO CURRENCYAMOUNT-AMT_DOCCUR,  "전표통화금액
         0 TO CURRENCYAMOUNT-EXCH_RATE,   "환율
         0 TO CURRENCYAMOUNT-EXCH_RATE_V. "간접호가환율
   APPEND CURRENCYAMOUNT.










   CALL FUNCTION 'BAPI_ACC_GL_POSTING_POST'
       EXPORTING
         DOCUMENTHEADER = DOCUMENTHEADER
       IMPORTING
         OBJ_TYPE       =  OBJ_TYPE
         OBJ_KEY        =  OBJ_KEY
         OBJ_SYS        =  OBJ_SYS
       TABLES
         ACCOUNTGL      = ACCOUNTGL
         CURRENCYAMOUNT = CURRENCYAMOUNT
         RETURN         = RETURN
         EXTENSION1     = EXTENSION1
       EXCEPTIONS
         OTHERS         =  1.

  IF SY-SUBRC <> 0.
     ROLLBACK WORK.
  ELSE.
     COMMIT WORK.
  ENDIF.

ENDFUNCTION.
