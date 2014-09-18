FUNCTION ZIM_INSURANCE_LGI_TO_LG.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFREQNO) LIKE  ZTINS-ZFREQNO
*"     VALUE(ZFINSEQ) LIKE  ZTINS-ZFINSEQ
*"     VALUE(ZFAMDNO) LIKE  ZTINS-ZFAMDNO
*"  EXCEPTIONS
*"      RCV_ERROR
*"      NOT_FOUND
*"      NOT_SELECT
*"----------------------------------------------------------------------
DATA : BUFFER_POINTER     TYPE     I.
DATA : PARA_LENG          TYPE     I.
DATA : W_INDEX            TYPE     I.
DATA : W_MOD              TYPE     I.
DATA : L_YAK(005)         TYPE     C.
DATA : W_AMEND_MARK.
DATA : L_ZFLAGR           LIKE     ZTINSAGR-ZFLAGR.


DATA : BEGIN OF TEXT_RECORD,
       REC_TEXT(70)          VALUE   SPACE,
       CR_LF    TYPE   X     VALUE   '0A'.
DATA : END   OF TEXT_RECORD.

DATA : L_TCURC            LIKE TCURC.

DATA : BEGIN OF WA,
       NETWORK_GB(002),    ">사용구분.
       SAUPJA_NO(025),     ">대표 사업자등록번호.
       BOJONG(001),        ">수출입구분
       REF_NO(020),        ">문서관리번호
       REF_CHNG_NO(002),   ">문서관리번호 차수
       CONT_DATE(009),     ">계약체결일자.
       CONT_GB(001),       ">계약구분
       POL_NO(015),        ">증권번호
       CHNG_SER(003),      ">변경회차
       ENDS_SER(004),      ">배서회차
       OP_POL_NO(015),     ">OP증권번호
       CNTR_ID(015),       ">보험계약자
       CNTR_NM(050),       ">보험계약자 명
       REL_ID(015),        ">피보험자 코드
       REL_NM(050),        ">피보험자 명
       OBJT_CD(007),       ">목적물 코드
       OBJT_TEXT(840),     ">목적물 내역
       HS_CD(010),         ">국제분류코드
       ST_AREA_CD(002),    ">출발지 코드
       ST_AREA_TXT(070),   ">출발지 명
       TRAN_AREA_CD(002),  ">환적지 코드
       TRAN_AREA_TXT(070), ">환적지 명
       ARR_AREA_CD(002),   ">도착지 코드
       ARR_AREA_TXT(070),  ">도착지 명
       LAST_AREA_CD(002),  ">최종도착지 코드
       LAST_AREA_TXT(070),    ">최종도착지 명
       CLM_AGENT(300),        ">CLAIM AGENT
       SVY_AGENT(300),        ">SURVEY AGENT
       PRE_INV_VAL_CURR(003), ">화물가액 화폐코드(전)
       PRE_INV_VAL(020),      ">화물가액  (전)
       PRE_INV_PRO_RATE(011), ">화물 희망이익율 (전)
       PRE_INV_EXCH_RATE(011),">화물가입금액 환율(전)
       PRE_INV_AMT_CURR(003), ">화물가입 화폐코드(전)
       PRE_INV_AMT(020),      ">화물가입금액(전)
       PRE_DTY_VAL_CURR(003), ">관세 가액화폐코드(전)
       PRE_DTY_VAL(020),      ">관세 가액 (전)
       PRE_DTY_PRO_RATE(011), ">관세율 (전)
       PRE_DTY_AMT_CURR(003), ">관세 가입화폐코드(전)
       PRE_DTY_AMT(020),      ">관세 가입금액 (전)
       INV_VAL_CURR(003),     ">화물가액 화폐코드(후)
       INV_VAL(020),          ">화물가액  (후)
       INV_PRO_RATE(011),     ">화물 희망이익율 (후)
       INV_EXCH_RATE(011),    ">화물가입금액 환율(후)
       INV_AMT_CURR(003),     ">화물가입 화폐코드(후)
       INV_AMT(020),          ">화물가입금액(후)
       DTY_VAL_CURR(003),     ">관세 가액화폐코드(후)
       DTY_VAL(020),          ">관세 가액 (후)
       DTY_PRO_RATE(011),     ">관세율 (후)
       DTY_AMT_CURR(003),     ">관세 가입화폐코드(후)
       DTY_AMT(020),          ">관세 가입금액 (후)
       INV_DIFF_AMT(020),     ">화물가입금액 발생분
       DTY_DIFF_AMT(020),     ">관세가입금액 발생분
       REF_CD1(002),          ">문서코드1
       REF_CD2(002),          ">문서코드2
       REF_CD3(002),          ">문서코드3
       REF_TXT1(020),         ">문서번호1
       REF_TXT2(020),         ">문서번호2
       REF_TXT3(025),         ">문서번호3
       TOOL_NM(030),          ">운송용구명
       START_DATE(009),       ">출발일자
       BANK_CD(006),          ">질권기관
       BANK_NM(020),          ">질권기관 명
       YAK_CD1(005),          ">보험조건1
       YAK_CD2(005),          ">보험조건2
       YAK_CD3(005),          ">보험조건3
       YAK_CD4(005),          ">보험조건4
       YAK_CD5(005),          ">보험조건5
       YAK_CD6(005),          ">보험조건6
       YAK_CD7(005),          ">보험조건7
       YAK_CD8(005),          ">보험조건8
       YAK_CD9(005),          ">보험조건9
       YAK_CD10(005),         ">보험조건10
       PRE_INV_PROD_RATE(011),  ">화물요율 (전)
       PRE_VP_PROD_RATE(011),   ">VP요율 (전)
       PRE_DTY_PROD_RATE(011),  ">관세요율 (전)
       PRE_PROD_RATE(011),      ">합계요율(전)
       PRE_PREM_EXCHG(011),     ">보험료 적용환율(전)
       PRE_INV_E_PREM(020),     ">외화 화물보험료(전)
       PRE_VP_E_PREM(020),      ">외화 VP보험료(전)
       PRE_DTY_E_PREM(020),     ">외화 관세보험료(전)
       PRE_E_PREM(020),         ">외화 합계 보험료 (전)
       PRE_INV_K_PREM(020),     ">원화 화물보험료(전)
       PRE_VP_K_PREM(020),      ">원화 VP보험료(전)
       PRE_DTY_K_PREM(020),     ">원화 관세보험료(전)
       PRE_K_PREM(020),         ">원화 합계 보험료 (전)
       INV_PROD_RATE(011),      ">화물요율 (후)
       VP_PROD_RATE(011),       ">VP요율 (후)
       DTY_PROD_RATE(011),      ">관세요율 (후)
       PROD_RATE(011),          ">합계요율(후)
       PREM_EXCHG(011),         ">보험료 적용환율(후)
       INV_E_PREM(020),         ">외화 화물보험료(후)
       VP_E_PREM(020),          ">외화 VP보험료(후)
       DTY_E_PREM(020),         ">외화 관세보험료(후)
       E_PREM(020),             ">외화 합계 보험료 (후)
       INV_K_PREM(020),         ">원화 화물보험료(후)
       VP_K_PREM(020),          ">원화 VP보험료(후)
       DTY_K_PREM(020),         ">원화 관세보험료(후)
       K_PREM(020),             ">원화 합계 보험료 (후)
       INV_E_DIFF_PREM(020),    ">외화 화물보험료(차액)
       VP_E_DIFF_PREM(020),     ">외화 VP보험료(차액)
       DTY_E_DIFF_PREM(020),    ">외화 관세보험료(차액)
       E_DIFF_PREM(020),        ">외화 합계 보험료 (차액)
       INV_K_DIFF_PREM(020),    ">원화 화물보험료(차액)
       VP_K_DIFF_PREM(020),     ">원화 VP보험료(차액)
       DTY_K_DIFF_PREM(020),    ">원화 관세보험료(차액)
       K_DIFF_PREM(020),        ">원화 합계 보험료 (차액)
       PRT_ORG_CNT(002),        ">정본수
       PRT_COPY_CNT(002),       ">사본수
       PRT_BAL_DATE(009),       ">증권발급일자
       PRT_AREA_CD(004),        ">증권발급지
       INV_BAL_DATE(009),       ">인보이스 발행일
       INPUT_DATE(009),         ">입력일자
       USD_EX_RATE(11),         ">USD 환율
       USD_E_DIFF_PREM(020),    ">USD 합계 보험료 (차액)
       RCV_MARK(001).           ">수신확인여부
DATA : END OF WA.

*-----------------------------------------------------------------
*>> 보험 테이블 SEELCT.
*-----------------------------------------------------------------
   CALL  FUNCTION 'ZIM_GET_INSURANCE_DOC'
         EXPORTING
             ZFREQNO             =          ZFREQNO
             ZFINSEQ             =          ZFINSEQ
             ZFAMDNO             =          ZFAMDNO
         IMPORTING
             W_ZTINS             =          ZTINS
             W_ZTINSRSP          =          ZTINSRSP
             W_ZTINSSG3          =          ZTINSSG3
         TABLES
             IT_ZSINSAGR        =          IT_ZSINSAGR
             IT_ZSINSAGR_ORG    =          IT_ZSINSAGR_ORG
             IT_ZSINSSG2        =          IT_ZSINSSG2
             IT_ZSINSSG2_ORG    =          IT_ZSINSSG2_ORG
             IT_ZSINSSG5        =          IT_ZSINSSG5
             IT_ZSINSSG5_ORG    =          IT_ZSINSSG5_ORG
         EXCEPTIONS
             NOT_FOUND         =    4
             NOT_INPUT         =    8.

   IF SY-SUBRC NE 0.
      RAISE   NOT_FOUND.
   ENDIF.

   MOVE-CORRESPONDING : ZTINS     TO   *ZTINS,
                        ZTINSRSP  TO   *ZTINSRSP,
                        ZTINSSG3  TO   *ZTINSSG3.

*> DATA MOVE LOGIC.
*   MOVE : '01'          TO  WA-NETWORK_GB,        ">사용구분(VAN '01')
*          '1168118600'  TO  WA-SAUPJA_NO,         ">대표사업자번호
*          'I'           TO  WA-BOJONG,            ">수출입구분
*          'EID110860'   TO  WA-REF_NO(10),        ">문서관리번호(PK)
*          '00'          TO WA-REF_CHNG_NO.   ">문서관리번호 차수

   MOVE : '01'          TO  WA-NETWORK_GB,        ">사용구분(VAN '01')
          ZTINS-ZFELTXN TO  WA-SAUPJA_NO,         ">대표사업자번호
          'I'           TO  WA-BOJONG,            ">수출입구분
          ZTINS-ZFREQNO TO  WA-REF_NO(10),        ">문서관리번호(PK)
          ZTINS-ZFINSEQ TO  WA-REF_NO+10(5),      ">문서관리번호(PK)
*          ZTINS-ZFAMDNO TO  WA-REF_NO+15(5),      ">문서관리번호(PK)
          ZTINS-ZFAMDNO+3(2) TO WA-REF_CHNG_NO.   ">문서관리번호 차수
**

*----------------------------------------------------------------------
*> Native SQL LG 화재 테이블(TMV20)
*>   DESC : DB Link 후 Synonym Create 후 작업함.
*----------------------------------------------------------------------
  EXEC SQL.
      SELECT *
         INTO :WA
         FROM ZLGITMV20
         WHERE NETWORK_GB     =    :WA-NETWORK_GB
         AND   SAUPJA_NO      =    :WA-SAUPJA_NO
         AND   BOJONG         =    :WA-BOJONG
         AND   REF_NO         =    :WA-REF_NO
         AND   REF_CHNG_NO    =    :WA-REF_CHNG_NO
   ENDEXEC.

   IF SY-SUBRC NE 0.
      RAISE NOT_SELECT.
   ENDIF.

*> 부보조건.
   ZTINS-ZFBSINS = WA-YAK_CD1.   ">기본 조건.

   REFRESH : IT_ZSINSAGR.
   CLEAR : L_ZFLAGR, IT_ZSINSAGR.

   IF WA-YAK_CD2 IS INITIAL.
      ADD 10 TO L_ZFLAGR.
      MOVE : L_ZFLAGR   TO IT_ZSINSAGR-ZFLAGR,
             WA-YAK_CD2 TO IT_ZSINSAGR-ZFINSCD.
      PERFORM   GET_DD07T_SELECT(SAPMZIM00)
                USING     'ZDINSCD'  IT_ZSINSAGR-ZFINSCD
                CHANGING  IT_ZSINSAGR-ZFCNCDNM.
      APPEND  IT_ZSINSAGR.
   ENDIF.

   IF WA-YAK_CD3 IS INITIAL.
      ADD 10 TO L_ZFLAGR.
      MOVE : L_ZFLAGR   TO IT_ZSINSAGR-ZFLAGR,
             WA-YAK_CD3 TO IT_ZSINSAGR-ZFINSCD.
      PERFORM   GET_DD07T_SELECT(SAPMZIM00)
                USING     'ZDINSCD'  IT_ZSINSAGR-ZFINSCD
                CHANGING  IT_ZSINSAGR-ZFCNCDNM.
      APPEND  IT_ZSINSAGR.
   ENDIF.

   IF WA-YAK_CD4 IS INITIAL.
      ADD 10 TO L_ZFLAGR.
      MOVE : L_ZFLAGR   TO IT_ZSINSAGR-ZFLAGR,
             WA-YAK_CD4 TO IT_ZSINSAGR-ZFINSCD.
      PERFORM   GET_DD07T_SELECT(SAPMZIM00)
                USING     'ZDINSCD'  IT_ZSINSAGR-ZFINSCD
                CHANGING  IT_ZSINSAGR-ZFCNCDNM.
      APPEND  IT_ZSINSAGR.
   ENDIF.

   IF WA-YAK_CD5 IS INITIAL.
      ADD 10 TO L_ZFLAGR.
      MOVE : L_ZFLAGR   TO IT_ZSINSAGR-ZFLAGR,
             WA-YAK_CD5 TO IT_ZSINSAGR-ZFINSCD.
      PERFORM   GET_DD07T_SELECT(SAPMZIM00)
                USING     'ZDINSCD'  IT_ZSINSAGR-ZFINSCD
                CHANGING  IT_ZSINSAGR-ZFCNCDNM.
      APPEND  IT_ZSINSAGR.
   ENDIF.

   IF WA-YAK_CD6 IS INITIAL.
      ADD 10 TO L_ZFLAGR.
      MOVE : L_ZFLAGR   TO IT_ZSINSAGR-ZFLAGR,
             WA-YAK_CD6 TO IT_ZSINSAGR-ZFINSCD.
      PERFORM   GET_DD07T_SELECT(SAPMZIM00)
                USING     'ZDINSCD'  IT_ZSINSAGR-ZFINSCD
                CHANGING  IT_ZSINSAGR-ZFCNCDNM.
      APPEND  IT_ZSINSAGR.
   ENDIF.

   IF WA-YAK_CD7 IS INITIAL.
      ADD 10 TO L_ZFLAGR.
      MOVE : L_ZFLAGR   TO IT_ZSINSAGR-ZFLAGR,
             WA-YAK_CD7 TO IT_ZSINSAGR-ZFINSCD.
      PERFORM   GET_DD07T_SELECT(SAPMZIM00)
                USING     'ZDINSCD'  IT_ZSINSAGR-ZFINSCD
                CHANGING  IT_ZSINSAGR-ZFCNCDNM.
      APPEND  IT_ZSINSAGR.
   ENDIF.

   IF WA-YAK_CD8 IS INITIAL.
      ADD 10 TO L_ZFLAGR.
      MOVE : L_ZFLAGR   TO IT_ZSINSAGR-ZFLAGR,
             WA-YAK_CD8 TO IT_ZSINSAGR-ZFINSCD.
      PERFORM   GET_DD07T_SELECT(SAPMZIM00)
                USING     'ZDINSCD'  IT_ZSINSAGR-ZFINSCD
                CHANGING  IT_ZSINSAGR-ZFCNCDNM.
      APPEND  IT_ZSINSAGR.
   ENDIF.

   IF WA-YAK_CD9 IS INITIAL.
      ADD 10 TO L_ZFLAGR.
      MOVE : L_ZFLAGR   TO IT_ZSINSAGR-ZFLAGR,
             WA-YAK_CD9 TO IT_ZSINSAGR-ZFINSCD.
      PERFORM   GET_DD07T_SELECT(SAPMZIM00)
                USING     'ZDINSCD'  IT_ZSINSAGR-ZFINSCD
                CHANGING  IT_ZSINSAGR-ZFCNCDNM.
      APPEND  IT_ZSINSAGR.
   ENDIF.

   IF WA-YAK_CD10 IS INITIAL.
      ADD 10 TO L_ZFLAGR.
      MOVE : L_ZFLAGR   TO IT_ZSINSAGR-ZFLAGR,
             WA-YAK_CD10 TO IT_ZSINSAGR-ZFINSCD.
      PERFORM   GET_DD07T_SELECT(SAPMZIM00)
                USING     'ZDINSCD'  IT_ZSINSAGR-ZFINSCD
                CHANGING  IT_ZSINSAGR-ZFCNCDNM.
      APPEND  IT_ZSINSAGR.
   ENDIF.


*>> TEST...
   MOVE : WA-OBJT_CD      TO   ZTINSRSP-ZFMJM,   ">목적물코드.
          WA-INV_VAL_CURR TO   ZTINS-ZFINAMTC,   ">보험료통화.
          ZTINS-ZFINAMTC  TO   ZTINSRSP-ZFDAMIC,
          ZTINS-ZFINAMTC  TO   ZTINSRSP-ZFCAMIC,
          ZTINS-ZFINAMTC  TO   ZTINSRSP-ZFTAMIC.

*>외화 보험료
*   ZTINS-ZFINAMT = WA-INV_VAL+1.
   ZTINS-ZFINAMT = WA-E_DIFF_PREM+1.
   PERFORM  SET_CURR_CONV_TO_INTERNAL(SAPLZIM4)
                                CHANGING ZTINS-ZFINAMT
                                         ZTINS-ZFINAMTC.
   MOVE : ZTINS-ZFINAMTC  TO   ZTINSRSP-ZFTAMIC,
          ZTINS-ZFINAMT   TO   ZTINSRSP-ZFTAMI.

*>희망이익율.
   MOVE : WA-INV_PRO_RATE+1  TO   ZTINS-ZFPEIV.

*> 적용환율.
   MOVE : WA-INV_EXCH_RATE+1 TO   ZTINSRSP-ZFEXRT,
          1                  TO   ZTINSRSP-FFACT.

*>원화.
   MOVE : 'KRW'              TO   ZTINS-ZFKRW,
          WA-K_DIFF_PREM+1   TO   ZTINS-ZFKRWAMT,   ">
*          WA-INV_AMT+1       TO   ZTINS-ZFKRWAMT,
          'KRW'              TO   ZTINSRSP-ZFTPRC,
          'KRW'              TO   ZTINSRSP-ZFCPRC,
          'KRW'              TO   ZTINSRSP-ZFDPRC,
          'KRW'              TO   ZTINSRSP-ZFVPRC,
          'KRW'              TO   ZTINSRSP-ZFIPRC.

   PERFORM  SET_CURR_CONV_TO_INTERNAL(SAPLZIM4)
                                CHANGING ZTINS-ZFKRWAMT
                                         ZTINS-ZFKRW.

*>>적용환율.
   MOVE : WA-PREM_EXCHG+1   TO    ZTINSRSP-ZFEXRT,
          1                  TO   ZTINSRSP-FFACT.

*-----------------------------------------------------------
*> 부보요율(합계요율)
* ----> 차액을 가져오는 로직 막음.
*-------> 최종금액을 가져오는 로직 막음.
*-----------------------------------------------------------
   MOVE : WA-PROD_RATE+1       TO    ZTINS-ZFINRT,
          WA-E_DIFF_PREM+1     TO    ZTINSRSP-ZFTAMI,   ">외화합계.
          WA-INV_E_DIFF_PREM+1 TO    ZTINSRSP-ZFCAMI,   ">Cargo 외화.
          WA-DTY_E_DIFF_PREM+1 TO    ZTINSRSP-ZFDAMI,   ">관세 외화.
          WA-INV_K_DIFF_PREM+1 TO    ZTINSRSP-ZFCPR,    ">원화 CARGO
          WA-VP_K_DIFF_PREM+1  TO    ZTINSRSP-ZFVPR,    ">원화 VP
          WA-DTY_K_DIFF_PREM+1 TO    ZTINSRSP-ZFDPR,    ">원환 관세
          WA-K_DIFF_PREM+1     TO    ZTINSRSP-ZFTPR.    ">원화 토탈.
*>>>>>> 최종 금액을 가져옴.
*  MOVE : WA-E_PREM+1       TO    ZTINSRSP-ZFTAMI,   ">외화합계.
*         WA-INV_E_PREM+1   TO    ZTINSRSP-ZFCAMI,   ">Cargo 외화.
*         WA-DTY_E_PREM+1   TO    ZTINSRSP-ZFDAMI,   ">관세 외화.
*         WA-INV_K_PREM+1   TO    ZTINSRSP-ZFCPR,    ">원화 CARGO
*         WA-VP_K_PREM+1    TO    ZTINSRSP-ZFVPR,    ">원화 VP
*         WA-DTY_K_PREM+1   TO    ZTINSRSP-ZFDPR,    ">원환 관세
*         WA-K_PREM+1       TO    ZTINSRSP-ZFTPR.    ">원화 토탈.

*>원화.
   PERFORM  SET_CURR_CONV_TO_INTERNAL(SAPLZIM4)
                                USING    ZTINSRSP-ZFCPR
                                         ZTINS-ZFKRW.

   PERFORM  SET_CURR_CONV_TO_INTERNAL(SAPLZIM4)
                                USING    ZTINSRSP-ZFVPR
                                         ZTINS-ZFKRW.

   PERFORM  SET_CURR_CONV_TO_INTERNAL(SAPLZIM4)
                                USING    ZTINSRSP-ZFDPR
                                         ZTINS-ZFKRW.

   PERFORM  SET_CURR_CONV_TO_INTERNAL(SAPLZIM4)
                                USING    ZTINSRSP-ZFTPR
                                         ZTINS-ZFKRW.

*>외화.
   PERFORM  SET_CURR_CONV_TO_INTERNAL(SAPLZIM4)
                                USING    ZTINSRSP-ZFTAMI
                                         ZTINSRSP-ZFTAMIC.

   PERFORM  SET_CURR_CONV_TO_INTERNAL(SAPLZIM4)
                                USING    ZTINSRSP-ZFCAMI
                                         ZTINSRSP-ZFTAMIC.

   PERFORM  SET_CURR_CONV_TO_INTERNAL(SAPLZIM4)
                                USING    ZTINSRSP-ZFDAMI
                                         ZTINSRSP-ZFTAMIC.

   CALL FUNCTION 'ZIM_DATE_CONVERT_INTERNAL'
        EXPORTING
            I_DATE     =      WA-CONT_DATE    ">계약일자
        IMPORTING
            E_DATE     =      ZTINSRSP-ZFISDT
        EXCEPTIONS
            OTHERS     =      4.
   IF SY-SUBRC NE 0.
      EXIT.
   ENDIF.

   MOVE : WA-POL_NO  TO  ZTINS-ZFINNO,
          'O'        TO  ZTINS-ZFDOCST,
          'R'        TO  ZTINS-ZFEDIST.


*   WRITE : / WA-CONT_DATE,
*/,             WA-CONT_GB,
*/,             WA-POL_NO,
*  /,           WA-CHNG_SER,
*/,             WA-ENDS_SER,
*  /,           WA-OP_POL_NO,
*    /,         WA-CNTR_ID,
*      /,       WA-CNTR_NM,
*        /,     WA-REL_ID,
*/,             WA-REL_NM,
*  /,           WA-OBJT_CD,
*    /,         WA-OBJT_TEXT,
*      /,       WA-HS_CD,
*        /,     WA-ST_AREA_CD,
*          /,   WA-ST_AREA_TXT,
*/,             WA-TRAN_AREA_CD,
*  /,           WA-TRAN_AREA_TXT,
*    /,         WA-ARR_AREA_CD,
*      /,       WA-ARR_AREA_TXT,
*        /,     WA-LAST_AREA_CD,
*          /,   WA-LAST_AREA_TXT,
*/,             WA-CLM_AGENT,
*  /,           WA-SVY_AGENT,
*    /,         WA-PRE_INV_VAL_CURR,
*      /,       WA-PRE_INV_VAL ,
*        /,     WA-PRE_INV_PRO_RATE,
*          /,   WA-PRE_INV_EXCH_RATE,
*            /, WA-PRE_INV_AMT_CURR,
*/,             WA-PRE_INV_AMT ,
*  /,           WA-PRE_DTY_VAL_CURR,
*    /,         WA-PRE_DTY_VAL ,
*      /,       WA-PRE_DTY_PRO_RATE,
*        /,     WA-PRE_DTY_AMT_CURR,
*          /,   WA-PRE_DTY_AMT,
*            /, WA-INV_VAL_CURR,
*/,             WA-INV_VAL ,
*  /,           WA-INV_PRO_RATE,
*    /,         WA-INV_EXCH_RATE,
*      /,       WA-INV_AMT_CURR,
*        /,     WA-INV_AMT,
*          /,   WA-DTY_VAL_CURR,
*            /, WA-DTY_VAL,
*/,             WA-DTY_PRO_RATE,
*  /,           WA-DTY_AMT_CURR,
*    /,         WA-DTY_AMT,
*      /,       WA-INV_DIFF_AMT,
*        /,     WA-DTY_DIFF_AMT,
*          /,   WA-REF_CD1,
*            /, WA-REF_CD2,
*/,             WA-REF_CD3,
*  /,           WA-REF_TXT1,
*    /,         WA-REF_TXT2,
*/,             WA-REF_TXT3,
*/,             WA-TOOL_NM,
*  /,           WA-START_DATE,
*    /,         WA-BANK_CD,
*      /,       WA-BANK_NM,
*        /,     WA-YAK_CD1,
*/,             WA-YAK_CD2,
*  /,           WA-YAK_CD3,
*    /,         WA-YAK_CD4,
*      /,       WA-YAK_CD5,
*        /,     WA-YAK_CD6,
*          /,   WA-YAK_CD7,
*            /, WA-YAK_CD8,
*/,             WA-YAK_CD9,
*  /,           WA-YAK_CD10,
*    /,         WA-PRE_INV_PROD_RATE,
*      /,       WA-PRE_VP_PROD_RATE,
*        /,     WA-PRE_DTY_PROD_RATE,
*          /,   WA-PRE_PROD_RATE,
*            /, WA-PRE_PREM_EXCHG,
*/,             WA-PRE_INV_E_PREM,
*/,             WA-PRE_VP_E_PREM,
*/,             WA-PRE_DTY_E_PREM,
*  /,           WA-PRE_E_PREM,
*/,             WA-PRE_INV_K_PREM,
*  /,           WA-PRE_VP_K_PREM,
*    /,         WA-PRE_DTY_K_PREM,
*      /,       WA-PRE_K_PREM,
*        /,     WA-INV_PROD_RATE,
*          /,   WA-VP_PROD_RATE,
*            /, WA-DTY_PROD_RATE,
*/,             WA-PROD_RATE,
*  /,           WA-PREM_EXCHG,
*/,             WA-INV_E_PREM,
*  /,           WA-VP_E_PREM,
*    /,         WA-DTY_E_PREM,
*      /,       WA-E_PREM,
*        /,     WA-INV_K_PREM,
*          /,   WA-VP_K_PREM,
*            /, WA-DTY_K_PREM,
*/,             WA-K_PREM,
*  /,           WA-INV_E_DIFF_PREM,
*    /,         WA-VP_E_DIFF_PREM,
*      /,       WA-DTY_E_DIFF_PREM,
*        /,     WA-E_DIFF_PREM,
*/,             WA-INV_K_DIFF_PREM,
*  /,           WA-VP_K_DIFF_PREM,
*/,             WA-DTY_K_DIFF_PREM,
*  /,           WA-K_DIFF_PREM,
*    /,         WA-PRT_ORG_CNT,
*      /,       WA-PRT_COPY_CNT,
*        /,     WA-PRT_BAL_DATE,
*          /,   WA-PRT_AREA_CD,
*            /, WA-INV_BAL_DATE,
*/,             WA-INPUT_DATE,
*  /,           WA-USD_EX_RATE,
*/,             WA-USD_E_DIFF_PREM,
*  /,           WA-RCV_MARK.


  CALL FUNCTION 'ZIM_INSURANCE_MODIFY'
       EXPORTING
            W_OK_CODE      = 'SAVE'
            ZFREQNO        = ZTINS-ZFREQNO
            ZFINSEQ        = ZTINS-ZFINSEQ
            ZFAMDNO        = ZTINS-ZFAMDNO
            ZFSTATUS       = 'U'
            W_ZTINS        = ZTINS
            W_ZTINS_OLD    = *ZTINS
            W_ZTINSRSP     = ZTINSRSP
            W_ZTINSRSP_OLD = *ZTINSRSP
            W_ZTINSSG3     = ZTINSSG3
            W_ZTINSSG3_OLD = *ZTINSSG3
            W_AMEND        = W_AMEND_MARK
       TABLES
            IT_ZSINSAGR    = IT_ZSINSAGR
            IT_ZSINSSG2    = IT_ZSINSSG2
            IT_ZSINSSG5    = IT_ZSINSSG5
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
     RAISE RCV_ERROR.
  ENDIF.

ENDFUNCTION.
