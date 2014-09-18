FUNCTION ZIM_INSURANCE_LG_TO_LGI.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFREQNO) LIKE  ZTINS-ZFREQNO
*"     VALUE(ZFINSEQ) LIKE  ZTINS-ZFINSEQ
*"     VALUE(ZFAMDNO) LIKE  ZTINS-ZFAMDNO
*"  EXCEPTIONS
*"      SEND_ERROR
*"----------------------------------------------------------------------
DATA : BUFFER_POINTER     TYPE     I.
DATA : PARA_LENG          TYPE     I.
DATA : W_INDEX            TYPE     I.
DATA : W_MOD              TYPE     I.
DATA : L_INV_VAL(20).
DATA : L_YAK(005)         TYPE     C.
DATA : L_ZFAMDNO          LIKE     ZTINS-ZFAMDNO.
DATA : L_ZFIVAMT          LIKE     ZTINS-ZFIVAMT.

DATA : OLD_ZTINS          LIKE     ZTINS.
DATA : OLD_ZTINSRSP       LIKE     ZTINSRSP.

DATA : BEGIN OF TEXT_RECORD,
       REC_TEXT(69)          VALUE   SPACE,
       CR_LF    TYPE   X     VALUE   '0A'.
DATA : END   OF TEXT_RECORD.

DATA : L_TCURC            LIKE TCURC.

DATA : NETWORK_GB(002)    TYPE C,   ">사용구분(VAN '01', CERTI '02')
       SAUPJA_NO(015)     TYPE C,   ">대표사업자번호
       BOJONG(001)        TYPE C,   ">수출입구분('I'수입,'E'수출)
       REF_NO(020)        TYPE C,   ">문서관리번호(PK)
       REF_CHNG_NO(002)   TYPE C,   ">문서관리번호 차수
       CHUNG_DATE(009)    TYPE C,   ">청약일자
       CONT_GB(001)       TYPE C,   ">계약구분
       CHNG_GB_CD(002)    TYPE C,   ">변경사유
       POL_NO(015)        TYPE C,   ">증권번호
       CHUNG_NO(020)      TYPE C,   ">청약번호
       OP_NO(002)         TYPE C,   ">OP번호
       CNTR_ID(015)       TYPE C,   ">보험계약자
       CNTR_NM(050)       TYPE C,   ">보험계약자 명
       REL_ID(015)        TYPE C,   ">피보험자 코드
       REL_NM(050)        TYPE C,   ">피보험자 명
       OBJT_CD(007)       TYPE C,   ">목적물 코드
       OBJT_TEXT(840)     TYPE C,   ">목적물 내역
       HS_CD(010)         TYPE C,   ">국제분류코드
       ST_AREA_CD(002)    TYPE C,   ">출발지 코드
       ST_AREA_TXT(070)   TYPE C,   ">출발지 명
       TRAN_AREA_CD(002)  TYPE C,   ">환적지 코드
       TRAN_AREA_TXT(070) TYPE C,   ">환적지 명
       ARR_AREA_CD(002)   TYPE C,   ">도착지 코드
       ARR_AREA_TXT(070)  TYPE C,   ">도착지 명
       LAST_AREA_CD(002)  TYPE C,   ">최종도착지 코드
       LAST_AREA_TXT(070) TYPE C,   ">최종도착지 명
       CLM_AGENT(006)     TYPE C,   ">CLAIM AGENT
       SVY_AGENT(006)     TYPE C,   ">SURVEY AGENT
       INV_VAL_CURR(003)  TYPE C,   ">화물가액 화폐코드
*       INV_VAL(17)        TYPE C,   ">화물가액
*       INV_PRO_RATE(11)   TYPE C,   ">화물 희망이익율
       INV_VAL            TYPE F,   ">화물가액
       INV_PRO_RATE       TYPE F,   ">화물 희망이익율
*       INV_VAL(17,2)                ">화물가액
*       INV_PRO_RATE(11,6)           ">화물 희망이익율
       INV_AMT_CURR(003)  TYPE C,   ">화물가입 화폐코드
*       INV_AMT(17,2)                ">화물가입금액
*       INV_AMT(17)        TYPE C,    ">화물가입금액
       INV_AMT            TYPE F,    ">화물가입금액
       DTY_VAL_CURR(003)  TYPE C,    ">관세보험 가액화폐코드
*       DTY_VAL(17)        TYPE C,    ">관세보험 가액
*       DTY_PRO_RATE(11)   TYPE C,    ">관세율
       DTY_VAL            TYPE F,    ">관세보험 가액
       DTY_PRO_RATE       TYPE F,    ">관세율
*       DTY_VAL(17,2)                ">관세보험 가액
*       DTY_PRO_RATE(11,6)           ">관세율
       REF_CD1(002)       TYPE C,   ">문서코드1
       REF_CD2(002)       TYPE C,   ">문서코드2
       REF_CD3(002)       TYPE C,   ">문서코드3
       REF_TXT1(020)      TYPE C,   ">문서번호1
       REF_TXT2(020)      TYPE C,   ">문서번호2
       REF_TXT3(025)      TYPE C,   ">문서번호3
       GOOD_CD(002)       TYPE C,   ">포장코드
       TOOL_NM(030)       TYPE C,   ">운송용구명
       START_DATE(009)    TYPE C,   ">출발일자
*       HULL_TON,         6,0        ">총톤수
*       HULL_TON(6)        TYPE C,   ">총톤수
       HULL_TON           TYPE F,   ">총톤수
       HULL_BUILT_DATE    TYPE C,   ">건조일
*       HULL_YEAR(3)       TYPE C,   ">선령
       HULL_YEAR          TYPE F,   ">선령
       HULL_NATIONAL(2)   TYPE C,   ">국적
       HULL_CLASS(2)      TYPE C,   ">선급
       BANK_CD(006)       TYPE C,   ">질권기관
       YAK_CD1(005)       TYPE C,   ">보험조건1
       YAK_CD2(005)       TYPE C,   ">보험조건2
       YAK_CD3(005)       TYPE C,   ">보험조건3
       YAK_CD4(005)       TYPE C,   ">보험조건4
       YAK_CD5(005)       TYPE C,   ">보험조건5
       YAK_CD6(005)       TYPE C,   ">보험조건6
       YAK_CD7(005)       TYPE C,   ">보험조건7
*       PRT_ORG_CNT,      2,0        ">정본수
*       PRT_COPY_CNT,     2,0        ">사본수
*       PRT_ORG_CNT(2)     TYPE C,   ">정본수
*       PRT_COPY_CNT(2)    TYPE C,   ">사본수
       PRT_ORG_CNT        TYPE F,   ">정본수
       PRT_COPY_CNT       TYPE F,   ">사본수
       PRT_BAL_DATE(9)    TYPE C,   ">증권발급일자
       PRT_AREA_CD(004)   TYPE C,   ">증권발급지
       VAN_USER(030)      TYPE C,   ">담당자
       INPUT_DATE(9)      TYPE C,   ">입력일자
       PROC_STATUS(002)   TYPE C,   ">상태구분
       ERR_CODE(002)      TYPE C,   ">ERROR_CODE
       PROC_USER_ID(008)  TYPE C.   ">처리 LG화재 담당자

*>> TEST시 부보신청 데이타 미사용토록 하였음.
*>> GO LIVE시 막을 것.
*--->2001.11.26 INSERT KSB
   EXIT.



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
      RAISE   SEND_ERROR.
   ENDIF.

*> DATA MOVE LOGIC.
   MOVE : '01'          TO  NETWORK_GB,        ">사용구분(VAN '01')
          ZTINS-ZFELTXN TO  SAUPJA_NO,         ">대표사업자번호
          'I'           TO  BOJONG,            ">수출입구분
          ZTINS-ZFREQNO TO  REF_NO(10),        ">문서관리번호(PK)
          ZTINS-ZFINSEQ TO  REF_NO+10(5),      ">문서관리번호(PK)
*          ZTINS-ZFAMDNO TO  REF_NO+15(5),      ">문서관리번호(PK)
          ZTINS-ZFAMDNO+3(2) TO REF_CHNG_NO.   ">문서관리번호 차수

   CALL FUNCTION 'ZIM_DATE_CONVERT_EXTERNAL'
        EXPORTING
            I_DATE     =      ZTINS-ZFINSDT    ">청약일자
        IMPORTING
            E_DATE     =      CHUNG_DATE
        EXCEPTIONS
            OTHERS     =      4.
   IF SY-SUBRC NE 0.
      EXIT.
   ENDIF.


*>>계약구분
   CASE ZTINS-ZFEDFU.
      WHEN '2'.                           ">취소.
         MOVE '2' TO CONT_GB.
      WHEN OTHERS.
         IF ZTINS-ZFAMDNO GT '00000'.     ">변경.
            MOVE '1' TO CONT_GB.
         ELSE.                            ">원계약.
            MOVE '0' TO CONT_GB.
         ENDIF.
   ENDCASE.

*>">변경사유. 증권번호.

   IF ZTINS-ZFAMDNO GT '00000'.     ">변경.
      L_ZFAMDNO = ZTINS-ZFAMDNO - 1.

      SELECT SINGLE * INTO OLD_ZTINS
             FROM ZTINS
             WHERE    ZFREQNO EQ ZFREQNO
             AND      ZFINSEQ EQ ZFINSEQ
             AND      ZFAMDNO EQ L_ZFAMDNO.

      SELECT SINGLE * INTO OLD_ZTINSRSP
             FROM  ZTINSRSP
             WHERE    ZFREQNO EQ ZFREQNO
             AND      ZFINSEQ EQ ZFINSEQ
             AND      ZFAMDNO EQ L_ZFAMDNO.


      MOVE : '05'              TO   CHNG_GB_CD,  ">변경사유.
             OLD_ZTINS-ZFINNO  TO   POL_NO.      ">증권번호.
   ELSE.
      CLEAR : CHNG_GB_CD, POL_NO.
   ENDIF.

   CLEAR : CHUNG_NO,                    ">청약번호 .
           OBJT_CD,                     ">목적물 코드
           ST_AREA_CD,                  ">출발지 코드
           TRAN_AREA_CD,                ">환적지 코드
           ARR_AREA_CD,                 ">도착지 코드
           LAST_AREA_CD,                ">최종도착지 코드
           CLM_AGENT,                   ">CLAIM AGENT
           SVY_AGENT,                   ">SURVEY AGENT
           INV_AMT_CURR,                ">화물가입 화폐코드
           INV_AMT.                     ">화물가입금액

*-------------------------------------------------------
*> 목적물 내역...
* DESC : 70BYTE * 12 ==> 70BYTE당 NewLine Character.
*-------------------------------------------------------
   CLEAR : OBJT_TEXT, W_INDEX.
   LOOP AT IT_ZSINSSG2.
      IF IT_ZSINSSG2-ZFDSOG1 IS INITIAL.
         CONTINUE.
      ENDIF.
      ADD 1 TO W_INDEX.
      TEXT_RECORD-REC_TEXT = IT_ZSINSSG2-ZFDSOG1.

      BUFFER_POINTER    =   STRLEN( OBJT_TEXT ).
      PARA_LENG         =   STRLEN( TEXT_RECORD ).
      MOVE  TEXT_RECORD TO OBJT_TEXT+BUFFER_POINTER(PARA_LENG).

      IF W_INDEX GE 12.
         EXIT.
      ENDIF.
   ENDLOOP.

   IF SY-SUBRC EQ 0.
      PERFORM P3000_EDI_RECORD_ADJUST(SAPLZIM4)  CHANGING OBJT_TEXT.
   ENDIF.

   MOVE : ZTINS-ZFOPNO+7(2) TO OP_NO,         ">OP번호( 8~9자리 )
          ZTINS-ZFELTXN     TO CNTR_ID,       ">사업자등록번호.
          ZTINS-ZFELENM     TO CNTR_NM,       ">보험계약자 명.
          ZTINS-ZFELTXN     TO REL_ID,        ">피보험자 코드.
          ZTINS-ZFELENM     TO REL_NM,        ">피보험자 명.
          ZTINS-ZFRSTAW     TO HS_CD,         ">국제분류코드.
          ZTINSSG3-ZFSHCUNM TO ST_AREA_TXT,   ">출발지 명
          ZTINSSG3-ZFTRCUNM TO TRAN_AREA_TXT, ">환적지 명
          ZTINSSG3-ZFARCUNM TO ARR_AREA_TXT,  ">도착지 명
          ZTINSSG3-ZFLACUNM TO LAST_AREA_TXT. ">최종도착지 명

*> CURRENCY ISO CODE SELECT.
   CLEAR : L_TCURC.
   SELECT SINGLE * INTO L_TCURC FROM TCURC
                   WHERE WAERS  EQ  ZTINS-WAERS.
   IF SY-SUBRC NE 0.
      L_TCURC-ISOCD = ZTINS-WAERS.
   ELSE.
      IF L_TCURC-ISOCD IS INITIAL.
         L_TCURC-ISOCD = ZTINS-WAERS.
      ENDIF.
   ENDIF.

   MOVE L_TCURC-ISOCD       TO INV_VAL_CURR.  ">화물가액 화폐코드.

*>> 화물가액.
   WRITE : ZTINS-ZFIVAMT    TO L_INV_VAL CURRENCY ZTINS-WAERS.
   PERFORM    P2000_WRITE_NO_MASK      CHANGING  L_INV_VAL.
   INV_VAL = L_INV_VAL.

*> 차액으로 넘기기..
*   IF ZTINS-ZFAMDNO IS INITIAL.
*      L_ZFIVAMT = ZTINS-ZFIVAMT.
*   ELSE.
*      L_ZFIVAMT = ZTINS-ZFIVAMT - OLD_ZTINS-ZFIVAMT.
*   ENDIF.
*
*   WRITE : L_ZFIVAMT    TO L_INV_VAL CURRENCY ZTINS-WAERS.
*   PERFORM    P2000_WRITE_NO_MASK     CHANGING  L_INV_VAL.


*   IF L_ZFIVAMT LE 0.
*>음수 부호 앞으로 보내기...
*      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*           CHANGING
*              VALUE   =   L_INV_VAL.
*   ENDIF.
*
*   INV_VAL = L_INV_VAL.

*>>화물 희망이익율
*   WRITE : ZTINS-ZFPEIV     TO INV_PRO_RATE.
   INV_PRO_RATE = ZTINS-ZFPEIV.
*   INV_PRO_RATE =
*   PERFORM    P2000_WRITE_NO_MASK     CHANGING  INV_PRO_RATE.


   CLEAR : DTY_VAL_CURR,    ">관세보험 가액화폐코드
           DTY_VAL,         ">관세보험 가액
           DTY_PRO_RATE,    ">관세율
           REF_CD1,         ">문서코드1
           REF_CD2,         ">문서코드2 (INVOICE NO.)
           REF_TXT1,        ">문서번호1
           REF_TXT2,        ">문서번호2
           GOOD_CD,         ">포장코드
           TOOL_NM,         ">운송용구명
           START_DATE,      ">출발일자
           HULL_TON,        ">총톤수
           HULL_BUILT_DATE, ">건조일
           HULL_YEAR,       ">선령
           HULL_NATIONAL,   ">국적
           HULL_CLASS,      ">선급
           BANK_CD,         ">질권기관
           YAK_CD1,         ">보험조건1
           YAK_CD2,         ">보험조건2
           YAK_CD3,         ">보험조건3
           YAK_CD4,         ">보험조건4
           YAK_CD5,         ">보험조건5
           YAK_CD6,         ">보험조건6
           YAK_CD7.         ">보험조건7

*-------------------------------------------------------
*> 보험조건....
* DESC : 최대 7번 반복...
*-------------------------------------------------------
   YAK_CD1 = ZTINS-ZFBSINS.

   CLEAR : W_INDEX.
   LOOP AT IT_ZSINSAGR.
      ADD 1 TO W_INDEX.
      CASE W_INDEX.
         WHEN 1.   YAK_CD2 = IT_ZSINSAGR-ZFINSCD.
         WHEN 2.   YAK_CD3 = IT_ZSINSAGR-ZFINSCD.
         WHEN 3.   YAK_CD4 = IT_ZSINSAGR-ZFINSCD.
         WHEN 4.   YAK_CD5 = IT_ZSINSAGR-ZFINSCD.
         WHEN 5.   YAK_CD6 = IT_ZSINSAGR-ZFINSCD.
         WHEN 6.   YAK_CD7 = IT_ZSINSAGR-ZFINSCD.
      ENDCASE.
      IF W_INDEX GE 6.
         EXIT.
      ENDIF.
   ENDLOOP.

   IF NOT ZTINS-ZFREDON2 IS INITIAL.
      MOVE :  '01'            TO REF_CD1,    ">문서코드2
              ZTINS-ZFREDON2  TO REF_TXT1.   ">문서번호1(LC NO)
   ENDIF.
*>

   MOVE :  '07'            TO REF_CD3,        ">문서코드3
           REF_NO          TO REF_TXT3,       ">문서번호3
           ZTINS-ZFNUOD    TO PRT_ORG_CNT,    ">정본수
           ZTINS-ZFNUCD    TO PRT_COPY_CNT,   ">사본수
*>> 향후 결정됨.
*          '이름/TEL'      TO VAN_USER,       ">담당자

           '00'            TO PROC_STATUS.    ">상태구분

   CALL FUNCTION 'ZIM_DATE_CONVERT_EXTERNAL'
        EXPORTING
            I_DATE     =      SY-DATUM     ">입력일자.
        IMPORTING
            E_DATE     =      INPUT_DATE
        EXCEPTIONS
            OTHERS     =      4.
   IF SY-SUBRC NE 0.
      EXIT.
   ENDIF.

*>> 목적물 코드.
   MOVE SPACE TO OBJT_CD.

*----------------------------------------------------------------------
*> Native SQL LG 화재 테이블(TMV10)
*>   DESC : DB Link 후 Synonym Create 후 작업함.
*----------------------------------------------------------------------
*        CNTR_NM,       REL_ID,        REL_NM,          OBJT_CD,
  EXEC SQL.
      INSERT  INTO ZLGITMV10
      ( NETWORK_GB,    SAUPJA_NO,     BOJONG,          REF_NO,
        REF_CHNG_NO,   CHUNG_DATE,    CONT_GB,         CHNG_GB_CD,
        POL_NO,        CHUNG_NO,      OP_NO,           CNTR_ID,
        CNTR_NM,       REL_ID,        REL_NM,          OBJT_CD,
        OBJT_TEXT,     HS_CD,         ST_AREA_CD,      ST_AREA_TXT,
        TRAN_AREA_CD,  TRAN_AREA_TXT, ARR_AREA_CD,     ARR_AREA_TXT,
        LAST_AREA_CD,  LAST_AREA_TXT, CLM_AGENT,       SVY_AGENT,
        INV_VAL_CURR,  INV_VAL,       INV_PRO_RATE,    INV_AMT_CURR,
        INV_AMT,       DTY_VAL_CURR,  DTY_VAL,         DTY_PRO_RATE,
        REF_CD1,       REF_CD2,       REF_CD3,         REF_TXT1,
        REF_TXT2,      REF_TXT3,      GOOD_CD,         TOOL_NM,
                       HULL_TON,                       HULL_YEAR,
        HULL_NATIONAL, HULL_CLASS,    BANK_CD,         YAK_CD1,
        YAK_CD2,       YAK_CD3,       YAK_CD4,         YAK_CD5,
        YAK_CD6,       YAK_CD7,       PRT_ORG_CNT,     PRT_COPY_CNT,
                       PRT_AREA_CD,   VAN_USER,
        PROC_STATUS,   ERR_CODE,      PROC_USER_ID )
      VALUES
      ( :NETWORK_GB,    :SAUPJA_NO,     :BOJONG,          :REF_NO,
        :REF_CHNG_NO,   :CHUNG_DATE,    :CONT_GB,         :CHNG_GB_CD,
        :POL_NO,        :CHUNG_NO,      :OP_NO,           :CNTR_ID,
        :CNTR_NM,       :REL_ID,        :REL_NM,          :OBJT_CD,
        :OBJT_TEXT,     :HS_CD,         :ST_AREA_CD,      :ST_AREA_TXT,
        :TRAN_AREA_CD,  :TRAN_AREA_TXT, :ARR_AREA_CD,     :ARR_AREA_TXT,
        :LAST_AREA_CD,  :LAST_AREA_TXT, :CLM_AGENT,       :SVY_AGENT,
        :INV_VAL_CURR,  :INV_VAL,       :INV_PRO_RATE,    :INV_AMT_CURR,
        :INV_AMT,       :DTY_VAL_CURR,  :DTY_VAL,         :DTY_PRO_RATE,
        :REF_CD1,       :REF_CD2,       :REF_CD3,         :REF_TXT1,
        :REF_TXT2,      :REF_TXT3,      :GOOD_CD,         :TOOL_NM,
                        :HULL_TON,                        :HULL_YEAR,
        :HULL_NATIONAL, :HULL_CLASS,    :BANK_CD,         :YAK_CD1,
        :YAK_CD2,       :YAK_CD3,       :YAK_CD4,         :YAK_CD5,
        :YAK_CD6,       :YAK_CD7,       :PRT_ORG_CNT,     :PRT_COPY_CNT,
                        :PRT_AREA_CD,   :VAN_USER,
        :PROC_STATUS,   :ERR_CODE,      :PROC_USER_ID )
   ENDEXEC.

*      INSERT  INTO ZLGITMV10
*      ( NETWORK_GB,    SAUPJA_NO,     BOJONG,          REF_NO,
*        REF_CHNG_NO,   CHUNG_DATE,    CONT_GB,         CHNG_GB_CD,
*        POL_NO,        CHUNG_NO,      OP_NO,           CNTR_ID,
*        CNTR_NM,       REL_ID,        REL_NM,          OBJT_CD,
*        OBJT_TEXT,     HS_CD,         ST_AREA_CD,      ST_AREA_TXT,
*        TRAN_AREA_CD,  TRAN_AREA_TXT, ARR_AREA_CD,     ARR_AREA_TXT,
*        LAST_AREA_CD,  LAST_AREA_TXT, CLM_AGENT,       SVY_AGENT,
*        INV_VAL_CURR,  INV_VAL,       INV_PRO_RATE,    INV_AMT_CURR,
*        INV_AMT,       DTY_VAL_CURR,  DTY_VAL,         DTY_PRO_RATE,
*        REF_CD1,       REF_CD2,       REF_CD3,         REF_TXT1,
*        REF_TXT2,      REF_TXT3,      GOOD_CD,         TOOL_NM,
*                       HULL_TON,                       HULL_YEAR,
*        HULL_NATIONAL, HULL_CLASS,    BANK_CD,         YAK_CD1,
*        YAK_CD2,       YAK_CD3,       YAK_CD4,         YAK_CD5,
*        YAK_CD6,       YAK_CD7,       PRT_ORG_CNT,     PRT_COPY_CNT,
*                       PRT_AREA_CD,   VAN_USER,        INPUT_DATE,
*        PROC_STATUS,   ERR_CODE,      PROC_USER_ID )
*      VALUES
*      ( :NETWORK_GB,    :SAUPJA_NO,     :BOJONG,          :REF_NO,
*        :REF_CHNG_NO,   :CHUNG_DATE,    :CONT_GB,         :CHNG_GB_CD,
*        :POL_NO,        :CHUNG_NO,      :OP_NO,           :CNTR_ID,
*        :CNTR_NM,       :REL_ID,        :REL_NM,          :OBJT_CD,
*        :OBJT_TEXT,     :HS_CD,         :ST_AREA_CD,      :ST_AREA_TXT,
*        :TRAN_AREA_CD,  :TRAN_AREA_TXT, :ARR_AREA_CD,     :ARR_AREA_TXT
*
*        :LAST_AREA_CD,  :LAST_AREA_TXT, :CLM_AGENT,       :SVY_AGENT,
*        :INV_VAL_CURR,  :INV_VAL,       :INV_PRO_RATE,    :INV_AMT_CURR
*
*        :INV_AMT,       :DTY_VAL_CURR,  :DTY_VAL,         :DTY_PRO_RATE
*
*        :REF_CD1,       :REF_CD2,       :REF_CD3,         :REF_TXT1,
*        :REF_TXT2,      :REF_TXT3,      :GOOD_CD,         :TOOL_NM,
*                        :HULL_TON,                        :HULL_YEAR,
*        :HULL_NATIONAL, :HULL_CLASS,    :BANK_CD,         :YAK_CD1,
*        :YAK_CD2,       :YAK_CD3,       :YAK_CD4,         :YAK_CD5,
*        :YAK_CD6,       :YAK_CD7,       :PRT_ORG_CNT,     :PRT_COPY_CNT
*
*                        :PRT_AREA_CD,   :VAN_USER,        :INPUT_DATE,
*        :PROC_STATUS,   :ERR_CODE,      :PROC_USER_ID )


*>> UPDATE SUCCESS시...
   IF SY-SUBRC EQ 0.
      CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_INS_BEFORE'
                 EXPORTING
                      W_ZFREQNO     =     ZFREQNO
                      W_ZFINSEQ     =     ZFINSEQ
                      W_ZFAMDNO     =     ZFAMDNO
                      W_ZFDOCST     =     'R'
                      W_ZFEDIST     =     'S'.
   ELSE.
*>> 오류시..
      RAISE SEND_ERROR.
   ENDIF.

ENDFUNCTION.
