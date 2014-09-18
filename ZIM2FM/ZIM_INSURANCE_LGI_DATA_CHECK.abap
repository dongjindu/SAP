FUNCTION ZIM_INSURANCE_LGI_DATA_CHECK.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFREQNO) LIKE  ZTINS-ZFREQNO
*"     VALUE(ZFINSEQ) LIKE  ZTINS-ZFINSEQ
*"     VALUE(ZFAMDNO) LIKE  ZTINS-ZFAMDNO
*"  EXCEPTIONS
*"      NOT_FOUND
*"      NOT_SELECT
*"----------------------------------------------------------------------
DATA : BUFFER_POINTER     TYPE     I.
DATA : PARA_LENG          TYPE     I.
DATA : W_INDEX            TYPE     I.
DATA : W_MOD              TYPE     I.
DATA : L_YAK(005)         TYPE     C.
DATA : W_AMEND_MARK.

DATA : L_TCURC            LIKE TCURC.

DATA : BEGIN OF WA,
       NETWORK_GB(002)    TYPE C,   ">사용구분(VAN '01', CERTI '02')
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
       INV_VAL            TYPE F,   ">화물가액
       INV_PRO_RATE       TYPE F,   ">화물 희망이익율
       INV_AMT_CURR(003)  TYPE C,   ">화물가입 화폐코드
       INV_AMT            TYPE F,    ">화물가입금액
       DTY_VAL_CURR(003)  TYPE C,    ">관세보험 가액화폐코드
       DTY_VAL            TYPE F,    ">관세보험 가액
       DTY_PRO_RATE       TYPE F,    ">관세율
       REF_CD1(002)       TYPE C,   ">문서코드1
       REF_CD2(002)       TYPE C,   ">문서코드2
       REF_CD3(002)       TYPE C,   ">문서코드3
       REF_TXT1(020)      TYPE C,   ">문서번호1
       REF_TXT2(020)      TYPE C,   ">문서번호2
       REF_TXT3(025)      TYPE C,   ">문서번호3
       GOOD_CD(002)       TYPE C,   ">포장코드
       TOOL_NM(030)       TYPE C,   ">운송용구명
       START_DATE(009)    TYPE C,   ">출발일자
       HULL_TON           TYPE F,   ">총톤수
       HULL_BUILT_DATE    TYPE C,   ">건조일
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
       PRT_ORG_CNT        TYPE F,   ">정본수
       PRT_COPY_CNT       TYPE F,   ">사본수
       PRT_BAL_DATE(9)    TYPE C,   ">증권발급일자
       PRT_AREA_CD(004)   TYPE C,   ">증권발급지
       VAN_USER(030)      TYPE C,   ">담당자
       INPUT_DATE(9)      TYPE C,   ">입력일자
       PROC_STATUS(002)   TYPE C,   ">상태구분
       ERR_CODE(002)      TYPE C,   ">ERROR_CODE
       PROC_USER_ID(008)  TYPE C.   ">처리 LG화재 담당자
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
   MOVE : '01'          TO  WA-NETWORK_GB,        ">사용구분(VAN '01')
          ZTINS-ZFELTXN TO  WA-SAUPJA_NO,         ">대표사업자번호
          'I'           TO  WA-BOJONG,            ">수출입구분
          ZTINS-ZFREQNO TO  WA-REF_NO(10),        ">문서관리번호(PK)
          ZTINS-ZFINSEQ TO  WA-REF_NO+10(5),      ">문서관리번호(PK)
          ZTINS-ZFAMDNO+3(2) TO WA-REF_CHNG_NO.   ">문서관리번호 차수
**

*----------------------------------------------------------------------
*> Native SQL LG 화재 테이블(TMV20)
*>   DESC : DB Link 후 Synonym Create 후 작업함.
*----------------------------------------------------------------------
  EXEC SQL.
      SELECT *
         INTO :WA
         FROM ZLGITMV10
         WHERE NETWORK_GB     =    :WA-NETWORK_GB
         AND   SAUPJA_NO      =    :WA-SAUPJA_NO
         AND   BOJONG         =    :WA-BOJONG
         AND   REF_NO         =    :WA-REF_NO
         AND   REF_CHNG_NO    =    :WA-REF_CHNG_NO
   ENDEXEC.

   IF SY-SUBRC NE 0.
      RAISE NOT_SELECT.
   ENDIF.


ENDFUNCTION.
