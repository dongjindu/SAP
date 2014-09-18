FUNCTION ZIM_KHNP_CUSDEC_HS_EDI_DOC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFBLNO) LIKE  ZTIDS-ZFBLNO
*"     VALUE(W_ZFCLSEQ) LIKE  ZTIDS-ZFCLSEQ
*"     VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"  EXPORTING
*"     REFERENCE(W_EDI_RECORD)
*"  TABLES
*"      REC_HS STRUCTURE  ZSRECHS
*"  EXCEPTIONS
*"      CREATE_ERROR
*"----------------------------------------------------------------------
DATA : L_TEXT         LIKE     DD07T-DDTEXT,
       L_TEXT280(280) TYPE     C,
       L_TEXT350(350) TYPE     C,
       L_BOOLEAN      TYPE     C,
       L_TYPE(002)    TYPE     C,
       L_TYPE_TEMP    LIKE     L_TYPE,
       L_CODE(003)    TYPE     C,
       W_LIN_CNT      TYPE     I,
       W_DET_CNT      TYPE     I,
       W_HS_CNT       TYPE     I.

DATA : W_ZERO3(03)      TYPE     C  VALUE '000',
       W_ZERO6(06)      TYPE     C  VALUE '000000',
       W_ZERO8(08)      TYPE     C  VALUE '00000000',
       W_ZERO9(09)      TYPE     C  VALUE '000000000',
       W_ZERO10(10)     TYPE     C  VALUE '0000000000',
       W_ZERO11(11)     TYPE     C  VALUE '00000000000',
       W_ZERO12(12)     TYPE     C  VALUE '000000000000',
       W_ZERO13(13)     TYPE     C  VALUE '0000000000000',
       W_ZERO14(14)     TYPE     C  VALUE '00000000000000',
       W_ZERO15(15)     TYPE     C  VALUE '000000000000000'.
REFRESH : REC_HS.

*DATA : BEGIN OF REC_HS,
*       REC_001(015)   TYPE     C,          " 신고번호.
*       REC_002(003)   TYPE     C,          " 란번호.
*       REC_003(010)   TYPE     C,          " 세번부호.
*       REC_004(002)   TYPE     C,          " 중량단위.
*       REC_005(014)   TYPE     C,          " 순중량.
*       REC_006(002)   TYPE     C,          " 수량단위.
*       REC_007(010)   TYPE     C,          " 수량.
*       REC_008(007)   TYPE     C,          " 원산지약.
*       REC_009(002)   TYPE     C,          " 원산지 CD.
*       REC_010(013)   TYPE     C,          " 신고액 달러.
*       REC_011(013)   TYPE     C,          " 신고액 원화.
*       REC_012(050)   TYPE     C,          " 표준품명.
*       REC_013(011)   TYPE     C,          " 괸세액.
*       REC_014(011)   TYPE     C,          " 내국세.
*       REC_015(011)   TYPE     C,          " 교육세.
*       REC_016(011)   TYPE     C,          " 공급가액.
*       REC_017(011)   TYPE     C,          " 부가세.
*       REC_018(011)   TYPE     C,          " 보험료.
*       REC_019(011)   TYPE     C,          " 운임.
*       REC_020(002)   TYPE     C,          " 세율구분.
*       REC_021(006)   TYPE     C,          " 관세종가.
*       REC_022(015)   TYPE     C,          " 결제금액.
*       REC_023(006)   TYPE     C,          " 내국종가.
*       REC_024(020)   TYPE     C,          " KEY.
*       REC_025(011)   TYPE     C,          " 농특세.
*       REC_026(011)   TYPE     C,          " 주세.
*       REC_027(050)   TYPE     C,          " 거래품명.
*       REC_028(004)   TYPE     C,          " 상품코드.
*       REC_029(050)   TYPE     C,          " 상표명.
*       REC_030(003)   TYPE     C,          " 규격수.
*       REC_031(001)   TYPE     C,          " 첨부여부.
*       REC_032(002)   TYPE     C,          " 환급단위.
*       REC_033(010)   TYPE     C,          " 환급수량.
*       REC_034(001)   TYPE     C,          " 관세계산방법.
*       REC_035(001)   TYPE     C,          " 관세감면구분.
*       REC_036(010)   TYPE     C,          " 관세분납구분.
*       REC_037(012)   TYPE     C,          " 관세감면부.
*       REC_038(007)   TYPE     C,          " 관세감면율.
*       REC_039(002)   TYPE     C,          " 내국구분.
*       REC_040(007)   TYPE     C,          " 특면세부.
*       REC_041(006)   TYPE     C,          " 내종세부.
*       REC_042(001)   TYPE     C,          " 부가구분.
*       REC_043(007)   TYPE     C,          " 부가감면부.
*       REC_044(007)   TYPE     C,          " 부감면율.
*       REC_045(001)   TYPE     C,          " 교육구분.
*       REC_046(001)   TYPE     C,          " 농특구분.
*       REC_047(001)   TYPE     C,          " 가산구분.
*       REC_048(006)   TYPE     C,          " 가산율.
*       REC_049(003)   TYPE     C,          " 가산통화.
*       REC_050(009)   TYPE     C,          " 가산환율.
*       REC_051(015)   TYPE     C,          " 가산금액.
*       REC_052(015)   TYPE     C,          " 가산금(원).
*       REC_053(001)   TYPE     C,          " 공제구분.
*       REC_054(006)   TYPE     C,          " 공제율.
*       REC_055(003)   TYPE     C,          " 공제통화.
*       REC_056(009)   TYPE     C,          " 공제환율.
*       REC_057(015)   TYPE     C,          " 공제금액.
*       REC_058(001)   TYPE     C,          " 원산유무.
*       REC_059(001)   TYPE     C,          " 원산방법.
*       REC_060(001)   TYPE     C,          " 원산형태.
*       REC_061(001)   TYPE     C,          " 관세거래.
*       REC_062(001)   TYPE     C,          " 관세품목.
*       REC_063(001)   TYPE     C,          " 관세검사.
*       REC_064(050)   TYPE     C,          " 관세기재.
*       REC_065(050)   TYPE     C,          " 관세기재.
*       REC_066(050)   TYPE     C,          " 관세기재.
*       REC_067(050)   TYPE     C,          " 관세기재.
*       REC_068(031)   TYPE     C,          " 세관기재.
*       REC_069(031)   TYPE     C,          " 세관기재.
*       REC_070(031)   TYPE     C,          " 세관기재.
*       REC_071(031)   TYPE     C,          " 세관기재.
*       REC_072(031)   TYPE     C,          " 세관기재.
*       REC_073(031)   TYPE     C,          " 세관기재.
*       REC_074(031)   TYPE     C,          " 세관기재.
*       REC_075(031)   TYPE     C,          " 세관기재.
*       REC_076(031)   TYPE     C,          " 세관기재.
*       REC_077(031)   TYPE     C,          " 세관기재.
*       REC_078(010)   TYPE     C,          " 관분납회.
*       REC_079(011)   TYPE     C,          " 관분당금.
*       REC_080(010)   TYPE     C,          " 관총분수.
*       REC_081(001)   TYPE     C,          " 특송 C/S.
*       REC_082(001)   TYPE     C,          " C/S 검사.
*       REC_083(001)   TYPE     C.          " 검사변경.
**       CR_LF          TYPE     X        VALUE '0A'.
*DATA : END   OF REC_HS.

*> CUSTOM CLEARANCE DATA GET
   CALL FUNCTION 'ZIM_GET_IDR_DOCUMENT'
        EXPORTING
              ZFBLNO             =       W_ZFBLNO
              ZFCLSEQ            =       W_ZFCLSEQ
        IMPORTING
              W_ZTIDR            =       ZTIDR
        TABLES
              IT_ZSIDRHS         =       IT_ZSIDRHS
              IT_ZSIDRHS_ORG     =       IT_ZSIDRHS_ORG
              IT_ZSIDRHSD        =       IT_ZSIDRHSD
              IT_ZSIDRHSD_ORG    =       IT_ZSIDRHSD_ORG
              IT_ZSIDRHSL        =       IT_ZSIDRHSL
              IT_ZSIDRHSL_ORG    =       IT_ZSIDRHSL_ORG
        EXCEPTIONS
              NOT_FOUND     =       4
              NOT_INPUT     =       8.

   CASE SY-SUBRC.
      WHEN 4.
         MESSAGE E018 WITH W_ZFREQNO RAISING  CREATE_ERROR.
      WHEN 8.
         MESSAGE E019 RAISING  CREATE_ERROR.
   ENDCASE.

*>> BL 자료 GET!
   CLEAR ZTBL.
   SELECT SINGLE * FROM ZTBL      WHERE ZFBLNO EQ W_ZFBLNO.

*-----------------------------------------------------------------------
* FLAT-FILE RECORD CREATE
*-----------------------------------------------------------------------
   LOOP  AT  IT_ZSIDRHS.
      CLEAR : REC_HS, W_EDI_RECORD.

      CONCATENATE  ZTIDR-ZFBLNO  ZTIDR-ZFCLSEQ INTO  REC_HS-REC_024.
      WRITE  IT_ZSIDRHS-ZFCONO  TO REC_HS-REC_002.

      MOVE : IT_ZSIDRHS-STAWN    TO  REC_HS-REC_003,   " 세번부호.
             SPACE               TO  REC_HS-REC_004,   " 중량단위.
             '000000000000.0'    TO  REC_HS-REC_005,   " 중량.
             SPACE               TO  REC_HS-REC_006,   " 수량단위.
             '0000000000'        TO  REC_HS-REC_007,   " 수량.
             SPACE               TO  REC_HS-REC_008,   " 원산지 약어.
             IT_ZSIDRHS-ZFORIG   TO  REC_HS-REC_009,   " 원산지 CD.
             '0000000000000'     TO  REC_HS-REC_010,   " 신고액 $
             '0000000000000'     TO  REC_HS-REC_011,   " 신고액 \
             IT_ZSIDRHS-ZFGDNM   TO  REC_HS-REC_012,   " 품명.
             '00000000000'       TO  REC_HS-REC_013,   " 관세액.
             '00000000000'       TO  REC_HS-REC_014,   " 내국세.
             '00000000000'       TO  REC_HS-REC_015,   " 교육세.
             '00000000000'       TO  REC_HS-REC_016,   " 공급가액.
             '00000000000'       TO  REC_HS-REC_017,   " 부가세.
             '00000000000'       TO  REC_HS-REC_018,   " 보험료.
             '00000000000'       TO  REC_HS-REC_019,   " 운임.
             IT_ZSIDRHS-ZFTXGB   TO  REC_HS-REC_020,   " 세율구분.
             '000.00'            TO  REC_HS-REC_021,   " 관세종가.
             '000000000000.00'   TO  REC_HS-REC_022,   " 결제금액.
             '000.00'            TO  REC_HS-REC_023,   " 내국종가.
             '00000000000'       TO  REC_HS-REC_025,   " 농특세.
             '00000000000'       TO  REC_HS-REC_026,   " 주세.
             IT_ZSIDRHS-ZFTGDNM  TO  REC_HS-REC_027,   " 거래품명.
             IT_ZSIDRHS-ZFGCCD   TO  REC_HS-REC_028,   " 상표코드.
             IT_ZSIDRHS-ZFGCNM   TO  REC_HS-REC_029,   " 상표코드명.
             '000'               TO  REC_HS-REC_030,   " 규격수.
             IT_ZSIDRHS-ZFATTYN  TO  REC_HS-REC_031,   " 첨부서류여부.
             SPACE               TO  REC_HS-REC_032,   " 환급단위.
             '0000000000'        TO  REC_HS-REC_033,   " 환급수량.
             IT_ZSIDRHS-ZFTXMT   TO  REC_HS-REC_034,   " 관세계산방법.
             IT_ZSIDRHS-ZFCDPCD  TO  REC_HS-REC_035,   " 관세감면구분.
             IT_ZSIDRHS-ZFCUDIV  TO  REC_HS-REC_036,   " 관세분납부호.
             IT_ZSIDRHS-ZFCDPNO  TO  REC_HS-REC_037,   " 관세감면부호.
             '000.000'           TO  REC_HS-REC_038,   " 관세감면율.
             IT_ZSIDRHS-ZFHMTCD  TO  REC_HS-REC_039,   " 내국세구분.
             SPACE               TO  REC_HS-REC_040,   " 특면세부호.
             SPACE               TO  REC_HS-REC_041,   " 내국세종부.
             IT_ZSIDRHS-ZFVTXCD  TO  REC_HS-REC_042,   " 부가세구분.
             SPACE               TO  REC_HS-REC_043,   " 부가세감면부호.
             '000.000'           TO  REC_HS-REC_044,   " 부가세감면율.
             SPACE               TO  REC_HS-REC_045,   " 교육구분.
             SPACE               TO  REC_HS-REC_046,   " 농특구분.
             SPACE               TO  REC_HS-REC_047,   " 가산구분.
             '000.00'            TO  REC_HS-REC_048,   " 가산율.
             SPACE               TO  REC_HS-REC_049,   " 가산통화.
             '0000.0000'         TO  REC_HS-REC_050,   " 가산환율.
             '000000000000.00'   TO  REC_HS-REC_051,   " 가산금액.
             '000000000000000'   TO  REC_HS-REC_052,   " 가산금액원화.
             SPACE               TO  REC_HS-REC_053,   " 공제구분.
             '000.00'            TO  REC_HS-REC_054,   " 공제율.
             SPACE               TO  REC_HS-REC_055,   " 공제통화.
             '0000.0000'         TO  REC_HS-REC_056,   " 공제환율.
             '000000000000.00'   TO  REC_HS-REC_057,   " 공제금액.
             IT_ZSIDRHS-ZFORYN   TO  REC_HS-REC_058,   " 원산유무.
             IT_ZSIDRHS-ZFORME   TO  REC_HS-REC_059,   " 원산방법.
             IT_ZSIDRHS-ZFORTY   TO  REC_HS-REC_060,   " 원산형태.
             IT_ZSIDRHS-ZFTRRL   TO  REC_HS-REC_061,   " 관세거래.
             IT_ZSIDRHS-ZFGDAL   TO  REC_HS-REC_062,   " 관세품목.
             IT_ZSIDRHS-ZFEXOP   TO  REC_HS-REC_063,   " 관세검사.
             IT_ZSIDRHS-ZFCTW1   TO  REC_HS-REC_064,   " 관세사기재1.
             IT_ZSIDRHS-ZFCTW2   TO  REC_HS-REC_065,   " 관세사기재2.
             IT_ZSIDRHS-ZFCTW3   TO  REC_HS-REC_066,   " 관세사기재3.
             IT_ZSIDRHS-ZFCTW4   TO  REC_HS-REC_067,   " 관세사기재4.
             SPACE               TO  REC_HS-REC_068,   " 세관기재1.
             SPACE               TO  REC_HS-REC_069,   " 세관기재2.
             SPACE               TO  REC_HS-REC_070,   " 세관기재3.
             SPACE               TO  REC_HS-REC_071,   " 세관기재4.
             SPACE               TO  REC_HS-REC_072,   " 세관기재5.
             SPACE               TO  REC_HS-REC_073,   " 세관기재6.
             SPACE               TO  REC_HS-REC_074,   " 세관기재7.
             SPACE               TO  REC_HS-REC_075,   " 세관기재8.
             SPACE               TO  REC_HS-REC_076,   " 세관기재9.
             SPACE               TO  REC_HS-REC_077,   " 세관기재10.
             '0000000000'        TO  REC_HS-REC_078,   " 관분납회.
             '00000000000'       TO  REC_HS-REC_079,   " 관세분당금.
             '0000000000'        TO  REC_HS-REC_080,   " 관세총분납수.
             IT_ZSIDRHS-ZFSTCS   TO  REC_HS-REC_081,   " 특송 C/S.
             IT_ZSIDRHS-ZFCSGB   TO  REC_HS-REC_082,   " C/S 구분.
             IT_ZSIDRHS-ZFCSCH   TO  REC_HS-REC_083.   " 변경구분.

      APPEND  REC_HS.
   ENDLOOP.

ENDFUNCTION.
