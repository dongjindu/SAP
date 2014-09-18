FUNCTION ZIM_KHNP_CUSDEC_EDI_DOC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFBLNO) LIKE  ZTIDS-ZFBLNO
*"     VALUE(W_ZFCLSEQ) LIKE  ZTIDS-ZFCLSEQ
*"     VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"  EXPORTING
*"     REFERENCE(W_EDI_RECORD)
*"  EXCEPTIONS
*"      CREATE_ERROR
*"----------------------------------------------------------------------
DATA : L_TEXT           LIKE     DD07T-DDTEXT,
       L_TEXT280(280)   TYPE     C,
       L_TEXT350(350)   TYPE     C,
       L_BOOLEAN        TYPE     C,
       L_TYPE(002)      TYPE     C,
       L_TYPE_TEMP      LIKE     L_TYPE,
       L_CODE(003)      TYPE     C,
       W_LIN_CNT        TYPE     I,
       W_DET_CNT        TYPE     I,
       W_HS_CNT         TYPE     I,
       W_TEXT_AMT14(14) TYPE     C,
       W_TEXT_AMT13(13) TYPE     C,
       W_TEXT_AMT15(15) TYPE     C,
       W_TEXT_AMT12(12) TYPE     C,
       W_TEXT_AMT11(11) TYPE     C,
       W_TEXT_AMT10(10) TYPE     C,
       W_TEXT_AMT09(09) TYPE     C,
       W_TEXT_AMT08(08) TYPE     C,
       W_TEXT_AMT06(06) TYPE     C,
       W_TEXT_AMT03(03) TYPE     C.

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

DATA : BEGIN OF REC_CUSDEC,
       REC_001(015)   TYPE     C,          " 신고번호.
       REC_002(001)   TYPE     C,          " 신고구분.
       REC_003(008)   TYPE     C,          " 신고일자.
       REC_004(028)   TYPE     C,          " 수입상호.
       REC_005(008)   TYPE     C,          " 수입부호.
       REC_006(028)   TYPE     C,          " 납세상호.
       REC_007(012)   TYPE     C,          " 납세대표.
       REC_008(015)   TYPE     C,          " 납세통관.
       REC_009(040)   TYPE     C,          " 납세주소.
       REC_010(010)   TYPE     C,          " 납세사업자등록.
       REC_011(026)   TYPE     C,          " 공급자상호.
       REC_012(010)   TYPE     C,          " 공급자 CD.
       REC_013(003)   TYPE     C,          " 세관.
       REC_014(002)   TYPE     C,          " 과.
       REC_015(002)   TYPE     C,          " 거래구분.
       REC_016(002)   TYPE     C,          " 수입종류.
       REC_017(008)   TYPE     C,          " 입항일자.
       REC_018(003)   TYPE     C,          " 도착항CD.
       REC_019(008)   TYPE     C,          " 반입장 CD.
       REC_020(006)   TYPE     C,          " 장치위치.
       REC_021(008)   TYPE     C,          " 입고일자.
       REC_022(002)   TYPE     C,          " 징수형태.
       REC_023(020)   TYPE     C,          " 선기명.
       REC_024(002)   TYPE     C,          " 운송형태.
       REC_025(003)   TYPE     C,          " 운송용기.
       REC_026(020)   TYPE     C,          " HOUSE B/L.
       REC_027(020)   TYPE     C,          " 화물번호.
       REC_028(020)   TYPE     C,          " KEY FIELD.
       REC_029(003)   TYPE     C,          " 인도조건.
       REC_030(003)   TYPE     C,          " 결제통화.
       REC_031(014)   TYPE     C,          " 결제금액.
       REC_032(003)   TYPE     C,          " 운임통화.
       REC_033(013)   TYPE     C,          " 운임금액.
       REC_034(003)   TYPE     C,          " 보험통화.
       REC_035(013)   TYPE     C,          " 보험금액.
       REC_036(002)   TYPE     C,          " 포장단위.
       REC_037(008)   TYPE     C,          " 포장갯수.
       REC_038(014)   TYPE     C,          " 중량.
       REC_039(003)   TYPE     C,          " 총란수.
       REC_040(012)   TYPE     C,          " 감정가격.
       REC_041(010)   TYPE     C,          " 신고가 $.
       REC_042(015)   TYPE     C,          " 납부번호.
       REC_043(008)   TYPE     C,          " 수리일자.
       REC_044(020)   TYPE     C,          " LC NO.
       REC_045(020)   TYPE     C,          " MASTER B/L.
       REC_046(028)   TYPE     C,          " 무역상호.
       REC_047(007)   TYPE     C,          " 무역CD.
       REC_048(004)   TYPE     C,          " 수입자 CD.
       REC_049(001)   TYPE     C,          " 수입자 KD.
       REC_050(004)   TYPE     C,          " 납세자 CD.
       REC_051(003)   TYPE     C,          " 납세자 우편.
       REC_052(001)   TYPE     C,          " 통관계획.
       REC_053(030)   TYPE     C,          " 장치장명.
       REC_054(002)   TYPE     C,          " 결재방법.
       REC_055(002)   TYPE     C,          " 공급자국.
       REC_056(013)   TYPE     C,          " 도착항명.
       REC_057(002)   TYPE     C,          " 선기국 CD.
       REC_058(012)   TYPE     C,          " 선기국명.
       REC_059(002)   TYPE     C,          " 적출국 CD.
       REC_060(010)   TYPE     C,          " 적출국명.
       REC_061(001)   TYPE     C,          " 원산지 YN.
       REC_062(004)   TYPE     C,          " 운수 CD.
       REC_063(020)   TYPE     C,          " 운수명.
       REC_064(002)   TYPE     C,          " 특송 CD.
       REC_065(020)   TYPE     C,          " 특송명.
       REC_066(009)   TYPE     C,          " 달러환율.
       REC_067(009)   TYPE     C,          " 적용환율.
       REC_068(001)   TYPE     C,          " 가산구분.
       REC_069(006)   TYPE     C,          " 가산율.
       REC_070(003)   TYPE     C,          " 가산통화.
       REC_071(009)   TYPE     C,          " 가산환율.
       REC_072(015)   TYPE     C,          " 가산금액.
       REC_073(015)   TYPE     C,          " 가산원화.
       REC_074(001)   TYPE     C,          " 공제구분.
       REC_075(006)   TYPE     C,          " 공제율.
       REC_076(003)   TYPE     C,          " 공제통화.
       REC_077(009)   TYPE     C,          " 공제환율.
       REC_078(015)   TYPE     C,          " 공제금액.
       REC_079(015)   TYPE     C,          " 공제원화.
       REC_080(012)   TYPE     C,          " 신고액 원화.
       REC_081(011)   TYPE     C,          " 관세.
       REC_082(011)   TYPE     C,          " 특소세.
       REC_083(011)   TYPE     C,          " 교육세.
       REC_084(011)   TYPE     C,          " 부가세.
       REC_085(011)   TYPE     C,          " 농특세.
       REC_086(011)   TYPE     C,          " 주세.
       REC_087(012)   TYPE     C,          " 담당자.
       REC_088(012)   TYPE     C,          " 접수일자.
       REC_089(008)   TYPE     C.          " 납기일자.
DATA : END   OF REC_CUSDEC.

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
   CLEAR : REC_CUSDEC, W_EDI_RECORD.

   CONCATENATE  ZTIDR-ZFTDAD1  ZTIDR-ZFTDAD2  INTO  REC_CUSDEC-REC_009.
   CONCATENATE  ZTIDR-ZFBLNO   ZTIDR-ZFCLSEQ  INTO  REC_CUSDEC-REC_028.

   MOVE : ZTIDR-ZFIDWDT   TO  REC_CUSDEC-REC_003,   " 신고일자.
          ZTIDR-ZFIAPNM   TO  REC_CUSDEC-REC_004,   " 수입상호.
          SPACE           TO  REC_CUSDEC-REC_005,   " 수입부호.
          ZTIDR-ZFTDNM1   TO  REC_CUSDEC-REC_006,   " 납세상호.
          ZTIDR-ZFTDNM2   TO  REC_CUSDEC-REC_007,   " 납세대표.
          ZTIDR-ZFTDNO    TO  REC_CUSDEC-REC_008,   " 납세통관.
          ZTIDR-ZFTDTC    TO  REC_CUSDEC-REC_010,   " 사업자등록번호.
          ZTIDR-ZFSUPNM   TO  REC_CUSDEC-REC_011,   " 공급자 상호.
          ZTIDR-ZFSUPNO   TO  REC_CUSDEC-REC_012,   " 공급자 부호.
          ZTIDR-ZFINRC    TO  REC_CUSDEC-REC_013,   " 세관.
          ZTIDR-ZFINRCD   TO  REC_CUSDEC-REC_014,   " 과.
          ZTIDR-ZFPONC    TO  REC_CUSDEC-REC_015,   " 거래구분.
          ZTIDR-ZFITKD    TO  REC_CUSDEC-REC_016,   " 수입종류.
          ZTIDR-ZFENDT    TO  REC_CUSDEC-REC_017,   " 입항일자.
          ZTIDR-ZFAPRTC   TO  REC_CUSDEC-REC_018,   " 도착항 CD.
          ZTIDR-ZFISPL    TO  REC_CUSDEC-REC_019,   " 반입장 CD.
          ZTIDR-ZFLOCA    TO  REC_CUSDEC-REC_020,   " 장치위치.
          ZTIDR-ZFINDT    TO  REC_CUSDEC-REC_021,   " 입고일자.
          ZTIDR-ZFCOCD    TO  REC_CUSDEC-REC_022,   " 징수형태.
          ZTIDR-ZFCARNM   TO  REC_CUSDEC-REC_023,   " 선기명.
          ZTIDR-ZFTRMET   TO  REC_CUSDEC-REC_024,   " 운송형태.
          ZTIDR-ZFTRCN    TO  REC_CUSDEC-REC_025,   " 운송용기.
          ZTIDR-ZFHBLNO   TO  REC_CUSDEC-REC_026,   " HOUSE B/L.
          ZTIDR-ZFGOMNO   TO  REC_CUSDEC-REC_027,   " 화물관리번호.
          ZTIDR-INCO1     TO  REC_CUSDEC-REC_029,   " 인도조건.
          ZTIDR-ZFSTAMC   TO  REC_CUSDEC-REC_030,   " 결제통화.
          ZTIDR-ZFTFAC    TO  REC_CUSDEC-REC_032,   " 운임통화.
          'KRW'           TO  REC_CUSDEC-REC_034,   " 보험통화.
          ZTIDR-ZFPKNM    TO  REC_CUSDEC-REC_036,   " 포장단위.
          '000000000000'  TO  REC_CUSDEC-REC_040,   " 감정가격.
          SPACE           TO  REC_CUSDEC-REC_042,   " 납부번호.
          SPACE           TO  REC_CUSDEC-REC_043,   " 수리일자.
          ZTIDR-ZFOPNNO   TO  REC_CUSDEC-REC_044,   " L/C NO.
          ZTIDR-ZFMBLNO   TO  REC_CUSDEC-REC_045,   " MASTER B/L.
          ZTIDR-ZFTRDNM   TO  REC_CUSDEC-REC_046,   " 무역상호.
          ZTIDR-ZFTRDNO   TO  REC_CUSDEC-REC_047,   " 무역코드.
          ZTIDR-ZFIAPCD   TO  REC_CUSDEC-REC_048,   " 수입자 CD.
          ZTIDR-ZFIMCD    TO  REC_CUSDEC-REC_049,   " 수입자 종류.
          ZTIDR-ZFTDCD    TO  REC_CUSDEC-REC_050,   " 납세자 CD.
          SPACE           TO  REC_CUSDEC-REC_051,   " 납세우편.
          ZTIDR-ZFCUPR    TO  REC_CUSDEC-REC_052,   " 통관계획.
          ZTIDR-ZFPLNM    TO  REC_CUSDEC-REC_053,   " 장치장명.
          ZTIDR-ZFAMCD    TO  REC_CUSDEC-REC_054,   " 결제방법.
          ZTIDR-ZFSUPC    TO  REC_CUSDEC-REC_055,   " 공급자국.
          ZTIDR-ZFCAC     TO  REC_CUSDEC-REC_057,   " 선기국CD.
          ZTIDR-ZFSCON    TO  REC_CUSDEC-REC_059,   " 적출국CD.
          ZTIDR-ZFORGYN   TO  REC_CUSDEC-REC_061,   " 원산지 유무.
          ZTIDR-ZFFWORG   TO  REC_CUSDEC-REC_062,   " 운수 CODE.
          SPACE           TO  REC_CUSDEC-REC_063,   " 운수명.
          ZTIDR-ZFSTRCD   TO  REC_CUSDEC-REC_064,   " 특송 CD.
          SPACE           TO  REC_CUSDEC-REC_065,   " 특송명.
          ZTIDR-ZFADAMC   TO  REC_CUSDEC-REC_068,   " 가산구분.
          ZTIDR-ZFADAMCU  TO  REC_CUSDEC-REC_070,   " 가산통화.
          ZTIDR-ZFDUAMC   TO  REC_CUSDEC-REC_074,   " 공제구분.
          ZTIDR-ZFDUAMCU  TO  REC_CUSDEC-REC_076.   " 공제통화.

   " 국명 GET!
   " 선기국명.
   SELECT  SINGLE  LANDX  INTO  REC_CUSDEC-REC_058
   FROM    T005T
   WHERE   SPRAS   EQ   SY-LANGU
   AND     LAND1   EQ   ZTIDR-ZFCAC.

   " 적출국명.
   SELECT  SINGLE  LANDX  INTO  REC_CUSDEC-REC_060
   FROM    T005T
   WHERE   SPRAS   EQ   SY-LANGU
   AND     LAND1   EQ   ZTIDR-ZFSCON.

   " 도착항명.
   SELECT  SINGLE  DDTEXT  INTO  REC_CUSDEC-REC_056
   FROM    DD07T
   WHERE   DOMNAME     EQ  'ZEAPRTC'
   AND     DDLANGUAGE  EQ  SY-LANGU
   AND     VALPOS      EQ  ZTIDR-ZFAPRTC.

   " 금액, 율 TEXT 화.
   " 결제금액.
   IF ZTIDR-ZFSTAMT IS INITIAL.
      MOVE   '00000000000.00'   TO  REC_CUSDEC-REC_031.
   ELSE.
      WRITE      ZTIDR-ZFSTAMT  TO  W_TEXT_AMT14  DECIMALS 2.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT14.
      WRITE      W_TEXT_AMT14   TO  REC_CUSDEC-REC_031 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_031  WITH  W_ZERO14.
   ENDIF.

   " 운임금액.
   IF ZTIDR-ZFTFA  IS INITIAL.
      MOVE   '0000000000.00'   TO  REC_CUSDEC-REC_033.
   ELSE.
      WRITE      ZTIDR-ZFTFA  TO  W_TEXT_AMT13 DECIMALS 2.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT13.
      WRITE      W_TEXT_AMT13  TO  REC_CUSDEC-REC_033 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_033   WITH  W_ZERO13.
   ENDIF.

   " 보험금액.
   IF ZTIDR-ZFINAMT  IS  INITIAL.
      MOVE  '0000000000.00'   TO  REC_CUSDEC-REC_035.
   ELSE.
      WRITE      ZTIDR-ZFINAMT  TO  W_TEXT_AMT13 DECIMALS 2.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT13.
      WRITE      W_TEXT_AMT13   TO  REC_CUSDEC-REC_035 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_035   WITH  W_ZERO13.
   ENDIF.

   " 포장갯수.
   IF ZTIDR-ZFPKCNT  IS INITIAL.
      MOVE  '00000000'   TO  REC_CUSDEC-REC_037.
   ELSE.
      WRITE      ZTIDR-ZFPKCNT  TO  W_TEXT_AMT08 DECIMALS 0.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT08.
      WRITE      W_TEXT_AMT08  TO  REC_CUSDEC-REC_037 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_037   WITH  W_ZERO8.
   ENDIF.

   " 중량.
   IF ZTIDR-ZFTOWT  IS  INITIAL.
      MOVE  '000000000000.0'   TO  REC_CUSDEC-REC_038.
   ELSE.
      WRITE      ZTIDR-ZFTOWT  TO  W_TEXT_AMT14 DECIMALS 1.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT14.
      WRITE      W_TEXT_AMT14  TO  REC_CUSDEC-REC_038 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_038   WITH  W_ZERO14.
   ENDIF.

   " 총란수.
   DESCRIBE TABLE  IT_ZSIDRHS  LINES  W_LINE.
   WRITE      W_LINE        TO  W_TEXT_AMT03  DECIMALS  0.
   PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT03.
   WRITE      W_TEXT_AMT03  TO  REC_CUSDEC-REC_039 RIGHT-JUSTIFIED.
   OVERLAY    REC_CUSDEC-REC_039   WITH  W_ZERO3.

   " 신고가$.
   IF ZTIDR-ZFTBAU  IS  INITIAL.
      MOVE  '0000000000'   TO  REC_CUSDEC-REC_041.
   ELSE.
      WRITE      ZTIDR-ZFTBAU  TO  W_TEXT_AMT10 DECIMALS 0.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT10.
      WRITE      W_TEXT_AMT10  TO  REC_CUSDEC-REC_041 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_041   WITH  W_ZERO10.
   ENDIF.

   " 달러환율.
   IF  ZTIDR-ZFEXUS  IS  INITIAL.
       MOVE  '0000.0000'    TO   REC_CUSDEC-REC_066.
   ELSE.
       WRITE      ZTIDR-ZFEXUS  TO  W_TEXT_AMT09 DECIMALS 4.
       PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT09.
       WRITE      W_TEXT_AMT09  TO  REC_CUSDEC-REC_066 RIGHT-JUSTIFIED.
       OVERLAY    REC_CUSDEC-REC_066   WITH  W_ZERO9.
   ENDIF.

   " 적용환율.
   IF ZTIDR-ZFEXRT  IS  INITIAL.
      MOVE  '0000.0000'   TO  REC_CUSDEC-REC_067.
   ELSE.
      WRITE      ZTIDR-ZFEXRT  TO  W_TEXT_AMT09 DECIMALS 4.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT09.
      WRITE      W_TEXT_AMT09  TO  REC_CUSDEC-REC_067 LEFT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_067   WITH  W_ZERO9.
   ENDIF.

   " 가산율.
   IF ZTIDR-ZFADRT  IS INITIAL.
      MOVE  '000.00'  TO  REC_CUSDEC-REC_069.
   ELSE.
      WRITE      ZTIDR-ZFADRT  TO  W_TEXT_AMT06 DECIMALS 2.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT06.
      WRITE      W_TEXT_AMT06  TO  REC_CUSDEC-REC_069 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_069   WITH  W_ZERO6.
   ENDIF.

   " 가산환율.
   IF ZTIDR-ZFEXAD  IS  INITIAL.
      MOVE  '0000.0000'  TO  REC_CUSDEC-REC_071.
   ELSE.
      WRITE      ZTIDR-ZFEXAD  TO  W_TEXT_AMT09 DECIMALS 4.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT09.
      WRITE      W_TEXT_AMT09  TO  REC_CUSDEC-REC_071 LEFT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_071   WITH  W_ZERO9.
   ENDIF.

   " 가산금액.
   IF ZTIDR-ZFADAM  IS  INITIAL.
      MOVE  '000000000000.00'  TO  REC_CUSDEC-REC_072.
   ELSE.
      WRITE      ZTIDR-ZFADAM  TO  W_TEXT_AMT15 DECIMALS 2.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT15.
      WRITE      W_TEXT_AMT15  TO  REC_CUSDEC-REC_072 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_072   WITH  W_ZERO15.
   ENDIF.

   " 가산금액(원).
   IF ZTIDR-ZFADAMK IS INITIAL.
      MOVE  '000000000000000'   TO  REC_CUSDEC-REC_073.
   ELSE.
      WRITE      ZTIDR-ZFADAMK  TO  W_TEXT_AMT15 DECIMALS 0.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT15.
      WRITE      W_TEXT_AMT15  TO  REC_CUSDEC-REC_073 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_073   WITH  W_ZERO15.
   ENDIF.

   " 공제율.
   IF ZTIDR-ZFDURT IS INITIAL.
      MOVE  '000.00'  TO  REC_CUSDEC-REC_075.
   ELSE.
      WRITE      ZTIDR-ZFDURT  TO  W_TEXT_AMT06 DECIMALS 2.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT06.
      WRITE      W_TEXT_AMT06  TO  REC_CUSDEC-REC_075 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_075   WITH  W_ZERO6.
   ENDIF.

   " 공제환율.
   IF ZTIDR-ZFEXDU IS INITIAL.
      MOVE  '0000.0000'  TO  REC_CUSDEC-REC_077.
   ELSE.
      WRITE      ZTIDR-ZFEXDU  TO  W_TEXT_AMT09 DECIMALS 4.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT09.
      WRITE      W_TEXT_AMT09  TO  REC_CUSDEC-REC_077 LEFT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_077   WITH  W_ZERO9.
   ENDIF.

   " 공제금액.
   IF ZTIDR-ZFDUAM IS INITIAL.
      MOVE  '000000000000.00'  TO  REC_CUSDEC-REC_078.
   ELSE.
      WRITE      ZTIDR-ZFDUAM  TO  W_TEXT_AMT15 DECIMALS 2.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT15.
      WRITE      W_TEXT_AMT15  TO  REC_CUSDEC-REC_078 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_078   WITH  W_ZERO15.
   ENDIF.

   " 공제금액(원).
   IF ZTIDR-ZFDUAMK IS INITIAL.
      MOVE  '000000000000000'  TO  REC_CUSDEC-REC_079.
   ELSE.
      WRITE      ZTIDR-ZFDUAMK  TO  W_TEXT_AMT15 DECIMALS 0.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT15.
      WRITE      W_TEXT_AMT15  TO  REC_CUSDEC-REC_079 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_079   WITH  W_ZERO15.
   ENDIF.

   " 신고액(원).
   IF ZTIDR-ZFTBAK IS INITIAL.
      MOVE  '000000000000'  TO  REC_CUSDEC-REC_080.
   ELSE.
      WRITE      ZTIDR-ZFTBAK  TO  W_TEXT_AMT12 DECIMALS 0.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT12.
      WRITE      W_TEXT_AMT12  TO  REC_CUSDEC-REC_080 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_080   WITH  W_ZERO12.
   ENDIF.

   " 관세.
   IF ZTIDR-ZFCUAMTS IS INITIAL.
      MOVE  '00000000000'  TO  REC_CUSDEC-REC_081.
   ELSE.
      WRITE      ZTIDR-ZFCUAMTS  TO  W_TEXT_AMT11 DECIMALS 0.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT11.
      WRITE      W_TEXT_AMT11  TO  REC_CUSDEC-REC_081 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_081   WITH  W_ZERO11.
   ENDIF.

   " 특소세.
   IF ZTIDR-ZFSCAMTS IS INITIAL.
      MOVE  '00000000000'  TO  REC_CUSDEC-REC_082.
   ELSE.
      WRITE      ZTIDR-ZFSCAMTS  TO  W_TEXT_AMT11 DECIMALS 0.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT11.
      WRITE      W_TEXT_AMT11  TO  REC_CUSDEC-REC_082 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_082   WITH  W_ZERO11.
   ENDIF.

   " 교육세.
   IF ZTIDR-ZFTRAMTS IS INITIAL.
      MOVE '00000000000'  TO  REC_CUSDEC-REC_083.
   ELSE.
      WRITE      ZTIDR-ZFTRAMTS  TO  W_TEXT_AMT11 DECIMALS 0.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT11.
      WRITE      W_TEXT_AMT11  TO  REC_CUSDEC-REC_083 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_083   WITH  W_ZERO11.
   ENDIF.

   " 부가세.
   IF ZTIDR-ZFVAAMTS IS INITIAL.
      MOVE  '00000000000'  TO  REC_CUSDEC-REC_084.
   ELSE.
      WRITE      ZTIDR-ZFVAAMTS  TO  W_TEXT_AMT11 DECIMALS 0.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT11.
      WRITE      W_TEXT_AMT11  TO  REC_CUSDEC-REC_084 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_084   WITH  W_ZERO11.
   ENDIF.

   " 농특세.
   IF ZTIDR-ZFAGAMTS IS INITIAL.
      MOVE  '00000000000'  TO  REC_CUSDEC-REC_085.
   ELSE.
      WRITE      ZTIDR-ZFAGAMTS  TO  W_TEXT_AMT11 DECIMALS 0.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT11.
      WRITE      W_TEXT_AMT11  TO  REC_CUSDEC-REC_085 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_085   WITH  W_ZERO11.
   ENDIF.

   " 주세.
   IF ZTIDR-ZFDRAMTS IS INITIAL.
      MOVE  '00000000000'  TO  REC_CUSDEC-REC_086.
   ELSE.
      WRITE      ZTIDR-ZFDRAMTS  TO  W_TEXT_AMT11 DECIMALS 0.
      PERFORM    P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT11.
      WRITE      W_TEXT_AMT11  TO  REC_CUSDEC-REC_086 RIGHT-JUSTIFIED.
      OVERLAY    REC_CUSDEC-REC_086   WITH  W_ZERO11.
   ENDIF.

   MOVE   REC_CUSDEC  TO  W_EDI_RECORD.

ENDFUNCTION.
