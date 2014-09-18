*&---------------------------------------------------------------------*
*& Include ZRIM00TOP                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입 System Main Data Define Include                  *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.01.25                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*-----------------------------------------------------------------------
* DESC : 사용할 Table Define
*-----------------------------------------------------------------------
TABLES : J_1BT001WV,
         ZTIEPORT,
        *ZTIEPORT,
         KONV,
         LFZA,
         ADRP,
         A902,
         ADRC,     " Addresses (central address admin.).
         PRPS,
         CSKT,
         VBAK,     " S/O HEADER
         VBAP,     " S/O Items
         VBKD,     "
         VBPA,
         EKKO,     " P/O Header Table
         EKPO,     " Purchasing Document Item
         EKBZ,     " 이력.
         EKBE,
         EIPO,     " Foreign Trade: Export/Import: Item Data
         EIKP,     " 해외무역: 수출/수입 헤더데이타.
         EKAB,
         KOMK,     " Pricing Communications-Header
         DD07T,    " DD: Texts for Domain Fixed Values (Language-Depende
         DD03L,    " Table Fields
         DD03T,    " DD: Texts for fields (language dependent)
         DD04L,    " Data elements
         DD04T,    " R/3 DD: Data element texts
         KONP,     " CONDITION ITEM
         KOMV,     " 가격결정 통신-조건레코?
         MARA,     " 자재마스터: 일반데이?
         MARC,     " 자재마스터: C 세그멘?
         MARD,     " 자재마스터: 저장위치/Batch 세그멘?
         MAKT,     " 자재내역.
         MBEW,     " 자재평가.
         USR01,    "  User master record (runtime data)
         USR02,    " Logon data
         LFA1,     " Vendor master (general section)
         LFM1,     " Vendor master record purchasing organization
         LFM2,     " Vendor Master Record: Purchasing Data
         LIKP,
         LIPS,
         WYT3,     " 파트너 기?
         TCURV,    "
         LFBK,     " Vendor Master (Bank Details)
         BNKA,     " Bank master record.
         STXH,     " STXD SAPscript text file header
         STXL,     " STXD SAPscript text file lines
         TTXID,    " Valid text IDs
         T001,     " Company Codes
         T001L,    " STORAGE LOCATION.
         T001W,    " Plants/branches
         T001K,    " Valuation area
         T005,     " Countries
         T005T,    " 국가이름.
         T604T,    " HS CODE 명.
         T005U,    " Taxes: Region Key: Texts.
         T006,     " 단위.
         T156,     " 이동유형.
         T685,     " 조건: 유형.
         T163B,    " 구매오더이력범?
         T163C,    " 구매오더이력범주에 대한 텍스?
         T024E,    " Purchasing Organizations
         T024,     " Purchasing Groups...
         T052,     " Terms of payment
         T007A,    " Tax Keys
         T16FB,    " Release Indicators: Purchasing Doc.
         T160,     " SAP Transaction Control, Purchasing
         T161,     " Purchasing Document Types
         T161M,    " Fine-Tuned Control: Message Types
         T161T,    " Texts for Purchasing Document Types
         T161V,    "Shipping-Specific Data for Stock Tfr. for Purch. Doc
         T165K,
         TCURR,    " 환?
         TCURC,    " Currency
         V_CURC,   " CURRENCY VIEW
         TCURT,    " CURRENCY NAME
         TINC,     " 고객: 인도조?
         TINCT,    " 고객: 인도조건: 텍스?
         TSTCT,    " Transaction Code Texts
         TMED,     " Purchasing Various Control Parameters
         SPOP,     " POPUP_TO_CONFIRM_... function 모듈 팝업화면 필?
         DFIES,    " View of Field Attributes for GET_FIELD
         BAPICURR, " BAPI에 대한 통화.
        *BAPICURR, " BAPI에 대한 통화.
         RSMPE,    " Menu Painter: Input/Output fields (4.0)
         BKPF,     " Accounting Document Header
         BSEG,     " Accounting Document Segment
         MSEG,     " Document Segment: Material
         VDARL,    " Loans
         TCURM,
         MT06E,
         MTCOR,
         MTCOM,
         *MT06E,
         MT06B,
         T004,
         T014,
         T043,
         T043T,
         X001,
         TCURX,    " Decimal Places in Currencies
*-----------------
* 한수원 결재관련. 2003.01.24
*----------------
         RBKP,
         VBKPF,
*-----------------------------------------------------------------------
* EDI CODE
*-----------------------------------------------------------------------
         ZTDHF1,
         ZTDDF1,
         ZTCDF1,
*-----------------------------------------------------------------------
* Bill of lading Table
*-----------------------------------------------------------------------
         ZTBL,     " Bill of Lading
        *ZTBL,     " Bill of Lading
         ZTBLIT,   " B/L 자재.
         ZTBLCON,  " B/L Container
         ZTBLCST,  " B/L 비용.
         ZTBLINOU, " B/L 반입예정,반입,반?
        *ZTBLINOU, " B/L 반입예정,반입,반?
         ZTBLINR,  " B/L 반입신?
        *ZTBLINR,  " B/L 반입신?
         ZTBLOUR,  " B/L 반출신?
        *ZTBLOUR,  " B/L 반출신?
         ZTBLUG,   " 긴급보세운송 의?
         ZTBLUGC,  " 긴급보세운송 차?
         ZTBURN,   " 기장번호 Number Range
         ZTTAXBKHD, ">기납증 헤더.
        *ZTTAXBKHD,
         ZTTAXBKIT, ">기납증 아이템.
        *ZTTAXBKIT, ">기납증 아이템.
*-----------------------------------------------------------------------
* L/G 문서.
*-----------------------------------------------------------------------
         ZTLG,     " Letter of Guarantee
        *ZTLG,     " Letter of Guarantee
         ZTLGGOD,  " L/G 상품명?
*-----------------------------------------------------------------------
* 통관요청.
*-----------------------------------------------------------------------
         ZTIV,        ">통관요청 Header
        *ZTIV,        ">통관요청 Header
         ZTIVIT,      ">통관요청 Item
         ZTIVCD,      ">통관요청 배부내역.
         ZTIVHST,     ">통관이력.
        *ZTIVHST,     ">통관이력.
         ZTIVHSTIT,   ">통관분할입고 자재이?
        *ZTIVHSTIT,
         ZSIVHSTIT,
*-----------------------------------------------------------------------
* Commercial Invoice
*-----------------------------------------------------------------------
         ZTCIVHD,      " Commercial Invoice HEADER
        *ZTCIVHD,      " Commercial Invoice HEADER
         ZTCIVIT,      " Commercial Invoice Items
        *ZTCIVIT,      " Commercial Invoice Items
         ZTCIVHST,     " Verify 이력.
*-----------------------------------------------------------------------
* 하역관리.
*-----------------------------------------------------------------------
         ZTCGHD,       " 하역관리 Header
        *ZTCGHD,       " 하역관리 Header
         ZTCGIT,       " 하역관리 자재.
        *ZTCGIT,       " 하역관리 자재.
         ZTCGCST,      " 하역관리 비용.
        *ZTCGCST,      " 하역관리 비용.
*-----------------------------------------------------------------------
* Payment Notice
*-----------------------------------------------------------------------
         ZTPMTHD,     " Payment Notice HEADER
         ZTPMTHST,    " 전기이력.
         *ZTPMTHD,    " Payment Notice HEADER
         ZTPMTIV,     " Payment Notice Invoice
*-----------------------------------------------------------------------
* Clearence
*-----------------------------------------------------------------------
         ZTCUCL,      " 통?
         ZTCUCLCST,   " 통관비?
         ZTCUCLIV,    " 통관 Invoice
         ZTCUCLIVIT,  " 통관 Invoice Item
         ZTIDR,       " 수입신?
         *ZTIDR,      " 수입신?
         ZTIDRHS,     " 수입신고 란사?
         ZTIDRHSD,    " 수입신고 규?
         ZTIDRHSL,    " 수입신고 요건 확?
         ZTIDS,       " 수입면?
         *ZTIDS,      " 수입면?
         ZTIDSHS,     " 수입면허 란사?
         ZTIDSHSD,    " 수입면허 규?
         ZTIDSHSL,    " 수입면허 요건 확?
         ZTIDRCR,     " 수입관?
         ZTIDRCRIT,   " 감면허가품?
         ZTIDRDTU,    " 감면허가품목 사용실?
*-----------------------------------------------------------------------
* Clearence(US)
*-----------------------------------------------------------------------
         ZTIDRUS,
         *ZTIDRUS,
         ZTIDRUSH,
         ZTIDRUSD,
         ZTIDSUS,
         *ZTIDSUS,
         ZTIDSUSH,
         ZTIDSUSD,
*-----------------------------------------------------------------------
* 세금계산?
*-----------------------------------------------------------------------
         ZTVTIV,      " 세금계산서용 Invoice
         ZTVTIVIT,    " 세금계산서용 Invoice Item
         ZTVT,        " 세금계산?
         ZTVTSG1,     " 세금계산서 Seg 1
         ZTVTSG3,     " 세금계산서 Seg 3
         ZTRED,       " 인수?
         ZTREDSG1,    " 인수증 Seg 1
*-----------------------------------------------------------------------
* 수입보험용 TABLE
*-----------------------------------------------------------------------
         ZTINS,    " 보험부?
        *ZTINS,    " 보험부?
         ZTINSAGR, " 보험부보 AGR
         ZTINSRSP, " 보험부보 Response
        *ZTINSRSP, " 보험부보 Response
         ZTINSSG2, " 보험부보 Seg 2
         ZTINSSG3, " 보험부보 Seg 3
        *ZTINSSG3, " 보험부보 Seg 3
         ZTINSSG5, " 보험부보 Seg 5
*-----------------------------------------------------------------------
* 수입보험용(BL) TABLE
*-----------------------------------------------------------------------
         ZTINSB,    " 보험부?
        *ZTINSB,    " 보험부?
         ZTINSBAGR, " 보험부보 AGR
         ZTINSBRSP, " 보험부보 Response
        *ZTINSBRSP, " 보험부보 Response
         ZTINSBSG2, " 보험부보 Seg 2
         ZTINSBSG3, " 보험부보 Seg 3
        *ZTINSBSG3, " 보험부보 Seg 3
         ZTINSBSG5, " 보험부보 Seg 5
*-----------------------------------------------------------------------
* 수입의뢰 비용 TABLE
*-----------------------------------------------------------------------
         ZTRECST,  " 비용 Header Table
        *ZTRECST,  " 비용 Header Table
*-----------------------------------------------------------------------
* 수입의뢰용 TABLE
*-----------------------------------------------------------------------
         ZTREQHD,      " 수입의뢰 Header Table
         ZTREQHD_TMP,  " 수입의뢰 Header Table
        *ZTREQHD,      " 수입의뢰 Header Table
         ZTREQST,      " 수입의뢰 상태(Status) Table
        *ZTREQST,      " 수입의뢰 상태(Status) Table
         ZTREQIT_TMP,  " 수입의뢰 품목(Item) Table
         ZTREQIT,      " 수입의뢰 품목(Item) Table
         ZTREQIL_TMP,  " 수입추천 Table
         ZTREQIL,      " 수입추천 Table
         ZTREQORJ_TMP, " 수입의뢰 원산지 내?
         ZTREQORJ,     " 수입의뢰 원산지 내?
*-----------------------------------------------------------------------
* MASTER L/C용 TABLE
*-----------------------------------------------------------------------
         ZTMLCHD,       " Master L/C
        *ZTMLCHD,       " Master L/C
         ZTMLCSG2,      " Master L/C Seg 2
        *ZTMLCSG2,      " Master L/C Seg 2
         ZTMLCSG7G,     " Master L/C Seg 7 상품명?
         ZTMLCSG7O,     " Master L/C Seg 7 원산?
         ZTMLCSG8E,     " Master L/C Seg 8 기타부가조?
         ZTMLCSG910,    " Master L/C Seg 9-10
        *ZTMLCSG910,    " Master L/C Seg 9-10
         ZTMLCSG9O,     " Master L/C Seg 9 기타구비서?
*-----------------------------------------------------------------------
* MASTER L/C AMEND용 TABLE
*-----------------------------------------------------------------------
         ZTMLCAMHD,     " Master L/C Amend
        *ZTMLCAMHD,     " Master L/C Amend
         ZTMLCAMNARR,   " Master L/C Amend 기타조건변경사?
*-----------------------------------------------------------------------
* LOCAL L/C TABLE
*-----------------------------------------------------------------------
         ZTLLCHD,       " Local L/C
        *ZTLLCHD,       " Local L/C
         ZTLLCOF,       " Local L/C 물품매도확약?
         ZTLLCSG23,     " Local L/C Seg 2 - 3
        *ZTLLCSG23,     " Local L/C Seg 2 - 3
         ZTLLCAMHD,     " Local L/C amend
        *ZTLLCAMHD,     " Local L/C amend
         ZTLLCAMSGOF,   " Local L/C amend 물품매도확약?
*-----------------------------------------------------------------------
* 구매승인서 TABLE
*-----------------------------------------------------------------------
         ZTPUR,         " 구매승인서 HEAD
        *ZTPUR,         " 구매승인서 HEAD
         ZTPURSG1,      " 구매승인서 SEG 1
         ZTPURSG1G,     " 구매승인서 SEG 1 품목규?
         ZTPURSG4,      " 구매승인서 Seg 4
*-----------------------------------------------------------------------
* T/T TABLE
*-----------------------------------------------------------------------
         ZTTTHD,        " TT(지급지시서) Header
        *ZTTTHD,        " TT(지급지시서) Header
         ZTTTSG5,       " TT(지급지시서) SG5
*-----------------------------------------------------------------------
* Offer Sheet TABLE
*-----------------------------------------------------------------------
         ZTOFF,         " Offer(물품매도확약서)
        *ZTOFF,         " Offer(물품매도확약서)
         ZTOFFFTX,      " Offer FTX
        *ZTOFFFTX,      " Offer FTX
         ZTOFFO,        " Offer 원산?
         ZTOFFSDE,      " Offer 선적시기 도착시기 기타참?
         ZTOFFSG6,      " Offer Seg 6
*-----------------------------------------------------------------------
* CONFIGURATION용 TABLE
*-----------------------------------------------------------------------
         ZTIMIMG00,     " 수입시스템 Basic Config Table
        *ZTIMIMG00,     " 수입시스템 Basic Config Table
         ZTIMIMGTX,     " EDI TEXT(COMPANY별)
        *ZTIMIMGTX,     " EDI TEXT(COMPANY별)
         ZTIMIMG01,     " 수입의뢰 가능 Condition Table
        *ZTIMIMG01,     " 수입의뢰 가능 Condition Table
         ZTIMIMG02,     " 세?
        *ZTIMIMG02,     " 세?
         ZTIMIMG03,     " 보세구?
        *ZTIMIMG03,     " 보세구?
         ZTIMIMG04,     " PLANNED COST RATE
        *ZTIMIMG04,     " PLANNED COST RATE
         ZTIMIMG05,     " 보세운송 운임단?
        *ZTIMIMG05,     " 보세운송 운임단?
         ZTIMIMG06,     " 관세청 고시환?
        *ZTIMIMG06,     " 관세청 고시환?
         ZTIMIMG07,     " 관세청 통관수수료?
        *ZTIMIMG07,     " 관세청 통관수수료?
         ZTIMIMG08,     " 관리코?
        *ZTIMIMG08,     " 관리코?
         ZTIMIMG09,     " HS코드별 관세?
        *ZTIMIMG09,     " HS코드별 관세?
         ZTIMIMG10,     " 관세사 관?
        *ZTIMIMG10,     " 관세사 관?
         ZTIMIMG11,     " G/R, I/V, 비용처리 Configuration
        *ZTIMIMG11,     " G/R, I/V, 비용처리 Configuration
         ZTIMIMG12,     " 운송수단 MATCH CODE 관리.
        *ZTIMIMG12,     " 운송수단 MATCH CODE 관리.
         ZTIMIMG17,     " 항공화물해외운송 요?
        *ZTIMIMG17,     " 항공화물해외운송 요?
         ZTIMIMG21,     " 해외항공화물운송요율표 관리. NHJ 2002.09.18
        *ZTIMIMG21,     " 해외항공화물운송요율표 관리. NHJ 2002.09.18
         ZTIMIMG22,     " 해외해상화물운송요율표 관리. NHJ 2002.09.18
        *ZTIMIMG22,     " 해외해상화물운송요율표 관리. NHJ 2002.09.18
         ZTIMIMG23,     " Port/Freight applicable area MATCH
        *ZTIMIMG23,     " Port/Freight applicable area MATCH
         ZTIMIMG24,     " MID Management..             NSH 2004.05.25
        *ZTIMIMG24,     " MID Management..             NSH 2004.05.25
         ZTIMIMG20,     " 수송요율 유지보수. JSY20020912.
        *ZTIMIMG20,     " 수송요율 유지보수. JSY20020912.
*-----------------------------------------------------------------------
* 모선관리 TABLE
*-----------------------------------------------------------------------
        ZTMSHD,        "모선관리 Header
       *ZTMSHD,        "모선관리 Header
        ZTMSIT,        "모선관리 Item
        ZTMSCST,       "조출/체선료 Table
*-----------------------------------------------------------------------
* 수송 관련 TABLE.  (한수원 JSY20020912 추가 )
*-----------------------------------------------------------------------
         ZTTRHD,        " 보세창고출고(수송) Header
        *ZTTRHD,        " 보세창고출고(수송) Header
         ZTTRIT,        " Delivery Item
         ZTTRITD,       " Delivery Item Detail
         ZTTRCST,       " 수송비 세부내역.
         ZTTRCSTIT.     " 수송비 산출내역.
*-----------------------------------------------------------------------
* DESC : 사용할 구조체 Define
*-----------------------------------------------------------------------
TABLES: ZSREQHD,     " 수입의뢰 Header용 Structure
        ZSREQIT,     " 수입의뢰 품목(Item) Structure
        ZSREQIL,     " 수입추천 Structure
        ZSRECST,     " 수입의뢰 비용 Structure
        ZSBLCON,     " Container 구조.
        ZSBLCST,     " B/L 비용 구조.
        ZSBLINOU,    " B/L 반입예정,반입,반출 Structure
        ZSBLUGC,     " 긴급보세운송 의뢰 차량 Structure
        ZSBL,        " B/L HEADER STRUCTURE.
        ZSBLIT,      " B/L 자재내역 Structure
        ZSTAXBKIT,   " 기납증 아이템.
        ZSCGHD,      " 하역 헤더.
        ZSCGIT,      " 하역 자재.
        ZSCGCST,     " 하역 비용.
        ZSLGGOD,     " L/G 상품명세 Structure
        KOMP,        " 가격결정 통신품목.
        ZSIV,        " 통관요청 HEADER 구조.
        ZSIVIT,      " 통관요청 Item 구조.
        ZSIVCD,      " 통관요청 비용배부 구조.
        ZSIVHST,     " G/R 이력.
        ZSIVHST1,    " 제비용 이력.
        ZSCIVHD,     " Commercial Invoice.
        ZSCIVIT,     " Commercial Invoice Items
        ZSCIVHST,    " VERIFY 이력.
        ZSCUCLIVIT,  " 과세통관 Invoice Item 구조.
        ZSCUCLCST,   " 통관비용 구조.
        ZSIDRDTU,    " 감면허가품목 사용실적 구조.
        ZSAMDLIST,   " 수입의뢰 Amend Field List
        ZSPMTIV,     " Payment Notice Item
        ZSVTIVIT,    " 세금계산서용 Invoice Item 구조.
        ZSVTSG3,     " 세금계산서 Segemnt 3 구조.
        ZSREDSG1,    " 인수증 Segemnt 1 구조.
        ZSLLCOF,     " Local L/C 물품매도확약.
        ZSMLCAMNARR, " Master L/C Amend 기타조건변경사항 구조.
        ZSMLCSG7G,   " Master L/C Seg 7 상품명세 구조.
        ZSMLCSG7O,   " Master L/C Seg 7 원산지.
        ZSMLCSG7O_1, " Master L/C Seg 7 원산지.
        ZSMLCSG8E,   " Master L/C Seg 8 기타부가조건.
        ZSMLCSG9O,   " Master L/C Seg 9 Master L/C Seg 9 기타구비서류.
        ZSPURSG1,    " 구매승인서 Seg 1( 물품명세 )
        ZSPURSG1G,   " 구매승인서 Seg 1 품목규격(구조체)
        ZSPURSG4,    " 구매승인서 Seg 4( 근거서류 )
        ZSRECOMMEND, " 수입추천 관리용 구조.
        ZEDIFILE,    " EDI .
        ZSLLCAMSGOF, " Local L/C Amend 최종물품매도확약서 Structure
        ZSOFFO,      " Offer 원산지 Structure
        ZSTCURR,     " 통관 환율.
        ZSTTSG5,     " 지급지시서 관련서류 Structure
        ZSOFFSDE,    " Offer 기타참조사항 Structure
        ZSOFFSG6,    " Offer Seg 6 Structure
        ZSOFFSG6G,   " Offer Seg 6 품목규격 Structure
        ZSIMIMG01,   " Payment Term Configuration Structure
        ZSIMIMG02,   " 세관/공급업체 매치코드 Structure
        ZSIMIMG03,   " 보세구역 Structure
        ZSIMIMG04,   " PLANNED COST RATE Structure
        ZSIMIMG05,   " 보세운송 운임단가 Structure
        ZSIMIMG06,   " 관세청 고시환율 Structure
        ZSIMIMG07,   " 관세청 통관수수료율 Structure
        ZSIMIMG08,   " 관리코드 Structure
        ZSIMIMG09,   " H/S CODE별 관세율 구조.
        ZSIMIMG10,   " 관세사/공급업체 Match CODE
        ZSIMIMG12,   " 운송수단 MATCH CODE 관리.
        ZSIMIMG17,   " 항공화물해외운송 요율.
        ZSIMIMG20,   " 수송요율 유지보수.   <===추가=====JSY20020912.
        ZSIMIMG21,   " 항공화물 해외 운송 요율.
        ZSIMIMG22,   " 해상화물 해외 운송 요율.
        ZSIMIMG23,   " Port/Freight applicable area MATCH
        ZSIEPORT,    " Managing port by country
        ZSIMIMG24,   " MID Management.
        ZSDOHD,      " Delivery Header
        ZSDOIT,      " Delivery Item
        ZSTRIT,      " 수송Item.            <===추가=====JSY20020916.
        ZSTRCST,     " 수송비 세부내역.     <===추가=====JSY20020927.
        ZSTRCSTIT,   " 수송비 산출내역.     <===추가=====JSY20021030.
        ZSINSHD,     " 보험부보 HEADER 구조.
        ZSINSSG2,    " 적하보험 SG2 상품명세 구조.
        ZSINSAGR,    " 보험부보 기본/부가조건 Structure
        ZSINSSG5,    " 보험부보 Seg 5
        ZSINSBSG2,   " 적하보험 SG2 상품명세 구조(BL).
        ZSINSBAGR,   " 보험부보 기본/부가조건 Structure(BL)
        ZSINSBSG5,   " 보험부보 Seg 5(BL)
        ZSIDRHS,     " 수입신고 란사?
        ZSIDRHSD,    " 수입신고 규?
        ZSIDRHSL,    " 수입신고 요건확건.
        ZSIDSHS,     " 수입면허 란사항.
        ZSIDSHSD,    " 수입면허 규격.
        ZSIDSHSL,    " 수입면허 요건확건.
        ZSIDRCR,     " 감면허가.
        ZSIDRCRIT,   " 감면허가품목.
        ZSIDRUSH,
        ZSIDRUSD,
        ZSIDSUSH,
        ZSIDSUSD,
        ZSMSIT,      " 모선관리 Item
        ZSMSHD,      " 모선관리 Header
        ZSMSCST,     " 모선관리 조출/체선료.
        ZSIMCOST,    " 수입비용.
        ZSPMTHST,
        EKET,
        ZSCCHD.      " 관세/부가세.

*-----------------------------------------------------------------------
*  DESC : 사용할 VIEW Define
*-----------------------------------------------------------------------
TABLES : ZVREQHD_ST,    " 수입의뢰 Header + Status View
         ZVEKKO_REQHD_ST, " P/O Header + 수입의뢰 Header + 수입의뢰 Stat
         ZVIMCOST,      " 수입의뢰비용.
         V_T077K,       " Vendor Account Groups
         V_TCURR,       ">
         V_T052.        " Terms of Payment
*-----------------------------------------------------------------------
* DESC : 보험 AMEND용 TEMP
*-----------------------------------------------------------------------
DATA :  ZTINS_OLD     LIKE ZTINS,    " 보험부보.
        OLD_ZTINS     LIKE ZTINS,    " 보험부보.
        ZTINSRSP_OLD  LIKE ZTINSRSP, " 보험부보 Response
        ZTINSSG3_OLD  LIKE ZTINSSG3. " 보험부보 Seg

DATA :  ZTINSB_OLD     LIKE ZTINSB,    " 보험부보.
        OLD_ZTINSB     LIKE ZTINSB,    " 보험부보.
        ZTINSBRSP_OLD  LIKE ZTINSBRSP, " 보험부보 Response
        ZTINSBSG3_OLD  LIKE ZTINSBSG3. " 보험부보 Seg

*-----------------------------------------------------------------------
*>
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_BLSDP_HELP OCCURS 0,
       ZFBLSDP   LIKE ZTIMIMG08-ZFCD,
       ZFCDNM    LIKE ZTIMIMG08-ZFCDNM,
       END OF IT_BLSDP_HELP.

DATA : BEGIN OF IT_ZFCD_HELP OCCURS 0,
       ZFCD      LIKE ZTIMIMG08-ZFCD,
       ZFCDNM    LIKE ZTIMIMG08-ZFCDNM,
       END OF IT_ZFCD_HELP.

DATA : BEGIN OF IT_OPBN_HELP OCCURS 0,
       LIFNR     LIKE LFA1-LIFNR,
       NAME1     LIKE LFA1-NAME1,
       ORT01     LIKE LFA1-ORT01,
       END OF IT_OPBN_HELP.

*-----------------------------------------------------------------------
* DESC : DATA TYPE DEFINE
*-----------------------------------------------------------------------
TYPES: BEGIN OF STATE_VECTOR,
         TYPE(1)   TYPE C, " E=Einstufig Z=Zweistufig
         ACTION(1) TYPE C, " S=Anz., U=훞d., A=Hinzuf., T=Tr
         MODE(1)   TYPE C, " L=Liste, D=Detail
         DATA(1)   TYPE C, " G=gesamt, X=Extract, D=Geloeschte
         MARK(1)   TYPE C, " M=Markiert,  =Nicht Markiert
         DELETE(1) TYPE C, " D=Gel?cht,  =Nicht Gel?cht
         FILL1(1)  TYPE C, "filler, not used
         FILL2(1)  TYPE C, "     - " -
END OF STATE_VECTOR.

* 수입의뢰비용.
DATA : IT_ZSIMCOST LIKE ZSIMCOST OCCURS 0 WITH HEADER LINE.

* 변경 항목을 보여주기위한           Internal Table
DATA : IT_ZSAMDLIST LIKE ZSAMDLIST OCCURS 50 WITH HEADER LINE.
DATA : IT_DD03L     LIKE DD03L     OCCURS 100 WITH HEADER LINE.

DATA : BEGIN OF IT_IVIT_SUM OCCURS 50,
       ZFREQNO    LIKE    ZSIVIT-ZFREQNO,
       ZFITMNO    LIKE    ZSIVIT-ZFITMNO,
       ZFBLNO     LIKE    ZSIVIT-ZFBLNO,
       ZFBLIT     LIKE    ZSIVIT-ZFBLIT,
       ZFCGNO     LIKE    ZSIVIT-ZFCGNO,
       ZFCGIT     LIKE    ZSIVIT-ZFCGIT,
       CCMENGE    LIKE    ZSIVIT-CCMENGE,
       END OF IT_IVIT_SUM.

DATA : BEGIN OF IT_BUKRS OCCURS 10,
       BUKRS      LIKE    T001-BUKRS,
       END   OF IT_BUKRS.

DATA : BEGIN OF IT_T001  OCCURS 10.
INCLUDE   STRUCTURE  T001.
DATA : ZFMARK    TYPE   C,
       LOEKZ     LIKE   EKKO-LOEKZ.
DATA : END   OF IT_T001.

DATA : BEGIN OF IT_0210  OCCURS 10.
DATA : BUKRS     LIKE   ZTIMIMG21-BUKRS,
       ZFAPLDT   LIKE   ZTIMIMG21-ZFAPLDT,
       ZFRAGB    LIKE   ZTIMIMG21-ZFRAGB,
       ZFMARK    TYPE   C,
       LOEKZ     LIKE   EKKO-LOEKZ.
DATA : END   OF IT_0210.

DATA : BEGIN OF IT_0220  OCCURS 10.
DATA : BUKRS     LIKE   ZTIMIMG22-BUKRS,
       ZFAPLDT   LIKE   ZTIMIMG22-ZFAPLDT,
       ZFMARK    TYPE   C,
       LOEKZ     LIKE   EKKO-LOEKZ.
DATA : END   OF IT_0220.

DATA : IT_VBAP      LIKE VBAP      OCCURS 10 WITH HEADER LINE.
DATA : IT_ZSIVHSTIT LIKE ZSIVHSTIT OCCURS 10 WITH HEADER LINE.
*----------------------------------------------------------------------*
* 기납증 품목 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSTAXBKIT     LIKE ZSTAXBKIT OCCURS 10 WITH HEADER LINE.
DATA : IT_ZSTAXBKIT_ORG LIKE ZSTAXBKIT OCCURS 10 WITH HEADER LINE.

*----------------------------------------------------------------------*
* 품목 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSREQHD  LIKE ZSREQHD OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSCIVHD  LIKE ZSCIVHD OCCURS 50 WITH HEADER LINE.
DATA : IT_ZTLG     LIKE ZTLG    OCCURS 50 WITH HEADER LINE.
DATA : IT_ZTINS    LIKE ZTINS   OCCURS 50 WITH HEADER LINE.

DATA : IT_ZSTAXBKHD LIKE ZTTAXBKHD OCCURS 50 WITH HEADER LINE.
DATA : IT_ZTIDRUS  LIKE ZTIDRUS OCCURS 50 WITH HEADER LINE.    "NCW추가
*----------------------------------------------------------------------*
* 품목 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSREQIT     LIKE ZSREQIT OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSREQIT_ORG LIKE ZSREQIT OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSREQIT_OLD LIKE ZSREQIT OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSREQIT_TMP LIKE ZTREQIT_TMP OCCURS 100 WITH HEADER LINE.

DATA : IT_EKAB        LIKE EKAB    OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* 수입의뢰 원산지 명세 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZTREQORJ     LIKE ZSMLCSG7O OCCURS 10 WITH HEADER LINE.
DATA : IT_ZTREQORJ_ORG LIKE ZSMLCSG7O OCCURS 10 WITH HEADER LINE.
DATA : IT_ZTREQORJ_OLD LIKE ZSMLCSG7O OCCURS 10 WITH HEADER LINE.
DATA : IT_ZTREQORJ_TMP LIKE ZTREQORJ_TMP OCCURS 10 WITH HEADER LINE.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* 수입의뢰 추천 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSREQIL      LIKE ZSREQIL   OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSREQIL_ORG  LIKE ZSREQIL   OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSREQIL_OLD  LIKE ZSREQIL   OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSREQIL_TMP  LIKE ZTREQIL_TMP   OCCURS 50 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* 품목 명세 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSMLCSG7G     LIKE ZSMLCSG7G OCCURS 500 WITH HEADER LINE.
DATA : IT_ZSMLCSG7G_ORG LIKE ZSMLCSG7G OCCURS 500 WITH HEADER LINE.
DATA : IT_ZSMLCSG7G_OLD LIKE ZSMLCSG7G OCCURS 500 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* 원산지 명세 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSMLCSG7O     LIKE ZSMLCSG7O OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSMLCSG7O_ORG LIKE ZSMLCSG7O OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSMLCSG7O_OLD LIKE ZSMLCSG7O OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* 기타부가 조건 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSMLCSG8E     LIKE ZSMLCSG8E OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSMLCSG8E_ORG LIKE ZSMLCSG8E OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSMLCSG8E_OLD LIKE ZSMLCSG8E OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* 기타구비서류  리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSMLCSG9O     LIKE ZSMLCSG9O OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSMLCSG9O_ORG LIKE ZSMLCSG9O OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSMLCSG9O_OLD LIKE ZSMLCSG9O OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* 수입승인내역 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSINSHD       LIKE ZSINSHD   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSINSHD_ORG   LIKE ZSINSHD   OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* 물품매도확약서 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSLLCOF       LIKE ZSLLCOF   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSLLCOF_ORG   LIKE ZSLLCOF   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSLLCOF_OLD   LIKE ZSLLCOF   OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* 구매승인서 Seg 1( 물품명세 ) 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSPURSG1       LIKE ZSPURSG1  OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSPURSG1_ORG   LIKE ZSPURSG1  OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSPURSG1_OLD   LIKE ZSPURSG1  OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* 구매승인서 Seg 1( 품목규격 ) 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSPURSG1G       LIKE ZSPURSG1G  OCCURS 0 WITH HEADER LINE.
* 구매승인서 Seg 1 리스트를 위한 Internal Table( Original Data )
DATA : IT_ZSPURSG1G_ORG   LIKE ZSPURSG1G  OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSPURSG1G_OLD   LIKE ZSPURSG1G  OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSPURSG1G_SUB   LIKE ZSPURSG1G  OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* 구매승인서 Seg 4( 근거서류 ) 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSPURSG4       LIKE ZSPURSG4  OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSPURSG4_ORG   LIKE ZSPURSG4  OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSPURSG4_OLD   LIKE ZSPURSG4  OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* 지급지시서 관련서류 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSTTSG5      LIKE ZSTTSG5   OCCURS 10 WITH HEADER LINE.
DATA : IT_ZSTTSG5_ORG  LIKE ZSTTSG5   OCCURS 10 WITH HEADER LINE.
DATA : IT_ZSTTSG5_OLD  LIKE ZSTTSG5   OCCURS 10 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Amend 기타조건변경사항 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSMLCAMNARR      LIKE ZSMLCAMNARR OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSMLCAMNARR_ORG  LIKE ZSMLCAMNARR OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSMLCAMNARR_OLD  LIKE ZSMLCAMNARR OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Local L/C Amend 최종물품매도확약서 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSLLCAMSGOF      LIKE ZSLLCAMSGOF OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSLLCAMSGOF_ORG  LIKE ZSLLCAMSGOF OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSLLCAMSGOF_OLD  LIKE ZSLLCAMSGOF OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* local offer sheet 원산지 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSOFFO          LIKE ZSOFFO      OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSOFFO_ORG      LIKE ZSOFFO      OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSOFFO_OLD      LIKE ZSOFFO      OCCURS 100 WITH HEADER LINE.

*----------------------------------------------------------------------*
* local offer sheet 기타참조 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSOFFSDE        LIKE ZSOFFSDE    OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSOFFSDE_ORG    LIKE ZSOFFSDE    OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSOFFSDE_OLD    LIKE ZSOFFSDE    OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* local offer seg. 6 리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSOFFSG6        LIKE ZSOFFSG6    OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSOFFSG6_ORG    LIKE ZSOFFSG6    OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSOFFSG6_OLD    LIKE ZSOFFSG6    OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* local offer seg. 6 규격    리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSOFFSG6G       LIKE ZSOFFSG6G   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSOFFSG6G_ORG   LIKE ZSOFFSG6G   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSOFFSG6G_OLD   LIKE ZSOFFSG6G   OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Invoice Head  리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSIV      LIKE ZSIV      OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIV_TMP  LIKE ZSIV      OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIV_ORG  LIKE ZSIV      OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Invoice Head  리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSIVCD      LIKE ZSIVCD  OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIVCD_ORG  LIKE ZSIVCD  OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIVCD_TMP  LIKE ZSIVCD  OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* 수입의뢰 비용리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSRECST      LIKE ZSRECST   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSRECST_ORG  LIKE ZSRECST   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSRECST_OLD  LIKE ZSRECST   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSRECST_DEL  LIKE ZSRECST   OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* B/L 자재  리스트를 위한 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSBLIT      LIKE ZSBLIT    OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLIT_MUL  LIKE ZSBLIT    OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLIT_TMP  LIKE ZSBLIT    OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLIT_ORG  LIKE ZSBLIT    OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLIT_OLD  LIKE ZSBLIT    OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLIT_DEL  LIKE ZSBLIT    OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLIT_PO   LIKE ZSBLIT    OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table of B/L Container
*----------------------------------------------------------------------*
DATA : IT_ZSBLCON      LIKE ZSBLCON   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLCON_ORG  LIKE ZSBLCON   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLCON_OLD  LIKE ZSBLCON   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLCON_DEL  LIKE ZSBLCON   OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table of B/L Expense
*----------------------------------------------------------------------*
DATA : IT_ZSBLCST      LIKE ZSBLCST   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLCST_ORG  LIKE ZSBLCST   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLCST_OLD  LIKE ZSBLCST   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLCST_DEL  LIKE ZSBLCST   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLCST_POST  LIKE ZSBLCST   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBLCST_POST1 LIKE ZSBLCST   OCCURS 100 WITH HEADER LINE.
DATA : BEGIN OF IT_BLCSTHD  OCCURS 10,
       ZFMARK    LIKE  ZSBLCST-ZFMARK,
       ZFSEQ     LIKE  ZSBLCST-ZFSEQ,
       ZFPAY     LIKE  ZTBLCST-ZFPAY,
       NAME1     LIKE  LFA1-NAME1,
       COND_TYPE LIKE  ZSBLCST-COND_TYPE,
       ZFCDTY    LIKE  ZSBLCST-ZFCDTY,
       TEXT(20),
       MWSKZ     LIKE  ZSBLCST-MWSKZ,
       ZFWERKS   LIKE  ZSBLCST-ZFWERKS,
       ZTERM     LIKE  ZSBLCST-ZTERM,
       BLART     LIKE  ZTBKPF-BLART,
       BUPLA     LIKE  ZTBKPF-BUPLA,
       WAERS     LIKE  ZSBLCST-WAERS,
       ZFCD1     LIKE  ZSBLCST-ZFCD1,
       END   OF IT_BLCSTHD.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table for L/G Goods Description
*----------------------------------------------------------------------*
DATA : IT_ZSLGGOD      LIKE ZSLGGOD   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSLGGOD_ORG  LIKE ZSLGGOD   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSLGGOD_OLD  LIKE ZSLGGOD   OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSLGGOD_DEL  LIKE ZSLGGOD   OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table for B/L Other Expense
*----------------------------------------------------------------------*
DATA : IT_ZSBLCST1      LIKE ZSBLCST   OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSBLCST1_ORG  LIKE ZSBLCST   OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSBLCST1_OLD  LIKE ZSBLCST   OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSBLCST1_DEL  LIKE ZSBLCST   OCCURS 50 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table for Unloading Expense
*----------------------------------------------------------------------*
DATA : IT_ZSCGCST      LIKE ZSCGCST   OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSCGCST_ORG  LIKE ZSCGCST   OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSCGCST_OLD  LIKE ZSCGCST   OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSCGCST_DEL  LIKE ZSCGCST   OCCURS 50 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table for Unloading Material
*----------------------------------------------------------------------*
DATA : IT_ZSCGIT      LIKE ZSCGIT   OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSCGIT_ORG  LIKE ZSCGIT   OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSCGIT_OLD  LIKE ZSCGIT   OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSCGIT_DEL  LIKE ZSCGIT   OCCURS 50 WITH HEADER LINE.
*----------------------------------------------------------------------*
DATA : IT_ZSCGHD      LIKE ZSCGHD   OCCURS 50 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Internal Table for B/L Bonded-in, Bonded-out Structure
*----------------------------------------------------------------------*
DATA : IT_ZSBLINOU      LIKE ZSBLINOU  OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSBLINOU_ORG  LIKE ZSBLINOU  OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSBLINOU_OLD  LIKE ZSBLINOU  OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSBLINOU_DEL  LIKE ZSBLINOU  OCCURS 50 WITH HEADER LINE.

DATA : IT_ZTBLINR       LIKE ZTBLINR   OCCURS 50 WITH HEADER LINE.
DATA : IT_ZTBLOUR       LIKE ZTBLOUR   OCCURS 50 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Payment Term Configuration Structure
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG01     LIKE ZSIMIMG01 OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSIMIMG01_ORG LIKE ZSIMIMG01 OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSIMIMG01_DEL LIKE ZSIMIMG01 OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSIMIMG01_OLD LIKE ZSIMIMG01 OCCURS 50 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Planned Cost Rate  Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG02     LIKE ZSIMIMG02 OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSIMIMG02_ORG LIKE ZSIMIMG02 OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSIMIMG02_DEL LIKE ZSIMIMG02 OCCURS 50 WITH HEADER LINE.
DATA : IT_ZSIMIMG02_OLD LIKE ZSIMIMG02 OCCURS 50 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table for Bonded Area Code
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG03     LIKE ZSIMIMG03 OCCURS 10 WITH HEADER LINE.
DATA : IT_ZSIMIMG03_ORG LIKE ZSIMIMG03 OCCURS 10 WITH HEADER LINE.
DATA : IT_ZSIMIMG03_DEL LIKE ZSIMIMG03 OCCURS 10 WITH HEADER LINE.
DATA : IT_ZSIMIMG03_OLD LIKE ZSIMIMG03 OCCURS 10 WITH HEADER LINE.
DATA : IT_ZSIMIMG03_TMP LIKE ZSIMIMG03 OCCURS 10 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Planned Cost Rate  Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG04     LIKE ZSIMIMG04 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG04_ORG LIKE ZSIMIMG04 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG04_DEL LIKE ZSIMIMG04 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG04_OLD LIKE ZSIMIMG04 OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal  Table for Bonded Transportation Freight Unit Price
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG05     LIKE ZSIMIMG05 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG05_ORG LIKE ZSIMIMG05 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG05_DEL LIKE ZSIMIMG05 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG05_OLD LIKE ZSIMIMG05 OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table for exchange rate of Customs House
*----------------------------------------------------------------------*
DATA : IT_ZTIMIMG06     LIKE ZTIMIMG06 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG06     LIKE ZSIMIMG06 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG06_ORG LIKE ZSIMIMG06 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG06_DEL LIKE ZSIMIMG06 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG06_OLD LIKE ZSIMIMG06 OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table for Customs Broker Fee
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG07     LIKE ZSIMIMG07 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG07_ORG LIKE ZSIMIMG07 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG07_DEL LIKE ZSIMIMG07 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG07_OLD LIKE ZSIMIMG07 OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table for Code Management
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG08     LIKE ZSIMIMG08 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG08_ORG LIKE ZSIMIMG08 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG08_DEL LIKE ZSIMIMG08 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG08_OLD LIKE ZSIMIMG08 OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Tables for duty rate of HS Code
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG09     LIKE ZSIMIMG09 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG09_ORG LIKE ZSIMIMG09 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG09_DEL LIKE ZSIMIMG09 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG09_OLD LIKE ZSIMIMG09 OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table for Customs Broker/Vendor Match
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG10     LIKE ZSIMIMG10 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG10_ORG LIKE ZSIMIMG10 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG10_DEL LIKE ZSIMIMG10 OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG10_OLD LIKE ZSIMIMG10 OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Tables for Air freight rate
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG17     LIKE ZSIMIMG17 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG17_ORG LIKE ZSIMIMG17 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG17_DEL LIKE ZSIMIMG17 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG17_OLD LIKE ZSIMIMG17 OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Tables for Air freight rate < NHJ 2002.09.18 >
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG21     LIKE ZSIMIMG21 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG21_ORG LIKE ZSIMIMG21 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG21_DEL LIKE ZSIMIMG21 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG21_OLD LIKE ZSIMIMG21 OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Tables for Ocean freight rate < NHJ 2002.09.18 >
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG22     LIKE ZSIMIMG22 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG22_ORG LIKE ZSIMIMG22 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG22_DEL LIKE ZSIMIMG22 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG22_OLD LIKE ZSIMIMG22 OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Tables for Ocean freight rate < NHJ 2002.09.18 >
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG24     LIKE ZSIMIMG24 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG24_ORG LIKE ZSIMIMG24 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG24_DEL LIKE ZSIMIMG24 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG24_OLD LIKE ZSIMIMG24 OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* INTERNAL TABLES for Transportation Method Match code management
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG12     LIKE ZSIMIMG12 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG12_ORG LIKE ZSIMIMG12 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG12_DEL LIKE ZSIMIMG12 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG12_OLD LIKE ZSIMIMG12 OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* INTERNAL TABLES for transport rate   => JSY 20020916
*----------------------------------------------------------------------*
DATA : IT_ZSIMIMG20     LIKE ZSIMIMG20 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG20_ORG LIKE ZSIMIMG20 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG20_DEL LIKE ZSIMIMG20 OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSIMIMG20_OLD LIKE ZSIMIMG20 OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* INTERNAL TABLES for In-land transportation => JSY 20020918
*----------------------------------------------------------------------*
DATA : IT_ZSTRHD        LIKE ZTTRHD  OCCURS 100 WITH HEADER LINE.
*--ITEM---*
DATA : IT_ZSTRIT        LIKE ZSTRIT  OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSTRIT_ORG    LIKE ZSTRIT  OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSTRIT_DEL    LIKE ZSTRIT  OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSTRIT_OLD    LIKE ZSTRIT  OCCURS 100 WITH HEADER LINE.
*--In-land transportation fee --*
DATA : IT_ZSTRCST       LIKE ZSTRCST OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSTRCST_ORG   LIKE ZSTRCST OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSTRCST_DEL   LIKE ZSTRCST OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSTRCST_OLD   LIKE ZSTRCST OCCURS 100 WITH HEADER LINE.
*--transportation fee detail-*
DATA : IT_ZSTRCSTIT       LIKE ZSTRCSTIT OCCURS 100 WITH HEADER LINE.
DATA : IT_ZTTRCSTIT       LIKE ZTTRCSTIT OCCURS 100 WITH HEADER LINE.
DATA : IT_ZTTRCSTIT_ORG   LIKE ZTTRCSTIT OCCURS 100 WITH HEADER LINE.
DATA : IT_ZTTRCSTIT_DEL   LIKE ZTTRCSTIT OCCURS 100 WITH HEADER LINE.
DATA : IT_ZTTRCSTIT_OLD   LIKE ZTTRCSTIT OCCURS 100 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Tables for Car of Emergency Bonded transportation
*----------------------------------------------------------------------*
DATA : IT_ZSBLUGC        LIKE ZSBLUGC OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSBLUGC_DEL    LIKE ZSBLUGC OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Invoice Item Internal Tables
*----------------------------------------------------------------------*
DATA : IT_ZSIVIT         LIKE ZSIVIT OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIVIT_DEL     LIKE ZSIVIT OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIVIT_TMP     LIKE ZSIVIT OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIVIT_ORG     LIKE ZSIVIT OCCURS 0 WITH HEADER LINE.

DATA : IT_ZSIVHST        LIKE ZSIVHST  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIVHST1       LIKE ZSIVHST1 OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Invoice Item Internal Tables
*----------------------------------------------------------------------*
DATA : IT_ZSCIVIT         LIKE ZSCIVIT OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSCIVIT_TMP     LIKE ZSCIVIT OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSCIVIT_ORG     LIKE ZSCIVIT OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSCIVIT_OLD     LIKE ZSCIVIT OCCURS 0 WITH HEADER LINE.

DATA : IT_ZSCIVHST        LIKE ZSCIVHST OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Clearance Invoice Item Internal Tables
*----------------------------------------------------------------------*
DATA : IT_ZSCUCLIVIT     LIKE ZSCUCLIVIT OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSCUCLIVIT_DEL LIKE ZSCUCLIVIT OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Clearance expense Internal Tables
*----------------------------------------------------------------------*
DATA : IT_ZSCUCLCST      LIKE ZSCUCLCST OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSCUCLCST_ORG  LIKE ZSCUCLCST OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSCUCLCST_DEL  LIKE ZSCUCLCST OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table for Customs Declaration(KR)
*----------------------------------------------------------------------*
DATA : IT_ZSIDRHS       LIKE ZSIDRHS OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDRHS_ORG   LIKE ZSIDRHS OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* Internal Table for Customs Declaration(KR)
*----------------------------------------------------------------------*
DATA : IT_ZSIDRHSD      LIKE ZSIDRHSD OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDRHSD_ORG  LIKE ZSIDRHSD OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDRHSD_S    LIKE ZSIDRHSD OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDRHSD_DEL  LIKE ZSIDRHSD OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* Internal Table for Customs Declaration(KR)
*----------------------------------------------------------------------*
DATA : IT_ZSIDRHSL      LIKE ZSIDRHSL OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDRHSL_ORG  LIKE ZSIDRHSL OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDRHSL_S    LIKE ZSIDRHSL OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Customs Declaration(US)
*----------------------------------------------------------------------*
DATA : IT_ZSIDRUSH      LIKE ZSIDRUSH OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDRUSH_ORG  LIKE ZSIDRUSH OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* Customs Declaration(US-HS Detail) Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSIDRUSD      LIKE ZSIDRUSD OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDRUSD_ORG  LIKE ZSIDRUSD OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDRUSD_SEL  LIKE ZSIDRUSD OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Customs Clearance(US)
*----------------------------------------------------------------------*
DATA : IT_ZSIDSUSH      LIKE ZSIDSUSH OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDSUSH_ORG  LIKE ZSIDSUSH OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* Customs Declaration(US-HS Detail) Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSIDSUSD      LIKE ZSIDSUSD OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDSUSD_ORG  LIKE ZSIDSUSD OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDSUSD_SEL  LIKE ZSIDSUSD OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Internal Table for Customs Clearance(KR)
*----------------------------------------------------------------------*
DATA : IT_ZSIDSHS       LIKE ZSIDSHS OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDSHS_ORG   LIKE ZSIDSHS OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* Internal Table for Customs clearance(KR)
*----------------------------------------------------------------------*
DATA : IT_ZSIDSHSD      LIKE ZSIDSHSD OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDSHSD_ORG  LIKE ZSIDSHSD OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDSHSD_S    LIKE ZSIDSHSD OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDSHSD_DEL  LIKE ZSIDSHSD OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* Internal Table for Customs clearance(KR)
*----------------------------------------------------------------------*
DATA : IT_ZSIDSHSL      LIKE ZSIDSHSL OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDSHSL_ORG  LIKE ZSIDSHSL OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDSHSL_S    LIKE ZSIDSHSL OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table for license of tax deduction
*----------------------------------------------------------------------*
DATA : IT_ZSIDRCR       LIKE ZSIDRCR OCCURS 0 WITH HEADER LINE.
DATA : IT_TAB       LIKE ZSIDRCR OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* Internal Table for license of tax deduction
*----------------------------------------------------------------------*
DATA : IT_ZSIDRCRIT     LIKE ZSIDRCRIT OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDRCRIT_S   LIKE ZSIDRCRIT OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* Internal Table for license of tax deduction
*----------------------------------------------------------------------*
DATA : IT_ZSIDRDTU      LIKE ZSIDRDTU OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDRDTU_DEL  LIKE ZSIDRDTU OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Payment Notice Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSPMTIV       LIKE ZSPMTIV  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZTVTIV        LIKE ZTVTIV   OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSPMTIV_ORG   LIKE ZSPMTIV  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSPMTIV_DEL   LIKE ZSPMTIV  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSPMTHST      LIKE ZSPMTHST OCCURS 10 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tax Invoice Item Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSVTIVIT      LIKE ZSVTIVIT OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tax Invoice Segment 3 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSVTSG3       LIKE ZSVTSG3  OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Certi.of clear goods Segment 1 Internal Table
*----------------------------------------------------------------------*
DATA : IT_ZSREDSG1      LIKE ZSREDSG1 OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table of Insurance goods description
*----------------------------------------------------------------------*
DATA : IT_ZSINSSG2      LIKE ZSINSSG2  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSSG2_ORG  LIKE ZSINSSG2  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSSG2_DEL  LIKE ZSINSSG2  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSSG2_OLD  LIKE ZSINSSG2  OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table for additional Conditions
*----------------------------------------------------------------------*
DATA : IT_ZSINSAGR      LIKE ZSINSAGR  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSAGR_ORG  LIKE ZSINSAGR  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSAGR_DEL  LIKE ZSINSAGR  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSAGR_OLD  LIKE ZSINSAGR  OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table for insurance
*----------------------------------------------------------------------*
DATA : IT_ZSINSSG5      LIKE ZSINSSG5  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSSG5_ORG  LIKE ZSINSSG5  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSSG5_DEL  LIKE ZSINSSG5  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSSG5_OLD  LIKE ZSINSSG5  OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Internal Table( BL base ) of Insurance
*----------------------------------------------------------------------*
DATA : IT_ZSINSBSG2      LIKE ZSINSBSG2  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSBSG2_ORG  LIKE ZSINSBSG2  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSBSG2_DEL  LIKE ZSINSBSG2  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSBSG2_OLD  LIKE ZSINSBSG2  OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*  Internal Table( BL base ) of Insurance
*----------------------------------------------------------------------*
DATA : IT_ZSINSBAGR      LIKE ZSINSBAGR  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSBAGR_ORG  LIKE ZSINSBAGR  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSBAGR_DEL  LIKE ZSINSBAGR  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSBAGR_OLD  LIKE ZSINSBAGR  OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*  Internal Table( BL base ) of Insurance
*----------------------------------------------------------------------*
DATA : IT_ZSINSBSG5      LIKE ZSINSBSG5  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSBSG5_ORG  LIKE ZSINSBSG5  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSBSG5_DEL  LIKE ZSINSBSG5  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSINSBSG5_OLD  LIKE ZSINSBSG5  OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* INTERNAL TABLE for Mothershipment Management
*----------------------------------------------------------------------*
DATA : IT_ZSMSIT      LIKE ZSMSIT  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSMSIT_ORG  LIKE ZSMSIT  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSMSIT_DEL  LIKE ZSMSIT  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSMSIT_OLD  LIKE ZSMSIT  OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* INTERNAL TABLE for Mothershipment expense
*----------------------------------------------------------------------*
DATA : IT_ZSMSCST      LIKE ZSMSCST  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSMSCST_ORG  LIKE ZSMSCST  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSMSCST_DEL  LIKE ZSMSCST  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSMSCST_OLD  LIKE ZSMSCST  OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* INTERNAL TABLE for Mothership's Name
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_MSNM OCCURS 0,
      ZFMSNM   LIKE ZTMSHD-ZFMSNM,
END OF IT_MSNM.

*----------------------------------------------------------------------*
* INTERNAL TABLE for Mothershipment Header
*----------------------------------------------------------------------*
DATA : IT_ZSMSHD      LIKE ZSMSHD  OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
DATA : IT_ZSIEPORT      LIKE ZSIEPORT  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIEPORT_ORG  LIKE ZSIEPORT  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIEPORT_DEL  LIKE ZSIEPORT  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIEPORT_OLD  LIKE ZSIEPORT  OCCURS 0 WITH HEADER LINE.

DATA : IT_ZSIMIMG23      LIKE ZSIMIMG23  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG23_ORG  LIKE ZSIMIMG23  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG23_DEL  LIKE ZSIMIMG23  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIMIMG23_OLD  LIKE ZSIMIMG23  OCCURS 0 WITH HEADER LINE.

DATA : IT_CURR  LIKE ZSTCURR  OCCURS 0 WITH HEADER LINE,
       IT_TCURR LIKE TCURR    OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Internal Table for Customs Broker
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_ZFCUT_TAB OCCURS 0,
      NAME1    LIKE LFA1-NAME1,
      ZFVEN    LIKE ZTIMIMG10-ZFVEN,
      ZFCUT    LIKE ZTIMIMG10-ZFCUT,
END OF IT_ZFCUT_TAB.
*-----------------------------------------------------------------------

*----------------------------------------------------------------------*
* Internal Table for Duty, Taxes....
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_ZSIDSHSD_CAL OCCURS 0,
      ZFBLNO    LIKE  ZTIDSHSD-ZFBLNO,
      ZFCLSEQ   LIKE  ZTIDSHSD-ZFCLSEQ,
      ZFCONO    LIKE  ZTIDSHSD-ZFCONO,
      ZFRONO    LIKE  ZTIDSHSD-ZFRONO,
      ZFQNT     LIKE  ZTIDSHSD-ZFQNT,
      ZFBLIT    LIKE  ZTIVIT-ZFBLIT,
      ZFREQNO   LIKE  ZTIVIT-ZFREQNO,
      ZFITMNO   LIKE  ZTIVIT-ZFITMNO,
      ZFEXRT    LIKE  ZTIV-ZFEXRT,
      FFACT     LIKE  ZTIV-FFACT,
      ZFKRWAMT  LIKE  ZTINS-ZFKRWAMT,
      BLMENGE   LIKE  ZTBLIT-BLMENGE,
      ZFCNF     LIKE  ZTIDS-ZFCUAMTS,
      ZFCNFU    LIKE  ZTIDS-ZFCUAMTS,
      ZFINS     LIKE  ZTIDS-ZFCUAMTS,
      ZFGAM     LIKE  ZTIDS-ZFCUAMTS,
      ZFCUAMT   LIKE  ZTIDS-ZFCUAMTS,
      ZFCUAMTU  LIKE  ZTIDS-ZFCUAMTS,
      ZFTAX     LIKE  ZTIDS-ZFCUAMTS,
      ZFVAT     LIKE  ZTIDS-ZFCUAMTS,
      BPUMZ     LIKE  EKPO-BPUMZ,
      BPUMN     LIKE  EKPO-BPUMN,
      STAWN     LIKE  ZTIVIT-STAWN,
      ZFBASIC   LIKE  ZTIMIMG09-ZFBASIC,
END OF IT_ZSIDSHSD_CAL.

DATA: BEGIN OF IT_ZFREQNO  OCCURS 50,
      ZFREQNO  LIKE ZTREQHD-ZFREQNO,
      ZFBLNO   LIKE ZTBL-ZFBLNO,
      LOCKED,
END OF IT_ZFREQNO.

DATA: BEGIN OF IT_IV_LOCKED  OCCURS 50,
      ZFBLNO   LIKE ZTBL-ZFBLNO,
      LOCKED,
END OF IT_IV_LOCKED.

*----------------------------------------------------------------------
* INTERNAL TABLE for SERVICE ENTRY DATA GET
*----------------------------------------------------------------------
DATA : BEGIN OF IT_TEMP OCCURS  50,
       BELNR    LIKE  EKBE-BELNR.
DATA : END   OF IT_TEMP.

DATA: BEGIN OF IT_LOCKED  OCCURS 50,
      ZFREQNO  LIKE ZTREQHD-ZFREQNO,
      ZFBLNO   LIKE ZTBL-ZFBLNO,
      LOCKED,
END OF IT_LOCKED.

DATA : WZTTAXBKHD LIKE ZTTAXBKHD.

*-----------------------------------------------------------------------
* Internal Table  for Menu Status Function Inactive
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_EXCL OCCURS 20,
      FCODE    LIKE RSMPE-FUNC.
DATA: END   OF IT_EXCL.

DATA: PROGRAM LIKE SY-CPROG,
      DYNPRO  LIKE SY-DYNNR,
      DYNSUB  LIKE SY-DYNNR,
      DYNSUB1 LIKE SY-DYNNR.

*-----------------------------------------------------------------------
* FIELD SYMBOLS DEFINE
*-----------------------------------------------------------------------
FIELD-SYMBOLS : <FS_F>,
                <FS_F2>,
                <FS_REF>,
                <FS_CHK>,
                <FS_DOC>.

*-----------------------------------------------------------------------
* P/O Document Type -> Constant Define
*-----------------------------------------------------------------------
DATA : C_INFO VALUE 'I',         " Info-record
       C_ORDR VALUE 'W',         " source list
       C_BANF VALUE 'B',         " P/O Request
       C_BEST VALUE 'F',         " P/O
       C_ANFR VALUE 'A',         " RFQ
       C_KONT VALUE 'K',         " All Contract
       C_LFPL VALUE 'L',         " delivery Plan
       C_LERF VALUE 'Q'.         " Service entry

*-----------------------------------------------------------------------
* Import system function -> constant
*-----------------------------------------------------------------------
DATA : C_REQ_C  VALUE 'C',         " Create
       C_REQ_U  VALUE 'U',         " Change
       C_REQ_D  VALUE 'D',         " Display
       C_ADD_U  VALUE 'A',         " Additional Change
       C_ADD_D  VALUE 'B',         " Display
       C_OPEN_C VALUE 'O',         " Confirm
       C_OPEN_U VALUE 'G',         " Status Change
       C_OPEN_D VALUE 'R',         " Confirm Display
       C_INSU_I VALUE 'I',         " Insurance
       C_BL_SEND VALUE 'S',        " Shipping Document Send
       C_BL_COST VALUE 'T',        " Expense Input
       C_BL_REAL VALUE 'E'.        " Real Arrived Date Input

*-----------------------------------------------------------------------
* FIELD DEFINE
*-----------------------------------------------------------------------
DATA : W_LOOPLINES      LIKE SY-LOOPC,      " loop counter
       W_LOOPLINES2     LIKE SY-LOOPC,      " loop counter
       W_COUNTER        LIKE SY-LOOPC,      " counter
       W_COUNTER1       LIKE SY-LOOPC,      " counter
       W_COUNTER2       LIKE SY-LOOPC,      " counter
       W_COUNTER3       LIKE SY-LOOPC,      " counter
       W_COUNTER4       LIKE SY-LOOPC,      " counter
       W_CNT            LIKE SY-LOOPC,      " counter
       RADIO_NONE       TYPE C,             " BDC MODE('N')
       RADIO_ALL        TYPE C,             " BDC MODE('A')
       RADIO_ERROR      TYPE C,             " BDC MODE('E')
       W_LINE           LIKE SY-TABIX,      " TABLE INDEX
       W_LINE1          LIKE SY-TABIX,      " TABLE INDEX
       W_LINE2          LIKE SY-TABIX,      " TABLE INDEX
       W_SELECTED_LINE  LIKE SY-TABIX,      " TABLE INDEX
       W_LINE0103       LIKE SY-TABIX,      " TABLE INDEX
       W_TABIX          LIKE SY-TABIX,      " TABLE INDEX
       W_LOOP_CNT       TYPE I,             " loop counter
       W_PROC_CNT       TYPE I,             " Process Counter
       W_OK_CODE        LIKE SY-UCOMM,      " OK-CODE
       W_OLDOK_CODE     LIKE SY-UCOMM,      " OK-CODE
       OK-CODE          LIKE SY-UCOMM,      " OK-CODE
       W_STATUS         TYPE C,             " MENU STATUS
       W_BUKRS          LIKE ZTIMIMGTX-BUKRS,
       W_BUTXT          LIKE T001-BUTXT,    " Company Name
       W_BUTXT1         LIKE T001-BUTXT,    " Company Name
       W_ZFCDNM         LIKE ZTIMIMG08-ZFCDNM,
       W_NEW_STATUS     TYPE C,             " MENU STATUS
       W_OLD_STATUS     TYPE C,             " MENU STATUS
       W_ROW_MARK       TYPE C,             " RECORD Select Yes/No
       W_ROW_MARK1      TYPE C,             " RECORD Select Yes/No
       W_ROW_MARK2      TYPE C,             " RECORD Select Yes/No
       W_DEL_MARK       TYPE C,             " RECORD Select Yes/No
       W_PFSTAT(4)      TYPE C,             " PF-STATUS
       G_PARAM_LINE     LIKE SY-TABIX,      " TABLE Index
       LZTBLUG-ZFHBLNO  TYPE I,             " Line
       F(20)            TYPE C,             " Field Name Alias
       W_F(20)          TYPE C,             " Field Name Alias
       DY_FIELD(21)     TYPE C,             " f? GET CURSOR
       DY_LINE          LIKE SY-STEPL,      " f? GET CURSOR
       F4HLP            LIKE DYNPREAD OCCURS 1 WITH HEADER LINE,
       ZTERM            LIKE T052S-ZTERM,   " ausgew?lte Zahlungsbed.
       STATUS           TYPE STATE_VECTOR,  " Type Alias
       XSHOW(1)         TYPE C,             " Kennz.: Nur Anzeige ?
       OPTION(1)        TYPE C,             " popup Screen에서 사?
       ANTWORT(1)       TYPE C,             " popup Screen에서 사?
       CANCEL_OPTION    TYPE C,             " popup Screen에서 사?
       TEXTLEN          TYPE I,             " popup Screen에서 사?
       W_BSTYP          LIKE ZTIMIMG01-BSTYP,
       W_BSART          LIKE ZTIMIMG01-BSART,
       W_ZTERM          LIKE ZTIMIMG01-ZTERM,
       W_ITEM_CNT       LIKE SY-TABIX,
       W_AMOUNT         LIKE ZTREQHD-ZFLASTAM,
       W_ITEM_AMOUNT    LIKE ZTREQHD-ZFLASTAM,
       W_AMOUNT1        LIKE ZTREQHD-ZFLASTAM,
       W_AMOUNT2        LIKE ZTREQHD-ZFLASTAM,
       W_TOT_AMOUNT     LIKE ZTREQHD-ZFLASTAM,
       W_TOT_AMOUNT1    LIKE ZTREQHD-ZFLASTAM,
       W_PO_AMOUNT      LIKE ZTREQHD-ZFLASTAM,
       W_LOCAL_AMT      LIKE ZTREQHD-ZFLASTAM,
       W_LOCAL_AMT1     LIKE ZTREQHD-ZFLASTAM,
       W_BAPICURR_FR    LIKE BAPICURR-BAPICURR,
       W_BAPICURR_TO    LIKE BAPICURR-BAPICURR,
       W_BAPICURR_RATE  LIKE BAPICURR-BAPICURR,
       W_ZFCSCD         LIKE ZTIMIMG08-ZFCD,
       W_ETA_FROM       LIKE ZTMSHD-ZFETA,
       W_ETA_TO         LIKE ZTMSHD-ZFETA,
       W_POYNY          TYPE I,
       W_POYNN          TYPE I,
       W_SAMPLE         TYPE C,
       W_ZFPOTY         LIKE ZTBL-ZFPOTY,
       W_LIFNR_NM(28)   TYPE C,
       W_ZFBENI_NM(28)  TYPE C,
       W_LLIEF_NM(28)   TYPE C,
       W_ZFFORD_NM(28)  TYPE C,
       W_ZFTRCK_NM(28)  TYPE C,
       W_ZFMAVN_NM(28)  TYPE C,
       W_ZFPHVN_NM(28)  TYPE C,
       W_ZFOPBN_NM(28)  TYPE C,
       W_ZFLEVN_NM(28)  TYPE C,
       W_ZFPNBN_NM(28)  TYPE C,
       W_MSGV1          LIKE SY-MSGV1,
       W_TOT_FREIGHT    LIKE ZTIV-ZFIVAMT,
       W_PLANED_FREIGHT LIKE ZTIV-ZFIVAMT,
       W_SHAMT         LIKE ZTIV-ZFIVAMT,
       W_TMPAMT        LIKE ZTIV-ZFIVAMT,
       W_UBLAMT        LIKE ZTIV-ZFIVAMT,
       W_ZFPKCHG       LIKE ZTIV-ZFPKCHG,
       W_ZFHDCHG       LIKE ZTIV-ZFHDCHG,
       W_TEMP_MENGE    LIKE ZTIVIT-CCMENGE,
       W_OPEN_NM(28)   TYPE C,
       DIGITS          TYPE I VALUE 20,
       TMP_ZIFRMAAMT   LIKE BAPICURR-BAPICURR,
       W_OLD_SUBRC     LIKE SY-SUBRC,
       W_ZFBTSEQ       LIKE ZTBLINOU-ZFBTSEQ,
       W_GUBUN         TYPE C,
       W_ZFGUBN        LIKE ZTIMIMG23-ZFGUBN,
       W_MODIF_BIT     TYPE C,
       W_LFA1          LIKE LFA1,
       W_ADRC          LIKE ADRC,
       W_ZFWERKS       LIKE ZTREQHD-ZFWERKS,
       W_ZFMAUD        LIKE ZTREQHD-ZFMAUD,
       W_BEDNR         LIKE EKPO-BEDNR,         " Requirement tracking n
       W_MARK          TYPE C,
       W_AMEND_MARK    TYPE C,
       W_SY_SUBRC      LIKE SY-SUBRC,
       W_INSEQ         LIKE ZTINSB-ZFINSEQ,
       W_SEL_MAT_CNT   TYPE I,
       W_ZSREQIT       LIKE ZSREQIT,
       MM03_START_SICHT(15) TYPE C  VALUE 'BDEKLPQSVXZA',
       W_ZFREQNO            LIKE ZSREQHD-ZFREQNO,
       W_ZFAMDNO            LIKE ZSREQHD-ZFAMDNO,
       W_MENGE              LIKE ZSREQIT-MENGE,
       W_MENGE1             LIKE ZSREQIT-MENGE,
       W_MENGE2             LIKE ZSREQIT-MENGE,
       W_MENGE3             LIKE ZSREQIT-MENGE,
       W_MENGE4             LIKE ZSREQIT-MENGE,
       W_OLD_MENGE          LIKE ZSREQIT-MENGE,
       W_INBOUND_QTY        LIKE ZSREQIT-MENGE,
       W_INBOUND_TOT        LIKE ZSREQIT-MENGE,
       W_AMDNO              LIKE ZSREQHD-ZFAMDNO,
       W_ZFCIVRN            LIKE ZTCIVHD-ZFCIVRN,
       W_ZFITMNO            LIKE ZTREQIT-ZFITMNO,
       W_COUNT              TYPE I,
       W_COUNT1             TYPE I,
       W_MONETARY           TYPE I,
       W_IV_LINE            TYPE I,
       W_ZSREQHD            LIKE ZSREQHD,
       W_EBELN              LIKE EKKO-EBELN,
       W_MAX_ETA            LIKE ZTBL-ZFETA,
       W_ZFINSEQ            LIKE ZSREQHD-ZFINSEQ,
       W_ZFOPNNO            LIKE ZTREQST-ZFOPNNO,
       W_ZFTBNO             LIKE ZSREQHD-ZFTBNO,
       W_ZFREQTY            LIKE ZTREQHD-ZFREQTY,
       W_ZFLCKN             LIKE ZTREQHD-ZFLCKN,
       W_REQ_CNT            TYPE I,
       W_ERR_CHK            TYPE C,
       W_RATE               LIKE EKKO-WAERS,
       W_FIXED_RATE         LIKE TCURS-SPRED,
       W_ZFLSG7O            LIKE ZTMLCSG7O-ZFLSG7O,
       W_ZFLSG7O1           LIKE ZTMLCSG7O-ZFLSG7O,
       W_ZFLSG7G            LIKE ZTMLCSG7G-ZFLSG7G,
       W_DESC_TEXT(65),
       W_MOD             TYPE   I,
       W_TEXT_18(18)     TYPE   C,
       W_TEXT_17(17)     TYPE   C,
       W_TEXT1_18(18)    TYPE   C,
       W_TEXT_5(05)      TYPE   C,
       W_TXZ01           LIKE   EKPO-TXZ01,
       W_MATNR           LIKE   EKPO-MATNR,
       W_ZFMID           LIKE   ZTIMIMG24-ZFMID,
       W_NEW_DOCST       LIKE   ZTREQST-ZFDOCST  VALUE 'O',
       W_ZTIMIMG08       LIKE   ZTIMIMG08,
       W_ZFDHENO         LIKE   ZTDHF1-ZFDHENO,
       W_ZFCDDOC         LIKE   ZTCDF1-ZFCDDOC,
       W_ZFDHSRO         LIKE   ZTDHF1-ZFDHSRO,
       W_ZFDHREF         LIKE   ZTDHF1-ZFDHREF,
       W_ZFOPNDT         LIKE   ZTREQST-ZFOPNDT,
       W_ZFIVAMT_D       LIKE   BAPICURR-BAPICURR,
       W_ZFIVAMT_S       LIKE   ZTIV-ZFIVAMT,
       W_ZFIVAMT_T       LIKE   ZTIV-ZFIVAMT,
       W_ZFIVDNO_M       LIKE   ZTIVIT-ZFIVDNO,
       W_ZFIVAMP_S       LIKE   ZTIV-ZFIVAMT,
       W_ZFDAMT_S        LIKE   ZTIV-ZFDAMT,
       W_ZFUPCST_S       LIKE   ZTIV-ZFUPCST,
       W_ZFPCST_S        LIKE   ZTIV-ZFPCST,
       W_ZFIMOC_S        LIKE   ZSIVIT-ZFIMOC,
       W_ZFIMOC_SUM      LIKE   ZSIVIT-ZFIMOC,
       W_ZFIVAMK_S       LIKE   ZSIVIT-ZFIVAMK,
       W_ZFIVAMP         LIKE   ZTIV-ZFIVAMT,
       W_ZFIVAMT         LIKE   BAPICURR-BAPICURR,
       LINE              TYPE   I,
       LINE2             TYPE   I,
       LINE_PURSG4       TYPE   I,
       W_LINE_0109       TYPE   I,
       W_LINE_0109_1     TYPE   I,
       W_USD             LIKE   EKKO-WAERS  VALUE  'USD',
       W_KRW             LIKE   EKKO-WAERS  VALUE  'USD',
       W_TEXT40(40)      TYPE   C,
       W_TEXT20(20)      TYPE   C,
       W_TEXT18(18)      TYPE   C,
       W_TEXT13(13)      TYPE   C,
       W_TOT_ITEM        LIKE   ZTREQIT-MENGE,
       W_DATE            LIKE   SY-DATUM,
       W_CDATE(10)       TYPE   C,
       W_CHR_ETA(10)     TYPE   C,
       W_TITLE           TYPE   SPOP-TEXTLINE1,
       W_ETA_CHA         TYPE   I,
       W_MAX_MSNO        LIKE   ZTMSHD-ZFMSNO.
RANGES  R_TERM    FOR    ZTMSHD-ZFETA    OCCURS 10.

DATA : RADIO_HBL(1)      TYPE C,
       RADIO_BLN(1)      TYPE C.
DATA : RADIO_PO(1)       TYPE C,
       RADIO_LC(1)       TYPE C,
       RADIO_RQ(1)       TYPE C,
       RADIO_NP(1)       TYPE C,
       RADIO_NPO(1)      TYPE C,
       RADIO_BDCA(1)     TYPE C,
       RADIO_BDCN(1)     TYPE C,
       RADIO_BDCE(1)     TYPE C,
       STANDARD_UP(1)    TYPE C,
       W_VENDOR_NM(28)   TYPE C,
       W_FWDR_NM(28)     TYPE C,
       W_HAYEK_NM(28)    TYPE C,
       W_TRUK_NM(28)     TYPE C,
       W_ZFBNARM(35)     TYPE C,
       W_ZFINRC_NM(14)   TYPE C,
       W_ORIGIN_NM(15)   TYPE C,
       W_ORIGIN_NM1(15)  TYPE C,
       W_CARGO_TYPE(10)  TYPE C,
       W_IMGR_NM(24)     TYPE C,
       W_KUNWE_NM(30)    TYPE C,
       W_KUNNR_NM(30)    TYPE C,
       ZTBL_ZFIVAMC      LIKE ZTBL-ZFBLAMC,
       ZTBL_ZFIVAMT      LIKE ZTBL-ZFBLAMT,
       W_TMP_TEXT(70)    TYPE C,
       W_HBLNO           LIKE ZSREQHD-ZFHBLNO,
       W_BLNO            LIKE ZTBL-ZFBLNO,
       W_LGSEQ           LIKE ZTLG-ZFLGSEQ,
       W_ZFDOCNO         LIKE ZTBL-ZFDOCNO,
       W_VIA             LIKE ZTBL-ZFVIA,
       WL_VIA            LIKE ZTBL-ZFVIA,
       WZ_VIA            TYPE C,
       W_BUTTON_ANSWER   TYPE C,
       W_ZFPORT          LIKE ZTIMIMG05-ZFPORT,
       W_ZFBNARCD        LIKE ZTIMIMG05-ZFBNARCD,
       W_ZFTRANS         LIKE ZTIMIMG05-ZFTRANS,
       W_ZFCOSTCD        LIKE ZTIMIMG05-ZFCOSTCD,
       W_ZFAPLDT         LIKE ZTIMIMG06-ZFAPLDT,
       W_STAWN           LIKE ZTIMIMG09-STAWN,
       W_ZFCUT           LIKE ZSIMIMG10-ZFCUT,    "
       W_ZFENTP          LIKE ZSIMIMG07-ZFENTP,    "
       W_ZFSTDOC         LIKE ZSIMIMG07-ZFSTDOC,
       W_ZFSTAMT         LIKE ZSIMIMG07-ZFSTAMT,
       W_ZFSTHS          LIKE ZSIMIMG07-ZFSTHS,
       W_ZFADAMT         LIKE ZSIMIMG07-ZFADAMT,
       W_EXPVZ           LIKE ZSIMIMG12-EXPVZ,
       W_YEAR(2)         TYPE C,
       W_YYYY(4)         TYPE C,
       W_YYYYMMDD_FROM   LIKE ZTIDR-ZFIDWDT,
       W_YYYYMMDD_TO     LIKE ZTIDR-ZFIDWDT,
       W_ZFIDRNO         LIKE ZTIDR-ZFIDRNO,
       W_SEQ(5)          TYPE N,
       W_TMP(6)          TYPE N,
       W_TMP_1(1)        TYPE N,
       W_CHK(1)          TYPE N,
       INCLUDE(8)        TYPE C,             "
       X1                LIKE SY-CUCOL,      " X-Koordiante f? Popup
       X2                LIKE SY-CUCOL,      " X-Koordinate f? Popup
       Y1                LIKE SY-CUROW,      " Y-Koordinate f? Popup
       Y2                LIKE SY-CUROW,      " Y-Koordinate f? Popup
       TFILL             LIKE SY-TFILL,      " Anzahl Zeilen in Tabelle
       W_STATUS_CHK,
       W_ZFAMEND         LIKE ZTREQST-ZFAMDNO,
       W_ZTREQST         LIKE ZTREQST,
       W_DEL_NM(50)      TYPE C,
       W_ARR_NM(50)      TYPE C,
       W_ZFLGSEQ         LIKE ZTLGGOD-ZFLGSEQ,
       W_CAMT            LIKE ZTLG-ZFCIAM,
       W_TOT_MENGE       LIKE ZTBLIT-BLMENGE,
       W_ZFNTDT_FROM     LIKE ZTPMTHD-ZFNTDT,
       W_ZFNTDT_TO       LIKE ZTPMTHD-ZFNTDT,
       W_ZFPNBN_FROM     LIKE ZTPMTHD-ZFPNBN,
       W_ZFPNBN_TO       LIKE ZTPMTHD-ZFPNBN,
       W_ZFHBLNO_FROM    LIKE ZTPMTHD-ZFHBLNO,
       W_ZFHBLNO_TO      LIKE ZTPMTHD-ZFHBLNO,
       W_ZFOPNNO_FROM    LIKE ZTPMTHD-ZFOPNNO,
       W_ZFOPNNO_TO      LIKE ZTPMTHD-ZFOPNNO,
       W_ZFISNO_FROM     LIKE ZTPMTHD-ZFISNO,
       W_ZFISNO_TO       LIKE ZTPMTHD-ZFISNO,
       W_ZFBLNO          LIKE ZTBL-ZFBLNO,
       W_ZFIVAMT_SUM     LIKE ZTIV-ZFIVAMT,
       W_LAND1           LIKE ZTIMIMG17-LAND1,
       W_WKURS           LIKE ZTREQHD-WKURS,
       W_TEXT80(80)      TYPE C,
       W_FIELD_CHK       TYPE I,
       W_LINE2103        TYPE I,
       W_ZFHBLNO         LIKE ZTBLUG-ZFHBLNO,
       W_ZFBNARCD_NM     LIKE ZTIMIMG03-ZFBNARM,
       W_ZFPOSDT_FROM    LIKE ZTVTIV-ZFPOSDT,
       W_ZFPOSDT_TO      LIKE ZTVTIV-ZFPOSDT,
       W_ZFDODT_FROM     LIKE ZTVTIV-ZFDODT,
       W_ZFDODT_TO       LIKE ZTVTIV-ZFDODT,
       W_LIFNR_FROM      LIKE ZTVTIV-LIFNR,
       W_LIFNR_TO        LIKE ZTVTIV-LIFNR,
       W_EBELN_FROM      LIKE ZTVTIVIT-EBELN,
       W_EBELN_TO        LIKE ZTVTIVIT-EBELN,
       W_ZFUSQN_SUM      LIKE ZTIDRDTU-ZFUSQN,
       W_ZFGFDYR_FROM    LIKE ZTVTIV-ZFGFDYR,
       W_ZFGFDYR_TO      LIKE ZTVTIV-ZFGFDYR,
       W_ZFGFDNO_FROM    LIKE ZTVTIV-ZFGFDNO,
       W_ZFGFDNO_TO      LIKE ZTVTIV-ZFGFDNO,
       W_RERUN(1)        TYPE C,
       W_ZFIVAMC         LIKE ZTVTIVIT-ZFIVAMC,
       W_ZFKAMT_S        LIKE ZTVTIVIT-ZFKAMT,
       W_ZFKRW           LIKE ZTVTIVIT-ZFKRW,
       W_ZFCSQ_M         LIKE ZTCUCLCST-ZFCSQ,
       W_ZFCSQ           LIKE ZTBLCST-ZFCSQ,
       W_ZFAMT_S         LIKE ZTIDR-ZFSTAMT,
       CPOSITION         TYPE I,
       RD_MLC(1)         TYPE C,
       RD_DPDATT(1)      TYPE C,
       RD_LLC(1)         TYPE C,
       W_ZFOPNNO_M       LIKE ZTPMTHD-ZFOPNNO,
       W_ZFREQNO_M       LIKE ZTPMTHD-ZFREQNO,
       W_EBELN_D         LIKE ZTPMTHD-EBELN,
       W_ZFREQNO_D       LIKE ZTPMTHD-ZFREQNO,
       W_ZFOPNNO_L       LIKE ZTPMTHD-ZFOPNNO,
       W_ZFISNO_L        LIKE ZTPMTHD-ZFISNO,
       W_ZFREQNO_L       LIKE ZTPMTHD-ZFREQNO,
       W_ZFLSG3          LIKE ZTVTSG3-ZFLSG3,
       W_READ_CHK        TYPE C,
       W_ZFPNIT          LIKE ZTPMTIV-ZFPNIT,
       W_ZFPNAM          LIKE ZTPMTIV-ZFPNAM,
       W_ZFTIVAM         LIKE ZTPMTHD-ZFTIVAM,
       SV_ZFPNBN         LIKE ZTPMTHD-ZFPNBN,
       SV_ZFNTDT         LIKE ZTPMTHD-ZFNTDT,
       SV_ZFPNAMC        LIKE ZTPMTHD-ZFPNAMC,
       SV_ZFPNAM         LIKE ZTPMTHD-ZFPNAM,
       W_ZFIVPKHD        LIKE ZTIV-ZFIVAMT,
       W_ZFETA           LIKE SY-DATUM,
       W_ZFTRTEC         LIKE ZTBL-ZFTRTEC,
       W_SARCHIV         LIKE VDARL-SARCHIV,
       W_LOANRT          LIKE ZTPMTHD-ZFLPER1,
       W_CURRDEC         LIKE TCURX-CURRDEC,
       W_CURRDEC1        LIKE TCURX-CURRDEC,
       W_CURRDEC2        LIKE TCURX-CURRDEC,
       W_ZFCIVNO         LIKE ZTCIVHD-ZFCIVNO,
       W_ZFCGNO          LIKE ZTCGHD-ZFCGNO,
       W_ZFIVNO          LIKE ZTIV-ZFIVNO,
       W_ZFCAMT          LIKE ZTCUCLCST-ZFCAMT,
       W_ZFCAMTU         LIKE ZTCUCLCST-ZFCAMT,
       W_AMOUNT_CNF      LIKE ZTCUCLCST-ZFCAMT,
       W_ZFINAMT         LIKE ZTIDS-ZFINAMT,
       W_ZFTFA           LIKE ZTIDS-ZFTFA,
       W_ZFCAMTK         LIKE ZTCUCLCST-ZFCAMT,
       W_TOT_HM          LIKE ZTCUCLCST-ZFCAMT,
       W_TOT_MP          LIKE ZTCUCLCST-ZFCAMT,
       W_TOT_TAX         LIKE ZTCUCLCST-ZFCAMT,
       W_TOT_VAT         LIKE ZTCUCLCST-ZFCAMT,
       W_TOT_GAM         LIKE ZTCUCLCST-ZFCAMT,
       W_ZFUSQN          LIKE ZTIDRDTU-ZFUSQN,
       W_AREA_NM         LIKE ZTIMIMG08-ZFCDNM.

DATA:  W_UKURS_1         LIKE TCURR-UKURS,
       W_UKURS_2         LIKE TCURR-UKURS,
       W_EXCHR           LIKE RKB1K-EXCHR.

DATA: DYNPROG            LIKE SY-REPID,
      DYNNR              LIKE SY-DYNNR,
      WINDOW_TITLE(30)   TYPE C.

DATA : OBJECTCLASS       LIKE CDHDR-OBJECTCLAS.

DATA  W_CLUSTD           LIKE STXL-CLUSTD.
DATA : OBJECID           LIKE CDHDR-OBJECTID,
       OBJEKTID          LIKE CDHDR-OBJECTID.

*>> Unloading Quantity Check
DATA: BEGIN OF IT_CG_SUM OCCURS 0,
      ZFBLNO             LIKE ZTBLIT-ZFBLNO,
      ZFBLIT             LIKE ZTBLIT-ZFBLIT,
      CGMENGE            LIKE ZTCGIT-CGMENGE.
DATA :END OF IT_CG_SUM.

DATA: BEGIN OF IT_STXL               OCCURS 100.
        INCLUDE STRUCTURE STXL.
DATA: END OF IT_STXL.

DATA: BEGIN OF IT_W3HTML             OCCURS 200.
        INCLUDE STRUCTURE W3HTML.
DATA: END OF IT_W3HTML.

DATA : W_ZFTRT   LIKE ZTIDR-ZFTRT,
       W_ZFTRT_A LIKE ZTIDR-ZFTRT,
       W_ZFTRT_B LIKE ZTIDR-ZFTRT.

DATA: F_PASSWD(08).                    " Password
DATA: G_PASSWORD1        LIKE XU400-NEWCODE.
DATA: W_ZFTRME_TEXT      LIKE DD07T-DDTEXT.
DATA  ZTLLCAMHD_TMP      LIKE ZTLLCAMHD.
DATA: W_ZSBLCST_ZFCAMT1  LIKE ZTBLCST-ZFCAMT.
DATA: W_ZSBLCST_ZFCAMT2  LIKE ZTBLCST-ZFCAMT.
DATA: W_ZTBL_ZFTRTE      LIKE ZTBL-ZFTRTE.
DATA: W_APLDT            LIKE SY-DATUM.
DATA  W_TXT1(12)         TYPE C.
DATA  W_CUR(5)           TYPE C.
DATA  W_AMT_CHA          TYPE I.
*------ EDI
DATA  W_EDI_RECORD(65535).
*DATA  W_EDI_RECORD    LIKE    ZEDIFILE-ZFRECORD.

DATA: BEGIN OF IT_EDIFILE OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
      END OF IT_EDIFILE.

*----- ZBTXT_LINES ( Ausgabezeilen f? Zahlungsbedingungen ) ----------*
DATA:    BEGIN OF ZBTXT_LINES OCCURS 5,
           LINE(80),
         END OF ZBTXT_LINES.

DATA: W_ZTERM_TEXT(30)   TYPE C.
DATA: W_ZFCD_TEXT1(80),
      W_ZFCD_TEXT2(80),
      W_ZFCD_TEXT3(80),
      W_ZFCD_TEXT4(80),
      W_ZFCD_TEXT5(80).

* change document?
DATA: BEGIN OF IT_CDTXT               OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF IT_CDTXT.

DATA  W_GDATU    LIKE TCURR-GDATU.


RANGES: R_ZFRLST1 FOR ZTREQST-ZFRLST1 OCCURS 0,
        R_ZFRLST2 FOR ZTREQST-ZFRLST2 OCCURS 0.



* SAPScript Header Text Read?
CONSTANTS:
* BSTYP
  BSTYP-INFO VALUE 'I',
  BSTYP-ORDR VALUE 'W',
  BSTYP-BANF VALUE 'B',
  BSTYP-BEST VALUE 'F',
  BSTYP-ANFR VALUE 'A',
  BSTYP-KONT VALUE 'K',
  BSTYP-LFPL VALUE 'L',
  BSTYP-LERF VALUE 'Q',
* BSAKZ
  BSAKZ-NORM VALUE ' ',
  BSAKZ-TRAN VALUE 'T',
  BSAKZ-RAHM VALUE 'R',
* PSTYP
  PSTYP-LAGM VALUE '0',
  PSTYP-BLNK VALUE '1',
  PSTYP-KONS VALUE '2',
  PSTYP-LOHN VALUE '3',
  PSTYP-MUNB VALUE '4',
  PSTYP-STRE VALUE '5',
  PSTYP-TEXT VALUE '6',
  PSTYP-UMLG VALUE '7',
  PSTYP-WAGR VALUE '8',
  PSTYP-DIEN VALUE '9',
* Kzvbr
  KZVBR-ANLA VALUE 'A',
  KZVBR-UNBE VALUE 'U',
  KZVBR-VERB VALUE 'V',
  KZVBR-EINZ VALUE 'E',
  KZVBR-PROJ VALUE 'P',
* ESOKZ
  ESOKZ-PIPE VALUE 'P',
  ESOKZ-LOHN VALUE '3',
  ESOKZ-KONSI VALUE '2',               "konsi
  ESOKZ-CHARG VALUE '1',               "sc-jp
  ESOKZ-NORM VALUE '0'.


TABLES : THEAD,
         TLINE.

DATA:   BEGIN OF THEADTAB OCCURS 20.   "Header Langtext
        INCLUDE STRUCTURE THEAD.
DATA:    NEU,
         FIXIE,
         SELKZ,
         KOOBJECT LIKE THEAD-TDOBJECT, "Enjoy: Quellobjekt des Textes
         KOID     LIKE THEAD-TDID,     "Enjoy: Quellid des Textes
        END   OF THEADTAB.

DATA:   BEGIN OF HTEXT,
          TDOBJECT     LIKE THEAD-TDOBJECT,
          TDNAME       LIKE THEAD-TDNAME,
          TDID         LIKE THEAD-TDID,
          TDSPRAS      LIKE THEAD-TDSPRAS,
        END OF  HTEXT.

DATA:   BEGIN OF REFTEXT OCCURS 20.    "Header Langtext
        INCLUDE STRUCTURE THEAD.
DATA:   END   OF REFTEXT.

DATA:   BEGIN OF ANFTAB OCCURS 10,     "Tabelle der Anfragesprachen
           SPRAS LIKE SY-LANGU,
           EBELN LIKE EKKO-EBELN,
           LIFNR LIKE EKKO-LIFNR,
        END   OF ANFTAB.

DATA:   EKKO_ENTRIES      LIKE SY-TFILL.    "Anzahl gefundener Kopftexte
DATA:   GF_ARCHIVE_HANDLE LIKE SY-TABIX.

DATA:   BEGIN OF TLINETAB OCCURS 30.   "Zeilen Langtext
        INCLUDE STRUCTURE TLINE.
DATA:   END   OF TLINETAB.
DATA:   W_ZFIVAMT_EX      LIKE   BAPICURR-BAPICURR.


*>>>>> LIV BAPI FUNCTION?
TABLES : BAPI_INCINV_CREATE_HEADER,
         BAPI_INCINV_CREATE_ITEM,
         BAPI_INCINV_CREATE_TAX,
*-------------------------------------------------------------------
*> dreamland remark.
*>------------------------------------------------------------------
*        BAPI_INCINV_CREATE_WITHTAX,
*        BAPI_INCINV_CREATE_VENDORSPLIT,
*>------------------------------------------------------------------
         BAPIRET2.

DATA : HEADERDATA        LIKE    BAPI_INCINV_CREATE_HEADER,
       I_INVOICE         LIKE    RBKP-XRECH,
       I_CREDITMEMO      LIKE    RBKP-XRECH,
       INVOICEDOCNUMBER  LIKE    BAPI_INCINV_FLD-INV_DOC_NO,
       FISCALYEAR        LIKE    BAPI_INCINV_FLD-FISC_YEAR.

DATA:   BEGIN OF ITEMDATA OCCURS 0.   ">> ITEM .
        INCLUDE STRUCTURE   BAPI_INCINV_CREATE_ITEM.
DATA:   END   OF ITEMDATA.

DATA:   BEGIN OF TAXDATA OCCURS 0.   ">> TAX .
        INCLUDE STRUCTURE   BAPI_INCINV_CREATE_TAX.
DATA:   END   OF TAXDATA.

*-------------------------------------------------------------------
*> dreamland remark.
*-------------------------------------------------------------------
*DATA:   BEGIN OF WITHTAXDATA OCCURS 0.   ">> WITH TAX DATA
*        INCLUDE STRUCTURE   BAPI_INCINV_CREATE_WITHTAX.
*DATA:   END   OF WITHTAXDATA.
*
*DATA:   BEGIN OF VENDORITEMSPLITDATA OCCURS 0.   ">> VENDOR SPLIT 내역.
*        INCLUDE STRUCTURE   BAPI_INCINV_CREATE_VENDORSPLIT.
*DATA:   END   OF VENDORITEMSPLITDATA.
*-------------------------------------------------------------------
DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

DATA:   BEGIN OF XRETURN OCCURS 0.   ">> RETURN .
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF XRETURN.

TABLES : BAPIMEPOHEADER,
         BAPIMEPOHEADERX.

TABLES : BAL_S_DMSG.
*>>> ERROR
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
INCLUDE  STRUCTURE  BDCMSGCOLL.
DATA : ICON         LIKE BAL_S_DMSG-%_ICON,
       MESSTXT(255) TYPE C,
       ZFIVNO       LIKE ZTIV-ZFIVNO,
       ZFSEQ        LIKE ZSBLCST-ZFSEQ,
       ZFSUBSEQ     TYPE I.
DATA : END OF IT_ERR_LIST.

*>> Declaration Internal Table for Container Information.
DATA : BEGIN OF IT_CONTLST OCCURS 0,
       ZFBLNO      LIKE ZTBL-ZFBLNO,
       ZFBLIT      LIKE ZTBLIT-ZFBLIT,
       MATNR       LIKE ZTBLIT-MATNR,        " Material Number..
       TXZ01       LIKE ZTBLIT-TXZ01,        " Material Description..
       EBELN       LIKE EKKO-EBELN,          " P/O Number..
       EBELP       LIKE EKPO-EBELP,          " P/O Item..
       VBELN       LIKE LIKP-VBELN,          " Inbound Delivery Number..
       TRAID       LIKE LIKP-TRAID,          " KD Container Number..
       BORGR_GRP   LIKE LIKP-BORGR_GRP,      " Seal Number..
       KDMAT       LIKE LIPS-KDMAT,          " Case Number..
       LFIMG       LIKE LIPS-LFIMG,          " Actual Qty Delivered..
       VRKME       LIKE LIPS-VRKME.          " Sales Unit..
DATA : END OF IT_CONTLST.

DATA : BEGIN OF IT_CIVRN   OCCURS 0,
       ZFCIVNO      LIKE ZTCIVHD-ZFCIVNO.
DATA : END   OF IT_CIVRN.

DATA : BEGIN OF IT_ERR_GRP OCCURS 0,
       ZFSEQ        LIKE ZSBLCST-ZFSEQ,
       END OF IT_ERR_GRP.

DATA:   W_ZFMSNO   LIKE ZTMSHD-ZFMSNO,
        W_ZFMSSEQ  LIKE ZTMSIT-ZFMSSEQ,
        W_ZFAPRTC  LIKE ZTMSCST-ZFAPRTC.
DATA    W_HEAD_DEL TYPE C.
DATA    W_MAX_SEQ  LIKE ZTMSIT-ZFMSSEQ.
DATA    W_NAME     LIKE ZTMSHD-ZFMSNM.
DATA    W_EKGRP    LIKE ZTMSHD-EKGRP.
DATA    W_OK_GUBN  TYPE C.
DATA    W_DATA_CNT TYPE I.
DATA    W_BEZEI    LIKE T618T-BEZEI.
DATA    W_CHK_CNT TYPE I.
DATA    W_ZFINSU1  LIKE ZTINS-ZFINSU1.
DATA    W_ZFINSU2  LIKE ZTINS-ZFINSU2.
DATA    W_ZFOPCD   LIKE ZTINS-ZFOPCD.
DATA    W_ZFOPCD1(10)   TYPE     C.
DATA    W_ZFEDI    LIKE ZTINS-ZFEDI.
DATA    W_TOTAMT   LIKE ZTRECST-ZFUPCST.
DATA :  W_3100_FIRST    VALUE   'N',
        W_3101_FIRST    VALUE   'N',
        W_3500_FIRST    VALUE   'N',
        W_3501_FIRST    VALUE   'N',
        W_0810_FIRST    VALUE   'Y',
        W_0820_FIRST    VALUE   'Y',
        W_0830_FIRST    VALUE   'Y',
        W_9910_FIRST    VALUE   'Y',
        W_SCR_GUBUN.

DATA :  W_CIV_CHK,
        W_CG_CHK,
        W_CC_CHK.

DATA :  C_ZFODOC1(100)   TYPE C
        VALUE 'SHIPPING MARKS MUST SHOW LG CHEM'.

DATA :  TEMP_WRBTR(16),
        TEMP_KURSF(10),
        TEMP_BKTXT        LIKE BKPF-BKTXT,
        TEMP_MENGE(17),
        TEMP_BLART(2),
        TEMP_FNAM         LIKE BDCDATA-FNAM,
        TEMP_XBLNR(16),
        TEMP_ERFMG(17).
DATA    W_TOT_AMT LIKE ZTCGCST-ZFUPCST.
DATA    TRTYP.
DATA    W_ZFIMDNO LIKE ZTBKPF-ZFIMDNO.
DATA    W_DYNNR   LIKE SY-DYNNR.
DATA    W_LIFNR   LIKE LFA1-LIFNR.
DATA    W_TEXT70(70).


TYPES: IMIS_TYPE_C12(12) TYPE C,
       IMIS_TYPE_C24(24) TYPE C.

DATA:  W_TEXT12          TYPE IMIS_TYPE_C12,
       W_TEXT24          TYPE IMIS_TYPE_C24.
*DATA  BEGIN OF ZSCCHD-GJAHR OCCURS 1.
*
*DATA  END   OF ZSCCHD-GJAHR.
DATA  W_EXRATE           LIKE ZTREQHD-KURSF.
DATA  W_MAX_DATE         LIKE SY-DATUM.
DATA: W_ZFIDSDT          LIKE ZTIDS-ZFIDSDT.
DATA: W_EXIT_MODE,
      W_EDIT_CHECK.
DATA  XZTIMIMGTX         LIKE ZTIMIMGTX.

DATA: MATERIALDOCUMENT  TYPE    BAPI2017_GM_HEAD_RET-MAT_DOC,
      MATDOCUMENTYEAR   TYPE    BAPI2017_GM_HEAD_RET-DOC_YEAR,
      GR_DOC            TYPE    BAPI2017_GM_HEAD_RET.

DATA  W_FIRST_0100 VALUE   'Y'.

TABLES : TBSL,
         TBSLT,
         ZTBKPF,
         ZSBKPF,
         ZTBSEG,
        *ZTBKPF,
         ZSBSEG,
         ZTCCHD,
        *ZTCCHD,
         ZTCCIT,
         ZSCCIT,
        *ZSCCIT,
         ZSBDIVL,
         UF05A,
         BSIS,
         SKA1,
         SKB1,
         ZTBDIV,
         RF05A,
         ZSBDIV,
         ZTBHIS,
         ZSBHIS,
         TTXD,
         T003,
         T074U,
        *LFA1,
*--------> VIEW.
         LFB1,
         VF_KRED.


CONTROLS: TC_0100    TYPE TABLEVIEW USING SCREEN 0100.
CONTROLS: TC_2000    TYPE TABLEVIEW USING SCREEN 2000.

DATA:    BEGIN OF TAX,
           BUKRS             LIKE BSEG-BUKRS,    " Bulrs f. Steuer
           KALSM             LIKE T005-KALSM,    " Schema
           MWSKZ             LIKE BSEG-MWSKZ,    " St.Kennz. Kontokorr.
           FWSTE             LIKE BSET-FWSTE,    " Steuerbetrag FW
           HWSTE             LIKE BSET-HWSTE,    " Steuerbetrag HW
           SHKZG             LIKE BSET-SHKZG,    " S/H Kennz. Steuern
           XSTVR             LIKE BKPF-XMWST,    " Steuern im Steuerbild
           ZDEKR             LIKE BSEG-BUZEI,    " Erste Kontokorrent Zl
           H2STE             LIKE BSET-H2STE,    " Steuerbetrag Hw2
           H3STE             LIKE BSET-H3STE,    " Steuerbetrag Hw3
         END OF TAX.

DATA : IT_ZSBKPF     LIKE ZSBKPF OCCURS 10 WITH HEADER LINE.

*-----------------------------------------------------------------------
* Authority field define.
*-----------------------------------------------------------------------
DATA: F_BKPF_BLA(10)      TYPE C VALUE 'F_BKPF_BLA'.

DATA: ACT_HINZ(2)         TYPE C VALUE '01',
      CHAR_X                     VALUE 'X',
      EGRKZ               LIKE     T007A-EGRKZ.

DATA: W_KBETR             LIKE     KONP-KBETR,
      W_KBETR1            LIKE     KONP-KBETR,
      W_COST_TYPE         LIKE     DD07T-DDTEXT,
      W_CODE_TYPE(30),
      W_CHG_CHK,
      W_KONWA             LIKE     KONP-KONWA,
      W_WMWST             LIKE     ZTBKPF-WMWST,
      W_WMWST1            LIKE     ZTBKPF-WMWST.

DATA:  W_AMTTXT1(19),
       W_AMTTXT2(19),
       W_AMTTXT3(19),
       W_AMTLEN1 TYPE I,
       W_AMTLEN2 TYPE I,
       W_AMTLEN3 TYPE I.

DATA : TOP_LINE            LIKE SY-TABIX,
       POSI_GB(1)          TYPE C.

*----------------------------------------------------------------------*
* Offer Sheet Create
*----------------------------------------------------------------------*
DATA: BEGIN OF SM_REQHD OCCURS 0.
        INCLUDE STRUCTURE ZTREQHD.
DATA: END OF SM_REQHD.

* Import Request Item Internal Table Declaration.
DATA: BEGIN OF SM_REQIT OCCURS 0.
        INCLUDE STRUCTURE ZTREQIT.
DATA: END OF SM_REQIT.

* Data Declaration for Offer Sheet Creation.
DATA  G_BENIF_NM        LIKE LFA1-NAME1.
DATA: G_ADVBK_NM        LIKE LFA1-NAME1.
DATA: G_REQNO           LIKE ZTREQHD-ZFREQNO.
DATA: W_ORIG            LIKE ZTMLCSG7O-ZFORIG.
DATA: G_ORNM            LIKE T005T-LANDX.
DATA: G_SPRT            LIKE ZTREQHD-ZFSPRT.
DATA: G_APRT            LIKE ZTREQHD-ZFAPRT.
DATA: G_PSYN(20)        TYPE C.
DATA: G_TSYN(20)        TYPE C.
DATA: G_REQSD           LIKE ZTREQHD-ZFREQSD.
DATA: G_REQED           LIKE ZTREQHD-ZFREQED.
DATA: G_EBELN           LIKE ZTREQHD-EBELN.
DATA: G_REQTY           LIKE ZTREQHD-ZFREQTY.
DATA: G_BENI_ADD(40)    TYPE C.
DATA: G_ADVBK_ADD       LIKE T005U-BEZEI.
DATA: G_BENI_ADD1(40)   TYPE C.
DATA: G_ADVBK_ADD1      LIKE BNKA-STRAS.
DATA: G_ADVBK_ADD2      LIKE BNKA-ORT01.
DATA: G_ADVBK_ACCNO     LIKE LFBK-BANKN.
DATA: G_ADVBK_TELNO(40) TYPE C.
DATA: G_ADVBK_FAXNO(40) TYPE C.
DATA: G_INCO1           LIKE ZTMLCHD-INCO1.
DATA: G_INCP            LIKE ZTMLCHD-ZFINCP.
DATA: FM_NAME           TYPE RS38L_FNAM.
DATA: W_EREKZ           LIKE EKPO-EREKZ,
      W_REPOS           LIKE EKPO-REPOS,
      W_CONO            LIKE ZTIDRHS-ZFCONO,
      W_RONO            LIKE ZTIDRHSD-ZFRONO,
      W_DOWN_PAY        LIKE EKBE-DMBTR,
      W_MESSAGE1        LIKE SPOP-TEXTLINE1,
      W_MESSAGE2        LIKE SPOP-TEXTLINE2.

DATA : W_ZFOPNNO_TMP LIKE    ZTREQST-ZFOPNNO,
       W_SEQ_TMP(5)  TYPE    C,
       W_ZZBUSTYPE   TYPE    C,
       W_TYPE(2)     TYPE    C.

DATA : W_DDATE(10).
CONTROLS: TC_0230    TYPE TABLEVIEW USING SCREEN 0230.
*-----------------------------------------------------------------------
* Internal Table for use excel download
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_DOWN OCCURS 0,
       MATNR       LIKE   ZTIMIMG24-MATNR,
       MAKTX       LIKE   ZSIMIMG24-MAKTX,
       ZFMID       LIKE   ZTIMIMG24-ZFMID,
       ZFHMCVC     LIKE   ZTIMIMG24-ZFHMCVC,
       ZFHMCVN1    LIKE   ZTIMIMG24-ZFHMCVN1,
       ZFHMCVAD1   LIKE   ZTIMIMG24-ZFHMCVAD1,
       ZFHMCZCD    LIKE   ZTIMIMG24-ZFHMCZCD,
       ZFHMCITY    LIKE   ZTIMIMG24-ZFHMCITY,
       ZFHMCOUNTRY LIKE   ZTIMIMG24-ZFHMCOUNTRY,
       CDAT        LIKE   ZTIMIMG24-CDAT,
       ERNAM       LIKE   ZTIMIMG24-ERNAM,
       UDAT        LIKE   ZTIMIMG24-UDAT,
       UNAME       LIKE   ZTIMIMG24-UNAME.
DATA : END OF IT_DOWN.
DATA  W_MAX_ITEMS(3) TYPE N.
