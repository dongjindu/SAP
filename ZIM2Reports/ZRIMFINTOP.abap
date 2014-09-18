*----------------------------------------------------------------------*
*   INCLUDE ZRIMFINTOP                                                 *
*----------------------------------------------------------------------*


TABLES: ZTFINHD,
        ZTFINIT,
        ZTIMIMG00,
        ZTIMIMG08,
        ZTREQHD,
        ZTREQST,
        ZTBKPF,
        ZSBSEG,
        SPOP,
        ZTIMIMG11,
        ZTBL,
        BKPF,
        *LFA1,
        LFA1,
        VF_KRED,
        tbsl,
        t074u,
        tbslt,
        T007A,
        KONP,
        BAPICURR,
        T163C,
        T052.


DATA: W_ERR_CHK(1),
      W_LINE           TYPE I,
      W_COUNT          TYPE I,
      LINE             TYPE   I,
      W_DV_CT(1),
      MARKFIELD,
      W_DOM_TEX1       LIKE DD07T-DDTEXT,
      W_DOM_TEX2       LIKE DD07T-DDTEXT,
      W_DOM_TEX3       LIKE DD07T-DDTEXT,
      W_COST_TYPE      LIKE DD07T-DDTEXT,
      W_CODE_TYPE(30),
      WINDOW_TITLE(30) TYPE C,
      DYNPROG            LIKE SY-REPID,
      DYNNR              LIKE SY-DYNNR,
      W_PROC_CNT       TYPE I,               " 처리건수.
      W_SELECTED_LINES TYPE P,               " 선택 LINE COUNT
      W_TABIX          LIKE SY-TABIX,
      W_PAGE           TYPE I,
      W_MOD            TYPE   I,
      W_ERR_MODE,
      W_SUBRC          LIKE SY-SUBRC,
      F(20)            TYPE C,             " Field Name Alias
      INCLUDE(8)       TYPE C,
      W_LOOPLINES      LIKE SY-LOOPC,      " loop counter
      OPTION(1)        TYPE C,             " 공통 popup Screen에서 사?
      ANTWORT(1)       TYPE C,             " 공통 popup Screen에서 사?
      CANCEL_OPTION    TYPE C,             " 공통 popup Screen에서 사?
      TEXTLEN          TYPE I,             " 공통 popup Screen에서 사?
      W_ROW_MARK       TYPE C,             " RECORD의 선택여?
      G_PARAM_LINE     LIKE SY-TABIX,      " TABLE의 마지?
      W_OK_CODE        LIKE SY-UCOMM,      " OK-CODE
      OK-CODE          LIKE SY-UCOMM,      " OK-CODE
      W_AMOUNT         LIKE ZTFINHD-ZFTFEE, " TOTAL AMOUNT.
      EGRKZ            LIKE     T007A-EGRKZ,
      W_ROW_MARK1      TYPE C.             " RECORD의 선택여?

DATA: W_KBETR             LIKE     KONP-KBETR,
      W_KBETR1            LIKE     KONP-KBETR,
      W_KONWA             LIKE     KONP-KONWA,
      W_WMWST             LIKE     ZTBKPF-WMWST,
      W_WMWST1            LIKE     ZTBKPF-WMWST.
CONTROLS: TC_0100    TYPE TABLEVIEW USING SCREEN 0100.

DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON         LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C,
              ZFIVNO       LIKE ZTIV-ZFIVNO,
              ZFSEQ        LIKE ZSBLCST-ZFSEQ,
              ZFSUBSEQ     TYPE I.
DATA : END OF IT_ERR_LIST.

*>> 비용코드 HELP.
DATA : BEGIN OF IT_COST_HELP OCCURS 0,
       ZFCD      LIKE ZTIMIMG08-ZFCD,
       ZFCDNM    LIKE ZTIMIMG08-ZFCDNM,
       ZFCD1     LIKE ZTIMIMG08-ZFCD1,
       ZFCD5     LIKE ZTIMIMG08-ZFCD5,
       COND_TYPE LIKE ZTIMIMG08-COND_TYPE,
       END OF IT_COST_HELP.

DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

DATA:   BEGIN OF XRETURN OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF XRETURN.

TABLES : BAPIMEPOHEADER,
         BAPIMEPOHEADERX.

*-----------------------------------------------------------------------
* INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_FINHD OCCURS 0,
       ZFDHENO  LIKE  ZTFINHD-ZFDHENO,	  " 문서관리번호.
       ZFREQNO  LIKE  ZTREQST-ZFREQNO,     " 수입의뢰관리번호.
       ZFAMDNO  LIKE  ZTREQST-ZFAMDNO,     " AMEND SEQ.
       ZFOPBN   LIKE  ZTREQHD-ZFOPBN,
       ZFBILCD  LIKE  ZTFINHD-ZFBILCD,	  " 계산서용도.
       ZFREFNO  LIKE  ZTFINHD-ZFREFNO,     " 참조문서번호.
       ZFTRDT   LIKE  ZTFINHD-ZFTRDT,      " 거래일자.
       ZFRCDT	  LIKE  ZTFINHD-ZFRCDT,	  " 수신일.
       ZFTERM	  LIKE  ZTFINHD-ZFTERM,      " 결제방법.
       ZFNOTE	  LIKE  ZTFINHD-ZFNOTE,      " 어음조건.
       BUKRS    LIKE  ZTFINHD-BUKRS,
       GJAHR    LIKE  ZTFINHD-GJAHR,
       BELNR    LIKE  ZTFINHD-BELNR,
       ZFBKCD	  LIKE  ZTFINHD-ZFBKCD,	 " 은행코드.
       ZFBKNM	  LIKE  ZTFINHD-ZFBKNM,      " 은행명.
       ZFBRNM	  LIKE  ZTFINHD-ZFBRNM,	  " 발급은행 지점명.
       ZFTFEE	  LIKE  ZTFINHD-ZFTFEE,      " 수수료(이자) 합계.
       WAERS	  LIKE  ZTFINHD-WAERS,       " 통화키.
       ZFDBYN	  LIKE  ZTFINHD-ZFDBYN,      " 수수료 반영 여부.
       ZFDBDT	  LIKE  ZTFINHD-ZFDBDT,      " 수수료 반영 일자.
       OK(1),                              " 매치여부.
       MAT_OK(4),                          " 매치내용.
       ZFDBTM	  LIKE  ZTFINHD-ZFDBTM.      " 수수료 반영 시간.
DATA : END OF IT_FINHD.

DATA : BEGIN OF IT_FINIT OCCURS 0,
       ZFDHENO   LIKE  ZTFINIT-ZFDHENO,	" 문서관리번호.
       ZFSEQ	   LIKE  ZTFINIT-ZFSEQ,	" 일련번호.
       ZFCD	   LIKE  ZTFINIT-ZFCD,       " 관리코드.
       ZFRATE	   LIKE  ZTFINIT-ZFRATE,     " 수수료(이자) 적용율.
       ZFAMT	   LIKE  ZTFINIT-ZFAMT,      " 금액.
       WAERS	   LIKE  ZTFINIT-WAERS,      " 통화키.
       ZFCDNM    LIKE  ZTIMIMG08-ZFCDNM,   " 비용내역.
       COND_TYPE LIKE ZTIMIMG08-COND_TYPE, " 조건코드.
       BLART     LIKE ZTIMIMG08-BLART,     " 문서종류.
       DV_CT(1),                           " DELVERY COST 여부.
       ZFFEE	   LIKE  ZTFINIT-ZFFEE,      " 산출금액(수수료,이자).
       ZFKRW	   LIKE  ZTFINIT-ZFKRW,      " 원화통화.
       ZFEXRT	   LIKE  ZTFINIT-ZFEXRT,     " 환율.
       ZFDAY	   LIKE  ZTFINIT-ZFDAY,      " 적용일수.
       ZFFROM	   LIKE  ZTFINIT-ZFFROM,     " 적용기간(FROM).
       ZFEND	   LIKE  ZTFINIT-ZFEND.      " 적용기간(TO).
DATA : END OF IT_FINIT.

DATA : BEGIN OF IT_COSTGB OCCURS 2,
       DV_CT(1),                           " DELVERY COST 여부.
       END   OF IT_COSTGB.

DATA: BEGIN OF IT_SELECTED OCCURS 0,
      BUKRS      LIKE  ZTFINHD-BUKRS,
      ZFDHENO    LIKE  ZTFINIT-ZFDHENO,	" 문서관리번호.
      ZFREQNO    LIKE ZTREQST-ZFREQNO,   " 수입의뢰 관리번호.
      ZFAMDNO    LIKE ZTREQST-ZFAMDNO,   " Amend Seq.
      ZFBKCD	   LIKE  ZTFINHD-ZFBKCD,   " 은행코드.
      ZFTFEE	   LIKE  ZTFINHD-ZFTFEE,   " 수수료(이자) 합계.
END OF IT_SELECTED.

*>> 비용 항목용 INTENANL TABLE.
DATA: IT_ZSBSEG          LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.
DATA: IT_ZSBSEGC_OLD     LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.
DATA: IT_ZSBSEGV         LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.
DATA: IT_ZSBSEGV_OLD     LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.
DATA: W_OLD_ZFBKCD       LIKE ZTFINHD-ZFBKCD.
DATA: W_OLD_BUKRS        LIKE ZTBKPF-BUKRS.
DATA: IT_ZTFINHD         LIKE ZTFINHD OCCURS 100 WITH HEADER LINE.
DATA: IT_ZTFINIT         LIKE ZTFINIT OCCURS 100 WITH HEADER LINE.

DATA: IT_ZTDHF1          LIKE ZTDHF1  OCCURS 100 WITH HEADER LINE.
