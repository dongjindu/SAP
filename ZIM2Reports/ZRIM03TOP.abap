*----------------------------------------------------------------------*
*INCLUDE ZRIM03TOP .
*&---------------------------------------------------------------------*
*&  프로그램명 : 관세/부가세 Main Data Define Include                  *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.06.11                                            *
*&  적용회사PJT: LG 화학                                               *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

CONSTANTS: GC_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME
                                   VALUE 'TOP_OF_PAGE'.

DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      GS_KEYINFO  TYPE SLIS_KEYINFO_ALV,
      GT_SORT     TYPE SLIS_T_SORTINFO_ALV,
      GT_SP_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GT_EVENTS   TYPE SLIS_T_EVENT.

DATA: GT_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
DATA  G_USER_COMMAND      TYPE SLIS_FORMNAME VALUE 'P2000_USER_COMMAND'.
DATA  G_STATUS            TYPE SLIS_FORMNAME VALUE 'P2000_SET_STATUS'.
DATA: G_REPID LIKE SY-REPID.
DATA  G_SAVE(1) TYPE C.
DATA  G_VARIANT LIKE DISVARIANT.
DATA: W_ZFCCIT  LIKE ZTCCIT-ZFCCIT.



*-----------------------------------------------------------------------
* Title Text Define
*-----------------------------------------------------------------------
DATA : W_CREATE(4)        TYPE     C     VALUE   '생성',
       W_CHANGE(4)        TYPE     C     VALUE   '변경',
       W_DISPLAY(4)       TYPE     C     VALUE   '조회',
       W_ADD_CHG(17)      TYPE     C     VALUE   'Additional Change',
       W_ADD_DIS(18)      TYPE     C     VALUE   'Additional Display',
       W_OPEN(7)          TYPE     C     VALUE   'Opennig',
       W_OPEN_CHANGE(11)  TYPE     C     VALUE   'Open Change',
       W_STAUTS(13)       TYPE     C     VALUE   'Status Change',
       W_OPEN_DISPLAY(12) TYPE     C     VALUE   'Open Display'.

*>> 관세/부가세 항목용 INTERNAL TABLE.
DATA : IT_ZSCCIT      LIKE ZSCCIT OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSCCIT_OLD  LIKE ZSCCIT OCCURS 100 WITH HEADER LINE.

*>> 비용 항목용 INTENANL TABLE.
DATA: IT_ZSBSEGC         LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.
DATA: IT_ZSBSEGC_OLD     LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.
DATA: IT_ZSBSEGV         LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.
DATA: IT_ZSBSEGV_OLD     LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.

*>> 비용배부내용 INTERNAL TABLE.
DATA : IT_ZSBDIV      LIKE ZSBDIV OCCURS 100 WITH HEADER LINE.
DATA : IT_ZTBDIV      LIKE ZTBDIV OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBDIVL     LIKE ZSBDIVL OCCURS 100 WITH HEADER LINE.

*>> 전기 이력 INTERNAL TABLE.
DATA : IT_ZSBHIS      LIKE ZSBHIS OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBHISCC    LIKE ZSBHIS OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBHISVT    LIKE ZSBHIS OCCURS 100 WITH HEADER LINE.
DATA : IT_SELECTED    LIKE ZSBHIS OCCURS 10  WITH HEADER LINE.


*>> 비용코드 HELP.
DATA : BEGIN OF IT_COST_HELP OCCURS 0,
       ZFCD      LIKE ZTIMIMG08-ZFCD,
       ZFCDNM    LIKE ZTIMIMG08-ZFCDNM,
       ZFCD1     LIKE ZTIMIMG08-ZFCD1,
       ZFCD5     LIKE ZTIMIMG08-ZFCD5,
       COND_TYPE LIKE ZTIMIMG08-COND_TYPE,
       END OF IT_COST_HELP.

DATA:  W_ZFCVAMT LIKE ZTCCIT-ZFCVAMT,
       W_AMT_TMP LIKE ZTCCIT-ZFCVAMT,
       W_FWBAS   LIKE ZTCCIT-FWBAS.

*>> 수입관련문서 다중 선택시 MEMORY ID로 GET하는 INTERNAL TABLE.
DATA : BEGIN OF IT_ZFIMDNO OCCURS 0,
       ZFCSTGRP  LIKE ZTBSEG-ZFCSTGRP,
       ZFIMDNO   LIKE ZTBSEG-ZFIMDNO,
       ZFDCNM    LIKE ZSBSEG-ZFDCNM,
       ZUONR     LIKE ZTBSEG-ZUONR,
       ZFPOYN    LIKE ZSBSEG-ZFPOYN,
       END   OF IT_ZFIMDNO.


*>> 최초로 들어올 경우의 화면.
DATA: W_FIRST_SCR0100     TYPE    C  VALUE 'Y',
      W_TMP_TABIX         LIKE    SY-TABIX,
      W_LIST_INDEX1       LIKE    SY-TABIX,
      W_LIST_INDEX2       LIKE    SY-TABIX,
      W_ERR_MODE,
*      W_EDIT_CHECK,
      W_ZFCSTGRP          LIKE     ZTBKPF-ZFCSTGRP,
      W_CHK_BIT           TYPE     C,
      W_READ_ERROR        VALUE    'N',
      W_ZTCCHD            LIKE     ZTCCHD,
      W_COMMAND           LIKE     SY-UCOMM,
      W_ZFDCNM            LIKE     ZSBSEG-ZFDCNM,
      TEXT_080            LIKE     ICONT-QUICKINFO,
      TEXT_090            LIKE     ICONT-QUICKINFO,
      W_AMPEL             LIKE     RF05A-AMPEL.
