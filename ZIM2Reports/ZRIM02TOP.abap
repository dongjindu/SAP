*&---------------------------------------------------------------------*
*& Include ZRIM02TOP                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입비용 Main Data Define Include                     *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.05.214                                           *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
* ALV
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
DATA    G_SAVE(1) TYPE C.
DATA    G_VARIANT LIKE DISVARIANT.

*-----------------------------------------------------------------------
* Title Text Define
*-----------------------------------------------------------------------
DATA : W_CREATE(6)        TYPE     C     VALUE   'Create',
       W_CHANGE(6)        TYPE     C     VALUE   'Change',
       W_DISPLAY(7)       TYPE     C     VALUE   'Display',
       W_ADD_CHG(17)      TYPE     C     VALUE   'Additional Change',
       W_ADD_DIS(18)      TYPE     C     VALUE   'Additional Display',
       W_OPEN(7)          TYPE     C     VALUE   'Opennig',
       W_OPEN_CHANGE(11)  TYPE     C     VALUE   'Open Change',
       W_STAUTS(13)       TYPE     C     VALUE   'Status Change',
       W_OPEN_DISPLAY(12) TYPE     C     VALUE   'Open Display'.

*>> 비용항목용 INTERNAL TABLE.
DATA : IT_ZSBSEG      LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBSEG_OLD  LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.

*>> 비용배부내용 INTERNAL TABLE.
DATA : IT_ZSBDIV      LIKE ZSBDIV OCCURS 100 WITH HEADER LINE.
DATA : IT_ZTBDIV      LIKE ZTBDIV OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBDIVL     LIKE ZSBDIVL OCCURS 100 WITH HEADER LINE.

*>> 전기 이력 INTERNAL TABLE.
DATA : IT_ZSBHIS      LIKE ZSBHIS OCCURS 100 WITH HEADER LINE.


*>> 비용코드 HELP.
DATA : BEGIN OF IT_COST_HELP OCCURS 0,
       ZFCD      LIKE ZTIMIMG08-ZFCD,
       ZFCDNM    LIKE ZTIMIMG08-ZFCDNM,
       ZFCD1     LIKE ZTIMIMG08-ZFCD1,
       ZFCD5     LIKE ZTIMIMG08-ZFCD5,
       COND_TYPE LIKE ZTIMIMG08-COND_TYPE,
       END OF IT_COST_HELP.

*>> 수입관련문서 다중 선택시 MEMORY ID로 GET하는 INTERNAL TABLE.
DATA : BEGIN OF IT_ZFIMDNO OCCURS 0,
       ZFCSTGRP  LIKE ZTBSEG-ZFCSTGRP,
       ZFIMDNO   LIKE ZTBSEG-ZFIMDNO,
       ZFDCNM    LIKE ZSBSEG-ZFDCNM,
       ZUONR     LIKE ZTBSEG-ZUONR,
       PS_POSID  LIKE ZSBSEG-PS_POSID,
       KOSTL     LIKE ZTBL-KOSTL,
       ZFUPT     LIKE ZTBL-ZFUPT,
       ZFPOYN    LIKE ZSBSEG-ZFPOYN,
       END   OF IT_ZFIMDNO.


*>> 최초로 들어올 경우의 화면.
DATA: W_FIRST_SCR0100     VALUE    'Y',
      W_TMP_TABIX         LIKE    SY-TABIX,
      W_ERR_MODE,
      W_ZFCSTGRP          LIKE     ZTBKPF-ZFCSTGRP,
      W_CHK_BIT           TYPE     C,
      W_READ_ERROR        VALUE    'N',
      W_ZTBKPF            LIKE     ZTBKPF,
      W_COMMAND           LIKE     SY-UCOMM,
      W_ZFDCNM            LIKE     ZSBSEG-ZFDCNM,
      TEXT_080            LIKE     ICONT-QUICKINFO.

*>> 회사코드 Set.
DATA : P_BUKRS            LIKE      ZTIMIMG00-ZFBUKRS.
