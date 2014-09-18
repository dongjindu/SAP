*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM01T01                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 :  수입 B/L 관련 Scrren 및 상수 Define                  *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.12                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*>> ALV용.
TYPE-POOLS: SLIS.


DATA: G_REPID   LIKE SY-REPID.
DATA  G_SAVE(1) TYPE C.
*>> 반입.LG 버전.
DATA: MARKFIELD,
      W_SELECTED_LINES  TYPE P.                 " 선택 LINE COUNT
TABLES: ZTBLINR_TMP,*ZTBLINR_TMP.
DATA :  W_TBLNO LIKE ZSREQHD-ZFTBLNO.
DATA  G_VARIANT LIKE DISVARIANT.
DATA  G_USER_COMMAND   TYPE SLIS_FORMNAME VALUE 'P2000_ALV_COMMAND'.
DATA  G_STATUS         TYPE SLIS_FORMNAME VALUE 'P2000_ALV_PF_STATUS'.

CONTROLS TABSTRIP    TYPE TABSTRIP.
CONTROLS: TC_0010    TYPE TABLEVIEW USING SCREEN 0010,
          TC_0011    TYPE TABLEVIEW USING SCREEN 0011,
          TC_0012    TYPE TABLEVIEW USING SCREEN 0012,
          TC_0021    TYPE TABLEVIEW USING SCREEN 0021,
          TC_0031    TYPE TABLEVIEW USING SCREEN 0031,
          TC_0060    TYPE TABLEVIEW USING SCREEN 0060,
          TC_0070    TYPE TABLEVIEW USING SCREEN 0070,
          TC_0102    TYPE TABLEVIEW USING SCREEN 0103,
          TC_0103    TYPE TABLEVIEW USING SCREEN 0103,
          TC_0104    TYPE TABLEVIEW USING SCREEN 0105,
          TC_0105    TYPE TABLEVIEW USING SCREEN 0104,
          TC_0106    TYPE TABLEVIEW USING SCREEN 0106,
          TC_0110    TYPE TABLEVIEW USING SCREEN 0110,
          TC_0111    TYPE TABLEVIEW USING SCREEN 0111,
          TC_0112    TYPE TABLEVIEW USING SCREEN 0112,
          TC_0113    TYPE TABLEVIEW USING SCREEN 0113,
          TC_0120    TYPE TABLEVIEW USING SCREEN 0120,
          TC_0131    TYPE TABLEVIEW USING SCREEN 0131, "JSY 20021011.
          TC_0143    TYPE TABLEVIEW USING SCREEN 0143,
          TC_0812    TYPE TABLEVIEW USING SCREEN 0812,
          TC_0813    TYPE TABLEVIEW USING SCREEN 0813,
          TC_2604    TYPE TABLEVIEW USING SCREEN 2604,
          TC_3111    TYPE TABLEVIEW USING SCREEN 3111,
          TC_3114    TYPE TABLEVIEW USING SCREEN 3114,
          TC_3114_1  TYPE TABLEVIEW USING SCREEN 3114,
          TC_3115    TYPE TABLEVIEW USING SCREEN 3115,
          TC_3116    TYPE TABLEVIEW USING SCREEN 3116,
          TC_3117    TYPE TABLEVIEW USING SCREEN 3117,
          TC_3117_1  TYPE TABLEVIEW USING SCREEN 3117,
          TC_3511    TYPE TABLEVIEW USING SCREEN 3511,
          TC_3520    TYPE TABLEVIEW USING SCREEN 3520,
          TC_4104    TYPE TABLEVIEW USING SCREEN 4104,
          TC_4105    TYPE TABLEVIEW USING SCREEN 4105,
          TC_3513    TYPE TABLEVIEW USING SCREEN 3513,
          TC_3515    TYPE TABLEVIEW USING SCREEN 3515,
          TC_3519    TYPE TABLEVIEW USING SCREEN 3519,
          TC_5710    TYPE TABLEVIEW USING SCREEN 5710,
          TC_6212    TYPE TABLEVIEW USING SCREEN 6212,
          TC_6216    TYPE TABLEVIEW USING SCREEN 6216,
          TC_6218    TYPE TABLEVIEW USING SCREEN 6218,
          TC_6219    TYPE TABLEVIEW USING SCREEN 6219,
          TC_6410    TYPE TABLEVIEW USING SCREEN 6410,
          TC_6412    TYPE TABLEVIEW USING SCREEN 6412,
          TC_6610    TYPE TABLEVIEW USING SCREEN 6610,
          TC_66101   TYPE TABLEVIEW USING SCREEN 6610,
          TC_6710    TYPE TABLEVIEW USING SCREEN 6710,
          TC_7110    TYPE TABLEVIEW USING SCREEN 7110,
          TC_7412    TYPE TABLEVIEW USING SCREEN 7412,
          TC_7416    TYPE TABLEVIEW USING SCREEN 7416,
          TC_7418    TYPE TABLEVIEW USING SCREEN 7418,
          TC_7419    TYPE TABLEVIEW USING SCREEN 7419,
          TC_7420    TYPE TABLEVIEW USING SCREEN 7420,
*          TC_8116    TYPE TABLEVIEW USING SCREEN 8116,
          TC_8210    TYPE TABLEVIEW USING SCREEN 8210,
          TC_8212    TYPE TABLEVIEW USING SCREEN 8212,
          TC_8220    TYPE TABLEVIEW USING SCREEN 8220,
          TC_8410    TYPE TABLEVIEW USING SCREEN 8410,
          TC_8510    TYPE TABLEVIEW USING SCREEN 8510,
          TC_8514    TYPE TABLEVIEW USING SCREEN 8514,
          TC_8710    TYPE TABLEVIEW USING SCREEN 8710,
          TC_9911    TYPE TABLEVIEW USING SCREEN 9911,
          TC_9216    TYPE TABLEVIEW USING SCREEN 9216,
          TC_9912_1  TYPE TABLEVIEW USING SCREEN 9912,
          TC_9912_2  TYPE TABLEVIEW USING SCREEN 9912.
*
*-----------------------------------------------------------------------
* MENU Status Variable Define
*-----------------------------------------------------------------------
DATA : C_BL(15)      VALUE 'Bill of Lading',
       C_LG(20)      VALUE 'Letter of Guarantee',
       C_CIV(19)     VALUE 'Commercial Invoice',
       C_LOV(14)     VALUE 'Local Invoice',
       C_PUV(15)     VALUE 'P/U Invoice',
       C_CC(24)      VALUE 'Clearance/ G.R. Request',
       C_LO(24)      VALUE 'Local L/C G.R. Request',
       C_PU(19)      VALUE 'P/U G.R. Request',
       C_CG(19)      VALUE 'Cargo Management'.


*>> Import Cost Code HELP.
DATA : BEGIN OF IT_COST_HELP OCCURS 0,
       ZFCD      LIKE ZTIMIMG08-ZFCD,
       ZFCDNM    LIKE ZTIMIMG08-ZFCDNM,
       ZFCD1     LIKE ZTIMIMG08-ZFCD1,
       ZFCD5     LIKE ZTIMIMG08-ZFCD5,
       COND_TYPE LIKE ZTIMIMG08-COND_TYPE,
       END OF IT_COST_HELP.


*-----------------------------------------------------------------------
* Title Text Define
*-----------------------------------------------------------------------
DATA : W_CREATE(6)        TYPE     C     VALUE   'Create',
       W_CHANGE(6)        TYPE     C     VALUE   'Change',
       W_DISPLAY(7)       TYPE     C     VALUE   'Display',
       W_OPEN(08)         TYPE     C     VALUE   'Openning',
       W_REQUEST(6)       TYPE     C     VALUE   'Create',
       W_COST(10)         TYPE     C     VALUE   'Cost Entry',
       W_SEND(19)         TYPE     C     VALUE   'Sales Document Send',
       W_REAL(34)         TYPE     C     VALUE
                          'B/L Actual Date of Arrival in Port',
       W_PROCESS(14)      TYPE     C     VALUE   '실반출 Process',
       W_STAUTS(13)       TYPE     C     VALUE   'Status Change',
       W_ADD_CON(16)      TYPE     C     VALUE   'Container Change',
       W_ADD_CHG(16)      TYPE     C     VALUE 'Oversea Freigt Change'.

DATA : W_FIRST_SCR0144    VALUE    'Y',
       W_ZFCOTM_NM(20)    TYPE     C,
       W_ZFCUT_NM(20)     TYPE     C,
       W_ZFMATGB_NM(18)   TYPE     C,
       W_ZFSHNO           LIKE     ZTBLIT-ZFSHNO,
       W_PSPNR            LIKE     PRPS-PSPNR,
       W_FIRST_SCR0200    VALUE    'Y'.

DATA : IT_SELECTED LIKE ZSREQHD OCCURS 50 WITH HEADER LINE.
DATA : IT_COST_SELECTED LIKE ZSBLCST OCCURS 50 WITH HEADER LINE.

DATA : IT_ZSBSEG_POST LIKE ZSBSEG OCCURS 10 WITH HEADER LINE.
DATA : IT_ZSBSEG      LIKE ZSBSEG OCCURS 10 WITH HEADER LINE.
DATA : IT_ZSBSEG_TMP  LIKE ZSBSEG OCCURS 10 WITH HEADER LINE.

DATA : BEGIN OF IT_ZFSHNO OCCURS 5,
       EBELN     LIKE ZSBLIT-EBELN,
       ZFSHNO    LIKE ZSBLIT-ZFSHNO,
       END   OF IT_ZFSHNO.

DATA : BEGIN OF IT_ZFREDNO OCCURS 5,
       ZFREDNO   LIKE ZTRED-ZFREDNO,
       END   OF IT_ZFREDNO.

DATA : BEGIN OF IT_GSBER OCCURS 5,
       GSBER    LIKE     BSEG-GSBER,
       END   OF IT_GSBER.

DATA : OUT_TEXT(70)  TYPE C,
       LINE1(3)      TYPE N.             " 페이지당 LINE COUNT

*>> B/L 운임계산 스크린용 STRUCTURE.
DATA : BEGIN OF WA_BLCALC,
       ZFRTCD(15),                       " 요율 Display용.
       ZFRTCD1(15),
       ZFRTCD2(15),
       BLCURR1     LIKE ZTBLCST-WAERS,   " 계약통화 Display용.
       BLCURR2     LIKE ZTBLCST-WAERS,   " 계약통화 Display용.
       ZFCAMT1     LIKE ZTBLCST-ZFCAMT,  " 계산운임.
       ZFCAMT2     LIKE ZTBLCST-ZFCAMT,  " 계산운임.
       ZFCAMT3     LIKE ZTBLCST-ZFCAMT,  " Calculated Freight (20Ft HQ).
       ZFCAMT4     LIKE ZTBLCST-ZFCAMT,  " Calculated Freight (40Ft HQ).
       ZFTOTAMT    LIKE ZTBLCST-ZFCAMT.    " 총  운임.
DATA : END OF WA_BLCALC.

*> 라디오버튼 - 기본/차상위(B/L운임)
DATA: RB_DF, RB_UP.
*> 운임계산화면에 처음세팅 체크.
DATA: W_FIRST_CHK.
*> 반입화면 단위 TEXT.
DATA : W_TOWTM(3), W_PKCNM(2).

DATA : LOOPLINES LIKE SY-LOOPC.
