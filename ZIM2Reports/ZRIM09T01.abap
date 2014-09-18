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
DATA  G_VARIANT LIKE DISVARIANT.
DATA  G_USER_COMMAND   TYPE SLIS_FORMNAME VALUE 'P2000_ALV_COMMAND'.
DATA  G_STATUS         TYPE SLIS_FORMNAME VALUE 'P2000_ALV_PF_STATUS'.

CONTROLS TABSTRIP    TYPE TABSTRIP.
CONTROLS: TC_0010    TYPE TABLEVIEW USING SCREEN 0010,
          TC_0011    TYPE TABLEVIEW USING SCREEN 0011,
          TC_0012    TYPE TABLEVIEW USING SCREEN 0012,
          TC_0021    TYPE TABLEVIEW USING SCREEN 0021,
          TC_0031    TYPE TABLEVIEW USING SCREEN 0031,
          TC_0102    TYPE TABLEVIEW USING SCREEN 0103,
          TC_0103    TYPE TABLEVIEW USING SCREEN 0103,
          TC_0104    TYPE TABLEVIEW USING SCREEN 0105,
          TC_0105    TYPE TABLEVIEW USING SCREEN 0104,
          TC_0106    TYPE TABLEVIEW USING SCREEN 0106,
          TC_0110    TYPE TABLEVIEW USING SCREEN 0110,
          TC_0111    TYPE TABLEVIEW USING SCREEN 0111,
          TC_0120    TYPE TABLEVIEW USING SCREEN 0120,
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
          TC_8116    TYPE TABLEVIEW USING SCREEN 8116,
          TC_8210    TYPE TABLEVIEW USING SCREEN 8210,
          TC_8212    TYPE TABLEVIEW USING SCREEN 8212,
          TC_8220    TYPE TABLEVIEW USING SCREEN 8220,
          TC_8410    TYPE TABLEVIEW USING SCREEN 8410,
          TC_8510    TYPE TABLEVIEW USING SCREEN 8510,
          TC_8514    TYPE TABLEVIEW USING SCREEN 8514,
          TC_8710    TYPE TABLEVIEW USING SCREEN 8710,
          TC_9911    TYPE TABLEVIEW USING SCREEN 9911,
          TC_9912_1  TYPE TABLEVIEW USING SCREEN 9912,
          TC_9912_2  TYPE TABLEVIEW USING SCREEN 9912.
*
*-----------------------------------------------------------------------
* 수입문서의 메뉴 상태를 상수로 정?
*-----------------------------------------------------------------------
DATA : C_BL(15)      VALUE 'Bill of Lading',
       C_LG(20)      VALUE 'Letter of Guarantee',
       C_CIV(19)     VALUE 'Commercial Invoice',
       C_LOV(14)     VALUE 'Local L/C 물대',
       C_PUV(15)     VALUE '구매승인서 물대',
       C_CC(17)      VALUE '통관요청/입고요청',
       C_LO(18)      VALUE 'Local L/C 입고요청',
       C_PU(19)      VALUE '구매승인서 입고요청',
       C_CG(09)      VALUE '하역 관리'.

*-----------------------------------------------------------------------
* Title Text Define
*-----------------------------------------------------------------------
DATA : W_CREATE(6)        TYPE     C     VALUE   'Create',
       W_CHANGE(6)        TYPE     C     VALUE   'Change',
       W_DISPLAY(7)       TYPE     C     VALUE   'Display',
       W_OPEN(08)         TYPE     C     VALUE   'Openning',
       W_SEND(13)         TYPE     C     VALUE   '선적서류 송부',
       W_REAL(11)         TYPE     C     VALUE   '실입항 입력',
       W_PROCESS(14)      TYPE     C     VALUE   '실반출 Process',
       W_STAUTS(13)       TYPE     C     VALUE   'Status Change',
       W_ADD_CON(16)      TYPE     C     VALUE   'Container Change',
       W_ADD_CHG(16)      TYPE     C     VALUE   '해외운임 Change'.

DATA : W_ZFCOTM_NM(20)    TYPE     C,
       W_ZFCUT_NM(20)     TYPE     C,
       W_ZFMATGB_NM(18)   TYPE     C,
       W_FIRST_SCR0200    VALUE    'Y'.
