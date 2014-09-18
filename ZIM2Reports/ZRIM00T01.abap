*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM00T01                                                  *
*----------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰 Scrren 및 상수 Define                        *
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
CONTROLS: TC_0004    TYPE TABLEVIEW USING SCREEN 0004,
          TC_0010    TYPE TABLEVIEW USING SCREEN 0010,
          TC_0011    TYPE TABLEVIEW USING SCREEN 0011,
          TC_0102    TYPE TABLEVIEW USING SCREEN 0102,
          TC_0103    TYPE TABLEVIEW USING SCREEN 0103,
          TC_0103_1  TYPE TABLEVIEW USING SCREEN 0103,
          TC_0106    TYPE TABLEVIEW USING SCREEN 0106,
          TC_0107    TYPE TABLEVIEW USING SCREEN 0107,
          TC_0108    TYPE TABLEVIEW USING SCREEN 0108,
          TC_0109    TYPE TABLEVIEW USING SCREEN 0109,
          TC_0109_1  TYPE TABLEVIEW USING SCREEN 0109,
*          TC_0110    TYPE TABLEVIEW USING SCREEN 0110,
          TC_0143    TYPE TABLEVIEW USING SCREEN 0143,
          TC_0114    TYPE TABLEVIEW USING SCREEN 0114,
          TC_0123    TYPE TABLEVIEW USING SCREEN 0123,
          TC_0125    TYPE TABLEVIEW USING SCREEN 0123,
          TC_0191    TYPE TABLEVIEW USING SCREEN 0191,
          TC_1114    TYPE TABLEVIEW USING SCREEN 1114,
          TC_1106    TYPE TABLEVIEW USING SCREEN 1106,
          TC_1199    TYPE TABLEVIEW USING SCREEN 1199,
          TC_2103    TYPE TABLEVIEW USING SCREEN 2103,
          TC_2103_1  TYPE TABLEVIEW USING SCREEN 2103,
          TC_4104    TYPE TABLEVIEW USING SCREEN 4104,
          TC_4105    TYPE TABLEVIEW USING SCREEN 4105,
          TC_4105_1  TYPE TABLEVIEW USING SCREEN 4105.

*-----------------------------------------------------------------------
* 수입문서의 메뉴 상태를 상수로 정?
*-----------------------------------------------------------------------
DATA : C_M_LC(10)    VALUE 'Master L/C',                  "
       C_L_LC(09)    VALUE 'Local L/C',                   "
       C_TT(20)      VALUE 'Telegraphic transfer',        "
       C_PU(10)      VALUE 'Purchasing License',                  "
       C_DP(24)      VALUE 'Document against Payment',    "
       C_DA(27)      VALUE 'Document against Acceptance', "
       C_OF(11)      VALUE 'Offer Sheet'.                 "

*-----------------------------------------------------------------------
* 수입문서의 메뉴 상태를 상수로 정?
*-----------------------------------------------------------------------
DATA : C_M_LC_A(16)    VALUE 'Master L/C amend',
       C_L_LC_A(15)    VALUE 'Local L/C amend',
       C_TT_A(26)      VALUE 'Telegraphic transfer amend',
       C_PU_A(16)      VALUE 'Purchasing License amend',
       C_DP_A(30)      VALUE 'Document against Payment amend',
       C_DA_A(33)      VALUE 'Document against Acceptance amend'.

*-----------------------------------------------------------------------
* Title Text Define
*-----------------------------------------------------------------------
DATA : W_CREATE(6)        TYPE     C     VALUE   'Create',
       W_CHANGE(6)        TYPE     C     VALUE   'Change',
       W_DISPLAY(7)       TYPE     C     VALUE   'Display',
       W_ADD_CHG(17)      TYPE     C     VALUE   'Additional Change',
       W_ADD_DIS(18)      TYPE     C     VALUE   'Additional Display',
       W_OPEN(7)          TYPE     C     VALUE   'Opening',
       W_OPEN_CHANGE(11)  TYPE     C     VALUE   'Open Change',
       W_STAUTS(13)       TYPE     C     VALUE   'Status Change',
       W_OPEN_DISPLAY(12) TYPE     C     VALUE   'Open Display',
       W_INSURANCE(12)    TYPE     C     VALUE   'Insurance'.
