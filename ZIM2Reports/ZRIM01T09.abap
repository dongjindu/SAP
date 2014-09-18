*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM01T01                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 보세창고출고 관련 Scrren 및 상수 Define               *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.20                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

TABLES: ZTBWIT,
        ZTBWHD,
       *ZTBWHD,
        ZSBWIT.
*----------------------------------------------------------------------*
* INTERNAL TABLE.
*----------------------------------------------------------------------*
DATA : IT_ZSBWIT      LIKE ZSBWIT      OCCURS 100 WITH HEADER LINE.
DATA : IT_ZSBWIT_OLD  LIKE ZSBWIT      OCCURS 100 WITH HEADER LINE.

*----------------------------------------------------------------------*
* 변수.
*----------------------------------------------------------------------+
DATA: G_REPID          LIKE SY-REPID.
DATA: W_ZFGISEQ        LIKE ZTBWHD-ZFGISEQ,
      W_BWMENGE1       LIKE ZSBWIT-BWMENGE1,
      W_ZFPKCN         LIKE ZTBWHD-ZFPKCN,
      W_ZFTOWT         LIKE ZTBWHD-ZFTOWT,
      W_ZFTOVL         LIKE ZTBWHD-ZFTOVL,
      W_BWMENGE2       LIKE ZSBWIT-BWMENGE1.

DATA  G_SAVE(1)        TYPE C.
DATA  G_VARIANT        LIKE DISVARIANT.
DATA  G_USER_COMMAND   TYPE SLIS_FORMNAME VALUE 'P2000_ALV_COMMAND'.
DATA  G_STATUS         TYPE SLIS_FORMNAME VALUE 'P2000_ALV_PF_STATUS'.


CONTROLS TABSTRIP    TYPE TABSTRIP.
CONTROLS: TC_0101    TYPE TABLEVIEW USING SCREEN 0101.
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
       W_DOM_TEX1(17)     TYPE     C,
       W_ADD_CHG(16)      TYPE     C     VALUE   '해외운임 Change'.

DATA : W_ZFCOTM_NM(20)    TYPE     C,
       W_ZFCUT_NM(20)     TYPE     C,
       W_ZFMATGB_NM(18)   TYPE     C,
       W_FIRST_SCR0200    VALUE    'Y'.
