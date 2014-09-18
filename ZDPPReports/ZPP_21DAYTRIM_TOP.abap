*----------------------------------------------------------------------*
*   INCLUDE ZSJ_TEST001_TOP                                            *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   INCLUDE ZSJ_TEST029_TOP                                            *
*----------------------------------------------------------------------*

**-------------------------------------------------------------------*
**  TABLE DEFINE
**-------------------------------------------------------------------*
TABLES : ZTPP_INPUT_PLAN,
         ZTPP_PMT07JB_A.

**-------------------------------------------------------------------*
**  DEFINE TYPE
**-------------------------------------------------------------------*
TYPES : BEGIN OF T_KEY ,

           SEQ_DATE   LIKE ZTPP_INPUT_PLAN-SEQ_DATE,
           SEQ_WEEK(2)   TYPE N,
           WORD       LIKE ZTPP_INPUT_PLAN-WORK_ORDER,
           EXTC       LIKE ZTPP_INPUT_PLAN-EXTC,
           INTC       LIKE ZTPP_INPUT_PLAN-INTC,
           OCNN       LIKE ZTPP_INPUT_PLAN-OCNN,
           VERS       LIKE ZTPP_INPUT_PLAN-OCNN,
           MI         LIKE ZTPP_INPUT_PLAN-MI,
           MODL       LIKE ZTPP_INPUT_PLAN-MODL,
           NQTY       TYPE P,
       END OF T_KEY.


**-------------------------------------------------------------------*
**  DEFINE ITAB
**-------------------------------------------------------------------*
*Performence Test
RANGES IT_ATINN FOR CABN-ATINN.

DATA : BEGIN OF GT_KEY OCCURS 0.
INCLUDE TYPE T_KEY.
*           SEQ_DATE   LIKE ZTPP_INPUT_PLAN-SEQ_DATE,
*           SEQ_WEEK(2)   TYPE N,
*           WORD       LIKE ZTPP_INPUT_PLAN-WORK_ORDER,
*           EXTC       LIKE ZTPP_INPUT_PLAN-EXTC,
*           INTC       LIKE ZTPP_INPUT_PLAN-INTC,
*           OCNN       LIKE ZTPP_INPUT_PLAN-OCNN,
*           VERS       LIKE ZTPP_INPUT_PLAN-OCNN,
*           MI         LIKE ZTPP_INPUT_PLAN-MI,
*           MODL       LIKE ZTPP_INPUT_PLAN-MODL,
*           NQTY       TYPE P,
DATA : END OF GT_KEY.
*&---------------------------------------------------------------------*
*&  DEFINE VARIABLES
*&---------------------------------------------------------------------*

FIELD-SYMBOLS : <ITAB> TYPE TABLE,
                <LINE> .

DATA: OK_CODE LIKE SY-UCOMM,
      GV_REPID LIKE SY-REPID.

DATA : GV_LINE TYPE I.

CONSTANTS : C_PLANT(2)     TYPE C VALUE 'M1',
            C_PATH(100)    TYPE C VALUE '/usr/sap/EDI_SAP/',
            C_DAYFILE(20)  TYPE C VALUE 'MB_21_DAY',
            C_WEEKFILE(20) TYPE C VALUE 'MB_21_WEEK'.
