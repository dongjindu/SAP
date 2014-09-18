*----------------------------------------------------------------------
* Program ID        : ZACOU118
* Title             : [CO] M/H Report
* Created on        : 10/25/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : M/H Report
* Date         Developer   Request      Description
* 03/15/2007   Manju       UD1K940111   Program bug fix
*                                       exclude GR qty for MIP
*                                       parts
*----------------------------------------------------------------------
REPORT ZACOU118 NO STANDARD PAGE HEADING MESSAGE-ID ZMCO.

INCLUDE ZACOUI00.


*include zacou118_top.
TABLES: ZTCO_MHV, MARC, MARA. " Standard  M/H Actual
*        ZTCOU118.    " [CO] M/H

TYPES: BEGIN OF TY_ACT,
*         KOKRS     TYPE KOKRS,             " Controling Area
*         BDATJ     TYPE BDATJ,             " Fiscal Year
         POPER     TYPE POPER,             " Period
         FEVOR     TYPE FEVOR,             " production schedule
         ARTNR     TYPE ARTNR,             " Product
         SHOP      TYPE ZZSHOP,            " Shop
         KOSTL     TYPE KOSTL,             " Cost center
         GR_QTY    LIKE ZTCO_MHV-GR_QTY,   " GR Qty
         CURR_MH   LIKE ZTCO_MHV-GR_QTY,
         ADD_MH    LIKE ZTCO_MHV-GR_QTY,
         GROSS_MH  LIKE ZTCO_MHV-GR_QTY,
         DEL_MH    LIKE ZTCO_MHV-GR_QTY,
*        DEL_RAT   LIKE ZTCO_MHV-GR_QTY,
         FINAL_MH  LIKE ZTCO_MHV-GR_QTY,
         NET_MH    LIKE ZTCO_MHV-GR_QTY,
         PP_GR   LIKE ZTCO_MHV-GR_QTY,
         DIR_MH2   LIKE ZTCO_MHV-GR_QTY,
         SEMI_MH   LIKE ZTCO_MHV-GR_QTY,
         PC_MH     LIKE ZTCO_MHV-GR_QTY,
         QC_MH     LIKE ZTCO_MHV-GR_QTY,
         PM_MH     LIKE ZTCO_MHV-GR_QTY,
         INDIR_MH  LIKE ZTCO_MHV-GR_QTY,
         ADMIN_MH  LIKE ZTCO_MHV-GR_QTY,
         OS_DIR    LIKE ZTCO_MHV-GR_QTY,
         OS_SEMI   LIKE ZTCO_MHV-GR_QTY,
         OS_PC     LIKE ZTCO_MHV-GR_QTY,
         OS_QC     LIKE ZTCO_MHV-GR_QTY,
         OS_PM     LIKE ZTCO_MHV-GR_QTY,
         OS_INDIR  LIKE ZTCO_MHV-GR_QTY,
         OS_ADMIN  LIKE ZTCO_MHV-GR_QTY,
         PRODH     TYPE PRODH_D,           " Product hierarchy
         MVGR3     TYPE MVGR3,             " Material group 3
         MVGR4     TYPE MVGR4,             " Material group 4
         MVGR5     TYPE MVGR5,             " Material group 5
         LLV_MATNR TYPE ZTCO_MHV-LLV_MATNR,                 "UD1K940111
       END OF TY_ACT.

TYPES: BEGIN OF TY_MH,
         PA,                               " Actual(A)/Plan(P)
         ALVL(18),                         " Analysis level
         PRODH     TYPE PRODH_D,           " Product hierarchy
         MVGR3     TYPE MVGR3,             " Material group 3
         MVGR4     TYPE MVGR4,             " Material group 4
         MVGR5     TYPE MVGR5,             " Material group 5
         DL(3),                            " Dealer code
         ARTNR     TYPE ARTNR,             " Product
         SHOP      TYPE ZZSHOP,            " Shop
         KOSTL     TYPE KOSTL,             " Cost center
         POPER     TYPE POPER,             " Period
         GR_QTY    LIKE ZTCO_MHV-GR_QTY,   " GR Qty
         CURR_MH   LIKE ZTCO_MHV-GR_QTY,
         ADD_MH    LIKE ZTCO_MHV-GR_QTY,
         GROSS_MH  LIKE ZTCO_MHV-GR_QTY,
         DEL_MH    LIKE ZTCO_MHV-GR_QTY,
         FINAL_MH  LIKE ZTCO_MHV-GR_QTY,
         NET_MH    LIKE ZTCO_MHV-GR_QTY,
         PP_GR   LIKE ZTCO_MHV-GR_QTY,
         DIR_MH2   LIKE ZTCO_MHV-GR_QTY,
         SEMI_MH   LIKE ZTCO_MHV-GR_QTY,
         PC_MH     LIKE ZTCO_MHV-GR_QTY,
         QC_MH     LIKE ZTCO_MHV-GR_QTY,
         PM_MH     LIKE ZTCO_MHV-GR_QTY,
         INDIR_MH  LIKE ZTCO_MHV-GR_QTY,
         ADMIN_MH  LIKE ZTCO_MHV-GR_QTY,
         OS_DIR    LIKE ZTCO_MHV-GR_QTY,
         OS_SEMI   LIKE ZTCO_MHV-GR_QTY,
         OS_PC     LIKE ZTCO_MHV-GR_QTY,
         OS_QC     LIKE ZTCO_MHV-GR_QTY,
         OS_PM     LIKE ZTCO_MHV-GR_QTY,
         OS_INDIR  LIKE ZTCO_MHV-GR_QTY,
         OS_ADMIN  LIKE ZTCO_MHV-GR_QTY,
       END OF TY_MH.

TYPES: BEGIN OF TY_MHV,
         PA,                               " Actual(A)/Plan(P)
         POPER     TYPE POPER,             " Period
         ALVL(18),                         " Analysis level
         SHOP      TYPE ZZSHOP,            " Shop
         KOSTL     TYPE KOSTL,             " Cost center
         GR_QTY    LIKE ZTCO_MHV-GR_QTY,   " GR Qty
       END OF TY_MHV.

TYPES: BEGIN OF TY_HP,
         PA,                               " Actual(A)/Plan(P)
         POPER     TYPE POPER,             " Period
         ALVL(18),                         " Analysis level
         SHOP      TYPE ZZSHOP,            " Shop
         KOSTL     TYPE KOSTL,             " Cost center
         PP_GR   LIKE ZTCO_MHV-PP_GR,  " GR Qty
       END OF TY_HP.

TYPES: BEGIN OF TY_ITAB,
         PA,                               " Actual(A)/Plan(P)
         ALVL(18),                         " Analysis level
         PRODH       TYPE PRODH_D,         " Product hierarchy
         MVGR3       TYPE MVGR3,           " Material group 3
         MVGR4       TYPE MVGR4,           " Material group 4
         MVGR5       TYPE MVGR5,           " Material group 5
         DL(3),                            " Dealer code
         ARTNR       TYPE ARTNR,           " Product
         SHOP        TYPE ZZSHOP,          " Shop
         KOSTL       TYPE KOSTL,           " Cost center
         MHTYPE(20),                       " MH Type
         MHTYPE1(35),                      " MH Type for display
         AVG1(15)   TYPE P DECIMALS 3,     " JAN M/H
         AVG2(15)   TYPE P DECIMALS 3,     " FEB M/H
         AVG3(15)   TYPE P DECIMALS 3,     " MAR M/H
         AVG4(15)   TYPE P DECIMALS 3,     " APR M/H
         AVG5(15)   TYPE P DECIMALS 3,     " MAY M/H
         AVG6(15)   TYPE P DECIMALS 3,     " JUN M/H
         AVG7(15)   TYPE P DECIMALS 3,     " JUL M/H
         AVG8(15)   TYPE P DECIMALS 3,     " AUG M/H
         AVG9(15)   TYPE P DECIMALS 3,     " SEP M/H
         AVG10(15)  TYPE P DECIMALS 3,     " OCT M/H
         AVG11(15)  TYPE P DECIMALS 3,     " NOV M/H
         AVG12(15)  TYPE P DECIMALS 3,     " DEC M/H
         TOT(16)    TYPE P DECIMALS 3,     " TOT M/H
       END OF TY_ITAB.

TYPES: BEGIN OF TY_ITAB1,
         PA,                               " Actual(A)/Plan(P)
         ALVL(18),                         " Analysis level
         SHOP        TYPE ZZSHOP,          " Shop
         KOSTL       TYPE KOSTL,           " Cost center
       END OF TY_ITAB1.

TYPES: BEGIN OF TY_OUT.
INCLUDE TYPE TY_ITAB.
TYPES:   PA1(6),                           " Actual/Plan for display
         TABCOLOR TYPE SLIS_T_SPECIALCOL_ALV,
       END OF TY_OUT.

DATA: GT_ACT  TYPE TABLE OF TY_ACT   WITH HEADER LINE,
      GT_PLN  TYPE TABLE OF TY_ACT   WITH HEADER LINE,
      GT_MH   TYPE TABLE OF TY_MH    WITH HEADER LINE,
      GT_MHV  TYPE TABLE OF TY_MHV   WITH HEADER LINE,
      GT_HPV   TYPE TABLE OF TY_HP    WITH HEADER LINE,
      ITAB    TYPE TABLE OF TY_ITAB  WITH HEADER LINE,
      GT_OUT  TYPE TABLE OF TY_OUT   WITH HEADER LINE.

DATA  GV_CHK VALUE 'X'.


DATA: BEGIN OF IT_PRODH OCCURS 0,
        PRODH  TYPE PRODH_D,
        VTEXT  TYPE BEZEI40,
      END OF IT_PRODH.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
* Block: Report Type
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-T00.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETER P_HPV  RADIOBUTTON GROUP R2 .
SELECTION-SCREEN COMMENT 5(3) TEXT-P01 FOR FIELD P_HPV.

SELECTION-SCREEN POSITION 11.
PARAMETER P_HPE  RADIOBUTTON GROUP R2.
SELECTION-SCREEN COMMENT 16(3) TEXT-P02 FOR FIELD P_HPE.

SELECTION-SCREEN POSITION 21.
PARAMETER P_HPP  RADIOBUTTON GROUP R2.
SELECTION-SCREEN COMMENT 25(3) TEXT-P03 FOR FIELD P_HPP.

SELECTION-SCREEN POSITION 31.
PARAMETER P_MHV  RADIOBUTTON GROUP R2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 40(3) TEXT-P04 FOR FIELD P_MHV.

SELECTION-SCREEN END OF LINE.

PARAMETERS: P_ACT AS CHECKBOX DEFAULT 'X',
            P_PLN AS CHECKBOX.
PARAMETERS: P_MON RADIOBUTTON GROUP R1 DEFAULT 'X',  " Monthly Report
            P_DAY RADIOBUTTON GROUP R1 MODIF ID A.   " Day Report

SELECTION-SCREEN END OF BLOCK B0.

* Block: General Selection
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-T01.
PARAMETERS: P_KOKRS LIKE ZTCO_MHV-KOKRS OBLIGATORY
                                        MEMORY ID CAC
                                        MATCHCODE OBJECT FC_KOKRS,
            P_YEAR  LIKE ZTCO_MHV-BDATJ OBLIGATORY MEMORY ID BDTJ,
            P_KLVAR LIKE CKI64A-KLVAR DEFAULT 'ZPCP'.
SELECT-OPTIONS S_POPER FOR ZTCO_MHV-POPER.
SELECTION-SCREEN END OF BLOCK B1.

* Block: Select Measure
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-T02.

SELECTION-SCREEN END OF BLOCK B2.

* Block: Analysis Level
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-T03.
PARAMETERS: P_VCL AS CHECKBOX DEFAULT 'X',      " Vehicle model
            P_EGN AS CHECKBOX,                  " Engine
            P_TRM AS CHECKBOX,                  " Trim level
            P_TM  AS CHECKBOX,                  " TM
            P_DL  AS CHECKBOX.                  " Dealer code
SELECTION-SCREEN END OF BLOCK B3.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-T05.
SELECT-OPTIONS: S_FEVOR FOR MARC-FEVOR,
                S_MTART FOR MARA-MTART.
SELECT-OPTIONS  S_ARTNR FOR ZTCO_MHV-ARTNR.
SELECTION-SCREEN END OF BLOCK B4.
PARAMETERS: P_VARI TYPE SLIS_VARI.

*----------------------------------------------------------------------
* AT SELECTION-SCREEN
*----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'A'.
      SCREEN-INTENSIFIED = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------
* AT SELECTION-SCREEN ON VALUE-REQUEST
*----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

*----------------------------------------------------------------------
* Start of selection
*----------------------------------------------------------------------
START-OF-SELECTION.
  IF P_VCL = 'X' OR
     P_EGN = 'X' OR
     P_TRM = 'X' OR
     P_TM  = 'X' OR
     P_DL  = 'X'.
*    if p_hpv <> 'X' and p_mhv <> 'X'.
*      message s000 with 'Choose Analysis level only HPV or MHV.'.
*      exit.
*    endif.
  ELSE.
    CLEAR GV_CHK.
  ENDIF.

*  if p_hpv <> 'X' and p_mhv <> 'X'.
*    clear gv_chk.
*  endif.

* Create Internal Table GT_ITAB for Actual & Plan Data
  PERFORM GET_DATA.

  IF ITAB[] IS INITIAL.
    MESSAGE S000 WITH 'No data found.'.
    EXIT.
  ELSE.
*   Create Internal Table GT_OUT for display
    PERFORM GET_GT_OUT.
  ENDIF.

*----------------------------------------------------------------------
* End of selection
*----------------------------------------------------------------------
END-OF-SELECTION.

  PERFORM DISP_RESULT.

*&---------------------------------------------------------------------
*&      Form  GET_DATA
*&---------------------------------------------------------------------
*       Get Costing Data
*----------------------------------------------------------------------
FORM GET_DATA.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_PRODH
    FROM T179T
    WHERE SPRAS = SY-LANGU.

* Get Actual MH
  IF P_ACT = 'X'.
    PERFORM GET_ACTUAL.
  ENDIF.

* Get Plan MH
  IF P_PLN = 'X'.
    PERFORM GET_PLAN.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ACTUAL
*&---------------------------------------------------------------------*
*       Get Actual Data
*----------------------------------------------------------------------*
FORM   GET_ACTUAL.

  PERFORM GET_GT_ACT.

  IF P_MON = 'X'.
    PERFORM GET_GT_MH  USING 'A'.

    CLEAR ITAB.
    REFRESH ITAB.

    IF P_MHV = 'X'.
      PERFORM GET_ITAB_MHV USING 'A'.
    ELSE.
      PERFORM GET_ITAB_HP  USING 'A'.
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_ACTUAL
*&---------------------------------------------------------------------*
*&      Form  GET_GT_ACT
*&---------------------------------------------------------------------*
*       Get Actual MH
*----------------------------------------------------------------------*
FORM GET_GT_ACT.
  CLEAR GT_ACT.
  REFRESH GT_ACT.

  IF P_HPV = 'X'.
    SELECT POPER     ARTNR     SHOP   KOSTL
           GR_QTY    CURR_MH   ADD_MH    GROSS_MH
           DEL_MH    FINAL_MH  NET_MH
           PP_GR   DIR_MH2 AS DIR_MH2
           SEMI_MH   PC_MH     QC_MH  PM_MH  INDIR_MH  ADMIN_MH
           OS_DIR    OS_SEMI   OS_PC  OS_QC
           OS_PM     OS_INDIR  OS_ADMIN
           C~FEVOR
           D~PRDHA AS PRODH
           B~MVGR3     B~MVGR4     B~MVGR5 LLV_MATNR
      INTO CORRESPONDING FIELDS OF TABLE GT_ACT
      FROM ZTCO_MHV AS A
      JOIN MVKE AS B
        ON B~MATNR = A~ARTNR
       AND B~LVORM = SPACE         "not deleted
      JOIN MARC AS C
        ON C~MATNR = A~ARTNR
       AND C~BESKZ = 'E'
      JOIN MARA AS D
        ON D~MATNR = C~MATNR
     WHERE KOKRS = P_KOKRS
       AND BDATJ = P_YEAR
       AND ARTNR IN S_ARTNR
       AND POPER IN S_POPER
       AND C~FEVOR IN S_FEVOR
       AND ( SHOP = 'MXBX' OR SHOP = 'MXPX' OR SHOP = 'MXTX' )
       AND C~SFEPR = 'VEHI'
       AND D~MTART IN S_MTART.

  ELSEIF P_HPE = 'X'.
    SELECT POPER     ARTNR     SHOP   KOSTL
           GR_QTY    CURR_MH   ADD_MH    GROSS_MH
           DEL_MH    "del_rat
           FINAL_MH  NET_MH
           PP_GR   DIR_MH2 AS DIR_MH2
           SEMI_MH   PC_MH     QC_MH  PM_MH  INDIR_MH  ADMIN_MH
           OS_DIR    OS_SEMI   OS_PC  OS_QC
           OS_PM     OS_INDIR  OS_ADMIN
           C~FEVOR
           D~PRDHA AS PRODH LLV_MATNR
      INTO CORRESPONDING FIELDS OF TABLE GT_ACT
      FROM ZTCO_MHV AS A
      JOIN MARC AS C
        ON C~MATNR = A~ARTNR
       AND C~BESKZ = 'E'
      JOIN MARA AS D
        ON D~MATNR = C~MATNR
     WHERE KOKRS = P_KOKRS
       AND BDATJ = P_YEAR
       AND POPER IN S_POPER
       AND ARTNR IN S_ARTNR
       AND SHOP = 'MXEX'
       AND C~FEVOR IN S_FEVOR
       AND C~FEVOR = 'SEA'
       AND D~MTART IN S_MTART.

  ELSEIF P_HPP = 'X'.
    SELECT POPER     ARTNR     SHOP   KOSTL
           GR_QTY    CURR_MH   ADD_MH    GROSS_MH
           DEL_MH    "del_rat
           FINAL_MH  NET_MH
           PP_GR   DIR_MH2 AS DIR_MH2
           SEMI_MH   PC_MH     QC_MH  PM_MH  INDIR_MH  ADMIN_MH
           OS_DIR    OS_SEMI   OS_PC  OS_QC
           OS_PM     OS_INDIR  OS_ADMIN
           C~FEVOR
           D~PRDHA AS PRODH LLV_MATNR
      INTO CORRESPONDING FIELDS OF TABLE GT_ACT
      FROM ZTCO_MHV AS A
      JOIN MARC AS C
        ON C~MATNR = A~ARTNR
       AND C~BESKZ = 'E'
      JOIN MARA AS D
        ON D~MATNR = C~MATNR
     WHERE KOKRS = P_KOKRS
       AND BDATJ = P_YEAR
       AND POPER IN S_POPER
       AND ARTNR IN S_ARTNR
       AND SHOP = 'MXSX'
       AND C~FEVOR IN S_FEVOR
       AND C~FEVOR = 'SPP'
       AND D~MTART IN S_MTART.

  ELSE.
    SELECT POPER     ARTNR     SHOP   KOSTL
           GR_QTY    CURR_MH   ADD_MH    GROSS_MH
           DEL_MH    "del_rat
           FINAL_MH  NET_MH
           PP_GR   DIR_MH2 AS DIR_MH2
           SEMI_MH   PC_MH     QC_MH  PM_MH  INDIR_MH  ADMIN_MH
           OS_DIR    OS_SEMI   OS_PC  OS_QC
           OS_PM     OS_INDIR  OS_ADMIN
           C~FEVOR
           D~PRDHA AS PRODH  LLV_MATNR
      INTO CORRESPONDING FIELDS OF TABLE GT_ACT
      FROM ZTCO_MHV AS A
*      join mvke as b               "sales view : ONE!!!
*        on b~matnr = a~artnr
*       and b~lvorm = space         "not deleted
      JOIN MARC AS C
        ON C~MATNR = A~ARTNR
       AND C~BESKZ = 'E'
      JOIN MARA AS D
        ON D~MATNR = C~MATNR
     WHERE KOKRS   = P_KOKRS
       AND BDATJ   = P_YEAR
       AND POPER   IN S_POPER
       AND ARTNR   IN S_ARTNR
       AND C~FEVOR IN S_FEVOR
       AND D~MTART IN S_MTART.

    PERFORM FILL_MATERIAL_GROUPS.

  ENDIF.

ENDFORM.                    " GET_GT_ACT
*&---------------------------------------------------------------------*
*&      Form  GET_ALVL
*&---------------------------------------------------------------------*
*       Get Analysis level
*----------------------------------------------------------------------*
FORM GET_ALVL CHANGING P_PRODH   TYPE PRODH_D
                       P_MVGR4   TYPE MVGR4
                       P_MVGR5   TYPE MVGR5
                       P_MVGR3   TYPE MVGR3
                       P_DEAL    TYPE CHAR03
                       P_ARTNR   TYPE ARTNR
                       P_FEVOR   TYPE FEVOR
                       P_ALVL    TYPE CHAR18.
* P_VCL = 'X' : Vehicle model : product hierachy : GT_ACT-PRODH
* P_EGN = 'X' : Engine        : Material group 4 : GT_ACT-MVGR4
* P_TRM = 'X' : Trim level    : Material group 5 : GT_ACT-MVGR5
* P_TM = 'X'  : T/M           : Material group 3 : GT_ACT-MVGR3
* P_DL = 'X'  : Dealer code   : 2nd 3 digit of Material Number
* Vehicle model
  IF P_VCL = 'X'.
    P_PRODH = ITAB-PRODH = GT_ACT-PRODH.
  ENDIF.

* Engine
  IF P_EGN = 'X'.
    P_MVGR4 = ITAB-MVGR4 = GT_ACT-MVGR4.
  ENDIF.

* Trim level
  IF P_TRM = 'X'.
    P_MVGR5 = ITAB-MVGR5 = GT_ACT-MVGR5.
  ENDIF.

* TM
  IF P_TM = 'X'.
    P_MVGR3 = ITAB-MVGR3 = GT_ACT-MVGR3.
  ENDIF.

* Dealer code
  IF P_DL = 'X'.
    P_DEAL = ITAB-DL = GT_ACT-ARTNR+1(3).
  ENDIF.

* when dosen't choose Analysis level, Analysis level is product
  IF GV_CHK IS INITIAL.
    P_ALVL = P_ARTNR = GT_ACT-ARTNR.
  ELSE.
    CONCATENATE P_PRODH '-' P_FEVOR ':'
                P_MVGR4 '-' P_MVGR5 '-' P_MVGR3 '-' P_DL
           INTO P_ALVL.
    CONDENSE P_ALVL.

    P_ARTNR = GT_ACT-ARTNR.
  ENDIF.

ENDFORM.                    " GET_ALVL
*&---------------------------------------------------------------------*
*&      Form  GET_TITLE
*&---------------------------------------------------------------------*
*       Get report title
*----------------------------------------------------------------------*
FORM GET_TITLE CHANGING P_TITLE TYPE LVC_TITLE.
  IF P_HPV = 'X'.
    P_TITLE = 'HPV'.
  ELSEIF P_HPE = 'X'.
    P_TITLE = 'HPE'.
  ELSEIF P_HPP = 'X'.
    P_TITLE = 'HPP'.
  ELSEIF P_MHV = 'X'.
    P_TITLE = 'MHV'.
  ENDIF.

  IF P_MON = 'X'.
    CONCATENATE P_TITLE 'Monthly Report'
           INTO P_TITLE SEPARATED BY SPACE.
  ELSEIF P_DAY = 'X'.
    CONCATENATE P_TITLE 'Day Type Report'
           INTO P_TITLE SEPARATED BY SPACE.
  ENDIF.

ENDFORM.                    " GET_TITLE
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
FORM GET_GT_OUT.
  CLEAR GT_OUT.
  REFRESH GT_OUT.

  LOOP AT ITAB.
    MOVE-CORRESPONDING ITAB TO GT_OUT.

    IF ITAB-PA = 'A'.
      GT_OUT-PA1 = 'ACTUAL'.
    ELSEIF ITAB-PA = 'P'.
      GT_OUT-PA1 = 'PLAN'.
    ENDIF.

    APPEND GT_OUT.
    CLEAR GT_OUT.
  ENDLOOP.

ENDFORM.                    " GET_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_PLAN
*&---------------------------------------------------------------------*
*       Get Plan Data
*----------------------------------------------------------------------*
FORM GET_PLAN.

  PERFORM GET_GT_ACT_PLN.

  IF P_MON = 'X'.
    PERFORM GET_GT_MH USING 'P'.

    IF P_MHV = 'X'.
      PERFORM GET_ITAB_MHV USING 'P'.
    ELSE.
      PERFORM GET_ITAB_HP  USING 'P'.
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_PLAN
*&---------------------------------------------------------------------
*&      Form  DISP_RESULT
*&---------------------------------------------------------------------
*       Display M/H
*----------------------------------------------------------------------
FORM DISP_RESULT.
  CLEAR: GT_FIELDCAT, GS_LAYOUT, GT_SORT, GT_EVENTS,
         GT_FIELDCAT[], GT_SORT[], GT_EVENTS[].

* Set field category
  PERFORM SET_FIELD_CATEGORY.

* Set layout
  CLEAR GS_LAYOUT.
  GS_LAYOUT-ZEBRA             = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GS_LAYOUT-COLTAB_FIELDNAME  = 'TABCOLOR'.

* Set sort
  PERFORM SET_SORT.

* Set color
  PERFORM SET_COLOR.

* Set list header
  PERFORM COMMENT1 USING GT_LIST_TOP_OF_PAGE.

* Set event
* event 'USER_COMMAND'.
  DATA: LS_ALV_EVENTS    TYPE SLIS_ALV_EVENT.
  CLEAR LS_ALV_EVENTS.
  LS_ALV_EVENTS-NAME = SLIS_EV_USER_COMMAND.
  LS_ALV_EVENTS-FORM = 'ALV_USER_COMMAND'.
  APPEND LS_ALV_EVENTS TO GT_EVENTS.
*  perform set_events changing gt_events.

* Set callback program
  GV_REPID = GS_VARIANT-REPORT = SY-REPID.

* Set variant
  GS_VARIANT-VARIANT = P_VARI.

* Display alv grid
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = GV_REPID
            I_CALLBACK_PF_STATUS_SET = GC_PF_STATUS_SET
            IS_LAYOUT                = GS_LAYOUT
            IT_FIELDCAT              = GT_FIELDCAT[]
            IT_SORT                  = GT_SORT_ALV[]
            IT_EVENTS                = GT_EVENTS[]
            I_SAVE                   = 'A'
            IS_VARIANT               = GS_VARIANT
       TABLES
            T_OUTTAB                 = GT_OUT.

ENDFORM.                    " DISP_RESULT
*---------------------------------------------------------------------*
*       FORM ALV_USER_COMMAND                                         *
*---------------------------------------------------------------------*
*       user command for ALV processing                               *
*---------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING R_UCOMM     LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  CHECK R_UCOMM = '&DATA_SAVE'.

  PERFORM SAVE_DATA.

ENDFORM.                               "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Set field category
*----------------------------------------------------------------------*
FORM SET_FIELD_CATEGORY.
  IF P_MON = 'X'.
    CLEAR GS_FIELDCAT.
    GS_FIELDCAT-FIELDNAME = 'ALVL'.
    GS_FIELDCAT-KEY       = 'X'.
    GS_FIELDCAT-SELTEXT_L = 'Analysis '.
    GS_FIELDCAT-OUTPUTLEN = 18.
    GS_FIELDCAT-DATATYPE  = 'CHAR'.
    GS_FIELDCAT-NO_OUT = 'X'.
    APPEND GS_FIELDCAT TO GT_FIELDCAT.

    IF P_HPV = 'X' OR P_MHV = 'X'.
      IF GV_CHK IS INITIAL.
        PERFORM BUILD_FIELD_CATEGORY USING:
          'ARTNR'   'X'  'Product'  18    'CHAR'.
      ENDIF.

      IF P_VCL = 'X'.
        PERFORM BUILD_FIELD_CATEGORY USING:
          'PRODH'  'X'  'Vehicle model'  18 'CHAR'.
      ENDIF.

      IF P_EGN = 'X'.
        PERFORM BUILD_FIELD_CATEGORY USING:
          'MVGR4'  'X'  'Engine'  3 'CHAR'.
      ENDIF.

      IF P_TRM = 'X'.
        PERFORM BUILD_FIELD_CATEGORY USING:
          'MVGR5'  'X'  'Trim level'  3 'CHAR'.
      ENDIF.

      IF P_TM = 'X'.
        PERFORM BUILD_FIELD_CATEGORY USING:
          'MVGR3'  'X'  'T/M'  3 'CHAR'.
      ENDIF.

      IF P_DL = 'X'.
        PERFORM BUILD_FIELD_CATEGORY USING:
          'DL'  'X'   'Dealer code'  3 'CHAR'.
      ENDIF.

    ELSE.
      PERFORM BUILD_FIELD_CATEGORY USING:
        'ARTNR'   'X'  'Product'  18    'CHAR'.
    ENDIF.

    PERFORM BUILD_FIELD_CATEGORY USING:
      'PA1'     'X'  'P/A'       6     'CHAR',
      'SHOP'    ' '  'Shop'      4     'CHAR',
      'KOSTL'   ' '  'WC'        10    'CHAR',
      'MHTYPE'  ' '  'MH'        10    'CHAR',
      'MHTYPE1' ' '  'MH Type'   20    'CHAR',
      'AVG1'    ' '  'JAN'       13    'DEC',
      'AVG2'    ' '  'FEB'       13    'DEC',
      'AVG3'    ' '  'MAR'       13    'DEC',
      'AVG4'    ' '  'APR'       13    'DEC',
      'AVG5'    ' '  'MAY'       13    'DEC',
      'AVG6'    ' '  'JUN'       13    'DEC',
      'AVG7'    ' '  'JLY'       13    'DEC',
      'AVG8'    ' '  'AUG'       13    'DEC',
      'AVG9'    ' '  'SEP'       13    'DEC',
      'AVG10'   ' '  'OCT'       13    'DEC',
      'AVG11'   ' '  'NOV'       13    'DEC',
      'AVG12'   ' '  'DEC'       13    'DEC',
      'TOT'     ' '  'TOT'       13    'DEC'.
  ENDIF.

  GS_FIELDCAT-DO_SUM = 'X'.
  GS_FIELDCAT-DECIMALS_OUT = 3.
  MODIFY GT_FIELDCAT FROM GS_FIELDCAT
   TRANSPORTING DO_SUM DECIMALS_OUT
   WHERE DATATYPE = 'DEC'.

ENDFORM.                    " SET_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SET_SORT : Set sort
*&---------------------------------------------------------------------*
FORM SET_SORT.
  CLEAR GS_SORT_ALV.
  GS_SORT_ALV-SPOS = 1.
  GS_SORT_ALV-FIELDNAME = 'PA1'.
  GS_SORT_ALV-TABNAME = 'GT_OUT'.
  GS_SORT_ALV-UP = 'X'.
  GS_SORT_ALV-SUBTOT = 'X'.
  APPEND GS_SORT_ALV TO GT_SORT_ALV.
*
  GS_SORT_ALV-SPOS = 2.
  IF GV_CHK IS INITIAL.
    GS_SORT_ALV-FIELDNAME = 'ARTNR'.
    APPEND GS_SORT_ALV TO GT_SORT_ALV.
  ELSE.
    IF P_HPV = 'X' OR P_MHV = 'X'.
      GS_SORT_ALV-FIELDNAME = 'ALVL'.
      APPEND GS_SORT_ALV TO GT_SORT_ALV.

      IF P_VCL = 'X'.
        GS_SORT_ALV-SPOS = GS_SORT_ALV-SPOS + 1.
        GS_SORT_ALV-FIELDNAME = 'PRODH'.
        CLEAR GS_SORT_ALV-SUBTOT.
        APPEND GS_SORT_ALV TO GT_SORT_ALV.
      ENDIF.

      IF P_EGN = 'X'.
        GS_SORT_ALV-SPOS = GS_SORT_ALV-SPOS + 1.
        GS_SORT_ALV-FIELDNAME = 'MVGR4'.
        CLEAR GS_SORT_ALV-SUBTOT.
        APPEND GS_SORT_ALV TO GT_SORT_ALV.
      ENDIF.

      IF P_TRM = 'X'.
        GS_SORT_ALV-SPOS = GS_SORT_ALV-SPOS + 1.
        GS_SORT_ALV-FIELDNAME = 'MVGR5'.
        CLEAR GS_SORT_ALV-SUBTOT.
        APPEND GS_SORT_ALV TO GT_SORT_ALV.
      ENDIF.

      IF P_TM = 'X'.
        GS_SORT_ALV-SPOS = GS_SORT_ALV-SPOS + 1.
        GS_SORT_ALV-FIELDNAME = 'MVGR3'.
        CLEAR GS_SORT_ALV-SUBTOT.
        APPEND GS_SORT_ALV TO GT_SORT_ALV.
      ENDIF.

      IF P_DL = 'X'.
        GS_SORT_ALV-SPOS = GS_SORT_ALV-SPOS + 1.
        GS_SORT_ALV-FIELDNAME = 'MVGR3'.
        CLEAR GS_SORT_ALV-SUBTOT.
        APPEND GS_SORT_ALV TO GT_SORT_ALV.
      ENDIF.

    ENDIF.
  ENDIF.

  GS_SORT_ALV-SPOS = GS_SORT_ALV-SPOS + 1.
  GS_SORT_ALV-FIELDNAME = 'SHOP'.
  CLEAR GS_SORT_ALV-SUBTOT.
  APPEND GS_SORT_ALV TO GT_SORT_ALV.

  GS_SORT_ALV-SPOS = GS_SORT_ALV-SPOS + 1.
  GS_SORT_ALV-FIELDNAME = 'KOSTL'.
  APPEND GS_SORT_ALV TO GT_SORT_ALV.

ENDFORM.                    " SET_SORT
*&---------------------------------------------------------------------
*&      Form  SET_COLOR : Set color
*&---------------------------------------------------------------------
FORM SET_COLOR.
  CLEAR: GS_SPECIALCOL, GT_SPECIALCOL[], GT_OUT-TABCOLOR[].

  GS_SPECIALCOL-FIELDNAME = 'MHTYPE1'.
  GS_SPECIALCOL-COLOR-COL = '5'.
  GS_SPECIALCOL-COLOR-INT = 0.
  APPEND GS_SPECIALCOL TO GT_SPECIALCOL.

  CLEAR GS_SPECIALCOL.
  GS_SPECIALCOL-FIELDNAME = 'TOT'.
  GS_SPECIALCOL-COLOR-COL = '7'.
  GS_SPECIALCOL-COLOR-INT = 0.
  APPEND GS_SPECIALCOL TO GT_SPECIALCOL.

  GT_OUT-TABCOLOR[] = GT_SPECIALCOL[].
  MODIFY GT_OUT TRANSPORTING TABCOLOR WHERE TABCOLOR IS INITIAL.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------
*&      Form  COMMENT1 : Set list header
*&---------------------------------------------------------------------
FORM COMMENT1 USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA LS_LINE TYPE SLIS_LISTHEADER.

  DATA L_TITLE TYPE LVC_TITLE.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.

  IF P_HPV = 'X'.
    L_TITLE = 'HPV'.
  ELSEIF P_HPE = 'X'.
    L_TITLE = 'HPE'.
  ELSEIF P_HPP = 'X'.
    L_TITLE = 'HPP'.
  ELSEIF P_MHV = 'X'.
    L_TITLE = 'MHV'.
  ENDIF.

  IF P_MON = 'X'.
    CONCATENATE L_TITLE 'Monthly Report'
           INTO L_TITLE SEPARATED BY SPACE.
  ELSEIF P_DAY = 'X'.
    CONCATENATE L_TITLE 'Day Type Report'
           INTO L_TITLE SEPARATED BY SPACE.
  ENDIF.

  LS_LINE-KEY  = L_TITLE.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
*
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
*
  LS_LINE-KEY  = 'Controlling Area:'.
  LS_LINE-INFO = P_KOKRS.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'Year:'.
  LS_LINE-INFO = P_YEAR.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'Compare with:'.
  LS_LINE-INFO = P_KLVAR.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CLEAR: LS_LINE-KEY, LS_LINE-INFO.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

ENDFORM.                                                    " COMMENT1
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       Save M/H to table ZTCOU118
*----------------------------------------------------------------------*
FORM SAVE_DATA.
  DATA: L_CNT(10),
        L_KEYFG(3) TYPE C.
  DATA: IT_SAVE LIKE ZTCO_MHV_SUM OCCURS 0 WITH HEADER LINE.
  IF P_HPV = 'X'.
    L_KEYFG = 'HPV'.
  ELSEIF P_HPE = 'X'.
    L_KEYFG = 'HPE'.
  ELSEIF P_HPP = 'X'.
    L_KEYFG = 'HPP'.
  ELSE.
    L_KEYFG = 'MHV'.
  ENDIF.
  READ TABLE S_POPER INDEX 1.

* only allow saving in detail level for prod.hiearchy.
  IF P_VCL = 'X'
  AND  NOT GT_OUT[] IS INITIAL
  AND NOT S_POPER[] IS INITIAL.
    CLEAR L_CNT.

    REFRESH IT_SAVE.
    LOOP AT GT_OUT.

      MOVE-CORRESPONDING GT_OUT TO IT_SAVE.

      CASE S_POPER-LOW.
        WHEN '001'. IT_SAVE-POPER = 1.  IT_SAVE-AVGXX = GT_OUT-AVG1.
        WHEN '002'. IT_SAVE-POPER = 2.  IT_SAVE-AVGXX = GT_OUT-AVG2.
        WHEN '003'. IT_SAVE-POPER = 3.  IT_SAVE-AVGXX = GT_OUT-AVG3.
        WHEN '004'. IT_SAVE-POPER = 4.  IT_SAVE-AVGXX = GT_OUT-AVG4.
        WHEN '005'. IT_SAVE-POPER = 5.  IT_SAVE-AVGXX = GT_OUT-AVG5.
        WHEN '006'. IT_SAVE-POPER = 6.  IT_SAVE-AVGXX = GT_OUT-AVG6.
        WHEN '007'. IT_SAVE-POPER = 7.  IT_SAVE-AVGXX = GT_OUT-AVG7.
        WHEN '008'. IT_SAVE-POPER = 8.  IT_SAVE-AVGXX = GT_OUT-AVG8.
        WHEN '009'. IT_SAVE-POPER = 9.  IT_SAVE-AVGXX = GT_OUT-AVG9.
        WHEN '010'. IT_SAVE-POPER = 10. IT_SAVE-AVGXX = GT_OUT-AVG10.
        WHEN '011'. IT_SAVE-POPER = 11. IT_SAVE-AVGXX = GT_OUT-AVG11.
        WHEN '012'. IT_SAVE-POPER = 12. IT_SAVE-AVGXX = GT_OUT-AVG12.
      ENDCASE.

      IT_SAVE-KEYFG = L_KEYFG.
      IT_SAVE-KOKRS = P_KOKRS.
      IT_SAVE-BDATJ = P_YEAR.
      IT_SAVE-AEDAT = SY-DATUM.
      IT_SAVE-AENAM = SY-UNAME.
      IT_SAVE-MANDT = SY-MANDT.
      COLLECT IT_SAVE.
    ENDLOOP.

    IF P_PLN = 'X'.
      DELETE FROM ZTCO_MHV_SUM WHERE KOKRS = P_KOKRS
                                 AND KEYFG = L_KEYFG
                                 AND BDATJ = P_YEAR
                                 AND POPER = S_POPER-LOW
                                 AND PA    = 'P'.
    ENDIF.
    IF P_ACT = 'X'.
      DELETE FROM ZTCO_MHV_SUM WHERE KOKRS = P_KOKRS
                                 AND KEYFG = L_KEYFG
                                 AND BDATJ = P_YEAR
                                 AND POPER = S_POPER-LOW
                                 AND PA    = 'A'.
    ENDIF.

    INSERT ZTCO_MHV_SUM FROM TABLE IT_SAVE.
    MESSAGE S000 WITH 'Data is saved ' SY-DBCNT 'records.'.

  ENDIF.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ITAB_HP
*&---------------------------------------------------------------------*
FORM GET_ITAB_HP  USING P_PA.

  LOOP AT GT_MH.
    MOVE-CORRESPONDING GT_MH TO ITAB.

    PERFORM GET_AVG USING: 'DIR_MH2' , "1.Direct MH',
                           'SEMI_MH' , "2.Semi direct MH',
                           'PC_MH'   , "3.Production Control MH',
                           'QC_MH'   , "4.Quality Control MH',
                           'PM_MH'   , "5.Maintenance MH',
                           'INDIR_MH', "6.Indirect MH',
                           'ADMIN_MH', "7.Admin MH',
                           'OS_DIR'  , "8.Outsource:Direct',
                           'OS_SEMI' , "9.Outsource:Semi direct',
                           'OS_PC'   , "A.Outsource:Production Control',
                           'OS_QC'   , "B.Outsource:Quality Control',
                           'OS_PM'   , "C.Outsource:Maintenance',
                           'OS_INDIR', "D.Outsource:Indirect',
                           'OS_ADMIN'. "E.Outsource:Admin'.

  ENDLOOP.

  LOOP AT ITAB.
    IF ITAB-MHTYPE+0(6) <> 'GR_QTY' AND ITAB-TOT <> 0.
      ITAB-TOT = ITAB-TOT / 12.
    ENDIF.

    CASE ITAB-MHTYPE.
      WHEN 'DIR_MH2' . ITAB-MHTYPE1 = '1.Direct MH'.
      WHEN 'SEMI_MH' . ITAB-MHTYPE1 = '2.Semi direct MH'.
      WHEN 'PC_MH'   . ITAB-MHTYPE1 = '3.Production Control MH'.
      WHEN 'QC_MH'   . ITAB-MHTYPE1 = '4.Quality Control MH'.
      WHEN 'PM_MH'   . ITAB-MHTYPE1 = '5.Maintenance MH'.
      WHEN 'INDIR_MH'. ITAB-MHTYPE1 = '6.Indirect MH'.
      WHEN 'ADMIN_MH'. ITAB-MHTYPE1 = '7.Admin MH'.
      WHEN 'OS_DIR'  . ITAB-MHTYPE1 = '8.Outsource:Direct'.
      WHEN 'OS_SEMI' . ITAB-MHTYPE1 = '9.Outsource:Semi direct'.
      WHEN 'OS_PC'   . ITAB-MHTYPE1 = 'A.Outsource:Production Control'.
      WHEN 'OS_QC'   . ITAB-MHTYPE1 = 'B.Outsource:Quality Control'.
      WHEN 'OS_PM'   . ITAB-MHTYPE1 = 'C.Outsource:Maintenance'.
      WHEN 'OS_INDIR'. ITAB-MHTYPE1 = 'D.Outsource:Indirect'.
      WHEN 'OS_ADMIN'. ITAB-MHTYPE1 = 'E.Outsource:Admin'.
    ENDCASE.

    MODIFY ITAB.
  ENDLOOP.

ENDFORM.                    " GET_ITAB_HP
*&---------------------------------------------------------------------*
*&      Form  GET_AVG
*&---------------------------------------------------------------------*
*       Get M/H
*----------------------------------------------------------------------*
FORM GET_AVG USING P_MHTYPE  TYPE CHAR20.
*                  P_MHTYPE1 TYPE CHAR35.
  FIELD-SYMBOLS: <FS>  TYPE ANY,
                 <FS1> TYPE ANY.

  DATA: L_NAME(20),
        N(2),
        L_TOT(15) TYPE P DECIMALS 3.

  CLEAR: N, L_TOT.

  ITAB-MHTYPE = P_MHTYPE.
* ITAB-MHTYPE1 = P_MHTYPE1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = GT_MH-POPER
       IMPORTING
            OUTPUT = N.

  CONCATENATE 'GT_MH-' ITAB-MHTYPE INTO L_NAME.
  ASSIGN (L_NAME) TO <FS>.

  IF NOT <FS> IS INITIAL.
    CONCATENATE 'ITAB-AVG' N INTO L_NAME.
    ASSIGN (L_NAME) TO <FS1>.
    <FS1> = <FS>.

    ITAB-TOT = ITAB-TOT + <FS1>.
  ENDIF.

  COLLECT ITAB.
  CLEAR: ITAB-MHTYPE, ITAB-MHTYPE1,
         ITAB-AVG1,  ITAB-AVG2,  ITAB-AVG3,
         ITAB-AVG4,  ITAB-AVG5,  ITAB-AVG6,
         ITAB-AVG7,  ITAB-AVG8,  ITAB-AVG9,
         ITAB-AVG10, ITAB-AVG11, ITAB-AVG12,
         ITAB-TOT.

ENDFORM.                                                    " GET_AVG
*&---------------------------------------------------------------------*
*&      Form  GET_ITAB_MHV
*&---------------------------------------------------------------------*
FORM GET_ITAB_MHV  USING P_PA.
  LOOP AT GT_MH.
    MOVE-CORRESPONDING GT_MH TO ITAB.

    PERFORM GET_AVG USING: 'CURR_MH' , "1.Current MH',
                           'ADD_MH'  ,                      "2.Add MH',
                           'GROSS_MH', "3.Gross MH',
                           'DEL_MH'  , "4.Gross MH:Delta',
*                          'DEL_RAT' , "5.Delta MH Rate',
                           'FINAL_MH', "6.Gross MH:Final',
                           'NET_MH'  ,                      "7.Net MH',
                           'OS_DIR'  .                      "8.OS  MH'.

  ENDLOOP.

  LOOP AT ITAB.
    IF ITAB-MHTYPE+0(6) <> 'GR_QTY' AND ITAB-TOT <> 0.
      ITAB-TOT = ITAB-TOT / 12.
    ENDIF.

    CASE ITAB-MHTYPE.
      WHEN 'CURR_MH' . ITAB-MHTYPE1 = '1.MH-ClsCur'.
      WHEN 'ADD_MH'  . ITAB-MHTYPE1 = '2.MH-ClsAdd'.
      WHEN 'GROSS_MH'. ITAB-MHTYPE1 = '3.MH-ClsTot'.
      WHEN 'DEL_MH'  . ITAB-MHTYPE1 = '4.MH-Changed'.
*     when 'DEL_RAT' . itab-mhtype1 = '5.Delta MH Rate'.
      WHEN 'FINAL_MH'. ITAB-MHTYPE1 = '6.MH-Final'.
*     when 'NET_MH'  . itab-mhtype1 = '7.Net MH'.
      WHEN 'OS_DIR'  . ITAB-MHTYPE1 = '8.MH-Temp'.
    ENDCASE.

    MODIFY ITAB.
  ENDLOOP.


ENDFORM.                    " GET_ITAB_MHV
*&---------------------------------------------------------------------*
*&      Form  GET_GT_MH
*&---------------------------------------------------------------------*
FORM GET_GT_MH USING P_PA.

* Get Internal table for summary by analysis level
  PERFORM GET_SUMMARY_BY_ALVL USING P_PA.

  DATA L_RATE  TYPE ZAVG1.

  CLEAR GT_MH.
  REFRESH GT_MH.

  LOOP AT GT_ACT.
    GT_MH-PA = P_PA.

    IF GV_CHK = 'X'.  "p_hpv = 'X' or p_mhv = 'X'.
*     Get Analysis level
      PERFORM GET_ALVL CHANGING GT_MH-PRODH
                                GT_MH-MVGR4
                                GT_MH-MVGR5
                                GT_MH-MVGR3
                                GT_MH-DL
                                GT_MH-ARTNR
                                GT_ACT-FEVOR
                                GT_MH-ALVL.
      GT_MH-PRODH = GT_ACT-PRODH.
    ELSE.
      GT_MH-ALVL = GT_MH-ARTNR = GT_ACT-ARTNR.
      GT_MH-PRODH = GT_ACT-PRODH.
    ENDIF.

*   clear artnr
    CLEAR GT_MH-ARTNR.

    GT_MH-SHOP = GT_ACT-SHOP.
    GT_MH-KOSTL = GT_ACT-KOSTL.
    GT_MH-POPER = GT_ACT-POPER.

    CLEAR L_RATE.

    IF P_MHV = 'X'.

      PERFORM GET_RATE_MHV USING    P_PA
                           CHANGING L_RATE.

      GT_MH-GR_QTY = GT_ACT-GR_QTY.
      GT_MH-CURR_MH = GT_ACT-CURR_MH * L_RATE.
      GT_MH-ADD_MH = GT_ACT-ADD_MH * L_RATE.
      GT_MH-GROSS_MH = GT_ACT-GROSS_MH * L_RATE.
      GT_MH-DEL_MH = GT_ACT-DEL_MH * L_RATE.
*     gt_mh-del_rat = gt_act-del_rat * l_rate.
      GT_MH-FINAL_MH = GT_ACT-FINAL_MH * L_RATE.
      GT_MH-NET_MH = GT_ACT-NET_MH * L_RATE.
      GT_MH-OS_DIR  = GT_ACT-OS_DIR * L_RATE.

    ELSE.
      PERFORM GET_RATE_HP USING    P_PA
                          CHANGING L_RATE.

      GT_MH-PP_GR = GT_ACT-PP_GR.
      GT_MH-DIR_MH2 = GT_ACT-DIR_MH2 * L_RATE.
      GT_MH-SEMI_MH = GT_ACT-SEMI_MH * L_RATE.
      GT_MH-PC_MH = GT_ACT-PC_MH * L_RATE.
      GT_MH-QC_MH = GT_ACT-QC_MH * L_RATE.
      GT_MH-PM_MH = GT_ACT-PM_MH * L_RATE.
      GT_MH-INDIR_MH = GT_ACT-INDIR_MH * L_RATE.
      GT_MH-ADMIN_MH = GT_ACT-ADMIN_MH * L_RATE.
      GT_MH-OS_DIR  = GT_ACT-OS_DIR * L_RATE.
      GT_MH-OS_SEMI = GT_ACT-OS_SEMI * L_RATE.
      GT_MH-OS_PC = GT_ACT-OS_PC * L_RATE.
      GT_MH-OS_QC = GT_ACT-OS_QC * L_RATE.
      GT_MH-OS_PM = GT_ACT-OS_PM * L_RATE.
      GT_MH-OS_INDIR = GT_ACT-OS_INDIR * L_RATE.
      GT_MH-OS_ADMIN = GT_ACT-OS_ADMIN * L_RATE.
    ENDIF.

    COLLECT GT_MH.
    CLEAR GT_MH.
  ENDLOOP.

ENDFORM.                                                    " GET_GT_MH
*&---------------------------------------------------------------------*
*&      Form  GET_RATE_MHV
*&---------------------------------------------------------------------*
*       Get rate: MHV
*----------------------------------------------------------------------*
FORM GET_RATE_MHV USING    P_PA
                  CHANGING P_RATE  TYPE ZAVG1.
**  UD1K940968 - by IG.MOON
**  {
**  DATA L_QTY(13) TYPE P DECIMALS 4.
**
**  CLEAR L_QTY.
**  P_RATE = 1.
**
**  READ TABLE GT_MHV WITH KEY PA    = P_PA
**                            POPER = GT_ACT-POPER
**                            ALVL  = GT_MH-ALVL
**                            SHOP  = GT_ACT-SHOP
**                            KOSTL = GT_ACT-KOSTL
**                    BINARY SEARCH.
**  IF SY-SUBRC <> 0.
**    EXIT.
**  ENDIF.
**
**  LOOP AT GT_MHV FROM SY-TABIX.
**    IF GT_MHV-PA    = P_PA AND
**       GT_MHV-POPER = GT_ACT-POPER AND
**       GT_MHV-SHOP  = GT_ACT-SHOP AND
**       GT_MHV-KOSTL = GT_ACT-KOSTL.
**
**      L_QTY = L_QTY + GT_MHV-GR_QTY.
**    ELSE.
**      EXIT.
**    ENDIF.
**  ENDLOOP.
**
**  IF SY-SUBRC = 0.
***    if gt_act-gr_qty <> 0 and l_qty <> 0.
**    IF L_QTY <> 0.
**      P_RATE = GT_ACT-GR_QTY / L_QTY.
**    ENDIF.
**  ENDIF.
**
***  if p_rate = 0.
***    p_rate = 1.
***  endif.
**

  P_RATE = 1.
  READ TABLE GT_MHV WITH KEY PA    = P_PA
                            POPER = GT_ACT-POPER
                            ALVL  = GT_MH-ALVL
                    BINARY SEARCH.

  IF SY-SUBRC = 0 AND GT_MHV-GR_QTY <> 0.
    P_RATE = GT_ACT-GR_QTY / GT_MHV-GR_QTY.
  ENDIF.

** }
** UD1K940968

ENDFORM.                    " GET_RATE_MHV
*&---------------------------------------------------------------------*
*&      Form  GET_RATE_HP
*&---------------------------------------------------------------------*
*       Get rate: HPV / HPE /HPP
*----------------------------------------------------------------------*
FORM GET_RATE_HP USING P_PA
                 CHANGING P_RATE TYPE ZAVG1.
**  UD1K940968 - by IG.MOON
**  {
**  DATA L_QTY(13) TYPE P DECIMALS 4.
**
**  CLEAR L_QTY.
**  P_RATE = 1.
**
**  READ TABLE GT_HPV WITH KEY PA    = P_PA
**                            POPER = GT_ACT-POPER
**                            ALVL  = GT_MH-ALVL
**                            SHOP  = GT_ACT-SHOP
**                            KOSTL = GT_ACT-KOSTL
**                   BINARY SEARCH.
**
**  IF SY-SUBRC <> 0.
**    EXIT.
**  ENDIF.
**
**  LOOP AT GT_HPV FROM SY-TABIX.
**    IF GT_HPV-PA = P_PA AND
**       GT_HPV-POPER = GT_ACT-POPER AND
**       GT_HPV-SHOP = GT_ACT-SHOP AND
**       GT_HPV-KOSTL = GT_ACT-KOSTL.
**
**      L_QTY = L_QTY + GT_HPV-PP_GR.
**    ELSE.
**      EXIT.
**    ENDIF.
**  ENDLOOP.
**
***  if gt_act-PP_GR <> 0 and l_qty <> 0.
**  IF L_QTY <> 0.
**    P_RATE = GT_ACT-PP_GR / L_QTY.
**  ENDIF.

  P_RATE = 1.
  READ TABLE GT_HPV WITH KEY PA    = P_PA
                            POPER = GT_ACT-POPER
                            ALVL  = GT_MH-ALVL
                    BINARY SEARCH.

  IF SY-SUBRC = 0 AND GT_HPV-PP_GR <> 0.
    P_RATE = GT_ACT-GR_QTY / GT_HPV-PP_GR.
  ENDIF.

**  } UD1K940968

ENDFORM.                    " GET_RATE_HP
*&---------------------------------------------------------------------*
*&      Form  GET_SUMMARY
*&---------------------------------------------------------------------*
*       Get Internal table for summary by analysis level
*----------------------------------------------------------------------*
*  -->  P_PA   Actual / Plan
*----------------------------------------------------------------------*
FORM GET_SUMMARY_BY_ALVL USING P_PA.
  DATA: L_PRODH     TYPE PRODH_D,
        L_MVGR4     TYPE MVGR4,
        L_MVGR5     TYPE MVGR5,
        L_MVGR3     TYPE MVGR3,
        L_DEAL(3),
        L_ALVL(18).
  DATA: LT_ACT  TYPE TABLE OF TY_ACT   WITH HEADER LINE.
  LT_ACT[] = GT_ACT[].
  SORT LT_ACT BY POPER ARTNR KOSTL.
  DELETE ADJACENT DUPLICATES FROM LT_ACT COMPARING POPER ARTNR KOSTL.

  CLEAR: GT_MHV, GT_HPV.
  REFRESH: GT_MHV, GT_HPV.

  LOOP AT LT_ACT.
    CLEAR: L_PRODH, L_MVGR4, L_MVGR5, L_MVGR3, L_DEAL, L_ALVL.

    IF GV_CHK IS INITIAL.
      L_ALVL = LT_ACT-ARTNR.
    ELSE.
      L_PRODH = LT_ACT-PRODH.
      L_MVGR4 = LT_ACT-MVGR4.
      L_MVGR5 = LT_ACT-MVGR5.
      L_MVGR3 = LT_ACT-MVGR3.
      L_DEAL = LT_ACT-ARTNR+1(3).

      IF P_VCL IS INITIAL.
        CLEAR L_PRODH.
      ENDIF.

      IF P_EGN IS INITIAL.
        CLEAR L_MVGR4.
      ENDIF.

      IF P_TRM IS INITIAL.
        CLEAR L_MVGR5.
      ENDIF.

      IF P_TM IS INITIAL.
        CLEAR L_MVGR3.
      ENDIF.

      IF P_DL IS INITIAL.
        CLEAR L_DEAL.
      ENDIF.

      CONCATENATE L_PRODH  '-' LT_ACT-FEVOR ':'
                  L_MVGR4  '-' L_MVGR5 '-' L_MVGR3 '-' L_DEAL
             INTO L_ALVL.
      CONDENSE L_ALVL.
    ENDIF.

* Begin of changes - UD1K940111
*   if not lt_act-LLV_MATNR is initial.
*    clear :lt_act-PP_GR,lt_act-gr_qty.
*   endif.
* end of changes - UD1K940111

    GT_MHV-PA     = P_PA.
    GT_MHV-POPER  = LT_ACT-POPER.
    GT_MHV-ALVL   = L_ALVL.
    GT_MHV-SHOP    = LT_ACT-SHOP.
    GT_MHV-KOSTL   = LT_ACT-KOSTL.
* UD1K940968 - by IG.MOON
* {
    GT_MHV-GR_QTY = LT_ACT-PP_GR.
    COLLECT GT_MHV.
* } UD1K940968

    CLEAR GT_MHV.

    GT_HPV-PA = P_PA.
    GT_HPV-POPER   = LT_ACT-POPER.
    GT_HPV-ALVL    = L_ALVL.
    GT_HPV-SHOP    = LT_ACT-SHOP.
    GT_HPV-KOSTL   = LT_ACT-KOSTL.
    GT_HPV-PP_GR = LT_ACT-PP_GR.

* UD1K940968 - by IG.MOON
* {
    COLLECT GT_HPV.
* } UD1K940968

    CLEAR GT_HPV.
  ENDLOOP.

* UD1K940968 - by IG.MOON
* {
*  SORT GT_MHV BY PA POPER ALVL SHOP KOSTL.
*  SORT GT_HPV BY PA POPER ALVL SHOP KOSTL.

  SORT : GT_MHV BY PA POPER ALVL ASCENDING GR_QTY DESCENDING,
         GT_HPV BY PA POPER ALVL ASCENDING PP_GR DESCENDING.

*  DELETE ADJACENT DUPLICATES FROM GT_MHV.
*  DELETE ADJACENT DUPLICATES FROM GT_HPV.

  DELETE ADJACENT DUPLICATES FROM :
          GT_MHV COMPARING ALVL,
          GT_HPV COMPARING ALVL.

* } UD1K940968

ENDFORM.                    " GET_SUMMARY_BY_ALVL
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD'.

ENDFORM.                    " PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  get_gt_act_pln
*&---------------------------------------------------------------------*
FORM GET_GT_ACT_PLN.

  CLEAR GT_ACT.
  REFRESH GT_ACT.

  IF P_HPV = 'X'.
    SELECT KOKRS     BDATJ     POPER     ARTNR     SHOP   KOSTL
           GR_QTY AS PP_GR
           CURR_MH  AS DIR_MH2
*          curr_mh  as gross_mh
*          curr_mh  as final_mh
           SEMI_MH   PC_MH     QC_MH  PM_MH  INDIR_MH  ADMIN_MH
           OS_DIR    OS_SEMI   OS_PC  OS_QC
           OS_PM     OS_INDIR  OS_ADMIN
           D~PRDHA AS PRODH
           MVGR3     MVGR4     MVGR5
* UD1K940968 - by IG.MOON
* {
           C~FEVOR
* } UD1K940968

      INTO CORRESPONDING FIELDS OF TABLE GT_ACT
      FROM ZTCO_MHV_PLN AS A
      JOIN MVKE AS B
        ON B~MATNR = A~ARTNR
      JOIN MARC AS C
        ON C~MATNR = A~ARTNR
       AND C~BESKZ = 'E'
      JOIN MARA AS D
        ON D~MATNR = C~MATNR
     WHERE KOKRS = P_KOKRS
       AND BDATJ = P_YEAR
       AND ARTNR IN S_ARTNR
       AND ( SHOP = 'MXBX' OR SHOP = 'MXPX' OR SHOP = 'MXTX' )
       AND C~SFEPR = 'VEHI'
       AND D~MTART IN S_MTART.

  ELSEIF P_HPE = 'X'.
    SELECT KOKRS     BDATJ     POPER     ARTNR     SHOP   KOSTL
           GR_QTY AS PP_GR
           CURR_MH  AS DIR_MH2
           SEMI_MH   PC_MH     QC_MH  PM_MH  INDIR_MH  ADMIN_MH
           OS_DIR    OS_SEMI   OS_PC  OS_QC
           OS_PM     OS_INDIR  OS_ADMIN
           D~PRDHA AS PRODH
* UD1K940968 - by IG.MOON
* {
           C~FEVOR
* } UD1K940968
      INTO CORRESPONDING FIELDS OF TABLE GT_ACT
      FROM ZTCO_MHV_PLN AS A
      JOIN MARC AS C
        ON C~MATNR = A~ARTNR
       AND C~BESKZ = 'E'
      JOIN MARA AS D
        ON D~MATNR = C~MATNR
     WHERE KOKRS = P_KOKRS
       AND BDATJ = P_YEAR
       AND ARTNR IN S_ARTNR
       AND SHOP = 'MXEX'
       AND C~FEVOR = 'SEA'
       AND D~MTART IN S_MTART.

  ELSEIF P_HPP = 'X'.
    SELECT KOKRS     BDATJ     POPER     ARTNR     SHOP   KOSTL
           GR_QTY AS PP_GR
           CURR_MH  AS DIR_MH2
           SEMI_MH   PC_MH     QC_MH  PM_MH  INDIR_MH  ADMIN_MH
           OS_DIR    OS_SEMI   OS_PC  OS_QC
           OS_PM     OS_INDIR  OS_ADMIN
           D~PRDHA AS PRODH
* UD1K940968 - by IG.MOON
* {
           C~FEVOR
* } UD1K940968
      INTO CORRESPONDING FIELDS OF TABLE GT_ACT
      FROM ZTCO_MHV_PLN AS A
      JOIN MARC AS C
        ON C~MATNR = A~ARTNR
       AND C~BESKZ = 'E'
      JOIN MARA AS D
        ON D~MATNR = C~MATNR
     WHERE KOKRS = P_KOKRS
       AND BDATJ = P_YEAR
       AND C~FEVOR = 'SPP'
       AND D~MTART IN S_MTART.

*MHV
  ELSE.

* UD1K940968 - by IG.MOON
* {
**    SELECT KOKRS     BDATJ     POPER     ARTNR     SHOP   KOSTL
**           GR_QTY
***          curr_mh  as curr_mh
***          curr_mh  as gross_mh
**           CURR_MH  AS FINAL_MH
**           SEMI_MH   PC_MH     QC_MH  PM_MH  INDIR_MH  ADMIN_MH
**           OS_DIR    OS_SEMI   OS_PC  OS_QC
**           OS_PM     OS_INDIR  OS_ADMIN
**           D~PRDHA AS PRODH
**      INTO CORRESPONDING FIELDS OF TABLE GT_ACT
**      FROM ZTCO_MHV_PLN AS A
**      JOIN MARA AS D
**        ON D~MATNR = A~ARTNR
**     WHERE KOKRS = P_KOKRS
**       AND BDATJ = P_YEAR
**       AND ARTNR IN S_ARTNR
**       AND D~MTART IN S_MTART.

    SELECT KOKRS     BDATJ     POPER     ARTNR     SHOP   KOSTL
           GR_QTY
           GR_QTY AS PP_GR
           CURR_MH  AS FINAL_MH
           SEMI_MH   PC_MH     QC_MH  PM_MH  INDIR_MH  ADMIN_MH
           OS_DIR    OS_SEMI   OS_PC  OS_QC
           OS_PM     OS_INDIR  OS_ADMIN
           C~PRDHA AS PRODH
           B~FEVOR
      INTO CORRESPONDING FIELDS OF TABLE GT_ACT
      FROM ZTCO_MHV_PLN AS A
* {
      JOIN MARC AS B
        ON B~MATNR = A~ARTNR
       AND B~BESKZ = 'E'
      JOIN MARA AS C
        ON C~MATNR = B~MATNR
* }
     WHERE KOKRS = P_KOKRS
       AND BDATJ = P_YEAR
       AND ARTNR IN S_ARTNR
       AND C~MTART IN S_MTART.

* } UD1K940968
    PERFORM FILL_MATERIAL_GROUPS.
  ENDIF.

ENDFORM.                    " get_gt_act_pln
*&---------------------------------------------------------------------*
*&      Form  fill_material_groups
*&---------------------------------------------------------------------*
FORM FILL_MATERIAL_GROUPS.

  DATA: L_IDX LIKE SY-TABIX.
  DATA: LT_MVKE LIKE MVKE OCCURS 0 WITH HEADER LINE.

  SELECT * INTO TABLE LT_MVKE
    FROM MVKE
    FOR ALL ENTRIES IN GT_ACT
    WHERE MATNR = GT_ACT-ARTNR.
  SORT LT_MVKE BY MATNR.

  LOOP AT GT_ACT.
    L_IDX = SY-TABIX.

    READ TABLE LT_MVKE WITH KEY MATNR = GT_ACT-ARTNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_ACT-MVGR3 = LT_MVKE-MVGR3.
      GT_ACT-MVGR4 = LT_MVKE-MVGR4.
      GT_ACT-MVGR5 = LT_MVKE-MVGR5.
      MODIFY GT_ACT INDEX L_IDX TRANSPORTING MVGR3 MVGR4 MVGR5.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " fill_material_groups
