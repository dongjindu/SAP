REPORT ZPP_21DAYTRIMINPPLAN_HASEEB .

************************************************************************
* Program Name      : ZPP_21DAYTRIMINPPLAN_MOBIS
* Author            : Haseeb Mohammad
* Creation Date     : 2007-01-31
* Specifications By : Daniel Kim
* Pattern           :
* Development Request No :UD1K930591
* Addl Documentation:
* Description       : Send 21 day input plan to MOBIS. Every day.
*
*
* Modification Logs
* Date           Developer         Transport     descriptio
* 02-19-2007     Haseeb Mohammad   UD1K930781    Date count correction.
* 03-07-2007     Haseeb Mohammad   UD1K930992    VERSION number if '000'
*                                                change it to '   '.
* 11-17-2010     Haseeb Mohammad   UD1K950224    File format for Weekly
*                                                data changed.
************************************************************************

TABLES : ZTPP_INPUT_PLAN,
         AUSP,
         CABN.
*         ZTPP_WOSUM.


DATA :   BEGIN OF IT_OUT OCCURS 0,
           PLANT(2)     TYPE C ,
           CDATE(8)     TYPE C ,
           WO_NO(15)    TYPE C,
           EG_NIP(4)    TYPE C,
           TM_MIP(4)    TYPE C,
           SEAT_TC(4)   TYPE C,
           MODEL(2)     TYPE C,
           YEAR_C(1)    TYPE C,
           NATI_C(5)    TYPE C,
           MODL_C(12)   TYPE C,
           OCNN_C(4)    TYPE C,
           VERS_N(3)    TYPE C,
           O_COLOR(3)   TYPE C,
           I_COLOR(3)   TYPE C,
           D1(4)        TYPE C ,
           D2(4)        TYPE C ,
           D3(4)        TYPE C ,
           D4(4)        TYPE C ,
           D5(4)        TYPE C ,
           D6(4)        TYPE C ,
           D7(4)        TYPE C ,
           D8(4)        TYPE C ,
           D9(4)        TYPE C ,
           D10(4)       TYPE C ,
           D11(4)       TYPE C ,
           D12(4)       TYPE C ,
           D13(4)       TYPE C ,
           D14(4)       TYPE C ,
           D15(4)       TYPE C ,
           D16(4)       TYPE C ,
           D17(4)       TYPE C ,
           D18(4)       TYPE C ,
           D19(4)       TYPE C ,
           D20(4)       TYPE C ,
           D21(4)       TYPE C ,
           REST(40)     TYPE C ,
           PLAN_QTY(84) TYPE C,
         END OF IT_OUT,

         BEGIN OF IT_AUSP OCCURS 0,
          OBJEK LIKE AUSP-OBJEK,
          ATINN LIKE AUSP-ATINN,
          ATWRT LIKE AUSP-ATWRT,
         END OF IT_AUSP,

         BEGIN OF IT_TEMP OCCURS 0,
           WO_NO(15)    TYPE C,
           O_COLOR(3)   TYPE C,
           I_COLOR(3)   TYPE C,
           RSNUM        LIKE ZTPP_INPUT_PLAN-RSNUM,
           PLANT(2)     TYPE C VALUE 'M1',
           CDATE(8)     TYPE C ,
           EG_NIP(4)    TYPE C,
           TM_MIP(4)    TYPE C,
           SEAT_TC(4)   TYPE C,
           MODEL(2)     TYPE C,
           YEAR_C(1)    TYPE C,
           NATI_C(5)    TYPE C,
           MODL_C(12)   TYPE C,
           OCNN_C(4)    TYPE C,
           VERS_N(3)    TYPE C,
           D1(4)        TYPE C ,
           D2(4)        TYPE C ,
           D3(4)        TYPE C ,
           D4(4)        TYPE C ,
           D5(4)        TYPE C ,
           D6(4)        TYPE C ,
           D7(4)        TYPE C ,
           D8(4)        TYPE C ,
           D9(4)        TYPE C ,
           D10(4)       TYPE C ,
           D11(4)       TYPE C ,
           D12(4)       TYPE C ,
           D13(4)       TYPE C ,
           D14(4)       TYPE C ,
           D15(4)       TYPE C ,
           D16(4)       TYPE C ,
           D17(4)       TYPE C ,
           D18(4)       TYPE C ,
           D19(4)       TYPE C ,
           D20(4)       TYPE C ,
           D21(4)       TYPE C ,
           REST(40)     TYPE C ,
        END OF IT_TEMP.

DATA: IT_MODEL LIKE TABLE OF ZTPP_MODEL_CONV WITH HEADER LINE.
DATA: W_START_DATE LIKE SY-DATUM.
DATA: ZATINN LIKE AUSP-ATINN.
DATA : W1(4), w2(4), w3(4), w4(4), w5(4). "UD1K950224
RANGES IT_ATINN FOR CABN-ATINN.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS IT_RSNUM FOR ZTPP_INPUT_PLAN-RSNUM ."NO INTERVALS.
PARAMETERS: P_LPLAN AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
  IT_RSNUM-LOW = SY-DATUM.
  IT_RSNUM-HIGH = SY-DATUM + 21.
  IT_RSNUM-SIGN = 'I'.
  IT_RSNUM-OPTION = 'BT'.
  APPEND IT_RSNUM.

START-OF-SELECTION.

  PERFORM GET_DATA.
  PERFORM WRITE_DATA.

END-OF-SELECTION.


START-OF-SELECTION .
*&---------------------------------------------------------------------*
*&      Form  GET_DATA  "  make data for file.
*&---------------------------------------------------------------------*
FORM GET_DATA.

  DATA: BEGIN OF IT_INPUT OCCURS 0,
         WORK_ORDER LIKE ZTPP_INPUT_PLAN-WORK_ORDER,
         EXTC       LIKE ZTPP_INPUT_PLAN-EXTC,
         INTC       LIKE ZTPP_INPUT_PLAN-INTC,
         RSNUM      LIKE ZTPP_INPUT_PLAN-RSNUM,
         MODL       LIKE ZTPP_INPUT_PLAN-MODL,
         MI         LIKE ZTPP_INPUT_PLAN-MI,
         OCNN       LIKE ZTPP_INPUT_PLAN-OCNN,
         VERS       LIKE ZTPP_INPUT_PLAN-VERS,
        END OF IT_INPUT.
  DATA: FLAG TYPE C.
  DATA: WA_ATNAM LIKE CABN-ATNAM.
  DATA:
        BEGIN OF IT_WO OCCURS 0,
          OBJEK LIKE AUSP-OBJEK,
        END OF IT_WO,

        BEGIN OF WA_AUSP ,
          OBJEK    LIKE AUSP-OBJEK,
          EG_MIP   LIKE AUSP-ATWRT,
          TM_MIP   LIKE AUSP-ATWRT,
          SEAT_TC  LIKE AUSP-ATWRT,
          YEAR_C   LIKE AUSP-ATWRT,
          NATI_C   LIKE AUSP-ATWRT,
        END OF WA_AUSP,
      ITAB LIKE SORTED TABLE OF WA_AUSP WITH UNIQUE KEY OBJEK ,
       Z_FIELD(15) TYPE C,
       COUNT(2) TYPE C,
       C1(4) TYPE C,
       WA_WO(20) TYPE C.

  FIELD-SYMBOLS : <F1> .

* Select data from ztpp_input_plan table
  SELECT WORK_ORDER EXTC INTC RSNUM MODL MI OCNN VERS
            INTO TABLE IT_INPUT FROM ZTPP_INPUT_PLAN
                            WHERE RSNUM IN IT_RSNUM .

  DELETE IT_INPUT WHERE RSNUM = '        ' OR RSNUM = '00000000'.
  SORT IT_INPUT BY WORK_ORDER EXTC INTC RSNUM.
  READ TABLE IT_INPUT INDEX 1.
  W_START_DATE = IT_INPUT-RSNUM.

*  DELETE ADJACENT DUPLICATES FROM IT_INPUT COMPARING WORK_ORDER EXTC
*INTC.

* Get the unique list of workorder, and workordercolor
  LOOP AT IT_INPUT.
    CONCATENATE IT_INPUT-WORK_ORDER IT_INPUT-EXTC IT_INPUT-INTC
                INTO IT_WO-OBJEK.
    COLLECT IT_WO.
    IT_WO-OBJEK = IT_INPUT-WORK_ORDER.
    COLLECT IT_WO.

  ENDLOOP.

  PERFORM GET_ATINN  USING 'P_ALC_U_1'.

  PERFORM GET_ATINN  USING 'P_ALC_U_2'.

  PERFORM GET_ATINN  USING 'P_ALC_C_1'.

  PERFORM GET_ATINN  USING 'P_219_1'.

  PERFORM GET_ATINN  USING 'P_DESTINATION_CODE'.
* Get the data from AUSP for the workorders collected above.
  IF NOT IT_WO[] IS INITIAL.
    SELECT OBJEK ATINN ATWRT INTO TABLE IT_AUSP FROM AUSP
             FOR ALL ENTRIES IN IT_WO WHERE OBJEK = IT_WO-OBJEK
             AND ATINN IN IT_ATINN.
  ENDIF.

  SORT IT_AUSP BY OBJEK ATINN.
* make a internal table with all characteristics of WO are assigned.
  LOOP AT IT_AUSP.

    WA_AUSP-OBJEK = IT_AUSP-OBJEK.


    SELECT SINGLE ATNAM INTO WA_ATNAM FROM CABN
           WHERE ATINN = IT_AUSP-ATINN.
    CASE WA_ATNAM.
      WHEN 'P_ALC_U_1'.
        WA_AUSP-EG_MIP = IT_AUSP-ATWRT.
      WHEN 'P_ALC_U_2'.
        WA_AUSP-TM_MIP = IT_AUSP-ATWRT.
      WHEN 'P_ALC_C_1'.
        WA_AUSP-SEAT_TC = IT_AUSP-ATWRT.
      WHEN 'P_219_1'.
        WA_AUSP-YEAR_C = IT_AUSP-ATWRT.
      WHEN 'P_DESTINATION_CODE'.
        WA_AUSP-NATI_C = IT_AUSP-ATWRT.
    ENDCASE.
    AT END OF OBJEK.
      APPEND WA_AUSP TO ITAB.
      CLEAR FLAG.
      CLEAR WA_AUSP.
    ENDAT.

  ENDLOOP.

** Changed by Furong on 02/19/10
  SELECT * INTO TABLE IT_MODEL
    FROM ZTPP_MODEL_CONV.
** end of chnage on 02/19/10

* Make a final table for the file output.
* All the fields are leftaligned, and zeros are filled for numbers
  LOOP AT IT_INPUT.
    IT_TEMP-WO_NO = IT_INPUT-WORK_ORDER.
    SHIFT IT_TEMP-WO_NO LEFT DELETING LEADING SPACE.
    PERFORM DATE_SELECTION USING IT_INPUT-RSNUM.
    IT_TEMP-PLANT = 'M1'.
    IT_TEMP-CDATE = SY-DATUM.
** Changed by Furong on 11/10/2010
    IT_TEMP-RSNUM = IT_INPUT-RSNUM.
** End of change
    READ TABLE ITAB INTO WA_AUSP
       WITH KEY OBJEK = IT_INPUT-WORK_ORDER.
    IT_TEMP-EG_NIP = WA_AUSP-EG_MIP.
    SHIFT IT_TEMP-EG_NIP LEFT DELETING LEADING SPACE.

    IT_TEMP-TM_MIP = WA_AUSP-TM_MIP.
    SHIFT IT_TEMP-TM_MIP LEFT DELETING LEADING SPACE.
    IT_TEMP-YEAR_C = WA_AUSP-YEAR_C.
    IT_TEMP-NATI_C = WA_AUSP-NATI_C.
    SHIFT IT_TEMP-TM_MIP LEFT DELETING LEADING SPACE.
** Changed by Furong on 02/19/2010
    READ TABLE IT_MODEL WITH KEY MODEL = IT_INPUT-MODL.
    IT_TEMP-MODEL = IT_MODEL-MOBIS.
*    IF IT_INPUT-MODL = 'CRA'.
*      IT_TEMP-MODEL = 'CM'.
*    ELSEIF IT_INPUT-MODL = 'EMF'.
*      IT_TEMP-MODEL = 'NF'.
*    ENDIF.
** End of change
    IT_TEMP-MODL_C = IT_INPUT-MI.
    SHIFT IT_TEMP-MODL_C LEFT DELETING LEADING SPACE.
    IT_TEMP-OCNN_C = IT_INPUT-OCNN.
    SHIFT IT_TEMP-OCNN_C LEFT DELETING LEADING SPACE.
    IF IT_INPUT-VERS = '000'.                               "UD1K930992
      IT_TEMP-VERS_N = '   '.
    ELSE.
      IT_TEMP-VERS_N = IT_INPUT-VERS.
      SHIFT IT_TEMP-VERS_N LEFT DELETING LEADING SPACE.
    ENDIF.
    COUNT = 1.

    AT END OF EXTC.
      IT_TEMP-O_COLOR = IT_INPUT-EXTC.
      SHIFT IT_TEMP-O_COLOR LEFT DELETING LEADING SPACE.
*Get the seat color based on WO, EXTC, INTC
      CONCATENATE IT_INPUT-WORK_ORDER IT_INPUT-EXTC IT_INPUT-INTC
            INTO WA_WO.
      READ TABLE ITAB INTO WA_AUSP WITH KEY
           OBJEK = WA_WO.
      IT_TEMP-SEAT_TC = WA_AUSP-SEAT_TC.
      SHIFT IT_TEMP-TM_MIP LEFT DELETING LEADING SPACE.

*
      FLAG = 'X'.
    ENDAT.
    AT END OF INTC.
      IT_TEMP-I_COLOR = IT_INPUT-INTC.
      SHIFT IT_TEMP-I_COLOR LEFT DELETING LEADING SPACE.
      IT_TEMP-O_COLOR = IT_INPUT-EXTC.
      SHIFT IT_TEMP-O_COLOR LEFT DELETING LEADING SPACE.

      CONCATENATE IT_INPUT-WORK_ORDER IT_INPUT-EXTC IT_INPUT-INTC
            INTO WA_WO.
      READ TABLE ITAB INTO WA_AUSP WITH KEY
           OBJEK = WA_WO.
      IT_TEMP-SEAT_TC = WA_AUSP-SEAT_TC.
      SHIFT IT_TEMP-TM_MIP LEFT DELETING LEADING SPACE.

      FLAG = 'X'.
    ENDAT.
    IF FLAG = 'X'.
      IT_TEMP-REST = '0'.
      PERFORM ADD_LEADZERO USING IT_TEMP-REST.
      DO 21 TIMES.
        CONCATENATE 'IT_TEMP-D' COUNT INTO Z_FIELD.
        ASSIGN (Z_FIELD) TO <F1>.
        MOVE <F1> TO C1.
        PERFORM ADD_LEADZERO CHANGING C1.
        MOVE C1 TO <F1>.
        COUNT = COUNT + 1.
      ENDDO.
      APPEND IT_TEMP.
      CLEAR FLAG.
      CLEAR IT_TEMP.
      CLEAR COUNT.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ATINN  " make characteristic table
*&---------------------------------------------------------------------*
FORM GET_ATINN USING    P_ZATINN.
  SELECT SINGLE ATINN INTO ZATINN
     FROM CABN
     WHERE ATNAM = P_ZATINN.
  IT_ATINN-SIGN = 'I'.
  IT_ATINN-OPTION = 'EQ'.
  IT_ATINN-LOW = ZATINN. APPEND IT_ATINN.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  DATE_SELECTION  " calculate number for cars for day.
*&---------------------------------------------------------------------*
FORM DATE_SELECTION USING    P_RSNUM.
  DATA: WA_D TYPE I.
  WA_D = P_RSNUM - IT_RSNUM-LOW.
  CASE WA_D.
    WHEN 0.
      IT_TEMP-D1 = IT_TEMP-D1 + 1.
    WHEN  1.
      IT_TEMP-D2 = IT_TEMP-D2 + 1.
    WHEN  2.
      IT_TEMP-D3 = IT_TEMP-D3 + 1.
    WHEN  3.
      IT_TEMP-D4 = IT_TEMP-D4 + 1.
    WHEN  4.
      IT_TEMP-D5 = IT_TEMP-D5 + 1.
    WHEN  5.
      IT_TEMP-D6 = IT_TEMP-D6 + 1.
    WHEN  6.
      IT_TEMP-D7 = IT_TEMP-D7 + 1.
    WHEN  7.
      IT_TEMP-D8 = IT_TEMP-D8 + 1.
    WHEN  8.
      IT_TEMP-D9 = IT_TEMP-D9 + 1.
    WHEN  9.
      IT_TEMP-D10 = IT_TEMP-D10 + 1.
    WHEN  10.
      IT_TEMP-D11 = IT_TEMP-D11 + 1.
    WHEN  11.
      IT_TEMP-D12 = IT_TEMP-D12 + 1.
    WHEN  12.
      IT_TEMP-D13 = IT_TEMP-D13 + 1.
    WHEN  13.
      IT_TEMP-D14 = IT_TEMP-D14 + 1.
    WHEN  14.
      IT_TEMP-D15 = IT_TEMP-D15 + 1.
    WHEN  15.
      IT_TEMP-D16 = IT_TEMP-D16 + 1.
    WHEN  16.
      IT_TEMP-D17 = IT_TEMP-D17 + 1.
    WHEN  17.
      IT_TEMP-D18 = IT_TEMP-D18 + 1.
    WHEN  18.
      IT_TEMP-D19 = IT_TEMP-D19 + 1.
    WHEN  19.
      IT_TEMP-D20 = IT_TEMP-D20 + 1.
    WHEN  20.
      IT_TEMP-D21 = IT_TEMP-D21 + 1.


  ENDCASE.

ENDFORM.                    " DATE_SELECTION
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILE   " add leading zero's where null
*&---------------------------------------------------------------------*
FORM ADD_LEADZERO CHANGING    P_FIELD.
  IF P_FIELD IS INITIAL.
    P_FIELD = '0'.
  ENDIF.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = P_FIELD
       IMPORTING
            OUTPUT = P_FIELD.

ENDFORM.                   " ADD_LEADZERO
*&---------------------------------------------------------------------*
*&      Form  DAY_Average   " Sum up Days for a week based on dayofweek
*&---------------------------------------------------------------------*
FORM DAY_Average using L_DAY.

* Changed by Haseeb 11/17/2010. below calculations are hard coded to
* ease the future modficiations to the code.  UD1K950224

  CASE L_DAY.
    WHEN 1.
      w1 = IT_OUT-D1 + IT_OUT-D2 + IT_OUT-D3 + IT_OUT-D4 + IT_OUT-D5 +
           IT_OUT-D6 + IT_OUT-D7.
      w2 = IT_OUT-D8 + IT_OUT-D9 + IT_OUT-D10 + IT_OUT-D11 + IT_OUT-D12
            + IT_OUT-D13 + IT_OUT-D14.
      w3 = IT_OUT-D15 + IT_OUT-D16 + IT_OUT-D17 + IT_OUT-D18 +
           IT_OUT-D19 + IT_OUT-D20 + IT_OUT-D21.
      w4 = '0000'.
      w5 = '0000'.

    WHEN 2.
      w1 = IT_OUT-D1 + IT_OUT-D2 + IT_OUT-D3 + IT_OUT-D4 + IT_OUT-D5 +
           IT_OUT-D6.
      w2 = IT_OUT-D7 + IT_OUT-D8 + IT_OUT-D9 + IT_OUT-D10 + IT_OUT-D11
          + IT_OUT-D12 + IT_OUT-D13.
      w3 = IT_OUT-D14 + IT_OUT-D15 + IT_OUT-D16 + IT_OUT-D17 +
            IT_OUT-D18 + IT_OUT-D19 + IT_OUT-D20.
      w4 = IT_OUT-D21.
      w5 = '0000'.

    WHEN 3.
      w1 = IT_OUT-D1 + IT_OUT-D2 + IT_OUT-D3 + IT_OUT-D4 + IT_OUT-D5.
      w2 = IT_OUT-D6 + IT_OUT-D7 + IT_OUT-D8 + IT_OUT-D9 + IT_OUT-D10 +
            IT_OUT-D11 + IT_OUT-D12.
      w3 = IT_OUT-D13 + IT_OUT-D14 + IT_OUT-D15 + IT_OUT-D16 +
           IT_OUT-D17 + IT_OUT-D18 + IT_OUT-D19.
      w4 = IT_OUT-D20 + IT_OUT-D21.
      w5 = '0000'.

    WHEN 4.
      w1 = IT_OUT-D1 + IT_OUT-D2 + IT_OUT-D3 + IT_OUT-D4.
      w2 = IT_OUT-D5 + IT_OUT-D6 + IT_OUT-D7 + IT_OUT-D8 + IT_OUT-D9 +
           IT_OUT-D10 + IT_OUT-D11.
      w3 = IT_OUT-D12 + IT_OUT-D13 + IT_OUT-D14 + IT_OUT-D15 +
           IT_OUT-D16 + IT_OUT-D17 + IT_OUT-D18.
      w4 = IT_OUT-D19 + IT_OUT-D20 + IT_OUT-D21.
      w5 = '0000'.

    WHEN 5.
      w1 = IT_OUT-D1 + IT_OUT-D2 + IT_OUT-D3.
      w2 = IT_OUT-D4 + IT_OUT-D5 + IT_OUT-D6 + IT_OUT-D7 + IT_OUT-D8 +
           IT_OUT-D9 + IT_OUT-D10.
      w3 = IT_OUT-D11 + IT_OUT-D12 + IT_OUT-D13 + IT_OUT-D14 +
           IT_OUT-D15 + IT_OUT-D16 + IT_OUT-D17.
      w4 = IT_OUT-D18 + IT_OUT-D19 + IT_OUT-D20 + IT_OUT-D21.
      w5 = '0000'.

    WHEN 6.
      w1 = IT_OUT-D1 + IT_OUT-D2.
      w2 = IT_OUT-D3 + IT_OUT-D4 + IT_OUT-D5 + IT_OUT-D6 + IT_OUT-D7 +
           IT_OUT-D8 + IT_OUT-D9.
      w3 = IT_OUT-D10 + IT_OUT-D11 + IT_OUT-D12 + IT_OUT-D13 +
           IT_OUT-D14 + IT_OUT-D15 + IT_OUT-D16.
      w4 = IT_OUT-D17 + IT_OUT-D18 + IT_OUT-D19 + IT_OUT-D20 +
           IT_OUT-D21.
      w5 = '0000'.

    WHEN 7.
      w1 = IT_OUT-D1.
      w2 = IT_OUT-D2 + IT_OUT-D3 + IT_OUT-D4 + IT_OUT-D5 + IT_OUT-D6 +
           IT_OUT-D7 + IT_OUT-D8.
     w3 = IT_OUT-D9 + IT_OUT-D10 + IT_OUT-D11 + IT_OUT-D12 + IT_OUT-D13
            + IT_OUT-D14 + IT_OUT-D15.
      w4 = IT_OUT-D16 + IT_OUT-D17 + IT_OUT-D18 + IT_OUT-D19 +
           IT_OUT-D20 + IT_OUT-D21.
      w5 = '0000'.

  ENDCASE.
  PERFORM ADD_LEADZERO USING w1.
  PERFORM ADD_LEADZERO USING w2.
  PERFORM ADD_LEADZERO USING w3.
  PERFORM ADD_LEADZERO USING w4.

ENDFORM.                   " DAY_Average
*&---------------------------------------------------------------------*
*&      Form  WRITE_DATA   " write data to file.
*&---------------------------------------------------------------------*
FORM WRITE_DATA.
  DATA : P_FILE LIKE RLGRAP-FILENAME,
           P_PATH  LIKE FILENAMECI-PATHINTERN,
           REC_CNT TYPE I.

  DATA: L_COLOR_3(3),
        L_LEN TYPE I,
        L_COUNT TYPE I,
        L_QTY(4),
        L_DAY TYPE P,
         COUNT(2) TYPE C,
       C1(4) TYPE C,
*       L_ST_DAY LIKE L_DAY,
            Z_FIELD(15) TYPE C,
          weekplan(84) type c.

  DATA: LT_07JB LIKE TABLE OF ZTPP_PMT07JB_A WITH HEADER LINE.

  FIELD-SYMBOLS: <F1>.

  P_PATH = '/usr/sap/EDI_SAP/'.
  IF P_LPLAN IS INITIAL.
    CONCATENATE P_PATH 'MB_21_DAY' SY-DATUM '.TXT' INTO P_FILE.
  ELSE.
    CONCATENATE P_PATH 'MB_21_WEEK' SY-DATUM '.TXT' INTO P_FILE.
  ENDIF.
  OPEN DATASET P_FILE FOR OUTPUT IN TEXT MODE.
  IF SY-SUBRC <> 0.
    WRITE: / 'ERROR IN CREATING A FILE', SY-SUBRC.
    STOP.
  ENDIF.

**begin loop IT_TEMP
  LOOP AT IT_TEMP.

    MOVE-CORRESPONDING IT_TEMP TO IT_OUT.
** changed by Furong on 02/17/10
    READ TABLE IT_MODEL WITH KEY MOBIS = IT_TEMP-MODEL.

    IF SY-SUBRC = 0.

      SELECT SINGLE CTRN_KEY_COLR INTO L_COLOR_3
       FROM ZTBM_ABYCOLDT
       WHERE CTRN_CARS_C = IT_MODEL-BMDL
         AND CTRN_GUBN_C = 'INT'
         AND CTRN_CONF_COLR = IT_TEMP-I_COLOR.
      L_LEN = STRLEN( L_COLOR_3 ).
      IF L_LEN = 3.
        IT_OUT-I_COLOR = L_COLOR_3.
      ENDIF.

      SELECT SINGLE CTRN_KEY_COLR INTO L_COLOR_3
       FROM ZTBM_ABYCOLDT
       WHERE CTRN_CARS_C = IT_MODEL-BMDL
         AND CTRN_GUBN_C = 'EXT'
         AND CTRN_CONF_COLR = IT_TEMP-O_COLOR.
      L_LEN = STRLEN( L_COLOR_3 ).
      IF L_LEN = 3.
        IT_OUT-O_COLOR = L_COLOR_3.
      ENDIF.
    ENDIF.
** End of change
** changed by Furong on 10/25/10
    IF P_LPLAN = 'X'.
      REFRESH: LT_07JB.
      SELECT * INTO TABLE LT_07JB
         FROM ZTPP_PMT07JB_A
         WHERE ORDR = IT_TEMP-WO_NO+0(9)
           AND DIST = IT_TEMP-WO_NO+9(5)
           AND EXTC = IT_TEMP-O_COLOR
           AND INTC = IT_TEMP-I_COLOR
           AND GUBB = 'B'.
      IT_OUT-PLAN_QTY = '0'.
      PERFORM ADD_LEADZERO CHANGING IT_OUT-PLAN_QTY.
      CLEAR: L_COUNT.
      LOOP AT LT_07JB.
        L_QTY = LT_07JB-PQTY.
        PERFORM ADD_LEADZERO CHANGING L_QTY.
        MOVE L_QTY TO IT_OUT-PLAN_QTY+L_COUNT(4).
        L_COUNT = L_COUNT + 4.
      ENDLOOP.
*Haseeb Mohammad - 11-18-2010   UD1K950224

      CALL FUNCTION 'DAY_IN_WEEK'
           EXPORTING
                DATUM = IT_TEMP-RSNUM
           IMPORTING
                WOTNR = L_DAY.

      PERFORM DAY_Average using L_DAY.
      weekplan = IT_out-PLAN_QTY.
      IT_out-PLAN_QTY = ''.

      CONCATENATE w1 w2 w3 w4 w5 weekplan into IT_OUT-PLAN_QTY.
**End of change Haseeb. UD1K950224

    ENDIF.

** End of change


    TRANSFER IT_OUT TO P_FILE.

    IF SY-SUBRC <> 0.
      WRITE: /'***ERROR writing to file', P_FILE, 'rc=', SY-SUBRC.
      WRITE: /'Record:'.
      WRITE: / P_FILE.
      STOP.
    ENDIF.
  ENDLOOP.
**  end of loop IT_TEMP
  CLOSE DATASET P_FILE.
  IF SY-SUBRC <> 0.
    WRITE: /'***ERROR closing file', P_FILE, 'rc=', SY-SUBRC.
    STOP.
  ELSE.
    DESCRIBE TABLE IT_TEMP LINES REC_CNT.
    WRITE : / 'FILE', P_FILE, 'CREATED SUCESSFULLY'.
    WRITE : / 'TOTAL NUMBER OF RECORDS :' ,REC_CNT.
  ENDIF.

ENDFORM.                    " WRITE_DATA
