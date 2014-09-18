************************************************************************
* Program Name      : ZIPP_PRDT_ACT_PLAN
* Author            : Furong Wang
* Creation Date     : 10/22/2006
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Send production actual and plan data to HMC
* Modification Logs
* Date       Developer    RequestNo    Description
*
*********************************************************************

REPORT ZIPP_PRDT_ACT_PLAN NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

DATA: L_DAY_MONTH LIKE SY-DATUM,
      L_DAY_YEAR LIKE SY-DATUM.

DATA : L_MSGTXT(100),
       L_RESULT(1).

CONSTANTS: C_DEST(10) VALUE 'WMPP01'.   "Interface Destination.

*** Changed by Furong on 07/29/09
*           C_CG_DATE TYPE D VALUE '20090531'.
*** End of change

*** Changed by Furong on 07/26/10
CONSTANTS: C_CG_DATE TYPE D VALUE '20100731'.
*** End of change

DEFINE __CLS.                          " clear & refresh
  CLEAR &1.REFRESH &1.
END-OF-DEFINITION.

DATA: IT_PROD_ACT LIKE TABLE OF ZTPP_ACT_HMC WITH HEADER LINE.
DATA: IT_PROD_ACT_MORE LIKE TABLE OF ZTPP_ACT_HMC WITH HEADER LINE.
DATA: IT_PLAN_DAY LIKE TABLE OF ZTPP_PLAN_HMC WITH HEADER LINE.
DATA: GT_STOCK LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE.

DATA: GT_STOCK_07312010 LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE.
DATA: GT_STOCK_08012010 LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE.
*      GT_STOCK_05312009 LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE
.

DATA: W_BEG_DATE LIKE SY-DATUM,
      W_END_DATE LIKE SY-DATUM.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS: S_DATUM FOR SY-DATUM.
PARAMETERS: P_DATUM LIKE SY-DATUM.
PARAMETERS: P_SND AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.

START-OF-SELECTION.

* by ig.moon 6/10/2009 {
*  PERFORM GET_ACT_DATA.     "-
  PERFORM GET_ACT_DATA_NEW. "+
* }

  PERFORM SEND_ACT_DATA.

  PERFORM GET_PLAN_DATA.
  PERFORM SEND_PLAN_DATA.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_ACT_DATA.
  DATA:  LT_FSC LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE,
         LT_MONTH LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE,
         LT_YEAR LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE.
*       LT_PMT01TB_COL LIKE TABLE OF ZTPP_PMT01TB_COL WITH HEADER LINE.
  DATA: LT_STOCK LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE.
*DATA: LT_STOCK_05312009 LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE
*.
*  DATA: LT_TEST LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE.
* DATA: LT_TEST_05312009 LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE
*.

*
*  SELECT * INTO TABLE LT_PROD_ACT_TEMP
*         FROM ZTPP_PROD_ACTUAL
*         WHERE PRDT_DATE IN S_DATUM.
*.

  CONCATENATE P_DATUM+0(4) '0101' INTO L_DAY_YEAR.
  CONCATENATE P_DATUM+0(6) '01' INTO L_DAY_MONTH.


  SELECT * INTO TABLE LT_FSC
           FROM ZTPP_PROD_ACTUAL
          WHERE PRDT_DATE BETWEEN L_DAY_YEAR AND P_DATUM.

** Changed by Furong on 05/28/09
*  SELECT * INTO TABLE LT_PMT01TB_COL
*    FROM ZTPP_PMT01TB_COL
*    WHERE ZDATE = P_DATUM
*      AND ( ZREGI = 'AA' OR ZREGI = 'AB' )
*      AND ZESTC > 0.

  SELECT HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC
         SUM( QTY_SIGNOFF ) AS QTY_SIGNOFF
         SUM( QTY_CGATE ) AS QTY_CGATE
         SUM( QTY_VPCOUT ) AS QTY_VPCOUT
         SUM( QTY_SHIPOUT ) AS QTY_SHIPOUT
         SUM( QTY_MGATE ) AS QTY_MGATE
     INTO CORRESPONDING FIELDS OF TABLE LT_STOCK
     FROM ZTPP_PROD_ACTUAL
    WHERE PRDT_DATE <= P_DATUM
    GROUP BY  HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC.

  LOOP AT LT_STOCK.
** Changed by Furong on 11/20/09
*    IF LT_STOCK-NATN+3(2) = 'AA' OR
*       LT_STOCK-NATN+3(2) = 'AB'.
    IF LT_STOCK-NATN+3(1) <> 'X'.
** End of change
    ELSE.
      DELETE LT_STOCK.
    ENDIF.
  ENDLOOP.

*** Changed by Furong on 07/29/09
*  SELECT HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC
*          SUM( QTY_SIGNOFF ) AS QTY_SIGNOFF
*          SUM( QTY_CGATE ) AS QTY_CGATE
*          SUM( QTY_VPCOUT ) AS QTY_VPCOUT
*          SUM( QTY_SHIPOUT ) AS QTY_SHIPOUT
*      INTO CORRESPONDING FIELDS OF TABLE LT_STOCK_05312009
*      FROM ZTPP_PROD_ACTUAL
*     WHERE PRDT_DATE <= C_CG_DATE
*     GROUP BY  HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC.
*
*  LOOP AT LT_STOCK_05312009.
*    IF LT_STOCK_05312009-NATN+3(2) = 'AA' OR
*       LT_STOCK_05312009-NATN+3(2) = 'AB'.
*    ELSE.
*      DELETE LT_STOCK_05312009.
*    ENDIF.
*  ENDLOOP.
*** End of change

*** For testing on 06/05/09

*  SELECT NATN BMDL OCN
*           SUM( QTY_SIGNOFF ) AS QTY_SIGNOFF
*           SUM( QTY_CGATE ) AS QTY_CGATE
*           SUM( QTY_VPCOUT ) AS QTY_VPCOUT
*           SUM( QTY_SHIPOUT ) AS QTY_SHIPOUT
*       INTO CORRESPONDING FIELDS OF TABLE LT_TEST
*       FROM ZTPP_PROD_ACTUAL
*      WHERE PRDT_DATE <= P_DATUM
*      GROUP BY NATN BMDL OCN.
*
*  LOOP AT LT_TEST.
*    IF LT_TEST-NATN+3(2) = 'AA' OR
*       LT_TEST-NATN+3(2) = 'AB'.
*    ELSE.
*      DELETE LT_TEST.
*    ENDIF.
*  ENDLOOP.

*** Changed by Furong on 07/29/09
*  SELECT NATN BMDL OCN
*          SUM( QTY_SIGNOFF ) AS QTY_SIGNOFF
*          SUM( QTY_CGATE ) AS QTY_CGATE
*          SUM( QTY_VPCOUT ) AS QTY_VPCOUT
*          SUM( QTY_SHIPOUT ) AS QTY_SHIPOUT
*      INTO CORRESPONDING FIELDS OF TABLE LT_TEST_05312009
*      FROM ZTPP_PROD_ACTUAL
*     WHERE PRDT_DATE <= C_CG_DATE
*     GROUP BY  NATN BMDL OCN.
*
*  LOOP AT LT_TEST_05312009.
*    IF LT_TEST_05312009-NATN+3(2) = 'AA' OR
*       LT_TEST_05312009-NATN+3(2) = 'AB'.
*    ELSE.
*      DELETE LT_TEST_05312009.
*    ENDIF.
*  ENDLOOP.
*** End of change
** End of test

** End of change

  LOOP AT LT_FSC.
    IF LT_FSC-NATN+3(2) = 'XX' OR
       LT_FSC-NATN+3(2) = 'XY'.
      CONTINUE.
    ENDIF.
    LT_YEAR-PLANT = LT_FSC-PLANT.
    LT_YEAR-HKCODE = LT_FSC-HKCODE.
    LT_YEAR-DEST = LT_FSC-DEST.
    LT_YEAR-MODEL = LT_FSC-MODEL.
    LT_YEAR-NATN = LT_FSC-NATN.
    LT_YEAR-BMDL = LT_FSC-BMDL.
    LT_YEAR-OCN = LT_FSC-OCN.
    LT_YEAR-EXTC = LT_FSC-EXTC.
    LT_YEAR-INTC = LT_FSC-INTC.
    LT_YEAR-QTY_YY = LT_FSC-QTY_SIGNOFF.
    IF LT_FSC-PRDT_DATE >= L_DAY_MONTH  AND LT_FSC-PRDT_DATE <= P_DATUM
.
      LT_YEAR-QTY_MM = LT_FSC-QTY_SIGNOFF.
    ENDIF.
    COLLECT LT_YEAR.
    CLEAR: LT_YEAR.
  ENDLOOP.


** Changed by Furong on 06/01/09

*  SORT LT_FSC BY PRDT_DATE NATN BMDL OCN EXTC INTC.
*  SORT LT_STOCK BY  NATN BMDL OCN EXTC INTC.


*  LOOP AT LT_YEAR.
*    MOVE-CORRESPONDING LT_YEAR TO IT_PROD_ACT.
*    READ TABLE LT_FSC WITH KEY PRDT_DATE = P_DATUM
*                            NATN = LT_YEAR-NATN
*                            BMDL = LT_YEAR-BMDL
*                            OCN = LT_YEAR-OCN
*                            EXTC = LT_YEAR-EXTC
*                            INTC = LT_YEAR-INTC
*                            BINARY SEARCH.
*
*    IF SY-SUBRC = 0.
*      IT_PROD_ACT-QTY_SIGNOFF = LT_FSC-QTY_SIGNOFF.
*    ENDIF.
*** Changed by Furong on 05/29/09
*    IF LT_YEAR-NATN+3(2) = 'AA' OR LT_YEAR-NATN+3(2) =  'AB'.
**      READ TABLE LT_PMT01TB_COL WITH KEY
**                                ZDATE = P_DATUM
**                                ZMODL = LT_YEAR-MODEL
**                                ZNATN = LT_YEAR-NATN
**                                ZBMDL = LT_YEAR-BMDL
**                                ZOCN = LT_YEAR-OCN
**                                ZEXTC = LT_YEAR-EXTC
**                                ZINTC = LT_YEAR-INTC.
**      IF SY-SUBRC = 0.
**        IT_PROD_ACT-STOCK = LT_PMT01TB_COL-ZESTC.
**      ENDIF.
**    ENDIF.
*
*      READ TABLE LT_STOCK WITH KEY NATN = LT_YEAR-NATN
*                             BMDL = LT_YEAR-BMDL
*                             OCN = LT_YEAR-OCN
*                             EXTC = LT_YEAR-EXTC
*                             INTC = LT_YEAR-INTC
*                             BINARY SEARCH..
*      IF SY-SUBRC = 0.
*        IF LT_YEAR-NATN+0(3) = 'B28'.
*          IF P_DATUM <= C_CG_DATE.
*            IT_PROD_ACT-STOCK = LT_STOCK-QTY_SIGNOFF
*                - LT_STOCK-QTY_CGATE - LT_STOCK-QTY_VPCOUT.
*          ELSE.
*            IT_PROD_ACT-STOCK = LT_STOCK-QTY_SIGNOFF
*                - LT_STOCK-QTY_VPCOUT.
*          ENDIF.
*        ELSE.
*          IT_PROD_ACT-STOCK =  LT_STOCK-QTY_SIGNOFF
*            - LT_STOCK-QTY_SHIPOUT.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*** End of change on 05/29/09
*    IT_PROD_ACT-PRDT_DATE = P_DATUM.
*    IT_PROD_ACT-CRDATE = SY-DATUM.
*    IT_PROD_ACT-TRDATE = SY-DATUM.
*
*    IF IT_PROD_ACT-QTY_SIGNOFF = 0 AND
*       IT_PROD_ACT-QTY_MM = 0 AND
*       IT_PROD_ACT-QTY_YY = 0  AND
*       IT_PROD_ACT-STOCK = 0.
*    ELSE.
*      APPEND IT_PROD_ACT.
*    ENDIF.
*    CLEAR: IT_PROD_ACT.
*  ENDLOOP.
*
******************
  LOOP AT LT_STOCK.

    MOVE-CORRESPONDING LT_STOCK TO IT_PROD_ACT.
    READ TABLE LT_FSC WITH KEY PRDT_DATE = P_DATUM
                            NATN = LT_STOCK-NATN
                            BMDL = LT_STOCK-BMDL
                            OCN = LT_STOCK-OCN
                            EXTC = LT_STOCK-EXTC
                            INTC = LT_STOCK-INTC.
*                            BINARY SEARCH.

    IF SY-SUBRC = 0.
      IT_PROD_ACT-QTY_SIGNOFF = LT_FSC-QTY_SIGNOFF.
    ELSE.
      CLEAR: IT_PROD_ACT-QTY_SIGNOFF.
    ENDIF.

    READ TABLE LT_YEAR WITH KEY NATN = LT_STOCK-NATN
                            BMDL = LT_STOCK-BMDL
                            OCN = LT_STOCK-OCN
                            EXTC = LT_STOCK-EXTC
                            INTC = LT_STOCK-INTC.
    IF SY-SUBRC = 0.
      IT_PROD_ACT-QTY_MM = LT_YEAR-QTY_MM.
      IT_PROD_ACT-QTY_YY = LT_YEAR-QTY_YY.
    ENDIF.
** Changed by Furong on 11/20/09
*    IF LT_STOCK-NATN+3(2) = 'AA' OR LT_STOCK-NATN+3(2) =  'AB'.
    IF LT_STOCK-NATN+3(1) <> 'X'.
** End of change
      IF LT_STOCK-NATN+0(3) = 'B28'.

*** Changed by Furong on 07/29/09
*        IF p_datum <= c_cg_date.
*          it_prod_act-stock = lt_stock-qty_signoff
*                              - lt_stock-qty_cgate
*                              - lt_stock-qty_vpcout.
*        ELSE.
*          READ TABLE lt_stock_05312009 WITH KEY
*                                       hkcode = lt_stock-hkcode
*                                       plant = lt_stock-plant
*                                       model = lt_stock-model
*                                       dest = lt_stock-dest
*                                       natn = lt_stock-natn
*                                       bmdl = lt_stock-bmdl
*                                       ocn = lt_stock-ocn
*                                       extc = lt_stock-extc
*                                       intc = lt_stock-intc.
*          IF sy-subrc = 0.
*            it_prod_act-stock = lt_stock-qty_signoff
*                               - lt_stock_05312009-qty_cgate
*                               - lt_stock-qty_vpcout.
*          ELSE.
*            it_prod_act-stock = lt_stock-qty_signoff
*                               - lt_stock-qty_vpcout.
*          ENDIF.
*        ENDIF.

        IT_PROD_ACT-STOCK = LT_STOCK-QTY_SIGNOFF
                            - LT_STOCK-QTY_CGATE.
*** End of change on 07/29/09

      ELSE.
        IT_PROD_ACT-STOCK =  LT_STOCK-QTY_SIGNOFF
                             - LT_STOCK-QTY_SHIPOUT.
      ENDIF.
    ENDIF.

** End of change on 05/29/09
    IT_PROD_ACT-PRDT_DATE = P_DATUM.
    IT_PROD_ACT-CRDATE = SY-DATUM.
    IT_PROD_ACT-TRDATE = SY-DATUM.

    IF IT_PROD_ACT-QTY_SIGNOFF = 0 AND
       IT_PROD_ACT-QTY_MM = 0 AND
       IT_PROD_ACT-QTY_YY = 0  AND
       IT_PROD_ACT-STOCK = 0.
    ELSE.
      APPEND IT_PROD_ACT.
    ENDIF.
    CLEAR: IT_PROD_ACT.
*    clear: LT_STOCK_05312009.
  ENDLOOP.

******************
** End of change


** stock
** Changed by Furong on 05/28/09
*  LOOP AT LT_PMT01TB_COL.
*    READ TABLE LT_YEAR WITH KEY MODEL = LT_PMT01TB_COL-ZMODL
*                                NATN = LT_PMT01TB_COL-ZNATN
*                                BMDL = LT_PMT01TB_COL-ZBMDL
*                                OCN = LT_PMT01TB_COL-ZOCN
*                                EXTC = LT_PMT01TB_COL-ZEXTC
*                                INTC = LT_PMT01TB_COL-ZINTC.
*    IF SY-SUBRC <> 0.
*      CLEAR: IT_PROD_ACT.
*      IT_PROD_ACT-STOCK = LT_PMT01TB_COL-ZESTC.
*      IT_PROD_ACT-PRDT_DATE = P_DATUM.
*      IT_PROD_ACT-HKCODE = 'H'.
*      IT_PROD_ACT-PLANT = 'A1'.
*      IT_PROD_ACT-MODEL = LT_PMT01TB_COL-ZMODL.
*      IT_PROD_ACT-NATN = LT_PMT01TB_COL-ZNATN.
*      IT_PROD_ACT-DEST = LT_PMT01TB_COL-ZNATN.
*      IT_PROD_ACT-BMDL = LT_PMT01TB_COL-ZBMDL.
*      IT_PROD_ACT-OCN = LT_PMT01TB_COL-ZOCN.
*      IT_PROD_ACT-EXTC = LT_PMT01TB_COL-ZEXTC.
*      IT_PROD_ACT-INTC = LT_PMT01TB_COL-ZINTC.
*      IT_PROD_ACT-CRDATE = SY-DATUM.
*      IT_PROD_ACT-TRDATE = SY-DATUM.
*      APPEND IT_PROD_ACT.
*    ENDIF.
*  ENDLOOP.
** End of change
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SEND_ACT_DATA                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SEND_ACT_DATA.
  IF P_SND = 'X'.
    CALL FUNCTION 'Z_FPP_PROD_ACTUAL'
      DESTINATION C_DEST
      IMPORTING
        FLAG          = L_RESULT
      TABLES
        I_PROD_ACT    = IT_PROD_ACT
      EXCEPTIONS
             COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
             SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

    IF SY-SUBRC = 0.
      IF SY-BATCH = 'X'.
       MESSAGE I001 WITH 'Production actual data successfully sent out'.
      ELSE.
      MESSAGE I001 WITH 'Production actual data successfully sent out,'
                                        'Press ENTER to send plan data'.
      ENDIF.
    ELSE.
      MESSAGE I001 WITH L_MSGTXT.
    ENDIF.
  ENDIF.

  DELETE FROM ZTPP_ACT_HMC WHERE PRDT_DATE = P_DATUM.
  INSERT ZTPP_ACT_HMC FROM TABLE IT_PROD_ACT.
*    MODIFY ZTPP_ACT_HMC FROM TABLE IT_PROD_ACT.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE I000 WITH 'Table saving error'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_PLAN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PLAN_DATA.
  DATA: LT_PLAN_DAY_TEMP LIKE TABLE OF ZTPP_PLAN_DAY WITH HEADER LINE.
** Changed by furong on 04/16/08
*  CONCATENATE P_DATUM+0(6) '01' INTO W_BEG_DATE.
  CONCATENATE SY-DATUM+0(6) '01' INTO W_BEG_DATE.
** End of change
  CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN                  = W_BEG_DATE
   IMPORTING
     LAST_DAY_OF_MONTH       =  W_END_DATE
*   EXCEPTIONS
*     DAY_IN_NO_DATE          = 1
*     OTHERS                  = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  SELECT * INTO TABLE LT_PLAN_DAY_TEMP
         FROM ZTPP_PLAN_DAY
         WHERE PRDT_DATE BETWEEN W_BEG_DATE AND W_END_DATE.
  LOOP AT LT_PLAN_DAY_TEMP.
    MOVE-CORRESPONDING LT_PLAN_DAY_TEMP TO IT_PLAN_DAY.
    IT_PLAN_DAY-CRDATE = SY-DATUM.
    IT_PLAN_DAY-TRDATE = SY-DATUM.
    APPEND IT_PLAN_DAY.
  ENDLOOP.
ENDFORM.                    " GET_PLAN_DATA
*&---------------------------------------------------------------------*
*&      Form  SEND_PLAN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_PLAN_DATA.
  IF P_SND = 'X'.
    CALL FUNCTION 'Z_FPP_PLAN_DAY'
       DESTINATION C_DEST
       IMPORTING
         FLAG          = L_RESULT
       TABLES
         I_PLAN_DAY       = IT_PLAN_DAY
       EXCEPTIONS
              COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
              SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

*  IF L_RESULT = 'S'.
    IF SY-SUBRC = 0.
      MESSAGE I001 WITH 'Plan data successfully sent out'.
    ELSE.
      MESSAGE I001 WITH L_MSGTXT.
    ENDIF.
  ENDIF.
*    DELETE FROM ZTPP_PLAN_HMC WHERE PRDT_DATE = P_DATUM.
  DELETE FROM ZTPP_PLAN_HMC WHERE PRDT_DATE
                            BETWEEN W_BEG_DATE AND W_END_DATE.

  INSERT ZTPP_PLAN_HMC FROM TABLE IT_PLAN_DAY.
*    MODIFY ZTPP_PLAN_HMC FROM TABLE IT_PLAN_DAY.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE I000 WITH 'Table saving error'.
  ENDIF.


ENDFORM.                    " SEND_PLAN_DATA
*&---------------------------------------------------------------------*
*&      Form  get_act_data_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ACT_DATA_NEW.

  DATA:  LT_FSC LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE,
         LT_MONTH LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE,
         LT_YEAR LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE.

  CONCATENATE P_DATUM+0(4) '0101' INTO L_DAY_YEAR.
  CONCATENATE P_DATUM+0(6) '01' INTO L_DAY_MONTH.

  DATA $IX LIKE SY-TABIX.


  SELECT * INTO TABLE LT_FSC
           FROM ZTPP_PROD_ACTUAL
          WHERE PRDT_DATE BETWEEN L_DAY_YEAR AND P_DATUM.

*** Changed by Furong on 07/29/09
*  __CLS : GT_STOCK, GT_STOCK_05312009.
*

* by Daniel on 11/04/10 {
*  SELECT HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC
*         SUM( QTY_SIGNOFF ) AS QTY_SIGNOFF
*         SUM( QTY_CGATE ) AS QTY_CGATE
*         SUM( QTY_VPCOUT ) AS QTY_VPCOUT
*         SUM( QTY_SHIPOUT ) AS QTY_SHIPOUT
*         SUM( QTY_MGATE ) AS QTY_MGATE
**** Changed by Furong on 01/13/10
*         SUM( S_DISPOSAL ) AS S_DISPOSAL
**        SUM( v_DISPOSAL ) AS v_DISPOSAL
*** end of change
**** Changed by Furong on 08/06/10
*         SUM( V_DISPOSAL ) AS V_DISPOSAL
*** End of change
** by Daniel on 11/04/10 {
*         SUM( H_DISPOSAL ) AS H_DISPOSAL
** }
*     INTO CORRESPONDING FIELDS OF TABLE GT_STOCK
*     FROM ZTPP_PROD_ACTUAL
*    WHERE PRDT_DATE <= P_DATUM
*    GROUP BY  HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC.

SELECT HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC
         SUM( QTY_SIGNOFF ) AS QTY_SIGNOFF
         SUM( QTY_CGATE )   AS QTY_CGATE
         SUM( QTY_VPCOUT )  AS QTY_VPCOUT
         SUM( QTY_SHIPIN )  AS QTY_SHIPIN
         SUM( QTY_SHIPOUT ) AS QTY_SHIPOUT
         SUM( QTY_MGATE )   AS QTY_MGATE
         SUM( S_DISPOSAL )  AS S_DISPOSAL
         SUM( V_DISPOSAL )  AS V_DISPOSAL
         SUM( H_DISPOSAL )  AS H_DISPOSAL
     INTO CORRESPONDING FIELDS OF TABLE GT_STOCK
     FROM ZTPP_PROD_ACTUAL
    WHERE PRDT_DATE <= P_DATUM
    GROUP BY  HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC.
* }


*** Changed by Furong on 07/31/09

*  SELECT HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC
*          SUM( QTY_SIGNOFF ) AS QTY_SIGNOFF
*          SUM( QTY_CGATE ) AS QTY_CGATE
*          SUM( QTY_VPCOUT ) AS QTY_VPCOUT
*          SUM( QTY_SHIPOUT ) AS QTY_SHIPOUT
*      INTO CORRESPONDING FIELDS OF TABLE GT_STOCK_05312009
*      FROM ZTPP_PROD_ACTUAL
*     WHERE PRDT_DATE <= C_CG_DATE
*     GROUP BY  HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC.
*** End of change

** Changed by Furong on 07/26/10

* by Daniel on 11/04/10 {
*  SELECT HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC
*         SUM( QTY_SIGNOFF ) AS QTY_SIGNOFF
*         SUM( QTY_CGATE ) AS QTY_CGATE
*         SUM( QTY_VPCOUT ) AS QTY_VPCOUT
*         SUM( QTY_SHIPOUT ) AS QTY_SHIPOUT
*         SUM( QTY_MGATE ) AS QTY_MGATE
*         SUM( S_DISPOSAL ) AS S_DISPOSAL
*         SUM( V_DISPOSAL ) AS V_DISPOSAL
** by Daniel on 11/04/10 {
*         SUM( H_DISPOSAL ) AS H_DISPOSAL
** }
*      INTO CORRESPONDING FIELDS OF TABLE GT_STOCK_07312010
*      FROM ZTPP_PROD_ACTUAL
*     WHERE PRDT_DATE <= C_CG_DATE
*     GROUP BY  HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC.

SELECT HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC
         SUM( QTY_SIGNOFF ) AS QTY_SIGNOFF
         SUM( QTY_CGATE )   AS QTY_CGATE
         SUM( QTY_VPCOUT )  AS QTY_VPCOUT
         SUM( QTY_SHIPIN )  AS QTY_SHIPIN
         SUM( QTY_SHIPOUT ) AS QTY_SHIPOUT
         SUM( QTY_MGATE )   AS QTY_MGATE
         SUM( S_DISPOSAL )  AS S_DISPOSAL
         SUM( V_DISPOSAL )  AS V_DISPOSAL
         SUM( H_DISPOSAL )  AS H_DISPOSAL
      INTO CORRESPONDING FIELDS OF TABLE GT_STOCK_07312010
      FROM ZTPP_PROD_ACTUAL
     WHERE PRDT_DATE <= C_CG_DATE
     GROUP BY  HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC.


*SELECT HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC
*         SUM( QTY_SIGNOFF ) AS QTY_SIGNOFF
*         SUM( QTY_CGATE ) AS QTY_CGATE
*         SUM( QTY_VPCOUT ) AS QTY_VPCOUT
*         SUM( QTY_SHIPOUT ) AS QTY_SHIPOUT
*         SUM( QTY_MGATE ) AS QTY_MGATE
*         SUM( S_DISPOSAL ) AS S_DISPOSAL
*         SUM( V_DISPOSAL ) AS V_DISPOSAL
** by Daniel on 11/04/10 {
*         SUM( H_DISPOSAL ) AS H_DISPOSAL
** }
* INTO CORRESPONDING FIELDS OF TABLE GT_STOCK_08012010
* FROM ZTPP_PROD_ACTUAL
*WHERE ( PRDT_DATE > C_CG_DATE AND PRDT_DATE <= P_DATUM )
*GROUP BY  HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC.

SELECT HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC
         SUM( QTY_SIGNOFF ) AS QTY_SIGNOFF
         SUM( QTY_CGATE ) AS QTY_CGATE
         SUM( QTY_VPCOUT ) AS QTY_VPCOUT
         SUM( QTY_SHIPIN ) AS QTY_SHIPIN
         SUM( QTY_SHIPOUT ) AS QTY_SHIPOUT
         SUM( QTY_MGATE ) AS QTY_MGATE
         SUM( S_DISPOSAL ) AS S_DISPOSAL
         SUM( V_DISPOSAL ) AS V_DISPOSAL
         SUM( H_DISPOSAL ) AS H_DISPOSAL
 INTO CORRESPONDING FIELDS OF TABLE GT_STOCK_08012010
 FROM ZTPP_PROD_ACTUAL
WHERE ( PRDT_DATE > C_CG_DATE AND PRDT_DATE <= P_DATUM )
GROUP BY  HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC.
* }

** End of change

  LOOP AT LT_FSC.
    IF LT_FSC-NATN+3(2) = 'XX' OR
       LT_FSC-NATN+3(2) = 'XY'.
      CONTINUE.
    ENDIF.
    LT_YEAR-PLANT = LT_FSC-PLANT.
    LT_YEAR-HKCODE = LT_FSC-HKCODE.
    LT_YEAR-DEST = LT_FSC-DEST.
    LT_YEAR-MODEL = LT_FSC-MODEL.
    LT_YEAR-NATN = LT_FSC-NATN.
    LT_YEAR-BMDL = LT_FSC-BMDL.
    LT_YEAR-OCN = LT_FSC-OCN.
    LT_YEAR-EXTC = LT_FSC-EXTC.
    LT_YEAR-INTC = LT_FSC-INTC.
    LT_YEAR-QTY_YY = LT_FSC-QTY_SIGNOFF.
    IF LT_FSC-PRDT_DATE >= L_DAY_MONTH
      AND LT_FSC-PRDT_DATE <= P_DATUM.
      LT_YEAR-QTY_MM = LT_FSC-QTY_SIGNOFF.
    ENDIF.
    COLLECT LT_YEAR.
    CLEAR: LT_YEAR.
  ENDLOOP.

  SORT LT_FSC BY PRDT_DATE NATN BMDL OCN EXTC INTC.

  __CLS : IT_PROD_ACT, IT_PROD_ACT_MORE.

  LOOP AT LT_YEAR.

    MOVE-CORRESPONDING LT_YEAR TO IT_PROD_ACT.
    IT_PROD_ACT-PRDT_DATE = P_DATUM.
    IT_PROD_ACT-CRDATE = SY-DATUM.
    IT_PROD_ACT-TRDATE = SY-DATUM.
    IT_PROD_ACT-QTY_MM = LT_YEAR-QTY_MM.
    IT_PROD_ACT-QTY_YY = LT_YEAR-QTY_YY.
    APPEND IT_PROD_ACT.
  ENDLOOP.

  SORT IT_PROD_ACT BY PRDT_DATE NATN BMDL OCN EXTC INTC.

  LOOP AT LT_FSC.

    CHECK LT_FSC-PRDT_DATE EQ P_DATUM.
    READ TABLE IT_PROD_ACT WITH KEY PRDT_DATE = P_DATUM
                            NATN = LT_FSC-NATN
                            BMDL = LT_FSC-BMDL
                            OCN = LT_FSC-OCN
                            EXTC = LT_FSC-EXTC
                            INTC = LT_FSC-INTC
                            BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      $IX = SY-TABIX.
      IT_PROD_ACT-QTY_SIGNOFF = LT_FSC-QTY_SIGNOFF.
      MODIFY IT_PROD_ACT INDEX $IX TRANSPORTING QTY_SIGNOFF.
    ELSE.
      MOVE-CORRESPONDING LT_FSC TO IT_PROD_ACT_MORE.
      IT_PROD_ACT_MORE-PRDT_DATE = P_DATUM.
      IT_PROD_ACT_MORE-CRDATE = SY-DATUM.
      IT_PROD_ACT_MORE-TRDATE = SY-DATUM.
      APPEND IT_PROD_ACT_MORE.
    ENDIF.
  ENDLOOP.

  APPEND LINES OF IT_PROD_ACT_MORE TO IT_PROD_ACT.
  SORT IT_PROD_ACT BY PRDT_DATE NATN BMDL OCN EXTC INTC.
*** Changed by Furong on 07/29/09
*  SORT GT_STOCK_05312009 BY HKCODE PLANT MODEL DEST NATN BMDL
*                                                OCN EXTC INTC.
*** End of change

** Changed by Furong on 07/26/10
  SORT GT_STOCK_07312010 BY HKCODE PLANT MODEL DEST NATN BMDL
                                                 OCN EXTC INTC.
  SORT GT_STOCK_08012010 BY HKCODE PLANT MODEL DEST NATN BMDL
                                                 OCN EXTC INTC.

** End of change

  __CLS IT_PROD_ACT_MORE.

  LOOP AT GT_STOCK.

    READ TABLE IT_PROD_ACT WITH KEY PRDT_DATE = P_DATUM
                            NATN = GT_STOCK-NATN
                            BMDL = GT_STOCK-BMDL
                            OCN = GT_STOCK-OCN
                            EXTC = GT_STOCK-EXTC
                            INTC = GT_STOCK-INTC
                            BINARY SEARCH.

    IF SY-SUBRC EQ 0.
      $IX = SY-TABIX.
      PERFORM FILL_ACT_TABLE TABLES IT_PROD_ACT.
      MODIFY IT_PROD_ACT INDEX $IX.
    ELSE.
      MOVE-CORRESPONDING GT_STOCK TO IT_PROD_ACT_MORE.
      PERFORM FILL_ACT_TABLE TABLES IT_PROD_ACT_MORE.
      IF NOT IT_PROD_ACT_MORE-STOCK IS INITIAL.
        IT_PROD_ACT_MORE-PRDT_DATE = P_DATUM.
        IT_PROD_ACT_MORE-CRDATE = SY-DATUM.
        IT_PROD_ACT_MORE-TRDATE = SY-DATUM.
** Changed by Furong on 01/04/10
        CLEAR: IT_PROD_ACT_MORE-QTY_SIGNOFF.
** End of cahnge on 01/04/10
        APPEND IT_PROD_ACT_MORE.
      ENDIF.
    ENDIF.

  ENDLOOP.

  APPEND LINES OF IT_PROD_ACT_MORE TO IT_PROD_ACT.
  SORT IT_PROD_ACT BY PRDT_DATE NATN BMDL OCN EXTC INTC.

  LOOP AT IT_PROD_ACT.
    $IX = SY-TABIX.
    IF IT_PROD_ACT-NATN+3(2) EQ 'AA' OR
       IT_PROD_ACT-NATN+3(2) EQ 'AB' OR
       IT_PROD_ACT-NATN+3(2) EQ 'AC' OR
       IT_PROD_ACT-NATN+3(2) EQ 'XA'.
      IF IT_PROD_ACT-NATN+3(2) EQ 'XA'.
        CLEAR IT_PROD_ACT-STOCK.
        MODIFY IT_PROD_ACT INDEX $IX TRANSPORTING STOCK.
      ENDIF.
    ELSE.
      DELETE IT_PROD_ACT INDEX $IX.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_PROD_ACT.
    $IX = SY-TABIX.
    IF IT_PROD_ACT-QTY_SIGNOFF IS INITIAL AND
       IT_PROD_ACT-QTY_MM IS INITIAL AND
       IT_PROD_ACT-QTY_YY IS INITIAL AND
       IT_PROD_ACT-STOCK IS INITIAL.
      DELETE IT_PROD_ACT INDEX $IX.
    ENDIF.
  ENDLOOP.

  SORT IT_PROD_ACT BY PRDT_DATE NATN BMDL OCN EXTC INTC.

ENDFORM.                    " get_act_data_new
*&---------------------------------------------------------------------*
*&      Form  fill_act_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PROD_ACT  text
*      -->P_LT_STOCK  text
*----------------------------------------------------------------------*
FORM FILL_ACT_TABLE TABLES   P_ACT STRUCTURE ZTPP_ACT_HMC.
  DATA $IX_GT_STOCK_05312009 TYPE I.

** Changed by Furong on 11/20/09
*  IF GT_STOCK-NATN+3(2) = 'AA' OR GT_STOCK-NATN+3(2) =  'AB'.
  IF GT_STOCK-NATN+3(1) <> 'X'.
** End of change
    IF GT_STOCK-NATN+0(3) = 'B28'.

*** Changed by Furong on 07/29/09
*      IF P_DATUM <= C_CG_DATE.
*        P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
*                            - GT_STOCK-QTY_CGATE
*                            - GT_STOCK-QTY_VPCOUT.
*      ELSE.
*        READ TABLE GT_STOCK_05312009 WITH KEY
*                                     HKCODE = GT_STOCK-HKCODE
*                                     PLANT = GT_STOCK-PLANT
*                                     MODEL = GT_STOCK-MODEL
*                                     DEST = GT_STOCK-DEST
*                                     NATN = GT_STOCK-NATN
*                                     BMDL = GT_STOCK-BMDL
*                                     OCN = GT_STOCK-OCN
*                                     EXTC = GT_STOCK-EXTC
*                                     INTC = GT_STOCK-INTC
*                                     BINARY SEARCH.
*        IF SY-SUBRC = 0.
*          $IX_GT_STOCK_05312009 = SY-TABIX.
*          P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
*                             - GT_STOCK_05312009-QTY_CGATE
*                             - GT_STOCK-QTY_VPCOUT.
*
*          DELETE  GT_STOCK_05312009 INDEX $IX_GT_STOCK_05312009.
*
*        ELSE.
*          P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
*                             - GT_STOCK-QTY_VPCOUT.
*        ENDIF.
*      ENDIF.
*** End of change
**
*      P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
*                           - GT_STOCK-QTY_CGATE.
*
*    ELSE.
*      P_ACT-STOCK =  GT_STOCK-QTY_SIGNOFF
*                           - GT_STOCK-QTY_SHIPOUT.
*    ENDIF.

** Changed by Furong on 01/13/10
*    P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
*                            - GT_STOCK-QTY_CGATE.
*    ELSE.
*      P_ACT-STOCK =  GT_STOCK-QTY_SIGNOFF
*                           - GT_STOCK-QTY_SHIPOUT.
*    ENDIF.

** Changed by Furong on 07/26/10
*     P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
*                            - GT_STOCK-QTY_CGATE
*                            - GT_STOCK-S_DISPOSAL.
*
      CLEAR: GT_STOCK_08012010, GT_STOCK_07312010.
      READ TABLE GT_STOCK_07312010 WITH KEY
                                        HKCODE = GT_STOCK-HKCODE
                                        PLANT = GT_STOCK-PLANT
                                        MODEL = GT_STOCK-MODEL
                                        DEST = GT_STOCK-DEST
                                        NATN = GT_STOCK-NATN
                                        BMDL = GT_STOCK-BMDL
                                        OCN = GT_STOCK-OCN
                                        EXTC = GT_STOCK-EXTC
                                        INTC = GT_STOCK-INTC
                                        BINARY SEARCH.

      READ TABLE GT_STOCK_08012010 WITH KEY
                                        HKCODE = GT_STOCK-HKCODE
                                        PLANT = GT_STOCK-PLANT
                                        MODEL = GT_STOCK-MODEL
                                        DEST = GT_STOCK-DEST
                                        NATN = GT_STOCK-NATN
                                        BMDL = GT_STOCK-BMDL
                                        OCN = GT_STOCK-OCN
                                        EXTC = GT_STOCK-EXTC
                                        INTC = GT_STOCK-INTC
                                        BINARY SEARCH.
      IF P_DATUM <= C_CG_DATE.

* by Daniel on 11/03/10 {
*        P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
*                             - GT_STOCK_07312010-QTY_CGATE.

        P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
                    - GT_STOCK_07312010-QTY_CGATE
                    - GT_STOCK-S_DISPOSAL.
* }

** Changed by Furong on 08/06/10
*                             - GT_STOCK-S_DISPOSAL
*                             - GT_STOCK-V_DISPOSAL.
** Changed by Furong on 08/09/10
*                             - GT_STOCK-S_DISPOSAL.
** End of change on 08/09/10
** End of change
      ELSE.
** Changed by Furong on 08/03/10
** Changed by Furong on 08/09/10

* by Daniel on 11/03/10 {
*        P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
*                              - GT_STOCK_07312010-QTY_CGATE
*** Changed by Furong on 08/09/10
**                              - GT_STOCK_08012010-QTY_VPCOUT.
*                              - GT_STOCK_08012010-QTY_MGATE.

*        P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
*                    - GT_STOCK_07312010-QTY_CGATE
*                    - GT_STOCK_08012010-QTY_MGATE
*                    - GT_STOCK-S_DISPOSAL
*                    - GT_STOCK-V_DISPOSAL.

* by Daniel on 11/04/10 {
        P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
                    - GT_STOCK_07312010-QTY_CGATE
                    - GT_STOCK_08012010-QTY_MGATE
                    - GT_STOCK-S_DISPOSAL
                    - GT_STOCK_08012010-V_DISPOSAL.

*        P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
*                    - GT_STOCK-QTY_SHIPIN
*                    - GT_STOCK-S_DISPOSAL
*                    - GT_STOCK-V_DISPOSAL.
* }
* }

** End of change on 08/09/10


** Changed by Furong on 08/09/10
*                              - GT_STOCK-S_DISPOSAL.
** End of change on 08/09/10
*        P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
*                               - GT_STOCK_07312010-QTY_CGATE
*                               - GT_STOCK_08012010-QTY_MGATE
*                               - GT_STOCK-S_DISPOSAL
*                               - GT_STOCK-V_DISPOSAL.
** End of change ON 8/09/10
** End of change ON 8/03/10
      ENDIF.
** End of change
    ELSE.
** Changed by Furong on 08/09/10
*      P_ACT-STOCK =  GT_STOCK-QTY_SIGNOFF
*                           - GT_STOCK-QTY_SHIPOUT
*                           - GT_STOCK-S_DISPOSAL
*                           - GT_STOCK-V_DISPOSAL.

* by Daniel on 11/03/10 {
*     P_ACT-STOCK =  GT_STOCK-QTY_SIGNOFF
*                           - GT_STOCK-QTY_SHIPOUT.

* by Daniel on 11/03/10 {
*      P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
*                  - GT_STOCK-QTY_SHIPOUT
*                  - GT_STOCK-S_DISPOSAL
*                  - GT_STOCK-V_DISPOSAL.

      P_ACT-STOCK = GT_STOCK-QTY_SIGNOFF
                  - GT_STOCK-QTY_SHIPOUT
                  - GT_STOCK-S_DISPOSAL
                  - GT_STOCK-V_DISPOSAL
                  - GT_STOCK-H_DISPOSAL.
* }
* }

** Changed by Furong on 08/09/10
*                           - GT_STOCK-S_DISPOSAL.
** End of change ON 8/09/10
** End of change ON 8/09/10

    ENDIF.


** End of change on 01/13/10
  ENDIF.

ENDFORM.                    " fill_act_table
