************************************************************************
* Program Name      : ZIPP_PRDT_PLAN
* Author            : Furong Wang
* Creation Date     : 10/17/2006
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Send production actual and plan data to HMC
* Modification Logs
* Date       Developer    RequestNo    Description
*
*********************************************************************

REPORT  ZIPP_PRDT_PLAN NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

DATA: L_DAY_MONTH LIKE SY-DATUM,
      L_DAY_YEAR LIKE SY-DATUM.

DATA : L_MSGTXT(100),
       L_RESULT(1).
CONSTANTS: C_DEST(10) VALUE 'WMPP01'.   "Interface Destination.

DATA: IT_PROD_ACT LIKE TABLE OF ZTPP_ACT_HMC WITH HEADER LINE.
DATA: IT_PLAN_DAY LIKE TABLE OF ZTPP_PLAN_HMC WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_DATUM FOR SY-DATUM.
SELECTION-SCREEN END OF BLOCK B1.

START-OF-SELECTION.
  PERFORM GET_act_DATA.
  PERFORM SEND_act_DATA.

  PERFORM GET_PLAN_DATA.
  PERFORM SEND_PLAN_DATA.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_act_DATA.
 DATA: LT_PROD_ACT_TEMP LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE,
       LT_PMT01TB_COL LIKE TABLE OF ZTPP_PMT01TB_COL WITH HEADER LINE.

  SELECT * INTO TABLE LT_PROD_ACT_TEMP
         FROM ZTPP_PROD_ACTUAL
         WHERE PRDT_DATE IN S_DATUM. "BETWEEN Z_BEG_DATE AND Z_MAX_DATE.

  SELECT * INTO TABLE LT_PMT01TB_COL
    FROM ZTPP_PMT01TB_COL
    WHERE ZDATE IN S_DATUM.

  LOOP AT LT_PROD_ACT_TEMP.
    if LT_PROD_ACT_TEMP-DEST+3(2) = 'XX' OR
       LT_PROD_ACT_TEMP-DEST+3(2) = 'XY'.
       CONTINUE.
    ENDIF.
    MOVE-CORRESPONDING LT_PROD_ACT_TEMP TO IT_PROD_ACT.
    IF LT_PROD_ACT_TEMP-DEST+3(2) = 'AA'.
       READ TABLE LT_PMT01TB_COL WITH KEY
                                 ZDATE = LT_PROD_ACT_TEMP-PRDT_DATE
                                 ZMODL = LT_PROD_ACT_TEMP-MODEL
                                 ZNATN = LT_PROD_ACT_TEMP-NATN
                                 ZBMDL = LT_PROD_ACT_TEMP-BMDL
                                 ZOCN = LT_PROD_ACT_TEMP-OCN
                                 ZEXTC = LT_PROD_ACT_TEMP-EXTC
                                 ZINTC = LT_PROD_ACT_TEMP-INTC.
       IF SY-SUBRC = 0.
          IT_PROD_ACT-STOCK = LT_PMT01TB_COL-ZESTC.
       ENDIF.
    ENDIF.

    CONCATENATE IT_PROD_ACT-PRDT_DATE+0(6) '01' INTO L_DAY_MONTH.
    CONCATENATE IT_PROD_ACT-PRDT_DATE+0(4) '0101' INTO L_DAY_YEAR.

    SELECT SUM( QTY_SIGNOFF ) INTO IT_PROD_ACT-QTY_MM
    FROM ZTPP_PROD_ACTUAL
    WHERE HKCODE = LT_PROD_ACT_TEMP-HKCODE
      AND PLANT = LT_PROD_ACT_TEMP-PLANT
      AND MODEL = LT_PROD_ACT_TEMP-MODEL
      AND DEST = LT_PROD_ACT_TEMP-DEST
      AND NATN = LT_PROD_ACT_TEMP-NATN
      AND BMDL = LT_PROD_ACT_TEMP-BMDL
      AND OCN = LT_PROD_ACT_TEMP-OCN
      AND EXTC = LT_PROD_ACT_TEMP-EXTC
      AND INTC = LT_PROD_ACT_TEMP-INTC
      AND ( PRDT_DATE BETWEEN L_DAY_MONTH AND
            LT_PROD_ACT_TEMP-PRDT_DATE )
      GROUP BY HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC.
      EXIT.
    ENDSELECT.

    SELECT SUM( QTY_SIGNOFF ) INTO IT_PROD_ACT-QTY_YY
   FROM ZTPP_PROD_ACTUAL
   WHERE HKCODE = LT_PROD_ACT_TEMP-HKCODE
     AND PLANT = LT_PROD_ACT_TEMP-PLANT
     AND MODEL = LT_PROD_ACT_TEMP-MODEL
     AND DEST = LT_PROD_ACT_TEMP-DEST
     AND NATN = LT_PROD_ACT_TEMP-NATN
     AND BMDL = LT_PROD_ACT_TEMP-BMDL
     AND OCN = LT_PROD_ACT_TEMP-OCN
     AND EXTC = LT_PROD_ACT_TEMP-EXTC
     AND INTC = LT_PROD_ACT_TEMP-INTC
     AND ( PRDT_DATE BETWEEN L_DAY_YEAR AND
           LT_PROD_ACT_TEMP-PRDT_DATE )
     GROUP BY HKCODE PLANT MODEL DEST NATN BMDL OCN EXTC INTC.
      EXIT.
    ENDSELECT.
    IT_PROD_ACT-CRDATE = SY-DATUM.
    IT_PROD_ACT-TRDATE = SY-DATUM.
    APPEND IT_PROD_ACT.
    CLEAR: IT_PROD_ACT.
  ENDLOOP.
ENDFORM.

*call function ''.

FORM SEND_act_DATA.
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
    if sy-BATCH = 'X'.
       MESSAGE I001 WITH 'Production actual data successfully sent out'.
    Else.
    MESSAGE I001 WITH 'Production actual data successfully sent out,'
                      'Press ENTER to send plan data'.
    endif.
    MODIFY ZTPP_ACT_HMC FROM TABLE IT_PROD_ACT.
    IF SY-SUBRC = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE I000 WITH 'Table saving error'.
    ENDIF.
  ELSE.
    MESSAGE I001 WITH L_MSGTXT.
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
  SELECT * INTO TABLE LT_PLAN_DAY_TEMP
         FROM ZTPP_PLAN_DAY
         WHERE PRDT_DATE IN S_DATUM. "BETWEEN Z_BEG_DATE AND Z_MAX_DATE.
  LOOP AT LT_PLAN_DAY_temp.
    MOVE-CORRESPONDING LT_PLAN_dAY_TEMP TO IT_PLAN_DAY.
    IT_PLAN_DAY-CRDATE = sy-datum.
    IT_PLAN_DAY-TRDATE = sy-datum.
    append it_plan_day.
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
    MODIFY ZTPP_PLAN_HMC FROM TABLE IT_PLAN_DAY.
    IF SY-SUBRC = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE I000 WITH 'Table saving error'.
    ENDIF.
  ELSE.
    MESSAGE I001 WITH L_MSGTXT.
  ENDIF.

ENDFORM.                    " SEND_PLAN_DATA
