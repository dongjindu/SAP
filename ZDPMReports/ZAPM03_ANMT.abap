
************************************************************************
* Program Name      : ZAPM03_ANMT
* Author            : Myoungho, Park
* Creation Date     : 2003.08.20.
* Specifications By : Myoungho, Park
* Development Request No :
* Addl Documentation:
* Description       : Calculate Annual MTTR/ MTBF
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT ZAPM03_ANMT NO STANDARD PAGE HEADING
                   LINE-SIZE 132
                   LINE-COUNT 64(1)
                   MESSAGE-ID ZMPM.

TABLES: ZTPM_SHOP, "//Shop Master
        ZTPM_ANMT. "//Annual average MTTR/MTBF

**** internal table for shop list
DATA: BEGIN OF IT_SHOP OCCURS 0,
        SHOP LIKE ZTPM_SHOP-SHOP,
      END OF IT_SHOP.

*** internal table for year list
DATA: BEGIN OF IT_YEAR OCCURS 0,
         AJAHR LIKE ZTPM_ANMT-AJAHR,
      END OF IT_YEAR.

DATA: IT_TEMP_RATE LIKE	ZTPM_ANMT OCCURS 0 WITH HEADER LINE,
      IT_RATE      LIKE	ZTPM_ANMT OCCURS 0 WITH HEADER LINE.



*************** SELECTION-SCREEN  *****************************
***************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_AJAHR FOR ZTPM_ANMT-AJAHR,
                 S_SHOP  FOR ZTPM_SHOP-SHOP.
SELECTION-SCREEN END OF BLOCK BLOCK1.


************** INITIALIZATION ********************************
**************************************************************
INITIALIZATION.
**** set year default value..
  S_AJAHR-LOW = SY-DATUM(4).
  APPEND S_AJAHR.

************** START-OF-SELECTION *****************************
***************************************************************
START-OF-SELECTION.
**** select shop entry...
  SELECT DISTINCT SHOP  INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
          FROM  ZTPM_SHOP
          WHERE SHOP IN S_SHOP
          AND   SPRAS = SY-LANGU.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-002.
  ENDIF.
***** select year list....
  SELECT  DISTINCT AJAHR INTO CORRESPONDING FIELDS OF TABLE IT_YEAR
          FROM  ZTPM_OPTIME
          WHERE AJAHR IN S_SHOP.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-003.
  ENDIF.

END-OF-SELECTION.
**** calculate average MTTR / MTBF ratio
  PERFORM CAL_AVR_RATE.

  CHECK SY-SUBRC EQ 0.
**** Save data...
  PERFORM SAVE_DATA.
*** display Error message
  PERFORM WRITE_RESULT.

*&---------------------------------------------------------------------*
*&      Form  CAL_AVR_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_AVR_RATE.
  CLEAR: IT_RATE, IT_RATE[].
  LOOP AT IT_YEAR.
    LOOP AT IT_SHOP.
      PERFORM CAL_MTTR USING IT_YEAR-AJAHR
                             IT_SHOP-SHOP.

      PERFORM CAL_MTBF USING IT_YEAR-AJAHR
                             IT_SHOP-SHOP.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " CAL_AVR_RATE
*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_RESULT.
  ULINE.
  LOOP AT IT_RATE.
    WRITE: / '|', IT_RATE-AJAHR,
             '|', IT_RATE-SHOP,
             '|', IT_RATE-ZMTBT,
             '|', IT_RATE-AVRATE,
             '|' .
  ENDLOOP.
  ULINE.
ENDFORM.                    " WRITE_RESULT
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA.
  DATA: IT_ZTPM_ANMT LIKE ZTPM_ANMT OCCURS 0 WITH HEADER LINE.

  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZTPM_ANMT
           FROM  ZTPM_ANMT
           WHERE SHOP  IN S_SHOP
           AND   AJAHR IN S_AJAHR.

  LOOP AT IT_RATE.
    READ TABLE IT_ZTPM_ANMT WITH KEY SHOP  = IT_RATE-SHOP
                                    AJAHR  = IT_RATE-AJAHR.
    IF SY-SUBRC EQ 0.
      MOVE: IT_ZTPM_ANMT-ERDAT TO	IT_RATE-ERDAT,
            IT_ZTPM_ANMT-ERZET TO	IT_RATE-ERZET,
            IT_ZTPM_ANMT-ERNAM TO	IT_RATE-ERNAM.
      MOVE: SY-DATUM TO IT_RATE-AEDAT,
            SY-UZEIT TO IT_RATE-AEZET,
            SY-UNAME TO IT_RATE-AENAM.
    ELSE.
      MOVE: SY-DATUM TO	IT_RATE-ERDAT,
            SY-UZEIT TO	IT_RATE-ERZET,
            SY-UNAME TO	IT_RATE-ERNAM,
            SY-DATUM TO IT_RATE-AEDAT,
            SY-UZEIT TO IT_RATE-AEZET,
            SY-UNAME TO IT_RATE-AENAM.
    ENDIF.
    MODIFY IT_RATE.
  ENDLOOP.

  MODIFY ZTPM_ANMT FROM TABLE IT_RATE.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-004.
  ELSE.
    MESSAGE S000(ZMPM) WITH TEXT-005.
  ENDIF.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  CAL_MTTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_MTTR USING P_AJAHR P_SHOP.
  CLEAR: IT_TEMP_RATE, IT_TEMP_RATE[].
*** CALL FUNCTION Calculate MTTR Function
  CALL FUNCTION 'Z_FPM_MTTR'
       EXPORTING
            E_AJAHR = P_AJAHR
            E_SHOP  = P_SHOP
       TABLES
            T_RATE  = IT_TEMP_RATE.
  APPEND LINES OF IT_TEMP_RATE TO IT_RATE.
ENDFORM.                    " CAL_MTTR
*&---------------------------------------------------------------------*
*&      Form  CAL_MTBF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_MTBF USING P_AJAHR P_SHOP.
  CLEAR: IT_TEMP_RATE, IT_TEMP_RATE[].
**** call functionCalculate MTBF function
  CALL FUNCTION 'Z_FPM_MTBF'
       EXPORTING
            E_AJAHR = P_AJAHR
            E_SHOP  = P_SHOP
       TABLES
            T_RATE  = IT_TEMP_RATE.
  APPEND LINES OF IT_TEMP_RATE TO IT_RATE.
ENDFORM.                    " CAL_MTBF
