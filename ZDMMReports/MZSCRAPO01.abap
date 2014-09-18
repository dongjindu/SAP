*----------------------------------------------------------------------*
***INCLUDE MZSCRAPO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1000 OUTPUT.
  SET PF-STATUS 'FIRST'.
  SET TITLEBAR '002'.

  GET PARAMETER ID 'ZQM_WRK' FIELD MARC-WERKS.
  IF MARC-WERKS IS INITIAL.
    MESSAGE E026 WITH 'Please enter your plant in your user profile'.
    EXIT.
  ENDIF.

  TRANSLATE MARC-WERKS TO UPPER CASE.
  IF MARC-WERKS = 'E001' OR MARC-WERKS = 'E002'.
    LOOP AT SCREEN.
      IF SCREEN-NAME = '%#AUTOTEXT009'
      OR SCREEN-NAME = '%#AUTOTEXT011'
*      OR SCREEN-NAME = '%#AUTOTEXT013'
      OR SCREEN-NAME = '%#AUTOTEXT002'
      OR SCREEN-NAME = '%#AUTOTEXT010'
      OR SCREEN-NAME = '%#AUTOTEXT012'.
        SCREEN-INVISIBLE = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " STATUS_1000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1001 OUTPUT.
  SET PF-STATUS 'SECOND'.
  SET TITLEBAR '001'.

** Changed by Furong on 01/07/09
*  marc-werks = 'P001'.

*  IF MARC-WERKS IS INITIAL.
*    IF SY-TCODE = 'ZSCRAP1_ENG'.
*      MARC-WERKS = 'E001'.
*    ELSE.
**  GET PARAMETER ID 'WRK' FIELD MARC-WERKS.
*      MARC-WERKS = 'P001'.
*    ENDIF.
*  ENDIF.
  SET CURSOR FIELD 'MATNR'.

  IF   W_OR_REPLACE = 'VOQN' AND SY-UCOMM = 'VOQN'.
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
      EXPORTING
*        TITEL              = ' '
      TEXTLINE1     = '    You have entered the part reorder screen,'
        TEXTLINE2     = '    first a scrap ticket must be generated.'
       START_COLUMN       = 30
       START_ROW          = 8.
  ENDIF.

  IF W_CALL = 'SSSC'.
*    LOOP AT SCREEN.
*      IF SCREEN-NAME = 'LFA1-LIFNR'.
*        SCREEN-INPUT = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.

    SELECT SINGLE LIFNUM INTO LIFNR
         FROM QMEL
         WHERE QMNUM = QMNUM.

  ENDIF.
* End of change
ENDMODULE.                 " STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1002 OUTPUT.
  DATA: L_CURSOR(20).

  W_REPID = SY-REPID.
  W_DYNNR = SY-DYNNR.

  SET PF-STATUS 'THIRD'.
  SET TITLEBAR '003'.

  RIWO00-QMART = 'Q3'.
*  rmmg1-lgnum  = 'P01'.
*  rmmg1-lgort  = 'P499'.
*  bwart = '313'.
  T001L-LGORT  = LGORT.

  IF W_CALL = 'SSRW'.
    RIWO00-QMART = 'Q4'.
    LOOP AT SCREEN.
      IF SCREEN-NAME = 'RMMG1-LGNUM' OR
         SCREEN-NAME = 'T001L-LGORT' OR
         SCREEN-NAME = 'RMMG1-LGORT' OR
         SCREEN-NAME = 'MSEG-CHARG' OR
         SCREEN-NAME = 'BWART' OR
         SCREEN-NAME = 'RIWO00-QMART'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF FEGRP = '0'.

    LOOP AT SCREEN.
      IF SCREEN-NAME = 'URGRP'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ELSEIF SCREEN-NAME = 'URCOD'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*  IF SY-TCODE = 'ZSCRAP1_ENG'.
  IF MARC-WERKS = 'E001' OR MARC-WERKS = 'E002'.
    LOOP AT SCREEN.
      IF SCREEN-NAME = 'RMMG1-LGNUM'
       OR SCREEN-NAME = 'ITOBATTR-EQUNR'
       OR SCREEN-NAME = 'MSEG-CHARG'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.



  ENDIF.
*  if LFA1-LIFNR = 'AG7N'.
*
*  LOOP AT SCREEN.
*      IF SCREEN-NAME = 'LFA1-LIFNR'.
*        SCREEN-INPUT = '1'.
*        MODIFY SCREEN.
*      ENDIF.
*  ENDLOOP.
*  endif.

** Changed by Furong on 01/07/09
  IF RKMNG IS INITIAL.
    PERFORM DISPLAY_VAR.
    SET CURSOR FIELD 'RKMNG'.
    P_CURSOR = 'RKMNG'.
  ELSE.
    IF  P_CURSOR = 'RKMNG'.
      SET CURSOR FIELD 'FEGRP'.
      CLEAR: P_CURSOR.
    ENDIF.
  ENDIF.
** End of change

  IF LFA1-LIFNR IS INITIAL.
    IF LGORT =  'P100' OR
        LGORT =  'P110' OR
        LGORT =  'P120'.
      LFA1-LIFNR = 'AG7N'.
      SELECT SINGLE NAME1 INTO LFA1-NAME1 FROM LFA1
            WHERE LIFNR = LFA1-LIFNR.
    ENDIF.
  ENDIF.

ENDMODULE.                 " STATUS_1002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1100 OUTPUT.
  SET PF-STATUS 'FOURTH'.
  SET TITLEBAR '004'.

ENDMODULE.                 " STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1003 OUTPUT.
  SET PF-STATUS 'FIFTH'.
  SET TITLEBAR '005'.

ENDMODULE.                 " STATUS_1003  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1801  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1801 OUTPUT.
  SET PF-STATUS 'REPTAG'.
  SET TITLEBAR 'REPTAG'.
  PERFORM INIT_DATA.
ENDMODULE.                 " STATUS_1801  OUTPUT
**&---------------------------------------------------------------------
**
**&      Module  STATUS_1802  OUTPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE STATUS_1802 OUTPUT.
*  SET PF-STATUS 'PQREVIEW'.
*  SET TITLEBAR 'PQRE'.
*ENDMODULE.                 " STATUS_1802  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_1004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1004 OUTPUT.
  DATA: L_EQUNR LIKE ITOBATTR-EQUNR.
  SET PF-STATUS 'EIGHT'.
  SET TITLEBAR '008'.

  T001L-LGORT  = LGORT.

  BWART = '201'.
  IF FEGRP = '0'.

    LOOP AT SCREEN.
      IF SCREEN-NAME = 'URGRP'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ELSEIF SCREEN-NAME = 'URCOD'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF RKMNG IS INITIAL.
    SET CURSOR FIELD 'RKMNG'.
    P_CURSOR = 'RKMNG'.
  ELSE.
    IF  P_CURSOR = 'RKMNG'.
      SET CURSOR FIELD 'FEGRP'.
      CLEAR: P_CURSOR.
    ENDIF.
  ENDIF.


ENDMODULE.                 " status_1004  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_1005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1005 OUTPUT.
* DATA: L_EQUNR LIKE ITOBATTR-EQUNR.
  SET PF-STATUS 'SUPPLIER'.
  SET TITLEBAR 'SUPPLIER'.
  W_REPID = SY-REPID.
  W_DYNNR = SY-DYNNR.

  T001L-LGORT  = LGORT.

  BWART = '201'.
  IF FEGRP = '0'.

    LOOP AT SCREEN.
      IF SCREEN-NAME = 'URGRP'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ELSEIF SCREEN-NAME = 'URCOD'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF RKMNG IS INITIAL.
    SET CURSOR FIELD 'RKMNG'.
    P_CURSOR = 'RKMNG'.
  ELSE.
    IF  P_CURSOR = 'RKMNG'.
      SET CURSOR FIELD 'FEGRP'.
      CLEAR: P_CURSOR.
    ENDIF.
  ENDIF.

ENDMODULE.                 " status_1005  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_1006  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1006 OUTPUT.
*  SET PF-STATUS 'SAMPLE'.
  SET PF-STATUS 'TEST'.
  SET TITLEBAR 'SAMPLE'.


  W_REPID = SY-REPID.
  W_DYNNR = SY-DYNNR.

  T001L-LGORT  = LGORT.

  RKMNG = 1.
  BWART = '201'.
  IF FEGRP = '0'.

    LOOP AT SCREEN.
      IF SCREEN-NAME = 'URGRP'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ELSEIF SCREEN-NAME = 'URCOD'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF RKMNG IS INITIAL.
    SET CURSOR FIELD 'RKMNG'.
    P_CURSOR = 'RKMNG'.
  ELSE.
    IF  P_CURSOR = 'RKMNG'.
      SET CURSOR FIELD 'FEGRP'.
      CLEAR: P_CURSOR.
    ENDIF.
  ENDIF.
ENDMODULE.                 " status_1006  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1802  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1802 OUTPUT.
  SET PF-STATUS 'REPRINTO'.
  IF W_CALL = 'SHD'.
    SET TITLEBAR 'REPRINTO'.
  ELSE.
    SET TITLEBAR 'REPRINTSAMPLE'.
  ENDIF.

  PERFORM INIT_DATA.

ENDMODULE.                 " STATUS_1802  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1201  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1201 OUTPUT.
*  IF SY-UNAME = '101457' OR SY-UNAME = 'his20068'
*  OR SY-UNAME = 'HIS20068'.
*  else.
  AUTHORITY-CHECK OBJECT 'ZQM_SSTS' ID 'ZQM_SSTS' FIELD 'X'.
  IF SY-SUBRC <> 0.
    MESSAGE I026 WITH 'No Authorization to access the function'.
    LEAVE TO SCREEN 1000.
  ENDIF.
*  ENDIF.

  IF W_SUB_CALL = 'SSRW'.
    SET TITLEBAR '1201A'.
    SET PF-STATUS 'ST1201A'.
  ELSE.
    SET TITLEBAR '1201'.
    SET PF-STATUS 'ST1201'.
  ENDIF.
ENDMODULE.                 " STATUS_1201  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_1203  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1203 OUTPUT.
*  DATA: L_CURSOR(20).
  SET PF-STATUS 'THIRD'.
  SET TITLEBAR '1203'.

  RIWO00-QMART = 'Q3'.
*  rmmg1-lgnum  = 'P01'.
*  rmmg1-lgort  = 'P499'.
*  bwart = '313'.
  T001L-LGORT  = LGORT.

  LOOP AT SCREEN.

    IF SCREEN-NAME = 'URGRP' OR
       SCREEN-NAME = 'URCOD' OR
       SCREEN-NAME = 'FEGRP' OR
       SCREEN-NAME = 'FECOD' OR
       SCREEN-NAME = 'OTGRP' OR
       SCREEN-NAME =  'OTEIL' OR
       SCREEN-NAME = 'QMGRP' OR
       SCREEN-NAME = 'QMCOD'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.



*  if LFA1-LIFNR = 'AG7N'.
*
*  LOOP AT SCREEN.
*      IF SCREEN-NAME = 'LFA1-LIFNR'.
*        SCREEN-INPUT = '1'.
*        MODIFY SCREEN.
*      ENDIF.
*  ENDLOOP.
*  endif.

** Changed by Furong on 01/07/09
  IF RKMNG IS INITIAL.
*    PERFORM DISPLAY_VAR.
    PERFORM GET_VAR1203.
    SET CURSOR FIELD 'RKMNG'.
    P_CURSOR = 'RKMNG'.
  ELSE.
    IF  P_CURSOR = 'RKMNG'.
      SET CURSOR FIELD 'FEGRP'.
      CLEAR: P_CURSOR.
    ENDIF.
  ENDIF.
** End of change

  IF LFA1-LIFNR IS INITIAL.
    IF LGORT =  'P100' OR
        LGORT =  'P110' OR
        LGORT =  'P120'.
      LFA1-LIFNR = 'AG7N'.
      SELECT SINGLE NAME1 INTO LFA1-NAME1 FROM LFA1
            WHERE LIFNR = LFA1-LIFNR.
    ENDIF.
  ENDIF.
  PERFORM UPDATE_PRIMARY_VALUE.

  IF NOT ITOBATTR-EQUNR IS INITIAL.

    LOOP AT SCREEN.
      IF SCREEN-NAME = 'ITOBATTR-EQUNR'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " status_1203  OUTPUT
