*&----------------------------------------------------------------------
*& Program Name   : ZRPP_219_MATERIALT
*& Created by     : Furong Wang
*& Created on     : 04/05/2012
*& Reference Pgm  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================

*&----------------------------------------------------------------------
REPORT ZRPP_219_MATERIAL NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

TABLES : ZSMM_219_PARTS, T001W.

FIELD-SYMBOLS: <FS_CLN>, <FS_VAL>.

DATA : IT_DATA LIKE ZTMM_219_PARTS OCCURS 0 WITH HEADER LINE.

DATA: W_MATNR_TEXT LIKE MAKT-MAKTX,
      W_DISPO LIKE MARC-DISPO,
      W_DISPO_TEXT LIKE T024D-DSNAM,
      W_VSPVB LIKE MARC-VSPVB,
      W_SORTF LIKE STPO-SORTF,
      W_MEINS LIKE MARA-MEINS.

*---- LIST BOX DATA
DATA: XNAME    TYPE VRM_ID,
      XLIST    TYPE VRM_VALUES,
      XVALUE   LIKE LINE OF XLIST,
      NAME     TYPE VRM_ID.    "Field Name..

*----------------------------------------------------------------------*
*                       VARIABLE DECLARATION                           *
*----------------------------------------------------------------------*
DATA : OKCODE   LIKE  SY-UCOMM,
       OK_CODE  LIKE  SY-UCOMM.

*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
  PERFORM INITIALIZATION.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION .
*  ZSMM_219_PARTS-DATBI = SY-DATUM.
  ZSMM_219_PARTS-DATAB = '99991231'.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS OUTPUT.
  SET PF-STATUS 'ST100'.
  SET TITLEBAR  'ST100'.
ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_GET_DATA
*&---------------------------------------------------------------------*
FORM CHECK_DATA USING P_ERROR.
  DATA: LT_ALC LIKE TABLE OF ZTBM_ABXOPVDT WITH HEADER LINE.

  DATA:  L_CN(2) TYPE N,
         L_TEXT(40),
         L_COLUMN LIKE ZTBM_ABXOPVDT-CLNO,
         L_VALUE LIKE ZTBM_ABXOPVDT-VALU.

  IF NOT ZSMM_219_PARTS-MATNR IS INITIAL.
    SELECT SINGLE MEINS INTO ZSMM_219_PARTS-MEINS
        FROM MARA
      WHERE MATNR = ZSMM_219_PARTS-MATNR.
    IF SY-SUBRC <> 0.
      P_ERROR = 'X'.
      MESSAGE I001 WITH 'Material does not exist'.
      EXIT.
    ENDIF.
  ENDIF.

  IF ZSMM_219_PARTS-MATNR IS INITIAL
     OR ZSMM_219_PARTS-WERKS IS INITIAL
     OR ZSMM_219_PARTS-MODEL IS INITIAL.
    P_ERROR = 'X'.
    MESSAGE I001 WITH 'Input neccessary fields..'.
    EXIT.
  ENDIF.
  IF ZSMM_219_PARTS-DATBI IS INITIAL AND
     ZSMM_219_PARTS-DATAB IS INITIAL.
    P_ERROR = 'X'.
    MESSAGE I001 WITH 'Input the Date...'.
    EXIT.
  ENDIF.
  IF ZSMM_219_PARTS-DATBI > ZSMM_219_PARTS-DATAB
     AND ZSMM_219_PARTS-DATAB IS NOT INITIAL.
    P_ERROR = 'X'..
    MESSAGE I001 WITH 'Lower limit is greater than upper limit.'.
    EXIT.
  ENDIF.
  IF ZSMM_219_PARTS-USG_MENGE IS INITIAL.
    P_ERROR = 'X'.
    MESSAGE I001 WITH 'Input Usage Data'.
    EXIT.
  ENDIF.

  SELECT * INTO TABLE LT_ALC
    FROM ZTBM_ABXOPVDT
    WHERE CARX = ZSMM_219_PARTS-MODEL+0(2).
  SORT LT_ALC BY CLNO VALU.

  L_CN = '01'.
  WHILE L_CN <= 16.
    CONCATENATE 'ZSMM_219_PARTS-' 'CLNO' L_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <FS_CLN>.
    L_COLUMN = <FS_CLN>.
    IF NOT L_COLUMN IS INITIAL.
      CONCATENATE 'ZSMM_219_PARTS-' 'VALU' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS_VAL>.
      L_VALUE = <FS_VAL>.
      CASE L_VALUE.
        WHEN '-'.
          READ TABLE LT_ALC WITH KEY CLNO = L_COLUMN
                                     BINARY SEARCH.
          IF SY-SUBRC <> 0.
            P_ERROR = 'X'.
            MESSAGE I001 WITH 'Wrong ALC code: '
                              L_COLUMN.
          ENDIF.
        WHEN ' '.
          P_ERROR = 'X'.
          MESSAGE I002 WITH 'Please input the value for ALC : '
                              L_COLUMN.

        WHEN OTHERS.
          READ TABLE LT_ALC WITH KEY CLNO = L_COLUMN
                                     VALU = L_VALUE
                              BINARY SEARCH.
          IF SY-SUBRC <> 0.
            P_ERROR = 'X'.
            MESSAGE I003 WITH
                    'Wrong ALC code/value : '
                     L_COLUMN '/' L_VALUE.
          ENDIF.
      ENDCASE.
    ENDIF.
    L_CN = L_CN + 1.
  ENDWHILE.


ENDFORM.                    " CHECK_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE_SET_1202  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_VALUE_SET OUTPUT.
  NAME = 'ZSMM_219_PARTS-MODEL'.
  PERFORM SET_FIELD_MODEL USING NAME ZSMM_219_PARTS-MODEL .

ENDMODULE.                 " INITIAL_VALUE_SET_1202  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAME  text
*      -->P_IS219_MODEL  text
*----------------------------------------------------------------------*
FORM SET_FIELD_MODEL USING P_NAME P_PARAMETER .
  DATA: L_ATINN              LIKE CABN-ATINN,
        L_ATNAM              LIKE CABN-ATNAM,
        L_ATWRT              LIKE AUSP-ATWRT,
        L_ATWTB              LIKE CAWNT-ATWTB.

  CLEAR : XLIST[],XVALUE.

  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
   WHERE ATNAM = 'P_MODEL'.

  SELECT N~ATWRT T~ATWTB INTO (L_ATWRT, L_ATWTB)
    FROM CAWN AS N INNER JOIN CAWNT AS T
      ON N~ATINN = T~ATINN
     AND N~ATZHL = T~ATZHL
   WHERE N~ATINN = L_ATINN
     AND T~SPRAS = SY-LANGU .
    XVALUE-TEXT = L_ATWRT.  " ZTPP_VEH_MODEL-NAME.  l_atwtb
    XVALUE-KEY  = L_ATWRT.  " ZTPP_VEH_MODEL-MODEL.
    APPEND XVALUE TO XLIST .
  ENDSELECT.

* LIST BOX SETTING
  PERFORM LIST_BOX_FUNCTION USING P_NAME.
*  IF P_PARAMETER IS INITIAL.
*    READ TABLE XLIST INTO XVALUE  INDEX 1.
*    P_PARAMETER = XVALUE-KEY.
*  ENDIF.
ENDFORM.                    " SET_FIELD_MODEL
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1202  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_100 INPUT.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM  SAVE_DATA.
    WHEN 'DISPLAY'.
      PERFORM  DISPLAY_DATA.
    WHEN 'DELETE'.
      PERFORM DELETE_DATA.
    WHEN 'LIST'.
      PERFORM LIST_DATA.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1202  INPUT
*&---------------------------------------------------------------------*
*&      Module  COLUME_VALUE_FIND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE COLUME_VALUE_FIND INPUT.
*
*  SELECT SINGLE MAKTX INTO W_MATNR_TEXT
*    FROM MAKT
*    WHERE MATNR = ZSMM_219_PARTS-MATNR
*      AND SPRAS = 'E'.
*
*  SELECT SINGLE MEINS INTO W_MEINS
*     FROM MARA
*   WHERE MATNR = ZSMM_219_PARTS-MATNR.
*
*  SELECT SINGLE B~DISPO DSNAM VSPVB
*   INTO (W_DISPO, W_DISPO_TEXT, W_VSPVB)
*   FROM MARC AS B
*   INNER JOIN T024D AS C
*   ON B~WERKS = C~WERKS
*   AND B~DISPO = C~DISPO
* WHERE MATNR = ZSMM_219_PARTS-MATNR
*   AND B~WERKS = ZSMM_219_PARTS-WERKS.
*
*  SELECT SINGLE SORTF INTO W_SORTF
*    FROM STPO
*    WHERE IDNRK = ZSMM_219_PARTS-MATNR.
*
*ENDMODULE.                 " COLUME_VALUE_FIND  INPUT
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NAME  text
*----------------------------------------------------------------------*
FORM LIST_BOX_FUNCTION USING P_LIST_NAME .
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = P_LIST_NAME  " list box
      VALUES          = XLIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.

ENDFORM.                    " LIST_BOX_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA .
  DATA: L_FLAG(1).

  PERFORM CHECK_DATA USING L_FLAG.
  PERFORM SORT_DATA.
  IF L_FLAG IS INITIAL.
    REFRESH: IT_DATA.
    IF ZSMM_219_PARTS-MEINS IS INITIAL.
      SELECT SINGLE MEINS INTO ZSMM_219_PARTS-MEINS
      FROM MARA
      WHERE MATNR = ZSMM_219_PARTS-MATNR.
    ENDIF.
    MOVE-CORRESPONDING ZSMM_219_PARTS TO IT_DATA.
    APPEND IT_DATA.
    MODIFY ZTMM_219_PARTS FROM TABLE IT_DATA.
    IF SY-SUBRC = 0.
      MESSAGE S001 WITH 'Data is saved successfully'.
      COMMIT WORK.
*      PERFORM CLEAR_DATA.
    ELSE.
      MESSAGE E001 WITH 'Error in data saving'.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  DATA: LW_219 LIKE ZTMM_219_PARTS.
*  CALL TRANSACTION 'ZPPR00011'.

  IF ZSMM_219_PARTS-MATNR <> ' ' AND
     ZSMM_219_PARTS-MODEL <> ' ' AND
     ZSMM_219_PARTS-WERKS <> ' '.
    SELECT * INTO LW_219 UP TO 1 ROWS
     FROM ZTMM_219_PARTS
     WHERE MATNR = ZSMM_219_PARTS-MATNR
       AND WERKS = ZSMM_219_PARTS-WERKS
       AND MODEL = ZSMM_219_PARTS-MODEL.
    ENDSELECT.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING LW_219 TO ZSMM_219_PARTS.
    ELSE.
      MESSAGE I001 WITH 'No 219 data found'.
    ENDIF.
  ELSE.
    MESSAGE I001 WITH 'Please input all neccessary fields'.
  ENDIF.

ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_DATA .
  CLEAR: ZSMM_219_PARTS, W_MATNR_TEXT,W_DISPO,W_DISPO_TEXT,
         W_VSPVB, W_SORTF, W_MEINS.

ENDFORM.                    " CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Module  COLUME_VALUE_FIND  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE COLUME_VALUE_FIND INPUT.
*  DATA: LW_219 LIKE ZTMM_219_PARTS,
*        L_MODEL(2).
*
*  SELECT SINGLE MAKTX INTO W_MATNR_TEXT
*    FROM MAKT
*    WHERE MATNR = ZSMM_219_PARTS-MATNR
*      AND SPRAS = 'E'.
*
*  SELECT SINGLE MEINS INTO ZSMM_219_PARTS-MEINS
*     FROM MARA
*   WHERE MATNR = ZSMM_219_PARTS-MATNR.
*
*  SELECT SINGLE B~DISPO DSNAM VSPVB
*   INTO (W_DISPO, W_DISPO_TEXT, W_VSPVB)
*   FROM MARC AS B
*   INNER JOIN T024D AS C
*   ON B~WERKS = C~WERKS
*   AND B~DISPO = C~DISPO
* WHERE MATNR = ZSMM_219_PARTS-MATNR
*   AND B~WERKS = ZSMM_219_PARTS-WERKS.
*
*  SELECT SINGLE SORTF INTO W_SORTF
*    FROM STPO
*    WHERE IDNRK = ZSMM_219_PARTS-MATNR.
**     AND WERKS =  ZSMM_219_PARTS-WERKS.
*
*  IF ZSMM_219_PARTS-MATNR <> ' ' AND
*     ZSMM_219_PARTS-MODEL <> ' ' AND
*     ZSMM_219_PARTS-WERKS <> ' '.
*    SELECT * INTO LW_219 UP TO 1 ROWS
*     FROM ZTMM_219_PARTS
*     WHERE MATNR = ZSMM_219_PARTS-MATNR
*       AND WERKS = ZSMM_219_PARTS-WERKS
*       AND MODEL = ZSMM_219_PARTS-MODEL.
*    ENDSELECT.
*    IF SY-SUBRC = 0.
*      MOVE-CORRESPONDING LW_219 TO ZSMM_219_PARTS.
*    ELSE.
*      CLEAR: ZSMM_219_PARTS-CLNO01, ZSMM_219_PARTS-VALU01,
*             ZSMM_219_PARTS-CLNO02, ZSMM_219_PARTS-VALU02,
*             ZSMM_219_PARTS-CLNO03, ZSMM_219_PARTS-VALU03,
*             ZSMM_219_PARTS-CLNO04, ZSMM_219_PARTS-VALU04,
*             ZSMM_219_PARTS-CLNO05, ZSMM_219_PARTS-VALU05,
*             ZSMM_219_PARTS-CLNO06, ZSMM_219_PARTS-VALU06,
*             ZSMM_219_PARTS-CLNO07, ZSMM_219_PARTS-VALU07,
*             ZSMM_219_PARTS-CLNO08, ZSMM_219_PARTS-VALU08,
*             ZSMM_219_PARTS-CLNO09, ZSMM_219_PARTS-VALU09,
*             ZSMM_219_PARTS-CLNO10, ZSMM_219_PARTS-VALU10,
*             ZSMM_219_PARTS-CLNO11, ZSMM_219_PARTS-VALU11,
*             ZSMM_219_PARTS-CLNO12, ZSMM_219_PARTS-VALU12,
*             ZSMM_219_PARTS-CLNO13, ZSMM_219_PARTS-VALU13,
*             ZSMM_219_PARTS-CLNO14, ZSMM_219_PARTS-VALU14,
*             ZSMM_219_PARTS-CLNO15, ZSMM_219_PARTS-VALU15,
*             ZSMM_219_PARTS-CLNO16, ZSMM_219_PARTS-VALU16,
*             ZSMM_219_PARTS-EXT_COLOR,
*             ZSMM_219_PARTS-INT_COLOR,
*             ZSMM_219_PARTS-USG_MENGE.
*    ENDIF.
*  ENDIF.
*  IF ZSMM_219_PARTS-MODEL <> ' '.
*    L_MODEL = ZSMM_219_PARTS-MODEL+0(2).
*    SET PARAMETER ID 'ZMOD' FIELD L_MODEL.
*  ENDIF.
ENDMODULE.                 " COLUME_VALUE_FIND  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_DATA .
  DATA: LW_219 LIKE ZSMM_219_PARTS,
        L_CN(2) TYPE N,
        L_TEXT(40).
  DATA: BEGIN OF LT_219 OCCURS 16,
        COLUMN(3) TYPE N,
        VALUE(1),
        END OF LT_219.

  L_CN = '01'.
  LW_219 = ZSMM_219_PARTS.

  WHILE L_CN <= 16.
    CONCATENATE 'LW_219-' 'CLNO' L_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <FS_CLN>.
    LT_219-COLUMN = <FS_CLN>.
    IF NOT LT_219-COLUMN IS INITIAL.
      CONCATENATE 'LW_219-' 'VALU' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS_VAL>.
      LT_219-VALUE = <FS_VAL>.
      APPEND LT_219.
    ENDIF.
    CLEAR: LT_219.
    L_CN = L_CN + 1.
  ENDWHILE.
  SORT LT_219 BY COLUMN.

  L_CN = '01'.
*  CLEAR: ZSMM_219_PARTS.
  LOOP AT LT_219.
    CONCATENATE ' ZSMM_219_PARTS-' 'CLNO' L_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <FS_CLN>.
    <FS_CLN> = LT_219-COLUMN.

    CONCATENATE ' ZSMM_219_PARTS-' 'VALU' L_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <FS_VAL>.
    <FS_VAL> = LT_219-VALUE.

    CLEAR: LT_219.
    L_CN = L_CN + 1.
  ENDLOOP.

  WHILE L_CN <= 17.
    CONCATENATE 'ZSMM_219_PARTS-' 'CLNO' L_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <FS_CLN>.
    <FS_CLN> = ' '.
    CONCATENATE 'ZSMM_219_PARTS-' 'VALU' L_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <FS_VAL>.
    <FS_VAL> = ' '.
    L_CN = L_CN + 1.
  ENDWHILE.

ENDFORM.                    " SORT_DATA
*&---------------------------------------------------------------------*
*&      Module  CHECK_MATNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_MATNR INPUT.

  IF ZSMM_219_PARTS-MATNR <> ' '.
    SELECT SINGLE MEINS INTO ZSMM_219_PARTS-MEINS
        FROM MARA
      WHERE MATNR = ZSMM_219_PARTS-MATNR.

    IF SY-SUBRC <> 0.
      PERFORM CLEAR_VARIABLE.
      SET CURSOR FIELD ZSMM_219_PARTS-MATNR.
      MESSAGE I001 WITH 'Material code does not exist'.
      EXIT.
    ENDIF.

    SELECT SINGLE MEINS INTO ZSMM_219_PARTS-MEINS
       FROM MARA
     WHERE MATNR = ZSMM_219_PARTS-MATNR.

    SELECT SINGLE MAKTX INTO W_MATNR_TEXT
       FROM MAKT
       WHERE MATNR = ZSMM_219_PARTS-MATNR
         AND SPRAS = 'E'.


    SELECT SINGLE B~DISPO DSNAM VSPVB
      INTO (W_DISPO, W_DISPO_TEXT, W_VSPVB)
      FROM MARC AS B
      INNER JOIN T024D AS C
      ON B~WERKS = C~WERKS
      AND B~DISPO = C~DISPO
      WHERE MATNR = ZSMM_219_PARTS-MATNR
      AND B~WERKS = ZSMM_219_PARTS-WERKS.

*    SELECT SINGLE SORTF DATUV
*      INTO (W_SORTF, ZSMM_219_PARTS-DATBI)
*      FROM STPO as a
*      inner join mast as b
*      on a~STLNR = b~STLNR
*      WHERE IDNRK = ZSMM_219_PARTS-MATNR
*        and b~STLAN = '1'.

    SELECT SINGLE USR01 INTO W_SORTF
      FROM PLPO
      WHERE PLNTY = 'M'
        AND PLNNR = 'RP'
        AND USR00 = W_VSPVB.
    IF SY-SUBRC = 0.
      ZSMM_219_PARTS-SORTF = W_SORTF.
    ENDIF.

    SELECT SINGLE a~DATUV
      INTO ZSMM_219_PARTS-DATBI
      FROM aenr as a
      inner join STPO as b
      on a~AENNR = b~AENNR
      inner join mast as c
      on b~STLNR = c~STLNR
      WHERE IDNRK = ZSMM_219_PARTS-MATNR
        and c~STLAN = '1'.

*    PERFORM CHECK_VALUE.
  ELSE.
    PERFORM CLEAR_VARIABLE.
  ENDIF.
ENDMODULE.                 " CHECK_MATNR  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM CHECK_VALUE.
*  DATA: LW_219 LIKE ZTMM_219_PARTS.
*
*  SELECT SINGLE B~DISPO DSNAM VSPVB
*  INTO (W_DISPO, W_DISPO_TEXT, W_VSPVB)
*  FROM MARC AS B
*  INNER JOIN T024D AS C
*  ON B~WERKS = C~WERKS
*  AND B~DISPO = C~DISPO
*  WHERE MATNR = ZSMM_219_PARTS-MATNR
*  AND B~WERKS = ZSMM_219_PARTS-WERKS.
*
*  IF ZSMM_219_PARTS-MATNR <> ' ' AND
*    ZSMM_219_PARTS-MODEL <> ' ' AND
*    ZSMM_219_PARTS-WERKS <> ' '.
*    SELECT * INTO LW_219 UP TO 1 ROWS
*     FROM ZTMM_219_PARTS
*     WHERE MATNR = ZSMM_219_PARTS-MATNR
*       AND WERKS = ZSMM_219_PARTS-WERKS
*       AND MODEL = ZSMM_219_PARTS-MODEL.
*    ENDSELECT.
*    IF SY-SUBRC = 0.
*      MOVE-CORRESPONDING LW_219 TO ZSMM_219_PARTS.
*    ELSE.
*      CLEAR: ZSMM_219_PARTS-CLNO01, ZSMM_219_PARTS-VALU01,
*             ZSMM_219_PARTS-CLNO02, ZSMM_219_PARTS-VALU02,
*             ZSMM_219_PARTS-CLNO03, ZSMM_219_PARTS-VALU03,
*             ZSMM_219_PARTS-CLNO04, ZSMM_219_PARTS-VALU04,
*             ZSMM_219_PARTS-CLNO05, ZSMM_219_PARTS-VALU05,
*             ZSMM_219_PARTS-CLNO06, ZSMM_219_PARTS-VALU06,
*             ZSMM_219_PARTS-CLNO07, ZSMM_219_PARTS-VALU07,
*             ZSMM_219_PARTS-CLNO08, ZSMM_219_PARTS-VALU08,
*             ZSMM_219_PARTS-CLNO09, ZSMM_219_PARTS-VALU09,
*             ZSMM_219_PARTS-CLNO10, ZSMM_219_PARTS-VALU10,
*             ZSMM_219_PARTS-CLNO11, ZSMM_219_PARTS-VALU11,
*             ZSMM_219_PARTS-CLNO12, ZSMM_219_PARTS-VALU12,
*             ZSMM_219_PARTS-CLNO13, ZSMM_219_PARTS-VALU13,
*             ZSMM_219_PARTS-CLNO14, ZSMM_219_PARTS-VALU14,
*             ZSMM_219_PARTS-CLNO15, ZSMM_219_PARTS-VALU15,
*             ZSMM_219_PARTS-CLNO16, ZSMM_219_PARTS-VALU16,
*             ZSMM_219_PARTS-EXT_COLOR,
*             ZSMM_219_PARTS-INT_COLOR,
*             ZSMM_219_PARTS-USG_MENGE.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " CHECK_VALUE
*&---------------------------------------------------------------------*
*&      Module  CHECK_MODEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_MODEL INPUT.
  DATA: L_MODEL(2).

  IF ZSMM_219_PARTS-MODEL <> ' '.
    L_MODEL = ZSMM_219_PARTS-MODEL+0(2).
    EXPORT L_MODEL TO MEMORY ID 'ZMOD'.
*    SET PARAMETER ID 'ZMOD' FIELD L_MODEL.
  ENDIF.
*  PERFORM CHECK_VALUE.
ENDMODULE.                 " CHECK_MODEL  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE CHECK_VALUE INPUT.
*  PERFORM CHECK_VALUE.
*ENDMODULE.                 " CHECK_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CLEAR_VARIABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_VARIABLE .
  CLEAR: W_MATNR_TEXT,W_DISPO,W_DISPO_TEXT,
           W_VSPVB, W_SORTF, W_MEINS.
  CLEAR: ZSMM_219_PARTS-CLNO01, ZSMM_219_PARTS-VALU01,
             ZSMM_219_PARTS-CLNO02, ZSMM_219_PARTS-VALU02,
             ZSMM_219_PARTS-CLNO03, ZSMM_219_PARTS-VALU03,
             ZSMM_219_PARTS-CLNO04, ZSMM_219_PARTS-VALU04,
             ZSMM_219_PARTS-CLNO05, ZSMM_219_PARTS-VALU05,
             ZSMM_219_PARTS-CLNO06, ZSMM_219_PARTS-VALU06,
             ZSMM_219_PARTS-CLNO07, ZSMM_219_PARTS-VALU07,
             ZSMM_219_PARTS-CLNO08, ZSMM_219_PARTS-VALU08,
             ZSMM_219_PARTS-CLNO09, ZSMM_219_PARTS-VALU09,
             ZSMM_219_PARTS-CLNO10, ZSMM_219_PARTS-VALU10,
             ZSMM_219_PARTS-CLNO11, ZSMM_219_PARTS-VALU11,
             ZSMM_219_PARTS-CLNO12, ZSMM_219_PARTS-VALU12,
             ZSMM_219_PARTS-CLNO13, ZSMM_219_PARTS-VALU13,
             ZSMM_219_PARTS-CLNO14, ZSMM_219_PARTS-VALU14,
             ZSMM_219_PARTS-CLNO15, ZSMM_219_PARTS-VALU15,
             ZSMM_219_PARTS-CLNO16, ZSMM_219_PARTS-VALU16,
             ZSMM_219_PARTS-EXT_COLOR,
             ZSMM_219_PARTS-INT_COLOR,
             ZSMM_219_PARTS-USG_MENGE.

ENDFORM.                    " CLEAR_VARIABLE
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_DATA.
  DATA: LW_219 LIKE ZTMM_219_PARTS.
*  CALL TRANSACTION 'ZPPR00011'.

  SELECT * INTO LW_219 UP TO 1 ROWS
  FROM ZTMM_219_PARTS
  WHERE MATNR = ZSMM_219_PARTS-MATNR
    AND WERKS = ZSMM_219_PARTS-WERKS
    AND MODEL = ZSMM_219_PARTS-MODEL.
  ENDSELECT.
  IF SY-SUBRC = 0.
*    DELETE FROM ZTMM_219_PARTS
*     WHERE MATNR =  ZSMM_219_PARTS-MATNR
*       AND WERKS = ZSMM_219_PARTS-WERKS
*       AND MODEL = ZSMM_219_PARTS-MODEL.
    DELETE ZTMM_219_PARTS FROM LW_219.

    IF SY-SUBRC = 0.
      MESSAGE S001 WITH 'The data has been deleted'.
      COMMIT WORK.
      PERFORM CLEAR_VARIABLE.
    ELSE.
      MESSAGE E001 WITH 'Error in deleting the data'.
      ROLLBACK WORK.
    ENDIF.
  ELSE.
    MESSAGE I001 WITH 'There is no 219 data existed'.
  ENDIF.

ENDFORM.                    " DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  LIST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIST_DATA .
  CALL TRANSACTION 'ZPPR00011'.
ENDFORM.                    " LIST_DATA
*&---------------------------------------------------------------------*
*&      Module  CHECK_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_WERKS INPUT.
  IF ZSMM_219_PARTS-WERKS IS INITIAL.
  ELSE.
    SELECT SINGLE * FROM T001W
    WHERE WERKS = ZSMM_219_PARTS-WERKS.
    IF SY-SUBRC = 0 .
    ELSE.
      SET CURSOR FIELD ZSMM_219_PARTS-WERKS.
      MESSAGE I001 WITH 'Please the correct plant code'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_WERKS  INPUT
