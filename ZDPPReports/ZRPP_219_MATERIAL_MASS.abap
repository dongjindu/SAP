*&----------------------------------------------------------------------
*& Program Name   : ZRPP_219_MATERIAL_MASS
*& Created by     : SE HO PARK
*& Created on     : 11/15/2013
*& Reference Pgm  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================

*&----------------------------------------------------------------------
REPORT ZRPP_219_MATERIAL_MASS NO STANDARD PAGE HEADING
                              LINE-SIZE 132
                              LINE-COUNT 64(1)
                              MESSAGE-ID ZMPP.

TYPE-POOLS: SLIS, VRM.

TABLES : ZSMM_219_PARTS, T001W.

FIELD-SYMBOLS: <FS_CLN>, <FS_VAL>.

DATA : IT_DATA LIKE ZTMM_219_PARTS OCCURS 0 WITH HEADER LINE.

DATA: W_MATNR_TEXT LIKE MAKT-MAKTX,
      W_DISPO LIKE MARC-DISPO,
      W_DISPO_TEXT LIKE T024D-DSNAM,
      W_VSPVB LIKE MARC-VSPVB,
      W_SORTF LIKE STPO-SORTF,
      W_MEINS LIKE MARA-MEINS.

DATA: BEGIN OF IT_OUTPUT OCCURS 0.
        INCLUDE STRUCTURE ZTMM_219_PARTS.
DATA: LIGHTS,
      MAKTX LIKE MAKT-MAKTX,
      DISPO LIKE MARC-DISPO,
*     SORTF LIKE STPO-SORTF,
      LGPRO LIKE MARC-LGPRO,
      VSPVB LIKE MARC-VSPVB,
      MENGE(10),
      219(100),
      SEQ   TYPE I,
      MARK,
      CELLTAB       TYPE LVC_T_STYL.     "Field to switch editability
DATA: END OF IT_OUTPUT.

DATA IS_219_PARTS      LIKE ZSMM_219_PARTS.
DATA IT_DELETE         LIKE IT_OUTPUT OCCURS 0 WITH HEADER LINE.

FIELD-SYMBOLS: <F_OUTPUT> LIKE LINE OF IT_OUTPUT.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

DATA : WA_IS_LAYOUT    TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME     LIKE LINE OF IT_FIELDNAME.  "IT_FIELDCAT.

DATA: WA_SAVE          TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT       TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA IT_TOOLBAR        TYPE UI_FUNCTIONS.

*---- LIST BOX DATA
DATA: XNAME    TYPE VRM_ID,
      XLIST    TYPE VRM_VALUES,
      XVALUE   LIKE LINE OF XLIST,
      NAME     TYPE VRM_ID.    "Field Name..

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA W_INDEX_ROWS       TYPE LVC_T_ROW  WITH HEADER LINE.
DATA W_ROW_NO           TYPE LVC_T_ROID WITH HEADER LINE.
DATA ST_ALV_STABLE      TYPE LVC_S_STBL.

DATA: W_ROW_ID TYPE LVC_S_ROW,
      W_COL_ID TYPE LVC_S_COL.

DATA: W_REPID LIKE SY-REPID,
      W_DYNNR LIKE SY-DYNNR,
      W_CNT   TYPE I.

DATA W_ANSWER.

CONSTANTS C_YES       TYPE C                VALUE '1'.

CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_handle_receiver1 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.

    METHODS HANDLE_TOOLBAR
              FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
                  IMPORTING E_OBJECT E_INTERACTIVE.

    METHODS HANDLE_DATA_CHANGED
              FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
                  IMPORTING ER_DATA_CHANGED.

    METHODS HANDLE_USER_COMMAND
              FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
                  IMPORTING E_UCOMM.

ENDCLASS.          " LCL_EVENT_RECEIVER DEFINITION1
*----------------------------------------------------------------------*
*       CLASS lcl_handle_receiver IMPLEMENTATION1
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  " ALV Toolbar Create.
  METHOD HANDLE_TOOLBAR.
    PERFORM HANDLE_TOOLBAR  USING E_OBJECT E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar

  " ALV Data Change.
  METHOD HANDLE_DATA_CHANGED.
    PERFORM HANDLE_DATA_CHANGED USING ER_DATA_CHANGED.
  ENDMETHOD.                   "HANDLE_DATA_CHANGED

  " ALV User Command
  METHOD HANDLE_USER_COMMAND.
    PERFORM HANDLE_USER_COMMAND USING E_UCOMM.
  ENDMETHOD.                    "handle_user_command

ENDCLASS.          " LCL_EVENT_RECEIVER IMPLEMENTATION1

DATA W_EVTR          TYPE REF TO LCL_EVENT_RECEIVER.

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
*  ZSMM_219_PARTS-DATAB = '99991231'.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS OUTPUT.

* Status
  DATA: BEGIN OF LT_EXTAB OCCURS 0,
          FCODE     LIKE  RSMPE-FUNC,
        END OF LT_EXTAB.

  CLEAR: LT_EXTAB, LT_EXTAB[].

  LT_EXTAB-FCODE = 'DELETE'.
  APPEND LT_EXTAB.

  LT_EXTAB-FCODE = 'LIST'.
  APPEND LT_EXTAB.

  SET PF-STATUS 'ST100' EXCLUDING LT_EXTAB.
  SET TITLEBAR  'ST100'.

ENDMODULE.                 " STATUS  OUTPUT
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
      PERFORM SAVE_DATA.
    WHEN 'DISPLAY'.
      PERFORM DISPLAY_DATA.
    WHEN 'DELETE'.
      PERFORM DELETE_DATA.
    WHEN 'LIST'.
      PERFORM LIST_DATA.
    WHEN 'FIND'.
      PERFORM CALL_SCREEN_0200.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1202  INPUT
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
FORM SAVE_DATA .

  DATA L_FLAG(1).
  DATA LS_219_PARTS    LIKE ZTMM_219_PARTS.
  DATA LT_219_PARTS    LIKE ZTMM_219_PARTS OCCURS 0 WITH HEADER LINE.
  DATA LT_219_DELETE   LIKE ZTMM_219_PARTS OCCURS 0 WITH HEADER LINE.

  IF IT_OUTPUT[] IS NOT INITIAL.
    CLEAR L_FLAG.
    LOOP AT IT_OUTPUT.
      IF IT_OUTPUT-DATBI IS INITIAL AND
      IT_OUTPUT-DATAB IS INITIAL.
        L_FLAG = 'X'.
        MESSAGE I001 WITH TEXT-M07.
        EXIT.
      ENDIF.

      IF IT_OUTPUT-DATBI > IT_OUTPUT-DATAB
      AND IT_OUTPUT-DATAB IS NOT INITIAL.
        L_FLAG = 'X'.
        MESSAGE I001 WITH TEXT-M08.
        EXIT.
      ENDIF.

      IF IT_OUTPUT-MENGE IS INITIAL.
        L_FLAG = 'X'.
        MESSAGE I001 WITH TEXT-M09.
        EXIT.
      ENDIF.
    ENDLOOP.

    CHECK L_FLAG IS INITIAL.
    CLEAR: LT_219_PARTS, LT_219_PARTS[].
    LOOP AT IT_OUTPUT.
      MOVE-CORRESPONDING IT_OUTPUT TO LT_219_PARTS.

      IF IT_OUTPUT-MEINS IS INITIAL.
        SELECT SINGLE MEINS
        INTO LT_219_PARTS-MEINS
        FROM MARA
        WHERE MATNR = IT_OUTPUT-MATNR.
      ENDIF.
      LT_219_PARTS-USG_MENGE = IT_OUTPUT-MENGE.
      LT_219_PARTS-WERKS  = ZSMM_219_PARTS-WERKS.
      LT_219_PARTS-MODEL  = ZSMM_219_PARTS-MODEL.
      LT_219_PARTS-CLNO01 = ZSMM_219_PARTS-CLNO01.
      LT_219_PARTS-VALU01 = ZSMM_219_PARTS-VALU01.
      LT_219_PARTS-CLNO02 = ZSMM_219_PARTS-CLNO02.
      LT_219_PARTS-VALU02 = ZSMM_219_PARTS-VALU02.
      LT_219_PARTS-CLNO03 = ZSMM_219_PARTS-CLNO03.
      LT_219_PARTS-VALU03 = ZSMM_219_PARTS-VALU03.
      LT_219_PARTS-CLNO04 = ZSMM_219_PARTS-CLNO04.
      LT_219_PARTS-VALU04 = ZSMM_219_PARTS-VALU04.
      LT_219_PARTS-CLNO05 = ZSMM_219_PARTS-CLNO05.
      LT_219_PARTS-VALU05 = ZSMM_219_PARTS-VALU05.
      LT_219_PARTS-CLNO06 = ZSMM_219_PARTS-CLNO06.
      LT_219_PARTS-VALU06 = ZSMM_219_PARTS-VALU06.
      LT_219_PARTS-CLNO07 = ZSMM_219_PARTS-CLNO07.
      LT_219_PARTS-VALU07 = ZSMM_219_PARTS-VALU07.
      LT_219_PARTS-CLNO08 = ZSMM_219_PARTS-CLNO08.
      LT_219_PARTS-VALU08 = ZSMM_219_PARTS-VALU08.
      LT_219_PARTS-CLNO09 = ZSMM_219_PARTS-CLNO09.
      LT_219_PARTS-VALU09 = ZSMM_219_PARTS-VALU09.
      LT_219_PARTS-CLNO10 = ZSMM_219_PARTS-CLNO10.
      LT_219_PARTS-VALU10 = ZSMM_219_PARTS-VALU10.
      LT_219_PARTS-CLNO11 = ZSMM_219_PARTS-CLNO11.
      LT_219_PARTS-VALU11 = ZSMM_219_PARTS-VALU11.
      LT_219_PARTS-CLNO12 = ZSMM_219_PARTS-CLNO12.
      LT_219_PARTS-VALU12 = ZSMM_219_PARTS-VALU12.
      LT_219_PARTS-CLNO13 = ZSMM_219_PARTS-CLNO13.
      LT_219_PARTS-VALU13 = ZSMM_219_PARTS-VALU13.
      LT_219_PARTS-CLNO14 = ZSMM_219_PARTS-CLNO14.
      LT_219_PARTS-VALU14 = ZSMM_219_PARTS-VALU14.
      LT_219_PARTS-CLNO15 = ZSMM_219_PARTS-CLNO15.
      LT_219_PARTS-VALU15 = ZSMM_219_PARTS-VALU15.
      LT_219_PARTS-CLNO16 = ZSMM_219_PARTS-CLNO16.
      LT_219_PARTS-VALU16 = ZSMM_219_PARTS-VALU16.
      APPEND LT_219_PARTS.  CLEAR LT_219_PARTS.

    ENDLOOP.
    IF IT_DELETE[] IS NOT INITIAL.
      LOOP AT IT_DELETE.
        DELETE FROM ZTMM_219_PARTS WHERE MATNR = IT_DELETE-MATNR
        AND WERKS = IT_DELETE-WERKS
        AND MODEL = IT_DELETE-MODEL.
      ENDLOOP.
    ENDIF.

    MODIFY ZTMM_219_PARTS FROM TABLE LT_219_PARTS.
    IF SY-SUBRC = 0.
      MESSAGE S001 WITH 'Data is saved successfully'.
      COMMIT WORK.
      CLEAR: IT_DELETE, IT_DELETE[].
      LOOP AT IT_OUTPUT.
        PERFORM BUILD_INPUT_STYLE.
        MODIFY IT_OUTPUT. CLEAR IT_OUTPUT.
      ENDLOOP.
      PERFORM ALV_CLASS_REFRESH USING ALV_GRID.

    ELSE.
      MESSAGE E001 WITH 'Error in data saving'.
      ROLLBACK WORK.

    ENDIF.

  ELSE.
    MESSAGE I001 WITH TEXT-M06.

  ENDIF.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.

  DATA: L_INDEX LIKE SY-TABIX,
  L_CN(2) TYPE N,
  L_TEXT(40),
  L_VAL_TEXT(40),
  L_COLUMN(3) TYPE N,
  L_VALUE(1),
  L_SIGN(1),
  L_LENGTH TYPE I.

  FIELD-SYMBOLS: <FS_CLN>, <FS_VAL>.

  DATA: LW_219 LIKE ZTMM_219_PARTS.

  CLEAR: IT_OUTPUT, IT_OUTPUT[].

  IF ZSMM_219_PARTS-MATNR IS NOT INITIAL.
    SELECT A~MATNR MAKTX A~WERKS MODEL DISPO LGPRO
    VSPVB DATBI A~DATAB EXT_COLOR INT_COLOR
    USG_MENGE A~MEINS CLNO01 VALU01 CLNO02 VALU02
    CLNO03 VALU03 CLNO04 VALU04 CLNO05 VALU05
    CLNO06 VALU06 CLNO07 VALU07 CLNO08 VALU08
    CLNO09 VALU09 CLNO10 VALU10 CLNO11 VALU11
    CLNO12 VALU12 CLNO13 VALU13 CLNO14 VALU14
    CLNO15 VALU15 CLNO16 VALU16
    INTO CORRESPONDING FIELDS OF TABLE IT_OUTPUT
    FROM ZTMM_219_PARTS AS A INNER JOIN MARA AS B
    ON A~MATNR = B~MATNR
    INNER JOIN MARC AS C
    ON B~MATNR = C~MATNR
    AND A~WERKS = C~WERKS
    INNER JOIN MAKT AS D
    ON B~MATNR = D~MATNR
    WHERE A~MATNR  = ZSMM_219_PARTS-MATNR
    AND A~WERKS  = ZSMM_219_PARTS-WERKS
    AND A~MODEL  = ZSMM_219_PARTS-MODEL
    AND A~CLNO01 = ZSMM_219_PARTS-CLNO01
    AND A~VALU01 = ZSMM_219_PARTS-VALU01
    AND A~CLNO02 = ZSMM_219_PARTS-CLNO02
    AND A~VALU02 = ZSMM_219_PARTS-VALU02
    AND A~CLNO03 = ZSMM_219_PARTS-CLNO03
    AND A~VALU03 = ZSMM_219_PARTS-VALU03
    AND A~CLNO04 = ZSMM_219_PARTS-CLNO04
    AND A~VALU04 = ZSMM_219_PARTS-VALU04
    AND A~CLNO05 = ZSMM_219_PARTS-CLNO05
    AND A~VALU05 = ZSMM_219_PARTS-VALU05
    AND A~CLNO06 = ZSMM_219_PARTS-CLNO06
    AND A~VALU06 = ZSMM_219_PARTS-VALU06
    AND A~CLNO07 = ZSMM_219_PARTS-CLNO07
    AND A~VALU07 = ZSMM_219_PARTS-VALU07
    AND A~CLNO08 = ZSMM_219_PARTS-CLNO08
    AND A~VALU08 = ZSMM_219_PARTS-VALU08
    AND A~CLNO09 = ZSMM_219_PARTS-CLNO09
    AND A~VALU09 = ZSMM_219_PARTS-VALU09
    AND A~CLNO10 = ZSMM_219_PARTS-CLNO10
    AND A~VALU10 = ZSMM_219_PARTS-VALU10
    AND A~CLNO11 = ZSMM_219_PARTS-CLNO11
    AND A~VALU11 = ZSMM_219_PARTS-VALU11
    AND A~CLNO12 = ZSMM_219_PARTS-CLNO12
    AND A~VALU12 = ZSMM_219_PARTS-VALU12
    AND A~CLNO13 = ZSMM_219_PARTS-CLNO13
    AND A~VALU13 = ZSMM_219_PARTS-VALU13
    AND A~CLNO14 = ZSMM_219_PARTS-CLNO14
    AND A~VALU14 = ZSMM_219_PARTS-VALU14
    AND A~CLNO15 = ZSMM_219_PARTS-CLNO15
    AND A~VALU15 = ZSMM_219_PARTS-VALU15
    AND A~CLNO16 = ZSMM_219_PARTS-CLNO16
    AND A~VALU16 = ZSMM_219_PARTS-VALU16.

  ENDIF.

  IF ZSMM_219_PARTS-MODEL <> ' ' AND
  ZSMM_219_PARTS-WERKS <> ' '.
    SELECT A~MATNR MAKTX A~WERKS MODEL DISPO LGPRO
    VSPVB DATBI A~DATAB EXT_COLOR INT_COLOR
    USG_MENGE A~MEINS CLNO01 VALU01 CLNO02 VALU02
    CLNO03 VALU03 CLNO04 VALU04 CLNO05 VALU05
    CLNO06 VALU06 CLNO07 VALU07 CLNO08 VALU08
    CLNO09 VALU09 CLNO10 VALU10 CLNO11 VALU11
    CLNO12 VALU12 CLNO13 VALU13 CLNO14 VALU14
    CLNO15 VALU15 CLNO16 VALU16
    APPENDING CORRESPONDING FIELDS OF TABLE IT_OUTPUT
    FROM ZTMM_219_PARTS AS A INNER JOIN MARA AS B
    ON A~MATNR = B~MATNR
    INNER JOIN MARC AS C
    ON B~MATNR = C~MATNR
    AND A~WERKS = C~WERKS
    INNER JOIN MAKT AS D
    ON B~MATNR = D~MATNR
    WHERE A~WERKS  = ZSMM_219_PARTS-WERKS
    AND A~MODEL  = ZSMM_219_PARTS-MODEL
    AND A~CLNO01 = ZSMM_219_PARTS-CLNO01
    AND A~VALU01 = ZSMM_219_PARTS-VALU01
    AND A~CLNO02 = ZSMM_219_PARTS-CLNO02
    AND A~VALU02 = ZSMM_219_PARTS-VALU02
    AND A~CLNO03 = ZSMM_219_PARTS-CLNO03
    AND A~VALU03 = ZSMM_219_PARTS-VALU03
    AND A~CLNO04 = ZSMM_219_PARTS-CLNO04
    AND A~VALU04 = ZSMM_219_PARTS-VALU04
    AND A~CLNO05 = ZSMM_219_PARTS-CLNO05
    AND A~VALU05 = ZSMM_219_PARTS-VALU05
    AND A~CLNO06 = ZSMM_219_PARTS-CLNO06
    AND A~VALU06 = ZSMM_219_PARTS-VALU06
    AND A~CLNO07 = ZSMM_219_PARTS-CLNO07
    AND A~VALU07 = ZSMM_219_PARTS-VALU07
    AND A~CLNO08 = ZSMM_219_PARTS-CLNO08
    AND A~VALU08 = ZSMM_219_PARTS-VALU08
    AND A~CLNO09 = ZSMM_219_PARTS-CLNO09
    AND A~VALU09 = ZSMM_219_PARTS-VALU09
    AND A~CLNO10 = ZSMM_219_PARTS-CLNO10
    AND A~VALU10 = ZSMM_219_PARTS-VALU10
    AND A~CLNO11 = ZSMM_219_PARTS-CLNO11
    AND A~VALU11 = ZSMM_219_PARTS-VALU11
    AND A~CLNO12 = ZSMM_219_PARTS-CLNO12
    AND A~VALU12 = ZSMM_219_PARTS-VALU12
    AND A~CLNO13 = ZSMM_219_PARTS-CLNO13
    AND A~VALU13 = ZSMM_219_PARTS-VALU13
    AND A~CLNO14 = ZSMM_219_PARTS-CLNO14
    AND A~VALU14 = ZSMM_219_PARTS-VALU14
    AND A~CLNO15 = ZSMM_219_PARTS-CLNO15
    AND A~VALU15 = ZSMM_219_PARTS-VALU15
    AND A~CLNO16 = ZSMM_219_PARTS-CLNO16
    AND A~VALU16 = ZSMM_219_PARTS-VALU16.

  ENDIF.

  IF IT_OUTPUT[] IS NOT INITIAL.
    SORT IT_OUTPUT BY MATNR WERKS MODEL.
    DELETE ADJACENT DUPLICATES FROM IT_OUTPUT
    COMPARING MATNR WERKS MODEL.

    IS_219_PARTS = ZSMM_219_PARTS.

    LOOP AT IT_OUTPUT.
      L_INDEX = SY-TABIX.
      L_CN = '01'.
      L_SIGN = ','.
      WHILE L_CN <= 16.
        CLEAR: L_COLUMN, L_VALUE.
        CONCATENATE 'IT_OUTPUT-' 'CLNO' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS_CLN>.
        L_COLUMN = <FS_CLN>.
        IF L_COLUMN <> ' '.
          CONCATENATE 'IT_OUTPUT-' 'VALU' L_CN INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS_VAL>.
          L_VALUE = <FS_VAL>.
          CONCATENATE IT_OUTPUT-219 L_COLUMN L_VALUE L_SIGN
          INTO IT_OUTPUT-219.
        ENDIF.
        L_CN = L_CN + 1.
      ENDWHILE.

*      L_LENGTH = STRLEN( IT_OUTPUT-219 ).
*      L_LENGTH = L_LENGTH - 1.
*      IT_OUTPUT-219 = IT_OUTPUT-219+0(L_LENGTH).

      SELECT SINGLE USR01
      INTO IT_OUTPUT-SORTF
      FROM PLPO
      WHERE PLNTY = 'M'
      AND PLNNR = 'RP'
      AND USR00 = IT_OUTPUT-VSPVB.

      IT_OUTPUT-MENGE = IT_OUTPUT-USG_MENGE.

      "Input Style
      PERFORM BUILD_INPUT_STYLE.

      IT_OUTPUT-SEQ = L_INDEX.
      MODIFY IT_OUTPUT.  CLEAR IT_OUTPUT.

    ENDLOOP.

  ELSE.
    MESSAGE I001 WITH 'No 219 data found'.

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

    SELECT SINGLE A~DATUV
    INTO ZSMM_219_PARTS-DATBI
    FROM AENR AS A
    INNER JOIN STPO AS B
    ON A~AENNR = B~AENNR
    INNER JOIN MAST AS C
    ON B~STLNR = C~STLNR
    WHERE IDNRK = ZSMM_219_PARTS-MATNR
    AND C~STLAN = '1'.

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
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CALL_SCREEN_0200 .

  DATA L_CHK.
  CLEAR L_CHK.

  IF ZSMM_219_PARTS-WERKS IS INITIAL.
    MESSAGE S001 WITH 'Plant is required field.' DISPLAY LIKE 'E'.
    L_CHK = 'X'.
  ENDIF.

  CHECK L_CHK IS INITIAL.
  IF ZSMM_219_PARTS-MODEL IS INITIAL.
    MESSAGE S001 WITH 'Model is required field.' DISPLAY LIKE 'E'.
    L_CHK = 'X'.
  ENDIF.

  CHECK L_CHK IS INITIAL.
  CLEAR ZSMM_219_PARTS-MATNR.
  CALL SCREEN 200 STARTING AT 31 5
                    ENDING AT 75 6.

ENDFORM.                    " CALL_SCREEN_0200
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.

  SET PF-STATUS '200'.
  SET TITLEBAR '200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE OK_CODE.
    WHEN 'OKAY'.
      PERFORM FIND_MATERIAL_DATA.

    WHEN 'CNCL'.
      CLEAR ZSMM_219_PARTS-MATNR.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  FIND_MATERIAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FIND_MATERIAL_DATA .

  DATA: LW_219 LIKE ZTMM_219_PARTS.

  IF ZSMM_219_PARTS-MATNR IS INITIAL.
    MESSAGE I001 WITH 'Input the Material Code'
    DISPLAY LIKE 'E'.

  ELSE.
    SELECT *  INTO LW_219
              UP TO 1 ROWS
              FROM ZTMM_219_PARTS
             WHERE MATNR = ZSMM_219_PARTS-MATNR
               AND WERKS = ZSMM_219_PARTS-WERKS
               AND MODEL = ZSMM_219_PARTS-MODEL.
    ENDSELECT.

    IF SY-SUBRC <> 0.
      PERFORM CLEAR_VARIABLE.
      SET CURSOR FIELD ZSMM_219_PARTS-MATNR.
      MESSAGE I001 WITH 'Material code does not exist'.

    ELSE.
      MOVE-CORRESPONDING LW_219 TO ZSMM_219_PARTS.
      CLEAR: IT_OUTPUT, IT_OUTPUT[].
      LEAVE TO SCREEN 0.
    ENDIF.

  ENDIF.

ENDFORM.                    " FIND_MATERIAL_DATA
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.

  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_TOOLBAR_GRID.
    PERFORM BUILD_EVENT_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_OUTPUT'.
    PERFORM BUILD_INPUT_STYLE_GRID.
    PERFORM ASSIGN_ITAB_TO_ALV.

  ELSE.
    IF W_ROW_ID IS INITIAL.
      PERFORM ALV_CLASS_REFRESH USING ALV_GRID.

    ELSE.
      CLEAR: W_ROW_ID, W_COL_ID.

    ENDIF.

  ENDIF.

ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  CLEAR: W_REPID.
  CREATE OBJECT GRID_CONTAINER
    EXPORTING
      CONTAINER_NAME              = WA_CUSTOM_CONTROL
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        TITEL = W_REPID
        TXT2  = SY-SUBRC
        TXT1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT ALV_GRID
    EXPORTING
      I_PARENT      = GRID_CONTAINER
      I_APPL_EVENTS = 'X'.

ENDFORM.                    " CREATE_CONTAINER_N_OBJECT
*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = ' '.   "/optimizes the column width
  WA_IS_LAYOUT-INFO_FNAME = 'IF'.
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  WA_IS_LAYOUT-EXCP_FNAME = 'LIGHTS'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  WA_IS_LAYOUT-STYLEFNAME  = 'CELLTAB'.

*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.

ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.

*  IT_SORT-SPOS           = 1.
*  IT_SORT-FIELDNAME      = 'MODEL'.
*  IT_SORT-UP             = 'X'.
**  IT_SORT-SUBTOT         = 'X'.
*  APPEND IT_SORT.

*  IT_SORT-SPOS           = 2.
*  IT_SORT-FIELDNAME      = 'MATNR'.
*  IT_SORT-UP             = 'X'.
**  IT_SORT-SUBTOT         = 'X'.
*  APPEND IT_SORT.

ENDFORM.                    " build_sortcat_display
*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.

  DATA: LW_ITAB TYPE SLIS_TABNAME.
*        lw_waers LIKE t001-waers,

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
  IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME     = W_REPID
      I_INTERNAL_TABNAME = LW_ITAB
      I_INCLNAME         = W_REPID
    CHANGING
      CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
  'S' 'MATNR'       ' ',
  ' ' 'REF_TABLE'   'MARA',
  ' ' 'REF_FIELD'   'MATNR',
  ' ' 'COLTEXT'     'Material',
  ' ' 'LZERO'       ' ',
  ' ' 'EDIT'        'X',
  'E' 'OUTPUTLEN'   '18',

  'S' 'MAKTX'        ' ',
  ' ' 'COLTEXT'     'Description',
  'E' 'OUTPUTLEN'   '30',

  'S' 'WERKS'       ' ',
  ' ' 'COLTEXT'     'Plant',
  'E' 'OUTPUTLEN'   '5',

  'S' 'LGPRO'       ' ',
  ' ' 'COLTEXT'     'Sloc',
  'E' 'OUTPUTLEN'   '4',

  'S' 'DISPO'       ' ',
  ' ' 'COLTEXT'     'MRP',
  'E' 'OUTPUTLEN'   '5',

  'S' 'SORTF'       ' ',
  ' ' 'COLTEXT'     'S. String',
  'E' 'OUTPUTLEN'   '6',

  'S' 'VSPVB'       ' ',
  ' ' 'COLTEXT'     'Supl Area',
  'E' 'OUTPUTLEN'   '10',

  'S' 'DATBI'       ' ',
  ' ' 'REF_TABLE'   'BKPF',
  ' ' 'REF_FIELD'   'BUDAT',
  ' ' 'COLTEXT'     'Valid from',
  ' ' 'EDIT'        'X',
  'E' 'OUTPUTLEN'   '10',

  'S' 'DATAB'       ' ',
  ' ' 'REF_TABLE'   'BKPF',
  ' ' 'REF_FIELD'   'BLDAT',
  ' ' 'COLTEXT'     'Valid to',
  ' ' 'EDIT'        'X',
  'E' 'OUTPUTLEN'   '10',

  'S' 'EXT_COLOR'   ' ',
  ' ' 'COLTEXT'     'Ext Col',
  ' ' 'EDIT'        'X',
  'E' 'OUTPUTLEN'   '8',

  'S' 'INT_COLOR'   ' ',
  ' ' 'COLTEXT'     'Int Col',
  ' ' 'EDIT'        'X',
  'E' 'OUTPUTLEN'   '8',

  'S' 'MENGE'       ' ',
  ' ' 'COLTEXT'     'Usage',
  ' ' 'LZERO'       ' ',
  ' ' 'EDIT'        'X',
  'E' 'OUTPUTLEN'   '8',

  'S' 'MEINS'       ' ',
  ' ' 'COLTEXT'     'UoM',
  ' ' 'EDIT'        ' ',
  'E' 'OUTPUTLEN'   '3',

  'S' '219'         ' ',
  ' ' 'NO_OUT'      'X',
  ' ' 'COLTEXT'     '219 Code',
  ' ' 'EDIT'        ' ',
  'E' 'OUTPUTLEN'   '100'.


ENDFORM.                    "build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
USING    P_GUBUN
P_FIELD
P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
    WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog'.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    ADD 1 TO W_CNT.
    P_FIELDCAT-COL_POS = W_CNT.
    APPEND P_FIELDCAT.
  ENDIF.

ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = WA_IS_LAYOUT
      I_SAVE               = WA_SAVE
      IS_VARIANT           = WA_VARIANT
      I_DEFAULT            = SPACE
      IT_TOOLBAR_EXCLUDING = IT_TOOLBAR[]
    CHANGING
      IT_FIELDCATALOG      = IT_FIELDCAT[]
      IT_OUTTAB            = IT_OUTPUT[]
      IT_SORT              = IT_SORT[].

*- AFTER ENTER
  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

*- AFTER CHANGED
  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.


ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_TOOLBAR USING E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
E_INTERACTIVE.

  "Separator Line
  PERFORM ALV_APPEND_TOOLBAR USING E_OBJECT
  '3'
  ' '
  ' '
  ' '
  ' '
  ' '.
  "Insert.
  PERFORM ALV_APPEND_TOOLBAR USING E_OBJECT
  ' '
  'LINEC'
  ICON_INSERT_ROW
  TEXT-T05
  TEXT-T05
  ' '.
  "Separator Line
  PERFORM ALV_APPEND_TOOLBAR USING E_OBJECT
  '3'
  ' '
  ' '
  ' '
  ' '
  ' '.
  "Delete.
  PERFORM ALV_APPEND_TOOLBAR USING E_OBJECT
  ' '
  'LINED'
  ICON_DELETE_ROW
  TEXT-T06
  TEXT-T06
  ' '.

ENDFORM.                    " HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  ALV_APPEND_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_APPEND_TOOLBAR USING E_OBJECT TYPE REF TO
CL_ALV_EVENT_TOOLBAR_SET
P_TYPE
P_FCODE
P_ICON
P_QUICKINFO
P_TEXT
P_DISABLED.

  DATA LS_TOOLBAR  TYPE STB_BUTTON.

  CLEAR LS_TOOLBAR.

  MOVE : P_TYPE       TO LS_TOOLBAR-BUTN_TYPE,
  P_FCODE      TO LS_TOOLBAR-FUNCTION,
  P_ICON       TO LS_TOOLBAR-ICON,
  P_QUICKINFO  TO LS_TOOLBAR-QUICKINFO,
  P_TEXT       TO LS_TOOLBAR-TEXT,
  P_DISABLED   TO LS_TOOLBAR-DISABLED.

  APPEND LS_TOOLBAR   TO E_OBJECT->MT_TOOLBAR.

ENDFORM.                    " ALV_APPEND_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  INSERT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM INSERT_LINE .

  DATA: LS_CELLTAB  TYPE LVC_S_STYL,
  L_MODE     TYPE RAW4,
  L_LINE     TYPE I.

*  CLEAR IT_OUTPUT.
*  DESCRIBE TABLE IT_OUTPUT LINES L_LINE.
*  ADD 1 TO L_LINE.
  LOOP AT  IT_OUTPUT.
    ADD 1 TO IT_OUTPUT-SEQ .
    MODIFY IT_OUTPUT. CLEAR IT_OUTPUT.
  ENDLOOP.

  IT_OUTPUT-MATNR = ''.
  IT_OUTPUT-DATBI = SY-DATUM.
  IT_OUTPUT-DATAB = '99991231'.
  IT_OUTPUT-SEQ = 1.
  APPEND IT_OUTPUT.

  SORT IT_OUTPUT BY SEQ.

ENDFORM.                    " INSERT_LINE
*&---------------------------------------------------------------------*
*&      Form  DELETE_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DELETE_LINE .

  DATA LV_CHK.
  DATA LV_MSG(100).
  DATA LV_LINE      TYPE N.

  PERFORM GET_SELECTED_ROWS USING ALV_GRID.

  IF W_ROW_NO[] IS INITIAL.
    MESSAGE S001 WITH TEXT-M02 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE W_ROW_NO LINES LV_LINE.
  IF LV_LINE > 1.
    MESSAGE S001 WITH TEXT-M03 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT W_INDEX_ROWS WHERE INDEX NE 0.
    CLEAR IT_OUTPUT.
    READ TABLE IT_OUTPUT INDEX W_INDEX_ROWS-INDEX.
    IT_OUTPUT-MARK = 'X'.
    MODIFY IT_OUTPUT INDEX W_INDEX_ROWS-INDEX.

  ENDLOOP.

  IF LV_CHK IS INITIAL.
    CHECK LV_CHK IS INITIAL.
    CLEAR W_ANSWER.
    PERFORM CALL_POPUP_CONFIRM USING C_YES
    TEXT-T01 " TITLE
    TEXT-T02
    ' '
    CHANGING W_ANSWER.
    CHECK W_ANSWER = C_YES.
    LOOP AT IT_OUTPUT WHERE MARK = 'X'.
      MOVE-CORRESPONDING IT_OUTPUT TO IT_DELETE.
      APPEND IT_DELETE. CLEAR IT_DELETE.
      DELETE IT_OUTPUT.
    ENDLOOP.

    PERFORM ALV_CLASS_REFRESH USING ALV_GRID.

  ELSE.
*    GT_DISP-MARK = ''.
*    MODIFY GT_DISP TRANSPORTING MARK WHERE MARK = 'X'.

  ENDIF.

ENDFORM.                    " DELETE_LINE
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_SELECTED_ROWS USING P_GRID TYPE REF TO CL_GUI_ALV_GRID.

  CLEAR: W_INDEX_ROWS, W_INDEX_ROWS[], W_ROW_NO, W_ROW_NO[].

  CALL METHOD P_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = W_INDEX_ROWS[]
      ET_ROW_NO     = W_ROW_NO[].

ENDFORM.                    " GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*&      Form  ALV_CLASS_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_CLASS_REFRESH  USING    P_GRID TYPE REF TO CL_GUI_ALV_GRID.

  " Refresh.
  ST_ALV_STABLE-ROW = 'X'.
  ST_ALV_STABLE-COL = 'X'.

  CALL METHOD P_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = ST_ALV_STABLE.

ENDFORM.                    " ALV_CLASS_REFRESH
*&---------------------------------------------------------------------*
*&      Form  call_popup_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CALL_POPUP_CONFIRM USING     P_DEFAULT
P_TEXT01
P_TEXT02
P_DISP_CA
CHANGING  P_ANSWER.

  CLEAR P_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = P_TEXT01
      TEXT_QUESTION         = P_TEXT02
      DEFAULT_BUTTON        = P_DEFAULT
      DISPLAY_CANCEL_BUTTON = P_DISP_CA
    IMPORTING
      ANSWER                = P_ANSWER.

  IF SY-SUBRC <> 0.
  ENDIF.

ENDFORM. " call_popup_confirm
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENT_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_EVENT_GRID .

  CREATE OBJECT W_EVTR.

  SET HANDLER W_EVTR->HANDLE_TOOLBAR      FOR ALV_GRID.
  SET HANDLER W_EVTR->HANDLE_DATA_CHANGED FOR ALV_GRID.
  SET HANDLER W_EVTR->HANDLE_USER_COMMAND FOR ALV_GRID.

ENDFORM.                    " BUILD_EVENT_GRID
*&---------------------------------------------------------------------*
*&      Form  BUILD_INPUT_STYLE_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_INPUT_STYLE_GRID .

  LOOP AT IT_OUTPUT.
    "Input Style
    PERFORM BUILD_INPUT_STYLE.

    MODIFY IT_OUTPUT. CLEAR IT_OUTPUT.

  ENDLOOP.

ENDFORM.                    " BUILD_INPUT_STYLE_GRID
*&---------------------------------------------------------------------*
*&      Form  BUILD_INPUT_STYLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_INPUT_STYLE .

*"Input colume Control
  DATA LT_CELLTAB   TYPE LVC_T_STYL.

  REFRESH LT_CELLTAB.

  PERFORM FILL_CELLTAB  CHANGING LT_CELLTAB.

  IT_OUTPUT-CELLTAB[] = LT_CELLTAB[].

ENDFORM.                    " BUILD_INPUT_STYLE
*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_CELLTAB  CHANGING PT_CELLTAB TYPE LVC_T_STYL.

  DATA : LS_CELLTAB TYPE LVC_S_STYL.

  DATA : BEGIN OF L_FIELDCAT,
  ROW_POS TYPE INT4,
  COL_POS TYPE INT4,
  FIELDNAME(30),
  END OF L_FIELDCAT.

  LOOP AT IT_FIELDCAT INTO L_FIELDCAT.
    LS_CELLTAB-FIELDNAME = L_FIELDCAT-FIELDNAME.

    CASE LS_CELLTAB-FIELDNAME.
      WHEN 'MATNR'.
        IF IT_OUTPUT-MATNR IS INITIAL.
          LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ELSE.
          LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        ENDIF.

      WHEN 'DATBI'.
        LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.

      WHEN 'DATAB'.
        LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.

      WHEN 'EXT_COLOR'.
        LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.

      WHEN 'INT_COLOR'.
        LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.

      WHEN 'MENGE'.
        LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.

      WHEN OTHERS.
        LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.

    ENDCASE.

    INSERT LS_CELLTAB INTO TABLE PT_CELLTAB.

  ENDLOOP.

ENDFORM.                    " FILL_CELLTAB
*&---------------------------------------------------------------------*
*&      Form  BUILD_TOOLBAR_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_TOOLBAR_GRID .

  DEFINE BTN_EXCLUDING_NEW.
    APPEND &1->MC_FC_&2 TO &3.
  END-OF-DEFINITION.


  BTN_EXCLUDING_NEW : "ALV_GRID reprep            IT_toolbar,
  "ALV_GRID refresh           IT_toolbar,
  ALV_GRID GRAPH             IT_TOOLBAR,
  ALV_GRID LOC_UNDO          IT_TOOLBAR,
  ALV_GRID LOC_DELETE_ROW    IT_TOOLBAR,
  ALV_GRID LOC_INSERT_ROW    IT_TOOLBAR,
  ALV_GRID LOC_COPY_ROW      IT_TOOLBAR,
  ALV_GRID LOC_CUT           IT_TOOLBAR,
  ALV_GRID LOC_APPEND_ROW    IT_TOOLBAR,
  ALV_GRID LOC_PASTE_NEW_ROW IT_TOOLBAR,
  ALV_GRID INFO              IT_TOOLBAR,
  ALV_GRID LOC_COPY          IT_TOOLBAR,
  ALV_GRID LOC_PASTE         IT_TOOLBAR.


ENDFORM.                    " BUILD_TOOLBAR_GRID
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED  USING  ER_DATA_CHANGED TYPE REF TO
                                   CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA LS_MOD_CELL TYPE LVC_S_MODI.
  DATA LV_CHK.

  DATA: LE_ROW     TYPE I,
        LE_COL     TYPE I,
        LE_VALUE   TYPE C,
        LES_ROW_ID TYPE LVC_S_ROW,
        LES_COL_ID TYPE LVC_S_COL,
        LES_ROW_NO TYPE LVC_S_ROID.

* Getting Current Cell
  CALL METHOD ALV_GRID->GET_CURRENT_CELL
    IMPORTING
      E_ROW     = LE_ROW
      E_VALUE   = LE_VALUE
      E_COL     = LE_COL
      ES_ROW_ID = LES_ROW_ID
      ES_COL_ID = LES_COL_ID
      ES_ROW_NO = LES_ROW_NO.

  LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELL.
    CLEAR LV_CHK.
    READ TABLE IT_OUTPUT INDEX LS_MOD_CELL-ROW_ID
    ASSIGNING <F_OUTPUT>.

    CHECK SY-SUBRC EQ 0.
    CASE LS_MOD_CELL-FIELDNAME.
      WHEN 'MATNR'.
        PERFORM DATA_CHANGED_MATNR USING LS_MOD_CELL
                                         ER_DATA_CHANGED.

      WHEN 'DATBI'.
        PERFORM DATA_CHANGED_DATBI USING LS_MOD_CELL
                                         ER_DATA_CHANGED.

      WHEN 'DATAB'.
        PERFORM DATA_CHANGED_DATAB USING LS_MOD_CELL
                                         ER_DATA_CHANGED.

      WHEN 'INT_COLOR'.
        PERFORM DATA_CHANGED_INT_COLOR USING LS_MOD_CELL
                                             ER_DATA_CHANGED.

      WHEN 'EXT_COLOR'.
        PERFORM DATA_CHANGED_EXT_COLOR USING LS_MOD_CELL
                                             ER_DATA_CHANGED.

      WHEN 'MENGE'.
        PERFORM DATA_CHANGED_MENGE USING LS_MOD_CELL
                                         ER_DATA_CHANGED.

    ENDCASE.

    PERFORM ALV_CLASS_REFRESH USING ALV_GRID.

    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
      EXPORTING
        CONTROL = ALV_GRID.

    CALL METHOD ALV_GRID->SET_CURRENT_CELL_VIA_ID
      EXPORTING
        IS_COLUMN_ID = LES_COL_ID
        IS_ROW_ID    = LES_ROW_ID.

    W_COL_ID = LES_COL_ID.
    W_ROW_ID = LES_ROW_ID.

  ENDLOOP.

ENDFORM.                    " HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND  USING    P_UCOMM.

  DATA LV_LINE   TYPE SYINDEX.

  CASE P_UCOMM.
    WHEN 'LINEC'.
      PERFORM INSERT_LINE.

    WHEN 'LINED'.
      PERFORM DELETE_LINE.

  ENDCASE.

  PERFORM ALV_CLASS_REFRESH USING ALV_GRID.

ENDFORM.                    " HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  MODIFY_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MODIFY_CELL  USING    P_FIELD
P_P_MATNR_ROW_ID
P_CHANGE_VALUE
P_PR_DATA_CHANGED
TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  CALL METHOD P_PR_DATA_CHANGED->MODIFY_CELL
    EXPORTING
      I_ROW_ID    = P_P_MATNR_ROW_ID
      I_FIELDNAME = P_FIELD
      I_VALUE     = P_CHANGE_VALUE.

ENDFORM.                    " MODIFY_CELL
*&---------------------------------------------------------------------*
*&      Form  data_input_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DATA_INPUT_ERROR USING ER_DATA_CHANGED TYPE REF TO
CL_ALV_CHANGED_DATA_PROTOCOL
RS_MOD_CELLS TYPE LVC_S_MODI
P_MSGTY
P_MSGV1
P_FIELDNAME.

  CONDENSE P_MSGV1.

* Error Message Display
  CALL METHOD ER_DATA_CHANGED->ADD_PROTOCOL_ENTRY
    EXPORTING
      I_MSGID     = 'ZMPP'
      I_MSGNO     = '000'
      I_MSGTY     = P_MSGTY
      I_MSGV1     = P_MSGV1
      I_MSGV2     = ' '
      I_MSGV3     = ' '
      I_FIELDNAME = P_FIELDNAME
      I_ROW_ID    = RS_MOD_CELLS-ROW_ID.

ENDFORM. " data_input_error
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DATA_CHANGED_MATNR  USING   PS_MOD_CELL TYPE LVC_S_MODI
ER_DATA_CHANGED TYPE REF TO
CL_ALV_CHANGED_DATA_PROTOCOL.
  DATA L_MATNR    TYPE MATNR.
  DATA L_MATNR_I  TYPE MATNR.
  DATA L_MEINS    TYPE MEINS.

  DATA L_MSG(50).

  DATA L_MAKTX    LIKE MAKT-MAKTX.
  DATA L_WERKS    LIKE MARC-WERKS.
  DATA L_DISPO    LIKE MARC-DISPO.
  DATA L_SORTF    LIKE STPO-SORTF.
  DATA L_LGPRO    LIKE MARC-LGPRO.
  DATA L_VSPVB    LIKE MARC-VSPVB.

  L_MATNR_I = PS_MOD_CELL-VALUE.

  "Modify Cell.
  PERFORM MODIFY_CELL USING 'MATNR'
  PS_MOD_CELL-ROW_ID
  L_MATNR_I
  ER_DATA_CHANGED.

  SELECT SINGLE MATNR MEINS
  INTO (L_MATNR, L_MEINS)
  FROM MARA
  WHERE MATNR = L_MATNR_I.

  IF SY-SUBRC NE 0.
    CLEAR L_MSG.
    MESSAGE ID 'ZMPP' TYPE 'E' NUMBER '002'
    INTO L_MSG
    WITH L_MATNR_I TEXT-M03.

    "Error Message Display
    PERFORM DATA_INPUT_ERROR  USING  ER_DATA_CHANGED
    PS_MOD_CELL
    'E'
    L_MSG
    'MATNR'.
    EXIT.

  ELSE.
    SELECT SINGLE MATNR
    INTO L_MATNR
    FROM ZTMM_219_PARTS
    WHERE MATNR = L_MATNR_I
    AND WERKS = ZSMM_219_PARTS-WERKS
    AND MODEL = ZSMM_219_PARTS-MODEL.

    IF SY-SUBRC = 0.
      CLEAR L_MSG.
      MESSAGE ID 'ZMPP' TYPE 'E' NUMBER '002'
      INTO L_MSG
      WITH L_MATNR_I TEXT-M10.

      "Error Message Display
      PERFORM DATA_INPUT_ERROR  USING  ER_DATA_CHANGED
      PS_MOD_CELL
      'E'
      L_MSG
      'MATNR'.
      EXIT.

    ELSE.
      SELECT SINGLE DISPO LGPRO VSPVB
      INTO (L_DISPO,L_LGPRO,L_VSPVB)
      FROM MARC
      WHERE MATNR EQ L_MATNR_I
      AND WERKS EQ ZSMM_219_PARTS-WERKS.

      IF SY-SUBRC NE 0.
        CLEAR L_MSG.
        MESSAGE ID 'ZMPP' TYPE 'E' NUMBER '002'
        INTO L_MSG
        WITH L_MATNR_I TEXT-M05.

        "Error Message Display
        PERFORM DATA_INPUT_ERROR  USING  ER_DATA_CHANGED
        PS_MOD_CELL
        'E'
        L_MSG
        'MATNR'.
        EXIT.

      ELSE.
        SELECT SINGLE MAKTX
        INTO L_MAKTX
        FROM MAKT
        WHERE MATNR EQ L_MATNR_I
        AND SPRAS EQ SY-LANGU.


        SELECT SINGLE USR01
        INTO L_SORTF
        FROM PLPO
        WHERE PLNTY = 'M'
        AND PLNNR = 'RP'
        AND USR00 = L_VSPVB.

        READ TABLE IT_OUTPUT INDEX PS_MOD_CELL-ROW_ID
        ASSIGNING <F_OUTPUT>.

        <F_OUTPUT>-MATNR = L_MATNR_I.
        <F_OUTPUT>-WERKS = ZSMM_219_PARTS-WERKS.
        <F_OUTPUT>-MAKTX = L_MAKTX.
        <F_OUTPUT>-DISPO = L_DISPO.
        <F_OUTPUT>-LGPRO = L_LGPRO.
        <F_OUTPUT>-VSPVB = L_VSPVB.
        <F_OUTPUT>-SORTF = L_SORTF.
        <F_OUTPUT>-MEINS = L_MEINS.


        "Modify Cell.
        PERFORM MODIFY_CELL USING 'MATNR'
        PS_MOD_CELL-ROW_ID
        L_MATNR_I
        ER_DATA_CHANGED.

        PERFORM MODIFY_CELL USING 'WERKS'
        PS_MOD_CELL-ROW_ID
        ZSMM_219_PARTS-WERKS
        ER_DATA_CHANGED.

        PERFORM MODIFY_CELL USING 'MAKTX'
        PS_MOD_CELL-ROW_ID
        L_MAKTX
        ER_DATA_CHANGED.

        PERFORM MODIFY_CELL USING 'DISPO'
        PS_MOD_CELL-ROW_ID
        L_DISPO
        ER_DATA_CHANGED.

        PERFORM MODIFY_CELL USING 'LGPRO'
        PS_MOD_CELL-ROW_ID
        L_LGPRO
        ER_DATA_CHANGED.

        PERFORM MODIFY_CELL USING 'VSPVB'
        PS_MOD_CELL-ROW_ID
        L_VSPVB
        ER_DATA_CHANGED.

        PERFORM MODIFY_CELL USING 'SORTF'
        PS_MOD_CELL-ROW_ID
        L_SORTF
        ER_DATA_CHANGED.

        PERFORM MODIFY_CELL USING 'MEINS'
        PS_MOD_CELL-ROW_ID
        L_MEINS
        ER_DATA_CHANGED.

      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " DATA_CHANGED_MATNR
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_DATBI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DATA_CHANGED_DATBI  USING    PS_MOD_CELL TYPE LVC_S_MODI
ER_DATA_CHANGED TYPE REF TO
CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA L_DATBI        LIKE ZTMM_219_PARTS-DATBI.
  DATA L_MSG(50).

  L_DATBI = PS_MOD_CELL-VALUE.

  "Modify Cell.
  PERFORM MODIFY_CELL USING 'DATBI'
  PS_MOD_CELL-ROW_ID
  L_DATBI
  ER_DATA_CHANGED.

  <F_OUTPUT>-DATBI = L_DATBI.

*  IF <F_OUTPUT>-DATAB IS INITIAL.
*    <F_OUTPUT>-DATBI = L_DATBI.
*
*  ELSE.
*    IF L_DATBI > <F_OUTPUT>-DATAB.
*      CLEAR L_MSG.
*      MESSAGE ID 'ZMPP' TYPE 'E' NUMBER '002'
*         INTO L_MSG
*         WITH TEXT-M04.
*
*      "Error Message Display
*      PERFORM DATA_INPUT_ERROR  USING  ER_DATA_CHANGED
*                                       PS_MOD_CELL
*                                       'E'
*                                       L_MSG
*                                       'DATBI'.
*      EXIT.
*    ENDIF.
*  ENDIF.

  "Modify Cell.
  PERFORM MODIFY_CELL USING 'DATBI'
  PS_MOD_CELL-ROW_ID
  L_DATBI
  ER_DATA_CHANGED.

ENDFORM.                    " DATA_CHANGED_DATBI
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_DATAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DATA_CHANGED_DATAB  USING  PS_MOD_CELL TYPE LVC_S_MODI
                                ER_DATA_CHANGED TYPE REF TO
                                   CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA L_DATAB        LIKE ZTMM_219_PARTS-DATAB.
  DATA L_MSG(50).

  L_DATAB = PS_MOD_CELL-VALUE.

  "Modify Cell.
  PERFORM MODIFY_CELL USING 'DATAB'
  PS_MOD_CELL-ROW_ID
  L_DATAB
  ER_DATA_CHANGED.


  <F_OUTPUT>-DATAB = L_DATAB.

  IF <F_OUTPUT>-DATBI IS NOT INITIAL.
    IF L_DATAB < <F_OUTPUT>-DATBI.
      CLEAR L_MSG.
      MESSAGE ID 'ZMPP' TYPE 'E' NUMBER '002'
      INTO L_MSG
      WITH L_DATAB TEXT-M04.

      "Error Message Display
      PERFORM DATA_INPUT_ERROR  USING  ER_DATA_CHANGED
      PS_MOD_CELL
      'E'
      L_MSG
      'DATAB'.
      EXIT.
    ENDIF.
  ENDIF.

  "Modify Cell.
  PERFORM MODIFY_CELL USING 'DATAB'
  PS_MOD_CELL-ROW_ID
  L_DATAB
  ER_DATA_CHANGED.

ENDFORM.                    " DATA_CHANGED_DATAB
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_EXT_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DATA_CHANGED_EXT_COLOR  USING  PS_MOD_CELL TYPE LVC_S_MODI
                                    ER_DATA_CHANGED TYPE REF TO
                                       CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA L_EXT_COLOR        LIKE ZTMM_219_PARTS-EXT_COLOR.

  L_EXT_COLOR = PS_MOD_CELL-VALUE.

  "Modify Cell.
  PERFORM MODIFY_CELL USING 'EXT_COLOR'
  PS_MOD_CELL-ROW_ID
  L_EXT_COLOR
  ER_DATA_CHANGED.

  READ TABLE IT_OUTPUT INDEX PS_MOD_CELL-ROW_ID
  ASSIGNING <F_OUTPUT>.

  <F_OUTPUT>-EXT_COLOR = L_EXT_COLOR.

  "Modify Cell.
  PERFORM MODIFY_CELL USING 'EXT_COLOR'
  PS_MOD_CELL-ROW_ID
  L_EXT_COLOR
  ER_DATA_CHANGED.

ENDFORM.                    " DATA_CHANGED_EXT_COLOR
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_INT_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DATA_CHANGED_INT_COLOR  USING  PS_MOD_CELL TYPE LVC_S_MODI
                                    ER_DATA_CHANGED TYPE REF TO
                                      CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA L_INT_COLOR        LIKE ZTMM_219_PARTS-INT_COLOR.

  L_INT_COLOR = PS_MOD_CELL-VALUE.

  "Modify Cell.
  PERFORM MODIFY_CELL USING 'INT_COLOR'
  PS_MOD_CELL-ROW_ID
  L_INT_COLOR
  ER_DATA_CHANGED.

  READ TABLE IT_OUTPUT INDEX PS_MOD_CELL-ROW_ID
  ASSIGNING <F_OUTPUT>.

  <F_OUTPUT>-INT_COLOR = L_INT_COLOR.

  "Modify Cell.
  PERFORM MODIFY_CELL USING 'INT_COLOR'
  PS_MOD_CELL-ROW_ID
  L_INT_COLOR
  ER_DATA_CHANGED.

ENDFORM.                    " DATA_CHANGED_INT_COLOR
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_MENGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DATA_CHANGED_MENGE  USING    PS_MOD_CELL TYPE LVC_S_MODI
ER_DATA_CHANGED TYPE REF TO
CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA L_MENGE LIKE ZTMM_219_PARTS-USG_MENGE.

  L_MENGE = PS_MOD_CELL-VALUE.

  "Modify Cell.
  PERFORM MODIFY_CELL USING 'MENGE'
  PS_MOD_CELL-ROW_ID
  L_MENGE
  ER_DATA_CHANGED.

  READ TABLE IT_OUTPUT INDEX PS_MOD_CELL-ROW_ID
  ASSIGNING <F_OUTPUT>.

  <F_OUTPUT>-MENGE = L_MENGE.

  "Modify Cell.
  PERFORM MODIFY_CELL USING 'MENGE'
  PS_MOD_CELL-ROW_ID
  L_MENGE
  ER_DATA_CHANGED.

ENDFORM.                    " DATA_CHANGED_MENGE
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_DATA INPUT.

  IF IT_OUTPUT[] IS NOT INITIAL AND ( IS_219_PARTS <> ZSMM_219_PARTS ).
    CLEAR W_ANSWER.
    PERFORM CALL_POPUP_CONFIRM USING C_YES
    TEXT-T03 " TITLE
    TEXT-T04
    ' '
    CHANGING W_ANSWER.
    IF W_ANSWER = C_YES.
      CLEAR: IT_OUTPUT, IT_OUTPUT[].

    ELSE.
      ZSMM_219_PARTS = IS_219_PARTS.

    ENDIF.

  ENDIF.

ENDMODULE.                 " CHECK_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Form  F4_CLNO01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F4_CLNO01  USING    P_FIELDNAME.

  DATA DYNPFIELD    LIKE DYNPREAD OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF LT_F4 OCCURS 0,
          CLNO LIKE ZTBM_ABXOPVDT-CLNO,
          VANM LIKE ZTBM_ABXOPVDT-VANM.
  DATA: END OF LT_F4.

  DEFINE DYNPRO_VALUES_READ.

    CLEAR: DYNPFIELD, DYNPFIELD[].
    DYNPFIELD-FIELDNAME = &1.
    APPEND DYNPFIELD.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        DYNAME               = SY-CPROG
        DYNUMB               = SY-DYNNR
      TABLES
        DYNPFIELDS           = DYNPFIELD
      EXCEPTIONS
        INVALID_ABAPWORKAREA = 1
        INVALID_DYNPROFIELD  = 2
        INVALID_DYNPRONAME   = 3
        INVALID_DYNPRONUMMER = 4
        INVALID_REQUEST      = 5
        NO_FIELDDESCRIPTION  = 6
        INVALID_PARAMETER    = 7
        UNDEFIND_ERROR       = 8
        DOUBLE_CONVERSION    = 9
        STEPL_NOT_FOUND      = 10
        OTHERS               = 11.

    IF SY-SUBRC = 0.
      READ TABLE DYNPFIELD WITH KEY FIELDNAME = &1.
      IF SY-SUBRC = 0.
        &2 = DYNPFIELD-FIELDVALUE.
      ENDIF.
    ENDIF.

  END-OF-DEFINITION.
  DYNPRO_VALUES_READ 'ZSMM_219_PARTS-MODEL' ZSMM_219_PARTS-MODEL.

  IF ZSMM_219_PARTS-MODEL IS NOT INITIAL.
    CLEAR: LT_F4, LT_F4[].
    SELECT CLNO VANM
      INTO CORRESPONDING FIELDS OF TABLE LT_F4
      FROM ZTBM_ABXOPVDT
     WHERE CARX EQ ZSMM_219_PARTS-MODEL+0(2).

    IF SY-SUBRC = 0.
      SORT LT_F4 BY CLNO.
      DELETE ADJACENT DUPLICATES FROM LT_F4 COMPARING CLNO.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          RETFIELD        = 'CLNO'
          DYNPPROG        = 'ZRPP_219_MATERIAL_MASS'
          DYNPNR          = '0100'
          DYNPROFIELD     = P_FIELDNAME
          VALUE_ORG       = 'S'
        TABLES
          VALUE_TAB       = LT_F4
        EXCEPTIONS
          PARAMETER_ERROR = 1
          NO_VALUES_FOUND = 2
          OTHERS          = 3.
    ENDIF.
  ENDIF.

ENDFORM.                    " F4_CLNO01
*&---------------------------------------------------------------------*
*&      Form  F4_VALU01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F4_VALU01  USING    P_FIELDNAME
                         P_DYNPFIELD
                         P_VALUE.

  DATA DYNPFIELD    LIKE DYNPREAD OCCURS 0 WITH HEADER LINE.

  RANGES: LR_CLNO   FOR ZTBM_ABXOPVDT-CLNO.

  DATA: BEGIN OF LT_F4 OCCURS 0,
          VALU LIKE ZTBM_ABXOPVDT-VALU,
          CLNM LIKE ZTBM_ABXOPVDT-CLNM.
  DATA: END OF LT_F4.

  DEFINE DYNPRO_VALUES_READ.

    CLEAR: DYNPFIELD, DYNPFIELD[].
    DYNPFIELD-FIELDNAME = &1.
    APPEND DYNPFIELD.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        DYNAME               = SY-CPROG
        DYNUMB               = SY-DYNNR
      TABLES
        DYNPFIELDS           = DYNPFIELD
      EXCEPTIONS
        INVALID_ABAPWORKAREA = 1
        INVALID_DYNPROFIELD  = 2
        INVALID_DYNPRONAME   = 3
        INVALID_DYNPRONUMMER = 4
        INVALID_REQUEST      = 5
        NO_FIELDDESCRIPTION  = 6
        INVALID_PARAMETER    = 7
        UNDEFIND_ERROR       = 8
        DOUBLE_CONVERSION    = 9
        STEPL_NOT_FOUND      = 10
        OTHERS               = 11.

    IF SY-SUBRC = 0.
      READ TABLE DYNPFIELD WITH KEY FIELDNAME = &1.
      IF SY-SUBRC = 0.
        &2 = DYNPFIELD-FIELDVALUE.
      ENDIF.
    ENDIF.

  END-OF-DEFINITION.

  IF ZSMM_219_PARTS-MODEL IS NOT INITIAL.
    CLEAR: LT_F4, LT_F4[].

    DYNPRO_VALUES_READ P_DYNPFIELD P_VALUE.

    IF P_VALUE IS NOT INITIAL.
      CLEAR: LR_CLNO, LR_CLNO.
      LR_CLNO-SIGN   = 'I'.
      LR_CLNO-OPTION = 'EQ'.
      LR_CLNO-LOW    = P_VALUE.
      LR_CLNO-HIGH   = ''.
      APPEND LR_CLNO.
    ENDIF.

    SELECT VALU CLNM
      INTO CORRESPONDING FIELDS OF TABLE LT_F4
      FROM ZTBM_ABXOPVDT
     WHERE CARX EQ ZSMM_219_PARTS-MODEL+0(2)
       AND CLNO IN LR_CLNO.

    IF SY-SUBRC = 0.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          RETFIELD        = 'VALU'
          DYNPPROG        = 'ZRPP_219_MATERIAL_MASS'
          DYNPNR          = '0100'
          DYNPROFIELD     = P_FIELDNAME
          VALUE_ORG       = 'S'
        TABLES
          VALUE_TAB       = LT_F4
        EXCEPTIONS
          PARAMETER_ERROR = 1
          NO_VALUES_FOUND = 2
          OTHERS          = 3.
    ENDIF.
  ENDIF.

ENDFORM.                    " F4_VALU01
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO01  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO01 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO01'.

ENDMODULE.                 " F4_CLNO01  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU01  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU01 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU01'
                          'ZSMM_219_PARTS-CLNO01'
                          ZSMM_219_PARTS-CLNO01.

ENDMODULE.                 " F4_VALU01  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO02  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO02 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO02'.

ENDMODULE.                 " F4_CLNO02  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU02  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU02 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU02'
                          'ZSMM_219_PARTS-CLNO02'
                          ZSMM_219_PARTS-CLNO02.

ENDMODULE.                 " F4_VALU02  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO03  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO03 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO03'.

ENDMODULE.                 " F4_CLNO03  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU03  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU03 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU03'
                          'ZSMM_219_PARTS-CLNO03'
                          ZSMM_219_PARTS-CLNO03.

ENDMODULE.                 " F4_VALU03  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO04  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO04 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO04'.

ENDMODULE.                 " F4_CLNO04  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU04  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU04 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU04'
                          'ZSMM_219_PARTS-CLNO04'
                          ZSMM_219_PARTS-CLNO04.

ENDMODULE.                 " F4_VALU04  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO05  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO05 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO05'.

ENDMODULE.                 " F4_CLNO05  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU05  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU05 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU05'
                          'ZSMM_219_PARTS-CLNO05'
                          ZSMM_219_PARTS-CLNO05.

ENDMODULE.                 " F4_VALU05  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO06  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO06 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO06'.

ENDMODULE.                 " F4_CLNO06  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU06  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU06 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU06'
                          'ZSMM_219_PARTS-CLNO06'
                          ZSMM_219_PARTS-CLNO06.

ENDMODULE.                 " F4_VALU06  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO07  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO07 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO07'.

ENDMODULE.                 " F4_CLNO07  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU07  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU07 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU07'
                          'ZSMM_219_PARTS-CLNO07'
                          ZSMM_219_PARTS-CLNO07.

ENDMODULE.                 " F4_VALU07  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO08  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO08 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO08'.

ENDMODULE.                 " F4_CLNO08  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU08  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU08 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU08'
                          'ZSMM_219_PARTS-CLNO08'
                          ZSMM_219_PARTS-CLNO08.

ENDMODULE.                 " F4_VALU08  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO09  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO09 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO09'.

ENDMODULE.                 " F4_CLNO09  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU09  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU09 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU09'
                          'ZSMM_219_PARTS-CLNO09'
                          ZSMM_219_PARTS-CLNO09.

ENDMODULE.                 " F4_VALU09  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO10  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO10 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO10'.

ENDMODULE.                 " F4_CLNO10  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU10  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU10 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU10'
                          'ZSMM_219_PARTS-CLNO10'
                          ZSMM_219_PARTS-CLNO10.

ENDMODULE.                 " F4_VALU10  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO11  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO11 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO11'.

ENDMODULE.                 " F4_CLNO11  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU11  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU11 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU11'
                          'ZSMM_219_PARTS-CLNO11'
                          ZSMM_219_PARTS-CLNO11.

ENDMODULE.                 " F4_VALU11  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO12  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO12 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO12'.

ENDMODULE.                 " F4_CLNO12  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU12  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU12 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU12'
                          'ZSMM_219_PARTS-CLNO12'
                          ZSMM_219_PARTS-CLNO12.

ENDMODULE.                 " F4_VALU12  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO13  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO13 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO13'.

ENDMODULE.                 " F4_CLNO13  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU13  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU13 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU13'
                          'ZSMM_219_PARTS-CLNO13'
                          ZSMM_219_PARTS-CLNO13.

ENDMODULE.                 " F4_VALU13  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO14  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO14 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO14'.

ENDMODULE.                 " F4_CLNO14  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU14  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU14 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU14'
                          'ZSMM_219_PARTS-CLNO14'
                          ZSMM_219_PARTS-CLNO14.

ENDMODULE.                 " F4_VALU14  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO15  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO15 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO15'.

ENDMODULE.                 " F4_CLNO15  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU15  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU15 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU15'
                          'ZSMM_219_PARTS-CLNO15'
                          ZSMM_219_PARTS-CLNO15.

ENDMODULE.                 " F4_VALU15  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CLNO16  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_CLNO16 INPUT.

  PERFORM F4_CLNO01 USING 'ZSMM_219_PARTS-CLNO16'.

ENDMODULE.                 " F4_CLNO16  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VALU16  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VALU16 INPUT.

  PERFORM F4_VALU01 USING 'ZSMM_219_PARTS-VALU16'
                          'ZSMM_219_PARTS-CLNO16'
                          ZSMM_219_PARTS-CLNO16.

ENDMODULE.                 " F4_VALU16  INPUT
