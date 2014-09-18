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

TYPE-POOLS: SLIS, VRM.
TABLES : ZTMM_219_PARTS, MARC, MARA, STPO.

*----------------------------------------------------------------------*
*                     INTENAL TABLE DECLARATION                        *
*----------------------------------------------------------------------*
DATA : IT_DATA LIKE ZTMM_219_PARTS OCCURS 0 WITH HEADER LINE.
*DATA : BEGIN OF IT_OUTPUT OCCURS 0,
*       LIGHTS,
*       MATNR LIKE ZTMM_219_PARTS-MATNR,
*       MAKTX LIKE MAKT-MAKTX,
*       WERKS LIKE ZTMM_219_PARTS-WERKS,
*       DISPO LIKE MARC-DISPO,
*       SORTF LIKE STPO-SORTF,
*       LGPRO LIKE MARC-LGPRO,
*       VSPVB LIKE MARC-VSPVB,
*       DATBI LIKE ZTMM_219_PARTS-DATBI,
*       DATAB LIKE ZTMM_219_PARTS-DATAB,
*       EXT_COLOR  LIKE ZTMM_219_PARTS-EXT_COLOR,
*       INT_COLOR  LIKE ZTMM_219_PARTS-INT_COLOR,
*       USG_MENGE  LIKE ZTMM_219_PARTS-USG_MENGE,
*       MEINS LIKE ZTMM_219_PARTS-MEINS,
*       219(100),
*       END OF IT_OUTPUT.

DATA: BEGIN OF IT_OUTPUT OCCURS 0.
        INCLUDE STRUCTURE ZTMM_219_PARTS.
DATA: LIGHTS,
     MAKTX LIKE MAKT-MAKTX,
     DISPO LIKE MARC-DISPO,
*     SORTF LIKE STPO-SORTF,
     LGPRO LIKE MARC-LGPRO,
     VSPVB LIKE MARC-VSPVB,
     MENGE(10),
     219(100).
DATA: END OF IT_OUTPUT.
DATA: G_LIGHTS_FIELDNAME  TYPE SLIS_FIELDNAME VALUE 'LIGHTS'.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDNAME.  "IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: OK_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_DYNNR LIKE SY-DYNNR,
      W_CNT   TYPE I.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_WERKS LIKE ZTMM_219_PARTS-WERKS DEFAULT 'P001' OBLIGATORY.

SELECT-OPTIONS: S_MATNR FOR ZTMM_219_PARTS-MATNR.
PARAMETERS: P_MODEL LIKE ZTMM_219_PARTS-MODEL.

SELECT-OPTIONS: S_DISPO FOR MARC-DISPO,
                S_MTART FOR MARA-MTART,
                S_VSPVB FOR MARC-VSPVB,
                S_SORTF FOR STPO-SORTF.

PARAMETERS: P_EXTC LIKE ZTMM_219_PARTS-EXT_COLOR,
            P_INTC LIKE ZTMM_219_PARTS-INT_COLOR.

SELECTION-SCREEN SKIP.
*SELECTION-SCREEN uline.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(8) TEXT-P01.
SELECTION-SCREEN POSITION 15.
PARAMETERS: CLNO01 LIKE ZSMM_219_PARTS-CLNO01.
SELECTION-SCREEN POSITION 20.
PARAMETERS: VALU01 LIKE ZSMM_219_PARTS-VALU01.
SELECTION-SCREEN POSITION 30.
PARAMETERS: CLNO02 LIKE ZSMM_219_PARTS-CLNO02.
SELECTION-SCREEN POSITION 35.
PARAMETERS: VALU02 LIKE ZSMM_219_PARTS-VALU02.

SELECTION-SCREEN POSITION 45.
PARAMETERS: CLNO03 LIKE ZSMM_219_PARTS-CLNO03.
SELECTION-SCREEN POSITION 50.
PARAMETERS: VALU03 LIKE ZSMM_219_PARTS-VALU03.
SELECTION-SCREEN POSITION 60.
PARAMETERS: CLNO04 LIKE ZSMM_219_PARTS-CLNO04.
SELECTION-SCREEN POSITION 65.
PARAMETERS: VALU04 LIKE ZSMM_219_PARTS-VALU04.
SELECTION-SCREEN POSITION 74.
PARAMETERS: CLNO05 LIKE ZSMM_219_PARTS-CLNO05.
SELECTION-SCREEN POSITION 79.
PARAMETERS: VALU05 LIKE ZSMM_219_PARTS-VALU05.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B1.

*SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
*PARAMETERS: P_219 RADIOBUTTON GROUP GRP1 DEFAULT 'X',
*            P_MAT RADIOBUTTON GROUP GRP1 no-display.
*SELECTION-SCREEN END OF BLOCK B2.

AT SELECTION-SCREEN OUTPUT.
*  PERFORM MODIFY_SCREEN_ALL.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DISPO-LOW.
  PERFORM REQUEST_DISPO.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DISPO-HIGH.
  PERFORM REQUEST_DISPO.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_MODEL.
  PERFORM REQUEST_MODEL.

START-OF-SELECTION.

  PERFORM GET_DATA.
  IF IT_OUTPUT[] IS INITIAL.
    MESSAGE I001 WITH 'No data found'.
  ELSE.
    PERFORM DISPLAY_DATA.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  DATA: L_INDEX LIKE SY-TABIX,
        L_CN(2) TYPE N,
        L_TEXT(40),
        L_VAL_TEXT(40),
        L_COLUMN(3) TYPE N,
        L_VALUE(1),
        L_SIGN(1),
        L_LENGTH TYPE I,
        L_CHECK1(1),
        L_CHECK2(1),
        L_CHECK3(1),
        L_CHECK4(1),
        L_CHECK5(1),
        L_MATCH_RESULT TYPE MATCH_RESULT,
                 L_WILDC_EXT(1),
                          L_WILDC_INT(1).

*  DATA: LT_TEMP LIKE  TABLE OF IT_OUTPUT WITH HEADER LINE.
*                S_SORTF FOR STPO-SORTF.
  FIELD-SYMBOLS: <FS_CLN>, <FS_VAL>.

  PERFORM START_PROGRESSBAR USING
          'Getting Material Master dataoff date' '50'.

*  CASE P_219.
** from 219
*    WHEN 'X'.
  IF P_MODEL IS INITIAL.
    SELECT A~MATNR MAKTX A~WERKS MODEL DISPO LGPRO
           VSPVB DATBI A~DATAB EXT_COLOR INT_COLOR
           USG_MENGE A~MEINS CLNO01 VALU01 CLNO02 VALU02
               CLNO03 VALU03 CLNO04 VALU04 CLNO05 VALU05
               CLNO06 VALU06 CLNO07 VALU07 CLNO08 VALU08
               CLNO09 VALU09 CLNO10 VALU10 CLNO11 VALU11
               CLNO12 VALU12 CLNO13 VALU13 CLNO14 VALU14
               CLNO15 VALU15 CLNO16 VALU16
      INTO CORRESPONDING FIELDS OF TABLE IT_OUTPUT
      FROM ZTMM_219_PARTS AS A
      INNER JOIN MARA AS B
      ON A~MATNR = B~MATNR
      INNER JOIN MARC AS C
      ON B~MATNR = C~MATNR
      AND A~WERKS = C~WERKS
      INNER JOIN MAKT AS D
      ON A~MATNR = D~MATNR
*      INNER JOIN STPO AS E
*      ON A~MATNR = E~IDNRK
*      INNER JOIN MAST AS F
*      ON E~STLNR = F~STLNR
      WHERE A~MATNR IN S_MATNR
        AND A~WERKS = P_WERKS
        AND MTART IN S_MTART
        AND DISPO IN S_DISPO
        AND VSPVB IN S_VSPVB
        AND SORTF IN S_SORTF
*        AND F~STLAN = '1'
        AND SPRAS = 'E'.
  ELSE.
    SELECT A~MATNR MAKTX A~WERKS MODEL DISPO LGPRO
               VSPVB DATBI A~DATAB EXT_COLOR INT_COLOR
               USG_MENGE A~MEINS CLNO01 VALU01 CLNO02 VALU02
               CLNO03 VALU03 CLNO04 VALU04 CLNO05 VALU05
               CLNO06 VALU06 CLNO07 VALU07 CLNO08 VALU08
               CLNO09 VALU09 CLNO10 VALU10 CLNO11 VALU11
               CLNO12 VALU12 CLNO13 VALU13 CLNO14 VALU14
               CLNO15 VALU15 CLNO16 VALU16
          INTO CORRESPONDING FIELDS OF TABLE IT_OUTPUT
          FROM ZTMM_219_PARTS AS A
          INNER JOIN MARA AS B
          ON A~MATNR = B~MATNR
          INNER JOIN MARC AS C
          ON B~MATNR = C~MATNR
          AND A~WERKS = C~WERKS
          INNER JOIN MAKT AS D
            ON B~MATNR = D~MATNR
*          INNER JOIN STPO AS E
*            ON A~MATNR = E~IDNRK
*          INNER JOIN MAST AS F
*            ON E~STLNR = F~STLNR
       WHERE A~MATNR IN S_MATNR
       AND A~WERKS =  P_WERKS
          AND MODEL = P_MODEL
       AND MTART IN S_MTART
       AND DISPO IN S_DISPO
       AND VSPVB IN S_VSPVB
       AND SORTF IN S_SORTF
*        AND F~STLAN = '1'
       AND SPRAS = 'E'.
  ENDIF.

  SORT IT_OUTPUT BY MATNR WERKS MODEL.
  DELETE ADJACENT DUPLICATES FROM IT_OUTPUT
    COMPARING  MATNR WERKS MODEL.


** check color
  IF P_EXTC IS INITIAL AND
     P_INTC IS INITIAL.
  ELSE.
    IF NOT P_EXTC IS INITIAL.
      FIND '*' IN P_EXTC RESULTS L_MATCH_RESULT.
      IF SY-SUBRC = 0.
        L_WILDC_EXT = 'X'.
      ELSE.
        CLEAR: L_WILDC_EXT.
      ENDIF.
    ENDIF.
    IF NOT P_INTC IS INITIAL.
      FIND '*' IN P_INTC RESULTS L_MATCH_RESULT.
      IF SY-SUBRC = 0.
        L_WILDC_INT = 'X'.
      ELSE.
        CLEAR: L_WILDC_INT.
      ENDIF.
    ENDIF.

    LOOP AT IT_OUTPUT.
      L_INDEX = SY-TABIX.
      IF NOT P_EXTC IS INITIAL.
        IF L_WILDC_EXT = 'X'.
          IF IT_OUTPUT-EXT_COLOR CP P_EXTC.
          ELSE.
            DELETE IT_OUTPUT INDEX L_INDEX.
            CONTINUE.
          ENDIF.
        ELSEIF  IT_OUTPUT-EXT_COLOR <> P_EXTC.
          DELETE IT_OUTPUT INDEX L_INDEX.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF NOT P_INTC IS INITIAL.
        IF L_WILDC_INT = 'X'.
          IF IT_OUTPUT-INT_COLOR CP P_INTC.
          ELSE.
            DELETE IT_OUTPUT INDEX L_INDEX.
            CONTINUE.
          ENDIF.
        ELSEIF  IT_OUTPUT-INT_COLOR <> P_INTC.
          DELETE IT_OUTPUT INDEX L_INDEX.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

** 219 check
  IF CLNO01 IS INITIAL AND
     CLNO02 IS INITIAL AND
     CLNO03 IS INITIAL AND
     CLNO04 IS INITIAL AND
     CLNO05 IS INITIAL.
  ELSE.

    LOOP AT IT_OUTPUT.
      L_INDEX = SY-TABIX.
      CLEAR: L_CHECK1,L_CHECK2,L_CHECK3,L_CHECK4,L_CHECK5.

      IF NOT CLNO01 IS INITIAL.
        L_CN = '01'.
        WHILE L_CN <= 16.
          CONCATENATE 'IT_OUTPUT-' 'CLNO' L_CN INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS_CLN>.
          L_COLUMN = <FS_CLN>.
          IF L_COLUMN = CLNO01.
            CONCATENATE 'IT_OUTPUT-' 'VALU' L_CN INTO L_VAL_TEXT.
            ASSIGN (L_VAL_TEXT) TO <FS_VAL>.
            L_VALUE = <FS_VAL>.
            IF VALU01 IS INITIAL OR
               VALU01 = L_VALUE.
              L_CHECK1 = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
          L_CN = L_CN + 1.
        ENDWHILE.
        IF L_CHECK1 IS INITIAL.
          DELETE IT_OUTPUT INDEX L_INDEX.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF NOT CLNO02 IS INITIAL.
        L_CN = '01'.
        WHILE L_CN <= 16.
          CONCATENATE 'IT_OUTPUT-' 'CLNO' L_CN INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS_CLN>.
          L_COLUMN = <FS_CLN>.
          IF L_COLUMN = CLNO02.
            CONCATENATE 'IT_OUTPUT-' 'VALU' L_CN INTO L_VAL_TEXT.
            ASSIGN (L_VAL_TEXT) TO <FS_VAL>.
            L_VALUE = <FS_VAL>.
            IF VALU02 IS INITIAL OR
               VALU02 = L_VALUE.
              L_CHECK2 = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
          L_CN = L_CN + 1.
        ENDWHILE.
        IF L_CHECK2 IS INITIAL.
          DELETE IT_OUTPUT INDEX L_INDEX.
          CONTINUE.
        ENDIF.

      ENDIF.

      IF NOT CLNO03 IS INITIAL.
        L_CN = '01'.
        WHILE L_CN <= 16.
          CONCATENATE 'IT_OUTPUT-' 'CLNO' L_CN INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS_CLN>.
          L_COLUMN = <FS_CLN>.
          IF L_COLUMN = CLNO03.
            CONCATENATE 'IT_OUTPUT-' 'VALU' L_CN INTO L_VAL_TEXT.
            ASSIGN (L_VAL_TEXT) TO <FS_VAL>.
            L_VALUE = <FS_VAL>.
            IF VALU03 IS INITIAL OR
               VALU03 = L_VALUE.
              L_CHECK3 = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
          L_CN = L_CN + 1.
        ENDWHILE.
        IF L_CHECK3 IS INITIAL.
          DELETE IT_OUTPUT INDEX L_INDEX.
          CONTINUE.
        ENDIF.

      ENDIF.

      IF NOT CLNO04 IS INITIAL.
        L_CN = '01'.
        WHILE L_CN <= 16.
          CONCATENATE 'IT_OUTPUT-' 'CLNO' L_CN INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS_CLN>.
          L_COLUMN = <FS_CLN>.
          IF L_COLUMN = CLNO04.
            CONCATENATE 'IT_OUTPUT-' 'VALU' L_CN INTO L_VAL_TEXT.
            ASSIGN (L_VAL_TEXT) TO <FS_VAL>.
            L_VALUE = <FS_VAL>.
            IF VALU04 IS INITIAL OR
               VALU04 = L_VALUE.
              L_CHECK4 = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
          L_CN = L_CN + 1.
        ENDWHILE.
        IF L_CHECK4 IS INITIAL.
          DELETE IT_OUTPUT INDEX L_INDEX.
          CONTINUE.
        ENDIF.

      ENDIF.

      IF NOT CLNO05 IS INITIAL.
        L_CN = '01'.
        WHILE L_CN <= 16.
          CONCATENATE 'IT_OUTPUT-' 'CLNO' L_CN INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS_CLN>.
          L_COLUMN = <FS_CLN>.
          IF L_COLUMN = CLNO05.
            CONCATENATE 'IT_OUTPUT-' 'VALU' L_CN INTO L_VAL_TEXT.
            ASSIGN (L_VAL_TEXT) TO <FS_VAL>.
            L_VALUE = <FS_VAL>.
            IF VALU05 IS INITIAL OR
               VALU05 = L_VALUE.
              L_CHECK5 = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
          L_CN = L_CN + 1.
        ENDWHILE.
        IF L_CHECK5 IS INITIAL.
          DELETE IT_OUTPUT INDEX L_INDEX.
          CONTINUE.
        ENDIF.
      ENDIF.

    ENDLOOP.
*  REFRESH IT_OUTPUT.
*  IT_OUTPUT[] = LT_TEMP[].
*  REFRESH LT_TEMP[].
  ENDIF.

  PERFORM START_PROGRESSBAR USING
          'Processing the data' '50'.

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
    IF IT_OUTPUT-219 IS INITIAL.
      IT_OUTPUT-LIGHTS = '1'.
    ELSE.
      L_LENGTH = STRLEN( IT_OUTPUT-219 ).
      L_LENGTH = L_LENGTH - 1.
      IT_OUTPUT-219 = IT_OUTPUT-219+0(L_LENGTH).
      IT_OUTPUT-LIGHTS = '3'.
    ENDIF.

*    SELECT SINGLE SORTF INTO IT_OUTPUT-SORTF
*     FROM STPO
*     WHERE IDNRK = IT_OUTPUT-MATNR.

      SELECT SINGLE USR01 INTO IT_OUTPUT-SORTF
      FROM PLPO
      WHERE PLNTY = 'M'
        AND PLNNR = 'RP'
        AND USR00 = IT_OUTPUT-VSPVB.

    IT_OUTPUT-MENGE = IT_OUTPUT-USG_MENGE.
    MODIFY IT_OUTPUT INDEX L_INDEX.
    CLEAR: IT_OUTPUT.
  ENDLOOP.

** from material amster
*    WHEN ' '.
*      SELECT A~MATNR WERKS LGPRO DISPO VSPVB
*        INTO CORRESPONDING FIELDS OF TABLE IT_OUTPUT
*        FROM MARA AS A
*        INNER JOIN MARC AS B
*        ON A~MATNR = B~MATNR
*        WHERE A~MATNR IN S_MATNR
*         AND MTART IN S_MTART
*         AND WERKS =  P_WERKS
*         AND DISPO IN S_DISPO.
*
*      IF SY-SUBRC = 0.
*        PERFORM START_PROGRESSBAR USING
*              'Getting Material Master dataoff date' '50'.
*
*        IF P_MODEL IS INITIAL.
*          SELECT * INTO TABLE IT_DATA
*            FROM ZTMM_219_PARTS
*            FOR ALL ENTRIES IN IT_OUTPUT
*           WHERE MATNR = IT_OUTPUT-MATNR
*            AND WERKS =  IT_OUTPUT-WERKS.
*        ELSE.
*          SELECT * INTO TABLE IT_DATA
*          FROM ZTMM_219_PARTS
*          FOR ALL ENTRIES IN IT_OUTPUT
*         WHERE MATNR = IT_OUTPUT-MATNR
*          AND WERKS =  IT_OUTPUT-WERKS
*          AND MODEL = P_MODEL.
*        ENDIF.
*
*        PERFORM START_PROGRESSBAR USING
*                'Processing the data' '50'.
*
*        SORT IT_DATA BY MATNR WERKS.
*        LOOP AT IT_OUTPUT.
*
*          L_INDEX = SY-TABIX.
*          READ TABLE IT_DATA WITH KEY MATNR = IT_OUTPUT-MATNR
*                                      WERKS = IT_OUTPUT-WERKS
*                                      BINARY SEARCH.
*          IF SY-SUBRC = 0.
*            L_CN = '01'.
*            L_SIGN = ','.
*            WHILE L_CN <= 16.
*              CONCATENATE 'IT_DATA-' 'CLNO' L_CN INTO L_TEXT.
*              ASSIGN (L_TEXT) TO <FS_CLN>.
*              L_COLUMN = <FS_CLN>.
*              IF L_COLUMN <> ' '.
*                CONCATENATE 'IT_DATA-' 'VALU' L_CN INTO L_TEXT.
*                ASSIGN (L_TEXT) TO <FS_VAL>.
*                L_VALUE = <FS_VAL>.
*                CONCATENATE IT_OUTPUT-219 L_COLUMN L_VALUE L_SIGN
*                INTO IT_OUTPUT-219.
*              ENDIF.
*              L_CN = L_CN + 1.
*              CLEAR: L_COLUMN, L_VALUE.
*            ENDWHILE.
*            IT_OUTPUT-MODEL = IT_DATA-MODEL.
*            IT_OUTPUT-EXT_COLOR = IT_DATA-EXT_COLOR.
*            IT_OUTPUT-INT_COLOR = IT_DATA-INT_COLOR.
*            IT_OUTPUT-USG_MENGE = IT_DATA-USG_MENGE.
*            IT_OUTPUT-MENGE =  IT_OUTPUT-USG_MENGE.
*            IT_OUTPUT-MEINS = IT_DATA-MEINS.
*            L_LENGTH = STRLEN( IT_OUTPUT-219 ).
*            L_LENGTH = L_LENGTH - 1.
*            IT_OUTPUT-219 = IT_OUTPUT-219+0(L_LENGTH).
*            IT_OUTPUT-DATBI = IT_DATA-DATBI.
*            IT_OUTPUT-DATAB = IT_DATA-DATAB.
*            IT_OUTPUT-LIGHTS = '3'.
*          ELSE.
*            IT_OUTPUT-LIGHTS = '1'.
*          ENDIF.
*          SELECT SINGLE MAKTX INTO IT_OUTPUT-MAKTX
*            FROM MAKT
*           WHERE MATNR = IT_OUTPUT-MATNR
*             AND SPRAS = 'E'.
*
*          SELECT SINGLE SORTF INTO IT_OUTPUT-SORTF
*           FROM STPO
*           WHERE IDNRK = IT_OUTPUT-MATNR.
*
*          MODIFY IT_OUTPUT INDEX L_INDEX.
*          CLEAR: IT_OUTPUT.
*        ENDLOOP.
*      ENDIF.
*  ENDCASE.
ENDFORM.                    " get_data



*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN_ALL.

*  LOOP AT SCREEN.
*    IF P_BATCH = 'X' AND SCREEN-GROUP1 EQ 'ABC'.
*      SCREEN-INVISIBLE = 1.
*      SCREEN-ACTIVE    = 0.
*      SCREEN-INPUT     = 0.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN_ALL
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  CALL SCREEN 800.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0800 OUTPUT.
  SET PF-STATUS 'ST800'.
  SET TITLEBAR 'T800'.

ENDMODULE.                 " STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_OUTPUT'.
    PERFORM ASSIGN_ITAB_TO_ALV.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-INFO_FNAME = 'IF'.
  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
  WA_IS_LAYOUT-EXCP_FNAME = 'LIGHTS'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

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

  IT_SORT-SPOS           = 1.
  IT_SORT-FIELDNAME      = 'MODEL'.
  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
  APPEND IT_SORT.

  IT_SORT-SPOS           = 2.
  IT_SORT-FIELDNAME      = 'MATNR'.
  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
  APPEND IT_SORT.

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

                                 'S' 'MODEL'       ' ',
*                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Model',
                                  ' ' 'LZERO'       ' ',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'MATNR'       ' ',
*                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  ' ' 'LZERO'       ' ',
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
                                  ' ' 'COLTEXT'     'Valid from',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'DATAB'       ' ',
                                  ' ' 'COLTEXT'     'Valid to',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'EXT_COLOR'       ' ',
                                  ' ' 'COLTEXT'     'Ext Col',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'INT_COLOR'       ' ',
                                  ' ' 'COLTEXT'     'Int Col',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'MENGE'       ' ',
                                  ' ' 'COLTEXT'     'Usage',
                                  ' ' 'LZERO'       ' ',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'MEINS'       ' ',
                                 ' ' 'COLTEXT'     'UoM',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' '219'       ' ',
                                  ' ' 'COLTEXT'     '219 Code',
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
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = WA_IS_LAYOUT
      I_SAVE               = WA_SAVE
      IS_VARIANT           = WA_VARIANT
      I_DEFAULT            = SPACE
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      IT_FIELDCATALOG      = IT_FIELDCAT[]
      IT_OUTTAB            = IT_OUTPUT[]
      IT_SORT              = IT_SORT[].

ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0800 INPUT.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*&      Form  REQUEST_DISPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REQUEST_DISPO.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
           DISPO  LIKE MARC-DISPO,
           DESC LIKE T024D-DSNAM,
           END OF VALUE_TAB.

  SELECT DISTINCT A~DISPO DSNAM
     INTO TABLE VALUE_TAB
     FROM MARC AS A
     INNER JOIN T024D AS B
     ON A~DISPO = B~DISPO
     WHERE A~WERKS = 'P001'
     AND A~DISPO <> ' '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DISPO'
      DYNPPROG        = W_REPID
      DYNPNR          = W_DYNNR
      DYNPROFIELD     = 'DISPO'
      WINDOW_TITLE    = 'MRP Controller'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = VALUE_TAB
    EXCEPTIONS
      PARAMETER_ERROR = 1.


ENDFORM.                    " REQUEST_DISPO
*&---------------------------------------------------------------------*
*&      Form  REQUEST_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REQUEST_MODEL .
  DATA: L_ATINN              LIKE CABN-ATINN,
         L_ATNAM              LIKE CABN-ATNAM,
         L_ATWRT              LIKE AUSP-ATWRT,
         L_ATWTB              LIKE CAWNT-ATWTB.
  DATA: RETURN_TAB LIKE TABLE OF DDSHRETVAL WITH HEADER LINE.

  DATA : BEGIN OF VALUE_TAB OCCURS 0,
           MODEL LIKE ZSMM_219_PARTS-MODEL,
           NAME(40),
         END OF VALUE_TAB.

  DATA: L_MODEL LIKE ZSMM_219_PARTS-MODEL.

  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
   WHERE ATNAM = 'P_MODEL'.

  SELECT N~ATWRT T~ATWTB INTO (L_ATWRT, L_ATWTB)
    FROM CAWN AS N INNER JOIN CAWNT AS T
      ON N~ATINN = T~ATINN
     AND N~ATZHL = T~ATZHL
   WHERE N~ATINN = L_ATINN
     AND T~SPRAS = SY-LANGU .
    VALUE_TAB-NAME = L_ATWRT.  " ZTPP_VEH_MODEL-NAME.  l_atwtb
    VALUE_TAB-MODEL = L_ATWRT.  " ZTPP_VEH_MODEL-MODEL.
    APPEND VALUE_TAB.
  ENDSELECT.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'MODEL'
      DYNPPROG        = W_REPID
      DYNPNR          = W_DYNNR
      DYNPROFIELD     = 'MODEL'
      WINDOW_TITLE    = 'Model'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = VALUE_TAB
      RETURN_TAB      = RETURN_TAB
    EXCEPTIONS
      PARAMETER_ERROR = 1.

  READ TABLE RETURN_TAB INDEX 1.
  P_MODEL = RETURN_TAB-FIELDVAL.

  IF P_MODEL <> ' '.
    L_MODEL = P_MODEL+0(2).
  ENDIF.
  EXPORT L_MODEL TO MEMORY ID 'ZMOD'.
ENDFORM.                    " REQUEST_MODEL
*&---------------------------------------------------------------------*
*&      Form  START_PROGRESSBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0271   text
*      -->P_0272   text
*----------------------------------------------------------------------*
FORM START_PROGRESSBAR USING PF_TEXT VALUE(PF_VAL).
  DATA: PERCENT(3) TYPE N.

  MOVE: SY-INDEX TO PERCENT.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE = PF_VAL
      TEXT       = PF_TEXT
    EXCEPTIONS
      OTHERS     = 1.
ENDFORM.                    " START_PROGRESSBAR
