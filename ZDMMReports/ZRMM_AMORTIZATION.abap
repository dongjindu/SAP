************************************************************************
* Program Name      : ZRMM_AMORTIZATION
* Creation Date     : 11/17/2008
* Developer         : Furong Wang
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZRMM_AMORTIZATION  NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMMM.
TABLES: EKPO, EKKO.
TYPE-POOLS: VRM, SLIS.

DATA : BEGIN OF IT_TAB OCCURS 0,
      EBELN LIKE EKKO-EBELN,
      MATNR LIKE EKPO-MATNR,
      ERNAM LIKE EKKO-ERNAM,
      AEDAT LIKE EKKO-AEDAT,
      TXZ01 LIKE EKPO-TXZ01,
      LIFNR LIKE EKKO-LIFNR,
      EKGRP LIKE EKKO-EKGRP,
      NAME1 LIKE LFA1-NAME1,
      KTMNG LIKE EKPO-KTMNG,
      GRQTY LIKE EKBE-MENGE,
      DIFFQTY LIKE EKBE-MENGE,
      RLVL(1),
      ITEMTX(256),
      IF(4) TYPE C,
      END OF IT_TAB.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_FI  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_CO  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE,
       IT_FIELDCAT_DET TYPE LVC_T_FCAT WITH HEADER LINE. "/Detail

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: OK_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT   TYPE   I.

DATA: NAME  TYPE VRM_ID,
      LIST  TYPE VRM_VALUES,
      VALUE LIKE LINE OF LIST.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : S_MATNR FOR EKPO-MATNR,
                 S_LIFNR FOR EKKO-LIFNR,
                 S_EKGRP FOR EKKO-EKGRP,
                 S_EBELN FOR EKKO-EBELN,
                 S_AEDAT FOR EKKO-AEDAT.
*                 S_RLVL FOR EKPO-LOEKZ NO INTERVALS. "no-extension.
PARAMETERS: P_RLVL AS LISTBOX  VISIBLE LENGTH 40.
SELECTION-SCREEN END OF BLOCK BLOCK1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_LISTBOX_RLVL.

AT SELECTION-SCREEN.


START-OF-SELECTION.
  PERFORM GET_DATA.
  IF IT_TAB[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    PERFORM DISPLAY_DATA.
  ENDIF.
*---------------------------------------------------------------------*
*       FORM get_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_DATA.
  DATA: BEGIN OF LT_EKPO OCCURS 0,
        EBELN LIKE EKPO-EBELN,
        EBELP LIKE EKPO-EBELP,
*        KONNR LIKE EKKO-KONNR,
        MATNR LIKE EKPO-MATNR,
        ERNAM LIKE EKKO-ERNAM,
        AEDAT LIKE EKKO-AEDAT,
        TXZ01 LIKE EKPO-TXZ01,
        LIFNR LIKE EKKO-LIFNR,
        EKGRP LIKE EKKO-EKGRP,
        NAME1 LIKE LFA1-NAME1,
        KTMNG LIKE EKPO-KTMNG,
        GRQTY LIKE EKBE-MENGE,
        DIFFQTY LIKE EKBE-MENGE,
        RLVL(1),
        SA LIKE EKKO-EBELN,
        SA_ITEM LIKE EKPO-EBELP,
        END OF LT_EKPO.

  DATA: LT_CON LIKE TABLE OF LT_EKPO WITH HEADER LINE,
        LT_TEMP_TAB LIKE TABLE OF IT_TAB WITH HEADER LINE,
        LT_DATA  LIKE TABLE OF IT_TAB WITH HEADER LINE.

  DATA: BEGIN OF LT_EKBE OCCURS 0,
        EBELN LIKE EKBE-EBELN,
        EBELP LIKE EKBE-EBELP,
        MATNR LIKE EKPO-MATNR,
*            GJAHR LIKE EKBE-GJAHR,
        BELNR LIKE EKBE-BELNR,
*            LFBNR LIKE EKBE-LFBNR,
        BWART LIKE EKBE-BWART,
        SHKZG LIKE EKBE-SHKZG,
        MENGE LIKE EKBE-MENGE,
        END OF LT_EKBE.

  DATA: L_GRQTY LIKE EKBE-MENGE,
        L_LINE TYPE I.

  DATA: L_ID LIKE THEAD-TDID,
        L_NAME LIKE THEAD-TDNAME,
        L_OBJ LIKE THEAD-TDOBJECT,
        L_CON_ITEM LIKE EKPO-EBELP,
        L_KTMNG LIKE EKPO-KTMNG,
        L_EBELN LIKE EKKO-EBELN,
        L_HEADER(1),
        L_FIRST(1),
        L_RLVL LIKE IT_TAB-RLVL.

  DATA: LT_LINES LIKE TABLE OF TLINE WITH HEADER LINE.

  REFRESH IT_TAB.

*  IF S_KONNR[] IS INITIAL.
*
*    SELECT A~EBELN EBELP A~KONNR MATNR A~ERNAM A~AEDAT TXZ01
*           A~LIFNR EKGRP NAME1 KTMNG
*     INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
*     FROM EKKO AS A INNER JOIN EKPO AS B
*     ON A~EBELN = B~EBELN
*     INNER JOIN LFA1 AS C
*     ON C~LIFNR = A~LIFNR
*     WHERE A~LIFNR IN S_LIFNR
**     AND A~BSTYP = 'K'
*     AND A~AEDAT IN S_AEDAT
*     AND EKGRP IN S_EKGRP
*     AND A~KONNR <> ' '
*     AND B~MATNR IN S_MATNR
*     AND B~ELIKZ = ' '
*     AND B~EREKZ = ' '.
*
*    DESCRIBE TABLE LT_EKPO LINES L_LINE.
*
*    IF NOT S_MATNR[] IS INITIAL AND L_LINE > 0.
*      SELECT A~EBELN EBELP A~KONNR MATNR A~ERNAM A~AEDAT TXZ01
*                A~LIFNR EKGRP NAME1 KTMNG
*             INTO CORRESPONDING FIELDS OF TABLE LT_CON
*             FROM EKKO AS A INNER JOIN EKPO AS B
*             ON A~EBELN = B~EBELN
*             INNER JOIN LFA1 AS C
*             ON C~LIFNR = A~LIFNR
*             FOR ALL ENTRIES IN LT_EKPO
*             WHERE B~MATNR <> LT_EKPO-MATNR
*                AND EKGRP IN S_EKGRP
**               AND A~BSTYP = 'K'
*               AND A~KONNR = LT_EKPO-KONNR
*               AND A~KONNR <> ' '
*               AND B~ELIKZ = ' '
*               AND B~EREKZ = ' '.
*    ENDIF.
*    IF SY-SUBRC = 0.
*      APPEND LINES OF LT_CON TO LT_EKPO.
*    ENDIF.
*  ELSE.
*
*    SELECT A~EBELN EBELP A~KONNR MATNR A~ERNAM A~AEDAT TXZ01
*          A~LIFNR EKGRP NAME1 KTMNG
*     INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
*     FROM EKKO AS A INNER JOIN EKPO AS B
*     ON A~EBELN = B~EBELN
*     INNER JOIN LFA1 AS C
*     ON C~LIFNR = A~LIFNR
*     WHERE A~LIFNR IN S_LIFNR
*      AND A~AEDAT IN S_AEDAT
*      AND EKGRP IN S_EKGRP
**     AND A~BSTYP = 'K'
*      AND A~KONNR IN S_KONNR
*      AND B~MATNR IN S_MATNR
*      AND B~ELIKZ = ' '
*      AND B~EREKZ = ' '.
*  ENDIF.

  SELECT A~EBELN EBELP    "A~KONNR
         MATNR A~ERNAM A~AEDAT TXZ01
          A~LIFNR EKGRP NAME1 KTMNG
     INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
     FROM EKKO AS A INNER JOIN EKPO AS B
     ON A~EBELN = B~EBELN
     INNER JOIN LFA1 AS C
     ON C~LIFNR = A~LIFNR
     WHERE A~EBELN IN S_EBELN
      AND A~LIFNR IN S_LIFNR
      AND A~AEDAT IN S_AEDAT
      AND EKGRP IN S_EKGRP
      AND A~BSART = 'MK'
      AND B~MATNR IN S_MATNR
      AND B~LOEKZ = ' '.
*      AND B~ELIKZ = ' '
*      AND B~EREKZ = ' '.

  DESCRIBE TABLE LT_EKPO LINES L_LINE.
  IF L_LINE > 0.
    LOOP AT LT_EKPO.
      IF LT_EKPO-MATNR IS INITIAL.
        CONTINUE.
      ENDIF.
      SELECT SINGLE A~EBELN B~EBELP INTO (LT_EKPO-SA, LT_EKPO-SA_ITEM)
         FROM EKKO AS A INNER JOIN EKPO AS B
                ON A~EBELN = B~EBELN
         WHERE B~MATNR = LT_EKPO-MATNR
*        AND LOEKZ = ' '
*        AND A~BSTYP = 'K'
           AND A~KONNR = LT_EKPO-EBELN.
      IF SY-SUBRC = 0.
        MODIFY LT_EKPO.
      ENDIF.
    ENDLOOP.

    SELECT EBELN EBELP MATNR BELNR BWART SHKZG MENGE
       INTO TABLE LT_EKBE
       FROM EKBE
       FOR ALL ENTRIES IN LT_EKPO
       WHERE EBELN = LT_EKPO-SA
         AND EBELP = LT_EKPO-SA_ITEM
         AND MATNR = LT_EKPO-MATNR
         AND BEWTP = 'E'
         AND BWART IN ('101', '102', '122', '123').
    SORT LT_EKBE BY EBELN EBELP MATNR.
  ELSE.
    EXIT.
  ENDIF.

  LOOP AT LT_EKPO.
    CLEAR: L_GRQTY.
    LOOP AT LT_EKBE WHERE EBELN = LT_EKPO-SA
                      AND EBELP = LT_EKPO-SA_ITEM
                      AND MATNR = LT_EKPO-MATNR.
      IF LT_EKBE-SHKZG = 'S'.
        L_GRQTY = L_GRQTY + LT_EKBE-MENGE.
      ELSE.
        L_GRQTY = L_GRQTY - LT_EKBE-MENGE.
      ENDIF.
    ENDLOOP.
    MOVE-CORRESPONDING LT_EKPO TO IT_TAB.
    IT_TAB-GRQTY = L_GRQTY.

    L_ID = 'K01'.

*    SELECT SINGLE EBELP INTO L_CON_ITEM
*      FROM EKPO
*      WHERE EBELN = LT_EKPO-EBELN
*        AND MATNR = LT_EKPO-MATNR.
    L_CON_ITEM = LT_EKPO-EBELP.

    CONCATENATE LT_EKPO-EBELN L_CON_ITEM INTO L_NAME.
    L_OBJ = 'EKPO'.

    CALL FUNCTION 'READ_TEXT'
    EXPORTING
*   CLIENT                        = SY-MANDT
      ID                            =  L_ID
      LANGUAGE                      = SY-LANGU
      NAME                          = L_NAME
      OBJECT                        = L_OBJ
*   ARCHIVE_HANDLE                = 0
*   LOCAL_CAT                     = ' '
* IMPORTING
*   HEADER                        =
    TABLES
      LINES                         = LT_LINES
 EXCEPTIONS
   ID                            = 1
   LANGUAGE                      = 2
   NAME                          = 3
   NOT_FOUND                     = 4
   OBJECT                        = 5
   REFERENCE_CHECK               = 6
   WRONG_ACCESS_TO_ARCHIVE       = 7
   OTHERS                        = 8
            .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    READ TABLE LT_LINES INDEX 1.
    IT_TAB-ITEMTX = LT_LINES-TDLINE.
    READ TABLE LT_LINES INDEX 2.
    IF NOT LT_LINES-TDLINE IS INITIAL.
      CONCATENATE IT_TAB-ITEMTX LT_LINES-TDLINE INTO IT_TAB-ITEMTX
       SEPARATED BY SPACE.
    ENDIF.
    COLLECT IT_TAB.
    CLEAR: IT_TAB, LT_EKPO,LT_LINES.
    REFRESH LT_LINES.
*    MODIFY LT_EKPO.
  ENDLOOP.

********************************************

  REFRESH: LT_EKPO, LT_EKBE.
  SELECT A~EBELN EBELP MATNR LIFNR KTMNG
     INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
     FROM EKKO AS A INNER JOIN EKPO AS B
     ON A~EBELN = B~EBELN
     FOR ALL ENTRIES IN IT_TAB
     WHERE A~LIFNR = IT_TAB-LIFNR
     AND A~BSTYP = 'F'
     AND A~BSART = 'EM'
     AND B~MATNR = IT_TAB-MATNR.
*     AND B~ELIKZ = ' '
*     AND B~EREKZ = ' '.

  DESCRIBE TABLE LT_EKPO LINES L_LINE.

  IF L_LINE > 0.
    SELECT EBELN EBELP MATNR BELNR BWART SHKZG MENGE
       INTO TABLE LT_EKBE
       FROM EKBE
       FOR ALL ENTRIES IN LT_EKPO
       WHERE EBELN = LT_EKPO-EBELN
         AND EBELP = LT_EKPO-EBELP
         AND MATNR = LT_EKPO-MATNR
         AND BEWTP = 'E'
         AND BWART IN ('101', '102', '122', '123').
    IF SY-SUBRC = 0.
      SORT LT_EKBE BY EBELN EBELP MATNR.
      SORT LT_EKPO BY LIFNR MATNR.
      LOOP AT LT_EKPO.
        CLEAR: L_GRQTY.
        LOOP AT LT_EKBE WHERE EBELN = LT_EKPO-EBELN
                          AND EBELP = LT_EKPO-EBELP
                          AND MATNR = LT_EKPO-MATNR.
          IF LT_EKBE-SHKZG = 'S'.
            L_GRQTY = L_GRQTY + LT_EKBE-MENGE.
          ELSE.
            L_GRQTY = L_GRQTY - LT_EKBE-MENGE.
          ENDIF.
        ENDLOOP.
        IF L_GRQTY <> 0.
          LT_EKPO-GRQTY = L_GRQTY.
          MODIFY LT_EKPO.
        ENDIF.
      ENDLOOP.
      LOOP AT IT_TAB.
        CLEAR: L_GRQTY.
        LOOP AT LT_EKPO WHERE LIFNR = IT_TAB-LIFNR
                          AND MATNR = IT_TAB-MATNR.
          L_GRQTY = L_GRQTY + LT_EKPO-GRQTY.
        ENDLOOP.
        IF L_GRQTY <> 0.
          IT_TAB-GRQTY = IT_TAB-GRQTY + L_GRQTY.
*          IT_TAB-KTMNG = IT_TAB-KTMNG - L_GRQTY.
          MODIFY IT_TAB.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  SORT IT_TAB BY EBELN MATNR.

  LOOP AT IT_TAB.

    IF IT_TAB-MATNR IS INITIAL.
      IT_TAB-DIFFQTY =  IT_TAB-KTMNG - IT_TAB-GRQTY.
      IF IT_TAB-DIFFQTY > 30000.
        IT_TAB-RLVL = 'Z'.
      ENDIF.
      IF IT_TAB-DIFFQTY = 0.
        IT_TAB-RLVL = 'X'.
      ENDIF.

      IF IT_TAB-DIFFQTY <= 30000 AND IT_TAB-DIFFQTY >= 20001.
        IT_TAB-RLVL = 'E'.
      ENDIF.
      IF IT_TAB-DIFFQTY <= 20000 AND IT_TAB-DIFFQTY >= 15001.
        IT_TAB-RLVL = 'D'.
      ENDIF.
      IF IT_TAB-DIFFQTY <= 15000 AND IT_TAB-DIFFQTY >= 10001.
        IT_TAB-RLVL = 'C'.
      ENDIF.
      IF IT_TAB-DIFFQTY <= 10000 AND IT_TAB-DIFFQTY >= 5001.
        IT_TAB-RLVL = 'B'.
      ENDIF.
      IF IT_TAB-DIFFQTY <= 5000 AND IT_TAB-DIFFQTY >= 1.
        IT_TAB-RLVL = 'A'.
      ENDIF.
      L_RLVL = IT_TAB-RLVL.
      MODIFY IT_TAB.
    ELSE.
      IT_TAB-RLVL = L_RLVL.
      MODIFY IT_TAB.
    ENDIF.
  ENDLOOP.
  IF P_RLVL IS INITIAL.
  ELSE.
    DELETE IT_TAB WHERE NOT RLVL = P_RLVL.
  ENDIF.

*  READ TABLE IT_TAB INDEX 1.
*  L_KONNR = IT_TAB-KONNR.
  CLEAR:  L_KTMNG, L_GRQTY, L_EBELN,L_HEADER.
  L_FIRST = 'X'.
  LOOP AT IT_TAB.
    IF L_EBELN <> IT_TAB-EBELN.

      IF L_HEADER = 'X'.
        LT_TEMP_TAB-GRQTY = L_GRQTY.
        LT_TEMP_TAB-DIFFQTY =  LT_TEMP_TAB-KTMNG - LT_TEMP_TAB-GRQTY.
        IF LT_TEMP_TAB-DIFFQTY > 30000.
          LT_TEMP_TAB-RLVL = 'Z'.
        ENDIF.
        IF LT_TEMP_TAB-DIFFQTY = 0.
          LT_TEMP_TAB-RLVL = 'X'.
        ENDIF.

       IF LT_TEMP_TAB-DIFFQTY <= 30000 AND LT_TEMP_TAB-DIFFQTY >= 20001.
          LT_TEMP_TAB-RLVL = 'E'.
        ENDIF.
       IF LT_TEMP_TAB-DIFFQTY <= 20000 AND LT_TEMP_TAB-DIFFQTY >= 15001.
          LT_TEMP_TAB-RLVL = 'D'.
        ENDIF.
       IF LT_TEMP_TAB-DIFFQTY <= 15000 AND LT_TEMP_TAB-DIFFQTY >= 10001.
          LT_TEMP_TAB-RLVL = 'C'.
        ENDIF.
        IF LT_TEMP_TAB-DIFFQTY <= 10000 AND LT_TEMP_TAB-DIFFQTY >= 5001.
          LT_TEMP_TAB-RLVL = 'B'.
        ENDIF.
        IF LT_TEMP_TAB-DIFFQTY <= 5000 AND LT_TEMP_TAB-DIFFQTY >= 1.
          LT_TEMP_TAB-RLVL = 'A'.
        ENDIF.

        LT_TEMP_TAB-IF = 'C310'.
        APPEND LT_TEMP_TAB.

        LOOP AT LT_DATA.
          CLEAR: LT_DATA-DIFFQTY, LT_DATA-KTMNG, LT_DATA-RLVL.
          MODIFY LT_DATA.
        ENDLOOP.
      ENDIF.
      IF L_FIRST = 'X'.
        CLEAR: L_FIRST.
        LT_TEMP_TAB = IT_TAB.
      ELSE.
        APPEND LINES OF  LT_DATA TO LT_TEMP_TAB.
        CLEAR:  L_KTMNG, L_GRQTY, LT_DATA, LT_TEMP_TAB.
        REFRESH LT_DATA.
        LT_TEMP_TAB = IT_TAB.
      ENDIF.
      IF IT_TAB-MATNR IS INITIAL.
        L_HEADER = 'X'.
      ELSE.
        CLEAR: L_HEADER.
        APPEND LT_TEMP_TAB.
      ENDIF.

      L_EBELN = IT_TAB-EBELN.
    ELSE.
      LT_DATA = IT_TAB.
      IF L_HEADER = 'X'.
        CLEAR: LT_DATA-EBELN.
      ENDIF.
*    L_KTMNG = L_KTMNG + IT_TAB-KTMNG.
      L_GRQTY = L_GRQTY + IT_TAB-GRQTY.
      APPEND LT_DATA.
      CLEAR: LT_DATA.
    ENDIF.
  ENDLOOP.
  IF L_HEADER = 'X'.

    LT_TEMP_TAB-GRQTY = L_GRQTY.
    LT_TEMP_TAB-DIFFQTY =  LT_TEMP_TAB-KTMNG - LT_TEMP_TAB-GRQTY.
    IF LT_TEMP_TAB-DIFFQTY > 30000.
      LT_TEMP_TAB-RLVL = 'Z'.
    ENDIF.
    IF LT_TEMP_TAB-DIFFQTY = 0.
      LT_TEMP_TAB-RLVL = 'X'.
    ENDIF.

    IF LT_TEMP_TAB-DIFFQTY <= 30000 AND LT_TEMP_TAB-DIFFQTY >= 20001.
      LT_TEMP_TAB-RLVL = 'E'.
    ENDIF.
    IF LT_TEMP_TAB-DIFFQTY <= 20000 AND LT_TEMP_TAB-DIFFQTY >= 15001.
      LT_TEMP_TAB-RLVL = 'D'.
    ENDIF.
    IF LT_TEMP_TAB-DIFFQTY <= 15000 AND LT_TEMP_TAB-DIFFQTY >= 10001.
      LT_TEMP_TAB-RLVL = 'C'.
    ENDIF.
    IF LT_TEMP_TAB-DIFFQTY <= 10000 AND LT_TEMP_TAB-DIFFQTY >= 5001.
      LT_TEMP_TAB-RLVL = 'B'.
    ENDIF.
    IF LT_TEMP_TAB-DIFFQTY <= 5000 AND LT_TEMP_TAB-DIFFQTY >= 1.
      LT_TEMP_TAB-RLVL = 'A'.
    ENDIF.
    LT_TEMP_TAB-IF = 'C310'.
    APPEND LT_TEMP_TAB.
    LOOP AT LT_DATA.
      CLEAR: LT_DATA-DIFFQTY, LT_DATA-KTMNG, LT_DATA-RLVL.
      MODIFY LT_DATA.
    ENDLOOP.

    APPEND LINES OF  LT_DATA TO LT_TEMP_TAB.
    L_EBELN = IT_TAB-EBELN.
    CLEAR:  L_KTMNG, L_GRQTY, LT_DATA, LT_TEMP_TAB.
    REFRESH LT_DATA.
  ELSE.
    APPEND LINES OF  LT_DATA TO LT_TEMP_TAB.
    CLEAR:  L_KTMNG, L_GRQTY, LT_DATA, LT_TEMP_TAB.
    REFRESH LT_DATA.
  ENDIF.

  REFRESH IT_TAB.
  IT_TAB[] = LT_TEMP_TAB[].
  REFRESH LT_TEMP_TAB.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_SCREEN.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_EXCEL'.
      SCREEN-INPUT = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_screen

*---------------------------------------------------------------------*
*       FORM display_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM DISPLAY_DATA.
  CALL SCREEN 200.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'ST200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_TAB'.
    PERFORM ASSIGN_ITAB_TO_ALV.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.
ENDMODULE.                 " display_alv  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN ''.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
               I_DEFAULT        = SPACE
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_TAB[]
               IT_SORT          = IT_SORT[].

ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  CLEAR: W_REPID.
  CREATE OBJECT GRID_CONTAINER
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
          EXCEPTIONS
           CNTL_ERROR = 1
           CNTL_SYSTEM_ERROR = 2
           CREATE_ERROR = 3
           LIFETIME_ERROR = 4
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
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.
ENDFORM.                    " create_container_n_object

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

*  IT_SORT-SPOS           = 1.
*  IT_SORT-FIELDNAME      = 'KONNR'.
*  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
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

                                  'S' 'EBELN'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Contract Number',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',


                                  'S' 'TXZ01'        ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '40',

                                  'S' 'LIFNR'       ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'EKGRP'       ' ',
                                  ' ' 'COLTEXT'     'Pur Grp',
                                  'E' 'OUTPUTLEN'   '7',


                                  'S' 'ERNAM'       ' ',
                                  ' ' 'COLTEXT'     'Created by',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'AEDAT'       ' ',
                                  ' ' 'COLTEXT'     'Created on',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'NAME1'        ' ',
                                  ' ' 'COLTEXT'     'Vendor Name',
                                  'E' 'OUTPUTLEN'   '40',

                                  'S' 'KTMNG'       ' ',
                                  ' ' 'COLTEXT'     'Target QTY',
                                  ' ' 'DECIMALS_O'    '0',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '13',


                                  'S' 'GRQTY'       ' ',
                                  ' ' 'COLTEXT'     'GR QTY',
                                  ' ' 'DECIMALS_O'    '0',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'DIFFQTY'       ' ',
                                  ' ' 'COLTEXT'     'Different QTY',
                                  ' ' 'DECIMALS_O'    '0',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '13',


                                  'S' 'RLVL'        ' ',
                                  ' ' 'COLTEXT'     'Risk Level',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'ITEMTX'        ' ',
                                  ' ' 'COLTEXT'     'Remarks',
                                  'E' 'OUTPUTLEN'   '256'.

ENDFORM.
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
*&      Form  SET_LISTBOX_rlvl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_LISTBOX_RLVL.

  IF IT_TAB-DIFFQTY > 30000.
    IT_TAB-RLVL = 'Z'.
  ENDIF.
  IF IT_TAB-DIFFQTY = 0.
    IT_TAB-RLVL = 'X'.
  ENDIF.
  IF IT_TAB-DIFFQTY <= 30000 AND IT_TAB-DIFFQTY >= 20001.
    IT_TAB-RLVL = 'E'.
  ENDIF.
  IF IT_TAB-DIFFQTY <= 20000 AND IT_TAB-DIFFQTY >= 15001.
    IT_TAB-RLVL = 'D'.
  ENDIF.
  IF IT_TAB-DIFFQTY <= 15000 AND IT_TAB-DIFFQTY >= 10001.
    IT_TAB-RLVL = 'C'.
  ENDIF.
  IF IT_TAB-DIFFQTY <= 10000 AND IT_TAB-DIFFQTY >= 5001.
    IT_TAB-RLVL = 'B'.
  ENDIF.
  IF IT_TAB-DIFFQTY <= 5000 AND IT_TAB-DIFFQTY >= 1.
    IT_TAB-RLVL = 'A'.
  ENDIF.

  NAME = 'P_RLVL'.
  MOVE: SPACE       TO VALUE-KEY,
        ' For all'       TO VALUE-TEXT.
  APPEND VALUE TO LIST.
  MOVE: 'A'      TO  VALUE-KEY,
        ' 1 =< Difference < 5001' TO VALUE-TEXT.
  APPEND VALUE TO LIST.

  MOVE: 'B'      TO  VALUE-KEY,
          '  5001 =< Difference < 10001' TO VALUE-TEXT.
  APPEND VALUE TO LIST.

  MOVE: 'C'      TO  VALUE-KEY,
          ' 10001 =< Difference < 15001' TO VALUE-TEXT.
  APPEND VALUE TO LIST.

  MOVE: 'D'      TO  VALUE-KEY,
         ' 15001 =< Difference < 20001' TO VALUE-TEXT.
  APPEND VALUE TO LIST.

  MOVE: 'E'      TO  VALUE-KEY,
            ' 20001 =< Difference < 30001' TO VALUE-TEXT.
  APPEND VALUE TO LIST.
  MOVE: 'Z'      TO  VALUE-KEY,
            ' 30001 <= Difference' TO VALUE-TEXT.
  APPEND VALUE TO LIST.
  MOVE: 'X'      TO  VALUE-KEY,
            ' Difference = 0' TO VALUE-TEXT.
  APPEND VALUE TO LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = NAME
            VALUES = LIST.
ENDFORM.                    " SET_LISTBOX_rlvl
