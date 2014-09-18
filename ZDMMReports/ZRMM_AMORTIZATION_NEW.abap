************************************************************************
* Program Name      : ZRMM_AMORTIZATION_NEW
* Creation Date     : 09/2009
* Developer         : Furong Wang
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
*
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
      GRQTY LIKE MSEG-MENGE,
      DIFFQTY LIKE ZTMM_RISK_LEVEL-MINQTY,
      RLVL(2),
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

DATA:  IT_MAIL TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
       WITH HEADER LINE,
        IT_EBODY TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
       WITH HEADER LINE.


*DATA: P_DIST(25) VALUE 'POIFVAL'.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : S_MATNR FOR EKPO-MATNR,
                 S_LIFNR FOR EKKO-LIFNR,
                 S_EKGRP FOR EKKO-EKGRP,
                 S_EBELN FOR EKKO-EBELN.
PARAMETERS: P_DIFQTY TYPE ZZQTY_S,  " EKPO-KTMNG,  " OBLIGATORY,
            P_DAYS TYPE I.

*SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN  BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) TEXT-U05.
SELECTION-SCREEN COMMENT 15(6) TEXT-U04 FOR FIELD P_ALL.
PARAMETERS: P_ALL AS CHECKBOX USER-COMMAND CHAL DEFAULT 'X'.
SELECTION-SCREEN  END OF LINE.

SELECTION-SCREEN  BEGIN OF LINE.
SELECTION-SCREEN COMMENT 15(6) TEXT-U01 FOR FIELD P_A.
PARAMETERS: P_A AS CHECKBOX MODIF ID ABC USER-COMMAND CHOT1.
SELECTION-SCREEN COMMENT 27(6) TEXT-U02 FOR FIELD P_E.
PARAMETERS: P_E AS CHECKBOX MODIF ID ABC USER-COMMAND CHOT2.
SELECTION-SCREEN COMMENT 39(6) TEXT-U03 FOR FIELD P_F.
PARAMETERS: P_F AS CHECKBOX MODIF ID ABC USER-COMMAND CHOT3.
SELECTION-SCREEN  END OF LINE.

SELECTION-SCREEN ULINE.
SELECTION-SCREEN  BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-U06 FOR FIELD P_BATCH.
PARAMETERS: P_BATCH AS CHECKBOX.

SELECTION-SCREEN  END OF LINE.
PARAMETERS: P_DIST(25) DEFAULT 'POIFVAL' .
SELECTION-SCREEN END OF BLOCK BLOCK1.

AT SELECTION-SCREEN.
  IF SY-UCOMM = 'CHAL'.
    PERFORM MODIFY_SCREEN_ALL.
  ENDIF.
  IF SY-UCOMM = 'CHOT1' OR SY-UCOMM = 'CHOT2' OR SY-UCOMM = 'CHOT3'.
    PERFORM MODIFY_SCREEN_OTHER USING SY-UCOMM.
  ENDIF.

START-OF-SELECTION.
  PERFORM GET_DATA.
  IF IT_TAB[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    IF P_BATCH IS INITIAL.
      PERFORM DISPLAY_DATA.
    ELSE.
      PERFORM SEND_EMAIL.
    ENDIF.
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
        MATNR LIKE EKPO-MATNR,
        ERNAM LIKE EKKO-ERNAM,
        AEDAT LIKE EKKO-AEDAT,
        TXZ01 LIKE EKPO-TXZ01,
        LIFNR LIKE EKKO-LIFNR,
        EKGRP LIKE EKKO-EKGRP,
        NAME1 LIKE LFA1-NAME1,
        KTMNG LIKE EKPO-KTMNG,
        GRQTY LIKE MSEG-MENGE,
        DIFFQTY TYPE ZZQTY_S,
        RLVL(2),
        WERKS LIKE EKPO-WERKS,
        KDATB LIKE EKKO-KDATB,
        KDATE LIKE EKKO-KDATE,
        LOEKZ LIKE EKPO-LOEKZ,
        END OF LT_EKPO.

*  DATA: BEGIN OF LT_EKBE OCCURS 0,
*        EBELN LIKE EKBE-EBELN,
*        EBELP LIKE EKBE-EBELP,
*        MATNR LIKE EKPO-MATNR,
*        BELNR LIKE EKBE-BELNR,
*        BWART LIKE EKBE-BWART,
*        SHKZG LIKE EKBE-SHKZG,
*        MENGE LIKE EKBE-MENGE,
*        BUDAT LIKE EKBE-BUDAT,
*        END OF LT_EKBE.

  DATA: BEGIN OF LT_MSEG OCCURS 0,
        EBELN LIKE MSEG-EBELN,
        EBELP LIKE MSEG-EBELP,
        MATNR LIKE EKPO-MATNR,
        BELNR LIKE MSEG-BELNR,
        BWART LIKE MSEG-BWART,
        SHKZG LIKE MSEG-SHKZG,
        MENGE LIKE MSEG-MENGE,
        BUDAT LIKE MKPF-BUDAT,
        MBLNR LIKE MSEG-MBLNR,
        MJAHR LIKE MSEG-MJAHR,
        ZEILE LIKE MSEG-ZEILE,
        END OF LT_MSEG.

  DATA: L_GRQTY LIKE MSEG-MENGE,
        L_LINE TYPE I,
        L_OB LIKE SY-DATUM,
        L_FLAG(1),
        L_GRFLAG(1),
        L_INDEX LIKE SY-TABIX.

  DATA: L_ID LIKE THEAD-TDID,
        L_NAME LIKE THEAD-TDNAME,
        L_OBJ LIKE THEAD-TDOBJECT,
        L_CON_ITEM LIKE EKPO-EBELP,
        L_EBELN LIKE EKKO-EBELN,
        L_RLVL LIKE IT_TAB-RLVL.

  DATA: LT_LINES LIKE TABLE OF TLINE WITH HEADER LINE.
  DATA: LT_RLVL LIKE TABLE OF ZTMM_RISK_LEVEL WITH HEADER LINE.

  REFRESH IT_TAB.

  IF P_ALL IS INITIAL.
    CASE 'X'.
      WHEN P_A.
        SELECT A~EBELN EBELP
             MATNR A~ERNAM A~AEDAT TXZ01
              A~LIFNR EKGRP NAME1 KTMNG B~WERKS KDATB KDATE B~LOEKZ
         INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
         FROM EKKO AS A INNER JOIN EKPO AS B
         ON A~EBELN = B~EBELN
         INNER JOIN LFA1 AS C
         ON C~LIFNR = A~LIFNR
         WHERE A~EBELN IN S_EBELN
          AND A~BSART = 'MK'
          AND A~LIFNR IN S_LIFNR
          AND ( KDATB <= SY-DATUM AND KDATE >= SY-DATUM )
          AND EKGRP IN S_EKGRP
          AND B~MATNR IN S_MATNR
           AND B~LOEKZ <> 'L'.
      WHEN P_E.
        SELECT A~EBELN EBELP
              MATNR A~ERNAM A~AEDAT TXZ01
               A~LIFNR EKGRP NAME1 KTMNG B~WERKS KDATB KDATE B~LOEKZ
          INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
          FROM EKKO AS A INNER JOIN EKPO AS B
          ON A~EBELN = B~EBELN
          INNER JOIN LFA1 AS C
          ON C~LIFNR = A~LIFNR
          WHERE A~EBELN IN S_EBELN
           AND A~BSART = 'MK'
           AND A~LIFNR IN S_LIFNR
           AND KDATE <= SY-DATUM
           AND EKGRP IN S_EKGRP
           AND B~MATNR IN S_MATNR
           AND B~LOEKZ <> 'L'.
      WHEN P_F.
        SELECT A~EBELN EBELP
              MATNR A~ERNAM A~AEDAT TXZ01
               A~LIFNR EKGRP NAME1 KTMNG B~WERKS KDATB KDATE B~LOEKZ
          INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
          FROM EKKO AS A INNER JOIN EKPO AS B
          ON A~EBELN = B~EBELN
          INNER JOIN LFA1 AS C
          ON C~LIFNR = A~LIFNR
          WHERE A~EBELN IN S_EBELN
           AND A~BSART = 'MK'
           AND A~LIFNR IN S_LIFNR
           AND KDATB > SY-DATUM
           AND EKGRP IN S_EKGRP
           AND B~MATNR IN S_MATNR
           AND B~LOEKZ <> 'L'.
    ENDCASE.
  ELSE.
    SELECT A~EBELN EBELP
           MATNR A~ERNAM A~AEDAT TXZ01
            A~LIFNR EKGRP NAME1 KTMNG B~WERKS KDATB KDATE B~LOEKZ
INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
       FROM EKKO AS A INNER JOIN EKPO AS B
       ON A~EBELN = B~EBELN
       INNER JOIN LFA1 AS C
       ON C~LIFNR = A~LIFNR
       WHERE A~EBELN IN S_EBELN
        AND A~LIFNR IN S_LIFNR
        AND EKGRP IN S_EKGRP
        AND A~BSART = 'MK'
        AND B~MATNR IN S_MATNR
        AND B~LOEKZ <> 'L'.
  ENDIF.
  DESCRIBE TABLE LT_EKPO LINES L_LINE.
  IF L_LINE > 0.

** Changed by Furong on 12/18/09
*    SELECT EBELN EBELP MATNR BELNR BWART SHKZG MENGE BUDAT
*       INTO TABLE LT_EKBE
*       FROM EKBE
*       FOR ALL ENTRIES IN LT_EKPO
*       WHERE WERKS = LT_EKPO-WERKS
*         AND ( BUDAT >= LT_EKPO-KDATB AND BUDAT <= LT_EKPO-KDATE )
*         AND BWART IN ('101', '102', '122', '123')
*         AND MATNR = LT_EKPO-MATNR
*         AND BEWTP = 'E'.
*    SORT LT_EKBE BY MATNR.
    SELECT EBELN EBELP MATNR BELNR BWART SHKZG MENGE BUDAT
           A~MBLNR A~MJAHR ZEILE
       INTO TABLE LT_MSEG
       FROM MSEG AS A
       INNER JOIN MKPF AS B
       ON A~MBLNR = B~MBLNR
       AND A~MJAHR = B~MJAHR
       FOR ALL ENTRIES IN LT_EKPO
       WHERE ( BUDAT >= LT_EKPO-KDATB AND BUDAT <= LT_EKPO-KDATE )
         AND BWART IN ('101', '102', '122', '123')
         AND MATNR = LT_EKPO-MATNR.
*         AND BLART = 'MK'.

    SORT LT_MSEG BY MATNR.
** End of change
  ELSE.
    EXIT.
  ENDIF.

  L_OB = SY-DATUM - P_DAYS.

  LOOP AT LT_EKPO.
    CLEAR: L_GRQTY, L_GRFLAG, L_NAME.

** Changed by Furong on 12/18/09
*    LOOP AT LT_EKBE WHERE MATNR = LT_EKPO-MATNR.
*      IF LT_EKBE-BUDAT >= L_OB.
*        L_GRFLAG = 'X'.
*      ENDIF.
*      IF LT_EKBE-SHKZG = 'S'.
*        L_GRQTY = L_GRQTY + LT_EKBE-MENGE.
*      ELSE.
*        L_GRQTY = L_GRQTY - LT_EKBE-MENGE.
*      ENDIF.
*    ENDLOOP.

    LOOP AT LT_MSEG WHERE MATNR = LT_EKPO-MATNR.
      IF LT_MSEG-BUDAT >= L_OB.
        L_GRFLAG = 'X'.
      ENDIF.
      IF LT_MSEG-SHKZG = 'S'.
        L_GRQTY = L_GRQTY + LT_MSEG-MENGE.
      ELSE.
        L_GRQTY = L_GRQTY - LT_MSEG-MENGE.
      ENDIF.
    ENDLOOP.
** End of change

    MOVE-CORRESPONDING LT_EKPO TO IT_TAB.
    IT_TAB-GRQTY = L_GRQTY.
    IF L_GRFLAG IS INITIAL AND LT_EKPO-LOEKZ <> 'S'.
      IT_TAB-RLVL = 'OB'.
    ENDIF.
    L_ID = 'K01'.

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
  ENDLOOP.


** Emergency PO

  REFRESH: LT_EKPO, LT_MSEG.

  SELECT A~EBELN EBELP MATNR LIFNR KTMNG WERKS KDATB KDATE B~LOEKZ
     INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
     FROM EKKO AS A INNER JOIN EKPO AS B
     ON A~EBELN = B~EBELN
     FOR ALL ENTRIES IN IT_TAB
     WHERE A~LIFNR = IT_TAB-LIFNR
     AND A~BSTYP = 'F'
     AND A~BSART = 'EM'
     AND B~MATNR = IT_TAB-MATNR.

  DESCRIBE TABLE LT_EKPO LINES L_LINE.

  IF L_LINE > 0.

** Changed by Furong on 12/18/09
*    SELECT EBELN EBELP MATNR BELNR BWART SHKZG MENGE BUDAT
*      INTO TABLE LT_EKBE
*      FROM EKBE
*      FOR ALL ENTRIES IN LT_EKPO
*      WHERE WERKS = LT_EKPO-WERKS
*          AND ( BUDAT >= LT_EKPO-KDATB AND BUDAT <= LT_EKPO-KDATE )
*        AND BWART IN ('101', '102', '122', '123')
*        AND MATNR = LT_EKPO-MATNR
*        AND BEWTP = 'E'.

    SELECT EBELN EBELP MATNR BELNR BWART SHKZG MENGE BUDAT
        A~MBLNR A~MJAHR ZEILE
       INTO TABLE LT_MSEG
       FROM MSEG AS A
       INNER JOIN MKPF AS B
       ON A~MBLNR = B~MBLNR
       AND A~MJAHR = B~MJAHR
       FOR ALL ENTRIES IN LT_EKPO
       WHERE ( BUDAT >= LT_EKPO-KDATB AND BUDAT <= LT_EKPO-KDATE )
         AND BWART IN ('101', '102', '122', '123')
         AND MATNR = LT_EKPO-MATNR.
*         AND BLART = 'MK'.
** End of change

    IF SY-SUBRC = 0.
** Changed by Furong on 12/18/09
*      SORT LT_EKBE BY MATNR.
      SORT LT_MSEG BY MATNR.
** End of change
      LOOP AT IT_TAB.
        CLEAR: L_GRQTY, L_GRFLAG.
** Changed by Furong on 12/18/09
*        LOOP AT LT_EKBE WHERE MATNR = IT_TAB-MATNR.
*          IF LT_EKBE-BUDAT >= L_OB.
*            L_GRFLAG = 'X'.
*          ENDIF.
*          IF LT_EKBE-SHKZG = 'S'.
*            L_GRQTY = L_GRQTY + LT_EKBE-MENGE.
*          ELSE.
*            L_GRQTY = L_GRQTY - LT_EKBE-MENGE.
*          ENDIF.
*        ENDLOOP.
        LOOP AT LT_MSEG WHERE MATNR = IT_TAB-MATNR.
          IF LT_MSEG-BUDAT >= L_OB.
            L_GRFLAG = 'X'.
          ENDIF.
          IF LT_MSEG-SHKZG = 'S'.
            L_GRQTY = L_GRQTY + LT_MSEG-MENGE.
          ELSE.
            L_GRQTY = L_GRQTY - LT_MSEG-MENGE.
          ENDIF.
        ENDLOOP.
** End of change
        IF L_GRQTY <> 0.
          IF L_GRFLAG = 'X'.
            CLEAR: IT_TAB-RLVL.
          ENDIF.
          IT_TAB-GRQTY = IT_TAB-GRQTY + L_GRQTY.
          MODIFY IT_TAB.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  SORT IT_TAB BY EBELN MATNR.

*  IF P_DIFQTY >= 0.
  SELECT * INTO TABLE LT_RLVL
    FROM ZTMM_RISK_LEVEL
    WHERE MINQTY <= P_DIFQTY.
*       AND MINQTY >= 0.
*  ELSE.
*    SELECT * INTO TABLE LT_RLVL
*      FROM ZTMM_RISK_LEVEL
*      WHERE MAXQTY > P_DIFQTY
*        AND MAXQTY < 0.
*  ENDIF.

  LOOP AT IT_TAB.
    IF IT_TAB-MATNR IS INITIAL.
      CLEAR: L_GRQTY.
      L_INDEX = SY-TABIX.
      L_EBELN = IT_TAB-EBELN.
    ELSE.
      L_GRQTY = L_GRQTY + IT_TAB-GRQTY.
    ENDIF.
    AT END OF EBELN.
      READ TABLE IT_TAB INDEX L_INDEX.
      IT_TAB-GRQTY = L_GRQTY.
      IT_TAB-IF = 'C310'.
      IF L_INDEX > 0.
        MODIFY IT_TAB INDEX L_INDEX TRANSPORTING GRQTY IF.
      ENDIF.
    ENDAT.
  ENDLOOP.

  LOOP AT IT_TAB.
    IF IT_TAB-MATNR IS INITIAL.
      L_EBELN = IT_TAB-EBELN.
      L_FLAG = 'X'.
      IT_TAB-DIFFQTY =  IT_TAB-KTMNG - IT_TAB-GRQTY.
      LOOP AT LT_RLVL.
        IF LT_RLVL-MINQTY <= IT_TAB-DIFFQTY
           AND LT_RLVL-MAXQTY >= IT_TAB-DIFFQTY.
          IT_TAB-RLVL = LT_RLVL-RLVL.
          CLEAR: L_FLAG.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF L_FLAG IS INITIAL.
        MODIFY IT_TAB.
      ELSE.
        DELETE IT_TAB.
      ENDIF.
    ELSE.
      IF IT_TAB-EBELN = L_EBELN AND L_FLAG = 'X'.
        DELETE IT_TAB.
      ENDIF.
    ENDIF.
  ENDLOOP.

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
    EXPORTING
      IS_LAYOUT            = WA_IS_LAYOUT
      I_SAVE               = WA_SAVE
      IS_VARIANT           = WA_VARIANT
      I_DEFAULT            = SPACE
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      IT_FIELDCATALOG      = IT_FIELDCAT[]
      IT_OUTTAB            = IT_TAB[]
      IT_SORT              = IT_SORT[].

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
*FORM SET_LISTBOX_RLVL.
*
*  IF IT_TAB-DIFFQTY > 30000.
*    IT_TAB-RLVL = 'Z'.
*  ENDIF.
*  IF IT_TAB-DIFFQTY = 0.
*    IT_TAB-RLVL = 'X'.
*  ENDIF.
*  IF IT_TAB-DIFFQTY <= 30000 AND IT_TAB-DIFFQTY >= 20001.
*    IT_TAB-RLVL = 'E'.
*  ENDIF.
*  IF IT_TAB-DIFFQTY <= 20000 AND IT_TAB-DIFFQTY >= 15001.
*    IT_TAB-RLVL = 'D'.
*  ENDIF.
*  IF IT_TAB-DIFFQTY <= 15000 AND IT_TAB-DIFFQTY >= 10001.
*    IT_TAB-RLVL = 'C'.
*  ENDIF.
*  IF IT_TAB-DIFFQTY <= 10000 AND IT_TAB-DIFFQTY >= 5001.
*    IT_TAB-RLVL = 'B'.
*  ENDIF.
*  IF IT_TAB-DIFFQTY <= 5000 AND IT_TAB-DIFFQTY >= 1.
*    IT_TAB-RLVL = 'A'.
*  ENDIF.
*
*  NAME = 'P_RLVL'.
*  MOVE: SPACE       TO VALUE-KEY,
*        ' For all'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*  MOVE: 'A'      TO  VALUE-KEY,
*        ' 1 =< Difference < 5001' TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'B'      TO  VALUE-KEY,
*          '  5001 =< Difference < 10001' TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'C'      TO  VALUE-KEY,
*          ' 10001 =< Difference < 15001' TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'D'      TO  VALUE-KEY,
*         ' 15001 =< Difference < 20001' TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'E'      TO  VALUE-KEY,
*            ' 20001 =< Difference < 30001' TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*  MOVE: 'Z'      TO  VALUE-KEY,
*            ' 30001 <= Difference' TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*  MOVE: 'X'      TO  VALUE-KEY,
*            ' Difference = 0' TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*       EXPORTING
*            ID     = NAME
*            VALUES = LIST.
*ENDFORM.                    " SET_LISTBOX_rlvl

*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN_ALL.
  LOOP AT SCREEN .
    IF P_ALL = 'X' AND SCREEN-GROUP1 EQ 'ABC'.
      P_A = ' '.
      P_E = ' '.
      P_F = ' '.
      SCREEN-INVISIBLE = 1.
      SCREEN-ACTIVE    = 0.
      SCREEN-INPUT     = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN_OTHER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN_OTHER USING P_UCOMM.
  LOOP AT SCREEN .
*    IF P_A = 'X' OR P_E = 'X' OR P_F = 'X'.
*      IF SCREEN-NAME = 'P_ALL'.
*        P_ALL = ' '.
**        SCREEN-INVISIBLE = 1.
*        SCREEN-ACTIVE    = 0.
*        SCREEN-INPUT     = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
    CASE P_UCOMM.
      WHEN 'CHOT1'.
        IF P_A = 'X'.
          IF SCREEN-NAME = 'P_ALL'.
            P_ALL = ' '.
            P_E = ' '.
            P_F = ' '.
          ENDIF.
        ENDIF.
      WHEN 'CHOT2'.
        IF P_E = 'X'.
          IF SCREEN-NAME = 'P_ALL'.
            P_ALL = ' '.
            P_A = ' '.
            P_F = ' '.
          ENDIF.
        ENDIF.
      WHEN 'CHOT3'.
        IF P_F = 'X'.
          IF SCREEN-NAME = 'P_ALL'.
            P_ALL = ' '.
            P_E = ' '.
            P_A = ' '.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN_OTHER
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_EMAIL.
  DATA: L_SUBJECT(40) TYPE C VALUE 'Amortization Report'.

  DATA:   IT_PACKING_LIST LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
          IT_CONTENTS LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
          IT_RECEIVERS LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
          IT_ATTACHMENT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
          GD_CNT TYPE I,
          TAB_LINES TYPE I,
          GD_SENT_ALL(1) TYPE C,
          GD_DOC_DATA LIKE SODOCCHGI1,
          GD_ERROR TYPE SY-SUBRC.

* Compose Email Body
  PERFORM COMPOSE_EMAIL_BODY.

  GD_DOC_DATA-DOC_SIZE = 1.

* Populate the subject/generic message attributes
  GD_DOC_DATA-OBJ_LANGU = SY-LANGU.
  GD_DOC_DATA-OBJ_NAME  = SY-REPID.
  GD_DOC_DATA-OBJ_DESCR = L_SUBJECT.
  GD_DOC_DATA-SENSITIVTY = 'F'.

* Describe the body of the message
  CLEAR IT_PACKING_LIST.
  REFRESH IT_PACKING_LIST.
  IT_PACKING_LIST-TRANSF_BIN = SPACE.
  IT_PACKING_LIST-HEAD_START = 1.
  IT_PACKING_LIST-HEAD_NUM = 0.
  IT_PACKING_LIST-BODY_START = 1.
  DESCRIBE TABLE IT_MAIL LINES IT_PACKING_LIST-BODY_NUM.
  IT_PACKING_LIST-DOC_TYPE = 'RAW'.
  APPEND IT_PACKING_LIST.

  DESCRIBE TABLE IT_MAIL LINES TAB_LINES.
  READ  TABLE IT_MAIL INDEX TAB_LINES.

 IT_PACKING_LIST-DOC_SIZE = ( TAB_LINES - 1 ) * 255 + STRLEN( IT_MAIL ).
  IT_PACKING_LIST-TRANSF_BIN = 'X'.
  IT_PACKING_LIST-HEAD_START = 1.
  IT_PACKING_LIST-HEAD_NUM   = 0.
  IT_PACKING_LIST-BODY_START = 1.
  IT_PACKING_LIST-BODY_NUM   = TAB_LINES.
  IT_PACKING_LIST-DOC_TYPE   = 'RAW'.
  IT_PACKING_LIST-OBJ_NAME   = 'ATTACHMENT'.
  IT_PACKING_LIST-OBJ_DESCR  = 'Amortization Report'.
  APPEND IT_PACKING_LIST.

  APPEND 'Please check attached file.' TO  IT_EBODY.
  APPEND '' TO  IT_EBODY.
  APPEND 'Thanks' TO  IT_EBODY.

* Add the recipients email address
  CLEAR IT_RECEIVERS.
  REFRESH IT_RECEIVERS.
  IT_RECEIVERS-RECEIVER = P_DIST.
  IT_RECEIVERS-REC_TYPE = 'C'.
  IT_RECEIVERS-COM_TYPE = 'INT'.
  IT_RECEIVERS-NOTIF_DEL = 'X'.
  IT_RECEIVERS-NOTIF_NDEL = 'X'.
  APPEND IT_RECEIVERS.

* Call the FM to post the message to SAPMAIL
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1' STARTING NEW TASK 'T1'
    EXPORTING
      DOCUMENT_DATA              = GD_DOC_DATA
*            PUT_IN_OUTBOX              = 'X'
            COMMIT_WORK                = 'X'
    TABLES
      PACKING_LIST               = IT_PACKING_LIST
      CONTENTS_TXT               = IT_EBODY
      CONTENTS_BIN               = IT_MAIL
      RECEIVERS                  = IT_RECEIVERS
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      DOCUMENT_TYPE_NOT_EXIST    = 3
      OPERATION_NO_AUTHORIZATION = 4
      PARAMETER_ERROR            = 5
      X_ERROR                    = 6
      ENQUEUE_ERROR              = 7
      OTHERS                     = 8.
  IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.

  ENDIF.

ENDFORM.                    " SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  Compose_EMAIL_body
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COMPOSE_EMAIL_BODY.
  DATA: L_MESSAGE TYPE SO_TEXT255.
  DATA : BEGIN OF LT_TEMP OCCURS 0,
    EBELN(10),
    MATNR(18),
    TXZ01(40),
    LIFNR(6),
    EKGRP(7),
    ERNAM(10),
    AEDAT(10),
    NAME1(40),
    KTMNG(13),
    GRQTY(13),
    DIFFQTY(13),
    RLVL(10),
    ITEMTX(150),
    END OF LT_TEMP.

  CLEAR: IT_MAIL,IT_MAIL[].

* Heading

  LT_TEMP-EBELN = 'Contract Number'.
  LT_TEMP-MATNR = 'Material'.
  LT_TEMP-TXZ01 = 'Description'.
  LT_TEMP-LIFNR = 'Vendor'.
  LT_TEMP-EKGRP = 'Pur Grp'.
  LT_TEMP-ERNAM = 'Created by'.
  LT_TEMP-AEDAT = 'Created on'.
  LT_TEMP-NAME1 = 'Vendor Name'.
  LT_TEMP-KTMNG = 'Target QTY'.
  LT_TEMP-GRQTY = 'GR QTY'.
  LT_TEMP-DIFFQTY = 'Different QTY'.
  LT_TEMP-RLVL = 'Risk Level'.
  LT_TEMP-ITEMTX = 'Remarks'.
  APPEND LT_TEMP.
  APPEND LT_TEMP TO IT_MAIL.
  CLEAR: LT_TEMP.
* Data

  LOOP AT IT_TAB.
    LT_TEMP-EBELN = IT_TAB-EBELN.
    LT_TEMP-MATNR = IT_TAB-MATNR.
    LT_TEMP-TXZ01 = IT_TAB-TXZ01.
    LT_TEMP-LIFNR = IT_TAB-LIFNR.
    LT_TEMP-EKGRP = IT_TAB-EKGRP.
    LT_TEMP-ERNAM = IT_TAB-ERNAM.
    LT_TEMP-AEDAT = IT_TAB-AEDAT.
    LT_TEMP-NAME1 = IT_TAB-NAME1.
    LT_TEMP-KTMNG = IT_TAB-KTMNG.
    LT_TEMP-GRQTY = IT_TAB-GRQTY.
    LT_TEMP-DIFFQTY = IT_TAB-DIFFQTY.
    LT_TEMP-RLVL = IT_TAB-RLVL.
    LT_TEMP-ITEMTX = IT_TAB-ITEMTX.

    APPEND LT_TEMP  TO IT_MAIL.
    CLEAR: IT_MAIL, LT_TEMP.
  ENDLOOP.
ENDFORM.                    " Compose_EMAIL_body
