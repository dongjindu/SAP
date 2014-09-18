*&---------------------------------------------------------------------*
*& Report  ZMMRINBOUND                                                 *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZMMRINBOUND                   .

TABLES : LIKP, LIPS, LIKPUK, VEKP.

DATA : BEGIN OF GT_ITAB OCCURS 0,
       TRAID LIKE LIKP-TRAID , "CONTAINER
       LIFEX LIKE LIKP-LIFEX ,
       LFDAT LIKE LIKP-LFDAT ,
       TRATY LIKE LIKP-TRATY ,
       COUNT TYPE I,
       BOLNR LIKE LIKP-BOLNR ,
       VHILM LIKE VEKP-VHILM ,
       VBELN LIKE LIKP-VBELN ,
       KDMAT LIKE LIPS-KDMAT ,
       LFIMG LIKE LIPS-LFIMG ,
       MATNR LIKE LIPS-MATNR ,
       MEINS LIKE LIPS-MEINS ,
       VGBEL LIKE LIPS-VGBEL ,
       VGPOS LIKE LIPS-VGPOS ,
       VRKME LIKE LIPS-VRKME ,
       LVSTK LIKE LIKPUK-LVSTK,
       WBSTK LIKE LIKPUK-WBSTK,
       END OF GT_ITAB.

DATA : GT_ITAB01 LIKE GT_ITAB OCCURS 0 WITH HEADER LINE.

DATA : OK_CODE LIKE SY-UCOMM.

DATA : G_CUSTOM_CONTAINER1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       G_CUSTOM_CONTAINER2 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       G_CONTAINER1        TYPE SCRFNAME VALUE 'CON1',
       G_GRID1             TYPE REF TO CL_GUI_ALV_GRID,
       IT_FIELDCATALOG     TYPE LVC_T_FCAT,
       GS_FCATLAYO         TYPE LVC_S_LAYO,
       gt_exclude          TYPE ui_functions.


SELECTION-SCREEN: BEGIN OF BLOCK DIRECT
                  WITH FRAME TITLE TEXT-F59.

SELECT-OPTIONS S_TRAID FOR LIKP-TRAID.
SELECT-OPTIONS S_LIFNR FOR LIKP-LIFNR MEMORY ID LIF.
SELECT-OPTIONS S_LVSTK FOR LIKPUK-LVSTK.
SELECT-OPTIONS S_WBSTK FOR LIKPUK-WBSTK.
SELECT-OPTIONS S_LFART FOR LIKP-LFART.

SELECTION-SCREEN: END OF BLOCK DIRECT.

START-OF-SELECTION.

  DATA : LV_CNT TYPE I.
  CLEAR : GT_ITAB, GT_ITAB[],
          GT_ITAB01, GT_ITAB01[].


  SELECT LIKP~LFDAT
         LIKP~LIFNR
         LIKP~TRAID
         LIKP~TRATY
         LIKP~VBELN
         LIKP~LIFEX
         LIKP~BOLNR
         LIPS~KDMAT
         LIPS~LFIMG
         LIPS~MATNR
         LIPS~MEINS
         LIPS~VGBEL
         LIPS~VGPOS
         LIPS~VRKME
         LIKPUK~LVSTK
         LIKPUK~WBSTK

  INTO CORRESPONDING FIELDS OF TABLE GT_ITAB01
  FROM LIKP INNER JOIN LIPS
                    ON LIPS~VBELN = LIKP~VBELN
            INNER JOIN LIKPUK
                    ON LIKPUK~VBELN = LIKP~VBELN
 WHERE LIKP~LIFNR IN S_LIFNR
   AND LIKP~TRAID IN S_TRAID
   AND LIKP~LFART IN S_LFART
   AND LIKPUK~LVSTK IN S_LVSTK
   AND LIKPUK~WBSTK IN S_WBSTK.


  IF SY-SUBRC = 0.

    DELETE ADJACENT DUPLICATES FROM GT_ITAB01 COMPARING ALL FIELDS.

    SORT GT_ITAB01 BY LFDAT LIFEX TRATY TRAID.

    LOOP AT GT_ITAB01.
      MOVE-CORRESPONDING GT_ITAB01 TO GT_ITAB.

      AT NEW TRAID.
        CLEAR LV_CNT.
      ENDAT.

      LV_CNT = LV_CNT + 1.
      GT_ITAB-COUNT = LV_CNT.
      GT_ITAB-VHILM = 'CASE'.
      IF LV_CNT NE 1.
        CLEAR : GT_ITAB-LFDAT,
                GT_ITAB-LIFEX,
                GT_ITAB-TRAID,
                GT_ITAB-TRATY.
      ENDIF.

      APPEND GT_ITAB.

    ENDLOOP.

    CALL SCREEN 100.

  ENDIF.
*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET TITLEBAR  '100'.
  SET PF-STATUS '100'.
ENDMODULE.                 " status_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  init_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT_SCREEN OUTPUT.

  IF G_CUSTOM_CONTAINER1 IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER1
      EXPORTING
        CONTAINER_NAME = G_CONTAINER1.

    CREATE OBJECT G_GRID1
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER1.

    PERFORM ALV_CONTENT.

    CALL METHOD G_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = GS_FCATLAYO
*      IT_TOOLBAR_EXCLUDING = GT_EXCLUDE
    CHANGING
      IT_OUTTAB            = GT_ITAB[]
      IT_FIELDCATALOG      = IT_FIELDCATALOG.
*      IT_SORT              = GT_SORT.
  ELSE.
    PERFORM GUI_ALV_REFRESH.
  ENDIF.


ENDMODULE.                 " init_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ALV_CONTENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_CONTENT.
**** layout ****
  CLEAR GS_FCATLAYO.
  GS_FCATLAYO-ZEBRA                = 'X'.
  GS_FCATLAYO-BOX_FNAME            = 'MARK'.
  GS_FCATLAYO-SEL_MODE             = 'A'.
*  GS_FCATLAYO-NO_TOOLBAR           = 'X'.
  GS_FCATLAYO-CWIDTH_OPT           = 'X'.
  GS_FCATLAYO-STYLEFNAME           = 'CELLTAB'.
*  GS_FCATLAYO-EDIT                 = 'X'.

**** FIELD CATALOG ****
  DATA : COL_POS TYPE I.
  DATA : L_FIELDCATALOG  TYPE LVC_S_FCAT.
  DATA : L_TXT(20).
  REFRESH IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'LFDAT'.
  L_FIELDCATALOG-REPTEXT           = 'Deliv.date'.
  L_FIELDCATALOG-KEY               = 'X'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'LIFEX'.
  L_FIELDCATALOG-REPTEXT           = 'Original document'.
  L_FIELDCATALOG-KEY               = 'X'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'TRATY'.
  L_FIELDCATALOG-REPTEXT           = 'TrpT'.
  L_FIELDCATALOG-KEY               = 'X'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'TRAID'.
  L_FIELDCATALOG-REPTEXT           = 'Container Number'.
  L_FIELDCATALOG-KEY               = 'X'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'COUNT'.
  L_FIELDCATALOG-REPTEXT           = 'Item Number'.
  L_FIELDCATALOG-KEY               = 'X'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'KDMAT'.
  L_FIELDCATALOG-REPTEXT           = 'Customer material number'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'VHILM'.
  L_FIELDCATALOG-REPTEXT           = 'Packaging Materials'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'MATNR'.
  L_FIELDCATALOG-REPTEXT           = 'Material number'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS             = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'MEINS'.
  L_FIELDCATALOG-REPTEXT           = 'BUN'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'LFIMG'.
  L_FIELDCATALOG-QFIELDNAME        = 'MEINS'.
  L_FIELDCATALOG-REPTEXT           = 'Delivery quantity'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'VGBEL'.
  L_FIELDCATALOG-REPTEXT           = 'Ref.Doc'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'VGPOS'.
  L_FIELDCATALOG-REPTEXT           = 'Ref.Item'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'BOLNR'.
  L_FIELDCATALOG-REPTEXT           = 'Invoice'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'VBELN'.
  L_FIELDCATALOG-REPTEXT           = 'Delivery'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

ENDFORM.                    " ALV_CONTENT
*&---------------------------------------------------------------------*
*&      Form  GUI_ALV_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GUI_ALV_REFRESH.
  CALL METHOD G_GRID1->REFRESH_TABLE_DISPLAY
    EXPORTING
      I_SOFT_REFRESH = 'X'.

ENDFORM.                    " GUI_ALV_REFRESH
*&---------------------------------------------------------------------*
*&      Module  exit_commnad  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_COMMNAD INPUT.
  CASE OK_CODE.
    WHEN 'EXIT' OR 'CANC'.
      LEAVE PROGRAM.
    WHEN 'BACK' .
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit_commnad  INPUT
