*----------------------------------------------------------------------*
*   INCLUDE ZSJ_TEST029_C01                                            *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_C01                                        *
*----------------------------------------------------------------------*
INCLUDE ZRPP_COMMON_ALVC.
INCLUDE <ICON>.

*---------------------------------------------------------------------*
*FORM  CREATE_OBJECT
*---------------------------------------------------------------------*
*TEXT :
*---------------------------------------------------------------------*
FORM P1000_CREATE_OBJECT .

  IF G_DOCKING_CONTAINER IS INITIAL.
    CREATE OBJECT G_DOCKING_CONTAINER
      EXPORTING
        REPID     = GV_REPID
        DYNNR     = '0100'
        SIDE      = CL_GUI_DOCKING_CONTAINER=>DOCK_AT_BOTTOM
*        RATIO     = 90
        EXTENSION = 2000.

    CREATE OBJECT G_GRID
      EXPORTING
        I_PARENT = G_DOCKING_CONTAINER.

    GS_O_LAYOUT-REPORT = G_REPID_C = GV_REPID .
*    GS_O_LAYOUT-LOG_GROUP = 'HEAD'.
    CLEAR : GS_LAYOUT.

    PERFORM SET_INPUT_CON
            USING G_GRID ' ' '0'.



    PERFORM SET_SORT_C USING:
                  '1' 'WORD'  'X' '' '' '' '' '',
                  '2' 'EXTC'  'X' '' '' '' '' '',
                  '3' 'INTC'  'X' '' '' '' '' ''.


    GS_LAYOUT-STYLEFNAME = 'H_STYLE'.
*  GS_LAYOUT-ZEBRA      = 'X'.
* B:single C:multi D:cell A:rowcol
    GS_LAYOUT-SEL_MODE   = 'B'.
* ROW COLOR
*    GS_LAYOUT-INFO_FNAME = 'COLOR'.
* CELL COLOR
*    GS_LAYOUT-CTAB_FNAME = 'TABCOLOR'.
** BOX
    GS_LAYOUT-BOX_FNAME  = 'MARK'.
* OPTIMAZE
    GS_LAYOUT-CWIDTH_OPT = 'X'.
* Title
*    GS_LAYOUT-GRID_TITLE = TEXT-TT1 .

*  PERFORM SET_TOOLBAR.

    PERFORM P1010_SET_GRID_EVENTS
            USING G_GRID
                  'X'.

    DATA : LS_FCAT LIKE LINE OF GT_FIELDCAT .
    LOOP AT GT_FIELDCAT INTO LS_FCAT.
      LS_FCAT-REPTEXT  = LS_FCAT-FIELDNAME .
      MODIFY GT_FIELDCAT FROM LS_FCAT.
    ENDLOOP.

    PERFORM CALL_GRID_DISPLAY_FCAT
    TABLES <ITAB>  USING  G_GRID GT_FIELDCAT.

    "

  ELSE.

    CALL METHOD G_GRID->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = GT_FIELDCAT[].

    CALL METHOD G_GRID->SET_FRONTEND_LAYOUT
      EXPORTING
        IS_LAYOUT = GS_LAYOUT.
  ENDIF.

ENDFORM.                    "CREATE_OBJECT

*&---------------------------------------------------------------------*
*&      Form  P1010_SET_GRID_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_GRID  text
*      -->P_0097   text
*----------------------------------------------------------------------*
FORM P1010_SET_GRID_EVENTS
  USING P_GRID TYPE REF TO CL_GUI_ALV_GRID
           P_TOOLBAR.

  DATA : P_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET,
         P_ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL,
         PS_ROW_NO     TYPE LVC_S_ROID,
         PR_EVENT_DATA TYPE REF TO CL_ALV_EVENT_DATA,
         PT_BAD_CELLS  TYPE LVC_T_MODI.

  CREATE OBJECT G_EVENTS.

*****  이벤트 핸들러 등록
*_DOUBLE CLICK
*  SET HANDLER G_EVENTS->DOUBLE_CLICK FOR P_GRID.
*  PERFORM EVENT_DOUBLE_CLICK
*          USING '' '' ''.

* HOTSPOT
*  SET HANDLER G_EVENTS->HOTSPOT_CLICK FOR P_GRID.

*_DATA CHANGED
*  SET HANDLER G_EVENTS->DATA_CHANGED FOR P_GRID.
*  PERFORM EVENT_DATA_CHANGED
*          USING P_ER_DATA_CHANGED '' '' '' ''.

**_DATA CHANGED FINISHED
*  SET HANDLER G_EVENTS->DATA_CHANGED_FINISHED FOR P_GRID.
*  PERFORM EVENT_DATA_CHANGED_FINIS
*          USING ''.

*  SET HANDLER G_EVENTS->PRINT_TOP_OF_PAGE FOR P_GRID.

  CHECK NOT P_TOOLBAR IS INITIAL.
  SET HANDLER G_EVENTS->USER_COMMAND FOR P_GRID.
  PERFORM  EVENT_UCOMM
          USING ''
                '' .

  SET HANDLER G_EVENTS->TOOLBAR FOR P_GRID.
  PERFORM  EVENT_TOOLBAR
          USING P_OBJECT
                '' ''.

ENDFORM.                    " P1010_SET_GRID_EVENTS

*&---------------------------------------------------------------------*
*&      Form  EVENT_UCOMM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0243   text
*      -->P_0244   text
*----------------------------------------------------------------------*
FORM EVENT_UCOMM   USING   E_UCOMM LIKE SY-UCOMM
                                                  P_CHECK.
**---------------------------------------------------------------
*  CHECK P_CHECK EQ 'X'.
**
**---------------------------------------------------------------
*  CASE E_UCOMM.
**___재전송
*    WHEN '&CS03'.
*      PERFORM P3000_CALL_CS03.
*  ENDCASE.

ENDFORM.                    " P1020_EVENT_UCOMM
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_OBJECT  text
*      -->P_0254   text
*      -->P_0255   text
*----------------------------------------------------------------------*
FORM EVENT_TOOLBAR
   USING  E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
               E_INTERACTIVE TYPE C
               P_CHECK.

*---------------------------------------------------------------
  CHECK P_CHECK EQ 'X' .

*---------------------------------------------------------------
*_변경일때만 추가 버튼 삽임
*  CHECK S_CHANGE IS NOT INITIAL.

  DATA : LS_TOOLBAR  TYPE STB_BUTTON.

*_SET : BUTTON TYPE - SEPARATOR
  CLEAR : LS_TOOLBAR.
  LS_TOOLBAR-BUTN_TYPE = 3.
*  APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

  CLEAR LS_TOOLBAR.
*  LS_TOOLBAR-FUNCTION = '&MMBE'.
*  LS_TOOLBAR-ICON = ICON_BIW_REPORT.
*  LS_TOOLBAR-QUICKINFO = '재고조회'.
*  LS_TOOLBAR-TEXT = ' 재고 조회'.
*  APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

ENDFORM.                    " P1030_EVENT_TOOLBAR
*&--------------------------------------------------------------------*
*&      Form  EVENT_DOUBLE_CLICK
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_0201   text
*      -->P_0202   text
*      -->P_0203   text
*---------------------------------------------------------------------*
FORM EVENT_DOUBLE_CLICK  USING   E_ROW
                                 E_COLUMN
                                 ES_ROW_NO.
*  READ TABLE GT_DATA INDEX E_ROW.
*  IF SY-SUBRC = 0 .
*    PERFORM P5000_GET_ZVIN USING GT_DATA.
*  ELSE.
*    READ TABLE GT_OLD INDEX E_ROW.
*    IF SY-SUBRC = 0 .
*      PERFORM P5000_GET_ZVIN USING GT_OLD.
*    ENDIF.
*
*
*  ENDIF.

ENDFORM.                    " P1040_EVENT_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0225   text
*----------------------------------------------------------------------*
FORM EVENT_HOTSPOT_CLICK  USING E_ROW_ID E_COLUMN_ID.

*  READ TABLE GT_DATA INDEX E_ROW_ID .
*  CASE E_COLUMN_ID .
*    WHEN 'LIFNR'.
*      SET PARAMETER ID 'LIF' FIELD GT_DATA-LIFNR.
*      SET PARAMETER ID 'EKO' FIELD GT_DATA-EKORG.
*      CALL TRANSACTION 'MK03' AND SKIP FIRST SCREEN.
*  ENDCASE.

ENDFORM.                    " P1060_EVENT_DATA_CHANGED_FINIS

*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ER_DATA_CHANGED  text
*      -->P_0213   text
*      -->P_0214   text
*      -->P_0215   text
*      -->P_0216   text
*----------------------------------------------------------------------*
FORM EVENT_DATA_CHANGED  USING    P_DATA_CHANGED
                                        VALUE(P_0213)
                                        VALUE(P_0214)
                                        VALUE(P_0215)
                                        VALUE(P_0216).

ENDFORM.                    " P1050_EVENT_DATA_CHANGED

*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED_FINIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0225   text
*----------------------------------------------------------------------*
FORM EVENT_DATA_CHANGED_FINIS  USING    VALUE(P_0225).

ENDFORM.                    " P1060_EVENT_DATA_CHANGED_FINIS

*&---------------------------------------------------------------------*
*&      Form  GET_FILEDCAT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM GET_FILEDCAT_ALV  USING GT_FIELDCAT TYPE LVC_T_FCAT.

  DATA : LS_FIELDCAT TYPE LVC_S_FCAT.
  DATA : LT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
         L_FIELDCAT  TYPE SLIS_FIELDCAT_ALV.

  CLEAR : LT_FIELDCAT ,LT_FIELDCAT[].

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = GV_REPID "'ZRPP_HMA_ZPODER'
      I_INTERNAL_TABNAME     = 'GT_DATA'
*      I_BYPASSING_BUFFER     = 'X'
      I_INCLNAME             = GV_REPID "'ZRPP_HMA_ZPODER'
    CHANGING
      CT_FIELDCAT            = LT_FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  CLEAR : GT_FIELDCAT , GT_FIELDCAT[].

  LOOP AT LT_FIELDCAT INTO L_FIELDCAT.
    CLEAR : LS_FIELDCAT.
    MOVE-CORRESPONDING L_FIELDCAT TO LS_FIELDCAT.

    LS_FIELDCAT-REPTEXT   = L_FIELDCAT-SELTEXT_S.
    LS_FIELDCAT-REF_TABLE = L_FIELDCAT-REF_TABNAME.
    LS_FIELDCAT-KEY       = SPACE.

*    IF  GV_NEW EQ 'X'.
*      CASE LS_FIELDCAT-FIELDNAME.
*        WHEN 'POYEAR'   . LS_FIELDCAT-NO_OUT = 'X'.
*        WHEN 'POMONTH'  . LS_FIELDCAT-NO_OUT = 'X'.
*        WHEN 'ORDQTY'   . LS_FIELDCAT-NO_OUT = 'X'.
*        WHEN 'NEWQTY'   . LS_FIELDCAT-NO_OUT = 'X'.
*        WHEN 'ZSDAT'    . LS_FIELDCAT-NO_OUT = 'X'.
*        WHEN 'ZSTIM'	   . LS_FIELDCAT-NO_OUT = 'X'.
*        WHEN 'ZUSER'	   . LS_FIELDCAT-NO_OUT = 'X'.
*      ENDCASE.
*    ELSE.
*      CASE LS_FIELDCAT-FIELDNAME.
*        WHEN 'DEST'.    LS_FIELDCAT-NO_OUT = 'X'.
*        WHEN 'MOYE'.    LS_FIELDCAT-NO_OUT = 'X'.
*
*        WHEN 'VERS'.    LS_FIELDCAT-NO_OUT = 'X'.
*        WHEN 'INITQTY'. LS_FIELDCAT-NO_OUT = 'X'.
*        WHEN 'MODQTY '. LS_FIELDCAT-NO_OUT = 'X'.
*        WHEN 'LCNO'.    LS_FIELDCAT-NO_OUT = 'X'.
*        WHEN 'LCNT'.    LS_FIELDCAT-NO_OUT = 'X'.
*
*        WHEN 'REQ_DATE'. LS_FIELDCAT-NO_OUT = 'X'.
*        WHEN 'CRT_DATE'. LS_FIELDCAT-NO_OUT = 'X'.
*        WHEN 'CHG_DATE'. LS_FIELDCAT-NO_OUT = 'X'.
*
*      ENDCASE.
*    ENDIF.

    CASE LS_FIELDCAT-FIELDNAME.

      WHEN 'STATUS'.
        LS_FIELDCAT-KEY  = 'X'.
        LS_FIELDCAT-FIX_COLUMN = 'X'.
        LS_FIELDCAT-OUTPUTLEN = 5.
        LS_FIELDCAT-COL_POS = 0.
      WHEN 'DOCNUM'.
        LS_FIELDCAT-KEY  = 'X'.
      WHEN 'UPDDAT'.
        LS_FIELDCAT-KEY  = 'X'.
      WHEN 'UPDTIM'.
        LS_FIELDCAT-KEY  = 'X'.
      WHEN 'WO_SER'.
        LS_FIELDCAT-EMPHASIZE = 'C210'.
*        LS_FIELDCAT-KEY  = 'X'.
        LS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN 'NATION'.
        LS_FIELDCAT-EMPHASIZE = 'C210'.
*        LS_FIELDCAT-KEY  = 'X'.
        LS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN 'DEALER'.
        LS_FIELDCAT-EMPHASIZE = 'C210'.
*        LS_FIELDCAT-KEY  = 'X'.
        LS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN 'EXTC'  .
        LS_FIELDCAT-EMPHASIZE = 'C210'.
*        LS_FIELDCAT-KEY  = 'X'.
        LS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN 'INTC'  .
        LS_FIELDCAT-EMPHASIZE = 'C210'.
*        LS_FIELDCAT-KEY  = 'X'.
        LS_FIELDCAT-FIX_COLUMN = 'X'.


      WHEN 'INITQTY'.
        LS_FIELDCAT-REPTEXT = 'Init.Qty'.
      WHEN 'MODQTY'.
        LS_FIELDCAT-REPTEXT = 'Mod.Qty'.
    ENDCASE.

    APPEND LS_FIELDCAT TO GT_FIELDCAT.

  ENDLOOP.

ENDFORM.                    " GET_FILEDCAT_ALV
*&---------------------------------------------------------------------*
*&      Form  P1200_CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_POS[]  text
*      -->P_G_GRID  text
*----------------------------------------------------------------------*
FORM P1200_CALL_GRID_DISPLAY  TABLES   P_TABLE
                          USING    P_GRID TYPE REF TO CL_GUI_ALV_GRID  .

  CALL METHOD P_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_SAVE               = 'A'
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT
      I_STRUCTURE_NAME     = 'GT_DATA'
      IS_VARIANT           = GS_O_LAYOUT
      IT_TOOLBAR_EXCLUDING = GT_EXCL_FUNC
    CHANGING
      IT_FIELDCATALOG      = GT_FIELDCAT
      IT_SORT              = GT_SORT
      IT_OUTTAB            = P_TABLE[].

ENDFORM.                    " P1200_CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CALL_GRID_DISPLAY_OLD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_DATA[]  text
*----------------------------------------------------------------------*
FORM CALL_GRID_DISPLAY_OLD .
  DATA: L_STRUCT    LIKE DD02L-TABNAME.

  L_STRUCT = 'GT_DATA'.
*-----> SET OBJECT
*  CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*    EXPORTING
*      I_STRUCTURE_NAME              = L_STRUCT
*      IS_VARIANT                    = GS_O_LAYOUT
*      I_SAVE                        = 'A'
*      IS_LAYOUT                     = GS_LAYOUT
*    CHANGING
*      IT_OUTTAB                     = GT_DATA[]
*      IT_SORT                       = GT_SORT
*      IT_FIELDCATALOG               = GT_FIELDCAT[]
*    EXCEPTIONS
*      INVALID_PARAMETER_COMBINATION = 1
*      PROGRAM_ERROR                 = 2
*      TOO_MANY_LINES                = 3
*      OTHERS                        = 4 .

ENDFORM.                    " CALL_GRID_DISPLAY_OLD
