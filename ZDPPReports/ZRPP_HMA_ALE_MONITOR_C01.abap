*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_C01                                        *
*----------------------------------------------------------------------*
INCLUDE ZRPP_COMMON_ALVC.


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

*    PERFORM SET_SORT_C USING:
*                  '1' 'WO_SER'  'X' '' '' '' '' '',
*                  '2' 'NATION'  'X' '' '' '' '' '',
*                  '3' 'DEALER'  'X' '' '' '' '' '',
*                   '4' 'EXTC'  'X' '' '' '' '' '',
*                   '5' 'INTC '  'X' '' '' '' '' ''.


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

*    PERFORM GET_FILEDCAT_ALV  USING GT_FIELDCAT[].
*    PERFORM CALL_GRID_DISPLAY_OLD.
*    PERFORM CALL_GRID_DISPLAY  TABLES GT_DATA[]  USING  G_GRID.

    "
    PERFORM CALL_GRID_DISPLAY_DYN
    USING  G_GRID.
  ELSE.

    CALL METHOD G_GRID->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = LT_FCAT[].

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

*****  ÀÌº¥Æ® ÇÚµé·¯ µî·Ï
*_DOUBLE CLICK
  SET HANDLER G_EVENTS->DOUBLE_CLICK FOR P_GRID.
*  PERFORM EVENT_DOUBLE_CLICK
*          USING '' '' ''.

* HOTSPOT
  SET HANDLER G_EVENTS->HOTSPOT_CLICK FOR P_GRID.

*_DATA CHANGED
  SET HANDLER G_EVENTS->DATA_CHANGED FOR P_GRID.
  PERFORM EVENT_DATA_CHANGED
          USING P_ER_DATA_CHANGED '' '' '' ''.

*_DATA CHANGED FINISHED
  SET HANDLER G_EVENTS->DATA_CHANGED_FINISHED FOR P_GRID.
  PERFORM EVENT_DATA_CHANGED_FINIS
          USING ''.

  SET HANDLER G_EVENTS->PRINT_TOP_OF_PAGE FOR P_GRID.

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
**___ÀçÀü¼Û
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
*_º¯°æÀÏ¶§¸¸ Ãß°¡ ¹öÆ° »ðÀÓ
*  CHECK S_CHANGE IS NOT INITIAL.

  DATA : LS_TOOLBAR  TYPE STB_BUTTON.

*_SET : BUTTON TYPE - SEPARATOR
  CLEAR : LS_TOOLBAR.
  LS_TOOLBAR-BUTN_TYPE = 3.
*  APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

  CLEAR LS_TOOLBAR.
*  LS_TOOLBAR-FUNCTION = '&MMBE'.
*  LS_TOOLBAR-ICON = ICON_BIW_REPORT.
*  LS_TOOLBAR-QUICKINFO = 'Àç°íÁ¶È¸'.
*  LS_TOOLBAR-TEXT = ' Àç°í Á¶È¸'.
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
FORM EVENT_DOUBLE_CLICK  USING    VALUE(P_0201)
                                             VALUE(P_0202)
                                             VALUE(P_0203).

ENDFORM.                    " P1040_EVENT_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0225   text
*----------------------------------------------------------------------*
FORM EVENT_HOTSPOT_CLICK  USING E_ROW_ID E_COLUMN_ID.

  "READ TABLE GT_DATA INDEX E_ROW_ID .
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



  ENDLOOP.

ENDFORM.                    " GET_FILEDCAT_ALV

*---------------------------------------------------------------------*
*       FORM CALL_GRID_DISPLAY_DYN                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_GRID                                                        *
*---------------------------------------------------------------------*
FORM CALL_GRID_DISPLAY_DYN
                          USING    P_GRID TYPE REF TO CL_GUI_ALV_GRID  .


  CALL METHOD P_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_SAVE               = 'A'
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT
      IS_VARIANT           = GS_O_LAYOUT
      IT_TOOLBAR_EXCLUDING = GT_EXCL_FUNC
    CHANGING
      IT_FIELDCATALOG      = LT_FCAT "GT_FIELDCAT
      IT_SORT              = GT_SORT
      IT_OUTTAB            =  <NEW_TAB>.

ENDFORM.                    " CALL_GRID_DISPLAY
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
  CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_STRUCTURE_NAME              = L_STRUCT
      IS_VARIANT                    = GS_O_LAYOUT
      I_SAVE                        = 'A'
      IS_LAYOUT                     = GS_LAYOUT
    CHANGING
      IT_OUTTAB                     = GT_DATA[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4 .

ENDFORM.                    " CALL_GRID_DISPLAY_OLD
*&---------------------------------------------------------------------*
*&      Form  P1100_CREATE_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1100_CREATE_OBJECT.
  DATA AI TYPE BDMOD1TP.
  DATA: FIELDCAT TYPE LVC_T_FCAT, FC TYPE LVC_S_FCAT.
  DATA  LAYOUT TYPE LVC_S_LAYO.

* Prepare Control
  SET PF-STATUS 'S200'.

  IF G_ALV_CONTAINER  IS INITIAL.

    CREATE OBJECT    G_ALV_CONTAINER
    EXPORTING CONTAINER_NAME = 'CUST_200'.

  ENDIF.

  IF G_GRID1 IS INITIAL.
    CREATE OBJECT G_GRID1
           EXPORTING I_PARENT = G_ALV_CONTAINER .

  ENDIF.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME       = 'BDMOD1TP'
       CHANGING
            CT_FIELDCAT            = FIELDCAT
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.
  CHECK SY-SUBRC = 0.

* Fill Fieldcatalog
  LOOP AT FIELDCAT INTO FC.
    IF FC-FIELDNAME = 'DOCNUM'.
      FC-COL_POS = 1.
      FC-OUTPUTLEN = 16.
      FC-KEY = 'X'.
    ELSEIF FC-FIELDNAME = 'STATUS'.
      FC-COL_POS = 2.
      FC-OUTPUTLEN = 4.
      FC-JUST = 'C'.
    ELSEIF FC-FIELDNAME = 'DOCNUM_PR'.
      FC-COLTEXT = 'IDoc-Nr. Partner'(017).
      FC-SELTEXT = 'IDoc-Nr. Partner'(017).
      FC-EMPHASIZE = 'C1'.
      FC-COL_POS = 3.
      FC-OUTPUTLEN = 16.
*      LOOP AT AUD_IDOC_TAB INTO AI WHERE NOT DOCNUM_PR IS INITIAL.
*      ENDLOOP.
*      IF SY-SUBRC <> 0.
*        FC-NO_OUT = 'X'.
*        FC-TECH = 'X'.
*      ENDIF.
    ELSEIF FC-FIELDNAME = 'STATUS_PR'.
      FC-EMPHASIZE = 'C1'.
      FC-COL_POS = 4.
      FC-OUTPUTLEN = 4.
      FC-JUST = 'C'.
      LOOP AT AUD_IDOC_TAB INTO AI WHERE NOT DOCNUM_PR IS INITIAL.
      ENDLOOP.
      IF SY-SUBRC <> 0.
        FC-NO_OUT = 'X'.
        FC-TECH = 'X'.
      ENDIF.
    ELSEIF FC-FIELDNAME = 'MESTYP'.
      FC-COL_POS = 5.
      FC-OUTPUTLEN = 15.
    ELSEIF FC-FIELDNAME = 'MESCOD'.
      FC-COL_POS = 6.
      FC-OUTPUTLEN = 5.
      LOOP AT AUD_IDOC_TAB INTO AI WHERE NOT MESCOD IS INITIAL.
      ENDLOOP.
      IF SY-SUBRC <> 0.
        FC-NO_OUT = 'X'.
      ENDIF.
    ELSEIF FC-FIELDNAME = 'MESFCT'.
      FC-COL_POS = 7.
      FC-OUTPUTLEN = 5.
      LOOP AT AUD_IDOC_TAB INTO AI WHERE NOT MESFCT IS INITIAL.
      ENDLOOP.
      IF SY-SUBRC <> 0.
        FC-NO_OUT = 'X'.
      ENDIF.
    ELSEIF FC-FIELDNAME = 'OBJNAME'.
      FC-COL_POS = 8.
      FC-OUTPUTLEN = 16.
      LOOP AT AUD_IDOC_TAB INTO AI WHERE NOT OBJNAME IS INITIAL.
      ENDLOOP.
      IF SY-SUBRC <> 0.
        FC-NO_OUT = 'X'.
      ENDIF.
      FC-COLTEXT = 'Objekttyp'(016).
      FC-SELTEXT = 'Objekttyp'(016).
    ELSEIF FC-FIELDNAME = 'METHODNAME'.
      FC-COL_POS = 9.
      FC-OUTPUTLEN = 16.
      LOOP AT AUD_IDOC_TAB INTO AI WHERE NOT METHODNAME IS INITIAL.
      ENDLOOP.
      IF SY-SUBRC <> 0.
        FC-NO_OUT = 'X'.
        FC-TECH = 'X'.
      ENDIF.
      FC-COLTEXT = 'Methode'(021).
      FC-SELTEXT = 'Methode'(021).
    ELSEIF FC-FIELDNAME = 'OBJKEY'.
      FC-COL_POS = 10.
      LOOP AT AUD_IDOC_TAB INTO AI WHERE NOT OBJKEY IS INITIAL.
      ENDLOOP.
      IF SY-SUBRC <> 0.
        FC-NO_OUT = 'X'.
        FC-TECH = 'X'.
      ENDIF.
      FC-OUTPUTLEN = 25.
      FC-COLTEXT = 'Objektschlüssel'(022).
      FC-SELTEXT = 'Objektschlüssel'(022).
    ELSEIF FC-FIELDNAME = 'STATXT'.
      FC-COL_POS = 11.
      FC-OUTPUTLEN = 50.
    ELSEIF FC-FIELDNAME = 'PARTNER'.
      FC-COL_POS = 12.
    ELSEIF FC-FIELDNAME = 'PARPRT'.
      FC-COL_POS = 13.
      FC-OUTPUTLEN = 5.
      FC-NO_OUT = 'X'.
    ELSEIF FC-FIELDNAME = 'PARPFC'.
      FC-COL_POS = 14.
      FC-OUTPUTLEN = 5.
      FC-NO_OUT = 'X'.
    ELSEIF FC-FIELDNAME = 'CREDAT'.
      FC-COL_POS = 15.
    ELSEIF FC-FIELDNAME = 'CRETIM'.
      FC-COL_POS = 16.
    ELSEIF FC-FIELDNAME = 'IDOCTP'.
      FC-COL_POS = 17.
      FC-OUTPUTLEN = 15.
    ELSEIF FC-FIELDNAME = 'CIMTYP'.
      FC-COL_POS = 18.
      LOOP AT AUD_IDOC_TAB INTO AI WHERE NOT CIMTYP IS INITIAL.
      ENDLOOP.
      IF SY-SUBRC <> 0.
        FC-NO_OUT = 'X'.
      ENDIF.
      FC-OUTPUTLEN = 15.
    ELSEIF FC-FIELDNAME = 'MAXSEGNUM'.
      FC-COL_POS = 19.
      FC-OUTPUTLEN = 7.
    ELSEIF FC-FIELDNAME = 'UPDDAT'.
      FC-COL_POS = 20.
      FC-NO_OUT = 'X'.
    ELSEIF FC-FIELDNAME = 'UPDTIM'.
      FC-COL_POS = 21.
      FC-NO_OUT = 'X'.
    ELSEIF FC-FIELDNAME = 'TEST'.
      FC-COL_POS = 22.
      FC-OUTPUTLEN = 4.
      FC-NO_OUT = 'X'.
    ENDIF.

    MODIFY FIELDCAT FROM FC.
  ENDLOOP.

  LAYOUT-ZEBRA = 'X'.
  LAYOUT-GRID_TITLE = 'IDoc-Auswahl'(019).
*  layout-NO_HGRIDLN = 'X'.
*  layout-NO_vGRIDLN = 'X'.
  LAYOUT-NO_ROWMARK = 'X'.
  LAYOUT-SEL_MODE = 'D'.

  CALL METHOD G_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
       EXPORTING I_STRUCTURE_NAME = 'BDMOD1TP'
                 IS_LAYOUT        = LAYOUT
       CHANGING  IT_OUTTAB        = AUD_IDOC_TAB
                 IT_FIELDCATALOG  = FIELDCAT.

  CALL METHOD CL_GUI_CFW=>FLUSH.


ENDFORM.                    " P1100_CREATE_OBJECT
