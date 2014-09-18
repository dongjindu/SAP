************************************************************************
* Program Name      : SAPMZAPM100_MAIN
* Author            : Myoungho, Park
* Creation Date     : 2003.08.21.
* Specifications By : Myoungho, Park
* Development Request No :
* Addl Documentation:
* Description       : PM Main menu
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  SAPMZAPM100_MAIN   MESSAGE-ID ZMPM.

TABLES : T001.         "//COMPANY

DATA : IT_T001 LIKE T001 OCCURS 0 WITH HEADER LINE.

DATA: IT_LAST_FUNC LIKE TABLE OF ZSPM_FUNC_LOC
                                       WITH HEADER LINE.

DATA: IT_EQUI_LIST LIKE TABLE OF ZSPM_EQUI_BY_FUNC
                                      WITH HEADER LINE,
      IT_EQUI_LIST_TMP LIKE IT_EQUI_LIST OCCURS 0.

DATA: BEGIN OF IT_EQUIPMENT OCCURS 0,
        NODE LIKE NODE_STR-NODE_KEY,
        ITEM LIKE IFLOT-TPLNR,
      END OF IT_EQUIPMENT.

DATA: BEGIN OF IT_WERKS OCCURS 0,
        WERKS LIKE T001W-WERKS,
        NAME1 LIKE T001W-NAME1,
      END OF IT_WERKS.

* Control Framework Basic Class
CLASS CL_GUI_CFW      DEFINITION LOAD.
CLASS LCL_APPLICATION DEFINITION DEFERRED.

* Local Class
DATA: WA_APPLICATION TYPE REF TO LCL_APPLICATION.

* Docking Container
DATA: DOCKING_LEFT TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      DOCKING_TOP  TYPE REF TO CL_GUI_DOCKING_CONTAINER.

*---  Global Flg
DATA: WA_INIT_FLG,
      WA_FREE_FLG.

DATA: WA_TOGGLE(4) VALUE 'MHRC',     "Menu Toggle
      WA_CHOICE(4) VALUE 'MHRC'.     "exception creating Container

DATA: WA_REPID TYPE SY-REPID,        "current main program
      WA_DYNNR TYPE SY-DYNNR.        "current screen number

DATA: WA_MTART LIKE MARA-MTART,      "Material Type
      WA_EQART LIKE EQUI-EQART.      "Type of Technical Object


*Tree by Location - For Functiion Location
DATA: FUNC_TREE      TYPE REF TO CL_GUI_SIMPLE_TREE,
      FUNC_NODE_ITAB LIKE NODE_STR OCCURS 0,
      FUNC_NODE      LIKE NODE_STR.
DATA: FUNC_NODE_ITAB2 LIKE NODE_STR OCCURS 0.

*Tree By Location - For Equipment List
TYPES: ITEM_TABLE_TYPE LIKE STANDARD TABLE OF MTREEITM
                                     WITH DEFAULT KEY.

* Picture
DATA: PICTURE TYPE REF TO CL_GUI_PICTURE,
      PIC_URL(255).

DATA: EQUI_TREE      TYPE REF TO CL_GUI_COLUMN_TREE,
      EQUI_NODE_ITAB TYPE TREEV_NTAB,
      EQUI_NODE      TYPE TREEV_NODE,
      EQUI_ITEM_ITAB TYPE ITEM_TABLE_TYPE,
      EQUI_ITEM      TYPE MTREEITM.

*Tree By Equipment - For Equipment List
DATA: EQUI_NODE_ITAB_2 TYPE TREEV_NTAB,
      EQUI_ITEM_ITAB_2 TYPE ITEM_TABLE_TYPE.

*Node Property Internal Table.
DATA: ACCUM_NODE_ITAB   LIKE NODE_STR OCCURS 0,
      ACCUM_NODE_ITAB_2 LIKE NODE_STR OCCURS 0.

*-- Event Handler : Double Click & Context Menu
DATA: EVENTS      TYPE CNTL_SIMPLE_EVENTS,      "Table
      EVENT       TYPE CNTL_SIMPLE_EVENT,       "Structure
      EVENTS2     TYPE CNTL_SIMPLE_EVENTS,      "Table
      EVENT2      TYPE CNTL_SIMPLE_EVENT,       "Structure

****-- GLOBAL VARIABLES for Node info
      WA_NODE_KEY(20),                           "Selected Node
      WA_NODE_KEY_BACK(20),                      "Old Node for Back
      WA_TMP_NODE_KEY(20),                       "Backup Selected NODE
      WA_CTX_FCODE LIKE SY-UCOMM,                "Context Menu fcode
      WA_SIZE_OF_KEY TYPE I,

      WA_FIRST_ROOT(6),                          "Func Loc Root
      WA_LAST_ROOT(20),                          "Equim Root(기능위치)
      WA_LAST_TPLNR LIKE IFLOT-TPLNR.            "Equim Root(기능위치)

****-- GLOBAL VARIABLES for CONTEXT MENU
DATA : WA_CUR_CXT_MENU LIKE SY-PFKEY.             "//CURRENT
DATA : WA_CXT_MHRC LIKE SY-PFKEY VALUE 'CXTMHRC'. "//LOCATION
DATA : WA_CXT_EQUI LIKE SY-PFKEY VALUE 'CXT400'.  "//EQUIPMENT
DATA : WA_CXT_ERSA LIKE SY-PFKEY VALUE 'CXT200'.  "//MATERIAL
DATA : WA_CXT_IBAU LIKE SY-PFKEY VALUE 'CXT300'.  "//ASS'Y

DATA: IT_PFTAB TYPE TABLE OF UI_FUNC.             "For Control Menu
DATA: WA_FCODE TYPE UI_FUNC.                      "For Control Function
"key on CXT100 GUI STATUS
DATA: OK_CODE LIKE SY-UCOMM.

DATA: RG_LAST_ROOT LIKE RANGE OF WA_LAST_ROOT WITH HEADER LINE.

*-- Node Key & Func Code Table For Func Loc
DATA : BEGIN OF IT_FUNC_LIST_NODE OCCURS 0,
         NODE_KEY(12).
        INCLUDE STRUCTURE ZSPM_FUNC_LOC.
DATA : END OF IT_FUNC_LIST_NODE.

DATA: IT_FUNC_LIST LIKE TABLE OF ZSPM_FUNC_LOC WITH HEADER LINE.

DATA : BEGIN OF IT_BOM_LIST_NODE OCCURS 0,
        NODE_KEY(12).
        INCLUDE STRUCTURE ZSPM_BOM_HIRACHY.
DATA : END OF IT_BOM_LIST_NODE.

DATA: IT_BOM_LIST LIKE TABLE OF ZSPM_BOM_HIRACHY WITH HEADER LINE.

DATA : WA_TITLE(20) TYPE C.

DATA : RETURN LIKE BAPIRETURN.

*-- BDC data
DATA: BDC_TAB LIKE TABLE OF BDCDATA WITH HEADER LINE.
DATA: MSG_TAB LIKE TABLE OF BDCMSGCOLL.

CONSTANTS C_ALPHABET TYPE DATATYPE-CHAR0128
          VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

*************  INITIALIZATION *********************
***************************************************
INITIALIZATION.
  WA_TITLE = TEXT-TT1.

***************** INCLUDE **************************
****************************************************

  INCLUDE ZAPM09_CLASS_DEFINITION.

  INCLUDE ZAPM09_CLASS_IMPLEMENTATION.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  WA_TITLE = TEXT-TT1.

  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_OBJECT OUTPUT.
  IF WA_INIT_FLG IS INITIAL.
* Create Docking Container
    PERFORM CREATE_DOCKING_OBJECT USING WA_TOGGLE.

* Create Tree for Functional Location
    PERFORM CREATE_FUNC_TREE_OBJECT USING WA_TOGGLE.

  ENDIF.
ENDMODULE.                 " CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_DOCKING_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_TOGGLE  text
*----------------------------------------------------------------------*
FORM CREATE_DOCKING_OBJECT USING    P_TOGGLE.

  WA_REPID = SY-REPID.
  WA_DYNNR = SY-DYNNR.

* Create Docking Container for Functional Location
  CREATE OBJECT DOCKING_LEFT
                EXPORTING REPID     = WA_REPID
                          DYNNR     = WA_DYNNR
                          SIDE      = DOCKING_LEFT->DOCK_AT_LEFT
                          EXTENSION = 300.

  IF P_TOGGLE EQ 'MHRC'.
* Create Docking Container for Picture
    CREATE OBJECT DOCKING_TOP
                  EXPORTING REPID     = WA_REPID
                            DYNNR     = WA_DYNNR
                            SIDE      = DOCKING_TOP->DOCK_AT_TOP
                            EXTENSION = 150.

  ENDIF.

* Create Local Object
  CREATE OBJECT WA_APPLICATION.
ENDFORM.                    " CREATE_DOCKING_OBJECT
*&---------------------------------------------------------------------*
*&      Form  CREATE_FUNC_TREE_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_TOGGLE  text
*----------------------------------------------------------------------*
FORM CREATE_FUNC_TREE_OBJECT USING    P_TOGGLE.
  CREATE OBJECT FUNC_TREE
    EXPORTING PARENT = DOCKING_LEFT
              NODE_SELECTION_MODE = FUNC_TREE->NODE_SEL_MODE_MULTIPLE.

  PERFORM DEFINE_EVENT_DOUBLE_CLICK.

  PERFORM CREATE_TREE.

  IF P_TOGGLE EQ 'MHRC'.
    CALL METHOD FUNC_TREE->ADD_NODES
      EXPORTING NODE_TABLE = FUNC_NODE_ITAB
                TABLE_STRUCTURE_NAME = 'NODE_STR'.
  ELSE.
    CALL METHOD FUNC_TREE->ADD_NODES
      EXPORTING NODE_TABLE = FUNC_NODE_ITAB2
                TABLE_STRUCTURE_NAME = 'NODE_STR'.
  ENDIF.

  CALL METHOD FUNC_TREE->EXPAND_ROOT_NODES
    EXPORTING
      LEVEL_COUNT         = 1
      EXPAND_SUBTREE      = ' '
    EXCEPTIONS
      FAILED              = 1
      ILLEGAL_LEVEL_COUNT = 2
      CNTL_SYSTEM_ERROR   = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " CREATE_FUNC_TREE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  CHECK_NODE_N_NODE_BACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_NODE_N_NODE_BACK.
  DATA: LV_NODE_KEY(20).

  CHECK WA_NODE_KEY NE WA_NODE_KEY_BACK.

  WA_NODE_KEY_BACK = WA_NODE_KEY.

* Picture Object Free
  IF NOT PICTURE IS INITIAL.
    CALL METHOD PICTURE->FREE.
  ENDIF.

* Create Picture Object
  CREATE OBJECT PICTURE
         EXPORTING PARENT = DOCKING_TOP.

  WA_FREE_FLG = 'X'.
ENDFORM.                    " CHECK_NODE_N_NODE_BACK
*&---------------------------------------------------------------------*
*&      Form  CHK_AND_CHANGE_NODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_NODE_KEY  text
*----------------------------------------------------------------------*
FORM CHK_AND_CHANGE_NODE USING    P_NODE.
  READ TABLE IT_FUNC_LIST_NODE WITH KEY NODE_KEY = P_NODE.
  IF SY-SUBRC = 0.
    MOVE IT_FUNC_LIST_NODE-TPLNR TO P_NODE.
  ELSE.
    CLEAR P_NODE .
  ENDIF.
ENDFORM.                    " CHK_AND_CHANGE_NODE
*&---------------------------------------------------------------------*
*&      Form  CREATE_PICTURE_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_NODE_KEY  text
*----------------------------------------------------------------------*
FORM CREATE_PICTURE_OBJECT USING    P_NODE_KEY.
  DATA: LV_NODE_KEY(20).

  IF WA_FREE_FLG IS INITIAL.
* Picture Object Free
    IF NOT PICTURE IS INITIAL.
      CALL METHOD PICTURE->FREE.
    ENDIF.
*Create Picture Object
    CREATE OBJECT PICTURE
            EXPORTING PARENT = DOCKING_TOP.
  ENDIF.

* Find URL of Picture
  CONCATENATE 'Z_PM_' P_NODE_KEY INTO LV_NODE_KEY.

  CLEAR PIC_URL.
  PERFORM LOAD_PICTURE USING    LV_NODE_KEY
                       CHANGING PIC_URL.
* load picture
  CALL METHOD PICTURE->LOAD_PICTURE_FROM_URL
      EXPORTING URL = PIC_URL.

  CLEAR WA_FREE_FLG.
ENDFORM.                    " CREATE_PICTURE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  CREATE_EQUI_TREE_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_NODE_KEY  text
*----------------------------------------------------------------------*
FORM CREATE_EQUI_TREE_OBJECT USING    P_NODE_KEY.
  CLEAR:   EQUI_NODE_ITAB,   EQUI_ITEM_ITAB,
           EQUI_NODE_ITAB_2, EQUI_ITEM_ITAB_2.
  REFRESH: EQUI_NODE_ITAB,   EQUI_ITEM_ITAB,
           EQUI_NODE_ITAB_2, EQUI_ITEM_ITAB_2.

  CLEAR IT_LAST_FUNC.       REFRESH IT_LAST_FUNC.
  CLEAR IT_BOM_LIST_NODE.   REFRESH IT_BOM_LIST_NODE.

*-- Create Tree Object
  PERFORM CREATE_TREE_COLUMN USING WA_TOGGLE.

*-- Find Last Location.
  PERFORM GET_LAST_TPLNR USING P_NODE_KEY WA_TOGGLE.
*--
  PERFORM GET_EQUI_BY_FUNC USING WA_TOGGLE.

*--- Equiment/Material TREE
  IF WA_TOGGLE EQ 'MHRC'.
    CALL METHOD EQUI_TREE->ADD_NODES_AND_ITEMS
      EXPORTING
        NODE_TABLE = EQUI_NODE_ITAB
        ITEM_TABLE = EQUI_ITEM_ITAB
        ITEM_TABLE_STRUCTURE_NAME = 'MTREEITM'
      EXCEPTIONS
        FAILED = 1
        CNTL_SYSTEM_ERROR = 3
        ERROR_IN_TABLES = 4
        DP_ERROR = 5
        TABLE_STRUCTURE_NAME_NOT_FOUND = 6.
    IF SY-SUBRC <> 0.
      MESSAGE A000.
    ENDIF.
  ELSE.
    CALL METHOD EQUI_TREE->ADD_NODES_AND_ITEMS
      EXPORTING
        NODE_TABLE = EQUI_NODE_ITAB_2
        ITEM_TABLE = EQUI_ITEM_ITAB_2
        ITEM_TABLE_STRUCTURE_NAME = 'MTREEITM'
      EXCEPTIONS
        FAILED = 1
        CNTL_SYSTEM_ERROR = 3
        ERROR_IN_TABLES = 4
        DP_ERROR = 5
        TABLE_STRUCTURE_NAME_NOT_FOUND = 6.
    IF SY-SUBRC <> 0.
      MESSAGE A000.
    ENDIF.

  ENDIF.

  CALL METHOD EQUI_TREE->EXPAND_ROOT_NODES
     EXPORTING
       LEVEL_COUNT         = 0
       EXPAND_SUBTREE      = ' '
     EXCEPTIONS
       FAILED              = 1
       ILLEGAL_LEVEL_COUNT = 2
       CNTL_SYSTEM_ERROR   = 3
       OTHERS              = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CREATE_EQUI_TREE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  FREE_EQUI_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FREE_EQUI_TREE.
  CALL METHOD EQUI_TREE->FREE
    EXCEPTIONS
      CNTL_ERROR        = 1
      CNTL_SYSTEM_ERROR = 2.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

  CLEAR EQUI_TREE.
ENDFORM.                    " FREE_EQUI_TREE
*&---------------------------------------------------------------------*
*&      Form  HANDLE_NODE_CXT_MENU_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MENU  :CT_MENU Object
*      -->P_GV_CXT_MHRC  : GUI STATUS name(CXT_MENU name)
*----------------------------------------------------------------------*
FORM HANDLE_NODE_CXT_MENU_NEW USING P_MENU TYPE REF TO CL_CTMENU
                                    P_GV_CXT   LIKE SY-PFKEY.
*--Call METHOD Load CT_MENU
  CALL METHOD P_MENU->LOAD_GUI_STATUS
    EXPORTING  PROGRAM = SY-CPROG
               STATUS  = P_GV_CXT
               MENU    = P_MENU
    EXCEPTIONS READ_ERROR = 1.
  IF SY-SUBRC NE 0.
    MESSAGE A000.
  ENDIF.

  CALL METHOD P_MENU->HIDE_FUNCTIONS
    EXPORTING
      FCODES =  IT_PFTAB.
  REFRESH IT_PFTAB.
ENDFORM.                    " HANDLE_NODE_CXT_MENU_NEW
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_EQUI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FCODE  text
*      -->P_WA_NODE_KEY  text
*      -->P_WA_MTART  text
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION_EQUI USING P_FCODE
                                 P_NODE
                                 P_MTART.

  DATA: LV_TPLNR LIKE IFLOT-TPLNR,
        LV_EQUNR LIKE EQUI-EQUNR,
        LV_ASSY  LIKE MARA-MATNR,
        LV_MATNR LIKE MARA-MATNR.

  DATA: LV_MTART LIKE MARA-MTART.

  DATA: LRG_TPLNR LIKE RANGE OF IFLOT-TPLNR WITH HEADER LINE,
        LRG_EQUNR LIKE RANGE OF EQUI-EQUNR  WITH HEADER LINE.

  DATA: LV_ZBUKRS TYPE BUKRS,
        LV_WERKS  LIKE T001W-WERKS,
        LV_ZZYYMM LIKE SY-DATUM.

  DATA : LV_DATUM LIKE SY-DATUM.

  CLEAR  : LV_TPLNR, LV_EQUNR, LV_ASSY, LV_MTART, LRG_TPLNR, LRG_EQUNR.
  REFRESH: LRG_TPLNR, LRG_EQUNR.

  CLEAR  : LV_ZBUKRS, LV_WERKS, LV_ZZYYMM.

  LV_TPLNR = WA_LAST_TPLNR.

  CASE P_MTART.
*-- ASS'Y
    WHEN 'IBAU'.
      LV_ASSY = P_NODE.

*-- material
    WHEN 'ERSA'.
      LV_MATNR = P_NODE.

*-- equipment
    WHEN 'EQUI'.
      LV_EQUNR = P_NODE.

    WHEN OTHERS.
  ENDCASE.

  IF P_MTART = 'ERSA' OR P_MTART = 'IBAU'.
    PERFORM GET_SWERK USING WA_LAST_TPLNR LV_WERKS.
    IF LV_EQUNR IS INITIAL.
      MOVE IT_BOM_LIST_NODE-MATNR_PARENT TO LV_EQUNR.
    ENDIF.
  ELSE.
    PERFORM GET_SWERK USING LV_TPLNR LV_WERKS.
  ENDIF.


*************** Function ************************
*************************************************
  CASE P_FCODE.
*-- Upload drawing
    WHEN 'UPLDN'.
      PERFORM MANAGE_IMAGE USING P_NODE 'X'.

*--- Delete drawing
    WHEN 'DELEN'.
      PERFORM MANAGE_IMAGE USING P_NODE ' '.
      PERFORM CREATE_PICTURE_OBJECT USING P_NODE.

*-- view drawing
    WHEN 'DISPN'.
      PERFORM CREATE_PICTURE_OBJECT USING P_NODE.

*-- Create Notificaion
    WHEN 'IW21'.
      CALL TRANSACTION P_FCODE.

*-- Change Notification
    WHEN 'IW22'.
      CALL TRANSACTION P_FCODE.

*--Display Notification
    WHEN 'IW23'.
      CALL TRANSACTION P_FCODE.

*-- Malfunction Report
    WHEN 'IW24'.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPLIQS0'         '7200',
        ' '    'VIQMEL-QMTXT'     '',
        ' '    'RIWO1-TPLNR'      LV_TPLNR,
        ' '    'RIWO1-EQUNR'      LV_EQUNR.
      IF P_MTART = 'IBAU'.
        PERFORM GENERATE_BDC_DATA USING:
          ' '    'RIWO1-BAUTL'      LV_ASSY.
      ELSEIF P_MTART = 'ERSA'.
        PERFORM GENERATE_BDC_DATA USING:
          ' '    'RIWO1-BAUTL'      LV_MATNR.
      ENDIF.
      PERFORM GENERATE_BDC_DATA USING:
        ' '    'BDC_CURSOR'      'VIQMEL-QMTXT'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Maintenance Request
    WHEN 'IW26'.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPLIQS0'         '7200',
        ' '    'VIQMEL-QMTXT'     '',
        ' '    'RIWO1-TPLNR'      LV_TPLNR,
        ' '    'RIWO1-EQUNR'      LV_EQUNR.
      IF P_MTART = 'IBAU'.
        PERFORM GENERATE_BDC_DATA USING:
          ' '    'RIWO1-BAUTL'      LV_ASSY.
      ELSEIF P_MTART = 'ERSA'.
        PERFORM GENERATE_BDC_DATA USING:
          ' '    'RIWO1-BAUTL'      LV_MATNR.
      ENDIF.
      PERFORM GENERATE_BDC_DATA USING:
          ' '    'BDC_CURSOR'      'VIQMEL-QMTXT'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Change Notification (List)
    WHEN 'IW28'.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIQMEL20'         '1000',
        ' '    'BDC_CURSOR'       'SELSCHEM',
        ' '    'IWERK-LOW'        LV_WERKS,
        ' '    'SWERK-LOW'        LV_WERKS,
        ' '    'STRNO-LOW'        LV_TPLNR.

      CASE P_MTART.
        WHEN 'EQUI'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'        LV_EQUNR,
         ' '    'MATNR-LOW'        ''.

        WHEN 'IBAU'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'        LV_EQUNR,
         ' '    'MATNR-LOW'        LV_ASSY.

        WHEN 'ERSA'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'        '',  "lv_equnr,
         ' '    'MATNR-LOW'        LV_MATNR.

        WHEN OTHERS.
      ENDCASE.

      PERFORM GENERATE_BDC_DATA USING:
        ' '    'DATUV'            LV_DATUM,
        ' '    'DATUB'            SY-DATUM,
        ' '    'STAI1-LOW'        ''.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Display Notification (List)
    WHEN 'IW29'.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIQMEL20'         '1000',
        ' '    'BDC_CURSOR'       'SELSCHEM',
        ' '    'IWERK-LOW'        LV_WERKS,
        ' '    'SWERK-LOW'        LV_WERKS,
        ' '    'STRNO-LOW'        LV_TPLNR.

      CASE P_MTART.
        WHEN 'EQUI'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'        LV_EQUNR,
         ' '    'MATNR-LOW'        ''.
        WHEN 'IBAU'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'        LV_EQUNR,
         ' '    'MATNR-LOW'        LV_ASSY.
        WHEN 'ERSA'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'        '',  "lv_equnr,
         ' '    'MATNR-LOW'        LV_MATNR.
        WHEN OTHERS.
      ENDCASE.

      PERFORM GENERATE_BDC_DATA USING:
        ' '    'DATUV'            LV_DATUM,
        ' '    'DATUB'            SY-DATUM.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Display Notification (Multi Level)
    WHEN 'IW30'.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIQMEL10'         '1000',
        ' '    'BDC_CURSOR'       'SELSCHEM',
        ' '    'STRNO-LOW'        LV_TPLNR.

      CASE P_MTART.
        WHEN 'EQUI'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'        LV_EQUNR,
         ' '    'MATNR-LOW'        ''.
        WHEN 'IBAU'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'        '',  "lv_equnr,
         ' '    'MATNR-LOW'        LV_ASSY.
        WHEN 'ERSA'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'        '',  "lv_equnr,
         ' '    'MATNR-LOW'        LV_MATNR.
        WHEN OTHERS.
      ENDCASE.

      PERFORM GENERATE_BDC_DATA USING:
        ' '    'DATUV'            LV_DATUM,
        ' '    'DATUB'            SY-DATUM. ",

      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*---Create Order
    WHEN 'IW31'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPLCOIH'         '0100',
        ' '    'AUFPAR-PM_AUFART' '',
        ' '    'CAUFVD-IWERK'     LV_WERKS,  "//계획플랜트
        ' '    'CAUFVD-TPLNR'     LV_TPLNR.
      CASE P_MTART.
        WHEN 'EQUI'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'CAUFVD-EQUNR'        LV_EQUNR,  "//설비
         ' '    'CAUFVD-BAUTL'        LV_MATNR. "//자재
        WHEN 'IBAU'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'CAUFVD-EQUNR'        LV_EQUNR,  "//설비
         ' '    'CAUFVD-BAUTL'        LV_ASSY.    "//ASSY
        WHEN OTHERS.
      ENDCASE.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Change Order
    WHEN 'IW32'.
      CALL TRANSACTION P_FCODE.

*-- Display Order
    WHEN 'IW33'.
      CALL TRANSACTION P_FCODE.

*-- Print Order
    WHEN 'IW3D'.
      CALL TRANSACTION P_FCODE.

*--Change Order(List)
    WHEN 'IW38'.

      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIAUFK20'    '1000',
        ' '    'STRNO-LOW'   LV_TPLNR,
        ' '    'IWERK-LOW'   LV_WERKS,
        ' '    'SWERK-LOW'   LV_WERKS.

      CASE P_MTART.
        WHEN 'EQUI'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'        LV_EQUNR,
         ' '    'SERMAT-LOW'       LV_MATNR.
        WHEN 'IBAU'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'         LV_EQUNR,
         ' '    'SERMAT-LOW'        LV_ASSY.
        WHEN 'ERSA'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'         LV_EQUNR,
         ' '    'SERMAT-LOW'        LV_MATNR.
        WHEN OTHERS.
      ENDCASE.
      PERFORM GENERATE_BDC_DATA USING:
        ' '    'DATUV'       LV_DATUM,
        ' '    'DATUB'       SY-DATUM,
        ' '    'STAI1-LOW'   '',  " 'CRTD',
        ' '    'BDC_CURSOR'  'SELSCHEM'.

      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Display Order(List)
    WHEN 'IW39'.
      P_FCODE = P_FCODE+0(4).
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIAUFK20'    '1000',
        ' '    'IWERK-LOW'   LV_WERKS,
        ' '    'SWERK-LOW'   LV_WERKS,
        ' '    'STRNO-LOW'   LV_TPLNR.
      CASE P_MTART.
        WHEN 'EQUI'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'        LV_EQUNR,
         ' '    'SERMAT-LOW'       LV_MATNR.
        WHEN 'IBAU'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'         LV_EQUNR,
         ' '    'SERMAT-LOW'        LV_ASSY.
        WHEN 'ERSA'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'         LV_EQUNR,
         ' '    'SERMAT-LOW'        LV_MATNR.
        WHEN OTHERS.
      ENDCASE.
      PERFORM GENERATE_BDC_DATA USING:
        ' '    'DATUV'       LV_DATUM,
        ' '    'DATUB'       SY-DATUM,
        ' '    'BDC_CURSOR'  'AUFNR-LOW'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Order List
    WHEN 'IW40'.
      P_FCODE = P_FCODE+0(4).  "// 앞의 4자리만- IW38 과 구분
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIAUFK10'    '1000',
        ' '    'STRNO-LOW'   LV_TPLNR.
      CASE P_MTART.
        WHEN 'EQUI'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'        LV_EQUNR,
         ' '    'SERMAT-LOW'       LV_MATNR.
        WHEN 'IBAU'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'         LV_EQUNR,
         ' '    'SERMAT-LOW'        LV_ASSY.
        WHEN 'ERSA'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR-LOW'         LV_EQUNR,
         ' '    'SERMAT-LOW'        LV_MATNR.
        WHEN OTHERS.
      ENDCASE.
      PERFORM GENERATE_BDC_DATA USING:
        ' '    'DATUV'       LV_DATUM,
        ' '    'DATUB'       SY-DATUM,
        ' '    'BDC_CURSOR'  'AUFNR-LOW'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Create Order Confirmation
    WHEN 'IW41'.
      CALL TRANSACTION P_FCODE.

*---Change Order Confirmation
    WHEN 'IW43'.
      CALL TRANSACTION P_FCODE.

*--- Lisplay  Confirmation
    WHEN 'IW47'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIAFRU20'      '1000',
        ' '    'WERKS_C-LOW'   LV_WERKS,
        ' '    'WERKS_O-LOW'   LV_WERKS,
        ' '    'STRNO_O-LOW'   LV_TPLNR.
      CASE P_MTART.
        WHEN 'EQUI'.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR_O-LOW'        LV_EQUNR.
        WHEN OTHERS.
          PERFORM GENERATE_BDC_DATA USING:
         ' '    'EQUNR_O-LOW'        ''.  "LV_EQUNR.
      ENDCASE.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Actual Settlement Order
    WHEN 'KO88'.
      CALL TRANSACTION P_FCODE.

*-- Goods Movement
    WHEN 'MB11'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPMM07M'     '0400',
        ' '    'RM07M-WERKS'   LV_WERKS,
*        ' '    'RM07M-LGORT'   GV_LGORT,
        ' '    'RM07M-BWARTWA' '',
        ' '    'BDC_CURSOR'    'RM07M-BWARTWA'.

      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Display Goods Movement(List)
    WHEN 'IW3M'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIAUFM20'     '1000',
        ' '    'WERKS-LOW'   LV_WERKS,
        ' '    'DY_WAP'      ' ',      "//계획 GI
        ' '    'DY_WEB'      'X',      "//구매오더에 대한
        ' '    'DY_WAU'      ' ',      "//비계획출고
        ' '    'DY_WEF'      ' ',      "//오더에대한 GR
*        ' '    'RM07M-LGORT'   GV_LGORT,
        ' '    'BUDAT-LOW'    SY-DATUM,      "//전기일
        ' '    'MJAHR-LOW'    SY-DATUM+0(4). "//자재문서연도
      CASE P_MTART.
        WHEN 'ERSA'.
          PERFORM GENERATE_BDC_DATA USING:
           ' '    'MATNR-LOW'    LV_MATNR.
        WHEN OTHERS.
          PERFORM GENERATE_BDC_DATA USING:
           ' '    'MATNR-LOW'    ''.   "LV_MATNR.
      ENDCASE.

      PERFORM GENERATE_BDC_DATA USING:
        ' '    'BDC_CURSOR'    'MBLNR-LOW'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Dsiplay Material
    WHEN 'IH09'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIMARA20'     '1000'.
      CASE P_MTART.
        WHEN 'ERSA'.
          PERFORM GENERATE_BDC_DATA USING:
           ' '    'MTART-LOW'       P_MTART,  "//자재유형
           ' '    'MS_MATNR-LOW'    LV_MATNR. "//자재번호
        WHEN OTHERS.
          PERFORM GENERATE_BDC_DATA USING:
           ' '    'MTART-LOW'       '',  "//P_MTART,
           ' '    'MS_MATNR-LOW'    ''.   "LV_MATNR.
      ENDCASE.

      PERFORM GENERATE_BDC_DATA USING:
        ' '    'BDC_CURSOR'    'MS_MATNR-LOW'.

      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Stock Overview
    WHEN 'MMBE'.  "//재고개요
      IF P_MTART = 'ERSA'.  "//자재일때만
        CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
        PERFORM GENERATE_BDC_DATA USING:
          'X'    'RMMMBEST'      '1000',
          ' '    'MS_WERKS-LOW'   LV_WERKS,
          ' '    'MS_MATNR-LOW'   LV_MATNR,
          ' '    'MS_LGORT-LOW'   '',       "//GV_LGORT,
          ' '    'BDC_CURSOR'    'MS_MATNR-LOW'.

        PERFORM CALL_TRANSACTIONS USING P_FCODE.
      ENDIF.

*-- Multi-Level Equipment List
    WHEN 'IE07'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIEQUI30'      '1000',
        ' '    'EQUNR-LOW'     LV_EQUNR,
*        ' '    'MATNR-LOW'     LV_MATNR,
        ' '    'BDC_CURSOR'    'EQUNR-LOW'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.


*-- Storage Location Analysis
    WHEN 'MC.5'.
      CALL TRANSACTION P_FCODE.

*-- PMIS
    WHEN 'MCJB'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIEQS070'      '1000',
        ' '    'OEQUNR-LOW'     LV_EQUNR,
        ' '    'BDC_CURSOR'    'OEQUNR-LOW'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Graphic Scheduling Overview
    WHEN 'IP19'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIMHIS00'      '1000',
        ' '    'STRNO-LOW'      LV_TPLNR,
        ' '    'EQUNR-LOW'      LV_EQUNR.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

    WHEN OTHERS.
  ENDCASE.

  CLEAR P_FCODE.

  WA_INIT_FLG = 'X'.
ENDFORM.                    " CALL_TRANSACTION_EQUI
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_EQ_N_MAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_NODE_KEY  text
*      -->P_WA_MTART  text
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION_EQ_N_MAT USING    P_NODE
                                        P_MTART.
  DATA: LV_MATNR LIKE MARA-MATNR.
  DATA: LV_EQUNR LIKE EQUI-EQUNR.

  CASE  WA_MTART.
    WHEN 'EQUI'.
      SET PARAMETER ID 'EQN' FIELD P_NODE.
      CALL TRANSACTION 'IE03'  AND SKIP FIRST SCREEN.

    WHEN 'ERSA'.
      LV_MATNR = P_NODE+0(9).

      SET PARAMETER ID 'MAT' FIELD P_NODE.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

    WHEN 'IBAU'.
      LV_MATNR = P_NODE+0(9).

      SET PARAMETER ID 'MAT' FIELD P_NODE.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " CALL_TRANSACTION_EQ_N_MAT
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_MHRC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FCODE  text
*      -->P_WA_NODE_KEY  text
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION_MHRC USING    P_FCODE
                                    P_NODE.
  DATA: LV_TPLNR LIKE IFLOT-TPLNR,
        LV_EQUNR LIKE EQUI-EQUNR,
        LV_ASSY  LIKE MARA-MATNR.

  DATA: LV_MTART LIKE MARA-MTART.

  DATA: LRG_TPLNR LIKE RANGE OF IFLOT-TPLNR WITH HEADER LINE,
        LRG_EQUNR LIKE RANGE OF EQUI-EQUNR  WITH HEADER LINE.

  DATA: LV_ZBUKRS TYPE BUKRS,
        LV_WERKS  LIKE T001W-WERKS,
        LV_ZZYYMM LIKE SY-DATUM,
        LV_ZYEAR(4).
  DATA: LV_DATUM LIKE SY-DATUM.

  CLEAR  : LV_TPLNR, LV_EQUNR, LV_ASSY, LV_MTART, LRG_TPLNR, LRG_EQUNR.
  REFRESH: LRG_TPLNR, LRG_EQUNR.

  CLEAR  : LV_ZBUKRS, LV_WERKS, LV_ZZYYMM.

  LV_TPLNR = P_NODE.
  PERFORM GET_SWERK USING LV_TPLNR LV_WERKS.

  CASE P_FCODE.
*-- Upload drawing
    WHEN 'UPLD'.
      PERFORM MANAGE_IMAGE USING P_NODE 'X'.

*--- Delete drawing
    WHEN 'DELE'.
      PERFORM MANAGE_IMAGE USING P_NODE ' '.
      PERFORM CREATE_PICTURE_OBJECT USING P_NODE.

*-- view drawing
    WHEN 'DISP'.
      PERFORM CREATE_PICTURE_OBJECT USING P_NODE.

*-- Create Notificaion
    WHEN 'IW21'.
      CALL TRANSACTION P_FCODE.

*-- Change Notification
    WHEN 'IW22'.
      CALL TRANSACTION P_FCODE.

*--Display Notification
    WHEN 'IW23'.
      CALL TRANSACTION P_FCODE.

*-- Malfunction Report
    WHEN 'IW24'.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPLIQS0'         '7200',
        ' '    'VIQMEL-QMTXT'     '',
        ' '    'RIWO1-TPLNR'      LV_TPLNR,
        ' '    'RIWO1-EQUNR'      '', "LV_EQUNR
        ' '    'BDC_CURSOR'      'VIQMEL-QMTXT'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Maintenance Request
    WHEN 'IW26'.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPLIQS0'         '7200',
        ' '    'VIQMEL-QMTXT'     '',
        ' '    'RIWO1-TPLNR'      LV_TPLNR,
        ' '    'RIWO1-EQUNR'      '', "LV_EQUNR
        ' '    'BDC_CURSOR'      'VIQMEL-QMTXT'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Change Notification (List)
    WHEN 'IW28'.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIQMEL20'         '1000',
        ' '    'BDC_CURSOR'       'SELSCHEM',
        ' '    'IWERK-LOW'        LV_WERKS,
        ' '    'SWERK-LOW'        LV_WERKS,
        ' '    'STRNO-LOW'        LV_TPLNR,    "//Func Loc
        ' '    'DATUV'            LV_DATUM,    "//FROM date
        ' '    'DATUB'            SY-DATUM,    "//TO date
        ' '    'STAI1-LOW'        ''. " 'OSNO'. "//status
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Display Notification (List)
    WHEN 'IW29'.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIQMEL20'         '1000',
        ' '    'BDC_CURSOR'       'SELSCHEM',
        ' '    'IWERK-LOW'        LV_WERKS,
        ' '    'SWERK-LOW'        LV_WERKS,
        ' '    'STRNO-LOW'        LV_TPLNR,    "//Func Loc
        ' '    'DATUV'            LV_DATUM,    "//FROM date
        ' '    'DATUB'            SY-DATUM. ",  "//TO date
*        ' '    'STAI1-LOW'        'OSNO'.  "//status
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Display Notification (Multi Level)
    WHEN 'IW30'.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIQMEL10'         '1000',
        ' '    'BDC_CURSOR'       'SELSCHEM',
*       ' '    'IWERK-LOW'        LV_WERKS,
*       ' '    'SWERK-LOW'        LV_WERKS,
        ' '    'DATUV'            LV_DATUM,
        ' '    'DATUB'            SY-DATUM. ",
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*---Create Order
    WHEN 'IW31'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPLCOIH'         '0100',
        ' '    'AUFPAR-PM_AUFART' '',
        ' '    'CAUFVD-IWERK'     LV_WERKS,
        ' '    'CAUFVD-TPLNR'     LV_TPLNR.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Change Order
    WHEN 'IW32'.
      CALL TRANSACTION P_FCODE.

*-- Display Order
    WHEN 'IW33'.
      CALL TRANSACTION P_FCODE.

*-- Print Order
    WHEN 'IW3D'.
      CALL TRANSACTION P_FCODE.

*--Change Order(List)
    WHEN 'IW38'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIAUFK20'    '1000',
        ' '    'STRNO-LOW'   LV_TPLNR,
        ' '    'IWERK-LOW'   LV_WERKS,
        ' '    'SWERK-LOW'   LV_WERKS,
        ' '    'DATUV'       LV_DATUM,
        ' '    'DATUB'       SY-DATUM,
        ' '    'STAI1-LOW'   '',  " 'CRTD',
        ' '    'BDC_CURSOR'  'SELSCHEM'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Display Order(List)
    WHEN 'IW39'.
      P_FCODE = P_FCODE+0(4).
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIAUFK20'    '1000',
        ' '    'IWERK-LOW'   LV_WERKS,
        ' '    'SWERK-LOW'   LV_WERKS,
        ' '    'STRNO-LOW'   LV_TPLNR,
        ' '    'BDC_CURSOR'  'AUFNR-LOW'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Create Order Confirmation
    WHEN 'IW41'.
      CALL TRANSACTION P_FCODE.

*---Change Order Confirmation
    WHEN 'IW43'.
      CALL TRANSACTION P_FCODE.

*--- Lisplay  Confirmation
    WHEN 'IW47'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      CONCATENATE SY-DATUM+0(4) '0101' INTO LV_DATUM.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIAFRU20'      '1000',
        ' '    'WERKS_C-LOW'   LV_WERKS,
        ' '    'STRNO_O-LOW'   LV_TPLNR.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Actual Settlement Order
    WHEN 'KO88'.
      CALL TRANSACTION P_FCODE.

*-- Goods Movement
    WHEN 'MB11'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPMM07M'     '0400',
        ' '    'RM07M-WERKS'   LV_WERKS,
*       ' '    'RM07M-LGORT'   GV_LGORT,
        ' '    'RM07M-BWARTWA' '',
        ' '    'BDC_CURSOR'    'RM07M-BWARTWA'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Display Goods Movement(List)
    WHEN 'IW3M'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIAUFM20'     '1000',
        ' '    'WERKS-LOW'   LV_WERKS,
*       ' '    'RM07M-LGORT'   GV_LGORT,
        ' '    'BDC_CURSOR'    'MBLNR-LOW'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Dsiplay Material
    WHEN 'IH09'.
      CALL TRANSACTION P_FCODE.

*-- Stock Overview
    WHEN 'MMBE'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RMMMBEST'      '1000',
        ' '    'MS_WERKS-LOW'   LV_WERKS,
*       ' '    'MS_LGORT-LOW'   GV_LGORT,
        ' '    'BDC_CURSOR'    'MS_MATNR-LOW'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Multi-Level Func Loc List
    WHEN 'IL07'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RIIFLO30'      '1000',
        ' '    'STRNO-LOW'     LV_TPLNR,
        ' '    'BDC_CURSOR'    'STRNO-LOW'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Location Analysis
    WHEN 'MCI3'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RMCI0200'     '1000',
        ' '    'SL_0001-LOW'   LV_WERKS,
        ' '    'SL_0004-LOW'   LV_WERKS,
        ' '    'STRNO-LOW'     LV_TPLNR.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Damage Analysis
    WHEN 'MCI5'.
      CLEAR: BDC_TAB, MSG_TAB.    REFRESH: BDC_TAB, MSG_TAB.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RMCI0800'     '1000',
*       ' '    'SL_0002-LOW'   LV_EQUNR,
        ' '    'SL_0003-LOW'   '',
        ' '    'STRNO-LOW'     LV_TPLNR,
        ' '    'BDC_CURSOR'   'SL_0003-LOW'.
      PERFORM CALL_TRANSACTIONS USING P_FCODE.

*-- Storage Location Analysis
    WHEN 'MC.5'.
      CALL TRANSACTION P_FCODE.

    WHEN OTHERS.
  ENDCASE.

  CLEAR P_FCODE.

  WA_INIT_FLG = 'X'.
ENDFORM.                    " CALL_TRANSACTION_MHRC
*&---------------------------------------------------------------------*
*&      Form  CHECK_MTART_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_NODE_KEY  text
*      -->P_WA_MTART  text
*----------------------------------------------------------------------*
FORM CHECK_MTART_TYPE USING    P_CODE
                               P_MTART.
  CLEAR IT_BOM_LIST_NODE.
  READ TABLE IT_BOM_LIST_NODE WITH KEY NODE_KEY = P_CODE.

  CHECK SY-SUBRC = 0.
  MOVE : IT_BOM_LIST_NODE-MATNR_CHILD TO P_CODE,
         IT_BOM_LIST_NODE-MTART       TO P_MTART.
ENDFORM.                    " CHECK_MTART_TYPE
*&---------------------------------------------------------------------*
*&      Form  GET_FUNCTION_LOCATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      --> P_WERKS
*----------------------------------------------------------------------*
FORM GET_FUNCTION_LOCATION USING    P_WERKS.
  DATA : LIT_NODE   TYPE TABLE OF ZSPM_FUNC_LOC WITH HEADER LINE,
         LIT_FUNC_2 LIKE NODE_STR OCCURS 0.

  DATA : ACCUM_TEMP LIKE NODE_STR OCCURS 0.

  DATA : LV_PARENT LIKE IFLOT-TPLNR.

  DATA : LV_TMP1(30) TYPE C,
         LV_TMP2(30) TYPE C.

  DATA : LV_TEMP TYPE I.

  DATA: LV_FILTER     LIKE IFLOT-TPLNR,
        LV_PARTY(3),
        LV_CHILD_OLD  LIKE IFLOT-TPLNR,
        LRG_TPLNR TYPE RANGE OF IFLOT-TPLNR WITH HEADER LINE.

  CLEAR IT_EQUIPMENT.    REFRESH IT_EQUIPMENT.

  CLEAR: LIT_FUNC_2[], ACCUM_TEMP.
  CLEAR: FUNC_NODE_ITAB[], FUNC_NODE_ITAB2[].


  LOOP AT IT_WERKS WHERE WERKS = P_WERKS.
    CLEAR LIT_NODE.    REFRESH LIT_NODE.

    CALL FUNCTION 'Z_FPM_GET_FUNC_LOCATION'
         EXPORTING
              I_WERKS                 = IT_WERKS-WERKS
         TABLES
              T_FUNC_LIST             = LIT_NODE
         EXCEPTIONS
              NOT_FOUND_FUNC_LOCATION = 1
              OTHERS                  = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    DATA : LV_SUF_IND(4) TYPE N.
    DATA : LV_TABIX LIKE SY-TABIX,
           LV_INDEX LIKE SY-TABIX.
    DATA : LV_SUFFIX TYPE DATATYPE-CHAR0128.
    DATA : LV_CHILD(20) TYPE C.
    DATA : LV_NEW_NODE_KEY(12).           "Node_key

    IT_FUNC_LIST[] = LIT_NODE[].

    LOOP AT LIT_NODE.
      CLEAR LV_TABIX.
      LV_TABIX = SY-TABIX.

      AT FIRST.
        CLEAR LV_SUFFIX.
        PERFORM GENERATE_SUFFIX_BY_RANDOM CHANGING LV_SUFFIX.
      ENDAT.
      LV_CHILD = LIT_NODE-TPLNR.

      LV_SUF_IND = LV_SUF_IND + 1.
      CONCATENATE LV_SUFFIX LV_SUF_IND
                   INTO LIT_NODE-TPLNR.

      CLEAR LV_NEW_NODE_KEY.
      MOVE : LIT_NODE-TPLNR  TO  LV_NEW_NODE_KEY .

      READ TABLE IT_FUNC_LIST WITH KEY TPLNR = LV_CHILD.
      IF IT_FUNC_LIST-TPLMA IS INITIAL.
        MOVE : P_WERKS TO LIT_NODE-TPLMA.
      ELSE.
        MOVE : IT_FUNC_LIST-TPLMA TO LIT_NODE-TPLMA.
      ENDIF.

      CLEAR IT_FUNC_LIST.
      MOVE-CORRESPONDING LIT_NODE TO IT_FUNC_LIST.
      MODIFY IT_FUNC_LIST INDEX LV_TABIX.

      LOOP AT IT_FUNC_LIST WHERE TPLMA = LV_CHILD.
        LV_INDEX = SY-TABIX.
        IT_FUNC_LIST-TPLMA = LV_NEW_NODE_KEY.
        MODIFY IT_FUNC_LIST INDEX LV_INDEX.
      ENDLOOP.

      MODIFY LIT_NODE  INDEX LV_TABIX.
      PERFORM ACC_IT_FUNC_LIST_NODE_TABLE USING LIT_NODE-TPLNR
                                                LIT_NODE-TPLMA
                                                LV_CHILD
                                                LIT_NODE-PLTXT.
    ENDLOOP.

    SORT IT_FUNC_LIST BY ISFOLDER DESCENDING TPLMA ASCENDING .

    LOOP AT IT_FUNC_LIST.
      CLEAR FUNC_NODE.
      FUNC_NODE-NODE_KEY = IT_FUNC_LIST-TPLNR.
      FUNC_NODE-RELATKEY = IT_FUNC_LIST-TPLMA.

      FUNC_NODE-TEXT     = IT_FUNC_LIST-PLTXT.
      FUNC_NODE-ISFOLDER = IT_FUNC_LIST-ISFOLDER.

      PERFORM FILL_TREE TABLES FUNC_NODE_ITAB
                               ACCUM_NODE_ITAB
                        USING  FUNC_NODE.

    ENDLOOP.                   "IT_FUNC_LIST
    CLEAR: IT_WERKS.
  ENDLOOP.                     "End it_werks
ENDFORM.                    " GET_FUNCTION_LOCATION
*&---------------------------------------------------------------------*
*&      Form  LOAD_PICTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_NODE_KEY  text
*      <--P_PIC_URL  text
*----------------------------------------------------------------------*
FORM LOAD_PICTURE USING    P_NODE_KEY
                  CHANGING P_URL.

  DATA QUERY_TABLE    LIKE W3QUERY OCCURS 1 WITH HEADER LINE.
  DATA HTML_TABLE     LIKE W3HTML OCCURS 1.
  DATA RETURN_CODE    LIKE  W3PARAM-RET_CODE.
  DATA CONTENT_TYPE   LIKE  W3PARAM-CONT_TYPE.
  DATA CONTENT_LENGTH LIKE  W3PARAM-CONT_LEN.
  DATA PIC_DATA       LIKE W3MIME OCCURS 0.
  DATA PIC_SIZE       TYPE I.

  REFRESH QUERY_TABLE.
  QUERY_TABLE-NAME = '_OBJECT_ID'.
  QUERY_TABLE-VALUE = P_NODE_KEY.
  APPEND QUERY_TABLE.

  CALL FUNCTION 'WWW_GET_MIME_OBJECT'
       TABLES
            QUERY_STRING        = QUERY_TABLE
            HTML                = HTML_TABLE
            MIME                = PIC_DATA
       CHANGING
            RETURN_CODE         = RETURN_CODE
            CONTENT_TYPE        = CONTENT_TYPE
            CONTENT_LENGTH      = CONTENT_LENGTH
       EXCEPTIONS
            OBJECT_NOT_FOUND    = 1
            PARAMETER_NOT_FOUND = 2
            OTHERS              = 3.
  IF SY-SUBRC = 0.
    PIC_SIZE = CONTENT_LENGTH.
  ENDIF.

  CALL FUNCTION 'DP_CREATE_URL'
       EXPORTING
            TYPE     = 'image'
            SUBTYPE  = CNDP_SAP_TAB_UNKNOWN
            SIZE     = PIC_SIZE
            LIFETIME = CNDP_LIFETIME_TRANSACTION
       TABLES
            DATA     = PIC_DATA
       CHANGING
            URL      = P_URL
       EXCEPTIONS
            OTHERS   = 1.

ENDFORM.                    " LOAD_PICTURE
*&---------------------------------------------------------------------*
*&      Form  CREATE_TREE_COLUMN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_TOGGLE  text
*----------------------------------------------------------------------*
FORM CREATE_TREE_COLUMN USING    P_TOGGLE.
  DATA: HIERARCHY_HEADER TYPE TREEV_HHDR.

  HIERARCHY_HEADER-HEADING = '설비/자재'(T20).
  HIERARCHY_HEADER-WIDTH = 35.

* Create Tree Object or Column1
  IF P_TOGGLE EQ 'MHRC'.
    CREATE OBJECT EQUI_TREE
      EXPORTING
        PARENT              = DOCKING_TOP->DEFAULT_SCREEN
        NODE_SELECTION_MODE = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SINGLE
        ITEM_SELECTION = ' '
        HIERARCHY_COLUMN_NAME = 'COLUMN1'
        HIERARCHY_HEADER = HIERARCHY_HEADER
      EXCEPTIONS
        CNTL_SYSTEM_ERROR           = 1
        CREATE_ERROR                = 2
        FAILED                      = 3
        ILLEGAL_NODE_SELECTION_MODE = 4
        ILLEGAL_COLUMN_NAME         = 5
        LIFETIME_ERROR              = 6.
    IF SY-SUBRC <> 0.
      MESSAGE A000.
    ENDIF.
  ENDIF.

  CLEAR EVENTS2[].

* Node_Double_Click
  EVENT2-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK.
  EVENT2-APPL_EVENT = 'X'.
  APPEND EVENT2 TO EVENTS2.

* Context Menu
  EVENT2-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_CONTEXT_MENU_REQ.
  EVENT2-APPL_EVENT = ' '.
  APPEND EVENT2 TO EVENTS2.

* process PAI if context menu select event occurs
  CALL METHOD EQUI_TREE->SET_CTX_MENU_SELECT_EVENT_APPL
    EXPORTING APPL_EVENT = 'X'.

  CALL METHOD EQUI_TREE->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS = EVENTS2
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* Set Event Handler
*  SET HANDLER g_application->handle_button_click FOR equi_tree.
  SET HANDLER WA_APPLICATION->HANDLE_NODE_DOUBLE_CLICK2 FOR EQUI_TREE.
  SET HANDLER WA_APPLICATION->HANDLE_NODE_CONTEXT_MENU_REQ2
                             FOR EQUI_TREE.
  SET HANDLER WA_APPLICATION->HANDLE_NODE_CONTEXT_MENU_SEL2
                             FOR EQUI_TREE.

* Create Column2
  CALL METHOD EQUI_TREE->ADD_COLUMN
    EXPORTING
      NAME = 'COLUMN2'
      WIDTH = 20
      HEADER_TEXT = TEXT-D01     "//코  드
    EXCEPTIONS
      COLUMN_EXISTS                 = 1
      ILLEGAL_COLUMN_NAME           = 2
      TOO_MANY_COLUMNS              = 3
      ILLEGAL_ALIGNMENT             = 4
      DIFFERENT_COLUMN_TYPES        = 5
      CNTL_SYSTEM_ERROR             = 6
      FAILED                        = 7
      PREDECESSOR_COLUMN_NOT_FOUND  = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

*-- Create Column3
  CALL METHOD EQUI_TREE->ADD_COLUMN
    EXPORTING
      NAME = 'COLUMN3'
      WIDTH = 12
      HEADER_TEXT = TEXT-D02      "//'수 량'
    EXCEPTIONS
      COLUMN_EXISTS                 = 1
      ILLEGAL_COLUMN_NAME           = 2
      TOO_MANY_COLUMNS              = 3
      ILLEGAL_ALIGNMENT             = 4
      DIFFERENT_COLUMN_TYPES        = 5
      CNTL_SYSTEM_ERROR             = 6
      FAILED                        = 7
      PREDECESSOR_COLUMN_NOT_FOUND  = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.
ENDFORM.                    " CREATE_TREE_COLUMN
*&---------------------------------------------------------------------*
*&      Form  GET_LAST_TPLNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NODE_KEY  text
*      -->P_WA_TOGGLE  text
*----------------------------------------------------------------------*
FORM GET_LAST_TPLNR USING    P_LAST
                             P_TOGGLE.
  IF P_TOGGLE EQ 'MHRC'.
    PERFORM GET_EQUIPMENT_LIST USING P_LAST.
  ELSE.
    SORT IT_EQUIPMENT BY NODE ITEM.
    LOOP AT IT_EQUIPMENT WHERE NODE EQ P_LAST.
      PERFORM GET_EQUIPMENT_LIST USING IT_EQUIPMENT-ITEM.
    ENDLOOP.

    LOOP AT IT_LAST_FUNC.
      IT_LAST_FUNC-ISFOLDER = 'X'.
      MODIFY IT_LAST_FUNC INDEX SY-TABIX.
    ENDLOOP.
    CLEAR IT_LAST_FUNC.
  ENDIF.
ENDFORM.                    " GET_LAST_TPLNR
*&---------------------------------------------------------------------*
*&      Form  GET_EQUIPMENT_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LAST  text
*----------------------------------------------------------------------*
FORM GET_EQUIPMENT_LIST USING  P_LAST.
  DATA: LV_LAST LIKE IFLOT-TPLNR.

  SELECT TPLNR TPLMA PLTXT
  APPENDING CORRESPONDING FIELDS OF TABLE IT_LAST_FUNC
                      FROM IFLO
                    WHERE TPLNR = P_LAST
                      AND SPRAS = SY-LANGU.
ENDFORM.                    " GET_EQUIPMENT_LIST
*&---------------------------------------------------------------------*
*&      Form  GET_EQUI_BY_FUNC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_TOGGLE  text
*----------------------------------------------------------------------*
FORM GET_EQUI_BY_FUNC USING    P_TOGGLE.
  DATA: LV_TABIX(2),
        LV_ROOT(6).

  CLEAR RG_LAST_ROOT.    REFRESH RG_LAST_ROOT.

  LOOP AT IT_LAST_FUNC.
    IF P_TOGGLE EQ 'MHRC'.
      CLEAR LV_TABIX.
      LV_TABIX = SY-TABIX.
      CONCATENATE 'ITEM' LV_TABIX INTO WA_LAST_ROOT.
    ELSE.
      WA_LAST_ROOT        = IT_LAST_FUNC-TPLNR.
      RG_LAST_ROOT-SIGN   = 'I'.
      RG_LAST_ROOT-OPTION = 'EQ'.
      RG_LAST_ROOT-LOW    = WA_LAST_ROOT.
      APPEND RG_LAST_ROOT.    CLEAR RG_LAST_ROOT.
    ENDIF.
*--- accumulate node
    PERFORM ACC_IT_BOM_LIST_NODE_TABLE TABLES IT_BOM_LIST_NODE
                                       USING  IT_LAST_FUNC-TPLNR
                                              IT_LAST_FUNC-TPLMA
                                              WA_LAST_ROOT
                                              'TPLN'
                                              IT_LAST_FUNC-PLTXT.

    CLEAR: EQUI_NODE, EQUI_ITEM.
*-- fill node
    EQUI_NODE-NODE_KEY = WA_LAST_ROOT.  "Node Key
    CLEAR EQUI_NODE-RELATKEY.           "Parents Node Key
    CLEAR EQUI_NODE-RELATSHIP.          "Node elationship
    EQUI_NODE-HIDDEN = ' '.             "Hide Node
    EQUI_NODE-DISABLED = ' '.           "
    EQUI_NODE-ISFOLDER = 'X'.           "Display Folder
    CLEAR EQUI_NODE-N_IMAGE.            "embedded bitmap
    CLEAR EQUI_NODE-EXP_IMAGE.          "opened bitmap
    CLEAR EQUI_NODE-EXPANDER.           "Display '+'

*-- fill item ('COLUMN1')
    EQUI_ITEM-NODE_KEY = WA_LAST_ROOT.  "Root Node for item
    EQUI_ITEM-ITEM_NAME = 'COLUMN1'.    "Item Name
    EQUI_ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
    EQUI_ITEM-TEXT = IT_LAST_FUNC-PLTXT.

    IF P_TOGGLE EQ 'MHRC'.
      PERFORM FILL_COLUMN_TREE USING EQUI_NODE_ITAB
                                     EQUI_ITEM_ITAB
                                     EQUI_NODE
                                     EQUI_ITEM.
    ENDIF.

    PERFORM GET_EQUI_LIST USING IT_LAST_FUNC-TPLNR  "Original Name
                               WA_LAST_ROOT        "New Name
                               P_TOGGLE.

  ENDLOOP.
ENDFORM.                    " GET_EQUI_BY_FUNC
*&---------------------------------------------------------------------*
*&      Form  FILL_COLUMN_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EQUI_NODE_ITAB  text
*      -->P_EQUI_ITEM_ITAB  text
*      -->P_EQUI_NODE  text
*      -->P_EQUI_ITEM  text
*----------------------------------------------------------------------*
FORM FILL_COLUMN_TREE USING NODE_TABLE TYPE TREEV_NTAB
                            ITEM_TABLE TYPE ITEM_TABLE_TYPE
                            P_NODE     TYPE TREEV_NODE
                            P_ITEM     TYPE MTREEITM.

  DATA: NODE TYPE TREEV_NODE,
        ITEM TYPE MTREEITM.

  IF NOT P_NODE IS INITIAL.
    MOVE-CORRESPONDING P_NODE TO NODE.
    APPEND NODE TO NODE_TABLE.
  ENDIF.

  IF NOT P_ITEM IS INITIAL.
    MOVE-CORRESPONDING P_ITEM TO ITEM.
    APPEND ITEM TO ITEM_TABLE.
  ENDIF.

ENDFORM.                    " FILL_COLUMN_TREE
*&---------------------------------------------------------------------*
*&      Form  GET_EQUI_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LAST_FUNC_TPLNR  text
*      -->P_GV_LAST_ROOT  text
*      -->P_P_TOGGLE  text
*----------------------------------------------------------------------*
FORM GET_EQUI_LIST USING   P_ROOT_TPLNR
                           P_ROOT_NEW
                           P_TOGGLE.

  DATA: LV_NAME(30),                  "Node 이름
        LV_TEXT(40).                  "Node Text

  DATA : LV_WERKS LIKE T001W-WERKS.



  CLEAR IT_EQUI_LIST.        REFRESH IT_EQUI_LIST.
  CLEAR IT_EQUI_LIST_TMP.    REFRESH IT_EQUI_LIST_TMP.

*-- Get Plent by Equim num
  PERFORM GET_SWERK USING P_ROOT_TPLNR LV_WERKS.

*-- Get Equiment List by Func Loc
  PERFORM EQUI_LIST_BY_FUNC USING P_ROOT_TPLNR.

*-- Get Bom of Func Loc
  PERFORM GET_BOM_OF_FUNC USING P_ROOT_TPLNR.

*-- Append interanl Table
  IF NOT IT_EQUI_LIST_TMP[] IS INITIAL.
    APPEND LINES OF IT_EQUI_LIST_TMP TO IT_EQUI_LIST.
    SORT IT_EQUI_LIST BY TPLNR EQUNR.
  ENDIF.

  PERFORM MAKE_NODE USING P_ROOT_TPLNR
                           P_ROOT_NEW
                           P_TOGGLE
                           LV_WERKS.


ENDFORM.                    " GET_EQUI_LIST
*&---------------------------------------------------------------------*
*&      Form  GET_SWERK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ROOT_TPLNR  text
*      -->P_LV_WERKS  text
*----------------------------------------------------------------------*
FORM GET_SWERK USING    P_TPLNR P_SWERK.
  CLEAR P_SWERK.

  SELECT SINGLE SWERK INTO P_SWERK FROM IFLO
                WHERE TPLNR = P_TPLNR
                AND   SPRAS = SY-LANGU.
ENDFORM.                    " GET_SWERK
*&---------------------------------------------------------------------*
*&      Form  EQUI_LIST_BY_FUNC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EQUI_LIST_BY_FUNC USING P_ROOT_TPLNR.
  CALL FUNCTION 'Z_FPM_EQUI_LIST_BY_FUNC'
       EXPORTING
            I_TPLNR        = P_ROOT_TPLNR
       TABLES
            T_EQUI_LIST    = IT_EQUI_LIST
       EXCEPTIONS
            INVALID_TPLNR  = 1
            NOT_FOUND_EQUI = 2
            OTHERS         = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT IT_EQUI_LIST BY TPLNR EQUNR.
ENDFORM.                    " EQUI_LIST_BY_FUNC
*&---------------------------------------------------------------------*
*&      Form  GET_BOM_OF_FUNC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BOM_OF_FUNC USING P_ROOT_TPLNR.
*-- Get material on Func Loc
  CALL FUNCTION 'Z_FPM_GET_BOM_OF_FUNC'
       EXPORTING
            I_TPLNR        = P_ROOT_TPLNR
       IMPORTING
            RETURN         = RETURN
       TABLES
            T_EQUI_LIST    = IT_EQUI_LIST_TMP
       EXCEPTIONS
            INVALID_TPLNR  = 1
            NOT_FOUND_EQUI = 2
            OTHERS         = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " GET_BOM_OF_FUNC
*&---------------------------------------------------------------------*
*&      Form  MAKE_NODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_NODE USING   P_ROOT_TPLNR
                       P_ROOT_NEW
                       P_TOGGLE
                       P_WERKS.

*--make node key to prevent same node key on same Equim...
  DATA : LV_NODE_KEY LIKE EQUI_NODE-NODE_KEY.
  DATA : LV_MTART    LIKE MARA-MTART.

  LOOP AT IT_EQUI_LIST.
    CLEAR: EQUI_NODE, EQUI_ITEM.

    IF P_TOGGLE = 'MHRC'.
      LV_NODE_KEY = IT_EQUI_LIST-EQUNR .
    ENDIF.

    EQUI_NODE-NODE_KEY  = LV_NODE_KEY.
    EQUI_NODE-RELATKEY  = P_ROOT_NEW.
    EQUI_NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
    EQUI_NODE-HIDDEN    = ' '.
    EQUI_NODE-DISABLED  = ' '.
    EQUI_NODE-ISFOLDER  = ' '.

    LV_MTART = 'EQUI'.

*-- accumulate NODE
    PERFORM ACC_IT_BOM_LIST_NODE_TABLE TABLES IT_BOM_LIST_NODE
                                       USING IT_EQUI_LIST-EQUNR
                                             P_ROOT_NEW
                                             EQUI_NODE-NODE_KEY
                                             LV_MTART
                                             IT_EQUI_LIST-SHTXT.
    CASE LV_MTART.
      WHEN 'IBAU'.    "//ASS'Y
        EQUI_NODE-N_IMAGE   = '@BS@'.
        EQUI_NODE-EXP_IMAGE = '@BS@'.
      WHEN 'ERSA'.   "//자재
        EQUI_NODE-N_IMAGE   = '@7U@'.
        EQUI_NODE-EXP_IMAGE = '@7U@'.
      WHEN OTHERS.
        EQUI_NODE-N_IMAGE   = '@N5@'.
        EQUI_NODE-EXP_IMAGE = '@N5@'.
    ENDCASE.

    EQUI_NODE-EXPANDER  = 'X'.
    EQUI_ITEM-NODE_KEY  = LV_NODE_KEY.
    EQUI_ITEM-ITEM_NAME = 'COLUMN1'.
    EQUI_ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
    EQUI_ITEM-TEXT      = IT_EQUI_LIST-SHTXT.

    IF P_TOGGLE EQ 'MHRC'.
      PERFORM FILL_COLUMN_TREE USING EQUI_NODE_ITAB
                                     EQUI_ITEM_ITAB
                                     EQUI_NODE
                                     EQUI_ITEM.
    ELSE.
      PERFORM FILL_COLUMN_TREE USING EQUI_NODE_ITAB_2
                                     EQUI_ITEM_ITAB_2
                                     EQUI_NODE
                                     EQUI_ITEM.
    ENDIF.

    CLEAR EQUI_NODE.
    EQUI_ITEM-NODE_KEY  = LV_NODE_KEY.
    EQUI_ITEM-ITEM_NAME = 'COLUMN2'.
    EQUI_ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
    EQUI_ITEM-TEXT      = IT_EQUI_LIST-EQUNR.

    IF P_TOGGLE EQ 'MHRC'.
      PERFORM FILL_COLUMN_TREE USING EQUI_NODE_ITAB
                                     EQUI_ITEM_ITAB
                                     EQUI_NODE
                                     EQUI_ITEM.
    ELSE.
      PERFORM FILL_COLUMN_TREE USING EQUI_NODE_ITAB_2
                                     EQUI_ITEM_ITAB_2
                                     EQUI_NODE
                                     EQUI_ITEM.
    ENDIF.

*-- COLUMN3
    CLEAR EQUI_NODE.
    EQUI_ITEM-NODE_KEY  = LV_NODE_KEY.
    EQUI_ITEM-ITEM_NAME = 'COLUMN3'.
    EQUI_ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
    EQUI_ITEM-TEXT      = IT_EQUI_LIST-ZMENGE.

    IF P_TOGGLE EQ 'MHRC'.
      PERFORM FILL_COLUMN_TREE USING EQUI_NODE_ITAB
                                     EQUI_ITEM_ITAB
                                     EQUI_NODE
                                     EQUI_ITEM.
    ELSE.
      PERFORM FILL_COLUMN_TREE USING EQUI_NODE_ITAB_2
                                     EQUI_ITEM_ITAB_2
                                     EQUI_NODE
                                     EQUI_ITEM.
    ENDIF.

*-- Equiment BOM
    IF P_TOGGLE EQ 'MHRC'.
      PERFORM GET_BOM_LIST USING IT_EQUI_LIST-EQUNR
                                  P_TOGGLE
                                  P_WERKS
                                  P_ROOT_TPLNR.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " MAKE_NODE
*&---------------------------------------------------------------------*
*&      Form  ACC_IT_BOM_LIST_NODE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BOM_LIST_NODE  text
*      -->P_IT_EQUI_LIST_EQUNR  text
*      -->P_P_ROOT_NEW  text
*      -->P_EQUI_NODE_NODE_KEY  text
*      -->P_LV_MTART  text
*      -->P_IT_EQUI_LIST_SHTXT  text
*----------------------------------------------------------------------*
FORM ACC_IT_BOM_LIST_NODE_TABLE
              TABLES   P_IT_BOM_LIST_NODE STRUCTURE IT_BOM_LIST_NODE
              USING    P_CHILD
                       P_PARENT
                       P_NODE
                       P_MTART
                       P_TEXT.

  CLEAR IT_BOM_LIST_NODE.
  MOVE : P_CHILD  TO IT_BOM_LIST_NODE-MATNR_CHILD,
         P_PARENT TO IT_BOM_LIST_NODE-MATNR_PARENT,
         P_NODE   TO IT_BOM_LIST_NODE-NODE_KEY,
         P_MTART  TO IT_BOM_LIST_NODE-MTART,
         P_TEXT   TO IT_BOM_LIST_NODE-MAKTX.

  APPEND IT_BOM_LIST_NODE.

ENDFORM.                    " ACC_IT_BOM_LIST_NODE_TABLE
*&---------------------------------------------------------------------*
*&      Form  GET_BOM_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EQUI_LIST_EQUNR  text
*      -->P_P_TOGGLE  text
*      -->P_P_WERKS  text
*      -->P_P_ROOT_TPLNR  text
*----------------------------------------------------------------------*
FORM GET_BOM_LIST USING  P_EQUNR
                         P_TOGGLE
                         P_WERKS
                         P_TPLNR.
  DATA : LT_BOM_TEMP LIKE TABLE OF  ZSPM_BOM_HIRACHY
                                    WITH HEADER LINE.
  DATA : LT_STB LIKE STPOX OCCURS 0 WITH HEADER LINE.
  DATA : LV_SUBMT LIKE ITOB-SUBMT.

  CLEAR LT_BOM_TEMP.    REFRESH LT_BOM_TEMP.
  CLEAR LV_SUBMT.

  CALL FUNCTION 'Z_FPM_GET_BOM_OF_EQUNR'
       EXPORTING
            I_EQUNR       = P_EQUNR
            I_WERKS       = P_WERKS
            I_TPLNR       = P_TPLNR
       IMPORTING
            RETURN        = RETURN
            E_SUBMT       = LV_SUBMT
       TABLES
            T_BOM_HIRACHY = LT_BOM_TEMP
            T_STB         = LT_STB.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*-- Check Logic  Same BOM.
  PERFORM MAKE_UP_BOM_LIST TABLES  LT_BOM_TEMP
                            USING  P_EQUNR
                                   P_TOGGLE
                                   P_WERKS
                                   P_TPLNR.

  PERFORM MAKE_UP_COLUMN_TREE USING P_TOGGLE.

ENDFORM.                    " GET_BOM_LIST
*&---------------------------------------------------------------------*
*&      Form  MAKE_UP_BOM_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_EQUNR  text
*      -->P_P_TOGGLE  text
*      -->P_P_WERKS  text
*      -->P_P_TPLNR  text
*----------------------------------------------------------------------*
FORM MAKE_UP_BOM_LIST TABLES LT_BOM_TEMP STRUCTURE IT_BOM_LIST
                      USING  P_EQUNR
                             P_TOGGLE
                             P_WERKS
                             P_TPLNR.

  DATA: LV_TABIX  LIKE SY-TABIX,            "sy-tabix
        LV_INDEX  LIKE SY-TABIX,
        LV_SUFFIX TYPE DATATYPE-CHAR0128,   "suffix
        LV_NEW_NODE_KEY(12).                "Node_key
  DATA: LV_CHILD TYPE MATNR.

  CLEAR IT_BOM_LIST.    REFRESH IT_BOM_LIST.

  IT_BOM_LIST[] = LT_BOM_TEMP[].

  DATA : LV_SUF_IND(3) TYPE N.   "//ASS'Y
  DATA : LV_SUF_IND2(4) TYPE N.  "//Material

  CLEAR LV_SUF_IND.

  LOOP AT LT_BOM_TEMP.
    CLEAR LV_TABIX.
    LV_TABIX = SY-TABIX.

    AT FIRST.
      CLEAR LV_SUFFIX.
      PERFORM GENERATE_SUFFIX_BY_RANDOM CHANGING LV_SUFFIX.
    ENDAT.

    LV_CHILD = LT_BOM_TEMP-MATNR_CHILD.
    IF LT_BOM_TEMP-MTART = 'IBAU'.
      LV_SUF_IND = LV_SUF_IND + 1.
      CONCATENATE LV_SUFFIX LV_SUF_IND
                            INTO LT_BOM_TEMP-MATNR_CHILD.
    ELSEIF LT_BOM_TEMP-MTART = 'ERSA'.
      LV_SUF_IND2 = LV_SUF_IND2 + 1.
      CONCATENATE LV_SUFFIX LV_SUF_IND2
                            INTO LT_BOM_TEMP-MATNR_CHILD.
    ENDIF.

    CLEAR LV_NEW_NODE_KEY.
    MOVE : LT_BOM_TEMP-MATNR_CHILD  TO  LV_NEW_NODE_KEY .

    READ TABLE IT_BOM_LIST WITH KEY MATNR_CHILD = LV_CHILD.
    MOVE : IT_BOM_LIST-MATNR_PARENT TO LT_BOM_TEMP-MATNR_PARENT.

    CLEAR IT_BOM_LIST.
    MOVE-CORRESPONDING LT_BOM_TEMP TO IT_BOM_LIST.
    MODIFY IT_BOM_LIST INDEX LV_TABIX.

    LOOP AT IT_BOM_LIST WHERE MATNR_PARENT = LV_CHILD.
      LV_INDEX = SY-TABIX.
      IT_BOM_LIST-MATNR_PARENT = LV_NEW_NODE_KEY.
      MODIFY IT_BOM_LIST INDEX LV_INDEX.
    ENDLOOP.

    MODIFY LT_BOM_TEMP INDEX LV_TABIX.

    PERFORM ACC_IT_BOM_LIST_NODE_TABLE TABLES IT_BOM_LIST_NODE
                                       USING LV_CHILD
                                             LT_BOM_TEMP-MATNR_PARENT
                                             LT_BOM_TEMP-MATNR_CHILD
                                             LT_BOM_TEMP-MTART
                                             LT_BOM_TEMP-MAKTX.
  ENDLOOP.

ENDFORM.                    " MAKE_UP_BOM_LIST
*&---------------------------------------------------------------------*
*&      Form  GENERATE_SUFFIX_BY_RANDOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUFFIX  text
*----------------------------------------------------------------------*
FORM GENERATE_SUFFIX_BY_RANDOM CHANGING P_SUFFIX.
  DATA: LV_RND_VALUE TYPE DATATYPE-CHAR0128,
        LV_LENGTH    TYPE DATATYPE-INTEGER2.

  CLEAR: LV_RND_VALUE, LV_LENGTH.

* Must initialize Random Value
  CALL FUNCTION 'RANDOM_INITIALIZE'
       EXPORTING
            CHARSET = C_ALPHABET.

  LV_LENGTH = STRLEN( C_ALPHABET ).

  CALL FUNCTION 'RANDOM_C_BY_SET'
       EXPORTING
            LEN_MIN   = 3
            LEN_MAX   = 3
            CHAR_MIN  = 1
            CHAR_MAX  = LV_LENGTH
            CHARSET   = C_ALPHABET
       IMPORTING
            RND_VALUE = LV_RND_VALUE.

  P_SUFFIX = LV_RND_VALUE.
ENDFORM.                    " GENERATE_SUFFIX_BY_RANDOM
*&---------------------------------------------------------------------*
*&      Form  MAKE_UP_COLUMN_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BOM_LIST  text
*----------------------------------------------------------------------*
FORM MAKE_UP_COLUMN_TREE USING P_TOGGLE.
  LOOP AT IT_BOM_LIST.
    CLEAR: EQUI_NODE, EQUI_ITEM.

    EQUI_NODE-NODE_KEY  = IT_BOM_LIST-MATNR_CHILD.
    EQUI_NODE-RELATKEY  = IT_BOM_LIST-MATNR_PARENT.
    EQUI_NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
    EQUI_NODE-HIDDEN    = ' '.
    EQUI_NODE-DISABLED  = ' '.
    EQUI_NODE-ISFOLDER  = IT_BOM_LIST-ISFOLDER.

    IF   IT_BOM_LIST-MTART EQ 'IBAU'.
      EQUI_NODE-N_IMAGE   = '@BS@'.
      EQUI_NODE-EXP_IMAGE = '@BS@'.
    ELSEIF IT_BOM_LIST-MTART EQ 'ERSA'.
      EQUI_NODE-N_IMAGE   = '@7U@'.
      EQUI_NODE-EXP_IMAGE = '@7U@'.
    ELSE.
      EQUI_NODE-N_IMAGE   = '@N5@'.
      EQUI_NODE-EXP_IMAGE = '@N5@'.
    ENDIF.

    EQUI_NODE-EXPANDER  = 'X'.
    EQUI_ITEM-NODE_KEY  = IT_BOM_LIST-MATNR_CHILD.
    EQUI_ITEM-ITEM_NAME = 'COLUMN1'.
    EQUI_ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
    EQUI_ITEM-TEXT      = IT_BOM_LIST-MAKTX.

    IF P_TOGGLE EQ 'MHRC'.
      PERFORM FILL_COLUMN_TREE USING EQUI_NODE_ITAB
                                     EQUI_ITEM_ITAB
                                     EQUI_NODE
                                     EQUI_ITEM.
    ENDIF.

    CLEAR EQUI_NODE.
    EQUI_ITEM-NODE_KEY  = IT_BOM_LIST-MATNR_CHILD.
    EQUI_ITEM-ITEM_NAME = 'COLUMN2'.
    EQUI_ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.

    CLEAR IT_BOM_LIST_NODE.
    READ TABLE IT_BOM_LIST_NODE
                    WITH KEY NODE_KEY = IT_BOM_LIST-MATNR_CHILD.
    IF SY-SUBRC = 0.
      EQUI_ITEM-TEXT = IT_BOM_LIST_NODE-MATNR_CHILD.
    ENDIF.

    IF P_TOGGLE EQ 'MHRC'.
      PERFORM FILL_COLUMN_TREE USING EQUI_NODE_ITAB
                                     EQUI_ITEM_ITAB
                                     EQUI_NODE
                                     EQUI_ITEM.
    ENDIF.

    CLEAR EQUI_NODE.
    EQUI_ITEM-NODE_KEY  = IT_BOM_LIST-MATNR_CHILD.
    EQUI_ITEM-ITEM_NAME = 'COLUMN3'.
    EQUI_ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
    EQUI_ITEM-TEXT      = IT_BOM_LIST-ZMENGE.

    IF P_TOGGLE EQ 'MHRC'.
      PERFORM FILL_COLUMN_TREE USING EQUI_NODE_ITAB
                                     EQUI_ITEM_ITAB
                                     EQUI_NODE
                                     EQUI_ITEM.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MAKE_UP_COLUMN_TREE
*&---------------------------------------------------------------------*
*&      Form  MANAGE_IMAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NODE  text
*      -->P_0859   text
*----------------------------------------------------------------------*
FORM MANAGE_IMAGE  USING P_NODE P_FLAG.

  CALL FUNCTION 'Z_FPM_UPLOAD_IMAGE'
       EXPORTING
            NODE_KEY        = P_NODE
            UP_FLAG         = P_FLAG
       EXCEPTIONS
            CANCEL_PROCESS  = 1
            FAILURE_PROCESS = 2
            DUPLICATE_ERROR = 3
            NOT_EXIST       = 4
            OTHERS          = 5.
  CASE SY-SUBRC.
    WHEN 0.
      IF P_FLAG EQ 'X'.
        MESSAGE I000 WITH TEXT-M04.
      ELSE.
        MESSAGE I000 WITH TEXT-M09.
      ENDIF.
    WHEN 1.
      MESSAGE I000 WITH TEXT-M05.
    WHEN 2.
      MESSAGE I000 WITH TEXT-M06.
    WHEN 3.
      MESSAGE I000 WITH TEXT-M07.
    WHEN 4.
      MESSAGE I000 WITH TEXT-M10.
    WHEN 5.
      MESSAGE I000 WITH TEXT-M08.
  ENDCASE.
ENDFORM.                    " MANAGE_IMAGE
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FCODE  text
*----------------------------------------------------------------------*
FORM CALL_TRANSACTIONS USING    P_TCODE.
  CALL TRANSACTION P_TCODE USING BDC_TAB
                           MODE  'E'
                           UPDATE 'S'
                           MESSAGES INTO MSG_TAB.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " CALL_TRANSACTIONS
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0925   text
*      -->P_0926   text
*      -->P_0927   text
*----------------------------------------------------------------------*
FORM GENERATE_BDC_DATA USING DYNBEGIN NAME VALUE.
  CLEAR BDC_TAB.

  IF DYNBEGIN = 'X'.
    MOVE: NAME     TO BDC_TAB-PROGRAM,
          VALUE    TO BDC_TAB-DYNPRO,
          DYNBEGIN TO BDC_TAB-DYNBEGIN.
    APPEND BDC_TAB.
  ELSE.
    MOVE: NAME     TO BDC_TAB-FNAM,
          VALUE    TO BDC_TAB-FVAL.
    APPEND BDC_TAB.
  ENDIF.
ENDFORM.                    " GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  ACC_IT_FUNC_LIST_NODE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_NODE_TPLNR  text
*      -->P_LIT_NODE_TPLMA  text
*      -->P_LV_CHILD  text
*      -->P_LIT_NODE_PLTXT  text
*----------------------------------------------------------------------*
FORM ACC_IT_FUNC_LIST_NODE_TABLE USING P_CHILD
                                       P_TPLMA
                                       P_TPLNR
                                       P_TEXT.

  CLEAR IT_FUNC_LIST_NODE.
  MOVE : P_TPLNR  TO IT_FUNC_LIST_NODE-TPLNR,
         P_TPLMA  TO IT_FUNC_LIST_NODE-TPLMA,
         P_CHILD  TO IT_FUNC_LIST_NODE-NODE_KEY,
         P_TEXT   TO IT_FUNC_LIST_NODE-PLTXT,
         ' '      TO IT_FUNC_LIST_NODE-ISFOLDER.

  APPEND IT_FUNC_LIST_NODE.
ENDFORM.                    " ACC_IT_FUNC_LIST_NODE_TABLE
*&---------------------------------------------------------------------*
*&      Form  FILL_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FUNC_NODE_ITAB  text
*      -->P_ACCUM_NODE_ITAB  text
*      -->P_FUNC_NODE  text
*----------------------------------------------------------------------*
FORM FILL_TREE TABLES NODE_ITAB STRUCTURE NODE_STR
                      ACCUM_ITAB STRUCTURE NODE_STR
               USING  P_NODE LIKE NODE_STR.

  DATA: NODE LIKE LDTREENODE.

  CLEAR NODE.
  NODE-NODE_KEY = P_NODE-NODE_KEY.
  NODE-ISFOLDER = P_NODE-ISFOLDER.
  NODE-TEXT     = P_NODE-TEXT.
  NODE-EXPANDER = P_NODE-EXPANDER.

  IF P_NODE-RELATKEY NE SPACE.
    NODE-RELATKEY  = P_NODE-RELATKEY.
    NODE-RELATSHIP = CL_GUI_SIMPLE_TREE=>RELAT_LAST_CHILD.
  ENDIF.

  APPEND NODE TO: NODE_ITAB,
                  ACCUM_ITAB.
ENDFORM.                    " FILL_TREE
*&---------------------------------------------------------------------*
*&      Form  DEFINE_EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINE_EVENT_DOUBLE_CLICK.
  CLEAR EVENTS[].

  EVENT-EVENTID = CL_GUI_SIMPLE_TREE=>EVENTID_NODE_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  EVENT-EVENTID = CL_GUI_SIMPLE_TREE=>EVENTID_NODE_CONTEXT_MENU_REQ.
  EVENT-APPL_EVENT = ' '.
  APPEND EVENT TO EVENTS.

*-- Performance Tunning 을 위해 I/F 변경
*-- 점명 까지만 Display하고 나머지는 노드 확장시 처리
  EVENT-EVENTID = CL_GUI_SIMPLE_TREE=>EVENTID_EXPAND_NO_CHILDREN.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

* process PAI if context menu select event occurs
  CALL METHOD FUNC_TREE->SET_CTX_MENU_SELECT_EVENT_APPL
    EXPORTING APPL_EVENT = 'X'.

  CALL METHOD FUNC_TREE->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS = EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

  SET HANDLER WA_APPLICATION->HANDLE_NODE_DOUBLE_CLICK
                              FOR FUNC_TREE.
  SET HANDLER WA_APPLICATION->HANDLE_NODE_CONTEXT_MENU_REQ
                              FOR FUNC_TREE.
  SET HANDLER WA_APPLICATION->HANDLE_NODE_CONTEXT_MENU_SEL
                              FOR FUNC_TREE.
  SET HANDLER WA_APPLICATION->HANDLE_EXPAND_NO_CHILDREN
                              FOR FUNC_TREE.

ENDFORM.                    " DEFINE_EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  CREATE_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_TREE.
  PERFORM GET_ROOT_AND_TOP_LEVEL.

  PERFORM GET_WERKS.

ENDFORM.                    " CREATE_TREE
*&---------------------------------------------------------------------*
*&      Form  GET_ROOT_AND_TOP_LEVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ROOT_AND_TOP_LEVEL.
  CLEAR  : FUNC_NODE_ITAB,  FUNC_NODE_ITAB2,
           ACCUM_NODE_ITAB, ACCUM_NODE_ITAB_2.
  REFRESH: FUNC_NODE_ITAB,  FUNC_NODE_ITAB2,
           ACCUM_NODE_ITAB, ACCUM_NODE_ITAB_2.

  CLEAR WA_FIRST_ROOT.
  WA_FIRST_ROOT = 'ROOT'.

*--Root
  CLEAR FUNC_NODE.
  FUNC_NODE-NODE_KEY = 'ROOT'.
  FUNC_NODE-TEXT     = TEXT-T01.
  FUNC_NODE-ISFOLDER = 'X'.

  PERFORM FILL_TREE TABLES FUNC_NODE_ITAB ACCUM_NODE_ITAB
                    USING  FUNC_NODE.

  PERFORM ACC_IT_FUNC_LIST_NODE_TABLE USING FUNC_NODE-NODE_KEY
                                            ' '
                                            WA_LAST_ROOT
                                            FUNC_NODE-TEXT.

  REFRESH IT_T001.  CLEAR IT_T001.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_T001
          FROM T001
          WHERE SPRAS = SY-LANGU
*          AND   LAND1 = 'KR'
*          AND   WAERS = 'KRW'
          AND   KTOPL = 'HNA1'.

  CHECK SY-SUBRC = 0.

  LOOP AT IT_T001.
    CLEAR FUNC_NODE.
    FUNC_NODE-NODE_KEY = IT_T001-BUKRS.
    FUNC_NODE-RELATKEY = 'ROOT'.
    FUNC_NODE-TEXT     = IT_T001-BUTXT.
    FUNC_NODE-ISFOLDER = 'X'.

    PERFORM FILL_TREE TABLES FUNC_NODE_ITAB
                             ACCUM_NODE_ITAB
                      USING  FUNC_NODE.

    PERFORM ACC_IT_FUNC_LIST_NODE_TABLE USING FUNC_NODE-NODE_KEY
                                              FUNC_NODE-RELATKEY
                                              FUNC_NODE-NODE_KEY
                                              FUNC_NODE-TEXT.
  ENDLOOP.

ENDFORM.                    " GET_ROOT_AND_TOP_LEVEL
*&---------------------------------------------------------------------*
*&      Form  GET_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_WERKS.
  DATA : BEGIN OF LT_PLANT OCCURS 0,
          BWKEY  LIKE T001K-BWKEY,
          NAME1  LIKE T001W-NAME1,
         END OF LT_PLANT.

  CLEAR IT_WERKS.    REFRESH IT_WERKS.

  LOOP AT IT_T001.
    REFRESH LT_PLANT. CLEAR LT_PLANT.

    SELECT B~BWKEY A~NAME1
          INTO CORRESPONDING FIELDS OF TABLE LT_PLANT
               FROM T001W AS A INNER JOIN T001K AS B
                 ON A~WERKS = B~BWKEY
               WHERE B~BUKRS = IT_T001-BUKRS .
    IF  SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    RANGES : LRA_WERKS FOR T001W-WERKS.

    LRA_WERKS-SIGN = 'I'. LRA_WERKS-OPTION = 'EQ'.
*    LRA_WERKS-LOW = 'P001'.  APPEND LRA_WERKS.
*    LRA_WERKS-LOW = 'E001'.  APPEND LRA_WERKS.
*    LRA_WERKS-LOW = '1100'.  APPEND LRA_WERKS.
*    LRA_WERKS-LOW = '1200'.  APPEND LRA_WERKS.
*    LRA_WERKS-LOW = '1300'.  APPEND LRA_WERKS.
*    LRA_WERKS-LOW = '1400'.  APPEND LRA_WERKS.
*    LRA_WERKS-LOW = '1500'.  APPEND LRA_WERKS.
    CLEAR LRA_WERKS.

    LOOP AT LT_PLANT.
*      IF LT_PLANT-BWKEY IN LRA_WERKS.
*        CONTINUE.
*      ENDIF.

      CLEAR: FUNC_NODE.
      FUNC_NODE-NODE_KEY = LT_PLANT-BWKEY.
      FUNC_NODE-RELATKEY = IT_T001-BUKRS.

      FUNC_NODE-TEXT     = LT_PLANT-NAME1.
      FUNC_NODE-ISFOLDER = 'X'.
      FUNC_NODE-EXPANDER = 'X'.

      PERFORM FILL_TREE TABLES FUNC_NODE_ITAB  ACCUM_NODE_ITAB
                        USING  FUNC_NODE.

      PERFORM ACC_IT_FUNC_LIST_NODE_TABLE USING FUNC_NODE-NODE_KEY
                                                FUNC_NODE-RELATKEY
                                                LT_PLANT-BWKEY
                                                FUNC_NODE-TEXT.

      CLEAR IT_WERKS.
      MOVE : LT_PLANT-BWKEY TO IT_WERKS-WERKS,
             LT_PLANT-NAME1 TO IT_WERKS-NAME1.
      APPEND IT_WERKS.
    ENDLOOP.
  ENDLOOP.

  FUNC_NODE_ITAB2[] = FUNC_NODE_ITAB[].
ENDFORM.                    " GET_WERKS
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_COMMAND INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: RETURN_CODE TYPE I.

*-- USER COMMAND를 처리하기 전에 이벤트를 처리하기 위해
*-- 아래의 METHOD 사용
  CALL METHOD CL_GUI_CFW=>DISPATCH
    IMPORTING RETURN_CODE = RETURN_CODE.

*-- RETURN_CODE 가 이벤트일때는 OK_CODE 를 CLEAR 하고 EXIT.
  IF RETURN_CODE <> CL_GUI_CFW=>RC_NOEVENT.
    CLEAR OK_CODE.
    EXIT.
  ENDIF.

*-- USER COMMAND 처리..
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      PERFORM FREE_PREVIOUS_OBJECT USING OK_CODE.
  ENDCASE.

  CLEAR OK_CODE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  FREE_PREVIOUS_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OK_CODE  text
*----------------------------------------------------------------------*
FORM FREE_PREVIOUS_OBJECT USING    P_TOGGLE.
  WA_INIT_FLG = 'X'.

  CHECK WA_CHOICE NE P_TOGGLE.       "선택 메뉴를 다시 눌렀는지 Check
  CLEAR WA_INIT_FLG.

* Container를 Free 시키면 포함하는 Object도 동시에 Free됨
  IF P_TOGGLE EQ 'MHRC'.                         "위치별

    IF NOT DOCKING_LEFT IS INITIAL.
      CALL METHOD DOCKING_LEFT->FREE
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2.
      IF SY-SUBRC <> 0.
        MESSAGE A000.
      ENDIF.
      CLEAR DOCKING_LEFT.
      CLEAR FUNC_TREE.
    ENDIF.

    IF NOT EQUI_TREE IS INITIAL.
      CALL METHOD EQUI_TREE->FREE
       EXCEPTIONS
         CNTL_ERROR        = 1
         CNTL_SYSTEM_ERROR = 2.
      IF SY-SUBRC <> 0.
        MESSAGE A000.
      ENDIF.
      CLEAR EQUI_TREE.
    ENDIF.

    WA_TOGGLE = 'MHRC'.
    WA_CHOICE = 'MHRC'.

  ELSE.                                          "설비별

    IF NOT DOCKING_LEFT IS INITIAL.
      CALL METHOD DOCKING_LEFT->FREE
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2.
      IF SY-SUBRC <> 0.
        MESSAGE A000.
      ENDIF.
      CLEAR DOCKING_LEFT.
      CLEAR FUNC_TREE.
    ENDIF.

    IF NOT DOCKING_TOP IS INITIAL.
      CALL METHOD DOCKING_TOP->FREE
        EXCEPTIONS
          CNTL_SYSTEM_ERROR = 1
          CNTL_ERROR        = 2.
      IF SY-SUBRC <> 0.
        MESSAGE A000.
      ENDIF.
      CLEAR DOCKING_TOP.
      CLEAR PICTURE.
    ENDIF.

    IF NOT EQUI_TREE IS INITIAL.
      CALL METHOD EQUI_TREE->FREE
       EXCEPTIONS
         CNTL_ERROR        = 1
         CNTL_SYSTEM_ERROR = 2.
      IF SY-SUBRC <> 0.
        MESSAGE A000.
      ENDIF.
      CLEAR EQUI_TREE.
    ENDIF.

    WA_TOGGLE = 'MEQU'.
    WA_CHOICE = 'MEQU'.
  ENDIF.

ENDFORM.                    " FREE_PREVIOUS_OBJECT
