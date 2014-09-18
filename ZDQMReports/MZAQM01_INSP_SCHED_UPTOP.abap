************************************************************************
* Program Name      : SAPMZAQM01_INSP_SCHED
* Author            : SeungLyong, Lee
* Creation Date     : 2003.09.04.
* Specifications By : SeungLyong, Lee
* Development Request No :
* Addl Documentation:
* Description       : Inspection Scheduling Layout
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

PROGRAM  SAPMZAQM01_INSP_SCHED  MESSAGE-ID ZMQM     .

*&&& Data Declaration.  &&&*
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool
TABLES : FELD.      "//Screen Object Structure

*-- Include Program ( Include Constants or etc)
INCLUDE : ZQM_INCLUDE_POOL01. "/Inspection Constants and etc


*-- SAP Scripts Object Interface
*TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name  "//Table Description)
TABLES : ZTQM_INSP_HDR,  "/Inspection Scheduling Header
         ZTQM_INSP_ITEM, "/Inspection Scheduling Item
         ZTQM_INSP_ITEM_F, "/Item(Flat Type)-Collective Summary
         ZTQM_INSP_S_ITEM. "/Sub Item : ISIR, Regualr, MS

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_INSP_SCH_HDR,   "/Inspection Scheduling Header Str
         ZSQM_INSP_SCH_ITEM_F. "/Inspection Scheduling Item(Flat Type)

TABLES : ZSQM_INSP_SCH_EXCEL_FLAT. "/Excel Format Structure

TABLES : ZSCA_TIME_STAMP.   "/Time Stamp Structre.

*// Variables Definition for Excel Handling - Start
***--- OLE DECLARATION.
INCLUDE OLE2INCL.   "//OLE

*- EXCEL sheet using OLE automation.
* handles for OLE objects
DATA: H_EXCEL_OBJECT   TYPE OLE2_OBJECT,    " Excel object
      H_WORKBOOK_LIST  TYPE OLE2_OBJECT,    " list of workbooks
      H_WORKBOOK       TYPE OLE2_OBJECT,    " workbook
      H_CELL           TYPE OLE2_OBJECT,    " cell
      H_FONT           TYPE OLE2_OBJECT,    " font
      H_COLOR          TYPE OLE2_OBJECT.    " COLOR

*-- Variable for Excel Position
DATA : WA_HOR  TYPE I VALUE 0,    "//가로
       WA_VER  TYPE I VALUE 1,    "//세로
       WA_BOLD TYPE N .           "//BOLD(Thickness)


*// Variables Definition for Excel Handling - End.

*//InfoType;()
*//Cluster or Import Parameter;(Parameter Name)

*//Controls(for only Screen Control Element);(TC_ , or TS_)
*-- TABLE CONTROL
CONTROLS: TC_0200  TYPE TABLEVIEW USING SCREEN 0200.
CONTROLS: TC_0300  TYPE TABLEVIEW USING SCREEN 0300.
*//Type (Table Structure);(TY_ )- Table or Structure


*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'
CONSTANTS : C_MARK   VALUE 'X'.
*-- Screen Control Mode
CONSTANTS : C_CREATE(7)  TYPE C VALUE 'CREATE',
            C_CHANGE(7)  TYPE C VALUE 'CHANGE',
            C_DISPLAY(7) TYPE C VALUE 'DISPLAY'.
*-- Process Status
CONSTANTS : C_UPLOADED(8)  TYPE C VALUE 'UPLOADED',
            C_SAVED(8)     TYPE C VALUE 'SAVED'.

**-- Inspection Type Constants
*CONSTANTS : C_INSP_TYPE_REGULAR TYPE QPART VALUE '8920',
*            C_INSP_TYPE_ISIR    TYPE QPART VALUE '8910',
*            C_INSP_TYPE_MS      TYPE QPART VALUE '8930'.
*
**-- Scheduling Status Constants
*CONSTANTS : C_CREATION  TYPE ZQINSPSTATUS VALUE '1',
*            C_RELEASE   TYPE ZQINSPSTATUS VALUE '2',
*            C_DONTUSE   TYPE ZQINSPSTATUS VALUE '3'.
*
**-- Inspection Purpose Code
**--  - ISIR    : KATALOGART = 'P',  CODEGRUPPE = 'ISIR' CODE= *
**--  - REGULAR : KATALOGART = 'P',  CODEGRUPPE = 'RE',  CODE='01'
*CONSTANTS : C_KATALOGART      TYPE QKATART   VALUE 'P',
*            C_CODEGRUPPE_ISIR TYPE QCODEGRP  VALUE 'IS',
*            C_CODEGRUPPE_REGU TYPE QCODEGRP  VALUE 'RE',
*            C_CODE_REGU       TYPE QCODE     VALUE '01'.
*
*CONSTANTS : C_VORGLFNR_ISIR_REG  TYPE QLFNKN VALUE '00000001',
*            C_VORGLFNR_MS_1      TYPE QLFNKN VALUE '00000001',
*            C_VORGLFNR_MS_2      TYPE QLFNKN VALUE '00000002'.

**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.
DATA : WA_MODE(7) TYPE C,
       WA_STATUS(8) TYPE C.

*--
DATA : WA_RETURN     LIKE	BAPIRETURN1.   "Return Values

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.


*-- Table Control Field Variables
DATA : WA_FLDTXT    LIKE FELD-NAME,  "Field Name Variable
       WA_CUR_LINE  LIKE FELD-LINE.  "Field Line Variable

*-- Table Control Variables
DATA : WA_CON_LINES LIKE SY-LOOPC.  " LINES OF TABLECONTROL
DATA : WA_COUNT     TYPE   I.       " COUNT Variables
DATA : WA_LINES     LIKE SY-TABIX.
DATA : WA_TABIX     LIKE SY-TABIX.
DATA : WA_SEL_LINE  LIKE SY-TABIX.  "Select Line of T/C
DATA : WA_TCNAME    LIKE FELD-NAME. "table control Field Name

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?
*-- Global Structure of Radio Button for Screen Control according
*-- Selection of ISIR/Regular
DATA : BEGIN OF ST_DIST,
         REGU   TYPE C  VALUE C_MARK,
         ISIR   TYPE C,
       END OF ST_DIST.


DATA : WA_FILENAME   LIKE RLGRAP-FILENAME .

DATA : WA_ZSQM_INSP_SCH_ITEM_F LIKE ZSQM_INSP_SCH_ITEM_F.
DATA : WA_ZSQM_INSP_SCH_HDR    LIKE ZSQM_INSP_SCH_HDR. "/Header Backup

DATA : BEGIN OF WA_ITEM_F_B,  "/BACKUP INTERNAL TABLE Data for T/C
        VEHICLE    TYPE ZVEHICLE,
        IYEAR      TYPE ZQYEAR,
        ART        TYPE QPART,
        KATALOGART TYPE QKATART,
        CODEGRUPPE TYPE QCODEGRP,
        EXTWG      TYPE EXTWG,
        EWBEZ      TYPE EWBEZ.
        INCLUDE STRUCTURE  ZSCA_TIME_STAMP.
DATA :  END OF WA_ITEM_F_B.


*//Internal Tables and Index Fields;(IT_), (I_)
*-- Inspection Scheduling Item(Flat Type) for Excel Upload and T/C
DATA : IT_ZSQM_INSP_SCH_ITEM_F LIKE ZSQM_INSP_SCH_ITEM_F
                              OCCURS 0 WITH HEADER LINE.

DATA : IT_ZSQM_INSP_SCH_EXCEL_FLAT LIKE ZSQM_INSP_SCH_EXCEL_FLAT
                              OCCURS 0 WITH HEADER LINE.

*-- Internale Tables with structure as sama as DB
DATA: IT_ZTQM_INSP_HDR    LIKE ZTQM_INSP_HDR
                              OCCURS 0 WITH HEADER LINE,
      IT_ZTQM_INSP_ITEM   LIKE ZTQM_INSP_ITEM
                              OCCURS 0 WITH HEADER LINE,
      IT_ZTQM_INSP_ITEM_F LIKE ZTQM_INSP_ITEM_F
                              OCCURS 0 WITH HEADER LINE,
      IT_ZTQM_INSP_S_ITEM LIKE ZTQM_INSP_S_ITEM
                              OCCURS 0 WITH HEADER LINE.

*-- Internale Tables for backup of Data on change mode.
DATA: IT_ITEM_B   LIKE ZTQM_INSP_ITEM
                              OCCURS 0 WITH HEADER LINE,
      IT_ITEM_F_B LIKE ZTQM_INSP_ITEM_F
                              OCCURS 0 WITH HEADER LINE,
      IT_S_ITEM_B LIKE ZTQM_INSP_S_ITEM
                              OCCURS 0 WITH HEADER LINE.

*//Ranges; (R_)
*RANGES :  "/

*//Field Symbols; <FS_>
*-- TABLE CONTROLS VARIABLE(field-symbols)
FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL. "table control
"                              Table_control Object(CXTAB)

*//Field Group;

***//& Selection Screen Definition(Parameters Select-Option)
*-- Paramerters : (P_), Select-Options : (S_)
*SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
*SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )

*-- Selection for Selection Screen
*START-OF-SELECTION.
*END-OF-SELECTION.

*// Event Handling(Except Selection Screen (Flow)event)
*Load of Program.

*Initialization.

***//Macro Definitions
*DEFINE RANGE_MACRO.
*END-OF-DEFINITION.

**<<<<<<<<< Program Main / Subroutine / Flow Logic >>>>>>>>>>>>**
