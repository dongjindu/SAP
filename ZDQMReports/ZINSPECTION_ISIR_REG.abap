************************************************************************
* Program Name      : SAPMZRQM25R_NOTI_SUMMARY
* Author            : SeungLyong, Lee
* Creation Date     : 2003.10.27.
* Specifications By : SeungLyong, Lee
* Pattern           : 1.2.4 Call Screen + 3.1 General
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Notification Summary report
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 03/19/2004 SLLEE        UD1K907648   Add Coding field(QM-20040318-002)
*
*
************************************************************************

REPORT  SAPMZRQM25R_NOTI_SUMMARY    .

*&&& Data Declaration.  &&&*
TYPE-POOLS ZQMT1.  "/QM-Type group for inspection
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool
**
TABLES : FELD.      "//Screen Object Structure

*-- Include Program ( Include Constants or etc)
INCLUDE : ZQM_INCLUDE_POOL01. "/Inspection Constants and etc
INCLUDE <ICON>.

*//Tables;(TABLES : Table_Name  "//Table Description)
TABLES : T001W,
         QALS,
         QPGR,
         QPCD,
         MARA,
         MARC,
         JEST.

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_IS_RES_SEL. "/Inspection Result Status -Selection screen
TABLES : ZSQM_IS_R_ISE, "/Inspection Result Status : ISIR - E001
         ZSQM_IS_R_ISP, "/Inspection Result Status : ISIR - P001
         ZSQM_IS_R_REG. "/Inspection Result Status : Regular - P001/E001
TABLES : ZSQM_IS_R_LIST, "/Inspection Result Status : SQL - List
         ZTQM_MAT_EO,
         ZTQM_QNS_ITEM,
         ZTQM_QNS_IT_MS,
         ZTQM_VENDOR_ID.

*//Type (Table Structure);(TY_ )- Table or Structure

*-- PF-Status : Excluding Function Code table
TYPES: BEGIN OF TY_FCODE,
        FCODE LIKE RSMPE-FUNC,
      END OF TY_FCODE.
DATA: IT_EX_FUNC TYPE STANDARD TABLE OF TY_FCODE WITH
                       NON-UNIQUE DEFAULT KEY INITIAL SIZE 5,
      WA_EX_FUNC TYPE TY_FCODE.

DATA : IT_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS WITH HEADER LINE.

*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'
CONSTANTS : C_MARK   VALUE 'X'.
CONSTANTS : C_FIRST(15)  TYPE C VALUE 'FIRST',
            C_DETAIL(15) TYPE C VALUE 'DETAIL'.

*-- Report Level control constants
CONSTANTS : C_BASIC(10)  TYPE C VALUE 'BASIC'.


**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.
DATA: WA_VENDOR LIKE ZTQM_VENDOR_ID-LIFNR.
DATA :  WA_RENEWAL_FLG.
*--
DATA : WA_RETURN     LIKE	BAPIRETURN1.   "Return Values

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.
DATA : WA_REPID LIKE SY-REPID,
       WA_DYNNR LIKE SY-DYNNR.

*-- Table Control Field Variables
DATA : WA_FLDTXT    LIKE FELD-NAME,  "Field Name Variable
       WA_CUR_LINE  LIKE FELD-LINE.  "Field Line Variable

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?
*-- Global Structure of Radio Button for Screen Control according
*-- Selection of ISIR/Regular
DATA : BEGIN OF ST_DIST,
         REGU   TYPE C,
         ISIR   TYPE C,
       END OF ST_DIST.
*-- Control flag for interface with other program
DATA : WA_IMPORT_EXIST.

*-- List Box Set Flag
DATA : WA_LISTBOX_SET.

*-- Error check flag
DATA : WA_ERROR_FOUNDED.

*-- Inspection date
DATA : WA_FIRSTDAY  TYPE QPRSTART, "/Inspection Start Date
       WA_LASTDAY   TYPE QPRENDE.  "/End Date of the Inspection
*-- field count variable for dynamic access
DATA : WA_FLD_ISRE_CNT  TYPE I, "/Count of ISIR or Regular
       WA_FLD_MS_CNT   TYPE I.  "/Count of MS

*- Selected Ext. material group and responsible person
DATA : WA_EXTWG  TYPE EXTWG,
       WA_RESPP  TYPE ZQM_RESP_P.

*-- Report Level control
DATA : WA_LEVEL(10) TYPE C.

*//Ranges; (R_)
RANGES :
         R_LIFNR       FOR ZSQM_IS_RES_SEL-LIFNR_L,
         R_CODEGRP_VH  FOR ZSQM_IS_RES_SEL-CODEGRP_VH,
         R_CODE_VH     FOR ZSQM_IS_RES_SEL-CODE_VH,
         R_EXTWG       FOR ZSQM_IS_RES_SEL-EXTWG_L,
         R_PASTR       FOR QALS-PASTRTERM,
         R_PAEND       FOR QALS-PAENDTERM.

*         R_STAT        FOR JEST-STAT.
data:  P_IYEAR   like ZTQM_QNS_ITEM-IYEAR.
*       P_PASTR    like    QALS-PASTRTERM ,
*         P_PAEND  like QALS-PAENDTERM.

*//Internal Tables and Index Fields;(IT_), (I_)
*- Inspection Result Status : ISIR - E001
DATA : IT_ZSQM_IS_R_ISE  LIKE ZSQM_IS_R_ISE OCCURS 0 WITH HEADER LINE.
*- Inspection Result Status : ISIR - P001
DATA : IT_ZSQM_IS_R_ISP  LIKE ZSQM_IS_R_ISP OCCURS 0 WITH HEADER LINE.
*- Inspection Result Status : Regular - P001/E001
DATA : IT_ZSQM_IS_R_REG  LIKE ZSQM_IS_R_REG OCCURS 0 WITH HEADER LINE.
*- Inspection Result Status : SQL - List
*DATA : IT_ZSQM_IS_R_LIST LIKE ZSQM_IS_R_LIST OCCURS 0 WITH HEADER LINE.

*DATA: BEGIN OF IT_ZSQM_IS_R_LIST OCCURS 0 .
*       INCLUDE STRUCTURE ZSQM_IS_R_LIST.
*data:  MODL LIKE ZTQM_MAT_EO-MODL.
*data:  EODT LIKE ZTQM_MAT_EO-EODT.
*DATA: END OF IT_ZSQM_IS_R_LIST.

data: IT_ZSQM_IS_R_LIST like ZSQM_IS_R_LIST_P occurs 0 with header line.
data: it_zsqm_reg_list_P like ZSQM_REG_LIST_P occurs 0 with header line.
*- Inspection scheduling - Detail List
DATA : IT_ZSQM_IS_R_LIST_D LIKE ZSQM_IS_R_LIST OCCURS 0
                                                   WITH HEADER LINE.

**-- Vehicle/Engine type master temp table for Text input

*--/ Work area structure same as DB structure
*DATA :

*/// ALV Control....: Start
* Control Framework Basic Class
CLASS CL_GUI_CFW      DEFINITION LOAD.

*// Declare reference variables, the container and internal table
DATA: WA_CUSTOM_CONTROL    TYPE   SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

*DATA : WA_CONTROL_OBJECT TYPE REF TO CL_GUI_CONTROL.

*-- ALV LIST STRUCTURE
DATA : WA_STRUCTURE_NAME LIKE DD02L-TABNAME.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED. "/ALV Event Handling

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

* Global variables for attributes or etc of ALV GRID
DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO. "/The Layout Structure
DATA : IT_FIELDCAT   TYPE LVC_T_FCAT WITH HEADER LINE, "for Basic
       IT_FIELDCAT_D TYPE LVC_T_FCAT WITH HEADER LINE, "for Detail
       IT_SORT       TYPE LVC_T_SORT WITH HEADER LINE.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

*/// ALV Control....: End


*****//& Selection Screen Definition(Parameters Select-Option)
**-- Paramerters : (P_), Select-Options : (S_)


*
**-- Seclection Screen Flow Logic Control Event Handling
**AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
** ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
** ON EXIT-COMMAND )
*AT SELECTION-SCREEN OUTPUT.
**  SET TITLEBAR '1000'.
*
*AT SELECTION-SCREEN ON BLOCK BLK.
*  CHECK SY-UCOMM = 'ONLI'.
**-- Start of Selection
*START-OF-SELECTION.
***-- End of Selection.
*END-OF-SELECTION.


SET SCREEN 9000.

DEFINE RANGE_SET.
  MOVE : 'I' TO &1-SIGN.
  IF NOT &2 IS INITIAL AND
         &3 IS INITIAL.
    MOVE :'EQ' TO &1-OPTION.
    MOVE : &2  TO &1-LOW.
  ELSEIF NOT &2 IS INITIAL AND
         NOT &3 IS INITIAL.
    MOVE : 'BT' TO &1-OPTION.
    MOVE : &2   TO &1-LOW,
           &3   TO &1-HIGH.
  ELSEIF  &2 IS INITIAL AND
         NOT &3 IS INITIAL.
    MOVE : 'EQ' TO &1-OPTION.
    MOVE : &3   TO &1-LOW.
  ENDIF.
  APPEND &1.

END-OF-DEFINITION.


****//& Selection Screen Definition(Parameters Select-Option)
**-- Paramerters : (P_), Select-Options : (S_)
*SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
*SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )
*AT SELECTION-SCREEN OUTPUT.
*  SET TITLEBAR '1000'.

*AT SELECTION-SCREEN.
*  CHECK SY-UCOMM = 'ONLI'.


*-- Selection for Selection Screen
*START-OF-SELECTION.
**-- End of Selection.
*END-OF-SELECTION.


*// Event Handling(Except Selection Screen (Flow)event)
LOAD-OF-PROGRAM.
*- Set Vehicle/Engine type catagory
  ZSQM_IS_RES_SEL-KATART_VH = C_VE_ENG_CAT_TYPE.

* Get defualt vendor
PERFORM GET_DEFAULT_VENDOR.


*- Set default date for inspection start date
  CONCATENATE SY-DATUM+0(4)
              '0101'    INTO ZSQM_IS_RES_SEL-PASTR_L.
*  MOVE : SY-DATUM TO ZSQM_IS_RES_SEL-PASTR_H.
MOVE : SY-DATUM TO ZSQM_IS_RES_SEL-PAEND_L.

*-- Get Selection data for Selection from Memory ID using IMPORT
  IMPORT ZSQM_IS_RES_SEL  FROM MEMORY ID 'ZSCHEDULE'.

  IF SY-SUBRC = 0.  "/Import data exist.
    WA_IMPORT_EXIST = C_MARK.

    FREE MEMORY ID 'ZSCHEDULE'.
    MOVE : ZSQM_IS_RES_SEL-EXTWG_L TO WA_EXTWG.

*-- Set Inspection type radio button selection by inspection type
    IF ZSQM_IS_RES_SEL-ART = C_INSP_TYPE_ISIR.
      ST_DIST-ISIR = C_MARK.
    ELSEIF ZSQM_IS_RES_SEL-ART = C_INSP_TYPE_REGULAR.
      ST_DIST-REGU = C_MARK.
    ENDIF.

  ENDIF.

*Initialization.
.


***//Macro Definitions
*-- macro : RANGE_SET &1 &2 &3
*--           &1 - Range Variable
*--           &2 - Low Variable
*--           &3 - high Variable


****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.

    METHODS:

    HANDLE_DOUBLE_CLICK
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW E_COLUMN,

    HANDLE_TOOLBAR
        FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING E_OBJECT E_INTERACTIVE,

    HANDLE_USER_COMMAND
        FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
            IMPORTING E_UCOMM.

  PRIVATE SECTION.

ENDCLASS.

* lcl_event_receiver (Definition)
****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

*-- / Double Click
  METHOD HANDLE_DOUBLE_CLICK.
* .The event DOUBLE_CLICK provides parameters for row and column
*   of the click. Use row parameter to select a line of the
*   corresponding internal table.

*- read selected row from internal table
*   READ TABLE IT_ZSQM_EQ_CLASS INDEX E_ROW-INDEX INTO LW_ZSQM_EQ_CLASS.

  ENDMETHOD.                           "handle_double_click

*-- / Handling Tollbar control
  METHOD HANDLE_TOOLBAR.

*This event is triggered by the ALV each time the toolbar of the control
* needs to be regenerated. To add self-defined functions to the
*toolbar, you trigger the event using method set_toolbar_interactive and
* write an event handler method

    DATA: LS_TOOLBAR  TYPE STB_BUTTON.

  ENDMETHOD.
*-------------------------------------------------------------------

*-- / Handling User defined commands for Toolbar
  METHOD HANDLE_USER_COMMAND.

*   In event handler method for event USER_COMMAND: Query your
*   function codes defined in Class Definition and react accordingly.

    DATA : LT_ROWS   TYPE LVC_T_ROW,
           LW_LINE_ROW LIKE LINE OF LT_ROWS.
    DATA : LW_LINES TYPE I.

  ENDMETHOD.                           "handle_user_command

ENDCLASS.
*
* lcl_event_receiver (Implementation)
*===================================================================



**<<<<<<<<< Program Main / Subroutine / Flow Logic >>>>>>>>>>>>**
*&------------------------------------------------------------------*
*&      Form  GET_TEXT_FOR_SELECT_CRITERION
*&------------------------------------------------------------------*
FORM GET_TEXT_FOR_SELECT_CRITERION.

ENDFORM.                    " GET_TEXT_FOR_SELECT_CRITERION
*&------------------------------------------------------------------*
*&      Form  RETRIEVE_SELECT_CRITERION
*&------------------------------------------------------------------*
FORM RETRIEVE_SELECT_CRITERION.

*-- Select All equipment List by Select criterion From ZVQM_EQ_CL_STAT

ENDFORM.                    " RETRIEVE_SELECT_CRITERION
*&------------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT  OUTPUT
*&------------------------------------------------------------------*
MODULE CREATE_ALV_OBJECT OUTPUT.

  IF GRID_CONTAINER IS INITIAL. "/Not Created Container for ALV GRID
    CLEAR WA_RENEWAL_FLG.
*- Create Container('GRID_CONTAINER') with Custom Contro on screen
    CREATE OBJECT GRID_CONTAINER
           EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
           EXCEPTIONS
            CNTL_ERROR = 1
            CNTL_SYSTEM_ERROR = 2
            CREATE_ERROR = 3
            LIFETIME_ERROR = 4
            LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    IF SY-SUBRC NE 0.
      WA_REPID = SY-REPID.
      CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
                TITEL = WA_REPID
                TXT2  = SY-SUBRC
                TXT1  = 'The control can not be created'(E02).
    ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
    CREATE OBJECT ALV_GRID
           EXPORTING I_PARENT = GRID_CONTAINER
                     I_APPL_EVENTS = 'X'.

*-- Prepare Setting Attributes and etc of ALV Object
    PERFORM SET_ATTRIBUTES_ALV_GRID.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure
    PERFORM MASK_COLUMNS_OF_ALV_GRID TABLES IT_FIELDCAT.

*        Set field attribute/Text by by Inspection type(ISIR, Regular)
      PERFORM SET_FIELD_ATTR_FOR_ISIR_P  TABLES IT_FIELDCAT.


**-- adjust field sort and subtotal to display total of column
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
*                                        IT_FIELDCAT.
* Set field attribute/Text by by Inspection type(ISIR, Regular)
     PERFORM SET_FIELD_ATTR_FOR_DETAIL  TABLES IT_FIELDCAT
                                                IT_FIELDCAT_D.
*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
    PERFORM SET_TABLE_TO_ALV.

*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
    CREATE OBJECT EVENT_RECEIVER.
    SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALV_GRID.
*-   toolbar control event
    SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALV_GRID.
    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALV_GRID.

*- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
    CALL METHOD ALV_GRID->SET_TOOLBAR_INTERACTIVE.

    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.


  ENDIF.


  IF NOT GRID_CONTAINER IS INITIAL AND "/Not Created  for ALV GRID
     WA_RENEWAL_FLG = C_MARK.
    CLEAR WA_RENEWAL_FLG.

***********added
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure
    PERFORM MASK_COLUMNS_OF_ALV_GRID TABLES IT_FIELDCAT.

*        Set field attribute/Text by by Inspection type(ISIR, Regular)
      PERFORM SET_FIELD_ATTR_FOR_ISIR_P  TABLES IT_FIELDCAT.


***********end

*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
    PERFORM SET_TABLE_TO_ALV.


    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.
  ENDIF.

ENDMODULE.                 " CREATE_ALV_OBJECT  OUTPUT
*&------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  SET PF-STATUS '9100'.
  IF WA_LEVEL = C_FIRST.
    SET TITLEBAR  '9100'.
  ELSE.
    SET TITLEBAR  '9100'.
  ENDIF.
ENDMODULE.                 " STATUS_9100  OUTPUT
*&------------------------------------------------------------------*
*&      Form  REFRESH_ALV_GRID_DATA_DISP
*&------------------------------------------------------------------*
FORM REFRESH_ALV_GRID_DATA_DISP.
  CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
*         EXPORTING
*           IS_STABLE      =
*           I_SOFT_REFRESH =
*         EXCEPTIONS
*           FINISHED       = 1
*           others         = 2
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " REFRESH_ALV_GRID_DATA_DISP
*&------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.

  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
*          PERFORM FREE_ALV_GRID.

      LEAVE TO SCREEN 0.

    WHEN 'REFRESH'.
*-- Get data from DB AND Process
      PERFORM GET_DATA_AND_PROCESS.

      WA_RENEWAL_FLG = C_MARK.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&-----------------------------------------------------------------*
*&      Module  EXIT_9000  INPUT
*&-----------------------------------------------------------------*
MODULE EXIT_9000 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT'.
      PERFORM FREE_ALV_GRID.
      LEAVE TO SCREEN 0.

    WHEN 'RW'.
      PERFORM FREE_ALV_GRID.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " EXIT_9000  INPUT
*&-----------------------------------------------------------------*
*&      Module  EXIT_9100  INPUT
*&-----------------------------------------------------------------*
MODULE EXIT_9100 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT'.

      LEAVE TO SCREEN 0.

    WHEN 'RW'.

      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " EXIT_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.
*//-- Set Layout Structure


  WA_IS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = C_MARK.  "/Optimize column width
*  WA_IS_LAYOUT-DETAILTITL = ''.        "/Title bar of detail screen
*  WA_IS_LAYOUT-GRID_TITLE = ''.        "/ Title bar text
*  WA_IS_LAYOUT-KEYHOT      = C_MARK.    "/ Key columns as hotspot
*  WA_IS_LAYOUT-NO_HEADERS  = C_MARK.     "/Hide column headings
*  WA_IS_LAYOUT-NO_HGRIDLN  = C_MARK.     "/Hide horizontal grid lines
*  WA_IS_LAYOUT-NO_VGRIDLN  = C_MARK.     "/Hide vertical grid lines
*  WA_IS_LAYOUT-NO_MERGING  = C_MARK.     "/Disable cell merging
*  WA_IS_LAYOUT-NO_ROWMARK  = C_MARK.     "/Disable row selections
*  WA_IS_LAYOUT-NO_TOOLBAR  = C_MARK.     "/Hide toolbar
  WA_IS_LAYOUT-NUMC_TOTAL  = C_MARK. "/Allow totals for NUMC
*  WA_IS_LAYOUT-S_DRAGDROP  = LW_S_DRAGDROP. "/Drag & Drop control

  WA_IS_LAYOUT-SEL_MODE  = 'A'. "/mode for select col and row
*  WA_IS_LAYOUT-SGL_CLK_HD = C_MARK. "/sorts the list whe column clicked

*//-- Set Variant Structure
  WA_VARIANT-REPORT = SY-REPID.
  WA_VARIANT-USERNAME = SY-UNAME.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9100  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9100 OUTPUT.

*  CASE ZSQM_NOTI_SEL-QMART.
*    WHEN '1'.
*      LOOP AT SCREEN.
*        IF SCREEN-GROUP1 = 'Q23'.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDLOOP.
*    WHEN '2' OR '3' OR '4'.
*      LOOP AT SCREEN.
*        IF SCREEN-GROUP1 = 'Q1'.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDLOOP.
*    WHEN OTHERS.
*  ENDCASE.

ENDMODULE.                 " MODIFY_SCREEN_9100  OUTPUT
*&----------------------------------------------------------------*
*&      Form  MASK_COLUMNS_OF_ALV_GRID
*&-----------------------------------------------------------------*
FORM MASK_COLUMNS_OF_ALV_GRID TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.

  CLEAR : WA_STRUCTURE_NAME.

  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.

CASE ZSQM_IS_RES_SEL-ART.
          WHEN C_INSP_TYPE_ISIR.

  WA_STRUCTURE_NAME = 'ZSQM_IS_R_LIST_P'.
 WHEN C_INSP_TYPE_REGULAR.
 WA_STRUCTURE_NAME = 'ZSQM_REG_LIST_P'.
 endcase.

* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = WA_STRUCTURE_NAME
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].



* Set field attribute

*  LOOP AT PT_FIELDCAT.
*    IF PT_FIELDCAT-FIELDNAME = 'QMNUM'.
*      PT_FIELDCAT-KEY_SEL = C_MARK.
*      PT_FIELDCAT-KEY      = C_MARK.
*    ELSEIF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_COD'.
*      PT_FIELDCAT-COLTEXT = 'Coding'(T49).
*    ELSEIF PT_FIELDCAT-FIELDNAME = 'CODEGRP_VH'.
**      PT_FIELDCAT-KEY_SEL = C_MARK.
*      PT_FIELDCAT-COLTEXT = 'V/E Group'(T50).
*    ELSEIF PT_FIELDCAT-FIELDNAME = 'CODE_VH'.
*      PT_FIELDCAT-COLTEXT = 'V/E'(T51).
**    ELSEIF PT_FIELDCAT-FIELDNAME = 'PARNR'.
**      PT_FIELDCAT-COLTEXT = 'Vendor/Resp.Dept.'(T52).
*    ELSEIF PT_FIELDCAT-FIELDNAME = 'MATNR'.
*      PT_FIELDCAT-OUTPUTLEN = 18.
**    ELSEIF PT_FIELDCAT-FIELDNAME = 'DEVICEID'.
**      PT_FIELDCAT-COLTEXT = 'Device data'.
*    ELSEIF PT_FIELDCAT-FIELDNAME = 'MAKTX'.
*
*    ELSEIF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_FE'.
*      PT_FIELDCAT-COLTEXT = 'Defect type'(T53).
*      PT_FIELDCAT-OUTPUTLEN = 20.
*    ELSEIF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_UR'.
*      PT_FIELDCAT-COLTEXT = 'Cause'(T54).
*      PT_FIELDCAT-OUTPUTLEN = 20.
*    ELSEIF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_OT'.
*      PT_FIELDCAT-COLTEXT = 'Defect Location'(T55).
*      PT_FIELDCAT-OUTPUTLEN = 20.
*    ELSEIF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_AT'.
*      PT_FIELDCAT-COLTEXT = 'Activity'(T56).
*      PT_FIELDCAT-OUTPUTLEN = 20.
*    ELSEIF PT_FIELDCAT-FIELDNAME = 'TXT04'.
*      PT_FIELDCAT-COLTEXT = 'Noti.Status'(T57).
*    ELSEIF PT_FIELDCAT-FIELDNAME = 'TASK_TXT04'.
*      PT_FIELDCAT-COLTEXT = 'Task Status'(T58).
*
*    ELSEIF PT_FIELDCAT-FIELDNAME = 'PLANDAT'.
*      PT_FIELDCAT-COLTEXT = 'Plan date'(T59).
*    ELSEIF PT_FIELDCAT-FIELDNAME = 'SENDDAT'.
*      PT_FIELDCAT-COLTEXT = 'Send date'(T60).
*    ENDIF.
*    MODIFY PT_FIELDCAT.
*  ENDLOOP.

ENDFORM.                    " MASK_COLUMNS_OF_ALV_GRID
*&------------------------------------------------------------------*
*&      Form  FREE_ALV_GRID
*&------------------------------------------------------------------*
FORM FREE_ALV_GRID.
  CHECK NOT ALV_GRID IS INITIAL.
  CALL METHOD ALV_GRID->FREE.
  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    WA_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = WA_REPID
              TXT2  = SY-SUBRC
              TXT1  = TEXT-E01.
  ENDIF.

ENDFORM.                    " FREE_ALV_GRID
*&-----------------------------------------------------------------*
*&      Module  F4_HELP_MANAGER  INPUT
*&------------------------------------------------------------------*
MODULE F4_HELP_MANAGER INPUT.

  PERFORM GET_PARTNER_VALUE   USING 'Z2'  "/Partner Function:Manager
                                    'ZSQM_NOTI_SEL-PARNR_MAG'
                                    'ZSQM_NOTI_SEL-NAME_LIST'.

ENDMODULE.                 " F4_HELP_MANAGER  INPUT
*&-------------------------------------------------------------------*
*&      Module  F4_HELP_VENDOR  INPUT
*&-------------------------------------------------------------------*
MODULE F4_HELP_VENDOR INPUT.
  PERFORM GET_PARTNER_VALUE   USING 'Z5'  "/Partner Function:Manager
                                    'ZSQM_NOTI_SEL-LIFNUM'
                                    'ZSQM_NOTI_SEL-NAME_LIF'.
ENDMODULE.                 " F4_HELP_VENDOR  INPUT
*&------------------------------------------------------------------*
*&      Module  F4_HELP_PARNR_VERA  INPUT
*&------------------------------------------------------------------*
MODULE F4_HELP_PARNR_VERA INPUT.
  PERFORM GET_PARTNER_VALUE   USING 'Z3'  "/Partner Function:Manager
                                    'ZSQM_NOTI_SEL-PARNR_VERA'
                                    'ZSQM_NOTI_SEL-NAME_VERA'.
ENDMODULE.                 " F4_HELP_PARNR_VERA  INPUT

*&-----------------------------------------------------------------*
*&      Form  GET_PARTNER_VALUE
*&-----------------------------------------------------------------*
FORM GET_PARTNER_VALUE USING    VALUE(P_PARVW)
                                VALUE(P_PARNR_FIELD)
                                VALUE(P_NAME_FIELD).
*// TPAR : Business Partner: Functions Table
  DATA : LW_NRART LIKE TPAR-NRART. "/Type of partner number
  DATA : LW_PARNR LIKE IHPA-PARNR. "/Selected Partner
  DATA : LW_NAME_LIST LIKE USER_ADDR-NAME_TEXTC. "User name


  DATA: BEGIN OF LW_DYNPFIELDS OCCURS 100.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END OF LW_DYNPFIELDS.
  DATA: LW_DYNAME LIKE D020S-PROG.
  DATA: LW_DYNUMB LIKE D020S-DNUM.

  CHECK NOT P_PARVW IS INITIAL.

*-- get partner type of partner number(partner function)
  SELECT SINGLE NRART INTO LW_NRART
    FROM TPAR
      WHERE PARVW = P_PARVW.

  CHECK SY-SUBRC = 0.
*--Search Help using partner type(LW_NRART)-HR organization or SAP User
  CALL FUNCTION 'SEARCH_OM_PARTNER'
       EXPORTING
            ACT_NRART         = LW_NRART
*            ACTIVE_PLVAR      = ' '
            SEARCH_STRING     = ' '
*            RESTRICTION_OTYPE = ' '
*            RESTRICTION_OBJID = '00000000'
       IMPORTING
            SEL_PARNR         = LW_PARNR
       EXCEPTIONS
            NO_ACTIVE_PLVAR   = 1
            NOT_SELECTED      = 2
            NO_OM_OTYPE       = 3
            OTHERS            = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CHECK NOT LW_PARNR IS INITIAL.


  CASE P_PARVW.
    WHEN 'Z2'.                            "/Manager
      SELECT SINGLE NAME_TEXTC     INTO LW_NAME_LIST
        FROM USER_ADDR
           WHERE BNAME = LW_PARNR.
    WHEN 'Z3'.                           "/Resp. Department
      SELECT SINGLE ORGTX    INTO LW_NAME_LIST
        FROM T527X
           WHERE ORGEH = LW_PARNR
             AND ENDDA > SY-DATUM
             AND SPRSL = SY-LANGU.
  ENDCASE.

*-- Update screen field data using FUNCTION 'DYNP_VALUES_UPDATE'

  LW_DYNAME = SY-REPID.
  LW_DYNUMB = SY-DYNNR.

  LW_DYNPFIELDS-FIELDNAME  = P_PARNR_FIELD.
  LW_DYNPFIELDS-FIELDVALUE = LW_PARNR.
  APPEND LW_DYNPFIELDS.
  LW_DYNPFIELDS-FIELDNAME  = P_NAME_FIELD.
  LW_DYNPFIELDS-FIELDVALUE = LW_NAME_LIST.
  APPEND LW_DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME     = LW_DYNAME
            DYNUMB     = LW_DYNUMB
       TABLES
            DYNPFIELDS = LW_DYNPFIELDS.



ENDFORM.                    " GET_PARTNER_VALUE
*&-----------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&-----------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.

      PERFORM FREE_ALV_GRID.
      LEAVE TO SCREEN 0.

    WHEN 'ONLI'.

**-- Set range variables for SQL
      PERFORM SET_RANGE_VAR_FOR_SEARCH.
*
**-- Get data from DB AND Process
*      PERFORM GET_DATA_AND_PROCESS.


*    - Set inspection type by user selection of radio button
      PERFORM SET_INSPECTION_TYPE.
*    - Set plant by Vehicle/Engine type Code Group
      PERFORM SET_PLAT_DATA_BY_CODE_G.

*    - Set range data by user input for query to DB
      PERFORM SET_RANGE_VALUE.
*   - Get data and count for Basic list.
      PERFORM GET_DATA_AND_LIST_BASIC.

    CHECK NOT IT_ZSQM_IS_R_LIST[] IS INITIAL.



      WA_RENEWAL_FLG = C_MARK.

*       IF WA_IMPORT_EXIST = ' '. "/Normal
*        WA_LEVEL = C_BASIC.
        CALL SCREEN 9100.
*        endif.


  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&------------------------------------------------------------------*
*&      Form  SET_RANGE_VAR_FOR_SEARCH
*&------------------------------------------------------------------*
FORM SET_RANGE_VAR_FOR_SEARCH.
  DATA : LW_PARNR LIKE IHPA-PARNR.

  CLEAR :
*  R_MAWERK,     R_PARNR_MAG, R_LIFNUM, R_PARNR_VERA,
           R_CODEGRP_VH, R_CODE_VH.
*           R_ERDAT,  R_QMART,
*           R_STAT, R_TASK_STAT,      R_QMGRP,      R_QMCOD.
  REFRESH :
*  R_MAWERK,     R_PARNR_MAG, R_LIFNUM, R_PARNR_VERA,
             R_CODEGRP_VH, R_CODE_VH.
*             R_ERDAT,  R_QMART,
*             R_STAT, R_TASK_STAT,     R_QMGRP,      R_QMCOD.
*
*
*  IF NOT ZSQM_NOTI_SEL-MAWERK IS INITIAL.
*    RANGE_SET R_MAWERK  ZSQM_NOTI_SEL-MAWERK ''.
*  ENDIF.
*
*  IF NOT ZSQM_NOTI_SEL-PARNR_MAG IS INITIAL.
*    LW_PARNR = ZSQM_NOTI_SEL-PARNR_MAG.
*    SHIFT LW_PARNR LEFT  DELETING LEADING  '0'.
*
*    RANGE_SET R_PARNR_MAG  LW_PARNR ''.
*  ENDIF.
*
*  IF NOT ZSQM_NOTI_SEL-LIFNUM IS INITIAL.
*    RANGE_SET R_LIFNUM     ZSQM_NOTI_SEL-LIFNUM ''.
*  ENDIF.
*
*  IF NOT ZSQM_NOTI_SEL-PARNR_VERA IS INITIAL.
*    LW_PARNR = ZSQM_NOTI_SEL-PARNR_VERA.
*    SHIFT LW_PARNR LEFT  DELETING LEADING  '0'.
*
*    RANGE_SET R_PARNR_VERA     LW_PARNR ''.
*  ENDIF.
*
  IF NOT ZSQM_IS_RES_SEL-CODEGRP_VH IS INITIAL.
    RANGE_SET R_CODEGRP_VH     ZSQM_IS_RES_SEL-CODEGRP_VH ''.
    IF NOT ZSQM_IS_RES_SEL-CODE_VH IS INITIAL.
      RANGE_SET R_CODE_VH    ZSQM_IS_RES_SEL-CODE_VH ''.
    ENDIF.
  ENDIF.
*
*  IF NOT ZSQM_NOTI_SEL-ERDAT_L IS INITIAL.
*    RANGE_SET R_ERDAT    ZSQM_NOTI_SEL-ERDAT_L ZSQM_NOTI_SEL-ERDAT_H.
*  ENDIF.
*
*  CASE ZSQM_NOTI_SEL-QMART.
*    WHEN '1'.
*      RANGE_SET  R_QMART  C_QMART_INT_DEF ''.
*    WHEN '2'.
*      RANGE_SET  R_QMART  C_QMART_LINE_EXT ''.
*    WHEN '3'.
*      RANGE_SET  R_QMART  C_QMART_INSP_EXT ''.
*    WHEN '4'.
*      RANGE_SET  R_QMART  C_QMART_LINE_EXT  C_QMART_INSP_EXT.
*  ENDCASE.
*
**- Notification Status.
*  IF WA_ST-CRT = C_MARK.
*    RANGE_SET  R_STAT C_STAT_NOTI_CREATION ''.
*  ENDIF.
*  IF WA_ST-REL = C_MARK.
*    RANGE_SET  R_STAT C_STAT_NOTI_RELEASE ''.
*  ENDIF.
*  IF WA_ST-CMP = C_MARK.
*    RANGE_SET  R_STAT C_STAT_NOTI_COMPLETION ''.
*  ENDIF.
*  IF R_STAT[] IS INITIAL.
*    MESSAGE W000(ZMQM) WITH
*      'At least, one of Notification status must be selected.'(W02).
*    STOP.
*  ENDIF.
*
**-  Coding
*  IF NOT ZSQM_NOTI_SEL-QMCOD IS INITIAL.
*    RANGE_SET : R_QMGRP    ZSQM_NOTI_SEL-QMGRP '',
*                R_QMCOD    ZSQM_NOTI_SEL-QMCOD ''.
*  ENDIF.
*
**-- Notification Task Status
*  CHECK ZSQM_NOTI_SEL-QMART = '1'.                          "/'Q1'.
*  RANGE_SET  R_TASK_STAT   C_STAT_TASK_CREATION    ''.
*  RANGE_SET  R_TASK_STAT   C_STAT_TASK_RELEASE     ''.
*  RANGE_SET  R_TASK_STAT   C_STAT_TASK_COMPLETION  ''.
*  RANGE_SET  R_TASK_STAT   C_STAT_TASK_SUCCESSFUL  ''.

ENDFORM.                    " SET_RANGE_VAR_FOR_SEARCH
*&------------------------------------------------------------------*
*&      Module  CHECK_VH_ENG_INPUT  INPUT
*&------------------------------------------------------------------*
MODULE CHECK_VH_ENG_INPUT INPUT.


*  CHECK     ZSQM_NOTI_SEL-CODEGRP_VH IS INITIAL AND
*        NOT ZSQM_NOTI_SEL-CODE_VH    IS INITIAL.
*
*  MESSAGE E000(ZMQM)
*      WITH 'Please Input Vehicle/Engine Code Group.'(E20).

ENDMODULE.                 " CHECK_VH_ENG_INPUT  INPUT
*&------------------------------------------------------------------*
*&      Module  GET_TEXT_SCREEN_9000  INPUT
*&------------------------------------------------------------------*
MODULE GET_TEXT_SCREEN_9000 INPUT.

*  CLEAR : ZSQM_NOTI_SEL-NAME1,
*          ZSQM_NOTI_SEL-QMARTX,
*          ZSQM_NOTI_SEL-NAME_LIST,
*          ZSQM_NOTI_SEL-NAME_LIF,
*          ZSQM_NOTI_SEL-NAME_VERA,
*          ZSQM_NOTI_SEL-KURZTEXT_VH,
*          ZSQM_NOTI_SEL-KURZTEXT_G,
*          ZSQM_NOTI_SEL-KURZTEXT_COD.
*
*  IF NOT ZSQM_NOTI_SEL-MAWERK IS INITIAL.
*    SELECT SINGLE NAME1 INTO ZSQM_NOTI_SEL-NAME1
*        FROM T001W
*          WHERE WERKS = ZSQM_NOTI_SEL-MAWERK.
*    IF SY-SUBRC NE 0.
*      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-MAWERK
*                              'is not exist!'(E10).
*    ENDIF.
*
*  ENDIF.
*
*  CASE ZSQM_NOTI_SEL-QMART.
*    WHEN '1'.
*      SELECT SINGLE QMARTX INTO ZSQM_NOTI_SEL-QMARTX
*         FROM TQ80_T
*           WHERE QMART = 'Q1'
*             AND SPRAS = SY-LANGU.
*    WHEN OTHERS.
*
*      SELECT SINGLE QMARTX INTO ZSQM_NOTI_SEL-QMARTX
*         FROM TQ80_T
*           WHERE QMART = 'Q2'
*             AND SPRAS = SY-LANGU.
*
*      CLEAR TQ80_T.
*
*      SELECT SINGLE *   FROM TQ80_T
*           WHERE QMART = 'Q3'
*             AND SPRAS = SY-LANGU.
*
*      IF ZSQM_NOTI_SEL-QMART = '2'.
*
*      ELSEIF ZSQM_NOTI_SEL-QMART = '3' .
*        MOVE : TQ80_T-QMARTX TO ZSQM_NOTI_SEL-QMARTX.
*      ELSE .
*        CONCATENATE ZSQM_NOTI_SEL-QMARTX  '+' TQ80_T-QMARTX
*                 INTO ZSQM_NOTI_SEL-QMARTX SEPARATED BY SPACE.
*      ENDIF.
*
*  ENDCASE.
*
*  IF NOT ZSQM_NOTI_SEL-PARNR_MAG IS INITIAL.
*    SELECT SINGLE NAME_TEXTC     INTO ZSQM_NOTI_SEL-NAME_LIST
*        FROM USER_ADDR
*           WHERE BNAME = ZSQM_NOTI_SEL-PARNR_MAG.
*    IF SY-SUBRC NE 0.
*      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-MAWERK
*                              TEXT-E10.
*    ENDIF.
*  ENDIF.
*
*  IF NOT ZSQM_NOTI_SEL-LIFNUM IS INITIAL.
*    SELECT SINGLE NAME1  INTO ZSQM_NOTI_SEL-NAME_LIF
*      FROM LFA1
*        WHERE LIFNR = ZSQM_NOTI_SEL-LIFNUM.
*    IF SY-SUBRC NE 0.
*      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-LIFNUM
*                              TEXT-E10.
*    ENDIF.
*  ENDIF.
*
*  IF NOT ZSQM_NOTI_SEL-PARNR_VERA IS INITIAL.
*    SELECT SINGLE ORGTX    INTO ZSQM_NOTI_SEL-NAME_VERA
*      FROM T527X
*         WHERE ORGEH = ZSQM_NOTI_SEL-PARNR_VERA
*           AND ENDDA > SY-DATUM
*           AND SPRSL = SY-LANGU.
*    IF SY-SUBRC NE 0.
*      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-PARNR_VERA
*                              TEXT-E10.
*    ENDIF.
*  ENDIF.
*
*
*  IF NOT ZSQM_NOTI_SEL-CODEGRP_VH IS INITIAL.
*    SELECT SINGLE KURZTEXT INTO ZSQM_NOTI_SEL-KURZTEXT_G
*      FROM ZVQM_QPGRT01
*         WHERE KATALOGART = ZSQM_NOTI_SEL-KATART_VH
*           AND CODEGRUPPE = ZSQM_NOTI_SEL-CODEGRP_VH.
*
*    IF SY-SUBRC NE 0.
*      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-CODEGRP_VH
*                              TEXT-E10.
*    ENDIF.
*
*  ENDIF.
*
*  IF NOT ZSQM_NOTI_SEL-CODEGRP_VH IS INITIAL AND
*     NOT ZSQM_NOTI_SEL-CODE_VH    IS INITIAL.
*    SELECT SINGLE KURZTEXT_C INTO ZSQM_NOTI_SEL-KURZTEXT_VH
*      FROM ZVQM_VEHICLE
*         WHERE KATALOGART = ZSQM_NOTI_SEL-KATART_VH
*           AND CODEGRUPPE = ZSQM_NOTI_SEL-CODEGRP_VH
*           AND CODE       = ZSQM_NOTI_SEL-CODE_VH.
*
*    IF SY-SUBRC NE 0.
*      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-CODEGRP_VH
*                              ','
*                              ZSQM_NOTI_SEL-CODE_VH
*                              TEXT-E10.
*    ENDIF.
*
*  ENDIF.
*
*  IF NOT ZSQM_NOTI_SEL-QMCOD IS INITIAL.
*    SELECT SINGLE  KURZTEXT_C INTO ZSQM_NOTI_SEL-KURZTEXT_COD
*       FROM ZVQM_QPCT_EN
*         WHERE KATALOGART  = C_CODING_CATEGORY
*           AND CODEGRUPPE  = ZSQM_NOTI_SEL-QMGRP
*           AND CODE        = ZSQM_NOTI_SEL-QMCOD
*           AND SPRAS       = SY-LANGU
*           AND SPRAS_CD    = SY-LANGU.
*    IF SY-SUBRC NE 0.
*      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-QMCOD
*                              ','
*                              ''
*                              TEXT-E10.
*    ENDIF.
*
*  ENDIF.
*

ENDMODULE.                 " GET_TEXT_SCREEN_9000  INPUT
*&------------------------------------------------------------------*
*&      Form  SET_LISTBOX_QMART
*&------------------------------------------------------------------*
FORM SET_LISTBOX_QMART.
*  WA_NAME = 'ZSQM_NOTI_SEL-QMART'.
**  WA_VALUE-KEY = '1'. WA_VALUE-TEXT = 'Q1'.
**  APPEND WA_VALUE TO IT_LIST.
*  WA_VALUE-KEY = '2'. WA_VALUE-TEXT = 'Q2'.
*  APPEND WA_VALUE TO IT_LIST.
**  WA_VALUE-KEY = '3'. WA_VALUE-TEXT = 'Q3'.
**  APPEND WA_VALUE TO IT_LIST.
**  WA_VALUE-KEY = '4'. WA_VALUE-TEXT = 'Q2+Q3'.
**  APPEND WA_VALUE TO IT_LIST.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*       EXPORTING
*            ID     = WA_NAME
*            VALUES = IT_LIST.
ENDFORM.                    " SET_LISTBOX_QMART
*&-----------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&-----------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&------------------------------------------------------------------*
*&      Form  GET_DATA_USING_SEL_CRITERION
*&------------------------------------------------------------------*
FORM GET_DATA_USING_SEL_CRITERION.

*  DATA : LT_NOTI_SUM LIKE IT_ZSQM_NOTI_SUM OCCURS 0 WITH HEADER LINE.
*
*  DATA : BEGIN OF LT_NOTI_TASK OCCURS 0, "/task list of noti.
*              QMNUM      TYPE QMNUM,
*              MANUM      TYPE MANUM,
*              TASK_STAT  TYPE J_STATUS,
*              TASK_TXT04 TYPE J_TXT04,
*         END OF LT_NOTI_TASK.
*
*  DATA : LT_TASK_T LIKE LT_NOTI_TASK OCCURS 0 WITH HEADER LINE.
*
*  DATA : LW_NOTI_INDEX LIKE SY-TABIX,
*         LW_INDEX      LIKE SY-TABIX.
*
*  REFRESH : IT_ZSQM_NOTI_SUM.
*
*
**-<< Modified : 06/28/2004 - sllee : Req. by Mr. Kim  - Start
**--   - Exclude Noti. which have active status 'DLFL' : by subquery.
*
*  CASE ZSQM_NOTI_SEL-QMART.
*
*    WHEN '1'.
*
**     - Notification  list for Q1
*      SELECT A~QMNUM A~QMART A~KATART_VH A~CODEGRP_VH A~CODE_VH
*             A~MAWERK A~EXTWG A~KATART_AT A~CODEGRP_AT A~CODE_AT
*             E~PARNR A~MATNR  I~MAKTX A~DEVICEID B~FEKAT B~FEGRP
*B~FECOD
*             C~URKAT C~URGRP C~URCOD B~OTKAT  B~OTGRP B~OTEIL
*             F~STAT
*             A~QMGRP A~QMCOD
*          INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_NOTI_SUM
*        FROM ( ( ( (  QMEL AS A  INNER JOIN QMFE AS B
*           ON   A~QMNUM = B~QMNUM ) INNER JOIN QMUR AS C
*           ON   A~QMNUM = C~QMNUM
*            AND B~FENUM = C~FENUM ) INNER JOIN IHPA AS E
*           ON   A~OBJNR = E~OBJNR ) INNER JOIN JEST AS F
*           ON   A~OBJNR = F~OBJNR ) INNER JOIN MAKT AS I
*           ON   A~MATNR = I~MATNR
*        WHERE  A~MAWERK     IN R_MAWERK
*          AND  A~QMART      IN R_QMART
*          AND  A~KATART_VH  = ZSQM_NOTI_SEL-KATART_VH
*          AND  A~CODEGRP_VH IN R_CODEGRP_VH
*          AND  A~CODE_VH    IN R_CODE_VH
*          AND  A~ERDAT      IN R_ERDAT
**          AND  A~QMKAT      = C_CODING_CATEGORY
*          AND  A~QMGRP      IN R_QMGRP
*          AND  A~QMCOD      IN R_QMCOD
*          AND  B~FEGRP      IN (C_DEFECT_FEGRP1, C_DEFECT_FEGRP2)
*          AND  E~PARNR      IN R_PARNR_VERA
*          AND  E~PARVW      =  C_PARVW_RESP_DEP
*          AND  F~INACT      = ' '
*          AND  F~STAT       IN R_STAT
*          AND  I~SPRAS      = SY-LANGU
*          AND  NOT EXISTS ( SELECT *
*                              FROM QMEL AS K INNER JOIN JEST AS L
*                               ON K~OBJNR = L~OBJNR
*                                 WHERE K~QMNUM = A~QMNUM
*                                   AND L~STAT  = C_STAT_NOTI_DELETION
*                                   AND L~INACT = ' ' ) .
*
**-- Get task information of Notification  list for Q1
*
*      SELECT A~QMNUM  G~MANUM
*             H~STAT AS TASK_STAT
*          INTO CORRESPONDING FIELDS OF TABLE LT_NOTI_TASK
*        FROM  (  QMEL AS A  INNER JOIN QMSM AS G
*           ON   A~QMNUM = G~QMNUM ) INNER JOIN JEST AS H
*           ON   G~OBJNR = H~OBJNR
*         FOR ALL ENTRIES IN  IT_ZSQM_NOTI_SUM
*        WHERE  A~QMNUM      = IT_ZSQM_NOTI_SUM-QMNUM
*          AND  H~INACT      = ' '
*          AND  H~STAT       IN R_TASK_STAT
*          AND  G~MANUM      = ( SELECT MAX( MANUM ) FROM QMSM
*                                  WHERE QMNUM = A~QMNUM ).
*
**--   copy LT_NOTI_TASK to LT_TASK_T for Deleting not available task
*      LT_TASK_T[] = LT_NOTI_TASK[].
**--    if successful status of notification task is exist,
**      delete completion task
*      LOOP AT LT_TASK_T WHERE TASK_STAT = C_STAT_TASK_SUCCESSFUL .
*
*        READ TABLE LT_NOTI_TASK
*                       WITH KEY QMNUM     = LT_TASK_T-QMNUM
*                                TASK_STAT = C_STAT_TASK_COMPLETION.
*        IF SY-SUBRC = 0.
*          LW_NOTI_INDEX = SY-TABIX.
*          DELETE LT_NOTI_TASK INDEX LW_NOTI_INDEX.
*        ENDIF.
*      ENDLOOP.
*
*
**-- merge data
*      LOOP AT LT_NOTI_TASK.
*        CLEAR IT_ZSQM_NOTI_SUM.
*        READ TABLE IT_ZSQM_NOTI_SUM WITH KEY QMNUM = LT_NOTI_TASK-QMNUM
*.
*        IF SY-SUBRC = 0.
*          LW_INDEX = SY-TABIX.
*          MOVE-CORRESPONDING LT_NOTI_TASK TO IT_ZSQM_NOTI_SUM.
*          MODIFY IT_ZSQM_NOTI_SUM INDEX LW_INDEX.
*        ENDIF.
*      ENDLOOP.
*
*
*    WHEN OTHERS.
*
**     - Notification list for Q2, Q3 AND Q2 + Q3
*      SELECT A~QMNUM A~QMART A~KATART_VH A~CODEGRP_VH A~CODE_VH
*             A~MAWERK A~EXTWG A~KATART_AT A~CODEGRP_AT A~CODE_AT
*             E~PARNR A~MATNR I~MAKTX A~DEVICEID F~STAT B~FEKAT
*             B~FEGRP B~FECOD
*             C~URKAT C~URGRP C~URCOD B~OTKAT  B~OTGRP B~OTEIL
*             A~PLANDAT A~COMPLETED  A~SUCCESS
*             A~QMGRP A~QMCOD A~SENDDAT
*          INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_NOTI_SUM
*        FROM  ( ( ( (  QMEL AS A  INNER JOIN QMFE AS B
*           ON   A~QMNUM = B~QMNUM ) INNER JOIN QMUR AS C
*           ON   A~QMNUM = C~QMNUM
*            AND B~FENUM = C~FENUM ) INNER JOIN IHPA AS E
*           ON   A~OBJNR = E~OBJNR ) INNER JOIN JEST AS F
*           ON   A~OBJNR = F~OBJNR ) INNER JOIN MAKT AS I
*           ON   A~MATNR = I~MATNR
*        WHERE  A~MAWERK     IN R_MAWERK
*          AND  A~QMART      IN R_QMART
*          AND  A~KATART_VH  = ZSQM_NOTI_SEL-KATART_VH
*          AND  A~CODEGRP_VH IN R_CODEGRP_VH
*          AND  A~CODE_VH    IN R_CODE_VH
*          AND  A~ERDAT      IN R_ERDAT
**          AND  A~QMKAT      = C_CODING_CATEGORY
*          AND  A~QMGRP      IN R_QMGRP
*          AND  A~QMCOD      IN R_QMCOD
*          AND  B~FEGRP      IN (C_DEFECT_FEGRP1, C_DEFECT_FEGRP2)
*          AND  E~PARNR      IN R_LIFNUM
*          AND  E~PARVW      =  C_PARVW_VENDOR
*          AND  F~INACT      = ' '
*          AND  F~STAT       IN R_STAT
*          AND  I~SPRAS      = SY-LANGU
*          AND  NOT EXISTS ( SELECT *
*                              FROM QMEL AS K INNER JOIN JEST AS L
*                               ON K~OBJNR = L~OBJNR
*                                 WHERE K~QMNUM = A~QMNUM
*                                   AND L~STAT  = C_STAT_NOTI_DELETION
*                                   AND L~INACT = ' ' ) .
*
*      CHECK SY-SUBRC = 0.
*
**-- Fill Noti Task Status for Q2/Q3
*      LOOP AT IT_ZSQM_NOTI_SUM.
*        LW_NOTI_INDEX = SY-TABIX.
*
*        IF IT_ZSQM_NOTI_SUM-SUCCESS = C_MARK.
*
*          MOVE : C_STAT_TASK_SUCCESSFUL
*                       TO  IT_ZSQM_NOTI_SUM-TASK_STAT.
*
*        ELSEIF IT_ZSQM_NOTI_SUM-COMPLETED = C_MARK.
*
*          MOVE : C_STAT_TASK_COMPLETION
*                       TO IT_ZSQM_NOTI_SUM-TASK_STAT.
*
*        ELSEIF NOT IT_ZSQM_NOTI_SUM-PLANDAT IS INITIAL AND
*                   IT_ZSQM_NOTI_SUM-COMPLETED IS INITIAL.
*
*          MOVE : C_STAT_TASK_RELEASE
*                       TO IT_ZSQM_NOTI_SUM-TASK_STAT.
*
*        ELSEIF  IT_ZSQM_NOTI_SUM-PLANDAT IS INITIAL AND
*                IT_ZSQM_NOTI_SUM-COMPLETED  IS INITIAL.
*
*          MOVE : C_STAT_TASK_CREATION
*                       TO IT_ZSQM_NOTI_SUM-TASK_STAT.
*
*        ENDIF.
*
*        MODIFY IT_ZSQM_NOTI_SUM INDEX LW_NOTI_INDEX.
*      ENDLOOP.
*
*
*  ENDCASE.

*-<< Modified : 06/28/2004 - sllee : Req. by Mr. Kim  - End

ENDFORM.                    " GET_DATA_USING_SEL_CRITERION
*&---------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------*
MODULE SET_CURSOR_FIELD OUTPUT.
  SET CURSOR FIELD WA_FLDTXT LINE WA_CUR_LINE.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT
*&----------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&----------------------------------------------------------------*
MODULE GET_CURSOR_FIELD INPUT.
  CLEAR: WA_FLDTXT, WA_CUR_LINE.
  GET CURSOR FIELD WA_FLDTXT LINE WA_CUR_LINE.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT
*&------------------------------------------------------------------*
*&      Module  CONTROL_BY_QMART  INPUT
*&------------------------------------------------------------------*
MODULE CONTROL_BY_QMART INPUT.
*  CASE ZSQM_NOTI_SEL-QMART.
*    WHEN '1'.
*      CLEAR : ZSQM_NOTI_SEL-LIFNUM, ZSQM_NOTI_SEL-NAME_LIF.
*    WHEN '2' OR '3' OR '4'.
*      CLEAR : ZSQM_NOTI_SEL-PARNR_VERA, ZSQM_NOTI_SEL-NAME_VERA.
*    WHEN OTHERS.
*  ENDCASE.
ENDMODULE.                 " CONTROL_BY_QMART  INPUT
*&-----------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9000  OUTPUT
*&-----------------------------------------------------------------*
MODULE MODIFY_SCREEN_9000 OUTPUT.
*  CASE ZSQM_NOTI_SEL-QMART.
*    WHEN '1'.
*      LOOP AT SCREEN.
*        IF SCREEN-GROUP1 = 'Q23'.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDLOOP.
*    WHEN '2' OR '3' OR '4'.
*      LOOP AT SCREEN.
*        IF SCREEN-GROUP1 = 'Q1'.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDLOOP.
*    WHEN OTHERS.
*  ENDCASE.
ENDMODULE.                 " MODIFY_SCREEN_9000  OUTPUT
*&-----------------------------------------------------------------*
*&      Form  GET_CODE_TEXT_AND_MODIFY
*&------------------------------------------------------------------*
FORM GET_CODE_TEXT_AND_MODIFY.

***- Coding Code
*  LOOP AT IT_QMCOD.
*    CLEAR IT_ZSQM_NOTI_SUM.
*    MOVE : IT_QMCOD-KURZTEXT TO IT_ZSQM_NOTI_SUM-KURZTEXT_COD.
*    MODIFY IT_ZSQM_NOTI_SUM   TRANSPORTING KURZTEXT_COD
*             WHERE QMNUM NE ''
*               AND QMGRP = IT_QMCOD-QMGRP
*               AND QMCOD = IT_QMCOD-QMCOD.
*  ENDLOOP.
*
****-- Defect type code
*  LOOP AT IT_DEFECT.
*    CLEAR IT_ZSQM_NOTI_SUM.
**    MOVE : IT_DEFECT-FEKAT    TO IT_ZSQM_NOTI_SUM-FEKAT,
**           IT_DEFECT-FEGRP    TO IT_ZSQM_NOTI_SUM-FEGRP,
**           IT_DEFECT-FECOD    TO IT_ZSQM_NOTI_SUM-FECOD,
*    MOVE : IT_DEFECT-KURZTEXT TO IT_ZSQM_NOTI_SUM-KURZTEXT_FE.
*
*    MODIFY IT_ZSQM_NOTI_SUM  TRANSPORTING KURZTEXT_FE
*                 WHERE QMNUM NE ''
*                   AND FEKAT = IT_DEFECT-FEKAT
*                   AND FEGRP = IT_DEFECT-FEGRP
*                   AND FECOD = IT_DEFECT-FECOD.
*
*  ENDLOOP.
*
****-- Cause code
*
*  LOOP AT IT_CAUSE.
*    CLEAR IT_ZSQM_NOTI_SUM.
**    MOVE : IT_CAUSE-URKAT    TO IT_ZSQM_NOTI_SUM-URKAT,
**           IT_CAUSE-URGRP    TO IT_ZSQM_NOTI_SUM-URGRP,
**           IT_CAUSE-URCOD    TO IT_ZSQM_NOTI_SUM-URCOD,
*    MOVE : IT_CAUSE-KURZTEXT TO IT_ZSQM_NOTI_SUM-KURZTEXT_UR.
*
*    MODIFY IT_ZSQM_NOTI_SUM  TRANSPORTING KURZTEXT_UR
*                 WHERE QMNUM NE ''
*                   AND URKAT = IT_CAUSE-URKAT
*                   AND URGRP = IT_CAUSE-URGRP
*                   AND URCOD = IT_CAUSE-URCOD.
*
*  ENDLOOP.
*
****-- Defect location code
*  LOOP AT IT_DEFLOC.
*    CLEAR IT_ZSQM_NOTI_SUM.
**    MOVE : IT_DEFLOC-OTKAT    TO IT_ZSQM_NOTI_SUM-OTKAT,
**           IT_DEFLOC-OTGRP    TO IT_ZSQM_NOTI_SUM-OTGRP,
**           IT_DEFLOC-OTEIL    TO IT_ZSQM_NOTI_SUM-OTEIL,
*    MOVE : IT_DEFLOC-KURZTEXT TO IT_ZSQM_NOTI_SUM-KURZTEXT_OT.
*
*    MODIFY IT_ZSQM_NOTI_SUM  TRANSPORTING KURZTEXT_OT
*                 WHERE QMNUM NE ''
*                   AND OTKAT = IT_DEFLOC-OTKAT
*                   AND OTGRP = IT_DEFLOC-OTGRP
*                   AND OTEIL = IT_DEFLOC-OTEIL.
*
*  ENDLOOP.
*
***-- Activity type code
*
*  LOOP AT IT_ACTIVITY.
*    CLEAR IT_ZSQM_NOTI_SUM.
**    MOVE : IT_ACTIVITY-KATART_AT    TO IT_ZSQM_NOTI_SUM-KATART_AT,
**           IT_ACTIVITY-CODEGRP_AT   TO IT_ZSQM_NOTI_SUM-CODEGRP_AT,
**           IT_ACTIVITY-CODE_AT      TO IT_ZSQM_NOTI_SUM-CODE_AT,
*    MOVE : IT_ACTIVITY-KURZTEXT_AT  TO IT_ZSQM_NOTI_SUM-KURZTEXT_AT.
*
*    MODIFY IT_ZSQM_NOTI_SUM  TRANSPORTING KURZTEXT_AT
*                 WHERE QMNUM NE ''
*                   AND KATART_AT  = IT_ACTIVITY-KATART_AT
*                   AND CODEGRP_AT = IT_ACTIVITY-CODEGRP_AT
*                   AND CODE_AT    = IT_ACTIVITY-CODE_AT .
*
*  ENDLOOP.
*
**/-- System Status Text
**- Notification status text
*  LOOP AT IT_TJ02T.
*    CLEAR IT_ZSQM_NOTI_SUM.
*    MOVE IT_TJ02T-TXT04 TO IT_ZSQM_NOTI_SUM-TXT04.
*    MODIFY IT_ZSQM_NOTI_SUM  TRANSPORTING TXT04
*                 WHERE  STAT = IT_TJ02T-ISTAT.
*  ENDLOOP.
*
**- Notification Task status text
**  IF ZSQM_NOTI_SEL-QMART = '1'.
*  LOOP AT IT_TJ02T.
*    CLEAR IT_ZSQM_NOTI_SUM.
*    MOVE IT_TJ02T-TXT04 TO IT_ZSQM_NOTI_SUM-TASK_TXT04.
*    MODIFY IT_ZSQM_NOTI_SUM  TRANSPORTING TASK_TXT04
*                 WHERE TASK_STAT = IT_TJ02T-ISTAT.
*  ENDLOOP.
**  ENDIF.
*
ENDFORM.                    " GET_CODE_TEXT_AND_MODIFY
*&------------------------------------------------------------------*
*&      Form  GET_CODE_MASTER
*&------------------------------------------------------------------*
FORM GET_CODE_MASTER.


ENDFORM.                    " GET_CODE_MASTER
*&------------------------------------------------------------------*
*&      Form  MOVE_DATA_FOR_DISPLAY
*&------------------------------------------------------------------*
FORM MOVE_DATA_FOR_DISPLAY.




ENDFORM.                    " MOVE_DATA_FOR_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  SET_TABLE_TO_ALV
*&---------------------------------------------------------------------*
FORM SET_TABLE_TO_ALV.



DATA : LW_NOTI_INDEX LIKE SY-TABIX.

  REFRESH IT_ZSQM_REG_LIST_P.


  LOOP AT IT_ZSQM_IS_R_LIST.
    CLEAR IT_ZSQM_REG_LIST_P.
    MOVE-CORRESPONDING IT_ZSQM_IS_R_LIST TO IT_ZSQM_REG_LIST_P.
    APPEND IT_ZSQM_REG_LIST_P.
  ENDLOOP.


CASE ZSQM_IS_RES_SEL-ART.
          WHEN C_INSP_TYPE_ISIR.

 CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
          EXPORTING I_STRUCTURE_NAME = WA_STRUCTURE_NAME
                    IS_LAYOUT        = WA_IS_LAYOUT
                    I_SAVE           = WA_SAVE
                    IS_VARIANT       = WA_VARIANT
                    I_DEFAULT        = SPACE
                    IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
          CHANGING
*          IT_FIELDCATALOG  = IT_FIELDCAT_D[]
          IT_FIELDCATALOG  = IT_FIELDCAT[]

*                    IT_OUTTAB        = IT_ZSQM_IS_R_LIST_D[].
                    IT_OUTTAB        = IT_ZSQM_IS_R_LIST[].
  WHEN C_INSP_TYPE_REGULAR.
            CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
               EXPORTING I_STRUCTURE_NAME = WA_STRUCTURE_NAME
                         IS_LAYOUT        = WA_IS_LAYOUT
                         I_SAVE           = WA_SAVE
                         IS_VARIANT       = WA_VARIANT
                         I_DEFAULT        = SPACE
                         IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
               CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                         IT_OUTTAB        = IT_ZSQM_REG_LIST_P[].
        ENDCASE.



ENDFORM.                    " SET_TABLE_TO_ALV
*&---------------------------------------------------------------------*
*&      Module  set_initial_value  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_INITIAL_VALUE OUTPUT.

ENDMODULE.                 " set_initial_value  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_AND_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_AND_PROCESS.


ENDFORM.                    " GET_DATA_AND_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SET_INSPECTION_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_INSPECTION_TYPE.
 CASE C_MARK.
    WHEN ST_DIST-ISIR.
      ZSQM_IS_RES_SEL-ART = C_INSP_TYPE_ISIR.
    WHEN ST_DIST-REGU.
      ZSQM_IS_RES_SEL-ART = C_INSP_TYPE_REGULAR.
  ENDCASE.

  CLEAR ZSQM_IS_RES_SEL-KURZTEXT.

  SELECT SINGLE KURZTEXT INTO ZSQM_IS_RES_SEL-KURZTEXT
     FROM TQ30T
       WHERE SPRACHE = SY-LANGU
         AND ART     = ZSQM_IS_RES_SEL-ART.

ENDFORM.                    " SET_INSPECTION_TYPE
*&---------------------------------------------------------------------*
*&      Form  SET_PLAT_DATA_BY_CODE_G
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PLAT_DATA_BY_CODE_G.
 CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
    WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type
      ZSQM_IS_RES_SEL-WERKS = C_PLANT_ENGINE.
    WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
      ZSQM_IS_RES_SEL-WERKS = C_PLANT_VEHICLE.
  ENDCASE.

  CLEAR ZSQM_IS_RES_SEL-NAME1.

  SELECT SINGLE NAME1 INTO ZSQM_IS_RES_SEL-NAME1
    FROM T001W
     WHERE WERKS = ZSQM_IS_RES_SEL-WERKS.

ENDFORM.                    " SET_PLAT_DATA_BY_CODE_G
*&---------------------------------------------------------------------*
*&      Form  SET_RANGE_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_RANGE_VALUE.
*REFRESH : R_LIFNR, R_CODEGRP_VH,
*            R_CODE_VH, R_EXTWG,
*            R_PASTR, R_PAEND.
*
*  CLEAR   : R_LIFNR, R_CODEGRP_VH,
*            R_CODE_VH, R_EXTWG, R_PASTR, R_PAEND.
*
*  RANGE_MACRO :
*     R_LIFNR       ZSQM_IS_RES_SEL-LIFNR_L  ZSQM_IS_RES_SEL-LIFNR_H,
*     R_CODEGRP_VH  ZSQM_IS_RES_SEL-CODEGRP_VH  '',
*     R_CODE_VH     ZSQM_IS_RES_SEL-CODE_VH     '',
*     R_EXTWG    ZSQM_IS_RES_SEL-EXTWG_L  ZSQM_IS_RES_SEL-EXTWG_H,
*     R_PASTR    ZSQM_IS_RES_SEL-PASTR_L  ZSQM_IS_RES_SEL-PASTR_H,
*     R_PAEND    ZSQM_IS_RES_SEL-PAEND_L  ZSQM_IS_RES_SEL-PAEND_H.

ENDFORM.                    " SET_RANGE_VALUE
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_AND_LIST_BASIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_AND_LIST_BASIC.
*    - Get data from DB
  PERFORM GET_DATA_FROM_DB.

  CHECK NOT IT_ZSQM_IS_R_LIST[] IS INITIAL.

ENDFORM.                    " GET_DATA_AND_LIST_BASIC
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_FROM_DB.
 REFRESH IT_ZSQM_IS_R_LIST.

  CASE 'X'.

    WHEN ST_DIST-REGU.
* R_CODE_VH    = ZSQM_IS_RES_SEL-CODE_VH.
      SELECT F~EXTWG D~RESPP A~MATNR  A~LIFNR G~MAKTX
               A~CODE_IP  A~PRUEFLOS B~PRUEFLOS_MS
               A~DATUV_0010 A~DATUB_0010 A~DATUV_0020 A~DATUB_0020
               A~DATUV_0030 A~DATUB_0030 A~DATUV_0040 A~DATUB_0040
               A~DATUV_0050 A~DATUB_0050 A~DATUV_0060 A~DATUB_0060
               A~DATUV_0070 A~DATUB_0070 A~DATUV_0080 A~DATUB_0080
               A~DATUV_0090 A~DATUB_0090 A~DATUV_0100 A~DATUB_0100
               A~DATUV_0110 A~DATUB_0110 A~DATUV_0120 A~DATUB_0120
               A~DATUV_0130 A~DATUB_0130 A~DATUV_0140 A~DATUB_0140
               A~DATUV_0150 A~DATUB_0150 A~DATUV_0160 A~DATUB_0160
               A~DATUV_0170 A~DATUB_0170 A~DATUV_0180 A~DATUB_0180
               A~DATUV_0190 A~DATUB_0190 A~DATUV_0200 A~DATUB_0200
               A~DATUV_0210 A~DATUB_0210 A~DATUV_0220 A~DATUB_0220
               B~DATUV_1010 B~DATUB_1010 B~DATUV_1020 B~DATUB_1020
               B~DATUV_1030 B~DATUB_1030


                   INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_IS_R_LIST
        FROM  ( ( ( ( ZTQM_QNS_ITEM AS A INNER JOIN ZTQM_QNS_IT_MS AS B
                ON   A~IYEAR      = B~IYEAR
                 AND A~KATART_VH  = B~KATART_VH
                 AND A~CODEGRP_VH = B~CODEGRP_VH
                 AND A~CODE_VH    = B~CODE_VH
                 AND A~ART        = B~ART
                 AND A~MATNR      = B~MATNR
                 AND A~EONO       = B~EONO
             AND A~LIFNR      = B~LIFNR ) INNER JOIN ZTQM_QNS_IT_D AS C
                ON   A~IYEAR      = C~IYEAR
                 AND A~KATART_VH  = C~KATART_VH
                 AND A~CODEGRP_VH = C~CODEGRP_VH
                 AND A~CODE_VH    = C~CODE_VH
                 AND A~ART        = C~ART
                 AND A~MATNR      = C~MATNR
                 AND A~EONO       = C~EONO
                 AND A~LIFNR      = C~LIFNR
*                                        ) INNER JOIN QALS AS C
*          ON   A~PRUEFLOS   = C~PRUEFLOS
                                         ) INNER JOIN ZTQM_MAT_REG AS D
                ON  A~MATNR       = D~MATNR


*          AND A~EONO        = D~EONO
                AND A~LIFNR       = D~LIFNR
                                               ) INNER JOIN MARA AS F
                ON   A~MATNR      = F~MATNR    ) INNER JOIN MAKT AS G
                ON   F~MATNR      = G~MATNR


                 WHERE A~ART        = ZSQM_IS_RES_SEL-ART
                   AND A~KATART_VH  = ZSQM_IS_RES_SEL-KATART_VH
                   AND A~CODEGRP_VH = ZSQM_IS_RES_SEL-CODEGRP_VH
                   AND A~CODE_VH  IN R_CODE_VH
*                   AND A~CODE_VH  =  ZSQM_IS_RES_SEL-CODE_VH
*                   AND A~LIFNR     IN R_LIFNR
               AND A~LIFNR     =    ZSQM_IS_RES_SEL-LIFNR_L
* changed by 100565 11/10/04
*               AND A~IYEAR = P_IYEAR
                   AND F~EXTWG     IN R_EXTWG
* changed by 100565 11/11/04
*                   AND C~DATUV_FD  = R_PASTR
*                   AND C~DATUV_LD  = R_PAEND
*             AND C~PASTRTERM IN R_PASTR
*             AND C~PAENDTERM IN R_PAEND
                    AND C~DATUV_FD  >= ZSQM_IS_RES_SEL-Pastr_L
                   AND C~DATUV_LD  <=  ZSQM_IS_RES_SEL-PAEND_l
                   AND A~I_STAT     = C_RELEASE
                   AND G~SPRAS      = SY-LANGU.



    WHEN ST_DIST-ISIR.




      SELECT F~EXTWG D~RESPP A~MATNR A~EONO A~LIFNR G~MAKTX
             A~CODE_IP  A~PRUEFLOS B~PRUEFLOS_MS
             M~EODT    M~MODL
             A~DATUV_0010 A~DATUB_0010 A~DATUV_0020 A~DATUB_0020
             A~DATUV_0030 A~DATUB_0030 A~DATUV_0040 A~DATUB_0040
             A~DATUV_0050 A~DATUB_0050 A~DATUV_0060 A~DATUB_0060
             A~DATUV_0070 A~DATUB_0070 A~DATUV_0080 A~DATUB_0080
             A~DATUV_0090 A~DATUB_0090 A~DATUV_0100 A~DATUB_0100
             A~DATUV_0110 A~DATUB_0110 A~DATUV_0120 A~DATUB_0120
             A~DATUV_0130 A~DATUB_0130 A~DATUV_0140 A~DATUB_0140
             A~DATUV_0150 A~DATUB_0150 A~DATUV_0160 A~DATUB_0160
             A~DATUV_0170 A~DATUB_0170 A~DATUV_0180 A~DATUB_0180
             A~DATUV_0190 A~DATUB_0190 A~DATUV_0200 A~DATUB_0200
             A~DATUV_0210 A~DATUB_0210 A~DATUV_0220 A~DATUB_0220
             B~DATUV_1010 B~DATUB_1010 B~DATUV_1020 B~DATUB_1020
             B~DATUV_1030 B~DATUB_1030
                   INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_IS_R_LIST
        FROM  ( ( ( ( ZTQM_QNS_ITEM AS A INNER JOIN ZTQM_QNS_IT_MS AS B
              ON   A~IYEAR      = B~IYEAR
               AND A~KATART_VH  = B~KATART_VH
               AND A~CODEGRP_VH = B~CODEGRP_VH
               AND A~CODE_VH    = B~CODE_VH
               AND A~ART        = B~ART
               AND A~MATNR      = B~MATNR
               AND A~EONO       = B~EONO
             AND A~LIFNR      = B~LIFNR ) INNER JOIN ZTQM_QNS_IT_D AS C
              ON   A~IYEAR      = C~IYEAR
               AND A~KATART_VH  = C~KATART_VH
               AND A~CODEGRP_VH = C~CODEGRP_VH
               AND A~CODE_VH    = C~CODE_VH
               AND A~ART        = C~ART
               AND A~MATNR      = C~MATNR
               AND A~EONO       = C~EONO
               AND A~LIFNR      = C~LIFNR
*                                        ) INNER JOIN QALS AS C
*          ON   A~PRUEFLOS   = C~PRUEFLOS
                                        ) INNER JOIN ZTQM_MAT_ISIR AS D
              ON  A~MATNR       = D~MATNR
              AND A~EONO        = D~EONO
              AND A~LIFNR       = D~LIFNR
*added 100565 09/22/04
                                        ) INNER JOIN ZTQM_MAT_EO AS M
              ON  D~PTNO       = M~PTNO
              AND D~EONO        = M~EONO
              AND D~LIFNR       = M~VEND
                                             ) INNER JOIN MARA AS F
              ON   A~MATNR      = F~MATNR     INNER JOIN MAKT AS G
              ON   F~MATNR      = G~MATNR


               WHERE A~ART        = ZSQM_IS_RES_SEL-ART
                 AND A~KATART_VH  = ZSQM_IS_RES_SEL-KATART_VH
                 AND A~CODEGRP_VH = ZSQM_IS_RES_SEL-CODEGRP_VH
                 AND A~CODE_VH   IN R_CODE_VH
*                 AND A~LIFNR     IN R_LIFNR
            AND A~LIFNR     =    ZSQM_IS_RES_SEL-LIFNR_L

                 AND F~EXTWG     IN R_EXTWG
*                 AND C~DATUV_FD  IN R_PASTR
*                 AND C~DATUV_LD  IN R_PAEND
***             AND C~PASTRTERM IN R_PASTR
***             AND C~PAENDTERM IN R_PAEND
                AND C~DATUV_FD  >= ZSQM_IS_RES_SEL-Pastr_L
                 AND C~DATUV_LD  <=  ZSQM_IS_RES_SEL-PAEND_l


                 AND A~I_STAT     = C_RELEASE
                 AND G~SPRAS      = SY-LANGU.
  ENDCASE.

  CHECK SY-SUBRC NE 0.

  MESSAGE W000(ZMQM) WITH 'No entries.'(W01).


ENDFORM.                    " GET_DATA_FROM_DB
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_ATTR_FOR_ISIR_P
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_ATTR_FOR_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_IT_FIELDCAT_D  text
*----------------------------------------------------------------------*
FORM SET_FIELD_ATTR_FOR_DETAIL TABLES  PT_FIELDCAT   TYPE LVC_T_FCAT
                                       PT_FIELDCAT_D TYPE LVC_T_FCAT.
 LOOP AT PT_FIELDCAT_D.
    IF PT_FIELDCAT_D-FIELDNAME = 'EXTWG'.
      PT_FIELDCAT_D-NO_OUT = C_MARK.
    ELSEIF PT_FIELDCAT_D-FIELDNAME = 'MATNR' OR
           PT_FIELDCAT_D-FIELDNAME = 'MAKTX' OR
           PT_FIELDCAT_D-FIELDNAME = 'CODE_VH' OR
           PT_FIELDCAT_D-FIELDNAME = 'LIFNR'   OR
           PT_FIELDCAT_D-FIELDNAME = 'EONO'.
      PT_FIELDCAT_D-KEY_SEL = C_MARK.
      PT_FIELDCAT_D-KEY     = C_MARK.
      PT_FIELDCAT_D-EMPHASIZE = 'C410'.

      IF PT_FIELDCAT_D-FIELDNAME = 'EONO'.
        CASE ZSQM_IS_RES_SEL-ART.
          WHEN C_INSP_TYPE_ISIR.
*            PT_FIELDCAT_D-EMPHASIZE = C_MARK.
            PT_FIELDCAT_D-NO_OUT = C_MARK.
          WHEN C_INSP_TYPE_REGULAR.
            CLEAR PT_FIELDCAT_D-KEY_SEL.
            CLEAR PT_FIELDCAT_D-KEY.
            PT_FIELDCAT_D-NO_OUT = C_MARK.
        ENDCASE.
      ENDIF.

    ELSEIF PT_FIELDCAT_D-FIELDNAME = 'RESPP'.
      CASE ZSQM_IS_RES_SEL-ART.
        WHEN C_INSP_TYPE_ISIR.
*            PT_FIELDCAT_D-EMPHASIZE = C_MARK.
*            PT_FIELDCAT_D-NO_OUT = C_MARK.
        WHEN C_INSP_TYPE_REGULAR.
* changed by 100565 request by Mr Moon 7/19/04 - Start
*Commented out the following code
*          PT_FIELDCAT_D-NO_OUT = C_MARK.
* changed by 100565 request by Mr Moon 7/19/04 - End
      ENDCASE.


    ELSEIF PT_FIELDCAT_D-FIELDNAME = 'I_STAT'.
      PT_FIELDCAT_D-OUTPUTLEN = 3.
      PT_FIELDCAT_D-EMPHASIZE = 'C210'.

    ELSEIF PT_FIELDCAT_D-FIELDNAME = 'PRUEFLOS'.
      MOVE  'Inspection Lot'(TT5)  TO : PT_FIELDCAT_D-COLTEXT,
                                        PT_FIELDCAT_D-SCRTEXT_L,
                                        PT_FIELDCAT_D-SCRTEXT_M,
                                        PT_FIELDCAT_D-SCRTEXT_S.
    ELSEIF PT_FIELDCAT_D-FIELDNAME = 'PRUEFLOS_MS'.
      MOVE  'Inspection Lot(MS)'(TT6)  TO : PT_FIELDCAT_D-COLTEXT,
                                            PT_FIELDCAT_D-SCRTEXT_L,
                                            PT_FIELDCAT_D-SCRTEXT_M,
                                            PT_FIELDCAT_D-SCRTEXT_S.
      PT_FIELDCAT_D-EMPHASIZE = C_MARK.

    ENDIF.

*-- ISIR/REGULAR
    IF PT_FIELDCAT_D-FIELDNAME+0(4) = 'DATU'.
      READ TABLE PT_FIELDCAT  WITH KEY
                                FIELDNAME = PT_FIELDCAT_D-FIELDNAME.
      IF SY-SUBRC NE 0.
        PT_FIELDCAT_D-NO_OUT = C_MARK.
      ELSE.
        MOVE : PT_FIELDCAT-COLTEXT    TO PT_FIELDCAT_D-COLTEXT,
               PT_FIELDCAT-SCRTEXT_L  TO PT_FIELDCAT_D-SCRTEXT_L,
               PT_FIELDCAT-SCRTEXT_M  TO PT_FIELDCAT_D-SCRTEXT_M,
               PT_FIELDCAT-SCRTEXT_S  TO PT_FIELDCAT_D-SCRTEXT_S.
      ENDIF.

      IF PT_FIELDCAT_D-FIELDNAME+0(5) = 'DATUV'.
        PT_FIELDCAT_D-EMPHASIZE = 'C300'.
      ELSEIF PT_FIELDCAT_D-FIELDNAME+0(5) = 'DATUB'.
        PT_FIELDCAT_D-EMPHASIZE = 'C311'.
      ENDIF.

      IF PT_FIELDCAT_D-FIELDNAME+6(1) = '1'.
        IF PT_FIELDCAT_D-FIELDNAME+0(5) = 'DATUV'.
          PT_FIELDCAT_D-EMPHASIZE = 'C400'.
        ELSEIF PT_FIELDCAT_D-FIELDNAME+0(5) = 'DATUB'.
          PT_FIELDCAT_D-EMPHASIZE = 'C411'.
        ENDIF.
      ENDIF.

    ENDIF.

    MODIFY PT_FIELDCAT_D.

  ENDLOOP.


ENDFORM.                    " SET_FIELD_ATTR_FOR_DETAIL
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_ATTR_FOR_ISIR_P
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM SET_FIELD_ATTR_FOR_ISIR_P TABLES    PT_FIELDCAT TYPE LVC_T_FCAT.
DATA : BEGIN OF LT_QPMK OCCURS 30,
          MKMNR      TYPE  QMERKNR,
          KURZTEXT   TYPE  QKURZTEXT,
         END OF LT_QPMK.
  	
    DATA : LW_MKMNR TYPE QMERKNR.

  DATA : LW_COL_TEXT TYPE LVC_TXTCOL. "/Temporary Column Text

*  DATA : LW_FLD_NAME(40) TYPEC.
*  FIELD-SYMBOLS : <LW_FS>.

  DATA : LW_FLD_N_IND(4) TYPE N,
         LW_FLD_N_DIV(3) TYPE N,
         LW_MIC_SER(2)   TYPE N.



*--  Get MIC Master text list
  SELECT A~MKMNR B~KURZTEXT  INTO CORRESPONDING FIELDS OF TABLE LT_QPMK
                        FROM QPMK AS  A INNER JOIN QPMT AS B
                           ON   A~ZAEHLER  = B~ZAEHLER
                            AND A~MKMNR    = B~MKMNR
                            AND A~VERSION  = B~VERSION
                          WHERE A~ZAEHLER = ZSQM_IS_RES_SEL-WERKS
*                            AND A~VERSION = '000001'
                            AND A~LOEKZ   = '2'
                            AND B~SPRACHE = SY-LANGU
                            AND B~GELOESCHT = ' '
                            AND A~VERSION =
                                    ( SELECT MAX( VERSION )
                                        FROM QPMK
                                         WHERE ZAEHLER = A~ZAEHLER
                                           AND MKMNR   = A~MKMNR ).

  CHECK SY-SUBRC = 0.

* Set field attribute
  LOOP AT PT_FIELDCAT.
    IF PT_FIELDCAT-FIELDNAME = 'EXTWG'.
      PT_FIELDCAT-KEY_SEL = C_MARK.
      PT_FIELDCAT-KEY     = C_MARK.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'MATNR_C'.
      MOVE  'Material'(TT4)  TO : PT_FIELDCAT-COLTEXT,
                                  PT_FIELDCAT-SCRTEXT_L,
                                  PT_FIELDCAT-SCRTEXT_M,
                                  PT_FIELDCAT-SCRTEXT_S.
      PT_FIELDCAT-EMPHASIZE = 'C410'.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'PLOS_C'.
      MOVE  'Inspection Lot'(TT5)  TO : PT_FIELDCAT-COLTEXT,
                                        PT_FIELDCAT-SCRTEXT_L,
                                        PT_FIELDCAT-SCRTEXT_M,
                                        PT_FIELDCAT-SCRTEXT_S.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'PLOS_MS_C'.
      MOVE  'Inspection Lot(MS)'(TT6)  TO : PT_FIELDCAT-COLTEXT,
                                            PT_FIELDCAT-SCRTEXT_L,
                                            PT_FIELDCAT-SCRTEXT_M,
                                            PT_FIELDCAT-SCRTEXT_S.
      PT_FIELDCAT-EMPHASIZE = C_MARK.

    ENDIF.

    IF PT_FIELDCAT-DATATYPE = 'INT4'.
      PT_FIELDCAT-DO_SUM = C_MARK.
    ENDIF.

*-- ISIR/REGULAR
    IF PT_FIELDCAT-FIELDNAME+0(4) = 'DATU' AND
       PT_FIELDCAT-FIELDNAME+6(1) = '0'.

      IF PT_FIELDCAT-FIELDNAME+0(5) = 'DATUV'.
        PT_FIELDCAT-EMPHASIZE = 'C300'.
      ELSEIF PT_FIELDCAT-FIELDNAME+0(5) = 'DATUB'.
        PT_FIELDCAT-EMPHASIZE = 'C311'.
      ENDIF.

      LW_FLD_N_IND =  PT_FIELDCAT-FIELDNAME+6(4). "/'DATUV/B_xxxx'
      LW_MIC_SER   =  LW_FLD_N_IND+1(2). "/'0xx0' "/MIC Serial
*      - Start/End date of MIC
      CASE ZSQM_IS_RES_SEL-ART.
        WHEN C_INSP_TYPE_ISIR.
          CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
            WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type

              CONCATENATE ZSQM_IS_RES_SEL-WERKS+0(1)  "/Plant:'x001'
                     C_CODEGRUPPE_ISIR+2(2)      "/Insp. type:'QPxx'
                      '_'
                          LW_MIC_SER
                            INTO LW_MKMNR.

            WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
              CONCATENATE ZSQM_IS_RES_SEL-WERKS+0(1)  "/Plant:'x001'
                     C_CODEGRUPPE_ISIR+2(2)      "/Insp. type:'QPxx'
                       '_'
                          LW_MIC_SER
                            INTO LW_MKMNR.
          ENDCASE.
        WHEN C_INSP_TYPE_REGULAR.
          CONCATENATE ZSQM_IS_RES_SEL-WERKS+0(1)  "/Plant:'x001'
                      C_CODEGRUPPE_REGU+2(2)    "/Insp. type:'QPxx'
                       '_'
                      LW_MIC_SER
                        INTO LW_MKMNR.
      ENDCASE.

*      select Column Text for MIC - 'XXX_nn'
      READ TABLE LT_QPMK WITH KEY MKMNR = LW_MKMNR.

      IF SY-SUBRC = 0.

*-- Make MIC Column Text
        CONCATENATE   LW_FLD_N_IND+1(3)
                      '-'
                      LT_QPMK-KURZTEXT
                                INTO LW_COL_TEXT.

        MOVE  LW_COL_TEXT TO :      PT_FIELDCAT-COLTEXT,
                                    PT_FIELDCAT-SCRTEXT_L,
                                    PT_FIELDCAT-SCRTEXT_M,
                                    PT_FIELDCAT-SCRTEXT_S.

      ELSE. "- 'XXXnn' "/ for Dev Test system - sllee
        REPLACE '_' WITH ' ' INTO  LW_MKMNR.
        CONDENSE LW_MKMNR NO-GAPS.
        READ TABLE LT_QPMK WITH KEY MKMNR = LW_MKMNR.

*-- Make MIC Column Text
        CONCATENATE   LW_FLD_N_IND+1(3)
                      '-'
                      LT_QPMK-KURZTEXT
                                INTO LW_COL_TEXT.

        IF SY-SUBRC = 0.
          MOVE  LW_COL_TEXT TO :       PT_FIELDCAT-COLTEXT,
                                      PT_FIELDCAT-SCRTEXT_L,
                                      PT_FIELDCAT-SCRTEXT_M,
                                      PT_FIELDCAT-SCRTEXT_S.
        ENDIF.
      ENDIF.

    ENDIF.

**-- MS
    IF PT_FIELDCAT-FIELDNAME+0(4) = 'DATU' AND
       PT_FIELDCAT-FIELDNAME+6(1) = '1'.

      IF PT_FIELDCAT-FIELDNAME+0(5) = 'DATUV'.
        PT_FIELDCAT-EMPHASIZE = 'C400'.
      ELSEIF PT_FIELDCAT-FIELDNAME+0(5) = 'DATUB'.
        PT_FIELDCAT-EMPHASIZE = 'C411'.
      ENDIF.

      LW_FLD_N_IND =  PT_FIELDCAT-FIELDNAME+6(4). "/'DATUV/B_xxxx'
      LW_MIC_SER   =  LW_FLD_N_IND+1(2). "/'0xx0' "/MIC Serial
*      - Start/End date of MIC
      CASE ZSQM_IS_RES_SEL-ART.
        WHEN C_INSP_TYPE_ISIR.
          CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
            WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type

              CONCATENATE ZSQM_IS_RES_SEL-WERKS+0(1)  "/Plant:'x001'
                     C_CODEGRUPPE_MS+2(2)      "/Insp. type:'QPxx'
                       '_'
                          LW_MIC_SER
                            INTO LW_MKMNR.

            WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
              CONCATENATE ZSQM_IS_RES_SEL-WERKS+0(1)  "/Plant:'x001'
                     C_CODEGRUPPE_MS+2(2)      "/Insp. type:'QPxx'
                       '_'
                          LW_MIC_SER
                            INTO LW_MKMNR.
          ENDCASE.
        WHEN C_INSP_TYPE_REGULAR.
          CONCATENATE ZSQM_IS_RES_SEL-WERKS+0(1)  "/Plant:'x001'
                      C_CODEGRUPPE_MS+2(2)    "/Insp. type:'QPxx'
                       '_'
                      LW_MIC_SER
                        INTO LW_MKMNR.
      ENDCASE.

*      select Column Text for MIC - 'XXX_nn'
      READ TABLE LT_QPMK WITH KEY MKMNR = LW_MKMNR.

      IF SY-SUBRC = 0.
*-- Make MIC Column Text
        CONCATENATE   LW_FLD_N_IND+1(3)
                      '-'
                      LT_QPMK-KURZTEXT
                                INTO LW_COL_TEXT.

        MOVE  LW_COL_TEXT  TO :     PT_FIELDCAT-COLTEXT,
                                    PT_FIELDCAT-SCRTEXT_L,
                                    PT_FIELDCAT-SCRTEXT_M,
                                    PT_FIELDCAT-SCRTEXT_S.
      ELSE. "- 'XXXnn' "/ for Dev Test system - sllee
        REPLACE '_' WITH ' ' INTO  LW_MKMNR.
        CONDENSE LW_MKMNR NO-GAPS.
        READ TABLE LT_QPMK WITH KEY MKMNR = LW_MKMNR.

*-- Make MIC Column Text
        CONCATENATE   LW_FLD_N_IND+1(3)
                      '-'
                      LT_QPMK-KURZTEXT
                                INTO LW_COL_TEXT.

        IF SY-SUBRC = 0.
          MOVE  LW_COL_TEXT TO :      PT_FIELDCAT-COLTEXT,
                                      PT_FIELDCAT-SCRTEXT_L,
                                      PT_FIELDCAT-SCRTEXT_M,
                                      PT_FIELDCAT-SCRTEXT_S.
        ENDIF.
      ENDIF.

    ENDIF.

    MODIFY PT_FIELDCAT.

  ENDLOOP.
ENDFORM.                    " SET_FIELD_ATTR_FOR_ISIR_P
*&---------------------------------------------------------------------*
*&      Form  GET_DEFAULT_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DEFAULT_VENDOR.
CLEAR WA_VENDOR.
SELECT SINGLE LIFNR FROM ZTQM_VENDOR_ID  INTO WA_VENDOR WHERE UNAME =
SY-UNAME.
IF SY-SUBRC = 0.
ZSQM_IS_RES_SEL-LIFNR_L = WA_VENDOR.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'Q23'.
          SCREEN-input = 0.
          Screen-output = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

ENDIF.


ENDFORM.                    " GET_DEFAULT_VENDOR
*&---------------------------------------------------------------------*
*&      Module  GET_DEFAULT_VENDOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_DEFAULT_VENDOR INPUT.
CLEAR WA_VENDOR.
SELECT SINGLE LIFNR FROM ZTQM_VENDOR_ID  INTO WA_VENDOR WHERE UNAME =
SY-UNAME.
IF SY-SUBRC = 0.
ZSQM_IS_RES_SEL-LIFNR_L = WA_VENDOR.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'Q23'.
          SCREEN-input = 0.
          Screen-output = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
ENDIF.
ENDMODULE.                 " GET_DEFAULT_VENDOR  INPUT
