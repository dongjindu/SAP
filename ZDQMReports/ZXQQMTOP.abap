*----------------------------------------------------------------------*
***INCLUDE ZXQQMTOP .
*----------------------------------------------------------------------*

*<<<<<<<< Start of EQM01 - ZQMEX_02 QM-Notification Enhancement >>>>>>
TABLES : FELD.  "//Screen Fieds Attributes Structure

*-- Screen Fields Attritutes Variables
DATA : WA_FLDTXT    LIKE FELD-NAME,  "Screen Field Name
       WA_CUR_LINE  LIKE FELD-LINE.  "Screen Field Line Count

*----- Long Text Objects..
TABLES: THEAD.            "SAPscript: Text Object Interface Structure

*-- User Data Structures.
TABLES : ZSQM_CI_QMEL.   "/Screen Layout Str. for User Data for CI_QMEL

DATA : WA_THEAD_A LIKE THEAD, "/FOR Description of improvement
       WA_THEAD_B LIKE THEAD. "/FOR Content of Confirmation
CONSTANTS: C_LINE_LENGTH TYPE I VALUE 132." changed 80 to 132
*CONSTANTS: C_LINE_LENGTH TYPE I VALUE 45." changed 80 to 132
*-- types for Editor Table
*     structure to define a table with a line of characters,
TYPES: BEGIN OF ST_LINE,
          LINE(C_LINE_LENGTH) TYPE C,
       END OF ST_LINE.
TYPES : TT_LINE TYPE ST_LINE OCCURS 0.

DATA: BEGIN OF IT_EDITOR,   "Text STRUCTURE
          LINE(C_LINE_LENGTH)           TYPE C,
      END   OF IT_EDITOR.

*-- Internal Tables for Editor
DATA : IT_EDITOR_A LIKE IT_EDITOR OCCURS 10.
DATA : IT_EDITOR_B LIKE IT_EDITOR OCCURS 10.

*-- Internal Tables for Saving of Sap Scripts Long Text
DATA : IT_TLINE_A LIKE TLINE OCCURS 10 WITH HEADER LINE,
       IT_TLINE_B LIKE TLINE OCCURS 10 WITH HEADER LINE.
DATA: WI_QMNUM LIKE QMEL-QMNUM.

*-- Flag of User Data Text for First Read (Display/Change)
DATA  WA_TEXT_READ_FLG TYPE C.

*----- Class
CLASS CL_GUI_CFW DEFINITION LOAD.  "Control Framework Basic Class

*----- Editor
DATA : TE_0101_A  TYPE REF TO CL_GUI_TEXTEDIT, "Editor for Descrip.Impr
       TE_0101_B  TYPE REF TO CL_GUI_TEXTEDIT, "Editor for Content.conf
*HASEEB
       TE_0102_A  TYPE REF TO CL_GUI_TEXTEDIT, "Root process,RC_PROCESS
       TE_0102_B  TYPE REF TO CL_GUI_TEXTEDIT, "Root system, RC_SYSTEM
       TE_0102_C  TYPE REF TO CL_GUI_TEXTEDIT, "CA_PROCESS
       TE_0102_D  TYPE REF TO CL_GUI_TEXTEDIT, "CA_SYSTEM
       TE_0102_E  TYPE REF TO CL_GUI_TEXTEDIT, "C0RR_ACT_APP
       TE_0102_F  TYPE REF TO CL_GUI_TEXTEDIT. "METH_VAL_EFF
*HASEEB

*------ Container
DATA : TEC_0101_A TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       TEC_0101_B TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

*HASEEB
       TEC_0102_A TYPE REF TO CL_GUI_CUSTOM_CONTAINER, "RC_PROCESS
       TEC_0102_B TYPE REF TO CL_GUI_CUSTOM_CONTAINER, "RC_SYSTEM
       TEC_0102_C TYPE REF TO CL_GUI_CUSTOM_CONTAINER, "CA_PROCESS
       TEC_0102_D TYPE REF TO CL_GUI_CUSTOM_CONTAINER, "CA_SYSTEM
       TEC_0102_E TYPE REF TO CL_GUI_CUSTOM_CONTAINER, "CORR_ACT_APP
       TEC_0102_F TYPE REF TO CL_GUI_CUSTOM_CONTAINER. "METH_VAL_EFF

*HASEEB

*-- EDITOR MODE
CONSTANTS: C_READ_ONLY TYPE I VALUE 1,   " Read only mode
           C_EDITABLE  TYPE I VALUE 0.   " Edit mode

*-- Sapscript Text Object and ID.
CONSTANTS :C_TDOBJECT LIKE THEAD-TDOBJECT VALUE 'ZQMEL', "Text Object
           C_TDID_DS  LIKE THEAD-TDID     VALUE 'ZQM1',"Text ID for Des.
           C_TDID_CT  LIKE THEAD-TDID     VALUE 'ZQM2'."Text ID for Con

*-- ETC
CONSTANTS :  C_MTART_FERT TYPE MTART VALUE 'FERT'. "/Vehicle or Engine

*-- Line Length of Editor
DATA : LWA_LINE_LENGTH TYPE I VALUE 80.
DATA: W_TEST(3),
W_FLAG(1).
*<<<<<<<< End of EQM01 - ZQMEX_02 >>>>>>
