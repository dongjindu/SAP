************************************************************************
* Program Name      : ZPWT
* Author            : Sudheer Bheemakumar
* Creation Date     : 2004/11/15.
* Specifications By : Naveen Javaregowda
* Development Request
* Addl Documentation:
* Description       : Planned Working Time Report
* Modification Logs
* Date       Developer    RequestNo    Description
* XX/XX/XXXX   XYZ        XXXXXXXXX    XXXXXXXXXXX
************************************************************************

REPORT  ZPWT.
DATA: OK_CODE LIKE SY-UCOMM,
      G_REPID LIKE SY-REPID,
      CUSTOM_CONTAINER1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONT_ON_MAIN TYPE SCRFNAME VALUE 'GRD_PLWKTM',
      GRID1 TYPE REF TO CL_GUI_ALV_GRID,
      GS_LAYOUT TYPE LVC_S_LAYO,
      IT_PLNWKTM TYPE TABLE OF RPM_TS_PLAN_WORK_TIME,
      ZIT_PLNWKTM TYPE TABLE OF ZRPM_TS_PLAN_WORK_TIME,
      WA_IT_PLNWKTM LIKE RPM_TS_PLAN_WORK_TIME,
      WA_ZIT_PLNWKTM LIKE ZRPM_TS_PLAN_WORK_TIME.

      DATA : IT_FIELDCAT TYPE LVC_T_FCAT WITH HEADER LINE.
      CONSTANTS : C_MARK   VALUE 'X'.

      DATA: SAPUSRID(8) TYPE C,
            ESSUSRNAME(25).
      DATA: ESSUSRID(8) TYPE N.

      SAPUSRID = SY-UNAME.
      ESSUSRID = SAPUSRID.

*     get the name of the user from table USER_ADDR using
*     the SAP login ID
      SELECT SINGLE NAME_TEXTC FROM USER_ADDR INTO ESSUSRNAME
      WHERE BNAME = SAPUSRID.




      SET SCREEN 100.
*&---------------------------------------------------------------------*
*&      Module  PWT_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PWT_100 OUTPUT.

* §  ALV Control to show Planned Working Time.
* SET PF-STATUS 'MAIN100'.
   G_REPID = SY-REPID.

  IF CUSTOM_CONTAINER1 IS INITIAL.
* Fetch data from Form through BAPI
    PERFORM GET_PWT_INFO.
    LOOP AT IT_PLNWKTM INTO WA_IT_PLNWKTM.
      WA_ZIT_PLNWKTM-PERNR = ESSUSRID.
*      WA_ZIT_PLNWKTM-PERSON_NAME = ESSUSRNAME.
      MOVE-CORRESPONDING WA_IT_PLNWKTM TO WA_ZIT_PLNWKTM.
      APPEND WA_ZIT_PLNWKTM TO ZIT_PLNWKTM.
    ENDLOOP.

* custom container control for ALV Control
    CREATE OBJECT CUSTOM_CONTAINER1
        EXPORTING
            CONTAINER_NAME = CONT_ON_MAIN
        EXCEPTIONS
            CNTL_ERROR = 1
            CNTL_SYSTEM_ERROR = 2
            CREATE_ERROR = 3
            LIFETIME_ERROR = 4
            LIFETIME_DYNPRO_DYNPRO_LINK = 5.
    IF sy-subrc ne 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
                TITEL = G_REPID
                TXT2  = SY-SUBRC
                TXT1  = 'The control could not be created'(510).
    ENDIF.

* create an instance of alv control
    CREATE OBJECT GRID1
             EXPORTING I_PARENT = CUSTOM_CONTAINER1.

*-- Prepare Setting Attributes and etc of ALV Object
    PERFORM SET_ATTRIBUTES_ALV_GRID.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure
    PERFORM MASK_COLUMNS_OF_ALV_GRID TABLES IT_FIELDCAT.

*-- Display data on ALV GRID Control using method
    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME ='ZRPM_TS_PLAN_WORK_TIME'
                   IS_LAYOUT        = GS_LAYOUT
         CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                   IT_OUTTAB        = ZIT_PLNWKTM.

  ENDIF.                               "IF grid1 IS INITIAL

  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS EXPORTING CONTROL = GRID1.

ENDMODULE.                 " PWT_100  OUTPUT

*&-----------------------------------------------------------------*
*&      Form  GET_PWT_INFO.
*&-----------------------------------------------------------------*
FORM GET_PWT_INFO.

*  CALL BAPI TO FETCH THE DATA
  CALL FUNCTION 'RPM_GET_PLANNED_WORKING_TIME'
    EXPORTING
     IV_PERNR                       = ESSUSRID    "SY-UNAME
*     IV_BEGDA                       =
*     IV_ENDDA                       =
*   IMPORTING
*      EV_SUBRC                       =
   TABLES
     ET_RPM_TS_PLAN_WORK_TIME       = IT_PLNWKTM.

ENDFORM.                    " GET_PWT_INFO

*&-----------------------------------------------------------------*
*&      Form  MASK_COLUMNS_OF_ALV_GRID
*&-----------------------------------------------------------------*
FORM MASK_COLUMNS_OF_ALV_GRID TABLES PT_FIELDCAT TYPE LVC_T_FCAT.

  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.

* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = 'ZRPM_TS_PLAN_WORK_TIME'
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].

* Set field attribute

  LOOP AT PT_FIELDCAT.
    CASE PT_FIELDCAT-FIELDNAME.
      WHEN 'PERNR'.
        PT_FIELDCAT-COLTEXT = 'Pers. No.'(T01).
      WHEN 'PERSON_NAME'.
        PT_FIELDCAT-COLTEXT = 'Pers. Name'(T02).
      WHEN 'BEGDA'.
        PT_FIELDCAT-COLTEXT = 'Start Date'(T03).
      WHEN 'ENDDA'.
        PT_FIELDCAT-COLTEXT = 'End Date'(T04).
      WHEN 'SCHKZ'.
        PT_FIELDCAT-COLTEXT = 'WS Rule'(T05).
      WHEN 'EMPCT'.
        PT_FIELDCAT-COLTEXT = 'Empl. %'(T06).
      WHEN 'MOSTD'.
        PT_FIELDCAT-COLTEXT = 'Mthly Hrs'(T07).
      WHEN 'WOSTD'.
        PT_FIELDCAT-COLTEXT = 'Weekly Hrs'(T08).
      WHEN 'ARBST'.
        PT_FIELDCAT-COLTEXT = 'Daily Hrs'(T09).
      WHEN 'WKWDY'.
        PT_FIELDCAT-COLTEXT = 'Workdays'(T10).
      WHEN 'JRSTD'.
        PT_FIELDCAT-COLTEXT = 'Annual Hrs'(T11).
    ENDCASE.
    MODIFY PT_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " MASK_COLUMNS_OF_ALV_GRID

INCLUDE ZGLBLDECLRNS.

*&-----------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID.
*&-----------------------------------------------------------------*

FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : GS_LAYOUT.
*//-- Set Layout Structure

  GS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
  GS_LAYOUT-CWIDTH_OPT = C_MARK.  "/Optimize column width
*  WA_IS_LAYOUT-DETAILTITL = ''.        "/Title bar of detail screen
   GS_LAYOUT-GRID_TITLE = 'PLANNED WORKING TIME'.  "/ Title bar text
*  WA_IS_LAYOUT-KEYHOT      = C_MARK.    "/ Key columns as hotspot
*  GS_LAYOUT-NO_HEADERS  = C_MARK.     "/Hide column headings
*  WA_IS_LAYOUT-NO_HGRIDLN  = C_MARK.     "/Hide horizontal grid lines
*  WA_IS_LAYOUT-NO_VGRIDLN  = C_MARK.     "/Hide vertical grid lines
*   GS_LAYOUT-NO_MERGING  = C_MARK.     "/Disable cell merging
*  WA_IS_LAYOUT-NO_ROWMARK  = C_MARK.     "/Disable row selections
   GS_LAYOUT-NO_TOOLBAR  = C_MARK.     "/Hide toolbar
   GS_LAYOUT-NUMC_TOTAL  = C_MARK. "/Allow totals for NUMC
*  WA_IS_LAYOUT-S_DRAGDROP  = LW_S_DRAGDROP. "/Drag & Drop control

  GS_LAYOUT-SEL_MODE  = 'A'. "/mode for select col and row
*  WA_IS_LAYOUT-SGL_CLK_HD = C_MARK. "/sorts the list whe column clicked

*//-- Set Variant Structure
*  GS_LAYOUT-REPORT = SY-REPID.
*  GS_LAYOUT-USERNAME = SY-UNAME.

ENDFORM.
