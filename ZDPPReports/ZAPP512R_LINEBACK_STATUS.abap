************************************************************************
* Program Name      : ZAPP512R_LINEBACK_STATUS
* Author            : Bobby
* Creation Date     : 2003.09.23.
* Specifications By : Bobby
* Development Request No : UD1K902288
* Addl Documentation:
* Description       : APP512: Line-Back Status List
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  ZAPP512R_LINEBACK_STATUS NO STANDARD PAGE HEADING
                                 MESSAGE-ID ZDPP.

TABLES: ZTPP_CHANGE.

DATA: IT_CHANGE LIKE ZTPP_CHANGE OCCURS 0,
      IT_LIST LIKE TABLE OF IT_CHANGE.
TYPE-POOLS: SLIS.

DATA : G_CUSTOM_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       MYAREA                TYPE SCRFNAME VALUE 'MYAREA'      ,
       ALV_GRID              TYPE REF TO CL_GUI_ALV_GRID        .
DATA: GS_LAYOUT TYPE LVC_S_LAYO,
      GS_VARIANT TYPE DISVARIANT,
      GT_FIELDCAT_SLIS      TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      IT_FIELDCATALOG       TYPE LVC_T_FCAT WITH HEADER LINE    .
DATA: OK_CODE LIKE SY-UCOMM .
DATA GT_FIELDCAT TYPE LVC_T_FCAT.

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE T1.
SELECT-OPTIONS: S_DATE FOR SY-DATUM.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE T2.
SELECT-OPTIONS: S_EQUI FOR ZTPP_CHANGE-BODYNO NO INTERVALS.
SELECTION-SCREEN END OF BLOCK B2.
************************************************************************
*              INITIALIZATION                                          *
************************************************************************
INITIALIZATION.

************************************************************************
*              AT SELECTION SCREEN                                     *
************************************************************************
AT SELECTION-SCREEN.

************************************************************************
*              START-OF-SELECTION PROCESSING                           *
************************************************************************
START-OF-SELECTION.
PERFORM DATA_SELECT.
*PERFORM DATA_JOIN.
END-OF-SELECTION.
CALL SCREEN 9000.


*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM DATA_SELECT.
IF S_EQUI[] IS INITIAL.
*LINE-BACK
   SELECT * FROM ZTPP_CHANGE
            INTO TABLE IT_CHANGE
            WHERE CDATE IN S_DATE
              AND CFLAG EQ 'B'.
ELSE.
*SPEC CHANGE
   SELECT * FROM ZTPP_CHANGE
            INTO TABLE IT_CHANGE
            WHERE CDATE IN S_DATE
              AND CFLAG EQ 'S'
              AND BODYNO IN S_EQUI.
ENDIF.
ENDFORM.                    " DATA_SELECT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '9000'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
MODULE CREATE_OBJECT OUTPUT.
  IF G_CUSTOM_CONTAINER IS INITIAL.
     CREATE OBJECT G_CUSTOM_CONTAINER
            EXPORTING CONTAINER_NAME = MYAREA.

     CREATE OBJECT ALV_GRID
            EXPORTING I_PARENT = G_CUSTOM_CONTAINER .
     PERFORM ALV_LIST_TAB.
     PERFORM BUILD_LAYOUT.
     PERFORM  BUILD_FIELDCATALOG   TABLES GT_FIELDCAT_SLIS.


     CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
            EXPORTING  I_DEFAULT        = ' '
                       IS_LAYOUT        = GS_LAYOUT
                       IS_VARIANT       = GS_VARIANT
                       I_STRUCTURE_NAME = 'IT_LIST'
            CHANGING   IT_OUTTAB        = IT_CHANGE[]
                       IT_FIELDCATALOG  = GT_FIELDCAT[].

  ENDIF.

ENDMODULE.                 " CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ALV_LIST_TAB
*&---------------------------------------------------------------------*
FORM ALV_LIST_TAB.
CLEAR GS_VARIANT.
GS_VARIANT-REPORT = SY-REPID.
GS_VARIANT-USERNAME = SY-UNAME.
ENDFORM.                    " ALV_LIST_TAB
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT.
       CLEAR: GS_LAYOUT.
IF S_EQUI[] IS INITIAL.
      GS_LAYOUT-GRID_TITLE = 'LINE-BACK LIST'.
ELSE.
      GS_LAYOUT-GRID_TITLE = 'SPEC CHANGE LIST'.
ENDIF.
      GS_LAYOUT-SMALLTITLE = 'X'.
      GS_LAYOUT-CWIDTH_OPT = 'X'.
      GS_LAYOUT-ZEBRA      = 'X'.

ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*

FORM BUILD_FIELDCATALOG  TABLES   P_FIELDCAT_SLIS TYPE
                                  SLIS_T_FIELDCAT_ALV.
  DATA  : LS_FVAT        TYPE LVC_S_FCAT.
  DATA: LT_REPID    TYPE SY-REPID    .
  LT_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = LT_REPID        "SY-REPID
      I_INTERNAL_TABNAME     = 'IT_CHANGE'
      I_INCLNAME             = LT_REPID        "SY-REPID
    CHANGING
      CT_FIELDCAT            = P_FIELDCAT_SLIS[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2.

      LOOP AT P_FIELDCAT_SLIS.
        CLEAR IT_FIELDCATALOG .
        MOVE-CORRESPONDING P_FIELDCAT_SLIS TO IT_FIELDCATALOG.
        MOVE P_FIELDCAT_SLIS-REPTEXT_DDIC TO IT_FIELDCATALOG-REPTEXT.
        APPEND IT_FIELDCATALOG TO GT_FIELDCAT.
      ENDLOOP.
ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE OK_CODE.
    WHEN 'EXIT' OR 'CANC' OR 'BACK'. LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
