************************************************************************
* Program Name      : ZRPP309R_MAT_NOTIFICATION
* Author            : Bongsoo, Kim
* Creation Date     : 2003.09.26.
* Specifications By : Bongsoo, Kim
* Pattern           : 1.1
* Development Request No : UD1K902302
* Addl Documentation:
* Description       : Material Master Notification
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZRPP309R_MAT_NOTIFICATION
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
*  TYPE-POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ZTBM_ABXMMRDT,
         ZTBM_AMMR,
         MAKT.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_AMMR OCCURS 0,
        MTNO  TYPE ZTBM_AMMR-MTNO,
        PLNT  TYPE ZTBM_AMMR-PLNT,
        ZDATE TYPE ZTBM_AMMR-ZDATE,
        NMTY  TYPE ZTBM_AMMR-NMTY,
        OMTY  TYPE ZTBM_AMMR-OMTY,
        NPTY  TYPE ZTBM_AMMR-NPTY,
        OPTY  TYPE ZTBM_AMMR-OPTY,
        ZDESC TYPE ZTBM_AMMR-ZDESC,
        NMRP  TYPE ZTBM_AMMR-NMRP,
        OMRP  TYPE ZTBM_AMMR-OMRP,
      END OF IT_AMMR.
*----------------------------------------------------------------------*
* ALV DECLARATION.
*----------------------------------------------------------------------*
DATA : OK_CODE       LIKE  SY-UCOMM,
       SAVE_OK_CODE  LIKE  SY-UCOMM.
DATA : ALV_GRID               TYPE REF TO CL_GUI_ALV_GRID,
       GS_CUSTOM_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       WA_CONTAINER           TYPE SCRFNAME VALUE 'CONTAINER'.
*       GS_APPLICATION         TYPE REF TO LCL_APPLICATION,
DATA : GS_VARIANT        TYPE DISVARIANT ,  "Display Variant
       GS_LAYOUT         TYPE LVC_S_LAYO ,  "Layout
       GS_PRINT          TYPE LVC_S_PRNT ,  "Print control
       GT_SPECIAL_GROUPS TYPE LVC_T_SGRP ,  "Field groups
       GT_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS , "Exclu Toolbar Std FUNC
       GT_HEADER         TYPE TABLE OF SLIS_LISTHEADER WITH HEADER LINE,
       GT_FIELDCAT       TYPE LVC_T_FCAT ,  "Field Catalog
       GT_SORT           TYPE LVC_T_SORT ,  "Sort criteria
       GT_FILTER         TYPE LVC_T_FILT .  "Filter criteria
DATA : WA_FIELDCAT     TYPE LVC_S_FCAT,
       WA_SORT          TYPE LVC_S_SORT.  "Sort criteria.
*----------------------------------------------------------------------*
*  RANGES
*----------------------------------------------------------------------*
RANGES: R_ZDATE FOR ZTBM_AMMR-ZDATE,
        R_MTNO  FOR ZTBM_AMMR-MTNO,
        R_PLNT  FOR ZTBM_AMMR-PLNT.
*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK    VALUE  'X',
           C_GUBB    VALUE  '*',
           C_MITU    VALUE  'M'.
DATA P_POS(02) TYPE N.
*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS: S_ZDATE FOR ZTBM_AMMR-ZDATE OBLIGATORY
                                            NO-EXTENSION,
                S_MTNO  FOR ZTBM_AMMR-MTNO NO-EXTENSION,
                S_PLNT  FOR ZTBM_AMMR-PLNT NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK B1.


*----------------------------------------------------------------------*
*  MACRO DECLARATION
*----------------------------------------------------------------------*
DEFINE FIELDCAT_COMPOSE.

  IT_FIELDCAT-FIELDNAME = &1.
  IT_FIELDCAT-REPTEXT   = &2.
  IT_FIELDCAT-OUTPUTLEN = 20.
  APPEND IT_FIELDCAT TO GT_FIELDCAT.

END-OF-DEFINITION.

DEFINE SET_FIELDCAT.
  L_FIELDCAT-REPTEXT   = &1.
  L_FIELDCAT-SCRTEXT_L = &1.
  L_FIELDCAT-SCRTEXT_M = &1.
  L_FIELDCAT-SCRTEXT_S = &1.
  L_FIELDCAT-COLDDICTXT = 'L'.
  L_FIELDCAT-OUTPUTLEN = &2.

END-OF-DEFINITION.
************************************************************************
* INITIALIZATION
************************************************************************
INITIALIZATION.
  PERFORM INITIALIZATION.
************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN OUTPUT.
*  PERFORM SCREEN_MODIFY.
************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
  PERFORM AT_SELECTION-SCREEN.
************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM EXECUTE_PROCESS.

END-OF-SELECTION.
  CALL SCREEN 9000.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  CONCATENATE SY-DATUM(06) '01' INTO S_ZDATE-LOW.
  S_ZDATE-HIGH = SY-DATUM.
  S_ZDATE-SIGN = 'I'.
  S_ZDATE-OPTION = 'BT'.
  APPEND S_ZDATE.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXECUTE_PROCESS.
  REFRESH IT_AMMR. CLEAR IT_AMMR.

  SELECT MTNO  "TYPE ZTBM_AMMR-MTNO,
         PLNT  "TYPE ZTBM_AMMR-PLNT,
         ZDATE "TYPE ZTBM_AMMR-ZDATE,
         NMTY  "TYPE ZTBM_AMMR-NMTY,
         OMTY  "TYPE ZTBM_AMMR-OMTY,
         NPTY  "TYPE ZTBM_AMMR-NPTY,
         OPTY  "TYPE ZTBM_AMMR-OPTY,
         ZDESC "TYPE ZTBM_AMMR-ZDESC,
         NMRP  "TYPE ZTBM_AMMR-NMRP,
         OMRP  "TYPE ZTBM_AMMR-OMRP,
       FROM  ZTBM_AMMR
       INTO TABLE IT_AMMR
       WHERE  MTNO   IN R_MTNO
       AND    PLNT   IN R_PLNT
       AND    ZDATE  IN R_ZDATE.
  IF SY-SUBRC EQ 0.
    SORT IT_AMMR BY MTNO PLNT ZDATE.
  ENDIF.


ENDFORM.                    " EXECUTE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION-SCREEN.

  CLEAR: R_ZDATE, R_ZDATE[].
  LOOP AT S_ZDATE.
    MOVE-CORRESPONDING S_ZDATE TO R_ZDATE.
    APPEND R_ZDATE.
  ENDLOOP.

  CLEAR: R_MTNO, R_MTNO[].
  LOOP AT S_MTNO.
    MOVE-CORRESPONDING S_MTNO TO R_MTNO.
    APPEND R_MTNO.
  ENDLOOP.

  CLEAR: R_PLNT, R_PLNT[].
  LOOP AT S_PLNT.
    MOVE-CORRESPONDING S_PLNT TO R_PLNT.
    APPEND R_PLNT.
  ENDLOOP.
ENDFORM.                    " AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'MAIN'.
  SET TITLEBAR '9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
MODULE CREATE_ALV_GRID OUTPUT.
  IF GS_CUSTOM_CONTAINER IS INITIAL.
*-----> CREATE OBJECT
*    CREATE OBJECT GS_APPLICATION.

    CREATE OBJECT GS_CUSTOM_CONTAINER
        EXPORTING CONTAINER_NAME = WA_CONTAINER.

    CREATE OBJECT ALV_GRID
        EXPORTING I_PARENT = GS_CUSTOM_CONTAINER.

    PERFORM  BUILD_VARIANT.
    PERFORM  BUILD_LAYOUT.
    PERFORM  BUILD_FIELDCAT.
    PERFORM  BUILD_SORT USING: '1' 'PLNT' 'X',
                               '2' 'MTNO' 'X',
                               '3' 'ZDATE' 'X'.
*-----> SET OBJECT
    CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
        I_STRUCTURE_NAME              = 'IT_AMMR'
        IS_VARIANT                    = GS_VARIANT
        I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
        IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
      CHANGING
        IT_OUTTAB                     = IT_AMMR[]
        IT_FIELDCATALOG               = GT_FIELDCAT[]
    IT_SORT                       =  GT_SORT
*        IT_FILTER                     =
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.



  ENDIF.
ENDMODULE.                 " CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_VARIANT
*&---------------------------------------------------------------------*
FORM BUILD_VARIANT.
  GS_VARIANT-REPORT = SY-REPID.
ENDFORM.                    " BUILD_VARIANT
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM BUILD_LAYOUT.
  GS_LAYOUT-ZEBRA  = 'X'.       "ZEBRA
  GS_LAYOUT-CWIDTH_OPT = 'X'.   "OPTIMIZE COLUMN WIDTH
  GS_LAYOUT-DETAILINIT = 'X'.   "DISPLAY INITIAL VALUES ON DETAIL SCREEN

ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM BUILD_FIELDCAT.
  REFRESH GT_FIELDCAT. CLEAR GT_FIELDCAT.
  PERFORM FIELD_CATALOG USING:
     '1'  'X' ''  'PLNT'  ''  '05'  'PLANT'        ''  'L'  '',
     '2'  'X' ''  'MTNO'  ''  '18'  'MATERIAL NO'  ''  'L'  '',
     '3'  ' ' ''  'ZDESC' ''  '40'  'DESCRIPTION'  ''  'L'  '',
     '4'  ' ' ''  'NMTY'  ''  '08'  'MAT/TYPE'     ''  'L'  '',
     '5'  ' ' ''  'OMTY'  ''  '08'  'OLD/TYPE'     ''  'L'  '',
     '6'  ' ' ''  'NPTY'  ''  '10'  'IND/SECTOR'   ''  'L'  '',
     '7'  ' ' ''  'OPTY'  ''  '10'  'OLD/SECTOR'   ''  'L'  '',
     '8'  ' ' ''  'NMRP'  ''  '12'  'MRP TYPE'     ''  'L'  '',
     '9'  ' ' ''  'OMRP'  ''  '12'  'OLD MRP TYPE' ''  'L'  '',
     '10' ' ' ''  'ZDATE' ''  '10'  'DATE'         ''  'L'  ''.
*  CLEAR Z_FLAG.
ENDFORM.                    " BUILD_FIELDCAT
*---------------------------------------------------------------------*
*       FORM FIELD_CATALOG                                            *
*---------------------------------------------------------------------*
FORM FIELD_CATALOG USING P_POS P_KEY P_NO_OUT P_FIELDNAME P_DO_SUM
                         P_OUTPUTLEN P_REPTEXT_DDIC P_NO_ZERO P_JUST
                         P_CFIELDNAME.
  DATA : LS_FIELDCAT TYPE LVC_S_FCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-COL_POS       = P_POS.               "COLUMN
  LS_FIELDCAT-TABNAME       = 'IT_AOPV'.
  LS_FIELDCAT-CHECKBOX       = ' '.
*  LS_FIELDCAT-CTABNAME      = 'IT_LIST'.
  LS_FIELDCAT-KEY           = P_KEY .            "KEY
  LS_FIELDCAT-NO_OUT        = P_NO_OUT.          "
  LS_FIELDCAT-FIELDNAME     = P_FIELDNAME.       "
  LS_FIELDCAT-DO_SUM        = P_DO_SUM.          "
  LS_FIELDCAT-OUTPUTLEN     = P_OUTPUTLEN.       "
  LS_FIELDCAT-REPTEXT       = P_REPTEXT_DDIC.    "
  LS_FIELDCAT-NO_ZERO       = P_NO_ZERO.
*'L' LEFT 'R' RIGHT 'C' CENTERED
  LS_FIELDCAT-JUST          = P_JUST.        "
  LS_FIELDCAT-CFIELDNAME    = P_CFIELDNAME.  "
*LS_FIELDCAT-CURRENCY = 'KRW'.     "
*LS_FIELDCAT-QUANTITY = 'QUAN.     "
*  LS_FIELDCAT-DATATYPE = 'CURR'.       "
  APPEND LS_FIELDCAT TO  GT_FIELDCAT.
  CLEAR LS_FIELDCAT.

ENDFORM.                    " FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
FORM BUILD_SORT USING    P_SPOS
                         P_FIELDNAME
                         P_UP.
  WA_SORT-SPOS = P_SPOS.	     "Sort sequence
  WA_SORT-FIELDNAME = P_FIELDNAME.    "ALV control: Field name of *
*                                        internal table field

  WA_SORT-UP  = P_UP.         "Single-character flag
*    GT_SORT-DOWN         "Single-character flag
*    GT_SORT-GROUP        "New group: Insert page feed, underlin
*    GT_SORT-SUBTOT       "Output subtotal
*    GT_SORT-COMP         "Single-character flag
*    GT_SORT-EXPA         "Single-character flag
*    GT_SORT-SELTEX       "Sort criterion
*    GT_SORT-OBLIGATORY   "Single-character flag
*    GT_SORT-LEVEL        "Natural number
*    GT_SORT-NOGT_SORT-OUT       "Single-character flag
*    GT_SORT-INTOPT       "Internal optimization (INTERNAL USE)
  APPEND WA_SORT TO GT_SORT.
ENDFORM.                    " BUILD_SORT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE OK_CODE.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DATA  INPUT
*&---------------------------------------------------------------------*
MODULE READ_DATA INPUT.
  IF R_ZDATE-LOW GT R_ZDATE-HIGH OR R_MTNO-LOW GT R_MTNO-HIGH OR
     R_PLNT-LOW GT R_PLNT-HIGH.
    MESSAGE E001 WITH 'Lower limit is greater than upper limit'.
  ELSE.
    PERFORM READ_DATA.
  ENDIF.
ENDMODULE.                 " READ_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM READ_DATA.
  CLEAR : R_ZDATE[].
  IF R_ZDATE-HIGH IS INITIAL.
    R_ZDATE-SIGN = 'I'.
    R_ZDATE-OPTION = 'EQ'.
    APPEND R_ZDATE.
  ELSE.
    R_ZDATE-SIGN = 'I'.
    R_ZDATE-OPTION = 'BT'.
    APPEND R_ZDATE.
  ENDIF.
  CLEAR : R_MTNO[].
  IF R_MTNO-HIGH IS INITIAL.
    R_MTNO-SIGN = 'I'.
    R_MTNO-OPTION = 'EQ'.
    APPEND R_MTNO.
  ELSE.
    R_MTNO-SIGN = 'I'.
    R_MTNO-OPTION = 'BT'.
    APPEND R_MTNO.
  ENDIF.

  CLEAR : R_PLNT[].
  IF R_PLNT-HIGH IS INITIAL.
    R_PLNT-SIGN = 'I'.
    R_PLNT-OPTION = 'EQ'.
    APPEND R_PLNT.
  ELSE.
    R_PLNT-SIGN = 'I'.
    R_PLNT-OPTION = 'BT'.
    APPEND R_PLNT.
  ENDIF.

  PERFORM EXECUTE_PROCESS.

  CALL  METHOD GS_CUSTOM_CONTAINER->FREE.
  FREE  GS_CUSTOM_CONTAINER.
ENDFORM.                    " READ_DATA
