************************************************************************
* Program Name      : ZRPP307_NATION_LIST
* Author            : JO KIM
* Creation Date     : 2003.12.23.
* Specifications By :
* Pattern           : 2.1
* Development Request No : UD1K902288
* Addl Documentation:
* Description       : Display nation TRT list
* Modification Logs
* Date       Developer    RequestNo    Description
* Change : Change the Time Tule(Tack-Time --> Lead Time)
************************************************************************
REPORT ZRPP307_NATION_LIST NO STANDARD PAGE HEADING
                           MESSAGE-ID ZMPP.

*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ZTBM_ATRT.      "Nation TRT imformation

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : IT_ATRT  LIKE TABLE OF ZTBM_ATRT    WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : WA_FNAME_TX(40),
       WA_SAVELINE_IX     LIKE  SY-INDEX.

*----------------------------------------------------------------------*
* ALV DECLARATION.
*----------------------------------------------------------------------*
DATA : OK_CODE       LIKE  SY-UCOMM,
       SAVE_OK_CODE  LIKE  SY-UCOMM.
DATA : ALV_GRID               TYPE REF TO CL_GUI_ALV_GRID,
       GS_CUSTOM_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       WA_CONTAINER           TYPE SCRFNAME VALUE 'CONTAINER'.
*       GS_APPLICATION         TYPE REF TO LCL_APPLICATION,
DATA : GS_VARIANT             TYPE DISVARIANT ,  "Display Variant
       GS_LAYOUT         TYPE LVC_S_LAYO ,  "Layout
       GS_PRINT          TYPE LVC_S_PRNT ,  "Print control
       GT_SPECIAL_GROUPS TYPE LVC_T_SGRP ,  "Field groups
       GT_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS , "Exclu Toolbar Std FUNC
       GT_HEADER         TYPE TABLE OF SLIS_LISTHEADER WITH HEADER LINE,
       GT_FIELDCAT       TYPE LVC_T_FCAT ,  "Field Catalog
       GT_SORT           TYPE LVC_T_SORT ,  "Sort criteria
       GT_FILTER         TYPE LVC_T_FILT .  "Filter criteria
DATA : WA_FIELDCAT     TYPE LVC_S_FCAT.

*----------------------------------------------------------------------*
* DECLARATION FOR SEARCH HELP
*----------------------------------------------------------------------*
DATA DYNPREAD LIKE DYNPREAD OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF VALUETAB OCCURS 0,
          VALUE(80).
DATA: END OF VALUETAB.

DATA: BEGIN OF FIELDS OCCURS 0.
        INCLUDE STRUCTURE HELP_VALUE.
DATA: END OF FIELDS.

DATA: BEGIN OF DYNPFIELDS  OCCURS 0.
        INCLUDE STRUCTURE DYNPREAD.
DATA: END OF DYNPFIELDS.

DATA  SELECT_INDEX LIKE SY-TABIX.

DATA: BEGIN OF SELECT_VALUES OCCURS 0.
        INCLUDE STRUCTURE HELP_VTAB.
DATA: END OF SELECT_VALUES.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS : P_NATN  LIKE  ZTBM_ATRT-NATN_C,
             P_DELR  LIKE  ZTBM_ATRT-DELR_C.
*SELECT-OPTIONS : S_NATN  FOR  ZTBM_ATRT-NATN_C  NO INTERVALS,
*                 S_DELR  FOR  ZTBM_ATRT-DELR_C  NO INTERVALS.
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
* INITIALIZAION
************************************************************************
INITIALIZATION.
*  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN ON VALUE-REQUEST
************************************************************************
*-----> AT SELECTION-SCREEN ON VALUE-REQUEST
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_NATN.
  PERFORM HELP_REQUEST_NATN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_DELR.
  PERFORM HELP_REQUEST_DELR.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION-SCREEN.


************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM EXECUTE_PROCESS.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
*  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  VALUE_READ
*&---------------------------------------------------------------------*
FORM VALUE_READ USING  P_NAME.
  DYNPREAD-FIELDNAME = P_NAME. APPEND DYNPREAD.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME                   = SY-CPROG
            DYNUMB                   = SY-DYNNR
       TABLES
            DYNPFIELDS               = DYNPREAD
*      EXCEPTIONS
*           INVALID_ABAPWORKAREA     = 1
*           INVALID_DYNPROFIELD      = 2
*           INVALID_DYNPRONAME       = 3
*           INVALID_DYNPRONUMMER     = 4
*           INVALID_REQUEST          = 5
*           NO_FIELDDESCRIPTION      = 6
*           INVALID_PARAMETER        = 7
*           UNDEFIND_ERROR           = 8
*           DOUBLE_CONVERSION        = 9
*           OTHERS                   = 10
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " VALUE_READ

*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDS
*&---------------------------------------------------------------------*
FORM ADD_FIELDS USING  P_TABNAME P_FIELDNAME P_FLAG.
  FIELDS-TABNAME = P_TABNAME.
  FIELDS-FIELDNAME = P_FIELDNAME.
  FIELDS-SELECTFLAG = P_FLAG.
  APPEND FIELDS.      CLEAR FIELDS.
ENDFORM.                    " ADD_FIELDS

*&---------------------------------------------------------------------*
*&      Form  HELP_VALUES_GET
*&---------------------------------------------------------------------*
FORM HELP_VALUES_GET.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            DISPLAY                   = ' '
       IMPORTING
            INDEX                     = SELECT_INDEX
       TABLES
            FIELDS                    = FIELDS
            SELECT_VALUES             = SELECT_VALUES
            VALUETAB                  = VALUETAB
       EXCEPTIONS
            FIELD_NOT_IN_DDIC         = 1
            MORE_THEN_ONE_SELECTFIELD = 2
            NO_SELECTFIELD            = 3
            OTHERS                    = 4.
ENDFORM.                    " HELP_VALUES_GET

*&---------------------------------------------------------------------*
*&      Form  VALUE_UPDATE
*&---------------------------------------------------------------------*
FORM VALUE_UPDATE USING  P_PROCESS
                         P_FIELDNAME
                         P_FIELDVALUE
                         P_STEPL.
  CLEAR DYNPFIELDS.
  DYNPFIELDS-FIELDNAME = P_FIELDNAME.
  DYNPFIELDS-FIELDVALUE = P_FIELDVALUE.
  IF P_STEPL > 0.
    DYNPFIELDS-STEPL = P_STEPL.
  ENDIF.
  APPEND DYNPFIELDS.      CLEAR DYNPFIELDS.

  IF P_PROCESS EQ 'X'.
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
         EXPORTING
              DYNAME               = SY-CPROG
              DYNUMB               = SY-DYNNR
         TABLES
              DYNPFIELDS           = DYNPFIELDS
         EXCEPTIONS
              INVALID_ABAPWORKAREA = 1
              INVALID_DYNPROFIELD  = 2
              INVALID_DYNPRONAME   = 3
              INVALID_DYNPRONUMMER = 4
              INVALID_REQUEST      = 5
              NO_FIELDDESCRIPTION  = 6
              UNDEFIND_ERROR       = 7
              OTHERS               = 8.
    REFRESH DYNPFIELDS.
  ENDIF.

ENDFORM.                    " VALUE_UPDATE

*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_NATN
*&---------------------------------------------------------------------*
FORM HELP_REQUEST_NATN.
  DATA : L_NATN_C TYPE  ZTBM_ATRT-NATN_C,
         L_DELR_C TYPE  ZTBM_ATRT-DELR_C.
  DATA : BEGIN OF IT_NATN OCCURS 0,
          NATN_C  TYPE  ZTBM_ATRT-NATN_C,
          DELR_C  TYPE  ZTBM_ATRT-DELR_C,
          TEXT_E  TYPE  ZTBM_ATRT-TEXT_E.
  DATA : END OF IT_NATN.
  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.

  PERFORM VALUE_READ USING: 'P_DELR'.
  LOOP AT DYNPREAD.
    CASE SY-TABIX.
      WHEN 1.
        CONCATENATE DYNPREAD-FIELDVALUE '%' INTO L_DELR_C.
    ENDCASE.
  ENDLOOP.

  SELECT NATN_C
         DELR_C
         TEXT_E
         INTO TABLE IT_NATN
         FROM ZTBM_ATRT
         WHERE DELR_C LIKE L_DELR_C.

  LOOP AT IT_NATN.
    VALUETAB-VALUE = IT_NATN-NATN_C.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = IT_NATN-DELR_C.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = IT_NATN-TEXT_E.
    APPEND VALUETAB. CLEAR VALUETAB.
  ENDLOOP.

  PERFORM ADD_FIELDS USING: 'ZTBM_ATRT' 'NATN_C' 'X',
                            'ZTBM_ATRT' 'DELR_C' 'X',
                            'ZTBM_ATRT' 'TEXT_E' ' '.
  PERFORM HELP_VALUES_GET.

  IF SELECT_INDEX > 0.
    READ TABLE IT_NATN   INDEX SELECT_INDEX.
    PERFORM VALUE_UPDATE USING:
            ' '   'P_NATN' IT_NATN-NATN_C 0,
            'X'   'P_DELR' IT_NATN-DELR_C 0.
  ENDIF.
  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.

ENDFORM.                    " HELP_REQUEST_NATN
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_DELR
*&---------------------------------------------------------------------*
FORM HELP_REQUEST_DELR.
  DATA : L_NATN_C TYPE  ZTBM_ATRT-NATN_C,
         L_DELR_C TYPE  ZTBM_ATRT-DELR_C.
  DATA : BEGIN OF IT_NATN OCCURS 0,
          NATN_C  TYPE  ZTBM_ATRT-NATN_C,
          DELR_C  TYPE  ZTBM_ATRT-DELR_C,
          TEXT_E  TYPE  ZTBM_ATRT-TEXT_E.
  DATA : END OF IT_NATN.
  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.

  PERFORM VALUE_READ USING: 'P_NATN'.
  LOOP AT DYNPREAD.
    CASE SY-TABIX.
      WHEN 1.
        CONCATENATE DYNPREAD-FIELDVALUE '%' INTO L_NATN_C.
    ENDCASE.
  ENDLOOP.

  SELECT NATN_C
         DELR_C
         TEXT_E
         INTO TABLE IT_NATN
         FROM ZTBM_ATRT
         WHERE NATN_C LIKE L_NATN_C.

  LOOP AT IT_NATN.
    VALUETAB-VALUE = IT_NATN-NATN_C.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = IT_NATN-DELR_C.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = IT_NATN-TEXT_E.
    APPEND VALUETAB. CLEAR VALUETAB.
  ENDLOOP.

  PERFORM ADD_FIELDS USING: 'ZTBM_ATRT' 'NATN_C' 'X',
                            'ZTBM_ATRT' 'DELR_C' 'X',
                            'ZTBM_ATRT' 'TEXT_E' ' '.
  PERFORM HELP_VALUES_GET.

  IF SELECT_INDEX > 0.
    READ TABLE IT_NATN   INDEX SELECT_INDEX.
    PERFORM VALUE_UPDATE USING:
            ' '   'P_NATN' IT_NATN-NATN_C 0,
            'X'   'P_DELR' IT_NATN-DELR_C 0.
  ENDIF.
  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.

ENDFORM.                    " HELP_REQUEST_DELR
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXECUTE_PROCESS.

  PERFORM SELECT_ZTBM_ATRT.

  CALL SCREEN 9000.


ENDFORM.                    " EXECUTE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZTBM_ATRT
*&---------------------------------------------------------------------*
FORM SELECT_ZTBM_ATRT.
  DATA : L_NATN_C   LIKE  ZTBM_ATRT-NATN_C,
         L_DELR_C   LIKE  ZTBM_ATRT-DELR_C.
  CONCATENATE : P_NATN  '%'  INTO  L_NATN_C,
                P_DELR  '%'  INTO  L_DELR_C.
  SELECT *
         INTO TABLE IT_ATRT
         FROM ZTBM_ATRT
         WHERE NATN_C LIKE L_NATN_C
           AND DELR_C LIKE L_DELR_C.
ENDFORM.                    " SELECT_ZTBM_ATRT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'MAIN'.
*  SET TITLEBAR '9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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

*-----> SET OBJECT
    CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
        I_STRUCTURE_NAME              = 'ZTBM_ATRT'
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
        IT_OUTTAB                     = IT_ATRT[]
        IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
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
  DATA: L_STRUCT    LIKE DD02L-TABNAME.


  DATA: ZERO_FNAME1(20),
        ZERO_FNAME2(20),
        ZERO_CNT TYPE I.

  L_STRUCT = 'ZTBM_ATRT'.
  CLEAR : WA_FIELDCAT, GT_FIELDCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_BUFFER_ACTIVE        = 'X'
            I_STRUCTURE_NAME       = L_STRUCT
       CHANGING
            CT_FIELDCAT            = GT_FIELDCAT[]
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.

*  DELETE GT_FIELDCAT  WHERE FIELDNAME = 'MANDT' OR
*                            FIELDNAME = 'ZUSER' OR
*                            FIELDNAME = 'ZSDAT' OR
*                            FIELDNAME = 'ZSTIM' OR
*                            FIELDNAME = '' OR
*                            FIELDNAME = ''.

  LOOP AT GT_FIELDCAT INTO WA_FIELDCAT.
    PERFORM SET_FIELD_INFO USING WA_FIELDCAT.
    MODIFY GT_FIELDCAT FROM WA_FIELDCAT.
    CLEAR WA_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_INFO
*&---------------------------------------------------------------------*
FORM SET_FIELD_INFO USING L_FIELDCAT STRUCTURE LVC_S_FCAT.

  CASE L_FIELDCAT-FIELDNAME.
    WHEN 'NATN_C'.
*      SET_FIELDCAT  'FLG' 3.
*      L_FIELDCAT-KEY = 'X'.
    WHEN 'DELR_C'.
    WHEN 'TEXT_E'.
    WHEN 'PEL0_EOMIT'.
    WHEN 'DRIV_CTYPE'.
    WHEN 'NATN_CWEAT'.
    WHEN 'BASE_CNATN'.
    WHEN 'REPR_CAREA'.
    WHEN 'BULB_CALC'.
    WHEN 'BULB_CLOCT'.
    WHEN 'BULB_C'.
    WHEN 'SMTR_CALC'.
    WHEN 'SMTR_CLOCT'.
    WHEN 'SMTR_C'.
    WHEN 'AUDF_CALC'.
    WHEN 'AUDF_CLOCT'.
    WHEN 'AUDF_C'.
    WHEN 'STLF_CALC'.
    WHEN 'STLF_CLOCT'.
    WHEN 'STLF_C'.
    WHEN 'TMPT_CALC'.
    WHEN 'TMPT_CLOCT'.
    WHEN 'TMPT_C'.
    WHEN 'TMPT_CMIN'.
    WHEN 'BLNK_CALC1'.
    WHEN 'BLNK_CLOC1'.
    WHEN 'BLNK_CSEQ1'.
    WHEN 'BLNK_CALC2'.
    WHEN 'BLNK_CLOC2'.
    WHEN 'BLNK_CSEQ2'.
    WHEN 'BLNK_CALC3'.
    WHEN 'BLNK_CLOC3'.
    WHEN 'BLNK_CSEQ3'.
    WHEN 'BLNK_CALC4'.
    WHEN 'BLNK_CLOC4'.
    WHEN 'BLNK_CSEQ4'.
    WHEN 'BLNK_CALC5'.
    WHEN 'BLNK_CLOC5'.
    WHEN 'BLNK_CSEQ5'.

  ENDCASE.


ENDFORM.                    " SET_FIELD_INFO
*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_CURSOR_FIELD OUTPUT.
  SET CURSOR FIELD WA_FNAME_TX LINE WA_SAVELINE_IX.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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
*       text
*----------------------------------------------------------------------*
MODULE READ_DATA INPUT.
  PERFORM SELECT_ZTBM_ATRT.
  CALL  METHOD GS_CUSTOM_CONTAINER->FREE.
  FREE  GS_CUSTOM_CONTAINER.

ENDMODULE.                 " READ_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CURSOR_FIELD INPUT.
  CLEAR: WA_FNAME_TX, WA_SAVELINE_IX.
  GET CURSOR FIELD WA_FNAME_TX LINE WA_SAVELINE_IX.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*&      Module  NATN_ON_VALUE_REQUEST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE NATN_ON_VALUE_REQUEST INPUT.
 PERFORM HELP_REQUEST_NATN.
ENDMODULE.                 " NATN_ON_VALUE_REQUEST  INPUT
*&---------------------------------------------------------------------*
*&      Module  DELR_ON_VALUE_REQUEST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DELR_ON_VALUE_REQUEST INPUT.
 PERFORM HELP_REQUEST_DELR.
ENDMODULE.                 " DELR_ON_VALUE_REQUEST  INPUT
