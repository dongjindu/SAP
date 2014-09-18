************************************************************************
* Program Name      : ZIPP600I_SET_ZTPPPB
* Author            : JongOh, Kim
* Creation Date     : 2004.02.14
* Specifications By : JongOh, Kim
* Pattern           : 5.2.2
* Development Request No : UD1K907281
* Addl Documentation:
* Description       :Transfer of Body Input Plan by Press Parts
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************

REPORT ZIPP600I_SET_ZTPPPB  NO STANDARD PAGE HEADING
                               MESSAGE-ID ZMPP.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : MARA,        "General Material Data
         MARC,        "Plant Data for Material
         MARD,        "Storage Location Data for Material
         T001W,       "Plants/Branches
         MKAL,        "Production Versions of Material
         MAKT,        "Material Descriptions
         ZTPPPB,      "Body Input Plan by Press Parts from PP to MES
         ZSPPPB_RFC . "

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_MARC OCCURS 0,
        WERKS   TYPE   MARC-WERKS,
        MATNR   TYPE   MARC-MATNR.
DATA: END OF IT_MARC.

DATA: IT_MDSUX   LIKE  TABLE OF MDSU       WITH HEADER LINE.

DATA: IT_ZTPPPB  LIKE  TABLE OF ZTPPPB     WITH HEADER LINE.
DATA: IT_ZSPPPB  LIKE  TABLE OF ZSPPPB_RFC WITH HEADER LINE.
*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : WA_DATUM_LOW     TYPE    SY-DATUM      ,
       WA_DATUM_HIGH    TYPE    SY-DATUM      .

DATA : WA_COUNT         TYPE    SY-TABIX      .

DATA : WA_FNAME_TX(40),
       WA_SAVELINE_IX     LIKE  SY-INDEX      ,
       WA_FABKL           LIKE  T001W-FABKL   . "Factory calendar key

DATA : WA_ERROR_IX        LIKE  SY-INDEX      ,
       WA_SUCCESS_IX      LIKE  SY-INDEX      ,
       WA_TOTAL_IX        LIKE  SY-INDEX      .

RANGES: R_MATNR         FOR    MARA-MATNR     .
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
       GS_SORT           TYPE LVC_S_SORT ,  "Sort Criteria
       GT_SPECIAL_GROUPS TYPE LVC_T_SGRP ,  "Field groups
       GT_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS , "Exclu Toolbar Std FUNC
       GT_HEADER         TYPE TABLE OF SLIS_LISTHEADER WITH HEADER LINE,
       GT_FIELDCAT       TYPE LVC_T_FCAT ,  "Field Catalog
       GT_SORT           TYPE LVC_T_SORT ,  "Sort criteria
       GT_FILTER         TYPE LVC_T_FILT .  "Filter criteria
DATA : WA_FIELDCAT       TYPE LVC_S_FCAT.

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK    VALUE  'X',
           C_DEST(10) VALUE 'WMPP01'.   "Outbound Interface Destination

CONSTANTS: C_SPP     TYPE   MARC-FEVOR   VALUE  'SPP',
           C_SPB     TYPE   MARC-FEVOR   VALUE  'SPB',
           C_P128    TYPE   MARD-LGORT   VALUE  'P128',
           C_P125    TYPE   MARD-LGORT   VALUE  'P125'.

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

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS : P_WERKS      LIKE   T001W-WERKS OBLIGATORY MEMORY ID WRK,
             P_PANEL      LIKE   T024F-FEVOR OBLIGATORY MEMORY ID CFV,
             P_DATUM      LIKE   SY-DATUM    OBLIGATORY MEMORY ID ZDAT.
*             P_BLANK      LIKE   T024F-FEVOR OBLIGATORY MEMORY ID ZPB.
*SELECT-OPTIONS : S_DATUM   FOR   SY-DATUM  OBLIGATORY NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK B1.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
  PERFORM INITIALIZATION.

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

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  CALL SCREEN 9000.
*  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  P_WERKS  =  'P001'.
  P_PANEL  =  C_SPP .
  P_DATUM  =  SY-DATUM.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION-SCREEN.
  DATA : L_NUM      TYPE    I    .
  CLEAR : WA_FABKL   .
  SELECT SINGLE FABKL
           INTO WA_FABKL
           FROM T001W
           WHERE WERKS EQ P_WERKS.
  IF SY-SUBRC NE 0.
    MESSAGE E001 WITH TEXT-301.
  ENDIF.

  IF P_DATUM LT SY-DATUM.
    MESSAGE E001 WITH TEXT-302.
  ELSE.
    WA_DATUM_LOW   = P_DATUM .
    WA_DATUM_HIGH  = WA_DATUM_LOW .

    DO .
      CALL FUNCTION 'DATE_CHECK_WORKINGDAY'
           EXPORTING
                DATE                       = WA_DATUM_HIGH
                FACTORY_CALENDAR_ID        = WA_FABKL
                MESSAGE_TYPE               = 'E'
           EXCEPTIONS
                DATE_AFTER_RANGE           = 1
                DATE_BEFORE_RANGE          = 2
                DATE_INVALID               = 3
                DATE_NO_WORKINGDAY         = 4
                FACTORY_CALENDAR_NOT_FOUND = 5
                MESSAGE_TYPE_INVALID       = 6
                OTHERS                     = 7.
      IF SY-SUBRC EQ 0.
        L_NUM = L_NUM + 1.
      ENDIF.

      IF L_NUM EQ 10.
        EXIT .
      ELSE.
        WA_DATUM_HIGH = WA_DATUM_HIGH + 1.
      ENDIF.

    ENDDO .

  ENDIF.
ENDFORM.                    " AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXECUTE_PROCESS.

*----> SELECT Production scheduler's value is P_PANEL input
  CLEAR : IT_MARC , IT_MARC[].
  SELECT WERKS
         MATNR
        INTO TABLE IT_MARC
        FROM MARC
        WHERE WERKS EQ P_WERKS
          AND FEVOR EQ P_PANEL
          AND LVORM EQ SPACE.

  LOOP AT IT_MARC .
    CLEAR MAKT .
    SELECT SINGLE *
                 FROM MAKT
                 WHERE MATNR EQ IT_MARC-MATNR
                   AND SPRAS EQ SY-LANGU     .

    CLEAR : IT_MDSUX ,  IT_MDSUX[].
    CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
      EXPORTING
*          PLSCN                          =
        MATNR                          =  IT_MARC-MATNR
        WERKS                          =  IT_MARC-WERKS
*          BERID                          =
*          ERGBZ                          =
*          AFIBZ                          =
*          INPER                          =
*          DISPLAY_LIST_MDPSX             =
*          DISPLAY_LIST_MDEZX             =
*          DISPLAY_LIST_MDSUX             =
*        IMPORTING
*          E_MT61D                        =
*          E_MDKP                         =
      TABLES
*          MDPSX                          =
*          MDEZX                          =
        MDSUX                          = IT_MDSUX
      EXCEPTIONS
        MATERIAL_PLANT_NOT_FOUND       = 1
        PLANT_NOT_FOUND                = 2
        OTHERS                         = 3   .

    LOOP AT IT_MDSUX  WHERE DAT00 GE WA_DATUM_LOW
                        AND DAT00 LE WA_DATUM_HIGH
                        AND PLANR EQ SPACE
                        AND DELKZ EQ SPACE     .
      IT_ZSPPPB-MANDT      = SY-MANDT                .
      IT_ZSPPPB-ZDATE      = P_DATUM                 .
      IT_ZSPPPB-PRS_PDATE  = IT_MDSUX-DAT00          .
      IT_ZSPPPB-PRS_PB_NO  = IT_MARC-MATNR           .
      IT_ZSPPPB-PRS_PB_DES = MAKT-MAKTX              .
      IT_ZSPPPB-PRS_PQTY   = IT_MDSUX-MNG02 * ( -1 )
                            + IT_MDSUX-MNG01 * ( -1 ) .
      IT_ZSPPPB-MEINS      = 'EA'                    .
      IF IT_ZSPPPB-PRS_PQTY GT 0.
        APPEND IT_ZSPPPB .      CLEAR IT_ZSPPPB .
      ENDIF.
    ENDLOOP .

  ENDLOOP.

  DESCRIBE TABLE IT_ZSPPPB LINES WA_COUNT .
  SORT IT_ZSPPPB BY ZDATE PRS_PDATE PRS_PB_NO .

ENDFORM.                    " EXECUTE_PROCESS
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET TITLEBAR '9000'.
  IF IT_ZSPPPB[] IS INITIAL.
    SET PF-STATUS 'MAIN' EXCLUDING 'ZTRAN'.
  ELSE.
    SET PF-STATUS 'MAIN'.
  ENDIF.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_ALV_GRID OUTPUT.
  IF GS_CUSTOM_CONTAINER IS INITIAL.
*-----> CREATE OBJECT
*    CREATE OBJECT GS_APPLICATION .

    CREATE OBJECT GS_CUSTOM_CONTAINER
        EXPORTING CONTAINER_NAME = WA_CONTAINER.

    CREATE OBJECT ALV_GRID
        EXPORTING I_PARENT = GS_CUSTOM_CONTAINER.

    PERFORM  BUILD_VARIANT.
    PERFORM  BUILD_LAYOUT.
    PERFORM  BUILD_SORT.
    PERFORM  BUILD_FIELDCAT.

*-----> SET OBJECT
    CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
        I_STRUCTURE_NAME              = 'IT_ZSPPPB'
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
        IT_OUTTAB                     = IT_ZSPPPB[]
        IT_FIELDCATALOG               = GT_FIELDCAT[]
        IT_SORT                       = GT_SORT
*        IT_FILTER                     =
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4      .

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

ENDMODULE.                 " CREATE_ALV_GRID  OUTPUT
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
*&      Module  GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CURSOR_FIELD INPUT.
  CLEAR: WA_FNAME_TX, WA_SAVELINE_IX.
  GET CURSOR FIELD WA_FNAME_TX LINE WA_SAVELINE_IX.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK_CODE.
    WHEN 'ZTRAN'.   "ALC TRANSFER
      PERFORM SAVE_AND_TRANSFER.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
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
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
FORM BUILD_SORT.
*GT_SORT-SPOS	    "Sort sequence
*GT_SORT-FIELDNAME    "ALV control: Field name of internal table field
*GT_SORT-UP           "Single-character flag
*GT_SORT-DOWN         "Single-character flag
  CLEAR : GS_SORT, GT_SORT[].
  GS_SORT-SPOS = 1.
  GS_SORT-FIELDNAME = 'PRS_PDATE'.
  GS_SORT-UP        = 'X'.
  APPEND GS_SORT TO GT_SORT.
  GS_SORT-SPOS = 2.
  GS_SORT-FIELDNAME = 'PRS_PB_NO'.
  GS_SORT-UP        = 'X'.
  APPEND GS_SORT TO GT_SORT.
ENDFORM.                    " BUILD_SORT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM BUILD_FIELDCAT.
  DATA: L_STRUCT    LIKE DD02L-TABNAME.


  DATA: ZERO_FNAME1(20),
        ZERO_FNAME2(20),
        ZERO_CNT TYPE I.

  L_STRUCT = 'ZSPPPB_RFC'.
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

  LOOP AT GT_FIELDCAT INTO WA_FIELDCAT .

    PERFORM SET_FIELD_INFO USING WA_FIELDCAT
                                 SY-TABIX .

    CLEAR WA_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_INFO
*&---------------------------------------------------------------------*
FORM SET_FIELD_INFO USING  L_FIELDCAT STRUCTURE LVC_S_FCAT
                           P_TABIX .

  CASE L_FIELDCAT-FIELDNAME.
    WHEN 'PRS_PDATE'.
      SET_FIELDCAT  'Body Plan Date' 16.
      L_FIELDCAT-KEY = 'X'.
      MODIFY GT_FIELDCAT FROM L_FIELDCAT  INDEX P_TABIX .
    WHEN 'PRS_PB_NO'.
      SET_FIELDCAT 'Part #' 18.
      L_FIELDCAT-KEY = 'X'.
      MODIFY GT_FIELDCAT FROM L_FIELDCAT  INDEX P_TABIX .
    WHEN 'PRS_PB_DES'.
      SET_FIELDCAT 'Description ' 40.
*      L_FIELDCAT-KEY = 'X'.
      MODIFY GT_FIELDCAT FROM L_FIELDCAT  INDEX P_TABIX .
    WHEN 'PRS_PQTY'.
      SET_FIELDCAT 'Qty' 10.
      L_FIELDCAT-QUANTITY = 'EA' .
      MODIFY GT_FIELDCAT FROM L_FIELDCAT  INDEX P_TABIX .
    WHEN 'MEINS'.
      SET_FIELDCAT 'Unit' 4.
      MODIFY GT_FIELDCAT FROM L_FIELDCAT  INDEX P_TABIX .
    WHEN OTHERS .
      DELETE GT_FIELDCAT INDEX P_TABIX.
  ENDCASE.
ENDFORM.                    " SET_FIELD_INFO
*&---------------------------------------------------------------------*
*&      Form  SAVE_AND_TRANSFER
*&---------------------------------------------------------------------*
FORM SAVE_AND_TRANSFER.

  DATA : L_MSGTXT(100) .

*---> Interface date update
  IT_ZSPPPB-ZUSER = SY-UNAME.
  IT_ZSPPPB-ZSDAT = SY-DATUM.
  IT_ZSPPPB-ZSTIM = SY-UZEIT.
  CLEAR : IT_ZSPPPB-ZMSG , R_MATNR, R_MATNR[] .

  MODIFY IT_ZSPPPB TRANSPORTING ZUSER ZSDAT ZSTIM
                                WHERE MANDT EQ SY-MANDT.

  DESCRIBE TABLE IT_ZSPPPB  LINES WA_TOTAL_IX .
  CALL FUNCTION 'Z_FPP_SET_ZTPPPB'
    DESTINATION  C_DEST
    TABLES
      T_ZSPPPB       =  IT_ZSPPPB
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

  IF SY-SUBRC NE 0.
    IT_ZSPPPB-ZRESULT  = 'E' .
    IT_ZSPPPB-ZMSG  = L_MSGTXT .
    IT_ZSPPPB-ZEDAT = SY-DATUM .
    IT_ZSPPPB-ZETIM = SY-UZEIT .
    MODIFY IT_ZSPPPB TRANSPORTING ZUSER ZSDAT ZSTIM
                                  ZEDAT ZETIM ZMSG
                                  ZRESULT
                                  WHERE MANDT EQ SY-MANDT .
    WA_ERROR_IX  =  WA_TOTAL_IX .
    MESSAGE I001 WITH L_MSGTXT .

  ELSE .
    IT_ZSPPPB-ZEDAT = SY-DATUM .
    IT_ZSPPPB-ZETIM = SY-UZEIT .
    MODIFY IT_ZSPPPB TRANSPORTING ZUSER ZSDAT ZSTIM
                                  ZEDAT ZETIM ZMSG
                                  WHERE MANDT EQ SY-MANDT .
  ENDIF.

  LOOP AT IT_ZSPPPB .
    MOVE-CORRESPONDING IT_ZSPPPB TO IT_ZTPPPB .
    IF IT_ZTPPPB-ZRESULT EQ SPACE .
      IT_ZTPPPB-ZRESULT  =  IT_ZSPPPB-ZZRET .
    ENDIF.

    IF IT_ZSPPPB-ZZRET EQ 'E'.
      WA_ERROR_IX = WA_ERROR_IX + 1.
    ELSE.
      WA_SUCCESS_IX = WA_SUCCESS_IX + 1.
    ENDIF.
    APPEND IT_ZTPPPB .   CLEAR IT_ZTPPPB .
  ENDLOOP.

  CASE P_PANEL .
    WHEN C_SPP.
      R_MATNR-SIGN   = 'I'  .
      R_MATNR-OPTION = 'NP' .
      R_MATNR-LOW    = 'B*' .
      APPEND R_MATNR .
      DELETE FROM ZTPPPB
             WHERE ZDATE     EQ   P_DATUM
               AND PRS_PB_NO IN   R_MATNR      .
    WHEN C_SPB.
      R_MATNR-SIGN   = 'I'  .
      R_MATNR-OPTION = 'CP' .
      R_MATNR-LOW    = 'B*' .
      APPEND R_MATNR .
      DELETE FROM ZTPPPB
             WHERE ZDATE     EQ   P_DATUM
               AND PRS_PB_NO IN   R_MATNR      .
  ENDCASE.

  INSERT ZTPPPB FROM TABLE IT_ZTPPPB .
  CALL SCREEN 9005 STARTING AT 20 10.
ENDFORM.                    " SAVE_AND_TRANSFER
*&---------------------------------------------------------------------*
*&      Module  STATUS_9005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9005 OUTPUT.
  SET PF-STATUS '9005'.
  SET TITLEBAR '9005'.

ENDMODULE.                 " STATUS_9005  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9005 INPUT.
  SAVE_OK_CODE = OK_CODE .
  CASE SAVE_OK_CODE.
    WHEN 'ENTE' OR 'CANC'.
      LEAVE TO SCREEN 0 .
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9005  INPUT
