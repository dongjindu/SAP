************************************************************************
* Program Name      : MID Upload
* Author            : Hyunju Na
* Creation Date     : 2004.06.09.
* Description       : MID(Local File) Upload to Import System
*
************************************************************************
REPORT ZRIMMIDUP1 MESSAGE-ID ZIM
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

*>> MID Management Table
TABLES :  ZTIMIMG24.

*-----------------------------------------------------------------------
* FTZ Interface Internal Table Define
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_ZTIMIMG24  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG24.
DATA: END OF IT_ZTIMIMG24.

  DATA: W_TABIX      LIKE SY-TABIX,
        NEW_LFIMG    LIKE  PLFH-MGVGW.

  DATA:  IT_FUNC TYPE STANDARD TABLE OF RSMPE-FUNC.
  DATA:  WA_FUNC LIKE LINE OF IT_FUNC.
  DATA:  W_TITLE(80).
  DATA:  CC_NAME TYPE SCRFNAME VALUE 'CC_0100'.

* For OK code
  DATA: OK_CODE TYPE SY-UCOMM,  SAVE_OK_CODE LIKE OK_CODE.

* For PF-STATUS and Titlebar
  CLASS LCL_PS DEFINITION DEFERRED.
  DATA: CRV_PS TYPE REF TO LCL_PS.
  DATA: CRV_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
        CRV_ALV_GRID         TYPE REF TO CL_GUI_ALV_GRID.

* Variables for ALV
  DATA: WA_LAYOUT   TYPE LVC_S_LAYO.
  DATA: IT_FIELDCAT TYPE LVC_T_FCAT WITH HEADER LINE.
  DATA: WA_TOOLBAR  TYPE STB_BUTTON.
  DATA: WA_SORT     TYPE LVC_S_SORT.
  DATA: IT_SORT     LIKE TABLE OF WA_SORT.
  DATA: IT_ROID TYPE LVC_T_ROID.
  DATA: WA_ROID LIKE LINE OF IT_ROID.
  DATA: IT_ROW TYPE LVC_T_ROW.
  DATA: WA_ROW LIKE LINE OF IT_ROW.

  DATA: BEGIN OF WA_ZTIMIMG24.
          INCLUDE STRUCTURE ZTIMIMG24.
  DATA:  "profl LIKE mara-profl,
        END OF WA_ZTIMIMG24.

  DATA: IT_ZTIMIMG24_OUT LIKE TABLE OF WA_ZTIMIMG24.

INCLUDE ZRIMMIDUPCLS.

*----------------------------------------------------------------------*
*  Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-B02.

   PARAMETERS: P_FILE LIKE RLGRAP-FILENAME
               DEFAULT 'C:\MID.TXT'.

SELECTION-SCREEN END OF BLOCK B2.

START-OF-SELECTION.

  PERFORM P1000_UPLOAD_FILE.

  IF IT_ZTIMIMG24 IS INITIAL.
     MESSAGE S977  WITH 'There is no data!'.
     EXIT.
  ENDIF.
  PERFORM P1000_DISPLAY_LOG.    "Display Data Log

*&---------------------------------------------------------------------*
*&      Form  P1000_DISPLAY_LOG
*&---------------------------------------------------------------------*
FORM P1000_DISPLAY_LOG.

  IT_ZTIMIMG24_OUT[]  =  IT_ZTIMIMG24[].
  CALL SCREEN 0100.

ENDFORM.                    " P1000_DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS OUTPUT.

  IF W_TITLE IS INITIAL.
    W_TITLE = 'Display Data Processing Log'.
  ENDIF.

  CREATE OBJECT CRV_PS
    EXPORTING IM_PS      = 'PS'                "PF-STATUS
              IM_IT_FUNC = IT_FUNC             "Excluding func
              IM_TB      = 'TB'                "TITLEBAR
              IM_TITLE   = W_TITLE.            "TITLE
  CLEAR IT_FUNC.

ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.

  CC_NAME = 'CC_0100'.
  IF CRV_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT CRV_CUSTOM_CONTAINER
      EXPORTING CONTAINER_NAME = CC_NAME.

    CREATE OBJECT CRV_ALV_GRID
      EXPORTING I_PARENT = CRV_CUSTOM_CONTAINER.

* Set a titlebar for the grid control
    WA_LAYOUT-GRID_TITLE = 'Display Data Processing Log'.

* Set column header
    PERFORM MASK_COLUMNS TABLES IT_FIELDCAT.

* Show ALV Control
    CALL METHOD CRV_ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_STRUCTURE_NAME              = 'ZTIMIMG24'
        IS_LAYOUT                     = WA_LAYOUT   "Title
      CHANGING
        IT_OUTTAB                     = IT_ZTIMIMG24_OUT
        IT_FIELDCATALOG               = IT_FIELDCAT[]
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CALL METHOD CRV_ALV_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        I_SOFT_REFRESH =  'X'
      EXCEPTIONS
        FINISHED       = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

ENDMODULE.                 " PBO_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  MASK_COLUMNS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MASK_COLUMNS TABLES   P_IT_FIELDCAT STRUCTURE IT_FIELDCAT.

* Build the fieldcat according to DDIC structure ztmm_6026_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = 'ZTIMIMG24'
       CHANGING
            CT_FIELDCAT      = P_IT_FIELDCAT[].

ENDFORM.                    " mask_columns
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.

  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  CASE SY-DYNNR.
    WHEN 0100.
      CASE SAVE_OK_CODE.
        WHEN 'EXIT'.
          LEAVE PROGRAM.
        WHEN 'CANC'.
          LEAVE PROGRAM.
      ENDCASE.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  BACK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BACK INPUT.

  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  CHECK SAVE_OK_CODE = 'BACK'.
  CASE SY-DYNNR.
    WHEN 0100.
      SET SCREEN 0.
  ENDCASE.

ENDMODULE.                 " BACK  INPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM P1000_UPLOAD_FILE.

  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      FILENAME = P_FILE
      FILETYPE = 'DAT'
    TABLES
      DATA_TAB = IT_ZTIMIMG24.

  IF SY-SUBRC NE 0.
     MESSAGE E463(ZIM1) WITH P_FILE.
  ENDIF.

  LOOP AT IT_ZTIMIMG24.
     W_TABIX  =  SY-TABIX.
     MOVE : SY-DATUM  TO  IT_ZTIMIMG24-CDAT,
            SY-UNAME  TO  IT_ZTIMIMG24-ERNAM.
     MODIFY  IT_ZTIMIMG24  INDEX  W_TABIX.
  ENDLOOP.

  MODIFY ZTIMIMG24   FROM TABLE   IT_ZTIMIMG24.
ENDFORM.                    " P1000_UPLOAD_FILE
