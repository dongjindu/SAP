************************************************************************
* Program Name      : ZRIM_FTZ_TO_SAP
* Author            : Hyunju Na
* Creation Date     : 2004.05.06.
* Description       : FTZ(Entry Summary) -> SAP(Import)
*
************************************************************************
REPORT ZRIM_FTZ_TO_SAP MESSAGE-ID ZIM
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE ZRIMSAPTOP.
INCLUDE ZRIMFTZCLASS.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_DATUM    FOR SY-DATUM.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.

START-OF-SELECTION.

  PERFORM P1000_GET_DATA.

  IF IT_ZTFTZHD_LOG IS INITIAL.
     MESSAGE S977  WITH 'There is no data!'.
     EXIT.
  ENDIF.
  PERFORM P1000_DISPLAY_LOG.    "Display Data Log

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_DATA
*&---------------------------------------------------------------------*
FORM P1000_GET_DATA.

  ">> 1. Standard Data Select.
  CLEAR: IT_ZTFTZHD_LOG.
  REFRESH : IT_ZTFTZHD_LOG.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTFTZHD_LOG
  FROM   ZTFTZHD_LOG
  WHERE  ZEDAT        IN   S_DATUM.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P1000_DISPLAY_LOG
*&---------------------------------------------------------------------*
FORM P1000_DISPLAY_LOG.

  CALL SCREEN 0100.

ENDFORM.                    " P1000_DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS OUTPUT.

  IF W_TITLE IS INITIAL.
    W_TITLE = 'Display Data(Entry Summary) Processing Log'.
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
        I_STRUCTURE_NAME              = 'ZTFTZHD_LOG'
        IS_LAYOUT                     = WA_LAYOUT   "Title
      CHANGING
        IT_OUTTAB                     = IT_ZTFTZHD_LOG
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
            I_STRUCTURE_NAME = 'ZTFTZHD_LOG'
       CHANGING
            CT_FIELDCAT      = P_IT_FIELDCAT[].

* Make Column header
  LOOP AT P_IT_FIELDCAT.
    IF P_IT_FIELDCAT-FIELDNAME = 'ZFENTNO'.
      P_IT_FIELDCAT-COLTEXT = 'Entry Number'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFENTP'.
      P_IT_FIELDCAT-COLTEXT = 'Entry Type'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFEDT'.
      P_IT_FIELDCAT-COLTEXT = 'Entry Date'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFENPCD'.
      P_IT_FIELDCAT-COLTEXT = 'Port code of entry(Customs)'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFENDT'.
      P_IT_FIELDCAT-COLTEXT = 'Date of arrival in port'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFTOWT'.
      P_IT_FIELDCAT-NO_OUT = 'X'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFTOWTM'.
      P_IT_FIELDCAT-NO_OUT = 'X'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFBNAR'.
      P_IT_FIELDCAT-COLTEXT = 'Location of Goods'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFIVAMK'.
      P_IT_FIELDCAT-COLTEXT = 'Invoice amount'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFUSD'.
      P_IT_FIELDCAT-NO_OUT = 'X'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFDUTY'.
      P_IT_FIELDCAT-COLTEXT = 'Duty'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFMPF'.
      P_IT_FIELDCAT-COLTEXT = 'MPF'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFHMF'.
      P_IT_FIELDCAT-COLTEXT = 'HMF'.
    ENDIF.
    MODIFY P_IT_FIELDCAT.
  ENDLOOP.
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

  CASE SAVE_OK_CODE.
     WHEN 'BACK'.
        IF SY-DYNNR EQ '0100'.
           SET SCREEN 0.
        ENDIF.
     WHEN 'DISP'.
        CALL METHOD CRV_ALV_GRID->GET_CURRENT_CELL
             IMPORTING E_ROW     =  WS_ROW
                       E_VALUE   =  WS_VALUE
                       E_COL     =  WS_COL
                       ES_ROW_ID =  IT_ROW_ID
                       ES_COL_ID =  IT_COL_ID
                       ES_ROW_NO =  IT_ROW_NO.

        READ TABLE IT_ZTFTZHD_LOG INDEX WS_ROW INTO WA_ZTFTZHD_LOG.
        ">> B/L Display.
        SET PARAMETER ID 'ZPENTNO'   FIELD WA_ZTFTZHD_LOG-ZFENTNO.
        SET PARAMETER ID 'ZPIVNO'    FIELD ''.
        CALL TRANSACTION 'ZIMCC3' AND SKIP  FIRST SCREEN.
     WHEN 'START'.
        CALL METHOD CRV_ALV_GRID->GET_CURRENT_CELL
             IMPORTING E_ROW     =  WS_ROW
                       E_VALUE   =  WS_VALUE
                       E_COL     =  WS_COL
                       ES_ROW_ID =  IT_ROW_ID
                       ES_COL_ID =  IT_COL_ID
                       ES_ROW_NO =  IT_ROW_NO.

        READ TABLE IT_ZTFTZHD_LOG INDEX WS_ROW INTO WA_ZTFTZHD_LOG.
        ">> Function Call.
*        CALL FUNCTION 'ZIM_FTZ_TO_SAP_TEST'
*          EXPORTING
*            ZFDOCNO         =  WA_ZTFTZHD_LOG-ZFDOCNO
*          TABLES
*            IT_ZSFTZHD       =  IT_ZSFTZHD
*            IT_ZSFTZHS       =  IT_ZSFTZHS
*            IT_ZSFTZMT       =  IT_ZSFTZMT.

        CALL FUNCTION 'ZIM_FTZ_TO_SAP_NEW'
          EXPORTING
            ZFDOCNO         =  WA_ZTFTZHD_LOG-ZFDOCNO
          TABLES
            IT_ZSFTZHD       =  IT_ZSFTZHD
*            IT_ZSFTZHS       =  IT_ZSFTZHS
            IT_ZSFTZMT       =  IT_ZSFTZMT.
        IF SY-SUBRC EQ 0.
           MESSAGE S977  WITH
                  'Restart Entry Summary Interface Log Data'.
        ENDIF.
  ENDCASE.

ENDMODULE.                 " BACK  INPUT
