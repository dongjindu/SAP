************************************************************************
* Program Name      : ZMMR61800T
* Author            :
* Creation Date     : 2011.05.20.
* Description       : New BOL invoice creation process
* Date        Developer      Request      Description
* 05/23/2011  PAUL           UD1K951759   Create
************************************************************************
REPORT ZMMR61800T MESSAGE-ID ZIM
                  NO STANDARD PAGE HEADING
                  LINE-SIZE 400.

INCLUDE ZMMR61800TOP.
INCLUDE ZRIMFTZCLASS.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_DATUM    FOR ZTMM_FTZ_INV_HD-ZEDAT,
                s_ZHBLNO   FOR ZTMM_FTZ_INV_HD-ZFCIVNO.

PARAMETERS: P_STATUS LIKE ZTMM_FTZ_INV_HD-ZRESULT.

SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.

START-OF-SELECTION.

  PERFORM P1000_GET_DATA.

  IF IT_ZTBLHD_LOG IS INITIAL.
    MESSAGE S977  WITH 'There is no data!'.
    EXIT.
  ENDIF.
  PERFORM P1000_DISPLAY_LOG.    "Display Data Log

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_DATA
*&---------------------------------------------------------------------*
FORM P1000_GET_DATA.

  ">> 1. Standard Data Select.
  CLEAR: IT_ZTBLHD_LOG.
  REFRESH : IT_ZTBLHD_LOG.

  IF NOT   P_STATUS IS INITIAL.                             "UD1K930445
     SELECT DISTINCT
            A~ZFCIVRN
            A~BUKRS
            A~ZFCIVNO
            A~ZFHBLNO
            A~ZFMBLNO
            A~ZFCIDT
            A~INCO1
            A~ZEKRW
            A~ZFMAVN
            A~ZFOPBN
            A~ZEIVAMT
            A~ZFCAMT
            A~ZFIAMT
            A~ZSDAT
            A~ZSTIM
            A~ZMODE
            A~ZEDAT
            A~ZETIM
            A~ZRESULT
            A~ZZRET
            A~ZZMSG

       INTO CORRESPONDING FIELDS OF TABLE IT_ZTBLHD_LOG

       FROM ZTMM_FTZ_INV_HD       AS A
*      INNER JOIN ZTMM_FTZ_INV_ITM AS B
*              ON A~ZFCIVRN = B~ZFCIVRN
      WHERE A~ZEDAT        IN   S_DATUM AND
            A~ZFCIVNO      IN   S_ZHBLNO AND              "UD1K930445
            A~ZRESULT      EQ P_STATUS. "  and            "UD1K930445
*              a~ZEDAT        in ( Select max( ZEDAT )
*                                from ZTMM_FTZ_INV_HD
*                               where ZFCIVNO = a~ZFCIVNO  and
*                                     ZFHBLNO = a~ZFHBLNO
*                                     group by ZFCIVNO ZFHBLNO   )
*              order by a~ZEDAT a~ZETIM descending .
  ELSE.                                                     "UD1K930445
     SELECT DISTINCT
            A~ZFCIVRN
            A~BUKRS
            A~ZFCIVNO
            A~ZFHBLNO
            A~ZFMBLNO
            A~ZFCIDT
            A~INCO1
            A~ZEKRW
            A~ZFMAVN
            A~ZFOPBN
            A~ZEIVAMT
            A~ZFCAMT
            A~ZFIAMT
            A~ZSDAT
            A~ZSTIM
            A~ZMODE
            A~ZEDAT
            A~ZETIM
            A~ZRESULT
            A~ZZRET
            A~ZZMSG

      INTO CORRESPONDING FIELDS OF TABLE IT_ZTBLHD_LOG
      FROM ZTMM_FTZ_INV_HD AS A
*     INNER JOIN ZTMM_FTZ_INV_ITM AS B
*             ON A~ZFCIVRN = B~ZFCIVRN

      WHERE  A~ZEDAT        IN   S_DATUM AND
             A~ZFCIVNO      IN   S_ZHBLNO .                 "UD1K930445

  ENDIF.                                                    "UD1K930445

  SORT IT_ZTBLHD_LOG BY ZEDAT ZETIM.                        "HIS20094

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTBLHD_LOG
*  FROM   ZTMM_FTZ_INV_HD
*  WHERE  ZEDAT        IN   S_DATUM.

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
    W_TITLE = 'Display Data(B/L) Processing Log'.
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
    WA_LAYOUT-SEL_MODE   = 'D'.

* Set column header
    PERFORM MASK_COLUMNS TABLES IT_FIELDCAT.

* Show ALV Control
    CALL METHOD CRV_ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_STRUCTURE_NAME              = 'ZTMM_FTZ_INV_HD'
        IS_LAYOUT                     = WA_LAYOUT   "Title
      CHANGING
        IT_OUTTAB                     = IT_ZTBLHD_LOG
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
            I_STRUCTURE_NAME = 'ZTMM_FTZ_INV_HD'
       CHANGING
            CT_FIELDCAT      = P_IT_FIELDCAT[].

* Make Column header
  LOOP AT P_IT_FIELDCAT.
    IF P_IT_FIELDCAT-FIELDNAME = 'ZFHBLNO'.
      P_IT_FIELDCAT-COLTEXT = 'House B/L'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFCIVNO'.
      P_IT_FIELDCAT-COLTEXT = 'Invoice'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFMBLNO'.
      P_IT_FIELDCAT-COLTEXT = 'Master B/L'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFBLDT'.
      P_IT_FIELDCAT-COLTEXT = 'Issuing Date'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFSHTY'.
      P_IT_FIELDCAT-COLTEXT = 'Shipping Type'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFVIA'.
      P_IT_FIELDCAT-COLTEXT = 'Via'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFVSL'.
      P_IT_FIELDCAT-COLTEXT = 'Vessel Name'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZF20FT'.
      P_IT_FIELDCAT-COLTEXT = '20FT'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZF40FT'.
      P_IT_FIELDCAT-COLTEXT = '40FT'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZF45FT'.
      P_IT_FIELDCAT-COLTEXT = '45FT'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZF40HQ'.
      P_IT_FIELDCAT-COLTEXT = '40FT HQ'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFNEWT'.
      P_IT_FIELDCAT-COLTEXT = 'Interface Net Weight'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFNEWTM'.
      P_IT_FIELDCAT-COLTEXT = 'Net Unit'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFTOVL'.
      P_IT_FIELDCAT-COLTEXT = 'Interface Total Volumn'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFTOVLM'.
      P_IT_FIELDCAT-COLTEXT = 'Volume Unit'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFETD'.
      P_IT_FIELDCAT-COLTEXT = 'ETD'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFETA'.
      P_IT_FIELDCAT-COLTEXT = 'ETA'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFSPRT'.
      P_IT_FIELDCAT-COLTEXT = 'Shipping Port'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZFAPRT'.
      P_IT_FIELDCAT-COLTEXT = 'Arriving Port'.
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

      READ TABLE IT_ZTBLHD_LOG INDEX WS_ROW INTO WA_ZTBLHD_LOG.
      ">> B/L Display.
      SET PARAMETER ID 'ZPHBLNO'   FIELD WA_ZTBLHD_LOG-ZFHBLNO.
      SET PARAMETER ID 'ZPBLNO'    FIELD ''.
      CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
    WHEN 'START'.
      CALL METHOD CRV_ALV_GRID->GET_CURRENT_CELL
           IMPORTING E_ROW     =  WS_ROW
                     E_VALUE   =  WS_VALUE
                     E_COL     =  WS_COL
                     ES_ROW_ID =  IT_ROW_ID
                     ES_COL_ID =  IT_COL_ID
                     ES_ROW_NO =  IT_ROW_NO.

      READ TABLE IT_ZTBLHD_LOG INDEX WS_ROW INTO WA_ZTBLHD_LOG.
      ">> Function Call.
      CALL FUNCTION 'ZIM_WEB_TO_SAP_BL_TEST'
           EXPORTING
                ZFDOCNO   = WA_ZTBLHD_LOG-ZFCIVRN
           TABLES
                IT_ZSBLHD = IT_ZSBLHD
                IT_ZSBLIT = IT_ZSBLIT.
      IF SY-SUBRC EQ 0.
        MESSAGE S977  WITH 'Restart B/L Interface Log Data'.
      ENDIF.
      PERFORM P1000_GET_DATA.             "Refresh Data Log "HIS20094
  ENDCASE.

ENDMODULE.
