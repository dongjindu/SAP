*----------------------------------------------------------------------*
***INCLUDE MZAQM01_INSP_SCHEDI01 .
*----------------------------------------------------------------------*
*&----------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&----------------------------------------------------------------*
MODULE GET_CURSOR_FIELD INPUT.
  CLEAR: WA_FLDTXT, WA_CUR_LINE.
  GET CURSOR FIELD WA_FLDTXT LINE WA_CUR_LINE.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'DUMMY'. "/when Select Radio Button
      CHECK ST_DIST-ISIR = C_MARK.
      CLEAR ZSQM_INSP_SCH_HDR-IYEAR.

    WHEN OTHERS.
*--      Assign Inspection type(8910, 8920) by user Selection
      IF ST_DIST-REGU = C_MARK.
        ZSQM_INSP_SCH_HDR-ART = C_INSP_TYPE_REGULAR.
      ELSE.
        ZSQM_INSP_SCH_HDR-ART = C_INSP_TYPE_ISIR.
      ENDIF.

      CASE OK_CODE.
        WHEN 'CREATE'.
          WA_MODE = C_CREATE. "/Command Status Set to WA_MODE
          ZSQM_INSP_SCH_HDR-STATUS = '1'.

*--    Check Exist Data if user click ICON_CREATE on Application toolbar
          PERFORM CHECK_ENABLE_CREATE.
          REFRESH IT_ZSQM_INSP_SCH_ITEM_F.

          CALL SCREEN 0200.

        WHEN 'CHANGE'.
          WA_MODE = C_CHANGE.

          PERFORM READ_N_CHECK_ENABLE_DATA.
          PERFORM READ_DATA_FROM_DB_0300.
*--  Fill Material External Group code and Text
          PERFORM FILL_MATNR_EXTERNAL_MAT_GRP.

*--   Back up Retrieved data to internal tables for backup (.._B)
          PERFORM BACKUP_RETRIEVED_DATA.

          CALL SCREEN 0300.
        WHEN 'DISPLAY'.
          WA_MODE = C_DISPLAY.

          PERFORM READ_N_CHECK_ENABLE_DATA.
*          PERFORM READ_HEADER_DATA_FOR_DISPLAY.
          PERFORM READ_DATA_FROM_DB_0300.
*--  Fill Material External Group code and Text
          PERFORM FILL_MATNR_EXTERNAL_MAT_GRP.

*--   Back up Retrieved data to internal tables for backup (.._B)
          PERFORM BACKUP_RETRIEVED_DATA.

          CALL SCREEN 0300.
        WHEN OTHERS.

      ENDCASE.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ISIR_REGULAR_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_ISIR_REGULAR_0100 INPUT.
  CHECK NOT ( SY-UCOMM = 'DUMMY' OR SY-UCOMM IS INITIAL ).

  CASE C_MARK.
    WHEN ST_DIST-REGU.
      IF ZSQM_INSP_SCH_HDR-VEHICLE IS INITIAL  OR
         ZSQM_INSP_SCH_HDR-IYEAR   IS INITIAL.

        MESSAGE E000(ZMPM) WITH 'Please Required Values'(E01).

      ENDIF.
    WHEN ST_DIST-ISIR.
      IF ZSQM_INSP_SCH_HDR-VEHICLE IS INITIAL.
        MESSAGE E000(ZMPM) WITH TEXT-E01.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " CHECK_ISIR_REGULAR_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  IF SY-DYNNR NE '0100'.
    CLEAR WA_ANSWER.
    PERFORM POPUP_TO_CONFIRM_LOSS_OF_DATA  USING WA_ANSWER.
    IF WA_ANSWER = 'N'. STOP. ENDIF.
  ENDIF.

  CASE OK_CODE.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'RW'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
      CLEAR WA_ANSWER.
      PERFORM POPUP_TO_CONFIRM_LOSS_OF_DATA  USING WA_ANSWER.
      CHECK WA_ANSWER = 'J'.
      LEAVE TO SCREEN 0.

    WHEN 'UPLOAD'.  "/Upload Data from PC File
      PERFORM UPLOAD_DATA_FROM_EXCEL.
*-- Fill Inspection Purpose(KATALOGART, CODEGRUPPE, CODE)
*-- to Interna table
      PERFORM FILL_INSPECTION_PURPOSE_COD.

      PERFORM DISPLAY_PROGRESS_INDICATOR  USING 70
                                          'Converting Excel Data'.

*--  Fill Material External Group code and Text
      PERFORM FILL_MATNR_EXTERNAL_MAT_GRP.
      WA_STATUS = C_UPLOADED.

    WHEN 'SAVE'.  "/Save data to DB
      PERFORM SAVE_UPLOADING_DATA_0200.
      WA_STATUS = C_SAVED.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&-----------------------------------------------------------------*
*&      Module  GET_TEXT_FIELD_VALUE  INPUT
*&-----------------------------------------------------------------*
MODULE GET_TEXT_FIELD_VALUE INPUT.

  PERFORM GET_VEHICLE_NAME_AND_CHECK
                             USING  ZSQM_INSP_SCH_HDR-VEHICLE
                          CHANGING  ZSQM_INSP_SCH_HDR-VEHICLE_N.


ENDMODULE.                 " GET_TEXT_FIELD_VALUE  INPUT
*&-----------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_0200  INPUT
*&-----------------------------------------------------------------*
MODULE TABLE_CONTROL_INPUT_0200 INPUT.
*---MOVE data to Internal Table from TABLE CONTROL.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  CLEAR IT_ZSQM_INSP_SCH_ITEM_F.

  READ TABLE IT_ZSQM_INSP_SCH_ITEM_F INDEX <TC>-CURRENT_LINE.
  MOVE-CORRESPONDING : IT_ZSQM_INSP_SCH_ITEM_F TO WA_ITEM_F_B.

  MOVE-CORRESPONDING ZSQM_INSP_SCH_ITEM_F TO IT_ZSQM_INSP_SCH_ITEM_F.
  MOVE-CORRESPONDING WA_ITEM_F_B          TO IT_ZSQM_INSP_SCH_ITEM_F.

  MODIFY IT_ZSQM_INSP_SCH_ITEM_F INDEX <TC>-CURRENT_LINE.

**-- move Selected Table Control Index to Global Variable(WA_SEL_LINE)
*  IF ZSQM_INSP_SCH_ITEM_F-MARK = C_MARK.
*    WA_SEL_LINE = <TC>-CURRENT_LINE.
*  ENDIF.
*

ENDMODULE.                 " TABLE_CONTROL_INPUT_0200  INPUT

************************

*&-----------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_0300  INPUT
*&-----------------------------------------------------------------*
MODULE TABLE_CONTROL_INPUT_0300 INPUT.

  CHECK WA_MODE = C_CHANGE.

*---MOVE data to Internal Table from TABLE CONTROL.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  CLEAR IT_ZSQM_INSP_SCH_ITEM_F.

  READ TABLE IT_ZSQM_INSP_SCH_ITEM_F INDEX <TC>-CURRENT_LINE.
  MOVE-CORRESPONDING : IT_ZSQM_INSP_SCH_ITEM_F TO WA_ITEM_F_B.

  MOVE-CORRESPONDING ZSQM_INSP_SCH_ITEM_F TO IT_ZSQM_INSP_SCH_ITEM_F.
  MOVE-CORRESPONDING WA_ITEM_F_B          TO IT_ZSQM_INSP_SCH_ITEM_F.

  MODIFY IT_ZSQM_INSP_SCH_ITEM_F INDEX <TC>-CURRENT_LINE.

**-- move Selected Table Control Index to Global Variable(WA_SEL_LINE)
*  IF ZSQM_INSP_SCH_ITEM_F-MARK = C_MARK.
*    WA_SEL_LINE = <TC>-CURRENT_LINE.
*  ENDIF.
*

ENDMODULE.                 " TABLE_CONTROL_INPUT_0300  INPUT
*&-----------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&-----------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'BACK'.
      CLEAR WA_ANSWER.
      PERFORM POPUP_TO_CONFIRM_LOSS_OF_DATA  USING WA_ANSWER.
      IF WA_ANSWER = 'N'. STOP. ENDIF.
      LEAVE TO SCREEN 0.
    WHEN 'CHG_DISP'.
      CASE WA_MODE.
        WHEN C_CHANGE.
          WA_MODE = C_DISPLAY.
        WHEN C_DISPLAY.
          IF ZSQM_INSP_SCH_HDR-STATUS = C_RELEASE.
            MESSAGE W000(ZMQM) WITH TEXT-W01.
          ELSE.
            WA_MODE = C_CHANGE.
          ENDIF.
      ENDCASE.

    WHEN 'SAVE'.
      PERFORM PREPARE_FOR_SAVE_0300.
      PERFORM CHECK_CHAGNED_DATA_EXIT.

      PERFORM SAVE_CHANGED_DATA_0300.
      WA_STATUS = C_SAVED.

    WHEN 'CREATE_LOT'.
      CLEAR IT_ZSQM_INSP_SCH_ITEM_F.
      READ TABLE IT_ZSQM_INSP_SCH_ITEM_F WITH KEY MARK = C_MARK.
      CHECK SY-SUBRC = 0.
      IF IT_ZSQM_INSP_SCH_ITEM_F-STATUS NE C_CREATION.
        MESSAGE E000(ZMQM)
              WITH 'It Can not Create Inspection Lot'(E07).
        EXIT.
      ENDIF.

      PERFORM CREATE_INSPECTION_LOT.

    WHEN 'REL'. "/User Select Released Status Manually.
      WA_SEL_LINE = TC_0300-TOP_LINE + WA_CUR_LINE - 1.
      READ TABLE IT_ZSQM_INSP_SCH_ITEM_F INDEX WA_SEL_LINE.
      CHECK SY-SUBRC = 0.
      IF IT_ZSQM_INSP_SCH_ITEM_F-STATUS = C_RELEASE AND
         IT_ZSQM_INSP_SCH_ITEM_F-PRUEFLOS IS INITIAL AND
         IT_ZSQM_INSP_SCH_ITEM_F-PRUEFLOS IS INITIAL.

        IT_ZSQM_INSP_SCH_ITEM_F-STATUS = C_CREATION .
        MODIFY IT_ZSQM_INSP_SCH_ITEM_F INDEX WA_SEL_LINE.
        MESSAGE E000(ZMQM) WITH
          'It Can not manually Change Status to Release status!'(E09).
      ENDIF.

    WHEN 'EXCEL_DWN'.
      CHECK NOT IT_ZSQM_INSP_SCH_ITEM_F[] IS INITIAL.
      PERFORM CREATE_N_SET_EXCEL_OBJECT. "//Create and Set OBJECT
      PERFORM DOWNLOAD_TO_EXEL_NEW.      "//Download to EXCEL file
      SET PROPERTY OF H_EXCEL_OBJECT  'Visible' = 1.
      PERFORM FREE_DISCONNECT_TO_EXCEL. "/Disconnect Excel Object


    WHEN OTHERS.

      CASE OK_CODE.
        WHEN 'ADD_ROW'.
          CHECK WA_MODE = C_CHANGE.
          CLEAR IT_ZSQM_INSP_SCH_ITEM_F.

          IT_ZSQM_INSP_SCH_ITEM_F-STATUS = C_CREATION.
          MOVE : SY-DATUM TO IT_ZSQM_INSP_SCH_ITEM_F-ERDAT,
                 SY-UZEIT TO IT_ZSQM_INSP_SCH_ITEM_F-ERZET,
                 SY-UNAME TO IT_ZSQM_INSP_SCH_ITEM_F-ERNAM.

          APPEND IT_ZSQM_INSP_SCH_ITEM_F.

        WHEN 'CUT_SEL' OR 'DEL_ROW'. "/Delete Selected ROW.
          CHECK WA_MODE = C_CHANGE.
          PERFORM DELETE_SELECTED_ROW.

        WHEN 'SEL_ALL'.
          CLEAR IT_ZSQM_INSP_SCH_ITEM_F.
          MOVE C_MARK TO IT_ZSQM_INSP_SCH_ITEM_F-MARK.
          MODIFY IT_ZSQM_INSP_SCH_ITEM_F TRANSPORTING MARK
                      WHERE MATNR  NE ''
                        AND STATUS = C_CREATION.
        WHEN 'DESEL'.
          CLEAR IT_ZSQM_INSP_SCH_ITEM_F.
          MODIFY IT_ZSQM_INSP_SCH_ITEM_F TRANSPORTING MARK
                      WHERE MATNR NE ''.

        WHEN OTHERS.
      ENDCASE.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&------------------------------------------------------------------*
*&      Module  CHECK_STATUS_SET_STATUS_HDR  INPUT
*&------------------------------------------------------------------*
MODULE CHECK_STATUS_SET_STATUS_HDR INPUT.
  CHECK WA_MODE = C_CHANGE.

  READ TABLE IT_ZSQM_INSP_SCH_ITEM_F WITH KEY STATUS = C_CREATION.
  CASE SY-SUBRC.
    WHEN 0.
      ZSQM_INSP_SCH_HDR-STATUS = C_CREATION.
    WHEN OTHERS.
      READ TABLE IT_ZSQM_INSP_SCH_ITEM_F WITH KEY STATUS = C_RELEASE.
      IF SY-SUBRC = 0.
        ZSQM_INSP_SCH_HDR-STATUS = C_RELEASE.
      ELSE.
        ZSQM_INSP_SCH_HDR-STATUS = C_DONTUSE.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " CHECK_STATUS_SET_STATUS_HDR  INPUT
