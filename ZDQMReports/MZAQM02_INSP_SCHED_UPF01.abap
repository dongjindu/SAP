*----------------------------------------------------------------------*
***INCLUDE MZAQM01_INSP_SCHEDF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_ENABLE_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_ENABLE_CREATE.
*-- Existence Check
  SELECT SINGLE *
     FROM ZTQM_INSP_HDR
       WHERE VEHICLE = ZSQM_INSP_SCH_HDR-VEHICLE
         AND ART     = ZSQM_INSP_SCH_HDR-ART
         AND IYEAR   = ZSQM_INSP_SCH_HDR-IYEAR.

  CHECK SY-SUBRC = 0.
  MESSAGE E000(ZMQM) WITH ZSQM_INSP_SCH_HDR-VEHICLE
                         'is exist in DB, Can''t Create'(E02).


ENDFORM.                    " CHECK_ENABLE_CREATE
*&------------------------------------------------------------------*
*&      Form  GET_VEHICLE_NAME_AND_CHECK
*&------------------------------------------------------------------*
FORM GET_VEHICLE_NAME_AND_CHECK USING    P_VEHICLE    TYPE MATNR
                                CHANGING P_VEHICLE_N.
  CLEAR P_VEHICLE_N.

  SELECT SINGLE MAKTX     INTO P_VEHICLE_N
      FROM ZVQM_MATNR_VH
        WHERE VEHICLE = P_VEHICLE.

  CHECK SY-SUBRC NE 0.
  CHECK NOT ( SY-UCOMM = 'DUMMY' OR SY-UCOMM IS INITIAL ).
  MESSAGE E000(ZMPM) WITH P_VEHICLE ',Not Exist Vehicle Code.'(E03).

ENDFORM.                    " GET_VEHICLE_NAME_AND_CHECK
*&-------------------------------------------------------------------*
*&      Form  UPLOAD_DATA_FROM_EXCEL
*&-------------------------------------------------------------------*
FORM UPLOAD_DATA_FROM_EXCEL.
  DATA : LW_Q_TEXT(132) TYPE C.
  CONCATENATE TEXT-Q01
              ZSQM_INSP_SCH_HDR-VEHICLE_N
              '?'
           INTO LW_Q_TEXT   SEPARATED BY SPACE.

  CLEAR WA_ANSWER.

  PERFORM POP_UP_TO_CONFIRM    USING WA_ANSWER
                                     TEXT-QT1    "/Pop_up Tile
                                     LW_Q_TEXT.   "/Question

  CHECK WA_ANSWER = 'Y'.
  CLEAR WA_FILENAME.
  PERFORM GET_FILE_NAME   USING  WA_FILENAME.

  CHECK NOT WA_FILENAME IS INITIAL.

  PERFORM UPLOAD_DATA_FROM_FILE  TABLES   IT_ZSQM_INSP_SCH_EXCEL_FLAT
                                 USING    WA_FILENAME.


  CHECK NOT IT_ZSQM_INSP_SCH_EXCEL_FLAT[] IS INITIAL.

  LOOP AT IT_ZSQM_INSP_SCH_EXCEL_FLAT.
    CLEAR: IT_ZSQM_INSP_SCH_ITEM_F.
    MOVE-CORRESPONDING :IT_ZSQM_INSP_SCH_EXCEL_FLAT
                                  TO IT_ZSQM_INSP_SCH_ITEM_F.

    APPEND IT_ZSQM_INSP_SCH_ITEM_F.
  ENDLOOP.

ENDFORM.                    " UPLOAD_DATA_FROM_EXCEL
*&------------------------------------------------------------------*
*&      Form  POP_UP_TO_CONFIRM
*&------------------------------------------------------------------*
FORM POP_UP_TO_CONFIRM USING    P_ANSWER
                                P_TITLE
                                P_Q_TEXT.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
            TITLEBAR              = P_TITLE
            TEXT_QUESTION         = P_Q_TEXT
            TEXT_BUTTON_1         = 'Yes'
            ICON_BUTTON_1         = ' '
            TEXT_BUTTON_2         = 'No'
            ICON_BUTTON_2         = ' '
            DEFAULT_BUTTON        = '2'
            DISPLAY_CANCEL_BUTTON = ' '
       IMPORTING
            ANSWER                = P_ANSWER
       EXCEPTIONS
            TEXT_NOT_FOUND        = 1
            OTHERS                = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF P_ANSWER = '1'.
    P_ANSWER = 'Y'.
  ELSEIF P_ANSWER = '2'.
    P_ANSWER = 'N'.
  ENDIF.

ENDFORM.                    " POP_UP_TO_CONFIRM
*&-----------------------------------------------------------------*
*&      Form  GET_FILE_NAME
*&-----------------------------------------------------------------*
FORM GET_FILE_NAME USING    P_FILENAME.

  DATA : LW_REPID LIKE SY-REPID,
         LW_DYNNR LIKE SY-DYNNR.

  LW_REPID = SY-REPID.
  LW_DYNNR = SY-DYNNR.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
       EXPORTING
            PROGRAM_NAME  = LW_REPID
            DYNPRO_NUMBER = LW_DYNNR
            FIELD_NAME    = ' '
            STATIC        = ' '
            MASK          = '*.xls'
       CHANGING
            FILE_NAME     = P_FILENAME
       EXCEPTIONS
            MASK_TOO_LONG = 1
            OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " GET_FILE_NAME
*&------------------------------------------------------------------*
*&      Form  UPLOAD_DATA_FROM_FILE
*&------------------------------------------------------------------*
FORM UPLOAD_DATA_FROM_FILE
                     TABLES  PT_TABLE
                      USING  P_FILENAME LIKE RLGRAP-FILENAME.

  DATA : LT_ITAB TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
  DATA : LW_INDEX LIKE SY-TABIX.
  DATA : LW_START_COL TYPE I VALUE '1',
         LW_START_ROW TYPE I VALUE '1',
         LW_END_COL   TYPE I VALUE '256',
         LW_END_ROW   TYPE I VALUE '65536'.
  FIELD-SYMBOLS : <LW_FS>.
  DATA : LW_FIELD_TYPE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME                = P_FILENAME
            I_BEGIN_COL             = LW_START_COL
            I_BEGIN_ROW             = LW_START_ROW
            I_END_COL               = LW_END_COL
            I_END_ROW               = LW_END_ROW
       TABLES
            INTERN                  = LT_ITAB
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.


  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMQM) WITH  TEXT-E05. "'File Upload Failed !'.
    STOP.
  ENDIF.

  PERFORM DISPLAY_PROGRESS_INDICATOR  USING 50
                                            'Uploading'.

  CHECK NOT LT_ITAB[] IS INITIAL.

  SORT LT_ITAB BY ROW COL.

  REFRESH PT_TABLE.

*-- Transfer Data to Internal Table from Excel Input Tables
  DELETE LT_ITAB WHERE ROW <= 3. "/Delete Header line(1-3)

  LOOP AT LT_ITAB.
    MOVE : LT_ITAB-COL TO LW_INDEX.
    ASSIGN COMPONENT LW_INDEX OF STRUCTURE PT_TABLE TO <LW_FS>.
*    IF LT_ITAB-COL >= 6.
**      WRITE LT_ITAB-VALUE  TO <LW_FS> DDMMYY.
*    ELSE.

    DESCRIBE FIELD <LW_FS> TYPE LW_FIELD_TYPE.

    IF LW_FIELD_TYPE = 'D'.  "'MM/DD/YYYY"
      CONCATENATE LT_ITAB-VALUE+6(4)    "YEAR  (YYYY)
                  LT_ITAB-VALUE+0(2)    "MONTH (MM)
                  LT_ITAB-VALUE+3(2)    "DAY   (DD)
                              INTO <LW_FS>.
    ELSE.
      MOVE : LT_ITAB-VALUE TO <LW_FS>.
    ENDIF.
*    ENDIF.
    AT END OF ROW.
      APPEND PT_TABLE.
      CLEAR PT_TABLE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " UPLOAD_DATA_FROM_FILE
*&------------------------------------------------------------------*
*&      Form  DISPLAY_PROGRESS_INDICATOR
*&------------------------------------------------------------------*
FORM DISPLAY_PROGRESS_INDICATOR USING    VALUE(P_PERCENT)
                                         VALUE(P_TEXT).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = P_PERCENT
            TEXT       = P_TEXT.

ENDFORM.                    " DISPLAY_PROGRESS_INDICATOR
*&-----------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM_LOSS_OF_DATA
*&-----------------------------------------------------------------*
FORM POPUP_TO_CONFIRM_LOSS_OF_DATA USING    P_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
       EXPORTING
            TEXTLINE1     = 'Unsaved data will be lost!'(T51)
            TEXTLINE2     = 'Do you want continue? '(T52)
            TITEL         = 'Leave Current Processing '(T50)
            START_COLUMN  = 25
            START_ROW     = 6
            DEFAULTOPTION = 'N'
       IMPORTING
            ANSWER        = P_ANSWER.


ENDFORM.                    " POPUP_TO_CONFIRM_LOSS_OF_DATA
*&------------------------------------------------------------------*
*&      Form  SAVE_UPLOADING_DATA_0200
*&------------------------------------------------------------------*
FORM SAVE_UPLOADING_DATA_0200.

  PERFORM PREPARE_FOR_SAVE_0200.

  PERFORM SAVE_DATA_0200.


ENDFORM.                    " SAVE_UPLOADING_DATA_0200
*&------------------------------------------------------------------*
*&      Form  FILL_INSPECTION_PURPOSE_COD
*&------------------------------------------------------------------*
FORM FILL_INSPECTION_PURPOSE_COD.

*-- Fill Inspection Purpose(KATALOGART, CODEGRUPPE, CODE)
*-- to Interna table
*--  - ISIR    : KATALOGART = 'P',  CODEGRUPPE = 'IS'
*--  - REGULAR : KATALOGART = 'P',  CODEGRUPPE = 'RE',  CODE='01'

*  CONSTANTS : C_KATALOGART      TYPE QKATART   VALUE 'P',
*              C_CODEGRUPPE_ISIR TYPE QCODEGRP  VALUE 'IS',
*              C_CODEGRUPPE_REGU TYPE QCODEGRP  VALUE 'RE',
*              C_CODE_REGU       TYPE QCODE     VALUE '01'.

  CLEAR IT_ZSQM_INSP_SCH_ITEM_F.
  CASE C_MARK.
    WHEN ST_DIST-REGU.
      IT_ZSQM_INSP_SCH_ITEM_F-KATALOGART = C_KATALOGART.
      IT_ZSQM_INSP_SCH_ITEM_F-CODEGRUPPE = C_CODEGRUPPE_REGU.
      IT_ZSQM_INSP_SCH_ITEM_F-CODE       = C_CODE_REGU.

      MODIFY IT_ZSQM_INSP_SCH_ITEM_F
         TRANSPORTING  KATALOGART CODEGRUPPE CODE
           WHERE MATNR NE ''.

    WHEN ST_DIST-ISIR.
      IT_ZSQM_INSP_SCH_ITEM_F-KATALOGART = C_KATALOGART.
      IT_ZSQM_INSP_SCH_ITEM_F-CODEGRUPPE = C_CODEGRUPPE_ISIR.
*      IT_ZSQM_INSP_SCH_ITEM_F-CODE       = .
      MODIFY IT_ZSQM_INSP_SCH_ITEM_F
         TRANSPORTING  KATALOGART CODEGRUPPE
           WHERE MATNR NE ''.
  ENDCASE.


ENDFORM.                    " FILL_INSPECTION_PURPOSE_COD
*&-----------------------------------------------------------------*
*&      Form  PREPARE_FOR_SAVE_0200
*&-----------------------------------------------------------------*
FORM PREPARE_FOR_SAVE_0200.

*-- Move Header data to ZTQM_INSP_HDR.
  CLEAR ZTQM_INSP_HDR.
  MOVE-CORRESPONDING ZSQM_INSP_SCH_HDR TO ZTQM_INSP_HDR.

*IT_ZSQM_INSP_SCH_ITEM_F

*--Move raw data to IT_ZTQM_INSP_ITEM_F for Save
  REFRESH IT_ZTQM_INSP_ITEM_F.
  LOOP AT IT_ZSQM_INSP_SCH_ITEM_F.
    CLEAR IT_ZTQM_INSP_ITEM_F.
    MOVE-CORRESPONDING IT_ZSQM_INSP_SCH_ITEM_F TO IT_ZTQM_INSP_ITEM_F.
    APPEND IT_ZTQM_INSP_ITEM_F.
  ENDLOOP.

  CLEAR : IT_ZTQM_INSP_ITEM_F.
  MOVE : ZTQM_INSP_HDR-VEHICLE TO IT_ZTQM_INSP_ITEM_F-VEHICLE,
        ZTQM_INSP_HDR-IYEAR   TO IT_ZTQM_INSP_ITEM_F-IYEAR,
        ZTQM_INSP_HDR-ART   TO IT_ZTQM_INSP_ITEM_F-ART,
        ZTQM_INSP_HDR-STATUS  TO IT_ZTQM_INSP_ITEM_F-STATUS.

  MODIFY IT_ZTQM_INSP_ITEM_F TRANSPORTING VEHICLE IYEAR ART STATUS
                       WHERE MATNR NE ''.

*-- Copy IT_ZTQM_INSP_ITEM_F TO IT_ZTQM_INSP_ITEM.
  DATA : LW_FLD_CNT(4) TYPE N.
  DATA : LW_FLD_NAME(40) TYPE C.
  FIELD-SYMBOLS : <L_FS>.

  REFRESH : IT_ZTQM_INSP_ITEM, IT_ZTQM_INSP_S_ITEM.

  LOOP AT IT_ZTQM_INSP_ITEM_F.
    CLEAR IT_ZTQM_INSP_ITEM.
    MOVE-CORRESPONDING IT_ZTQM_INSP_ITEM_F TO IT_ZTQM_INSP_ITEM.

*--   Convert Flat type date data to item Data & item sub data.

    CLEAR LW_FLD_CNT.

    DO 12 TIMES.

      CLEAR IT_ZTQM_INSP_S_ITEM.
      MOVE-CORRESPONDING IT_ZTQM_INSP_ITEM_F TO IT_ZTQM_INSP_S_ITEM.

      LW_FLD_CNT = LW_FLD_CNT + 10.

*--      ISIR or REGULAR
      IF LW_FLD_CNT <= '0100'.
        CONCATENATE 'IT_ZTQM_INSP_ITEM_F-DATUV_' LW_FLD_CNT
                                                 INTO LW_FLD_NAME.
        ASSIGN (LW_FLD_NAME) TO <L_FS>.
        MOVE <L_FS> TO IT_ZTQM_INSP_S_ITEM-PRUEFDATUV.

        MOVE : ZTQM_INSP_HDR-ART   TO IT_ZTQM_INSP_S_ITEM-ART,
               C_VORGLFNR_ISIR_REG TO IT_ZTQM_INSP_S_ITEM-VORGLFNR,
               LW_FLD_CNT          TO IT_ZTQM_INSP_S_ITEM-MERKNR.

*--      MS Interior
      ELSEIF LW_FLD_CNT = '0110'.
        MOVE : 'IT_ZTQM_INSP_ITEM_F-DATUV_1010' TO LW_FLD_NAME.
        ASSIGN (LW_FLD_NAME) TO <L_FS>.
        MOVE <L_FS> TO IT_ZTQM_INSP_S_ITEM-PRUEFDATUV.

        MOVE : C_INSP_TYPE_MS  TO IT_ZTQM_INSP_S_ITEM-ART,   "/MS type
               C_VORGLFNR_MS_1 TO IT_ZTQM_INSP_S_ITEM-VORGLFNR,
               '0010'          TO IT_ZTQM_INSP_S_ITEM-MERKNR.

*--      MS Exterior
      ELSEIF LW_FLD_CNT = '0120'.
        MOVE : 'IT_ZTQM_INSP_ITEM_F-DATUV_2010' TO LW_FLD_NAME.
        ASSIGN (LW_FLD_NAME) TO <L_FS>.
        MOVE <L_FS> TO IT_ZTQM_INSP_S_ITEM-PRUEFDATUV.

        MOVE : C_INSP_TYPE_MS  TO IT_ZTQM_INSP_S_ITEM-ART,   "/MS type
               C_VORGLFNR_MS_2 TO IT_ZTQM_INSP_S_ITEM-VORGLFNR,
               '0010' TO IT_ZTQM_INSP_S_ITEM-MERKNR.

      ENDIF.

      APPEND IT_ZTQM_INSP_S_ITEM.

    ENDDO.

    APPEND IT_ZTQM_INSP_ITEM.

  ENDLOOP.

*-- FILL Header data : ISIR/REGULAR
  CLEAR : IT_ZTQM_INSP_ITEM, IT_ZTQM_INSP_S_ITEM.

  MOVE : ZTQM_INSP_HDR-VEHICLE TO IT_ZTQM_INSP_ITEM-VEHICLE,
         ZTQM_INSP_HDR-IYEAR   TO IT_ZTQM_INSP_ITEM-IYEAR,
         ZTQM_INSP_HDR-ART   TO IT_ZTQM_INSP_ITEM-ART,
         ZTQM_INSP_HDR-STATUS  TO IT_ZTQM_INSP_ITEM-STATUS.

  MODIFY IT_ZTQM_INSP_ITEM TRANSPORTING VEHICLE IYEAR ART STATUS
                                 WHERE MATNR NE ''.

  MOVE : ZTQM_INSP_HDR-VEHICLE TO IT_ZTQM_INSP_S_ITEM-VEHICLE,
         ZTQM_INSP_HDR-IYEAR   TO IT_ZTQM_INSP_S_ITEM-IYEAR,
         ZTQM_INSP_HDR-STATUS  TO IT_ZTQM_INSP_S_ITEM-STATUS.

  MODIFY IT_ZTQM_INSP_S_ITEM TRANSPORTING VEHICLE IYEAR STATUS
                                 WHERE MATNR NE ''.

*-- Fill Time Stamp data for Header / Flat Data.
  MOVE : SY-DATUM TO ZTQM_INSP_HDR-ERDAT,
         SY-UZEIT TO ZTQM_INSP_HDR-ERZET,
         SY-UNAME TO ZTQM_INSP_HDR-ERNAM.

  CLEAR : IT_ZTQM_INSP_ITEM_F.

  MOVE : SY-DATUM TO IT_ZTQM_INSP_ITEM_F-ERDAT,
         SY-UZEIT TO IT_ZTQM_INSP_ITEM_F-ERZET,
         SY-UNAME TO IT_ZTQM_INSP_ITEM_F-ERNAM.

  MODIFY  IT_ZTQM_INSP_ITEM_F TRANSPORTING ERDAT ERZET ERNAM
                 WHERE MATNR NE ''.

ENDFORM.                    " PREPARE_FOR_SAVE_0200
*&-----------------------------------------------------------------*
*&      Form  FILL_MATNR_EXTERNAL_MAT_GRP
*&-----------------------------------------------------------------*
FORM FILL_MATNR_EXTERNAL_MAT_GRP.
  DATA : LW_INDEX LIKE SY-TABIX.

  LOOP AT IT_ZSQM_INSP_SCH_ITEM_F.
    LW_INDEX = SY-TABIX.

    SELECT SINGLE A~EXTWG B~EWBEZ
      INTO (IT_ZSQM_INSP_SCH_ITEM_F-EXTWG,
            IT_ZSQM_INSP_SCH_ITEM_F-EWBEZ)
       FROM MARA AS A INNER JOIN TWEWT AS B
          ON A~EXTWG = B~EXTWG
         WHERE A~MATNR = IT_ZSQM_INSP_SCH_ITEM_F-MATNR.

    MODIFY IT_ZSQM_INSP_SCH_ITEM_F INDEX LW_INDEX.
  ENDLOOP.

ENDFORM.                    " FILL_MATNR_EXTERNAL_MAT_GRP
*&-----------------------------------------------------------------*
*&      Form  SAVE_DATA_0200
*&-----------------------------------------------------------------*
FORM SAVE_DATA_0200.

  PERFORM ENQUEUE_DB.

  INSERT ZTQM_INSP_HDR.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMQM)
          WITH 'Error founded Access DB'(E11) 'ZTQM_INSP_HDR'.
  ENDIF.

  INSERT ZTQM_INSP_ITEM FROM TABLE IT_ZTQM_INSP_ITEM.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMQM)
          WITH TEXT-E11 'ZTQM_INSP_ITEM'.
  ENDIF.

  INSERT ZTQM_INSP_ITEM_F FROM TABLE IT_ZTQM_INSP_ITEM_F.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMQM)
          WITH TEXT-E11 'ZTQM_INSP_ITEM_F'.
  ENDIF.

  INSERT ZTQM_INSP_S_ITEM FROM TABLE IT_ZTQM_INSP_S_ITEM.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMQM)
          WITH TEXT-E11 'ZTQM_INSP_S_ITEM'.
  ENDIF.

  CHECK SY-SUBRC = 0.
  COMMIT WORK.
  PERFORM DEQUEUE_DB.
  MESSAGE S000(ZMQM) WITH 'Successfully Saved!'(S01).

ENDFORM.                    " SAVE_DATA_0200
*&------------------------------------------------------------------*
*&      Form  ENQUEUE_DB
*&------------------------------------------------------------------*
FORM ENQUEUE_DB.

  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_INSP_HDR'
       EXPORTING
            MODE_ZTQM_INSP_HDR = 'X'
            MANDT              = SY-MANDT
            VEHICLE            = ZTQM_INSP_HDR-VEHICLE
            ART                = ZTQM_INSP_HDR-ART
            IYEAR              = ZTQM_INSP_HDR-IYEAR
       EXCEPTIONS
            FOREIGN_LOCK       = 1
            SYSTEM_FAILURE     = 2
            OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_INSP_IT1'
       EXPORTING
            MODE_ZTQM_INSP_ITEM = 'X'
            MANDT               = SY-MANDT
            VEHICLE             = ZTQM_INSP_HDR-VEHICLE
            ART                 = ZTQM_INSP_HDR-ART
            IYEAR               = ZTQM_INSP_HDR-IYEAR
*            MATNR               =
       EXCEPTIONS
            FOREIGN_LOCK        = 1
            SYSTEM_FAILURE      = 2
            OTHERS              = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_INSP_IT2'
       EXPORTING
            MODE_ZTQM_INSP_ITEM_F = 'X'
            MANDT                 = SY-MANDT
            VEHICLE               = ZTQM_INSP_HDR-VEHICLE
            ART                   = ZTQM_INSP_HDR-ART
            IYEAR                 = ZTQM_INSP_HDR-IYEAR
*            MATNR                 =
       EXCEPTIONS
            FOREIGN_LOCK          = 1
            SYSTEM_FAILURE        = 2
            OTHERS                = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_INSP_IT3'
       EXPORTING
            MODE_ZTQM_INSP_S_ITEM = 'X'
            MANDT                 = SY-MANDT
            VEHICLE               = ZTQM_INSP_HDR-VEHICLE
            ART                   = ZTQM_INSP_HDR-ART
            IYEAR                 = ZTQM_INSP_HDR-IYEAR
*            MATNR                 =
*            VORGLFNR              =
*            MERKNR                =
       EXCEPTIONS
            FOREIGN_LOCK          = 1
            SYSTEM_FAILURE        = 2
            OTHERS                = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " ENQUEUE_DB
*&------------------------------------------------------------------*
*&      Form  DEQUEUE_DB
*&------------------------------------------------------------------*
FORM DEQUEUE_DB.

  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_INSP_HDR'
       EXPORTING
            MODE_ZTQM_INSP_HDR = 'X'
            MANDT              = SY-MANDT
            VEHICLE            = ZTQM_INSP_HDR-VEHICLE
            ART                = ZTQM_INSP_HDR-ART
            IYEAR              = ZTQM_INSP_HDR-IYEAR.


  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_INSP_IT1'
       EXPORTING
            MODE_ZTQM_INSP_ITEM = 'X'
            MANDT               = SY-MANDT
            VEHICLE             = ZTQM_INSP_HDR-VEHICLE
            ART                 = ZTQM_INSP_HDR-ART
            IYEAR               = ZTQM_INSP_HDR-IYEAR.
*            MATNR               = ''.


  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_INSP_IT2'
       EXPORTING
            MODE_ZTQM_INSP_ITEM_F = 'X'
            MANDT                 = SY-MANDT
            VEHICLE               = ZTQM_INSP_HDR-VEHICLE
            ART                   = ZTQM_INSP_HDR-ART
            IYEAR                 = ZTQM_INSP_HDR-IYEAR.
*            MATNR                 = ''.


  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_INSP_IT3'
       EXPORTING
            MODE_ZTQM_INSP_S_ITEM = 'X'
            MANDT                 = SY-MANDT
            VEHICLE               = ZTQM_INSP_HDR-VEHICLE
            ART                   = ZTQM_INSP_HDR-ART
            IYEAR                 = ZTQM_INSP_HDR-IYEAR.
*            MATNR                 = ''
*            VORGLFNR              = ''
*            MERKNR                = ''.




ENDFORM.                    " DEQUEUE_DB
*&------------------------------------------------------------------*
*&      Form  READ_N_CHECK_ENABLE_DATA
*&------------------------------------------------------------------*
FORM READ_N_CHECK_ENABLE_DATA.

DATA : LWS_ZSQM_INSP_SCH_HDR LIKE ZSQM_INSP_SCH_HDR.
  CLEAR LWS_ZSQM_INSP_SCH_HDR.

*-- Get header Data and Check status field
  SELECT SINGLE *
     INTO CORRESPONDING FIELDS OF LWS_ZSQM_INSP_SCH_HDR
      FROM ZTQM_INSP_HDR
        WHERE VEHICLE = ZSQM_INSP_SCH_HDR-VEHICLE
          AND ART     = ZSQM_INSP_SCH_HDR-ART
          AND IYEAR   = ZSQM_INSP_SCH_HDR-IYEAR.


  IF SY-SUBRC NE 0.

    MESSAGE E000(ZMQM) WITH ZSQM_INSP_SCH_HDR-VEHICLE
                            TEXT-E04.

  ELSEIF SY-SUBRC = 0 AND
         ZSQM_INSP_SCH_HDR-STATUS = '2'. "/Released All Material
    IF OK_CODE = 'CHANGE'.
      MESSAGE E000(ZMQM) WITH ZSQM_INSP_SCH_HDR-VEHICLE
                              TEXT-E06.
    ENDIF.

    WA_MODE = C_DISPLAY.
  ELSEIF SY-SUBRC = 0 AND
         ZSQM_INSP_SCH_HDR-STATUS = '3'.
    MESSAGE I000(ZMQM) WITH ZSQM_INSP_SCH_HDR-VEHICLE
                            TEXT-I01.
    WA_MODE = C_DISPLAY.
  ENDIF.

  MOVE-CORRESPONDING LWS_ZSQM_INSP_SCH_HDR TO ZSQM_INSP_SCH_HDR.

ENDFORM.                    " READ_N_CHECK_ENABLE_DATA
*&-----------------------------------------------------------------*
*&      Form  READ_DATA_FROM_DB_0300
*&-----------------------------------------------------------------*
FORM READ_DATA_FROM_DB_0300.
  REFRESH: IT_ZTQM_INSP_ITEM, IT_ZSQM_INSP_SCH_ITEM_F,
           IT_ZTQM_INSP_ITEM_F,  IT_ZTQM_INSP_S_ITEM.
  CLEAR :  IT_ZTQM_INSP_ITEM, IT_ZSQM_INSP_SCH_ITEM_F,
           IT_ZTQM_INSP_ITEM_F,  IT_ZTQM_INSP_S_ITEM.

*-- Get Item Data (Material)
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_ZTQM_INSP_ITEM
      FROM ZTQM_INSP_ITEM
        WHERE VEHICLE = ZSQM_INSP_SCH_HDR-VEHICLE
          AND ART     = ZSQM_INSP_SCH_HDR-ART
          AND IYEAR   = ZSQM_INSP_SCH_HDR-IYEAR.

*-- Get sub item data (Material, MIC)
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_ZTQM_INSP_S_ITEM
      FROM ZTQM_INSP_S_ITEM
        WHERE VEHICLE = ZSQM_INSP_SCH_HDR-VEHICLE
          AND ART     = ZSQM_INSP_SCH_HDR-ART
          AND IYEAR   = ZSQM_INSP_SCH_HDR-IYEAR.

*-- Get Item Data (Material) : Flat Type
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_INSP_SCH_ITEM_F
      FROM ZTQM_INSP_ITEM_F
        WHERE VEHICLE = ZSQM_INSP_SCH_HDR-VEHICLE
          AND ART     = ZSQM_INSP_SCH_HDR-ART
          AND IYEAR   = ZSQM_INSP_SCH_HDR-IYEAR.

*--    BACKUP item data(flat)
  LOOP AT IT_ZSQM_INSP_SCH_ITEM_F.
    CLEAR : IT_ZTQM_INSP_ITEM_F.
    MOVE-CORRESPONDING IT_ZSQM_INSP_SCH_ITEM_F
                    TO IT_ZTQM_INSP_ITEM_F.
    APPEND IT_ZTQM_INSP_ITEM_F.
  ENDLOOP.


ENDFORM.                    " READ_DATA_FROM_DB_0300
*&-----------------------------------------------------------------*
*&      Form  READ_HEADER_DATA_FOR_DISPLAY
*&-----------------------------------------------------------------*
FORM READ_HEADER_DATA_FOR_DISPLAY.
*-- Get header Data and Check status field
  SELECT SINGLE *
     INTO CORRESPONDING FIELDS OF ZSQM_INSP_SCH_HDR
      FROM ZTQM_INSP_HDR
        WHERE VEHICLE = ZSQM_INSP_SCH_HDR-VEHICLE
          AND ART     = ZSQM_INSP_SCH_HDR-ART
          AND IYEAR   = ZSQM_INSP_SCH_HDR-IYEAR.

  CHECK SY-SUBRC NE 0.
  MESSAGE E000(ZMQM) WITH ZSQM_INSP_SCH_HDR-VEHICLE
                          TEXT-E04.
ENDFORM.                    " READ_HEADER_DATA_FOR_DISPLAY
*&-----------------------------------------------------------------*
*&      Form  SAVE_CHANGED_DATA_0300
*&-----------------------------------------------------------------*
FORM SAVE_CHANGED_DATA_0300.


  PERFORM SAVE_DATA_0300.

ENDFORM.                    " SAVE_CHANGED_DATA_0300
*&------------------------------------------------------------------*
*&      Form  PREPARE_FOR_SAVE_0300
*&------------------------------------------------------------------*
FORM PREPARE_FOR_SAVE_0300.
*-- Move Header data to ZTQM_INSP_HDR.
  CLEAR ZTQM_INSP_HDR.
  MOVE-CORRESPONDING ZSQM_INSP_SCH_HDR TO ZTQM_INSP_HDR.

*IT_ZSQM_INSP_SCH_ITEM_F

*--Move raw data to IT_ZTQM_INSP_ITEM_F for Save
  REFRESH IT_ZTQM_INSP_ITEM_F.
  LOOP AT IT_ZSQM_INSP_SCH_ITEM_F.
    CLEAR IT_ZTQM_INSP_ITEM_F.
    MOVE-CORRESPONDING IT_ZSQM_INSP_SCH_ITEM_F TO IT_ZTQM_INSP_ITEM_F.
    APPEND IT_ZTQM_INSP_ITEM_F.
  ENDLOOP.

  CLEAR : IT_ZTQM_INSP_ITEM_F.
  MOVE : ZTQM_INSP_HDR-VEHICLE TO IT_ZTQM_INSP_ITEM_F-VEHICLE,
        ZTQM_INSP_HDR-IYEAR   TO IT_ZTQM_INSP_ITEM_F-IYEAR,
        ZTQM_INSP_HDR-ART   TO IT_ZTQM_INSP_ITEM_F-ART.
*        ZTQM_INSP_HDR-STATUS  TO IT_ZTQM_INSP_ITEM_F-STATUS.

  MODIFY IT_ZTQM_INSP_ITEM_F TRANSPORTING VEHICLE IYEAR ART  "STATUS
                       WHERE MATNR NE ''.

*-- Copy IT_ZTQM_INSP_ITEM_F TO IT_ZTQM_INSP_ITEM.
  DATA : LW_FLD_CNT(4) TYPE N.
  DATA : LW_FLD_NAME_V(40) TYPE C,  "/Start Date Field Name
         LW_FLD_NAME_B(40) TYPE C.  "/End Date Field Name
  FIELD-SYMBOLS : <L_FS_V>,  "/Start Date Field Symbol
                  <L_FS_B>.  "/End Date Field Symbol

  REFRESH : IT_ZTQM_INSP_ITEM, IT_ZTQM_INSP_S_ITEM.

  LOOP AT IT_ZTQM_INSP_ITEM_F.
    CLEAR IT_ZTQM_INSP_ITEM.
    MOVE-CORRESPONDING IT_ZTQM_INSP_ITEM_F TO IT_ZTQM_INSP_ITEM.

*--   Convert Flat type date data to item Data & item sub data.

    CLEAR LW_FLD_CNT.

    DO 12 TIMES.

      CLEAR IT_ZTQM_INSP_S_ITEM.
      MOVE-CORRESPONDING IT_ZTQM_INSP_ITEM_F TO IT_ZTQM_INSP_S_ITEM.

      LW_FLD_CNT = LW_FLD_CNT + 10.

*--      ISIR or REGULAR
      IF LW_FLD_CNT <= '0100'.
        CONCATENATE 'IT_ZTQM_INSP_ITEM_F-DATUV_' LW_FLD_CNT
                                                 INTO LW_FLD_NAME_V.
        CONCATENATE 'IT_ZTQM_INSP_ITEM_F-DATUB_' LW_FLD_CNT
                                                 INTO LW_FLD_NAME_B.
        ASSIGN : (LW_FLD_NAME_V) TO <L_FS_V>,
                 (LW_FLD_NAME_B) TO <L_FS_B>.
        MOVE : <L_FS_V> TO IT_ZTQM_INSP_S_ITEM-PRUEFDATUV,
               <L_FS_B> TO IT_ZTQM_INSP_S_ITEM-PRUEFDATUB.

        MOVE : ZTQM_INSP_HDR-ART   TO IT_ZTQM_INSP_S_ITEM-ART,
               C_VORGLFNR_ISIR_REG TO IT_ZTQM_INSP_S_ITEM-VORGLFNR,
               LW_FLD_CNT          TO IT_ZTQM_INSP_S_ITEM-MERKNR.

*                       "/Inspection Lot No(ISIR/REGULAR)
        MOVE : IT_ZTQM_INSP_ITEM_F-PRUEFLOS
                                  TO IT_ZTQM_INSP_S_ITEM-PRUEFLOS.

*--      MS Interior
      ELSEIF LW_FLD_CNT = '0110'.
        MOVE : 'IT_ZTQM_INSP_ITEM_F-DATUV_1010' TO LW_FLD_NAME_V,
               'IT_ZTQM_INSP_ITEM_F-DATUB_1010' TO LW_FLD_NAME_B.

        ASSIGN : (LW_FLD_NAME_V) TO <L_FS_V>,
                 (LW_FLD_NAME_B) TO <L_FS_B>.
        MOVE : <L_FS_V> TO IT_ZTQM_INSP_S_ITEM-PRUEFDATUV,
               <L_FS_B> TO IT_ZTQM_INSP_S_ITEM-PRUEFDATUB.

        MOVE : C_INSP_TYPE_MS  TO IT_ZTQM_INSP_S_ITEM-ART,   "/MS type
               C_VORGLFNR_MS_1 TO IT_ZTQM_INSP_S_ITEM-VORGLFNR,
               '0010'          TO IT_ZTQM_INSP_S_ITEM-MERKNR.
*                       "/Inspection Lot No(MS)
        MOVE : IT_ZTQM_INSP_ITEM_F-PRUEFLOS_MS  "/Inspection Lot No(MS)
                                  TO IT_ZTQM_INSP_S_ITEM-PRUEFLOS.

*--      MS Exterior
      ELSEIF LW_FLD_CNT = '0120'.
        MOVE : 'IT_ZTQM_INSP_ITEM_F-DATUV_2010' TO LW_FLD_NAME_V,
               'IT_ZTQM_INSP_ITEM_F-DATUB_2010' TO LW_FLD_NAME_B.

        ASSIGN : (LW_FLD_NAME_V) TO <L_FS_V>,
                 (LW_FLD_NAME_B) TO <L_FS_B>.
        MOVE : <L_FS_V> TO IT_ZTQM_INSP_S_ITEM-PRUEFDATUV,
               <L_FS_B> TO IT_ZTQM_INSP_S_ITEM-PRUEFDATUB.

        MOVE : C_INSP_TYPE_MS  TO IT_ZTQM_INSP_S_ITEM-ART,   "/MS type
               C_VORGLFNR_MS_2 TO IT_ZTQM_INSP_S_ITEM-VORGLFNR,
               '0010' TO IT_ZTQM_INSP_S_ITEM-MERKNR.
*                       "/Inspection Lot No(MS)
        MOVE : IT_ZTQM_INSP_ITEM_F-PRUEFLOS_MS   "/Inspection Lot No(MS)
                                  TO IT_ZTQM_INSP_S_ITEM-PRUEFLOS.

      ENDIF.

      APPEND IT_ZTQM_INSP_S_ITEM.

    ENDDO.

    APPEND IT_ZTQM_INSP_ITEM.

  ENDLOOP.

*-- FILL Header data : ISIR/REGULAR
  CLEAR : IT_ZTQM_INSP_ITEM, IT_ZTQM_INSP_S_ITEM.

  MOVE : ZTQM_INSP_HDR-VEHICLE TO IT_ZTQM_INSP_ITEM-VEHICLE,
         ZTQM_INSP_HDR-IYEAR   TO IT_ZTQM_INSP_ITEM-IYEAR,
         ZTQM_INSP_HDR-ART   TO IT_ZTQM_INSP_ITEM-ART.

  MODIFY IT_ZTQM_INSP_ITEM TRANSPORTING VEHICLE IYEAR ART
                                 WHERE MATNR NE ''.

  MOVE : ZTQM_INSP_HDR-VEHICLE TO IT_ZTQM_INSP_S_ITEM-VEHICLE,
         ZTQM_INSP_HDR-IYEAR   TO IT_ZTQM_INSP_S_ITEM-IYEAR.


  MODIFY IT_ZTQM_INSP_S_ITEM TRANSPORTING VEHICLE IYEAR
                                 WHERE MATNR NE ''.

ENDFORM.                    " PREPARE_FOR_SAVE_0300
*&-----------------------------------------------------------------*
*&      Form  SAVE_DATA_0300
*&-----------------------------------------------------------------*
FORM SAVE_DATA_0300.
  PERFORM ENQUEUE_DB.

  UPDATE ZTQM_INSP_HDR.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMQM)
          WITH 'Error founded Access DB'(E11) 'ZTQM_INSP_HDR'.
  ENDIF.

  MODIFY ZTQM_INSP_ITEM FROM TABLE IT_ZTQM_INSP_ITEM.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMQM)
          WITH TEXT-E11 'ZTQM_INSP_ITEM'.
  ENDIF.

  MODIFY ZTQM_INSP_ITEM_F FROM TABLE IT_ZTQM_INSP_ITEM_F.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMQM)
          WITH TEXT-E11 'ZTQM_INSP_ITEM_F'.
  ENDIF.

  MODIFY ZTQM_INSP_S_ITEM FROM TABLE IT_ZTQM_INSP_S_ITEM.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMQM)
          WITH TEXT-E11 'ZTQM_INSP_S_ITEM'.
  ENDIF.

  CHECK SY-SUBRC = 0.
  COMMIT WORK.
  PERFORM DEQUEUE_DB.
  MESSAGE S000(ZMQM) WITH TEXT-S01.

ENDFORM.                    " SAVE_DATA_0300
*&------------------------------------------------------------------*
*&      Form  BACKUP_RETRIEVED_DATA
*&------------------------------------------------------------------*
FORM BACKUP_RETRIEVED_DATA.
  IT_ITEM_B[]   = IT_ZTQM_INSP_ITEM[].
  IT_ITEM_F_B[] = IT_ZTQM_INSP_ITEM_F[].
  IT_S_ITEM_B[] = IT_ZTQM_INSP_S_ITEM[].

*-- Backup Header Data
  WA_ZSQM_INSP_SCH_HDR  = ZSQM_INSP_SCH_HDR .

**-- Clear TimeStamp Field Value for Checking of change.
*  CLEAR : IT_ITEM_B, IT_ITEM_F_B, IT_S_ITEM_B.
*  MODIFY IT_ITEM_F_B TRANSPORTING AEDAT AEZET AENAM
*                  WHERE MATNR NE ''.

ENDFORM.                    " BACKUP_RETRIEVED_DATA
*&-----------------------------------------------------------------*
*&      Form  CHECK_CHAGNED_DATA_EXIT
*&-----------------------------------------------------------------*
FORM CHECK_CHAGNED_DATA_EXIT.
*-- Check user chage data using compare internal table with Backup
  IF IT_ITEM_F_B[]            NE IT_ZTQM_INSP_ITEM_F[] OR
     ZSQM_INSP_SCH_HDR-STATUS NE ZSQM_INSP_SCH_HDR-STATUS.

*-- Fill Time Stamp data  for Header / Flat Data.
    MOVE : SY-DATUM TO ZTQM_INSP_HDR-AEDAT,
           SY-UZEIT TO ZTQM_INSP_HDR-AEZET,
           SY-UNAME TO ZTQM_INSP_HDR-AENAM.

    CLEAR : IT_ZTQM_INSP_ITEM_F.

    MOVE : SY-DATUM TO IT_ZTQM_INSP_ITEM_F-AEDAT,
           SY-UZEIT TO IT_ZTQM_INSP_ITEM_F-AEZET,
           SY-UNAME TO IT_ZTQM_INSP_ITEM_F-AENAM.

    MODIFY  IT_ZTQM_INSP_ITEM_F TRANSPORTING AEDAT AEZET AENAM
                   WHERE MATNR NE ''.

  ELSE.
    MESSAGE I000(ZMQM) WITH 'No Changed data.'(I02).
    STOP.
  ENDIF.

ENDFORM.                    " CHECK_CHAGNED_DATA_EXIT
*&------------------------------------------------------------------*
*&      Form  CREATE_INSPECTION_LOT
*&------------------------------------------------------------------*
FORM CREATE_INSPECTION_LOT.

  DATA : LW_INSP_HDR  TYPE ZTQM_INSP_HDR,
         LW_INSP_ITEM TYPE ZTQM_INSP_ITEM_F.
  DATA : LW_ITEM_INDEX LIKE SY-TABIX.

  DATA : LW_RETURN  TYPE BAPIRETURN.
  DATA : LT_BDC_MSG LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
  DATA : LW_PRUEFLOS    TYPE QPLOS,
         LW_PRUEFLOS_MS TYPE QPLOS.

  READ TABLE IT_ZSQM_INSP_SCH_ITEM_F WITH KEY MARK = C_MARK.
  LW_ITEM_INDEX = SY-TABIX.

  MOVE-CORRESPONDING : ZSQM_INSP_SCH_HDR       TO LW_INSP_HDR,
                       IT_ZSQM_INSP_SCH_ITEM_F TO LW_INSP_ITEM.

  CALL FUNCTION 'Z_FQM_INSPECTION_LOT_CREATE'
       EXPORTING
            I_INSP_HDR              = LW_INSP_HDR
            I_INSP_ITEM             = LW_INSP_ITEM
       IMPORTING
            E_PRUEFLOS              = LW_PRUEFLOS
            E_PRUEFLOS_MS           = LW_PRUEFLOS_MS
            RETURN                  = LW_RETURN
       TABLES
            T_BDC_MSG               = LT_BDC_MSG
       EXCEPTIONS
            ERROR_DURING_CREATE_LOT = 1
            OTHERS                  = 2.

  IF SY-SUBRC <> 0.
    READ TABLE LT_BDC_MSG WITH KEY MSGTYP = 'E'.
    MESSAGE E000(ZMQM) WITH LT_BDC_MSG-MSGNR
                            LT_BDC_MSG-MSGV1
                            LT_BDC_MSG-MSGV2.
    EXIT.
  ENDIF.

  READ TABLE LT_BDC_MSG WITH KEY MSGTYP = 'S'
                                 MSGNR  = '100'.

  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMQM) WITH TEXT-E08.
    EXIT.
  ENDIF.

  MOVE : LW_PRUEFLOS TO IT_ZSQM_INSP_SCH_ITEM_F-PRUEFLOS,
         LW_PRUEFLOS_MS TO IT_ZSQM_INSP_SCH_ITEM_F-PRUEFLOS_MS.

  IT_ZSQM_INSP_SCH_ITEM_F-STATUS = C_RELEASE.
  MOVE : SY-DATUM TO IT_ZSQM_INSP_SCH_ITEM_F-AEDAT,
         SY-UZEIT TO IT_ZSQM_INSP_SCH_ITEM_F-AEZET,
         SY-UNAME TO IT_ZSQM_INSP_SCH_ITEM_F-AENAM.

  MODIFY  IT_ZSQM_INSP_SCH_ITEM_F INDEX LW_ITEM_INDEX.

  CHECK SY-SUBRC = 0.

  MESSAGE S000(ZMQM)
  WITH 'Successfully Create Inspection Lot :'(S02) LW_PRUEFLOS.

ENDFORM.                    " CREATE_INSPECTION_LOT
*&-------------------------------------------------------------*
*&      Form  CREATE_N_SET_EXCEL_OBJECT
*&-------------------------------------------------------------*
FORM CREATE_N_SET_EXCEL_OBJECT.
**-- Create Excel OBJECT
  CREATE OBJECT H_EXCEL_OBJECT 'EXCEL.APPLICATION'.

*-- Set Object Attributes -  Visible
  SET PROPERTY OF H_EXCEL_OBJECT  'Visible' = 1.

*--  Get WORKBOOK LIST, initially empty(initial)
  CALL METHOD OF H_EXCEL_OBJECT 'Workbooks' = H_WORKBOOK_LIST.

*-- Add NEW WORKBOOK
  CALL METHOD OF H_WORKBOOK_LIST 'Add' = H_WORKBOOK.

ENDFORM.                    " CREATE_N_SET_EXCEL_OBJECT
*&------------------------------------------------------------------*
*&      Form  DOWNLOAD_TO_EXEL_NEW
*&-------------------------------------------------------------------*
FORM DOWNLOAD_TO_EXEL_NEW.
  DATA : LW_VALUE(100) TYPE C.

  DATA : LW_FLD_CNT(4) TYPE N.
  DATA : LW_FLD_NAME_V(40) TYPE C,  "/Start Date Field Name
         LW_FLD_NAME_B(40) TYPE C.  "/End Date Field Name
  FIELD-SYMBOLS : <L_FS_V>,  "/Start Date Field Symbol
                  <L_FS_B>.  "/End Date Field Symbol
  DATA : LW_HOR LIKE WA_HOR.

  CLEAR : WA_VER, WA_HOR.

  LOOP AT IT_ZSQM_INSP_SCH_ITEM_F.
    AT NEW MATNR.
      WA_HOR = 13.
      WA_VER = SY-TABIX.
    ENDAT.

    PERFORM FILL_CELL_OF_EXCEL
       USING : WA_VER  1   IT_ZSQM_INSP_SCH_ITEM_F-VEHICLE,
               WA_VER  2   IT_ZSQM_INSP_SCH_ITEM_F-IYEAR,
               WA_VER  3   IT_ZSQM_INSP_SCH_ITEM_F-ART,
               WA_VER  4   IT_ZSQM_INSP_SCH_ITEM_F-MATNR,
               WA_VER  5   IT_ZSQM_INSP_SCH_ITEM_F-MTART,
               WA_VER  6   IT_ZSQM_INSP_SCH_ITEM_F-STATUS,
               WA_VER  7   IT_ZSQM_INSP_SCH_ITEM_F-EXTWG,
               WA_VER  8   IT_ZSQM_INSP_SCH_ITEM_F-EWBEZ,
               WA_VER  9   IT_ZSQM_INSP_SCH_ITEM_F-WERKS,
               WA_VER 10   IT_ZSQM_INSP_SCH_ITEM_F-KATALOGART,
               WA_VER 11   IT_ZSQM_INSP_SCH_ITEM_F-CODEGRUPPE,
               WA_VER 12   IT_ZSQM_INSP_SCH_ITEM_F-CODE,
               WA_VER 13   IT_ZSQM_INSP_SCH_ITEM_F-PRUEFLOS,
               WA_VER 14   IT_ZSQM_INSP_SCH_ITEM_F-PRUEFLOS_MS.

    CLEAR LW_FLD_CNT.

    DO 12 TIMES.

      LW_FLD_CNT = LW_FLD_CNT + 10.

*--      ISIR or REGULAR
      IF LW_FLD_CNT <= '0100'.
        CONCATENATE 'IT_ZTQM_INSP_ITEM_F-DATUV_' LW_FLD_CNT
                                                 INTO LW_FLD_NAME_V.
        CONCATENATE 'IT_ZTQM_INSP_ITEM_F-DATUB_' LW_FLD_CNT
                                                 INTO LW_FLD_NAME_B.
        ASSIGN : (LW_FLD_NAME_V) TO <L_FS_V>,
                 (LW_FLD_NAME_B) TO <L_FS_B>.

*--      MS Interior
      ELSEIF LW_FLD_CNT = '0110'.
        MOVE : 'IT_ZTQM_INSP_ITEM_F-DATUV_1010' TO LW_FLD_NAME_V,
               'IT_ZTQM_INSP_ITEM_F-DATUB_1010' TO LW_FLD_NAME_B.

        ASSIGN : (LW_FLD_NAME_V) TO <L_FS_V>,
                 (LW_FLD_NAME_B) TO <L_FS_B>.

*--      MS Exterior
      ELSEIF LW_FLD_CNT = '0120'.
        MOVE : 'IT_ZTQM_INSP_ITEM_F-DATUV_2010' TO LW_FLD_NAME_V,
               'IT_ZTQM_INSP_ITEM_F-DATUB_2010' TO LW_FLD_NAME_B.

        ASSIGN : (LW_FLD_NAME_V) TO <L_FS_V>,
                 (LW_FLD_NAME_B) TO <L_FS_B>.

      ENDIF.

      WA_HOR = WA_HOR + 2.
      LW_HOR = WA_HOR + 1.

      PERFORM FILL_CELL_OF_EXCEL
         USING : WA_VER  WA_HOR   <L_FS_V>,
                 WA_VER  LW_HOR   <L_FS_B>.

    ENDDO.

    PERFORM DISPLAY_PROGRESS_INDICATOR USING  50
                                              TEXT-T01.

  ENDLOOP.
ENDFORM.                    " DOWNLOAD_TO_EXEL_NEW
*&-------------------------------------------------------------*
*&      Form  FILL_CELL_OF_EXCEL
*&-------------------------------------------------------------*
FORM FILL_CELL_OF_EXCEL USING   P_VER
                                P_HOR
                                P_VALUE.

  CALL METHOD OF H_EXCEL_OBJECT 'Cells' = H_CELL
                            EXPORTING #1 = P_VER #2 = P_HOR.

  SET PROPERTY OF H_CELL 'Value' = P_VALUE .
*  GET PROPERTY OF H_CELL 'Font' = H_CELL.
*  SET PROPERTY OF H_FONT 'Bold' = P_BOLD .
*  SET PROPERTY OF H_COLOR 'Color' = 10.

ENDFORM.                    " FILL_CELL_OF_EXCEL
*&------------------------------------------------------------------*
*&      Form  FREE_DISCONNECT_TO_EXCEL
*&------------------------------------------------------------------*
FORM FREE_DISCONNECT_TO_EXCEL.
* disconnect from Excel
  FREE OBJECT H_EXCEL_OBJECT.
ENDFORM.                    " FREE_DISCONNECT_TO_EXCEL
