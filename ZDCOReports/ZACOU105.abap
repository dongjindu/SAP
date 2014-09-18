*----------------------------------------------------------------------
* Program ID        : ZACOU105
* Title             : [CO] Following Part Information
* Created on        : 09/01/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Register following part information.
*----------------------------------------------------------------------
REPORT ZACOU105 MESSAGE-ID ZMCO.

INCLUDE ZACOUI00.
INCLUDE ZACOU105_TOP.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME.
PARAMETERS: P_KOKRS LIKE ZTCOU105-KOKRS OBLIGATORY MEMORY ID CAC
                                        MATCHCODE OBJECT FC_KOKRS.
SELECT-OPTIONS: S_FMATNR FOR ZTCOU105-FMATNR,
                S_TMATNR FOR ZTCOU105-TMATNR.
SELECTION-SCREEN END OF BLOCK B0.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM GET_DATA.
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.

  PERFORM CREATE_ALV_CONTROL.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'SAVE'.
*     Save data to table ZTCOU105
      PERFORM SAVE_DATA.

*     Refresh ALV field catalog & layout
      PERFORM REFRESH_FIELD.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  DATA LV_CNT TYPE I.

  CLEAR: GT_OUT, GV_CNT, LV_CNT, BEZEI.
  REFRESH GT_OUT.

* Company description
  SELECT SINGLE BEZEI INTO BEZEI
    FROM TKA01
   WHERE KOKRS = P_KOKRS.

* Get Following part information
  SELECT * INTO TABLE GT_ZTCOU105
    FROM ZTCOU105
   WHERE KOKRS = P_KOKRS
     AND FMATNR IN S_FMATNR
     AND TMATNR IN S_TMATNR.

  IF SY-SUBRC = 0.
    LOOP AT GT_ZTCOU105.
      MOVE-CORRESPONDING GT_ZTCOU105 TO GT_OUT.
      APPEND GT_OUT.
      CLEAR GT_OUT.
    ENDLOOP.
  ENDIF.

  DESCRIBE TABLE GT_ZTCOU105 LINES GV_CNT.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_CONTROL
*&---------------------------------------------------------------------*
FORM CREATE_ALV_CONTROL.
  IF G_CUSTOM_CONTAINER IS INITIAL.
*   Create object
    PERFORM CREATE_OBJECT.

*   Exclude toolbar
    PERFORM EXCLUDE_FUNCTIONS USING 'GT_EXCLUDE'.

*   Create field category
    PERFORM CREATE_FIELD_CATEGORY.

*   Setting for layout
    PERFORM SET_LVC_LAYOUT.

*   Define editable field
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
         EXPORTING I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

*   Setting for event
    PERFORM SET_EVENT.

*   Define variant
    GS_VARIANT-REPORT = SY-REPID.

*   Display alv grid
    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING IS_LAYOUT            = GS_LAYO
                   IT_TOOLBAR_EXCLUDING = GT_EXCLUDE
                   I_SAVE               = GC_VAR_SAVE
                   IS_VARIANT           = GS_VARIANT
         CHANGING  IT_OUTTAB            = GT_OUT[]
                   IT_FIELDCATALOG      = GT_FCAT[].

  ENDIF.

ENDFORM.                    " CREATE_ALV_CONTROL
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
FORM CREATE_FIELD_CATEGORY.
  CLEAR GT_FCAT.

  PERFORM FILL_FIELD_CATEGORY USING:
          1  'FMATNR'   'Matrial From'        18  'CHAR',
          2  'TMATNR'   'Matrial To'          18  'CHAR',
          3  'DATUV'    'Valid from'          10  'CHAR',
          4  'AENNR'    'Change No'           12  'CHAR'.

  LOOP AT GT_FCAT INTO GS_FCAT.
    IF GS_FCAT-FIELDNAME = 'FMATNR' OR
       GS_FCAT-FIELDNAME = 'TMATNR'.
      GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
      GS_FCAT-REF_TABLE = 'ZTCOU105'.
      MODIFY GT_FCAT FROM GS_FCAT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SET_EVENT
*&---------------------------------------------------------------------*
*       Setting for event
*----------------------------------------------------------------------*
FORM SET_EVENT.
  CREATE OBJECT G_EVENT_RECEIVER.
  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR G_GRID.

ENDFORM.                    " SET_EVENT
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       Event of changed data
*----------------------------------------------------------------------*
*      -->RR_DATA_CHANGED  Log is Visible
*----------------------------------------------------------------------*
FORM DATA_CHANGED USING RR_DATA_CHANGED
                        TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
        LS_CELLS     TYPE LVC_S_MODI,
        LT_VALUES TYPE TABLE OF BAPI_CHAR_VALUES WITH HEADER LINE.

  LOOP AT RR_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.
    READ TABLE GT_OUT INDEX LS_MOD_CELLS-ROW_ID.

    IF SY-SUBRC = 0.
      CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
                EXPORTING I_ROW_ID    = LS_MOD_CELLS-ROW_ID
                          I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
                          I_VALUE     = LS_MOD_CELLS-VALUE.

      MODIFY GT_OUT INDEX LS_MOD_CELLS-ROW_ID.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       Save data to table ZTCOU105
*----------------------------------------------------------------------*
FORM SAVE_DATA.
  DATA: LV_MATNR TYPE MATNR,
        LT_ROW   TYPE LVC_T_ROW,
        LS_ROW   TYPE LVC_S_ROW,
        LT_ROID  TYPE LVC_T_ROID,
        LV_CNT(5),
        LV_DCNT(5),
        LV_MSG(200),                 " Message
        LS_ZTCOU105 LIKE ZTCOU105.

* Save seleted data to table ZTCOU105
  CLEAR: LV_CNT, LV_DCNT, LT_ROW[], LT_ROID[].

  DELETE GT_OUT WHERE FMATNR IS INITIAL.

  LOOP AT GT_ZTCOU105.
    READ TABLE GT_OUT WITH KEY FMATNR = GT_ZTCOU105-FMATNR
                               TMATNR = GT_ZTCOU105-TMATNR.
    IF SY-SUBRC <> 0.
      DELETE ZTCOU105 FROM GT_ZTCOU105.
      IF SY-SUBRC = 0.
        LV_DCNT = LV_DCNT + 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CALL METHOD G_GRID->GET_SELECTED_ROWS
              IMPORTING ET_INDEX_ROWS = LT_ROW
                        ET_ROW_NO = LT_ROID.

  LOOP AT LT_ROW INTO LS_ROW.
    READ TABLE GT_OUT INDEX LS_ROW-INDEX.

    IF SY-SUBRC = 0.
      CLEAR LV_MATNR.
      SELECT SINGLE FMATNR INTO LV_MATNR
        FROM ZTCOU105
       WHERE KOKRS = P_KOKRS
         AND FMATNR = GT_OUT-FMATNR
         AND TMATNR = GT_OUT-TMATNR.

     IF SY-SUBRC = 0.
        GT_OUT-GJAHR = GT_OUT-datuv(4).
        GT_OUT-MONAT = GT_OUT-datuv+4(2).

        UPDATE ZTCOU105 SET KOKRS = P_KOKRS
                            FMATNR = GT_OUT-FMATNR
                            TMATNR = GT_OUT-TMATNR
                            DATUV  = GT_OUT-DATUV
                            AENNR  = GT_OUT-AENNR
                            GJAHR = GT_OUT-GJAHR
                            MONAT = GT_OUT-MONAT
                            AEDAT = SY-DATUM
                            AENAM = SY-UNAME
                      WHERE KOKRS = P_KOKRS
                        AND FMATNR = GT_OUT-FMATNR
                        AND TMATNR = GT_OUT-TMATNR.
      ELSE.
        CLEAR LS_ZTCOU105.
        MOVE-CORRESPONDING GT_OUT TO LS_ZTCOU105.
        LS_ZTCOU105-KOKRS = P_KOKRS.
        LS_ZTCOU105-GJAHR = LS_ZTCOU105-datuv(4).
        LS_ZTCOU105-MONAT = LS_ZTCOU105-datuv+4(2).
        LS_ZTCOU105-AEDAT = SY-DATUM.
        LS_ZTCOU105-AENAM = SY-UNAME.

        INSERT INTO ZTCOU105 VALUES LS_ZTCOU105.
      ENDIF.

      IF SY-SUBRC = 0.
        LV_CNT = LV_CNT + 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF LV_DCNT > 0.
    IF LV_CNT > 0.
      CONCATENATE 'have deleted' LV_DCNT  'records,'
                  'saved' LV_CNT 'records.'
             INTO LV_MSG SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'have deleted' LV_DCNT  'records.'
             INTO LV_MSG SEPARATED BY SPACE.
    ENDIF.

  ELSE.
    CONCATENATE 'You have saved data completely;'
                 LV_CNT  'records.'
            INTO LV_MSG SEPARATED BY SPACE.
  ENDIF.

  MESSAGE S000 WITH LV_MSG.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
FORM SET_LVC_LAYOUT.
  GS_LAYO-EDIT       = 'X'.
  GS_LAYO-ZEBRA      = 'X'.
  GS_LAYO-SEL_MODE   = 'A'.       " Column and row selection
  GS_LAYO-CWIDTH_OPT = 'X'.

ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
FORM EXCLUDE_FUNCTIONS USING P_TABNAME.
  PERFORM APPEND_EXCLUDE_FUNCTIONS
          TABLES GT_EXCLUDE[]
           USING: CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
                  CL_GUI_ALV_GRID=>MC_FC_AVERAGE,
                  CL_GUI_ALV_GRID=>MC_FC_GRAPH,
                  CL_GUI_ALV_GRID=>MC_FC_INFO,
                  CL_GUI_ALV_GRID=>MC_FC_REFRESH.

ENDFORM.                    " EXCLUDE_FUNCTIONS
