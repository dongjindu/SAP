*----------------------------------------------------------------------*
*   INCLUDE ZACOU116_F01                                               *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  CLEAR: GT_116, GT_OUT.
  REFRESH: GT_116, GT_OUT.

  SELECT * INTO TABLE GT_116
    FROM ZTCOU116
   WHERE KOKRS = P_KOKRS
     AND BDATJ = P_YEAR
     AND VER   = P_VER.

  IF SY-SUBRC = 0.
    LOOP AT GT_116.
      MOVE-CORRESPONDING GT_116 TO GT_OUT.
      APPEND GT_OUT.
      CLEAR GT_OUT.
    ENDLOOP.

  ELSE.
    MESSAGE S000 WITH 'No data found.'.
    EXIT.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Create ALV control: Field catalog
*----------------------------------------------------------------------*
FORM CREATE_FIELD_CATEGORY.
  DATA LV_CNT TYPE I.

  PERFORM FILL_FIELD_CATEGORY USING:
*     1  'KOKRS' 'Controlling Area'      4    'CHAR',
*     2  'BDATJ' 'Year'                  4    'NUMC',
*     3  'VER'   'Ver'                   2    'NUMC',
     1  'LAND1' 'Country key'           08   'CHAR',
     2  'MATNR' 'Material'              18   'CHAR',
     3  'FRA1'  'Freight%(CKD)'         12   'DEC',
     4  'ZOTH'  'Other(L/C Relevant)%'  12   'DEC',
     5  'ZOTI'  'Other(B/L Relevant)%'  12   'DEC',
     6  'AEDAT' 'Changed on'            10   'DATS',
     7  'AENAM' 'Changed by'            10   'CHAR'.

  GS_FCAT-JUST = 'R'.
  GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
  GS_FCAT-REF_TABLE = 'ZTCOU116'.
  GS_FCAT-OUTPUTLEN = 18.
  MODIFY GT_FCAT FROM GS_FCAT TRANSPORTING JUST REF_FIELD REF_TABLE
                                     WHERE DATATYPE = 'DEC'.


  LOOP AT GT_FCAT INTO GS_FCAT.
    LV_CNT = LV_CNT + 1.
    CASE GS_FCAT-FIELDNAME.

      WHEN 'FRA1' or 'ZOTH' or 'ZOTI'.
        GS_FCAT-EDIT = 'X'.
        GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
        GS_FCAT-REF_TABLE = 'ZTCOU116'.

        MODIFY GT_FCAT INDEX LV_CNT FROM GS_FCAT
                       TRANSPORTING REF_FIELD REF_TABLE EDIT.
      WHEN 'LAND1'.
        GS_FCAT-EDIT = 'X'.
        GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
        GS_FCAT-REF_TABLE = 'T005'.
        MODIFY GT_FCAT INDEX LV_CNT FROM GS_FCAT
                       TRANSPORTING REF_FIELD REF_TABLE EDIT.

      WHEN 'MATNR'.
        GS_FCAT-EDIT = 'X'.
        GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
        GS_FCAT-REF_TABLE = 'MARA'.
        MODIFY GT_FCAT INDEX LV_CNT FROM GS_FCAT
                       TRANSPORTING REF_FIELD REF_TABLE EDIT.

    ENDCASE.
  ENDLOOP.
ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       Save to table ZTCOU116 & Update table ZTCOU102
*----------------------------------------------------------------------*
FORM SAVE_DATA.
  DATA: LT_ROW   TYPE LVC_T_ROW,
        LS_ROW   TYPE LVC_S_ROW,
        LT_ROID  TYPE LVC_T_ROID,
        L_BDATJ  TYPE BDATJ,
        L_CNT(5),
        L_DCNT(5),
        L_UCNT(5),
        L_MSG(200),                 " Message
        LS_116 LIKE ZTCOU116.

* Save seleted data to table ZTCOU116
  CLEAR: L_CNT, L_DCNT, L_UCNT, LT_ROW[], LT_ROID[].

*  LOOP AT GT_116.
*    READ TABLE GT_OUT WITH KEY KOKRS = GT_116-KOKRS
*                               BDATJ = GT_116-BDATJ
*                               VER = GT_116-VER.
*    IF SY-SUBRC <> 0.
*      DELETE ZTCOU116 FROM GT_116.
*      IF SY-SUBRC = 0.
*        L_DCNT = L_DCNT + 1.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  CALL METHOD G_GRID->GET_SELECTED_ROWS
*              IMPORTING ET_INDEX_ROWS = LT_ROW
*                        ET_ROW_NO = LT_ROID.
*
*  LOOP AT LT_ROW INTO LS_ROW.
*    READ TABLE GT_OUT INDEX LS_ROW-INDEX.
*
*    IF SY-SUBRC = 0.
*      PERFORM UPDATE_ZTCOU102 CHANGING L_UCNT.
*
*      CLEAR L_BDATJ.
*      SELECT SINGLE BDATJ INTO L_BDATJ
*        FROM ZTCOU116
*       WHERE KOKRS = GT_OUT-KOKRS
*         AND BDATJ = GT_OUT-BDATJ
*         AND VER   = GT_OUT-VER.
*
*      IF SY-SUBRC = 0.
*        UPDATE ZTCOU116 SET FRA1  = GT_OUT-FRA1
*                            ZOTH  = GT_OUT-ZOTH
*                            ZOTI  = GT_OUT-ZOTI
*                            AEDAT = SY-DATUM
*                            AENAM = SY-UNAME
*                      WHERE KOKRS = GT_OUT-KOKRS
*                        AND BDATJ = GT_OUT-BDATJ
*                        AND VER   = GT_OUT-VER.
*      ELSE.
*        CLEAR LS_116.
*        MOVE-CORRESPONDING GT_OUT TO LS_116.
*
*        LS_116-KOKRS = P_KOKRS.
*        LS_116-BDATJ = P_YEAR.
*        LS_116-AEDAT = SY-DATUM.
*        LS_116-AENAM = SY-UNAME.
*
*        INSERT INTO ZTCOU116 VALUES LS_116.
*      ENDIF.
*
*      IF SY-SUBRC = 0.
*        L_CNT = L_CNT + 1.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

  DATA: L_NEW TYPE I,
        L_OLD TYPE I.

  DESCRIBE TABLE GT_OUT LINES L_NEW.
  DESCRIBE TABLE GT_116 LINES L_OLD.

  IF L_OLD > 0 AND L_NEW = 0.
    SELECT COUNT( * ) INTO SY-INDEX FROM ZTCOU102
       WHERE KOKRS = P_KOKRS
         AND BDATJ = P_YEAR
         AND KALKA = 'BP'
         AND VER   = P_VER.
    IF SY-SUBRC = 0.
      MESSAGE W000 WITH 'Data already exist with LDC!'.
      EXIT.
    ENDIF.
  ENDIF.

  DELETE FROM ZTCOU116
       WHERE KOKRS = P_KOKRS
         AND BDATJ = P_YEAR
         AND VER   = P_VER.

  LOOP AT GT_OUT.
    MOVE-CORRESPONDING GT_OUT TO LS_116.

    LS_116-KOKRS = P_KOKRS.
    LS_116-BDATJ = P_YEAR.
    LS_116-VER   = P_VER.
    LS_116-AEDAT = SY-DATUM.
    LS_116-AENAM = SY-UNAME.
    INSERT INTO ZTCOU116 VALUES LS_116.
    L_UCNT = L_UCNT + 1.

    IF P_REF = 'X'.
      PERFORM UPDATE_ZTCOU102 CHANGING L_UCNT.
    ENDIF.
  ENDLOOP.

  IF  P_REF = 'X'.
    CONDENSE L_UCNT.
    CONCATENATE 'You have updated costing result table;'
                L_UCNT  'records.'
           INTO L_MSG SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'New LDC% is updated; '
                L_UCNT  'records.'
           INTO L_MSG SEPARATED BY SPACE.
  ENDIF.

  MESSAGE S000 WITH L_MSG.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
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
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
FORM EXCLUDE_FUNCTIONS USING    VALUE(P_0080).
  PERFORM APPEND_EXCLUDE_FUNCTIONS
          TABLES GT_EXCLUDE[]
           USING: CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
                  CL_GUI_ALV_GRID=>MC_FC_AVERAGE,
                  CL_GUI_ALV_GRID=>MC_FC_GRAPH,
                  CL_GUI_ALV_GRID=>MC_FC_INFO,
                  CL_GUI_ALV_GRID=>MC_FC_REFRESH.

ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTCOU102
*&---------------------------------------------------------------------*
*       Update Table ZTCOU102 : Freight & Other
*----------------------------------------------------------------------*
FORM UPDATE_ZTCOU102 CHANGING P_CNT TYPE CHAR05.
  DATA: L_ZOTH TYPE ZOTH,
        L_IDX  TYPE SY-INDEX.

  CLEAR GT_102.
  REFRESH GT_102.
  CLEAR P_CNT.

  SELECT A~KOKRS A~BDATJ A~POPER A~KALKA A~VER A~MATNR
         A~WERKS A~LIFNR A~WERTN
    INTO TABLE GT_102
    FROM ZTCOU102 AS A
    JOIN LFA1 AS B
      ON B~LIFNR = A~LIFNR
     AND B~LAND1 <> 'US'
   WHERE A~KOKRS = P_KOKRS
     AND A~BDATJ = P_YEAR
     AND A~KALKA = 'BP'
     AND A~VER = GT_OUT-VER.

  CHECK SY-SUBRC = 0.

  SORT GT_OUT BY LAND1 MATNR.

  LOOP AT GT_102.
    READ TABLE GT_OUT WITH KEY LAND1 = GT_OUT-LAND1
                               MATNR = GT_OUT-MATNR.
    IF SY-SUBRC <> 0.
      READ TABLE GT_OUT WITH KEY LAND1 = GT_OUT-LAND1.
    ENDIF.

    IF SY-SUBRC = 0.

*     Freight: Info-price * Freight%
      GT_102-FRG = GT_102-WERTN * ( GT_OUT-FRA1 / 100 ).

*     Other: Info-price * ( Other(L/C Relevant)% + Other(B/L Relevant)%)
      L_ZOTH = GT_OUT-ZOTH / 100 + GT_OUT-ZOTI / 100.
      GT_102-OTH = GT_102-WERTN * L_ZOTH.

      UPDATE ZTCOU102 SET FRG = GT_102-FRG
                          OTH = GT_102-OTH
                   WHERE KOKRS = GT_102-KOKRS
                     AND BDATJ = GT_102-BDATJ
                     AND KALKA = GT_102-KALKA
                     AND VER   = GT_102-VER
                     AND POPER = GT_102-POPER
                     AND MATNR = GT_102-MATNR
                     AND ZLOCK NE 'X'.

      IF SY-SUBRC = 0.
        P_CNT = P_CNT + 1.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " UPDATE_ZTCOU102
