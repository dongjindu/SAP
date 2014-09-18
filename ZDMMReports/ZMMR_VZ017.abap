*&--------------------------------------------------------------------&*
*& Program     : ZMMR_VZ001                                           &*
*& Create Date : 2008.01.30                                          &*
*& Creater     : CSG                                                  &*
*& Description : HS-Code Maintenance Program                          &*
*&                                                                    &*
*&--------------------------------------------------------------------&*
*& Change No : #1           Changer :       Change Date :             &*
*& Change Description :                    .                          &*
*&                                                                    &*
*&--------------------------------------------------------------------&*

REPORT ZMMR_VZ017 NO STANDARD PAGE HEADING
                          MESSAGE-ID ZMM01.

INCLUDE : ZMM_COMMON01,
          ZMMR_VZ017TOP.
*&---------------------------------------------------------------------*
*&    INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INIT.
*&---------------------------------------------------------------------*
*&    AT SELECTION-SCREEN ON
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
  PERFORM GET_DATA.
  IF IT_ITAB[] IS INITIAL.

  ELSE.
    G_CHNG = 'X'.
    CALL SCREEN 1100.
  ENDIF.
*&---------------------------------------------------------------------*
*&    START-OF-SELECTION
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&    GET
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&    END-OF-SELECTION
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&    TOP-OF-PAGE
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&    TOP-OF-PAGE ON LINE-SELECTION
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&    END-OF-PAGE
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&    AT LINE-SELECTION
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&    AT USER-COMMAND
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
  CLEAR : IT_ITAB, IT_ITAB[].

  SELECT * INTO
  CORRESPONDING FIELDS OF TABLE IT_ITAB
  FROM T604 AS A INNER JOIN T604T AS B
       ON A~MANDT EQ B~MANDT
      AND A~LAND1 EQ B~LAND1
      AND A~STAWN EQ B~STAWN
  WHERE B~SPRAS = 'E'.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1100 OUTPUT.

  DATA : FCODE TYPE TABLE OF SY-UCOMM.
  CLEAR: FCODE, FCODE[].

  CASE 'X'.
    WHEN G_DISP.
      APPEND 'DSP' TO FCODE.
      APPEND 'DEL' TO FCODE.
      APPEND 'NEW' TO FCODE.
      APPEND 'SAVE' TO FCODE.
    WHEN G_CHNG.
      APPEND 'CHG' TO FCODE.
  ENDCASE.
  SET PF-STATUS 'G1100' EXCLUDING FCODE.
  SET TITLEBAR 'T1100'.

  DESCRIBE TABLE IT_ITAB LINES TC_LIST-LINES.
ENDMODULE.                 " STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT .

  CLEAR : G_DISP, G_CHNG, G_NEW.
  CLEAR : DEL_ITAB, DEL_ITAB[].
ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_CONTROL OUTPUT.
  LOOP AT SCREEN.
    CASE 'X'.
      WHEN G_CHNG.
        IF SCREEN-NAME EQ 'IT_ITAB-TEXT1' OR
           SCREEN-NAME EQ 'IT_ITAB-BEMEH' OR
           SCREEN-NAME EQ 'IT_ITAB-IMPMA' OR
           SCREEN-NAME EQ 'IT_ITAB-MINOL'.
          SCREEN-INPUT = 1.
          MODIFY SCREEN.
        ENDIF.
      WHEN G_DISP.
        IF SCREEN-NAME NE 'IT_ITAB-MARK'.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDMODULE.                 " SCREEN_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BACK_EXIT INPUT.

  LEAVE PROGRAM.
ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_ITEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_ITEM INPUT.
  MODIFY IT_ITAB INDEX TC_LIST-CURRENT_LINE.
ENDMODULE.                 " READ_ITEM  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1100 INPUT.

  DATA: SAVE_OKCODE LIKE OK_CODE,
        L_ANSWER(1) TYPE C.
  CLEAR:SAVE_OKCODE, L_ANSWER.
  MOVE: OK_CODE TO SAVE_OKCODE.
  CLEAR OK_CODE.

  CASE SAVE_OKCODE.
    WHEN 'BACK'.
      PERFORM POP_TO_CONFIRM CHANGING L_ANSWER.
      CHECK L_ANSWER = 'J'.
      LEAVE PROGRAM.

    WHEN 'DSP'.
      G_DISP = 'X'.
      CLEAR G_CHNG.

    WHEN 'CHG'.
      G_CHNG = 'X'.
      CLEAR G_DISP.

    WHEN 'SALL'.
      LOOP AT IT_ITAB.
        IT_ITAB-MARK = 'X'.
        MODIFY IT_ITAB. CLEAR IT_ITAB.
      ENDLOOP.

    WHEN 'DSEL'.
      LOOP AT IT_ITAB.
        CLEAR IT_ITAB-MARK.
        MODIFY IT_ITAB. CLEAR IT_ITAB.
      ENDLOOP.

    WHEN 'DEL'.
      LOOP AT IT_ITAB WHERE MARK = 'X'.
        MOVE-CORRESPONDING IT_ITAB TO DEL_ITAB.
        APPEND DEL_ITAB.  CLEAR DEL_ITAB.

        DELETE IT_ITAB.
        CLEAR IT_ITAB.
      ENDLOOP.
      MESSAGE S000 WITH 'Deleted entry!'.
    WHEN 'DTL'.
      PERFORM MOVE_DATA.
      CALL SCREEN 1200.

    WHEN 'NEW'.
      CLEAR ST_ITAB.
      G_NEW = 'X'.
      W_REP_INDEX = 1.
      CLEAR : TEMP_ITAB, TEMP_ITAB[].
      CALL SCREEN 1200.

    WHEN 'SAVE'.
      PERFORM SAVE_DATA.
      MESSAGE S000 WITH 'Data was saved'.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1200 OUTPUT.
  SET PF-STATUS 'G1200'.
  IF G_NEW EQ 'X'.
    SET TITLEBAR 'T1200'.
  ELSE.
    SET TITLEBAR 'T1100'.
  ENDIF.

ENDMODULE.                 " STATUS_1200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MOVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MOVE_DATA .
  CLEAR : TEMP_ITAB, TEMP_ITAB[],
          W_REP_LINES, W_REP_INDEX.

  LOOP AT IT_ITAB WHERE MARK = 'X'.
    MOVE-CORRESPONDING IT_ITAB TO TEMP_ITAB.
    APPEND TEMP_ITAB.  CLEAR TEMP_ITAB.
    CLEAR IT_ITAB.
  ENDLOOP.

  DESCRIBE TABLE TEMP_ITAB LINES W_REP_LINES.
  W_REP_INDEX = 1.

  IF TEMP_ITAB[] IS INITIAL.
    READ TABLE IT_ITAB INDEX 1.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING IT_ITAB TO ST_ITAB.
    ENDIF.
  ELSE.
    READ TABLE TEMP_ITAB INDEX 1.
    MOVE-CORRESPONDING TEMP_ITAB TO ST_ITAB.
  ENDIF.
ENDFORM.                    " MOVE_DATA
*&---------------------------------------------------------------------*
*&      Module  SCREEN_CONTROL_1200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_CONTROL_1200 OUTPUT.

  IF G_NEW EQ 'X'.
    LOOP AT SCREEN.
      SCREEN-INPUT = 1.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    CASE 'X'.
      WHEN G_CHNG.
        LOOP AT SCREEN.
          IF SCREEN-NAME EQ 'ST_TEXT1' OR
             SCREEN-NAME EQ 'ST_BEMEH' OR
             SCREEN-NAME EQ 'ST_IMPMA' OR
             SCREEN-NAME EQ 'ST_MINOL'.
            SCREEN-INPUT = 1.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.

      WHEN G_DISP.
        LOOP AT SCREEN.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDLOOP.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " SCREEN_CONTROL_1200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT1200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BACK_EXIT1200 INPUT.
  CLEAR L_ANSWER.

  IF G_NEW EQ 'X'.
*    PERFORM POP_TO_CONFIRM CHANGING L_ANSWER.
    PERFORM CHANGE_ITAB.
    PERFORM MOVE_IT_ITAB.
    LEAVE TO SCREEN 0.
  ELSE.
    LEAVE PROGRAM.
  ENDIF.
ENDMODULE.                 " BACK_EXIT1200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1200 INPUT.

  CLEAR L_ANSWER.
  MOVE: OK_CODE TO SAVE_OKCODE.
  CLEAR OK_CODE.

  CASE SAVE_OKCODE.
*.. BACK
    WHEN 'BACK'.
      PERFORM CHANGE_ITAB.
      PERFORM MOVE_IT_ITAB.
      LEAVE TO SCREEN 0.
*.. Previous
    WHEN 'PREV'.
      IF G_NEW NE 'X'.
        PERFORM DETAIL_PREV_RTN.
      ELSE.
        PERFORM NEW_PREV_RTN.
      ENDIF.
*.. Next
    WHEN 'NEXT'.
      IF G_NEW NE 'X'.
        PERFORM DETAIL_NEXT_RTN.
      ELSE.
        PERFORM NEW_NEXT_RTN.
      ENDIF.
*.. Data Save
    WHEN 'SAVE'.
      PERFORM CHANGE_ITAB.
      PERFORM MOVE_IT_ITAB.
      PERFORM SAVE_DATA.
      MESSAGE S000 WITH 'Data was saved'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1200  INPUT
*&---------------------------------------------------------------------*
*&      Form  READ_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ITEM .

  READ TABLE TEMP_ITAB INDEX W_REP_INDEX.
  IF SY-SUBRC = 0.
    CLEAR ST_ITAB.
    MOVE-CORRESPONDING TEMP_ITAB TO ST_ITAB.
  ENDIF.
ENDFORM.                    " READ_ITEM
*&---------------------------------------------------------------------*
*&      Form  CHANGE_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_ITAB .

  READ TABLE TEMP_ITAB WITH KEY LAND1 = ST_ITAB-LAND1
                                STAWN = ST_ITAB-STAWN.
  IF SY-SUBRC = 0.
    LOOP AT TEMP_ITAB WHERE LAND1 = ST_ITAB-LAND1
                        AND STAWN = ST_ITAB-STAWN.
      MOVE-CORRESPONDING ST_ITAB TO TEMP_ITAB.
      MODIFY TEMP_ITAB.
      CLEAR TEMP_ITAB.
    ENDLOOP.
  ELSE.
    APPEND ST_ITAB TO TEMP_ITAB.
  ENDIF.

*  DESCRIBE TABLE TEMP_ITAB LINES W_REP_LINES.
ENDFORM.                    " CHANGE_ITAB
*&---------------------------------------------------------------------*
*&      Form  DETAIL_NEXT_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DETAIL_NEXT_RTN .

  IF W_REP_INDEX EQ W_REP_LINES.
    MESSAGE S000 WITH 'Last selected entry has been reached'.
  ELSE.
    W_REP_INDEX = W_REP_INDEX + 1.
    IF W_REP_INDEX GE W_REP_LINES.
      MOVE: W_REP_LINES TO W_REP_INDEX.
    ENDIF.

    PERFORM CHANGE_ITAB.
    IF W_REP_INDEX GE W_REP_LINES.
      MOVE: W_REP_LINES TO W_REP_INDEX.
    ENDIF.
    PERFORM READ_ITEM.
  ENDIF.
ENDFORM.                    " DETAIL_NEXT_RTN
*&---------------------------------------------------------------------*
*&      Form  NEW_NEXT_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM NEW_NEXT_RTN .

  PERFORM CHANGE_ITAB.
  DESCRIBE TABLE TEMP_ITAB LINES W_REP_LINES.


  IF W_REP_INDEX = W_REP_LINES.
    CLEAR ST_ITAB.
    W_REP_INDEX = W_REP_INDEX + 1.
  ELSE.
    W_REP_INDEX = W_REP_INDEX + 1.
    READ TABLE TEMP_ITAB INDEX W_REP_INDEX.
    CHECK SY-SUBRC = 0.
    MOVE-CORRESPONDING TEMP_ITAB TO ST_ITAB.
  ENDIF.
*  IF W_REP_INDEX GE W_REP_LINES.
*    MOVE: W_REP_LINES TO W_REP_INDEX.
*  ENDIF.


ENDFORM.                    " NEW_NEXT_RTN
*&---------------------------------------------------------------------*
*&      Form  DETAIL_PREV_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DETAIL_PREV_RTN .

  IF W_REP_INDEX EQ 1.
    MESSAGE S000 WITH 'First selected entry has been reached'.
  ELSE.
    W_REP_INDEX = W_REP_INDEX - 1.
    IF W_REP_INDEX LE 0.
      MOVE: 1 TO W_REP_INDEX.
    ENDIF.
    PERFORM CHANGE_ITAB.
    IF W_REP_INDEX LE 0.
      MOVE: 1 TO W_REP_INDEX.
    ENDIF.
    PERFORM READ_ITEM.
  ENDIF.
ENDFORM.                    " DETAIL_PREV_RTN
*&---------------------------------------------------------------------*
*&      Form  NEW_PREV_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM NEW_PREV_RTN .

  PERFORM CHANGE_ITAB.
  DESCRIBE TABLE TEMP_ITAB LINES W_REP_LINES.

  IF W_REP_INDEX EQ 1.
    MESSAGE S000 WITH 'First selected entry has been reached'.
  ELSE.
    W_REP_INDEX = W_REP_INDEX - 1.
    IF W_REP_INDEX LE 0.
      MOVE: 1 TO W_REP_INDEX.
    ENDIF.
    READ TABLE TEMP_ITAB INDEX W_REP_INDEX.
    CHECK SY-SUBRC = 0.
    MOVE-CORRESPONDING TEMP_ITAB TO ST_ITAB.
  ENDIF.

ENDFORM.                    " NEW_PREV_RTN
*&---------------------------------------------------------------------*
*&      Module  CHECK_PARAMETER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_PARAMETER INPUT.

  CHECK G_NEW = 'X'.

  SELECT SINGLE *
  FROM T604
  WHERE LAND1 = ST_ITAB-LAND1
    AND STAWN = ST_ITAB-STAWN.

  IF SY-SUBRC = 0.
    MESSAGE E000 WITH 'An entry already exists with same key'.
    STOP.
  ENDIF.
ENDMODULE.                 " CHECK_PARAMETER  INPUT
*&---------------------------------------------------------------------*
*&      Form  MOVE_IT_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MOVE_IT_ITAB .

  LOOP AT TEMP_ITAB.

    READ TABLE IT_ITAB WITH KEY LAND1 = TEMP_ITAB-LAND1
                                STAWN = TEMP_ITAB-STAWN.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING TEMP_ITAB TO IT_ITAB.
      MODIFY IT_ITAB TRANSPORTING LAND1 STAWN TEXT1
                                  BEMEH IMPMA MINOL
                          WHERE LAND1 = TEMP_ITAB-LAND1
                            AND STAWN = TEMP_ITAB-STAWN.
      CLEAR IT_ITAB.
    ELSE.
      MOVE-CORRESPONDING TEMP_ITAB TO IT_ITAB.
      APPEND IT_ITAB. CLEAR IT_ITAB.
    ENDIF.
    CLEAR TEMP_ITAB.
  ENDLOOP.
ENDFORM.                    " MOVE_IT_ITAB
*&---------------------------------------------------------------------*
*&      Form  POP_TO_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM POP_TO_CONFIRM  CHANGING    P_ANSWER.
  DATA : L_TITLE(35) TYPE C,
         L_TEXT1(35) TYPE C,
         L_TEXT2(35) TYPE C.

  CLEAR : L_TITLE, L_TEXT1, L_TEXT2.

  MOVE : 'Exit Maintenance'   TO L_TITLE,
         'Do you want leave from'   TO L_TEXT1,
         'this screen?' TO L_TEXT2.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      DEFAULTOPTION  = 'N'
      TEXTLINE1      = L_TEXT1
      TEXTLINE2      = L_TEXT2
      TITEL          = L_TITLE
      CANCEL_DISPLAY = 'X'
    IMPORTING
      ANSWER         = P_ANSWER.

ENDFORM.                    " POP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA .

  CLEAR : IT_T604,  IT_T604[],
          IT_T604T, IT_T604T[].

  IF NOT DEL_ITAB[] IS INITIAL.
    LOOP AT DEL_ITAB.
      MOVE-CORRESPONDING : DEL_ITAB TO IT_T604,
                           DEL_ITAB TO IT_T604T.
      APPEND IT_T604.  CLEAR IT_T604.

      MOVE : 'E' TO IT_T604T-SPRAS.
      APPEND IT_T604T. CLEAR IT_T604T.
      CLEAR DEL_ITAB.
    ENDLOOP.

    DELETE T604 FROM TABLE IT_T604.
    COMMIT WORK AND WAIT.

    DELETE T604T FROM TABLE IT_T604T.
    COMMIT WORK AND WAIT.
  ENDIF.

  CLEAR : IT_T604,  IT_T604[],
          IT_T604T, IT_T604T[].

  LOOP AT IT_ITAB.
    MOVE-CORRESPONDING : IT_ITAB TO IT_T604,
                         IT_ITAB TO IT_T604T.
    APPEND IT_T604.  CLEAR IT_T604.

    MOVE : 'E' TO IT_T604T-SPRAS.
    APPEND IT_T604T. CLEAR IT_T604T.
    CLEAR IT_ITAB.
  ENDLOOP.

  MODIFY T604 FROM TABLE IT_T604.
  COMMIT WORK AND WAIT.

  MODIFY T604T FROM TABLE IT_T604T.
  COMMIT WORK AND WAIT.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Module  CHECK_LAND1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_LAND1 INPUT.
  DATA : L_LAND1 LIKE T005-LAND1,
         L_TEXT(100).
  SELECT SINGLE LAND1 INTO L_LAND1
  FROM T005
  WHERE LAND1 EQ ST_ITAB-LAND1.

  IF SY-SUBRC NE 0.
    CONCATENATE 'Entry ' ST_ITAB-LAND1
                'does not exist in T005'
                '(Check entry)'
                INTO L_TEXT
                SEPARATED BY SPACE.
    MESSAGE E000 WITH L_TEXT.
    STOP.
  ENDIF.
ENDMODULE.                 " CHECK_LAND1  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_CURSOR OUTPUT.
  SET CURSOR FIELD V_FIELD LINE V_LINE.
ENDMODULE.                 " SET_CURSOR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CURSOR INPUT.
  GET CURSOR FIELD V_FIELD LINE V_LINE.
ENDMODULE.                 " GET_CURSOR  INPUT
