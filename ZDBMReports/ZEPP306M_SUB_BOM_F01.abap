*----------------------------------------------------------------------*
*   INCLUDE ZEPP306M_SUB_BOM_F01                                       *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_ZTBM_SUB_BOM_VEL
*&---------------------------------------------------------------------*
FORM READ_ZTBM_SUB_BOM_VEL.
  DATA: L_WERKS TYPE T001W-WERKS,
        L_MATNR TYPE MARA-MATNR,
        L_MTART TYPE MARA-MTART.

  REFRESH IT_UPGV. CLEAR IT_UPGV.
  REFRESH IT_BOM_VEL. CLEAR IT_BOM_VEL.
  CASE ZMODE.
    WHEN 'CHANGE'.
      CONCATENATE: T001W-WERKS '%' INTO L_WERKS,
                   MARA-MATNR  '%' INTO L_MATNR,
                   MARA-MTART  '%' INTO L_MTART.
    WHEN 'CREATE'.
      L_WERKS = T001W-WERKS.
      L_MATNR = MARA-MATNR.
      L_MTART = MARA-MTART.
  ENDCASE.
  SELECT *
       FROM ZTBM_SUB_BOM_VEL
       INTO TABLE IT_BOM_VEL
       WHERE WERKS LIKE L_WERKS
       AND   MATNR LIKE L_MATNR
       AND   MTART LIKE L_MTART.
  IF SY-SUBRC EQ 0.
    LOOP AT IT_BOM_VEL.
      MOVE-CORRESPONDING IT_BOM_VEL TO IT_UPGV.
      IF IT_UPGV-Z_CAR IS INITIAL.
        IT_UPGV-Z_CAR = '*'.
      ENDIF.
      IF IT_UPGV-Z_YEAR IS INITIAL.
        IT_UPGV-Z_YEAR = '*'.
      ENDIF.
      IF IT_UPGV-Z_NATION IS INITIAL.
        IT_UPGV-Z_NATION = '*'.
      ENDIF.
      IF IT_UPGV-Z_BT IS INITIAL.
        IT_UPGV-Z_BT = '*'.
      ENDIF.
      IF IT_UPGV-Z_TL IS INITIAL.
        IT_UPGV-Z_TL = '*'.
      ENDIF.
      IF IT_UPGV-Z_EC IS INITIAL.
        IT_UPGV-Z_EC = '*'.
      ENDIF.
      IF IT_UPGV-Z_FT IS INITIAL.
        IT_UPGV-Z_FT = '*'.
      ENDIF.
      IF IT_UPGV-Z_TM IS INITIAL.
        IT_UPGV-Z_TM = '*'.
      ENDIF.
      IF IT_UPGV-Z_ST IS INITIAL.
        IT_UPGV-Z_ST = '*'.
      ENDIF.
      IF IT_UPGV-Z_COLOR IS INITIAL.
        IT_UPGV-Z_COLOR = '*'.
      ENDIF.
      APPEND IT_UPGV.
      CLEAR: IT_BOM_VEL, IT_UPGV.
    ENDLOOP.
    Z_FLAG = 'X'.
  ELSE.
    Z_FLAG = ' '.
  ENDIF.
ENDFORM.                    " READ_ZTBM_SUB_BOM_VEL
*&---------------------------------------------------------------------*
*&      Form  SAVE_PROCESS
*&---------------------------------------------------------------------*
FORM SAVE_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX,
        L_CHK.
*  SORT IT_BOM_VEL BY WERKS MATNR.
*  LOOP AT IT_UPGV.
*    READ TABLE IT_BOM_VEL WITH KEY WERKS = IT_UPGV-WERKS
*                                   MATNR = IT_UPGV-MATNR
*                          BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      L_TABIX = SY-TABIX.
*      MOVE-CORRESPONDING IT_UPGV TO IT_BOM_VEL.
*      MODIFY IT_BOM_VEL INDEX L_TABIX.
*    ELSE.
*      MOVE-CORRESPONDING IT_UPGV TO IT_BOM_VEL.
*      APPEND IT_BOM_VEL.
*    ENDIF.
*  ENDLOOP.
  PERFORM CHECK_IT_UPGV_TO_IT_BOM_VEL CHANGING L_CHK.
  IF L_CHK EQ 'X'.
    WA_TITLE = 'There is amendment'.
    WA_TEXT1 = 'Do you Save?'.

    PERFORM POPUP_TO_CONFIRM_STEP USING WA_TITLE
                                        WA_TEXT1
                                        WA_TEXT2
                                  CHANGING WA_ANS.
    IF WA_ANS EQ 'J'.
      PERFORM INSERT_ZTBM_SUB_BOM_VEL.
      LEAVE PROGRAM.
    ELSEIF WA_ANS EQ 'N'.
      LEAVE TO SCREEN 0.
    ELSEIF WA_ANS EQ 'A'.

    ENDIF.
  ENDIF.
*  PERFORM READ_ZTBM_SUB_BOM_VEL.

ENDFORM.                    " SAVE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_MARA_MTART
*&---------------------------------------------------------------------*
FORM READ_MARA_MTART.
  DATA: L_TABIX TYPE SY-TABIX.
  LOOP AT IT_UPGV.
    L_TABIX = SY-TABIX.
    IF IT_UPGV-MTART IS INITIAL.
      IF NOT IT_UPGV-MATNR IS INITIAL.
        SELECT SINGLE MTART
                    FROM MARA
                    INTO IT_UPGV-MTART
                    WHERE MATNR EQ IT_UPGV-MATNR.
        IF SY-SUBRC EQ 0.
          MODIFY IT_UPGV INDEX L_TABIX TRANSPORTING MTART.
        ENDIF.
      ENDIF.
    ENDIF.
*    IF IT_UPGV-WERKS IS INITIAL.
*      IF NOT IT_UPGV-MATNR IS INITIAL.
*        SELECT SINGLE WERKS
*                    FROM MARC
*                    INTO IT_UPGV-WERKS
*                    WHERE MATNR EQ IT_UPGV-MATNR.
*        IF SY-SUBRC EQ 0.
*          MODIFY IT_UPGV INDEX L_TABIX TRANSPORTING WERKS.
*        ELSE.
*          MESSAGE S000 WITH 'NO PLANT DELETE ITEM'.
*        ENDIF.
*      ENDIF.
*    ELSE.
*      IF NOT IT_UPGV-MATNR IS INITIAL.
*        SELECT SINGLE WERKS
*                    FROM MARC
*                    INTO IT_UPGV-WERKS
*                    WHERE MATNR EQ IT_UPGV-MATNR
*                    AND   WERKS EQ IT_UPGV-WERKS.
*        IF SY-SUBRC NE 0.
*          MESSAGE S000 WITH 'NO PLANT'.
*        ENDIF.
*      ENDIF.
*    ENDIF.
  ENDLOOP.

  DELETE IT_UPGV WHERE MATNR IS INITIAL
                 AND   WERKS IS INITIAL.
ENDFORM.                    " READ_MARA_MTART
*&---------------------------------------------------------------------*
*&      Form  CHECK_IT_UPGV_TO_IT_BOM_VEL
*&---------------------------------------------------------------------*
FORM CHECK_IT_UPGV_TO_IT_BOM_VEL CHANGING P_CHK.
  DATA: L_TABIX TYPE SY-TABIX,
        L_SEQU  TYPE ZTBM_SUB_BOM_VEL-SEQU.
  REFRESH IT_BOM_DEL. CLEAR IT_BOM_DEL.

  SORT IT_UPGV BY WERKS MATNR MTART SEQU Z_NATION.
  SORT IT_BOM_VEL BY WERKS MATNR MTART SEQU Z_NATION.
  LOOP AT IT_UPGV.
    READ TABLE IT_BOM_VEL WITH KEY WERKS = IT_UPGV-WERKS
                                   MATNR = IT_UPGV-MATNR
                                   MTART = IT_UPGV-MTART
                                   SEQU  = IT_UPGV-SEQU
                                Z_NATION = IT_UPGV-Z_NATION
                          BINARY SEARCH.
    IF SY-SUBRC EQ 0.

      L_TABIX = SY-TABIX.

      IF   IT_UPGV-Z_CAR     NE IT_BOM_VEL-Z_CAR
        OR IT_UPGV-Z_YEAR    NE IT_BOM_VEL-Z_YEAR
*        OR IT_UPGV-Z_NATION  NE IT_BOM_VEL-Z_NATION
        OR IT_UPGV-Z_BT      NE IT_BOM_VEL-Z_BT
        OR IT_UPGV-Z_TL      NE IT_BOM_VEL-Z_TL
        OR IT_UPGV-Z_EC      NE IT_BOM_VEL-Z_EC
        OR IT_UPGV-Z_FT      NE IT_BOM_VEL-Z_FT
        OR IT_UPGV-Z_TM      NE IT_BOM_VEL-Z_TM
        OR IT_UPGV-Z_ST      NE IT_BOM_VEL-Z_ST
        OR IT_UPGV-Z_COLOR   NE IT_BOM_VEL-Z_COLOR.
        P_CHK = 'X'.
        MOVE-CORRESPONDING IT_UPGV TO IT_BOM_VEL.
*        MODIFY IT_BOM_VEL INDEX L_TABIX.
        SELECT MAX( SEQU )
             FROM ZTBM_SUB_BOM_VEL
             INTO L_SEQU
             WHERE MATNR EQ IT_UPGV-MATNR
             AND   WERKS EQ IT_UPGV-WERKS
             AND   MTART EQ IT_UPGV-MTART
             AND   Z_NATION EQ IT_UPGV-Z_NATION.
        IF SY-SUBRC EQ 0.
          IT_BOM_VEL-SEQU = L_SEQU + 1.
          CLEAR L_SEQU.
        ENDIF.
        INSERT IT_BOM_VEL INDEX L_TABIX.


      ENDIF.
    ELSE.
      P_CHK = 'X'.
      MOVE-CORRESPONDING IT_UPGV TO IT_BOM_VEL.
      SELECT SINGLE MAX( SEQU )
                 FROM ZTBM_SUB_BOM_VEL
                 INTO IT_BOM_VEL-SEQU
                 WHERE MATNR EQ IT_UPGV-MATNR
                 AND   WERKS EQ IT_UPGV-WERKS
                 AND   MTART EQ IT_UPGV-MTART
                 AND   Z_NATION EQ IT_UPGV-Z_NATION.
      IF SY-SUBRC EQ 0.
        IT_BOM_VEL-SEQU = IT_BOM_VEL-SEQU + 1.
      ELSE.
        IT_BOM_VEL-SEQU = '0001'.
      ENDIF.
      APPEND IT_BOM_VEL.
    ENDIF.
    CLEAR: IT_UPGV, IT_BOM_VEL.
  ENDLOOP.

  SORT IT_UPGV BY WERKS MATNR MTART SEQU Z_NATION.
  SORT IT_BOM_VEL BY WERKS MATNR MTART SEQU Z_NATION.

  LOOP AT IT_BOM_VEL.
    READ TABLE IT_UPGV WITH KEY WERKS = IT_BOM_VEL-WERKS
                                MATNR = IT_BOM_VEL-MATNR
                                MTART = IT_BOM_VEL-MTART
                                SEQU  = IT_BOM_VEL-SEQU
                             Z_NATION = IT_BOM_VEL-Z_NATION
                          BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      L_TABIX = SY-TABIX.

      IF   IT_UPGV-Z_CAR     NE IT_BOM_VEL-Z_CAR
        OR IT_UPGV-Z_YEAR    NE IT_BOM_VEL-Z_YEAR
*        OR IT_UPGV-Z_NATION  NE IT_BOM_VEL-Z_NATION
        OR IT_UPGV-Z_BT      NE IT_BOM_VEL-Z_BT
        OR IT_UPGV-Z_TL      NE IT_BOM_VEL-Z_TL
        OR IT_UPGV-Z_EC      NE IT_BOM_VEL-Z_EC
        OR IT_UPGV-Z_FT      NE IT_BOM_VEL-Z_FT
        OR IT_UPGV-Z_TM      NE IT_BOM_VEL-Z_TM
        OR IT_UPGV-Z_ST      NE IT_BOM_VEL-Z_ST
        OR IT_UPGV-Z_COLOR   NE IT_BOM_VEL-Z_COLOR.
        P_CHK = 'X'.
*        MOVE-CORRESPONDING IT_UPGV TO IT_BOM_VEL.
*        MODIFY IT_BOM_VEL INDEX L_TABIX.
      ENDIF.
    ELSE.
*      IF IT_BOM_VEL-SEQU NE '0001'.
        P_CHK = 'X'.
        MOVE-CORRESPONDING IT_BOM_VEL TO IT_BOM_DEL.
        APPEND IT_BOM_DEL.
*      ENDIF.
    ENDIF.
    CLEAR: IT_UPGV, IT_BOM_VEL, IT_BOM_DEL.
  ENDLOOP.

ENDFORM.                    " CHECK_IT_UPGV_TO_IT_BOM_VEL
*&---------------------------------------------------------------------*
*&      Form  INSERT_ZTBM_SUB_BOM_VEL
*&---------------------------------------------------------------------*
FORM INSERT_ZTBM_SUB_BOM_VEL.
  DATA L_TABIX LIKE SY-TABIX.
  IF NOT IT_BOM_DEL[] IS INITIAL.
    DELETE ZTBM_SUB_BOM_VEL FROM TABLE IT_BOM_DEL.
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
      CLEAR: IT_BOM_VEL, IT_BOM_DEL.
      LOOP AT IT_BOM_DEL.
        READ TABLE IT_BOM_VEL WITH KEY MATNR = IT_BOM_DEL-MATNR
                                       WERKS = IT_BOM_DEL-WERKS
                                       SEQU  = IT_BOM_DEL-SEQU.
        IF SY-SUBRC EQ 0.
          L_TABIX = SY-TABIX.
          DELETE IT_BOM_VEL INDEX L_TABIX.
        ENDIF.
        CLEAR: IT_BOM_VEL, IT_BOM_DEL.
      ENDLOOP.
    ENDIF.
  ENDIF.
  MODIFY ZTBM_SUB_BOM_VEL FROM TABLE IT_BOM_VEL.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    MESSAGE S001 WITH 'MODIFY SUCCESS ZTBM_SUB_BOM_VEL'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE S001 WITH 'ERROR MODIFY ZTBM_SUB_BOM_VEL'.
  ENDIF.
ENDFORM.                    " INSERT_ZTBM_SUB_BOM_VEL
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM_STEP
*&---------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM_STEP USING    P_TITLE
                                    P_TEXT1
                                    P_TEXT2
                           CHANGING P_ANS.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
*           DEFAULTOPTION  = 'Y'
            TEXTLINE1      = P_TEXT1
            TEXTLINE2      = P_TEXT2
            TITEL          = P_TITLE
*           START_COLUMN   = 25
*           START_ROW      = 6
*           CANCEL_DISPLAY = 'X'
      IMPORTING
           ANSWER         = P_ANS.
ENDFORM.                    " POPUP_TO_CONFIRM_STEP
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_8000 INPUT.
  CASE 'X'.
    WHEN CREATE_MODE.
      PERFORM CREATE_MODE.
    WHEN CHANGE_MODE.
      PERFORM CHANGE_MODE.
    WHEN DELETE_MODE.
      PERFORM DELETE_MODE.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_8000  INPUT
*&---------------------------------------------------------------------*
*&      Form  DELETE_ZTBM_SUB_BOM_VEL
*&---------------------------------------------------------------------*
FORM DELETE_ZTBM_SUB_BOM_VEL.
  SELECT *
       FROM ZTBM_SUB_BOM_VEL
       INTO TABLE IT_BOM_VEL
              WHERE MATNR EQ '8983039710'.
  IF SY-SUBRC EQ 0.
    DELETE ZTBM_SUB_BOM_VEL FROM TABLE IT_BOM_VEL.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " DELETE_ZTBM_SUB_BOM_VEL
*&---------------------------------------------------------------------*
*&      Form  CREATE_MODE
*&---------------------------------------------------------------------*
FORM CREATE_MODE.
  ZMODE = 'CREATE'.
  REFRESH IT_UPGV_CHK. CLEAR IT_UPGV_CHK.
  PERFORM READ_ZTBM_SUB_BOM_VEL.
  IF Z_FLAG EQ ' '.
    IT_UPGV_CHK[] = IT_UPGV[].
    DESCRIBE TABLE IT_UPGV LINES T_9000-LINES.
    T_9000-TOP_LINE = 1.
    CALL SCREEN 9000.
  ELSE.
    MESSAGE S000 WITH 'Material already maintained .'.

    WA_TITLE = 'Material already maintained .'.
    WA_TEXT1 = 'Do you process in alter mode?'.

    PERFORM POPUP_TO_CONFIRM_STEP USING WA_TITLE
                                        WA_TEXT1
                                        WA_TEXT2
                                  CHANGING WA_ANS.
    IF WA_ANS EQ 'J'.
      ZMODE = 'CHANGE'.
      CALL SCREEN 9000.
    ELSE.

    ENDIF.
  ENDIF.
ENDFORM.                    " CREATE_MODE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_MODE
*&---------------------------------------------------------------------*
FORM CHANGE_MODE.
  ZMODE = 'CHANGE'.
  REFRESH IT_UPGV_CHK. CLEAR IT_UPGV_CHK.
  PERFORM READ_ZTBM_SUB_BOM_VEL.
  IF Z_FLAG EQ 'X'.
    IT_UPGV_CHK[] = IT_UPGV[].
    DESCRIBE TABLE IT_UPGV LINES T_9000-LINES.
    T_9000-TOP_LINE = 1.
    CALL SCREEN 9000.
  ELSE.
    MESSAGE S000 WITH 'NO DATA'.
    WA_TITLE = 'NO DATA'.
    WA_TEXT1 = 'Do you process in creation mode?'.

    PERFORM POPUP_TO_CONFIRM_STEP USING WA_TITLE
                                        WA_TEXT1
                                        WA_TEXT2
                                  CHANGING WA_ANS.
    IF WA_ANS EQ 'J'.
      ZMODE = 'CREATE'.
      CALL SCREEN 9000.
    ELSE.

    ENDIF.
  ENDIF.
ENDFORM.                    " CHANGE_MODE
*&---------------------------------------------------------------------*
*&      Form  DELETE_MODE
*&---------------------------------------------------------------------*
FORM DELETE_MODE.
  ZMODE = 'DELETE'.
*  DATA IT_TTT LIKE ZTBM_SUB_BOM_VEL OCCURS 0 WITH HEADER LINE.
*  SELECT *
*       FROM ZTBM_SUB_BOM_VEL
*       INTO TABLE IT_TTT
*       WHERE MATNR EQ '8983039710'.
*  DELETE ZTBM_SUB_BOM_VEL FROM TABLE IT_TTT.
  PERFORM DELETE_ZTBM_SUB_BOM_VEL.
ENDFORM.                    " DELETE_MODE
*&---------------------------------------------------------------------*
*&      Form  CREATE_MODEL_VALUE
*&---------------------------------------------------------------------*
FORM CREATE_MODEL_VALUE.
  DATA: L_FLAG.
  REFRESH: IT_MODL, IT_MODL_VAL. CLEAR: IT_MODL, IT_MODL_VAL.
  CASE WA_FIELD+16(10).
    WHEN 'Z_CAR'.
      WA_ZFIELD = '01'.
      WA_ZFDESC = 'MODEL TYPE'.
      L_FLAG = 'X'.
    WHEN 'Z_YEAR'.
      WA_ZFIELD = '02'.
      WA_ZFDESC = 'MODEL YEAR'.
      L_FLAG = 'X'.
    WHEN 'Z_NATION'.
      WA_ZFIELD = '03'.
      WA_ZFDESC = 'NATION'.
      L_FLAG = 'X'.
    WHEN 'Z_BT'.
      WA_ZFIELD = '04'.
      WA_ZFDESC = 'B/T'.
      L_FLAG = 'X'.
    WHEN 'Z_TL'.
      WA_ZFIELD = '05'.
      WA_ZFDESC = 'T/L'.
      L_FLAG = 'X'.
    WHEN 'Z_EC'.
      WA_ZFIELD = '06'.
      WA_ZFDESC = 'E/C'.
      L_FLAG = 'X'.
    WHEN 'Z_FT'.
      WA_ZFIELD = '07'.
      WA_ZFDESC = 'F/T'.
      L_FLAG = 'X'.
    WHEN 'Z_TM'.
      WA_ZFIELD = '08'.
      WA_ZFDESC = 'T/M'.
      L_FLAG = 'X'.
    WHEN 'Z_ST'.
      WA_ZFIELD = '09'.
      WA_ZFDESC = 'S/T'.
      L_FLAG = 'X'.
    WHEN 'Z_COLOR'.
      WA_ZFIELD = '10'.
      WA_ZFDESC = 'COLOR'.
      L_FLAG = 'X'.
    WHEN 'Z_REGI'.
      WA_ZFIELD = '11'.
      WA_ZFDESC = 'REGION'.
      L_FLAG = 'X'.
  ENDCASE.
  IF L_FLAG EQ 'X'.
    PERFORM READ_ZTBM_MODEL_VAL USING WA_ZFIELD
                                      WA_ZFDESC.
    DESCRIBE TABLE IT_MODL_VAL LINES WA_LINES.
    T_9100-TOP_LINE = 1.
    T_9100-LINES = WA_LINES.
    CALL SCREEN 9100.
  ENDIF.
ENDFORM.                    " CREATE_MODEL_VALUE
*&---------------------------------------------------------------------*
*&      Form  READ_ZTBM_MODEL_VAL
*&---------------------------------------------------------------------*
FORM READ_ZTBM_MODEL_VAL USING    P_VALUE
                                  P_FDESC.

  SELECT *
       FROM ZTBM_MODEL_VAL
       INTO TABLE IT_MODL_VAL
       WHERE ZFIELD EQ P_VALUE.
  IF SY-SUBRC EQ 0.
    IT_MODL[] = IT_MODL_VAL[].
  ELSE.
    IT_MODL_VAL-MANDT = SY-MANDT.
    IT_MODL_VAL-ZFIELD = P_VALUE.
*    IT_MODL_VAL-ZFDESC = P_FDESC.
    DO 16 TIMES.
      APPEND IT_MODL_VAL.
    ENDDO.
    IT_MODL[] = IT_MODL_VAL[].
  ENDIF.
ENDFORM.                    " READ_ZTBM_MODEL_VAL
*&---------------------------------------------------------------------*
*&      Form  LINE_INSERT
*&---------------------------------------------------------------------*
FORM LINE_INSERT.
  DATA: L_TABIX LIKE SY-TABIX,
        LA_MODL LIKE IT_MODL_VAL.
  CASE OKCODE+5(1).
    WHEN 'D'.
      L_TABIX = T_9100-TOP_LINE.
      GET CURSOR FIELD WA_ZFIELD1
                 LINE  WA_CURSOR_LINE.
      L_TABIX = L_TABIX + WA_CURSOR_LINE - 1.
      READ TABLE IT_MODL_VAL INDEX L_TABIX.
      IF SY-SUBRC EQ 0.
        L_TABIX = SY-TABIX.
        IF L_TABIX GE 1.
          DELETE IT_MODL_VAL INDEX L_TABIX.
        ENDIF.
      ENDIF.
    WHEN 'R'.
      L_TABIX = T_9100-TOP_LINE.
      GET CURSOR FIELD WA_ZFIELD1
                 LINE  WA_CURSOR_LINE.
      L_TABIX = L_TABIX + WA_CURSOR_LINE - 1.
      LA_MODL-MANDT = SY-MANDT.
      LA_MODL-ZFIELD = WA_ZFIELD.
*      LA_MODL-ZFDESC = WA_ZFDESC.
      IF L_TABIX GE 1.
        INSERT LA_MODL INTO IT_MODL_VAL INDEX L_TABIX.
      ENDIF.
    WHEN 'I'.
      DO 16 TIMES.
        LA_MODL-MANDT = SY-MANDT.
        LA_MODL-ZFIELD = WA_ZFIELD.
*        LA_MODL-ZFDESC = WA_ZFDESC.
        APPEND LA_MODL TO IT_MODL_VAL.
      ENDDO.
      DESCRIBE TABLE IT_MODL_VAL LINES L_TABIX.
      T_9100-LINES = L_TABIX.
      WA_ZFIELD1 = 'ZTBM_MODEL_VAL-ZVALUE'.
      WA_CURSOR_LINE = T_9100-TOP_LINE = L_TABIX - 16.
  ENDCASE.
  DESCRIBE TABLE IT_MODL_VAL LINES L_TABIX.
  T_9100-LINES = L_TABIX.
ENDFORM.                    " LINE_INSERT
*&---------------------------------------------------------------------*
*&      Form  SAVE_ZTBM_MODEL_VAL
*&---------------------------------------------------------------------*
FORM SAVE_ZTBM_MODEL_VAL.
  DATA: L_TABIX TYPE SY-TABIX.
  REFRESH IT_MODL_DEL. CLEAR IT_MODL_DEL.
  SORT IT_MODL_VAL BY ZFIELD ZVALUE.
  SORT IT_MODL BY ZFIELD ZVALUE.
  LOOP AT IT_MODL_VAL.
    READ TABLE IT_MODL WITH KEY ZFIELD = IT_MODL_VAL-ZFIELD
                                ZVALUE = IT_MODL_VAL-ZVALUE.
    IF SY-SUBRC EQ 0.
      L_TABIX = SY-TABIX.
      IT_MODL = IT_MODL_VAL.
      MODIFY IT_MODL INDEX L_TABIX.
    ELSE.
      IT_MODL = IT_MODL_VAL.
      APPEND IT_MODL.
    ENDIF.
    CLEAR: IT_MODL, IT_MODL_VAL.
  ENDLOOP.

  LOOP AT IT_MODL.
    L_TABIX = SY-TABIX.
    READ TABLE IT_MODL_VAL WITH KEY ZFIELD = IT_MODL-ZFIELD
                                    ZVALUE = IT_MODL-ZVALUE.
    IF SY-SUBRC NE 0.
      IT_MODL_DEL = IT_MODL.
      APPEND IT_MODL_DEL.

      DELETE IT_MODL INDEX L_TABIX.
    ENDIF.
    CLEAR: IT_MODL, IT_MODL_DEL, IT_MODL_VAL.
  ENDLOOP.

ENDFORM.                    " SAVE_ZTBM_MODEL_VAL
*&---------------------------------------------------------------------*
*&      Form  MODIFY_ZTBM_MODEL_VAL
*&---------------------------------------------------------------------*
FORM MODIFY_ZTBM_MODEL_VAL.
  MODIFY ZTBM_MODEL_VAL FROM TABLE IT_MODL.
  IF SY-SUBRC EQ 0.
    DELETE ZTBM_MODEL_VAL FROM TABLE IT_MODL_DEL.
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  REFRESH: IT_MODL_VAL, IT_MODL_DEL, IT_MODL.
  CLEAR: IT_MODL_VAL, IT_MODL_DEL, IT_MODL.
  LEAVE TO SCREEN 0.
ENDFORM.                    " MODIFY_ZTBM_MODEL_VAL
*&---------------------------------------------------------------------*
*&      Form  SEARCH_HELP
*&---------------------------------------------------------------------*
FORM SEARCH_HELP USING    P_VALUE
                          P_FIELD.
  DATA : BEGIN OF LT_MODL OCCURS 0,
        ZFIELD TYPE ZTBM_MODEL_VAL-ZFIELD,
        ZVALUE TYPE ZTBM_MODEL_VAL-ZVALUE,
        ZVALNM TYPE ZTBM_MODEL_VAL-ZVALNM,
*        ZDESCR TYPE ZTBM_MODEL_VAL-ZDESCR,
        ZFDESC TYPE ZTBM_MODEL_VAL-ZFDESC,
        END OF LT_MODL.

  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.

*  PERFORM VALUE_READ USING: 'P_MATNR'.
*  LOOP AT DYNPREAD.
*    CASE SY-TABIX.
*      WHEN 1. P_MATNR = DYNPREAD-FIELDVALUE.
*    ENDCASE.
*  ENDLOOP.
*  CLEAR DYNPREAD. REFRESH DYNPREAD.
*  CLEAR VALUETAB. REFRESH VALUETAB.
*  CLEAR FIELDS.   REFRESH FIELDS.
*  PERFORM VALUE_READ USING: 'P_WERKS'.
*  LOOP AT DYNPREAD.
*    CASE SY-TABIX.
*      WHEN 1. P_WERKS = DYNPREAD-FIELDVALUE.
*    ENDCASE.
*  ENDLOOP.

  SELECT ZFIELD
         ZVALUE
         ZVALNM
*         ZDESCR
         ZFDESC
       FROM  ZTBM_MODEL_VAL
       INTO TABLE LT_MODL
       WHERE  ZFIELD  EQ P_VALUE.


  LOOP AT LT_MODL.
    VALUETAB-VALUE = LT_MODL-ZFIELD.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = LT_MODL-ZVALUE.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = LT_MODL-ZVALNM .
    APPEND VALUETAB. CLEAR VALUETAB.
*    VALUETAB-VALUE = LT_MODL-ZDESCR.
*    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = LT_MODL-ZFDESC.
    APPEND VALUETAB. CLEAR VALUETAB.
  ENDLOOP.

  PERFORM ADD_FIELDS USING: 'ZTBM_MODEL_VAL' 'ZFIELD' ' ',
                            'ZTBM_MODEL_VAL' 'ZVALUE' 'X',
                            'ZTBM_MODEL_VAL' 'ZVALNM' ' ',
*                            'ZTBM_MODEL_VAL' 'ZDESCR' ' ',
                            'ZTBM_MODEL_VAL' 'ZFDESC' ' '.
  PERFORM HELP_VALUES_GET.


  IF SELECT_INDEX > 0.
    READ TABLE LT_MODL   INDEX SELECT_INDEX.

    PERFORM VALUE_UPDATE USING:
            'X'   P_FIELD LT_MODL-ZVALUE 0.
  ENDIF.
ENDFORM.                    " SEARCH_HELP
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
*&      Form  FIELD_VALUE_INITIAL_CHECK
*&---------------------------------------------------------------------*
FORM FIELD_VALUE_INITIAL_CHECK.
  IF  IT_UPGV-Z_CAR IS INITIAL.
    IT_UPGV-Z_CAR = '*'.
  ENDIF.
  IF  IT_UPGV-Z_YEAR IS INITIAL.
    IT_UPGV-Z_YEAR = '*'.
  ENDIF.
  IF  IT_UPGV-Z_NATION IS INITIAL.
    IT_UPGV-Z_NATION = '*'.
  ENDIF.
  IF  IT_UPGV-Z_BT IS INITIAL.
    IT_UPGV-Z_BT = '*'.
  ENDIF.
  IF  IT_UPGV-Z_TL IS INITIAL.
    IT_UPGV-Z_TL = '*'.
  ENDIF.
  IF  IT_UPGV-Z_EC IS INITIAL.
    IT_UPGV-Z_EC = '*'.
  ENDIF.
  IF  IT_UPGV-Z_FT IS INITIAL.
    IT_UPGV-Z_FT = '*'.
  ENDIF.
  IF  IT_UPGV-Z_TM IS INITIAL.
    IT_UPGV-Z_TM = '*'.
  ENDIF.
  IF  IT_UPGV-Z_ST IS INITIAL.
    IT_UPGV-Z_ST = '*'.
  ENDIF.
  IF  IT_UPGV-Z_COLOR IS INITIAL.
    IT_UPGV-Z_COLOR = '*'.
  ENDIF.
ENDFORM.                    " FIELD_VALUE_INITIAL_CHECK
*&---------------------------------------------------------------------*
*&      Form  FIELD_MATNR
*&---------------------------------------------------------------------*
FORM FIELD_MATNR.
  DATA: L_COUNT TYPE SY-TABIX,
        LT_UPGV LIKE IT_UPGV OCCURS 0 WITH HEADER LINE.
  CLEAR L_COUNT.
  LT_UPGV[] = IT_UPGV[].
  DELETE LT_UPGV INDEX T_9000-CURRENT_LINE.
  IF    NOT ZSBM_MODEL_VALS-WERKS IS INITIAL
    AND NOT ZSBM_MODEL_VALS-MATNR IS INITIAL.
    SELECT SINGLE B~MAKTX
         FROM MARC AS A INNER JOIN MAKT AS B
                        ON A~MATNR EQ B~MATNR
         INTO  ZSBM_MODEL_VALS-MAKTX
         WHERE A~WERKS EQ ZSBM_MODEL_VALS-WERKS
         AND   A~MATNR EQ ZSBM_MODEL_VALS-MATNR.
    IF SY-SUBRC EQ 0.
      LOOP AT LT_UPGV WHERE WERKS EQ ZSBM_MODEL_VALS-WERKS
                      AND   MATNR EQ ZSBM_MODEL_VALS-MATNR.
        L_COUNT = L_COUNT + 1.
      ENDLOOP.
    ELSE.
      MESSAGE E000 WITH 'MATERIAL DOES NOT AT PLANT'.
    ENDIF.
  ELSE.
    IF NOT ZSBM_MODEL_VALS-MATNR IS INITIAL.
      IF ZSBM_MODEL_VALS-WERKS IS INITIAL.
        MESSAGE E000 WITH 'Plant Essential Input Data.'.
      ENDIF.
    ENDIF.
  ENDIF.
  IF L_COUNT GE 1.
    CLEAR L_COUNT.
    MESSAGE E000 WITH 'SAME DATA'.
  ELSE.
  ENDIF.
  IF CREATE_MODE EQ 'X'.

    PERFORM CHECK_MATERIAL_PLANT.
  ENDIF.
ENDFORM.                    " FIELD_MATNR
*&---------------------------------------------------------------------*
*&      Form  FIELD_ZVALUE
*&---------------------------------------------------------------------*
FORM FIELD_ZVALUE.
  DATA: L_COUNT TYPE SY-TABIX,
        LT_MODL_VAL LIKE IT_MODL_VAL OCCURS 0 WITH HEADER LINE.
  LT_MODL_VAL[] = IT_MODL_VAL[].
  DELETE LT_MODL_VAL INDEX T_9100-CURRENT_LINE.
  IF    NOT ZTBM_MODEL_VAL-ZFIELD IS INITIAL
    AND NOT ZTBM_MODEL_VAL-ZVALUE IS INITIAL.
    LOOP AT LT_MODL_VAL WHERE ZFIELD EQ ZTBM_MODEL_VAL-ZFIELD
                        AND   ZVALUE EQ ZTBM_MODEL_VAL-ZVALUE.
      L_COUNT = L_COUNT + 1.
    ENDLOOP.
  ENDIF.
  IF L_COUNT GE 1.
    CLEAR L_COUNT.
    MESSAGE E000 WITH 'SAME DATA'.
  ENDIF.
ENDFORM.                    " FIELD_ZVALUE
*&---------------------------------------------------------------------*
*&      Form  FIELD_VALUE
*&---------------------------------------------------------------------*
FORM FIELD_VALUE.
  DATA: L_ZFIELD(02) TYPE N, " LIKE ZTBM_MODEL_VAL-ZFIELD,
        L_ZVALUE LIKE ZTBM_MODEL_VAL-ZVALUE,
        L_CHK.
  IF NOT ZSBM_MODEL_VALS-Z_CAR IS INITIAL.
    L_ZFIELD =   01.
    L_ZVALUE =  ZSBM_MODEL_VALS-Z_CAR.
    PERFORM READ_ZTBM_MODEL_VAL_CHECK USING    L_ZFIELD
                                               L_ZVALUE
                                      CHANGING L_CHK.
    IF L_CHK EQ 'X'.
      CLEAR L_CHK.
      MESSAGE E000 WITH 'MODEL TYPE : ' L_ZFIELD L_ZVALUE
                         'NO DATA : CHARACTERISTIC CREATE'.
    ENDIF.
  ENDIF.
  IF NOT ZSBM_MODEL_VALS-Z_YEAR IS INITIAL.
    L_ZFIELD =   02.
    L_ZVALUE =  ZSBM_MODEL_VALS-Z_YEAR.
    PERFORM READ_ZTBM_MODEL_VAL_CHECK USING    L_ZFIELD
                                               L_ZVALUE
                                      CHANGING L_CHK.
    IF L_CHK EQ 'X'.
      CLEAR L_CHK.
      MESSAGE E000 WITH 'MODEL YEAR : ' L_ZFIELD L_ZVALUE
                         'NO DATA : CHARACTERISTIC CREATE'.
    ENDIF.
  ENDIF.
  IF NOT ZSBM_MODEL_VALS-Z_NATION IS INITIAL.
    L_ZFIELD =   03.
    L_ZVALUE =  ZSBM_MODEL_VALS-Z_NATION.
    PERFORM READ_ZTBM_MODEL_VAL_CHECK USING    L_ZFIELD
                                               L_ZVALUE
                                      CHANGING L_CHK.
    IF L_CHK EQ 'X'.
      CLEAR L_CHK.
      MESSAGE E000 WITH 'NATION : ' L_ZFIELD L_ZVALUE
                         'NO DATA : CHARACTERISTIC CREATE'.
    ENDIF.
  ENDIF.
  IF NOT ZSBM_MODEL_VALS-Z_BT IS INITIAL.
    L_ZFIELD =   04.
    L_ZVALUE =  ZSBM_MODEL_VALS-Z_BT.
    PERFORM READ_ZTBM_MODEL_VAL_CHECK USING    L_ZFIELD
                                               L_ZVALUE
                                      CHANGING L_CHK.
    IF L_CHK EQ 'X'.
      CLEAR L_CHK.
      MESSAGE E000 WITH 'B/T : ' L_ZFIELD L_ZVALUE
                         'NO DATA : CHARACTERISTIC CREATE'.
    ENDIF.
  ENDIF.
  IF NOT ZSBM_MODEL_VALS-Z_TL IS INITIAL.
    L_ZFIELD =   05.
    L_ZVALUE =  ZSBM_MODEL_VALS-Z_TL.
    PERFORM READ_ZTBM_MODEL_VAL_CHECK USING    L_ZFIELD
                                               L_ZVALUE
                                      CHANGING L_CHK.
    IF L_CHK EQ 'X'.
      CLEAR L_CHK.
      MESSAGE E000 WITH 'T/L : ' L_ZFIELD L_ZVALUE
                        'NO DATA : CHARACTERISTIC CREATE'.
    ENDIF.
  ENDIF.
  IF NOT ZSBM_MODEL_VALS-Z_EC IS INITIAL.
    L_ZFIELD =   06.
    L_ZVALUE =  ZSBM_MODEL_VALS-Z_EC.
    PERFORM READ_ZTBM_MODEL_VAL_CHECK USING    L_ZFIELD
                                              L_ZVALUE
                                     CHANGING L_CHK.
    IF L_CHK EQ 'X'.
      CLEAR L_CHK.
      MESSAGE E000 WITH 'E/C : ' L_ZFIELD L_ZVALUE
                        'NO DATA : CHARACTERISTIC CREATE'.
    ENDIF.
  ENDIF.
  IF NOT ZSBM_MODEL_VALS-Z_FT IS INITIAL.
    L_ZFIELD =   07.
    L_ZVALUE =  ZSBM_MODEL_VALS-Z_FT.
    PERFORM READ_ZTBM_MODEL_VAL_CHECK USING    L_ZFIELD
                                              L_ZVALUE
                                     CHANGING L_CHK.
    IF L_CHK EQ 'X'.
      CLEAR L_CHK.
      MESSAGE E000 WITH 'F/T : ' L_ZFIELD L_ZVALUE
                        'NO DATA : CHARACTERISTIC CREATE'.
    ENDIF.
  ENDIF.
  IF NOT ZSBM_MODEL_VALS-Z_TM IS INITIAL.
    L_ZFIELD =   08.
    L_ZVALUE =  ZSBM_MODEL_VALS-Z_TM.
    PERFORM READ_ZTBM_MODEL_VAL_CHECK USING    L_ZFIELD
                                               L_ZVALUE
                                      CHANGING L_CHK.
    IF L_CHK EQ 'X'.
      CLEAR L_CHK.
      MESSAGE E000 WITH 'T/M : ' L_ZFIELD L_ZVALUE
                        'NO DATA : CHARACTERISTIC CREATE'.
    ENDIF.
  ENDIF.
  IF NOT ZSBM_MODEL_VALS-Z_ST IS INITIAL.
    L_ZFIELD =   09.
    L_ZVALUE =  ZSBM_MODEL_VALS-Z_ST.
    PERFORM READ_ZTBM_MODEL_VAL_CHECK USING    L_ZFIELD
                                               L_ZVALUE
                                      CHANGING L_CHK.
    IF L_CHK EQ 'X'.
      CLEAR L_CHK.
      MESSAGE E000 WITH 'S/T : ' L_ZFIELD L_ZVALUE
                         'NO DATA : CHARACTERISTIC CREATE'.
    ENDIF.
  ENDIF.
  IF NOT ZSBM_MODEL_VALS-Z_COLOR IS INITIAL.
    L_ZFIELD =   10.
    L_ZVALUE =  ZSBM_MODEL_VALS-Z_COLOR.
    PERFORM READ_ZTBM_MODEL_VAL_CHECK USING    L_ZFIELD
                                               L_ZVALUE
                                      CHANGING L_CHK.
    IF L_CHK EQ 'X'.
      CLEAR L_CHK.
      MESSAGE E000 WITH 'COLOR : ' L_ZFIELD L_ZVALUE
                         'NO DATA : CHARACTERISTIC CREATE'.
    ENDIF.
  ENDIF.
ENDFORM.                    " FIELD_VALUE
*&---------------------------------------------------------------------*
*&      Form  READ_ZTBM_MODEL_VAL_CHECK
*&---------------------------------------------------------------------*
FORM READ_ZTBM_MODEL_VAL_CHECK USING    P_ZFIELD
                                        P_ZVALUE
                               CHANGING P_CHK.
  SELECT SINGLE *
              FROM ZTBM_MODEL_VAL
              WHERE ZFIELD EQ P_ZFIELD
              AND   ZVALUE EQ P_ZVALUE.
  IF SY-SUBRC NE 0.
    P_CHK = 'X'.
  ENDIF.

ENDFORM.                    " READ_ZTBM_MODEL_VAL_CHECK
*&---------------------------------------------------------------------*
*&      Form  FIELD_VALUE_CHECK
*&---------------------------------------------------------------------*
FORM FIELD_VALUE_CHECK.
  DATA: L_CHK.
  IF NOT IT_UPGV[] IS INITIAL.
    PERFORM CHECK_IT_UPGV_TO_IT_BOM_VEL CHANGING L_CHK.
    IF L_CHK EQ 'X'.
      WA_TITLE = 'There is amendment'.
      WA_TEXT1 = 'Do you Save?'.

      PERFORM POPUP_TO_CONFIRM_STEP USING WA_TITLE
                                          WA_TEXT1
                                          WA_TEXT2
                                    CHANGING WA_ANS.
      IF WA_ANS EQ 'J'.
        PERFORM INSERT_ZTBM_SUB_BOM_VEL.
      ENDIF.
    ENDIF.
    PERFORM READ_ZTBM_SUB_BOM_VEL.
    DESCRIBE TABLE IT_UPGV LINES T_9000-LINES.
    T_9000-TOP_LINE = 1.

  ELSE.
    PERFORM READ_ZTBM_SUB_BOM_VEL.
    SORT IT_UPGV BY MATNR WERKS Z_NATION SEQU.
    DESCRIBE TABLE IT_UPGV LINES T_9000-LINES.
    T_9000-TOP_LINE = 1.
  ENDIF.
ENDFORM.                    " FIELD_VALUE_CHECK
*&---------------------------------------------------------------------*
*&      Form  DATA_CHECK_SAVE
*&---------------------------------------------------------------------*
FORM DATA_CHECK_SAVE.
  DATA: L_CHK.
  IF    NOT IT_UPGV[] IS INITIAL
     OR NOT IT_BOM_VEL[] IS INITIAL.
    PERFORM CHECK_IT_UPGV_TO_IT_BOM_VEL CHANGING L_CHK.
    IF L_CHK EQ 'X'.
      WA_TITLE = 'There is amendment'.
      WA_TEXT1 = 'Do you Save?'.

      PERFORM POPUP_TO_CONFIRM_STEP USING WA_TITLE
                                          WA_TEXT1
                                          WA_TEXT2
                                    CHANGING WA_ANS.
      IF WA_ANS EQ 'J'.
        CLEAR L_CHK.
        PERFORM INSERT_ZTBM_SUB_BOM_VEL.
      ELSEIF WA_ANS EQ 'A'.

      ELSEIF WA_ANS EQ 'N'.
        LEAVE TO SCREEN 0.
      ENDIF.
    ELSE.
      LEAVE TO SCREEN 0.
    ENDIF.
  ELSE.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.                    " DATA_CHECK_SAVE
*&---------------------------------------------------------------------*
*&      Form  GET_CURSOR
*&---------------------------------------------------------------------*
FORM GET_CURSOR.
  GET CURSOR FIELD WA_ZFIELD1
             LINE WA_CURSOR_LINE.

ENDFORM.                    " GET_CURSOR
*&---------------------------------------------------------------------*
*&      Form  MODIFY_9000
*&---------------------------------------------------------------------*
FORM MODIFY_9000.
  CASE 'X'.
    WHEN CREATE_MODE.
*      PERFORM CHECK_MATERIAL_PLANT.
      MOVE-CORRESPONDING ZSBM_MODEL_VALS TO IT_UPGV.
      PERFORM FIELD_VALUE_INITIAL_CHECK.
      MODIFY IT_UPGV INDEX T_9000-CURRENT_LINE.
      IF SY-SUBRC NE 0.
        APPEND IT_UPGV. " INDEX T_9000-CURRENT_LINE.
      ENDIF.
    WHEN CHANGE_MODE.
      MOVE-CORRESPONDING ZSBM_MODEL_VALS TO IT_UPGV.
      PERFORM FIELD_VALUE_INITIAL_CHECK.
      MODIFY IT_UPGV INDEX T_9000-CURRENT_LINE.
      IF SY-SUBRC NE 0.
        APPEND IT_UPGV. " INDEX T_9000-CURRENT_LINE.
      ENDIF.
  ENDCASE.
ENDFORM.                    " MODIFY_9000
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL_PLANT
*&---------------------------------------------------------------------*
FORM CHECK_MATERIAL_PLANT.
  DATA L_SEQU LIKE ZSBM_MODEL_VALS-SEQU.
  SELECT SINGLE MAX( SEQU )
              FROM ZTBM_SUB_BOM_VEL
              INTO L_SEQU
              WHERE WERKS EQ ZSBM_MODEL_VALS-WERKS
              AND   MATNR EQ ZSBM_MODEL_VALS-MATNR
*              AND   MATNR EQ ZSBM_MODEL_VALS-MATNR
              AND   Z_NATION EQ ZSBM_MODEL_VALS-Z_NATION.
  IF SY-SUBRC EQ 0.
    ZSBM_MODEL_VALS-SEQU = L_SEQU + 1.
*    MESSAGE E000 WITH ZSBM_MODEL_VALS-WERKS
*                      ZSBM_MODEL_VALS-MATNR
**                      ZSBM_MODEL_VALS-MTART
*                      'Material already'.
  ELSE.
    L_SEQU = '0001'.
  ENDIF.
ENDFORM.                    " CHECK_MATERIAL_PLANT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DATA
*&---------------------------------------------------------------------*
FORM REFRESH_DATA.
  REFRESH: IT_MODL, IT_UPGV_CHK, IT_BOM_VEL, IT_BOM_DEL,
           IT_MODL_VAL, IT_UPGV, IT_MODL_DEL.
  CLEAR  : IT_MODL, IT_UPGV_CHK, IT_BOM_VEL, IT_BOM_DEL,
           IT_MODL_VAL, IT_UPGV, IT_MODL_DEL.

ENDFORM.                    " REFRESH_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_MAKT
*&---------------------------------------------------------------------*
FORM READ_MAKT.
  IF ZSBM_MODEL_VALS-MATNR IS INITIAL.
    CLEAR ZSBM_MODEL_VALS-MAKTX.
  ELSE.
    SELECT SINGLE MAKTX
                FROM MAKT
                INTO ZSBM_MODEL_VALS-MAKTX
                WHERE MATNR EQ ZSBM_MODEL_VALS-MATNR.
    IF SY-SUBRC NE 0.
*      ZSBM_MODEL_VALS-MAKTX = L_MAKTX.
*    ELSE.
      CLEAR ZSBM_MODEL_VALS-MAKTX.
    ENDIF.
  ENDIF.
*  DATA L_MAKTX TYPE MAKT-MAKTX.
*  SELECT SINGLE MAKTX
*              FROM MAKT
*              INTO L_MAKTX
*              WHERE MATNR EQ ZSBM_MODEL_VALS-MATNR.
*  IF SY-SUBRC EQ 0.
*    ZSBM_MODEL_VALS-MAKTX = L_MAKTX.
*  ELSE.
*    CLEAR ZSBM_MODEL_VALS-MAKTX.
*  ENDIF.
ENDFORM.                    " READ_MAKT
*&---------------------------------------------------------------------*
*&      Form  DELETE_PROCESS
*&---------------------------------------------------------------------*
FORM DELETE_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX.
  DELETE IT_UPGV WHERE ZCHK EQ 'X'.
  CLEAR IT_UPGV.
  DESCRIBE TABLE IT_UPGV LINES T_9000-LINES.
  T_9000-TOP_LINE = '1'.
ENDFORM.                    " DELETE_PROCESS
