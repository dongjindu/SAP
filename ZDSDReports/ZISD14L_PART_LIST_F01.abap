*----------------------------------------------------------------------*
*   INCLUDE ZASD03L_VEN_WC_F01                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT_STATUS
*&---------------------------------------------------------------------*
FORM INIT_STATUS.
  CASE 'X'.
  WHEN P_INF.
    W_GUBUN = 'I'.

    REFRESH TAB. CLEAR TAB.
    MOVE 'PREV' TO WA_TAB-FCODE.
    APPEND WA_TAB TO TAB.
    MOVE 'NEXT' TO WA_TAB-FCODE.
    APPEND WA_TAB TO TAB.

    SET PF-STATUS 'ISD14R' EXCLUDING TAB.
  WHEN P_PRC.
    W_GUBUN = 'P'.

    REFRESH TAB. CLEAR TAB.
    MOVE 'PREV' TO WA_TAB-FCODE.
    APPEND WA_TAB TO TAB.
    MOVE 'NEXT' TO WA_TAB-FCODE.
    APPEND WA_TAB TO TAB.

    SET PF-STATUS 'ISD14R' EXCLUDING TAB.
  WHEN P_HST.
    W_GUBUN = 'H'.

    REFRESH TAB. CLEAR TAB.
    MOVE 'PREV' TO WA_TAB-FCODE.
    APPEND WA_TAB TO TAB.
    MOVE 'NEXT' TO WA_TAB-FCODE.
    APPEND WA_TAB TO TAB.

    SET PF-STATUS 'ISD14R' EXCLUDING TAB.
  WHEN P_SUP.
    W_GUBUN = 'S'.

    SET PF-STATUS 'ISD14R'.
  ENDCASE.
ENDFORM.                    " INIT_STATUS
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  CASE W_GUBUN.
  WHEN 'I'.
    SELECT *
           INTO CORRESPONDING FIELDS OF TABLE IT_PART_INF
           FROM ZTSD_PART_INF
          WHERE ZCPTN EQ P_ZCPTN.
  WHEN 'P'.
    SELECT *
           INTO CORRESPONDING FIELDS OF TABLE IT_PART_PRC
           FROM ZTSD_PART_PRC
          WHERE ZCPTN EQ P_ZCPTN.
  WHEN 'H'.
    SELECT *
           INTO CORRESPONDING FIELDS OF TABLE IT_PART_HST
           FROM ZTSD_PART_HST
          WHERE ZCPTN EQ P_ZCPTN.
  WHEN 'S'.
    SELECT *
           INTO CORRESPONDING FIELDS OF TABLE IT_PART_SUP
           FROM ZTSD_PART_SUP
          WHERE ZCPTN EQ P_ZCPTN.
  ENDCASE.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM DISPLAY_DATA.
  CASE W_GUBUN.
  WHEN 'I'.
    DESCRIBE TABLE IT_PART_INF LINES W_CNT.
    IF W_CNT = 0.
      CLEAR ZTSD_PART_INF.
      ZTSD_PART_INF-ZCPTN = P_ZCPTN.

      CALL SCREEN 9001.
    ELSE.
      PERFORM DISPLAY_INF.
    ENDIF.
  WHEN 'P'.
    DESCRIBE TABLE IT_PART_PRC LINES W_CNT.
    IF W_CNT = 0.
      CLEAR ZTSD_PART_PRC.
      ZTSD_PART_PRC-ZCPTN = P_ZCPTN.

      CALL SCREEN 9002.
    ELSE.
      PERFORM DISPLAY_PRC.
    ENDIF.
  WHEN 'H'.
    DESCRIBE TABLE IT_PART_HST LINES W_CNT.
    IF W_CNT = 0.
      CLEAR ZTSD_PART_HST.
      ZTSD_PART_HST-ZCPTN = P_ZCPTN.

      CALL SCREEN 9003.
    ELSE.
      PERFORM DISPLAY_HST.
    ENDIF.
  WHEN 'S'.
    W_CNT = 0.
    LOOP AT IT_PART_SUP WHERE ZSQYY = P_ZSQYY.
      W_CNT = W_CNT + 1.
    ENDLOOP.
    IF W_CNT = 0.
      CLEAR ZTSD_PART_SUP.
      ZTSD_PART_SUP-ZCPTN = P_ZCPTN.
      ZTSD_PART_SUP-ZSQYY = P_ZSQYY.

      CALL SCREEN 9004.
    ELSE.
      PERFORM DISPLAY_SUP.
    ENDIF.
  ENDCASE.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_INF
*&---------------------------------------------------------------------*
FORM DISPLAY_INF.
  SORT IT_PART_INF.
  LOOP AT IT_PART_INF.
    WRITE:/ SY-VLINE, (03) IT_PART_INF-ZUSE,
            SY-VLINE, (45) IT_PART_INF-ZPTNA,
            SY-VLINE, (03) IT_PART_INF-ZPTG2,
            SY-VLINE.
    W_TABIX = SY-TABIX.
    HIDE : W_TABIX.
    WRITE:/(61) SY-ULINE.
  ENDLOOP.
ENDFORM.                    " DISPLAY_INF
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PRC
*&---------------------------------------------------------------------*
FORM DISPLAY_PRC.
  SORT IT_PART_PRC DESCENDING BY ZSORC ZDIST ZMOD
                   ASCENDING ZEFFM.
  LOOP AT IT_PART_PRC.
    WRITE:/ SY-VLINE, (03) IT_PART_PRC-ZSORC,
            SY-VLINE, (05) IT_PART_PRC-ZDIST,
            SY-VLINE, (05) IT_PART_PRC-ZMOD,
            SY-VLINE, (05) IT_PART_PRC-ZMYFM,
            SY-VLINE, (05) IT_PART_PRC-ZMYTO,
            SY-VLINE, (01) IT_PART_PRC-ZEFFG,
            SY-VLINE, (10) IT_PART_PRC-ZEFFM,
            SY-VLINE, (10) IT_PART_PRC-ZEFTO,
            SY-VLINE, (12) IT_PART_PRC-ZUNPR CURRENCY IT_PART_PRC-ZCURR,
            SY-VLINE, (05) IT_PART_PRC-ZCURR,
            SY-VLINE.
    W_TABIX = SY-TABIX.
    HIDE : W_TABIX.
    WRITE:/(92) SY-ULINE.
  ENDLOOP.
ENDFORM.                    " DISPLAY_PRC
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_HST
*&---------------------------------------------------------------------*
FORM DISPLAY_HST.
  SORT IT_PART_HST DESCENDING BY ZSORC ZVEND
                   ASCENDING ZEFFM.
  LOOP AT IT_PART_HST.
    WRITE:/ SY-VLINE, (03) IT_PART_HST-ZSORC,
            SY-VLINE, (06) IT_PART_HST-ZVEND,
            SY-VLINE, (01) IT_PART_HST-ZEFFG,
            SY-VLINE, (10) IT_PART_HST-ZEFFM,
            SY-VLINE, (10) IT_PART_HST-ZEFTO,
            SY-VLINE.
    W_TABIX = SY-TABIX.
    HIDE : W_TABIX.
    WRITE:/(46) SY-ULINE.
  ENDLOOP.
ENDFORM.                    " DISPLAY_HST
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SUP
*&---------------------------------------------------------------------*
FORM DISPLAY_SUP.
  SORT IT_PART_SUP DESCENDING BY ZSORC ZVEND.
  LOOP AT IT_PART_SUP WHERE ZSQYY = P_ZSQYY.
    WRITE:/ SY-VLINE, (03) IT_PART_SUP-ZSORC,
            SY-VLINE, (06) IT_PART_SUP-ZVEND,
            SY-VLINE, (07) IT_PART_SUP-ZQTY01 NO-ZERO,
            SY-VLINE, (07) IT_PART_SUP-ZQTY02 NO-ZERO,
            SY-VLINE, (07) IT_PART_SUP-ZQTY03 NO-ZERO,
            SY-VLINE, (07) IT_PART_SUP-ZQTY04 NO-ZERO,
            SY-VLINE, (07) IT_PART_SUP-ZQTY05 NO-ZERO,
            SY-VLINE, (07) IT_PART_SUP-ZQTY06 NO-ZERO,
            SY-VLINE, (07) IT_PART_SUP-ZQTY07 NO-ZERO,
            SY-VLINE, (07) IT_PART_SUP-ZQTY08 NO-ZERO,
            SY-VLINE, (07) IT_PART_SUP-ZQTY09 NO-ZERO,
            SY-VLINE, (07) IT_PART_SUP-ZQTY10 NO-ZERO,
            SY-VLINE, (07) IT_PART_SUP-ZQTY11 NO-ZERO,
            SY-VLINE, (07) IT_PART_SUP-ZQTY12 NO-ZERO,
            SY-VLINE.
    W_TABIX = SY-TABIX.
    HIDE : W_TABIX.
    WRITE:/(136) SY-ULINE.
  ENDLOOP.
ENDFORM.                    " DISPLAY_SUP
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  CASE W_GUBUN.
  WHEN 'I'.
    WRITE:/ ' Part No :', P_ZCPTN.
    FORMAT COLOR COL_HEADING.
    WRITE:/(61) SY-ULINE.
    WRITE:/ SY-VLINE, (03) 'CKD',
            SY-VLINE, (45) 'Part Name',
            SY-VLINE, (03) 'PG2',
            SY-VLINE.
    WRITE:/(61) SY-ULINE.
    FORMAT COLOR COL_HEADING OFF.
  WHEN 'P'.
    WRITE:/ ' Part No :', P_ZCPTN.
    FORMAT COLOR COL_HEADING.
    WRITE:/(92) SY-ULINE.
    WRITE:/ SY-VLINE, (03) 'Src',
            SY-VLINE, (05) 'Dist.',
            SY-VLINE, (05) 'Model',
            SY-VLINE, (05) 'MY Fr',
            SY-VLINE, (05) 'MY To',
            SY-VLINE, (01) 'F',
            SY-VLINE, (10) 'Eff.Dt Fr',
            SY-VLINE, (10) 'Eff.Dt Fr',
            SY-VLINE, (12) 'Unit Price',
            SY-VLINE, (05) 'CurrK',
            SY-VLINE.
    WRITE:/(92) SY-ULINE.
    FORMAT COLOR COL_HEADING OFF.
  WHEN 'H'.
    WRITE:/ ' Part No :', P_ZCPTN.
    FORMAT COLOR COL_HEADING.
    WRITE:/(46) SY-ULINE.
    WRITE:/ SY-VLINE, (03) 'Src',
            SY-VLINE, (06) 'Vendor',
            SY-VLINE, (01) 'F',
            SY-VLINE, (10) 'Eff.Dt Fr',
            SY-VLINE, (10) 'Eff.Dt Fr',
            SY-VLINE.
    WRITE:/(46) SY-ULINE.
    FORMAT COLOR COL_HEADING OFF.
  WHEN 'S'.
    WRITE:/ ' Part No :', P_ZCPTN,
            'Supply Year :', P_ZSQYY.
    FORMAT COLOR COL_HEADING.
    WRITE:/(136) SY-ULINE.
    WRITE:/ SY-VLINE, (03) 'Src',
            SY-VLINE, (06) 'Vendor',
            SY-VLINE, (07) '   1   ',
            SY-VLINE, (07) '   2   ',
            SY-VLINE, (07) '   3   ',
            SY-VLINE, (07) '   4   ',
            SY-VLINE, (07) '   5   ',
            SY-VLINE, (07) '   6   ',
            SY-VLINE, (07) '   7   ',
            SY-VLINE, (07) '   8   ',
            SY-VLINE, (07) '   9   ',
            SY-VLINE, (07) '  10   ',
            SY-VLINE, (07) '  11   ',
            SY-VLINE, (07) '  12   ',
            SY-VLINE.
    WRITE:/(136) SY-ULINE.
    FORMAT COLOR COL_HEADING OFF.
  ENDCASE.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CREA'.
      PERFORM CREATE_DATA.
      IF W_SAVE = 'Y'.
        SY-LSIND = SY-LSIND - 1.
        CASE W_GUBUN.
        WHEN 'I'.
          PERFORM DISPLAY_INF.
        WHEN 'P'.
          PERFORM DISPLAY_PRC.
        WHEN 'H'.
          PERFORM DISPLAY_HST.
        WHEN 'S'.
          PERFORM DISPLAY_SUP.
        ENDCASE.
      ENDIF.
    WHEN 'CHAN'.
      PERFORM CHANGE_DATA.
      IF W_SAVE = 'Y'.
        SY-LSIND = SY-LSIND - 1.
        CASE W_GUBUN.
        WHEN 'I'.
          PERFORM DISPLAY_INF.
        WHEN 'P'.
          PERFORM DISPLAY_PRC.
        WHEN 'H'.
          PERFORM DISPLAY_HST.
        WHEN 'S'.
          PERFORM DISPLAY_SUP.
        ENDCASE.
      ENDIF.
    WHEN 'DELE'.
      PERFORM DELETE_DATA.
      IF W_ANSWER = 'J'.
        CASE W_GUBUN.
        WHEN 'I'.
          DESCRIBE TABLE IT_PART_INF LINES W_CNT.
        WHEN 'P'.
          DESCRIBE TABLE IT_PART_PRC LINES W_CNT.
        WHEN 'H'.
          DESCRIBE TABLE IT_PART_HST LINES W_CNT.
        WHEN 'S'.
          DESCRIBE TABLE IT_PART_SUP LINES W_CNT.
        ENDCASE.
        IF W_CNT = 0.
          LEAVE TO SCREEN 0.
        ELSE.
          SY-LSIND = SY-LSIND - 1.
          CASE W_GUBUN.
          WHEN 'I'.
            PERFORM DISPLAY_INF.
          WHEN 'P'.
            PERFORM DISPLAY_PRC.
          WHEN 'H'.
            PERFORM DISPLAY_HST.
          WHEN 'S'.
            PERFORM DISPLAY_SUP.
          ENDCASE.
        ENDIF.
      ENDIF.
    WHEN 'PREV'.
      P_ZSQYY = P_ZSQYY - 1.
      READ TABLE IT_PART_SUP WITH KEY ZSQYY = P_ZSQYY.
      IF SY-SUBRC = 0.
        SY-LSIND = SY-LSIND - 1.
        PERFORM DISPLAY_SUP.
      ELSE.
        P_ZSQYY = P_ZSQYY + 1.
        MESSAGE I000 WITH 'FIRST ENTRY'.
      ENDIF.
    WHEN 'NEXT'.
      P_ZSQYY = P_ZSQYY + 1.
      READ TABLE IT_PART_SUP WITH KEY ZSQYY = P_ZSQYY.
      IF SY-SUBRC = 0.
        SY-LSIND = SY-LSIND - 1.
        PERFORM DISPLAY_SUP.
      ELSE.
        P_ZSQYY = P_ZSQYY - 1.
        MESSAGE I000 WITH 'LAST ENTRY'.
      ENDIF.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CREATE_DATA
*&---------------------------------------------------------------------*
FORM CREATE_DATA.
  CASE W_GUBUN.
  WHEN 'I'.
    CLEAR ZTSD_PART_INF.
    ZTSD_PART_INF-ZCPTN = P_ZCPTN.

    CALL SCREEN 9001.
  WHEN 'P'.
    CLEAR ZTSD_PART_PRC.
    ZTSD_PART_PRC-ZCPTN = P_ZCPTN.

    CALL SCREEN 9002.
  WHEN 'H'.
    CLEAR ZTSD_PART_HST.
    ZTSD_PART_HST-ZCPTN = P_ZCPTN.

    CALL SCREEN 9003.
  WHEN 'S'.
    CLEAR ZTSD_PART_SUP.
    ZTSD_PART_SUP-ZCPTN = P_ZCPTN.
    ZTSD_PART_SUP-ZSQYY = P_ZSQYY.

    CALL SCREEN 9004.
  ENDCASE.
ENDFORM.                    " CREATE_DATA
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DATA
*&---------------------------------------------------------------------*
FORM CHANGE_DATA.
  CLEAR W_SAVE.

  IF SY-LISEL+3(1) EQ ' '.
    CASE W_GUBUN.
    WHEN 'I'.
      READ TABLE IT_PART_INF INDEX W_TABIX.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING IT_PART_INF TO ZTSD_PART_INF.
        CALL SCREEN 9001.
      ENDIF.
    WHEN 'P'.
      READ TABLE IT_PART_PRC INDEX W_TABIX.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING IT_PART_PRC TO ZTSD_PART_PRC.
        CALL SCREEN 9002.
      ENDIF.
    WHEN 'H'.
      READ TABLE IT_PART_HST INDEX W_TABIX.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING IT_PART_HST TO ZTSD_PART_HST.
        CALL SCREEN 9003.
      ENDIF.
    WHEN 'S'.
      READ TABLE IT_PART_SUP INDEX W_TABIX.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING IT_PART_SUP TO ZTSD_PART_SUP.
        CALL SCREEN 9004.
      ENDIF.
    ENDCASE.
  ELSE.
    MESSAGE I000 WITH TEXT-M05.
  ENDIF.
ENDFORM.                    " CHANGE_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
FORM DELETE_DATA.
  CLEAR W_ANSWER.

  IF SY-LISEL+3(1) EQ ' '.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        DEFAULTOPTION     = 'N'
        TEXTLINE1         = TEXT-M07
        TITEL             = TEXT-M08
      IMPORTING
        ANSWER            = W_ANSWER.
    IF W_ANSWER EQ 'J'.
      CASE W_GUBUN.
      WHEN 'I'.
        READ TABLE IT_PART_INF INDEX W_TABIX.
        DELETE FROM ZTSD_PART_INF
              WHERE ZCPTN EQ P_ZCPTN.
        IF SY-SUBRC = 0.
          MESSAGE I000 WITH TEXT-M09.
          DELETE IT_PART_INF WHERE ZCPTN EQ P_ZCPTN.
        ELSE.
          MESSAGE I000 WITH TEXT-M10.
          ROLLBACK WORK.
        ENDIF.
      WHEN 'P'.
        READ TABLE IT_PART_PRC INDEX W_TABIX.
        DELETE FROM ZTSD_PART_PRC
              WHERE ZCPTN EQ P_ZCPTN
              AND   ZSORC EQ IT_PART_PRC-ZSORC
              AND   ZDIST EQ IT_PART_PRC-ZDIST
              AND   ZMOD  EQ IT_PART_PRC-ZMOD
              AND   ZMYFM EQ IT_PART_PRC-ZMYFM
              AND   ZEFFM EQ IT_PART_PRC-ZEFFM.
        IF SY-SUBRC = 0.
          MESSAGE I000 WITH TEXT-M09.
          DELETE IT_PART_PRC WHERE ZCPTN EQ P_ZCPTN
                             AND   ZSORC EQ IT_PART_PRC-ZSORC
                             AND   ZDIST EQ IT_PART_PRC-ZDIST
                             AND   ZMOD  EQ IT_PART_PRC-ZMOD
                             AND   ZMYFM EQ IT_PART_PRC-ZMYFM
                             AND   ZEFFM EQ IT_PART_PRC-ZEFFM.
        ELSE.
          MESSAGE I000 WITH TEXT-M10.
          ROLLBACK WORK.
        ENDIF.
      WHEN 'H'.
        READ TABLE IT_PART_HST INDEX W_TABIX.
        DELETE FROM ZTSD_PART_HST
              WHERE ZCPTN EQ P_ZCPTN
              AND   ZSORC EQ IT_PART_HST-ZSORC
              AND   ZVEND EQ IT_PART_HST-ZVEND
              AND   ZEFFM EQ IT_PART_HST-ZEFFM.
        IF SY-SUBRC = 0.
          MESSAGE I000 WITH TEXT-M09.
          DELETE IT_PART_HST WHERE ZCPTN EQ P_ZCPTN
                             AND   ZSORC EQ IT_PART_HST-ZSORC
                             AND   ZVEND EQ IT_PART_HST-ZVEND
                             AND   ZEFFM EQ IT_PART_HST-ZEFFM.
        ELSE.
          MESSAGE I000 WITH TEXT-M10.
          ROLLBACK WORK.
        ENDIF.
      WHEN 'S'.
        READ TABLE IT_PART_SUP INDEX W_TABIX.
        DELETE FROM ZTSD_PART_SUP
              WHERE ZCPTN EQ P_ZCPTN
              AND   ZSORC EQ IT_PART_SUP-ZSORC
              AND   ZVEND EQ IT_PART_SUP-ZVEND
              AND   ZSQYY EQ IT_PART_SUP-ZSQYY.
        IF SY-SUBRC = 0.
          MESSAGE I000 WITH TEXT-M09.
          DELETE IT_PART_SUP WHERE ZCPTN EQ P_ZCPTN
                             AND   ZSORC EQ IT_PART_SUP-ZSORC
                             AND   ZVEND EQ IT_PART_SUP-ZVEND
                             AND   ZSQYY EQ IT_PART_SUP-ZSQYY.
        ELSE.
          MESSAGE I000 WITH TEXT-M10.
          ROLLBACK WORK.
        ENDIF.
      ENDCASE.
    ENDIF.
  ELSE.
    MESSAGE I000 WITH TEXT-M05.
  ENDIF.
ENDFORM.                    " DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA.
  CASE W_GUBUN.
  WHEN 'I'.
    READ TABLE IT_PART_INF WITH KEY ZCPTN = ZTSD_PART_INF-ZCPTN.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING ZTSD_PART_INF TO IT_PART_INF.
      MODIFY IT_PART_INF INDEX SY-TABIX.

      MODIFY ZTSD_PART_INF.
      IF SY-SUBRC = 0.
        COMMIT WORK.
        MESSAGE I000 WITH TEXT-M01.
        W_SAVE = 'Y'.
        LEAVE TO SCREEN 0.
      ELSE.
        ROLLBACK WORK.
        MESSAGE I000 WITH TEXT-M02.
      ENDIF.
    ELSE.
      ZTSD_PART_INF-ZERDA = SY-DATUM.
      ZTSD_PART_INF-ERZET = SY-UZEIT.
      ZTSD_PART_INF-ERNAM = SY-UNAME.
      MOVE-CORRESPONDING ZTSD_PART_INF TO IT_PART_INF.
      APPEND IT_PART_INF.

      INSERT ZTSD_PART_INF FROM ZTSD_PART_INF.
      IF SY-SUBRC = 0.
        COMMIT WORK.
        MESSAGE I000 WITH TEXT-M03.
        W_SAVE = 'Y'.
        LEAVE TO SCREEN 0.
      ELSE.
        ROLLBACK WORK.
        MESSAGE I000 WITH TEXT-M04.
      ENDIF.
    ENDIF.
  WHEN 'P'.
    READ TABLE IT_PART_PRC WITH KEY ZCPTN = ZTSD_PART_PRC-ZCPTN
                                    ZSORC = ZTSD_PART_PRC-ZSORC
                                    ZDIST = ZTSD_PART_PRC-ZDIST
                                    ZMOD  = ZTSD_PART_PRC-ZMOD
                                    ZMYFM = ZTSD_PART_PRC-ZMYFM
                                    ZEFFM = ZTSD_PART_PRC-ZEFFM.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING ZTSD_PART_PRC TO IT_PART_PRC.
      MODIFY IT_PART_PRC INDEX SY-TABIX.

      MODIFY ZTSD_PART_PRC.
      IF SY-SUBRC = 0.
        COMMIT WORK.
        MESSAGE I000 WITH TEXT-M01.
        W_SAVE = 'Y'.
        LEAVE TO SCREEN 0.
      ELSE.
        ROLLBACK WORK.
        MESSAGE I000 WITH TEXT-M02.
      ENDIF.
    ELSE.
      ZTSD_PART_PRC-ZERDA = SY-DATUM.
      ZTSD_PART_PRC-ERZET = SY-UZEIT.
      ZTSD_PART_PRC-ERNAM = SY-UNAME.
      MOVE-CORRESPONDING ZTSD_PART_PRC TO IT_PART_PRC.
      APPEND IT_PART_PRC.

      INSERT ZTSD_PART_PRC FROM ZTSD_PART_PRC.
      IF SY-SUBRC = 0.
        COMMIT WORK.
        MESSAGE I000 WITH TEXT-M03.
        W_SAVE = 'Y'.
        LEAVE TO SCREEN 0.
      ELSE.
        ROLLBACK WORK.
        MESSAGE I000 WITH TEXT-M04.
      ENDIF.
    ENDIF.
  WHEN 'H'.
    READ TABLE IT_PART_HST WITH KEY ZCPTN = ZTSD_PART_HST-ZCPTN
                                    ZSORC = ZTSD_PART_HST-ZSORC
                                    ZVEND = ZTSD_PART_HST-ZVEND
                                    ZEFFM = ZTSD_PART_HST-ZEFFM.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING ZTSD_PART_HST TO IT_PART_HST.
      MODIFY IT_PART_HST INDEX SY-TABIX.

      MODIFY ZTSD_PART_HST.
      IF SY-SUBRC = 0.
        COMMIT WORK.
        MESSAGE I000 WITH TEXT-M01.
        W_SAVE = 'Y'.
        LEAVE TO SCREEN 0.
      ELSE.
        ROLLBACK WORK.
        MESSAGE I000 WITH TEXT-M02.
      ENDIF.
    ELSE.
      ZTSD_PART_HST-ZERDA = SY-DATUM.
      ZTSD_PART_HST-ERZET = SY-UZEIT.
      ZTSD_PART_HST-ERNAM = SY-UNAME.
      MOVE-CORRESPONDING ZTSD_PART_HST TO IT_PART_HST.
      APPEND IT_PART_HST.

      INSERT ZTSD_PART_HST FROM ZTSD_PART_HST.
      IF SY-SUBRC = 0.
        COMMIT WORK.
        MESSAGE I000 WITH TEXT-M03.
        W_SAVE = 'Y'.
        LEAVE TO SCREEN 0.
      ELSE.
        ROLLBACK WORK.
        MESSAGE I000 WITH TEXT-M04.
      ENDIF.
    ENDIF.
  WHEN 'S'.
    READ TABLE IT_PART_SUP WITH KEY ZCPTN = ZTSD_PART_SUP-ZCPTN
                                    ZSORC = ZTSD_PART_SUP-ZSORC
                                    ZVEND = ZTSD_PART_SUP-ZVEND
                                    ZSQYY = ZTSD_PART_SUP-ZSQYY.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING ZTSD_PART_SUP TO IT_PART_SUP.
      MODIFY IT_PART_SUP INDEX SY-TABIX.

      MODIFY ZTSD_PART_SUP.
      IF SY-SUBRC = 0.
        COMMIT WORK.
        MESSAGE I000 WITH TEXT-M01.
        W_SAVE = 'Y'.
        LEAVE TO SCREEN 0.
      ELSE.
        ROLLBACK WORK.
        MESSAGE I000 WITH TEXT-M02.
      ENDIF.
    ELSE.
      ZTSD_PART_SUP-ZERDA = SY-DATUM.
      ZTSD_PART_SUP-ERZET = SY-UZEIT.
      ZTSD_PART_SUP-ERNAM = SY-UNAME.
      MOVE-CORRESPONDING ZTSD_PART_SUP TO IT_PART_SUP.
      APPEND IT_PART_SUP.

      INSERT ZTSD_PART_SUP FROM ZTSD_PART_SUP.
      IF SY-SUBRC = 0.
        COMMIT WORK.
        MESSAGE I000 WITH TEXT-M03.
        W_SAVE = 'Y'.
        LEAVE TO SCREEN 0.
      ELSE.
        ROLLBACK WORK.
        MESSAGE I000 WITH TEXT-M04.
      ENDIF.
    ENDIF.
  ENDCASE.
ENDFORM.                    " SAVE_DATA
