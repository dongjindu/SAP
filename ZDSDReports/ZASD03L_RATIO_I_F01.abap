*----------------------------------------------------------------------*
*   INCLUDE ZASD03L_RATIO_I_F01                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE IT_RATIO_I
         FROM ZTSD_RATIO_I
        WHERE ZVEND EQ P_ZVEND
        AND   ZGROP EQ P_ZGROP
        AND   ZDTFG NE 'X'.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM DISPLAY_DATA.
  DESCRIBE TABLE IT_RATIO_I LINES W_CNT.
  IF W_CNT = 0.
    CLEAR ZTSD_RATIO_I.
    ZTSD_RATIO_I-ZVEND = P_ZVEND.
    ZTSD_RATIO_I-ZGROP = P_ZGROP.
    ZTSD_RATIO_I-ZVSEQ = 1.

    CALL SCREEN 9000.
  ELSE.
    PERFORM DISPLAY_LIST.
  ENDIF.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST
*&---------------------------------------------------------------------*
FORM DISPLAY_LIST.
  SORT IT_RATIO_I DESCENDING BY ZVSEQ.
  LOOP AT IT_RATIO_I.
    WRITE:/ SY-VLINE, (03) IT_RATIO_I-ZVSEQ,
            SY-VLINE, (18) IT_RATIO_I-ZCPTN,
            SY-VLINE, (06) IT_RATIO_I-ZPTG1,
            SY-VLINE, (06) IT_RATIO_I-ZPTG2,
            SY-VLINE, (08) IT_RATIO_I-ZITEM,
            SY-VLINE, (03) IT_RATIO_I-ZNATR,
            SY-VLINE, (03) IT_RATIO_I-ZCAUS,
            SY-VLINE, (03) IT_RATIO_I-ZCOLR,
            SY-VLINE, (03) IT_RATIO_I-ZPCAU,
            SY-VLINE, (06) IT_RATIO_I-ZODFM,
            SY-VLINE, (06) IT_RATIO_I-ZODTO,
            SY-VLINE, (04) IT_RATIO_I-ZDYFM,
            SY-VLINE, (04) IT_RATIO_I-ZDYTO,
            SY-VLINE, (10) IT_RATIO_I-ZEFFG,
            SY-VLINE, (10) IT_RATIO_I-ZEFFM,
            SY-VLINE, (10) IT_RATIO_I-ZEFTO,
            SY-VLINE, (12) IT_RATIO_I-ZVRAT,
            SY-VLINE.
    WRITE:/(167) SY-ULINE.

    IF W_ZVSEQ < IT_RATIO_I-ZVSEQ.
      W_ZVSEQ = IT_RATIO_I-ZVSEQ.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " DISPLAY_RESULT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  WRITE:/ ' Vendor code :', P_ZVEND, 'Item group :', P_ZGROP.
  FORMAT COLOR COL_HEADING.
  WRITE:/(167) SY-ULINE.
  WRITE:/ SY-VLINE, (03) 'Seq',
          SY-VLINE, (18) 'Causal Part No',
          SY-VLINE, (06) 'Part 1',
          SY-VLINE, (06) 'Part 2',
          SY-VLINE, (08) 'Item Code',
          SY-VLINE, (03) 'Ncd',
          SY-VLINE, (03) 'Ccd',
          SY-VLINE, (03) 'B/C',
          SY-VLINE, (03) 'PCC',
          SY-VLINE, (06) 'ODFM',
          SY-VLINE, (06) 'ODTO',
          SY-VLINE, (04) 'DYFM',
          SY-VLINE, (04) 'DYTO',
          SY-VLINE, (10) 'Eff. DT FG',
          SY-VLINE, (10) 'Eff. DT FM',
          SY-VLINE, (10) 'Eff. DT TO',
          SY-VLINE, (12) 'Vendor Ratio', "9
          SY-VLINE.
  WRITE:/(167) SY-ULINE.
  FORMAT COLOR COL_HEADING OFF.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND.
  DATA : OK_CODE(4).
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
        PERFORM DISPLAY_LIST.
      ENDIF.
    WHEN 'CHAN'.
      PERFORM CHANGE_DATA.
      IF W_SAVE = 'Y'.
        SY-LSIND = SY-LSIND - 1.
        PERFORM DISPLAY_LIST.
      ENDIF.
    WHEN 'DELE'.
      PERFORM DELETE_DATA.
      IF W_ANSWER = 'J'.
        DESCRIBE TABLE IT_RATIO_I LINES W_CNT.
        IF W_CNT = 0.
          LEAVE TO SCREEN 0.
        ELSE.
          SY-LSIND = SY-LSIND - 1.
          PERFORM DISPLAY_LIST.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CREATE_DATA
*&---------------------------------------------------------------------*
FORM CREATE_DATA.
  CLEAR ZTSD_RATIO_I.
  ZTSD_RATIO_I-ZVEND = P_ZVEND.
  ZTSD_RATIO_I-ZGROP = P_ZGROP.
  ZTSD_RATIO_I-ZVSEQ = W_ZVSEQ + 1.

  CALL SCREEN 9000.
ENDFORM.                    " CREATE_DATA
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DATA
*&---------------------------------------------------------------------*
FORM CHANGE_DATA.
  CLEAR W_SAVE.

  IF SY-LISEL+2(1) CA '0123456789'.
    READ TABLE IT_RATIO_I WITH KEY ZVSEQ = SY-LISEL+2(3).
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING IT_RATIO_I TO ZTSD_RATIO_I.
      CALL SCREEN 9000.
    ENDIF.
  ELSE.
    MESSAGE I000 WITH TEXT-M05.
  ENDIF.
ENDFORM.                    " CHANGE_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
FORM DELETE_DATA.
  CLEAR W_ANSWER.

  IF SY-LISEL+2(1) CA '0123456789'.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        DEFAULTOPTION     = 'N'
        TEXTLINE1         = TEXT-M07
        TITEL             = TEXT-M08
      IMPORTING
        ANSWER            = W_ANSWER.
    IF W_ANSWER EQ 'J'.
      UPDATE ZTSD_RATIO_I SET ZDTFG = 'X'
            WHERE ZVEND EQ P_ZVEND
            AND   ZGROP EQ P_ZGROP
            AND   ZVSEQ EQ SY-LISEL+2(3).
      IF SY-SUBRC = 0.
        MESSAGE I000 WITH TEXT-M09.
        DELETE IT_RATIO_I WHERE ZVEND EQ P_ZVEND
                          AND   ZGROP EQ P_ZGROP
                          AND   ZVSEQ EQ SY-LISEL+2(3).
      ELSE.
        MESSAGE I000 WITH TEXT-M10.
        ROLLBACK WORK.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE I000 WITH TEXT-M05.
  ENDIF.
ENDFORM.                    " DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA.
  READ TABLE IT_RATIO_I WITH KEY ZVSEQ = ZTSD_RATIO_I-ZVSEQ.
  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING ZTSD_RATIO_I TO IT_RATIO_I.
    MODIFY IT_RATIO_I INDEX SY-TABIX.

    MODIFY ZTSD_RATIO_I.
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
    ZTSD_RATIO_I-ZERDA = SY-DATUM.
    ZTSD_RATIO_I-ERZET = SY-UZEIT.
    ZTSD_RATIO_I-ERNAM = SY-UNAME.
    MOVE-CORRESPONDING ZTSD_RATIO_I TO IT_RATIO_I.
    APPEND IT_RATIO_I.

    INSERT ZTSD_RATIO_I FROM ZTSD_RATIO_I.
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
ENDFORM.                    " SAVE_DATA
