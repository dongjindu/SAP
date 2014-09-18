*----------------------------------------------------------------------*
*   INCLUDE ZASD03L_RATIO_I_F01                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE IT_RATIO_USER
         FROM ZTSD_RATIO_USER
        WHERE ZITEM EQ P_ZITEM
        AND   ZGROP EQ P_ZGROP
        AND   ZDTFG EQ SPACE.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM DISPLAY_DATA.
  DESCRIBE TABLE IT_RATIO_USER LINES W_CNT.
  IF W_CNT = 0.
    CLEAR ZTSD_RATIO_USER.
    ZTSD_RATIO_USER-ZITEM = P_ZITEM.
    ZTSD_RATIO_USER-ZGROP = P_ZGROP.
    ZTSD_RATIO_USER-ZVSEQ = 1.

    CALL SCREEN 9000.
  ELSE.
    PERFORM DISPLAY_LIST.
  ENDIF.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST
*&---------------------------------------------------------------------*
FORM DISPLAY_LIST.
  SORT IT_RATIO_USER DESCENDING BY ZVSEQ.
  LOOP AT IT_RATIO_USER.
    WRITE:/ SY-VLINE, (03) IT_RATIO_USER-ZVSEQ,
            SY-VLINE, (18) IT_RATIO_USER-ZITEM,
            SY-VLINE, (08) IT_RATIO_USER-ZMNOP,
            SY-VLINE, (01) IT_RATIO_USER-ZSKIP,
            SY-VLINE, (08) IT_RATIO_USER-ZISFM,
            SY-VLINE, (08) IT_RATIO_USER-ZISTO,
            SY-VLINE, (02) IT_RATIO_USER-ZMODL,
            SY-VLINE, (03) IT_RATIO_USER-ZNATR,
            SY-VLINE, (03) IT_RATIO_USER-ZCAUS,
            SY-VLINE, (03) IT_RATIO_USER-ZCOLR,
            SY-VLINE, (03) IT_RATIO_USER-ZPCAU,
            SY-VLINE, (06) IT_RATIO_USER-ZODFM,
            SY-VLINE, (06) IT_RATIO_USER-ZODTO,
            SY-VLINE, (04) IT_RATIO_USER-ZDYFM,
            SY-VLINE, (04) IT_RATIO_USER-ZDYTO,
            SY-VLINE, (04) IT_RATIO_USER-ZEFFG,
            SY-VLINE, (10) IT_RATIO_USER-ZEFFM,
            SY-VLINE, (10) IT_RATIO_USER-ZEFTO,
            SY-VLINE, (06) IT_RATIO_USER-ZVEN1,
            SY-VLINE, (10) IT_RATIO_USER-ZSHR1,
            SY-VLINE, (06) IT_RATIO_USER-ZVEN2,
            SY-VLINE, (10) IT_RATIO_USER-ZSHR2,
            SY-VLINE, (06) IT_RATIO_USER-ZVEN3,
            SY-VLINE, (10) IT_RATIO_USER-ZSHR3,
            SY-VLINE.
    WRITE:/(225) SY-ULINE.

    IF W_ZVSEQ < IT_RATIO_USER-ZVSEQ.
      W_ZVSEQ = IT_RATIO_USER-ZVSEQ.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " DISPLAY_RESULT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  WRITE:/ ' Vendor code :', P_ZITEM, 'Item group :', P_ZGROP.
  FORMAT COLOR COL_HEADING.
  WRITE:/(225) SY-ULINE.
  WRITE:/ SY-VLINE, (03) 'Seq',
          SY-VLINE, (18) 'Part/OP',
          SY-VLINE, (08) 'OP Code',
          SY-VLINE, (01) 'Skip',
          SY-VLINE, (08) 'IssDat FR',
          SY-VLINE, (08) 'IssDat TO',
          SY-VLINE, (02) 'MD',
          SY-VLINE, (03) 'Ncd',
          SY-VLINE, (03) 'Ccd',
          SY-VLINE, (03) 'B/C',
          SY-VLINE, (03) 'PCC',
          SY-VLINE, (06) 'ODFM',
          SY-VLINE, (06) 'ODTO',
          SY-VLINE, (04) 'DYFM',
          SY-VLINE, (04) 'DYTO',
          SY-VLINE, (04) 'EFFG',
          SY-VLINE, (10) 'Eff. DT FM',
          SY-VLINE, (10) 'Eff. DT TO',
          SY-VLINE, (06) 'Vendor 1',
          SY-VLINE, (10) 'Ratio 1',
          SY-VLINE, (06) 'Vendor 2',
          SY-VLINE, (10) 'Ratio 2',
          SY-VLINE, (06) 'Vendor 3',
          SY-VLINE, (10) 'Ratio 3',
          SY-VLINE.
  WRITE:/(225) SY-ULINE.
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
        DESCRIBE TABLE IT_RATIO_USER LINES W_CNT.
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
  CLEAR ZTSD_RATIO_USER.
  W_CREATE = 'X'.
  ZTSD_RATIO_USER-ZITEM = P_ZITEM.
  ZTSD_RATIO_USER-ZGROP = P_ZGROP.
  ZTSD_RATIO_USER-ZVSEQ = W_ZVSEQ + 1.

  CALL SCREEN 9000.
ENDFORM.                    " CREATE_DATA
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DATA
*&---------------------------------------------------------------------*
FORM CHANGE_DATA.
  CLEAR W_SAVE.

  IF SY-LISEL+2(1) CA '0123456789'.
    READ TABLE IT_RATIO_USER WITH KEY ZVSEQ = SY-LISEL+2(3).
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING IT_RATIO_USER TO ZTSD_RATIO_USER.
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
      UPDATE ZTSD_RATIO_USER SET ZDTFG = 'X'
            WHERE ZITEM EQ P_ZITEM
            AND   ZGROP EQ P_ZGROP
            AND   ZVSEQ EQ SY-LISEL+2(3).
      IF SY-SUBRC = 0.
        MESSAGE I000 WITH TEXT-M09.
        DELETE IT_RATIO_USER WHERE ZITEM EQ P_ZITEM
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
  READ TABLE IT_RATIO_USER WITH KEY ZVSEQ = ZTSD_RATIO_USER-ZVSEQ.
  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING ZTSD_RATIO_USER TO IT_RATIO_USER.
    MODIFY IT_RATIO_USER INDEX SY-TABIX.

    MODIFY ZTSD_RATIO_USER.
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
    ZTSD_RATIO_USER-ZERDA = SY-DATUM.
    ZTSD_RATIO_USER-ERZET = SY-UZEIT.
    ZTSD_RATIO_USER-ERNAM = SY-UNAME.
    MOVE-CORRESPONDING ZTSD_RATIO_USER TO IT_RATIO_USER.
    APPEND IT_RATIO_USER.

    INSERT ZTSD_RATIO_USER FROM ZTSD_RATIO_USER.
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
