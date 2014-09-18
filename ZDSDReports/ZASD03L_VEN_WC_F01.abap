*----------------------------------------------------------------------*
*   INCLUDE ZASD03L_VEN_WC_F01                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE IT_VEN_WC
         FROM ZTSD_VEN_WC
        WHERE ZVEND EQ P_ZVEND.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM DISPLAY_DATA.
  DESCRIBE TABLE IT_VEN_WC LINES W_CNT.
  IF W_CNT = 0.
    CLEAR ZTSD_VEN_WC.
    ZTSD_VEN_WC-ZVEND = P_ZVEND.
    ZTSD_VEN_WC-ZVSEQ = 1.

    CALL SCREEN 9000.
  ELSE.
    PERFORM DISPLAY_LIST.
  ENDIF.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST
*&---------------------------------------------------------------------*
FORM DISPLAY_LIST.
  SORT IT_VEN_WC DESCENDING BY ZVSEQ.
  LOOP AT IT_VEN_WC.
    WRITE:/ SY-VLINE, (03) IT_VEN_WC-ZVSEQ,
            SY-VLINE, (18) IT_VEN_WC-ZCPTN,
            SY-VLINE, (06) IT_VEN_WC-ZPTG1,
            SY-VLINE, (06) IT_VEN_WC-ZPTG2,
            SY-VLINE, (10) IT_VEN_WC-ZMYFM,
            SY-VLINE, (10) IT_VEN_WC-ZMYTO,
            SY-VLINE, (10) IT_VEN_WC-ZEFFG,
            SY-VLINE, (10) IT_VEN_WC-ZEFFM,
            SY-VLINE, (10) IT_VEN_WC-ZEFTO,
            SY-VLINE, (08) IT_VEN_WC-ZCTYP,
            SY-VLINE, (08) IT_VEN_WC-ZPRTO,
            SY-VLINE, (08) IT_VEN_WC-ZSHTO,
            SY-VLINE, (08) IT_VEN_WC-ZDVTO,
            SY-VLINE, (08) IT_VEN_WC-ZRPTO,
            SY-VLINE, (09) IT_VEN_WC-ZODRD,
            SY-VLINE.
    WRITE:/(178) SY-ULINE.

    IF W_ZVSEQ < IT_VEN_WC-ZVSEQ.
      W_ZVSEQ = IT_VEN_WC-ZVSEQ.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " DISPLAY_RESULT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  WRITE:/ ' Vendor code :', P_ZVEND.
  FORMAT COLOR COL_HEADING.
  WRITE:/(178) SY-ULINE.
  WRITE:/ SY-VLINE, (03) 'Seq',
          SY-VLINE, (18) 'Causal Part No',
          SY-VLINE, (06) 'Part 1',
          SY-VLINE, (06) 'Part 2',
          SY-VLINE, (10) 'Model Yr F',
          SY-VLINE, (10) 'Model Yr T',
          SY-VLINE, (10) 'Eff. DT FG',
          SY-VLINE, (10) 'Eff. DT FM',
          SY-VLINE, (10) 'Eff. DT TO',
          SY-VLINE, (08) 'Claim Ty',
          SY-VLINE, (08) 'Prod. To',
          SY-VLINE, (08) 'Ship. To',
          SY-VLINE, (08) 'Dlvy. To',
          SY-VLINE, (08) 'Repr. To',
          SY-VLINE, (09) 'Wrt. Mlg.',
          SY-VLINE.
  WRITE:/(178) SY-ULINE.
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
        DESCRIBE TABLE IT_VEN_WC LINES W_CNT.
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
  CLEAR ZTSD_VEN_WC.
  ZTSD_VEN_WC-ZVEND = P_ZVEND.
  ZTSD_VEN_WC-ZVSEQ = W_ZVSEQ + 1.

  CALL SCREEN 9000.
ENDFORM.                    " CREATE_DATA
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DATA
*&---------------------------------------------------------------------*
FORM CHANGE_DATA.
  CLEAR W_SAVE.

  IF SY-LISEL+2(1) CA '0123456789'.
    READ TABLE IT_VEN_WC WITH KEY ZVSEQ = SY-LISEL+2(3).
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING IT_VEN_WC TO ZTSD_VEN_WC.
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
      DELETE FROM ZTSD_VEN_WC
            WHERE ZVEND EQ P_ZVEND
            AND   ZVSEQ EQ SY-LISEL+2(3).
      IF SY-SUBRC = 0.
        MESSAGE I000 WITH TEXT-M09.
        DELETE IT_VEN_WC WHERE ZVEND EQ P_ZVEND
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
  READ TABLE IT_VEN_WC WITH KEY ZVSEQ = ZTSD_VEN_WC-ZVSEQ.
  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING ZTSD_VEN_WC TO IT_VEN_WC.
    MODIFY IT_VEN_WC INDEX SY-TABIX.

    MODIFY ZTSD_VEN_WC.
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
    ZTSD_VEN_WC-ZERDA = SY-DATUM.
    ZTSD_VEN_WC-ERZET = SY-UZEIT.
    ZTSD_VEN_WC-ERNAM = SY-UNAME.
    MOVE-CORRESPONDING ZTSD_VEN_WC TO IT_VEN_WC.
    APPEND IT_VEN_WC.

    INSERT ZTSD_VEN_WC FROM ZTSD_VEN_WC.
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
