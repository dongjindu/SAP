*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM01I02                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입 B/L 관련 PAI MODULE Include                      *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.21                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR7110  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR7110 INPUT.

  IF W_STATUS NE 'D'.
    REFRESH IT_ZSBLUGC_DEL.
    CLEAR   IT_ZSBLUGC_DEL.
    LOOP AT IT_ZSBLUGC.
      IF OK-CODE = 'MKA1'.
        MOVE   'X'             TO IT_ZSBLUGC-ZFMARK.
      ENDIF.
      IF OK-CODE = 'MKL1'.
        CLEAR  IT_ZSBLUGC-ZFMARK.
      ENDIF.
      IF OK-CODE = 'DEL1' AND IT_ZSBLUGC-ZFMARK = 'X'.
      ELSE.
        MOVE-CORRESPONDING IT_ZSBLUGC TO IT_ZSBLUGC_DEL.
        APPEND IT_ZSBLUGC_DEL.
      ENDIF.
    ENDLOOP.

    REFRESH IT_ZSBLUGC.
    LOOP AT IT_ZSBLUGC_DEL.
      CLEAR   IT_ZSBLUGC.
      MOVE-CORRESPONDING IT_ZSBLUGC_DEL TO IT_ZSBLUGC.
      APPEND IT_ZSBLUGC.
    ENDLOOP.
  ENDIF.

  CASE OK-CODE.
    WHEN 'REF1'.
      PERFORM  P2000_REF1_PROCESS_SCR7110.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR7110  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR7100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR7100 INPUT.

  MOVE C_REQ_C TO W_STATUS.

  SET SCREEN 7110.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR7100  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR7110  INPUT
*&---------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR7110 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_7110-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " IT_GET_LINE_SCR7110  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR7200  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR7200 INPUT.

  MOVE C_REQ_U TO W_STATUS.

  SET SCREEN 7110.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR7200  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR3110  INPUT
*&---------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR3110 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_3111-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " IT_GET_LINE_SCR3110  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR3110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE  USER_COMMAND_SCR3110 INPUT.

  IF W_STATUS NE 'D'.
    REFRESH IT_ZSIVIT_DEL.
    CLEAR   IT_ZSIVIT_DEL.
    LOOP AT IT_ZSIVIT.
      IF OK-CODE = 'MKA1'.
        MOVE   'X'             TO IT_ZSIVIT-ZFMARK.
      ENDIF.
      IF OK-CODE = 'MKL1'.
        CLEAR  IT_ZSIVIT-ZFMARK.
      ENDIF.
      IF OK-CODE = 'DEL1' AND IT_ZSIVIT-ZFMARK = 'X'.
      ELSE.
        MOVE-CORRESPONDING IT_ZSIVIT TO IT_ZSIVIT_DEL.
        APPEND IT_ZSIVIT_DEL.
      ENDIF.
    ENDLOOP.

    REFRESH IT_ZSIVIT.

    DESCRIBE TABLE IT_ZSIVIT_DEL LINES LINE.

    IF LINE > 0.
      CLEAR : W_ZFIVAMT_S, W_ZFIVAMP_S.
      LOOP AT IT_ZSIVIT_DEL.
        CLEAR   IT_ZSIVIT.
        MOVE-CORRESPONDING IT_ZSIVIT_DEL TO IT_ZSIVIT.
        PERFORM QTY_ITAMT_IVAMT_SCR3110.
        APPEND IT_ZSIVIT.
      ENDLOOP.
    ENDIF.

*     PERFORM DIST_PROCESS_SCR3110.
    ZTIV-ZFPKCHG =
         ZTIV-ZFPKCHG *  ( 100 - ZTREQHD-ZFPREPAY ) / 100.
    ZTIV-ZFHDCHG =
         ZTIV-ZFHDCHG *  ( 100 - ZTREQHD-ZFPREPAY ) / 100.
*    IF ZTIV-ZFPRPYN = 'Y'.
*       ZTIV-ZFPKCHGP = ZTIV-ZFPKCHG * ZTREQHD-ZFPREPAY / 100.
*       ZTIV-ZFHDCHGP = ZTIV-ZFHDCHG * ZTREQHD-ZFPREPAY / 100.
*    ENDIF.
    MOVE W_ZFIVAMP_S   TO ZTIV-ZFIVAMT.
    ADD  ZTIV-ZFPKCHG  TO ZTIV-ZFIVAMT.
    ADD  ZTIV-ZFHDCHG  TO ZTIV-ZFIVAMT.
*    IF ZTIV-ZFIVAMT > ZTIV-ZFIVAMP.
*      ZTIV-ZFPIVAM = ZTIV-ZFIVAMT - ZTIV-ZFIVAMP.
*    ELSE.
*      ZTIV-ZFPIVAM = 0.
*    ENDIF.
*    IF ZTIV-ZFPRPYN = 'Y'.
*      ZTIV-ZFPIVAM = 0.
*    ENDIF.

    W_ZFIVPKHD = W_ZFIVAMT_S + ZTIV-ZFPKCHG + ZTIV-ZFHDCHG.
    IF ZTIV-ZFIVAMT NE W_ZFIVPKHD.
      MESSAGE W784.
    ENDIF.
    IF ZTIV-ZFPOYN = 'Y'.
*      SELECT SUM( ZFPKCHGP ) SUM( ZFHDCHGP ) SUM( ZFIVAMP )
*        INTO (W_ZFPKCHG, W_ZFHDCHG, W_ZFIVAMP)
*        FROM ZTIV
*       WHERE ZFREQNO = ZTIV-ZFREQNO
*         AND ZFIVNO NE ZTIV-ZFIVNO.
*        IF W_STATUS = 'C'.
      ADD ZTIV-ZFPKCHG TO W_ZFPKCHG.
      ADD ZTIV-ZFHDCHG TO W_ZFHDCHG.
      ADD ZTIV-ZFIVAMT  TO W_ZFIVAMP.
*        ENDIF.
      IF ZTREQHD-ZFPKCHG < W_ZFPKCHG.
        MESSAGE W860.
      ENDIF.
      IF ZTREQHD-ZFHDCHG < W_ZFHDCHG.
        MESSAGE W861.
      ENDIF.
      IF ZTREQHD-ZFLASTAM < W_ZFIVAMP.
        MESSAGE W877.
      ENDIF.
    ENDIF.
*>> 선급금이 있을 경우....
*    IF ZTIV-ZFPRPYN EQ 'Y' AND NOT ZTIV-ZFPRTE IS INITIAL.
*       MOVE : ZTIV-ZFIVAMP  TO  ZTIV-ZFPIVAM.
*    ENDIF.
  ENDIF.                               "IF W_STATUS NE 'D'.

  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR3110  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR3200  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR3200 INPUT.

  IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.

    CASE SY-DYNNR.
      WHEN '3200' OR '3201'.
        MOVE C_REQ_U TO W_STATUS.
      WHEN '3300' OR '3301'.
        MOVE C_REQ_D TO W_STATUS.
      WHEN '3400' OR '3401'.
        MOVE C_OPEN_U TO W_STATUS.
    ENDCASE.

    SET SCREEN 3110.  LEAVE SCREEN.
  ENDIF.
ENDMODULE.                             " USER_COMMAND_SCR3200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR5700  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR5700 INPUT.

  MOVE C_REQ_U TO W_STATUS.

  SET SCREEN 5710.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR5700  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR5710  INPUT
*&---------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR5710 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_5710-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " IT_GET_LINE_SCR5710  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR5710  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR5710 INPUT.

  IF SY-TCODE = 'ZIM57'.
    CLEAR : W_ZFIVAMP_S, W_ZFDAMT_S, W_ZFUPCST_S,
            W_ZFPCST_S,  W_ZFIMOC_S, W_ZFIVAMK_S.
    LOOP AT IT_ZSIVIT.
      READ TABLE IT_ZSIVIT  WITH KEY IT_ZSIVIT(18)  BINARY SEARCH.
*          IF IT_ZSIVIT-ZFPRQN IS INITIAL.
*             IT_ZSIVIT-ZFPRQN = IT_ZSIVIT-MENGE *
*                               ( 100 - ZTIV-ZFPRTE ) / 100.
*          ENDIF.
*          IF IT_ZSIVIT-ZFIVAMP IS INITIAL.
*             IT_ZSIVIT-ZFIVAMP = IT_ZSIVIT-ZFIVAMT *
*                               ( 100 - ZTIV-ZFPRTE ) / 100.
*          ENDIF.
*          IF IT_ZSIVIT-ZFPCST  IS INITIAL.
*             IF ZTIV-ZFIVAMC EQ ZTIV-ZFKRW.
*                IT_ZSIVIT-ZFPCST = IT_ZSIVIT-ZFIVAMT *
*                                   ZTIV-ZFPLRTE / 100.
*             ELSE.
*                IT_ZSIVIT-ZFPCST = IT_ZSIVIT-ZFIVAMT * ZTIV-ZFEXRT *
*                                   ZTIV-ZFPLRTE / 100.
*             ENDIF.
*          ENDIF.
*          IT_ZSIVIT-ZFIMOC = IT_ZSIVIT-ZFDAMT + IT_ZSIVIT-ZFUPCST +
*                             IT_ZSIVIT-ZFPCST.
*          IT_ZSIVIT-ZFIVAMK = IT_ZSIVIT-ZFIVAMT * ZTIV-ZFEXRT.
*      ADD IT_ZSIVIT-ZFIVAMP TO W_ZFIVAMP_S.
      ADD IT_ZSIVIT-ZFDAMT  TO W_ZFDAMT_S.
      ADD IT_ZSIVIT-ZFUPCST TO W_ZFUPCST_S.
      ADD IT_ZSIVIT-ZFPCST  TO W_ZFPCST_S.
      ADD IT_ZSIVIT-ZFIMOC  TO W_ZFIMOC_S.
      ADD IT_ZSIVIT-ZFIVAMK TO W_ZFIVAMK_S.
      MODIFY IT_ZSIVIT   INDEX SY-TABIX.
    ENDLOOP.

    ADD  ZTIV-ZFPKCHG   TO W_ZFIVAMP_S.
    ADD  ZTIV-ZFHDCHG   TO W_ZFIVAMP_S.
    MOVE W_ZFIVAMP_S    TO ZTIV-ZFIVAMT.
    MOVE W_ZFDAMT_S     TO ZTIV-ZFDAMT.
    MOVE W_ZFUPCST_S    TO ZTIV-ZFUPCST.
    MOVE W_ZFPCST_S     TO ZTIV-ZFPCST.
    MOVE W_ZFIVAMK_S    TO ZTIV-ZFIVAMK.
    MOVE W_ZFIMOC_S     TO W_ZFIMOC_SUM.
*    IF ZTIV-ZFIVAMT > ZTIV-ZFIVAMP.
*      ZTIV-ZFPIVAM = ZTIV-ZFIVAMT - ZTIV-ZFIVAMP.
*    ELSE.
*      ZTIV-ZFPIVAM = 0.
*    ENDIF.
  ENDIF.                               "IF W_STATUS NE 'D'.

  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR5710  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR3400  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR3400 INPUT.

  MOVE C_REQ_U TO W_STATUS.

  SET SCREEN 5710.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR3400  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6700  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6700 INPUT.

  MOVE C_REQ_C TO W_STATUS.

  SET SCREEN 6710.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR6700  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR6710  INPUT
*&---------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR6710 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_6710-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " IT_GET_LINE_SCR6710  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6800  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6800 INPUT.

  MOVE C_REQ_U TO W_STATUS.

  SET SCREEN 6710.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR6800  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6200  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6200 INPUT.

  MOVE C_REQ_U  TO W_STATUS.
  MOVE SY-DATUM TO ZTIDR-ZFIDWDT.      "수입신고 희망?

  SET SCREEN 6210.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR6200  INPUT
*&---------------------------------------------------------------------*
*&      Module  REQIT_GET_LINE_SCR6216  INPUT
*&---------------------------------------------------------------------*
MODULE REQIT_GET_LINE_SCR6216 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_6216-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " REQIT_GET_LINE_SCR6216  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6210  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6210 INPUT.

  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE' OR 'SVCO'.
      MOVE OK-CODE TO W_OK_CODE.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'REOG'.
      PERFORM  P2000_REOG_DISPLAY.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR6210  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6216  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6216 INPUT.

  CASE OK-CODE.
    WHEN 'NEHS'.
      CLEAR : ZTIDRHS.
      MOVE : ZTIDR-ZFBLNO   TO  ZTIDRHS-ZFBLNO,
             ZTIDR-ZFCLSEQ  TO  ZTIDRHS-ZFCLSEQ.
      PERFORM  P2000_SCR6217_HS_PROCESS.
    WHEN 'HS'.                         " 란사?
      W_COUNT = 0.
      LOOP AT IT_ZSIDRHS WHERE ZFMARK NE SPACE.
        W_COUNT = W_COUNT + 1.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.          MESSAGE  E962.
        WHEN 1.          PERFORM  P2000_SCR6217_HS_PROCESS.
        WHEN OTHERS.     MESSAGE  E965.
      ENDCASE.
    WHEN 'HSD'.                        " 규격명?
      W_COUNT = 0.
      LOOP AT IT_ZSIDRHS WHERE ZFMARK NE SPACE.
        W_COUNT = W_COUNT + 1.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.          MESSAGE  E962.
        WHEN 1.
           MOVE-CORRESPONDING  IT_ZSIDRHS  TO  ZTIDRHS.
           PERFORM  P2000_SCR6218_HSD_PROCESS.
        WHEN OTHERS.     MESSAGE  E965.
      ENDCASE.
    WHEN 'HSL'.                        " 요건확?
      W_COUNT = 0.
      LOOP AT IT_ZSIDRHS WHERE ZFMARK NE SPACE.
        W_COUNT = W_COUNT + 1.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.          MESSAGE  E962.
        WHEN 1.          PERFORM  P2000_SCR6219_HSL_PROCESS.
        WHEN OTHERS.     MESSAGE  E965.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR6216  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6710  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6710 INPUT.

  IF W_STATUS NE 'D'.
    REFRESH IT_ZSCUCLIVIT_DEL.
    CLEAR   IT_ZSCUCLIVIT_DEL.
    LOOP AT IT_ZSCUCLIVIT.
      IF OK-CODE = 'MKA1'.
        MOVE   'X'             TO IT_ZSCUCLIVIT-ZFMARK.
      ENDIF.
      IF OK-CODE = 'MKL1'.
        CLEAR  IT_ZSCUCLIVIT-ZFMARK.
      ENDIF.
      IF OK-CODE = 'DEL1' AND IT_ZSCUCLIVIT-ZFMARK = 'X'.
      ELSE.
        MOVE-CORRESPONDING IT_ZSCUCLIVIT TO IT_ZSCUCLIVIT_DEL.
        APPEND IT_ZSCUCLIVIT_DEL.
      ENDIF.
    ENDLOOP.

    REFRESH IT_ZSCUCLIVIT.

    DESCRIBE TABLE IT_ZSCUCLIVIT_DEL LINES LINE.

    IF LINE > 0.
      CLEAR : W_ZFIVAMT_S.
      LOOP AT IT_ZSCUCLIVIT_DEL.
        CLEAR   IT_ZSCUCLIVIT.
        MOVE-CORRESPONDING IT_ZSCUCLIVIT_DEL TO IT_ZSCUCLIVIT.
        PERFORM QTY_ITAMT_IVAMT_SCR6710.
        APPEND IT_ZSCUCLIVIT.
      ENDLOOP.
    ENDIF.

*          IF ZTCUCLIV-ZFIVAMT IS INITIAL.
    MOVE W_ZFIVAMT_S TO ZTCUCLIV-ZFIVAMT.
*          ELSE.
*             PERFORM DIST_PROCESS_SCR6710.
*          ENDIF.
  ENDIF.                           "IF W_STATUS NE 'D'.

  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN 'COST'.
      PERFORM  P2000_CALL_COST_SCREEN.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR6710  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR6217_HS_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SCR6217_HS_PROCESS.

  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '란사항' INTO SPOP-TITEL
                                        SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  IF OK-CODE NE 'NEHS'.
     MOVE-CORRESPONDING    IT_ZSIDRHS     TO   ZTIDRHS.
  ENDIF.
  CALL SCREEN 6217 STARTING AT 15 1
                   ENDING   AT 100 30.

  IF ANTWORT EQ 'Y'.

    READ TABLE IT_ZSIDRHS WITH KEY ZFBLNO  = ZTIDRHS-ZFBLNO
                                   ZFCLSEQ = ZTIDRHS-ZFCLSEQ
                                   ZFCONO  = ZTIDRHS-ZFCONO.
    IF SY-SUBRC NE 0.
       MOVE-CORRESPONDING  ZTIDRHS  TO  IT_ZSIDRHS.
       APPEND  IT_ZSIDRHS.
    ELSE.
       MOVE-CORRESPONDING    ZTIDRHS     TO   IT_ZSIDRHS.
       MODIFY IT_ZSIDRHS INDEX SY-TABIX.
    ENDIF.
  ENDIF.

ENDFORM.                               " P2000_SCR6217_HS_PROCESS
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR6217  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR6217 INPUT.

  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.
      PERFORM  P2000_HS_DATA_CHECK.
      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.
      ANTWORT = 'N'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'HSD'.                        " 규격명세.
      PERFORM  P2000_HS_DATA_CHECK.
      PERFORM  P2000_SCR6218_HSD_PROCESS.
    WHEN 'HSL'.                        " 요건확인.
      PERFORM  P2000_SCR6219_HSL_PROCESS.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " GET_OK_CODE_SCR6217  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR6218  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR6218 INPUT.

  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'DEL1' .          " 라인 삭제.
      LOOP AT IT_ZSIDRHSD_S   WHERE ZFMARK NE SPACE.
         DELETE IT_ZSIDRHSD_S  INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'INS1'.           " 삽입.
      IF LINE GT 0.
         CLEAR : IT_ZSBLCST.
         MOVE : ZTIDRHS-ZFBLNO  TO   IT_ZSIDRHSD_S-ZFBLNO,
                ZTIDRHS-ZFCLSEQ TO   IT_ZSIDRHSD_S-ZFCLSEQ,
                ZTIDRHS-ZFCONO  TO   IT_ZSIDRHSD_S-ZFCONO,
                ZTIDRHS-STAWN   TO   IT_ZSIDRHSD_S-STAWN.
        SELECT  MAX( ZFRONO )  INTO  W_RONO
        FROM    ZTIDRHSD
        WHERE   ZFBLNO          EQ   IT_ZSIDRHSD_S-ZFBLNO
        AND     ZFCLSEQ         EQ   IT_ZSIDRHSD_S-ZFCLSEQ
        AND     ZFCONO          EQ   IT_ZSIDRHSD_S-ZFCONO.

        LOOP  AT  IT_ZSIDRHSD_S.
           IF  W_RONO  LE  IT_ZSIDRHSD_S-ZFRONO.
               MOVE   IT_ZSIDRHSD_S-ZFRONO  TO  W_RONO.
           ENDIF.
        ENDLOOP.
        W_RONO  =  W_RONO  +  10.
        MOVE   W_RONO          TO   IT_ZSIDRHSD_S-ZFRONO.
        INSERT  IT_ZSIDRHSD_S INDEX  LINE.
      ELSE.
        MESSAGE S962.
      ENDIF.
    WHEN 'CANC'.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.
      PERFORM  P2000_ITEM_DATA_CHECK.
      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.
      ANTWORT = 'N'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " GET_OK_CODE_SCR6218  INPUT

*&---------------------------------------------------------------------*
*&      Form  P2000_SCR6218_HSD_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SCR6218_HSD_PROCESS.

  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '행사항' INTO SPOP-TITEL
                                        SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  REFRESH IT_ZSIDRHSD_S.
  CLEAR   IT_ZSIDRHSD.
  LOOP AT IT_ZSIDRHSD WHERE ZFBLNO  EQ  ZTIDRHS-ZFBLNO
                      AND   ZFCLSEQ EQ  ZTIDRHS-ZFCLSEQ
                      AND   ZFCONO  EQ  ZTIDRHS-ZFCONO.
    CLEAR   IT_ZSIDRHSD_S.
    MOVE-CORRESPONDING IT_ZSIDRHSD TO IT_ZSIDRHSD_S.
    APPEND IT_ZSIDRHSD_S.
  ENDLOOP.

  CALL SCREEN 6218 STARTING AT 15 1
                   ENDING   AT 100 15.

  IF ANTWORT EQ 'Y'.
    LOOP AT IT_ZSIDRHSD_S.
      READ TABLE IT_ZSIDRHSD WITH KEY ZFBLNO  = IT_ZSIDRHSD_S-ZFBLNO
                                      ZFCLSEQ = IT_ZSIDRHSD_S-ZFCLSEQ
                                      ZFCONO  = IT_ZSIDRHSD_S-ZFCONO
                                      ZFRONO  = IT_ZSIDRHSD_S-ZFRONO.
      IF SY-SUBRC NE 0.
         MOVE-CORRESPONDING  IT_ZSIDRHSD_S   TO   IT_ZSIDRHSD.
         APPEND  IT_ZSIDRHSD.
      ELSE.
         MOVE-CORRESPONDING    IT_ZSIDRHSD_S    TO   IT_ZSIDRHSD.
         MODIFY IT_ZSIDRHSD    INDEX SY-TABIX.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " P2000_SCR6218_HSD_PROCESS
*&---------------------------------------------------------------------*
*&      Module  REQIT_GET_LINE_SCR6218  INPUT
*&---------------------------------------------------------------------*
MODULE REQIT_GET_LINE_SCR6218 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_6218-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " REQIT_GET_LINE_SCR6218  INPUT
*&---------------------------------------------------------------------*
*&      Module  REQIT_GET_LINE_SCR6219  INPUT
*&---------------------------------------------------------------------*
MODULE REQIT_GET_LINE_SCR6219 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_6219-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " REQIT_GET_LINE_SCR6219  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR6219  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR6219 INPUT.

  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.
      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.
      ANTWORT = 'N'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " GET_OK_CODE_SCR6219  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR6219_HSL_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SCR6219_HSL_PROCESS.

  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '요건사항' INTO SPOP-TITEL
                                        SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  REFRESH IT_ZSIDRHSL_S.
  CLEAR   IT_ZSIDRHSL.
  LOOP AT IT_ZSIDRHSL.
    IF IT_ZSIDRHSL-ZFCONO NE IT_ZSIDRHS-ZFCONO.
      CONTINUE.
    ENDIF.
    CLEAR   IT_ZSIDRHSL_S.
    MOVE-CORRESPONDING IT_ZSIDRHSL TO IT_ZSIDRHSL_S.
    APPEND IT_ZSIDRHSL_S.
  ENDLOOP.

  CALL SCREEN 6219 STARTING AT 15 1
                   ENDING   AT 95 15.

  IF ANTWORT EQ 'Y'.
    DELETE IT_ZSIDRHSL WHERE ZFCONO EQ IT_ZSIDRHS-ZFCONO.
    LOOP AT IT_ZSIDRHSL_S.
      MOVE-CORRESPONDING    IT_ZSIDRHSL_S    TO   IT_ZSIDRHSL.
      APPEND IT_ZSIDRHSL.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " P2000_SCR6219_HSL_PROCESS
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR3300  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR3300 INPUT.

  MOVE C_REQ_D TO W_STATUS.

  SET SCREEN 3110.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR3300  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6300  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6300 INPUT.

  MOVE C_REQ_D TO W_STATUS.

  SET SCREEN 6210.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR6300  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8100 INPUT.

  MOVE C_REQ_C TO W_STATUS.

  CASE OK-CODE.
    WHEN  'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN  OTHERS.
      IF  ZTREQHD-ZFREQTY  =  'LO'.
        SET  SCREEN  8220.
      ELSE.
        SET  SCREEN  8210.
      ENDIF.
  ENDCASE.
  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR8100  INPUT
*&---------------------------------------------------------------------*
*&      Module  REQIT_GET_LINE_SCR6212  INPUT
*&---------------------------------------------------------------------*
MODULE IDRHSD_GET_LINE_SCR6212 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_6212-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " IDRHSD_GET_LINE_SCR6212  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6500  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6500 INPUT.

  MOVE C_REQ_D TO W_STATUS.

  SET SCREEN 6410.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR6500  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6400  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6400 INPUT.

  MOVE C_REQ_U TO W_STATUS.

  SET SCREEN 6410.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR6400  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR6410  INPUT
*&---------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR6410 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_6410-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " IT_GET_LINE_SCR6410  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6410  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6410 INPUT.

  CASE OK-CODE.
    WHEN 'IT'.                         " 감면허가품?
      W_COUNT = 0.
      LOOP AT IT_ZSIDRCR WHERE ZFMARK NE SPACE.
        W_COUNT = W_COUNT + 1.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.          MESSAGE  E962.
        WHEN 1.          PERFORM  P2000_SCR6410_IT_PROCESS.
        WHEN OTHERS.     MESSAGE  E965.
      ENDCASE.
    WHEN 'DEL1'.
      LOOP AT IT_ZSIDRCR WHERE ZFMARK = 'X'.
        DELETE IT_ZSIDRCR.
        DELETE IT_ZSIDRCRIT
         WHERE ZFBLNO  EQ IT_ZSIDRCR-ZFBLNO
         AND ZFCLSEQ EQ IT_ZSIDRCR-ZFCLSEQ
         AND ZFCONO  EQ IT_ZSIDRCR-ZFCONO.
      ENDLOOP.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR6410  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6500  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6450 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  MOVE C_REQ_C TO W_STATUS.

  SET SCREEN 6410.  LEAVE SCREEN.


ENDMODULE.                             " USER_COMMAND_SCR6450  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR6412  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR6412 INPUT.

  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.
      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.
      ANTWORT = 'N'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " GET_OK_CODE_SCR6412  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR6410_IT_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SCR6410_IT_PROCESS.

  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '감면허가품목' INTO SPOP-TITEL
                                        SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.
  REFRESH IT_ZSIDRCRIT_S.
  CLEAR   IT_ZSIDRCRIT.
  LOOP AT IT_ZSIDRCRIT.
    IF IT_ZSIDRCRIT-ZFCONO NE IT_ZSIDRCR-ZFCONO.
      CONTINUE.
    ENDIF.
    CLEAR   IT_ZSIDRCRIT_S.
    MOVE-CORRESPONDING IT_ZSIDRCRIT TO IT_ZSIDRCRIT_S.
    APPEND IT_ZSIDRCRIT_S.
  ENDLOOP.

  CALL SCREEN 6412 STARTING AT 15 1
                   ENDING   AT 100 16.

  IF ANTWORT EQ 'Y'.
    LOOP AT IT_ZSIDRCRIT.
      W_TABIX = SY-TABIX.
      READ TABLE IT_ZSIDRCRIT_S WITH  KEY ZFCONO = IT_ZSIDRCRIT-ZFCONO
                                        ZFRONO = IT_ZSIDRCRIT-ZFRONO.
      MOVE-CORRESPONDING IT_ZSIDRCRIT_S TO IT_ZSIDRCRIT.
      IF SY-SUBRC EQ 0.
        MODIFY IT_ZSIDRCRIT INDEX W_TABIX .
      ELSE.
        DELETE IT_ZSIDRCRIT
                      WHERE ZFCONO EQ IT_ZSIDRCRIT_S-ZFCONO
                      AND   ZFRONO NE IT_ZSIDRCRIT_S-ZFRONO.
      ENDIF.
    ENDLOOP.

    LOOP AT IT_ZSIDRCRIT_S.
      READ TABLE IT_ZSIDRCRIT WITH  KEY ZFCONO = IT_ZSIDRCRIT_S-ZFCONO
                                        ZFRONO = IT_ZSIDRCRIT_S-ZFRONO.
      W_TABIX = SY-TABIX.
      MOVE-CORRESPONDING IT_ZSIDRCRIT_S TO IT_ZSIDRCRIT.
      IF SY-SUBRC EQ 0.
        MODIFY IT_ZSIDRCRIT INDEX W_TABIX .
      ELSE.
        APPEND IT_ZSIDRCRIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                               " P2000_SCR6410_IT_PROCESS
*&---------------------------------------------------------------------*
*&      Module  REQIT_GET_LINE_SCR6412  INPUT
*&---------------------------------------------------------------------*
MODULE REQIT_GET_LINE_SCR6412 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_6412-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " REQIT_GET_LINE_SCR6412  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6600  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6600 INPUT.

  MOVE C_REQ_U TO W_STATUS.

  SET SCREEN 6610.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR6600  INPUT

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIDRDTU
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIDRDTU.

  REFRESH IT_ZSIDRDTU_DEL.
  REFRESH IT_ZSIDRDTU.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIDRDTU
                                        FROM ZTIDRDTU.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIDRDTU'.
  ENDIF.

ENDFORM.                               " P1000_READ_ZTIDRDTU
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR6610  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR6610 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_6610-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " GET_LINE_SCR6610  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_6610_UPDATE_SCR6610  INPUT
*&---------------------------------------------------------------------*
MODULE TC_6610_UPDATE_SCR6610 INPUT.

  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  READ TABLE IT_ZSIDRDTU INDEX TC_6610-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.

  MOVE-CORRESPONDING ZSIDRDTU TO IT_ZSIDRDTU.
  IF OK-CODE = 'MKAL'.
    MOVE   'X'             TO W_ROW_MARK.
  ENDIF.
  IF OK-CODE = 'MKLO'.
    CLEAR  W_ROW_MARK.
  ENDIF.
  MOVE W_ROW_MARK TO IT_ZSIDRDTU-ZFMARK.

  IF IT_ZSIDRDTU-ZFCONO IS INITIAL.    " 란번?
    MESSAGE E820.
  ENDIF.
  IF IT_ZSIDRDTU-ZFRONO IS INITIAL.    " 행번?
    MESSAGE E821.
  ENDIF.

  SELECT SINGLE *
    FROM ZTIDRCRIT
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ
     AND ZFCONO = IT_ZSIDRDTU-ZFCONO
     AND ZFRONO = IT_ZSIDRDTU-ZFRONO.
  IF SY-SUBRC NE 0.
    MESSAGE E822.
  ENDIF.
  IF ZTIDRCRIT-ZFDETY NE '4'.
    MESSAGE E825.
  ENDIF.

  MOVE ZTIDRCRIT-ZFDEQN       TO IT_ZSIDRDTU-ZFDEQN.
  MOVE ZTIDRCRIT-ZFDEQNM      TO IT_ZSIDRDTU-ZFDEQNM.
  MOVE ZTIDRCRIT-MAKTX        TO IT_ZSIDRDTU-MAKTX.
  MOVE ZTIDRCRIT-ZFDEQNM      TO IT_ZSIDRDTU-ZFUSQNM.

  IF W_OLD_SUBRC = 0.
    MODIFY IT_ZSIDRDTU INDEX TC_6610-CURRENT_LINE.
  ELSE.
    APPEND IT_ZSIDRDTU.
  ENDIF.

ENDMODULE.                             " TC_6610_UPDATE_SCR6610  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6610  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6610 INPUT.

  CASE OK-CODE.
    WHEN 'REFR'.
      PERFORM READ_IDRDTU_TO_IT.
    WHEN OTHERS.
      IF W_STATUS NE 'D'.
        REFRESH IT_ZSIDRDTU_DEL.
        CLEAR   IT_ZSIDRDTU_DEL.
        LOOP AT IT_ZSIDRDTU.
          IF OK-CODE = 'MKAL'.
            MOVE   'X'             TO IT_ZSIDRDTU-ZFMARK.
          ENDIF.
          IF OK-CODE = 'MKLO'.
            CLEAR  IT_ZSIDRDTU-ZFMARK.
          ENDIF.
          IF OK-CODE = 'DELE' AND IT_ZSIDRDTU-ZFMARK = 'X'.
          ELSE.
            MOVE-CORRESPONDING IT_ZSIDRDTU TO IT_ZSIDRDTU_DEL.
            APPEND IT_ZSIDRDTU_DEL.
          ENDIF.
        ENDLOOP.

        REFRESH IT_ZSIDRDTU.

        DESCRIBE TABLE IT_ZSIDRDTU_DEL LINES LINE.

        IF LINE > 0.
          LOOP AT IT_ZSIDRDTU_DEL.
            CLEAR   IT_ZSIDRDTU.
            MOVE-CORRESPONDING IT_ZSIDRDTU_DEL TO IT_ZSIDRDTU.
            APPEND IT_ZSIDRDTU.
          ENDLOOP.
        ENDIF.

      ENDIF.                           "IF W_STATUS NE 'D'.
  ENDCASE.

  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR6610  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8300  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8300 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

*  IF W_ZFPOSDT_FROM IS INITIAL.        " Posting Date
*    SELECT MIN( BUDAT ) INTO W_ZFPOSDT_FROM
*      FROM BKPF
*     WHERE BLART = 'RE'.
*  ENDIF.
*  IF W_ZFPOSDT_TO IS INITIAL.
*    SELECT MAX( BUDAT ) INTO W_ZFPOSDT_TO
*      FROM BKPF
*     WHERE BLART = 'RE'.
*  ENDIF.

*  IF W_ZFDODT_FROM IS INITIAL.         " Document Date
*    SELECT MIN( BLDAT ) INTO W_ZFDODT_FROM
*      FROM BKPF
*     WHERE BLART = 'RE'.
*  ENDIF.
*  IF W_ZFDODT_TO   IS INITIAL.
*    SELECT MAX( BLDAT ) INTO W_ZFDODT_TO
*      FROM BKPF
*     WHERE BLART = 'RE'.
*  ENDIF.

  IF W_ZFDODT_FROM   IS INITIAL.
    MESSAGE E794.
    EXIT.
  ENDIF.

  IF W_LIFNR_FROM IS INITIAL.          " Vendor
    SELECT MIN( LIFNR ) INTO W_LIFNR_FROM
      FROM LFA1.
  ENDIF.
  IF W_LIFNR_TO   IS INITIAL.
    SELECT MAX( LIFNR ) INTO W_LIFNR_TO
      FROM LFA1.
  ENDIF.

  IF W_EBELN_FROM IS INITIAL.          " P/O No
    SELECT MIN( EBELN ) INTO W_EBELN_FROM
      FROM EKKO.
  ENDIF.
  IF W_EBELN_TO   IS INITIAL.
    SELECT MAX( EBELN ) INTO W_EBELN_TO
      FROM EKKO.
  ENDIF.

  IF W_ZFOPNNO_FROM IS INITIAL.        " L/C No
    SELECT MIN( ZFOPNNO ) INTO W_ZFOPNNO_FROM
      FROM ZTREQHD.
  ENDIF.
  IF W_ZFOPNNO_TO   IS INITIAL.
    SELECT MAX( ZFOPNNO ) INTO W_ZFOPNNO_TO
      FROM ZTREQHD.
  ENDIF.

  IF W_ZFGFDYR_FROM IS INITIAL.
    SELECT MIN( GJAHR ) INTO W_ZFGFDYR_FROM
      FROM BKPF
     WHERE BLART = 'RE'.
  ENDIF.
  IF W_ZFGFDYR_TO   IS INITIAL.
    SELECT MAX( GJAHR ) INTO W_ZFGFDYR_TO
      FROM BKPF
     WHERE BLART = 'RE'.
  ENDIF.

  IF W_ZFGFDNO_FROM IS INITIAL.
    SELECT MIN( BELNR ) INTO W_ZFGFDNO_FROM
      FROM BKPF
     WHERE GJAHR = W_ZFGFDYR_FROM
       AND BLART = 'RE'.
  ENDIF.
  IF W_ZFGFDNO_TO   IS INITIAL.
    SELECT MAX( BELNR ) INTO W_ZFGFDNO_TO
      FROM BKPF
     WHERE GJAHR = W_ZFGFDYR_TO
       AND BLART = 'RE'.
  ENDIF.

  IF OK-CODE ='EXEC'.
    PERFORM READ_BKPF_ZTVTIV_SCR8300.
  ENDIF.

ENDMODULE.                             " USER_COMMAND_SCR8300  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR8410  INPUT
*&---------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR8410 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_8410-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " IT_GET_LINE_SCR8410  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8410  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8410 INPUT.

  CASE OK-CODE.
    WHEN 'OTDC'.
      CALL TRANSACTION 'ZIMA2'.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN 'BKPF'.
      SET PARAMETER ID 'BLN' FIELD ZTVTIV-ZFGFDNO.
      SET PARAMETER ID 'BUK' FIELD ZTVTIV-BUKRS.
      SET PARAMETER ID 'GJR' FIELD ZTVTIV-ZFGFDYR.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR8410  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8400  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8400 INPUT.

  MOVE C_REQ_D TO W_STATUS.
  SET SCREEN 8410.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR8400  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8500  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8500 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  MOVE C_REQ_U TO W_STATUS.

  SET SCREEN 8510.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR8500  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8510  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8510 INPUT.

  CASE OK-CODE.
    WHEN 'CO'.                         " 공급자/공급받는자/수탁?
      PERFORM  P2000_SCR8510_CO_PROCESS.
    WHEN 'IT'.                         " 물품내?
      PERFORM  P2000_SCR8510_IT_PROCESS.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'OTDC' OR 'CHDC' OR 'DISP'.
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'DELE'.
      PERFORM  P2000_SET_INDICATE.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN 'DISTR'.
      PERFORM  P2000_SCR8510_VT_PRINT.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR8510  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR8510_IT_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SCR8510_IT_PROCESS.

  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '물품내역' INTO SPOP-TITEL
                                    SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  CALL SCREEN 8514 STARTING AT 15 1
                   ENDING   AT 95 25.

ENDFORM.                               " P2000_SCR8510_IT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR8510_CO_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SCR8510_CO_PROCESS.

  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '공급자/공급받는자/수탁자' INTO SPOP-TITEL
                                                 SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  CALL SCREEN 8512 STARTING AT 13 1
                   ENDING   AT 90 24.

ENDFORM.                               " P2000_SCR8510_CO_PROCESS
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR8512  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR8512 INPUT.

  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CLS'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " GET_OK_CODE_SCR8512  INPUT
*&---------------------------------------------------------------------*
*&      Module  REQIT_GET_LINE_SCR8514  INPUT
*&---------------------------------------------------------------------*
MODULE REQIT_GET_LINE_SCR8514 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_8514-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " REQIT_GET_LINE_SCR8514  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR8514  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR8514 INPUT.

  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CLS'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " GET_OK_CODE_SCR8514  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8600  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8600 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  MOVE C_REQ_D TO W_STATUS.

  SET SCREEN 8510.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR8600  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8700  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8700 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  CHECK NOT ZTRED-ZFREDNO IS INITIAL.

  MOVE C_REQ_U TO W_STATUS.

  SET SCREEN 8710.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR8700  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR8710  INPUT
*&---------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR8710 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_8710-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " IT_GET_LINE_SCR8710  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8710  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8710 INPUT.

  CASE OK-CODE.
    WHEN 'LC'.                         " L/C 사?
      PERFORM  P2000_SCR8710_LC_PROCESS.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'OTDC' OR 'CHDC' OR 'DISP'.
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'DELE'.
      PERFORM  P2000_SET_INDICATE.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN 'DISCR'.
      PERFORM  P2000_SCR8710_RED_PRINT.
    WHEN 'DIPO' OR 'DILI' OR 'DIVT'.
      PERFORM  P2000_SCR8510_DISPLAY.
    WHEN 'DIMA'.
      PERFORM  P2000_SCR8510_DISPLAY.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR8710  INPUT

*&---------------------------------------------------------------------*
*&      Form  P2000_SCR8710_LC_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SCR8710_LC_PROCESS.

  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' 'L/C 사항' INTO SPOP-TITEL
                                    SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  CALL SCREEN 8712 STARTING AT 15  5
                   ENDING   AT 95  23.

ENDFORM.                               " P2000_SCR8710_LC_PROCESS
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR8712  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR8712 INPUT.

  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CLS'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " GET_OK_CODE_SCR8712  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8800  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8800 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  CHECK NOT ZTRED-ZFREDNO IS INITIAL.

  MOVE C_REQ_D TO W_STATUS.

  SET SCREEN 8710.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR8800  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR7300  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR7300 INPUT.

  MOVE C_REQ_D TO W_STATUS.

  SET SCREEN 7110.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR7300  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_7110_UPDATE_SCR7110  INPUT
*&---------------------------------------------------------------------*
MODULE TC_7110_UPDATE_SCR7110 INPUT.
  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  READ TABLE IT_ZSBLUGC INDEX TC_7110-CURRENT_LINE.

  MOVE-CORRESPONDING ZSBLUGC TO IT_ZSBLUGC.
  IF OK-CODE = 'MKA1'.
    MOVE   'X'             TO W_ROW_MARK.
  ENDIF.
  IF OK-CODE = 'MKL1'.
    CLEAR  W_ROW_MARK.
  ENDIF.
  MOVE W_ROW_MARK TO IT_ZSBLUGC-ZFMARK.

  IF ZSBLUGC-ZFCARNO IS INITIAL.
    MESSAGE S704.
    EXIT.
  ENDIF.

  IF SY-SUBRC = 0.
    MODIFY IT_ZSBLUGC   INDEX TC_7110-CURRENT_LINE.
  ELSE.
    APPEND IT_ZSBLUGC.
  ENDIF.

ENDMODULE.                             " TC_7110_UPDATE_SCR7110  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_ZTBLUG_SCR7200_SCR7300  INPUT
*&---------------------------------------------------------------------*
MODULE READ_ZTBLUG_SCR7200_SCR7300 INPUT.

  PERFORM OK_CODE_BTRE.
  PERFORM OK_CODE_BACK_EXIT.

  CLEAR : W_LIFNR_NM, W_ZFFORD_NM, W_ZFTRCK_NM, W_ZFBNARCD_NM.

  IF ZTBLUG-ZFHBLNO IS INITIAL.
    MESSAGE E702.
  ENDIF.

* 긴급보세운송 의뢰 조?
  SELECT SINGLE *
    FROM ZTBLUG
   WHERE ZFHBLNO EQ ZTBLUG-ZFHBLNO.
  IF SY-SUBRC NE 0.
    MESSAGE E703 WITH ZTBLUG-ZFHBLNO.
  ENDIF.
  SELECT SINGLE *
    FROM EKKO
   WHERE EBELN EQ ZTBLUG-ZFREBELN.
  MOVE EKKO-LIFNR TO ZTREQHD-LIFNR.
  SELECT SINGLE NAME1 INTO W_LIFNR_NM
    FROM LFA1
   WHERE LIFNR = ZTREQHD-LIFNR.
  SELECT SINGLE NAME1 INTO W_ZFFORD_NM
    FROM LFA1
   WHERE LIFNR = ZTBLUG-ZFFORD.
  SELECT SINGLE NAME1 INTO W_ZFTRCK_NM
    FROM LFA1
   WHERE LIFNR = ZTBLUG-ZFTRCK.
  SELECT MAX( ZFBNARM ) INTO W_ZFBNARCD_NM
    FROM ZTIMIMG03
   WHERE ZFBNARCD = ZTBLUG-ZFBNARCD.
* 긴급보세운송 의뢰 차량정보 조?
  REFRESH IT_ZSBLUGC.
  SELECT *
    FROM ZTBLUGC
   WHERE ZFHBLNO EQ ZTBLUG-ZFHBLNO.
    MOVE-CORRESPONDING ZTBLUGC TO IT_ZSBLUGC.
    CLEAR IT_ZSBLUGC-ZFMARK.
    APPEND IT_ZSBLUGC.
  ENDSELECT.

ENDMODULE.                 " READ_ZTBLUG_SCR7200_SCR7300  INPUT
*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR7110  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR7110 INPUT.

  PERFORM OK_CODE_BTRE.

  SELECT SINGLE *
    FROM EKKO
   WHERE EBELN EQ ZTBLUG-ZFREBELN.
  MOVE EKKO-LIFNR TO ZTREQHD-LIFNR.
  SELECT SINGLE NAME1 INTO W_LIFNR_NM
    FROM LFA1
         WHERE LIFNR = ZTREQHD-LIFNR.
  SELECT SINGLE NAME1 INTO W_ZFFORD_NM
    FROM LFA1
   WHERE LIFNR = ZTBLUG-ZFFORD.
  SELECT SINGLE NAME1 INTO W_ZFTRCK_NM
    FROM LFA1
   WHERE LIFNR = ZTBLUG-ZFTRCK.

ENDMODULE.                             " AFI_PROCESS_SCR7110  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_ZTBLUG_SCR7100  INPUT
*&---------------------------------------------------------------------*
MODULE READ_ZTBLUG_SCR7100 INPUT.

  PERFORM OK_CODE_BTRE.
  PERFORM OK_CODE_BACK_EXIT.

  CLEAR : W_LIFNR_NM, W_ZFFORD_NM, W_ZFTRCK_NM, W_ZFBNARCD_NM.

  IF ZTBLUG-ZFHBLNO IS INITIAL.
    MESSAGE E702.
  ENDIF.

* B/L 조?
  CLEAR W_ZFBLNO.
  SELECT MAX( ZFBLNO ) INTO W_ZFBLNO
    FROM ZTBL
   WHERE ZFHBLNO EQ ZTBLUG-ZFHBLNO.
  IF NOT W_ZFBLNO IS INITIAL.
    SELECT SINGLE *
      FROM ZTBL
     WHERE ZFBLNO = W_ZFBLNO.
    MOVE-CORRESPONDING ZTBL TO ZTBLUG.
    MOVE ZTBL-ZFCARC TO ZTBLUG-ZFSHCU.
    MESSAGE W892.
    SELECT SINGLE *
      FROM EKKO
     WHERE EBELN EQ ZTBLUG-ZFREBELN.
    MOVE EKKO-LIFNR TO ZTREQHD-LIFNR.
    SELECT SINGLE NAME1 INTO W_LIFNR_NM
      FROM LFA1
     WHERE LIFNR = ZTREQHD-LIFNR.
    SELECT SINGLE NAME1 INTO W_ZFFORD_NM
      FROM LFA1
     WHERE LIFNR = ZTBLUG-ZFFORD.
    SELECT SINGLE NAME1 INTO W_ZFTRCK_NM
      FROM LFA1
     WHERE LIFNR = ZTBLUG-ZFTRCK.
    SELECT MAX( ZFBNARM ) INTO W_ZFBNARCD_NM
      FROM ZTIMIMG03
     WHERE ZFBNARCD = ZTBLUG-ZFBNARCD.
    CLEAR W_COUNT.
    SELECT COUNT( DISTINCT ZFHBLNO ) INTO W_COUNT
      FROM ZTBLUG
     WHERE ZFHBLNO EQ ZTBLUG-ZFHBLNO.
    IF W_COUNT = 0.
      EXIT.
    ENDIF.
    MOVE C_REQ_U TO W_STATUS.
    SET SCREEN 7110.  LEAVE SCREEN.
  ENDIF.

* 긴급보세운송 의뢰 조?
  SELECT SINGLE *
    FROM ZTBLUG
   WHERE ZFHBLNO EQ ZTBLUG-ZFHBLNO.
  IF SY-SUBRC NE 0.
    MOVE ZTBLUG-ZFHBLNO TO W_ZFHBLNO.
    CLEAR ZTBLUG.
    MOVE W_ZFHBLNO TO ZTBLUG-ZFHBLNO.
    EXIT.
  ENDIF.
  MESSAGE W705.
  SELECT SINGLE *
    FROM EKKO
   WHERE EBELN EQ ZTBLUG-ZFREBELN.
  MOVE EKKO-LIFNR TO ZTREQHD-LIFNR.
  SELECT SINGLE NAME1 INTO W_LIFNR_NM
    FROM LFA1
   WHERE LIFNR = ZTREQHD-LIFNR.
  SELECT SINGLE NAME1 INTO W_ZFFORD_NM
    FROM LFA1
   WHERE LIFNR = ZTBLUG-ZFFORD.
  SELECT SINGLE NAME1 INTO W_ZFTRCK_NM
    FROM LFA1
   WHERE LIFNR = ZTBLUG-ZFTRCK.
  SELECT MAX( ZFBNARM ) INTO W_ZFBNARCD_NM
    FROM ZTIMIMG03
   WHERE ZFBNARCD = ZTBLUG-ZFBNARCD.
* 긴급보세운송 의뢰 차량정보 조?
  REFRESH IT_ZSBLUGC.
  SELECT *
    FROM ZTBLUGC
   WHERE ZFHBLNO EQ ZTBLUG-ZFHBLNO.
    MOVE-CORRESPONDING ZTBLUGC TO IT_ZSBLUGC.
    CLEAR IT_ZSBLUGC-ZFMARK.
    APPEND IT_ZSBLUGC.
  ENDSELECT.
  MOVE C_REQ_U TO W_STATUS.
  SET SCREEN 7110.  LEAVE SCREEN.

ENDMODULE.                             " READ_ZTBLUG_SCR7100  INPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_BTRE  INPUT
*&---------------------------------------------------------------------*
MODULE OK_CODE_BTRE INPUT.

  IF OK-CODE EQ 'BACK' OR OK-CODE EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.
  IF OK-CODE EQ 'CRDC'.
    LEAVE TO TRANSACTION 'ZIM71'.
  ENDIF.
  IF OK-CODE EQ 'CHDC'.
    LEAVE TO TRANSACTION 'ZIM72'.
  ENDIF.
  IF OK-CODE EQ 'DISP'.
    LEAVE TO TRANSACTION 'ZIM73'.
  ENDIF.

ENDMODULE.                             " OK_CODE_BTRE  INPUT
*&---------------------------------------------------------------------*
*&      Form  OK_CODE_BTRE
*&---------------------------------------------------------------------*
FORM OK_CODE_BTRE.

  IF OK-CODE EQ 'CRDC'.
    LEAVE TO TRANSACTION 'ZIM71'.
  ENDIF.
  IF OK-CODE EQ 'CHDC'.
    LEAVE TO TRANSACTION 'ZIM72'.
  ENDIF.
  IF OK-CODE EQ 'DISP'.
    LEAVE TO TRANSACTION 'ZIM73'.
  ENDIF.
  IF OK-CODE EQ 'DELE'.
    PERFORM P3000_ZTBLUG_DELETE_SCR7110.
  ENDIF.

ENDFORM.                               " OK_CODE_BTRE
*&---------------------------------------------------------------------*
*&      Form  P2000_REF1_PROCESS_SCR7110
*&---------------------------------------------------------------------*
FORM P2000_REF1_PROCESS_SCR7110.

* 긴급보세운송 의뢰 차량정보 조?
  REFRESH IT_ZSBLUGC.
  SELECT * FROM ZTBLUGC WHERE ZFHBLNO EQ ZTBLUG-ZFHBLNO.
    MOVE-CORRESPONDING ZTBLUGC TO IT_ZSBLUGC.
    CLEAR IT_ZSBLUGC-ZFMARK.
    APPEND IT_ZSBLUGC.
  ENDSELECT.

ENDFORM.                               " P2000_REF1_PROCESS_SCR7110
*&---------------------------------------------------------------------*
*&      Form  OK_CODE_COIV
*&---------------------------------------------------------------------*
FORM OK_CODE_COIV.

  IF OK-CODE EQ 'REF1'.
    PERFORM  P2000_REF1_PROCESS_SCR3110.
  ENDIF.
  IF OK-CODE EQ 'CRDC'.
    LEAVE TO TRANSACTION 'ZIM31'.
  ENDIF.
  IF OK-CODE EQ 'CHDC'.
    LEAVE TO TRANSACTION 'ZIM32'.
  ENDIF.
  IF OK-CODE EQ 'DISP'.
    LEAVE TO TRANSACTION 'ZIM33'.
  ENDIF.
  IF OK-CODE EQ 'DELE'.
    PERFORM P3000_ZTIV_DELETE_SCR3110.
  ENDIF.
  IF OK-CODE EQ 'DSRQ'.
*    SET PARAMETER ID 'ZPREQNO' FIELD ZTIV-ZFREQNO.
*    SET PARAMETER ID 'BES'     FIELD ''.
*    SET PARAMETER ID 'ZPOPNNO' FIELD ''.
*    EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
*    CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
  ENDIF.
  IF OK-CODE EQ 'DSBL'.
    SET PARAMETER ID 'ZPBLNO'  FIELD ZTIV-ZFBLNO.
    SET PARAMETER ID 'ZPHBLNO' FIELD ''.
    EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
    CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
  ENDIF.
  IF OK-CODE EQ 'MK03'.
    PERFORM P2000_VENDOR_DISPLAY USING ZTBL-LIFNR.
  ENDIF.
  IF OK-CODE EQ 'BENE'.
    PERFORM P2000_VENDOR_DISPLAY USING ZTIV-LIFNR.
  ENDIF.
  IF OK-CODE EQ 'FWDR'.
    PERFORM P2000_VENDOR_DISPLAY USING ZTBL-ZFFORD.
  ENDIF.
  IF OK-CODE EQ 'ZIM58'.
    SET PARAMETER ID 'ZPIVNO'  FIELD ZTIV-ZFIVNO.
    SET PARAMETER ID 'ZPCIVNO' FIELD ''.
*    EXPORT 'ZPIVNO'       TO MEMORY ID 'ZPIVQNO'.
*    EXPORT 'ZPCIVNO'      TO MEMORY ID 'ZPIVQNO'.
    CALL TRANSACTION 'ZIM34' AND SKIP  FIRST SCREEN.
  ENDIF.

ENDFORM.                               " OK_CODE_COIV
*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR3110  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR3110 INPUT.

  PERFORM OK_CODE_COIV.

ENDMODULE.                             " AFI_PROCESS_SCR3110  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_3110_UPDATE_SCR3110  INPUT
*&---------------------------------------------------------------------*
MODULE TC_3110_UPDATE_SCR3110 INPUT.

  CHECK W_STATUS NE C_REQ_D.
  CHECK W_STATUS NE C_OPEN_U.

  CHECK NOT ZSIVIT-ZFBLNO IS INITIAL.
  READ TABLE IT_ZSIVIT    WITH KEY ZFBLNO  = ZSIVIT-ZFBLNO
                                   ZFBLIT  = ZSIVIT-ZFBLIT.
  W_OLD_SUBRC  = SY-SUBRC.
  W_TABIX      = SY-TABIX.
  IF W_SY_SUBRC NE 0.  W_SY_SUBRC = 0. EXIT. ENDIF.
  CHECK SY-SUBRC   EQ 0.
*-----------------------------------------------------------------------
  MOVE : ZSIVIT-CCMENGE  TO  ZSIVIT-GRMENGE.

  IF ZTIV-ZFGRST EQ 'N'.
    IF ZSIVIT-UMSON EQ 'X'.
      IF NOT ZSIVIT-MATNR IS INITIAL AND NOT ZSIVIT-EBELN IS INITIAL AND
         ZSIVIT-WEPOS IS INITIAL.
        PERFORM P2000_NO_INPUT  USING  'ZSIVIT' 'UMSON'
                                        DFIES-SCRTEXT_M L_SUBRC.
        MESSAGE E553 WITH ZSIVIT-EBELN ZSIVIT-EBELP.
      ENDIF.
    ENDIF.
    IF ZSIVIT-ZFPOTY IS INITIAL AND
       ZSIVIT-UMSON IS INITIAL  AND NOT ZSIVIT-GRMENGE IS INITIAL.
* Begin of changes -
       if   ZSIVIT-ZFNOCCMn eq 0 .
          ZSIVIT-CCMENGE  = ZSIVIT-ZFNOCCMn.
          ZSIVIT-GRMENGE  = ZSIVIT-ZFNOCCMn.
        else.
* End of changes -
      PERFORM P2000_NO_INPUT  USING  'ZSIVIT' 'GRMENGE'
                                      DFIES-SCRTEXT_M L_SUBRC.
      MESSAGE E550.
      endif.
    ENDIF.
    ">> G/R Quantity
    IF ZSIVIT-ZFPOTY  IS INITIAL AND
       ZSIVIT-GRMENGE IS INITIAL AND ZSIVIT-UMSON EQ 'X'.
      PERFORM P2000_NO_INPUT  USING  'ZSIVIT' 'GRMENGE'
                                      DFIES-SCRTEXT_M L_SUBRC.
      MESSAGE E551.
    ENDIF.
  ELSEIF ZTIV-ZFGRST EQ 'X'.
    IF ZSIVIT-UMSON EQ 'X'.
      PERFORM P2000_NO_INPUT  USING  'ZSIVIT' 'UMSON'
                                     DFIES-SCRTEXT_M L_SUBRC.
      MESSAGE E554.
      IF NOT ZSIVIT-ZFPOTY IS INITIAL.
        PERFORM P2000_NO_INPUT  USING  'ZSIVIT' 'UMSON'
                                        DFIES-SCRTEXT_M L_SUBRC.
        MESSAGE E697.
      ENDIF.
    ENDIF.
    IF NOT ZSIVIT-GRMENGE IS INITIAL.
      PERFORM P2000_NO_INPUT  USING  'ZSIVIT' 'GRMENGE'
                                     DFIES-SCRTEXT_M L_SUBRC.
      MESSAGE W555.
      ZSIVIT-GRMENGE = 0.
    ENDIF.
  ENDIF.

  " Summary
  MOVE-CORRESPONDING  ZSIVIT  TO   IT_IVIT_SUM.
  COLLECT IT_IVIT_SUM.

  CLEAR : IT_IVIT_SUM.
  READ TABLE IT_IVIT_SUM WITH KEY ZFREQNO = ZSIVIT-ZFREQNO
                                  ZFITMNO = ZSIVIT-ZFITMNO
                                  ZFBLNO  = ZSIVIT-ZFBLNO
                                  ZFBLIT  = ZSIVIT-ZFBLIT
                                  ZFCGNO  = ZSIVIT-ZFCGNO
                                  ZFCGIT  = ZSIVIT-ZFCGIT.
  W_SUBRC = SY-SUBRC.

  CLEAR : ZTIVIT.
  SELECT SINGLE * FROM  ZTIVIT
           WHERE ZFIVNO   EQ  ZTIV-ZFIVNO
           AND   ZFIVDNO  EQ  ZSIVIT-ZFIVDNO.

  ">> G/R Quantity Check
  IF NOT ZSIVIT-GRMENGE IS INITIAL.
    CLEAR : W_MENGE1.
    IF ZSIVIT-ZFCGNO IS INITIAL.
      IF ZSIVIT-ZFBLNO IS INITIAL.
        IF NOT ZSIVIT-ZFREQNO IS INITIAL.
          W_MENGE = ZSIVIT-MENGE   - ZSIVIT-GRMENGE -
                  ( ZSIVIT-ZFGRTOT - ZTIVIT-GRMENGE ).
          IF W_MENGE LT 0.
            PERFORM P2000_NO_INPUT USING 'ZSIVIT' 'GRMENGE'
                                   DFIES-SCRTEXT_M W_SUBRC.
            W_MENGE = ZSIVIT-ZFGRTOT - ZTIVIT-GRMENGE
                                     + ZSIVIT-CCMENGE.
            WRITE :  W_MENGE TO W_AMTTXT1 UNIT ZSIVIT-MEINS .
            W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
            WRITE : ZSIVIT-MENGE TO W_AMTTXT2 UNIT ZSIVIT-MEINS .
            W_AMTLEN2 = STRLEN( W_AMTTXT2 ).
            MESSAGE E438 WITH ZSIVIT-ZFIVDNO 'G/R'
                              W_AMTTXT1(W_AMTLEN1) W_AMTTXT2(W_AMTLEN2).
            EXIT.
          ENDIF.
        ENDIF.
      ELSE.
        W_MENGE = ZSIVIT-MENGE_BL - ZSIVIT-GRMENGE -
                  ( ZSIVIT-ZFGRTOT - ZTIVIT-GRMENGE ).
        IF W_MENGE LT 0 and ZSIVIT-ZFNOCCMN > 0.
* Begin of changes - UD1K920770
          ZSIVIT-CCMENGE = ZSIVIT-ZFNOCCMN. "UD1K920770
          ZSIVIT-GRMENGE = ZSIVIT-ZFNOCCMN. "UD1K920770
          if   ZSIVIT-CCMENGE <= 0.         "UD1K920770
          PERFORM P2000_NO_INPUT USING 'ZSIVIT' 'GRMENGE'
                                 DFIES-SCRTEXT_M W_SUBRC.
          W_MENGE = ZSIVIT-ZFGRTOT - ZTIVIT-GRMENGE
                                   + ZSIVIT-GRMENGE.
          WRITE :  W_MENGE TO W_AMTTXT1 UNIT ZSIVIT-MEINS .
          W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
          WRITE : ZSIVIT-MENGE_BL TO W_AMTTXT2 UNIT ZSIVIT-MEINS .
          W_AMTLEN2 = STRLEN( W_AMTTXT2 ).
          MESSAGE E439 WITH ZSIVIT-ZFIVDNO 'G/R'
                            W_AMTTXT1(W_AMTLEN1) W_AMTTXT2(W_AMTLEN2).
          EXIT.
          endif.   "UD1K920770
* End  of changes - UD1K920770
        ENDIF.
      ENDIF.
    ELSE.
      W_MENGE = ZSIVIT-CGMENGE - ZSIVIT-ZFGRTOT -
                ( ZSIVIT-GRMENGE - ZTIVIT-GRMENGE ).
      IF W_MENGE LT 0.
        PERFORM P2000_NO_INPUT USING 'ZSIVIT' 'GRMENGE'
                               DFIES-SCRTEXT_M W_SUBRC.
        W_MENGE = ZSIVIT-ZFGRTOT - ZTIVIT-GRMENGE
                                 + ZSIVIT-GRMENGE.
        WRITE :  W_MENGE TO W_AMTTXT1 UNIT ZSIVIT-MEINS .
        W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
        WRITE : ZSIVIT-CGMENGE TO W_AMTTXT2 UNIT ZSIVIT-MEINS .
        W_AMTLEN2 = STRLEN( W_AMTTXT2 ).

        MESSAGE E440 WITH ZSIVIT-ZFIVDNO 'G/R'
                          W_AMTTXT1(W_AMTLEN1) W_AMTTXT2(W_AMTLEN2).
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  ">> Customs Clearance Quantity Check
  IF NOT ZSIVIT-CCMENGE IS INITIAL.
    CLEAR : W_MENGE1.
    IF ZSIVIT-ZFBLNO IS INITIAL.
      IF NOT ZSIVIT-ZFREQNO IS INITIAL.
        W_MENGE = ZSIVIT-MENGE   - ZSIVIT-CCMENGE -
                ( ZSIVIT-ZFCCTOT - ZTIVIT-CCMENGE ).
        IF W_MENGE LT 0.
          PERFORM P2000_NO_INPUT USING 'ZSIVIT' 'CCMENGE'
                                 DFIES-SCRTEXT_M W_SUBRC.
          W_MENGE = ZSIVIT-ZFCCTOT - ZTIVIT-CCMENGE
                                   + ZSIVIT-CCMENGE.
          WRITE :  W_MENGE TO W_AMTTXT1 UNIT ZSIVIT-MEINS .
          W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
          WRITE :  W_MENGE TO W_AMTTXT2 UNIT ZSIVIT-MEINS .
          W_AMTLEN2 = STRLEN( W_AMTTXT2 ).
          MESSAGE E438 WITH ZSIVIT-ZFIVDNO 'Clearance'
                            W_AMTTXT1(W_AMTLEN1) W_AMTTXT1(W_AMTLEN2).
          EXIT.
        ENDIF.
      ENDIF.
    ELSE.
      W_MENGE = ZSIVIT-MENGE_BL - ZSIVIT-CCMENGE -
                ( ZSIVIT-ZFCCTOT - ZTIVIT-CCMENGE ).
      IF W_MENGE LT 0.
        PERFORM P2000_NO_INPUT USING 'ZSIVIT' 'CCMENGE'
                               DFIES-SCRTEXT_M W_SUBRC.
        W_MENGE = ZSIVIT-ZFCCTOT - ZTIVIT-CCMENGE
                                 + ZSIVIT-CCMENGE.
        WRITE :  W_MENGE TO W_AMTTXT1 UNIT ZSIVIT-MEINS .
        W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
        WRITE : ZSIVIT-MENGE_BL TO W_AMTTXT2 UNIT ZSIVIT-MEINS .
        W_AMTLEN2 = STRLEN( W_AMTTXT2 ).
        MESSAGE E439 WITH ZSIVIT-ZFIVDNO 'Clearance'
                          W_AMTTXT1(W_AMTLEN1)  W_AMTTXT2(W_AMTLEN2).
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  ">> Clarance Check.
  IF NOT ZSIVIT-CCMENGE IS INITIAL.
    IF ZSIVIT-ZFIVAMT  IS INITIAL.
      IF NOT ZSIVIT-NETPR IS INITIAL.
        ZSIVIT-ZFIVAMT = ( IT_ZSIVIT-NETPR / IT_ZSIVIT-PEINH ) *
                         ( IT_ZSIVIT-BPUMZ / IT_ZSIVIT-BPUMN ) *
                          ZSIVIT-CCMENGE.
      ENDIF.
    ENDIF.
    WRITE :  ZSIVIT-GRMENGE TO W_AMTTXT1 UNIT ZSIVIT-MEINS.
    W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
    WRITE :  ZSIVIT-CCMENGE TO W_AMTTXT2 UNIT ZSIVIT-MEINS.
    W_AMTLEN2 = STRLEN( W_AMTTXT2 ).

    IF ZSIVIT-GRMENGE GT 0.
      IF ZSIVIT-GRMENGE GT ZSIVIT-CCMENGE.
        PERFORM P2000_NO_INPUT     USING 'ZSIVIT' 'WERKS'
                                         DFIES-SCRTEXT_M W_SUBRC.
        MESSAGE E559 WITH W_AMTTXT1(W_AMTLEN1) W_AMTTXT2(W_AMTLEN2).
        EXIT.
      ENDIF.
    ENDIF.

    IF ZSIVIT-WERKS IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIVIT' 'WERKS'.
    ENDIF.

    IF ZTIMIMG00-GRPARTX NE 'X'.
      IF ZSIVIT-UMSON EQ 'X'.
         IF ZSIVIT-LGORT IS INITIAL.
            PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIVIT' 'LGORT'.
            EXIT.
         ENDIF.
      ENDIF.
    ENDIF.

    IF ZSIVIT-ZFMATGB IS INITIAL  AND
       ( ZTIMIMG00-ZFCSTMD NE 'S' AND ZTIMIMG00-ZFCSTMD NE 'P' ).
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIVIT' 'ZFMATGB'.
      EXIT.
    ENDIF.
    IF ZSIVIT-ZFIVAMK IS INITIAL.
      IF ZSIVIT-ZFIVAMC NE 'USD'.
        IF ZTIV-FFACT IS INITIAL.
           ZTIV-FFACT = 1.
        ENDIF.
          *BAPICURR-BAPICURR = ( ZTIV-ZFEXRT / ZTIV-FFACT )
                               * ZSIVIT-ZFIVAMT.

        SELECT SINGLE * FROM  TCURX
               WHERE  CURRKEY     = ZSIVIT-ZFIVAMC.
        IF SY-SUBRC NE 0.
          TCURX-CURRDEC = 2.
        ENDIF.
        IF TCURX-CURRDEC NE 0.
          PERFORM SET_CURR_CONV_TO_INTERNAL USING
                  *BAPICURR-BAPICURR   ZSIVIT-ZFKRW.
        ENDIF.
        IF *BAPICURR-BAPICURR GT 9999999999999.
          MESSAGE W923 WITH *BAPICURR-BAPICURR.
          ZSIVIT-ZFIVAMK = 0.
        ELSE.
          ZSIVIT-ZFIVAMK = *BAPICURR-BAPICURR.
        ENDIF.
      ELSE.
        ZSIVIT-ZFIVAMK = ZSIVIT-ZFIVAMT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ZSIVIT-CCMENGE NE ZSIVIT-GRMENGE.
    MESSAGE W670.
  ENDIF.

  MOVE-CORRESPONDING ZSIVIT TO IT_ZSIVIT.
  MOVE W_ROW_MARK           TO IT_ZSIVIT-ZFMARK.

  IF W_OLD_SUBRC EQ 0.
    MODIFY IT_ZSIVIT   INDEX TC_3111-CURRENT_LINE.
  ELSE.
    APPEND IT_ZSIVIT.
  ENDIF.

ENDMODULE.                             " TC_3110_UPDATE_SCR3110  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_REF1_PROCESS_SCR3110
*&---------------------------------------------------------------------*
FORM P2000_REF1_PROCESS_SCR3110.

  REFRESH IT_ZSIVIT.
  CLEAR   IT_ZSIVIT.
  IF W_STATUS EQ 'C' OR W_STATUS EQ 'U'.
    IF RADIO_NPO = 'X'.
      PERFORM READ_MSEG_TO_IT.         " Material Document item ->
    ELSE.
*      IF NOT ( ZTIV-ZFREQNO IS INITIAL ).
*        PERFORM READ_LCIT_TO_IT.       " L/C Item -> Int
*      ENDIF.
    ENDIF.
  ELSE.
    PERFORM READ_IVIT_TO_IT.
  ENDIF.

ENDFORM.                               " P2000_REF1_PROCESS_SCR3110
*&---------------------------------------------------------------------*
*&      Form  READ_LCIT_TO_IT
*&---------------------------------------------------------------------*
FORM READ_LCIT_TO_IT.

  CLEAR   W_ZFIVAMT_S.
  SELECT *
    FROM ZTREQIT.
*   WHERE ZFREQNO EQ ZTIV-ZFREQNO.
    CLEAR IT_ZSIVIT.
    MOVE-CORRESPONDING ZTREQIT TO IT_ZSIVIT.
    MOVE 0               TO IT_ZSIVIT-CCMENGE.
    MOVE 0               TO IT_ZSIVIT-KWERT.
    MOVE ZTREQIT-ZFITMNO TO IT_ZSIVIT-ZFIVDNO.
    MOVE ZTREQIT-MENGE   TO IT_ZSIVIT-MENGE1.
*>>KSB MODIFY 2001/02/22
*    SELECT SUM( MENGE )  INTO W_TEMP_MENGE
*      FROM ZVIVHD_IT
*     WHERE ZFREQNO = ZTREQIT-ZFREQNO
*       AND ZFIVDNO = ZTREQIT-ZFITMNO.
*    IF ZTIV-ZFPRPYN = 'N'.
*      SELECT SUM( MENGE )  INTO W_TEMP_MENGE
*        FROM ZVIVHD_IT
*       WHERE ZFREQNO = ZTREQIT-ZFREQNO
*         AND ZFIVDNO = ZTREQIT-ZFITMNO
*         AND ZFPRPYN = 'N'.
*    ENDIF.
    IF IT_ZSIVIT-MENGE1 > W_TEMP_MENGE.
      IT_ZSIVIT-MENGE2 = IT_ZSIVIT-MENGE1 - W_TEMP_MENGE.
    ENDIF.
    MOVE ZTIV-ZFIVAMC TO IT_ZSIVIT-ZFIVAMC.
*    IF ZTIV-ZFPRPYN = 'Y'.
*      MOVE ZTREQIT-MENGE TO IT_ZSIVIT-MENGE.
*      IT_ZSIVIT-ZFIVAMT = IT_ZSIVIT-MENGE * IT_ZSIVIT-NETPR
*                        / IT_ZSIVIT-PEINH.
*      ADD IT_ZSIVIT-ZFIVAMT TO W_ZFIVAMT_S.
*    ENDIF.
    APPEND IT_ZSIVIT.
  ENDSELECT.
  CLEAR ZTIV-ZFIVAMT.
*  IF ZTIV-ZFPRPYN = 'Y'.
*    ZTIV-ZFIVAMT = W_ZFIVAMT_S + ZTIV-ZFPKCHG + ZTIV-ZFHDCHG.
*  ENDIF.

ENDFORM.                               " READ_LCIT_TO_IT
*&---------------------------------------------------------------------*
*&      Form  READ_IVIT_TO_IT
*&---------------------------------------------------------------------*
FORM READ_IVIT_TO_IT.

  CLEAR W_ZFIVAMT_S.
  REFRESH IT_ZSIVIT.
*  SELECT * FROM ZTIVIT
*   WHERE ZFIVNO EQ ZTIV-ZFIVNO .
*    CLEAR   IT_ZSIVIT.
*    MOVE-CORRESPONDING ZTIVIT TO IT_ZSIVIT.
*    IF NOT ( ZTIV-ZFREQNO IS INITIAL ).
*      SELECT SINGLE *
*        FROM ZTREQIT
*       WHERE ZFREQNO EQ ZTIV-ZFREQNO
*         AND ZFITMNO EQ ZTIVIT-ZFIVDNO.
*      MOVE ZTREQIT-MENGE   TO IT_ZSIVIT-MENGE1.
**      SELECT SUM( MENGE ) INTO W_TEMP_MENGE
**        FROM ZVIVHD_IT
**       WHERE ZFREQNO = ZTREQIT-ZFREQNO
**         AND ZFIVDNO = ZTREQIT-ZFITMNO.
*      IF IT_ZSIVIT-MENGE1 > W_TEMP_MENGE.
*        IT_ZSIVIT-MENGE2 = IT_ZSIVIT-MENGE1 - W_TEMP_MENGE.
*      ENDIF.
*    ENDIF.
*    IT_ZSIVIT-ZFIMOC = IT_ZSIVIT-ZFDAMT +
*                       IT_ZSIVIT-ZFUPCST + IT_ZSIVIT-ZFPCST.
*    ADD IT_ZSIVIT-ZFIVAMT TO W_ZFIVAMT_S.
*    APPEND IT_ZSIVIT.
*  ENDSELECT.

  SELECT MAX( ZFIVDNO ) INTO W_ZFIVDNO_M
    FROM ZTIVIT
   WHERE ZFIVNO EQ ZTIV-ZFIVNO.

ENDFORM.                               " READ_IVIT_TO_IT
*&---------------------------------------------------------------------*
*&      Form  DIST_PROCESS_SCR3110
*&---------------------------------------------------------------------*
FORM DIST_PROCESS_SCR3110.

* Invoice 금액 = Item 금액의 합계 -> No Logic
* Invoice 금액 = 0 -> Move item금액 합계 to Invoice 금?
* Invoice 금액 <> Item 금액의 합계 -> Invoice 금액을 Item 금액에 배?
* IF ZTIV-ZFIVAMT IS INITIAL.
*    MOVE W_ZFIVAMT_S TO ZTIV-ZFIVAMT.
* endif.
  IF ZTIV-ZFIVAMT EQ W_ZFIVAMT_S.
    EXIT.
  ENDIF.

  PERFORM P2000_MESSAGE_BOX USING
               '배부 확인'             " 타이틀...
               'I/T 금액 합과 I/V 금액의 차이가 발생.'
               'Item 으로 배부하시겠습니까?'                " Message #2
               'N'                     " 취소 버튼 유/?
               '1'.                    " default button

  IF ANTWORT NE 'Y'.
    MOVE W_ZFIVAMT_S TO ZTIV-ZFIVAMT.
    EXIT.
  ENDIF.

  W_ZFIVAMT_D = ZTIV-ZFIVAMT - W_ZFIVAMT_S.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
       EXPORTING
            CURRENCY             = ZTIV-ZFIVAMC
            AMOUNT_EXTERNAL      = W_ZFIVAMT_D
            MAX_NUMBER_OF_DIGITS = DIGITS
       IMPORTING
            AMOUNT_INTERNAL      = W_ZFIVAMT_D
       EXCEPTIONS
            OTHERS               = 1.

  WRITE  W_ZFIVAMT_D TO W_TEXT18 CURRENCY ZTIV-ZFIVAMC.
  MESSAGE S724 WITH W_TEXT18.

  CLEAR W_ZFIVAMT_T.
  LOOP AT IT_ZSIVIT.
    READ TABLE IT_ZSIVIT  WITH KEY IT_ZSIVIT(18)  BINARY SEARCH.
    IT_ZSIVIT-ZFIVAMT = IT_ZSIVIT-ZFIVAMT *
                        ZTIV-ZFIVAMT / W_ZFIVAMT_S.
    ADD IT_ZSIVIT-ZFIVAMT TO W_ZFIVAMT_T.
    MODIFY IT_ZSIVIT   INDEX SY-TABIX.
  ENDLOOP.

  IF ZTIV-ZFIVAMT NE W_ZFIVAMT_T.      " 단수차이조?
    IT_ZSIVIT-ZFIVAMT = IT_ZSIVIT-ZFIVAMT + ZTIV-ZFIVAMT - W_ZFIVAMT_S.
    MODIFY IT_ZSIVIT   INDEX SY-TABIX.
  ENDIF.

ENDFORM.                               " DIST_PROCESS_SCR3110
*&---------------------------------------------------------------------*
*&      Form  QTY_ITAMT_IVAMT_SCR3110
*&---------------------------------------------------------------------*
FORM QTY_ITAMT_IVAMT_SCR3110.

  IF IT_ZSIVIT-ZFIVAMT = 0.
    IT_ZSIVIT-ZFIVAMT = IT_ZSIVIT-CCMENGE *
                        IT_ZSIVIT-NETPR / IT_ZSIVIT-PEINH.
  ELSE.
    IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'N'. "무환 With No Ref.
      IT_ZSIVIT-ZFIVAMT = IT_ZSIVIT-CCMENGE *
                          IT_ZSIVIT-NETPR / IT_ZSIVIT-PEINH.
    ENDIF.
  ENDIF.
  ADD IT_ZSIVIT-ZFIVAMT TO W_ZFIVAMT_S.

  IF IT_ZSIVIT-KPEIN > 0.              " Install Charge
    IT_ZSIVIT-KWERT = IT_ZSIVIT-CCMENGE *
                     IT_ZSIVIT-KBETR / IT_ZSIVIT-KPEIN.
  ENDIF.

*  IT_ZSIVIT-ZFPRQN =
*     IT_ZSIVIT-CCMENGE *  ( 100 - ZTREQHD-ZFPREPAY ) / 100.
*  IT_ZSIVIT-ZFIVAMP =
*     IT_ZSIVIT-ZFIVAMT *  ( 100 - ZTREQHD-ZFPREPAY ) / 100.
*  IF ZTIV-ZFPRPYN = 'Y'.
*    IT_ZSIVIT-ZFPRQN =  IT_ZSIVIT-MENGE   * ZTREQHD-ZFPREPAY / 100.
*    IT_ZSIVIT-ZFIVAMP = IT_ZSIVIT-ZFIVAMT * ZTREQHD-ZFPREPAY / 100.
*  ENDIF.
*  ADD IT_ZSIVIT-ZFIVAMP TO W_ZFIVAMP_S.

ENDFORM.                               " QTY_ITAMT_IVAMT_SCR3110
*&---------------------------------------------------------------------*
*&      Module  ZFIVAMC_CHECK_SCR3110  INPUT
*&---------------------------------------------------------------------*
MODULE ZFIVAMC_CHECK_SCR3110 INPUT.

  CHECK W_STATUS NE C_REQ_D.
  CHECK W_STATUS NE C_OPEN_U.

  IF ZTIV-ZFPOYN = 'N'.                " 무환.
    IF ZTIV-ZFIVAMC IS INITIAL.        " 통화코드.
      MESSAGE E721.
    ENDIF.
  ENDIF.

ENDMODULE.                             " ZFIVAMC_CHECK_SCR3110  INPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIV_DELETE
*&---------------------------------------------------------------------*
FORM P3000_ZTIV_DELETE_SCR3110.

  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  IF ANTWORT NE 'Y'.
    EXIT.
  ENDIF.

  DELETE FROM ZTIV WHERE ZFIVNO = ZTIV-ZFIVNO.
  DELETE FROM ZTIVIT WHERE ZFIVNO = ZTIV-ZFIVNO.
  PERFORM ZTBL_ZTIV_POYN.
  IF SY-SUBRC NE 0.
    MESSAGE  E727.
  ENDIF.

  MESSAGE S756.

  SET SCREEN 3200.
  LEAVE SCREEN.

ENDFORM.                               " P3000_ZTIV_DELETE
*&---------------------------------------------------------------------*
*&      Form  OK_CODE_TXCU
*&---------------------------------------------------------------------*
FORM OK_CODE_TXCU.

  IF OK-CODE EQ 'REF1'.
    PERFORM  P2000_REF1_PROCESS_SCR6710.
  ENDIF.
  IF OK-CODE EQ 'CRDC'.
    LEAVE TO TRANSACTION 'ZIM67'.
  ENDIF.
  IF OK-CODE EQ 'CHDC'.
    LEAVE TO TRANSACTION 'ZIM68'.
  ENDIF.
  IF OK-CODE EQ 'DISP'.
    LEAVE TO TRANSACTION 'ZIM69'.
  ENDIF.
  IF OK-CODE EQ 'DELE'.
    PERFORM P3000_ZTCUCLIV_DELETE_SCR6710.
  ENDIF.

ENDFORM.                               " OK_CODE_TXCU
*&---------------------------------------------------------------------*
*&      Form  OK_CODE_BACK_EXIT
*&---------------------------------------------------------------------*
FORM OK_CODE_BACK_EXIT.

  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                               " OK_CODE_BACK_EXIT
*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR6710  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR6710 INPUT.

  PERFORM OK_CODE_TXCU.

ENDMODULE.                             " AFI_PROCESS_SCR6710  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFIVAMC_CHECK_SCR6710  INPUT
*&---------------------------------------------------------------------*
MODULE ZFIVAMC_CHECK_SCR6710 INPUT.
  CHECK W_STATUS NE C_REQ_D.

  IF ZTCUCLIV-ZFIVAMC IS INITIAL.      " 통화코?
    MESSAGE E721.
  ENDIF.

  IF ZTCUCLIV-ZFCCDT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCUCLIV' 'ZFCCDT'.
  ENDIF.
  IF ZTCUCLIV-ZFCUT  IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCUCLIV' 'ZFCUT'.
  ENDIF.

  PERFORM  P1000_GET_CUT      USING      ZTCUCLIV-ZFCUT
                                         CHANGING  ZSIMIMG10-NAME1.

ENDMODULE.                             " ZFIVAMC_CHECK_SCR6710  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_6710_UPDATE_SCR6710  INPUT
*&---------------------------------------------------------------------*
MODULE TC_6710_UPDATE_SCR6710 INPUT.
  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  READ TABLE IT_ZSCUCLIVIT INDEX TC_6710-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.

  MOVE-CORRESPONDING ZSCUCLIVIT TO IT_ZSCUCLIVIT.
  IF OK-CODE = 'MKA1'.
    MOVE   'X'             TO W_ROW_MARK.
  ENDIF.
  IF OK-CODE = 'MKL1'.
    CLEAR  W_ROW_MARK.
  ENDIF.
  MOVE W_ROW_MARK TO IT_ZSCUCLIVIT-ZFMARK.

  IF IT_ZSCUCLIVIT-MATNR IS INITIAL.   " Material Code
    MESSAGE W718.
  ENDIF.

  IF IT_ZSCUCLIVIT-TXZ01 IS INITIAL.   " Short Text
    IF NOT ( IT_ZSCUCLIVIT-MATNR IS INITIAL ).
      SELECT SINGLE MAKTX INTO IT_ZSCUCLIVIT-TXZ01
        FROM MAKT
       WHERE SPRAS = SY-LANGU
         AND MATNR = IT_ZSCUCLIVIT-MATNR.
      IF SY-SUBRC NE 0.
        MESSAGE E716 WITH IT_ZSCUCLIVIT-MATNR.
      ENDIF.
    ELSE.
      MESSAGE E881.
    ENDIF.
  ENDIF.
  IF IT_ZSCUCLIVIT-STAWN IS INITIAL.   " HS Code
    IF NOT ( IT_ZSCUCLIVIT-MATNR IS INITIAL ).
      SELECT MAX( STAWN ) INTO IT_ZSCUCLIVIT-STAWN
        FROM MARC
       WHERE MATNR = IT_ZSCUCLIVIT-MATNR.
      IF SY-SUBRC NE 0 OR IT_ZSCUCLIVIT-STAWN IS INITIAL.
        MESSAGE E717 WITH IT_ZSCUCLIVIT-MATNR.
      ENDIF.
    ELSE.
      MESSAGE E882.
    ENDIF.
  ENDIF.

  MOVE IT_ZSCUCLIVIT-MEINS TO IT_ZSCUCLIVIT-BPRME.
  IF IT_ZSCUCLIVIT-MEINS IS INITIAL.   " 수량단?
    MESSAGE E719.
  ENDIF.
  IF IT_ZSCUCLIVIT-NETPR IS INITIAL.   " 단?
    MESSAGE E720.
  ENDIF.
  IF IT_ZSCUCLIVIT-PEINH IS INITIAL.   " 단가단?
    MOVE 1 TO IT_ZSCUCLIVIT-PEINH.
  ENDIF.

  IF W_OLD_SUBRC = 0.
    MODIFY IT_ZSCUCLIVIT   INDEX TC_6710-CURRENT_LINE.
  ELSE.
    IF IT_ZSCUCLIVIT-ZFIVDNO IS INITIAL.                " 일련번호 부?
      W_ZFIVDNO_M = W_ZFIVDNO_M + 10.
      MOVE W_ZFIVDNO_M TO IT_ZSCUCLIVIT-ZFIVDNO.
    ENDIF.
    APPEND IT_ZSCUCLIVIT.
  ENDIF.

ENDMODULE.                             " TC_6710_UPDATE_SCR6710  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_REF1_PROCESS_SCR6710
*&---------------------------------------------------------------------*
FORM P2000_REF1_PROCESS_SCR6710.

  REFRESH IT_ZSCUCLIVIT.
  CLEAR   IT_ZSCUCLIVIT.
  IF W_STATUS EQ 'C'.
    IF NOT ( MSEG-MBLNR IS INITIAL ) OR NOT ( MSEG-MJAHR IS INITIAL ).
      PERFORM READ_MSEG_TO_IT_SCR6700. " Material Document item -> IT
    ENDIF.
  ELSE.
    PERFORM READ_CUCLIVIT_TO_IT.
  ENDIF.

ENDFORM.                               " P2000_REF1_PROCESS_SCR6710
*&---------------------------------------------------------------------*
*&      Form  READ_CUCLIVIT_TO_IT
*&---------------------------------------------------------------------*
FORM READ_CUCLIVIT_TO_IT.

  REFRESH IT_ZSCUCLIVIT.
  CLEAR   IT_ZSCUCLIVIT.
  IF W_STATUS NE 'C'.
    SELECT *
      FROM ZTCUCLIVIT
     WHERE ZFIVNO EQ ZTCUCLIV-ZFIVNO .

      MOVE-CORRESPONDING ZTCUCLIVIT TO IT_ZSCUCLIVIT.

      APPEND IT_ZSCUCLIVIT.
    ENDSELECT.

    SELECT MAX( ZFIVDNO ) INTO W_ZFIVDNO_M
      FROM ZTCUCLIVIT
     WHERE ZFIVNO EQ ZTCUCLIV-ZFIVNO.
  ENDIF.

ENDFORM.                               " READ_CUCLIVIT_TO_IT
*&---------------------------------------------------------------------*
*&      Form  QTY_ITAMT_IVAMT_SCR6710
*&---------------------------------------------------------------------*
FORM QTY_ITAMT_IVAMT_SCR6710.

*   IF IT_ZSCUCLIVIT-MENGE = 0 AND IT_ZSCUCLIVIT-ZFIVAMT = 0.
*   ENDIF.
*   IF IT_ZSCUCLIVIT-MENGE = 0 AND IT_ZSCUCLIVIT-ZFIVAMT > 0.
*      IT_ZSCUCLIVIT-MENGE = IT_ZSCUCLIVIT-ZFIVAMT /
*                            IT_ZSCUCLIVIT-NETPR * IT_ZSCUCLIVIT-PEINH.
*   ENDIF.
*   IF IT_ZSCUCLIVIT-MENGE > 0 AND IT_ZSCUCLIVIT-ZFIVAMT = 0.
  IT_ZSCUCLIVIT-ZFIVAMT = IT_ZSCUCLIVIT-MENGE *
                          IT_ZSCUCLIVIT-NETPR / IT_ZSCUCLIVIT-PEINH.
*   ENDIF.
*   IF IT_ZSCUCLIVIT-MENGE > 0 AND IT_ZSCUCLIVIT-ZFIVAMT > 0.
*   ENDIF.
  ADD IT_ZSCUCLIVIT-ZFIVAMT TO W_ZFIVAMT_S.

ENDFORM.                               " QTY_ITAMT_IVAMT_SCR6710
*&---------------------------------------------------------------------*
*&      Form  DIST_PROCESS_SCR6710
*&---------------------------------------------------------------------*
FORM DIST_PROCESS_SCR6710.

  IF ZTCUCLIV-ZFIVAMT EQ W_ZFIVAMT_S.
    EXIT.
  ENDIF.

  PERFORM P2000_MESSAGE_BOX USING
               '배부 확인'             " 타이틀...
               'I/T 금액 합과 I/V 금액의 차이가 발생.'
               'Item 으로 배부하시겠습니까?'                " Message #2
               'N'                     " 취소 버튼 유/?
               '1'.                    " default button

  IF ANTWORT NE 'Y'.
    MOVE W_ZFIVAMT_S TO ZTCUCLIV-ZFIVAMT.
    EXIT.
  ENDIF.

  W_ZFIVAMT_D = ZTCUCLIV-ZFIVAMT - W_ZFIVAMT_S.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
       EXPORTING
            CURRENCY             = ZTCUCLIV-ZFIVAMC
            AMOUNT_EXTERNAL      = W_ZFIVAMT_D
            MAX_NUMBER_OF_DIGITS = DIGITS
       IMPORTING
            AMOUNT_INTERNAL      = W_ZFIVAMT_D
       EXCEPTIONS
            OTHERS               = 1.

  WRITE  W_ZFIVAMT_D TO W_TEXT18 CURRENCY ZTCUCLIV-ZFIVAMC.
  MESSAGE S724 WITH W_TEXT18.

  CLEAR W_ZFIVAMT_T.
  LOOP AT IT_ZSCUCLIVIT.
    READ TABLE IT_ZSCUCLIVIT  WITH KEY IT_ZSCUCLIVIT(33)  BINARY SEARCH.
    IT_ZSCUCLIVIT-ZFIVAMT = IT_ZSCUCLIVIT-ZFIVAMT *
                            ZTCUCLIV-ZFIVAMT / W_ZFIVAMT_S.
    ADD IT_ZSCUCLIVIT-ZFIVAMT TO W_ZFIVAMT_T.
    MODIFY IT_ZSCUCLIVIT    INDEX SY-TABIX.
  ENDLOOP.

  IF ZTCUCLIV-ZFIVAMT NE W_ZFIVAMT_T.  " 단수차이조?
    IT_ZSCUCLIVIT-ZFIVAMT = IT_ZSCUCLIVIT-ZFIVAMT +
                            ZTCUCLIV-ZFIVAMT - W_ZFIVAMT_S.
    MODIFY IT_ZSCUCLIVIT    INDEX SY-TABIX.
  ENDIF.

ENDFORM.                               " DIST_PROCESS_SCR6710
*&---------------------------------------------------------------------*
*&      Module  READ_ZTCUCLIV_SCR6700  INPUT
*&---------------------------------------------------------------------*
MODULE READ_ZTCUCLIV_SCR6700 INPUT.

  PERFORM OK_CODE_TXCU.
  PERFORM OK_CODE_BACK_EXIT.

  IF ZTBL-ZFHBLNO IS INITIAL AND ZTBL-ZFBLNO IS INITIAL.
    MESSAGE E706.
  ENDIF.
  IF NOT ( ZTBL-ZFHBLNO IS INITIAL ).  " B/L 관리번호 Search
    SELECT COUNT( DISTINCT ZFBLNO ) INTO W_COUNT
      FROM ZTBL
     WHERE ZFHBLNO = ZTBL-ZFHBLNO.
    CASE W_COUNT.
      WHEN 0.
        MESSAGE E707 WITH ZTBL-ZFHBLNO.
      WHEN 1.
        SELECT SINGLE *
          FROM ZTBL
         WHERE ZFHBLNO = ZTBL-ZFHBLNO.
      WHEN OTHERS.
        MESSAGE E708 WITH ZTBL-ZFHBLNO.
    ENDCASE.
  ENDIF.

  IF NOT ( ZTBL-ZFBLNO IS INITIAL ) AND ZTBL-ZFHBLNO IS INITIAL.
    SELECT SINGLE *
      FROM ZTBL
     WHERE ZFBLNO = ZTBL-ZFBLNO.
    IF SY-SUBRC NE 0.
      MESSAGE E711 WITH ZTBL-ZFBLNO.
    ENDIF.
  ENDIF.

  MOVE ZTBL-ZFBLNO TO ZTCUCLIV-ZFBLNO.

  REFRESH IT_ZSCUCLIVIT.
  CLEAR   IT_ZSCUCLIVIT.

  IF NOT ( MSEG-MBLNR IS INITIAL ) OR NOT ( MSEG-MJAHR IS INITIAL ).
    SELECT COUNT( DISTINCT MBLNR ) INTO W_COUNT
      FROM MSEG
     WHERE MBLNR = MSEG-MBLNR
       AND MJAHR = MSEG-MJAHR.
    IF W_COUNT = 0.
      MESSAGE E863.
    ENDIF.
    SELECT COUNT( DISTINCT ZFIVNO ) MAX( ZFIVNO )
      INTO (W_COUNT, ZTCUCLIV-ZFIVNO)
      FROM ZTCUCLIV
     WHERE ZFBLNO = ZTBL-ZFBLNO
       AND ZFCLSEQ = ZTCUCLIV-ZFCLSEQ
       AND MJAHR = MSEG-MJAHR
       AND MBLNR = MSEG-MBLNR.
    IF W_COUNT > 0.
      MESSAGE E710 WITH ZTCUCLIV-ZFIVNO.
    ENDIF.
    PERFORM READ_MSEG_TO_IT_SCR6700. " Material Document item -> IT
    MOVE MSEG-MJAHR TO ZTCUCLIV-MJAHR.
    MOVE MSEG-MBLNR TO ZTCUCLIV-MBLNR.
    MOVE '11'       TO ZTCUCLIV-ZFPONC.
  ENDIF.


ENDMODULE.                             " READ_ZTCUCLIV_SCR6700  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_ZTCUCLIV_SCR6800_SCR6900  INPUT
*&---------------------------------------------------------------------*
MODULE READ_ZTCUCLIV_SCR6800_SCR6900 INPUT.

  PERFORM OK_CODE_TXCU.
  PERFORM OK_CODE_BACK_EXIT.

  IF  ZTCUCLIV-ZFIVNO IS INITIAL.
    MESSAGE E729.
  ENDIF.

  SELECT SINGLE * FROM ZTCUCLIV
   WHERE ZFIVNO = ZTCUCLIV-ZFIVNO
     AND ZFCLCD = 'B'.

  IF SY-SUBRC NE 0.
    MESSAGE E723.
  ENDIF.

*>> 비용DOCUMENT SELECT.
  W_ZFIMDNO = ZTCUCLIV-ZFIVNO.
  CALL FUNCTION 'ZIM_GET_COST_DOCUMENT'
       EXPORTING
            ZFCSTGRP    = '006'
            ZFIMDNO     = W_ZFIMDNO
       TABLES
            IT_ZSIMCOST = IT_ZSIMCOST.

  IF ZTCUCLIV-ZFCUST NE '1' AND  SY-DYNNR = '6800'.
    MESSAGE E731.
  ENDIF.

  PERFORM READ_CUCLIVIT_TO_IT.

  CLEAR ZTBL.
  SELECT SINGLE * FROM ZTBL
                  WHERE ZFBLNO = ZTCUCLIV-ZFBLNO.

  IF SY-TCODE EQ 'ZIM68'.
    PERFORM  P2000_SET_ZTCUCLIV_LOCK    USING   'L'.
  ENDIF.

ENDMODULE.                 " READ_ZTCUCLIV_SCR6800_SCR6900  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6900  OUTPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6900 OUTPUT.

  MOVE C_REQ_D TO W_STATUS.

  SET SCREEN 6710.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR6900  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIV_SCR3200_SCR3300
*&---------------------------------------------------------------------*
FORM READ_ZTIV_SCR3200_SCR3300.

  CLEAR: ZTREQST-ZFOPNDT, W_ZFFORD_NM, W_LIFNR_NM, W_ZFBENI_NM, W_SHAMT.
*>  2000/10/13  강석봉 수정 작업(강나형 대리 요청)
*  IF NOT ( ZTIV-ZFCIVNO IS INITIAL ) AND ZTIV-ZFIVNO IS INITIAL.
*  IF NOT ( ZTIV-ZFCIVNO IS INITIAL ).
*    SELECT COUNT( DISTINCT ZFIVNO ) INTO W_COUNT
*           FROM  ZTIV
*           WHERE ZFCIVNO = ZTIV-ZFCIVNO.
*    IF W_COUNT > 1.
*      MESSAGE E741 WITH ZTIV-ZFCIVNO.
*    ENDIF.
*    IF W_COUNT = 1.
*      SELECT SINGLE * FROM ZTIV
*             WHERE ZFCIVNO = ZTIV-ZFCIVNO.
*    ENDIF.
*  ENDIF.

  SELECT SINGLE * FROM ZTIV
   WHERE ZFIVNO = ZTIV-ZFIVNO.
  IF SY-SUBRC NE 0.
    MESSAGE E723.
  ENDIF.
* LOCK OBJECT
  IF SY-TCODE EQ 'ZIM32'.
    PERFORM    P2000_SET_IV_DOC_LOCK USING    'L'.
  ENDIF.

*  IF SY-DYNNR = '3200'.
*    IF ZTIV-ZFPRPYN = 'N' AND ZTIV-ZFCUST NE '1'.
*      MESSAGE E731.
*    ENDIF.
*    IF ZTIV-ZFPRPYN = 'Y' AND ZTIV-ZFIVST NE 'N'.
*      MESSAGE E731.
*    ENDIF.
*  ENDIF.

  IF NOT ZTIV-ZFBLNO IS INITIAL.
    SELECT SINGLE * FROM ZTBL
           WHERE ZFBLNO = ZTIV-ZFBLNO.
* B/L LOCK
*     IF SY-TCODE EQ 'ZIM32'.    " 변?
*        PERFORM P2000_SET_BL_DOC_LOCK USING    'L'.
*     ENDIF.
  ENDIF.

*  SELECT SUM( ZFIVAMT ) INTO W_TMPAMT FROM ZTIV
*                        WHERE ZFBLNO = ZTIV-ZFBLNO
*                        AND ZFPRPYN = 'N'.
  IF ZTBL-ZFBLAMT > W_TMPAMT.
    W_UBLAMT = ZTBL-ZFBLAMT - W_TMPAMT.
  ENDIF.

*  IF NOT ( ZTIV-ZFREQNO IS INITIAL ).
*     SELECT SINGLE * FROM ZTREQHD
*            WHERE ZFREQNO = ZTIV-ZFREQNO.
*     SELECT SUM( ZFIVAMT ) INTO W_TMPAMT FROM ZTIV
*            WHERE ZFREQNO = ZTIV-ZFREQNO.
*     IF ZTREQHD-ZFLASTAM > W_TMPAMT.
*        W_SHAMT = ZTREQHD-ZFLASTAM - W_TMPAMT.
*     ENDIF.
* 수입의뢰 READ/LOCK
*     IF SY-TCODE EQ 'ZIM32'.     " 변?
*        PERFORM  P1000_READ_ZTREQST USING     ZTIV-ZFREQNO.
*     ENDIF.
*  ENDIF.

  SELECT SINGLE NAME1 INTO W_ZFFORD_NM " Forewarder
    FROM LFA1
   WHERE LIFNR = ZTBL-ZFFORD.
  SELECT SINGLE NAME1 INTO W_LIFNR_NM  " Vendor
    FROM LFA1
   WHERE LIFNR = ZTREQHD-LIFNR.
  SELECT SINGLE NAME1 INTO W_ZFBENI_NM " Benificiary
    FROM LFA1
   WHERE LIFNR = ZTREQHD-ZFBENI.
  SELECT MAX( ZFOPNDT ) INTO ZTREQST-ZFOPNDT
    FROM ZTREQST
   WHERE ZFREQNO = ZTREQHD-ZFREQNO.

  REFRESH IT_ZSIVIT.
  CLEAR   IT_ZSIVIT.
  PERFORM READ_IVIT_TO_IT.             " I/V Item -> Int

ENDFORM.                               " READ_ZTIV_SCR3200_SCR3300
*&---------------------------------------------------------------------*
*&      Module  REQNO_CHECK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE REQNO_CHECK_SCRCOM INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분기.
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  W_COUNT = 1.
  ANTWORT = 'N'.
* Input Check.
  IF ZSIV-ZFHBLNO IS INITIAL AND ZSIV-ZFBLNO  IS INITIAL.
    IF ZSIV-ZFIVNO  IS INITIAL.
       MESSAGE E411.  EXIT.
    ENDIF.
  ELSE.
    IF NOT ZSIV-ZFHBLNO IS INITIAL. " B/L NO Input.
      CLEAR : W_COUNT.
      SELECT ZFHBLNO INTO W_ZFHBLNO
      FROM   ZVBL_IV
      WHERE  ZFHBLNO EQ ZSIV-ZFHBLNO
      GROUP BY ZFHBLNO.
         W_COUNT = W_COUNT + 1.
      ENDSELECT.
      CASE W_COUNT.
        WHEN 0.     MESSAGE W305 WITH ZSIV-ZFHBLNO.
        WHEN 1.
          SELECT ZFIVNO INTO ZSIV-ZFIVNO UP TO 1 ROWS
                        FROM ZVBL_IV
                        WHERE ZFHBLNO EQ ZSIV-ZFHBLNO.
          ENDSELECT.
        WHEN OTHERS.
          PERFORM P2000_CC_DOC_ITEM_SELECT.
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          PERFORM P2000_SEARCH_FIELD_MOVE.
      ENDCASE.
      IF W_COUNT NE 0.
        ZTIV-ZFIVNO = ZSIV-ZFIVNO.
        PERFORM   P1000_READ_IV_DOC.
        EXIT.
      ENDIF.
    ENDIF.
    IF NOT ZSIV-ZFBLNO IS INITIAL.
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTIV
                        WHERE ZFBLNO EQ ZSIV-ZFBLNO.
      CASE W_COUNT.
        WHEN 0.     MESSAGE W038 WITH ZSIV-ZFBLNO.
        WHEN 1.
          SELECT ZFIVNO INTO ZSIV-ZFIVNO UP TO 1 ROWS
                        FROM ZTIV
                        WHERE ZFBLNO EQ ZSIV-ZFBLNO.
          ENDSELECT.
        WHEN OTHERS.
          PERFORM P2000_CC_DOC_ITEM_SELECT_1.
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          PERFORM P2000_SEARCH_FIELD_MOVE.
      ENDCASE.
      IF W_COUNT NE 0.
        ZTIV-ZFIVNO = ZSIV-ZFIVNO.
        PERFORM   P1000_READ_IV_DOC.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT ZSIV-ZFIVNO  IS INITIAL.    " 관리번호가 입력 경우.
    ANTWORT = 'N'.
    W_COUNT = '1'.
    ZTIV-ZFIVNO = ZSIV-ZFIVNO.
    PERFORM   P1000_READ_IV_DOC.
  ENDIF.

*  PERFORM READ_ZTIV_SCR3200_SCR3300.
ENDMODULE.                 " REQNO_CHECK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  REQNO_CHECK_SCR3100  INPUT
*&---------------------------------------------------------------------*
MODULE REQNO_CHECK_SCR3100 INPUT.

  PERFORM OK_CODE_COIV.
  PERFORM OK_CODE_BACK_EXIT.

*  MOVE ZTIV-ZFBLNO TO W_ZFBLNO.
*  MOVE ZTIV-ZFREQNO TO W_ZFREQNO.
*  MOVE ZTIV-ZFCIVNO TO W_ZFCIVNO.
  CLEAR ZTIV.
*  MOVE W_ZFBLNO TO ZTIV-ZFBLNO.
*  MOVE W_ZFREQNO TO ZTIV-ZFREQNO.
*  MOVE W_ZFCIVNO TO ZTIV-ZFCIVNO.

  IF RADIO_HBL = 'X'.
    IF ZTBL-ZFHBLNO IS INITIAL.        " B/L 관리번호 Search By HBL
      MESSAGE E706.
    ELSE.
      SELECT COUNT( DISTINCT ZFBLNO ) INTO W_COUNT FROM ZTBL
             WHERE ZFHBLNO = ZTBL-ZFHBLNO.
      CASE W_COUNT.
        WHEN 0.
          MESSAGE E707 WITH ZTBL-ZFHBLNO.
        WHEN 1.

          SELECT SINGLE * FROM ZTBL
                 WHERE ZFHBLNO = ZTBL-ZFHBLNO.
          MOVE ZTBL-ZFBLNO TO ZTIV-ZFBLNO.
*>> B/L LOCK
          PERFORM P2000_SET_BL_DOC_LOCK USING    'L'.
        WHEN OTHERS.
          MESSAGE E708 WITH ZTBL-ZFHBLNO.
      ENDCASE.
    ENDIF.
  ENDIF.

  IF RADIO_BLN = 'X'.
    IF ZTIV-ZFBLNO IS INITIAL.         " B/L 관리번호 Search
      MESSAGE E715.
    ELSE.
      SELECT SINGLE * FROM ZTBL
             WHERE ZFBLNO = ZTIV-ZFBLNO.
      IF SY-SUBRC NE 0.
        MESSAGE E711 WITH ZTIV-ZFBLNO.
      ENDIF.
*>> B/L LOCK
      PERFORM P2000_SET_BL_DOC_LOCK USING    'L'.
    ENDIF.
  ENDIF.

  IF RADIO_NP = 'X'.
    MOVE 'N' TO ZTIV-ZFPOYN.
*    CLEAR ZTIV-ZFREQNO.
    CLEAR ZTREQHD.
  ELSE.
    MOVE 'Y' TO ZTIV-ZFPOYN.
  ENDIF.

  IF RADIO_PO = 'X'.
    IF ZTREQHD-EBELN IS INITIAL.       " 수입의뢰 관리번호 Search By PO
      MESSAGE E733.
    ELSE.
      SELECT COUNT( DISTINCT ZFREQNO ) INTO W_COUNT
             FROM ZTREQHD
             WHERE EBELN = ZTREQHD-EBELN.
      CASE W_COUNT.
        WHEN 0.
          MESSAGE E732 WITH ZTREQHD-EBELN.
        WHEN 1.
          SELECT SINGLE * FROM ZTREQHD
                 WHERE EBELN = ZTREQHD-EBELN.
*          MOVE ZTREQHD-ZFREQNO TO ZTIV-ZFREQNO.
        WHEN OTHERS.
          MESSAGE E734 WITH ZTREQHD-EBELN.
      ENDCASE.
*      IF ZTREQHD-ZFCLOSE = 'X'.
*         MESSAGE E735 WITH ZTIV-ZFREQNO.
*      ENDIF.
* ZTREQST READ
      PERFORM   P1000_READ_ZTREQST   USING   ZTREQHD-ZFREQNO.
    ENDIF.                             " 수입의뢰 관리번호 Search By PO
  ENDIF.

  IF RADIO_LC = 'X'.
    IF ZTREQHD-ZFOPNNO IS INITIAL.     " 수입의뢰 관리번호 Search By LC
      MESSAGE E736.
    ELSE.
      SELECT COUNT( DISTINCT ZFREQNO ) INTO W_COUNT
             FROM ZTREQHD
             WHERE ZFOPNNO = ZTREQHD-ZFOPNNO.
      CASE W_COUNT.
        WHEN 0.
          MESSAGE E709 WITH ZTREQHD-ZFOPNNO.
        WHEN 1.
          SELECT SINGLE * FROM ZTREQHD
                 WHERE ZFOPNNO = ZTREQHD-ZFOPNNO.
*           MOVE ZTREQHD-ZFREQNO TO ZTIV-ZFREQNO.
        WHEN OTHERS.
          MESSAGE E713 WITH ZTREQHD-ZFOPNNO.
      ENDCASE.

      IF ZTREQHD-ZFCLOSE = 'X'.
        MESSAGE E714 WITH ZTREQHD-ZFOPNNO.
      ENDIF.
* ZTREQST READ
      PERFORM   P1000_READ_ZTREQST   USING   ZTREQHD-ZFREQNO.

    ENDIF.                             " 수입의뢰 관리번호 Search By LC
  ENDIF.

  IF RADIO_RQ = 'X'.
*    IF ZTIV-ZFREQNO IS INITIAL.
*      MESSAGE E737.
*    ELSE.
*      SELECT SINGLE * FROM ZTREQHD
*       WHERE ZFREQNO = ZTIV-ZFREQNO.
*      IF SY-SUBRC NE 0.
*        MESSAGE E712 WITH ZTIV-ZFREQNO.
*      ENDIF.
*      IF ZTREQHD-ZFCLOSE = 'X'.
*        MESSAGE E735 WITH ZTIV-ZFREQNO.
*      ENDIF.
* ZTREQST READ
*      PERFORM   P1000_READ_ZTREQST   USING   ZTREQHD-ZFREQNO.
*
*    ENDIF.
  ENDIF.

  IF RADIO_NPO = 'X'.
    IF ( MSEG-MBLNR IS INITIAL ) OR ( MSEG-MJAHR IS INITIAL ).
      MESSAGE E862.
    ELSE.
      SELECT COUNT( DISTINCT MBLNR ) INTO W_COUNT
        FROM MSEG
       WHERE MBLNR = MSEG-MBLNR
         AND MJAHR = MSEG-MJAHR.
      IF W_COUNT = 0.
        MESSAGE E863.
      ENDIF.
      CLEAR W_EBELN.
      SELECT MAX( EBELN ) INTO W_EBELN
        FROM MSEG
       WHERE MBLNR = MSEG-MBLNR
         AND MJAHR = MSEG-MJAHR.
      IF W_EBELN IS INITIAL.
        MESSAGE E863.
      ENDIF.
    ENDIF.
  ENDIF.

  PERFORM READ_ZTIV_SCR3100.

ENDMODULE.                             " REQNO_CHECK_SCR3100  INPUT
*&---------------------------------------------------------------------*
*&      Module  REQNO_CHECK_FOR_SCR5710  INPUT
*&---------------------------------------------------------------------*
MODULE IVNO_CHECK_FOR_SCR5710 INPUT.

  PERFORM OK_CODE_AMTF.
  PERFORM OK_CODE_BACK_EXIT.

  PERFORM READ_ZTIV_FOR_SCR5710.

ENDMODULE.                             " ivNO_CHECK_FOR_SCR5710  INPUT

*&---------------------------------------------------------------------*
*&      Form  OK_CODE_AMTF
*&---------------------------------------------------------------------*
FORM OK_CODE_AMTF.

  IF OK-CODE EQ 'IVPR'.
    LEAVE TO TRANSACTION 'ZIM56'.
  ENDIF.
  IF OK-CODE EQ 'AMTF'.
    LEAVE TO TRANSACTION 'ZIM34'.
  ENDIF.
  IF OK-CODE EQ 'AMTI'.
    LEAVE TO TRANSACTION 'ZIM34'.
  ENDIF.
  IF OK-CODE EQ 'STCG'.
    LEAVE TO TRANSACTION 'ZIM34'.
  ENDIF.
  IF OK-CODE EQ 'DSRQ'.
*    SET PARAMETER ID 'ZPREQNO' FIELD ZTIV-ZFREQNO.
*    SET PARAMETER ID 'BES'     FIELD ''.
*    SET PARAMETER ID 'ZPOPNNO' FIELD ''.
*    EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
*    CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
  ENDIF.
  IF OK-CODE EQ 'DSBL'.
    SET PARAMETER ID 'ZPBLNO'  FIELD ZTIV-ZFBLNO.
    SET PARAMETER ID 'ZPHBLNO' FIELD ''.
    EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
    CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
  ENDIF.
  IF OK-CODE EQ 'FB031'.
*    CHECK NOT ZTIV-ZFGFDNO IS INITIAL.
*    SELECT SINGLE * FROM ZTREQHD WHERE ZFREQNO EQ ZTIV-ZFREQNO.

*    SET PARAMETER ID 'GJR'     FIELD ZTIV-ZFGFDYR.
*    SET PARAMETER ID 'BUK'     FIELD ZTREQHD-BUKRS.
    CLEAR ZTIMIMG00.
    SELECT SINGLE * FROM ZTIMIMG00.
    CASE ZTIMIMG00-ZFIVTY.
      WHEN 'C'.
*               SET PARAMETER ID 'BLN'      FIELD ZTIV-ZFGFDNO.
*               CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
      WHEN 'L'.
*               SET PARAMETER ID 'RBN'      FIELD ZTIV-ZFGFDNO.
*               CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.
      WHEN  OTHERS.
        EXIT.
    ENDCASE.
  ENDIF.
  IF OK-CODE EQ 'FB032'.
*    CHECK NOT ZTIV-ZFCFDNO IS INITIAL.
*    SELECT SINGLE * FROM ZTREQHD WHERE ZFREQNO EQ ZTIV-ZFREQNO.
*    SET PARAMETER ID 'GJR'     FIELD ZTIV-ZFCFDYR.
*    SET PARAMETER ID 'BUK'     FIELD ZTREQHD-BUKRS.
    CLEAR ZTIMIMG00.
    SELECT SINGLE * FROM ZTIMIMG00.
    CASE ZTIMIMG00-ZFIVTY.
      WHEN 'C'.
*               SET PARAMETER ID 'BLN'      FIELD ZTIV-ZFCFDNO.
*               CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
      WHEN 'L'.
*               SET PARAMETER ID 'RBN'      FIELD ZTIV-ZFCFDNO.
*               CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.
      WHEN  OTHERS.
        EXIT.
    ENDCASE.
  ENDIF.
  IF OK-CODE EQ 'MB03'.
*    CHECK NOT ZTIV-ZFMDNO IS INITIAL.
*    SELECT SINGLE * FROM ZTREQHD WHERE ZFREQNO EQ ZTIV-ZFREQNO.
*    SET PARAMETER ID 'BUK'     FIELD ZTREQHD-BUKRS.
*    SET PARAMETER ID 'MJA'     FIELD ZTIV-ZFMDYR.
*    SET PARAMETER ID 'MBN'     FIELD ZTIV-ZFMDNO.
*    SET PARAMETER ID 'POS'     FIELD ''.
*    CALL TRANSACTION 'MB03' AND SKIP  FIRST SCREEN.
  ENDIF.
  IF OK-CODE EQ 'MK03'.
*    PERFORM P2000_VENDOR_DISPLAY USING ZTIV-ZFMAVN.
  ENDIF.

ENDFORM.                               " OK_CODE_AMTF
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIV_FOR_SCR5710
*&---------------------------------------------------------------------*
FORM READ_ZTIV_FOR_SCR5710.

  CLEAR: ZTREQST-ZFOPNDT, W_ZFFORD_NM, W_LIFNR_NM, W_ZFBENI_NM, W_SHAMT.
*>  2000/10/13  강석봉 수정 작업(강나형 대리 요청)
*  IF NOT ( ZTIV-ZFCIVNO IS INITIAL ) AND ZTIV-ZFIVNO IS INITIAL.
*  IF NOT ( ZTIV-ZFCIVNO IS INITIAL ).
*     SELECT COUNT( DISTINCT ZFIVNO ) INTO W_COUNT
*            FROM ZTIV
*            WHERE ZFCIVNO = ZTIV-ZFCIVNO.
*     IF W_COUNT > 1.
*        MESSAGE E741 WITH ZTIV-ZFCIVNO.
*     ENDIF.
*
*     IF W_COUNT = 1.
*        SELECT * FROM ZTIV UP TO 1 ROWS
*                 WHERE ZFCIVNO = ZTIV-ZFCIVNO.
*        ENDSELECT.
*     ENDIF.
*  ENDIF.

  SELECT SINGLE * FROM ZTIV
         WHERE ZFIVNO = ZTIV-ZFIVNO.
  IF SY-SUBRC NE 0.
    MESSAGE E723.
  ENDIF.

* ?>>>> LOCK OBJECT
  IF SY-TCODE EQ 'ZIM34' OR SY-TCODE EQ 'ZIM57'.
    PERFORM    P2000_SET_IV_DOC_LOCK USING    'L'.
  ENDIF.

  IF SY-DYNNR = '5700'.
*    IF ZTIV-ZFCUST NE 'Y' OR ZTIV-ZFCDST NE 'Y' OR
*       ZTIV-ZFIVST NE 'N' OR ZTIV-ZFGRST NE 'N' OR
*       ZTIV-ZFPAYYN NE 'N' OR ZTIV-ZFCIVST NE 'N'.
*       MESSAGE W744.
*    ENDIF.
  ENDIF.

  IF NOT ( ZTIV-ZFBLNO IS INITIAL ).
    SELECT SINGLE * FROM ZTBL
                    WHERE ZFBLNO = ZTIV-ZFBLNO.
*    IF SY-TCODE EQ 'ZIM32' OR SY-TCODE EQ 'ZIM57'.
*       PERFORM P2000_SET_BL_DOC_LOCK USING    'L'.
*    ENDIF.
  ENDIF.

*  IF NOT ( ZTIV-ZFREQNO IS INITIAL ).
*    SELECT SINGLE * FROM ZTREQHD
*                    WHERE ZFREQNO = ZTIV-ZFREQNO.
*    IF SY-TCODE EQ 'ZIM32'.
*       PERFORM  P1000_READ_ZTREQST USING     ZTIV-ZFREQNO.
*    ENDIF.
*  ENDIF.

  SELECT SINGLE NAME1 INTO W_ZFMAVN_NM " 물대 Vendor
    FROM LFA1
   WHERE LIFNR = ZTIV-LIFNR.
  SELECT SINGLE NAME1 INTO W_ZFPHVN_NM " Phandom Vendor
    FROM LFA1
   WHERE LIFNR = ZTIV-ZFPHVN.
  W_ZFIMOC_SUM = ZTIV-ZFDAMT + ZTIV-ZFUPCST + ZTIV-ZFPCST.
  PERFORM READ_IVIT_TO_IT.             " I/V Item -> Int

ENDFORM.                               " READ_ZTIV_FOR_SCR5710
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR5800  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR5800 INPUT.

  MOVE C_REQ_D TO W_STATUS.

  SET SCREEN 5710.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR5800  INPUT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIV_SCR3100
*&---------------------------------------------------------------------*
FORM READ_ZTIV_SCR3100.

  CLEAR: ZTREQST-ZFOPNDT, W_ZFFORD_NM, W_LIFNR_NM, W_ZFBENI_NM, W_SHAMT.
  CLEAR: W_UBLAMT.

*  IF NOT ( ZTIV-ZFCIVNO IS INITIAL ).
*    SELECT COUNT( DISTINCT ZFIVNO ) INTO W_COUNT
*      FROM ZTIV
*     WHERE ZFCIVNO = ZTIV-ZFCIVNO.
*    IF W_COUNT > 0.
*      MESSAGE W740 WITH ZTIV-ZFCIVNO.
*    ENDIF.
*  ENDIF.

  SELECT COUNT( DISTINCT ZFIVNO ) INTO W_COUNT
    FROM ZTIV
   WHERE ZFBLNO = ZTIV-ZFBLNO
     AND ZFCUST NE '1'.
  IF W_COUNT > 0.
    MESSAGE W745.
  ENDIF.

  IF NOT ( ZTREQHD-ZFREQNO IS INITIAL ).              " 유환여?
    MOVE 'Y'  TO ZTIV-ZFPOYN.
    MOVE '11' TO ZTIV-ZFPONC.
  ELSE.
    MOVE 'N' TO ZTIV-ZFPOYN.
    CLEAR W_ZFIVDNO_M.                 " Item 일련번?
  ENDIF.

  IF ZTIV-ZFPOYN = 'Y'.   " 유환.

*     SELECT COUNT( DISTINCT ZFIVNO ) MAX( ZFIVNO )
*       INTO (W_COUNT, ZTIV-ZFIVNO)
*       FROM ZTIV
*      WHERE ZFBLNO = ZTIV-ZFBLNO
*        AND ZFREQNO = ZTIV-ZFREQNO.
    IF W_COUNT > 0.
*       MESSAGE E710 WITH ZTIV-ZFIVNO.
    ENDIF.

*     SELECT SUM( ZFIVAMT ) SUM( ZFPKCHG ) SUM( ZFHDCHG )
*       INTO (W_TMPAMT, W_ZFPKCHG, W_ZFHDCHG)
*       FROM ZTIV
*      WHERE ZFREQNO = ZTIV-ZFREQNO
*        AND ZFPRPYN = 'N'.
    IF ZTREQHD-ZFLASTAM > W_TMPAMT.
      W_SHAMT = ZTREQHD-ZFLASTAM - W_TMPAMT.
    ENDIF.
    IF ZTREQHD-ZFPKCHG > W_ZFPKCHG.
      ZTIV-ZFPKCHG = ZTREQHD-ZFPKCHG - W_ZFPKCHG.
    ENDIF.
    IF ZTREQHD-ZFHDCHG > W_ZFHDCHG.
      ZTIV-ZFHDCHG = ZTREQHD-ZFHDCHG - W_ZFHDCHG.
    ENDIF.
    MOVE ZTREQHD-WAERS  TO ZTIV-ZFIVAMC.
    MOVE ZTREQHD-ZFBENI TO ZTIV-LIFNR.

    SELECT SINGLE NAME1 INTO W_LIFNR_NM  " Vendor
      FROM LFA1
     WHERE LIFNR = ZTREQHD-LIFNR.
    SELECT SINGLE NAME1 INTO W_ZFBENI_NM " Benificiary
      FROM LFA1
     WHERE LIFNR = ZTREQHD-ZFBENI.
    SELECT MAX( ZFOPNDT ) INTO ZTREQST-ZFOPNDT         " 개설?
      FROM ZTREQST
     WHERE ZFREQNO = ZTREQHD-ZFREQNO.
  ENDIF.

  IF RADIO_NPO = 'X'.
*     SELECT COUNT( DISTINCT ZFIVNO ) MAX( ZFIVNO )
*       INTO (W_COUNT, ZTIV-ZFIVNO)
*       FROM ZTIV
*      WHERE ZFBLNO = ZTIV-ZFBLNO
*        AND MJAHR = MSEG-MJAHR
*        AND MBLNR = MSEG-MBLNR.
    IF W_COUNT > 0.
      MESSAGE E710 WITH ZTIV-ZFIVNO.
    ENDIF.
  ENDIF.

  SELECT SINGLE NAME1 INTO W_ZFFORD_NM " Forewarder
    FROM LFA1
   WHERE LIFNR = ZTBL-ZFFORD.
*  SELECT SUM( ZFIVAMT ) INTO W_TMPAMT
*    FROM ZTIV
*   WHERE ZFBLNO = ZTIV-ZFBLNO
*     AND ZFPRPYN = 'N'.
  IF ZTBL-ZFBLAMT > W_TMPAMT.
    W_UBLAMT = ZTBL-ZFBLAMT - W_TMPAMT.
  ENDIF.

  REFRESH IT_ZSIVIT.
  CLEAR   IT_ZSIVIT.
*  MOVE 'N' TO ZTIV-ZFPRPYN.
  MOVE 'N' TO ZTIV-ZFPONMA.
  IF RADIO_NPO = 'X'.
*     MOVE MSEG-MJAHR TO ZTIV-MJAHR.
*     MOVE MSEG-MBLNR TO ZTIV-MBLNR.
    MOVE 'Y'   TO ZTIV-ZFPONMA.
    PERFORM READ_MSEG_TO_IT.           " Material Document item ->
  ELSE.
*    IF NOT ( ZTIV-ZFREQNO IS INITIAL ).
*      IF ZTREQHD-ZFPREPAY > 0.
*        SELECT COUNT( DISTINCT ZFREQNO ) INTO W_COUNT
*          FROM ZTIV
*         WHERE ZFREQNO = ZTIV-ZFREQNO.
*        IF W_COUNT = 0.
*          MOVE 'Y' TO ZTIV-ZFPRPYN.
*        ENDIF.
*      ENDIF.
*      PERFORM READ_LCIT_TO_IT.         " L/C Item -> Int
*    ENDIF.
  ENDIF.
ENDFORM.                               " READ_ZTIV_SCR3100
*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR5710  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR5710 INPUT.

  PERFORM OK_CODE_AMTF.

ENDMODULE.                             " AFI_PROCESS_SCR5710  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_5710_UPDATE_SCR5710  INPUT
*&---------------------------------------------------------------------*
MODULE TC_5710_UPDATE_SCR5710 INPUT.
*  IF W_STATUS = 'D'.      EXIT.   ENDIF.
  IF SY-TCODE NE 'ZIM57'. EXIT.   ENDIF.
  READ TABLE IT_ZSIVIT INDEX TC_5710-CURRENT_LINE.

  MOVE-CORRESPONDING ZSIVIT TO IT_ZSIVIT.
  MOVE W_ROW_MARK TO IT_ZSIVIT-ZFMARK.

  MODIFY IT_ZSIVIT   INDEX TC_5710-CURRENT_LINE.

ENDMODULE.                             " TC_5710_UPDATE_SCR5710  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFPRTE_CHECK_SCR5710  INPUT
*&---------------------------------------------------------------------*
MODULE ZFPRTE_CHECK_SCR5710 INPUT.

  IF SY-TCODE = 'ZIM57'.
    IF ZTIV-ZFIVAMC NE ZTIV-ZFKRW AND ZTIV-ZFEXRT IS INITIAL.
      MESSAGE E743.
    ENDIF.

*    IF ZTIV-ZFPRTE > 100.
*      MESSAGE E742.
*    ENDIF.
  ENDIF.

ENDMODULE.                             " ZFPRTE_CHECK_SCR5710  INPUT
*&---------------------------------------------------------------------*
*&      Module  IDRNO_CHECK_FOR_SCR6210  INPUT
*&---------------------------------------------------------------------*
MODULE IDRNO_CHECK_FOR_SCR6210 INPUT.

*  PERFORM OK_CODE_IMDR.
  PERFORM OK_CODE_BACK_EXIT.
  PERFORM  P2000_OK_CODE_PROCESS.

  PERFORM READ_ZTIDR_FOR_SCR6210.

ENDMODULE.                             " IDRNO_CHECK_FOR_SCR6210  INPUT
*&---------------------------------------------------------------------*
*&      Form  OK_CODE_IDMR
*&---------------------------------------------------------------------*
FORM OK_CODE_IMDR.

  IF OK-CODE EQ 'CHDC'.
    LEAVE TO TRANSACTION 'ZIM62'.
  ENDIF.
  IF OK-CODE EQ 'DISP'.
    LEAVE TO TRANSACTION 'ZIM63'.
  ENDIF.
  IF OK-CODE EQ 'DELE'.
    PERFORM P3000_ZTIDR_DELETE_SCR6210.
  ENDIF.
  IF OK-CODE EQ 'DSBL'.
    SET PARAMETER ID 'ZPBLNO'  FIELD ZTIDR-ZFBLNO.
    EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
    CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
  ENDIF.

ENDFORM.                               " OK_CODE_IMDR
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDR_FOR_SCR6210
*&---------------------------------------------------------------------*
FORM READ_ZTIDR_FOR_SCR6210.

* IMPORT SYSTEM IMG READ
  SELECT SINGLE * FROM ZTIMIMG00.

  IF NOT ZTIDR-ZFREBELN IS INITIAL.
     SELECT COUNT( DISTINCT ZFBLNO ) INTO  W_COUNT
     FROM   ZTBL
     WHERE  ZFREBELN  EQ  ZTIDR-ZFREBELN.
     IF W_COUNT GT 1.
        MESSAGE  E751  WITH  ZTIDR-ZFREBELN.
     ENDIF.
     IF W_COUNT EQ 1.
        SELECT SINGLE * FROM ZTBL
        WHERE  ZFREBELN EQ   ZTIDR-ZFREBELN.
        IF SY-SUBRC EQ 0.
           MOVE  ZTBL-ZFBLNO  TO  ZTIDR-ZFBLNO.
        ENDIF.
     ENDIF.
  ENDIF.

  IF ( ZTIDR-ZFBLNO IS INITIAL ) AND NOT ( ZTIDR-ZFHBLNO IS INITIAL ).
    SELECT COUNT( DISTINCT ZFBLNO ) INTO W_COUNT
    FROM   ZTIDR
    WHERE  ZFHBLNO = ZTIDR-ZFHBLNO.
    IF W_COUNT > 1.
      MESSAGE E751 WITH ZTIDR-ZFHBLNO.
    ENDIF.
    IF W_COUNT = 1.
      SELECT SINGLE * FROM ZTIDR
      WHERE ZFHBLNO = ZTIDR-ZFHBLNO.
    ENDIF.
  ENDIF.

  IF ( ZTIDR-ZFBLNO IS INITIAL ) AND ( ZTIDR-ZFHBLNO IS INITIAL ) AND
                                 NOT ( ZTIDR-ZFIDRNO IS INITIAL ).
    SELECT SINGLE * FROM ZTIDR
    WHERE  ZFIDRNO = ZTIDR-ZFIDRNO.
  ENDIF.

  IF ( ZTIDR-ZFBLNO IS INITIAL )  AND ( ZTIDR-ZFHBLNO IS INITIAL ) AND
     ( ZTIDR-ZFIDRNO IS INITIAL ) AND ( ZTIDR-ZFREBELN IS INITIAL ).
    MESSAGE E752.
  ENDIF.

  IF ZTIDR-ZFCLSEQ IS INITIAL.
    SELECT MAX( ZFCLSEQ ) INTO ZTIDR-ZFCLSEQ
           FROM ZTIDR
           WHERE ZFBLNO = ZTIDR-ZFBLNO.
  ENDIF.

  SELECT SINGLE * FROM ZTIDR
  WHERE  ZFBLNO  = ZTIDR-ZFBLNO
  AND    ZFCLSEQ = ZTIDR-ZFCLSEQ.
   *ZTIDR  =  ZTIDR.

  IF SY-SUBRC NE 0.
    IF SY-TCODE NE 'ZIM62'.
      MESSAGE E038 WITH ZTIDR-ZFBLNO.
    ELSE.
      IF ZTIDR-ZFBLNO IS INITIAL AND NOT ZTIDR-ZFHBLNO IS INITIAL.
        SELECT MAX( ZFBLNO ) INTO ZTIDR-ZFBLNO
               FROM ZTBL
               WHERE ZFHBLNO EQ ZTIDR-ZFHBLNO.
        IF ZTIDR-ZFBLNO IS INITIAL.
          MESSAGE E305 WITH ZTIDR-ZFHBLNO.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ZTIMIMG00-ZFIMPATH EQ '2'.
      SELECT SINGLE * FROM ZTBL
             WHERE ZFBLNO EQ ZTIDR-ZFBLNO.
      IF SY-SUBRC EQ 0.
        IF ZTBL-ZFRPTTY IS INITIAL.
          IF SY-CALLD EQ 'X'.
            MESSAGE S939 WITH  ZTBL-ZFHBLNO.
            LEAVE TO SCREEN 0.
            EXIT.
          ELSE.
            MESSAGE E939 WITH  ZTBL-ZFHBLNO.
          ENDIF.
        ENDIF.
*>> 한수원 특이 사항. (통관요청 접수여부 체크)
        IF ZTBL-ZFCCRST EQ 'R'.
          IF SY-CALLD EQ 'X'.
            MESSAGE S977
              WITH
              'Clearance Doc is not accepted.(impossible declaration)'.
            LEAVE TO SCREEN 0.
            EXIT.
          ELSE.
            MESSAGE E977
              WITH
              'Clearance Doc is not accepted.(impossible declaration)'.
          ENDIF.
        ENDIF.
*============================================>
        IF ZTIMIMG00-BLSTYN EQ 'X'.
          IF ZTBL-ZFBLST EQ '4'.
            IF SY-CALLD EQ 'X'.
              MESSAGE S938 WITH ZTBL-ZFHBLNO.
              LEAVE TO SCREEN 0.
              EXIT.
            ELSE.
              MESSAGE E938 WITH ZTBL-ZFHBLNO.
            ENDIF.
          ELSE.
            CALL FUNCTION 'ZIM_BDC_CALL_TRANSACTION_ZIM31'
                 EXPORTING
                      W_ZFBLNO  = ZTBL-ZFBLNO
                 TABLES
                      RETURN    = RETURN
                 EXCEPTIONS
                      REQ_ERROR = 4.

            IF SY-SUBRC EQ 0.
              COMMIT WORK.
              SELECT MAX( ZFCLSEQ ) INTO ZTIDR-ZFCLSEQ
                     FROM ZTIDR
                     WHERE ZFBLNO = ZTIDR-ZFBLNO.
              SELECT SINGLE * FROM ZTIDR
                     WHERE  ZFBLNO  = ZTIDR-ZFBLNO
                     AND    ZFCLSEQ = ZTIDR-ZFCLSEQ.
               *ZTIDR  =  ZTIDR.
            ELSE.
              ROLLBACK WORK.
              READ TABLE RETURN WITH KEY TYPE = 'E'.
              IF SY-CALLD EQ 'X'.
                RETURN-TYPE = 'S'.
              ELSE.
                RETURN-TYPE = 'E'.
              ENDIF.
              MESSAGE ID     RETURN-ID
                      TYPE   RETURN-TYPE
                      NUMBER RETURN-NUMBER
                      WITH   RETURN-MESSAGE_V1
                             RETURN-MESSAGE_V2
                             RETURN-MESSAGE_V3
                             RETURN-MESSAGE_V4.
              IF SY-CALLD EQ 'X'.
                LEAVE TO SCREEN 0.
              ELSE.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          CALL FUNCTION 'ZIM_BDC_CALL_TRANSACTION_ZIM31'
               EXPORTING
                    W_ZFBLNO  = ZTBL-ZFBLNO
               TABLES
                    RETURN    = RETURN
               EXCEPTIONS
                    REQ_ERROR = 4.

          IF SY-SUBRC EQ 0.
            COMMIT WORK.
            SELECT MAX( ZFCLSEQ ) INTO ZTIDR-ZFCLSEQ
                   FROM ZTIDR
                   WHERE ZFBLNO = ZTIDR-ZFBLNO.
            SELECT SINGLE * FROM ZTIDR
                   WHERE  ZFBLNO  = ZTIDR-ZFBLNO
                   AND    ZFCLSEQ = ZTIDR-ZFCLSEQ.
             *ZTIDR  =  ZTIDR.
          ELSE.
            ROLLBACK WORK.
            READ TABLE RETURN WITH KEY TYPE = 'E'.
            IF SY-CALLD EQ 'X'.
              RETURN-TYPE = 'S'.
            ELSE.
              RETURN-TYPE = 'E'.
            ENDIF.
            MESSAGE ID     RETURN-ID
                    TYPE   RETURN-TYPE
                    NUMBER RETURN-NUMBER
                    WITH   RETURN-MESSAGE_V1
                           RETURN-MESSAGE_V2
                           RETURN-MESSAGE_V3
                           RETURN-MESSAGE_V4.
            IF SY-CALLD EQ 'X'.
              LEAVE TO SCREEN 0.
            ELSE.
              EXIT.
            ENDIF.
          ENDIF.

        ENDIF.
      ELSE.
        MESSAGE E038 WITH ZTIDR-ZFBLNO.
      ENDIF.
    ELSE.
      MESSAGE E753.
    ENDIF.
  ENDIF.

  IF SY-TCODE = 'ZIM62'.
*--> 2002.08.30 MODIFIED BY JSY.
     CLEAR ZTIV.
     SELECT SINGLE * FROM ZTIV
              WHERE ZFIVNO = ZTIDR-ZFIVNO.
*    CLEAR ZTCUCL.
*    SELECT SINGLE *  FROM  ZTCUCL
*    WHERE  ZFBLNO  = ZTIDR-ZFBLNO
*    AND    ZFCLSEQ = ZTIDR-ZFCLSEQ.
*    IF ZTCUCL-ZFCUST EQ 'Y'. MESSAGE E477. ENDIF.
*    IF ZTCUCL-ZFCUST EQ '3'.
    IF ZTIV-ZFCUST EQ 'Y'. MESSAGE E477. ENDIF.
    IF ZTIV-ZFCUST EQ '3'.
      IF ZTIDR-ZFDOCST EQ 'R'.
        MESSAGE E105 WITH ZTIDR-ZFBLNO ZTIDR-ZFCLSEQ ZTIDR-ZFEDIST.
      ELSEIF ZTIDR-ZFDOCST EQ 'N'.
        MESSAGE W754.
      ENDIF.
    ENDIF.

*----> 2001.11.19 KSB INSERT.
*>> 보세운송일 경우, 반입데이타 갱신 작업.
    SELECT SINGLE * FROM ZTBL
           WHERE ZFBLNO EQ ZTIDR-ZFBLNO.

    IF ( ZTBL-ZFRPTTY EQ 'B' OR ZTBL-ZFRPTTY EQ 'N' ) AND
         ZTIMIMG00-ZFINOU NE 'X'.
      SELECT * FROM ZTBLINR_TMP UP TO 1 ROWS
               WHERE   ZFBLNO EQ ZTBL-ZFBLNO.
      ENDSELECT.

      IF SY-SUBRC EQ 0.
        CONCATENATE  ZTBLINR_TMP-ZFGMNO ZTBLINR_TMP-ZFMSN
                     ZTBLINR_TMP-ZFHSN
                     INTO  ZTIDR-ZFGOMNO.
        MOVE  ZTBLINR_TMP-ZFPKCNM  TO  ZTIDR-ZFPKNM.  ">포장종류.
        MOVE  ZTBLINR_TMP-ZFTOWTM  TO  ZTIDR-ZFTOWTM. ">총중량단위.
        MOVE  ZTBLINR_TMP-ZFTOWT   TO  ZTIDR-ZFTOWT.  ">총중량.
        MOVE  ZTBLINR_TMP-ZFCARNM  TO  ZTIDR-ZFCARNM. ">선기명.
        MOVE  ZTBLINR_TMP-ZFINDT   TO  ZTIDR-ZFINDT.  ">반입일자.
        MOVE  ZTBLINR_TMP-ZFINRNO  TO  ZTIDR-ZFINRNO. ">반입신고번호.
        MOVE  ZTBLINR_TMP-ZFBNARCD TO  ZTIDR-ZFBNARCD.">보세구역.
        MOVE  ZTBLINR_TMP-ZFBTRNO  TO  ZTIDR-ZFBTRNO. ">보운신고번호.

      ELSEIF ZTIMIMG00-ZFINOU EQ 'X'.
        SELECT * FROM  ZTBLINR UP TO 1 ROWS
                 WHERE ZFBLNO  EQ ZTBL-ZFBLNO.
        ENDSELECT.
        IF SY-SUBRC EQ 0.
           SELECT SINGLE * FROM  ZTBLINOU
                  WHERE  ZFBLNO  EQ ZTBLINR-ZFBLNO
                  AND    ZFBTSEQ EQ ZTBLINR-ZFBTSEQ.

           CONCATENATE  ZTBLINOU-ZFGMNO ZTBLINOU-ZFMSN ZTBLINOU-ZFHSN
                  INTO  ZTIDR-ZFGOMNO.

           MOVE  ZTBLINR-ZFPKCNM   TO  ZTIDR-ZFPKNM.  ">포장종류.
           MOVE  ZTBLINR-ZFKG      TO  ZTIDR-ZFTOWTM. ">총중량단위.
           MOVE  ZTBLINR-ZFINWT    TO  ZTIDR-ZFTOWT.  ">총중량.
           MOVE  ZTBL-ZFCARNM      TO  ZTIDR-ZFCARNM. ">선기명.
           MOVE  ZTBLINR-ZFINDT    TO  ZTIDR-ZFINDT.  ">반입일자.
           MOVE  ZTBLINR-ZFINRNO   TO  ZTIDR-ZFINRNO. ">반입신고번호.
           MOVE  ZTBLINR-ZFBNARCD  TO  ZTIDR-ZFBNARCD.">보세구역.
           MOVE  ZTBLINOU-ZFBTRNO  TO  ZTIDR-ZFBTRNO. ">보운신고번호.
         ELSE.
           CONCATENATE  ZTBL-ZFGMNO ZTBL-ZFMSN ZTBL-ZFHSN
                  INTO  ZTIDR-ZFGOMNO.
           MOVE  ZTBL-ZFPKCNM       TO  ZTIDR-ZFPKNM.    "포장종류.
           MOVE  ZTBL-ZFNEWTM       TO  ZTIDR-ZFTOWTM.   "총중량단위.
           MOVE  ZTBL-ZFNEWT        TO  ZTIDR-ZFTOWT.
           MOVE  ZTBL-ZFCARNM       TO  ZTIDR-ZFCARNM.   "선기명.
         ENDIF.
      ENDIF.
    ELSE.
      CONCATENATE  ZTBL-ZFGMNO ZTBL-ZFMSN ZTBL-ZFHSN
                               INTO  ZTIDR-ZFGOMNO.
      MOVE  ZTBL-ZFPKCNM       TO  ZTIDR-ZFPKNM.        "포장종류.
      MOVE  ZTBL-ZFNEWTM       TO  ZTIDR-ZFTOWTM.       "총중량단위.
      MOVE  ZTBL-ZFNEWT        TO  ZTIDR-ZFTOWT.
      MOVE  ZTBL-ZFCARNM       TO  ZTIDR-ZFCARNM.       "선기명.
    ENDIF.
  ENDIF.

  PERFORM READ_IDRHS_TO_IT.
  PERFORM READ_IDRHSD_TO_IT.
  PERFORM READ_IDRHSL_TO_IT.

  IF SY-TCODE EQ 'ZIM62'.
    PERFORM  P2000_SET_ZTIDR_LOCK    USING   'L'.
  ENDIF.

ENDFORM.                               " READ_ZTIDR_FOR_SCR6210
*&---------------------------------------------------------------------*
*&      Form  READ_IDRHS_TO_IT
*&---------------------------------------------------------------------*
FORM READ_IDRHS_TO_IT.

  REFRESH IT_ZSIDRHS.
  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_ZSIDRHS
  FROM   ZTIDRHS
  WHERE  ZFBLNO   =  ZTIDR-ZFBLNO
  AND    ZFCLSEQ  =  ZTIDR-ZFCLSEQ.
  IT_ZSIDRHS_ORG[] = IT_ZSIDRHS[].

ENDFORM.                               " READ_IDRHS_TO_IT
*&---------------------------------------------------------------------*
*&      Form  READ_IDRHSD_TO_IT
*&---------------------------------------------------------------------*
FORM READ_IDRHSD_TO_IT.

  SELECT  *
  INTO    CORRESPONDING FIELDS OF TABLE IT_ZSIDRHSD
  FROM    ZTIDRHSD
  WHERE   ZFBLNO       EQ   ZTIDR-ZFBLNO
  AND     ZFCLSEQ      EQ   ZTIDR-ZFCLSEQ.
  IT_ZSIDRHSD_ORG[] = IT_ZSIDRHSD[].

ENDFORM.                               " READ_IDRHSD_TO_IT
*&---------------------------------------------------------------------*
*&      Form  READ_IDRHSL_TO_IT
*&---------------------------------------------------------------------*
FORM READ_IDRHSL_TO_IT.

  REFRESH IT_ZSIDRHSL.
  SELECT  *
  INTO    CORRESPONDING FIELDS OF TABLE IT_ZSIDRHSL
  FROM    ZTIDRHSL
  WHERE   ZFBLNO    =   ZTIDR-ZFBLNO
  AND     ZFCLSEQ   =   ZTIDR-ZFCLSEQ .
  IT_ZSIDRHSL_ORG[] = IT_ZSIDRHSL[].

ENDFORM.                               " READ_IDRHSL_TO_IT
*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR6210  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR6210 INPUT.

  PERFORM OK_CODE_IMDR.

ENDMODULE.                             " AFI_PROCESS_SCR6210  INPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIDR_DELETE_SCR6210
*&---------------------------------------------------------------------*
FORM P3000_ZTIDR_DELETE_SCR6210.

  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  IF ANTWORT NE 'Y'.
    EXIT.
  ENDIF.

*>> 통관비용.
*--> 2002.08.30 MODIFIED BY JSY.
*  SELECT SINGLE * FROM ZTCUCL
*         WHERE ZFBLNO = ZTIDR-ZFBLNO
*         AND ZFCLSEQ = ZTIDR-ZFCLSEQ.
   SELECT SINGLE * FROM ZTIV
           WHERE ZFIVNO = ZTIDR-ZFIVNO.
*  IF ZTCUCL-ZFCUST EQ '4'.
   IF ZTIV-ZFCUST EQ '4'.
    MESSAGE E755.
    EXIT.
  ENDIF.

  DELETE FROM ZTIDRHSL WHERE ZFBLNO  = ZTIDR-ZFBLNO           " 요?
                         AND ZFCLSEQ = ZTIDR-ZFCLSEQ.
  DELETE FROM ZTIDRHSD WHERE ZFBLNO  = ZTIDR-ZFBLNO           " 규격(행)
                         AND ZFCLSEQ = ZTIDR-ZFCLSEQ.
  DELETE FROM ZTIDRHS  WHERE ZFBLNO  = ZTIDR-ZFBLNO             " ?
                        AND ZFCLSEQ  = ZTIDR-ZFCLSEQ.
  DELETE FROM ZTIDR    WHERE ZFBLNO  = ZTIDR-ZFBLNO             " 신?
                         AND ZFCLSEQ = ZTIDR-ZFCLSEQ.
*-> 2002.08.30 MODIFIED BY JSY.
*  DELETE FROM ZTCUCL WHERE ZFBLNO  = ZTIDR-ZFBLNO             " 통?
*                       AND ZFCLSEQ = ZTIDR-ZFCLSEQ.
*  통관 Invoice
  UPDATE ZTIV SET   UNAM   = SY-UNAME
                    UDAT   = SY-DATUM
                    ZFCUST = '1'
               WHERE ZFIVNO EQ ZTIDR-ZFIVNO.
*              WHERE ZFIVNO IN
*                         ( SELECT ZFIVNO FROM ZTCUCLIV
*                                  WHERE ZFBLNO  EQ ZTIDR-ZFBLNO
*                                  AND  ZFCLSEQ  EQ ZTIDR-ZFCLSEQ ).

*  SELECT * FROM ZTCUCLIV
*           WHERE ZFBLNO EQ ZTIDR-ZFBLNO
*           AND ZFCLSEQ  EQ ZTIDR-ZFCLSEQ.
*
*    DELETE FROM ZTCUCLIVIT WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
*
*    SELECT SINGLE * FROM ZTIV
*           WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
*    MOVE SY-UNAME TO ZTIV-UNAM.
*    MOVE SY-DATUM TO ZTIV-UDAT.
*    MOVE '1'      TO ZTIV-ZFCUST.
*    UPDATE ZTIV.
*  ENDSELECT.
*  DELETE FROM ZTCUCLIVIT WHERE ZFIVNO IN
*                         ( SELECT ZFIVNO FROM ZTCUCLIV
*                                  WHERE ZFBLNO  EQ ZTIDR-ZFBLNO
*                                  AND  ZFCLSEQ  EQ ZTIDR-ZFCLSEQ ).
*
*  DELETE FROM ZTCUCLIV WHERE ZFBLNO  = ZTIDR-ZFBLNO
*                         AND ZFCLSEQ = ZTIDR-ZFCLSEQ.

  MESSAGE S756.

  SET SCREEN 6200.
  LEAVE SCREEN.

ENDFORM.                               " P3000_ZTIDR_DELETE_SCR6210
*&---------------------------------------------------------------------*
*&      Module  TC_6216_UPDATE_SCR6212  INPUT
*&---------------------------------------------------------------------*
MODULE TC_6212_UPDATE_SCR6212 INPUT.
  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  READ TABLE IT_ZSIDRHSD INDEX TC_6212-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.

  MOVE-CORRESPONDING ZSIDRHSD TO IT_ZSIDRHSD.
  IF OK-CODE = 'MKA1'.
    MOVE   'X'             TO W_ROW_MARK.
  ENDIF.
  IF OK-CODE = 'MKL1'.
    CLEAR  W_ROW_MARK.
  ENDIF.
  MOVE W_ROW_MARK TO IT_ZSIDRHSD-ZFMARK.

  IF IT_ZSIDRHSD-ZFGDDS2 IS INITIAL.
    SELECT SINGLE MAKTX INTO IT_ZSIDRHSD-ZFGDDS2       " Short Text
      FROM MAKT
     WHERE SPRAS = SY-LANGU
       AND MATNR = IT_ZSIDRHSD-ZFGDDS1.
    IF SY-SUBRC NE 0.
      MESSAGE E716 WITH IT_ZSIDRHSD-ZFGDDS1.
    ENDIF.
  ENDIF.
  IF IT_ZSIDRHSD-ZFQNTM IS INITIAL.    " 수량단?
    MESSAGE E719.
  ENDIF.
  IF IT_ZSIDRHSD-ZFCUR IS INITIAL.     " 통?
    MESSAGE E721.
  ENDIF.

  IF W_OLD_SUBRC = 0.
    MODIFY IT_ZSIDRHSD INDEX TC_6212-CURRENT_LINE.
  ELSE.
    APPEND IT_ZSIDRHSD.
  ENDIF.


ENDMODULE.                             " TC_6212_UPDATE_SCR6212  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6212  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6212 INPUT.

  CASE OK-CODE.
    WHEN 'REF1'.
      IT_ZSIDRHSD[]  =  IT_ZSIDRHSD_ORG[].
    WHEN 'MKA1'.
      LOOP AT IT_ZSIDRHSD.
        MOVE 'X'  TO  IT_ZSIDRHSD-ZFMARK.
        MODIFY IT_ZSIDRHSD.
      ENDLOOP.
    WHEN 'MKL1'.
      LOOP AT IT_ZSIDRHSD.
        CLEAR IT_ZSIDRHSD-ZFMARK.  MODIFY IT_ZSIDRHSD.
      ENDLOOP.
    WHEN 'DEL1'.
      LOOP AT IT_ZSIDRHSD WHERE  ZFMARK = 'X'.
        MESSAGE  W478.
        MOVE-CORRESPONDING IT_ZSIDRHSD  TO  IT_ZSIDRHSD_DEL.
        APPEND IT_ZSIDRHSD_DEL.
        DELETE IT_ZSIDRHSD.
      ENDLOOP.
    WHEN 'LCDP'.
      CLEAR  W_CHK.
      LOOP AT IT_ZSIDRHSD WHERE  ZFMARK = 'X'.
        W_CHK  =  W_CHK + 1.
      ENDLOOP.
      IF W_CHK = 0.
        MESSAGE  E951.
      ELSEIF W_CHK > 1.
        MESSAGE  E965.
      ENDIF.
      SELECT  SINGLE * FROM ZTIVIT
      WHERE   ZFIVNO  = IT_ZSIDRHSD-ZFIVNO
      AND     ZFIVDNO = IT_ZSIDRHSD-ZFIVDNO.
      IF ZTIVIT-ZFREQNO IS INITIAL.
        MESSAGE E009.
      ENDIF.
      PERFORM  P2000_LC_DOC_DISPLAY     USING  ZTIVIT-ZFREQNO
                                                ''.

  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR6212  INPUT
*&---------------------------------------------------------------------*
*&      Form  QTY_AMT_SCR6212
*&---------------------------------------------------------------------*
FORM QTY_AMT_SCR6212.

  IF IT_ZSIDRHSD-PEINH > 0.
    IT_ZSIDRHSD-ZFAMT = IT_ZSIDRHSD-ZFQNT * IT_ZSIDRHSD-NETPR
                                          / IT_ZSIDRHSD-PEINH. "금?
  ELSE.
    IT_ZSIDRHSD-ZFAMT = IT_ZSIDRHSD-ZFQNT * IT_ZSIDRHSD-NETPR.
  ENDIF.
  ADD IT_ZSIDRHSD-ZFAMT TO W_ZFAMT_S.

ENDFORM.                               " QTY_AMT_SCR6212
*&---------------------------------------------------------------------*
*&      Form  P2000_REF1_PROCESS_SCR6212
*&---------------------------------------------------------------------*
FORM P2000_REF1_PROCESS_SCR6212.

  PERFORM READ_IDRHSD_TO_IT.

ENDFORM.                               " P2000_REF1_PROCESS_SCR6212
*&---------------------------------------------------------------------*
*&      Form  OK_CODE_DRDC
*&---------------------------------------------------------------------*
FORM OK_CODE_DRDC.

  IF OK-CODE EQ 'CHDC'.
    LEAVE TO TRANSACTION 'ZIM64'.
  ENDIF.
  IF OK-CODE EQ 'DISP'.
    LEAVE TO TRANSACTION 'ZIM65'.
  ENDIF.
  IF OK-CODE EQ 'DELE'.
    IF W_STATUS = 'C'.
      MESSAGE E344.
    ENDIF.
    PERFORM P2000_SET_MESSAGE USING  OK-CODE.
    CASE ANTWORT.
      WHEN 'Y'.              " Yes...
        PERFORM  P3000_DELETE_SCR6410.
        CLEAR OK-CODE.
        PERFORM  P2000_SET_UNLOCK.
        PERFORM  P2000_SET_SCREEN_SCRCOM.
        LEAVE SCREEN.
      WHEN 'N'.              " No...
        MESSAGE  S343.
        CLEAR OK-CODE.
        PERFORM  P2000_SET_UNLOCK.
        PERFORM  P2000_SET_SCREEN_SCRCOM.
        LEAVE SCREEN.
      WHEN 'C'.              " Cancel
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
  IF OK-CODE EQ 'CREA'.
    LEAVE TO TRANSACTION 'ZIM77'."감면허가 생성 이채경.
  ENDIF.

ENDFORM.                               " OK_CODE_DRDC
*&---------------------------------------------------------------------*
*&      Module  IDRNO_CHECK_FOR_SCR6410  INPUT
*&---------------------------------------------------------------------*
MODULE IDRNO_CHECK_FOR_SCR6410 INPUT.

  PERFORM OK_CODE_DRDC.
  PERFORM OK_CODE_BACK_EXIT.
  CASE SY-DYNNR.
    WHEN '6400' OR '6500'.
      PERFORM READ_ZTIDRCR_FOR_SCR6410.
    WHEN '6450'.
      PERFORM READ_ZTIDRCR_CRE_FOR_SCR6410."감면허가 생성?
  ENDCASE.
ENDMODULE.                             " IDRNO_CHECK_FOR_SCR6410  INPUT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDRCR_CREATE_FOR_SCR6410
*&---------------------------------------------------------------------*
FORM READ_ZTIDRCR_CRE_FOR_SCR6410.
*>> 수입면허번호가 있을 경?
  IF NOT ( ZTCUCL-ZFIDRNO IS INITIAL ).
    SELECT SINGLE *
      FROM ZTCUCL
     WHERE ZFIDRNO = ZTCUCL-ZFIDRNO.
    IF SY-SUBRC NE 0.
      MESSAGE E758.
    ENDIF.
    SELECT SINGLE *
      FROM ZTBL
     WHERE ZFBLNO = ZTCUCL-ZFBLNO.
  ENDIF.
*>> HOUSE B/L 만 있을 경우.B/L 초?
  IF ( ZTCUCL-ZFBLNO IS INITIAL ) AND NOT ( ZTBL-ZFHBLNO IS INITIAL ).
    SELECT COUNT( DISTINCT ZFBLNO ) INTO W_COUNT
      FROM ZTBL
     WHERE ZFHBLNO = ZTBL-ZFHBLNO.
    IF W_COUNT > 1.
      MESSAGE E759 WITH ZTBL-ZFHBLNO.
    ENDIF.
    IF W_COUNT = 1.
      SELECT SINGLE *
        FROM ZTBL
       WHERE ZFHBLNO = ZTBL-ZFHBLNO.
      MOVE ZTBL-ZFBLNO TO ZTCUCL-ZFBLNO.
    ENDIF.
    IF W_COUNT = 0.
      MESSAGE E707 WITH ZTBL-ZFHBLNO.
    ENDIF.
  ENDIF.
*>>B/L NO 있고 HOUSE B/L 없을경우.
  IF NOT ( ZTCUCL-ZFBLNO IS INITIAL ) AND ( ZTBL-ZFHBLNO IS INITIAL ).
    SELECT SINGLE *
      FROM ZTBL
     WHERE ZFBLNO = ZTCUCL-ZFBLNO.
    IF SY-SUBRC NE 0.
      MESSAGE E759 WITH ZTCUCL-ZFBLNO.
    ENDIF.
  ENDIF.
*>>선택한사항이 하나도 없을 경?
  IF ( ZTCUCL-ZFBLNO IS INITIAL ) AND ( ZTBL-ZFHBLNO IS INITIAL )
                                  AND ( ZTCUCL-ZFIDRNO IS INITIAL ).
    MESSAGE E760.
  ENDIF.

*>>통관순번이 입력되지 않았을 때.
  IF ZTCUCL-ZFCLSEQ IS INITIAL.
    SELECT MAX( ZFCLSEQ ) INTO ZTCUCL-ZFCLSEQ
      FROM ZTIDSHS
     WHERE ZFBLNO = ZTCUCL-ZFBLNO.
    IF ZTCUCL-ZFCLSEQ IS INITIAL.
      MESSAGE E340."란번호가 없습니다.
    ENDIF.
    SELECT  * FROM ZTCUCL
      WHERE ZFBLNO =  ZTCUCL-ZFBLNO AND
            ZFCLSEQ = ZTCUCL-ZFCLSEQ.
    ENDSELECT.
  ENDIF.

*>>선택한 내용이 감면허가내역에 있는지 *
  SELECT COUNT( DISTINCT ZFBLNO ) INTO W_COUNT
    FROM ZTIDRCR
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.
  IF W_COUNT NE 0.
    MESSAGE E336 ." 이미 감면허가 내용에 있습니다.
  ENDIF.
*>>선택한 내용이 란번호가 있는지.
  SELECT COUNT( DISTINCT ZFCONO ) INTO W_COUNT
     FROM ZTIDSHS
     WHERE ZFBLNO EQ ZTCUCL-ZFBLNO
       AND ZFCLSEQ EQ ZTCUCL-ZFCLSEQ.
  IF W_COUNT EQ 0.
    MESSAGE E340."란번호가 없습니다.
  ENDIF.
*>>화면에 구성에 필요한 나머지 필드 가져오기위해.
  CLEAR : ZTIDS, ZTIMIMG02, W_ZFINRC_NM.
  SELECT SINGLE *
    FROM ZTIDS
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.
  SELECT SINGLE *
    FROM ZTIMIMG02
   WHERE ZFCOTM = ZTIDS-ZFINRC.
  SELECT SINGLE NAME1 INTO W_ZFINRC_NM
    FROM LFA1
   WHERE LIFNR = ZTIMIMG02-ZFVEN.
  PERFORM P1000_GET_IT_IDRCR.
  PERFORM P1000_GET_IT_IDRCRIT.

ENDFORM.   " READ_ZTIDRCR_CRE_FOR_SCR6410

*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDRCR_FOR_SCR6410
*&---------------------------------------------------------------------*
FORM READ_ZTIDRCR_FOR_SCR6410.

  IF NOT ( ZTCUCL-ZFIDRNO IS INITIAL ).
    SELECT SINGLE *
      FROM ZTCUCL
     WHERE ZFIDRNO = ZTCUCL-ZFIDRNO.
    IF SY-SUBRC NE 0.
      MESSAGE E758.
    ENDIF.
    SELECT SINGLE *
      FROM ZTBL
     WHERE ZFBLNO = ZTCUCL-ZFBLNO.
  ENDIF.

  IF ( ZTCUCL-ZFBLNO IS INITIAL ) AND NOT ( ZTBL-ZFHBLNO IS INITIAL ).
    SELECT COUNT( DISTINCT ZFBLNO ) INTO W_COUNT
      FROM ZTBL
     WHERE ZFHBLNO = ZTBL-ZFHBLNO.
    IF W_COUNT > 1.
      MESSAGE E759 WITH ZTBL-ZFHBLNO.
    ENDIF.
    IF W_COUNT = 1.
      SELECT SINGLE *
        FROM ZTBL
       WHERE ZFHBLNO = ZTBL-ZFHBLNO.
      MOVE ZTBL-ZFBLNO TO ZTCUCL-ZFBLNO.
    ENDIF.
    IF W_COUNT = 0.
      MESSAGE E707 WITH ZTBL-ZFHBLNO.
    ENDIF.
  ENDIF.
  IF NOT ( ZTCUCL-ZFBLNO IS INITIAL ) AND ( ZTBL-ZFHBLNO IS INITIAL ).
    SELECT SINGLE *
      FROM ZTBL
     WHERE ZFBLNO = ZTCUCL-ZFBLNO.
    IF SY-SUBRC NE 0.
      MESSAGE E759 WITH ZTCUCL-ZFBLNO.
    ENDIF.
  ENDIF.

  IF ( ZTCUCL-ZFBLNO IS INITIAL ) AND ( ZTBL-ZFHBLNO IS INITIAL )
                                  AND ( ZTCUCL-ZFIDRNO IS INITIAL ).
    MESSAGE E760.
  ENDIF.

  IF ZTCUCL-ZFCLSEQ IS INITIAL.
    SELECT MAX( ZFCLSEQ ) INTO ZTCUCL-ZFCLSEQ
      FROM ZTIDRCR
     WHERE ZFBLNO = ZTCUCL-ZFBLNO.
  ENDIF.

  SELECT COUNT( DISTINCT ZFBLNO ) INTO W_COUNT
    FROM ZTIDRCR
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.
  IF W_COUNT EQ 0.
    MESSAGE E761.
  ENDIF.

  IF ZTCUCL-ZFIDRNO IS INITIAL.
    SELECT SINGLE *
      FROM ZTCUCL
     WHERE ZFBLNO = ZTCUCL-ZFBLNO
       AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.
  ENDIF.

  CLEAR : ZTIDS, ZTIMIMG02, W_ZFINRC_NM.
  SELECT SINGLE *
    FROM ZTIDS
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.
  SELECT SINGLE *
    FROM ZTIMIMG02
   WHERE ZFCOTM = ZTIDS-ZFINRC.
  SELECT SINGLE NAME1 INTO W_ZFINRC_NM
    FROM LFA1
   WHERE LIFNR = ZTIMIMG02-ZFVEN.

  PERFORM READ_IDRCR_TO_IT.
  PERFORM READ_IDRCRIT_TO_IT.

  IF SY-TCODE EQ 'ZIM64'.
    PERFORM P2000_SET_ZTIDRCR_LOCK   USING   'L'.
  ENDIF.
ENDFORM.                               " READ_ZTIDRCR_FOR_SCR6410
*&---------------------------------------------------------------------*
*&      Module  TC_6216_UPDATE_SCR6216  INPUT
*&---------------------------------------------------------------------*
MODULE TC_6216_UPDATE_SCR6216 INPUT.
  READ TABLE IT_ZSIDRHS INDEX TC_6216-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.

  MOVE-CORRESPONDING ZSIDRHS TO IT_ZSIDRHS.
  MOVE W_ROW_MARK TO IT_ZSIDRHS-ZFMARK.

  IF W_OLD_SUBRC = 0.
    MODIFY IT_ZSIDRHS  INDEX TC_6216-CURRENT_LINE.
  ENDIF.

ENDMODULE.                             " TC_6216_UPDATE_SCR6216  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_6212  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_6212 INPUT.

  READ TABLE IT_ZSIDRHSD  WITH KEY ZSIDRHSD(24)  BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.

*    MOVE-CORRESPONDING   ZSIDRHSD   TO IT_ZSIDRHSD.
    MOVE : ZSIDRHSD-ZFSTCD  TO IT_ZSIDRHSD-ZFSTCD,
           ZSIDRHSD-ZFGDDS1 TO IT_ZSIDRHSD-ZFGDDS1,
           ZSIDRHSD-STAWN   TO IT_ZSIDRHSD-STAWN.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIDRHSD-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIDRHSD-ZFMARK.
    ENDIF.

    IF IT_ZSIDRHSD-ZFGDDS1 IS INITIAL.                      " 규격1
      SELECT SINGLE MAKTX INTO IT_ZSIDRHSD-ZFGDDS1
      FROM   MAKT
      WHERE  SPRAS = SY-LANGU
      AND    MATNR = IT_ZSIDRHSD-ZFSTCD.
      IF SY-SUBRC NE 0 OR IT_ZSIDRHSD-ZFGDDS1 IS INITIAL.
        MESSAGE S716 WITH IT_ZSIDRHSD-ZFSTCD.
        EXIT.
      ENDIF.
    ENDIF.
    IF IT_ZSIDRHSD-STAWN IS INITIAL.
      SELECT MAX( STAWN ) INTO IT_ZSIDRHSD-STAWN           " HS Code
      FROM   MARC
      WHERE  MATNR = IT_ZSIDRHSD-ZFSTCD.
      IF SY-SUBRC NE 0.
        MESSAGE S717 WITH IT_ZSIDRHSD-ZFSTCD.
      ENDIF.
    ENDIF.

*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHSD-ZFAMT
*                                            IT_ZSIDRHSD-ZFCUR.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHSD-NETPR
*                                            IT_ZSIDRHSD-ZFCUR.
    MODIFY IT_ZSIDRHSD INDEX W_TABIX.

  ENDIF.

ENDMODULE.                             " SET_SCR_MARK_TC_6212  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_6216  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_6216 INPUT.

  READ TABLE IT_ZSIDRHS  WITH KEY ZSIDRHS(21)  BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
*   MOVE-CORRESPONDING   ZSIDRHS   TO IT_ZSIDRHS.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIDRHS-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIDRHS-ZFMARK.
    ENDIF.

*   PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHS-ZFTXPER
*                                           IT_ZSIDRHS-ZFKRW.
*   PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHS-ZFHSAM
*                                           IT_ZSIDRHS-ZFKRW.
*   PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHS-ZFCUAMT
*                                           IT_ZSIDRHS-ZFKRW.
*   PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHS-ZFCCAMT
*                                           IT_ZSIDRHS-ZFKRW.
*   PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHS-ZFHMAMT
*                                           IT_ZSIDRHS-ZFKRW.
*   PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHS-ZFHCAMT
*                                           IT_ZSIDRHS-ZFKRW.
*   PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHS-ZFEDAMT
*                                           IT_ZSIDRHS-ZFKRW.
*   PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHS-ZFECAMT
*                                           IT_ZSIDRHS-ZFKRW.
*   PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHS-ZFAGAMT
*                                           IT_ZSIDRHS-ZFKRW.
*   PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHS-ZFVAAMT
*                                           IT_ZSIDRHS-ZFKRW.
*   PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHS-ZFVCAMT
*                                           IT_ZSIDRHS-ZFKRW.
*   PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHS-ZFTBAK
*                                           IT_ZSIDRHS-ZFKRW.
*   PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHS-ZFTBAU
*                                           IT_ZSIDRHS-ZFUSD.

    MODIFY IT_ZSIDRHS INDEX W_TABIX.

  ENDIF.

ENDMODULE.                             " SET_SCR_MARK_TC_6216  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_6218_UPDATE_SCR6218  INPUT
*&---------------------------------------------------------------------*
MODULE TC_6218_UPDATE_SCR6218 INPUT.

  MOVE : ZTIDRHS-ZFBLNO   TO   ZSIDRHSD-ZFBLNO,
         ZTIDRHS-ZFCLSEQ  TO   ZSIDRHSD-ZFCLSEQ,
         ZTIDRHS-ZFCONO   TO   ZSIDRHSD-ZFCONO,
         ZTIDRHS-STAWN    TO   ZSIDRHSD-STAWN,
         ZTIDR-ZFSTAMC    TO   ZSIDRHSD-ZFCUR.

  IF ZSIDRHSD-ZFRONO  IS INITIAL.
     SELECT MAX( ZFRONO )  INTO  W_RONO
     FROM   ZTIDRHSD
     WHERE  ZFBLNO         EQ    ZSIDRHSD-ZFBLNO
     AND    ZFCLSEQ        EQ    ZSIDRHSD-ZFCLSEQ
     AND    ZFCONO         EQ    ZSIDRHSD-ZFCONO.

     LOOP  AT  IT_ZSIDRHSD_S.
        IF W_RONO  LE  IT_ZSIDRHSD_S-ZFRONO.
           MOVE  IT_ZSIDRHSD_S-ZFRONO  TO  W_RONO.
        ENDIF.
     ENDLOOP.
     ZSIDRHSD-ZFRONO  =  W_RONO  +  10.
  ENDIF.

  READ TABLE IT_ZSIDRHSD_S  WITH KEY ZSIDRHSD(24)  BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
    MOVE : ZSIDRHSD-ZFBLNO  TO    IT_ZSIDRHSD_S-ZFBLNO,
           ZSIDRHSD-ZFCLSEQ TO    IT_ZSIDRHSD_S-ZFCLSEQ,
           ZSIDRHSD-ZFCONO  TO    IT_ZSIDRHSD_S-ZFCONO,
           ZSIDRHSD-ZFRONO  TO    IT_ZSIDRHSD_S-ZFRONO,
           ZSIDRHSD-ZFSTCD  TO    IT_ZSIDRHSD_S-ZFSTCD,
           ZSIDRHSD-ZFGDDS1 TO    IT_ZSIDRHSD_S-ZFGDDS1,
           ZSIDRHSD-ZFGDDS2 TO    IT_ZSIDRHSD_S-ZFGDDS2,
           ZSIDRHSD-ZFGDDS3 TO    IT_ZSIDRHSD_S-ZFGDDS3,
           ZSIDRHSD-ZFGDIN1 TO    IT_ZSIDRHSD_S-ZFGDIN1,
           ZSIDRHSD-ZFGDIN2 TO    IT_ZSIDRHSD_S-ZFGDIN2,
           ZSIDRHSD-ZFQNT   TO    IT_ZSIDRHSD_S-ZFQNT,
           ZSIDRHSD-ZFQNTM  TO    IT_ZSIDRHSD_S-ZFQNTM,
           ZSIDRHSD-NETPR   TO    IT_ZSIDRHSD_S-NETPR,
           ZSIDRHSD-STAWN   TO    IT_ZSIDRHSD_S-STAWN,
           ZSIDRHSD-ZFCUR   TO    IT_ZSIDRHSD_S-ZFCUR.

    IF IT_ZSIDRHSD_S-ZFGDDS1 IS INITIAL.                    " 규격2
      SELECT SINGLE MAKTX INTO IT_ZSIDRHSD_S-ZFGDDS1
        FROM MAKT
       WHERE SPRAS = SY-LANGU
         AND MATNR = IT_ZSIDRHSD_S-ZFSTCD.
      IF SY-SUBRC NE 0 OR IT_ZSIDRHSD_S-ZFGDDS1 IS INITIAL.
        MESSAGE S716 WITH IT_ZSIDRHSD_S-ZFSTCD.
        EXIT.
      ENDIF.
    ENDIF.
    IF IT_ZSIDRHSD_S-STAWN IS INITIAL.
      SELECT MAX( STAWN ) INTO IT_ZSIDRHSD_S-STAWN        " HS Code
        FROM MARC
       WHERE MATNR = IT_ZSIDRHSD_S-ZFGDDS1.
      IF SY-SUBRC NE 0 OR IT_ZSIDRHSD_S-STAWN IS INITIAL.
        MESSAGE S717 WITH IT_ZSIDRHSD_S-ZFGDDS1.
      ENDIF.
    ENDIF.

    IT_ZSIDRHSD_S-ZFAMT  =  IT_ZSIDRHSD_S-ZFQNT  *  IT_ZSIDRHSD_S-NETPR.
    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRHSD_S-ZFAMT
                                            IT_ZSIDRHSD_S-ZFCUR.
    IF W_SY_SUBRC EQ 0.
       MODIFY IT_ZSIDRHSD_S INDEX W_TABIX.
    ELSE.
       APPEND  IT_ZSIDRHSD_S.
    ENDIF.

ENDMODULE.                             " TC_6218_UPDATE_SCR6218  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_6219_UPDATE_SCR6219  INPUT
*&---------------------------------------------------------------------*
MODULE TC_6219_UPDATE_SCR6219 INPUT.

  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  READ TABLE IT_ZSIDRHSL_S INDEX TC_6219-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.

  MOVE-CORRESPONDING ZSIDRHSL TO IT_ZSIDRHSL_S.
  MOVE W_ROW_MARK TO IT_ZSIDRHSL_S-ZFMARK.

  IF W_OLD_SUBRC = 0.
    MODIFY IT_ZSIDRHSL_S   INDEX TC_6219-CURRENT_LINE.
  ELSE.
    MOVE   IT_ZSIDRHS-ZFBLNO     TO IT_ZSIDRHSL_S-ZFBLNO.
    MOVE   IT_ZSIDRHS-ZFCLSEQ    TO IT_ZSIDRHSL_S-ZFCLSEQ.
    MOVE   IT_ZSIDRHS-ZFCONO     TO IT_ZSIDRHSL_S-ZFCONO.
    APPEND IT_ZSIDRHSL_S.
  ENDIF.

ENDMODULE.                             " TC_6219_UPDATE_SCR6219  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_REOG_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_REOG_DISPLAY.

  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.                          " Yes...
      PERFORM  P3000_REOG_PROC_SCRCOM.
      CLEAR OK-CODE.
      PERFORM  P2000_SET_UNLOCK.
      PERFORM  P2000_SET_SCREEN_SCRCOM.
      LEAVE SCREEN.
    WHEN 'N'.                          " No...
      MESSAGE  S957.
      CLEAR OK-CODE.
      PERFORM  P2000_SET_UNLOCK.
      PERFORM  P2000_SET_SCREEN_SCRCOM.
      LEAVE SCREEN.
    WHEN 'C'.                          " Cancel
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                               " P2000_REOG_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P3000_REOG_PROC_SCRCOM
*&---------------------------------------------------------------------*
FORM P3000_REOG_PROC_SCRCOM.

*  DELETE FROM ZTIDRHSL WHERE ZFBLNO = ZTIDR-ZFBLNO           " 요?
*                         AND ZFCLSEQ = ZTIDR-ZFCLSEQ.
  DELETE FROM ZTIDRHSD WHERE ZFBLNO = ZTIDR-ZFBLNO           " 규격(행)
                         AND ZFCLSEQ = ZTIDR-ZFCLSEQ.
  DELETE FROM ZTIDRHS  WHERE ZFBLNO = ZTIDR-ZFBLNO             " ?
                        AND ZFCLSEQ = ZTIDR-ZFCLSEQ.

  LOOP AT IT_ZSIDRHSD.
    CLEAR ZTIDRHS.
    SELECT SINGLE *
      FROM ZTIDRHS
     WHERE ZFBLNO = IT_ZSIDRHSD-ZFBLNO
       AND ZFCLSEQ = IT_ZSIDRHSD-ZFCLSEQ
       AND STAWN = IT_ZSIDRHSD-STAWN.

    IF SY-SUBRC NE 0.
      SELECT MAX( ZFCONO ) INTO ZTIDRHS-ZFCONO
        FROM ZTIDRHS
       WHERE ZFBLNO = IT_ZSIDRHSD-ZFBLNO
         AND ZFCLSEQ = IT_ZSIDRHSD-ZFCLSEQ.
      MOVE IT_ZSIDRHSD-ZFBLNO      TO ZTIDRHS-ZFBLNO.
      MOVE IT_ZSIDRHSD-ZFCLSEQ     TO ZTIDRHS-ZFCLSEQ.
      ADD  1                       TO ZTIDRHS-ZFCONO.      "?
      MOVE IT_ZSIDRHSD-STAWN       TO ZTIDRHS-STAWN.       "HS Code
      MOVE 'KRW'                   TO ZTIDRHS-ZFKRW.
      INSERT ZTIDRHS.
      IF SY-SUBRC NE 0.
        MESSAGE E785 WITH IT_ZSIDRHSD-ZFBLNO.
        EXIT.
      ENDIF.
    ENDIF.

    CLEAR ZTIDRHSD.
    MOVE-CORRESPONDING IT_ZSIDRHSD TO ZTIDRHSD.
    SELECT MAX( ZFRONO ) INTO ZTIDRHSD-ZFRONO
      FROM ZTIDRHSD
     WHERE ZFBLNO = ZTIDRHS-ZFBLNO
       AND ZFCLSEQ = ZTIDRHS-ZFCLSEQ
       AND ZFCONO = ZTIDRHS-ZFCONO.
    MOVE ZTIDRHS-ZFBLNO      TO ZTIDRHSD-ZFBLNO.
    MOVE ZTIDRHS-ZFCLSEQ     TO ZTIDRHSD-ZFCLSEQ.
    MOVE ZTIDRHS-ZFCONO      TO ZTIDRHSD-ZFCONO.     "?
    ADD  1                   TO ZTIDRHSD-ZFRONO.     "?
    INSERT ZTIDRHSD.
    IF SY-SUBRC NE 0.
      MESSAGE E785 WITH IT_ZSIDRHSD-ZFBLNO.
      EXIT.
    ENDIF.
  ENDLOOP.

  MESSAGE  S767  WITH  ZTIDR-ZFBLNO.

ENDFORM.                               " P3000_REOG_PROC_SCRCOM
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTCUCLIV_DELETE_SCR6710
*&---------------------------------------------------------------------*
FORM P3000_ZTCUCLIV_DELETE_SCR6710.

  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  IF ANTWORT NE 'Y'.
    EXIT.
  ENDIF.

  DELETE FROM ZTCUCLIV WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
  DELETE FROM ZTCUCLIVIT WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.

  MESSAGE S756.

  SET SCREEN 6800.
  LEAVE SCREEN.

ENDFORM.                               " P3000_ZTCUCLIV_DELETE_SCR6710
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTBLUG_DELETE_SCR7110
*&---------------------------------------------------------------------*
FORM P3000_ZTBLUG_DELETE_SCR7110.

  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  IF ANTWORT NE 'Y'.
    EXIT.
  ENDIF.

  DELETE FROM ZTBLUG WHERE ZFHBLNO = ZTBLUG-ZFHBLNO.
  DELETE FROM ZTBLUGC WHERE ZFHBLNO = ZTBLUGC-ZFHBLNO.

  MESSAGE S756.

  SET SCREEN 7200.
  LEAVE SCREEN.

ENDFORM.                               " P3000_ZTBLUG_DELETE_SCR7110
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR7400  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR7400 INPUT.

  MOVE C_REQ_C TO W_STATUS.

  SET SCREEN 7410.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR7400  INPUT
*&---------------------------------------------------------------------*
*&      Module  IDRNO_CHECK_FOR_SCR7410  INPUT
*&---------------------------------------------------------------------*
MODULE IDRNO_CHECK_FOR_SCR7410 INPUT.

  PERFORM  P2000_OK_CODE_PROCESS.

  PERFORM READ_ZTIDR_FOR_SCR7410.

  SELECT SINGLE * FROM ZTIV
         WHERE  ZFIVNO EQ  ZTIDR-ZFIVNO.

ENDMODULE.                             " IDRNO_CHECK_FOR_SCR7410  INPUT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDR_FOR_SCR7410
*&---------------------------------------------------------------------*
FORM READ_ZTIDR_FOR_SCR7410.

  IF  ZTIDR-ZFBLNO  IS INITIAL
  AND ZTIDR-ZFHBLNO IS INITIAL
  AND ZTIDR-ZFIDRNO IS INITIAL.
    MESSAGE E866.
  ENDIF.
*>> HOUSE B/L.
  IF NOT ZTIDR-ZFHBLNO IS INITIAL.
    SELECT MAX( ZFBLNO ) INTO ZTIDR-ZFBLNO
      FROM   ZTIDR
      WHERE  ZFHBLNO EQ  ZTIDR-ZFHBLNO.
  ENDIF.
*>>> IMPORT IMG 사항 SELECT.
  SELECT SINGLE * FROM ZTIMIMG00.
*>> 수입신고번호 입력시.
  IF NOT ZTIDR-ZFIDRNO IS INITIAL.
    SELECT MAX( ZFBLNO ) INTO ZTIDR-ZFBLNO
      FROM   ZTIDR
      WHERE  ZFIDRNO EQ  ZTIDR-ZFIDRNO.
  ENDIF.
*>> 통관 순번 미입력시.
  IF  ZTIDR-ZFCLSEQ IS INITIAL.
    SELECT MAX( ZFCLSEQ ) INTO ZTIDR-ZFCLSEQ
       FROM ZTIDR
      WHERE ZFBLNO = ZTIDR-ZFBLNO.
  ENDIF.
*>> CHECK
  SELECT SINGLE *
         FROM  ZTIDR
         WHERE ZFBLNO  = ZTIDR-ZFBLNO
           AND ZFCLSEQ = ZTIDR-ZFCLSEQ.
  IF SY-SUBRC NE 0.
    MESSAGE E753.
  ENDIF.
  CLEAR ZTIDS.
  SELECT SINGLE *
         FROM ZTIDS
         WHERE  ZFBLNO  = ZTIDR-ZFBLNO
           AND  ZFCLSEQ = ZTIDR-ZFCLSEQ.
  IF SY-SUBRC EQ 0.
    MESSAGE E779 WITH ZTIDS-ZFIDRNO.
  ENDIF.

*  CLEAR ZTCUCL.
  CLEAR ZTIV.
  SELECT SINGLE *
  FROM   ZTIV
  WHERE  ZFIVNO  = ZTIDR-ZFIVNO.
*  AND    ZFCLSEQ = ZTIDR-ZFCLSEQ.
  IF ZTIV-ZFCUST EQ 'Y'. MESSAGE E777. ENDIF.
  IF ZTIV-ZFCUST NE '3'. MESSAGE E774. ENDIF.

*> HEADER DATA MOVE.
  MOVE-CORRESPONDING ZTIDR TO ZTIDS.

  PERFORM READ_ZTIDRHS_TO_IT.
  PERFORM READ_ZTIDRHSD_TO_IT.
  PERFORM READ_ZTIDRHSL_TO_IT.

*>관세/부가세 계산 로직 추가...
*  PERFORM P2000_CALC_COST_AMOUNT.

ENDFORM.                               " READ_ZTIDR_FOR_SCR7410
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDRHSD_FOR_SCR7410
*&---------------------------------------------------------------------*
FORM READ_ZTIDRHSD_FOR_SCR7410.

  REFRESH IT_ZSIDSHSD.
  SELECT *
    FROM ZTIDRHSD
   WHERE ZFBLNO = ZTIDR-ZFBLNO
     AND ZFCLSEQ = ZTIDR-ZFCLSEQ.
    CLEAR   IT_ZSIDSHSD.
    IF ( ZTCUCL-ZFCUST = '3' OR ZTCUCL-ZFCUST = 'Y' ) AND
       ( ZTCUCL-ZFCLCD = 'A' ).
      SELECT SINGLE *
        FROM ZTIV
       WHERE ZFIVNO = ZTIDRHSD-ZFIVNO.
*      SELECT SUM( MENGE ) INTO W_MENGE
*        FROM ZVIVHD_IT
*       WHERE ZFREQNO = ZTIV-ZFREQNO
*         AND ZFIVDNO = ZTIDRHSD-ZFIVDNO.
      ZTIDRHSD-ZFRMNT = ZTIDRHSD-ZFMENGE - W_MENGE.
    ELSE.
      ZTIDRHSD-ZFRMNT = 0.
    ENDIF.
    MOVE-CORRESPONDING ZTIDRHSD TO IT_ZSIDSHSD.
    APPEND IT_ZSIDSHSD.
  ENDSELECT.

ENDFORM.                               " READ_ZTIDRHSD_FOR_SCR7410
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR7410  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR7410 INPUT.

  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
*   WHEN 'REOG'.
*      PERFORM  P2000_REOG_SCR7410.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR7410  INPUT
*&---------------------------------------------------------------------*
*&      Module  IDSHSD_GET_LINE_SCR7412  INPUT
*&---------------------------------------------------------------------*
MODULE IDSHSD_GET_LINE_SCR7412 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_7412-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " IDSHSD_GET_LINE_SCR7412  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_7412  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_7412 INPUT.

*  READ TABLE IT_ZSIDSHSD  WITH KEY ZSIDSHSD(24)  BINARY SEARCH.
  READ TABLE IT_ZSIDSHSD  INDEX  TC_7412-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
    MOVE : ZSIDSHSD-ZFSTCD  TO IT_ZSIDSHSD-ZFSTCD,
           ZSIDSHSD-ZFGDDS1 TO IT_ZSIDSHSD-ZFGDDS1,
           ZSIDSHSD-ZFQNT   TO IT_ZSIDSHSD-ZFQNT,
           ZSIDSHSD-ZFQNTM  TO IT_ZSIDSHSD-ZFQNTM,
           ZSIDSHSD-ZFGDDS3 TO IT_ZSIDSHSD-ZFGDDS3,
           ZSIDSHSD-ZFGDIN1 TO IT_ZSIDSHSD-ZFGDIN1,
           ZSIDSHSD-ZFGDIN2 TO IT_ZSIDSHSD-ZFGDIN2.

*    IF NOT ( W_ROW_MARK IS INITIAL ).
*      IT_ZSIDSHSD-ZFMARK = 'X'.
*    ELSE.
*      CLEAR : IT_ZSIDSHSD-ZFMARK.
*    ENDIF.
    IF IT_ZSIDSHSD-ZFGDDS1 IS INITIAL.                      " 규격1
      SELECT SINGLE MAKTX INTO IT_ZSIDSHSD-ZFGDDS1
        FROM MAKT
       WHERE SPRAS = SY-LANGU
         AND MATNR = IT_ZSIDSHSD-ZFSTCD.
      IF SY-SUBRC NE 0.
        MESSAGE S716 WITH IT_ZSIDSHSD-ZFSTCD.
        EXIT.
      ENDIF.
    ENDIF.
    IF IT_ZSIDSHSD-STAWN IS INITIAL.
      SELECT MAX( STAWN ) INTO IT_ZSIDSHSD-STAWN           " HS Code
        FROM MARC
       WHERE MATNR = IT_ZSIDSHSD-ZFSTCD.
      IF SY-SUBRC NE 0 OR IT_ZSIDSHSD-STAWN IS INITIAL.
        MESSAGE S717 WITH IT_ZSIDSHSD-ZFSTCD.
      ENDIF.
    ENDIF.

*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHSD-ZFAMT
*                                            IT_ZSIDSHSD-ZFCUR.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHSD-NETPR
*                                            IT_ZSIDSHSD-ZFCUR.

    MODIFY IT_ZSIDSHSD INDEX W_TABIX.

  ENDIF.

ENDMODULE.                             " SET_SCR_MARK_TC_7412  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR7412  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR7412 INPUT.

  CASE OK-CODE.
    WHEN 'REF1'.
      IT_ZSIDSHSD[] = IT_ZSIDSHSD_ORG[].
    WHEN 'MKA1'.
      LOOP AT IT_ZSIDSHSD.
        MOVE 'X'  TO  IT_ZSIDSHSD-ZFMARK.
        MODIFY IT_ZSIDSHSD.
      ENDLOOP.
    WHEN 'MKL1'.
      LOOP AT IT_ZSIDSHSD.
        CLEAR IT_ZSIDSHSD-ZFMARK.  MODIFY IT_ZSIDSHSD.
      ENDLOOP.
    WHEN 'DEL1'.
      LOOP AT IT_ZSIDSHSD WHERE  ZFMARK = 'X'.
        MESSAGE  W478.
        MOVE-CORRESPONDING IT_ZSIDRHSD  TO  IT_ZSIDSHSD_DEL.
        APPEND IT_ZSIDSHSD_DEL.
        DELETE IT_ZSIDSHSD.
      ENDLOOP.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR7412  INPUT
*&---------------------------------------------------------------------*
*&      Form  QTY_AMT_SCR7412
*&---------------------------------------------------------------------*
FORM QTY_AMT_SCR7412.

  IF IT_ZSIDSHSD-PEINH > 0.
    IT_ZSIDSHSD-ZFAMT = IT_ZSIDSHSD-ZFQNT * IT_ZSIDSHSD-NETPR
                                          / IT_ZSIDSHSD-PEINH. "금?
  ELSE.
    IT_ZSIDSHSD-ZFAMT = IT_ZSIDSHSD-ZFQNT * IT_ZSIDSHSD-NETPR.
  ENDIF.

ENDFORM.                               " QTY_AMT_SCR7412
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC7416_SCR7416  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC7416_SCR7416 OUTPUT.

  W_LOOPLINES = SY-LOOPC.              " LOOPING COUNT
  READ TABLE IT_ZSIDSHS  INDEX TC_7416-CURRENT_LINE.
  IF SY-SUBRC = 0.                     " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIDSHS  TO ZSIDSHS.     " DATA MOVE
    MOVE: IT_ZSIDSHS-ZFMARK        TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                             " IT_TO_TC7416_SCR7416  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  REQIT_GET_LINE_SCR7416  INPUT
*&---------------------------------------------------------------------*
MODULE REQIT_GET_LINE_SCR7416 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_7416-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " REQIT_GET_LINE_SCR7416  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_7416  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_7416 INPUT.

  READ TABLE IT_ZSIDSHS  WITH KEY ZSIDSHS(21)  BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
    MOVE-CORRESPONDING   ZSIDSHS   TO IT_ZSIDSHS.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIDSHS-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIDSHS-ZFMARK.
    ENDIF.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHS-ZFTXPER
*                                            IT_ZSIDSHS-ZFKRW.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHS-ZFHSAM
*                                            IT_ZSIDSHS-ZFKRW.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHS-ZFCUAMT
*                                            IT_ZSIDSHS-ZFKRW.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHS-ZFCCAMT
*                                            IT_ZSIDSHS-ZFKRW.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHS-ZFHMAMT
*                                            IT_ZSIDSHS-ZFKRW.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHS-ZFHCAMT
*                                            IT_ZSIDSHS-ZFKRW.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHS-ZFEDAMT
*                                            IT_ZSIDSHS-ZFKRW.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHS-ZFECAMT
*                                            IT_ZSIDSHS-ZFKRW.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHS-ZFAGAMT
*                                            IT_ZSIDSHS-ZFKRW.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHS-ZFVAAMT
*                                            IT_ZSIDSHS-ZFKRW.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHS-ZFVCAMT
*                                            IT_ZSIDSHS-ZFKRW.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHS-ZFTBAK
*                                            IT_ZSIDSHS-ZFKRW.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHS-ZFTBAU
*                                            IT_ZSIDSHS-ZFUSD.

    MODIFY IT_ZSIDSHS INDEX W_TABIX.

  ENDIF.

ENDMODULE.                             " SET_SCR_MARK_TC_7416  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR7416  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR7416 INPUT.

  CASE OK-CODE.
    WHEN 'HS'.                         " 란사?
      W_COUNT = 0.
      LOOP AT IT_ZSIDSHS WHERE ZFMARK NE SPACE.
        W_COUNT = W_COUNT + 1.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.          MESSAGE  E962.
        WHEN 1.          PERFORM  P2000_SCR7417_HS_PROCESS.
        WHEN OTHERS.     MESSAGE  E965.
      ENDCASE.
    WHEN 'HSD'.                        " 규격명?
      W_COUNT = 0.
      LOOP AT IT_ZSIDSHS WHERE ZFMARK NE SPACE.
        W_COUNT = W_COUNT + 1.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.          MESSAGE  E962.
        WHEN 1.          PERFORM  P2000_SCR7418_HSD_PROCESS.
        WHEN OTHERS.     MESSAGE  E965.
      ENDCASE.
    WHEN 'HSL'.                        " 요건확?
      W_COUNT = 0.
      LOOP AT IT_ZSIDSHS WHERE ZFMARK NE SPACE.
        W_COUNT = W_COUNT + 1.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.          MESSAGE  E962.
        WHEN 1.          PERFORM  P2000_SCR7419_HSL_PROCESS.
        WHEN OTHERS.     MESSAGE  E965.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR7416  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR7417_HS_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SCR7417_HS_PROCESS.

  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '란사항' INTO SPOP-TITEL
                                        SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  MOVE-CORRESPONDING    IT_ZSIDSHS     TO   ZTIDSHS.

  CALL SCREEN 7417 STARTING AT 15 1
                   ENDING   AT 105 25.

  IF ANTWORT EQ 'Y'.
    READ TABLE IT_ZSIDSHS WITH KEY ZFBLNO = IT_ZSIDSHS-ZFBLNO
                                   ZFCLSEQ = IT_ZSIDSHS-ZFCLSEQ
                                   ZFCONO = IT_ZSIDSHS-ZFCONO.
    MOVE-CORRESPONDING    ZTIDSHS     TO   IT_ZSIDSHS.
    MODIFY IT_ZSIDSHS INDEX SY-TABIX.
  ENDIF.

ENDFORM.                               " P2000_SCR7417_HS_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR7418_HSD_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SCR7418_HSD_PROCESS.

  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '행사항' INTO SPOP-TITEL
                                       SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  REFRESH IT_ZSIDSHSD_S.
  CLEAR   IT_ZSIDSHSD.
  LOOP AT IT_ZSIDSHSD.
    IF IT_ZSIDSHSD-ZFCONO NE IT_ZSIDSHS-ZFCONO.
      CONTINUE.
    ENDIF.
    CLEAR   IT_ZSIDSHSD_S.
    MOVE-CORRESPONDING IT_ZSIDSHSD TO IT_ZSIDSHSD_S.
    APPEND IT_ZSIDSHSD_S.
  ENDLOOP.

  CALL SCREEN 7418 STARTING AT 15 1
                   ENDING   AT 95 15.

  IF ANTWORT EQ 'Y'.
    LOOP AT IT_ZSIDSHSD_S.
      READ TABLE IT_ZSIDSHSD WITH KEY ZFBLNO = IT_ZSIDSHSD_S-ZFBLNO
                                      ZFCLSEQ = IT_ZSIDSHSD_S-ZFCLSEQ
                                      ZFCONO = IT_ZSIDSHSD_S-ZFCONO
                                      ZFRONO = IT_ZSIDSHSD_S-ZFRONO.
      MOVE-CORRESPONDING    IT_ZSIDSHSD_S    TO   IT_ZSIDSHSD.
      MODIFY IT_ZSIDSHSD    INDEX SY-TABIX.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " P2000_SCR7418_HSD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR7419_HSL_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SCR7419_HSL_PROCESS.

  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '요건사항' INTO SPOP-TITEL
                                        SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  REFRESH IT_ZSIDSHSL_S.
  CLEAR   IT_ZSIDSHSL.
  LOOP AT IT_ZSIDSHSL.
    IF IT_ZSIDSHSL-ZFCONO NE IT_ZSIDSHS-ZFCONO.
      CONTINUE.
    ENDIF.
    CLEAR   IT_ZSIDSHSL_S.
    MOVE-CORRESPONDING IT_ZSIDSHSL TO IT_ZSIDSHSL_S.
    APPEND IT_ZSIDSHSL_S.
  ENDLOOP.

  CALL SCREEN 7419 STARTING AT 15 1
                   ENDING   AT 95 15.

  IF ANTWORT EQ 'Y'.
    DELETE IT_ZSIDSHSL WHERE ZFCONO EQ IT_ZSIDSHS-ZFCONO.
    LOOP AT IT_ZSIDSHSL_S.
      MOVE-CORRESPONDING    IT_ZSIDSHSL_S    TO   IT_ZSIDSHSL.
      APPEND IT_ZSIDSHSL.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " P2000_SCR7419_HSL_PROCESS
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR7417  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR7417 INPUT.

  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.
      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.
      ANTWORT = 'N'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'HSD'.                        " 규격명?
      PERFORM  P2000_SCR7418_HSD_PROCESS.
    WHEN 'HSL'.                        " 요건확?
      PERFORM  P2000_SCR7419_HSL_PROCESS.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " GET_OK_CODE_SCR7417  INPUT
*&---------------------------------------------------------------------*
*&      Module  REQIT_GET_LINE_SCR7418  INPUT
*&---------------------------------------------------------------------*
MODULE REQIT_GET_LINE_SCR7418 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_7418-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " REQIT_GET_LINE_SCR7418  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_7418_UPDATE_SCR7418  INPUT
*&---------------------------------------------------------------------*
MODULE TC_7418_UPDATE_SCR7418 INPUT.

  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  READ TABLE IT_ZSIDSHSD_S INDEX TC_7418-CURRENT_LINE.

  MOVE-CORRESPONDING ZSIDSHSD TO IT_ZSIDSHSD_S.
  MOVE W_ROW_MARK TO IT_ZSIDSHSD_S-ZFMARK.

*  PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHSD_S-ZFAMT
*                                          IT_ZSIDSHSD_S-ZFCUR.
*  PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHSD_S-NETPR
*                                          IT_ZSIDSHSD_S-ZFCUR.
  MODIFY IT_ZSIDSHSD_S   INDEX TC_7418-CURRENT_LINE.

ENDMODULE.                             " TC_7418_UPDATE_SCR7418  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR7418  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR7418 INPUT.

  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.
      PERFORM  P2000_ITEM_DATA_CHECK.
      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.
      ANTWORT = 'N'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " GET_OK_CODE_SCR7418  INPUT
*&---------------------------------------------------------------------*
*&      Module  REQIT_GET_LINE_SCR7419  INPUT
*&---------------------------------------------------------------------*
MODULE REQIT_GET_LINE_SCR7419 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_7419-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " REQIT_GET_LINE_SCR7419  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_7419_UPDATE_SCR7419  INPUT
*&---------------------------------------------------------------------*
MODULE TC_7419_UPDATE_SCR7419 INPUT.

  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  READ TABLE IT_ZSIDSHSL_S INDEX TC_7419-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.

  MOVE-CORRESPONDING ZSIDSHSL TO IT_ZSIDSHSL_S.
  MOVE W_ROW_MARK TO IT_ZSIDSHSL_S-ZFMARK.

  IF W_OLD_SUBRC = 0.
    MODIFY IT_ZSIDSHSL_S   INDEX TC_7419-CURRENT_LINE.
  ELSE.
    MOVE   IT_ZSIDSHS-ZFBLNO     TO IT_ZSIDSHSL_S-ZFBLNO.
    MOVE   IT_ZSIDSHS-ZFCLSEQ    TO IT_ZSIDSHSL_S-ZFCLSEQ.
    MOVE   IT_ZSIDSHS-ZFCONO     TO IT_ZSIDSHSL_S-ZFCONO.
    APPEND IT_ZSIDSHSL_S.
  ENDIF.

ENDMODULE.                             " TC_7419_UPDATE_SCR7419  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR7419  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR7419 INPUT.

  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.
      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.
      ANTWORT = 'N'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " GET_OK_CODE_SCR7419  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR7500  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR7500 INPUT.

  MOVE C_REQ_U TO W_STATUS.

  SET SCREEN 7410.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR7500  INPUT
*&---------------------------------------------------------------------*
*&      Module  IDRNO_CHECK_SCR7500_SCR7600  INPUT
*&---------------------------------------------------------------------*
MODULE IDRNO_CHECK_SCR7500_SCR7600 INPUT.

*  PERFORM OK_CODE_IMDS.
  PERFORM OK_CODE_BACK_EXIT.
  PERFORM  P2000_OK_CODE_PROCESS.

  PERFORM READ_ZTIDS_FOR_SCR7500_SCR7600.
*--> 2002.08.30 MODIFIED BY JSY.
  SELECT SINGLE * FROM ZTIV
          WHERE ZFIVNO EQ ZTIDS-ZFIVNO.
*         WHERE ZFIVNO EQ ( SELECT ZFIVNO FROM ZTCUCLIV
*                                  WHERE  ZFBLNO  EQ ZTIDS-ZFBLNO
*                                  AND    ZFCLSEQ EQ ZTIDS-ZFCLSEQ ).

  IF SY-SUBRC EQ 0.
*>> 비용DOCUMENT SELECT.
    W_ZFIMDNO = ZTIV-ZFIVNO.
    CALL FUNCTION 'ZIM_GET_COST_DOCUMENT'
         EXPORTING
              ZFCSTGRP    = '006'
              ZFIMDNO     = W_ZFIMDNO
         TABLES
              IT_ZSIMCOST = IT_ZSIMCOST.
  ENDIF.

ENDMODULE.                 " IDRNO_CHECK_SCR7500_SCR7600  INPUT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDS_FOR_SCR7500_SCR7600
*&---------------------------------------------------------------------*
FORM READ_ZTIDS_FOR_SCR7500_SCR7600.

  IF ( ZTIDS-ZFBLNO IS INITIAL ) AND ( ZTIDS-ZFHBLNO IS INITIAL )
                                 AND ( ZTIDS-ZFIDRNO IS INITIAL ).
    MESSAGE E866.
  ENDIF.

  IF ( ZTIDS-ZFBLNO IS INITIAL ) AND NOT ( ZTIDS-ZFHBLNO IS INITIAL ).
    SELECT COUNT( DISTINCT ZFBLNO ) INTO W_COUNT
    FROM   ZTIDS
    WHERE  ZFHBLNO = ZTIDS-ZFHBLNO.
    IF W_COUNT > 1.
      MESSAGE E867 WITH ZTIDS-ZFHBLNO.
    ENDIF.
    IF W_COUNT = 1.
      SELECT * FROM ZTIDS UP TO 1 ROWS
               WHERE  ZFHBLNO = ZTIDS-ZFHBLNO.
      ENDSELECT.
    ENDIF.
    IF W_COUNT EQ 0.
      SELECT MAX( ZFBLNO ) INTO ZTIDS-ZFBLNO
             FROM   ZTIDR
             WHERE  ZFHBLNO EQ  ZTIDS-ZFHBLNO.
      IF ZTIDS-ZFBLNO IS INITIAL.
        MESSAGE E867 WITH ZTIDS-ZFHBLNO.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ( ZTIDS-ZFBLNO IS INITIAL ) AND ( ZTIDS-ZFHBLNO IS INITIAL ) AND
                                 NOT ( ZTIDS-ZFIDRNO IS INITIAL ).
    SELECT SINGLE * FROM ZTIDS
    WHERE  ZFIDRNO = ZTIDS-ZFIDRNO.
  ENDIF.


  IF ZTIDS-ZFCLSEQ IS INITIAL.
    SELECT MAX( ZFCLSEQ ) INTO ZTIDS-ZFCLSEQ
    FROM   ZTIDS
    WHERE  ZFBLNO = ZTIDS-ZFBLNO.
  ENDIF.

  SELECT SINGLE *
  FROM   ZTIDS
  WHERE  ZFBLNO = ZTIDS-ZFBLNO
  AND    ZFCLSEQ = ZTIDS-ZFCLSEQ.
  IF SY-SUBRC NE 0. MESSAGE E782. ENDIF.
   *ZTIDS  =  ZTIDS.

  CLEAR ZTCUCL.
  SELECT SINGLE *
  FROM   ZTCUCL
  WHERE  ZFBLNO  = ZTIDS-ZFBLNO
  AND    ZFCLSEQ = ZTIDS-ZFCLSEQ.

  PERFORM READ_ZTIDSHS_TO_IT.
  PERFORM READ_ZTIDSHSD_TO_IT.
  PERFORM READ_ZTIDSHSL_TO_IT.
  PERFORM READ_ZTCUCLCST_TO_IT.

  IF SY-TCODE EQ 'ZIM75'.
    PERFORM  P2000_SET_ZTIDS_LOCK    USING   'L'.
  ENDIF.

ENDFORM.                               " READ_ZTIDS_FOR_SCR7500_SCR7600
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDRHS_TO_IT
*&---------------------------------------------------------------------*
FORM READ_ZTIDRHS_TO_IT.

  REFRESH IT_ZSIDSHS.
* SELECT *
* INTO   CORRESPONDING FIELDS OF TABLE IT_ZSIDSHS
* FROM   ZTIDRHS
* WHERE  ZFBLNO   =  ZTIDR-ZFBLNO
* AND    ZFCLSEQ  =  ZTIDR-ZFCLSEQ.

  SELECT * FROM   ZTIDRHS
           WHERE  ZFBLNO   =  ZTIDR-ZFBLNO
           AND    ZFCLSEQ  =  ZTIDR-ZFCLSEQ.
    MOVE-CORRESPONDING ZTIDRHS TO IT_ZSIDSHS.
    APPEND IT_ZSIDSHS.
  ENDSELECT.


ENDFORM.                               " READ_ZTIDRHS_TO_IT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDRHSD_TO_IT
*&---------------------------------------------------------------------*
FORM READ_ZTIDRHSD_TO_IT.

  REFRESH : IT_ZSIDSHSD, IT_ZSIDSHSD_CAL.
*  SELECT  *
*  INTO    CORRESPONDING FIELDS OF TABLE IT_ZSIDSHSD
*  FROM    ZTIDRHSD
*  WHERE   ZFBLNO   =   ZTIDR-ZFBLNO
*  AND     ZFCLSEQ  =   ZTIDR-ZFCLSEQ.
  SELECT * FROM ZTIDRHSD
           WHERE   ZFBLNO   =   ZTIDR-ZFBLNO
           AND     ZFCLSEQ  =   ZTIDR-ZFCLSEQ.
    MOVE-CORRESPONDING ZTIDRHSD TO IT_ZSIDSHSD.
    APPEND IT_ZSIDSHSD.
  ENDSELECT.

ENDFORM.                               " READ_ZTIDRHSD_TO_IT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDRHSL_TO_IT
*&---------------------------------------------------------------------*
FORM READ_ZTIDRHSL_TO_IT.

  REFRESH IT_ZSIDSHSL.
*  SELECT  *
*  INTO    CORRESPONDING FIELDS OF TABLE IT_ZSIDSHSL
*  FROM    ZTIDRHSL
*  WHERE   ZFBLNO  =  ZTIDR-ZFBLNO
*  AND     ZFCLSEQ =  ZTIDR-ZFCLSEQ.
  SELECT * FROM ZTIDRHSL
           WHERE ZFBLNO  = ZTIDR-ZFBLNO
           AND   ZFCLSEQ = ZTIDR-ZFCLSEQ.
    MOVE-CORRESPONDING ZTIDRHSL TO IT_ZSIDSHSL.
    APPEND IT_ZSIDSHSL.
  ENDSELECT.


ENDFORM.                               " READ_ZTIDRHSL_TO_IT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDSHS_TO_IT
*&---------------------------------------------------------------------*
FORM READ_ZTIDSHS_TO_IT.

  REFRESH IT_ZSIDSHS.
  SELECT  *
  INTO    CORRESPONDING FIELDS OF TABLE IT_ZSIDSHS
  FROM    ZTIDSHS
  WHERE   ZFBLNO   =  ZTIDS-ZFBLNO
  AND     ZFCLSEQ  =  ZTIDS-ZFCLSEQ .
  IT_ZSIDSHS_ORG[] = IT_ZSIDSHS[].

ENDFORM.                               " READ_ZTIDSHS_TO_IT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDSHSD_TO_IT
*&---------------------------------------------------------------------*
FORM READ_ZTIDSHSD_TO_IT.

  REFRESH IT_ZSIDSHSD.
  SELECT  *
  INTO    CORRESPONDING FIELDS OF TABLE IT_ZSIDSHSD
  FROM    ZTIDSHSD
  WHERE   ZFBLNO  =  ZTIDS-ZFBLNO
  AND     ZFCLSEQ =  ZTIDS-ZFCLSEQ .
  IT_ZSIDSHSD_ORG[] = IT_ZSIDSHSD[].

ENDFORM.                               " READ_ZTIDSHSD_TO_IT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDSHSL_TO_IT
*&---------------------------------------------------------------------*
FORM READ_ZTIDSHSL_TO_IT.

  REFRESH IT_ZSIDSHSL.
  SELECT  *
  INTO    CORRESPONDING FIELDS OF TABLE IT_ZSIDSHSL
  FROM    ZTIDSHSL
  WHERE   ZFBLNO   =  ZTIDS-ZFBLNO
  AND     ZFCLSEQ  =  ZTIDS-ZFCLSEQ .
  IT_ZSIDSHSL_ORG[] = IT_ZSIDSHSL_ORG[].

ENDFORM.                               " READ_ZTIDSHSL_TO_IT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR7600  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR7600 INPUT.

  MOVE C_REQ_D TO W_STATUS.

  SET SCREEN 7410.  LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_SCR7600  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR7420  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR7420 INPUT.

  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.
      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.
      ANTWORT = 'N'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " GET_OK_CODE_SCR7420  INPUT
*&---------------------------------------------------------------------*
*&      Form  READ_IDRCR_TO_IT
*&---------------------------------------------------------------------*
FORM READ_IDRCR_TO_IT.

  REFRESH IT_ZSIDRCR.
  SELECT * FROM ZTIDRCR
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.
    CLEAR   IT_ZSIDRCR.
    MOVE-CORRESPONDING ZTIDRCR TO IT_ZSIDRCR.
    APPEND IT_ZSIDRCR.
  ENDSELECT.

ENDFORM.                               " READ_IDRCR_TO_IT
*&---------------------------------------------------------------------*
*&      Form  READ_IDRCRIT_TO_IT
*&---------------------------------------------------------------------*
FORM READ_IDRCRIT_TO_IT.

  REFRESH IT_ZSIDRCRIT.
  SELECT *
    FROM ZTIDRCRIT
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.
    CLEAR   IT_ZSIDRCRIT.
    MOVE-CORRESPONDING ZTIDRCRIT TO IT_ZSIDRCRIT.
    APPEND IT_ZSIDRCRIT.
  ENDSELECT.

ENDFORM.                               " READ_IDRCRIT_TO_IT
*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR6410  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR6410 INPUT.

  PERFORM OK_CODE_DRDC.

ENDMODULE.                             " AFI_PROCESS_SCR6410  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_6410_UPDATE_SCR6410  INPUT
*&---------------------------------------------------------------------*
MODULE TC_6410_UPDATE_SCR6410 INPUT.
  IF W_STATUS EQ 'D'.
    READ TABLE IT_ZSIDRCR INDEX TC_6410-CURRENT_LINE.
    MOVE W_ROW_MARK TO IT_ZSIDRCR-ZFMARK.
    MODIFY IT_ZSIDRCR     INDEX TC_6410-CURRENT_LINE.
    EXIT.
  ENDIF.
  IF ZSIDRCR-ZFCONO IS INITIAL. EXIT. ENDIF.
  READ TABLE IT_ZSIDRCR INDEX TC_6410-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.
  MOVE-CORRESPONDING ZSIDRCR  TO IT_ZSIDRCR.
  MOVE W_ROW_MARK TO IT_ZSIDRCR-ZFMARK.
  SELECT SINGLE * FROM ZTIDSHS
   WHERE ZFBLNO =  IT_ZSIDRCR-ZFBLNO
     AND ZFCLSEQ = IT_ZSIDRCR-ZFCLSEQ
     AND ZFCONO =  IT_ZSIDRCR-ZFCONO.
  IF SY-SUBRC NE 0.
    MESSAGE E337 WITH ZSIDRCR-ZFCONO.
  ENDIF.
  IF W_OLD_SUBRC EQ 0.
    MODIFY IT_ZSIDRCR     INDEX TC_6410-CURRENT_LINE.
  ELSE.
    APPEND IT_ZSIDRCR.
    IF IT_ZSIDRCRIT-ZFCONO NE IT_ZSIDRCR-ZFCONO.
      SELECT  * FROM ZTIDSHSD
                     WHERE ZFBLNO = IT_ZSIDRCR-ZFBLNO AND
                          ZFCLSEQ = IT_ZSIDRCR-ZFCLSEQ AND
                           ZFCONO = IT_ZSIDRCR-ZFCONO.
        CLEAR IT_ZSIDRCRIT.
        MOVE-CORRESPONDING ZTIDSHSD TO IT_ZSIDRCRIT.
        APPEND IT_ZSIDRCRIT.
      ENDSELECT.
    ENDIF.
  ENDIF.
ENDMODULE.                             " TC_6410_UPDATE_SCR6410  INPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_REF_ZSPMT_ORG
*&---------------------------------------------------------------------*
FORM P1000_REF_ZSPMT_ORG.

ENDFORM.                               " P1000_REF_ZSPMT_ORG
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR8110_NEWL_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SCR8110_NEWL_PROCESS.

  PERFORM  P2000_SET_ACTION_TITLE.
  SPOP-TITEL = 'Create : Payment Notice'.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  CLEAR ZTPMTHD.
*  CALL SCREEN 8114 STARTING AT 15 1
*                   ENDING   AT 64 6.

ENDFORM.                               " P2000_SCR8110_NEWL_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR8110_NEWL_YES
*&---------------------------------------------------------------------*
FORM P2000_SCR8110_NEWL_YES.

ENDFORM.                               " P2000_SCR8110_NEWL_YES
*&---------------------------------------------------------------------*
*&      Module  ZFPONC_CHECK_SCR3110  INPUT
*&---------------------------------------------------------------------*
MODULE ZFPONC_CHECK_SCR3110 INPUT.

  CHECK  W_STATUS NE C_REQ_D.
  CHECK  W_STATUS NE C_OPEN_U.
*>> LOCAL DATA LOGIC SKIP
  CHECK  ZTIV-ZFREQTY NE 'LO'.
  CHECK  ZTIV-ZFREQTY NE 'PU'.

*  IF ZSIV-ZFYSDST    IS INITIAL AND          "> 양수/양도 구분자.
*     ZSIV-ZFTRIPLE   IS INITIAL.             "> 삼국무역구분자.
*     EXIT.
*  ENDIF.

  CLEAR : ZSIMIMG08, ZTIMIMG08.
  IF ZTIV-ZFPONC IS INITIAL.
    IF ZTIV-ZFCUST NE 'N'.
      MESSAGE E167 WITH 'Classification of import transaction'.
    ELSE.
      EXIT.
    ENDIF.
  ENDIF.

  SELECT SINGLE * FROM ZTIMIMG08
                  WHERE ZFCDTY = '001'
                  AND ZFCD     =  ZTIV-ZFPONC.

  MOVE-CORRESPONDING  ZTIMIMG08 TO ZSIMIMG08.

  IF SY-SUBRC NE 0.
    MESSAGE E809 WITH ZTIV-ZFPONC.
  ENDIF.

  IF ZTIMIMG08-ZFCD1 = 'Y' AND ZTIV-ZFPOYN = 'Y'.
    EXIT.
  ENDIF.
  IF ZTIMIMG08-ZFCD1 = 'N' AND ZTIV-ZFPOYN = 'N'.
    EXIT.
  ENDIF.
  IF ZTIMIMG08-ZFCD1 IS INITIAL AND ZTIV-ZFPOYN = 'N'.
    EXIT.
  ENDIF.
  MESSAGE E811.

ENDMODULE.                             " ZFPONC_CHECK_SCR3110  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFBNARCD_CHECK_SCR7110  INPUT
*&---------------------------------------------------------------------*
MODULE ZFBNARCD_CHECK_SCR7110 INPUT.

  IF W_STATUS = 'D'.
    EXIT.
  ENDIF.

  IF ZTBLUG-ZFBNARCD IS INITIAL.
    EXIT.
  ENDIF.

  CLEAR W_ZFBNARCD_NM.
  SELECT MAX( ZFBNARM ) INTO W_ZFBNARCD_NM
    FROM ZTIMIMG03
   WHERE ZFBNARCD = ZTBLUG-ZFBNARCD.
  IF W_ZFBNARCD_NM IS INITIAL.
    MESSAGE E810 WITH ZTBLUG-ZFBNARCD.
  ENDIF.

ENDMODULE.                             " ZFBNARCD_CHECK_SCR7110  INPUT
*&---------------------------------------------------------------------*
*&      Form  READ_BKPF_UPDATE_ZTVTIV_SCR8300
*&---------------------------------------------------------------------*
FORM READ_BKPF_ZTVTIV_SCR8300.

*  IF W_RERUN = 'X'.                    " 선택 조건에 맞는 Row를 Delete
*    SELECT *
*      FROM ZTVTIV
**     WHERE ZFPOSDT >= W_ZFPOSDT_FROM
**       AND ZFPOSDT <= W_ZFPOSDT_TO
**       AND ZFDODT  >= W_ZFDODT_FROM
*     WHERE ZFDODT = W_ZFDODT_FROM
**       AND ZFDODT  <= W_ZFDODT_TO
*       AND LIFNR   >= W_LIFNR_FROM
*       AND LIFNR   <= W_LIFNR_TO
*       AND EBELN   >= W_EBELN_FROM
*       AND EBELN   <= W_EBELN_TO.
*      IF ZTVTIV-ZFGFDYR < W_ZFGFDYR_FROM.     " 회계전표번?
*        CONTINUE.
*      ENDIF.
*      IF ZTVTIV-ZFGFDYR = W_ZFGFDYR_FROM AND
*         ZTVTIV-ZFGFDNO < W_ZFGFDNO_FROM.
*        CONTINUE.
*      ENDIF.
*      IF ZTVTIV-ZFGFDYR > W_ZFGFDYR_TO.
*        CONTINUE.
*      ENDIF.
*      IF ZTVTIV-ZFGFDYR = W_ZFGFDYR_TO AND
*         ZTVTIV-ZFGFDNO > W_ZFGFDNO_TO.
*        CONTINUE.
*      ENDIF.
*      SELECT MAX( ZFOPNNO ) INTO W_ZFOPNNO              " L/C No
*        FROM ZTREQHD
*       WHERE EBELN = ZTVTIV-EBELN.
*      IF W_ZFOPNNO < W_ZFOPNNO_FROM OR
*         W_ZFOPNNO > W_ZFOPNNO_TO.
*        CONTINUE.
*      ENDIF.
*
*      CLEAR ZTVTIV.
*      SELECT SINGLE *
*        FROM ZTVTIV
*       WHERE ZFGFDYR = ZTVTIV-ZFGFDYR
*         AND ZFGFDNO = ZTVTIV-ZFGFDNO.
*      IF ( ZTVTIV-ZFREDNO NE '          ' ) OR
*         ( ZTVTIV-ZFVTNO NE '          ' ).
*         CONTINUE.
*      ENDIF.
*
*      DELETE FROM ZTVTIVIT WHERE ZFGFDYR = ZTVTIV-ZFGFDYR
*                             AND ZFGFDNO = ZTVTIV-ZFGFDNO.
*      DELETE ZTVTIV.
*    ENDSELECT.
*  ENDIF.

*  MOVE 0   TO W_PROC_CNT.
*  SELECT *
*    FROM BKPF
*   WHERE BLART = 'RE'
*     AND BUDAT >= W_ZFPOSDT_FROM       " Posting Date
*     AND BUDAT <= W_ZFPOSDT_TO
*     AND BLDAT >= W_ZFDODT_FROM        " Document Date
**   WHERE BLDAT = W_ZFDODT_FROM        " Document Date
*     AND BLDAT <= W_ZFDODT_TO
*     ORDER BY GJAHR BELNR.
*    IF BKPF-GJAHR < W_ZFGFDYR_FROM.    " 회계전표번?
*      CONTINUE.
*    ENDIF.
*    IF BKPF-GJAHR = W_ZFGFDYR_FROM AND
*       BKPF-BELNR < W_ZFGFDNO_FROM.
*      CONTINUE.
*    ENDIF.
*    IF BKPF-GJAHR > W_ZFGFDYR_TO.
*      CONTINUE.
*    ENDIF.
*    IF BKPF-GJAHR = W_ZFGFDYR_TO AND
*       BKPF-BELNR > W_ZFGFDNO_TO.
*      CONTINUE.
*    ENDIF.
*    CLEAR ZTVTIV.
*    MOVE BKPF-GJAHR         TO ZTVTIV-ZFGFDYR.
*    MOVE BKPF-BELNR         TO ZTVTIV-ZFGFDNO.
*    MOVE BKPF-BUDAT         TO ZTVTIV-ZFPOSDT.
*    MOVE BKPF-BLDAT         TO ZTVTIV-ZFDODT.
*    MOVE BKPF-KURSF         TO ZTVTIV-ZFEXRT.
*    MOVE '         '        TO ZTVTIV-ZFVTNO.
*    MOVE '         '        TO ZTVTIV-ZFREDNO.
*    MOVE SY-UNAME           TO ZTVTIV-ERNAM.
*    MOVE SY-DATUM           TO ZTVTIV-CDAT.
*    MOVE SY-UNAME           TO ZTVTIV-UNAM.
*    MOVE SY-DATUM           TO ZTVTIV-UDAT.
*
*    SELECT SINGLE *
*      FROM ZTIMIMG11.

*    MOVE 0        TO W_LOOP_CNT.
*    SELECT *
*      FROM BSEG
*     WHERE BUKRS = BKPF-BUKRS
*       AND BELNR = BKPF-BELNR
*       AND GJAHR = BKPF-GJAHR
*       AND BSCHL = '31'
*       AND ( ZTERM = ZTIMIMG11-ZTERM1 OR ZTERM = ZTIMIMG11-ZTERM2 ).
*      IF NOT ( BSEG-AUGBL IS INITIAL ).
*        CONTINUE.
*      ENDIF.
*      IF BSEG-LIFNR < W_LIFNR_FROM OR  " Vendor
*         BSEG-LIFNR > W_LIFNR_TO.
*        CONTINUE.
*      ENDIF.
*      MOVE BSEG-LIFNR         TO ZTVTIV-LIFNR.
*      IF BSEG-ZTERM = ZTIMIMG11-ZTERM1.
*        MOVE 'LO'  TO ZTVTIV-ZFREQTY.
*      ELSE.
*        MOVE 'PU'  TO ZTVTIV-ZFREQTY.
*      ENDIF.
*      SELECT *
*        FROM BSEG
*       WHERE BUKRS = BKPF-BUKRS
*         AND BELNR = BKPF-BELNR
*         AND GJAHR = BKPF-GJAHR
*         AND BSCHL = '86'.
*        MOVE BSEG-EBELN   TO ZTVTIV-EBELN.
*        IF BSEG-EBELN < W_EBELN_FROM OR" P/O No
*           BSEG-EBELN > W_EBELN_TO.
*          CONTINUE.
*        ENDIF.
*
*        SELECT MAX( ZFREQNO ) INTO W_ZFREQNO
*          FROM ZTREQHD
*         WHERE EBELN = BSEG-EBELN.
*        IF W_ZFREQNO IS INITIAL.
*          CONTINUE.
*        ENDIF.
*        SELECT SINGLE *
*          FROM ZTREQHD
*         WHERE ZFREQNO = W_ZFREQNO.
*        IF ZTREQHD-ZFOPNNO < W_ZFOPNNO_FROM OR
*           ZTREQHD-ZFOPNNO > W_ZFOPNNO_TO.
*          CONTINUE.
*        ENDIF.
*        MOVE ZTREQHD-ZFWERKS TO ZTVTIV-ZFWERKS.
*        CLEAR ZTVTIVIT.
*        MOVE BSEG-GJAHR         TO ZTVTIVIT-ZFGFDYR.
*        MOVE BSEG-BELNR         TO ZTVTIVIT-ZFGFDNO.
*        MOVE BSEG-EBELP         TO ZTVTIVIT-ZFIVDNO.
*        MOVE BSEG-MENGE         TO ZTVTIVIT-ZFQUN.
*        MOVE BSEG-MEINS         TO ZTVTIVIT-ZFQUNM.
*        SELECT SINGLE *
*          FROM EKPO
*         WHERE EBELN = BSEG-EBELN
*           AND EBELP = BSEG-EBELP.
*        MOVE EKPO-NETPR         TO ZTVTIVIT-NETPR.
*        MOVE EKPO-PEINH         TO ZTVTIVIT-PEINH.
*        MOVE BSEG-MATNR         TO ZTVTIVIT-MATNR.
*        MOVE BSEG-BPRME         TO ZTVTIVIT-BPRME.
**        MOVE BSEG-WRBTR         TO ZTVTIVIT-ZFIVAMT.
*        MOVE BKPF-WAERS         TO ZTVTIVIT-ZFIVAMC.
**        MOVE BSEG-DMBTR         TO ZTVTIVIT-ZFKAMT.
*        MOVE 'KRW'              TO ZTVTIVIT-ZFKRW.
*        SELECT COUNT( DISTINCT ZFIVDNO ) INTO W_COUNT
*          FROM ZTVTIVIT
*         WHERE ZFGFDYR = BSEG-GJAHR
*           AND ZFGFDNO = BSEG-BELNR
*           AND ZFIVDNO = BSEG-EBELP.
*        IF W_COUNT > 0.
*          CONTINUE.
*        ENDIF.
*        SELECT *
*          FROM BSEG
*         WHERE BUKRS = BKPF-BUKRS
*           AND BELNR = BKPF-BELNR
*           AND GJAHR = BKPF-GJAHR
*           AND EBELN = BSEG-EBELN
*           AND EBELP = BSEG-EBELP.
*           IF BSEG-SHKZG = 'S'. "Debit
*               ZTVTIVIT-ZFIVAMT =  ZTVTIVIT-ZFIVAMT + BSEG-WRBTR.
*               ZTVTIVIT-ZFKAMT  =  ZTVTIVIT-ZFKAMT  + BSEG-DMBTR.
*           ELSE. " Credit
*               ZTVTIVIT-ZFIVAMT =  ZTVTIVIT-ZFIVAMT - BSEG-WRBTR.
*               ZTVTIVIT-ZFKAMT  =  ZTVTIVIT-ZFKAMT  - BSEG-DMBTR.
*           ENDIF.
*        ENDSELECT.
*        INSERT ZTVTIVIT.
*        IF SY-SUBRC NE 0.
*          MESSAGE I812.
*          EXIT.
*        ENDIF.
*        ADD 1            TO W_LOOP_CNT.
*      ENDSELECT.
*    ENDSELECT.
*    IF W_LOOP_CNT = 0.
*      CONTINUE.
*    ENDIF.
*    INSERT ZTVTIV.
*    IF SY-SUBRC NE 0.
*      MESSAGE I812.
*      EXIT.
*    ENDIF.
*    ADD 1                   TO W_PROC_CNT.
*  ENDSELECT.

*  MESSAGE S813  WITH W_PROC_CNT.

ENDFORM.                               " READ_BKPF_UPDATE_ZTVTIV_SCR8300
*&---------------------------------------------------------------------*
*&      Module  ZTVTIV_READ_FOR_ZCR8400  INPUT
*&---------------------------------------------------------------------*
MODULE ZTVTIV_READ_FOR_SCR8400 INPUT.

  PERFORM OK_CODE_BACK_EXIT.

  IF ZTVTIV-ZFGFDYR IS INITIAL OR ZTVTIV-ZFGFDNO IS INITIAL.
    MESSAGE E814.
  ENDIF.

  SELECT SINGLE *
    FROM ZTVTIV
   WHERE ZFGFDYR = ZTVTIV-ZFGFDYR
     AND ZFGFDNO = ZTVTIV-ZFGFDNO
     AND BUKRS   = ZTVTIV-BUKRS.
  IF SY-SUBRC NE 0.
    MESSAGE E815.
  ENDIF.

  CLEAR : W_ZFIVAMT_S, W_ZFIVAMC, W_ZFKAMT_S, W_LIFNR_NM.
  SELECT SUM( ZFIVAMT ) MAX( ZFIVAMC ) SUM( ZFKAMT )
    INTO (W_ZFIVAMT_S, W_ZFIVAMC, W_ZFKAMT_S)
    FROM ZTVTIVIT
   WHERE ZFGFDYR = ZTVTIV-ZFGFDYR
     AND ZFGFDNO = ZTVTIV-ZFGFDNO
     AND BUKRS   = ZTVTIV-BUKRS.
  SELECT SINGLE NAME1 INTO W_LIFNR_NM
    FROM LFA1
   WHERE LIFNR = ZTVTIV-LIFNR.
  MOVE 'KRW'     TO W_ZFKRW.

  PERFORM READ_ZTVTIVIT_TO_IT.

ENDMODULE.                             " ZTVTIV_READ_FOR_SCR8400  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6900  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6900 INPUT.

ENDMODULE.                             " USER_COMMAND_SCR6900  INPUT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTVTIVIT_TO_IT
*&---------------------------------------------------------------------*
FORM READ_ZTVTIVIT_TO_IT.

  REFRESH IT_ZSVTIVIT.
  SELECT *
    FROM ZTVTIVIT
   WHERE ZFGFDYR = ZTVTIV-ZFGFDYR
     AND ZFGFDNO = ZTVTIV-ZFGFDNO
     AND BUKRS   = ZTVTIV-BUKRS.
    CLEAR IT_ZSVTIVIT.
    MOVE-CORRESPONDING ZTVTIVIT TO IT_ZSVTIVIT.
    SELECT SINGLE MAKTX INTO IT_ZSVTIVIT-TXZ01       " Short Text
      FROM MAKT
     WHERE SPRAS = SY-LANGU
       AND MATNR = IT_ZSVTIVIT-MATNR.
    APPEND IT_ZSVTIVIT.
  ENDSELECT.

ENDFORM.                               " READ_ZTVTIVIT_TO_IT

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIDRCR_MODIFY_SCR6400
*&---------------------------------------------------------------------*
FORM P3000_ZTIDRCR_MODIFY_SCR6400.

  DESCRIBE TABLE IT_ZSIDRCR LINES LINE.
  IF LINE = 0.
    MESSAGE E726.
  ENDIF.
  DESCRIBE TABLE IT_ZSIDRCRIT LINES LINE.
  IF LINE = 0.
    MESSAGE E376.
  ENDIF.

  DELETE FROM ZTIDRCR
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.
  DELETE FROM ZTIDRCRIT
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.

  LOOP AT IT_ZSIDRCR.
    CLEAR : ZTIDRCR.
    MOVE-CORRESPONDING IT_ZSIDRCR TO ZTIDRCR.
    MOVE SY-UNAME                 TO ZTIDRCR-UNAM.
    MOVE SY-DATUM                 TO ZTIDRCR-UDAT.
    INSERT ZTIDRCR.
    IF SY-SUBRC NE 0.
      MESSAGE  E816.
      EXIT.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIDRCRIT.
    CLEAR : ZTIDRCRIT.
    MOVE-CORRESPONDING IT_ZSIDRCRIT TO ZTIDRCRIT.
    MOVE SY-UNAME                   TO ZTIDRCRIT-UNAM.
    MOVE SY-DATUM                   TO ZTIDRCRIT-UDAT.
    INSERT ZTIDRCRIT.
    IF SY-SUBRC NE 0.
      MESSAGE  E377.
      EXIT.
    ENDIF.
  ENDLOOP.

  MESSAGE  S817.

ENDFORM.                               " P3000_ZTIDRCR_MODIFY_SCR6400

*&---------------------------------------------------------------------*
*&      Module  CUCLCST_GET_LINE_SCR7420  INPUT
*&---------------------------------------------------------------------*
MODULE CUCLCST_GET_LINE_SCR7420 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_7420-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " CUCLCST_GET_LINE_SCR7420  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR7420  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR7420 INPUT.

  CASE OK-CODE.
    WHEN 'REF1'.
      IT_ZSCUCLCST[] = IT_ZSCUCLCST_ORG[].
    WHEN 'MKA1'.
      PERFORM  P2000_SET_ROW_MARK.
    WHEN 'MKL1'.
      PERFORM  P2000_SET_ROW_MARK.
    WHEN 'DEL1'.
      PERFORM P2000_DATA_DELETE.
    WHEN 'VEN1' OR 'VEN2' OR 'MKPF' OR 'BKPF'.
      PERFORM   P2000_LINE_SELECT.
      PERFORM   P2000_LINE_CALL_T_CODE.
    WHEN 'SCHG'.
      W_CHK_CNT = 0.
      LOOP AT IT_ZSCUCLCST WHERE ZFMARK = 'X'.
        W_TABIX   = SY-TABIX.
        W_CHK_CNT = W_CHK_CNT + 1.
      ENDLOOP.
      IF W_CHK_CNT > 1. MESSAGE S965. EXIT. ENDIF.
      IF W_CHK_CNT = 0. MESSAGE S766. EXIT. ENDIF.

      MOVE-CORRESPONDING IT_ZSCUCLCST TO ZSCUCLCST.
      PERFORM P2000_DOC_CHANGE.
      IF ANTWORT = 'Y'.
        MOVE ' ' TO ZSCUCLCST-ZFACDO.
        MOVE ' ' TO ZSCUCLCST-ZFFIYR.
        MOVE-CORRESPONDING ZSCUCLCST TO IT_ZSCUCLCST.
        MODIFY IT_ZSCUCLCST INDEX W_TABIX.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR7420  INPUT

*&---------------------------------------------------------------------*
*&      Form  READ_ZTCUCLCST_TO_IT
*&---------------------------------------------------------------------*
FORM READ_ZTCUCLCST_TO_IT.

  REFRESH IT_ZSCUCLCST.
  SELECT  *
  INTO    CORRESPONDING FIELDS OF TABLE IT_ZSCUCLCST
  FROM    ZTCUCLCST
  WHERE   ZFBLNO   =  ZTIDS-ZFBLNO
  AND     ZFCLSEQ  =  ZTIDS-ZFCLSEQ .

  LOOP  AT  IT_ZSCUCLCST.
    W_TABIX   =  SY-TABIX.
*>> 비용코드명 SELECT!
    SELECT  SINGLE ZFCDNM  INTO  IT_ZSCUCLCST-ZFCDNM
    FROM    ZTIMIMG08
    WHERE   ZFCDTY  =  '006'
    AND     ZFCD    =  IT_ZSCUCLCST-ZFCSCD.
*>> 구매처, 지급처명 SELECT!
    SELECT  SINGLE NAME1  INTO  IT_ZSCUCLCST-ZFVENNM
    FROM    LFA1
    WHERE   LIFNR   =  IT_ZSCUCLCST-ZFVEN.

    SELECT  SINGLE NAME1  INTO  IT_ZSCUCLCST-ZFPAYNM
    FROM    LFA1
    WHERE   LIFNR   =  IT_ZSCUCLCST-ZFPAY.

    MODIFY IT_ZSCUCLCST INDEX W_TABIX.
  ENDLOOP.
  IT_ZSCUCLCST_ORG[] = IT_ZSCUCLCST[].

  SELECT MAX( ZFCSQ ) INTO W_ZFCSQ_M
  FROM   ZTCUCLCST
  WHERE  ZFBLNO  = ZTIDS-ZFBLNO
  AND    ZFCLSEQ = ZTIDS-ZFCLSEQ.

ENDFORM.                               " READ_ZTCUCLCST_TO_IT

*&---------------------------------------------------------------------*
*&      Module  TC_7420_UPDATE_SCR7420  INPUT
*&---------------------------------------------------------------------*
MODULE TC_7420_UPDATE_SCR7420 INPUT.

  READ TABLE IT_ZSCUCLCST INDEX TC_7420-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.
  MOVE SY-TABIX TO W_TABIX.

*  MOVE-CORRESPONDING ZSCUCLCST  TO  IT_ZSCUCLCST.
  MOVE W_ROW_MARK TO IT_ZSCUCLCST-ZFMARK.
  IF W_OLD_SUBRC = 0.
    MODIFY IT_ZSCUCLCST   INDEX W_TABIX.
*  ELSE.
*    APPEND IT_ZSCUCLCST.
  ENDIF.

ENDMODULE.                             " TC_7420_UPDATE_SCR7420  INPUT

*&---------------------------------------------------------------------*
*&      Module  TC_6412_UPDATE_SCR6412  INPUT
*&---------------------------------------------------------------------*
MODULE TC_6412_UPDATE_SCR6412 INPUT.

  IF ZSIDRCRIT-ZFRONO IS INITIAL. EXIT. ENDIF.
  READ TABLE IT_ZSIDRCRIT_S INDEX TC_6412-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.
  MOVE-CORRESPONDING ZSIDRCRIT TO IT_ZSIDRCRIT_S.
  MOVE W_ROW_MARK TO IT_ZSIDRCRIT_S-ZFMARK.

  SELECT SINGLE * FROM ZTIDSHSD
   WHERE  ZFBLNO =  IT_ZSIDRCRIT_S-ZFBLNO
     AND  ZFCLSEQ = IT_ZSIDRCRIT_S-ZFCLSEQ
     AND  ZFCONO =  IT_ZSIDRCRIT_S-ZFCONO
     AND  ZFRONO =  IT_ZSIDRCRIT_S-ZFRONO.

  IF SY-SUBRC NE 0.
    MESSAGE E342 WITH ZSIDRCRIT-ZFRONO.
  ENDIF.

  IF W_OLD_SUBRC EQ 0.
    MODIFY IT_ZSIDRCRIT_S     INDEX TC_6412-CURRENT_LINE.
  ELSE.
    APPEND IT_ZSIDRCRIT_S.
  ENDIF.
ENDMODULE.                             " TC_6412_UPDATE_SCR6412  INPUT

*&---------------------------------------------------------------------*
*&      Module  IDRNO_CHECK_FOR_SCR6610  INPUT
*&---------------------------------------------------------------------*
MODULE IDRNO_CHECK_FOR_SCR6610 INPUT.

  PERFORM OK_CODE_BACK_EXIT.

  PERFORM READ_ZTIDRDTU_FOR_SCR6610.

ENDMODULE.                             " IDRNO_CHECK_FOR_SCR6610  INPUT

*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDRDTU_FOR_SCR6610
*&---------------------------------------------------------------------*
FORM READ_ZTIDRDTU_FOR_SCR6610.

  IF NOT ( ZTCUCL-ZFIDRNO IS INITIAL ).
    SELECT SINGLE *
      FROM ZTCUCL
     WHERE ZFIDRNO = ZTCUCL-ZFIDRNO.
    IF SY-SUBRC NE 0.
      MESSAGE E758.
    ENDIF.
    SELECT SINGLE *
      FROM ZTBL
     WHERE ZFBLNO = ZTCUCL-ZFBLNO.
  ENDIF.

  IF ( ZTCUCL-ZFBLNO IS INITIAL ) AND NOT ( ZTBL-ZFHBLNO IS INITIAL ).
    SELECT COUNT( DISTINCT ZFBLNO ) INTO W_COUNT
      FROM ZTBL
     WHERE ZFHBLNO = ZTBL-ZFHBLNO.
    IF W_COUNT > 1.
      MESSAGE E759 WITH ZTBL-ZFHBLNO.
    ENDIF.
    IF W_COUNT = 1.
      SELECT SINGLE *
        FROM ZTBL
       WHERE ZFHBLNO = ZTBL-ZFHBLNO.
      MOVE ZTBL-ZFBLNO TO ZTCUCL-ZFBLNO.
    ENDIF.
    IF W_COUNT = 0.
      MESSAGE E707 WITH ZTBL-ZFHBLNO.
    ENDIF.
  ENDIF.
  IF NOT ( ZTCUCL-ZFBLNO IS INITIAL ) AND ( ZTBL-ZFHBLNO IS INITIAL ).
    SELECT SINGLE *
      FROM ZTBL
     WHERE ZFBLNO = ZTCUCL-ZFBLNO.
    IF SY-SUBRC NE 0.
      MESSAGE E759 WITH ZTCUCL-ZFBLNO.
    ENDIF.
  ENDIF.

  IF ( ZTCUCL-ZFBLNO IS INITIAL ) AND ( ZTBL-ZFHBLNO IS INITIAL )
                                  AND ( ZTCUCL-ZFIDRNO IS INITIAL ).
    MESSAGE E760.
  ENDIF.

  IF ZTCUCL-ZFCLSEQ IS INITIAL.
    SELECT MAX( ZFCLSEQ ) INTO ZTCUCL-ZFCLSEQ
      FROM ZTIDRCR
     WHERE ZFBLNO = ZTCUCL-ZFBLNO.
  ENDIF.

  SELECT COUNT( DISTINCT ZFBLNO ) INTO W_COUNT
    FROM ZTIDRCR
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.
  IF W_COUNT EQ 0.
    MESSAGE E761.
  ENDIF.

  IF ZTCUCL-ZFIDRNO IS INITIAL.
    SELECT SINGLE *
      FROM ZTCUCL
     WHERE ZFBLNO = ZTCUCL-ZFBLNO
       AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.
  ENDIF.

  CLEAR : ZTIDS, ZTIMIMG02, W_ZFINRC_NM.
  SELECT SINGLE *
    FROM ZTIDS
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.
  SELECT SINGLE *
    FROM ZTIMIMG02
   WHERE ZFCOTM = ZTIDS-ZFINRC.
  SELECT SINGLE NAME1 INTO W_ZFINRC_NM
    FROM LFA1
   WHERE LIFNR = ZTIMIMG02-ZFVEN.

  PERFORM READ_IDRCRIT_TO_IT_SCR6600.
  PERFORM READ_IDRDTU_TO_IT.

ENDFORM.                               " READ_ZTIDRDTU_FOR_SCR6610

*&---------------------------------------------------------------------*
*&      Form  READ_IDRDTU_TO_IT
*&---------------------------------------------------------------------*
FORM READ_IDRDTU_TO_IT.

  REFRESH IT_ZSIDRDTU.
  SELECT *
    FROM ZTIDRDTU
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.
    CLEAR   IT_ZSIDRDTU.
    MOVE-CORRESPONDING ZTIDRDTU TO IT_ZSIDRDTU.
    SELECT SINGLE *
      FROM ZTIDRCRIT
     WHERE ZFBLNO = ZTCUCL-ZFBLNO
       AND ZFCLSEQ = ZTCUCL-ZFCLSEQ
       AND ZFCONO = IT_ZSIDRDTU-ZFCONO
       AND ZFRONO = IT_ZSIDRDTU-ZFRONO.
    MOVE ZTIDRCRIT-ZFDEQN       TO IT_ZSIDRDTU-ZFDEQN.
    MOVE ZTIDRCRIT-ZFDEQNM      TO IT_ZSIDRDTU-ZFDEQNM.
    MOVE ZTIDRCRIT-MAKTX        TO IT_ZSIDRDTU-MAKTX.
    APPEND IT_ZSIDRDTU.
  ENDSELECT.

ENDFORM.                               " READ_IDRDTU_TO_IT

*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR6610  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR6610 INPUT.

  PERFORM OK_CODE_DRTU.

ENDMODULE.                             " AFI_PROCESS_SCR6610  INPUT

*&---------------------------------------------------------------------*
*&      Form  OK_CODE_DRTU
*&---------------------------------------------------------------------*
FORM OK_CODE_DRTU.

ENDFORM.                               " OK_CODE_DRTU

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIDRDTU_MODIFY_SCR6600
*&---------------------------------------------------------------------*
FORM P3000_ZTIDRDTU_MODIFY_SCR6600.

  DELETE FROM ZTIDRDTU
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ.

  DESCRIBE TABLE IT_ZSIDRDTU LINES LINE.
  IF LINE = 0.
    MESSAGE  S823.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSIDRDTU.
    CLEAR : ZTIDRDTU.
    MOVE-CORRESPONDING IT_ZSIDRDTU TO ZTIDRDTU.
    SELECT MAX( ZFUSEQ ) INTO ZTIDRDTU-ZFUSEQ
      FROM ZTIDRDTU
     WHERE ZFBLNO = ZTCUCL-ZFBLNO
       AND ZFCLSEQ = ZTCUCL-ZFCLSEQ
       AND ZFCONO = ZTIDRDTU-ZFCONO
       AND ZFRONO = ZTIDRDTU-ZFRONO.
    MOVE ZTCUCL-ZFBLNO             TO ZTIDRDTU-ZFBLNO.
    MOVE ZTCUCL-ZFCLSEQ            TO ZTIDRDTU-ZFCLSEQ.
    ADD 10                         TO ZTIDRDTU-ZFUSEQ.
    MOVE SY-UNAME                  TO ZTIDRDTU-ERNAM.
    MOVE SY-DATUM                  TO ZTIDRDTU-CDAT.
    MOVE SY-UNAME                  TO ZTIDRDTU-UNAM.
    MOVE SY-DATUM                  TO ZTIDRDTU-UDAT.
    INSERT ZTIDRDTU.
    IF SY-SUBRC NE 0.
      MESSAGE  E824.
      EXIT.
    ENDIF.
    UPDATE ZTIDRCRIT SET ZFEDDT = ZTIDRDTU-ZFUSDT
                         UNAM   = SY-UNAME
                         UDAT   = SY-DATUM
     WHERE ZFBLNO = ZTCUCL-ZFBLNO
       AND ZFCLSEQ = ZTCUCL-ZFCLSEQ
       AND ZFCONO = ZTIDRDTU-ZFCONO
       AND ZFRONO = ZTIDRDTU-ZFRONO.
  ENDLOOP.

  MESSAGE  S823.

ENDFORM.                               " P3000_ZTIDRDTU_MODIFY_SCR6600

*&---------------------------------------------------------------------*
*&      Module  P1000_ZTIV_READ_SCR8100  INPUT
*&---------------------------------------------------------------------*
MODULE P1000_ZTIV_READ_SCR8100 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

*  PERFORM OK_CODE_PATM.
*  PERFORM OK_CODE_BACK_EXIT.

  CLEAR  *ZTPMTHD.
*>> 생성 조건 CHECK!
  IF W_EBELN EQ SPACE  AND  W_ZFREQNO     EQ SPACE
                       AND  W_ZFOPNNO     EQ SPACE
                       AND  ZTRED-ZFREDNO EQ SPACE
                       AND  ZTRED-ZFISNO  EQ SPACE.
    MESSAGE  E193.
  ENDIF.

*>> PO NUMBER 로 CHECK!
  IF W_EBELN  NE  SPACE .

    IF W_ZFREQNO IS INITIAL AND W_ZFOPNNO IS INITIAL.
      SELECT  COUNT( DISTINCT ZFREQNO ) INTO  W_COUNT
      FROM    ZTREQHD                   WHERE EBELN  =  W_EBELN.
      IF W_COUNT EQ 0. MESSAGE W870 WITH W_EBELN. ENDIF.
    ELSEIF W_ZFREQNO IS INITIAL AND W_ZFOPNNO NE SPACE.
      SELECT  COUNT( DISTINCT ZFREQNO ) INTO  W_COUNT
      FROM    ZTREQHD
      WHERE   EBELN    EQ  W_EBELN
      AND     ZFOPNNO  EQ  W_ZFOPNNO.
      IF W_COUNT EQ 0. MESSAGE W570. ENDIF.
    ELSEIF W_ZFREQNO NE SPACE AND W_ZFOPNNO IS INITIAL.
      SELECT  COUNT( DISTINCT ZFREQNO ) INTO  W_COUNT
      FROM    ZTREQHD
      WHERE   EBELN    EQ  W_EBELN
      AND     ZFREQNO  EQ  W_ZFREQNO.
      IF W_COUNT EQ 0. MESSAGE W570. ENDIF.
    ELSE.
      SELECT  COUNT( DISTINCT ZFREQNO ) INTO  W_COUNT
      FROM    ZTREQHD
      WHERE   EBELN    EQ  W_EBELN
      AND     ZFOPNNO  EQ  W_ZFOPNNO
      AND     ZFREQNO  EQ  W_ZFREQNO.
      IF W_COUNT EQ 0. MESSAGE W570. ENDIF.
    ENDIF.

    CASE W_COUNT.
      WHEN 1.
        SELECT SINGLE *   FROM ZTREQHD
        WHERE  EBELN  EQ  W_EBELN.
        MOVE ZTREQHD-ZFREQNO TO W_ZFREQNO.
        MOVE ZTREQHD-ZFOPNNO TO W_ZFOPNNO.
      WHEN OTHERS.
        MESSAGE E871 WITH W_EBELN.
    ENDCASE.
  ENDIF.

*>> LC NUMBER 로 CHECK!
  IF W_ZFOPNNO  NE  SPACE.
    IF W_EBELN IS INITIAL  AND W_ZFREQNO IS INITIAL.
      SELECT  COUNT( DISTINCT ZFREQNO )  INTO  W_COUNT
      FROM    ZTREQHD                    WHERE ZFOPNNO  =  W_ZFOPNNO.
      IF W_COUNT EQ 0. MESSAGE E709 WITH W_ZFOPNNO. ENDIF.
    ELSEIF W_EBELN IS INITIAL AND W_ZFREQNO NE SPACE.
      SELECT  COUNT( DISTINCT ZFREQNO ) INTO  W_COUNT
      FROM    ZTREQHD
      WHERE   ZFREQNO  EQ  W_ZFREQNO
      AND     ZFOPNNO  EQ  W_ZFOPNNO.
      IF W_COUNT EQ 0. MESSAGE E570. ENDIF.
    ELSEIF W_EBELN NE SPACE AND W_ZFREQNO IS INITIAL.
      SELECT  COUNT( DISTINCT ZFREQNO ) INTO  W_COUNT
      FROM    ZTREQHD
      WHERE   EBELN    EQ  W_EBELN
      AND     ZFOPNNO  EQ  W_ZFOPNNO.
      IF W_COUNT EQ 0. MESSAGE E570. ENDIF.
    ELSE.
      SELECT  COUNT( DISTINCT ZFREQNO ) INTO  W_COUNT
      FROM    ZTREQHD
      WHERE   ZFREQNO  EQ  W_ZFREQNO
      AND     ZFOPNNO  EQ  W_ZFOPNNO
      AND     EBELN    EQ  W_EBELN.
      IF W_COUNT EQ 0. MESSAGE E570. ENDIF.
    ENDIF.
    CASE W_COUNT.
      WHEN 0.
        MESSAGE E709 WITH W_ZFOPNNO.
      WHEN 1.
        SELECT SINGLE  *  FROM ZTREQHD
        WHERE  ZFOPNNO EQ W_ZFOPNNO.
        MOVE ZTREQHD-ZFREQNO TO W_ZFREQNO.
        MOVE ZTREQHD-EBELN   TO W_EBELN.
      WHEN OTHERS.
        MESSAGE E713 WITH W_ZFOPNNO.
    ENDCASE.
  ENDIF.
*>> 인수증 번호로 CHECK!
  IF W_EBELN IS INITIAL AND W_ZFREQNO IS INITIAL
                        AND W_ZFOPNNO IS INITIAL.

    IF ZTRED-ZFREDNO NE SPACE AND ZTRED-ZFISNO IS INITIAL.
      SELECT  SINGLE *
      FROM    ZTRED      WHERE ZFREDNO = ZTRED-ZFREDNO.
      IF SY-SUBRC NE 0.  MESSAGE  E854.  ENDIF.
    ELSEIF ZTRED-ZFREDNO IS INITIAL AND ZTRED-ZFISNO NE SPACE.
      SELECT  SINGLE *
      FROM    ZTRED      WHERE ZFISNO =  ZTRED-ZFISNO.
      IF SY-SUBRC NE 0.  MESSAGE E854.   ENDIF.
    ELSEIF ZTRED-ZFREDNO NE SPACE AND ZTRED-ZFISNO NE SPACE.
      SELECT  SINGLE *
      FROM    ZTRED
      WHERE   ZFISNO =  ZTRED-ZFISNO AND ZFREDNO = ZTRED-ZFREDNO.
      IF SY-SUBRC NE 0.  MESSAGE E854.   ENDIF.
    ENDIF.

    SELECT  SINGLE *
    FROM    ZTREQHD
    WHERE   EBELN   =  ZTRED-EBELN
    AND     ZFOPNNO =  ZTRED-ZFLLCON.
    MOVE    ZTREQHD-ZFREQNO  TO  W_ZFREQNO.

  ENDIF.

*>> 수입의뢰 번호로 DATA SELECT!
  SELECT SINGLE *  FROM  ZTREQHD  WHERE  ZFREQNO  =  W_ZFREQNO.
  MOVE  ZTREQHD-EBELN    TO  W_EBELN.
  MOVE  ZTREQHD-ZFOPNNO  TO  W_ZFOPNNO.
  MOVE  ZTREQHD-EBELN    TO  ZTPMTHD-EBELN.
  MOVE  ZTREQHD-ZFOPNNO  TO  ZTPMTHD-ZFOPNNO.
  MOVE  ZTREQHD-ZFREQNO  TO  ZTPMTHD-ZFREQNO.
  MOVE  ZTREQHD-BUKRS    TO  ZTPMTHD-BUKRS.
*  MOVE  'KRW'            TO  ZTPMTHD-ZFKRW.
  MOVE : ZTREQHD-WAERS   TO  ZTPMTHD-ZFPNAMC,
         ZTREQHD-WAERS   TO  ZTPMTHD-ZFUSITC,
         'N'             TO  ZTPMTHD-ZFPYA,
         SY-DATUM        TO  ZTPMTHD-ZFNTDT,
         SY-DATUM        TO  ZTPMTHD-ZFPYDT,
         SY-DATUM        TO  ZTPMTHD-NEGODT,
         'N'             TO  ZTPMTHD-ZFUSIRP.
* MKIM 2001.05.15 추?
*  IF ZTREQHD-ZFLCKN <> '2' AND ZTREQHD-ZFLCKN <> '3' AND
*     ZTREQHD-ZFLCKN <> 'G' .
*     MESSAGE E575.
*  ENDIF.

*> 문서상태 체크.
  CALL FUNCTION 'ZIM_IMPORT_DOC_STATUS_CHECK'
       EXPORTING
            ZFREQNO = W_ZFREQNO.

  SELECT SINGLE * FROM T001
                  WHERE BUKRS EQ ZTPMTHD-BUKRS.

  ZTPMTHD-ZFKRW = T001-WAERS.
  ZTPMTHD-ZFLCKN = ZTREQHD-ZFLCKN.
  CASE ZTPMTHD-ZFLCKN.
    WHEN '2' OR '3'.
      ZTPMTHD-ZFDHDOC = 'DISCHG'.
      ZTPMTHD-ZFBKCHC = ZTREQHD-WAERS.
      ZTPMTHD-ZFUSITC = ZTREQHD-WAERS.
    WHEN '8'.
      ZTPMTHD-ZFDHDOC = 'LDANTC'.
    WHEN OTHERS.
      ZTPMTHD-ZFDHDOC = 'DOANTC'.
      ZTPMTHD-ZFBKCHC = T001-WAERS.
  ENDCASE.


  CASE  ZTREQHD-ZFREQTY.
    WHEN  'DA'  OR  'DP'  OR  'TT' OR 'GS' OR 'LC'.
      PERFORM P1000_MOVE_REQHD_PMTHD_SCR8100.
      PERFORM READ_ZTIV_MLC_SCR8100.
    WHEN  'PU'.
      MESSAGE E991 WITH ZTREQHD-ZFOPNNO ZTREQHD-ZFREQTY.
    WHEN  'LO'.
      IF ZTRED-ZFISNO  IS INITIAL AND ZTRED-ZFREDNO IS INITIAL.
        SELECT  COUNT( DISTINCT ZFISNO )  INTO  W_COUNT
        FROM    ZTRED
        WHERE   EBELN   EQ   ZTPMTHD-EBELN
        AND     ZFLLCON EQ   ZTPMTHD-ZFOPNNO.
        CASE  W_COUNT.
          WHEN 0.
            MESSAGE E885 WITH W_ZFOPNNO.
          WHEN 1.
          WHEN OTHERS.
            MESSAGE E886 WITH W_ZFOPNNO.
        ENDCASE.
      ENDIF.
      SELECT  SINGLE  *   FROM ZTRED
      WHERE   EBELN   EQ  W_EBELN
      AND     ZFLLCON EQ  W_ZFOPNNO.

      MOVE  ZTRED-ZFISNO  TO  ZTPMTHD-ZFISNO.
      MOVE  'KRW'         TO  ZTPMTHD-ZFKRW.

      SELECT  SINGLE  *  FROM  ZTLLCHD
      WHERE   ZFREQNO EQ W_ZFREQNO.

      PERFORM P1000_MOVE_REQHD_PMTHD_SCR8100.
      PERFORM READ_ZTVTIV_LLC_SCR8100.
  ENDCASE.

  DESCRIBE  TABLE   IT_ZSPMTIV  LINES  LINE.
  IF  LINE  EQ  0.  MESSAGE  E925.  ENDIF.

ENDMODULE.                             " P1000_ZTIV_READ_SCR8100  INPUT

*&---------------------------------------------------------------------*
*&      Form  OK_CODE_PATM
*&---------------------------------------------------------------------*
FORM OK_CODE_PATM.

*  IF OK-CODE EQ 'CRDC'.
*     LEAVE TO TRANSACTION 'ZIMP2'.
* ENDIF.
* IF OK-CODE EQ 'CHDC'.
*   LEAVE TO TRANSACTION 'ZIMP3'.
* ENDIF.
* IF OK-CODE EQ 'DISP'.
*   LEAVE TO TRANSACTION 'ZIMP4'.
* ENDIF.
* IF OK-CODE EQ 'DELE'.
*    PERFORM P3000_ZTPMTHD_DELETE_SCR8110.
* ENDIF.
  IF  ( OK-CODE EQ 'DSRQ' ) OR ( OK-CODE EQ 'DSLC' ).
    IF ZTPMTHD-ZFREQNO IS INITIAL.
      MESSAGE S019.    EXIT.
    ENDIF.
    SET PARAMETER ID 'ZPREQNO' FIELD ZTPMTHD-ZFREQNO.
    SET PARAMETER ID 'ZPOPNNO' FIELD ''.
    SET PARAMETER ID 'BES'     FIELD ''.

    CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
  ENDIF.

  IF OK-CODE EQ 'DSPO'.
    IF ZTPMTHD-EBELN IS INITIAL.
      MESSAGE S003.   EXIT.
    ENDIF.
    SET PARAMETER ID 'BES' FIELD ZTPMTHD-EBELN.
    CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.
  ENDIF.
  IF OK-CODE EQ 'DSBL'.

    IF ZTPMTHD-ZFHBLNO IS INITIAL.
      MESSAGE S897.    EXIT.
    ENDIF.
    SET PARAMETER ID 'ZPHBLNO'  FIELD ZTPMTHD-ZFHBLNO.
    SET PARAMETER ID 'ZPBLNO'   FIELD ''.
    CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
  ENDIF.

  IF OK-CODE EQ 'DSRE'.
    IF ZTRED-ZFREDNO IS INITIAL.
      EXIT.
    ENDIF.
    SELECT MAX( ZFREDNO )  INTO ZTRED-ZFREDNO FROM ZTRED
        WHERE ZFISNO = ZTPMTHD-ZFISNO.

    SET PARAMETER ID 'ZPREDNO'  FIELD ZTRED-ZFREDNO.
    CALL TRANSACTION 'ZIMA7' AND SKIP  FIRST SCREEN.
  ENDIF.

  IF OK-CODE EQ 'DSFI'.
    IF ZTPMTHD-BELNR IS INITIAL. EXIT. ENDIF.
    SET PARAMETER ID 'BUK'  FIELD ZTPMTHD-BUKRS.
    SET PARAMETER ID 'BLN'  FIELD ZTPMTHD-BELNR.
    SET PARAMETER ID 'GJR'  FIELD ZTPMTHD-GJAHR.
    CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
  ENDIF.
  IF OK-CODE EQ 'DSTR'.
    IF ZTPMTHD-BELNR_TR IS INITIAL. EXIT. ENDIF.
    SET PARAMETER ID 'BUK'  FIELD ZTPMTHD-BUKRS.
    SET PARAMETER ID 'BLN'  FIELD ZTPMTHD-BELNR_TR.
    SET PARAMETER ID 'GJR'  FIELD ZTPMTHD-GJAHR_TR.
    CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
  ENDIF.
  IF OK-CODE EQ 'DSPY'.
    IF ZTPMTHD-ZFPMNO IS INITIAL. EXIT. ENDIF.
    SET PARAMETER ID 'BUK'  FIELD ZTPMTHD-BUKRS.
    SET PARAMETER ID 'BLN'  FIELD ZTPMTHD-ZFPMNO.
    SET PARAMETER ID 'GJR'  FIELD ZTPMTHD-ZFPMYR.
    CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
  ENDIF.

  IF OK-CODE EQ 'PSHT'.    ">전기이력.
    LEAVE TO SCREEN 8213.
  ENDIF.

ENDFORM.                               " OK_CODE_PATM
*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR2110  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR8210 INPUT.

  PERFORM OK_CODE_PATM.

ENDMODULE.                             " AFI_PROCESS_SCR8210  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR8212  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR8212 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_8212-CURRENT_LINE + LINE - 1.

ENDMODULE.                             " GET_LINE_SCR8212  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZTVT_READ_FOR_SCR8500_SCR8600  INPUT
*&---------------------------------------------------------------------*
MODULE ZTVT_READ_FOR_SCR8500_SCR8600 INPUT.

  PERFORM OK_CODE_TXCA.
  PERFORM OK_CODE_BACK_EXIT.

  IF ZTVT-ZFVTNO IS INITIAL AND ZTVT-ZFVTSEQ IS INITIAL AND
     ZTVT-ZFDOCNO IS INITIAL.
    MESSAGE E830.
  ENDIF.

  IF NOT ( ZTVT-ZFVTSEQ IS INITIAL ) AND ZTVT-ZFVTNO IS INITIAL.
    SELECT MAX( ZFVTNO ) INTO ZTVT-ZFVTNO
      FROM ZTVT
     WHERE ZFVTSEQ = ZTVT-ZFVTSEQ.
  ENDIF.
  IF ZTVT-ZFVTNO IS INITIAL AND ZTVT-ZFVTSEQ IS INITIAL AND
      NOT ( ZTVT-ZFDOCNO IS INITIAL ).
    SELECT MAX( ZFVTNO ) INTO ZTVT-ZFVTNO
      FROM ZTVT
     WHERE ZFDOCNO = ZTVT-ZFDOCNO.
  ENDIF.

  SELECT SINGLE *
    FROM ZTVT
   WHERE ZFVTNO = ZTVT-ZFVTNO.
  IF SY-SUBRC NE 0.
    MESSAGE E831.
  ENDIF.

  IF SY-DYNNR = '8500'.
    IF ZTVT-ZFEDIST NE 'N' OR ZTVT-ZFVTRYN NE 'N'.
      MESSAGE E832.
    ENDIF.
    SELECT COUNT( DISTINCT ZFREDNO ) INTO W_COUNT
      FROM ZTRED
     WHERE ZFVTNO = ZTVT-ZFVTNO.
    IF W_COUNT > 0.
      MESSAGE E858.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM ZTVTSG1
   WHERE ZFVTNO = ZTVT-ZFVTNO.

  REFRESH IT_ZSVTSG3.
  SELECT *
    FROM ZTVTSG3
   WHERE ZFVTNO = ZTVT-ZFVTNO.
    CLEAR IT_ZSVTSG3.
    MOVE-CORRESPONDING ZTVTSG3 TO IT_ZSVTSG3.
    APPEND IT_ZSVTSG3.
  ENDSELECT.


ENDMODULE.                 " ZTVT_READ_FOR_SCR8500_SCR8600  INPUT

*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR8510  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR8510 INPUT.

  PERFORM OK_CODE_TXCA.

ENDMODULE.                             " AFI_PROCESS_SCR8510  INPUT

*&---------------------------------------------------------------------*
*&      Form  OK_CODE_TXCA
*&---------------------------------------------------------------------*
FORM OK_CODE_TXCA.

  IF OK-CODE EQ 'CHDC'.
    LEAVE TO TRANSACTION 'ZIMA3'.
  ENDIF.
  IF OK-CODE EQ 'DISP'.
    LEAVE TO TRANSACTION 'ZIMA4'.
  ENDIF.
  IF OK-CODE EQ 'DELE'.
    PERFORM P3000_ZTVT_DELETE_SCR8510.
  ENDIF.

ENDFORM.                               " OK_CODE_TXCA

*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR7410  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR7410 INPUT.

  PERFORM OK_CODE_IMDS.

ENDMODULE.                             " AFI_PROCESS_SCR7410  INPUT
*&---------------------------------------------------------------------*
*&      Form  OK_CODE_IMDS
*&---------------------------------------------------------------------*
FORM OK_CODE_IMDS.

  IF OK-CODE EQ 'CRDC'.
    LEAVE TO TRANSACTION 'ZIM74'.
  ENDIF.
  IF OK-CODE EQ 'CHDC'.
    LEAVE TO TRANSACTION 'ZIM75'.
  ENDIF.
  IF OK-CODE EQ 'DISP'.
    LEAVE TO TRANSACTION 'ZIM76'.
  ENDIF.
  IF OK-CODE EQ 'DELE'.
    PERFORM P3000_ZTIDS_DELETE_SCR7410.
  ENDIF.
  IF OK-CODE EQ 'DSBL'.
    SET PARAMETER ID 'ZPBLNO'  FIELD ZTIDS-ZFBLNO.
    EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
    CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
  ENDIF.

ENDFORM.                               " OK_CODE_IMDS
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIDS_DELETE_SCR7410
*&---------------------------------------------------------------------*
FORM P3000_ZTIDS_DELETE_SCR7410.

  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  IF ANTWORT NE 'Y'.
    EXIT.
  ENDIF.
*>> 통관비용.
  W_LINE = 0.
  SELECT COUNT( * ) INTO W_COUNT
         FROM ZTCUCLCST
         WHERE ZFBLNO   = ZTIDS-ZFBLNO
         AND   ZFCLSEQ  = ZTIDS-ZFCLSEQ
         AND ( ZFACDO   IS NOT NULL
         OR    ZFACDO   NE SPACE ).
  IF W_COUNT GT 0.
    MESSAGE E519 WITH ZTIDS-ZFBLNO ZTIDS-ZFCLSEQ W_COUNT.
  ENDIF.

  DELETE FROM ZTIDSHSL WHERE ZFBLNO = ZTIDS-ZFBLNO       " 요?
                         AND ZFCLSEQ = ZTIDS-ZFCLSEQ.
  DELETE FROM ZTIDSHSD WHERE ZFBLNO = ZTIDS-ZFBLNO       " 규격(행)
                         AND ZFCLSEQ = ZTIDS-ZFCLSEQ.
  DELETE FROM ZTIDSHS  WHERE ZFBLNO = ZTIDS-ZFBLNO       " ?
                         AND ZFCLSEQ = ZTIDS-ZFCLSEQ.
  DELETE FROM ZTIDS    WHERE ZFBLNO = ZTIDS-ZFBLNO       " 면?
                         AND ZFCLSEQ = ZTIDS-ZFCLSEQ.
  DELETE FROM ZTCUCLCST WHERE ZFBLNO = ZTIDS-ZFBLNO      " 통관비?
                         AND ZFCLSEQ = ZTIDS-ZFCLSEQ.
  CLEAR W_ZFBTSEQ.
  SELECT MAX( ZFBTSEQ ) INTO W_ZFBTSEQ
    FROM ZTBLOUR
   WHERE ZFBLNO = ZTIDS-ZFBLNO
     AND ZFOUTY = '50'.   "반출유?
  DELETE FROM ZTBLOUR
   WHERE ZFBLNO = ZTIDS-ZFBLNO
     AND ZFBTSEQ EQ W_ZFBTSEQ.
* 감정가격,면허일,환율,관세,통관수수료...
  UPDATE ZTCUCL SET ZFIDRAMU = 0    ZFIDRAM = 0    ZFIDSDT = '        '
                    ZFCUST = '3'    ZFEXRT = 0
                    UNAM = SY-UNAME UDAT = SY-DATUM
         WHERE ZFBLNO = ZTIDS-ZFBLNO
           AND ZFCLSEQ = ZTIDS-ZFCLSEQ.
  IF SY-SUBRC NE 0.
    MESSAGE E776.
    SET SCREEN 7400.
    LEAVE SCREEN.
  ENDIF.
*
  SELECT *                             " 통관 Invoice
    FROM ZTCUCLIV
   WHERE ZFBLNO = ZTIDS-ZFBLNO
     AND ZFCLSEQ = ZTIDS-ZFCLSEQ.
    CLEAR ZTCUCLIV-ZFIVAMK.
    MOVE SY-UNAME TO ZTCUCLIV-UNAM.
    MOVE SY-DATUM TO ZTCUCLIV-UDAT.
    MOVE '3'      TO ZTCUCLIV-ZFCUST.
    UPDATE ZTCUCLIV.
    IF SY-SUBRC NE 0.
      MESSAGE E776.
      SET SCREEN 7400.
      LEAVE SCREEN.
    ENDIF.
    SELECT SINGLE *
      FROM ZTIV
     WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
    IF SY-SUBRC EQ 0.
      CLEAR : ZTIV-ZFEXRT, ZTCUCLIV-ZFIVAMK.
      MOVE SY-UNAME TO ZTIV-UNAM.
      MOVE SY-DATUM TO ZTIV-UDAT.
* 01/01/15 김연중.
      IF ZTIV-ZFPOYN = 'N'. "무환.
        MOVE 'N' TO ZTIV-ZFCDST.
*         MOVE 'N' TO ZTIV-ZFIVST.
      ENDIF.
*
      MOVE '3'      TO ZTIV-ZFCUST.
      UPDATE ZTIV.
      IF SY-SUBRC NE 0.
        MESSAGE E776.
        SET SCREEN 7400.
        LEAVE SCREEN.
      ENDIF.
    ENDIF.
  ENDSELECT.
*
  DELETE FROM ZTIDRCR   WHERE ZFBLNO = ZTIDS-ZFBLNO       " 감면허?
                          AND ZFCLSEQ = ZTIDS-ZFCLSEQ.
  DELETE FROM ZTIDRCRIT WHERE ZFBLNO = ZTIDS-ZFBLNO       " 감면허가품?
                          AND ZFCLSEQ = ZTIDS-ZFCLSEQ.
  MESSAGE S756.

  SET SCREEN 7500.
  LEAVE SCREEN.

ENDFORM.                               " P3000_ZTIDS_DELETE_SCR7410

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTVT_DELETE_SCR8510
*&---------------------------------------------------------------------*
FORM P3000_ZTVT_DELETE_SCR8510.

  SELECT COUNT( DISTINCT ZFREDNO ) INTO W_COUNT
    FROM ZTRED
   WHERE ZFVTNO = ZTVT-ZFVTNO.
  IF W_COUNT > 0.
    MESSAGE E858.
    EXIT.
  ENDIF.

  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  IF ANTWORT NE 'Y'.
    EXIT.
  ENDIF.

  DELETE FROM ZTVT WHERE ZFVTNO = ZTVT-ZFVTNO.      " 세금계산?
  DELETE FROM ZTVTSG1 WHERE ZFVTNO = ZTVT-ZFVTNO.  " 세금계산서 Seg1
  DELETE FROM ZTVTSG3 WHERE ZFVTNO = ZTVT-ZFVTNO. " 세금계산서 Seg3
  UPDATE ZTVTIV SET ZFVTNO = '          '
                    UNAM = SY-UNAME
                    UDAT = SY-DATUM
         WHERE ZFVTNO = ZTVT-ZFVTNO.
  IF SY-SUBRC NE 0.
    MESSAGE E833.
    SET SCREEN 8500.
    LEAVE SCREEN.
  ENDIF.
  MESSAGE S756.

  SET SCREEN 8500.
  LEAVE SCREEN.

ENDFORM.                               " P3000_ZTVT_DELETE_SCR8510

*&---------------------------------------------------------------------*
*&      Module  TC_8514_UPDATE_SCR8514  INPUT
*&---------------------------------------------------------------------*
MODULE TC_8514_UPDATE_SCR8514 INPUT.

  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  READ TABLE IT_ZSVTSG3 INDEX TC_8514-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.

  MOVE-CORRESPONDING ZSVTSG3 TO IT_ZSVTSG3.
  MOVE W_ROW_MARK TO IT_ZSVTSG3-ZFMARK.

  IF W_OLD_SUBRC = 0.
    MODIFY IT_ZSVTSG3   INDEX TC_8514-CURRENT_LINE.
  ELSE.
    APPEND IT_ZSVTSG3.
  ENDIF.

ENDMODULE.                             " TC_8514_UPDATE_SCR8514  INPUT

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTVT_MODIFY_SCR8500
*&---------------------------------------------------------------------*
FORM P3000_ZTVT_MODIFY_SCR8500.

  IF W_OK_CODE EQ 'DELE'.
*>> 물품내역 DELETE.
    DELETE FROM ZTVTSG3  WHERE  ZFVTNO EQ ZTVT-ZFVTNO.
*>> 공급자 내역 DELETE.
    DELETE FROM ZTVTSG1  WHERE  ZFVTNO EQ ZTVT-ZFVTNO.
*>> 세금계산서 DELETE.
    DELETE FROM ZTVT     WHERE  ZFVTNO EQ ZTVT-ZFVTNO.
*>> 세금계산서번호 SPACE.
    UPDATE  ZTVTIV
    SET     ZFVTNO  =  SPACE
            UNAM    =  SY-UNAME
            UDAT    =  SY-DATUM
    WHERE   ZFVTNO  EQ   ZTVT-ZFVTNO.
  ELSE.
    PERFORM P3000_VATBIL_EDI_CHECK.
*>> 세금계산서 UPDATE.
    MOVE SY-UNAME TO ZTVT-UNAM.
    MOVE SY-DATUM TO ZTVT-UDAT.
    UPDATE  ZTVT.
    IF SY-SUBRC NE 0. MESSAGE E835. ENDIF.

*>> 공급자 내역 UPDATE.
    UPDATE  ZTVTSG1.
    IF SY-SUBRC NE 0. MESSAGE E835. ENDIF.

*>> 물품내역 UPDATE.
    LOOP  AT  IT_ZSVTSG3.
      CLEAR  ZTVTSG3.
      MOVE-CORRESPONDING IT_ZSVTSG3  TO  ZTVTSG3.
      UPDATE ZTVTSG3.
      IF SY-SUBRC NE 0. MESSAGE E835. ENDIF.
    ENDLOOP.
  ENDIF.

*  DELETE FROM ZTVTSG3
*  WHERE ZFVTNO = ZTVTSG3-ZFVTNO.
* UPDATE ZTVT.
* UPDATE ZTVTSG1.
*
* LOOP AT IT_ZSVTSG3.
*   CLEAR : ZTVTSG3.
*   MOVE-CORRESPONDING IT_ZSVTSG3   TO ZTVTSG3.
*   MOVE ZTVT-ZFVTNO                TO ZTVTSG3-ZFVTNO.
*   SELECT MAX( ZFLSG3 ) INTO W_ZFLSG3
*     FROM ZTVTSG3
*    WHERE ZFVTNO = ZTVT-ZFVTNO.
*   ADD 1           TO W_ZFLSG3.
*   MOVE W_ZFLSG3   TO ZTVTSG3-ZFLSG3.
*   INSERT   ZTVTSG3.
*   IF SY-SUBRC NE 0.
*     MESSAGE  E835.
*     EXIT.
*   ENDIF.
* ENDLOOP.

  SET PARAMETER ID 'ZPVTNO' FIELD ZTVT-ZFVTNO.
  MESSAGE  S834.

ENDFORM.                               " P3000_ZTVT_MODIFY_SCR8500

*&---------------------------------------------------------------------*
*&      Module  TC_8212_UPDATE_SCR8212  INPUT
*&---------------------------------------------------------------------*
MODULE TC_8212_UPDATE_SCR8212 INPUT.

  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  READ TABLE IT_ZSPMTIV INDEX TC_8212-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.

  MOVE-CORRESPONDING ZSPMTIV TO IT_ZSPMTIV.
  IF OK-CODE = 'MKA1'.
    MOVE   'X'             TO W_ROW_MARK.
  ENDIF.
  IF OK-CODE = 'MKL1'.
    CLEAR  W_ROW_MARK.
  ENDIF.
  MOVE W_ROW_MARK TO IT_ZSPMTIV-ZFMARK.

  IF W_OLD_SUBRC = 0.
    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSPMTIV-ZFPNAM
                                            IT_ZSPMTIV-ZFIVAMC.
    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSPMTIV-ZFPBAMT
                                            IT_ZSPMTIV-ZFIVAMC.
    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSPMTIV-ZFIVAMT
                                            IT_ZSPMTIV-ZFIVAMC.
    MODIFY IT_ZSPMTIV   INDEX TC_8212-CURRENT_LINE.
  ENDIF.

  IF IT_ZSPMTIV-ZFPNAM > IT_ZSPMTIV-ZFPBAMT.
    MESSAGE E848.
    EXIT.
  ENDIF.

ENDMODULE.                             " TC_8212_UPDATE_SCR8212  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8210  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8210 INPUT.
  MOVE OK-CODE TO W_OK_CODE.
  CASE OK-CODE.
    WHEN 'REF1'.
      IF  W_STATUS  =  C_REQ_C.
        PERFORM  READ_ZTIV_MLC_SCR8100.
      ELSE.
        PERFORM  READ_PMTIV_TO_IT.
      ENDIF.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE' OR 'SVCO'.
      MOVE OK-CODE TO W_OK_CODE.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'DELE'.
      PERFORM  P2000_SET_INDICATE.
    WHEN 'OTDC' OR 'CRDC' OR 'CHDC' OR 'DISP' OR 'ADDC' OR 'OPDC' OR
         'DCSD' OR 'DCRP'.
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'TRTR' OR 'TRTI' OR 'TRCL' OR 'TRCC' OR 'PYMT' OR 'PYCN'.
      PERFORM  P2000_SET_INDICATE.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN OTHERS.
      IF W_STATUS NE 'D'.
        MOVE ZTPMTHD-ZFPNAMC TO ZTPMTHD-ZFSCUR.
*>>>KSB CHANGE
*         ZTPMTHD-ZFSAMT = ZTPMTHD-ZFPNAM
*                        - ZTPMTHD-ZFLAMT12 - ZTPMTHD-ZFLAMT22
*                        - ZTPMTHD-ZFLNAMT1 - ZTPMTHD-ZFLNAMT2.

        REFRESH IT_ZSPMTIV_DEL.
        CLEAR   IT_ZSPMTIV_DEL.
        LOOP AT IT_ZSPMTIV.
          IF OK-CODE = 'MKA1'.
            MOVE   'X'             TO IT_ZSPMTIV-ZFMARK.
          ENDIF.
          IF OK-CODE = 'MKL1'.
            CLEAR  IT_ZSPMTIV-ZFMARK.
          ENDIF.
          IF OK-CODE = 'DEL1' AND IT_ZSPMTIV-ZFMARK = 'X'.
          ELSE.
            MOVE-CORRESPONDING IT_ZSPMTIV TO IT_ZSPMTIV_DEL.
            APPEND IT_ZSPMTIV_DEL.
          ENDIF.
        ENDLOOP.

        REFRESH IT_ZSPMTIV.
        DESCRIBE TABLE IT_ZSPMTIV_DEL LINES LINE.

        CLEAR W_ZFTIVAM.
        IF LINE > 0.
          LOOP AT IT_ZSPMTIV_DEL.
            CLEAR  IT_ZSPMTIV.
            MOVE-CORRESPONDING IT_ZSPMTIV_DEL TO IT_ZSPMTIV.
            ADD    IT_ZSPMTIV-ZFPNAM          TO W_ZFTIVAM.
            IF IT_ZSPMTIV-ZFPNAM < IT_ZSPMTIV-ZFIVAMT.
              MOVE 'Y' TO IT_ZSPMTIV-ZFPPYYN.
            ELSE.
              MOVE 'N' TO IT_ZSPMTIV-ZFPPYYN.
            ENDIF.
            APPEND IT_ZSPMTIV.
          ENDLOOP.
        ENDIF.
        MOVE W_ZFTIVAM TO ZTPMTHD-ZFTIVAM.
        IF LINE > 0.
          IF ZTPMTHD-ZFTIVAM NE ZTPMTHD-ZFPNAM.
            MESSAGE W847.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.                           "IF W_STATUS NE 'D'.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR8210  INPUT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIV_MLC_SCR8100
*&---------------------------------------------------------------------*
FORM READ_ZTIV_MLC_SCR8100.

  REFRESH IT_ZSPMTIV.
  CLEAR : W_ZFPNIT, W_ZFTIVAM, ZTBL, ZTCIVHD.
*MKIM변경 2001.05.03
* SELECT ZFCIVRN AS ZFIVNO ZFBLNO SUM( ZFIVAMT ) AS ZFIVAMT "Invoice
*        SUM( ZFIVAMP ) AS ZFPBAMT "Payable
*        MAX( ZFIVAMC ) AS ZFIVAMC
* INTO   CORRESPONDING FIELDS OF TABLE IT_ZSPMTIV
* FROM   ZTCIVIT
* WHERE  ZFREQNO  EQ  ZTPMTHD-ZFREQNO
* GROUP BY
*        ZFCIVRN  ZFBLNO.

  SELECT ZFCIVRN            ZFIVAMT ZFIVAMP AS ZFPBAMT ZFIVAMC
  INTO   CORRESPONDING FIELDS OF TABLE IT_ZSPMTIV
  FROM   ZTCIVHD
  WHERE  ZFIVST  EQ 'Y'
  AND    ZFCIVRN IN ( SELECT DISTINCT ZFCIVRN
                             FROM     ZTCIVIT
                             WHERE    ZFREQNO EQ ZTPMTHD-ZFREQNO ).


*MKIM막음 2001.05.03
*  SELECT ZFCIVRN AS ZFIVNO ZFBLNO SUM( ZFIVAMP ) AS ZFIVAMT
*         MAX( ZFIVAMC ) AS ZFIVAMC
*  INTO   CORRESPONDING FIELDS OF TABLE IT_ZSPMTIV
*  FROM   ZTCIVIT
*  WHERE  ZFREQNO  EQ  ZTPMTHD-ZFREQNO
*  GROUP BY
*         ZFCIVRN  ZFBLNO.

  CLEAR : W_COUNT, W_SEQ.
  LOOP AT IT_ZSPMTIV.
    W_TABIX   =  SY-TABIX.
*>> B/L NO SELECT.2001/10/10 KSB INSERT
    MOVE : IT_ZSPMTIV-ZFCIVRN TO IT_ZSPMTIV-ZFIVNO.

    SELECT ZFBLNO INTO IT_ZSPMTIV-ZFBLNO
           FROM   ZTCIVIT UP TO 1 ROWS
           WHERE  ZFCIVRN  EQ  IT_ZSPMTIV-ZFCIVRN.
    ENDSELECT.

*>> COMMERCIAL INVOICE 관련 정보 GET
    SELECT  SINGLE *     FROM  ZTCIVHD
    WHERE   ZFCIVRN      EQ    IT_ZSPMTIV-ZFIVNO.
    IF ZTCIVHD-ZFIVST NE 'Y'.
      DELETE  IT_ZSPMTIV  INDEX  W_TABIX.
      CONTINUE.
    ENDIF.

*>> COMMERCIAL INVOICE HISTORY 정보 GET
    SELECT  SINGLE *     FROM  ZTCIVHST
    WHERE   ZFCIVRN      EQ    IT_ZSPMTIV-ZFIVNO
    AND     ZFCIVHST     EQ    ( SELECT MAX( ZFCIVHST )
                                 FROM   ZTCIVHST
                                 WHERE  ZFCIVRN EQ IT_ZSPMTIV-ZFIVNO ).
*>> 회계 전표 번호 DISPLAY.
    IF  ZTCIVHST-SHKZG  =  'S' AND  ZTCIVHST-CBELNR EQ SPACE.
      MOVE  ZTCIVHST-GJAHR  TO  IT_ZSPMTIV-ZFGFDYR.
      MOVE  ZTCIVHST-BELNR  TO  IT_ZSPMTIV-ZFGFDNO.
    ENDIF.

    IF IT_ZSPMTIV-ZFGFDNO IS INITIAL.
      DELETE  IT_ZSPMTIV  INDEX  W_TABIX.
      CONTINUE.
    ENDIF.

*>> 기처리 데이타 누락시킴.... 2001/07/20 KSB INSERT
    SELECT COUNT( * ) INTO W_COUNT
           FROM  ZTPMTIV
           WHERE ZFGFDNO  EQ IT_ZSPMTIV-ZFGFDNO
           AND   ZFGFDYR  EQ IT_ZSPMTIV-ZFGFDYR.
    IF W_COUNT GT 0.
      DELETE  IT_ZSPMTIV  INDEX  W_TABIX.
      CONTINUE.
    ENDIF.

*>> BL 정보 관련 정보 GET
    IF IT_ZSPMTIV-ZFBLNO  NE  SPACE.
      SELECT  SINGLE *     FROM  ZTBL
              WHERE   ZFBLNO       EQ    IT_ZSPMTIV-ZFBLNO.
      W_COUNT   =  W_COUNT  +  1.
      W_ZFHBLNO =  ZTBL-ZFHBLNO.
      W_ZFBLNO  =  ZTBL-ZFBLNO.
    ENDIF.

*>> 처리금액 계산.
    SELECT  SUM( ZFPNAM )  INTO  W_ZFPNAM  "IT_ZSPMTIV-ZFPBAMT
    FROM    ZTPMTIV        WHERE ZFIVNO   =  IT_ZSPMTIV-ZFIVNO
                             AND ZFBLNO   =  IT_ZSPMTIV-ZFBLNO."MKIM
*MKIM 2001.05.03막?
*   IF  ZTCIVHD-ZFIVAMP  >  IT_ZSPMTIV-ZFPBAMT.
*       IT_ZSPMTIV-ZFPNAM  =  ZTCIVHD-ZFIVAMP - IT_ZSPMTIV-ZFPBAMT.
*   ENDIF.

*MKIM 2001.05.03 만?
    IF  IT_ZSPMTIV-ZFIVAMT > W_ZFPNAM.
      IT_ZSPMTIV-ZFPNAM  =  IT_ZSPMTIV-ZFPBAMT - W_ZFPNAM.
    ENDIF.

    MOVE   ZTCIVHD-ZFCIVNO     TO  IT_ZSPMTIV-ZFCIVNO.
    MOVE   ZTBL-ZFHBLNO        TO  IT_ZSPMTIV-ZFHBLNO.
    MOVE   ZTBL-ZFBLNO         TO  IT_ZSPMTIV-ZFBLNO.
    MOVE   ZTBL-ZFETA          TO  IT_ZSPMTIV-ZFETA.

    IF IT_ZSPMTIV-ZFPNAM < IT_ZSPMTIV-ZFIVAMT. " Partial Payment
      MOVE 'Y' TO IT_ZSPMTIV-ZFPPYYN.
    ELSE.
      MOVE 'N' TO IT_ZSPMTIV-ZFPPYYN.
    ENDIF.

    W_ZFTIVAM  =  W_ZFTIVAM  +  IT_ZSPMTIV-ZFPNAM.
    IF  IT_ZSPMTIV-ZFPNAM  EQ  0.
      DELETE  IT_ZSPMTIV  INDEX  W_TABIX.
      CONTINUE.
    ENDIF.

    W_SEQ     =  W_SEQ  +  10.
    MOVE   W_SEQ               TO  IT_ZSPMTIV-ZFPNIT.

    MODIFY  IT_ZSPMTIV  INDEX  W_TABIX.
  ENDLOOP.

  IF  W_COUNT  EQ  1.
    MOVE  W_ZFHBLNO       TO  ZTPMTHD-ZFHBLNO.
    MOVE  W_ZFBLNO        TO  ZTPMTHD-ZFBLNO.
*>> 환율 정?
    MOVE  ZTCIVHD-ZFEXRT  TO  ZTPMTHD-ZFEXRT.
    MOVE  ZTCIVHD-FFACT   TO  ZTPMTHD-FFACT.
  ENDIF.

  MOVE IT_ZSPMTIV-ZFIVAMC TO ZTPMTHD-ZFPNAMC.
  MOVE IT_ZSPMTIV-ZFIVAMC TO ZTPMTHD-ZFUSITC.
  MOVE IT_ZSPMTIV-ZFIVAMC TO ZTPMTHD-ZFBKCHC.
  MOVE W_ZFTIVAM          TO ZTPMTHD-ZFTIVAM.

ENDFORM.                               " READ_ZTIV_MLC_SCR8100

*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR8210  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR8210 INPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZTPMTHD-ZTERM'.
      EXIT.
    ENDIF.
  ENDLOOP.

*  IF SCREEN-INPUT EQ '1'.
  CALL FUNCTION 'FI_F4_ZTERM'
       EXPORTING
            I_KOART       = 'K'
            I_ZTERM       = ZTPMTHD-ZTERM
            I_XSHOW       = ' '
       IMPORTING
            E_ZTERM       = T052-ZTERM
       EXCEPTIONS
            NOTHING_FOUND = 01.
*  ELSE.
*    CALL FUNCTION 'FI_F4_ZTERM'
*         EXPORTING
*              I_KOART       = 'K'
*              I_ZTERM       = ZTPMTHD-ZTERM
*              I_XSHOW       = 'X'
*         IMPORTING
*              E_ZTERM       = T052-ZTERM
*         EXCEPTIONS
*              NOTHING_FOUND = 01.
*  ENDIF.

  IF SY-SUBRC NE 0.
    MESSAGE S177(06) WITH ZTPMTHD-ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    ZTPMTHD-ZTERM = T052-ZTERM.
  ENDIF.

ENDMODULE.                             " HELP_ZTERM_SCR8210  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_PAYMENT_TERM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_PAYMENT_TERM INPUT.

  IF W_STATUS = 'D'. EXIT. ENDIF.
  CLEAR W_COUNT.
  SELECT COUNT( DISTINCT ZTERM ) INTO   W_COUNT
  FROM   T052
  WHERE  ZTERM = ZTPMTHD-ZTERM.
  IF W_COUNT = 0.  MESSAGE E844.  ENDIF.

  CLEAR ZTIMIMG01.
  SELECT SINGLE *
  FROM   ZTIMIMG01
  WHERE  ZTERM = ZTPMTHD-ZTERM.
  IF SY-SUBRC = 0.
    MOVE ZTIMIMG01-ZFLCKN TO ZTPMTHD-ZFLCKN. " L/C Type
  ELSE.
    MOVE '9'              TO ZTPMTHD-ZFLCKN. " L/C Type
  ENDIF.

ENDMODULE.                             " CHECK_PAYMENT_TERM  INPUT

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTPMTHD_MODIFY_SCR8210
*&---------------------------------------------------------------------*
FORM P3000_ZTPMTHD_MODIFY_SCR8210.

  IF W_OK_CODE NE 'DELE'.
*>> NOTICE AMOUNT, INTEREST, BANKING CHARGE 셋중에 하나라도 입력.
    CLEAR  W_TOT_AMT.
    W_TOT_AMT  =  ZTPMTHD-ZFPNAM  +  ZTPMTHD-ZFUSIT + ZTPMTHD-ZFBKCH.
    IF  W_TOT_AMT  EQ  0.
      MESSAGE  E481.
    ENDIF.

*>> NOTICE AMOUNT 입력된 경우 CHECK!
    IF ZTPMTHD-ZFPNAM  NE 0.
      IF ZTPMTHD-ZFPNAMC  EQ  SPACE.
        MESSAGE  E482  WITH 'Notice Amount'.
      ENDIF.
      IF ZTPMTHD-ZFPNAM NE ZTPMTHD-ZFTIVAM.
        MESSAGE  E847.
      ENDIF.
    ENDIF.

*>> USANCE INTEREST 입력된 경우 CHECK!
    IF ZTPMTHD-ZFUSIT  NE  0.
      IF ZTPMTHD-ZFUSITC  EQ  SPACE.
        MESSAGE  E482 WITH 'Usance Interest'.
      ENDIF.
      IF ZTPMTHD-ZFUSIRP  EQ  SPACE.
        MESSAGE  E483 WITH 'Usance Interest'.
      ENDIF.
      IF ZTPMTHD-ZFUSIRDT EQ  SPACE.
        MESSAGE  E484 WITH 'Usance Interest'.
      ENDIF.
      IF ZTPMTHD-ZFUSITR  IS INITIAL.
        MESSAGE  E485 WITH 'Usance Interest'.
      ENDIF.
    ENDIF.

*>> BANKING CHARGE 입력된 경우 CHECK!
    IF ZTPMTHD-ZFBKCH  NE  0.
      IF ZTPMTHD-ZFBKCHC  EQ  SPACE.
        MESSAGE  E482  WITH 'Banking Charge'.
      ENDIF.
      IF ZTPMTHD-ZFBKCHP  EQ  SPACE.
        MESSAGE  E483  WITH 'Banking Charge'.
      ENDIF.
      IF ZTPMTHD-ZFBKCHDT EQ  SPACE.
        MESSAGE  E484  WITH 'Banking Charge'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF  W_STATUS = 'C'.
*      PERFORM   P2000_GET_NUMBER_NEXT  USING  'PN'  ZTPMTHD-ZFPNNO.
*      SET PARAMETER ID 'ZPPNNO' FIELD ZTPMTHD-ZFPNNO.

*      SELECT MAX( ZFPNNO ) INTO ZTPMTHD-ZFPNNO      " 관리번?
*      FROM   ZTPMTHD.
*      IF ZTPMTHD-ZFPNNO < '1000000000'.
*         MOVE '1000000000' TO ZTPMTHD-ZFPNNO.
*      ENDIF.
*      ADD   1       TO ZTPMTHD-ZFPNNO.
    MOVE 'N'      TO ZTPMTHD-ZFPMYN.
    MOVE 'N'      TO ZTPMTHD-ZFBKCHT.
    MOVE 'N'      TO ZTPMTHD-ZFPYST.
    MOVE 'N'      TO ZTPMTHD-ZFBKCHA.
    MOVE 'N'      TO ZTPMTHD-ZFPYT.
    MOVE 'N'      TO ZTPMTHD-ZFPYL.
    MOVE 'N'      TO ZTPMTHD-ZFPYA.
  ENDIF.

  IF W_OK_CODE = 'SVCO'.
    MOVE 'C'  TO  ZTPMTHD-ZFPYST.
    IF ZTPMTHD-ZFLCKN = '8'. "Local L/C

      CLEAR ZTLLCHD.
      SELECT SINGLE *  FROM   ZTLLCHD
      WHERE  ZFREQNO = ZTPMTHD-ZFREQNO.

      IF ZTLLCHD-ZFLLCTY = '2AA' OR ZTLLCHD-ZFLLCTY = '2AB'.
        IF ZTPMTHD-ZFTIVAM NE ZTPMTHD-ZFPNAM.
          MESSAGE  W888.
          MOVE  'N'   TO  ZTPMTHD-ZFPYST.
        ENDIF.
      ELSEIF ZTLLCHD-ZFLLCTY = '2AC'.
        IF ZTPMTHD-ZFTIVAM  NE  ZTPMTHD-ZFTIVAMK.
          MESSAGE  W888.
          MOVE 'N'    TO  ZTPMTHD-ZFPYST.
        ENDIF.
      ENDIF.
    ELSE. " Local L/C ?
      IF ZTPMTHD-ZFTIVAM NE ZTPMTHD-ZFPNAM.
        MESSAGE W888.
        MOVE 'N'  TO  ZTPMTHD-ZFPYST.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT IT_ZSPMTIV.
    MOVE ZTPMTHD-ZFPNNO             TO ZTPMTIV-ZFPNNO.
    SELECT MAX( ZFPNIT ) INTO W_ZFPNIT " 일련번?
    FROM   ZTPMTIV
    WHERE  ZFPNNO = ZTPMTHD-ZFPNNO.
    IF  IT_ZSPMTIV-ZFPNIT  IS  INITIAL.
      ADD   10       TO W_ZFPNIT.
      MOVE  W_ZFPNIT TO ZTPMTIV-ZFPNIT.
    ENDIF.
  ENDLOOP.

  " 지급반제 처리.
  IF W_OK_CODE  EQ 'PYMT' OR W_OK_CODE EQ 'PYCN'.
    IF W_OK_CODE EQ 'PYMT'.
      CALL FUNCTION 'ZIM_GOODS_AP_PAYMENT_POST'
           EXPORTING
                ZFPNNO     = ZTPMTHD-ZFPNNO
                ZFBLDT     = ZTPMTHD-ZFBLDT
                ZFBUDT     = ZTPMTHD-ZFBUDT
                BLART      = 'KZ'
           TABLES
                RETURN     = RETURN
           EXCEPTIONS
                POST_ERROR = 4.
    ELSE.
      CALL FUNCTION 'ZIM_GOODS_AP_PAYMENT_CANCEL'
           EXPORTING
                ZFPNNO     = ZTPMTHD-ZFPNNO
           TABLES
                RETURN     = RETURN
           EXCEPTIONS
                POST_ERROR = 4.
    ENDIF.
    IF SY-SUBRC EQ 0.
      READ TABLE RETURN INDEX 1.
      MESSAGE ID     RETURN-ID
              TYPE   'S'
              NUMBER RETURN-NUMBER
              WITH   RETURN-MESSAGE_V1
                     RETURN-MESSAGE_V2
                     RETURN-MESSAGE_V3
                     RETURN-MESSAGE_V4.
      EXIT.
    ELSE.
      IF NOT RETURN[] IS INITIAL.
        LOOP AT RETURN WHERE TYPE EQ 'E'.
        ENDLOOP.
        IF SY-SUBRC EQ 0.
          MESSAGE ID     RETURN-ID
                  TYPE   'S'
                  NUMBER RETURN-NUMBER
                  WITH   RETURN-MESSAGE_V1
                         RETURN-MESSAGE_V2
                         RETURN-MESSAGE_V3
                         RETURN-MESSAGE_V4.
        ELSE.
          DESCRIBE TABLE RETURN LINES W_LINE.
          W_LINE = W_LINE - 1.
          IF W_LINE LE 0.
            MESSAGE S944.   EXIT.
          ENDIF.
          READ TABLE RETURN INDEX W_LINE.
          IF SY-SUBRC NE 0.
            MESSAGE S944.   EXIT.
          ENDIF.
          MESSAGE ID     RETURN-ID
                  TYPE   'S'
                  NUMBER RETURN-NUMBER
                  WITH   RETURN-MESSAGE_V1
                         RETURN-MESSAGE_V2
                         RETURN-MESSAGE_V3
                         RETURN-MESSAGE_V4.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE S944.
        LEAVE TO TRANSACTION 'ZIMP4' AND SKIP FIRST SCREEN.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ( W_OK_CODE = 'TRTR' OR W_OK_CODE = 'TRCL' OR
       W_OK_CODE = 'TRTI' OR W_OK_CODE = 'TRCC' OR
       W_OK_CODE = 'SVCO' OR W_OK_CODE = 'SAVE' OR
       W_OK_CODE = 'DELE' )                    AND
       W_STATUS NE C_REQ_D.

    IF W_STATUS EQ C_REQ_C.
      PERFORM   P2000_GET_NUMBER_NEXT  USING  'PN'  ZTPMTHD-ZFPNNO.
      SET PARAMETER ID 'ZPPNNO' FIELD ZTPMTHD-ZFPNNO.
    ENDIF.

    CALL FUNCTION 'ZIM_PM_DOC_MODIFY'
         EXPORTING
              ZFPNNO         = ZTPMTHD-ZFPNNO
              ZFSTATUS       = W_STATUS
              W_ZTPMTHD_OLD  = *ZTPMTHD
              W_ZTPMTHD      = ZTPMTHD
              W_OK_CODE      = W_OK_CODE
         TABLES
              IT_ZSPMTIV_OLD = IT_ZSPMTIV_ORG
              IT_ZSPMTIV     = IT_ZSPMTIV
         EXCEPTIONS
              ERROR_INSERT   = 1
              ERROR_UPDATE   = 2
              ERROR_DELETE   = 3.
  ENDIF.

  IF SY-SUBRC EQ  0.
    IF W_OK_CODE NE 'DELE'.
      SET PARAMETER ID 'ZPPNNO' FIELD ZTPMTHD-ZFPNNO.
      IF W_OK_CODE = 'TRTR' OR W_OK_CODE = 'TRCL' OR
         W_OK_CODE = 'TRTI' OR W_OK_CODE = 'TRCC'.
        CASE W_OK_CODE.
          WHEN 'TRTR'.                  "> Payment Notice A/P Repayment.
            CALL FUNCTION 'ZIM_GOODS_AP_REPAYMENT_POST'
                 EXPORTING
                      ZFPNNO     = ZTPMTHD-ZFPNNO
                      BLDAT      = ZTPMTHD-BLDAT
                      BUDAT      = ZTPMTHD-BUDAT
                      BLART      = ZTPMTHD-BLART
                 TABLES
                      RETURN     = RETURN
                 EXCEPTIONS
                      POST_ERROR = 4.
          WHEN 'TRTI'.                 "> Interest Accounting Doc.
            CALL FUNCTION 'ZIM_CHARGE_INTR_POST'
                 EXPORTING
                      ZFPNNO     = ZTPMTHD-ZFPNNO
                      BLART      = 'RE'
                      MODE       = 'N'
                 TABLES
                      RETURN     = RETURN
                 EXCEPTIONS
                      POST_ERROR = 4.
          WHEN 'TRCL'.                "> A/P Repayment Cancel.
            CALL FUNCTION 'ZIM_GOODS_AP_REPAYMENT_CANCEL'
                 EXPORTING
                      ZFPNNO     = ZTPMTHD-ZFPNNO
                 TABLES
                      RETURN     = RETURN
                 EXCEPTIONS
                      POST_ERROR = 4.
          WHEN 'TRCC'.                "> Cancel Interest Accounting Doc.
            Call Function 'ZIM_CHARGE_INTR_CANCEL'
                 EXPORTING
                      ZFPNNO         = ZTPMTHD-ZFPNNO
                      POSTINGDATE    = BSIS-BUDAT
                      REASONREVERSAL = UF05A-STGRD
                 TABLES
                      RETURN     = RETURN
                 EXCEPTIONS
                      POST_ERROR = 4.
          WHEN OTHERS.
        ENDCASE.
        IF SY-SUBRC EQ 0.
          READ TABLE RETURN INDEX 1.
          MESSAGE ID     RETURN-ID
                  TYPE   'S'
                  NUMBER RETURN-NUMBER
                  WITH   RETURN-MESSAGE_V1
                         RETURN-MESSAGE_V2
                         RETURN-MESSAGE_V3
                         RETURN-MESSAGE_V4.
          EXIT.
        ELSE.
          IF NOT RETURN[] IS INITIAL.
*                 READ TABLE RETURN WITH KEY TYPE = 'E'.
            LOOP AT RETURN WHERE TYPE EQ 'E'.
            ENDLOOP.
            IF SY-SUBRC EQ 0.
              MESSAGE ID     RETURN-ID
                      TYPE   'S'
                      NUMBER RETURN-NUMBER
                      WITH   RETURN-MESSAGE_V1
                             RETURN-MESSAGE_V2
                             RETURN-MESSAGE_V3
                             RETURN-MESSAGE_V4.
            ELSE.
              DESCRIBE TABLE RETURN LINES W_LINE.
              W_LINE = W_LINE - 1.
              IF W_LINE LE 0.
                MESSAGE S944.   EXIT.
              ENDIF.
              READ TABLE RETURN INDEX W_LINE.
              IF SY-SUBRC NE 0.
                MESSAGE S944.   EXIT.
              ENDIF.
              MESSAGE ID     RETURN-ID
                      TYPE   'S'
                      NUMBER RETURN-NUMBER
                      WITH   RETURN-MESSAGE_V1
                             RETURN-MESSAGE_V2
                             RETURN-MESSAGE_V3
                             RETURN-MESSAGE_V4.
              EXIT.
            ENDIF.
          ELSE.
            MESSAGE S944.
            LEAVE TO TRANSACTION 'ZIMP4' AND SKIP FIRST SCREEN.
            EXIT.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE  S846  WITH  ZTPMTHD-ZFPNNO.
      ENDIF.
    ELSE.
      MESSAGE  S756.
    ENDIF.
  ELSE.
    MESSAGE  E845.
  ENDIF.

ENDFORM.                               " P3000_ZTPMTHD_MODIFY_SCR8210
*&---------------------------------------------------------------------*
*&      Module  ZTRED_READ_FOR_SCR8700_SCR8800  INPUT
*&---------------------------------------------------------------------*
MODULE ZTRED_READ_FOR_SCR8700_SCR8800 INPUT.

  PERFORM OK_CODE_REDO.
  PERFORM OK_CODE_BACK_EXIT.

  IF ZTRED-EBELN   IS INITIAL AND
     ZTRED-ZFREDNO IS INITIAL AND ZTRED-ZFISNO IS INITIAL AND
     ZTRED-ZFDOCNO IS INITIAL.
    MESSAGE E853.
  ENDIF.

  IF NOT ZTRED-EBELN IS INITIAL.
    CLEAR : W_COUNT.
    REFRESH : IT_ZFREDNO.
    SELECT DISTINCT ZFREDNO INTO IT_ZFREDNO-ZFREDNO
           FROM  ZTRED
           WHERE EBELN EQ ZTRED-EBELN.
      ADD 1 TO W_COUNT.
      APPEND IT_ZFREDNO.
    ENDSELECT.
    CASE W_COUNT.
      WHEN 0.   MESSAGE S423(ZIM1) WITH ZTRED-EBELN.
      WHEN 1.
        READ TABLE IT_ZFREDNO INDEX 1.
        MOVE:IT_ZFREDNO-ZFREDNO TO ZTRED-ZFREDNO.
      WHEN 2.   MESSAGE S424(ZIM1) WITH ZTRED-EBELN W_COUNT.
    ENDCASE.
  ENDIF.

  IF NOT ( ZTRED-ZFISNO IS INITIAL ) AND ZTRED-ZFREDNO IS INITIAL.
    CLEAR : W_COUNT.
    REFRESH : IT_ZFREDNO.
    SELECT DISTINCT ZFREDNO INTO IT_ZFREDNO-ZFREDNO
           FROM  ZTRED
           WHERE ZFISNO = ZTRED-ZFISNO.
      ADD 1 TO W_COUNT.
      APPEND IT_ZFREDNO.
    ENDSELECT.
    CASE W_COUNT.
      WHEN 0.   MESSAGE S425(ZIM1) WITH ZTRED-ZFISNO.
      WHEN 1.
        READ TABLE IT_ZFREDNO INDEX 1.
        MOVE:IT_ZFREDNO-ZFREDNO TO ZTRED-ZFREDNO.
      WHEN 2.   MESSAGE S424(ZIM1) WITH ZTRED-ZFISNO W_COUNT.
    ENDCASE.
  ENDIF.

  IF ZTRED-ZFREDNO IS INITIAL AND ZTRED-ZFISNO IS INITIAL AND
      NOT ( ZTRED-ZFDOCNO IS INITIAL ).
    SELECT MAX( ZFREDNO ) INTO ZTRED-ZFREDNO
      FROM ZTRED
     WHERE ZFDOCNO = ZTRED-ZFDOCNO.
  ENDIF.

*> 인수증 관리번호가 존재할 경우.
  CHECK NOT ZTRED-ZFREDNO IS INITIAL.

  SELECT SINGLE *
    FROM ZTRED
   WHERE ZFREDNO = ZTRED-ZFREDNO.
  IF SY-SUBRC NE 0.
    MESSAGE E854.
  ENDIF.

  IF SY-DYNNR = '8700'.
    IF ZTRED-ZFEDIST NE 'N'.
      MESSAGE E855.
    ENDIF.
  ENDIF.

  REFRESH IT_ZSREDSG1.
  SELECT *
    FROM ZTREDSG1
   WHERE ZFREDNO = ZTRED-ZFREDNO.
    CLEAR IT_ZSREDSG1.
    MOVE-CORRESPONDING ZTREDSG1 TO IT_ZSREDSG1.
    APPEND IT_ZSREDSG1.
  ENDSELECT.

  MESSAGE S607(ZIM1).

ENDMODULE.                 " ZTRED_READ_FOR_SCR8700_SCR8800  INPUT

*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR8710  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR8710 INPUT.

  PERFORM OK_CODE_REDO.

ENDMODULE.                             " AFI_PROCESS_SCR8710  INPUT

*&---------------------------------------------------------------------*
*&      Form  OK_CODE_REDO
*&---------------------------------------------------------------------*
FORM OK_CODE_REDO.

  IF OK-CODE EQ 'CHDC'.
    LEAVE TO TRANSACTION 'ZIMA6'.
  ENDIF.
  IF OK-CODE EQ 'DISP'.
    LEAVE TO TRANSACTION 'ZIMA7'.
  ENDIF.
  IF OK-CODE EQ 'DELE'.
    PERFORM P3000_ZTRED_DELETE_SCR8710.
  ENDIF.

ENDFORM.                               " OK_CODE_REDO

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTRED_DELETE_SCR8710
*&---------------------------------------------------------------------*
FORM P3000_ZTRED_DELETE_SCR8710.

  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  IF ANTWORT NE 'Y'.
    EXIT.
  ENDIF.

  DELETE FROM ZTRED WHERE ZFREDNO = ZTRED-ZFREDNO.  " 인수?
  DELETE FROM ZTREDSG1 WHERE ZFREDNO = ZTRED-ZFREDNO. " 인수증 Seg1
  UPDATE ZTVTIV SET ZFREDNO = '          '
                    UNAM = SY-UNAME
                    UDAT = SY-DATUM
         WHERE ZFREDNO = ZTRED-ZFREDNO.
  IF SY-SUBRC NE 0.
    MESSAGE E833.
    SET SCREEN 8700.
    LEAVE SCREEN.
  ENDIF.
  MESSAGE S756.

  SET SCREEN 8700.
  LEAVE SCREEN.

ENDFORM.                               " P3000_ZTRED_DELETE_SCR8710

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTRED_MODIFY_SCR8710
*&---------------------------------------------------------------------*
FORM P3000_ZTRED_MODIFY_SCR8710.

  DELETE FROM ZTREDSG1
         WHERE ZFREDNO = ZTRED-ZFREDNO.

  IF W_OK_CODE EQ 'DELE'.
*> IMG 체크 ( 2001/10/08 KSB INSERT )
    IF ZTIMIMG00-ZFTAXYN IS INITIAL.
      DELETE FROM ZTVT
             WHERE ZFVTNO IN
                 ( SELECT ZFVTNO FROM ZTVTIV
                          WHERE ZFREDNO = ZTRED-ZFREDNO ).

      DELETE FROM ZTVTSG1
             WHERE ZFVTNO IN
                 ( SELECT ZFVTNO FROM ZTVTIV
                          WHERE ZFREDNO = ZTRED-ZFREDNO ).

      DELETE FROM ZTVTSG3
             WHERE ZFVTNO IN
                 ( SELECT ZFVTNO FROM ZTVTIV
                          WHERE ZFREDNO = ZTRED-ZFREDNO ).

    ENDIF.

    DELETE FROM ZTRED
           WHERE ZFREDNO = ZTRED-ZFREDNO.  " 인수증.

    DELETE FROM ZTREDSG1
           WHERE ZFREDNO = ZTRED-ZFREDNO.  " 인수증 Seg1

    IF ZTIMIMG00-ZFTAXYN IS INITIAL.

      UPDATE ZTVTIV SET   ZFVTNO  = SPACE
                          ZFREDNO = SPACE
                          UNAM    = SY-UNAME
                          UDAT    = SY-DATUM
                    WHERE ZFREDNO = ZTRED-ZFREDNO.
    ELSE.
      UPDATE ZTVTIV SET   ZFREDNO = SPACE
                          UNAM    = SY-UNAME
                          UDAT    = SY-DATUM
                    WHERE ZFREDNO = ZTRED-ZFREDNO.
    ENDIF.

    MESSAGE S339.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSREDSG1.
    CLEAR : ZTREDSG1.
    MOVE-CORRESPONDING IT_ZSREDSG1   TO ZTREDSG1.
    MOVE ZTRED-ZFREDNO               TO ZTREDSG1-ZFREDNO.
    INSERT  ZTREDSG1.
    IF SY-SUBRC NE 0.
      MESSAGE  E852.
      EXIT.
    ENDIF.
  ENDLOOP.
* 20000/10/02 강석?
  SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ ZTRED-LIFNR.

  PERFORM P3000_LOCRCT_EDI_CHECK.
  MOVE SY-UNAME TO ZTRED-UNAM.
  MOVE SY-DATUM TO ZTRED-UDAT.
  UPDATE ZTRED.

  MESSAGE  S851.

ENDFORM.                               " P3000_ZTRED_MODIFY_SCR8710

*&---------------------------------------------------------------------*
*&      Form  READ_MSEG_TO_IT
*&---------------------------------------------------------------------*
FORM READ_MSEG_TO_IT.

  CLEAR   W_ZFIVAMT_S.
  SELECT *
    FROM MSEG
   WHERE MBLNR = MSEG-MBLNR
     AND MJAHR = MSEG-MJAHR.
    CLEAR IT_ZSIVIT.
    MOVE MSEG-EBELP            TO IT_ZSIVIT-ZFIVDNO.
    MOVE MSEG-MATNR            TO IT_ZSIVIT-MATNR.
    MOVE MSEG-MENGE            TO IT_ZSIVIT-CCMENGE.
    MOVE MSEG-MENGE            TO IT_ZSIVIT-MENGE1.
    MOVE MSEG-MEINS            TO IT_ZSIVIT-MEINS.
    CLEAR EKPO.
    SELECT SINGLE *
      FROM EKPO
     WHERE EBELN = MSEG-EBELN
       AND EBELP = MSEG-EBELP.
    MOVE EKPO-TXZ01            TO IT_ZSIVIT-TXZ01.
    MOVE EKPO-NETPR            TO IT_ZSIVIT-NETPR.
    MOVE EKPO-PEINH            TO IT_ZSIVIT-PEINH.
    MOVE EKPO-BPRME            TO IT_ZSIVIT-BPRME.
*>>>KSB CHANGE
*   MOVE EKPO-ZZHSCODE         TO IT_ZSIVIT-STAWN.

    IT_ZSIVIT-ZFIVAMT =
         MSEG-MENGE * EKPO-NETPR / EKPO-PEINH.
    ADD  IT_ZSIVIT-ZFIVAMT     TO W_ZFIVAMT_S.

    CLEAR EKKO.
    SELECT SINGLE *
      FROM EKKO
     WHERE EBELN = MSEG-EBELN.
    MOVE EKKO-WAERS            TO IT_ZSIVIT-ZFIVAMC.
    MOVE EKKO-WAERS            TO ZTIV-ZFIVAMC.
    APPEND IT_ZSIVIT.
  ENDSELECT.
  MOVE W_ZFIVAMT_S TO ZTIV-ZFIVAMT.

ENDFORM.                               " READ_MSEG_TO_IT

*&---------------------------------------------------------------------*
*&      Form  P3000_VATBIL_EDI_CHECK
*&---------------------------------------------------------------------*
FORM P3000_VATBIL_EDI_CHECK.

  MOVE 'O'    TO ZTVT-ZFEDICK.         " EDI Check
  IF ZTVTSG1-ZFTXN1 IS INITIAL.        " 공급자 사업자등록번?
    MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFCONM1 IS INITIAL.       " 공급자 상?
    MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFCHNM1 IS INITIAL.       " 공급자 대표자?
    MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFADD11 IS INITIAL.       " 공급자 주소 1
    MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFTXN2 IS INITIAL.        " 공급받는자 사업자등록번?
    MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFCONM2 IS INITIAL.       " 공급받는자 상?
    MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFCHNM2 IS INITIAL.       " 공급받는자 대표자?
    MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFADD12 IS INITIAL.       " 공급받는자 주소 1
    MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVT-ZFVCDT IS INITIAL.           " 세금계산서 작성?
    MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVT-ZFVERC IS INITIAL.           " 공란?
    MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVT-ZFVTAMT IS INITIAL.          " 공급가?
    MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  CLEAR LFA1.
  SELECT SINGLE *
    FROM LFA1
   WHERE LIFNR = ZTVT-LIFNR.
  IF LFA1-BAHNS IS INITIAL.            " EDI 식별?
    MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.

ENDFORM.                               " P3000_VATBIL_EDI_CHECK

*&---------------------------------------------------------------------*
*&      Module  ARRIVE_READ_IMG03_SCR7110  INPUT
*&---------------------------------------------------------------------*
MODULE ARRIVE_READ_IMG03_SCR7110 INPUT.

  PERFORM P1000_GET_BONDED_NAME USING      ZTBLUG-ZFBNARCD
                                CHANGING   ZTBLINOU-ZFABNAR " 불필?
                                           W_ZFBNARCD_NM.

ENDMODULE.                 " ARRIVE_READ_IMG03_SCR7110  INPUT

*&---------------------------------------------------------------------*
*&      Module  VALUE_ZFCUT_SCR6212  INPUT
*&---------------------------------------------------------------------*
MODULE VALUE_ZFCUT_SCR6212 INPUT.

  SELECT ZTIMIMG10~ZFCUT ZTIMIMG10~ZFVEN LFA1~NAME1
    INTO CORRESPONDING FIELDS OF TABLE IT_ZFCUT_TAB
    FROM ZTIMIMG10 INNER JOIN LFA1
    ON  ZTIMIMG10~ZFVEN = LFA1~LIFNR.

  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.
  WINDOW_TITLE = '관세사 조회 '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'OTYPE'
            DYNPPROG        = DYNPROG
            DYNPNR          = DYNNR
            DYNPROFIELD     = 'ZTIDR-ZFCUT'
            WINDOW_TITLE    = WINDOW_TITLE
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = IT_ZFCUT_TAB
       EXCEPTIONS
            PARAMETER_ERROR = 1
            NO_VALUES_FOUND = 2
            OTHERS          = 3.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

ENDMODULE.                             " VALUE_ZFCUT_SCR6212  INPUT

*&---------------------------------------------------------------------*
*&      Module  VALUE_ZFCUT_SCR7412  INPUT
*&---------------------------------------------------------------------*
MODULE VALUE_ZFCUT_SCR7412 INPUT.

  SELECT ZTIMIMG10~ZFCUT ZTIMIMG10~ZFVEN LFA1~NAME1
    INTO CORRESPONDING FIELDS OF TABLE IT_ZFCUT_TAB
    FROM ZTIMIMG10 INNER JOIN LFA1
    ON  ZTIMIMG10~ZFVEN = LFA1~LIFNR.

  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.
  WINDOW_TITLE = '관세사 조회 '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'OTYPE'
            DYNPPROG        = DYNPROG
            DYNPNR          = DYNNR
            DYNPROFIELD     = 'ZTIDS-ZFCUT'
            WINDOW_TITLE    = WINDOW_TITLE
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = IT_ZFCUT_TAB
       EXCEPTIONS
            PARAMETER_ERROR = 1
            NO_VALUES_FOUND = 2
            OTHERS          = 3.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

ENDMODULE.                             " VALUE_ZFCUT_SCR7412  INPUT

*&---------------------------------------------------------------------*
*&      Form  P1000_MOVE_REQHD_PMTHD_SCR8100
*&---------------------------------------------------------------------*
FORM P1000_MOVE_REQHD_PMTHD_SCR8100.

  MOVE ZTREQHD-ZTERM TO ZTPMTHD-ZTERM. " Payment Term
  CLEAR ZTIMIMG01.
  SELECT SINGLE  * FROM ZTIMIMG01
  WHERE  ZTERM   EQ ZTPMTHD-ZTERM
  AND    ZFAPLDT LT SY-DATUM
  AND    ZFAPLDT <= ( SELECT MAX( ZFAPLDT )  FROM  ZTIMIMG01
                     WHERE   ZTERM  =  ZTPMTHD-ZTERM ).

  IF SY-SUBRC = 0.
    MOVE ZTIMIMG01-ZFLCKN TO ZTPMTHD-ZFLCKN. " L/C Type
  ELSE.
    MOVE '9'              TO ZTPMTHD-ZFLCKN. " L/C Type
  ENDIF.
*>> 2001.04.12 나현주 추가. USANCE, GSM 일 경우 DATA SET.
  IF ZTPMTHD-ZFLCKN = 'G'.
    MOVE  '3'          TO  ZTPMTHD-SRHYTHM.
    MOVE  '6'          TO  ZTPMTHD-ARHYTM.
  ELSEIF ZTPMTHD-ZFLCKN = '2' OR ZTPMTHD-ZFLCKN = '3'.
    MOVE  '1'          TO  ZTPMTHD-SRHYTHM.
    CLEAR  ZTPMTHD-ARHYTM.
  ENDIF.

  MOVE ZTREQHD-ZFJEWGB TO ZTPMTHD-ZFJEWGB. " 재원구?
*>> 결제집계 구분 SET
  MOVE '5' TO ZTPMTHD-ZFTRTY.
  IF ZTREQHD-ZFMATGB = '1' OR
     ZTREQHD-ZFMATGB = '3' OR
     ZTREQHD-ZFMATGB = '5'.
    MOVE '1' TO ZTPMTHD-ZFTRTY.
  ENDIF.
  IF ZTREQHD-ZFMATGB = '4'.
    MOVE '2' TO ZTPMTHD-ZFTRTY.
  ENDIF.
  IF ZTREQHD-ZTERM = 'TT01'.
    MOVE '3' TO ZTPMTHD-ZFTRTY.
  ENDIF.
  IF ZTREQHD-ZTERM = 'TT02'.
    MOVE '4' TO ZTPMTHD-ZFTRTY.
  ENDIF.

  MOVE  ZTREQHD-BUKRS   TO  ZTPMTHD-BUKRS.
*>> Benificiary 정보.
  MOVE ZTREQHD-ZFBENI TO ZTPMTHD-ZFBENI. " Benificiary
  IF NOT ( ZTPMTHD-ZFBENI IS INITIAL ).
    SELECT SINGLE NAME1 INTO  W_ZFBENI_NM
    FROM   LFA1         WHERE LIFNR = ZTPMTHD-ZFBENI.
  ENDIF.
*>> 개설은행 정보.
  MOVE ZTREQHD-ZFOPBN TO ZTPMTHD-ZFOPBN. " 개설은?
  IF NOT ( ZTPMTHD-ZFOPBN IS INITIAL ).
    SELECT SINGLE NAME1 INTO  W_ZFOPBN_NM
    FROM   LFA1         WHERE LIFNR = ZTPMTHD-ZFOPBN.
    SELECT MAX( BANKL ) INTO  ZTPMTHD-ZFOPBNK
    FROM   LFBK         WHERE LIFNR = ZTPMTHD-ZFOPBN.
  ENDIF.
*>> 차입기간 정보.
  MOVE ZTREQHD-ZFLEVN TO ZTPMTHD-ZFLEVN. " 차입기?
  IF NOT ( ZTPMTHD-ZFLEVN IS INITIAL ).
    SELECT SINGLE NAME1 INTO  W_ZFLEVN_NM
    FROM   LFA1         WHERE LIFNR = ZTPMTHD-ZFLEVN.
    SELECT MAX( BANKL ) INTO  ZTPMTHD-ZFLEVNK
    FROM   LFBK         WHERE LIFNR = ZTPMTHD-ZFLEVN.
  ENDIF.
*>> 통지은행 정보.
  MOVE ZTREQHD-ZFOPBN TO ZTPMTHD-ZFPNBN. " 개설은?
  IF NOT ZTPMTHD-ZFPNBN IS INITIAL.
    SELECT SINGLE NAME1 INTO  W_ZFPNBN_NM
    FROM   LFA1         WHERE LIFNR = ZTPMTHD-ZFPNBN.
    SELECT MAX( BANKL ) INTO  ZTPMTHD-ZFPNBNK
    FROM   LFBK         WHERE LIFNR = ZTPMTHD-ZFPNBN.
  ENDIF.

ENDFORM.                               " P1000_MOVE_REQHD_PMTHD_SCR8100

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8212  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8212 INPUT.

  CASE OK-CODE.
    WHEN 'REF1'.
      PERFORM  READ_ZTIV_MLC_SCR8100.
    WHEN OTHERS.
      IF W_STATUS NE 'D'.
        MOVE ZTPMTHD-ZFPNAMC TO ZTPMTHD-ZFSCUR.
*>>>KSB CHANGE
*         ZTPMTHD-ZFSAMT = ZTPMTHD-ZFPNAM
*                        - ZTPMTHD-ZFLAMT12 - ZTPMTHD-ZFLAMT22
*                        - ZTPMTHD-ZFLNAMT1 - ZTPMTHD-ZFLNAMT2.

        REFRESH IT_ZSPMTIV_DEL.
        CLEAR   IT_ZSPMTIV_DEL.
        LOOP AT IT_ZSPMTIV.
          IF OK-CODE = 'MKA1'.
            MOVE   'X'             TO IT_ZSPMTIV-ZFMARK.
          ENDIF.
          IF OK-CODE = 'MKL1'.
            CLEAR  IT_ZSPMTIV-ZFMARK.
          ENDIF.
          IF OK-CODE = 'DEL1' AND IT_ZSPMTIV-ZFMARK = 'X'.
          ELSE.
            MOVE-CORRESPONDING IT_ZSPMTIV TO IT_ZSPMTIV_DEL.
            APPEND IT_ZSPMTIV_DEL.
          ENDIF.
        ENDLOOP.

        REFRESH IT_ZSPMTIV.

        DESCRIBE TABLE IT_ZSPMTIV_DEL LINES LINE.

        CLEAR W_ZFTIVAM.
        IF LINE > 0.
          LOOP AT IT_ZSPMTIV_DEL.
            CLEAR  IT_ZSPMTIV.
            MOVE-CORRESPONDING IT_ZSPMTIV_DEL TO IT_ZSPMTIV.
            ADD    IT_ZSPMTIV-ZFPNAM          TO W_ZFTIVAM.
            IF IT_ZSPMTIV-ZFPNAM < IT_ZSPMTIV-ZFIVAMT.
              MOVE 'Y' TO IT_ZSPMTIV-ZFPPYYN.
            ELSE.
              MOVE 'N' TO IT_ZSPMTIV-ZFPPYYN.
            ENDIF.
            APPEND IT_ZSPMTIV.
          ENDLOOP.
        ENDIF.
        MOVE W_ZFTIVAM TO ZTPMTHD-ZFTIVAM.
        IF LINE > 0.
          IF ZTPMTHD-ZFTIVAM NE ZTPMTHD-ZFPNAM.
            MESSAGE W847.
            EXIT.
          ENDIF.
        ENDIF.

      ENDIF.                           "IF W_STATUS NE 'D'.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR8212  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8214  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8214 INPUT.

  IF W_STATUS EQ 'D'.
    EXIT.
  ENDIF.

  IF NOT ( ZTPMTHD-ZFRANL1 IS INITIAL ).                    " 차입1
    CLEAR VDARL.
    CLEAR W_SARCHIV.
    SELECT MAX( SARCHIV ) INTO W_SARCHIV
      FROM VDARL
     WHERE BUKRS = '1000'
       AND RANL = ZTPMTHD-ZFRANL1.
    SELECT SINGLE *
      FROM VDARL
     WHERE BUKRS = '1000'
       AND SARCHIV = W_SARCHIV
       AND RANL = ZTPMTHD-ZFRANL1.
    IF SY-SUBRC NE 0.
      MESSAGE E876.
      EXIT.
    ENDIF.
    MOVE VDARL-RANLALT1 TO ZTPMTHD-ZFLPER1. " 차입비?
    MOVE VDARL-SANTWHR  TO ZTPMTHD-ZFLCUR1.  " 차입통?
    IF ZTPMTHD-ZFLAMT12 IS INITIAL.    " 차입금액(결제통화)
      ZTPMTHD-ZFLAMT12 = ZTPMTHD-ZFPNAM * ZTPMTHD-ZFLPER1 / 100.
    ENDIF.
    IF ZTPMTHD-ZFLAMT11 IS INITIAL.    " 차입금액(차입통화)
      IF ZTPMTHD-ZFPNAMC = ZTPMTHD-ZFLCUR1.
        ZTPMTHD-ZFLAMT11 = ZTPMTHD-ZFLAMT12.
      ELSE.
        IF ZTPMTHD-ZFLRAT1 IS INITIAL.
*              PERFORM P1000_GET_CONRATIO USING ZTPMTHD-ZFPNAMC
*                                               ZTPMTHD-ZFLCUR1
*                                               ZTPMTHD-ZFLRAT1.
        ENDIF.
        IF ZTPMTHD-ZFLRAT1 NE 0.
          ZTPMTHD-ZFLAMT11 = ZTPMTHD-ZFLAMT12 / ZTPMTHD-ZFLRAT1.
          PERFORM P1000_AMT_CONV1 USING ZTPMTHD-ZFPNAMC
                                        ZTPMTHD-ZFLCUR1
                                        ZTPMTHD-ZFLAMT11.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR : ZTPMTHD-ZFLPER1, ZTPMTHD-ZFLCUR1, ZTPMTHD-ZFLAMT12,
            ZTPMTHD-ZFLAMT11, ZTPMTHD-ZFLRAT1.
  ENDIF.

  IF NOT ( ZTPMTHD-ZFRANL2 IS INITIAL ).                    " 차입2
    CLEAR VDARL.
    CLEAR W_SARCHIV.
    SELECT MAX( SARCHIV ) INTO W_SARCHIV
      FROM VDARL
     WHERE BUKRS = '1000'
       AND RANL = ZTPMTHD-ZFRANL2.
    SELECT SINGLE *
      FROM VDARL
     WHERE BUKRS = '1000'
       AND SARCHIV = W_SARCHIV
       AND RANL = ZTPMTHD-ZFRANL2.
    IF SY-SUBRC NE 0.
      MESSAGE E878.
      EXIT.
    ENDIF.
*>>>KSB CHANGE
    MOVE VDARL-RANLALT1 TO ZTPMTHD-ZFLPER2. " 차입비?
    MOVE VDARL-SANTWHR  TO ZTPMTHD-ZFLCUR2.  " 차입통?

    IF ZTPMTHD-ZFLAMT22 IS INITIAL.    " 차입금액(결제통화)
      ZTPMTHD-ZFLAMT22 = ZTPMTHD-ZFPNAM * ZTPMTHD-ZFLPER2 / 100.
    ENDIF.
    IF ZTPMTHD-ZFLAMT21 IS INITIAL.    " 차입금액(차입통화)
      IF ZTPMTHD-ZFPNAMC = ZTPMTHD-ZFLCUR2.
        ZTPMTHD-ZFLAMT21 = ZTPMTHD-ZFLAMT22.
      ELSE.
        IF ZTPMTHD-ZFLRAT2 NE 0.
          ZTPMTHD-ZFLAMT21 = ZTPMTHD-ZFLAMT22 / ZTPMTHD-ZFLRAT2.
          PERFORM P1000_AMT_CONV1 USING ZTPMTHD-ZFPNAMC
                                        ZTPMTHD-ZFLCUR2
                                        ZTPMTHD-ZFLAMT21.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR : ZTPMTHD-ZFLPER2, ZTPMTHD-ZFLCUR2, ZTPMTHD-ZFLAMT22,
            ZTPMTHD-ZFLAMT21, ZTPMTHD-ZFLRAT2.
  ENDIF.

*>>>KSB CHANGE
*  IF NOT ( ZTPMTHD-ZFLSNO1 IS INITIAL ). " Lease 1
*     CLEAR VDARL.
*     CLEAR W_SARCHIV.
*     SELECT MAX( SARCHIV ) INTO W_SARCHIV
*       FROM VDARL
*      WHERE BUKRS = '1000'
*        AND RANL = ZTPMTHD-ZFLSNO1.
*     SELECT SINGLE *
*       FROM VDARL
*      WHERE BUKRS = '1000'
*        AND SARCHIV = W_SARCHIV
*        AND RANL = ZTPMTHD-ZFLSNO1.
*     IF SY-SUBRC NE 0.
*        MESSAGE E879.
*        EXIT.
*     ENDIF.
*     MOVE VDARL-RANLALT1 TO ZTPMTHD-ZFLSLN1. " 차입비?
*     MOVE VDARL-SANTWHR  TO ZTPMTHD-ZFLSCUR1.  " 차입통?
*     IF ZTPMTHD-ZFLNAMT1 IS INITIAL.    " 차입금액(결제통화)
*        ZTPMTHD-ZFLNAMT1 = ZTPMTHD-ZFPNAM * ZTPMTHD-ZFLSLN1 / 100.
*     ENDIF.
*     IF ZTPMTHD-ZFLSAMT1 IS INITIAL.    " 차입금액(차입통화)
*        IF ZTPMTHD-ZFPNAMC = ZTPMTHD-ZFLSCUR1.
*           ZTPMTHD-ZFLSAMT1 = ZTPMTHD-ZFLNAMT1.
*        ELSE.
*           IF ZTPMTHD-ZFLSRT1 NE 0.
*              ZTPMTHD-ZFLSAMT1 = ZTPMTHD-ZFLNAMT1 / ZTPMTHD-ZFLSRT1.
*              PERFORM P1000_AMT_CONV1 USING ZTPMTHD-ZFPNAMC
*                                            ZTPMTHD-ZFLSCUR1
*                                            ZTPMTHD-ZFLSAMT1.
*           ENDIF.
*        ENDIF.
*     ENDIF.
*  ELSE.
*     CLEAR : ZTPMTHD-ZFLSLN1, ZTPMTHD-ZFLNAMT1, ZTPMTHD-ZFLSCUR1,
*             ZTPMTHD-ZFLSAMT1, ZTPMTHD-ZFLSRT1.
*  ENDIF.

*  IF NOT ( ZTPMTHD-ZFLSNO2 IS INITIAL ). " Lease 2
*     CLEAR VDARL.
*     CLEAR W_SARCHIV.
*     SELECT MAX( SARCHIV ) INTO W_SARCHIV
*       FROM VDARL
*      WHERE BUKRS = '1000'
*        AND RANL = ZTPMTHD-ZFLSNO2.
*     SELECT SINGLE *
*       FROM VDARL
*      WHERE BUKRS = '1000'
*        AND SARCHIV = W_SARCHIV
*        AND RANL = ZTPMTHD-ZFLSNO2.
*     IF SY-SUBRC NE 0.
*        MESSAGE E880.
*        EXIT.
*     ENDIF.
*
*     MOVE VDARL-RANLALT1 TO ZTPMTHD-ZFLSLN2. " 차입비?
*     MOVE VDARL-SANTWHR  TO ZTPMTHD-ZFLSCUR2.  " 차입통?
*     IF ZTPMTHD-ZFLNAMT2 IS INITIAL.    " 차입금액(결제통화)
*        ZTPMTHD-ZFLNAMT2 = ZTPMTHD-ZFPNAM * ZTPMTHD-ZFLSLN2 / 100.
*     ENDIF.
*     IF ZTPMTHD-ZFLSAMT2 IS INITIAL.    " 차입금액(차입통화)
*        IF ZTPMTHD-ZFPNAMC = ZTPMTHD-ZFLSCUR2.
*           ZTPMTHD-ZFLSAMT2 = ZTPMTHD-ZFLNAMT2.
*        ELSE.
*           IF ZTPMTHD-ZFLSRT2 NE 0.
*              ZTPMTHD-ZFLSAMT2 = ZTPMTHD-ZFLNAMT2 / ZTPMTHD-ZFLSRT2.
*              PERFORM P1000_AMT_CONV1 USING ZTPMTHD-ZFPNAMC
*                                            ZTPMTHD-ZFLSCUR2
*                                            ZTPMTHD-ZFLSAMT2.
*           ENDIF.
*        ENDIF.
*     ENDIF.
*  ELSE.
*     CLEAR : ZTPMTHD-ZFLSLN2, ZTPMTHD-ZFLNAMT2, ZTPMTHD-ZFLSCUR2,
*             ZTPMTHD-ZFLSAMT2, ZTPMTHD-ZFLSRT2.
*  ENDIF.

*  W_LOANRT = ZTPMTHD-ZFLPER1 + ZTPMTHD-ZFLPER2 +
*             ZTPMTHD-ZFLSLN1 + ZTPMTHD-ZFLSLN2.
*  IF W_LOANRT > 100.
*    MESSAGE E883.
*    EXIT.
*  ENDIF.
*  MOVE ZTPMTHD-ZFPNAMC TO ZTPMTHD-ZFSCUR.
*  ZTPMTHD-ZFSAMT = ZTPMTHD-ZFPNAM
*                 - ZTPMTHD-ZFLAMT12 - ZTPMTHD-ZFLAMT22
*                 - ZTPMTHD-ZFLNAMT1 - ZTPMTHD-ZFLNAMT2.

ENDMODULE.                             " USER_COMMAND_SCR8214  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZTPMTHD_READ_SCR8110_SCR8120  INPUT
*&---------------------------------------------------------------------*
MODULE ZTPMTHD_READ_SCR8110_SCR8120 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

*  PERFORM OK_CODE_PATM.
*  PERFORM OK_CODE_BACK_EXIT.

  IF ZTPMTHD-ZFPNNO IS INITIAL.
    MESSAGE E873.
  ENDIF.

  SELECT SINGLE * FROM ZTPMTHD
  WHERE  ZFPNNO = ZTPMTHD-ZFPNNO.
  IF SY-SUBRC NE 0. MESSAGE E874. ENDIF.

  IF SY-TCODE EQ 'ZIMP3'.
    IF ZTPMTHD-ZFPYA EQ 'Y'.
      MESSAGE E945.
    ENDIF.
  ELSE.
    IF ZTPMTHD-ZFPYA EQ 'Y'.
      MESSAGE S945.
    ENDIF.
  ENDIF.

  SELECT SINGLE NAME1 INTO W_ZFPNBN_NM
  FROM   LFA1
  WHERE  LIFNR = ZTPMTHD-ZFPNBN.

  IF NOT ( ZTPMTHD-ZFBENI IS INITIAL ).
    SELECT SINGLE NAME1 INTO W_ZFBENI_NM
    FROM   LFA1
    WHERE  LIFNR = ZTPMTHD-ZFBENI.
  ENDIF.

  IF NOT ( ZTPMTHD-ZFOPBN IS INITIAL ).
    SELECT SINGLE NAME1 INTO W_ZFOPBN_NM
    FROM   LFA1
    WHERE  LIFNR = ZTPMTHD-ZFOPBN.
  ENDIF.

  IF NOT ( ZTPMTHD-ZFLEVN IS INITIAL ).
    SELECT SINGLE NAME1 INTO W_ZFLEVN_NM
    FROM   LFA1
    WHERE  LIFNR = ZTPMTHD-ZFLEVN.
  ENDIF.

  IF SY-DYNNR = '8110'.
    IF ZTPMTHD-ZFPYST = 'C' AND ZTPMTHD-ZFPYT = 'N'.
      MESSAGE W875.
    ENDIF.
    IF ZTPMTHD-ZFPYST = 'C' AND ZTPMTHD-ZFPYT = 'Y'.
      MESSAGE E894.
    ENDIF.
  ENDIF.

   *ZTPMTHD  =  ZTPMTHD.
  PERFORM READ_PMTIV_TO_IT.            " Payment I/V -> Int

  SELECT SINGLE *
  FROM   ZTREQHD
  WHERE  ZFREQNO = ZTPMTHD-ZFREQNO.

  SELECT SINGLE *
  FROM   ZTLLCHD
  WHERE  ZFREQNO = ZTPMTHD-ZFREQNO.

  CLEAR : W_COUNT.
  W_COUNT  =  W_COUNT + 1.

*> 이력.
  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_ZSPMTHST
  FROM   ZTPMTHST
  WHERE  ZFPNNO  EQ  ZTPMTHD-ZFPNNO.

*>> 변경일 경우, LOCK OBJECT
  IF SY-TCODE EQ 'ZIMP3'.
    PERFORM   P2000_SET_ZTPMTHD_LOCK   USING   'L'.
  ENDIF.

ENDMODULE.                 " ZTPMTHD_READ_SCR8110_SCR8120  INPUT

*&---------------------------------------------------------------------*
*&      Form  READ_PMTIV_TO_IT
*&---------------------------------------------------------------------*
FORM READ_PMTIV_TO_IT.

  REFRESH : IT_ZSPMTIV, IT_ZSPMTIV_ORG.
  CLEAR   : IT_ZSPMTIV, IT_ZSPMTIV_ORG.
  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_ZSPMTIV
  FROM   ZTPMTIV
  WHERE  ZFPNNO  EQ  ZTPMTHD-ZFPNNO.

  IT_ZSPMTIV_ORG[] = IT_ZSPMTIV[].

ENDFORM.                               " READ_PMTIV_TO_IT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8110  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8110 INPUT.

  MOVE C_REQ_U TO W_STATUS.

  IF ZTREQHD-ZFREQTY = 'LO'.
    SET SCREEN 8220.  LEAVE SCREEN.
  ELSE.
    SET SCREEN 8210.  LEAVE SCREEN.
  ENDIF.

ENDMODULE.                             " USER_COMMAND_SCR8110  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8120  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8120 INPUT.

  MOVE C_REQ_D TO W_STATUS.

  IF ZTREQHD-ZFREQTY = 'LO'.
    SET SCREEN 8220.  LEAVE SCREEN.
  ELSE.
    SET SCREEN 8210.  LEAVE SCREEN.
  ENDIF.

ENDMODULE.                             " USER_COMMAND_SCR8120  INPUT

*&---------------------------------------------------------------------*
*&      Module  TC_8220_UPDATE_SCR8220  INPUT
*&---------------------------------------------------------------------*
MODULE TC_8220_UPDATE_SCR8220 INPUT.

  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  READ TABLE IT_ZSPMTIV INDEX TC_8212-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.

  MOVE-CORRESPONDING ZSPMTIV TO IT_ZSPMTIV.
  IF OK-CODE = 'MKA1'.
    MOVE   'X'             TO W_ROW_MARK.
  ENDIF.
  IF OK-CODE = 'MKL1'.
    CLEAR  W_ROW_MARK.
  ENDIF.
  MOVE W_ROW_MARK TO IT_ZSPMTIV-ZFMARK.

  IF W_OLD_SUBRC = 0.
    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSPMTIV-ZFIVAMT
                                            IT_ZSPMTIV-ZFIVAMC.
    MODIFY IT_ZSPMTIV   INDEX TC_8212-CURRENT_LINE.
  ENDIF.

ENDMODULE.                             " TC_8220_UPDATE_SCR8220  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR8220  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR8220 INPUT.

  CASE OK-CODE.
    WHEN 'REF1'.
      PERFORM READ_ZTVTIV_LLC_SCR8100.
    WHEN OTHERS.
      IF W_STATUS NE 'D'.
        REFRESH IT_ZSPMTIV_DEL.
        CLEAR   IT_ZSPMTIV_DEL.
        LOOP AT IT_ZSPMTIV.
          IF OK-CODE = 'MKA1'.
            MOVE   'X'             TO IT_ZSPMTIV-ZFMARK.
          ENDIF.
          IF OK-CODE = 'MKL1'.
            CLEAR  IT_ZSPMTIV-ZFMARK.
          ENDIF.
          IF OK-CODE = 'DEL1' AND IT_ZSPMTIV-ZFMARK = 'X'.
          ELSE.
            MOVE-CORRESPONDING IT_ZSPMTIV TO IT_ZSPMTIV_DEL.
            APPEND IT_ZSPMTIV_DEL.
          ENDIF.
        ENDLOOP.

        REFRESH IT_ZSPMTIV.

        DESCRIBE TABLE IT_ZSPMTIV_DEL LINES LINE.

        CLEAR W_ZFTIVAM.
        IF LINE > 0.
          LOOP AT IT_ZSPMTIV_DEL.
            CLEAR  IT_ZSPMTIV.
            MOVE-CORRESPONDING IT_ZSPMTIV_DEL TO IT_ZSPMTIV.
            ADD    IT_ZSPMTIV-ZFIVAMT          TO W_ZFTIVAM.
            APPEND IT_ZSPMTIV.
          ENDLOOP.
        ENDIF.
        MOVE W_ZFTIVAM TO ZTPMTHD-ZFTIVAM.
        IF ZTPMTHD-ZFTIVAM NE ZTPMTHD-ZFPNAM.
          MESSAGE E847.
          EXIT.
        ENDIF.
      ENDIF.                           "IF W_STATUS NE 'D'.
  ENDCASE.

  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE' OR 'SVCO'.
      MOVE OK-CODE TO W_OK_CODE.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_SCR8220  INPUT

*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR8220  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR8220 INPUT.

  PERFORM OK_CODE_PATM.

ENDMODULE.                             " AFI_PROCESS_SCR8220  INPUT

*&---------------------------------------------------------------------*
*&      Form  READ_ZTVTIV_LLC_SCR8100
*&---------------------------------------------------------------------*
FORM READ_ZTVTIV_LLC_SCR8100.

  REFRESH IT_ZSPMTIV.
  CLEAR : W_ZFPNIT, W_ZFTIVAM.
  SELECT * INTO  TABLE IT_ZTVTIV
           FROM  ZTVTIV
           WHERE ZFREDNO EQ ZTRED-ZFREDNO.
*  SELECT * FROM ZTVTIV
*           WHERE ZFREDNO EQ ZTRED-ZFREDNO.
  LOOP AT IT_ZTVTIV.
    CLEAR IT_ZSPMTIV.

    MOVE  W_ZFPNIT          TO IT_ZSPMTIV-ZFPNIT.  "일련번호.
    MOVE  IT_ZTVTIV-ZFGFDYR    TO IT_ZSPMTIV-ZFGFDYR. " 전표연도.
    MOVE  IT_ZTVTIV-ZFGFDNO    TO IT_ZSPMTIV-ZFGFDNO. " 전표번호.
    MOVE  IT_ZTVTIV-ZFEXRT     TO IT_ZSPMTIV-ZFEXRT.
    IF SY-TABIX EQ 1.
      MOVE IT_ZTVTIV-ZFEXRT   TO ZTPMTHD-ZFEXRT.
    ENDIF.

    IF IT_ZTVTIV-ZFGFDNO IS INITIAL.
      CONTINUE.
    ENDIF.

    SELECT * FROM ZTVTIVIT
             WHERE ZFGFDYR = IT_ZSPMTIV-ZFGFDYR
             AND   ZFGFDNO = IT_ZSPMTIV-ZFGFDNO.
      ADD  ZTVTIVIT-ZFIVAMT TO IT_ZSPMTIV-ZFIVAMT. "I/V  Amt
      MOVE ZTVTIVIT-ZFIVAMC TO IT_ZSPMTIV-ZFIVAMC. "I/V Curr
    ENDSELECT.
*
    CLEAR : W_ZFPNAM, ZSPMTIV-ZFPBAMT.
    SELECT SUM( ZFPNAM ) INTO W_ZFPNAM
      FROM ZTPMTIV
     WHERE ZFGFDYR = IT_ZTVTIV-ZFGFDYR
       AND ZFGFDNO = IT_ZTVTIV-ZFGFDNO.
    IF IT_ZSPMTIV-ZFIVAMT > W_ZFPNAM.        " Payable Amount
      IT_ZSPMTIV-ZFPNAM = IT_ZSPMTIV-ZFIVAMT - W_ZFPNAM.
    ENDIF.
    IT_ZSPMTIV-ZFPBAMT =  W_ZFPNAM. " Notice Amount
    IF IT_ZSPMTIV-ZFPNAM  = 0.
      CONTINUE.
    ENDIF.
    ADD IT_ZSPMTIV-ZFPNAM       TO W_ZFTIVAM.
    IF IT_ZSPMTIV-ZFPNAM < IT_ZSPMTIV-ZFIVAMT. " Partial Payment
      MOVE 'Y' TO IT_ZSPMTIV-ZFPPYYN.
    ELSE.
      MOVE 'N' TO IT_ZSPMTIV-ZFPPYYN.
    ENDIF.
*
    SELECT SINGLE *
      FROM ZTVT
     WHERE ZFVTNO = IT_ZTVTIV-ZFVTNO.
    IF SY-SUBRC = 0.
      MOVE ZTVT-ZFVTRYN TO IT_ZSPMTIV-ZFVTRYN.
    ENDIF.
    MOVE IT_ZSPMTIV-ZFIVAMC TO ZTPMTHD-ZFPNAMC.
    MOVE IT_ZSPMTIV-ZFIVAMC TO ZTPMTHD-ZFUSITC.
    MOVE IT_ZSPMTIV-ZFIVAMC TO ZTPMTHD-ZFBKCHC.
    IF  IT_ZSPMTIV-ZFPNAM  EQ  0.
      CONTINUE.
    ENDIF.

    ADD   10                TO W_ZFPNIT.
    APPEND IT_ZSPMTIV.
*  ENDSELECT.
  ENDLOOP.


  MOVE   W_ZFTIVAM          TO ZTPMTHD-ZFTIVAM.
  MOVE   IT_ZSPMTIV-ZFIVAMC TO W_CUR.

ENDFORM.                               " READ_ZTVTIV_LLC_SCR8100

*&---------------------------------------------------------------------*
*&      Module  CHECK_BANK_SCR8210  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_BANK_SCR8210 INPUT.

  IF W_STATUS = 'D'. EXIT. ENDIF.

  IF NOT ( ZTPMTHD-ZFPNBN IS INITIAL )." 통지은?
    SELECT SINGLE NAME1 INTO W_ZFPNBN_NM
      FROM LFA1
     WHERE LIFNR = ZTPMTHD-ZFPNBN.
    IF ZTPMTHD-ZFPNBNK IS INITIAL.
      SELECT MAX( BANKL ) INTO ZTPMTHD-ZFPNBNK
        FROM LFBK
       WHERE LIFNR = ZTPMTHD-ZFPNBN.
    ENDIF.
  ENDIF.

  IF NOT ( ZTPMTHD-ZFLEVN IS INITIAL )." 차입기?
    SELECT SINGLE NAME1 INTO W_ZFLEVN_NM
      FROM LFA1
     WHERE LIFNR = ZTPMTHD-ZFLEVN.
    IF ZTPMTHD-ZFLEVNK IS INITIAL.
      SELECT MAX( BANKL ) INTO ZTPMTHD-ZFLEVNK
        FROM LFBK
       WHERE LIFNR = ZTPMTHD-ZFLEVN.
    ENDIF.
  ELSE.
    CLEAR ZTPMTHD-ZFLEVNK.
  ENDIF.

*  IF ZTPMTHD-ZFPNBN  IS INITIAL OR
*     ZTPMTHD-ZFPNBNK IS INITIAL.
*    MESSAGE W889.
*  ENDIF.
*  IF ZTPMTHD-ZFOPBNK IS INITIAL.
*    MESSAGE W891.
*  ENDIF.
*  IF NOT ( ZTPMTHD-ZFLEVN IS INITIAL ).
*    IF ZTPMTHD-ZFLEVNK IS INITIAL.
*      MESSAGE W890.
*    ENDIF.
*  ENDIF.

ENDMODULE.                             " CHECK_BANK_SCR8210  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_BANK_SCR8220  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_BANK_SCR8220 INPUT.

  IF NOT ( ZTPMTHD-ZFPNBN IS INITIAL )." 통지은?
    SELECT SINGLE NAME1 INTO W_ZFPNBN_NM
      FROM LFA1
     WHERE LIFNR = ZTPMTHD-ZFPNBN.
    IF ZTPMTHD-ZFPNBNK IS INITIAL.
      SELECT MAX( BANKL ) INTO ZTPMTHD-ZFPNBNK
        FROM LFBK
       WHERE LIFNR = ZTPMTHD-ZFPNBN.
    ENDIF.
  ENDIF.

  IF ZTPMTHD-ZFPNBN  IS INITIAL OR
     ZTPMTHD-ZFPNBNK IS INITIAL.
    MESSAGE E889.
  ENDIF.
  IF ZTPMTHD-ZFOPBNK IS INITIAL.
    MESSAGE E891.
  ENDIF.

ENDMODULE.                             " CHECK_BANK_SCR8220  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_ZFRANL1_SCR8214  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ZFRANL1_SCR8214 INPUT.
  DATA: L_DYNPFIELDS  LIKE DYNPREAD    OCCURS 0 WITH HEADER LINE,
        I_HELP_VALUE  LIKE HELP_VALUE  OCCURS 0 WITH HEADER LINE,
        I_HELP_VTAB   LIKE HELP_VTAB   OCCURS 0 WITH HEADER LINE,
        BEGIN OF VALUETAB OCCURS 0,
            VALUE LIKE HELP_VTAB-VALUE,
        END OF VALUETAB,
        J      TYPE I,
        BEGIN OF CODE_SET OCCURS 0,
            RANL      LIKE  VDARL-RANL,
            GSART     LIKE  VDARL-GSART,
            STITEL    LIKE  VDARL-STITEL,
            SANTWHR   LIKE  VDARL-SANTWHR,
            SFRIST    LIKE  VDARL-SFRIST,
            XALKZ     LIKE  VDARL-XALKZ,
            RANLALT1  LIKE  VDARL-RANLALT1,
            BZUSAGE   LIKE  VDARL-BZUSAGE,
            L_BZUSAGE(20),
        END OF CODE_SET,
        L_VALUE  LIKE  VDARL-RDARNEHM.
  TYPES: BEGIN OF F4TYP_HEAD_STRUC,
          TABNAME LIKE HELP_INFO-TABNAME,
          FIELDNAME LIKE HELP_INFO-FIELDNAME,
          HEAD_TEXT LIKE SHSTRUC-KEYWORD,
        END OF F4TYP_HEAD_STRUC.

  DATA: L_HEADING_TAB TYPE F4TYP_HEAD_STRUC OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME               = SY-CPROG
            DYNUMB               = '8210'
            REQUEST              = 'A'
       TABLES
            DYNPFIELDS           = L_DYNPFIELDS
       EXCEPTIONS
            INVALID_ABAPWORKAREA = 1
            INVALID_DYNPROFIELD  = 2
            INVALID_DYNPRONAME   = 3
            INVALID_DYNPRONUMMER = 4
            INVALID_REQUEST      = 5
            NO_FIELDDESCRIPTION  = 6
            INVALID_PARAMETER    = 7
            UNDEFIND_ERROR       = 8
            DOUBLE_CONVERSION    = 9
            ERROR_MESSAGE        = 10
            OTHERS               = 11.
  CHECK SY-SUBRC = 0.
  READ TABLE L_DYNPFIELDS WITH KEY FIELDNAME = 'ZTPMTHD-ZFLEVN'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = L_DYNPFIELDS-FIELDVALUE
       IMPORTING
            OUTPUT = L_VALUE.

  SELECT  RANL GSART STITEL SANTWHR SFRIST XALKZ RANLALT1 BZUSAGE
                  INTO TABLE CODE_SET
                  FROM VDARL WHERE BUKRS = '1000'
                  AND RDARNEHM = L_VALUE.

  LOOP AT CODE_SET.
    CLEAR CODE_SET-XALKZ.
    SELECT SINGLE   XKTEXT INTO  CODE_SET-XALKZ
                 FROM TD09T  WHERE SPRAS   = 'EN'
                               AND SFRIST  = CODE_SET-SFRIST.
    WRITE CODE_SET-BZUSAGE TO CODE_SET-L_BZUSAGE
           CURRENCY CODE_SET-SANTWHR.
    MODIFY CODE_SET.
    CLEAR  CODE_SET.
  ENDLOOP.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'RANL'.
  I_HELP_VALUE-SELECTFLAG = 'X'.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'GSART'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'STITEL'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'SANTWHR'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'XALKZ'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'RANLALT1'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'ZUOND'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

*  I_HELP_VALUE-TABNAME = 'VDARL'.
*  I_HELP_VALUE-FIELDNAME = 'BZUSAGE'.
*  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

* text
  L_HEADING_TAB-TABNAME   = 'VDARL'.
  L_HEADING_TAB-FIELDNAME = 'RANLALT1'.
  L_HEADING_TAB-HEAD_TEXT = '차입비율'.
  APPEND L_HEADING_TAB.

  L_HEADING_TAB-TABNAME   = 'VDARL'.
  L_HEADING_TAB-FIELDNAME = 'XALKZ'.
  L_HEADING_TAB-HEAD_TEXT = 'Maturity'.
  APPEND L_HEADING_TAB.

  L_HEADING_TAB-TABNAME   = 'VDARL'.
  L_HEADING_TAB-FIELDNAME = 'ZUOND'.
  L_HEADING_TAB-HEAD_TEXT = 'Commitment capital'.
  APPEND L_HEADING_TAB.

  LOOP AT CODE_SET.
    VALUETAB-VALUE  = CODE_SET-RANL.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-GSART.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-STITEL.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-SANTWHR.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-XALKZ.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-RANLALT1.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-L_BZUSAGE.
    APPEND VALUETAB.
  ENDLOOP.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            DISPLAY       = ' '
       IMPORTING
            INDEX         = J
       TABLES
            FIELDS        = I_HELP_VALUE
            SELECT_VALUES = I_HELP_VTAB
            VALUETAB      = VALUETAB
            HEADING_TABLE = L_HEADING_TAB.

  IF J <> 0.
    READ TABLE CODE_SET INDEX J.
    ZTPMTHD-ZFRANL1 = CODE_SET-RANL.
  ENDIF.
  REFRESH: I_HELP_VALUE, I_HELP_VTAB, VALUETAB, CODE_SET,
           L_DYNPFIELDS, L_HEADING_TAB.
  CLEAR: I_HELP_VALUE, I_HELP_VTAB, VALUETAB, CODE_SET,
           L_DYNPFIELDS, J, L_VALUE, L_HEADING_TAB.

ENDMODULE.                             " HELP_ZFRANL1_SCR8214  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_ZFRANL2_SCR8214  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ZFRANL2_SCR8214 INPUT.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME               = SY-CPROG
            DYNUMB               = '8210'
            REQUEST              = 'A'
       TABLES
            DYNPFIELDS           = L_DYNPFIELDS
       EXCEPTIONS
            INVALID_ABAPWORKAREA = 1
            INVALID_DYNPROFIELD  = 2
            INVALID_DYNPRONAME   = 3
            INVALID_DYNPRONUMMER = 4
            INVALID_REQUEST      = 5
            NO_FIELDDESCRIPTION  = 6
            INVALID_PARAMETER    = 7
            UNDEFIND_ERROR       = 8
            DOUBLE_CONVERSION    = 9
            ERROR_MESSAGE        = 10
            OTHERS               = 11.
  CHECK SY-SUBRC = 0.
  READ TABLE L_DYNPFIELDS WITH KEY FIELDNAME = 'ZTPMTHD-ZFLEVN'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = L_DYNPFIELDS-FIELDVALUE
       IMPORTING
            OUTPUT = L_VALUE.

  SELECT  RANL GSART STITEL SANTWHR SFRIST XALKZ RANLALT1 BZUSAGE
                 INTO TABLE CODE_SET
                 FROM VDARL WHERE BUKRS = '1000'
                 AND RDARNEHM = L_VALUE.

  LOOP AT CODE_SET.
    CLEAR CODE_SET-XALKZ.
    SELECT SINGLE   XKTEXT INTO  CODE_SET-XALKZ
                 FROM TD09T  WHERE SPRAS   = 'EN'
                               AND SFRIST  = CODE_SET-SFRIST.
    WRITE CODE_SET-BZUSAGE TO CODE_SET-L_BZUSAGE
          CURRENCY CODE_SET-SANTWHR.
    MODIFY CODE_SET.
    CLEAR  CODE_SET.
  ENDLOOP.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'RANL'.
  I_HELP_VALUE-SELECTFLAG = 'X'.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'GSART'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'STITEL'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'SANTWHR'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'XALKZ'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'RANLALT1'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'ZUOND'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

* text
  L_HEADING_TAB-TABNAME   = 'VDARL'.
  L_HEADING_TAB-FIELDNAME = 'RANLALT1'.
  L_HEADING_TAB-HEAD_TEXT = '차입비율'.
  APPEND L_HEADING_TAB.

  L_HEADING_TAB-TABNAME   = 'VDARL'.
  L_HEADING_TAB-FIELDNAME = 'XALKZ'.
  L_HEADING_TAB-HEAD_TEXT = 'Maturity'.
  APPEND L_HEADING_TAB.

  L_HEADING_TAB-TABNAME   = 'VDARL'.
  L_HEADING_TAB-FIELDNAME = 'ZUOND'.
  L_HEADING_TAB-HEAD_TEXT = 'Commitment capital'.
  APPEND L_HEADING_TAB.

  LOOP AT CODE_SET.
    VALUETAB-VALUE  = CODE_SET-RANL.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-GSART.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-STITEL.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-SANTWHR.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-XALKZ.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-RANLALT1.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-L_BZUSAGE.
    APPEND VALUETAB.
  ENDLOOP.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            DISPLAY       = ' '
       IMPORTING
            INDEX         = J
       TABLES
            FIELDS        = I_HELP_VALUE
            SELECT_VALUES = I_HELP_VTAB
            VALUETAB      = VALUETAB
            HEADING_TABLE = L_HEADING_TAB.

  IF J <> 0.
    READ TABLE CODE_SET INDEX J.
    ZTPMTHD-ZFRANL2 = CODE_SET-RANL.
  ENDIF.
  REFRESH: I_HELP_VALUE, I_HELP_VTAB, VALUETAB, CODE_SET,
           L_DYNPFIELDS, L_HEADING_TAB.

  CLEAR: I_HELP_VALUE, I_HELP_VTAB, VALUETAB, CODE_SET,
           L_DYNPFIELDS, J, L_VALUE, L_HEADING_TAB.

ENDMODULE.                 " HELP_ZFRANL2_SCR8214  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_ZFLSNO1_SCR8214  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ZFLSNO1_SCR8214 INPUT.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME               = SY-CPROG
            DYNUMB               = '8210'
            REQUEST              = 'A'
       TABLES
            DYNPFIELDS           = L_DYNPFIELDS
       EXCEPTIONS
            INVALID_ABAPWORKAREA = 1
            INVALID_DYNPROFIELD  = 2
            INVALID_DYNPRONAME   = 3
            INVALID_DYNPRONUMMER = 4
            INVALID_REQUEST      = 5
            NO_FIELDDESCRIPTION  = 6
            INVALID_PARAMETER    = 7
            UNDEFIND_ERROR       = 8
            DOUBLE_CONVERSION    = 9
            ERROR_MESSAGE        = 10
            OTHERS               = 11.
  CHECK SY-SUBRC = 0.
  READ TABLE L_DYNPFIELDS WITH KEY FIELDNAME = 'ZTPMTHD-ZFLEVN'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = L_DYNPFIELDS-FIELDVALUE
       IMPORTING
            OUTPUT = L_VALUE.

  SELECT  RANL GSART STITEL SANTWHR SFRIST XALKZ RANLALT1 BZUSAGE
                 INTO TABLE CODE_SET
                 FROM VDARL WHERE BUKRS = '1000'
                 AND RDARNEHM = L_VALUE.

  LOOP AT CODE_SET.
    CLEAR CODE_SET-XALKZ.
    SELECT SINGLE   XKTEXT INTO  CODE_SET-XALKZ
                 FROM TD09T  WHERE SPRAS   = 'EN'
                               AND SFRIST  = CODE_SET-SFRIST.
    WRITE CODE_SET-BZUSAGE TO CODE_SET-L_BZUSAGE
          CURRENCY CODE_SET-SANTWHR.
    MODIFY CODE_SET.
    CLEAR  CODE_SET.
  ENDLOOP.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'RANL'.
  I_HELP_VALUE-SELECTFLAG = 'X'.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'GSART'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'STITEL'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'SANTWHR'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'XALKZ'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'RANLALT1'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'ZUOND'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

* text
  L_HEADING_TAB-TABNAME   = 'VDARL'.
  L_HEADING_TAB-FIELDNAME = 'RANLALT1'.
  L_HEADING_TAB-HEAD_TEXT = '차입비율'.
  APPEND L_HEADING_TAB.

  L_HEADING_TAB-TABNAME   = 'VDARL'.
  L_HEADING_TAB-FIELDNAME = 'XALKZ'.
  L_HEADING_TAB-HEAD_TEXT = 'Maturity'.
  APPEND L_HEADING_TAB.

  L_HEADING_TAB-TABNAME   = 'VDARL'.
  L_HEADING_TAB-FIELDNAME = 'ZUOND'.
  L_HEADING_TAB-HEAD_TEXT = 'Commitment capital'.
  APPEND L_HEADING_TAB.

  LOOP AT CODE_SET.
    VALUETAB-VALUE  = CODE_SET-RANL.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-GSART.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-STITEL.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-SANTWHR.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-XALKZ.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-RANLALT1.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-L_BZUSAGE.
    APPEND VALUETAB.
  ENDLOOP.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            DISPLAY       = ' '
       IMPORTING
            INDEX         = J
       TABLES
            FIELDS        = I_HELP_VALUE
            SELECT_VALUES = I_HELP_VTAB
            VALUETAB      = VALUETAB
            HEADING_TABLE = L_HEADING_TAB.

  IF J <> 0.
    READ TABLE CODE_SET INDEX J.
*>>>>KSB CHANGE
*     ZTPMTHD-ZFLSNO1 = CODE_SET-RANL.
  ENDIF.

  REFRESH: I_HELP_VALUE, I_HELP_VTAB, VALUETAB, CODE_SET,
           L_DYNPFIELDS, L_HEADING_TAB.

  CLEAR: I_HELP_VALUE, I_HELP_VTAB, VALUETAB, CODE_SET,
           L_DYNPFIELDS, J, L_VALUE, L_HEADING_TAB.

ENDMODULE.                 " HELP_ZFLSNO1_SCR8214  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_ZFLSN02_SCR8214  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ZFLSN02_SCR8214 INPUT.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME               = SY-CPROG
            DYNUMB               = '8210'
            REQUEST              = 'A'
       TABLES
            DYNPFIELDS           = L_DYNPFIELDS
       EXCEPTIONS
            INVALID_ABAPWORKAREA = 1
            INVALID_DYNPROFIELD  = 2
            INVALID_DYNPRONAME   = 3
            INVALID_DYNPRONUMMER = 4
            INVALID_REQUEST      = 5
            NO_FIELDDESCRIPTION  = 6
            INVALID_PARAMETER    = 7
            UNDEFIND_ERROR       = 8
            DOUBLE_CONVERSION    = 9
            ERROR_MESSAGE        = 10
            OTHERS               = 11.
  CHECK SY-SUBRC = 0.
  READ TABLE L_DYNPFIELDS WITH KEY FIELDNAME = 'ZTPMTHD-ZFLEVN'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = L_DYNPFIELDS-FIELDVALUE
       IMPORTING
            OUTPUT = L_VALUE.

  SELECT  RANL GSART STITEL SANTWHR SFRIST XALKZ RANLALT1 BZUSAGE
                INTO TABLE CODE_SET
                FROM VDARL WHERE BUKRS = '1000'
                AND RDARNEHM = L_VALUE.

  LOOP AT CODE_SET.
    CLEAR CODE_SET-XALKZ.
    SELECT SINGLE   XKTEXT INTO  CODE_SET-XALKZ
                 FROM TD09T  WHERE SPRAS   = 'EN'
                               AND SFRIST  = CODE_SET-SFRIST.
    WRITE CODE_SET-BZUSAGE TO CODE_SET-L_BZUSAGE
          CURRENCY CODE_SET-SANTWHR.
    MODIFY CODE_SET.
    CLEAR  CODE_SET.
  ENDLOOP.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'RANL'.
  I_HELP_VALUE-SELECTFLAG = 'X'.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'GSART'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'STITEL'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'SANTWHR'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'XALKZ'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'RANLALT1'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

  I_HELP_VALUE-TABNAME = 'VDARL'.
  I_HELP_VALUE-FIELDNAME = 'ZUOND'.
  I_HELP_VALUE-SELECTFLAG = ' '.      APPEND I_HELP_VALUE.

* text
  L_HEADING_TAB-TABNAME   = 'VDARL'.
  L_HEADING_TAB-FIELDNAME = 'RANLALT1'.
  L_HEADING_TAB-HEAD_TEXT = '차입비율'.
  APPEND L_HEADING_TAB.

  L_HEADING_TAB-TABNAME   = 'VDARL'.
  L_HEADING_TAB-FIELDNAME = 'XALKZ'.
  L_HEADING_TAB-HEAD_TEXT = 'Maturity'.
  APPEND L_HEADING_TAB.

  L_HEADING_TAB-TABNAME   = 'VDARL'.
  L_HEADING_TAB-FIELDNAME = 'ZUOND'.
  L_HEADING_TAB-HEAD_TEXT = 'Commitment capital'.
  APPEND L_HEADING_TAB.

  LOOP AT CODE_SET.
    VALUETAB-VALUE  = CODE_SET-RANL.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-GSART.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-STITEL.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-SANTWHR.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-XALKZ.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-RANLALT1.
    APPEND VALUETAB.
    VALUETAB-VALUE  = CODE_SET-L_BZUSAGE.
    APPEND VALUETAB.
  ENDLOOP.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            DISPLAY       = ' '
       IMPORTING
            INDEX         = J
       TABLES
            FIELDS        = I_HELP_VALUE
            SELECT_VALUES = I_HELP_VTAB
            VALUETAB      = VALUETAB
            HEADING_TABLE = L_HEADING_TAB.

  IF J <> 0.
    READ TABLE CODE_SET INDEX J.
*>>>KSB CHANGE
*    ZTPMTHD-ZFLSNO2 = CODE_SET-RANL.
  ENDIF.

  REFRESH: I_HELP_VALUE, I_HELP_VTAB, VALUETAB, CODE_SET,
           L_DYNPFIELDS, L_HEADING_TAB.

  CLEAR: I_HELP_VALUE, I_HELP_VTAB, VALUETAB, CODE_SET,
           L_DYNPFIELDS, J, L_VALUE, L_HEADING_TAB.

ENDMODULE.                 " HELP_ZFLSN02_SCR8214  INPUT

*&---------------------------------------------------------------------*
*&      Form  P1000_AMT_CONV1
*&---------------------------------------------------------------------*
FORM P1000_AMT_CONV1 USING    P_CUR1 P_CUR2 P_AMT.

  CLEAR : TCURX, W_CURRDEC1.
  SELECT SINGLE *
    FROM TCURX
   WHERE CURRKEY = P_CUR1.
  IF SY-SUBRC NE 0.
    TCURX-CURRDEC = 2.
  ENDIF.
  MOVE TCURX-CURRDEC TO W_CURRDEC1.

  CLEAR : TCURX, W_CURRDEC2.
  SELECT SINGLE *
    FROM TCURX
   WHERE CURRKEY = P_CUR2.
  IF SY-SUBRC NE 0.
    TCURX-CURRDEC = 2.
  ENDIF.
  MOVE TCURX-CURRDEC TO W_CURRDEC2.

  IF W_CURRDEC1 > W_CURRDEC2.
    W_CURRDEC = W_CURRDEC1 - W_CURRDEC2.
    DO W_CURRDEC TIMES.
      P_AMT = P_AMT / 10.
    ENDDO.
  ENDIF.
  IF W_CURRDEC1 < W_CURRDEC2.
    W_CURRDEC = W_CURRDEC2 - W_CURRDEC1.
    DO W_CURRDEC TIMES.
      P_AMT = P_AMT * 10.
    ENDDO.
  ENDIF.

ENDFORM.                    " P1000_AMT_CONV1

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_CONRATIO
*&---------------------------------------------------------------------*
FORM P1000_GET_CONRATIO USING P_CUR1 P_CUR2 P_RATIO.

  CLEAR: W_EXCHR, W_UKURS_1.
  CALL FUNCTION 'RKC_SINGLE_EXCHANGE_RATE_GET'
       EXPORTING
            DATUM         = ZTPMTHD-ZFNTDT
            INVKZ         = ' '
            KURST         = 'M'
            NCURR         = 'KRW'
            VCURR         = P_CUR1
       IMPORTING
            EXCHR         = W_EXCHR
       EXCEPTIONS
            NO_RATE_FOUND = 1
            OTHERS        = 2.
  IF SY-SUBRC = 0.
    W_UKURS_1 = W_EXCHR.                 "환?
  ELSE.
    W_UKURS_1 = 1.
  ENDIF.

  CLEAR: W_EXCHR, W_UKURS_2.
  CALL FUNCTION 'RKC_SINGLE_EXCHANGE_RATE_GET'
       EXPORTING
            DATUM         = ZTPMTHD-ZFNTDT
            INVKZ         = ' '
            KURST         = 'M'
            NCURR         = 'KRW'
            VCURR         = P_CUR2
       IMPORTING
            EXCHR         = W_EXCHR
       EXCEPTIONS
            NO_RATE_FOUND = 1
            OTHERS        = 2.
  IF SY-SUBRC = 0.
    W_UKURS_2 = W_EXCHR.                 "환?
  ELSE.
    W_UKURS_2 = 1.
  ENDIF.

  P_RATIO = W_UKURS_1 / W_UKURS_2.

ENDFORM.                    " P1000_GET_CONRATIO

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTPMTHD_DELETE_SCR8110
*&---------------------------------------------------------------------*
FORM P3000_ZTPMTHD_DELETE_SCR8110.

  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  IF ANTWORT NE 'Y'.
    EXIT.
  ENDIF.

  DELETE FROM ZTPMTHD WHERE ZFPNNO = ZTPMTHD-ZFPNNO.
  DELETE FROM ZTPMTIV WHERE ZFPNNO = ZTPMTHD-ZFPNNO.
  IF SY-SUBRC NE 0.
    MESSAGE E895.
    SET SCREEN 8110.
    LEAVE SCREEN.
  ENDIF.
  MESSAGE S756.

  SET SCREEN 8110.
  LEAVE SCREEN.

ENDFORM.                    " P3000_ZTPMTHD_DELETE_SCR8110

*&---------------------------------------------------------------------*
*&      Form  READ_MSEG_TO_IT_SCR6700
*&---------------------------------------------------------------------*
FORM READ_MSEG_TO_IT_SCR6700.

  CLEAR   W_ZFIVAMT_S.
  SELECT *
    FROM MSEG
   WHERE MBLNR = MSEG-MBLNR
     AND MJAHR = MSEG-MJAHR.
    CLEAR IT_ZSCUCLIVIT.
    MOVE MSEG-EBELP            TO IT_ZSCUCLIVIT-ZFIVDNO.
    MOVE MSEG-MATNR            TO IT_ZSCUCLIVIT-MATNR.
    MOVE MSEG-MENGE            TO IT_ZSCUCLIVIT-MENGE.
*    MOVE MSEG-MENGE            TO IT_ZSCUCLIVIT-MENGE1.
    MOVE MSEG-MEINS            TO IT_ZSCUCLIVIT-MEINS.
    CLEAR EKPO.
    SELECT SINGLE *
      FROM EKPO
     WHERE EBELN = MSEG-EBELN
       AND EBELP = MSEG-EBELP.
    MOVE EKPO-TXZ01            TO IT_ZSCUCLIVIT-TXZ01.
    MOVE EKPO-NETPR            TO IT_ZSCUCLIVIT-NETPR.
    MOVE EKPO-PEINH            TO IT_ZSCUCLIVIT-PEINH.
    MOVE EKPO-BPRME            TO IT_ZSCUCLIVIT-BPRME.
*>>>KSB CHANGE
*    MOVE EKPO-ZZHSCODE         TO IT_ZSCUCLIVIT-STAWN.
    IT_ZSCUCLIVIT-ZFIVAMT =
         MSEG-MENGE * EKPO-NETPR / EKPO-PEINH.
    ADD  IT_ZSCUCLIVIT-ZFIVAMT     TO W_ZFIVAMT_S.
    CLEAR EKKO.
    SELECT SINGLE *
      FROM EKKO
     WHERE EBELN = MSEG-EBELN.
    MOVE EKKO-WAERS            TO IT_ZSCUCLIVIT-ZFIVAMC.
    MOVE EKKO-WAERS            TO ZTCUCLIV-ZFIVAMC.
    APPEND IT_ZSCUCLIVIT.
  ENDSELECT.
  MOVE W_ZFIVAMT_S TO ZTCUCLIV-ZFIVAMT.

ENDFORM.                    " READ_MSEG_TO_IT_SCR6700

*&---------------------------------------------------------------------*
*&      Module  ZFPONC_CHECK_SCR6710  INPUT
*&---------------------------------------------------------------------*
MODULE ZFPONC_CHECK_SCR6710 INPUT.

  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  SELECT SINGLE *
    FROM ZTIMIMG08
   WHERE ZFCDTY = '001'
     AND ZFCD   =  ZTCUCLIV-ZFPONC.
  IF SY-SUBRC NE 0.
    MESSAGE E809 WITH ZTCUCLIV-ZFPONC.
  ENDIF.

ENDMODULE.                 " ZFPONC_CHECK_SCR6710  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZFINRC_PROCESS_SCR6210  INPUT
*&---------------------------------------------------------------------*
MODULE ZFINRC_PROCESS_SCR6210 INPUT.

  CHECK W_STATUS NE C_REQ_D.

*  MOVE ZTIMIMG00-ZFTDNO    TO ZTIDR-ZFTDNO.             " 납세자 통?
*  MOVE ZTIMIMG00-ZFTDAD1   TO ZTIDR-ZFTDAD1.            " 납세자 주소1
*  MOVE ZTIMIMG00-ZFTDAD2   TO ZTIDR-ZFTDAD2.            " 납세자 주소2
*  MOVE ZTIMIMG00-ZFTDTC    TO ZTIDR-ZFTDTC.             " 납세자 사업?
  IF ZTIDR-ZFINRC IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDR' 'ZFINRC'.
  ENDIF.
  IF ZTIDR-ZFWERKS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDR' 'ZFWERKS'.
  ENDIF.

  SELECT SINGLE * FROM ZTIMIMG02
         WHERE    ZFCOTM  EQ  ZTIDR-ZFINRC
         AND      ZFWERKS EQ  ZTIDR-ZFWERKS.

  IF SY-SUBRC EQ 0.
*     SELECT SINGLE * FROM  T001W
*            WHERE    WERKS EQ ZTIDR-ZFWERKS.

    MOVE ZTIMIMG02-ZFTDNM1    TO ZTIDR-ZFTDNM1.  " 납세자 상호.
    MOVE ZTIMIMG02-ZFTDNO     TO ZTIDR-ZFTDNO.   " 납세자 통관고유번호.
    MOVE ZTIMIMG02-ZFTDAD1    TO ZTIDR-ZFTDAD1.  " 납세자 주소1
    MOVE ZTIMIMG02-ZFTDAD2    TO ZTIDR-ZFTDAD2.  " 납세자 주소2
    MOVE ZTIMIMG02-ZFTDTC     TO ZTIDR-ZFTDTC.   " 납세자 사업자.
  ELSE.
    MESSAGE E604(ZIM1) WITH ZTIDR-ZFINRC ZTIDR-ZFWERKS.
  ENDIF.
*  IF ZTIDR-ZFINRC = '151'. " 청?
*  ENDIF.
*  IF ZTIDR-ZFINRC = '121'. " 구?
*     MOVE ZTIMIMG00-ZFTDNO2    TO ZTIDR-ZFTDNO.         " 납세자 통관?
*     MOVE ZTIMIMG00-ZFTDAD21   TO ZTIDR-ZFTDAD1.        " 납세자 주소1
*     MOVE ZTIMIMG00-ZFTDAD22   TO ZTIDR-ZFTDAD2.        " 납세자 주소2
*     MOVE ZTIMIMG00-ZFTDTC2    TO ZTIDR-ZFTDTC.         " 납세자 사업?
*  ENDIF.

ENDMODULE.                 " ZFINRC_PROCESS_SCR6210  INPUT

*&---------------------------------------------------------------------*
*&      Form  READ_IDRCRIT_TO_IT_SCR6600
*&---------------------------------------------------------------------*
FORM READ_IDRCRIT_TO_IT_SCR6600.

  REFRESH IT_ZSIDRCRIT.
  SELECT *
    FROM ZTIDRCRIT
   WHERE ZFBLNO = ZTCUCL-ZFBLNO
     AND ZFCLSEQ = ZTCUCL-ZFCLSEQ
     AND ZFDETY = '4'.
    CLEAR   IT_ZSIDRCRIT.
    MOVE-CORRESPONDING ZTIDRCRIT TO IT_ZSIDRCRIT.
    SELECT SUM( ZFUSQN ) INTO W_ZFUSQN
      FROM ZTIDRDTU
     WHERE ZFBLNO = ZTCUCL-ZFBLNO
       AND ZFCLSEQ = ZTCUCL-ZFCLSEQ
       AND ZFCONO = IT_ZSIDRCRIT-ZFCONO
       AND ZFRONO = IT_ZSIDRCRIT-ZFRONO.
    IT_ZSIDRCRIT-ZFRMQN = IT_ZSIDRCRIT-ZFDEQN - W_ZFUSQN.
    APPEND IT_ZSIDRCRIT.
  ENDSELECT.

ENDFORM.                    " READ_IDRCRIT_TO_IT_SCR6600

*&---------------------------------------------------------------------*
*&      Form  P2000_ZTBLUG_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_ZTBLUG_DOC_DISPLAY USING    P_ZFBLNO.

  IF P_ZFBLNO IS INITIAL.
    MESSAGE E311.
  ENDIF.

  SET PARAMETER ID 'ZPBLNO' FIELD P_ZFBLNO.

  CALL TRANSACTION 'ZIM73' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_ZTBLUG_DOC_DISPLAY

*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR8410  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR8410 INPUT.

  PERFORM OK_CODE_TIVD.

ENDMODULE.                 " AFI_PROCESS_SCR8410  INPUT

*&---------------------------------------------------------------------*
*&      Form  OK_CODE_TIVD
*&---------------------------------------------------------------------*
FORM OK_CODE_TIVD.

  IF OK-CODE EQ 'FB03'.
    SET PARAMETER ID 'BUK'     FIELD '1000'.
    SET PARAMETER ID 'GJR'     FIELD ZTVTIV-ZFGFDYR.
    SET PARAMETER ID 'BLN'     FIELD ZTVTIV-ZFGFDNO.
    CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
  ENDIF.

ENDFORM.                    " OK_CODE_TIVD

*&---------------------------------------------------------------------*
*&      Form  P3000_LOCRCT_EDI_CHECK
*&---------------------------------------------------------------------*
FORM P3000_LOCRCT_EDI_CHECK.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTIMIMGTX-BUKRS.
  IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
    MOVE 'O'    TO ZTRED-ZFEDICK.  " EDI Check
    IF ZTRED-ZFISNO IS INITIAL.     " 인수증 발급번?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFREVDT IS INITIAL. " 인수?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFISUDT IS INITIAL. " 발급?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFREXDT IS INITIAL. " 인수증 유효기?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFSCONM IS INITIAL. " 공급자 회사?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFRCONM IS INITIAL. " 수령인 상?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFRCHNM IS INITIAL. " 수령인 대표자?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFTXN4 IS INITIAL. " 수령인 전자서?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFRADD1 IS INITIAL. " 수령인 주?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFLLCON IS INITIAL. " Local L/C No
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFOPBNCD IS INITIAL. " 개설은행 EDI Code
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFOBNEID IS INITIAL. " 개설은행 식별?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFOBNM IS INITIAL. " 개설은행?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFOBBR IS INITIAL. " 개설은행 지점?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    IF ZTRED-ZFEXDT IS INITIAL. " 유효기?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    CLEAR W_CNT.
    SELECT COUNT( DISTINCT ZFREDNO ) INTO W_CNT
      FROM ZTREDSG1
     WHERE ZFREDNO = ZTRED-ZFREDNO.
    IF W_CNT = 0. " 물품내?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
    CLEAR LFA1.
    SELECT SINGLE *
     FROM LFA1
     WHERE LIFNR = ZTRED-LIFNR.
    IF LFA1-BAHNS IS INITIAL.       " EDI 식별?
      MOVE 'X' TO ZTRED-ZFEDICK.
    ENDIF.
  ENDIF.
ENDFORM.                    " P3000_LOCRCT_EDI_CHECK

*&---------------------------------------------------------------------*
*&      Form  CALC_ZTCUCLCST_TO_IT
*&---------------------------------------------------------------------*
FORM CALC_ZTCUCLCST_TO_IT.

  REFRESH IT_ZSCUCLCST.
* 관세,통관수수료,선급부가?
  CLEAR : ZTBL, W_ZFCAMT, W_TOT_VAT, W_ZFCAMTU,
          W_ZFCAMTU, W_ZFCAMT, W_TOT_VAT, W_TOT_TAX, W_TOT_GAM.

  SELECT SINGLE * FROM   ZTBL
  WHERE  ZFBLNO = ZTCUCL-ZFBLNO.

  LOOP  AT  IT_ZSIDSHSD.
    CLEAR  IT_ZSIDSHSD_CAL.
    MOVE : IT_ZSIDSHSD-ZFBLNO  TO  IT_ZSIDSHSD_CAL-ZFBLNO,
           IT_ZSIDSHSD-ZFCLSEQ TO  IT_ZSIDSHSD_CAL-ZFCLSEQ,
           IT_ZSIDSHSD-ZFCONO  TO  IT_ZSIDSHSD_CAL-ZFCONO,
           IT_ZSIDSHSD-ZFRONO  TO  IT_ZSIDSHSD_CAL-ZFRONO,
           IT_ZSIDSHSD-ZFQNT   TO  IT_ZSIDSHSD_CAL-ZFQNT.
* 수입의뢰번호, BL번호, 하역번호 구하기.
    SELECT SINGLE ZFBLIT ZFREQNO ZFITMNO STAWN
    INTO   (IT_ZSIDSHSD_CAL-ZFBLIT,  IT_ZSIDSHSD_CAL-ZFREQNO,
            IT_ZSIDSHSD_CAL-ZFITMNO, IT_ZSIDSHSD_CAL-STAWN)
    FROM   ZTIVIT
    WHERE  ZFIVNO   EQ  IT_ZSIDSHSD-ZFIVNO
    AND    ZFIVDNO  EQ  IT_ZSIDSHSD-ZFIVDNO.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.
    IF IT_ZSIDSHSD_CAL-ZFREQNO IS INITIAL.
      CONTINUE.
    ENDIF.

*과세 환율 구하기.
    SELECT SINGLE  ZFEXRT FFACT
    INTO  (IT_ZSIDSHSD_CAL-ZFEXRT, IT_ZSIDSHSD_CAL-FFACT)
    FROM   ZTIV
    WHERE  ZFIVNO  EQ  IT_ZSIDSHSD-ZFIVNO.
    IF IT_ZSIDSHSD_CAL-ZFEXRT LE 0 .
      IT_ZSIDSHSD_CAL-ZFEXRT = 1.
    ENDIF.
    IF IT_ZSIDSHSD_CAL-FFACT LE 0.
      IT_ZSIDSHSD_CAL-FFACT = 1.
    ENDIF.

*BL 자재 수량 구하기.
    SELECT SINGLE *
    FROM   ZTBLIT
    WHERE  ZFBLNO          EQ    IT_ZSIDSHSD_CAL-ZFBLNO
    AND    ZFBLIT          EQ    IT_ZSIDSHSD_CAL-ZFBLIT.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.
    MOVE ZTBLIT-BLMENGE TO IT_ZSIDSHSD_CAL-BLMENGE.
    IF NOT ZTBLIT-EBELN IS INITIAL.
      SELECT SINGLE * FROM EKPO
             WHERE    EBELN  EQ  ZTBLIT-EBELN
             AND      EBELP  EQ  ZTBLIT-EBELP.
      MOVE : EKPO-BPUMN   TO IT_ZSIDSHSD_CAL-BPUMN,
             EKPO-BPUMZ   TO IT_ZSIDSHSD_CAL-BPUMZ.
    ELSE.
      MOVE : 1   TO IT_ZSIDSHSD_CAL-BPUMN,
             1   TO IT_ZSIDSHSD_CAL-BPUMZ.
    ENDIF.
* 보험료 구하기.
    SELECT SINGLE ZFKRWAMT  INTO  IT_ZSIDSHSD_CAL-ZFKRWAMT
    FROM   ZTINS
    WHERE  ZFREQNO   EQ IT_ZSIDSHSD_CAL-ZFREQNO
    AND    ZFAMDNO   EQ ( SELECT MAX( ZFAMDNO )
                          FROM   ZTINS
                          WHERE  ZFREQNO EQ IT_ZSIDSHSD_CAL-ZFREQNO )
    AND    ZFINSEQ   EQ ( SELECT MAX( ZFINSEQ )
                          FROM   ZTINS
                          WHERE  ZFREQNO EQ IT_ZSIDSHSD_CAL-ZFREQNO ).
*C&F 금액 구하기(= 단가*수량*환율).-->외화.
    IT_ZSIDSHSD_CAL-ZFCNFU = ( IT_ZSIDSHSD-NETPR / IT_ZSIDSHSD-PEINH )
                     * IT_ZSIDSHSD-ZFQNT *
                 ( IT_ZSIDSHSD_CAL-BPUMZ / IT_ZSIDSHSD_CAL-BPUMN ).
*C&F 금액 구하기(= 단가*수량*환율).-->원화.
    IT_ZSIDSHSD_CAL-ZFCNF = IT_ZSIDSHSD_CAL-ZFCNFU
               * ( IT_ZSIDSHSD_CAL-ZFEXRT / IT_ZSIDSHSD_CAL-FFACT ).

*보험료구하기(= 수량/해당 BL 수량)*보험금.
    IT_ZSIDSHSD_CAL-ZFINS = IT_ZSIDSHSD_CAL-ZFKRWAMT
                     * ( IT_ZSIDSHSD-ZFQNT / IT_ZSIDSHSD_CAL-BLMENGE ).
*감정가격구하기(= C&F 금액 + 보험료).
    IT_ZSIDSHSD_CAL-ZFGAM =
                     IT_ZSIDSHSD_CAL-ZFCNF + IT_ZSIDSHSD_CAL-ZFINS.
*관세율구하기.
    READ TABLE IT_ZSIDSHS WITH KEY ZFCONO = IT_ZSIDSHSD-ZFCONO.
    IF SY-SUBRC EQ 0.
      IT_ZSIDSHSD_CAL-ZFBASIC = IT_ZSIDSHS-ZFCURT.
    ENDIF.

    IF IT_ZSIDSHSD_CAL-ZFBASIC IS INITIAL.
      SELECT  SINGLE ZFBASIC INTO IT_ZSIDSHSD_CAL-ZFBASIC
      FROM    ZTIMIMG09
      WHERE   STAWN   EQ  IT_ZSIDSHSD_CAL-STAWN
      AND     ZFAPLDT EQ  ( SELECT MAX( ZFAPLDT )
                            FROM   ZTIMIMG09
                            WHERE  STAWN   =  IT_ZSIDSHSD_CAL-STAWN
                            AND    ZFAPLDT LE SY-DATUM ).
      IF SY-SUBRC NE 0.
        IT_ZSIDSHSD_CAL-ZFBASIC = 8.
      ENDIF.
    ELSE.
      IT_ZSIDSHSD_CAL-ZFBASIC = 8.
    ENDIF.

*관세구하기(= 감정가/관세율).
    IT_ZSIDSHSD_CAL-ZFCUAMT = IT_ZSIDSHSD_CAL-ZFGAM
                     * ( IT_ZSIDSHSD_CAL-ZFBASIC / 100 ).
*부과세과표액(= 관세 + 감정가).
    IT_ZSIDSHSD_CAL-ZFTAX   =
                     IT_ZSIDSHSD_CAL-ZFGAM + IT_ZSIDSHSD_CAL-ZFCUAMT.
*부가세구하기( = 부과세과표액 * 0.1).
    IT_ZSIDSHSD_CAL-ZFVAT   =
                     IT_ZSIDSHSD_CAL-ZFTAX * ( 10 / 100 ).
    IT_ZSIDSHSD_CAL-ZFVAT   =  TRUNC( IT_ZSIDSHSD_CAL-ZFVAT ).

*각 자재별 관세,부가세 합하기.
    W_ZFCAMTK   = IT_ZSIDSHSD_CAL-ZFCNF    + W_ZFCAMTK.
    W_ZFCAMTU   = IT_ZSIDSHSD_CAL-ZFCNFU   + W_ZFCAMTU.
    W_ZFCAMT    = IT_ZSIDSHSD_CAL-ZFCUAMT  + W_ZFCAMT.
    W_TOT_VAT   = IT_ZSIDSHSD_CAL-ZFVAT    + W_TOT_VAT.
    W_TOT_TAX   = IT_ZSIDSHSD_CAL-ZFTAX    + W_TOT_TAX.
    W_TOT_GAM   = IT_ZSIDSHSD_CAL-ZFGAM    + W_TOT_GAM.
    APPEND IT_ZSIDSHSD_CAL.

  ENDLOOP.

  MOVE : W_TOT_TAX TO ZTIDS-FWBAS,    ">과세표준액.
         W_TOT_VAT TO ZTIDS-ZFVAAMTS, ">부가세.
         W_ZFCAMTK TO ZTIDS-ZFTBAK,   ">과세원화.
         W_ZFCAMTU TO ZTIDS-ZFTBAU,   ">과세외화.
         W_ZFCAMT  TO ZTIDS-ZFCUAMTS. ">관세.
* 관세 구하기.
*  W_ZFCAMT = ZTIDS-ZFCUAMTS + ZTIDS-ZFSCAMTS + "총관세,총특소?
*            ZTIDS-ZFDRAMTS + ZTIDS-ZFTRAMTS + "총주세,총교통?
*            ZTIDS-ZFEDAMTS + ZTIDS-ZFAGAMTS.  "총교육세,총농특?

  CLEAR : ZTIMIMG08, IT_ZSCUCLCST.
* 구매처, 지급처 SELECT.
  SELECT SINGLE H~ZFVEN  I~NAME1  I~LNRZA
  INTO   (IT_ZSCUCLCST-ZFVEN, IT_ZSCUCLCST-ZFVENNM, IT_ZSCUCLCST-ZFPAY)
  FROM   ZTIMIMG02 AS H INNER JOIN LFA1 AS I
  ON     H~ZFVEN   EQ   I~LIFNR
  WHERE  H~ZFCOTM  EQ   ZTIDS-ZFINRC.

  IF IT_ZSCUCLCST-ZFPAY IS INITIAL.
    IT_ZSCUCLCST-ZFPAY = IT_ZSCUCLCST-ZFVEN.
  ENDIF.
*>> 지급처명 DISPLAY
  SELECT SINGLE NAME1  INTO  IT_ZSCUCLCST-ZFPAYNM " 지불처?
  FROM   LFA1          WHERE LIFNR = IT_ZSCUCLCST-ZFPAY.
*>> 지급조건 DISPLAY.
  SELECT SINGLE ZTERM  INTO  IT_ZSCUCLCST-ZTERM   " Payment Term
  FROM   LFB1          WHERE LIFNR = IT_ZSCUCLCST-ZFVEN
                       AND   BUKRS = ZTBL-BUKRS.
*>> 비용명 DISPLAY.
  SELECT SINGLE ZFCDNM ZFCD5
  INTO   (IT_ZSCUCLCST-ZFCDNM, IT_ZSCUCLCST-MWSKZ)
  FROM   ZTIMIMG08
  WHERE  ZFCDTY = '006' AND   ZFCD = '001'.

  MOVE ZTCUCL-ZFBLNO     TO IT_ZSCUCLCST-ZFBLNO.
  MOVE ZTCUCL-ZFCLSEQ    TO IT_ZSCUCLCST-ZFCLSEQ.
  MOVE ZTIDS-ZFIDSDT     TO IT_ZSCUCLCST-ZFOCDT. " 발생?
  MOVE 10                TO IT_ZSCUCLCST-ZFCSQ.
  MOVE '001'             TO IT_ZSCUCLCST-ZFCSCD.
  MOVE W_ZFCAMT          TO IT_ZSCUCLCST-ZFCAMT. " 금?
  MOVE 'KRW'             TO IT_ZSCUCLCST-ZFKRW.
  MOVE ZTBL-ZFWERKS      TO IT_ZSCUCLCST-ZFWERKS. "Plant
  MOVE SY-UNAME          TO IT_ZSCUCLCST-ERNAM.
  MOVE SY-DATUM          TO IT_ZSCUCLCST-CDAT.
  MOVE SY-UNAME          TO IT_ZSCUCLCST-UNAM.
  MOVE SY-DATUM          TO IT_ZSCUCLCST-UDAT.
  IF ZTIDS-ZFITKD NE 'B'.            "관세유보부분 제?
    IF IT_ZSCUCLCST-ZFCAMT > 0.
      APPEND IT_ZSCUCLCST.
      MOVE   W_ZFCAMT  TO  ZTIDS-ZFCUAMTS.
      MOVE   W_ZFCAMT  TO  ZTIDS-ZFTXAMTS.
    ENDIF.
  ENDIF.

  CLEAR  IT_ZSCUCLCST.
  DESCRIBE  TABLE IT_ZSCUCLCST LINES LINE.
  IF LINE EQ 0.
    MOVE 10             TO IT_ZSCUCLCST-ZFCSQ.
  ELSE.
    MOVE 20             TO IT_ZSCUCLCST-ZFCSQ.
  ENDIF.
  MOVE ZTCUCL-ZFBLNO     TO IT_ZSCUCLCST-ZFBLNO.
  MOVE ZTCUCL-ZFCLSEQ    TO IT_ZSCUCLCST-ZFCLSEQ.
  MOVE ZTIDS-ZFIDSDT     TO IT_ZSCUCLCST-ZFOCDT. " 발생?
  MOVE '002'             TO IT_ZSCUCLCST-ZFCSCD.

*>> 통관수수료 COMPUTE.
  CALL FUNCTION 'ZIM_CC_TAX_CALCULATE'
       CHANGING
            ZTIDS = ZTIDS.

  IF NOT ZTIDS-ZFCUTAMT IS INITIAL AND
     ZTIMIMG00-ZFPSMS   EQ '1'.
    MOVE ZTIDS-ZFCUTAMT    TO IT_ZSCUCLCST-ZFCAMT. "금?
    MOVE 'KRW'             TO IT_ZSCUCLCST-ZFKRW.
*>> 구매처, 지급처 DISPLAY
    SELECT SINGLE H~ZFVEN  I~NAME1  I~LNRZA
    INTO  (IT_ZSCUCLCST-ZFVEN, IT_ZSCUCLCST-ZFVENNM,
           IT_ZSCUCLCST-ZFPAY)
    FROM   ZTIMIMG10  AS  H  INNER JOIN  LFA1  AS  I
    ON     H~ZFVEN    EQ  I~LIFNR
    WHERE  H~ZFCUT    EQ  ZTIDS-ZFCUT.

    SELECT SINGLE NAME1  INTO  IT_ZSCUCLCST-ZFPAYNM " 지불처?
    FROM   LFA1          WHERE LIFNR = IT_ZSCUCLCST-ZFPAY.
*>> 지급조건 DISPLAY
    SELECT SINGLE ZTERM  INTO  IT_ZSCUCLCST-ZTERM
    FROM   LFB1
    WHERE  LIFNR  EQ  IT_ZSCUCLCST-ZFVEN
    AND    BUKRS  EQ  ZTBL-BUKRS.
*>> 비용명, TAX CODE DISPLAY
    CLEAR  ZTIMIMG08.
    SELECT SINGLE ZFCD5 ZFCDNM
    INTO   (IT_ZSCUCLCST-MWSKZ, IT_ZSCUCLCST-ZFCDNM)
    FROM   ZTIMIMG08
    WHERE  ZFCDTY  EQ  '006'   AND  ZFCD  EQ  '002'.

    MOVE ZTBL-ZFWERKS      TO IT_ZSCUCLCST-ZFWERKS. "Plant
    MOVE SY-UNAME          TO IT_ZSCUCLCST-ERNAM.
    MOVE SY-DATUM          TO IT_ZSCUCLCST-CDAT.
    MOVE SY-UNAME          TO IT_ZSCUCLCST-UNAM.
    MOVE SY-DATUM          TO IT_ZSCUCLCST-UDAT.
    IF IT_ZSCUCLCST-ZFCAMT > 0.
      APPEND IT_ZSCUCLCST.
    ENDIF.
  ENDIF.





*>> 선급부가세 COMPUTE
  CLEAR  IT_ZSCUCLCST.
  DESCRIBE  TABLE IT_ZSCUCLCST LINES LINE.
  IF LINE EQ 0.
    MOVE 10             TO IT_ZSCUCLCST-ZFCSQ.
  ELSEIF LINE EQ 1.
    MOVE 20             TO IT_ZSCUCLCST-ZFCSQ.
  ELSE.
    MOVE 30             TO IT_ZSCUCLCST-ZFCSQ.
  ENDIF.
  MOVE ZTCUCL-ZFBLNO     TO IT_ZSCUCLCST-ZFBLNO.
  MOVE ZTCUCL-ZFCLSEQ    TO IT_ZSCUCLCST-ZFCLSEQ.
  MOVE ZTIDS-ZFIDSDT     TO IT_ZSCUCLCST-ZFOCDT. " 발생?
  MOVE 30                TO IT_ZSCUCLCST-ZFCSQ.
  MOVE '003'             TO IT_ZSCUCLCST-ZFCSCD.
  MOVE W_TOT_VAT         TO IT_ZSCUCLCST-ZFCAMT. " 금?
  MOVE 'KRW'             TO IT_ZSCUCLCST-ZFKRW.

*>> 구매처, 지급처 DISPLAY
  SELECT SINGLE  H~ZFVEN  I~NAME1  I~LNRZA
  INTO  (IT_ZSCUCLCST-ZFVEN, IT_ZSCUCLCST-ZFVENNM, IT_ZSCUCLCST-ZFPAY)
  FROM   ZTIMIMG02  AS  H INNER JOIN LFA1 AS  I
  ON     H~ZFVEN    EQ  I~LIFNR
  WHERE  H~ZFCOTM   EQ  ZTIDS-ZFINRC.

  SELECT SINGLE NAME1  INTO  IT_ZSCUCLCST-ZFPAYNM " 지불처?
  FROM   LFA1          WHERE LIFNR = IT_ZSCUCLCST-ZFPAY.
*>> 지급조건 DISPLAY.
  SELECT SINGLE ZTERM  INTO IT_ZSCUCLCST-ZTERM
  FROM   LFB1
  WHERE  LIFNR  EQ  IT_ZSCUCLCST-ZFVEN
  AND    BUKRS  EQ  ZTBL-BUKRS.
*>> 비용명, TAX CODE DISPLAY
  CLEAR  ZTIMIMG08.
  SELECT SINGLE  ZFCD5   ZFCDNM
  INTO   (IT_ZSCUCLCST-MWSKZ, IT_ZSCUCLCST-ZFCDNM)
  FROM   ZTIMIMG08
  WHERE  ZFCDTY  =  '006'  AND  ZFCD  =  '003'.

  MOVE ZTBL-ZFWERKS      TO IT_ZSCUCLCST-ZFWERKS. "Plant
  MOVE SY-UNAME          TO IT_ZSCUCLCST-ERNAM.
  MOVE SY-DATUM          TO IT_ZSCUCLCST-CDAT.
  MOVE SY-UNAME          TO IT_ZSCUCLCST-UNAM.
  MOVE SY-DATUM          TO IT_ZSCUCLCST-UDAT.
  IF ZTIDS-ZFITKD NE 'B'.            "관세유보부분 제?
    IF IT_ZSCUCLCST-ZFCAMT > 0.
      APPEND IT_ZSCUCLCST.
      MOVE  W_TOT_VAT  TO  ZTIDS-ZFVAAMTS.
      ADD   W_TOT_VAT  TO  ZTIDS-ZFTXAMTS.
    ENDIF.
  ENDIF.

ENDFORM.                    " CALC_ZTCUCLCST_TO_IT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR6412INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR6412INPUT INPUT.
  CASE OK-CODE.
    WHEN 'DEL1'.
      DELETE IT_ZSIDRCRIT_S WHERE ZFMARK = 'X'.
*         DELETE IT_ZSIDRCRIT WHERE ZFBLNO = IT_ZSIDRCRIT_S-ZFBLNO
*                             AND  ZFCLSEQ = IT_ZSIDRCRIT_S-ZFCLSEQ
*                             AND  ZFCONO  = IT_ZSIDRCRIT_S-ZFCONO
*                             AND  ZFRONO  = IT_ZSIDRCRIT_S-ZFRONO.
  ENDCASE.
ENDMODULE."USER_COMMAND_SCR6412INPUT INPUT

*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCR9910  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_DOC_SCR9910 INPUT.

  PERFORM  P2000_OK_CODE_PROCESS.

* 모선명 입력 CHECK.
  IF  ZTMSHD-ZFMSNM  IS  INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTMSHD' 'ZFMSNM'.
  ENDIF.
  IF  ZTMSHD-EKGRP   IS  INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTMSHD' 'EKGRP'.
  ENDIF.

ENDMODULE.                 " READ_DOC_SCR9910  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9910  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9910 INPUT.

  PERFORM  P1000_READ_MSHD.
  PERFORM  P1000_SORT_MSHD.
* MKIM 2001.05.14
  SET PARAMETER ID 'ZPAPRTC' FIELD 'KR'.

  SET SCREEN 9911.  LEAVE TO SCREEN 9911.
ENDMODULE.                 " USER_COMMAND_SCR9910  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9911  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9911 INPUT.

  CLEAR W_NEW_STATUS.
  CASE  OK-CODE.
    WHEN 'BACK' OR 'EXIT'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'NEWL'.           " 검?
      W_NAME    =  ZTMSHD-ZFMSNM.
      W_EKGRP   =  ZTMSHD-EKGRP.

      REFRESH : IT_ZSMSIT,
                IT_ZSMSCST,
                IT_ZSMSIT_ORG[],
                IT_ZSMSCST_ORG[].
      CLEAR  :   ZTMSHD, *ZTMSHD.
      ZTMSHD-ZFMSNM = W_NAME.
      ZTMSHD-EKGRP  = W_EKGRP.
      W_STATUS  =  C_REQ_C.
      SET SCREEN 9912. LEAVE TO SCREEN 9912.
    WHEN 'DISP'.           " 세부사?
      READ  TABLE  IT_ZSMSHD  WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC  NE  0. MESSAGE  S766. EXIT. ENDIF.
      PERFORM  P2000_READ_MSHD.
      PERFORM  P2000_SORT_MSHD.
      W_STATUS  =  C_REQ_D.
      SET SCREEN 9912. LEAVE TO SCREEN 9912.
    WHEN 'CHGE'.
      READ  TABLE  IT_ZSMSHD  WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC  NE  0. MESSAGE  S766. EXIT. ENDIF.
      PERFORM  P2000_READ_MSHD.
      PERFORM  P2000_SORT_MSHD.
      W_STATUS  =  C_REQ_U.
      SET SCREEN 9912. LEAVE TO SCREEN 9912.
    WHEN 'DELE'.
      W_OK_CODE = OK-CODE.
      PERFORM  P3000_MSDATA_DELETE.
      PERFORM  P1000_READ_MSHD.
      PERFORM  P1000_SORT_MSHD.
      MESSAGE S756.
    WHEN 'MKAL'.
      W_OK_GUBN = '3'.
      PERFORM  P2000_SET_ROW_MARK.
    WHEN 'MKLO'.
      W_OK_GUBN = '3'.
      PERFORM  P2000_SET_ROW_MARK.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR9911  INPUT
*&---------------------------------------------------------------------*
*&      Module  MSHD_CHECK_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MSHD_CHECK_DATA INPUT.

  IF ZTMSHD-ZFSHSDF IS INITIAL.
    MESSAGE E459.
    EXIT.
  ENDIF.

  IF ZTMSHD-ZFSHSDT IS INITIAL.
    MESSAGE E460.
    EXIT.
  ENDIF.

  IF ZTMSHD-ZFETA IS INITIAL.
    MESSAGE E461.
    EXIT.
  ENDIF.

* 선적기간이 도착일보다 클수 없음.
  IF ZTMSHD-ZFSHSDF > ZTMSHD-ZFSHSDT.
    MESSAGE E453.
  ENDIF.

  IF  ZTMSHD-ZFSHSDT  >  ZTMSHD-ZFETA.
    MESSAGE E454.
  ENDIF.

ENDMODULE.                 " MSHD_CHECK_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  LCBL_GET_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LCBL_GET_DATA INPUT.

  IF  ZSMSIT-ZFMSNO IS INITIAL.
    ZSMSIT-ZFMSNO = ZTMSHD-ZFMSNO.
  ENDIF.

  IF  NOT ( ZSMSIT-ZFREQNO IS INITIAL ).
    SELECT SINGLE *
    FROM   ZTREQHD
    WHERE  ZFREQNO  =  ZSMSIT-ZFREQNO.

    IF  SY-SUBRC  NE  0.
      MESSAGE  E455 WITH ZSMSIT-ZFREQNO.
    ELSE.
      MOVE ZTREQHD-ZFOPNNO  TO ZSMSIT-ZFOPNNO.
      MOVE ZTREQHD-ZFREQTY  TO ZSMSIT-ZFREQTY.
      MOVE ZTREQHD-EBELN    TO ZSMSIT-EBELN.
      MOVE ZTREQHD-ZFLASTSD TO ZSMSIT-ZFLASTSD.
      MOVE ZTREQHD-ZFLASTED TO ZSMSIT-ZFLASTED.
    ENDIF.
  ENDIF.
ENDMODULE.                 " LCBL_GET_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_9912_1_UPDATE_SCR9912  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_9912_1_UPDATE_SCR9912 INPUT.

*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSMSIT INDEX TC_9912_1-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR : W_DEL_MARK, IT_ZSMSIT.

  MOVE-CORRESPONDING  ZSMSIT  TO  IT_ZSMSIT.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DEL1'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSMSIT-ELOEK.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSMSIT-ELOEK.
  ENDIF.
  MOVE : W_ROW_MARK      TO IT_ZSMSIT-ZFMARK.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSMSIT   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSMSIT.
  ENDIF.

ENDMODULE.                 " TC_9912_1_UPDATE_SCR9912  INPUT
*&---------------------------------------------------------------------*
*&      Module  AMT_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE AMT_DATA_CHECK INPUT.
*>>> 2001.03.30 KSB INSERT
  IF ZSMSCST-ZFAPRTC IS INITIAL AND     "도착?
     ZSMSCST-ZFETA   IS INITIAL AND     "도착?
     ZSMSCST-LIFNR   IS INITIAL AND     "공급?
     ZSMSCST-ZFCARGO IS INITIAL AND     "하역?
     ZSMSCST-ZFCCGB  IS INITIAL AND     "조/출 구?
     ZSMSCST-ZFSHAMT IS INITIAL AND     "조출/체선?
     ZSMSCST-ZFPER   IS INITIAL AND     "분배?
     ZSMSCST-ZFVAT   IS INITIAL AND     "VAT
     ZSMSCST-ZFUNCO  IS INITIAL AND     "미수?
     ZSMSCST-ZFNOPY  IS INITIAL AND     "미지?
     ZSMSCST-ZFCST   IS INITIAL AND     "영업외비?
     ZSMSCST-ZFPROF  IS INITIAL AND     "영업외수?
     ZSMSCST-WAERS   IS INITIAL AND     "통?
     ZSMSCST-ZFCAMT  IS INITIAL AND     "비용금?
     ZSMSCST-ZFEXRT  IS INITIAL AND     "환?
     ZSMSCST-ZTERM   IS INITIAL AND     "지급조?
     ZSMSCST-ZFWERKS IS INITIAL AND     "PLANT
     ZSMSCST-MWSKZ   IS INITIAL AND     "세?
     ZSMSCST-ZFKOSTL IS INITIAL AND     "COST CENTER.
     ZSMSCST-CDAT    IS INITIAL.
     EXIT.
  ENDIF.

* DATA VALIDATION CHECK!
  IF ZSMSCST-ZFAPRTC IS INITIAL.
*>> 2001.03.30 KSB MODIFY.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSMSCST' 'ZFAPRTC'.
  ENDIF.

  IF ZSMSCST-ZFETA IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSMSCST' 'ZFETA'.
    EXIT.
  ENDIF.

  IF ZSMSCST-LIFNR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSMSCST' 'LIFNR'.
    EXIT.
  ENDIF.

  IF ZSMSCST-ZFCARGO IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSMSCST' 'ZFCARGO'.
    EXIT.
  ENDIF.

  IF ZSMSCST-ZFCCGB IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSMSCST' 'ZFCCGB'.
    EXIT.
  ENDIF.

  IF ZSMSCST-ZFWERKS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSMSCST' 'ZFWERKS'.
    EXIT.
  ENDIF.

  IF ZSMSCST-ZFKOSTL IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSMSCST' 'ZFKOSTL'.
    EXIT.
  ENDIF.

  IF ZSMSCST-ZTERM IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSMSCST' 'ZTERM'.
    EXIT.
  ENDIF.

  IF ZSMSCST-ZFCCGB  =  'A' . "세금코드는 조출일경우만 CHECK
    IF ZSMSCST-MWSKZ IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSMSCST' 'MWSKZ'.
      EXIT.
    ENDIF.
  ENDIF.

* 조출/체선료 입력요.
  IF ZSMSCST-ZFSHAMT EQ 0.
    MESSAGE E465.
    EXIT.
  ENDIF.

* 체선일 경우는 미지급 및 부가세 입력불?
  IF ZSMSCST-ZFCCGB  =  'D' .
    IF ZSMSCST-ZFVAT GT 0  OR  ZSMSCST-ZFPROF GT 0.
      MESSAGE E508  WITH 'Demurrage' 'Non-operating income' 'VAT'.
    ENDIF.
  ENDIF.

* 조출/체선 료와 미수금 + 영업외비용, 미지급 + 영업외수익의 합이 일치.
  IF ZSMSCST-ZFCCGB  =  'A'.
    IF ZSMSCST-ZFVAT GT 0.
      W_AMT_CHA = ( ZSMSCST-ZFUNCO + ZSMSCST-ZFCST + ZSMSCST-ZFVAT )
                - ( ZSMSCST-ZFPROF + ZSMSCST-ZFNOPY ).
    ELSE.
      W_AMT_CHA  =  ZSMSCST-ZFSHAMT + ZSMSCST-ZFVAT -
                   ( ZSMSCST-ZFNOPY + ZSMSCST-ZFPROF ).
    ENDIF.
    IF W_AMT_CHA  NE  0. MESSAGE  E457. ENDIF.

  ELSE.
    W_AMT_CHA  =  ZSMSCST-ZFSHAMT  -
                  ( ZSMSCST-ZFUNCO + ZSMSCST-ZFCST ).

    IF W_AMT_CHA  NE  0.
      MESSAGE  E458.
    ENDIF.
  ENDIF.

ENDMODULE.                 " AMT_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_9912_2_UPDATE_SCR9912  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_9912_2_UPDATE_SCR9912 INPUT.

*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSMSCST INDEX TC_9912_2-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZSMSCST TO IT_ZSMSCST.

  IF NOT ( W_ROW_MARK1 IS INITIAL ) AND OK-CODE = 'DEL2'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSMSCST-ELOEK.
  ENDIF.

  IF NOT ( W_ROW_MARK1 IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSMSCST-ELOEK.
  ENDIF.
  MOVE : W_ROW_MARK1      TO IT_ZSMSCST-ZFMARK.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSMSCST   INDEX W_TABIX.
  ELSE.
    IF NOT ( IT_ZSMSCST-ZFAPRTC IS INITIAL ).
      APPEND  IT_ZSMSCST.
    ENDIF.
  ENDIF.

ENDMODULE.                 " TC_9912_2_UPDATE_SCR9912  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9912  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9912 INPUT.

  CLEAR : W_OK_GUBN, W_CHK_CNT.

  CASE  OK-CODE.
    WHEN 'DISP'.
      W_STATUS = C_REQ_D.
      PERFORM P2000_SAVE_PROCESS.
      PERFORM P2000_READ_MSHD.
      PERFORM P2000_SORT_MSHD.
    WHEN 'BACK' OR 'EXIT'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'NEWL'.
      IF W_STATUS = C_REQ_U.
        PERFORM P2000_SET_LOCK_ZTMSHD USING 'U'.
      ENDIF.

      W_STATUS = C_REQ_C.
      CLEAR W_NAME.
      W_NAME = ZTMSHD-ZFMSNM.

* BUFFER CLEAR.
      REFRESH : IT_ZSMSIT,  IT_ZSMSIT_ORG[],
                IT_ZSMSCST, IT_ZSMSCST_ORG[].
      CLEAR   : ZTMSHD, *ZTMSHD, ZSMSCST, ZSMSIT,
                IT_ZSMSIT, IT_ZSMSCST.
      ZTMSHD-ZFMSNM = W_NAME.
    WHEN 'SAVE'.           " 저장.
      CLEAR : ANTWORT.
      W_OK_CODE = OK-CODE.
      IF SY-TCODE EQ 'ZIMMS1'.
        PERFORM P2000_SET_MODIFY_CHECK  USING W_GUBUN.
      ENDIF.

      IF W_GUBUN EQ 'Y'.
        PERFORM P2000_SAVE_PROCESS.
        PERFORM P1000_READ_MSHD.
        PERFORM P1000_SORT_MSHD.
        SET SCREEN 9911. LEAVE TO SCREEN 9911.
      ELSE.
        MESSAGE I973.
      ENDIF.

    WHEN 'DELE'.           " 삭?
      CLEAR : ANTWORT,
              W_GUBUN.
      PERFORM P2000_DEL_ALL_DATA USING W_GUBUN.

      IF W_GUBUN  =  'N'.
        MESSAGE E462.
      ELSE.
        MESSAGE S756.
        PERFORM P1000_READ_MSHD.
        PERFORM P1000_SORT_MSHD.
        SET SCREEN 9911. LEAVE TO SCREEN 9911.
      ENDIF.
    WHEN 'CHGE'.           " 변?
      W_STATUS  =  C_REQ_U.
      PERFORM P2000_SET_LOCK_ZTMSHD USING 'L'.
    WHEN 'DEL1'.          "세부화면 삭제.
      W_OK_GUBN = '1'.
      PERFORM P2000_DATA_DELETE.
    WHEN 'DEL2'.
      W_OK_GUBN = '2'.
      PERFORM P2000_DATA_DELETE.
    WHEN 'MKA1' OR 'MKL1'.     "MARK ALL, MARK DEL.
      W_OK_GUBN = '1'.
      PERFORM P2000_SET_ROW_MARK.
    WHEN 'MKA2' OR 'MKL2'.
      W_OK_GUBN = '2'.
      PERFORM P2000_SET_ROW_MARK.
    WHEN 'CRT1'.
      REFRESH : IT_ZSMSIT, IT_ZSMSIT_DEL.
    WHEN 'CRT2'.
      W_NEW_STATUS = C_REQ_C.
      REFRESH : IT_ZSMSCST, IT_ZSMSCST_DEL.
    WHEN 'REF1'.
      W_OK_GUBN = '1'.
      PERFORM P2000_DATA_REFRESH.
    WHEN 'REF2'.
      W_OK_GUBN = '2'.
      PERFORM P2000_DATA_REFRESH.
    WHEN 'DELC'.
      W_OK_GUBN = '3'.
      PERFORM P2000_DATA_REFRESH.
    WHEN 'LCDP'.                " 수입의뢰문서 조회.
      LOOP AT IT_ZSMSIT WHERE ZFMARK = 'X'.
        W_CHK_CNT = W_CHK_CNT + 1.
      ENDLOOP.
      IF W_CHK_CNT > 1. MESSAGE S965. EXIT. ENDIF.
      IF W_CHK_CNT = 0. MESSAGE S766. EXIT. ENDIF.
      PERFORM  P2000_LC_DOC_DISPLAY     USING  IT_ZSMSIT-ZFREQNO
                                                ''.
    WHEN 'SCHG'.                " 상태변경.
      LOOP AT IT_ZSMSCST WHERE ZFMARK = 'X'.
        W_TABIX   = SY-TABIX.
        W_CHK_CNT = W_CHK_CNT + 1.
      ENDLOOP.
      IF W_CHK_CNT > 1. MESSAGE S965. EXIT. ENDIF.
      IF W_CHK_CNT = 0. MESSAGE S766. EXIT. ENDIF.

      MOVE-CORRESPONDING IT_ZSMSCST TO ZSMSCST.
      PERFORM P2000_DOC_CHANGE.
      IF ANTWORT = 'Y'.
        MOVE ' ' TO ZSMSCST-BELNR.
        MOVE ' ' TO ZSMSCST-GJAHR.
        MOVE-CORRESPONDING ZSMSCST TO IT_ZSMSCST.
        MODIFY IT_ZSMSCST INDEX W_TABIX.
      ENDIF.

    WHEN 'DOCDP'.               " 회계문서조회.
      LOOP AT IT_ZSMSCST WHERE ZFMARK = 'X'.
        W_CHK_CNT = W_CHK_CNT + 1.
      ENDLOOP.
      IF W_CHK_CNT > 1. MESSAGE S965. EXIT. ENDIF.
      IF W_CHK_CNT = 0. MESSAGE S766. EXIT. ENDIF.

      PERFORM  P2000_FI_DOC_DISPLAY      USING IT_ZSMSCST-BELNR
                                               IT_ZSMSCST-BUKRS
                                               IT_ZSMSCST-GJAHR.
    WHEN 'DOCVT'.               " 회계문서조회.
      LOOP AT IT_ZSMSCST WHERE ZFMARK = 'X'.
        W_CHK_CNT = W_CHK_CNT + 1.
      ENDLOOP.
      IF W_CHK_CNT > 1. MESSAGE S965. EXIT. ENDIF.
      IF W_CHK_CNT = 0. MESSAGE S766. EXIT. ENDIF.

      PERFORM  P2000_FI_DOC_DISPLAY      USING IT_ZSMSCST-BELNRV
                                               IT_ZSMSCST-BUKRS
                                               IT_ZSMSCST-GJAHRV.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR9912  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZFCUT_SCR6212  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_ZFCUT_SCR6212 INPUT.

  IF ZTIDR-ZFCUT NE SPACE.
     SELECT  SINGLE H~NAME1
     INTO    ZTIDR-ZFAPNM
     FROM    LFA1  AS  H  INNER JOIN  ZTIMIMG10 AS I
     ON      H~LIFNR      EQ    I~ZFVEN
     WHERE   I~ZFCUT      EQ    ZTIDR-ZFCUT.

     IF SY-SUBRC NE 0. MESSAGE E768 WITH ZTIDR-ZFCUT. ENDIF.
  ENDIF.

ENDMODULE.                 " GET_ZFCUT_SCR6212  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZFCUT_SCR7412  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_ZFCUT_SCR7412 INPUT.

  IF ZTIDS-ZFCUT NE SPACE.
    SELECT  SINGLE H~NAME1
    INTO    ZTIDS-ZFAPNM
    FROM    LFA1  AS  H  INNER JOIN  ZTIMIMG10 AS I
    ON      H~LIFNR      EQ    I~ZFVEN
    WHERE   I~ZFCUT      EQ    ZTIDS-ZFCUT.

    IF SY-SUBRC NE 0. MESSAGE E768 WITH ZTIDS-ZFCUT. ENDIF.
  ENDIF.

ENDMODULE.                 " GET_ZFCUT_SCR7412  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_8210_UPDATE_SCR8210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_8210_UPDATE_SCR8210 INPUT.

  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  READ TABLE IT_ZSPMTIV INDEX TC_8210-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.

  MOVE-CORRESPONDING ZSPMTIV TO IT_ZSPMTIV.
  IF OK-CODE = 'MKA1'.
    MOVE   'X'             TO W_ROW_MARK.
  ENDIF.
  IF OK-CODE = 'MKL1'.
    CLEAR  W_ROW_MARK.
  ENDIF.
  MOVE W_ROW_MARK TO IT_ZSPMTIV-ZFMARK.

  IF W_OLD_SUBRC = 0.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSPMTIV-ZFPNAM
*                                            IT_ZSPMTIV-ZFIVAMC.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSPMTIV-ZFPBAMT
*                                            IT_ZSPMTIV-ZFIVAMC.
*    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSPMTIV-ZFIVAMT
*                                            IT_ZSPMTIV-ZFIVAMC.
    MODIFY IT_ZSPMTIV   INDEX TC_8210-CURRENT_LINE.
  ENDIF.

*  IF IT_ZSPMTIV-ZFPNAM > IT_ZSPMTIV-ZFPBAMT.
*    MESSAGE E848.
*    EXIT.
*  ENDIF.

ENDMODULE.                 " TC_8210_UPDATE_SCR8210  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR8210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LINE_SCR8210 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_8210-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR8210  INPUT
*&---------------------------------------------------------------------*
*&      Module  AMT_COMPUTE_SCR9912  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE AMT_COMPUTE_SCR9912 INPUT.

* 조출/체선료 입력요.
  IF ZSMSCST-ZFSHAMT EQ 0.
    MESSAGE E465.
    EXIT.
  ENDIF.
* 조출/체선 구분 입력요.
  IF ZSMSCST-ZFCCGB IS INITIAL.
    MESSAGE E463 WITH 'Classfication of Dispatch/Demurrage'.
    EXIT.
  ENDIF.

* 분배율이 입력되었으면 분배율에 따라서 금액을 결정.
  CLEAR : ZSMSCST-ZFUNCO, ZSMSCST-ZFNOPY, ZSMSCST-ZFPROF, ZSMSCST-ZFCST.

* 조출료 인 경우는...
  IF ZSMSCST-ZFCCGB  =  'A'.
    IF ZSMSCST-ZFVAT GT 0.
      ZSMSCST-ZFUNCO = ZSMSCST-ZFSHAMT.
      ZSMSCST-ZFPROF = ZSMSCST-ZFSHAMT.
      ZSMSCST-ZFNOPY = ZSMSCST-ZFSHAMT * ( ZSMSCST-ZFPER / 100 ).
      ZSMSCST-ZFCST  = ZSMSCST-ZFNOPY.
      ZSMSCST-ZFNOPY = ZSMSCST-ZFCST + ZSMSCST-ZFVAT.
    ELSE.
      ZSMSCST-ZFUNCO = ZSMSCST-ZFSHAMT.
      ZSMSCST-ZFNOPY = ZSMSCST-ZFSHAMT * ( ZSMSCST-ZFPER / 100 ).
      ZSMSCST-ZFPROF = ZSMSCST-ZFSHAMT - ZSMSCST-ZFNOPY.
      ZSMSCST-ZFCST  = 0.
    ENDIF.
  ELSE.
    ZSMSCST-ZFNOPY = ZSMSCST-ZFSHAMT.
    ZSMSCST-ZFUNCO = ZSMSCST-ZFSHAMT * ( ZSMSCST-ZFPER / 100 ).
    ZSMSCST-ZFCST  = ZSMSCST-ZFSHAMT - ZSMSCST-ZFUNCO.
    ZSMSCST-ZFPROF = 0.
  ENDIF.

ENDMODULE.                 " AMT_COMPUTE_SCR9912  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFIVNO_CHECK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZFIVNO_CHECK_SCRCOM INPUT.

  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZSREQHD-EBELN   IS INITIAL AND   " P/O NO를 입력하지 않을 경우.
     ZSREQHD-ZFREQNO IS INITIAL AND   " 관리번호가 입력하지 않을 경우.
*     ZSREQHD-ZFOPNNO IS INITIAL AND   " 문서번호가 입력하지 않을 경우.
     ZSIV-ZFIVNO     IS INITIAL.
    MESSAGE E066.
  ENDIF.

  IF NOT ZSREQHD-EBELN IS INITIAL.      " 관리번호가 입력하지 않을 경?
* P/O NO에 Count
    SELECT COUNT( DISTINCT ZFIVNO ) INTO  W_COUNT
                      FROM  ZTIVIT
                      WHERE EBELN EQ ZSREQHD-EBELN.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E632 WITH ZSREQHD-EBELN.
      WHEN 1.
        SELECT ZFIVNO  INTO  ZSIV-ZFIVNO UP TO 1 ROWS
                       FROM  ZTIVIT
                       WHERE EBELN EQ ZSREQHD-EBELN.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* 입고 문서 READ PERFORM .
    ZTIV-ZFIVNO = ZSIV-ZFIVNO.
    PERFORM   P1000_READ_IV_DOC.
*     CASE SY-TCODE(5).
*        WHEN 'ZIM32'.
*           IF ZTIV-ZFGRST EQ 'Y'.
*              MESSAGE E422 WITH ZTIV-ZFIVNO
*                           'Completed G/R' 'change'.
*           ENDIF.
*        WHEN 'ZIM33'.
*        WHEN 'ZIM34'.
*     ENDCASE.
*     IF NOT ( ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU' ).
*        MESSAGE E633 WITH ZSIV-ZFIVNO ZTIV-ZFREQTY.
*     ENDIF.
    EXIT.
  ENDIF.

  IF NOT ZSREQHD-ZFREQNO IS INITIAL.      " 수입의뢰관리번호.
* P/O NO에 Count
    SELECT COUNT( DISTINCT ZFIVNO ) INTO  W_COUNT
                      FROM  ZTIVIT
                      WHERE ZFREQNO EQ ZSREQHD-ZFREQNO.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E634 WITH ZSREQHD-ZFREQNO.
      WHEN 1.
        SELECT ZFIVNO  INTO  ZSIV-ZFIVNO UP TO 1 ROWS
                       FROM  ZTIVIT
                       WHERE ZFREQNO EQ ZSREQHD-ZFREQNO.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_DOC_ITEM_SELECT1.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* 입고 문서 READ PERFORM .
    ZTIV-ZFIVNO = ZSIV-ZFIVNO.
    PERFORM   P1000_READ_IV_DOC.
*     CASE SY-TCODE(5).
*        WHEN 'ZIM32'.
*           IF ZTIV-ZFGRST EQ 'Y'.
*              MESSAGE E422 WITH ZTIV-ZFIVNO
*                           'Completed G/R' 'change'.
*           ENDIF.
*        WHEN 'ZIM33'.
*        WHEN 'ZIM34'.
*     ENDCASE.
*     IF NOT ( ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU' ).
*        MESSAGE E633 WITH ZSIV-ZFIVNO ZTIV-ZFREQTY.
*     ENDIF.
    EXIT.
  ENDIF.

  IF NOT ZSIV-ZFIVNO IS INITIAL.
    ANTWORT = 'N'.
    W_COUNT = '1'.
* 입고 문서 READ PERFORM .
    ZTIV-ZFIVNO = ZSIV-ZFIVNO.
    PERFORM   P1000_READ_IV_DOC.
*     CASE SY-TCODE(5).
*        WHEN 'ZIM32'.
*           IF ZTIV-ZFGRST EQ 'Y'.
*              MESSAGE E422 WITH ZTIV-ZFIVNO
*                           'completed G/R' 'change'.
*           ENDIF.
*        WHEN 'ZIM33'.
*        WHEN 'ZIM34'.
*     ENDCASE.
*     IF NOT ( ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU' ).
*        MESSAGE E633 WITH ZSIV-ZFIVNO ZTIV-ZFREQTY.
*     ENDIF.
  ENDIF.

ENDMODULE.                 " ZFIVNO_CHECK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  INCOM_GET_NAME_SCR4102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INCOM_GET_NAME_SCR4102 INPUT.

   CLEAR : ZTINSB-ZFINSU1, ZTINSB-ZFINSU2.

   IF W_STATUS EQ C_OPEN_C.
      IF ZTINSB-ZFINCOM IS INITIAL.
         PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINSB' 'ZFINCOM'.
      ENDIF.
   ENDIF.

* 피보험자 상호1, 피보험자 상호2, EDI 식별자, 손해보험회사 Vendor
   SELECT  SINGLE NAME1  NAME2
   INTO    (ZTINSB-ZFINSU1, ZTINSB-ZFINSU2)
   FROM    LFA1
   WHERE   LIFNR     EQ  ZTINSB-ZFINCOM.

   MOVE  ZTINSB-ZFINCOM  TO  ZTINSB-ZFOPCD.

ENDMODULE.                 " INCOM_GET_NAME_SCR4102  INPUT
