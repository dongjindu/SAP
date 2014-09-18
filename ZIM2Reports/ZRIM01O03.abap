*----------------------------------------------------------------------*
***INCLUDE ZRIM01O03 .
*----------------------------------------------------------------------*
*&  프로그램명 : 수입 B/L 관련 PBO MODULE Include                      *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2000.10.11                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR0130  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR0130 OUTPUT.
  IF PROGRAM IS INITIAL.
    PROGRAM = SY-CPROG.
  ENDIF.

  IF ZTBL-ZFVIA EQ 'AIR'.
    DYNPRO = '0131'.
  ELSEIF ZTBL-ZFVIA EQ 'VSL' .
    IF ZTBL-ZFSHTY EQ 'F' OR
       ZTBL-ZFSHTY EQ 'L'.
      DYNPRO = '0131'.
    ELSE.
      DYNPRO = '0135'.
    ENDIF.
  ENDIF.

  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR0130  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR0131  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR0131 OUTPUT.
  IF PROGRAM IS INITIAL.
    PROGRAM = SY-CPROG.        " Program SET
  ENDIF.

  IF ZTBL-ZFVIA EQ 'AIR'.
    DYNPRO = '0132'.
  ELSEIF ZTBL-ZFVIA EQ 'VSL' .
    IF ZTBL-ZFSHTY EQ 'F'.               " Full Container Cargo..
      DYNPRO = '0133'.
    ELSEIF ZTBL-ZFSHTY EQ 'L'.           " Less Container Cargo..
      DYNPRO = '0134'.
    ENDIF.
  ENDIF.

  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR0131  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0131  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0131 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSBLCST LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0131-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_0131-TOP_LINE.
    TC_0131-LINES = G_PARAM_LINE + 10.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0131 OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0131_MARK_SCR0131  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0131_MARK_SCR0131 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0131-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_0131_MARK_SCR0131  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0131_SCR0131  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0131_SCR0131 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0131-CURRENT_LINE GT TC_0131-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSBLCST   INDEX TC_0131-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSBLCST   TO ZSBLCST.     " DATA MOVE
    ZSBLCST-ZFCDNM    =  IT_ZSBLCST-ZFCDNM.
    MOVE: IT_ZSBLCST-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0131_SCR0131  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_SETTING_SUBSCR  OUTPUT
*&---------------------------------------------------------------------*
*       SCREEN 132 133 134.
*----------------------------------------------------------------------*
MODULE INIT_SETTING_SUBSCR OUTPUT.

  CHECK W_FIRST_CHK IS INITIAL.

  W_FIRST_CHK = 'X'.
  CLEAR : WA_BLCALC.

  ">> Local Currency Get.
  SELECT SINGLE * FROM T001 WHERE BUKRS EQ ZTBL-BUKRS.

*> Basic/Upgrade.(AIR)
  IF ZTBL-ZFVIA = 'AIR'.
    IF ZTBL-ZFDFUP IS INITIAL.
      RB_DF = 'X'.
      CLEAR RB_UP.
    ELSEIF ZTBL-ZFDFUP EQ 'X'.
      CLEAR RB_DF.
      RB_UP = 'X'.
    ENDIF.
  ENDIF.

  MOVE : ZTBL-ZFTRCUR  TO WA_BLCALC-BLCURR1,
         ZTBL-ZFTRCUR  TO WA_BLCALC-BLCURR2.

*> Basic Rate
  IF NOT ZTBL-ZFRTCD IS INITIAL.
    IF ZTBL-ZFVIA EQ 'AIR'.
      SELECT SINGLE ZFCDNM  INTO WA_BLCALC-ZFRTCD
                    FROM ZTIMIMG08
                   WHERE ZFCDTY = '017'
                     AND ZFCD   = ZTBL-ZFRTCD.

    ELSEIF ZTBL-ZFVIA EQ 'VSL' AND
         ( ZTBL-ZFSHTY EQ 'A'  OR ZTBL-ZFSHTY EQ 'M' ).
      SELECT SINGLE ZFCDNM  INTO WA_BLCALC-ZFRTCD
                    FROM ZTIMIMG08
                   WHERE ZFCDTY = '016'
                     AND ZFCD   = ZTBL-ZFRTCD.
    ENDIF.
  ENDIF.

*> No Display Mode
  IF W_STATUS NE C_REQ_D OR W_STATUS NE C_ADD_D OR
     W_STATUS NE C_OPEN_D.

*> Date of exchage rate Apply
    IF ZTBL-ZFEXDTT IS INITIAL.
      ZTBL-ZFEXDTT = ZTBL-ZFRETA.
    ENDIF.
    IF ZTBL-ZFEXDTT IS INITIAL.
       ZTBL-ZFEXDTT = SY-DATUM.
    ENDIF.
*> Freight Charge
    IF NOT ZTBL-ZFEXDTT IS INITIAL AND ZTBL-ZFEXRTT IS INITIAL.
      PERFORM   P2000_GET_EX_RATE_NODIALOG     USING ZTBL-ZFTRCUR
                                                     T001-WAERS
                                                     ZTBL-ZFEXDTT
                                            CHANGING ZTBL-ZFEXRTT
                                                     ZTBL-ZFFACTT.
    ENDIF.

*> Conference Net Price
    IF ZTBL-ZFCHARGE = 'X'.
      ZTBL-ZFNETPR1 = ZTBL-ZFFRE.
      ZTBL-ZFNETPR2 = ZTBL-ZFFRE.
    ELSE.
      PERFORM P3000_GET_NETPRICE_PER_WEIGHT.
    ENDIF.

*> IT_ZSBLCST Initialize.
   LOOP AT IT_ZSBLCST.
     W_TABIX = SY-TABIX.
     PERFORM P3000_ZSBLCST_INIT_SETTING.
     MODIFY IT_ZSBLCST INDEX W_TABIX.
   ENDLOOP.

  ENDIF.

  IF NOT ZTBL-ZFRTCD IS INITIAL.
*>  Freight Charge Compute
    PERFORM P3000_CALCULATE_FREIGHT.
  ENDIF.

ENDMODULE.                 " INIT_SETTING_SUBSCR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR3520  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR3520 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSCIVIT  LINES G_PARAM_LINE.   " LINE 수 GET
    TC_3520-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_3520-TOP_LINE.
    TC_3520-LINES = G_PARAM_LINE + 5.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR3520  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC3520_SCR3520  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC3520_SCR3520 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_3520-CURRENT_LINE GT TC_3520-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSCIVIT   INDEX TC_3520-CURRENT_LINE.
  IF SY-SUBRC = 0.                                    " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSCIVIT   TO ZSCIVIT.       " DATA MOVE
    MOVE: IT_ZSCIVIT-ZFMARK         TO W_ROW_MARK.    " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC3520_SCR3520  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LOOPLINES_SCR3111  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LOOPLINES_SCR3111 OUTPUT.

   LOOPLINES = SY-LOOPC.

ENDMODULE.                 " GET_LOOPLINES_SCR3111  OUTPUT
