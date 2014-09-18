*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM10I01                                                  *
*&---------------------------------------------------------------------*
*&  Program Name : Main PAI MODULE                                     *
*&  Created By   : Na Hyun Joo                                         *
*&  Created On   : 2003.10.08                                          *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.

  CASE SY-UCOMM.
     WHEN 'NO' .
        ANTWORT  =  'C'.   SET SCREEN 0.  LEAVE SCREEN.
  ENDCASE.

  IF W_STATUS EQ 'D'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  IF SY-DYNNR EQ '0100' OR SY-DYNNR EQ '0200' OR SY-DYNNR EQ '1100' OR
     SY-DYNNR EQ '1200' OR SY-DYNNR EQ '1300' OR SY-DYNNR EQ '2100' OR
     SY-DYNNR EQ '2200' OR SY-DYNNR EQ '2300'.
    SET SCREEN 0.   LEAVE SCREEN.
  ELSE.
    PERFORM P2000_SET_MESSAGE USING  SY-UCOMM.
  ENDIF.

ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT

*&---------------------------------------------------------------------*
*&      Module  CUSTOMS_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CUSTOMS_CHECK_SCR0100 INPUT.

  W_OK_CODE = OK-CODE.
  PERFORM  P2000_OK_CODE_PROCESS.

  SELECT SINGLE * FROM ZTIMIMG00.

  IF NOT ZTIDRUS-ZFIVNO  IS INITIAL.

    SELECT COUNT( DISTINCT ZFIVNO ) INTO W_COUNT
    FROM   ZTIDRUS
    WHERE  ZFIVNO    EQ  ZTIDRUS-ZFIVNO.

    CASE W_COUNT.
      WHEN 1.
        SELECT SINGLE * FROM ZTIDRUS
        WHERE  ZFIVNO   EQ   ZTIDRUS-ZFIVNO.
      WHEN OTHERS.
        MESSAGE E010(ZIM1) WITH ZTIDRUS-ZFIVNO.
        EXIT.
    ENDCASE.

  ENDIF.

  IF ZTIDRUS-ZFCLSEQ IS INITIAL.
    SELECT MAX( ZFCLSEQ ) INTO ZTIDRUS-ZFCLSEQ
    FROM   ZTIDRUS
    WHERE  ZFIVNO  EQ  ZTIDRUS-ZFIVNO.
  ENDIF.

  SELECT SINGLE * FROM ZTIDRUS
  WHERE  ZFIVNO   EQ   ZTIDRUS-ZFIVNO
  AND    ZFCLSEQ  EQ   ZTIDRUS-ZFCLSEQ.
   *ZTIDRUS  =  ZTIDRUS.

  IF SY-SUBRC NE 0 .
    MESSAGE  E010(ZIM1) WITH ZTIDRUS-ZFIVNO.
    EXIT.
  ELSE.
    W_COUNT   =  1.
  ENDIF.

  IF SY-TCODE EQ 'ZIMCD2'.
    SELECT SINGLE * FROM ZTIV WHERE ZFIVNO EQ ZTIDRUS-ZFIVNO.
    IF ZTIV-ZFCUST EQ 'Y'. MESSAGE E477. EXIT.  ENDIF.
    IF ZTIV-ZFCUST EQ '3'. MESSAGE W754. ENDIF.
  ENDIF.

  " HS DATA, MATERIAL DATA GET.
  PERFORM  P2000_READ_HS_DATA.
  PERFORM  P2000_READ_MATERIAL_DATA.

ENDMODULE.                 " CUSTOMS_CHECK_SCR0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0100 INPUT.

  IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
    CASE SY-TCODE.
      WHEN 'ZIMCD2'.
        W_STATUS = C_REQ_U.
        SET SCREEN 0101.  LEAVE TO SCREEN 0101.
      WHEN 'ZIMCD3'.
        W_STATUS = C_REQ_D.
        SET SCREEN 0101.  LEAVE TO SCREEN 0101.
      WHEN 'ZIMCC1'.
        W_STATUS = C_REQ_C.
        SET SCREEN 1101.  LEAVE TO SCREEN 1101.
      WHEN 'ZIMCC2'.
        IF ZTIDSUS-ZFAUTO EQ 'X'.
           MESSAGE  W977 WITH 'Auto-Creation From FTZ'.
        ENDIF.
        W_STATUS = C_REQ_U.
        SET SCREEN 1101.  LEAVE TO SCREEN 1101.
      WHEN 'ZIMCC3'.
        W_STATUS = C_REQ_D.
        SET SCREEN 1101.  LEAVE TO SCREEN 1101.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0101 INPUT.

  CASE OK-CODE.
    WHEN 'OTDC'.
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'COST'.
      PERFORM  P2000_CALL_COST_SCREEN.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE' OR 'SVCO' OR 'IMPREQ'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'CRDC' OR 'CHDC' OR 'DISP' .
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'DELE' OR 'DELR' OR 'REVK' OR 'OPCL' OR 'EDIS' OR
         'SDSD' OR 'REAL'.
      PERFORM  P2000_SET_INDICATE.
    WHEN 'DSBL'.
      IF SY-TCODE = 'ZIMCD2'  OR  SY-TCODE  =  'ZIMCD3'.
        PERFORM  P2000_BL_DOC_DISPLAY   USING  ZTIDRUS-ZFBLNO.
      ELSEIF SY-TCODE = 'ZIMCC2'  OR  SY-TCODE  =  'ZIMCC3'OR
             SY-TCODE = 'ZIMCC1'.
        PERFORM  P2000_BL_DOC_DISPLAY   USING  ZTIDSUS-ZFBLNO.
      ENDIF.
    WHEN 'DSIV'.
      IF SY-TCODE = 'ZIMCD2'  OR  SY-TCODE  =  'ZIMCD3'.
        PERFORM  P2000_IV_DOC_DISPLAY   USING  ZTIDRUS-ZFIVNO.
      ELSEIF SY-TCODE = 'ZIMCC2'  OR  SY-TCODE  =  'ZIMCC3'OR
             SY-TCODE = 'ZIMCC1'.
        PERFORM  P2000_IV_DOC_DISPLAY   USING  ZTIDSUS-ZFIVNO.
      ENDIF.
    WHEN 'DSDR'.
      PERFORM  P2000_IDR_DOC_MODIFY      USING  ZTIDSUS-ZFBLNO
                                                ZTIDSUS-ZFCLSEQ.
    WHEN 'DDLC'.
      PERFORM  P2000_DDLC_PROCESS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFINRC_PROCESS_SCR0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZFINRC_PROCESS_SCR0101 INPUT.

  CHECK W_STATUS NE C_REQ_D.
  IF ZTIDRUS-ZFINRC IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDRUS' 'ZFINRC'.
  ENDIF.
  SELECT SINGLE * FROM ZTIMIMG02
         WHERE    ZFCOTM  EQ  ZTIDRUS-ZFINRC.

  IF SY-SUBRC EQ 0.
    MESSAGE E604(ZIM1) WITH ZTIDRUS-ZFINRC.
  ENDIF.

ENDMODULE.                 " ZFINRC_PROCESS_SCR0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZFCUT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_ZFCUT_SCR0102 INPUT.

  IF ZTIDRUS-ZFCTW NE SPACE.
*     SELECT  SINGLE *
*     FROM    ZTIMIMG10 AS I
*     ON      H~LIFNR      EQ    I~ZFVEN
*     WHERE   I~ZFCUT      EQ    ZTIDRUS-ZFCTW.

    IF SY-SUBRC NE 0. MESSAGE E768 WITH ZTIDRUS-ZFCTW. ENDIF.
  ENDIF.

ENDMODULE.                 " GET_ZFCUT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CUT_EXRT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CUT_EXRT_SCR0102 INPUT.

  CHECK : W_STATUS NE C_REQ_D.

  IF NOT ZTIDRUS-ZFEEDT IS INITIAL.
    IF ZTIDRUS-ZFKRW NE ZTIDRUS-ZFIVAMC.
      PERFORM  P2000_GET_EX_RATE_NODIALOG USING ZTIDRUS-ZFIVAMC
                                                ZTIDRUS-ZFKRW
                                                ZTIDRUS-ZFEEDT
                                       CHANGING ZTIDRUS-ZFEXRT
                                                ZTIDRUS-FFACT.
    ENDIF.
  ENDIF.

  CLEAR : ZTIDRUS-ZFIVAMK, ZTIDRUS-ZFOTFE, ZTIDRUS-ZFDUTY,
          ZTIDRUS-ZFTOFEE.
  LOOP AT IT_ZSIDRUSH.
    W_TABIX  =  SY-TABIX.
    CLEAR : IT_ZSIDRHS-ZFTBAK.

    SELECT SINGLE * FROM ZTIMIMG08
    WHERE  ZFCDTY   EQ   '006'
    AND    ZFCD     EQ   '001'.
    MOVE : ZTIMIMG08-COND_TYPE   TO  W_KSCHL,
           IT_ZSIDRUSH-STAWN     TO  W_STAWN.

    PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIDRUSH-ZFHSAM
                                            ZTIDRUS-ZFIVAMC
                                            BAPICURR-BAPICURR.

    IT_ZSIDRHS-ZFTBAK = BAPICURR-BAPICURR * ( ZTIDRUS-ZFEXRT /
                                              ZTIDRUS-FFACT ).
    PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDRUSH-ZFTBAK
                                            IT_ZSIDRUSH-ZFKRW.

    " Other Tax recompute.
    PERFORM TAX_RECOMPUTE_CUSTOMS_DECL.

    ADD : IT_ZSIDRUSH-ZFTBAK    TO  ZTIDRUS-ZFIVAMK,
          IT_ZSIDRUSH-ZFMPAMT   TO  ZTIDRUS-ZFOTFE,
          IT_ZSIDRUSH-ZFHMAMT   TO  ZTIDRUS-ZFOTFE,
          IT_ZSIDRUSH-ZFCUAMT   TO  ZTIDRUS-ZFDUTY,
          IT_ZSIDRUSH-ZFCUAMT   TO  ZTIDRUS-ZFTOFEE,
          IT_ZSIDRUSH-ZFMPAMT   TO  ZTIDRUS-ZFTOFEE,
          IT_ZSIDRUSH-ZFHMAMT   TO  ZTIDRUS-ZFTOFEE.

    MODIFY IT_ZSIDRUSH.
  ENDLOOP.

ENDMODULE.                 " GET_CUT_EXRT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  HS_GET_LINE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HS_GET_LINE_SCR0103 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0103_1-CURRENT_LINE + LINE - 1.
  W_CURRENT_LINE  =  LINE.

ENDMODULE.                 " HS_GET_LINE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  HSDE_GET_LINE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HSDE_GET_LINE_SCR0103 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE2 = TC_0103_2-CURRENT_LINE + LINE2 - 1.

ENDMODULE.                 " HSDE_GET_LINE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0103_1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0103_1 INPUT.

  READ TABLE IT_ZSIDRUSH  WITH KEY ZSIDRUSH(21)  BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
    MOVE-CORRESPONDING ZSIDRUSH  TO  IT_ZSIDRUSH.
    MODIFY IT_ZSIDRUSH INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_0103_1  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0103_2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0103_2 INPUT.

  READ TABLE IT_ZSIDRUSD_SEL  WITH KEY ZSIDRUSD(21)  BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIDRUSD_SEL-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIDRUSD_SEL-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIDRUSD_SEL INDEX W_TABIX.

  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_0103_2  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0103 INPUT.

  CLEAR : W_TOT_DUTY, W_TOT_FEE, W_TOT_TAX.
  LOOP  AT  IT_ZSIDRUSH.
    ADD : IT_ZSIDRUSH-ZFCUAMT  TO  W_TOT_DUTY,
          IT_ZSIDRUSH-ZFMPAMT  TO  W_TOT_FEE,
          IT_ZSIDRUSH-ZFHMAMT  TO  W_TOT_FEE,
          IT_ZSIDRUSH-ZFADDAMT TO  W_TOT_FEE,
          IT_ZSIDRUSH-ZFCVDAMT TO  W_TOT_FEE,
          IT_ZSIDRUSH-ZFCUAMT  TO  W_TOT_TAX,
          IT_ZSIDRUSH-ZFMPAMT  TO  W_TOT_TAX,
          IT_ZSIDRUSH-ZFHMAMT  TO  W_TOT_TAX,
          IT_ZSIDRUSH-ZFADDAMT TO  W_TOT_TAX,
          IT_ZSIDRUSH-ZFCVDAMT TO  W_TOT_TAX.
  ENDLOOP.

  MOVE : W_TOT_DUTY   TO  ZTIDRUS-ZFDUTY,
         W_TOT_FEE    TO  ZTIDRUS-ZFOTFE,
         W_TOT_TAX    TO  ZTIDRUS-ZFTOFEE.

ENDMODULE.                 " USER_COMMAND_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  CUSTOMS_CREATE_SCR1100  INPUT
*&---------------------------------------------------------------------*
MODULE CUSTOMS_CREATE_SCR1100 INPUT.

  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZTIDRUS-ZFIVNO  IS INITIAL.
    MESSAGE E866(ZIM1). EXIT.
  ENDIF.

* Clearance Req. doc. No Count
  SELECT COUNT( * ) INTO W_COUNT
  FROM   ZTIV
  WHERE  ZFIVNO     EQ   ZTIDRUS-ZFIVNO.

  IF W_COUNT EQ 0.
     MESSAGE E010(ZIM1) WITH ZTIDRUS-ZFIVNO.
     EXIT.
  ENDIF.

  ">>Clerance serial No. No input!
  IF ZTIDRUS-ZFCLSEQ IS INITIAL.
     SELECT MAX( ZFCLSEQ ) INTO ZTIDRUS-ZFCLSEQ
     FROM   ZTIDRUS
     WHERE  ZFIVNO         EQ   ZTIDRUS-ZFIVNO.
  ENDIF.

*>> CHECK
  SELECT SINGLE * FROM ZTIDRUS
  WHERE  ZFIVNO   EQ   ZTIDRUS-ZFIVNO
  AND    ZFCLSEQ  EQ   ZTIDRUS-ZFCLSEQ.
  IF SY-SUBRC NE 0.
    MESSAGE E753.
    EXIT.
  ENDIF.

  CLEAR : ZTIDSUS, ZTIV.
  SELECT SINGLE * FROM ZTIDSUS
  WHERE  ZFIVNO   EQ   ZTIDRUS-ZFIVNO
  AND    ZFCLSEQ  EQ   ZTIDRUS-ZFCLSEQ.
  IF SY-SUBRC EQ 0.
     MESSAGE E779 WITH ZTIDSUS-ZFENTNO.
     EXIT.
  ENDIF.

  SELECT SINGLE * FROM ZTIV
  WHERE  ZFIVNO   EQ   ZTIDRUS-ZFIVNO.
  IF ZTIV-ZFCUST EQ 'Y'. MESSAGE E777. EXIT. ENDIF.
  IF ZTIV-ZFCUST NE '3'. MESSAGE E774. EXIT. ENDIF.

*> HEADER DATA MOVE.
  MOVE-CORRESPONDING ZTIDRUS TO ZTIDSUS.
  W_COUNT = 1.
  PERFORM READ_ZTIDRUSH_TO_IT.
  PERFORM READ_ZTIDRUSD_TO_IT.

ENDMODULE.                 " CUSTOMS_CREATE_SCR1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_FILED_CHECK_SCR1101  INPUT
*&---------------------------------------------------------------------*
MODULE INPUT_FILED_CHECK_SCR1101 INPUT.

  CHECK W_STATUS NE C_REQ_D.
  CHECK OK-CODE  NE 'EXIT'.

  IF ZTIDSUS-ZFENTNO IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDSUS' 'ZFENTNO'.
    EXIT.
  ELSE.
    " Entry Number Check.
    SELECT COUNT( * )   INTO  W_COUNT
    FROM   ZTIDSUS
    WHERE  ZFENTNO       EQ    ZTIDSUS-ZFENTNO
    AND    NOT ( ZFIVNO  EQ    ZTIDSUS-ZFIVNO
    AND          ZFCLSEQ EQ    ZTIDSUS-ZFCLSEQ ).
    IF W_COUNT GT 0.
      MESSAGE E779 WITH ZTIDSUS-ZFENTNO.
      EXIT.
    ENDIF.

    MOVE  ZTIDSUS-ZFENTNO(3)  TO  ZTIDSUS-ZFCTW.
    SELECT SINGLE * FROM ZTIMIMG10
    WHERE  ZFCUT    EQ   ZTIDSUS-ZFCTW.
    IF SY-SUBRC NE 0.
       MESSAGE  E977 WITH 'Invalid Entry No!'.
       EXIT.
    ENDIF.
  ENDIF.

  IF ZTIDSUS-ZFEDT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDSUS' 'ZFEDT'.
     EXIT.
  ELSE.

     SELECT *
     FROM   ZTIVIT UP TO 1 ROWS
     WHERE  ZFIVNO    EQ ZTIDSUS-ZFIVNO.
     ENDSELECT.

     SELECT SINGLE *  FROM  T001W
     WHERE  WERKS     EQ    ZTIVIT-WERKS.

     CALL FUNCTION 'ZIM_GET_NEXT_DATE'
          EXPORTING
             FACTORY_CALENDAR       =    T001W-FABKL
             NDATE                  =    ZTIDSUS-ZFEDT
             NDAY                   =    10
          IMPORTING
             RDATE                  =    ZTIDSUS-ZFSUMDT.
  ENDIF.

ENDMODULE.                 " INPUT_FILED_CHECK_SCR1101  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CUT_EXRT_SCR1102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CUT_EXRT_SCR1102 INPUT.

  CHECK : W_STATUS NE C_REQ_D.

  IF NOT ZTIDSUS-ZFEDT IS INITIAL AND ZTIDSUS-ZFEXRT IS INITIAL.
    IF ZTIDSUS-ZFKRW NE ZTIDSUS-ZFIVAMC.
      PERFORM  P2000_GET_EX_RATE_NODIALOG USING ZTIDSUS-ZFIVAMC
                                                ZTIDSUS-ZFKRW
                                                ZTIDSUS-ZFEDT
                                       CHANGING ZTIDSUS-ZFEXRT
                                                ZTIDSUS-FFACT.
    ENDIF.
  ENDIF.

*  CLEAR : ZTIDSUS-ZFIVAMK, ZTIDSUS-ZFDUTY, ZTIDSUS-ZFOTFE,
*          ZTIDSUS-ZFTOFEE.
*  LOOP AT IT_ZSIDSUSH.
*    W_TABIX  =  SY-TABIX.
*    CLEAR : IT_ZSIDSHS-ZFTBAK.
*
*    IF ZTIDSUS-ZFKRW NE ZTIDSUS-ZFIVAMC.
*       PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIDSUSH-ZFHSAM
*                                               ZTIDSUS-ZFIVAMC
*                                               BAPICURR-BAPICURR.
*
*       IF IT_ZSIDSUSH-ZFTBAK IS INITIAL.
*          IT_ZSIDSHS-ZFTBAK = BAPICURR-BAPICURR * ( ZTIDSUS-ZFEXRT /
*                                                    ZTIDSUS-FFACT ).
*       ENDIF.
*
*       PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSUSH-ZFTBAK
*                                               IT_ZSIDSUSH-ZFKRW.
*    ELSE.
*       IT_ZSIDSUSH-ZFTBAK  =  IT_ZSIDSUSH-ZFHSAM.
*    ENDIF.
*
*    " Other Tax recompute.
*    PERFORM TAX_RECALC_CUSTOMS_CLEARANCE.
*
*    ADD : IT_ZSIDSUSH-ZFTBAK    TO  ZTIDSUS-ZFIVAMK,
*          IT_ZSIDSUSH-ZFMPAMT   TO  ZTIDSUS-ZFOTFE,
*          IT_ZSIDSUSH-ZFHMAMT   TO  ZTIDSUS-ZFOTFE,
*          IT_ZSIDSUSH-ZFCUAMT   TO  ZTIDSUS-ZFDUTY,
*          IT_ZSIDSUSH-ZFCUAMT   TO  ZTIDSUS-ZFTOFEE,
*          IT_ZSIDSUSH-ZFMPAMT   TO  ZTIDSUS-ZFTOFEE,
*          IT_ZSIDSUSH-ZFHMAMT   TO  ZTIDSUS-ZFTOFEE.
*
*    MODIFY IT_ZSIDSUSH INDEX  W_TABIX.
*  ENDLOOP.

ENDMODULE.                 " GET_CUT_EXRT_SCR1102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_SUM_FEE_SCR1102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SUM_FEE_SCR1102 INPUT.

  CHECK : W_STATUS NE C_REQ_D.
  ZTIDSUS-ZFOTFE   =  ZTIDSUS-ZFHMAMT +  ZTIDSUS-ZFMPAMT.
  ZTIDSUS-ZFTOFEE  =  ZTIDSUS-ZFDUTY  +  ZTIDSUS-ZFOTFE.

ENDMODULE.                 " GET_SUM_FEE_SCR1102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_PORT_CODE_SCR1102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_PORT_CODE_SCR1102 INPUT.

  " Arriving Port Code(Customs) Get.
  SELECT SINGLE * FROM ZTIEPORT
  WHERE  LAND1    EQ   'US'
  AND    PORT     EQ   ZTIDSUS-ZFENPT.

  IF ZTIDSUS-ZFENPCD IS INITIAL.
     MOVE : ZTIEPORT-ZFREFCD    TO  ZTIDSUS-ZFENPCD,
            ZTIEPORT-ZFREFCD(2) TO  ZTIDSUS-ZFINRC.
  ENDIF.
  IF ZTIDSUS-ZFENPCD NE ZTIEPORT-ZFREFCD.
     UPDATE  ZTIEPORT
     SET     ZFREFCD   =  ZTIDSUS-ZFENPCD
     WHERE   LAND1     =  'US'
     AND     PORT      =  ZTIDSUS-ZFENPT.
  ENDIF.

  " Unlading Port Code(Customs) Get.
  CLEAR : ZTIEPORT.
  SELECT SINGLE * FROM ZTIEPORT
  WHERE  LAND1    EQ   'US'
  AND    PORT     EQ   ZTIDSUS-ZFAPRTC.

  IF ZTIDSUS-ZFAPTCD IS INITIAL.
    MOVE ZTIEPORT-ZFREFCD  TO  ZTIDSUS-ZFAPTCD.
  ENDIF.
  IF ZTIDSUS-ZFAPTCD NE ZTIEPORT-ZFREFCD.
    UPDATE  ZTIEPORT
    SET     ZFREFCD   =  ZTIDSUS-ZFAPTCD
    WHERE   LAND1     =  'US'
    AND     PORT      =  ZTIDSUS-ZFAPRTC.
  ENDIF.

  " lading Port Code(Customs) Get.
  CLEAR : ZTIEPORT.
  SELECT SINGLE * FROM ZTIEPORT
  WHERE  LAND1    EQ   ZTIDSUS-ZFCAC
  AND    PORT     EQ   ZTIDSUS-ZFSPRTC.

  IF ZTIDSUS-ZFSPUS IS INITIAL.
     MOVE ZTIEPORT-ZFREFCD  TO  ZTIDSUS-ZFSPUS.
  ENDIF.

  IF ZTIDSUS-ZFSPUS NE ZTIEPORT-ZFREFCD.
     UPDATE  ZTIEPORT
     SET     ZFREFCD   =  ZTIDSUS-ZFSPUS
     WHERE   LAND1     =  ZTIDSUS-ZFCAC
     AND     PORT      =  ZTIDSUS-ZFAPRTC.
  ENDIF.

ENDMODULE.                 " GET_PORT_CODE_SCR1102  INPUT
*&---------------------------------------------------------------------*
*&      Module  HS_GET_LINE_SCR1103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HS_GET_LINE_SCR1103 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_1103_1-CURRENT_LINE + LINE - 1.
  W_CURRENT_LINE  =  LINE.

ENDMODULE.                 " HS_GET_LINE_SCR1103  INPUT
*&---------------------------------------------------------------------*
*&      Module  HSDE_GET_LINE_SCR1103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HSDE_GET_LINE_SCR1103 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE2 = TC_1103_2-CURRENT_LINE + LINE2 - 1.

ENDMODULE.                 " HSDE_GET_LINE_SCR1103  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_1103_1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_1103_1 INPUT.

  READ TABLE IT_ZSIDSUSH  WITH KEY ZSIDSUSH(21)  BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
    MOVE-CORRESPONDING ZSIDSUSH  TO  IT_ZSIDSUSH.
    MODIFY IT_ZSIDSUSH INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_1103_1  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_1103_2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_1103_2 INPUT.

  READ TABLE IT_ZSIDSUSD_SEL  WITH KEY ZSIDSUSD(21)  BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIDSUSD_SEL-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIDSUSD_SEL-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIDSUSD_SEL INDEX W_TABIX.

  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_1103_2  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR1103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR1103 INPUT.

  CHECK W_STATUS NE C_REQ_D.

  CLEAR : ZTIV, ZTIMIMG08.
  SELECT SINGLE * FROM ZTIV WHERE ZFIVNO EQ ZTIDSUS-ZFIVNO.

  SELECT SINGLE * FROM ZTIMIMG08
  WHERE  ZFCDTY   EQ   '006'
  AND    ZFCD     EQ   '004'.

  IF ZTIDSUS-ZFAUTO NE 'X'.
     CLEAR : W_TOT_DUTY, W_TOT_FEE, W_TOT_TAX, W_TOT_MP, W_TOT_HM.
     LOOP  AT  IT_ZSIDSUSH.

        IF ZTIV-ZFVIA NE 'VSL'.
           CLEAR : IT_ZSIDSUSH-ZFHMRT, IT_ZSIDSUSH-ZFHMAMT.
        ENDIF.

       ADD : IT_ZSIDSUSH-ZFCUAMT  TO  W_TOT_DUTY,
             IT_ZSIDSUSH-ZFADDAMT TO  W_TOT_TAX,
             IT_ZSIDSUSH-ZFCVDAMT TO  W_TOT_TAX,
             IT_ZSIDSUSH-ZFHMAMT  TO  W_TOT_TAX,
             IT_ZSIDSUSH-ZFMPAMT  TO  W_TOT_MP,
             IT_ZSIDSUSH-ZFHMAMT  TO  W_TOT_HM.
     ENDLOOP.

     IF NOT ZTIMIMG08-ZFMXAMT IS INITIAL.
        IF W_TOT_MP  GE  ZTIMIMG08-ZFMXAMT.
           W_TOT_MP  =   ZTIMIMG08-ZFMXAMT.
        ENDIF.
     ENDIF.

     IF NOT ZTIMIMG08-ZFMNAMT IS INITIAL.
        IF W_TOT_MP  LE  ZTIMIMG08-ZFMNAMT.
           W_TOT_MP  =  ZTIMIMG08-ZFMNAMT.
        ENDIF.
     ENDIF.

     IF ZTIDSUS-ZFDUTY IS INITIAL.
        MOVE W_TOT_DUTY  TO  ZTIDSUS-ZFDUTY.
     ENDIF.
     IF ZTIDSUS-ZFMPAMT IS INITIAL.
        MOVE W_TOT_MP    TO  ZTIDSUS-ZFMPAMT.
     ENDIF.
     IF ZTIDSUS-ZFHMAMT IS INITIAL.
        MOVE W_TOT_HM    TO  ZTIDSUS-ZFHMAMT.
     ENDIF.
     IF ZTIDSUS-ZFOTFE  IS INITIAL.
        ZTIDSUS-ZFOTFE = W_TOT_MP + W_TOT_TAX.
     ENDIF.
     IF ZTIDSUS-ZFTOFEE IS INITIAL.
        ZTIDSUS-ZFTOFEE = ZTIDSUS-ZFDUTY + ZTIDSUS-ZFOTFE.
     ENDIF.
   ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR1103  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      ANTWORT = 'Y'.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  CUSTOMS_CLEARANCE_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CUSTOMS_CLEARANCE_GET INPUT.

  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZTIDSUS-ZFIVNO IS INITIAL  AND ZTIDSUS-ZFENTNO IS INITIAL.
     MESSAGE E012(ZIM1).
     EXIT.
  ENDIF.

  IF ( ZTIDSUS-ZFIVNO IS INITIAL ) AND ( ZTIDSUS-ZFENTNO NE SPACE ).
    SELECT SINGLE * FROM ZTIDSUS
    WHERE  ZFENTNO  EQ   ZTIDSUS-ZFENTNO.
  ENDIF.

  IF ZTIDSUS-ZFCLSEQ IS INITIAL.
    SELECT MAX( ZFCLSEQ ) INTO ZTIDSUS-ZFCLSEQ
    FROM   ZTIDSUS
    WHERE  ZFIVNO        EQ   ZTIDSUS-ZFIVNO.
  ENDIF.

  IF ZTIDSUS-ZFCLSEQ IS INITIAL.
     ZTIDSUS-ZFCLSEQ = 1.
  ENDIF.

  SELECT SINGLE * FROM ZTIDSUS
  WHERE  ZFIVNO   EQ   ZTIDSUS-ZFIVNO
  AND    ZFCLSEQ  EQ   ZTIDSUS-ZFCLSEQ.
  IF SY-SUBRC NE 0.  MESSAGE  E782.  EXIT.  ENDIF.
   *ZTIDSUS  =  ZTIDSUS.

  PERFORM READ_HS_DATA_TO_IT.
  PERFORM READ_HS_DETAIL_DATA_TO_IT.

  IF SY-TCODE EQ 'ZIMCC2'.
    PERFORM  P3000_SET_CLEARANCE_LOCK    USING   'L'.
  ENDIF.

  ">> Customs Clearance Expense Get.
  SELECT SINGLE * FROM ZTIV
          WHERE ZFIVNO EQ ZTIDSUS-ZFIVNO.

  IF SY-SUBRC EQ 0.
    W_ZFIMDNO = ZTIV-ZFIVNO.
    CALL FUNCTION 'ZIM_GET_COST_DOCUMENT'
         EXPORTING
              ZFCSTGRP    = '006'
              ZFIMDNO     = W_ZFIMDNO
         TABLES
              IT_ZSIMCOST = IT_ZSIMCOST.
  ENDIF.

ENDMODULE.                 " CUSTOMS_CLEARANCE_GET  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_COST_HISTORY_SCR0050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_COST_HISTORY_SCR0050 INPUT.

  LEAVE TO LIST-PROCESSING.

  G_REPID = SY-REPID.
  CLEAR G_VARIANT.
  G_VARIANT-REPORT = G_REPID.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = G_REPID
            I_STRUCTURE_NAME         = 'ZSIMCOST'
            I_CALLBACK_PF_STATUS_SET = G_STATUS
            I_CALLBACK_USER_COMMAND  = G_USER_COMMAND
            I_SAVE                   = G_SAVE
            IS_VARIANT               = G_VARIANT
       TABLES
            T_OUTTAB                 = IT_ZSIMCOST.

  LEAVE TO SCREEN W_DYNNR.

ENDMODULE.                 " SET_COST_HISTORY_SCR0050  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFCTW_PROCESS_SCR101  INPUT
*&---------------------------------------------------------------------*
MODULE ZFCTW_PROCESS_SCR101 INPUT.

  CHECK W_STATUS NE C_REQ_D.
  IF ZTIDRUS-ZFCTW IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDRUS' 'ZFCTW'.
    EXIT.
  ENDIF.
  SELECT SINGLE * FROM ZTIMIMG10
         WHERE    ZFCUT  EQ  ZTIDRUS-ZFCTW.

  IF SY-SUBRC NE 0.
    MESSAGE E206(ZIM1) WITH ZTIDRUS-ZFCTW ZTIDRUS-BUKRS.
    EXIT.
  ENDIF.

ENDMODULE.                 " ZFCTW_PROCESS_SCR101  INPUT
*&---------------------------------------------------------------------*
*&      Module  ENTRYDATE_PROCESS_SCR101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ENTRYDATE_PROCESS_SCR101 INPUT.

  CHECK W_STATUS NE C_REQ_D.
  IF ZTIDRUS-ZFEEDT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDRUS' 'ZFEEDT'.
    EXIT.
  ENDIF.

ENDMODULE.                 " ENTRYDATE_PROCESS_SCR101  INPUT
*&---------------------------------------------------------------------*
*&      Module  AMOUNT_CHECK_SCR0103  INPUT
*&---------------------------------------------------------------------*
MODULE AMOUNT_CHECK_SCR0103 INPUT.

  " INPUT VALUE CHECK.
  IF NOT ZSIDRUSH-ZFADDRT IS INITIAL.
    IF ZSIDRUSH-ZFADDNO IS INITIAL.
      MESSAGE  E977 WITH 'Input Anti-dumping case no!'.
      EXIT.
    ENDIF.
  ENDIF.

  IF NOT ZSIDRUSH-ZFCVRT IS INITIAL.
    IF ZSIDRUSH-ZFCVDNO IS INITIAL.
      MESSAGE  E977 WITH 'Input Countervailing case no!'.
      EXIT.
    ENDIF.
  ENDIF.

  IF ZTBL-ZFVIA NE 'VSL'.
    IF ZSIDRUSH-ZFHMAMT GT 0 OR ZSIDRUSH-ZFHMRT GT 0.
      MESSAGE  E977 WITH 'No Input Harbor Maintenanc Fee!'.
      EXIT.
    ENDIF.
  ENDIF.

  " Other Fee Compute.
  ZSIDRUSH-ZFMPAMT  =  ZSIDRUSH-ZFTBAK * ( ZSIDRUSH-ZFMPRT  / 100 ).
  ZSIDRUSH-ZFHMAMT  =  ZSIDRUSH-ZFTBAK * ( ZSIDRUSH-ZFHMRT  / 100 ).
  ZSIDRUSH-ZFCUAMT  =  ZSIDRUSH-ZFTBAK * ( ZSIDRUSH-ZFCURT  / 100 ).
  ZSIDRUSH-ZFADDAMT =  ZSIDRUSH-ZFTBAK * ( ZSIDRUSH-ZFADDRT / 100 ).
  ZSIDRUSH-ZFCVDAMT =  ZSIDRUSH-ZFTBAK * ( ZSIDRUSH-ZFCVRT  / 100 ).

ENDMODULE.                 " AMOUNT_CHECK_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  AMOUNT_CHECK_SCR1103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE AMOUNT_CHECK_SCR1103 INPUT.

  IF W_STATUS EQ 'D'.  EXIT. ENDIF.

  " INPUT VALUE CHECK.
  IF NOT ZSIDSUSH-ZFADDRT IS INITIAL.
    IF ZSIDSUSH-ZFADDNO IS INITIAL.
      MESSAGE  E977 WITH 'Input Anti-dumping case no!'.
      EXIT.
    ENDIF.
  ENDIF.

  IF NOT ZSIDSUSH-ZFCVRT IS INITIAL.
    IF ZSIDSUSH-ZFCVDNO IS INITIAL.
      MESSAGE  E977 WITH 'Input Countervailing case no!'.
      EXIT.
    ENDIF.
  ENDIF.

  IF ZTIV-ZFVIA NE 'VSL'.
    IF ZSIDSUSH-ZFHMAMT GT 0 OR ZSIDSUSH-ZFHMRT GT 0.
      MESSAGE  E977 WITH 'No Input Harbor Maintenanc Fee!'.
      EXIT.
    ENDIF.
  ENDIF.

  " Other Fee Compute.
  ZSIDSUSH-ZFMPAMT  =  ZSIDSUSH-ZFHSAM * ( ZSIDSUSH-ZFMPRT  / 100 ).
  ZSIDSUSH-ZFHMAMT  =  ZSIDSUSH-ZFHSAM * ( ZSIDSUSH-ZFHMRT  / 100 ).
  ZSIDSUSH-ZFCUAMT  =  ZSIDSUSH-ZFHSAM * ( ZSIDSUSH-ZFCURT  / 100 ).
  ZSIDSUSH-ZFADDAMT =  ZSIDSUSH-ZFHSAM * ( ZSIDSUSH-ZFADDRT / 100 ).
  ZSIDSUSH-ZFCVDAMT =  ZSIDSUSH-ZFHSAM * ( ZSIDSUSH-ZFCVRT  / 100 ).

ENDMODULE.                 " AMOUNT_CHECK_SCR1103  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFINRC_PROCESS_SCR1101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZFINRC_PROCESS_SCR1101 INPUT.

  CHECK W_STATUS NE C_REQ_D.
  IF ZTIDSUS-ZFINRC IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDSUS' 'ZFINRC'.
    EXIT.
  ENDIF.
  SELECT SINGLE * FROM ZTIMIMG02
         WHERE    ZFCOTM  EQ  ZTIDSUS-ZFINRC.

  IF SY-SUBRC NE 0.
    MESSAGE E205(ZIM1) WITH ZTIDSUS-ZFINRC ZTIDSUS-BUKRS.
    EXIT.
  ENDIF.

ENDMODULE.                 " ZFINRC_PROCESS_SCR1101  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
MODULE D0014_LIST_CHECK_SCR0014 INPUT.
* list-processing
  LEAVE TO LIST-PROCESSING.
* list Write
  PERFORM P2000_DATA_LISTING.

ENDMODULE.                 " D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR2100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR2100 INPUT.

  IMPORT  IT_DOHD  FROM MEMORY ID 'DO_HD'.
  IMPORT  IT_DOIT  FROM MEMORY ID 'DO_IT'.
  FREE MEMORY ID 'DO_HD'.
  FREE MEMORY ID 'DO_IT'.

  CLEAR : ZTBL, ZTIV, ZTTRHD, ZTBKPF.

*>> HEADER.
  READ TABLE IT_DOHD INDEX  1.

  SELECT SINGLE * FROM ZTBL
  WHERE  ZFHBLNO  EQ   IT_DOHD-ZFHBLNO.

  IF SY-SUBRC EQ 0.
     MOVE : ZTBL-BUKRS       TO  ZTTRHD-BUKRS,        " Company Code
            ZTBL-ZFWERKS     TO  ZTTRHD-WERKS,        " Plant
            '1'              TO  ZTTRHD-ZFTRGB,       " Transportation
            '1'              TO  ZTTRHD-ZFDRMT,       " Driver Method
            IT_DOHD-ZFREBELN TO  ZTTRHD-ZFREBELN,     " Rep. P/O
            SY-UNAME         TO  ZTTRHD-ZFSENDER,     " Sender
            'N'              TO  ZTTRHD-ZFGIYN,       " Good Issue Y/N
            'N'              TO  ZTTRHD-ZFPOSYN.      " Posting Status

     " Default Trucker Get.
     SELECT SINGLE * FROM ZTIMIMG11
                  WHERE BUKRS = ZTBL-BUKRS.

     IF SY-SUBRC EQ 0 AND
        ZTIMIMG11-ZFVNCT EQ 'X'.
        MOVE : ZTIMIMG11-ZFBOSE TO ZTTRHD-ZFTRCO.
     ENDIF.
  ENDIF.

  " Field Text Get
  PERFORM P1000_GET_NAMES.

  " Internal Table Get.
  REFRESH : IT_DOIT_SEL.
  LOOP AT IT_DOIT WHERE TRAID    EQ  IT_DOHD-TRAID.
     MOVE-CORRESPONDING IT_DOIT  TO  IT_DOIT_SEL.
     APPEND  IT_DOIT_SEL.
  ENDLOOP.

  " Call Screen
  W_STATUS = C_REQ_C.
  CLEAR DYNPRO.
  SET SCREEN 2101.  LEAVE TO SCREEN 2101.

ENDMODULE.                 " USER_COMMAND_SCR2100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_FIELD_NAME_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE GET_FIELD_NAME_SCRCOM INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der

ENDMODULE.                 " GET_FIELD_NAME_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR2102  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR2102 INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF ZTTRHD-ZFGIDT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTRHD' 'ZFGIDT'.
  ENDIF.

  IF ZTTRHD-ZFDRDT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTRHD' 'ZFDRDT'.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR2102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_TRCO_FIELD_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_TRCO_FIELD_SCR0102 INPUT.

  IF W_STATUS EQ 'D'.  EXIT.  ENDIF.

  IF ZTTRHD-ZFTRGB NE '2' AND
     ZTTRHD-ZFTRCO IS INITIAL.
     MESSAGE E315(ZIM1) WITH 'Self'.
  ENDIF.

ENDMODULE.                 " CHECK_TRCO_FIELD_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATE_FIELD_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_DATE_FIELD_SCR0102 INPUT.

  IF ZTTRHD-ZFGIDT GT ZTTRHD-ZFDRDT.
     MESSAGE E314(ZIM1).
  ENDIF.

ENDMODULE.                 " CHECK_DATE_FIELD_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_TRCO_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_TRCO_NAME_SCR0102 INPUT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF NOT ZTTRHD-ZFTRCO IS INITIAL.
     PERFORM  P1000_GET_VENDOR   USING      ZTTRHD-ZFTRCO
                                 CHANGING   WT_ZFTRCO.
  ENDIF.

ENDMODULE.                 " GET_TRCO_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CONTAINER_GET_LINE_SCR2103  INPUT
*&---------------------------------------------------------------------*
MODULE CONTAINER_GET_LINE_SCR2103 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_2103_1-CURRENT_LINE + LINE - 1.
  W_CURRENT_LINE  =  LINE.

ENDMODULE.                 " CONTAINER_GET_LINE_SCR2103  INPUT
*&---------------------------------------------------------------------*
*&      Module  CASE_GET_LINE_SCR2103  INPUT
*&---------------------------------------------------------------------*
MODULE CASE_GET_LINE_SCR2103 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE2 = TC_2103_2-CURRENT_LINE + LINE2 - 1.

ENDMODULE.                 " CASE_GET_LINE_SCR2103  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_2103_1  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_2103_1 INPUT.

  READ TABLE IT_DOHD  WITH KEY TRAID = ZSDOHD-TRAID.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSDOHD  TO  IT_DOHD.

  IF NOT W_ROW_MARK IS INITIAL.
     MOVE  'X'  TO  IT_DOHD-ZFMARK.
  ELSE.
     CLEAR : IT_DOHD-ZFMARK.
  ENDIF.

  IF W_SY_SUBRC = 0.
     MODIFY IT_DOHD INDEX W_TABIX.
  ELSE.
     APPEND IT_DOHD.
  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_2103_1  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_2103_2  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_2103_2 INPUT.

  READ TABLE IT_DOIT_SEL  WITH KEY TRAID = ZSDOIT-TRAID
                                   KDMAT = ZSDOIT-KDMAT
                                   MATNR = ZSDOIT-MATNR.

  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
     IF NOT ( W_ROW_MARK1 IS INITIAL ).
       IT_DOIT_SEL-ZFMARK = 'X'.
     ELSE.
       CLEAR : IT_DOIT_SEL-ZFMARK.
     ENDIF.
     MODIFY IT_DOIT_SEL INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_2103_2  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR2103  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR2103 INPUT.

   CASE  SY-UCOMM.
      WHEN 'INRO'.
         PERFORM  P2000_REPORT_CALL.

      WHEN 'DERO'.
         DELETE IT_DOHD  WHERE ZFMARK NE SPACE.
         LOOP  AT  IT_DOIT  WHERE  TRAID EQ IT_DOHD-TRAID.
            W_TABIX  =  SY-TABIX.
            DELETE  IT_DOIT  INDEX  W_TABIX.
            CONTINUE.
         ENDLOOP.
         REFRESH : IT_DOIT_SEL.

      WHEN 'DIPO'.
         CLEAR : W_COUNT.
         LOOP  AT  IT_DOIT_SEL WHERE ZFMARK = 'X'.
            W_COUNT = W_COUNT + 1.
         ENDLOOP.
         IF W_COUNT EQ 0.
            MESSAGE S962.   EXIT.
         ELSEIF W_COUNT GT 1.
            MESSAGE S965.   EXIT.
         ENDIF.

         " Call Transaction
         SET PARAMETER ID 'BSP' FIELD IT_DOIT_SEL-VGBEL.
         CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.
      WHEN 'DIIN'.
         CLEAR : W_COUNT.
         LOOP  AT  IT_DOIT_SEL WHERE ZFMARK = 'X'.
            W_COUNT = W_COUNT + 1.
         ENDLOOP.
         IF W_COUNT EQ 0.
            MESSAGE S962.   EXIT.
         ELSEIF W_COUNT GT 1.
            MESSAGE S965.   EXIT.
         ENDIF.
         " Call Transaction
         SET PARAMETER ID 'VL' FIELD IT_DOIT_SEL-VBELN.
         CALL TRANSACTION 'VL33N' AND SKIP  FIRST SCREEN.
   ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR2103  INPUT
*&---------------------------------------------------------------------*
*&      Module  CONTAINER_INFORMATION_GET  INPUT
*&---------------------------------------------------------------------*
MODULE CONTAINER_INFORMATION_GET INPUT.

   READ  TABLE IT_DOHD  WITH KEY  TRAID =  ZSDOHD-TRAID.
   IF SY-SUBRC EQ 0.  EXIT.  ENDIF.

ENDMODULE.                 " CONTAINER_INFORMATION_GET  INPUT
*&---------------------------------------------------------------------*
*&      Module  TR_DOC_DISPLAY_SCR2200  INPUT
*&---------------------------------------------------------------------*
MODULE TR_DOC_DISPLAY_SCR2200 INPUT.

  IF W_READ_CHK = 'Y'.
     IF ANTWORT NE 'Y'.
        EXIT.
     ENDIF.
  ENDIF.
  CLEAR DYNPRO.

  CASE SY-TCODE.
    WHEN 'ZIMT2'.
      W_STATUS = C_REQ_U.
      SET SCREEN 2101.

    WHEN 'ZIMT3'.
      W_STATUS = C_REQ_D.
      SET SCREEN 2101.
  ENDCASE.

*>>>> CHECK============================================
  IF NOT ( W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
      W_STATUS EQ C_OPEN_D ).
     IF ZTTRHD-ZFGIYN EQ 'X'.
        MESSAGE W332(ZIM1) WITH ZTTRHD-ZFTRNO 'Delivery Order Doc No'.
        LEAVE TO TRANSACTION 'ZIMT3' AND SKIP FIRST SCREEN.
     ENDIF.
  ENDIF.
* Lock Object Set
  IF NOT ( W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D ).
     IF SY-TCODE EQ 'ZIMT2'.
        PERFORM P2000_SET_TR_DOC_LOCK    USING    'L'.
     ENDIF.
  ENDIF.

  LEAVE SCREEN.

ENDMODULE.                 " TR_DOC_DISPLAY_SCR2200  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE READ_DOC_SCRCOM INPUT.

  PERFORM  P2000_OK_CODE_PROCESS.
  CLEAR : W_SRCH,  W_READ_CHK.

  IF ZTTRHD-ZFTRNO IS INITIAL AND  ZTBL-ZFHBLNO  IS INITIAL AND
     ZTBL-ZFBLNO   IS INITIAL.
     MESSAGE E300(ZIM1).
  ENDIF.

  "------------------------------------
  " Delivery Order Document No Input!
  "------------------------------------
  IF NOT ZTTRHD-ZFTRNO IS INITIAL.
     CLEAR IT_ZFTRNO. REFRESH IT_ZFTRNO.
     SELECT ZFTRNO INTO TABLE IT_ZFTRNO
              FROM ZTTRHD
             WHERE ZFTRNO = ZTTRHD-ZFTRNO.

     DESCRIBE TABLE IT_ZFTRNO LINES W_COUNT.

     IF W_COUNT EQ 0.
        MESSAGE E309(ZIM1) WITH ZTTRHD-ZFTRNO.
     ENDIF.
  "---------------------------------------------
  " B/L Number Inuput!
  "---------------------------------------------
  ELSEIF NOT ZTBL-ZFHBLNO IS INITIAL OR NOT ZTBL-ZFBLNO  IS INITIAL.

    IF ZTBL-ZFBLNO IS INITIAL.
       W_SRCH = 'HB'.
       DATA : WL_BLNO LIKE ZTBL-ZFBLNO.

       CLEAR IT_ZFTRNO. REFRESH IT_ZFTRNO.
       SELECT ZFBLNO INTO WL_BLNO FROM ZTBL
                WHERE ZFHBLNO = ZTBL-ZFHBLNO.

          SELECT DISTINCT ZFTRNO  APPENDING TABLE IT_ZFTRNO
          FROM   ZTTRIT
          WHERE  ZFBLNO = WL_BLNO.

          CLEAR WL_BLNO.
       ENDSELECT.

       DESCRIBE TABLE IT_ZFTRNO LINES W_COUNT.

       IF W_COUNT EQ 0.
          MESSAGE E311(ZIM1) WITH ZTBL-ZFHBLNO.
       ENDIF.
    ELSE.
       W_SRCH = 'BL'.
       CLEAR IT_ZFTRNO. REFRESH IT_ZFTRNO.
       SELECT DISTINCT ZFTRNO  INTO TABLE IT_ZFTRNO
       FROM   ZTTRIT
       WHERE  ZFBLNO = ZTBL-ZFBLNO.

       DESCRIBE TABLE IT_ZFTRNO LINES W_COUNT.

       IF W_COUNT EQ 0.
          MESSAGE E312(ZIM1) WITH ZTBL-ZFBLNO.
       ENDIF.
    ENDIF.
  ENDIF.
  CASE W_COUNT.
    WHEN 0.
    WHEN 1.
      READ TABLE IT_ZFTRNO INDEX 1.
      CLEAR IT_ZSTRHD. REFRESH IT_ZSTRHD.
      IT_ZSTRHD-ZFTRNO = IT_ZFTRNO-ZFTRNO.
      PERFORM P1000_TR_DOC_READ.
    WHEN OTHERS.
      W_READ_CHK = 'Y'.
      PERFORM P2000_DOC_ITEM_SELECT.
      IF ANTWORT NE 'Y'.
         EXIT.
      ENDIF.
      PERFORM P1000_TR_DOC_READ.
  ENDCASE.

ENDMODULE.                 " READ_DOC_SCRCOM  INPUT
