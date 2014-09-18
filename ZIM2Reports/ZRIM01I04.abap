*----------------------------------------------------------------------*
***INCLUDE ZRIM01I04 .
*----------------------------------------------------------------------*
*&  프로그램명 : 수입 B/L 관련 PAI MODULE Include                      *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.10.11                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0131  INPUT
*&---------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0131 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0131-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0131  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBLCST_UPDATE_SCR0131  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSBLCST_UPDATE_SCR0131 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

* Internal Table Read
  READ TABLE IT_ZSBLCST   INDEX TC_0131-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC EQ 0 AND ZSBLCST IS INITIAL.
    EXIT.
  ENDIF.

  IF ZSBLCST-ZFCSCD IS INITIAL.
    PERFORM P2000_NO_INPUT  USING  'ZSBLCST' 'ZFCSCD'
                            DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE E167 WITH 'Expense code'.
  ELSE.
    IF ZSBLCST-ZFCDNM IS INITIAL.
      SELECT SINGLE ZFCDNM ZFCD2
             INTO (ZSBLCST-ZFCDNM, ZSBLCST-ZFCD2)
             FROM ZTIMIMG08
             WHERE ZFCDTY EQ '004'
             AND   ZFCD   EQ ZSBLCST-ZFCSCD.
      IF ZSBLCST-ZFCSCD IS INITIAL.
        PERFORM P2000_NO_INPUT  USING  'ZSBLCST' 'ZFCSCD'
                                DFIES-SCRTEXT_M W_SUBRC.
        MESSAGE E433 WITH 'Overseas freight' ZSBLCST-ZFCSCD.
      ENDIF.
    ENDIF.
  ENDIF.

*>
  MOVE-CORRESPONDING ZSBLCST   TO IT_ZSBLCST.

*>> 자동계산시엔 기본으로 들어가는 데이타.
  MOVE : ZTBL-BUKRS            TO IT_ZSBLCST-BUKRS,
         SY-DATUM              TO IT_ZSBLCST-ZFOCDT.  "발생일.

  IF ZTIMIMG11-ZFVNCT IS INITIAL.   " Fowarder로 넣기.
    MOVE : ZTBL-ZFFORD           TO IT_ZSBLCST-ZFVEN,   "Vendor
           ZTBL-ZFFORD           TO IT_ZSBLCST-ZFPAY.   "지급처.
  ELSE.                             " 계약된 운송업체로 넣기.
    MOVE : ZTBL-ZFHAYEK          TO IT_ZSBLCST-ZFVEN,   "Vendor
           ZTBL-ZFHAYEK          TO IT_ZSBLCST-ZFPAY.   "지급처.
  ENDIF.

  IF IT_ZSBLCST-WAERS IS INITIAL.
    SELECT SINGLE * FROM ZTIMIMG08
                   WHERE ZFCDTY = '004'
                     AND ZFCD   = IT_ZSBLCST-ZFCSCD.
    IF ZTIMIMG08-ZFCURR EQ 'B'.       " 운임통화.
      MOVE : ZTBL-ZFTRCUR        TO    IT_ZSBLCST-WAERS.
    ELSEIF ZTIMIMG08-ZFCURR EQ 'K'.       " 원화.
      MOVE : 'KRW'               TO    IT_ZSBLCST-WAERS.
    ENDIF.
  ELSE.
*    IF IT_ZSBLCST-WAERS NE ZTBL-ZFTRCUR AND  " 운임통화나 원화만가능.
*       IT_ZSBLCST-WAERS NE 'KRW'.
*      MESSAGE E433(ZIM1) WITH ZTBL-ZFBLAMC 'KRW'.
*    ENDIF.
  ENDIF.

*>> 2001.04.11 나현주 추가. 원화인 경우 비용원화금액에 비용금액 SET.
  IT_ZSBLCST-KRW = W_KRW.

  IF IT_ZSBLCST-WAERS  EQ  'KRW'.
    MOVE  IT_ZSBLCST-ZFCAMT  TO  IT_ZSBLCST-ZFCKAMT.
    MOVE  1                  TO  IT_ZSBLCST-ZFEXRT.
  ELSE.
    MOVE  ZTBL-ZFEXRTT        TO IT_ZSBLCST-ZFEXRT. "운임환율적용.
*>>> 원화로 계산.
     PERFORM P3000_EXCHAGE_CHARGE.
  ENDIF.

*>>>> 지급처.
*  CLEAR LFA1.
*  SELECT SINGLE * FROM LFA1
*         WHERE LIFNR = IT_ZSBLCST-ZFVEN.
*  IF SY-SUBRC NE 0 AND NOT IT_ZSBLCST-ZFCAMT IS INITIAL.
*    MESSAGE E025.
*  ENDIF.
*  CLEAR IT_ZSBLCST-ZFPAY.
*  IF IT_ZSBLCST-ZFPAY IS INITIAL.
*    IF LFA1-LNRZA IS INITIAL.
*      MOVE IT_ZSBLCST-ZFVEN  TO IT_ZSBLCST-ZFPAY.   " 지불?
*    ELSE.
*      MOVE LFA1-LNRZA  TO IT_ZSBLCST-ZFPAY.   " 지불?
*    ENDIF.
*  ENDIF.

*>> Payment term.
  CLEAR : IT_ZSBLCST-ZTERM.
*  IF IT_ZSBLCST-ZTERM IS INITIAL.
  SELECT SINGLE ZTERM INTO IT_ZSBLCST-ZTERM   " Payment Term
         FROM LFB1
         WHERE LIFNR = IT_ZSBLCST-ZFVEN
         AND BUKRS   = IT_ZSBLCST-BUKRS.
*  ENDIF.
  IF NOT ( IT_ZSBLCST-ZFPAY IS INITIAL ).
    SELECT SINGLE *
      FROM LFA1
     WHERE LIFNR = IT_ZSBLCST-ZFPAY.
    IF SY-SUBRC NE 0 AND NOT IT_ZSBLCST-ZFCAMT IS INITIAL.
      MESSAGE E341.
    ENDIF.
  ENDIF.

*  IF IT_ZSBLCST-ZFOCDT IS INITIAL.   " 발생일.
*    IT_ZSBLCST-ZFOCDT    =    ZTBL-ZFETA.
*  ENDIF.

*> TAX CODE 비용전기시 받아서 쓸것임.
*  IF IT_ZSBLCST-MWSKZ IS INITIAL.    " TAX CODE
*    IF ZTBL-ZFVIA EQ 'AIR'.
*      WZ_VIA  =  'A'.
*    ELSEIF ZTBL-ZFVIA EQ 'VSL'.
*      WZ_VIA  =  'O'.
*    ENDIF.
*    SELECT SINGLE ZFCD5 INTO IT_ZSBLCST-MWSKZ FROM ZTIMIMG08
*     WHERE ZFCDTY EQ '004'
**        AND ( ZFCD4  EQ WZ_VIA OR
**              ZFCD4  EQ 'B' )
*       AND ZFCD   EQ IT_ZSBLCST-ZFCSCD.
*    IF IT_ZSBLCST-MWSKZ IS INITIAL.    " TAX CODE
*      MESSAGE W167 WITH 'Tax Code'.
*    ELSE.
*      SELECT SINGLE * FROM T007A
*             WHERE KALSM EQ 'TAXKR'
*             AND   MWSKZ EQ  IT_ZSBLCST-MWSKZ.
*      IF SY-SUBRC NE 0.
*        MESSAGE E495 WITH 'TAXKR' IT_ZSBLCST-MWSKZ.
*      ENDIF.
*    ENDIF.
*  ENDIF.

* ===> TAX RATE
  IF IT_ZSBLCST-MWSKZ IS INITIAL.
    CLEAR : IT_ZSBLCST-KBETR, IT_ZSBLCST-KONWA, IT_ZSBLCST-ZFVAT.
  ELSE.
    PERFORM   P1000_READ_KONP   USING   IT_ZSBLCST-MWSKZ
                                        IT_ZSBLCST-KBETR
                                        IT_ZSBLCST-KONWA.
  ENDIF.

*> 대표플랜트.
  IF IT_ZSBLCST-ZFWERKS IS INITIAL.    " 대표 PLANT
    IT_ZSBLCST-ZFWERKS  =    ZTBL-ZFWERKS.
  ENDIF.

*-----------------------------------------------------------------------
* TAX CODE  ===> 부가세 원단위 절?
* DESC : 유재오 과장 DEFINE (E&Y)
*  IF IT_ZSBLCST-ZFVAT IS INITIAL.
*    IF NOT IT_ZSBLCST-KBETR IS INITIAL.
*      W_AMOUNT = W_AMOUNT * IT_ZSBLCST-KBETR / 10.
*      COMPUTE W_AMOUNT = TRUNC( W_AMOUNT ).
*      W_AMOUNT = W_AMOUNT * 10.
*      IT_ZSBLCST-ZFVAT = W_AMOUNT.
*      PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSBLCST-ZFVAT
*                                                'KRW'.
*    ENDIF.
*  ENDIF.
*-----------------------------------------------------------------------
*>> 부대비 정산 이후 비용 수정시 CHECK!
  CLEAR W_TOT_AMT.
  IF W_SY_SUBRC EQ 0 AND IT_ZSBLCST-ZFCSTYN = 'X'.
    W_TOT_AMT = IT_ZSBLCST-ZFUPCST.
    IF W_TOT_AMT > 0.
      IF W_TOT_AMT > IT_ZSBLCST-ZFCKAMT.
        MESSAGE E471 WITH IT_ZSBLCST-ZFCSQ.
      ELSEIF W_TOT_AMT < IT_ZSBLCST-ZFCKAMT.
        MESSAGE W472 WITH IT_ZSBLCST-ZFCSQ.
      ENDIF.
    ENDIF.
  ENDIF.
  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSBLCST  INDEX W_TABIX.
  ELSE.
*     IT_ZSBLCST-ZFCSQ   = ( TC_0105-CURRENT_LINE * 10 ) + 10000.
    APPEND IT_ZSBLCST.
  ENDIF.

ENDMODULE.                 " IT_ZSBLCST_UPDATE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZTERM_NAME_SCR0131  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ZTERM_NAME_SCR0131 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
*>>> 금액을 입력하지 않았을 경우...
  CHECK  NOT ZSBLCST-ZFCKAMT IS INITIAL.

  IF ZSBLCST-ZTERM IS INITIAL.
    EXIT.
  ENDIF.
  REFRESH : ZBTXT_LINES.

  CALL FUNCTION 'FI_PRINT_ZTERM'                            "80864
       EXPORTING
            I_ZTERM         = ZSBLCST-ZTERM
            I_LANGU         = SY-LANGU
            I_XT052U        = 'X'                           "187501
            I_T052          = T052
       TABLES
            T_ZTEXT         = ZBTXT_LINES
       EXCEPTIONS
            ZTERM_NOT_FOUND = 01.

  IF SY-SUBRC NE 0.
    MESSAGE E410(FH) WITH SY-MSGV1 RAISING ZTERM_NOT_FOUND.
  ENDIF.
ENDMODULE.                 " GET_ZTERM_NAME_SCR0131  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0131_MARK_TC_0131  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0131_MARK_TC_0131 INPUT.

  READ TABLE IT_ZSBLCST  " WITH KEY ZSBLCST(18)  BINARY SEARCH.
                         INDEX TC_0131-CURRENT_LINE.
  IF SY-SUBRC = 0.
**>>> 금?
**      IF ZTBL-ZFEXRT IS INITIAL.
**         IT_ZSBLCST-ZFCKAMT = 0.
**      ELSE.
*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal )
*-----------------------------------------------------------------------
**        IT_ZSBLCST-ZFCKAMT = IT_ZSBLCST-ZFCAMT.
**         PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSBLCST-ZFCKAMT
**                                                    IT_ZSBLCST-KRW.
*
**         IF IT_ZSBLCST-WAERS NE 'KRW'.
**            IF IT_ZSBLCST-ZFCKAMT IS INITIAL.
**               IT_ZSBLCST-ZFCKAMT = ZTBL-ZFEXRT * IT_ZSBLCST-ZFCKAMT.
**            ENDIF.
**         ENDIF.
*      ENDIF.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSBLCST-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSBLCST-ZFMARK.
    ENDIF.
    MODIFY IT_ZSBLCST  INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0131_MARK_TC_0131  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_BL_TO_POINT_SUBSCR  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_BL_TO_POINT_SUBSCR INPUT.
  DATA : WL_DISPLAY,
         WL_CDTY LIKE ZTIMIMG08-ZFCDTY.

  REFRESH : IT_BLSDP_HELP.

  IF SY-DYNNR = '0132'.
    WL_CDTY = '017'.
  ELSEIF SY-DYNNR = '0133' OR SY-DYNNR = '0134'.
    WL_CDTY = '016'.
  ENDIF.

  SELECT *  INTO CORRESPONDING FIELDS OF TABLE IT_ZFCD_HELP
         FROM   ZTIMIMG08
         WHERE  ZFCDTY   EQ   WL_CDTY.

  IF SY-SUBRC NE 0.
    MESSAGE S406.
    EXIT.
  ENDIF.

  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.

  WINDOW_TITLE = 'Basic Rate'.
  CONCATENATE WINDOW_TITLE 'Code Help' INTO WINDOW_TITLE
              SEPARATED BY SPACE.

  IF W_STATUS EQ C_REQ_D.
    WL_DISPLAY = 'X'.
  ELSE.
    CLEAR: WL_DISPLAY.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
               RETFIELD        = 'ZFCD'
               DYNPPROG        = DYNPROG
               DYNPNR          = DYNNR
               DYNPROFIELD     = 'ZTBL-ZFRTCD'
               WINDOW_TITLE    = WINDOW_TITLE
               VALUE_ORG       = 'S'
               DISPLAY         = WL_DISPLAY
       TABLES
               VALUE_TAB       = IT_ZFCD_HELP
       EXCEPTIONS
               PARAMETER_ERROR = 1
               NO_VALUES_FOUND = 2
               OTHERS          = 3.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

ENDMODULE.                 " HELP_BL_TO_POINT_SUBSCR  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_FIELD_ZFRTCD  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_FIELD_ZFRTCD INPUT.

  IF ZTBL-ZFVIA = 'AIR'.
    SELECT SINGLE ZFCDNM ZFMRATE      " Basic Rate
                  INTO (WA_BLCALC-ZFRTCD , ZTBL-ZFMRATE)
                  FROM ZTIMIMG08
                 WHERE ZFCDTY = '017'
                   AND ZFCD   = ZTBL-ZFRTCD.

  ELSEIF ZTBL-ZFVIA = 'VSL'.
    SELECT SINGLE ZFCDNM ZFMRATE      " Basic Rate
                  INTO (WA_BLCALC-ZFRTCD , ZTBL-ZFMRATE)
                  FROM ZTIMIMG08
                 WHERE ZFCDTY = '016'
                   AND ZFCD   = ZTBL-ZFRTCD.
    WA_BLCALC-ZFRTCD1 = WA_BLCALC-ZFRTCD.
    WA_BLCALC-ZFRTCD2 = WA_BLCALC-ZFRTCD.
  ENDIF.

ENDMODULE.                 " CHECK_FIELD_ZFRTCD  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALCULATE_BASIC_FREIGHT  INPUT
*&---------------------------------------------------------------------*
MODULE CALCULATE_BASIC_FREIGHT INPUT.
*> Upgrade/Basic
  IF RB_DF = 'X'.
    CLEAR ZTBL-ZFDFUP.
  ELSEIF RB_UP = 'X'.
    ZTBL-ZFDFUP = 'X'.
  ENDIF.

*> Net Price
  IF ZTBL-ZFCHARGE = 'X'. " Conference Net Price
    ZTBL-ZFNETPR1 = ZTBL-ZFFRE.
    ZTBL-ZFNETPR2 = ZTBL-ZFFRE.
    ZTBL-ZFNETPR3 = ZTBL-ZFFRE.
    ZTBL-ZFNETPR4 = ZTBL-ZFFRE.
  ELSE.                  " IMG Net Price
     PERFORM P3000_GET_NETPRICE_PER_WEIGHT.
  ENDIF.

*> Freight Calcurate
  CHECK NOT ZTBL-ZFRTCD IS INITIAL.
   PERFORM P3000_CALCULATE_FREIGHT.

ENDMODULE.                 " CALCULATE_BASIC_FREIGHT  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_CHECK_SUBSCR  INPUT
*&---------------------------------------------------------------------*
MODULE INPUT_CHECK_SUBSCR INPUT.
* Display MODE MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTBL-ZFRTCD IS INITIAL.
    MESSAGE E440(ZIM1).
  ELSE.
    IF ZTBL-ZFVIA = 'AIR'.
      SELECT SINGLE ZFCDNM ZFMRATE      " Basic Rate
                    INTO (WA_BLCALC-ZFRTCD , ZTBL-ZFMRATE)
                    FROM ZTIMIMG08
                   WHERE ZFCDTY = '017'
                     AND ZFCD   = ZTBL-ZFRTCD.

    ELSEIF ZTBL-ZFVIA = 'VSL'.
      SELECT SINGLE ZFCDNM ZFMRATE      " Basic Rate
                    INTO (WA_BLCALC-ZFRTCD , ZTBL-ZFMRATE)
                    FROM ZTIMIMG08
                   WHERE ZFCDTY = '016'
                     AND ZFCD   = ZTBL-ZFRTCD.
      WA_BLCALC-ZFRTCD1 = WA_BLCALC-ZFRTCD.
      WA_BLCALC-ZFRTCD2 = WA_BLCALC-ZFRTCD.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_BASIC_CHARGE  INPUT
*&---------------------------------------------------------------------*
MODULE SET_BASIC_CHARGE INPUT.

  IF ZTBL-ZFVIA = 'AIR'.
    READ TABLE IT_ZSBLCST WITH KEY ZFCSCD = 'ABC'.
  ELSEIF ZTBL-ZFVIA = 'VSL'.
    READ TABLE IT_ZSBLCST WITH KEY ZFCSCD = 'OBC'.
  ENDIF.

  IF IT_ZSBLCST-ZFCAMT IS INITIAL.
     PERFORM P3000_SET_BASIC_CHARGE.
  ENDIF.

ENDMODULE.                 " SET_BASIC_CHARGE  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0132  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0132 INPUT.

  CASE SY-UCOMM.
    WHEN 'CAL'.
       PERFORM P3000_SET_BASIC_CHARGE.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0132  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_TAX_CODE_TO_ITM  INPUT
*&---------------------------------------------------------------------*
MODULE SET_TAX_CODE_TO_ITM INPUT.
* Internal Table Read
  LOOP AT IT_ZSBSEG_TMP.
    W_TABIX    = SY-TABIX.

    MOVE : ZSBKPF-MWSKZ         TO   IT_ZSBSEG_TMP-MWSKZ. "한수원.

    MODIFY IT_ZSBSEG_TMP INDEX W_TABIX.
  ENDLOOP.

ENDMODULE.                 " SET_TAX_CODE_TO_ITM  INPUT
*&---------------------------------------------------------------------*
*&      Module  MATNR_DATA_GET_SCR6218  INPUT
*&---------------------------------------------------------------------*
MODULE MATNR_DATA_GET_SCR6218 INPUT.

  " 자재코드 입력시 자재명, 단위 GET!
  IF NOT ZSIDRHSD-ZFSTCD IS INITIAL.
    CLEAR: MTCOM, MT06E, *MT06E, MT06B.
    MTCOM-KENNG     = 'MT06E'.
    MTCOM-MATNR     = ZSIDRHSD-ZFSTCD.
    MTCOM-WERKS     = ZTIDR-ZFWERKS.
    MTCOM-SPRAS     = T001W-SPRAS.
    MTCOM-ALAND     = ZTIDR-ZFSCON.
    MTCOM-PSTAT     = 'EBD'.
    MTCOM-KZSPR     = 'X'.
    MTCOM-SPR_MEINS = 'X'.
    MTCOM-KZMPN     = 'X'.
    MTCOM-XVKBW     = T001K-XVKBW.
    IF TCURM IS INITIAL.
      SELECT SINGLE * FROM TCURM.
    ENDIF.
    PERFORM LESEN_MATERIAL_NEU(SAPFMMEX) USING MTCOM 'F'
                                               MT06E MTCOR.
    IF ZSIDRHSD-STAWN IS INITIAL.
      MOVE MT06E-STAWN       TO     ZSIDRHSD-STAWN.
    ENDIF.
    IF ZSIDRHSD-ZFGDDS1 IS INITIAL.
      MOVE : MT06E-MAKTX      TO     ZSIDRHSD-ZFGDDS1.
    ENDIF.
    MOVE MT06E-MEINS          TO     ZSIDRHSD-ZFQNTM.
    MOVE MT06E-PEINH          TO     ZSIDRHSD-PEINH.
  ENDIF.

ENDMODULE.                 " MATNR_DATA_GET_SCR6218  INPUT
*&---------------------------------------------------------------------*
*&      Module  STAWN_NAME_GET_SCR6217  INPUT
*&---------------------------------------------------------------------*
MODULE STAWN_NAME_GET_SCR6217 INPUT.

  IF NOT ZTIDRHS-STAWN IS INITIAL.
    SELECT SINGLE * FROM T604T
           WHERE  LAND1  EQ    'KR'
           AND    STAWN  EQ    ZTIDRHS-STAWN
           AND    SPRAS  EQ    SY-LANGU.

    MOVE : T604T-TEXT1   TO    ZTIDRHS-ZFGDNM.
  ENDIF.

ENDMODULE.                 " STAWN_NAME_GET_SCR6217  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSREQIT_UPDATE  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSREQIT_UPDATE INPUT.

ENDMODULE.                 " IT_ZSREQIT_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_UNIT_TEXT  INPUT
*&---------------------------------------------------------------------*
MODULE SET_UNIT_TEXT INPUT.

 W_TOWTM = ZTBLINR_TMP-ZFTOWTM.
 W_PKCNM = ZTBLINR_TMP-ZFPKCNM.

ENDMODULE.                 " SET_UNIT_TEXT  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LIFNR_NAME  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LIFNR_NAME INPUT.

*구매처명
  PERFORM  P1000_GET_VENDOR   USING  ZTBLINR_TMP-LIFNR
                           CHANGING  LFA1-NAME1.

ENDMODULE.                 " GET_LIFNR_NAME  INPUT
*&---------------------------------------------------------------------*
*&      Module  P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
MODULE P2000_INIT_VALUE_CHECK INPUT.

  IF NOT ( SY-UCOMM EQ 'ENTR' OR SY-UCOMM EQ 'YES' ).
    EXIT.
  ENDIF.

  IF UF05A-STGRD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'UF05A' 'STGRD'.
  ENDIF.

  IF BSIS-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'BSIS' 'BUDAT'.
  ENDIF.

ENDMODULE.                 " P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_OPEN_BANK_SCR3510  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_OPEN_BANK_SCR3510 INPUT.

  IF ZTCIVHD-ZFMAVN IS INITIAL.
     MESSAGE  E977 WITH 'Input Vendor Code!'.
     EXIT.
  ENDIF.

  REFRESH : IT_OPBN_HELP.
  SELECT SINGLE * FROM LFA1
  WHERE  LIFNR    EQ   ZTCIVHD-ZFMAVN.
  IF LFA1-LNRZA   IS   INITIAL.
     SELECT * FROM LFZA WHERE LIFNR EQ ZTCIVHD-ZFMAVN.
        CLEAR : LFA1.
        SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LFZA-EMPFK.
        MOVE : LFZA-EMPFK      TO   IT_OPBN_HELP-LIFNR,
               LFA1-NAME1      TO   IT_OPBN_HELP-NAME1,
               LFA1-ORT01      TO   IT_OPBN_HELP-ORT01.
        APPEND IT_OPBN_HELP.
     ENDSELECT.
  ELSE.
     SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ  LFA1-LNRZA.
     MOVE : LFA1-LIFNR      TO   IT_OPBN_HELP-LIFNR,
            LFA1-NAME1      TO   IT_OPBN_HELP-NAME1,
            LFA1-ORT01      TO   IT_OPBN_HELP-ORT01.
     APPEND IT_OPBN_HELP.
  ENDIF.

  DESCRIBE  TABLE  IT_OPBN_HELP  LINES  W_LINE.
  IF W_LINE EQ 0.
    MESSAGE S406.  EXIT.
  ENDIF.

  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.

  WINDOW_TITLE = 'Alternative Payee'.
  CONCATENATE WINDOW_TITLE 'Help' INTO WINDOW_TITLE
              SEPARATED BY SPACE.

  IF W_STATUS EQ C_REQ_D.
     L_DISPLAY = 'X'.
  ELSE.
     CLEAR: L_DISPLAY.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
               RETFIELD        = 'LIFNR'
               DYNPPROG        = DYNPROG
               DYNPNR          = DYNNR
               DYNPROFIELD     = 'ZTCIVHD-ZFOPBN'
               WINDOW_TITLE    = WINDOW_TITLE
               VALUE_ORG       = 'S'
               DISPLAY         = L_DISPLAY
       TABLES
               VALUE_TAB       = IT_OPBN_HELP
       EXCEPTIONS
               PARAMETER_ERROR = 1
               NO_VALUES_FOUND = 2
               OTHERS          = 3.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

ENDMODULE.                 " HELP_OPEN_BANK_SCR3510  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0015  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0015 INPUT.

  CASE SY-UCOMM.
    WHEN 'DDLC'.
    WHEN 'DSID'.
      SET PARAMETER ID 'VL' FIELD IT_CONTLST-VBELN.
      CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0015  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSIVIT_UPDATE_SCR3111  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSIVIT_UPDATE_SCR3111 INPUT.

* Display Mode -> Module Exit.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
     EXIT.
  ENDIF.

* Internal Table Read
  READ TABLE IT_ZSIVIT   WITH KEY ZFIVDNO = ZSIVIT-ZFIVDNO.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSIVIT  TO IT_ZSIVIT.

  " B/L Document No Check
  IF ZSIVIT-ZFBLNO IS INITIAL.
     MESSAGE W039.    EXIT.
  ELSE.
    SELECT SINGLE * FROM ZTBL
                    WHERE ZFBLNO EQ IT_ZSIVIT-ZFBLNO.
    IF SY-SUBRC NE 0.
       CLEAR : ZSIVIT.
       MESSAGE W038 WITH IT_ZSIVIT-ZFBLNO.
       EXIT.
    ENDIF.

    " B/L Item No No input.
    IF NOT IT_ZSIVIT-ZFBLIT IS INITIAL.
*-----------------------------------------------------------------------
      READ TABLE  IT_ZSIVIT  WITH KEY ZFBLNO  = ZSIVIT-ZFBLNO
                                      ZFBLIT  = ZSIVIT-ZFBLIT.
      IF SY-SUBRC EQ 0 AND W_SY_SUBRC NE 0.
         CLEAR : ZSIVIT.
         MESSAGE S369 WITH IT_ZSIVIT-ZFBLNO IT_ZSIVIT-ZFBLIT
                           IT_ZSIVIT-ZFIVDNO.
         EXIT.
      ENDIF.
*-----------------------------------------------------------------------
      MOVE-CORRESPONDING ZSIVIT  TO IT_ZSIVIT.
      SELECT SINGLE * FROM ZTBLIT
      WHERE  ZFBLNO   EQ   IT_ZSIVIT-ZFBLNO
      AND    ZFBLIT   EQ   IT_ZSIVIT-ZFBLIT
      AND    BLOEKZ   NE   'X'.

      IF SY-SUBRC NE 0.
         CLEAR : ZSIVIT.
         MESSAGE W370 WITH IT_ZSIVIT-ZFBLNO IT_ZSIVIT-ZFBLIT.
         EXIT.
      ENDIF.

      MOVE-CORRESPONDING  ZTBLIT   TO    IT_ZSIVIT.
      MOVE : ZTBLIT-BLMENGE        TO    IT_ZSIVIT-MENGE_BL.

      ">> P/O Data Display
      SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                    BPUMN BPUMZ REPOS
          INTO (IT_ZSIVIT-MENGE_PO, IT_ZSIVIT-UEBTO,
                IT_ZSIVIT-UEBTK,    IT_ZSIVIT-WEPOS,
                IT_ZSIVIT-ELIKZ,    IT_ZSIVIT-LOEKZ,
                IT_ZSIVIT-UNTTO,    IT_ZSIVIT-BPUMN,
                IT_ZSIVIT-BPUMZ,    IT_ZSIVIT-REPOS)
          FROM   EKPO
          WHERE  EBELN   EQ   IT_ZSIVIT-EBELN
          AND    EBELP   EQ   IT_ZSIVIT-EBELP.

      IF SY-SUBRC EQ 0.
        IF IT_ZSIVIT-LOEKZ NE SPACE.
           CLEAR : ZSIVIT.
           MESSAGE W069 WITH IT_ZSIVIT-EBELN IT_ZSIVIT-EBELP.
           EXIT.
        ENDIF.
      ENDIF.

      ">> Import Request Quantity Display
      SELECT SINGLE MENGE KBETR KWERT KPEIN KMEIN
          INTO (IT_ZSIVIT-MENGE, IT_ZSIVIT-KBETR,
                IT_ZSIVIT-KWERT, IT_ZSIVIT-KPEIN,
                IT_ZSIVIT-KMEIN)
          FROM  ZTREQIT
          WHERE ZFREQNO EQ IT_ZSIVIT-ZFREQNO
          AND   ZFITMNO EQ IT_ZSIVIT-ZFITMNO.

      ">> Customs Clearance Quantity
       SELECT SUM( CCMENGE ) SUM( GRMENGE )
          INTO (IT_ZSIVIT-ZFCCTOT, IT_ZSIVIT-ZFGRTOT)
          FROM  ZTIVIT
          WHERE ZFBLNO  EQ IT_ZSIVIT-ZFBLNO
          AND   ZFBLIT  EQ IT_ZSIVIT-ZFBLIT.

      ">> Basic Customs Clearance Quantity
      IT_ZSIVIT-CCMENGE = IT_ZSIVIT-MENGE_BL - IT_ZSIVIT-ZFCCTOT.
      IF IT_ZSIVIT-CCMENGE LT 0.
         IT_ZSIVIT-CCMENGE = 0.
      ENDIF.

      IT_ZSIVIT-GRMENGE =  IT_ZSIVIT-MENGE_BL - IT_ZSIVIT-ZFGRTOT.
      IF IT_ZSIVIT-GRMENGE LT 0.
         IT_ZSIVIT-GRMENGE = 0.
      ENDIF.

      IF IT_ZSIVIT-BPUMN IS INITIAL. IT_ZSIVIT-BPUMN = 1. ENDIF.
      IF IT_ZSIVIT-PEINH IS INITIAL. IT_ZSIVIT-PEINH = 1. ENDIF.

      IT_ZSIVIT-ZFIVAMT = IT_ZSIVIT-CCMENGE                   *
                        ( IT_ZSIVIT-BPUMZ / IT_ZSIVIT-BPUMN ) *
                        ( IT_ZSIVIT-NETPR / IT_ZSIVIT-PEINH ).

      ">> Local Currency Compute
      PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIVIT-ZFIVAMT
                                              IT_ZSIVIT-ZFIVAMC
                                              IT_ZSIVIT-ZFIVAMK.

      IF ZTIV-FFACT IS INITIAL.  ZTIV-FFACT = 1.  ENDIF.

      IT_ZSIVIT-ZFIVAMK = ( ZTIV-ZFEXRT / ZTIV-FFACT )
                                        * IT_ZSIVIT-ZFIVAMK.

      PERFORM SET_CURR_CONV_TO_INTERNAL USING
                                  IT_ZSIVIT-ZFIVAMK ZTIV-ZFKRW.

      IT_ZSIVIT-UMSON  =  'X'.

    ">> IF Quantity is zero Then Internal table delete
    IF IT_ZSIVIT-GRMENGE LE 0. CLEAR IT_ZSIVIT-UMSON. ENDIF.
    IF NOT IT_ZSIVIT-ZFPOTY IS INITIAL AND IT_ZSIVIT-ZFPOTY NE 'S'.
       CLEAR  IT_ZSIVIT-UMSON.
    ENDIF.

    ">> Text Material G/R Mark Clear
    IF IT_ZSIVIT-EBELN  IS INITIAL.
       IF IT_ZSIVIT-ZFPOTY EQ 'S' AND IT_ZSIVIT-MATNR IS INITIAL.
          CLEAR : IT_ZSIVIT-GRMENGE, IT_ZSIVIT-UMSON.
       ENDIF.
    ENDIF.

   ">> P/O GR Marking Clear
   IF NOT IT_ZSIVIT-EBELN IS INITIAL.
      IF IT_ZSIVIT-WEPOS IS INITIAL.
         CLEAR IT_ZSIVIT-UMSON.
      ENDIF.
   ENDIF.

   IT_ZSIVIT-ZFNOCCMN = IT_ZSIVIT-MENGE_BL - IT_ZSIVIT-ZFCCTOT.

*-----------------------------------------------------------------------
* Data Validation Check
*-----------------------------------------------------------------------
      ">> Payment Type Check.
      SELECT SINGLE * FROM  ZTREQHD
                      WHERE ZFREQNO   EQ   IT_ZSIVIT-ZFREQNO.

      IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
         MESSAGE E319(ZIM1) WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
         EXIT.
      ENDIF.

      ">> Currency Check.
      SELECT SINGLE * FROM  ZTBL
                      WHERE ZFBLNO    EQ  IT_ZSIVIT-ZFBLNO.
      IF ZTBL-ZFBLAMC IS INITIAL.
         MOVE  ZTBL-ZFBLAMC  TO  ZTIV-ZFIVAMC.
      ENDIF.
      IF ZTIV-ZFIVAMC NE ZTBL-ZFBLAMC.
         MESSAGE E379
         WITH ZTIV-ZFIVAMC IT_ZSIVIT-ZFBLNO ZTBL-ZFBLAMC.
         EXIT.
      ENDIF.

*      ">> Import Type Check
*      IF ZTIV-ZFRPTTY NE ZTBL-ZFRPTTY.
*         MESSAGE E325(ZIM1)
*         WITH ZTIV-ZFRPTTY IT_ZSIVIT-ZFBLNO ZTBL-ZFRPTTY.
*         EXIT.
*      ENDIF.
*
*      ">> Mode of Transportation Check.
*      IF ZTIV-ZFVIA NE ZTBL-ZFVIA.
*         MESSAGE E320(ZIM1)
*         WITH ZTIV-ZFVIA IT_ZSIVIT-ZFBLNO ZTBL-ZFVIA.
*         EXIT.
*      ENDIF.
*
*      ">> Arrival Port Check.
*      IF ZTIV-ZFAPRTC NE ZTBL-ZFAPRTC.
*         MESSAGE E321(ZIM1)
*         WITH ZTIV-ZFAPRTC IT_ZSIVIT-ZFBLNO ZTBL-ZFAPRTC.
*         EXIT.
*      ENDIF.
*
      ">> Company Code Check.
      IF ZTIV-BUKRS IS INITIAL.
         ZTIV-BUKRS  =  ZTBL-BUKRS.
      ENDIF.

      SELECT SINGLE * FROM T001 WHERE BUKRS EQ ZTIV-BUKRS.
      MOVE : T001-WAERS        TO    IT_ZSIVIT-ZFKRW,
             ZTIV-ZFIVAMC      TO    IT_ZSIVIT-ZFIVAMC.


      ">> Item Lock
      PERFORM P2000_SET_IVIT_LOCK_ITEM.

      IF W_SY_SUBRC EQ 0.
         MODIFY IT_ZSIVIT  INDEX W_TABIX.
      ELSE.
        IT_ZSIVIT-ZFIVDNO = TC_3111-CURRENT_LINE * 10.
        APPEND  IT_ZSIVIT.
      ENDIF.
      EXIT.
   ENDIF.
 ENDIF.

ENDMODULE.                 " IT_ZSIVIT_UPDATE_SCR3111  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PLANNED_AMOUNT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_PLANNED_AMOUNT_SCR0102 INPUT.

  IF ZTBL-ZFPLACYN EQ 'X'.
     MESSAGE  I977 WITH 'Planed Freight Use Check!'.
  ELSE.
     MESSAGE  I977 WITH 'Actual Freight Use Check!'.
  ENDIF.

ENDMODULE.                 " CHECK_PLANNED_AMOUNT_SCR0102  INPUT
