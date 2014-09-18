*----------------------------------------------------------------------*
***INCLUDE ZRIM01F04 .
*----------------------------------------------------------------------*
*&  프로그램명 : 수입 B/L 관련 Sub MODULE Include                      *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.10.11                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P3000_GET_NETPRICE_PER_WEIGHT.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_GET_NETPRICE_PER_WEIGHT.

  DATA :  WL_TOWTM(3).

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      INPUT  = ZTBL-ZFTOWTM
    IMPORTING
      OUTPUT = WL_TOWTM.

  IF ZTBL-ZFVIA = 'AIR'.
    SELECT SINGLE * FROM ZTIMIMG21
            WHERE BUKRS = ZTBL-BUKRS
              AND ZFCD  = ZTBL-ZFCD
              AND MEINS = ZTBL-ZFTOWTM.

    CASE WL_TOWTM.
      WHEN 'KG'.
        IF ZTBL-ZFTOWT LT 45.
          ZTBL-ZFUPWT = 45.
          PERFORM SET_AIR_NETPRICE USING '1'.
        ELSEIF ZTBL-ZFTOWT LT 100.
          ZTBL-ZFUPWT = 100.
          PERFORM SET_AIR_NETPRICE USING '2'.
        ELSEIF ZTBL-ZFTOWT LT 300.
          ZTBL-ZFUPWT = 300.
          PERFORM SET_AIR_NETPRICE USING '3'.
        ELSEIF ZTBL-ZFTOWT LT 500.
          ZTBL-ZFUPWT = 500.
          PERFORM SET_AIR_NETPRICE USING '4'.
        ELSEIF ZTBL-ZFTOWT LT 1000.
          ZTBL-ZFUPWT = 1000.
          PERFORM SET_AIR_NETPRICE USING '5'.
        ELSEIF ZTBL-ZFTOWT LT 4000.
          ZTBL-ZFUPWT = 4000.
          PERFORM SET_AIR_NETPRICE USING '6'.
        ENDIF.
      WHEN 'LB'.
        IF ZTBL-ZFTOWT LT 100.
          ZTBL-ZFUPWT = 100.
          PERFORM SET_AIR_NETPRICE USING '1'.
        ELSEIF ZTBL-ZFTOWT LT 220.
          ZTBL-ZFUPWT = 220.
          PERFORM SET_AIR_NETPRICE USING '2'.
        ELSEIF ZTBL-ZFTOWT LT 660.
          ZTBL-ZFUPWT = 660.
          PERFORM SET_AIR_NETPRICE USING '3'.
        ELSEIF ZTBL-ZFTOWT LT 1100.
          ZTBL-ZFUPWT = 1100.
          PERFORM SET_AIR_NETPRICE USING '4'.
        ELSEIF ZTBL-ZFTOWT LT 2200.
          ZTBL-ZFUPWT = 2200.
          PERFORM SET_AIR_NETPRICE USING '5'.
        ELSEIF ZTBL-ZFTOWT LT 8800.
          ZTBL-ZFUPWT = 8800.
          PERFORM SET_AIR_NETPRICE USING '6'.
        ENDIF.
    ENDCASE.
  ELSEIF ZTBL-ZFVIA = 'VSL'.
    SELECT SINGLE * FROM ZTIMIMG22
            WHERE BUKRS = ZTBL-BUKRS
              AND ZFCD  = ZTBL-ZFCD.

    CASE ZTBL-ZFSHTY .
      WHEN 'F'.
        ZTBL-ZFNETPR1 = ZTIMIMG22-ZF20MLB.
        ZTBL-ZFNETPR2 = ZTIMIMG22-ZF40MLB.
        ZTBL-ZFNETPR3 = ZTIMIMG22-ZF45MLB.
        ZTBL-ZFNETPR4 = ZTIMIMG22-ZF40MLB * ZTIMIMG22-ZFHQ.
      WHEN 'L'.
        ZTBL-ZFNETPR1 = ZTIMIMG22-ZFLCL.
        CLEAR ZTBL-ZFNETPR2 .
      WHEN OTHERS.
        CLEAR : ZTBL-ZFNETPR1, ZTBL-ZFNETPR2,
                ZTBL-ZFNETPR3, ZTBL-ZFNETPR4.
    ENDCASE.
  ENDIF.
ENDFORM.                    " P3000_GET_NETPRICE_PER_WEIGHT
*&---------------------------------------------------------------------*
*&      Form  SET_AIR_NETPRICE
*&---------------------------------------------------------------------*
FORM SET_AIR_NETPRICE USING    VALUE(P_GUBUN).

  CASE P_GUBUN.
    WHEN '1'.
      ZTBL-ZFNETPR1 = ZTIMIMG21-ZF45KLT. "기본중량의 단가.
      ZTBL-ZFNETPR2 = ZTIMIMG21-ZF100KLT. "차상위중량의 단가.
    WHEN '2'.
      ZTBL-ZFNETPR1 = ZTIMIMG21-ZF100KLT. "기본중량의 단가.
      ZTBL-ZFNETPR2 = ZTIMIMG21-ZF300KLT. "차상위중량의 단가.
    WHEN '3'.
      ZTBL-ZFNETPR1 = ZTIMIMG21-ZF300KLT. "기본중량의 단가.
      ZTBL-ZFNETPR2 = ZTIMIMG21-ZF500KLT. "차상위중량의 단가.
    WHEN '4'.
      ZTBL-ZFNETPR1 = ZTIMIMG21-ZF500KLT. "기본중량의 단가.
      ZTBL-ZFNETPR2 = ZTIMIMG21-ZF1000KLT. "차상위중량의 단가.
    WHEN '5'.
      ZTBL-ZFNETPR1 = ZTIMIMG21-ZF1000KLT. "기본중량의 단가.
      ZTBL-ZFNETPR2 = ZTIMIMG21-ZF4000KLT. "차상위중량의 단가.
    WHEN '6'.
      ZTBL-ZFNETPR1 = ZTIMIMG21-ZF4000KLT. "기본중량의 단가.
*      ZTBL-ZFNETPR2 = ZTIMIMG21-ZF4000KLT. "차상위중량의 단가.

  ENDCASE.

ENDFORM.                    " SET_AIR_NETPRICE
*&---------------------------------------------------------------------*
*&      Form  P3000_CALCULATE_FREIGHT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_CALCULATE_FREIGHT.

  CLEAR : WA_BLCALC-ZFCAMT1, WA_BLCALC-ZFCAMT2, WA_BLCALC-ZFTOTAMT.

  IF ZTBL-ZFVIA = 'AIR'.
    WA_BLCALC-ZFCAMT1 = ZTBL-ZFTOWT * ZTBL-ZFNETPR1
                                    * ( ZTBL-ZFMRATE / 100 ).
    WA_BLCALC-ZFCAMT2 = ZTBL-ZFUPWT * ZTBL-ZFNETPR2
                                    * ( ZTBL-ZFMRATE / 100 ).
  ELSEIF ZTBL-ZFVIA = 'VSL'.
    CASE ZTBL-ZFSHTY.
      WHEN 'F'.
*>> 20 Feet.
        WA_BLCALC-ZFCAMT1 = ZTBL-ZF20FT * ZTBL-ZFNETPR1
                                        * ( ZTBL-ZFMRATE / 100 ).
*>> 20 Feet H.Q.
        WA_BLCALC-ZFCAMT3 = ZTBL-ZF20FHQ * ZTBL-ZFNETPR3
                          * ( ZTBL-ZFMRATE / 100 ) .
*>> 40 Feet.
        WA_BLCALC-ZFCAMT2 = ZTBL-ZF40FT * ZTBL-ZFNETPR2
                                        * ( ZTBL-ZFMRATE / 100 ).
*>> 40 Feet H.Q.
        WA_BLCALC-ZFCAMT4 = ZTBL-ZF40FHQ * ZTBL-ZFNETPR4
                          * ( ZTBL-ZFMRATE / 100 ) .
*>      Total
        WA_BLCALC-ZFTOTAMT = WA_BLCALC-ZFCAMT1 + WA_BLCALC-ZFCAMT2
                           + WA_BLCALC-ZFCAMT3 + WA_BLCALC-ZFCAMT4.

      WHEN 'L'.
        WA_BLCALC-ZFCAMT1 = ZTBL-ZFTOWT * ZTBL-ZFNETPR1
                                        * ( ZTBL-ZFMRATE / 100 ).

      WHEN OTHERS.

    ENDCASE.
  ENDIF.

ENDFORM.                    " P3000_CALCULATE_FREIGHT
*&---------------------------------------------------------------------*
*&      Form  P3000_SET_BASIC_CHARGE
*&---------------------------------------------------------------------*
FORM P3000_SET_BASIC_CHARGE.

  CASE ZTBL-ZFVIA .
    WHEN 'AIR'.
      READ TABLE IT_ZSBLCST WITH KEY ZFCSCD = 'ABC'.
      CLEAR : IT_ZSBLCST-ZFCAMT, IT_ZSBLCST-ZFCKAMT.

      IF ZTBL-ZFDFUP IS INITIAL.  " Basic.
        IT_ZSBLCST-ZFCAMT = WA_BLCALC-ZFCAMT1.
      ELSE.                       " Upgrade.
        IT_ZSBLCST-ZFCAMT = WA_BLCALC-ZFCAMT2.
      ENDIF.

      IF ZTBL-ZFPLACYN EQ 'X'.
         IT_ZSBLCST-ZFCAMT = ZTBL-ZFPLAMT.
      ENDIF.

      MODIFY IT_ZSBLCST TRANSPORTING ZFCAMT ZFCKAMT
                        WHERE ZFCSCD = 'ABC'.

    WHEN 'VSL'.
      READ TABLE IT_ZSBLCST WITH KEY ZFCSCD = 'OBC'.
      CLEAR : IT_ZSBLCST-ZFCAMT, IT_ZSBLCST-ZFCKAMT.

      IF ZTBL-ZFSHTY = 'F'.
        IT_ZSBLCST-ZFCAMT = WA_BLCALC-ZFTOTAMT.
      ELSEIF ZTBL-ZFSHTY = 'L'.
        IT_ZSBLCST-ZFCAMT = WA_BLCALC-ZFCAMT1.
      ENDIF.

      IF ZTBL-ZFPLACYN EQ 'X'.
         IT_ZSBLCST-ZFCAMT = ZTBL-ZFPLAMT.
      ENDIF.

      MODIFY IT_ZSBLCST TRANSPORTING ZFCAMT ZFCKAMT
                        WHERE ZFCSCD = 'OBC'.

  ENDCASE.

ENDFORM.                    " P3000_SET_BASIC_CHARGE
*&---------------------------------------------------------------------*
*&      Form  P3000_EXCHAGE_CHARGE
*&---------------------------------------------------------------------*
FORM P3000_EXCHAGE_CHARGE.

  IF IT_ZSBLCST-ZFEXRT IS INITIAL.
    IT_ZSBLCST-ZFCKAMT = 0.
  ELSE.
    IF IT_ZSBLCST-WAERS NE T001-WAERS.
      IT_ZSBLCST-ZFCKAMT = IT_ZSBLCST-ZFEXRT
                         * IT_ZSBLCST-ZFCAMT.

      SELECT SINGLE * FROM  TCURX
             WHERE  CURRKEY     = IT_ZSBLCST-WAERS.
      IF SY-SUBRC NE 0.
        TCURX-CURRDEC = 2.
      ENDIF.

      IF TCURX-CURRDEC NE 0.
        PERFORM SET_CURR_CONV_TO_INTERNAL USING
               IT_ZSBLCST-ZFCKAMT IT_ZSBLCST-KRW.
      ENDIF.
    ELSE.
      IT_ZSBLCST-ZFCKAMT = IT_ZSBLCST-ZFCAMT.
    ENDIF.
    W_AMOUNT             = IT_ZSBLCST-ZFCKAMT.
  ENDIF.
ENDFORM.                    " P3000_EXCHAGE_CHARGE
*&---------------------------------------------------------------------*
*&      Form  P3000_ZSBLCST_INIT_SETTING
*&---------------------------------------------------------------------*
FORM P3000_ZSBLCST_INIT_SETTING.

  IF IT_ZSBLCST-WAERS IS INITIAL.
    SELECT SINGLE * FROM ZTIMIMG08
                   WHERE ZFCDTY = '004'
                     AND ZFCD   = IT_ZSBLCST-ZFCSCD.
    IF ZTIMIMG08-ZFCURR EQ 'B'.
      MOVE : ZTBL-ZFTRCUR        TO    IT_ZSBLCST-WAERS.
    ELSEIF ZTIMIMG08-ZFCURR EQ 'K'.
      MOVE : T001-WAERS          TO    IT_ZSBLCST-WAERS.
    ENDIF.
  ENDIF.

  IT_ZSBLCST-KRW = T001-WAERS.

  IF IT_ZSBLCST-WAERS  EQ  T001-WAERS.
    MOVE  IT_ZSBLCST-ZFCAMT  TO  IT_ZSBLCST-ZFCKAMT.
    MOVE  1                  TO  IT_ZSBLCST-ZFEXRT.
  ELSE.
    MOVE  ZTBL-ZFEXRTT        TO IT_ZSBLCST-ZFEXRT.
    PERFORM P3000_EXCHAGE_CHARGE.
  ENDIF.

ENDFORM.                    " P3000_ZSBLCST_INIT_SETTING
*&---------------------------------------------------------------------*
*&      Form  P3000_SERVICE_ITEM_GET
*&---------------------------------------------------------------------*
FORM P3000_SERVICE_ITEM_GET.

  SELECT  BELNR  INTO CORRESPONDING FIELDS OF TABLE IT_TEMP
                 FROM  EKBE
                 WHERE EBELN  EQ  EKPO-EBELN
                 AND   EBELP  EQ  EKPO-EBELP
                 AND   VGABE  EQ  '9'
                 AND   SHKZG  EQ  'S'.

  LOOP  AT  IT_TEMP.

    " 취소된 자료인지 CHECK.
    SELECT  SINGLE  *  FROM  EKBE
            WHERE   EBELN    EQ  EKPO-EBELN
            AND     EBELP    EQ  EKPO-EBELP
            AND     VGABE    EQ  '1'
            AND     BELNR    EQ  ( SELECT MAX( BELNR )
                                   FROM   EKBE
                                   WHERE  EBELN  EQ  EKPO-EBELN
                                   AND    EBELP  EQ  EKPO-EBELP
                                   AND    VGABE  EQ  '1'
                                   AND    LFBNR  EQ  IT_TEMP-BELNR ).
    IF EKBE-SHKZG NE 'S'.  CONTINUE.  ENDIF.

    "I/V 처리한 자료인지 CHECK.
    SELECT SINGLE *  FROM  EKBE
           WHERE  EBELN    EQ   EKPO-EBELN
           AND    EBELP    EQ   EKPO-EBELP
           AND    VGABE    EQ   '2'
           AND    BELNR    EQ   ( SELECT MAX( BELNR )
                                  FROM   EKBE
                                  WHERE  EBELN   EQ  EKPO-EBELN
                                  AND    EBELP   EQ  EKPO-EBELP
                                  AND    VGABE   EQ  '2'
                                  AND    LFBNR   EQ  IT_TEMP-BELNR ).
    IF SY-SUBRC EQ 0 AND EKBE-SHKZG EQ 'S'.  CONTINUE.  ENDIF.

    MOVE  : EKPO-WERKS         TO    IT_ZSCIVIT-WERKS,
            EKPO-LGORT         TO    IT_ZSCIVIT-LGORT,
            ZTREQHD-WAERS      TO    IT_ZSCIVIT-ZFIVAMC,
            EKPO-EBELN         TO    IT_ZSCIVIT-EBELN,
            EKPO-EBELP         TO    IT_ZSCIVIT-EBELP,
            'KRW'              TO    IT_ZSCIVIT-ZFKRW,
            EKBE-WRBTR         TO    IT_ZSCIVIT-ZFIVAMT,
            EKBE-WRBTR         TO    IT_ZSCIVIT-ZFIVAMP,
            IT_TEMP-BELNR      TO    IT_ZSCIVIT-ZFSVNO,
            'X'                TO    ZTCIVHD-ZFSVYN.

*>> 처리 원화 금액 계산...
    PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSCIVIT-ZFIVAMP
                                            IT_ZSCIVIT-ZFIVAMC
                                            IT_ZSCIVIT-ZFIVAMK.
    IF ZTCIVHD-FFACT IS INITIAL.
      ZTCIVHD-FFACT = 1.
    ENDIF.

    IT_ZSCIVIT-ZFIVAMK = ( ZTCIVHD-ZFEXRT / ZTCIVHD-FFACT )
                                        * IT_ZSCIVIT-ZFIVAMK.

    PERFORM SET_CURR_CONV_TO_INTERNAL USING
           IT_ZSCIVIT-ZFIVAMK 'KRW'.

    IF NOT IT_ZSCIVIT-WERKS IS INITIAL AND
           ZTCIVHD-BUPLA    IS INITIAL.
      SELECT SINGLE * FROM  T001W
                      WHERE WERKS EQ IT_ZSCIVIT-WERKS.
      MOVE T001W-J_1BBRANCH TO ZTCIVHD-BUPLA.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " P3000_SERVICE_ITEM_GET
*&---------------------------------------------------------------------*
*&      Form  P2000_ITEM_DATA_CHECK
*&---------------------------------------------------------------------*
FORM P2000_ITEM_DATA_CHECK.

  CLEAR : ZTIDRHS-ZFHSAM.

  LOOP  AT  IT_ZSIDRHSD_S.

    W_TABIX  =  SY-TABIX.

    " 자재또는 규격은 입력해야 함.
    IF IT_ZSIDRHSD_S-ZFSTCD  IS INITIAL  AND
       IT_ZSIDRHSD_S-ZFGDDS1 IS INITIAL.
      MESSAGE  E231(ZIM1) WITH W_TABIX.
      EXIT.
    ENDIF.

    " 수량 입력 필수.
    IF IT_ZSIDRHSD_S-ZFQNT   IS INITIAL.
      MESSAGE  E232(ZIM1) WITH W_TABIX.
      EXIT.
    ENDIF.

    " 수량단위 입력 필수.
    IF IT_ZSIDRHSD_S-ZFQNTM  IS INITIAL.
      MESSAGE  E233(ZIM1) WITH W_TABIX.
      EXIT.
    ENDIF.

    " 단가 입력 필수.
    IF IT_ZSIDRHSD_S-NETPR   IS INITIAL.
      MESSAGE  E234(ZIM1) WITH W_TABIX.
      EXIT.
    ENDIF.

    " 프라이머리 키값 SET.
    IF IT_ZSIDRHSD_S-ZFBLNO  IS INITIAL.
      MOVE  ZTIDRHS-ZFBLNO  TO  IT_ZSIDRHSD_S-ZFBLNO.
    ENDIF.
    IF IT_ZSIDRHSD_S-ZFCLSEQ IS INITIAL.
      MOVE  ZTIDRHS-ZFCLSEQ TO  IT_ZSIDRHSD-ZFCLSEQ.
    ENDIF.
    IF IT_ZSIDRHSD_S-ZFCONO  IS INITIAL.
      MOVE ZTIDRHS-ZFCONO   TO IT_ZSIDRHSD-ZFCONO.
    ENDIF.
    IF IT_ZSIDRHSD_S-ZFRONO  IS INITIAL.
      SELECT MAX( ZFRONO )  INTO  W_RONO
      FROM   ZTIDRHSD
      WHERE  ZFBLNO         EQ    IT_ZSIDRHSD_S-ZFBLNO
      AND    ZFCLSEQ        EQ    IT_ZSIDRHSD_S-ZFCLSEQ
      AND    ZFCONO         EQ    IT_ZSIDRHSD_S-ZFCONO.

      LOOP  AT  IT_ZSIDRHSD_S.
        IF W_RONO LE IT_ZSIDRHSD_S-ZFRONO.
          MOVE  IT_ZSIDRHSD_S-ZFRONO  TO  W_RONO.
        ENDIF.
      ENDLOOP.

      W_RONO  =  W_RONO  +  10.
    ENDIF.

    MODIFY  IT_ZSIDRHSD_S  INDEX  W_TABIX.
    ZTIDRHS-ZFHSAM  =  IT_ZSIDRHSD_S-ZFAMT  +  ZTIDRHS-ZFHSAM.
  ENDLOOP.

ENDFORM.                    " P2000_ITEM_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_REQUESTER_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_REQUESTER_DISPLAY  USING    P_EBELN
                                       P_EBELP.

  SELECT SINGLE * FROM EKPO
                 WHERE EBELN = P_EBELN
                   AND EBELP = P_EBELP.

  IF EKPO-AFNAM IS INITIAL.
    MESSAGE E468(ZIM1).
  ENDIF.

  DATA : W_AFNAM LIKE BAPIBNAME-BAPIBNAME.
  W_AFNAM = EKPO-AFNAM .
  TRANSLATE W_AFNAM TO UPPER CASE.

  CALL FUNCTION 'SUSR_USER_MAINT_WITH_DIALOG'
    EXPORTING
      MAINT_FOR_OWN_USER_ONLY = 'X'
      DISPLAY_ONLY            = 'X'
      USER_TO_DISPLAY         = W_AFNAM
    EXCEPTIONS
      ERROR_WRITING_TO_DB     = 1
      OTHERS                  = 2.

ENDFORM.                    " P2000_REQUESTER_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_IVIT_LOCK_ITEM
*&---------------------------------------------------------------------*
FORM P2000_SET_IVIT_LOCK_ITEM.

   CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLDOC'
        EXPORTING
             ZFBLNO = IT_ZSIVIT-ZFBLNO
        EXCEPTIONS
             OTHERS = 1.
   IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'B/L Document'
                   IT_ZSIVIT-ZFBLNO ''
                   RAISING DOCUMENT_LOCKED.
   ELSE.
      IT_IV_LOCKED-LOCKED = 'X'.
      IT_IV_LOCKED-ZFBLNO = IT_ZSIVIT-ZFBLNO.
      APPEND IT_IV_LOCKED.
   ENDIF.

ENDFORM.                    " P2000_SET_IVIT_LOCK_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_IVIT_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_IVIT_LOCK USING    PA_MODE.

  LOOP AT IT_IV_LOCKED WHERE LOCKED = 'X'.
     CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLDOC'
          EXPORTING
              ZFBLNO = IT_IV_LOCKED-ZFBLNO.
  ENDLOOP.

ENDFORM.                    " P2000_SET_IVIT_LOCK
*&---------------------------------------------------------------------*
*&      Form  P3000_PLANED_FREIGHT_CLAC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_PLANED_FREIGHT_CLAC USING    P_CSCD
                                        P_EBELN
                                        P_EBELP
                                        P_MENGE
                                        P_AMOUNT
                               CHANGING W_PLANED_FREIGHT.

   DATA : W_NET_PRICE(13)  TYPE P  DECIMALS 2.

   CLEAR : EKKO, KONV, EKPO, ZTIMIMG08.
   SELECT SINGLE * FROM EKKO WHERE EBELN EQ P_EBELN.

   SELECT SINGLE * FROM ZTIMIMG08
   WHERE  ZFCDTY   EQ   '004'
   AND    ZFCD     EQ   P_CSCD.

   SELECT SINGLE * FROM KONV
   WHERE  KNUMV    EQ   EKKO-KNUMV
   AND    KPOSN    EQ   P_EBELP
   AND    KSCHL    EQ   ZTIMIMG08-COND_TYPE
   AND    KBETR    GT   0.

   IF KONV-KRECH EQ 'A'.
      W_PLANED_FREIGHT = P_AMOUNT * KONV-KBETR / 1000.
   ELSE.
      W_PLANED_FREIGHT = P_MENGE  * KONV-KBETR.
   ENDIF.

ENDFORM.                    " P3000_PLANED_FREIGHT_CLAC
