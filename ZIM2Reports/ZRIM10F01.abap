*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM10F01                                                  *
*&---------------------------------------------------------------------*
*&  Program Name : Main SUB MODULE                                     *
*&  Created By   : Na Hyun Joo                                         *
*&  Created On   : 2003.10.08                                          *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_GUI_TEXT
*&---------------------------------------------------------------------*
FORM P2000_SET_GUI_TEXT.

  CASE W_STATUS.
    WHEN C_REQ_C.   ASSIGN W_CREATE  TO <FS_F>.
    WHEN C_REQ_U.
      IF SY-TCODE EQ 'ZIMCC1'.
        ASSIGN W_REQUEST TO <FS_F>.
      ELSE.
        ASSIGN W_CHANGE  TO <FS_F>.
      ENDIF.
    WHEN C_REQ_D.   ASSIGN W_DISPLAY TO <FS_F>.
    WHEN C_OPEN_C.  ASSIGN W_OPEN    TO <FS_F>.
    WHEN C_OPEN_U.  ASSIGN W_STATUS  TO <FS_F>.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_GUI_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_PF_STATUS.

  CASE SY-TCODE.
    WHEN 'ZIMCD2' OR 'ZIMCD3'.                    " Customs Declaration
      MOVE 'IMDR' TO W_PFSTAT.
    WHEN 'ZIMCC1'  OR 'ZIMCC2' OR 'ZIMCC3'.        " Customs Clearance
      MOVE 'IMDS' TO W_PFSTAT.
    WHEN 'ZIMT1'  OR 'ZIMT2'  OR 'ZIMT3'.
      MOVE 'TRST' TO W_PFSTAT.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " P2000_SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_STATUS_TCODE_DISABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM P2000_SET_STATUS_TCODE_DISABLE.

  CASE SY-TCODE.
    WHEN 'ZIMCC1' OR 'ZIMT1'.                               " Create
      MOVE 'CRDC'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Create
      MOVE 'DELE'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Delete
      MOVE 'OPCL'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " OPEN CANC
      MOVE 'EDIS'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " EDI SEND
      MOVE 'CSDL'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Charge. Doc
      MOVE 'PSDL'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Posting Can
      MOVE 'PRES'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Entry Sum
      MOVE 'HIST'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " HEAD CHAN
      MOVE 'CKEK'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " CHECK
    WHEN 'ZIMCD2' OR 'ZIMCC2' OR 'ZIMT2'.                  " Change
      MOVE 'COPY'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Copy.
      MOVE 'CHDC'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Change
      MOVE 'CKEK'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Check
      MOVE 'OPCL'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Open Canc
      MOVE 'IMPREQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Imp. Req
    WHEN 'ZIMCD3' OR 'ZIMCC3' OR 'ZIMT3'.                  " Display
      MOVE 'COPY'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Copy
      MOVE 'DISP'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Display
      MOVE 'SAVE'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Save
      MOVE 'IMPREQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Imp.Req
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_STATUS_TCODE_DISABLE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_STATUS_SCR_DISABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SET_STATUS_SCR_DISABLE.

  CASE SY-DYNNR.
*-----------------------------------------------------------------------
* Customs Declaration
*-----------------------------------------------------------------------
    WHEN  0100.
      W_STATUS = C_REQ_U.
      SET TITLEBAR  'IMDRI' WITH W_REQUEST.
      MOVE 'SAVE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'REOG' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'SVCO' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DELE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DSBL' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DSIV' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'EDIS' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
    WHEN  0101.
      CLEAR ZTIMIMGTX.
      SELECT SINGLE * FROM ZTIMIMGTX WHERE BUKRS EQ ZTBL-BUKRS.

      CASE SY-TCODE.
        WHEN 'ZIMCD2'.
          SET TITLEBAR 'IMDRD' WITH W_REQUEST.
          MOVE 'CHDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        WHEN 'ZIMCD3'.
          SET TITLEBAR 'IMDRD' WITH W_DISPLAY.
          MOVE 'REOG' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
          MOVE 'SVCO' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        WHEN OTHERS.
      ENDCASE.
    WHEN  0200.
      W_STATUS = C_REQ_D.
      SET TITLEBAR  'IMDRI' WITH W_DISPLAY.
      MOVE 'EDIS' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'REOG' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'SVCO' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DSIV' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DSBL' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DELE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'SAVE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
*-----------------------------------------------------------------------
* Customs Clearance
*-----------------------------------------------------------------------
    WHEN  1100.
      W_STATUS = C_REQ_C.
      SET TITLEBAR  'IMDSI' WITH W_CREATE.
      MOVE 'OTDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'CRDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'SAVE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'PRES' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DELE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DSBL' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DSIV' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'HIST' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'COST' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DSDR' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'HIIT' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
    WHEN  1101.
      CASE SY-TCODE.
        WHEN 'ZIMCC1'.
          SET TITLEBAR  'IMDSD' WITH W_CREATE.
          MOVE 'OTDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
          MOVE 'DSBL' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
          MOVE 'CRDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
          MOVE 'CUCL' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
          MOVE 'DELE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
          MOVE 'PRES' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        WHEN 'ZIMCC2'.
          SET TITLEBAR  'IMDSD' WITH W_CHANGE.
          MOVE 'DSBL' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
          MOVE 'CHDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
          MOVE 'OTDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
          MOVE 'CRDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        WHEN 'ZIMCC3'.
          SET TITLEBAR  'IMDSD' WITH W_DISPLAY.
          MOVE 'OTDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
          MOVE 'DSBL' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
          MOVE 'CRDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
          MOVE 'DISP' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
          MOVE 'SAVE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      ENDCASE.
    WHEN  1200.
      W_STATUS = C_REQ_U.
      SET TITLEBAR  'IMDSI' WITH W_CHANGE.
      MOVE 'SAVE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'PRES' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'CHDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DELE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DSBL' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DSIV' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'HIST' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'COST' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DSDR' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'HIIT' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
    WHEN  1300.
      W_STATUS = C_REQ_D.
      SET TITLEBAR  'IMDSI' WITH W_DISPLAY.
      MOVE 'DISP' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'SAVE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'PRES' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DELE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DSBL' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DSIV' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'HIST' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'COST' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'HIIT' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'DSDR' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
*-----------------------------------------------------------------------
* Delivery Order
*-----------------------------------------------------------------------
    WHEN '2100'.
      SET TITLEBAR  'TRTL' WITH W_CHANGE.
      MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'POST' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'CSDL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'PSCL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
    WHEN '2200'.
      SET TITLEBAR  'TRTL' WITH W_CHANGE.
      MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'POST' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'CSDL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'PSCL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
    WHEN '2300'.
      SET TITLEBAR  'TRTL' WITH W_DISPLAY.
      MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'POST' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'CSDL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'PSCL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " P2000_SET_STATUS_SCR_DISABLE
*&---------------------------------------------------------------------*
*&      Form  P2000_READ_HS_DATA
*&---------------------------------------------------------------------*
FORM P2000_READ_HS_DATA.

  REFRESH : IT_ZSIDRUSH, IT_ZSIDRUSH_ORG.
  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_ZSIDRUSH
  FROM   ZTIDRUSH
  WHERE  ZFIVNO  EQ  ZTIDRUS-ZFIVNO
  AND    ZFCLSEQ EQ  ZTIDRUS-ZFCLSEQ.
  IT_ZSIDRUSH_ORG[] = IT_ZSIDRUSH[].

ENDFORM.                    " P2000_READ_HS_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_READ_MATERIAL_DATA
*&---------------------------------------------------------------------*
FORM P2000_READ_MATERIAL_DATA.

  REFRESH : IT_ZSIDRUSD, IT_ZSIDRUSD_ORG, IT_ZSIDRUSD_SEL.
  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_ZSIDRUSD
  FROM   ZTIDRUSD
  WHERE  ZFIVNO  EQ  ZTIDRUS-ZFIVNO
  AND    ZFCLSEQ EQ  ZTIDRUS-ZFCLSEQ.
  IT_ZSIDRUSD_ORG[] = IT_ZSIDRUSD[].

  SELECT *
  INTO    CORRESPONDING FIELDS OF TABLE IT_ZSIDRUSD_SEL
  FROM    ZTIDRUSD
  WHERE   ZFIVNO  EQ  ZTIDRUS-ZFIVNO
  AND     ZFCLSEQ EQ  ZTIDRUS-ZFCLSEQ
  AND     ZFCONO  EQ  '001'.

ENDFORM.                    " P2000_READ_MATERIAL_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR_MODE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SCR_MODE_SET.

  SELECT SINGLE * FROM ZTIMIMG00.

  LOOP AT SCREEN.
    CASE W_STATUS.
      WHEN C_REQ_C OR C_REQ_U.
        IF SCREEN-GROUP1 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
        IF W_STATUS EQ C_REQ_U.
          IF SCREEN-GROUP2 EQ 'CR'.
            IF SCREEN-NAME(15) = 'ZSMSCST-ZFAPRTC' AND
               W_NEW_STATUS  = C_REQ_C.
              SCREEN-INPUT = '1'.
            ELSEIF SCREEN-NAME(15) = 'ZSMSCST-ZFAPRTC' AND
               IT_ZSMSCST-ZFAPRTC  IS  INITIAL.
              SCREEN-INPUT = '1'.
            ELSE.
              SCREEN-INPUT = '0'.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN  C_ADD_U OR C_BL_SEND.
        IF SCREEN-GROUP2 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_REQ_D OR C_ADD_D OR C_OPEN_D.
        IF SCREEN-GROUP1 = 'I'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
      WHEN C_OPEN_C OR C_BL_REAL.
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_OPEN_U.
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN OTHERS.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " P2000_SCR_MODE_SET
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ACTION_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_ACTION_TITLE.

  CASE W_STATUS.
    WHEN C_REQ_C.   ASSIGN  W_CREATE       TO <FS_F>.       " Create
    WHEN C_REQ_U.   ASSIGN  W_CHANGE       TO <FS_F>.       " Change
    WHEN C_REQ_D.   ASSIGN  W_DISPLAY      TO <FS_F>.       " Display
    WHEN C_OPEN_C.  ASSIGN  W_OPEN         TO <FS_F>.       " Settlement
    WHEN C_OPEN_U.  ASSIGN  W_STAUTS       TO <FS_F>.       " Status Chg
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_ACTION_TITLE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_DOC_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_DOC_TITLE.

  CASE SY-DYNNR.
    WHEN '0101'.
      ASSIGN  C_CD              TO <FS_DOC>.
    WHEN '1101'.
      ASSIGN  C_CC              TO <FS_DOC>.
    WHEN '2101'.
      ASSIGN  C_TR              TO <FS_DOC>.
  ENDCASE.

ENDFORM.                    " P2000_SET_DOC_TITLE
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_EX_RATE_NODIALOG
*&---------------------------------------------------------------------*
FORM P2000_GET_EX_RATE_NODIALOG        USING   P_WAERS
                                               P_WAERS_TO
                                               P_DATE
                                    CHANGING   P_EXRATE
                                               P_FACTOR.

  DATA : L_TEXT_EXRATE(255)    TYPE C,
         L_FOREIGN_FACTOR(255) TYPE C,
         L_FACTOR              TYPE P.

  CLEAR :  P_EXRATE, P_FACTOR.

*  No input CURRENCY , Exchage Date!
  IF P_WAERS IS INITIAL.   P_EXRATE = 0.  EXIT.   ENDIF.
  IF P_DATE  IS INITIAL.   P_EXRATE = 0.  EXIT.   ENDIF.

* MESSAGE BOX
  IF P_WAERS EQ P_WAERS_TO.  P_EXRATE = 1.  EXIT.  ENDIF.

  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
       EXPORTING
            DATE             = P_DATE
            FOREIGN_AMOUNT   = 0
            FOREIGN_CURRENCY = P_WAERS
            LOCAL_CURRENCY   = P_WAERS_TO
            TYPE_OF_RATE     = 'M'
       IMPORTING
            EXCHANGE_RATE    = L_TEXT_EXRATE
            FOREIGN_FACTOR   = L_FOREIGN_FACTOR
            LOCAL_AMOUNT     = W_LOCAL_AMT
            FIXED_RATE       = W_FIXED_RATE
       EXCEPTIONS
            OTHERS           = 01.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
               RAISING AMOUNT_CALCULATION.
  ENDIF.

  PERFORM    P2000_WRITE_NO_MASK  CHANGING  L_TEXT_EXRATE.
  PERFORM    P2000_WRITE_NO_MASK  CHANGING  L_FOREIGN_FACTOR.

  P_EXRATE = L_TEXT_EXRATE.
  L_FACTOR = L_FOREIGN_FACTOR.
  IF P_EXRATE LT 0.
    P_EXRATE = P_EXRATE * -1.
  ENDIF.

  P_FACTOR = L_FACTOR.

ENDFORM.                    " P2000_GET_EX_RATE_NODIALOG

*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_NO_MASK
*&---------------------------------------------------------------------*
FORM P2000_WRITE_NO_MASK CHANGING P_TEXT_AMOUNT.

  SELECT SINGLE * FROM USR01 WHERE BNAME EQ SY-UNAME.

  CASE USR01-DCPFM.
    WHEN 'X'.    " Decimal point is period: N,NNN.NN
      PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT ',' ' '.
      CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
    WHEN 'Y'.    " Decimal point is N NNN NNN,NN
      PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  ',' '.'.
      CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
    WHEN OTHERS. " Decimal point is comma: N.NNN,NN
      PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  '.' ' '.
      PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  ',' '.'.
      CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
  ENDCASE.

ENDFORM.                    " P2000_WRITE_NO_MASK
*&---------------------------------------------------------------------*
*&      Form  TAX_RECOMPUTE_CUSTOMS_DECLARAT
*&---------------------------------------------------------------------*
FORM TAX_RECOMPUTE_CUSTOMS_DECL.

  ">> Tarrif Rate Get.
  CLEAR : A902, KONP.
  SELECT SINGLE * FROM A902
  WHERE  KSCHL    EQ   W_KSCHL
  AND    STAWN    EQ   W_STAWN.

  SELECT SINGLE * FROM KONP
  WHERE  KNUMH    EQ   A902-KNUMH
  AND    KSCHL    EQ   W_KSCHL.

  IF IT_ZSIDRUSH-ZFCURT IS INITIAL.
    IT_ZSIDRUSH-ZFCURT  =  KONP-KBETR  /  10.
  ENDIF.

  ">> Harbor Maintenance Fee
  SELECT SINGLE * FROM ZTIMIMG08
  WHERE  ZFCDTY   EQ   '006'
  AND    ZFCD     EQ   '003'.
  MOVE : ZTIMIMG08-ZFMNAMT  TO  W_HM_MIN,
         ZTIMIMG08-ZFMXAMT  TO  W_HM_MAX.
  IF IT_ZSIDRUSH-ZFHMRT IS INITIAL.
    MOVE ZTIMIMG08-ZFMRATE  TO  IT_ZSIDRUSH-ZFHMRT.
  ENDIF.
  ">> Merchandise Processing Fee
  SELECT SINGLE * FROM ZTIMIMG08
  WHERE  ZFCDTY   EQ   '006'
  AND    ZFCD     EQ   '004'.
  MOVE : ZTIMIMG08-ZFMNAMT  TO  W_MP_MIN,
         ZTIMIMG08-ZFMXAMT  TO  W_MP_MAX.
  IF IT_ZSIDRUSH-ZFMPRT IS INITIAL.
    MOVE ZTIMIMG08-ZFMRATE  TO  IT_ZSIDRUSH-ZFMPRT.
  ENDIF.

  IT_ZSIDRUSH-ZFCUAMT  =  IT_ZSIDRUSH-ZFTBAK
                                     * ( IT_ZSIDRUSH-ZFCURT / 100 ).

  ">> HMF Compute.
  IF ZTBL-ZFVIA EQ 'VSL'.
    W_HMF  =  IT_ZSIDRUSH-ZFTBAK * ( IT_ZSIDRUSH-ZFHMRT / 100 ).
    IF NOT W_HM_MIN IS INITIAL AND W_HM_MIN GT W_HMF.
      MOVE  W_HM_MIN  TO  IT_ZSIDRUSH-ZFHMAMT.
    ENDIF.
    IF NOT W_HM_MAX IS INITIAL AND W_HM_MAX LE W_HMF.
      MOVE  W_HM_MAX  TO  IT_ZSIDRUSH-ZFHMAMT.
    ENDIF.
  ENDIF.

  " MPF Compute.
  W_MPF  =  IT_ZSIDRUSH-ZFTBAK * ( IT_ZSIDRUSH-ZFHMRT / 100 ).
  IF NOT W_MP_MIN IS INITIAL AND W_MP_MIN GT W_MPF.
    MOVE  W_MP_MIN  TO  IT_ZSIDRUSH-ZFMPAMT.
  ENDIF.
  IF NOT W_MP_MAX IS INITIAL AND W_MP_MAX LE W_MPF.
    MOVE  W_MP_MAX  TO  IT_ZSIDRUSH-ZFMPAMT.
  ENDIF.

ENDFORM.                    " TAX_RECOMPUTE_CUSTOMS_DECLARAT
*&---------------------------------------------------------------------*
*&      Form  SET_CURR_CONV_TO_EXTERNAL
*&---------------------------------------------------------------------*
FORM SET_CURR_CONV_TO_EXTERNAL USING    P_AMOUNT
                                        P_WAERS
                                        P_TO_AMOUNT.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
       EXPORTING
            CURRENCY        = P_WAERS
            AMOUNT_INTERNAL = P_AMOUNT
       IMPORTING
            AMOUNT_EXTERNAL = BAPICURR-BAPICURR.

  P_TO_AMOUNT = BAPICURR-BAPICURR.

ENDFORM.                    " SET_CURR_CONV_TO_EXTERNAL
*&---------------------------------------------------------------------*
*&      Form  SET_CURR_CONV_TO_INTERNAL
*&---------------------------------------------------------------------*
FORM SET_CURR_CONV_TO_INTERNAL USING    P_AMOUNT
                                        P_WAERS.

  BAPICURR-BAPICURR = P_AMOUNT.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
       EXPORTING
            CURRENCY             = P_WAERS
            AMOUNT_EXTERNAL      = BAPICURR-BAPICURR
            MAX_NUMBER_OF_DIGITS = DIGITS
       IMPORTING
            AMOUNT_INTERNAL      = P_AMOUNT
       EXCEPTIONS
            OTHERS               = 1.

ENDFORM.                    " SET_CURR_CONV_TO_INTERNAL
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DOC_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTIDRUS_ZFBLNO  text
*----------------------------------------------------------------------*
FORM P2000_BL_DOC_DISPLAY USING    P_ZFBLNO.

  IF P_ZFBLNO IS INITIAL.
    MESSAGE E311.
  ENDIF.

  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.

  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_BL_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
FORM P2000_CHANGE_SYMBOL USING    P_AMOUNT  P_FROM  P_TO.

  DO.
    REPLACE  P_FROM   WITH   P_TO  INTO    P_AMOUNT.
    IF  SY-SUBRC  <>    0.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_EXIT_PROCESS.

  IF NOT W_STATUS EQ C_REQ_D.

    PERFORM P2000_SET_MESSAGE USING  OK-CODE.

    CASE ANTWORT.
      WHEN 'Y'.              " Yes...
*-----------------------------------------------------------------------
* DB Write
*-----------------------------------------------------------------------
        PERFORM  P3000_DB_MODIFY_SCRCOM.
        CLEAR OK-CODE.
        PERFORM  P2000_SET_UNLOCK.
        PERFORM  P2000_SET_SCREEN_SCRCOM.
        LEAVE SCREEN.
      WHEN 'N'.              " No...
        MESSAGE  S957.
        CLEAR OK-CODE.
        PERFORM  P2000_SET_UNLOCK.
        PERFORM  P2000_SET_SCREEN_SCRCOM.
        LEAVE SCREEN.
      WHEN 'C'.              " Cancel
      WHEN OTHERS.
    ENDCASE.
  ELSE.
    CLEAR OK-CODE.
    PERFORM  P2000_SET_SCREEN_SCRCOM.
    LEAVE SCREEN.
  ENDIF.

ENDFORM.                    " P2000_EXIT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INDICATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_INDICATE.

  W_OK_CODE = OK-CODE.
  IF W_STATUS EQ C_REQ_D .
    PERFORM P2000_SET_LOCK.
  ENDIF.

  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      CASE W_OK_CODE.
        WHEN 'DELE'.   " 삭제.
          CASE SY-DYNNR.
            WHEN '0101'.
              PERFORM  P3000_DEC_DEL_STATUS_CHECK.
            WHEN '1101'.
              PERFORM  P3000_CUS_DEL_STATUS_CHECK.
            WHEN '2101'.
              PERFORM  P3000_DEL_DEL_STATUS_CHECK.
          ENDCASE.
      ENDCASE.
      PERFORM  P3000_DB_MODIFY_SCRCOM.
      CLEAR OK-CODE.
      PERFORM  P2000_SET_UNLOCK.
      PERFORM  P2000_SET_SCREEN_SCRCOM.
      LEAVE SCREEN.
    WHEN 'N'.
  ENDCASE.

ENDFORM.                    " P2000_SET_INDICATE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_INIT_SCREEN.

  W_OK_CODE = OK-CODE.

  IF W_STATUS NE C_REQ_D.
    PERFORM P2000_SET_MESSAGE USING  'ANZG'.
  ELSE.
    ANTWORT  =  'Y'.
  ENDIF.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      PERFORM  P3000_DB_MODIFY_SCRCOM.
      CLEAR OK-CODE.
    WHEN 'N'.              " No...
      MESSAGE  S957.
      CLEAR OK-CODE.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

* LEAVE TO TRANSACTION
  PERFORM   P2000_SET_TRANSACTION.

ENDFORM.                    " P2000_SET_INIT_SCREEN
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SET_MESSAGE USING    P_SY_UCOMM.

  MOVE  OK-CODE  TO  W_OK_CODE.
  CASE P_SY_UCOMM.
    WHEN 'ANZG'.      " CHANGE  ==> DISPLAY
      PERFORM  P2000_ANZG_MESSAGE.
    WHEN 'IMPREQ'.
      PERFORM  P2000_IMPREQ_MESSAGE.
    WHEN 'SAVE'.      " 저장?
      PERFORM  P2000_SAVE_MESSAGE.
    WHEN 'CANC'.      " 취소?
      PERFORM  P2000_CANCEL_MESSAGE.
    WHEN 'BACK' OR 'EXIT'.   " 앞으로 or 종?
      PERFORM  P2000_EXIT_MESSAGE.
    WHEN 'DELE'.      " 삭제?
      PERFORM  P2000_DELETE_MESSAGE.
    WHEN 'SVCO'.      " Save and Confirm
      PERFORM  P2000_SAVE_CONFIRM_MESSAGE.
    WHEN 'CUCL'.      " 통관내용 저?
      PERFORM  P2000_CUCL_MESSAGE.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " P2000_SET_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_ANZG_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_ANZG_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING '이동 확인'  " 타이틀...
              '현재 입력내역을 저장하지 않습니다.'
              '저장 후 이동하시겠습니까?'                   " MSG2
              'Y'                  " 취소 버튼 유/?
              '1'.                 " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Move confirm'
             'Do not save entered item.'
             'Do you want to move another screen after save?'
             'Y'
             '1'.
  ENDCASE.

ENDFORM.                    " P2000_ANZG_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_BOX USING    TITLE   LIKE SPOP-TITEL
                                TEXT1   LIKE SPOP-TEXTLINE1
                                TEXT2   LIKE SPOP-TEXTLINE2
                                CANCEL  LIKE CANCEL_OPTION
                                DEFAULT LIKE OPTION.

  SPOP-TITEL     = TITLE.
  SPOP-TEXTLINE1 = TEXT1.
  SPOP-TEXTLINE2 = TEXT2.
  IF CANCEL EQ 'Y'.
    CANCEL_OPTION = 'Y'.
  ELSE.
    CLEAR : CANCEL_OPTION.
  ENDIF.
  OPTION = DEFAULT.
  TEXTLEN = 60.

  CALL SCREEN 0001 STARTING AT 30 6
                   ENDING   AT 78 10.

  IF ANTWORT = 'C'.       " Cancel
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_MESSAGE_BOX

*&---------------------------------------------------------------------*
*&      Form  P2000_IMPREQ_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_IMPREQ_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING '이동 확인'
                                   '현재 입력내역을 저장하지 않습니다.'
                                    '저장 후 이동하시겠습니까?'
                                    'N'
                                    '1'.
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'End confirm'
             'Do not save the entered item.'
             'Do you want to end after save?'
             'Y'
             '1'.
  ENDCASE.

ENDFORM.                    " P2000_IMPREQ_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SAVE_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING '저장 확인'
                                      '입력된 내역을 저장합니다.'
                                      '저장하시겠습니까?'
                                      'Y'
                                      '1'.
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING 'Save confirm '
                                      'Being saved entered item.'
                                      'Do you want to save?'
                                      'Y'
                                      '1'.
  ENDCASE.

ENDFORM.                    " P2000_SAVE_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_CANCEL_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING '취소 확인'
                                   '변경된 내용을 저장없이 종료됩니다.'
                                    '종료하시겠습니까?'
                                    'N'
                                    '2'.
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING 'Cancel confirm'
                          'Being ended without saving the changed item.'
                          'Do you want to end?'
                          'N'
                          '2'.
  ENDCASE.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      MESSAGE  S957.
      LEAVE TO SCREEN 0.  " " PROGRAM LEAVING
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_CANCEL_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_EXIT_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING '종료 확인'
                               '현재 입력내역을 저장하지 않습니다.'
                               '저장 후 종료하시겠습니까?'
                               'Y'
                               '1'.
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING 'End confirm'
                                      'Do not save the entered item.'
                                      'Do you want to end after save?'
                                      'Y'
                                      '1'.
  ENDCASE.

ENDFORM.                    " P2000_EXIT_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_DELETE_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING '삭제 확인'
                                   '현재 Document를 삭제합니다.'
                                   '삭제하시겠습니까?'
                                   'N'
                                   '1'.
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING  'Delete confirm'
                                  'Being deleted the current document.'
                                  'Do you want to delete?'
                                  'N'
                                  '1'.
  ENDCASE.

ENDFORM.                    " P2000_DELETE_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_CONFIRM_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SAVE_CONFIRM_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING '저장 확인'
                                   '입력된 내역을 확인하고 저장합니다.'
                                    '저장하시겠습니까?'
                                    'Y'
                                    '1'.
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING 'Save confirm'
                                      'Do save with confirm sign.'
                                      'Do you want to save?'
                                      'Y'
                                      '1'.
  ENDCASE.

ENDFORM.                    " P2000_SAVE_CONFIRM_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_CUCL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_CUCL_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING '저장 확인'
                                      '통관내용을 저장합니다.'
                                      '저장하시겠습니까?'
                                      'Y'
                                      '1'.
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING 'Save confirm'
                                      'Now do save the clearance data.'
                                      'Do you want to save?'
                                      'Y'
                                      '1'.
  ENDCASE.

ENDFORM.                    " P2000_CUCL_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_DB_MODIFY_SCRCOM
*&---------------------------------------------------------------------*
FORM P3000_DB_MODIFY_SCRCOM.

  CASE SY-TCODE.
    WHEN 'ZIMCD2' OR 'ZIMCD3'.
      PERFORM  P3000_DECLARATION_MODIFY.
    WHEN 'ZIMCC1' OR 'ZIMCC2' OR 'ZIMCC3'.
      PERFORM  P3000_CUSTOMS_CLEARANCE_MODIFY.
    WHEN 'ZIMT1'  OR 'ZIMT2'  OR 'ZIMT3'.
      PERFORM  P3000_DELIVERY_ORDER_MODIFY.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P3000_DB_MODIFY_SCRCOM

*&---------------------------------------------------------------------*
*&      Form  P3000_DECLARATION_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_DECLARATION_MODIFY.

  IF W_OK_CODE EQ 'DELE'.
     CLEAR : W_COUNT.
     SELECT COUNT( DISTINCT BELNR ) INTO W_COUNT
     FROM   ZVIMCOST
     WHERE  ZFCSTGRP EQ  '006'
     AND    ZFIMDNO  EQ  ZTIV-ZFIVNO
     AND    ZFPOSYN  NE  'N'.
     IF W_COUNT GT 0.
        MESSAGE E406(ZIM1).  EXIT.
     ENDIF.
  ELSE.
     DESCRIBE TABLE IT_ZSIDRUSH LINES LINE.
     IF LINE = 0. MESSAGE E762. EXIT. ENDIF.

     DESCRIBE TABLE IT_ZSIDRUSD LINES LINE.
     IF LINE = 0. MESSAGE E763. EXIT. ENDIF.
  ENDIF.

  CALL FUNCTION 'ZIM_ZTIDRUS_DOC_MODIFY'
       EXPORTING
            W_OK_CODE       = W_OK_CODE
            ZFIVNO          = ZTIDRUS-ZFIVNO
            ZFCLSEQ         = ZTIDRUS-ZFCLSEQ
            ZFSTATUS        = W_STATUS
            W_ZTIDRUS_OLD   = *ZTIDRUS
            W_ZTIDRUS       = ZTIDRUS
       TABLES
            IT_ZSIDRUSH_OLD = IT_ZSIDRUSH_ORG
            IT_ZSIDRUSH     = IT_ZSIDRUSH
            IT_ZSIDRUSD_OLD = IT_ZSIDRUSD_ORG
            IT_ZSIDRUSD     = IT_ZSIDRUSD
       EXCEPTIONS
            ERROR_UPDATE    = 1
            ERROR_DELETE    = 2.

  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    MESSAGE  S765.
  ELSE.
    ROLLBACK WORK.
    MESSAGE  E952.
  ENDIF.

ENDFORM.                    " P3000_DECLARATION_MODIFY

*&---------------------------------------------------------------------*
*&      Form  P3000_CUSTOMS_CLEARANCE_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_CUSTOMS_CLEARANCE_MODIFY.

  IF W_OK_CODE EQ 'DELE'.
    CLEAR : W_COUNT.
    SELECT COUNT( DISTINCT BELNR ) INTO W_COUNT
             FROM   ZVIMCOST
             WHERE  ZFCSTGRP EQ '006'
             AND    ZFIMDNO  EQ ZTIV-ZFIVNO
             AND    ZFPOSYN  NE 'N'.
    IF W_COUNT GT 0.
       MESSAGE E406(ZIM1).
       EXIT.
    ENDIF.
  ELSE.
    DESCRIBE TABLE IT_ZSIDSUSH LINES LINE.
    IF LINE = 0. MESSAGE E762. EXIT.  ENDIF.

    DESCRIBE TABLE IT_ZSIDSUSD LINES LINE.
    IF LINE = 0. MESSAGE E763. EXIT.  ENDIF.

    SELECT SINGLE * FROM  ZTIMIMG02
                    WHERE ZFCOTM EQ ZTIDSUS-ZFINRC.

    CALL FUNCTION 'FI_VENDOR_DATA'
         EXPORTING
              I_BUKRS        = ZTIDSUS-BUKRS
              I_LIFNR        = ZTIMIMG02-ZFVEN
         IMPORTING
              E_KRED         = VF_KRED
         EXCEPTIONS
              VENDOR_MISSING = 4.

    IF SY-SUBRC NE 0.
       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF ZTIDSUS-ZFDUTY IS INITIAL.
      MESSAGE W167 WITH 'Total customs'.
    ENDIF.
    IF ZTIDSUS-ZFOTFE IS INITIAL.
      MESSAGE W167 WITH 'Other Fee'.
    ENDIF.
    IF ZTIDSUS-ZFIVAMK IS INITIAL.
      MESSAGE W167 WITH 'Taxable price(Local)'.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ZIM_ZTIDSUS_DOC_MODIFY'
       EXPORTING
            W_OK_CODE       = W_OK_CODE
            ZFIVNO          = ZTIDSUS-ZFIVNO
            ZFCLSEQ         = ZTIDSUS-ZFCLSEQ
            ZFSTATUS        = W_STATUS
            W_ZTIDSUS_OLD   = *ZTIDSUS
            W_ZTIDSUS       = ZTIDSUS
       TABLES
            IT_ZSIDSUSH_OLD = IT_ZSIDSUSH_ORG
            IT_ZSIDSUSH     = IT_ZSIDSUSH
            IT_ZSIDSUSD_OLD = IT_ZSIDSUSD_ORG
            IT_ZSIDSUSD     = IT_ZSIDSUSD
       EXCEPTIONS
            ERROR_UPDATE    = 1
            ERROR_DELETE    = 2
            ERROR_INSERT    = 3.

  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    MESSAGE  S765.
  ELSE.
    ROLLBACK WORK.
    MESSAGE  E952.
  ENDIF.

ENDFORM.                    " P3000_CUSTOMS_CLEARANCE_MODIFY

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_UNLOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_UNLOCK.

  CASE SY-TCODE.
    WHEN 'ZIMCD2'.
      PERFORM  P3000_SET_DECLARATION_LOCK  USING 'U'.
    WHEN 'ZIMCC1' OR 'ZICC2'.
      PERFORM  P3000_SET_CLEARANCE_LOCK    USING 'U'.
    WHEN 'ZIMT1'  OR 'ZIMT2'.
      PERFORM P2000_SET_TR_DOC_LOCK        USING 'U'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_UNLOCK

*&---------------------------------------------------------------------*
*&      Form  P3000_SET_DECLARATION_LOCK
*&---------------------------------------------------------------------*
FORM P3000_SET_DECLARATION_LOCK USING    PA_MODE..

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIDRUS'
         EXPORTING
              ZFIVNO  = ZTIDRUS-ZFIVNO
              ZFCLSEQ = ZTIDRUS-ZFCLSEQ
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Import declaration Document'
                            ZTIDRUS-ZFIVNO ZTIDRUS-ZFCLSEQ
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIDRUS'
         EXPORTING
              ZFIVNO  = ZTIDRUS-ZFIVNO
              ZFCLSEQ = ZTIDRUS-ZFCLSEQ.
  ENDIF.

ENDFORM.                    " P3000_SET_DECLARATION_LOCK

*&---------------------------------------------------------------------*
*&      Form  P3000_SET_CLEARANCE_LOCK
*&---------------------------------------------------------------------*
FORM P3000_SET_CLEARANCE_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIDSUS'
         EXPORTING
              ZFIVNO  = ZTIDSUS-ZFIVNO
              ZFCLSEQ = ZTIDSUS-ZFCLSEQ
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Customs clearance Document'
                            ZTIDSUS-ZFIVNO ZTIDSUS-ZFCLSEQ
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIDSUS'
         EXPORTING
              ZFIVNO  = ZTIDSUS-ZFIVNO
              ZFCLSEQ = ZTIDSUS-ZFCLSEQ.
  ENDIF.

ENDFORM.                    " P3000_SET_CLEARANCE_LOCK

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SCREEN_SCRCOM
*&---------------------------------------------------------------------*
FORM P2000_SET_SCREEN_SCRCOM.

* CALL Transaction => Program Leave
  IF SY-CALLD EQ 'X'.  LEAVE.  ENDIF.

  CASE W_STATUS.
    WHEN C_REQ_C.
      CASE SY-TCODE.
        WHEN 'ZIMCC1'.    SET SCREEN 1100.  " Customs Clearance
        WHEN 'ZIMT1'.     SET SCREEN 2100.  " Delivery Order
        WHEN OTHERS.      SET SCREEN 100.
      ENDCASE.
    WHEN C_REQ_U.
      CASE SY-TCODE.
        WHEN 'ZIMCD2'.    SET SCREEN 0100.  " Customs Declaration
        WHEN 'ZIMCC2'.    SET SCREEN 1200.  " Customs Clearance
        WHEN 'ZIMT2'.     SET SCREEN 2200.  " Delivery Order
        WHEN OTHERS.      SET SCREEN 0100.
      ENDCASE.
    WHEN C_REQ_D.
      CASE SY-TCODE.
        WHEN 'ZIMCD3'.    SET SCREEN 0200.  " Customs Declaration
        WHEN 'ZIMCC3'.    SET SCREEN 1300.  " Customs Clearance
        WHEN 'ZIMT3'.     SET SCREEN 2300.  " Delivery Order
        WHEN OTHERS.      SET SCREEN 0200.
      ENDCASE.
    WHEN OTHERS.
      SET SCREEN 0.
  ENDCASE.
* 초기화면으로 exit시 Active Tab을 Clearing?
  CLEAR :  TABSTRIP-ACTIVETAB.

ENDFORM.                    " P2000_SET_SCREEN_SCRCOM
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_TRANSACTION.

  CASE SY-TCODE.
    WHEN 'ZIMCD2'  OR  'ZIMCD3'.
      CASE W_OK_CODE.
        WHEN 'OTDC'.
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CHDC'.                                        " CHANGE
          LEAVE TO TRANSACTION 'ZIMCD2' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.                                        " DISPLAY
          LEAVE TO TRANSACTION 'ZIMCD3' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIMCC1'  OR  'ZIMCC2'   OR  'ZIMCC3'.
      CASE W_OK_CODE.
        WHEN 'OTDC'.
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.                                        " CREATE
          LEAVE TO TRANSACTION 'ZIMCC1'.
        WHEN 'CHDC'.                                        " CHANGE
          LEAVE TO TRANSACTION 'ZIMCC2' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.                                        " DISPLAY
          LEAVE TO TRANSACTION 'ZIMCC3' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
  ENDCASE.

ENDFORM.                    " P2000_SET_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_LOCK.

  CASE SY-TCODE.
    WHEN 'ZIMCD1' OR 'ZIMCD2'.
      PERFORM  P3000_SET_DECLARATION_LOCK  USING 'L'.
    WHEN 'ZIMCC1' OR 'ZIMCC2' OR 'ZIMCC3'.
      PERFORM  P3000_SET_CLEARANCE_LOCK    USING 'L'.
    WHEN 'ZIMT1'  OR 'ZIMT2'  OR 'ZIMT3'.
      PERFORM P2000_SET_TR_DOC_LOCK        USING 'L'.
  ENDCASE.

ENDFORM.                    " P2000_SET_LOCK
*&---------------------------------------------------------------------*
*&      Form  P3000_CUS_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P3000_CUS_DEL_STATUS_CHECK.

*>> 수입 IMG 사항 SELECT!
  SELECT SINGLE * FROM ZTIMIMG00.
*>> 회계처리한 자료가 존재하면 DELETE 불가!
  W_LINE = 0.
  IF ZTIMIMG00-ZFCSTMD EQ 'I'.
    SELECT COUNT( * ) INTO W_COUNT
           FROM  ZTCUCLCST
           WHERE ZFBLNO   = ZTIDSUS-ZFBLNO
           AND   ZFCLSEQ  = ZTIDSUS-ZFCLSEQ
           AND   ZFACDO   NE SPACE
           AND   ZFFIYR   NE SPACE.
  ELSE.
    SELECT COUNT( * ) INTO W_COUNT
           FROM  ZTBSEG  AS  A  INNER JOIN ZTBKPF AS B
           ON    A~BUKRS        EQ    B~BUKRS
           AND   A~GJAHR        EQ    B~GJAHR
           AND   A~BELNR        EQ    B~BELNR
           WHERE A~ZFIMDNO      EQ    ZTIDSUS-ZFIVNO
           AND   A~ZFCSTGRP     EQ    '006'
           AND ( B~ZFACDO       NE    SPACE
           OR    B~ZFFIYR       NE    SPACE    ).
  ENDIF.
  IF W_COUNT GT 0.
    MESSAGE E519 WITH ZTIDSUS-ZFBLNO ZTIDSUS-ZFCLSEQ W_COUNT.
  ENDIF.

*----------------------------------------------------------------------*
*>> 통관요청한 자료가 입고완료만 삭제 불가!
*----------------------------------------------------------------------*
  CLEAR ZTIV.
  SELECT SINGLE * FROM ZTIV WHERE ZFIVNO EQ ZTIDSUS-ZFIVNO.
  IF ZTIV-ZFGRST EQ 'Y' OR ZTIV-ZFGRST EQ 'P'.
    MESSAGE E654 WITH ZTIDSUS-ZFBLNO ZTIDSUS-ZFCLSEQ.
  ENDIF.

  CLEAR : W_COUNT.
  SELECT COUNT( * ) INTO W_COUNT
         FROM ZTIVIT
         WHERE ZFBLNO EQ ZTIDSUS-ZFBLNO
         AND   NDFTX  EQ 'X'.
  IF W_COUNT GT 0.
    MESSAGE E418(ZIM1).
  ENDIF.

ENDFORM.                    " P3000_CUS_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_DEC_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P3000_DEC_DEL_STATUS_CHECK.

  SELECT SINGLE * FROM ZTIV
  WHERE  ZFIVNO   EQ   ZTIDRUS-ZFIVNO.
  IF ZTIV-ZFCUST EQ 'Y'.
    MESSAGE  E755.  EXIT.
  ENDIF.

  SELECT SINGLE * FROM ZTIV
  WHERE  ZFIVNO   EQ   ZTIDRUS-ZFIVNO.
  IF ZTIV-ZFGRST  EQ  'Y'.
    MESSAGE E654 WITH ZTIDRUS-ZFBLNO ZTIDRUS-ZFCLSEQ.
  ENDIF.

ENDFORM.                    " P3000_DEC_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_OK_CODE_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_OK_CODE_PROCESS.

  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'ENTR'.
    WHEN 'COPY' OR 'REBL' OR 'LOPU'.
    WHEN 'CRDC'.                         " Create
      CASE SY-TCODE.
        WHEN 'ZIMCC1' OR 'ZIMCC2' OR 'ZIMCC3'.
          LEAVE TO TRANSACTION 'ZIMCC1'.
        WHEN 'ZIMT0' OR 'ZIMT1' OR 'ZIMT2' OR 'ZIMT3'.
          LEAVE TO TRANSACTION 'ZIMT0'.
      ENDCASE.
    WHEN 'CHDC'.                         " Change
      CASE SY-TCODE.
        WHEN 'ZIMCD2' OR 'ZIMCD3'.
          LEAVE TO TRANSACTION 'ZIMCD2'.
        WHEN 'ZIMCC1' OR 'ZIMCC2' OR 'ZIMCC3'.
          LEAVE TO TRANSACTION 'ZIMCC2'.
        WHEN 'ZIMT0' OR 'ZIMT1' OR 'ZIMT2' OR 'ZIMT3'.
          LEAVE TO TRANSACTION 'ZIMT2'.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'DISP'.                         " Display
      CASE SY-TCODE.
        WHEN 'ZIMCD2' OR 'ZIMCD3'.
          LEAVE TO TRANSACTION 'ZIMCD3'.
        WHEN 'ZIMCC1' OR 'ZIMCC2' OR 'ZIMCC3'.
          LEAVE TO TRANSACTION 'ZIMCC3'.
        WHEN 'ZIMT0' OR 'ZIMT1' OR 'ZIMT2' OR 'ZIMT3'.
          LEAVE TO TRANSACTION 'ZIMT3'.
        WHEN OTHERS.
      ENDCASE.
    WHEN OTHERS.
      LEAVE TO TRANSACTION OK-CODE.
  ENDCASE.

ENDFORM.                    " P2000_OK_CODE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDRUSH_TO_IT
*&---------------------------------------------------------------------*
FORM READ_ZTIDRUSH_TO_IT.

  REFRESH : IT_ZSIDSUSH, IT_ZSIDSUSH_ORG.

  SELECT * FROM   ZTIDRUSH
           WHERE  ZFIVNO   EQ  ZTIDRUS-ZFIVNO
           AND    ZFCLSEQ  EQ  ZTIDRUS-ZFCLSEQ.
    MOVE-CORRESPONDING ZTIDRUSH TO IT_ZSIDSUSH.
    APPEND IT_ZSIDSUSH.
  ENDSELECT.

ENDFORM.                    " READ_ZTIDRUSH_TO_IT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTIDRUSD_TO_IT
*&---------------------------------------------------------------------*
FORM READ_ZTIDRUSD_TO_IT.

  REFRESH : IT_ZSIDSUSD, IT_ZSIDSUSD_ORG, IT_ZSIDSUSD_SEL.
  SELECT * FROM    ZTIDRUSD
           WHERE   ZFIVNO   EQ   ZTIDRUS-ZFIVNO
           AND     ZFCLSEQ  EQ   ZTIDRUS-ZFCLSEQ.
    MOVE-CORRESPONDING ZTIDRUSD TO IT_ZSIDSUSD.
    APPEND IT_ZSIDSUSD.
  ENDSELECT.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIDSUSD_SEL
  FROM   ZTIDRUSD
  WHERE  ZFIVNO    EQ  ZTIDRUS-ZFIVNO
  AND    ZFCLSEQ   EQ  ZTIDRUS-ZFCLSEQ
  AND    ZFCONO    EQ  '001'.

ENDFORM.                    " READ_ZTIDRUSD_TO_IT
*&---------------------------------------------------------------------*
*&      Form  TAX_RECOMPUTE_CUSTOMS_CLEARANC
*&---------------------------------------------------------------------*
FORM TAX_RECALC_CUSTOMS_CLEARANCE.

  ">> Tarrif Rate Get.
  CLEAR : A902, KONP.
  SELECT SINGLE * FROM A902
  WHERE  KSCHL    EQ   W_KSCHL
  AND    STAWN    EQ   W_STAWN.

  SELECT SINGLE * FROM KONP
  WHERE  KNUMH    EQ   A902-KNUMH
  AND    KSCHL    EQ   W_KSCHL.

  IF IT_ZSIDSUSH-ZFCURT IS INITIAL.
    IT_ZSIDSUSH-ZFCURT  =  KONP-KBETR  /  100.
  ENDIF.

  ">> Harbor Maintenance Fee
  CLEAR : ZTIMIMG08.
  SELECT SINGLE * FROM ZTIMIMG08
  WHERE  ZFCDTY   EQ   '006'
  AND    ZFCD     EQ   '003'.
  MOVE : ZTIMIMG08-ZFMNAMT  TO  W_HM_MIN,
         ZTIMIMG08-ZFMXAMT  TO  W_HM_MAX.

  IF ZTIV-ZFVIA EQ 'VSL'.
    IF IT_ZSIDSUSH-ZFHMRT IS INITIAL.
        MOVE ZTIMIMG08-ZFMRATE  TO  IT_ZSIDSUSH-ZFHMRT.
    ENDIF.
  ENDIF.

  ">> Merchandise Processing Fee
  CLEAR : ZTIMIMG08.
  SELECT SINGLE * FROM ZTIMIMG08
  WHERE  ZFCDTY   EQ   '006'
  AND    ZFCD     EQ   '004'.
  MOVE : ZTIMIMG08-ZFMNAMT  TO  W_MP_MIN,
         ZTIMIMG08-ZFMXAMT  TO  W_MP_MAX.
  IF IT_ZSIDSUSH-ZFMPRT IS INITIAL.
    MOVE ZTIMIMG08-ZFMRATE  TO  IT_ZSIDSUSH-ZFMPRT.
  ENDIF.

  IF IT_ZSIDSUSH-ZFCUAMT IS INITIAL.
    IT_ZSIDSUSH-ZFCUAMT  =  IT_ZSIDSUSH-ZFTBAK
                                    * ( IT_ZSIDSUSH-ZFCURT / 100 ).
  ENDIF.

  ">> HMF Compute.
  IF ZTIV-ZFVIA EQ 'VSL'.
    IF IT_ZSIDSUSH-ZFHMAMT IS INITIAL.
      W_HMF  =  IT_ZSIDSUSH-ZFTBAK * ( IT_ZSIDSUSH-ZFHMRT / 100 ).
      IF NOT W_HM_MIN IS INITIAL AND W_HM_MIN GT W_HMF.
        MOVE  W_HM_MIN  TO  IT_ZSIDSUSH-ZFHMAMT.
      ENDIF.
      IF NOT W_HM_MAX IS INITIAL AND W_HM_MAX LE W_HMF.
        MOVE  W_HM_MAX  TO  IT_ZSIDSUSH-ZFHMAMT.
      ENDIF.
    ENDIF.
  ENDIF.

  " MPF Compute.
  IF ZTIDSUS-ZFENTP NE 'EX' AND ZTIDSUS-ZFENTP NE 'XX'.
     IF IT_ZSIDSUSH-ZFMPAMT IS INITIAL.
        W_MPF  =  IT_ZSIDSUSH-ZFTBAK * ( IT_ZSIDSUSH-ZFHMRT / 100 ).
       IF NOT W_MP_MIN IS INITIAL AND W_MP_MIN GT W_MPF.
          MOVE  W_MP_MIN  TO  IT_ZSIDSUSH-ZFMPAMT.
       ENDIF.
       IF NOT W_MP_MAX IS INITIAL AND W_MP_MAX LE W_MPF.
          MOVE  W_MP_MAX  TO  IT_ZSIDSUSH-ZFMPAMT.
       ENDIF.
     ENDIF.
  ENDIF.

ENDFORM.                    " TAX_RECOMPUTE_CUSTOMS_CLEARANC
*&---------------------------------------------------------------------*
*&      Form  P2000_IDR_DOC_MODIFY
*&---------------------------------------------------------------------*
FORM P2000_IDR_DOC_MODIFY USING    P_ZFBLNO
                                   P_ZFCLSEQ.

  IF P_ZFBLNO IS INITIAL OR P_ZFCLSEQ IS INITIAL.
    MESSAGE E311.
  ENDIF.

  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPCLSEQ' FIELD P_ZFCLSEQ.

  CALL TRANSACTION 'ZIMCD3' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_IDR_DOC_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P2000_DDLC_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_DDLC_PROCESS.

  IF F(8)  EQ 'ZSIDSUSH'.
    IF W_CURRENT_LINE GT 0.
      REFRESH : IT_ZSIDSUSD_SEL.
      READ TABLE IT_ZSIDSUSH  INDEX  W_CURRENT_LINE.
      LOOP AT IT_ZSIDSUSD WHERE ZFCONO   EQ IT_ZSIDSUSH-ZFCONO.
        MOVE-CORRESPONDING IT_ZSIDSUSD  TO IT_ZSIDSUSD_SEL.
        APPEND IT_ZSIDSUSD_SEL.
      ENDLOOP.
    ENDIF.
  ELSEIF F(8) EQ 'ZSIDRUSH'.
    IF W_CURRENT_LINE GT 0.
      REFRESH : IT_ZSIDRUSD_SEL.
      READ TABLE IT_ZSIDRUSH  INDEX  W_CURRENT_LINE.
      LOOP AT IT_ZSIDRUSD WHERE ZFCONO   EQ IT_ZSIDRUSH-ZFCONO.
        MOVE-CORRESPONDING IT_ZSIDRUSD  TO IT_ZSIDRUSD_SEL.
        APPEND IT_ZSIDRUSD_SEL.
      ENDLOOP.
    ENDIF.
  ELSEIF F(6) EQ 'ZSDOHD'.
    IF W_CURRENT_LINE GT 0.
      REFRESH : IT_DOIT_SEL.
      READ TABLE IT_DOHD  INDEX  W_CURRENT_LINE.
      LOOP AT IT_DOIT   WHERE TRAID    EQ IT_DOHD-TRAID.
        MOVE-CORRESPONDING IT_DOIT  TO IT_DOIT_SEL.
        APPEND IT_DOIT_SEL.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_DDLC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_HS_DATA_TO_IT
*&---------------------------------------------------------------------*
FORM READ_HS_DATA_TO_IT.

  REFRESH : IT_ZSIDSUSH, IT_ZSIDSUSH_ORG.
  SELECT  *
  INTO    CORRESPONDING FIELDS OF TABLE IT_ZSIDSUSH
  FROM    ZTIDSUSH
  WHERE   ZFIVNO   EQ  ZTIDSUS-ZFIVNO
  AND     ZFCLSEQ  EQ  ZTIDSUS-ZFCLSEQ .
  IT_ZSIDSUSH_ORG[] = IT_ZSIDSUSH[].

ENDFORM.                    " READ_HS_DATA_TO_IT
*&---------------------------------------------------------------------*
*&      Form  READ_HS_DETAIL_DATA_TO_IT
*&---------------------------------------------------------------------*
FORM READ_HS_DETAIL_DATA_TO_IT.

  REFRESH : IT_ZSIDSUSD, IT_ZSIDSUSD_ORG, IT_ZSIDSUSD_SEL.
  SELECT  *
  INTO    CORRESPONDING FIELDS OF TABLE IT_ZSIDSUSD
  FROM    ZTIDSUSD
  WHERE   ZFIVNO   EQ  ZTIDSUS-ZFIVNO
  AND     ZFCLSEQ  EQ  ZTIDSUS-ZFCLSEQ .
  IT_ZSIDSUSD_ORG[] = IT_ZSIDSUSD[].

  SORT IT_ZSIDSUSH BY ZFBLNO ZFCLSEQ ZFCONO.
  READ TABLE IT_ZSIDSUSH INDEX  1.

  LOOP AT IT_ZSIDSUSD WHERE ZFIVNO  EQ IT_ZSIDSUSH-ZFIVNO
                      AND   ZFCLSEQ EQ IT_ZSIDSUSH-ZFCLSEQ
                      AND   ZFCONO  EQ IT_ZSIDSUSH-ZFCONO.
    MOVE-CORRESPONDING IT_ZSIDSUSD TO IT_ZSIDSUSD_SEL.
    APPEND  IT_ZSIDSUSD_SEL.
  ENDLOOP.

ENDFORM.                    " READ_HS_DETAIL_DATA_TO_IT
*&---------------------------------------------------------------------*
*&      Form  P2000_CALL_COST_SCREEN
*&---------------------------------------------------------------------*
FORM P2000_CALL_COST_SCREEN.

  DESCRIBE  TABLE  IT_ZSIMCOST  LINES  W_LINE.
  IF W_LINE GT 0.
    W_DYNNR  =  SY-DYNNR.
    SET SCREEN 0050.   LEAVE TO SCREEN 0050.
  ELSE.
    MESSAGE S608.
  ENDIF.

ENDFORM.                    " P2000_CALL_COST_SCREEN
*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
FORM P2000_ALV_COMMAND USING R_UCOMM      LIKE SY-UCOMM
                              RS_SELFIELD TYPE SLIS_SELFIELD.
*
  CASE R_UCOMM.
    WHEN  'WAHL'.             " 비용문서 & doubleclick
      IF SY-TCODE(4) EQ 'ZIMP'.
        READ TABLE IT_ZSPMTHST INDEX RS_SELFIELD-TABINDEX. "
        IF SY-SUBRC EQ 0.
          PERFORM  P2000_FI_DOCUMENT_DISPLAY
                                 USING   IT_ZSPMTHST-BUKRS
                                         IT_ZSPMTHST-GJAHR
                                         IT_ZSPMTHST-BELNR.
        ELSE.
          MESSAGE S962.
        ENDIF.

      ELSE.
        READ TABLE IT_ZSIMCOST INDEX RS_SELFIELD-TABINDEX. "
        IF SY-SUBRC EQ 0.
          PERFORM  P2000_FI_DOCUMENT_DISPLAY
                                 USING   IT_ZSIMCOST-BUKRS
                                         IT_ZSIMCOST-ZFFIYR
                                         IT_ZSIMCOST-ZFACDO.
        ELSE.
          MESSAGE S962.
        ENDIF.
      ENDIF.
      CLEAR R_UCOMM.
    WHEN  'DSPY'.             " 비용문서 & doubleclick
      IF SY-TCODE(4) EQ 'ZIMP'.
        READ TABLE IT_ZSPMTHST INDEX RS_SELFIELD-TABINDEX. "
        IF SY-SUBRC EQ 0.
          PERFORM  P2000_FI_DOCUMENT_DISPLAY
                                 USING   IT_ZSPMTHST-BUKRS
                                         IT_ZSPMTHST-ZFPMYR
                                         IT_ZSPMTHST-ZFPMNO.
        ELSE.
          MESSAGE S962.
        ENDIF.
      ENDIF.
      CLEAR R_UCOMM.
    WHEN 'WAH2' OR '&IC1'.
      IF SY-TCODE(4) EQ 'ZIMP'.
        READ TABLE IT_ZSPMTHST INDEX RS_SELFIELD-TABINDEX. "
        IF SY-SUBRC EQ 0.
          PERFORM  P2000_FI_DOCUMENT_DISPLAY
                                 USING   IT_ZSPMTHST-BUKRS
                                         IT_ZSPMTHST-GJAHR
                                         IT_ZSPMTHST-BELNR.
        ELSE.
          MESSAGE S962.
        ENDIF.

      ELSE.
        READ TABLE IT_ZSIMCOST INDEX RS_SELFIELD-TABINDEX.
        IF SY-SUBRC EQ 0.
          PERFORM  P2000_COST_DOCUMENT_DISPLAY
                                 USING   IT_ZSIMCOST-BUKRS
                                         IT_ZSIMCOST-GJAHR
                                         IT_ZSIMCOST-BELNR.
        ELSE.
          MESSAGE S962.
        ENDIF.
      ENDIF.
      CLEAR R_UCOMM.
  ENDCASE.

ENDFORM.                    " P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_ALV_PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'STANDA02' EXCLUDING EXTAB.

ENDFORM.                    " P2000_ALV_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  P2000_COST_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_COST_DOCUMENT_DISPLAY USING    P_BUKRS
                                          P_GJAHR
                                          P_BELNR.
  IF P_BELNR IS INITIAL.
    MESSAGE S167 WITH 'Import expense Doc'.   EXIT.
  ELSE.
    SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
    SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
    SET PARAMETER ID 'ZPBENR' FIELD P_BELNR.
    CALL TRANSACTION 'ZIMY3' AND SKIP  FIRST SCREEN.
  ENDIF.
ENDFORM.                    " P2000_COST_DOCUMENT_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  P2000_FI_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_FI_DOCUMENT_DISPLAY USING    P_BUKRS
                                        P_GJAHR
                                        P_BELNR.
  IF P_BELNR IS INITIAL.
    MESSAGE S588.   EXIT.
  ELSE.
*>>> LIV 전표번호인지, 회계전표인지를 구분.
    SELECT * FROM  EKBZ UP TO 1 ROWS
             WHERE BELNR EQ P_BELNR
             AND   GJAHR EQ P_GJAHR.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      SELECT * FROM  EKBE UP TO 1 ROWS
               WHERE BELNR EQ P_BELNR
               AND   GJAHR EQ P_GJAHR.
      ENDSELECT.
    ENDIF.
    IF SY-SUBRC EQ 0.
      SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
      SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
      SET PARAMETER ID 'RBN'    FIELD P_BELNR.
      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
    ELSE.
      SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
      SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
      SET PARAMETER ID 'BLN'    FIELD P_BELNR.
      CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_FI_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P3000_CREATE_CC_DATA
*&---------------------------------------------------------------------*
FORM P3000_CREATE_CC_DATA USING    P_BLNO   P_HBLNO.

  SELECT SINGLE *
           FROM ZTIV
          WHERE ZFBLNO  = P_BLNO.

  IF SY-SUBRC NE 0.
    REFRESH : BDCDATA.
    PERFORM P2000_DYNPRO USING :
        'X' 'SAPMZIM01'        '3100',
        ' ' 'ZSREQHD-ZFBLNO'   P_BLNO,
        ' ' 'ZSREQHD-ZFHBLNO'  P_HBLNO,
        ' ' 'ZSIV-ZFCLCD'      'C',
        ' ' 'BDC_OKCODE'       '=ENTR'.

    PERFORM P2000_DYNPRO USING :
        'X' 'SAPMZIM01'        '3110',
        ' ' 'BDC_OKCODE'       '=SAVE'.

    PERFORM P2000_DYNPRO USING :
        'X' 'SAPMZIM01'        '0001',
        ' ' 'BDC_OKCODE'       '=YES'.

    REFRESH : MESSTAB.

    CALL TRANSACTION 'ZIM31'  USING        BDCDATA
                             MODE         'N'            "NCW
                            MESSAGES     INTO   MESSTAB.

    L_SUBRC = SY-SUBRC.
    IF L_SUBRC NE 0.      ">> ERROR 발생시.
      LOOP AT MESSTAB.
        MOVE : MESSTAB-MSGTYP  TO     RETURN-TYPE,
               MESSTAB-MSGID   TO     RETURN-ID,
               MESSTAB-MSGNR   TO     RETURN-NUMBER,
               MESSTAB-MSGV1   TO     RETURN-MESSAGE_V1,
               MESSTAB-MSGV2   TO     RETURN-MESSAGE_V2,
               MESSTAB-MSGV3   TO     RETURN-MESSAGE_V3,
               MESSTAB-MSGV4   TO     RETURN-MESSAGE_V4.

        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
                  MSGID               = RETURN-ID
                  MSGNR               = RETURN-NUMBER
                  MSGV1               = RETURN-MESSAGE_V1
                  MSGV2               = RETURN-MESSAGE_V2
                  MSGV3               = RETURN-MESSAGE_V3
                  MSGV4               = RETURN-MESSAGE_V4
             IMPORTING
                  MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
        APPEND  RETURN.
      ENDLOOP.
      L_SUBRC = 4.
      MESSAGE E977 WITH
              'An error occured during creating entry summary'.
    ELSE.                 ">> SUCCESS 시.
    ENDIF.
  ENDIF.

  "----------------------------------------
  " Customs Declaration Create
  "----------------------------------------
  IF  ZTIDRUS-ZFBLNO IS INITIAL.         "NCW ADD(IF CONDITION)
    SELECT MAX( ZFBLNO ) INTO ZTIDRUS-ZFBLNO
      FROM ZTIDRUS
     WHERE ZFHBLNO EQ ZTIDRUS-ZFHBLNO.
  ENDIF.
  " Customs Clerance Sequence No Input!
  IF  ZTIDRUS-ZFCLSEQ IS INITIAL.
    SELECT MAX( ZFCLSEQ ) INTO ZTIDRUS-ZFCLSEQ
       FROM ZTIDRUS
      WHERE ZFBLNO = ZTIDRUS-ZFBLNO.
  ENDIF.
  SELECT SINGLE *
         FROM  ZTIDRUS
         WHERE ZFBLNO  = ZTIDRUS-ZFBLNO
           AND ZFCLSEQ = ZTIDRUS-ZFCLSEQ.
  IF SY-SUBRC NE 0.
    MESSAGE E753.
  ENDIF.

ENDFORM.                    " P3000_CREATE_CC_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_LISTING
*&---------------------------------------------------------------------*
FORM P2000_DATA_LISTING.

  CASE INCLUDE.
    WHEN 'TRCHANGE' .
      PERFORM P2000_TR_DUP_LIST.
    WHEN 'POPU'.
      PERFORM P2000_MESSAGE_LIST.
    WHEN OTHERS.
      LOOP AT IT_ZSREQHD.
        W_MOD = SY-TABIX MOD 2.
        CASE INCLUDE.
          WHEN 'LGCREATE'.
            PERFORM P2000_CL_CREATE_LIST.
        ENDCASE.
        HIDE : IT_ZSREQHD.
      ENDLOOP.
      CLEAR : IT_ZSREQHD.

  ENDCASE.

ENDFORM.                    " P2000_DATA_LISTING
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_NAMES
*&---------------------------------------------------------------------*
FORM P1000_GET_NAMES.

  CLEAR : WT_BUKRS, WT_PLANT, WT_ZFTRCO.

  " Company Text
  IF NOT ZTTRHD-BUKRS IS INITIAL.
    CLEAR : WT_BUKRS.
    SELECT SINGLE BUTXT INTO WT_BUKRS
           FROM   T001
           WHERE  BUKRS  EQ ZTTRHD-BUKRS
           AND    SPRAS  EQ SY-LANGU.
  ENDIF.

  " Plant Text
  IF NOT ZTTRHD-WERKS IS INITIAL.
    CLEAR : WT_PLANT.
    SELECT SINGLE NAME1 INTO WT_PLANT
           FROM   T001W
           WHERE  WERKS  EQ ZTTRHD-WERKS
           AND    SPRAS  EQ SY-LANGU.
  ENDIF.

  " Trucker
  IF NOT ZTTRHD-ZFTRCO IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ZTTRHD-ZFTRCO
                                CHANGING   WT_ZFTRCO.
  ENDIF.

ENDFORM.                    " P1000_GET_NAMES
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_VENDOR
*&---------------------------------------------------------------------*
FORM P1000_GET_VENDOR USING    P_LIFNR
                      CHANGING P_NAME1.

  DATA : L_TEXT(35).

  CLEAR : P_NAME1, W_LFA1.
  IF P_LIFNR IS INITIAL.
     EXIT.
  ENDIF.

* VENDOR MASTER SELECT( LFA1 )----------------------->
  CALL FUNCTION 'READ_LFA1'
    EXPORTING
      XLIFNR         = P_LIFNR
    IMPORTING
      XLFA1          = W_LFA1
    EXCEPTIONS
      KEY_INCOMPLETE = 01
      NOT_AUTHORIZED = 02
      NOT_FOUND      = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE I025.
    WHEN 02.     MESSAGE E950.
    WHEN 03.     MESSAGE E020   WITH    P_LIFNR.
  ENDCASE.

  MOVE: W_LFA1-NAME1   TO   L_TEXT.
  TRANSLATE L_TEXT TO UPPER CASE.
  P_NAME1 = L_TEXT.

ENDFORM.                    " P1000_GET_VENDOR
*&---------------------------------------------------------------------*
*&      Form  P3000_DELIVERY_ORDER_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_DELIVERY_ORDER_MODIFY.

  IF W_STATUS EQ C_REQ_C.
    PERFORM   P2000_GET_NUMBER_NEXT  USING  'IS'  ZTTRHD-ZFTRNO.
  ENDIF.
  CALL FUNCTION 'ZIM_ZTTR_DOC_MODIFY'
    EXPORTING
      ZFTRNO           = ZTTRHD-ZFTRNO
      ZFSTATUS         = W_STATUS
      W_ZTTRHD_OLD     = *ZTTRHD
      W_ZTTRHD         = ZTTRHD
      W_OK_CODE        = W_OK_CODE
    TABLES
      IT_DOHD          = IT_DOHD
      IT_DOIT          = IT_DOIT
    EXCEPTIONS
      ERROR_UPDATE     = 1.

  W_SUBRC  =  SY-SUBRC.

  IF W_SUBRC  NE  0.
    IF W_STATUS EQ C_REQ_C.
       CLEAR : ZTTRHD-ZFTRNO.
    ENDIF.
    MESSAGE  E952.
  ELSE.
    SET PARAMETER ID 'ZPTRNO' FIELD  ZTTRHD-ZFTRNO.
    IF W_OK_CODE EQ 'DELE'.
       MESSAGE  S756.
    ELSE.
       MESSAGE  S306(ZIM1) WITH  ZTTRHD-ZFTRNO.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_DELIVERY_ORDER_MODIFY

*&---------------------------------------------------------------------*
*&      Form  P2000_GET_NUMBER_NEXT
*&---------------------------------------------------------------------*
FORM P2000_GET_NUMBER_NEXT USING    P_GUBUN
                                    P_DOCNO.

  CALL FUNCTION 'ZIM_NUMBER_GET_NEXT'
    EXPORTING
      ZFREQTY         = P_GUBUN
    IMPORTING
      ZFREQNO         = P_DOCNO
    EXCEPTIONS
      NOT_INPUT       = 1
      NOT_TYPE        = 2
      NOT_RANGE       = 3
      NOT_FOUND       = 4
      LOCKED          = 6
      ERROR_DUPLICATE = 8.

  CASE SY-SUBRC.
    WHEN 1.     MESSAGE    E012.
    WHEN 2.     MESSAGE    E013      WITH  P_GUBUN.
    WHEN 3.     MESSAGE    E014      WITH  P_DOCNO.
    WHEN 4.     MESSAGE    E964.
    WHEN 6.
      MESSAGE    E510      WITH
                   SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    WHEN 8.     MESSAGE    E015      WITH  P_DOCNO.
  ENDCASE.

ENDFORM.                    " P2000_GET_NUMBER_NEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_TR_DOC_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_TR_DOC_LOCK USING   PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTTRHD'
      EXPORTING
        ZFTRNO         = ZTTRHD-ZFTRNO
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.

    CASE SY-SUBRC.
      WHEN  1.
        MESSAGE E510   WITH SY-MSGV1  'Delivery Order'
                            SY-MANDT ZTTRHD-ZFTRNO
                            RAISING DOCUMENT_LOCKED.
      WHEN  2.    MESSAGE E511.
    ENDCASE.

  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTTRHD'
      EXPORTING
        ZFTRNO = ZTTRHD-ZFTRNO.
  ENDIF.

ENDFORM.                    " P2000_SET_TR_DOC_LOCK
*&---------------------------------------------------------------------*
*&      Form  P3000_DEL_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P3000_DEL_DEL_STATUS_CHECK.

  IF ZTTRHD-ZFGIYN NE 'N'.
     MESSAGE E331(ZIM1) WITH ZTTRHD-ZFTRNO.
     EXIT.
  ENDIF.

ENDFORM.                    " P3000_DEL_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_REPORT_CALL
*&---------------------------------------------------------------------*
FORM P2000_REPORT_CALL.

  SUBMIT  ZRIMTRSLSTD AND RETURN.
  IMPORT IT_DOHD_APPEND FROM MEMORY ID 'DO_HD_APPEND'.
  IMPORT IT_DOIT_APPEND FROM MEMORY ID 'DO_IT_APPEND'.

  LOOP AT IT_DOHD_APPEND.
     READ  TABLE IT_DOHD  WITH KEY TRAID = IT_DOHD_APPEND-TRAID.
     IF SY-SUBRC NE 0.
        MOVE-CORRESPONDING  IT_DOHD_APPEND  TO  IT_DOHD.
        APPEND  IT_DOHD.
        LOOP AT IT_DOIT_APPEND  WHERE  TRAID  EQ IT_DOHD_APPEND-TRAID.
           MOVE-CORRESPONDING IT_DOIT_APPEND  TO IT_DOIT.
           APPEND  IT_DOIT.
        ENDLOOP.
     ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_REPORT_CALL
*&---------------------------------------------------------------------*
*&      Form  P2000_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
FORM P2000_DOC_ITEM_SELECT.

  REFRESH IT_ZSTRHD. CLEAR IT_ZSTRHD.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSTRHD
  FROM   ZTTRHD
  FOR    ALL ENTRIES IN IT_ZFTRNO
  WHERE  ZFTRNO      EQ IT_ZFTRNO-ZFTRNO.

*>> position
  DESCRIBE TABLE IT_ZSTRHD LINES TFILL.
  IF TFILL = 0.   MESSAGE E406.   ENDIF.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'TRCHANGE'.
  CALL SCREEN 0014 STARTING AT  08 3
                   ENDING   AT  72 15.






ENDFORM.                    " P2000_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_LIST
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_LIST.
   FORMAT COLOR COL_HEADING INTENSIFIED OFF.
   WRITE : / SY-ULINE(96), /         SY-VLINE NO-GAP,
            'Type'          NO-GAP, SY-VLINE NO-GAP,
            'Message Text',      94 SY-VLINE NO-GAP,
            'T'             NO-GAP, SY-VLINE,
          / SY-ULINE(96).
  LOOP AT IT_ERR_LIST.
     W_MOD  =  SY-TABIX MOD 2.
     FORMAT RESET.
     IF W_MOD EQ 0.
       FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
     ELSE.
       FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
     ENDIF.
     WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4)     NO-GAP,
               SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(87) NO-GAP,
               SY-VLINE NO-GAP.

     CASE IT_ERR_LIST-MSGTYP.
       WHEN 'E'.
         FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
       WHEN 'W'.
         FORMAT COLOR COL_KEY      INTENSIFIED OFF.
       WHEN 'I'.
         FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
       WHEN 'S'.
         FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
     ENDCASE.

     WRITE : IT_ERR_LIST-MSGTYP(1) NO-GAP, SY-VLINE NO-GAP.
     HIDE:IT_ERR_LIST.
   ENDLOOP.
   WRITE : / SY-ULINE(96).
   CLEAR : IT_ERR_LIST.

ENDFORM.                    " P2000_MESSAGE_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_TR_DUP_LIST
*&---------------------------------------------------------------------*
FORM P2000_TR_DUP_LIST.

  LOOP AT IT_ZSTRHD.
    W_MOD = SY-TABIX MOD 2.

    WRITE : / SY-VLINE NO-GAP,
              (10) IT_ZSTRHD-ZFTRNO NO-GAP COLOR COL_KEY INTENSIFIED,
              SY-VLINE NO-GAP.
    IF W_MOD EQ 0.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ENDIF.
    WRITE : (04) IT_ZSTRHD-WERKS     NO-GAP,   SY-VLINE  NO-GAP,
            (10) IT_ZSTRHD-ZFREBELN  NO-GAP,   SY-VLINE  NO-GAP,
            (12) IT_ZSTRHD-ZFSENDER  NO-GAP,   SY-VLINE NO-GAP,
            (08) IT_ZSTRHD-ZFGIDT    NO-GAP,   SY-VLINE NO-GAP,
            (08) IT_ZSTRHD-ZFDRDT    NO-GAP,   SY-VLINE NO-GAP.

    HIDE : IT_ZSTRHD.
  ENDLOOP.
  WRITE : / SY-ULINE.
  CLEAR : IT_ZSTRHD.

ENDFORM.                    " P2000_TR_DUP_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_TR_DOC_READ
*&---------------------------------------------------------------------*
FORM P1000_TR_DOC_READ.

  CLEAR : ZTTRHD, IT_DOHD, IT_DOIT, IT_DOIT_SEL.
  REFRESH : IT_DOHD, IT_DOIT, IT_DOIT_SEL.

  " Delivery Order Header Information.
  SELECT SINGLE * FROM ZTTRHD
  WHERE  ZFTRNO   EQ   IT_ZSTRHD-ZFTRNO.

  " Inbound Delivery Information Get.
  SELECT  * FROM ZTTRITD
  WHERE  ZFTRNO   EQ   IT_ZSTRHD-ZFTRNO.

     CLEAR : IT_DOIT.

     SELECT SINGLE * FROM  ZTTRIT
     WHERE  ZFTRNO   EQ    IT_ZSTRHD-ZFTRNO
     AND    ZFTRIT   EQ    ZTTRITD-ZFTRIT.

     SELECT SINGLE * FROM MAKT
     WHERE  MATNR    EQ   ZTTRIT-MATNR
     AND    SPRAS    EQ   SY-LANGU.
     MOVE  : ZTTRIT-ZFBLNO  TO     IT_DOIT-ZFBLNO,
             MAKT-MAKTX     TO     IT_DOIT-MAKTX.

     SELECT  SINGLE   A~VGBEL  A~VGPOS  B~TRAID  A~KDMAT  A~MATNR
             A~WERKS  A~LFIMG  A~MEINS  B~VBELN
     INTO    (IT_DOIT-VGBEL,   IT_DOIT-VGPOS,  IT_DOIT-TRAID,
              IT_DOIT-KDMAT,   IT_DOIT-MATNR,  IT_DOIT-WERKS,
              IT_DOIT-LFIMG,   IT_DOIT-MEINS,  IT_DOIT-VBELN)
     FROM    LIPS  AS  A  INNER  JOIN  LIKP  AS  B
     ON      A~VBELN      EQ     B~VBELN
     WHERE   A~VBELN      EQ     ZTTRITD-VBELN
     AND     A~VGBEL      EQ     ZTTRIT-EBELN
     AND     A~VGPOS      EQ     ZTTRIT-EBELP.
     APPEND  IT_DOIT.

  ENDSELECT.

  " Container Grouping.
  CLEAR : IT_DOHD, WL_TRAID.
  SORT  IT_DOIT  BY  TRAID.
  LOOP  AT  IT_DOIT.
     IF WL_TRAID  NE  IT_DOIT-TRAID.
        SELECT  SINGLE * FROM ZTBL
        WHERE   ZFBLNO   EQ   IT_DOIT-ZFBLNO.
        MOVE : ZTBL-ZFBLNO    TO  IT_DOHD-ZFBLNO,
               ZTBL-ZFHBLNO   TO  IT_DOHD-ZFHBLNO,
               ZTBL-ZFETD     TO  IT_DOHD-ZFETD,
               ZTBL-ZFETA     TO  IT_DOHD-ZFETA,
               ZTBL-ZFRETA    TO  IT_DOHD-ZFRETA,
               ZTBL-ZFREBELN  TO  IT_DOHD-ZFREBELN,
               IT_DOIT-TRAID  TO  IT_DOHD-TRAID.
        APPEND  IT_DOHD.
        CLEAR : IT_DOHD.
     ENDIF.
     MOVE  IT_DOIT-TRAID  TO  WL_TRAID.
  ENDLOOP.

  READ  TABLE  IT_DOHD  INDEX  1.
  LOOP  AT  IT_DOIT  WHERE  TRAID  EQ  IT_DOHD-TRAID.
     MOVE-CORRESPONDING  IT_DOIT  TO  IT_DOIT_SEL.
     APPEND  IT_DOIT_SEL.
  ENDLOOP.
ENDFORM.                    " P1000_TR_DOC_READ
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
FORM P2000_BL_DOC_ITEM_SELECT.

  W_BLNO    = ZTIDRUS-ZFBLNO.
  W_HBLNO   = ZTIDRUS-ZFHBLNO.

  REFRESH IT_ZSREQHD.

* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZTBL
           WHERE  ZFHBLNO EQ ZTIDRUS-ZFHBLNO
           ORDER  BY ZFBLNO.

  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ELSEIF TFILL = 1.
    READ TABLE IT_ZSREQHD INDEX 1.
    W_BLNO  = IT_ZSREQHD-ZFBLNO.
    W_HBLNO = IT_ZSREQHD-ZFHBLNO.
    ANTWORT = 'Y'.
    EXIT.
  ENDIF.

  W_STATUS_CHK = 'C'.
  INCLUDE = 'LGCREATE'.

  CALL SCREEN 0014 STARTING AT  07 3
                   ENDING   AT  87 15.

ENDFORM.                    " P2000_BL_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_CL_CREATE_LIST
*&---------------------------------------------------------------------*
FORM P2000_CL_CREATE_LIST.

  WRITE : / IT_ZSREQHD-ZFBLNO  NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE :
          IT_ZSREQHD-WAERS       NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFBLAMT CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFCARC      NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFSPRT(12)  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFAPPC      NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFAPRT(12)  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRCVER(10).

ENDFORM.                    " P2000_CL_CREATE_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_SEARCH_FIELD_MOVE
*&---------------------------------------------------------------------*
FORM P2000_SEARCH_FIELD_MOVE.

  ZTIDRUS-ZFBLNO  = W_BLNO.
  ZTIDRUS-ZFHBLNO = W_HBLNO.

ENDFORM.                    " P2000_SEARCH_FIELD_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_IV_DOC_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_IV_DOC_DISPLAY USING    P_ZFIVNO.

  IF P_ZFIVNO IS INITIAL.
    MESSAGE E977 WITH 'Clearance Request No. is blank'.
    EXIT.
  ENDIF.

  SET PARAMETER ID 'ZPIVNO'  FIELD P_ZFIVNO.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD ''.

  CALL TRANSACTION 'ZIM33' AND SKIP  FIRST SCREEN.


ENDFORM.                    " P2000_IV_DOC_DISPLAY
