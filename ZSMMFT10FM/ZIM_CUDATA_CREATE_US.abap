FUNCTION ZIM_CUDATA_CREATE_US.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(W_ZFIVNO) LIKE  ZTIV-ZFIVNO
*"  EXPORTING
*"     REFERENCE(W_ZFBLNO) LIKE  ZTBL-ZFBLNO
*"     REFERENCE(W_ZFCLSEQ) LIKE  ZTIDRUS-ZFCLSEQ
*"  EXCEPTIONS
*"      ERROR_INSERT
*"----------------------------------------------------------------------
  DATA : L_RATE TYPE F.

  TYPES: IMIS_TYPE_C12(18) TYPE   C,
         IMIS_TYPE_C24(18) TYPE   C.

  DATA:  W_TEXT12          TYPE   IMIS_TYPE_C12,
         W_TEXT24          TYPE   IMIS_TYPE_C24.

  CLEAR : ZTIDRUS, ZTIV, ZTBL, ZTIMIMG00, ZTIEPORT, *ZTIEPORT,
          ZTIMIMGTX, W_TOT_CUAMT, W_TOT_MPAMT, W_TOT_HMAMT.
  REFRESH : IT_ZTIDRUSH, IT_ZTIDRUSD.

*>> Import IMG Check
  SELECT  SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFIMPATH  EQ  '3'.
     W_STATUS  =  '3'.
  ELSE.
     W_STATUS  =  '2'.
  ENDIF.

*-----------------------------------------------------------------------
* For Customs Declaration Create( Data Get )
*-----------------------------------------------------------------------

  ">> Customs Clearance Request Table Get.
  SELECT  SINGLE * FROM ZTIV
  WHERE   ZFIVNO   EQ   W_ZFIVNO.

  ">> Customs Clearance Request Item Table Data Get.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_IVIT
  FROM   ZTIVIT
  WHERE  ZFIVNO  EQ  W_ZFIVNO.

  ">> B/L Header Data Get.
  SELECT SINGLE * FROM ZTBL
  WHERE  ZFBLNO   EQ   ZTIV-ZFBLNO.

  ">> Customs Clearance Sequence Get.
  SELECT MAX( ZFCLSEQ ) INTO W_ZFCLSEQ
  FROM   ZTIDRUS
  WHERE  ZFIVNO = W_ZFIVNO.

  IF W_ZFCLSEQ IS INITIAL.
     W_ZFCLSEQ  =  1.
  ELSE.
     W_ZFCLSEQ  =  W_ZFCLSEQ + 1.
  ENDIF.
  SET PARAMETER ID 'ZPIVNO'   FIELD W_ZFIVNO.
  SET PARAMETER ID 'ZPCLSEQ'  FIELD W_ZFCLSEQ.

  ">> Company Firm Name Or Adress Information Get.
  SELECT SINGLE * FROM ZTIMIMGTX
  WHERE  BUKRS    EQ   ZTIV-BUKRS.

  ">> Company Information Get.
  SELECT SINGLE * FROM T001
  WHERE  BUKRS    EQ   ZTIV-BUKRS.

  ">> Arriving Port Code Get
  SELECT SINGLE * FROM ZTIEPORT
  WHERE  LAND1    EQ   T001-LAND1
  AND    PORT     EQ   ZTIV-ZFAPRTC.

  ">> Loading Port Code Get
  SELECT SINGLE * FROM *ZTIEPORT
  WHERE  LAND1    EQ   ZTBL-ZFCARC
  AND    PORT     EQ   ZTBL-ZFSPRTC.

  ">> Harbor Maintenance Fee
  SELECT SINGLE * FROM ZTIMIMG08
  WHERE  ZFCDTY   EQ   '006'
  AND    ZFCD     EQ   '003'.
  MOVE : ZTIMIMG08-ZFMRATE  TO  W_HM_RATE,
         ZTIMIMG08-ZFMNAMT  TO  W_HM_MIN,
         ZTIMIMG08-ZFMXAMT  TO  W_HM_MAX.

  ">> Merchandise Processing Fee
  SELECT SINGLE * FROM ZTIMIMG08
  WHERE  ZFCDTY   EQ   '006'
  AND    ZFCD     EQ   '004'.
  MOVE : ZTIMIMG08-ZFMRATE  TO  W_MP_RATE,
         ZTIMIMG08-ZFMNAMT  TO  W_MP_MIN,
         ZTIMIMG08-ZFMXAMT  TO  W_MP_MAX.

  ">> Tarrif Condition Get.
  SELECT SINGLE * FROM ZTIMIMG08
  WHERE  ZFCDTY   EQ   '006'
  AND    ZFCD     EQ   '001'.
  MOVE   ZTIMIMG08-COND_TYPE   TO   W_KSCHL.
*-----------------------------------------------------------------------
* Customs Declaration Header Data Get.
*-----------------------------------------------------------------------
  ">> Exchange Rate Set.
  IF T001-WAERS  EQ  ZTIV-ZFIVAMC.
     MOVE : 1    TO  ZTIDRUS-ZFEXRT,
            1    TO  ZTIDRUS-FFACT.
  ELSE.
     PERFORM    P2000_GET_EXCHANGE_RATE USING ZTIV-ZFIVAMC
                                              T001-WAERS
                                              ZTIV-ZFCCDT
                                     CHANGING ZTIDRUS-ZFEXRT
                                              ZTIDRUS-FFACT.
  ENDIF.

  ">> Mode of Transportation set.
  IF ZTIV-ZFVIA EQ 'VSL' AND ZTBL-ZFSHTY EQ 'F'.
     MOVE   '11'   TO  ZTIDRUS-ZFTRMET.
  ELSEIF ZTIV-ZFVIA EQ 'VSL' AND ZTBL-ZFSHTY NE 'F'.
     MOVE   '10'   TO  ZTIDRUS-ZFTRMET.
  ELSEIF ZTIV-ZFVIA EQ 'AIR' AND ZTBL-ZFSHTY EQ 'F'.
     MOVE   '41'   TO  ZTIDRUS-ZFTRMET.
  ELSE.
     MOVE   '40'   TO  ZTIDRUS-ZFTRMET.
  ENDIF.

  ">> Entry Type Set.
  IF ZTIV-ZFRPTTY EQ 'F'.
     MOVE  '06'   TO  ZTIDRUS-ZFENTP.
  ELSE.
     MOVE  '01'   TO  ZTIDRUS-ZFENTP.
  ENDIF.

* Customs Clearance Automatic Creation & Confirm.
  IF W_STATUS = '3'.
     MOVE : ZTIV-ZFCCDT   TO  ZTIDRUS-ZFEEDT.     " Estimated entry DT.
  ENDIF.

  MOVE : SY-MANDT            TO  ZTIDRUS-MANDT,     " Client
         ZTIV-BUKRS          TO  ZTIDRUS-BUKRS,     " Company Code
         ZTBL-ZFBLNO         TO  ZTIDRUS-ZFBLNO,    " B/L Doc. No
         W_ZFCLSEQ           TO  ZTIDRUS-ZFCLSEQ,   " C/C Seq.
         ZTIV-ZFIVNO         TO  ZTIDRUS-ZFIVNO,    " Invoice No.
         ZTIV-ZFCUT          TO  ZTIDRUS-ZFCTW,     " Broker
         ZTIMIMGTX-ZFAPPNM   TO  ZTIDRUS-ZFAPNM,    " Firm Name
         ZTIMIMGTX-ZFAPPAD1  TO  ZTIDRUS-ZFAPPAD1,  " Adress1
         ZTIMIMGTX-ZFAPPAD2  TO  ZTIDRUS-ZFAPPAD2,  " Adress2
         T001-WAERS          TO  ZTIDRUS-ZFKRW,     " Local Currency
         ZTBL-ZFAPRTC        TO  ZTIDRUS-ZFENPT,    " Entry Port
         ZTIEPORT-ZFREFCD    TO  ZTIDRUS-ZFENPCD,   " Entry Port Code
         ZTIEPORT-ZFREFCD(2) TO  ZTIDRUS-ZFINRC,    " Customs Code
         ZTBL-ZFAPRTC        TO  ZTIDRUS-ZFAPRTC,   " Unloading Port
         ZTIEPORT-ZFREFCD    TO  ZTIDRUS-ZFAPTCD,   " Port Code
         ZTBL-ZFETA          TO  ZTIDRUS-ZFENDT,    " Import Date
         ZTBL-ZFHBLNO        TO  ZTIDRUS-ZFHBLNO,   " House B/L
         ZTBL-ZFMBLNO        TO  ZTIDRUS-ZFMBLNO,   " Master B/L
         ZTBL-ZFCARNM        TO  ZTIDRUS-ZFCARNM,   " Vessel Name
         ZTBL-ZFCARC         TO  ZTIDRUS-ZFCAC,     " Export Country
         ZTBL-ZFETD          TO  ZTIDRUS-ZFEXPDT,   " Export Date
         ZTBL-INCO1          TO  ZTIDRUS-INCO1,     " Incoterms
         ZTIMIMGTX-ZFAPNO2   TO  ZTIDRUS-ZFIMPNO,   " Import No.
         ZTIMIMGTX-ZFAPNO2   TO  ZTIDRUS-ZFSIGNEE,  " Consignee No
         ZTBL-ZFETA          TO  ZTIDRUS-ZFETA,     " ETA
         ZTBL-ZFCARC         TO  ZTIDRUS-ZFORIG,    " Origin
         ZTBL-ZFSPRTC        TO  ZTIDRUS-ZFSPRTC,   " Loading Port
         *ZTIEPORT-ZFREFCD   TO  ZTIDRUS-ZFSPUS,    " Loading Port Code
         ZTIV-ZFIVAMT        TO  ZTIDRUS-ZFIVAMT,   " Invoice amount
         ZTIV-ZFIVAMC        TO  ZTIDRUS-ZFIVAMC,   " Currency
         ZTIV-ZFKRW          TO  ZTIDRUS-ZFKRW.     " Local Currency

  IF ZTIV-ZFIVAMC  NE  T001-WAERS.
     ZTIDRUS-ZFIVAMK  = ( ZTIDRUS-ZFEXRT / ZTIDRUS-FFACT )
                                         * ZTIV-ZFIVAMT.
  ELSE.
     ZTIDRUS-ZFIVAMK  =  ZTIDRUS-ZFIVAMT.
  ENDIF.

*------------------------------------------------------------------
*>> Customs Declaration HS Data Set.
*------------------------------------------------------------------
  CLEAR : W_ZFRONO, W_ZFCONO, W_STAWN, W_TOT_AMOUNT, W_LOC_AMOUNT,
          W_CUAMT,  W_MPF,    W_HMF.

  SORT IT_IVIT BY STAWN.

  LOOP  AT  IT_IVIT.

     IF SY-TABIX EQ 1.
        MOVE IT_IVIT-STAWN  TO  W_STAWN.
        W_ZFCONO  =  W_ZFCONO  +  1.
     ENDIF.

     IF W_STAWN  NE  IT_IVIT-STAWN.

        " Compute Tarrif Amount, HMF, MPF.
        " Local Amount Compute.
        IF ZTBL-ZFBLAMC  NE  T001-WAERS.
           W_LOC_AMOUNT  = ( ZTIDRUS-ZFEXRT / ZTIDRUS-FFACT )
                                            * W_TOT_AMOUNT.
        ELSE.
           W_LOC_AMOUNT  =  W_TOT_AMOUNT.
        ENDIF.

        PERFORM  P2000_SET_HS_DATA USING  W_LOC_AMOUNT.
        MOVE  IT_IVIT-STAWN   TO  W_STAWN.

        CLEAR : W_TOT_AMOUNT,  W_CUAMT,  W_MPF, W_HMF, IT_ZTIDRUSH,
                W_LOC_AMOUNT,  W_ZFRONO.
     ENDIF.

*-----------------------------------------------------------------------
* Customs Declaration Detail Set.
*-----------------------------------------------------------------------
     W_ZFRONO  =  W_ZFRONO  +  1.
     MOVE-CORRESPONDING  IT_IVIT  TO  IT_ZTIDRUSD.

     ">> Tarrif Rate Get.
     CLEAR : A902, KONP.
     SELECT SINGLE * FROM A902
     WHERE  KSCHL    EQ   W_KSCHL
     AND    STAWN    EQ   IT_IVIT-STAWN.

     SELECT SINGLE * FROM KONP
     WHERE  KNUMH    EQ   A902-KNUMH
     AND    KSCHL    EQ   W_KSCHL.

     IT_ZTIDRUSD-ZFDUTY  =  IT_IVIT-ZFIVAMK
                         * ( ( KONP-KBETR  /  10 ) / 100 ).

     ">> HMF Compute.
     IF ZTBL-ZFVIA EQ 'VSL'.
        IT_ZTIDRUSD-ZFHMAMT  =  IT_IVIT-ZFIVAMK * ( W_HM_RATE / 100 ).
     ENDIF.

     " MPF Compute.
     IT_ZTIDRUSD-ZFMPAMT  =  IT_IVIT-ZFIVAMK * ( W_MP_RATE / 100 ).

     MOVE : SY-MANDT         TO   IT_ZTIDRUSD-MANDT,
            ZTIV-ZFIVNO      TO   IT_ZTIDRUSD-ZFIVNO,
            W_ZFCLSEQ        TO   IT_ZTIDRUSD-ZFCLSEQ,
            W_ZFRONO         TO   IT_ZTIDRUSD-ZFRONO,
            W_ZFCONO         TO   IT_ZTIDRUSD-ZFCONO,
            IT_IVIT-ZFBLNO   TO   IT_ZTIDRUSD-ZFBLNO,
            IT_IVIT-ZFBLIT   TO   IT_ZTIDRUSD-ZFBLIT,
            IT_IVIT-CCMENGE  TO   IT_ZTIDRUSD-ZFQNT,
            IT_IVIT-MEINS    TO   IT_ZTIDRUSD-ZFQNTM,
            IT_IVIT-ZFIVAMT  TO   IT_ZTIDRUSD-ZFAMT,
            IT_IVIT-ZFIVAMC  TO   IT_ZTIDRUSD-ZFCUR,
            IT_IVIT-ZFIVNO   TO   IT_ZTIDRUSD-ZFIVNO,
            IT_IVIT-ZFIVDNO  TO   IT_ZTIDRUSD-ZFIVDNO.
     APPEND IT_ZTIDRUSD.

     " Amount Compute.
     W_TOT_AMOUNT  =  W_TOT_AMOUNT  +  IT_IVIT-ZFIVAMT.

     ADD : IT_ZTIDRUSD-ZFDUTY   TO  W_CUAMT,
           IT_ZTIDRUSD-ZFHMAMT  TO  W_HMF,
           IT_ZTIDRUSD-ZFMPAMT  TO  W_MPF.
  ENDLOOP.

  " Local Amount Compute.
  IF ZTBL-ZFBLAMC  NE  T001-WAERS.
     W_LOC_AMOUNT  = ( ZTIDRUS-ZFEXRT / ZTIDRUS-FFACT ) * W_TOT_AMOUNT.
  ELSE.
     W_LOC_AMOUNT  =  W_TOT_AMOUNT.
  ENDIF.

  PERFORM  P2000_SET_HS_DATA  USING  W_LOC_AMOUNT.

  ">> Total Fee Compute
  IF NOT W_MP_MIN IS INITIAL AND W_TOT_MPAMT LE W_MP_MIN.
     W_TOT_MPAMT =  W_MP_MIN.
  ENDIF.
  IF NOT W_MP_MAX IS INITIAL AND W_TOT_MPAMT GE W_MP_MAX.
     W_TOT_MPAMT = W_MP_MAX.
  ENDIF.

  IF NOT W_HM_MIN IS INITIAL AND W_TOT_HMAMT LE W_HM_MIN.
     W_TOT_HMAMT = W_HM_MIN.
  ENDIF.
  IF NOT W_HM_MAX IS INITIAL AND W_TOT_HMAMT GE W_HM_MAX.
     W_TOT_HMAMT = W_HM_MAX.
  ENDIF.

  ">> Total Tariff, MPF, HMF.
  MOVE : W_TOT_CUAMT   TO   ZTIDRUS-ZFDUTY,
         W_TOT_MPAMT   TO   ZTIDRUS-ZFMPAMT,
         W_TOT_HMAMT   TO   ZTIDRUS-ZFHMAMT,
         T001-WAERS    TO   ZTIDRUS-ZFTOCUR,
         T001-WAERS    TO   ZTIDRUS-ZFKRW.
  ADD  : W_TOT_MPAMT   TO   ZTIDRUS-ZFOTFE,
         W_TOT_HMAMT   TO   ZTIDRUS-ZFOTFE,
         W_TOT_CUAMT   TO   ZTIDRUS-ZFTOFEE,
         W_TOT_MPAMT   TO   ZTIDRUS-ZFTOFEE,
         W_TOT_HMAMT   TO   ZTIDRUS-ZFTOFEE.

  INSERT ZTIDRUS.

  ">> Error Occured -> Rollback.
  IF SY-SUBRC NE 0.  RAISE ERROR_INSERT.  ENDIF.

  INSERT ZTIDRUSH  FROM TABLE IT_ZTIDRUSH.
  IF SY-SUBRC NE 0.  RAISE ERROR_INSERT.  ENDIF.

  INSERT ZTIDRUSD FROM TABLE IT_ZTIDRUSD.
  IF SY-SUBRC NE 0.  RAISE ERROR_INSERT.  ENDIF.

  UPDATE ZTIV
  SET    ZFCUST =  W_STATUS
         UNAM   =  SY-UNAME
         UDAT   =  SY-DATUM
  WHERE  ZFIVNO EQ W_ZFIVNO.

  IF SY-SUBRC NE 0.
     ROLLBACK WORK.
     RAISE ERROR_INSERT.
  ENDIF.

ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_HS_DATA
*&---------------------------------------------------------------------*
FORM P2000_SET_HS_DATA USING    W_LOC_AMOUNT.

  ">> Trade Goods Name
  SELECT  MAX( TEXT1 )  INTO IT_ZTIDRUSH-ZFGDNM
  FROM    T604T
  WHERE   SPRAS         EQ   SY-LANGU
  AND     STAWN         EQ   W_STAWN.

  MOVE :  SY-MANDT        TO   IT_ZTIDRUSH-MANDT,
          ZTBL-ZFBLNO     TO   IT_ZTIDRUSH-ZFBLNO,
          ZTIV-ZFIVNO     TO   IT_ZTIDRUSH-ZFIVNO,
          ZTIDRUS-ZFCLSEQ TO   IT_ZTIDRUSH-ZFCLSEQ,
          W_ZFCONO        TO   IT_ZTIDRUSH-ZFCONO,
          W_STAWN         TO   IT_ZTIDRUSH-STAWN,
          W_TOT_AMOUNT    TO   IT_ZTIDRUSH-ZFHSAM,
          ZTIV-ZFIVAMC    TO   IT_ZTIDRUSH-WAERS,
          T001-WAERS      TO   IT_ZTIDRUSH-ZFKRW,
          W_CUAMT         TO   IT_ZTIDRUSH-ZFCUAMT,
          W_MPF           TO   IT_ZTIDRUSH-ZFMPAMT,
          W_HMF           TO   IT_ZTIDRUSH-ZFHMAMT,
          W_MP_RATE       TO   IT_ZTIDRUSH-ZFMPRT,
          W_HM_RATE       TO   IT_ZTIDRUSH-ZFHMRT,
          W_TOT_AMOUNT    TO   IT_ZTIDRUSH-ZFTBAU,
          W_LOC_AMOUNT    TO   IT_ZTIDRUSH-ZFTBAK.
  APPEND  IT_ZTIDRUSH.

  ADD :  W_CUAMT          TO   W_TOT_CUAMT,
         W_MPF            TO   W_TOT_MPAMT,
         W_HMF            TO   W_TOT_HMAMT.
  W_ZFCONO  =  W_ZFCONO   +  1.

ENDFORM.                    " P2000_SET_HS_DATA
