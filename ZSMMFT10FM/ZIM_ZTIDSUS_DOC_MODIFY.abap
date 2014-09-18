FUNCTION ZIM_ZTIDSUS_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFIVNO) LIKE  ZTIDSUS-ZFIVNO
*"     VALUE(ZFCLSEQ) LIKE  ZTIDSUS-ZFCLSEQ
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTIDSUS_OLD) LIKE  ZTIDSUS STRUCTURE  ZTIDSUS
*"     VALUE(W_ZTIDSUS) LIKE  ZTIDSUS STRUCTURE  ZTIDSUS
*"     VALUE(W_OK_CODE)
*"     VALUE(W_EDI) TYPE  C DEFAULT SPACE
*"  TABLES
*"      IT_ZSIDSUSH_OLD STRUCTURE  ZSIDSUSH OPTIONAL
*"      IT_ZSIDSUSH STRUCTURE  ZSIDSUSH
*"      IT_ZSIDSUSD_OLD STRUCTURE  ZSIDSUSD OPTIONAL
*"      IT_ZSIDSUSD STRUCTURE  ZSIDSUSD
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"      ERROR_DELETE
*"      ERROR_INSERT
*"      ERROR_INSERT_HD
*"      ERROR_INSERT_HS
*"      ERROR_INSERT_MT
*"      ERROR_COST
*"----------------------------------------------------------------------
TABLES  *ZTIMIMG08.

DATA : W_DATE        LIKE   SY-DATUM,
       L_DATE        LIKE   SY-DATUM,
       L_YEAR(4),
       L_MONTH(2),
       W_VENDOR      LIKE   LFA1-LIFNR,
       L_SUBRC       LIKE   SY-SUBRC,
       W_ZFBTSEQ     LIKE   ZTBLOUR-ZFBTSEQ,
       W_ZFIVAMT_EX  LIKE   BAPICURR-BAPICURR,
       W_ZFIVAMT_IN  LIKE   BAPICURR-BAPICURR,
       DIGITS        TYPE   I   VALUE  20.

   " Import IMG Data Get!
   CLEAR : ZTIMIMG00.
   SELECT SINGLE * FROM ZTIMIMG00.

   MOVE-CORRESPONDING  W_ZTIDSUS  TO  ZTIDSUS.

   MOVE : ZFIVNO      TO     ZTIDSUS-ZFIVNO,
          ZFCLSEQ     TO     ZTIDSUS-ZFCLSEQ,
          SY-MANDT    TO     ZTIDSUS-MANDT,
          SY-UNAME    TO     ZTIDSUS-UNAM,
          SY-DATUM    TO     ZTIDSUS-UDAT.

   " B/L Information Get!
   SELECT SINGLE * FROM  ZTBL WHERE ZFBLNO = ZTIDSUS-ZFBLNO.

   IF W_OK_CODE EQ 'DELE'.
      ZFSTATUS = 'X'.
   ENDIF.

   CASE ZFSTATUS.
      WHEN 'C'.
         MOVE : SY-UNAME      TO    ZTIDSUS-CNAM,
                SY-DATUM      TO    ZTIDSUS-CDAT.

         INSERT   ZTIDSUS.
         IF SY-SUBRC NE 0.
            ROLLBACK WORK.
            RAISE ERROR_INSERT_HD.
         ENDIF.

         DELETE FROM ZTIDSUSH WHERE ZFIVNO  =  ZFIVNO
                              AND   ZFCLSEQ =  ZFCLSEQ.

         DELETE FROM ZTIDSUSD WHERE ZFIVNO  =  ZFIVNO
                              AND   ZFCLSEQ =  ZFCLSEQ.

         LOOP AT IT_ZSIDSUSH.
            CLEAR : ZTIDSUSH.
            MOVE-CORRESPONDING IT_ZSIDSUSH TO ZTIDSUSH.
            MOVE : ZFIVNO                  TO ZTIDSUSH-ZFIVNO,
                   ZFCLSEQ                 TO ZTIDSUSH-ZFCLSEQ,
                   SY-MANDT                TO ZTIDSUSH-MANDT.

            INSERT   ZTIDSUSH.
            " If Error Occured Then RollBack.
            IF SY-SUBRC NE 0.
               ROLLBACK WORK.
               RAISE ERROR_INSERT_HS.
            ENDIF.
         ENDLOOP.

         LOOP AT IT_ZSIDSUSD.
            CLEAR : ZTIDSUSD.
            MOVE-CORRESPONDING IT_ZSIDSUSD TO ZTIDSUSD.
            MOVE : ZFIVNO                 TO ZTIDSUSD-ZFIVNO,
                   ZFCLSEQ                TO ZTIDSUSD-ZFCLSEQ,
                   SY-MANDT               TO ZTIDSUSD-MANDT.

            INSERT   ZTIDSUSD.
            IF SY-SUBRC NE 0.
               ROLLBACK WORK.
               RAISE ERROR_INSERT_MT.
            ENDIF.
         ENDLOOP.
*----------------------------------------------------------------------
*>>> Charge Document Interface Internal Table
*----------------------------------------------------------------------
         SELECT SINGLE * FROM ZTIMIMG00.

         SELECT SINGLE * FROM ZTIV
         WHERE  ZFIVNO   EQ   ZTIDSUS-ZFIVNO.

         IF ZTIMIMG00-APOWNER EQ 'B'.
            IF ZTIDSUS-ZFCTW IS INITIAL.
               COMMIT WORK.
               RAISE ERROR_COST.
               EXIT.
            ELSE.
               SELECT SINGLE * FROM  ZTIMIMG10
                               WHERE ZFCUT  EQ ZTIDSUS-ZFCTW.
               IF SY-SUBRC NE 0.
                  COMMIT WORK.
                  RAISE ERROR_COST.
                  EXIT.
               ELSE.
                  MOVE  ZTIMIMG10-ZFVEN  TO  W_VENDOR.
               ENDIF.
            ENDIF.
         ELSE.
            IF ZTIDSUS-ZFINRC IS INITIAL.
               COMMIT WORK.
               RAISE ERROR_COST.
               EXIT.
            ELSE.
               SELECT SINGLE * FROM  ZTIMIMG02
                               WHERE ZFCOTM   EQ ZTIDSUS-ZFINRC.
               IF SY-SUBRC NE 0.
                  COMMIT WORK.
                  RAISE ERROR_COST.
                  EXIT.
               ELSE.
                  MOVE  ZTIMIMG02-ZFVEN  TO  W_VENDOR.
               ENDIF.
            ENDIF.

         ENDIF.

         CLEAR : T001W.
         IF NOT ZTBL-ZFWERKS IS INITIAL.
            SELECT SINGLE * FROM T001W
                            WHERE WERKS EQ ZTBL-ZFWERKS.
         ELSE.
            SELECT SINGLE * FROM T001W
                            WHERE WERKS EQ
                          ( SELECT WERKS FROM ZTIVIT
                                   WHERE ZFIVNO   EQ  ZTIV-ZFIVNO
                                   AND   ZFIVDNO  EQ  '00010' ).
         ENDIF.

         SELECT SINGLE * FROM T001
                         WHERE BUKRS EQ ZTBL-BUKRS.

         " Customs Select.
         SELECT SINGLE * FROM VF_KRED WHERE LIFNR = W_VENDOR
                                      AND   BUKRS = ZTBL-BUKRS.
         L_SUBRC  =  SY-SUBRC.

         IF L_SUBRC NE 0.
            MESSAGE I205(ZIM1) WITH W_VENDOR ZTBL-BUKRS.
            RAISE ERROR_COST.
         ENDIF.
*<--------------------------------  Duty ----------- ----------------->*
         IF L_SUBRC EQ 0.
            CLEAR : ZTBKPF, *ZTBKPF, IT_ZSBSEG.

            SELECT SINGLE * INTO *ZTIMIMG08 FROM ZTIMIMG08
                   WHERE  ZFCDTY   =   '006'
                   AND    ZFCD     =   '001'.

            MOVE : ZTBL-BUKRS        TO   ZTBKPF-BUKRS,
                   W_VENDOR          TO   ZTBKPF-LIFNR,
                   W_VENDOR          TO   ZTBKPF-ZFVEN,
                   ZTIDSUS-ZFEDT     TO   ZTBKPF-BLDAT,
                   ZTIDSUS-ZFEDT     TO   ZTBKPF-BUDAT,
                   ZTIDSUS-ZFSUMDT   TO   ZTBKPF-ZFBDT,
                   ZTBKPF-BLDAT(4)   TO   ZTBKPF-GJAHR,
                   ZTBKPF-BLDAT+4(2) TO   ZTBKPF-MONAT,
                   VF_KRED-AKONT     TO   ZTBKPF-AKONT,
                   VF_KRED-ZTERM     TO   ZTBKPF-ZTERM,
                  *ZTIMIMG08-BLART   TO   ZTBKPF-BLART,
                  *ZTIMIMG08-ZFCD5   TO   ZTBKPF-MWSKZ,
                   T001-WAERS        TO   ZTBKPF-WAERS,
                   T001-WAERS        TO   ZTBKPF-HWAER,
                   ZTIDSUS-ZFDUTY    TO   ZTBKPF-WRBTR,
                   ZTIDSUS-ZFDUTY    TO   ZTBKPF-DMBTR,
                   'X'               TO   ZTBKPF-ZFPCUR,
                   'N'               TO   ZTBKPF-ZFPOSYN,
                  *ZTIMIMG08-ZFCDNM  TO   ZTBKPF-BKTXT,
                   ZTIDSUS-ZFENTNO   TO   ZTBKPF-XBLNR,
                   '006'             TO   ZTBKPF-ZFCSTGRP,
                   ZTIV-ZFIVNO       TO   ZTBKPF-ZFIMDNO,
                   'X'               TO   ZTBKPF-ZFATPT,
                   'X'               TO   ZTBKPF-ZFAUTO,
                   ZTBL-ZFPOYN       TO   ZTBKPF-ZFPOYN.

            IF ZTIV-ZFPOYN EQ 'N'.
               CLEAR : ZTBKPF-ZFDCSTX.
            ELSE.
               ZTBKPF-ZFDCSTX = 'X'.
            ENDIF.

            REFRESH : IT_ZSBSEG.

            MOVE : '006'                TO IT_ZSBSEG-ZFCSTGRP,
                   '001'                TO IT_ZSBSEG-ZFCD,
                   ZTIV-ZFIVNO          TO IT_ZSBSEG-ZFIMDNO,
                   ZTIDSUS-ZFDUTY       TO IT_ZSBSEG-WRBTR,
                   ZTIDSUS-ZFDUTY       TO IT_ZSBSEG-DMBTR,
                  *ZTIMIMG08-ZFCDNM     TO IT_ZSBSEG-SGTXT,
                   ZTBKPF-ZFDCSTX       TO IT_ZSBSEG-ZFDCSTX,
                   '40'                 TO IT_ZSBSEG-NEWBS,
                   'S'                  TO IT_ZSBSEG-SHKZG,
                   0                    TO IT_ZSBSEG-FWBAS,
                 *ZTIMIMG08-COND_TYPE   TO IT_ZSBSEG-COND_TYPE,
                  ZTIDSUS-ZFENTNO       TO IT_ZSBSEG-ZUONR,
                  IT_ZSBSEG-ZFPOYN      TO ZTBKPF-ZFPOYN.

            PERFORM P1000_IMPORT_DOC_CHEKC   USING IT_ZSBSEG-ZFIMDNO
                                                   IT_ZSBSEG-ZFDCNM
                                                   IT_ZSBSEG-ZFPOYN
                                                   'I'
                                                   IT_ZSBSEG-KOSTL
                                                   ZTBKPF-GSBER
                                                   ZTBKPF-BUPLA
                                                   W_EDI.
            ">> Cost account No. decision
            CALL FUNCTION 'ZIM_GET_NODRAFT_ACCOUNT'
                  EXPORTING
                     ZFCSTGRP   =     IT_ZSBSEG-ZFCSTGRP
                     ZFCD       =     IT_ZSBSEG-ZFCD
                     ZFIMDNO    =     IT_ZSBSEG-ZFIMDNO
                  IMPORTING
                     NEWKO      =     IT_ZSBSEG-NEWKO.

            APPEND IT_ZSBSEG.

            ">> Create -> Charge Document create
            IF W_SUBRC EQ 0 OR
               ( W_SUBRC NE 0 AND W_EDI EQ 'X' ).
               CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
                    EXPORTING
                        W_OK_CODE           =   'SAVE'
                        BUKRS               =   ZTBL-BUKRS
                        GJAHR               =   ZTBKPF-GJAHR
                        ZFSTATUS            =   'C'
                        W_ZTBKPF_OLD        =  *ZTBKPF
                        W_ZTBKPF            =   ZTBKPF
                   TABLES
                        IT_ZSBSEG           =   IT_ZSBSEG
                   CHANGING
                        BELNR               =   ZTBKPF-BELNR
                   EXCEPTIONS
                       ERROR_UPDATE.
            ENDIF.
         ENDIF.
*<----------------------  Duty Creation End -------------------------->*
*
**<--------------------  Harbor Maintenance Fee --------------------->*
*         CLEAR : W_HMF_AMT.
*         IF ZTIDSUS-ZFHMAMT GT 0 AND L_SUBRC EQ 0
*                                 AND ZTBL-ZFVIA EQ 'VSL'.
*            REFRESH : IT_ZSBSEG.
*            CLEAR : ZTBKPF, *ZTBKPF, IT_ZSBSEG.
*
*            SELECT SINGLE * INTO *ZTIMIMG08 FROM ZTIMIMG08
*                   WHERE  ZFCDTY   =   '006'
*                   AND    ZFCD     =   '003'.
*
*            MOVE : ZTBL-BUKRS        TO   ZTBKPF-BUKRS,
*                   ZTIMIMG10-ZFVEN   TO   ZTBKPF-LIFNR,
*                   ZTIMIMG10-ZFVEN   TO   ZTBKPF-ZFVEN,
*                   ZTIDSUS-ZFEDT     TO   ZTBKPF-BLDAT,
*                   ZTIDSUS-ZFEDT     TO   ZTBKPF-BUDAT,
*                   ZTIDSUS-ZFSUMDT   TO   ZTBKPF-ZFBDT,
*                   ZTBKPF-BLDAT(4)   TO   ZTBKPF-GJAHR,
*                   ZTBKPF-BLDAT+4(2) TO   ZTBKPF-MONAT,
*                  *ZTIMIMG08-BLART   TO   ZTBKPF-BLART,
*                   VF_KRED-AKONT     TO   ZTBKPF-AKONT,
*                   VF_KRED-ZTERM     TO   ZTBKPF-ZTERM,
*                  *ZTIMIMG08-ZFCD5   TO   ZTBKPF-MWSKZ,
*                   T001-WAERS        TO   ZTBKPF-WAERS,
*                   T001-WAERS        TO   ZTBKPF-HWAER,
*                   ZTIDSUS-ZFHMAMT   TO   ZTBKPF-WRBTR,
*                   ZTIDSUS-ZFHMAMT   TO   ZTBKPF-DMBTR,
*                   'X'               TO   ZTBKPF-ZFPCUR,
*                   'N'               TO   ZTBKPF-ZFPOSYN,
*                  *ZTIMIMG08-ZFCDNM  TO   ZTBKPF-BKTXT,
*                   ZTIDSUS-ZFENTNO   TO   ZTBKPF-XBLNR,
*                   '006'             TO   ZTBKPF-ZFCSTGRP,
*                   ZTIV-ZFIVNO       TO   ZTBKPF-ZFIMDNO,
*                   'X'               TO   ZTBKPF-ZFATPT,
*                   'X'               TO   ZTBKPF-ZFAUTO,
*                   ZTBL-ZFPOYN       TO   ZTBKPF-ZFPOYN.
*
*            REFRESH : IT_ZSBSEG.
*
*            MOVE : '006'                TO IT_ZSBSEG-ZFCSTGRP,
*                   '003'                TO IT_ZSBSEG-ZFCD,
*                   ZTIV-ZFIVNO          TO IT_ZSBSEG-ZFIMDNO,
*                   ZTIDSUS-ZFHMAMT      TO IT_ZSBSEG-WRBTR,
*                   ZTIDSUS-ZFHMAMT      TO IT_ZSBSEG-DMBTR,
*                  *ZTIMIMG08-ZFCD5      TO IT_ZSBSEG-MWSKZ,
*                  *ZTIMIMG08-ZFCDNM     TO IT_ZSBSEG-SGTXT,
*                   ZTBKPF-ZFDCSTX       TO IT_ZSBSEG-ZFDCSTX,
*                   '40'                 TO IT_ZSBSEG-NEWBS,
*                   'S'                  TO IT_ZSBSEG-SHKZG,
*                   0                    TO IT_ZSBSEG-FWBAS,
*                 *ZTIMIMG08-COND_TYPE   TO IT_ZSBSEG-COND_TYPE,
*                  ZTIDSUS-ZFENTNO       TO IT_ZSBSEG-ZUONR,
*                  IT_ZSBSEG-ZFPOYN      TO ZTBKPF-ZFPOYN.
*
*            PERFORM P1000_IMPORT_DOC_CHEKC   USING IT_ZSBSEG-ZFIMDNO
*                                                   IT_ZSBSEG-ZFDCNM
*                                                   IT_ZSBSEG-ZFPOYN
*                                                   'I'
*                                                   IT_ZSBSEG-KOSTL
*                                                   ZTBKPF-GSBER
*                                                   ZTBKPF-BUPLA
*                                                   W_EDI.
*            ">> Cost Account No. decision
*            CALL FUNCTION 'ZIM_GET_NODRAFT_ACCOUNT'
*                  EXPORTING
*                     ZFCSTGRP   =     IT_ZSBSEG-ZFCSTGRP
*                     ZFCD       =     IT_ZSBSEG-ZFCD
*                     ZFIMDNO    =     IT_ZSBSEG-ZFIMDNO
*                  IMPORTING
*                     NEWKO      =     IT_ZSBSEG-NEWKO.
*
*            APPEND IT_ZSBSEG.
*
*            ">> Create -> Charge Document create
*            IF W_SUBRC EQ 0 OR
*               ( W_SUBRC NE 0 AND W_EDI EQ 'X' ).
*               CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
*                    EXPORTING
*                        W_OK_CODE           =   'SAVE'
*                        BUKRS               =   ZTBL-BUKRS
*                        GJAHR               =   ZTBKPF-GJAHR
*                        ZFSTATUS            =   'C'
*                        W_ZTBKPF_OLD        =  *ZTBKPF
*                        W_ZTBKPF            =   ZTBKPF
*                   TABLES
*                        IT_ZSBSEG           =   IT_ZSBSEG
*                   CHANGING
*                        BELNR               =   ZTBKPF-BELNR
*                   EXCEPTIONS
*                       ERROR_UPDATE.
*            ENDIF.
*         ENDIF.
**<--------------- Harbor Maintenance Fee Creation End -------------->*

*<----------------- Merchandise Processing Fee Creation -------------->*
         CLEAR : W_MPF_AMT.
         IF ZTIDSUS-ZFMPAMT GT 0 AND L_SUBRC EQ 0.
            REFRESH : IT_ZSBSEG.
            CLEAR : ZTBKPF, *ZTBKPF, IT_ZSBSEG.

            SELECT SINGLE * INTO *ZTIMIMG08 FROM ZTIMIMG08
                   WHERE  ZFCDTY   =   '006'
                   AND    ZFCD     =   '004'.

            MOVE : ZTBL-BUKRS        TO   ZTBKPF-BUKRS,
                   W_VENDOR          TO   ZTBKPF-LIFNR,
                   W_VENDOR          TO   ZTBKPF-ZFVEN,
                   ZTIDSUS-ZFEDT     TO   ZTBKPF-BLDAT,
                   ZTIDSUS-ZFEDT     TO   ZTBKPF-BUDAT,
                   ZTIDSUS-ZFSUMDT   TO   ZTBKPF-ZFBDT,
                   ZTBKPF-BLDAT(4)   TO   ZTBKPF-GJAHR,
                   ZTBKPF-BLDAT+4(2) TO   ZTBKPF-MONAT,
                   VF_KRED-AKONT     TO   ZTBKPF-AKONT,
                   VF_KRED-ZTERM     TO   ZTBKPF-ZTERM,
                  *ZTIMIMG08-BLART   TO   ZTBKPF-BLART,
                  *ZTIMIMG08-ZFCD5   TO   ZTBKPF-MWSKZ,
                   T001-WAERS        TO   ZTBKPF-WAERS,
                   T001-WAERS        TO   ZTBKPF-HWAER,
                   ZTIDSUS-ZFMPAMT   TO   ZTBKPF-WRBTR,
                   ZTIDSUS-ZFMPAMT   TO   ZTBKPF-DMBTR,
                   'X'               TO   ZTBKPF-ZFPCUR,
                   'N'               TO   ZTBKPF-ZFPOSYN,
                  *ZTIMIMG08-ZFCDNM  TO   ZTBKPF-BKTXT,
                   ZTIDSUS-ZFENTNO   TO   ZTBKPF-XBLNR,
                   '006'             TO   ZTBKPF-ZFCSTGRP,
                   ZTIV-ZFIVNO       TO   ZTBKPF-ZFIMDNO,
                   'X'               TO   ZTBKPF-ZFATPT,
                   'X'               TO   ZTBKPF-ZFAUTO,
                   ZTBL-ZFPOYN       TO   ZTBKPF-ZFPOYN.

            REFRESH : IT_ZSBSEG.

            MOVE : '006'                TO IT_ZSBSEG-ZFCSTGRP,
                   '004'                TO IT_ZSBSEG-ZFCD,
                   ZTIV-ZFIVNO          TO IT_ZSBSEG-ZFIMDNO,
                   ZTIDSUS-ZFMPAMT      TO IT_ZSBSEG-WRBTR,
                   ZTIDSUS-ZFMPAMT      TO IT_ZSBSEG-DMBTR,
                  *ZTIMIMG08-ZFCD5      TO IT_ZSBSEG-MWSKZ,
                  *ZTIMIMG08-ZFCDNM     TO IT_ZSBSEG-SGTXT,
                   ZTBKPF-ZFDCSTX       TO IT_ZSBSEG-ZFDCSTX,
                   '40'                 TO IT_ZSBSEG-NEWBS,
                   'S'                  TO IT_ZSBSEG-SHKZG,
                   0                    TO IT_ZSBSEG-FWBAS,
                 *ZTIMIMG08-COND_TYPE   TO IT_ZSBSEG-COND_TYPE,
                  ZTIDSUS-ZFENTNO       TO IT_ZSBSEG-ZUONR,
                  IT_ZSBSEG-ZFPOYN      TO ZTBKPF-ZFPOYN.

            PERFORM P1000_IMPORT_DOC_CHEKC   USING IT_ZSBSEG-ZFIMDNO
                                                   IT_ZSBSEG-ZFDCNM
                                                   IT_ZSBSEG-ZFPOYN
                                                   'I'
                                                   IT_ZSBSEG-KOSTL
                                                   ZTBKPF-GSBER
                                                   ZTBKPF-BUPLA
                                                   W_EDI.
            ">> Cost Account No. decision Function call.
            CALL FUNCTION 'ZIM_GET_NODRAFT_ACCOUNT'
                  EXPORTING
                     ZFCSTGRP   =     IT_ZSBSEG-ZFCSTGRP
                     ZFCD       =     IT_ZSBSEG-ZFCD
                     ZFIMDNO    =     IT_ZSBSEG-ZFIMDNO
                  IMPORTING
                     NEWKO      =     IT_ZSBSEG-NEWKO.

            APPEND IT_ZSBSEG.

            ">> Create -> Charge Document Create.
            IF W_SUBRC EQ 0 OR
               ( W_SUBRC NE 0 AND W_EDI EQ 'X' ).
               CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
                    EXPORTING
                        W_OK_CODE           =   'SAVE'
                        BUKRS               =   ZTBL-BUKRS
                        GJAHR               =   ZTBKPF-GJAHR
                        ZFSTATUS            =   'C'
                        W_ZTBKPF_OLD        =  *ZTBKPF
                        W_ZTBKPF            =   ZTBKPF
                   TABLES
                        IT_ZSBSEG           =   IT_ZSBSEG
                   CHANGING
                        BELNR               =   ZTBKPF-BELNR
                   EXCEPTIONS
                       ERROR_UPDATE.
            ENDIF.
         ENDIF.
*<----------------- Harbor Maintenance Fee Creation End -------------->*

*<-------------------------- Broker Fee ----------------------------->*
         " Customs Broker Fee Compute.
         IF ZTIMIMG00-ZFCUMTD EQ '1'.
            CALL FUNCTION 'ZIM_CC_TAX_CALCULATE_US'
                    IMPORTING
                       W_BROKER_FEE =  W_BROKER_FEE
                    CHANGING
                       ZTIDSUS =  ZTIDSUS.
         ENDIF.

         IF W_BROKER_FEE GT 0.
            CLEAR : ZTBKPF, *ZTBKPF, IT_ZSBSEG.

            IF ZTIMIMG00-APOWNER NE 'B'.
               IF ZTIDSUS-ZFCTW IS INITIAL.
                  COMMIT WORK.
                  RAISE ERROR_COST.
                  EXIT.
               ELSE.
                  SELECT SINGLE * FROM  ZTIMIMG10
                                  WHERE ZFCUT  EQ ZTIDSUS-ZFCTW.
                  IF SY-SUBRC NE 0.
                     COMMIT WORK.
                     RAISE ERROR_COST.
                     EXIT.
                  ELSE.
                     MOVE  ZTIMIMG10-ZFVEN  TO  W_VENDOR.
                  ENDIF.
               ENDIF.
               " Customs Select.
               CLEAR : VF_KRED.
               SELECT SINGLE * FROM VF_KRED
               WHERE  LIFNR = ZTIMIMG10-ZFVEN
               AND    BUKRS = ZTIDSUS-BUKRS.
               L_SUBRC  =  SY-SUBRC.

               IF L_SUBRC NE 0.
                  MESSAGE I205(ZIM1) WITH W_VENDOR ZTBL-BUKRS.
                  RAISE ERROR_COST.
               ENDIF.

            ENDIF.

            SELECT SINGLE * INTO *ZTIMIMG08 FROM ZTIMIMG08
                   WHERE  ZFCDTY   =   '006'
                   AND    ZFCD     =   '002'.

            MOVE : ZTBL-BUKRS        TO   ZTBKPF-BUKRS,
                   ZTIMIMG10-ZFVEN   TO   ZTBKPF-LIFNR,
                   ZTIMIMG10-ZFVEN   TO   ZTBKPF-ZFVEN,
                   ZTIDSUS-ZFEDT     TO   ZTBKPF-BLDAT,
                   ZTIDSUS-ZFEDT     TO   ZTBKPF-BUDAT,
                   ZTIDSUS-ZFSUMDT   TO   ZTBKPF-ZFBDT,
                   ZTBKPF-BLDAT(4)   TO   ZTBKPF-GJAHR,
                   ZTBKPF-BLDAT+4(2) TO   ZTBKPF-MONAT,
                   VF_KRED-AKONT     TO   ZTBKPF-AKONT,
                   VF_KRED-ZTERM     TO   ZTBKPF-ZTERM,
                  *ZTIMIMG08-BLART   TO   ZTBKPF-BLART,
                  *ZTIMIMG08-ZFCD5   TO   ZTBKPF-MWSKZ,
                   T001-WAERS        TO   ZTBKPF-WAERS,
                   T001-WAERS        TO   ZTBKPF-HWAER,
                   W_BROKER_FEE      TO   ZTBKPF-WRBTR,
                   W_BROKER_FEE      TO   ZTBKPF-DMBTR,
                   'X'               TO   ZTBKPF-ZFPCUR,
                   'N'               TO   ZTBKPF-ZFPOSYN,
                  *ZTIMIMG08-ZFCDNM  TO   ZTBKPF-BKTXT,
                   ZTIDSUS-ZFENTNO   TO   ZTBKPF-XBLNR,
                   '006'             TO   ZTBKPF-ZFCSTGRP,
                   ZTIV-ZFIVNO       TO   ZTBKPF-ZFIMDNO,
                   'X'               TO   ZTBKPF-ZFATPT,
                   'X'               TO   ZTBKPF-ZFAUTO,
                   ZTBL-ZFPOYN       TO   ZTBKPF-ZFPOYN.

            IF ZTIV-ZFPOYN EQ 'N'.
               CLEAR : ZTBKPF-ZFDCSTX.
            ELSE.
               IF *ZTIMIMG08-ZFCD1 EQ 'Y'.
                  ZTBKPF-ZFDCSTX = 'X'.
               ELSE.
                  CLEAR : ZTBKPF-ZFDCSTX.
               ENDIF.
            ENDIF.

            REFRESH : IT_ZSBSEG.

            MOVE : '006'                TO IT_ZSBSEG-ZFCSTGRP,
                   '002'                TO IT_ZSBSEG-ZFCD,
                   ZTIV-ZFIVNO          TO IT_ZSBSEG-ZFIMDNO,
                   W_BROKER_FEE         TO IT_ZSBSEG-WRBTR,
                   W_BROKER_FEE         TO IT_ZSBSEG-DMBTR,
                  *ZTIMIMG08-ZFCD5      TO IT_ZSBSEG-MWSKZ,
                  *ZTIMIMG08-ZFCDNM     TO IT_ZSBSEG-SGTXT,
                   ZTBKPF-ZFDCSTX       TO IT_ZSBSEG-ZFDCSTX,
                   '40'                 TO IT_ZSBSEG-NEWBS,
                   'S'                  TO IT_ZSBSEG-SHKZG,
                    0                   TO IT_ZSBSEG-FWBAS,
                 *ZTIMIMG08-COND_TYPE   TO IT_ZSBSEG-COND_TYPE,
                  ZTIDSUS-ZFENTNO       TO IT_ZSBSEG-ZUONR,
                  IT_ZSBSEG-ZFPOYN      TO ZTBKPF-ZFPOYN.

            PERFORM P1000_IMPORT_DOC_CHEKC   USING IT_ZSBSEG-ZFIMDNO
                                                   IT_ZSBSEG-ZFDCNM
                                                   IT_ZSBSEG-ZFPOYN
                                                   'I'
                                                   IT_ZSBSEG-KOSTL
                                                   ZTBKPF-GSBER
                                                   ZTBKPF-BUPLA
                                                   W_EDI.
            ">> Cost account No. decision
            CALL FUNCTION 'ZIM_GET_NODRAFT_ACCOUNT'
                  EXPORTING
                     ZFCSTGRP   =     IT_ZSBSEG-ZFCSTGRP
                     ZFCD       =     IT_ZSBSEG-ZFCD
                     ZFIMDNO    =     IT_ZSBSEG-ZFIMDNO
                  IMPORTING
                     NEWKO      =     IT_ZSBSEG-NEWKO.

            APPEND IT_ZSBSEG.

            ">> Create -> Charge Document create
            IF W_SUBRC EQ 0 OR
               ( W_SUBRC NE 0 AND W_EDI EQ 'X' ).
               CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
                    EXPORTING
                        W_OK_CODE           =   'SAVE'
                        BUKRS               =   ZTBL-BUKRS
                        GJAHR               =   ZTBKPF-GJAHR
                        ZFSTATUS            =   'C'
                        W_ZTBKPF_OLD        =  *ZTBKPF
                        W_ZTBKPF            =   ZTBKPF
                   TABLES
                        IT_ZSBSEG           =   IT_ZSBSEG
                   CHANGING
                        BELNR               =   ZTBKPF-BELNR
                   EXCEPTIONS
                       ERROR_UPDATE.
            ENDIF.
         ENDIF.
*<---------------  Broker Fee Creation End -------------------------->*

         COMMIT WORK.

      WHEN 'X'.
         DELETE  FROM ZTIDSUS    WHERE ZFIVNO  EQ ZFIVNO
                                 AND   ZFCLSEQ EQ ZFCLSEQ.
         IF SY-SUBRC NE 0.
            ROLLBACK WORK.
            RAISE  ERROR_DELETE.
         ENDIF.

         DELETE  FROM ZTIDSUSH  WHERE ZFIVNO  EQ ZFIVNO
                                AND   ZFCLSEQ EQ ZFCLSEQ.

         DELETE  FROM ZTIDSUSD  WHERE ZFIVNO  EQ ZFIVNO
                                AND   ZFCLSEQ EQ ZFCLSEQ.

         DATA : W_ZFIMPATH.
         SELECT SINGLE ZFIMPATH INTO W_ZFIMPATH FROM ZTIMIMG00.

         IF W_ZFIMPATH NE '3'.
            " INVOICE Status Modify
            SELECT  SINGLE *  FROM  ZTIV
            WHERE   ZFIVNO = ZTIDSUS-ZFIVNO.
            MOVE   SY-UNAME  TO  ZTIV-UNAM.
            MOVE   SY-DATUM  TO  ZTIV-UDAT.
            MOVE   '3'       TO  ZTIV-ZFCUST.
            UPDATE  ZTIV.

            IF SY-SUBRC NE 0.
               ZTIDSUS  =  W_ZTIDSUS_OLD.  INSERT ZTIDSUS.
               LOOP  AT  IT_ZSIDSUSH_OLD.
                 MOVE-CORRESPONDING IT_ZSIDSUSH_OLD  TO  ZTIDSUSH.
                 INSERT  ZTIDSUSH.
               ENDLOOP.
               LOOP  AT  IT_ZSIDSUSD_OLD.
                  MOVE-CORRESPONDING IT_ZSIDSUSD_OLD TO ZTIDSUSD.
                  INSERT  ZTIDSUSD.
               ENDLOOP.
               ROLLBACK WORK.
               RAISE  ERROR_DELETE.
            ENDIF.

         ELSEIF W_ZFIMPATH EQ '3'.

            DELETE  FROM ZTIDRUS  WHERE ZFIVNO  EQ ZFIVNO
                                  AND   ZFCLSEQ EQ ZFCLSEQ.
            IF SY-SUBRC NE 0.
               ROLLBACK WORK.
               RAISE  ERROR_DELETE.
            ENDIF.

            DELETE  FROM ZTIDRUSH  WHERE ZFIVNO  EQ ZFIVNO
                                   AND   ZFCLSEQ EQ ZFCLSEQ.

            DELETE  FROM ZTIDRUSD  WHERE ZFIVNO  EQ ZFIVNO
                                   AND   ZFCLSEQ EQ ZFCLSEQ.
            DELETE  FROM ZTIV      WHERE  ZFIVNO  EQ  ZTIDSUS-ZFIVNO.

            IF SY-SUBRC NE 0.
               ROLLBACK WORK.
               RAISE  ERROR_DELETE.
            ENDIF.

            DELETE  FROM ZTIVIT    WHERE  ZFIVNO  EQ  ZTIDSUS-ZFIVNO.
            DELETE  FROM ZTIVCD    WHERE  ZFIVNO  EQ  ZTIDSUS-ZFIVNO.
            DELETE  FROM ZTIVHST   WHERE  ZFIVNO  EQ  ZTIDSUS-ZFIVNO.
            DELETE  FROM ZTIVHST1  WHERE  ZFIVNO  EQ  ZTIDSUS-ZFIVNO.
            DELETE  FROM ZTIVHSTIT WHERE  ZFIVNO  EQ  ZTIDSUS-ZFIVNO.
         ENDIF.

         ">> Charge Document Delete
         SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTBSEG
                  FROM  ZTBSEG
                  WHERE ZFCSTGRP   EQ   '006'
                  AND   ZFIMDNO    EQ   ZTIDSUS-ZFIVNO.

         IF SY-SUBRC EQ 0.
            LOOP AT IT_ZTBSEG.
               DELETE FROM ZTBKPF
                  WHERE BUKRS    EQ  IT_ZTBSEG-BUKRS
                  AND   BELNR    EQ  IT_ZTBSEG-BELNR
                  AND   GJAHR    EQ  IT_ZTBSEG-GJAHR.

               DELETE FROM ZTBSEG
                  WHERE BUKRS    EQ  IT_ZTBSEG-BUKRS
                  AND   BELNR    EQ  IT_ZTBSEG-BELNR
                  AND   GJAHR    EQ  IT_ZTBSEG-GJAHR.

               DELETE FROM ZTBDIV
                  WHERE BUKRS    EQ  IT_ZTBSEG-BUKRS
                  AND   BELNR    EQ  IT_ZTBSEG-BELNR
                  AND   GJAHR    EQ  IT_ZTBSEG-GJAHR.

               DELETE FROM ZTBHIS
                  WHERE BUKRS    EQ  IT_ZTBSEG-BUKRS
                  AND   BELNR    EQ  IT_ZTBSEG-BELNR
                  AND   GJAHR    EQ  IT_ZTBSEG-GJAHR.
            ENDLOOP.
         ENDIF.
*----------------------------------------------------------------
         COMMIT WORK.

      WHEN OTHERS.            " Change

         IF W_ZTIDSUS_OLD NE ZTIDSUS.
            MOVE : SY-UNAME TO  ZTIDSUS-UNAM,
                   SY-DATUM TO  ZTIDSUS-UDAT.
            UPDATE   ZTIDSUS.
            IF SY-SUBRC NE 0.
               ROLLBACK WORK.
               RAISE ERROR_UPDATE.
            ENDIF.
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDSUS'
                 EXPORTING
                    UPD_CHNGIND     =     'U'
                    N_ZTIDSUS       =     ZTIDSUS
                    O_ZTIDSUS       =     W_ZTIDSUS_OLD.

         ENDIF.

         " HS DATA Update.
         SELECT * FROM ZTIDSUSH WHERE ZFIVNO   EQ  ZFIVNO
                                AND   ZFCLSEQ  EQ  ZFCLSEQ.

            READ TABLE IT_ZSIDSUSH WITH KEY ZFIVNO  = ZTIDSUSH-ZFIVNO
                                            ZFCLSEQ = ZTIDSUSH-ZFCLSEQ
                                   BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               *ZTIDSUSH = ZTIDSUSH.
               MOVE-CORRESPONDING IT_ZSIDSUSH TO ZTIDSUSH.

               UPDATE ZTIDSUSH.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDSUSH'
                    EXPORTING
                       UPD_CHNGIND       =     'U'
                       N_ZTIDSUSH        =     ZTIDSUSH
                       O_ZTIDSUSH        =     *ZTIDSUSH.
            ELSE.
               DELETE ZTIDSUSH.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDSUSH'
                    EXPORTING
                       UPD_CHNGIND       =     'U'
                       N_ZTIDSUSH        =     ZTIDSUSH
                       O_ZTIDSUSH        =     *ZTIDSUSH.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSIDSUSH.
            SELECT SINGLE * FROM  ZTIDSUSH
                            WHERE ZFIVNO   EQ  ZFIVNO
                            AND   ZFCLSEQ  EQ  ZFCLSEQ
                            AND   ZFCONO   EQ  IT_ZSIDSUSH-ZFCONO.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSIDSUSH TO ZTIDSUSH.
               MOVE : ZFIVNO                  TO ZTIDSUSH-ZFIVNO,
                      ZFCLSEQ                 TO ZTIDSUSH-ZFCLSEQ,
                      SY-MANDT                TO ZTIDSUSH-MANDT.

               INSERT  ZTIDSUSH.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
               CLEAR : *ZTIDSUSH.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDSUSH'
                    EXPORTING
                       UPD_CHNGIND       =     'U'
                       N_ZTIDSUSH        =     ZTIDSUSH
                       O_ZTIDSUSH        =     *ZTIDSUSH.
            ENDIF.
         ENDLOOP.

         " HS Detail
         SELECT * FROM ZTIDSUSD  WHERE ZFIVNO   EQ  ZFIVNO
                                 AND   ZFCLSEQ  EQ  ZFCLSEQ.

            READ TABLE IT_ZSIDSUSD  WITH KEY ZFIVNO  = ZTIDSUSD-ZFIVNO
                                             ZFCLSEQ = ZTIDSUSD-ZFCLSEQ
                                             ZFCONO  = ZTIDSUSD-ZFCONO
                                             ZFRONO  = ZTIDSUSD-ZFRONO
                                    BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               *ZTIDSUSD = ZTIDSUSD.
               MOVE-CORRESPONDING IT_ZSIDSUSD  TO ZTIDSUSD.
               UPDATE ZTIDSUSD.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDSUSD'
                    EXPORTING
                       UPD_CHNGIND       =     'U'
                       N_ZTIDSUSD        =     ZTIDSUSD
                       O_ZTIDSUSD        =     *ZTIDSUSD.
            ELSE.
               DELETE ZTIDSUSD.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDSUSD'
                    EXPORTING
                       UPD_CHNGIND       =     'D'
                       N_ZTIDSUSD        =     ZTIDSUSD
                       O_ZTIDSUSD        =     *ZTIDSUSD.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSIDSUSD.
            SELECT SINGLE * FROM  ZTIDSUSD
                            WHERE ZFIVNO   EQ  ZFIVNO
                            AND   ZFCLSEQ  EQ  ZFCLSEQ
                            AND   ZFCONO   EQ  IT_ZSIDSUSD-ZFCONO
                            AND   ZFRONO   EQ  IT_ZSIDSUSD-ZFRONO.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSIDSUSD  TO ZTIDSUSD.

               MOVE : ZFIVNO                 TO ZTIDSUSD-ZFIVNO,
                      ZFCLSEQ                TO ZTIDSUSD-ZFCLSEQ,
                      SY-MANDT               TO ZTIDSUSD-MANDT.

               INSERT  ZTIDSUSD.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
               CLEAR : *ZTIDSUSD.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDSUSD'
                    EXPORTING
                       UPD_CHNGIND       =     'I'
                       N_ZTIDSUSD        =     ZTIDSUSD
                       O_ZTIDSUSD        =     *ZTIDSUSD.
            ENDIF.
         ENDLOOP.
         COMMIT WORK.
   ENDCASE.

  IF ZFSTATUS NE 'X'.
     UPDATE ZTIV
     SET    ZFCUST  = 'Y'
            UNAM    = SY-UNAME
            UDAT    = SY-DATUM
     WHERE  ZFIVNO  = ZTIDSUS-ZFIVNO.
     IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        RAISE  ERROR_UPDATE.
     ENDIF.
  ENDIF.

ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  P1000_IMPORT_DOC_CHEKC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_IMPORT_DOC_CHEKC USING    P_ZFIMDNO
                                     P_ZFDCNM
                                     P_ZFPOYN
                                     P_GUBUN
                                     P_KOSTL
                                     ZTBKPF-GSBER
                                     ZTBKPF-BUPLA
                                     W_EDI.

   CLEAR : P_ZFDCNM, ZTREQHD, ZTBL, ZTIV, W_SUBRC.

   IF P_ZFIMDNO IS INITIAL.
      EXIT.
   ENDIF.

   CASE ZTBKPF-ZFCSTGRP.
      WHEN '003'.           ">Import Request
         SELECT SINGLE * FROM ZTREQHD
                         WHERE ZFREQNO EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.
         MOVE: ZTREQHD-ZFOPNNO TO P_ZFDCNM.
         P_ZFPOYN = 'Y'.
         " Business Place GET!
         SELECT SINGLE J_1BBRANCH  INTO  ZTBKPF-BUPLA
         FROM   T001W
         WHERE  WERKS  EQ ZTREQHD-ZFWERKS.

         " Business Area GET!
         SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
         FROM   T134G
         WHERE WERKS EQ ZTREQHD-ZFWERKS.
      WHEN '004' OR '005' .  ">B/L Document No
         SELECT SINGLE * FROM ZTBL
                         WHERE ZFBLNO  EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.
         IF W_SUBRC EQ 0.
            P_ZFPOYN = ZTBL-ZFPOYN.
            P_KOSTL  = ZTBL-KOSTL.
         ELSE.
            P_ZFPOYN = 'Y'.
         ENDIF.
         ">> Cargo Number
         IF NOT ZTBL-ZFGMNO IS INITIAL.
            MOVE: ZTBL-ZFGMNO TO P_ZFDCNM.
            IF NOT ZTBL-ZFMSN IS INITIAL.
               CONCATENATE P_ZFDCNM '-' ZTBL-ZFMSN INTO P_ZFDCNM.
            ENDIF.
            IF NOT ZTBL-ZFHSN IS INITIAL.
               CONCATENATE P_ZFDCNM '-' ZTBL-ZFHSN INTO P_ZFDCNM.
            ENDIF.
         ELSE.
            ">>. HOUSE B/L No.
            IF NOT ZTBL-ZFHBLNO IS INITIAL.
               MOVE: ZTBL-ZFHBLNO TO P_ZFDCNM.
            ELSE.
               ">> MASTER B/L No.
               IF NOT ZTBL-ZFMBLNO IS INITIAL.
                  MOVE: ZTBL-ZFMBLNO TO P_ZFDCNM.
               ELSE.
                  ">> Cargo House No
                  IF NOT ZTBL-ZFCGHNO IS INITIAL.
                     MOVE: ZTBL-ZFCGHNO TO P_ZFDCNM.
                  ENDIF.
               ENDIF.
            ENDIF.
         ENDIF.
         " Business place No
         SELECT SINGLE J_1BBRANCH  INTO  ZTBKPF-BUPLA
         FROM   T001W
         WHERE  WERKS  EQ ZTBL-ZFWERKS.

         " Business area GET!
         SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
         FROM   T134G
         WHERE WERKS EQ ZTBL-ZFWERKS.
      WHEN '006'.           ">Customs clearance Document No
         SELECT SINGLE * FROM ZTIV
                         WHERE ZFIVNO  EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.

         IF W_SUBRC EQ 0.
            P_ZFPOYN = ZTIV-ZFPOYN.
         ELSE.
            P_ZFPOYN = 'Y'.
         ENDIF.

         IF W_SUBRC EQ 0.
            IF ZTIV-ZFCUST EQ '3' OR ZTIV-ZFCUST EQ 'Y'.
               SELECT SINGLE * FROM ZTIDSUS
                               WHERE ZFIVNO  EQ P_ZFIMDNO.
               IF SY-SUBRC EQ 0.
                  IF ZTIDRUS-ZFENTNO IS INITIAL.
                     MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
                  ELSE.
                     MOVE: ZTIDSUS-ZFENTNO TO P_ZFDCNM.
                  ENDIF.
               ELSE.
                  MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
               ENDIF.
            ELSE.
               MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
            ENDIF.
         ENDIF.
         SELECT SINGLE * FROM ZTBL
                WHERE ZFBLNO EQ ZTIV-ZFBLNO.
         " Business Place GET!
         SELECT SINGLE J_1BBRANCH  INTO  ZTBKPF-BUPLA
         FROM   T001W
         WHERE  WERKS  EQ ZTBL-ZFWERKS.

         " Business area GET!
         SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
         FROM   T134G
         WHERE WERKS EQ ZTBL-ZFWERKS.
      WHEN OTHERS.
         EXIT.
   ENDCASE.

   ">> Error occured.
   IF W_SUBRC NE 0.
      IF W_EDI NE 'X'.
         CASE ZTBKPF-ZFCSTGRP.
            WHEN '003'.
               MESSAGE E585 WITH 'Import Req. No'  P_ZFIMDNO.
            WHEN '004' OR '005'.
               MESSAGE E585 WITH 'B/L Doc. No.'     P_ZFIMDNO.
            WHEN '006'.
               MESSAGE E585 WITH 'Customs Doc. No.' P_ZFIMDNO.
         ENDCASE.
      ENDIF.
   ENDIF.

ENDFORM.                    " P1000_IMPORT_DOC_CHEKC
