FUNCTION ZIM_ZTIDS_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFBLNO) LIKE  ZTIDS-ZFBLNO
*"     VALUE(ZFCLSEQ) LIKE  ZTIDS-ZFCLSEQ
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTIDS_OLD) LIKE  ZTIDS STRUCTURE  ZTIDS
*"     VALUE(W_ZTIDS) LIKE  ZTIDS STRUCTURE  ZTIDS
*"     VALUE(W_OK_CODE)
*"     VALUE(W_EDI) TYPE  C DEFAULT SPACE
*"  TABLES
*"      IT_ZSCUCLCST_OLD STRUCTURE  ZSCUCLCST OPTIONAL
*"      IT_ZSCUCLCST STRUCTURE  ZSCUCLCST OPTIONAL
*"      IT_ZSIDSHS_OLD STRUCTURE  ZSIDSHS OPTIONAL
*"      IT_ZSIDSHS STRUCTURE  ZSIDSHS
*"      IT_ZSIDSHSD_OLD STRUCTURE  ZSIDSHSD OPTIONAL
*"      IT_ZSIDSHSD STRUCTURE  ZSIDSHSD
*"      IT_ZSIDSHSL_OLD STRUCTURE  ZSIDSHSL OPTIONAL
*"      IT_ZSIDSHSL STRUCTURE  ZSIDSHSL OPTIONAL
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"      ERROR_DELETE
*"      ERROR_INSERT
*"----------------------------------------------------------------------
TABLES  *ZTIMIMG08.

DATA : W_MAX_SEQ     LIKE   ZTCUCLCST-ZFCSQ,
       W_DATE        LIKE   SY-DATUM,
       L_DATE        LIKE   SY-DATUM,
       L_YEAR(4),
       L_MONTH(2),
       L_SUBRC       LIKE   SY-SUBRC,
       W_ZFBTSEQ     LIKE   ZTBLOUR-ZFBTSEQ,
       W_ZFIVAMT_EX  LIKE   BAPICURR-BAPICURR,
       W_ZFIVAMT_IN  LIKE   BAPICURR-BAPICURR,
       DIGITS        TYPE   I   VALUE  20.

* 수입 IMG GET!
   CLEAR : ZTIMIMG00.
   SELECT SINGLE * FROM ZTIMIMG00.

   MOVE-CORRESPONDING  W_ZTIDS  TO  ZTIDS.

   MOVE : ZFBLNO      TO     ZTIDS-ZFBLNO,
          ZFCLSEQ     TO     ZTIDS-ZFCLSEQ,
          SY-MANDT    TO     ZTIDS-MANDT,
          SY-UNAME    TO     ZTIDS-UNAM,
          SY-DATUM    TO     ZTIDS-UDAT.

* BL 정보 GET!
   SELECT SINGLE * FROM  ZTBL WHERE ZFBLNO = ZTIDS-ZFBLNO.

   IF W_OK_CODE EQ 'DELE'.
      ZFSTATUS = 'X'.
   ENDIF.

*>
*   SET UPDATE TASK LOCAL.

   CASE ZFSTATUS.
      WHEN 'C'.               " 생성.
         MOVE : SY-UNAME      TO    ZTIDS-ERNAM,
                SY-DATUM      TO    ZTIDS-CDAT.

*>> 통관수수료 COMPUTE.
         IF ZTIMIMG00-ZFCUMTD EQ '1'.
            IF ZTIDS-ZFCUTAMT IS INITIAL.
               CALL FUNCTION 'ZIM_CC_TAX_CALCULATE'
                    CHANGING
                        ZTIDS =  ZTIDS.
            ENDIF.
         ENDIF.

         INSERT   ZTIDS.
         IF SY-SUBRC NE 0.
            ROLLBACK WORK.
            RAISE ERROR_UPDATE.
         ENDIF.

         DELETE FROM ZTIDSHS WHERE ZFBLNO  =  ZFBLNO
                             AND   ZFCLSEQ =  ZFCLSEQ.

         DELETE FROM ZTIDSHSD WHERE ZFBLNO  =  ZFBLNO
                              AND   ZFCLSEQ =  ZFCLSEQ.

         DELETE FROM ZTIDSHSL WHERE ZFBLNO  =  ZFBLNO
                              AND   ZFCLSEQ =  ZFCLSEQ.

         DELETE FROM ZTIDRCR WHERE ZFBLNO  =  ZFBLNO
                             AND   ZFCLSEQ =  ZFCLSEQ.

         DELETE FROM ZTIDRCRIT WHERE ZFBLNO  =  ZFBLNO
                               AND   ZFCLSEQ =  ZFCLSEQ.

         LOOP AT IT_ZSIDSHS.
            CLEAR : ZTIDSHS.
            MOVE-CORRESPONDING IT_ZSIDSHS TO ZTIDSHS.
            MOVE : ZFBLNO                 TO ZTIDSHS-ZFBLNO,
                   ZFCLSEQ                TO ZTIDSHS-ZFCLSEQ,
                   SY-MANDT               TO ZTIDSHS-MANDT.

            INSERT   ZTIDSHS.
* ERROR 발생시 DATA ROLLBACK
            IF SY-SUBRC NE 0.
               ROLLBACK WORK.
               RAISE ERROR_UPDATE.
            ENDIF.
*            CLEAR ZTIDRCR.
*            MOVE ZFBLNO                   TO ZTIDRCR-ZFBLNO.
*            MOVE ZFCLSEQ                  TO ZTIDRCR-ZFCLSEQ.
*            MOVE IT_ZSIDSHS-ZFCONO        TO ZTIDRCR-ZFCONO.
*            MOVE IT_ZSIDSHS-STAWN         TO ZTIDRCR-STAWN.
*            MOVE IT_ZSIDSHS-ZFTBAK        TO ZTIDRCR-ZFTBAK.
*            MOVE 'KRW'                    TO ZTIDRCR-ZFKRW.
*            MOVE IT_ZSIDSHS-ZFTBAU        TO ZTIDRCR-ZFTBAU.
*            MOVE 'USD'                    TO ZTIDRCR-ZFUSD.
*            MOVE IT_ZSIDSHS-ZFCUAMT       TO ZTIDRCR-ZFCUAMT.
*            MOVE IT_ZSIDSHS-ZFHMAMT       TO ZTIDRCR-ZFHMTX.
*            MOVE IT_ZSIDSHS-ZFEDAMT       TO ZTIDRCR-ZFEDTX.
*            MOVE IT_ZSIDSHS-ZFAGAMT       TO ZTIDRCR-ZFATX.
*            MOVE IT_ZSIDSHS-ZFVAAMT       TO ZTIDRCR-ZFVTX.
*            MOVE IT_ZSIDSHS-ZFCCAMT       TO ZTIDRCR-ZFCUDAK.
*            MOVE IT_ZSIDSHS-ZFRDRT        TO ZTIDRCR-ZFRDRT.
*
*            SELECT  SINGLE *   FROM  ZTIMIMG06
*            WHERE   WAERS    =   ZTIDS-ZFSTAMC
*            AND     ZFAPLDT  =   ( SELECT  MAX( ZFAPLDT )
*                                   FROM    ZTIMIMG06
*                                   WHERE   WAERS   =   ZTIDS-ZFSTAMC
*                                   AND     ZFAPLDT <=  ZTIDS-ZFIDSDT
*                                   AND     ZFEXPDT >=  ZTIDS-ZFIDSDT ).
*
*            IF SY-SUBRC  NE 0. ZTIMIMG06  =  1200. ENDIF.
*            IF NOT ( ZTIMIMG06-ZFEXRT IS INITIAL ).
*               ZTIDRCR-ZFCUDAU = ZTIDRCR-ZFCUDAK /
*                                 ZTIMIMG06-ZFEXRT * 100.
*            ENDIF.
*            IF ZTIDRCR-ZFMATGB = '4'.
*                MOVE '3'         TO  ZTIDRCR-ZFDETY. " 연구소(시설재)
*             ELSE.
*                MOVE '4'         TO  ZTIDRCR-ZFDETY. " 연구소(원자재)
*             ENDIF.
**            ENDIF.
**            IF IT_ZSIDSHS-ZFCDPNO = 'A0370301    '.
**               MOVE '5'            TO ZTIDRCR-ZFDETY. " 용도세?
**            ENDIF.
**            IF ( IT_ZSIDSHS-ZFCDPNO >= 'A02900010001' AND
**                 IT_ZSIDSHS-ZFCDPNO <= 'A02900010023' ).
**                 MOVE '6'        TO  ZTIDRCR-ZFDETY. " 재수출조?
**            ENDIF.
**            IF ( IT_ZSIDSHS-ZFCDPNO >= 'A034000001  ' AND
**                 IT_ZSIDSHS-ZFCDPNO <= 'A034000004  ' ).
**                 MOVE '7'        TO  ZTIDRCR-ZFDETY. " 재수?
**            ENDIF.
*            MOVE ZTBL-ZFMATGB          TO ZTIDRCR-ZFMATGB.
*            MOVE SY-UNAME              TO ZTIDRCR-ERNAM.
*            MOVE SY-DATUM              TO ZTIDRCR-CDAT.
*            MOVE SY-UNAME              TO ZTIDRCR-UNAM.
*            MOVE SY-DATUM              TO ZTIDRCR-UDAT.
*            INSERT ZTIDRCR.
** ERROR 발생시 DATA ROLLBACK
*            IF SY-SUBRC NE 0.
*               ROLLBACK WORK.
**               DELETE FROM ZTIDS     WHERE  ZFBLNO  =  ZFBLNO
**                                     AND    ZFCLSEQ =  ZFCLSEQ.
**               DELETE FROM ZTCUCLCST WHERE  ZFBLNO  =  ZFBLNO
**                                     AND    ZFCLSEQ =  ZFCLSEQ.
**               DELETE FROM ZTIDSHS   WHERE  ZFBLNO  =  ZFBLNO
**                                     AND    ZFCLSEQ =  ZFCLSEQ.
**               DELETE FROM ZTIDRCR   WHERE  ZFBLNO  =  ZFBLNO
**                                     AND    ZFCLSEQ =  ZFCLSEQ.
*               RAISE ERROR_UPDATE.
*            ENDIF.
         ENDLOOP.

         LOOP AT IT_ZSIDSHSD.
            CLEAR : ZTIDSHSD.
            MOVE-CORRESPONDING IT_ZSIDSHSD TO ZTIDSHSD.
            MOVE : ZFBLNO                 TO ZTIDSHSD-ZFBLNO,
                   ZFCLSEQ                TO ZTIDSHSD-ZFCLSEQ,
                   SY-MANDT               TO ZTIDSHSD-MANDT.

            INSERT   ZTIDSHSD.
* ERROR 발생시 DATA ROLLBACK
            IF SY-SUBRC NE 0.
*               DELETE FROM ZTIDS     WHERE  ZFBLNO  =  ZFBLNO
*                                     AND    ZFCLSEQ =  ZFCLSEQ.
*               DELETE FROM ZTCUCLCST WHERE  ZFBLNO  =  ZFBLNO
*                                     AND    ZFCLSEQ =  ZFCLSEQ.
*               DELETE FROM ZTIDSHS   WHERE  ZFBLNO  =  ZFBLNO
*                                     AND    ZFCLSEQ =  ZFCLSEQ.
*               DELETE FROM ZTIDSHSD  WHERE  ZFBLNO  =  ZFBLNO
*                                     AND    ZFCLSEQ =  ZFCLSEQ.
*               DELETE FROM ZTIDRCR   WHERE  ZFBLNO  =  ZFBLNO
*                                     AND    ZFCLSEQ =  ZFCLSEQ.
               ROLLBACK WORK.
               RAISE ERROR_UPDATE.
            ENDIF.
* 감면허가 품목 해당하면 감면품목 DATA CREATE.
            READ TABLE IT_ZSIDSHS  WITH KEY
                                   ZFBLNO  =  IT_ZSIDSHSD-ZFBLNO
                                   ZFCLSEQ =  IT_ZSIDSHSD-ZFCLSEQ
                                   ZFCONO  =  IT_ZSIDSHSD-ZFCONO.
            IF SY-SUBRC EQ 0.
*               IF ( IT_ZSIDSHS-ZFCDPNO >= 'A00700010001' AND
*                    IT_ZSIDSHS-ZFCDPNO <= 'A00790000000' ) OR
*                  ( IT_ZSIDSHS-ZFCDPNO >= 'A028070101  ' AND
*                    IT_ZSIDSHS-ZFCDPNO <= 'A02807010402' ) OR
*                  ( IT_ZSIDSHS-ZFCDPNO  = 'A028050109  ' ) OR
*                  ( IT_ZSIDSHS-ZFCDPNO  = 'A0370301    ' ) OR
*                  ( IT_ZSIDSHS-ZFCDPNO >= 'A02900010001' AND
*                    IT_ZSIDSHS-ZFCDPNO <= 'A02900010023' ) OR
*                  ( IT_ZSIDSHS-ZFCDPNO >= 'A034000001  ' AND
*                    IT_ZSIDSHS-ZFCDPNO <= 'A034000004  ' ).
*               ELSE.
*                  CONTINUE.
*               ENDIF.
            ELSE.
               CONTINUE.
            ENDIF.

*            CLEAR ZTIDRCRIT.
*            MOVE IT_ZSIDSHSD-ZFBLNO        TO ZTIDRCRIT-ZFBLNO.
*            MOVE IT_ZSIDSHSD-ZFCLSEQ       TO ZTIDRCRIT-ZFCLSEQ.
*            MOVE IT_ZSIDSHSD-ZFCONO        TO ZTIDRCRIT-ZFCONO.
*            MOVE IT_ZSIDSHSD-ZFRONO        TO ZTIDRCRIT-ZFRONO.
*            MOVE IT_ZSIDSHSD-ZFSTCD        TO ZTIDRCRIT-MATNR.
*            MOVE IT_ZSIDSHSD-ZFGDDS1       TO ZTIDRCRIT-MAKTX.
*            MOVE IT_ZSIDSHSD-ZFQNT         TO ZTIDRCRIT-ZFDEQN.
*            MOVE IT_ZSIDSHSD-ZFQNTM        TO ZTIDRCRIT-ZFDEQNM.
**            IF ( IT_ZSIDSHS-ZFCDPNO >= 'A00700010001' AND
**                 IT_ZSIDSHS-ZFCDPNO <= 'A00790000000' ).
**                 MOVE '1'           TO  ZTIDRCRIT-ZFDETY. " 첨?
**            ENDIF.
**            IF ( IT_ZSIDSHS-ZFCDPNO >= 'A028070101  ' AND
**                 IT_ZSIDSHS-ZFCDPNO <= 'A02807010402' ).
**                 MOVE '2'           TO  ZTIDRCRIT-ZFDETY. " 자동?
**            ENDIF.
**            IF IT_ZSIDSHS-ZFCDPNO = 'A028050109  '.
**              IF ZTBL-ZFMATGB = '4'.
**                 MOVE '3'      TO  ZTIDRCRIT-ZFDETY. " 연구소(시설재)
**              ELSE.
**                 MOVE '4'      TO  ZTIDRCRIT-ZFDETY. " 연구소(원자재)
**              ENDIF.
**            ENDIF.
**            IF IT_ZSIDSHS-ZFCDPNO = 'A0370301    '.
**               MOVE '5'            TO ZTIDRCRIT-ZFDETY. " 용도세?
**            ENDIF.
**            IF ( IT_ZSIDSHS-ZFCDPNO >= 'A02900010001' AND
**                 IT_ZSIDSHS-ZFCDPNO <= 'A02900010023' ).
**                 MOVE '6'        TO  ZTIDRCRIT-ZFDETY. " 재수출조?
**            ENDIF.
**            IF ( IT_ZSIDSHS-ZFCDPNO >= 'A034000001  ' AND
**                 IT_ZSIDSHS-ZFCDPNO <= 'A034000004  ' ).
**                 MOVE '7'        TO  ZTIDRCRIT-ZFDETY. " 재수?
**            ENDIF.
*
**           과세가격....
*            SELECT MAX( ZFREQNO ) INTO ZTIDRCRIT-ZFREQNO
*            FROM   ZTREQHD
*            WHERE  EBELN = ZTBL-ZFREBELN.
*            MOVE ZTBL-ZFMATGB           TO ZTIDRCRIT-ZFMATGB.
*            MOVE ZTBL-ZFPRNAM           TO ZTIDRCRIT-ZFPRNAM.
*            MOVE ZTIDS-ZFIDSDT          TO ZTIDRCRIT-ZFIDSDT.
*
*            IF ZTIDRCRIT-ZFDETY = '1' OR  ZTIDRCRIT-ZFDETY = '3'.
*               CALL FUNCTION 'ZIM_BEFORE_N_MONTH_DATE'
*                    EXPORTING
*                              DATE = ZTIDRCRIT-ZFIDSDT
*                              W_MONTH = 12
*                    IMPORTING
*                              W_OUT_DATE  = W_DATE
*                    EXCEPTIONS
*                              PLAUSIBILITY_CHECK_FAILED = 4.
*                MOVE W_DATE   TO     ZTIDRCRIT-ZFEDDT.
*            ENDIF.
*            IF ZTIDRCRIT-ZFDETY = '2'.
*               CALL FUNCTION 'ZIM_BEFORE_N_MONTH_DATE'
*                    EXPORTING
*                              DATE = ZTIDRCRIT-ZFIDSDT
*                              W_MONTH = 1
*                    IMPORTING
*                              W_OUT_DATE  = W_DATE
*                    EXCEPTIONS
*                              PLAUSIBILITY_CHECK_FAILED = 4.
*                MOVE W_DATE   TO     ZTIDRCRIT-ZFEDDT.
*            ENDIF.
*            MOVE 'KRW'                  TO ZTIDRCRIT-ZFKRW.
*            MOVE 'USD'                  TO ZTIDRCRIT-ZFUSD.
*            MOVE SY-UNAME               TO ZTIDRCRIT-ERNAM.
*            MOVE SY-DATUM               TO ZTIDRCRIT-CDAT.
*            MOVE SY-UNAME               TO ZTIDRCRIT-UNAM.
*            MOVE SY-DATUM               TO ZTIDRCRIT-UDAT.
*            INSERT ZTIDRCRIT.
** ERROR 발생시 DATA ROLLBACK
*            IF SY-SUBRC NE 0.
**               DELETE FROM ZTIDS     WHERE  ZFBLNO  =  ZFBLNO
**                                     AND    ZFCLSEQ =  ZFCLSEQ.
**               DELETE FROM ZTCUCLCST WHERE  ZFBLNO  =  ZFBLNO
**                                     AND    ZFCLSEQ =  ZFCLSEQ.
**               DELETE FROM ZTIDSHS   WHERE  ZFBLNO  =  ZFBLNO
**                                     AND    ZFCLSEQ =  ZFCLSEQ.
**               DELETE FROM ZTIDSHSD  WHERE  ZFBLNO  =  ZFBLNO
**                                     AND    ZFCLSEQ =  ZFCLSEQ.
**               DELETE FROM ZTIDRCRIT WHERE  ZFBLNO  =  ZFBLNO
**                                     AND    ZFCLSEQ =  ZFCLSEQ.
**               DELETE FROM ZTIDRCR   WHERE  ZFBLNO  =  ZFBLNO
**                                     AND    ZFCLSEQ =  ZFCLSEQ.
*               ROLLBACK WORK.
*               RAISE ERROR_UPDATE.
*            ENDIF.

         ENDLOOP.

         LOOP AT IT_ZSIDSHSL.
            CLEAR : ZTIDSHSL.
            MOVE-CORRESPONDING IT_ZSIDSHSL TO ZTIDSHSL.
            MOVE : ZFBLNO                 TO ZTIDSHSL-ZFBLNO,
                   ZFCLSEQ                TO ZTIDSHSL-ZFCLSEQ,
                   SY-MANDT               TO ZTIDSHSL-MANDT.

            INSERT   ZTIDSHSL.
* ERROR 발생시 DATA ROLLBACK
            IF SY-SUBRC NE 0.
               ROLLBACK WORK.
               RAISE ERROR_UPDATE.
            ENDIF.
         ENDLOOP.
* 반출신고 자료 생성.
         SELECT  SINGLE * FROM ZTBLINR
         WHERE   ZFBLNO  =  ZFBLNO
         AND     ZFBTSEQ =  ( SELECT  MAX( ZFBTSEQ )
                              FROM    ZTBLINR
                              WHERE   ZFBLNO   =   ZFBLNO ).
         IF SY-SUBRC EQ 0.
            DELETE FROM ZTBLOUR WHERE ZFBLNO = ZFBLNO
                   AND  ZFBTSEQ =  ( SELECT  MAX( ZFBTSEQ )
                                     FROM    ZTBLINR
                                     WHERE   ZFBLNO  =  ZFBLNO ).
            CLEAR : ZTBLOUR.
            MOVE : SY-MANDT         TO  ZTBLOUR-MANDT,
                   ZFBLNO           TO  ZTBLOUR-ZFBLNO,
                   ZTBLINR-ZFBTSEQ  TO  ZTBLOUR-ZFBTSEQ,
                   ZTBLINR-ZFINRNO  TO  ZTBLOUR-ZFOURNO,
                   ZTBLINR-ZFABNAR  TO  ZTBLOUR-ZFABNAR,
                   ZTBLINR-ZFBNARCD TO  ZTBLOUR-ZFBNARCD,
                   ZTBLINR-ZFYR     TO  ZTBLOUR-ZFYR,
                   ZTBLINR-BUKRS    TO  ZTBLOUR-BUKRS,
                   ZTBLINR-ZFSEQ    TO  ZTBLOUR-ZFSEQ,
                   '9'              TO  ZTBLOUR-ZFEDINF,
                   ZTBLINR-ZFINRC   TO  ZTBLOUR-ZFINRC,
                   ZTBLINR-ZFINRCD  TO  ZTBLOUR-ZFINRCD,
                   ZTBLINR-ZFPINS   TO  ZTBLOUR-ZFPOUS,
                   ZTBLINR-ZFPRIN   TO  ZTBLOUR-ZFPROU,
                   SPACE            TO  ZTBLOUR-ZFPRDS,
                   ZTBLINR-ZFCYCFS  TO  ZTBLOUR-ZFCYCFS,
                   SPACE            TO  ZTBLOUR-ZFPRDS,
                   ZTBLINR-ZFPKCN   TO  ZTBLOUR-ZFOUQN,
                   ZTBLINR-ZFPKCNM  TO  ZTBLOUR-ZFOUQNM,
                   ZTBLINR-ZFINWT   TO  ZTBLOUR-ZFOUWT,
                   ZTBLINR-ZFINTWT  TO  ZTBLOUR-ZFOUTWT,
                   ZTBLINR-ZFINTQN  TO  ZTBLOUR-ZFOUTQN,
                   ZTBLINR-ZFKG     TO  ZTBLOUR-ZFKG,
                   ZTBLINR-ZFCT     TO  ZTBLOUR-ZFCT,
                   ZTBLINR-ZFPINS   TO  ZTBLOUR-ZFPOUS,
                   '50'             TO  ZTBLOUR-ZFOUTY,
                   ZTIDS-ZFIDRNO    TO  ZTBLOUR-ZFOUANO,
                   SY-DATUM         TO  ZTBLOUR-ZFOTDT,
                   SY-UZEIT         TO  ZTBLOUR-ZFOUTM,
                   'O'              TO  ZTBLOUR-ZFDOCST,
                   'N'              TO  ZTBLOUR-ZFEDIST,
                   'O'              TO  ZTBLOUR-ZFEDICK,
                   SPACE            TO  ZTBLOUR-ZFDOCNO,
                   SPACE            TO  ZTBLOUR-ZFDOCNOR,
                   SY-UNAME         TO  ZTBLOUR-ERNAM,
                   SY-DATUM         TO  ZTBLOUR-CDAT,
                   SY-UNAME         TO  ZTBLOUR-UNAM,
                   SY-DATUM         TO  ZTBLOUR-UDAT.
            INSERT ZTBLOUR.
* ERROR 발생시 DATA ROLLBACK
            IF SY-SUBRC NE 0.
               ROLLBACK WORK.
               RAISE ERROR_UPDATE.
            ENDIF.
         ENDIF.

*> 수입신고 상태 바꾸기.
         SELECT SINGLE * FROM ZTIDR
                WHERE ZFBLNO  EQ ZTIDS-ZFBLNO
                AND   ZFCLSEQ EQ ZTIDS-ZFCLSEQ.
         MOVE 'O' TO ZTIDR-ZFDOCST.
         IF ZTIDR-ZFEDIST EQ 'S'.
            ZTIDR-ZFEDIST = 'R'.
         ENDIF.
         UPDATE ZTIDR.

*----------------------------------------------------------------------
*>>> 비용 전기를 위한 테이블 SELECT.
*----------------------------------------------------------------------
         SELECT SINGLE * FROM  ZTBL
                         WHERE ZFBLNO EQ ZTIDS-ZFBLNO.

         SELECT SINGLE * FROM ZTIV
                         WHERE ZFIVNO EQ ZTIDS-ZFIVNO.
*
*                         ( SELECT ZFIVNO FROM ZTCUCLIV
*                                  WHERE ZFBLNO  EQ ZTIDS-ZFBLNO
*                                  AND   ZFCLSEQ EQ ZTIDS-ZFCLSEQ ).

         IF ZTIDS-ZFINRC IS INITIAL.
            COMMIT WORK.
            EXIT.
         ELSE.
            SELECT SINGLE * FROM  ZTIMIMG02
                            WHERE ZFCOTM EQ ZTIDS-ZFINRC.
            IF SY-SUBRC NE 0.
               COMMIT WORK.
               EXIT.
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

         SELECT SINGLE * FROM ZTIMIMG11
                         WHERE BUKRS EQ ZTBL-BUKRS.


*> 세관코드 SELECT.
         SELECT SINGLE * FROM VF_KRED WHERE LIFNR = ZTIMIMG02-ZFVEN
                                      AND   BUKRS = ZTBL-BUKRS.
         L_SUBRC  =  SY-SUBRC.
         IF L_SUBRC NE 0.
            MESSAGE I205(ZIM1) WITH ZTIMIMG02-ZFVEN ZTBL-BUKRS.
         ENDIF.
*         CALL FUNCTION 'FI_VENDOR_DATA'
*              EXPORTING
*                i_bukrs = ZTBL-BUKRS
*                i_lifnr = ZTIMIMG02-ZFVEN
*              IMPORTING
*                e_kred  = vf_kred.
*
*<---------------  관세 데이타 생성. ----------------->*
         IF ZTIDS-ZFCUAMTS GT 0 AND L_SUBRC EQ 0.
            CLEAR : ZTBKPF, *ZTBKPF, IT_ZSBSEG.

            MOVE : ZTBL-BUKRS        TO   ZTBKPF-BUKRS.
*                   ZTBL-EKGRP        TO   ZTBKPF-EKGRP,
*                   'RE'              TO   ZTBKPF-BLART,
*                   T001W-J_1BBRANCH  TO   ZTBKPF-BUPLA.

*> 세관.VENDOR CODE GET
            MOVE ZTIMIMG02-ZFVEN TO   ZTBKPF-LIFNR.
            MOVE ZTIMIMG02-ZFVEN TO   ZTBKPF-ZFVEN.

*> 면허일 + 15일.
            CALL FUNCTION 'ZIM_GET_NEXT_DATE'
                 EXPORTING
                    FACTORY_CALENDAR       =    T001W-FABKL
*                    HOLIDAY_CALENDAR
                    NDATE                  =    ZTIDS-ZFIDSDT
                    NDAY                   =    15
                IMPORTING
                    RDATE                  =    ZTBKPF-ZFBDT. ">기산일.


*            ZTBKPF-BLDAT = ZTIDS-ZFIDSDT + 15.
* 관세 = 수입면허일.
             ZTBKPF-BLDAT = ZTIDS-ZFIDSDT.
             ZTBKPF-BUDAT = ZTIDS-ZFIDSDT.
             ZTBKPF-ZFBDT = ZTIDS-ZFIDSDT.

*            ZTBKPF-BUDAT = SY-DATUM.      ">전기일.
*            ZTBKPF-BLDAT = ZTIDS-ZFIDSDT. ">증빙일.
            ZTBKPF-GJAHR = ZTBKPF-BLDAT(4).
*> 기간...
            ZTBKPF-MONAT = ZTBKPF-BLDAT+4(2).
*            PERFORM PERIODE_ERMITTELN(SAPMZIM02)
*                                   USING ZTBKPF-BUDAT
*                                         ZTBKPF-GJAHR
*                                         ZTBKPF-MONAT.
*
*            SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
*                                FROM T134G
*                                WHERE WERKS EQ ZTBL-ZFWERKS.


            SELECT SINGLE * INTO *ZTIMIMG08 FROM ZTIMIMG08
                   WHERE  ZFCDTY   =   '006'
                   AND    ZFCD     =   '001'.

            ZTBKPF-AKONT = vf_kred-AKONT.
            ZTBKPF-ZTERM = VF_KRED-ZTERM.
            ZTBKPF-MWSKZ = *ZTIMIMG08-ZFCD5.
            ZTBKPF-WAERS = T001-WAERS.
            ZTBKPF-HWAER = T001-WAERS.
            ZTBKPF-WRBTR = ZTIDS-ZFCUAMTS.
            ZTBKPF-DMBTR = ZTIDS-ZFCUAMTS.
            CLEAR : ZTBKPF-XMWST, ZTBKPF-ZFDCSTX.
            ZTBKPF-ZFPCUR = 'X'.
            ZTBKPF-ZFPOSYN = 'N'.
            ZTBKPF-BKTXT = *ZTIMIMG08-ZFCDNM.
            ZTBKPF-XBLNR = ZTIDS-ZFIDRNO.
            ZTBKPF-ZFCSTGRP = '006'.
            ZTBKPF-ZFIMDNO  = ZTIV-ZFIVNO.
            ZTBKPF-ZFATPT   = 'X'.
            ZTBKPF-ZFAUTO   = 'X'.
            ZTBKPF-ZFPOYN   = ZTIV-ZFPOYN.

            IF ZTIV-ZFPOYN EQ 'N'.
               CLEAR : ZTBKPF-ZFDCSTX.
            ELSE.
               ZTBKPF-ZFDCSTX = 'X'.
            ENDIF.

            REFRESH : IT_ZSBSEG.

            MOVE : '006'                TO IT_ZSBSEG-ZFCSTGRP,
                   '001'                TO IT_ZSBSEG-ZFCD,
                   ZTIV-ZFIVNO          TO IT_ZSBSEG-ZFIMDNO,
                   ZTIDS-ZFCUAMTS       TO IT_ZSBSEG-WRBTR,
                   ZTIDS-ZFCUAMTS       TO IT_ZSBSEG-DMBTR,
                   *ZTIMIMG08-ZFCD5     TO IT_ZSBSEG-MWSKZ,
*                   ZTIDS-ZFIDRNO        TO IT_ZSBSEG-ZUONR,
                   *ZTIMIMG08-ZFCDNM    TO IT_ZSBSEG-SGTXT,
                   ZTBKPF-ZFDCSTX       TO IT_ZSBSEG-ZFDCSTX,
                   '40'                 TO IT_ZSBSEG-NEWBS,
                   'S'                  TO IT_ZSBSEG-SHKZG,
                   0                    TO IT_ZSBSEG-FWBAS,
                  *ZTIMIMG08-COND_TYPE  TO IT_ZSBSEG-COND_TYPE.

            PERFORM P1000_IMPORT_DOC_CHEKC   USING IT_ZSBSEG-ZFIMDNO
                                                   IT_ZSBSEG-ZFDCNM
                                                   IT_ZSBSEG-ZFPOYN
                                                   'I'
                                                   IT_ZSBSEG-KOSTL
                                                   ZTBKPF-GSBER
                                                   ZTBKPF-BUPLA
                                                   W_EDI.

            MOVE : ZTIDS-ZFIDRNO     TO IT_ZSBSEG-ZUONR,
                   IT_ZSBSEG-ZFPOYN  TO ZTBKPF-ZFPOYN.

*> 계정결정 함수.
            CALL FUNCTION 'ZIM_GET_NODRAFT_ACCOUNT'
                  EXPORTING
                     ZFCSTGRP   =     IT_ZSBSEG-ZFCSTGRP
                     ZFCD       =     IT_ZSBSEG-ZFCD
                     ZFIMDNO    =     IT_ZSBSEG-ZFIMDNO
                  IMPORTING
                     NEWKO      =     IT_ZSBSEG-NEWKO.

            APPEND IT_ZSBSEG.

            IF ZTIDS-ZFCOCD EQ '33'.  ">사후정산 대상...
*               MOVE 'I3'      TO   ZTBKPF-BLART.
               MOVE  *ZTIMIMG08-BLART  TO  ZTBKPF-BLART.

               CASE ZTIDS-ZFIDSDT+4(2).
                  WHEN '01' OR '02' OR '03'.
                     L_DATE       =  ZTIDS-ZFIDSDT.
                     L_DATE+4(2)  = '04'.
                  WHEN '04' OR '05' OR '06'.
                     L_DATE       =  ZTIDS-ZFIDSDT.
                     L_DATE+4(2)  = '07'.
                  WHEN '07' OR '08' OR '09'.
                     L_DATE       =  ZTIDS-ZFIDSDT.
                     L_DATE+4(2)  = '10'.
                  WHEN '10' OR '11' OR '12'.
                     L_DATE       =  ZTIDS-ZFIDSDT + 365.
                     L_DATE+4(2)  = '01'.
               ENDCASE.
               L_YEAR  = L_DATE(4).
               L_MONTH = L_DATE+4(2).

               CONCATENATE '정산월:' L_YEAR '/' L_MONTH
                            INTO  ZTBKPF-XBLNR.
            ELSE.
*               MOVE 'I4'      TO   ZTBKPF-BLART.
               MOVE  *ZTIMIMG08-BLART  TO  ZTBKPF-BLART.
            ENDIF.


*> 생성시에만 비용문서 생성함.
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
*<---------------  관세 데이타 생성. END ----------------->*

*<---------------  부가세   데이타 생성. ----------------->*
         SELECT SINGLE * FROM VF_KRED WHERE LIFNR = ZTIMIMG02-ZFVEN
                                      AND   BUKRS = ZTBL-BUKRS.
         L_SUBRC  =  SY-SUBRC.
         IF L_SUBRC NE 0.
            MESSAGE S205(ZIM1) WITH ZTIMIMG02-ZFVEN ZTBL-BUKRS.
         ENDIF.

         IF ZTIDS-ZFVAAMTS GT 0 AND L_SUBRC EQ 0.
            CLEAR : ZTBKPF, *ZTBKPF, IT_ZSBSEG.

            MOVE : ZTBL-BUKRS        TO   ZTBKPF-BUKRS,
*                   ZTBL-EKGRP      TO   ZTBKPF-EKGRP,
*                   'I6'              TO   ZTBKPF-BLART,
                   T001W-J_1BBRANCH  TO   ZTBKPF-BUPLA.

*> 세관.VENDOR CODE GET
            MOVE ZTIMIMG02-ZFVEN TO   ZTBKPF-LIFNR.
            MOVE ZTIMIMG02-ZFVEN TO   ZTBKPF-ZFVEN.

            CALL FUNCTION 'ZIM_GET_NEXT_DATE'
                 EXPORTING
                    FACTORY_CALENDAR       =    T001W-FABKL
*                    HOLIDAY_CALENDAR
                    NDATE                  =    ZTIDS-ZFIDSDT
                    NDAY                   =    15
                IMPORTING
                    RDATE                  =    ZTBKPF-BLDAT. ">증빙일.

            ZTBKPF-BUDAT = ZTBKPF-BLDAT.
*            ZTBKPF-BUDAT = SY-DATUM.      ">전기일. " nhj corecess.
            ZTBKPF-ZFBDT = ZTBKPF-BLDAT.  ">기산일.
            ZTBKPF-GJAHR = ZTBKPF-BLDAT(4).
*> 기간...
            ZTBKPF-MONAT = ZTBKPF-BLDAT+4(2).
*            PERFORM PERIODE_ERMITTELN(SAPMZIM02)
*                                   USING ZTBKPF-BUDAT
*                                         ZTBKPF-GJAHR
*                                         ZTBKPF-MONAT.
            SELECT SINGLE * INTO *ZTIMIMG08 FROM ZTIMIMG08
                   WHERE  ZFCDTY   =   '006'
                   AND    ZFCD     =   '003'.

            CLEAR : ZTBKPF-ZFDCSTX.
*-------------------------------------------------------
*> 2001.12.06 KSB MODIFY
*>  부가세 벤더의 조정계정  ===> 미지급금 기타로 Hard Coding
*>  FCM 결정 사항.
*            ZTBKPF-AKONT  = '2121490'.
            ZTBKPF-AKONT = vf_kred-AKONT.
*-------------------------------------------------------
            ZTBKPF-ZTERM = VF_KRED-ZTERM.
            ZTBKPF-MWSKZ = *ZTIMIMG08-ZFCD5.
            ZTBKPF-WAERS = T001-WAERS.
            ZTBKPF-HWAER = T001-WAERS.
            ZTBKPF-WMWST = ZTIDS-ZFVAAMTS.
*            ZTBKPF-DMBTR = ZTIDS-ZFVAAMTS.
            ZTBKPF-BLART = *ZTIMIMG08-BLART.

            CLEAR : ZTBKPF-XMWST, ZTBKPF-ZFDCSTX.
            ZTBKPF-ZFPCUR = 'X'.
            ZTBKPF-ZFPOSYN = 'N'.
            ZTBKPF-BKTXT = *ZTIMIMG08-ZFCDNM.
            ZTBKPF-XBLNR = ZTIDS-ZFIDRNO.
            ZTBKPF-ZFCSTGRP = '006'.
            ZTBKPF-ZFIMDNO  = ZTIV-ZFIVNO.
            ZTBKPF-ZFATPT   = 'X'.
            ZTBKPF-ZFAUTO   = 'X'.

            REFRESH : IT_ZSBSEG.

            MOVE : '006'                TO IT_ZSBSEG-ZFCSTGRP,
                   '003'                TO IT_ZSBSEG-ZFCD,
                   ZTIV-ZFIVNO          TO IT_ZSBSEG-ZFIMDNO,
                   ZTIDS-ZFVAAMTS       TO IT_ZSBSEG-WMWST,
*                   ZTIDS-ZFVAAMTS       TO IT_ZSBSEG-WMWST,
                   *ZTIMIMG08-ZFCD5     TO IT_ZSBSEG-MWSKZ,
*                   ZTIDS-ZFIDRNO        TO IT_ZSBSEG-ZUONR,
                   *ZTIMIMG08-ZFCDNM    TO IT_ZSBSEG-SGTXT,
                   SPACE                TO IT_ZSBSEG-ZFDCSTX,
                   '40'                 TO IT_ZSBSEG-NEWBS,
                   'S'                  TO IT_ZSBSEG-SHKZG,
                   ZTIDS-FWBAS          TO IT_ZSBSEG-FWBAS,
                  *ZTIMIMG08-COND_TYPE  TO IT_ZSBSEG-COND_TYPE.

            PERFORM P1000_IMPORT_DOC_CHEKC   USING IT_ZSBSEG-ZFIMDNO
                                                   IT_ZSBSEG-ZFDCNM
                                                   IT_ZSBSEG-ZFPOYN
                                                   'I'
                                                   IT_ZSBSEG-KOSTL
                                                   ZTBKPF-GSBER
                                                   ZTBKPF-BUPLA
                                                   W_EDI.

            MOVE : ZTIDS-ZFIDRNO     TO IT_ZSBSEG-ZUONR,
                   IT_ZSBSEG-ZFPOYN  TO ZTBKPF-ZFPOYN.

*----------------------------------------------------------------
*------> 대표 플랜트의 BA를 Select.
*>       2001.12.16 KSB INSERT... LG PROJECT.
            SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
                                FROM T134G
                                WHERE WERKS EQ ZTBL-ZFWERKS.
*----------------------------------------------------------------

*> 계정결정 함수.
            CALL FUNCTION 'ZIM_GET_NODRAFT_ACCOUNT'
                  EXPORTING
                     ZFCSTGRP   =     IT_ZSBSEG-ZFCSTGRP
                     ZFCD       =     IT_ZSBSEG-ZFCD
                     ZFIMDNO    =     IT_ZSBSEG-ZFIMDNO
                  IMPORTING
                     NEWKO      =     IT_ZSBSEG-NEWKO.

            APPEND IT_ZSBSEG.

*> 생성시에만 비용문서 생성함.
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
*<---------------  부가세   데이타 생성.END  ----------------->*
* CORECESS 주석처리.
*<---------------  통관수수료 데이타 생성. ----------------->*
*> 세관.VENDOR CODE GET
*         SELECT SINGLE * FROM ZTIMIMG10
*                WHERE ZFCUT  EQ  ZTIDS-ZFCUT.
*
*         MOVE ZTIMIMG10-ZFVEN TO   ZTBKPF-LIFNR.
*         MOVE ZTIMIMG10-ZFVEN TO   ZTBKPF-ZFVEN.
*
*         SELECT SINGLE * FROM VF_KRED WHERE LIFNR = ZTIMIMG10-ZFVEN
*                                      AND   BUKRS = ZTBL-BUKRS.
*         L_SUBRC  =  SY-SUBRC.
*         IF L_SUBRC NE 0.
*            MESSAGE I206(ZIM1) WITH ZTIMIMG10-ZFVEN ZTBL-BUKRS.
*         ENDIF.
**            CALL FUNCTION 'FI_VENDOR_DATA'
**                 EXPORTING
**                   i_bukrs = ZTBKPF-BUKRS
**                   i_lifnr = ZTIMIMG10-ZFVEN
**                 IMPORTING
**                   e_kred  = vf_kred.
**
*
*         IF ZTIDS-ZFCUTAMT GT 0 AND L_SUBRC EQ 0.
*            CLEAR : ZTBKPF, *ZTBKPF, IT_ZSBSEG.
*
*            MOVE : ZTBL-BUKRS        TO   ZTBKPF-BUKRS,
**                   ZTBL-EKGRP        TO   ZTBKPF-EKGRP,
**                   'I6'              TO   ZTBKPF-BLART,
*                   T001W-J_1BBRANCH  TO   ZTBKPF-BUPLA.
*
***> 면허일 + 15일.
**            CALL FUNCTION 'ZIM_GET_NEXT_DATE'
**                 EXPORTING
**                    FACTORY_CALENDAR       =    T001W-FABKL
***                    HOLIDAY_CALENDAR
**                    NDATE                  =    ZTIDS-ZFIDSDT
**                    NDAY                   =    15
**                IMPORTING
**                    RDATE                  =    ZTBKPF-BLDAT.
**
*            ZTBKPF-BLDAT = ZTIDS-ZFIDSDT.  ">면허일.
**            ZTBKPF-BLDAT = ZTIDS-ZFIDSDT + 15.
**NHJ        ZTBKPF-BUDAT = SY-DATUM.       ">전기일.  "CORECESS.
*            ZTBKPF-ZFBDT = ZTIDS-ZFIDSDT.  ">기산일.
*            ZTBKPF-BUDAT = ZTIDS-ZFIDSDT.
*            ZTBKPF-GJAHR = ZTBKPF-BLDAT(4).
**> 기간...
*            ZTBKPF-MONAT = ZTBKPF-BLDAT+4(2).
**            PERFORM PERIODE_ERMITTELN(SAPMZIM02)
**                                   USING ZTBKPF-BUDAT
**                                         ZTBKPF-GJAHR
**                                         ZTBKPF-MONAT.
*
*            SELECT SINGLE * INTO *ZTIMIMG08 FROM ZTIMIMG08
*                   WHERE  ZFCDTY   =   '006'
*                   AND    ZFCD     =   '002'.
*
**            SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
**                                FROM T134G
**                                WHERE WERKS EQ ZTBL-ZFWERKS.
*
*            CLEAR : ZTBKPF-ZFDCSTX.
*
*            IF *ZTIMIMG08-ZFCD1 EQ 'Y'.
*               MOVE  'X'  TO  ZTBKPF-ZFDCSTX.
*            ELSE.
*               CLEAR ZTBKPF-ZFDCSTX.
*            ENDIF.
*
*            ZTBKPF-LIFNR = VF_KRED-LIFNR.
*            ZTBKPF-ZFVEN = VF_KRED-LIFNR.
*            ZTBKPF-AKONT = vf_kred-AKONT.
*            ZTBKPF-ZTERM = VF_KRED-ZTERM.
*            ZTBKPF-BLART = *ZTIMIMG08-BLART.
*            ZTBKPF-MWSKZ = *ZTIMIMG08-ZFCD5.
*            ZTBKPF-WAERS = T001-WAERS.
*            ZTBKPF-HWAER = T001-WAERS.
*            ZTBKPF-WRBTR = ZTIDS-ZFCUTAMT.
*            ZTBKPF-DMBTR = ZTIDS-ZFCUTAMT.
*            CLEAR : ZTBKPF-XMWST.
*            ZTBKPF-ZFPCUR = 'X'.
*
*            ZTBKPF-ZFPOSYN = 'N'.
*            ZTBKPF-BKTXT = *ZTIMIMG08-ZFCDNM.
*            ZTBKPF-XBLNR = ZTIDS-ZFIDRNO.
*            ZTBKPF-ZFCSTGRP = '006'.
*            ZTBKPF-ZFIMDNO  = ZTIV-ZFIVNO.
*            ZTBKPF-ZFATPT   = 'X'.
*            ZTBKPF-ZFAUTO   = 'X'.
*
*            REFRESH : IT_ZSBSEG.
*
*            MOVE : '006'                TO IT_ZSBSEG-ZFCSTGRP,
*                   '002'                TO IT_ZSBSEG-ZFCD,
*                   ZTIV-ZFIVNO          TO IT_ZSBSEG-ZFIMDNO,
*                   ZTIDS-ZFCUTAMT       TO IT_ZSBSEG-WRBTR,
*                   ZTIDS-ZFCUTAMT       TO IT_ZSBSEG-DMBTR,
*                   *ZTIMIMG08-ZFCD5     TO IT_ZSBSEG-MWSKZ,
**NHJ                   ZTIDS-ZFIDRNO        TO IT_ZSBSEG-ZUONR,
*                    ZTBKPF-ZFDCSTX      TO IT_ZSBSEG-ZFDCSTX,
*                   *ZTIMIMG08-ZFCDNM    TO IT_ZSBSEG-SGTXT,
**                   SPACE                TO IT_ZSBSEG-ZFDCSTX,
*                   '40'                 TO IT_ZSBSEG-NEWBS,
*                   'S'                  TO IT_ZSBSEG-SHKZG,
*                   0                    TO IT_ZSBSEG-FWBAS,
*                  *ZTIMIMG08-COND_TYPE  TO IT_ZSBSEG-COND_TYPE.
*
*            PERFORM P1000_IMPORT_DOC_CHEKC   USING IT_ZSBSEG-ZFIMDNO
*                                                   IT_ZSBSEG-ZFDCNM
*                                                   IT_ZSBSEG-ZFPOYN
*                                                   'I'
*                                                   IT_ZSBSEG-KOSTL
*                                                   ZTBKPF-GSBER
*                                                   ZTBKPF-BUPLA
*                                                   W_EDI.
*
*            MOVE : ZTIDS-ZFIDRNO     TO IT_ZSBSEG-ZUONR,
*                   IT_ZSBSEG-ZFPOYN  TO ZTBKPF-ZFPOYN.
*
**> 계정결정 함수.
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
*            ">> 부가세 계산.
*             SELECT SINGLE * FROM T007A
*                    WHERE KALSM EQ 'TAXKR'
*                    AND   MWSKZ EQ  ZTBKPF-MWSKZ.
*             IF SY-SUBRC NE 0.
*                MESSAGE E495 WITH 'TAXKR' ZTBKPF-MWSKZ.
*             ENDIF.
*
*             SELECT * FROM  KONP
*                      WHERE KAPPL EQ 'TX'       ">세금.
*                      AND   KSCHL EQ 'KRIT'     ">매입부가세.
*                      AND   MWSK1 EQ ZTBKPF-MWSKZ.
*
*                 MOVE: KONP-KBETR   TO   W_KBETR,         ">비율.
*                       KONP-KONWA   TO   W_KONWA.         ">단위.
*                 IF NOT W_KBETR IS INITIAL.
*                     W_KBETR = W_KBETR / 10.
*                 ENDIF.
*             ENDSELECT.
*
*             IF SY-SUBRC EQ 0.
*                IF NOT W_KBETR IS INITIAL.
*                   PERFORM SET_CURR_CONV_TO_EXTERNAL(SAPMZIM01)
*                     USING ZTBKPF-WRBTR ZTBKPF-WAERS ZTBKPF-WMWST.
**>>>> 총액 : (100 + %) =  X : % ======>
*                  W_WMWST = ZTBKPF-WMWST.
*                  BAPICURR-BAPICURR = ZTBKPF-WMWST * W_KBETR * 1000.
*                  W_KBETR1 = W_KBETR.
*                  W_KBETR = 100.
*                  BAPICURR-BAPICURR = BAPICURR-BAPICURR / W_KBETR.
*
*                  BAPICURR-BAPICURR = BAPICURR-BAPICURR / 1000.
*                  ZTBKPF-WMWST = BAPICURR-BAPICURR.
*                   PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
*                            USING ZTBKPF-WMWST ZTBKPF-WAERS.
*
*                   PERFORM SET_CURR_CONV_TO_EXTERNAL(SAPMZIM01)
*                      USING ZTBKPF-WRBTR ZTBKPF-WAERS ZTBKPF-WRBTR.
*                 ZTBKPF-WRBTR = ZTBKPF-WRBTR + BAPICURR-BAPICURR.
*                 ZTBKPF-DMBTR = ZTBKPF-WRBTR.
*                   PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
*                            USING ZTBKPF-WRBTR ZTBKPF-WAERS.
*                   PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
*                            USING ZTBKPF-DMBTR ZTBKPF-WAERS.
*                 ELSE.
*                    CLEAR : ZTBKPF-WMWST.
*                 ENDIF.
*              ELSE.
*                 CLEAR : ZTBKPF-WMWST.
*              ENDIF.
*
**> 생성시에만 비용문서 생성함.
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
*<---------------  통관수수료 데이타 생성. END ----------------->*
         COMMIT WORK.

      WHEN 'X'.               " 삭제.

*------- 통관절차에 상관없는 공통부분----->>
         DELETE  FROM ZTIDS    WHERE ZFBLNO  EQ ZFBLNO
                               AND   ZFCLSEQ EQ ZFCLSEQ.
         IF SY-SUBRC NE 0.
            ROLLBACK WORK.
            RAISE  ERROR_DELETE.
         ENDIF.

         DELETE  FROM ZTIDSHS  WHERE ZFBLNO  EQ ZFBLNO
                               AND   ZFCLSEQ EQ ZFCLSEQ.

         DELETE  FROM ZTIDSHSD WHERE ZFBLNO  EQ ZFBLNO
                               AND   ZFCLSEQ EQ ZFCLSEQ.

         DELETE  FROM  ZTIDSHSL WHERE  ZFBLNO  =  ZFBLNO
                                AND    ZFCLSEQ =  ZFCLSEQ.
*
*         DELETE  FROM ZTCUCLCST  WHERE  ZFBLNO  EQ  ZFBLNO
*                                 AND    ZFCLSEQ EQ  ZFCLSEQ.
*

         DELETE  FROM ZTIDRCR    WHERE  ZFBLNO  EQ  ZFBLNO
                                 AND    ZFCLSEQ EQ  ZFCLSEQ.
         DELETE  FROM ZTIDRCRIT  WHERE  ZFBLNO  EQ  ZFBLNO
                                 AND    ZFCLSEQ EQ  ZFCLSEQ.

* 통관, INVOICE 상태 변경.
         SELECT  MAX( ZFBTSEQ )  INTO  W_ZFBTSEQ
         FROM    ZTBLOUR
         WHERE   ZFBLNO   =   ZFBLNO.
*         AND     ZFOUTY   =   '50' .
         IF NOT W_ZFBTSEQ IS INITIAL.
            DELETE  FROM  ZTBLOUR
                    WHERE   ZFBLNO  =  ZFBLNO
                    AND     ZFBTSEQ  =  W_ZFBTSEQ.
         ENDIF.
*------- 통관절차에 상관없는 공통부분 끝.----->>

*>>> 통관절차 구분값 가져오기.
  DATA : W_ZFIMPATH.
  SELECT SINGLE ZFIMPATH INTO W_ZFIMPATH FROM ZTIMIMG00.

*------- 통관절차에따라 삭제방법 다름.----->>
 IF W_ZFIMPATH NE '3'. "수입신고, 의뢰 자동생성이 아니면.

*> 수입신고 상태 바꾸기.
         SELECT SINGLE * FROM ZTIDR
                WHERE ZFBLNO  EQ ZTIDS-ZFBLNO
                AND   ZFCLSEQ EQ ZTIDS-ZFCLSEQ.
*         MOVE 'O' TO ZTIDR-ZFDOCST.
         IF ZTIDR-ZFEDIST EQ 'R'.
            ZTIDR-ZFEDIST = 'S'.
            ZTIDR-ZFDOCST = 'R'.
         ELSE.
            ZTIDR-ZFDOCST = 'N'.
         ENDIF.
         UPDATE ZTIDR.

* 통관 상태 변경.
*                 ZFIDRAMU  =  0
*                 ZFIDRAM   =  0
*                 ZFIDSDT = ' '
*                 ZFEXRT    =  0
*
*         UPDATE  ZTCUCL
*         SET     ZFCUST    =  '3'
*                 UNAM      =  SY-UNAME
*                 UDAT = SY-DATUM
*         WHERE   ZFBLNO    =  ZFBLNO
*         AND     ZFCLSEQ   =  ZFCLSEQ.
*         IF SY-SUBRC NE 0.
*            ZTIDS  =  W_ZTIDS_OLD.  INSERT ZTIDS.
*            LOOP  AT  IT_ZSIDSHS_OLD.
*              MOVE-CORRESPONDING IT_ZSIDSHS_OLD  TO  ZTIDSHS.
*              INSERT  ZTIDSHS.
*            ENDLOOP.
*            LOOP  AT  IT_ZSIDSHSD_OLD.
*               MOVE-CORRESPONDING IT_ZSIDSHSD_OLD TO ZTIDSHSD.
*               INSERT  ZTIDSHSD.
*            ENDLOOP.
*            LOOP  AT  IT_ZSIDSHSL_OLD.
*               MOVE-CORRESPONDING IT_ZSIDSHSL_OLD TO ZTIDSHSL.
*               INSERT  ZTIDSHSL.
*            ENDLOOP.
*            LOOP  AT  IT_ZSCUCLCST_OLD.
*                MOVE-CORRESPONDING IT_ZSCUCLCST_OLD TO ZTCUCLCST.
*                INSERT  ZTCUCLCST.
*            ENDLOOP.
*            ROLLBACK WORK.
*            RAISE  ERROR_DELETE.
*         ENDIF.

** 통관용 INVOICE 상태 변경.
*         SELECT  SINGLE *  FROM  ZTCUCLIV
*            WHERE   ZFBLNO  =  ZFBLNO
*            AND  ZFCLSEQ  =  ZFCLSEQ.
*
*         UPDATE  ZTCUCLIV
*            SET     ZFCUST =  '3'
*                    UNAM   =  SY-UNAME
*                    UDAT   = SY-DATUM
*            WHERE   ZFBLNO =  ZFBLNO
*            AND   ZFCLSEQ  =  ZFCLSEQ.
*         IF SY-SUBRC NE 0.
*            ZTIDS  =  W_ZTIDS_OLD.  INSERT ZTIDS.
*            LOOP  AT  IT_ZSIDSHS_OLD.
*              MOVE-CORRESPONDING IT_ZSIDSHS_OLD  TO  ZTIDSHS.
*              INSERT  ZTIDSHS.
*            ENDLOOP.
*            LOOP  AT  IT_ZSIDSHSD_OLD.
*               MOVE-CORRESPONDING IT_ZSIDSHSD_OLD TO ZTIDSHSD.
*               INSERT  ZTIDSHSD.
*            ENDLOOP.
*            LOOP  AT  IT_ZSIDSHSL_OLD.
*               MOVE-CORRESPONDING IT_ZSIDSHSL_OLD TO ZTIDSHSL.
*               INSERT  ZTIDSHSL.
*            ENDLOOP.
*            LOOP  AT  IT_ZSCUCLCST_OLD.
*                MOVE-CORRESPONDING IT_ZSCUCLCST_OLD TO ZTCUCLCST.
*                INSERT  ZTCUCLCST.
*            ENDLOOP.
*            ROLLBACK WORK.
*            RAISE  ERROR_DELETE.
*         ENDIF.
* INVOICE 상태 변경.
         SELECT  SINGLE *  FROM  ZTIV
                 WHERE  ZFIVNO = ZTIDS-ZFIVNO.
*         MOVE   0         TO  ZTIV-ZFEXRT.
*         MOVE   0         TO  ZTIV-ZFIVAMK.
         MOVE   SY-UNAME  TO  ZTIV-UNAM.
         MOVE   SY-DATUM  TO  ZTIV-UDAT.
         MOVE   '3'       TO  ZTIV-ZFCUST.
*         IF  ZTIV-ZFPOYN  =  'N'.
*             MOVE  'N'    TO  ZTIV-ZFCDST.
*         ENDIF.
         UPDATE  ZTIV.

         IF SY-SUBRC NE 0.
            ZTIDS  =  W_ZTIDS_OLD.  INSERT ZTIDS.
            LOOP  AT  IT_ZSIDSHS_OLD.
              MOVE-CORRESPONDING IT_ZSIDSHS_OLD  TO  ZTIDSHS.
              INSERT  ZTIDSHS.
            ENDLOOP.
            LOOP  AT  IT_ZSIDSHSD_OLD.
               MOVE-CORRESPONDING IT_ZSIDSHSD_OLD TO ZTIDSHSD.
               INSERT  ZTIDSHSD.
            ENDLOOP.
            LOOP  AT  IT_ZSIDSHSL_OLD.
               MOVE-CORRESPONDING IT_ZSIDSHSL_OLD TO ZTIDSHSL.
               INSERT  ZTIDSHSL.
            ENDLOOP.
*            LOOP  AT  IT_ZSCUCLCST_OLD.
*                MOVE-CORRESPONDING IT_ZSCUCLCST_OLD TO ZTCUCLCST.
*                INSERT  ZTCUCLCST.
*            ENDLOOP.
            ROLLBACK WORK.
            RAISE  ERROR_DELETE.
         ENDIF.

   ELSEIF W_ZFIMPATH EQ '3'. "수입신고, 의뢰 자동생성 일때.

*>> 수입신고 삭제.
         DELETE  FROM ZTIDR    WHERE ZFBLNO  EQ ZFBLNO
                               AND   ZFCLSEQ EQ ZFCLSEQ.
         IF SY-SUBRC NE 0.
            ROLLBACK WORK.
            RAISE  ERROR_DELETE.
         ENDIF.

         DELETE  FROM ZTIDRHS  WHERE ZFBLNO  EQ ZFBLNO
                               AND   ZFCLSEQ EQ ZFCLSEQ.

         DELETE  FROM ZTIDRHSD WHERE ZFBLNO  EQ ZFBLNO
                               AND   ZFCLSEQ EQ ZFCLSEQ.

         DELETE  FROM  ZTIDRHSL WHERE  ZFBLNO  =  ZFBLNO
                                AND    ZFCLSEQ =  ZFCLSEQ.

*>> 통관요청/입고요청 삭제.
         DELETE  FROM ZTIV      WHERE  ZFIVNO  EQ  ZTIDS-ZFIVNO.

         IF SY-SUBRC NE 0.
            ROLLBACK WORK.
            RAISE  ERROR_DELETE.
         ENDIF.

         DELETE  FROM ZTIVIT    WHERE  ZFIVNO  EQ  ZTIDS-ZFIVNO.

         DELETE  FROM ZTIVCD    WHERE  ZFIVNO  EQ  ZTIDS-ZFIVNO.

         DELETE  FROM ZTIVHST   WHERE  ZFIVNO  EQ  ZTIDS-ZFIVNO.

         DELETE  FROM ZTIVHST1  WHERE  ZFIVNO  EQ  ZTIDS-ZFIVNO.

         DELETE  FROM ZTIVHSTIT WHERE  ZFIVNO  EQ  ZTIDS-ZFIVNO.

   ENDIF.
*------- 통관절차에따라 삭제방법 다름 끝.----->>

*------- 통관절차에 상관없이 동일한 삭제루틴.----->>
*         DELETE  FROM  ZTIDRCR   WHERE  ZFBLNO  =  ZFBLNO
*                                 AND    ZFCLSEQ =  ZFCLSEQ.
*         DELETE  FROM  ZTIDRCRIT WHERE  ZFBLNO  =  ZFBLNO
*                                 AND    ZFCLSEQ =  ZFCLSEQ.
*----------------------------------------------------------------
*> 비용문서 삭제..
         SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTBSEG
                  FROM  ZTBSEG
                  WHERE ZFCSTGRP   EQ   '006'
*                  AND   ZFPOSYN    EQ   'N'
                  AND   ZFIMDNO    EQ   ZTIDS-ZFIVNO.

         IF SY-SUBRC EQ 0.
            READ TABLE IT_ZTBSEG INDEX 1.
            IF SY-SUBRC EQ 0.
               DELETE FROM ZTBKPF
                  WHERE BUKRS    EQ  IT_ZSBSEG-BUKRS
                  AND   BELNR    EQ  IT_ZSBSEG-BELNR
                  AND   GJAHR    EQ  IT_ZSBSEG-GJAHR.

               DELETE FROM ZTBSEG
                  WHERE BUKRS    EQ  IT_ZSBSEG-BUKRS
                  AND   BELNR    EQ  IT_ZSBSEG-BELNR
                  AND   GJAHR    EQ  IT_ZSBSEG-GJAHR.

               DELETE FROM ZTBDIV
                  WHERE BUKRS    EQ  IT_ZSBSEG-BUKRS
                  AND   BELNR    EQ  IT_ZSBSEG-BELNR
                  AND   GJAHR    EQ  IT_ZSBSEG-GJAHR.

               DELETE FROM ZTBHIS
                  WHERE BUKRS    EQ  IT_ZSBSEG-BUKRS
                  AND   BELNR    EQ  IT_ZSBSEG-BELNR
                  AND   GJAHR    EQ  IT_ZSBSEG-GJAHR.
            ENDIF.
         ENDIF.
*----------------------------------------------------------------
         COMMIT WORK.

      WHEN OTHERS.            " 변경.

*>> 정정 사유코드 입력 CHECK!
         IF NOT ZTIDS-ZFCHGCD IS INITIAL.
            MOVE 'Y'          TO ZTIDS-ZFCHGYN.
         ENDIF.
         IF W_ZTIDS_OLD NE ZTIDS.
            MOVE : SY-UNAME TO  ZTIDS-UNAM,
                   SY-DATUM TO  ZTIDS-UDAT,
                   SY-DATUM TO  ZTIDS-ZFCHGDT.
            UPDATE   ZTIDS.
            IF SY-SUBRC NE 0.
               ROLLBACK WORK.
               RAISE ERROR_UPDATE.
            ENDIF.
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDS'
                 EXPORTING
                    UPD_CHNGIND     =     'U'
                    N_ZTIDS         =     ZTIDS
                    O_ZTIDS         =     W_ZTIDS_OLD.

         ENDIF.
** 통관비?
*         SELECT * FROM ZTCUCLCST WHERE ZFBLNO   EQ  ZFBLNO
*                                 AND   ZFCLSEQ  EQ  ZFCLSEQ.
*
*         READ TABLE IT_ZSCUCLCST WITH KEY ZFBLNO  = ZTCUCLCST-ZFBLNO
*                                          ZFCLSEQ = ZTCUCLCST-ZFCLSEQ
*                                          ZFCSQ   = ZTCUCLCST-ZFCSQ
*                               BINARY SEARCH.
*         IF SY-SUBRC EQ 0.
*            MOVE-CORRESPONDING IT_ZSCUCLCST TO ZTCUCLCST.
*            MOVE : SY-UNAME        TO ZTCUCLCST-UNAM,
*            SY-DATUM               TO ZTCUCLCST-UDAT,
*            SY-MANDT               TO ZTCUCLCST-MANDT,
*            ZTCUCLCST-ZFCAMT       TO ZTCUCLCST-ZFCKAMT,
*            ZTCUCLCST-ZFKRW        TO ZTCUCLCST-WAERS,
*            ZFBLNO                 TO ZTCUCLCST-ZFBLNO,
*            ZFCLSEQ                TO ZTCUCLCST-ZFCLSEQ.
*            UPDATE ZTCUCLCST.
*            IF SY-SUBRC NE 0.
*               ROLLBACK WORK.
*               RAISE ERROR_UPDATE.
*            ENDIF.
*         ELSE.
*            DELETE ZTCUCLCST.
*            IF SY-SUBRC NE 0.
*               ROLLBACK WORK.
*               RAISE ERROR_UPDATE.
*            ENDIF.
*         ENDIF.
*         ENDSELECT.

*         SELECT  MAX( ZFCSQ )  INTO  W_MAX_SEQ
*         FROM    ZTCUCLCST
*         WHERE   ZFBLNO  =   ZFBLNO
*         AND     ZFCLSEQ =   ZFCLSEQ .
*         IF SY-SUBRC NE 0 OR W_MAX_SEQ IS INITIAL.
*             W_MAX_SEQ = 0.
*         ENDIF.
*
*         LOOP AT IT_ZSCUCLCST.
*            SELECT SINGLE * FROM  ZTCUCLCST
*                            WHERE ZFBLNO   EQ  ZFBLNO
*                            AND   ZFCLSEQ  EQ  ZFCLSEQ
*                            AND   ZFCSQ    EQ  IT_ZSCUCLCST-ZFCSQ.
*            IF SY-SUBRC NE 0.
*               SELECT  MAX( ZFCSQ )  INTO  W_MAX_SEQ
*               FROM    ZTCUCLCST
*               WHERE   ZFBLNO  =   ZFBLNO
*               AND     ZFCLSEQ =   ZFCLSEQ .
*
*               W_MAX_SEQ  =  W_MAX_SEQ  +  10.
*               MOVE  W_MAX_SEQ  TO  IT_ZSCUCLCST-ZFCSQ.
**               IF IT_ZSCUCLCST-ZFCSQ  IS INITIAL  OR
**                  IT_ZSCUCLCST-ZFCSQ  EQ 0.
**                  W_MAX_SEQ  =  W_MAX_SEQ  +  10.
**                  MOVE  W_MAX_SEQ  TO  IT_ZSCUCLCST-ZFCSQ.
**               ENDIF.
*
*               MOVE-CORRESPONDING IT_ZSCUCLCST TO ZTCUCLCST.
*               MOVE : ZFBLNO                 TO ZTCUCLCST-ZFBLNO,
*                      ZFCLSEQ                TO ZTCUCLCST-ZFCLSEQ,
*                      ZTCUCLCST-ZFCAMT       TO ZTCUCLCST-ZFCKAMT,
*                      ZTCUCLCST-ZFKRW        TO ZTCUCLCST-WAERS,
*                      SY-MANDT               TO ZTCUCLCST-MANDT,
*                      SY-UNAME               TO ZTCUCLCST-ERNAM,
*                      SY-DATUM               TO ZTCUCLCST-CDAT,
*                      SY-UNAME               TO ZTCUCLCST-UNAM,
*                      SY-DATUM               TO ZTCUCLCST-UDAT.
*
*               INSERT  ZTCUCLCST.
*               IF SY-SUBRC NE 0.
*                  ROLLBACK WORK.
*                  RAISE ERROR_UPDATE.
*               ENDIF.
*            ENDIF.
*         ENDLOOP.

* 수입면허 란사항.
         SELECT * FROM ZTIDSHS WHERE ZFBLNO   EQ  ZFBLNO
                               AND   ZFCLSEQ  EQ  ZFCLSEQ.

            READ TABLE IT_ZSIDSHS WITH KEY ZFBLNO  = ZTIDSHS-ZFBLNO
                                           ZFCLSEQ = ZTIDSHS-ZFCLSEQ
                                  BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               *ZTIDSHS = ZTIDSHS.
               MOVE-CORRESPONDING IT_ZSIDSHS TO ZTIDSHS.

               UPDATE ZTIDSHS.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDSHS'
                    EXPORTING
                       UPD_CHNGIND       =     'U'
                       N_ZTIDSHS         =     ZTIDSHS
                       O_ZTIDSHS         =     *ZTIDSHS.

            ELSE.
               DELETE ZTIDSHS.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDSHS'
                    EXPORTING
                       UPD_CHNGIND       =     'D'
                       N_ZTIDSHS         =     ZTIDSHS
                       O_ZTIDSHS         =     *ZTIDSHS.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSIDSHS.
            SELECT SINGLE * FROM  ZTIDSHS
                            WHERE ZFBLNO   EQ  ZFBLNO
                            AND   ZFCLSEQ  EQ  ZFCLSEQ
                            AND   ZFCONO   EQ  IT_ZSIDSHS-ZFCONO.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSIDSHS TO ZTIDSHS.
               MOVE : ZFBLNO                 TO ZTIDSHS-ZFBLNO,
                      ZFCLSEQ                TO ZTIDSHS-ZFCLSEQ,
                      SY-MANDT               TO ZTIDSHS-MANDT.

               INSERT  ZTIDSHS.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
               CLEAR : *ZTIDSHS.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDSHS'
                    EXPORTING
                       UPD_CHNGIND       =     'I'
                       N_ZTIDSHS         =     ZTIDSHS
                       O_ZTIDSHS         =     *ZTIDSHS.
            ENDIF.
         ENDLOOP.

* 수입면허 규?
         SELECT * FROM ZTIDSHSD  WHERE ZFBLNO   EQ  ZFBLNO
                                 AND   ZFCLSEQ  EQ  ZFCLSEQ.

            READ TABLE IT_ZSIDSHSD  WITH KEY ZFBLNO  = ZTIDSHSD-ZFBLNO
                                             ZFCLSEQ = ZTIDSHSD-ZFCLSEQ
                                             ZFCONO  = ZTIDSHSD-ZFCONO
                                             ZFRONO  = ZTIDSHSD-ZFRONO
                                    BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               *ZTIDSHSD = ZTIDSHSD.
               MOVE-CORRESPONDING IT_ZSIDSHSD  TO ZTIDSHSD.
               UPDATE ZTIDSHSD.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDSHSD'
                    EXPORTING
                       UPD_CHNGIND       =     'U'
                       N_ZTIDSHSD        =     ZTIDSHSD
                       O_ZTIDSHSD        =     *ZTIDSHSD.
            ELSE.
               DELETE ZTIDSHSD.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDSHSD'
                    EXPORTING
                       UPD_CHNGIND       =     'D'
                       N_ZTIDSHSD        =     ZTIDSHSD
                       O_ZTIDSHSD        =     *ZTIDSHSD.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSIDSHSD.
            SELECT SINGLE * FROM  ZTIDSHSD
                            WHERE ZFBLNO   EQ  ZFBLNO
                            AND   ZFCLSEQ  EQ  ZFCLSEQ
                            AND   ZFCONO   EQ  IT_ZSIDSHSD-ZFCONO
                            AND   ZFRONO   EQ  IT_ZSIDSHSD-ZFRONO.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSIDSHSD  TO ZTIDSHSD.

               MOVE : ZFBLNO                 TO ZTIDSHSD-ZFBLNO,
                      ZFCLSEQ                TO ZTIDSHSD-ZFCLSEQ,
                      SY-MANDT               TO ZTIDSHSD-MANDT.

               INSERT  ZTIDSHSD.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
               CLEAR : *ZTIDSHSD.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTIDSHSD'
                    EXPORTING
                       UPD_CHNGIND       =     'I'
                       N_ZTIDSHSD        =     ZTIDSHSD
                       O_ZTIDSHSD        =     *ZTIDSHSD.
            ENDIF.
         ENDLOOP.
* 수입면허 추?
         SELECT * FROM ZTIDSHSL  WHERE ZFBLNO   EQ  ZFBLNO
                                 AND   ZFCLSEQ  EQ  ZFCLSEQ.

            READ TABLE IT_ZSIDSHSL  WITH KEY ZFBLNO  = ZTIDSHSL-ZFBLNO
                                             ZFCLSEQ = ZTIDSHSL-ZFCLSEQ
                                             ZFCONO  = ZTIDSHSL-ZFCONO
                                             ZFCNDC  = ZTIDSHSL-ZFCNDC
                                             ZFCNNO  = ZTIDSHSL-ZFCNNO
                                    BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSIDSHSL  TO ZTIDSHSL.
               UPDATE ZTIDSHSL.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
            ELSE.
               DELETE ZTIDSHSL.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSIDSHSL.
            SELECT SINGLE * FROM  ZTIDSHSL
                            WHERE ZFBLNO   EQ  ZFBLNO
                            AND   ZFCLSEQ  EQ  ZFCLSEQ
                            AND   ZFCONO   EQ  IT_ZSIDSHSL-ZFCONO
                            AND   ZFCNDC   EQ  IT_ZSIDSHSL-ZFCNDC
                            AND   ZFCNNO   EQ  IT_ZSIDSHSL-ZFCNNO.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSIDSHSL  TO ZTIDSHSL.

               MOVE : ZFBLNO                 TO ZTIDSHSL-ZFBLNO,
                      ZFCLSEQ                TO ZTIDSHSL-ZFCLSEQ,
                      SY-MANDT               TO ZTIDSHSL-MANDT.

               INSERT  ZTIDSHSL.
               IF SY-SUBRC NE 0.
                  ROLLBACK WORK.
                  RAISE ERROR_UPDATE.
               ENDIF.
            ENDIF.
         ENDLOOP.
         COMMIT WORK.
   ENDCASE.

   IF ZFSTATUS EQ 'X'.
**>> 삭제시 로직....
** 통관용 INVOICE, 통관, INVOICE 상태 변경.
*      UPDATE ZTCUCL
*      SET    ZFCUST    =  'N'
*             UNAM      =  SY-UNAME
*             UDAT      =  SY-DATUM
*      WHERE  ZFBLNO    =  ZTIDS-ZFBLNO
*      AND    ZFCLSEQ   =  ZTIDS-ZFCLSEQ.
*      IF SY-SUBRC NE 0.
*         ROLLBACK WORK.
*         RAISE  ERROR_UPDATE.
*      ENDIF.
*
*      UPDATE ZTCUCLIV
*      SET    ZFCUST    =  'N'
*             UNAM      =  SY-UNAME
*             UDAT      =  SY-DATUM
*      WHERE  ZFBLNO    =  ZTIDS-ZFBLNO
*      AND    ZFCLSEQ   =  ZTIDS-ZFCLSEQ.
*      IF SY-SUBRC NE 0.
*         ROLLBACK WORK.
*         RAISE  ERROR_UPDATE.
*      ENDIF.
*      SELECT * FROM  ZTCUCLIV
*               WHERE  ZFBLNO    =  ZTIDS-ZFBLNO
*               AND    ZFCLSEQ   =  ZTIDS-ZFCLSEQ.
*         UPDATE ZTIV
*         SET    ZFCUST  = '3'
*                UNAM    = SY-UNAME
*                UDAT = SY-DATUM
*         WHERE  ZFIVNO  = ZTCUCLIV-ZFIVNO.
*         IF SY-SUBRC NE 0.
*            ROLLBACK WORK.
*            RAISE  ERROR_UPDATE.
*         ENDIF.
*      ENDSELECT.
*

   ELSE.
*---------------------------------------------------------------------
*>----------- 과세가격 재계산.....
*>> 생성/변경시 로직....
*---------------------------------------------------------------------
*>>------------------< 한수원 주석 처리 >----------------------------*>>
DATA: L_ZTIMIMG06_USD  LIKE ZTIMIMG06,
      W_ZFCAMT          LIKE ZTBSEG-WRBTR,
      W_ZFCAMTU         LIKE ZTBSEG-WRBTR,
      W_AMOUNT_CNF      LIKE ZTBSEG-WRBTR,
      W_ZFINAMT         LIKE ZTIDS-ZFINAMT,
      W_ZFTFA           LIKE ZTIDS-ZFTFA,
      W_ZFCAMTK         LIKE ZTBSEG-WRBTR,
      W_TOT_TAX         LIKE ZTBSEG-WRBTR,
      W_TOT_VAT         LIKE ZTBSEG-WRBTR,
      W_TOT_GAM         LIKE ZTBSEG-WRBTR,
      W_ZFUSQN          LIKE ZTIDRDTU-ZFUSQN,
      L_ZFSTAMT         LIKE ZTIDS-ZFSTAMT,
      L_ZFSTAMT_WON     LIKE ZTIDS-ZFSTAMT.

      SELECT SINGLE * FROM   ZTBL
             WHERE  ZFBLNO = ZFBLNO.

      IF  ZTIDS-ZFSTAMC  EQ  'KRW'.
          ZTIMIMG06-ZFEXRT  =  1.
          ZTIMIMG06-FFACT   =  1.
      ELSE.
          SELECT SINGLE *  FROM  ZTIMIMG06
          WHERE  WAERS     EQ    ZTIDS-ZFSTAMC
          AND    ZFAPLDT   EQ    ( SELECT  MAX( ZFAPLDT )
                                   FROM    ZTIMIMG06
                                   WHERE   WAERS   =  ZTIDS-ZFSTAMC
                                   AND     ZFAPLDT <= ZTIDS-ZFIDSDT
                                   AND     ZFEXPDT >= ZTIDS-ZFIDSDT ).
         IF SY-SUBRC EQ 0.
            MOVE : ZTIMIMG06-ZFEXRT   TO   ZTIDS-ZFEXRT,
                   ZTIMIMG06-FFACT    TO   ZTIDS-FFACT.
         ENDIF.
      ENDIF.

      IF ZTIMIMG06-FFACT EQ 0.
         ZTIMIMG06-FFACT = 1.
      ENDIF.

      IF ZTIDS-ZFSTAMC NE 'USD'.
         SELECT SINGLE * INTO L_ZTIMIMG06_USD FROM ZTIMIMG06
            WHERE WAERS    EQ  'USD'
            AND   ZFAPLDT  EQ ( SELECT MAX( ZFAPLDT )
                                FROM ZTIMIMG06
                                WHERE WAERS    EQ  'USD'
                                AND   ZFAPLDT  LE  ZTIDS-ZFIDWDT
                                AND   ZFEXPDT  GE  ZTIDS-ZFIDWDT ).
        IF SY-SUBRC NE 0.
           MESSAGE W504(ZIM1) WITH ZTIDS-ZFIDWDT.
*           EXIT.
        ENDIF.
     ELSE.
        L_ZTIMIMG06_USD = ZTIMIMG06.
     ENDIF.

*>보험료 출력용으로 전환.
     PERFORM SET_CURR_CONV_TO_EXTERNAL USING ZTIDS-ZFINAMT
                                            'KRW'
                                             W_ZFINAMT.
*>운임 출력용으로 전환.
     PERFORM SET_CURR_CONV_TO_EXTERNAL USING ZTIDS-ZFTFA
                                            'USD'
                                             W_ZFTFA.
*>USD 운임--->원화.
     ZTIDS-ZFTFBC = 'KRW'.
     W_ZFTFA = W_ZFTFA * ( L_ZTIMIMG06_USD-ZFEXRT /
                           L_ZTIMIMG06_USD-FFACT ).
     ZTIDS-ZFTFB = W_ZFTFA.
     PERFORM SET_CURR_CONV_TO_INTERNAL USING ZTIDS-ZFTFB 'KRW'.

*> 결제금액 ---> 외부형으로 변환.
     PERFORM SET_CURR_CONV_TO_EXTERNAL USING ZTIDS-ZFSTAMT
                                             ZTIDS-ZFSTAMC
                                             L_ZFSTAMT.
*> 원화금액으로 환산.(결제금액 원화) --> 해?
     L_ZFSTAMT_WON = L_ZFSTAMT * ( ZTIMIMG06-ZFEXRT /
                                   ZTIMIMG06-FFACT ).

*     SELECT SINGLE *  FROM ZTCUCLIV
*            WHERE     ZFBLNO   = ZFBLNO
*            AND       ZFCLSEQ  = ZFCLSEQ.

     SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVIT
            FROM ZTIVIT
            WHERE  ZFIVNO  = ZTIDS-ZFIVNO.

     CLEAR : W_ZFCAMTK.
     LOOP AT IT_ZSIVIT.
        W_TABIX = SY-TABIX.
        PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIVIT-ZFIVAMT
                                                ZTIDS-ZFSTAMC
                                                BAPICURR-BAPICURR.
*> 란별 과세가격 - 원화.
*  (결제외화금액 * 환율) + 운임원화 + 보험료 원?
        IF ZTIDS-FFACT IS INITIAL. ZTIDS-FFACT = 1. ENDIF.
        IF L_ZFSTAMT IS INITIAL.   L_ZFSTAMT = 1.   ENDIF.

        W_ZFCAMTK = BAPICURR-BAPICURR * ( ZTIDS-ZFEXRT / ZTIDS-FFACT )
               + W_ZFTFA   * ( BAPICURR-BAPICURR / L_ZFSTAMT )
               + W_ZFINAMT * ( BAPICURR-BAPICURR / L_ZFSTAMT ).

*        W_ZFCAMTU = BAPICURR-BAPICURR.
        PERFORM SET_CURR_CONV_TO_INTERNAL USING
                                          W_ZFCAMTK 'KRW'.
        IT_ZSIVIT-ZFIVAMK = W_ZFCAMTK.
        ADD  W_ZFCAMTK    TO   IT_ZSIDSHS-ZFTBAK.
        MODIFY IT_ZSIVIT INDEX W_TABIX.
     ENDLOOP.

* 통관용 INVOICE, 통관, INVOICE 상태 변경.
*      UPDATE ZTCUCL
*      SET    ZFIDSDT   =  ZTIDS-ZFIDSDT
*             ZFEXRT    =  ZTIMIMG06-ZFEXRT
*             ZFIDRAMU  =  ZTIDS-ZFTBAU
*             ZFIDRAM   =  ZTIDS-ZFTBAK
*             ZFIDRNO   =  ZTIDS-ZFIDRNO  ZFCUST    =  'Y'
*             UNAM      =  SY-UNAME       UDAT      =  SY-DATUM
*      WHERE  ZFBLNO    =  ZTIDS-ZFBLNO
*      AND    ZFCLSEQ   =  ZTIDS-ZFCLSEQ.
*      IF SY-SUBRC NE 0.
*         ROLLBACK WORK.
*         RAISE  ERROR_UPDATE.
*      ENDIF.
*
*      SELECT SINGLE *  FROM ZTCUCLIV
*             WHERE  ZFBLNO  = ZFBLNO
*             AND    ZFCLSEQ = ZFCLSEQ.
*
*      MOVE ZTIDS-ZFTBAK  TO ZTCUCLIV-ZFIVAMK.
*      MOVE ZTIDS-ZFSTAMT TO ZTCUCLIV-ZFIVAMT.
*      MOVE 'Y'       TO ZTCUCLIV-ZFCUST.
*      MOVE SY-UNAME  TO ZTCUCLIV-UNAM.
*      MOVE SY-DATUM  TO ZTCUCLIV-UDAT.
*      UPDATE  ZTCUCLIV.
*      IF SY-SUBRC NE 0.
*         ROLLBACK WORK.
*         RAISE  ERROR_UPDATE.
*      ENDIF.

      MODIFY ZTIVIT FROM TABLE IT_ZSIVIT.
      IF SY-SUBRC NE 0.
         ROLLBACK WORK.
         RAISE  ERROR_UPDATE.
      ENDIF.

*         SELECT  *   FROM  ZTIVIT
*         WHERE   ZFIVNO  =  ZTCUCLIV-ZFIVNO.
*
*           CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
*                EXPORTING
*                         CURRENCY             = ZTIVIT-ZFIVAMC
*                         AMOUNT_INTERNAL      = ZTIVIT-ZFIVAMT
*                IMPORTING
*                         AMOUNT_EXTERNAL      = W_ZFIVAMT_EX.
*
*           W_ZFIVAMT_IN = W_ZFIVAMT_EX * ( ZTIMIMG06-ZFEXRT /
*                                           ZTIMIMG06-FFACT ).
*
*           CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
*                EXPORTING
*                          CURRENCY             = 'KRW'
*                          AMOUNT_EXTERNAL      = W_ZFIVAMT_IN
*                          MAX_NUMBER_OF_DIGITS = DIGITS
*                IMPORTING
*                          AMOUNT_INTERNAL      = W_ZFIVAMT
*                EXCEPTIONS
*                          OTHERS               = 1.
*
*           MOVE W_ZFIVAMT TO ZTIVIT-ZFIVAMK.
*
*           UPDATE ZTIVIT.
*           IF SY-SUBRC NE 0.
*              ROLLBACK WORK.
*              RAISE  ERROR_UPDATE.
*           ENDIF.
*         ENDSELECT.
*
*         SELECT SUM( ZFIVAMK ) INTO ZTIV-ZFIVAMK
*           FROM ZTIVIT
*          WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
*
*         IF ZFSTATUS = 'C' AND ZTIV-ZFPOYN = 'N'. "무환.
*            MOVE 'Y' TO ZTIV-ZFCDST.
*         ENDIF.

         SELECT SUM( ZFIVAMK ) SUM( ZFIVAMT )
           INTO (ZTIV-ZFIVAMK, ZTIV-ZFIVAMT)
           FROM ZTIVIT
          WHERE ZFIVNO = ZTIDS-ZFIVNO.

         UPDATE ZTIV
         SET    ZFIVAMK = ZTIV-ZFIVAMK
                ZFIVAMT = ZTIV-ZFIVAMT
                ZFEXRT  = ZTIMIMG06-ZFEXRT
                FFACT   = ZTIMIMG06-FFACT
*                ZFIVAMK = ZTIDS-ZFTBAK
                ZFCUST  = 'Y'
                UNAM    = SY-UNAME
                UDAT    = SY-DATUM
         WHERE  ZFIVNO  = ZTIDS-ZFIVNO.
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

   CLEAR : P_ZFDCNM, ZTREQHD, ZTBL, ZTIV, ZTCGHD, ZTMSHD,
           W_SUBRC.

   IF P_ZFIMDNO IS INITIAL.
      EXIT.
   ENDIF.

   CASE ZTBKPF-ZFCSTGRP.
      WHEN '003'.           ">수입의뢰.
         SELECT SINGLE * FROM ZTREQHD
                         WHERE ZFREQNO EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.
         MOVE: ZTREQHD-ZFOPNNO TO P_ZFDCNM.
         P_ZFPOYN = 'Y'.
         " 사업장 GET!
         SELECT SINGLE J_1BBRANCH  INTO  ZTBKPF-BUPLA
         FROM   T001W
         WHERE  WERKS  EQ ZTREQHD-ZFWERKS.

         " 사업영역 GET!
         SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
         FROM   T134G
         WHERE WERKS EQ ZTREQHD-ZFWERKS.

* CORECESS 주석처리.
*         IF W_SUBRC EQ 0.
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTREQHD-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA
*                 EXCEPTIONS
*                    AREA_ERROR = 4.
*            IF SY-SUBRC NE 0.
*               W_SUBRC = 4.
*            ENDIF.
*         ENDIF.
      WHEN '004' OR '005' OR '007'.  ">B/L 관리번호.
         SELECT SINGLE * FROM ZTBL
                         WHERE ZFBLNO  EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.
         IF W_SUBRC EQ 0.
            P_ZFPOYN = ZTBL-ZFPOYN.
            P_KOSTL  = ZTBL-KOSTL.
         ELSE.
            P_ZFPOYN = 'Y'.
         ENDIF.
*> 1. 화물관리번호.
         IF NOT ZTBL-ZFGMNO IS INITIAL.
            MOVE: ZTBL-ZFGMNO TO P_ZFDCNM.
            IF NOT ZTBL-ZFMSN IS INITIAL.
               CONCATENATE P_ZFDCNM '-' ZTBL-ZFMSN INTO P_ZFDCNM.
            ENDIF.
            IF NOT ZTBL-ZFHSN IS INITIAL.
               CONCATENATE P_ZFDCNM '-' ZTBL-ZFHSN INTO P_ZFDCNM.
            ENDIF.
         ELSE.
*> 2. HOUSE B/L No.
            IF NOT ZTBL-ZFHBLNO IS INITIAL.
               MOVE: ZTBL-ZFHBLNO TO P_ZFDCNM.
            ELSE.
*> 3. MASTER B/L No.
               IF NOT ZTBL-ZFMBLNO IS INITIAL.
                  MOVE: ZTBL-ZFMBLNO TO P_ZFDCNM.
               ELSE.
*> 4. 선사 B/L No.
                  IF NOT ZTBL-ZFCGHNO IS INITIAL.
                     MOVE: ZTBL-ZFCGHNO TO P_ZFDCNM.
                  ENDIF.
               ENDIF.
            ENDIF.
         ENDIF.
         " 사업장 GET!
         SELECT SINGLE J_1BBRANCH  INTO  ZTBKPF-BUPLA
         FROM   T001W
         WHERE  WERKS  EQ ZTBL-ZFWERKS.

         " 사업영역 GET!
         SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
         FROM   T134G
         WHERE WERKS EQ ZTBL-ZFWERKS.

* CORECESS 주석처리.
*         IF W_SUBRC EQ 0.
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTBL-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA
*                 EXCEPTIONS
*                    AREA_ERROR = 4.
*            IF SY-SUBRC NE 0.
*               W_SUBRC = 4.
*            ENDIF.
*         ENDIF.
*
      WHEN '006'.           ">통관관리번호.
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
*               SELECT SINGLE * FROM ZTCUCLIV
*                               WHERE ZFIVNO EQ P_ZFIMDNO.
*               IF SY-SUBRC EQ 0.
                  SELECT SINGLE * FROM ZTIDS
                                  WHERE ZFIVNO  EQ P_ZFIMDNO.
                  IF SY-SUBRC EQ 0.
                     IF ZTIDR-ZFIDRNO IS INITIAL.
                        MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
                     ELSE.
                        MOVE: ZTIDS-ZFIDRNO TO P_ZFDCNM.
                     ENDIF.
                  ELSE.
                     MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
                  ENDIF.
*               ELSE.
*                  MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
*               ENDIF.
            ELSE.
               MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
            ENDIF.
         ENDIF.
         SELECT SINGLE * FROM ZTBL
                WHERE ZFBLNO EQ ZTIV-ZFBLNO.
         " 사업장 GET!
         SELECT SINGLE J_1BBRANCH  INTO  ZTBKPF-BUPLA
         FROM   T001W
         WHERE  WERKS  EQ ZTBL-ZFWERKS.

         " 사업영역 GET!
         SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
         FROM   T134G
         WHERE WERKS EQ ZTBL-ZFWERKS.

* CORECESS 주석처리.
*         IF SY-SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*            P_KOSTL  = ZTBL-KOSTL.
*            P_ZFPOYN = ZTBL-ZFPOYN.
*
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTBL-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA
*                 EXCEPTIONS
*                    AREA_ERROR = 4.
*            IF SY-SUBRC NE 0.
*               W_SUBRC = 4.
*            ENDIF.
*         ENDIF.
*
*      WHEN '007'.           ">하역관리번호.
*         SELECT SINGLE * FROM ZTCGHD
*                         WHERE ZFCGNO  EQ P_ZFIMDNO.
*         W_SUBRC = SY-SUBRC.
*         P_ZFPOYN = 'Y'.
*         IF SY-SUBRC EQ 0.
*            IF NOT ZTCGHD-ZFMSNO IS INITIAL.
*               SELECT SINGLE * FROM  ZTMSHD
*                               WHERE ZFMSNO  EQ  ZTCGHD-ZFMSNO.
*               IF SY-SUBRC EQ 0.
*                  MOVE ZTMSHD-ZFMSNM  TO  P_ZFDCNM.
*               ENDIF.
*            ENDIF.
*         ELSE.
* CORECESS 주석처리.
*            IF SY-TCODE NE 'ZIMY3'.
*               CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                    EXPORTING
*                       UNAME   =    SY-UNAME
*                       WERKS   =    ZTREQHD-ZFWERKS
*                    IMPORTING
*                       GSBER   =    ZTBKPF-GSBER
*                       BUPLA   =    ZTBKPF-BUPLA
*                    EXCEPTIONS
*                       AREA_ERROR = 4.
*               IF SY-SUBRC NE 0.
*                  W_SUBRC = 4.
*               ENDIF.
*            ENDIF.
*         ENDIF.
      WHEN '008'.           ">기납증리번호.
         SELECT SINGLE * FROM ZTTAXBKHD
                         WHERE ZFTBNO  EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.
         IF SY-SUBRC EQ 0.
            IF ZTTAXBKHD-BASISNO IS INITIAL.
               MOVE ZTTAXBKHD-EBELN    TO  P_ZFDCNM.
            ELSE.
               MOVE ZTTAXBKHD-BASISNO  TO  P_ZFDCNM.
            ENDIF.
         ELSE.
            CLEAR : P_ZFDCNM.
         ENDIF.
         SELECT SINGLE * FROM ZTREQHD
                WHERE ZFREQNO EQ ZTTAXBKHD-ZFREQNO.
*          IF SY-SUBRC EQ 0.
* CORECESS 주석처리.
*         IF SY-SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTREQHD-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA
*                 EXCEPTIONS
*                    AREA_ERROR = 4.
*            IF SY-SUBRC NE 0.
*               W_SUBRC = 4.
*            ENDIF.
*         ENDIF.
      WHEN OTHERS.
         EXIT.
   ENDCASE.

*>>오류가 발생했을 경우.
   IF W_SUBRC NE 0.
*      IF P_GUBUN EQ 'H'.
*         PERFORM P2000_NO_INPUT USING 'ZTBKPF' 'ZFIMDNO'.
*      ELSE.
*         PERFORM P2000_NO_INPUT USING 'ZSBSEG' 'ZFIMDNO'.
*      ENDIF.
      IF W_EDI NE 'X'.
         CASE ZTBKPF-ZFCSTGRP.
            WHEN '003'.
               MESSAGE E585 WITH '수입의뢰번호' P_ZFIMDNO.
            WHEN '004' OR '005'.
               MESSAGE E585 WITH 'B/L 관리번호' P_ZFIMDNO.
            WHEN '006'.
               MESSAGE E585 WITH '통관관리번호' P_ZFIMDNO.
            WHEN '007'.
               MESSAGE E585 WITH '하역관리번호' P_ZFIMDNO.
            WHEN '008'.
               MESSAGE E585 WITH '기납증관리번호' P_ZFIMDNO.
         ENDCASE.
      ENDIF.
   ENDIF.

ENDFORM.                    " P1000_IMPORT_DOC_CHEKC
