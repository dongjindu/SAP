FUNCTION ZIM_CHARGE_ITEM_CREATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(BUKRS) LIKE  ZTBKPF-BUKRS
*"     VALUE(BELNR) LIKE  ZTBKPF-BELNR
*"     VALUE(GJAHR) LIKE  ZTBKPF-GJAHR
*"     REFERENCE(ZFIMDNO) LIKE  ZTBSEG-ZFIMDNO
*"     VALUE(ZTBKPF) LIKE  ZTBKPF STRUCTURE  ZTBKPF
*"  TABLES
*"      IT_ZSBSEG STRUCTURE  ZSBSEG
*"      IT_ZTBDIV STRUCTURE  ZTBDIV
*"----------------------------------------------------------------------
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  11/10/2011  Valerian  UD1K953701  Overwrite vendor alloc. no. with
*                                    first item of G/L alloc. no.
*----------------------------------------------------------------------
DATA : W_ZFBSEQ      LIKE   ZTBDIV-ZFBSEQ,
       W_BUZEI       LIKE   ZTBDIV-BUZEI,
       L_MENGE       LIKE   ZTBDIV-MENGE,
       L_EXIT,
       L_MENGES      LIKE   ZTBDIV-MENGE,
       L_MENGEC      LIKE   ZTBDIV-MENGE,
       W_ZBUZEI      LIKE   ZTBDIV-ZBUZEI,
       W_ZUONR       LIKE   ZTBDIV-ZUONR,
       W_DMBTR1      LIKE   ZTBKPF-WRBTR,
       W_DMBTR2      LIKE   ZTBKPF-WRBTR,
       W_WRBTR1      LIKE   ZTBKPF-WRBTR,
       W_WRBTR2      LIKE   ZTBKPF-WRBTR,
       W_WRBTR       LIKE   ZTBKPF-WRBTR,
       W_WRBTR_CHA   LIKE   ZTBKPF-WRBTR,
       L_SUBRC       LIKE   SY-SUBRC,
       SV_EBELN      LIKE   ZTBDIV-EBELN,
       SV_EBELP      LIKE   ZTBDIV-EBELP,
       SV_COND       LIKE   ZTBDIV-COND_TYPE,
       W_CNT         TYPE   I.

  REFRESH : IT_ZTBDIV.
  CLEAR : IT_ZTBDIV, W_ZBUZEI.
*------------------------------------------------------------------
*>> HEADER.
*------------------------------------------------------------------
  CLEAR : IT_ZTBDIV, W_ZFBSEQ, W_BUZEI.

  " IMG DATA GET.
  CLEAR : ZTIMIMG00.
  SELECT SINGLE * FROM ZTIMIMG00.

  READ TABLE IT_ZSBSEG INDEX 1.
  ADD 1  TO  W_ZBUZEI.

  MOVE : SY-MANDT         TO    IT_ZTBDIV-MANDT,
         BUKRS            TO    IT_ZTBDIV-BUKRS,
         GJAHR            TO    IT_ZTBDIV-GJAHR,
         BELNR            TO    IT_ZTBDIV-BELNR,
         IT_ZSBSEG-BUZEI  TO    IT_ZTBDIV-BUZEI,
         IT_ZSBSEG-DBUZEI TO    IT_ZTBDIV-DBUZEI,
         W_ZBUZEI         TO    IT_ZTBDIV-ZBUZEI,
         W_ZFBSEQ         TO    IT_ZTBDIV-ZFBSEQ,
         ZTBKPF-ZFCSTGRP  TO    IT_ZTBDIV-ZFCSTGRP,
         ZTBKPF-ZFDCSTX   TO    IT_ZTBDIV-ZFDCSTX,
         '31'             TO    IT_ZTBDIV-NEWBS,
         ZTBKPF-LIFNR     TO    IT_ZTBDIV-NEWKO,
         ZTBKPF-AKONT     TO    IT_ZTBDIV-AKONT,
         ZTBKPF-ZTERM     TO    IT_ZTBDIV-ZTERM,
         ZTBKPF-XMWST     TO    IT_ZTBDIV-XMWST,
         ZTBKPF-MWSKZ     TO    IT_ZTBDIV-MWSKZ,
         ZTBKPF-ZFBDT     TO    IT_ZTBDIV-ZFBDT,
         ZTBKPF-BUPLA     TO    IT_ZTBDIV-BUPLA,
         ZTBKPF-GSBER     TO    IT_ZTBDIV-GSBER,
         ZTBKPF-WAERS     TO    IT_ZTBDIV-WAERS,
         ZTBKPF-HWAER     TO    IT_ZTBDIV-HWAER,
         ZTBKPF-KURSF     TO    IT_ZTBDIV-KURSF,
         ZTBKPF-WWERT     TO    IT_ZTBDIV-WWERT,
         ZTBKPF-WRBTR     TO    IT_ZTBDIV-WRBTR,
         ZTBKPF-DMBTR     TO    IT_ZTBDIV-DMBTR,
         ZTBKPF-WMWST     TO    IT_ZTBDIV-WMWST.

  IF ZTBKPF-ZFRVSX EQ 'X'.
     MOVE: '21'            TO    IT_ZTBDIV-NEWBS.
  ENDIF.

*> ITEM TEXT.
  PERFORM   GET_DD07T_SELECT(SAPMZIM01)
            USING 'ZDCSTGRP'  ZTBKPF-ZFCSTGRP
            CHANGING          IT_ZTBDIV-SGTXT.

*>> Expense Code Description
  READ TABLE IT_ZSBSEG  INDEX  1.
  CLEAR : ZTIMIMG08.
  SELECT  SINGLE * FROM ZTIMIMG08
  WHERE   ZFCDTY   EQ   ZTBKPF-ZFCSTGRP
  AND     ZFCD     EQ   IT_ZSBSEG-ZFCD.
  MOVE  ZTIMIMG08-ZFCDNM(18)  TO  IT_ZTBDIV-ZUONR.

* Overwrite vendor allocation no. with G/L allocation no.   "UD1K953701
  MOVE  IT_ZSBSEG-ZUONR TO IT_ZTBDIV-ZUONR.                 "UD1K953701

  IF IT_ZTBDIV-ZUONR IS INITIAL.
     IT_ZTBDIV-ZUONR = IT_ZTBDIV-SGTXT.
  ENDIF.

  CLEAR : IT_ZTBDIV-KOSTL, IT_ZTBDIV-PRCTR.

  APPEND IT_ZTBDIV.

*> INITIALIZATION.
  REFRESH : IT_ZSREQIT, IT_ZSBLIT, IT_ZSIVIT, IT_ZSCGIT, IT_ZSTRIT.

  LOOP AT IT_ZSBSEG.
*------------------------------------------------------
*> Distribution ITEM SELECT.
*------------------------------------------------------
     CASE IT_ZSBSEG-ZFCSTGRP.
        WHEN  '003'.
           SELECT * INTO CORRESPONDING
                    FIELDS OF TABLE IT_ZSREQIT
                    FROM  ZTREQIT
                    WHERE ZFREQNO   EQ   IT_ZSBSEG-ZFIMDNO
                    AND   MENGE     NE   0.
        WHEN  '004' OR '005' .
           SELECT SINGLE * FROM ZTBL
                  WHERE ZFBLNO   EQ   IT_ZSBSEG-ZFIMDNO.
           *ZTBL = ZTBL.
           CASE *ZTBL-ZFPOYN.
              WHEN 'Y'.    ">Monetary
                 SELECT * INTO CORRESPONDING
                          FIELDS OF TABLE IT_ZSBLIT
                          FROM  ZTBLIT
                          WHERE ZFBLNO   EQ   IT_ZSBSEG-ZFIMDNO
                          AND   BLMENGE  NE   0
                          AND   BLOEKZ   EQ   SPACE.
              WHEN 'M'.    ">Combine
                 SELECT * INTO CORRESPONDING
                          FIELDS OF TABLE IT_ZSBLIT
                          FROM  ZTBLIT
                          WHERE ZFBLNO   EQ   IT_ZSBSEG-ZFIMDNO
                          AND   BLMENGE  NE   0
                          AND ( ZFPOTY   IS   NULL
                          OR    ZFPOTY   EQ   SPACE )
                          AND   BLOEKZ   EQ   SPACE.
              WHEN 'N'.     ">Non-monetary
                 SELECT * INTO CORRESPONDING
                          FIELDS OF TABLE IT_ZSBLIT
                          FROM  ZTBLIT
                          WHERE ZFBLNO   EQ   IT_ZSBSEG-ZFIMDNO
                          AND   BLMENGE  NE   0
                          AND NOT ( MATNR    IS   NULL
                          OR        MATNR    EQ   SPACE )
                          AND   BLOEKZ   EQ   SPACE.
                 IF SY-SUBRC NE 0.
                    SELECT * INTO CORRESPONDING
                             FIELDS OF TABLE IT_ZSBLIT
                             FROM  ZTBLIT UP TO 1 ROWS
                             WHERE ZFBLNO   EQ   IT_ZSBSEG-ZFIMDNO
                             AND   BLOEKZ   EQ   SPACE.
                 ENDIF.
           ENDCASE.
        WHEN  '006'.
           SELECT SINGLE * FROM ZTIV
                  WHERE ZFIVNO   EQ     IT_ZSBSEG-ZFIMDNO.

           *ZTIV = ZTIV.

           CASE *ZTIV-ZFPOYN.
              WHEN 'Y'.    ">Monetary
                 SELECT * INTO CORRESPONDING
                          FIELDS OF TABLE IT_ZSIVIT
                          FROM  ZTIVIT
                          WHERE ZFIVNO   EQ   ZTIV-ZFIVNO
                          AND   CCMENGE  NE   0.
              WHEN 'M'.    ">Combine
                 SELECT * INTO CORRESPONDING
                          FIELDS OF TABLE IT_ZSIVIT
                          FROM  ZTIVIT
                          WHERE ZFIVNO   EQ   ZTIV-ZFIVNO
                          AND   CCMENGE  NE   0
                          AND ( ZFPOTY   IS   NULL
                          OR    ZFPOTY   EQ   SPACE ).
              WHEN 'N'.     ">Non-monetary
                 SELECT * INTO CORRESPONDING
                          FIELDS OF TABLE IT_ZSIVIT
                          FROM  ZTIVIT
                          WHERE ZFIVNO   EQ   ZTIV-ZFIVNO
                          AND   CCMENGE  NE   0
                          AND NOT ( MATNR    IS   NULL
                          OR        MATNR    EQ   SPACE ).
                 IF SY-SUBRC NE 0.
                    SELECT * INTO CORRESPONDING
                             FIELDS OF TABLE IT_ZSIVIT
                             FROM  ZTIVIT UP TO 1 ROWS
                             WHERE ZFIVNO   EQ   ZFIMDNO.
                 ENDIF.
           ENDCASE.
        WHEN OTHERS.
           CONTINUE.
     ENDCASE.

     IF SY-SUBRC NE 0.   CONTINUE.   ENDIF.

*-----------------------------------------------------------------------
*> TOTAL AMOUNT CALCULATE.
*-----------------------------------------------------------------------
     CLEAR : W_AMOUNT, W_COUNT.
     W_GUBUN  = 'N'.
     CASE IT_ZSBSEG-ZFCSTGRP.
        WHEN  '003'.
           LOOP AT IT_ZSREQIT.
              W_TABIX = SY-TABIX.
              CLEAR : EKKO, EKPO, KONV.

              SELECT SINGLE * FROM EKKO
              WHERE  EBELN EQ IT_ZSREQIT-EBELN.
              W_SUBRC  =  SY-SUBRC.

              SELECT SINGLE * FROM EKPO
                          WHERE EBELN  EQ   IT_ZSREQIT-EBELN
                          AND   EBELP  EQ   IT_ZSREQIT-EBELP.

              " Condition Master Get.
              CLEAR : KONV.
              SELECT SINGLE * FROM KONV
              WHERE  KNUMV    EQ   EKKO-KNUMV
              AND    KPOSN    EQ   EKPO-EBELP
              AND    KSCHL    EQ   IT_ZSBSEG-COND_TYPE.
              IF SY-SUBRC NE 0 OR KONV-KBETR LE 0.
                 IF EKPO-KNTTP IS  INITIAL.
                    DELETE  IT_ZSREQIT  INDEX  W_TABIX.
                    CONTINUE.
                 ENDIF.
              ENDIF.

              " Final Invoice Receip or No Invoice Receipt
              IF EKPO-EREKZ EQ 'X' OR EKPO-REPOS IS INITIAL.
                 DELETE  IT_ZSREQIT  INDEX  W_TABIX.
                 CONTINUE.
              ENDIF.

              MOVE : EKKO-WAERS   TO    IT_ZSREQIT-WAERS,
                     EKPO-BPUMZ   TO    IT_ZSREQIT-BPUMZ,
                     EKPO-BPUMN   TO    IT_ZSREQIT-BPUMN,
                     EKPO-WERKS   TO    IT_ZSREQIT-WERKS,
                     KONV-KBETR   TO    IT_ZSREQIT-KBETR,
                     EKKO-KNUMV   TO    IT_ZSREQIT-KNUMV.

              IF W_SUBRC EQ 0.
                 W_AMOUNT_TMP = ( IT_ZSREQIT-MENGE *
                             ( IT_ZSREQIT-BPUMZ / IT_ZSREQIT-BPUMN ) *
                             ( IT_ZSREQIT-NETPR / IT_ZSREQIT-PEINH ) ).

                 IF ZTBKPF-ZFDCSTX IS INITIAL OR EKPO-KNTTP NE SPACE.
                    CLEAR : IT_ZSREQIT-KBETR.
                    W_AMOUNT = W_AMOUNT + W_AMOUNT_TMP.
                    IF W_AMOUNT_TMP LE 0.
                       W_GUBUN = 'Y'.
                    ENDIF.
                 ELSE.
                    W_AMOUNT_TMP = W_AMOUNT_TMP * ( KONV-KBETR / 100 ).
                    W_AMOUNT     = W_AMOUNT + W_AMOUNT_TMP.
                 ENDIF.
                 MODIFY IT_ZSREQIT INDEX  W_TABIX.
                 ADD 1 TO W_COUNT.
              ELSE.
                 DELETE IT_ZSREQIT INDEX W_TABIX.
              ENDIF.
           ENDLOOP.
        WHEN  '004' OR '005' .
           LOOP AT IT_ZSBLIT.
              W_TABIX = SY-TABIX.

              CLEAR : EKKO, EKPO, KONV.
              SELECT SINGLE * FROM EKKO
              WHERE  EBELN    EQ   IT_ZSBLIT-EBELN.

              SELECT SINGLE * FROM EKPO
                          WHERE EBELN  EQ   IT_ZSBLIT-EBELN
                          AND   EBELP  EQ   IT_ZSBLIT-EBELP.

              L_SUBRC = SY-SUBRC.

              IF IT_ZSBLIT-EBELN IS INITIAL.
                 L_SUBRC = 4.
              ENDIF.

              IF EKPO-KNTTP IS INITIAL AND L_SUBRC EQ 0.
                 " Condition Master Get.
                 SELECT SINGLE * FROM KONV
                 WHERE  KNUMV    EQ   EKKO-KNUMV
                 AND    KPOSN    EQ   EKPO-EBELP
                 AND    KSCHL    EQ   IT_ZSBSEG-COND_TYPE.
                 IF SY-SUBRC NE 0 OR KONV-KBETR LE 0.
                    DELETE  IT_ZSBLIT  INDEX  W_TABIX.
                    CONTINUE.
                 ENDIF.
              ENDIF.

              " Final Invoice Receipt or No Invoice Receipt
              IF ( EKPO-EREKZ EQ 'X' OR EKPO-REPOS IS INITIAL ) AND
                   L_SUBRC EQ 0.
                 DELETE  IT_ZSBLIT  INDEX  W_TABIX.
                 CONTINUE.
              ENDIF.

              MOVE : EKKO-WAERS   TO    IT_ZSBLIT-WAERS,
                     EKPO-BPUMZ   TO    IT_ZSBLIT-BPUMZ,
                     EKPO-BPUMN   TO    IT_ZSBLIT-BPUMN,
                     KONV-KBETR   TO    IT_ZSBLIT-KBETR,
                     EKKO-KNUMV   TO    IT_ZSBLIT-KNUMV.
              IF IT_ZSBLIT-BPUMN IS INITIAL.
                 IT_ZSBLIT-BPUMN = 1.
              ENDIF.
              IF IT_ZSBLIT-PEINH IS INITIAL.
                 IT_ZSBLIT-PEINH = 1.
              ENDIF.
              IF L_SUBRC EQ 0.
                 W_AMOUNT_TMP = ( IT_ZSBLIT-BLMENGE *
                             ( IT_ZSBLIT-BPUMZ / IT_ZSBLIT-BPUMN ) *
                             ( IT_ZSBLIT-NETPR / IT_ZSBLIT-PEINH ) ).
                 IF ZTBKPF-ZFDCSTX IS INITIAL OR EKPO-KNTTP NE SPACE.
                    CLEAR : IT_ZSBLIT-KBETR.
                    W_AMOUNT = W_AMOUNT + W_AMOUNT_TMP.
                    IF W_AMOUNT_TMP LE 0.
                       W_GUBUN = 'Y'.
                    ENDIF.
                 ELSE.
                    W_AMOUNT_TMP = W_AMOUNT_TMP * ( KONV-KBETR / 100 ).
                    W_AMOUNT     = W_AMOUNT + W_AMOUNT_TMP.
                 ENDIF.

                 MODIFY IT_ZSBLIT INDEX  W_TABIX.
                 ADD 1 TO W_COUNT.
              ELSE.
                 IF *ZTBL-ZFPOYN EQ 'M'.
                    DELETE IT_ZSBLIT INDEX W_TABIX.
                    CONTINUE.
                 ELSE.
                    IT_ZSBLIT-BPUMZ = 1.
                    IT_ZSBLIT-BPUMN = 1.
                    ADD 1 TO W_COUNT.
                    W_GUBUN = 'Y'.
                    MODIFY IT_ZSBLIT INDEX  W_TABIX.
                 ENDIF.
              ENDIF.
           ENDLOOP.
        WHEN  '006'.
           LOOP AT IT_ZSIVIT.
              W_TABIX = SY-TABIX.
              CLEAR : EKKO, EKPO, KONV.
              SELECT SINGLE * FROM EKKO
              WHERE  EBELN    EQ   IT_ZSIVIT-EBELN.

              SELECT SINGLE * FROM EKPO
                     WHERE EBELN  EQ   IT_ZSIVIT-EBELN
                     AND   EBELP  EQ   IT_ZSIVIT-EBELP.

              L_SUBRC = SY-SUBRC.

              IF EKPO-KNTTP IS INITIAL AND L_SUBRC EQ 0.
                 " Condition Master Get.
                 SELECT SINGLE * FROM KONV
                 WHERE  KNUMV    EQ   EKKO-KNUMV
                 AND    KPOSN    EQ   EKPO-EBELP
                 AND    KSCHL    EQ   IT_ZSBSEG-COND_TYPE.
                 IF SY-SUBRC NE 0 OR KONV-KBETR LE 0.
                    DELETE  IT_ZSIVIT  INDEX  W_TABIX.
                    CONTINUE.
                 ENDIF.
              ENDIF.

              MOVE : EKKO-WAERS   TO    IT_ZSIVIT-WAERS,
                     EKPO-BPUMZ   TO    IT_ZSIVIT-BPUMZ,
                     EKPO-BPUMN   TO    IT_ZSIVIT-BPUMN,
                     KONV-KBETR   TO    IT_ZSIVIT-KBETR,
                     EKKO-KNUMV   TO    IT_ZSIVIT-KNUMV.

              IF IT_ZSIVIT-BPUMN IS INITIAL.
                 IT_ZSIVIT-BPUMN = 1.
              ENDIF.
              IF IT_ZSIVIT-PEINH IS INITIAL.
                 IT_ZSIVIT-PEINH = 1.
              ENDIF.
              IF L_SUBRC EQ 0.
                 W_AMOUNT_TMP = ( IT_ZSIVIT-CCMENGE *
                            ( IT_ZSIVIT-BPUMZ / IT_ZSIVIT-BPUMN ) *
                            ( IT_ZSIVIT-NETPR / IT_ZSIVIT-PEINH ) ).
                 IF ZTBKPF-ZFDCSTX IS INITIAL OR EKPO-KNTTP NE SPACE.
                    CLEAR : IT_ZSIVIT-KBETR.
                    W_AMOUNT = W_AMOUNT + W_AMOUNT_TMP.
                    IF W_AMOUNT_TMP LE 0.
                       W_GUBUN = 'Y'.
                    ENDIF.
                 ELSE.
                    W_AMOUNT_TMP = W_AMOUNT_TMP * ( KONV-KBETR / 100 ).
                    W_AMOUNT     = W_AMOUNT_TMP + W_AMOUNT.
                 ENDIF.

                 MODIFY IT_ZSIVIT INDEX  W_TABIX.
                 ADD 1 TO W_COUNT.
              ELSE.
                 IF *ZTIV-ZFPOYN EQ 'M'.
                    DELETE IT_ZSIVIT INDEX W_TABIX.
                    CONTINUE.
                 ENDIF.
                 ADD 1 TO W_COUNT.
                 MOVE : 1   TO   IT_ZSIVIT-BPUMZ,
                        1   TO   IT_ZSIVIT-BPUMN.
                 W_GUBUN = 'Y'.
                 MODIFY IT_ZSIVIT INDEX  W_TABIX.
              ENDIF.
           ENDLOOP.
        WHEN OTHERS.
     ENDCASE.

*------------------------------------------------
* Expense Distribution
*------------------------------------------------
     W_AMOUNT_WON = 0.  W_AMOUNT_USD = 0.
     CLEAR : W_ZFBSEQ.
     CASE IT_ZSBSEG-ZFCSTGRP.
        WHEN  '003'.
           LOOP AT IT_ZSREQIT.
              ADD 1 TO W_ZFBSEQ.
              ADD 1 TO W_ZBUZEI.
              CLEAR : IT_ZTBDIV.

              IT_ZTBDIV-ZFAMT =  ( IT_ZSREQIT-MENGE *
                          ( IT_ZSREQIT-BPUMZ / IT_ZSREQIT-BPUMN ) *
                          ( IT_ZSREQIT-NETPR / IT_ZSREQIT-PEINH ) ).
              IF IT_ZSREQIT-KBETR GT 0.
                 IT_ZTBDIV-ZFAMT = IT_ZTBDIV-ZFAMT *
                                   ( IT_ZSREQIT-KBETR / 100 ).
              ENDIF.

              IF W_AMOUNT LE 0 OR W_GUBUN EQ 'Y'.
                IT_ZTBDIV-WRBTR = ( 1 / W_COUNT )
                                * IT_ZSBSEG-WRBTR.

                IT_ZTBDIV-DMBTR = ( 1 / W_COUNT )
                                * IT_ZSBSEG-DMBTR.
              ELSE.
                IT_ZTBDIV-WRBTR = ( IT_ZTBDIV-ZFAMT / W_AMOUNT )
                                * IT_ZSBSEG-WRBTR.

                 IT_ZTBDIV-DMBTR = ( IT_ZTBDIV-ZFAMT / W_AMOUNT )
                                 * IT_ZSBSEG-DMBTR.
               ENDIF.

               IF IT_ZTBDIV-WRBTR EQ 0 OR IT_ZTBDIV-DMBTR EQ 0.
                  CONTINUE.
               ENDIF.

               " Business Place Get.
               SELECT SINGLE J_1BBRANCH INTO IT_ZTBDIV-BUPLA
                      FROM T001W
                      WHERE WERKS EQ IT_ZSREQIT-WERKS.

               IF NOT IT_ZSREQIT-MATNR IS INITIAL.
                  SELECT SINGLE * FROM  MARC
                                  WHERE MATNR EQ IT_ZSREQIT-MATNR
                                  AND   WERKS EQ IT_ZSREQIT-WERKS.
                  MOVE : MARC-PRCTR   TO  IT_ZTBDIV-PRCTR.

                  SELECT SINGLE * FROM MARA
                                  WHERE MATNR EQ IT_ZSREQIT-MATNR.
                  IF SY-SUBRC EQ 0 AND
                     NOT MARA-SPART IS INITIAL.
                     SELECT SINGLE GSBER INTO  IT_ZTBDIV-GSBER
                            FROM  T134G
                            WHERE WERKS EQ IT_ZSREQIT-WERKS
                            AND   SPART EQ MARA-SPART.
                  ELSE.
                     SELECT MAX( GSBER ) INTO IT_ZTBDIV-GSBER
                                 FROM T134G
                                 WHERE WERKS EQ IT_ZSREQIT-WERKS.
                  ENDIF.

               ELSEIF NOT IT_ZSBSEG-KOSTL IS INITIAL.
                  SELECT MAX( GSBER ) INTO IT_ZTBDIV-GSBER
                              FROM T134G
                              WHERE WERKS EQ IT_ZSREQIT-WERKS.

                  SELECT SINGLE * FROM TKA02
                              WHERE BUKRS EQ BUKRS
                              AND   GSBER EQ IT_ZTBDIV-GSBER.

                  SELECT * FROM  CSKS UP TO 1 ROWS
                           WHERE KOKRS  EQ TKA02-KOKRS
                           AND   KOSTL  EQ IT_ZSBSEG-KOSTL
                           AND   DATBI  GE SY-DATUM
                           AND   DATAB  LE SY-DATUM.
                     MOVE : CSKS-PRCTR   TO  IT_ZTBDIV-PRCTR.
                  ENDSELECT.
               ENDIF.

               MOVE : SY-MANDT            TO    IT_ZTBDIV-MANDT,
                      BUKRS               TO    IT_ZTBDIV-BUKRS,
                      GJAHR               TO    IT_ZTBDIV-GJAHR,
                      BELNR               TO    IT_ZTBDIV-BELNR,
                      IT_ZSBSEG-BUZEI     TO    IT_ZTBDIV-BUZEI,
                      IT_ZSBSEG-DBUZEI    TO    IT_ZTBDIV-DBUZEI,
                      W_ZBUZEI            TO    IT_ZTBDIV-ZBUZEI,
                      W_ZFBSEQ            TO    IT_ZTBDIV-ZFBSEQ,
                      ZTBKPF-ZFDCSTX      TO    IT_ZTBDIV-ZFDCSTX,
                      IT_ZSBSEG-COND_TYPE TO    IT_ZTBDIV-COND_TYPE,
                      ZTBKPF-ZFCSTGRP     TO    IT_ZTBDIV-ZFCSTGRP,
                      IT_ZSBSEG-ZFCD      TO    IT_ZTBDIV-ZFCD,
                      IT_ZSBSEG-ZFPOYN    TO    IT_ZTBDIV-ZFPOYN,
                      IT_ZSREQIT-ZFREQNO  TO    IT_ZTBDIV-ZFIMDNO,
                      IT_ZSREQIT-ZFITMNO  TO    IT_ZTBDIV-ZFIMDIT,
                      IT_ZSREQIT-EBELN    TO    IT_ZTBDIV-EBELN,
                      IT_ZSREQIT-EBELP    TO    IT_ZTBDIV-EBELP,
                      IT_ZSREQIT-MATNR    TO    IT_ZTBDIV-MATNR,
                      IT_ZSREQIT-WERKS    TO    IT_ZTBDIV-WERKS,
                      IT_ZSREQIT-MENGE    TO    IT_ZTBDIV-MENGE,
                      IT_ZSREQIT-MEINS    TO    IT_ZTBDIV-MEINS,
                      IT_ZSREQIT-NETPR    TO    IT_ZTBDIV-NETPR,
                      IT_ZSREQIT-PEINH    TO    IT_ZTBDIV-PEINH,
                      IT_ZSREQIT-BPRME    TO    IT_ZTBDIV-BPRME,
                      IT_ZSREQIT-WAERS    TO    IT_ZTBDIV-WAERS1,
                      IT_ZSREQIT-BPUMN    TO    IT_ZTBDIV-BPUMN,
                      IT_ZSREQIT-BPUMN    TO    IT_ZTBDIV-BPUMZ,
                      IT_ZSREQIT-MATNR    TO    IT_ZTBDIV-MATNR,
                      IT_ZSREQIT-TXZ01    TO    IT_ZTBDIV-TXZ01,
                      IT_ZSBSEG-NEWBS     TO    IT_ZTBDIV-NEWBS,
                      IT_ZSBSEG-NEWKO     TO    IT_ZTBDIV-NEWKO,
                      SPACE               TO    IT_ZTBDIV-AKONT,
                      ZTBKPF-ZTERM        TO    IT_ZTBDIV-ZTERM,
                      ZTBKPF-XMWST        TO    IT_ZTBDIV-XMWST,
                      IT_ZSBSEG-MWSKZ     TO    IT_ZTBDIV-MWSKZ,
                      ZTBKPF-ZFBDT        TO    IT_ZTBDIV-ZFBDT,
                      ZTBKPF-WAERS        TO    IT_ZTBDIV-WAERS,
                      ZTBKPF-HWAER        TO    IT_ZTBDIV-HWAER,
                      IT_ZSBSEG-KOSTL     TO    IT_ZTBDIV-KOSTL,
                      IT_ZSBSEG-PS_POSID  TO    IT_ZTBDIV-PS_POSID,
                      IT_ZSBSEG-KURSF     TO    IT_ZTBDIV-KURSF,
                      IT_ZSBSEG-WWERT     TO    IT_ZTBDIV-WWERT,
                      IT_ZSBSEG-ZUONR     TO    IT_ZTBDIV-ZUONR,
*                      IT_ZSBSEG-SGTXT     TO    IT_ZTBDIV-SGTXT,
                      IT_ZSREQIT-KNUMV    TO    IT_ZTBDIV-KNUMV.
*ANDY reqeust
               CONCATENATE IT_ZSBSEG-ZFCSTGRP '-' IT_ZSBSEG-ZFCD
                           ':' IT_ZSBSEG-SGTXT
                      INTO IT_ZTBDIV-SGTXT  .
*              CONCATENATE IT_ZTBDIV-MATNR IT_ZSBSEG-SGTXT(30) INTO
*                      IT_ZTBDIV-SGTXT  SEPARATED BY SPACE.

              " Import Expense -> Subseqent Debit Usage.
              IF ZTIMIMG00-ZFCSTMD EQ 'P'.
                 PERFORM  P1000_SUBSEQUENT_YN USING
                                      IT_ZTBDIV-COND_TYPE
                                      IT_ZTBDIV-EBELN
                                      IT_ZTBDIV-EBELP
                                      IT_ZTBDIV-TBTKZ
                                      IT_ZTBDIV-ZFCSTGRP
                                      IT_ZTBDIV-ZFIMDNO
                                      IT_ZTBDIV-ZFIMDIT.
              ENDIF.

              " Account Number Get.
              IF IT_ZTBDIV-NEWKO IS INITIAL.
                 PERFORM P1000_GETDATA_NEWKO USING IT_ZTBDIV-BUKRS
                                                   IT_ZTBDIV-ZFCSTGRP
                                                   IT_ZTBDIV-ZFCD
                                                   IT_ZTBDIV-COND_TYPE
                                                   IT_ZTBDIV-EBELN
                                                   IT_ZTBDIV-EBELP
                                                   IT_ZTBDIV-WERKS
                                                   IT_ZTBDIV-NEWKO
                                                   IT_ZTBDIV-PRCTR.
               ENDIF.

               ADD   IT_ZTBDIV-WRBTR  TO   W_AMOUNT_USD.
               ADD   IT_ZTBDIV-DMBTR  TO   W_AMOUNT_WON.

               APPEND IT_ZTBDIV.
           ENDLOOP.
        WHEN  '004' OR '005'.
           LOOP AT IT_ZSBLIT.
               ADD 1 TO W_ZFBSEQ.
               ADD 1 TO W_ZBUZEI.
               CLEAR : IT_ZTBDIV.

               IT_ZTBDIV-ZFAMT =  ( IT_ZSBLIT-BLMENGE *
                         ( IT_ZSBLIT-BPUMZ / IT_ZSBLIT-BPUMN ) *
                         ( IT_ZSBLIT-NETPR / IT_ZSBLIT-PEINH ) ).

               IF IT_ZSBLIT-KBETR GT 0.
                  IT_ZTBDIV-ZFAMT = IT_ZTBDIV-ZFAMT *
                                    ( IT_ZSBLIT-KBETR / 100 ).
               ENDIF.

               IF W_AMOUNT LE 0 OR W_GUBUN EQ 'Y'.
                 IT_ZTBDIV-WRBTR = ( 1 / W_COUNT )
                                 * IT_ZSBSEG-WRBTR.

                 IT_ZTBDIV-DMBTR = ( 1 / W_COUNT )
                                 * IT_ZSBSEG-DMBTR.
               ELSE.
                  IT_ZTBDIV-WRBTR = ( IT_ZTBDIV-ZFAMT / W_AMOUNT )
                                  * IT_ZSBSEG-WRBTR.

                  IT_ZTBDIV-DMBTR = ( IT_ZTBDIV-ZFAMT / W_AMOUNT )
                                  * IT_ZSBSEG-DMBTR.
               ENDIF.

               IF IT_ZTBDIV-WRBTR EQ 0 OR IT_ZTBDIV-DMBTR EQ 0.
                  CONTINUE.
               ENDIF.

               "Business Place
               SELECT SINGLE J_1BBRANCH INTO IT_ZTBDIV-BUPLA
                      FROM T001W
                      WHERE WERKS EQ IT_ZSBLIT-WERKS.

               " Business Area
               IF NOT IT_ZSBLIT-MATNR IS INITIAL.
                  SELECT SINGLE * FROM  MARC
                                  WHERE MATNR EQ IT_ZSBLIT-MATNR
                                  AND   WERKS EQ IT_ZSBLIT-WERKS.
                  MOVE : MARC-PRCTR   TO  IT_ZTBDIV-PRCTR.

                  SELECT SINGLE * FROM MARA
                                  WHERE MATNR EQ IT_ZSBLIT-MATNR.

                  IF SY-SUBRC EQ 0 AND
                     NOT MARA-SPART IS INITIAL.
                     SELECT SINGLE GSBER INTO  IT_ZTBDIV-GSBER
                            FROM  T134G
                            WHERE WERKS EQ IT_ZSBLIT-WERKS
                            AND   SPART EQ MARA-SPART.
                  ELSE.
                     SELECT MAX( GSBER ) INTO IT_ZTBDIV-GSBER
                                 FROM T134G
                                 WHERE WERKS EQ IT_ZSBLIT-WERKS.
                  ENDIF.

               ELSE.
                  IF NOT IT_ZSBSEG-KOSTL IS INITIAL.
                     SELECT MAX( GSBER ) INTO IT_ZTBDIV-GSBER
                            FROM T134G
                            WHERE WERKS EQ IT_ZSBLIT-WERKS.

                     SELECT SINGLE * FROM TKA02
                            WHERE BUKRS EQ BUKRS
                            AND   GSBER EQ IT_ZTBDIV-GSBER.

                      SELECT * FROM  CSKS UP TO 1 ROWS
                             WHERE KOKRS  EQ TKA02-KOKRS
                             AND   KOSTL  EQ IT_ZSBSEG-KOSTL
                             AND   DATBI  GE SY-DATUM
                             AND   DATAB  LE SY-DATUM.
                       MOVE : CSKS-PRCTR   TO  IT_ZTBDIV-PRCTR.
                    ENDSELECT.
                  ELSE.

                  ENDIF.

               ENDIF.

               MOVE : SY-MANDT            TO    IT_ZTBDIV-MANDT,
                      BUKRS               TO    IT_ZTBDIV-BUKRS,
                      GJAHR               TO    IT_ZTBDIV-GJAHR,
                      BELNR               TO    IT_ZTBDIV-BELNR,
                      IT_ZSBSEG-BUZEI     TO    IT_ZTBDIV-BUZEI,
                      IT_ZSBSEG-DBUZEI    TO    IT_ZTBDIV-DBUZEI,
                      W_ZBUZEI            TO    IT_ZTBDIV-ZBUZEI,
                      W_ZFBSEQ            TO    IT_ZTBDIV-ZFBSEQ,
                      ZTBKPF-ZFDCSTX      TO    IT_ZTBDIV-ZFDCSTX,
                      IT_ZSBSEG-COND_TYPE TO    IT_ZTBDIV-COND_TYPE,
                      ZTBKPF-ZFCSTGRP     TO    IT_ZTBDIV-ZFCSTGRP,
                      IT_ZSBSEG-ZFCD      TO    IT_ZTBDIV-ZFCD,
                      IT_ZSBSEG-ZFPOYN    TO    IT_ZTBDIV-ZFPOYN,
                      IT_ZSBLIT-ZFBLNO    TO    IT_ZTBDIV-ZFIMDNO,
                      IT_ZSBLIT-ZFBLIT    TO    IT_ZTBDIV-ZFIMDIT,
                      IT_ZSBLIT-EBELN     TO    IT_ZTBDIV-EBELN,
                      IT_ZSBLIT-EBELP     TO    IT_ZTBDIV-EBELP,
                      IT_ZSBLIT-MATNR     TO    IT_ZTBDIV-MATNR,
                      IT_ZSBLIT-WERKS     TO    IT_ZTBDIV-WERKS,
                      IT_ZSBLIT-BLMENGE   TO    IT_ZTBDIV-MENGE,
                      IT_ZSBLIT-MEINS     TO    IT_ZTBDIV-MEINS,
                      IT_ZSBLIT-NETPR     TO    IT_ZTBDIV-NETPR,
                      IT_ZSBLIT-PEINH     TO    IT_ZTBDIV-PEINH,
                      IT_ZSBLIT-BPRME     TO    IT_ZTBDIV-BPRME,
                      IT_ZSBLIT-WAERS     TO    IT_ZTBDIV-WAERS1,
                      IT_ZSBLIT-BPUMN     TO    IT_ZTBDIV-BPUMN,
                      IT_ZSBLIT-BPUMN     TO    IT_ZTBDIV-BPUMZ,
                      IT_ZSBLIT-MATNR     TO    IT_ZTBDIV-MATNR,
                      IT_ZSBLIT-TXZ01     TO    IT_ZTBDIV-TXZ01,
                      IT_ZSBSEG-NEWBS     TO    IT_ZTBDIV-NEWBS,
                      IT_ZSBSEG-NEWKO     TO    IT_ZTBDIV-NEWKO,
                      SPACE               TO    IT_ZTBDIV-AKONT,
                      ZTBKPF-ZTERM        TO    IT_ZTBDIV-ZTERM,
                      ZTBKPF-XMWST        TO    IT_ZTBDIV-XMWST,
                      IT_ZSBSEG-MWSKZ     TO    IT_ZTBDIV-MWSKZ,
                      ZTBKPF-ZFBDT        TO    IT_ZTBDIV-ZFBDT,
                      ZTBKPF-WAERS        TO    IT_ZTBDIV-WAERS,
                      ZTBKPF-HWAER        TO    IT_ZTBDIV-HWAER,
                      IT_ZSBSEG-KOSTL     TO    IT_ZTBDIV-KOSTL,
                      IT_ZSBSEG-PS_POSID  TO    IT_ZTBDIV-PS_POSID,
                      IT_ZSBSEG-KURSF     TO    IT_ZTBDIV-KURSF,
                      IT_ZSBSEG-WWERT     TO    IT_ZTBDIV-WWERT,
                      IT_ZSBSEG-ZUONR     TO    IT_ZTBDIV-ZUONR,
*                      IT_ZSBSEG-SGTXT     TO    IT_ZTBDIV-SGTXT,
                      IT_ZSBLIT-KNUMV     TO    IT_ZTBDIV-KNUMV.

*ANDY reqeust
               CONCATENATE IT_ZSBSEG-ZFCSTGRP '-' IT_ZSBSEG-ZFCD
                           ':' IT_ZSBSEG-SGTXT
                      INTO IT_ZTBDIV-SGTXT  .
*              CONCATENATE IT_ZTBDIV-MATNR IT_ZSBSEG-SGTXT(30) INTO
*                      IT_ZTBDIV-SGTXT  SEPARATED BY SPACE.

              " Import Expense -> Subsequent Debit
              IF ZTIMIMG00-ZFCSTMD EQ 'P'.
                 PERFORM  P1000_SUBSEQUENT_YN USING
                                      IT_ZTBDIV-COND_TYPE
                                      IT_ZTBDIV-EBELN
                                      IT_ZTBDIV-EBELP
                                      IT_ZTBDIV-TBTKZ
                                      IT_ZTBDIV-ZFCSTGRP
                                      IT_ZTBDIV-ZFIMDNO
                                      IT_ZTBDIV-ZFIMDIT.
              ENDIF.

              " Account No Get.
              IF IT_ZTBDIV-NEWKO IS INITIAL.
                 PERFORM P1000_GETDATA_NEWKO USING IT_ZTBDIV-BUKRS
                                                   IT_ZTBDIV-ZFCSTGRP
                                                   IT_ZTBDIV-ZFCD
                                                   IT_ZTBDIV-COND_TYPE
                                                   IT_ZTBDIV-EBELN
                                                   IT_ZTBDIV-EBELP
                                                   IT_ZTBDIV-WERKS
                                                   IT_ZTBDIV-NEWKO
                                                   IT_ZTBDIV-PRCTR.
               ENDIF.

               ADD   IT_ZTBDIV-WRBTR  TO   W_AMOUNT_USD.
               ADD   IT_ZTBDIV-DMBTR  TO   W_AMOUNT_WON.

               APPEND IT_ZTBDIV.
           ENDLOOP.
        WHEN  '006'.

           SELECT SINGLE * FROM ZTIDSUS
           WHERE  ZFIVNO   EQ   IT_ZSBSEG-ZFIMDNO.

           W_CHG_BALANCE = IT_ZSBSEG-DMBTR - ZTIDSUS-ZFDUTY.
           IF W_CHG_BALANCE LE 0.
              W_CHG_BALANCE = W_CHG_BALANCE * -1.
           ENDIF.

           LOOP AT IT_ZSIVIT.
              ADD 1 TO W_ZFBSEQ.
              ADD 1 TO W_ZBUZEI.
              CLEAR : IT_ZTBDIV.

              IT_ZTBDIV-ZFAMT =  ( IT_ZSIVIT-CCMENGE *
                         ( IT_ZSIVIT-BPUMZ / IT_ZSIVIT-BPUMN ) *
                         ( IT_ZSIVIT-NETPR / IT_ZSIVIT-PEINH ) ).

              IF IT_ZSIVIT-KBETR GT 0.
                 IT_ZTBDIV-ZFAMT = IT_ZTBDIV-ZFAMT *
                                   ( IT_ZSIVIT-KBETR / 100 ).
              ENDIF.

              "--------------------------------------------------
              " FTZ AUTO UPLOAD => FTZ DUTY
              "--------------------------------------------------
              IF IT_ZSBSEG-ZFCD EQ '001' AND ZTIDSUS-ZFAUTO EQ 'X' AND
                 W_CHG_BALANCE  LE 5.

                 CLEAR : ZTIDSUSD.
                 SELECT SINGLE * FROM ZTIDSUSD
                 WHERE  ZFIVNO   EQ   IT_ZSIVIT-ZFIVNO
                 AND    ZFIVDNO  EQ   IT_ZSIVIT-ZFIVDNO.
                 IT_ZTBDIV-WRBTR = ZTIDSUSD-ZFDUTY.
                 IT_ZTBDIV-DMBTR = ZTIDSUSD-ZFDUTY.
              "-------------------------------------------------
              " Non-Monetary case
              "-------------------------------------------------
              ELSEIF W_AMOUNT LE 0 OR W_GUBUN EQ 'Y'.
                 IT_ZTBDIV-WRBTR = ( 1 / W_COUNT ) * IT_ZSBSEG-WRBTR.
                 IT_ZTBDIV-DMBTR = ( 1 / W_COUNT ) * IT_ZSBSEG-DMBTR.
              "-------------------------------------------------
              " Monetary Case
              "-------------------------------------------------
              ELSE.
                 IT_ZTBDIV-WRBTR = ( IT_ZTBDIV-ZFAMT / W_AMOUNT )
                                                   * IT_ZSBSEG-WRBTR.
                 IT_ZTBDIV-DMBTR = ( IT_ZTBDIV-ZFAMT / W_AMOUNT )
                                                   * IT_ZSBSEG-DMBTR.
              ENDIF.

              IF IT_ZSBSEG-ZFCD NE '001'.
                 IF IT_ZTBDIV-WRBTR EQ 0 OR IT_ZTBDIV-DMBTR EQ 0.
                    CONTINUE.
                 ENDIF.
              ENDIF.

              " Business Place
              SELECT SINGLE J_1BBRANCH INTO IT_ZTBDIV-BUPLA
                     FROM T001W
                     WHERE WERKS EQ IT_ZSIVIT-WERKS.

              " Business Area
              IF NOT IT_ZSIVIT-MATNR IS INITIAL.
                 SELECT SINGLE * FROM  MARC
                                 WHERE MATNR EQ IT_ZSIVIT-MATNR
                                 AND   WERKS EQ IT_ZSIVIT-WERKS.
                 MOVE : MARC-PRCTR   TO  IT_ZTBDIV-PRCTR.

                 SELECT SINGLE * FROM MARA
                                 WHERE MATNR EQ IT_ZSIVIT-MATNR.

                 IF SY-SUBRC EQ 0 AND NOT MARA-SPART IS INITIAL.
                    SELECT SINGLE GSBER INTO  IT_ZTBDIV-GSBER
                           FROM  T134G
                           WHERE WERKS EQ IT_ZSIVIT-WERKS
                           AND   SPART EQ MARA-SPART.
                 ELSE.
                    SELECT MAX( GSBER ) INTO IT_ZTBDIV-GSBER
                                FROM T134G
                                WHERE WERKS EQ IT_ZSIVIT-WERKS.
                 ENDIF.

              ELSEIF NOT IT_ZSBSEG-KOSTL IS INITIAL.
                 SELECT MAX( GSBER ) INTO IT_ZTBDIV-GSBER
                             FROM T134G
                             WHERE WERKS EQ IT_ZSIVIT-WERKS.

                 SELECT SINGLE * FROM TKA02
                             WHERE BUKRS EQ BUKRS
                             AND   GSBER EQ IT_ZTBDIV-GSBER.

                 SELECT * FROM  CSKS UP TO 1 ROWS
                          WHERE KOKRS  EQ TKA02-KOKRS
                          AND   KOSTL  EQ IT_ZSBSEG-KOSTL
                          AND   DATBI  GE SY-DATUM
                          AND   DATAB  LE SY-DATUM.
                    MOVE : CSKS-PRCTR   TO  IT_ZTBDIV-PRCTR.
                 ENDSELECT.
              ENDIF.

              MOVE : SY-MANDT            TO    IT_ZTBDIV-MANDT,
                     BUKRS               TO    IT_ZTBDIV-BUKRS,
                     GJAHR               TO    IT_ZTBDIV-GJAHR,
                     BELNR               TO    IT_ZTBDIV-BELNR,
                     IT_ZSBSEG-BUZEI     TO    IT_ZTBDIV-BUZEI,
                     IT_ZSBSEG-DBUZEI    TO    IT_ZTBDIV-DBUZEI,
                     W_ZBUZEI            TO    IT_ZTBDIV-ZBUZEI,
                     W_ZFBSEQ            TO    IT_ZTBDIV-ZFBSEQ,
                     ZTBKPF-ZFDCSTX      TO    IT_ZTBDIV-ZFDCSTX,
                     IT_ZSBSEG-COND_TYPE TO    IT_ZTBDIV-COND_TYPE,
                     ZTBKPF-ZFCSTGRP     TO    IT_ZTBDIV-ZFCSTGRP,
                     IT_ZSBSEG-ZFCD      TO    IT_ZTBDIV-ZFCD,
                     IT_ZSBSEG-ZFPOYN    TO    IT_ZTBDIV-ZFPOYN,
                     IT_ZSIVIT-ZFIVNO    TO    IT_ZTBDIV-ZFIMDNO,
                     IT_ZSIVIT-ZFIVDNO   TO    IT_ZTBDIV-ZFIMDIT,
                     IT_ZSIVIT-EBELN     TO    IT_ZTBDIV-EBELN,
                     IT_ZSIVIT-EBELP     TO    IT_ZTBDIV-EBELP,
                     IT_ZSIVIT-MATNR     TO    IT_ZTBDIV-MATNR,
                     IT_ZSIVIT-WERKS     TO    IT_ZTBDIV-WERKS,
                     IT_ZSIVIT-GRMENGE   TO    IT_ZTBDIV-MENGE,
                     IT_ZSIVIT-MEINS     TO    IT_ZTBDIV-MEINS,
                     IT_ZSIVIT-NETPR     TO    IT_ZTBDIV-NETPR,
                     IT_ZSIVIT-PEINH     TO    IT_ZTBDIV-PEINH,
                     IT_ZSIVIT-BPRME     TO    IT_ZTBDIV-BPRME,
                     IT_ZSIVIT-WAERS     TO    IT_ZTBDIV-WAERS1,
                     IT_ZSIVIT-BPUMN     TO    IT_ZTBDIV-BPUMN,
                     IT_ZSIVIT-BPUMN     TO    IT_ZTBDIV-BPUMZ,
                     IT_ZSIVIT-MATNR     TO    IT_ZTBDIV-MATNR,
                     IT_ZSIVIT-TXZ01     TO    IT_ZTBDIV-TXZ01,
                     IT_ZSBSEG-NEWBS     TO    IT_ZTBDIV-NEWBS,
                     IT_ZSBSEG-NEWKO     TO    IT_ZTBDIV-NEWKO,
                     SPACE               TO    IT_ZTBDIV-AKONT,
                     ZTBKPF-ZTERM        TO    IT_ZTBDIV-ZTERM,
                     ZTBKPF-XMWST        TO    IT_ZTBDIV-XMWST,
                     IT_ZSBSEG-MWSKZ     TO    IT_ZTBDIV-MWSKZ,
                     ZTBKPF-ZFBDT        TO    IT_ZTBDIV-ZFBDT,
                     ZTBKPF-WAERS        TO    IT_ZTBDIV-WAERS,
                     ZTBKPF-HWAER        TO    IT_ZTBDIV-HWAER,
                     IT_ZSBSEG-PS_POSID  TO    IT_ZTBDIV-PS_POSID,
                     IT_ZSBSEG-KOSTL     TO    IT_ZTBDIV-KOSTL,
                     IT_ZSBSEG-KURSF     TO    IT_ZTBDIV-KURSF,
                     IT_ZSBSEG-WWERT     TO    IT_ZTBDIV-WWERT,
                     IT_ZSBSEG-ZUONR     TO    IT_ZTBDIV-ZUONR,
                     IT_ZSIVIT-KNUMV     TO    IT_ZTBDIV-KNUMV.

*ANDY reqeust
               CONCATENATE IT_ZSBSEG-ZFCSTGRP '-' IT_ZSBSEG-ZFCD
                           ':' IT_ZSBSEG-SGTXT
                      INTO IT_ZTBDIV-SGTXT  .
*             CONCATENATE IT_ZTBDIV-MATNR IT_ZSBSEG-SGTXT(30) INTO
*                         IT_ZTBDIV-SGTXT  SEPARATED BY SPACE.

             " Import Expense -> Subsequent Debit
             IF ZTIMIMG00-ZFCSTMD EQ 'P'.
                PERFORM  P1000_SUBSEQUENT_YN USING
                                     IT_ZTBDIV-COND_TYPE
                                     IT_ZTBDIV-EBELN
                                     IT_ZTBDIV-EBELP
                                     IT_ZTBDIV-TBTKZ
                                     IT_ZTBDIV-ZFCSTGRP
                                     IT_ZTBDIV-ZFIMDNO
                                     IT_ZTBDIV-ZFIMDIT.
             ENDIF.

             " Account No Get.
             IF IT_ZTBDIV-NEWKO IS INITIAL.
                PERFORM P1000_GETDATA_NEWKO USING IT_ZTBDIV-BUKRS
                                                  IT_ZTBDIV-ZFCSTGRP
                                                  IT_ZTBDIV-ZFCD
                                                  IT_ZTBDIV-COND_TYPE
                                                  IT_ZTBDIV-EBELN
                                                  IT_ZTBDIV-EBELP
                                                  IT_ZTBDIV-WERKS
                                                  IT_ZTBDIV-NEWKO
                                                  IT_ZTBDIV-PRCTR.
              ENDIF.

              ADD   IT_ZTBDIV-WRBTR  TO   W_AMOUNT_USD.
              ADD   IT_ZTBDIV-DMBTR  TO   W_AMOUNT_WON.

              APPEND IT_ZTBDIV.
          ENDLOOP.

          IF SY-SUBRC NE 0.
              W_COUNT = 1.
              ADD 1 TO W_ZFBSEQ.
              ADD 1 TO W_ZBUZEI.
              CLEAR : IT_ZTBDIV, W_AMOUNT.

              IT_ZTBDIV-ZFAMT =  IT_ZSBSEG-WMWST.

              IF W_AMOUNT LE 0 OR W_GUBUN EQ 'Y'.
                 IT_ZTBDIV-WRBTR = ( 1 / W_COUNT ) * IT_ZSBSEG-WRBTR.
                 IT_ZTBDIV-DMBTR = ( 1 / W_COUNT ) * IT_ZSBSEG-DMBTR.
                 IT_ZTBDIV-FWBAS = ( 1 / W_COUNT ) * IT_ZSBSEG-FWBAS.
                 IT_ZTBDIV-WMWST = ( 1 / W_COUNT ) * IT_ZSBSEG-WMWST.
              ELSE.
                 IT_ZTBDIV-WRBTR = ( IT_ZTBDIV-ZFAMT / W_AMOUNT )
                                                   * IT_ZSBSEG-WRBTR.
                 IT_ZTBDIV-DMBTR = ( IT_ZTBDIV-ZFAMT / W_AMOUNT )
                                                   * IT_ZSBSEG-DMBTR.
                 IT_ZTBDIV-FWBAS = ( IT_ZTBDIV-ZFAMT / W_AMOUNT )
                                                   * IT_ZSBSEG-FWBAS.
                 IT_ZTBDIV-WMWST = ( IT_ZTBDIV-ZFAMT / W_AMOUNT )
                                                   * IT_ZSBSEG-WMWST.
              ENDIF.

              IT_ZTBDIV-GSBER = ZTBKPF-GSBER.

              MOVE : SY-MANDT            TO    IT_ZTBDIV-MANDT,
                     BUKRS               TO    IT_ZTBDIV-BUKRS,
                     GJAHR               TO    IT_ZTBDIV-GJAHR,
                     BELNR               TO    IT_ZTBDIV-BELNR,
                     IT_ZSBSEG-BUZEI     TO    IT_ZTBDIV-BUZEI,
                     IT_ZSBSEG-DBUZEI    TO    IT_ZTBDIV-DBUZEI,
                     W_ZBUZEI            TO    IT_ZTBDIV-ZBUZEI,
                     W_ZFBSEQ            TO    IT_ZTBDIV-ZFBSEQ,
                     ZTBKPF-ZFDCSTX      TO    IT_ZTBDIV-ZFDCSTX,
                     IT_ZSBSEG-COND_TYPE TO    IT_ZTBDIV-COND_TYPE,
                     ZTBKPF-ZFCSTGRP     TO    IT_ZTBDIV-ZFCSTGRP,
                     IT_ZSBSEG-ZFCD      TO    IT_ZTBDIV-ZFCD,
                     IT_ZSBSEG-ZFPOYN    TO    IT_ZTBDIV-ZFPOYN,
                     SPACE               TO    IT_ZTBDIV-ZFIMDNO,
                     '00000'             TO    IT_ZTBDIV-ZFIMDIT,
                     SPACE               TO    IT_ZTBDIV-EBELN,
                     SPACE               TO    IT_ZTBDIV-EBELP,
                     SPACE               TO    IT_ZTBDIV-WERKS,
                     0                   TO    IT_ZTBDIV-MENGE,
                     SPACE               TO    IT_ZTBDIV-MEINS,
                     0                   TO    IT_ZTBDIV-NETPR,
                     0                   TO    IT_ZTBDIV-PEINH,
                     SPACE               TO    IT_ZTBDIV-BPRME,
                     IT_ZSBDIV-WAERS     TO    IT_ZTBDIV-WAERS1,
                     1                   TO    IT_ZTBDIV-BPUMN,
                     1                   TO    IT_ZTBDIV-BPUMZ,
                     IT_ZSBSEG-NEWBS     TO    IT_ZTBDIV-NEWBS,
                     IT_ZSBSEG-NEWKO     TO    IT_ZTBDIV-NEWKO,
                     SPACE               TO    IT_ZTBDIV-AKONT,
                     ZTBKPF-ZTERM        TO    IT_ZTBDIV-ZTERM,
                     ZTBKPF-XMWST        TO    IT_ZTBDIV-XMWST,
                     IT_ZSBSEG-MWSKZ     TO    IT_ZTBDIV-MWSKZ,
                     ZTBKPF-ZFBDT        TO    IT_ZTBDIV-ZFBDT,
                     ZTBKPF-BUPLA        TO    IT_ZTBDIV-BUPLA,
                     ZTBKPF-WAERS        TO    IT_ZTBDIV-WAERS,
                     ZTBKPF-HWAER        TO    IT_ZTBDIV-HWAER,
                     IT_ZSBSEG-KURSF     TO    IT_ZTBDIV-KURSF,
                     IT_ZSBSEG-PS_POSID  TO    IT_ZTBDIV-PS_POSID,
                     IT_ZSBSEG-KOSTL     TO    IT_ZTBDIV-KOSTL,
                     IT_ZSBSEG-PRCTR     TO    IT_ZTBDIV-PRCTR,
                     IT_ZSBSEG-KURSF     TO    IT_ZTBDIV-KURSF,
                     IT_ZSBSEG-WWERT     TO    IT_ZTBDIV-WWERT,
                     IT_ZSBSEG-ZUONR     TO    IT_ZTBDIV-ZUONR.

*ANDY reqeust
               CONCATENATE IT_ZSBSEG-ZFCSTGRP '-' IT_ZSBSEG-ZFCD
                           ':' IT_ZSBSEG-SGTXT
                      INTO IT_ZTBDIV-SGTXT  .
*             CONCATENATE IT_ZTBDIV-MATNR IT_ZSBSEG-SGTXT(30) INTO
*                         IT_ZTBDIV-SGTXT  SEPARATED BY SPACE.

              ADD   IT_ZTBDIV-WRBTR  TO   W_AMOUNT_USD.
              ADD   IT_ZTBDIV-DMBTR  TO   W_AMOUNT_WON.

              APPEND IT_ZTBDIV.

          ENDIF.
        WHEN OTHERS.
           CONTINUE.
     ENDCASE.

*----------------------------------------------------
*> Amount Process
*----------------------------------------------------
     DESCRIBE TABLE  IT_ZTBDIV  LINES W_LINE.
     READ TABLE  IT_ZTBDIV  INDEX  W_LINE.
     W_TABIX = SY-TABIX.

     W_WRBTR1  =  IT_ZSBSEG-WRBTR.
     W_WRBTR2  =  W_AMOUNT_WON.
     W_DMBTR1  =  IT_ZSBSEG-DMBTR.
     W_DMBTR2  =  W_AMOUNT_USD.
*
*     IF ZTBKPF-ZFPCUR EQ 'X'.
*        W_WRBTR1 = IT_ZSBSEG-DMBTR.
*        W_WRBTR2 = W_AMOUNT_WON.
*     ELSE.
*        W_WRBTR1 = IT_ZSBSEG-WRBTR.
*        W_WRBTR2 = W_AMOUNT_USD.
*     ENDIF.

     IF W_WRBTR2 NE W_WRBTR1.
        IF W_WRBTR2 GT W_WRBTR1.
           W_WRBTR_CHA  =  W_WRBTR2 - W_WRBTR1.
           LOOP AT IT_ZTBDIV WHERE WRBTR GT W_WRBTR_CHA.
              W_TABIX = SY-TABIX.
           ENDLOOP.

           IT_ZTBDIV-WRBTR = IT_ZTBDIV-WRBTR +
                            (  W_WRBTR1 - W_WRBTR2 ).
        ELSE.
           IT_ZTBDIV-WRBTR = IT_ZTBDIV-WRBTR +
                            ( W_WRBTR1 - W_WRBTR2 ).
        ENDIF.
        MODIFY   IT_ZTBDIV  INDEX   W_TABIX.
     ENDIF.

     IF W_DMBTR2 NE W_DMBTR1.
        IF W_DMBTR2 GT W_DMBTR1.
           W_WRBTR_CHA  =  W_DMBTR2 - W_DMBTR1.
           LOOP AT IT_ZTBDIV WHERE DMBTR GT W_WRBTR_CHA.
              W_TABIX  =  SY-TABIX.
           ENDLOOP.
           IT_ZTBDIV-DMBTR = IT_ZTBDIV-DMBTR +
                            (  W_DMBTR1 - W_DMBTR2 ).
        ELSE.
           IT_ZTBDIV-DMBTR = IT_ZTBDIV-DMBTR +
                            ( W_DMBTR1 - W_DMBTR2 ).
        ENDIF.
        MODIFY   IT_ZTBDIV  INDEX   W_TABIX.
     ENDIF.

  ENDLOOP.

*>> P/O Condition Check
  IF ZTIMIMG00-ZFCSTMD EQ 'P'.
     SORT  IT_ZTBDIV  BY  EBELN  EBELP  COND_TYPE.

     CLEAR : W_TABIX, W_CNT, W_SUBRC.
     LOOP  AT  IT_ZTBDIV  WHERE  ZFIMDNO  NE  SPACE.

        W_TABIX  =  SY-TABIX.

        IF IT_ZTBDIV-EBELN NE SV_EBELN OR IT_ZTBDIV-EBELP NE SV_EBELP OR
           IT_ZTBDIV-COND_TYPE  NE  SV_COND.

              CLEAR : EKBZ.
              SELECT SINGLE * FROM  EKBZ
              WHERE  EBELN  EQ  IT_ZTBDIV-EBELN
              AND    EBELP  EQ  IT_ZTBDIV-EBELP
              AND    BELNR  EQ  ( SELECT MAX( BELNR )
                                  FROM   EKBZ
                                  WHERE  EBELN  EQ IT_ZTBDIV-EBELN
                                  AND    EBELP  EQ IT_ZTBDIV-EBELP
                                  AND    KSCHL  EQ IT_ZTBDIV-COND_TYPE
                                  AND    VGABE  EQ '2'
                                  AND    BEWTP  EQ 'M' ).
           W_SUBRC  = SY-SUBRC.
           MOVE : IT_ZTBDIV-EBELN       TO  SV_EBELN,
                  IT_ZTBDIV-EBELP       TO  SV_EBELP,
                  IT_ZTBDIV-COND_TYPE   TO  SV_COND.
           CLEAR : W_CNT.
        ENDIF.

        W_CNT = W_CNT + 1.

        IF W_SUBRC NE 0 AND W_CNT GT 1.
           MOVE  'X'  TO  IT_ZTBDIV-TBTKZ.
           MODIFY  IT_ZTBDIV  INDEX  W_TABIX.
        ENDIF.
     ENDLOOP.
  ENDIF.

ENDFUNCTION.
