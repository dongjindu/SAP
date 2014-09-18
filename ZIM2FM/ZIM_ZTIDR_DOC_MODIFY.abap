FUNCTION ZIM_ZTIDR_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFBLNO) LIKE  ZTIDR-ZFBLNO
*"     VALUE(ZFCLSEQ) LIKE  ZTIDR-ZFCLSEQ
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTIDR_OLD) LIKE  ZTIDR STRUCTURE  ZTIDR
*"     VALUE(W_ZTIDR) LIKE  ZTIDR STRUCTURE  ZTIDR
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSIDRHS_OLD STRUCTURE  ZSIDRHS OPTIONAL
*"      IT_ZSIDRHS STRUCTURE  ZSIDRHS
*"      IT_ZSIDRHSD_OLD STRUCTURE  ZSIDRHSD OPTIONAL
*"      IT_ZSIDRHSD STRUCTURE  ZSIDRHSD
*"      IT_ZSIDRHSL_OLD STRUCTURE  ZSIDRHSL OPTIONAL
*"      IT_ZSIDRHSL STRUCTURE  ZSIDRHSL
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"      ERROR_DELETE
*"----------------------------------------------------------------------
DATA : W_ZFIDRNO         LIKE   ZTIDR-ZFIDRNO,
       W_COUNT           TYPE   I,
       W_SEQ(6)          TYPE   N,
       W_TMP(6)          TYPE   N,
       W_TMP_1(1)        TYPE   N,
       W_CHK(1)          TYPE   N,
       W_YEAR(2)         TYPE   C,
       W_YYYY(4)         TYPE   C,
       W_YYYYMMDD_FROM   LIKE   ZTIDR-ZFIDWDT,
       W_YYYYMMDD_TO     LIKE   ZTIDR-ZFIDWDT,
       L_ZFIDRNO         LIKE   ZTIDR-ZFIDRNO.

*>> KSB INSERT.
DATA : L_ZFIVNO          LIKE   ZTIV-ZFIVNO.

   MOVE-CORRESPONDING  W_ZTIDR  TO  ZTIDR.

   MOVE : W_ZTIDR-ZFIVNO TO  L_ZFIVNO,
          'O'         TO     ZTIDR-ZFEDICK,
          ZFBLNO      TO     ZTIDR-ZFBLNO,
          ZFCLSEQ     TO     ZTIDR-ZFCLSEQ,
          SY-MANDT    TO     ZTIDR-MANDT,
          SY-UNAME    TO     ZTIDR-UNAM,
          SY-DATUM    TO     ZTIDR-UDAT.

* BL 정보 GET!
   SELECT SINGLE * FROM  ZTBL WHERE ZFBLNO = ZTIDR-ZFBLNO.

   IF W_OK_CODE EQ 'DELE'.
      ZFSTATUS = 'X'.
   ENDIF.

   CASE ZFSTATUS.
      WHEN 'X'.               " 삭제
         DELETE  FROM ZTIDR    WHERE ZFBLNO  EQ ZFBLNO
                               AND   ZFCLSEQ EQ ZFCLSEQ.
         IF SY-SUBRC NE 0.  RAISE  ERROR_DELETE.  ENDIF.

         DELETE  FROM ZTIDRHS  WHERE ZFBLNO  EQ ZFBLNO
                               AND   ZFCLSEQ EQ ZFCLSEQ.

         DELETE  FROM ZTIDRHSD WHERE ZFBLNO  EQ ZFBLNO
                               AND   ZFCLSEQ EQ ZFCLSEQ.

         DELETE  FROM  ZTIDRHSL WHERE  ZFBLNO  EQ  ZFBLNO
                                AND    ZFCLSEQ EQ  ZFCLSEQ.

         DELETE  FROM  ZTIDRCR  WHERE  ZFBLNO  EQ  ZFBLNO
                                AND    ZFCLSEQ EQ  ZFCLSEQ.

         DELETE  FROM  ZTIDRCRIT  WHERE  ZFBLNO  EQ  ZFBLNO
                                  AND    ZFCLSEQ EQ  ZFCLSEQ.

         DELETE  FROM  ZTIDRDTU WHERE  ZFBLNO  EQ  ZFBLNO
                                AND    ZFCLSEQ EQ  ZFCLSEQ.

*         DELETE  FROM  ZTCUCL   WHERE  ZFBLNO  EQ  ZFBLNO
*                                AND    ZFCLSEQ EQ  ZFCLSEQ.
*
*         DELETE  FROM   ZTCUCLIVIT
*                 WHERE  ZFIVNO  IN  ( SELECT  ZFIVNO
*                                      FROM    ZTCUCLIV
*                                      WHERE   ZFBLNO  EQ  ZFBLNO
*                                      AND     ZFCLSEQ EQ  ZFCLSEQ ).
**>> NUMBER GET..(KSB INSERT)....
*         SELECT  SINGLE ZFIVNO INTO L_ZFIVNO
*                               FROM    ZTCUCLIV
*                               WHERE   ZFBLNO   EQ  ZFBLNO
*                               AND     ZFCLSEQ  EQ  ZFCLSEQ.
*         W_SUBRC = SY-SUBRC.
*
*         DELETE  FROM   ZTCUCLIV
*                 WHERE  ZFBLNO   EQ   ZFBLNO
*                 AND    ZFCLSEQ  EQ   ZFCLSEQ.

* INVOICE 상태 변경.
         IF W_SUBRC EQ 0.
            CALL  FUNCTION 'ZIM_GET_CC_DOCUMENT'
                  EXPORTING
                     ZFIVNO              =          L_ZFIVNO
                  IMPORTING
                     W_ZTIV              =          ZTIV
                  TABLES
                     IT_ZSIVIT           =          IT_ZSIVIT
                     IT_ZSIVIT_ORG       =          IT_ZSIVIT_ORG
                  EXCEPTIONS
                     NOT_FOUND           =    4
                     NOT_INPUT           =    8.

            MOVE-CORRESPONDING ZTIV TO *ZTIV.

            IF SY-SUBRC EQ 0.
               SELECT SINGLE * FROM ZTIMIMG00.

               CASE ZTIMIMG00-ZFIMPATH.
                  WHEN '1'.
                     MOVE : 'SAVE' TO W_OK_CODE,
                            'U'    TO ZFSTATUS.

                     IF ZTIV-ZFCLCD EQ 'X'.
                        EXIT.
                     ELSE.
                        MOVE : '1'      TO   ZTIV-ZFCUST,
                               SY-DATUM TO   ZTIV-UDAT,
                               SY-UNAME TO   ZTIV-UNAM.
                     ENDIF.

                  WHEN '2' OR '3'.
                     MOVE : 'DELE' TO W_OK_CODE,
                            'D'    TO ZFSTATUS.
                  WHEN OTHERS.
               ENDCASE.

               CALL FUNCTION 'ZIM_CUSTOMS_CLEARANCE_MODIFY'
                    EXPORTING
                      W_OK_CODE           =   W_OK_CODE
                      ZFIVNO              =   ZTIV-ZFIVNO
                      ZFSTATUS            =   ZFSTATUS
                      W_ZTIV              =   ZTIV
                      W_ZTIV_OLD          =  *ZTIV
                   TABLES
                      IT_ZSIVIT           =    IT_ZSIVIT
                      IT_ZSIVIT_OLD       =    IT_ZSIVIT_ORG
                   EXCEPTIONS
                      ERROR_UPDATE        =    4.
               IF SY-SUBRC EQ 0.
                  EXIT.
               ENDIF.
            ENDIF.
         ENDIF.
         EXIT.
      WHEN OTHERS.            " 변경
         UPDATE   ZTIDR.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

* 수입면허 규격
         SELECT * FROM ZTIDRHSD  WHERE ZFBLNO   EQ  ZFBLNO
                                 AND   ZFCLSEQ  EQ  ZFCLSEQ.

            READ TABLE IT_ZSIDRHSD  WITH KEY ZFBLNO  = ZTIDRHSD-ZFBLNO
                                             ZFCLSEQ = ZTIDRHSD-ZFCLSEQ
                                             ZFCONO  = ZTIDRHSD-ZFCONO
                                             ZFRONO  = ZTIDRHSD-ZFRONO
                                    BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSIDRHSD  TO ZTIDRHSD.
               UPDATE ZTIDRHSD.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTIDRHSD.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSIDRHSD.
            SELECT SINGLE * FROM  ZTIDRHSD
                            WHERE ZFBLNO   EQ  ZFBLNO
                            AND   ZFCLSEQ  EQ  ZFCLSEQ
                            AND   ZFCONO   EQ  IT_ZSIDRHSD-ZFCONO
                            AND   ZFRONO   EQ  IT_ZSIDRHSD-ZFRONO.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSIDRHSD  TO ZTIDRHSD.

               MOVE : ZFBLNO                 TO ZTIDRHSD-ZFBLNO,
                      ZFCLSEQ                TO ZTIDRHSD-ZFCLSEQ,
                      SY-MANDT               TO ZTIDRHSD-MANDT.

               INSERT  ZTIDRHSD.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.

* 수입면허 규격.
         SELECT * FROM ZTIDRHS WHERE ZFBLNO   EQ  ZFBLNO
                               AND   ZFCLSEQ  EQ  ZFCLSEQ.

            READ TABLE IT_ZSIDRHS WITH KEY ZFBLNO  = ZTIDRHS-ZFBLNO
                                           ZFCLSEQ = ZTIDRHS-ZFCLSEQ
                                           ZFCONO  = ZTIDRHS-ZFCONO
                                  BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSIDRHS TO ZTIDRHS.
               UPDATE ZTIDRHS.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTIDRHS.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSIDRHS.
            SELECT SINGLE * FROM  ZTIDRHS
                            WHERE ZFBLNO   EQ  ZFBLNO
                            AND   ZFCLSEQ  EQ  ZFCLSEQ
                            AND   ZFCONO   EQ  IT_ZSIDRHS-ZFCONO.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSIDRHS TO ZTIDRHS.
               MOVE : ZFBLNO                 TO ZTIDRHS-ZFBLNO,
                      ZFCLSEQ                TO ZTIDRHS-ZFCLSEQ,
                      SY-MANDT               TO ZTIDRHS-MANDT.

               INSERT  ZTIDRHS.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.

* 수입면허 추천
         SELECT * FROM ZTIDRHSL  WHERE ZFBLNO   EQ  ZFBLNO
                                 AND   ZFCLSEQ  EQ  ZFCLSEQ.

            READ TABLE IT_ZSIDRHSL  WITH KEY ZFBLNO  = ZTIDRHSL-ZFBLNO
                                             ZFCLSEQ = ZTIDRHSL-ZFCLSEQ
                                             ZFCONO  = ZTIDRHSL-ZFCONO
                                             ZFCNDC  = ZTIDRHSL-ZFCNDC
                                             ZFCNNO  = ZTIDRHSL-ZFCNNO
                                    BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSIDRHSL  TO ZTIDRHSL.
               UPDATE ZTIDRHSL.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTIDRHSL.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSIDRHSL.
            SELECT SINGLE * FROM  ZTIDRHSL
                            WHERE ZFBLNO   EQ  ZFBLNO
                            AND   ZFCLSEQ  EQ  ZFCLSEQ
                            AND   ZFCONO   EQ  IT_ZSIDRHSL-ZFCONO
                            AND   ZFCNDC   EQ  IT_ZSIDRHSL-ZFCNDC
                            AND   ZFCNNO   EQ  IT_ZSIDRHSL-ZFCNNO.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSIDRHSL  TO ZTIDRHSL.

               MOVE : ZFBLNO                 TO ZTIDRHSL-ZFBLNO,
                      ZFCLSEQ                TO ZTIDRHSL-ZFCLSEQ,
                      SY-MANDT               TO ZTIDRHSL-MANDT.

               INSERT  ZTIDRHSL.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.
   ENDCASE.

*  IF W_OK_CODE = 'SVCO' OR W_OK_CODE EQ 'EDIS'.
*     IF ZTIDR-ZFIDWDT IS INITIAL.
*        MOVE  SY-DATUM   TO  ZTIDR-ZFIDWDT.
*     ENDIF.
*     MOVE ZTIDR-ZFIDWDT+0(4) TO W_YYYY.
*     CONCATENATE W_YYYY '0101' INTO W_YYYYMMDD_FROM.
*     CONCATENATE W_YYYY '1231' INTO W_YYYYMMDD_TO.
*     CLEAR W_ZFIDRNO.
*
*     CONCATENATE '_____' SY-DATUM+2(2) '8______' INTO L_ZFIDRNO.
**--------------> INFOLINK  DREAMKSB <----------------------

**---> 2000/10/02 안덕기 과장 요청 ( 800001 이후 번호로 채번 )
**---> 연도별?
**                          AND ZFIDRNO LIKE '_______8_____'
*     SELECT MAX( ZFIDRNO ) INTO W_ZFIDRNO FROM ZTIDR
*                           WHERE ZFCUT = ZTIDR-ZFCUT
*                           AND ZFIDRNO LIKE L_ZFIDRNO
*                           AND ZFIDWDT >= W_YYYYMMDD_FROM
*                           AND ZFIDWDT <= W_YYYYMMDD_TO
*                           AND ( ZFBLNO  NE ZTIDR-ZFBLNO
*                           OR    ZFCLSEQ NE ZTIDR-ZFCLSEQ ).
*     IF W_ZFIDRNO IS INITIAL.
*        MOVE ZTIDR-ZFIDWDT+2(2) TO W_YEAR.
*        MOVE '800000'           TO W_SEQ.
*     ELSE.
*        MOVE W_ZFIDRNO+5(2)     TO W_YEAR.
*        MOVE W_ZFIDRNO+7(6)     TO W_SEQ.
*     ENDIF.
*     ADD  1                     TO W_SEQ.  " 수입신고번?
*     W_TMP = ZTIDR-ZFCUT+0(1) * 7 + ZTIDR-ZFCUT+1(1) * 3
*           + ZTIDR-ZFCUT+2(1) * 1 + ZTIDR-ZFCUT+3(1) * 7
*           + ZTIDR-ZFCUT+4(1) * 3
*           + W_YEAR+0(1) * 1 + W_YEAR+1(1) * 7
*           + W_SEQ+0(1) * 3 + W_SEQ+1(1) * 1
*           + W_SEQ+2(1) * 7 + W_SEQ+3(1) * 3
*           + W_SEQ+4(1) * 1 + W_SEQ+5(1) * 7.
*     W_TMP_1 = W_TMP MOD 10.
*     W_CHK   = 10 - W_TMP_1.
*     CONCATENATE ZTIDR-ZFCUT W_YEAR W_SEQ W_CHK INTO ZTIDR-ZFIDRNO.
**>> 수입신고 번호 중복 CHECK!
*     SELECT  COUNT( DISTINCT ZFIDRNO )  INTO  W_COUNT
*     FROM    ZTIDR
*     WHERE   ZFIDRNO  =  ZTIDR-ZFIDRNO.
*     IF W_COUNT >  0.
*        W_SEQ  =  W_SEQ  +  1.
*        CONCATENATE ZTIDR-ZFCUT W_YEAR W_SEQ W_CHK INTO ZTIDR-ZFIDRNO.
*     ENDIF.
*
*     CONCATENATE ZTIDR-ZFREBELN
*                 W_YEAR W_SEQ INTO ZTIDR-ZFIMCR. " 무역업체참조번?

     CLEAR ZTIDR-ZFNSCD.
     MOVE SY-UNAME TO ZTIDR-UNAM.
     MOVE SY-DATUM TO ZTIDR-UDAT.
     UPDATE ZTIDR.
     IF SY-SUBRC NE  0.  RAISE ERROR_UPDATE. ENDIF.

     SET PARAMETER ID 'ZPIDRNO' FIELD ZTIDR-ZFIDRNO.

**>> 통관 TABLE 의 상태변경및 수입신고 번호 SETTING!
*     UPDATE  ZTCUCL
*     SET     ZFCUST    =   '3'
*             ZFIDRNO   =   ZTIDR-ZFIDRNO
*             ZFIDWDT   =   ZTIDR-ZFIDWDT
*             UNAM      =   SY-UNAME
*             UDAT      =   SY-DATUM
*     WHERE   ZFBLNO    EQ  ZFBLNO
*     AND     ZFCLSEQ   EQ  ZFCLSEQ .
*     IF SY-SUBRC NE  0. RAISE  ERROR_UPDATE. ENDIF.
*
**>> 통관요청 INVOICE 상태변경.
*     UPDATE  ZTCUCLIV
*     SET     ZFCUST   =  '3'
*             UNAM     =  SY-UNAME
*             UDAT     =  SY-DATUM
*     WHERE   ZFBLNO   =  ZTIDR-ZFBLNO
*     AND     ZFCLSEQ  =  ZTIDR-ZFCLSEQ .
*     IF SY-SUBRC NE 0. RAISE ERROR_UPDATE. ENDIF.
*
**>> INVOICE TABLE 상태변경.
*    SELECT  *  FROM  ZTCUCLIV
*    WHERE      ZFBLNO  =  ZTIDR-ZFBLNO
*    AND        ZFCLSEQ =  ZTIDR-ZFCLSEQ.

    UPDATE  ZTIV
       SET     ZFCUST  =  '3'
               UNAM    =  SY-UNAME
               UDAT    =  SY-DATUM
       WHERE   ZFIVNO  =  ZTIDR-ZFIVNO.
       IF SY-SUBRC NE 0. RAISE  ERROR_UPDATE. ENDIF.
*    ENDSELECT.
*  ENDIF.

  UPDATE ZTBL
         SET  ZFBLSDP  = ZTIDR-ZFBLSDP
              ZFBNARCD = ZTIDR-ZFBNARCD
              ZFTRCK   = ZTIDR-ZFTRCK
              ZFTRQDT  = ZTIDR-ZFTRQDT
  WHERE ZFBLNO  EQ  ZFBLNO.

ENDFUNCTION.
