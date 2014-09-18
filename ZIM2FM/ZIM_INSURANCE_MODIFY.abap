FUNCTION ZIM_INSURANCE_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFREQNO) LIKE  ZTINS-ZFREQNO
*"     VALUE(ZFINSEQ) LIKE  ZTINS-ZFINSEQ
*"     VALUE(ZFAMDNO) LIKE  ZTINS-ZFAMDNO
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTINS_OLD) LIKE  ZTINS STRUCTURE  ZTINS
*"     VALUE(W_ZTINS) LIKE  ZTINS STRUCTURE  ZTINS
*"     VALUE(W_ZTINSRSP_OLD) LIKE  ZTINSRSP STRUCTURE  ZTINSRSP
*"     VALUE(W_ZTINSRSP) LIKE  ZTINSRSP STRUCTURE  ZTINSRSP
*"     VALUE(W_ZTINSSG3_OLD) LIKE  ZTINSSG3 STRUCTURE  ZTINSSG3
*"     VALUE(W_ZTINSSG3) LIKE  ZTINSSG3 STRUCTURE  ZTINSSG3
*"     VALUE(W_OK_CODE)
*"     VALUE(W_AMEND)
*"  TABLES
*"      IT_ZSINSAGR STRUCTURE  ZSINSAGR
*"      IT_ZSINSSG2 STRUCTURE  ZSINSSG2
*"      IT_ZSINSSG5 STRUCTURE  ZSINSSG5
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
DATA : UPD_CHNGIND      LIKE  CDPOS-CHNGIND.

* 삭제 지시자 검증
   IF W_OK_CODE EQ 'DELE'.   ZFSTATUS = 'X'.  ENDIF.

   MOVE-CORRESPONDING : W_ZTINS     TO   ZTINS,
                        W_ZTINSRSP  TO   ZTINSRSP,
                        W_ZTINSSG3  TO   ZTINSSG3.

*>> 보험문서 생성시... --> 횟차 증가..
*   IF ZFSTATUS EQ 'C'.
*      IF ZFAMDNO IS INITIAL.
*         SELECT MAX( ZFINSEQ ) INTO ZFINSEQ
*                FROM ZTINS
*                WHERE ZFREQNO  EQ  ZFREQNO.
*         ZFINSEQ = ZFINSEQ + 1.
*      ENDIF.
*   ENDIF.

   MOVE : SY-MANDT TO    ZTINS-MANDT,         " insurance Header
          ZFREQNO  TO    ZTINS-ZFREQNO,       " insurance Header
          ZFAMDNO  TO    ZTINS-ZFAMDNO,       " insurance Header
          ZFINSEQ  TO    ZTINS-ZFINSEQ,       " 회수.
          SY-MANDT TO    ZTINSRSP-MANDT,      " insurance response
          ZFREQNO  TO    ZTINSRSP-ZFREQNO,    " insurance response
          ZFAMDNO  TO    ZTINSRSP-ZFAMDNO,    " insurance response
          ZFINSEQ  TO    ZTINSRSP-ZFINSEQ,    " 회수.
          ZFREQNO  TO    ZTINSSG3-MANDT,      " insurance SG 3.
          ZFREQNO  TO    ZTINSSG3-ZFREQNO,    " insurance SG 3.
          ZFAMDNO  TO    ZTINSSG3-ZFAMDNO,    " insurance SG 3.
          ZFINSEQ  TO    ZTINSSG3-ZFINSEQ,    " 회수.
          SY-UNAME TO    ZTINS-UNAM,          "
          SY-DATUM TO    ZTINS-UDAT.          "

   CASE ZFSTATUS.
      WHEN 'C'.               " 생성
         MOVE : SY-UNAME   TO    ZTINS-ERNAM,         "
                SY-DATUM   TO    ZTINS-CDAT.          "
         INSERT     ZTINS.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         INSERT     ZTINSRSP.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         INSERT     ZTINSSG3.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* 변경 이력
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_INS'
             EXPORTING
                 UPD_CHNGIND        =   'I'
                 N_ZTINS            =   W_ZTINS
                 O_ZTINS            =   W_ZTINS_OLD
                 N_ZTINSRSP         =   W_ZTINSRSP
                 O_ZTINSRSP         =   W_ZTINSRSP_OLD
                 N_ZTINSSG3         =   W_ZTINSSG3
                 O_ZTINSSG3         =   W_ZTINSSG3_OLD.

         IF NOT ZFAMDNO IS INITIAL.
            W_ZFAMDNO = ZFAMDNO - 1.
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_INS_BEFORE'
               EXPORTING
                  W_ZFREQNO   =   ZFREQNO
                  W_ZFINSEQ   =   ZFINSEQ
                  W_ZFAMDNO   =   W_ZFAMDNO
                  W_ZFDOCST   =   'A'.
         ENDIF.

      WHEN 'X'.               " 삭제
* Header
         DELETE  FROM ZTINS   WHERE ZFREQNO EQ ZFREQNO
                              AND   ZFINSEQ EQ ZFINSEQ
                              AND   ZFAMDNO EQ ZFAMDNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* Response
         DELETE  FROM ZTINSRSP   WHERE ZFREQNO EQ ZFREQNO
                                 AND   ZFINSEQ EQ ZFINSEQ
                                 AND   ZFAMDNO EQ ZFAMDNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* Seg. 3
         DELETE  FROM ZTINSSG3   WHERE ZFREQNO EQ ZFREQNO
                                 AND   ZFINSEQ EQ ZFINSEQ
                                 AND   ZFAMDNO EQ ZFAMDNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* Seg. AGR
         DELETE  FROM ZTINSAGR   WHERE ZFREQNO EQ ZFREQNO
                                 AND   ZFINSEQ EQ ZFINSEQ
                                 AND   ZFAMDNO EQ ZFAMDNO.
* Seg. SG 2
         DELETE  FROM ZTINSSG2   WHERE ZFREQNO EQ ZFREQNO
                                 AND   ZFINSEQ EQ ZFINSEQ
                                 AND   ZFAMDNO EQ ZFAMDNO.
         IF ZFAMDNO IS INITIAL.
* Seg. SG 5
            DELETE  FROM ZTINSSG5   WHERE ZFREQNO EQ ZFREQNO
                                    AND   ZFINSEQ EQ ZFINSEQ.
         ENDIF.
* 변경 이력
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_INS'
             EXPORTING
                 UPD_CHNGIND        =   'D'
                 N_ZTINS            =   W_ZTINS
                 O_ZTINS            =   W_ZTINS_OLD
                 N_ZTINSRSP         =   W_ZTINSRSP
                 O_ZTINSRSP         =   W_ZTINSRSP_OLD
                 N_ZTINSSG3         =   W_ZTINSSG3
                 O_ZTINSSG3         =   W_ZTINSSG3_OLD.

         IF NOT ZFAMDNO IS INITIAL.
            W_ZFAMDNO = ZFAMDNO - 1.
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_INS_BEFORE'
               EXPORTING
                  W_ZFREQNO   =   ZFREQNO
                  W_ZFINSEQ   =   ZFINSEQ
                  W_ZFAMDNO   =   W_ZFAMDNO
                  W_ZFDOCST   =   'O'.
         ENDIF.
*-----------------------------------------------------------------------
* L/C COST 반영
* OPEN 문서 DELETE
         IF W_ZTINS_OLD-ZFDOCST EQ 'O'.
            UPD_CHNGIND = 'D'.

            W_ZTINS      =  W_ZTINS_OLD.
            W_ZTINSRSP   =  W_ZTINSRSP_OLD.
            IF W_ZTINS-ZFAMDNO GT '00000'.
               W_ZFAMDNO = W_ZTINS-ZFAMDNO - 1.
               SELECT SINGLE * INTO W_ZTINS_OLD FROM ZTINS
                               WHERE ZFREQNO  EQ  W_ZTINS-ZFREQNO
                               AND   ZFINSEQ  EQ  W_ZTINS-ZFINSEQ
                               AND   ZFAMDNO  EQ  W_ZFAMDNO.

               SELECT SINGLE * INTO W_ZTINSRSP_OLD FROM ZTINSRSP
                               WHERE ZFREQNO  EQ  W_ZTINS-ZFREQNO
                               AND   ZFINSEQ  EQ  W_ZTINS-ZFINSEQ
                               AND   ZFAMDNO  EQ  W_ZFAMDNO.
            ELSE.
               CLEAR : W_ZTINS_OLD, W_ZTINSRSP_OLD.
            ENDIF.

            CALL FUNCTION 'ZIM_SET_INSURANCE_COST'
                 EXPORTING
                        UPD_CHNGIND    =     UPD_CHNGIND
                        N_ZTINS        =     W_ZTINS
                        O_ZTINS        =     W_ZTINS_OLD
                        N_ZTINSRSP     =     W_ZTINSRSP
                        O_ZTINSRSP     =     W_ZTINSRSP_OLD.
         ENDIF.
*-----------------------------------------------------------------------


         EXIT.
*-----------------------------------------------------------------------
* UPDATE
*-----------------------------------------------------------------------
      WHEN OTHERS.            " 변경
         UPDATE     ZTINS.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         UPDATE     ZTINSRSP.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         UPDATE     ZTINSSG3.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* 변경 이력
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_INS'
             EXPORTING
                 UPD_CHNGIND        =   'U'
                 N_ZTINS            =   W_ZTINS
                 O_ZTINS            =   W_ZTINS_OLD
                 N_ZTINSRSP         =   W_ZTINSRSP
                 O_ZTINSRSP         =   W_ZTINSRSP_OLD
                 N_ZTINSSG3         =   W_ZTINSSG3
                 O_ZTINSSG3         =   W_ZTINSSG3_OLD.
*-----------------------------------------------------------------------
* L/C COST 반영
         IF W_ZTINS-ZFDOCST EQ 'O'.
            IF W_ZTINS-ZFDOCST EQ '00000'.
               IF W_ZTINS-ZFDOCST EQ W_ZTINS_OLD-ZFDOCST.
                  UPD_CHNGIND = 'U'.
               ELSE.
                  UPD_CHNGIND = 'I'.
               ENDIF.
            ELSE.
               IF W_ZTINS-ZFDOCST EQ W_ZTINS_OLD-ZFDOCST.
                  UPD_CHNGIND = 'U'.
               ELSE.
                  W_ZFAMDNO = W_ZTINS-ZFAMDNO - 1.
                  SELECT SINGLE * INTO W_ZTINS_OLD FROM ZTINS
                                  WHERE ZFREQNO  EQ  W_ZTINS-ZFREQNO
                                  AND   ZFINSEQ  EQ  W_ZTINS-ZFINSEQ
                                  AND   ZFAMDNO  EQ  W_ZFAMDNO.
                  SELECT SINGLE * INTO W_ZTINSRSP_OLD FROM ZTINSRSP
                                  WHERE ZFREQNO  EQ  W_ZTINS-ZFREQNO
                                  AND   ZFINSEQ  EQ  W_ZTINS-ZFINSEQ
                                  AND   ZFAMDNO  EQ  W_ZFAMDNO.
                  UPD_CHNGIND = 'U'.
               ENDIF.
            ENDIF.

            CALL FUNCTION 'ZIM_SET_INSURANCE_COST'
                 EXPORTING
                        UPD_CHNGIND    =     UPD_CHNGIND
                        N_ZTINS        =     W_ZTINS
                        O_ZTINS        =     W_ZTINS_OLD
                        N_ZTINSRSP     =     W_ZTINSRSP
                        O_ZTINSRSP     =     W_ZTINSRSP_OLD.
* OPEN CANCEL
         ELSEIF W_ZTINS_OLD-ZFDOCST EQ 'O'.
            UPD_CHNGIND = 'D'.

            W_ZTINS      =  W_ZTINS_OLD.
            W_ZTINSRSP   =  W_ZTINSRSP_OLD.

            W_ZFAMDNO = W_ZTINS-ZFAMDNO - 1.
            SELECT SINGLE * INTO W_ZTINS_OLD FROM ZTINS
                            WHERE ZFREQNO  EQ  W_ZTINS-ZFREQNO
                            AND   ZFINSEQ  EQ  W_ZTINS-ZFINSEQ
                            AND   ZFAMDNO  EQ  W_ZFAMDNO.

            SELECT SINGLE * INTO W_ZTINSRSP_OLD FROM ZTINSRSP
                            WHERE ZFREQNO  EQ  W_ZTINS-ZFREQNO
                            AND   ZFINSEQ  EQ  W_ZTINS-ZFINSEQ
                            AND   ZFAMDNO  EQ  W_ZFAMDNO.

            CALL FUNCTION 'ZIM_SET_INSURANCE_COST'
                 EXPORTING
                        UPD_CHNGIND    =     UPD_CHNGIND
                        N_ZTINS        =     W_ZTINS
                        O_ZTINS        =     W_ZTINS_OLD
                        N_ZTINSRSP     =     W_ZTINSRSP
                        O_ZTINSRSP     =     W_ZTINSRSP_OLD.
         ENDIF.
*-----------------------------------------------------------------------

   ENDCASE.

* 보험 부보 Seg. AGR
   SELECT * FROM ZTINSAGR WHERE ZFREQNO  EQ  ZFREQNO
                          AND   ZFINSEQ  EQ  ZFINSEQ
                          AND   ZFAMDNO  EQ  ZFAMDNO.

      READ TABLE IT_ZSINSAGR WITH KEY ZFLAGR = ZTINSAGR-ZFLAGR
                                               BINARY SEARCH.
      IF SY-SUBRC EQ 0.
         MOVE-CORRESPONDING IT_ZSINSAGR TO ZTINSAGR.
         MOVE : SY-MANDT     TO   ZTINSAGR-MANDT,
                ZFREQNO      TO   ZTINSAGR-ZFREQNO,
                ZFAMDNO      TO   ZTINSAGR-ZFAMDNO,
                ZFINSEQ      TO   ZTINSAGR-ZFINSEQ.
         UPDATE ZTINSAGR.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ELSE.
         DELETE ZTINSAGR.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDSELECT.

   LOOP AT IT_ZSINSAGR.
      SELECT SINGLE * FROM  ZTINSAGR
                      WHERE ZFREQNO  EQ  ZFREQNO
                      AND   ZFINSEQ  EQ  ZFINSEQ
                      AND   ZFAMDNO  EQ  ZFAMDNO
                      AND   ZFLAGR   EQ  IT_ZSINSAGR-ZFLAGR.
      IF SY-SUBRC NE 0.
         MOVE-CORRESPONDING IT_ZSINSAGR TO ZTINSAGR.
         MOVE : SY-MANDT     TO   ZTINSAGR-MANDT,
                ZFREQNO      TO   ZTINSAGR-ZFREQNO,
                ZFAMDNO      TO   ZTINSAGR-ZFAMDNO,
                ZFINSEQ      TO   ZTINSAGR-ZFINSEQ.
         INSERT  ZTINSAGR.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDLOOP.
* 보험 부보 Seg. 2
   SELECT * FROM ZTINSSG2 WHERE ZFREQNO  EQ ZFREQNO
                          AND   ZFINSEQ  EQ ZFINSEQ
                          AND   ZFAMDNO  EQ ZFAMDNO.

      READ TABLE IT_ZSINSSG2 WITH KEY ZFLSG2 = ZTINSSG2-ZFLSG2
                                               BINARY SEARCH.
      IF SY-SUBRC EQ 0.
         MOVE-CORRESPONDING IT_ZSINSSG2 TO ZTINSSG2.
         MOVE : SY-MANDT     TO   ZTINSSG2-MANDT,
                ZFREQNO      TO   ZTINSSG2-ZFREQNO,
                ZFAMDNO      TO   ZTINSSG2-ZFAMDNO,
                ZFINSEQ      TO   ZTINSSG2-ZFINSEQ.
         UPDATE ZTINSSG2.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ELSE.
         DELETE ZTINSSG2.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDSELECT.

   LOOP AT IT_ZSINSSG2.
      SELECT SINGLE * FROM  ZTINSSG2
                      WHERE ZFREQNO  EQ  ZFREQNO
                      AND   ZFINSEQ  EQ  ZFINSEQ
                      AND   ZFAMDNO  EQ  ZFAMDNO
                      AND   ZFLSG2   EQ  IT_ZSINSSG2-ZFLSG2.
      IF SY-SUBRC NE 0.
         MOVE-CORRESPONDING IT_ZSINSSG2 TO ZTINSSG2.
         MOVE : SY-MANDT     TO   ZTINSSG2-MANDT,
                ZFREQNO      TO   ZTINSSG2-ZFREQNO,
                ZFAMDNO      TO   ZTINSSG2-ZFAMDNO,
                ZFINSEQ      TO   ZTINSSG2-ZFINSEQ.
         INSERT  ZTINSSG2.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDLOOP.
* 보험 부보 Seg. 5
   SELECT * FROM ZTINSSG5 WHERE ZFREQNO  EQ ZFREQNO
                          AND   ZFINSEQ  EQ ZFINSEQ.

      READ TABLE IT_ZSINSSG5 WITH KEY ZFLSG5 = ZTINSSG5-ZFLSG5
                                               BINARY SEARCH.
      IF SY-SUBRC EQ 0.
         MOVE-CORRESPONDING IT_ZSINSSG5 TO ZTINSSG5.
         MOVE : SY-MANDT     TO   ZTINSSG5-MANDT,
                ZFREQNO      TO   ZTINSSG5-ZFREQNO,
                ZFINSEQ      TO   ZTINSSG5-ZFINSEQ.
         UPDATE ZTINSSG5.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
        ELSE.
         DELETE ZTINSSG5.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDSELECT.

   LOOP AT IT_ZSINSSG5.
      SELECT SINGLE * FROM  ZTINSSG5
                      WHERE ZFREQNO  EQ  ZFREQNO
                      AND   ZFINSEQ  EQ  ZFINSEQ
                      AND   ZFLSG5   EQ  IT_ZSINSSG5-ZFLSG5.
      IF SY-SUBRC NE 0.
         MOVE-CORRESPONDING IT_ZSINSSG5 TO ZTINSSG5.
         MOVE : SY-MANDT     TO   ZTINSSG5-MANDT,
                ZFREQNO      TO   ZTINSSG5-ZFREQNO,
                ZFINSEQ      TO   ZTINSSG5-ZFINSEQ.
         INSERT  ZTINSSG5.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDLOOP.

ENDFUNCTION.
