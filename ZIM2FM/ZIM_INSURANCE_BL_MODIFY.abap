FUNCTION ZIM_INSURANCE_BL_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFBLNO) LIKE  ZTINSB-ZFBLNO
*"     VALUE(ZFINSEQ) LIKE  ZTINSB-ZFINSEQ
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTINSB_OLD) LIKE  ZTINSB STRUCTURE  ZTINSB
*"     VALUE(W_ZTINSB) LIKE  ZTINSB STRUCTURE  ZTINSB
*"     VALUE(W_ZTINSBRSP_OLD) LIKE  ZTINSBRSP STRUCTURE  ZTINSBRSP
*"     VALUE(W_ZTINSBRSP) LIKE  ZTINSBRSP STRUCTURE  ZTINSBRSP
*"     VALUE(W_ZTINSBSG3_OLD) LIKE  ZTINSBSG3 STRUCTURE  ZTINSBSG3
*"     VALUE(W_ZTINSBSG3) LIKE  ZTINSBSG3 STRUCTURE  ZTINSBSG3
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSINSBAGR STRUCTURE  ZSINSBAGR
*"      IT_ZSINSBSG2 STRUCTURE  ZSINSBSG2
*"      IT_ZSINSBSG5 STRUCTURE  ZSINSBSG5
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
DATA : UPD_CHNGIND      LIKE  CDPOS-CHNGIND.

* 삭제 지시자 검증
   IF W_OK_CODE EQ 'DELE'.   ZFSTATUS = 'X'.  ENDIF.

   MOVE-CORRESPONDING : W_ZTINSB     TO   ZTINSB,
                        W_ZTINSBRSP  TO   ZTINSBRSP,
                        W_ZTINSBSG3  TO   ZTINSBSG3.

   MOVE : SY-MANDT TO    ZTINSB-MANDT,
          ZFBLNO   TO    ZTINSB-ZFBLNO,
          ZFINSEQ  TO    ZTINSB-ZFINSEQ,
          SY-MANDT TO    ZTINSBRSP-MANDT,
          ZFBLNO   TO    ZTINSBRSP-ZFBLNO,
          ZFINSEQ  TO    ZTINSBRSP-ZFINSEQ,
          SY-MANDT TO    ZTINSBSG3-MANDT,
          ZFBLNO   TO    ZTINSBSG3-ZFBLNO,
          ZFINSEQ  TO    ZTINSBSG3-ZFINSEQ,
          SY-UNAME TO    ZTINSB-UNAM,
          SY-DATUM TO    ZTINSB-UDAT.

   CASE ZFSTATUS.
      WHEN 'C'.                                        " 생성
         MOVE : SY-UNAME   TO    ZTINSB-ERNAM,         "
                SY-DATUM   TO    ZTINSB-CDAT.          "
         INSERT     ZTINSB.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         INSERT     ZTINSBRSP.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         INSERT     ZTINSBSG3.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* 변경 이력
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_INSB'
             EXPORTING
                 UPD_CHNGIND        =   'I'
                 N_ZTINSB            =   W_ZTINSB
                 O_ZTINSB            =   W_ZTINSB_OLD
                 N_ZTINSBRSP         =   W_ZTINSBRSP
                 O_ZTINSBRSP         =   W_ZTINSBRSP_OLD
                 N_ZTINSBSG3         =   W_ZTINSBSG3
                 O_ZTINSBSG3         =   W_ZTINSBSG3_OLD.

      WHEN 'X'.               " 삭제
* Header
         DELETE  FROM ZTINSB   WHERE ZFBLNO  EQ ZFBLNO
                               AND   ZFINSEQ EQ ZFINSEQ.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* Response
         DELETE  FROM ZTINSBRSP   WHERE ZFBLNO  EQ ZFBLNO
                                  AND   ZFINSEQ EQ ZFINSEQ.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* Seg. 3
         DELETE  FROM ZTINSBSG3   WHERE ZFBLNO  EQ ZFBLNO
                                  AND   ZFINSEQ EQ ZFINSEQ.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* Seg. AGR
         DELETE  FROM ZTINSBAGR  WHERE ZFBLNO  EQ ZFBLNO
                                 AND   ZFINSEQ EQ ZFINSEQ.
* Seg. SG 2
         DELETE  FROM ZTINSBSG2  WHERE ZFBLNO  EQ ZFBLNO
                                 AND   ZFINSEQ EQ ZFINSEQ.
         DELETE  FROM ZTINSBSG5  WHERE ZFBLNO  EQ ZFBLNO
                                 AND   ZFINSEQ EQ ZFINSEQ.
* 변경 이력
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_INSB'
             EXPORTING
                 UPD_CHNGIND        =   'D'
                 N_ZTINSB            =   W_ZTINSB
                 O_ZTINSB            =   W_ZTINSB_OLD
                 N_ZTINSBRSP         =   W_ZTINSBRSP
                 O_ZTINSBRSP         =   W_ZTINSBRSP_OLD
                 N_ZTINSBSG3         =   W_ZTINSBSG3
                 O_ZTINSBSG3         =   W_ZTINSBSG3_OLD.

         EXIT.
*-----------------------------------------------------------------------
* UPDATE
*-----------------------------------------------------------------------
      WHEN OTHERS.            " 변경
         UPDATE     ZTINSB.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         UPDATE     ZTINSBRSP.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         UPDATE     ZTINSBSG3.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* 변경 이력
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_INSB'
             EXPORTING
                 UPD_CHNGIND        =   'U'
                 N_ZTINSB            =   W_ZTINSB
                 O_ZTINSB            =   W_ZTINSB_OLD
                 N_ZTINSBRSP         =   W_ZTINSBRSP
                 O_ZTINSBRSP         =   W_ZTINSBRSP_OLD
                 N_ZTINSBSG3         =   W_ZTINSBSG3
                 O_ZTINSBSG3         =   W_ZTINSBSG3_OLD.
*-----------------------------------------------------------------------

   ENDCASE.

* 보험 부보 Seg. AGR
   SELECT * FROM ZTINSBAGR WHERE ZFBLNO  EQ  ZFBLNO
                           AND   ZFINSEQ  EQ  ZFINSEQ.

      READ TABLE IT_ZSINSBAGR WITH KEY ZFLAGR = ZTINSBAGR-ZFLAGR
                                                BINARY SEARCH.
      IF SY-SUBRC EQ 0.
         MOVE-CORRESPONDING IT_ZSINSBAGR TO ZTINSBAGR.
         MOVE : SY-MANDT     TO   ZTINSBAGR-MANDT,
                ZFBLNO       TO   ZTINSBAGR-ZFBLNO,
                ZFINSEQ      TO   ZTINSBAGR-ZFINSEQ.
         UPDATE ZTINSBAGR.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ELSE.
         DELETE ZTINSBAGR.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDSELECT.

   LOOP AT IT_ZSINSBAGR.
      SELECT SINGLE * FROM  ZTINSBAGR
                      WHERE ZFBLNO   EQ  ZFBLNO
                      AND   ZFINSEQ  EQ  ZFINSEQ
                      AND   ZFLAGR   EQ  IT_ZSINSBAGR-ZFLAGR.
      IF SY-SUBRC NE 0.
         MOVE-CORRESPONDING IT_ZSINSBAGR TO ZTINSBAGR.
         MOVE : SY-MANDT     TO   ZTINSBAGR-MANDT,
                ZFBLNO       TO   ZTINSBAGR-ZFBLNO,
                ZFINSEQ      TO   ZTINSBAGR-ZFINSEQ.
         INSERT  ZTINSBAGR.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDLOOP.
* 보험 부보 Seg. 2
   SELECT * FROM ZTINSBSG2 WHERE ZFBLNO   EQ ZFBLNO
                           AND   ZFINSEQ  EQ ZFINSEQ.

      READ TABLE IT_ZSINSBSG2 WITH KEY ZFLSG2 = ZTINSBSG2-ZFLSG2
                                                BINARY SEARCH.
      IF SY-SUBRC EQ 0.
         MOVE-CORRESPONDING IT_ZSINSBSG2 TO ZTINSBSG2.
         MOVE : SY-MANDT     TO   ZTINSBSG2-MANDT,
                ZFBLNO       TO   ZTINSBSG2-ZFBLNO,
                ZFINSEQ      TO   ZTINSBSG2-ZFINSEQ.
         UPDATE ZTINSBSG2.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ELSE.
         DELETE ZTINSBSG2.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDSELECT.

   LOOP AT IT_ZSINSBSG2.
      SELECT SINGLE * FROM  ZTINSBSG2
                      WHERE ZFBLNO   EQ  ZFBLNO
                      AND   ZFINSEQ  EQ  ZFINSEQ
                      AND   ZFLSG2   EQ  IT_ZSINSBSG2-ZFLSG2.
      IF SY-SUBRC NE 0.
         MOVE-CORRESPONDING IT_ZSINSBSG2 TO ZTINSBSG2.
         MOVE : SY-MANDT     TO   ZTINSBSG2-MANDT,
                ZFBLNO       TO   ZTINSBSG2-ZFBLNO,
                ZFINSEQ      TO   ZTINSBSG2-ZFINSEQ.
         INSERT  ZTINSBSG2.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDLOOP.
* 보험 부보 Seg. 5
   SELECT * FROM ZTINSBSG5 WHERE ZFBLNO   EQ ZFBLNO
                           AND   ZFINSEQ  EQ ZFINSEQ.

      READ TABLE IT_ZSINSBSG5 WITH KEY ZFLSG5 = ZTINSBSG5-ZFLSG5
                                                BINARY SEARCH.
      IF SY-SUBRC EQ 0.
         MOVE-CORRESPONDING IT_ZSINSBSG5 TO ZTINSBSG5.
         MOVE : SY-MANDT     TO   ZTINSBSG5-MANDT,
                ZFBLNO       TO   ZTINSBSG5-ZFBLNO,
                ZFINSEQ      TO   ZTINSBSG5-ZFINSEQ.
         UPDATE ZTINSBSG5.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
        ELSE.
         DELETE ZTINSBSG5.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDSELECT.

   LOOP AT IT_ZSINSBSG5.
      SELECT SINGLE * FROM  ZTINSBSG5
                      WHERE ZFBLNO   EQ  ZFBLNO
                      AND   ZFINSEQ  EQ  ZFINSEQ
                      AND   ZFLSG5   EQ  IT_ZSINSBSG5-ZFLSG5.
      IF SY-SUBRC NE 0.
         MOVE-CORRESPONDING IT_ZSINSBSG5 TO ZTINSBSG5.
         MOVE : SY-MANDT     TO   ZTINSBSG5-MANDT,
                ZFBLNO       TO   ZTINSBSG5-ZFBLNO,
                ZFINSEQ      TO   ZTINSBSG5-ZFINSEQ.
         INSERT  ZTINSBSG5.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDLOOP.

ENDFUNCTION.
