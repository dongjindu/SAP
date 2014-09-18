FUNCTION ZIM_OFFER_SHEET_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFREQNO) LIKE  ZTREQHD-ZFREQNO
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTOFF_OLD) LIKE  ZTOFF STRUCTURE  ZTOFF
*"     VALUE(W_ZTOFF) LIKE  ZTOFF STRUCTURE  ZTOFF
*"     VALUE(W_ZTOFFFTX_OLD) LIKE  ZTOFFFTX STRUCTURE  ZTOFFFTX
*"     VALUE(W_ZTOFFFTX) LIKE  ZTOFFFTX STRUCTURE  ZTOFFFTX
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSOFFO STRUCTURE  ZSOFFO
*"      IT_ZSOFFSDE STRUCTURE  ZSOFFSDE
*"      IT_ZSOFFSG6 STRUCTURE  ZSOFFSG6
*"      IT_ZSOFFSG6G STRUCTURE  ZSOFFSG6G
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
   DATA : W_ZFFLSDE    LIKE ZTOFFSDE-ZFFLSDE,
          W_ZFLSG4     LIKE ZTPURSG4-ZFLSG4.

   MOVE-CORRESPONDING : W_ZTOFF     TO   ZTOFF,
                        W_ZTOFFFTX  TO   ZTOFFFTX.

   MOVE : ZFREQNO  TO    ZTOFF-ZFREQNO,       " Offer Sheet Header
          ZFREQNO  TO    ZTOFFFTX-ZFREQNO,    " Offer Sheet text
          SY-MANDT TO    ZTOFF-MANDT,
          SY-MANDT TO    ZTOFFFTX-MANDT,
          SY-UNAME TO    ZTOFF-UNAM,
          SY-DATUM TO    ZTOFF-UDAT.

   IF W_OK_CODE EQ 'DELE'.
      ZFSTATUS = 'X'.
   ENDIF.

   CASE ZFSTATUS.
      WHEN 'C'.               " 생성
         MOVE : SY-UNAME TO    ZTOFF-ERNAM,
                SY-DATUM TO    ZTOFF-CDAT.

         INSERT     ZTOFF.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         INSERT     ZTOFFFTX.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

* 원산지 ITEM
         LOOP AT IT_ZSOFFO.
            CLEAR : ZTOFFO.
            MOVE-CORRESPONDING IT_ZSOFFO  TO ZTOFFO.
            MOVE : ZFREQNO                TO ZTOFFO-ZFREQNO,
                   SY-MANDT               TO ZTOFFO-MANDT.

            INSERT   ZTOFFO.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDLOOP.
* 기타참조사항
         LOOP AT IT_ZSOFFSDE.
            CLEAR : ZTOFFSDE.
            MOVE-CORRESPONDING IT_ZSOFFSDE TO ZTOFFSDE.
            MOVE : ZFREQNO                 TO ZTOFFSDE-ZFREQNO,
                   SY-MANDT                TO ZTOFFSDE-MANDT.

            INSERT   ZTOFFSDE.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDLOOP.
* 물품명세
         LOOP AT IT_ZSOFFSG6.
            CLEAR : ZTOFFSG6.

            MOVE-CORRESPONDING IT_ZSOFFSG6  TO ZTOFFSG6.
            MOVE : ZFREQNO                TO ZTOFFSG6-ZFREQNO,
                   SY-MANDT               TO ZTOFFSG6-MANDT.

            INSERT   ZTOFFSG6.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDLOOP.

* 규격
         LOOP AT IT_ZSOFFSG6G.
            CLEAR : ZTOFFSG6G.
            MOVE-CORRESPONDING IT_ZSOFFSG6G  TO ZTOFFSG6G.
            MOVE : ZFREQNO                TO ZTOFFSG6G-ZFREQNO,
                   SY-MANDT               TO ZTOFFSG6G-MANDT.

            INSERT   ZTOFFSG6G.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDLOOP.

* 변경이력
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_OFF'
             EXPORTING
                UPD_CHNGIND     =   'I'
                N_ZTOFF         =   ZTOFF
                O_ZTOFF         =   W_ZTOFF_OLD
                N_ZTOFFFTX      =   ZTOFFFTX
                O_ZTOFFFTX      =   W_ZTOFFFTX_OLD.

*-----------------------------------------------------------------------
* DELETE
*-----------------------------------------------------------------------
      WHEN 'X'.               " 삭제
         DELETE FROM ZTOFF   WHERE ZFREQNO EQ ZFREQNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         DELETE FROM ZTOFFFTX WHERE ZFREQNO EQ ZFREQNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* 원산지
         DELETE FROM ZTOFFO WHERE ZFREQNO EQ ZFREQNO.
* 기타참조사항
         DELETE FROM ZTOFFSDE WHERE ZFREQNO EQ ZFREQNO.
* 품목 ITEM
         DELETE FROM ZTOFFSG6 WHERE ZFREQNO EQ ZFREQNO.
* 수입의뢰 ORIGIN
         DELETE FROM ZTOFFSG6G  WHERE ZFREQNO EQ ZFREQNO.

* 변경이력
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_OFF'
             EXPORTING
                UPD_CHNGIND     =   'D'
                N_ZTOFF         =   ZTOFF
                O_ZTOFF         =   W_ZTOFF_OLD
                N_ZTOFFFTX      =   ZTOFFFTX
                O_ZTOFFFTX      =   W_ZTOFFFTX_OLD.

*-----------------------------------------------------------------------
* Update
*-----------------------------------------------------------------------
      WHEN OTHERS.            " 변경
         UPDATE     ZTOFF.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         UPDATE     ZTOFFFTX.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

* 원산지
         SELECT * FROM ZTOFFO  WHERE ZFREQNO  EQ  ZFREQNO.
            READ TABLE IT_ZSOFFO  WITH KEY ZFLO = ZTOFFO-ZFLO
                                  BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSOFFO TO ZTOFFO.
               UPDATE ZTOFFO.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTOFFO.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSOFFO.
            SELECT SINGLE * FROM  ZTOFFO
                            WHERE ZFREQNO  EQ  ZFREQNO
                            AND   ZFLO     EQ  IT_ZSOFFO-ZFLO.
            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSOFFO  TO ZTOFFO.
               MOVE : ZFREQNO                TO ZTOFFO-ZFREQNO,
                      SY-MANDT               TO ZTOFFO-MANDT.
               INSERT  ZTREQIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.
*  기타참조사항
         SELECT * FROM ZTOFFSDE  WHERE ZFREQNO  EQ  ZFREQNO.

            READ TABLE IT_ZSOFFSDE
                 WITH KEY ZFFLSDE = ZTOFFSDE-ZFFLSDE BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSOFFSDE  TO ZTOFFSDE.
               UPDATE ZTOFFSDE.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTOFFSDE.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSOFFSDE.
            SELECT SINGLE * FROM  ZTOFFSDE
                            WHERE ZFREQNO  EQ  ZFREQNO
                            AND   ZFFLSDE  EQ  IT_ZSOFFSDE-ZFFLSDE.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSOFFSDE  TO ZTOFFSDE.
               MOVE : ZFREQNO                  TO ZTOFFSDE-ZFREQNO,
                      SY-MANDT                 TO ZTOFFSDE-MANDT.

               INSERT  ZTOFFSDE.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.
*  품목명세
         SELECT * FROM ZTOFFSG6  WHERE ZFREQNO  EQ  ZFREQNO.

            READ TABLE IT_ZSOFFSG6
                 WITH KEY ZFLSG6  = ZTOFFSG6-ZFLSG6 BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSOFFSG6  TO ZTOFFSG6.
               UPDATE ZTOFFSG6.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTOFFSG6.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSOFFSG6.
            SELECT SINGLE * FROM  ZTOFFSG6
                            WHERE ZFREQNO  EQ  ZFREQNO
                            AND   ZFLSG6   EQ  IT_ZSOFFSG6-ZFLSG6.

            IF SY-SUBRC NE 0.

               MOVE-CORRESPONDING IT_ZSOFFSG6  TO ZTOFFSG6.
               MOVE : ZFREQNO                  TO ZTOFFSG6-ZFREQNO,
                      SY-MANDT                 TO ZTOFFSG6-MANDT.
               INSERT  ZTOFFSG6.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.

*  품목명세  규격
      SELECT * FROM ZTOFFSG6G  WHERE ZFREQNO  EQ  ZFREQNO.

            READ TABLE IT_ZSOFFSG6G
                 WITH KEY ZFLSG6  = ZTOFFSG6G-ZFLSG6
                          ZFLSG6G = ZTOFFSG6G-ZFLSG6G BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSOFFSG6G TO ZTOFFSG6G.
               UPDATE ZTOFFSG6G.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTOFFSG6G.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSOFFSG6G.
            SELECT SINGLE * FROM  ZTOFFSG6G
                            WHERE ZFREQNO  EQ  ZFREQNO
                            AND   ZFLSG6   EQ  IT_ZSOFFSG6G-ZFLSG6
                            AND   ZFLSG6G  EQ  IT_ZSOFFSG6G-ZFLSG6G.

            IF SY-SUBRC NE 0.

               MOVE-CORRESPONDING IT_ZSOFFSG6G TO ZTOFFSG6G.
               MOVE : ZFREQNO                  TO ZTOFFSG6G-ZFREQNO,
                      SY-MANDT                 TO ZTOFFSG6G-MANDT.
               INSERT  ZTOFFSG6G.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.

* 변경이력
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_OFF'
             EXPORTING
                UPD_CHNGIND     =   'U'
                N_ZTOFF         =   ZTOFF
                O_ZTOFF         =   W_ZTOFF_OLD
                N_ZTOFFFTX      =   ZTOFFFTX
                O_ZTOFFFTX      =   W_ZTOFFFTX_OLD.

   ENDCASE.
ENDFUNCTION.
