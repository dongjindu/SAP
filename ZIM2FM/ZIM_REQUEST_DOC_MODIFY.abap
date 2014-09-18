FUNCTION ZIM_REQUEST_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFREQNO) LIKE  ZTREQHD-ZFREQNO
*"     VALUE(ZFAMDNO) LIKE  ZTREQST-ZFAMDNO
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTREQHD_OLD) LIKE  ZTREQHD STRUCTURE  ZTREQHD OPTIONAL
*"     VALUE(W_ZTREQHD) LIKE  ZTREQHD STRUCTURE  ZTREQHD
*"     VALUE(W_ZTREQST) LIKE  ZTREQST STRUCTURE  ZTREQST
*"     VALUE(W_OK_CODE)
*"     VALUE(W_MODIFY) TYPE  C DEFAULT 'X'
*"  TABLES
*"      IT_ZSREQIT STRUCTURE  ZSREQIT OPTIONAL
*"      IT_ZSREQIT_OLD STRUCTURE  ZSREQIT OPTIONAL
*"      IT_ZTREQORJ STRUCTURE  ZSMLCSG7O OPTIONAL
*"      IT_ZTREQORJ_OLD STRUCTURE  ZSMLCSG7O OPTIONAL
*"      IT_ZSREQIL STRUCTURE  ZSREQIL OPTIONAL
*"      IT_ZSREQIL_OLD STRUCTURE  ZSREQIL OPTIONAL
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
DATA : W_ZFDOCST   LIKE    ZTREQST-ZFDOCST.

   MOVE-CORRESPONDING : W_ZTREQHD   TO   ZTREQHD,
                        W_ZTREQST   TO   ZTREQST.

   MOVE : ZFREQNO  TO    ZTREQHD-ZFREQNO,     " 수입의뢰 header
          ZFREQNO  TO    ZTREQST-ZFREQNO,     " 수입의뢰 상?
          ZFAMDNO  TO    ZTREQST-ZFAMDNO,     "      "
          SY-MANDT TO    ZTREQHD-MANDT,
          SY-MANDT TO    ZTREQST-MANDT,
          SY-DATUM TO    ZTREQST-UDAT,
          SY-UNAME TO    ZTREQST-UNAM,
          ZTREQHD-ZFLASTAM TO  ZTREQST-ZFOPAMT,
          ZTREQHD-WAERS    TO  ZTREQST-WAERS,
          ZTREQHD-ZFUSDAM  TO  ZTREQST-ZFUSDAM,
          ZTREQHD-ZFUSD    TO  ZTREQST-ZFUSD.
*>> RELEASE 여부에 따라서 DATA SETTING!
*>> 2001/03/13 --> 강석봉 수정 작업.
*   SELECT  SINGLE * FROM ZTIMIMG00.
*   IF ZTIMIMG00-ZFRELYN3 NE 'X' .
*      MOVE  ZTREQST-ZFREQDT  TO  ZTREQST-ZFAPPDT.
*      MOVE  SY-DATUM         TO  ZTREQST-ZFRVDT.
*      MOVE  SY-UNAME         TO  ZTREQST-ZFOPNNM.
*   ENDIF.

   SELECT SINGLE * FROM ZTIMIMG00.

   IF W_OK_CODE EQ 'DELE'.   ZFSTATUS = 'X'.   ENDIF.

   IF ZFSTATUS NE 'X'.
*--------< 구매오더 환율 고정지시자 마크>---------------------------
     IF ( ZTREQHD-ZFREQTY EQ 'TT' AND   ZTREQHD-ZFBACD  EQ 'B'
                                  AND   ZTREQST-ZFDOCST EQ 'O' ) OR
        (  ( ZTREQHD-ZFREQTY EQ 'LO' OR  ZTREQHD-ZFREQTY EQ 'PU' )
                                     AND ZTREQST-ZFDOCST EQ 'O' ).


*       ZTREQHD-ZFPREPAY GT 0 ) AND SY-TCODE EQ 'ZIM07'.
*>> P/O 고정환율 지시자 검증.
        IF ZTIMIMG00-ZFEXFIX EQ 'X'.
           SELECT SINGLE * FROM EKKO
                  WHERE    EBELN EQ ZTREQHD-EBELN.
           IF EKKO-WAERS NE ZTREQHD-WAERS.
              MESSAGE E504 WITH EKKO-EBELN EKKO-WAERS ZTREQHD-WAERS.
           ENDIF.
           IF NOT ( EKKO-WAERS EQ ZTREQHD-WAERS   AND
                    EKKO-WKURS EQ ZTREQHD-KURSF   AND
                    EKKO-KUFIX EQ 'X' ).
              CLEAR : BAPIMEPOHEADER,
                      BAPIMEPOHEADERX.
              MOVE : 'X'        TO      BAPIMEPOHEADERX-EXCH_RATE,
                     'X'        TO      BAPIMEPOHEADERX-EX_RATE_FX,
                     ZTREQHD-EBELN TO   BAPIMEPOHEADER-PO_NUMBER,
                     ZTREQHD-KURSF TO   BAPIMEPOHEADER-EXCH_RATE,
                     'X'           TO   BAPIMEPOHEADER-EX_RATE_FX.
*>>> BAPIs Functions
              CALL FUNCTION 'ZIM_BAPI_PO_CHANGE'
                   EXPORTING
                      PURCHASEORDER = ZTREQHD-EBELN
                      POHEADER      = BAPIMEPOHEADER
                      POHEADERX     = BAPIMEPOHEADERX
                   TABLES
                      RETURN        = XRETURN.

              LOOP AT XRETURN WHERE TYPE EQ 'E'.
                 MESSAGE ID     XRETURN-ID
                         TYPE   XRETURN-TYPE
                         NUMBER XRETURN-NUMBER
                         WITH   XRETURN-MESSAGE_V1
                                XRETURN-MESSAGE_V2
                                XRETURN-MESSAGE_V3
                                XRETURN-MESSAGE_V4.
              ENDLOOP.
              IF SY-SUBRC EQ 0.
*                 COMMIT WORK.
                 CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
              ELSE.
*                 ROLLBACK WORK.
                 CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              ENDIF.
           ENDIF.
         ENDIF.
      ENDIF.
   ENDIF.

*> DB변경 없을 경우...
   CHECK W_MODIFY EQ 'X'.

   CASE ZFSTATUS.
      WHEN 'C'.               " 생?
* 상태 테이블 insert

         IF ZFAMDNO IS INITIAL.
            MOVE : SY-DATUM TO    ZTREQST-CDAT,
                   SY-UNAME TO    ZTREQST-ERNAM.
            INSERT     ZTREQHD.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

            INSERT     ZTREQST.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         ELSE.
            W_ZFAMDNO = ZFAMDNO - 1.

            UPDATE     ZTREQHD.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
*-----------------------------------------------------------------------
* 2000/05/18 amend시 과거 Data Temp Tables에 보?
*-----------------------------------------------------------------------
* Header Tables
            move-corresponding   W_ZTREQHD_OLD  to  ztreqhd_tmp.
            ZTREQHD_TMP-ZFAMDNO = W_ZFAMDNO.
            insert     ztreqhd_tmp.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* Item Tables
            LOOP AT IT_ZSREQIT_OLD.
               MOVE-CORRESPONDING  it_zsreqit_old  TO ZTREQIT_TMP.
               ZTREQIT_TMP-ZFAMDNO = W_ZFAMDNO.
               insert ztreqit_tmp.
            ENDLOOP.
* Orijin Tables
            LOOP AT IT_ZTREQORJ_OLD.
               MOVE-CORRESPONDING  it_ztreqorj_old  TO ZTREQORJ_TMP.
               ZTREQORJ_TMP-ZFAMDNO = W_ZFAMDNO.
               insert ztreqORJ_tmp.
            ENDLOOP.
* 수입추천 내역 Amend용 Temp table
            LOOP AT IT_ZSREQIL_OLD.
               MOVE-CORRESPONDING  it_zsreqIL_old  TO ZTREQIL_TMP.
               ZTREQIL_TMP-ZFAMDNO = W_ZFAMDNO.
               insert ztreqIL_tmp.
            ENDLOOP.
*-----------------------------------------------------------------------
* 이전 문서 상태 Amend로 변?
*-----------------------------------------------------------------------
            W_ZFDOCST = 'A'.

            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BEFORE'
                EXPORTING
                      W_ZFREQNO    =   ZFREQNO
                      W_ZFAMDNO    =   W_ZFAMDNO
                      W_ZFDOCST    =   W_ZFDOCST.

         ENDIF.

*-----------------------------------------------------------------------
* DELETE
*-----------------------------------------------------------------------
      WHEN 'X'.               " 삭?
         DELETE FROM ZTREQST WHERE ZFREQNO EQ ZFREQNO
                             AND   ZFAMDNO EQ ZFAMDNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* 품목 ITEM
         DELETE FROM ZTREQIT   WHERE ZFREQNO EQ ZFREQNO.
* 수입의뢰 ORIGIN
         DELETE FROM ZTREQORJ  WHERE ZFREQNO EQ ZFREQNO.
* 수입추?
         DELETE FROM ZTREQIL   WHERE ZFREQNO EQ ZFREQNO.
         IF ZFAMDNO IS INITIAL.
* 수입의뢰 비용.
            DELETE FROM ZTRECST   WHERE ZFREQNO EQ ZFREQNO.
            DELETE FROM ZTREQHD WHERE ZFREQNO EQ ZFREQNO.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ELSE.
            W_ZFAMDNO = ZFAMDNO - 1.
*-----------------------------------------------------------------------
* Header Tables
            SELECT SINGLE * INTO CORRESPONDING FIELDS OF ZTREQHD
                                 FROM ZTREQHD_TMP
                                 WHERE  ZFREQNO  EQ  ZFREQNO
                                 AND    ZFAMDNO  EQ  W_ZFAMDNO.
            UPDATE     ZTREQHD.
* Item Tables
            REFRESH : IT_ZSREQIT_OLD.
            SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQIT_OLD
                                        FROM  ZTREQIT_TMP
                                        WHERE ZFREQNO   EQ  ZFREQNO
                                        AND   ZFAMDNO   EQ  W_ZFAMDNO
                                        ORDER BY ZFITMNO.
            LOOP AT IT_ZSREQIT_OLD.
               IT_ZSREQIT_OLD-EBELP = IT_ZSREQIT_OLD-ZFITMNO.
               MODIFY IT_ZSREQIT_OLD INDEX SY-TABIX.
            ENDLOOP.
            INSERT ZTREQIT  FROM TABLE IT_ZSREQIT_OLD.
* Orijin Tables
            REFRESH : IT_ZTREQORJ_OLD.
            SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTREQORJ_OLD
                                        FROM   ZTREQORJ_TMP
                                        WHERE  ZFREQNO  EQ  ZFREQNO
                                        AND    ZFAMDNO  EQ  W_ZFAMDNO
                                        ORDER BY ZFLSG7O.
            INSERT ZTREQORJ FROM TABLE IT_ZTREQORJ_OLD.
* 수입추천 내역 Amend용 Temp table
            REFRESH : IT_ZSREQIL_OLD.
            SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQIL_OLD
                                        FROM   ZTREQIL_TMP
                                        WHERE  ZFREQNO  EQ  ZFREQNO
                                        AND    ZFAMDNO  EQ  W_ZFAMDNO
                                        ORDER BY  ZFILSEQ.
            INSERT ZTREQIL FROM TABLE IT_ZSREQIL_OLD.
*-----------------------------------------------------------------------
            W_ZFDOCST = 'O'.
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BEFORE'
                EXPORTING
                      W_ZFREQNO    =   ZFREQNO
                      W_ZFAMDNO    =   W_ZFAMDNO
                      W_ZFDOCST    =   W_ZFDOCST.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

*-----------------------------------------------------------------------
* Item Tables
            DELETE FROM ZTREQIT_TMP  WHERE ZFREQNO   EQ   ZFREQNO
                                     AND   ZFAMDNO   EQ W_ZFAMDNO.
* Orijin Tables
            DELETE FROM ZTREQORJ_TMP WHERE ZFREQNO EQ ZFREQNO
                                     AND   ZFAMDNO EQ W_ZFAMDNO.
* 수입추천 내역 Amend용 Temp table
            DELETE FROM ZTREQIL_TMP WHERE ZFREQNO EQ ZFREQNO
                                    AND   ZFAMDNO EQ W_ZFAMDNO.
* 수입의뢰 HEADER TABLE
            DELETE FROM ZTREQHD_tmp WHERE ZFREQNO EQ ZFREQNO
                                    AND   ZFAMDNO EQ W_ZFAMDNO.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
*-----------------------------------------------------------------------

         ENDIF.

*-----------------------------------------------------------------------
* Update
*-----------------------------------------------------------------------
      WHEN OTHERS.            " 변?
         UPDATE     ZTREQHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         UPDATE     ZTREQST.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

   ENDCASE.

   IF ZFSTATUS NE 'X'.
* 수입의뢰 품?
      SELECT * FROM ZTREQIT WHERE ZFREQNO  EQ  ZFREQNO.
         READ TABLE IT_ZSREQIT WITH KEY ZFITMNO = ZTREQIT-ZFITMNO
                                                   BINARY SEARCH.
         IF SY-SUBRC EQ 0.
            IF IT_ZSREQIT-MENGE > 0.
               MOVE-CORRESPONDING IT_ZSREQIT TO ZTREQIT.
               MOVE : SY-MANDT     TO   ZTREQIT-MANDT,
                      ZFREQNO      TO   ZTREQIT-ZFREQNO.
*                      IT_ZSREQIT-ZFITMNO TO ZTREQIT-EBELP.

               UPDATE ZTREQIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* CHANGE DOCUMENT
               READ TABLE IT_ZSREQIT_OLD
                    WITH KEY ZFITMNO = ZTREQIT-ZFITMNO.
               IF SY-SUBRC NE 0.
                  CLEAR : IT_ZSREQIT_OLD.
               ENDIF.
               PERFORM   P3000_ITEM_CHANGE_DOCUMENT
                         USING    ZTREQIT
                                  IT_ZSREQIT_OLD
                                  'U'.

            ELSE.
               DELETE ZTREQIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* CHANGE DOCUMENT
               READ TABLE IT_ZSREQIT_OLD
                    WITH KEY ZFITMNO = ZTREQIT-ZFITMNO.
               IF SY-SUBRC NE 0.
                  CLEAR : IT_ZSREQIT_OLD.
               ENDIF.
               PERFORM   P3000_ITEM_CHANGE_DOCUMENT
                         USING    ZTREQIT
                                  IT_ZSREQIT_OLD
                                  'D'.

            ENDIF.
         ELSE.
            DELETE ZTREQIT.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* CHANGE DOCUMENT
            READ TABLE IT_ZSREQIT_OLD
                 WITH KEY ZFITMNO = ZTREQIT-ZFITMNO.
            IF SY-SUBRC NE 0.
               CLEAR : IT_ZSREQIT_OLD.
            ENDIF.
            PERFORM   P3000_ITEM_CHANGE_DOCUMENT
                      USING    ZTREQIT
                               IT_ZSREQIT_OLD
                               'D'.

         ENDIF.

      ENDSELECT.

      LOOP AT IT_ZSREQIT WHERE MENGE > 0.
         SELECT SINGLE * FROM  ZTREQIT
                         WHERE ZFREQNO  EQ  ZFREQNO
                         AND   ZFITMNO  EQ  IT_ZSREQIT-ZFITMNO.
         IF SY-SUBRC NE 0.
            MOVE-CORRESPONDING IT_ZSREQIT TO ZTREQIT.
            MOVE : ZFREQNO                TO ZTREQIT-ZFREQNO,
                   SY-MANDT               TO ZTREQIT-MANDT.
*                   IT_ZSREQIT-ZFITMNO     TO ZTREQIT-EBELP.

            INSERT  ZTREQIT.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* CHANGE DOCUMENT
            READ TABLE IT_ZSREQIT_OLD
                 WITH KEY ZFITMNO = ZTREQIT-ZFITMNO.
            IF SY-SUBRC NE 0.
               CLEAR : IT_ZSREQIT_OLD.
            ENDIF.
            PERFORM   P3000_ITEM_CHANGE_DOCUMENT
                      USING    ZTREQIT
                               IT_ZSREQIT_OLD
                               'I'.

         ENDIF.
      ENDLOOP.
*  수입의뢰 원산지 원산?
      SELECT * FROM ZTREQORJ  WHERE ZFREQNO  EQ  ZFREQNO.

         READ TABLE IT_ZTREQORJ
              WITH KEY ZFLSG7O = ZTREQORJ-ZFLSG7O BINARY SEARCH.

         IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING IT_ZTREQORJ  TO ZTREQORJ.
            MOVE : SY-MANDT     TO   ZTREQORJ-MANDT,
                   ZFREQNO      TO   ZTREQORJ-ZFREQNO.
            UPDATE ZTREQORJ.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ELSE.
            DELETE ZTREQORJ.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDIF.
      ENDSELECT.

      LOOP AT IT_ZTREQORJ.
         SELECT SINGLE * FROM  ZTREQORJ
                         WHERE ZFREQNO  EQ  ZFREQNO
                         AND   ZFLSG7O  EQ  IT_ZTREQORJ-ZFLSG7O.

         IF SY-SUBRC NE 0.
            MOVE-CORRESPONDING IT_ZTREQORJ  TO ZTREQORJ.
            MOVE : ZFREQNO                  TO ZTREQORJ-ZFREQNO,
                   SY-MANDT                 TO ZTREQORJ-MANDT.

            INSERT  ZTREQORJ.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDIF.
      ENDLOOP.
* 수입추천 품?
      SELECT * FROM ZTREQIL WHERE ZFREQNO  EQ  ZFREQNO.
         READ TABLE IT_ZSREQIL WITH KEY ZFILSEQ = ZTREQIL-ZFILSEQ
                                                   BINARY SEARCH.
         IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING IT_ZSREQIL TO ZTREQIL.
            MOVE : SY-MANDT     TO   ZTREQIL-MANDT,
                   ZFREQNO      TO   ZTREQIL-ZFREQNO.
            UPDATE ZTREQIL.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ELSE.
            DELETE ZTREQIL.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDIF.
      ENDSELECT.

      LOOP AT IT_ZSREQIL.
         SELECT SINGLE * FROM  ZTREQIL
                         WHERE ZFREQNO  EQ  ZFREQNO
                         AND   ZFILSEQ  EQ  IT_ZSREQIL-ZFILSEQ.
         IF SY-SUBRC NE 0.
            MOVE-CORRESPONDING IT_ZSREQIL TO ZTREQIL.
            MOVE : ZFREQNO                TO ZTREQIL-ZFREQNO,
                   SY-MANDT               TO ZTREQIL-MANDT.

            INSERT  ZTREQIL.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDIF.
      ENDLOOP.
   ENDIF.

ENDFUNCTION.
