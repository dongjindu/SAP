FUNCTION ZIM_CARGO_WORK_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFCGNO) LIKE  ZTCGHD-ZFCGNO
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTCGHD_OLD) LIKE  ZTCGHD STRUCTURE  ZTCGHD
*"     VALUE(W_ZTCGHD) LIKE  ZTCGHD STRUCTURE  ZTCGHD
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSCGIT_OLD STRUCTURE  ZSCGIT OPTIONAL
*"      IT_ZSCGIT STRUCTURE  ZSCGIT
*"      IT_ZSCGCST_OLD STRUCTURE  ZSCGCST OPTIONAL
*"      IT_ZSCGCST STRUCTURE  ZSCGCST
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
DATA : W_ZFCSQ       LIKE   ZTCGCST-ZFCSQ,
       W_LINE        LIKE   ZTCGIT-ZFCGIT.

   MOVE-CORRESPONDING : W_ZTCGHD      TO   ZTCGHD.

   MOVE : ZFCGNO       TO     ZTCGHD-ZFCGNO,
          SY-MANDT     TO     ZTCGHD-MANDT,
          SY-UNAME     TO     ZTCGHD-UNAM,
          SY-DATUM     TO     ZTCGHD-UDAT.

   IF W_OK_CODE EQ 'DELE'.
      ZFSTATUS = 'X'.
   ENDIF.

   CASE ZFSTATUS.
      WHEN 'C'.               " 생성
         MOVE : SY-UNAME      TO    ZTCGHD-ERNAM,
                SY-DATUM      TO    ZTCGHD-CDAT.

         INSERT   ZTCGHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

* change document -----------------------------------------------------
         CLEAR : W_ZTCGHD_OLD.
         call function 'ZIM_CHANGE_DOCUMENT_CGHD'
              exporting
                      upd_chngind    =    'I'
                      N_ZTCGHD       =    W_ZTCGHD
                      O_ZTCGHD       =    W_ZTCGHD_OLD.
*----------------------------------------------------------------------
*>> 하역 자재..
         W_LINE = 0.
         LOOP AT IT_ZSCGIT WHERE CGMENGE GT 0.
            ADD   10    TO    W_LINE.
            CLEAR : ZTCGIT.
            MOVE-CORRESPONDING IT_ZSCGIT TO ZTCGIT.
            MOVE : ZFCGNO                 TO ZTCGIT-ZFCGNO,
                   W_LINE                 TO ZTCGIT-ZFCGIT,
                   SY-MANDT               TO ZTCGIT-MANDT,
                   SY-UNAME               TO ZTCGIT-ERNAM,
                   SY-DATUM               TO ZTCGIT-CDAT,
                   SY-UNAME               TO ZTCGIT-UNAM,
                   SY-DATUM               TO ZTCGIT-UDAT.

            INSERT   ZTCGIT.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            CLEAR *ZTCGIT.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CGIT'
                    exporting
                           upd_chngind    =    'I'
                           N_ZTCGIT       =    ZTCGIT
                           O_ZTCGIT       =    *ZTCGIT.
*----------------------------------------------------------------------
         ENDLOOP.
*>> 하역 비용.
         LOOP AT IT_ZSCGCST.
            CLEAR : ZTCGCST.
            IF IT_ZSCGCST-BUKRS IS INITIAL.
               MOVE  ZTCGHD-BUKRS TO IT_ZSCGCST-BUKRS.
            ENDIF.
            IF IT_ZSCGCST-ZFCSQ  IS INITIAL.
               SELECT MAX( ZFCSQ ) INTO IT_ZSCGCST-ZFCSQ
                      FROM ZTCGCST
                      WHERE ZFCGNO  EQ  ZFCGNO.
               IF IT_ZSCGCST-ZFCSQ IS INITIAL.
                  IT_ZSCGCST-ZFCSQ = '00000'.
               ENDIF.
               ADD 10   TO   IT_ZSCGCST-ZFCSQ.
            ENDIF.
            MOVE-CORRESPONDING IT_ZSCGCST TO ZTCGCST.
            MOVE : ZFCGNO                 TO ZTCGCST-ZFCGNO,
                   ZTCGCST-ZFCKAMT        TO ZTCGCST-ZFCAMT,
                   ZTCGCST-ZFKRW          TO ZTCGCST-WAERS,
                   SY-MANDT               TO ZTCGCST-MANDT,
                   SY-UNAME               TO ZTCGCST-ERNAM,
                   SY-DATUM               TO ZTCGCST-CDAT,
                   SY-UNAME               TO ZTCGCST-UNAM,
                   SY-DATUM               TO ZTCGCST-UDAT.

            INSERT   ZTCGCST.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            CLEAR *ZTCGCST.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CGCST'
                    exporting
                           upd_chngind    =    'I'
                           N_ZTCGCST      =    ZTCGCST
                           O_ZTCGCST      =    *ZTCGCST.
*----------------------------------------------------------------------
         ENDLOOP.

      WHEN 'X'.               " 삭제
         DELETE  FROM ZTCGHD     WHERE ZFCGNO  EQ ZFCGNO.
         IF SY-SUBRC NE 0.     RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         call function 'ZIM_CHANGE_DOCUMENT_CGHD'
              exporting
                      upd_chngind    =    'D'
                      N_ZTCGHD       =    W_ZTCGHD
                      O_ZTCGHD       =    W_ZTCGHD_OLD.
*----------------------------------------------------------------------

         DELETE  FROM ZTCGIT  WHERE ZFCGNO EQ ZFCGNO.
         DELETE  FROM ZTCGCST WHERE ZFCGNO EQ ZFCGNO.
         LOOP  AT  IT_ZSCGIT.
            MOVE-CORRESPONDING IT_ZSCGIT  TO ZTCGIT.
            CLEAR *ZTCGIT.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CGIT'
                    exporting
                           upd_chngind    =    'D'
                           N_ZTCGIT       =    ZTCGIT
                           O_ZTCGIT       =    *ZTCGIT.
*----------------------------------------------------------------------
         ENDLOOP.
         LOOP  AT  IT_ZSCGCST.
            MOVE-CORRESPONDING IT_ZSCGCST  TO ZTCGCST.
            CLEAR *ZTCGCST.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CGCST'
                    exporting
                           upd_chngind    =    'D'
                           N_ZTCGCST      =    ZTCGCST
                           O_ZTCGCST      =    *ZTCGCST.
*----------------------------------------------------------------------
         ENDLOOP.

      WHEN OTHERS.            " 변경
         UPDATE   ZTCGHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         call function 'ZIM_CHANGE_DOCUMENT_CGHD'
              exporting
                      upd_chngind    =    'U'
                      N_ZTCGHD       =    W_ZTCGHD
                      O_ZTCGHD       =    W_ZTCGHD_OLD.
*----------------------------------------------------------------------
* 하역 자재.
         SELECT * FROM ZTCGIT WHERE ZFCGNO   EQ  ZFCGNO.
            CLEAR : IT_ZSCGIT.
            READ TABLE IT_ZSCGIT WITH KEY ZFCGIT  = ZTCGIT-ZFCGIT
                                  BINARY SEARCH.
*            W_SUBRC  = SY-SUBRC.
*            IF IT_ZSCGIT-CGMENGE LE 0.
*               W_SUBRC = 4.
*            ENDIF.
            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSCGIT  TO ZTCGIT.
               MOVE : SY-MANDT               TO ZTCGIT-MANDT,
                      ZFCGNO                 TO ZTCGIT-ZFCGNO,
                      SY-UNAME               TO ZTCGIT-UNAM,
                      SY-DATUM               TO ZTCGIT-UDAT.
               UPDATE ZTCGIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               CLEAR *ZTCGIT.
               READ TABLE IT_ZSCGIT_OLD WITH KEY
                          ZFCGNO = IT_ZSCGIT-ZFCGNO
                          ZFCGIT = IT_ZSCGIT-ZFCGIT.
               IF SY-SUBRC EQ 0.
                  MOVE-CORRESPONDING IT_ZSCGIT_OLD TO *ZTCGIT.
               ENDIF.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CGIT'
                    exporting
                            upd_chngind    =    'U'
                            N_ZTCGIT       =    ZTCGIT
                            O_ZTCGIT       =    *ZTCGIT.
*----------------------------------------------------------------------
            ELSE.
               DELETE ZTCGIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CGIT'
                    exporting
                            upd_chngind    =    'D'
                            N_ZTCGIT       =    ZTCGIT
                            O_ZTCGIT       =    *ZTCGIT.
*----------------------------------------------------------------------
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSCGIT WHERE CGMENGE NE 0.
            SELECT SINGLE * FROM  ZTCGIT
                            WHERE ZFCGNO   EQ  ZFCGNO
                            AND   ZFCGIT   EQ  IT_ZSCGIT-ZFCGIT.
*                            AND   CGMENGE  GT  0.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSCGIT  TO ZTCGIT.

               SELECT MAX( ZFCGIT ) INTO ZTCGIT-ZFCGIT
                      FROM ZTCGIT
                      WHERE ZFCGNO EQ ZFCGNO.

               ADD    10    TO   ZTCGIT-ZFCGIT.

               MOVE : ZFCGNO                 TO ZTCGIT-ZFCGNO,
                      SY-MANDT               TO ZTCGIT-MANDT,
                      SY-UNAME               TO ZTCGIT-ERNAM,
                      SY-DATUM               TO ZTCGIT-CDAT,
                      SY-UNAME               TO ZTCGIT-UNAM,
                      SY-DATUM               TO ZTCGIT-UDAT.

               INSERT  ZTCGIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               CLEAR *ZTCGIT.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CGIT'
                    exporting
                            upd_chngind    =    'I'
                            N_ZTCGIT       =    ZTCGIT
                            O_ZTCGIT       =    *ZTCGIT.
*----------------------------------------------------------------------
            ENDIF.
         ENDLOOP.

* 하역 비용.
         SELECT * FROM ZTCGCST WHERE ZFCGNO   EQ  ZFCGNO.

            READ TABLE IT_ZSCGCST WITH KEY ZFCSQ  = ZTCGCST-ZFCSQ
                                  BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSCGCST TO ZTCGCST.
               MOVE : SY-MANDT               TO ZTCGCST-MANDT,
                      ZFCGNO                 TO ZTCGCST-ZFCGNO,
                      ZTCGCST-ZFCKAMT        TO ZTCGCST-ZFCAMT,
                      ZTCGCST-ZFKRW          TO ZTCGCST-WAERS,
                      SY-UNAME               TO ZTCGCST-UNAM,
                      SY-DATUM               TO ZTCGCST-UDAT.
               UPDATE ZTCGCST.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               CLEAR *ZTCGCST.
               READ TABLE IT_ZSCGCST_OLD WITH KEY
                          ZFCGNO  =  IT_ZSCGCST-ZFCGNO
                          ZFCSQ   =  IT_ZSCGCST-ZFCSQ.
               IF SY-SUBRC EQ 0.
                  MOVE-CORRESPONDING IT_ZSCGCST_OLD TO *ZTCGCST.
               ENDIF.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CGCST'
                    exporting
                            upd_chngind    =    'U'
                            N_ZTCGCST      =    ZTCGCST
                            O_ZTCGCST      =    *ZTCGCST.
*----------------------------------------------------------------------

            ELSE.
               DELETE ZTCGCST.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CGCST'
                    exporting
                            upd_chngind    =    'D'
                            N_ZTCGCST       =    ZTCGCST
                            O_ZTCGCST       =    *ZTCGCST.
*----------------------------------------------------------------------
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSCGCST.
            SELECT SINGLE * FROM  ZTCGCST
                            WHERE ZFCGNO   EQ  ZFCGNO
                            AND   ZFCSQ    EQ  IT_ZSCGCST-ZFCSQ.

            IF SY-SUBRC NE 0.


               IF IT_ZSCGCST-BUKRS IS INITIAL.
                  MOVE  ZTCGHD-BUKRS TO IT_ZSCGCST-BUKRS.
               ENDIF.
               IF IT_ZSCGCST-ZFCSQ  IS INITIAL.
                  SELECT MAX( ZFCSQ ) INTO IT_ZSCGCST-ZFCSQ
                         FROM ZTCGCST
                         WHERE ZFCGNO  EQ  ZFCGNO.
                  IF IT_ZSCGCST-ZFCSQ IS INITIAL.
                     IT_ZSCGCST-ZFCSQ = '00000'.
                  ENDIF.
                  ADD 10   TO   IT_ZSCGCST-ZFCSQ.
               ENDIF.

               MOVE-CORRESPONDING IT_ZSCGCST  TO ZTCGCST.
               MOVE : ZFCGNO                 TO ZTCGCST-ZFCGNO,
                      ZTCGCST-ZFCKAMT        TO ZTCGCST-ZFCAMT,
                      ZTCGCST-ZFKRW          TO ZTCGCST-WAERS,
                      SY-MANDT               TO ZTCGCST-MANDT,
                      SY-UNAME               TO ZTCGCST-ERNAM,
                      SY-DATUM               TO ZTCGCST-CDAT,
                      SY-UNAME               TO ZTCGCST-UNAM,
                      SY-DATUM               TO ZTCGCST-UDAT.

               INSERT  ZTCGCST.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               CLEAR  *ZTCGCST.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CGCST'
                    exporting
                            upd_chngind    =    'I'
                            N_ZTCGCST      =    ZTCGCST
                            O_ZTCGCST      =    *ZTCGCST.
*----------------------------------------------------------------------
            ENDIF.
         ENDLOOP.
   ENDCASE.
ENDFUNCTION.
