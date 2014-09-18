FUNCTION ZIM_BL_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFBLNO) LIKE  ZTBL-ZFBLNO
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTBL_OLD) LIKE  ZTBL STRUCTURE  ZTBL
*"     VALUE(W_ZTBL) LIKE  ZTBL STRUCTURE  ZTBL
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSBLCST_OLD STRUCTURE  ZSBLCST OPTIONAL
*"      IT_ZSBLCST STRUCTURE  ZSBLCST
*"      IT_ZSBLCON_OLD STRUCTURE  ZSBLCON OPTIONAL
*"      IT_ZSBLCON STRUCTURE  ZSBLCON
*"      IT_ZSBLCST1_OLD STRUCTURE  ZSBLCST OPTIONAL
*"      IT_ZSBLCST1 STRUCTURE  ZSBLCST
*"      IT_ZSBLIT STRUCTURE  ZSBLIT OPTIONAL
*"      IT_ZSBLIT_OLD STRUCTURE  ZSBLIT OPTIONAL
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
  DATA : W_ZFCONSEQ    LIKE   ZTBLCON-ZFCONSEQ,
         W_ZFCSQ       LIKE   ZTBLCST-ZFCSQ,
         W_LINE        LIKE   ZTBLIT-ZFBLIT,
         W_WEBKEY(10).

  MOVE-CORRESPONDING : W_ZTBL      TO   ZTBL.

  MOVE : ZFBLNO      TO     ZTBL-ZFBLNO,
         SY-MANDT    TO     ZTBL-MANDT,
         SY-UNAME    TO     ZTBL-UNAM,
         SY-DATUM    TO     ZTBL-UDAT.

  IF W_OK_CODE EQ 'DELE'.
    ZFSTATUS = 'X'.
  ENDIF.

  CASE ZFSTATUS.
    WHEN 'C'.               " Create
      MOVE : SY-UNAME      TO    ZTBL-ERNAM,
             SY-DATUM      TO    ZTBL-CDAT.

      INSERT   ZTBL.
      IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
       SET PARAMETER ID 'ZPBLNO'  FIELD  ZTBL-ZFBLNO.
       SET PARAMETER ID 'ZPHBLNO' FIELD  ZTBL-ZFHBLNO.

* change document -----------------------------------------------------
      CLEAR : W_ZTBL_OLD.
      CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BL'
        EXPORTING
          UPD_CHNGIND = 'I'
          N_ZTBL      = W_ZTBL
          O_ZTBL      = W_ZTBL_OLD.
*----------------------------------------------------------------------

      LOOP AT IT_ZSBLCST.
        CLEAR : ZTBLCST.
        IF IT_ZSBLCST-BUKRS IS INITIAL.
          MOVE  ZTBL-BUKRS TO IT_ZSBLCST-BUKRS.
        ENDIF.
        IF IT_ZSBLCST-ZFCSQ  IS INITIAL.
          SELECT MAX( ZFCSQ ) INTO IT_ZSBLCST-ZFCSQ
                 FROM ZTBLCST
                 WHERE ZFBLNO  EQ  ZFBLNO
                 AND   ZFCSQ   GT  '10000'.
          IF IT_ZSBLCST-ZFCSQ IS INITIAL.
            IT_ZSBLCST-ZFCSQ = '10000'.
          ENDIF.
          ADD 10   TO   IT_ZSBLCST-ZFCSQ.
        ENDIF.
        MOVE-CORRESPONDING IT_ZSBLCST   TO ZTBLCST.
        MOVE : ZFBLNO                 TO ZTBLCST-ZFBLNO,
               SY-MANDT               TO ZTBLCST-MANDT,
               SY-UNAME               TO ZTBLCST-ERNAM,
               SY-DATUM               TO ZTBLCST-CDAT,
               SY-UNAME               TO ZTBLCST-UNAM,
               SY-DATUM               TO ZTBLCST-UDAT.
        INSERT   ZTBLCST.
        IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
        CLEAR  *ZTBLCST.
* change document -----------------------------------------------------
        CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLCST'
          EXPORTING
            UPD_CHNGIND = 'I'
            N_ZTBLCST   = ZTBLCST
            O_ZTBLCST   = *ZTBLCST.
*----------------------------------------------------------------------

      ENDLOOP.

      LOOP AT IT_ZSBLCST1.
        CLEAR : ZTBLCST.
        IF IT_ZSBLCST1-BUKRS IS INITIAL.
          MOVE  ZTBL-BUKRS  TO  IT_ZSBLCST1-BUKRS.
        ENDIF.
        IF IT_ZSBLCST1-ZFCSQ  IS INITIAL OR
           IT_ZSBLCST1-ZFCSQ  GE '10000'.
          SELECT MAX( ZFCSQ ) INTO IT_ZSBLCST1-ZFCSQ
                 FROM ZTBLCST
                 WHERE ZFBLNO  EQ  ZFBLNO
                 AND   ZFCSQ   LT  '10000'.
          IF IT_ZSBLCST1-ZFCSQ IS INITIAL.
            IT_ZSBLCST1-ZFCSQ = '00000'.
          ENDIF.
          ADD 10   TO   IT_ZSBLCST1-ZFCSQ.
        ENDIF.

        MOVE-CORRESPONDING IT_ZSBLCST1  TO ZTBLCST.
        MOVE : ZFBLNO                 TO ZTBLCST-ZFBLNO,
               SY-MANDT               TO ZTBLCST-MANDT,
               SY-UNAME               TO ZTBLCST-ERNAM,
               SY-DATUM               TO ZTBLCST-CDAT,
               SY-UNAME               TO ZTBLCST-UNAM,
               SY-DATUM               TO ZTBLCST-UDAT.

        INSERT   ZTBLCST.
        IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
        CLEAR  *ZTBLCST.
* change document -----------------------------------------------------
        CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLCST'
          EXPORTING
            UPD_CHNGIND = 'I'
            N_ZTBLCST   = ZTBLCST
            O_ZTBLCST   = *ZTBLCST.
*----------------------------------------------------------------------

      ENDLOOP.

      LOOP AT IT_ZSBLCON.
        CLEAR : ZTBLCON.
        MOVE-CORRESPONDING IT_ZSBLCON TO ZTBLCON.
        MOVE : ZFBLNO                 TO ZTBLCON-ZFBLNO,
               SY-MANDT               TO ZTBLCON-MANDT,
               SY-UNAME               TO ZTBLCON-ERNAM,
               SY-DATUM               TO ZTBLCON-CDAT,
               SY-UNAME               TO ZTBLCON-UNAM,
               SY-DATUM               TO ZTBLCON-UDAT.

        INSERT   ZTBLCON.
        IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDLOOP.

      W_LINE = 0.
      LOOP AT IT_ZSBLIT.
        ADD   10    TO    W_LINE.
        CLEAR : ZTBLIT.
        IF NOT IT_ZSBLIT-BLMENGE IS INITIAL.
          MOVE-CORRESPONDING IT_ZSBLIT  TO  ZTBLIT.

          MOVE : ZFBLNO                 TO  ZTBLIT-ZFBLNO,
                 W_LINE                 TO  ZTBLIT-ZFBLIT,
                 SY-MANDT               TO  ZTBLIT-MANDT,
                 SY-UNAME               TO  ZTBLIT-ERNAM,
                 SY-DATUM               TO  ZTBLIT-CDAT,
                 SY-UNAME               TO  ZTBLIT-UNAM,
                 SY-DATUM               TO  ZTBLIT-UDAT.
          INSERT   ZTBLIT.
          IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
          CLEAR  *ZTBLIT.
* change document -----------------------------------------------------
          CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLIT'
            EXPORTING
              UPD_CHNGIND = 'I'
              N_ZTBLIT    = ZTBLIT
              O_ZTBLIT    = *ZTBLIT.
*----------------------------------------------------------------------
        ENDIF.
      ENDLOOP.
    WHEN 'X'.               " Delete
      DELETE  FROM ZTBL     WHERE ZFBLNO  EQ ZFBLNO.
      IF SY-SUBRC NE 0.     RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
      CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BL'
        EXPORTING
          UPD_CHNGIND = 'D'
          N_ZTBL      = W_ZTBL
          O_ZTBL      = W_ZTBL_OLD.
*----------------------------------------------------------------------

      DELETE  FROM ZTBLCON  WHERE ZFBLNO  EQ ZFBLNO.
      DELETE  FROM ZTBLCST  WHERE ZFBLNO  EQ ZFBLNO.
      DELETE  FROM ZTBLIT   WHERE ZFBLNO  EQ ZFBLNO.
      LOOP  AT  IT_ZSBLIT.
        MOVE-CORRESPONDING IT_ZSBLIT  TO  ZTBLIT.
* change document -----------------------------------------------------
        CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLIT'
          EXPORTING
            UPD_CHNGIND = 'D'
            N_ZTBLIT    = ZTBLIT
            O_ZTBLIT    = *ZTBLIT.
*----------------------------------------------------------------------
      ENDLOOP.
      LOOP  AT  IT_ZSBLCST.
        MOVE-CORRESPONDING IT_ZSBLCST  TO  ZTBLCST.
* change document -----------------------------------------------------
        CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLCST'
          EXPORTING
            UPD_CHNGIND = 'D'
            N_ZTBLCST   = ZTBLCST
            O_ZTBLCST   = *ZTBLCST.
*----------------------------------------------------------------------
      ENDLOOP.
    WHEN OTHERS.            " Change
      UPDATE   ZTBL.
      IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
      CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BL'
        EXPORTING
          UPD_CHNGIND = 'U'
          N_ZTBL      = W_ZTBL
          O_ZTBL      = W_ZTBL_OLD.
*----------------------------------------------------------------------

* B/L Expense
      SELECT * FROM ZTBLCST WHERE ZFBLNO   EQ  ZFBLNO.

        IF ZTBLCST-ZFCSQ > '10000'.
          READ TABLE IT_ZSBLCST WITH KEY ZFCSQ  = ZTBLCST-ZFCSQ
                                BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING IT_ZSBLCST TO ZTBLCST.
            MOVE : SY-UNAME               TO ZTBLCST-UNAM,
                   SY-DATUM               TO ZTBLCST-UDAT,
                   SY-MANDT               TO ZTBLCST-MANDT,
                   ZFBLNO                 TO ZTBLCST-ZFBLNO.
            UPDATE ZTBLCST.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            READ TABLE IT_ZSBLCST_OLD  WITH KEY
                       ZFBLNO  =  IT_ZSBLCST-ZFBLNO
                       ZFCSQ   =  IT_ZSBLCST-ZFCSQ.
            IF SY-SUBRC EQ 0.
              MOVE-CORRESPONDING IT_ZSBLCST_OLD TO *ZTBLCST.
            ENDIF.
* change document -----------------------------------------------------
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLCST'
              EXPORTING
                UPD_CHNGIND = 'U'
                N_ZTBLCST   = ZTBLCST
                O_ZTBLCST   = *ZTBLCST.
*----------------------------------------------------------------------

          ELSE.
            DELETE ZTBLCST.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLCST'
              EXPORTING
                UPD_CHNGIND = 'D'
                N_ZTBLCST   = ZTBLCST
                O_ZTBLCST   = *ZTBLCST.
*----------------------------------------------------------------------

          ENDIF.
        ELSE.
          READ TABLE IT_ZSBLCST1 WITH KEY ZFCSQ  = ZTBLCST-ZFCSQ
                                 BINARY SEARCH.

          IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING IT_ZSBLCST1 TO ZTBLCST.
            MOVE : SY-UNAME               TO ZTBLCST-UNAM,
                   SY-DATUM               TO ZTBLCST-UDAT,
                   SY-MANDT               TO ZTBLCST-MANDT,
                   ZFBLNO                 TO ZTBLCST-ZFBLNO.
            UPDATE ZTBLCST.

            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            READ TABLE IT_ZSBLCST1_OLD  WITH KEY
                       ZFBLNO  =  IT_ZSBLCST1-ZFBLNO
                       ZFCSQ   =  IT_ZSBLCST1-ZFCSQ.
            IF SY-SUBRC EQ 0.
              MOVE-CORRESPONDING IT_ZSBLCST1_OLD TO *ZTBLCST.
            ENDIF.
* change document -----------------------------------------------------
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLCST'
              EXPORTING
                UPD_CHNGIND = 'U'
                N_ZTBLCST   = ZTBLCST
                O_ZTBLCST   = *ZTBLCST.
*----------------------------------------------------------------------
          ELSE.
            DELETE ZTBLCST.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLCST'
              EXPORTING
                UPD_CHNGIND = 'D'
                N_ZTBLCST   = ZTBLCST
                O_ZTBLCST   = *ZTBLCST.
*----------------------------------------------------------------------
          ENDIF.
        ENDIF.
      ENDSELECT.

      LOOP AT IT_ZSBLCST.
        SELECT SINGLE * FROM  ZTBLCST
                        WHERE ZFBLNO   EQ  ZFBLNO
                        AND   ZFCSQ    EQ  IT_ZSBLCST-ZFCSQ.
        IF SY-SUBRC NE 0.
          IF IT_ZSBLCST-BUKRS IS INITIAL.
            MOVE  ZTBL-BUKRS TO IT_ZSBLCST-BUKRS.
          ENDIF.
          IF IT_ZSBLCST-ZFCSQ  IS INITIAL.
            SELECT MAX( ZFCSQ ) INTO IT_ZSBLCST-ZFCSQ
                   FROM ZTBLCST
                   WHERE ZFBLNO  EQ  ZFBLNO
                   AND   ZFCSQ   GT  '10000'.
            IF IT_ZSBLCST-ZFCSQ IS INITIAL.
              IT_ZSBLCST-ZFCSQ = '10000'.
            ENDIF.
            ADD 10   TO   IT_ZSBLCST-ZFCSQ.
          ENDIF.

          MOVE-CORRESPONDING IT_ZSBLCST TO ZTBLCST.
          MOVE : ZFBLNO                 TO ZTBLCST-ZFBLNO,
                 SY-MANDT               TO ZTBLCST-MANDT,
                 SY-UNAME               TO ZTBLCST-ERNAM,
                 SY-DATUM               TO ZTBLCST-CDAT,
                 SY-UNAME               TO ZTBLCST-UNAM,
                 SY-DATUM               TO ZTBLCST-UDAT.

          INSERT  ZTBLCST.
          IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
          CLEAR  *ZTBLCST.
* change document -----------------------------------------------------
          CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLCST'
            EXPORTING
              UPD_CHNGIND = 'I'
              N_ZTBLCST   = ZTBLCST
              O_ZTBLCST   = *ZTBLCST.
*----------------------------------------------------------------------
        ENDIF.
      ENDLOOP.
      LOOP AT IT_ZSBLCST1.
        SELECT SINGLE * FROM  ZTBLCST
                        WHERE ZFBLNO   EQ  ZFBLNO
                        AND   ZFCSQ    EQ  IT_ZSBLCST1-ZFCSQ.
        IF SY-SUBRC NE 0.
          IF IT_ZSBLCST1-BUKRS IS INITIAL.
            MOVE  ZTBL-BUKRS TO IT_ZSBLCST1-BUKRS.
          ENDIF.
          IF IT_ZSBLCST1-ZFCSQ  IS INITIAL.
            SELECT MAX( ZFCSQ ) INTO IT_ZSBLCST1-ZFCSQ
                   FROM ZTBLCST
                   WHERE ZFBLNO  EQ  ZFBLNO
                   AND   ZFCSQ   LT  '10000'.
            IF IT_ZSBLCST1-ZFCSQ IS INITIAL.
              IT_ZSBLCST1-ZFCSQ = '00000'.
            ENDIF.
            ADD 10   TO   IT_ZSBLCST1-ZFCSQ.
          ENDIF.

          MOVE-CORRESPONDING IT_ZSBLCST1 TO ZTBLCST.
          MOVE : ZFBLNO                  TO ZTBLCST-ZFBLNO,
                 SY-MANDT                TO ZTBLCST-MANDT,
                 SY-UNAME                TO ZTBLCST-ERNAM,
                 SY-DATUM                TO ZTBLCST-CDAT,
                 SY-UNAME                TO ZTBLCST-UNAM,
                 SY-DATUM                TO ZTBLCST-UDAT.

          INSERT  ZTBLCST.
          IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
          CLEAR  *ZTBLCST.
* change document -----------------------------------------------------
          CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLCST'
            EXPORTING
              UPD_CHNGIND = 'I'
              N_ZTBLCST   = ZTBLCST
              O_ZTBLCST   = *ZTBLCST.
*----------------------------------------------------------------------
        ENDIF.
      ENDLOOP.

* B/L Container
      SELECT * FROM ZTBLCON WHERE ZFBLNO   EQ  ZFBLNO.

        READ TABLE IT_ZSBLCON WITH KEY ZFCONSEQ = ZTBLCON-ZFCONSEQ
                              BINARY SEARCH.

        IF SY-SUBRC EQ 0.
          MOVE-CORRESPONDING IT_ZSBLCON TO ZTBLCON.
          MOVE : SY-UNAME               TO ZTBLCON-UNAM,
                 SY-DATUM               TO ZTBLCON-UDAT.
          UPDATE ZTBLCON.
          IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
        ELSE.
          DELETE ZTBLCON.
          IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
        ENDIF.
      ENDSELECT.

      LOOP AT IT_ZSBLCON.
        SELECT SINGLE * FROM  ZTBLCON
                        WHERE ZFBLNO   EQ  ZFBLNO
                        AND   ZFCONSEQ EQ  IT_ZSBLCON-ZFCONSEQ.

        IF SY-SUBRC NE 0.
          MOVE-CORRESPONDING IT_ZSBLCON TO ZTBLCON.
          MOVE : ZFBLNO                 TO ZTBLCON-ZFBLNO,
                 SY-MANDT               TO ZTBLCON-MANDT,
                 SY-UNAME               TO ZTBLCON-ERNAM,
                 SY-DATUM               TO ZTBLCON-CDAT,
                 SY-UNAME               TO ZTBLCON-UNAM,
                 SY-DATUM               TO ZTBLCON-UDAT.

          INSERT  ZTBLCON.
          IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
        ENDIF.
      ENDLOOP.

* B/L Item
      SELECT * FROM ZTBLIT  WHERE ZFBLNO   EQ  ZFBLNO.

        READ TABLE IT_ZSBLIT  WITH KEY ZFBLIT   = ZTBLIT-ZFBLIT
                              BINARY SEARCH.

        IF SY-SUBRC EQ 0.
          IF IT_ZSBLIT-BLMENGE  IS INITIAL.
            DELETE ZTBLIT.
          ELSE.
            MOVE-CORRESPONDING IT_ZSBLIT  TO ZTBLIT.
            MOVE : SY-UNAME               TO ZTBLIT-UNAM,
                   SY-DATUM               TO ZTBLIT-UDAT.
            UPDATE ZTBLIT.
          ENDIF.
          IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
          CLEAR  *ZTBLIT.
          READ TABLE IT_ZSBLIT_OLD WITH KEY
                     ZFBLNO   =  IT_ZSBLIT-ZFBLNO
                     ZFBLIT   =  IT_ZSBLIT-ZFBLIT.
          IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING IT_ZSBLIT_OLD TO *ZTBLIT.
          ENDIF.
* change document -----------------------------------------------------
          CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLIT'
            EXPORTING
              UPD_CHNGIND = 'U'
              N_ZTBLIT    = ZTBLIT
              O_ZTBLIT    = *ZTBLIT.
*----------------------------------------------------------------------
        ELSE.
          DELETE ZTBLIT.
          IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
          CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLIT'
            EXPORTING
              UPD_CHNGIND = 'D'
              N_ZTBLIT    = ZTBLIT
              O_ZTBLIT    = *ZTBLIT.
*----------------------------------------------------------------------
        ENDIF.
      ENDSELECT.

*         LOOP AT IT_ZSBLIT WHERE BLMENGE NE 0.
      LOOP AT IT_ZSBLIT.
        SELECT SINGLE * FROM  ZTBLIT
                        WHERE ZFBLNO   EQ  ZFBLNO
                        AND   ZFBLIT   EQ  IT_ZSBLIT-ZFBLIT.

        IF SY-SUBRC NE 0 AND IT_ZSBLIT-BLMENGE GT 0.
          MOVE-CORRESPONDING IT_ZSBLIT  TO ZTBLIT.

          SELECT MAX( ZFBLIT ) INTO ZTBLIT-ZFBLIT
                 FROM ZTBLIT
                 WHERE ZFBLNO EQ ZFBLNO.

          ADD    10    TO   ZTBLIT-ZFBLIT.

          MOVE : ZFBLNO                 TO ZTBLIT-ZFBLNO,
                 SY-MANDT               TO ZTBLIT-MANDT,
                 SY-UNAME               TO ZTBLIT-ERNAM,
                 SY-DATUM               TO ZTBLIT-CDAT,
                 SY-UNAME               TO ZTBLIT-UNAM,
                 SY-DATUM               TO ZTBLIT-UDAT.

          INSERT  ZTBLIT.
          IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
          CLEAR *ZTBLIT.
* change document -----------------------------------------------------
          CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BLIT'
            EXPORTING
              UPD_CHNGIND = 'I'
              N_ZTBLIT    = ZTBLIT
              O_ZTBLIT    = *ZTBLIT.
*----------------------------------------------------------------------
        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDFUNCTION.
