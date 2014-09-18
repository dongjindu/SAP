FUNCTION ZIM_CHARGE_DOCUMENT_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(BUKRS) LIKE  ZTBKPF-BUKRS
*"     VALUE(GJAHR) LIKE  ZTBKPF-GJAHR
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTBKPF_OLD) LIKE  ZTBKPF STRUCTURE  ZTBKPF
*"     VALUE(W_ZTBKPF) LIKE  ZTBKPF STRUCTURE  ZTBKPF
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSBSEG_OLD STRUCTURE  ZSBSEG OPTIONAL
*"      IT_ZSBSEG STRUCTURE  ZSBSEG OPTIONAL
*"      IT_ZSBDIV STRUCTURE  ZSBDIV OPTIONAL
*"      IT_ZSBHIS STRUCTURE  ZSBHIS OPTIONAL
*"  CHANGING
*"     VALUE(BELNR) LIKE  ZTBKPF-BELNR
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
*-----------------------------------------------------------------------
*Date      Developer       Request         Description
*06/02/06  Manju           UD1K920953      Program bug fix to avoid
*                                          short dump in production
*                                          (Reference:662B146698 )
*"----------------------------------------------------------------------
DATA : W_ZFBSEQ      LIKE   ZTBDIV-ZFBSEQ,
       W_BUZEI       LIKE   ZTBDIV-BUZEI,
       W_ZUONR       LIKE   ZTBDIV-ZUONR,
       W_TOT_DMBTR   LIKE   ZTBKPF-DMBTR,
       W_DMBTR       LIKE   ZTBKPF-DMBTR,
       W_TOT_WRBTR   LIKE   ZTBKPF-WRBTR,
       W_BAL_WRBTR   LIKE   ZTBKPF-WRBTR,
       W_BAL_DMBTR   LIKE   ZTBKPF-DMBTR,
       W_MINUS_WRBTR LIKE   ZTBKPF-WRBTR,
       W_MINUS_DMBTR LIKE   ZTBKPF-DMBTR,
       W_WRBTR       LIKE   ZTBKPF-WRBTR,
       W_BSEG_LINE   LIKE   SY-TABIX,
       W_LINE_INDEX  LIKE   SY-TABIX,
       W_DIV_INDEX   LIKE   SY-TABIX,
       W_BAL_INDEX   LIKE   SY-TABIX,
       W_CNT_LINE    TYPE   I,
       W_CNT_MODE    TYPE   I,
       W_TOTAL_LINE  TYPE   I.

RANGES : R_ZFDOCNO   FOR    ZTBSEG-ZFDOCNO    OCCURS  0.

*-----------------------------------------------------------------------
*> Working Group Delete -> Deletion Bit Set.
*-----------------------------------------------------------------------
   IF W_OK_CODE EQ 'DELE'.
      ZFSTATUS = 'X'.
   ENDIF.

   MOVE-CORRESPONDING : W_ZTBKPF      TO   ZTBKPF.
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
*> Charge Header Data Move.
*-----------------------------------------------------------------------
   ">> Working Group is create -> Number Get.
   IF ZFSTATUS  EQ 'C'.
      PERFORM   P2000_GET_NUMBER_NEXT  USING  'CD'  BELNR GJAHR BUKRS.
   ENDIF.

   MOVE : SY-MANDT     TO     ZTBKPF-MANDT,
          BUKRS        TO     ZTBKPF-BUKRS,
          BELNR        TO     ZTBKPF-BELNR,
          GJAHR        TO     ZTBKPF-GJAHR,
          SY-UNAME     TO     ZTBKPF-UNAM,
          SY-UZEIT     TO     ZTBKPF-UTME,
          SY-DATUM     TO     ZTBKPF-UDAT.

   LOOP AT IT_ZSBSEG.
      W_TABIX = SY-TABIX.
      IT_ZSBSEG-BUZEI = W_TABIX.
      MODIFY IT_ZSBSEG INDEX W_TABIX.
   ENDLOOP.
*-----------------------------------------------------------------------

   CASE ZFSTATUS.
      WHEN 'C'.
         MOVE : SY-UNAME      TO    ZTBKPF-USNAM,
                SY-DATUM      TO    ZTBKPF-CPUDT,
                SY-UZEIT      TO    ZTBKPF-CPUTM.

         INSERT   ZTBKPF.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

* change document -----------------------------------------------------
         CLEAR : W_ZTBKPF_OLD.
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTBKPF'
              EXPORTING
                      UPD_CHNGIND    =    'I'
                      N_ZTBKPF       =    W_ZTBKPF
                      O_ZTBKPF       =    W_ZTBKPF_OLD.
*----------------------------------------------------------------------
         ">> Cost Item
         W_BUZEI = 0.
         LOOP AT IT_ZSBSEG.
            W_TABIX = SY-TABIX.
            ADD   1    TO    W_BUZEI.
            CLEAR : ZTBSEG.
            MOVE-CORRESPONDING IT_ZSBSEG TO ZTBSEG.
            MOVE: SY-MANDT     TO     ZTBSEG-MANDT,
                  BUKRS        TO     ZTBSEG-BUKRS,
                  BELNR        TO     ZTBSEG-BELNR,
                  GJAHR        TO     ZTBSEG-GJAHR,
                  W_BUZEI      TO     ZTBSEG-BUZEI,
                  SY-UNAME     TO     ZTBSEG-UNAM,
                  SY-UZEIT     TO     ZTBSEG-UTME,
                  SY-DATUM     TO     ZTBSEG-UDAT,
                  SY-UNAME     TO     ZTBSEG-USNAM,
                  SY-DATUM     TO     ZTBSEG-CPUDT,
                  SY-UZEIT     TO     ZTBSEG-CPUTM.

            INSERT   ZTBSEG.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            CLEAR *ZTBSEG.
* change document -----------------------------------------------------
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTBSEG'
                 EXPORTING
                        UPD_CHNGIND    =    'I'
                        N_ZTBSEG       =    ZTBSEG
                        O_ZTBSEG       =   *ZTBSEG.
*----------------------------------------------------------------------
            MOVE-CORRESPONDING ZTBSEG TO IT_ZSBSEG.
            MODIFY IT_ZSBSEG  INDEX   W_TABIX.
         ENDLOOP.

      WHEN 'X'.
         DELETE  FROM ZTBKPF
                 WHERE BUKRS  EQ  BUKRS
                 AND   BELNR  EQ  BELNR
                 AND   GJAHR  EQ  GJAHR.

         IF SY-SUBRC NE 0.     RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTBKPF'
              EXPORTING
                      UPD_CHNGIND    =    'D'
                      N_ZTBKPF       =    W_ZTBKPF
                      O_ZTBKPF       =    W_ZTBKPF_OLD.
*----------------------------------------------------------------------

         DELETE  FROM ZTBSEG
                 WHERE BUKRS  EQ  BUKRS
                 AND   BELNR  EQ  BELNR
                 AND   GJAHR  EQ  GJAHR.

         IF SY-SUBRC NE 0.     RAISE ERROR_UPDATE.   ENDIF.

         DELETE  FROM ZTBSEG1
                 WHERE BUKRS  EQ  BUKRS
                 AND   BELNR  EQ  BELNR
                 AND   GJAHR  EQ  GJAHR.

         DELETE  FROM ZTBDIV
                 WHERE BUKRS  EQ  BUKRS
                 AND   BELNR  EQ  BELNR
                 AND   GJAHR  EQ  GJAHR.

         DELETE  FROM ZTBHIS
                 WHERE BUKRS  EQ  BUKRS
                 AND   BELNR  EQ  BELNR
                 AND   GJAHR  EQ  GJAHR.

         REFRESH : R_ZFDOCNO.
         LOOP  AT  IT_ZSBSEG_OLD.
            MOVE-CORRESPONDING IT_ZSBSEG_OLD  TO ZTBSEG.
            CLEAR *ZTBSEG.
* change document -----------------------------------------------------
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTBSEG'
                    EXPORTING
                           UPD_CHNGIND    =    'D'
                           N_ZTBSEG       =    ZTBSEG
                           O_ZTBSEG       =   *ZTBSEG.
            IF NOT IT_ZSBSEG_OLD-ZFDOCNO IS INITIAL.
               CLEAR : R_ZFDOCNO.
               MOVE : 'I'                   TO R_ZFDOCNO-SIGN,
                      'EQ'                  TO R_ZFDOCNO-OPTION,
                      IT_ZSBSEG_OLD-ZFDOCNO TO R_ZFDOCNO-LOW,
                      SPACE                 TO R_ZFDOCNO-HIGH.
               APPEND  R_ZFDOCNO.
            ENDIF.
*----------------------------------------------------------------------
         ENDLOOP.

         ">> Cost Electronic Document No Clear
         IF NOT R_ZFDOCNO[] IS INITIAL.
            SELECT * INTO TABLE IT_ZTFINHD
                     FROM    ZTFINHD
                     WHERE   ZFDHENO   IN  R_ZFDOCNO.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_ZTFINHD.
                  W_TABIX = SY-TABIX.
                  MOVE : 'N'           TO   IT_ZTFINHD-ZFDBYN,
                         SPACE         TO   IT_ZTFINHD-BUKRS,
                         SPACE         TO   IT_ZTFINHD-GJAHR,
                         SPACE         TO   IT_ZTFINHD-BELNR,
                         SPACE         TO   IT_ZTFINHD-ZFDBDT,
                         SPACE         TO   IT_ZTFINHD-ZFDBTM,
                         SPACE         TO   IT_ZTFINHD-ZFDBID.
                  MODIFY IT_ZTFINHD     INDEX   W_TABIX.
               ENDLOOP.
               MODIFY ZTFINHD FROM TABLE IT_ZTFINHD.
            ENDIF.

            SELECT * INTO TABLE IT_ZTDHF1
                     FROM ZTDHF1
                     WHERE ZFDHENO  IN  R_ZFDOCNO.
            IF SY-SUBRC EQ 0.
               LOOP AT IT_ZTDHF1.
                  W_TABIX = SY-TABIX.
                  MOVE : 'N'           TO   IT_ZTDHF1-ZFDHAPP,
                         SPACE         TO   IT_ZTDHF1-ZFDHSSD,
                         SPACE         TO   IT_ZTDHF1-ZFDHSST.
                  MODIFY IT_ZTDHF1     INDEX   W_TABIX.
               ENDLOOP.
               MODIFY ZTDHF1 FROM TABLE IT_ZTDHF1.
            ENDIF.
         ENDIF.

      WHEN OTHERS.
         UPDATE   ZTBKPF.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTBKPF'
              EXPORTING
                      UPD_CHNGIND    =    'U'
                      N_ZTBKPF       =    W_ZTBKPF
                      O_ZTBKPF       =    W_ZTBKPF_OLD.
*----------------------------------------------------------------------
         ">> Cost Item
         SELECT * FROM ZTBSEG
                  WHERE BUKRS  EQ  BUKRS
                  AND   BELNR  EQ  BELNR
                  AND   GJAHR  EQ  GJAHR.
            CLEAR : IT_ZSBSEG.
            READ TABLE IT_ZSBSEG
                       WITH KEY BUKRS = ZTBSEG-BUKRS
                                BELNR = ZTBSEG-BELNR
                                GJAHR = ZTBSEG-GJAHR
                                BUZEI = ZTBSEG-BUZEI
                       BINARY SEARCH.
            W_sy_SUBRC  = SY-SUBRC.
            W_TABIX  = SY-TABIX.
            IF W_sy_SUBRC EQ 0.
               MOVE-CORRESPONDING ZTBSEG     TO *ZTBSEG.
               MOVE-CORRESPONDING IT_ZSBSEG  TO ZTBSEG.
               MOVE : SY-MANDT               TO ZTBSEG-MANDT,
                      BUKRS                  TO ZTBSEG-BUKRS,
                      BELNR                  TO ZTBSEG-BELNR,
                      GJAHR                  TO ZTBSEG-GJAHR,
                      SY-UNAME               TO ZTBSEG-UNAM,
                      SY-UZEIT               TO ZTBSEG-UTME,
                      SY-DATUM               TO ZTBSEG-UDAT.
               UPDATE ZTBSEG.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTBSEG'
                    EXPORTING
                            UPD_CHNGIND    =    'U'
                            N_ZTBSEG       =    ZTBSEG
                            O_ZTBSEG       =   *ZTBSEG.
*----------------------------------------------------------------------
               MOVE-CORRESPONDING ZTBSEG     TO IT_ZSBSEG.
               MODIFY IT_ZSBSEG   INDEX   W_TABIX.
            ELSE.
               DELETE ZTBSEG.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               MOVE-CORRESPONDING ZTBSEG     TO *ZTBSEG.

* change document -----------------------------------------------------
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTBSEG'
                    EXPORTING
                            UPD_CHNGIND    =    'D'
                            N_ZTBSEG       =    ZTBSEG
                            O_ZTBSEG       =   *ZTBSEG.
*----------------------------------------------------------------------
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSBSEG.
            W_TABIX = SY-TABIX.
            SELECT SINGLE * FROM  ZTBSEG
                   WHERE BUKRS  EQ  IT_ZSBSEG-BUKRS
                   AND   BELNR  EQ  IT_ZSBSEG-BELNR
                   AND   GJAHR  EQ  IT_ZSBSEG-GJAHR
                   AND   BUZEI  EQ  IT_ZSBSEG-BUZEI.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSBSEG  TO ZTBSEG.

               SELECT MAX( BUZEI ) INTO ZTBSEG-BUZEI
                      FROM ZTBSEG
                      WHERE BUKRS  EQ  IT_ZSBSEG-BUKRS
                      AND   BELNR  EQ  IT_ZSBSEG-BELNR
                      AND   GJAHR  EQ  IT_ZSBSEG-GJAHR.

               ADD    1    TO   ZTBSEG-BUZEI.

               MOVE: SY-MANDT     TO     ZTBSEG-MANDT,
                     BUKRS        TO     ZTBSEG-BUKRS,
                     BELNR        TO     ZTBSEG-BELNR,
                     GJAHR        TO     ZTBSEG-GJAHR,
                     SY-UNAME     TO     ZTBSEG-UNAM,
                     SY-UZEIT     TO     ZTBSEG-UTME,
                     SY-DATUM     TO     ZTBSEG-UDAT,
                     SY-UNAME     TO     ZTBSEG-USNAM,
                     SY-DATUM     TO     ZTBSEG-CPUDT,
                     SY-UZEIT     TO     ZTBSEG-CPUTM.

               INSERT  ZTBSEG.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               CLEAR *ZTBSEG.
* change document -----------------------------------------------------
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTBSEG'
                    EXPORTING
                            UPD_CHNGIND    =    'I'
                            N_ZTBSEG       =    ZTBSEG
                            O_ZTBSEG       =   *ZTBSEG.
*----------------------------------------------------------------------
               MOVE-CORRESPONDING ZTBSEG     TO IT_ZSBSEG.
               MODIFY IT_ZSBSEG   INDEX   W_TABIX.
            ENDIF.
         ENDLOOP.
   ENDCASE.

*------------------------------------------------------------------
*>> Charge Document Amount Distribution
*------------------------------------------------------------------
   REFRESH : IT_ZSBSEG1, IT_CHARGE_BL, IT_ZSBDIV, IT_ZSBDIV_TEMP.

   IF ZFSTATUS NE 'X'.
      "---------------------------------------------
      " Line Item Check
      "---------------------------------------------
      CALL FUNCTION 'ZIM_CHARGE_ITEM_CREATE'
           EXPORTING
               BUKRS    =    BUKRS
               GJAHR    =    GJAHR
               BELNR    =    BELNR
               ZFIMDNO  =    IT_ZSBSEG-ZFIMDNO
               ZTBKPF   =    ZTBKPF
           TABLES
               IT_ZSBSEG =   IT_ZSBSEG
               IT_ZTBDIV =   IT_ZSBDIV_TEMP.

      DESCRIBE TABLE IT_ZSBDIV_TEMP LINES W_TOTAL_LINE.
      IF W_TOTAL_LINE GT 450.
         ZTBKPF-ZFOVROW = 'X'.
      ELSE.
         CLEAR : ZTBKPF-ZFOVROW.
      ENDIF.

      IF ZTBKPF-ZFOVROW NE 'X'.
         DELETE  FROM ZTBDIV
                    WHERE BUKRS  EQ  BUKRS
                    AND   BELNR  EQ  BELNR
                    AND   GJAHR  EQ  GJAHR.
         IT_ZSBDIV[] = IT_ZSBDIV_TEMP[].

         INSERT ZTBDIV  FROM TABLE IT_ZSBDIV.
         IF SY-SUBRC NE 0.
            RAISE ERROR_UPDATE.
         ENDIF.
         UPDATE  ZTBKPF
         SET     ZFOVROW  =  ' '
         WHERE   BUKRS    EQ  BUKRS
         AND     BELNR    EQ  BELNR
         AND     GJAHR    EQ  GJAHR.

      ELSE.
         DELETE  FROM ZTBDIV
                    WHERE BUKRS  EQ  BUKRS
                    AND   BELNR  EQ  BELNR
                    AND   GJAHR  EQ  GJAHR.

         DELETE  FROM ZTBSEG1
                    WHERE BUKRS  EQ  BUKRS
                    AND   BELNR  EQ  BELNR
                    AND   GJAHR  EQ  GJAHR.

         UPDATE  ZTBKPF
         SET     ZFOVROW  =  'X'
         WHERE   BUKRS    EQ  BUKRS
         AND     BELNR    EQ  BELNR
         AND     GJAHR    EQ  GJAHR.

         ">> Account Document Line > 450 -> Document Separate
         SORT IT_ZSBSEG      BY BUKRS BELNR GJAHR BUZEI.
         SORT IT_ZSBDIV_TEMP BY BUKRS BELNR GJAHR BUZEI ZFBSEQ.
         READ TABLE IT_ZSBDIV_TEMP WITH KEY ZFBSEQ = '00000'
                                            NEWBS  = '31'.

         MOVE-CORRESPONDING IT_ZSBDIV_TEMP  TO  IT_ZSBDIV1.

         W_LINE_INDEX  =  1.

         REFRESH : IT_ZSBSEG1, IT_ZSBDIV.
         LOOP AT IT_ZSBSEG.

            CLEAR : W_CNT_LINE, W_TOT_DMBTR, W_TOT_WRBTR,
                    W_DMBTR,    W_WRBTR.
            LOOP AT IT_ZSBDIV_TEMP WHERE BUKRS  EQ IT_ZSBSEG-BUKRS
                                   AND   BELNR  EQ IT_ZSBSEG-BELNR
                                   AND   GJAHR  EQ IT_ZSBSEG-GJAHR
                                   AND   BUZEI  EQ IT_ZSBSEG-BUZEI
                                   AND   ZFBSEQ NE '00000'.

               W_DIV_INDEX = W_DIV_INDEX + 1.
               W_CNT_LINE  = W_CNT_LINE  + 1.
               W_CNT_MODE  = W_CNT_LINE MOD 450.

               MOVE-CORRESPONDING IT_ZSBDIV_TEMP  TO  IT_ZSBDIV.
               MOVE : W_LINE_INDEX    TO  IT_ZSBDIV-DBUZEI,
                      W_DIV_INDEX     TO  IT_ZSBDIV-ZFBSEQ.
               APPEND  IT_ZSBDIV.

               ADD : IT_ZSBDIV_TEMP-DMBTR  TO  W_TOT_DMBTR,
                     IT_ZSBDIV_TEMP-WRBTR  TO  W_TOT_WRBTR,
                     IT_ZSBDIV_TEMP-DMBTR  TO  W_DMBTR,
                     IT_ZSBDIV_TEMP-WRBTR  TO  W_WRBTR.

               MOVE-CORRESPONDING IT_ZSBSEG       TO  IT_ZSBSEG1.

               IF W_CNT_MODE EQ 0.
                  MOVE : W_DMBTR         TO  IT_ZSBSEG1-DMBTR,
                         W_WRBTR         TO  IT_ZSBSEG1-WRBTR,
                         W_LINE_INDEX    TO  IT_ZSBSEG1-DBUZEI.
                  APPEND  IT_ZSBSEG1.
                  MOVE-CORRESPONDING IT_ZSBDIV1 TO IT_ZSBDIV.
                  MOVE : W_LINE_INDEX    TO  IT_ZSBDIV-DBUZEI,
                         '00000'         TO  IT_ZSBDIV-ZFBSEQ,
                         W_DMBTR         TO  IT_ZSBDIV-DMBTR,
                         W_WRBTR         TO  IT_ZSBDIV-WRBTR,
                         '31'            TO  IT_ZSBDIV-NEWBS,
                         ZTBKPF-LIFNR    TO  IT_ZSBDIV-NEWKO.

                  APPEND IT_ZSBDIV.
                  CLEAR   IT_ZSBSEG1.
                  CLEAR : W_DMBTR, W_WRBTR, W_DIV_INDEX.
                  W_LINE_INDEX  =  W_LINE_INDEX  + 1.
               ENDIF.

               AT LAST.
** Changed by Furong for dump- 451 records of IT_ZSBDIV_TEMP
*                 IF IT_ZSBSEG1 is initial.
*                 else.
                  MOVE : W_LINE_INDEX    TO  IT_ZSBSEG1-DBUZEI,
                         W_DMBTR         TO  IT_ZSBSEG1-DMBTR,
                         W_WRBTR         TO  IT_ZSBSEG1-WRBTR.
                  APPEND  IT_ZSBSEG1.
                  CLEAR : IT_ZSBSEG1.
                  MOVE-CORRESPONDING IT_ZSBDIV1 TO IT_ZSBDIV.
                  MOVE : W_LINE_INDEX    TO  IT_ZSBDIV-DBUZEI,
                         '00000'         TO  IT_ZSBDIV-ZFBSEQ,
                         W_DMBTR         TO  IT_ZSBDIV-DMBTR,
                         W_WRBTR         TO  IT_ZSBDIV-WRBTR,
                         '31'            TO  IT_ZSBDIV-NEWBS,
                         ZTBKPF-LIFNR    TO  IT_ZSBDIV-NEWKO.
                  APPEND IT_ZSBDIV.
*                  endif.
               ENDAT.
** End of change
            ENDLOOP.

            ">> AMOUNT CONTROL.
            DESCRIBE TABLE IT_ZSBSEG1 LINES W_BSEG_LINE.
            READ     TABLE IT_ZSBSEG1 INDEX W_BSEG_LINE.
            IF W_TOT_DMBTR  NE  IT_ZSBSEG-DMBTR.
               IT_ZSBSEG1-DMBTR  =  IT_ZSBSEG1-DMBTR  +
                                  ( IT_ZSBSEG-DMBTR - W_TOT_DMBTR  ).
            ENDIF.
            IF W_TOT_WRBTR  NE  IT_ZSBSEG-WRBTR.
               IT_ZSBSEG1-WRBTR  =  IT_ZSBSEG1-WRBTR  +
                                  ( IT_ZSBSEG-WRBTR - W_TOT_WRBTR  ).
            ENDIF.
            MODIFY IT_ZSBSEG1 INDEX W_BSEG_LINE.

            CLEAR : W_TOT_DMBTR, W_TOT_WRBTR, W_BAL_DMBTR, W_BAL_WRBTR.
            LOOP AT IT_ZSBDIV  WHERE BUKRS  EQ IT_ZSBSEG1-BUKRS
                               AND   BELNR  EQ IT_ZSBSEG1-BELNR
                               AND   GJAHR  EQ IT_ZSBSEG1-GJAHR
                               AND   BUZEI  EQ IT_ZSBSEG1-BUZEI
                               AND   DBUZEI EQ IT_ZSBSEG1-DBUZEI
                               AND   ZFBSEQ NE '00000'.
                W_TOT_DMBTR = W_TOT_DMBTR + IT_ZSBDIV-DMBTR.
                W_TOT_WRBTR = W_TOT_WRBTR + IT_ZSBDIV-WRBTR.
            ENDLOOP.

            W_BAL_DMBTR = W_TOT_DMBTR - IT_ZSBSEG1-DMBTR.
            W_BAL_WRBTR = W_TOT_WRBTR - IT_ZSBSEG1-WRBTR.

            IF W_BAL_DMBTR LT 0.
               W_MINUS_DMBTR = W_BAL_DMBTR * -1.
            ELSE.
               W_MINUS_DMBTR = W_BAL_DMBTR.
            ENDIF.

            IF W_BAL_WRBTR LT 0.
               W_MINUS_WRBTR = W_BAL_WRBTR * -1.
            ELSE.
               W_MINUS_WRBTR = W_BAL_WRBTR.
            ENDIF.

            IF W_MINUS_DMBTR NE 0.

               LOOP AT IT_ZSBDIV WHERE BUKRS  = IT_ZSBSEG1-BUKRS
                                   AND BELNR  = IT_ZSBSEG1-BELNR
                                   AND GJAHR  = IT_ZSBSEG1-GJAHR
                                   AND BUZEI  = IT_ZSBSEG1-BUZEI
                                   AND DBUZEI = IT_ZSBSEG1-DBUZEI.
                  IF IT_ZSBDIV-ZFBSEQ NE '00000' AND
                     IT_ZSBDIV-DMBTR  GE W_MINUS_DMBTR.
                     W_BAL_INDEX = SY-TABIX.
* Begin of changes -  UD1K920953
               IT_ZSBDIV-DMBTR  =  IT_ZSBDIV-DMBTR  + W_MINUS_DMBTR.
               MODIFY IT_ZSBDIV INDEX W_BAL_INDEX.
* End of changes    - UD1K920953
                     EXIT.
                  ENDIF.
               ENDLOOP.
* Begin of changes - UD1K920953
* In the above case if record is not found  which meets above condition
* W_BAL_INDEX will be zero. So program will try to modify internal
* table with record index zero-> Which will be not found in int table.
*               IT_ZSBDIV-DMBTR  =  IT_ZSBDIV-DMBTR  + W_MINUS_DMBTR.
*               MODIFY IT_ZSBDIV INDEX W_BAL_INDEX.
* End of changes - UD1K920953
            ENDIF.
            IF W_MINUS_WRBTR NE 0.
               LOOP AT IT_ZSBDIV WHERE BUKRS  = IT_ZSBSEG1-BUKRS
                                   AND BELNR  = IT_ZSBSEG1-BELNR
                                   AND GJAHR  = IT_ZSBSEG1-GJAHR
                                   AND BUZEI  = IT_ZSBSEG1-BUZEI
                                   AND DBUZEI = IT_ZSBSEG1-DBUZEI.
                  IF IT_ZSBDIV-ZFBSEQ NE '00000' AND
                     IT_ZSBDIV-WRBTR  GE W_MINUS_WRBTR.
                     W_BAL_INDEX = SY-TABIX.
* Begin of changes -  UD1K920953
                IT_ZSBDIV-WRBTR  =  IT_ZSBDIV-WRBTR  + W_MINUS_WRBTR.
                MODIFY IT_ZSBDIV INDEX W_BAL_INDEX.
* End of changes -  UD1K920953
                     EXIT.
                  ENDIF.
               ENDLOOP.
* Begin of changes -  UD1K920953
*               IT_ZSBDIV-WRBTR  =  IT_ZSBDIV-WRBTR  + W_MINUS_WRBTR.
*               MODIFY IT_ZSBDIV INDEX W_BAL_INDEX.
* End  of changes -  UD1K920953
            ENDIF.
         ENDLOOP.

         INSERT ZTBSEG1  FROM TABLE IT_ZSBSEG1.
         INSERT ZTBDIV   FROM TABLE IT_ZSBDIV.
         IF SY-SUBRC NE 0.
            RAISE ERROR_UPDATE.
         ENDIF.
      ENDIF.
   ENDIF.

ENDFUNCTION.
