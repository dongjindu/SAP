FUNCTION ZIM_ZTTAXBOOK_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_OK_CODE)
*"     VALUE(ZFSTATUS)
*"     VALUE(ZFTBNO) LIKE  ZTTAXBKHD-ZFTBNO
*"     VALUE(NZTTAXBKHD) LIKE  ZTTAXBKHD STRUCTURE  ZTTAXBKHD
*"     VALUE(OZTTAXBKHD) LIKE  ZTTAXBKHD STRUCTURE  ZTTAXBKHD
*"  TABLES
*"      IT_ZSTAXBKIT STRUCTURE  ZSTAXBKIT OPTIONAL
*"      IT_ZSTAXBKIT_ORG STRUCTURE  ZSTAXBKIT OPTIONAL
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
DATA : L_ZFTBIT      LIKE   ZTTAXBKIT-ZFTBIT,
       L_COUNT       TYPE   I.

* 삭제 지시 체크
   IF  W_OK_CODE EQ 'DELE'.   ZFSTATUS = 'X'.   ENDIF.

   MOVE-CORRESPONDING : NZTTAXBKHD   TO   ZTTAXBKHD.

   MOVE : ZFTBNO   TO    ZTTAXBKHD-ZFTBNO,
          SY-MANDT TO    ZTTAXBKHD-MANDT,
          SY-DATUM TO    ZTTAXBKHD-UDAT,
          SY-UNAME TO    ZTTAXBKHD-UNAM.

*-----------------------------------------------------------------------
* LOCAL L/C MODIFY
*-----------------------------------------------------------------------
   CASE ZFSTATUS.
      WHEN 'C'.               " 생성

         MOVE : SY-DATUM TO    ZTTAXBKHD-CDAT,
                SY-UNAME TO    ZTTAXBKHD-ERNAM.
         INSERT     ZTTAXBKHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         CLEAR : *ZTTAXBKHD.

         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTTAXBKHD'
              EXPORTING
                      UPD_CHNGIND    =    'I'
                      N_ZTTAXBKHD    =    ZTTAXBKHD
                      O_ZTTAXBKHD    =   *ZTTAXBKHD.

         CLEAR : L_ZFTBIT.
         LOOP AT IT_ZSTAXBKIT.
            CLEAR : ZTTAXBKIT.
            ADD 1 TO L_ZFTBIT.
            MOVE-CORRESPONDING IT_ZSTAXBKIT TO ZTTAXBKIT.
            MOVE : ZFTBNO                   TO ZTTAXBKIT-ZFTBNO,
                   L_ZFTBIT                 TO ZTTAXBKIT-ZFTBIT,
                   SY-MANDT                 TO ZTTAXBKIT-MANDT.

            INSERT   ZTTAXBKIT.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

            CLEAR : *ZTTAXBKIT.
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTTAXBKIT'
                 EXPORTING
                      UPD_CHNGIND    =    'I'
                      N_ZTTAXBKIT    =    ZTTAXBKIT
                      O_ZTTAXBKIT    =   *ZTTAXBKIT.

         ENDLOOP.
*-----------------------------------------------------------------------
* 삭제
*-----------------------------------------------------------------------
      WHEN 'X'.               " 삭제
         DELETE FROM ZTTAXBKHD WHERE ZFTBNO EQ ZFTBNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         DELETE FROM ZTTAXBKIT
                WHERE ZFTBNO EQ ZFTBNO.

         DELETE FROM ZTTAXBKCST
                WHERE ZFTBNO EQ ZFTBNO.

         EXIT.
*-----------------------------------------------------------------------
* Update
*-----------------------------------------------------------------------
      WHEN OTHERS.            " 변경
         UPDATE     ZTTAXBKHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTTAXBKHD'
              EXPORTING
                      UPD_CHNGIND    =    'U'
                      N_ZTTAXBKHD    =    ZTTAXBKHD
                      O_ZTTAXBKHD    =   OZTTAXBKHD.

* 아이템.
         SELECT * FROM ZTTAXBKIT   WHERE ZFTBNO  EQ  ZFTBNO.
            MOVE-CORRESPONDING  ZTTAXBKIT TO *ZTTAXBKIT.
            READ TABLE IT_ZSTAXBKIT
                 WITH KEY ZFTBIT = ZTTAXBKIT-ZFTBIT BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSTAXBKIT   TO ZTTAXBKIT.
               MOVE : ZFTBNO                     TO ZTTAXBKIT-ZFTBNO,
                      SY-MANDT                   TO ZTTAXBKIT-MANDT.
               UPDATE ZTTAXBKIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTTAXBKIT'
                    EXPORTING
                      UPD_CHNGIND    =    'U'
                      N_ZTTAXBKIT    =    ZTTAXBKIT
                      O_ZTTAXBKIT    =   *ZTTAXBKIT.
            ELSE.
               DELETE ZTTAXBKIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTTAXBKIT'
                    EXPORTING
                      UPD_CHNGIND    =    'D'
                      N_ZTTAXBKIT    =    ZTTAXBKIT
                      O_ZTTAXBKIT    =   *ZTTAXBKIT.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSTAXBKIT.
            SELECT SINGLE * FROM  ZTTAXBKIT
                            WHERE ZFTBNO  EQ  ZFTBNO
                            AND   ZFTBIT  EQ  IT_ZSTAXBKIT-ZFTBIT.

            IF SY-SUBRC NE 0.
               SELECT MAX( ZFTBIT ) INTO L_ZFTBIT
                      FROM  ZTTAXBKIT
                      WHERE ZFTBNO   EQ   ZFTBNO.

               ADD 1 TO L_ZFTBIT.

               MOVE-CORRESPONDING IT_ZSTAXBKIT   TO ZTTAXBKIT.
               MOVE : ZFTBNO                     TO ZTTAXBKIT-ZFTBNO,
                      L_ZFTBIT                   TO ZTTAXBKIT-ZFTBIT,
                      SY-MANDT                   TO ZTTAXBKIT-MANDT.
               INSERT  ZTTAXBKIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

               CLEAR : *ZTTAXBKIT.
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTTAXBKIT'
                    EXPORTING
                      UPD_CHNGIND    =    'I'
                      N_ZTTAXBKIT    =    ZTTAXBKIT
                      O_ZTTAXBKIT    =   *ZTTAXBKIT.
            ENDIF.
         ENDLOOP.
   ENDCASE.

*--------------------------------------------------------------------
*> 전기용 배부내역.
*--------------------------------------------------------------------
DATA: L_BKMENGE LIKE  ZTTAXBKIT-BKMENGE,
      L_ZFAMT   LIKE  ZTTAXBKCST-ZFAMT.

   REFRESH : IT_ZSTAXBKIT, IT_ZTTAXBKCST1.
   CLEAR: IT_ZSTAXBKIT, IT_ZTTAXBKCST1.

   SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZSTAXBKIT
            FROM  ZTTAXBKIT
            WHERE ZFTBNO EQ ZFTBNO.

   SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZTTAXBKCST1
            FROM  ZTTAXBKCST
            WHERE ZFTBNO EQ ZFTBNO.

   REFRESH : IT_ZTTAXBKCST.
   LOOP AT IT_ZSTAXBKIT.
      MOVE-CORRESPONDING IT_ZSTAXBKIT TO IT_ZTTAXBKCST.
      MOVE: IT_ZSTAXBKIT-EBELN        TO IT_ZTTAXBKCST-KONNR,
            IT_ZSTAXBKIT-EBELP        TO IT_ZTTAXBKCST-KTPNR,
            IT_ZSTAXBKIT-ZFCUAMT      TO IT_ZTTAXBKCST-ZFAMT.

      SELECT SINGLE * FROM EKKO
             WHERE    EBELN EQ IT_ZSTAXBKIT-EBELN.

      IF EKKO-BSTYP EQ 'K'.   ">일괄계약일 경우.
         SELECT * INTO TABLE IT_EKAB
                   FROM EKAB
                  WHERE KONNR EQ IT_ZSTAXBKIT-EBELN
                    AND KTPNR EQ IT_ZSTAXBKIT-EBELP.

         SELECT * INTO TABLE IT_EKBE
                  FROM EKBE
                  FOR ALL ENTRIES IN IT_EKAB
                  WHERE EBELN EQ IT_EKAB-EBELN
                  AND   EBELP EQ IT_EKAB-EBELP
                  AND   VGABE EQ '1'
                  AND   BEWTP EQ 'E'
                  AND   SHKZG EQ 'S'.
      ELSE.
         SELECT * INTO TABLE IT_EKBE
                  FROM EKBE
                  WHERE EBELN EQ IT_ZSTAXBKIT-EBELN
                  AND   EBELP EQ IT_ZSTAXBKIT-EBELP
                  AND   VGABE EQ '1'
                  AND   BEWTP EQ 'E'
                  AND   SHKZG EQ 'S'.
      ENDIF.

      CLEAR : L_BKMENGE, L_ZFAMT.

      LOOP AT IT_EKBE.
         SELECT COUNT( * ) INTO L_COUNT
                FROM ZTTAXBKCST
                WHERE EBELN  EQ IT_EKBE-EBELN
                AND   EBELP  EQ IT_EKBE-EBELP
                AND   GJAHR  EQ IT_EKBE-GJAHR
                AND   BELNR  EQ IT_EKBE-BELNR
                AND   BUZEI  EQ IT_EKBE-BUZEI
                AND   ZFTBNO NE ZFTBNO.
         IF L_COUNT GT 0.
            CONTINUE.
         ENDIF.

         ADD IT_EKBE-MENGE TO L_BKMENGE.

         MOVE: IT_EKBE-EBELN  TO IT_ZTTAXBKCST-EBELN,
               IT_EKBE-EBELP  TO IT_ZTTAXBKCST-EBELP,
               IT_EKBE-GJAHR  TO IT_ZTTAXBKCST-GJAHR,
               IT_EKBE-BELNR  TO IT_ZTTAXBKCST-BELNR,
               IT_EKBE-BUZEI  TO IT_ZTTAXBKCST-BUZEI.

         IT_ZTTAXBKCST-ZFAMT = ( IT_EKBE-MENGE / IT_ZSTAXBKIT-BKMENGE )
                             *   IT_ZSTAXBKIT-ZFCUAMT.

         ADD IT_ZTTAXBKCST-ZFAMT TO L_ZFAMT.
*------------------------------------------------------------------
*> 수량 조정 작업.
*------------------------------------------------------------------
         IF L_BKMENGE GT IT_ZSTAXBKIT-BKMENGE.
            IT_ZTTAXBKCST-BKMENGE = IT_EKBE-MENGE -
                                  ( L_BKMENGE - IT_ZSTAXBKIT-BKMENGE ).
         ELSE.
            IT_ZTTAXBKCST-BKMENGE = IT_EKBE-MENGE.
         ENDIF.

*------------------------------------------------------------------
*> 금액 조정 작업.
*------------------------------------------------------------------
         IF L_ZFAMT GT IT_ZSTAXBKIT-ZFCUAMT.
            IT_ZTTAXBKCST-ZFAMT  = IT_ZTTAXBKCST-ZFAMT -
                                 ( L_ZFAMT - IT_ZSTAXBKIT-ZFCUAMT ).
            IF IT_ZTTAXBKCST-ZFAMT   GT 0 AND
               IT_ZTTAXBKCST-BKMENGE GT 0.
               APPEND IT_ZTTAXBKCST.
            ENDIF.
         ELSE.
            IF IT_ZTTAXBKCST-ZFAMT   GT 0 AND
               IT_ZTTAXBKCST-BKMENGE GT 0.
               APPEND IT_ZTTAXBKCST.
            ENDIF.
         ENDIF.
      ENDLOOP.
   ENDLOOP.

   IF NOT IT_ZTTAXBKCST1[] IS INITIAL.
      DELETE ZTTAXBKCST FROM TABLE IT_ZTTAXBKCST1.
   ENDIF.

   MODIFY ZTTAXBKCST FROM TABLE IT_ZTTAXBKCST.


ENDFUNCTION.
