FUNCTION ZIM_COMMERCIAL_INVOICE_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFCIVRN) LIKE  ZTCIVHD-ZFCIVRN
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTCIVHD_OLD) LIKE  ZTCIVHD STRUCTURE  ZTCIVHD
*"     VALUE(W_ZTCIVHD) LIKE  ZTCIVHD STRUCTURE  ZTCIVHD
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSCIVIT_OLD STRUCTURE  ZSCIVIT OPTIONAL
*"      IT_ZSCIVIT STRUCTURE  ZSCIVIT
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
DATA : W_ZFCIVSQ     LIKE   ZTCIVIT-ZFCIVSQ.

DATA : BEGIN OF IT_EBELN  OCCURS 10,
       EBELN    LIKE      EKKO-EBELN,
       END   OF IT_EBELN.

DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

   MOVE-CORRESPONDING : W_ZTCIVHD      TO   ZTCIVHD.

   MOVE : ZFCIVRN      TO     ZTCIVHD-ZFCIVRN,
          SY-MANDT     TO     ZTCIVHD-MANDT,
          SY-UNAME     TO     ZTCIVHD-UNAM,
          SY-DATUM     TO     ZTCIVHD-UDAT.

   IF W_OK_CODE EQ 'DELE'.
      ZFSTATUS = 'X'.
   ELSE.
      REFRESH : IT_EBELN.
      IF ZTIMIMG00-ZFEXFIX EQ 'X' AND
         ZTIMIMG00-ZFEXMTD EQ 'I' AND
         NOT ( ZTCIVHD-ZFREQTY EQ 'LO' OR
               ZTCIVHD-ZFREQTY EQ 'PU' ).
         LOOP AT IT_ZSCIVIT.
            IF NOT IT_ZSCIVIT-EBELN IS INITIAL.
               SELECT SINGLE * FROM EKKO
                      WHERE EBELN EQ IT_ZSCIVIT-EBELN.
               IF EKKO-WAERS NE ZTBL-ZFBLAMC.
                  MESSAGE S667 WITH EKKO-EBELN EKKO-WAERS
                                    ZTCIVHD-ZFIVAMC.
                  EXIT.
               ENDIF.

               IF SY-SUBRC EQ 0.
                  IF NOT ( ZTCIVHD-ZFEXRT  EQ EKKO-WKURS AND
                           EKKO-KUFIX      EQ 'X'        AND
                           ZTCIVHD-ZFIVAMC EQ EKKO-WAERS ).
                     MOVE : IT_ZSCIVIT-EBELN TO IT_EBELN.
                     COLLECT IT_EBELN.
                  ENDIF.
               ENDIF.
            ENDIF.
         ENDLOOP.
      ENDIF.

      IF NOT IT_EBELN[] IS INITIAL.
         LOOP AT IT_EBELN.
            CLEAR : BAPIMEPOHEADER,
                    BAPIMEPOHEADERX.

            MOVE : 'X'            TO  BAPIMEPOHEADERX-EXCH_RATE,
                   'X'            TO  BAPIMEPOHEADERX-EX_RATE_FX,
                   IT_EBELN-EBELN TO  BAPIMEPOHEADER-PO_NUMBER,
                   ZTCIVHD-ZFEXRT TO  BAPIMEPOHEADER-EXCH_RATE,
                   'X'            TO  BAPIMEPOHEADER-EX_RATE_FX.

            REFRESH : RETURN.
            CALL FUNCTION 'ZIM_BAPI_PO_CHANGE'
                   EXPORTING
                      PURCHASEORDER = IT_EBELN-EBELN
                      POHEADER      = BAPIMEPOHEADER
                      POHEADERX     = BAPIMEPOHEADERX
                   TABLES
                      RETURN        = RETURN.

            LOOP AT RETURN WHERE TYPE EQ 'E'.
               MESSAGE ID RETURN-ID  TYPE 'S'     NUMBER RETURN-NUMBER
                       WITH RETURN-MESSAGE_V1 RETURN-MESSAGE_V2
                            RETURN-MESSAGE_V3 RETURN-MESSAGE_V4.
               EXIT.
            ENDLOOP.
         ENDLOOP.
      ENDIF.
   ENDIF.

   CASE ZFSTATUS.
      WHEN 'C'.               " 생성
         MOVE : SY-UNAME      TO    ZTCIVHD-ERNAM,
                SY-DATUM      TO    ZTCIVHD-CDAT.

         INSERT   ZTCIVHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

* change document -----------------------------------------------------
         CLEAR : W_ZTCIVHD_OLD.
         call function 'ZIM_CHANGE_DOCUMENT_CIV'
              exporting
                      upd_chngind    =    'I'
                      N_ZTCIVHD      =    W_ZTCIVHD
                      O_ZTCIVHD      =    W_ZTCIVHD_OLD.
*----------------------------------------------------------------------

         LOOP AT IT_ZSCIVIT.
            CLEAR : ZTCIVIT.
            MOVE-CORRESPONDING IT_ZSCIVIT TO ZTCIVIT.
            MOVE : ZFCIVRN                TO ZTCIVIT-ZFCIVRN,
                   ZTCIVHD-ZFPRPYN        TO ZTCIVIT-ZFPRPYN,
                   SY-MANDT               TO ZTCIVIT-MANDT,
                   SY-UNAME               TO ZTCIVIT-ERNAM,
                   SY-DATUM               TO ZTCIVIT-CDAT,
                   SY-UNAME               TO ZTCIVIT-UNAM,
                   SY-DATUM               TO ZTCIVIT-UDAT.

            INSERT   ZTCIVIT.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         CLEAR : *ZTCIVIT.
         call function 'ZIM_CHANGE_DOCUMENT_CIVIT'
              exporting
                      upd_chngind    =    'I'
                      N_ZTCIVIT      =    ZTCIVIT
                      O_ZTCIVIT      =    *ZTCIVIT.
*----------------------------------------------------------------------
         ENDLOOP.

      WHEN 'X'.               " 삭제
         DELETE  FROM ZTCIVHD     WHERE ZFCIVRN  EQ ZFCIVRN.
         IF SY-SUBRC NE 0.     RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         call function 'ZIM_CHANGE_DOCUMENT_CIV'
              exporting
                      upd_chngind    =    'D'
                      N_ZTCIVHD      =    W_ZTCIVHD
                      O_ZTCIVHD      =    W_ZTCIVHD_OLD.
*----------------------------------------------------------------------

         DELETE  FROM ZTCIVIT  WHERE ZFCIVRN EQ ZFCIVRN.
         LOOP  AT  IT_ZSCIVIT_OLD.
* change document -----------------------------------------------------
            MOVE-CORRESPONDING IT_ZSCIVIT TO ZTCIVIT.
            call function 'ZIM_CHANGE_DOCUMENT_CIVIT'
                 exporting
                         upd_chngind    =    'D'
                         N_ZTCIVIT      =    ZTCIVIT
                         O_ZTCIVIT      =    ZTCIVIT.
*----------------------------------------------------------------------
         ENDLOOP.

      WHEN OTHERS.            " 변경
         UPDATE   ZTCIVHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         call function 'ZIM_CHANGE_DOCUMENT_CIV'
              exporting
                      upd_chngind    =    'U'
                      N_ZTCIVHD      =    W_ZTCIVHD
                      O_ZTCIVHD      =    W_ZTCIVHD_OLD.
*----------------------------------------------------------------------
* INVOCIE 자재.
         SELECT * FROM ZTCIVIT WHERE ZFCIVRN   EQ  ZFCIVRN.

            READ TABLE IT_ZSCIVIT WITH KEY ZFCIVSQ  = ZTCIVIT-ZFCIVSQ
                                  BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSCIVIT TO ZTCIVIT.
               MOVE : SY-UNAME               TO ZTCIVIT-UNAM,
                      SY-DATUM               TO ZTCIVIT-UDAT,
                      ZTCIVHD-ZFPRPYN        TO ZTCIVIT-ZFPRPYN.
               UPDATE ZTCIVIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               READ  TABLE IT_ZSCIVIT_OLD  WITH KEY
                           ZFCIVRN   =  ZFCIVRN
                           ZFCIVSQ   =  IT_ZSCIVIT-ZFCIVSQ.
               IF SY-SUBRC EQ 0.
                  MOVE-CORRESPONDING IT_ZSCIVIT_OLD TO *ZTCIVIT.
               ENDIF.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CIVIT'
                    exporting
                            upd_chngind    =    'U'
                            N_ZTCIVIT      =    ZTCIVIT
                            O_ZTCIVIT      =    *ZTCIVIT.
*----------------------------------------------------------------------

            ELSE.
               DELETE ZTCIVIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               MOVE-CORRESPONDING IT_ZSCIVIT  TO ZTCIVIT.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CIVIT'
                    exporting
                            upd_chngind    =    'D'
                            N_ZTCIVIT      =    ZTCIVIT
                            O_ZTCIVIT      =    *ZTCIVIT.
*----------------------------------------------------------------------

            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSCIVIT.
            SELECT SINGLE * FROM  ZTCIVIT
                            WHERE ZFCIVRN   EQ  ZFCIVRN
                            AND   ZFCIVSQ EQ  IT_ZSCIVIT-ZFCIVSQ.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSCIVIT TO ZTCIVIT.
               MOVE : ZFCIVRN                TO ZTCIVIT-ZFCIVRN,
                      ZTCIVHD-ZFPRPYN        TO ZTCIVIT-ZFPRPYN,
                      SY-MANDT               TO ZTCIVIT-MANDT,
                      SY-UNAME               TO ZTCIVIT-ERNAM,
                      SY-DATUM               TO ZTCIVIT-CDAT,
                      SY-UNAME               TO ZTCIVIT-UNAM,
                      SY-DATUM               TO ZTCIVIT-UDAT.

               INSERT  ZTCIVIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
               CLEAR  *ZTCIVIT.
               call function 'ZIM_CHANGE_DOCUMENT_CIVIT'
                    exporting
                            upd_chngind    =    'I'
                            N_ZTCIVIT      =    ZTCIVIT
                            O_ZTCIVIT      =    *ZTCIVIT.
*----------------------------------------------------------------------
            ENDIF.
         ENDLOOP.
   ENDCASE.
ENDFUNCTION.
