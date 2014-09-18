FUNCTION ZIM_ZTBLINOU_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_OK_CODE)
*"     VALUE(ZFBLNO) LIKE  ZTBLINOU-ZFBLNO
*"     VALUE(ZFBTSEQ) LIKE  ZTBLINOU-ZFBTSEQ
*"     VALUE(ZFSTATUS)
*"     VALUE(N_ZTBLINOU) LIKE  ZTBLINOU STRUCTURE  ZTBLINOU
*"     VALUE(O_ZTBLINOU) LIKE  ZTBLINOU STRUCTURE  ZTBLINOU OPTIONAL
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
* 삭제 지시자 검증
   IF W_OK_CODE EQ 'DELE'.   ZFSTATUS = 'X'.  ENDIF.

   MOVE : ZFBLNO   TO    ZTBLINOU-ZFBLNO,     "
          ZFBTSEQ  TO    ZTBLINOU-ZFBTSEQ,
          SY-DATUM TO    ZTBLINOU-UDAT,
          SY-UNAME TO    ZTBLINOU-UNAM.

   MOVE-CORRESPONDING N_ZTBLINOU  TO    ZTBLINOU.

* MODIFY
   CASE ZFSTATUS.
      WHEN 'C'.               " 생성
         INSERT ZTBLINOU.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         call function 'ZIM_CHANGE_DOCUMENT_BLINOU'
              exporting
                      upd_chngind    =    'I'
                      N_ZTBLINOU     =    N_ZTBLINOU
                      O_ZTBLINOU     =    O_ZTBLINOU.
*----------------------------------------------------------------------

      WHEN 'U'.               " 변경
         UPDATE ZTBLINOU.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         call function 'ZIM_CHANGE_DOCUMENT_BLINOU'
              exporting
                      upd_chngind    =    'U'
                      N_ZTBLINOU     =    N_ZTBLINOU
                      O_ZTBLINOU     =    O_ZTBLINOU.
*----------------------------------------------------------------------
      WHEN 'X'.               " 삭제
         DELETE ZTBLINOU.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         call function 'ZIM_CHANGE_DOCUMENT_BLINOU'
              exporting
                      upd_chngind    =    'D'
                      N_ZTBLINOU     =    N_ZTBLINOU
                      O_ZTBLINOU     =    O_ZTBLINOU.
*----------------------------------------------------------------------

      WHEN OTHERS.
  ENDCASE.
*>>>>>> SAP MODULE 이상으로 CHANGE DOCUMENT는 반영하지 못함......

ENDFUNCTION.
