FUNCTION ZIM_PAYORD_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFREQNO) LIKE  ZTREQHD-ZFREQNO
*"     VALUE(ZFAMDNO) LIKE  ZTREQST-ZFAMDNO
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTREQHD) LIKE  ZTREQHD STRUCTURE  ZTREQHD
*"     VALUE(W_ZTREQHD_OLD) LIKE  ZTREQHD STRUCTURE  ZTREQHD
*"     VALUE(W_ZTREQST) LIKE  ZTREQST STRUCTURE  ZTREQST
*"     VALUE(W_ZTREQST_OLD) LIKE  ZTREQST STRUCTURE  ZTREQST
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSREQIT_OLD STRUCTURE  ZSREQIT
*"      IT_ZSREQIT STRUCTURE  ZSREQIT
*"      IT_ZTREQORJ_OLD STRUCTURE  ZSMLCSG7O
*"      IT_ZTREQORJ STRUCTURE  ZSMLCSG7O
*"      IT_ZSREQIL_OLD STRUCTURE  ZSREQIL
*"      IT_ZSREQIL STRUCTURE  ZSREQIL
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
DATA : L_ZTREQST LIKE ZTREQST.

   if  w_ok_code eq 'DELE'.   zfstatus = 'X'.   endif.
*   MOVE-CORRESPONDING : W_ZTTTHD   TO   ZTTTHD.

   move : zfreqno  to    ztreqst-zfreqno,
          zfamdno  to    ztreqst-zfamdno,
*          ZFREQNO  TO    ZTTTHD-ZFREQNO,
*         ZFAMDNO  TO    ZTTTHD-ZFAMDNO,
          SY-MANDT TO    ZTTTHD-MANDT.

   IF ZFSTATUS NE 'X' AND W_ZTREQST-ZFDOCST EQ 'O'.
      SELECT SINGLE * INTO L_ZTREQST
             FROM  ZTREQST
             WHERE ZFREQNO   EQ  ZFREQNO
             AND   ZFAMDNO   EQ  ZFAMDNO.
   ENDIF.

*-----------------------------------------------------------------------
* 수입의뢰 HEADER 정의
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_REQUEST_DOC_MODIFY'
       EXPORTING
             ZFREQNO             =   ZFREQNO
             ZFAMDNO             =   ZFAMDNO
             ZFSTATUS            =   ZFSTATUS
             W_ZTREQHD           =   W_ZTREQHD
             W_ZTREQHD_OLD       =   W_ZTREQHD_OLD
             W_ZTREQST           =   W_ZTREQST
             W_OK_CODE           =   W_OK_CODE
       TABLES
             IT_ZSREQIT          =   IT_ZSREQIT
             IT_ZSREQIT_OLD      =   IT_ZSREQIT_OLD
             IT_ZSREQIL          =   IT_ZSREQIL
             IT_ZSREQIL_OLD      =   IT_ZSREQIL_OLD
             IT_ZTREQORJ         =   IT_ZTREQORJ
             IT_ZTREQORJ_OLD     =   IT_ZTREQORJ_OLD
       EXCEPTIONS
              ERROR_UPDATE.

   IF SY-SUBRC NE  0.   RAISE ERROR_UPDATE.   ENDIF.

*-----------------------------------------------------------------------
* 기타 수입입의뢰 문서
*-----------------------------------------------------------------------
*   case zfstatus.
*      when 'C'.               " 생성
*         INSERT ZTTTHD.
*         IF SY-SUBRC NE  0.   RAISE ERROR_UPDATE.   ENDIF.
*
*         LOOP AT IT_ZSTTSG5.
*            CLEAR : ZTTTSG5.
*            MOVE-CORRESPONDING   IT_ZSTTSG5    TO    ZTTTSG5.
*            MOVE : SY-MANDT                    TO    ZTTTSG5-MANDT,
*                   ZFREQNO                     TO    ZTTTSG5-ZFREQNO.
**                  ZFAMDNO                     TO    ZTTTSG5-ZFAMDNO.
*            INSERT    ZTTTSG5.
*            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
*         ENDLOOP.
** change document
*         call function 'ZIM_CHANGE_DOCUMENT_TT'
*              exporting
*                         upd_chngind    =    'I'
*                         N_ZTTThd       =    W_ZTTTHD
*                         O_ZTTTHD       =    W_ZTTTHD_OLD
*                         n_ztreqhd      =    w_ztreqhd
*                         o_ztreqhd      =    w_ztreqhd_old
*                         n_ztreqst      =    w_ztreqst
*                         O_ztreqst      =    w_ztreqst_OLD.
*
*      when 'X'.               " 삭제
*         DELETE FROM ZTTTHD  WHERE ZFREQNO      =    ZFREQNO.
**                            AND   ZFAMDNO      =    ZFAMDNO.
**         IF SY-SUBRC NE  0.   RAISE ERROR_UPDATE.   ENDIF.
*
*         DELETE FROM ZTTTSG5 WHERE ZFREQNO      =    ZFREQNO.
**                             AND   ZFAMDNO      =    ZFAMDNO.
*
** change document
*         call function 'ZIM_CHANGE_DOCUMENT_TT'
*              exporting
*                         upd_chngind    =    'D'
*                         N_ZTTThd       =    W_ZTTTHD
*                         O_ZTTTHD       =    W_ZTTTHD_OLD
*                         n_ztreqhd      =    w_ztreqhd
*                         o_ztreqhd      =    w_ztreqhd_old
*                         n_ztreqst      =    w_ztreqst
*                         O_ztreqst      =    w_ztreqst_OLD.
*
*      when others.            " 변경
*         UPDATE ZTTTHD.
*         IF SY-SUBRC NE  0.   RAISE ERROR_UPDATE.   ENDIF.
*         SELECT * FROM ZTTTSG5   WHERE ZFREQNO  EQ  ZFREQNO.
**                                 AND   ZFAMDNO  EQ  ZFAMDNO.
*            READ TABLE IT_ZSTTSG5
*                 WITH KEY ZFLSG5 = ZTTTSG5-ZFLSG5 BINARY SEARCH.
*            IF SY-SUBRC EQ 0.
*               MOVE-CORRESPONDING IT_ZSTTSG5   TO ZTTTSG5.
*               MOVE : ZFREQNO                  TO ZTTTSG5-ZFREQNO,
**                     ZFAMDNO                  TO ZTTTSG5-ZFAMDNO,
*                      SY-MANDT                 TO ZTLLCOF-MANDT.
*               UPDATE ZTTTSG5.
*               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
*            ELSE.
*               DELETE ZTTTSG5.
*               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
*            ENDIF.
*         ENDSELECT.
*
*         LOOP AT IT_ZSTTSG5.
*            SELECT SINGLE * FROM  ZTTTSG5
*                            WHERE ZFREQNO  EQ  ZFREQNO
**                           AND   ZFAMDNO  EQ  ZFAMDNO
*                            AND   ZFLSG5   EQ  IT_ZSTTSG5-ZFLSG5.
*            IF SY-SUBRC NE 0.
*               MOVE-CORRESPONDING IT_ZSTTSG5   TO ZTTTSG5.
*               MOVE : ZFREQNO                  TO ZTTTSG5-ZFREQNO,
**                      ZFAMDNO                  TO ZTTTSG5-ZFAMDNO,
*                      SY-MANDT                 TO ZTTTSG5-MANDT.
*               INSERT  ZTTTSG5.
*               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
*            ENDIF.
*         ENDLOOP.
*
** change document
*         call function 'ZIM_CHANGE_DOCUMENT_TT'
*              exporting
*                         upd_chngind    =    'U'
*                         N_ZTTTHD       =    w_ztTTHD
*                         O_ZTTTHD       =    w_ztTTHD_OLD
*                         n_ztreqhd      =    w_ztreqhd
*                         O_ztreqhd      =    w_ztreqhd_old
*                         n_ztreqst      =    w_ztreqst
*                         O_ztreqst      =    w_ztreqst_OLD.
*
*   endcase.

   IF ZFSTATUS          NE 'X' AND
      W_ZTREQST-ZFDOCST EQ 'O' AND
      W_ZTREQHD-ZFBACD  EQ 'B'.

      SELECT COUNT( * ) INTO W_COUNT
             FROM ZTCIVIT
             WHERE    ZFREQNO  EQ  ZFREQNO.

      IF W_COUNT EQ 0.
         CALL FUNCTION 'ZIM_BDC_CALL_TRANSACTION_ZIM35'
              EXPORTING
                 W_ZFREQNO          =  ZFREQNO
                 W_ZFAMDNO          =  ZFAMDNO
              TABLES
                 RETURN             =     RETURN
              EXCEPTIONS
                    REQ_ERROR       =     4.

         IF SY-SUBRC EQ 0.
            COMMIT WORK.
         ELSE.
            ROLLBACK WORK.
            READ TABLE RETURN WITH KEY TYPE = 'E'.
            IF SY-CALLD EQ 'X'.
               RETURN-TYPE = 'S'.
            ENDIF.
            MESSAGE ID     RETURN-ID
                    TYPE   RETURN-TYPE
                    NUMBER RETURN-NUMBER
                    WITH   RETURN-MESSAGE_V1
                           RETURN-MESSAGE_V2
                           RETURN-MESSAGE_V3
                           RETURN-MESSAGE_V4.
            IF SY-CALLD EQ 'X'.
               EXIT.
            ENDIF.

         ENDIF.
      ENDIF.
   ENDIF.

ENDFUNCTION.
