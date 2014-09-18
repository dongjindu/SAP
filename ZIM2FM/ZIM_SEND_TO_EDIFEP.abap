FUNCTION ZIM_SEND_TO_EDIFEP.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IT_ZTDHF1 STRUCTURE  ZTDHF1
*"      IT_ERR_ZTDHF1 STRUCTURE  ZTDHF1
*"----------------------------------------------------------------------
DATA : L_RETURN_CODE LIKE  ZTDHF1-ZFDHTRA.


   REFRESH : IT_ERR_ZTDHF1.
*-----------------------------------------------------------------------
* 표준 EDI HEADER TABLE LOOP
*-----------------------------------------------------------------------
   LOOP AT IT_ZTDHF1.
     W_TABIX = SY-TABIX.
* SELECT
     REFRESH : IT_ZTDDF1.
     SELECT *  FROM ZTDDF1
               APPENDING CORRESPONDING FIELDS OF TABLE IT_ZTDDF1
               WHERE ZFDDENO = IT_ZTDHF1-ZFDHENO.
* SORT
     SORT IT_ZTDDF1 BY ZFDDSEQ ASCENDING.

*-----------------------------------------------------------------------
* CALL RFC Function
*-----------------------------------------------------------------------
*     CALL FUNCTION 'SEND_TO_EDIFEP_IMPT'
*          DESTINATION 'EDI_FEP_IMPT'
*          EXPORTING
*                  ZFDOC           =      IT_ZTDHF1-ZFDHDOC
*                  ZFENO           =      IT_ZTDHF1-ZFDHENO
*                  ZFSRG           =      IT_ZTDHF1-ZFDHSRG
*                  ZFSRO           =      IT_ZTDHF1-ZFDHSRO
*                  ZFRSO           =      IT_ZTDHF1-ZFDHRSO
*          IMPORTING
**                  RETURN_RFC_SVR  =      IT_ZTDHF1-ZFDHTRA
*                  RETURN_RFC_SVR  =      L_RETURN_CODE
*          TABLES
*                  SEPT_ZTEPDF1    =      IT_ZTDDF1.
*-----------------------------------------------------------------------

* SUCCESS 시
* ===> 변환 결과는 나중에 들어옴....
*    IF L_RETURN_CODE EQ 'T_OK'.
     IF L_RETURN_CODE EQ '0000' AND SY-SUBRC EQ 0.
* 송신시간
        MOVE : SY-DATUM+2(6)      TO    IT_ZTDHF1-ZFDHJSD,
               SY-UZEIT           TO    IT_ZTDHF1-ZFDHJSH.
     ELSE.
*>>>>>>>>>>> ERROR 발생시
        MOVE-CORRESPONDING   IT_ZTDHF1     TO     IT_ERR_ZTDHF1.
        APPEND   IT_ERR_ZTDHF1.
     ENDIF.
     MODIFY IT_ZTDHF1   INDEX   W_TABIX.

* DATABASE UPDATE
*     SELECT SINGLE * FROM ZTDHF1 WHERE ZFDHENO = IT_ZTDHF1-ZFDHENO.
     MOVE-CORRESPONDING   IT_ZTDHF1     TO   ZTDHF1.
     UPDATE ZTDHF1.
*-----------------------------------------------------------------------
* DOCUMENT EDI STATUS CHANGE
*-----------------------------------------------------------------------
     CASE IT_ZTDHF1-ZFDHDOC.
        WHEN 'APP700' OR 'APP707' OR 'LOCAPP' OR 'LOCAMR' OR 'APPPUR'.
           SELECT SINGLE * FROM ZTREQST
                           WHERE ZFDOCNO EQ IT_ZTDHF1-ZFDHENO.
           IF SY-SUBRC EQ 0.
              CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BEFORE'
                 EXPORTING
                      W_ZFREQNO     =     ZTREQST-ZFREQNO
                      W_ZFAMDNO     =     ZTREQST-ZFAMDNO
                      W_ZFDOCST     =     'R'
                      W_ZFEDIST     =     'S'.
           ENDIF.
        WHEN 'APPCIP' OR 'APPEND'.    " 적하보험
           SELECT SINGLE * FROM ZTINS
                           WHERE ZFDOCNO EQ IT_ZTDHF1-ZFDHENO.
           IF SY-SUBRC EQ 0.
              CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_INS_BEFORE'
                 EXPORTING
                      W_ZFREQNO     =     ZTINS-ZFREQNO
                      W_ZFINSEQ     =     ZTINS-ZFINSEQ
                      W_ZFAMDNO     =     ZTINS-ZFAMDNO
                      W_ZFDOCST     =     'R'
                      W_ZFEDIST     =     'S'.
           ENDIF.
        WHEN 'DOMOFR'.
           SELECT SINGLE * FROM ZTOFF
                           WHERE ZFDOCNO EQ IT_ZTDHF1-ZFDHENO.
           IF SY-SUBRC EQ 0.
              CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_OFF_STATUS'
                   EXPORTING
                      W_ZFREQNO     =    ZTOFF-ZFREQNO
                      W_ZFDOCST     =    'R'
                      W_ZFEDIST     =    'S'.
           ENDIF.
        WHEN 'APPLOG'.
           SELECT SINGLE * FROM ZTLG
                           WHERE ZFDOCNO EQ IT_ZTDHF1-ZFDHENO.
           IF SY-SUBRC EQ 0.
              CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_LG_STATUS'
                   EXPORTING
                      W_ZFBLNO      =    ZTLG-ZFBLNO
                      W_ZFLGSEQ     =    ZTLG-ZFLGSEQ
                      W_ZFDOCST     =    'R'
                      W_ZFEDIST     =    'S'.
           ENDIF.
        WHEN OTHERS.
     ENDCASE.
*-----------------------------------------------------------------------

   ENDLOOP.
*  SYSTEM COMMIT...
   COMMIT WORK.

ENDFUNCTION.
