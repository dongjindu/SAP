FUNCTION ZIM_CHANGE_DOCUMENT_RELEASE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(W_ZFREQNO) LIKE  ZTREQST-ZFREQNO
*"     REFERENCE(W_ZFAMDNO) LIKE  ZTREQST-ZFAMDNO
*"     REFERENCE(W_ZFRLST1) LIKE  ZTREQST-ZFRLST1 OPTIONAL
*"     REFERENCE(W_ZFRLST2) LIKE  ZTREQST-ZFRLST2 OPTIONAL
*"----------------------------------------------------------------------
   DATA : W_ZFOPNNO     LIKE    ZTREQST-ZFOPNNO,
          W_ZFOPNNO_TMP LIKE    ZTREQST-ZFOPNNO,
          W_EBELN       LIKE    ZTREQHD-EBELN,
          W_SEQ(5)      TYPE    N,
          W_SEQ_TMP(5)  TYPE    C.
*   DATA : W_ZZBUSTYPE   LIKE    EKKO-ZZBUSTYPE,
    DATA : W_ZZBUSTYPE  TYPE    C,
          W_TYPE(2)     TYPE    C.

   SELECT SINGLE * FROM ZTREQHD
                   WHERE ZFREQNO EQ W_ZFREQNO.

   SELECT SINGLE * INTO O_ZTREQST FROM ZTREQST
                   WHERE ZFREQNO EQ W_ZFREQNO
                   AND   ZFAMDNO EQ W_ZFAMDNO.

   SELECT SINGLE * FROM  ZTIMIMGTX
                   WHERE BUKRS   EQ ZTREQHD-BUKRS.

   N_ZTREQST = O_ZTREQST.

   IF NOT W_ZFRLST1 IS INITIAL.
      MOVE : W_ZFRLST1   TO    N_ZTREQST-ZFRLST1,
             SY-DATUM    TO    N_ZTREQST-ZFRLDT1,
             SY-UNAME    TO    N_ZTREQST-ZFRLNM1,
             SY-DATUM    TO    N_ZTREQST-UDAT,
             SY-UNAME    TO    N_ZTREQST-UNAM.
   ENDIF.

   IF NOT W_ZFRLST2 IS INITIAL.
      MOVE : W_ZFRLST2   TO    N_ZTREQST-ZFRLST2,
             SY-DATUM    TO    N_ZTREQST-ZFRLDT2,
             SY-UNAME    TO    N_ZTREQST-ZFRLNM2,
             SY-DATUM    TO    N_ZTREQST-UDAT,
             SY-UNAME    TO    N_ZTREQST-UNAM.
* 개설 릴리즈시 D/A, D/P, T/T 내부번호 자동 채번
      IF N_ZTREQST-ZFREQTY EQ 'DA' OR
         N_ZTREQST-ZFREQTY EQ 'DP' OR
         N_ZTREQST-ZFREQTY EQ 'TT'.

         IF N_ZTREQST-ZFOPNNO IS INITIAL AND W_ZFRLST2 EQ 'R'.
            CLEAR : W_ZFOPNNO_TMP.
*>>>>>
*            SELECT SINGLE ZZBUSTYPE
*                   INTO  W_ZZBUSTYPE
*                   FROM  EKKO
*                   WHERE EBELN EQ ( SELECT EBELN FROM ZTREQHD
*                                       WHERE ZFREQNO EQ W_ZFREQNO ).
            IF N_ZTREQST-ZFREQTY EQ 'TT'.
               CONCATENATE 'T' ZTREQHD-ZFBACD INTO W_TYPE.
            ELSE.
               W_TYPE = N_ZTREQST-ZFREQTY.
            ENDIF.

            W_ZFOPNNO_TMP = SY-DATUM+2(4).
            CONCATENATE W_TYPE W_ZFOPNNO_TMP W_ZZBUSTYPE '%'
                        INTO  W_ZFOPNNO_TMP.
            SELECT MAX( ZFOPNNO ) INTO W_ZFOPNNO FROM ZTREQST
                                  WHERE ZFOPNNO LIKE W_ZFOPNNO_TMP.

            IF W_ZFOPNNO IS INITIAL.
               W_ZFOPNNO_TMP+6(5) = '00001'.
            ELSE.
               W_SEQ = W_ZFOPNNO+6(5).
               W_SEQ = W_SEQ + 1.
               W_SEQ_TMP = W_SEQ.
               CONCATENATE W_ZFOPNNO(6) W_SEQ_TMP
                      INTO W_ZFOPNNO_TMP.
            ENDIF.
            N_ZTREQST-ZFOPNNO = W_ZFOPNNO_TMP.
         ENDIF.

* 2002.09.09 NHJ 주석.
*         IF N_ZTREQST-ZFREQTY NE 'TT' OR ZTIMIMGTX-PAYORD NE 'X'.
          IF N_ZTREQST-ZFREQTY EQ 'TT' OR N_ZTREQST-ZFREQTY EQ 'DA' OR
             N_ZTREQST-ZFREQTY EQ 'DP'.
           IF W_ZFRLST2 EQ 'R'.
              N_ZTREQST-ZFOPNDT = N_ZTREQST-ZFAPPDT.
              N_ZTREQST-ZFDOCST = 'O'.
           ENDIF.
         ENDIF.

* HEADER TABLE UPDATE
         SELECT SINGLE * FROM ZTREQHD WHERE ZFREQNO EQ W_ZFREQNO.

         MOVE : N_ZTREQST-ZFOPNNO TO ZTREQHD-ZFOPNNO,
                ZTREQHD-ZFREQED   TO ZTREQHD-ZFLASTED,
                ZTREQHD-ZFREQSD   TO ZTREQHD-ZFLASTSD.

         UPDATE ZTREQHD.
      ENDIF.
   ENDIF.

   MOVE-CORRESPONDING N_ZTREQST    TO    ZTREQST.
   UPDATE ZTREQST.

* CHANGE DOCUMENT
  CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_STATUS'
     EXPORTING
        W_ZFREQNO      =     W_ZFREQNO
        W_ZFAMDNO      =     W_ZFAMDNO
        N_ZTREQST      =     N_ZTREQST
        O_ZTREQST      =     O_ZTREQST.


ENDFUNCTION.
