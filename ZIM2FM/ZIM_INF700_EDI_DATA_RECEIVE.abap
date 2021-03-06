FUNCTION ZIM_INF700_EDI_DATA_RECEIVE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"  EXCEPTIONS
*"      UPDATE_ERROR
*"      NOT_FOUND
*"      NO_REFERENCE
*"      DOCUMENT_LOCKED
*"      DATE_ERROR
*"----------------------------------------------------------------------
DATA : C_ZFDDFDA1(3),
       WL_VIA(1)        TYPE C.

  SELECT SINGLE * FROM ZTDHF1 WHERE ZFDHENO EQ W_ZFDHENO.
  IF SY-SUBRC NE 0.
     RAISE   NOT_FOUND.
  ENDIF.

  REFRESH: IT_SAITIN_A, IT_SAITIN_S.
* 전체 SELECT
  SELECT *  FROM ZTDDF1
            APPENDING CORRESPONDING FIELDS OF TABLE IT_SAITIN_A
            WHERE ZFDDENO = W_ZFDHENO.
* 시작점 SELECT
  SELECT *  APPENDING CORRESPONDING FIELDS OF TABLE IT_SAITIN_S
            FROM ZTDDF1
            WHERE ZFDDENO EQ    W_ZFDHENO
            AND ( ZFDDFDA LIKE  '{14%'
            OR    ZFDDFDA LIKE  '{15%' ).
* HEADER SELECT.
  CLEAR : ZTDHF1.
  SELECT SINGLE * FROM ZTDHF1 WHERE ZFDHENO EQ W_ZFDHENO.
  IF SY-SUBRC NE 0.   RAISE   NOT_FOUND.   ENDIF.

* 수입의뢰 문서 SELECT
  SELECT * FROM ZTREQST UP TO 1 ROWS
                        WHERE ( ZFDOCNO  EQ ZTDHF1-ZFDHREF
                        OR      ZFDOCNOR EQ W_ZFDHENO )
                        AND   ZFAMDNO EQ '00000'
                        ORDER BY ZFREQNO DESCENDING.
     EXIT.
  ENDSELECT.
  IF SY-SUBRC NE 0.   RAISE   NO_REFERENCE.   ENDIF.

* LOCK CHECK
  CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
       EXPORTING
             ZFREQNO                =     ZTREQST-ZFREQNO
             ZFAMDNO                =     ZTREQST-ZFAMDNO
       EXCEPTIONS
             OTHERS        = 1.

  IF SY-SUBRC <> 0.
       MESSAGE E510 WITH SY-MSGV1 'Import Document'
                         ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
               RAISING DOCUMENT_LOCKED.
  ENDIF.
*-----------------------------------------------------------------------
* 변경이력을 위해
  O_ZTREQST = ZTREQST.
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
* DATA MOVE
*-----------------------------------------------------------------------
  LOOP AT IT_SAITIN_S.
    CASE IT_SAITIN_S-ZFDDFDA.
* L/C No.
      WHEN '{14'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        IF IT_SAITIN_A-ZFDDFDA EQ 'AAC'.
           Z_ZFDDSEQ = Z_ZFDDSEQ + 1.
           READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
           ZTREQST-ZFOPNNO = IT_SAITIN_A-ZFDDFDA.
        ENDIF.
* 개설일자
      WHEN '{15'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        IF IT_SAITIN_A-ZFDDFDA EQ '182'.
           Z_ZFDDSEQ = Z_ZFDDSEQ + 1.
           READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
*-----------------------------------------------------------------------
* DATE CONVERT
*-----------------------------------------------------------------------
           CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                EXPORTING
                     DATE_EXTERNAL = IT_SAITIN_A-ZFDDFDA
                IMPORTING
                     DATE_INTERNAL = ZTREQST-ZFOPNDT.

           IF SY-SUBRC <> 0.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                     RAISING DATE_ERROR.
           ENDIF.
        ENDIF.

    ENDCASE.                           " *  IT_SAITIN_S-ZFDDFDA
  ENDLOOP.                             " ** IT_SAITIN_S

* 상태 변경
  MOVE : SY-UNAME    TO    ZTREQST-UNAM,
         SY-DATUM    TO    ZTREQST-UDAT,
         W_ZFDHENO   TO    ZTREQST-ZFDOCNOR,
         'O'         TO    ZTREQST-ZFDOCST,
         'R'         TO    ZTREQST-ZFEDIST.

  UPDATE ZTREQST.
  IF SY-SUBRC NE  0.   RAISE    UPDATE_ERROR.   ENDIF.

* CHANGE DOCUMENT
  CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_STATUS'
     EXPORTING
        W_ZFREQNO      =     ZTREQST-ZFREQNO
        W_ZFAMDNO      =     ZTREQST-ZFAMDNO
        N_ZTREQST      =     ZTREQST
        O_ZTREQST      =     O_ZTREQST.

  CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
             ZFREQNO                =     ZTREQST-ZFREQNO
             ZFAMDNO                =     ZTREQST-ZFAMDNO.

  ZTDHF1-ZFDHAPP = 'Y'.
  UPDATE  ZTDHF1.

  MESSAGE  S124  WITH  ZTREQST-ZFREQNO ZTREQST-ZFAMDNO '저장'.

ENDFUNCTION.
