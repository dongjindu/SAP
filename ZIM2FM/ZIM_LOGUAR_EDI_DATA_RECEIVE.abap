FUNCTION ZIM_LOGUAR_EDI_DATA_RECEIVE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"       EXCEPTIONS
*"              UPDATE_ERROR
*"              NOT_FOUND
*"              NO_REFERENCE
*"              DOCUMENT_LOCKED
*"----------------------------------------------------------------------
DATA : C_ZFDDFDA1(3),
       WL_VIA(1)        TYPE C,
       WL_NOREF         TYPE C    VALUE   'Y'.

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
            AND   ZFDDFDA LIKE  '{13%'.

* HEADER SELECT.
  CLEAR : ZTDHF1.
  SELECT SINGLE * FROM ZTDHF1 WHERE ZFDHENO EQ W_ZFDHENO.
  IF SY-SUBRC NE 0.
     RAISE   NOT_FOUND.
  ENDIF.

*-----------------------------------------------------------------------
* DATA MOVE
*-----------------------------------------------------------------------
  LOOP AT IT_SAITIN_S.
    CASE IT_SAITIN_S-ZFDDFDA.
* L/C No.
      WHEN '{13'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        CASE IT_SAITIN_A-ZFDDFDA.
           WHEN '2AW'.               " 수입화물선취보증(인도승락)서 번호
              Z_ZFDDSEQ = Z_ZFDDSEQ + 1.
              READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
              W_ZFOPNNO = IT_SAITIN_A-ZFDDFDA.
           WHEN 'AAC'.               " 신용장 번호
              WL_NOREF  = 'N'.
              Z_ZFDDSEQ = Z_ZFDDSEQ + 1.
              READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
* 수입의뢰 문서 SELECT
              SELECT * FROM ZTLG UP TO 1 ROWS
                        WHERE ZFDCNO EQ IT_SAITIN_A-ZFDDFDA
                        ORDER BY ZFBLNO  DESCENDING
                                 ZFLGSEQ DESCENDING.
                 EXIT.
              ENDSELECT.
              IF SY-SUBRC NE 0.
                 RAISE   NO_REFERENCE.
              ENDIF.

* LOCK CHECK
              CALL FUNCTION 'ENQUEUE_EZ_IM_ZTLGDOC'
                  EXPORTING
                     ZFBLNO                 =     ZTLG-ZFBLNO
                     ZFLGSEQ                =     ZTLG-ZFLGSEQ
                   EXCEPTIONS
                      OTHERS                 =     1.

              IF SY-SUBRC <> 0.
                 MESSAGE E510 WITH SY-MSGV1 'L/G Document'
                         ZTLG-ZFBLNO ZTLG-ZFLGSEQ
                         RAISING DOCUMENT_LOCKED.
              ENDIF.
        ENDCASE.
    ENDCASE.                           " *  IT_SAITIN_S-ZFDDFDA
  ENDLOOP.                             " ** IT_SAITIN_S

  IF WL_NOREF EQ 'Y'.
     RAISE   NO_REFERENCE.
  ENDIF.

* 상태 변경
  MOVE : SY-UNAME    TO    ZTLG-UNAM,
         SY-DATUM    TO    ZTLG-UDAT,
         'O'         TO    ZTLG-ZFDOCST,
         'R'         TO    ZTLG-ZFEDIST,
         W_ZFOPNNO   TO    ZTLG-ZFLGINO,
         SY-DATUM    TO    ZTLG-ZFLGIDT,
         W_ZFDHENO   TO    ZTLG-ZFDOCNO.

  UPDATE ZTLG.

  IF SY-SUBRC NE  0.
     RAISE    UPDATE_ERROR.
  ELSE.
     MESSAGE  S124  WITH  ZTLG-ZFBLNO ZTLG-ZFLGSEQ '저장'.
  ENDIF.

  CALL FUNCTION 'DEQUEUE_EZ_IM_ZTLGDOC'
         EXPORTING
             ZFBLNO                 =     ZTLG-ZFBLNO
             ZFLGSEQ                =     ZTLG-ZFLGSEQ.

  ZTDHF1-ZFDHAPP = 'Y'.
  UPDATE  ZTDHF1.

ENDFUNCTION.
