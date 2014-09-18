FUNCTION ZIM_VATBIL_EDI_DATA_RECEIVE.
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

* 세금계산서 SELECT
  SELECT * FROM ZTVT    UP TO 1 ROWS
                        WHERE   ZFDOCNO  EQ ZTDHF1-ZFDHREF
                        OR      ZDREINO  EQ W_ZFDHENO.
     EXIT.
  ENDSELECT.
  IF SY-SUBRC NE 0.   RAISE   NO_REFERENCE.   ENDIF.

*-----------------------------------------------------------------------
* LOCK CHECK

* 상태 변경
  MOVE : SY-UNAME    TO    ZTVT-UNAM,
         SY-DATUM    TO    ZTVT-UDAT,
         W_ZFDHENO   TO    ZTVT-ZDREINO,
         'E'         TO    ZTVT-ZFVTRYN,
         'R'         TO    ZTVT-ZFEDIST.

  UPDATE ZTVT.
  IF SY-SUBRC NE  0.   RAISE    UPDATE_ERROR.   ENDIF.

  ZTDHF1-ZFDHAPP = 'Y'.
  UPDATE  ZTDHF1.

  MESSAGE  S834.

ENDFUNCTION.
