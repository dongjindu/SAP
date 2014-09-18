FUNCTION ZIM_EDI_NUMBER_GET_NEXT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFCDDOC) LIKE  ZTCDF1-ZFCDDOC
*"     VALUE(W_ZFDHSRO) LIKE  ZTDHF1-ZFDHSRO
*"     VALUE(W_ZFDHREF) LIKE  ZTDHF1-ZFDHREF OPTIONAL
*"     VALUE(W_ZFEDIID) LIKE  ZTIMIMG03-ZFEDIID DEFAULT SPACE
*"     VALUE(W_BUKRS) LIKE  ZTIMIMGTX-BUKRS DEFAULT 'C100'
*"     VALUE(W_LOG_ID) DEFAULT 'Y'
*"  CHANGING
*"     VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"  EXCEPTIONS
*"      DB_ERROR
*"      NO_TYPE
*"      NOT_FOUND
*"----------------------------------------------------------------------
DATA : W_CDYMM   LIKE   ZTCDF1-ZFCDYMM.
DATA : W_CDSEQ   LIKE   ZTCDF1-ZFCDSEQ.
DATA : W_CDSEQT(6).

  SELECT SINGLE * FROM  ZTIMIMGTX
                  WHERE BUKRS EQ W_BUKRS.
  IF SY-SUBRC NE 0.    MESSAGE E963.    ENDIF.

  MOVE : SY-DATUM+2(4)   TO    W_CDYMM.

  SELECT * FROM DD07T WHERE DOMNAME     EQ 'ZDDHDOC'
                      AND   DDLANGUAGE  EQ SY-LANGU
                      AND   AS4LOCAL    EQ 'A'
                      AND   DOMVALUE_L  EQ W_ZFCDDOC
                      ORDER BY AS4VERS DESCENDING.
     EXIT.
  ENDSELECT.

  IF SY-SUBRC NE 0.
     RAISE NO_TYPE.
  ENDIF.

  SET UPDATE TASK LOCAL.

  IF W_LOG_ID EQ 'Y'.
     IF NOT W_ZFDHENO IS INITIAL.
        DELETE FROM ZTDHF1 WHERE ZFDHENO EQ W_ZFDHENO.
     ENDIF.
  ENDIF.

  IF W_ZFDHENO IS INITIAL.
     CLEAR : ZTCDF1.

     SELECT SINGLE * FROM   ZTCDF1
                     WHERE  ZFCDDOC   EQ   W_ZFDHREF
                     AND    ZFCDYMM   EQ   W_CDYMM.

     IF SY-SUBRC EQ 0.
        ZTCDF1-ZFCDSEQ = ZTCDF1-ZFCDSEQ + 1.
        UPDATE ZTCDF1.
     ELSE.
        MOVE :  W_ZFDHREF     TO    ZTCDF1-ZFCDDOC,
                W_CDYMM       TO    ZTCDF1-ZFCDYMM,
                '000001'      TO    ZTCDF1-ZFCDSEQ.
        INSERT ZTCDF1.
     ENDIF.

     W_CDSEQ  = ZTCDF1-ZFCDSEQ.
     W_CDSEQT = ZTCDF1-ZFCDSEQ.

     IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        RAISE DB_ERROR.
     ENDIF.

     IF W_CDSEQ > 999999.
        CONCATENATE W_ZFDHREF '-' W_CDYMM W_CDSEQT INTO W_ZFDHENO.
     ELSE.
        CONCATENATE W_ZFDHREF '-' W_CDYMM '-' W_CDSEQT+1(5)
                                                       INTO W_ZFDHENO.
     ENDIF.
  ENDIF.

*>>LOG 반영여부....--->
  IF W_LOG_ID NE 'Y'.
     COMMIT WORK.
     EXIT.
  ENDIF.

* HEADER DATA
  CLEAR : ZTDHF1.
  MOVE : SY-MANDT          TO   ZTDHF1-MANDT,     ">CLIENT.
         W_ZFDHENO         TO   ZTDHF1-ZFDHENO,   " 문서관리번호
         W_ZFCDDOC         TO   ZTDHF1-ZFDHDOC,   " 전자문서명
         W_BUKRS           TO   ZTDHF1-BUKRS,     " 회사코드.
*         'S'               TO   ZTDHF1-ZFDHSRG,   " 송수신구분
         W_ZFDHSRO         TO   ZTDHF1-ZFDHSRO,   " 거래상대방 ID
         W_ZFDHREF         TO   ZTDHF1-ZFDHREF,   " 발신인 부여 참조
         '000'             TO   ZTDHF1-ZFDHPRT,   " 문서출력 회수
         SY-DATUM          TO   ZTDHF1-ZFDHJSD,   " 송신일자
         SY-UZEIT          TO   ZTDHF1-ZFDHJSH.   " 송신시간
*        ZTIMIMGTX-ZFEDIID TO   ZTDHF1-ZFDHRSO.    " 회사 ID

*>> 송/수신 구분.
  CASE W_ZFCDDOC.
     WHEN 'APP700' OR 'APP707' OR 'LOCAPP' OR 'LOCAMR' OR
          'PAYORD' OR 'APPPUR' OR 'IMPREQ' OR 'LOCRCT' OR
          'VATBIL' OR 'DOMOFR' OR 'APPLOG'.
        ZTDHF1-ZFDHSRG = 'S'.
        ZTDHF1-ZFDHAPP = 'N'.

     WHEN 'INF700' OR 'INF707' OR 'LOCADV' OR 'LOCAMA' OR
          'DEBADV' OR 'PURLIC' OR 'IMPRES' OR 'LOGUAR' OR
          'DOANTC' OR 'LDANTC' OR 'DISCHG' OR 'FINBIL'.
        ZTDHF1-ZFDHSRG = 'R'.
        ZTDHF1-ZFDHAPP = 'N'.
  ENDCASE.

  INSERT ZTDHF1.

  IF SY-SUBRC NE 0.
     ROLLBACK WORK.
     RAISE DB_ERROR.
  ENDIF.

  COMMIT WORK.

ENDFUNCTION.
