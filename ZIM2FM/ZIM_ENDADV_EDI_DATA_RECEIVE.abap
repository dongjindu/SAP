FUNCTION ZIM_ENDADV_EDI_DATA_RECEIVE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"  EXCEPTIONS
*"      UPDATE_ERROR
*"      NOT_FOUND
*"      NO_REFERENCE
*"      DOCUMENT_LOCKED
*"----------------------------------------------------------------------
DATA : C_ZFDDFDA1(3),
       UPD_CHNGIND      LIKE  CDPOS-CHNGIND,
       O_ZTINS          LIKE  ZTINS,
       O_ZTINSRSP       LIKE  ZTINSRSP,
       WL_VIA(1)        TYPE C,
       WL_NOREF         TYPE C    VALUE   'Y'.

* HEADER SELECT.
  CLEAR : ZTDHF1.
  SELECT SINGLE * FROM ZTDHF1 WHERE ZFDHENO EQ W_ZFDHENO.
  IF SY-SUBRC NE 0.   RAISE   NOT_FOUND.   ENDIF.

  REFRESH: IT_SAITIN_A, IT_SAITIN_S.
* 전체 SELECT
  SELECT *  FROM ZTDDF1
            APPENDING CORRESPONDING FIELDS OF TABLE IT_SAITIN_A
            WHERE ZFDDENO = W_ZFDHENO.
* 시작점 SELECT
  SELECT *  APPENDING CORRESPONDING FIELDS OF TABLE IT_SAITIN_S
            FROM ZTDDF1
            WHERE ZFDDENO EQ    W_ZFDHENO
            AND   ZFDDFDA LIKE  '{%'.

*-----------------------------------------------------------------------
* 보험증권 문서 SELECT
*-----------------------------------------------------------------------
   SELECT * FROM ZTINS UP TO 1 ROWS
            WHERE ZFDOCNO  EQ ZTDHF1-ZFDHREF
            OR    ZFDOCNOR EQ W_ZFDHENO
            ORDER BY ZFREQNO DESCENDING
                     ZFAMDNO DESCENDING.
           EXIT.
  ENDSELECT.
  IF SY-SUBRC NE 0.   RAISE   NO_REFERENCE.   ENDIF.

* LOCK CHECK
  CALL FUNCTION 'ENQUEUE_EZ_IM_ZTINS'
       EXPORTING
          ZFREQNO                =     ZTINS-ZFREQNO
          ZFINSEQ                =     ZTINS-ZFINSEQ
          ZFAMDNO                =     ZTINS-ZFAMDNO
       EXCEPTIONS
          OTHERS                 =     1.

  IF SY-SUBRC <> 0.
     MESSAGE E510 WITH SY-MSGV1 'Insurance Document'
             ZTINS-ZFREQNO ZTINS-ZFINSEQ
             RAISING DOCUMENT_LOCKED.
  ENDIF.

  SELECT SINGLE * FROM ZTINSRSP
                  WHERE ZFREQNO  EQ  ZTINS-ZFREQNO
                  AND   ZFINSEQ  EQ  ZTINS-ZFINSEQ
                  AND   ZFAMDNO  EQ  ZTINS-ZFAMDNO.

*-----------------------------------------------------------------------
* 변경이력을 위해
*-----------------------------------------------------------------------
  CLEAR :    *ZTINSSG3.
  *ZTINS         = ZTINS.

  *ZTINSRSP      = ZTINSRSP.

  ZTINS-ZFDOCNOR =  W_ZFDHENO.

  UPD_CHNGIND = 'U'.
  IF ZTDHF1-ZFDHREF EQ W_ZFDHENO.
     O_ZTINS        = ZTINS.
     O_ZTINSRSP     = ZTINSRSP.
  ELSE.
     W_ZFAMDNO = ZTINS-ZFAMDNO - 1.

     SELECT SINGLE * INTO O_ZTINS FROM ZTINS
                  WHERE ZFREQNO  EQ  ZTINS-ZFREQNO
                  AND   ZFINSEQ  EQ  ZTINS-ZFINSEQ
                  AND   ZFAMDNO  EQ  W_ZFAMDNO.
     SELECT SINGLE * INTO O_ZTINSRSP FROM ZTINSRSP
                  WHERE ZFREQNO  EQ  ZTINS-ZFREQNO
                  AND   ZFINSEQ  EQ  ZTINS-ZFINSEQ
                  AND   ZFAMDNO  EQ  W_ZFAMDNO.
  ENDIF.
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
* DATA MOVE
*-----------------------------------------------------------------------
  LOOP AT IT_SAITIN_S.
    CASE IT_SAITIN_S-ZFDDFDA.
* 보험증권번호
      WHEN '{13'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 2.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        ZTINS-ZFINNO = IT_SAITIN_A-ZFDDFDA.
* 변경전 / 후 구분
      WHEN '{12' OR '{14'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        W_TEMP = IT_SAITIN_A-ZFDDFDA.
        IF IT_SAITIN_A-ZFDDFDA NE '36'.  CONTINUE.   ENDIF.
* 보험금액
      WHEN '{20'.
*>>>>>>>>>>>>>>변경후가 아니면 다음으로...
        IF W_TEMP NE '36'.  CONTINUE.   ENDIF.

        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        CASE IT_SAITIN_A-ZFDDFDA.
           WHEN '151'.
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFTAMI  = IT_SAITIN_A-ZFDDFDA.
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFTAMIC = IT_SAITIN_A-ZFDDFDA.
           WHEN '4AF'.              " Cargo
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFCAMI = IT_SAITIN_A-ZFDDFDA.
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFCAMIC = IT_SAITIN_A-ZFDDFDA.
           WHEN '55'.               " Duty
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFDAMI = IT_SAITIN_A-ZFDDFDA.
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFDAMIC = IT_SAITIN_A-ZFDDFDA.
           WHEN '4AA'.              " Total Premium
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFTPR   = IT_SAITIN_A-ZFDDFDA.
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFTPRC  = IT_SAITIN_A-ZFDDFDA.
           WHEN '4AB'.              " Cargo Premium
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFCPR   = IT_SAITIN_A-ZFDDFDA.
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFCPRC  = IT_SAITIN_A-ZFDDFDA.
           WHEN '4AC'.              " Duty  Premium
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFDPR   = IT_SAITIN_A-ZFDDFDA.
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFDPRC  = IT_SAITIN_A-ZFDDFDA.
           WHEN '4AD'.              " V/P   Premium
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFVPR   = IT_SAITIN_A-ZFDDFDA.
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFVPRC  = IT_SAITIN_A-ZFDDFDA.
           WHEN '4AE'.              " ITE   Premium
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFIPR   = IT_SAITIN_A-ZFDDFDA.
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINSRSP-ZFIPRC  = IT_SAITIN_A-ZFDDFDA.

        ENDCASE.                       " *  IT_SAITIN_A-ZFDDFDA
* 환율
      WHEN '{22'.
*>>>>>>>>>>>>>>변경후가 아니면 다음으로...
        IF W_TEMP NE '36'.  CONTINUE.   ENDIF.
        PERFORM READ_TABLE_5   TABLES   IT_SAITIN_A
                               CHANGING IT_SAITIN_S-ZFDDSEQ.
        ZTINSRSP-ZFEXRT  = IT_SAITIN_A-ZFDDFDA.
*발급일자
      WHEN '{15'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 2.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
*-----------------------------------------------------------------------
* DATE CONVERT
*-----------------------------------------------------------------------
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
             EXPORTING
                  DATE_EXTERNAL = IT_SAITIN_A-ZFDDFDA
             IMPORTING
                  DATE_INTERNAL = ZTINSRSP-ZFISDT.

        IF SY-SUBRC <> 0.
           MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
* Cargo Rate
      WHEN '{21'.
*>>>>>>>>>>>>>>변경후가 아니면 다음으로...
        IF W_TEMP NE '36'.  CONTINUE.   ENDIF.

        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        CASE IT_SAITIN_A-ZFDDFDA.
           WHEN '4AA'.              " Total Premium
           WHEN '4AB'.              " O.P Additional Premium
           WHEN '4AE'.              " Cargo Premium
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTINS-ZFINRT     = IT_SAITIN_A-ZFDDFDA.
           WHEN '4AF'.              " Duty  Premium
           WHEN '4AG'.              " V/P   Premium
           WHEN '4AH'.              " ITE   Premium
       ENDCASE.
    ENDCASE.                           " *  IT_SAITIN_S-ZFDDFDA
  ENDLOOP.                             " ** IT_SAITIN_S
* 저장을 위해 내부 자리로 변환
  PERFORM    SET_CURR_CONV_TO_INTERNAL CHANGING ZTINSRSP-ZFTAMI
                                                ZTINSRSP-ZFTAMIC.
  PERFORM    SET_CURR_CONV_TO_INTERNAL CHANGING ZTINSRSP-ZFCAMI
                                                ZTINSRSP-ZFCAMIC.
  PERFORM    SET_CURR_CONV_TO_INTERNAL CHANGING ZTINSRSP-ZFDAMI
                                                ZTINSRSP-ZFDAMIC.
  PERFORM    SET_CURR_CONV_TO_INTERNAL CHANGING ZTINSRSP-ZFTPR
                                                ZTINSRSP-ZFTPRC.
  PERFORM    SET_CURR_CONV_TO_INTERNAL CHANGING ZTINSRSP-ZFCPR
                                                ZTINSRSP-ZFCPRC.
  PERFORM    SET_CURR_CONV_TO_INTERNAL CHANGING ZTINSRSP-ZFDPR
                                                ZTINSRSP-ZFDPRC.
  PERFORM    SET_CURR_CONV_TO_INTERNAL CHANGING ZTINSRSP-ZFVPR
                                                ZTINSRSP-ZFVPRC.
  PERFORM    SET_CURR_CONV_TO_INTERNAL CHANGING ZTINSRSP-ZFIPR
                                                ZTINSRSP-ZFIPRC.
* 보험료
  MOVE : ZTINSRSP-ZFTPR   TO    ZTINS-ZFINAMT,
         ZTINSRSP-ZFTPRC  TO    ZTINS-ZFINAMTC,
         'KRW'            TO    ZTINS-ZFKRW.
* 원화
  ZTINS-ZFKRWAMT = ZTINS-ZFINAMT * ZTINSRSP-ZFEXRT.
  PERFORM    SET_CURR_CONV_TO_INTERNAL CHANGING ZTINS-ZFKRWAMT
                                                ZTINS-ZFKRW.

* 상태 변경
  MOVE : SY-UNAME    TO    ZTINS-UNAM,
         SY-DATUM    TO    ZTINS-UDAT,
         'O'         TO    ZTINS-ZFDOCST,
         'R'         TO    ZTINS-ZFEDIST.

  UPDATE ZTINS.
  IF SY-SUBRC NE  0.   RAISE    UPDATE_ERROR.   ENDIF.

  UPDATE ZTINSRSP.
  IF SY-SUBRC NE  0.   RAISE    UPDATE_ERROR.   ENDIF.

*-----------------------------------------------------------------------
* CHANGE DOCUMENT
  CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_INS'
     EXPORTING
        UPD_CHNGIND    =     'U'
        N_ZTINS        =     ZTINS
        O_ZTINS        =    *ZTINS
        N_ZTINSRSP     =     ZTINSRSP
        O_ZTINSRSP     =    *ZTINSRSP
        N_ZTINSSG3     =    *ZTINSSG3
        O_ZTINSSG3     =    *ZTINSSG3.
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
* L/C COST 반영
  CALL FUNCTION 'ZIM_SET_INSURANCE_COST'
     EXPORTING
        UPD_CHNGIND    =     UPD_CHNGIND
        N_ZTINS        =     ZTINS
        O_ZTINS        =     O_ZTINS
        N_ZTINSRSP     =     ZTINSRSP
        O_ZTINSRSP     =     O_ZTINSRSP.
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
* UNLOCK
  CALL FUNCTION 'DEQUEUE_EZ_IM_ZTINS'
         EXPORTING
             ZFREQNO                =     ZTINS-ZFREQNO
             ZFINSEQ                =     ZTINS-ZFINSEQ
             ZFAMDNO                =     ZTINS-ZFAMDNO.
*-----------------------------------------------------------------------

  ZTDHF1-ZFDHAPP = 'Y'.
  UPDATE  ZTDHF1.

  MESSAGE  S124  WITH  ZTINS-ZFREQNO ZTINS-ZFAMDNO '저장'.

ENDFUNCTION.
