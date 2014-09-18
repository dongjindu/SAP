*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM01I03                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입 B/L 관련 PAI MODULE Include                      *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.03.12                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&*
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_VALUE_SCR3112  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INPUT_VALUE_SCR3112 INPUT.

  CHECK W_STATUS NE C_REQ_D.
  CHECK W_STATUS NE C_OPEN_U.

  ">> Currency
  IF ZTIV-ZFIVAMC IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIV' 'ZFIVAMC'.
  ENDIF.

  ">> Customs Clearance Requested date
  IF ZTIV-ZFCCDT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIV' 'ZFCCDT'.
  ENDIF.

*>> LOCAL Logic Check skip
  CHECK ZTIV-ZFREQTY NE 'LO'.
  CHECK ZTIV-ZFREQTY NE 'PU'.

  ">> Customs Broker
*>> 2003.11.13 nash comment..
*  IF ZTIMIMG00-ZFIMPATH EQ '3' AND ZTIV-ZFCLCD NE 'X'.
  IF ZTIMIMG00-ZFIMPATH NE '3'.
    IF ZTIV-ZFCUT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIV' 'ZFCUT'.
    ENDIF.
  ENDIF.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFCSTMD NE 'S' AND ZTIMIMG00-ZFCSTMD NE 'P'.
    IF ZTIV-ZFPHVN IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIV' 'ZFPHVN'.
    ENDIF.
  ENDIF.

  IF ZTIV-ZFPONC IS INITIAL AND ZTIV-ZFCLCD NE 'X'.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIV' 'ZFPONC'.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_VALUE_SCR3112  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CC_EXCHAINGE_RATE_SCR3112  INPUT
*&---------------------------------------------------------------------*
MODULE GET_CC_EXCHAINGE_RATE_SCR3112 INPUT.
  CLEAR : ZTIV-ZFEXRT.
*>> LOCAL DATA LOGIC SKIP.
  CHECK ZTIV-ZFREQTY  NE 'LO'.
  CHECK ZTIV-ZFREQTY  NE 'PU'.


  IF ZTIV-ZFIVAMC EQ ZTIV-ZFKRW.
    MOVE: 1      TO  ZTIV-ZFEXRT,
          1      TO  ZTIV-FFACT.
    EXIT.
  ENDIF.
  PERFORM  P2000_GET_EX_RATE_NODIALOG   USING ZTIV-ZFIVAMC
                                              ZTIV-ZFKRW
                                              ZTIV-ZFCCDT
                                     CHANGING ZTIV-ZFEXRT
                                              ZTIV-FFACT.
  LOOP AT IT_ZSIVIT.
    IT_ZSIVIT-ZFIVAMK = ZSIVIT-ZFIVAMT * ZTIV-ZFEXRT.
    MODIFY IT_ZSIVIT INDEX SY-TABIX.
  ENDLOOP.

ENDMODULE.                 " GET_CC_EXCHAINGE_RATE_SCR3112  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR3111_MARK_TC_3111  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR3111_MARK_TC_3111 INPUT.

  READ TABLE IT_ZSIVIT  INDEX TC_3111-CURRENT_LINE.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIVIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIVIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIVIT INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR3111_MARK_TC_3111  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR3111  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR3111 INPUT.

  W_OK_CODE = SY-UCOMM.

  IF SY-UCOMM EQ 'MKA1'.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1'.
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
      " PAGE UP & DOWN
    WHEN 'PGUP'.
      TC_3111-TOP_LINE = TC_3111-TOP_LINE - LOOPLINES.
      IF TC_3111-TOP_LINE < 1.
        TC_3111-TOP_LINE = 1.
      ENDIF.
      EXIT.
    WHEN 'PGDN'.
      TC_3111-TOP_LINE = TC_3111-TOP_LINE + LOOPLINES.
      EXIT.
    WHEN 'DEL1'.
      DELETE IT_ZSIVIT  WHERE ZFMARK NE SPACE.
      PERFORM   P2000_IT_ZSIVIT_UPDATE  USING 'I'.
      EXIT.
    WHEN 'MKA1' OR 'MKL1'.
      LOOP AT IT_ZSIVIT.
        IT_ZSIVIT-ZFMARK = W_MARK.  MODIFY IT_ZSIVIT INDEX SY-TABIX.
      ENDLOOP.
      PERFORM   P2000_IT_ZSIVIT_UPDATE  USING 'I'.
      EXIT.
    WHEN 'REF1'.
      REFRESH : IT_ZSIVIT.
      IT_ZSIVIT[] = IT_ZSIVIT_ORG[].
      PERFORM   P2000_IT_ZSIVIT_UPDATE  USING 'I'.
      EXIT.
    WHEN 'PODP' OR 'IMDP' OR 'BLDP' OR 'CGDP'.
      PERFORM   P2000_LINE_SELECT_IV_ITEM.
      PERFORM   P2000_LINE_CALL_TCODE_IV_ITEM.
      PERFORM   P2000_IT_ZSIVIT_UPDATE  USING 'I'.
      EXIT.
    WHEN 'CALC'.
      PERFORM   P2000_IT_ZSIVIT_RECALC.
      PERFORM   P2000_IT_ZSIVIT_UPDATE  USING 'I'.
      EXIT.
    WHEN OTHERS.
      IF SY-BINPT NE 'X'.
        PERFORM   P2000_IT_ZSIVIT_UPDATE  USING 'I'.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR3111  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_BL_DATE_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_BL_DATE_SCR0102 INPUT.

* 환율 미입력?
  IF ZTBL-ZFEXRT IS INITIAL.
    SPOP-TEXTLINE1 = 'Did not input the exchange rate.'.
    IF ZTIMIMG00-ZFEXFIX EQ 'X'.
      CASE ZTIMIMG00-ZFEXMTD.
        WHEN 'B'.     ">B/L 발행일 기준.
          IF ZTBL-ZFBLDT NE *ZTBL-ZFBLDT
             AND NOT *ZTBL-ZFBLDT IS INITIAL.
            SPOP-TEXTLINE1 = 'Changed the B/L issuing date.'.
          ELSE.
            EXIT.
          ENDIF.
          PERFORM    P2000_GET_EXCHANGE_RATE USING ZTBL-ZFBLAMC
                                                   ZTBL-ZFBLDT
                                          CHANGING ZTBL-ZFEXRT
                                                   ZTBL-FFACT.
        WHEN 'A'.     ">B/L ETA 기준.
          IF ZTBL-ZFETA NE *ZTBL-ZFETA
             AND NOT *ZTBL-ZFETA IS INITIAL.
            SPOP-TEXTLINE1 = 'Changed the B/L ETA.'.
          ELSE.
            EXIT.
          ENDIF.
          PERFORM    P2000_GET_EXCHANGE_RATE USING ZTBL-ZFBLAMC
                                                   ZTBL-ZFETA
                                          CHANGING ZTBL-ZFEXRT
                                                   ZTBL-FFACT.
        WHEN 'R'.     ">B/L 실입항일 기준.
          IF ZTBL-ZFRETA NE *ZTBL-ZFRETA
             AND NOT *ZTBL-ZFRETA IS INITIAL.
            SPOP-TEXTLINE1 = 'Changed the B/L Real Time Arrived'.
          ELSE.
            EXIT.
          ENDIF.
          PERFORM    P2000_GET_EXCHANGE_RATE USING ZTBL-ZFBLAMC
                                                   ZTBL-ZFRETA
                                          CHANGING ZTBL-ZFEXRT
                                                   ZTBL-FFACT.
        WHEN OTHERS.
      ENDCASE.
    ELSE.
      PERFORM    P2000_GET_EXCHANGE_RATE USING ZTBL-ZFBLAMC
                                               ZTBL-ZFBLDT
                                      CHANGING ZTBL-ZFEXRT
                                               ZTBL-FFACT.

    ENDIF.
  ENDIF.

ENDMODULE.                 " GET_BL_DATE_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_IDS_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P2000_IDS_DEL_STATUS_CHECK.

*>> 수입 IMG 사항 SELECT!
  SELECT SINGLE * FROM ZTIMIMG00.

*>> 회계처리한 자료가 존재하면 DELETE 불가!

  W_LINE = 0.
  IF ZTIMIMG00-ZFCSTMD EQ 'I'.
    SELECT COUNT( * ) INTO W_COUNT
           FROM  ZTCUCLCST
           WHERE ZFBLNO   = ZTIDS-ZFBLNO
           AND   ZFCLSEQ  = ZTIDS-ZFCLSEQ
           AND   ZFACDO   NE SPACE
           AND   ZFFIYR   NE SPACE.
  ELSE.
*>> 통관요청번호 SELECT!
*     SELECT SINGLE * FROM ZTCUCLIV WHERE ZFBLNO  EQ  ZTIDS-ZFBLNO
*                                   AND   ZFCLSEQ EQ  ZTIDS-ZFCLSEQ.

    SELECT COUNT( * ) INTO W_COUNT
           FROM  ZTBSEG  AS  A  INNER JOIN ZTBKPF AS B
           ON    A~BUKRS        EQ    B~BUKRS
           AND   A~GJAHR        EQ    B~GJAHR
           AND   A~BELNR        EQ    B~BELNR
           WHERE A~ZFIMDNO      EQ    ZTCUCLIV-ZFIVNO
           AND   A~ZFCSTGRP     EQ    '006'
           AND ( B~ZFACDO       NE    SPACE
           OR    B~ZFFIYR       NE    SPACE    ).

  ENDIF.
  IF W_COUNT GT 0.
    MESSAGE E519 WITH ZTIDS-ZFBLNO ZTIDS-ZFCLSEQ W_COUNT.
  ENDIF.

*----------------------------------------------------------------------*
*>> 통관요청한 자료가 입고완료만 삭제 불가!
  CLEAR ZTIV.
  SELECT SINGLE * FROM ZTIV WHERE ZFBLNO EQ ZTIDS-ZFBLNO.
  IF ZTIV-ZFGRST EQ 'Y' OR ZTIV-ZFGRST EQ 'P'.
    MESSAGE E654 WITH ZTIDS-ZFBLNO ZTIDS-ZFCLSEQ.
  ENDIF.

  CLEAR : W_COUNT.
  SELECT COUNT( * ) INTO W_COUNT
         FROM ZTIVIT
         WHERE ZFBLNO EQ ZTIDS-ZFBLNO
         AND   NDFTX  EQ 'X'.
  IF W_COUNT GT 0.
    MESSAGE E418(ZIM1).
  ENDIF.

ENDFORM.                    " P2000_IDS_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Module  CHECK_MARK_FIELD_SCR0016  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_MARK_FIELD_SCR0016 INPUT.

  IF ZTBL-CCCK EQ 'X' AND ZTBL-CGCK EQ SPACE.
    SET CURSOR FIELD ZTBL-CGCK.
    MESSAGE W502.
  ENDIF.

ENDMODULE.                 " CHECK_MARK_FIELD_SCR0016  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CIV_MARK_SCR0016  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_CIV_MARK_SCR0016 INPUT.

  IF ZTBL-CIVCK EQ 'X'.
    IF ZSCIVHD-ZFCIVNO IS INITIAL.
      SET CURSOR FIELD ZSCIVHD-ZFCIVNO.
      MESSAGE W167 WITH 'Commercial invoice No'.
    ENDIF.
    IF ZTCIVHD-ZFIVAMT IS INITIAL.
      SET CURSOR FIELD ZSCIVHD-ZFIVAMT.
      MESSAGE W167 WITH 'Commercial invoice amount'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_CIV_MARK_SCR0016  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CG_MARK_SCR0016  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_CG_MARK_SCR0016 INPUT.

  IF ZTBL-CGCK EQ 'X'.
    IF ZSCGHD-ZFCGPT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCGHD' 'ZFCFPT'.

    ENDIF.
    IF ZSCGIT-ZFBNARCD IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCGIT' 'ZFBNARCD'.
    ELSE.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_CG_MARK_SCR0016  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_IDR_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_IDR_DEL_STATUS_CHECK.

  CASE ZTIDR-ZFDOCST.
    WHEN 'R'.
      MESSAGE E104 WITH ZTIDR-ZFBLNO ZTIDR-ZFCLSEQ ZTIDR-ZFDOCST.
    WHEN 'O'.
      MESSAGE E104 WITH ZTIDR-ZFBLNO ZTIDR-ZFCLSEQ ZTIDR-ZFDOCST.
    WHEN 'N'.
  ENDCASE.

*>> 통관상태가 통관완료 이면 삭제 불가!
* NHJ 주석처리 ( TABLE 삭제 로 인한 )
*   SELECT SINGLE  * FROM ZTCUCL
*  WHERE  ZFBLNO  = ZTIDR-ZFBLNO
*  AND    ZFCLSEQ = ZTIDR-ZFCLSEQ.
*   IF ZTCUCL-ZFCUST EQ 'Y'.
*     MESSAGE E755. EXIT.
*  ENDIF.
  SELECT SINGLE * FROM ZTIV
  WHERE  ZFIVNO   EQ   ZTIDR-ZFIVNO.
  IF ZTIV-ZFCUST EQ 'Y'.
    MESSAGE  E755.  EXIT.
  ENDIF.

*>> 해당 통관 자료가 입고완료면 삭제 불가!
* NHJ 주석처리 ( TABLE 삭제로 인한 )
*   SELECT SINGLE * FROM ZTCUCLIV
*   WHERE  ZFBLNO   EQ   ZTIDR-ZFBLNO
*   AND    ZFCLSEQ  EQ   ZTIDR-ZFCLSEQ.

  SELECT SINGLE * FROM ZTIV
  WHERE  ZFIVNO   EQ   ZTIDR-ZFIVNO.
  IF ZTIV-ZFGRST  EQ  'Y'.
    MESSAGE E654 WITH ZTIDR-ZFBLNO ZTIDR-ZFCLSEQ.
  ENDIF.

ENDFORM.                    " P2000_IDR_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Module  PORT_DUP_CHECK_SCR9912  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PORT_DUP_CHECK_SCR9912 INPUT.
  READ TABLE IT_ZSMSCST WITH KEY ZFAPRTC = ZSMSCST-ZFAPRTC.
  IF SY-SUBRC EQ 0.
    MESSAGE E507 WITH ZSMSCST-ZFAPRTC.
  ENDIF.
ENDMODULE.                 " PORT_DUP_CHECK_SCR9912  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_RENT_FIELD_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_RENT_FIELD_SCR0102 INPUT.
  W_COUNT = 0.
*>> 양도B/L을 선택했을 경우....
  IF ZTBL-ZFRENT EQ 'X'.
    SELECT COUNT( * ) INTO W_COUNT
           FROM  ZTCGIT
           WHERE ZFBLNO EQ ZTBL-ZFBLNO.
    IF W_COUNT GT 0.
      MESSAGE E532 WITH ZTBL-ZFBLNO.
    ENDIF.
    IF ZTBL-ZFSHTY NE 'B'.    ">BULK만 체크.
      MESSAGE E539 WITH ZTBL-ZFBLNO.
    ENDIF.
  ELSE.
    SELECT COUNT( * ) INTO W_COUNT
           FROM  ZTIVIT
           WHERE ZFBLNO EQ ZTBL-ZFBLNO
           AND ( ZFCGNO IS NULL
           OR    ZFCGNO EQ SPACE ).
    IF W_COUNT GT 0.
      MESSAGE E533 WITH ZTBL-ZFBLNO.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_RENT_FIELD_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR3115_MARK_TC_3115  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR3115_MARK_TC_3115 INPUT.

  READ TABLE IT_ZSIVHST   INDEX TC_3115-CURRENT_LINE.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIVHST-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIVHST-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIVHST INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR3115_MARK_TC_3115  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_HOUSE_BL_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_HOUSE_BL_CHECK INPUT.
  IF NOT ZTBL-ZFHBLNO IS INITIAL.
    IF ZTBLINOU-ZFHSN IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBLINOU' 'ZFHSN'.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_HOUSE_BL_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_EXEC_INT_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_EXEC_INT_DOCUMENT.

*>> 거래번호 입력 확인.
*  IF ZTPMTHD-RFHA IS INITIAL.
*     MESSAGE  E542.
*  ENDIF.
*>> 회사코드 CHECK!
*  IF ZTPMTHD-BUKRS IS INITIAL.
*     MESSAGE E543.
*  ENDIF.
*>> 자금 TRANSACTION CALL.
*   SET PARAMETER ID 'BUK' FIELD ZTPMTHD-BUKRS.
*   SET PARAMETER ID 'FAN' FIELD ZTPMTHD-RFHA.

*   CALL TRANSACTION 'TM02'.
*MKIM
*   CALL TRANSACTION 'ZTR34'.

ENDFORM.                    " P2000_EXEC_INT_DOCUMENT
*&---------------------------------------------------------------------*
*&      Module  VALUE_ZFCUT_SCR6710  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALUE_ZFCUT_SCR6710 INPUT.

  SELECT ZTIMIMG10~ZFCUT ZTIMIMG10~ZFVEN LFA1~NAME1
    INTO CORRESPONDING FIELDS OF TABLE IT_ZFCUT_TAB
    FROM ZTIMIMG10 INNER JOIN LFA1
    ON  ZTIMIMG10~ZFVEN = LFA1~LIFNR.

  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.
  WINDOW_TITLE = '관세사 조회 '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'OTYPE'
            DYNPPROG        = DYNPROG
            DYNPNR          = DYNNR
            DYNPROFIELD     = 'ZTCUCLIV-ZFCUT'
            WINDOW_TITLE    = WINDOW_TITLE
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = IT_ZFCUT_TAB
       EXCEPTIONS
            PARAMETER_ERROR = 1
            NO_VALUES_FOUND = 2
            OTHERS          = 3.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

ENDMODULE.                 " VALUE_ZFCUT_SCR6710  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALUE_ZFCUT_SCR3112  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALUE_ZFCUT_SCR3112 INPUT.

  SELECT ZTIMIMG10~ZFCUT ZTIMIMG10~ZFVEN LFA1~NAME1
    INTO CORRESPONDING FIELDS OF TABLE IT_ZFCUT_TAB
    FROM ZTIMIMG10 INNER JOIN LFA1
    ON  ZTIMIMG10~ZFVEN = LFA1~LIFNR.

  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.
  WINDOW_TITLE = '관세사 조회 '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'OTYPE'
            DYNPPROG        = DYNPROG
            DYNPNR          = DYNNR
            DYNPROFIELD     = 'ZTIV-ZFCUT'
            WINDOW_TITLE    = WINDOW_TITLE
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = IT_ZFCUT_TAB
       EXCEPTIONS
            PARAMETER_ERROR = 1
            NO_VALUES_FOUND = 2
            OTHERS          = 3.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

ENDMODULE.                 " VALUE_ZFCUT_SCR3112  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFGMNO_CHECK_SCR0812  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZFGMNO_CHECK_SCR0812 INPUT.

  READ TABLE IT_ZSCGIT   INDEX TC_0812-CURRENT_LINE.

  IF SY-SUBRC EQ 0.
    W_TABIX = SY-TABIX.
    IF NOT IT_ZSCGIT-ZFBLNO IS INITIAL.
      CLEAR : ZTBL.
      SELECT SINGLE * FROM ZTBL
             WHERE ZFBLNO  EQ   IT_ZSCGIT-ZFBLNO.
      IF NOT ZTBL-ZFGMNO IS INITIAL.
        IF NOT ( IT_ZSCGIT-ZFGMNO EQ ZTBL-ZFGMNO AND
                 IT_ZSCGIT-ZFMSN  EQ ZTBL-ZFMSN  AND
                 IT_ZSCGIT-ZFHSN  EQ ZTBL-ZFHSN ).
          CLEAR : W_TEXT20.
          IF NOT ZTBL-ZFGMNO IS INITIAL.
            W_TEXT20 = ZTBL-ZFGMNO.
          ENDIF.
          IF NOT ZTBL-ZFMSN IS INITIAL.
            IF W_TEXT20 IS INITIAL.
              W_TEXT20 = ZTBL-ZFMSN.
            ELSE.
              CONCATENATE W_TEXT20 '-' ZTBL-ZFMSN INTO W_TEXT20.
            ENDIF.
          ENDIF.
          IF NOT ZTBL-ZFHSN IS INITIAL.
            IF W_TEXT20 IS INITIAL.
              W_TEXT20 = ZTBL-ZFHSN.
            ELSE.
              CONCATENATE W_TEXT20 '-' ZTBL-ZFHSN INTO W_TEXT20.
            ENDIF.
          ENDIF.
          PERFORM P2000_NO_INPUT  USING  'ZSCGIT' 'ZFGMNO'
                                         DFIES-SCRTEXT_M W_SUBRC.
          MESSAGE W566 WITH W_TEXT20.
        ENDIF.
      ENDIF.
    ENDIF.
    MOVE : ZSCGIT-ZFGMNO     TO   IT_ZSCGIT-ZFGMNO,
           ZSCGIT-ZFMSN      TO   IT_ZSCGIT-ZFMSN,
           ZSCGIT-ZFHSN      TO   IT_ZSCGIT-ZFHSN.
    MODIFY IT_ZSCGIT  INDEX   W_TABIX.
  ENDIF.

ENDMODULE.                 " ZFGMNO_CHECK_SCR0812  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PORT_CODE_SCR9912  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_PORT_CODE_SCR9912 INPUT.
  CHECK : W_STATUS NE C_REQ_D.

  IF ZTMSHD-ZFAPPC EQ SPACE.
    SET PARAMETER ID 'ZPAPRTC' FIELD ''.
  ELSE.
    SET PARAMETER ID 'ZPAPRTC' FIELD ZTMSHD-ZFAPPC.
  ENDIF.
  CLEAR : ZTIMIMG08.
*   SELECT SINGLE * FROM ZTIMIMG08 WHERE ZFCDTY   EQ   '002'
*                                  AND   ZFCD     EQ   ZTMSHD-ZFSPRTC
*                                  AND   ZFCD2    EQ   ZTMSHD-ZFCARC.
  SELECT SINGLE * FROM ZTIEPORT
         WHERE    LAND1  EQ   ZTMSHD-ZFCARC
         AND      PORT   EQ   ZTMSHD-ZFSPRTC.

  IF SY-SUBRC NE 0.
    MESSAGE ID 'ZIM' TYPE 'E' NUMBER 308 WITH ZTMSHD-ZFSPRTC.
  ENDIF.

  W_TMP_TEXT = ZTIEPORT-PORTT.

*   PERFORM  GET_PORT_NAME     USING      '002'   ZTMSHD-ZFSPRTC  'E'
*                              CHANGING   W_TMP_TEXT.

ENDMODULE.                 " CHECK_PORT_CODE_SCR9912  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR3116  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR3116 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_3116-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR3116  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_3116_UPDATE_SCR3116  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_3116_UPDATE_SCR3116 INPUT.

  CHECK W_STATUS NE C_REQ_D.
  CHECK W_STATUS NE C_OPEN_U.

  READ TABLE IT_ZSIVIT INDEX TC_3116-CURRENT_LINE.
  MOVE: SY-SUBRC TO W_OLD_SUBRC,
        SY-TABIX TO W_TABIX.

*-----------------------------------------------------------------------
*>> 미입고 구분자 체크.
  IF ZTIV-ZFGRST EQ 'N'.
    IF ZSIVIT-UMSON EQ 'X'.
      IF ZSIVIT-REPOS IS INITIAL     AND
         NOT ZSIVIT-MATNR IS INITIAL AND
         NOT ZSIVIT-EBELN IS INITIAL.
        PERFORM P2000_NO_INPUT  USING  'ZSIVIT' 'UMSON'
                                        DFIES-SCRTEXT_M L_SUBRC.
        MESSAGE E553 WITH ZSIVIT-EBELN ZSIVIT-EBELP.
      ENDIF.
      IF NOT ZSIVIT-ZFPOTY IS INITIAL.
        PERFORM P2000_NO_INPUT  USING  'ZSIVIT' 'UMSON'
                                        DFIES-SCRTEXT_M L_SUBRC.
        MESSAGE E553 WITH ZSIVIT-EBELN ZSIVIT-EBELP.
      ENDIF.
    ENDIF.

    IF ZSIVIT-UMSON IS INITIAL.
      IF ZSIVIT-ZFPOTY IS INITIAL AND
         NOT ZSIVIT-GRMENGE IS INITIAL.
        PERFORM P2000_NO_INPUT  USING  'ZSIVIT' 'GRMENGE'
                                        DFIES-SCRTEXT_M L_SUBRC.
        MESSAGE E550.
      ENDIF.
    ENDIF.
*>> 입고수량...
    IF NOT ZSIVIT-ZFPOTY IS INITIAL AND
       ZSIVIT-GRMENGE IS INITIAL AND ZSIVIT-UMSON EQ 'X'.
      PERFORM P2000_NO_INPUT  USING  'ZSIVIT' 'GRMENGE'
                                      DFIES-SCRTEXT_M L_SUBRC.
      MESSAGE E551.
    ENDIF.

  ELSEIF ZTIV-ZFGRST EQ 'X'.
    IF ZSIVIT-UMSON EQ 'X'.
      PERFORM P2000_NO_INPUT  USING  'ZSIVIT' 'UMSON'
                                     DFIES-SCRTEXT_M L_SUBRC.
      MESSAGE E554.
    ENDIF.
    IF NOT ZSIVIT-GRMENGE IS INITIAL.
      PERFORM P2000_NO_INPUT  USING  'ZSIVIT' 'GRMENGE'
                                     DFIES-SCRTEXT_M L_SUBRC.
      MESSAGE E555.
    ENDIF.
  ENDIF.


*>> 입력내역 SUMMARY.
  MOVE-CORRESPONDING  ZSIVIT  TO   IT_IVIT_SUM.
  COLLECT IT_IVIT_SUM.

  CLEAR : IT_IVIT_SUM.
  READ TABLE IT_IVIT_SUM WITH KEY ZFREQNO = ZSIVIT-ZFREQNO
                                  ZFITMNO = ZSIVIT-ZFITMNO
                                  ZFBLNO  = ZSIVIT-ZFBLNO
                                  ZFBLIT  = ZSIVIT-ZFBLIT
                                  ZFCGNO  = ZSIVIT-ZFCGNO
                                  ZFCGIT  = ZSIVIT-ZFCGIT.
  W_SUBRC = SY-SUBRC.
*>>
  CLEAR : ZTIVIT.
  SELECT SINGLE * FROM  ZTIVIT
           WHERE ZFIVNO   EQ  ZTIV-ZFIVNO
           AND   ZFIVDNO  EQ  ZSIVIT-ZFIVDNO.


*>> 입고수량 체크.
  IF NOT ZSIVIT-GRMENGE IS INITIAL.
*>> 수량체크.
    CLEAR : W_MENGE1.
    IF ZSIVIT-ZFCGNO IS INITIAL.
      IF ZSIVIT-ZFBLNO IS INITIAL.
        IF NOT ZSIVIT-ZFREQNO IS INITIAL.
          W_MENGE = ZSIVIT-MENGE   - ZSIVIT-GRMENGE -
                  ( ZSIVIT-ZFGRTOT - ZTIVIT-GRMENGE ).
          IF W_MENGE LT 0.
            PERFORM P2000_NO_INPUT USING 'ZSIVIT' 'GRMENGE'
                                   DFIES-SCRTEXT_M W_SUBRC.
            W_MENGE = ZSIVIT-ZFGRTOT - ZTIVIT-GRMENGE
                                     + ZSIVIT-CCMENGE.
            WRITE :  W_MENGE TO W_AMTTXT1 UNIT ZSIVIT-MEINS .
            W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
            WRITE : ZSIVIT-MENGE TO W_AMTTXT2 UNIT ZSIVIT-MEINS.
            W_AMTLEN2 = STRLEN( W_AMTTXT2 ).

            W_MENGE = ZSIVIT-ZFGRTOT - ZTIVIT-GRMENGE
                                     + ZSIVIT-CCMENGE.
            MESSAGE E438 WITH ZSIVIT-ZFIVDNO 'G/R'
*                                                 W_MENGE ZSIVIT-MENGE.
             W_AMTTXT1(W_AMTLEN1)  W_AMTTXT2(W_AMTLEN2).
          ENDIF.
        ENDIF.
      ELSE.
        W_MENGE = ZSIVIT-MENGE_BL - ZSIVIT-GRMENGE -
                  ( ZSIVIT-ZFGRTOT - ZTIVIT-GRMENGE ).
        IF W_MENGE LT 0.
          PERFORM P2000_NO_INPUT USING 'ZSIVIT' 'GRMENGE'
                                 DFIES-SCRTEXT_M W_SUBRC.
          W_MENGE = ZSIVIT-ZFGRTOT - ZTIVIT-GRMENGE
                                   + ZSIVIT-GRMENGE.
          WRITE :  W_MENGE TO W_AMTTXT1 UNIT ZSIVIT-MEINS .
          W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
          WRITE : ZSIVIT-MENGE_BL TO W_AMTTXT2 UNIT ZSIVIT-MEINS .
          W_AMTLEN2 = STRLEN( W_AMTTXT2 ).
          MESSAGE E439 WITH ZSIVIT-ZFIVDNO 'G/R'
*                                              W_MENGE ZSIVIT-MENGE_BL.
          W_AMTTXT1(W_AMTLEN1)  W_AMTTXT2(W_AMTLEN2).
        ENDIF.
      ENDIF.
    ELSE.
      W_MENGE = ZSIVIT-CGMENGE - ZSIVIT-ZFGRTOT -
                ( ZSIVIT-GRMENGE - ZTIVIT-GRMENGE ).
      IF W_MENGE LT 0.
        PERFORM P2000_NO_INPUT USING 'ZSIVIT' 'GRMENGE'
                               DFIES-SCRTEXT_M W_SUBRC.
        W_MENGE = ZSIVIT-ZFGRTOT - ZTIVIT-GRMENGE
                                 + ZSIVIT-GRMENGE.
        WRITE :  W_MENGE TO W_AMTTXT1 UNIT ZSIVIT-MEINS .
        W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
        WRITE : ZSIVIT-CGMENGE TO W_AMTTXT2 UNIT ZSIVIT-MEINS .
        W_AMTLEN2 = STRLEN( W_AMTTXT2 ).
        MESSAGE E440 WITH ZSIVIT-ZFIVDNO 'G/R'
*                                   W_MENGE
*                                  ZSIVIT-CGMENGE.
        W_AMTTXT1(W_AMTLEN1)  W_AMTTXT2(W_AMTLEN2).

      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT ZSIVIT-GRMENGE IS INITIAL.
    IF ZSIVIT-WERKS IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIVIT' 'WERKS'.
    ENDIF.

*> 분할 통관을 허용하지 않을 경우.
    IF ZTIMIMG00-GRPARTX NE 'X'.
      IF ZSIVIT-LGORT IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIVIT' 'LGORT'.
      ENDIF.
    ENDIF.

    IF ZSIVIT-ZFMATGB IS INITIAL AND ZTIMIMG00-ZFCSTMD NE 'S'.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIVIT' 'ZFMATGB'.
    ENDIF.
    IF ZSIVIT-ZFIVAMK IS INITIAL.
      IF ZSIVIT-ZFIVAMC NE 'KRW'.
*           ZSIVIT-ZFIVAMK = ZTIV-ZFEXRT * ZSIVIT-ZFIVAMT.
        IF ZTIV-FFACT IS INITIAL.
          ZTIV-FFACT  =  1.
        ENDIF.
         *BAPICURR-BAPICURR = ( ZTIV-ZFEXRT / ZTIV-FFACT )
                              * ZSIVIT-ZFIVAMT.
        SELECT SINGLE * FROM  TCURX
               WHERE  CURRKEY     = ZSIVIT-ZFIVAMC.
        IF SY-SUBRC NE 0.
          TCURX-CURRDEC = 2.
        ENDIF.
        IF TCURX-CURRDEC NE 0.
          PERFORM SET_CURR_CONV_TO_INTERNAL USING
*                      ZSIVIT-ZFIVAMK   ZSIVIT-ZFKRW.
                  *BAPICURR-BAPICURR   ZSIVIT-ZFKRW.
        ENDIF.
        IF *BAPICURR-BAPICURR GT 9999999999999.
          MESSAGE W923 WITH *BAPICURR-BAPICURR.
          ZSIVIT-ZFIVAMK = 0.
        ELSE.
          ZSIVIT-ZFIVAMK = *BAPICURR-BAPICURR.
        ENDIF.

      ELSE.
        ZSIVIT-ZFIVAMK = ZSIVIT-ZFIVAMT.
      ENDIF.
*         PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIVIT' 'ZFIVAMK'.

    ENDIF.
  ELSE.
*     IF NOT ZSIVIT-ZFIVAMT  IS INITIAL OR
*        NOT ZSIVIT-ZFIVAMK  IS INITIAL.
*        PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIVIT' 'CCMENGE'.
*     ENDIF.
  ENDIF.

  MOVE-CORRESPONDING ZSIVIT TO IT_ZSIVIT.
  MOVE W_ROW_MARK           TO IT_ZSIVIT-ZFMARK.

  IF W_OLD_SUBRC EQ 0.
    MODIFY IT_ZSIVIT   INDEX TC_3116-CURRENT_LINE.
  ELSE.
    APPEND IT_ZSIVIT.
  ENDIF.


ENDMODULE.                 " TC_3116_UPDATE_SCR3116  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR3116_MARK_TC_3116  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR3116_MARK_TC_3116 INPUT.
  READ TABLE IT_ZSIVIT  INDEX TC_3116-CURRENT_LINE.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIVIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIVIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIVIT INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR3116_MARK_TC_3116  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_TERM_SCR9912  INPUT
*&---------------------------------------------------------------------*
*       하역사에 따라 지급조건과 세금코드 SETTING
*----------------------------------------------------------------------*
MODULE READ_TERM_SCR9912 INPUT.
  IF ZSMSCST-ZFCCGB IS INITIAL.
    MESSAGE E167 WITH 'Classification of Dispatch/Demurrage'.
  ENDIF.
  SELECT ZTERM
    INTO ZSMSCST-ZTERM
    FROM LFB1
   WHERE BUKRS = IT_ZSMSHD-BUKRS
     AND LIFNR = ZSMSCST-ZFCARGO.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    MESSAGE W864 WITH IT_ZSMSCST-ZFCARGO.
  ENDIF.
  IF ZSMSCST-ZFCCGB EQ 'A'.
    ZSMSCST-MWSKZ = 'S1'.
  ELSE.
    ZSMSCST-MWSKZ = ''.
  ENDIF.
ENDMODULE.                 " READ_TERM_SCR9912  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_COST_HISTORY_SCR0050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_COST_HISTORY_SCR0050 INPUT.

  LEAVE TO LIST-PROCESSING.

  G_REPID = SY-REPID.
  CLEAR G_VARIANT.
  G_VARIANT-REPORT = G_REPID.

*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = G_REPID
            I_STRUCTURE_NAME         = 'ZSIMCOST'
            I_CALLBACK_PF_STATUS_SET = G_STATUS
            I_CALLBACK_USER_COMMAND  = G_USER_COMMAND
            I_SAVE                   = G_SAVE
            IS_VARIANT               = G_VARIANT
       TABLES
            T_OUTTAB                 = IT_ZSIMCOST.

*  CLEAR : OK-CODE.
  LEAVE TO SCREEN W_DYNNR.

ENDMODULE.                 " SET_COST_HISTORY_SCR0050  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR0020 INPUT.

  IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' ).
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  IF ZTIVHST-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BUDAT'.
  ENDIF.
  IF ZTIVHST-BLDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BLDAT'.
  ENDIF.
  IF ZTIVHST-BWART IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BWART'.
  ENDIF.

  IF NOT W_ZFIDSDT IS INITIAL.
    IF  ZTIVHST-BLDAT LT W_ZFIDSDT.
      MESSAGE  E560.
    ENDIF.
  ENDIF.

  IF ZTIVHST-BWART IS INITIAL.
    MOVE ZTIMIMG00-ZFMVTY1  TO ZTIVHST-BWART.
    IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'Y'. "무환수출입.
      MOVE ZTIMIMG00-ZFMVTY2  TO ZTIVHST-BWART.
    ENDIF.
    IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'N'. "무환.
      MOVE ZTIMIMG00-ZFMVTY3  TO ZTIVHST-BWART.
    ENDIF.
  ELSE.
    SELECT SINGLE *
      FROM T156
     WHERE BWART = ZTIVHST-BWART.
    IF SY-SUBRC NE 0.
      MESSAGE E799.
      EXIT.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_GR_CANCEL_DATE_SCR0030  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_GR_CANCEL_DATE_SCR0030 INPUT.

  IF OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR'.
    IF *ZTIVHST-BUDAT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING '*ZTIVHST' 'BUDAT'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_GR_CANCEL_DATE_SCR0030  INPUT
*&---------------------------------------------------------------------*
*&      Module  REQIT_GET_LINE_SCR8510  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE REQIT_GET_LINE_SCR8510 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_8510-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " REQIT_GET_LINE_SCR8510  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_8510_UPDATE_SCR8510  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_8510_UPDATE_SCR8510 INPUT.

  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  READ TABLE IT_ZSVTSG3 INDEX TC_8510-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.

  MOVE-CORRESPONDING ZSVTSG3 TO IT_ZSVTSG3.
  MOVE W_ROW_MARK TO IT_ZSVTSG3-ZFMARK.

  IF W_OLD_SUBRC = 0.
    MODIFY IT_ZSVTSG3   INDEX TC_8510-CURRENT_LINE.
  ELSE.
    APPEND IT_ZSVTSG3.
  ENDIF.

ENDMODULE.                 " TC_8510_UPDATE_SCR8510  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR8710_RED_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SCR8710_RED_PRINT.

  SUBMIT  ZRIMLOCRCT WITH P_REDNO EQ ZTRED-ZFREDNO
                              AND RETURN. " 다시 돌아 올수 있게.

ENDFORM.                    " P2000_SCR8710_RED_PRINT
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR8510_VT_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SCR8510_VT_PRINT.

  SET PARAMETER    ID 'ZPVTNO'  FIELD ZTVT-ZFVTNO.
  EXPORT 'ZPVTNO'  TO MEMORY    ID   'ZPVTNO'.

  CALL TRANSACTION 'ZIMA9' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SCR8510_VT_PRINT
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR8510_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SCR8510_DISPLAY.

*>> 구매처 정보 조회.
  IF OK-CODE  EQ 'DILI'.

    SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
    SET PARAMETER ID 'LIF' FIELD ZTRED-LIFNR.
    SET PARAMETER ID 'EKO' FIELD ''.

    EXPORT 'LIF'   TO MEMORY ID 'LIF'.
    EXPORT 'EKO'   TO MEMORY ID 'EKO'.

    CALL TRANSACTION 'MK03' AND SKIP  FIRST SCREEN.

  ENDIF.
*>> PO 조회.
  IF OK-CODE  EQ 'DIPO'.
    PERFORM  P2000_PO_DOC_DISPLAY USING ZTRED-EBELN ''.
*     SET PARAMETER ID 'BSP' FIELD ''.
*     EXPORT 'BSP' TO MEMORY ID 'BSP'.
*     SET PARAMETER ID 'BES' FIELD ZTRED-EBELN.
*     EXPORT 'BES'  TO MEMORY ID 'BES'.
*
*     CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.

  ENDIF.
*>> 계산서 조회.
  IF OK-CODE  EQ 'DIVT'.

    SET PARAMETER    ID 'ZPVTNO'  FIELD ZTRED-ZFVTNO.
    EXPORT 'ZPVTNO'  TO MEMORY    ID   'ZPVTNO'.

    CALL TRANSACTION 'ZIMA4' AND SKIP FIRST SCREEN.

  ENDIF.
*>> 자재내역 조회.
  CLEAR : W_PROC_CNT.
  IF OK-CODE EQ 'DIMA'.
    LOOP  AT  IT_ZSREDSG1  WHERE  ZFMARK  EQ 'X'.
      W_PROC_CNT = W_PROC_CNT + 1.
    ENDLOOP.
    CASE W_PROC_CNT.
      WHEN 0.
        MESSAGE S951.
      WHEN 1.
        PERFORM P1000_EKPO_SELECT_ITEM.

        SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
        SET PARAMETER ID 'BUK' FIELD EKPO-BUKRS.
        SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
        SET PARAMETER ID 'LAG' FIELD ''.
        SET PARAMETER ID 'MXX' FIELD MM03_START_SICHT.
        CALL TRANSACTION 'MM03' AND SKIP  FIRST SCREEN.
      WHEN OTHERS.
        MESSAGE S965.
    ENDCASE.

    IF W_PROC_CNT EQ 0.
      MESSAGE  S766. EXIT.
    ENDIF.
    IF W_PROC_CNT > 1.
      MESSAGE  S965. EXIT.
    ENDIF.

    PERFORM P1000_EKPO_SELECT_ITEM.

    SET PARAMETER ID 'MAT' FIELD IT_ZSREDSG1-MATNR.
    SET PARAMETER ID 'BUK' FIELD EKPO-BUKRS.
    SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
    SET PARAMETER ID 'LAG' FIELD ''.
    SET PARAMETER ID 'MXX' FIELD MM03_START_SICHT.
    CALL TRANSACTION 'MM03' AND SKIP  FIRST SCREEN.
  ENDIF.

ENDFORM.                    " P2000_SCR8510_DISPLAY
*&---------------------------------------------------------------------*
*&      Module  TC_8710_UPDATE_SCR8710  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_8710_UPDATE_SCR8710 INPUT.

*  IF W_STATUS = 'D'.   EXIT.   ENDIF.

  READ TABLE IT_ZSREDSG1 INDEX TC_8710-CURRENT_LINE.
  MOVE SY-SUBRC TO W_OLD_SUBRC.

  MOVE-CORRESPONDING ZSREDSG1  TO IT_ZSREDSG1.
  MOVE W_ROW_MARK TO IT_ZSREDSG1-ZFMARK.

  IF W_OLD_SUBRC = 0.
    MODIFY IT_ZSREDSG1   INDEX TC_8710-CURRENT_LINE.
  ELSE.
    APPEND IT_ZSREDSG1.
  ENDIF.

ENDMODULE.                 " TC_8710_UPDATE_SCR8710  INPUT
*&---------------------------------------------------------------------*
*&      Module  FIELD_MATCH_CHECK_SCR3101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FIELD_MATCH_CHECK_SCR3101 INPUT.

  PERFORM  P2000_OK_CODE_PROCESS.

  CLEAR : ZTIV, *ZTIV.
  REFRESH : IT_ZSIVIT, IT_ZSIVCD, IT_ZSIVIT_ORG, IT_ZSIVIT_ORG.
  ANTWORT = 'N'.
*  PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIV' 'ZFPOYN'.

*>> 최종입고지시자.
  IF NOT ZSIV-ZFLGRST IS INITIAL.
    MESSAGE I402.
  ENDIF.

  IF ZSREQHD-ZFOPNNO IS INITIAL.         " 문서 NO를 입력하지 않을 경?
    IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E061.
    ENDIF.
  ELSE.
    IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
* L/C NO에 count인 문서관리번호를 SELECT.
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTREQST
                        WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO.
      IF W_COUNT EQ 0.
        MESSAGE E067 WITH ZSREQHD-ZFOPNNO.
      ELSEIF W_COUNT > 1.
        MESSAGE E134 WITH W_COUNT.
      ENDIF.

* L/C NO에 MAX인 문서관리번호를 SELECT.
      SELECT MAX( ZFREQNO ) INTO  W_ZFREQNO
                            FROM  ZTREQST
                            WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO.

      IF SY-SUBRC NE 0.
        MESSAGE E062 WITH  ZSREQHD-ZFOPNNO.
      ELSE.
        IF W_ZFREQNO IS INITIAL.
          IF ZSREQHD-ZFREQNO IS INITIAL.  " 관리번호가 입력되지 않?
            MESSAGE E062 WITH  ZSREQHD-ZFOPNNO.
          ENDIF.
        ELSE.
          ZSREQHD-ZFREQNO = W_ZFREQNO.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
* 수입의뢰 Header Table
  CALL FUNCTION 'ZIM_GET_REQ_DOC_HEADER'
         EXPORTING
               ZFREQNO           =     ZSREQHD-ZFREQNO
*              ZFAMDNO           =     ZTREQST-ZFAMDNO
         IMPORTING
               W_ZTREQHD         =      ZTREQHD
               W_ZTREQST         =      ZTREQST
*         TABLES
*               IT_ZSREQIL        =      IT_ZSREQIL
*               IT_ZSREQIL_ORG    =      IT_ZSREQIL_ORG
*               IT_ZTREQORJ       =      IT_ZTREQORJ
*               IT_ZTREQORJ_ORG   =      IT_ZTREQORJ_ORG
         EXCEPTIONS
               NOT_FOUND         =    4
               NOT_INPUT         =    8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E018 WITH ZSREQHD-ZFREQNO.
    WHEN 8.
      MESSAGE E019.
  ENDCASE.

  IF NOT ( ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU' ).
    MESSAGE E635 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
  ENDIF.

*>> 삼국무역...
  IF ZTREQHD-ZFTRIPLE EQ 'X'.
    MESSAGE E535 WITH ZTREQHD-ZFREQNO.
  ENDIF.

*>> 사후관리여부...
  IF ZTREQHD-ZFCLOSE EQ 'X'.
    MESSAGE E354 WITH ZTREQHD-ZFREQNO.
  ENDIF.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
    MESSAGE E963.
  ENDIF.

  IF ZTIMIMG00-ZFEXFIX EQ 'X'.
*>> 환율차이 오류 메시지.
    IF ZTREQHD-WAERS NE 'KRW' AND
       ( ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU' ).
      SELECT SINGLE * FROM EKKO
                      WHERE EBELN  EQ  ZTREQHD-EBELN.
      IF EKKO-KUFIX NE 'X'.
        MESSAGE E528 WITH ZTREQHD-EBELN.
      ELSE.
        IF ZTREQHD-KURSF NE EKKO-WKURS.
          MESSAGE E614 WITH ZTREQHD-EBELN EKKO-WKURS
                            ZTREQHD-ZFREQNO ZTREQHD-KURSF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  REFRESH : IT_ZFREQNO, IT_LOCKED, IT_ZSCIVIT, IT_ZSIVIT.
*  IF SY-TCODE EQ 'ZIM31'.
  PERFORM  P2000_REQ_TO_CC_MOVE.           ">수입의뢰 ===> 통관요청.
*  ELSE.
*     PERFORM  P2000_REQ_DATA_MOVE.            " B/L DATA MOVE.
*  ENDIF.
  ANTWORT = 'Y'.


ENDMODULE.                 " FIELD_MATCH_CHECK_SCR3101  INPUT
*&---------------------------------------------------------------------*
*&      Module  FIELD_MATCH_CHECK_SCR3501  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FIELD_MATCH_CHECK_SCR3501 INPUT.

  PERFORM  P2000_OK_CODE_PROCESS.

*>> 선급금 여부 체크.
  IF ZSCIVHD-ZFPRPYN IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCIVHD' 'ZFPRPYN'.
  ELSEIF ZSCIVHD-ZFPRPYN EQ 'Y'.
    MESSAGE W640.
  ENDIF.

  CLEAR : ZTIV, *ZTIV.
  REFRESH : IT_ZSIVIT, IT_ZSIVCD, IT_ZSIVIT_ORG, IT_ZSIVIT_ORG.
  ANTWORT = 'N'.
*  PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIV' 'ZFPOYN'.

  IF ZSREQHD-ZFOPNNO IS INITIAL.         " 문서 NO를 입력하지 않을 경?
    IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E061.
    ENDIF.
  ELSE.
    IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
* L/C NO에 count인 문서관리번호를 SELECT.
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTREQST
                        WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO.
      IF W_COUNT EQ 0.
        MESSAGE E067 WITH ZSREQHD-ZFOPNNO.
      ELSEIF W_COUNT > 1.
        MESSAGE E134 WITH W_COUNT.
      ENDIF.

* L/C NO에 MAX인 문서관리번호를 SELECT.
      SELECT MAX( ZFREQNO ) INTO  W_ZFREQNO
                            FROM  ZTREQST
                            WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO.

      IF SY-SUBRC NE 0.
        MESSAGE E062 WITH  ZSREQHD-ZFOPNNO.
      ELSE.
        IF W_ZFREQNO IS INITIAL.
          IF ZSREQHD-ZFREQNO IS INITIAL.  " 관리번호가 입력되지 않?
            MESSAGE E062 WITH  ZSREQHD-ZFOPNNO.
          ENDIF.
        ELSE.
          ZSREQHD-ZFREQNO = W_ZFREQNO.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
* 수입의뢰 Header Table
  CALL FUNCTION 'ZIM_GET_REQ_DOC_HEADER'
         EXPORTING
               ZFREQNO           =     ZSREQHD-ZFREQNO
*              ZFAMDNO           =     ZTREQST-ZFAMDNO
         IMPORTING
               W_ZTREQHD         =      ZTREQHD
               W_ZTREQST         =      ZTREQST
*         TABLES
*               IT_ZSREQIL        =      IT_ZSREQIL
*               IT_ZSREQIL_ORG    =      IT_ZSREQIL_ORG
*               IT_ZTREQORJ       =      IT_ZTREQORJ
*               IT_ZTREQORJ_ORG   =      IT_ZTREQORJ_ORG
         EXCEPTIONS
               NOT_FOUND         =    4
               NOT_INPUT         =    8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E018 WITH ZSREQHD-ZFREQNO.
    WHEN 8.
      MESSAGE E019.
  ENDCASE.

  IF NOT ( ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU' ).
    MESSAGE E635 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
  ENDIF.

*>> 삼국무역...
  IF ZTREQHD-ZFTRIPLE EQ 'X'.
    MESSAGE E535 WITH ZTREQHD-ZFREQNO.
  ENDIF.

*>> 사후관리여부...
  IF ZTREQHD-ZFCLOSE EQ 'X'.
    MESSAGE E354 WITH ZTREQHD-ZFREQNO.
  ENDIF.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
    MESSAGE E963.
  ENDIF.

  IF ZTIMIMG00-ZFEXFIX EQ 'X'.
*>> 환율차이 오류 메시지.
    IF ZTREQHD-WAERS NE 'KRW' AND
       ( ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU' ).
      SELECT SINGLE * FROM EKKO
                      WHERE EBELN  EQ  ZTREQHD-EBELN.
      IF EKKO-KUFIX NE 'X'.
        MESSAGE E528 WITH ZTREQHD-EBELN.
      ELSE.
        IF ZTREQHD-KURSF NE EKKO-WKURS.
          MESSAGE E614 WITH ZTREQHD-EBELN EKKO-WKURS
                            ZTREQHD-ZFREQNO ZTREQHD-KURSF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  REFRESH : IT_ZFREQNO, IT_LOCKED, IT_ZSCIVIT, IT_ZSIVIT.
*  IF SY-TCODE EQ 'ZIM31'.
*    PERFORM  P2000_REQ_TO_CC_MOVE.           ">수입의뢰 ===> 통관요청.
*  ELSE.
  PERFORM  P2000_REQ_DATA_MOVE.            " B/L DATA MOVE.
*  ENDIF.
  ANTWORT = 'Y'.


ENDMODULE.                 " FIELD_MATCH_CHECK_SCR3501  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_VALUE_CHECK_SCR0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INPUT_VALUE_CHECK_SCR0110 INPUT.

  CHECK W_STATUS NE C_REQ_D.
  DATA : WL_ZFNEWTM LIKE ZTBL-ZFNEWTM,
         WL_ZFTOVLM LIKE ZTBL-ZFTOVLM.

*>> 이 모듈 전체 수정 <= JSY 20021009 한수원.
* 포장수.
*  IF ZTBL-ZFPKCN IS INITIAL.
*    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFPKCN'.
*  ENDIF.
*  IF ZTBL-ZFPKCNM IS INITIAL.
*    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFPKCNM'.
*  ENDIF.

*  IF SY-TCODE EQ 'ZIM222'.
  PERFORM P3000_WIGHT_INPUT_CHECK.
*ENDIF.

*>> 단위 체크.(해상: TON, 항공: LB, KG)
  IF NOT ZTBL-ZFNEWTM IS INITIAL.
*> 단위변환.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
         EXPORTING
              INPUT          = ZTBL-ZFNEWTM
         IMPORTING
              OUTPUT         = WL_ZFNEWTM
         EXCEPTIONS
              UNIT_NOT_FOUND = 4.

    IF SY-SUBRC <> 0.
      MESSAGE E432(ZIM1) WITH 'Shipping weight'.
    ENDIF.

    IF ZTBL-ZFVIA EQ 'AIR'.
      IF WL_ZFNEWTM NE 'LB' AND  WL_ZFNEWTM NE 'KG'.
        MESSAGE E431(ZIM1) WITH 'Air' 'Shipping weight' 'LB or KG'.
      ENDIF.
    ELSEIF ZTBL-ZFVIA EQ 'VSL'.
      IF WL_ZFNEWTM NE 'TON'.
        MESSAGE E431(ZIM1) WITH 'Ocear' 'Shipping weight' 'TON'.
      ENDIF.
    ENDIF.
    ZTBL-ZFTOWTM = ZTBL-ZFNEWTM.  "계산중량단위는 항상 선적중량단위.
  ENDIF.
*>> 용적단위 체크( CBM, KG )
  IF NOT ZTBL-ZFTOVLM IS INITIAL .
*> 단위변환.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
         EXPORTING
              INPUT          = ZTBL-ZFTOVLM
         IMPORTING
              OUTPUT         = WL_ZFTOVLM
         EXCEPTIONS
              UNIT_NOT_FOUND = 4.

    IF SY-SUBRC <> 0.
      MESSAGE E432(ZIM1) WITH 'Shipping measurement'.
    ENDIF.

    IF WL_ZFTOVLM NE 'CBM'       AND
       WL_ZFTOVLM NE 'KG'        AND
       WL_ZFTOVLM NE 'LB'.
      MESSAGE E431(ZIM1) WITH
              '' 'Shipping measurement' 'CBM, LB or KG'.
    ENDIF.
  ENDIF.

* Chargeable Weight (계산).
  IF ZTBL-ZFTOWT IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFTOWT'.
    IF NOT WL_ZFTOVLM IS INITIAL AND
       NOT WL_ZFNEWTM IS INITIAL.

      DATA : WL_WGHT LIKE ZTBL-ZFNEWT.

*>> 용적을 중량단위로 환산.
      CASE WL_ZFNEWTM.
        WHEN 'TON'.
          IF WL_ZFTOVLM EQ 'CBM'. "( 1cbm = 1t )
            WL_WGHT = ZTBL-ZFTOVL.
          ELSEIF WL_ZFTOVLM EQ 'KG'. "( 1kg = 0.001t )
            WL_WGHT = ZTBL-ZFTOVL / 1000 .
          ELSEIF WL_ZFTOVLM EQ 'LB'.
            WL_WGHT = ( ZTBL-ZFTOVL * 100 ) / 220459.
          ENDIF.

        WHEN 'LB'.
          IF WL_ZFTOVLM EQ 'CBM'. " ( 1t = 2204.59lb )
            WL_WGHT = ( ZTBL-ZFTOVL / 100 ) * 220459 .
          ELSEIF WL_ZFTOVLM EQ 'KG'. "( 1kg = 2.20459lb )
            WL_WGHT = ( ZTBL-ZFTOVL / 100000 ) * 220459 .
          ELSEIF WL_ZFTOVLM EQ 'LB'.
            WL_WGHT = ZTBL-ZFTOVL.
          ENDIF.

        WHEN 'KG'.
          IF WL_ZFTOVLM EQ 'CBM'. " ( 1t = 1000kg )
            WL_WGHT = ZTBL-ZFTOVL * 1000 .
          ELSEIF WL_ZFTOVLM EQ 'KG'.
            WL_WGHT = ZTBL-ZFTOVL.
          ELSEIF WL_ZFTOVLM EQ 'LB'.
            WL_WGHT = ( ZTBL-ZFTOVL * 100000 ) / 220459 .
          ENDIF.

      ENDCASE.

*>> 중량과 용적중 큰것을 계산중량에 넣는다.
      IF ZTBL-ZFNEWT GT WL_WGHT.
        ZTBL-ZFTOWT = ZTBL-ZFNEWT.
      ELSE.
        ZTBL-ZFTOWT = WL_WGHT.
      ENDIF.

    ENDIF.
  ENDIF.


ENDMODULE.                 " INPUT_VALUE_CHECK_SCR0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_DATE INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF ZTCIVHD-ZFCIDT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCIVHD' 'ZFCIDT'.
  ENDIF.

** Changed by Furong on 12/03/09
  ZTCIVHD-ZFGSDT = ZTCIVHD-ZFCIDT.
** End of change

ENDMODULE.                 " CHECK_INPUT_DATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFCIVRN_CHECK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZFCIVRN_CHECK_SCRCOM INPUT.

  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZSCIVHD-EBELN   IS INITIAL AND   " P/O NO를 입력하지 않을 경우.
     ZSCIVHD-ZFREQNO IS INITIAL AND   " 관리번호가 입력하지 않을 경우.
*     ZSCIVHD-ZFOPNNO IS INITIAL AND   " 문서번호가 입력하지 않을 경우.
     ZSCIVHD-ZFCIVRN IS INITIAL.
    MESSAGE E066.
  ENDIF.

  IF NOT ZSCIVHD-EBELN IS INITIAL.   " 관리번호가 입력하지 않을 경?
* P/O NO에 Count
    SELECT COUNT( DISTINCT ZFCIVRN ) INTO  W_COUNT
                      FROM  ZTCIVIT
                      WHERE EBELN EQ ZSCIVHD-EBELN.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E632 WITH ZSCIVHD-EBELN.
      WHEN 1.
        SELECT ZFCIVRN INTO  ZSCIVHD-ZFCIVRN UP TO 1 ROWS
                       FROM  ZTCIVIT
                       WHERE EBELN EQ ZSCIVHD-EBELN.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_PO_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* 물대 문서 READ PERFORM .
    PERFORM   P1000_COMMERCIAL_DOC_READ.
    EXIT.
  ENDIF.

  IF NOT ZSCIVHD-ZFREQNO IS INITIAL.      " 수입의뢰관리번호.
* 수입의뢰 NO에 Count
    SELECT COUNT( DISTINCT ZFCIVRN ) INTO  W_COUNT
                      FROM  ZTCIVIT
                      WHERE ZFREQNO EQ ZSCIVHD-ZFREQNO.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E634 WITH ZSCIVHD-ZFREQNO.
      WHEN 1.
        SELECT ZFCIVRN INTO  ZSCIVHD-ZFCIVRN UP TO 1 ROWS
                       FROM  ZTCIVIT
                       WHERE ZFREQNO EQ ZSCIVHD-ZFREQNO.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_IV_DOC_ITEM_SELECT1.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
    PERFORM   P1000_COMMERCIAL_DOC_READ.
    EXIT.
  ENDIF.

  IF NOT ZSCIVHD-ZFCIVRN IS INITIAL.
    ANTWORT = 'N'.
    W_COUNT = '1'.
* 물대 문서 READ PERFORM .
    PERFORM   P1000_COMMERCIAL_DOC_READ.
  ENDIF.

ENDMODULE.                 " ZFCIVRN_CHECK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_BUPLA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_BUPLA INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF ZTIMIMG00-ZFBPLK EQ 'X'.
    IF ZTCIVHD-BUPLA IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCIVHD' 'BUPLA'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_BUPLA  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR0021  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR0021 INPUT.

  IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' OR OK-CODE EQ 'DDCL' ).
    EXIT.
  ENDIF.

  IF ZTIVHST-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BUDAT'.
  ENDIF.
  IF ZTIVHST-BLDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BLDAT'.
  ENDIF.
  IF ZTIVHST-BWART IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BWART'.
  ENDIF.

*> 증빙일과 면허일 체크.
  IF NOT ( ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU' ).
    IF NOT ZTIVHST-BLDAT IS INITIAL.
      IF  ZTIVHST-BLDAT LT W_ZFIDSDT.
        MESSAGE  E560.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ZTIVHST-BWART IS INITIAL.
    MOVE ZTIMIMG00-ZFMVTY1  TO ZTIVHST-BWART.
    IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'Y'. "무환수출입.
      MOVE ZTIMIMG00-ZFMVTY2  TO ZTIVHST-BWART.
    ENDIF.
    IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'N'. "무환.
      MOVE ZTIMIMG00-ZFMVTY3  TO ZTIVHST-BWART.
    ENDIF.
  ELSE.
    SELECT SINGLE *
      FROM T156
     WHERE BWART = ZTIVHST-BWART.
    IF SY-SUBRC NE 0.
      MESSAGE E799.
      EXIT.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR0021  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0021  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0021 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0021-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0021  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_GR_MENGE_SCR0021  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_GR_MENGE_SCR0021 INPUT.

  IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' OR OK-CODE EQ 'DDCL' ).
    EXIT.
  ENDIF.

  READ TABLE IT_ZSIVHSTIT INDEX TC_0021-CURRENT_LINE.
  MOVE: SY-SUBRC TO W_OLD_SUBRC,
        SY-TABIX TO W_TABIX.

*-----------------------------------------------------------------------
*>> 입고 구분자 체크.
  IF ZSIVHSTIT-UMSON EQ 'X'.
    IF ZSIVHSTIT-GRMENGE IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIVHSTIT' 'GRMENGE'.
    ENDIF.
    IF ZSIVHSTIT-LGORT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIVHSTIT' 'LGORT'.
    ENDIF.
*> 수량체크.
    W_MENGE = ZSIVHSTIT-CCMENGE - ZSIVHSTIT-GRTOTMN.
    WRITE :  W_MENGE TO W_AMTTXT1 UNIT ZSIVIT-MEINS .
    W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
    WRITE : ZSIVIT-GRMENGE TO W_AMTTXT2 UNIT ZSIVIT-MEINS .
    W_AMTLEN2 = STRLEN( W_AMTTXT2 ).
    IF ZSIVHSTIT-GRMENGE GT W_MENGE.
      PERFORM P2000_NO_INPUT  USING  'ZSIVHSTIT' 'GRMENGE'
                                     DFIES-SCRTEXT_M W_SUBRC.
      MESSAGE E642 WITH W_AMTTXT2(W_AMTLEN2) W_AMTTXT1(W_AMTLEN1).
    ENDIF.
  ENDIF.

* MOVE-CORRESPONDING ZSIVHSTIT TO IT_ZSIVHSTIT.
  MOVE : ZSIVHSTIT-UMSON    TO    IT_ZSIVHSTIT-UMSON,
         ZSIVHSTIT-GRMENGE  TO    IT_ZSIVHSTIT-GRMENGE,
         ZSIVHSTIT-LGORT    TO    IT_ZSIVHSTIT-LGORT,
         ZSIVHSTIT-LICHA    TO    IT_ZSIVHSTIT-LICHA.

  MOVE W_ROW_MARK              TO IT_ZSIVHSTIT-ZFMARK.

  IF W_OLD_SUBRC EQ 0.
    MODIFY IT_ZSIVHSTIT   INDEX TC_0021-CURRENT_LINE.
  ELSE.
    APPEND IT_ZSIVHSTIT.
  ENDIF.

ENDMODULE.                 " CHECK_GR_MENGE_SCR0021  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0021  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0021 INPUT.

  IF OK-CODE EQ 'CANC' OR OK-CODE EQ 'NO'.
    ANTWORT = 'N'.
    SET SCREEN 0.   LEAVE SCREEN.
  ELSEIF OK-CODE EQ 'YES'.
*     IF SY-DYNNR EQ '0021'.
*        READ TABLE IT_ZSIVHST WITH KEY ZFMARK = 'X'.
*        IF SY-SUBRC EQ 0.
*           MOVE-CORRESPONDING IT_ZSIVHST TO ZTIVHST.
*        ELSE.
*           MESSAGE E962.
*        ENDIF.
*     ENDIF.
    ANTWORT = 'Y'.
    SET SCREEN 0.   LEAVE SCREEN.
  ELSE.
    ANTWORT = 'N'.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR0021  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0031  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0031 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0031-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0031  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0031_MARK_TC_0031  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0031_MARK_TC_0031 INPUT.

  READ TABLE IT_ZSIVHST INDEX TC_0031-CURRENT_LINE.
  IF SY-SUBRC EQ 0.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIVHST-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIVHST-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIVHST  INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0031_MARK_TC_0031  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0031  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0031 INPUT.

  IF OK-CODE EQ 'CANC' OR OK-CODE EQ 'NO'.
    ANTWORT = 'N'.
    SET SCREEN 0.   LEAVE SCREEN.
  ELSEIF OK-CODE EQ 'YES'.
    IF SY-DYNNR EQ '0031'.
      READ TABLE IT_ZSIVHST WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING IT_ZSIVHST TO ZTIVHST.
      ELSE.
        MESSAGE E962.
      ENDIF.
    ENDIF.
    ANTWORT = 'Y'.
    SET SCREEN 0.   LEAVE SCREEN.
  ELSE.
    ANTWORT = 'N'.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR0031  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FILED_SCR8210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FILED_SCR8210 INPUT.

  CHECK W_STATUS NE C_REQ_D.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTPMTHD-BUKRS.
  IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
    IF ZTPMTHD-ZFDHDOC IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'ZFDHDOC'.
    ENDIF.

    IF NOT ( ZTPMTHD-ZFDHDOC EQ 'DOANTC' OR
             ZTPMTHD-ZFDHDOC EQ 'DISCHG' OR
             ZTPMTHD-ZFDHDOC EQ 'LDANTC' ).
      MESSAGE E914 WITH ZTPMTHD-ZFDHDOC.
    ENDIF.
    CASE ZTPMTHD-ZFLCKN.
      WHEN '1'.
        IF ZTPMTHD-ZFDHDOC NE 'DOANTC'.
          MESSAGE E914 WITH ZTPMTHD-ZFDHDOC.
        ENDIF.
      WHEN '2' OR '3'.
        IF ZTPMTHD-ZFDHDOC NE 'DISCHG'.
          MESSAGE E914 WITH ZTPMTHD-ZFDHDOC.
        ENDIF.
      WHEN '8'.
        IF ZTPMTHD-ZFDHDOC NE 'LDANTC'.
          MESSAGE E914 WITH ZTPMTHD-ZFDHDOC.
        ENDIF.
    ENDCASE.
    IF ZTPMTHD-ZFDHDOC EQ 'DISCHG'.
      IF ZTPMTHD-ZFBKCHP IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'ZFBKCHP'.
      ENDIF.
    ENDIF.
  ELSE.
    IF ZTPMTHD-ZFLCKN EQ '2' OR ZTPMTHD-ZFLCKN EQ '3'.
      IF ZTPMTHD-ZFBKCHP IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'ZFBKCHP'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ZTPMTHD-ZFEXRT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'ZFEXRT'.
  ENDIF.

  IF ZTPMTHD-FFACT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'FFACT'.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_FILED_SCR8210  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_BL_TO_POINT_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_BL_TO_POINT_SCR0102 INPUT.
  DATA : L_DISPLAY.

  REFRESH : IT_BLSDP_HELP.
  SELECT *
*          INTO CORRESPONDING FIELDS OF TABLE IT_BLSDP_HELP
         FROM   ZTIMIMG08
         WHERE  ZFCDTY   EQ   '012'.
    MOVE : ZTIMIMG08-ZFCD   TO   IT_BLSDP_HELP-ZFBLSDP,
           ZTIMIMG08-ZFCDNM TO   IT_BLSDP_HELP-ZFCDNM.
    APPEND IT_BLSDP_HELP.
  ENDSELECT.

  IF SY-SUBRC NE 0.
    MESSAGE S406.
    EXIT.
  ENDIF.

  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.

  WINDOW_TITLE = 'B/L 송부처'.
  CONCATENATE WINDOW_TITLE '코드 HELP' INTO WINDOW_TITLE
              SEPARATED BY SPACE.

  IF W_STATUS EQ C_REQ_D.
    L_DISPLAY = 'X'.
  ELSE.
    CLEAR: L_DISPLAY.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
*                RETFIELD        = 'OTYPE'
               RETFIELD        = 'ZFBLSDP'
               DYNPPROG        = DYNPROG
               DYNPNR          = DYNNR
               DYNPROFIELD     = 'ZTBL-ZFBLSDP'
               WINDOW_TITLE    = WINDOW_TITLE
               VALUE_ORG       = 'S'
               DISPLAY         = L_DISPLAY
       TABLES
               VALUE_TAB       = IT_BLSDP_HELP
       EXCEPTIONS
               PARAMETER_ERROR = 1
               NO_VALUES_FOUND = 2
               OTHERS          = 3.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

ENDMODULE.                 " HELP_BL_TO_POINT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_AMOUNT_TC_7412  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_AMOUNT_TC_7412 INPUT.

* READ TABLE IT_ZSIDSHSD  WITH KEY ZSIDSHSD(24)  BINARY SEARCH.
  READ TABLE IT_ZSIDSHSD  INDEX  TC_7412-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
    MOVE : ZSIDSHSD-NETPR  TO IT_ZSIDSHSD-NETPR,
           ZSIDSHSD-ZFAMT  TO IT_ZSIDSHSD-ZFAMT.

*     PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIDSHSD-ZFAMT
*                                             ZTIDS-ZFSTAMC
*                                             IT_ZSIDSHSD-ZFAMT.
*     PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIDSHSD-NETPR
*                                             ZTIDS-ZFSTAMC
*                                             IT_ZSIDSHSD-NETPR.
*
*     PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHSD-ZFAMT
*                                             ZTIDS-ZFSTAMC.
*     PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSIDSHSD-NETPR
*                                             ZTIDS-ZFSTAMC.

    MODIFY IT_ZSIDSHSD INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_AMOUNT_TC_7412  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_TC_7412_MARK_SCR7412  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_TC_7412_MARK_SCR7412 INPUT.

  READ TABLE IT_ZSIDSHSD  INDEX  TC_7412-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIDSHSD-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIDSHSD-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIDSHSD  INDEX  W_TABIX.
  ENDIF.
ENDMODULE.                 " SET_TC_7412_MARK_SCR7412  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_BL_DOCUMENT_SCR3100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_BL_DOCUMENT_SCR3100 INPUT.

  CLEAR : ZTIV, *ZTIV.
  REFRESH : IT_ZSIVIT, IT_ZSIVCD, IT_ZSIVIT_ORG, IT_ZSIVIT_ORG.

  PERFORM  P2000_OK_CODE_PROCESS.

* 입력값 CHECK.
  IF ZSREQHD-ZFHBLNO IS INITIAL.        " B/L NO를 입력하지 않을 경?
    IF ZSREQHD-ZFBLNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E304.
    ENDIF.
  ELSE.
* B/L NO에 Count
    IF ZSREQHD-ZFBLNO IS INITIAL.
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTBL
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
      CASE W_COUNT.
        WHEN 0.     MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
        WHEN 1.
          SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
                        FROM ZTBL
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
            EXIT.
          ENDSELECT.
        WHEN OTHERS.
          PERFORM P2000_BL_DOC_ITEM_SELECT.
          IF ANTWORT NE 'Y'.
            LEAVE TO SCREEN 3100.              "NCW 추가
            EXIT.
          ENDIF.

          PERFORM P2000_SEARCH_FIELD_MOVE.
      ENDCASE.
    ENDIF.
  ENDIF.

* B/L READ PERFORM ?
  ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
  PERFORM   P1000_READ_BL_DOC.

*>> B/L 납품 완료지시자.
  IF ZTBL-ZFELIKZ EQ 'X'.
    MESSAGE W404 WITH ZTBL-ZFBLNO.
  ENDIF.

*>> 양도/양수 구분자..
  IF NOT ZSIV-ZFYSDST IS INITIAL.
    IF ZSIV-ZFYSDST EQ 'L'.     "> 양도.
      IF ZTBL-ZFRENT NE 'X'.   "> B/L 양도 구분자.
        MESSAGE E503 WITH ZTBL-ZFBLNO 'transfer'.
      ENDIF.
    ELSE.
      IF ZTBL-ZFPOTY NE 'R'.
        MESSAGE E503 WITH ZTBL-ZFBLNO 'Acquistion by transfer'.
      ENDIF.
    ENDIF.
    PERFORM  P2000_BL_TO_CC_DATA.   ">> B/L DATA ---> CC DATA.
    EXIT.
  ENDIF.
  IF NOT ( ZTBL-ZFVIA EQ 'VSL' AND ZTBL-ZFSHTY EQ 'B' ). "> Bulk
    PERFORM  P2000_BL_TO_CC_DATA.
    EXIT.
  ENDIF.

*>> 수입 IMG CONFIG.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFCGYN NE 'X'.
    PERFORM  P2000_BL_TO_CC_DATA.
    EXIT.
  ENDIF.

  ANTWORT = 'N'.
  W_COUNT = 0.
  SELECT H~ZFCGNO INTO ZSCGHD-ZFCGNO
         FROM   ZTCGHD AS H  INNER JOIN ZTCGIT AS I
         ON     H~ZFCGNO   EQ   I~ZFCGNO
         WHERE  I~ZFBLNO   EQ   ZTBL-ZFBLNO
         GROUP BY H~ZFCGNO.
    ADD 1 TO W_COUNT.
  ENDSELECT.
  CASE W_COUNT.
*>>>>>>>> B/L DATA ---> CC DATA.
    WHEN 0.
      IF ZTBL-ZFVIA EQ 'VSL' AND ZTBL-ZFSHTY EQ 'B'. "> Bulk
        MESSAGE  E409 WITH ZTBL-ZFBLNO.
      ELSE.
        PERFORM  P2000_BL_TO_CC_DATA.
      ENDIF.
      EXIT.
    WHEN 1.
      SELECT H~ZFCGNO INTO ZSCGHD-ZFCGNO UP TO 1 ROWS
             FROM   ZTCGHD AS H  INNER JOIN ZTCGIT AS I
             ON     H~ZFCGNO   EQ   I~ZFCGNO
             WHERE  I~ZFBLNO   EQ   ZTBL-ZFBLNO
             GROUP BY H~ZFCGNO.
      ENDSELECT.
    WHEN OTHERS.
      PERFORM P2000_CARGO_DOC_SELECT.
      IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
      PERFORM P2000_SEARCH_FIELD_MOVE.
  ENDCASE.

  PERFORM   P1000_READ_CARGO_WORK_DOC.
*>>>>>>>> CARGO WORK DATA ---> CC DATA.
  PERFORM   P2000_CG_TO_CC_DATA.


ENDMODULE.                 " GET_BL_DOCUMENT_SCR3100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0112_MARK_TC_0112  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0112_MARK_TC_0112 INPUT.

  READ TABLE IT_ZSBLIT   INDEX TC_0112-CURRENT_LINE.
  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSBLIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSBLIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSBLIT INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0112_MARK_TC_0112  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBLIT_UPDATE_SCR0112  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSBLIT_UPDATE_SCR0112 INPUT.
* Display Mode Module EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

* Internal Table Read
  READ TABLE IT_ZSBLIT   INDEX TC_0112-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF NOT ZTBL-ZFPOTY IS INITIAL.
    MOVE ZTBL-ZFPOTY   TO  ZSBLIT-ZFPOTY.
  ENDIF.

*-------------<    CASE : Non-Monetary    >-----------------*
  IF NOT ZSBLIT-ZFPOTY IS INITIAL OR ZTBL-ZFMATGB EQ '1'.

    IF NOT ZSBLIT-EBELN  IS INITIAL  AND NOT ZSBLIT-EBELP IS INITIAL
       AND ZSBLIT-ZFPOTY IS INITIAL.
      PERFORM  P2000_GET_PO_ITEM_BL.
      IF W_SY_SUBRC EQ 0.
        MODIFY IT_ZSBLIT  INDEX W_TABIX.
      ELSE.
        MOVE : SY-UNAME    TO   IT_ZSBLIT-ERNAM,
               SY-DATUM    TO   IT_ZSBLIT-CDAT,
               SY-UNAME    TO   IT_ZSBLIT-UNAM,
               SY-DATUM    TO   IT_ZSBLIT-UDAT.

        APPEND IT_ZSBLIT.
      ENDIF.
      EXIT.
    ENDIF.

*>> MATERIAL CODE CHECK.
    IF ZSBLIT-MATNR EQ SPACE AND ZSBLIT-TXZ01 EQ SPACE.
      PERFORM  P2000_NO_INPUT  USING 'ZSBLIT' 'MATNR'
                                     DFIES-SCRTEXT_M W_SUBRC.
      MESSAGE E083(ME) WITH 'Material code' 'Text'.
    ENDIF.

*>> PLANT CHECK.
    IF ZSBLIT-WERKS EQ SPACE.
      MESSAGE S083(ME) WITH 'Plant' SPACE.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'WERKS'.
    ELSE.
      TRTYP = 'V'.       ">Transaction Type.
      PERFORM WERKS(SAPFMMEX) USING ZSBLIT-WERKS SPACE  'X' TRTYP
                              CHANGING T001W T001K T001.
    ENDIF.
*>> Material Check.
    IF ZSBLIT-MATNR NE SPACE.
      CALL FUNCTION 'VENDOR_MASTER_DATA_SELECT_00'
           EXPORTING
                I_LFA1_LIFNR     = ZTBL-LIFNR
                I_LFM1_EKORG     = ZTBL-EKORG
                I_LFM2_WERKS     = ZSBLIT-WERKS
                I_DATA           = 'X'
                I_PARTNER        = ' '
           IMPORTING
                A_LFM2           = LFM2
           EXCEPTIONS
                VENDOR_NOT_FOUND = 01.
      IF SY-SUBRC NE 0.
        MESSAGE E027(06) WITH ZTBL-LIFNR ZTBL-EKORG.
      ENDIF.

      CALL FUNCTION 'MEX_CHECK_COMPANY_CODE_ALLOWED'
           EXPORTING
                IM_HEADER_BUKRS   = ZTBL-BUKRS
                IM_ITEM_BUKRS     = T001-BUKRS
                IM_BSTYP          = 'F'
                IM_PURCHORG_BUKRS = T024E-BUKRS.

      CALL FUNCTION 'VENDOR_MASTER_DATA_SELECT_01'
           EXPORTING
                I_LAND1     = T001W-LAND1
                I_ZONE1     = T001W-ZONE1
                I_LIFNR     = ZTBL-LIFNR
           EXCEPTIONS
                NOT_ALLOWED = 01.
      IF SY-SUBRC NE 0.
        MESSAGE W473(06) WITH ZTBL-LIFNR T001W-ZONE1 T001W-LAND1.
      ENDIF.

      CLEAR: MTCOM, MT06E, *MT06E, MT06B.
      MTCOM-KENNG = 'MT06E'.
      MTCOM-MATNR = ZSBLIT-MATNR.
      MTCOM-WERKS = ZSBLIT-WERKS.
      MTCOM-SPRAS = T001W-SPRAS.
      MTCOM-ALAND = ZTBL-ZFCARC.
      MTCOM-PSTAT = 'EBD'.
      MTCOM-KZSPR = 'X'.
      MTCOM-SPR_MEINS = 'X'.
      MTCOM-KZMPN = 'X'.
      MTCOM-XVKBW = T001K-XVKBW.
      IF TCURM IS INITIAL.               "Konsi
        SELECT SINGLE * FROM TCURM.      "Konsi
      ENDIF.
      PERFORM LESEN_MATERIAL_NEU(SAPFMMEX) USING MTCOM 'F'
                                                 MT06E MTCOR.
      IF ZSBLIT-TXZ01 IS INITIAL.
        MOVE : MT06E-MAKTX      TO     ZSBLIT-TXZ01.
      ENDIF.
      MOVE MT06E-MEINS      TO     ZSBLIT-MEINS.
      MOVE MT06E-PEINH      TO     ZSBLIT-PEINH.
      IF ZSBLIT-MATKL IS INITIAL.
        MOVE MT06E-MATKL      TO     ZSBLIT-MATKL.
      ELSE.
        IF ZSBLIT-MATKL NE MT06E-MATKL.
          MESSAGE E531 WITH ZSBLIT-MATNR MT06E-MATKL ZSBLIT-MATKL.
        ENDIF.
      ENDIF.

      IF ZSBLIT-STAWN IS INITIAL.
        MOVE MT06E-STAWN      TO     ZSBLIT-STAWN.
      ENDIF.
*<>>Latest Purchase Unit Price.
      IF ZSBLIT-NETPR IS INITIAL.
        CALL FUNCTION 'ZIM_MAT_PURCH_PRICE_EKPO'
             EXPORTING
                  IP_WERKS = ZSBLIT-WERKS
                  IP_MATNR = ZSBLIT-MATNR
                  IP_EKORG = ZTBL-EKORG
                  IP_LIFNR = ZTBL-LIFNR
                  IP_GUBUN = 'CF'
                  IP_BEDAT = SY-DATUM
             IMPORTING
                  EP_PRICE = ZSBLIT-NETPR
                  EP_PEINH = ZSBLIT-PEINH
                  EP_BPRME = ZSBLIT-BPRME.

        IF ZSBLIT-NETPR IS INITIAL.
          MOVE : MT06E-PEINH      TO     ZSBLIT-PEINH,
                 MT06E-MEINS      TO     ZSBLIT-BPRME.
          IF MT06E-VPRSV EQ 'S'.
            MOVE MT06E-STPRS     TO     ZSBLIT-NETPR.
          ELSE.
            MOVE MT06E-VERPR     TO     ZSBLIT-NETPR.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*>> Storage Location Check.
    IF ZSBLIT-LGORT NE SPACE.
      PERFORM LGORT(SAPFMMEX) USING ZSBLIT-LGORT ZSBLIT-WERKS
                              CHANGING T001L.
    ENDIF.

    IF ZSBLIT-TXZ01 IS INITIAL.
      MESSAGE S083(ME) WITH 'description' SPACE.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'TXZ01'.
    ENDIF.
*>> B/L Quantity.
    IF ZSBLIT-BLMENGE IS INITIAL.
      MESSAGE S083(ME) WITH 'B/L quantity' SPACE.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'BLMENGE'.
    ENDIF.
*>>> B/L Unit..
    IF ZSBLIT-MEINS IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'MEINS'.
    ENDIF.
*>> Material Group.
    IF ZSBLIT-MATKL IS INITIAL.
      MESSAGE S083(ME) WITH 'material group' SPACE.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'MATKL'.
    ENDIF.
*>> B/L Net Price.
    IF ZSBLIT-NETPR IS INITIAL.
      MESSAGE S083(ME) WITH 'Unit price' SPACE.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'NETPR'.
    ENDIF.
*>> Unit of price.
    IF ZSBLIT-PEINH IS INITIAL.
      MESSAGE S083(ME) WITH 'Price unit' SPACE.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'PEINH'.
    ENDIF.
*>> Order Unit of price
    IF ZSBLIT-BPRME IS INITIAL.
      MESSAGE S083(ME) WITH 'Order price unit' SPACE.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'BPRME'.
    ENDIF.
*>> H/S CODE CHECK.
    IF ZSBLIT-STAWN EQ SPACE.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'STAWN'.
    ENDIF.

    MOVE-CORRESPONDING ZSBLIT  TO IT_ZSBLIT.
    CLEAR : IT_ZSBLIT-ZFREQNO, IT_ZSBLIT-ZFITMNO, IT_ZSBLIT-ZFSHNO.
*----> Change....
    IF W_SY_SUBRC EQ 0.
      MODIFY IT_ZSBLIT  INDEX W_TABIX.
    ELSE.
*----> End Addition
      MOVE : SY-UNAME    TO   IT_ZSBLIT-ERNAM,
             SY-DATUM    TO   IT_ZSBLIT-CDAT,
             SY-UNAME    TO   IT_ZSBLIT-UNAM,
             SY-DATUM    TO   IT_ZSBLIT-UDAT.

      APPEND IT_ZSBLIT.
    ENDIF.
    EXIT.
  ENDIF.
*-----------------<    Non-Monetary Transaction    >-------------------*

*-----------------------------------------------------------------------
*-------------------<    Monetary Transaction    >---------------------*
*-----------------------------------------------------------------------
  CHECK NOT ZSBLIT-ZFREQNO IS INITIAL.
  IF W_SY_SUBRC NE 0.
    IT_ZSBLIT_MUL[]  =  IT_ZSBLIT[].
  ENDIF.
  MOVE-CORRESPONDING ZSBLIT  TO IT_ZSBLIT.

*>> Not Input Import Request Document No.
  IF ZSBLIT-ZFREQNO IS INITIAL.
    MESSAGE W019.   EXIT.
  ELSE.
    SELECT SINGLE * FROM ZTREQHD
                    WHERE ZFREQNO EQ IT_ZSBLIT-ZFREQNO.
    IF SY-SUBRC NE 0.
      CLEAR : ZSBLIT.
      MESSAGE W018 WITH IT_ZSBLIT-ZFREQNO.
      EXIT.
    ENDIF.
*>> Triangle Trade Marking Yes/No
    IF ZTREQHD-ZFTRIPLE EQ 'X'.
      MESSAGE W535 WITH ZTREQHD-ZFREQNO.
      CLEAR : ZSBLIT.  EXIT.
    ENDIF.
*>> L/C Close Check
    IF ZTREQHD-ZFCLOSE EQ 'X'.
      MESSAGE W535 WITH ZTREQHD-ZFREQNO.
      CLEAR : ZSBLIT.  EXIT.
    ENDIF.
    IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
      MESSAGE W317 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
      CLEAR : ZSBLIT.  EXIT.
    ENDIF.

*>> Currency Check.
    IF ZTBL-ZFBLAMC IS INITIAL.
      ZTBL-ZFBLAMC = ZTREQHD-WAERS.
    ENDIF.
    IF ZTBL-ZFBLAMC NE ZTREQHD-WAERS.
      MESSAGE E379 WITH ZTBL-ZFBLAMC IT_ZSBLIT-ZFREQNO ZTREQHD-WAERS.
    ENDIF.
*>> Vendor Check.
    IF ZTBL-LIFNR IS INITIAL.
      ZTBL-LIFNR  =  ZTREQHD-LIFNR.
    ENDIF.
    IF ZTBL-LIFNR NE ZTREQHD-LIFNR.
      MESSAGE E380 WITH ZTBL-LIFNR IT_ZSBLIT-ZFREQNO ZTREQHD-LIFNR.
    ENDIF.
*>> Beneficiay Check.
    IF ZTBL-ZFBENI IS INITIAL.
      ZTBL-ZFBENI  =  ZTREQHD-ZFBENI.
    ENDIF.
    IF ZTBL-ZFBENI NE ZTREQHD-ZFBENI.
      MESSAGE E381 WITH ZTBL-ZFBENI IT_ZSBLIT-ZFREQNO ZTREQHD-ZFBENI.
    ENDIF.
*>> Company Code Check.
    IF ZTBL-BUKRS IS INITIAL.
      ZTBL-BUKRS  =  ZTREQHD-BUKRS.
    ENDIF.
    IF ZTBL-BUKRS NE ZTREQHD-BUKRS.
      MESSAGE E382 WITH ZTBL-BUKRS IT_ZSBLIT-ZFREQNO ZTREQHD-BUKRS.
    ENDIF.
*>> Purchasing Group, Purchasing Org. Check
    SELECT SINGLE * FROM ZTREQST
                    WHERE ZFREQNO EQ ZTREQHD-ZFREQNO
                    AND   ZFAMDNO EQ '00000'.
    IF ZTBL-EKORG IS INITIAL.
      ZTBL-EKORG  =  ZTREQST-EKORG.
    ENDIF.
    IF ZTBL-EKGRP IS INITIAL.
      ZTBL-EKGRP  =  ZTREQST-EKGRP.
    ENDIF.
*>> Input Import Request Item No.
    IF NOT IT_ZSBLIT-ZFITMNO IS INITIAL.
*-----------------------------------------------------------------------
* < 한수원 TAG 별로 입력가능하게끔 동일 수입의뢰번호로 다중생성 >
* 기존에 해당 수입의뢰 아이템이 입력되었을 경우 체크.
*      READ TABLE  IT_ZSBLIT WITH KEY ZFREQNO = ZSBLIT-ZFREQNO
*                                     ZFITMNO = ZSBLIT-ZFITMNO.
*      IF SY-SUBRC EQ 0 AND W_SY_SUBRC NE 0.
*        CLEAR : ZSBLIT.
*        MESSAGE S358 WITH ZSBLIT-ZFREQNO ZSBLIT-ZFITMNO
*                          IT_ZSBLIT-ZFBLIT.
*        EXIT.
*      ENDIF.
*-----------------------------------------------------------------------
      MOVE-CORRESPONDING ZSBLIT  TO IT_ZSBLIT.

      SELECT SINGLE * INTO CORRESPONDING FIELDS OF IT_ZSBLIT
             FROM   ZTREQIT
             WHERE ZFREQNO EQ ZSBLIT-ZFREQNO
             AND   ZFITMNO EQ ZSBLIT-ZFITMNO.
      IF SY-SUBRC NE 0.
        CLEAR : ZSBLIT.
        MESSAGE W357 WITH IT_ZSBLIT-ZFREQNO IT_ZSBLIT-ZFITMNO.
        EXIT.
      ENDIF.
*++++> P/O DATA Display.
      SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                    BPUMN BPUMZ WERKS LGORT MATKL
             INTO (IT_ZSBLIT-MENGE_PO, IT_ZSBLIT-UEBTO,
                   IT_ZSBLIT-UEBTK,    IT_ZSBLIT-WEPOS,
                   IT_ZSBLIT-ELIKZ,    IT_ZSBLIT-LOEKZ,
                   IT_ZSBLIT-UNTTO,
                   IT_ZSBLIT-BPUMN,    IT_ZSBLIT-BPUMZ,
                   IT_ZSBLIT-WERKS,    IT_ZSBLIT-LGORT,
                   IT_ZSBLIT-MATKL)
             FROM   EKPO
             WHERE  EBELN   EQ   IT_ZSBLIT-EBELN
             AND    EBELP   EQ   IT_ZSBLIT-EBELP.
      IF SY-SUBRC EQ 0.
        IF IT_ZSBLIT-LOEKZ NE SPACE.
          CLEAR : ZSBLIT.
          MESSAGE W069 WITH IT_ZSBLIT-EBELN IT_ZSBLIT-EBELP.
          EXIT.
        ENDIF.
*        IF IT_ZSBLIT-ELIKZ EQ 'X'.
*          CLEAR : ZSBLIT.
*          MESSAGE W359 WITH IT_ZSBLIT-EBELN IT_ZSBLIT-EBELP.
*          EXIT.
*        ENDIF.
      ELSE.
        CLEAR : ZSBLIT.
        MESSAGE W071 WITH IT_ZSBLIT-EBELN IT_ZSBLIT-EBELP.
        EXIT.
      ENDIF.

*----> B/L Item
      IF W_STATUS EQ C_REQ_C.
        SELECT SUM( BLMENGE ) INTO IT_ZSBLIT-ZFBLTOT
               FROM  ZTBLIT
               WHERE ZFBLNO  NE ZTBL-ZFBLNO
               AND   ZFREQNO EQ IT_ZSBLIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSBLIT-ZFITMNO
               AND   BLOEKZ  NE 'X'.
      ELSE.
        SELECT SUM( BLMENGE ) INTO IT_ZSBLIT-ZFBLTOT
               FROM  ZTBLIT
               WHERE ZFREQNO EQ IT_ZSBLIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSBLIT-ZFITMNO
               AND   BLOEKZ  NE 'X'.
      ENDIF.
*>> Inputed B/L Quantity Check
      CLEAR : W_MENGE.
      LOOP  AT  IT_ZSBLIT_MUL  WHERE ZFREQNO EQ IT_ZSBLIT-ZFREQNO
                               AND   ZFITMNO EQ IT_ZSBLIT-ZFITMNO.
        W_MENGE  =  W_MENGE  + IT_ZSBLIT_MUL-BLMENGE.
      ENDLOOP.

*>>> B/L Basic Quantity.

* BEGIN OF HIS20094
* Since it is possible PO contains multiple import request docs,
* get the import documents and check if the total is tally.
      DATA: L_ZTREQIT_MENGE TYPE ZTREQIT-MENGE,
            L_PAR1          TYPE SYMSGV,
            L_PAR2          TYPE SYMSGV.

      CLEAR L_ZTREQIT_MENGE.
      IF IT_ZSBLIT-MENGE <> IT_ZSBLIT-MENGE_PO.
        SELECT SUM( MENGE ) INTO L_ZTREQIT_MENGE
          FROM ZTREQIT
         WHERE EBELN = IT_ZSBLIT-EBELN
           AND EBELP = IT_ZSBLIT-EBELP.
        IF SY-SUBRC = 0.
          IF L_ZTREQIT_MENGE = IT_ZSBLIT-MENGE_PO.
            IT_ZSBLIT-MENGE = IT_ZSBLIT-MENGE_PO.
          ELSEIF L_ZTREQIT_MENGE < IT_ZSBLIT-MENGE_PO.
            CONCATENATE 'Import (' ZSBLIT-ZFREQNO '/' ZSBLIT-ZFITMNO ')'
                   INTO L_PAR1.
            CONCATENATE 'PO (' IT_ZSBLIT-EBELN '/' IT_ZSBLIT-EBELP ')'
                   INTO L_PAR2.
            MESSAGE E000(ZZ)
               WITH L_PAR1 'to' L_PAR2 'Quantity Mismatch'.
          ENDIF.
        ELSE.
          MESSAGE E000(ZZ) WITH 'Error when reading table ZTREQIT'.
        ENDIF.
      ENDIF.
* END OF HIS20094

      IT_ZSBLIT-BLMENGE = IT_ZSBLIT-MENGE - IT_ZSBLIT-ZFBLTOT - W_MENGE.
      IT_ZSBLIT-ZFBLTOT = IT_ZSBLIT-ZFBLTOT + W_MENGE.
      IT_ZSBLIT-EBELN   = IT_ZSBLIT-EBELN.
      IT_ZSBLIT-EBELP   = IT_ZSBLIT-EBELP.
      IF IT_ZSBLIT-BLMENGE LT 0.
        IT_ZSBLIT-BLMENGE = 0.
      ENDIF.
*----> Formal Data Update.
      IF W_SY_SUBRC EQ 0.
        MODIFY IT_ZSBLIT  INDEX W_TABIX.
      ELSE.
        MOVE : SY-UNAME    TO   IT_ZSBLIT-ERNAM,
               SY-DATUM    TO   IT_ZSBLIT-CDAT,
               SY-UNAME    TO   IT_ZSBLIT-UNAM,
               SY-DATUM    TO   IT_ZSBLIT-UDAT.

        APPEND  IT_ZSBLIT.
      ENDIF.
      EXIT.
    ENDIF.

    MOVE-CORRESPONDING ZSBLIT  TO IT_ZSBLIT.
    IF W_SY_SUBRC EQ 0.
      DELETE IT_ZSBLIT  INDEX W_TABIX.
    ENDIF.

*>>Import Request HEADER SELECT.
    SELECT SINGLE * FROM ZTREQHD
                    WHERE ZFREQNO EQ ZSBLIT-ZFREQNO.

    IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
      MESSAGE E317 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
    ENDIF.

*>> Currency Check
    IF ZTBL-ZFBLAMC IS INITIAL.
      ZTBL-ZFBLAMC = ZTREQHD-WAERS.
    ENDIF.
    IF ZTBL-ZFBLAMC NE ZTREQHD-WAERS.
      MESSAGE E379 WITH ZTBL-ZFBLAMC IT_ZSBLIT-ZFREQNO ZTREQHD-WAERS.
    ENDIF.
*>> Vendor Check
    IF ZTBL-LIFNR IS INITIAL.
      ZTBL-LIFNR  =  ZTREQHD-LIFNR.
    ENDIF.
    IF ZTBL-LIFNR NE ZTREQHD-LIFNR.
      MESSAGE E380 WITH ZTBL-LIFNR IT_ZSBLIT-ZFREQNO ZTREQHD-LIFNR.
    ENDIF.
*>> Beneficiay Check
    IF ZTBL-ZFBENI IS INITIAL.
      ZTBL-ZFBENI  =  ZTREQHD-ZFBENI.
    ENDIF.
    IF ZTBL-ZFBENI NE ZTREQHD-ZFBENI.
      MESSAGE E381 WITH ZTBL-ZFBENI IT_ZSBLIT-ZFREQNO ZTREQHD-ZFBENI.
    ENDIF.
*>> Company Code Check
    IF ZTBL-BUKRS IS INITIAL.
      ZTBL-BUKRS  =  ZTREQHD-BUKRS.
    ENDIF.
    IF ZTBL-BUKRS NE ZTREQHD-BUKRS.
      MESSAGE E382 WITH ZTBL-BUKRS IT_ZSBLIT-ZFREQNO ZTREQHD-BUKRS.
    ENDIF.
*>> Purchasing Group, Purchasing Organization Check
    SELECT SINGLE * FROM ZTREQST
                    WHERE ZFREQNO EQ ZTREQHD-ZFREQNO
                    AND   ZFAMDNO EQ '00000'.
    IF ZTBL-EKORG IS INITIAL.
      ZTBL-EKORG  =  ZTREQST-EKORG.
    ENDIF.
    IF ZTBL-EKGRP IS INITIAL.
      ZTBL-EKGRP  =  ZTREQST-EKGRP.
    ENDIF.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBLIT_TMP
             FROM ZTREQIT
             WHERE ZFREQNO EQ ZSBLIT-ZFREQNO.

    LOOP AT IT_ZSBLIT_TMP.
      MOVE-CORRESPONDING  IT_ZSBLIT_TMP   TO  IT_ZSBLIT.
*-----------------------------------------------------------------------
      READ TABLE  IT_ZSBLIT WITH KEY ZFREQNO = IT_ZSBLIT-ZFREQNO
                                     ZFITMNO = IT_ZSBLIT-ZFITMNO.
      IF SY-SUBRC EQ 0 AND W_SY_SUBRC NE 0.
        CLEAR : ZSBLIT.
        MESSAGE S358 WITH IT_ZSBLIT-ZFREQNO IT_ZSBLIT-ZFITMNO
                          IT_ZSBLIT-ZFBLIT.
        CONTINUE.
      ENDIF.
*-----------------------------------------------------------------------
*++++> P/O DATA Display.
      SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                    BPUMZ BPUMN WERKS LGORT MATKL
             INTO (IT_ZSBLIT-MENGE_PO, IT_ZSBLIT-UEBTO,
                   IT_ZSBLIT-UEBTK,    IT_ZSBLIT-WEPOS,
                   IT_ZSBLIT-ELIKZ,    IT_ZSBLIT-LOEKZ,
                   IT_ZSBLIT-UNTTO,
                   IT_ZSBLIT-BPUMZ,    IT_ZSBLIT-BPUMN,
                   IT_ZSBLIT-WERKS,    IT_ZSBLIT-LGORT,
                   IT_ZSBLIT-MATKL)
             FROM   EKPO
             WHERE  EBELN   EQ   IT_ZSBLIT-EBELN
             AND    EBELP   EQ   IT_ZSBLIT-EBELP.

      IF SY-SUBRC EQ 0.
        IF IT_ZSBLIT-LOEKZ NE SPACE.
          CLEAR : ZSBLIT.
          MESSAGE W069 WITH IT_ZSBLIT-EBELN IT_ZSBLIT-ZFITMNO.
          CONTINUE.
        ENDIF.
*        IF IT_ZSBLIT-ELIKZ EQ 'X'.
*          CLEAR : ZSBLIT.
*          MESSAGE W359 WITH IT_ZSBLIT-EBELN IT_ZSBLIT-EBELP.
*          CONTINUE.
*        ENDIF.
      ELSE.
        CLEAR : ZSBLIT.
        MESSAGE W071 WITH IT_ZSBLIT-EBELN IT_ZSBLIT-EBELP.
        CONTINUE.
      ENDIF.
*----> B/L Item
      IF W_STATUS EQ C_REQ_C.
        SELECT SUM( BLMENGE ) INTO IT_ZSBLIT-ZFBLTOT
               FROM  ZTBLIT
               WHERE ZFBLNO  NE ZTBL-ZFBLNO
               AND   ZFREQNO EQ IT_ZSBLIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSBLIT-ZFITMNO
               AND   BLOEKZ  NE 'X'.
      ELSE.
        SELECT SUM( BLMENGE ) INTO IT_ZSBLIT-ZFBLTOT
               FROM  ZTBLIT
               WHERE ZFREQNO EQ IT_ZSBLIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSBLIT-ZFITMNO
               AND   BLOEKZ  NE 'X'.
      ENDIF.

*>>> B/L Basic Quantity
      IT_ZSBLIT-BLMENGE = IT_ZSBLIT-MENGE - IT_ZSBLIT-ZFBLTOT.

      IF IT_ZSBLIT-BLMENGE LT 0.
        IT_ZSBLIT-BLMENGE = 0.
      ENDIF.

*----> Change
      IF W_SY_SUBRC EQ 0.
        MODIFY IT_ZSBLIT  INDEX W_TABIX.
      ELSE.
        MOVE : SY-UNAME    TO   IT_ZSBLIT-ERNAM,
               SY-DATUM    TO   IT_ZSBLIT-CDAT,
               SY-UNAME    TO   IT_ZSBLIT-UNAM,
               SY-DATUM    TO   IT_ZSBLIT-UDAT.

        APPEND IT_ZSBLIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " IT_ZSBLIT_UPDATE_SCR0112  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBLIT_MENGE_CHECK_SCR0112  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSBLIT_MENGE_CHECK_SCR0112 INPUT.

  " Display Mode -> Module Exit
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK ZSBLIT-ZFPOTY IS INITIAL.     " Monetary Transaction
  CHECK NOT ZSBLIT-EBELN IS INITIAL.  " P/O Input

  READ TABLE IT_ZSBLIT   INDEX TC_0112-CURRENT_LINE.
  W_TABIX = SY-TABIX.

*미달납품 허용치 (%).
  IF NOT IT_ZSBLIT-UNTTO IS INITIAL.
    W_MENGE1 = ( ( ZSBLIT-MENGE * ZSBLIT-UEBTO ) / 100 ).
    W_MENGE  = ZSBLIT-BLMENGE - W_MENGE1.
    W_MENGE2 = ZSBLIT-MENGE - W_MENGE1.

    CLEAR : W_OLD_MENGE.
    SELECT SINGLE BLMENGE INTO W_OLD_MENGE
           FROM   ZTBLIT
           WHERE  ZFBLNO   EQ   ZSBLIT-ZFBLNO
           AND    ZFBLIT   EQ   ZSBLIT-ZFBLIT
           AND    BLOEKZ   NE   'X'.
*>> 수입의뢰 - OPEN 수량 + 저장된 B/L수량(현재) - 입력B/L수량.
    W_OLD_MENGE = W_MENGE2 - ( IT_ZSBLIT-ZFBLTOT - W_OLD_MENGE ).

    IF ZSBLIT-BLMENGE LT W_OLD_MENGE.
      MESSAGE W365 WITH ZSBLIT-ZFBLIT.
    ENDIF.
  ENDIF.

  IF ZSBLIT-UEBTK EQ 'X'.
    MOVE ZSBLIT-BLMENGE  TO     IT_ZSBLIT-BLMENGE.
    MODIFY  IT_ZSBLIT  INDEX W_TABIX.
    EXIT.
  ENDIF.

*초과납품 허용치 (%).
  IF ZSBLIT-UEBTO IS INITIAL.
    CLEAR : W_OLD_MENGE.
    SELECT SINGLE BLMENGE INTO W_OLD_MENGE
           FROM   ZTBLIT
           WHERE  ZFBLNO   EQ   ZSBLIT-ZFBLNO
           AND    ZFBLIT   EQ   ZSBLIT-ZFBLIT
           AND    BLOEKZ   NE   'X'.
*>> 수입의뢰 - OPEN 수량 + 저장된 B/L수량(현재) - 입력B/L수량.
    W_OLD_MENGE = IT_ZSBLIT-MENGE -
                ( IT_ZSBLIT-ZFBLTOT - W_OLD_MENGE ).
    IF ZSBLIT-BLMENGE GT W_OLD_MENGE.
      MESSAGE E360 WITH ZSBLIT-ZFBLIT.
      EXIT.
    ENDIF.
  ELSE.                            " 초과납품 허용치 (%)가 입력시.
    W_MENGE1 = ( ( ZSBLIT-MENGE * ZSBLIT-UEBTO ) / 100 ).
    W_MENGE  = W_MENGE1 + ZSBLIT-BLMENGE.   " B/L수량  + 초과납품허용.
    W_MENGE2 = W_MENGE1 + ZSBLIT-MENGE.     " 수입수량 + 초과납품허용.

    CLEAR : W_OLD_MENGE.
    SELECT SINGLE BLMENGE INTO W_OLD_MENGE
           FROM   ZTBLIT
           WHERE  ZFBLNO   EQ   ZSBLIT-ZFBLNO
           AND    ZFBLIT   EQ   ZSBLIT-ZFBLIT
           AND    BLOEKZ   NE   'X'.

    W_OLD_MENGE = W_MENGE2 - ( IT_ZSBLIT-ZFBLTOT - W_OLD_MENGE ).
    IF ZSBLIT-BLMENGE GT W_OLD_MENGE.
      MESSAGE E360 WITH ZSBLIT-ZFBLIT.
      EXIT.
    ENDIF.
  ENDIF.

  IF ZSBLIT-BLMENGE GT 0.
    IF ZSBLIT-ZFSHNO IS INITIAL.
      SELECT MAX( ZFSHNO ) INTO W_ZFSHNO
             FROM ZTBLIT
             WHERE EBELN  EQ ZSBLIT-EBELN
             AND   EBELP  EQ ZSBLIT-EBELP.
      W_ZFSHNO  =  W_ZFSHNO + 1.
      MOVE  W_ZFSHNO  TO  ZSBLIT-ZFSHNO.
    ELSE.
      IF NOT ZSBLIT-ZFBLNO IS INITIAL AND
         NOT ZSBLIT-ZFBLIT IS INITIAL.
        SELECT MAX( ZFSHNO ) INTO W_ZFSHNO
               FROM ZTBLIT
               WHERE EBELN  EQ ZSBLIT-EBELN
               AND   EBELP  EQ ZSBLIT-EBELP
               AND   ZFBLNO NE ZSBLIT-ZFBLNO
               AND   ZFBLIT NE ZSBLIT-ZFBLIT
               AND   ZFSHNO GT ZSBLIT-ZFSHNO.
      ELSE.
        SELECT MAX( ZFSHNO ) INTO W_ZFSHNO
               FROM ZTBLIT
               WHERE EBELN  EQ ZSBLIT-EBELN
               AND   EBELP  EQ ZSBLIT-EBELP
               AND   ZFSHNO GT ZSBLIT-ZFSHNO.
      ENDIF.

      IF NOT W_ZFSHNO IS INITIAL.
        MESSAGE E500(ZIM1) WITH W_ZFSHNO ZSBLIT-ZFSHNO.
        EXIT.
      ENDIF.

      IF NOT ZSBLIT-ZFBLNO IS INITIAL AND
         NOT ZSBLIT-ZFBLIT IS INITIAL.
        SELECT MAX( ZFSHNO ) INTO W_ZFSHNO
               FROM ZTBLIT
               WHERE EBELN  EQ ZSBLIT-EBELN
               AND   EBELP  EQ ZSBLIT-EBELP.
      ELSE.
        SELECT MAX( ZFSHNO ) INTO W_ZFSHNO
               FROM ZTBLIT
               WHERE EBELN  EQ ZSBLIT-EBELN
               AND   EBELP  EQ ZSBLIT-EBELP.
      ENDIF.
      IF W_ZFSHNO IS INITIAL.
        IF NOT ( ZSBLIT-ZFSHNO EQ '99' OR
                 ZSBLIT-ZFSHNO EQ '01' ).
          MESSAGE E502(ZIM1).  EXIT.
        ENDIF.
      ELSE.
      ENDIF.
    ENDIF.
  ENDIF.

  "-----------------------------------
  " Inbound Delivery Check.
  "-----------------------------------
  CLEAR : W_INBOUND_TOT.
  ">> Invoice Data Get
  SELECT *
    FROM ZTCIVIT
   WHERE ZFBLNO  EQ  ZTBL-ZFBLNO
     AND EBELN   EQ  ZSBLIT-EBELN
     AND EBELP   EQ  ZSBLIT-EBELP.

    ">> Commercial Invoice No Set
    SELECT SINGLE *
      FROM ZTCIVHD
     WHERE ZFCIVRN  EQ  ZTCIVIT-ZFCIVRN.

    ">> Inbound Amount Get.
    SELECT SUM( B~LFIMG ) INTO W_INBOUND_QTY
    FROM   LIKP  AS  A    INNER JOIN  LIPS AS B
    ON     A~VBELN        EQ    B~VBELN
    WHERE  A~BOLNR        EQ    ZTCIVHD-ZFCIVNO
      AND  B~VGBEL        EQ    ZSBLIT-EBELN
      AND  B~VGPOS        EQ    ZSBLIT-EBELP.

    W_INBOUND_TOT = W_INBOUND_TOT + W_INBOUND_QTY.

  ENDSELECT.

  IF W_INBOUND_TOT  NE ZSBLIT-BLMENGE.
    MESSAGE W005(ZIM1).
  ENDIF.

  MOVE ZSBLIT-BLMENGE  TO     IT_ZSBLIT-BLMENGE.
  MOVE ZSBLIT-ZFSHNO   TO     IT_ZSBLIT-ZFSHNO.

  MODIFY  IT_ZSBLIT  INDEX W_TABIX.

ENDMODULE.                 " IT_ZSBLIT_MENGE_CHECK_SCR0112  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_SALES_ORDER_DATA_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_SALES_ORDER_DATA_SCR0102 INPUT.
  CHECK : ZTBL-ZFPOTY EQ 'H'.
  CHECK : W_STATUS NE C_REQ_D.

  IF ZTBL-VBELN IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'VBELN'.
  ENDIF.

  CALL FUNCTION 'SD_VBAK_SINGLE_READ'
       EXPORTING
            I_VBELN            = ZTBL-VBELN
*             I_BYPASSING_BUFFER = CHARB
       IMPORTING
            E_VBAK             = VBAK
       EXCEPTIONS
            RECORD_NOT_FOUND   = 1
            OTHERS             = 2.

  IF SY-SUBRC NE 0.
    MESSAGE E302(V1) WITH ZTBL-VBELN.
  ELSE.
    IF VBAK-AUART NE ZTIMIMG00-AUART.
      MESSAGE E699 WITH VBAK-AUART.
    ENDIF.
  ENDIF.

  MOVE : VBAK-VKORG     TO      ZTBL-VKORG,
         VBAK-VTWEG     TO      ZTBL-VTWEG,
         VBAK-SPART     TO      ZTBL-SPART,
         VBAK-WAERK     TO      ZTBL-ZFBLAMC,
         VBAK-BUKRS_VF  TO      ZTBL-BUKRS,
         VBAK-KUNNR     TO      ZTBL-KUNNR,
         '80'           TO      ZTBL-ZFPONC.

*> 인도처.
  SELECT SINGLE * FROM VBPA
         WHERE VBELN  EQ  VBAK-VBELN
         AND   POSNR  EQ  '000000'
         AND   PARVW  EQ  'WE'.
  IF SY-SUBRC EQ 0.
    ZTBL-KUNWE = VBPA-KUNNR.
  ENDIF.

  SELECT SINGLE * FROM VBKD
         WHERE VBELN EQ VBAK-VBELN
         AND   POSNR EQ '000000'.
  IF SY-SUBRC EQ 0.
    IF ZTBL-INCO1 IS INITIAL.
      MOVE VBKD-INCO1 TO ZTBL-INCO1.
    ENDIF.
  ENDIF.

  IF ZTBL-KUNWE IS INITIAL.
    MOVE : ZTBL-KUNNR TO ZTBL-KUNWE.
  ENDIF.

*> 담당자명 SELECT.
  CALL FUNCTION 'ZIM_GET_SY_UNAME_TEXT'
       EXPORTING
            P_UNAME      = SY-UNAME
       IMPORTING
            P_FIRST_NAME = ADRP-NAME_FIRST
            P_LAST_NAME  = ADRP-NAME_LAST.

  CONCATENATE  ADRP-NAME_LAST ADRP-NAME_FIRST INTO ZTBL-ZFPRNAM.

*   IF ZTBL-LIFNR IS INITIAL.
*      SELECT SINGLE LIFNR INTO ZTBL-LIFNR
*             FROM KNA1
*             WHERE KUNNR EQ VBAK-KUNNR.
*   ENDIF.

*   IF ZTBL-ZFBENI IS INITIAL.
*      MOVE : ZTBL-LIFNR TO ZTBL-ZFBENI.
*   ENDIF.

  IF ZTBL-KOSTL IS INITIAL.
    MOVE  VBAK-KOSTL TO ZTBL-KOSTL.
  ENDIF.

  SELECT * INTO TABLE IT_VBAP
           FROM  VBAP
           WHERE VBELN   EQ   ZTBL-VBELN.


  IF ZTBL-VKORG IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'VKORG'.
  ENDIF.
  IF ZTBL-VTWEG IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'VTWEG'.
  ENDIF.
  IF ZTBL-SPART IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'SPART'.
  ENDIF.


  REFRESH : IT_ZSBLIT.
  LOOP AT IT_VBAP.
    CLEAR : IT_ZSBLIT.
    MOVE-CORRESPONDING   IT_VBAP TO IT_ZSBLIT.
    MOVE : IT_VBAP-MATNR         TO     IT_ZSBLIT-MATNR,
           IT_VBAP-ARKTX         TO     IT_ZSBLIT-TXZ01,
           IT_VBAP-NETWR         TO     IT_ZSBLIT-NETPR,
           'H'                   TO     IT_ZSBLIT-ZFPOTY,
           IT_VBAP-VBELN         TO     IT_ZSBLIT-VBELN,
           IT_VBAP-POSNR         TO     IT_ZSBLIT-POSNR,
*             IT_VBAP-KWMENG        TO     IT_ZSBLIT-BLMENGE,
           IT_VBAP-KWMENG        TO     IT_ZSBLIT-MENGE,
           IT_VBAP-KPEIN         TO     IT_ZSBLIT-PEINH,
           IT_VBAP-KMEIN         TO     IT_ZSBLIT-BPRME.

    SELECT SUM( BLMENGE ) INTO  IT_ZSBLIT-ZFBLTOT
                          FROM  ZTBLIT
                          WHERE VBELN  EQ   IT_ZSBLIT-VBELN
                          AND   POSNR  EQ   IT_ZSBLIT-POSNR
                          AND   ZFBLNO NE   ZTBL-ZFBLNO.

    IT_ZSBLIT-BLMENGE =  IT_ZSBLIT-MENGE - IT_ZSBLIT-ZFBLTOT.
    IF IT_ZSBLIT-BLMENGE LT 0.
      IT_ZSBLIT-BLMENGE = 0.
    ENDIF.

    IF IT_ZSBLIT-STAWN IS INITIAL.
      SELECT SINGLE STAWN INTO IT_ZSBLIT-STAWN
             FROM   MARC
             WHERE  MATNR EQ   IT_ZSBLIT-MATNR
             AND    WERKS EQ   IT_ZSBLIT-WERKS.
    ENDIF.

    IF ZTBL-ZFWERKS IS INITIAL.
      MOVE IT_ZSBLIT-WERKS TO ZTBL-ZFWERKS.
    ENDIF.

    IF ZTBL-INCO1 IS INITIAL.
      SELECT SINGLE * FROM VBKD
                      WHERE VBELN EQ IT_ZSBLIT-VBELN
                      AND   POSNR EQ IT_ZSBLIT-POSNR.
      IF SY-SUBRC EQ 0.
        MOVE : VBKD-INCO1   TO ZTBL-INCO1.
      ENDIF.
    ENDIF.

    MOVE :    IT_VBAP-WERKS   TO ZTBL-ZFWERKS,
              IT_ZSBLIT-TXZ01 TO ZTBL-ZFRGDSR.

    APPEND IT_ZSBLIT.
  ENDLOOP.

ENDMODULE.                 " GET_SALES_ORDER_DATA_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0113  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0113 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0113-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0113  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0113_MARK_TC_0113  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0113_MARK_TC_0113 INPUT.

  READ TABLE IT_ZSBLIT   INDEX TC_0113-CURRENT_LINE.
  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSBLIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSBLIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSBLIT INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0113_MARK_TC_0113  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LINE_SCR0060 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  W_LINE = TC_0060-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  COMPANYCODE_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE COMPANYCODE_CHECK_SCR0100 INPUT.

  IF ZTBKPF-BUKRS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUKRS'.
  ELSE.
    PERFORM  P1000_GET_COMPANY_CODE(SAPMZIM02) USING ZTBKPF-BUKRS.
*>> IMG Account Check.
    SELECT SINGLE * FROM ZTIMIMG11
           WHERE    BUKRS  EQ   ZTBKPF-BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE S987 WITH ZTBKPF-BUKRS.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.

ENDMODULE.                 " COMPANYCODE_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  PERIOD_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PERIOD_CHECK_SCR0060 INPUT.
  DATA: S_MONAT LIKE BKPF-MONAT.      "Save field for input period

*> Document Date
  IF ZTBKPF-BLDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BLDAT'.
  ENDIF.

  IF ZTBKPF-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUDAT'.
  ENDIF.

  IF ZTBKPF-ZFCNAME IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'ZFCNAME'.
  ENDIF.

*  IF ZSBKPF-ZFBDT IS INITIAL.
*     ZSBKPF-ZFBDT = ZTBKPF-BUDAT.
*  ENDIF.

  CLEAR ZTBKPF-GJAHR.
  S_MONAT = ZTBKPF-MONAT.

  PERFORM PERIODE_ERMITTELN(SAPMZIM02) USING ZTBKPF-BUDAT
                                             ZTBKPF-GJAHR
                                             ZTBKPF-MONAT.

  IF NOT S_MONAT IS INITIAL
  AND    S_MONAT NE ZTBKPF-MONAT
  AND ( SY-BINPT = SPACE AND SY-CALLD = SPACE ).
    MESSAGE W000(F5) WITH S_MONAT ZTBKPF-BUDAT ZTBKPF-MONAT.
  ENDIF.

ENDMODULE.                 " PERIOD_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  BELEGART_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BELEGART_CHECK_SCR0060 INPUT.

  IF ZTBKPF-BLART IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BLART'.
  ENDIF.
*> 문서 종류 체크.
  PERFORM BELEGART_PRUEFEN(SAPFF001)
          USING ZTBKPF-BLART ZTBKPF-GJAHR.
*> 전기년도 체크.
  BKPF-BUKRS = ZTBKPF-BUKRS.
  PERFORM NUMMERNKREIS_LESEN(SAPFF001)
          USING ZTBKPF-GJAHR.
*> 권한 검증.
  PERFORM P2000_BELEGART_AUTHORITY_CHECK.

ENDMODULE.                 " BELEGART_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_EXCHAGE_RATE_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_EXCHAGE_RATE_SCR0060 INPUT.

  CLEAR SY-MSGTY.
  CALL FUNCTION 'CURRENCY_CODE_CHECK'
       EXPORTING
            I_OBJECT        = 'BKPF'
            I_BUKRS         = ZTBKPF-BUKRS
            I_CURRENCY_CODE = ZTBKPF-WAERS
*            i_budat         = ZTBKPF-BUDAT
*            i_bldat         = ZTBKPF-BLDAT
*            i_wwert         = ZTBKPF-WWERT
*            i_tcode         = 'F-43'
*            i_awtyp         = 'BKPF'
*            i_blart         = ZTBKPF-BLART
*            i_zedat         = ZTBKPF-DBEDT
       EXCEPTIONS
            ERROR_MESSAGE = 1.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSEIF SY-MSGTY = 'W'.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*------- Waehrungsinformationen fuer den Belegkopf ---------------------
  ZTBKPF-HWAER = T001-WAERS.
*  MOVE-CORRESPONDING X001 TO BKPF.

  CALL FUNCTION 'FI_CURRENCY_CHECK'
       EXPORTING
            I_BLDAT  = ZTBKPF-BLDAT
            I_BUDAT  = ZTBKPF-BUDAT
            I_BUKRS  = ZTBKPF-BUKRS
            I_BLART  = ZTBKPF-BLART
*            I_KURS2  = BKPF-KURS2
*            I_KURS3  = BKPF-KURS3
            I_KURSF  = ZTBKPF-KURSF
            I_WAERS  = ZTBKPF-WAERS
            X_DIALOG = CHAR_X          "Warnungen im Dialog!
            I_WWERT  = ZTBKPF-WWERT
       IMPORTING
*            E_KURS2  = BKPF-KURS2
*            E_KURS3  = BKPF-KURS3
            E_KURSF  = ZTBKPF-KURSF
            E_WWERT  = ZTBKPF-WWERT.

  IF ZTBKPF-KURSF LT 0.
    ZTBKPF-KURSF = 0.
  ENDIF.

  IF ZTBKPF-WAERS EQ ZTBKPF-HWAER.
    ZTBKPF-ZFPCUR = 'X'.
  ENDIF.

  SET PARAMETER ID 'WHR' FIELD ZTBKPF-WAERS.

ENDMODULE.                 " GET_EXCHAGE_RATE_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  VENDOR_ACCOUNT_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VENDOR_ACCOUNT_CHECK_SCR0060 INPUT.

  IF ZTBKPF-BUPLA IS INITIAL.    ">
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUPLA'.
  ENDIF.

  IF ZTBKPF-GSBER IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'GSBER'.
  ENDIF.

  IF ZTBKPF-LIFNR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'LIFNR'.
  ENDIF.

  CALL FUNCTION 'FI_POSTING_KEY_DATA'
       EXPORTING
            I_BSCHL       = '31'
            I_UMSKZ       = SPACE  ">bseg-umskz
       IMPORTING
            E_T074U       = T074U
            E_TBSL        = TBSL
            E_TBSLT       = TBSLT
       EXCEPTIONS
            ERROR_MESSAGE = 1.

* 1. PBO: no message if bschl request umskz
  IF SY-SUBRC = 1.
* (del) if not ( firstcall = 'X'                           "Note 352492
    IF TBSL-XSONU NE SPACE.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH
              SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

*>> VENDOR MASTER DEFINE.
  CLEAR : *LFA1.
  SELECT SINGLE * INTO *LFA1 FROM LFA1
                  WHERE LIFNR EQ ZTBKPF-LIFNR.
  IF SY-SUBRC NE 0.
    MESSAGE E023 WITH ZTBKPF-LIFNR.
  ENDIF.

  CALL FUNCTION 'FI_VENDOR_DATA'
       EXPORTING
            I_BUKRS = ZTBKPF-BUKRS
            I_LIFNR = ZTBKPF-LIFNR
       IMPORTING
            E_KRED  = VF_KRED.

*    IF ZTBKPF-ZTERM IS INITIAL.
*       ZTBKPF-ZTERM = VF_KRED-ZTERM.
*    ELSEIF ZTBKPF-ZTERM NE VF_KRED-ZTERM.
  IF ZTBKPF-ZTERM NE VF_KRED-ZTERM.
    IF ZTBKPF-ZTERM IS INITIAL.
      ZTBKPF-ZTERM = VF_KRED-ZTERM.
    ELSE.
      MESSAGE W574 WITH  ZTBKPF-LIFNR VF_KRED-ZTERM ZTBKPF-ZTERM.
    ENDIF.
  ENDIF.

*    if lfb1-bukrs is initial.
*       move-corresponding vf_kred to lfa1.
*    else.
*       move-corresponding vf_kred to lfa1.
*       move-corresponding vf_kred to lfb1.
*       lfb1-sperr = vf_kred-sperr_b.
*       lfb1-loevm = vf_kred-loevm_b.
*       lfb1-begru = vf_kred-begru_b.
*   endif.

  IF ZTBKPF-MWSKZ IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'MWSKZ'.
  ENDIF.

  IF ZTBKPF-AKONT IS INITIAL.
    ZTBKPF-AKONT = VF_KRED-AKONT.
  ENDIF.
  IF ZTBKPF-AKONT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'AKONT'.
  ENDIF.
*>> TAX CODE CHECK.
  CALL FUNCTION 'FI_TAX_INDICATOR_CHECK'
       EXPORTING
            I_BUKRS  = ZTBKPF-BUKRS
            I_HKONT  = VF_KRED-AKONT
            I_KOART  = 'K'
            I_MWSKZ  = ZTBKPF-MWSKZ
            I_STBUK  = SPACE
            X_DIALOG = 'X'
       IMPORTING
            E_EGRKZ  = EGRKZ.
*> ??????.
  IF ZTBKPF-WRBTR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WRBTR'.
  ENDIF.
*> ??.
  IF ZTBKPF-WAERS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WAERS'.
  ENDIF.

*>> ??? ??.
  IF ZTBKPF-XMWST EQ 'X'.
    SELECT SINGLE * FROM T007A
           WHERE KALSM EQ 'TAXKR'
           AND   MWSKZ EQ  ZTBKPF-MWSKZ.
    IF SY-SUBRC NE 0.
      MESSAGE E495 WITH 'TAXKR' ZTBKPF-MWSKZ.
    ENDIF.

    SELECT * FROM  KONP
             WHERE KAPPL EQ 'TX'       ">??.
             AND   KSCHL EQ 'KRIT'     ">?????.
             AND   MWSK1 EQ ZTBKPF-MWSKZ.

      MOVE: KONP-KBETR   TO   W_KBETR,         ">??.
            KONP-KONWA   TO   W_KONWA.         ">??.
      IF NOT W_KBETR IS INITIAL.
        W_KBETR = W_KBETR / 10.
      ENDIF.
    ENDSELECT.

    IF SY-SUBRC EQ 0.
      IF NOT W_KBETR IS INITIAL.
        PERFORM SET_CURR_CONV_TO_EXTERNAL(SAPMZIM01)
                USING ZTBKPF-WRBTR ZTBKPF-WAERS ZTBKPF-WMWST.
*>>>> ?? : (100 + %) =  X : % ======>
        W_WMWST = ZTBKPF-WMWST.
        BAPICURR-BAPICURR = ZTBKPF-WMWST * W_KBETR * 1000.
        W_KBETR1 = W_KBETR.
        W_KBETR = ( W_KBETR + 100 ).
        BAPICURR-BAPICURR = BAPICURR-BAPICURR / W_KBETR.

*           ZTBKPF-WMWST = W_WMWST - ( BAPICURR-BAPICURR * 100 / 1000 ).
*           ZTBKPF-WMWST = ZTBKPF-WMWST / 10.
        BAPICURR-BAPICURR = BAPICURR-BAPICURR / 1000.
        ZTBKPF-WMWST = BAPICURR-BAPICURR.
*            COMPUTE ZTBKPF-WMWST = TRUNC( BAPICURR-BAPICURR ).
*            ZTBKPF-WMWST = ZTBKPF-WMWST * 10.

        PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
                USING ZTBKPF-WMWST ZTBKPF-WAERS.
      ELSE.
        CLEAR : ZTBKPF-WMWST.
      ENDIF.
    ELSE.
      CLEAR : ZTBKPF-WMWST.
    ENDIF.
  ENDIF.

ENDMODULE.                 " VENDOR_ACCOUNT_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_HEADER_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INPUT_HEADER_CHECK_SCR0060 INPUT.

*> 사업장.
  IF ZTBKPF-BUPLA IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUPLA'.
  ENDIF.
*> 지급처.
*  IF ZTBKPF-LIFNR IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'LIFNR'.
*  ENDIF.
**> 지급보류키.
*  IF ZTBKPF-ZLSPR IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'ZLSPR'.
*  ENDIF.
*> 전표통화금액.
  IF ZTBKPF-WRBTR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WRBTR'.
  ENDIF.
*> 통화.
  IF ZTBKPF-WAERS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WAERS'.
  ENDIF.

*> 유환여부..
  IF ZTBKPF-ZFPOYN IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'ZFPOYN'.
  ENDIF.

*  IF NOT ZTBKPF-TBTKZ IS INITIAL.
*     MESSAGE W000
*  ENDIF.


ENDMODULE.                 " INPUT_HEADER_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFADVPT_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZFADVPT_CHECK_SCR0060 INPUT.

  IF ZTBKPF-ZFADVPT IS INITIAL.    "> 업무가불여부...
    CLEAR : ZTBKPF-HKONT.
    MESSAGE W926 WITH 'Clear'.
  ELSE.
    MESSAGE W926 WITH 'Setting up'.
    IF ZTBKPF-HKONT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'HKONT'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZFADVPT_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  TBTKZ_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TBTKZ_CHECK_SCR0060 INPUT.

  IF ZTBKPF-TBTKZ IS INITIAL.    "> 후속 차변/대변.
    MESSAGE W616.
  ELSE.
    SELECT SINGLE * FROM T163C
                    WHERE SPRAS EQ SY-LANGU
                    AND BEWTP   EQ ZTBKPF-TBTKZ.
    IF SY-SUBRC EQ 0.
      MESSAGE W617 WITH T163C-BEWTL.
    ELSE.
      MESSAGE W617 WITH ZTBKPF-TBTKZ.
    ENDIF.
  ENDIF.

ENDMODULE.                 " TBTKZ_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  VAT_AMOUNT_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VAT_AMOUNT_CHECK_SCR0060 INPUT.


  IF ZTBKPF-WAERS EQ ZTBKPF-HWAER.
*        ZTBKPF-DMBTR = ZTBKPF-WRBTR + ZTBKPF-WMWST.
    ZTBKPF-DMBTR = ZTBKPF-WRBTR.
  ELSE.
    IF NOT ZTBKPF-KURSF IS INITIAL.
      PERFORM    SET_CURR_CONV_TO_EXTERNAL(SAPMZIM01)
                 USING ZTBKPF-WRBTR
                       ZTBKPF-WAERS
                       ZTBKPF-DMBTR.

      ZTBKPF-DMBTR = ZTBKPF-DMBTR * ZTBKPF-KURSF.

      PERFORM    SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
                 USING ZTBKPF-DMBTR
                       ZTBKPF-HWAER.

    ENDIF.
  ENDIF.

ENDMODULE.                 " VAT_AMOUNT_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBSEG_UPDATE_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSBSEG_UPDATE_SCR0060 INPUT.

*   W_CHK_BIT = 'Y'.
**> 보험 자동기표여부 체크...
*   IF ZTIMIMG00-ZFATIC EQ 'X'.
*      IF ZTBKPF-ZFCSTGRP EQ '003' AND ZSBSEG-ZFCD EQ '1AB'.
*         CLEAR : ZSBSEG.
*         MESSAGE W607.
*         EXIT.
*      ENDIF.
*   ENDIF.

*> 관세/부가세 체크...
**   IF ZTBKPF-ZFPOYN EQ 'Y'.
*      IF ( ZTBKPF-ZFCSTGRP EQ '006' AND ZSBSEG-ZFCD EQ '001' ) OR
*         ( ZTBKPF-ZFCSTGRP EQ '006' AND ZSBSEG-ZFCD EQ '003' ).
*         IF ZSBSEG-ZFCD EQ '001'.
*            MESSAGE W618 WITH 'Customs'.
*         ELSEIF ZSBSEG-ZFCD EQ '003'.
*            MESSAGE W618 WITH 'Advance VAT'.
*         ENDIF.
*         CLEAR : ZSBSEG.
*         EXIT.
*      ENDIF.
**   ENDIF.

*   IF ZTBKPF-ZFCSTGRP EQ '008'.
*
*   ENDIF.

* Internal Table Read
  READ TABLE IT_ZSBSEG  INDEX TC_0060-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

** 입력여부 체크.
*  IF  ZSBSEG-ZFCD    IS INITIAL
**  AND ZSBSEG-ZFIMDNO IS INITIAL
*  AND ZSBSEG-WRBTR   IS INITIAL
*  AND ZSBSEG-WMWST   IS INITIAL
*  AND ZSBSEG-SGTXT   IS INITIAL.
*     W_CHK_BIT = 'N'.
*  ENDIF.

*> MOVE.
  MOVE-CORRESPONDING  ZSBSEG  TO   IT_ZSBSEG.
  MOVE : ZTBKPF-ZFCSTGRP      TO   IT_ZSBSEG-ZFCSTGRP.

*> INPUT VALUE CHECK.
*  IF W_CHK_BIT EQ 'Y'.
*> 관리번호.
  IF IT_ZSBSEG-ZFCD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'ZFCD'.
  ELSE.
    SELECT SINGLE * FROM ZTIMIMG08
            WHERE   ZFCDTY   =   IT_ZSBSEG-ZFCSTGRP
            AND     ZFCD     =   IT_ZSBSEG-ZFCD.
    IF SY-SUBRC NE 0.
      PERFORM P2000_NO_INPUT(SAPMZIM02)  USING  'ZSBSEG' 'ZFCD'.
      MESSAGE E909 WITH IT_ZSBSEG-ZFCD.
    ELSE.
      MOVE: ZTIMIMG08-ZFCDNM    TO IT_ZSBSEG-ZFCDNM.
*> TAX CODE CHECK.
      IF ZTIMIMG08-ZFCD5(2) NE IT_ZSBSEG-MWSKZ.
        MESSAGE W596 WITH ZTIMIMG08-ZFCDNM
                ZTIMIMG08-ZFCD5 IT_ZSBSEG-MWSKZ.
      ENDIF.
      IF IT_ZSBSEG-MWSKZ NE ZTBKPF-MWSKZ.
        MESSAGE W598 WITH  ZTBKPF-LIFNR ZTBKPF-MWSKZ
                           IT_ZSBSEG-MWSKZ.
      ENDIF.
*> 조건유형.
      IF ZTIMIMG08-ZFCD1 EQ 'Y'.   ">DELIVERY COST.
        IF ZTIMIMG08-COND_TYPE IS INITIAL.
          MESSAGE E595 WITH W_COST_TYPE IT_ZSBSEG-ZFCD.
        ELSE.
          IF ZTIMIMG08-COND_TYPE NE ZSBSEG-COND_TYPE.
            MESSAGE W610 WITH
                 ZTIMIMG08-COND_TYPE ZSBSEG-COND_TYPE.
          ELSE.
            MOVE: IT_ZSIMIMG08-ZFCDNM    TO IT_ZSBSEG-ZFCDNM,
                  IT_ZSIMIMG08-COND_TYPE TO IT_ZSBSEG-COND_TYPE.
          ENDIF.
        ENDIF.
        IT_ZSBSEG-ZFDCSTX = 'X'.
        ZTBKPF-ZFDCSTX = 'X'.
      ELSE.
        IT_ZSBSEG-COND_TYPE = 'ZRA2'.
        CLEAR : IT_ZSBSEG-ZFDCSTX, ZTBKPF-ZFDCSTX.
      ENDIF.
    ENDIF.
  ENDIF.

*> 관련문서번호.
  IF ZSBSEG-ZFIMDNO IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'ZFIMDNO'.
  ELSE.
    PERFORM P1000_IMPORT_DOC_CHEKC(SAPMZIM02)
                                     USING IT_ZSBSEG-ZFIMDNO
                                           IT_ZSBSEG-ZFDCNM
                                           IT_ZSBSEG-ZFPOYN
                                           'I'
                                           IT_ZSBSEG-KOSTL
                                           IT_ZSBSEG-ZFCD.
  ENDIF.
*
  IF ZSBSEG-NEWKO IS INITIAL.
    PERFORM  P2000_SET_NEWKO(SAPMZIM02)  USING ZSBSEG-NEWKO
                                               ZSBSEG-ZFCD
                                               ZSBSEG-ZFIMDNO.
    IT_ZSBSEG-NEWKO = ZSBSEG-NEWKO.
    IF ZSBSEG-NEWKO IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'NEWKO'.
    ENDIF.
  ENDIF.

  IF NOT IT_ZSBSEG-NEWKO IS INITIAL.
    CALL FUNCTION 'FI_POSTING_KEY_DATA'
         EXPORTING
              I_BSCHL = IT_ZSBSEG-NEWBS
              I_UMSKZ = SPACE
         IMPORTING
              E_TBSL  = TBSL
              E_TBSLT = TBSLT
              E_T074U = T074U.

*        SKB1-SAKNR = IT_ZSBSEG-NEWKO+7(10).
    SKB1-SAKNR = IT_ZSBSEG-NEWKO.
    SKB1-BUKRS = ZTBKPF-BUKRS.

    SELECT SINGLE * INTO SKA1 FROM SKA1
           WHERE KTOPL = T001-KTOPL
           AND   SAKNR = SKB1-SAKNR.

    IF SY-SUBRC = 0.
      SELECT SINGLE * INTO SKB1 FROM SKB1
           WHERE SAKNR = SKB1-SAKNR
           AND   BUKRS = SKB1-BUKRS.
    ENDIF.

    IF SY-SUBRC NE 0.
      MESSAGE E106(F5) WITH SKB1-SAKNR SKB1-BUKRS.
    ENDIF.
  ENDIF.

*> 전표금액.
  IF IT_ZSBSEG-WRBTR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'WRBTR'.
  ENDIF.
*> 환산일.
  IF IT_ZSBSEG-WWERT IS INITIAL.
    MOVE : ZTBKPF-WWERT TO IT_ZSBSEG-WWERT.
  ENDIF.
*> 환율.
  IF IT_ZSBSEG-KURSF IS INITIAL.
    MOVE : ZTBKPF-KURSF TO IT_ZSBSEG-KURSF.
  ENDIF.
  IF IT_ZSBSEG-KURSF IS INITIAL.
    IF ZTBKPF-WAERS NE ZTBKPF-HWAER.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'KURSF'.
    ENDIF.
  ENDIF.
*> 지정.
  IF IT_ZSBSEG-ZUONR IS INITIAL.
    IT_ZSBSEG-ZUONR = IT_ZSBSEG-ZFDCNM.
  ENDIF.
*> 품목 TEXT.
  IF IT_ZSBSEG-SGTXT IS INITIAL.
    IT_ZSBSEG-SGTXT = IT_ZSBSEG-ZFCDNM.
  ENDIF.

*  ENDIF.

  IF ZTBKPF-WAERS EQ ZTBKPF-HWAER.
    IT_ZSBSEG-DMBTR = IT_ZSBSEG-WRBTR.
  ELSE.
    IF NOT IT_ZSBSEG-KURSF IS INITIAL.
      IT_ZSBSEG-DMBTR = IT_ZSBSEG-WRBTR * IT_ZSBSEG-KURSF.

      SELECT SINGLE * FROM  TCURX
             WHERE  CURRKEY     = ZTBKPF-WAERS.
      IF SY-SUBRC NE 0.
        TCURX-CURRDEC = 2.
      ENDIF.

      IF TCURX-CURRDEC NE 0.
        PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
               USING IT_ZSBSEG-DMBTR ZTBKPF-HWAER.
      ENDIF.
    ENDIF.

  ENDIF.

*>무환이거나, 컨테이너 텍스일 경우.
  IF ZTBKPF-ZFPOYN EQ 'N'.    " ZSBSEG-ZFCD EQ 'CTT'.

    IF NOT ZTBL-KOSTL IS INITIAL.
      MOVE ZTBL-KOSTL TO ZSBSEG-KOSTL.
    ENDIF.
    IF NOT ZTBL-PS_POSID IS INITIAL.
      MOVE ZTBL-PS_POSID TO ZSBSEG-PS_POSID.
    ENDIF.

    IF ZSBSEG-KOSTL    IS INITIAL AND
       ZSBSEG-PS_POSID IS INITIAL.
      PERFORM P2000_NO_INPUT  USING  'ZSBSEG' 'KOSTL'
                              DFIES-SCRTEXT_M W_SUBRC.
      MESSAGE E211(ZIM1).
    ENDIF.

    IF NOT ZSBSEG-PS_POSID IS INITIAL.
      SELECT *
          FROM  PRPS "WBS (작업분할구조) 요소 마스터 데이타.
          UP TO 1 ROWS
          WHERE POSID = ZSBSEG-PS_POSID.
      ENDSELECT.
      IF SY-SUBRC NE 0.
        MESSAGE E212(ZIM1) WITH ZSBSEG-PS_POSID.
      ENDIF.
    ENDIF.
*     IF ZSBSEG-KOSTL IS INITIAL.
*        PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'KOSTL'.
*     ENDIF.
    MOVE : ZSBSEG-KOSTL    TO IT_ZSBSEG-KOSTL.
    MOVE : ZSBSEG-PS_POSID TO IT_ZSBSEG-PS_POSID.
  ENDIF.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSBSEG INDEX W_TABIX.
*  ELSE.
*     IF W_CHK_BIT EQ 'Y'.
*        APPEND IT_ZSBSEG.
*     ENDIF.
  ENDIF.

ENDMODULE.                 " IT_ZSBSEG_UPDATE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBSEG_UPDATE_SCR0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSBSEG_UPDATE_SCR0070 INPUT.

*   W_CHK_BIT = 'Y'.
**> 보험 자동기표여부 체크...
*   IF ZTIMIMG00-ZFATIC EQ 'X'.
*      IF ZTBKPF-ZFCSTGRP EQ '003' AND ZSBSEG-ZFCD EQ '1AB'.
*         CLEAR : ZSBSEG.
*         MESSAGE W607.
*         EXIT.
*      ENDIF.
*   ENDIF.

*> 관세/부가세 체크...
**   IF ZTBKPF-ZFPOYN EQ 'Y'.
*      IF ( ZTBKPF-ZFCSTGRP EQ '006' AND ZSBSEG-ZFCD EQ '001' ) OR
*         ( ZTBKPF-ZFCSTGRP EQ '006' AND ZSBSEG-ZFCD EQ '003' ).
*         IF ZSBSEG-ZFCD EQ '001'.
*            MESSAGE W618 WITH 'Customs'.
*         ELSEIF ZSBSEG-ZFCD EQ '003'.
*            MESSAGE W618 WITH 'Advance VAT'.
*         ENDIF.
*         CLEAR : ZSBSEG.
*         EXIT.
*      ENDIF.
**   ENDIF.

*   IF ZTBKPF-ZFCSTGRP EQ '008'.
*
*   ENDIF.

* Internal Table Read
  READ TABLE IT_ZSBSEG_TMP  INDEX TC_0070-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

** 입력여부 체크.
*  IF  ZSBSEG-ZFCD    IS INITIAL
**  AND ZSBSEG-ZFIMDNO IS INITIAL
*  AND ZSBSEG-WRBTR   IS INITIAL
*  AND ZSBSEG-WMWST   IS INITIAL
*  AND ZSBSEG-SGTXT   IS INITIAL.
*     W_CHK_BIT = 'N'.
*  ENDIF.

*> MOVE.
  MOVE-CORRESPONDING  ZSBSEG  TO   IT_ZSBSEG_TMP.
  MOVE : ZSBKPF-ZFCSTGRP      TO   IT_ZSBSEG_TMP-ZFCSTGRP,
         ZSBKPF-MWSKZ         TO   IT_ZSBSEG_TMP-MWSKZ. "한수원.

*> INPUT VALUE CHECK.
*  IF W_CHK_BIT EQ 'Y'.
*> 관리번호.
  IF IT_ZSBSEG_TMP-ZFCD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'ZFCD'.
  ELSE.
    SELECT SINGLE * FROM ZTIMIMG08
            WHERE   ZFCDTY   =   IT_ZSBSEG_TMP-ZFCSTGRP
            AND     ZFCD     =   IT_ZSBSEG_TMP-ZFCD.
    IF SY-SUBRC NE 0.
      PERFORM P2000_NO_INPUT(SAPMZIM02)  USING  'ZSBSEG' 'ZFCD'.
      MESSAGE E909 WITH IT_ZSBSEG_TMP-ZFCD.
    ELSE.
      MOVE: ZTIMIMG08-ZFCDNM    TO IT_ZSBSEG_TMP-ZFCDNM.
*> TAX CODE CHECK.
      IF ZTIMIMG08-ZFCD5(2) NE IT_ZSBSEG_TMP-MWSKZ.
        MESSAGE W596 WITH ZTIMIMG08-ZFCDNM
                ZTIMIMG08-ZFCD5 IT_ZSBSEG_TMP-MWSKZ.
      ENDIF.
      IF IT_ZSBSEG_TMP-MWSKZ NE ZTBKPF-MWSKZ.
        MESSAGE W598 WITH  ZTBKPF-LIFNR ZTBKPF-MWSKZ
                           IT_ZSBSEG_TMP-MWSKZ.
      ENDIF.
*> 조건유형.
      IF ZTIMIMG08-ZFCD1 EQ 'Y'.   ">DELIVERY COST.
        IF ZTIMIMG08-COND_TYPE IS INITIAL.
          MESSAGE E595 WITH W_COST_TYPE IT_ZSBSEG_TMP-ZFCD.
        ELSE.
          IF ZTIMIMG08-COND_TYPE NE ZSBSEG-COND_TYPE.
            MESSAGE W610 WITH
                 ZTIMIMG08-COND_TYPE ZSBSEG-COND_TYPE.
          ELSE.
            MOVE: IT_ZSIMIMG08-ZFCDNM
                                TO IT_ZSBSEG_TMP-ZFCDNM,
                  IT_ZSIMIMG08-COND_TYPE
                                TO IT_ZSBSEG_TMP-COND_TYPE.
          ENDIF.
        ENDIF.
        IT_ZSBSEG_TMP-ZFDCSTX = 'X'.
        ZSBKPF-ZFDCSTX = 'X'.
      ELSE.
        IT_ZSBSEG_TMP-COND_TYPE = 'ZRA2'.
        CLEAR : IT_ZSBSEG_TMP-ZFDCSTX, ZSBKPF-ZFDCSTX.
      ENDIF.
    ENDIF.
  ENDIF.

*> 관련문서번호.
  IF ZSBSEG-ZFIMDNO IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'ZFIMDNO'.
  ELSE.
    PERFORM P1000_IMPORT_DOC_CHEKC(SAPMZIM02)
                                     USING IT_ZSBSEG_TMP-ZFIMDNO
                                           IT_ZSBSEG_TMP-ZFDCNM
                                           IT_ZSBSEG_TMP-ZFPOYN
                                           'I'
                                           IT_ZSBSEG_TMP-KOSTL
                                           IT_ZSBSEG_TMP-ZFCD.
  ENDIF.
*
  IF ZSBSEG-NEWKO IS INITIAL.
    PERFORM  P2000_SET_NEWKO(SAPMZIM02)  USING ZSBSEG-NEWKO
                                               ZSBSEG-ZFCD
                                               ZSBSEG-ZFIMDNO.
    IT_ZSBSEG-NEWKO = ZSBSEG-NEWKO.
    IF ZSBSEG-NEWKO IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'NEWKO'.
    ENDIF.
  ENDIF.

  IF NOT IT_ZSBSEG_TMP-NEWKO IS INITIAL.
    CALL FUNCTION 'FI_POSTING_KEY_DATA'
         EXPORTING
              I_BSCHL = IT_ZSBSEG_TMP-NEWBS
              I_UMSKZ = SPACE
         IMPORTING
              E_TBSL  = TBSL
              E_TBSLT = TBSLT
              E_T074U = T074U.

*        SKB1-SAKNR = IT_ZSBSEG-NEWKO+7(10).
    SKB1-SAKNR = IT_ZSBSEG_TMP-NEWKO.
    SKB1-BUKRS = ZSBKPF-BUKRS.

    SELECT SINGLE * INTO SKA1 FROM SKA1
           WHERE KTOPL = T001-KTOPL
           AND   SAKNR = SKB1-SAKNR.

    IF SY-SUBRC = 0.
      SELECT SINGLE * INTO SKB1 FROM SKB1
           WHERE SAKNR = SKB1-SAKNR
           AND   BUKRS = SKB1-BUKRS.
    ENDIF.

    IF SY-SUBRC NE 0.
      MESSAGE E106(F5) WITH SKB1-SAKNR SKB1-BUKRS.
    ENDIF.
  ENDIF.

*> 전표금액.
  IF IT_ZSBSEG_TMP-WRBTR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'WRBTR'.
  ENDIF.
*> 환산일.
  IF IT_ZSBSEG_TMP-WWERT IS INITIAL.
    MOVE : ZSBKPF-WWERT TO IT_ZSBSEG_TMP-WWERT.
  ENDIF.
*> 환율.
  IF IT_ZSBSEG_TMP-KURSF IS INITIAL.
    MOVE : ZSBKPF-KURSF TO IT_ZSBSEG_TMP-KURSF.
  ENDIF.
  IF IT_ZSBSEG_TMP-KURSF IS INITIAL.
    IF ZSBKPF-WAERS NE ZSBKPF-HWAER.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'KURSF'.
    ENDIF.
  ENDIF.
*> 지정.
  IF IT_ZSBSEG_TMP-ZUONR IS INITIAL.
    IT_ZSBSEG_TMP-ZUONR = IT_ZSBSEG_TMP-ZFDCNM.
  ENDIF.
*> 품목 TEXT.
  IF IT_ZSBSEG_TMP-SGTXT IS INITIAL.
    IT_ZSBSEG_TMP-SGTXT = IT_ZSBSEG_TMP-ZFCDNM.
  ENDIF.

*  ENDIF.

  IF ZSBKPF-WAERS EQ ZSBKPF-HWAER.
    IT_ZSBSEG_TMP-DMBTR = IT_ZSBSEG_TMP-WRBTR.
  ELSE.
    IF NOT IT_ZSBSEG_TMP-KURSF IS INITIAL.
      IT_ZSBSEG_TMP-DMBTR = IT_ZSBSEG_TMP-WRBTR
                          * IT_ZSBSEG_TMP-KURSF.

      SELECT SINGLE * FROM  TCURX
             WHERE  CURRKEY     = ZSBKPF-WAERS.
      IF SY-SUBRC NE 0.
        TCURX-CURRDEC = 2.
      ENDIF.

      IF TCURX-CURRDEC NE 0.
        PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
               USING IT_ZSBSEG_TMP-DMBTR ZSBKPF-HWAER.
      ENDIF.
    ENDIF.

  ENDIF.

*>무환이거나, 컨테이너 텍스일 경우.
  IF ZSBKPF-ZFPOYN EQ 'N'.    " ZSBSEG-ZFCD EQ 'CTT'.

    IF NOT *ZTBL-KOSTL IS INITIAL.
      MOVE *ZTBL-KOSTL TO ZSBSEG-KOSTL.
    ENDIF.
    IF NOT *ZTBL-PS_POSID IS INITIAL.
      MOVE *ZTBL-PS_POSID TO ZSBSEG-PS_POSID.
    ENDIF.

    IF ZSBSEG-KOSTL    IS INITIAL AND
       ZSBSEG-PS_POSID IS INITIAL.
      PERFORM P2000_NO_INPUT  USING  'ZSBSEG' 'KOSTL'
                              DFIES-SCRTEXT_M W_SUBRC.
      MESSAGE E211(ZIM1).
    ENDIF.

    IF NOT ZSBSEG-PS_POSID IS INITIAL.
      SELECT *
          FROM  PRPS "WBS (작업분할구조) 요소 마스터 데이타.
          UP TO 1 ROWS
          WHERE POSID = ZSBSEG-PS_POSID.
      ENDSELECT.
      IF SY-SUBRC NE 0.
        MESSAGE E212(ZIM1) WITH ZSBSEG-PS_POSID.
      ENDIF.
    ENDIF.
    MOVE : ZSBSEG-KOSTL    TO IT_ZSBSEG_TMP-KOSTL.
    MOVE : ZSBSEG-PS_POSID TO IT_ZSBSEG_TMP-PS_POSID.
  ENDIF.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSBSEG_TMP INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " IT_ZSBSEG_UPDATE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0060  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0060 INPUT.

  READ TABLE IT_ZSBSEG  INDEX TC_0060-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSBSEG-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSBSEG-ZFMARK.
    ENDIF.
    MODIFY IT_ZSBSEG INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0061_LIST_CHECK_SCR0061  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0061_LIST_CHECK_SCR0061 INPUT.
* list-processing
  LEAVE TO LIST-PROCESSING.
* list Write
  CLEAR : W_TABIX, W_CHG_CHK.
  LOOP AT IT_ZSBLCST_POST.
    ADD 1 TO W_TABIX.
    W_MOD = SY-TABIX MOD 2.
    FORMAT RESET.
    IF W_TABIX EQ 1.
      PERFORM  P3000_SCR0061_HEADER_WRITE.
    ENDIF.

    ON CHANGE OF  IT_ZSBLCST_POST-ZFPAY     OR
                  IT_ZSBLCST_POST-COND_TYPE OR
*                  IT_ZSBLCST_POST-ZFCD1     OR
                  IT_ZSBLCST_POST-MWSKZ     OR
                  IT_ZSBLCST_POST-ZFWERKS   OR
                  IT_ZSBLCST_POST-WAERS.                            .
      IF W_TABIX NE 1.
        PERFORM  P3000_SCR0061_HEADER_WRITE.
      ENDIF.
    ENDON.

*    IF ZSBLCST-ZFCSCD EQ 'CTT' AND
*       IT_ZSBLCST_POST-ZFCSCD NE ZSBLCST-ZFCSCD.
*       IF W_CHG_CHK NE 'Y'.
*          PERFORM  P3000_SCR0061_HEADER_WRITE.
*       ENDIF.
*    ENDIF.
*
*    IF IT_ZSBLCST_POST-ZFCSCD EQ 'CTT' AND
*       ZSBLCST-ZFCSCD NE IT_ZSBLCST_POST-ZFCSCD.
*       IF W_CHG_CHK NE 'Y'.
*          PERFORM  P3000_SCR0061_HEADER_WRITE.
*       ENDIF.
*    ENDIF.

    IF W_MOD EQ 0.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ENDIF.

    WRITE : / SY-VLINE NO-GAP,
            5 SY-VLINE NO-GAP,
              IT_ZSBLCST_POST-ZFCSCD NO-GAP,
              SY-VLINE NO-GAP,
              IT_ZSBLCST_POST-ZFCDNM(22) NO-GAP,
              SY-VLINE,
*               IT_ZSBLCST_POST-COND_TYPE,
*               SY-VLINE,
              IT_ZSBLCST_POST-ZFCAMT CURRENCY IT_ZSBLCST_POST-WAERS,
              SY-VLINE,
              IT_ZSBLCST_POST-ZFCKAMT CURRENCY IT_ZSBLCST_POST-KRW,
              SY-VLINE.
    MOVE-CORRESPONDING IT_ZSBLCST_POST TO ZSBLCST.
    CLEAR : W_CHG_CHK.
    HIDE IT_ZSBLCST_POST.
  ENDLOOP.
  WRITE : / SY-ULINE(76).
  CLEAR : IT_ZSBLCST_POST.
ENDMODULE.                 " D0061_LIST_CHECK_SCR0061  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0060 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'ENTR'.   EXIT.
    WHEN 'YES'.    ANTWORT = 'Y'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.     ANTWORT = 'N'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDMODULE.                 " GET_OK_CODE_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR0060 INPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZTBKPF-ZTERM'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF SCREEN-INPUT EQ '1'.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZTBKPF-ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ELSE.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZTBKPF-ZTERM
              I_XSHOW       = 'X'
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ENDIF.
  IF SY-SUBRC NE 0.
    MESSAGE S177(06) WITH ZTBKPF-ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    ZTBKPF-ZTERM = T052-ZTERM.
  ENDIF.

ENDMODULE.                 " HELP_ZTERM_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_TBTKZ_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_TBTKZ_SCR0060 INPUT.

  PERFORM   P2000_TBTKZ_HELP(SAPMZIMG)  USING   ZTBKPF-TBTKZ.
  SET CURSOR FIELD 'ZTBKPF-TBTKZ'.

ENDMODULE.                 " HELP_TBTKZ_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_COST_CODE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_COST_CODE_SCR0060 INPUT.

  DATA : W_ERR_MODE.

  PERFORM   P2000_CHECK_COST_GROUP  USING   W_ERR_MODE.

  IF W_ERR_MODE EQ 'N'.
    SELECT *
           INTO CORRESPONDING FIELDS OF TABLE IT_COST_HELP
           FROM   ZTIMIMG08
           WHERE  ZFCDTY   EQ   ZTBKPF-ZFCSTGRP.
    IF SY-SUBRC NE 0.
      MESSAGE S406.
      EXIT.
    ENDIF.

    DYNPROG = SY-REPID.
    DYNNR   = SY-DYNNR.

    WINDOW_TITLE = W_COST_TYPE.
    CONCATENATE W_COST_TYPE '코드 HELP' INTO WINDOW_TITLE
                SEPARATED BY SPACE.

    IF W_STATUS EQ C_REQ_D.
      L_DISPLAY = 'X'.
    ELSE.
      CLEAR: L_DISPLAY.
    ENDIF.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
         EXPORTING
*                RETFIELD        = 'OTYPE'
              RETFIELD        = 'ZFCD'
              DYNPPROG        = DYNPROG
              DYNPNR          = DYNNR
              DYNPROFIELD     = 'ZSBSEG-ZFCD'
              WINDOW_TITLE    = WINDOW_TITLE
              VALUE_ORG       = 'S'
              DISPLAY         = L_DISPLAY
         TABLES
              VALUE_TAB       = IT_COST_HELP
         EXCEPTIONS
              PARAMETER_ERROR = 1
              NO_VALUES_FOUND = 2
              OTHERS          = 3.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
  ENDIF.

ENDMODULE.                 " HELP_COST_CODE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBSEG_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSBSEG_CHECK_SCR0060 INPUT.

  CHECK W_STATUS NE C_REQ_D.

* Internal Table Read
  READ TABLE IT_ZSBSEG  INDEX TC_0060-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

** 입력여부 체크.
  IF ZTBKPF-ZFPOYN EQ 'N'. " ZSBSEG-ZFCD EQ 'CTT'.
    IF NOT ZTBL-KOSTL IS INITIAL.
      MOVE ZTBL-KOSTL TO ZSBSEG-KOSTL.
    ENDIF.
    IF NOT ZTBL-PS_POSID IS INITIAL.
      MOVE ZTBL-PS_POSID TO ZSBSEG-PS_POSID.
    ENDIF.

    IF ZSBSEG-KOSTL    IS INITIAL AND
       ZSBSEG-PS_POSID IS INITIAL.
      PERFORM P2000_NO_INPUT  USING  'ZSBSEG' 'KOSTL'
                              DFIES-SCRTEXT_M W_SUBRC.
      MESSAGE E211(ZIM1).
    ENDIF.

    IF NOT ZSBSEG-PS_POSID IS INITIAL.
      SELECT *
          FROM  PRPS "WBS (작업분할구조) 요소 마스터 데이타.
          UP TO 1 ROWS
          WHERE POSID = ZSBSEG-PS_POSID.
      ENDSELECT.
      IF SY-SUBRC NE 0.
        MESSAGE E212(ZIM1) WITH ZSBSEG-PS_POSID.
      ENDIF.
    ENDIF.
*     IF NOT ZTBL-KOSTL IS INITIAL.
*        MOVE ZTBL-KOSTL TO ZSBSEG-KOSTL.
*     ENDIF.
*     IF ZSBSEG-KOSTL IS INITIAL.
*        PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'KOSTL'.
*     ENDIF.
    MOVE : ZSBSEG-KOSTL    TO IT_ZSBSEG-KOSTL.
    MOVE : ZSBSEG-PS_POSID TO IT_ZSBSEG-PS_POSID.
  ENDIF.
  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSBSEG INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " IT_ZSBSEG_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_CONT_TABLE_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_CONT_TABLE_SCR0102 INPUT.

* JSY 20021008 한수원수정. (DATA UPLOAD용 주석처리 2003.02.18)
  IF ZTBL-ZFSHTY EQ 'M' OR ZTBL-ZFSHTY EQ 'A'.
*    IF NOT ZTBL-ZFGITA IS INITIAL.
*      MESSAGE W441(ZIM1).
*      CLEAR : ZTBL-ZFGITA.
*    ENDIF.
    IF ZTBL-ZF20FT IS INITIAL AND
       ZTBL-ZF40FT IS INITIAL .
*      MESSAGE E935.
      MESSAGE W935.
    ENDIF.
  ENDIF.

*  IF ZTBL-ZFSHTY EQ 'L'.
*    CLEAR : ZTBL-ZF20FT, ZTBL-ZF40FT, ZTBL-ZF20HQ, ZTBL-ZF40HQ.
*    IF ZTBL-ZFGITA IS INITIAL.
*      MESSAGE E935.
*    ENDIF.
*  ENDIF.

*  IF ZTBL-ZFSHTY EQ 'B'.
*    MESSAGE W442(ZIM1).
*    CLEAR : ZTBL-ZF20FT, ZTBL-ZF40FT,
*            ZTBL-ZF20HQ, ZTBL-ZF40HQ, ZTBL-ZFGITA.
*  ENDIF.

ENDMODULE.                 " SET_CONT_TABLE_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBLIT_MENGE_CHECK_SCR0113  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSBLIT_MENGE_CHECK_SCR0113 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK NOT ZSBLIT-VBELN IS INITIAL.  " P/O 가 입력되지 않았을 경우.

*  READ TABLE IT_ZSBLIT   WITH KEY ZFBLIT = ZSBLIT-ZFBLIT.
  READ TABLE IT_ZSBLIT   INDEX TC_0113-CURRENT_LINE.
  W_TABIX = SY-TABIX.

  CLEAR : W_OLD_MENGE.
  SELECT SINGLE BLMENGE INTO W_OLD_MENGE
         FROM   ZTBLIT
         WHERE  ZFBLNO   EQ   ZSBLIT-ZFBLNO
         AND    ZFBLIT   EQ   ZSBLIT-ZFBLIT
         AND    BLOEKZ   NE   'X'.
*>> 수입의뢰 - OPEN 수량 + 저장된 B/L수량(현재) - 입력B/L수량.
  W_OLD_MENGE = IT_ZSBLIT-MENGE -
              ( IT_ZSBLIT-ZFBLTOT - W_OLD_MENGE ).
  IF  ZSBLIT-BLMENGE GT W_OLD_MENGE.
    MESSAGE E360 WITH ZSBLIT-ZFBLIT.
  ENDIF.

  MOVE : ZSBLIT-BLMENGE  TO     IT_ZSBLIT-BLMENGE,
         ZSBLIT-STAWN    TO     IT_ZSBLIT-STAWN.

  MODIFY  IT_ZSBLIT  INDEX W_TABIX.

ENDMODULE.                 " IT_ZSBLIT_MENGE_CHECK_SCR0113  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_BL_SHIP_DOC_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_BL_SHIP_DOC_SCR0102 INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF SY-TCODE EQ 'ZIM221'.
    IF ZTBL-ZFBLSDT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFBLSDT'.
    ENDIF.
    IF ZTBL-ZFBLSDP IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFBLSDP'.
    ENDIF.
  ELSEIF SY-TCODE EQ 'ZIM222'.
    IF ZTBL-ZFBLSDP IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFBLSDP'.
    ENDIF.
  ENDIF.
*>> 선적항에 따라서 수송여부 결정.
  IF ZTBL-ZFBLSDP EQ '001'.
    MOVE  'X'  TO  ZTBL-ZFISYN.
  ELSE.
    CLEAR : ZTBL-ZFISYN.
  ENDIF.

ENDMODULE.                 " CHECK_BL_SHIP_DOC_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_BL_REAL_DATE_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_BL_REAL_DATE_SCR0102 INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF SY-TCODE EQ 'ZIM222'.
    IF ZTBL-ZFRETA IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFRETA'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_BL_REAL_DATE_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBLIT_UPDATE_SCR0113  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSBLIT_UPDATE_SCR0113 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

*  CHECK ZSBLIT-VBELN IS INITIAL.

* Internal Table Read
*  READ TABLE IT_ZSBLIT   WITH KEY ZFBLIT = ZSBLIT-ZFBLIT.
  READ TABLE IT_ZSBLIT   INDEX TC_0113-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSBLIT  TO IT_ZSBLIT.

*>> 수입의뢰 번호가 입력되지 않았을 경우.
  IF ZSBLIT-VBELN IS INITIAL.
    CLEAR : ZSBLIT.
    MESSAGE W937.   EXIT.
  ELSE.
    IF ZSBLIT-POSNR IS INITIAL.
      CLEAR : ZSBLIT.
      MESSAGE W948.   EXIT.
    ENDIF.
*>> 영업문서번호가 입력되었을 경우.
    CALL FUNCTION 'SD_VBAK_SINGLE_READ'
         EXPORTING
             I_VBELN            = ZSBLIT-VBELN
*             I_BYPASSING_BUFFER = CHARB
         IMPORTING
             E_VBAK             = VBAK
         EXCEPTIONS
             RECORD_NOT_FOUND   = 1
             OTHERS             = 2.

    IF SY-SUBRC NE 0.
      MESSAGE E302(V1) WITH ZSBLIT-VBELN.
    ELSE.
      IF VBAK-AUART NE ZTIMIMG00-AUART.
        MESSAGE E699 WITH VBAK-AUART.
      ENDIF.
    ENDIF.

    IF VBAK-KUNNR     NE      ZTBL-KUNNR.
      MESSAGE E946 WITH 'Distributor' VBAK-KUNNR VBAK-VBELN ZTBL-KUNNR.
    ENDIF.
    IF VBAK-VKORG     NE      ZTBL-VKORG.
      MESSAGE E946 WITH 'Sales organization'
                   VBAK-VKORG VBAK-VBELN ZTBL-VKORG.
    ENDIF.
    IF VBAK-VTWEG     NE      ZTBL-VTWEG.
      MESSAGE E946 WITH 'Distribution channel'
                   VBAK-VTWEG VBAK-VBELN ZTBL-VTWEG.
    ENDIF.
    IF VBAK-SPART     NE      ZTBL-SPART.
      MESSAGE E946 WITH 'Product line' VBAK-SPART VBAK-VBELN ZTBL-SPART.
    ENDIF.
    IF VBAK-WAERK     NE      ZTBL-ZFBLAMC.
      MESSAGE E946 WITH 'Currency unit'
                   VBAK-WAERK VBAK-VBELN ZTBL-ZFBLAMC.
    ENDIF.
    IF VBAK-BUKRS_VF  NE      ZTBL-BUKRS.
      MESSAGE E946 WITH 'Company code' VBAK-BUKRS_VF VBAK-VBELN
                                   ZTBL-BUKRS.
    ENDIF.
    IF VBAK-KUNNR     NE      ZTBL-KUNNR.
      MESSAGE E942 WITH ZTBL-KUNNR VBAK-VBELN VBAK-KUNNR.
    ENDIF.
*> 인도처.
    SELECT SINGLE * FROM VBPA
           WHERE VBELN  EQ  VBAK-VBELN
           AND   POSNR  EQ  '000000'
           AND   PARVW  EQ  'WE'.

    IF VBPA-KUNNR IS INITIAL.
      MOVE : ZTBL-KUNNR TO VBPA-KUNNR.
    ENDIF.

    IF ZTBL-KUNWE NE VBPA-KUNNR.
      MESSAGE E946 WITH 'Place of delivery'
                   VBPA-KUNNR VBAK-VBELN ZTBL-KUNWE.
    ENDIF.

    MOVE : VBAK-VKORG     TO      ZTBL-VKORG,
           VBAK-VTWEG     TO      ZTBL-VTWEG,
           VBAK-SPART     TO      ZTBL-SPART,
           VBAK-WAERK     TO      ZTBL-ZFBLAMC,
           VBAK-BUKRS_VF  TO      ZTBL-BUKRS,
           VBAK-KUNNR     TO      ZTBL-KUNNR,
           '80'           TO      ZTBL-ZFPONC,
           VBPA-KUNNR     TO      ZTBL-KUNWE.

*>> ITEM번호가 입력되었을 경우.
    IF NOT IT_ZSBLIT-POSNR IS INITIAL.
      SELECT * INTO TABLE IT_VBAP
               FROM  VBAP
               WHERE VBELN   EQ   IT_ZSBLIT-VBELN
               AND   POSNR   EQ   IT_ZSBLIT-POSNR.
    ELSE.
      SELECT * INTO TABLE IT_VBAP
               FROM  VBAP
               WHERE VBELN   EQ   IT_ZSBLIT-VBELN.
    ENDIF.
*> 해당하는 데이타가 존재하지 않을 경우.
    IF SY-SUBRC NE 0.
      CLEAR : ZSBLIT.
      MESSAGE W947 WITH ZSBLIT-VBELN.
      EXIT.
    ENDIF.

    LOOP AT IT_VBAP.
* 기존에 해당 수입의뢰 아이템이 입력되었을 경우 체크.
      READ TABLE  IT_ZSBLIT WITH KEY VBELN = IT_VBAP-VBELN
                                     POSNR = IT_VBAP-POSNR.
      IF SY-SUBRC EQ 0.
        CLEAR : ZSBLIT.
        MESSAGE S358 WITH IT_VBAP-VBELN IT_VBAP-POSNR
                          IT_ZSBLIT-ZFBLIT.
        CONTINUE.
      ENDIF.
      CLEAR : IT_ZSBLIT.
      MOVE-CORRESPONDING   IT_VBAP TO IT_ZSBLIT.
      MOVE : IT_VBAP-MATNR         TO     IT_ZSBLIT-MATNR,
             IT_VBAP-ARKTX         TO     IT_ZSBLIT-TXZ01,
             IT_VBAP-NETWR         TO     IT_ZSBLIT-NETPR,
             'H'                   TO     IT_ZSBLIT-ZFPOTY,
             IT_VBAP-VBELN         TO     IT_ZSBLIT-VBELN,
             IT_VBAP-POSNR         TO     IT_ZSBLIT-POSNR,
*               IT_VBAP-KWMENG        TO     IT_ZSBLIT-BLMENGE,
             IT_VBAP-KWMENG        TO     IT_ZSBLIT-MENGE,
             IT_VBAP-KPEIN         TO     IT_ZSBLIT-PEINH,
             IT_VBAP-KMEIN         TO     IT_ZSBLIT-BPRME.

      SELECT SUM( BLMENGE ) INTO  IT_ZSBLIT-ZFBLTOT
                            FROM  ZTBLIT
                            WHERE VBELN  EQ   IT_ZSBLIT-VBELN
                            AND   POSNR  EQ   IT_ZSBLIT-POSNR
                            AND   ZFBLNO NE   ZTBL-ZFBLNO.

      IT_ZSBLIT-BLMENGE =  IT_ZSBLIT-MENGE - IT_ZSBLIT-ZFBLTOT.
      IF IT_ZSBLIT-BLMENGE LT 0.
        IT_ZSBLIT-BLMENGE = 0.
      ENDIF.
      IF IT_ZSBLIT-STAWN IS INITIAL.
        SELECT SINGLE STAWN INTO IT_ZSBLIT-STAWN
               FROM   MARC
               WHERE  MATNR EQ   IT_ZSBLIT-MATNR
               AND    WERKS EQ   IT_ZSBLIT-WERKS.
      ENDIF.

      IF ZTBL-ZFWERKS IS INITIAL.
        MOVE IT_ZSBLIT-WERKS TO ZTBL-ZFWERKS.
      ENDIF.

      IF ZTBL-INCO1 IS INITIAL.
        SELECT SINGLE * FROM VBKD
                        WHERE VBELN EQ IT_ZSBLIT-VBELN
                        AND   POSNR EQ IT_ZSBLIT-POSNR.
        IF SY-SUBRC EQ 0.
          MOVE : VBKD-INCO1   TO ZTBL-INCO1.
        ENDIF.
      ENDIF.

      MOVE : IT_VBAP-WERKS   TO ZTBL-ZFWERKS,
             IT_ZSBLIT-TXZ01 TO ZTBL-ZFRGDSR.
      MOVE : SY-UNAME    TO   IT_ZSBLIT-ERNAM,
             SY-DATUM    TO   IT_ZSBLIT-CDAT,
             SY-UNAME    TO   IT_ZSBLIT-UNAM,
             SY-DATUM    TO   IT_ZSBLIT-UDAT.

      APPEND IT_ZSBLIT.
      MOVE-CORRESPONDING IT_ZSBLIT TO ZSBLIT.
      W_CHG_CHK = 'Y'.
    ENDLOOP.

  ENDIF.

ENDMODULE.                 " IT_ZSBLIT_UPDATE_SCR0113  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR8211  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR8211 INPUT.

  IF ZTPMTHD-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'BUDAT'.
  ENDIF.

  IF ZTPMTHD-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'BLDAT'.
  ENDIF.

  IF ZTPMTHD-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'BLART'.
  ENDIF.

*>> Local 반제의 경우 통화가 KRW만 업무가불.
*   IF ZTPMTHD-HKONT IS INITIAL AND ZTPMTHD-ZFLCKN EQ '8'
*                               AND ZTPMTHD-ZFPNAMC EQ 'KRW'.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'HKONT'.
*   ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR8211  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_POSTING_HISTORY_SCR8213  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_POSTING_HISTORY_SCR8213 INPUT.

  LEAVE TO LIST-PROCESSING.

  G_REPID = SY-REPID.
  CLEAR G_VARIANT.
  G_VARIANT-REPORT = G_REPID.

*
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = G_REPID
            I_STRUCTURE_NAME         = 'ZSPMTHST'
            I_CALLBACK_PF_STATUS_SET = G_STATUS
            I_CALLBACK_USER_COMMAND  = G_USER_COMMAND
            I_SAVE                   = G_SAVE
            IS_VARIANT               = G_VARIANT
       TABLES
            T_OUTTAB                 = IT_ZSPMTHST.

  CLEAR : OK-CODE.
  LEAVE TO SCREEN 8210.

ENDMODULE.                 " SET_POSTING_HISTORY_SCR8213  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CUSTOMER_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_CUSTOMER_SCR0102 INPUT.

  PERFORM P1000_GET_CUSTOMER_DATA  USING ZTBL-KUNNR
                                         W_KUNNR_NM.
  PERFORM P1000_GET_CUSTOMER_DATA  USING ZTBL-KUNWE
                                         W_KUNWE_NM.

ENDMODULE.                 " CHECK_CUSTOMER_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ZFSHNO_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_ZFSHNO_SCR0102 INPUT.

  CHECK W_STATUS NE C_REQ_D.

*  IF ZTBL-ZFSHNO IS INITIAL.           " 선적차수.
*     READ TABLE IT_ZSBLIT WITH KEY ZFSHNO = ''.
*     IF SY-SUBRC EQ 0.
*        MOVE IT_ZSBLIT-ZFSHNO TO ZTBL-ZFSHNO.
*     ENDIF.
*  ELSE.
*     LOOP AT IT_ZSBLIT WHERE ZFSHNO = ''.
*        MOVE : SY-TABIX    TO W_TABIX,
*               ZTBL-ZFSHNO TO IT_ZSBLIT-ZFSHNO.
*        MODIFY IT_ZSBLIT INDEX W_TABIX.
*     ENDLOOP.
*  ENDIF.

ENDMODULE.                 " CHECK_ZFSHNO_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_FILED_CHECK_SCR7410  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INPUT_FILED_CHECK_SCR7410 INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF ZTIDS-ZFIDRNO IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDS' 'ZFIDRNO'.
  ELSE.
*  NHJ 주석처리 CORECESS
*     SELECT COUNT( * ) INTO W_COUNT
*           FROM ZTIDS
*           WHERE     ZFIDRNO EQ ZTIDS-ZFIDRNO
*           AND NOT ( ZFBLNO  EQ ZTIDS-ZFBLNO
*           AND       ZFCLSEQ EQ ZTIDS-ZFCLSEQ ).
*    IF W_COUNT GT 0.
*       MESSAGE E779 WITH ZTIDS-ZFIDRNO.
*    ENDIF.
  ENDIF.

  IF ZTIDS-ZFRFFNO IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDS' 'ZFRFFNO'.
  ENDIF.

  IF ZTIDS-ZFIDSDT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDS' 'ZFIDSDT'.
  ENDIF.

ENDMODULE.                 " INPUT_FILED_CHECK_SCR7410  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_BL_TO_POINT_SCR6210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_BL_TO_POINT_SCR6210 INPUT.

  REFRESH : IT_BLSDP_HELP.
  SELECT *
*          INTO CORRESPONDING FIELDS OF TABLE IT_BLSDP_HELP
         FROM   ZTIMIMG08
         WHERE  ZFCDTY   EQ   '012'.
    MOVE : ZTIMIMG08-ZFCD   TO   IT_BLSDP_HELP-ZFBLSDP,
           ZTIMIMG08-ZFCDNM TO   IT_BLSDP_HELP-ZFCDNM.
    APPEND IT_BLSDP_HELP.
  ENDSELECT.

  IF SY-SUBRC NE 0.
    MESSAGE S406.
    EXIT.
  ENDIF.

  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.

  WINDOW_TITLE = 'B/L 송부처'.
  CONCATENATE WINDOW_TITLE '코드 HELP' INTO WINDOW_TITLE
              SEPARATED BY SPACE.

  IF W_STATUS EQ C_REQ_D.
    L_DISPLAY = 'X'.
  ELSE.
    CLEAR: L_DISPLAY.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
*                RETFIELD        = 'OTYPE'
               RETFIELD        = 'ZFBLSDP'
               DYNPPROG        = DYNPROG
               DYNPNR          = DYNNR
               DYNPROFIELD     = 'ZTIDR-ZFBLSDP'
               WINDOW_TITLE    = WINDOW_TITLE
               VALUE_ORG       = 'S'
               DISPLAY         = L_DISPLAY
       TABLES
               VALUE_TAB       = IT_BLSDP_HELP
       EXCEPTIONS
               PARAMETER_ERROR = 1
               NO_VALUES_FOUND = 2
               OTHERS          = 3.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

ENDMODULE.                 " HELP_BL_TO_POINT_SCR6210  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CUT_EXRT_SCR6212  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CUT_EXRT_SCR6212 INPUT.

  CHECK : W_STATUS NE C_REQ_D.

  IF NOT ZTIDR-ZFIDWDT IS INITIAL.
*>환율.
    IF ZTIDR-ZFSTAMC NE 'KRW'.
      SELECT SINGLE * FROM ZTIMIMG06
             WHERE WAERS    EQ  ZTIDR-ZFSTAMC
             AND   ZFAPLDT  EQ ( SELECT MAX( ZFAPLDT )
                                 FROM ZTIMIMG06
                                 WHERE WAERS    EQ  ZTIDR-ZFSTAMC
                                 AND   ZFAPLDT  LE  ZTIDR-ZFIDWDT
                                 AND   ZFEXPDT  GE  ZTIDR-ZFIDWDT ).
      IF SY-SUBRC EQ 0.
        MOVE: ZTIMIMG06-ZFEXRT  TO  ZTIDR-ZFEXRT,
              ZTIMIMG06-FFACT   TO  ZTIDR-FFACT.
      ELSE.
        MESSAGE E436 WITH ZTIDR-ZFIDWDT.
      ENDIF.
    ELSE.
      ZTIDR-ZFEXRT = 1.
      ZTIDR-FFACT  = 1.
    ENDIF.
    IF ZTIDR-FFACT LE 0.
      ZTIDR-FFACT = 1.
    ENDIF.

*>>란별 금액 계산.
    LOOP AT IT_ZSIDRHS.
      W_TABIX = SY-TABIX.
      CLEAR : IT_ZSIDRHS-ZFTBAU, IT_ZSIDRHS-ZFTBAK.
      LOOP AT IT_ZSIDRHSD WHERE ZFCONO EQ IT_ZSIDRHS-ZFCONO.
        ADD IT_ZSIDRHSD-ZFAMT TO IT_ZSIDRHS-ZFTBAU.
      ENDLOOP.
      PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIDRHS-ZFTBAU
                                              'USD'
                                              BAPICURR-BAPICURR.

      IT_ZSIDRHS-ZFTBAK = BAPICURR-BAPICURR * ( ZTIDR-ZFEXRT /
                                                ZTIDR-FFACT ).
      PERFORM SET_CURR_CONV_TO_INTERNAL USING
              IT_ZSIDRHS-ZFTBAK 'KRW'.
      MODIFY IT_ZSIDRHS.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " GET_CUT_EXRT_SCR6212  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_COST_CODE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_COST_CODE_SCR0100 INPUT.

  PERFORM   P2000_CHECK_COST_GROUP  USING   W_ERR_MODE.

  IF W_ERR_MODE EQ 'N'.
    SELECT *
           INTO CORRESPONDING FIELDS OF TABLE IT_COST_HELP
           FROM   ZTIMIMG08
           WHERE  ZFCDTY   EQ   ZTBKPF-ZFCSTGRP.
    IF SY-SUBRC NE 0.
      MESSAGE S406.
      EXIT.
    ENDIF.

    DYNPROG = SY-REPID.
    DYNNR   = SY-DYNNR.

    WINDOW_TITLE = W_COST_TYPE.
    CONCATENATE W_COST_TYPE '코드 HELP' INTO WINDOW_TITLE
                SEPARATED BY SPACE.

    IF W_STATUS EQ C_REQ_D.
      L_DISPLAY = 'X'.
    ELSE.
      CLEAR: L_DISPLAY.
    ENDIF.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
         EXPORTING
*                RETFIELD        = 'OTYPE'
              RETFIELD        = 'ZFCD'
              DYNPPROG        = DYNPROG
              DYNPNR          = DYNNR
              DYNPROFIELD     = 'ZSBSEG-ZFCD'
              WINDOW_TITLE    = WINDOW_TITLE
              VALUE_ORG       = 'S'
              DISPLAY         = L_DISPLAY
         TABLES
              VALUE_TAB       = IT_COST_HELP
         EXCEPTIONS
              PARAMETER_ERROR = 1
              NO_VALUES_FOUND = 2
              OTHERS          = 3.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
  ENDIF.

ENDMODULE.                 " HELP_COST_CODE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0070 INPUT.
* CL_GUI_CFW=>DISPATCH must be called if events are registered
* that trigger PAI
* this method calls the event handler method of an event
  CALL METHOD CL_GUI_CFW=>DISPATCH
    IMPORTING
      RETURN_CODE = RETURN_CODE.
*    if return_code <> cl_gui_cfw=>rc_noevent.
  IF RETURN_CODE <> CL_GUI_CFW=>RC_NOEVENT.
    PERFORM  P2000_BLCST_TREE_EVENT_HANDLER.
    CLEAR G_OK_CODE.
    CLEAR : G_EVENT, G_NODE_KEY.
    EXIT.
  ENDIF.

  CASE SY-UCOMM.
    WHEN 'CANC'.
      IF NOT G_CUSTOM_CONTAINER IS INITIAL.
        " destroy tree container (detroys contained tree control, too)
        CALL METHOD G_CUSTOM_CONTAINER->FREE
          EXCEPTIONS
            CNTL_SYSTEM_ERROR = 1
            CNTL_ERROR        = 2.
        IF SY-SUBRC <> 0.
          MESSAGE A000.
        ENDIF.
        CLEAR G_CUSTOM_CONTAINER.
        CLEAR G_TREE.
        CLEAR G_APPLICATION.
      ENDIF.
      CLEAR : G_NODE_KEY, G_NODE_KEY_OLD, G_EVENT, G_NODE_KEY.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'ENTR'.   EXIT.
    WHEN 'YES'.
      CLEAR : W_SELECTED_LINE.
      IF NOT ZSBSEG-ZFSEQ IS INITIAL.
        G_ZFSEQ = ZSBSEG-ZFSEQ.
        MOVE-CORRESPONDING: ZSBKPF  TO  IT_ZSBKPF.
        MODIFY IT_ZSBKPF INDEX G_ZFSEQ.

        DELETE IT_ZSBSEG WHERE ZFSEQ EQ G_ZFSEQ.
        LOOP AT IT_ZSBSEG_TMP.
          MOVE-CORRESPONDING IT_ZSBSEG_TMP TO IT_ZSBSEG.
          APPEND IT_ZSBSEG.
        ENDLOOP.
      ENDIF.

      LOOP AT IT_ZSBKPF WHERE ZFMARK EQ 'X'.
        W_TABIX = SY-TABIX.
        MOVE:  SY-MANDT             TO  IT_ZSBKPF-MANDT,
               ZTBKPF-BUKRS         TO  IT_ZSBKPF-BUKRS,
               ZTBKPF-BLDAT         TO  IT_ZSBKPF-BLDAT,
               ZTBKPF-BUDAT         TO  IT_ZSBKPF-BUDAT,
               ZTBKPF-MONAT         TO  IT_ZSBKPF-MONAT,
               ZTBKPF-GJAHR         TO  IT_ZSBKPF-GJAHR,
               ZTBKPF-ZFCNAME       TO  IT_ZSBKPF-ZFCNAME,
               ZTBKPF-ZLSCH         TO  IT_ZSBKPF-ZLSCH,
               ZTBKPF-ZLSPR         TO  IT_ZSBKPF-ZLSPR.

        MODIFY IT_ZSBKPF INDEX W_TABIX.
        ADD 1 TO W_SELECTED_LINE.
      ENDLOOP.

      IF W_SELECTED_LINE EQ 0.
        MESSAGE E405(ZIM1).
      ENDIF.
*-------------------------------------------------------------------
*> B/L Expense Posting
*-------------------------------------------------------------------
      IT_ZSBSEG_TMP[] = IT_ZSBSEG[].
      REFRESH : IT_ERR_LIST.
      W_LINE1 = 0.
      LOOP AT IT_ZSBKPF WHERE ZFMARK EQ 'X'.
        ADD 1 TO W_LINE1.
        LINE1 = ( W_LINE1 / W_SELECTED_LINE ) * 100.
        OUT_TEXT = 'FI POSTING PROGRESS %99999%%'.
        REPLACE '%99999%' WITH LINE1 INTO OUT_TEXT.
        PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE1.

        MOVE-CORRESPONDING IT_ZSBKPF TO ZTBKPF.

        REFRESH : IT_ZSBSEG.
        LOOP AT IT_ZSBSEG_TMP WHERE ZFSEQ EQ IT_ZSBKPF-ZFSEQ.
          CLEAR : IT_ZSBSEG.
          MOVE-CORRESPONDING IT_ZSBSEG_TMP TO IT_ZSBSEG.

          APPEND IT_ZSBSEG.
        ENDLOOP.
*--------------------------------------------------------------------
*>Posting
*--------------------------------------------------------------------
        PERFORM  P3000_COST_POSTING USING IT_ZSBKPF-ZFSEQ.

      ENDLOOP.
*-------------------------------------------------------------------

      IF NOT G_CUSTOM_CONTAINER IS INITIAL.
        " destroy tree container (detroys contained tree control, too)
        CALL METHOD G_CUSTOM_CONTAINER->FREE
          EXCEPTIONS
            CNTL_SYSTEM_ERROR = 1
            CNTL_ERROR        = 2.
        IF SY-SUBRC <> 0.
          MESSAGE A000.
        ENDIF.
        CLEAR G_APPLICATION.
        CLEAR G_CUSTOM_CONTAINER.
        CLEAR G_TREE.
      ENDIF.
      ANTWORT = 'Y'.
      CLEAR : G_NODE_KEY, G_NODE_KEY_OLD, G_EVENT, G_NODE_KEY.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.
      IF NOT G_CUSTOM_CONTAINER IS INITIAL.
        " destroy tree container (detroys contained tree control, too)
        CALL METHOD G_CUSTOM_CONTAINER->FREE
          EXCEPTIONS
            CNTL_SYSTEM_ERROR = 1
            CNTL_ERROR        = 2.
        IF SY-SUBRC <> 0.
          MESSAGE A000.
        ENDIF.
        CLEAR G_CUSTOM_CONTAINER.
        CLEAR G_TREE.
        CLEAR G_APPLICATION.
      ENDIF.
      CLEAR : G_NODE_KEY, G_NODE_KEY_OLD, G_EVENT, G_NODE_KEY.
      ANTWORT = 'N'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  CLEAR : G_EVENT, G_NODE_KEY.

ENDMODULE.                 " GET_OK_CODE_SCR0070  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LINE_SCR0070 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  W_LINE = TC_0070-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0070  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_EXCHAGE_RATE_SCR0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_EXCHAGE_RATE_SCR0070 INPUT.

  CHECK NOT G_NODE_KEY_OLD IS INITIAL.

  CLEAR SY-MSGTY.
  CALL FUNCTION 'CURRENCY_CODE_CHECK'
       EXPORTING
            I_OBJECT        = 'BKPF'
            I_BUKRS         = ZTBKPF-BUKRS
            I_CURRENCY_CODE = ZSBKPF-WAERS
*            i_budat         = ZTBKPF-BUDAT
*            i_bldat         = ZTBKPF-BLDAT
*            i_wwert         = ZSBKPF-WWERT
*            i_tcode         = 'F-43'
*            i_awtyp         = 'BKPF'
*            i_blart         = ZSBKPF-BLART
*            i_zedat         = ZTBKPF-DBEDT
       EXCEPTIONS
            ERROR_MESSAGE = 1.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSEIF SY-MSGTY = 'W'.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*------- Waehrungsinformationen fuer den Belegkopf ---------------------
  ZSBKPF-HWAER = T001-WAERS.
*  MOVE-CORRESPONDING X001 TO BKPF.

  CALL FUNCTION 'FI_CURRENCY_CHECK'
       EXPORTING
            I_BLDAT  = ZTBKPF-BLDAT
            I_BUDAT  = ZTBKPF-BUDAT
            I_BUKRS  = ZTBKPF-BUKRS
            I_BLART  = ZSBKPF-BLART
*            I_KURS2  = BKPF-KURS2
*            I_KURS3  = BKPF-KURS3
            I_KURSF  = ZSBKPF-KURSF
            I_WAERS  = ZSBKPF-WAERS
            X_DIALOG = CHAR_X          "Warnungen im Dialog!
            I_WWERT  = ZSBKPF-WWERT
       IMPORTING
*            E_KURS2  = BKPF-KURS2
*            E_KURS3  = BKPF-KURS3
            E_KURSF  = ZSBKPF-KURSF
            E_WWERT  = ZSBKPF-WWERT.

  IF ZSBKPF-KURSF LT 0.
    ZSBKPF-KURSF = 0.
  ENDIF.

  IF ZSBKPF-WAERS EQ ZSBKPF-HWAER.
    ZSBKPF-ZFPCUR = 'X'.
  ENDIF.

  SET PARAMETER ID 'WHR' FIELD ZSBKPF-WAERS.

ENDMODULE.                 " GET_EXCHAGE_RATE_SCR0070  INPUT
*&---------------------------------------------------------------------*
*&      Module  BELEGART_CHECK_SCR0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BELEGART_CHECK_SCR0070 INPUT.

  CHECK NOT G_NODE_KEY_OLD IS INITIAL.

  IF ZSBKPF-BLART IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'BLART'.
  ENDIF.
*> Document Type Check.
  PERFORM BELEGART_PRUEFEN(SAPFF001)
          USING ZSBKPF-BLART ZTBKPF-GJAHR.
*> Posting Year Check
  BKPF-BUKRS = ZTBKPF-BUKRS.
  PERFORM NUMMERNKREIS_LESEN(SAPFF001)
          USING ZTBKPF-GJAHR.

ENDMODULE.                 " BELEGART_CHECK_SCR0070  INPUT
*&---------------------------------------------------------------------*
*&      Module  VENDOR_ACCOUNT_CHECK_SCR0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VENDOR_ACCOUNT_CHECK_SCR0070 INPUT.

  CHECK NOT G_NODE_KEY_OLD IS INITIAL.

  IF ZTIMIMG00-ZFBPLK EQ 'X'.
    IF ZSBKPF-BUPLA IS INITIAL.    ">
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'BUPLA'.
    ENDIF.
  ENDIF.
  IF ZTIMIMG00-ZFBALK EQ 'X'.
    IF ZSBKPF-GSBER IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'GSBER'.
    ENDIF.
  ENDIF.
  IF ZSBKPF-LIFNR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'LIFNR'.
  ENDIF.

  CALL FUNCTION 'FI_POSTING_KEY_DATA'
       EXPORTING
            I_BSCHL       = '31'
            I_UMSKZ       = SPACE  ">bseg-umskz
       IMPORTING
            E_T074U       = T074U
            E_TBSL        = TBSL
            E_TBSLT       = TBSLT
       EXCEPTIONS
            ERROR_MESSAGE = 1.

* 1. PBO: no message if bschl request umskz
  IF SY-SUBRC = 1.
* (del) if not ( firstcall = 'X'                           "Note 352492
    IF TBSL-XSONU NE SPACE.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH
              SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

*>> VENDOR MASTER DEFINE.
  CLEAR : *LFA1.
  SELECT SINGLE * INTO *LFA1 FROM LFA1
                  WHERE LIFNR EQ ZSBKPF-LIFNR.
  IF SY-SUBRC NE 0.
    MESSAGE E023 WITH ZSBKPF-LIFNR.
  ENDIF.

  CALL FUNCTION 'FI_VENDOR_DATA'
       EXPORTING
            I_BUKRS = ZTBKPF-BUKRS
            I_LIFNR = ZSBKPF-LIFNR
       IMPORTING
            E_KRED  = VF_KRED.

*    IF ZTBKPF-ZTERM IS INITIAL.
*       ZTBKPF-ZTERM = VF_KRED-ZTERM.
*    ELSEIF ZTBKPF-ZTERM NE VF_KRED-ZTERM.
  IF ZSBKPF-ZTERM NE VF_KRED-ZTERM.
    IF ZSBKPF-ZTERM IS INITIAL.
      ZSBKPF-ZTERM = VF_KRED-ZTERM.
    ELSE.
      MESSAGE W574 WITH  ZSBKPF-LIFNR VF_KRED-ZTERM ZSBKPF-ZTERM.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'MRM_PAYMENT_TERMS_GET'
       EXPORTING
          IF_ZTERM = ZSBKPF-ZTERM
          IF_BLDAT = ZTBKPF-BLDAT
          IF_BUDAT = ZTBKPF-BUDAT
          IF_ZFBDT = ZSBKPF-ZFBDT
     IMPORTING
          EF_ZFBDT = ZSBKPF-ZFBDT
*            ef_zbd1t = cs_rbkpv-zbd1t
*            ef_zbd1p = cs_rbkpv-zbd1p
*            ef_zbd2t = cs_rbkpv-zbd2t
*            ef_zbd2p = cs_rbkpv-zbd2p
*            ef_zbd3t = cs_rbkpv-zbd3t
*            ef_zlsch = cs_rbkpv-zlsch
          EF_ZLSPR = ZSBKPF-ZLSPR.

  IF ZTBKPF-ZLSPR IS INITIAL.
    ZSBKPF-ZLSPR = ZTBKPF-ZLSPR.
  ENDIF.



*    if lfb1-bukrs is initial.
*       move-corresponding vf_kred to lfa1.
*    else.
*       move-corresponding vf_kred to lfa1.
*       move-corresponding vf_kred to lfb1.
*       lfb1-sperr = vf_kred-sperr_b.
*       lfb1-loevm = vf_kred-loevm_b.
*       lfb1-begru = vf_kred-begru_b.
*   endif.

  IF ZSBKPF-MWSKZ IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'MWSKZ'.
  ENDIF.

  IF ZSBKPF-AKONT IS INITIAL.
    ZSBKPF-AKONT = VF_KRED-AKONT.
  ENDIF.
  IF ZSBKPF-AKONT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'AKONT'.
  ENDIF.
*>> TAX CODE CHECK.
  CALL FUNCTION 'FI_TAX_INDICATOR_CHECK'
       EXPORTING
            I_BUKRS  = ZTBKPF-BUKRS
            I_HKONT  = VF_KRED-AKONT
            I_KOART  = 'K'
            I_MWSKZ  = ZSBKPF-MWSKZ
            I_STBUK  = SPACE
            X_DIALOG = 'X'
       IMPORTING
            E_EGRKZ  = EGRKZ.

*> Tax Amount
  READ TABLE IT_ZSBKPF WITH KEY ZFSEQ = IT_ZSBSEG-ZFSEQ.
  ZSBKPF-WRBTR = IT_ZSBKPF-WRBTR - IT_ZSBKPF-WMWST + ZSBKPF-WMWST.
  IF ZSBKPF-HWAER = ZSBKPF-WAERS.
    ZSBKPF-DMBTR = ZSBKPF-WRBTR.
  ELSE.
    " Convert
  ENDIF.

*> Amount
  IF ZSBKPF-WRBTR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'WRBTR'.
  ENDIF.
*> Currency
  IF ZSBKPF-WAERS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'WAERS'.
  ENDIF.

*>> Tax Auto Calc.
  IF ZTBKPF-XMWST EQ 'X'.
    SELECT SINGLE * FROM T007A
           WHERE KALSM EQ 'TAXUS'
           AND   MWSKZ EQ  ZSBKPF-MWSKZ.
    IF SY-SUBRC NE 0.
      MESSAGE E495 WITH 'TAXKR' ZSBKPF-MWSKZ.
    ENDIF.

    SELECT * FROM  KONP
             WHERE KAPPL EQ 'TX'
             AND   KSCHL EQ 'MWVS'
             AND   MWSK1 EQ ZSBKPF-MWSKZ.

      MOVE: KONP-KBETR   TO   W_KBETR,         ">??.
            KONP-KONWA   TO   W_KONWA.         ">??.
      IF NOT W_KBETR IS INITIAL.
        W_KBETR = W_KBETR / 10.
      ENDIF.
    ENDSELECT.

    IF SY-SUBRC EQ 0.
      IF NOT W_KBETR IS INITIAL.
        PERFORM SET_CURR_CONV_TO_EXTERNAL(SAPMZIM01)
                USING ZSBKPF-WRBTR ZSBKPF-WAERS ZSBKPF-WMWST.
*>>>> ?? : (100 + %) =  X : % ======>
        W_WMWST = ZSBKPF-WMWST.
        BAPICURR-BAPICURR = ZSBKPF-WMWST * W_KBETR * 1000.
        W_KBETR1 = W_KBETR.
        W_KBETR = ( W_KBETR + 100 ).
        BAPICURR-BAPICURR = BAPICURR-BAPICURR / W_KBETR.

*           ZTBKPF-WMWST = W_WMWST - ( BAPICURR-BAPICURR * 100 / 1000 ).
*           ZTBKPF-WMWST = ZTBKPF-WMWST / 10.
        BAPICURR-BAPICURR = BAPICURR-BAPICURR / 1000.
        ZSBKPF-WMWST = BAPICURR-BAPICURR.
*            COMPUTE ZTBKPF-WMWST = TRUNC( BAPICURR-BAPICURR ).
*            ZTBKPF-WMWST = ZTBKPF-WMWST * 10.

        PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
                USING ZSBKPF-WMWST ZSBKPF-WAERS.
      ELSE.
        CLEAR : ZSBKPF-WMWST.
      ENDIF.
    ELSE.
      CLEAR : ZSBKPF-WMWST.
    ENDIF.
  ENDIF.

ENDMODULE.                 " VENDOR_ACCOUNT_CHECK_SCR0070  INPUT
*&---------------------------------------------------------------------*
*&      Module  VAT_AMOUNT_CHECK_SCR0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VAT_AMOUNT_CHECK_SCR0070 INPUT.

  CHECK NOT G_NODE_KEY_OLD IS INITIAL.

  IF ZSBKPF-WAERS EQ ZSBKPF-HWAER.
    ZSBKPF-DMBTR = ZSBKPF-WRBTR.
  ELSE.
    IF NOT ZSBKPF-KURSF IS INITIAL.
      PERFORM    SET_CURR_CONV_TO_EXTERNAL(SAPMZIM01)
                 USING ZSBKPF-WRBTR
                       ZSBKPF-WAERS
                       ZSBKPF-DMBTR.

      ZSBKPF-DMBTR = ZSBKPF-DMBTR * ZSBKPF-KURSF.

      PERFORM    SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
                 USING ZSBKPF-DMBTR
                       ZSBKPF-HWAER.
    ENDIF.
  ENDIF.

ENDMODULE.                 " VAT_AMOUNT_CHECK_SCR0070  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBSEG_CHECK_SCR0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSBSEG_CHECK_SCR0070 INPUT.

  CHECK W_STATUS NE C_REQ_D.

* Internal Table Read
  READ TABLE IT_ZSBSEG_TMP INDEX TC_0070-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

** 입력여부 체크.
  IF ZSBKPF-ZFPOYN EQ 'N'. " ZSBSEG-ZFCD EQ 'CTT'.
    IF NOT ZTBL-KOSTL IS INITIAL.
      MOVE ZTBL-KOSTL TO ZSBSEG-KOSTL.
    ENDIF.
    IF NOT ZTBL-PS_POSID IS INITIAL.
      MOVE ZTBL-PS_POSID TO ZSBSEG-PS_POSID.
    ENDIF.

    IF ZSBSEG-KOSTL    IS INITIAL AND
       ZSBSEG-PS_POSID IS INITIAL.
      PERFORM P2000_NO_INPUT  USING  'ZSBSEG' 'KOSTL'
                              DFIES-SCRTEXT_M W_SUBRC.
      MESSAGE E211(ZIM1).
    ENDIF.

    IF NOT ZSBSEG-PS_POSID IS INITIAL.
      SELECT *
          FROM  PRPS "WBS (작업분할구조) 요소 마스터 데이타.
          UP TO 1 ROWS
          WHERE POSID = ZSBSEG-PS_POSID.
      ENDSELECT.
      IF SY-SUBRC NE 0.
        MESSAGE E212(ZIM1) WITH ZSBSEG-PS_POSID.
      ENDIF.
    ENDIF.
*     IF NOT ZTBL-KOSTL IS INITIAL.
*        MOVE ZTBL-KOSTL TO ZSBSEG-KOSTL.
*     ENDIF.
*     IF ZSBSEG-KOSTL IS INITIAL.
*        PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'KOSTL'.
*     ENDIF.
    MOVE : ZSBSEG-KOSTL    TO IT_ZSBSEG_TMP-KOSTL.
    MOVE : ZSBSEG-PS_POSID TO IT_ZSBSEG_TMP-PS_POSID.
  ENDIF.
  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSBSEG_TMP INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " IT_ZSBSEG_CHECK_SCR0070  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0070 INPUT.

  READ TABLE IT_ZSBSEG_TMP  INDEX TC_0070-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSBSEG_TMP-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSBSEG_TMP-ZFMARK.
    ENDIF.
    MODIFY IT_ZSBSEG_TMP INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_0070  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_HEADER_CHECK_SCR0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INPUT_HEADER_CHECK_SCR0070 INPUT.

  CHECK NOT G_NODE_KEY_OLD IS INITIAL.
*> Business Place
  IF ZTIMIMG00-ZFBPLK EQ 'X'.
    IF ZSBKPF-BUPLA IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'BUPLA'.
    ENDIF.
  ENDIF.
  IF ZTIMIMG00-ZFBALK EQ 'X'.
    IF ZSBKPF-GSBER IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'GSBER'.
    ENDIF.
  ENDIF.
*> Document Currency Amocunt Check
  IF ZSBKPF-WRBTR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'WRBTR'.
  ENDIF.
*> Currency
  IF ZSBKPF-WAERS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'WAERS'.
  ENDIF.

*> Monetary Yes/No
  IF ZSBKPF-ZFPOYN IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'ZFPOYN'.
  ENDIF.

*> Reference No
  IF ZSBKPF-XBLNR IS INITIAL.
    MESSAGE  E977 WITH 'Input Vendor Invoice No!'.
    EXIT.
  ENDIF.

ENDMODULE.                 " INPUT_HEADER_CHECK_SCR0070  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFADVPT_CHECK_SCR0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZFADVPT_CHECK_SCR0070 INPUT.

  CHECK NOT G_NODE_KEY_OLD IS INITIAL.

  IF ZSBKPF-ZFADVPT IS INITIAL.    "> 업무가불여부...
    CLEAR : ZSBKPF-HKONT.
    MESSAGE W926 WITH 'Clear'.
  ELSE.
    MESSAGE W926 WITH 'Setting up'.
    IF ZSBKPF-HKONT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'HKONT'.
    ELSE.
      SELECT SINGLE * FROM LFA1
             WHERE    LIFNR EQ ZSBKPF-HKONT.
      IF SY-SUBRC NE 0.
        MESSAGE E585 WITH 'Employer vendor' ZSBKPF-HKONT.
      ELSE.
        IF LFA1-KTOKK NE ZTIMIMG00-KTOKK.
          MESSAGE E408(ZIM1) WITH LFA1-KTOKK ZTIMIMG00-KTOKK.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZFADVPT_CHECK_SCR0070  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR0070 INPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZSBKPF-ZTERM'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF SCREEN-INPUT EQ '1'.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZSBKPF-ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ELSE.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZSBKPF-ZTERM
              I_XSHOW       = 'X'
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ENDIF.
  IF SY-SUBRC NE 0.
    MESSAGE S177(06) WITH ZSBKPF-ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    ZSBKPF-ZTERM = T052-ZTERM.
  ENDIF.

ENDMODULE.                 " HELP_ZTERM_SCR0070  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0071  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0071 INPUT.
* CL_GUI_CFW=>DISPATCH must be called if events are registered
* this method calls the event handler method of an event
  CALL METHOD CL_GUI_CFW=>DISPATCH
    IMPORTING
      RETURN_CODE = RETURN_CODE.
*    if return_code <> cl_gui_cfw=>rc_noevent.
  IF RETURN_CODE <> CL_GUI_CFW=>RC_NOEVENT.
    PERFORM  P2000_MESSAGE_TREE_HANDLER.
    CLEAR : G_EVENT, G_NODE_KEY.
    EXIT.
  ENDIF.

  CASE SY-UCOMM.
    WHEN 'CANC' OR 'YES' OR 'NO'.
      IF NOT G_CUSTOM_CONTAINER IS INITIAL.
        " destroy tree container (detroys contained tree control, too)
        CALL METHOD G_CUSTOM_CONTAINER->FREE
          EXCEPTIONS
            CNTL_SYSTEM_ERROR = 1
            CNTL_ERROR        = 2.
        IF SY-SUBRC <> 0.
          MESSAGE A000.
        ENDIF.
        CLEAR G_CUSTOM_CONTAINER.
        CLEAR G_TREE.
        CLEAR G_APPLICATION.
      ENDIF.
      CLEAR : G_NODE_KEY, G_NODE_KEY_OLD, G_EVENT, G_NODE_KEY.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
*    WHEN 'ENTR'.   EXIT.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  CLEAR : G_EVENT, G_NODE_KEY.

ENDMODULE.                 " GET_OK_CODE_SCR0071  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_GSBER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_GSBER INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF ZTIMIMG00-ZFBALK EQ 'X'.
    IF ZTCIVHD-ZFSVYN NE 'X'.
      IF ZTCIVHD-GSBER IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCIVHD' 'GSBER'.
      ELSE.
        READ TABLE IT_GSBER WITH KEY GSBER = ZTCIVHD-GSBER.
        IF SY-SUBRC NE 0.
          SET CURSOR FIELD ZTCIVHD-GSBER.
          MESSAGE W407(ZIM1) WITH ZTCIVHD-GSBER.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_GSBER  INPUT
*&---------------------------------------------------------------------*
*&      Module  DUTY_CHECK_SCR070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DUTY_CHECK_SCR070 INPUT.

  IF ZSREQHD-ZFDUTYX IS INITIAL.    "> 월합세금계산서 여부...
    MESSAGE W420(ZIM1) WITH 'Clear'.
  ELSE.
    MESSAGE W420(ZIM1) WITH 'Setting up'.
  ENDIF.

ENDMODULE.                 " DUTY_CHECK_SCR070  INPUT
*&---------------------------------------------------------------------*
*&      Module  REQIT_GET_LINE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE REQIT_GET_LINE_SCR0103 INPUT.

ENDMODULE.                 " REQIT_GET_LINE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSCIVIT_UPDATE_SCR3520  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSCIVIT_UPDATE_SCR3520 INPUT.

* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK ZSCIVIT-EBELN IS INITIAL.

* Internal Table Read
  READ TABLE IT_ZSCIVIT   WITH KEY ZFCIVSQ = ZSCIVIT-ZFCIVSQ.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSCIVIT  TO IT_ZSCIVIT.

*>> 수입의뢰 번호가 입력되지 않았을 경우.
  IF ZSCIVIT-ZFREQNO IS INITIAL.
    MESSAGE W019.   EXIT.
  ELSE.
*>> 수입의뢰 번호가 입력되었을 경우.
    SELECT SINGLE * FROM ZTREQHD
                    WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO.
    IF SY-SUBRC NE 0.
      CLEAR : ZSCIVIT.
      MESSAGE W018 WITH IT_ZSCIVIT-ZFREQNO.
      EXIT.
    ENDIF.
*>> 삼국무역...
    IF ZTREQHD-ZFTRIPLE EQ 'X'.
      MESSAGE W535 WITH ZTREQHD-ZFREQNO.
      CLEAR : ZSCIVIT.  EXIT.
    ENDIF.
*>> 사후관리여부...
    IF ZTREQHD-ZFCLOSE EQ 'X'.
      MESSAGE W535 WITH ZTREQHD-ZFREQNO.
      CLEAR : ZSCIVIT.  EXIT.
    ENDIF.
*>> 문서 타입 CHECK.
    IF ZTCIVHD-ZFREQTY NE ZTREQHD-ZFREQTY.
      MESSAGE W568 WITH ZTCIVHD-ZFREQTY ZTREQHD-ZFREQTY.
      CLEAR : ZSCIVIT.  EXIT.
    ENDIF.

    READ TABLE IT_ZSCIVIT INDEX  1.
    IF SY-SUBRC EQ 0.
      IF IT_ZSCIVIT-ZFREQNO NE ZTREQHD-ZFREQNO.
        MESSAGE W569.
        CLEAR : ZSCIVIT.  EXIT.
      ENDIF.
    ENDIF.

*>> 수입의뢰 ITEM번호가 입력되었을 경우.
    IF NOT IT_ZSCIVIT-ZFITMNO IS INITIAL.
*-----------------------------------------------------------------------
* 기존에 해당 수입의뢰 아이템이 입력되었을 경우 체크.
      READ TABLE  IT_ZSCIVIT WITH KEY ZFREQNO = ZSCIVIT-ZFREQNO
                                      ZFITMNO = ZSCIVIT-ZFITMNO.
      IF SY-SUBRC EQ 0 AND W_SY_SUBRC NE 0.
        CLEAR : ZSCIVIT.
        MESSAGE S358 WITH ZSCIVIT-ZFREQNO ZSCIVIT-ZFITMNO
                          IT_ZSCIVIT-ZFCIVSQ.
        EXIT.
      ENDIF.
*-----------------------------------------------------------------------
      MOVE-CORRESPONDING ZSCIVIT  TO IT_ZSCIVIT.
*----> 수입의뢰 ITEM번호가 입력되었을 경우, 발췌하여 메세지.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF IT_ZSCIVIT
             FROM   ZTREQIT
             WHERE ZFREQNO EQ ZSCIVIT-ZFREQNO
             AND   ZFITMNO EQ ZSCIVIT-ZFITMNO.
      IF SY-SUBRC NE 0.
        CLEAR : ZSCIVIT.
        MESSAGE W357 WITH ZSCIVIT-ZFREQNO ZSCIVIT-ZFITMNO.
        EXIT.
      ENDIF.
*++++> P/O DATA 조회.
      CLEAR : EKPO.
      SELECT SINGLE * FROM   EKPO
             WHERE    EBELN  EQ   IT_ZSCIVIT-EBELN
             AND      EBELP  EQ   IT_ZSCIVIT-EBELP.

      " 품목범주가 SERVICE 일 경우는 SERVICE ENTRY 금액 GET.
      IF EKPO-PSTYP  EQ  '9'.
        PERFORM   P3000_SERVICE_ITEM_GET.
        EXIT.
      ENDIF.

      " PO ITEM 이 삭제한 경우 CONTINUE.
      IF EKPO-LOEKZ  NE  SPACE.
        CLEAR : ZSCIVIT.
        MESSAGE  W069  WITH EKPO-EBELN  EKPO-EBELP.
        EXIT.
      ENDIF.

      " 송장처리가 종결된 경우 CONTINUE.
      IF EKPO-EREKZ  EQ  'X'.
        CLEAR : ZSCIVIT.
        MESSAGE  W220(ZIM1)  WITH  EKPO-EBELN  EKPO-EBELP.
        EXIT.
      ENDIF.

      " 송장처리 하지 않을 경우는 CONTINUE.
      IF EKPO-REPOS  NE  'X'.
        CLEAR : ZSCIVIT.
        MESSAGE  W221(ZIM1)  WITH  EKPO-EBELN  EKPO-EBELP.
        EXIT.
      ENDIF.

      MOVE  : EKPO-MENGE         TO    IT_ZSCIVIT-MENGE_PO,
              EKPO-UEBTO         TO    IT_ZSCIVIT-UEBTO,
              EKPO-UEBTK         TO    IT_ZSCIVIT-UEBTK,
              EKPO-WEPOS         TO    IT_ZSCIVIT-WEPOS,
              EKPO-ELIKZ         TO    IT_ZSCIVIT-ELIKZ,
              EKPO-LOEKZ         TO    IT_ZSCIVIT-LOEKZ,
              EKPO-UNTTO         TO    IT_ZSCIVIT-UNTTO,
              EKPO-BPUMN         TO    IT_ZSCIVIT-BPUMN,
              EKPO-BPUMZ         TO    IT_ZSCIVIT-BPUMZ,
              EKPO-WERKS         TO    IT_ZSCIVIT-WERKS,
              EKPO-LGORT         TO    IT_ZSCIVIT-LGORT,
              ZTREQHD-WAERS      TO    IT_ZSCIVIT-ZFIVAMC,
              EKPO-EBELP         TO    IT_ZSCIVIT-EBELN,
              EKPO-EBELN         TO    IT_ZSCIVIT-EBELP,
              'KRW'              TO    IT_ZSCIVIT-ZFKRW.

*----> INVOICE 자재내역(기입력건)
      IF W_STATUS EQ C_REQ_C.
        SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO.

        SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFPRPYN NE 'Y'.        " 선급금이 아닌 것.
      ELSE.
        SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO.

        SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFPRPYN NE 'Y'.        " 선급금이 아닌 것.
      ENDIF.
*> 수입의뢰 SELECT
      SELECT SINGLE * FROM  ZTREQHD
                      WHERE ZFREQNO   EQ   IT_ZSCIVIT-ZFREQNO.

      IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
        MESSAGE E317 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
      ENDIF.

*>> 통화단위 검증..
      IF ZTCIVHD-ZFIVAMC IS INITIAL.
        ZTCIVHD-ZFIVAMC = ZTREQHD-WAERS.
      ENDIF.
      IF ZTCIVHD-ZFIVAMC NE ZTREQHD-WAERS.
        MESSAGE E379
        WITH ZTCIVHD-ZFIVAMC IT_ZSCIVIT-ZFREQNO ZTREQHD-WAERS.
      ENDIF.
*>> Beneficiay 검증.
      IF ZTCIVHD-ZFMAVN IS INITIAL.
        ZTCIVHD-ZFMAVN  =  ZTREQHD-ZFBENI.
      ENDIF.
      IF ZTCIVHD-ZFMAVN NE ZTREQHD-ZFBENI.
        MESSAGE E381
        WITH ZTCIVHD-ZFMAVN IT_ZSCIVIT-ZFREQNO ZTREQHD-ZFBENI.
      ENDIF.
*>> 회사코드 검증.
      IF ZTCIVHD-BUKRS IS INITIAL.
        ZTCIVHD-BUKRS  =  ZTREQHD-BUKRS.
      ENDIF.
      IF ZTCIVHD-BUKRS NE ZTREQHD-BUKRS.
        MESSAGE E382
           WITH ZTCIVHD-BUKRS IT_ZSCIVIT-ZFREQNO ZTREQHD-BUKRS.
      ENDIF.


*>>> INVOICE 기본 수?
      IT_ZSCIVIT-ZFPRQN = IT_ZSCIVIT-MENGE - IT_ZSCIVIT-CIVTOT.
      IT_ZSCIVIT-CMENGE = IT_ZSCIVIT-MENGE - IT_ZSCIVIT-CIVTOT1.
      IT_ZSCIVIT-EBELN   = ZTREQHD-EBELN.
      IT_ZSCIVIT-EBELP   = IT_ZSCIVIT-ZFITMNO.
      IT_ZSCIVIT-ZFKRW   = 'KRW'.
      IT_ZSCIVIT-ZFIVAMC = ZTCIVHD-ZFIVAMC.

*>>> 선급비율 계산하여 수량 조정...
      IF NOT ZTCIVHD-ZFPRTE IS INITIAL.
        W_MENGE = ( IT_ZSCIVIT-MENGE * ZTCIVHD-ZFPRTE ) / 100.
        IF W_MENGE LE IT_ZSCIVIT-ZFPRQN.
          IT_ZSCIVIT-ZFPRQN = W_MENGE.
        ENDIF.
      ENDIF.

*>>> ITEM DOCUMENT LOCK...
      PERFORM P2000_SET_CIVIT_LOCK_ITEM.

*----> 기존 데이타 갱신.
      IF W_SY_SUBRC EQ 0.
        IT_ZSCIVIT-EBELN = ZTREQHD-EBELN.
        MODIFY IT_ZSCIVIT  INDEX W_TABIX.
      ELSE.
*----> 신규 데이타 추가.
        IT_ZSCIVIT-ZFBLIT = TC_3520-CURRENT_LINE * 10.
        IT_ZSCIVIT-EBELN  = ZTREQHD-EBELN.
        APPEND  IT_ZSCIVIT.
      ENDIF.
      EXIT.
    ENDIF.

    MOVE-CORRESPONDING ZSCIVIT  TO IT_ZSCIVIT.
*---> 기존 데이타가 있었을 경우.
    IF W_SY_SUBRC EQ 0.
      DELETE IT_ZSCIVIT  INDEX W_TABIX.
    ELSE.
      IT_ZSCIVIT-ZFCIVSQ = TC_3513-CURRENT_LINE * 10.
    ENDIF.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSCIVIT_TMP
             FROM ZTREQIT
             WHERE ZFREQNO EQ ZSCIVIT-ZFREQNO.

    LOOP AT IT_ZSCIVIT_TMP.
      MOVE-CORRESPONDING   IT_ZSCIVIT_TMP TO  IT_ZSCIVIT.
*-----------------------------------------------------------------------
* 기존에 해당 수입의뢰 아이템이 입력되었을 경우 체크.
      READ TABLE  IT_ZSCIVIT WITH KEY ZFREQNO = IT_ZSCIVIT-ZFREQNO
                                      ZFITMNO = IT_ZSCIVIT-ZFITMNO.
      IF SY-SUBRC EQ 0 AND W_SY_SUBRC NE 0.
        CLEAR : ZSCIVIT.
        MESSAGE S358 WITH IT_ZSCIVIT-ZFREQNO IT_ZSCIVIT-ZFITMNO
                          IT_ZSCIVIT-ZFCIVSQ.
        CONTINUE.
      ENDIF.
*-----------------------------------------------------------------------
*++++> P/O DATA 조회.
      SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                    BPUMN BPUMZ
             INTO (IT_ZSCIVIT-MENGE_PO, IT_ZSCIVIT-UEBTO,
                   IT_ZSCIVIT-UEBTK,    IT_ZSCIVIT-WEPOS,
                   IT_ZSCIVIT-ELIKZ,    IT_ZSCIVIT-LOEKZ,
                   IT_ZSCIVIT-UNTTO,
                   IT_ZSCIVIT-BPUMN,    IT_ZSCIVIT-BPUMZ)
             FROM   EKPO
             WHERE  EBELN   EQ   ZTREQHD-EBELN
             AND    EBELP   EQ   IT_ZSCIVIT-ZFITMNO.

      IF SY-SUBRC EQ 0.
        IF IT_ZSCIVIT-LOEKZ NE SPACE.
          CLEAR : ZSCIVIT.
          MESSAGE W069 WITH ZTREQHD-EBELN IT_ZSCIVIT-ZFITMNO.
          CONTINUE.
        ENDIF.
*           IF IT_ZSCIVIT-ELIKZ EQ 'X'.
*              CLEAR : ZSCIVIT.
*              MESSAGE W359 WITH ZTREQHD-EBELN IT_ZSCIVIT-ZFITMNO.
*              CONTINUE.
*           ENDIF.
      ELSE.
        CLEAR : ZSCIVIT.
        MESSAGE W071 WITH ZTREQHD-EBELN IT_ZSCIVIT-ZFITMNO.
        CONTINUE.
      ENDIF.
*----> INVOICE 자재내역(기입력건)
      IF W_STATUS EQ C_REQ_C.
        SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO.

        SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFPRPYN NE 'Y'.        " 선급금이 아닌 것.

      ELSE.
        SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO.

        SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFPRPYN NE 'Y'.        " 선급금이 아닌 것.
      ENDIF.
*> 수입의뢰 SELECT
      SELECT SINGLE * FROM  ZTREQHD
                      WHERE ZFREQNO   EQ   IT_ZSCIVIT-ZFREQNO.

      IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
        MESSAGE E317 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
      ENDIF.

*>> 통화단위 검증..
      IF ZTCIVHD-ZFIVAMC IS INITIAL.
        ZTCIVHD-ZFIVAMC = ZTREQHD-WAERS.
      ENDIF.
      IF ZTCIVHD-ZFIVAMC NE ZTREQHD-WAERS.
        MESSAGE E379
        WITH ZTCIVHD-ZFIVAMC IT_ZSCIVIT-ZFREQNO ZTREQHD-WAERS.
      ENDIF.
*>> Beneficiay 검증.
      IF ZTCIVHD-ZFMAVN IS INITIAL.
        ZTCIVHD-ZFMAVN  =  ZTREQHD-ZFBENI.
      ENDIF.
      IF ZTCIVHD-ZFMAVN NE ZTREQHD-ZFBENI.
        MESSAGE E381
        WITH ZTCIVHD-ZFMAVN IT_ZSCIVIT-ZFREQNO ZTREQHD-ZFBENI.
      ENDIF.
*>> 회사코드 검증.
      IF ZTCIVHD-BUKRS IS INITIAL.
        ZTCIVHD-BUKRS  =  ZTREQHD-BUKRS.
      ENDIF.
      IF ZTCIVHD-BUKRS NE ZTREQHD-BUKRS.
        MESSAGE E382
           WITH ZTCIVHD-BUKRS IT_ZSCIVIT-ZFREQNO ZTREQHD-BUKRS.
      ENDIF.


*>>> INVOICE 기본 수?
      IT_ZSCIVIT-ZFPRQN = IT_ZSCIVIT-MENGE - IT_ZSCIVIT-CIVTOT.
      IT_ZSCIVIT-CMENGE = IT_ZSCIVIT-MENGE - IT_ZSCIVIT-CIVTOT1.
      IT_ZSCIVIT-EBELN   = ZTREQHD-EBELN.
      IT_ZSCIVIT-EBELP   = IT_ZSCIVIT-ZFITMNO.
      IT_ZSCIVIT-ZFKRW   = 'KRW'.
      IT_ZSCIVIT-ZFIVAMC = ZTCIVHD-ZFIVAMC.

*>>> 선급비율 계산하여 수량 조정...
      IF NOT ZTCIVHD-ZFPRTE IS INITIAL.
        W_MENGE = ( IT_ZSCIVIT-MENGE * ZTCIVHD-ZFPRTE ) / 100.
        IF W_MENGE LE IT_ZSCIVIT-ZFPRQN.
          IT_ZSCIVIT-ZFPRQN = W_MENGE.
        ENDIF.
      ENDIF.
*>>> ITEM DOCUMENT LOCK...
      PERFORM P2000_SET_CIVIT_LOCK_ITEM.

*----> 중간에 삽입.
      IF W_SY_SUBRC EQ 0.
        IT_ZSCIVIT-ZFCIVSQ = W_TABIX * 10.
        INSERT IT_ZSCIVIT  INDEX W_TABIX.
        ADD 1 TO W_TABIX.
      ELSE.
*----> 마지막에 추가.
        APPEND IT_ZSCIVIT.
        IT_ZSCIVIT-ZFCIVSQ = IT_ZSCIVIT-ZFCIVSQ + 10.
      ENDIF.
*     ENDSELECT.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " IT_ZSCIVIT_UPDATE_SCR3520  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR3520_MARK_TC_3520  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR3520_MARK_TC_3520 INPUT.

  READ TABLE IT_ZSCIVIT  WITH KEY
                         ZFCIVSQ = ZSCIVIT-ZFCIVSQ  BINARY SEARCH.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSCIVIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSCIVIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSCIVIT INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR3520_MARK_TC_3520  INPUT
