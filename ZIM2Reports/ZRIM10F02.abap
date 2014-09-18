*----------------------------------------------------------------------*
***INCLUDE ZRIM10F02 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P2000_TRCOST_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_TRCOST_POST.

  IF W_STATUS NE C_REQ_D.
*> 변경여부 체크.
    PERFORM P2000_MODIFY_CHECK USING W_MODIF_BIT.
    IF W_MODIF_BIT EQ 'Y'.
      PERFORM P2000_SET_MESSAGE USING  OK-CODE.
    ELSE.
      ANTWORT = 'Y'.
    ENDIF.
  ELSE.
    ANTWORT = 'Y'.
  ENDIF.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      IF W_STATUS    NE C_REQ_D AND  W_MODIF_BIT EQ 'Y'.
        PERFORM  P3000_DB_MODIFY_SCRCOM.
      ELSE.
        PERFORM  P2000_POST_DATA_MOVE.
      ENDIF.
*---- 전기 관련----------------------------->
      MOVE :
        SY-DATUM        TO ZTBKPF-BUDAT,      " 전표전기일.
        SY-DATUM        TO ZTBKPF-BLDAT,      " 전표증빙일.
        SY-DATUM        TO ZTBKPF-ZFBDT,      " 기준일.
        SY-DATUM+4(2)   TO ZTBKPF-MONAT.      " 전기월.
      PERFORM P2000_MESSAGE_POSTING.

      IF ANTWORT EQ 'Y'.
        PERFORM P2000_POSTING_ZTTRCOST.
      ENDIF.
*--------------------------------------------->
      CLEAR OK-CODE.
      PERFORM  P2000_SET_UNLOCK.
      PERFORM  P2000_SET_SCREEN_SCRCOM.
      LEAVE SCREEN.
    WHEN 'N'.              " No...
      CLEAR OK-CODE.
      PERFORM  P2000_SET_UNLOCK.
  ENDCASE.
ENDFORM.                    " P2000_TRCOST_POST
*&---------------------------------------------------------------------*
*&      Form  P2000_POST_DATA_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_POST_DATA_MOVE.

*> 유무환 여부.
  LOOP AT IT_ZSTRCST.
    SELECT SINGLE * FROM ZTBL
                   WHERE ZFBLNO = IT_ZSTRCST-ZFBLNO
                     AND ZFPOYN NE 'N'.
    IF SY-SUBRC EQ 0 .
      W_POYN_CK = 'Y'.
      EXIT.
    ELSE.
      W_POYN_CK = 'N'.
    ENDIF.
  ENDLOOP.


  MOVE: SY-MANDT        TO ZTBKPF-MANDT,      " CLIENT
        SY-UNAME        TO ZTBKPF-ZFCNAME,    " 입력담당자.
        'X'             TO ZTBKPF-ZFPCUR,     " 현지통화여부.
        ZTTRHD-BUKRS    TO ZTBKPF-BUKRS,      " 회사코드.
        ZTTRHD-ZFTRNO   TO ZTBKPF-ZFIMDNO,    " 문서번호.
        SY-DATUM(4)     TO ZTBKPF-GJAHR,      " 회계연도.
        '009'           TO ZTBKPF-ZFCSTGRP,   " 비용그룹.
        W_POYN_CK       TO ZTBKPF-ZFPOYN,     " 유환여부.
        'RE'            TO ZTBKPF-BLART,      " 문서유형.
        1               TO ZTBKPF-KURSF,      " 환율.
        SY-DATUM        TO ZTBKPF-WWERT,      " 환산일.
        SY-DATUM        TO ZTBKPF-BLDAT,      " 증빙일.(추가)
        SY-DATUM        TO ZTBKPF-BUDAT,      " 전기일.(추가)
        SY-DATUM        TO ZTBKPF-ZFBDT,      " 만기계산기준일.(추가)
        ' '             TO ZTBKPF-ZLSPR,      " 지급보류키.
        '수송비(운반비/인건비)'
                         TO ZTBKPF-BKTXT,      " HEADER TEXT.
        T001-WAERS       TO ZTBKPF-HWAER,      " 현지통화키.
*        ZTBKPF-WRBTR     TO *ZTBKPF-WRBTR,     " 전표통화금액.
*        T001-WAERS       TO *ZTBKPF-WAERS,     " 통화키.
        ZTBKPF-WRBTR     TO ZTBKPF-DMBTR,      " 현지통화금액.
        T001-WAERS       TO ZTBKPF-WAERS,      " 통화키.
        'X'              TO ZTBKPF-ZFAUTO,     " 자동전기여부.
        'X'              TO ZTBKPF-ZFATPT,     " 자동생성여부.
*        'N'              TO ZTBKPF-ZFPOSYN,    " 전표처리여부.
         ZTTRHD-ZFTRCO   TO ZTBKPF-LIFNR,      " VENDOR CODE
         ZTTRHD-ZFTRCO   TO ZTBKPF-ZFVEN.      " VENDOR CODE.

*> 세부내역.-----------------------------------------------------------
  LOOP AT IT_ZSBSEG.
    IF IT_ZSBSEG-WRBTR EQ 0.
      DELETE IT_ZSBSEG INDEX SY-TABIX.
    ELSE.
      MOVE : ZTTRHD-ZFTRNO    TO   IT_ZSBSEG-ZFIMDNO,
             ZTBKPF-MWSKZ     TO   IT_ZSBSEG-MWSKZ,
             W_POYN_CK        TO   IT_ZSBSEG-ZFPOYN,
             IT_ZSBSEG-WRBTR  TO   IT_ZSBSEG-DMBTR,    " 현지통화금액.
             ZTBKPF-BUKRS     TO   IT_ZSBSEG-BUKRS,
             ZTBKPF-BELNR     TO   IT_ZSBSEG-BELNR,
             ZTBKPF-GJAHR     TO   IT_ZSBSEG-GJAHR.

      MODIFY IT_ZSBSEG INDEX SY-TABIX.
    ENDIF.
  ENDLOOP.
*--------------------------------------------------------------------->

ENDFORM.                    " P2000_POST_DATA_MOVE
*&----------------------------------------------------------------------
*&      Form  P2000_MESSAGE_POSTING
*&----------------------------------------------------------------------
FORM P2000_MESSAGE_POSTING.

  PERFORM P2000_POSTING_MESSAGE USING '전기 확인'     " 타이틀...
                               '선택된자료를 전기합니다 '
                               '전기하시겠습니까?'          " MSG2
                               'N'                 " 취소 버튼 유/?
                               '1'.                " default button


ENDFORM.                    " P2000_MESSAGE_POSTING

*&---------------------------------------------------------------------*
*&      Form  P2000_POSTING_ZTTRCOST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_POSTING_ZTTRCOST.

  CLEAR :IT_ERR_LIST. REFRESH IT_ERR_LIST.
*>사업영역.
  SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
                      FROM T134G
                      WHERE WERKS EQ ZTTRHD-WERKS.

*>
  SELECT SINGLE J_1BBRANCH INTO ZTBKPF-BUPLA
                           FROM T001W
                           WHERE WERKS EQ ZTTRHD-WERKS.

  MOVE : ZTTRHD-BUKRS            TO ZTBKPF-BUKRS.

  LOOP AT IT_ZSBSEG.
    PERFORM P1000_IMPORT_DOC_CHEKC
                                     USING IT_ZSBSEG-ZFIMDNO
                                           IT_ZSBSEG-ZFDCNM
                                           IT_ZSBSEG-ZFPOYN
                                           'I'
                                           IT_ZSBSEG-KOSTL.

    PERFORM  P2000_SET_NEWKO
                              USING IT_ZSBSEG-NEWKO
                                    IT_ZSBSEG-ZFCD
                                    IT_ZSBSEG-ZFIMDNO.

    MODIFY IT_ZSBSEG INDEX SY-TABIX.
  ENDLOOP.

*> 회계전기.
  PERFORM P3000_FI_POSTING.

  DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
  IF W_LINE GT 0.
    INCLUDE = 'POPU'.
    CALL SCREEN 0014 STARTING AT  05   3
                     ENDING   AT  110 12.
    CLEAR : INCLUDE.
  ENDIF.

ENDFORM.                    " P2000_POSTING_ZTTRCOST
*&----------------------------------------------------------------------
*&      Form  P3000_FI_POSTING
*&----------------------------------------------------------------------
FORM P3000_FI_POSTING.
  DATA : L_ZTBKPF LIKE ZTBKPF.

  SET UPDATE TASK LOCAL.
  CLEAR : L_ZTBKPF.
  CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
    EXPORTING
      W_OK_CODE    = 'UPDT'
      BUKRS        = ZTBKPF-BUKRS
      GJAHR        = ZTBKPF-GJAHR
      ZFSTATUS     = ' '
      W_ZTBKPF_OLD = L_ZTBKPF
      W_ZTBKPF     = ZTBKPF
    TABLES
      IT_ZSBSEG    = IT_ZSBSEG
    CHANGING
      BELNR        = ZTBKPF-BELNR
    EXCEPTIONS
      ERROR_UPDATE = 4.

  W_SUBRC = SY-SUBRC.
  IF SY-SUBRC NE 0.
    MESSAGE S494.
    PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                USING  'E'.
    ROLLBACK WORK.
    EXIT.
  ENDIF.

  REFRESH : RETURN.
  CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_POST'
       EXPORTING
          BUKRS             =     ZTBKPF-BUKRS
          BELNR             =     ZTBKPF-BELNR
          GJAHR             =     ZTBKPF-GJAHR
*          MODE              =     'A'
       IMPORTING
          INVOICEDOCNUMBER  =     INVOICEDOCNUMBER
          FISCALYEAR        =     FISCALYEAR
       TABLES
          RETURN            =     RETURN
       EXCEPTIONS
          POST_ERROR        =     4.

  IF SY-SUBRC EQ 0.
*     PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
*                                  USING   'S'.
    PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
    SELECT SINGLE * FROM ZTTRHD
           WHERE    ZFTRNO  EQ ZTTRHD-ZFTRNO .

    ZTTRHD-BELNR = ZTBKPF-BELNR.
    ZTTRHD-GJAHR = ZTBKPF-GJAHR.

    MODIFY ZTTRHD.

    IF SY-SUBRC NE 0.
      MESSAGE E429 WITH 'Transportation cost data'.
    ENDIF.

    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.

    CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
      EXPORTING
        W_OK_CODE    = 'DELE'
        BUKRS        = ZTBKPF-BUKRS
        GJAHR        = ZTBKPF-GJAHR
        ZFSTATUS     = 'U'
        W_ZTBKPF_OLD = L_ZTBKPF
        W_ZTBKPF     = ZTBKPF
      TABLES
        IT_ZSBSEG    = IT_ZSBSEG
      CHANGING
        BELNR        = ZTBKPF-BELNR
      EXCEPTIONS
        ERROR_UPDATE = 4.

    IF RETURN[] IS INITIAL.
      MESSAGE S494.
      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                  USING  'E'.
    ELSE.
      PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_FI_POSTING
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_COST_DOCUMENT
*&---------------------------------------------------------------------*
FORM P2000_SHOW_COST_DOCUMENT.
*> 변경여부 체크.
  IF W_STATUS NE C_REQ_D.
    PERFORM P2000_MODIFY_CHECK USING W_MODIF_BIT.
    IF W_MODIF_BIT EQ 'Y'.
      PERFORM P2000_SET_MESSAGE USING  OK-CODE.
    ELSE.
      ANTWORT = 'Y'.
    ENDIF.

    CASE ANTWORT.
      WHEN 'Y'.              " Yes...
        IF W_MODIF_BIT EQ 'Y'.
          PERFORM  P3000_DB_MODIFY_SCRCOM.
        ENDIF.
        CLEAR OK-CODE.
        PERFORM  P2000_SET_UNLOCK.
      WHEN 'N'.
        CLEAR OK-CODE.
        PERFORM  P2000_SET_UNLOCK.
    ENDCASE.
  ENDIF.

  IF ZTBKPF-ZFPOSYN EQ 'N'.
    MESSAGE S588.
    EXIT.
  ENDIF.
  SET  PARAMETER ID  'BUK'       FIELD   ZTBKPF-BUKRS.
  SET  PARAMETER ID  'GJR'       FIELD   ZTBKPF-GJAHR.
  SET  PARAMETER ID  'ZPBENR'    FIELD   ZTBKPF-BELNR.
  CALL TRANSACTION 'ZIMY3'.

ENDFORM.                    " P2000_SHOW_COST_DOCUMENT
*&----------------------------------------------------------------------
*&      Form  P2000_MULTI_MSG_MAKE
*&----------------------------------------------------------------------
FORM P2000_MULTI_MSG_MAKE TABLES   IT_ERR_LIST STRUCTURE IT_ERR_LIST.

  LOOP AT  RETURN.

    MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
           RETURN-ID           TO     IT_ERR_LIST-MSGID,
           RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
           RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
           RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
           RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
           RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
           RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT.

    CASE IT_ERR_LIST-MSGTYP.
      WHEN 'E' OR 'A'.
        MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
      WHEN 'I'.
        MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'S'.
        MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'W'.
        MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
    ENDCASE.
    APPEND  IT_ERR_LIST.

  ENDLOOP.

ENDFORM.                    " P2000_MULTI_MSG_MAKE
*&----------------------------------------------------------------------
*&      Form  P2000_MESSAGE_MAKE
*&----------------------------------------------------------------------
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST
                         USING   P_MSGTY.

  MOVE : P_MSGTY             TO     IT_ERR_LIST-MSGTYP,
         SY-MSGID            TO     IT_ERR_LIST-MSGID,
         SY-MSGNO            TO     IT_ERR_LIST-MSGNR,
         SY-MSGV1            TO     IT_ERR_LIST-MSGV1,
         SY-MSGV2            TO     IT_ERR_LIST-MSGV2,
         SY-MSGV3            TO     IT_ERR_LIST-MSGV3,
         SY-MSGV4            TO     IT_ERR_LIST-MSGV4.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = IT_ERR_LIST-MSGID
      MSGNR               = IT_ERR_LIST-MSGNR
      MSGV1               = IT_ERR_LIST-MSGV1
      MSGV2               = IT_ERR_LIST-MSGV2
      MSGV3               = IT_ERR_LIST-MSGV3
      MSGV4               = IT_ERR_LIST-MSGV4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = IT_ERR_LIST-MESSTXT.

  CASE IT_ERR_LIST-MSGTYP.
    WHEN 'E' OR 'A'.
      MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
    WHEN 'I'.
      MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
    WHEN 'S'.
      MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
    WHEN 'W'.
      MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
  ENDCASE.

  APPEND  IT_ERR_LIST.

ENDFORM.                    " P2000_MESSAGE_MAKE
*&----------------------------------------------------------------------
*&      Form  P1000_IMPORT_DOC_CHEKC
*&----------------------------------------------------------------------
FORM P1000_IMPORT_DOC_CHEKC USING    P_ZFIMDNO
                                     P_ZFDCNM
                                     P_ZFPOYN
                                     P_GUBUN
                                     P_KOSTL.

  CLEAR : P_ZFDCNM, ZTREQHD, ZTBL, ZTIV, ZTCGHD, ZTMSHD.

  IF P_ZFIMDNO IS INITIAL.  EXIT.  ENDIF.

  CASE ZTBKPF-ZFCSTGRP.
    WHEN '003'.           ">수입의뢰.
      SELECT SINGLE * FROM ZTREQHD
                      WHERE ZFREQNO EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      MOVE: ZTREQHD-ZFOPNNO TO P_ZFDCNM.
      P_ZFPOYN = 'Y'.
    WHEN '004' OR '005'.  ">B/L 관리번호.
      SELECT SINGLE * FROM ZTBL
                      WHERE ZFBLNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      IF W_SUBRC EQ 0.
        P_ZFPOYN = ZTBL-ZFPOYN.
        P_KOSTL  = ZTBL-KOSTL.
      ELSE.
        P_ZFPOYN = 'Y'.
      ENDIF.
*> 1. 화물관리번호.
      IF NOT ZTBL-ZFGMNO IS INITIAL.
        MOVE: ZTBL-ZFGMNO TO P_ZFDCNM.
        IF NOT ZTBL-ZFMSN IS INITIAL.
          CONCATENATE P_ZFDCNM '-' ZTBL-ZFMSN INTO P_ZFDCNM.
        ENDIF.
        IF NOT ZTBL-ZFHSN IS INITIAL.
          CONCATENATE P_ZFDCNM '-' ZTBL-ZFHSN INTO P_ZFDCNM.
        ENDIF.
      ELSE.
*> 2. HOUSE B/L No.
        IF NOT ZTBL-ZFHBLNO IS INITIAL.
          MOVE: ZTBL-ZFHBLNO TO P_ZFDCNM.
        ELSE.
*> 3. MASTER B/L No.
          IF NOT ZTBL-ZFMBLNO IS INITIAL.
            MOVE: ZTBL-ZFMBLNO TO P_ZFDCNM.
          ELSE.
*> 4. 선사 B/L No.
            IF NOT ZTBL-ZFCGHNO IS INITIAL.
              MOVE: ZTBL-ZFCGHNO TO P_ZFDCNM.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
* CORECESS 주석처리.
*         IF W_SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTBL-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA.
*         ENDIF.
*
    WHEN '006'.           ">통관관리번호.
      SELECT SINGLE * FROM ZTIV
                      WHERE ZFIVNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.

      IF W_SUBRC EQ 0.
        P_ZFPOYN = ZTIV-ZFPOYN.
      ELSE.
        P_ZFPOYN = 'Y'.
      ENDIF.

      IF W_SUBRC EQ 0.
        IF ZTIV-ZFCUST EQ '3' OR ZTIV-ZFCUST EQ 'Y'.
          SELECT SINGLE * FROM ZTIDR
                          WHERE ZFIVNO EQ P_ZFIMDNO.
          IF SY-SUBRC EQ 0.
            IF ZTIDR-ZFIDRNO IS INITIAL.
              MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
            ELSE.
              MOVE: ZTIDR-ZFIDRNO TO P_ZFDCNM.
            ENDIF.
          ELSE.
            MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
          ENDIF.
        ELSE.
          MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
        ENDIF.
      ENDIF.
      SELECT SINGLE * FROM ZTBL
             WHERE ZFBLNO EQ ZTIV-ZFBLNO.

* CORECESS 주석처리.
*         IF SY-SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*            P_KOSTL  = ZTBL-KOSTL.
*            P_ZFPOYN = ZTBL-ZFPOYN.
*
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTBL-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA.
*         ENDIF.

    WHEN '007'.           ">하역관리번호.
      SELECT SINGLE * FROM ZTCGHD
                      WHERE ZFCGNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      P_ZFPOYN = 'Y'.
      IF SY-SUBRC EQ 0.
        IF NOT ZTCGHD-ZFMSNO IS INITIAL.
          SELECT SINGLE * FROM  ZTMSHD
                          WHERE ZFMSNO  EQ  ZTCGHD-ZFMSNO.
          IF SY-SUBRC EQ 0.
            MOVE ZTMSHD-ZFMSNM  TO  P_ZFDCNM.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '008'.           ">기납증리번호.
      SELECT SINGLE * FROM ZTTAXBKHD
                      WHERE ZFTBNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      IF SY-SUBRC EQ 0.
        IF ZTTAXBKHD-BASISNO IS INITIAL.
          MOVE ZTTAXBKHD-EBELN    TO  P_ZFDCNM.
        ELSE.
          MOVE ZTTAXBKHD-BASISNO  TO  P_ZFDCNM.
        ENDIF.
      ELSE.
        CLEAR : P_ZFDCNM.
      ENDIF.
      SELECT SINGLE * FROM ZTREQHD
             WHERE ZFREQNO EQ ZTTAXBKHD-ZFREQNO.
*          IF SY-SUBRC EQ 0.
* CORECESS 주석처리.
*         IF SY-SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTREQHD-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA.
*         ENDIF.
    WHEN '009'.           ">수송관리번호.
      SELECT SINGLE * FROM ZTTRHD
                      WHERE ZFTRNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      IF SY-SUBRC EQ 0.
        CLEAR : P_ZFDCNM.
      ENDIF.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

*>>오류가 발생했을 경우.
  IF SY-SUBRC NE 0.
    CASE ZTBKPF-ZFCSTGRP.
      WHEN '003'.
        MESSAGE E585 WITH 'Import request No'   P_ZFIMDNO.
      WHEN '004' OR '005'.
        MESSAGE E585 WITH 'B/L Doc No'   P_ZFIMDNO.
      WHEN '006'.
        MESSAGE E585 WITH 'Clearance No'   P_ZFIMDNO.
      WHEN '007'.
        MESSAGE E585 WITH 'Loading/Unloading No'   P_ZFIMDNO.
      WHEN '008'.
        MESSAGE E585 WITH 'CTM No' P_ZFIMDNO.
      WHEN '009'.
        MESSAGE E585 WITH 'Transportation No'   P_ZFIMDNO.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P1000_IMPORT_DOC_CHEKC
*&----------------------------------------------------------------------
*&      Form  P2000_SET_NEWKO
*&----------------------------------------------------------------------
FORM P2000_SET_NEWKO   USING ZSBSEG-NEWKO
                             ZSBSEG-ZFCD
                             ZSBSEG-ZFIMDNO.
*> 계정결정 함수.
  CALL FUNCTION 'ZIM_GET_NODRAFT_ACCOUNT'
    EXPORTING
      ZFCSTGRP = '009'
      ZFCD     = ZSBSEG-ZFCD
      ZFIMDNO  = ZSBSEG-ZFIMDNO
    IMPORTING
      NEWKO    = ZSBSEG-NEWKO.

ENDFORM.                    " P2000_SET_NEWKO
*&----------------------------------------------------------------------
*&      Form  P2000_POSTING_CANCLE
*&----------------------------------------------------------------------
FORM P2000_POSTING_CANCLE.

  REFRESH : IT_ERR_LIST.


  SELECT  SINGLE * FROM ZTBKPF
  WHERE   BUKRS    EQ   ZTBKPF-BUKRS
    AND   BELNR    EQ   ZTBKPF-BELNR
    AND   GJAHR    EQ   ZTBKPF-GJAHR.

  IF ZTBKPF-ZFPOSYN EQ 'N'.
    MESSAGE  S577.
    PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                 USING   'E'.
    EXIT.
  ENDIF.

*>> AP POSTING FUNCTION CALL
  CALL FUNCTION 'ZIM_BAPI_COST_INVOICES_CANCEL'
    EXPORTING
      P_ZFIMDTY                 = 'CS'
      P_BUKRS                   = ZTBKPF-BUKRS
      INVOICEDOCNUMBER          = ZTBKPF-ZFACDO
      FISCALYEAR                = ZTBKPF-ZFFIYR
      REASONREVERSAL            = UF05A-STGRD
      POSTINGDATE               = BSIS-BUDAT
    IMPORTING
      INVOICEDOCNUMBER_REVERSAL = INVOICEDOCNUMBER
      FISCALYEAR_REVERSAL       = FISCALYEAR
    TABLES
      RETURN                    = RETURN
    EXCEPTIONS
      LIV_ERROR                 = 4.

  IF SY-SUBRC NE 0.           ">> 오류 발생시...
    IF RETURN[] IS INITIAL.
      MESSAGE  S691.
      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'E'.
    ELSE.
      PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
    ENDIF.
  ELSE.
    PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'S'.

    IF SY-SUBRC NE 0.
      MESSAGE E429 WITH 'Transportation cost data'.
    ENDIF.
  ENDIF.

  DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
  IF W_LINE GT 0.
    INCLUDE = 'POPU'.
    CALL SCREEN 0014 STARTING AT  05   3
                     ENDING   AT  110 12.
    CLEAR : INCLUDE.
  ENDIF.


ENDFORM.                    " P2000_POSTING_CANCLE
*&---------------------------------------------------------------------*
*&      Form  P2000_TRCOST_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_TRCOST_CANCEL.

  SELECT SINGLE * FROM ZTIMIMG00.
*>> POPUP WINDOWS-->전기취소용.
  IF ZTIMIMG00-CSREALYN EQ 'X'.
     ANTWORT  =  'Y'.
  ELSE.
     MOVE '수송비용 전기취소' TO SPOP-TITEL.
     IF BSIS-BUDAT IS INITIAL.
        MOVE SY-DATUM    TO BSIS-BUDAT.
     ENDIF.

     CALL SCREEN 0021 STARTING AT 25 3
                      ENDING   AT 65 9.
  ENDIF.
  IF ANTWORT EQ 'Y'.
    PERFORM P2000_POSTING_CANCLE.
    PERFORM  P2000_SET_SCREEN_SCRCOM.
    LEAVE SCREEN.
  ENDIF.
ENDFORM.                    " P2000_TRCOST_CANCEL
*&---------------------------------------------------------------------*
*&      Form  P2000_TR_REPORT_CALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_TR_REPORT_CALL.

  CASE OK-CODE.
    WHEN 'PROR'.
      SUBMIT ZRIMTRORDER WITH P_ZFTRNO EQ ZTTRHD-ZFTRNO
                        AND RETURN.
      SELECT SINGLE ZFRELST INTO ZTTRHD-ZFRELST
             FROM ZTTRHD
            WHERE ZFTRNO = ZTTRHD-ZFTRNO.

    WHEN 'PRSD'.
      REFRESH : RG_SEL.
      CLEAR   : RG_SEL.
      MOVE : 'I'           TO RG_SEL-SIGN,
             'EQ'          TO RG_SEL-OPTION,
             ZTTRHD-ZFTRNO TO RG_SEL-LOW.
      APPEND RG_SEL.

      SUBMIT ZRIMTRSEND WITH S_ZFTRNO IN RG_SEL
                        AND RETURN.

    WHEN 'PRPL'.
      SUBMIT ZRIMTRROUT WITH P_ZFTRNO EQ ZTTRHD-ZFTRNO
                        AND RETURN.

  ENDCASE.
ENDFORM.                    " P2000_TR_REPORT_CALL
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CHARGE_RECORD
*&---------------------------------------------------------------------*
FORM P1000_READ_CHARGE_RECORD.

  REFRESH : IT_ZSIMIMG08, IT_ZSBSEG.
*    DELETE IT_ZSBLCST WHERE ZFACDO EQ SPACE.

  CLEAR : W_LFA1, W_ZFCSQ.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG08
                                        FROM ZTIMIMG08
                                        WHERE ZFCDTY EQ '009'
                                        ORDER BY ZFCD.
  LOOP AT IT_ZSIMIMG08.
    CLEAR : IT_ZSBSEG.

    MOVE : ZTTRHD-ZFTRNO             TO IT_ZSBSEG-ZFIMDNO,
           SPACE                      TO IT_ZSBSEG-KOSTL,
           '009'                      TO IT_ZSBSEG-ZFCSTGRP,
           IT_ZSIMIMG08-COND_TYPE     TO IT_ZSBSEG-COND_TYPE,
           IT_ZSIMIMG08-ZFCD          TO IT_ZSBSEG-ZFCD,
*         ZTTRCHD-MWSKZ              TO IT_ZSBSEG-MWSKZ,
           '40'                       TO IT_ZSBSEG-NEWBS,
           'S'                        TO IT_ZSBSEG-SHKZG,
*         ZTTRCHD-ZFMHAP             TO IT_ZSBSEG-WRBTR,
           0                          TO IT_ZSBSEG-WMWST,
*         ZTTRCHD-ZFMHAP             TO IT_ZSBSEG-DMBTR,
           1                          TO IT_ZSBSEG-KURSF,
           SY-DATUM                   TO IT_ZSBSEG-WWERT,
*           P_KOSTL                    TO IT_ZSBSEG-KOSTL,   " 확인.
           IT_ZSIMIMG08-ZFCDNM           TO IT_ZSBSEG-ZFCDNM,
           IT_ZSIMIMG08-ZFCDNM           TO IT_ZSBSEG-SGTXT.

    IF IT_ZSIMIMG08-ZFCD1 EQ 'Y'.
      MOVE  : 'X'  TO  IT_ZSBSEG-ZFDCSTX,
              'X'  TO  ZTBKPF-ZFDCSTX.
    ELSE.
      CLEAR : IT_ZSBSEG-ZFDCSTX, ZTBKPF-ZFDCSTX.
    ENDIF.

    PERFORM  P2000_SET_NEWKO
                              USING IT_ZSBSEG-NEWKO
                                    IT_ZSBSEG-ZFCD
                                    IT_ZSBSEG-ZFIMDNO.

    APPEND IT_ZSBSEG.
  ENDLOOP.
  SORT IT_ZSBSEG BY ZFCD.


ENDFORM.                    " P1000_READ_CHARGE_RECORD
*&---------------------------------------------------------------------*
*&      Form  P2000_CALL_DETAIL_SCR23
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_CALL_DETAIL_SCR23.

  CLEAR : ZTTRCST, IT_ZSTRCSTIT, ZSTRCSTIT.
  REFRESH : IT_ZSTRCSTIT.

  READ TABLE IT_ZSTRCST WITH KEY ZFMARK = 'X'.
  IF SY-SUBRC NE 0.
    MESSAGE S962.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING IT_ZSTRCST TO ZTTRCST.

  LOOP AT IT_ZTTRCSTIT
             WHERE ZFSEQ  = ZTTRCST-ZFSEQ.

    MOVE-CORRESPONDING IT_ZTTRCSTIT TO IT_ZSTRCSTIT.
    APPEND IT_ZSTRCSTIT.
  ENDLOOP.

  CALL SCREEN 0023 STARTING AT  10   20
                   ENDING   AT  104   40.

  IF ANTWORT = 'Y'.
    LOOP AT IT_ZTTRCSTIT WHERE ZFSEQ = ZTTRCST-ZFSEQ.
      W_TABIX    = SY-TABIX.
      READ TABLE IT_ZSTRCSTIT WITH KEY ZFSEQ = IT_ZTTRCSTIT-ZFSEQ
                                     ZFITSEQ = IT_ZTTRCSTIT-ZFITSEQ.
      IF SY-SUBRC NE 0.
        DELETE IT_ZTTRCSTIT INDEX W_TABIX.
      ENDIF.
    ENDLOOP.

    LOOP AT IT_ZSTRCSTIT.
      READ TABLE IT_ZTTRCSTIT WITH KEY ZFSEQ = IT_ZSTRCSTIT-ZFSEQ
                                     ZFITSEQ = IT_ZSTRCSTIT-ZFITSEQ.
      W_TABIX    = SY-TABIX.
      IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING IT_ZSTRCSTIT TO IT_ZTTRCSTIT.
        MODIFY IT_ZTTRCSTIT INDEX W_TABIX.
      ELSE.
        MOVE-CORRESPONDING IT_ZSTRCSTIT TO IT_ZTTRCSTIT.
        APPEND IT_ZTTRCSTIT.
      ENDIF.
    ENDLOOP.

*>> 운반비, 인건비 계산 루틴.
    PERFORM CALCUL_SUM_TRANSCOST.

*>> 총합계산루틴.
    CLEAR : ZTBKPF-WRBTR.

    LOOP AT IT_ZSBSEG.
      ADD IT_ZSBSEG-WRBTR TO ZTBKPF-WRBTR.
    ENDLOOP.

    ZTBKPF-WRBTR = ZTBKPF-WRBTR + ZTBKPF-WMWST.

  ENDIF.


ENDFORM.                    " P2000_CALL_DETAIL_SCR23
*&---------------------------------------------------------------------*
*&      Form  CALCUL_SUM_TRANSCOST
*&---------------------------------------------------------------------*
FORM CALCUL_SUM_TRANSCOST.

  CLEAR : W_THAP, W_MHAP.
  LOOP AT IT_ZTTRCSTIT.
    ADD : IT_ZTTRCSTIT-ZFTRAMT TO W_THAP,
          IT_ZTTRCSTIT-ZFMAMT  TO W_MHAP.
  ENDLOOP.

*> 운반비.
  READ TABLE IT_ZSBSEG WITH KEY ZFCD = '001'.
  IT_ZSBSEG-WRBTR = W_THAP.
  MODIFY IT_ZSBSEG TRANSPORTING WRBTR WHERE ZFCD = '001'.

*> 인건비.
  READ TABLE IT_ZSBSEG WITH KEY ZFCD = '002'.
  IT_ZSBSEG-WRBTR = W_MHAP.
  MODIFY IT_ZSBSEG TRANSPORTING WRBTR WHERE ZFCD = '002'.


ENDFORM.                    " CALCUL_SUM_TRANSCOST
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BLNO_TO_CST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_SET_BLNO_TO_CST .

  LOOP AT IT_ZSTRIT.
    READ TABLE IT_ZSTRCST WITH KEY ZFBLNO = IT_ZSTRIT-ZFBLNO.
    IF SY-SUBRC EQ 0.
      CONTINUE.
    ELSE.
      CLEAR IT_ZSTRCST.
      MOVE IT_ZSTRIT-ZFBLNO TO IT_ZSTRCST-ZFBLNO.
      APPEND IT_ZSTRCST.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " P1000_SET_BLNO_TO_CST

*&---------------------------------------------------------------------*
*&      Form  P2000_REQUESTER_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_REQUESTER_DISPLAY  USING    P_EBELN
                                       P_EBELP.

 DATA : L_AFNAM  LIKE  BAPIBNAME.

 SELECT SINGLE * FROM EKPO
                WHERE EBELN = P_EBELN
                  AND EBELP = P_EBELP.

  IF EKPO-AFNAM IS INITIAL.
    MESSAGE E468(ZIM1).
  ENDIF.

  TRANSLATE EKPO-AFNAM TO UPPER CASE.

  MOVE  EKPO-AFNAM   TO  L_AFNAM.

  CALL FUNCTION 'SUSR_USER_MAINT_WITH_DIALOG'
   EXPORTING
     MAINT_FOR_OWN_USER_ONLY       = 'X'
     DISPLAY_ONLY                  = 'X'
     USER_TO_DISPLAY               = L_AFNAM
   EXCEPTIONS
     ERROR_WRITING_TO_DB           = 1
     OTHERS                        = 2
            .

ENDFORM.                    " P2000_REQUESTER_DISPLAY
