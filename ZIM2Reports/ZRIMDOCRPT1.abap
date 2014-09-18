*&---------------------------------------------------------------------*
*& Report  ZRIMDOCRPT                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 구매오더별 진행관리                                   *
*&      작성자 : 이석철 INFOLINK Ltd.                                  *
*&      작성일 : 2000.06.16                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMDOCRPT   MESSAGE-ID ZIM
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
*      Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMDOCRPTTOP.
INCLUDE   ZRIMUTIL01.

*-----------------------------------------------------------------------
*      Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.               " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS,
                S_EBELN   FOR ZTREQHD-EBELN      " P/O Number
                          MEMORY ID BES,
                S_LIFNR   FOR ZTREQHD-LIFNR,    " vendor
                S_EKGRP   FOR ZTREQST-EKGRP,    " Purch. Grp.
                S_REQNO   FOR ZTREQHD-ZFREQNO   " 수입의뢰 관리번호.
                          MEMORY ID ZPREQNO,
                S_REQDT   FOR ZTREQST-ZFREQDT,   " 요개설일.
                S_MATGB   FOR ZTREQHD-ZFMATGB,   " 자재구분.
                S_REQTY   FOR ZTREQHD-ZFREQTY,   " 수입의뢰 Type
                S_WERKS   FOR ZTREQHD-ZFWERKS,   " 대표 plant
                S_EKORG   FOR ZTREQST-EKORG.     " Purch. Org.
PARAMETERS :    P_NAME    LIKE USR02-BNAME.      " 담당자.
SELECT-OPTIONS: S_DOCST   FOR ZTREQST-ZFDOCST.  " 문서상태.
SELECTION-SCREEN END OF BLOCK B1.

*-----------------------------------------------------------------------
*      AT SELECTION-SCREEN.
*-----------------------------------------------------------------------
AT SELECTION-SCREEN.
  IF  S_REQDT IS INITIAL AND S_MATGB  IS INITIAL AND
      S_REQTY IS INITIAL AND S_WERKS  IS INITIAL AND
      S_EKORG IS INITIAL AND P_NAME   IS INITIAL AND
      S_EBELN IS INITIAL AND S_LIFNR  IS INITIAL AND
      S_EKGRP IS INITIAL AND S_REQNO  IS INITIAL AND
      S_DOCST IS INITIAL.
      MESSAGE E193.
  ENDIF.

*-----------------------------------------------------------------------
* PARAMETER 초기값 Setting
*-----------------------------------------------------------------------
INITIALIZATION.                        " 초기값 SETTING
  PERFORM   P2000_SET_PARAMETER.

*-----------------------------------------------------------------------
*       HEADER 출?
*-----------------------------------------------------------------------
TOP-OF-PAGE.
  PERFORM P3000_WRITE_HEADER.

*-----------------------------------------------------------------------
*      START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

*>> 해당 조건에 대한 수입의뢰 DATA 검색.
   PERFORM   P1000_READ_REQ_DATA       USING   W_ERR_CHK.
   CHECK     W_ERR_CHK  NE  'Y'.

*>> 수입의뢰에 해당하는 PO DATA 검색.
   PERFORM   P1000_READ_PO_DATA.

*>> 수입의뢰에 해당하는 보험부보 DATA 검색.
   PERFORM   P1000_READ_INS_DATA.

*>> 수입의뢰건에 대한 AMEND DATA 검색.
   PERFORM   P1000_READ_AMEND_DATA.

*>> 수입의뢰에 해당하는 COMMERCIAL INVOICE DATA 검색.
   PERFORM   P1000_READ_CIV_DATA.

*>> 수입의뢰건에 해당하는 BL DATA 검색.
   PERFORM   P1000_READ_BL_DATA.

*>> LG DATA 검색.
   PERFORM   P1000_READ_LG_DATA.

*>> 반입 자료 검색.
   PERFORM   P1000_READ_INR_DATA.

*>> 반출자료 검색.
   PERFORM   P1000_READ_OUR_DATA.

*>> 하역 DATA 검색.
   PERFORM   P1000_READ_CG_DATA.

*>> BL 건에 해당하는 과세통관 요청 DATA 검색.
   PERFORM   P1000_READ_CUIV_DATA.

*>> 수입의뢰건에 해당하는 통관요청 DATA 검색.
   PERFORM   P1000_READ_IV_DATA.

*>> 해당 통관요청건에 해당하는 수입신고 DATA 검색.
   PERFORM   P1000_READ_ZTIDR_DATA.

*>> 수입신고건에 해당하는 수입면허 DATA 검색.
   PERFORM   P1000_READ_ZTIDS_DATA.

*>> 통관요청건에 해당하는 입고문서 DATA 검색.
   PERFORM   P1000_READ_IN_DATA.

*-----------------------------------------------------------------------
*      END OF SELECTION
*-----------------------------------------------------------------------
END-OF-SELECTION.

* GUI Status
   SET  TITLEBAR 'ZIM91'.               " TITLE BAR
   SET  PF-STATUS 'ZIM91'.              " PF-STATUS

*>> INTERNAL TABLE SORT.
   PERFORM   P2000_SORT_DATA.
   PERFORM   P3000_WRITE_ALL_DATA.

*-----------------------------------------------------------------------
*      User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
*------- Abbrechen (CNCL) ----------------------------------------------
      WHEN 'CNCL'.
         SET SCREEN 0.    LEAVE SCREEN.
*------- Suchen (SUCH) -------------------------------------------------
      WHEN 'SUCH'.
*------- Sortieren nach Feldbezeichnung (SORB) -------------------------
      WHEN 'SORB'.
*------- Sortieren nach Feldname (SORF) --------------------------------
      WHEN 'SORF'.
*------- Techn. Name ein/aus (TECH) ------------------------------------
      WHEN 'TECH'.
*------- Weiter suchen (WESU) ------------------------------------------
      WHEN 'WESU'.
      WHEN OTHERS.
   ENDCASE.

*-----------------------------------------------------------------------
* LINE 선택시 문서 내용 DISPLAY.
*-----------------------------------------------------------------------
AT LINE-SELECTION.

   DATA : L_TEXT(30).

   GET CURSOR FIELD L_TEXT.

*>> PO 및 구매처 DISPLAY.
   IF L_TEXT(7)  EQ  'IT_EKKO' AND L_TEXT(13)  NE  'IT_EKKO-LIFNR'.
      SELECT SINGLE * FROM EKKO
             WHERE EBELN EQ IT_EKKO-EBELN.

      IF EKKO-BSTYP EQ 'L'.
         SET  PARAMETER  ID  'SAG'  FIELD  IT_EKKO-EBELN.
         CALL  TRANSACTION  'ME33L'  AND  SKIP FIRST SCREEN.
      ELSE.
         SET  PARAMETER  ID  'BES'  FIELD  IT_EKKO-EBELN.
         CALL  TRANSACTION  'ME23N'  AND  SKIP FIRST SCREEN.
      ENDIF.
      EXIT.
   ELSEIF  L_TEXT(13)  EQ  'IT_EKKO-LIFNR'.

      SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
      SET PARAMETER ID 'LIF' FIELD IT_EKKO-LIFNR.
      SET PARAMETER ID 'EKO' FIELD ''.
      EXPORT 'LIF'   TO MEMORY ID 'LIF'.
      EXPORT 'EKO'   TO MEMORY ID 'EKO'.

      CALL TRANSACTION 'MK03' AND SKIP  FIRST SCREEN.
      EXIT.
   ENDIF.
*>> 수입의뢰 DISPLAY
   IF L_TEXT(10)  EQ  'IT_ZTREQHD'.
      SET PARAMETER ID 'ZPOPNNO'  FIELD  SPACE.
      SET PARAMETER ID 'BES'      FIELD  SPACE.
      SET PARAMETER ID 'ZPREQNO'  FIELD  IT_ZTREQHD-ZFREQNO.
      CALL TRANSACTION 'ZIM03'    AND SKIP FIRST SCREEN.
      EXIT.
   ENDIF.
*>> 보험부보 내역 DISPLAY.
   IF L_TEXT(8)  EQ  'IT_ZTINS'.
      IF  IT_ZTINS-ZFAMDNO  GT  '00000'.
          SET PARAMETER ID 'ZPOPNNO'  FIELD  SPACE.
          SET PARAMETER ID 'BES'      FIELD  SPACE.
          SET PARAMETER ID 'ZPREQNO'  FIELD  IT_ZTINS-ZFREQNO.
          SET PARAMETER ID 'ZPINSEQ'  FIELD  IT_ZTINS-ZFINSEQ.
          SET PARAMETER ID 'ZPAMDNO'  FIELD  IT_ZTINS-ZFAMDNO.
          CALL  TRANSACTION 'ZIM47'   AND SKIP FIRST SCREEN.
          EXIT.
      ELSE.
          SET PARAMETER ID 'ZPOPNNO'  FIELD  SPACE.
          SET PARAMETER ID 'BES'      FIELD  SPACE.
          SET PARAMETER ID 'ZPREQNO'  FIELD  IT_ZTINS-ZFREQNO.
          SET PARAMETER ID 'ZPINSEQ'  FIELD  IT_ZTINS-ZFINSEQ.
          CALL  TRANSACTION 'ZIM43'   AND SKIP FIRST SCREEN.
          EXIT.
      ENDIF.
   ENDIF.
*>> AMEND 사항 DISPLAY.
   IF L_TEXT(10)  EQ  'IT_ZTREQST'.
      SET PARAMETER ID 'ZPOPNNO'  FIELD  SPACE.
      SET PARAMETER ID 'BES'      FIELD  SPACE.
      SET PARAMETER ID 'ZPREQNO'  FIELD  IT_ZTREQST-ZFREQNO.
      SET PARAMETER ID 'ZPAMDNO'  FIELD  IT_ZTREQST-ZFAMDNO.
      CALL TRANSACTION 'ZIM13'    AND SKIP FIRST SCREEN.
      EXIT.
   ENDIF.
*>> CIV 사항 DISPLAY.
   IF L_TEXT(6)  EQ  'IT_CIV'.
      SET PARAMETER ID 'ZPCIVNO'  FIELD  IT_CIV-ZFCIVNO.
      SET PARAMETER ID 'ZPCIVRN'  FIELD  IT_CIV-ZFCIVRN.
      CALL TRANSACTION 'ZIM37'    AND SKIP FIRST SCREEN.
      EXIT.
   ENDIF.
*>> BL 사항 DISPLAY
   IF L_TEXT(5)  EQ  'IT_BL'.
      SET PARAMETER ID 'ZPHBLNO' FIELD  SPACE.
      SET PARAMETER ID 'ZPBLNO'  FIELD  IT_BL-ZFBLNO.
      CALL TRANSACTION 'ZIM23'   AND SKIP FIRST SCREEN.
      EXIT.
   ENDIF.
*>> LG 정보 DISPLAY
   IF L_TEXT(7)  EQ  'IT_ZTLG'.
      SET PARAMETER ID 'ZPHBLNO' FIELD  SPACE.
      SET PARAMETER ID 'ZPBLNO'  FIELD  IT_ZTLG-ZFBLNO.
      SET PARAMETER ID 'ZPLGSEQ' FIELD  IT_ZTLG-ZFLGSEQ.
      CALL TRANSACTION 'ZIM28'   AND SKIP FIRST SCREEN.
      EXIT.
   ENDIF.
*>> 하역정보 DISPLAY.
   IF L_TEXT(5)  EQ  'IT_CG'.
      SET PARAMETER ID 'ZPHBLNO' FIELD  SPACE.
      SET PARAMETER ID 'ZPBLNO'  FIELD  IT_CG-ZFBLNO.
      SET PARAMETER ID 'ZPCGPT'  FIELD  IT_CG-ZFCGPT.
      CALL TRANSACTION 'ZIM83'   AND  SKIP FIRST SCREEN.
      EXIT.
   ENDIF.
*>> 보세운송(반입) DISPLAY
   IF L_TEXT(6)  EQ  'IT_INR'.
      SET PARAMETER ID 'BES'     FIELD  SPACE.
      SET PARAMETER ID 'ZPHBLNO' FIELD  SPACE.
      SET PARAMETER ID 'ZPBLNO'  FIELD  IT_INR-ZFBLNO.
      SET PARAMETER ID 'ZPBTSEQ' FIELD  IT_INR-ZFBTSEQ.
      CALL TRANSACTION 'ZIMI8'   AND SKIP FIRST SCREEN.
      EXIT.
   ENDIF.
*>> 보세운송(반출) DISPLAY
   IF L_TEXT(6)  EQ  'IT_OUR'.
      SET PARAMETER ID 'ZPHBLNO' FIELD  SPACE.
      SET PARAMETER ID 'ZPBLNO'  FIELD  IT_OUR-ZFBLNO.
      SET PARAMETER ID 'ZPBTSEQ' FIELD  IT_OUR-ZFBTSEQ.
      CALL TRANSACTION 'ZIMO3'   AND SKIP FIRST SCREEN.
      EXIT.
   ENDIF.
*>> 통관/입고 요청 DISPLAY
   IF L_TEXT(5)  EQ  'IT_IV'.
      SET PARAMETER ID 'ZPHBLNO' FIELD  SPACE.
      SET PARAMETER ID 'ZPBLNO'  FIELD  SPACE.
      SET PARAMETER ID 'ZPIVNO'  FIELD  IT_IV-ZFIVNO.
      CALL TRANSACTION 'ZIM33'   AND  SKIP FIRST SCREEN.
      EXIT.
   ENDIF.
*>> 과세통관 요청 DISPLAY
   IF L_TEXT(9)  EQ  'IT_ZTCUIV'.
      SET PARAMETER ID 'ZPIVNO'  FIELD  IT_ZTCUIV-ZFIVNO.
      CALL TRANSACTION 'ZIM69'   AND  SKIP FIRST SCREEN.
      EXIT.
   ENDIF.
*>> 수입신고 DISPLAY
   IF L_TEXT(8)  EQ  'IT_ZTIDR'.
      SET PARAMETER ID 'ZPIDRNO' FIELD  SPACE.
      SET PARAMETER ID 'ZPBLNO'  FIELD  IT_ZTIDR-ZFBLNO.
      SET PARAMETER ID 'ZPCLSEQ' FIELD  IT_ZTIDR-ZFCLSEQ.
      CALL TRANSACTION 'ZIM63'   AND  SKIP FIRST SCREEN.
      EXIT.
   ENDIF.
*>> 수입면허 DISPLAY
   IF L_TEXT(8)  EQ  'IT_ZTIDS'.
      SET PARAMETER ID 'ZPHBLNO' FIELD  SPACE.
      SET PARAMETER ID 'ZPIDRNO' FIELD  SPACE.
      SET PARAMETER ID 'ZPBLNO'  FIELD  IT_ZTIDS-ZFBLNO.
      SET PARAMETER ID 'ZPCLSEQ' FIELD  IT_ZTIDS-ZFCLSEQ.
      CALL TRANSACTION 'ZIM76'   AND  SKIP FIRST SCREEN.
      EXIT.
   ENDIF.
*>> 입고 전표 DISPLAY.
   IF L_TEXT(5)  EQ  'IT_IN'.
      SET PARAMETER ID 'BUK'    FIELD  IT_IN-BUKRS.
      SET PARAMETER ID 'MBN'    FIELD  IT_IN-MBLNR.
      SET PARAMETER ID 'MJA'    FIELD  IT_IN-MJAHR.
*>> 입고문서 조회 FUNCTION CALL.
      CALL FUNCTION 'MIGO_DIALOG'
         EXPORTING
            i_action                  = 'A04'
            i_refdoc                  = 'R02'
            i_notree                  = 'X'
*           I_NO_AUTH_CHECK           =
            i_skip_first_screen       = 'X'
*           I_DEADEND                 = 'X'
            i_okcode                  = 'OK_GO'
*           I_LEAVE_AFTER_POST        =
*           i_new_rollarea            = 'X'
*           I_SYTCODE                 =
*           I_EBELN                   =
*           I_EBELP                   =
            i_mblnr                   = IT_IN-MBLNR
            i_mjahr                   = IT_IN-MJAHR
*           I_ZEILE                   =
         EXCEPTIONS
            illegal_combination       = 1
            OTHERS                    = 2.

*      CALL TRANSACTION 'MB03'   AND  SKIP FIRST SCREEN.
   ENDIF.


*-----------------------------------------------------------------------
*      Form  P2000_SET_PARAMETER
*-----------------------------------------------------------------------
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR  'ZIM91'.               " TITLE BAR
* SET  PF-STATUS 'ZIM91'.
ENDFORM.                               " P2000_SET_PARAMETER

*-----------------------------------------------------------------------
*      FORM P3000_WRITE_HEADER
*-----------------------------------------------------------------------
FORM P3000_WRITE_HEADER.
  SKIP 1.
  FORMAT COLOR COL_BACKGROUND  INTENSIFIED  ON.
  WRITE : 55  '  [ 문서별 진행 현황 ] '
           COLOR COL_HEADING INTENSIFIED.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_REQ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P1000_READ_REQ_DATA USING    W_ERR_CHK.

   CLEAR   : W_LINE.
   REFRESH : IT_ST, IT_ZTREQHD.

   MOVE  'N'      TO   W_ERR_CHK.
   CONCATENATE P_NAME '%' INTO P_NAME.

   SELECT  ZFREQNO  INTO CORRESPONDING FIELDS OF TABLE IT_ST
   FROM    ZTREQST
   WHERE   ZFREQTY    IN   S_REQTY
   AND     ZFREQNO    IN   S_REQNO
   AND     ZFDOCST    IN   S_DOCST
   AND     CDAT       IN   S_REQDT
   AND     ZFOPNNM    LIKE P_NAME
   GROUP BY
           ZFREQNO.

   SELECT  *   INTO    CORRESPONDING FIELDS OF TABLE  IT_ZTREQHD
   FROM    ZTREQHD
   FOR     ALL  ENTRIES IN   IT_ST
   WHERE   ZFREQNO      EQ   IT_ST-ZFREQNO
   AND     BUKRS        IN   S_BUKRS
   AND     EBELN        IN   S_EBELN
   AND     LIFNR        IN   S_LIFNR
   AND     ZFMATGB      IN   S_MATGB
   AND     ZFWERKS      IN   S_WERKS.

   DESCRIBE  TABLE  IT_ZTREQHD  LINES  W_LINE.
   IF W_LINE EQ 0.
      MESSAGE   S738.
      MOVE   'Y'       TO      W_ERR_CHK.
   ENDIF.

ENDFORM.                    " P1000_READ_REQ_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_PO_DATA.

   REFRESH : IT_EKKO.
   MOVE      'N'        TO    W_ERR_CHK.

   SELECT   *   INTO  CORRESPONDING FIELDS OF TABLE IT_EKKO
   FROM    EKKO
   FOR     ALL   ENTRIES  IN  IT_ZTREQHD
   WHERE   EBELN          EQ  IT_ZTREQHD-EBELN
   AND     EKORG          IN  S_EKORG.

ENDFORM.                    " P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_INS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_INS_DATA.

   REFRESH : IT_ZTINS.

   SELECT   *    INTO  CORRESPONDING FIELDS OF TABLE IT_ZTINS
   FROM    ZTINS
   FOR     ALL    ENTRIES   IN  IT_ZTREQHD
   WHERE   ZFREQNO          EQ  IT_ZTREQHD-ZFREQNO.

ENDFORM.                    " P1000_READ_INS_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_AMEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_AMEND_DATA.

*>> AMEND DATA SELECT.
   SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_ZTREQST
   FROM    ZTREQST
   FOR  ALL  ENTRIES  IN  IT_ZTREQHD
   WHERE   ZFREQNO    EQ  IT_ZTREQHD-ZFREQNO
   AND     ZFAMDNO    NE  '00000'.

ENDFORM.                    " P1000_READ_AMEND_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_CIV_DATA.

   REFRESH :  IT_ZTCIV, IT_CIV.

   SELECT  ZFCIVRN  ZFREQNO
   INTO CORRESPONDING FIELDS OF TABLE IT_ZTCIV
   FROM    ZTCIVIT
   FOR     ALL   ENTRIES   IN     IT_ZTREQHD
   WHERE   ZFREQNO         EQ     IT_ZTREQHD-ZFREQNO
   GROUP BY
           ZFCIVRN  ZFREQNO.

   LOOP  AT  IT_ZTCIV.

      CLEAR   ZTCIVHD.
      SELECT  SINGLE *  FROM  ZTCIVHD
      WHERE   ZFCIVRN   EQ    IT_ZTCIV-ZFCIVRN.

      MOVE-CORRESPONDING ZTCIVHD TO  IT_CIV.
      MOVE  IT_ZTCIV-ZFREQNO     TO  IT_CIV-ZFREQNO.

      APPEND  IT_CIV.

   ENDLOOP.

ENDFORM.                    " P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_BL_DATA.

   REFRESH :  IT_ZTBL, IT_BL.

   SELECT  ZFBLNO  ZFREQNO
   INTO CORRESPONDING FIELDS OF TABLE IT_ZTBL
   FROM    ZTBLIT
   FOR     ALL   ENTRIES   IN     IT_ZTREQHD
   WHERE   ZFREQNO         EQ     IT_ZTREQHD-ZFREQNO
   GROUP BY
           ZFBLNO  ZFREQNO.

   LOOP  AT  IT_ZTBL.

      CLEAR   ZTBL.
      SELECT  SINGLE *  FROM  ZTBL
      WHERE   ZFBLNO    EQ    IT_ZTBL-ZFBLNO.

      MOVE-CORRESPONDING ZTBL    TO  IT_BL.
      MOVE  IT_ZTBL-ZFREQNO      TO  IT_BL-ZFREQNO.

      APPEND  IT_BL.

   ENDLOOP.

ENDFORM.                    " P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_IV_DATA.

   REFRESH : IT_ZTIV, IT_IV.

   SELECT  ZFIVNO    ZFREQNO   ZFBLNO
   INTO    CORRESPONDING FIELDS OF TABLE IT_ZTIV
   FROM    ZTIVIT
   FOR     ALL  ENTRIES  IN  IT_ZTREQHD
   WHERE   ZFREQNO       EQ  IT_ZTREQHD-ZFREQNO
   GROUP BY
           ZFIVNO   ZFREQNO   ZFBLNO.

   LOOP  AT  IT_ZTIV.

      CLEAR  ZTIV.
      SELECT  SINGLE * FROM ZTIV
      WHERE   ZFIVNO   EQ   IT_ZTIV-ZFIVNO.

      MOVE-CORRESPONDING  ZTIV   TO  IT_IV.
      MOVE    IT_ZTIV-ZFREQNO    TO  IT_IV-ZFREQNO.
      MOVE    IT_ZTIV-ZFBLNO     TO  IT_IV-ZFBLNO.

      APPEND  IT_IV.

   ENDLOOP.

ENDFORM.                    " P1000_READ_IT_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIDR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTIDR_DATA.

   REFRESH : IT_ZTIDR, IT_IDR.

   SELECT   ZFBLNO  ZFCLSEQ  ZFIVNO
   INTO     CORRESPONDING FIELDS OF TABLE  IT_IDR
   FROM     ZTIDRHSD
   FOR      ALL   ENTRIES  IN  IT_IV
   WHERE    ZFBLNO         EQ  IT_IV-ZFBLNO
   GROUP BY
            ZFBLNO   ZFCLSEQ  ZFIVNO.

   LOOP  AT  IT_IDR.

      CLEAR  ZTIDR.
      SELECT  SINGLE * FROM ZTIDR
      WHERE   ZFBLNO   EQ   IT_IDR-ZFBLNO
      AND     ZFCLSEQ  EQ   IT_IDR-ZFCLSEQ.

      MOVE-CORRESPONDING  ZTIDR   TO  IT_ZTIDR.
      MOVE    IT_IDR-ZFIVNO       TO  IT_ZTIDR-ZFIVNO.

      APPEND  IT_ZTIDR.

   ENDLOOP.


ENDFORM.                    " P1000_READ_ZTIDR_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIDS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTIDS_DATA.

   SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_ZTIDS
   FROM    ZTIDS
   FOR     ALL   ENTRIES  IN  IT_ZTIDR
   WHERE   ZFBLNO         EQ  IT_ZTIDR-ZFBLNO
   AND     ZFCLSEQ        EQ  IT_ZTIDR-ZFCLSEQ.

ENDFORM.                    " P1000_READ_ZTIDS_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_SORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SORT_DATA.

   SORT  IT_EKKO      BY  LIFNR    EBELN.
   SORT  IT_ZTREQHD   BY  LIFNR    EBELN    ZFREQNO.
   SORT  IT_ZTINS     BY  ZFREQNO  ZFAMDNO  ZFINSEQ.
   SORT  IT_ZTREQST   BY  ZFREQNO  ZFAMDNO.
   SORT  IT_CIV       BY  ZFREQNO  ZFCIVRN.
   SORT  IT_BL        BY  ZFREQNO  ZFBLNO.
   SORT  IT_ZTLG      BY  ZFBLNO   ZFLGSEQ.
   SORT  IT_INR       BY  ZFBLNO   ZFBTSEQ.
   SORT  IT_OUR       BY  ZFBLNO   ZFBTSEQ.
   SORT  IT_IV        BY  ZFREQNO  ZFIVNO.
   SORT  IT_ZTIDR     BY  ZFBLNO   ZFCLSEQ.
   SORT  IT_ZTIDS     BY  ZFBLNO   ZFCLSEQ.
   SORT  IT_IN        BY  ZFIVNO   MBLNR.

ENDFORM.                    " P2000_SORT_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ALL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_ALL_DATA.

   CLEAR : W_LINE_CNT, W_MOD, W_IV_CNT.
   REFRESH : IT_ZTIVHST.

   LOOP  AT  IT_EKKO.

      IF  SY-TABIX  EQ  1.

          MOVE  IT_EKKO-LIFNR   TO   SV_LIFNR.
*>> 구매처 WRITE.
          CLEAR  SV_NAME.
          SELECT SINGLE NAME1 INTO  SV_NAME
          FROM   LFA1         WHERE LIFNR   EQ  SV_LIFNR.
          PERFORM  P4000_WRITE_LIFNR.
      ENDIF.

      IF  IT_EKKO-LIFNR  NE  SV_LIFNR.

*>> 구매처명 SELECT
          CLEAR  SV_NAME.
          SELECT SINGLE NAME1 INTO SV_NAME
          FROM   LFA1   WHERE  LIFNR  EQ  IT_EKKO-LIFNR.
          PERFORM  P4000_WRITE_LIFNR.

          MOVE   IT_EKKO-LIFNR  TO  SV_LIFNR.

      ENDIF.

*>> 구매처의 PO DATA WRITE.
      CLEAR : T024E, T024.
      SELECT SINGLE * FROM T024E WHERE EKORG  EQ  IT_EKKO-EKORG.
      SELECT SINGLE * FROM T024  WHERE EKGRP  EQ  IT_EKKO-EKGRP.

      PERFORM   P4000_WRITE_PO_DATA.

   ENDLOOP.

ENDFORM.                    " P3000_WRITE_ALL_DATA

*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_LIFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_LIFNR.

   SKIP  1.
   FORMAT COLOR COL_HEADING  INTENSIFIED  OFF.
   WRITE : /5(10)   '구매처 : '  ,
             (11)   IT_EKKO-LIFNR     ,
             (30)   SV_NAME      ,
          119       '                   ',
          '     '.
   HIDE : IT_EKKO.

ENDFORM.                    " P4000_WRITE_LIFNR
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_PO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_PO_DATA.

   FORMAT  COLOR  COL_GROUP  INTENSIFIED  OFF.
   WRITE : /1      '* Purchase Order  : ',
            22(10) IT_EKKO-EBELN,
            35(5)  IT_EKKO-EKORG,
            42(20) T024E-EKOTX,
            63(4)  IT_EKKO-EKGRP,
            68(20) T024-EKNAM,
           119     '                   ',
                   '     '.
   HIDE : IT_EKKO.

*>> 수입의뢰 DATA WRITE.
   LOOP  AT  IT_ZTREQHD  WHERE  EBELN  =  IT_EKKO-EBELN.

      PERFORM   P4000_WRITE_REQHD_DATA.

   ENDLOOP.

ENDFORM.                    " P4000_WRITE_PO_DATA

*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_REQHD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_REQHD_DATA.

*>> 은행명 SELECT
   CLEAR  LFA1.
   SELECT   SINGLE  *  FROM  LFA1  WHERE  LIFNR  EQ  IT_ZTREQHD-ZFOPBN.

   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   WRITE : /1(20) '** 수입의뢰       : ',
            22(10) IT_ZTREQHD-ZFREQNO  ,
            35(5)  IT_ZTREQHD-ZFREQTY  ,
            42(10) IT_ZTREQHD-INCO1    ,
            54(10) IT_ZTREQHD-ZFOPBN   ,
            65(20) LFA1-NAME1          ,
            86(25) IT_ZTREQHD-ZFOPNNO  ,
            120    IT_ZTREQHD-ZFLASTAM  CURRENCY  IT_ZTREQHD-WAERS,
                   IT_ZTREQHD-WAERS.
   HIDE : IT_ZTREQHD.

*>> 보험부보 내역 WRITE.
   LOOP  AT  IT_ZTINS    WHERE  ZFREQNO  =  IT_ZTREQHD-ZFREQNO
                         AND    ZFAMDNO  =  '00000'.

      PERFORM   P4000_WRITE_INS_DATA.

   ENDLOOP.

*>> AMEND 사항 WRITE
   LOOP  AT  IT_ZTREQST  WHERE  ZFREQNO  =  IT_ZTREQHD-ZFREQNO.

       PERFORM  P4000_WRITE_AMEND_DATA.

   ENDLOOP.

*>> COMMERCIAL INVOICE WRITE
   LOOP  AT  IT_CIV     WHERE  ZFREQNO  =  IT_ZTREQHD-ZFREQNO.

      PERFORM   P4000_WRITE_CIV_DATA.

   ENDLOOP.

*>> LOCAL, PU 인 경우는 입고만 발생.
*>> 통관요청 자료 SELECT
   IF IT_ZTREQHD-ZFREQTY  EQ  'LO'  OR  IT_ZTREQHD-ZFREQTY  EQ  'PU'.
      LOOP  AT  IT_IV  WHERE   ZFREQNO  EQ   IT_ZTREQHD-ZFREQNO.

         PERFORM   P4000_WRITE_IV_DATA.

      ENDLOOP.
   ENDIF.

*>> BL DATA WRITE.
   LOOP  AT  IT_BL     WHERE   ZFREQNO  =  IT_ZTREQHD-ZFREQNO.

      PERFORM   P4000_WRITE_BL_DATA.

   ENDLOOP.

ENDFORM.                    " P4000_WRITE_REQHD_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_AMEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_AMEND_DATA.

   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   WRITE : /(20) '*** Amend 내역    : ',
            (11) IT_ZTREQST-ZFREQNO,
            (5)  IT_ZTREQST-ZFAMDNO,
            (40) IT_ZTREQST-ZFOPNNM,
            (40) IT_ZTREQST-ZFOPNNO,
           120    IT_ZTREQST-ZFOPAMT  CURRENCY  IT_ZTREQST-WAERS,
                  IT_ZTREQST-WAERS.
   HIDE : IT_ZTREQST.

*>> AMEND 내역의 보험부보 내역.
   LOOP  AT  IT_ZTINS  WHERE  ZFREQNO    =   IT_ZTREQST-ZFREQNO
                       AND    ZFAMDNO    =   IT_ZTREQST-ZFAMDNO.

      PERFORM   P4000_WRITE_INS_DATA.

   ENDLOOP.

ENDFORM.                    " P4000_WRITE_AMEND_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_INS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_INS_DATA.

*>> 운송방?
   CLEAR SV_TEXT.
   CASE  IT_ZTINS-ZFTRANS.
      WHEN  'A'.
         MOVE   'Air'          TO  SV_TEXT.
      WHEN  'O'.
         MOVE   'Ocean'        TO  SV_TEXT.
      WHEN  'B'.
         MOVE   'Air + Ocean'  TO  SV_TEXT.
      WHEN OTHERS.
         MOVE   'None(기타)'   TO  SV_TEXT.
   ENDCASE.

   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   WRITE : /1(20) '*** 보험부보 내역 : ',
            22(10) IT_ZTINS-ZFREQNO,
            35(5)  IT_ZTINS-ZFAMDNO,
            42(5)  IT_ZTINS-ZFINSEQ,
            49(1)  IT_ZTINS-ZFTRANS,
            51(14) SV_TEXT,
            68(40) IT_ZTINS-ZFINNO,
            119    IT_ZTINS-ZFINAMT  CURRENCY  IT_ZTINS-ZFINAMTC,
                   IT_ZTINS-ZFINAMTC.
   HIDE : IT_ZTINS.

ENDFORM.                    " P4000_WRITE_INS_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_CIV_DATA.

*>> 물대 구매처 명 SELECT
   SELECT  SINGLE *   FROM  LFA1  WHERE  LIFNR  EQ  IT_CIV-ZFOPBN.
   MOVE    LFA1-NAME1       TO    SV_NAME.
   SELECT  SINGLE  *  FROM  LFA1  WHERE  LIFNR  EQ  IT_CIV-ZFMAVN.

   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   WRITE : /1(20) '**** Commercial IV:',
            22(10) IT_CIV-ZFCIVRN,
            35(10) IT_CIV-ZFCIDT,
            49(10) IT_CIV-ZFMAVN,
            60(20) LFA1-NAME1,
            82(30) IT_CIV-ZFOPBN,
            93(20) SV_NAME,
            119    IT_CIV-ZFIVAMT  CURRENCY  IT_CIV-ZFIVAMC,
                   IT_CIV-ZFIVAMC.
   HIDE : IT_CIV.

ENDFORM.                    " P4000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_BL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_BL_DATA.

*>> BL  NO.  SET
   CLEAR : SV_BLNO, LFA1.
   MOVE   IT_BL-ZFHBLNO   TO   SV_BLNO.
   IF  IT_BL-ZFHBLNO  IS  INITIAL.
       MOVE   IT_BL-ZFMBLNO   TO   SV_BLNO.
   ELSEIF  IT_BL-ZFMBLNO  IS  INITIAL.
       MOVE   IT_BL-ZFCGHNO   TO   SV_BLNO.
   ENDIF.
*>> 선사명 SELECT
   SELECT  SINGLE  *  FROM  LFA1  WHERE  LIFNR  EQ  IT_BL-ZFFORD.

   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   WRITE : /1(20) '***** BL          : ',
            22(10) IT_BL-ZFBLNO,
            35(10) IT_BL-ZFETD,
            49(10) IT_BL-ZFETA,
            60(10) IT_BL-ZFFORD,
            71(20) LFA1-NAME1,
            92(24) SV_BLNO,
            119    IT_BL-ZFBLAMT  CURRENCY  IT_BL-ZFBLAMC,
                   IT_BL-ZFBLAMC.
   HIDE : IT_BL.

*>> LG 자료 SELECT.
   LOOP  AT  IT_ZTLG  WHERE  ZFBLNO  EQ  IT_BL-ZFBLNO.

      PERFORM   P4000_WRITE_LG_DATA.

   ENDLOOP.

*>> 하역 정보 SELECT
   LOOP  AT  IT_CG  WHERE   ZFBLNO  EQ  IT_BL-ZFBLNO.

      PERFORM   P4000_WRITE_CG_DATA.

   ENDLOOP.

*>> 반입, 반출 정보 SELECT
   LOOP  AT  IT_INR  WHERE  ZFBLNO  EQ  IT_BL-ZFBLNO.

      PERFORM   P4000_WRITE_INR_DATA.

   ENDLOOP.

*>> 통관요청 자료 SELECT
   LOOP  AT  IT_IV  WHERE   ZFBLNO  EQ   IT_BL-ZFBLNO.

      W_IV_CNT  =  W_IV_CNT + 1.
      PERFORM   P4000_WRITE_IV_DATA.

   ENDLOOP.

*>> 과세통관 자료 검색.
   IF W_IV_CNT LT 1.

      LOOP  AT  IT_ZTCUIV  WHERE   ZFBLNO  EQ   IT_BL-ZFBLNO.

         PERFORM   P4000_WRITE_CUIV_DATA.

      ENDLOOP.

   ENDIF.

ENDFORM.                    " P4000_WRITE_BL_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_IV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_IV_DATA.

*>> 통관구분 GET
   CLEAR : SV_TEXT, SV_TEXT1, SV_TEXT2.
   CASE  IT_IV-ZFCLCD.
      WHEN  'A'.
         MOVE  '보세운송통관'      TO   SV_TEXT.
      WHEN  'C'.
         MOVE  '입항지통관'        TO   SV_TEXT.
      WHEN  'X'.
         MOVE  '미통관대상'        TO   SV_TEXT.
      WHEN  'B'.
         MOVE  '과세통관'          TO   SV_TEXT.
   ENDCASE.
*>> 통관상태 GET
   CASE  IT_IV-ZFCUST.
      WHEN  '1'.
         MOVE  '수입신고 의뢰 생성'   TO  SV_TEXT1.
      WHEN  '2'.
         MOVE  '수입신고 의뢰 대상'   TO  SV_TEXT1.
      WHEN  '3'.
         MOVE  '수입신고 의뢰 중'     TO  SV_TEXT1.
      WHEN  'Y'.
         MOVE  '통관완료'             TO  SV_TEXT1.
      WHEN  'N'.
         MOVE  '미통관대상'           TO  SV_TEXT1.
   ENDCASE.
*>> 입고 상태 SET
*   CASE   IT_IV-ZFGRST.
*     WHEN  'Y'.
*        MOVE  '입고완료'             TO  SV_TEXT2.
*     WHEN  'N'.
*        MOVE  '입고대상'             TO  SV_TEXT2.
*     WHEN  'X'.
*        MOVE  '미입고대상'           TO  SV_TEXT2.
*  ENDCASE.

   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   IF IT_ZTREQHD-ZFREQTY  EQ  'LO'  OR  IT_ZTREQHD-ZFREQTY EQ 'PU'.
      WRITE : /1(20) '******* 입고요청  : ',
               22(10) IT_IV-ZFIVNO,
               35(10) IT_IV-ZFCCDT,
               49(20) SV_TEXT,
               71(20) SV_TEXT1,
               92(20) SV_TEXT2,
               119    IT_IV-ZFIVAMT  CURRENCY  IT_IV-ZFIVAMC,
                      IT_IV-ZFIVAMC.
      HIDE : IT_IV.

*>> 입고 TABLE WRITE
      LOOP  AT  IT_IN  WHERE  ZFIVNO  EQ  IT_IV-ZFIVNO.
         PERFORM   P4000_WRITE_GR_DATA.
      ENDLOOP.
   ELSE.
      WRITE : /1(20) '******* 통관요청  : ',
               22(10) IT_IV-ZFIVNO,
               35(10) IT_IV-ZFCCDT,
               49(20) SV_TEXT,
               71(20) SV_TEXT1,
               92(20) SV_TEXT2,
               119    IT_IV-ZFIVAMT  CURRENCY  IT_IV-ZFIVAMC,
                      IT_IV-ZFIVAMC.
      HIDE : IT_IV.

   ENDIF.

*>> 수입신고 자료 WRITE.
   LOOP  AT  IT_ZTIDR  WHERE  ZFIVNO   =   IT_IV-ZFIVNO.
      PERFORM   P4000_WRITE_IDR_DATA.
   ENDLOOP.

ENDFORM.                    " P4000_WRITE_IV_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_IDR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_IDR_DATA.

*>> 관세사 SELECT
   CLEAR SV_NAME.
   SELECT SINGLE A~NAME1  INTO  SV_NAME
   FROM   LFA1  AS  A  INNER  JOIN  ZTIMIMG10  AS  B
   ON     A~LIFNR      EQ     B~ZFVEN
   WHERE  B~ZFCUT      EQ     IT_ZTIDR-ZFCUT.

   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   WRITE : /1(20) '******** 수입신고 : ',
            22(10) IT_ZTIDR-ZFBLNO,
            35(5)  IT_ZTIDR-ZFCLSEQ,
            49(20) SV_NAME,
            71(20) IT_ZTIDR-ZFIDRNO,
            119    IT_ZTIDR-ZFSTAMT  CURRENCY  IT_ZTIDR-ZFSTAMC,
                   IT_ZTIDR-ZFSTAMC.
   HIDE : IT_ZTIDR.

   LOOP  AT  IT_ZTIDS  WHERE  ZFBLNO  EQ  IT_ZTIDR-ZFBLNO
                       AND    ZFCLSEQ EQ  IT_ZTIDR-ZFCLSEQ.

      PERFORM  P4000_WRITE_IDS_DATA.

   ENDLOOP.

ENDFORM.                    " P4000_WRITE_IDR_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_LG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_LG_DATA.

   REFRESH : IT_ZTLG.

   SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_ZTLG
   FROM    ZTLG
   FOR     ALL  ENTRIES   IN  IT_BL
   WHERE   ZFBLNO         EQ  IT_BL-ZFBLNO.

ENDFORM.                    " P1000_READ_LG_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_INR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_INR_DATA.

   REFRESH : IT_INR.

   SELECT  *  INTO  CORRESPONDING  FIELDS  OF  TABLE  IT_INR
   FROM    ZTBLINR
   FOR     ALL  ENTRIES  IN  IT_BL
   WHERE   ZFBLNO        EQ  IT_BL-ZFBLNO.

ENDFORM.                    " P1000_READ_INR_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_OUR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_OUR_DATA.

   REFRESH : IT_OUR.

   SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_OUR
   FROM    ZTBLOUR
   FOR     ALL   ENTRIES  IN  IT_BL
   WHERE   ZFBLNO         EQ  IT_BL-ZFBLNO.

ENDFORM.                    " P1000_READ_OUR_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_LG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_LG_DATA.

   CLEAR : LFA1.
   SELECT  SINGLE *  FROM  LFA1  WHERE  LIFNR  EQ  IT_ZTLG-ZFCARIR.

   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   WRITE : /1(20) '***** LG          : ',
            22(10) IT_ZTLG-ZFBLNO,
            35(10) IT_ZTLG-ZFLGSEQ,
            49(10) IT_ZTLG-ZFCARIR,
            60(20) LFA1-NAME1,
            81(20) IT_ZTLG-ZFCARNM,
            119    IT_ZTLG-ZFCIAM  CURRENCY  IT_ZTLG-ZFCIAMC,
                   IT_ZTLG-ZFCIAMC.
   HIDE : IT_ZTLG.

ENDFORM.                    " P4000_WRITE_LG_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_INR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_INR_DATA.

*>> 도착지 보세구역.
   CLEAR ZTIMIMG03.
   SELECT  SINGLE * FROM ZTIMIMG03  WHERE  ZFBNAR  EQ  IT_INR-ZFABNAR.

   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   WRITE : /1(20) '****** 반입       : ',
            22(10) IT_INR-ZFBLNO,
            35(10) IT_INR-ZFBTSEQ,
            49(10) IT_INR-ZFABNAR,
            60(20) ZTIMIMG03-ZFBNARM,
            81(20) IT_INR-ZFINRNO,
            123    IT_INR-ZFINWT  UNIT  IT_INR-ZFKG,
                   IT_INR-ZFKG.
   HIDE : IT_INR.

*>> 반출자료 GET
   LOOP  AT  IT_OUR  WHERE  ZFBLNO  EQ  IT_INR-ZFBLNO
                     AND    ZFBTSEQ EQ  IT_INR-ZFBTSEQ.

      PERFORM   P4000_WRITE_OUR_DATA.

   ENDLOOP.

ENDFORM.                    " P4000_WRITE_INR_DATA

*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_OUR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_OUR_DATA.

*>> 도착지 보세구역.
   CLEAR : ZTIMIMG03.
   SELECT  SINGLE * FROM ZTIMIMG03  WHERE  ZFBNAR  EQ  IT_OUR-ZFABNAR.
   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   WRITE : /1(20) '****** 반출       : ',
            22(10) IT_OUR-ZFBLNO,
            35(10) IT_OUR-ZFBTSEQ,
            49(10) IT_OUR-ZFABNAR,
            60(20) ZTIMIMG03-ZFBNARM,
            81(20) IT_OUR-ZFOURNO,
            123    IT_OUR-ZFOUWT  UNIT  IT_OUR-ZFKG,
                   IT_OUR-ZFKG.
   HIDE : IT_OUR.

ENDFORM.                    " P4000_WRITE_OUR_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_IDS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_IDS_DATA.

   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   WRITE : /1(20) '********* 수입면허:',
            22(10) IT_ZTIDS-ZFBLNO,
            35(5)  IT_ZTIDS-ZFCLSEQ,
            49(10) IT_ZTIDS-ZFIDWDT,
            71(20) IT_ZTIDS-ZFIDRNO,
            119    IT_ZTIDS-ZFSTAMT  CURRENCY  IT_ZTIDS-ZFSTAMC,
                   IT_ZTIDS-ZFSTAMC.
   HIDE : IT_ZTIDS.

*>> 입고 자료 SELECT & WRITE.
   LOOP  AT  IT_IN  WHERE  ZFIVNO  EQ  IT_IV-ZFIVNO.
      PERFORM   P4000_WRITE_GR_DATA.
   ENDLOOP.

ENDFORM.                    " P4000_WRITE_IDS_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_GR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_GR_DATA.

   IF SY-TABIX  EQ  1.
      PERFORM   P4000_WRITE_IN_DATA.
      MOVE  IT_IN-MBLNR  TO  SV_MBLNR.
   ENDIF.

   IF  SV_MBLNR  NE  IT_IN-MBLNR.
       PERFORM   P4000_WRITE_IN_DATA.
       MOVE  IT_IN-MBLNR  TO  SV_MBLNR.
   ENDIF.

ENDFORM.                    " P4000_WRITE_GR_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_CG_DATA.

   REFRESH :  IT_ZTCG, IT_CG.

   SELECT  ZFCGNO   ZFBLNO
   INTO CORRESPONDING FIELDS OF TABLE IT_ZTCG
   FROM    ZTCGIT
   FOR     ALL   ENTRIES   IN     IT_BL
   WHERE   ZFBLNO          EQ     IT_BL-ZFBLNO
   GROUP BY
           ZFCGNO  ZFBLNO.

   LOOP  AT  IT_ZTCG.

      CLEAR   ZTCGHD.
      SELECT  SINGLE *  FROM  ZTCGHD
      WHERE   ZFCGNO    EQ    IT_ZTCG-ZFCGNO.

      MOVE-CORRESPONDING ZTCGHD  TO  IT_CG.
      MOVE  IT_ZTCG-ZFBLNO       TO  IT_CG-ZFBLNO.

      APPEND  IT_CG.

   ENDLOOP.

ENDFORM.                    " P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_CG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_CG_DATA.

*>> 도착항 SELECT.
   CLEAR  SV_TEXT.
   SELECT SINGLE PORTT INTO SV_TEXT
          FROM   ZTIEPORT
          WHERE  LAND1  EQ 'KR'
          AND    PORT   EQ IT_CG-ZFCGPT.
*   SELECT SINGLE ZFCDNM  INTO  SV_TEXT
*   FROM   ZTIMIMG08
*   WHERE  ZFCDTY  EQ  '002'
*   AND    ZFCD    EQ  IT_CG-ZFCGPT.

   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   WRITE : /1(20) '****** 하역       : ',
            22(10) IT_CG-ZFCGNO,
            35(10) IT_CG-ZFETA,
            49(10) IT_CG-ZFARVLDT,
            60(10) IT_CG-ZFCGPT,
            71(20) SV_TEXT,
            92(24) IT_CG-ZFKEYM,
            119    '                   ',
                   '     '.
   HIDE : IT_CG.

ENDFORM.                    " P4000_WRITE_CG_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CUIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_CUIV_DATA.

   REFRESH : IT_ZTCUIV.

   SELECT *  INTO CORRESPONDING FIELDS OF TABLE IT_ZTCUIV
   FROM   ZTCUCLIV
   FOR    ALL  ENTRIES  IN  IT_BL
   WHERE  ZFBLNO        EQ  IT_BL-ZFBLNO.

ENDFORM.                    " P1000_READ_CUIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_CUIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_CUIV_DATA.

*>> 통관구분 GET
   CLEAR : SV_TEXT, SV_TEXT1, SV_TEXT2.
   CASE  IT_ZTCUIV-ZFCLCD.
      WHEN  'A'.
         MOVE  '보세운송통관'      TO   SV_TEXT.
      WHEN  'C'.
         MOVE  '입항지통관'        TO   SV_TEXT.
      WHEN  'X'.
         MOVE  '미통관대상'        TO   SV_TEXT.
      WHEN  'B'.
         MOVE  '과세통관'          TO   SV_TEXT.
   ENDCASE.
*>> 통관상태 GET
   CASE  IT_ZTCUIV-ZFCUST.
      WHEN  '1'.
         MOVE  '수입신고 의뢰 생성'   TO  SV_TEXT1.
      WHEN  '2'.
         MOVE  '수입신고 의뢰 대상'   TO  SV_TEXT1.
      WHEN  '3'.
         MOVE  '수입신고 의뢰 중'     TO  SV_TEXT1.
      WHEN  'Y'.
         MOVE  '통관완료'             TO  SV_TEXT1.
      WHEN  'N'.
         MOVE  '미통관대상'           TO  SV_TEXT1.
   ENDCASE.

   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   WRITE : /1(20) '******* 통관요청  : ',
            22(10) IT_ZTCUIV-ZFIVNO,
            35(10) IT_ZTCUIV-ZFCCDT,
            49(20) SV_TEXT,
            71(20) SV_TEXT1,
            92(20) SV_TEXT2,
            119    IT_ZTCUIV-ZFIVAMT  CURRENCY  IT_ZTCUIV-ZFIVAMC,
                   IT_ZTCUIV-ZFIVAMC.
   HIDE : IT_ZTCUIV.

*>> 수입신고 자료 WRITE.
   LOOP  AT  IT_ZTIDR  WHERE  ZFIVNO   =   IT_IV-ZFIVNO.

      PERFORM   P4000_WRITE_IDR_DATA.

   ENDLOOP.


ENDFORM.                    " P4000_WRITE_CUIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_IN_DATA.

   REFRESH IT_IN.

   SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_IN
   FROM    ZTIVHST  AS  A  INNER  JOIN  ZTIVHSTIT  AS  B
   ON      A~ZFIVNO        EQ     B~ZFIVNO
   AND     A~ZFIVHST       EQ     B~ZFIVHST
   FOR     ALL  ENTRIES    IN     IT_IV
   WHERE   A~ZFIVNO        EQ     IT_IV-ZFIVNO
   AND     B~ZFGRST        EQ     'Y'.

ENDFORM.                    " P1000_READ_IN_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_IN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_IN_DATA.

   W_LINE_CNT  =  W_LINE_CNT  +  1.
   W_MOD       =  W_LINE_CNT  MOD  2.

   IF  W_MOD  EQ  1.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  OFF.
   ELSE.
       FORMAT  COLOR  COL_NORMAL  INTENSIFIED  ON.
   ENDIF.

   WRITE : /1(20) '**********  입고  : ',
            22(10) IT_IN-MBLNR,
            35(5)  IT_IN-MJAHR,
            49(10) IT_IN-BLDAT,
            61(10) IT_IN-BUDAT,
            72(10) IT_IN-BWART,
            119    '                   ',
                   '     '.
   HIDE : IT_IN.

ENDFORM.                    " P4000_WRITE_IN_DATA
