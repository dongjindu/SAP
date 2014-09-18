*----------------------------------------------------------------------*
*INCLUDE ZRIM03I01 .
*----------------------------------------------------------------------*
*&  프로그램명 : 수입관세/부가세등록 Main PAI MODULE Include
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.06.10
*&  적용회사PJT: LG 화학.
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.

  IF W_STATUS EQ C_REQ_D.
      SET SCREEN 0.   LEAVE SCREEN.
  ELSE.
      PERFORM P2000_SET_MESSAGE USING  SY-UCOMM.
  ENDIF.

ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0100 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0100-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  COMMAND_BEFORE_PROCESS_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE COMMAND_BEFORE_PROCESS_SCR0100 INPUT.

   PERFORM  SET_MODIFY_FIELD_CHECK.

   IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선택.
      W_MARK = 'X'.
   ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해제.
      CLEAR : W_MARK.
   ENDIF.

   CASE OK-CODE.
      WHEN 'DDLC'.       ">Double click'
         PERFORM  P2000_DDLC_PROCESS.
      WHEN 'OTDC'.       ">기타비용문서...
**>> CALL SCREEN.
         PERFORM  P2000_SET_INIT_SCREEN.
      WHEN 'CRDC'.       ">생성.
         PERFORM  P2000_SET_INIT_SCREEN.
      WHEN 'CHDC'.       ">변경.
         PERFORM  P2000_SET_INIT_SCREEN.
      WHEN 'DISP'.       ">조회.
         PERFORM  P2000_SET_INIT_SCREEN.
      WHEN 'BACK' OR 'EXIT'.
         PERFORM  P2000_EXIT_PROCESS.
      WHEN 'DELE'.
         PERFORM  P2000_SET_INDICATE.
      WHEN OTHERS.
         W_EXIT_MODE = 'N'.
         EXIT.
   ENDCASE.

   W_EXIT_MODE = 'Y'.

ENDMODULE.                 " COMMAND_BEFORE_PROCESS_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_MODIFY_FIELD_CHECK
*&---------------------------------------------------------------------*
FORM SET_MODIFY_FIELD_CHECK.

  CHECK : W_EDIT_CHECK  NE 'Y'.
  CASE SY-TCODE.
     WHEN 'ZIMC6'.
        IF ZTCCHD-ZFCCNO IS INITIAL.
           W_EDIT_CHECK = 'N'.
           EXIT.
        ENDIF.
     WHEN OTHERS.
  ENDCASE.

  IF ZTCCHD NE *ZTCCHD.
     W_EDIT_CHECK = 'Y'.
  ENDIF.
  CHECK : W_EDIT_CHECK  NE 'Y'.

  LOOP AT IT_ZSCCIT.
     W_TABIX = SY-TABIX.
     READ TABLE IT_ZSCCIT_OLD INDEX W_TABIX.
     IF SY-SUBRC EQ 0.

     ELSE.
        W_EDIT_CHECK = 'Y'.  EXIT.
     ENDIF.
  ENDLOOP.

  CHECK : W_EDIT_CHECK  NE 'Y'.

  LOOP AT IT_ZSCCIT_OLD.
     W_TABIX = SY-TABIX.
     READ TABLE IT_ZSCCIT INDEX W_TABIX.
     IF SY-SUBRC EQ 0.

     ELSE.
        W_EDIT_CHECK = 'Y'.  EXIT.
     ENDIF.
  ENDLOOP.

ENDFORM.                    " SET_MODIFY_FIELD_CHECK
*&---------------------------------------------------------------------*
*&      Module  COMPANYCODE_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE COMPANYCODE_CHECK_SCR0100 INPUT.

   CHECK : W_STATUS    NE C_REQ_D.
   CHECK : W_EXIT_MODE NE 'Y'.

   IF ZTCCHD-BUKRS IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCCHD' 'BUKRS'.
   ELSE.
      PERFORM  P1000_GET_COMPANY_CODE USING ZTCCHD-BUKRS.
   ENDIF.

ENDMODULE.                 " COMPANYCODE_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  PERIOD_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE PERIOD_CHECK_SCR0100 INPUT.

  DATA: S_MONAT LIKE BKPF-MONAT.      "Save field for input period

  CHECK : W_STATUS    NE C_REQ_D.
  CHECK : W_EXIT_MODE NE 'Y'.
*> 증빙일.
  IF ZTCCHD-BLDAT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCCHD' 'BLDAT'.
  ENDIF.
  IF ZTCCHD-BUDAT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCCHD' 'BUDAT'.
  ENDIF.

  IF ZTCCHD-ZFBDT IS INITIAL.
     ZTCCHD-ZFBDT = ZTCCHD-BUDAT.
  ENDIF.

  CLEAR ZTCCHD-GJAHR.
  S_MONAT = ZTCCHD-MONAT.

  PERFORM PERIODE_ERMITTELN USING ZTCCHD-BUDAT
                                  ZTCCHD-GJAHR
                                  ZTCCHD-MONAT.

  IF NOT S_MONAT IS INITIAL
  AND    S_MONAT NE ZTCCHD-MONAT
  AND ( SY-BINPT = SPACE AND SY-CALLD = SPACE ).
    MESSAGE W000(F5) WITH S_MONAT ZTCCHD-BUDAT ZTCCHD-MONAT.
  ENDIF.

ENDMODULE.                 " PERIOD_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSCCIT_UPDATE_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSCCIT_UPDATE_SCR0100 INPUT.

  CHECK : W_STATUS    NE C_REQ_D.
  CHECK : W_EXIT_MODE NE 'Y'.

  W_CHK_BIT = 'Y'.
*  Internal Table Read
  READ TABLE IT_ZSCCIT  INDEX TC_0100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

** 입력여부 체크.
  IF  ZSCCIT-ZFIVNO  IS INITIAL.
     W_CHK_BIT = 'N'.
  ENDIF.
*> MOVE.
  CLEAR IT_ZSCCIT.

  MOVE-CORRESPONDING  ZSCCIT  TO  IT_ZSCCIT.
*> INPUT VALUE CHECK.
  IF W_CHK_BIT EQ 'Y'.

     IF IT_ZSCCIT-SGTXT1 IS INITIAL.
        MOVE '관세'                   TO  IT_ZSCCIT-SGTXT1.
     ENDIF.
     IF IT_ZSCCIT-SGTXT2 IS INITIAL.
        MOVE '부가세'                 TO  IT_ZSCCIT-SGTXT2.
     ENDIF.

     IF IT_ZSCCIT-ZFIVNO IS INITIAL. EXIT. ENDIF.
     SELECT SINGLE *
            FROM ZTIV
           WHERE ZFIVNO = IT_ZSCCIT-ZFIVNO.

     IF SY-SUBRC NE 0.    " 과세 통관일경우.
        SELECT SINGLE *
          FROM ZTCUCLIV
         WHERE ZFIVNO = IT_ZSCCIT-ZFIVNO.

        IF SY-SUBRC NE 0. " 둘다 없을 경우.
           MESSAGE E191 WITH 'Clearance request No'.
        ELSE.
           MOVE ZTCUCLIV-ZFBLNO TO W_ZFBLNO.
        ENDIF.
     ELSE.
        IF ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ  'PU'.
           MESSAGE W989 WITH ZTIV-ZFREQTY.
        ENDIF.
        MOVE ZTIV-ZFBLNO TO W_ZFBLNO.
        SELECT SINGLE *
           FROM ZTCUCLIV
          WHERE ZFIVNO = IT_ZSCCIT-ZFIVNO.
     ENDIF.
*>> 면허및 부가세/관세금액.
*   CLEAR ZTCUCL.
*   SELECT SINGLE *
*      FROM ZTCUCL
*     WHERE ZFBLNO   EQ ZTCUCLIV-ZFBLNO
*       AND ZFCLSEQ  EQ ZTCUCLIV-ZFCLSEQ.
*   IF SY-SUBRC EQ 0.
*       IF ZTCUCL-ZFCUST EQ 'N'. " 통관불가건.
*          MESSAGE E638.
*       ENDIF.
*   ENDIF.
     CLEAR: ZTIDS.
     SELECT SINGLE *
       FROM ZTIDS
      WHERE ZFBLNO   EQ ZTCUCLIV-ZFBLNO
        AND ZFCLSEQ  EQ ZTCUCLIV-ZFCLSEQ.
     IF SY-SUBRC NE 0.
         CLEAR: ZTIDS-ZFCUAMTS,ZTIDS-ZFVAAMTS,ZTIDS-ZFTBAK.
     ENDIF.
     IF IT_ZSCCIT-ZFCCAMT IS INITIAL.
        MOVE  ZTIDS-ZFCUAMTS TO IT_ZSCCIT-ZFCCAMT.
     ENDIF.
     IF IT_ZSCCIT-ZFCVAMT IS INITIAL.
        MOVE ZTIDS-ZFVAAMTS TO IT_ZSCCIT-ZFCVAMT.
     ENDIF.
     IF IT_ZSCCIT-FWBAS IS INITIAL.             " 과세 표준액.
        MOVE ZTIDS-ZFTBAK TO IT_ZSCCIT-FWBAS.   " 과세 가격 원화.
     ENDIF.
     IF IT_ZSCCIT-ZFIDRNO IS INITIAL.
        MOVE ZTIDS-ZFIDRNO TO IT_ZSCCIT-ZFIDRNO.
     ENDIF.
     IF IT_ZSCCIT-FWBAS NE SPACE AND IT_ZSCCIT-ZFCVAMT NE SPACE.
*>> TO AMOUNT EXTERNAL FORMAT..
        PERFORM P2000_CHECK_AMT.
        IF IT_ZSCCIT-ZFCVAMT  NE W_ZFCVAMT.
           MESSAGE W977 WITH
              'Amount difference with taxable standard expense rate'.
        ENDIF.
     ENDIF.

*>> 계정코드 가지고 오기.
     PERFORM P2000_GETDATA_IMG.                 " IMG SETTING 사항.

  ENDIF.


  IF W_SY_SUBRC EQ 0.
     MODIFY IT_ZSCCIT INDEX W_TABIX.
  ELSE.
     IF W_CHK_BIT EQ 'Y'.
       APPEND IT_ZSCCIT.
     ENDIF.
  ENDIF.

ENDMODULE.                 " IT_ZSCCIT_UPDATE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0100 INPUT.

   IF OK-CODE EQ 'CCL'.
      PERFORM P2000_CACULATION.
   ENDIF.
   PERFORM   P2000_BALANCE_CALCULATE.
   CASE OK-CODE.
      WHEN 'SAVE'.
         PERFORM  P2000_EXIT_PROCESS.
      WHEN 'POPU'.      ">다중입력.
         PERFORM  P2000_MULTI_DOC_SELECT.
      WHEN 'OPDC'.      "전기.
         PERFORM  P2000_SET_INDICATE.
      WHEN 'CCDC'.      "전기취소.
         PERFORM  P2000_SET_INDICATE.
      WHEN 'FIHI'.      ">문서이력.
         PERFORM  P2000_POSTING_HISTORY.
      WHEN 'HIST'.      ">헤더변경사항.
         PERFORM  P2000_HEADER_CHANGE_DOC.
      WHEN 'HIIT'.      ">아이템변경사항.
         PERFORM  P2000_SELECT_ITEM.     " 선택된 아이템.
         PERFORM  P2000_ITEM_CHANGE_DOC.
      WHEN 'IMDC'.      ">관련문서.
         PERFORM  P2000_SELECT_ITEM.     " 선택된 아이템.
         PERFORM  P2000_ITEM_REF_DOC_DISPLAY.
      WHEN 'CCFI'.      ">관세회계문서.
         PERFORM  P2000_FI_DOCUMENT_DISPLAY
                           USING   ZTCCHD-BUKRS
                                   ZTCCHD-ZFFYCC
                                   ZTCCHD-ZFCCAN.
      WHEN 'VTFI'.      ">부과세회계문서.
        PERFORM  P2000_FI_DOCUMENT_DISPLAY
                           USING    ZTCCHD-BUKRS
                                    ZTCCHD-ZFFYVT
                                    ZTCCHD-ZFVTAN.

      WHEN 'MK03'.      ">지급처.
         PERFORM   P2000_VENDOR_DISPLAY USING ZTCCHD-LIFNR
                                              '지불처'.
      WHEN '0003' OR '0004'.   ">전체선택 및 선택해제.
         IF OK-CODE EQ '0003'.
            MOVE 'X'  TO W_MARK.
         ELSE.
            CLEAR : W_MARK.
         ENDIF.
         LOOP AT IT_ZSCCIT.
            IT_ZSCCIT-ZFMARK = W_MARK.
            MODIFY IT_ZSCCIT INDEX  SY-TABIX.
         ENDLOOP.
      WHEN '0005'.             ">라인 삽입.
         PERFORM  P2000_SELECT_ITEM.     " 선택된 아이템.
         CHECK : W_COUNT EQ 1.
         CLEAR: IT_ZSCCIT.
         INSERT IT_ZSCCIT INDEX  W_TMP_TABIX.
      WHEN '0007'.             ">라인 삭제.
         DELETE IT_ZSCCIT WHERE ZFMARK EQ 'X'.
      WHEN 'CCCD'.             ">관세비용문서조회.
         PERFORM P2000_DISPLAY_DOCUMEMT USING ZTCCHD-BELNR1
                                              ZTCCHD-GJAHR1
                                              ZTCCHD-BUKRS.
      WHEN 'VTCD'.             ">부가세비용문서조회.
         PERFORM P2000_DISPLAY_DOCUMEMT USING ZTCCHD-BELNR2
                                              ZTCCHD-GJAHR2
                                              ZTCCHD-BUKRS.
      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  BELEGART_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE BELEGART_CHECK_SCR0100 INPUT.

   CHECK : W_STATUS    NE C_REQ_D.
   CHECK : W_EXIT_MODE NE 'Y'.

   IF ZTCCHD-BLART IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCCHD' 'BLART'.
   ENDIF.
*> 문서 종류 체크.
   PERFORM BELEGART_PRUEFEN(SAPFF001)
           USING ZTCCHD-BLART ZTCCHD-GJAHR.
*> 전기년도 체크.
   BKPF-BUKRS = ZTCCHD-BUKRS.
   PERFORM NUMMERNKREIS_LESEN(SAPFF001)
           USING ZTCCHD-GJAHR.
*> 권한 검증.
   PERFORM P2000_BELEGART_AUTHORITY_CHECK.

ENDMODULE.                 " BELEGART_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  VENDOR_ACCOUNT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE VENDOR_ACCOUNT_CHECK_SCR0100 INPUT.

   CHECK : W_STATUS    NE C_REQ_D.
   CHECK : W_EXIT_MODE NE 'Y'.

   IF ZTCCHD-BUPLA IS INITIAL.    ">
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCCHD' 'BUPLA'.
   ENDIF.

   IF ZTCCHD-LIFNR IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCCHD' 'LIFNR'.
   ENDIF.

   CALL FUNCTION 'FI_POSTING_KEY_DATA'
         exporting
              i_bschl       = '31'
              i_umskz       = SPACE       ">bseg-umskz
         importing
              e_t074u       = t074u
              e_tbsl        = tbsl
              e_tbslt       = tbslt
         exceptions
              error_message = 1.

* 1. PBO: no message if bschl request umskz
    if sy-subrc = 1.
* (del) if not ( firstcall = 'X'                           "Note 352492
      if tbsl-xsonu ne space.
        message id sy-msgid type sy-msgty number sy-msgno with
                sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.

*>> VENDOR MASTER DEFINE.
    CLEAR : *LFA1.
    SELECT SINGLE * INTO *LFA1 FROM LFA1
                    WHERE LIFNR EQ ZTCCHD-LIFNR.
    IF SY-SUBRC NE 0.
       MESSAGE E023 WITH ZTCCHD-LIFNR.
    ENDIF.

    CALL FUNCTION 'FI_VENDOR_DATA'
         EXPORTING
            i_bukrs = ZTCCHD-BUKRS
            i_lifnr = ZTCCHD-LIFNR
         IMPORTING
            e_kred  = vf_kred.
    IF ZTCCHD-ZTERM NE VF_KRED-ZTERM.
       MESSAGE W574 WITH  ZTCCHD-LIFNR VF_KRED-ZTERM ZTCCHD-ZTERM.
    ENDIF.

   IF ZTCCHD-AKONT IS INITIAL.
      ZTCCHD-AKONT = vf_kred-AKONT.
   ENDIF.
   IF ZTCCHD-AKONT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCCHD' 'AKONT'.
   ENDIF.
*>> TAX CODE CHECK.
   call function 'FI_TAX_INDICATOR_CHECK'
        exporting
            i_bukrs  = ZTCCHD-BUKRS
            i_hkont  = vf_kred-AKONT
            i_koart  = 'K'
            i_mwskz  = ZTCCHD-MWSKZ
            i_stbuk  = SPACE
            x_dialog = 'X'
       importing
            e_egrkz  = egrkz.
   IF ZTCCHD-WAERS IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCCHD' 'WAERS'.
   ENDIF.
   IF ZTCCHD-MWSKZ IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCCHD' 'MWSKZ'.
   ENDIF.

ENDMODULE.                 " VENDOR_ACCOUNT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_HEADER_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE INPUT_HEADER_CHECK_SCR0100 INPUT.

ENDMODULE.                 " INPUT_HEADER_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.

  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
MODULE P2000_INIT_VALUE_CHECK INPUT.

  IF NOT ( SY-UCOMM EQ 'ENTR' OR
           SY-UCOMM EQ 'YES' ).
     EXIT.
  ENDIF.

  IF UF05A-STGRD IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'UF05A' 'STGRD'.
  ENDIF.

  IF BSIS-BUDAT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'BSIS' 'BUDAT'.
  ENDIF.

ENDMODULE.                 " P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR0100 INPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZTCCHD-ZTERM'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF SCREEN-INPUT EQ '1'.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZTCCHD-ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ELSE.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZTCCHD-ZTERM
              I_XSHOW       = 'X'
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ENDIF.
  IF SY-SUBRC NE 0.
    MESSAGE S177(06) WITH ZTCCHD-ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    ZTCCHD-ZTERM = T052-ZTERM.
  ENDIF.

ENDMODULE.                 " HELP_ZTERM_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_DOCUMENT_SCR00010  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INPUT_DOCUMENT_SCR00010 INPUT.

   IF NOT ( SY-UCOMM EQ 'YES' OR
            SY-UCOMM EQ 'ENTR' ).
      EXIT.
   ENDIF.

*> 문서번호.
   IF ZSCCHD-ZFCCNO IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCCHD' 'ZFCCNO'.
   ENDIF.

*> CHARGE DOCUMENT READ.
   PERFORM   P1000_GET_CHARGE_DOCUMENT  USING   'E'.

ENDMODULE.                 " CHECK_INPUT_DOCUMENT_SCR00010  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0100  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0100 INPUT.

   READ TABLE IT_ZSCCIT  INDEX TC_0100-CURRENT_LINE.
   W_SY_SUBRC = SY-SUBRC.
   W_TABIX    = SY-TABIX.

   IF W_SY_SUBRC = 0.
     IF NOT ( W_ROW_MARK IS INITIAL ).
        IT_ZSCCIT-ZFMARK = 'X'.
     ELSE.
        CLEAR : IT_ZSCCIT-ZFMARK.
     ENDIF.
     MODIFY IT_ZSCCIT INDEX W_TABIX.
   ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TEXCODE_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE TEXCODE_CHECK_SCR0100 INPUT.

   CHECK : W_STATUS    NE C_REQ_D.
   CHECK : W_EXIT_MODE NE 'Y'.

   SELECT SINGLE *
         FROM ZTIMIMG08
        WHERE ZFCD  EQ '001'.
   IF ZTIMIMG08-ZFCD5 NE ZTCCHD-MWSKZ.
      MESSAGE W977 WITH 'Tax code is different with IMG setting'.
   ENDIF.

ENDMODULE.                 " TEXCODE_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  AMT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE AMT_CHECK_SCR0100 INPUT.

  CHECK : W_STATUS    NE C_REQ_D.
  CHECK : W_EXIT_MODE NE 'Y'.
*  IF ZTCCHD-ZFCVAMT IS INITIAL.
*     MESSAGE E977 WITH 'Input amount!'.
*  ENDIF.
*  IF ZTCCHD-ZFCCAMT IS INITIAL.
*     MESSAGE E977 WITH 'Input amount!'.
*  ENDIF.

ENDMODULE.                 " AMT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_ITEM_DOC_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_ITEM_DOC_SCR0100 INPUT.

   PERFORM  P1000_GET_ZFIVNO_HELP USING  ZSCCIT-ZFIVNO.
   IF ANTWORT EQ 'Y'.
      IF W_STATUS NE C_REQ_D.
         MOVE : ZSCCIT-ZFIVNO TO ZSCCIT-ZFIVNO.
      ENDIF.
   ENDIF.

ENDMODULE.                 " HELP_ITEM_DOC_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCR0030  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCR0030 INPUT.

  ANTWORT = 'N'.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " USER_EXIT_SCR0030  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0030  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0030 INPUT.

 CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
       EXIT.
*       ANTWORT = 'Y'.
 ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0030  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZFIVNO_SCR0050  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ZFIVNO_SCR0050 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC' OR 'NO'.
       EXIT.
  ENDCASE.

  CALL  FUNCTION 'ZIM_GET_CC_DOCUMENT'
     EXPORTING
        ZFIVNO              =          ZSIV-ZFIVNO
     IMPORTING
        W_ZTIV              =          ZTIV
     TABLES
         IT_ZSIVIT          =          IT_ZSIVIT
         IT_ZSIVIT_ORG      =          IT_ZSIVIT_ORG
         IT_ZSIVCD          =          IT_ZSIVCD
         IT_ZSIVHST         =          IT_ZSIVHST
         IT_ZSIVHST1        =          IT_ZSIVHST1
      EXCEPTIONS
         NOT_FOUND         =    4
         NOT_INPUT         =    8.

  CASE SY-SUBRC.
     WHEN 4.
        MESSAGE E413 WITH ZSIV-ZFIVNO.
     WHEN 8.
        MESSAGE E412.
  ENDCASE.

ENDMODULE.                 " GET_ZFIVNO_SCR0050  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFIVNO_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE ZFIVNO_CHECK_SCR0100 INPUT.

  READ TABLE IT_ZSCCIT WITH KEY ZFIVNO = ZSCCIT-ZFIVNO.
  W_TABIX = SY-TABIX.
  IF W_TABIX NE TC_0100-CURRENT_LINE.
     IF SY-SUBRC EQ 0.
        MESSAGE E624 WITH ZSCCIT-ZFIVNO W_TABIX.
     ENDIF.
  ENDIF.

ENDMODULE.                 " ZFIVNO_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0014_LIST_CHECK_SCR0014 INPUT.
   LEAVE TO LIST-PROCESSING.

   CASE INCLUDE.
      WHEN 'POPU'.
         FORMAT COLOR COL_HEADING INTENSIFIED OFF.
         WRITE : / SY-ULINE(96), / SY-VLINE NO-GAP,
                   '유형'   NO-GAP, SY-VLINE NO-GAP,
*                   ' 문서번호 ' NO-GAP, SY-VLINE NO-GAP,
                   '메세지 텍스트', 94 SY-VLINE NO-GAP,
                   'T'      NO-GAP, SY-VLINE,
                 / SY-ULINE(96).
*         MESSAGE
         LOOP AT IT_ERR_LIST.
            W_MOD  =  SY-TABIX MOD 2.
            FORMAT RESET.
            IF W_MOD EQ 0.
               FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
            ELSE.
               FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
            ENDIF.
            WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4)     NO-GAP,
*                      SY-VLINE NO-GAP, IT_ERR_LIST-BELNR       NO-GAP,
                      SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(87) NO-GAP,
                      SY-VLINE NO-GAP.

            CASE IT_ERR_LIST-MSGTYP.
               WHEN 'E' OR 'A'.
                  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
               WHEN 'W'.
                  FORMAT COLOR COL_KEY      INTENSIFIED OFF.
               WHEN 'I'.
                  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
               WHEN 'S'.
                  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
            ENDCASE.

            WRITE : IT_ERR_LIST-MSGTYP(1) NO-GAP, SY-VLINE NO-GAP.
*                   / SY-ULINE(96).
            HIDE:IT_ERR_LIST.
         ENDLOOP.
         WRITE : / SY-ULINE(96).
         CLEAR : IT_ERR_LIST.

      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_POSTING_HISTORY_SCR0110  INPUT
*&---------------------------------------------------------------------*
MODULE SET_POSTING_HISTORY_SCR0110 INPUT.

  LEAVE TO LIST-PROCESSING.
*>> 관세.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /30  ' [   회계문서이력   ] '.

  CLEAR : W_TABIX.
  LOOP AT IT_ZSBHIS WHERE BUKRS EQ ZTCCHD-BUKRS
                    AND   BELNR EQ ZTCCHD-BELNR1
                    AND   GJAHR EQ ZTCCHD-GJAHR1.
     ADD 1 TO W_TABIX.
     IF W_TABIX EQ 1.
        SKIP 2.
        FORMAT COLOR COL_HEADING INTENSIFIED OFF.
        WRITE : /   '[관 세]',
                  10 '회사코드:',     19 IT_ZSBHIS-BUKRS,
                  26 '회계전표번호:', 39 IT_ZSBHIS-BELNR,
                  52 '회계년도:',     61 IT_ZSBHIS-GJAHR.

        PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
     ENDIF.
     W_MOD = W_TABIX MOD 2.
     PERFORM   P3000_LINE_WRITE.
  ENDLOOP.
  IF SY-SUBRC EQ 0.
     ULINE AT /1(90).
  ENDIF.
  CLEAR : IT_ZSBHIS.

*>> 부가세.
  CLEAR : W_TABIX.
  LOOP AT IT_ZSBHIS WHERE BUKRS EQ ZTCCHD-BUKRS
                    AND   BELNR EQ ZTCCHD-BELNR2
                    AND   GJAHR EQ ZTCCHD-GJAHR2.
     ADD 1 TO W_TABIX.
     IF W_TABIX EQ 1.
        SKIP 2.
        FORMAT COLOR COL_HEADING INTENSIFIED OFF.
        WRITE : / '[부가세]',
                10 '회사코드:',     19 IT_ZSBHIS-BUKRS,
                26 '회계전표번호:', 39 IT_ZSBHIS-BELNR,
                52 '회계년도:',     61 IT_ZSBHIS-GJAHR.
        PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
     ENDIF.
     W_MOD = W_TABIX MOD 2.
     PERFORM   P3000_LINE_WRITE.
  ENDLOOP.
  IF SY-SUBRC EQ 0.
     ULINE AT /1(90).
  ENDIF.
  CLEAR : IT_ZSBHIS.

  CLEAR : OK-CODE,INCLUDE.
  LEAVE TO SCREEN 0100.

ENDMODULE.                 " SET_POSTING_HISTORY_SCR0110  INPUT
