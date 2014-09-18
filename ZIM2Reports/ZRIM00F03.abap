*&---------------------------------------------------------------------
*& INCLUDE ZRIM00F03.
*&---------------------------------------------------------------------
*&  프로그램명 : 수입의뢰 Main SUB MODULE Include(NEW)
*&      작성자 : 강석봉 INFOLINK Ltd.
*&      작성일 : 2001.08.25
*&  적용회사PJT:
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_COMPANY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_COMPANY_DATA USING    P_BUKRS
                                      P_IMTRD.

  CALL FUNCTION 'ZIM_GET_COMPANY_DATA'
       EXPORTING
          BUKRS       =    P_BUKRS
          IMTRD       =    P_IMTRD
       IMPORTING
          XT001       =    T001
          XZTIMIMG00  =    ZTIMIMG00
          XZTIMIMGTX  =    ZTIMIMGTX
          OZTIMIMGTX  =    XZTIMIMGTX
          XZTIMIMG11  =    ZTIMIMG11
       EXCEPTIONS
          NOT_FOUND   =    4.

  IF SY-SUBRC NE 0.
     MESSAGE ID   SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " P1000_READ_COMPANY_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_DEFAULT_BANK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_SET_DEFAULT_BANK.

* < 2002.06.12 CORECESS Modify. DEFAULT Bank Display >
  IF ZTIMIMG00-ZFBKYN EQ 'X'.
     CASE ZTREQHD-ZFREQTY.
        WHEN 'LC'.
           IF ZTIMIMG00-LCBKYN EQ 'X'.
              ZTREQHD-ZFOPBN = ZTIMIMG00-LCBKCD.
           ENDIF.
        WHEN 'LO'.
           IF ZTIMIMG00-LOBKYN EQ 'X'.
              ZTREQHD-ZFOPBN = ZTIMIMG00-LOBKCD.
           ENDIF.
        WHEN 'PU'.
           IF ZTIMIMG00-PUBKYN EQ 'X'.
              ZTREQHD-ZFOPBN = ZTIMIMG00-PUBKCD.
           ENDIF.
        WHEN 'DA'.
           IF ZTIMIMG00-DABKYN EQ 'X'.
              ZTREQHD-ZFOPBN = ZTIMIMG00-DABKCD.
           ENDIF.
        WHEN 'DP'.
           IF ZTIMIMG00-DPBKYN EQ 'X'.
              ZTREQHD-ZFOPBN = ZTIMIMG00-DPBKCD.
           ENDIF.
        WHEN 'TT'.
           IF ZTIMIMG00-TTBKYN EQ 'X'.
              ZTREQHD-ZFOPBN = ZTIMIMG00-TTBKCD.
           ENDIF.
        WHEN 'GS'.
           IF ZTIMIMG00-GSMBKYN EQ 'X'.
              ZTREQHD-ZFOPBN = ZTIMIMG00-GSMBKCD.
           ENDIF.
        WHEN OTHERS.
     ENDCASE.
  ENDIF.

ENDFORM.                    " P1000_SET_DEFAULT_BANK
*&---------------------------------------------------------------------*
*&      Form  P3000_SET_COMPANY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_SET_COMPANY_DATA.

   CASE ZTREQHD-ZFREQTY.
      WHEN 'LC'.
         MOVE : ZTIMIMGTX-ZFAPPNM   TO ZTMLCSG2-ZFAPPNM,
                ZTIMIMGTX-ZFAPPAD1  TO ZTMLCSG2-ZFAPPAD1,
                ZTIMIMGTX-ZFAPPAD2  TO ZTMLCSG2-ZFAPPAD2,
                ZTIMIMGTX-ZFAPPAD3  TO ZTMLCSG2-ZFAPPAD3,
                ZTIMIMGTX-ZFTELNO   TO ZTMLCSG2-ZFTELNO,
                XZTIMIMGTX-ZFELENM   TO ZTMLCSG2-ZFELENM,
                XZTIMIMGTX-ZFREPRE   TO ZTMLCSG2-ZFREPRE,
                XZTIMIMGTX-ZFELEID   TO ZTMLCSG2-ZFELEID,
                XZTIMIMGTX-ZFELEAD1  TO ZTMLCSG2-ZFELEAD1,
                XZTIMIMGTX-ZFELEAD2  TO ZTMLCSG2-ZFELEAD2.
      WHEN 'LO'.
         MOVE : XZTIMIMGTX-ZFAPPNML  TO ZTLLCSG23-ZFAPPNM1,
                XZTIMIMGTX-ZFAPPAD1L TO ZTLLCSG23-ZFAPPNM2,
                XZTIMIMGTX-ZFAPPAD2L TO ZTLLCSG23-ZFAPPNM3,
                XZTIMIMGTX-ZFELENML  TO ZTLLCSG23-ZFELENM,
                XZTIMIMGTX-ZFTDNM2   TO ZTLLCSG23-ZFREPRE,
                XZTIMIMGTX-ZFELEID   TO ZTLLCSG23-ZFELEID.
      WHEN 'PU'.
         MOVE : XZTIMIMGTX-ZFAPPNML  TO ZTPUR-ZFAPPNM1,
                XZTIMIMGTX-ZFAPPAD1L TO ZTPUR-ZFAPPAD1,
                XZTIMIMGTX-ZFAPPAD2L TO ZTPUR-ZFAPPAD2,
                XZTIMIMGTX-ZFAPPAD3L TO ZTPUR-ZFAPPAD3,
                XZTIMIMGTX-ZFELENML  TO ZTPUR-ZFELEAD1,
                XZTIMIMGTX-ZFREPREL  TO ZTPUR-ZFELEAD2,
                XZTIMIMGTX-ZFELEID   TO ZTPUR-ZFELEID.
      WHEN 'TT'.
         MOVE : ZTIMIMGTX-ZFAPPNM   TO ZTTTHD-ZFAPPNM,
                ZTIMIMGTX-ZFAPPAD1  TO ZTTTHD-ZFAPPAD1,
                ZTIMIMGTX-ZFAPPAD2  TO ZTTTHD-ZFAPPAD2,
                ZTIMIMGTX-ZFAPPAD3  TO ZTTTHD-ZFAPPAD3,
                XZTIMIMGTX-ZFELENM  TO ZTTTHD-ZFELENM,
                XZTIMIMGTX-ZFREPRE  TO ZTTTHD-ZFREPRE,
                XZTIMIMGTX-ZFELEID  TO ZTTTHD-ZFELEID.           "
   ENDCASE.

ENDFORM.                    " P3000_SET_COMPANY_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_CHECK_BANK_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_CHECK_BANK_CODE.

   IF ZTIMIMG00-ZFBKYN EQ 'X'.  EXIT.  ENDIF.

   CASE ZTREQHD-ZFREQTY.
      WHEN 'LC'.
         IF ZTIMIMG00-LCBKYN NE 'X' AND ZTREQHD-ZFOPBN IS INITIAL.
            PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFOPBN'.
         ENDIF.
      WHEN 'LO'.
         IF ZTIMIMG00-LOBKYN NE 'X' AND ZTREQHD-ZFOPBN IS INITIAL.
            PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFOPBN'.
         ENDIF.
*>> NCW 수정 2003.12.03 - DA,DP,TT 은행배정 체크 막음
*      WHEN 'DA'.
*         IF ZTIMIMG00-DABKYN NE 'X' AND ZTREQHD-ZFOPBN IS INITIAL.
*            PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFOPBN'.
*         ENDIF.
*      WHEN 'DP'.
*         IF ZTIMIMG00-DPBKYN EQ 'X' AND ZTREQHD-ZFOPBN IS INITIAL.
*            PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFOPBN'.
*         ENDIF.
      WHEN 'PU'.
         IF ZTIMIMG00-PUBKYN EQ 'X' AND ZTREQHD-ZFOPBN IS INITIAL.
            PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFOPBN'.
         ENDIF.
*      WHEN 'TT'.
*        IF ZTIMIMG00-TTBKYN EQ 'X' AND ZTREQHD-ZFOPBN IS INITIAL.
*            PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFOPBN'.
*         ENDIF.
      WHEN 'GS'.
         IF ZTIMIMG00-GSMBKYN EQ 'X' AND ZTREQHD-ZFOPBN IS INITIAL.
            PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFOPBN'.
         ENDIF.
   ENDCASE.

ENDFORM.                    " P2000_CHECK_BANK_CODE
*&---------------------------------------------------------------------*
*&      Form  P2000_LGI_SEND_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_LGI_SEND_MESSAGE.

  IF W_STATUS EQ C_REQ_D.
    PERFORM P2000_MESSAGE_BOX USING '부보의뢰 확인'   " 타이틀..
                      '현재 문서의 내역으로 부보의뢰 합니다.'
                      '의뢰 하시겠습니까?'                  " MSG2
                      'N'                      " 취소 버튼 유/?
                      '1'.                     " default button

  ELSE.
    PERFORM P2000_MESSAGE_BOX USING '부보의뢰 확인'       " 타이틀..
                      '변경된 Data는 저장후에 반영됩니다.'
                      '저장 후 부보의뢰 하시겠습니까?'      " MSG2
                      'Y'                      " 취소 버튼 유/?
                      '1'.                     " default button
  ENDIF.

ENDFORM.                    " P2000_LGI_SEND_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_LGI_SEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LGI_SEND.

   IF W_STAUTS NE C_REQ_D.
      PERFORM  P3000_DB_MODIFY_SCRCOM.
   ENDIF.

   IF ZTINS-ZFEDICK EQ 'X'.
      IF W_STAUTS EQ C_REQ_D.
         PERFORM P2000_SET_INS_DOC_LOCK USING 'U'.
      ENDIF.
      MESSAGE E119 WITH ZTINS-ZFREQNO ZTINS-ZFINSEQ.
   ENDIF.

   CALL FUNCTION 'ZIM_INSURANCE_LG_TO_LGI'
        EXPORTING
            ZFREQNO     =     ZTINS-ZFREQNO
            ZFINSEQ     =     ZTINS-ZFINSEQ
            ZFAMDNO     =     ZTINS-ZFAMDNO
        EXCEPTIONS
            SEND_ERROR  =     4.
   IF SY-SUBRC NE 0.
      MESSAGE S927.
   ENDIF.
   CLEAR OK-CODE.
   PERFORM  P2000_SET_UNLOCK.
   PERFORM  P2000_SET_SCREEN_SCRCOM.
   LEAVE SCREEN.

ENDFORM.                    " P3000_LGI_SEND
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_PO_ITEM_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_8842   text
*      -->P_IT_ZSREQIT_EBELN  text
*      -->P_IT_ZSREQIT_EBELP  text
*----------------------------------------------------------------------*
FORM P1000_GET_PO_ITEM_TEXT USING    P_ID
                                     P_EBELN
                                     P_EBELP
                            CHANGING   W_ZFLSG7G.

    CLEAR : EKPO.
    SELECT SINGLE * FROM EKPO WHERE EBELN = P_EBELN
                              AND   EBELP = P_EBELP.
    IF EKPO-MATNR IS INITIAL. EXIT.  ENDIF.

    HTEXT-TDOBJECT     = 'MATERIAL'.
    HTEXT-TDID         = P_ID.
    HTEXT-TDSPRAS      = 'KO'.
    HTEXT-TDNAME(18)   = EKPO-MATNR.

    REFRESH TLINETAB.
    CALL FUNCTION 'READ_TEXT'
         EXPORTING
              ID        = HTEXT-TDID
              LANGUAGE  = HTEXT-TDSPRAS
              NAME      = HTEXT-TDNAME
              OBJECT    = HTEXT-TDOBJECT
         IMPORTING
              HEADER    = THEAD
         TABLES
              LINES     = TLINETAB
         EXCEPTIONS
              ID        = 1
              LANGUAGE  = 2
              NAME      = 3
              NOT_FOUND = 4
              OBJECT    = 5
              OTHERS    = 6.

    LOOP AT TLINETAB.
       W_DESC_TEXT = TLINETAB-TDLINE.
       IF NOT W_DESC_TEXT IS INITIAL.
          PERFORM   P3000_ITEM_DESC_WRITE   USING      W_DESC_TEXT
                                            CHANGING   W_ZFLSG7G.
       ENDIF.
    ENDLOOP.

ENDFORM.                    " P1000_GET_PO_ITEM_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_OFFER
*&---------------------------------------------------------------------*
FORM P2000_SHOW_OFFER.

  PERFORM P1000_READ_OFFER_DATA.


ENDFORM.                    " P2000_SHOW_OFFER
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_OFFER_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_OFFER_DATA.

  DATA : L_NAME1(255),
         L_NAME2(255),
         L_NAME3(255),
         L_NAME4(255).

  CLEAR : W_LFA1.

  SELECT SINGLE *
           FROM ZTREQHD
          WHERE ZFREQNO EQ ZSREQHD-ZFREQNO.

  SELECT SINGLE *
           FROM ZTMLCHD
          WHERE ZFREQNO EQ ZSREQHD-ZFREQNO.

  MOVE ZSREQHD-ZFREQNO  TO G_REQNO.
  MOVE ZTREQHD-ZFSPRT   TO G_SPRT.
  MOVE ZTREQHD-ZFAPRT   TO G_APRT.
  MOVE ZTREQHD-ZFREQSD  TO G_REQSD.
  MOVE ZTREQHD-ZFREQED  TO G_REQED.
  MOVE ZTREQHD-EBELN    TO G_EBELN.
  MOVE ZTREQHD-ZFREQTY  TO G_REQTY.
  MOVE ZTMLCHD-INCO1    TO G_INCO1.
  MOVE ZTMLCHD-ZFINCP     TO G_INCP.

  IF ZTREQHD-ZZPSHIP EQ 'X'.
    MOVE 'ALLOWED' TO G_PSYN.
  ELSE.
    MOVE 'NOT ALLOWED' TO G_PSYN.
  ENDIF.

  IF ZTREQHD-ZZTSHIP EQ 'X'.
    MOVE 'ALLOWED' TO G_TSYN.
  ELSE.
    MOVE 'NOT ALLOWED' TO G_TSYN.
  ENDIF.

  SELECT SINGLE ZFORIG INTO W_ORIG
           FROM ZTMLCSG7O
          WHERE ZFREQNO EQ ZSREQHD-ZFREQNO.

  SELECT SINGLE LANDX INTO G_ORNM
           FROM T005T
          WHERE LAND1 = W_ORIG
            AND SPRAS = SY-LANGU.

  CALL FUNCTION 'ZIM_GET_VENDOR_ADDRESS_FORMAT'
       EXPORTING
            LIFNR     = ZTREQHD-ZFBENI
       IMPORTING
            NAME1     = L_NAME1
            NAME2     = L_NAME2
            NAME3     = L_NAME3
            NAME4     = L_NAME4
            P_LFA1    = W_LFA1
            P_ADRC    = W_ADRC
       EXCEPTIONS
            NO_INPUT  = 01
            NOT_FOUND = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE I025.
    WHEN 03.     MESSAGE E020   WITH    ZTREQHD-ZFBENI.
  ENDCASE.

  TRANSLATE : L_NAME1 TO UPPER CASE,
              L_NAME2 TO UPPER CASE,
              L_NAME3 TO UPPER CASE,
              L_NAME4 TO UPPER CASE.

  MOVE : L_NAME1      TO W_LFA1-NAME1,
         L_NAME2      TO W_LFA1-NAME2,
         L_NAME3      TO W_LFA1-NAME3,
         L_NAME4      TO W_LFA1-NAME4.

  MOVE: W_LFA1-NAME1   TO   G_BENIF_NM.
  MOVE: W_LFA1-NAME2   TO   G_BENI_ADD.
  MOVE: W_LFA1-NAME3   TO   G_BENI_ADD1.

  CLEAR : W_LFA1, W_ADRC.

  SELECT SINGLE *
           FROM LFA1
          WHERE LIFNR = ZTREQHD-ZFBENI.

  IF LFA1-LAND1 IS INITIAL.
    MESSAGE E977 WITH
    'Not maintain country code in vendor master record.'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM LFBK
          WHERE LIFNR = LFA1-LIFNR
            AND BANKS = LFA1-LAND1.

  SELECT SINGLE *
           FROM BNKA
          WHERE BANKS = LFA1-LAND1
            AND BANKL = LFBK-BANKL.

  SELECT SINGLE *
           FROM T005U
          WHERE SPRAS = SY-LANGU
            AND LAND1 = LFA1-LAND1
            AND BLAND = BNKA-PROVZ.

  SELECT SINGLE *
           FROM ADRC
          WHERE ADDRNUMBER = BNKA-ADRNR.

  MOVE BNKA-BANKA        TO G_ADVBK_NM.
  MOVE T005U-BEZEI       TO G_ADVBK_ADD.
  MOVE BNKA-STRAS        TO G_ADVBK_ADD1.
  MOVE BNKA-ORT01        TO G_ADVBK_ADD2.
  MOVE LFBK-BANKN        TO G_ADVBK_ACCNO.
  CONCATENATE ADRC-TEL_NUMBER ADRC-TEL_EXTENS
         INTO G_ADVBK_TELNO SEPARATED BY SPACE.
  CONCATENATE ADRC-FAX_NUMBER ADRC-FAX_EXTENS
         INTO G_ADVBK_FAXNO SEPARATED BY SPACE.
*  MOVE ADRC-TEL_EXTENS   TO G_ADVBK-TELNO2.

  SELECT * INTO TABLE SM_REQIT
           FROM ZTREQIT
          WHERE ZFREQNO = ZSREQHD-ZFREQNO.

  SORT SM_REQIT BY ZFITMNO.

ENDFORM.                    " P1000_READ_OFFER_DATA

*&---------------------------------------------------------------------*
*&      Form  P2000_TRANS_METHOD_SET_BL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_TRANS_METHOD_SET_BL.

DATA : L_ZFLAGR   LIKE   ZTINSBAGR-ZFLAGR.

* 보험구분.
  CASE ZTINS-ZFTRANS.
     WHEN 'A' OR 'B'.
        MOVE: 'A'                   TO ZTINSBSG3-ZFCARNU, " 항?
             'PER REGULAR AIRLINER' TO ZTINSBSG3-ZFCARNM. " 선기?
     WHEN 'O'.
        MOVE:'T.B.D'                TO ZTINSBSG3-ZFCARNU, " 항?
             'TO BE DECLARED'       TO ZTINSBSG3-ZFCARNM. " 선기?
  ENDCASE.

ENDFORM.                    " P2000_TRANS_METHOD_SET_BL
*&---------------------------------------------------------------------*
*&      Form  P2000_REQUESTER_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_REQUESTER_DISPLAY  USING    P_EBELN
                                       P_EBELP.

 DATA : L_AFNAM   LIKE  BAPIBNAME-BAPIBNAME.

 SELECT SINGLE * FROM EKPO
                WHERE EBELN = P_EBELN
                  AND EBELP = P_EBELP.

  IF EKPO-AFNAM IS INITIAL.
     MESSAGE E468(ZIM1).
     EXIT.
  ELSE.
    TRANSLATE EKPO-AFNAM TO UPPER CASE.
  ENDIF.

  MOVE   EKPO-AFNAM   TO  L_AFNAM.

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
