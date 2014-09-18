*&---------------------------------------------------------------------*
*& INCLUDE ZRIM00I01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰 Main PAI MODULE Include                      *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.01.25                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&   DESC : 공통 모듈( CANCEL일 경우, PROGRAM 종료 모듈 )
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.

  IF W_STATUS EQ 'D'.
    SET SCREEN 0.   LEAVE SCREEN.
  ELSE.
    IF SY-DYNNR EQ '0100' OR SY-DYNNR EQ '0200' OR SY-DYNNR EQ '0300'
    OR SY-DYNNR EQ '0500' OR SY-DYNNR EQ '0700'
    OR SY-DYNNR EQ '1100' OR SY-DYNNR EQ '1200' OR SY-DYNNR EQ '1300'
    OR SY-DYNNR EQ '1500' OR SY-DYNNR EQ '1700'
    OR SY-DYNNR EQ '2100' OR SY-DYNNR EQ '2200' OR SY-DYNNR EQ '2300'
    OR SY-DYNNR EQ '4100' OR SY-DYNNR EQ '4200' OR SY-DYNNR EQ '4300'
    OR SY-DYNNR EQ '4400'
    OR SY-DYNNR EQ '4500' OR SY-DYNNR EQ '4600' OR SY-DYNNR EQ '4700'
    OR SY-DYNNR EQ '4800' OR SY-DYNNR EQ '0190'.
      SET SCREEN 0.   LEAVE SCREEN.
    ELSE.
      PERFORM P2000_SET_MESSAGE USING  SY-UCOMM.
    ENDIF.
  ENDIF.

ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_PO_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&   DESC : 구매오더의 데이타를 Read
*----------------------------------------------------------------------*
MODULE READ_PO_SCR0100 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.
*-----------------------------------------------------------------------
* P/O HEADER TABLE SELECT ( EKKO )
*-----------------------------------------------------------------------
  REFRESH : IT_ZSREQIT, IT_ZSREQIT_ORG, IT_ZTREQORJ, IT_ZTREQORJ_ORG,
            IT_ZSREQIL, IT_ZSREQIL_ORG, IT_ZSIMCOST.
  CLEAR : ZTREQHD, ZTREQST.

  CALL FUNCTION 'ZIM_GET_PO_HEADER'
         EXPORTING
               EBELN   =  ZSREQHD-EBELN
         IMPORTING
               W_EKKO  =  EKKO
         EXCEPTIONS
                NOT_INPUT      =   1
                NOT_FOUND      =   2
                NOT_RELEASED   =   3
                NOT_TYPE       =   4
                NO_TRANSACTION =   5
                PO_DELETION    =   6.

  CASE SY-SUBRC.
    WHEN 1.    MESSAGE E003.
    WHEN 2.    MESSAGE E001     WITH ZSREQHD-EBELN.
    WHEN 3.    MESSAGE E390(ME) WITH ZSREQHD-EBELN.
    WHEN 4.    MESSAGE E002     WITH ZSREQHD-EBELN EKKO-BSTYP.
    WHEN 5.    MESSAGE E000     WITH 'ME23N'.
    WHEN 6.    MESSAGE E005     WITH ZSREQHD-EBELN.
  ENDCASE.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
    MESSAGE E963.
  ENDIF.

*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 ) --> Vendor
*-----------------------------------------------------------------------
  CALL FUNCTION 'READ_LFA1'
       EXPORTING
            XLIFNR         = EKKO-LIFNR
       IMPORTING
            XLFA1          = LFA1
       EXCEPTIONS
            KEY_INCOMPLETE = 01
            NOT_AUTHORIZED = 02
            NOT_FOUND      = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE E022.
    WHEN 02.     MESSAGE E950.
    WHEN 03.     MESSAGE E020   WITH    EKKO-LIFNR.
  ENDCASE.

  IF NOT EKKO-LLIEF IS INITIAL.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 ) --> Supply Vendor
*-----------------------------------------------------------------------
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = EKKO-LLIEF
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    CASE SY-SUBRC.
      WHEN 01.     MESSAGE W115.
      WHEN 02.     MESSAGE E950.
      WHEN 03.     MESSAGE E116   WITH    EKKO-LLIEF.
    ENDCASE.
  ENDIF.

*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 ) --> Invoice Partner
*-----------------------------------------------------------------------
  IF NOT EKKO-LIFRE IS INITIAL.
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = EKKO-LIFRE
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    CASE SY-SUBRC.
      WHEN 01.     MESSAGE W345.
      WHEN 02.     MESSAGE E950.
      WHEN 03.     MESSAGE E346   WITH    EKKO-LIFRE.
    ENDCASE.
  ENDIF.

*-----------------------------------------------------------------------
* Import P/O Check ( ZTIMIMG01 )
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_GET_ZTIMIMG01'
       EXPORTING
            BSART       = EKKO-BSART
            BSTYP       = EKKO-BSTYP
            ZTERM       = EKKO-ZTERM
       IMPORTING
            W_ZFREQTY   = ZSREQHD-ZFREQTY
            W_ZTIMIMG01 = ZTIMIMG01
       EXCEPTIONS
            NOT_OK      = 1
            NOT_FOUND   = 2.

  W_ZFREQTY = ZTIMIMG01-ZFREQTY.
  W_ZFLCKN  = ZTIMIMG01-ZFLCKN.

  CASE SY-SUBRC.
    WHEN 1.    MESSAGE E004     WITH ZSREQHD-EBELN EKKO-ZTERM.
    WHEN 2.    MESSAGE E958     WITH EKKO-ZTERM.
    WHEN OTHERS.
  ENDCASE.

*-----------------------------------------------------------------------
* ORIGIN SELECT
*-----------------------------------------------------------------------
  IF W_ZFREQTY NE 'LO' AND W_ZFREQTY NE 'PU'.
    SELECT SINGLE LANDX
      INTO IT_ZTREQORJ-ZFORNM
      FROM T005T
     WHERE LAND1 = LFA1-LAND1
       AND SPRAS  EQ  'EN'.

    IF SY-SUBRC  EQ  0 .
      IT_ZTREQORJ-ZFORIG = LFA1-LAND1.
      IT_ZTREQORJ-ZFLSG7O = 10.
      APPEND IT_ZTREQORJ.
      IT_ZTREQORJ_ORG = IT_ZTREQORJ.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------------------
* Purchasing Organization SELECT ( T024E )
*-----------------------------------------------------------------------
  CALL FUNCTION 'T024E_SINGLE_READ'
       EXPORTING
            T024E_EKORG = EKKO-EKORG
       IMPORTING
            WT024E      = T024E
       EXCEPTIONS
            NOT_FOUND   = 1
            OTHERS      = 2.

  IF SY-SUBRC NE 0.
    MESSAGE E001(MH) WITH EKKO-EKORG.
  ENDIF.

*-----------------------------------------------------------------------
* Purchasing Group SELECT ( T024 )
*-----------------------------------------------------------------------
  CALL FUNCTION 'MM_ARRANG_PURCHASE_GRP_SELECT'
       EXPORTING
            PURCHASE_GROUP           = EKKO-EKGRP
       IMPORTING
            E_T024                   = T024
       EXCEPTIONS
            PURCHASE_GROUP_NOT_FOUND = 1
            OTHERS                   = 2.

  IF SY-SUBRC NE 0.
    MESSAGE E030(MN) WITH EKKO-EKGRP.
  ENDIF.

*-----------------------------------------------------------------------
* P/O ITEM TABLE SELECT ( EKPO )
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_GET_PO_ITEM'
         EXPORTING
               EBELN   =  ZSREQHD-EBELN
               KNUMV   =  EKKO-KNUMV
               KSCHL   =  ZTIMIMG00-ZFKSCHL3  "INSTALL Chg.
               LOEKZ   =  SPACE
               ELIKZ   =  SPACE
         IMPORTING
               W_ITEM_CNT      =   W_ITEM_CNT
               W_TOT_AMOUNT    =   W_TOT_AMOUNT
               W_ZFMAUD        =   W_ZFMAUD
               W_ZFWERKS       =   W_ZFWERKS
               W_BEDNR         =   W_BEDNR
               W_MATNR         =   W_MATNR
               W_TXZ01         =   W_TXZ01
         TABLES
               IT_ZSREQIT      =   IT_ZSREQIT
               IT_ZSREQIT_ORG  =   IT_ZSREQIT_ORG
         EXCEPTIONS
                KEY_INCOMPLETE =   1
                NOT_FOUND      =   2
                NO_REFERENCE   =   3
                NO_AMOUNT      =   4.

  CASE SY-SUBRC.
    WHEN 1.    MESSAGE E003.
    WHEN 2.    MESSAGE E006     WITH ZSREQHD-EBELN.
    WHEN 3.    MESSAGE E006     WITH ZSREQHD-EBELN.
    WHEN 4.    MESSAGE E007     WITH ZSREQHD-EBELN.
  ENDCASE.

*-----------------------------------------------------------------------
* Item Count
*-----------------------------------------------------------------------
  IF W_ITEM_CNT EQ 0.
    MESSAGE E006     WITH ZSREQHD-EBELN.
  ENDIF.

*-----------------------------------------------------------------------
* Amount Zero
*-----------------------------------------------------------------------
  IF W_TOT_AMOUNT <= 0.
    MESSAGE E007     WITH ZSREQHD-EBELN.
  ENDIF.

* Local Process => Only 1.
  IF W_ZFREQTY EQ 'LO' OR W_ZFREQTY EQ 'PU'.
    W_REQ_CNT = 0.
    SELECT COUNT( * ) INTO W_REQ_CNT FROM ZTREQHD
                      WHERE EBELN EQ ZSREQHD-EBELN.
    IF W_REQ_CNT GT 0.
      MESSAGE E264 WITH ZSREQHD-EBELN.
    ENDIF.
  ENDIF.

* P/O LOCK
  CALL FUNCTION 'ENQUEUE_EMEKKOE'
       EXPORTING
            EBELN  = ZSREQHD-EBELN
       EXCEPTIONS
            OTHERS = 1.

  IF SY-SUBRC <> 0.
    MESSAGE E510 WITH SY-MSGV1 'Purchase Order' ZSREQHD-EBELN SPACE
                 RAISING DOCUMENT_LOCKED.
  ENDIF.

* Get Packing Charge / Handing Charge
  PERFORM  P1000_READ_HEADER_CHARGE  USING  EKKO-KNUMV
                                            EKKO-EBELN.
*-----------------------------------------------------------------------
* Latest Open Amount Auto Change
*-----------------------------------------------------------------------
  ZTREQHD-WAERS = EKKO-WAERS.
  ZTREQHD-INCO1 = EKKO-INCO1.
  CLEAR : ZTREQHD-ZFINSYN.

  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

ENDMODULE.                 " READ_PO_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.
* W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      ANTWORT = 'Y'.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0100 INPUT.
  W_OK_CODE = OK-CODE.
  W_ZFLSG7O = 0.
* Old Document Copy
  IF OK-CODE EQ 'COPY'.
    PERFORM  P2000_PRE_DOC_NO_INPUT.
  ENDIF.

*-----------------------------------------------------------------------
* DATA MOVE
*-----------------------------------------------------------------------
  IF ( W_OK_CODE EQ 'COPY' AND ANTWORT EQ 'Y' ) OR
     ( W_OK_CODE EQ 'ENTR' ).

    PERFORM   P2000_MOVE_PO_DATA.

    IF ZTIMIMG00-ZFRELYN1 EQ 'X'.
      SET SCREEN 0151.  LEAVE TO SCREEN 0151.
    ELSE.
      IF ZTREQHD-ZFREQTY EQ 'LC'.
        PERFORM   P3000_SET_ITEM_MARK   USING   0     'X'.
        PERFORM   P3000_GOOD_DESC_CREATE.
        PERFORM   P3000_SET_ITEM_MARK   USING   0     ''.
      ENDIF.
      PERFORM  P2000_SET_REQ_SCR.
    ENDIF.
  ELSE.
    CALL FUNCTION 'DEQUEUE_EMEKKOE'
         EXPORTING
              EBELN = ZSREQHD-EBELN.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR0102 INPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZTREQHD-ZTERM'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF SCREEN-INPUT EQ '1'.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZTREQHD-ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ELSE.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZTREQHD-ZTERM
              I_XSHOW       = 'X'
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ENDIF.

  IF SY-SUBRC NE 0.
*   message e177 with ekko-zterm.
    MESSAGE S177(06) WITH ZTREQHD-ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    ZTREQHD-ZTERM = T052-ZTERM.
  ENDIF.


ENDMODULE.                 " HELP_ZTERM_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0101  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0101 INPUT.
  CASE OK-CODE.
    WHEN 'ZIMG'.                    " Import IMG
      CALL TRANSACTION 'ZIMGM'.
    WHEN 'DDLC'.                    " Double Click Event
      PERFORM  P2000_DDLC_PROCESS.
    WHEN 'COST'.
      IF SY-TCODE(4) NE 'ZIM4'.
        PERFORM  P2000_CALL_COST_SCREEN.
      ELSE.
        PERFORM  P4000_DISPLAY_DOCUMENT.
      ENDIF.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'OTDC' OR 'CRDC' OR 'CHDC' OR 'DISP' OR 'ADDC' OR 'OPDC'.
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'DELE' OR 'DELR' OR 'REVK' OR 'OPCL' OR 'EDIS'.
      PERFORM  P2000_SET_INDICATE.
    WHEN 'LGIS'.           ">부보의뢰.
      PERFORM  P2000_SET_INDICATE.
    WHEN 'FLAT'.           " FLAT DATA
      PERFORM  P2000_SHOW_FLAT.
    WHEN 'CHFD'.           " 변경항목 조회.
      PERFORM  P2000_CHANGE_ITEM.
    WHEN 'OFCR'.           " Offer Sheet 조회.
      PERFORM  P2000_SHOW_OFFER.
    WHEN 'CKEK'.           " 검증.
      PERFORM P2000_EDI_DOC_CHECK.
    WHEN 'RECO'.           " 수입추천.
      PERFORM P2000_REQ_IL_DISPLAY.
    WHEN 'STAT'.           " 문서 상태 POPUP
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN 'ME23'.           "구매문서조회...
      PERFORM  P2000_PO_DOCUMENT_DISPLAY.
    WHEN 'MK03'.           "구매거래처 조회.
      PERFORM  P2000_VENDOR_DISPLAY.
    WHEN 'ZIBK'.           " 개설은행.
      PERFORM  P2000_OPEN_BANK_DISPLAY.
    WHEN 'ZIBE'.           " Beneficiary
      PERFORM  P2000_VENDOR_MASTER USING ZTREQHD-ZFBENI.
    WHEN 'ZICI'.           " 차입기관.
      PERFORM  P2000_VENDOR_MASTER USING ZTREQHD-ZFLEVN.
    WHEN 'ZIM03'.          " L/C
      PERFORM  P2000_LC_DOC_DISPLAY    USING  ZTREQHD-ZFREQNO ''.
    WHEN 'PREV'.           " 이전 L/C
      IF W_STATUS EQ C_REQ_C.
        W_ZFAMEND = ZTREQST-ZFAMDNO.
      ELSE.
        W_ZFAMEND = ZTREQST-ZFAMDNO - 1.
      ENDIF.
      PERFORM   P1000_PREV_NEXT_DOC  USING ZTREQHD-ZFREQNO  W_ZFAMEND.
    WHEN 'NEXT'.           " 이후 L/C
      W_ZFAMEND = ZTREQST-ZFAMDNO + 1.
      PERFORM   P1000_PREV_NEXT_DOC  USING ZTREQHD-ZFREQNO  W_ZFAMEND.
    WHEN 'ZIM93'.          " 구매오더별 HISTORY
      PERFORM  P2000_ZIM93_DISPLAY     USING  ZTREQHD-ZFREQNO
                                              ZTREQHD-EBELN.
*   WHEN 'ZIBK'.           " 개설은?
*      PERFORM  P2000_OPEN_BANK_DISPLAY.

    WHEN 'HIST'.           "헤더변경문?
*-----------------------------------------------------------------------
*  HISTORY CHANGE를 보는 프로그?
*-----------------------------------------------------------------------
      PERFORM   P2000_HEADER_CHANGE_DOC.
    WHEN 'APP700'.
      CASE ZTREQHD-ZFREQTY.
        WHEN 'LC'.
          SUBMIT ZRIMAPP700 WITH P_REQNO EQ ZTREQHD-ZFREQNO
                            AND RETURN. " 다시 돌아 올수 있게.
        WHEN 'LO'.
          SUBMIT ZRIMLOCAPP WITH P_REQNO EQ ZTREQHD-ZFREQNO
                            AND RETURN. " 다시 돌아 올수 있게.
        WHEN 'TT'.
          SUBMIT ZRIMPAYORD WITH P_REQNO EQ ZTREQHD-ZFREQNO
                            AND RETURN. " 다시 돌아 올수 있게.
        WHEN 'PU'.
*               SUBMIT ZRIMAPPPUR WITH P_REQNO EQ ZTREQHD-ZFREQNO
*                                 WITH P_AMDNO EQ ZTREQST-ZFAMDNO
*                                 AND RETURN. " 다시 돌아 올수 있게.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'INF700'.
      CASE ZTREQHD-ZFREQTY.
        WHEN 'LC'.
          SUBMIT ZRIMINF700 WITH P_REQNO EQ ZTREQHD-ZFREQNO
                            AND RETURN. " 다시 돌아 올수 있게.
        WHEN 'LO'.
          SUBMIT ZRIMLOCADV WITH P_REQNO EQ ZTREQHD-ZFREQNO
                            AND RETURN. " 다시 돌아 올수 있게.
        WHEN 'TT'.
*               SUBMIT ZRIMDEBADV WITH P_REQNO EQ ZTREQHD-ZFREQNO
*                                 AND RETURN. " 다시 돌아 올수 있게.
        WHEN 'PU'.
*               SUBMIT ZRIMPURLIC WITH P_REQNO EQ ZTREQHD-ZFREQNO
*                                 WITH P_AMDNO EQ ZTREQST-ZFAMDNO
*                                 AND RETURN. " 다시 돌아 올수 있게.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'APP707'.
      CASE ZTREQHD-ZFREQTY.
        WHEN 'LC'.
          SUBMIT ZRIMAPP707 WITH P_REQNO EQ ZTREQHD-ZFREQNO
                            WITH P_AMDNO EQ ZTREQST-ZFAMDNO
                            AND RETURN. " 다시 돌아 올수 있게.
        WHEN 'LO'.
          SUBMIT ZRIMLOCAMR WITH P_REQNO EQ ZTREQHD-ZFREQNO
                            WITH P_AMDNO EQ ZTREQST-ZFAMDNO
                            AND RETURN. " 다시 돌아 올수 있게.
        WHEN 'TT'.
          SUBMIT ZRIMPAYORD WITH P_REQNO EQ ZTREQHD-ZFREQNO
                            AND RETURN. " 다시 돌아 올수 있게.
        WHEN 'PU'.
*               SUBMIT ZRIMAPPPUR WITH P_REQNO EQ ZTREQHD-ZFREQNO
*                                 WITH P_AMDNO EQ ZTREQST-ZFAMDNO
*                                 AND RETURN. " 다시 돌아 올수 있게.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'INF707'.
      CASE ZTREQHD-ZFREQTY.
        WHEN 'LC'.
          SUBMIT ZRIMINF707 WITH P_REQNO EQ ZTREQHD-ZFREQNO
                            WITH P_AMDNO EQ ZTREQST-ZFAMDNO
                            AND RETURN. " 다시 돌아 올수 있게.
        WHEN 'LO'.
          SUBMIT ZRIMLOCAMA WITH P_REQNO EQ ZTREQHD-ZFREQNO
                            WITH P_AMDNO EQ ZTREQST-ZFAMDNO
                            AND RETURN. " 다시 돌아 올수 있게.
        WHEN 'TT'.
*               SUBMIT ZRIMDEBADV WITH P_REQNO EQ ZTREQHD-ZFREQNO
*                                 WITH P_AMDNO EQ ZTREQST-ZFAMDNO
*                                 AND RETURN. " 다시 돌아 올수 있게.
        WHEN 'PU'.
*               SUBMIT ZRIMPURLIC WITH P_REQNO EQ ZTREQHD-ZFREQNO
*                                 WITH P_AMDNO EQ ZTREQST-ZFAMDNO
*                                 AND RETURN. " 다시 돌아 올수 있게.
        WHEN OTHERS.
      ENDCASE.
*   WHEN 'TTREQ'.          "송금요청서.
*     SUBMIT ZRIMTTREQ  WITH P_REQNO EQ ZTREQHD-ZFREQNO
*                       WITH P_AMDNO EQ ZTREQST-ZFAMDNO
*                       AND RETURN. " 다시 돌아 올수 있게.

    WHEN 'PUDOC'.          "구매승인서.
      SUBMIT ZRIMPUDOC  WITH P_REQNO EQ ZTREQHD-ZFREQNO
                        WITH P_AMDNO EQ ZTREQST-ZFAMDNO
                        AND RETURN. "다시돌아 올 수 있게.

    WHEN 'UCUR'.           "개설화폐변?
*>>>>> P/O에서부터 통화단위를 변경하여야만 함.===> 2000/10/30 KSB
      EXIT.
*           SPOP-TITEL = 'Changes : 통화단위'.
*           CANCEL_OPTION = 'Y'.
**     CLEAR : CANCEL_OPTION.
*           OPTION = 1.
*           TEXTLEN = 40.
*           MOVE : ZTREQHD-ZFUPYN   TO ZSREQHD-ZFUPYN,
*                  ZTREQHD-WKURS    TO ZSREQHD-WKURS,
*                  ZTREQHD-WAERS    TO ZSREQHD-WAERS,
*                  ZTREQST-ZFOPAMT  TO ZSREQHD-ZFOPAMT.
*
*           IF *ZTREQHD-ZFOPAMT IS INITIAL.
*              MOVE : ZTREQST-ZFOPAMT  TO *ZTREQHD-ZFOPAMT,
*                     ZTREQST-WAERS    TO *ZTREQHD-ZFWAERS,
*                     ZTREQST-ZFOPAMT  TO ZSREQHD-ZFOPAMT,
*                     ZTREQST-WAERS    TO ZSREQHD-WAERS.
*           ELSE.
*              MOVE : ZTREQST-ZFOPAMT  TO ZSREQHD-ZFOPAMT,
*                     ZTREQST-WAERS    TO ZSREQHD-WAERS.
*           ENDIF.
**           IF ZTREQHD-WKURS IS INITIAL.
*              W_WKURS = ZTREQHD-WKURS.
**           ENDIF.
*
**           CALL SCREEN 0017 STARTING AT 15 5
**                            ENDING   AT 74 12.
*
**           IF ANTWORT EQ 'Y'.
**
**              PERFORM   P2000_COVERT_CURRENCY.
**
**           ENDIF.
*
    WHEN 'HIIT'.           "품목변경문?
*-----------------------------------------------------------------------
*  HISTORY CHANGE를 보는 프로그?
*-----------------------------------------------------------------------
      PERFORM P2000_SELECT_MATERIAL.     " 선택된 자?
      PERFORM P2000_ITEM_CHANGE_HISTORY.
*     WHEN 'ZI72'.           "자재별 진행관?
*          PERFORM SELECT_MATERIAL_CHECK.
*          PERFORM MATERIAL_HIERARCHY.
    WHEN 'MM03'.           "자재 기준정?
      PERFORM P2000_SELECT_MATERIAL.     " 선택된 자?
      PERFORM P2000_MATERIAL_DISPLAY.
    WHEN 'MD04'.           "재고/소요량 리스?
      PERFORM P2000_SELECT_MATERIAL.     " 선택된 자?
      PERFORM P2000_MATERIAL_MD04.
    WHEN 'MMBE'.           "재고개?
      PERFORM P2000_SELECT_MATERIAL.     " 선택된 자?
      PERFORM P2000_MATERIAL_MMBE.
    WHEN 'MB51'.           "자재별 자재문?
      PERFORM P2000_SELECT_MATERIAL.     " 선택된 자?
      PERFORM P2000_MATERIAL_MB51.
    WHEN 'ME2M'.           "미결 구매 오?
      PERFORM P2000_SELECT_MATERIAL.     " 선택된 자?
      PERFORM P2000_MATERIAL_ME2M.
    WHEN 'ME03'.           "소스 리스?
      PERFORM P2000_SELECT_MATERIAL.     " 선택된 자?
      PERFORM P2000_MATERIAL_ME03.
    WHEN 'EDI1'.         " 송신문서 EDI REPORT
      CASE SY-TCODE(4).
        WHEN 'ZIM0' OR 'ZIM1'.   MOVE ZTREQST-ZFDOCNO TO W_ZFDOCNO.
        WHEN 'ZIM4'.             MOVE ZTINS-ZFDOCNO   TO W_ZFDOCNO.
        WHEN 'ZIML'.             MOVE ZTOFF-ZFDOCNO   TO W_ZFDOCNO.
        WHEN OTHERS.   EXIT.
      ENDCASE.
      IF W_ZFDOCNO IS INITIAL.
        MESSAGE E324.
      ELSE.
        SELECT SINGLE * FROM ZTDHF1
                        WHERE ZFDHENO EQ W_ZFDOCNO.
        IF SY-SUBRC NE 0.
          MESSAGE E323 WITH W_ZFDOCNO.
        ENDIF.
      ENDIF.

      CASE SY-TCODE(4).
        WHEN 'ZIM0' OR 'ZIM1'.
          PERFORM P2000_EDI_SEND_REPORT.
*            WHEN 'ZIM4'.
*            WHEN 'ZIML'.
        WHEN OTHERS.
          MESSAGE I299 WITH  ZTREQST-ZFDOCNO.
          SUBMIT  ZRIMFLAT_DISP  WITH P_DDENO  EQ W_ZFDOCNO
                  AND RETURN.
      ENDCASE.
    WHEN 'EDI2'.         " 수신문서 EDI REPORT
      CASE SY-TCODE(4).
        WHEN 'ZIM0' OR 'ZIM1'.   MOVE ZTREQST-ZFDOCNOR TO W_ZFDOCNO.
        WHEN 'ZIM4'.             MOVE ZTINS-ZFDOCNOR   TO W_ZFDOCNO.
        WHEN 'ZIML'.   EXIT.
        WHEN OTHERS.   EXIT.
      ENDCASE.
      IF W_ZFDOCNO IS INITIAL.
        MESSAGE E322.
      ELSE.
        SELECT SINGLE * FROM ZTDHF1
                        WHERE ZFDHENO EQ W_ZFDOCNO.
        IF SY-SUBRC NE 0.
          MESSAGE E323 WITH W_ZFDOCNO.
        ENDIF.
      ENDIF.

      CASE SY-TCODE(4).
        WHEN 'ZIM0' OR 'ZIM1'.
          PERFORM P2000_LC_EDI_RCV_REPORT.
        WHEN 'ZIM4'.
          PERFORM P2000_INS_EDI_RCV_REPORT.
*            WHEN 'ZIML'.
        WHEN OTHERS.
          MESSAGE I299 WITH  ZTREQST-ZFDOCNOR.
          SUBMIT  ZRIMFLAT_DISP  WITH P_DDENO  EQ W_ZFDOCNO
                  AND RETURN.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0106  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0106 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0106-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0106  INPUT
*&---------------------------------------------------------------------*
*&      Module  MLCSG7G_GET_LINE_SCR0103  INPUT
*&---------------------------------------------------------------------*
MODULE MLCSG7G_GET_LINE_SCR0103 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0103_1-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " MLCSG7G_GET_LINE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  REQIT_GET_LINE_SCR0103  INPUT
*&---------------------------------------------------------------------*
MODULE REQIT_GET_LINE_SCR0103 INPUT.

  CLEAR : W_ZFITMNO.
  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  W_LINE0103 = TC_0103-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " REQIT_GET_LINE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0107  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0107 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0107-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0107  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0108  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0108 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0108-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0108  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0102 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'.
      LOOP AT IT_ZTREQORJ  WHERE ZFMARK NE SPACE.
        DELETE IT_ZTREQORJ INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'.
      LOOP AT IT_ZTREQORJ.
        IT_ZTREQORJ-ZFMARK = W_MARK.   MODIFY IT_ZTREQORJ.
      ENDLOOP.

    WHEN 'REF1'.
      REFRESH : IT_ZTREQORJ.
      LOOP AT IT_ZTREQORJ_ORG.
        MOVE-CORRESPONDING   IT_ZTREQORJ_ORG   TO   IT_ZTREQORJ.
        APPEND IT_ZTREQORJ.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.
*
*-----------------------------------------------------------------------
* ORIJIN CODE
*-----------------------------------------------------------------------
  PERFORM   P2000_ZTREQORJ_UPDATE.

  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

ENDMODULE.                 " USER_COMMAND_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0109_1  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0109_1 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  W_LINE_0109_1 = TC_0109_1-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0109_1  INPUT
*&---------------------------------------------------------------------*
*&      Module  LLCOF_GET_LINE_SCR0114  INPUT
*&---------------------------------------------------------------------*
MODULE LLCOF_GET_LINE_SCR0114 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0114-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " LLCOF_GET_LINE_SCR0114  INPUT
*&---------------------------------------------------------------------*
*&      Module  PURSG1_GET_LINE_SCR0123  INPUT
*&---------------------------------------------------------------------*
MODULE PURSG1_GET_LINE_SCR0123 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0123-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " PURSG1_GET_LINE_SCR0123  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0123  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0123 INPUT.

  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1' OR
     SY-UCOMM EQ 'MKA2' OR SY-UCOMM EQ 'DEL2' OR
     SY-UCOMM EQ 'MKA3' OR SY-UCOMM EQ 'DEL3'.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1' OR
         SY-UCOMM EQ 'MKL2' OR SY-UCOMM EQ 'UND2' OR  " 선택 해?
         SY-UCOMM EQ 'MKL3' OR SY-UCOMM EQ 'UND3'.    " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'. " 품목 삭제 / 취?
      IF OK-CODE EQ 'DEL1'.
        PERFORM  P2000_ITEM_DEL_CHECK.
      ENDIF.
      LOOP AT IT_ZSREQIT   WHERE ZFMARK NE SPACE.
        W_TABIX  =  SY-TABIX.
        DELETE IT_ZSPURSG1 WHERE ZFLSG1 = IT_ZSREQIT-ZFITMNO.
        DELETE IT_ZSREQIT INDEX W_TABIX.
      ENDLOOP.
      TC_0103-TOP_LINE = 1.
    WHEN 'DEL2' OR 'UND2'.   " 상품용역명세 삭제 / 취?
      LOOP AT IT_ZSPURSG1    WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        DELETE IT_ZSPURSG1G WHERE ZFLSG1 EQ IT_ZSPURSG1-ZFLSG1.
        DELETE IT_ZSPURSG1  INDEX W_TABIX.
      ENDLOOP.
    WHEN 'DEL3' OR 'UND3'.   " 상품용역명세 삭제 / 취?
      LOOP AT IT_ZSPURSG4    WHERE ZFMARK NE SPACE.
        DELETE IT_ZSPURSG4  INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'PORE'.
      LOOP  AT  IT_ZSREQIT.
        PERFORM   P2000_REFRESH_PRICE.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해?
      LOOP AT IT_ZSREQIT.
        IT_ZSREQIT-ZFMARK = W_MARK.   MODIFY IT_ZSREQIT.
      ENDLOOP.
    WHEN 'MKA2' OR 'MKL2'. " 전체선택 or 선택해?
      LOOP AT IT_ZSPURSG1.
        IT_ZSPURSG1-ZFMARK = W_MARK.   MODIFY IT_ZSPURSG1.
      ENDLOOP.
    WHEN 'MKA3' OR 'MKL3'. " 전체선택 or 선택해?
      LOOP AT IT_ZSPURSG4.
        IT_ZSPURSG4-ZFMARK = W_MARK.   MODIFY IT_ZSPURSG4.
      ENDLOOP.
    WHEN 'REF1'.           " 품목 Refresh
*>> 2001.07.04 NHJ 변경( PO ITEM 내역 REFRESH )
      PERFORM   P2000_REFRESH_ITEM.
*      PERFORM   P2000_REFRESH_RECORD_CHK.
*     REFRESH : IT_ZSREQIT.
*     LOOP AT IT_ZSREQIT_ORG.
*        MOVE-CORRESPONDING   IT_ZSREQIT_ORG   TO   IT_ZSREQIT.
*        APPEND IT_ZSREQIT.
*     ENDLOOP.

*      PERFORM   P2000_PURSG1_APPEND   USING ''.
    WHEN 'REF2'.           " 품목명세 Refresh
      REFRESH : IT_ZSPURSG1.
      LOOP AT IT_ZSPURSG1_ORG.
        MOVE-CORRESPONDING   IT_ZSPURSG1_ORG   TO   IT_ZSPURSG1.
        APPEND IT_ZSPURSG1.
      ENDLOOP.
    WHEN 'REF3'.           " 근거서류 Refresh
      REFRESH : IT_ZSPURSG4.
      LOOP AT IT_ZSPURSG4_ORG.
        MOVE-CORRESPONDING   IT_ZSPURSG4_ORG   TO   IT_ZSPURSG4.
        APPEND IT_ZSPURSG4.
      ENDLOOP.
    WHEN 'MKIT'.    " 선택 상품 명세 생성 버튼 선택시.
      PERFORM   P2000_PURSG1_APPEND   USING 'X'.
    WHEN 'MKDC'.    " 근거서류 생성 버튼 선택시.
      PERFORM   P2000_PURSG4_APPEND   USING 'X'.
    WHEN 'TEXT'.    " 근거서류 세부 버튼 선택시.
      W_COUNT = 0.
      LOOP AT IT_ZSPURSG4 WHERE ZFMARK NE SPACE.
        W_COUNT = W_COUNT + 1.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.          MESSAGE   E962.
        WHEN 1.          PERFORM   P2000_SCR0125_PROCESS.
        WHEN OTHERS.     MESSAGE   E965.
      ENDCASE.
    WHEN 'DETL'.    " 선택 상품 명세 세부 버튼 선택시.
      W_COUNT = 0.
      LOOP AT IT_ZSPURSG1 WHERE ZFMARK NE SPACE.
        W_COUNT = W_COUNT + 1.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.          MESSAGE   E962.
        WHEN 1.          PERFORM   P2000_SCR0123_PROCESS.
        WHEN OTHERS.     MESSAGE   E965.
      ENDCASE.

    WHEN OTHERS.
  ENDCASE.
* 근거서류 번호 INDEX UPDATE
  PERFORM  P3000_ZSPURSG4_INDEX_UPDATE.

*-----------------------------------------------------------------------
* 최종 개설금액 자동 갱?
*-----------------------------------------------------------------------
  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.


ENDMODULE.                 " USER_COMMAND_SCR0123  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0008  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0008 INPUT.

  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'ENTR'.
    WHEN 'CANC'.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.
      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.
      ANTWORT = 'N'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'TEXT'.
      CLEAR : ANTWORT.
      PERFORM   P2000_SCR0004_SHOW.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
  ENDCASE.


ENDMODULE.                 " GET_OK_CODE_SCR0008  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0004  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0004 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0004-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0004  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0004  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0004 INPUT.

  W_OK_CODE = SY-UCOMM.
* 미입력 삭?
  LOOP AT IT_ZSPURSG1G_SUB WHERE ZFGOSI EQ SPACE.
    DELETE IT_ZSPURSG1G_SUB INDEX SY-TABIX.
  ENDLOOP.

* INDEX 수정 작?
  LOOP AT IT_ZSPURSG1G_SUB.
    IT_ZSPURSG1G_SUB-ZFLSG1G  =   SY-TABIX * 10.
    MODIFY IT_ZSPURSG1G_SUB.
  ENDLOOP.

  CASE SY-UCOMM.
    WHEN 'CANC'.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.
      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
*      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.
      ANTWORT = 'N'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      ANTWORT = 'Y'.
  ENDCASE.

ENDMODULE.                 " GET_OK_CODE_SCR0004  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0125  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0125 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE_PURSG4 = TC_0125-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0125  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0125  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0125 INPUT.

  CASE OK-CODE.
    WHEN 'TEXT'.           " 근거서?
      PERFORM  P2000_SCR0125_PROCESS.
    WHEN 'CADD'.          "  주소 갱?
      PERFORM  P1000_SET_OPEN_BANK.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0125  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE READ_DOC_SCRCOM INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  W_COUNT = 1.
* 입력값 CHECK.
  IF ZSREQHD-EBELN IS INITIAL.           " P/O NO를 입력하지 않을 경?
    IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E063.
    ELSE.
* 수입의뢰 문서 READ PERFORM ?
      PERFORM   P1000_REQ_DOC_READ.
    ENDIF.
  ELSE.
*    IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
* P/O NO에 Count
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZVREQHD_ST
                      WHERE EBELN EQ ZSREQHD-EBELN
                      AND    ZFAMDNO EQ '00000'.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E064 WITH ZSREQHD-EBELN.
      WHEN 1.
        SELECT ZFREQNO INTO ZSREQHD-ZFREQNO UP TO 1 ROWS
                       FROM ZVREQHD_ST
                       WHERE EBELN EQ ZSREQHD-EBELN
                       AND   ZFAMDNO EQ '00'.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
*    ENDIF.
* 수입의뢰 문서 READ PERFORM ?
    PERFORM   P1000_REQ_DOC_READ.
  ENDIF.

  IF SY-DYNNR EQ '0500'.   ">추가내역 입력.
    SELECT SINGLE * FROM ZTIMIMG00.
    IF SY-SUBRC NE 0.
      MESSAGE E963.
    ENDIF.
    IF ZTIMIMG00-ZFBKYN EQ 'X'.
      IF ZTREQHD-ZFUSDAM GE ZTIMIMG00-ZFUSDAM.
*                ( ZTREQHD-ZFREQTY EQ 'LC' OR
*                  ZTREQHD-ZFREQTY EQ 'GS' ).
**                  ZTREQHD-ZFREQTY EQ 'LC' ).
        IF ZTREQHD-ZFOPBN IS INITIAL.
          MESSAGE E449.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " READ_DOC_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  IMPORT_DOC_DISPLAY_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE IMPORT_DOC_DISPLAY_SCRCOM INPUT.

  IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
    PERFORM  P2000_SET_REQ_SCR.
  ENDIF.

ENDMODULE.                 " IMPORT_DOC_DISPLAY_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_MASTER_LC
*&---------------------------------------------------------------------*
FORM P1000_READ_MASTER_LC.
  CALL FUNCTION 'ZIM_GET_MASTER_LC_DATA'
       EXPORTING
            ZFREQNO          = ZTREQHD-ZFREQNO
       IMPORTING
            W_ZTMLCHD        = ZTMLCHD
            W_ZTMLCSG2       = ZTMLCSG2
            W_ZTMLCSG910     = ZTMLCSG910
       TABLES
            IT_ZSMLCSG7G     = IT_ZSMLCSG7G
            IT_ZSMLCSG7O     = IT_ZSMLCSG7O
            IT_ZSMLCSG8E     = IT_ZSMLCSG8E
            IT_ZSMLCSG9O     = IT_ZSMLCSG9O
            IT_ZSMLCSG7G_ORG = IT_ZSMLCSG7G_ORG
            IT_ZSMLCSG7O_ORG = IT_ZSMLCSG7O_ORG
            IT_ZSMLCSG8E_ORG = IT_ZSMLCSG8E_ORG
            IT_ZSMLCSG9O_ORG = IT_ZSMLCSG9O_ORG
       EXCEPTIONS
            NOT_FOUND        = 4
            NOT_INPUT        = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E018 WITH ZTREQHD-ZFREQNO.
    WHEN 8.
      MESSAGE E019.
  ENDCASE.

*-----------------------------------------------------------------------
* 변경내역 확인을 위?
*-----------------------------------------------------------------------
   *ZTMLCHD    = ZTMLCHD.
   *ZTMLCSG2   = ZTMLCSG2.
   *ZTMLCSG910 = ZTMLCSG910.

*>> H/S CODE 기타 정보에 SET!
*      LOOP  AT  IT_ZSREQIT.
*         IF SY-TABIX EQ 1.
*            MOVE  IT_ZSREQIT-STAWN  TO  ZTMLCHD-ZFETC1.
*            EXIT.
*         ENDIF.
*      ENDLOOP.

*>> P/O NO 기타정보에 SET
  MOVE ZTREQHD-EBELN TO ZTMLCHD-ZFETC1.


ENDFORM.                    " P1000_READ_MASTER_LC
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR4100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR4100 INPUT.
  IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
    W_STATUS = C_REQ_C.
    CLEAR : ZTINS, ZTINSSG3, ZTINSRSP.
    CLEAR : W_ZFINSU1, W_ZFINSU2, W_ZFOPCD, W_ZFEDI.
* Import Config Select
    SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
    IF SY-SUBRC NE 0.   MESSAGE E961.   ENDIF.
    SELECT SINGLE * FROM ZTIMIMGTX
           WHERE    BUKRS   EQ   ZTREQHD-BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE E949 WITH ZTREQHD-BUKRS.
    ENDIF.

    SELECT SINGLE ZFCD5 ZFCD4
    INTO   (W_ZFOPCD, ZTINS-ZFEDI)
    FROM   ZTIMIMG08
    WHERE  ZFCDTY    EQ  '010'
    AND    ZFCD      EQ  ZTIMIMGTX-ZFINSC.

    PERFORM ALPHAFORMAT(SAPFF001) USING W_ZFOPCD ZTINS-ZFOPCD.

    " Insurance Company Code Get.
    IF ZTINS-ZFOPCD IS INITIAL.
       SELECT SINGLE * FROM ZTIMIMG11
       WHERE  BUKRS    EQ   ZTREQHD-BUKRS.
       MOVE  ZTIMIMG11-ZFINSCP   TO  ZTINS-ZFOPCD.
    ENDIF.

    SELECT SINGLE NAME1 NAME2
    INTO   (ZTINS-ZFINSU1, ZTINS-ZFINSU2)
    FROM   LFA1
    WHERE  LIFNR     EQ  ZTINS-ZFOPCD.

* DATA MOVE
    MOVE : ZTREQHD-ZFREQNO   TO ZTINS-ZFREQNO,   " Document No
           ZTREQHD-ZFTRANS   TO ZTINS-ZFTRANS,   " Transportation
           ZTREQHD-ZFINSYN   TO ZTINS-ZFINCD,    " Insurance Grade
           ZTREQHD-BUKRS     TO ZTINS-BUKRS,     " Company Code
           ZTIMIMGTX-ZFOPNO  TO ZTINS-ZFOPNO,    " Blanket Policy No
           ZTIMIMGTX-ZFELTXN  TO ZTINS-ZFELTXN,  " Business Regist. No
           ZTIMIMGTX-ZFELENM  TO ZTINS-ZFELENM,  " Firm Name
           ZTIMIMGTX-ZFREPRE  TO ZTINS-ZFREPRE,  " CEO's Name
           ZTIMIMGTX-ZFELEID  TO ZTINS-ZFELEID,  " Electronic Firm Name
           '3'               TO ZTINS-ZFNUCD,    " Number of copy
           '2'               TO ZTINS-ZFNUOD,    " Number of original
           'N'               TO ZTINS-ZFDOCST,   " Document Status
           'N'               TO ZTINS-ZFEDIST,   " EDI Status
           'X'               TO ZTINS-ZFEDICK,   " EDI CHECK
           ZTREQHD-ZFLASTAM  TO ZTINS-ZFIVAMT,   " Invoice Amount
           ZTREQHD-WAERS     TO ZTINS-WAERS,     " Cur.
           ZTREQHD-WAERS     TO ZTINS-ZFINAMTC,  " Primium Currency
           ZTREQHD-WAERS     TO ZTINSRSP-ZFTAMIC," Total Currency
           ZTREQHD-WAERS     TO ZTINSRSP-ZFCAMIC," Cargo Currency
           ZTREQHD-WAERS     TO ZTINSRSP-ZFDAMIC," Duty Currency
           ZTREQHD-ZFSHCU    TO ZTINSSG3-ZFSHCU, " Loading Port
           ZTREQHD-ZFARCU    TO ZTINSSG3-ZFARCU. " Arriving port

    IF  NOT ZTREQHD-EBELN IS INITIAL.
      MOVE:'POR'             TO ZTINS-ZFREDOC1,  " Reference Document
           ZTREQHD-EBELN     TO ZTINS-ZFREDON1.  " Reference Issuing No
    ENDIF.
    IF  NOT ZTREQST-ZFOPNNO IS INITIAL.
      MOVE:'LC'              TO ZTINS-ZFREDOC2,  " Reference Document2
          ZTREQST-ZFOPNNO    TO ZTINS-ZFREDON2.  " Reference Issuing No2
    ENDIF.

    MOVE :  '110'             TO ZTINS-ZFPEIV,    " Demanded Profit Rate
            SY-DATUM          TO ZTINS-ZFINSDT.   " Insurance Date
    " Local Currency Get.
    SELECT SINGLE * FROM T001 WHERE BUKRS EQ ZTREQHD-BUKRS.
    MOVE   T001-WAERS         TO :  ZTINS-ZFKRW,     ZTINSRSP-ZFTPRC,
                                    ZTINSRSP-ZFCPRC, ZTINSRSP-ZFDPRC,
                                    ZTINSRSP-ZFVPRC, ZTINSRSP-ZFIPRC.

    IF ZTINS-ZFKRW EQ ZTINS-WAERS.
       MOVE  :  1   TO  ZTINSRSP-ZFEXRT,
                1   TO  ZTINSRSP-FFACT.
    ENDIF.

* Loading Port
    CLEAR : T005T.
    SELECT SINGLE * FROM T005T WHERE   SPRAS EQ 'E'
                               AND     LAND1 EQ ZTINSSG3-ZFSHCU.
    MOVE : T005T-LANDX     TO   ZTINSSG3-ZFSHCUNM.
* Arriving Port
    CLEAR : T005T.
    SELECT SINGLE * FROM T005T WHERE   SPRAS EQ 'E'
                               AND     LAND1 EQ ZTINSSG3-ZFARCU.
    MOVE : T005T-LANDX     TO   ZTINSSG3-ZFARCUNM.  " 도착국가명.

* HS CODE & Goods Description
    REFRESH : IT_ZSINSSG2.
    LOOP AT IT_ZSREQIT.
      MOVE : IT_ZSREQIT-STAWN      TO ZTINS-ZFRSTAW.
      PERFORM ALPHAFORMAT(SAPFF001) USING W_ZFOPCD IT_ZSREQIT-MATNR.
      CONCATENATE  IT_ZSREQIT-TXZ01 '('  W_ZFOPCD  ')'
              INTO IT_ZSINSSG2-ZFDSOG1.
      COLLECT IT_ZSINSSG2.
    ENDLOOP.
    LOOP AT IT_ZSINSSG2.
      W_TABIX = SY-TABIX.
      IT_ZSINSSG2-ZFLSG2  =  SY-TABIX * 10.
      MODIFY IT_ZSINSSG2 INDEX W_TABIX.
    ENDLOOP.
* Condition of insuring
    PERFORM  P2000_TRANS_METHOD_SET.

    REFRESH : IT_ZSINSSG5.
    IT_ZSINSSG5-ZFLSG5 = '00010'.
    IT_ZSINSSG5-ZFINSC = ZTIMIMGTX-ZFINSC.
    IT_ZSINSSG5-ZFINSRT = '100'.

    APPEND IT_ZSINSSG5.                " APPEND
    W_ZFTRANS = ZTINS-ZFTRANS.

    SET SCREEN 4101.  LEAVE TO SCREEN 4101.    " SET SCREEN  -->
  ENDIF.
ENDMODULE.                 " USER_COMMAND_SCR4100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR4104  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR4104 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_4104-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR4104  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR4105_1  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR4105_1 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_4105_1-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR4105_1  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR4105  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR4105 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_4105-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR4105  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0190  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0190 INPUT.
  IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
    IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
      SET SCREEN 0.   LEAVE SCREEN.
    ENDIF.

    MOVE 'D' TO W_STATUS.

    TC_0191-TOP_LINE = 1.
    SET SCREEN 0191.  LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR0190  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0191  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0191 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0191-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0191  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0191_UPDATE_SCR0191  INPUT
*&---------------------------------------------------------------------*
MODULE TC_0191_UPDATE_SCR0191 INPUT.
  IF W_STATUS = 'D'.   EXIT.   ENDIF.
  READ TABLE IT_ZSRECST   INDEX TC_0191-CURRENT_LINE.

  W_OLD_SUBRC =  SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

  IF ZSRECST IS INITIAL. EXIT. ENDIF.
  IF ZSRECST-WAERS  IS INITIAL AND ZSRECST-ZFCSCD  IS INITIAL AND
     ZSRECST-ZFCAMT IS INITIAL AND ZSRECST-ZFCKAMT IS INITIAL AND
     ZSRECST-ZFVAT  IS INITIAL AND ZSRECST-ZFEXRT  IS INITIAL AND
     ZSRECST-ZFVEN  IS INITIAL AND ZSRECST-ZFPAY   IS INITIAL AND
     ZSRECST-ZFOCDT IS INITIAL AND ZSRECST-ZTERM   IS INITIAL AND
     ZSRECST-MWSKZ  IS INITIAL AND ZSRECST-ZFWERKS IS INITIAL.
    EXIT.
  ENDIF.
  MOVE 'KRW'  TO ZSRECST-ZFKRW.

  IF ZSRECST-ZFCSCD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSRECST' 'ZFCSCD'.
  ENDIF.
  IF ZSRECST-ZFCKAMT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSRECST' 'ZFCKAMT'.
  ENDIF.

  MOVE-CORRESPONDING ZSRECST TO IT_ZSRECST.

  CLEAR ZTIMIMG08.
  SELECT SINGLE * FROM ZTIMIMG08
                  WHERE ZFCDTY = '003'
                  AND   ZFCD   = IT_ZSRECST-ZFCSCD.
  IF SY-SUBRC NE 0.
    MESSAGE E818 WITH IT_ZSRECST-ZFCSCD.
  ENDIF.

  MOVE ZTIMIMG08-ZFCDNM TO IT_ZSRECST-ZFCDNM. "Code ?

  IF IT_ZSRECST-MWSKZ IS INITIAL.
*     IF ZTIMIMG08-ZFCD5 IS INITIAL.
*        MOVE 'V0'             TO IT_ZSRECST-MWSKZ. " Tax Code
*     ELSE.
    MOVE ZTIMIMG08-ZFCD5  TO IT_ZSRECST-MWSKZ. " Tax Code
*     ENDIF.
  ENDIF.

  IF IT_ZSRECST-ZFCKAMT IS INITIAL.
    IF IT_ZSRECST-WAERS EQ 'KRW'.
      IT_ZSRECST-ZFCKAMT = IT_ZSRECST-ZFCAMT.
    ELSE.
      IF NOT IT_ZSRECST-ZFCAMT IS INITIAL.
        IT_ZSRECST-ZFCKAMT = IT_ZSRECST-ZFCAMT * IT_ZSRECST-ZFEXRT.

        SELECT SINGLE * FROM  TCURX
                        WHERE  CURRKEY     = IT_ZSRECST-WAERS.
        IF SY-SUBRC NE 0.
          TCURX-CURRDEC = 2.
        ENDIF.
        IF TCURX-CURRDEC NE 0.
          PERFORM SET_CURR_CONV_TO_INTERNAL USING
                     IT_ZSRECST-ZFCKAMT IT_ZSRECST-ZFKRW.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
* mkim -- 발생일자를 오늘 날짜?
  IF IT_ZSRECST-ZFOCDT IS INITIAL.
    IT_ZSRECST-ZFOCDT = SY-DATUM.
  ENDIF.
*
  CASE IT_ZSRECST-ZFCSCD.
    WHEN '10' OR '2' OR '2AT'.   "> OPEN, AMEND, TERM CHARGE.
      IF IT_ZSRECST-ZFVEN IS INITIAL.
        IT_ZSRECST-ZFVEN = ZTREQHD-ZFOPBN.
      ENDIF.
  ENDCASE.

  CLEAR W_LFA1.
  CALL FUNCTION 'READ_LFA1'
       EXPORTING
            XLIFNR         = IT_ZSRECST-ZFVEN
       IMPORTING
            XLFA1          = W_LFA1
       EXCEPTIONS
            KEY_INCOMPLETE = 01
            NOT_AUTHORIZED = 02
            NOT_FOUND      = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE I025.
    WHEN 02.     MESSAGE E950.
    WHEN 03.     MESSAGE E025.
  ENDCASE.

  IF IT_ZSRECST-ZFPAY IS INITIAL.
    IT_ZSRECST-ZFPAY = W_LFA1-LNRZA.
  ENDIF.

  CLEAR W_LFA1.
  CALL FUNCTION 'READ_LFA1'
       EXPORTING
            XLIFNR         = IT_ZSRECST-ZFPAY
       IMPORTING
            XLFA1          = W_LFA1
       EXCEPTIONS
            KEY_INCOMPLETE = 01
            NOT_AUTHORIZED = 02
            NOT_FOUND      = 03.

  CASE SY-SUBRC.
*          WHEN 01.     MESSAGE I857.
    WHEN 01.     IT_ZSRECST-ZFPAY = IT_ZSRECST-ZFVEN.
    WHEN 02.     MESSAGE E950.
    WHEN 03.     MESSAGE E341.
  ENDCASE.

  IF IT_ZSRECST-ZFOCDT IS INITIAL.
    MOVE ZSREQHD-ZFOCDT TO IT_ZSRECST-ZFOCDT.
  ENDIF.

  IF IT_ZSRECST-ZTERM IS INITIAL.
    SELECT SINGLE ZTERM INTO IT_ZSRECST-ZTERM   " Payment Term
      FROM LFB1
     WHERE LIFNR EQ IT_ZSRECST-ZFVEN
       AND BUKRS EQ ZTREQHD-BUKRS.
    IF IT_ZSRECST-ZTERM  IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSRECST' 'ZTERM'.
    ENDIF.
  ENDIF.

  IF IT_ZSRECST-ZFWERKS IS INITIAL.
    MOVE ZTREQHD-ZFWERKS TO IT_ZSRECST-ZFWERKS.   " Plant
  ENDIF.

  CLEAR : TCURX.
  SELECT SINGLE * FROM  TCURX
                  WHERE  CURRKEY     = IT_ZSRECST-WAERS.
  IF SY-SUBRC NE 0.
    TCURX-CURRDEC = 2.
  ENDIF.
  IF TCURX-CURRDEC NE 0.
    PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSRECST-ZFCAMT
                                               IT_ZSRECST-WAERS.
  ENDIF.

  CLEAR : TCURX.
  SELECT SINGLE * FROM  TCURX
                  WHERE  CURRKEY     = IT_ZSRECST-ZFKRW.
  IF SY-SUBRC NE 0.
    TCURX-CURRDEC = 2.
  ENDIF.
  IF TCURX-CURRDEC NE 0.
    PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSRECST-ZFCKAMT
                                               IT_ZSRECST-ZFKRW.
  ENDIF.

*>> 비용배부 후에 금액 수정 CHECK!
  CLEAR W_TOTAMT.
  IF W_OLD_SUBRC EQ 0 AND IT_ZSRECST-ZFCSTYN = 'X'.
    W_TOTAMT = IT_ZSRECST-ZFUPCST.
    IF W_TOTAMT > 0.
      IF W_TOTAMT > IT_ZSRECST-ZFCKAMT.
        MESSAGE E471 WITH W_TABIX.
      ELSEIF W_TOTAMT < IT_ZSRECST-ZFCKAMT.
        MESSAGE W472 WITH W_TABIX.
      ENDIF.
    ENDIF.
  ENDIF.

  IF  W_OLD_SUBRC EQ 0.
    MODIFY IT_ZSRECST   INDEX W_TABIX.
  ELSE.
*     DESCRIBE TABLE IT_ZSRECST LINES W_LINE0103.
*     IT_ZSRECST-ZFCSQ = ( W_LINE0103 + 1 ) * 10.
    APPEND  IT_ZSRECST.
  ENDIF.

ENDMODULE.                 " TC_0191_UPDATE_SCR0191  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0191  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0191 INPUT.
  CLEAR W_CHK_CNT.

  CASE OK-CODE.
    WHEN 'CHDC'.               " Change document
      PERFORM   P2000_HEADER_CHANGE_DOC.
    WHEN 'ZIMG'.                    " Import IMG
      CALL TRANSACTION OK-CODE.
    WHEN 'DDLC'.                    " Double Click Event
      PERFORM  P2000_DDLC_PROCESS.
    WHEN 'EXIT' OR 'BACK'.     " EXIT or BACK
      IF W_STATUS EQ 'C' OR W_STATUS EQ 'I'.
        PERFORM P2000_COST_MODIFY_CHECK   USING   W_GUBUN.
        IF W_GUBUN EQ 'Y'.
          PERFORM P2000_SAVE_PROCESS.
          IF ANTWORT NE 'C'.
            PERFORM 2000_BACK_SCREEN_DEFINE.
          ENDIF.
        ELSE.
          PERFORM 2000_BACK_SCREEN_DEFINE.
        ENDIF.
      ELSE.
        CLEAR OK-CODE.
        PERFORM 2000_BACK_SCREEN_DEFINE.
      ENDIF.
    WHEN 'NEWL'.               " NEW ENTRY
      W_OLD_STATUS = W_STATUS.
      PERFORM P2000_ZTRECST_LOCK_MODE    USING    'L'.
      MOVE 'I' TO W_STATUS.
      PERFORM  P2000_IT_TAB_REFRESH.
    WHEN 'SAVE' OR 'ANZG'.     " SAVE or CHANGE=>DISPLAY
      CLEAR : ANTWORT.
      W_OK_CODE = OK-CODE.
      PERFORM P2000_COST_MODIFY_CHECK   USING   W_GUBUN.
      IF W_GUBUN EQ 'Y'.
        PERFORM P2000_SAVE_PROCESS.
      ELSE.
        IF OK-CODE EQ 'SAVE'.  MESSAGE I973.   ENDIF.
        IF W_OK_CODE EQ 'ANZG'.
          PERFORM P2000_ZTRECST_LOCK_MODE    USING    'U'.
          MOVE 'D' TO W_STATUS.
          PERFORM  P1000_DATA_REREAD.    " DATA READ
        ENDIF.
      ENDIF.
    WHEN 'DELT'.               " Delete
      PERFORM P2000_DATA_DELETE.
    WHEN 'DELE' OR 'DELC'.     " DELETE mark or unDELETE mark
      PERFORM P2000_SET_DEL_MARK.
    WHEN 'MKAL' OR 'MKLO'.     "  전체 선택 및 선택 해제?
      PERFORM P2000_SET_ROW_MARK.
    WHEN 'AEND'.         " DISPLAY ==> CHANGE
      PERFORM P2000_ZTRECST_LOCK_MODE    USING    'L'.
      MOVE 'C' TO W_STATUS.
      PERFORM  P1000_DATA_REREAD.
    WHEN 'SCHG'.                " 상태변경.
      LOOP AT IT_ZSRECST WHERE ZFMARK = 'X'.
        W_TABIX   = SY-TABIX.
        W_CHK_CNT = W_CHK_CNT + 1.
      ENDLOOP.
      IF W_CHK_CNT > 1. MESSAGE S965. EXIT. ENDIF.
      IF W_CHK_CNT = 0. MESSAGE S766. EXIT. ENDIF.

      MOVE-CORRESPONDING IT_ZSRECST TO ZSRECST.
      PERFORM P2000_DOC_CHANGE_OPEN.
      IF ANTWORT = 'Y'.
        MOVE ' ' TO ZSRECST-ZFACDO.
        MOVE ' ' TO ZSRECST-ZFFIYR.
        MOVE-CORRESPONDING ZSRECST TO IT_ZSRECST.
        MODIFY IT_ZSRECST INDEX W_TABIX.
      ENDIF.
    WHEN 'DOCDP'.               " 회계문서조회.
      LOOP AT IT_ZSRECST WHERE ZFMARK = 'X'.
        W_CHK_CNT = W_CHK_CNT + 1.
      ENDLOOP.
      IF W_CHK_CNT > 1. MESSAGE S965. EXIT. ENDIF.
      IF W_CHK_CNT = 0. MESSAGE S766. EXIT. ENDIF.

      PERFORM  P2000_FI_DOC_DISPLAY      USING IT_ZSRECST-ZFACDO
                                               IT_ZSRECST-BUKRS
                                               IT_ZSRECST-ZFFIYR.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0191  INPUT
*&---------------------------------------------------------------------*
*&      Module  IMPORT_DOC_DISPLAY_SCR0200  INPUT
*&---------------------------------------------------------------------*
MODULE IMPORT_DOC_DISPLAY_SCR0200 INPUT.

*   IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
  IF W_READ_CHK = 'Y'.
    W_STATUS = C_REQ_U.  W_READ_CHK = 'N'.
    SELECT SINGLE * FROM ZTIMIMG00.
    IF SY-SUBRC NE 0.
      MESSAGE S963.
      LEAVE TO SCREEN 200.
    ENDIF.

    IF ZTIMIMG00-ZFRELYN1 EQ 'X'.
      SET SCREEN 0151.  LEAVE TO SCREEN 0151.
    ELSE.
      PERFORM  P2000_SET_REQ_SCR.
    ENDIF.
  ENDIF.

ENDMODULE.                 " IMPORT_DOC_DISPLAY_SCR0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR2103  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR2103 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1' OR   " 전체 선?
     SY-UCOMM EQ 'MKA2' OR SY-UCOMM EQ 'DEL2'.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1' OR
         SY-UCOMM EQ 'MKL2' OR SY-UCOMM EQ 'UND2'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DETL'.           " 공급물품명?
      W_LINE = 0.
      LOOP AT IT_ZSOFFSG6  WHERE ZFMARK NE SPACE.
        W_LINE = W_LINE + 1.
        MOVE-CORRESPONDING    IT_ZSOFFSG6   TO   ZTOFFSG6.
      ENDLOOP.
      CASE W_LINE.
        WHEN 0.       MESSAGE   E962.
        WHEN 1.       PERFORM   P2000_SCR2103_PROCESS.
        WHEN OTHERS.  MESSAGE   E965.
      ENDCASE.
    WHEN 'DEL1' OR 'UND1'.       " 상품명세 삭?
      LOOP AT IT_ZSOFFSG6  WHERE ZFMARK NE SPACE.
        DELETE IT_ZSOFFSG6 INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'DEL2' OR 'UND2'.       " 원산지 삭?
      LOOP AT IT_ZSOFFO    WHERE ZFMARK NE SPACE.
        DELETE IT_ZSOFFO   INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'.       " 상품명세 전체선택 or 선택해?
      LOOP AT IT_ZSOFFSG6.
        IT_ZSOFFSG6-ZFMARK = W_MARK.  MODIFY IT_ZSOFFSG6.
      ENDLOOP.
    WHEN 'MKA2' OR 'MKL2'.       " 원산지 전체선택 or 선택해?
      LOOP AT IT_ZSOFFO.
        IT_ZSOFFO-ZFMARK = W_MARK.  MODIFY IT_ZSOFFO.
      ENDLOOP.
*>> PO 단가 반영(사후단가 결정인 경우).
    WHEN 'PORE'.
      LOOP  AT  IT_ZSREQIT.
        PERFORM   P2000_REFRESH_PRICE.
      ENDLOOP.
    WHEN 'REF2'.           " 원산지 Refresh
      REFRESH : IT_ZSOFFO.
      LOOP AT IT_ZSOFFO_ORG.
        MOVE-CORRESPONDING   IT_ZSOFFO_ORG   TO   IT_ZSOFFO.
        APPEND IT_ZSOFFO.
      ENDLOOP.
    WHEN 'REF1'.           " 상품명세 Refresh
      REFRESH : IT_ZSOFFSG6.
      LOOP AT IT_ZSOFFSG6_ORG.
        MOVE-CORRESPONDING   IT_ZSOFFSG6_ORG   TO   IT_ZSOFFSG6.
        APPEND IT_ZSOFFSG6.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.
*
*-----------------------------------------------------------------------
* ORIJIN CODE
*-----------------------------------------------------------------------
  PERFORM   P2000_ZSOFFO_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR2103  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0007  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0007 INPUT.

  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.
      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.
      ANTWORT = 'N'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'TEXT'.
      CLEAR : ANTWORT.
      PERFORM   P2000_SCR0006_SHOW.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                 " GET_OK_CODE_SCR0007  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0106  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0106 INPUT.

  READ TABLE IT_ZSMLCSG7O WITH KEY
                          ZFLSG7O = ZSMLCSG7O-ZFLSG7O  BINARY SEARCH.

  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING   ZSMLCSG7O TO IT_ZSMLCSG7O.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSMLCSG7O-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSMLCSG7O-ZFMARK.
    ENDIF.

    MODIFY IT_ZSMLCSG7O INDEX SY-TABIX.

  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_0106  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_BENIFICIARY_NAME  INPUT
*&---------------------------------------------------------------------*
MODULE GET_BENIFICIARY_NAME INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTREQHD-ZFBENI IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.
  ENDIF.

  PERFORM  P2000_REFRESH_ADDRESS     USING   ZTREQHD-ZFBENI.
* Benificiary
*  PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFBENI
*                              CHANGING   W_ZFBENI_NM.
*  CASE ZTREQHD-ZFREQTY.
*     WHEN 'PU'.
*        MOVE : ZTREQHD-ZFBENI     TO ZTPUR-ZFBENI.
*        IF ZTPUR-ZFVENNM1   IS INITIAL.
*           MOVE : W_LFA1-NAME1    TO   ZTPUR-ZFVENNM1,   " PULL NAME
*                  ''              TO   ZTPUR-ZFVENNM2,
*                  W_LFA1-BAHNS    TO   ZTPUR-ZFVENID.
*        ENDIF.
*        IF ZTPUR-ZFVENAD1   IS INITIAL.
*           MOVE : W_LFA1-NAME2    TO   ZTPUR-ZFVENAD1,   " PULL NAME
*                  W_LFA1-NAME3    TO   ZTPUR-ZFVENAD2,   " PULL NAME
*                  W_LFA1-NAME4    TO   ZTPUR-ZFVENAD3.   " PULL NAME
*        ENDIF.
*     WHEN 'LC'.
*
*        MOVE : ZTREQHD-ZFBENI     TO ZTMLCHD-ZFBENI.
*        IF ZTMLCSG2-ZFBENI1 IS INITIAL.
*           CLEAR : LFBK.
*           SELECT * FROM  LFBK UP TO 1 ROWS
*                    WHERE LIFNR EQ ZTREQHD-ZFBENI.
*                   AND   BANKS EQ 'KR'.
*              EXIT.
*           ENDSELECT.
*           TRANSLATE LFBK TO UPPER CASE.
*           MOVE : W_LFA1-NAME1    TO   ZTMLCSG2-ZFBENI1,   " PULL NAME
*                  W_LFA1-NAME2    TO   ZTMLCSG2-ZFBENI2,
*                  W_LFA1-NAME3    TO   ZTMLCSG2-ZFBENI3,
*                  W_LFA1-NAME4    TO   ZTMLCSG2-ZFBENI4,
*                 LFBK-BANKN      TO   ZTMLCSG2-ZFBENIA.   " ACCOUNT NO.
*        ENDIF.
*     WHEN 'LO'.
*        MOVE : ZTREQHD-ZFBENI     TO ZTLLCHD-ZFBENI.
*        IF ZTLLCSG23-ZFBENI1 IS INITIAL.
*          MOVE : W_LFA1-NAME1       TO ZTLLCSG23-ZFBENI1,   " PULL NAME
*                  W_LFA1-J_1KFREPRE  TO ZTLLCSG23-ZFBENI2,
*                 W_LFA1-NAME2       TO ZTLLCSG23-ZFBENI3.
*       ENDIF.
*    WHEN 'TT'.
*       MOVE : ZTREQHD-ZFBENI     TO ZTTTHD-ZFBENI.
*       IF ZTTTHD-ZFBENI1 IS INITIAL.
*          MOVE : W_LFA1-NAME1        TO ZTTTHD-ZFBENI1,  " PULL NAME
*                 W_LFA1-NAME2        TO ZTTTHD-ZFBENI2,  " Benificiary
*                 W_LFA1-NAME3        TO ZTTTHD-ZFBENI3.  " Benificiary
*       ENDIF.
* ENDCASE.

ENDMODULE.                 " GET_BENIFICIARY_NAME  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_RENT_VENDOR_NAME  INPUT
*&---------------------------------------------------------------------*
MODULE GET_RENT_VENDOR_NAME INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* 차입기관 NAME1 GET
  PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFLEVN
                              CHANGING   W_ZFLEVN_NM.

ENDMODULE.                 " GET_RENT_VENDOR_NAME  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_FOWARDER_NAME  INPUT
*&---------------------------------------------------------------------*
MODULE GET_FOWARDER_NAME INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* FOWARDER NAME1 GET
* PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFFORD
*                             CHANGING   W_ZFFORD_NM.

ENDMODULE.                 " GET_FOWARDER_NAME  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSREQIT_UPDATE  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSREQIT_UPDATE INPUT.
* Display MODE MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZSREQIT WITH KEY ZFITMNO = ZSREQIT-ZFITMNO.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE : ZSREQIT-STAWN  TO  IT_ZSREQIT-STAWN,
         ZSREQIT-MENGE  TO  IT_ZSREQIT-MENGE.

*-----------------------------------------------------------------------
* ITEM
*-----------------------------------------------------------------------
  PERFORM   P2000_REQ_ITEM_CHECK   TABLES  IT_ZSREQIT
                                   USING   'I'.

  IF IT_ZSREQIT-KPEIN EQ 0.
    IT_ZSREQIT-KWERT = 0.
  ELSE.
    IT_ZSREQIT-KWERT = ( IT_ZSREQIT-KBETR / IT_ZSREQIT-KPEIN ) *
                         IT_ZSREQIT-MENGE.
  ENDIF.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSREQIT   INDEX W_TABIX.
    IF ZTREQHD-ZFREQTY EQ 'PU'.
      PERFORM   P3000_PURSG1_MODIFY_COM.
    ENDIF.
  ELSE.
    APPEND IT_ZSREQIT.
    IF ZTREQHD-ZFREQTY EQ 'PU'.
      PERFORM   P3000_PURSG1_APPEND_COM.
    ENDIF.
  ENDIF.

ENDMODULE.                 " IT_ZSREQIT_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0103  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0103 INPUT.

  IF W_ZFITMNO  LE  IT_ZSREQIT-ZFITMNO.
    MOVE  IT_ZSREQIT-ZFITMNO  TO  W_ZFITMNO.
  ENDIF.

  READ TABLE IT_ZSREQIT  WITH KEY ZFITMNO = ZSREQIT-ZFITMNO
                         BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSREQIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSREQIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSREQIT INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0153  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0153 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'. " 품목 삭?
      IF OK-CODE EQ 'DEL1'  AND SY-TCODE NE 'ZIM01'.
        PERFORM  P2000_ITEM_DEL_CHECK.
      ENDIF.
      LOOP AT IT_ZSREQIT   WHERE ZFMARK NE SPACE.
        DELETE IT_ZSREQIT   INDEX SY-TABIX.
      ENDLOOP.
      TC_0103-TOP_LINE = 1.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해?
      LOOP AT IT_ZSREQIT.
        IT_ZSREQIT-ZFMARK = W_MARK.   MODIFY IT_ZSREQIT.
      ENDLOOP.
    WHEN 'PORE'.
      LOOP  AT  IT_ZSREQIT.
        PERFORM   P2000_REFRESH_PRICE.
      ENDLOOP.
    WHEN 'REF1'.           " 품목 Refresh
*>> 2001.07.04 NHJ 변경( PO ITEM 내역 REFRESH )
      PERFORM   P2000_REFRESH_ITEM.
*      PERFORM   P2000_REFRESH_RECORD_CHK.
*     REFRESH : IT_ZSREQIT.
*     LOOP AT IT_ZSREQIT_ORG.
*        MOVE-CORRESPONDING   IT_ZSREQIT_ORG   TO   IT_ZSREQIT.
*        APPEND IT_ZSREQIT.
*     ENDLOOP.
    WHEN OTHERS.
  ENDCASE.
*-----------------------------------------------------------------------
* 최종 개설금액 자동 갱신.
*-----------------------------------------------------------------------
  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

ENDMODULE.                 " USER_COMMAND_SCR0153  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0106  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0106 INPUT.

  CASE OK-CODE.
    WHEN 'DEL1'.           " 원산지 삭?
      LOOP AT IT_ZSMLCSG7O WHERE ZFMARK NE SPACE.
        READ TABLE IT_ZSMLCSG7O
                          WITH KEY IT_ZSMLCSG7O(05)  BINARY SEARCH.
        DELETE IT_ZSMLCSG7O INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 원산지 전체선택 or 원산지 선택해?
      IF SY-UCOMM EQ 'MKA1'.      " 전체 선?
        W_MARK = 'X'.
      ELSEIF SY-UCOMM EQ 'MKL1'.  " 선택 해?
        CLEAR : W_MARK.
      ENDIF.
      LOOP AT IT_ZSMLCSG7O.
        IT_ZSMLCSG7O-ZFMARK = W_MARK.   MODIFY IT_ZSMLCSG7O.
      ENDLOOP.

    WHEN 'REF1'.           " 원산지 Refresh
      REFRESH : IT_ZSMLCSG7O.
      LOOP AT IT_ZSMLCSG7O_ORG.
        MOVE-CORRESPONDING   IT_ZSMLCSG7O_ORG   TO   IT_ZSMLCSG7O.
        APPEND IT_ZSMLCSG7O.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0106  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZTREQORJ_UPDATE  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZTREQORJ_UPDATE INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZTREQORJ  WITH KEY ZFLSG7O = ZSMLCSG7O-ZFLSG7O.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  SELECT SINGLE LANDX INTO    ZSMLCSG7O-ZFORNM FROM T005T
*                              WHERE SPRAS  EQ SY-LANGU
                              WHERE SPRAS  EQ 'E'
                              AND   LAND1  EQ ZSMLCSG7O-ZFORIG.

  MOVE-CORRESPONDING ZSMLCSG7O TO IT_ZTREQORJ.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZTREQORJ INDEX W_TABIX.
  ELSE.
    IT_ZTREQORJ-ZFLSG7O  = TC_0106-CURRENT_LINE * 10.
    APPEND IT_ZTREQORJ.
  ENDIF.

ENDMODULE.                 " IT_ZTREQORJ_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_REQ_DATA_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE READ_REQ_DATA_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC' OR 'NO'.
      EXIT.
  ENDCASE.

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

*-----------------------------------------------------------------------
* 기존 문서 READ
*-----------------------------------------------------------------------
  PERFORM   P1000_PRE_REQ_DOC_READ.

ENDMODULE.                 " READ_REQ_DATA_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCR0002  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCR0002 INPUT.
  ANTWORT = 'N'.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " USER_EXIT_SCR0002  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0010  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0010 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0010-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0010 INPUT.

  CASE OK-CODE.
    WHEN 'DDCL'.
      IF TC_0010-TOP_LINE > LINE.    EXIT.   ENDIF.
      READ TABLE IT_ZSREQHD INDEX LINE.
      IT_ZSREQHD-ZFMARK = 'X'.    MODIFY   IT_ZSREQHD   INDEX  LINE.
      ANTWORT = 'Y'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN OTHERS.   EXIT.
  ENDCASE.

  IF ANTWORT EQ 'Y'.
    W_SEL_MAT_CNT = 0.
    LOOP AT IT_ZSREQHD WHERE ZFMARK EQ 'X'.
      W_SEL_MAT_CNT = W_SEL_MAT_CNT + 1.
      MOVE-CORRESPONDING  IT_ZSREQHD  TO  W_ZSREQHD.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      MESSAGE I962.
    ELSE.
      IF W_SEL_MAT_CNT NE 1.
        MESSAGE I965.
      ELSE.
        ZSREQHD-ZFREQNO = W_ZSREQHD-ZFREQNO.
        ZSREQHD-ZFAMDNO = W_ZSREQHD-ZFAMDNO.
        SET SCREEN 0.   LEAVE SCREEN.
      ENDIF.
    ENDIF.
  ELSE.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.
ENDMODULE.                 " GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0010  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0010 INPUT.

  READ TABLE IT_ZSREQHD   WITH KEY ZSREQHD(18)  BINARY SEARCH.

  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING   ZSREQHD   TO IT_ZSREQHD.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSREQHD-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSREQHD-ZFMARK.
    ENDIF.
*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal )
*-----------------------------------------------------------------------
*>>> 금?
    PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSREQHD-ZFLASTAM
                                               IT_ZSREQHD-WAERS.

    MODIFY IT_ZSREQHD INDEX SY-TABIX.

  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PAYMENT_TERM  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_PAYMENT_TERM INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  IF ZTREQHD-ZFLCKN IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFLCKN'.
  ENDIF.
*-----------------------------------------------------------------------
* 수입의뢰 가능 P/O 검증 ( ZTIMIMG01 )
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_GET_ZTIMIMG01'
         EXPORTING
*              KTOKK        =  LFA1-KTOKK
               BSART        =  EKKO-BSART
               BSTYP        =  EKKO-BSTYP
               ZTERM        =  ZTREQHD-ZTERM
         IMPORTING
               W_ZFREQTY    = ZSREQHD-ZFREQTY
               W_ZTIMIMG01  = ZTIMIMG01
         EXCEPTIONS
               NOT_OK            =   1
               NOT_FOUND         =   2.

  CASE SY-SUBRC.
    WHEN 1.    MESSAGE E004     WITH ZSREQHD-EBELN EKKO-ZTERM.
    WHEN 2.    MESSAGE E958     WITH ZTREQHD-ZTERM.
    WHEN OTHERS.
*>> PAYMENT TERM 과 L/C 종류 CHECK!
      PERFORM LC_KIND_CHECK.

      IF W_ZFREQTY NE ZTIMIMG01-ZFREQTY.
        MESSAGE E065 WITH W_ZFREQTY ZTIMIMG01-ZFREQTY.
      ELSE.
        ZTREQHD-ZFBACD = ZTIMIMG01-ZFBACD.
        CASE ZTREQHD-ZFREQTY.
          WHEN 'LC'.
            ZTMLCHD-ZFTRMB = ZTIMIMG01-ZFTRMB.
            ZTMLCHD-ZFUSPR = ZTIMIMG01-ZFUSPR.
            ZTREQHD-ZFBACD = ZTIMIMG01-ZFBACD.
            ZTREQHD-ZFPREPAY = ZTIMIMG01-ZFPREPAY.   " 선급금 비율.
            ZTMLCHD-ZFTRTX1 = ZTIMIMG01-ZFTRTX1.
            ZTMLCHD-ZFTRTX2 = ZTIMIMG01-ZFTRTX2.
            ZTMLCHD-ZFTRTX3 = ZTIMIMG01-ZFTRTX3.
            ZTMLCHD-ZFTRTX4 = ZTIMIMG01-ZFTRTX4.
            ZTMLCHD-ZFUSAT  = ZTIMIMG01-ZFUSAT.
            CASE ZTIMIMG01-ZFUSAT.
              WHEN 'UB'.
                ZTMLCHD-ZFPAGR = '2AA'.
              WHEN 'US'.
                ZTMLCHD-ZFPAGR = '2AB'.
            ENDCASE.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " CHECK_PAYMENT_TERM  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OPEN_BANK_NAME  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OPEN_BANK_NAME INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CLEAR : W_OPEN_NM.
  IF ZTREQHD-ZFOPBN IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.

* 개설은행 NAME1 GET
*  PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFOPBN
*                              CHANGING   W_OPEN_NM.
* 개설은?
  PERFORM  P1000_GET_BANK     USING      ZTREQHD-ZFOPBN
                              CHANGING   W_LFA1.

  MOVE W_LFA1-NAME1 TO W_OPEN_NM.
*<< 은행배정 IMG CHECK >>*
  IF NOT ZTIMIMG00-ZFUSDAM IS INITIAL.
     IF ZTIMIMG00-ZFBKYN EQ 'X'
        AND ZTREQHD-ZFUSDAM GE ZTIMIMG00-ZFUSDAM.
        EXIT.
     ENDIF.
  ENDIF.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC' OR 'LO' OR 'PU' OR 'TT' OR 'DA' OR 'DP' OR 'GS'.
      IF ZTREQHD-ZFOPBN IS INITIAL.
        MESSAGE E167 WITH 'Opening bank'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  PERFORM  P2000_BANK_TEXT_MOVE.

  IF ZTREQHD-ZFREQTY EQ 'LC'.
*> 회사코드 정보 GET..(2001.08.21 KSB INSERT)
    PERFORM  P1000_READ_COMPANY_DATA  USING ZTREQHD-BUKRS
                                            ZTREQHD-IMTRD.

    PERFORM  P2000_SET_SHIPING_TEXT.
  ENDIF.

ENDMODULE.                 " GET_OPEN_BANK_NAME  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZTREQORJ_UPDATE_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZTREQORJ_UPDATE_SCR0102 INPUT.
* if display mode, exit.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZTREQORJ  WITH KEY ZFLSG7O = ZSMLCSG7O-ZFLSG7O.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  SELECT SINGLE LANDX INTO    ZSMLCSG7O-ZFORNM FROM T005T
                              WHERE SPRAS  EQ 'E'
                              AND   LAND1  EQ ZSMLCSG7O-ZFORIG.
  MOVE: ZSMLCSG7O-ZFORNM TO W_TEXT70.

  TRANSLATE W_TEXT70 TO UPPER CASE.
  ZSMLCSG7O-ZFORNM = W_TEXT70.
  MOVE-CORRESPONDING ZSMLCSG7O TO IT_ZTREQORJ.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZTREQORJ INDEX W_TABIX.
  ELSE.
    IT_ZTREQORJ-ZFLSG7O = TC_0102-CURRENT_LINE * 10.
    APPEND IT_ZTREQORJ.
  ENDIF.

ENDMODULE.                 " IT_ZTREQORJ_UPDATE_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCR0300  INPUT
*&---------------------------------------------------------------------*
MODULE READ_DOC_SCR0300 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZSREQHD-EBELN IS INITIAL AND     " P/O NO를 입력하지 않을 경?
     ZSREQHD-ZFREQNO IS INITIAL AND   " 관리번호가 입력하지 않을 경?
     ZSREQHD-ZFOPNNO IS INITIAL.      " 문서번호가 입력하지 않을 경?
    MESSAGE E066.
  ENDIF.
*
  IF NOT ZSREQHD-ZFOPNNO IS INITIAL.  " 문서번호가 입력된 경?
*    IF ZSREQHD-ZFREQNO IS INITIAL.
* 문서 승인번?
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZTREQST
                      WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO
                      AND   ZFAMDNO EQ '00000'.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E067 WITH ZSREQHD-ZFOPNNO.
      WHEN 1.
        SELECT ZFREQNO INTO ZSREQHD-ZFREQNO UP TO 1 ROWS
                       FROM ZTREQST
                       WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO
                       AND   ZFAMDNO EQ '00000'.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_OPEN_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
*    ENDIF.
* 수입의뢰 문서 READ PERFORM ?
    PERFORM   P1000_REQ_DOC_READ.
    PERFORM   P1000_READ_ZTRECST.    "
    EXIT.
  ENDIF.

*  IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
  IF NOT ZSREQHD-EBELN IS INITIAL.      " 관리번호가 입력하지 않을 경?
* P/O NO에 Count
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZTREQHD
                      WHERE EBELN EQ ZSREQHD-EBELN.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E064 WITH ZSREQHD-EBELN.
      WHEN 1.
        SELECT ZFREQNO INTO ZSREQHD-ZFREQNO UP TO 1 ROWS
                       FROM ZTREQHD
                       WHERE EBELN EQ ZSREQHD-EBELN.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* 수입의뢰 문서 READ PERFORM ?
    PERFORM   P1000_REQ_DOC_READ.
    PERFORM   P1000_READ_ZTRECST.    "
    EXIT.
  ENDIF.
* 수입의뢰 문서 READ PERFORM ?
  PERFORM  P1000_REQ_DOC_READ.
  PERFORM  P1000_READ_ZTRECST.    "

ENDMODULE.                 " READ_DOC_SCR0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0011  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0011 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0011-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0011  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0103  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0103 INPUT.

  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1' OR
     SY-UCOMM EQ 'MKA2' OR SY-UCOMM EQ 'DEL2'.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1' OR
         SY-UCOMM EQ 'MKL2' OR SY-UCOMM EQ 'UND2'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'.
      IF OK-CODE EQ 'DEL1' AND SY-TCODE NE 'ZIM01'.
        PERFORM  P2000_ITEM_DEL_CHECK.
      ENDIF.
      LOOP AT IT_ZSREQIT   WHERE ZFMARK NE SPACE.
        DELETE IT_ZSREQIT   INDEX SY-TABIX.
      ENDLOOP.
      TC_0103-TOP_LINE = 1.
    WHEN 'INS2'.             " LINE INSET.
      W_COUNT = 0.
      LOOP AT IT_ZSMLCSG7G   WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        ADD   1    TO    W_COUNT.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.         MESSAGE S962.   EXIT.
        WHEN 1.
        WHEN OTHERS.    MESSAGE S965.   EXIT.
      ENDCASE.
      CLEAR : IT_ZSMLCSG7G.
      IT_ZSMLCSG7G-ZFLSG7G = W_TABIX * 10.
      INSERT  IT_ZSMLCSG7G  INDEX  W_TABIX.

    WHEN 'DEL2' OR 'UND2'.
      LOOP AT IT_ZSMLCSG7G   WHERE ZFMARK NE SPACE.
        DELETE IT_ZSMLCSG7G INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'.
      LOOP AT IT_ZSREQIT.
        IT_ZSREQIT-ZFMARK = W_MARK.   MODIFY IT_ZSREQIT.
      ENDLOOP.
    WHEN 'MKA2' OR 'MKL2'.
      LOOP AT IT_ZSMLCSG7G.
        IT_ZSMLCSG7G-ZFMARK = W_MARK.   MODIFY IT_ZSMLCSG7G.
      ENDLOOP.
    WHEN 'PORE'.
      LOOP  AT  IT_ZSREQIT.
        PERFORM   P2000_REFRESH_PRICE.
      ENDLOOP.
    WHEN 'REF1'.
      PERFORM   P2000_REFRESH_ITEM.
    WHEN 'SU01'.
      W_COUNT = 0.
      LOOP AT IT_ZSREQIT  WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        ADD   1    TO    W_COUNT.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.         MESSAGE S962.   EXIT.
        WHEN 1.
         PERFORM P2000_REQUESTER_DISPLAY USING IT_ZSREQIT-EBELN
                                               IT_ZSREQIT-EBELP.
        WHEN OTHERS.    MESSAGE S965.   EXIT.
      ENDCASE.
    WHEN 'PODP'.
      W_COUNT = 0.
      LOOP AT IT_ZSREQIT  WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        ADD   1    TO    W_COUNT.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.         MESSAGE S962.   EXIT.
        WHEN 1.
            MOVE-CORRESPONDING  IT_ZSREQIT  TO  W_ZSREQIT.
            PERFORM P2000_PO_ITEM_DISPLAY.
        WHEN OTHERS.    MESSAGE S965.   EXIT.
      ENDCASE.

    WHEN 'REF2'.
      REFRESH : IT_ZSMLCSG7G.
      LOOP AT IT_ZSMLCSG7G_ORG.
        CLEAR : IT_ZSMLCSG7G.
        MOVE-CORRESPONDING   IT_ZSMLCSG7G_ORG   TO   IT_ZSMLCSG7G.
        APPEND IT_ZSMLCSG7G.
      ENDLOOP.
    WHEN 'MKIT'.
      PERFORM  P3000_GOOD_DESC_CREATE.
    WHEN OTHERS.
  ENDCASE.

  PERFORM   P2000_IT_ZSMLCSG7G_UPDATE.
  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

ENDMODULE.                 " USER_COMMAND_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZSMLCSG7G_UPDATE_SCR0103  INPUT
*&---------------------------------------------------------------------*
MODULE ZSMLCSG7G_UPDATE_SCR0103 INPUT.

  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZSMLCSG7G WITH KEY ZFLSG7G = ZSMLCSG7G-ZFLSG7G.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSMLCSG7G TO IT_ZSMLCSG7G.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSMLCSG7G INDEX W_TABIX.
  ELSE.
    IT_ZSMLCSG7G-ZFLSG7G = TC_0103_1-CURRENT_LINE * 10.
    APPEND IT_ZSMLCSG7G.
  ENDIF.

ENDMODULE.                 " ZSMLCSG7G_UPDATE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0103_MARK_TC_0103_1  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0103_MARK_TC_0103_1 INPUT.

  READ TABLE IT_ZSMLCSG7G WITH KEY ZSMLCSG7G(5)  BINARY SEARCH.

  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING   ZSMLCSG7G TO IT_ZSMLCSG7G.
    IF NOT ( W_ROW_MARK1 IS INITIAL ).
      IT_ZSMLCSG7G-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSMLCSG7G-ZFMARK.
    ENDIF.
    MODIFY IT_ZSMLCSG7G INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0103_MARK_TC_0103_1  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_BANK_CHARGE  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_BANK_CHARGE INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  PERFORM  P2000_BANK_CHARGE_MOVE.
ENDMODULE.                 " CHECK_BANK_CHARGE  INPUT
*&---------------------------------------------------------------------*
*&      Module  PARTIAL_SHIPMENT_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE PARTIAL_SHIPMENT_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  PERFORM  P2000_PARTIAL_SHIPMENT_MOVE.

ENDMODULE.                 " PARTIAL_SHIPMENT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  TRANS_SHIPMENT_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE TRANS_SHIPMENT_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  PERFORM  P2000_TRANS_SHIPMENT_MOVE.

ENDMODULE.                 " TRANS_SHIPMENT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  SHIP_PORT_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE SHIP_PORT_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      MOVE : ZTREQHD-ZFSPRT     TO ZTMLCHD-ZFSPRT.   " 선적국.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " SHIP_PORT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  TRANS_PORT_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE TRANS_PORT_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      MOVE : ZTREQHD-ZFAPRT     TO ZTMLCHD-ZFAPRT.   " 도착?
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " TRANS_PORT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXPIRY_DATE_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE EXPIRY_DATE_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* MKIM 2001.05.14
  CASE ZTREQHD-ZFREQTY.
*     WHEN 'LC'.
*        IF ZTREQHD-ZFREQSD IS INITIAL.
*           PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFREQSD'.
*        ENDIF.
    WHEN OTHERS.
  ENDCASE.

  CASE ZTREQHD-ZFREQTY.
*     WHEN  OTHERS.
*>> 2001.06.12(NHJ 추가)
    WHEN 'LO' OR 'LC'.
*     WHEN 'LC'.
      IF ZTREQHD-ZFREQSD IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFREQSD'.
      ENDIF.
      IF ZTREQHD-ZFREQED IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFREQED'.
      ENDIF.
*     WHEN OTHERS.
  ENDCASE.

  PERFORM   P2000_EXPIRY_DATE_CHECK.

ENDMODULE.                 " EXPIRY_DATE_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INSURANCE_INCOTERM  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INSURANCE_INCOTERM INPUT.

*-----------------------------------------------------------------------
* 최종 개설금액 자동 갱?
*-----------------------------------------------------------------------
*  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

  IF ZTIMIMG00-ZFINMT EQ '1'.
     PERFORM   P2000_INSURANCE_BIT_SET.
  ENDIF.

ENDMODULE.                 " CHECK_INSURANCE_INCOTERM  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0102 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0102-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZSMLCSG8E_UPDATE_SCR0107  INPUT
*&---------------------------------------------------------------------*
MODULE ZSMLCSG8E_UPDATE_SCR0107 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZSMLCSG8E WITH KEY ZFLSG8E = ZSMLCSG8E-ZFLSG8E.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSMLCSG8E TO IT_ZSMLCSG8E.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSMLCSG8E INDEX W_TABIX.
  ELSE.
    IT_ZSMLCSG8E-ZFLSG8E = TC_0107-CURRENT_LINE * 10.
    APPEND IT_ZSMLCSG8E.
  ENDIF.

ENDMODULE.                 " ZSMLCSG8E_UPDATE_SCR0107  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0107  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0107 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'.       " 기타 부가조건 삭제 / 취?
      LOOP AT IT_ZSMLCSG8E  WHERE ZFMARK NE SPACE.
        READ TABLE IT_ZSMLCSG8E WITH KEY
                      ZFLSG8E = IT_ZSMLCSG8E-ZFLSG8E  BINARY SEARCH.
        IT_ZSMLCSG8E-LOEKZ = W_MARK.
        MODIFY IT_ZSMLCSG8E INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'.       " 전체선택 or 선택해?
      LOOP AT IT_ZSMLCSG8E.
        IT_ZSMLCSG8E-ZFMARK = W_MARK.   MODIFY IT_ZSMLCSG8E.
      ENDLOOP.
    WHEN 'REF1'.                 " Refresh
      REFRESH : IT_ZSMLCSG8E.
      LOOP AT IT_ZSMLCSG8E_ORG.
        MOVE-CORRESPONDING   IT_ZSMLCSG8E_ORG   TO   IT_ZSMLCSG8E.
        APPEND IT_ZSMLCSG8E.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

*-----------------------------------------------------------------------
* 인도조건 (파트 1)이 'CIF' 나 'CIP'일 경우 --> 보험조건 반드시 입?
*-----------------------------------------------------------------------
  PERFORM  P2000_CIF_COB_CHECK.
*-----------------------------------------------------------------------
* 기타 부가 조?
*-----------------------------------------------------------------------
  PERFORM   P2000_IT_ZSMLCSG8E_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR0107  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSMLCSG9O_UPDATE_SCR0108  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSMLCSG9O_UPDATE_SCR0108 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
*-----------------------------------------------------------------------
* check box 검?
*-----------------------------------------------------------------------
  IF ZTMLCHD-ZFADCD5 IS INITIAL.
*    MESSAGE I114.     ZTMLCHD-ZFADCD5 = 'X'.
    ZTMLCHD-ZFADCD5 = 'X'.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZSMLCSG9O  WITH KEY ZFLSG9O = ZSMLCSG9O-ZFLSG9O.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF NOT ZSMLCSG9O-ZFCD IS INITIAL AND ZSMLCSG9O-ZFODOC1 IS INITIAL.

    CALL FUNCTION 'ZIM_GET_ZTIMIMG08_RECORD'
         EXPORTING
              W_ZFCDTY    = '999'
              W_ZFCD      = ZSMLCSG9O-ZFCD
         IMPORTING
              W_ZTIMIMG08 = W_ZTIMIMG08
         EXCEPTIONS
              NOT_FOUND   = 4
              NOT_INPUT   = 8.

    CASE SY-SUBRC.
      WHEN 4.    MESSAGE E112 WITH   ZSMLCSG9O-ZFCD.
      WHEN 0.    ZSMLCSG9O-ZFODOC1 = W_ZTIMIMG08-ZFCDNM.
    ENDCASE.
  ENDIF.

  MOVE-CORRESPONDING ZSMLCSG9O TO IT_ZSMLCSG9O.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSMLCSG9O INDEX W_TABIX.
  ELSE.
    IT_ZSMLCSG9O-ZFLSG9O = TC_0108-CURRENT_LINE * 10.
    APPEND IT_ZSMLCSG9O.
  ENDIF.

ENDMODULE.                 " IT_ZSMLCSG9O_UPDATE_SCR0108  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0108_MARK_TC_0106  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0108_MARK_TC_0106 INPUT.

  READ TABLE IT_ZSMLCSG9O WITH KEY ZSMLCSG9O(5)  BINARY SEARCH.

  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING   ZSMLCSG9O TO IT_ZSMLCSG9O.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSMLCSG9O-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSMLCSG9O-ZFMARK.
    ENDIF.

    MODIFY IT_ZSMLCSG9O INDEX SY-TABIX.

  ENDIF.

ENDMODULE.                 " SET_SCR0108_MARK_TC_0106  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0108  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0108 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'.       " 삭제 및 취?
      LOOP AT IT_ZSMLCSG9O WHERE ZFMARK NE SPACE.
        DELETE IT_ZSMLCSG9O INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해?
      LOOP AT IT_ZSMLCSG9O.
        IT_ZSMLCSG9O-ZFMARK = W_MARK.   MODIFY IT_ZSMLCSG9O.
      ENDLOOP.
    WHEN 'REF1'.           " Refresh
      REFRESH : IT_ZSMLCSG9O.
      LOOP AT IT_ZSMLCSG9O_ORG.
        MOVE-CORRESPONDING   IT_ZSMLCSG9O_ORG   TO   IT_ZSMLCSG9O.
        APPEND IT_ZSMLCSG9O.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.
*
*-----------------------------------------------------------------------
* 기타 구비 서?
*-----------------------------------------------------------------------
  PERFORM   P2000_IT_ZSMLCSG9O_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR0108  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSREQIL_UPDATE_SCR0109  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSREQIL_UPDATE_SCR0109 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZSREQIL    WITH KEY ZFILSEQ = ZSREQIL-ZFILSEQ.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSREQIL   TO IT_ZSREQIL.
*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal )
*-----------------------------------------------------------------------
*>>> 외화금?
*   IF NOT IT_ZSREQIL-ZFRECCU IS INITIAL AND
*      NOT IT_ZSREQIL-ZFRECAM IS INITIAL.
*      PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSREQIL-ZFRECAM
*                                                 IT_ZSREQIL-ZFRECCU.
*   ENDIF.
*-----------------------------------------------------------------------
*  수입추천번?
  IF IT_ZSREQIL-ZFRECNO  IS INITIAL.   MESSAGE E092.   ENDIF.
*
*>> 수입추천번호 중복 CHECK. 2001.04.10 나현주 추가.
  READ  TABLE  IT_ZSREQIL  WITH KEY  ZFRECNO = IT_ZSREQIL-ZFRECNO.
  IF SY-SUBRC EQ 0.
    MESSAGE  E541.
  ENDIF.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSREQIL   INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " IT_ZSREQIL_UPDATE_SCR0109  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0109_MARK_TC_0109  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0109_MARK_TC_0109 INPUT.

  READ TABLE IT_ZSREQIL WITH KEY ZFILSEQ = ZSREQIL-ZFILSEQ
                        BINARY SEARCH.
*
  IF SY-SUBRC = 0.
*     MOVE-CORRESPONDING   ZSMLCSG11 TO IT_ZSMLCSG11.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSREQIL-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSREQIL-ZFMARK.
    ENDIF.
*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal )
*-----------------------------------------------------------------------
*>>> 외화금?
*     PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSREQIL-ZFRECAM
*                                                IT_ZSREQIL-ZFRECCU.
*-----------------------------------------------------------------------
    MODIFY IT_ZSREQIL INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0109_MARK_TC_0109  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0109_MARK_TC_0109_1  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0109_MARK_TC_0109_1 INPUT.

  READ TABLE IT_ZSINSHD   WITH KEY ZFREQNO = ZSINSHD-ZFREQNO
                                   ZFAMDNO = ZSINSHD-ZFAMDNO
                          BINARY SEARCH.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK1 IS INITIAL ).
      IT_ZSINSHD-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSINSHD-ZFMARK.
    ENDIF.
    MODIFY IT_ZSINSHD INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0109_MARK_TC_0109_1  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0109  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0109 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'.       " 원산지 삭?

      LOOP AT IT_ZSREQIL WHERE ZFMARK NE SPACE.
        DELETE  IT_ZSREQIL  INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해?
      LOOP AT IT_ZSREQIL.
        IT_ZSREQIL-ZFMARK = W_MARK.   MODIFY IT_ZSREQIL.
      ENDLOOP.
    WHEN 'REF1'.           " Refresh
      REFRESH : IT_ZSREQIL.
      IF ZTREQHD-ZFRECYN EQ 'X'.
        LOOP AT  IT_ZSREQIT.
          SELECT * FROM ZTIMIMG09 UP TO 1 ROWS
                                  WHERE STAWN    EQ IT_ZSREQIT-STAWN
                                  AND   ZFAPLDT  <= SY-DATUM
                                  ORDER BY ZFAPLDT. "DESCENDING.
          ENDSELECT.
          IF NOT ZTIMIMG09-ZFIRLW IS INITIAL.
            MOVE-CORRESPONDING IT_ZSREQIT  TO IT_ZSREQIL.
            MOVE IT_ZSREQIT-ZFITMNO   TO IT_ZSREQIL-ZFILSEQ.
*                     IT_ZSREQIT-ZFIRLW =  ZTIMIMG09-ZFIRLW.
            APPEND IT_ZSREQIL.
          ENDIF.
        ENDLOOP.
      ELSE.
        MESSAGE S010.
      ENDIF.

*     REFRESH : IT_ZSREQIL.
*     LOOP AT IT_ZSREQIT_ORG WHERE ZFIRLW NE SPACE
*                            AND   MENGE  > 0.
*        W_TABIX   =  SY-TABIX.
*        READ TABLE IT_ZSREQIL
*             WITH KEY ZFILSEQ = IT_ZSREQIT_ORG-ZFITMNO BINARY SEARCH.
*        IF SY-SUBRC NE 0.
**           MOVE-CORRESPONDING   IT_ZSREQIT_ORG   TO   IT_ZSREQIL.
**           MODIFY IT_ZSREQIL  INDEX W_TABIX.
**        ELSE.
*           CLEAR : IT_ZSREQIL.
*           MOVE-CORRESPONDING   IT_ZSREQIT_ORG   TO   IT_ZSREQIL.
*           MOVE IT_ZSREQIT_ORG-ZFITMNO   TO IT_ZSREQIL-ZFILSEQ.
*           APPEND IT_ZSREQIL.
*        ENDIF.
*     ENDLOOP.
    WHEN 'RECO'.
      CLEAR : W_COUNT.
      LOOP AT IT_ZSREQIL WHERE ZFMARK EQ 'X'.
        W_COUNT = W_COUNT + 1.
        MOVE-CORRESPONDING   IT_ZSREQIL   TO   ZSREQIL.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.
          MESSAGE S951.
        WHEN 1.    PERFORM  P2000_RECOMMAND_PROCESS.
        WHEN OTHERS.
          MESSAGE S965.
      ENDCASE.
    WHEN 'INDC'.
      W_COUNT = 0.
      LOOP AT IT_ZSINSHD   WHERE ZFMARK NE SPACE.
        READ TABLE IT_ZSINSHD
                          WITH KEY ZFREQNO  =  ZSINSHD-ZFREQNO
                                   ZFAMDNO  =  ZSINSHD-ZFAMDNO
                                                   BINARY SEARCH.
        MOVE-CORRESPONDING IT_ZSINSHD TO ZSINSHD.
        W_COUNT = W_COUNT + 1.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.          MESSAGE S962.
        WHEN 1.
* INSURANCE DOC LINK
          PERFORM  P2000_INS_DOC_DISPLAY.
        WHEN OTHERS.     MESSAGE S965.
      ENDCASE.
    WHEN 'INCR'.   " 보험문서 생?
*      IF W_STATUS EQ C_REQ_D.
      IF ZTREQHD-ZFINSYN NE 'N'.
        SET PARAMETER ID 'ZPOPNNO' FIELD ''.
        SET PARAMETER ID 'BES'     FIELD ''.
        SET PARAMETER ID 'ZPREQNO' FIELD ZTREQHD-ZFREQNO.
        CALL TRANSACTION 'ZIM41' AND SKIP FIRST SCREEN.
* Insurance Data Select
        PERFORM   P1000_INSURANCE_READ.
      ELSE.
        MESSAGE S178.
      ENDIF.
*      ELSE.
*         MESSAGE E177.
*      ENDIF.
    WHEN OTHERS.
  ENDCASE.
*
*-----------------------------------------------------------------------
* 기타 구비 서?
*-----------------------------------------------------------------------
*  PERFORM   P2000_IT_ZSMLCSG11_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR0109  INPUT
*&---------------------------------------------------------------------*
*&      Module  DOC_CHECK_SCR0107  INPUT
*&---------------------------------------------------------------------*
MODULE DOC_CHECK_SCR0107 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  IF    ( ZTMLCSG910-ZFCOMYN   IS INITIAL ) AND
        ( ZTMLCSG910-ZFOCEYN   IS INITIAL ) AND
        ( ZTMLCSG910-ZFAIRYN   IS INITIAL ) AND
        ( ZTMLCSG910-ZFINYN    IS INITIAL ) AND
        ( ZTMLCSG910-ZFPACYN   IS INITIAL ) AND
        ( ZTMLCSG910-ZFCEOYN   IS INITIAL ) AND
        ( ZTMLCSG910-ZFOTDYN   IS INITIAL ).
    MESSAGE I078.
  ENDIF.

ENDMODULE.                 " DOC_CHECK_SCR0107  INPUT
*&---------------------------------------------------------------------*
*&      Module  COMM_INVOICE_CHK_SCR0107  INPUT
*&---------------------------------------------------------------------*
MODULE COMM_INVOICE_CHK_SCR0107 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTMLCSG910-ZFCOMYN IS INITIAL.
    IF NOT ( ZTMLCSG910-ZFNOCOM IS INITIAL ).  " 미체크시 자동 체?
*       MESSAGE I079.   ZTMLCSG910-ZFCOMYN = 'X'.
      ZTMLCSG910-ZFCOMYN = 'X'.
    ENDIF.
  ELSE.
    IF ZTMLCSG910-ZFNOCOM IS INITIAL. MESSAGE E080. ENDIF.
  ENDIF.

ENDMODULE.                 " COMM_INVOICE_CHK_SCR0107  INPUT
*&---------------------------------------------------------------------*
*&      Module  OCEAN_BILL_CHECK_SCR0107  INPUT
*&---------------------------------------------------------------------*
MODULE OCEAN_BILL_CHECK_SCR0107 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTMLCSG910-ZFOCEYN IS INITIAL.
    IF NOT ( ZTMLCSG910-ZFOCEC1 IS INITIAL )
       OR NOT ( ZTMLCSG910-ZFOCEC2 IS INITIAL )
       OR NOT ( ZTMLCSG910-ZFOCEAC IS INITIAL )
       OR NOT ( ZTMLCSG910-ZFOCEAN IS INITIAL ).

*       MESSAGE I081.
      ZTMLCSG910-ZFOCEYN = 'X'.
    ENDIF.
  ELSE.
    IF ZTREQHD-ZFTRANS  = 'A'.
      MESSAGE E467 WITH 'Air'.
      EXIT.
    ENDIF.
    IF NOT ZTREQHD-ZFOPBN IS INITIAL.
      IF ZTMLCSG910-ZFOCEC1 IS INITIAL.
        MESSAGE I082.
      ENDIF.
    ENDIF.
    PERFORM P2000_PAY_YN_CHECK USING ZTMLCSG910-ZFOCEAC. " 운임지불여?
    IF ZTMLCSG910-ZFOCEAN IS INITIAL.
      MESSAGE I085.
    ENDIF.
  ENDIF.


ENDMODULE.                 " OCEAN_BILL_CHECK_SCR0107  INPUT
*&---------------------------------------------------------------------*
*&      Module  AIR_BILL_CHECK_SCR0107  INPUT
*&---------------------------------------------------------------------*
MODULE AIR_BILL_CHECK_SCR0107 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTMLCSG910-ZFAIRYN IS INITIAL.
    IF NOT ( ZTMLCSG910-ZFAIRC1 IS INITIAL )
       OR NOT ( ZTMLCSG910-ZFAIRC2 IS INITIAL )
       OR NOT ( ZTMLCSG910-ZFAIRAC IS INITIAL )
       OR NOT ( ZTMLCSG910-ZFAIRAN IS INITIAL ).

*       MESSAGE I086.
      ZTMLCSG910-ZFAIRYN = 'X'.

    ENDIF.
  ELSE.
    IF ZTREQHD-ZFTRANS  = 'O'.
      MESSAGE E467 WITH 'Ocean'.
      EXIT.
    ENDIF.
    IF NOT ZTREQHD-ZFOPBN IS INITIAL.
      IF ZTMLCSG910-ZFAIRC1 IS INITIAL.
        MESSAGE I082.
      ENDIF.
    ENDIF.

    PERFORM P2000_PAY_YN_CHECK USING ZTMLCSG910-ZFAIRAC. " 운임지불여?
    IF ZTMLCSG910-ZFAIRAN IS INITIAL.
      MESSAGE I085.
    ENDIF.
  ENDIF.

ENDMODULE.                 " AIR_BILL_CHECK_SCR0107  INPUT
*&---------------------------------------------------------------------*
*&      Module  INSURANCE_CHECK_SCR0107  INPUT
*&---------------------------------------------------------------------*
MODULE INSURANCE_CHECK_SCR0107 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTMLCSG910-ZFINYN IS INITIAL.
    IF NOT ( ZTMLCSG910-ZFINCO1 IS INITIAL )
       OR NOT ( ZTMLCSG910-ZFINCO2 IS INITIAL ).
*       MESSAGE I087.  ZTMLCSG910-ZFINYN = 'X'.
      ZTMLCSG910-ZFINYN = 'X'.
    ENDIF.
  ELSE.
    IF ZTMLCSG910-ZFINCO1 IS INITIAL.
      MESSAGE I088.
    ENDIF.
  ENDIF.

ENDMODULE.                 " INSURANCE_CHECK_SCR0107  INPUT
*&---------------------------------------------------------------------*
*&      Module  PACKING_LIST_CHECK_SCR0107  INPUT
*&---------------------------------------------------------------------*
MODULE PACKING_LIST_CHECK_SCR0107 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTMLCSG910-ZFPACYN IS INITIAL.
    IF NOT ( ZTMLCSG910-ZFNOPAC IS INITIAL ).
*       MESSAGE I095.
      ZTMLCSG910-ZFPACYN = 'X'.
    ENDIF.
  ELSE.
    IF ZTMLCSG910-ZFNOPAC IS INITIAL.   MESSAGE I096.   ENDIF.
  ENDIF.

ENDMODULE.                 " PACKING_LIST_CHECK_SCR0107  INPUT
*&---------------------------------------------------------------------*
*&      Module  OTHER_DOC_CHK_SCR0107  INPUT
*&---------------------------------------------------------------------*
MODULE OTHER_DOC_CHK_SCR0107 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  DESCRIBE TABLE IT_ZSMLCSG8E LINES G_PARAM_LINE.   " 기타 구비?

  IF ZTMLCSG910-ZFOTDYN EQ 'X'.
    IF G_PARAM_LINE EQ 0.
*       MESSAGE I097.    ZTMLCSG910-ZFOTDYN = ''.
      ZTMLCSG910-ZFOTDYN = ''.
    ENDIF.
  ELSE.
    IF G_PARAM_LINE NE 0.
*       MESSAGE I098.    ZTMLCSG910-ZFOTDYN = 'X'.
      ZTMLCSG910-ZFOTDYN = 'X'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " OTHER_DOC_CHK_SCR0107  INPUT
*&---------------------------------------------------------------------*
*&      Module  SHIPMENT_CHECK_SCR0108  INPUT
*&---------------------------------------------------------------------*
MODULE SHIPMENT_CHECK_SCR0108 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTMLCHD-ZFADCD1 IS INITIAL.
    IF NOT ( ZTMLCHD-ZFCARR IS INITIAL ).
      ZTMLCHD-ZFADCD1 = 'X'.
*       MESSAGE I120.   ZTMLCHD-ZFADCD1 = 'X'.
    ENDIF.
  ELSE.
    IF ZTMLCHD-ZFCARR IS INITIAL.  MESSAGE I121.  ENDIF.
  ENDIF.

ENDMODULE.                 " SHIPMENT_CHECK_SCR0108  INPUT
*&---------------------------------------------------------------------*
*&      Module  ADD_CONDITION_CHK_SCR0108  INPUT
*&---------------------------------------------------------------------*
MODULE ADD_CONDITION_CHK_SCR0108 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTMLCHD-ZFADCD5 EQ 'X'.
    DESCRIBE TABLE IT_ZSMLCSG9O LINES G_PARAM_LINE.   " 기타 구비서?
    IF G_PARAM_LINE EQ 0.
      MESSAGE I113.
    ENDIF.
  ENDIF.

ENDMODULE.                 " ADD_CONDITION_CHK_SCR0108  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0113  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0113 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'. " 품목 삭?
      IF OK-CODE EQ 'DEL1'.
        PERFORM  P2000_ITEM_DEL_CHECK.
      ENDIF.
      LOOP AT IT_ZSREQIT   WHERE ZFMARK NE SPACE.
        DELETE IT_ZSREQIT   INDEX SY-TABIX.
      ENDLOOP.
      TC_0103-TOP_LINE = 1.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해?
      LOOP AT IT_ZSREQIT.
        IT_ZSREQIT-ZFMARK = W_MARK.   MODIFY IT_ZSREQIT.
      ENDLOOP.
    WHEN 'PORE'.
      LOOP  AT  IT_ZSREQIT.
        PERFORM   P2000_REFRESH_PRICE.
      ENDLOOP.
    WHEN 'REF1'.           " 품목 Refresh
*>> 2001.07.04 NHJ 변경( PO ITEM의 내역으로 REFRESH )
      PERFORM   P2000_REFRESH_ITEM.
*>> 후속작업 검증...
*      PERFORM   P2000_REFRESH_RECORD_CHK.
*     REFRESH : IT_ZSREQIT.
*     LOOP AT IT_ZSREQIT_ORG.
*        MOVE-CORRESPONDING   IT_ZSREQIT_ORG   TO   IT_ZSREQIT.
*        APPEND IT_ZSREQIT.
*     ENDLOOP.
    WHEN OTHERS.
  ENDCASE.
*-----------------------------------------------------------------------
* 최종 개설금액 자동 갱?
*-----------------------------------------------------------------------
  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

ENDMODULE.                 " USER_COMMAND_SCR0113  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_TRANS_METHOD_SCR0152  INPUT
*&---------------------------------------------------------------------*
MODULE SET_TRANS_METHOD_SCR0152 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
*> 회사코드 정보 GET..(2001.08.21 KSB INSERT)
  PERFORM  P1000_READ_COMPANY_DATA  USING ZTREQHD-BUKRS
                                          ZTREQHD-IMTRD.

*  PERFORM  P2000_DELIVERY_MODE_SET.
  PERFORM  P2000_SET_SHIPING_TEXT.

ENDMODULE.                 " SET_TRANS_METHOD_SCR0152  INPUT
*&---------------------------------------------------------------------*
*&      Module  INCOTERMS_TEXT_GET  INPUT
*&---------------------------------------------------------------------*
MODULE INCOTERMS_TEXT_GET INPUT.
* If exit mode, exit.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC' .
      IF ZTREQHD-INCO1 IS INITIAL.
        MESSAGE E167 WITH 'Incoterms'.
      ENDIF.
      MOVE : ZTREQHD-INCO1   TO     ZTMLCHD-INCO1.
      PERFORM   P1000_GET_INCOTERMS_TEXT  USING    ZTREQHD-INCO1
                                          CHANGING ZTMLCHD-ZFINCN.
    WHEN 'DA' OR 'DP' OR 'TT'.
      IF ZTREQHD-INCO1 IS INITIAL.
        MESSAGE E167 WITH 'Incoterms'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " INCOTERMS_TEXT_GET  INPUT
*&---------------------------------------------------------------------*
*&      Module  APP_DATE_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE APP_DATE_SCR0102 INPUT.
* If exit mode, exit.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC' OR 'LO' OR 'PU' OR 'TT' OR 'GS'.
      IF ZTREQST-ZFAPPDT IS INITIAL.
        MESSAGE E167 WITH 'Opening expected date'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

*-----------------------------------------------------------------------
* Incase of local L/C, move SD/ED 2 months later from now on.
*-----------------------------------------------------------------------
  IF ZTREQHD-ZFREQTY EQ 'LO'.
    ZTREQHD-ZFJEWGB = '1'.   " 'Own capital' Default
    EXIT.

*-----------------------------------------------------------------------
* Incase of local L/C, get 2 months later date and move it to ED.
*-----------------------------------------------------------------------
*     CALL FUNCTION 'ZIM_BEFORE_N_MONTH_DATE'
*          EXPORTING
*               DATE                      = ZTREQST-ZFAPPDT
*               W_MONTH                   = 3
*          IMPORTING
*               W_OUT_DATE                = W_DATE
*          EXCEPTIONS
*               PLAUSIBILITY_CHECK_FAILED = 4.
*
*     IF SY-SUBRC NE 0.
*        MESSAGE E133 WITH ZTREQST-ZFAPPDT.
*     ELSE.
*        IF ZTREQHD-ZFREQED IS INITIAL.
*           ZTREQHD-ZFREQED = W_DATE.
*           ZTREQHD-ZFREQSD = W_DATE.
*           ZTLLCHD-ZFEXDT  = W_DATE.
*        ENDIF.
*     ENDIF.
  ENDIF.

ENDMODULE.                 " APP_DATE_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  DRAFTE_AT_SCR0106  INPUT
*&---------------------------------------------------------------------*
MODULE DRAFTE_AT_SCR0106 INPUT.
* If exit mode, exit.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  IF NOT ZTMLCHD-ZFTRMB IS INITIAL.
    IF  ZTMLCHD-ZFTRTX1 IS INITIAL AND
        ZTMLCHD-ZFTRTX2 IS INITIAL AND
        ZTMLCHD-ZFTRTX3 IS INITIAL AND
        ZTMLCHD-ZFTRTX4 IS INITIAL.
      MESSAGE E167 WITH 'Payment term detail'.
    ELSE.
      IF  ZTMLCHD-ZFTRTX1 IS INITIAL.
        MESSAGE E167 WITH 'Payment term detail'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " DRAFTE_AT_SCR0106  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCR1100  INPUT
*&---------------------------------------------------------------------*
MODULE READ_DOC_SCR1100 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZSREQHD-EBELN IS INITIAL AND     " P/O NO를 입력하지 않을 경?
     ZSREQHD-ZFREQNO IS INITIAL AND   " 관리번호가 입력하지 않을 경?
     ZSREQHD-ZFOPNNO IS INITIAL.      " 문서번호가 입력하지 않을 경?
    MESSAGE E066.
  ENDIF.
*
  IF NOT ZSREQHD-ZFOPNNO IS INITIAL.  " 문서번호가 입력된 경?
    IF ZSREQHD-ZFREQNO IS INITIAL.
* 문서 승인번?
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTREQST
                        WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO
                        AND  ( ZFDOCST EQ 'O' ).
*                         OR     ZFDOCST EQ 'A' ).
      CASE W_COUNT.
        WHEN 0.     MESSAGE E067 WITH ZSREQHD-ZFOPNNO.
        WHEN 1.
*             SELECT ZFREQNO ZFAMDNO
*                    INTO ( ZSREQHD-ZFREQNO, ZSREQHD-ZFAMDNO )
          SELECT * UP TO 1 ROWS
                         FROM ZTREQST
                         WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO
                         AND  ( ZFDOCST EQ 'O' ).
*                            OR     ZFDOCST EQ 'A' ).
            EXIT.
          ENDSELECT.
          MOVE : ZTREQST-ZFREQNO    TO   ZSREQHD-ZFREQNO,
                 ZTREQST-ZFAMDNO    TO   ZSREQHD-ZFAMDNO.
        WHEN OTHERS.
          PERFORM P2000_AMD_DOC_ITEM_SELECT.
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          PERFORM P2000_SEARCH_FIELD_MOVE.
      ENDCASE.
    ENDIF.
  ENDIF.

  IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
* P/O NO에 Count
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZVREQHD_ST
                      WHERE EBELN EQ ZSREQHD-EBELN
                        AND  ( ZFDOCST EQ 'O' ).
*                        OR     ZFDOCST EQ 'A' ).
    CASE W_COUNT.
      WHEN 0.     MESSAGE E064 WITH ZSREQHD-EBELN.
      WHEN 1.
        SELECT * UP TO 1 ROWS
                       FROM ZVREQHD_ST
                       WHERE EBELN EQ ZSREQHD-EBELN
                      AND  ( ZFDOCST EQ 'O' ).
*                        OR     ZFDOCST EQ 'A' ).
          EXIT.
        ENDSELECT.
        MOVE : ZVREQHD_ST-ZFREQNO    TO   ZSREQHD-ZFREQNO,
               ZVREQHD_ST-ZFAMDNO    TO   ZSREQHD-ZFAMDNO.
      WHEN OTHERS.
        PERFORM P2000_AMD_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
  ENDIF.
* 수입의뢰 문서 READ PERFORM ?
  PERFORM   P1000_REQ_DOC_READ.

ENDMODULE.                 " READ_DOC_SCR1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZSLLCOF_UPDATE_SCR0114  INPUT
*&---------------------------------------------------------------------*
MODULE ZSLLCOF_UPDATE_SCR0114 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZSLLCOF  WITH KEY ZFLSGOF = ZSLLCOF-ZFLSGOF.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSLLCOF   TO IT_ZSLLCOF.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSLLCOF   INDEX W_TABIX.
  ELSE.
    IT_ZSLLCOF-ZFLSGOF   = TC_0114-CURRENT_LINE * 10.
    APPEND IT_ZSLLCOF.
  ENDIF.

ENDMODULE.                 " ZSLLCOF_UPDATE_SCR0114  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0114_MARK_TC_0114  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0114_MARK_TC_0114 INPUT.

  READ TABLE IT_ZSLLCOF   WITH KEY ZFLSGOF = ZSLLCOF-ZFLSGOF
                          BINARY SEARCH.

  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING   ZSLLCOF   TO IT_ZSLLCOF.
    IF NOT ( W_ROW_MARK  IS INITIAL ).
      IT_ZSLLCOF-ZFMARK   = 'X'.
    ELSE.
      CLEAR : IT_ZSLLCOF-ZFMARK.
    ENDIF.
    MODIFY IT_ZSLLCOF   INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0114_MARK_TC_0114  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0114  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0114 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'CADD'.    " ADDRESS UPDATE.
      PERFORM   P2000_REFRESH_ADDRESS   USING   ZTREQHD-ZFBENI.
    WHEN 'CAD1'.    " ADDRESS UPDATE.( BANK )
      PERFORM  P1000_SET_OPEN_BANK.
    WHEN 'DEL1' OR 'UND1'.       " 원산지 삭?

      LOOP AT IT_ZSLLCOF   WHERE ZFMARK NE SPACE.
        DELETE IT_ZSLLCOF INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 원산지 전체선택 or 원산지 선택해?
      LOOP AT IT_ZSLLCOF.
        IT_ZSLLCOF-ZFMARK = W_MARK.   MODIFY IT_ZSLLCOF.
      ENDLOOP.

    WHEN 'REF1'.           " 원산지 Refresh
      REFRESH : IT_ZSLLCOF.
      LOOP AT IT_ZSLLCOF_ORG.
        MOVE-CORRESPONDING   IT_ZSLLCOF_ORG    TO   IT_ZSLLCOF.
        APPEND IT_ZSLLCOF.
      ENDLOOP.
    WHEN 'OFFD'.           " LOCAL OFFER DISPLAY
      W_COUNT = 0.
      LOOP AT IT_ZSLLCOF WHERE ZFMARK NE SPACE.
        W_COUNT = W_COUNT + 1.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.
          MESSAGE E962.
        WHEN 1.

*           CALL TRANSACTION 'ZIML3' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
          MESSAGE E965.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.
*
*-----------------------------------------------------------------------
* ORIJIN CODE
*-----------------------------------------------------------------------
  PERFORM   P2000_ZSLLCOF_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR0114  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZTLLCSG23_ZFDOMYN_SCR0115  INPUT
*&---------------------------------------------------------------------*
MODULE ZTLLCSG23_ZFDOMYN_SCR0115 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTLLCSG23-ZFDOMYN  IS INITIAL.
    IF NOT ( ZTLLCSG23-ZFNODOM  IS INITIAL ).  " 미체크시 자동 체?
      MESSAGE I126.   ZTLLCSG23-ZFDOMYN  = 'X'.
    ENDIF.
  ELSE.
    IF ZTLLCSG23-ZFNODOM  IS INITIAL.
      MESSAGE E167 WITH 'Item receipt certificate copy No'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZTLLCSG23_ZFDOMYN_SCR0115  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZTLLCSG23_BILL_CHK_SCR0115  INPUT
*&---------------------------------------------------------------------*
MODULE ZTLLCSG23_BILL_CHK_SCR0115 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* 송?
  IF ZTLLCSG23-ZFINVYN  IS INITIAL.
    IF NOT ( ZTLLCSG23-ZFNOINV  IS INITIAL ).  " 미체크시 자동 체?
      MESSAGE I128.   ZTLLCSG23-ZFINVYN  = 'X'.
    ENDIF.
  ELSE.
    IF ZTLLCSG23-ZFNOINV  IS INITIAL.
      MESSAGE E167 WITH 'Invoice reported item detail'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZTLLCSG23_BILL_CHK_SCR0115  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZTLLCSG23_ZFOFYN_SCR0115  INPUT
*&---------------------------------------------------------------------*
MODULE ZTLLCSG23_ZFOFYN_SCR0115 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTLLCSG23-ZFOFYN   IS INITIAL.
    IF NOT ( ZTLLCSG23-ZFNOOF   IS INITIAL ).  " 미체크시 자동 체?
      MESSAGE I128.   ZTLLCSG23-ZFOFYN   = 'X'.
    ENDIF.
  ELSE.
    IF ZTLLCSG23-ZFNOOF   IS INITIAL.
      MESSAGE E167 WITH 'Offer sheet copy No'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZTLLCSG23_ZFOFYN_SCR0115  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZTLLCSG23_ZFLLCYN_SCR0115  INPUT
*&---------------------------------------------------------------------*
MODULE ZTLLCSG23_ZFLLCYN_SCR0115 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTLLCSG23-ZFLLCYN  IS INITIAL.
    IF NOT ( ZTLLCSG23-ZFNOLLC  IS INITIAL ).  " 미체크시 자동 체?
      MESSAGE I129.   ZTLLCSG23-ZFLLCYN  = 'X'.
    ENDIF.
  ELSE.
    IF ZTLLCSG23-ZFNOLLC  IS INITIAL.
      MESSAGE E167 WITH 'Local L/C copy No'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZTLLCSG23_ZFLLCYN_SCR0115  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZTLLCSG23_BILL_CHK1_SCR0115  INPUT
*&---------------------------------------------------------------------*
MODULE ZTLLCSG23_BILL_CHK1_SCR0115 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  IF ZTLLCSG23-ZFBILYN EQ 'X' AND ZTLLCSG23-ZFINVYN EQ 'X'.
    MESSAGE E130.
  ENDIF.

ENDMODULE.                 " ZTLLCSG23_BILL_CHK1_SCR0115  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZTLLCSG23_BILL_CHK2_SCR0115  INPUT
*&---------------------------------------------------------------------*
MODULE ZTLLCSG23_BILL_CHK2_SCR0115 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* 세금계산?
  IF ZTLLCSG23-ZFBILYN  IS INITIAL.
    IF NOT ( ZTLLCSG23-ZFNOBIL  IS INITIAL ).  " 미체크시 자동 체?
      MESSAGE I127.   ZTLLCSG23-ZFBILYN  = 'X'.
    ENDIF.
  ELSE.
    IF ZTLLCSG23-ZFNOBIL  IS INITIAL.
      MESSAGE E167 WITH 'Tax bill No'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZTLLCSG23_BILL_CHK2_SCR0115  INPUT
*&---------------------------------------------------------------------*
*&      Module  OTHER_DOC_CHECK_SCR0115  INPUT
*&---------------------------------------------------------------------*
MODULE OTHER_DOC_CHECK_SCR0115 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF  ZTLLCSG23-ZFDOMYN IS INITIAL AND
      ZTLLCSG23-ZFBILYN IS INITIAL AND
      ZTLLCSG23-ZFINVYN IS INITIAL AND
      ZTLLCSG23-ZFOFYN IS INITIAL  AND
      ZTLLCSG23-ZFLLCYN IS INITIAL.
    MESSAGE E131.
  ELSE.
    IF  ZTLLCSG23-ZFBILYN IS INITIAL AND
        ZTLLCSG23-ZFINVYN IS INITIAL.
      MESSAGE E131.
    ENDIF.
  ENDIF.


ENDMODULE.                 " OTHER_DOC_CHECK_SCR0115  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0123  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0123 INPUT.

  READ TABLE IT_ZSPURSG1  WITH KEY ZFLSG1 = ZSPURSG1-ZFLSG1
                          BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
*    MOVE-CORRESPONDING   ZSPURSG1   TO IT_ZSPURSG1.

    IF NOT ( W_ROW_MARK1 IS INITIAL ).
      IT_ZSPURSG1-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSPURSG1-ZFMARK.
    ENDIF.
*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal )
*-----------------------------------------------------------------------
*>>> 금?
*    PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSPURSG1-ZFGOAMT
*                                               IT_ZSPURSG1-WAERS.
*>>> 단?
*    PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSPURSG1-NETPR
*                                               IT_ZSPURSG1-WAERS.

    MODIFY IT_ZSPURSG1 INDEX W_TABIX.

  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_0123  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0004_MARK_TC_0004  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0004_MARK_TC_0004 INPUT.

  READ TABLE IT_ZSPURSG1G_SUB WITH KEY ZFLSG1G = ZSPURSG1G-ZFLSG1G
                                       BINARY SEARCH.
  IF SY-SUBRC = 0.
*        MOVE-CORRESPONDING   ZSPURSG1G TO IT_ZSPURSG1G_SUB.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSPURSG1G_SUB-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSPURSG1G_SUB-ZFMARK.
    ENDIF.
    MODIFY IT_ZSPURSG1G_SUB INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0004_MARK_TC_0004  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZSPURSG1G_UPDATE_SCR0004  INPUT
*&---------------------------------------------------------------------*
MODULE ZSPURSG1G_UPDATE_SCR0004 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZSPURSG1G_SUB WITH KEY ZFLSG1G = ZSPURSG1G-ZFLSG1G.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSPURSG1G TO IT_ZSPURSG1G_SUB.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSPURSG1G_SUB INDEX W_TABIX.
  ELSE.
    IT_ZSPURSG1G_SUB-ZFLSG1G = TC_0004-CURRENT_LINE * 10.
    APPEND IT_ZSPURSG1G_SUB.
  ENDIF.

ENDMODULE.                 " ZSPURSG1G_UPDATE_SCR0004  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0125  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0125 INPUT.

*  READ TABLE IT_ZSPURSG4  WITH KEY ZFLSG4 = ZSPURSG4-ZFLSG4
*                          BINARY SEARCH.
  READ TABLE IT_ZSPURSG4  INDEX  TC_0125-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
*    MOVE-CORRESPONDING   ZSPURSG4   TO IT_ZSPURSG4.

    IF NOT ( W_ROW_MARK2 IS INITIAL ).
      IT_ZSPURSG4-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSPURSG4-ZFMARK.
    ENDIF.
*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal )
*-----------------------------------------------------------------------
*>>> 금?
*    PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSPURSG4-ZFGOAMT
*                                               IT_ZSPURSG4-WAERS.

    MODIFY IT_ZSPURSG4 INDEX W_TABIX.

  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_0125  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_PUR_DOC_NUMBER_SCR0012  INPUT
*&---------------------------------------------------------------------*
MODULE GET_PUR_DOC_NUMBER_SCR0012 INPUT.

  IF SY-UCOMM EQ  'NO'.
    ANTWORT = 'N'.   SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  IF ZTPURSG4-ZFSDOC IS INITIAL.
    MESSAGE  E167  WITH 'Well-founded Doc code'.
  ENDIF.

ENDMODULE.                 " GET_PUR_DOC_NUMBER_SCR0012  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_PUR_DOC_NUMBER1_SCR0012  INPUT
*&---------------------------------------------------------------------*
MODULE GET_PUR_DOC_NUMBER1_SCR0012 INPUT.

  IF SY-UCOMM EQ  'NO'.
    ANTWORT = 'N'.   SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  IF ZTPURSG4-ZFSDNO IS INITIAL.
    MESSAGE  E167  WITH 'Well-founded Doc code'.
  ENDIF.

ENDMODULE.                 " GET_PUR_DOC_NUMBER1_SCR0012  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZSPURSG4_UPDATE_SCR0123  INPUT
*&---------------------------------------------------------------------*
MODULE ZSPURSG4_UPDATE_SCR0123 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZSPURSG4  INDEX  TC_0125-CURRENT_LINE.
* READ TABLE IT_ZSPURSG4  WITH KEY ZFLSG4  = ZSPURSG4-ZFLSG4.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSPURSG4  TO IT_ZSPURSG4.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSPURSG4 INDEX W_TABIX.
  ELSE.
    IT_ZSPURSG4-ZFLSG4  = TC_0125-CURRENT_LINE * 10.
    APPEND IT_ZSPURSG4.
  ENDIF.
ENDMODULE.                 " ZSPURSG4_UPDATE_SCR0123  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR1103  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR1103 INPUT.

  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1' OR
     SY-UCOMM EQ 'MKA2' OR SY-UCOMM EQ 'DEL2'.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1' OR
         SY-UCOMM EQ 'MKL2' OR SY-UCOMM EQ 'UND2'.
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'. " Item Delete / Cancel
      IF OK-CODE EQ 'DEL1' AND SY-TCODE NE 'ZIM01'.
        PERFORM  P2000_ITEM_DEL_CHECK.
      ENDIF.
      LOOP AT IT_ZSREQIT   WHERE ZFMARK NE SPACE.
        DELETE IT_ZSREQIT   INDEX SY-TABIX.
      ENDLOOP.
      TC_0103-TOP_LINE = 1.
    WHEN 'MKA1' OR 'MKL1'. " All Selection, All Deselection
      LOOP AT IT_ZSREQIT.
        IT_ZSREQIT-ZFMARK = W_MARK.   MODIFY IT_ZSREQIT.
      ENDLOOP.
    WHEN 'PORE'.
      LOOP  AT  IT_ZSREQIT.
        PERFORM   P2000_REFRESH_PRICE.
      ENDLOOP.
    WHEN 'REF1'.
      PERFORM   P2000_REFRESH_ITEM.
    WHEN 'SU01'.
      W_COUNT = 0.
      LOOP AT IT_ZSREQIT  WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        ADD   1    TO    W_COUNT.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.         MESSAGE S962.   EXIT.
        WHEN 1.
         PERFORM P2000_REQUESTER_DISPLAY USING IT_ZSREQIT-EBELN
                                               IT_ZSREQIT-EBELP.
        WHEN OTHERS.    MESSAGE S965.   EXIT.
      ENDCASE.
*>> 2002.12.18 JSY 추가 P/O 조회.
    WHEN 'PODP'.
      W_COUNT = 0.
      LOOP AT IT_ZSREQIT  WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        ADD   1    TO    W_COUNT.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.         MESSAGE S962.   EXIT.
        WHEN 1.
            MOVE-CORRESPONDING  IT_ZSREQIT  TO  W_ZSREQIT.
            PERFORM P2000_PO_ITEM_DISPLAY.
        WHEN OTHERS.    MESSAGE S965.   EXIT.
      ENDCASE.
*>>>  후속작업 검?
*      PERFORM   P2000_REFRESH_RECORD_CHK.
*     REFRESH : IT_ZSREQIT.
*     LOOP AT IT_ZSREQIT_ORG.
*        MOVE-CORRESPONDING   IT_ZSREQIT_ORG   TO   IT_ZSREQIT.
*        APPEND IT_ZSREQIT.
*     ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

*-----------------------------------------------------------------------
* 최종 개설금액 자동 갱?
*-----------------------------------------------------------------------
  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

ENDMODULE.                 " USER_COMMAND_SCR1103  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_REQ_DOC_SCR2100  INPUT
*&---------------------------------------------------------------------*
MODULE READ_REQ_DOC_SCR2100 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  W_COUNT = 1.
* 입력값 CHECK.
  IF ZSREQHD-EBELN IS INITIAL.           " P/O NO를 입력하지 않을 경?
    IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E063.
    ELSE.
* 수입의뢰 문서 READ PERFORM ?
      PERFORM   P1000_REQ_DOC_READ_OFFER.
    ENDIF.
  ELSE.
* P/O NO에 Count
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZTREQHD
                      WHERE EBELN EQ ZSREQHD-EBELN
                      AND   ZFREQTY EQ 'LO'.

    CASE W_COUNT.
      WHEN 0.     MESSAGE E064 WITH ZSREQHD-EBELN.
      WHEN 1.
        SELECT ZFREQNO INTO ZSREQHD-ZFREQNO UP TO 1 ROWS
                          FROM ZTREQHD
                          WHERE EBELN EQ ZSREQHD-EBELN
                          AND   ZFREQTY EQ 'LO'.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* 수입의뢰 문서 READ PERFORM ?
    PERFORM   P1000_REQ_DOC_READ_OFFER.
    EXIT.
  ENDIF.
* 수입의뢰 문서 READ PERFORM ?
  PERFORM   P1000_REQ_DOC_READ_OFFER.


ENDMODULE.                 " READ_REQ_DOC_SCR2100  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSOFFO_UPDATE_SCR2103  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSOFFO_UPDATE_SCR2103 INPUT.


* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZSOFFO    WITH KEY ZFLO    = ZSOFFO-ZFLO.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  SELECT SINGLE LANDX INTO    ZSOFFO-ZFORNM FROM T005T
                              WHERE SPRAS  EQ SY-LANGU
                              AND   LAND1  EQ ZSOFFO-ZFORIG.
  MOVE: ZSOFFO-ZFORNM TO W_TEXT70.

  TRANSLATE W_TEXT70 TO UPPER CASE.
  ZSOFFO-ZFORNM = W_TEXT70.

  MOVE-CORRESPONDING ZSOFFO    TO IT_ZSOFFO.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSOFFO   INDEX W_TABIX.
  ELSE.
    IT_ZSOFFO-ZFLO  = TC_2103_1-CURRENT_LINE * 10.
    APPEND IT_ZSOFFO.
  ENDIF.

ENDMODULE.                 " IT_ZSOFFO_UPDATE_SCR2103  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR2103_MARK_TC_2103_1  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR2103_MARK_TC_2103_1 INPUT.

  READ TABLE IT_ZSOFFO   WITH KEY ZFLO = ZSOFFO-ZFLO  BINARY SEARCH.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK1 IS INITIAL ).
      IT_ZSOFFO-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSOFFO-ZFMARK.
    ENDIF.
    MODIFY IT_ZSOFFO INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR2103_MARK_TC_2103_1  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_OFF_DOC_SCR2100  INPUT
*&---------------------------------------------------------------------*
MODULE READ_OFF_DOC_SCR2100 INPUT.

  ZTOFF-ZFREQNO = ZTREQHD-ZFREQNO.

  PERFORM   P1000_READ_OFFER_SHEET.

ENDMODULE.                 " READ_OFF_DOC_SCR2100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR2103_MARK_TC_2103  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR2103_MARK_TC_2103 INPUT.

  READ TABLE IT_ZSOFFSG6 WITH KEY ZFLSG6 = ZSOFFSG6-ZFLSG6
                         BINARY SEARCH.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSOFFSG6-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSOFFSG6-ZFMARK.
    ENDIF.
    MODIFY IT_ZSOFFSG6 INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR2103_MARK_TC_2103  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_REQ_DOC_SCR2200  INPUT
*&---------------------------------------------------------------------*
MODULE READ_REQ_DOC_SCR2200 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  W_COUNT = 1.
* 입력값 CHECK.
  IF ZSREQHD-EBELN IS INITIAL.           " P/O NO를 입력하지 않을 경?
    IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E063.
    ELSE.
* 수입의뢰 문서 READ PERFORM ?
      PERFORM   P1000_REQ_DOC_READ_OFFER.
    ENDIF.
  ELSE.
* P/O NO에 Count
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZVREQHD_ST AS R INNER JOIN ZTOFF AS O
                            ON    R~ZFREQNO  EQ  O~ZFREQNO
                      WHERE R~EBELN   EQ ZSREQHD-EBELN
                      AND   R~ZFAMDNO EQ '00000'.

    CASE W_COUNT.
      WHEN 0.     MESSAGE E064 WITH ZSREQHD-EBELN.
      WHEN 1.
        SELECT R~ZFREQNO INTO ZSREQHD-ZFREQNO UP TO 1 ROWS
                       FROM  ZVREQHD_ST AS R INNER JOIN ZTOFF AS O
                          ON    R~ZFREQNO  EQ  O~ZFREQNO
                    WHERE R~EBELN   EQ ZSREQHD-EBELN
                    AND   R~ZFAMDNO EQ '00000'.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_OFF_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* 수입의뢰 문서 READ PERFORM ?
    PERFORM   P1000_REQ_DOC_READ_OFFER.
    EXIT.
  ENDIF.

* 수입의뢰 문서 READ PERFORM ?
  PERFORM   P1000_REQ_DOC_READ_OFFER.

ENDMODULE.                 " READ_REQ_DOC_SCR2200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR2101  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR2101 INPUT.

  CASE OK-CODE.
    WHEN 'ZIMG'.                    " Import IMG
      CALL TRANSACTION OK-CODE.
    WHEN 'DDLC'.                    " Double Click Event
      PERFORM  P2000_DDLC_PROCESS.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'OTDC' OR 'CRDC' OR 'CHDC' OR 'DISP' OR 'ADDC' OR 'OPDC'.
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'DELE' OR 'DELR' OR 'REVK' OR 'OPCL' OR 'EDIS'.
      PERFORM  P2000_SET_INDICATE_OFFER.
    WHEN 'FLAT'.           " FLAT DATA
      PERFORM P2000_SHOW_FLAT.
    WHEN 'CKEK'.           " 검?
      PERFORM   P2000_OFFER_SHEET_CHECK.
      IF ZTOFF-ZFEDICK = 'O'.
        MESSAGE I123 WITH ZTOFF-ZFREQNO ''.
      ENDIF.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN 'ME23'.           "구매문서조회...
      PERFORM  P2000_PO_DOCUMENT_DISPLAY.
    WHEN 'MK03'.           "구매거래처 조?
      PERFORM  P2000_VENDOR_DISPLAY.
    WHEN 'ZIM03'.          " 수입문서 조?
      PERFORM   P2000_LC_DOC_DISPLAY    USING  ZTOFF-ZFREQNO  ''.
    WHEN 'ZIM93'.          "문서별 진행관?
      PERFORM  P2000_ZIM93_DISPLAY     USING  ZTREQHD-ZFREQNO
                                              ZTREQHD-EBELN.
*     WHEN 'ZI72'.           "자재별 진행관?
*          PERFORM SELECT_MATERIAL_CHECK.
*          PERFORM MATERIAL_HIERARCHY.
    WHEN 'HIST'.           "헤더변경문?
*-----------------------------------------------------------------------
*  HISTORY CHANGE를 보는 프로그?
*-----------------------------------------------------------------------
      PERFORM   P2000_HEADER_CHANGE_DOC.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_SCR2101  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0011  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0011 INPUT.

  CASE OK-CODE.
    WHEN 'DDCL'.
      IF TC_0011-TOP_LINE > LINE.    EXIT.   ENDIF.
      READ TABLE IT_ZSREQHD INDEX LINE.
      IT_ZSREQHD-ZFMARK = 'X'.    MODIFY   IT_ZSREQHD   INDEX  LINE.
      ANTWORT = 'Y'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN OTHERS.   EXIT.
  ENDCASE.

  IF ANTWORT EQ 'Y'.
    W_SEL_MAT_CNT = 0.
    LOOP AT IT_ZSREQHD WHERE ZFMARK EQ 'X'.
      W_SEL_MAT_CNT = W_SEL_MAT_CNT + 1.
      MOVE-CORRESPONDING  IT_ZSREQHD  TO  W_ZSREQHD.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      MESSAGE I962.
    ELSE.
      IF W_SEL_MAT_CNT NE 1.
        MESSAGE I965.
      ELSE.
        ZSREQHD-ZFREQNO = W_ZSREQHD-ZFREQNO.
        ZSREQHD-ZFAMDNO = W_ZSREQHD-ZFAMDNO.
        SET SCREEN 0.   LEAVE SCREEN.
      ENDIF.
    ENDIF.
  ELSE.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR0011  INPUT
*&---------------------------------------------------------------------*
*&      Module  SHIP_DATE_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE SHIP_DATE_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      IF ZTREQHD-ZFREQSD IS INITIAL.
        MESSAGE E167 WITH 'Shipping date(S/D)'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  PERFORM   P2000_EXPIRY_DATE_CHECK.

ENDMODULE.                 " SHIP_DATE_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCR4100  INPUT
*&---------------------------------------------------------------------*
MODULE READ_DOC_SCR4100 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZSREQHD-EBELN IS INITIAL AND
     ZSREQHD-ZFREQNO IS INITIAL AND
     ZSREQHD-ZFOPNNO IS INITIAL.
    MESSAGE E066.
  ENDIF.
* IMPORT IMG SELECT
  PERFORM    P1000_GET_RELEASE_DATA.

  IF NOT ZSREQHD-ZFOPNNO IS INITIAL.  " Input L/C
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZVREQHD_ST
                      WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO
                      AND   ZFRLST1 IN R_ZFRLST1
                      AND   ZFRTNYN EQ SPACE
                      AND   ZFCLOSE EQ SPACE
                      AND   ZFINSYN NE SPACE.

    CASE W_COUNT.
      WHEN 0.     MESSAGE E067 WITH ZSREQHD-ZFOPNNO.
      WHEN 1.
        SELECT ZFREQNO INTO ZSREQHD-ZFREQNO UP TO 1 ROWS
                       FROM ZVREQHD_ST
                       WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO
                       AND   ZFRLST1 IN R_ZFRLST1
                       AND   ZFRTNYN EQ SPACE
                       AND   ZFCLOSE EQ SPACE
                       AND   ZFINSYN NE SPACE.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_INS_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* Import Request Data Get.
    PERFORM   P1000_REQ_DOC_READ.

* Insurance Grade.
    IF NOT ( ZTREQHD-ZFINSYN EQ 'A' OR ZTREQHD-ZFINSYN EQ 'Z' ).
      MESSAGE E219 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFINSYN.
    ENDIF.

* Insurance Document Get.
    SELECT COUNT( * ) INTO W_CNT FROM ZTINS
                      WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.
    IF W_CNT GT 0.
      MESSAGE W216 WITH ZTREQST-ZFREQNO.
    ENDIF.

    CLEAR : W_ZFINSEQ.
    SELECT MAX( ZFINSEQ ) INTO W_ZFINSEQ
           FROM ZTINS
           WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.

    IF NOT W_ZFINSEQ IS INITIAL.
      SELECT SINGLE * INTO OLD_ZTINS
             FROM   ZTINS
             WHERE  ZFREQNO  EQ   ZTREQHD-ZFREQNO
             AND    ZFINSEQ  EQ   W_ZFINSEQ
             AND    ZFAMDNO  EQ ( SELECT MAX( ZFAMDNO )
                                  FROM   ZTINS
                                  WHERE  ZFREQNO EQ ZTREQHD-ZFREQNO
                                  AND    ZFINSEQ EQ W_ZFINSEQ ).

      IF SY-SUBRC EQ 0.
        IF OLD_ZTINS-ZFDOCST NE 'O'.
          MESSAGE E544 WITH OLD_ZTINS-ZFREQNO OLD_ZTINS-ZFINSEQ
                            OLD_ZTINS-ZFAMDNO.
        ENDIF.
      ENDIF.
    ENDIF.
    EXIT.
  ENDIF.

  IF NOT ZSREQHD-EBELN IS INITIAL.    "
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZVREQHD_ST
                      WHERE EBELN EQ ZSREQHD-EBELN
                      AND   ZFRLST1 IN R_ZFRLST1
                      AND   ZFRTNYN EQ SPACE
                      AND   ZFCLOSE EQ SPACE
                      AND   ZFINSYN NE SPACE.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E064 WITH ZSREQHD-EBELN.
      WHEN 1.
        SELECT ZFREQNO INTO ZSREQHD-ZFREQNO UP TO 1 ROWS
                       FROM ZVREQHD_ST
                       WHERE EBELN EQ ZSREQHD-EBELN
                       AND   ZFRLST1 IN R_ZFRLST1
                       AND   ZFRTNYN EQ SPACE
                       AND   ZFCLOSE EQ SPACE
                       AND   ZFINSYN NE SPACE.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_INS_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.

* Import Request Data get.
    PERFORM   P1000_REQ_DOC_READ.

* Insurance Grade
    IF NOT ( ZTREQHD-ZFINSYN EQ 'A' OR ZTREQHD-ZFINSYN EQ 'Z' ).
      MESSAGE E219 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFINSYN.
    ENDIF.

* Get Insurance Document Data.
    SELECT COUNT( * ) INTO W_CNT FROM ZTINS
                      WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.
    IF W_CNT GT 0.
      MESSAGE W216 WITH ZTREQST-ZFREQNO.
    ENDIF.

    CLEAR : W_ZFINSEQ.
    SELECT MAX( ZFINSEQ ) INTO W_ZFINSEQ
           FROM ZTINS
           WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.

    IF NOT W_ZFINSEQ IS INITIAL.
      SELECT SINGLE * INTO OLD_ZTINS
             FROM   ZTINS
             WHERE  ZFREQNO  EQ   ZTREQHD-ZFREQNO
             AND    ZFINSEQ  EQ   W_ZFINSEQ
             AND    ZFAMDNO  EQ ( SELECT MAX( ZFAMDNO )
                                  FROM   ZTINS
                                  WHERE  ZFREQNO EQ ZTREQHD-ZFREQNO
                                  AND    ZFINSEQ EQ W_ZFINSEQ ).

      IF SY-SUBRC EQ 0.
        IF OLD_ZTINS-ZFDOCST NE 'O'.
          MESSAGE E544 WITH OLD_ZTINS-ZFREQNO OLD_ZTINS-ZFINSEQ
                            OLD_ZTINS-ZFAMDNO.
        ENDIF.
      ENDIF.
    ENDIF.
    EXIT.
  ENDIF.
* Import Request Data Get.
  PERFORM   P1000_REQ_DOC_READ.

* Insurance grade.
  IF NOT ( ZTREQHD-ZFINSYN EQ 'A' OR ZTREQHD-ZFINSYN EQ 'Z' ).
    MESSAGE E219 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFINSYN.
  ENDIF.
* Insurance Document Get.
  SELECT COUNT( * ) INTO W_CNT FROM ZTINS
                    WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.
  IF W_CNT GT 0.
    MESSAGE W216 WITH ZTREQST-ZFREQNO.
  ENDIF.

  CLEAR : W_ZFINSEQ.
  SELECT MAX( ZFINSEQ ) INTO W_ZFINSEQ
         FROM ZTINS
         WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.

  IF NOT W_ZFINSEQ IS INITIAL.
    SELECT SINGLE * INTO OLD_ZTINS
           FROM   ZTINS
           WHERE  ZFREQNO  EQ   ZTREQHD-ZFREQNO
           AND    ZFINSEQ  EQ   W_ZFINSEQ
           AND    ZFAMDNO  EQ ( SELECT MAX( ZFAMDNO )
                                FROM   ZTINS
                                WHERE  ZFREQNO EQ ZTREQHD-ZFREQNO
                                AND    ZFINSEQ EQ W_ZFINSEQ ).

    IF SY-SUBRC EQ 0.
      IF OLD_ZTINS-ZFDOCST NE 'O'.
        MESSAGE E544 WITH OLD_ZTINS-ZFREQNO OLD_ZTINS-ZFINSEQ
                          OLD_ZTINS-ZFAMDNO.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " READ_DOC_SCR4100  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
MODULE D0014_LIST_CHECK_SCR0014 INPUT.

  LEAVE TO LIST-PROCESSING.
* LIST WRITE
  PERFORM P2000_DATA_LISTING.

ENDMODULE.                 " D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_INSURANCE_SCR4200  INPUT
*&---------------------------------------------------------------------*
MODULE READ_INSURANCE_SCR4200 INPUT.
  IF NOT ( ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1 ).
    EXIT.
  ENDIF.

  IF SY-TCODE EQ 'ZIM45'.
    SELECT MAX( ZFAMDNO ) INTO ZSREQHD-ZFAMDNO FROM ZTINS
                          WHERE ZFREQNO  EQ  ZSREQHD-ZFREQNO
                          AND   ZFINSEQ  EQ  ZSREQHD-ZFINSEQ.
  ENDIF.

  CALL FUNCTION 'ZIM_GET_INSURANCE_DOC'
       EXPORTING
            ZFREQNO         = ZSREQHD-ZFREQNO
            ZFINSEQ         = ZSREQHD-ZFINSEQ
            ZFAMDNO         = ZSREQHD-ZFAMDNO
       IMPORTING
            W_ZTINS         = ZTINS
            W_ZTINSRSP      = ZTINSRSP
            W_ZTINSSG3      = ZTINSSG3
       TABLES
            IT_ZSINSAGR     = IT_ZSINSAGR
            IT_ZSINSAGR_ORG = IT_ZSINSAGR_ORG
            IT_ZSINSSG2     = IT_ZSINSSG2
            IT_ZSINSSG2_ORG = IT_ZSINSSG2_ORG
            IT_ZSINSSG5     = IT_ZSINSSG5
            IT_ZSINSSG5_ORG = IT_ZSINSSG5_ORG
       EXCEPTIONS
            NOT_FOUND       = 4
            NOT_INPUT       = 8.

  CASE SY-SUBRC.
    WHEN 4.
      IF SY-TCODE NE 'ZIM41'.
        MESSAGE E169 WITH ZSREQHD-ZFREQNO.
      ENDIF.
    WHEN 8.
      MESSAGE E173.
  ENDCASE.

  CASE SY-TCODE.
    WHEN 'ZIM45'.
      IF NOT ZTINS-BELNR IS INITIAL.
        MESSAGE S109(ZIM1) WITH ZTINS-BELNR.
      ENDIF.
    WHEN 'ZIM44' OR 'ZIM48'.
      IF NOT ZTINS-BELNR IS INITIAL.
        MESSAGE W109(ZIM1) WITH ZTINS-BELNR.
      ENDIF.
  ENDCASE.

* 보험 등급 검?
  IF NOT ( ZTREQHD-ZFINSYN EQ 'A' OR ZTREQHD-ZFINSYN EQ 'Z' ).
    MESSAGE I220 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFINSYN.
  ENDIF.

* 변경이력을 위?
   *ZTINS    = ZTINS.
   *ZTINSRSP = ZTINSRSP.
   *ZTINSSG3 = ZTINSSG3.

* AMEND용 TEMP
  IF SY-TCODE EQ 'ZIM45'.
    ZTINS_OLD    = ZTINS.
    ZTINSRSP_OLD = ZTINSRSP.
    ZTINSSG3_OLD = ZTINSSG3.
    IT_ZSINSAGR_OLD[] = IT_ZSINSAGR[].
    IT_ZSINSSG2_OLD[] = IT_ZSINSSG2[].
    IT_ZSINSSG5_OLD[] = IT_ZSINSSG5[].
  ELSEIF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR
     SY-TCODE EQ 'ZIM47' OR SY-TCODE EQ 'ZIM48'.

    W_ZFAMDNO = ZSREQHD-ZFAMDNO - 1.

    CALL  FUNCTION 'ZIM_GET_INSURANCE_DOC'
       EXPORTING
          ZFREQNO             =          ZSREQHD-ZFREQNO
          ZFINSEQ             =          ZSREQHD-ZFINSEQ
          ZFAMDNO             =          W_ZFAMDNO
       IMPORTING
          W_ZTINS             =          ZTINS_OLD
          W_ZTINSRSP          =          ZTINSRSP_OLD
          W_ZTINSSG3          =          ZTINSSG3_OLD
       TABLES
           IT_ZSINSAGR        =          IT_ZSINSAGR_OLD
*           IT_ZSINSAGR_ORG    =          IT_ZSINSAGR_ORG
           IT_ZSINSSG2        =          IT_ZSINSSG2_OLD
*           IT_ZSINSSG2_ORG    =          IT_ZSINSSG2_ORG
           IT_ZSINSSG5        =          IT_ZSINSSG5_OLD
*           IT_ZSINSSG5_ORG    =          IT_ZSINSSG5_ORG
        EXCEPTIONS
           NOT_FOUND         =    4
           NOT_INPUT         =    8.
    CASE SY-SUBRC.
      WHEN 4.
        MESSAGE E169 WITH ZSREQHD-ZFREQNO.
      WHEN 8.
        MESSAGE E173.
    ENDCASE.

  ENDIF.

* 생성일 경우, exit
  CHECK : SY-TCODE NE 'ZIM41'.

* DOC 상태 CHECK.....
  PERFORM   P2000_INS_STATUS_CHECK.

  W_ZFTRANS = ZTINS-ZFTRANS.
* LOCK OBJECT
  IF NOT ( W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D ).
    PERFORM P2000_SET_INS_DOC_LOCK USING    'L'.
  ENDIF.

ENDMODULE.                 " READ_INSURANCE_SCR4200  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCR4200  INPUT
*&---------------------------------------------------------------------*
MODULE READ_DOC_SCR4200 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZSREQHD-EBELN IS INITIAL   AND    " P/O NO를 입력하지 않을 경?
     ZSREQHD-ZFREQNO IS INITIAL AND    " 관리번호가 입력하지 않을 경?
     ZSREQHD-ZFOPNNO IS INITIAL.       " 문서번호가 입력하지 않을 경?
    MESSAGE E066.
  ENDIF.
*
  IF NOT ZSREQHD-ZFOPNNO IS INITIAL.   " 문서번호가 입력된 경?
* 문서 승인번?
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                      ON    R~ZFREQNO    EQ   I~ZFREQNO
                      WHERE R~ZFOPNNO EQ ZSREQHD-ZFOPNNO
                      AND   R~ZFAMDNO EQ '00000'
                      AND   I~ZFAMDNO EQ '00000'.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E171 WITH ZSREQHD-ZFOPNNO.
      WHEN 1.
        SELECT R~ZFREQNO I~ZFINSEQ I~ZFAMDNO
                 INTO (ZSREQHD-ZFREQNO, ZSREQHD-ZFINSEQ,
                       ZSREQHD-ZFAMDNO)
                 UP TO 1 ROWS
                 FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                 ON    R~ZFREQNO    EQ   I~ZFREQNO
                 WHERE R~ZFOPNNO EQ ZSREQHD-ZFOPNNO
                 AND   R~ZFAMDNO EQ '00000'
                 AND   I~ZFAMDNO EQ '00000'.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_INS_DOC_DISP_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* 수입의뢰 문서 READ PERFORM ?
    PERFORM   P1000_REQ_DOC_READ_INS.
    EXIT.
  ENDIF.

  IF NOT ZSREQHD-EBELN IS INITIAL.      " P/O가 입력된 경?
* P/O NO에 Count
    SELECT COUNT( * ) INTO  W_COUNT
          FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
          ON    R~ZFREQNO    EQ   I~ZFREQNO
          WHERE R~EBELN   EQ ZSREQHD-EBELN
          AND   R~ZFAMDNO EQ '00000'
          AND   I~ZFAMDNO EQ '00000'.

    CASE W_COUNT.
      WHEN 0.     MESSAGE E172 WITH ZSREQHD-EBELN.
      WHEN 1.
        SELECT R~ZFREQNO I~ZFINSEQ I~ZFAMDNO
                 INTO (ZSREQHD-ZFREQNO, ZSREQHD-ZFINSEQ,
                       ZSREQHD-ZFAMDNO)
                 UP TO 1 ROWS
                 FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                 ON    R~ZFREQNO    EQ   I~ZFREQNO
                 WHERE R~EBELN   EQ ZSREQHD-EBELN
                 AND   R~ZFAMDNO EQ '00000'
                 AND   I~ZFAMDNO EQ '00000'.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_INS_DOC_DISP_SELECT1.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* 수입의뢰 문서 READ PERFORM ?
    PERFORM   P1000_REQ_DOC_READ_INS.
    EXIT.
  ENDIF.

  IF ZSREQHD-ZFINSEQ IS INITIAL.      " 회차가 미입력되었을 경우.
* P/O NO에 Count
    SELECT COUNT( * ) INTO  W_COUNT
          FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
          ON    R~ZFREQNO    EQ   I~ZFREQNO
          WHERE R~ZFREQNO EQ ZSREQHD-ZFREQNO
          AND   R~ZFAMDNO EQ '00000'
          AND   I~ZFAMDNO EQ '00000'.

    CASE W_COUNT.
      WHEN 0.     MESSAGE E548 WITH ZSREQHD-ZFREQNO.
      WHEN 1.
        SELECT R~ZFREQNO I~ZFINSEQ I~ZFAMDNO
                 INTO (ZSREQHD-ZFREQNO, ZSREQHD-ZFINSEQ,
                       ZSREQHD-ZFAMDNO)
                 UP TO 1 ROWS
                 FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                 ON    R~ZFREQNO    EQ   I~ZFREQNO
                 WHERE R~ZFREQNO EQ ZSREQHD-ZFREQNO
                 AND   R~ZFAMDNO EQ '00000'
                 AND   I~ZFAMDNO EQ '00000'.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_INS_DOC_DISP_SELECT3.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* 수입의뢰 문서 READ PERFORM ?
    PERFORM   P1000_REQ_DOC_READ_INS.
    EXIT.

  ENDIF.
* 수입의뢰 문서 READ PERFORM ?
  PERFORM   P1000_REQ_DOC_READ_INS.

ENDMODULE.                 " READ_DOC_SCR4200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR4200  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR4200 INPUT.
  IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
    W_STATUS = C_REQ_U.
    SET SCREEN 4101.  LEAVE TO SCREEN 4101.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR4200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR4300  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR4300 INPUT.

  IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
    W_STATUS = C_REQ_D.
    SET SCREEN 4101.  LEAVE TO SCREEN 4101.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR4300  INPUT
*&---------------------------------------------------------------------*
*&      Module  TRNAS_METHOD_SCR4102  INPUT
*&---------------------------------------------------------------------*
MODULE TRNAS_METHOD_SCR4102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTINS-ZFTRANS IS INITIAL.   " OR ZTINS-ZFTRANS EQ 'B'.
*     MESSAGE E176.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINS' 'ZFTRANS'.
  ENDIF.

  IF W_ZFTRANS NE ZTINS-ZFTRANS.
* 부보조건 TEXT SET
    PERFORM  P2000_TRANS_METHOD_SET.
  ENDIF.

* Amend시 변경 내?
  IF SY-DYNNR EQ '4502'.
    IF ZTINS-ZFTRANS EQ ZTINS_OLD-ZFTRANS.
      IF ZTINSSG3-ZFCARNU EQ ZTINSSG3_OLD-ZFCARNU AND
         ZTINSSG3-ZFCARNM EQ ZTINSSG3_OLD-ZFCARNM.
        CLEAR : ZTINS-ZFTMYN.
      ELSE.
        ZTINS-ZFTMYN = 'X'.
      ENDIF.
      IF IT_ZSINSAGR[] NE IT_ZSINSAGR_OLD[].
        ZTINS-ZFCDYN = 'X'.
      ELSE.
        CLEAR : ZTINS-ZFCDYN.
      ENDIF.
    ELSE.
      IF ZTINSSG3-ZFCARNU EQ ZTINSSG3_OLD-ZFCARNU AND
         ZTINSSG3-ZFCARNM EQ ZTINSSG3_OLD-ZFCARNM.
        CLEAR : ZTINS-ZFTMYN.
      ELSE.
        ZTINS-ZFTMYN = 'X'.
      ENDIF.
      IF IT_ZSINSAGR[] NE IT_ZSINSAGR_OLD[].
        ZTINS-ZFCDYN = 'X'.
      ELSE.
        CLEAR : ZTINS-ZFCDYN.
      ENDIF.
    ENDIF.
  ENDIF.

  W_ZFTRANS = ZTINS-ZFTRANS.

ENDMODULE.                 " TRNAS_METHOD_SCR4102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_FIELD_NAME_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE GET_FIELD_NAME_SCRCOM INPUT.
  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der

ENDMODULE.                 " GET_FIELD_NAME_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR4400  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR4400 INPUT.

  IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
    W_STATUS = C_OPEN_C.
    PERFORM   P3000_INIT_CURR_SET.
    SET SCREEN 4101.  LEAVE TO SCREEN 4101.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR4400  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_MATGB_PORT_CHECK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE GET_MATGB_PORT_CHECK_SCRCOM INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Local이 아닐 경우...
  CHECK : ZTREQHD-ZFMATGB NE '2'.
  IF ZTREQHD-ZFREQTY EQ 'LC'.
    IF  ZTREQHD-ZFSHCU IS INITIAL.
      MESSAGE E167 WITH 'Shipping country code'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " GET_MATGB_PORT_CHECK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_MATGB_PORT_CHECK1_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE GET_MATGB_PORT_CHECK1_SCRCOM INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Local이 아닐 경우...
  CHECK : ZTREQHD-ZFMATGB NE '2'.
  IF ZTREQHD-ZFREQTY EQ 'LC'.
    IF  ZTREQHD-ZFSPRTC IS INITIAL.
      MESSAGE E167 WITH 'Shipping port'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " GET_MATGB_PORT_CHECK1_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_MATGB_PORT_CHECK2_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE GET_MATGB_PORT_CHECK2_SCRCOM INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Local이 아닐 경우...
  CHECK : ZTREQHD-ZFMATGB NE '2'.
  IF ZTREQHD-ZFREQTY EQ 'LC'.
    IF  ZTREQHD-ZFARCU IS INITIAL.
      MESSAGE E167 WITH 'Arrival country code'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " GET_MATGB_PORT_CHECK2_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_MATGB_PORT_CHECK3_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE GET_MATGB_PORT_CHECK3_SCRCOM INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Local이 아닐 경우...
  CHECK : ZTREQHD-ZFMATGB NE '2'.
  IF ZTREQHD-ZFREQTY EQ 'LC'.
    IF  ZTREQHD-ZFAPRTC IS INITIAL.
      MESSAGE E167 WITH 'Shipping port'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " GET_MATGB_PORT_CHECK3_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCR4600  INPUT
*&---------------------------------------------------------------------*
MODULE READ_DOC_SCR4600 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZSREQHD-EBELN IS INITIAL AND     " P/O NO를 입력하지 않을 경?
     ZSREQHD-ZFREQNO IS INITIAL AND   " 관리번호가 입력하지 않을 경?
     ZSREQHD-ZFOPNNO IS INITIAL.      " 문서번호가 입력하지 않을 경?
    MESSAGE E066.
  ENDIF.
*
  IF NOT ZSREQHD-ZFOPNNO IS INITIAL.      " 문서번호가 입력된 경?
    IF ZSREQHD-ZFAMDNO IS INITIAL. " Amend Seq.
* 문서 승인번?
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                        ON    R~ZFREQNO    EQ   I~ZFREQNO
                        WHERE R~ZFOPNNO EQ ZSREQHD-ZFOPNNO
                        AND   R~ZFAMDNO EQ '00000'
                        AND   I~ZFAMDNO NE '00000'.
    ELSE.
* 문서 승인번?
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                        ON    R~ZFREQNO    EQ   I~ZFREQNO
                        WHERE R~ZFOPNNO EQ ZSREQHD-ZFOPNNO
                        AND   R~ZFAMDNO EQ '00000'
                        AND   I~ZFAMDNO EQ ZSREQHD-ZFAMDNO.
    ENDIF.

    CASE W_COUNT.
      WHEN 0.     MESSAGE E171 WITH ZSREQHD-ZFOPNNO.
      WHEN 1.
        IF ZSREQHD-ZFAMDNO IS INITIAL. " Amend Seq.
          SELECT R~ZFREQNO I~ZFINSEQ I~ZFAMDNO
                   INTO (ZSREQHD-ZFREQNO, ZSREQHD-ZFINSEQ,
                         ZSREQHD-ZFAMDNO) UP TO 1 ROWS
                   FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                   ON    R~ZFREQNO    EQ   I~ZFREQNO
                   WHERE R~ZFOPNNO EQ ZSREQHD-ZFOPNNO
                   AND   R~ZFAMDNO EQ '00000'
                   AND   I~ZFAMDNO NE '00000'.
            EXIT.
          ENDSELECT.
        ELSE.
          SELECT R~ZFREQNO I~ZFINSEQ I~ZFAMDNO
                   INTO (ZSREQHD-ZFREQNO, ZSREQHD-ZFINSEQ,
                         ZSREQHD-ZFAMDNO) UP TO 1 ROWS
                   FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                   ON    R~ZFREQNO    EQ   I~ZFREQNO
                   WHERE R~ZFOPNNO EQ ZSREQHD-ZFOPNNO
                   AND   R~ZFAMDNO EQ '00000'
                   AND   I~ZFAMDNO EQ ZSREQHD-ZFAMDNO.
            EXIT.
          ENDSELECT.
        ENDIF.
      WHEN OTHERS.
        PERFORM P2000_INS_AMD_DISP_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* 수입의뢰 문서 READ PERFORM ?
    PERFORM   P1000_REQ_DOC_READ_INS.
    EXIT.
  ENDIF.
* P/O NO에 Count
  IF NOT ZSREQHD-EBELN IS INITIAL.     " P/O가 입력가 입력된 경?
    IF ZSREQHD-ZFAMDNO IS INITIAL. " Amend Seq.
      SELECT COUNT( * ) INTO  W_COUNT
            FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
            ON    R~ZFREQNO    EQ   I~ZFREQNO
            WHERE R~EBELN   EQ ZSREQHD-EBELN
            AND   R~ZFAMDNO EQ '00000'
            AND   I~ZFAMDNO NE '00000'.
    ELSE.
      SELECT COUNT( * ) INTO  W_COUNT
         FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
         ON    R~ZFREQNO    EQ   I~ZFREQNO
         WHERE R~EBELN   EQ ZSREQHD-EBELN
         AND   R~ZFAMDNO EQ '00000'
         AND   I~ZFAMDNO EQ ZSREQHD-ZFAMDNO.
    ENDIF.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E172 WITH ZSREQHD-EBELN.
      WHEN 1.
        IF ZSREQHD-ZFAMDNO IS INITIAL. " Amend Seq.
          SELECT R~ZFREQNO I~ZFINSEQ I~ZFAMDNO
                      INTO (ZSREQHD-ZFREQNO, ZSREQHD-ZFINSEQ,
                           ZSREQHD-ZFAMDNO) UP TO 1 ROWS
                      FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                      ON    R~ZFREQNO    EQ   I~ZFREQNO
                      WHERE R~EBELN   EQ ZSREQHD-EBELN
                      AND   R~ZFAMDNO EQ '00000'
                      AND   I~ZFAMDNO NE '00000'.
            EXIT.
          ENDSELECT.
        ELSE.
          SELECT R~ZFREQNO I~ZFINSEQ I~ZFAMDNO
                     INTO (ZSREQHD-ZFREQNO, ZSREQHD-ZFINSEQ,
                          ZSREQHD-ZFAMDNO) UP TO 1 ROWS
                     FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                     ON    R~ZFREQNO    EQ   I~ZFREQNO
                     WHERE R~EBELN   EQ ZSREQHD-EBELN
                     AND   R~ZFAMDNO EQ '00000'
                     AND   I~ZFAMDNO EQ ZSREQHD-ZFAMDNO.
            EXIT.
          ENDSELECT.
        ENDIF.
      WHEN OTHERS.
        PERFORM P2000_INS_AMD_DISP_SELECT1.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* 수입의뢰 문서 READ PERFORM ?
    PERFORM   P1000_REQ_DOC_READ_INS.
    EXIT.
  ENDIF.

* 수입의뢰 NO에 Count
  IF NOT ZSREQHD-ZFREQNO IS INITIAL.     " 수입의뢰 입력가 입력된 경우.
    IF ZSREQHD-ZFAMDNO IS INITIAL. " Amend Seq.
      SELECT COUNT( * ) INTO  W_COUNT
            FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
            ON    R~ZFREQNO    EQ   I~ZFREQNO
            WHERE R~ZFREQNO    EQ ZSREQHD-ZFREQNO
            AND   R~ZFAMDNO    EQ '00000'
            AND   I~ZFAMDNO    NE '00000'.
    ELSE.
      SELECT COUNT( * ) INTO  W_COUNT
         FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
         ON    R~ZFREQNO    EQ   I~ZFREQNO
         WHERE R~ZFREQNO    EQ ZSREQHD-ZFREQNO
         AND   R~ZFAMDNO    EQ '00000'
         AND   I~ZFAMDNO    EQ ZSREQHD-ZFAMDNO.
    ENDIF.

    CASE W_COUNT.
      WHEN 0.     MESSAGE E548 WITH ZSREQHD-ZFREQNO.
      WHEN 1.
        IF ZSREQHD-ZFAMDNO IS INITIAL. " Amend Seq.
          SELECT R~ZFREQNO I~ZFAMDNO I~ZFINSEQ
                      INTO (ZSREQHD-ZFREQNO, ZSREQHD-ZFAMDNO,
                            ZSREQHD-ZFINSEQ) UP TO 1 ROWS
                      FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                      ON    R~ZFREQNO   EQ   I~ZFREQNO
                      WHERE R~ZFREQNO   EQ ZSREQHD-ZFREQNO
                      AND   R~ZFAMDNO   EQ '00000'
                      AND   I~ZFAMDNO   NE '00000'.
            EXIT.
          ENDSELECT.
        ELSE.
          SELECT R~ZFREQNO I~ZFAMDNO I~ZFINSEQ
                     INTO (ZSREQHD-ZFREQNO, ZSREQHD-ZFAMDNO,
                           ZSREQHD-ZFINSEQ) UP TO 1 ROWS
                     FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                     ON    R~ZFREQNO    EQ   I~ZFREQNO
                     WHERE R~ZFREQNO    EQ ZSREQHD-ZFREQNO
                     AND   R~ZFAMDNO    EQ '00000'
                     AND   I~ZFAMDNO    EQ ZSREQHD-ZFAMDNO.
            EXIT.
          ENDSELECT.
        ENDIF.
      WHEN OTHERS.
        PERFORM P2000_INS_AMD_DISP_SELECT3.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* 수입의뢰 문서 READ PERFORM ?
    PERFORM   P1000_REQ_DOC_READ_INS.
    EXIT.
  ENDIF.

* 연번.
  IF ZSREQHD-ZFAMDNO IS INITIAL. " Amend seq.
    SELECT COUNT( * ) INTO  W_COUNT
           FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
           ON    R~ZFREQNO    EQ   I~ZFREQNO
           WHERE R~ZFREQNO EQ ZSREQHD-ZFREQNO
           AND   R~ZFAMDNO EQ '00000'
           AND   I~ZFAMDNO NE '00000'.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E180 WITH ZSREQHD-ZFREQNO.
      WHEN 1.
        SELECT R~ZFREQNO I~ZFAMDNO
               INTO (ZSREQHD-ZFREQNO,
                    ZSREQHD-ZFAMDNO) UP TO 1 ROWS
               FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
               ON    R~ZFREQNO    EQ   I~ZFREQNO
               WHERE R~ZFREQNO EQ ZSREQHD-ZFREQNO
               AND   R~ZFAMDNO EQ '00000'
               AND   I~ZFAMDNO NE '00000'.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM   P2000_AMD_DOC_DISP_SELECT3.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
  ENDIF.

* 수입의뢰 문서 READ PERFORM ?
  PERFORM   P1000_REQ_DOC_READ_INS.

ENDMODULE.                 " READ_DOC_SCR4600  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR4600  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR4600 INPUT.
  IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
    CASE SY-TCODE.
      WHEN 'ZIM46'.    W_STATUS = C_REQ_U.
      WHEN 'ZIM47'.    W_STATUS = C_REQ_D.
      WHEN 'ZIM48'.    W_STATUS = C_OPEN_C.
    ENDCASE.
    IF SY-TCODE EQ 'ZIM48'.
      PERFORM   P3000_INIT_CURR_SET.
    ENDIF.
    SET SCREEN 4501.  LEAVE TO SCREEN 4501.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR4600  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZTTTSG5_GET_LINE_SCR0143  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZTTTSG5_GET_LINE_SCR0143 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0143-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " ZTTTSG5_GET_LINE_SCR0143  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZSTTSG5_UPDATE_SCR0143  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZSTTSG5_UPDATE_SCR0143 INPUT.

  READ TABLE IT_ZSTTSG5  WITH KEY ZFLSG5  = ZSTTSG5-ZFLSG5
                         BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING   ZSTTSG5 TO IT_ZSTTSG5.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSTTSG5 INDEX W_TABIX.
  ELSE.
    IT_ZSTTSG5-ZFLSG5 = TC_0143-CURRENT_LINE * 10.
    APPEND IT_ZSTTSG5.
  ENDIF.


ENDMODULE.                 " ZSTTSG5_UPDATE_SCR0143  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0143_MARK_TC_0143  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0143_MARK_TC_0143 INPUT.

  READ TABLE IT_ZSTTSG5  WITH KEY ZFLSG5  = ZSTTSG5-ZFLSG5
                         BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
    MOVE-CORRESPONDING   ZSTTSG5   TO IT_ZSTTSG5.
    IF NOT ( W_ROW_MARK1 IS INITIAL ).
      IT_ZSTTSG5-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSTTSG5-ZFMARK.
    ENDIF.
    MODIFY IT_ZSTTSG5 INDEX W_TABIX.
  ENDIF.


ENDMODULE.                 " SET_SCR0143_MARK_TC_0143  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0143  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0143 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1' OR
     SY-UCOMM EQ 'MKA2' OR SY-UCOMM EQ 'DEL2'.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1' OR
         SY-UCOMM EQ 'MKL2' OR SY-UCOMM EQ 'UND2'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'. " 품목 삭제 / 취?
      IF OK-CODE EQ 'DEL1' AND SY-TCODE NE 'ZIM01'.
        PERFORM  P2000_ITEM_DEL_CHECK.
      ENDIF.
      LOOP AT IT_ZSREQIT   WHERE ZFMARK NE SPACE.
        DELETE IT_ZSREQIT   INDEX SY-TABIX.
      ENDLOOP.
      TC_0103-TOP_LINE = 1.
    WHEN 'DEL2' OR 'UND2'.   " 상품용역명세 삭제 / 취?
      LOOP AT IT_ZSTTSG5     WHERE ZFMARK NE SPACE.
        DELETE IT_ZSTTSG5   INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해?
      LOOP AT IT_ZSREQIT.
        IT_ZSREQIT-ZFMARK = W_MARK.   MODIFY IT_ZSREQIT.
      ENDLOOP.
    WHEN 'MKA2' OR 'MKL2'. " 전체선택 or 선택해?
      LOOP AT IT_ZSTTSG5.
        IT_ZSTTSG5-ZFMARK = W_MARK.   MODIFY IT_ZSTTSG5.
      ENDLOOP.
    WHEN 'PORE'.
      LOOP  AT  IT_ZSREQIT.
        PERFORM   P2000_REFRESH_PRICE.
      ENDLOOP.
    WHEN 'REF1'.           " 품목 Refresh
*>> 2001.07.04 NHJ 변경( PO ITEM 내역 REFRESH )
      PERFORM   P2000_REFRESH_ITEM.
*      PERFORM   P2000_REFRESH_RECORD_CHK.
*     REFRESH : IT_ZSREQIT.
*     LOOP AT IT_ZSREQIT_ORG.
*        CLEAR : IT_ZSREQIT.
*        MOVE-CORRESPONDING   IT_ZSREQIT_ORG   TO   IT_ZSREQIT.
*        APPEND IT_ZSREQIT.
*     ENDLOOP.
    WHEN 'REF2'.           " 품목명세 Refresh
      REFRESH : IT_ZSTTSG5.
      LOOP AT IT_ZSTTSG5_ORG.
        CLEAR : IT_ZSTTSG5.
        MOVE-CORRESPONDING   IT_ZSTTSG5_ORG   TO   IT_ZSTTSG5.
        APPEND IT_ZSTTSG5.
      ENDLOOP.
*>> 2002.12.18 JSY 추가 청구자 조회.
    WHEN 'SU01'.
      W_COUNT = 0.
      LOOP AT IT_ZSREQIT  WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        ADD   1    TO    W_COUNT.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.         MESSAGE S962.   EXIT.
        WHEN 1.
         PERFORM P2000_REQUESTER_DISPLAY USING IT_ZSREQIT-EBELN
                                               IT_ZSREQIT-EBELP.
        WHEN OTHERS.    MESSAGE S965.   EXIT.
      ENDCASE.
*>> 2002.12.18 JSY 추가 P/O 조회.
    WHEN 'PODP'.
      W_COUNT = 0.
      LOOP AT IT_ZSREQIT  WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        ADD   1    TO    W_COUNT.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.         MESSAGE S962.   EXIT.
        WHEN 1.
            MOVE-CORRESPONDING  IT_ZSREQIT  TO  W_ZSREQIT.
            PERFORM P2000_PO_ITEM_DISPLAY.
        WHEN OTHERS.    MESSAGE S965.   EXIT.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

* INDEX 수정 작?
  PERFORM   P2000_IT_ZSTTSG5_UPDATE.

*-----------------------------------------------------------------------
* 최종 개설금액 자동 갱?
*-----------------------------------------------------------------------
  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

ENDMODULE.                 " USER_COMMAND_SCR0143  INPUT

*&---------------------------------------------------------------------*
*&      Module  PASSWD_CHECK_SCR0145  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PASSWD_CHECK_SCR0145 INPUT.

* G_PASSWORD1 = F_PASSWD.

* CALL 'XXPASS'
*   ID 'CODE' FIELD G_PASSWORD1
*   ID 'CODX' FIELD USR02-BCODE
*   ID 'NAME' FIELD USR02-BNAME
*   ID 'VERS' FIELD USR02-CODVN.
*
* ZTTTHD-ZFPASSWD = USR02-BCODE.
*
* IF USR02-BCODE EQ '0000000000000000' OR
*    USR02-BCODE EQ '2020202020202020'.
*    MESSAGE E167 WITH 'PassWord'.
* ENDIF.
*


ENDMODULE.                 " PASSWD_CHECK_SCR0145  INPUT

*&---------------------------------------------------------------------*
*&      Module  BANK_OPERATION_SCR0114  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BANK_OPERATION_SCR0114 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTTTHD-ZFBUSFUN EQ '2AJ'.
    IF ZTTTHD-ZFCOMMTY IS INITIAL.
      MESSAGE E167 WITH 'Payment commission type'.
    ENDIF.
  ELSE.
    IF NOT ZTTTHD-ZFCOMMTY IS INITIAL.
      MESSAGE E215.
    ENDIF.
  ENDIF.

ENDMODULE.                 " BANK_OPERATION_SCR0114  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZSINSSG2_UPDATE_SCR4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZSINSSG2_UPDATE_SCR4104 INPUT.
  READ TABLE IT_ZSINSSG2 WITH KEY ZFLSG2  = ZSINSSG2-ZFLSG2
                         BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING   ZSINSSG2  TO IT_ZSINSSG2.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSINSSG2 INDEX W_TABIX.
  ELSE.
    IT_ZSINSSG2-ZFLSG2 = TC_4104-CURRENT_LINE * 10.
    APPEND IT_ZSINSSG2.
  ENDIF.

ENDMODULE.                 " ZSINSSG2_UPDATE_SCR4104  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR4104 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'. " 품목 삭?
      LOOP AT IT_ZSINSSG2  WHERE ZFMARK NE SPACE.
        DELETE IT_ZSINSSG2  INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해?
      LOOP AT IT_ZSINSSG2.
        IT_ZSINSSG2-ZFMARK = W_MARK.   MODIFY IT_ZSINSSG2.
      ENDLOOP.
    WHEN 'REF1'.           " 품목 Refresh
      REFRESH : IT_ZSINSSG2.
      LOOP AT IT_ZSINSSG2_ORG.
        MOVE-CORRESPONDING   IT_ZSINSSG2_ORG   TO   IT_ZSINSSG2.
        APPEND IT_ZSINSSG2.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

* 품목명세 INDEX 수정 작?
  PERFORM   P2000_IT_ZSINSSG2_UPDATE.


ENDMODULE.                 " USER_COMMAND_SCR4104  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_TAG7433_SCR4105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_TAG7433_SCR4105 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  READ TABLE IT_ZSINSAGR INDEX  TC_4105-CURRENT_LINE.
*  READ TABLE IT_ZSINSAGR WITH KEY ZFLAGR  = ZSINSAGR-ZFLAGR
*                         BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING   ZSINSAGR  TO IT_ZSINSAGR.
  IF NOT ZSINSAGR-ZFINSCD IS INITIAL AND ZSINSAGR-ZFCNCDNM IS INITIAL.
    PERFORM   GET_DD07T_SELECT USING    'ZDINSCD'  IT_ZSINSAGR-ZFINSCD
                               CHANGING  IT_ZSINSAGR-ZFCNCDNM.
  ENDIF.

*  IF NOT ZSINSAGR-ZFCNCD IS INITIAL AND ZSINSAGR-ZFCNCDNM IS INITIAL.
*     PERFORM   GET_DD07T_SELECT USING    'ZDCNCD'  IT_ZSINSAGR-ZFCNCD
*                                CHANGING  IT_ZSINSAGR-ZFCNCDNM.
*  ENDIF.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSINSAGR INDEX W_TABIX.
  ELSE.
    IT_ZSINSAGR-ZFLAGR = TC_4105-CURRENT_LINE * 10.
    APPEND IT_ZSINSAGR.
  ENDIF.

ENDMODULE.                 " GET_TAG7433_SCR4105  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_SEGMENT5_SCR4105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_SEGMENT5_SCR4105 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  READ TABLE IT_ZSINSSG5 WITH KEY ZFLSG5  = ZSINSSG5-ZFLSG5
                         BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING   ZSINSSG5  TO IT_ZSINSSG5.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSINSSG5 INDEX W_TABIX.
  ELSE.
    IT_ZSINSSG5-ZFLSG5 = TC_4104-CURRENT_LINE * 10.
    APPEND IT_ZSINSSG5.
  ENDIF.

ENDMODULE.                 " CHECK_SEGMENT5_SCR4105  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR4105  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR4105 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1' OR
     SY-UCOMM EQ 'MKA2' OR SY-UCOMM EQ 'DEL2'.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1' OR
         SY-UCOMM EQ 'MKL2' OR SY-UCOMM EQ 'UND2'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'. " 품목 삭제 / 취?
      LOOP AT IT_ZSINSAGR  WHERE ZFMARK NE SPACE.
        DELETE IT_ZSINSAGR  INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'DEL2' OR 'UND2'.   " 상품용역명세 삭제 / 취?
      LOOP AT IT_ZSINSSG5    WHERE ZFMARK NE SPACE.
        DELETE IT_ZSINSSG5  INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해?
      LOOP AT IT_ZSINSAGR.
        IT_ZSINSAGR-ZFMARK = W_MARK.   MODIFY IT_ZSINSAGR.
      ENDLOOP.
    WHEN 'MKA2' OR 'MKL2'. " 전체선택 or 선택해?
      LOOP AT IT_ZSINSSG5.
        IT_ZSINSSG5-ZFMARK = W_MARK.   MODIFY IT_ZSINSSG5.
      ENDLOOP.
    WHEN 'REF1'.           " Refresh
      REFRESH : IT_ZSINSAGR.
      LOOP AT IT_ZSINSAGR_ORG.
        CLEAR : IT_ZSINSAGR.
        MOVE-CORRESPONDING   IT_ZSINSAGR_ORG   TO   IT_ZSINSAGR.
        APPEND IT_ZSINSAGR.
      ENDLOOP.
    WHEN 'REF2'.           " Refresh
      REFRESH : IT_ZSINSSG5.
      LOOP AT IT_ZSINSSG5_ORG.
        CLEAR : IT_ZSINSSG5.
        MOVE-CORRESPONDING   IT_ZSINSSG5_ORG   TO   IT_ZSINSSG5.
        APPEND IT_ZSINSSG5.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

* INDEX 수정 작?
  PERFORM   P2000_IT_ZSINSAGR_UPDATE.
  PERFORM   P2000_IT_ZSINSSG5_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR4105  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_MARK_TC4105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_MARK_TC4105 INPUT.

  READ TABLE IT_ZSINSAGR WITH KEY ZFLAGR  = ZSINSAGR-ZFLAGR
                         BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF NOT ( W_ROW_MARK IS INITIAL ).
    IT_ZSINSAGR-ZFMARK = 'X'.
  ELSE.
    CLEAR : IT_ZSINSAGR-ZFMARK.
  ENDIF.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSINSAGR INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_MARK_TC4105  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_MARK_TC4105_1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_MARK_TC4105_1 INPUT.

  READ TABLE IT_ZSINSSG5 WITH KEY ZFLSG5  = ZSINSSG5-ZFLSG5
                         BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF NOT ( W_ROW_MARK1 IS INITIAL ).
    IT_ZSINSSG5-ZFMARK = 'X'.
  ELSE.
    CLEAR : IT_ZSINSSG5-ZFMARK.
  ENDIF.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSINSSG5 INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_MARK_TC4105_1  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_MARK_TC4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_MARK_TC4104 INPUT.
  READ TABLE IT_ZSINSSG2 WITH KEY ZFLSG2  = ZSINSSG2-ZFLSG2
                         BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF NOT ( W_ROW_MARK IS INITIAL ).
    IT_ZSINSSG2-ZFMARK = 'X'.
  ELSE.
    CLEAR : IT_ZSINSSG2-ZFMARK.
  ENDIF.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSINSSG2 INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_MARK_TC4104  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_LC_NO_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_LC_NO_SCR0102 INPUT.

  CHECK : W_STATUS EQ C_OPEN_C.

  IF ZTREQST-ZFOPNDT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQST' 'ZFOPNDT'.
*     MESSAGE E167 WITH 'Opening date'.
  ENDIF.

*>> DATA MIGRATION 으로 인한 주석처리.
*  IF ZTREQHD-KURSF IS INITIAL.
*
*    SPOP-TEXTLINE1 = 'Exchange rate를 입력하지 않았습니다.'.
*
*    PERFORM    P2000_GET_EXCHANGE_RATE     USING ZTREQHD-WAERS
*                                                 ZTREQST-ZFOPNDT
*                                        CHANGING ZTREQHD-KURSF
*                                                 ZTREQHD-FFACT.
*  ENDIF.
*
*  IF ZTREQHD-FFACT IS INITIAL.
*    MESSAGE W637.
*    ZTREQHD-FFACT = 1.
*  ENDIF.

  IF ZTREQST-ZFOPNNO IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQST' 'ZFOPNNO'.
*     MESSAGE E167 WITH 'L/C-approval No'.
  ELSE.
    SELECT COUNT( * ) INTO W_CNT FROM ZTREQST
                      WHERE ZFOPNNO EQ ZTREQST-ZFOPNNO
                      AND   ZFREQNO NE ZTREQHD-ZFREQNO.
    IF W_CNT GT 0.
      MESSAGE W222 WITH ZTREQST-ZFOPNNO W_CNT.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_LC_NO_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_PO_NO_INPUT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE READ_PO_NO_INPUT_SCRCOM INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

* 이미 SELECT 성공시...
  IF W_READ_CHK = 'Y'.   EXIT.   ENDIF.

* 입력값 CHECK.
  IF ZSREQHD-EBELN IS INITIAL.           " P/O NO를 입력하지 않을 경?
    IF ZSREQHD-ZFREQNO IS INITIAL.
      MESSAGE E167 WITH 'Purchasing document number'.
    ELSE.
      EXIT.
    ENDIF.
  ENDIF.
* P/O NO에 Count
  SELECT COUNT( * ) INTO  W_COUNT
                    FROM  ZVREQHD_ST
                    WHERE EBELN EQ ZSREQHD-EBELN
                    AND    ZFAMDNO EQ '00000'.
  CASE W_COUNT.
    WHEN 0.     MESSAGE E064 WITH ZSREQHD-EBELN.
    WHEN 1.
      SELECT ZFREQNO INTO ZSREQHD-ZFREQNO UP TO 1 ROWS
                     FROM ZVREQHD_ST
                     WHERE EBELN EQ ZSREQHD-EBELN
                     AND   ZFAMDNO EQ '00000'.
        EXIT.
      ENDSELECT.
    WHEN OTHERS.
      PERFORM P2000_DOC_ITEM_SELECT.
      IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
      PERFORM P2000_SEARCH_FIELD_MOVE.
  ENDCASE.

* 수입의뢰 문서 READ PERFORM ?
  PERFORM   P1000_REQ_DOC_READ.

ENDMODULE.                 " READ_PO_NO_INPUT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_REQNO_INPUT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE READ_REQNO_INPUT_SCRCOM INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

* 이미 SELECT 성공시...
  IF W_READ_CHK = 'Y'.   EXIT.   ENDIF.

* 입력값 CHECK.
  IF ZSREQHD-ZFREQNO IS INITIAL.           "
    IF ZSREQHD-EBELN IS INITIAL.           " P/O NO를 입력하지 않을 경?
      MESSAGE E167 WITH 'Import request No'.
    ELSE.
      EXIT.
    ENDIF.
  ENDIF.

* 수입의뢰 문서 READ PERFORM ?
  PERFORM   P1000_REQ_DOC_READ.

ENDMODULE.                 " READ_REQNO_INPUT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR0191  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR0191 INPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZSRECST-ZTERM'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF SCREEN-INPUT EQ '1'.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZSRECST-ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ELSE.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZSRECST-ZTERM
              I_XSHOW       = 'X'
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ENDIF.

  IF SY-SUBRC NE 0.
*   message e177 with ekko-zterm.
    MESSAGE S177(06) WITH ZSRECST-ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    ZSRECST-ZTERM = T052-ZTERM.
  ENDIF.

ENDMODULE.                 " HELP_ZTERM_SCR0191  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_ZTERM_NAME_SCR0191  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_ZTERM_NAME_SCR0191 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  IF ZSRECST-ZTERM IS INITIAL.
    EXIT.
  ENDIF.
  REFRESH : ZBTXT_LINES.

  CALL FUNCTION 'FI_PRINT_ZTERM'                            "80864
       EXPORTING
            I_ZTERM         = ZSRECST-ZTERM
            I_LANGU         = SY-LANGU
            I_XT052U        = 'X'                           "187501
            I_T052          = T052
       TABLES
            T_ZTEXT         = ZBTXT_LINES
       EXCEPTIONS
            ZTERM_NOT_FOUND = 01.

  IF SY-SUBRC NE 0.
    MESSAGE E410(FH) WITH SY-MSGV1 RAISING ZTERM_NOT_FOUND.
  ENDIF.
ENDMODULE.                 " GET_ZTERM_NAME_SCR0191  INPUT

*&---------------------------------------------------------------------*
*&      Module  IT_ZSRECST_UPDATE_SCR0191  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSRECST_UPDATE_SCR0191 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

* Internal Table Read
  READ TABLE IT_ZSRECST   WITH KEY ZFCSQ = ZSRECST-ZFCSQ.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSRECST   TO IT_ZSRECST.

  IT_ZSRECST-ZFKRW = 'KRW'.


  IF IT_ZSRECST-ZFCKAMT EQ 0.
    IT_ZSRECST-ZFCKAMT = ZSRECST-ZFCAMT * ZSRECST-ZFEXRT.
* 통화키로 자리수 변경..( External ==> Internal )
    PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSRECST-ZFCKAMT
                                               IT_ZSRECST-ZFKRW.

  ENDIF.


* 필수 입력 검증.
  IF NOT IT_ZSRECST-ZFCAMT  IS INITIAL OR
     NOT IT_ZSRECST-ZFCKAMT IS INITIAL.
    IF NOT IT_ZSRECST-ZFCAMT IS INITIAL AND
           IT_ZSRECST-WAERS  IS INITIAL.
      MESSAGE E167 WITH 'Expense amount Currency'.
    ENDIF.
    IF IT_ZSRECST-ZFVEN IS INITIAL.    " Vendor
      MESSAGE E167 WITH 'Vendor'.
    ENDIF.
    IF IT_ZSRECST-ZFPAY IS INITIAL.    " 지불?
      MESSAGE E167 WITH 'Payee'.
    ENDIF.
    IF IT_ZSRECST-ZTERM IS INITIAL.    " Term of payment
      MESSAGE W167 WITH 'Term of payment'.
    ENDIF.
    IF IT_ZSRECST-MWSKZ IS INITIAL.    " TAX CODE
      MESSAGE W167 WITH 'Tax Code'.
    ENDIF.
    IF IT_ZSRECST-ZFWERKS IS INITIAL.    " 대표 PLANT
      MESSAGE W167 WITH 'Rep Plant'.
    ENDIF.
  ENDIF.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSRECST  INDEX W_TABIX.
  ELSE.
    IT_ZSBLCST-ZFCSQ   = ( TC_0191-CURRENT_LINE * 10 ).
    APPEND IT_ZSRECST.
  ENDIF.


ENDMODULE.                 " IT_ZSRECST_UPDATE_SCR0191  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_COST_CODE_SCR0191  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_COST_CODE_SCR0191 INPUT.

  PERFORM P2000_GET_ZTIMIMG08_DESC  USING     '003'
                                              ZSRECST-ZFCSCD
                                    CHANGING  ZSRECST-ZFCDNM.

ENDMODULE.                 " GET_COST_CODE_SCR0191  INPUT

*&---------------------------------------------------------------------*
*&      Module  BANK_CHG_CHECK_SCR0152  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BANK_CHG_CHECK_SCR0152 INPUT.
* if display mode, exit.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC' OR 'TT' OR 'DA' OR 'DP'.
      IF  ZTREQHD-ZFCHG  IS INITIAL.
        MESSAGE E167 WITH 'Banking Charge'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " BANK_CHG_CHECK_SCR0152  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0105  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0105 INPUT.

  IF SY-UCOMM EQ 'CADD'.    " ADDRESS UPDATE.
    PERFORM   P2000_REFRESH_ADDRESS   USING   ZTREQHD-ZFBENI.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR0105  INPUT
*&---------------------------------------------------------------------*
*&      Module  DOC_TYPE_CHK_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE DOC_TYPE_CHK_SCR0102 INPUT.
* if display mode, exit.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  PERFORM  P2000_LLCTY_CHECK   USING   ZSREQHD-ZFLLCTY.

ENDMODULE.                 " DOC_TYPE_CHK_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHK_CURRENCY_SCR0152  INPUT
*&---------------------------------------------------------------------*
MODULE CHK_CURRENCY_SCR0152 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  IF NOT ZTREQHD-ZFPKCUR IS INITIAL.
    IF ZTREQHD-ZFPKCUR NE ZTREQHD-WAERS.
      MESSAGE E266.
    ENDIF.
  ENDIF.

  IF NOT ZTREQHD-ZFHDCUR IS INITIAL.
    IF ZTREQHD-ZFHDCUR NE ZTREQHD-WAERS.
      MESSAGE E266.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHK_CURRENCY_SCR0152  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_INIT_VALUE_SCR0190  INPUT
*&---------------------------------------------------------------------*
MODULE READ_INIT_VALUE_SCR0190 INPUT.
*-----------------------------------------------------------------------
* ZTIMIMG04 TABLE SELECT
*-----------------------------------------------------------------------
  PERFORM   P1000_READ_ZTRECST.

  READ TABLE IT_ZSRECST INDEX 1.
  IF SY-SUBRC EQ 0.
    IF ZSREQHD-ZFVEN IS INITIAL.
      ZSREQHD-ZFVEN = IT_ZSRECST-ZFVEN.
    ENDIF.
    IF ZSREQHD-ZFPAY IS INITIAL.
      ZSREQHD-ZFPAY = IT_ZSRECST-ZFPAY.
    ENDIF.
  ENDIF.

  IF ZSREQHD-ZFVEN IS INITIAL.
    MESSAGE E167 WITH 'Vendor Code'.
  ELSE.
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = ZSREQHD-ZFVEN
         IMPORTING
              XLFA1          = W_LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    CASE SY-SUBRC.
      WHEN 01.     MESSAGE E025.
      WHEN 02.     MESSAGE E950.
      WHEN 03.     MESSAGE E020   WITH ZSREQHD-ZFVEN.
    ENDCASE.
  ENDIF.

  IF ZSREQHD-ZFPAY IS INITIAL.
    MESSAGE W272.
    MOVE  W_LFA1-LNRZA  TO IT_ZSRECST-ZFPAY.   " 지불?
  ENDIF.

  CALL FUNCTION 'READ_LFA1'
       EXPORTING
            XLIFNR         = ZSREQHD-ZFPAY
       IMPORTING
            XLFA1          = W_LFA1
       EXCEPTIONS
            KEY_INCOMPLETE = 01
            NOT_AUTHORIZED = 02
            NOT_FOUND      = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE E167   WITH 'Payee'.
    WHEN 02.     MESSAGE E950.
    WHEN 03.     MESSAGE E020   WITH ZSREQHD-ZFPAY.
  ENDCASE.


  IF ZSREQHD-ZFOCDT IS INITIAL.
    MESSAGE E167 WITH 'Occuring date'.
  ENDIF.

ENDMODULE.                 " READ_INIT_VALUE_SCR0190  INPUT

*&---------------------------------------------------------------------*
*&      Module  TC_0191_MARK_UPDATE_SCRF0191  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0191_MARK_UPDATE_SCRF0191 INPUT.

  READ TABLE IT_ZSRECST   INDEX TC_0191-CURRENT_LINE.
  W_OLD_SUBRC =  SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSRECST-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSRECST-LOEKZ.
  ENDIF.

  MOVE : W_ROW_MARK      TO IT_ZSRECST-ZFMARK.
  IF  W_OLD_SUBRC EQ 0.
    MODIFY IT_ZSRECST   INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " TC_0191_MARK_UPDATE_SCRF0191  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_REQ_DATE_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_REQ_DATE_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF NOT ZTREQST-ZFREQDT IS INITIAL.
    IF ZTREQST-ZFREQDT LT SY-DATUM.
      MESSAGE W291 WITH ZTREQST-ZFREQDT SY-DATUM.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_REQ_DATE_SCR0102  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_INPUT_VALUE_SCR0017  INPUT
*&---------------------------------------------------------------------*
MODULE GET_INPUT_VALUE_SCR0017 INPUT.

  IF ZSREQHD-WAERS EQ EKKO-WAERS.
    CLEAR ZSREQHD-ZFUPYN.
    MESSAGE E330.
    MOVE : 1      TO    ZSREQHD-WKURS.
  ELSE.
    MOVE : 'X'          TO  ZSREQHD-ZFUPYN.
    IF *ZTREQHD-ZFWAERS EQ ZSREQHD-WAERS.
      MESSAGE W330.
      MOVE : 1      TO    ZSREQHD-WKURS.   EXIT.
    ENDIF.
*>>> 변경?
    PERFORM SET_CURR_CONV_TO_EXTERNAL USING    *ZTREQHD-ZFOPAMT
                                               *ZTREQHD-ZFWAERS
                                               W_BAPICURR_FR.
*>>> 변경?
    PERFORM SET_CURR_CONV_TO_EXTERNAL USING    ZSREQHD-ZFOPAMT
                                               ZSREQHD-WAERS
                                               W_BAPICURR_TO.
*>>> 변경환율(New 개설금액 / Old 개설금액)
    W_BAPICURR_RATE = W_BAPICURR_TO / W_BAPICURR_FR.
    IF W_BAPICURR_RATE GE 10000.
      MESSAGE E329 WITH W_BAPICURR_RATE.
    ENDIF.

    ZSREQHD-WKURS = W_BAPICURR_RATE.
  ENDIF.

ENDMODULE.                 " GET_INPUT_VALUE_SCR0017  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0017  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0017 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'ENTR'.   EXIT.     " ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.   EXIT.
*       ANTWORT = 'Y'.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0017  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SUPPLY_NAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SUPPLY_NAME INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTREQHD-LLIEF IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.
  ENDIF.

  PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-LLIEF
                              CHANGING   W_LLIEF_NM.

ENDMODULE.                 " GET_SUPPLY_NAME  INPUT
*&---------------------------------------------------------------------*
*&      Module  MWSKZ_UPDATE_SCR0191  INPUT
*&---------------------------------------------------------------------*
MODULE MWSKZ_UPDATE_SCR0191 INPUT.

  PERFORM   P1000_READ_KONP   USING   IT_ZSRECST-MWSKZ
                                      IT_ZSRECST-KBETR
                                      IT_ZSRECST-KONWA.

ENDMODULE.                 " MWSKZ_UPDATE_SCR0191  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0191_MARK_TC_0191  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0191_MARK_TC_0191 INPUT.

  READ TABLE IT_ZSRECST  WITH KEY ZSRECST(18)  BINARY SEARCH.

  IF SY-SUBRC = 0.
*>>> 금?
*      IF IT_ZSRECST-ZFEXRT IS INITIAL.
*         IT_ZSRECST-ZFCKAMT = 0.
*     ELSE.
*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal )
*-----------------------------------------------------------------------
*        IT_ZSBLCST-ZFCKAMT = IT_ZSBLCST-ZFCAMT.
*         PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSBLCST-ZFCKAMT
*                                                    IT_ZSBLCST-KRW.

*         IF IT_ZSBLCST-WAERS NE 'KRW'.
*            IF IT_ZSBLCST-ZFCKAMT IS INITIAL.
*               IT_ZSBLCST-ZFCKAMT = ZTBL-ZFEXRT * IT_ZSBLCST-ZFCKAMT.
*            ENDIF.
*         ENDIF.
*      ENDIF.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSRECST-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSRECST-ZFMARK.
    ENDIF.
    MODIFY IT_ZSRECST  INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0191_MARK_TC_0191  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_CALC_LOCAL_AMOUNT_SCR4102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_CALC_LOCAL_AMOUNT_SCR4102 INPUT.

* Confirm
  CHECK  W_STATUS EQ C_OPEN_C.

  IF ZTINS-ZFINRT IS INITIAL.        " Insurance Rate
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINS' 'ZFINRT'.
  ENDIF.
  IF ZTINSRSP-ZFEXRT IS INITIAL.     " Exchange rate
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINSRSP' 'ZFEXRT'.
  ENDIF.
  W_ZFINAMT = ZTINS-ZFIVAMT * ( ZTINS-ZFINRT / 100 )
                            * ( ZTINS-ZFPEIV / 100 ).
       " Premium
  IF ZTINS-ZFINAMT IS INITIAL OR ZTINS-ZFINAMT NE W_ZFINAMT.
    ZTINS-ZFINAMT = W_ZFINAMT.
*    ZTINS-ZFINAMT = ZTINS-ZFIVAMT * ( ZTINS-ZFINRT / 100 )
*                                  * ( ZTINS-ZFPEIV / 100 ).
  ENDIF.
  IF ZTINSRSP-FFACT IS INITIAL.
    MESSAGE W637.
    ZTINSRSP-FFACT = 1.
  ENDIF.

  IF ZTINS-ZFKRWAMT IS INITIAL OR ZTINS-ZFKRWAMT NE ZTINS-ZFINAMT.
    PERFORM    SET_CURR_CONV_TO_EXTERNAL USING ZTINS-ZFINAMT
                                               ZTINS-ZFINAMTC
                                               ZTINS-ZFKRWAMT.
    IF ZTINSRSP-FFACT IS INITIAL.
      ZTINSRSP-FFACT  =  1.
    ENDIF.
    ZTINS-ZFKRWAMT = ZTINS-ZFKRWAMT *
                   ( ZTINSRSP-ZFEXRT / ZTINSRSP-FFACT ).

    PERFORM    SET_CURR_CONV_TO_INTERNAL USING ZTINS-ZFKRWAMT
                                               ZTINS-ZFKRW.
*>> 원단위 절사하기 위한 로직.
*    ZTINS-ZFKRWAMT = ( TRUNC( ZTINS-ZFKRWAMT * 10 ) ) / 10.
  ENDIF.
  IF ZTINSRSP-ZFTAMI IS INITIAL OR ZTINSRSP-ZFTAMI NE ZTINS-ZFINAMT.
    ZTINSRSP-ZFTAMI = ZTINS-ZFINAMT.
  ENDIF.
  IF ZTINSRSP-ZFCAMI IS INITIAL OR ZTINSRSP-ZFCAMI NE ZTINS-ZFINAMT.
    ZTINSRSP-ZFCAMI = ZTINS-ZFINAMT.
  ENDIF.

  IF ZTINSRSP-ZFTPR IS INITIAL OR ZTINSRSP-ZFTPR NE ZTINS-ZFINAMT.
    ZTINSRSP-ZFTPR = ZTINS-ZFINAMT.
  ENDIF.
  IF ZTINSRSP-ZFCPR IS INITIAL OR ZTINSRSP-ZFCPR NE ZTINS-ZFINAMT.
    ZTINSRSP-ZFCPR = ZTINS-ZFINAMT.
  ENDIF.

ENDMODULE.                 " SET_CALC_LOCAL_AMOUNT  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INS_OPEN_DATA_SCR4102  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INS_OPEN_DATA_SCR4102 INPUT.
* Confirm
  CHECK  W_STATUS EQ C_OPEN_C.

  IF ZTINS-ZFINNO IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINS' 'ZFINNO'.
  ENDIF.

  IF ZTINSRSP-ZFISDT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINSRSP' 'ZFISDT'.
  ENDIF.

  IF ZTINS-ZFINSFX  EQ  'X'.
    IF ZTINS-ZFINCD EQ 'A'.
      IF ZTINS-ZFBLNO IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINS' 'ZFBLNO'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_INS_OPEN_DATA_SCR4102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0192  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0192 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      ANTWORT = 'Y'.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.


ENDMODULE.                 " GET_OK_CODE_SCR0192  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INS_EXRT_AMOUNT_SCR4102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INS_EXRT_AMOUNT_SCR4102 INPUT.
* Case Confirm
  CHECK  W_STATUS EQ C_OPEN_C.
  IF ZTINSRSP-ZFTAMI IS INITIAL OR ZTINSRSP-ZFTAMI NE ZTINS-ZFINAMT.
    ZTINSRSP-ZFTAMI = ZTINS-ZFINAMT.
  ENDIF.
  IF ZTINSRSP-ZFCAMI IS INITIAL OR ZTINSRSP-ZFCAMI NE ZTINS-ZFINAMT.
    ZTINSRSP-ZFCAMI = ZTINS-ZFINAMT.
  ENDIF.
  W_AMOUNT = ZTINSRSP-ZFCAMI + ZTINSRSP-ZFDAMI.

*  IF ZTINSRSP-ZFTAMI NE W_AMOUNT.
*    PERFORM  P2000_NO_INPUT(SAPMZIM01) USING 'ZTINSRSP' 'ZFTAMI'
*                                            dfies-scrtext_m W_SUBRC.
*   MESSAGE  E521.
* ENDIF.

*  IF ZTINS-ZFINAMT NE ZTINSRSP-ZFTAMI.       " 보험료 외화.
*    PERFORM  P2000_NO_INPUT(SAPMZIM01) USING 'ZTINS' 'ZFINAMT'
*                                              dfies-scrtext_m W_SUBRC.
*    MESSAGE  E520.
* ENDIF.

ENDMODULE.                 " CHECK_INS_EXRT_AMOUNT_SCR4102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INS_LOCAL_AMOUNT_SCR4102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INS_LOCAL_AMOUNT_SCR4102 INPUT.
* Confirm
  CHECK  W_STATUS EQ C_OPEN_C.

  IF ZTINSRSP-ZFTPR IS INITIAL OR ZTINSRSP-ZFTPR NE ZTINS-ZFINAMT.
    ZTINSRSP-ZFTPR = ZTINS-ZFINAMT.
  ENDIF.
*  IF ZTINSRSP-ZFCPR IS INITIAL OR ZTINSRSP-ZFCPR NE ZTINS-ZFINAMT.
*    ZTINSRSP-ZFCPR = ZTINS-ZFINAMT.
*  ENDIF.
  W_AMOUNT = ZTINSRSP-ZFCPR + ZTINSRSP-ZFDPR + ZTINSRSP-ZFVPR +
             ZTINSRSP-ZFIPR.

  IF ZTINSRSP-ZFTPR NE W_AMOUNT.
    PERFORM  P2000_NO_INPUT(SAPMZIM01) USING 'ZTINSRSP' 'ZFTPR'
                                              DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE  E521.
  ENDIF.

  IF ZTINS-ZFKRWAMT NE ZTINSRSP-ZFTPR.
    PERFORM  P2000_NO_INPUT(SAPMZIM01) USING 'ZTINS' 'ZFKRWAMT'
                                              DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE  E520.
  ENDIF.

ENDMODULE.                 " CHECK_INS_LOCAL_AMOUNT_SCR4102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
*>수입자 구분.
  IF ZTREQHD-IMTRD IS INITIAL.
    IF NOT ( ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU' ).
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'IMTRD'.
    ENDIF.
  ENDIF.
*>> 자재구분.
  IF ZTREQHD-ZFMATGB IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFMATGB'.
  ENDIF.
*>> 수익자.
  IF ZTREQHD-ZFBENI IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFBENI'.
  ENDIF.
*>> Supplier
  IF ZTREQHD-LLIEF IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'LLIEF'.
  ENDIF.
*>> 운송 방법. --> 2002.05.29 NSH 주석처리.
  IF NOT ( ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU' ).
*    move 'A' to ztreqhd-zftrans.
*    IF ZTREQHD-ZFTRANS IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFTRANS'.
*    ENDIF.
  ENDIF.
*>> 지급방법.
  IF ZTREQHD-ZTERM IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZTERM'.
  ENDIF.
*>> L/C 종류.
  IF ZTREQHD-ZFLCKN IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFLCKN'.
  ENDIF.
*>> Bank CHG.
  IF ZTREQHD-ZFCHG IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFCHG'.
  ENDIF.
*>> 요개설일.
  IF ZTREQST-ZFREQDT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFREQDT'.
  ENDIF.

  IF ZTREQHD-ZFREQTY EQ 'LC'.
*>> 선적국.
*    IF ZTREQHD-ZFSHCU IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFSHCU'.
*    ENDIF.
*>> 선적항.
*    IF ZTREQHD-ZFSPRTC IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFSPRTC'.
*    ENDIF.
*>> 선적항텍스트.
    IF ZTREQHD-ZFSPRTC IS INITIAL AND
       ZTREQHD-ZFSPRT  IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFSPRT'.
    ENDIF.
*>> 도착국.
*    IF ZTREQHD-ZFARCU IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFARCU'.
*    ENDIF.
*>> 도착항.
*    IF ZTREQHD-ZFAPRTC IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFAPRTC'.
*    ENDIF.
*>> 도착항텍스트.
    IF ZTREQHD-ZFAPRTC IS INITIAL AND
       ZTREQHD-ZFAPRT  IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFAPRT'.
    ENDIF.
  ENDIF.
*>> PAYMENT TERM에 따라서 L/C 종류 CHECK!
  PERFORM  LC_KIND_CHECK.
ENDMODULE.                 " CHECK_INPUT_FIELD_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_OPEN_FIELD_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_OPEN_FIELD_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
*>> 재원구분.
*   IF ZTREQHD-ZFJEWGB IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFJEWGB'.
*  ENDIF.
*>> 개설일자.
  IF ZTREQST-ZFAPPDT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQST' 'ZFAPPDT'.
  ENDIF.
*>> 개설은행.
  IF ZTREQHD-ZFUSDAM GE ZTIMIMG00-ZFUSDAM AND SY-TCODE(4) EQ 'ZIM0'.
    IF ZTIMIMG00-ZFRELYN1 EQ 'X'.
      IF SY-TCODE EQ 'ZIM05'.
        PERFORM  P2000_CHECK_BANK_CODE.
      ENDIF.
    ELSE.
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM05'.  ">전체일 경우.
          PERFORM  P2000_CHECK_BANK_CODE.
      ENDCASE.
    ENDIF.
  ENDIF.

*>>지급은행의 계좌번호 자동 입력.
  IF ZTREQHD-ZFREQTY EQ 'TT' AND  ZTTTHD-ZFOBAK IS INITIAL.
    SELECT * FROM LFBK UP TO 1 ROWS  WHERE LIFNR EQ ZTREQHD-ZFOPBN.
    ENDSELECT.

    MOVE  LFBK-BANKN  TO  ZTTTHD-ZFOBAK.
  ENDIF.

  IF ZTREQHD-ZFREQTY EQ 'LO'  OR  ZTREQHD-ZFREQTY EQ 'LC'.
*>> S/D.
    IF ZTREQHD-ZFREQSD IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFREQSD'.
    ENDIF.
*>> E/D.
    IF ZTREQHD-ZFREQED IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFREQED'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_OPEN_FIELD_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_LC_CLOSE_BIT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_LC_CLOSE_BIT_SCR0102 INPUT.

  IF ZTREQHD-ZFCLOSE EQ 'X'.
*      IF ZTREQHD-ZFSUBYN IS INITIAL.
*         PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFSUBYN'.
*      ENDIF.
  ELSE.
    IF NOT ZTREQHD-ZFSUBYN IS INITIAL.
      MESSAGE E534.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_LC_CLOSE_BIT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_TRIPLE_BIT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_TRIPLE_BIT_SCR0102 INPUT.

  CLEAR : W_COUNT.
  IF ZTREQHD-ZFTRIPLE EQ 'X'.
    SELECT COUNT( * ) INTO W_COUNT
           FROM ZTBLIT
           WHERE EBELN EQ ZTREQHD-EBELN.
    IF W_COUNT GT 0.
      MESSAGE E936 WITH ZTREQHD-ZFREQNO 'B/L'.
    ENDIF.

    SELECT COUNT( * ) INTO W_COUNT
           FROM ZTCIVIT
           WHERE EBELN EQ ZTREQHD-EBELN.
    IF W_COUNT GT 0.
      MESSAGE E537 WITH ZTREQHD-ZFREQNO 'Commericial invoice'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_TRIPLE_BIT_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZINSDT_CONFIRM_SCR4102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZINSDT_CONFIRM_SCR4102 INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF NOT ZTINS-ZFINSDT IS INITIAL.
    IF ZTINS-ZFINSDT < ZTREQST-CDAT.
      MESSAGE  E552.
    ENDIF.
  ENDIF.
ENDMODULE.                 " ZINSDT_CONFIRM_SCR4102  INPUT
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
*&      Module  D0017_LIST_CHECK_SCR0017  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0017_LIST_CHECK_SCR0017 INPUT.
  LEAVE TO LIST-PROCESSING.
  CASE INCLUDE.
    WHEN 'POPU'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / SY-ULINE(107), / SY-VLINE NO-GAP,
                '유형'   NO-GAP, SY-VLINE NO-GAP,
                'Doc.관리No' NO-GAP, SY-VLINE NO-GAP,
                '메세지 텍스트', 105 SY-VLINE NO-GAP,
                'T'      NO-GAP, SY-VLINE,
              / SY-ULINE(107).
*         MESSAGE
      LOOP AT IT_ERR_LIST.
        W_MOD  =  SY-TABIX MOD 2.
        FORMAT RESET.
        IF W_MOD EQ 0.
          FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
        ELSE.
          FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
        ENDIF.
        WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4) NO-GAP,
                  SY-VLINE NO-GAP, IT_ERR_LIST-ZFIVNO  NO-GAP,
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
      WRITE : / SY-ULINE(107).
      CLEAR : IT_ERR_LIST.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " D0017_LIST_CHECK_SCR0017  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZTINS_COMP_SCR4102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZTINS_COMP_SCR4102 INPUT.

   IF W_STATUS EQ 'D'.  EXIT.  ENDIF.

   IF ZTINS-ZFOPCD IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINS' 'ZFOPCD'.
   ENDIF.

   SELECT SINGLE NAME1  INTO  ZTINS-ZFINSU1
   FROM   LFA1
   WHERE  LIFNR         EQ    ZTINS-ZFOPCD.

ENDMODULE.                 " ZTINS_COMP_SCR4102  INPUT
