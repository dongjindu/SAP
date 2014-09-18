*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  12/21/2011  Valerian  UD1K953731  [FI] - Default Payment Method by
*                                    Vendor Master
*----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& INCLUDE ZRIM02I01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입비용 Main PAI MODULE Include
*&      작성자 : 강석봉 INFOLINK Ltd.
*&      작성일 : 2001.05.14
*&  적용회사PJT:
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.
* W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
*    WHEN OTHERS.
*       ANTWORT = 'Y'.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.

  IF W_STATUS EQ C_REQ_D.
    SET SCREEN 0.   LEAVE SCREEN.
  ELSE.
    PERFORM P2000_SET_MESSAGE USING  SY-UCOMM.
  ENDIF.

ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0100 INPUT.

  PERFORM   P2000_BALANCE_CALCULATE.

  CASE OK-CODE.
    WHEN 'CKEK'.         "Simulation.
      PERFORM  P2000_SIMULATION_LIST.
    WHEN 'SAVE'.         "Save
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'OPDC'.         "Posting.
      PERFORM  P2000_SET_INDICATE.
    WHEN 'CCDC'.         "Posting Cancel.
      PERFORM  P2000_SET_INDICATE.
    WHEN 'FIHI'.         "Document History
      PERFORM  P2000_POSTING_HISTORY.
    WHEN 'HIST'.         "Header Change Document
      PERFORM  P2000_HEADER_CHANGE_DOC.
    WHEN 'POPU'.         "Multi Input
      PERFORM  P2000_MULTI_DOC_SELECT.
    WHEN 'HIIT'.         "Item Change Document
      PERFORM  P2000_SELECT_ITEM.
      PERFORM  P2000_ITEM_CHANGE_DOC.
    WHEN 'IMDC'.         "Reference Document
      PERFORM  P2000_SELECT_ITEM.
      PERFORM  P2000_ITEM_REF_DOC_DISPLAY.
    WHEN 'DETL' OR 'PI'. "Detail Screen
      PERFORM  P2000_SELECT_ITEM.
      PERFORM  P2000_ITEM_DETAIL_SCREEN.
    WHEN 'FB03'.         "FI Document Display.
      PERFORM  P2000_FI_DOCUMENT_DISPLAY
                             USING   ZTBKPF-BUKRS
                                     ZTBKPF-ZFFIYR
                                     ZTBKPF-ZFACDO.

    WHEN 'FB03R'.        "Advanced Payment FI Document Display
      PERFORM  P2000_FI_DOCUMENT_DISPLAY
                             USING   ZTBKPF-BUKRS
                                     ZTBKPF-ZFCLYR
                                     ZTBKPF-ZFCLNO.
    WHEN 'FB03P'.        "Different Actual Payeer Document Display
      PERFORM  P2000_FI_DOCUMENT_DISPLAY
                             USING   ZTBKPF-BUKRS
                                     ZTBKPF-ZFPYYR
                                     ZTBKPF-ZFPYNO.

    WHEN 'MK03'.         "Payeer
      PERFORM   P2000_VENDOR_DISPLAY USING ZTBKPF-LIFNR 'Payeer'.

    WHEN '0003' OR '0004'.
      IF OK-CODE EQ '0003'.
        MOVE 'X'  TO W_MARK.
      ELSE.
        CLEAR : W_MARK.
      ENDIF.
      LOOP AT IT_ZSBSEG.
        IT_ZSBSEG-ZFMARK = W_MARK.
        MODIFY IT_ZSBSEG INDEX  SY-TABIX.
      ENDLOOP.
    WHEN '0005'.
      PERFORM  P2000_SELECT_ITEM.
      CHECK : W_COUNT EQ 1.
      CLEAR: IT_ZSBSEG.
      PERFORM  P2000_SET_NEWKO  USING   IT_ZSBSEG-NEWKO
                                        IT_ZSBSEG-ZFCD
                                        ZTBKPF-ZFIMDNO.
      MOVE: ZTBKPF-WWERT   TO IT_ZSBSEG-WWERT,
            ZTBKPF-KURSF   TO IT_ZSBSEG-KURSF,
            '40'           TO IT_ZSBSEG-NEWBS,
            'S'            TO IT_ZSBSEG-SHKZG,
            0              TO IT_ZSBSEG-DMBTR,
            ZTBKPF-MWSKZ   TO IT_ZSBSEG-MWSKZ,
            ZTBKPF-ZFIMDNO TO IT_ZSBSEG-ZFIMDNO,
            W_ZFDCNM       TO IT_ZSBSEG-ZFDCNM.

      INSERT IT_ZSBSEG INDEX  W_TMP_TABIX.

    WHEN '0010'.
      PERFORM  P2000_SELECT_ITEM.
      CHECK : W_COUNT EQ 1.
      CLEAR: IT_ZSBSEG-ZFMARK.
      ADD 1 TO  W_TMP_TABIX.
      INSERT IT_ZSBSEG INDEX  W_TMP_TABIX.
    WHEN '0007'.
      DELETE IT_ZSBSEG WHERE ZFMARK EQ 'X'.
    WHEN 'CALC'.
      CLEAR : ZTBKPF-WRBTR, ZTBKPF-WMWST, ZTBKPF-DMBTR.
      LOOP AT IT_ZSBSEG.
        ADD : IT_ZSBSEG-WRBTR TO ZTBKPF-WRBTR,
              IT_ZSBSEG-DMBTR TO ZTBKPF-DMBTR,
              IT_ZSBSEG-WMWST TO ZTBKPF-WMWST.
      ENDLOOP.
      PERFORM   P2000_BALANCE_CALCULATE.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                " USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0100 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0100-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR0100 INPUT.

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
*&      Module  COMMAND_BEFORE_PROCESS_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE COMMAND_BEFORE_PROCESS_SCR0100 INPUT.

  PERFORM  SET_MODIFY_FIELD_CHECK.

  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.      " All Selected.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " All Deselected.
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DDLC'.       ">Double click'
      PERFORM  P2000_DDLC_PROCESS.
    WHEN 'OTDC'.       ">Other Document...
**>> CALL SCREEN.
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'CRDC'.       ">Create.
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'CHDC'.       ">Change.
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'DISP'.       ">Display.
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'BACK' OR 'EXIT'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'DELE'.
      PERFORM  P2000_SET_INDICATE.
    WHEN OTHERS.
      W_EXIT_MODE = 'N'.
*      READ TABLE IT_ZSBSEG INDEX 1.
*      IF ZTBKPF-ZFCSTGRP EQ '003' AND IT_ZSBSEG-ZFCD EQ '1AB'.
*        W_EXIT_MODE = 'Y'.
*      ENDIF.
      EXIT.
  ENDCASE.

  W_EXIT_MODE = 'Y'.

ENDMODULE.                 " COMMAND_BEFORE_PROCESS_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_HEADER_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE INPUT_HEADER_CHECK_SCR0100 INPUT.

  CHECK : W_STATUS    NE C_REQ_D.
  CHECK : W_EXIT_MODE NE 'Y'.

*> 사업장.
*  IF ZTBKPF-BUPLA IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUPLA'.
*  ENDIF.
*> 지급처.
*  IF ZTBKPF-LIFNR IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'LIFNR'.
*  ENDIF.
**> 지급보류키.
*  IF ZTBKPF-ZLSPR IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'ZLSPR'.
*  ENDIF.

*> 전표통화금액.
*  IF ZTBKPF-WRBTR IS INITIAL AND  ZTBKPF-WMWST IS INITIAL.
*     PERFORM P2000_NO_INPUT USING 'ZTBKPF' 'WRBTR'.
*     MESSAGE E000.
*  ENDIF.

*> 통화.
  IF ZTBKPF-WAERS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WAERS'.
  ENDIF.

*> 유환여부..
  IF ZTBKPF-ZFPOYN IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'ZFPOYN'.
  ENDIF.

ENDMODULE.                 " INPUT_HEADER_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  COMPANYCODE_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE COMPANYCODE_CHECK_SCR0100 INPUT.

  CHECK : W_STATUS    NE C_REQ_D.
  CHECK : W_EXIT_MODE NE 'Y'.

  IF ZTBKPF-BUKRS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUKRS'.
  ELSE.
    PERFORM  P1000_GET_COMPANY_CODE USING ZTBKPF-BUKRS.
*>> IMG 비용계정코드.
    SELECT SINGLE * FROM ZTIMIMG11
           WHERE    BUKRS  EQ   ZTBKPF-BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE S987 WITH ZTBKPF-BUKRS.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.

ENDMODULE.                 " COMPANYCODE_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  BELEGART_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE BELEGART_CHECK_SCR0100 INPUT.

  CHECK : W_STATUS    NE C_REQ_D.
  CHECK : W_EXIT_MODE NE 'Y'.

  IF ZTBKPF-BLART IS INITIAL AND
     NOT ( OK-CODE EQ 'SAVE' OR OK-CODE EQ 'POST' ).
    EXIT.
  ENDIF.

  " Reference Check.
  IF ZTBKPF-XBLNR IS INITIAL.
     MESSAGE  E977 WITH 'Input Vendor Invoice Number!'.
     EXIT.
  ENDIF.

*> FI Document Type Check.
  PERFORM BELEGART_PRUEFEN(SAPFF001)
          USING ZTBKPF-BLART ZTBKPF-GJAHR.

*> Posting Year Check
  BKPF-BUKRS = ZTBKPF-BUKRS.
  PERFORM NUMMERNKREIS_LESEN(SAPFF001)
           USING ZTBKPF-GJAHR.
*> Authority Check.
  PERFORM P2000_BELEGART_AUTHORITY_CHECK.

ENDMODULE.                 " BELEGART_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_EXCHAGE_RATE_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE GET_EXCHAGE_RATE_SCR0100 INPUT.

  CHECK : W_STATUS    NE C_REQ_D.
  CHECK : W_EXIT_MODE NE 'Y'.

* Currency must be set before call of 'CURRENCY_CODE_CHECK "Note 210410
*  if rfopt-xnowa ne space.                                 "Note 210410
*    bkpf-waers = t001-waers.                               "Note 210410
*  endif.                                                   "Note 210410
*
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

ENDMODULE.                 " GET_EXCHAGE_RATE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  COST_GROUP_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE COST_GROUP_CHECK_SCR0100 INPUT.

  CHECK : W_STATUS    NE C_REQ_D.
  CHECK : W_EXIT_MODE NE 'Y'.

  CLEAR:W_ZFDCNM.
  IF ZTBKPF-ZFCSTGRP EQ '008'.
    MESSAGE E692.
  ENDIF.

  IF ZTBKPF-ZFCSTGRP IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'ZFCSTGRP'.
  ELSE.
    IF W_ZFCSTGRP IS INITIAL.
      W_ZFCSTGRP = ZTBKPF-ZFCSTGRP.
      PERFORM  P1000_GET_COST_ITEM.
    ELSE.
      IF W_ZFCSTGRP NE ZTBKPF-ZFCSTGRP.
*>> LINE COUNTER.
        DESCRIBE TABLE IT_ZSBSEG LINES W_LINE.
        IF W_LINE GT 0.
          PERFORM  P2000_ITEM_REFRESH_MSG.
          IF ANTWORT EQ 'Y'.
            W_ZFCSTGRP = ZTBKPF-ZFCSTGRP.
            PERFORM  P1000_GET_COST_ITEM.
          ENDIF.
        ELSE.
          SET CURSOR FIELD 'ZTBKPF-ZFCSTGRP'.
          MESSAGE W590 WITH W_ZFCSTGRP ZTBKPF-ZFCSTGRP.
          W_ZFCSTGRP = ZTBKPF-ZFCSTGRP.
          PERFORM  P1000_GET_COST_ITEM.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*-->
  IF NOT ZTBKPF-ZFIMDNO IS INITIAL.
    PERFORM P1000_IMPORT_DOC_CHEKC   USING ZTBKPF-ZFIMDNO
                                           W_ZFDCNM
                                           ZTBKPF-ZFPOYN
                                          'H'
                                           ZTBSEG-KOSTL
                                           ''.
  ENDIF.

ENDMODULE.                 " COST_GROUP_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  VENDOR_ACCOUNT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE VENDOR_ACCOUNT_CHECK_SCR0100 INPUT.

  CHECK : W_STATUS    NE C_REQ_D.
  CHECK : W_EXIT_MODE NE 'Y'.

*   IF ZTBKPF-BUPLA IS INITIAL.    ">
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUPLA'.
*   ENDIF.
*
*   IF ZTBKPF-GSBER IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'GSBER'.
*   ENDIF.

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

  IF ZTBKPF-ZLSCH IS INITIAL.                               "UD1K953731
    ZTBKPF-ZLSCH = VF_KRED-ZWELS.                           "UD1K953731
  ENDIF.                                                    "UD1K953731

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
*   IF ZTBKPF-WRBTR IS INITIAL AND ZTBKPF-WMWST IS INITIAL.
**      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WRBTR'.
*      PERFORM P2000_NO_INPUT USING 'ZTBKPF' 'WRBTR'.
*      MESSAGE E934.
*   ENDIF.
*> ??.
  IF ZTBKPF-WAERS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WAERS'.
  ENDIF.

*>> ??? ??.
  IF ZTBKPF-XMWST EQ 'X'.
    SELECT SINGLE * FROM T007A
           WHERE KALSM EQ  T005-KALSM
           AND   MWSKZ EQ  ZTBKPF-MWSKZ.
    IF SY-SUBRC NE 0.
      MESSAGE E495 WITH T005-KALSM ZTBKPF-MWSKZ.
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

ENDMODULE.                 " VENDOR_ACCOUNT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  VAT_AMOUNT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE VAT_AMOUNT_CHECK_SCR0100 INPUT.

  CHECK : W_STATUS    NE C_REQ_D.
  CHECK : W_EXIT_MODE NE 'Y'.

  IF ZTBKPF-XMWST EQ 'X' AND NOT ZTBKPF-WMWST IS INITIAL.
    MESSAGE E555(F5).
  ENDIF.

  IF ZTBKPF-WAERS EQ ZTBKPF-HWAER.
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

ENDMODULE.                 " VAT_AMOUNT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBSEG_UPDATE_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSBSEG_UPDATE_SCR0100 INPUT.

  CHECK : W_STATUS    NE C_REQ_D.
  CHECK : W_EXIT_MODE NE 'Y'.
  DATA  : W_ZFCD(5)   TYPE C.

  CLEAR : W_ZFCD.

  W_CHK_BIT = 'Y'.
* Internal Table Read
  READ TABLE IT_ZSBSEG  INDEX TC_0100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  DATA : W_BELNR LIKE ZTBSEG-BELNR.              " NCW Insert
  CLEAR W_BELNR.

* Input Yes/No Check!
  IF  ZSBSEG-ZFCD    IS INITIAL
  AND ZSBSEG-WRBTR   IS INITIAL
  AND ZSBSEG-WMWST   IS INITIAL
  AND ZSBSEG-SGTXT   IS INITIAL.
    W_CHK_BIT = 'N'.
  ENDIF.

*> MOVE.
  MOVE-CORRESPONDING  ZSBSEG  TO   IT_ZSBSEG.
  MOVE : ZTBKPF-ZFCSTGRP      TO   IT_ZSBSEG-ZFCSTGRP.

*> INPUT VALUE CHECK.
  IF W_CHK_BIT EQ 'Y'.
*> Management No.
    IF IT_ZSBSEG-ZFCD IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'ZFCD'.
    ELSE.
*>> NCW Modify - 2003.12.07
*   Error message in case of freight duplication
      IF ZSBSEG-ZFCD EQ 'OBC' OR ZSBSEG-ZFCD EQ 'ABC'.
         IF ZSBSEG-ZFCD EQ 'OBC'.
            W_ZFCD = 'ABC'.
         ELSE.
            W_ZFCD = 'OBC'.
         ENDIF.

         SELECT SINGLE B~BELNR INTO W_BELNR
           FROM ZTBKPF AS A INNER JOIN ZTBSEG AS B
             ON A~BUKRS    EQ B~BUKRS AND
                A~BELNR    EQ B~BELNR AND
                A~GJAHR    EQ B~GJAHR
          WHERE A~ZFPOSYN  EQ 'Y'
            AND B~ZFIMDNO  EQ ZSBSEG-ZFIMDNO
            AND B~ZFCSTGRP EQ ZTBKPF-ZFCSTGRP
            AND ( B~ZFCD   EQ ZSBSEG-ZFCD
             OR   B~ZFCD   EQ W_ZFCD ).

         IF SY-SUBRC EQ 0.
            MESSAGE E006(ZIM1) WITH ZTBSEG-BELNR.
         ENDIF.
      ENDIF.
*  Error message in case of customs duplication
      IF ZTBKPF-ZFCSTGRP EQ '006'.
         IF ZSBSEG-ZFCD EQ '001'.

             SELECT SINGLE  B~BELNR INTO W_BELNR
               FROM ZTBKPF AS A INNER JOIN ZTBSEG AS B
                 ON A~BUKRS    EQ B~BUKRS AND
                    A~BELNR    EQ B~BELNR AND
                    A~GJAHR    EQ B~GJAHR
              WHERE A~ZFPOSYN  EQ 'Y'
                AND B~ZFIMDNO  EQ ZSBSEG-ZFIMDNO
                AND B~ZFCSTGRP EQ ZTBKPF-ZFCSTGRP
                AND B~ZFCD     EQ ZSBSEG-ZFCD.

            IF SY-SUBRC EQ 0.
               MESSAGE E007(ZIM1) WITH ZTBSEG-BELNR.
               EXIT.
            ENDIF.
         ENDIF.
      ENDIF.
*<<

      READ TABLE IT_ZSIMIMG08
              WITH KEY  ZFCDTY   =   IT_ZSBSEG-ZFCSTGRP
                        ZFCD     =   IT_ZSBSEG-ZFCD.
      IF SY-SUBRC NE 0.
        PERFORM P2000_NO_INPUT  USING  'ZSBSEG' 'ZFCD'.
        MESSAGE E909 WITH IT_ZSBSEG-ZFCD.
      ELSE.
        MOVE: IT_ZSIMIMG08-ZFCDNM    TO IT_ZSBSEG-ZFCDNM.

*> TAX CODE CHECK.
        IF IT_ZSIMIMG08-ZFCD5(2) NE IT_ZSBSEG-MWSKZ.
          MESSAGE W596 WITH IT_ZSIMIMG08-ZFCDNM
                  IT_ZSIMIMG08-ZFCD5 IT_ZSBSEG-MWSKZ.
        ENDIF.
        IF IT_ZSBSEG-MWSKZ NE ZTBKPF-MWSKZ.
          MESSAGE W598 WITH  ZTBKPF-LIFNR ZTBKPF-MWSKZ
                             IT_ZSBSEG-MWSKZ.
        ENDIF.
*> Condition category Check.
        IF IT_ZSIMIMG08-ZFCD1 EQ 'Y'.   ">DELIVERY COST.
          IF IT_ZSIMIMG08-COND_TYPE IS INITIAL.
            MESSAGE E595 WITH W_COST_TYPE IT_ZSBSEG-ZFCD.
          ELSE.
            IF IT_ZSIMIMG08-COND_TYPE NE IT_ZSIMIMG08-COND_TYPE.
              MESSAGE W610 WITH
                   IT_ZSIMIMG08-COND_TYPE IT_ZSIMIMG08-COND_TYPE.
            ELSE.
              MOVE: IT_ZSIMIMG08-ZFCDNM    TO IT_ZSBSEG-ZFCDNM,
                    IT_ZSIMIMG08-COND_TYPE TO IT_ZSBSEG-COND_TYPE.
            ENDIF.
          ENDIF.
          IT_ZSBSEG-ZFDCSTX = 'X'.
          ZTBKPF-ZFDCSTX = 'X'.
        ELSE.
          IT_ZSBSEG-COND_TYPE = IT_ZSIMIMG08-COND_TYPE.
          CLEAR : IT_ZSBSEG-ZFDCSTX, ZTBKPF-ZFDCSTX.
        ENDIF.
        IF ZTBKPF-BLART IS INITIAL.
          MOVE : IT_ZSIMIMG08-BLART TO ZTBKPF-BLART.
        ELSE.
          IF IT_ZSIMIMG08-BLART NE ZTBKPF-BLART.
            MESSAGE E107(ZIM1) WITH ZTBKPF-BLART
                                    IT_ZSIMIMG08-BLART.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*> Reference Document No
    IF ZSBSEG-ZFIMDNO IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'ZFIMDNO'.
    ELSE.
      PERFORM P1000_IMPORT_DOC_CHEKC   USING IT_ZSBSEG-ZFIMDNO
                                             IT_ZSBSEG-ZFDCNM
                                             IT_ZSBSEG-ZFPOYN
                                             'I'
                                             IT_ZSBSEG-KOSTL
                                             IT_ZSBSEG-ZFCD.
      CASE ZSBSEG-ZFCSTGRP.
        WHEN '004' OR  '005' OR '007'.
          SELECT SINGLE PS_POSID INTO IT_ZSBSEG-PS_POSID
                 FROM   ZTBL
                 WHERE  ZFBLNO  EQ IT_ZSBSEG-ZFIMDNO.
        WHEN '006'.
          SELECT SINGLE PS_POSID INTO IT_ZSBSEG-PS_POSID
                 FROM   ZTBL
                 WHERE  ZFBLNO  EQ
                      ( SELECT ZFBLNO FROM ZTIV
                        WHERE ZFIVNO EQ IT_ZSBSEG-ZFIMDNO ).
      ENDCASE.
    ENDIF.

    MOVE : ZTBKPF-ZFCSTGRP TO ZSBSEG-ZFCSTGRP.
    PERFORM  P2000_SET_NEWKO  USING ZSBSEG-NEWKO ZSBSEG-ZFCD
                                    ZSBSEG-ZFIMDNO.

    IT_ZSBSEG-NEWKO = ZSBSEG-NEWKO.

    IF IT_ZSBSEG-NEWBS IS INITIAL.
      MOVE: '40' TO IT_ZSBSEG-NEWBS,
            'S'  TO IT_ZSBSEG-SHKZG.
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

    IF ZTBKPF-ZFCSTGRP NE '006' OR IT_ZSBSEG-ZFCD NE '001'.
       IF IT_ZSBSEG-WRBTR IS INITIAL.
          PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'WRBTR'.
       ENDIF.
    ENDIF.

*> Convert Date
    IF IT_ZSBSEG-WWERT IS INITIAL.
      MOVE : ZTBKPF-WWERT TO IT_ZSBSEG-WWERT.
    ENDIF.
*> Exchange Rate
    IF IT_ZSBSEG-KURSF IS INITIAL.
      MOVE : ZTBKPF-KURSF TO IT_ZSBSEG-KURSF.
    ENDIF.
    IF IT_ZSBSEG-KURSF IS INITIAL.
      IF ZTBKPF-WAERS NE ZTBKPF-HWAER.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'KURSF'.
      ENDIF.
    ENDIF.
*> Assignement
    IF IT_ZSBSEG-ZUONR IS INITIAL.
      IT_ZSBSEG-ZUONR = IT_ZSBSEG-ZFDCNM.
    ENDIF.
*> Item TEXT.
    IF IT_ZSBSEG-SGTXT IS INITIAL.
      IT_ZSBSEG-SGTXT = IT_ZSBSEG-ZFCDNM.
    ENDIF.

  ENDIF.

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

*-----------------------------------------------------------------------
* Business Place, Business Area FIELD GET
*-----------------------------------------------------------------------
  IF ZTIMIMG00-ZFBALK EQ 'X' AND ZTIMIMG00-ZFBPLK EQ 'X'.
     CASE  ZTBKPF-ZFCSTGRP.
       WHEN '003'.
         SELECT SINGLE * FROM  ZTREQHD
                WHERE  ZFREQNO EQ  IT_ZSBSEG-ZFIMDNO.
         IF SY-SUBRC EQ 0.
           IF ZTBKPF-BUPLA IS INITIAL.
             SELECT SINGLE J_1BBRANCH  INTO  ZTBKPF-BUPLA
             FROM   T001W
             WHERE  WERKS  EQ ZTREQHD-ZFWERKS.
           ENDIF.
         ELSE.
           CLEAR : ZTBKPF-BUPLA.
         ENDIF.
         IF ZTBKPF-GSBER IS INITIAL.
           SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
                  FROM T134G
                  WHERE WERKS EQ ZTREQHD-ZFWERKS.
           IF SY-SUBRC NE 0.
             CLEAR : ZTBKPF-GSBER.
           ENDIF.
         ENDIF.
       WHEN '004' OR '005' OR '007'.
         SELECT SINGLE * FROM ZTBL
                WHERE  ZFBLNO EQ  IT_ZSBSEG-ZFIMDNO.
         IF SY-SUBRC EQ 0.
           IF ZTBKPF-BUPLA IS INITIAL.
             SELECT SINGLE J_1BBRANCH  INTO  ZTBKPF-BUPLA
             FROM   T001W
             WHERE  WERKS  EQ ZTBL-ZFWERKS.
           ENDIF.
         ENDIF.
         IF ZTBKPF-GSBER IS INITIAL.
           SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
                  FROM T134G
                  WHERE WERKS EQ ZTBL-ZFWERKS.
           IF SY-SUBRC NE 0.
             CLEAR : ZTBKPF-GSBER.
           ENDIF.
         ENDIF.
       WHEN  '006'.
         SELECT SINGLE * FROM ZTBL
                WHERE ZFBLNO EQ ( SELECT ZFBLNO FROM ZTIV
                                   WHERE ZFIVNO  EQ IT_ZSBSEG-ZFIMDNO ).
         IF SY-SUBRC EQ 0.
           IF ZTBKPF-BUPLA IS INITIAL.
             SELECT SINGLE J_1BBRANCH INTO ZTBKPF-BUPLA
             FROM   T001W
             WHERE  WERKS EQ ZTBL-ZFWERKS.
           ENDIF.
         ELSE.
           CLEAR : ZTBKPF-BUPLA.
         ENDIF.
         IF ZTBKPF-GSBER IS INITIAL.
           SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
                  FROM T134G
                  WHERE WERKS EQ ZTBL-ZFWERKS.
           IF SY-SUBRC NE 0.
             CLEAR : ZTBKPF-GSBER.
           ENDIF.
         ENDIF.
       WHEN  '009'.
         SELECT SINGLE * FROM ZTTRHD
                WHERE ZFTRNO EQ IT_ZSBSEG-ZFIMDNO.
         IF SY-SUBRC EQ 0.
           IF ZTBKPF-BUPLA IS INITIAL.
             SELECT SINGLE J_1BBRANCH INTO ZTBKPF-BUPLA
             FROM   T001W
             WHERE  WERKS EQ ZTTRHD-WERKS.
           ENDIF.
         ELSE.
           CLEAR : ZTBKPF-BUPLA.
         ENDIF.
         IF ZTBKPF-GSBER IS INITIAL.
           SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
                  FROM T134G
                  WHERE WERKS EQ ZTTRHD-WERKS.
           IF SY-SUBRC NE 0.
             CLEAR : ZTBKPF-GSBER.
           ENDIF.
         ENDIF.
     ENDCASE.
  ENDIF.
  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSBSEG INDEX W_TABIX.
  ELSE.
    IF W_CHK_BIT EQ 'Y'.
      APPEND IT_ZSBSEG.
    ENDIF.
  ENDIF.

ENDMODULE.                 " IT_ZSBSEG_UPDATE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_DOCUMENT_SCR00010  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INPUT_DOCUMENT_SCR00010 INPUT.

  IF NOT ( SY-UCOMM EQ 'YES' OR
           SY-UCOMM EQ 'ENTR' ).
    EXIT.
  ENDIF.

*> Company Code Check.
  IF ZSBKPF-BUKRS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'BUKRS'.
  ENDIF.

*> Fiscal Year
  IF ZSBKPF-GJAHR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'GJAHR'.
  ENDIF.

*> Document No
  IF ZSBKPF-BELNR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'BELNR'.
  ENDIF.

*> CHARGE DOCUMENT READ.
  PERFORM   P1000_GET_CHARGE_DOCUMENT  USING   'E'.

ENDMODULE.                 " CHECK_INPUT_DOCUMENT_SCR00010  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0100  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0100 INPUT.

  READ TABLE IT_ZSBSEG  INDEX TC_0100-CURRENT_LINE.
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

ENDMODULE.                 " SET_SCR_MARK_TC_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
MODULE D0014_LIST_CHECK_SCR0014 INPUT.
  LEAVE TO LIST-PROCESSING.
  CASE INCLUDE.
    WHEN 'TAXBKPO'.
*> HEADER WRITE.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / ' P/O      ' NO-GAP,    SY-VLINE NO-GAP,
                'Imp.Req.No' NO-GAP,    SY-VLINE NO-GAP,
           (22) 'Bas Doc.No' NO-GAP,    SY-VLINE NO-GAP,
                ' Give Date' NO-GAP,    SY-VLINE NO-GAP,
                ' Tax No ' NO-GAP,    SY-VLINE NO-GAP,
                '  Approve No  ' NO-GAP,    SY-VLINE.
      FORMAT RESET.
      WRITE : / SY-ULINE.
*> LINE ITEM WRITE..
      LOOP AT IT_ZSTAXBKHD.
        W_MOD = SY-TABIX MOD 2.
        PERFORM   P2000_TAXPO_DISPLAY_LIST.
        HIDE:IT_ZSTAXBKHD.
      ENDLOOP.
      CLEAR : IT_ZSTAXBKHD.
    WHEN 'POPU'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / SY-ULINE(96), / SY-VLINE NO-GAP,
                'TYPE'   NO-GAP, SY-VLINE NO-GAP,
                'Message Text ', 94 SY-VLINE NO-GAP,
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
          WHEN 'E'.
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
    WHEN 'BLHELP'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / 'B/L Number : ', ZSREQHD-ZFHBLNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE : / 'B/L Doc.No'            NO-GAP,  SY-VLINE NO-GAP,
                'Cur. '                 NO-GAP,  SY-VLINE NO-GAP,
                '      Amount       '   NO-GAP,  SY-VLINE NO-GAP,
                'Shp'                   NO-GAP,  SY-VLINE NO-GAP,
                'Loading Port'          NO-GAP,  SY-VLINE NO-GAP,
                'Trp'                   NO-GAP,  SY-VLINE NO-GAP,
                'ArrivingPort'          NO-GAP,  SY-VLINE NO-GAP,
                'In charge '.
      FORMAT RESET.
      WRITE : / SY-ULINE.
      LOOP AT IT_ZSREQHD.
        W_MOD = SY-TABIX MOD 2.
        WRITE:/ IT_ZSREQHD-ZFBLNO  NO-GAP COLOR COL_KEY INTENSIFIED,
                SY-VLINE NO-GAP.
        IF W_MOD EQ 0.
          FORMAT COLOR COL_NORMAL INTENSIFIED ON.
        ELSE.
          FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
        ENDIF.
        WRITE : IT_ZSREQHD-WAERS   NO-GAP,    SY-VLINE NO-GAP,
                IT_ZSREQHD-ZFBLAMT CURRENCY IT_ZSREQHD-WAERS NO-GAP,
                SY-VLINE NO-GAP,
                IT_ZSREQHD-ZFCARC  NO-GAP,    SY-VLINE NO-GAP,
                IT_ZSREQHD-ZFSPRT(12)  NO-GAP,    SY-VLINE NO-GAP,
                IT_ZSREQHD-ZFAPPC  NO-GAP,    SY-VLINE NO-GAP,
                IT_ZSREQHD-ZFAPRT(12)  NO-GAP,    SY-VLINE NO-GAP,
                IT_ZSREQHD-ZFRCVER(10).

        HIDE:IT_ZSREQHD.
      ENDLOOP.
      CLEAR : IT_ZSREQHD.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
*&      Module  P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
MODULE P2000_INIT_VALUE_CHECK INPUT.

  IF NOT ( SY-UCOMM EQ 'ENTR' OR SY-UCOMM EQ 'YES' ).
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
*&      Module  HELP_DOC_NO_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_DOC_NO_SCR0100 INPUT.

  PERFORM   P2000_CHECK_COST_GROUP  USING   W_ERR_MODE.

  IF W_ERR_MODE EQ 'N'.
    CASE ZTBKPF-ZFCSTGRP.
      WHEN '003'.
        PERFORM  P1000_GET_ZFREQNO_HELP USING  ZSBKPF-ZFIMDNO.
      WHEN '004' OR '005' OR '007'.
        PERFORM  P1000_GET_ZFBLNO_HELP  USING  ZSBKPF-ZFIMDNO.
      WHEN '006'.
        PERFORM  P1000_GET_ZFIVNO_HELP  USING  ZSBKPF-ZFIMDNO.
      WHEN '008'.
        PERFORM  P1000_GET_ZFTBNO_HELP  USING  ZSBKPF-ZFIMDNO.
      WHEN '009'.
        PERFORM  P1000_GET_ZFTRNO_HELP  USING  ZSBKPF-ZFIMDNO.
      WHEN OTHERS.
    ENDCASE.
    IF ANTWORT EQ 'Y'.
      IF W_STATUS NE C_REQ_D.
        MOVE : ZSBKPF-ZFIMDNO TO ZTBKPF-ZFIMDNO.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " HELP_DOC_NO_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCR0030  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCR0030 INPUT.

  ANTWORT = 'N'.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " USER_EXIT_SCR0030  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_REQ_DATA_SCR0030  INPUT
*&---------------------------------------------------------------------*
MODULE READ_REQ_DATA_SCR0030 INPUT.
  CASE SY-UCOMM.
    WHEN 'CANC' OR 'NO'.
      EXIT.
  ENDCASE.

  IF ZSREQHD-ZFOPNNO IS INITIAL.
    IF ZSREQHD-ZFREQNO IS INITIAL.
      MESSAGE E061.
    ENDIF.
  ELSE.
    IF ZSREQHD-ZFREQNO IS INITIAL.
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTREQST
                        WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO.
      IF W_COUNT EQ 0.
        MESSAGE E067 WITH ZSREQHD-ZFOPNNO.
      ENDIF.

      SELECT MAX( ZFREQNO ) INTO  W_ZFREQNO
                            FROM  ZTREQST
                            WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO.

      IF SY-SUBRC NE 0.
        MESSAGE E062 WITH  ZSREQHD-ZFOPNNO.
      ELSE.
        IF W_ZFREQNO IS INITIAL.
          IF ZSREQHD-ZFREQNO IS INITIAL.
            MESSAGE E062 WITH  ZSREQHD-ZFOPNNO.
          ENDIF.
        ELSE.
          ZSREQHD-ZFREQNO = W_ZFREQNO.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
* Import Request Header Table
  CALL FUNCTION 'ZIM_GET_REQ_DOC_HEADER'
         EXPORTING
               ZFREQNO           =     ZSREQHD-ZFREQNO
         IMPORTING
               W_ZTREQHD         =      ZTREQHD
               W_ZTREQST         =      ZTREQST
         EXCEPTIONS
               NOT_FOUND         =    4
               NOT_INPUT         =    8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E018 WITH ZSREQHD-ZFREQNO.
    WHEN 8.
      MESSAGE E019.
  ENDCASE.

*>> Close Check
  IF ZTREQHD-ZFCLOSE EQ 'X'.
    MESSAGE E354 WITH ZTREQHD-ZFREQNO.
  ENDIF.

ENDMODULE.                 " READ_REQ_DATA_SCR0030  INPUT
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
  ENDCASE.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0030  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_BL_DOCUMENT_SCR0040  INPUT
*&---------------------------------------------------------------------*
MODULE GET_BL_DOCUMENT_SCR0040 INPUT.
  CASE SY-UCOMM.
    WHEN 'CANC' OR 'NO'.   EXIT.
    WHEN OTHERS.
  ENDCASE.

  IF ZSREQHD-ZFBLNO IS INITIAL.
    IF ZSREQHD-ZFHBLNO IS INITIAL.
      MESSAGE E304.
    ELSE.
* B/L NO에 Count
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
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          PERFORM P2000_SEARCH_FIELD_MOVE.
      ENDCASE.
    ENDIF.
  ELSE.
    IF NOT ZSCIVHD-ZFHBLNO IS INITIAL.
* B/L NO에 Count
      SELECT COUNT( * ) INTO  W_COUNT
                       FROM  ZTBL
                       WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
      CASE W_COUNT.
        WHEN 0.
        WHEN 1.
          SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
                        FROM ZTBL
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
            EXIT.
          ENDSELECT.
        WHEN OTHERS.
          PERFORM P2000_BL_DOC_ITEM_SELECT.
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          PERFORM P2000_SEARCH_FIELD_MOVE.
      ENDCASE.
    ENDIF.
  ENDIF.
* B/L READ PERFORM ?
  ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
  PERFORM   P1000_READ_BL_DOC.

ENDMODULE.                 " GET_BL_DOCUMENT_SCR0040  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_COST_DIVIDE_LIST_SCR0110  INPUT
*&---------------------------------------------------------------------*
MODULE SET_COST_DIVIDE_LIST_SCR0110 INPUT.

  LEAVE TO LIST-PROCESSING.

  REFRESH : IT_ZSBDIVL.

  SELECT SINGLE * FROM T001
                  WHERE BUKRS EQ ZTBKPF-BUKRS.

  LOOP AT IT_ZTBDIV.
    CLEAR : IT_ZSBDIVL.
    MOVE-CORRESPONDING  IT_ZTBDIV  TO   IT_ZSBDIVL.
    CASE IT_ZTBDIV-NEWBS.
      WHEN '40'.
        SELECT SINGLE TXT20 INTO IT_ZSBDIVL-KONTO_TXT
               FROM  SKAT
               WHERE SPRAS  EQ  SY-LANGU
               AND   KTOPL  EQ  T001-KTOPL
               AND   SAKNR  EQ  IT_ZTBDIV-NEWKO+7(10).
      WHEN '31'.
        SELECT SINGLE NAME1 INTO IT_ZSBDIVL-KONTO_TXT
               FROM  LFA1
               WHERE LIFNR  EQ  IT_ZTBDIV-NEWKO(10).
    ENDCASE.
    APPEND  IT_ZSBDIVL.
  ENDLOOP.

*REUSE_ALV_LIST_DISPLAY
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_STRUCTURE_NAME = 'ZSBDIVL'
       TABLES
            T_OUTTAB         = IT_ZSBDIVL.

  CLEAR : OK-CODE.
  LEAVE TO SCREEN 0100.

ENDMODULE.                 " SET_COST_DIVIDE_LIST_SCR0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_COST_CODE_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_COST_CODE_SCR0100 INPUT.

  DATA : L_DISPLAY.

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
    CONCATENATE W_COST_TYPE 'Code Help' INTO WINDOW_TITLE
                SEPARATED BY SPACE.

    IF W_STATUS EQ C_REQ_D.
      L_DISPLAY = 'X'.
    ELSE.
      CLEAR: L_DISPLAY.
    ENDIF.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
         EXPORTING
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
*&      Module  HELP_ITEM_DOC_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_ITEM_DOC_SCR0100 INPUT.

  PERFORM   P2000_CHECK_COST_GROUP  USING   W_ERR_MODE.

  IF W_ERR_MODE EQ 'N'.
    CASE ZTBKPF-ZFCSTGRP.
      WHEN '003'.
        PERFORM  P1000_GET_ZFREQNO_HELP USING  ZSBKPF-ZFIMDNO.
      WHEN '004' OR '005' OR '007'.
        PERFORM  P1000_GET_ZFBLNO_HELP  USING  ZSBKPF-ZFIMDNO.
      WHEN '006'.
        PERFORM  P1000_GET_ZFIVNO_HELP  USING  ZSBKPF-ZFIMDNO.
      WHEN '008'.
        PERFORM  P1000_GET_ZFTBNO_HELP  USING  ZSBKPF-ZFIMDNO.
      WHEN '009'.
        PERFORM  P1000_GET_ZFTRNO_HELP  USING  ZSBKPF-ZFIMDNO.
      WHEN OTHERS.
    ENDCASE.
    IF ANTWORT EQ 'Y'.
      IF W_STATUS NE C_REQ_D.
        MOVE : ZSBKPF-ZFIMDNO TO ZSBSEG-ZFIMDNO.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " HELP_ITEM_DOC_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0120  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0120 INPUT.

  PERFORM   P2000_BALANCE_CALCULATE.

  CASE OK-CODE.
    WHEN 'ENTR'.

    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0100.
    WHEN 'HIIT'.      ">Item Change Log
      PERFORM  P2000_SELECT_ITEM.     " Selected Item
      PERFORM  P2000_ITEM_CHANGE_DOC.
    WHEN 'IMDC'.      ">Reference Document
      PERFORM  P2000_SELECT_ITEM.     " Selected Item
      PERFORM  P2000_ITEM_REF_DOC_DISPLAY.
    WHEN 'FB03'.      ">FI Document
      PERFORM  P2000_FI_DOCUMENT_DISPLAY
                             USING   ZTBKPF-BUKRS
                                     ZTBKPF-ZFFIYR
                                     ZTBKPF-ZFACDO.
    WHEN 'MK03'.      ">Payee
      PERFORM   P2000_VENDOR_DISPLAY USING ZTBKPF-LIFNR
                                           'Payee'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0120  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZFCGNO_SCR0060  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ZFCGNO_SCR0060 INPUT.
  CASE SY-UCOMM.
    WHEN 'CANC' OR 'NO'.
      EXIT.
  ENDCASE.

  CALL FUNCTION 'ZIM_GET_CARGO_WORK_DOCUMENT'
       EXPORTING
            ZFCGNO         = ZSCGHD-ZFCGNO
       IMPORTING
            W_ZTCGHD       = ZTCGHD
       TABLES
            IT_ZSCGCST     = IT_ZSCGCST
            IT_ZSCGCST_ORG = IT_ZSCGCST_ORG
            IT_ZSCGIT      = IT_ZSCGIT
            IT_ZSCGIT_ORG  = IT_ZSCGIT_ORG
       EXCEPTIONS
            NOT_FOUND      = 4
            NOT_INPUT      = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E390 WITH ZSCGHD-ZFCGNO.
    WHEN 8.
      MESSAGE E391.
  ENDCASE.

ENDMODULE.                 " GET_ZFCGNO_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZFIVNO_SCR0050  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ZFIVNO_SCR0050 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC' OR 'NO'.
      EXIT.
  ENDCASE.

  CALL FUNCTION 'ZIM_GET_CC_DOCUMENT'
       EXPORTING
            ZFIVNO        = ZSIV-ZFIVNO
       IMPORTING
            W_ZTIV        = ZTIV
       TABLES
            IT_ZSIVIT     = IT_ZSIVIT
            IT_ZSIVIT_ORG = IT_ZSIVIT_ORG
            IT_ZSIVCD     = IT_ZSIVCD
            IT_ZSIVHST    = IT_ZSIVHST
            IT_ZSIVHST1   = IT_ZSIVHST1
       EXCEPTIONS
            NOT_FOUND     = 4
            NOT_INPUT     = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E413 WITH ZSIV-ZFIVNO.
    WHEN 8.
      MESSAGE E412.
  ENDCASE.

ENDMODULE.                 " GET_ZFIVNO_SCR0050  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_POSTING_HISTORY_SCR0130  INPUT
*&---------------------------------------------------------------------*
MODULE SET_POSTING_HISTORY_SCR0130 INPUT.

  LEAVE TO LIST-PROCESSING.

  G_REPID = SY-REPID.
  CLEAR G_VARIANT.
  G_VARIANT-REPORT = G_REPID.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = G_REPID
            I_STRUCTURE_NAME         = 'ZSBHIS'
            I_CALLBACK_PF_STATUS_SET = G_STATUS
            I_CALLBACK_USER_COMMAND  = G_USER_COMMAND
            I_SAVE                   = G_SAVE
            IS_VARIANT               = G_VARIANT
       TABLES
            T_OUTTAB                 = IT_ZSBHIS.

  CLEAR : OK-CODE.
  LEAVE TO SCREEN 0100.

ENDMODULE.                 " SET_POSTING_HISTORY_SCR0130  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_TBTKZ_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_TBTKZ_SCR0100 INPUT.

  PERFORM   P2000_TBTKZ_HELP(SAPMZIMG)  USING   ZTBKPF-TBTKZ.
  SET CURSOR FIELD 'ZTBKPF-TBTKZ'.

ENDMODULE.                 " HELP_TBTKZ_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TBTKZ_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE TBTKZ_CHECK_SCR0100 INPUT.

  IF ZTBKPF-TBTKZ IS INITIAL.    "> Subsequent Debit, Credit.
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

ENDMODULE.                 " TBTKZ_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCR0070  INPUT
*&---------------------------------------------------------------------*
MODULE READ_DOC_SCR0070 INPUT.

  IF ZSREQHD-EBELN   IS INITIAL AND ZSREQHD-ZFREQNO IS INITIAL AND
     ZSREQHD-ZFOPNNO IS INITIAL AND ZSREQHD-ZFTBNO  IS INITIAL.
     MESSAGE E066.
  ENDIF.

  W_COUNT = 1.

*>> P/O No Check
  IF NOT ZSREQHD-EBELN IS INITIAL.
* P/O NO Count
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZTTAXBKHD
                      WHERE EBELN EQ ZSREQHD-EBELN.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E684 WITH ZSREQHD-EBELN.
      WHEN 1.
        SELECT ZFTBNO  INTO ZSREQHD-ZFTBNO UP TO 1 ROWS
                       FROM ZTTAXBKHD
                       WHERE EBELN EQ ZSREQHD-EBELN.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        REFRESH IT_ZSTAXBKHD.
* Table Multi-Select
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSTAXBKHD
                 FROM   ZTTAXBKHD
                 WHERE  EBELN   EQ ZSREQHD-EBELN.

        PERFORM P2000_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* READ PERFORM
    PERFORM    P1000_TAXBOOK_READ.
    EXIT.
  ENDIF.

  IF NOT ZSREQHD-ZFOPNNO IS INITIAL.
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZTTAXBKHD
                      WHERE BASISNO EQ ZSREQHD-ZFOPNNO.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E689 WITH ZSREQHD-ZFOPNNO.
      WHEN 1.
        SELECT ZFTBNO  INTO  ZSREQHD-ZFTBNO UP TO 1 ROWS
                       FROM  ZTTAXBKHD
                       WHERE BASISNO EQ ZSREQHD-ZFOPNNO.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        REFRESH IT_ZSTAXBKHD.
* Table Multi-Select
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSTAXBKHD
                 FROM   ZTTAXBKHD
                 WHERE  BASISNO EQ ZSREQHD-ZFOPNNO.

        PERFORM P2000_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
    PERFORM    P1000_TAXBOOK_READ.
    EXIT.
  ENDIF.

*>> Import Request Document No Check.
  IF NOT ZSREQHD-ZFREQNO IS INITIAL.
* P/O NO에 Count.
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZTTAXBKHD
                      WHERE ZFREQNO EQ ZSREQHD-ZFREQNO.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E690 WITH ZSREQHD-ZFREQNO.
      WHEN 1.
        SELECT ZFTBNO  INTO ZSREQHD-ZFTBNO UP TO 1 ROWS
                       FROM ZTTAXBKHD
                       WHERE ZFREQNO EQ ZSREQHD-ZFREQNO.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        REFRESH IT_ZSTAXBKHD.
* Table Multi-Select.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSTAXBKHD
                 FROM   ZTTAXBKHD
                 WHERE  ZFREQNO  EQ ZSREQHD-ZFREQNO.

        PERFORM P2000_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
* READ PERFORM ?
    PERFORM    P1000_TAXBOOK_READ.
    EXIT.
  ENDIF.

* Import Request Document READ PERFORM.
  PERFORM    P1000_TAXBOOK_READ.

ENDMODULE.                 " READ_DOC_SCR0070  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFADVPT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE ZFADVPT_CHECK_SCR0100 INPUT.

  IF ZTBKPF-ZFADVPT IS INITIAL.
    CLEAR : ZTBKPF-HKONT.
    MESSAGE W926 WITH 'Clear'.
  ELSE.
    MESSAGE W926 WITH 'Setting up'.
    IF ZTBKPF-HKONT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'HKONT'.
    ELSE.
      SELECT SINGLE * FROM LFA1
                     WHERE LIFNR EQ ZTBKPF-HKONT.
      IF SY-SUBRC NE 0.
        MESSAGE E585 WITH 'Employer vendor' ZTBKPF-HKONT.
      ELSE.
        IF LFA1-KTOKK NE ZTIMIMG00-KTOKK.
          MESSAGE E408(ZIM1) WITH LFA1-KTOKK ZTIMIMG00-KTOKK.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT ZTBKPF-ZFADVPT IS INITIAL.
    IF  NOT ZTBKPF-ZFPYPT IS INITIAL.
      MESSAGE  E555(ZIM1).
    ENDIF.
    IF NOT ZTBKPF-ZFPYN IS INITIAL.
      MESSAGE  E555(ZIM1).
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZFADVPT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBSEG_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSBSEG_CHECK_SCR0100 INPUT.

  CHECK W_STATUS NE C_REQ_D.

* Internal Table Read
  READ TABLE IT_ZSBSEG  INDEX TC_0100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSBSEG INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " IT_ZSBSEG_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFRVSX_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE ZFRVSX_CHECK_SCR0100 INPUT.

  IF ZTBKPF-ZFRVSX IS INITIAL.
    MESSAGE W419(ZIM1) WITH 'Clear'.
  ELSE.
    MESSAGE W419(ZIM1) WITH 'Setting up'.
  ENDIF.

ENDMODULE.                 " ZFRVSX_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFPYPT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE ZFPYPT_CHECK_SCR0100 INPUT.

  IF ZTBKPF-ZFPYPT IS INITIAL.
    CLEAR : ZTBKPF-ZFPYN.
    MESSAGE W926(ZIM1) WITH 'Clear'.
  ELSE.
    MESSAGE W926(ZIM1) WITH 'Setting up'.
    IF ZTBKPF-ZFPYN IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'ZFPYN'.
    ENDIF.
  ENDIF.

  IF NOT ZTBKPF-ZFPYPT IS INITIAL.
    IF  NOT ZTBKPF-ZFADVPT IS INITIAL.
      MESSAGE  E555(ZIM1).
    ENDIF.
    IF NOT ZTBKPF-HKONT IS INITIAL.
      MESSAGE  E555(ZIM1).
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZFPYPT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_OPEN_BANK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_OPEN_BANK_SCR0100 INPUT.
  IF ZTBKPF-LIFNR IS INITIAL.
     MESSAGE S977(ZIM) WITH 'Input Vendor Code!'.
     EXIT.
  ENDIF.

  REFRESH : IT_OPBN_HELP.
  SELECT SINGLE * FROM LFA1
  WHERE  LIFNR    EQ   ZTBKPF-LIFNR.
  IF LFA1-LNRZA   IS   INITIAL.
     SELECT * FROM LFZA WHERE LIFNR EQ ZTBKPF-LIFNR.
        CLEAR : LFA1.
        SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LFZA-EMPFK.
        MOVE : LFZA-EMPFK      TO   IT_OPBN_HELP-LIFNR,
               LFA1-NAME1      TO   IT_OPBN_HELP-NAME1,
               LFA1-ORT01      TO   IT_OPBN_HELP-ORT01.
        APPEND IT_OPBN_HELP.
     ENDSELECT.
  ELSE.
     SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ  LFA1-LNRZA.
     MOVE : LFA1-LIFNR      TO   IT_OPBN_HELP-LIFNR,
            LFA1-NAME1      TO   IT_OPBN_HELP-NAME1,
            LFA1-ORT01      TO   IT_OPBN_HELP-ORT01.
     APPEND IT_OPBN_HELP.
  ENDIF.

  DESCRIBE  TABLE  IT_OPBN_HELP  LINES  W_LINE.
  IF W_LINE EQ 0.
    MESSAGE S406.  EXIT.
  ENDIF.

  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.

  WINDOW_TITLE = 'Alternative Payee'.
  CONCATENATE WINDOW_TITLE 'Help' INTO WINDOW_TITLE
              SEPARATED BY SPACE.

  IF W_STATUS EQ C_REQ_D.
     L_DISPLAY = 'X'.
  ELSE.
     CLEAR: L_DISPLAY.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
               RETFIELD        = 'LIFNR'
               DYNPPROG        = DYNPROG
               DYNPNR          = DYNNR
               DYNPROFIELD     = 'ZTBKPF-ZFOPBN'
               WINDOW_TITLE    = WINDOW_TITLE
               VALUE_ORG       = 'S'
               DISPLAY         = L_DISPLAY
       TABLES
               VALUE_TAB       = IT_OPBN_HELP
       EXCEPTIONS
               PARAMETER_ERROR = 1
               NO_VALUES_FOUND = 2
               OTHERS          = 3.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
ENDMODULE.                 " HELP_OPEN_BANK_SCR0100  INPUT
