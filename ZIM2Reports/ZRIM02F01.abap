*&---------------------------------------------------------------------
*& INCLUDE ZRIM02F01 .
*&---------------------------------------------------------------------
*&  프로그램명 : 수입비용 Main SUB MODULE Include
*&      작성자 : 강석봉 INFOLINK Ltd.
*&      작성일 : 2001.05.14
*&  적용회사PJT:
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_GUI_TEXT
*&---------------------------------------------------------------------*
FORM P2000_SET_GUI_TEXT.

  CASE W_STATUS.
    WHEN C_REQ_C.   ASSIGN W_CREATE  TO <FS_F>.
    WHEN C_REQ_U.   ASSIGN W_CHANGE  TO <FS_F>.
    WHEN C_REQ_D.   ASSIGN W_DISPLAY TO <FS_F>.
    WHEN C_OPEN_C.  ASSIGN W_OPEN    TO <FS_F>.
    WHEN C_OPEN_U.  ASSIGN W_STAUTS  TO <FS_F>.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_GUI_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM P2000_SET_PF_STATUS.

  CASE SY-DYNNR.
    WHEN '0100'.
      MOVE 'FIDC' TO W_PFSTAT.
    WHEN '0120'.
      MOVE 'FIDI' TO W_PFSTAT.
    WHEN '0130'.
      MOVE 'FIDI' TO W_PFSTAT.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_STATUS_TCODE_DISABLE
*&---------------------------------------------------------------------*
FORM P2000_SET_STATUS_TCODE_DISABLE.

  CASE SY-TCODE.
* 생성 t-code
    WHEN 'ZIMY1'.
      MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 기타문서.
      MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 생성.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 삭제.
      MOVE 'CCDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 전기취소.
      MOVE 'RERQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 결재요청화면.
      MOVE 'HIST' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " HEAD CHAN
      MOVE 'HIIT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ITEM CHAN
      SET TITLEBAR  'FIDO' WITH W_CREATE.
* 변경 t-code
    WHEN 'ZIMY2'.
*      MOVE 'OPDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 전기취소.
      MOVE 'CCDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 전기취소.
      MOVE 'RERQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 결재요청화면.
      MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 변경.
      SET TITLEBAR  'FIDO' WITH W_CHANGE.
* 조회 t-code
    WHEN 'ZIMY3'.
      MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 조회.
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 저장.
*      MOVE 'CKEK' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 점검.
      SET TITLEBAR  'FIDO' WITH W_DISPLAY.
    WHEN OTHERS.
  ENDCASE.

  CASE ZTBKPF-ZFPOSYN.
    WHEN 'Y'.
      MOVE 'OPDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 변경.
      IF ZTBKPF-ZFADVPT EQ 'X'.
        MOVE  'FB03P'  TO  IT_EXCL-FCODE.   APPEND IT_EXCL.
      ENDIF.
      IF ZTBKPF-ZFPYPT EQ 'X'.
        MOVE  'FB03R'  TO  IT_EXCL-FCODE.   APPEND IT_EXCL.
      ENDIF.
    WHEN 'N'.
      MOVE 'FB03'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 전기.
      MOVE 'FB03R' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 업무가불.
      MOVE 'FB03P' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " A/P 반제.
      MOVE 'CCDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 변경.
      MOVE 'RERQ'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 결재요청.
    WHEN 'P'.
      MOVE 'FB03R' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 업무가불.
      MOVE 'FB03P' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " A/P 반제.
    WHEN 'C'.
      MOVE 'FB03R' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 업무가불.
      MOVE 'FB03P' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " A/P 반제.
  ENDCASE.

  IF ZTIMIMG00-ZFATIC EQ 'X'.
    READ TABLE IT_ZSBSEG INDEX 1.
    IF ZTBKPF-ZFCSTGRP EQ '003' AND IT_ZSBSEG-ZFCD EQ '1AB'.
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 저장.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 저장.
    ENDIF.
  ENDIF.

  IF ZTIMIMG00-ZFAVPT NE  'X'.
    MOVE 'FB03R' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
  ENDIF.

  IF ZTIMIMG00-ZFPYPT NE 'X'.
    MOVE 'FB03P' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
  ENDIF.

  DESCRIBE TABLE IT_ZSBHIS  LINES W_LINE.
  IF W_LINE EQ 0.
    MOVE 'HIFI' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">이력.
  ENDIF.

ENDFORM.                    " P2000_SET_STATUS_TCODE_DISABLE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_REQ_INIT_SCR
*&---------------------------------------------------------------------*
FORM P2000_SET_REQ_INIT_SCR.
* document
  MOVE 'HIST' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " HEADER CHANGE DOC
  MOVE 'UCUR' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 화폐변경.
  MOVE 'HIIT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ITEM CHANGE DOC.
  MOVE 'ZIMG' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " IMPORT IMG
  MOVE 'DDLC' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " DOUBLE CLICK
  MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 저장.
  MOVE 'CKEK' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 점검.
  MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 다른문서.
  MOVE 'EDIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " EDI SEND
  MOVE 'PREV' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " PREVIOUS DOC.
  MOVE 'NEXT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " NEXT DOCUMENT
* header
*  IF SY-DYNNR EQ '0300'.
  MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 삭제.
  MOVE 'REVK' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 취소.
*  ENDIF.
  MOVE 'CHFD'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 변경항목.
  MOVE 'DELR'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 접수취소.
  MOVE 'OPCL'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 확정취소.
  MOVE 'RECO'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 수입추소.
  MOVE 'ME23'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " p/o history
  MOVE 'MK03'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.         " Vendor
  MOVE 'MK23'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " p/o
  MOVE 'ZIM03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " p/o
  MOVE 'ZIM93' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " L/C history
  MOVE 'ZIBK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 개설은?
  MOVE 'ZIBE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Beneficiary
  MOVE 'ZICI'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 차입기?
* item
  MOVE 'MM03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 자?
  MOVE 'MD04' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 자재/소요?
  MOVE 'MMBE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 자재 개?
  MOVE 'MB51' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 자재별 자재문?
  MOVE 'ME2M' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 자재별 구매문?
  MOVE 'ME03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 소스 리스?
* env
  MOVE 'STAT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.          " STATUS
  MOVE 'FLAT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.          " FLAT DATA

ENDFORM.                    " P2000_SET_REQ_INIT_SCR
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SET_MESSAGE USING    P_SY_UCOMM.

  W_COMMAND =  P_SY_UCOMM.

  CASE P_SY_UCOMM.
    WHEN 'ANZG'.      " CHANGE  ==> DISPLAY
      PERFORM  P2000_ANZG_MESSAGE.
    WHEN 'SAVE'.      " Save
      PERFORM  P2000_SAVE_MESSAGE.
    WHEN 'CANC'.      " Cancel
      PERFORM  P2000_CANCEL_MESSAGE.
    WHEN 'BACK' OR 'EXIT'.
      PERFORM  P2000_EXIT_MESSAGE.
    WHEN 'DELE'.      " Delete
      PERFORM  P2000_DELETE_MESSAGE.
    WHEN 'OPDC'.
      PERFORM  P2000_POSTING_MESSAGE.
    WHEN 'CCDC'.
      PERFORM  P2000_FI_CANCEL_MESSAGE.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SAVE_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING '저장 확인'             " 타이틀...
                                          '입력된 내역을 저장합니다.'
                                        '저장하시겠습니까?' " Message #2
                                          'Y'                 " 취소 버?
                                     '1'.                      " default
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Save confirm '
             'Being saved entered item.'
             'Do you want to save?'
             'Y'
             '1'.
  ENDCASE.

ENDFORM.                    " P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_BOX USING    TITLE  LIKE SPOP-TITEL
                                TEXT1  LIKE SPOP-TEXTLINE1
                                TEXT2  LIKE SPOP-TEXTLINE2
                                CANCEL LIKE CANCEL_OPTION
                                DEFAULT LIKE OPTION.

  SPOP-TITEL = TITLE.
  SPOP-TEXTLINE1 = TEXT1.
  SPOP-TEXTLINE2 = TEXT2.
  IF CANCEL EQ 'Y'.
    CANCEL_OPTION = 'Y'.
  ELSE.
    CLEAR : CANCEL_OPTION.
  ENDIF.
  OPTION = DEFAULT.
  TEXTLEN = 40.

  IF W_COMMAND EQ 'CCDC'.
    IF BSIS-BUDAT IS INITIAL.
      BSIS-BUDAT = ZTBKPF-BUDAT.
      UF05A-STGRD = '03'.
    ENDIF.

    CALL SCREEN 0020 STARTING AT 30 2
                     ENDING   AT 78 11.

  ELSE.
    CALL SCREEN 0001 STARTING AT 30 6
                     ENDING   AT 78 10.

  ENDIF.

  IF ANTWORT = 'C'.                                         " Cancel
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_EXIT_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING
             '종료 확인'
             '현재 입력내역을 저장하지 않습니다.'
             '저장 후 종료하시겠습니까?'
             'Y'
             '1'.
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'End confirm'
             'Do not save the entered item.'
             'Do you want to end after save?'
             'Y'
             '1'.
  ENDCASE.

ENDFORM.                    " P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_CANCEL_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING '취소 확인'             " 타이틀...
                                   '변경된 내용을 저장없이 종료됩니다.'
                                        '종료하시겠습니까?' " Message #2
                                          'N'                 " 취소 버?
                                     '2'.                      " default
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Cancel confirm'
             'Being ended without saving the changed item.'
             'Do you want to end?'
             'N'
             '2'.
  ENDCASE.

  CASE ANTWORT.
    WHEN 'Y'.                                               " Yes...
      MESSAGE  S957.
      LEAVE TO SCREEN 0.  " " PROGRAM LEAVING
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_DELETE_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING '삭제 확인'             " 타이틀...
                                  '현재 Document를 삭제합니다.'
                                  '삭제하시겠습니까?'       " MSG2
                                  'N'                 " 취소 버튼 유/?
                                  '1'.                " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Delete confirm'
             'Being deleted the current document.'
             'Do you want to delete?'
             'N'
             '1'.
  ENDCASE.

ENDFORM.                    " P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_ANZG_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_ANZG_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING '이동 확인'             " 타이틀...
                                   '현재 입력내역을 저장하지 않습니다.'
                                       '저장 후 이동하시겠습니까?'" MSG2
                                          'Y'                 " 취소 버?
                                     '1'.                      " default
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Move confirm'
             'Do not save entered item.'
             'Do you want to move another screen after save?'
             'Y'
             '1'.
  ENDCASE.
ENDFORM.                    " P2000_ANZG_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_COMPANY_CODE
*&---------------------------------------------------------------------*
FORM P1000_GET_COMPANY_CODE USING    P_BUKRS   LIKE  BKPF-BUKRS.

  DATA : L_BUKRS  LIKE   BKPF-BUKRS.

  MOVE P_BUKRS  TO L_BUKRS.

  CALL FUNCTION 'COMPANY_CODE_READ'
       EXPORTING
            I_BUKRS = L_BUKRS
       IMPORTING
            E_T001  = T001
            E_T004  = T004
            E_T005  = T005
            E_T014  = T014
            E_T043  = T043
            E_T043T = T043T.

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
       EXPORTING
            I_BUKRS = T001-BUKRS
            I_LAND1 = T001-LAND1
            I_RCOMP = T001-RCOMP
       IMPORTING
            E_X001  = X001.

*------- Externe Buchungskreise ablehnen -------------------------------
  IF T001-XEXTB NE SPACE.
    MESSAGE E571(F5).
  ENDIF.

  IF T043T-BUKRS = SPACE.
    MESSAGE W103(F5) WITH P_BUKRS.
  ENDIF.

*  AUTHORITY-CHECK OBJECT F_BKPF_BUK
*    ID 'ACTVT' FIELD ACT_HINZ
*    ID 'BUKRS' FIELD BKPF-BUKRS.
*  IF SY-SUBRC NE 0.
*    MESSAGE E083(F5) WITH BKPF-BUKRS.
*  ENDIF.

*------- Kalkulationsschema lesen --------------------------------------
  IF TAX-BUKRS = SPACE.
    CALL FUNCTION 'FIND_TAX_SPREADSHEET'
         EXPORTING
              BUCHUNGSKREIS = T001-BUKRS
         IMPORTING
              SCHEMA        = T005-KALSM
         EXCEPTIONS
              NOT_FOUND     = 04.

    IF SY-SUBRC NE 0.
      MESSAGE E470(F5) WITH T001-BUKRS.
    ENDIF.
    CLEAR TTXD.
    SELECT SINGLE * FROM TTXD WHERE KALSM = T005-KALSM.
    T007A-KALSM = T005-KALSM.
  ELSE.
    T005-KALSM = T007A-KALSM.
  ENDIF.

  SET PARAMETER ID 'BUK' FIELD P_BUKRS.

  IF ZTBKPF-HWAER IS INITIAL.
    ZTBKPF-HWAER = T001-WAERS.
  ENDIF.

ENDFORM.                    " P1000_GET_COMPANY_CODE
*&---------------------------------------------------------------------*
*&      Form  SET_MODIFY_FIELD_CHECK
*&---------------------------------------------------------------------*
FORM SET_MODIFY_FIELD_CHECK.

  CHECK : W_EDIT_CHECK  NE 'Y'.

  CASE SY-TCODE.
    WHEN 'ZIMY2'.
      IF ZTBKPF-BELNR IS INITIAL.
        W_EDIT_CHECK = 'N'.
        EXIT.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  IF ZTBKPF NE *ZTBKPF.
    W_EDIT_CHECK = 'Y'.
  ENDIF.
  CHECK : W_EDIT_CHECK  NE 'Y'.


  LOOP AT IT_ZSBSEG.
    W_TABIX = SY-TABIX.
    READ TABLE IT_ZSBSEG_OLD INDEX W_TABIX.
    IF SY-SUBRC EQ 0.
      IF IT_ZSBSEG-ZFCD NE IT_ZSBSEG_OLD-ZFCD.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-ZFIMDNO NE IT_ZSBSEG_OLD-ZFIMDNO.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-WRBTR NE IT_ZSBSEG_OLD-WRBTR.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-ZUONR NE IT_ZSBSEG_OLD-ZUONR.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-SGTXT NE IT_ZSBSEG_OLD-SGTXT.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-KURSF NE IT_ZSBSEG_OLD-KURSF.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-WWERT NE IT_ZSBSEG_OLD-WWERT.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-NEWKO NE IT_ZSBSEG_OLD-NEWKO.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
    ELSE.
      W_EDIT_CHECK = 'Y'.  EXIT.
    ENDIF.
  ENDLOOP.
  CHECK : W_EDIT_CHECK  NE 'Y'.

  LOOP AT IT_ZSBSEG_OLD.
    W_TABIX = SY-TABIX.
    READ TABLE IT_ZSBSEG INDEX W_TABIX.
    IF SY-SUBRC EQ 0.
      IF IT_ZSBSEG-ZFCD NE IT_ZSBSEG_OLD-ZFCD.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-ZFIMDNO NE IT_ZSBSEG_OLD-ZFIMDNO.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-WRBTR NE IT_ZSBSEG_OLD-WRBTR.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-ZUONR NE IT_ZSBSEG_OLD-ZUONR.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-SGTXT NE IT_ZSBSEG_OLD-SGTXT.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-KURSF NE IT_ZSBSEG_OLD-KURSF.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-WWERT NE IT_ZSBSEG_OLD-WWERT.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
      IF IT_ZSBSEG-NEWKO NE IT_ZSBSEG_OLD-NEWKO.
        W_EDIT_CHECK = 'Y'.  EXIT.
      ENDIF.
    ELSE.
      W_EDIT_CHECK = 'Y'.  EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " SET_MODIFY_FIELD_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_DDLC_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_DDLC_PROCESS.

  ASSIGN (F)   TO    <FS_F>.

  CASE F.
    WHEN 'ZTBKPF-LIFNR'.
      PERFORM   P2000_VENDOR_DISPLAY USING F '지불처'.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " P2000_DDLC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INIT_SCREEN
*&---------------------------------------------------------------------*
FORM P2000_SET_INIT_SCREEN.

  W_OK_CODE = OK-CODE.
  ANTWORT = 'N'.

  IF W_EDIT_CHECK EQ 'Y'.
    IF NOT ( W_STATUS EQ C_REQ_D ).
      PERFORM P2000_SET_MESSAGE USING  'ANZG'.
      CASE ANTWORT.
        WHEN 'Y'.              " Yes...
          PERFORM  P3000_DB_MODIFY_SCRCOM.
          IF W_EDIT_CHECK NE 'Y'.
            MESSAGE S211.
          ENDIF.

          CLEAR OK-CODE.
        WHEN 'N'.              " No...
          MESSAGE  S957.
          CLEAR OK-CODE.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
    ENDIF.
  ELSE.
    MESSAGE S211.
  ENDIF.
  PERFORM   P2000_SET_TRANSACTION.

ENDFORM.                    " P2000_SET_INIT_SCREEN
*&---------------------------------------------------------------------*
*&      Form  P3000_DB_MODIFY_SCRCOM
*&---------------------------------------------------------------------*
FORM P3000_DB_MODIFY_SCRCOM.

  CASE SY-TCODE.
    WHEN 'ZIMY1' OR 'ZIMY2' OR 'ZIMY3'.
      PERFORM P3000_CHARGE_DOC_MODIFY.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P3000_DB_MODIFY_SCRCOM
*&---------------------------------------------------------------------*
*&      Form  P3000_CHARGE_DOC_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_CHARGE_DOC_MODIFY.

  IF W_OK_CODE NE 'DELE'.
    PERFORM   P2000_CONDITION_TYPE_CHECK.
    IF RF05A-AZSAL NE 0.
      MESSAGE W592.   EXIT.
    ENDIF.
*>> HYUNDAI NHJ COMMENT.
*    IF ZTBKPF-GSBER IS INITIAL.
*      MESSAGE  W977  WITH  'Input Business Area.'.
*      EXIT.
*    ENDIF.
  ENDIF.

* 생성일 경우 다음 번호를 채번..
*  IF W_STATUS EQ C_REQ_C.
*     ZTBKPF-GJAHR  =  ZTBKPF-BLDAT(4).
*     PERFORM   P2000_GET_NUMBER_NEXT  USING  'CD'  ZTBKPF-BELNR.
*  ENDIF.
  IF ZTBKPF-ZFCSTGRP EQ '006'. "통관비용.
    READ TABLE IT_ZSBSEG WITH KEY BUKRS = ZTBKPF-BUKRS
                                  BELNR = ZTBKPF-BELNR
                                  GJAHR = ZTBKPF-GJAHR
                                  ZFCSTGRP = ZTBKPF-ZFCSTGRP
                                  ZFCD  = '002'.
    IF SY-SUBRC NE 0.
      MOVE ZTBKPF-LIFNR TO     ZTBKPF-ZFVEN.
    ENDIF.
  ELSE.
    IF ZTBKPF-ZFCSTGRP NE '004' AND ZTBKPF-ZFCSTGRP NE '005'.
      MOVE ZTBKPF-LIFNR TO     ZTBKPF-ZFVEN.
    ENDIF.
  ENDIF.

  LOOP AT IT_ZSBSEG.
    W_TABIX = SY-TABIX.
    IF ZTBKPF-ZFRVSX EQ 'X'.   "> 역기표 여부.
      MOVE: '50' TO IT_ZSBSEG-NEWBS,
            'H'  TO IT_ZSBSEG-SHKZG.
    ELSE.
      MOVE: '40' TO IT_ZSBSEG-NEWBS,
            'S'  TO IT_ZSBSEG-SHKZG.
    ENDIF.
    MODIFY IT_ZSBSEG INDEX W_TABIX.
  ENDLOOP.

  CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
       EXPORTING
            W_OK_CODE     = W_OK_CODE
            BUKRS         = ZTBKPF-BUKRS
            GJAHR         = ZTBKPF-GJAHR
            ZFSTATUS      = W_STATUS
            W_ZTBKPF_OLD  = *ZTBKPF
            W_ZTBKPF      = ZTBKPF
       TABLES
            IT_ZSBSEG_OLD = IT_ZSBSEG_OLD
            IT_ZSBSEG     = IT_ZSBSEG
            IT_ZSBDIV     = IT_ZSBDIV
            IT_ZSBHIS     = IT_ZSBHIS
       CHANGING
            BELNR         = ZTBKPF-BELNR
       EXCEPTIONS
            ERROR_UPDATE.

  W_SUBRC = SY-SUBRC.
  IF W_SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  IF W_OK_CODE EQ 'DELE'.
    IF W_SUBRC NE  0.
      MESSAGE  E313.
    ELSE.
      MESSAGE  S581  WITH  ZTBKPF-BUKRS ZTBKPF-GJAHR ZTBKPF-BELNR.
      PERFORM  P2000_SET_LOCK_MODE USING 'U'.
      LEAVE TO SCREEN 0.
      CASE SY-TCODE.
        WHEN 'ZIMY1'.
          PERFORM P2000_DATA_INITIALIZE.
          PERFORM   P2000_SET_CREATE_FIELD_VALUE.
        WHEN 'ZIMY2' OR 'ZIMY3'.
          SET PARAMETER ID 'BUK'    FIELD ZTBKPF-BUKRS.
          SET PARAMETER ID 'GJR'    FIELD ZTBKPF-GJAHR.
          SET PARAMETER ID 'ZPBENR' FIELD ''.
          PERFORM P2000_DATA_INITIALIZE.
      ENDCASE.
    ENDIF.
  ELSE.
    IF W_SUBRC NE  0.
      MESSAGE  E208.
    ELSE.
      SET PARAMETER ID 'BUK'    FIELD ZTBKPF-BUKRS.
      SET PARAMETER ID 'GJR'    FIELD ZTBKPF-GJAHR.
      SET PARAMETER ID 'ZPBENR' FIELD ZTBKPF-BELNR.

      MESSAGE  S579  WITH  ZTBKPF-BUKRS ZTBKPF-GJAHR ZTBKPF-BELNR.
      IF SY-TCODE EQ 'ZIMY1'.
*>> INITIALIZE.
*           PERFORM   P2000_DATA_INITIALIZE.
*           PERFORM   P2000_SET_CREATE_FIELD_VALUE.
        IF W_OK_CODE NE 'OPDC'.
          LEAVE TO TRANSACTION 'ZIMY3'.
        ENDIF.
      ELSEIF SY-TCODE EQ 'ZIMY2'.
        IF W_OK_CODE NE 'OPDC'.
          PERFORM   P2000_SET_LOCK_MODE USING 'U'.
          LEAVE TO TRANSACTION 'ZIMY3'.
        ENDIF.
      ELSE.
        W_EDIT_CHECK = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_CHARGE_DOC_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_NUMBER_NEXT
*&---------------------------------------------------------------------*
FORM P2000_GET_NUMBER_NEXT       USING    P_GUBUN     P_DOCNO.

  CALL FUNCTION 'ZIM_NUMBER_GET_NEXT'
       EXPORTING
            ZFREQTY         = P_GUBUN
            GJAHR           = ZTBKPF-GJAHR
            BUKRS           = ZTBKPF-BUKRS
       IMPORTING
            ZFREQNO         = P_DOCNO
       EXCEPTIONS
            NOT_INPUT       = 1
            NOT_TYPE        = 2
            NOT_RANGE       = 3
            NOT_FOUND       = 4
            LOCKED          = 6
            ERROR_DUPLICATE = 8.

  CASE SY-SUBRC.
    WHEN 1.     MESSAGE    E012.
    WHEN 2.     MESSAGE    E013      WITH  P_GUBUN.
    WHEN 3.     MESSAGE    E014      WITH  P_DOCNO.
    WHEN 4.     MESSAGE    E964.
    WHEN 6.
      MESSAGE    E510      WITH
                   SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    WHEN 8.     MESSAGE    E015      WITH  P_DOCNO.
  ENDCASE.

ENDFORM.                    " P2000_GET_NUMBER_NEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_TRANSACTION
*&---------------------------------------------------------------------*
FORM P2000_SET_TRANSACTION.

  CASE SY-TCODE.
    WHEN 'ZIMY1' OR 'ZIMY2' OR 'ZIMY3'.          " B/L
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          CALL SCREEN 0010 STARTING AT 30 6
                           ENDING   AT 78 10.
        WHEN 'CRDC'.   " CREATE
          W_FIRST_SCR0100  = 'Y'.
          LEAVE TO TRANSACTION 'ZIMY1'.
        WHEN 'CHDC'.   " CHANGE
          W_FIRST_SCR0100  = 'Y'.
          LEAVE TO TRANSACTION 'ZIMY2'.
        WHEN 'DISP'.   " DISPLAY
          W_FIRST_SCR0100  = 'Y'.
          LEAVE TO TRANSACTION 'ZIMY3'.
        WHEN OTHERS.
      ENDCASE.
  ENDCASE.

ENDFORM.                    " P2000_SET_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_CHARGE_DOCUMENT
*&---------------------------------------------------------------------*
FORM P1000_GET_CHARGE_DOCUMENT USING    P_MODE.

  W_READ_ERROR = 'N'.

  IF SY-DYNNR EQ '0010'.
    SELECT SINGLE * INTO W_ZTBKPF FROM ZTBKPF
             WHERE BUKRS  EQ  ZSBKPF-BUKRS
             AND   BELNR  EQ  ZSBKPF-BELNR
             AND   GJAHR  EQ  ZSBKPF-GJAHR.
    IF SY-SUBRC NE 0.
      W_READ_ERROR = 'Y'.
      MESSAGE E582 WITH ZSBKPF-BUKRS ZSBKPF-GJAHR ZSBKPF-BELNR.
    ENDIF.
    IF SY-TCODE EQ 'ZIMY2'.
      IF W_ZTBKPF-ZFPOSYN EQ 'Y'.
        W_READ_ERROR = 'Y'.
        MESSAGE E584
                WITH ZSBKPF-BUKRS ZSBKPF-GJAHR ZSBKPF-BELNR.
      ELSEIF W_ZTBKPF-ZFPOSYN EQ 'P'.
        W_READ_ERROR = 'Y'.
        MESSAGE E584
                WITH ZSBKPF-BUKRS ZSBKPF-GJAHR ZSBKPF-BELNR.
      ENDIF.
    ENDIF.
  ENDIF.

  PERFORM P2000_DATA_INITIALIZE.

  CALL FUNCTION 'ZIM_GET_CHARGE_DOCUMENT'
       EXPORTING
            BUKRS                  = ZSBKPF-BUKRS
            BELNR                  = ZSBKPF-BELNR
            GJAHR                  = ZSBKPF-GJAHR
       IMPORTING
            W_ZTBKPF               = ZTBKPF
       TABLES
            IT_ZSBSEG              = IT_ZSBSEG
            IT_ZSBSEG_OLD          = IT_ZSBSEG_OLD
            IT_ZSBDIV              = IT_ZSBDIV
            IT_ZSBHIS              = IT_ZSBHIS
       EXCEPTIONS
            NOT_FOUND              = 4
            COMPANDYCODE_NOT_INPUT = 6
            DOCUMENT_NO_NOT_INPUT  = 8
            FISC_YEAR_NOT_INPUT    = 10.

  W_SUBRC = SY-SUBRC.

  CASE W_SUBRC.
    WHEN  0.
      W_ZFCSTGRP = ZTBKPF-ZFCSTGRP.

      SET PARAMETER ID 'BUK'    FIELD ZSBKPF-BUKRS.
      SET PARAMETER ID 'GJR'    FIELD ZSBKPF-GJAHR.
      SET PARAMETER ID 'ZPBENR' FIELD ZSBKPF-BELNR.

      CASE SY-TCODE.
        WHEN 'ZIMY2'.       ">변경일 경우.
          IF ZTBKPF-ZFPOSYN EQ 'Y'.
            MESSAGE ID 'ZIM' TYPE P_MODE NUMBER 584
                    WITH ZSBKPF-BUKRS ZSBKPF-GJAHR ZSBKPF-BELNR.

            IF P_MODE EQ 'S'.
*>> INITIALIZE.
              PERFORM P2000_DATA_INITIALIZE.
              PERFORM   P2000_SET_CREATE_FIELD_VALUE.
              W_READ_ERROR = 'Y'.
              EXIT.
            ENDIF.
          ENDIF.
*> 관세/부가세일 경우, 막음.
*               IF ZTBKPF-ZFCSTGRP EQ '006'.
*                  READ TABLE IT_ZSBSEG INDEX 1.
*                  IF SY-SUBRC EQ 0.
*                     IF IT_ZSBSEG-ZFCD EQ '001' OR  ">관세.
*                        IT_ZSBSEG-ZFCD EQ '003',
*                        SCREEN-INPUT     = '0'.
*                     ENDIF.
*                  ENDIF.
*               ENDIF.

          CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBKPF'
               EXPORTING
                    BUKRS  = ZSBKPF-BUKRS
                    GJAHR  = ZSBKPF-GJAHR
                    BELNR  = ZSBKPF-BELNR
               EXCEPTIONS
                    OTHERS = 1.

          IF SY-SUBRC <> 0.
            MESSAGE ID 'ZIM' TYPE P_MODE NUMBER 510
                WITH SY-MSGV1 'Charge Document'
                                  ZSBKPF-BUKRS ZSBKPF-BELNR.

            IF P_MODE EQ 'S'.
*>> INITIALIZE.
              PERFORM P2000_DATA_INITIALIZE.
              PERFORM   P2000_SET_CREATE_FIELD_VALUE.
              W_READ_ERROR = 'Y'.
              EXIT.
            ENDIF.
          ELSE.
            IF ZTBKPF-ZFCNAME IS INITIAL.
              GET PARAMETER ID 'ZPCNAME' FIELD ZTBKPF-ZFCNAME.
            ENDIF.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

      W_EDIT_CHECK = 'N'.

*> 비용 TEXT.
      PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                USING 'ZDCSTGRP'  ZTBKPF-ZFCSTGRP
                CHANGING          W_COST_TYPE.

*> GET 문서 TEXT.
      PERFORM P1000_IMPORT_DOC_CHEKC   USING ZTBKPF-ZFIMDNO
                                             W_ZFDCNM
                                             ZTBKPF-ZFPOYN
                                            'H'
                                             IT_ZSBSEG-KOSTL
                                             ''.
      LOOP AT IT_ZSBSEG.
        W_TABIX = SY-TABIX.

        PERFORM P1000_IMPORT_DOC_CHEKC   USING IT_ZSBSEG-ZFIMDNO
                                               IT_ZSBSEG-ZFDCNM
                                               IT_ZSBSEG-ZFPOYN
                                              'I'
                                               IT_ZSBSEG-KOSTL
                                               IT_ZSBSEG-ZFCD.
        MODIFY IT_ZSBSEG INDEX  W_TABIX.
      ENDLOOP.

*> 비용코드.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG08
                FROM ZTIMIMG08
                WHERE ZFCDTY EQ ZTBKPF-ZFCSTGRP.
*> 거래처 코드.
      IF ZTBKPF-LIFNR IS INITIAL.
        CLEAR : *LFA1.
      ELSE.
        SELECT SINGLE * INTO *LFA1
               FROM LFA1
               WHERE LIFNR EQ ZTBKPF-LIFNR.
      ENDIF.

      MOVE: ZTBKPF-ZFCSTGRP        TO   W_ZFCSTGRP.
      W_FIRST_SCR0100 = 'N'.
      MOVE-CORRESPONDING   ZTBKPF  TO   *ZTBKPF.
      IT_ZSBSEG_OLD[] = IT_ZSBSEG[].

*> FINBIL 문서 체크...
      READ TABLE IT_ZSBSEG INDEX 1.
      IF NOT IT_ZSBSEG-ZFDOCNO IS INITIAL.
        MESSAGE S666.
        IF SY-TCODE NE 'ZIMY3'.
          LEAVE TO TRANSACTION 'ZIMY3'.
        ENDIF.
        EXIT.
      ENDIF.
*> 관세 전가액.
      READ TABLE IT_ZSBSEG INDEX 1.
      IF ZTBKPF-ZFCSTGRP EQ '008'.
        MESSAGE S692.
        IF SY-TCODE NE 'ZIMY3'.
          LEAVE TO TRANSACTION 'ZIMY3'.
        ENDIF.
        EXIT.
      ENDIF.
*> 보험료 자동체크...
      READ TABLE IT_ZSBSEG INDEX 1.
      IF ZTIMIMG00-ZFATIC EQ 'X'.
        IF ZTBKPF-ZFCSTGRP EQ '003' AND IT_ZSBSEG-ZFCD EQ '1AB'.
          MESSAGE S607.
          IF SY-TCODE NE 'ZIMY3'.
            LEAVE TO TRANSACTION 'ZIMY3'.
          ENDIF.
          EXIT.
        ENDIF.
      ENDIF.

*> B/L 비용처리방법.
*         IF ZTIMIMG00-BLCSTMD EQ 'X'.
*            MESSAGE S933.
*            IF SY-TCODE NE 'ZIMY3'.
*               LEAVE TO TRANSACTION 'ZIMY3'.
*            ENDIF.
*         ENDIF.

*> 관세/부가세 체크...
*          READ TABLE IT_ZSBSEG INDEX 1.
*          IF ( ZTBKPF-ZFCSTGRP EQ '006' AND IT_ZSBSEG-ZFCD EQ '001' )
*          OR ( ZTBKPF-ZFCSTGRP EQ '006' AND IT_ZSBSEG-ZFCD EQ '002' )
*          OR ( ZTBKPF-ZFCSTGRP EQ '006' AND IT_ZSBSEG-ZFCD EQ '003' ).
*             CLEAR : ZSBSEG.
*             IF IT_ZSBSEG-ZFCD EQ '001'.
*                MESSAGE S618 WITH 'Customs'.
*             ELSEIF IT_ZSBSEG-ZFCD EQ '002'.
*                MESSAGE S618 WITH 'Clearance commission'.
*             ELSEIF IT_ZSBSEG-ZFCD EQ '003'.
*                MESSAGE S618 WITH 'Advance VAT'.
*             ENDIF.
*             IF SY-TCODE NE 'ZIMY3'.
*                LEAVE TO TRANSACTION 'ZIMY3'.
*             ENDIF.
*             EXIT.
*          ENDIF.
    WHEN  4.
      W_READ_ERROR = 'Y'.
      MESSAGE ID 'ZIM' TYPE P_MODE NUMBER 582
              WITH ZSBKPF-BUKRS ZSBKPF-GJAHR ZSBKPF-BELNR.
    WHEN  6.
      W_READ_ERROR = 'Y'.
      MESSAGE ID 'ZIM' TYPE P_MODE NUMBER 583
              WITH 'Company code'.
    WHEN  8.
      W_READ_ERROR = 'Y'.
      MESSAGE ID 'ZIM' TYPE P_MODE NUMBER 583
              WITH 'Import expense Doc No'.
    WHEN 10.
      W_READ_ERROR = 'Y'.
      MESSAGE ID 'ZIM' TYPE P_MODE NUMBER 583
              WITH 'Fiscal year'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P1000_GET_CHARGE_DOCUMENT
*&---------------------------------------------------------------------*
*&      Module  PERIOD_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE PERIOD_CHECK_SCR0100 INPUT.
  DATA: S_MONAT LIKE BKPF-MONAT.      "Save field for input period

  CHECK : W_STATUS    NE C_REQ_D.
  CHECK : W_EXIT_MODE NE 'Y'.

*> 증빙일.
  IF ZTBKPF-BLDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BLDAT'.
  ENDIF.
  IF ZTBKPF-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUDAT'.
  ENDIF.

  IF ZTBKPF-ZFBDT IS INITIAL.
    ZTBKPF-ZFBDT = ZTBKPF-BUDAT.
  ENDIF.

*>> Insert 2004.01.17
  IF ZTBKPF-GJAHR NE ZTBKPF-BUDAT(4) AND
     W_STATUS    NE C_REQ_C.
     MESSAGE E815(ZIM1) WITH ZTBKPF-GJAHR.
  ENDIF.

  CLEAR ZTBKPF-GJAHR.
  S_MONAT = ZTBKPF-MONAT.

  PERFORM PERIODE_ERMITTELN USING ZTBKPF-BUDAT
                                  ZTBKPF-GJAHR
                                  ZTBKPF-MONAT.

  IF NOT S_MONAT IS INITIAL AND S_MONAT NE ZTBKPF-MONAT AND
   ( SY-BINPT = SPACE AND SY-CALLD = SPACE ).
    MESSAGE W000(F5) WITH S_MONAT ZTBKPF-BUDAT ZTBKPF-MONAT.
  ENDIF.

ENDMODULE.                 " PERIOD_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  PERIODE_ERMITTELN
*&---------------------------------------------------------------------*
FORM PERIODE_ERMITTELN USING I_BUDAT LIKE BKPF-BUDAT
                             E_GJAHR LIKE BKPF-GJAHR
                             E_MONAT LIKE BKPF-MONAT.
*
  CALL FUNCTION 'FI_PERIOD_DETERMINE'
       EXPORTING
            I_BUKRS = T001-BUKRS
            I_BUDAT = I_BUDAT
            I_MONAT = E_MONAT
            I_GJAHR = E_GJAHR
       IMPORTING
            E_GJAHR = E_GJAHR
            E_MONAT = E_MONAT.

ENDFORM.                    " PERIODE_ERMITTELN
*&---------------------------------------------------------------------*
*&      Form  P2000_BELEGART_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_BELEGART_AUTHORITY_CHECK.

  CHECK T003-BRGRU NE SPACE.

  AUTHORITY-CHECK OBJECT F_BKPF_BLA
    ID 'ACTVT' FIELD ACT_HINZ
    ID 'BRGRU' FIELD T003-BRGRU.

  CHECK SY-SUBRC NE 0.
  MESSAGE E087(F5) WITH ZTBKPF-BLART.

ENDFORM.                    " P2000_BELEGART_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_COST_ITEM
*&---------------------------------------------------------------------*
FORM P1000_GET_COST_ITEM.

  REFRESH : IT_ZSBSEG.
  CLEAR : IT_ZSBSEG.

  PERFORM   GET_DD07T_SELECT(SAPMZIM01)
            USING 'ZDCSTGRP'  ZTBKPF-ZFCSTGRP
            CHANGING          W_COST_TYPE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG08
                FROM ZTIMIMG08
                WHERE ZFCDTY EQ ZTBKPF-ZFCSTGRP.
  IF SY-SUBRC NE 0.
    MESSAGE E591 WITH ZTBKPF-ZFCSTGRP.
  ENDIF.


*   LOOP AT IT_ZSIMIMG08.
*      CLEAR : IT_ZSBSEG.
*      MOVE-CORRESPONDING IT_ZSIMIMG08  TO  IT_ZSBSEG.
*      MOVE: ZTBKPF-KURSF               TO  IT_ZSBSEG-KURSF,
*            ZTBKPF-WWERT               TO  IT_ZSBSEG-WWERT,
*            ZTBKPF-ZFCSTGRP            TO  IT_ZSBSEG-ZFCSTGRP,
*            '40'                       TO  IT_ZSBSEG-NEWBS,
*            'S'                        TO  IT_ZSBSEG-SHKZG.
*
*      CASE ZTBKPF-ZFCSTGRP.
*         WHEN '003'.
*            MOVE: ZTIMIMG11-ZFIOCAC1  TO IT_ZSBSEG-NEWKO.
*         WHEN '004'.
*            MOVE: ZTIMIMG11-ZFIOCAC2  TO IT_ZSBSEG-NEWKO.
*         WHEN '005'.
*            MOVE: ZTIMIMG11-ZFIOCAC2  TO IT_ZSBSEG-NEWKO.
*         WHEN '006'.
*            MOVE: ZTIMIMG11-ZFIOCAC4  TO IT_ZSBSEG-NEWKO.
*         WHEN '007'.
*            MOVE: ZTIMIMG11-ZFIOCAC12 TO IT_ZSBSEG-NEWKO.
*         WHEN OTHERS.
*      ENDCASE.
*
*      APPEND IT_ZSBSEG.
*   ENDLOOP.
ENDFORM.                    " P1000_GET_COST_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_ITEM_REFRESH_MSG
*&---------------------------------------------------------------------*
FORM P2000_ITEM_REFRESH_MSG.

  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX
      USING '아이템 갱신 확인'
            '현재 문서의 비용항목을 재갱신합니다.'
            '계속 진행하시겠습니까?'
            'N'
            '1'.
  ELSE.
    PERFORM P2000_MESSAGE_BOX
      USING 'Confirm Item Update'
            'Update again Charge Item in current document.'
            'Continue?'
            'N'
            '1'.
  ENDIF.

ENDFORM.                    " P2000_ITEM_REFRESH_MSG
*&---------------------------------------------------------------------*
*&      Form  P2000_NO_INPUT
*&---------------------------------------------------------------------*
FORM P2000_NO_INPUT USING    NOI_TAB NOI_FIELD.

  DATA: BEGIN OF FELD,
          TABLE(10),
          STRICH,
          NAME(10),
        END OF FELD.

  FELD-TABLE = NOI_TAB.
  FELD-STRICH = '-'.
  CONDENSE FELD.
  SEARCH FELD FOR '-'.
  SY-FDPOS = SY-FDPOS - 1.
  WRITE '-' TO FELD+SY-FDPOS.
  SY-FDPOS = SY-FDPOS + 1.
  WRITE NOI_FIELD TO FELD+SY-FDPOS.
  IF SY-STEPL EQ 0.
    SET CURSOR FIELD FELD.
  ELSE.
    SET CURSOR FIELD FELD LINE SY-STEPL.
  ENDIF.

  PERFORM GET_FTEXT(RDDFIE00) USING NOI_TAB NOI_FIELD SY-LANGU
                              CHANGING DFIES W_SUBRC.

  IF W_SUBRC EQ 0.
    MESSAGE S083(ME) WITH DFIES-SCRTEXT_M.
  ELSE.
    MESSAGE S083(ME) WITH NOI_TAB NOI_FIELD.
  ENDIF.

ENDFORM.                    " P2000_NO_INPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_IMPORT_DOC_CHEKC
*&---------------------------------------------------------------------*
FORM P1000_IMPORT_DOC_CHEKC USING    P_ZFIMDNO
                                     P_ZFDCNM
                                     P_ZFPOYN
                                     P_GUBUN
                                     P_KOSTL
                                     P_ZFCD.

  DATA  : W_TEXT(25) TYPE C.
  CLEAR : P_ZFDCNM, ZTREQHD, ZTBL, ZTIV, ZTCGHD, ZTMSHD.

  IF P_ZFIMDNO IS INITIAL.
    EXIT.
  ENDIF.

  CASE ZTBKPF-ZFCSTGRP.
    WHEN '003'.           ">수입의뢰.
      SELECT SINGLE * FROM ZTREQHD
                      WHERE ZFREQNO EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      MOVE: ZTREQHD-ZFOPNNO TO P_ZFDCNM.

      WRITE  ZTREQHD-ZFLASTAM  TO  W_TEXT CURRENCY ZTREQHD-WAERS
                                   LEFT-JUSTIFIED.
      CONCATENATE  W_TEXT ZTREQHD-WAERS INTO W_TEXT
                   SEPARATED BY SPACE.

      P_ZFPOYN = 'Y'.
* CORECESS 주석처리.
*         IF W_SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTREQHD-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA.
*         ENDIF.
*> 한수원 PROCESS < 2002.11.08 - NHJ >
      IF ZTREQHD-ZFMATGB EQ '4'.
        IF ZTBKPF-ZFCSTGRP NE '007'.
          MESSAGE E173(ZIM1).
          EXIT.
        ENDIF.
      ENDIF.

    WHEN '004' OR '005' OR '007'.  ">B/L 관리번호.
      SELECT SINGLE * FROM ZTBL
                      WHERE ZFBLNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      IF W_SUBRC EQ 0.
        P_ZFPOYN = ZTBL-ZFPOYN.
        P_KOSTL  = ZTBL-KOSTL.
      ELSE.
        P_ZFPOYN = 'Y'.
      ENDIF.

      W_TEXT = ZTBL-ZFHBLNO.

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
*> 한수원 PROCESS < 2002.11.08 - NHJ >
      IF ZTBL-ZFMATGB EQ '4'.
        IF ZTBKPF-ZFCSTGRP NE '007'.
          MESSAGE E173(ZIM1).
          EXIT.
        ENDIF.
      ENDIF.

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
                          WHERE ZFIVNO  EQ P_ZFIMDNO.
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

      IF SY-SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
        P_KOSTL  = ZTBL-KOSTL.
        P_ZFPOYN = ZTBL-ZFPOYN.
        W_TEXT   = ZTBL-ZFHBLNO.
        IF P_GUBUN EQ 'I'.
          IF P_ZFCD  EQ '003' AND ZTBKPF-ZFCSTGRP EQ '006'.
            SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
                   FROM T134G
                   WHERE WERKS EQ ZTBL-ZFWERKS.
          ELSE.
* CORECESS 주석처리.
*                  CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                       EXPORTING
*                          UNAME   =    SY-UNAME
*                          WERKS   =    ZTBL-ZFWERKS
*                       IMPORTING
*                          GSBER   =    ZTBKPF-GSBER
*                          BUPLA   =    ZTBKPF-BUPLA.
          ENDIF.
        ENDIF.
      ENDIF.
*> 한수원 PROCESS < 2002.11.08 - NHJ >
      IF NOT ( P_ZFCD  EQ '003' OR P_ZFCD IS INITIAL ).
        IF ZTBL-ZFMATGB EQ '4'.
          IF ZTBKPF-ZFCSTGRP NE '007'.
            MESSAGE E173(ZIM1).
            EXIT.
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
  IF W_SUBRC NE 0.
    IF P_GUBUN EQ 'H'.
      PERFORM P2000_NO_INPUT USING 'ZTBKPF' 'ZFIMDNO'.
    ELSE.
      PERFORM P2000_NO_INPUT USING 'ZSBSEG' 'ZFIMDNO'.
    ENDIF.

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
  ELSE.
    IF SY-TCODE NE 'ZIMY3'.
      ZTBKPF-BKTXT = W_TEXT.
    ENDIF.
  ENDIF.

ENDFORM.                    " P1000_IMPORT_DOC_CHEKC
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_NEWKO
*&---------------------------------------------------------------------*
FORM P2000_SET_NEWKO   USING ZSBSEG-NEWKO
                             ZSBSEG-ZFCD
                             ZSBSEG-ZFIMDNO.
*> 계정결정 함수.
  CALL FUNCTION 'ZIM_GET_NODRAFT_ACCOUNT'
       EXPORTING
            ZFCSTGRP = ZSBSEG-ZFCSTGRP
            ZFCD     = ZSBSEG-ZFCD
            ZFIMDNO  = ZSBSEG-ZFIMDNO
       IMPORTING
            NEWKO    = ZSBSEG-NEWKO.


*   CASE ZTBKPF-ZFCSTGRP.
*      WHEN '003'.
*         MOVE: ZTIMIMG11-ZFIOCAC1  TO ZSBSEG-NEWKO.
*      WHEN '004' OR '005'.
*         SELECT SINGLE * FROM ZTBL
*                WHERE  ZFBLNO EQ ZSBSEG-ZFIMDNO.
*
*         IF ZTBL-ZFPOYN NE 'N'.
*            MOVE: ZTIMIMG11-ZFIOCAC2  TO ZSBSEG-NEWKO.
*         ELSE.
*
*            CASE ZTBL-ZFPOTY.
*               WHEN 'S'.   ">Sample B/L.
*                  MOVE: ZTIMIMG11-ZFIOCAC20  TO ZSBSEG-NEWKO.
*               WHEN 'P'.   ">대체용.
*                  MOVE: ZTIMIMG11-ZFIOCAC14  TO ZSBSEG-NEWKO.
*               WHEN 'B'.   ">보상용.
*                  MOVE: ZTIMIMG11-ZFIOCAC15  TO ZSBSEG-NEWKO.
*               WHEN 'A'.   ">수리용.
*                  MOVE: ZTIMIMG11-ZFIOCAC16  TO ZSBSEG-NEWKO.
*               WHEN 'H'.   ">Ship. Back(영업반품).
*                  MOVE: ZTIMIMG11-ZFIOCAC17  TO ZSBSEG-NEWKO.
*               WHEN 'Z'.   ">기타.
*                  MOVE: ZTIMIMG11-ZFIOCAC3   TO ZSBSEG-NEWKO.
*            ENDCASE.
*         ENDIF.
*      WHEN '006'.
*         SELECT SINGLE * FROM ZTBL
*                WHERE ZFBLNO EQ ( SELECT ZFBLNO
*                                  FROM ZTIV
*                                  WHERE  ZFBLNO EQ ZSBSEG-ZFIMDNO ).
*
*         IF ZTBL-ZFPOYN NE 'N'.
*            MOVE: ZTIMIMG11-ZFIOCAC4  TO ZSBSEG-NEWKO.
*         ELSE.
*            CASE ZTBL-ZFPOTY.
*               WHEN 'S'.   ">Sample B/L.
*                  MOVE: ZTIMIMG11-ZFIOCAC20  TO ZSBSEG-NEWKO.
*               WHEN 'P'.   ">대체용.
*                  MOVE: ZTIMIMG11-ZFIOCAC14  TO ZSBSEG-NEWKO.
*               WHEN 'B'.   ">보상용.
*                  MOVE: ZTIMIMG11-ZFIOCAC15  TO ZSBSEG-NEWKO.
*               WHEN 'A'.   ">수리용.
*                  MOVE: ZTIMIMG11-ZFIOCAC16  TO ZSBSEG-NEWKO.
*               WHEN 'H'.   ">Ship. Back(영업반품).
*                  MOVE: ZTIMIMG11-ZFIOCAC17  TO ZSBSEG-NEWKO.
*               WHEN 'Z'.   ">기타.
*                  MOVE: ZTIMIMG11-ZFIOCAC5   TO ZSBSEG-NEWKO.
*            ENDCASE.
*         ENDIF.
**> 부가세일 경우.
*         IF ZSBSEG-ZFCD EQ '003'.
*            MOVE: ZTIMIMG11-ZFIOCAC6   TO ZSBSEG-NEWKO.
*         ENDIF.
*      WHEN '007'.
*         MOVE: ZTIMIMG11-ZFIOCAC12 TO ZSBSEG-NEWKO.
*      WHEN '008'.
*         MOVE: ZTIMIMG11-ZFIOCAC13 TO ZSBSEG-NEWKO.
*      WHEN OTHERS.
*         CLEAR : ZSBSEG-NEWKO.
*   ENDCASE.

ENDFORM.                    " P2000_SET_NEWKO
*&---------------------------------------------------------------------*
*&      Form  SET_OBJ_ICON
*&---------------------------------------------------------------------*
FORM SET_OBJ_ICON USING      P_ICON
                             P_FIELD
                             P_INFO.

  CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = P_ICON
            INFO                  = P_INFO
       IMPORTING
            RESULT                =  P_FIELD.

ENDFORM.                    " SET_OBJ_ICON
*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_EXIT_PROCESS.

  IF NOT W_STATUS EQ C_REQ_D.
    PERFORM P2000_SET_MESSAGE USING  OK-CODE.
    CASE ANTWORT.
      WHEN 'Y'.              " Yes...
*-----------------------------------------------------------------------
* DB Write
*-----------------------------------------------------------------------
        PERFORM  P3000_DB_MODIFY_SCRCOM.
        CLEAR OK-CODE.
        PERFORM  P2000_SET_LOCK_MODE USING 'U'.
*           PERFORM  P2000_SET_SCREEN_SCRCOM.
*           LEAVE SCREEN.
      WHEN 'N'.              " No...
        MESSAGE  S957.
        CLEAR OK-CODE.
        IF W_STATUS NE C_REQ_D.
          PERFORM  P2000_SET_LOCK_MODE USING 'U'.
        ENDIF.
        PERFORM  P2000_SET_SCREEN_SCRCOM.
        LEAVE SCREEN.
      WHEN 'C'.              " Cancel
        MESSAGE  S957.
      WHEN OTHERS.
    ENDCASE.

  ELSE.
    CLEAR OK-CODE.
    PERFORM  P2000_SET_SCREEN_SCRCOM.
    LEAVE SCREEN.
  ENDIF.

ENDFORM.                    " P2000_EXIT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_LOCK_MODE
*&---------------------------------------------------------------------*
FORM P2000_SET_LOCK_MODE USING    PA_MODE.

  CASE SY-TCODE.
    WHEN 'ZIMY1' OR 'ZIMY2' OR 'ZIMY3'.    " CHARGE DOC.
      PERFORM P2000_SET_CHARGE_DOC_LOCK    USING   PA_MODE.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_LOCK_MODE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_CHARGE_DOC_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_CHARGE_DOC_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBKPF'
         EXPORTING
              BUKRS  = ZTBKPF-BUKRS
              GJAHR  = ZTBKPF-GJAHR
              BELNR  = ZTBKPF-BELNR
         EXCEPTIONS
              OTHERS = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Charge Document'
                            ZTBKPF-BUKRS ZTBKPF-BELNR.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBKPF'
         EXPORTING
              BUKRS = ZTBKPF-BUKRS
              GJAHR = ZTBKPF-GJAHR
              BELNR = ZTBKPF-BELNR.
  ENDIF.

ENDFORM.                    " P2000_SET_CHARGE_DOC_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SCREEN_SCRCOM
*&---------------------------------------------------------------------*
FORM P2000_SET_SCREEN_SCRCOM.
* CALL Transaction일 경우, 프로그램 Leave
*  IF SY-CALLD EQ 'X'.
  LEAVE TO SCREEN 0.
*  ENDIF.

ENDFORM.                    " P2000_SET_SCREEN_SCRCOM
*&---------------------------------------------------------------------*
*&      Form  P2000_BALANCE_CALCULATE
*&---------------------------------------------------------------------*
FORM P2000_BALANCE_CALCULATE.

  RF05A-AZSAL = ( ZTBKPF-WRBTR - ZTBKPF-WMWST ) * -1.

  LOOP AT IT_ZSBSEG.
    W_TABIX = SY-TABIX.

    ADD IT_ZSBSEG-WRBTR TO RF05A-AZSAL.
    RF05A-AZSAL =  RF05A-AZSAL - IT_ZSBSEG-WMWST.

    IF SY-TABIX EQ 1.
      ZTBKPF-ZFPOYN = IT_ZSBSEG-ZFPOYN.
    ENDIF.
    IF ZTBKPF-ZFPOYN NE IT_ZSBSEG-ZFPOYN.
      ZTBKPF-ZFPOYN = 'M'.
    ENDIF.
  ENDLOOP.

  IF RF05A-AZSAL NE 0.
    MOVE : ICON_RED_LIGHT         TO RF05A-AMPEL,
           'No Zero balance' TO TEXT_080.
  ELSE.
    MOVE : ICON_GREEN_LIGHT  TO RF05A-AMPEL,
           'No Zero balance' TO TEXT_080.
  ENDIF.

  PERFORM SET_OBJ_ICON        USING RF05A-AMPEL
                              RF05A-AMPEL TEXT_080.

ENDFORM.                    " P2000_BALANCE_CALCULATE
*&---------------------------------------------------------------------*
*&      Form  P2000_VENDOR_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_VENDOR_DISPLAY USING    P_VENDOR_CODE
                                   P_TEXT.

  IF P_VENDOR_CODE IS INITIAL.
    MESSAGE S167 WITH P_TEXT.   EXIT.
  ENDIF.
* 화면 PARAMETER ID
*  SET PARAMETER ID 'KDY' FIELD '/320/310/130/120/110'.
  SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
  SET PARAMETER ID 'LIF' FIELD P_VENDOR_CODE.
  SET PARAMETER ID 'EKO' FIELD ''.
*   EXPORT 'LIF'   TO MEMORY ID 'LIF'.
*   EXPORT 'EKO'   TO MEMORY ID 'EKO'.

  CALL TRANSACTION 'MK03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_VENDOR_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_HEADER_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P2000_HEADER_CHANGE_DOC.

  CASE SY-DYNNR.
    WHEN '0100'.          "> CHARGE DOC.
      OBJECTCLASS   =   'ZTBKPF'.
      OBJEKTID      =   ZTBKPF+3(18).
    WHEN OTHERS.   EXIT.
  ENDCASE.

  SUBMIT  RCS00120 WITH  OBJEKT   =   OBJECTCLASS
                   WITH  OBJEKTID =   OBJEKTID
                   AND   RETURN.

ENDFORM.                    " P2000_HEADER_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_ITEM_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P2000_ITEM_CHANGE_DOC.

  CHECK : W_COUNT EQ 1.

  CASE SY-DYNNR.
    WHEN '0100'.          "> CHARGE DOC.
      OBJECTCLASS   =   'ZTBSEG'.
      OBJEKTID      =   ZTBSEG+3(21).
    WHEN OTHERS.   EXIT.
  ENDCASE.

  SUBMIT  RCS00120 WITH  OBJEKT   =   OBJECTCLASS
                   WITH  OBJEKTID =   OBJEKTID
                   AND   RETURN.

ENDFORM.                    " P2000_ITEM_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_SELECT_ITEM
*&---------------------------------------------------------------------*
FORM P2000_SELECT_ITEM.

  CLEAR : W_COUNT.
  LOOP AT IT_ZSBSEG WHERE ZFMARK = 'X'.
    W_TMP_TABIX = SY-TABIX.
    ADD 1   TO   W_COUNT.
    MOVE-CORRESPONDING IT_ZSBSEG  TO   ZTBSEG.
    MOVE : ZTBKPF-BUKRS           TO   ZTBSEG-BUKRS,
           ZTBKPF-GJAHR           TO   ZTBSEG-GJAHR,
           ZTBKPF-BELNR           TO   ZTBSEG-BELNR.
  ENDLOOP.

  CASE W_COUNT.
    WHEN 0.        MESSAGE S951. EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965. EXIT.
  ENDCASE.

ENDFORM.                    " P2000_SELECT_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_ITEM_REF_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_ITEM_REF_DOC_DISPLAY.

  CHECK : W_COUNT EQ 1.
  CHECK : NOT ZTBSEG-ZFIMDNO IS INITIAL.

  CASE ZTBSEG-ZFCSTGRP.
    WHEN '003'.
      SET PARAMETER ID 'BES'     FIELD ''.
      SET PARAMETER ID 'ZPREQNO' FIELD ZTBSEG-ZFIMDNO.
      SET PARAMETER ID 'ZPOPNNO' FIELD ''.
      SET PARAMETER ID 'ZPINSEQ' FIELD ZTBSEG-ZFINSEQ.
      SET PARAMETER ID 'ZPAMDNO' FIELD ZTBSEG-ZFAMDNO.

      IF ZTBSEG-ZFCD EQ '1AB'.
        IF ZTBSEG-ZFAMDNO IS INITIAL.
          CALL TRANSACTION 'ZIM43' AND SKIP  FIRST SCREEN.
        ELSE.
          CALL TRANSACTION 'ZIM47' AND SKIP  FIRST SCREEN.
        ENDIF.
      ELSE.
        CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
      ENDIF.

    WHEN '004' OR '005'.
      SET PARAMETER ID 'ZPBLNO'  FIELD ZTBSEG-ZFIMDNO.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.

      CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
    WHEN '006'.
      CLEAR : ZTIV.
      SELECT SINGLE * FROM  ZTIV
                      WHERE ZFIVNO  EQ  ZTBSEG-ZFIMDNO.
      IF SY-SUBRC EQ 0.
        CASE ZTIV-ZFCUST.
          WHEN '1' OR 'X'.
            SET PARAMETER ID 'ZPIVNO'  FIELD ZTBSEG-ZFIMDNO.
            SET PARAMETER ID 'ZPBLNO'  FIELD ''.
            SET PARAMETER ID 'ZPHBLNO' FIELD ''.

            CALL TRANSACTION 'ZIM33' AND SKIP  FIRST SCREEN.
          WHEN '2' OR '3'.
            SELECT SINGLE * FROM  ZTIDRUS
                   WHERE ZFIVNO  EQ  ZTBSEG-ZFIMDNO.
            IF SY-SUBRC EQ 0.
              SET PARAMETER ID 'ZPIVNO'  FIELD ZTIDRUS-ZFIVNO.
              SET PARAMETER ID 'ZPCLSEQ' FIELD ZTIDRUS-ZFCLSEQ.

              CALL TRANSACTION 'ZIMCD3' AND SKIP  FIRST SCREEN.
            ENDIF.

          WHEN 'Y'.
            SELECT SINGLE * FROM  ZTIDSUS
                   WHERE ZFIVNO  EQ  ZTBSEG-ZFIMDNO.
            IF SY-SUBRC EQ 0.
              SET PARAMETER ID 'ZPIVNO'  FIELD ZTIDSUS-ZFIVNO.
              SET PARAMETER ID 'ZPCLSEQ' FIELD ZTIDSUS-ZFCLSEQ.
              SET PARAMETER ID 'ZPHBLNO' FIELD ''.
              SET PARAMETER ID 'ZPENTNO' FIELD ''.

              CALL TRANSACTION 'ZIMCC3' AND SKIP  FIRST SCREEN.
            ENDIF.
          WHEN OTHERS.
            EXIT.
        ENDCASE.
      ELSE.
      ENDIF.

    WHEN '007'.
      SET PARAMETER ID 'ZPBLNO'  FIELD ''.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      SET PARAMETER ID 'ZPCGPT'  FIELD ''.
      SET PARAMETER ID 'ZPCGNO'  FIELD ZTBSEG-ZFIMDNO.

      CALL TRANSACTION 'ZIM83' AND SKIP  FIRST SCREEN.
    WHEN '008'.
      SET PARAMETER ID 'BES'     FIELD ''.
      SET PARAMETER ID 'ZPREQNO' FIELD ''.
      SET PARAMETER ID 'ZPOPNNO' FIELD ''.
      SET PARAMETER ID 'ZPINSEQ' FIELD ''.
      SET PARAMETER ID 'ZPTBNO'  FIELD ZTBSEG-ZFIMDNO.
      CALL TRANSACTION 'ZIMZ3' AND SKIP  FIRST SCREEN.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " P2000_ITEM_REF_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INDICATE
*&---------------------------------------------------------------------*
FORM P2000_SET_INDICATE.

  IF RF05A-AZSAL NE 0 AND OK-CODE NE 'DELE'.
    MESSAGE E592.   EXIT.
  ENDIF.

  W_OK_CODE = OK-CODE.

  ">> Data Delete -> FI Posting Check.
  IF OK-CODE EQ 'DELE' AND
     ( ZTBKPF-ZFPOSYN EQ 'Y' OR ZTBKPF-ZFPOSYN EQ 'P' OR
       ZTBKPF-ZFPOSYN EQ 'C' ).
    MESSAGE E584 WITH ZTBKPF-BUKRS ZTBKPF-BELNR ZTBKPF-GJAHR.
  ENDIF.
  IF W_STATUS EQ C_REQ_D.
    PERFORM  P2000_SET_LOCK_MODE  USING  'L'.
  ENDIF.

* MESSAGE BOX
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.                                               " Yes...
      IF W_OK_CODE EQ 'OPDC'.
        IF W_STATUS NE C_REQ_D.
          PERFORM  P3000_DB_MODIFY_SCRCOM.
          IF W_SUBRC EQ 0.
            PERFORM  P3000_FI_POSTING.
          ENDIF.
        ELSE.
          PERFORM  P3000_FI_POSTING.
        ENDIF.

      ELSEIF W_OK_CODE EQ 'CCDC'.
        PERFORM  P3000_FI_DOC_CANCEL.
      ELSEIF W_OK_CODE EQ 'DELE'.
        PERFORM  P3000_DB_MODIFY_SCRCOM.
      ELSE.
        CLEAR OK-CODE.
        PERFORM  P2000_SET_LOCK_MODE USING 'U'.
      ENDIF.
      LEAVE SCREEN.
    WHEN OTHERS.              " No...
      IF W_STATUS EQ C_REQ_D.
        PERFORM  P2000_SET_LOCK_MODE USING 'U'.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_SET_INDICATE
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK USING    P_OK_CODE.

*   CASE P_OK_CODE.
*     WHEN 'OPDC' OR 'CCDC'.
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIMY1'
*                        ID 'ZIACTVT' FIELD '*'.
*     WHEN OTHERS.
*  ENDCASE.
*
*  IF SY-SUBRC NE 0.
*     CASE P_OK_CODE.
*        WHEN 'DELE'.            " 삭제.
*           MESSAGE E077 WITH SY-UNAME 'delete'.
*        WHEN 'OPDC'.            " 전기.
*           MESSAGE E077 WITH SY-UNAME 'Accounting posting'.
*        WHEN 'CCDC'.            " 전기취소.
*           MESSAGE E077 WITH SY-UNAME 'Posting cancel'.
*     ENDCASE.
*  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_INITIALIZE
*&---------------------------------------------------------------------*
FORM P2000_DATA_INITIALIZE.

  CLEAR : ZTBKPF, *ZTBKPF, W_ZFCSTGRP, W_ZFDCNM, W_COST_TYPE,
          IT_ZSBSEG, IT_ZSBSEG_OLD, IT_ZSBDIV, IT_ZSBHIS,
         *LFA1.

  REFRESH : IT_ZSBSEG, IT_ZSBSEG_OLD, IT_ZSBDIV, IT_ZSBHIS.

  W_EDIT_CHECK = 'N'.

  MOVE: 0                        TO RF05A-AZSAL,
        ICON_YELLOW_LIGHT        TO RF05A-AMPEL,
        'Not yet decide balance' TO TEXT_080.

  PERFORM SET_OBJ_ICON        USING RF05A-AMPEL
                              RF05A-AMPEL TEXT_080.
  CLEAR W_ZFCSTGRP.

ENDFORM.                    " P2000_DATA_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_CREATE_FIELD_VALUE
*&---------------------------------------------------------------------*
FORM P2000_SET_CREATE_FIELD_VALUE.

  GET PARAMETER ID 'BUK'    FIELD ZTBKPF-BUKRS.

  IF NOT ZTBKPF-BUKRS IS INITIAL.
*>> COMMPANY CODE CHECK.
    PERFORM  P1000_GET_COMPANY_CODE USING ZTBKPF-BUKRS.
  ENDIF.

  MOVE : SY-MANDT    TO      ZTBKPF-MANDT,
         SY-DATUM    TO      ZTBKPF-BUDAT,
         SPACE       TO      ZTBKPF-BLART,
         SPACE       TO      ZTBKPF-ZLSPR,
         'Y'         TO      ZTBKPF-ZFPOYN,
         SPACE       TO      ZTBKPF-TBTKZ,
         'X'         TO      ZTBKPF-ZFPCUR,
         SY-UNAME    TO      ZTBKPF-ZFCNAME,
         'N'         TO      ZTBKPF-ZFPOSYN,
         'USD'       TO      ZTBKPF-WAERS.
  IF ZTBKPF-HWAER IS INITIAL.
    MOVE : 'USD' TO ZTBKPF-HWAER.
  ENDIF.

  MOVE: 0                        TO RF05A-AZSAL,
        ICON_YELLOW_LIGHT        TO RF05A-AMPEL,
        'Not yet decide balance' TO TEXT_080.

  PERFORM SET_OBJ_ICON        USING RF05A-AMPEL
                              RF05A-AMPEL TEXT_080.

  CLEAR W_ZFCSTGRP.

  MOVE-CORRESPONDING  ZTBKPF  TO  *ZTBKPF.

ENDFORM.                    " P2000_SET_CREATE_FIELD_VALUE
*&---------------------------------------------------------------------*
*&      Form  P3000_FI_POSTING
*&---------------------------------------------------------------------*
FORM P3000_FI_POSTING.

  REFRESH : IT_ERR_LIST.

  IF W_READ_ERROR EQ 'Y' OR ZTBKPF-BELNR IS INITIAL.
    MESSAGE  S587.
    PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                 USING   'E'.
    EXIT.
  ENDIF.

  IF ZTBKPF-ZFPOSYN EQ 'Y'.
    MESSAGE  S578.
    PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                 USING   'E'.
    EXIT.
  ENDIF.

  "----------------------------------------------
  " Posting Check
  "----------------------------------------------
  DESCRIBE  TABLE IT_ZSBDIV LINES W_LINE.
  IF W_LINE EQ 1.
     MESSAGE  S004(ZIM1).
     PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                  USING   'E'.
     EXIT.
  ENDIF.

  CLEAR : W_LINE.
  LOOP  AT  IT_ZSBDIV  WHERE  NEWKO EQ SPACE.
     W_LINE  =  W_LINE  +  1.
  ENDLOOP.
  IF W_LINE EQ 1.
     MESSAGE  S004(ZIM1).
     PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                  USING   'E'.
     EXIT.
  ENDIF.

  ">> Header Data Get.
  SELECT SINGLE *
    FROM ZTBKPF
   WHERE BUKRS  EQ  ZTBKPF-BUKRS
     AND BELNR  EQ  ZTBKPF-BELNR
     AND GJAHR  EQ  ZTBKPF-GJAHR.

  ">>AP Function Call
  IF ZTBKPF-ZFOVROW NE 'X'.
     CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_POST'
          EXPORTING
               BUKRS      = ZTBKPF-BUKRS
               BELNR      = ZTBKPF-BELNR
               GJAHR      = ZTBKPF-GJAHR
          TABLES
               RETURN     = RETURN
         EXCEPTIONS
               POST_ERROR = 4.
  ELSE.
     ">> Customs Clearance Cost AP Function Call
     CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_POST_CR'
          EXPORTING
               BUKRS      = ZTBKPF-BUKRS
               BELNR      = ZTBKPF-BELNR
               GJAHR      = ZTBKPF-GJAHR
          TABLES
               RETURN     = RETURN
         EXCEPTIONS
               POST_ERROR = 4.
  ENDIF.

  W_SUBRC = SY-SUBRC.

  ">> Error Occured.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    IF RETURN[] IS INITIAL.
      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'E'.
    ELSE.
      PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
    ENDIF.
  ELSE.
    COMMIT WORK.
    PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
  ENDIF.

  DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
  IF W_LINE GT 0.
    INCLUDE = 'POPU'.
    CALL SCREEN 0014 STARTING AT  04   3
                     ENDING   AT  100 12.
    CLEAR : INCLUDE.
  ENDIF.

  PERFORM  P2000_SET_LOCK_MODE  USING  'U'.
  IF SY-CALLD EQ 'X'.
    LEAVE PROGRAM.
  ELSE.
    LEAVE TO TRANSACTION 'ZIMY3'.
  ENDIF.

ENDFORM.                    " P3000_FI_POSTING
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*&      Form  P3000_FI_DOC_CANCEL
*&---------------------------------------------------------------------*
FORM P3000_FI_DOC_CANCEL.

  REFRESH : IT_ERR_LIST.

  IF W_READ_ERROR EQ 'Y' OR
     ZTBKPF-BELNR IS INITIAL.
    MESSAGE  S587.
    PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                 USING   'E'.
    EXIT.
  ENDIF.

  IF ZTBKPF-ZFPOSYN EQ 'N'.
    MESSAGE  S588.
    PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                 USING   'E'.
    EXIT.
  ENDIF.

*>> AP POSTING FUNCTION CALL
  CALL FUNCTION 'ZIM_BAPI_COST_INVOICES_CANCEL'
       EXPORTING
         P_ZFIMDTY        =     'CS'
         P_BUKRS          =     ZTBKPF-BUKRS
         INVOICEDOCNUMBER =     ZTBKPF-ZFACDO
         FISCALYEAR       =     ZTBKPF-ZFFIYR
         REASONREVERSAL   =     UF05A-STGRD
         POSTINGDATE      =     BSIS-BUDAT
         ZTBKPF           =     ZTBKPF
      TABLES
         RETURN           =     RETURN
      EXCEPTIONS
         LIV_ERROR        =     4.

  W_SUBRC = SY-SUBRC.

  IF SY-SUBRC NE 0.           ">> Error Occured..
    IF RETURN[] IS INITIAL.
      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'E'.
    ELSE.
      PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
    ENDIF.
  ELSE.
    PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
  ENDIF.

  DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
  IF W_LINE GT 0.
    INCLUDE = 'POPU'.
    CALL SCREEN 0014 STARTING AT  04   3
                     ENDING   AT  100 12.
    CLEAR : INCLUDE.
  ENDIF.

  IF W_SUBRC EQ 0.
    PERFORM  P2000_SET_LOCK_MODE  USING  'U'.
    IF SY-CALLD EQ 'X'.
      LEAVE PROGRAM.
    ELSE.
      LEAVE TO TRANSACTION 'ZIMY3'.
    ENDIF.
    W_FIRST_SCR0100 = 'Y'.
  ELSE.
    IF W_STATUS EQ C_REQ_D.
      PERFORM  P2000_SET_LOCK_MODE  USING  'L'.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_FI_DOC_CANCEL
*&---------------------------------------------------------------------*
*&      Form  P2000_POSTING_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POSTING_MESSAGE.
  IF W_STATUS EQ C_REQ_D.
    IF SY-LANGU EQ '3'.
      PERFORM P2000_MESSAGE_BOX USING '전기 확인'
                                      '회계 전기됩니다.'
                                      '전기하시겠습니까?'
                                      'N'
                                      '1'.
    ELSE.
      PERFORM P2000_MESSAGE_BOX USING
             'Posting confirm'
             'Now do post.'
             'Do you want to post?'
             'N'
             '1'.
    ENDIF.
  ELSE.
    IF SY-LANGU EQ '3'.
      PERFORM P2000_MESSAGE_BOX USING '전기 확인'
                                     '변경된 내용을 저장후 전기됩니다.'
                                      '전기하시겠습니까?'
                                      'N'
                                      '1'.
    ELSE.
      PERFORM P2000_MESSAGE_BOX USING
             'Posting confirm'
             'Now do post after save.'
             'Do you want to post?'
             'N'
             '1'.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_POSTING_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_FI_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_FI_CANCEL_MESSAGE.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-CSREALYN EQ 'X'.
    ANTWORT  =  'Y'.
  ELSE.
    IF SY-LANGU EQ '3'.
      PERFORM P2000_MESSAGE_BOX USING
              '전기취소 확인'
              '전기된 내용의 역분개를 수행합니다.'
              '전기하시겠습니까?'
              'N'
              '1'.
    ELSE.
      PERFORM P2000_MESSAGE_BOX USING
             'Reverse posting confirm'
             'Now do reverse posting.'
             'Do you want to posting?'
             'N'
             '1'.
    ENDIF.
  ENDIF.
ENDFORM.                    " P2000_FI_CANCEL_MESSAGE
*&--------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&--------------------------------------------------------------------*
FORM TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = GT_LIST_TOP_OF_PAGE.

ENDFORM.                    "TOP_OF_PAGE
*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'CANC' OR 'CNCL' OR 'CANCL'.
      IF INCLUDE EQ 'BLHELP'.
         W_ZFBLNO = IT_ZSREQHD-ZFBLNO.
         ANTWORT = 'Y'.
      ELSE.
         ANTWORT = 'C'.
      ENDIF.
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
  ENDCASE.

*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
  CASE INCLUDE.
    WHEN 'POPU'.
      IF NOT IT_ERR_LIST-MSGTYP IS INITIAL.
        MESSAGE ID IT_ERR_LIST-MSGID TYPE 'I'
                NUMBER IT_ERR_LIST-MSGNR
                WITH   IT_ERR_LIST-MSGV1
                       IT_ERR_LIST-MSGV2
                       IT_ERR_LIST-MSGV3
                       IT_ERR_LIST-MSGV4.
      ENDIF.
      CLEAR : IT_ERR_LIST.
    WHEN 'TAXBKPO'.
      W_EBELN     = IT_ZSTAXBKHD-EBELN.
      W_ZFREQNO   = IT_ZSTAXBKHD-ZFREQNO.
      W_ZFOPNNO   = IT_ZSTAXBKHD-BASISNO.
      W_ZFTBNO    = IT_ZSTAXBKHD-ZFTBNO.
      ANTWORT = 'Y'.
    WHEN 'BLHELP'.
        W_ZFBLNO = IT_ZSREQHD-ZFBLNO.
        ANTWORT = 'Y'.
    WHEN OTHERS.
  ENDCASE.
  SET SCREEN 0.  LEAVE SCREEN.

*&---------------------------------------------------------------------*
*&      Form  P2000_FI_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_FI_DOCUMENT_DISPLAY USING    P_BUKRS
                                        P_GJAHR
                                        P_BELNR.
  IF P_BELNR IS INITIAL.
    MESSAGE S589.   EXIT.
  ELSE.
*>>> LIV 전표번호인지, 회계전표인지를 구분.
    SELECT * FROM EKBZ UP TO 1 ROWS
             WHERE BELNR EQ P_BELNR
             AND   GJAHR EQ P_GJAHR.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      SELECT * FROM EKBE UP TO 1 ROWS
               WHERE BELNR EQ P_BELNR
               AND   GJAHR EQ P_GJAHR.
      ENDSELECT.
    ENDIF.
    IF SY-SUBRC EQ 0.
       SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
       SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
       SET PARAMETER ID 'RBN'    FIELD P_BELNR.
       CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
    ELSE.
      SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
      SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
      SET PARAMETER ID 'BLN'    FIELD P_BELNR.
      CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_FI_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZFREQNO_HELP
*&---------------------------------------------------------------------*
FORM P1000_GET_ZFREQNO_HELP   USING   ZSBKFP-ZFIMDNO.

  CLEAR : ZSBKPF-ZFIMDNO.
  IF SY-LANGU EQ '3'.
    SPOP-TITEL = '수입의뢰번호 HELP'.
  ELSE.
    SPOP-TITEL = 'Import request doc.no. HELP'.
  ENDIF.
  OPTION = '1'.

  CALL SCREEN 0030 STARTING AT 20 6
                   ENDING   AT 75 10.

  IF ANTWORT EQ 'Y'.
    IF W_STATUS NE C_REQ_D.
      ZSBKPF-ZFIMDNO = ZTREQHD-ZFREQNO.
    ENDIF.
  ENDIF.

ENDFORM.                    " P1000_GET_ZFREQNO_HELP
*&---------------------------------------------------------------------*
*&      Form  P2000_CHECK_COST_GROUP
*&---------------------------------------------------------------------*
FORM P2000_CHECK_COST_GROUP USING    W_ERR_MODE.

  W_ERR_MODE = 'Y'.

  IF ZTBKPF-ZFCSTGRP IS INITIAL.
    PERFORM P2000_NO_INPUT USING 'ZTBKPF' 'ZFCSTGRP'.
    EXIT.
  ELSE.
    CLEAR : DD07T.
    SELECT * FROM DD07T WHERE DOMNAME     EQ 'ZDCSTGRP'
                        AND   DDLANGUAGE  EQ SY-LANGU
                        AND   AS4LOCAL    EQ 'A'
                        AND   DOMVALUE_L  EQ ZTBKPF-ZFCSTGRP
                        ORDER BY AS4VERS DESCENDING.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      IF SY-LANGU = '3'.
        MESSAGE S593 WITH 'Cost Group'.
      ELSE.
        MESSAGE S593 WITH 'Cost Group'.
      ENDIF.
      EXIT.
    ENDIF.

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

  W_ERR_MODE = 'N'.

ENDFORM.                    " P2000_CHECK_COST_GROUP
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZFBLNO_HELP
*&---------------------------------------------------------------------*
FORM P1000_GET_ZFBLNO_HELP USING    ZSBKPF_ZFIMDNO.

  CLEAR : ZSBKPF-ZFIMDNO.
  IF SY-LANGU EQ '3'.
    SPOP-TITEL = 'B/L 관리번호 HELP'.
  ELSE.
    SPOP-TITEL = 'B/L doc. no. HELP'.
  ENDIF.
  OPTION = '1'.

  CALL SCREEN 0040 STARTING AT 15 6
                   ENDING   AT 65 10.

  IF ANTWORT EQ 'Y'.
    IF W_STATUS NE C_REQ_D.
      ZSBKPF-ZFIMDNO = ZTBL-ZFBLNO.
    ENDIF.
  ENDIF.

ENDFORM.                    " P1000_GET_ZFBLNO_HELP
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
FORM P2000_BL_DOC_ITEM_SELECT.
  W_BLNO    = ZSREQHD-ZFBLNO.
  W_HBLNO   = ZSREQHD-ZFHBLNO.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZTBL
           WHERE  ZFHBLNO EQ ZSREQHD-ZFHBLNO
           ORDER  BY ZFBLNO.

  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ELSEIF TFILL = 1.
    READ TABLE IT_ZSREQHD INDEX 1.
    W_BLNO  = IT_ZSREQHD-ZFBLNO.
    W_HBLNO = IT_ZSREQHD-ZFHBLNO.
    ANTWORT = 'Y'.
    EXIT.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'BLHELP'.                 " BL HELP.
  CALL SCREEN 0014 STARTING AT  07 3
                   ENDING   AT  87 15.

ENDFORM.                    " P2000_BL_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BL_DOC
*&---------------------------------------------------------------------*
FORM P1000_READ_BL_DOC.

  CALL FUNCTION 'ZIM_GET_BL_DOCUMENT'
       EXPORTING
            ZFBLNO          = ZTBL-ZFBLNO
       IMPORTING
            W_ZTBL          = ZTBL
       TABLES
            IT_ZSBLCST      = IT_ZSBLCST
            IT_ZSBLCST_ORG  = IT_ZSBLCST_ORG
            IT_ZSBLCST1     = IT_ZSBLCST1
            IT_ZSBLCST1_ORG = IT_ZSBLCST1_ORG
            IT_ZSBLCON      = IT_ZSBLCON
            IT_ZSBLCON_ORG  = IT_ZSBLCON_ORG
            IT_ZSBLIT       = IT_ZSBLIT
            IT_ZSBLIT_ORG   = IT_ZSBLIT_ORG
       EXCEPTIONS
            NOT_FOUND       = 4
            NOT_INPUT       = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E038 WITH ZTBL-ZFBLNO.
    WHEN 8.
      MESSAGE E039.
  ENDCASE.

ENDFORM.                    " P1000_READ_BL_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_SEARCH_FIELD_MOVE
*&---------------------------------------------------------------------*
FORM P2000_SEARCH_FIELD_MOVE.

  ZSREQHD-ZFREQNO = W_ZFREQNO.
  ZSREQHD-ZFAMDNO = W_ZFAMDNO.
  ZSREQHD-ZFOPNNO = W_ZFOPNNO.
  ZSREQHD-EBELN   = W_EBELN.
  ZSREQHD-ZFBLNO  = W_ZFBLNO.
  ZSREQHD-ZFHBLNO = W_HBLNO.
  ZSCIVHD-ZFCIVRN = W_ZFCIVRN.
  ZSCIVHD-ZFCIVNO = W_ZFCIVNO.
  ZSCGHD-ZFCGNO   = W_ZFCGNO.
  ZSIV-ZFIVNO     = W_ZFIVNO.
  ZSREQHD-ZFTBNO  = W_ZFTBNO.

ENDFORM.                    " P2000_SEARCH_FIELD_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_SIMULATION_LIST
*&---------------------------------------------------------------------*
FORM P2000_SIMULATION_LIST.

  IF RF05A-AZSAL NE 0.
    MESSAGE E592.   EXIT.
  ENDIF.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTBDIV
  FROM     ZTBDIV
  WHERE    BUKRS  EQ  ZTBKPF-BUKRS
  AND      GJAHR  EQ  ZTBKPF-GJAHR
  AND      BELNR  EQ  ZTBKPF-BELNR.

*  CALL FUNCTION 'ZIM_CHARGE_ITEM_CREATE'
*       EXPORTING
*            BUKRS     = ZTBKPF-BUKRS
*            GJAHR     = ZTBKPF-GJAHR
*            BELNR     = ZTBKPF-BELNR
*            ZTBKPF    = ZTBKPF
*       TABLES
*            IT_ZSBSEG = IT_ZSBSEG
*            IT_ZTBDIV = IT_ZTBDIV.
*
  LEAVE TO SCREEN '0110'.

ENDFORM.                    " P2000_SIMULATION_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZFIVNO_HELP
*&---------------------------------------------------------------------*
FORM P1000_GET_ZFIVNO_HELP USING    ZSBKPF-ZFIMDNO.

  CLEAR : ZSBKPF-ZFIMDNO.
  IF SY-LANGU EQ '3'.
    SPOP-TITEL = '통관 관리번호 HELP'.
  ELSE.
    SPOP-TITEL = 'Clearance/ G.R req. No. HELP'.
  ENDIF.
  OPTION = '1'.

  CALL SCREEN 0050 STARTING AT 20 6
                   ENDING   AT 60 9.

  IF ANTWORT EQ 'Y'.
    IF W_STATUS NE C_REQ_D.
      ZSBKPF-ZFIMDNO = ZTIV-ZFIVNO.
    ENDIF.
  ENDIF.

ENDFORM.                    " P1000_GET_ZFIVNO_HELP
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZFCGNO_HELP
*&---------------------------------------------------------------------*
FORM P1000_GET_ZFCGNO_HELP USING    ZSBKPF-ZFIMDNO.

  CLEAR : ZSBKPF-ZFIMDNO.
  IF SY-LANGU EQ '3'.
    SPOP-TITEL = '하역 관리번호 HELP'.
  ELSE.
    SPOP-TITEL = 'Loading/Unloading doc.no. HELP'.
  ENDIF.
  OPTION = '1'.

  CALL SCREEN 0060 STARTING AT 20 6
                   ENDING   AT 55 9.

  IF ANTWORT EQ 'Y'.
    IF W_STATUS NE C_REQ_D.
      ZSBKPF-ZFIMDNO = ZTCGHD-ZFCGNO.
    ENDIF.
  ENDIF.

ENDFORM.                    " P1000_GET_ZFCGNO_HELP
*&---------------------------------------------------------------------*
*&      Form  P2000_CONDITION_TYPE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_CONDITION_TYPE_CHECK.
  DATA : L_ZFDCSTX   LIKE   ZTBKPF-ZFDCSTX,
         L_ZFPOYN    LIKE   ZTBKPF-ZFPOYN.

  CHECK W_STATUS   NE  C_REQ_D.
  CLEAR : L_ZFDCSTX, W_COUNT.
  LOOP AT IT_ZSBSEG.
    IF SY-TABIX EQ 1.
      L_ZFDCSTX = IT_ZSBSEG-ZFDCSTX.
      L_ZFPOYN  = IT_ZSBSEG-ZFPOYN.
    ENDIF.
    IF L_ZFDCSTX NE IT_ZSBSEG-ZFDCSTX.
      MESSAGE E605.
    ENDIF.

*> 유환/무환 여부.
*      IF L_ZFPOYN  NE IT_ZSBSEG-ZFPOYN.
*         MESSAGE E696.
*      ENDIF.
*      IF ZTBKPF-ZFPOYN NE IT_ZSBSEG-ZFPOYN.
*         MESSAGE E696.
*      ENDIF.

    L_ZFPOYN  = IT_ZSBSEG-ZFPOYN.
    L_ZFDCSTX = IT_ZSBSEG-ZFDCSTX.
    ADD 1 TO W_COUNT.
  ENDLOOP.

  IF W_COUNT EQ 0.
    MESSAGE E604.
  ENDIF.

  MOVE : L_ZFDCSTX TO ZTBKPF-ZFDCSTX.

ENDFORM.                    " P2000_CONDITION_TYPE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_ITEM_DETAIL_SCREEN
*&---------------------------------------------------------------------*
FORM P2000_ITEM_DETAIL_SCREEN.

  CHECK : W_COUNT EQ 1.

  MOVE-CORRESPONDING   ZTBSEG   TO   ZSBSEG.
  LEAVE TO SCREEN 0120.

ENDFORM.                    " P2000_ITEM_DETAIL_SCREEN
*&---------------------------------------------------------------------*
*&      Form  P2000_POSTING_HISTORY
*&---------------------------------------------------------------------*
FORM P2000_POSTING_HISTORY.
  DESCRIBE TABLE IT_ZSBDIV LINES W_LINE.

  IF W_LINE GT 0.
    LEAVE TO SCREEN 0130.
  ENDIF.

ENDFORM.                    " P2000_POSTING_HISTORY
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM P2000_USER_COMMAND USING R_UCOMM     LIKE SY-UCOMM
                              RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
    WHEN  'WAHL'.                      "menubutton
      READ TABLE IT_ZSBHIS INDEX RS_SELFIELD-TABINDEX. "cursorposit.
      IF SY-SUBRC EQ 0.
        PERFORM  P2000_FI_DOCUMENT_DISPLAY
                               USING   IT_ZSBHIS-BUKRS
                                       IT_ZSBHIS-ZFGJAHR
                                       IT_ZSBHIS-ZFBELNR.
      ELSE.
        MESSAGE S962.
      ENDIF.
      CLEAR R_UCOMM.
    WHEN '&IC1'.                       "doubleclick
      READ TABLE IT_ZSBHIS INDEX RS_SELFIELD-TABINDEX. "cursorposit.
      IF SY-SUBRC EQ 0.
        PERFORM  P2000_FI_DOCUMENT_DISPLAY
                               USING   IT_ZSBHIS-BUKRS
                                       IT_ZSBHIS-ZFGJAHR
                                       IT_ZSBHIS-ZFBELNR.
      ELSE.
        MESSAGE S962.
      ENDIF.
      CLEAR R_UCOMM.
    WHEN 'WAHL1'.    ">업무가불 문서.
      READ TABLE IT_ZSBHIS INDEX RS_SELFIELD-TABINDEX. "cursorposit.
      IF SY-SUBRC EQ 0.
        IF IT_ZSBHIS-ZFBELNR1 IS INITIAL.
          MESSAGE S207(ZIM1).
        ELSE.
          PERFORM  P2000_FI_DOCUMENT_DISPLAY
                                USING   IT_ZSBHIS-BUKRS
                                        IT_ZSBHIS-ZFGJAHR1
                                        IT_ZSBHIS-ZFBELNR1.
        ENDIF.
      ELSE.
        MESSAGE S962.
      ENDIF.
      CLEAR R_UCOMM.
  ENDCASE.
ENDFORM.                    "P2000_USER_COMMAND
*&--------------------------------------------------------------------*
*&      Form  P2000_SET_STATUS
*&--------------------------------------------------------------------*
FORM P2000_SET_STATUS  USING  EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'STANDA02' EXCLUDING EXTAB.

ENDFORM.                    "P2000_SET_STATUS
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_DOC_SELECT
*&---------------------------------------------------------------------*
FORM P2000_MULTI_DOC_SELECT.
  TABLES INDX.
  DATA: INDXKEY  LIKE INDX-SRTFD VALUE 'ZKEYVALUE',
        INDXKEY1 LIKE INDX-SRTFD VALUE 'ZKEYVALUE1',
        WA_INDX  TYPE INDX.

*>>> MEMOFY ID SET
  EXPORT ZFCSTGRP = ZTBKPF-ZFCSTGRP
         TO DATABASE INDX(ST)  FROM WA_INDX ID INDXKEY.

*>> 문서 다중 선택 프로그램 CALL.
  SUBMIT ZRIMCSTDOC VIA SELECTION-SCREEN
                    AND RETURN.

*>>> MEMOFY ID GET
  IMPORT IT_ZFIMDNO = IT_ZFIMDNO
         FROM DATABASE INDX(ST)  ID INDXKEY1 TO WA_INDX.
  FREE MEMORY ID INDXKEY1.

*>> 기존 마지막 ROW GET
  DESCRIBE TABLE IT_ZSBSEG LINES W_LINE.
  IF W_LINE GT 0.
    READ TABLE IT_ZSBSEG INDEX W_LINE.
  ELSE.
    CLEAR : IT_ZSBSEG.
  ENDIF.

*>> 해당 값 SET.
  LOOP AT IT_ZFIMDNO.
    MOVE-CORRESPONDING IT_ZFIMDNO TO IT_ZSBSEG.
    MOVE : ZTBKPF-MWSKZ           TO IT_ZSBSEG-MWSKZ,
           ZTBKPF-WWERT           TO IT_ZSBSEG-WWERT,
           ZTBKPF-KURSF           TO IT_ZSBSEG-KURSF.
    APPEND IT_ZSBSEG.
  ENDLOOP.

ENDFORM.                    " P2000_MULTI_DOC_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
FORM P2000_DOC_ITEM_SELECT.

  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFTBNO  = ZSREQHD-ZFTBNO.
* INPUT VALUE 보존을 위해...
  W_ZSREQHD = ZSREQHD.

  DESCRIBE TABLE IT_ZSTAXBKHD LINES TFILL.
  IF TFILL = 0.   MESSAGE E406.   ENDIF.

  W_STATUS_CHK = 'C'.
  INCLUDE = 'TAXBKPO'.                 " L/C 변?
  CALL SCREEN 0014 STARTING AT  09 3
                   ENDING   AT  90 15.

ENDFORM.                    " P2000_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_TAXPO_DISPLAY_LIST
*&---------------------------------------------------------------------*
FORM P2000_TAXPO_DISPLAY_LIST.

  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  WRITE : / IT_ZSTAXBKHD-EBELN   NO-GAP,    SY-VLINE NO-GAP,
            IT_ZSTAXBKHD-ZFREQNO NO-GAP,    SY-VLINE NO-GAP,
            IT_ZSTAXBKHD-BASISNO NO-GAP,    SY-VLINE NO-GAP,
            IT_ZSTAXBKHD-ZFBUYDT NO-GAP,    SY-VLINE NO-GAP,
            IT_ZSTAXBKHD-ZFTBNO  NO-GAP,    SY-VLINE NO-GAP,
            IT_ZSTAXBKHD-ZFTBPNO NO-GAP,    SY-VLINE.

ENDFORM.                    " P2000_TAXPO_DISPLAY_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_TAXBOOK_READ
*&---------------------------------------------------------------------*
FORM P1000_TAXBOOK_READ.

* 수입의뢰 Header Table
  CALL FUNCTION 'ZIM_GET_TAX_BOOK'
       EXPORTING
            ZFTBNO           = ZSREQHD-ZFTBNO
       IMPORTING
            ZTTAXBKHD        = ZTTAXBKHD
       TABLES
            IT_ZSTAXBKIT     = IT_ZSTAXBKIT
            IT_ZSTAXBKIT_ORG = IT_ZSTAXBKIT_ORG
       EXCEPTIONS
            NOT_FOUND        = 4
            NOT_INPUT        = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E685 WITH ZSREQHD-ZFREQNO.
    WHEN 8.
      MESSAGE E686.
  ENDCASE.

  IF ZTTAXBKHD-ZFPOSYN EQ 'Y'.
    MESSAGE E687 WITH ZTTAXBKHD-ZFTBNO 'Accounting transaction'.
  ENDIF.

   *ZTTAXBKHD = ZTTAXBKHD.

ENDFORM.                    " P1000_TAXBOOK_READ
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZFTBNO_HELP
*&---------------------------------------------------------------------*
FORM P1000_GET_ZFTBNO_HELP USING    ZSBKPF_ZFIMDNO.

  CLEAR : ZSBKPF-ZFIMDNO.
  SPOP-TITEL = 'CTM No HELP'.
  OPTION = '1'.

  CALL SCREEN 0070 STARTING AT 10  2
                   ENDING   AT 65  8.

  IF ANTWORT EQ 'Y'.
    IF W_STATUS NE C_REQ_D.
      ZSBKPF-ZFIMDNO = ZTTAXBKHD-ZFTBNO.
    ENDIF.
  ENDIF.

ENDFORM.                    " P1000_GET_ZFTBNO_HELP
*&---------------------------------------------------------------------*
*&      Module  GET_ZFTRNO_SCR0080  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ZFTRNO_SCR0080 INPUT.

  CLEAR : IT_ZSTRIT.
  REFRESH : IT_ZSTRIT.

  SELECT SINGLE * FROM ZTTRHD WHERE  ZFTRNO EQ ZTTRHD-ZFTRNO.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSTRIT
  FROM   ZTTRIT
  WHERE  ZFTRNO EQ ZTTRHD-ZFTRNO.

ENDMODULE.                 " GET_ZFTRNO_SCR0080  INPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZFTRNO_HELP
*&---------------------------------------------------------------------*
FORM P1000_GET_ZFTRNO_HELP USING    P_ZSBKPF_ZFIMDNO.

  CLEAR : ZSBKPF-ZFIMDNO.

  SPOP-TITEL = '출고 관리번호 HELP'.
  OPTION = '1'.

  CALL SCREEN 0080 STARTING AT 10  2
                   ENDING   AT 65  8.

  IF ANTWORT EQ 'Y'.
    IF W_STATUS NE C_REQ_D.
      ZSBKPF-ZFIMDNO = ZTTRHD-ZFTRNO.
    ENDIF.
  ENDIF.

ENDFORM.                    " P1000_GET_ZFTRNO_HELP
