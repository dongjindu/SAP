*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM01O01                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입 B/L 관련 PBO MODULE Include                      *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.12                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE PF_STATUS_SCRCOM OUTPUT.

  REFRESH : IT_EXCL.              " Inactive Function용 Internal Table

*-----------------------------------------------------------------------
* GUI TITLE TEXT SETTING
*-----------------------------------------------------------------------
  PERFORM   P2000_SET_GUI_TEXT.

*-----------------------------------------------------------------------
* PF-STATUS Setting
*-----------------------------------------------------------------------
  PERFORM    P2000_SET_PF_STATUS.

*-----------------------------------------------------------------------
* PF-STATUS Inactive Function Setting ( TRANSACTION )
*-----------------------------------------------------------------------
  PERFORM    P2000_SET_STATUS_TCODE_DISABLE.

*-----------------------------------------------------------------------
* Screen, Work GUI TITLE SETTING( SCREEN )
*-----------------------------------------------------------------------
  PERFORM    P2000_SET_STATUS_SCR_DISABLE.

  CASE SY-TCODE.
    WHEN 'ZIM31' OR 'ZIM31L' OR 'ZIM32' OR 'ZIM32L' OR
         'ZIM33' OR 'ZIM33L'.
      IF ZTIV-ZFCUST NE '1'.
        MOVE 'IMPREQ' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      ENDIF.
    WHEN 'ZIMP2' OR 'ZIMP3' OR 'ZIMP4'.
      IF ZTPMTHD-ZFPYA EQ 'Y' AND ZTPMTHD-ZFPMYN EQ 'N'.
        MOVE 'TRTR' TO IT_EXCL-FCODE.  APPEND IT_EXCL. "반제전표.
        MOVE 'PYCN' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        MOVE 'DSPY' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        IF ZTPMTHD-ZFLCKN EQ '2' OR ZTPMTHD-ZFLCKN EQ '3'.
          MOVE 'PYMT' TO IT_EXCL-FCODE. APPEND IT_EXCL.
        ENDIF.
      ELSEIF ZTPMTHD-ZFPYA EQ 'N' AND ZTPMTHD-ZFPMYN EQ 'N'.
        MOVE 'TRCL' TO IT_EXCL-FCODE.  APPEND IT_EXCL. "반제전표.
        MOVE 'DSFI' TO IT_EXCL-FCODE.  APPEND IT_EXCL. "반제전표.
        MOVE 'PYMT' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        MOVE 'PYCN' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        MOVE 'DSPY' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      ELSEIF ZTPMTHD-ZFPYA EQ 'Y' AND ZTPMTHD-ZFPMYN EQ 'Y'.
        MOVE 'PYMT' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        MOVE 'TRCL' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        MOVE 'TRTR' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      ELSE.
        MOVE 'DSFI' TO IT_EXCL-FCODE.  APPEND IT_EXCL. "반제전표.
        MOVE 'PYMT' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        MOVE 'PYCN' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        MOVE 'DSPY' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        MOVE 'TRCL' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      ENDIF.
      IF ZTPMTHD-BELNR_TR IS INITIAL OR ZTPMTHD-GJAHR_TR IS INITIAL.
        MOVE 'DSTR' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        MOVE 'TRCC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      ELSE.
        MOVE 'TRTI' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      ENDIF.
      DESCRIBE TABLE IT_ZSPMTHST LINES W_LINE.
      IF W_LINE EQ 0.
        MOVE 'PSHT' TO IT_EXCL-FCODE. APPEND IT_EXCL.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTBL-BUKRS.

  IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
    " LG EDI 사용여부에 따라서...
    IF ZTIMIMGTX-APPLOG IS INITIAL.
      IF SY-DYNNR EQ '2601'.
        MOVE 'EDIS' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        MOVE 'CKEK' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        MOVE 'FLAT' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
        MOVE 'LGPT' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      ENDIF.
    ENDIF.
    " 보험 EDI 사용 여부에 따라서...
    IF ZTIMIMGTX-APPCIP IS INITIAL.
       IF SY-DYNNR EQ '4101'.
          MOVE 'LGIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'EDIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'CKEK'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'FLAT'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
       ENDIF.
    ENDIF.
  ENDIF.

*>> 운임검토서. JSY20021112추가.(한수원)
 IF SY-TCODE = 'ZIM23' AND SY-DYNNR = '0101'.
  LOOP AT IT_ZSBLCST.
     IF NOT IT_ZSBLCST-ZFACDO IS INITIAL.
        DELETE IT_EXCL WHERE FCODE EQ 'DTRS'
                          OR FCODE EQ 'DHYK'.
        EXIT.
     ENDIF.
  ENDLOOP.
 ENDIF.
*>> 비용문서.
  DESCRIBE TABLE IT_ZSIMCOST LINES W_LINE.
  IF W_LINE EQ 0.
     MOVE 'COST' TO IT_EXCL-FCODE. APPEND IT_EXCL.
  ENDIF.
*>> 비용문서(보험).
  IF ZTINSB-BELNR IS INITIAL AND SY-TCODE(4) EQ 'ZIMB'.
     MOVE 'COST' TO IT_EXCL-FCODE. APPEND IT_EXCL.
  ENDIF.

*>>> READY KOREA LTD.의 Package도입으로 반영
  MOVE 'FLAT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " FLAT DATA

*-----------------------------------------------------------------------
* PF-STATUS SETTING
*-----------------------------------------------------------------------
  SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.

ENDMODULE.                 " PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.

  MOVE OK-CODE TO W_OK_CODE.
  CLEAR : OK-CODE.
  if W_MAX_ITEMS is initial.
     W_MAX_ITEMS = '990'.
  endif.
ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0001 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

  IF OPTION = '1'.
    SET CURSOR FIELD 'SPOP-OPTION1'.
  ELSE.
    SET CURSOR FIELD 'SPOP-OPTION2'.
  ENDIF.

ENDMODULE.                 " SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0001 OUTPUT.

*  AUTHORITY-CHECK OBJECT 'ZM_BDC_MGT'
*                  ID 'ACTVT' FIELD '*'.
  W_SY_SUBRC = SY-SUBRC.

  LOOP AT SCREEN.
*>> Local건 경우 통화가 KRW만 업무가불 처리.
*    IF ( SY-DYNNR EQ '8211' AND ZTPMTHD-ZFLCKN NE '8' )
*       OR ( SY-DYNNR EQ '8211' AND ZTPMTHD-ZFLCKN EQ '8' AND
*                                   ZTPMTHD-ZFPNAMC NE 'KRW' ).
    IF SCREEN-NAME EQ 'ZTPMTHD-HKONT'.
      SCREEN-INPUT     = '0'.
      SCREEN-INVISIBLE = '1'.
    ENDIF.
    IF SCREEN-NAME = 'SPOP-OPTION_CAN'.
      IF CANCEL_OPTION = SPACE.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE'.                   "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ENDIF.

    IF W_SY_SUBRC NE 0 AND SY-DYNNR EQ '3515'.
      IF SCREEN-NAME(10) EQ 'RADIO_NONE'  OR
         SCREEN-NAME(09) EQ 'RADIO_ALL'   OR
         SCREEN-NAME(11) EQ 'RADIO_ERROR' OR
         SCREEN-NAME(06) EQ 'BLOCK2'.
        SCREEN-INVISIBLE = 1.
      ENDIF.
    ELSEIF ZTIMIMG00-ZFIVTY EQ 'L' AND SY-DYNNR EQ '3515'.
      IF SCREEN-NAME(10) EQ 'RADIO_NONE'  OR
         SCREEN-NAME(09) EQ 'RADIO_ALL'   OR
         SCREEN-NAME(11) EQ 'RADIO_ERROR' OR
         SCREEN-NAME(06) EQ 'BLOCK2'.
        SCREEN-INVISIBLE = 1.
      ENDIF.
    ELSEIF SY-DYNNR EQ '3516'.
      IF SCREEN-NAME(10) EQ 'RADIO_NONE'  OR
         SCREEN-NAME(09) EQ 'RADIO_ALL'   OR
         SCREEN-NAME(11) EQ 'RADIO_ERROR' OR
         SCREEN-NAME(06) EQ 'BLOCK2'.
        SCREEN-INVISIBLE = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0002  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0002 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR0101 OUTPUT.

  IF PROGRAM IS INITIAL.
    PROGRAM = SY-CPROG.        " Program SET
  ENDIF.

  IF DYNPRO IS INITIAL.
    IF SY-TCODE EQ 'ZIM21' OR SY-TCODE EQ 'ZIM22'
                           OR SY-TCODE EQ 'ZIM23'.
      DYNPRO = '0102'.
    ELSE.
      IF SY-TCODE EQ 'ZIM223'.
          IF ZTIMIMG00-BLCALYN EQ 'X'. " Freight Auto Calc.
             DYNPRO = '0130'.
          ELSE.
             DYNPRO = '0104'.
          ENDIF.
      ELSEIF SY-TCODE EQ 'ZIM35P'.
         DYNPRO = '0144'.
      ELSE.
        DYNPRO = '2601'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF SY-TCODE EQ 'ZIM21' OR SY-TCODE EQ 'ZIM22' OR SY-TCODE EQ 'ZIM23'
  OR SY-TCODE EQ 'ZIM221' OR SY-TCODE EQ 'ZIM222'
  OR SY-TCODE EQ 'ZIM223'
  OR SY-TCODE EQ 'ZIM26' OR SY-TCODE EQ 'ZIM27' OR SY-TCODE EQ 'ZIM28'
  OR SY-TCODE EQ 'ZIM29'
  OR SY-TCODE EQ 'ZIM81' OR SY-TCODE EQ 'ZIM82' OR SY-TCODE EQ 'ZIM83'
  OR SY-TCODE EQ 'ZIM35P' OR SY-TCODE EQ 'ZIM37' .

    IF TABSTRIP-ACTIVETAB EQ 'ITM1' OR TABSTRIP-ACTIVETAB EQ 'ITM2' OR
       TABSTRIP-ACTIVETAB EQ 'ITM3' OR TABSTRIP-ACTIVETAB EQ 'ITM4' OR
       TABSTRIP-ACTIVETAB EQ 'ITM5' OR TABSTRIP-ACTIVETAB EQ 'ITM6' OR
       TABSTRIP-ACTIVETAB EQ 'ITM7'.
      IF OK-CODE(3) NE 'ITM'.
        OK-CODE = TABSTRIP-ACTIVETAB.
      ENDIF.
    ELSE.
      IF SY-TCODE EQ 'ZIM223'.
        OK-CODE = 'ITM3'.
      ELSE.
        OK-CODE = 'ITM1'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF OK-CODE IS INITIAL.
    IF SY-TCODE EQ 'ZIM223'.
      OK-CODE = 'ITM3'.
    ELSE.
      OK-CODE = 'ITM1'.
    ENDIF.
  ENDIF.

  IF TABSTRIP-ACTIVETAB IS INITIAL.
    IF SY-TCODE EQ 'ZIM223'.
      TABSTRIP-ACTIVETAB = 'ITM3'.
    ELSE.
      TABSTRIP-ACTIVETAB = 'ITM1'.
    ENDIF.
  ENDIF.

  IF W_STATUS EQ 'D'.
    IF OK-CODE EQ 'ENTR'.
      CASE TABSTRIP-ACTIVETAB.
        WHEN 'ITM1'.
          OK-CODE = 'ITM2'.
        WHEN 'ITM2'.
          OK-CODE = 'ITM3'.
        WHEN 'ITM3'.
          IF SY-DYNNR EQ '2601'.
            OK-CODE = 'ITM3'.
          ELSE.
            OK-CODE = 'ITM4'.
          ENDIF.
        WHEN 'ITM4'.
          OK-CODE = 'ITM5'.
        WHEN 'ITM5'.
          OK-CODE = 'ITM6'.
        WHEN 'ITM6'.
          OK-CODE = 'ITM6'.
        WHEN 'ITM7'.
          OK-CODE = 'ITM7'.
      ENDCASE.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------------------
* TEXT SET
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_ACTION_TITLE.

*-----------------------------------------------------------------------
* Document Type SET
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_DOC_TITLE.

*-----------------------------------------------------------------------
* GUI TITLE SETTING
*-----------------------------------------------------------------------
  CASE OK-CODE.
    WHEN 'ITM1'.
      CASE SY-DYNNR.
        WHEN '0101'.
          DYNPRO = '0102'.
          SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC> 'General'.
        WHEN '2601'.
          DYNPRO = '2602'.
          SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC> 'General'.
        WHEN '0811'.
          DYNPRO = '0812'.
          SET TITLEBAR  'BLST' WITH <FS_F> <FS_DOC>
            'Cargo Work Material'.
         WHEN '0141'. "MKIM 020604
            DYNPRO = '0144'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'General'.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ITM2'.
      CASE SY-DYNNR.
        WHEN '0101'.
          DYNPRO = '0103'.
          SET TITLEBAR 'BLST' WITH <FS_F> <FS_DOC>
            'Request Custom Clearance/Good Receipt'.
        WHEN '2601'.
          DYNPRO = '2603'.
          SET TITLEBAR  'BLST' WITH <FS_F> <FS_DOC>
            'Transfortation Information'.
        WHEN '0811'.
          IF ZTIMIMG00-ZFPSMS EQ '1'.
            DYNPRO = '0813'.
          ELSE.
          ENDIF.
          SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC>
            'Cargo Work Cost'.
         WHEN '0141'. "MKIM 020604
            DYNPRO = '0145'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
              'Detailed Information & Etc.'.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ITM3'.
      CASE SY-DYNNR.
        WHEN '0101'.
          IF ZTIMIMG00-BLCALYN EQ 'X'. " Freight Auto Calc.
             DYNPRO = '0130'.
          ELSE.
             DYNPRO = '0104'.
          ENDIF.
          SET TITLEBAR 'BLST' WITH <FS_F> <FS_DOC> 'Weight & Charge'.
        WHEN '2601'.
          DYNPRO = '2604'.
          SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC>
            'Other Information'.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ITM4'.
      DYNPRO = '0105'.
      SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC> 'Container'.
    WHEN 'ITM5'.
      DYNPRO = '0106'.
      SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC>
        'Bonded Transfortation Expense'.
    WHEN 'ITM6'.
      IF ZTBL-ZFPOTY EQ 'H'.         ">Monetary, Non-monetary Yes/No
        DYNPRO = '0113'.
      ELSE.
        DYNPRO = '0112'.
      ENDIF.
      SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC> 'Material Information'.
    WHEN 'ITM7'.
      DYNPRO = '0120'.
      SET TITLEBAR  'BLST'
          WITH <FS_F> <FS_DOC> 'Commercial Invoice'.
  ENDCASE.

  TABSTRIP-ACTIVETAB = OK-CODE.
  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCRCOM OUTPUT.

  PERFORM P2000_SCR_MODE_SET.

ENDMODULE.                 " OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0102  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0102 OUTPUT.

  DESCRIBE TABLE IT_ZSIV      LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0102-LINES = G_PARAM_LINE.                     " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0102  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0102_SCR0102  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0102_SCR0102 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0102-CURRENT_LINE GT TC_0102-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIV      INDEX TC_0102-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIV      TO ZSIV.        " DATA MOVE
    MOVE: IT_ZSIV-ZFMARK            TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0102_SCR0102  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0104 OUTPUT.

  G_PARAM_LINE = TC_0104-TOP_LINE.
  TC_0104-LINES = G_PARAM_LINE + 18.                 " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0104_MARK_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0104_MARK_SCR0104 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0104-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_0104_MARK_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0104_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0104_SCR0104 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0104-CURRENT_LINE GT TC_0104-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSBLCON   INDEX TC_0104-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSBLCON   TO ZSBLCON.     " DATA MOVE
    MOVE: IT_ZSBLCON-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0104_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0105  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0105 OUTPUT.

*  G_PARAM_LINE = TC_0105-TOP_LINE.
*  TC_0105-LINES = G_PARAM_LINE + 15.                 " LINE 수 정?
  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSBLCST LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0105-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_0105-TOP_LINE.
    TC_0105-LINES = G_PARAM_LINE + 10.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0105  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0105_MARK_SCR0105  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0105_MARK_SCR0105 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0105-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_0105_MARK_SCR0105  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0105_SCR0105  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0105_SCR0105 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0105-CURRENT_LINE GT TC_0105-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSBLCST   INDEX TC_0105-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSBLCST   TO ZSBLCST.     " DATA MOVE
    ZSBLCST-ZFCDNM    =  IT_ZSBLCST-ZFCDNM.
    MOVE: IT_ZSBLCST-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0105_SCR0105  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0106  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0106 OUTPUT.

*  DESCRIBE TABLE IT_ZSBLCST1 LINES G_PARAM_LINE.    " LINE 수 GET
*  TC_0106-LINES = G_PARAM_LINE.                     " LINE 수 정?
* G_PARAM_LINE = TC_0106-TOP_LINE.
* TC_0106-LINES = G_PARAM_LINE + 15.                 " LINE 수 정?
  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSBLCST1 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0106-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_0106-TOP_LINE.
    TC_0106-LINES = G_PARAM_LINE + 16.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0106  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0106_MARK_SCR0106  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0106_MARK_SCR0106 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0106-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_0106_MARK_SCR0106  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0106_SCR0106  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0106_SCR0106 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0106-CURRENT_LINE GT TC_0106-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSBLCST1  INDEX TC_0106-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSBLCST1  TO ZSBLCST.     " DATA MOVE
    MOVE: IT_ZSBLCST1-ZFMARK        TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0106_SCR0106  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TC_7110_MARK_SCR7110  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_7110_MARK_SCR7110 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_7110-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_7110_MARK_SCR7110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR7110  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR7110 OUTPUT.

  G_PARAM_LINE = TC_7110-TOP_LINE.
  TC_7110-LINES = G_PARAM_LINE + 2.                 " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR7110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR2604  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR2604 OUTPUT.

* G_PARAM_LINE = TC_2604-TOP_LINE.
* TC_2604-LINES = G_PARAM_LINE + 6.                 " LINE 수 정?
  TC_2604-LINES = 10.
ENDMODULE.                 " TOTAL_LINE_GET_SCR2604  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_2604_MARK_SCR2604  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_2604_MARK_SCR2604 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_2604-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_2604_MARK_SCR2604  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC2604_SCR2604  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC2604_SCR2604 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_2604-CURRENT_LINE GT TC_2604-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSLGGOD   INDEX TC_2604-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSLGGOD   TO ZSLGGOD.     " DATA MOVE
    MOVE: IT_ZSLGGOD-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC2604_SCR2604  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_6218_MARK_SCR6218  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_6218_MARK_SCR6218 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_6218-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_6218_MARK_SCR6218  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0102_ENABLE_SET_SCR0102  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0102_ENABLE_SET_SCR0102 OUTPUT.
*  PERFORM   P2000_FIELD_MODE_SET   USING  IT_ZSIV-LOEKZ.
  PERFORM   P2000_SCR_MODE_SET.
ENDMODULE.                 " TC_0102_ENABLE_SET_SCR0102  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0102_MARK_SCR0102  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0102_MARK_SCR0102 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0102-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_0102_MARK_SCR0102  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0010 OUTPUT.
  DESCRIBE TABLE IT_ZSREQHD LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0010-LINES = G_PARAM_LINE.                   " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0010_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0010_SCR0010 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0010-CURRENT_LINE GT TC_0010-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSREQHD   INDEX TC_0010-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSREQHD   TO ZSREQHD.     " DATA MOVE
    MOVE: IT_ZSREQHD-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0010_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0011  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0011 OUTPUT.

  DESCRIBE TABLE IT_ZSREQHD LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0011-LINES = G_PARAM_LINE.                   " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0011  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0011_SCR0011  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0011_SCR0011 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0011-CURRENT_LINE GT TC_0011-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSREQHD   INDEX TC_0011-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSREQHD   TO ZSREQHD.     " DATA MOVE
    MOVE: IT_ZSREQHD-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0011_SCR0011  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0012  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0012 OUTPUT.

  DESCRIBE TABLE IT_ZSREQHD LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0012-LINES = G_PARAM_LINE.                   " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0012  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0012_SCR0012  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0012_SCR0012 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0012-CURRENT_LINE GT TC_0012-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSREQHD   INDEX TC_0012-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSREQHD   TO ZSREQHD.     " DATA MOVE
    MOVE: IT_ZSREQHD-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0012_SCR0012  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
MODULE D0014_STATUS_SCR0014 OUTPUT.

  IF W_STATUS_CHK = 'D'.
    SET PF-STATUS 'STDLISA'.
  ELSE.
    SET PF-STATUS 'STDLISW'.
  ENDIF.

  CASE INCLUDE.
    WHEN 'POPU'.
      SET TITLEBAR 'POPU' WITH 'Message status'.
    WHEN 'BLCST'.
      SET TITLEBAR 'POPU' WITH 'Object status of expense posting'.
    WHEN 'CIVPO' OR 'CIVRQ' OR 'CIVNO'.
      SET TITLEBAR 'POPU' WITH 'LOCAL goods price Doc No LIST'.
    WHEN 'LOGR' OR 'LOGR1'.
      SET TITLEBAR 'POPU' WITH 'G/R Doc No LIST'.
    WHEN 'MSNMFIND'.
      SET TITLEBAR 'POPU' WITH 'Mothership name LIST'.
    WHEN 'COMMIV'.
      SET TITLEBAR 'POPU' WITH 'Commercial Invoice LIST'.
    WHEN 'CGLIST'.
      SET TITLEBAR 'POPU' WITH
         'The list of Loading/Unloading duplication by port'.
    WHEN 'CGLIST1'.
      SET TITLEBAR 'POPU' WITH
                   'The list of Loading/Unloading duplication by B/L'.
    WHEN 'CC'.
      SET TITLEBAR 'POPU' WITH
                   'The list of Customs Clearance duplication by B/L'.
    WHEN 'INCREATE'.
      SET TITLEBAR 'POPU' WITH
                   'The list of insurance duplication by B/L'.
    WHEN OTHERS.
      SET TITLEBAR 'POPU' WITH 'Partial referance LIST'.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0103 OUTPUT.

  DESCRIBE TABLE IT_ZSIVIT    LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0103-LINES = G_PARAM_LINE.                     " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0103_MARK_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0103_MARK_SCR0103 OUTPUT.
  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0103-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK1 = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK1.
  ENDIF.

ENDMODULE.                 " TC_0103_MARK_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0103_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0103_SCR0103 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0103-CURRENT_LINE GT TC_0103-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIVIT    INDEX TC_0103-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIVIT    TO ZSIVIT.      " DATA MOVE
    MOVE : IT_ZSIVIT-ZFMARK         TO W_ROW_MARK1.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0103_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_CALL_VALUE_SCR3100  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_CALL_VALUE_SCR3100 OUTPUT.

  IF SY-CALLD EQ 'X'.
    GET PARAMETER ID 'ZPCIVNO' FIELD ZTCIVHD-ZFCIVNO.
    GET PARAMETER ID 'ZPHBLNO' FIELD ZTBL-ZFHBLNO.
    GET PARAMETER ID 'ZPBLNO'  FIELD ZTBL-ZFBLNO.
    GET PARAMETER ID 'BES'     FIELD ZTREQHD-EBELN.
    GET PARAMETER ID 'ZPOPNNO' FIELD ZTREQHD-ZFOPNNO.
*     GET PARAMETER ID 'ZPREQNO' FIELD ZTIV-ZFREQNO.
    GET PARAMETER ID 'MJA'     FIELD MSEG-MJAHR.
    GET PARAMETER ID 'MBN'     FIELD MSEG-MBLNR.

* 참조 B/L
    IF ZTBL-ZFHBLNO IS INITIAL AND ZTIV-ZFBLNO IS INITIAL.
      RADIO_HBL = 'X'.
    ELSE.
      IF NOT ZTIV-ZFBLNO IS INITIAL.
        RADIO_BLN = 'X'.
      ELSE.
        RADIO_HBL = 'X'.
      ENDIF.
    ENDIF.

* 수입의?
*     IF NOT ZTIV-ZFREQNO IS INITIAL.
*        RADIO_RQ = 'X'.
*     ELSE.
*        IF NOT ZTREQHD-ZFOPNNO IS INITIAL.
*           RADIO_LC = 'X'.
*        ELSE.
*           IF NOT ZTREQHD-EBELN IS INITIAL.
*              RADIO_PO = 'X'.
*           ELSE.
*              RADIO_NP = 'X'.
*           ENDIF.
*        ENDIF.
*     ENDIF.

* 무환(대체,보상품...)
    IF NOT ( MSEG-MJAHR IS INITIAL ) AND NOT ( MSEG-MBLNR IS INITIAL ).
      RADIO_RQ  = ' '.
      RADIO_LC  = ' '.
      RADIO_PO  = ' '.
      RADIO_NP  = ' '.
      RADIO_NPO = 'X'.
    ENDIF.

  ELSE.
*     CLEAR : ZTBL-ZFHBLNO, ZTIV-ZFBLNO,
*             ZTREQHD-EBELN, ZTREQHD-ZFOPNNO, ZTIV-ZFREQNO.
  ENDIF.

ENDMODULE.                 " SET_CALL_VALUE_SCR3100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCR9220  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCR9220 OUTPUT.

  LOOP AT SCREEN.
    CASE W_STATUS.
      WHEN C_REQ_C.                          " 변?
        IF SCREEN-GROUP1 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN  C_ADD_U.                       " 추가변?
        IF SCREEN-GROUP2 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN  C_REQ_U.                       " 변?
        IF SCREEN-GROUP2 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_REQ_D OR C_ADD_D OR C_OPEN_D.  " 조회, 추가조회, 확정조?
        IF SCREEN-GROUP1 = 'I'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
      WHEN C_OPEN_C OR C_OPEN_U.            " 확정, 확정변경.
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_INSU_I.                        " 보험관?
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          IF SCREEN-GROUP1 = 'I'.
            SCREEN-INPUT   = '1'.
          ELSE.
            SCREEN-INPUT   = '0'.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    IF SCREEN-NAME(10) EQ 'W_ROW_MARK'.
      SCREEN-INPUT   = '1'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.


ENDMODULE.                 " OPERATION_MODE_SET_SCR9220  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0110 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSBLIT  LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0110-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_0110-TOP_LINE.
    TC_0110-LINES = G_PARAM_LINE + 14.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0110_MARK_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0110_MARK_SCR0110 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0110-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.


ENDMODULE.                 " TC_0110_MARK_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0110_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0110_SCR0110 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0110-CURRENT_LINE GT TC_0110-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSBLIT   INDEX TC_0110-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSBLIT   TO ZSBLIT.       " DATA MOVE
    MOVE: IT_ZSBLIT-ZFMARK         TO W_ROW_MARK.   " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0110_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OPERATION_MODE_SCR0110 OUTPUT.

  LOOP AT SCREEN.
    CASE W_STATUS.
      WHEN C_REQ_C OR C_REQ_U.              " 생성, 변?
        IF SCREEN-GROUP1 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN  C_ADD_U OR C_BL_SEND.                       " 추가변?
        IF SCREEN-GROUP2 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
*       WHEN  C_REQ_U.                       " 변?
*         IF SCREEN-GROUP2 = 'IO'.   SCREEN-INPUT   = '1'.
*         ELSE.                      SCREEN-INPUT   = '0'.
*         ENDIF.
*         IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_REQ_D OR C_ADD_D OR C_OPEN_D.  " 조회, 추가조회, 확정조?
        IF SCREEN-GROUP1 = 'I'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
      WHEN C_OPEN_C OR C_OPEN_U OR C_BL_REAL.   " 확정, 확정변경.
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_INSU_I.                        " 보험관?
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          IF SCREEN-GROUP1 = 'I'.
            SCREEN-INPUT   = '1'.
          ELSE.
            SCREEN-INPUT   = '0'.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    IF SY-DYNNR EQ '0110' OR SY-DYNNR EQ '0112'.
      IF NOT IT_ZSBLIT-ZFREQNO IS INITIAL.
        IF SCREEN-NAME(14) EQ 'ZSBLIT-ZFREQNO' OR
           SCREEN-NAME(14) EQ 'ZSBLIT-ZFITMNO'.
          SCREEN-INPUT   = '0'.
        ENDIF.
      ENDIF.
    ENDIF.
    IF SY-DYNNR EQ '0112' OR SY-DYNNR EQ '0105'.
      IF SY-TCODE EQ'ZIM223'.
        SCREEN-INPUT  =  '0'.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '0111'.
*>>>> TEXT일 경우도 수량, 단가를 관리토록 함.
      IF NOT IT_ZSBLIT-MATNR IS INITIAL OR
         NOT IT_ZSBLIT-TXZ01 IS INITIAL.
        IF SCREEN-NAME(12) EQ 'ZSBLIT-MATNR' OR
           SCREEN-NAME(12) EQ 'ZSBLIT-MEINS' OR
           SCREEN-NAME(12) EQ 'ZSBLIT-MATKL' OR
           SCREEN-NAME(12) EQ 'ZSBLIT-WAERS' OR
           SCREEN-NAME(12) EQ 'ZSBLIT-BPRME'.
          SCREEN-INPUT   = '0'.
        ENDIF.
*       ELSEIF NOT IT_ZSBLIT-TXZ01 IS INITIAL.
*          IF SCREEN-NAME(12) EQ 'ZSBLIT-MATNR' OR
*             SCREEN-NAME(12) EQ 'ZSBLIT-MEINS' OR
*             SCREEN-NAME(14) EQ 'ZSBLIT-BLMENGE' OR
*             SCREEN-NAME(12) EQ 'ZSBLIT-NETPR' OR
*             SCREEN-NAME(12) EQ 'ZSBLIT-PEINH' OR
*             SCREEN-NAME(12) EQ 'ZSBLIT-BPRME' OR
*             SCREEN-NAME(12) EQ 'ZSBLIT-WAERS'.
*             SCREEN-INPUT   = '0'.
*          ENDIF.
      ENDIF.
      IF NOT IT_ZSBLIT-WERKS IS INITIAL.
        IF SCREEN-NAME(12) EQ 'ZSBLIT-WERKS'.
          SCREEN-INPUT   = '0'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '0110' OR SY-DYNNR EQ '0111' OR
       SY-DYNNR EQ '0112' OR SY-DYNNR EQ '0113'.
      IF IT_ZSBLIT-CGMENGE GT 0 OR IT_ZSBLIT-CCMENGE GT 0 OR
         IT_ZSBLIT-ZFPRQN  GT 0 OR IT_ZSBLIT-CMENGE  GT 0.
        SCREEN-INPUT = '0'.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '0112'.
      IF IT_ZSBLIT-ZFPOTY IS INITIAL.
        IF NOT IT_ZSBLIT-ZFREQNO IS INITIAL.
          CASE SCREEN-NAME.
            WHEN 'ZSBLIT-MATNR' OR 'ZSBLIT-TXZ01' OR
                 'ZSBLIT-MEINS' OR 'ZSBLIT-NETPR' OR
                 'ZSBLIT-PEINH' OR 'ZSBLIT-BPRME' OR
                 'ZSBLIT-STAWN' OR 'ZSBLIT-MATKL' OR
                 'ZSBLIT-WERKS' OR 'ZSBLIT-LGORT'.
              SCREEN-INPUT   = '0'.
            WHEN OTHERS.
          ENDCASE.
        ENDIF.

        IF NOT IT_ZSBLIT-ZFREQNO IS INITIAL.
          IF SCREEN-NAME(14) EQ 'ZSBLIT-ZFREQNO' OR
             SCREEN-NAME(14) EQ 'ZSBLIT-ZFITMNO'.
            SCREEN-INPUT   = '0'.
          ENDIF.
        ENDIF.
      ELSE.
        CASE SCREEN-NAME.
          WHEN 'ZSBLIT-ZFREQNO' OR 'ZSBLIT-ZFITMNO' OR
               'ZSBLIT-ZFSHNO'.
            SCREEN-INPUT   = '0'.
        ENDCASE.
      ENDIF.
*>> 주기기 OR 무환일 경우 PO, PO-ITEM 번호 INPUT<2003.02.05>
      IF W_STATUS NE C_REQ_D.
         IF ZTBL-ZFPOYN EQ 'N' OR ZTBL-ZFMATGB EQ '1'.
            IF SCREEN-NAME(12) EQ 'ZSBLIT-EBELN' OR
               SCREEN-NAME(12) EQ 'ZSBLIT-EBELP'.
               SCREEN-INPUT = '1'.
            ENDIF.
         ENDIF.
       ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '0113'.
      IF NOT IT_ZSBLIT-VBELN IS INITIAL.
        IF SCREEN-NAME EQ 'ZSBLIT-VBELN' OR
           SCREEN-NAME EQ 'ZSBLIT-POSNR'.
          SCREEN-INPUT   = '0'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF IT_ZSBLIT-BLOEKZ EQ 'X'.
      SCREEN-INPUT   = '0'.
    ENDIF.

    IF SCREEN-NAME(10) EQ 'W_ROW_MARK'.
      SCREEN-INPUT   = '1'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.


ENDMODULE.                 " OPERATION_MODE_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR3511  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR3511 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSCIVIT  LINES G_PARAM_LINE.   " LINE 수 GET
    TC_3511-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_3511-TOP_LINE.
    TC_3511-LINES = G_PARAM_LINE + 5.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR3511  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC3510_SCR3510  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC3511_SCR3511 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_3511-CURRENT_LINE GT TC_3511-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSCIVIT   INDEX TC_3511-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSCIVIT   TO ZSCIVIT.       " DATA MOVE
    MOVE: IT_ZSCIVIT-ZFMARK         TO W_ROW_MARK.   " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC3511_SCR3511  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_3511_MARK_SCR3511  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_3511_MARK_SCR3511 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_3511-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_3510_MARK_SCR3511  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR3510  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR3510 OUTPUT.

  IF PROGRAM IS INITIAL.
    PROGRAM = SY-CPROG.        " 프로그램 SET
  ENDIF.
*-----------------------------------------------------------------------
* TEXT SET
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_ACTION_TITLE.

*-----------------------------------------------------------------------
* 문서 종류 SET
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_DOC_TITLE.

*
*>> 유환이고, 선급금이면..
*  IF ZTCIVHD-ZFPOYN EQ 'Y' AND ZTCIVHD-ZFPRPYN EQ 'Y'.
  IF ZTCIVHD-ZFPRPYN EQ 'Y'.
     IF ZTCIVHD-ZFSVYN IS INITIAL.
        DYNPRO = '3513'.
        SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC> 'Simple Invoice'.
     ELSE.
        DYNPRO = '3520'.
        SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC> 'Simple Invoice'.
     ENDIF.
  ENDIF.

  IF ZTCIVHD-ZFPRPYN EQ 'N'.
    CASE ZTCIVHD-ZFREQTY.
      WHEN 'LO'.
        DYNPRO = '3513'.
        SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC> 'Detailed Screen'.
      WHEN 'PU'.
        DYNPRO = '3513'.
        SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC>
                                          'Detailed Screen'.
      WHEN OTHERS.
        DYNPRO = '3511'.
        SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC> 'Detailed Screen'.
    ENDCASE.
  ENDIF.
*>> 무환이면, 선급금이 아니면.
*  IF ZTCIVHD-ZFPOYN EQ 'N'.
*     DYNPRO = '3511'.
*     SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC>
*                   'Non-monetary transaction Invoice'.
*  ENDIF.

  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

  DESCRIBE TABLE IT_ZSCIVHST LINES LINE1.
  IF LINE1 EQ 0.
    DYNSUB1 = '9217'.
  ELSE.
    DYNSUB1 = '3519'.
  ENDIF.

ENDMODULE.                 " SET_SUB_SCREEN_SCR3510  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SCR3511  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OPERATION_MODE_SCR3511 OUTPUT.
  LOOP AT SCREEN.
    CASE W_STATUS.
      WHEN C_REQ_C OR C_REQ_U.              " 생성, 변?
*       WHEN C_REQ_C.                          " 변?
        IF SCREEN-GROUP1 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN  C_ADD_U.                       " 추가변?
        IF SCREEN-GROUP2 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
*       WHEN  C_REQ_U.                       " 변?
*         IF SCREEN-GROUP2 = 'IO'.   SCREEN-INPUT   = '1'.
*         ELSE.                      SCREEN-INPUT   = '0'.
*         ENDIF.
*         IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_REQ_D OR C_ADD_D OR C_OPEN_D.  " 조회, 추가조회, 확정조?
        IF SCREEN-GROUP1 = 'I'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
*      WHEN C_OPEN_C OR C_OPEN_U.            " 확정, 확정변경.
      WHEN C_OPEN_C.                        " 확정, 확정변경.
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_OPEN_U.                        " 확정변경.
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.

      WHEN C_INSU_I.                        " 보험관?
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          IF SCREEN-GROUP1 = 'I'.
            SCREEN-INPUT   = '1'.
          ELSE.
            SCREEN-INPUT   = '0'.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    IF SCREEN-NAME(10) EQ 'W_ROW_MARK'.
      SCREEN-INPUT   = '1'.
    ENDIF.
    IF ZTCIVHD-ZFPRPYN EQ 'Y'.
      IF SCREEN-NAME EQ 'ZSCIVIT-CMENGE'.
        ZSCIVIT-CMENGE = 0.
        SCREEN-INPUT   = '0'.
      ENDIF.
    ENDIF.

    IF NOT IT_ZSCIVIT-ZFREQNO IS INITIAL.
      IF SCREEN-NAME(14) EQ 'ZSCIVIT-ZFBLNO'  OR
         SCREEN-NAME(14) EQ 'ZSCIVIT-ZFBLIT'  OR
         SCREEN-NAME(15) EQ 'ZSCIVIT-ZFREQNO' OR
         SCREEN-NAME(15) EQ 'ZSCIVIT-ZFITMNO'.
        SCREEN-INPUT   = '0'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " OPERATION_MODE_SCR3511  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR3513  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR3513 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSCIVIT  LINES G_PARAM_LINE.   " LINE 수 GET
    TC_3513-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
      DESCRIBE TABLE IT_ZSCIVIT  LINES G_PARAM_LINE.   " LINE 수 GET
      TC_3513-LINES = G_PARAM_LINE.                   " LINE 수 정의.
    ELSE.
      G_PARAM_LINE = TC_3513-TOP_LINE.
      TC_3513-LINES = G_PARAM_LINE + 5.              " LINE 수 정의.
    ENDIF.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR3513  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC3513_SCR3513  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC3513_SCR3513 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_3513-CURRENT_LINE GT TC_3513-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSCIVIT   INDEX TC_3513-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSCIVIT   TO ZSCIVIT.       " DATA MOVE
    MOVE: IT_ZSCIVIT-ZFMARK         TO W_ROW_MARK.   " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC3513_SCR3513  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0120 OUTPUT.

  DESCRIBE TABLE IT_ZSCIVIT  LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0120-LINES = G_PARAM_LINE.                   " LINE 수 정의.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0120  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0120_SCR0120  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0120_SCR0120 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0120-CURRENT_LINE GT TC_0120-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSCIVIT   INDEX TC_0120-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSCIVIT   TO ZSCIVIT.       " DATA MOVE
    MOVE: IT_ZSCIVIT-ZFMARK         TO W_ROW_MARK.    " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0120_SCR0120  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0812  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0812 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSCGIT  LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0812-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_0812-TOP_LINE.
    TC_0812-LINES = G_PARAM_LINE + 10.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0812  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SCR0812  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OPERATION_MODE_SCR0812 OUTPUT.
  LOOP AT SCREEN.
    CASE W_STATUS.
      WHEN C_REQ_C OR C_REQ_U.              " 생성, 변?
*       WHEN C_REQ_C.                          " 변?
        IF SCREEN-GROUP1 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN  C_ADD_U.                       " 추가변?
        IF SCREEN-GROUP2 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
*       WHEN  C_REQ_U.                       " 변?
*         IF SCREEN-GROUP2 = 'IO'.   SCREEN-INPUT   = '1'.
*         ELSE.                      SCREEN-INPUT   = '0'.
*         ENDIF.
*         IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_REQ_D OR C_ADD_D OR C_OPEN_D.  " 조회, 추가조회, 확정조?
        IF SCREEN-GROUP1 = 'I'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
      WHEN C_OPEN_C OR C_OPEN_U.            " 확정, 확정변경.
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_INSU_I.                        " 보험관?
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          IF SCREEN-GROUP1 = 'I'.
            SCREEN-INPUT   = '1'.
          ELSE.
            SCREEN-INPUT   = '0'.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
*>> P/O 삭제 지시자.
    IF IT_ZSCGIT-LOEKZ EQ 'X'.
      SCREEN-INPUT   = '0'.
    ENDIF.
*>> B/L 삭제 지시자.
    IF IT_ZSCGIT-BLOEKZ EQ 'X'.
      SCREEN-INPUT   = '0'.
    ENDIF.
*>> 하역 삭제지시자.
    IF IT_ZSCGIT-CGLOEKZ EQ 'X'.
      SCREEN-INPUT   = '0'.
    ENDIF.
*>> 통관수량이 있을 경우, 입력을 막음.
    IF IT_ZSCGIT-CCMENGE GT 0.
      IF SCREEN-NAME(14)   EQ   'ZSCGIT-CCMENGE'.
*          SCREEN-NAME(13)   EQ   'ZSCGIT-ZFBLIT' OR
*          SCREEN-NAME(14)   EQ   'ZSCGIT-CCMENGE'.
        SCREEN-INPUT   = '0'.
      ENDIF.
    ENDIF.

*    IF NOT IT_ZSCGIT-EBELN IS INITIAL.
    IF NOT IT_ZSCGIT-ZFBLNO IS INITIAL.
      IF SCREEN-NAME(13)   EQ   'ZSCGIT-ZFBLNO' OR
         SCREEN-NAME(13)   EQ   'ZSCGIT-ZFBLIT'.
        SCREEN-INPUT   = '0'.
      ENDIF.
    ELSE.
      IF IT_ZSCGIT-ZFPOTY IS INITIAL.
        IF SCREEN-NAME(14)   EQ  'ZSCGIT-CGMENGE' OR
           SCREEN-NAME(15)   EQ  'ZSCGIT-ZFBNARCD'.
          SCREEN-INPUT   = '0'.
        ENDIF.
      ENDIF.
    ENDIF.


    IF SCREEN-NAME(10) EQ 'W_ROW_MARK'.
      SCREEN-INPUT   = '1'.
    ENDIF.
*>> B/L 자재화면일 경우.
    IF NOT IT_ZSBLIT-ZFREQNO IS INITIAL AND SY-DYNNR EQ '0101'.
      IF SCREEN-NAME(14) EQ 'ZSBLIT-ZFREQNO' OR
         SCREEN-NAME(14) EQ 'ZSBLIT-ZFITMNO'.
        SCREEN-INPUT   = '0'.
      ENDIF.
    ENDIF.
*>> 하역 자재화면일 경우.
    IF NOT IT_ZSCGIT-ZFREQNO IS INITIAL AND SY-DYNNR EQ '0811'.
      IF SCREEN-NAME(14) EQ 'ZSCGIT-ZFBLNO'  OR
         SCREEN-NAME(14) EQ 'ZSCGIT-ZFBLIT'  OR
         SCREEN-NAME(15) EQ 'ZSCGIT-CGMENGE' OR
         SCREEN-NAME(16) EQ 'ZSCGIT-ZFBNARCD'.
        SCREEN-INPUT   = '0'.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " OPERATION_MODE_SCR0812  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0812_MARK_SCR0812  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0812_MARK_SCR0812 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0812-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_0812_MARK_SCR0812  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0812_SCR0812  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0812_SCR0812 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0812-CURRENT_LINE GT TC_0812-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSCGIT   INDEX TC_0812-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSCGIT   TO ZSCGIT.       " DATA MOVE
    MOVE: IT_ZSCGIT-ZFMARK         TO W_ROW_MARK.   " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0812_SCR0812  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0813_MARK_SCR0813  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0813_MARK_SCR0813 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0813-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_0813_MARK_SCR0813  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0813  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0813 OUTPUT.

*  DESCRIBE TABLE IT_ZSCGCST LINES G_PARAM_LINE.    " LINE 수 GET
*  TC_0813-LINES = G_PARAM_LINE.                     " LINE 수 정?

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSCGCST LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0813-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_0813-TOP_LINE.
    TC_0813-LINES = G_PARAM_LINE + 15.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0813  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0813_SCR0813  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0813_SCR0813 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0813-CURRENT_LINE GT TC_0813-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSCGCST  INDEX TC_0813-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSCGCST   TO ZSCGCST.     " DATA MOVE
    MOVE: IT_ZSCGCST-ZFMARK        TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0813_SCR0813  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR3110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR3110 OUTPUT.

  IF PROGRAM IS INITIAL.
    PROGRAM = SY-CPROG.        " 프로그램 SET
  ENDIF.
*
  IF DYNPRO IS INITIAL.
    DYNPRO = '3112'.
  ENDIF.

  IF ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU'.
    DYNSUB1 = '3116'.
  ELSE.
    DYNSUB1 = '3111'.
  ENDIF.

  IF TABSTRIP-ACTIVETAB EQ 'ITM1' OR TABSTRIP-ACTIVETAB EQ 'ITM2' OR
     TABSTRIP-ACTIVETAB EQ 'ITM3'.
    IF OK-CODE(3) NE 'ITM'.
      OK-CODE = TABSTRIP-ACTIVETAB.
    ENDIF.
  ELSE.
    OK-CODE = 'ITM1'.
  ENDIF.

  IF OK-CODE IS INITIAL.
    OK-CODE = 'ITM1'.
  ENDIF.

  IF TABSTRIP-ACTIVETAB IS INITIAL.
    TABSTRIP-ACTIVETAB = 'ITM1'.
  ENDIF.

*-----------------------------------------------------------------------
* TEXT SET
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_ACTION_TITLE.

*-----------------------------------------------------------------------
* 문서 종류 SET
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_DOC_TITLE.

*-----------------------------------------------------------------------
* GUI TITLE SETTING
*-----------------------------------------------------------------------
  CASE OK-CODE.
    WHEN 'ITM1'.
      DYNPRO = '3112'.
      SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC> 'General'.
    WHEN 'ITM2'.
      DYNPRO = '3113'.
      SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC> 'Document status'.
    WHEN 'ITM3'.
      IF ZTIMIMG00-ZFCSTMD EQ 'I'.
        DYNPRO = '3114'.
      ELSE.
        DYNPRO = '3115'.
      ENDIF.
      SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC> 'Document record'.
    WHEN 'ITM4'.
      DYNPRO = '3113'.
      SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC>
                            'Record of cost distribution'.
  ENDCASE.

  TABSTRIP-ACTIVETAB = OK-CODE.
  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR3110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_INIT_DATA_SCR3100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_INIT_DATA_SCR3100 OUTPUT.

  IF W_3100_FIRST = 'N' AND SY-CALLD NE 'X'.
    CLEAR : ZSIV.
    MOVE : 'Y'          TO      ZSIV-ZFPOYN,
           'X'          TO      ZSIV-ZFCSTYN,
*            'Y'          TO      W_3100_FIRST,
           'C'          TO      ZSIV-ZFCLCD.
  ENDIF.

  IF W_3100_FIRST = 'N'.
    SELECT SINGLE * FROM ZTIMIMG00.
    IF SY-SUBRC NE 0.
      MESSAGE S963.
      LEAVE TO SCREEN 0.
    ENDIF.
    IF ZTIMIMG00-ZFCSTMD IS INITIAL.
      MESSAGE S986.
      LEAVE TO SCREEN 0.
    ELSE.
      IF ZTIMIMG00-ZFCSTMD EQ 'S'.
        CLEAR : ZSIV-ZFCSTYN.
      ENDIF.
    ENDIF.
    W_3100_FIRST = 'Y'.
  ENDIF.

ENDMODULE.                 " SET_INIT_DATA_SCR3100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR3519  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR3519 OUTPUT.

  DESCRIBE TABLE IT_ZSCIVHST LINES G_PARAM_LINE.   " LINE 수 GET
  TC_3519-LINES = G_PARAM_LINE.                    " LINE 수 정의.

ENDMODULE.                 " TOTAL_LINE_GET_SCR3519  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC3519_SCR3519  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC3519_SCR3519 OUTPUT.
  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_3519-CURRENT_LINE GT TC_3519-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSCIVHST   INDEX TC_3519-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSCIVHST  TO ZSCIVHST.       " DATA MOVE
    MOVE: IT_ZSCIVHST-ZFMARK        TO W_ROW_MARK.   " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC3519_SCR3519  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_INIT_VALUE_SCR3500  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_INIT_VALUE_SCR3500 OUTPUT.

  IF W_3500_FIRST EQ 'N'.
    CLEAR : ZSCIVHD-ZFCIVNO, ZSCIVHD-ZFTRIPLE.
    ZSCIVHD-ZFPOYN  = 'Y'.
    ZSCIVHD-ZFPRPYN = 'N'.
    W_3500_FIRST = 'Y'.
  ENDIF.

ENDMODULE.                 " SET_INIT_VALUE_SCR3500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC3114_SCR3114  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC3114_SCR3114 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_3114-CURRENT_LINE GT TC_3114-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.

  READ TABLE IT_ZSIVHST   INDEX TC_3114-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIVHST  TO ZSIVHST.          " DATA MOVE
    MOVE IT_ZSIVHST-ZFMARK         TO W_ROW_MARK.      " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC3114_SCR3114  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR3114  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR3114 OUTPUT.

  DESCRIBE TABLE IT_ZSIVHST  LINES LINE.
  TC_3114-LINES = LINE.

  DESCRIBE TABLE IT_ZSIVHST1 LINES LINE.
  TC_3114_1-LINES = LINE.

ENDMODULE.                 " TOTAL_LINE_GET_SCR3114  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC3117_SCR3117  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC3117_SCR3117 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_3117-CURRENT_LINE GT TC_3117-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.

  READ TABLE IT_ZSIVIT_TMP INDEX TC_3117-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIVIT_TMP  TO ZSIVIT.          " DATA MOVE
    MOVE IT_ZSIVIT_TMP-ZFMARK         TO W_ROW_MARK.      " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC3117_SCR3117  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR3117  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR3117 OUTPUT.

  DESCRIBE TABLE IT_ZSIVIT_TMP LINES LINE.
  TC_3117-LINES = LINE.

ENDMODULE.                 " TOTAL_LINE_GET_SCR3117  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR3117_1  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR3117_1 OUTPUT.

  DESCRIBE TABLE IT_ZSIVCD_TMP LINES LINE.
  TC_3117_1-LINES = LINE.

ENDMODULE.                 " TOTAL_LINE_GET_SCR3117_1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC3117_1_SCR3117  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC3117_1_SCR3117 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_3117_1-CURRENT_LINE GT TC_3117_1-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.

  READ TABLE IT_ZSIVCD_TMP   INDEX TC_3117_1-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIVCD_TMP  TO ZSIVCD.          " DATA MOVE
    MOVE IT_ZSIVCD_TMP-ZFMARK         TO W_ROW_MARK1.     " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC3117_1_SCR3117  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0015_STATUS_SCR0015  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0015_STATUS_SCR0015 OUTPUT.

  IF INCLUDE NE 'CONTDISP'.
    SET PF-STATUS 'ERRLIST'.
  ELSE.
    SET PF-STATUS 'CONTLIST'.
  ENDIF.

  CASE INCLUDE.
    WHEN 'POPU'.
      SET TITLEBAR 'POPU' WITH 'Error LIST'.
    WHEN 'CONTDISP'.
      SET TITLEBAR 'POPU' WITH 'Container List'.
    WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0015_STATUS_SCR0015  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0111  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0111 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSBLIT  LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0111-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_0111-TOP_LINE.
    TC_0111-LINES = G_PARAM_LINE + 14.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0111  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0111_MARK_SCR0111  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0111_MARK_SCR0111 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0111-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_0111_MARK_SCR0111  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0111_SCR0111  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0111_SCR0111 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0111-CURRENT_LINE GT TC_0111-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSBLIT   INDEX TC_0111-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSBLIT   TO ZSBLIT.       " DATA MOVE
    MOVE: IT_ZSBLIT-ZFMARK         TO W_ROW_MARK.   " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0111_SCR0111  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR3116  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR3116 OUTPUT.
*  G_PARAM_LINE = TC_3111-TOP_LINE.
*  TC_3111-LINES = G_PARAM_LINE + 4.                 " LINE 수 정?
*  IF ZTIV-ZFPOYN EQ 'Y' OR ZTIV-ZFPONMA EQ 'Y'.
  DESCRIBE TABLE IT_ZSIVIT LINES LINE.
  TC_3116-LINES = LINE.
*  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR3116  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC3116_SCR3116  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC3116_SCR3116 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSIVIT   INDEX TC_3116-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
    MOVE ZTIV-ZFIVAMC             TO IT_ZSIVIT-ZFIVAMC.
    MOVE-CORRESPONDING IT_ZSIVIT  TO ZSIVIT.          " DATA MOVE
    MOVE IT_ZSIVIT-ZFMARK         TO W_ROW_MARK.      " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC3116_SCR3116  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR9912  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR9912 INPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZSMSCST-ZTERM'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF SCREEN-INPUT EQ '1'.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZSMSCST-ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ELSE.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZSMSCST-ZTERM
              I_XSHOW       = 'X'
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ENDIF.

  IF SY-SUBRC NE 0.
*   message e177 with ekko-zterm.
    MESSAGE S177(06) WITH ZSMSCST-ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    ZSMSCST-ZTERM = T052-ZTERM.
  ENDIF.

ENDMODULE.                 " HELP_ZTERM_SCR9912  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0020 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0114_STATUS_SCR0114  OUTPUT
*&---------------------------------------------------------------------*
MODULE D0114_STATUS_SCR0114 OUTPUT.

  IF W_STATUS_CHK EQ 'U' .
    MOVE 'UMAT' TO IT_EXCL-FCODE.
    APPEND IT_EXCL.
  ENDIF.
  IF INCLUDE = 'POLST' OR INCLUDE =  'BLLST'.
    SET PF-STATUS 'BLLST' EXCLUDING IT_EXCL.
  ENDIF.
  CASE INCLUDE.
    WHEN 'POLST'.
      SET TITLEBAR 'POPU' WITH 'B/L duplication List by P/O No'.
    WHEN 'BLLST'.
      SET TITLEBAR 'POPU' WITH 'Duplication List by B/L No'.
    WHEN OTHERS.
  ENDCASE.
  SUPPRESS DIALOG.

ENDMODULE.          " D0114_STATUS_SCR0114  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR9215  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR9215 OUTPUT.

  IF PROGRAM IS INITIAL.
    PROGRAM = SY-CPROG.        " 프로그램 SET
  ENDIF.

  DESCRIBE TABLE IT_ZSBLIT LINES W_LINE.

  IF W_LINE GT 0.
    DYNSUB = '9216'.
  ELSE.
    DYNSUB = '9217'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC9116_SCR9126  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC9116_SCR9126 OUTPUT.

*  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
*  IF TC_9216-CURRENT_LINE GT TC_9216-LINES.
*     EXIT FROM STEP-LOOP.
*  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSBLIT   INDEX TC_9216-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSBLIT   TO ZSBLIT.       " DATA MOVE
    MOVE: IT_ZSBLIT-ZFMARK         TO W_ROW_MARK.   " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC9116_SCR9126  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0031  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0031 OUTPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0031-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0031  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0143  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0143 OUTPUT.
  G_PARAM_LINE = TC_0143-TOP_LINE.
  TC_0143-LINES = G_PARAM_LINE + 5.              " LINE 수 정?
ENDMODULE.                 " TOTAL_LINE_GET_SCR0143  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0143_ENABLE_SET_SCR0143  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0143_ENABLE_SET_SCR0143 OUTPUT.
   PERFORM P2000_SCR_MODE_SET.
ENDMODULE.                 " TC_0143_ENABLE_SET_SCR0143  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0143_MARK_SCR0143  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0143_MARK_SCR0143 OUTPUT.
  IF OK-CODE = 'MKA2'                  " mark all
     AND TC_0143-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK1 = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL2'.                " delete all marks
     CLEAR W_ROW_MARK1.
  ENDIF.

ENDMODULE.                 " TC_0143_MARK_SCR0143  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0143_SCR0143  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0143_SCR0143 OUTPUT.
  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0143-CURRENT_LINE GT TC_0143-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSTTSG5    INDEX TC_0143-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING IT_ZSTTSG5   TO ZSTTSG5.     " DATA MOVE
     MOVE: IT_ZSTTSG5-ZFMARK         TO W_ROW_MARK1.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0143_SCR0143  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0103_ENABLE_SET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0103_ENABLE_SET_SCR0103 OUTPUT.

ENDMODULE.                 " TC_0103_ENABLE_SET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_CALL_VALUE_SCR0144  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_CALL_VALUE_SCR0144 OUTPUT.
 GET PARAMETER ID 'ZPCIVRN' FIELD ZTTTHD-ZFCIVRN.
ENDMODULE.                 " SET_CALL_VALUE_SCR0144  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_PAYORD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_PAYORD OUTPUT.
  IF W_FIRST_SCR0144 = 'Y'.
    SELECT SINGLE * FROM ZTTTHD  WHERE ZFCIVRN = ZTTTHD-ZFCIVRN.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSTTSG5
                FROM ZTTTSG5 WHERE ZFCIVRN = ZTTTHD-ZFCIVRN
                ORDER BY ZFLSG5.
*    LOOP AT IT_ZSTTSG5.
*       MOVE-CORRESPONDING   IT_ZSTTSG5 TO ZSTTSG5.
*         APPEND ZSTTSG5.
*    ENDLOOP.
    W_FIRST_SCR0144 = 'N'.
  ENDIF.

ENDMODULE.                 " DISPLAY_PAYORD  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR4104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR4104 OUTPUT.

  TC_4104-LINES = 3.                    " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR4104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_4104_MARK_SCR4104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_4104_MARK_SCR4104 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_4104-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_4104_MARK_SCR4104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC4104_SCR4104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC4104_SCR4104 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_4104-CURRENT_LINE GT TC_4104-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSINSBSG2  INDEX TC_4104-CURRENT_LINE.
  IF SY-SUBRC = 0.                                    " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSINSBSG2  TO ZSINSBSG2.    " DATA MOVE
    MOVE: IT_ZSINSBSG2-ZFMARK        TO W_ROW_MARK.   " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC4104_SCR4104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR4105  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR4105 OUTPUT.

  G_PARAM_LINE = TC_4105-TOP_LINE.
  IF SY-DYNNR EQ '4505'.
    TC_4105-LINES = G_PARAM_LINE + 12.                " LINE 수 정?
  ELSE.
    TC_4105-LINES = G_PARAM_LINE + 5.                 " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR4105  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_4105_MARK_SCR4105  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_4105_MARK_SCR4105 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_4105-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_4105_MARK_SCR4105  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC4105_SCR4105  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC4105_SCR4105 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_4105-CURRENT_LINE GT TC_4105-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSINSBAGR  INDEX TC_4105-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSINSBAGR  TO ZSINSBAGR.    " DATA MOVE
    MOVE: IT_ZSINSBAGR-ZFMARK        TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC4105_SCR4105  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR4101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR4101 OUTPUT.

  IF PROGRAM IS INITIAL.
     PROGRAM = SY-CPROG.
  ENDIF.

  IF DYNPRO IS INITIAL.
     DYNPRO = '4102'.
  ENDIF.

  IF SY-TCODE EQ 'ZIMB1' OR SY-TCODE EQ 'ZIMB2' OR
     SY-TCODE EQ 'ZIMB3' OR SY-TCODE EQ 'ZIMB4'.

    IF TABSTRIP-ACTIVETAB EQ 'ITM1' OR TABSTRIP-ACTIVETAB EQ 'ITM2' OR
       TABSTRIP-ACTIVETAB EQ 'ITM3' OR TABSTRIP-ACTIVETAB EQ 'ITM4' .
       IF OK-CODE(3) NE 'ITM'.
          OK-CODE = TABSTRIP-ACTIVETAB.
       ENDIF.
    ELSE.
       OK-CODE = 'ITM1'.
    ENDIF.
  ENDIF.

  IF OK-CODE IS INITIAL.
     OK-CODE = 'ITM1'.
  ENDIF.

  IF TABSTRIP-ACTIVETAB IS INITIAL.
     TABSTRIP-ACTIVETAB = 'ITM1'.
  ENDIF.

  IF W_STATUS EQ 'D' OR W_STATUS EQ 'R' OR W_STATUS EQ 'B'.
    IF OK-CODE EQ 'ENTR'.
      CASE TABSTRIP-ACTIVETAB.
        WHEN 'ITM1'.
          OK-CODE = 'ITM2'.
        WHEN 'ITM2'.
          OK-CODE = 'ITM3'.
        WHEN 'ITM3'.
          OK-CODE = 'ITM4'.
        WHEN 'ITM4'.
          OK-CODE = 'ITM4'.
      ENDCASE.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------------------
* TEXT SET
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_ACTION_TITLE.

*-----------------------------------------------------------------------
* 문서 종류 SET
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_DOC_TITLE.

*-----------------------------------------------------------------------
* GUI TITLE SETTING
*-----------------------------------------------------------------------
  CASE OK-CODE.
    WHEN 'ITM1'.
      DYNPRO = '4102'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'General Info.'.
    WHEN 'ITM2'.
      DYNPRO = '4103'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Insuarance Policy'.
    WHEN 'ITM3'.
      DYNPRO = '4104'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Items'.
    WHEN 'ITM4'.
      DYNPRO = '4105'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                        'Additional Conditions'.
  ENDCASE.

  TABSTRIP-ACTIVETAB = OK-CODE.
  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR4101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LOOPLINES_SCR0112  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LOOPLINES_SCR0112 OUTPUT.
   LOOPLINES = SY-LOOPC.
ENDMODULE.                 " GET_LOOPLINES_SCR0112  OUTPUT
