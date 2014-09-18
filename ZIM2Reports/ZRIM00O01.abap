*&---------------------------------------------------------------------*
*& INCLUDE ZRIM00O01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰 Main PBO MODULE Include                      *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.01.25                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  PF_STATUS_SCRCOM  OUTPUT
*&   DESC : GUI STATUS 및 GUI TITLE BAR를 SET하는 공통 MODULE
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
* PF-STATUS상의 Inactive Function Setting ( TRANSACTION 별 )
*-----------------------------------------------------------------------
  PERFORM    P2000_SET_STATUS_TCODE_DISABLE.

*-----------------------------------------------------------------------
* 화면별, 업무별 GUI TITLE SETTING( SCREEN 별 )
*-----------------------------------------------------------------------
  PERFORM    P2000_SET_STATUS_SCR_DISABLE.
  IF SY-DYNNR EQ '4101' OR SY-DYNNR EQ '4501'.
    IF ZTINS-ZFDOCST NE 'N'.
      MOVE 'LGIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL. "부보의뢰.
    ENDIF.
  ENDIF.
*>> COPY 기능 막음.
  IF SY-DYNNR NE '0100'.
    MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 화폐변경.
  ENDIF.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTREQHD-BUKRS.
  IF ZTIMIMGTX-ZFEDIYN NE 'X'.
    CASE SY-DYNNR.
      WHEN '0101' OR '0111' OR '0121' OR '0131' OR '0141' OR '0151'
        OR '1101' OR '1111' OR '2101' OR '4101' OR '4501'.
        MOVE 'EDIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'FLAT'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'CKEK'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'APP700' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'INF700' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'INF707' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'PUDOC'  TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'TTREQ'  TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'APP707' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'LGIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
    ENDCASE.
  ELSE.
* NCW
    CASE SY-DYNNR.
      WHEN '0101'.
        IF ZTIMIMGTX-APP700 IS INITIAL.
          MOVE 'EDIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'FLAT'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'CKEK'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'APP700' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'INF700' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        ENDIF.
      WHEN '0111'.
        IF ZTIMIMGTX-LOCAPP IS INITIAL.
          MOVE 'EDIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'FLAT'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'CKEK'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'APP700' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'INF700' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        ENDIF.
      WHEN '0121'.                                     " 구매승인서.
        IF ZTIMIMGTX-APPPUR IS INITIAL.
          MOVE 'EDIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'FLAT'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'CKEK'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'PUDOC'  TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        ENDIF.
      WHEN '0131'.                                     " D/A, D/P.
      WHEN '0141'.                                     " T/T.
        IF ZTIMIMGTX-PAYORD IS INITIAL.
          MOVE 'EDIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'FLAT'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'CKEK'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'APP700' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'INF700' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'TTREQ'  TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        ENDIF.
      WHEN '1101'.
        IF ZTIMIMGTX-APP707 IS INITIAL.
          MOVE 'EDIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'CKEK'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'FLAT'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'APP707' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'INF707' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        ENDIF.
      WHEN '1111'.
        IF ZTIMIMGTX-LOCAMR IS INITIAL.
          MOVE 'EDIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'CKEK'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'FLAT'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'APP707' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'INF707' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'TTREQ'  TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        ENDIF.
      WHEN '2101'.
        MOVE 'EDIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'CKEK'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'FLAT'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
      WHEN '4101'.
        IF ZTIMIMGTX-APPCIP IS INITIAL.
          MOVE 'LGIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'EDIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'CKEK'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
          MOVE 'FLAT'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        ENDIF.
      WHEN '4501'.
        MOVE 'LGIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'EDIS'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'CKEK'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        MOVE 'FLAT'   TO IT_EXCL-FCODE.   APPEND IT_EXCL.
    ENDCASE.
  ENDIF.
*>>> READY KOREA LTD.의 Package도입으로 반?
  MOVE 'UCUR' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 화폐변경.
  MOVE 'FLAT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " FLAT DATA
  MOVE 'EDIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " EDI SEND.
*>> 비용문서.
  DESCRIBE TABLE IT_ZSIMCOST LINES W_LINE.
  IF W_LINE EQ 0.
    MOVE 'COST' TO IT_EXCL-FCODE. APPEND IT_EXCL.
  ENDIF.
*>> 비용문서(보험).
  IF ZTINS-BELNR IS INITIAL AND SY-TCODE(4) EQ 'ZIM4'.
    MOVE 'COST' TO IT_EXCL-FCODE. APPEND IT_EXCL.
  ENDIF.

*-----------------------------------------------------------------------
* PF-STATUS SETTING
*-----------------------------------------------------------------------
  SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.

ENDMODULE.                 " PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       DESC : OK-CODE Clearing
*----------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.

  MOVE OK-CODE TO W_OK_CODE.
  OK-CODE = ''.

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

  LOOP AT SCREEN.
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
    IF W_STATUS EQ C_INSU_I.
      DYNPRO = '0108'.
    ELSE.
      DYNPRO = '0102'.
    ENDIF.
  ENDIF.

  IF SY-TCODE EQ 'ZIM01' OR SY-TCODE EQ 'ZIM02' OR
     SY-TCODE EQ 'ZIM03' OR SY-TCODE EQ 'ZIM05' OR
     SY-TCODE EQ 'ZIM07' OR
     SY-TCODE EQ 'ZIM11' OR SY-TCODE EQ 'ZIM12' OR
     SY-TCODE EQ 'ZIM13' OR SY-TCODE EQ 'ZIM15' OR
     SY-TCODE EQ 'ZIM17' OR
     SY-TCODE EQ 'ZIML1' OR SY-TCODE EQ 'ZIML2' OR   " OFFER SHEET
     SY-TCODE EQ 'ZIML3' OR
     SY-TCODE EQ 'ZIM31' OR SY-TCODE EQ 'ZIM32' OR SY-TCODE EQ 'ZIM33'.

    IF TABSTRIP-ACTIVETAB EQ 'ITM1' OR TABSTRIP-ACTIVETAB EQ 'ITM2' OR
       TABSTRIP-ACTIVETAB EQ 'ITM3' OR TABSTRIP-ACTIVETAB EQ 'ITM4' OR
       TABSTRIP-ACTIVETAB EQ 'ITM5' OR TABSTRIP-ACTIVETAB EQ 'ITM6' OR
       TABSTRIP-ACTIVETAB EQ 'ITM7' OR TABSTRIP-ACTIVETAB EQ 'ITM8' OR
       TABSTRIP-ACTIVETAB EQ 'ITM9'.
      IF OK-CODE(3) NE 'ITM'.
        OK-CODE = TABSTRIP-ACTIVETAB.
      ENDIF.
    ELSE.
      OK-CODE = 'ITM1'.
    ENDIF.
  ENDIF.

  IF OK-CODE IS INITIAL.
    IF W_STATUS = C_INSU_I.
      OK-CODE = 'ITM8'.
    ELSE.
      OK-CODE = 'ITM1'.
    ENDIF.
  ENDIF.

  IF TABSTRIP-ACTIVETAB IS INITIAL.
    IF W_STATUS =  'I'.
      TABSTRIP-ACTIVETAB = 'ITM8'.
    ELSE.
      TABSTRIP-ACTIVETAB = 'ITM1'.
    ENDIF.
  ENDIF.

  IF W_STATUS EQ 'D' OR W_STATUS EQ 'R' OR W_STATUS EQ 'B'.
    IF OK-CODE EQ 'ENTR'.
      CASE TABSTRIP-ACTIVETAB.
        WHEN 'ITM1'.
          OK-CODE = 'ITM2'.
        WHEN 'ITM2'.
          OK-CODE = 'ITM3'.
        WHEN 'ITM3'.
          CASE ZSREQHD-ZFREQTY.
            WHEN  'DA' OR 'DP'.
              OK-CODE = 'ITM3'.
            WHEN OTHERS.
              OK-CODE = 'ITM4'.
          ENDCASE.
        WHEN 'ITM4'.
          CASE ZSREQHD-ZFREQTY.
            WHEN  'PU'.              " 구매승인?
              OK-CODE = 'ITM4'.
            WHEN  'LO'.              " LOCAL L/C
              IF SY-TCODE(4) EQ 'ZIM1'.
                OK-CODE = 'ITM4'.
              ELSE.
                OK-CODE = 'ITM5'.
              ENDIF.
            WHEN OTHERS.
              OK-CODE = 'ITM5'.
          ENDCASE.
        WHEN 'ITM5'.
          CASE ZTREQHD-ZFREQTY.
            WHEN  'LC'.              " MASTER L/C
              OK-CODE = 'ITM6'.
            WHEN  'LO' OR 'TT'.      " LOCAL L/C
              OK-CODE = 'ITM5'.
            WHEN OTHERS.
              OK-CODE = 'ITM6'.
          ENDCASE.
          OK-CODE = 'ITM6'.
        WHEN 'ITM6'.
          OK-CODE = 'ITM7'.
        WHEN 'ITM7'.
          OK-CODE = 'ITM8'.
        WHEN 'ITM8'.
          OK-CODE = 'ITM8'.
        WHEN 'ITM9'.
          OK-CODE = 'ITM9'.
      ENDCASE.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------------------
* TEXT SET
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_ACTION_TITLE.

*-----------------------------------------------------------------------
* Document Title SET
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_DOC_TITLE.

*-----------------------------------------------------------------------
* GUI TITLE SETTING
*-----------------------------------------------------------------------
  CASE OK-CODE.
* CASE W_OK_CODE.
    WHEN 'ITM1'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        DYNPRO = '0102'.
      ELSEIF SY-TCODE(4) EQ 'ZIM1'.
        DYNPRO = '1102'.
      ENDIF.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Request Detail'.
    WHEN 'ITM2'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        CASE ZTREQHD-ZFREQTY.
          WHEN 'LC'.    DYNPRO = '0103'.
          WHEN 'LO'.    DYNPRO = '0113'.
          WHEN 'PU'.    DYNPRO = '0123'.
          WHEN 'TT'.    DYNPRO = '0143'.
          WHEN OTHERS.  DYNPRO = '1103'.
        ENDCASE.
      ELSEIF SY-TCODE(4) EQ 'ZIM1'.
        CASE ZTREQHD-ZFREQTY.
          WHEN 'LC'.    DYNPRO = '1103'.
          WHEN 'LO'.    DYNPRO = '1103'.
          WHEN 'PU'.    DYNPRO = '0123'.
          WHEN 'TT'.    DYNPRO = '0143'.
          WHEN OTHERS.  DYNPRO = '1103'.
        ENDCASE.
      ENDIF.
      IF ZTREQHD-ZFREQTY EQ 'PU'.
        SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                      'Item & well-founded Doc'.
      ELSE.
        SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Items'.
      ENDIF.
    WHEN 'ITM3'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        CASE ZTREQHD-ZFREQTY.
          WHEN 'LC'.
            DYNPRO = '0104'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'General'.
          WHEN 'LO'.
            DYNPRO = '0114'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                'Opening application description'.
          WHEN 'PU'.
            DYNPRO = '0124'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                         'General Info'.
          WHEN 'TT'.
            DYNPRO = '0144'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                         'General Info'.
          WHEN 'DA' OR 'DP' OR 'GS'.
            DYNPRO = '0109'.
            SET TITLEBAR  'ZILC_1'
                WITH <FS_F> <FS_DOC>
                'Import recommendation & insurance'.
          WHEN OTHERS.
            DYNPRO = '0104'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                         'General Info'.
        ENDCASE.
      ELSEIF SY-TCODE(4) EQ 'ZIM1'.
        CASE ZTREQHD-ZFREQTY.
          WHEN 'LC'.
            DYNPRO = '1104'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                          'General Info'.
          WHEN 'LO'.
            DYNPRO = '1114'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                          'Change application description'.
          WHEN 'PU'.
            DYNPRO = '0124'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'General Info'.
          WHEN 'DA' OR  'DP' OR 'TT'.
            DYNPRO = '0109'.
            SET TITLEBAR  'ZILC_1'
                WITH <FS_F> <FS_DOC>
                'Import recommendation & insurance'.
          WHEN OTHERS.
            DYNPRO = '1104'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'General Info'.
        ENDCASE.
      ENDIF.

    WHEN 'ITM4'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        CASE ZTREQHD-ZFREQTY.
          WHEN 'LC'.
            DYNPRO = '0105'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Swift 1'.
          WHEN 'LO'.
            DYNPRO = '0115'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                    'Document description'.
          WHEN 'PU'.
            DYNPRO = '0125'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                    'Amount & others'.
          WHEN 'TT'.
            DYNPRO = '0145'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                   'Details & others'.
          WHEN OTHERS.
            DYNPRO = '0105'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Swift 1'.
        ENDCASE.
      ELSEIF SY-TCODE(4) EQ 'ZIM1'.
        CASE ZTREQHD-ZFREQTY.
          WHEN 'LC'.
            DYNPRO = '1105'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Swift 1'.
          WHEN 'LO'.
            DYNPRO = '1115'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                    'Other condition change'.
          WHEN 'PU'.
            DYNPRO = '0125'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                    'Amount & others'.
          WHEN OTHERS.
            DYNPRO = '1105'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Swift 1'.
        ENDCASE.
      ENDIF.
    WHEN 'ITM5'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        CASE ZTREQHD-ZFREQTY.
          WHEN 'LC'.
            DYNPRO = '0106'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Swift 2'.
          WHEN 'LO'.
            DYNPRO = '0116'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                    'Original export L/C description'.
          WHEN 'PU' OR 'TT'.
            DYNPRO = '0109'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                 'Import recommendation & insurance'.
          WHEN OTHERS.
            DYNPRO = '0106'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Swift 2'.
        ENDCASE.
      ELSEIF SY-TCODE(4) EQ 'ZIM1'.
        CASE ZTREQHD-ZFREQTY.
          WHEN 'LC'.
            DYNPRO = '1106'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Swift 2'.
          WHEN OTHERS.
            DYNPRO = '1106'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Swift 2'.
        ENDCASE.
      ENDIF.

    WHEN 'ITM6'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        CASE ZTREQHD-ZFREQTY.
          WHEN 'LO'.
            DYNPRO = '0109'.
            SET TITLEBAR 'ZILC_1' WITH <FS_F> <FS_DOC>
                                     'Insurance'.
          WHEN OTHERS.
            DYNPRO = '0107'.
            SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                                               'shipment info.'.
        ENDCASE.
      ELSE.
        DYNPRO = '0109'.
        SET TITLEBAR 'ZILC_1' WITH <FS_F> <FS_DOC>
                              'Import recommendation & insurance'.
      ENDIF.
    WHEN 'ITM7'.
      DYNPRO = '0108'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Add.Condition'.
    WHEN 'ITM8'.
      DYNPRO = '0109'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                              'Import recommendation & insurance'.
    WHEN 'ITM9'.
      DYNPRO = '0050'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                              'Import request expense'.
  ENDCASE.

  TABSTRIP-ACTIVETAB = OK-CODE.
  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0103 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSREQIT   LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0103-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ELSE.
    G_PARAM_LINE   = TC_0103-TOP_LINE.
    TC_0103-LINES = G_PARAM_LINE  +  16.
  ENDIF.
* IF TC_0103-LINES LT TC_0103-TOP_LINE.
ENDMODULE.                 " TOTAL_LINE_GET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCRCOM OUTPUT.

  PERFORM P2000_SCR_MODE_SET.

ENDMODULE.                 " OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0103_MARK_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0103_MARK_SCR0103 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0103-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
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
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSREQIT   INDEX TC_0103-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSREQIT   TO ZSREQIT.     " DATA MOVE
    MOVE: IT_ZSREQIT-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0103_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0103_1  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0103_1 OUTPUT.

* DESCRIBE TABLE IT_ZSMLCSG7G  LINES G_PARAM_LINE.   " LINE 수 GET
* TC_0103_1-LINES = G_PARAM_LINE.                     " LINE 수 정?
  G_PARAM_LINE = TC_0103_1-TOP_LINE.
  TC_0103_1-LINES = G_PARAM_LINE + 4.              " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0103_1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0103_1_MARK_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0103_1_MARK_SCR0103 OUTPUT.

  IF OK-CODE = 'MKA2'                  " mark all
     AND TC_0103_1-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK1 = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL2'.                " delete all marks
    CLEAR W_ROW_MARK1.
  ENDIF.

ENDMODULE.                 " TC_0103_1_MARK_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0103_1_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0103_1_SCR0103 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0103_1-CURRENT_LINE GT TC_0103_1-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSMLCSG7G   INDEX TC_0103_1-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSMLCSG7G TO ZSMLCSG7G.     " DATA MOVE
    MOVE: IT_ZSMLCSG7G-ZFMARK         TO W_ROW_MARK1.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0103_1_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0106  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0106 OUTPUT.

* DESCRIBE TABLE IT_ZSMLCSG7O LINES G_PARAM_LINE.   " LINE 수 GET
* TC_0106-LINES = G_PARAM_LINE.                     " LINE 수 정?
  G_PARAM_LINE = TC_0106-TOP_LINE.
  TC_0106-LINES = G_PARAM_LINE + 3.                 " LINE 수 정?
* TC_0106-LINES = TC_0106-TOP_LINE + 5.             " LINE 수 정?

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
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSMLCSG7O INDEX TC_0106-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSMLCSG7O TO ZSMLCSG7O.   " DATA MOVE
    MOVE: IT_ZSMLCSG7O-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0106_SCR0106  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0109  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0109 OUTPUT.

  DESCRIBE TABLE IT_ZSREQIL   LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0109-LINES = G_PARAM_LINE.                     " LINE 수 정?
* G_PARAM_LINE = TC_0109-TOP_LINE.
* TC_0109-LINES = G_PARAM_LINE + 5.                 " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0109  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0109_MARK_SCR0109  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0109_MARK_SCR0109 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0109-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_0109_MARK_SCR0109  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TC0109_TO_SCR0109  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TC0109_TO_SCR0109 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0109-CURRENT_LINE GT TC_0109-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSREQIL   INDEX TC_0109-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSREQIL   TO ZSREQIL.     " DATA MOVE
    MOVE: IT_ZSREQIL-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TC0109_TO_SCR0109  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0109  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0109 INPUT.

  GET CURSOR LINE W_LINE_0109 FIELD F.        "CURSOR_2 = Nummer der
  W_LINE_0109 = TC_0109-CURRENT_LINE + W_LINE_0109 - 1.

ENDMODULE.                 " GET_LINE_SCR0109  INPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0107  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0107 OUTPUT.
* DESCRIBE TABLE IT_ZSMLCSG7O LINES G_PARAM_LINE.   " LINE 수 GET
* TC_0106-LINES = G_PARAM_LINE.                     " LINE 수 정?
  G_PARAM_LINE = TC_0107-TOP_LINE.
  TC_0107-LINES = G_PARAM_LINE + 4.                 " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0107  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0107_MARK_SCR0107  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0107_MARK_SCR0107 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0107-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_0107_MARK_SCR0107  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0107_SCR0107  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0107_SCR0107 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0107-CURRENT_LINE GT TC_0107-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSMLCSG8E INDEX TC_0107-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSMLCSG8E TO ZSMLCSG8E.   " DATA MOVE
    MOVE: IT_ZSMLCSG8E-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0107_SCR0107  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0108_MARK_SCR0108  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0108_MARK_SCR0108 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0108-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_0108_MARK_SCR0108  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0108_SCR0108  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0108_SCR0108 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0108-CURRENT_LINE GT TC_0108-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSMLCSG9O INDEX TC_0108-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSMLCSG9O TO ZSMLCSG9O.   " DATA MOVE
    MOVE: IT_ZSMLCSG9O-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0108_SCR0108  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0108  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0108 OUTPUT.
* DESCRIBE TABLE IT_ZSMLCSG7O LINES G_PARAM_LINE.   " LINE 수 GET
* TC_0106-LINES = G_PARAM_LINE.                     " LINE 수 정?
  G_PARAM_LINE = TC_0108-TOP_LINE.
  TC_0108-LINES = G_PARAM_LINE + 21.                 " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0108  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0109_1  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0109_1 OUTPUT.

  DESCRIBE TABLE IT_ZSINSHD   LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0109_1-LINES = G_PARAM_LINE.                     " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0109_1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0109_1_MARK_SCR0109_1  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0109_1_MARK_SCR0109_1 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0109_1-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK1 = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK1.
  ENDIF.

ENDMODULE.                 " TC_0109_1_MARK_SCR0109_1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TC0109_1_TO_SCR0109_1  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TC0109_1_TO_SCR0109_1 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0109_1-CURRENT_LINE GT TC_0109_1-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSINSHD   INDEX TC_0109_1-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSINSHD   TO ZSINSHD.     " DATA MOVE
    MOVE: IT_ZSINSHD-ZFMARK         TO W_ROW_MARK1. " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TC0109_1_TO_SCR0109_1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0114  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0114 OUTPUT.

* DESCRIBE TABLE IT_ZSMLCSG7G  LINES G_PARAM_LINE.   " LINE 수 GET
* TC_0103_1-LINES = G_PARAM_LINE.                     " LINE 수 정?
  G_PARAM_LINE = TC_0114-TOP_LINE.
  TC_0114-LINES = G_PARAM_LINE + 2.              " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0114  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0114_MARK_SCR0114  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0114_MARK_SCR0114 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0114-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_0114_MARK_SCR0114  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0114_SCR0114  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0114_SCR0114 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0114-CURRENT_LINE GT TC_0114-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSLLCOF   INDEX TC_0114-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSLLCOF   TO ZSLLCOF.     " DATA MOVE
    MOVE: IT_ZSLLCOF-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0114_SCR0114  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0123  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0123 OUTPUT.

  DESCRIBE TABLE IT_ZSPURSG1   LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0123-LINES = G_PARAM_LINE.                     " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0123  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0123_MARK_SCR0123  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0123_MARK_SCR0123 OUTPUT.

  IF OK-CODE = 'MKA2'                  " mark all
     AND TC_0123-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK1 = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL2'.                " delete all marks
    CLEAR W_ROW_MARK1.
  ENDIF.

ENDMODULE.                 " TC_0123_MARK_SCR0123  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0123_SCR0123  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0123_SCR0123 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0123-CURRENT_LINE GT TC_0123-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSPURSG1   INDEX TC_0123-CURRENT_LINE.
  IF SY-SUBRC = 0.                                      " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSPURSG1  TO ZSPURSG1.       " DATA MOVE
    MOVE: IT_ZSPURSG1-ZFMARK        TO W_ROW_MARK1.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0123_SCR0123  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0004  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0004 OUTPUT.
  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSPURSG1G_SUB LINES G_PARAM_LINE.  " LINE 수 GET
    TC_0004-LINES = G_PARAM_LINE.                        " LINE 수 정?
  ELSE.
    G_PARAM_LINE = TC_0004-TOP_LINE.
    TC_0004-LINES = G_PARAM_LINE + 8.                 " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0004  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0004_MARK_SCR0004  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0004_MARK_SCR0004 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_0004-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.


ENDMODULE.                 " TC_0004_MARK_SCR0004  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0004_SCR0004  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0004_SCR0004 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0004-CURRENT_LINE GT TC_0004-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSPURSG1G_SUB INDEX TC_0004-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSPURSG1G_SUB TO ZSPURSG1G.   " DATA MOVE
    MOVE: IT_ZSPURSG1G_SUB-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0004_SCR0004  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0125  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0125 OUTPUT.

* DESCRIBE TABLE IT_ZSPURSG4  LINES G_PARAM_LINE.   " LINE 수 GET
* TC_0125-LINES = G_PARAM_LINE.                     " LINE 수 정?
  TC_0125-LINES = 3.                                " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0125  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0125_MARK_SCR0125  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0125_MARK_SCR0125 OUTPUT.

  IF OK-CODE = 'MKA3'                  " mark all
     AND TC_0125-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK2 = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL3'.                " delete all marks
    CLEAR W_ROW_MARK2.
  ENDIF.

ENDMODULE.                 " TC_0125_MARK_SCR0125  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0125_SCR0125  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0125_SCR0125 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0125-CURRENT_LINE GT TC_0125-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSPURSG4  INDEX TC_0125-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSPURSG4  TO ZSPURSG4.    " DATA MOVE
    MOVE: IT_ZSPURSG4-ZFMARK        TO W_ROW_MARK2. " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0125_SCR0125  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR4101  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR4101 OUTPUT.
  IF PROGRAM IS INITIAL.
    PROGRAM = SY-CPROG.        " Program SET
  ENDIF.
*
  IF DYNPRO IS INITIAL.
    DYNPRO = '4102'.
  ENDIF.

  IF SY-TCODE EQ 'ZIM41' OR SY-TCODE EQ 'ZIM42' OR
     SY-TCODE EQ 'ZIM43' OR SY-TCODE EQ 'ZIM44' OR
     SY-TCODE EQ 'ZIM45' OR
     SY-TCODE EQ 'ZIM46' OR SY-TCODE EQ 'ZIM47' OR
     SY-TCODE EQ 'ZIM48'.

    IF TABSTRIP-ACTIVETAB EQ 'ITM1' OR TABSTRIP-ACTIVETAB EQ 'ITM2' OR
       TABSTRIP-ACTIVETAB EQ 'ITM3' OR TABSTRIP-ACTIVETAB EQ 'ITM4' OR
       TABSTRIP-ACTIVETAB EQ 'ITM5'.
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
          OK-CODE = 'ITM5'.
        WHEN 'ITM5'.
          OK-CODE = 'ITM5'.
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
      DYNPRO = '4102'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'General info'.
    WHEN 'ITM2'.
      DYNPRO = '4103'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Insurance contract'.
    WHEN 'ITM3'.
      DYNPRO = '4104'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Related items'.
    WHEN 'ITM4'.
      DYNPRO = '4105'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                              'Additional condition'.
    WHEN 'ITM5'.
      DYNPRO = '4106'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Other info'.
  ENDCASE.

  TABSTRIP-ACTIVETAB = OK-CODE.
  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR4101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR4104  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR4104 OUTPUT.

  TC_4104-LINES = 3.                    " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR4104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_4104_MARK_SCR4104  OUTPUT
*&---------------------------------------------------------------------*
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
MODULE IT_TO_TC4104_SCR4104 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_4104-CURRENT_LINE GT TC_4104-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSINSSG2  INDEX TC_4104-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSINSSG2  TO ZSINSSG2.    " DATA MOVE
    MOVE: IT_ZSINSSG2-ZFMARK        TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC4104_SCR4104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR4105  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR4105 OUTPUT.
* DESCRIBE TABLE IT_ZSMLCSG7O LINES G_PARAM_LINE.   " LINE 수 GET
* TC_0106-LINES = G_PARAM_LINE.                     " LINE 수 정?
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
MODULE IT_TO_TC4105_SCR4105 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_4105-CURRENT_LINE GT TC_4105-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSINSAGR  INDEX TC_4105-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSINSAGR  TO ZSINSAGR.    " DATA MOVE
    MOVE: IT_ZSINSAGR-ZFMARK        TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC4105_SCR4105  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR4105_1  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR4105_1 OUTPUT.

* DESCRIBE TABLE IT_ZSMLCSG7G  LINES G_PARAM_LINE.   " LINE 수 GET
* TC_0103_1-LINES = G_PARAM_LINE.                     " LINE 수 정?
  G_PARAM_LINE = TC_4105_1-TOP_LINE.
  TC_4105_1-LINES = G_PARAM_LINE + 3.              " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR4105_1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_4105_1_MARK_SCR4105  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_4105_1_MARK_SCR4105 OUTPUT.

  IF OK-CODE = 'MKA2'                  " mark all
     AND TC_4105_1-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK1 = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL2'.                " delete all marks
    CLEAR W_ROW_MARK1.
  ENDIF.

ENDMODULE.                 " TC_4105_1_MARK_SCR4105  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC4105_1_SCR4105  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC4105_1_SCR4105 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_4105_1-CURRENT_LINE GT TC_4105_1-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSINSSG5    INDEX TC_4105_1-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSINSSG5 TO ZSINSSG5.     " DATA MOVE
    MOVE: IT_ZSINSSG5-ZFMARK       TO W_ROW_MARK1.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC4105_1_SCR4105  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR4501  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR4501 OUTPUT.
  IF PROGRAM IS INITIAL.
    PROGRAM = SY-CPROG.        " 프로그램 SET
  ENDIF.
*
  IF DYNPRO IS INITIAL.
    DYNPRO = '4102'.
  ENDIF.

  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR
     SY-TCODE EQ 'ZIM47' OR SY-TCODE EQ 'ZIM48'.

    IF TABSTRIP-ACTIVETAB EQ 'ITM1' OR TABSTRIP-ACTIVETAB EQ 'ITM2' OR
       TABSTRIP-ACTIVETAB EQ 'ITM3' OR TABSTRIP-ACTIVETAB EQ 'ITM4'.
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
      DYNPRO = '4502'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'General info'.
    WHEN 'ITM2'.
      DYNPRO = '4503'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Insurance contract'.
    WHEN 'ITM3'.
      DYNPRO = '4104'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Related items'.
    WHEN 'ITM4'.
      DYNPRO = '4505'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                              'Additional condition'.
  ENDCASE.

  TABSTRIP-ACTIVETAB = OK-CODE.
  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR4501  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DYNSUB_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE DYNSUB_CLEAR_SCRCOM OUTPUT.

  CLEAR : PROGRAM, DYNSUB, DYNPRO.

ENDMODULE.                 " DYNSUB_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE TABLE_STATUS_SCRCOM OUTPUT.

  REFRESH : IT_EXCL.

  IF W_STATUS EQ 'D'.
    ASSIGN W_DISPLAY TO <FS_F>.
  ELSEIF W_STATUS EQ 'C'.
    ASSIGN W_CHANGE  TO <FS_F>.
  ELSEIF W_STATUS EQ 'I'.
    ASSIGN W_CREATE  TO <FS_F>.
  ENDIF.

*-----------------------------------------------------------------------
* PF-STATUS Setting
*-----------------------------------------------------------------------
  CASE SY-TCODE.
    WHEN 'ZIMC1'.
      MOVE 'EULG' TO W_PFSTAT.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

*-----------------------------------------------------------------------
* PF-STATUS상의 Inactive Function Setting
*-----------------------------------------------------------------------
  CASE SY-TCODE.
    WHEN 'ZIMC1'.          " 수입 비용 Setting
      IF SY-DYNNR EQ '0190'.
        SET PF-STATUS 'EUL1'.
        SET TITLEBAR  '0190'.
      ELSEIF SY-DYNNR EQ '0191'.
* Disable Menu Status Setting
        PERFORM  P2000_SET_DISABLE_MENU.
        SET TITLEBAR '0191' WITH <FS_F>.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " TABLE_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCR0190  OUTPUT
*&---------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCR0190 OUTPUT.

  LOOP AT SCREEN.
    CASE W_STATUS.
      WHEN 'I'.
        IF SCREEN-GROUP2 = 'CR'.
          SCREEN-INPUT   = '1'.
        ENDIF.
      WHEN 'C'.
        IF SCREEN-GROUP1 = 'IO'.
          IF SCREEN-GROUP2 = 'CR'.
            SCREEN-INPUT   = '1'.
          ELSE.
            SCREEN-INPUT   = '0'.
          ENDIF.
        ELSE.
          IF SCREEN-GROUP2 = 'CR'.
            SCREEN-INPUT   = '0'.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
        IF SCREEN-GROUP1 = 'IO' OR SCREEN-GROUP2 = 'CR'.
          SCREEN-INPUT   = '0'.
        ENDIF.
    ENDCASE.
*>> 전표발생건인 경우는 DISPLAY MODE.
    IF NOT ( IT_ZSRECST-ZFACDO IS INITIAL ) AND
       SCREEN-GROUP4 = 'ST'.
      SCREEN-INPUT  = '0'.
    ENDIF.

* DISPLAY MODE OR CHANGE MODE 시에도 조회 BUTTON은 ENABLE.
    IF SCREEN-NAME(5) = 'P_DOC'.
      SCREEN-INPUT   =  '1'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " OPERATION_MODE_SET_SCR0190  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR2101  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR2101 OUTPUT.

  IF PROGRAM IS INITIAL.
    PROGRAM = SY-CPROG.        " 프로그램 SET
  ENDIF.
*
  IF DYNPRO IS INITIAL.
    DYNPRO = '2102'.
  ENDIF.

  IF SY-TCODE EQ 'ZIML1' OR SY-TCODE EQ 'ZIML2' OR   " OFFER SHEET
     SY-TCODE EQ 'ZIML3'.

    IF TABSTRIP-ACTIVETAB EQ 'ITM1' OR TABSTRIP-ACTIVETAB EQ 'ITM2' OR
       TABSTRIP-ACTIVETAB EQ 'ITM3' OR TABSTRIP-ACTIVETAB EQ 'ITM4'.
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
      DYNPRO = '2102'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                              'Request description'.
    WHEN 'ITM2'.
      DYNPRO = '2103'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                              'Item description'.
    WHEN 'ITM3'.
      DYNPRO = '2104'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Other info1'.
    WHEN 'ITM4'.
      DYNPRO = '2105'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC> 'Other info2'.
    WHEN OTHERS.
  ENDCASE.

  TABSTRIP-ACTIVETAB = OK-CODE.
  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR2101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR0151  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR0151 OUTPUT.

  IF PROGRAM IS INITIAL.
    PROGRAM = SY-CPROG.        " 프로그램 SET
  ENDIF.
*
  IF DYNPRO IS INITIAL.
    DYNPRO = '0102'.
  ENDIF.

  IF SY-TCODE EQ 'ZIM01' OR SY-TCODE EQ 'ZIM02'.
    IF TABSTRIP-ACTIVETAB EQ 'ITM1' OR TABSTRIP-ACTIVETAB EQ 'ITM2'.
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
      DYNPRO = '0152'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                              'Request description'.
    WHEN 'ITM2'.
      DYNPRO = '0153'.
      SET TITLEBAR  'ZILC_1'  WITH <FS_F> <FS_DOC>
                              'Item description'.
  ENDCASE.

  IF OK-CODE(3) NE 'ITM'.
    OK-CODE = 'ITM1'.
  ENDIF.

  TABSTRIP-ACTIVETAB = OK-CODE.
  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR0151  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0106_SCR0152  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0106_SCR0152 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0106-CURRENT_LINE GT TC_0106-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZTREQORJ  INDEX TC_0106-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZTREQORJ  TO ZSMLCSG7O.   " DATA MOVE
    MOVE: IT_ZTREQORJ-ZFMARK        TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0106_SCR0152  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0152_MARK_TC_0106  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0152_MARK_TC_0106 INPUT.

  READ TABLE IT_ZTREQORJ  "WITH KEY ZSMLCSG7O(5)  BINARY SEARCH.
                         WITH KEY ZFLSG7O = ZSMLCSG7O-ZFLSG7O.

  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING   ZSMLCSG7O TO IT_ZTREQORJ.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZTREQORJ-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZTREQORJ-ZFMARK.
    ENDIF.

    MODIFY IT_ZTREQORJ INDEX SY-TABIX.

  ENDIF.

ENDMODULE.                 " SET_SCR0152_MARK_TC_0106  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0003  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0003 INPUT.
  CLEAR : ANTWORT.
  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
*   WHEN OTHERS.
*      ANTWORT = 'Y'.
  ENDCASE.
  IF NOT ANTWORT IS INITIAL.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.
ENDMODULE.                 " GET_OK_CODE_SCR0003  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0002  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0002 INPUT.

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

ENDMODULE.                 " GET_OK_CODE_SCR0002  INPUT
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
*&      Module  TOTAL_LINE_GET_SCR0102  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0102 OUTPUT.

* DESCRIBE TABLE IT_ZSMLCSG7O LINES G_PARAM_LINE.   " LINE 수 GET
* TC_0106-LINES = G_PARAM_LINE.                     " LINE 수 정?
  G_PARAM_LINE = TC_0102-TOP_LINE.
  TC_0102-LINES = G_PARAM_LINE + 2.                 " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0102  OUTPUT
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
*&      Module  IT_TO_TC0102_SCR0102  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0102_SCR0102 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0102-CURRENT_LINE GT TC_0102-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZTREQORJ  INDEX TC_0102-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZTREQORJ  TO ZSMLCSG7O.   " DATA MOVE
    MOVE: IT_ZTREQORJ-ZFMARK        TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0102_SCR0102  OUTPUT
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
*&      Module  TC_0106_ENABLE_SET_SCR0152  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0106_ENABLE_SET_SCR0152 OUTPUT.

  PERFORM   P2000_FIELD_MODE_SET   USING  IT_ZTREQORJ-LOEKZ.

ENDMODULE.                 " TC_0106_ENABLE_SET_SCR0152  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0103_ENABLE_SET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0103_ENABLE_SET_SCR0103 OUTPUT.
  PERFORM   P2000_MENGE_MODE_SET.
ENDMODULE.                 " TC_0103_ENABLE_SET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0107_MARK_TC_0107  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0107_MARK_TC_0107 INPUT.

  READ TABLE IT_ZSMLCSG8E WITH KEY
                       ZFLSG8E = ZSMLCSG8E-ZFLSG8E  BINARY SEARCH.

  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING   ZSMLCSG8E TO IT_ZSMLCSG8E.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSMLCSG8E-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSMLCSG8E-ZFMARK.
    ENDIF.
    MODIFY IT_ZSMLCSG8E INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0107_MARK_TC_0107  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0107_ENABLE_SET_SCR0107  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0107_ENABLE_SET_SCR0107 OUTPUT.
  PERFORM   P2000_FIELD_MODE_SET   USING  IT_ZSMLCSG8E-LOEKZ.
ENDMODULE.                 " TC_0107_ENABLE_SET_SCR0107  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0108_ENABLE_SET_SCR0108  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0108_ENABLE_SET_SCR0108 OUTPUT.
  PERFORM   P2000_FIELD_MODE_SET   USING  IT_ZSMLCSG9O-LOEKZ.
ENDMODULE.                 " TC_0108_ENABLE_SET_SCR0108  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0109_ENABLE_SET_SCR0109  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0109_ENABLE_SET_SCR0109 OUTPUT.
  PERFORM   P2000_FIELD_MODE_SET   USING  ''.
ENDMODULE.                 " TC_0109_ENABLE_SET_SCR0109  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0103_1_ENABLE_SET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0103_1_ENABLE_SET_SCR0103 OUTPUT.
  PERFORM   P2000_FIELD_MODE_SET   USING  IT_ZSMLCSG7G-LOEKZ.
ENDMODULE.                 " TC_0103_1_ENABLE_SET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_ZSPURSG1G_ZFGOSI_SCR0008  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_ZSPURSG1G_ZFGOSI_SCR0008 OUTPUT.
  CLEAR : ZSPURSG1G.
  LOOP AT IT_ZSPURSG1G WHERE ZFLSG1 EQ ZTPURSG1-ZFLSG1.
    MOVE-CORRESPONDING IT_ZSPURSG1G TO ZSPURSG1G.
    MOVE : IT_ZSPURSG1G-ZFGOSI      TO ZSPURSG1G-ZFGOSI.
    EXIT.
  ENDLOOP.

ENDMODULE.                 " SET_ZSPURSG1G_ZFGOSI_SCR0008  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0012  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0012 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0012  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR2103 OUTPUT.

  DESCRIBE TABLE IT_ZSOFFSG6   LINES G_PARAM_LINE.   " LINE 수 GET
  TC_2103-LINES = G_PARAM_LINE.                     " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_2103_MARK_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_2103_MARK_SCR2103 OUTPUT.

  IF OK-CODE = 'MKA2'                  " mark all
     AND TC_2103-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK  = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL2'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_2103_MARK_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC2103_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC2103_SCR2103 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_2103-CURRENT_LINE GT TC_2103-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSOFFSG6   INDEX TC_2103-CURRENT_LINE.
  IF SY-SUBRC = 0.                                      " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSOFFSG6  TO ZSOFFSG6.       " DATA MOVE
    MOVE: IT_ZSOFFSG6-ZFMARK        TO W_ROW_MARK.   " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC2103_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_2103_1_MARK_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_2103_1_MARK_SCR2103 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_2103_1-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK1 = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK1.
  ENDIF.

ENDMODULE.                 " TC_2103_1_MARK_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC2103_1_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC2103_1_SCR2103 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_2103_1-CURRENT_LINE GT TC_2103_1-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSOFFO INDEX TC_2103_1-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSOFFO    TO ZSOFFO.      " DATA MOVE
    MOVE: IT_ZSOFFO-ZFMARK          TO W_ROW_MARK1. " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC2103_1_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OFFSG6_GET_LINE_SCR2103  INPUT
*&---------------------------------------------------------------------*
MODULE OFFSG6_GET_LINE_SCR2103 INPUT.

  GET CURSOR LINE W_LINE2103 FIELD F.        "CURSOR_2 = Nummer der
  W_LINE2103 = TC_2103-CURRENT_LINE + W_LINE2103 - 1.

ENDMODULE.                 " OFFSG6_GET_LINE_SCR2103  INPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR2103_1  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR2103_1 OUTPUT.

  G_PARAM_LINE = TC_2103_1-TOP_LINE.
  TC_2103_1-LINES = G_PARAM_LINE + 5.                 " LINE 수 정?
*  TC_2103_1-LINES = TC_2103_1-TOP_LINE + 5.             " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR2103_1  OUTPUT
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
    WHEN 'INCREATE' OR 'INCREAT1'.
      SET TITLEBAR 'POPU' WITH 'Partial referance LIST'.
    WHEN 'LCCHANGE' OR 'LCDISPLY' OR 'LCDISPL1'.
      SET TITLEBAR 'POPU' WITH 'Duplicated import request LIST'.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  VARIANT_CLEAR_SCR4500  OUTPUT
*&---------------------------------------------------------------------*
MODULE VARIANT_CLEAR_SCR4500 OUTPUT.

  CLEAR : ZSREQHD-ZFAMDNO.

ENDMODULE.                 " VARIANT_CLEAR_SCR4500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0143  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0143 OUTPUT.

* DESCRIBE TABLE IT_ZSMLCSG7G  LINES G_PARAM_LINE.   " LINE 수 GET
* TC_0103_1-LINES = G_PARAM_LINE.                     " LINE 수 정?
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
*&      Module  IT_TO_TC1199_SCR1199  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC1199_SCR1199 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_1199-CURRENT_LINE GT TC_1199-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSAMDLIST  INDEX TC_1199-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSAMDLIST  TO ZSAMDLIST.   " DATA MOVE
  ENDIF.

ENDMODULE.                 " IT_TO_TC1199_SCR1199  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_INIT_VALUE_SCR0190  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_INIT_VALUE_SCR0190 OUTPUT.
  ZSREQHD-ZFOCDT = SY-DATUM.
ENDMODULE.                 " SET_INIT_VALUE_SCR0190  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_DEFAULT_SCR0115  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_DEFAULT_SCR0115 OUTPUT.

*   IF ZTLLCHD-ZFETC1 IS INITIAL.
*      ZTLLCHD-ZFETC1 = '117548930063'.
*   ENDIF.
*   IF ZTLLCHD-ZFETC3 IS INITIAL.
*      ZTLLCHD-ZFETC3 = '현대전자LOCAL L/C담당：김현제 TEL:3459-5430'.
*   ENDIF.

ENDMODULE.                 " SET_DEFAULT_SCR0115  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUPPRESS_DIALOG_SCR0050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SUPPRESS_DIALOG_SCR0050 OUTPUT.
  SUPPRESS DIALOG.
ENDMODULE.                 " SET_SUPPRESS_DIALOG_SCR0050  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0017_STATUS_SCR0017  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0017_STATUS_SCR0017 OUTPUT.
  SET PF-STATUS 'ERRLIST'.

  CASE INCLUDE.
    WHEN 'POPU'.
      SET TITLEBAR 'POPU' WITH 'Message LIST'.
    WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0017_STATUS_SCR0017  OUTPUT
