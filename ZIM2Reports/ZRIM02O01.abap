*&---------------------------------------------------------------------*
*& INCLUDE ZRIM02O01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입비용 Main PBO MODULE Include                      *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.05.14                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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
*&      Module  PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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

  IF ZTBKPF-ZFCSTGRP EQ '008'.
    MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 삭제.
    MOVE 'CCDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 전기취소.
    MOVE 'OPDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 전기.
    MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 전기.
  ENDIF.
*-----------------------------------------------------------------------
* PF-STATUS SETTING
*-----------------------------------------------------------------------
  SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.


ENDMODULE.                 " PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.

  MOVE OK-CODE TO W_OK_CODE.
  CLEAR : OK-CODE.

ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0100 OUTPUT.

  IF W_STATUS EQ C_REQ_C OR W_STATUS EQ C_REQ_U.
    G_PARAM_LINE = TC_0100-TOP_LINE.
    TC_0100-LINES = G_PARAM_LINE + 06.               " LINE 수 정의.
  ELSEIF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSBSEG   LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0100-LINES = G_PARAM_LINE.                    " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0100_ENABLE_SET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0100_ENABLE_SET_SCR0100 OUTPUT.

  LOOP AT SCREEN.
    CASE W_STATUS.
      WHEN C_REQ_C OR C_REQ_U.              " 생성, 변?
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
      WHEN C_REQ_D OR C_ADD_D OR C_OPEN_D.  " 조회, 추가조회, 확정조?
        IF SCREEN-GROUP1 = 'I'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

*>> 비용자동생성..
    IF ZTIMIMG00-ZFATIC EQ 'X'.
      READ TABLE IT_ZSBSEG INDEX 1.
      IF ZTBKPF-ZFCSTGRP EQ '003' AND IT_ZSBSEG-ZFCD EQ '1AB'.
        SCREEN-INPUT     = '0'.
      ENDIF.
    ENDIF.

    IF ZTBKPF-ZFCSTGRP NE '003'.
      IF SCREEN-NAME EQ 'ZSBSEG-ZFVPR'   OR
         SCREEN-NAME EQ 'ZSBSEG-ZFAMDNO' OR
         SCREEN-NAME EQ 'ZSBSEG-ZFINSEQ'.
        SCREEN-INPUT     = '0'.
      ENDIF.
    ELSE.
      READ TABLE IT_ZSBSEG INDEX 1.
      IF SY-SUBRC EQ 0.
        IF NOT IT_ZSBSEG-ZFCD EQ '1AB'.
          IF SCREEN-NAME EQ 'ZSBSEG-ZFVPR'   OR
             SCREEN-NAME EQ 'ZSBSEG-ZFAMDNO' OR
             SCREEN-NAME EQ 'ZSBSEG-ZFINSEQ'.
            SCREEN-INPUT     = '0'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*> 세금자동계산.
    IF ZTBKPF-XMWST EQ 'X' AND SCREEN-NAME  EQ 'ZTBKPF-WMWST'.
       SCREEN-INPUT     = '0'.
       SCREEN-INVISIBLE = '1'.
    ENDIF.

    IF ZTBKPF-WAERS EQ T001-WAERS AND
     ( SCREEN-NAME EQ 'ZTBKPF-HWAER' OR
       SCREEN-NAME EQ 'ZTBKPF-DMBTR' OR
       SCREEN-NAME EQ 'ZTBKPF-KURSF' OR
       SCREEN-NAME EQ 'ZTBKPF-WWERT' ).
      SCREEN-INPUT     = '0'.
      SCREEN-INVISIBLE = '1'.
    ENDIF.

    " HYUNDAI AUTO. NHJ MODIFICATION.
    IF ZTIMIMG00-ZFBPLK IS INITIAL.
       IF SCREEN-NAME EQ 'ZTBKPF-BUPLA'.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
    ENDIF.

    IF ZTIMIMG00-ZFBALK IS INITIAL.
       IF SCREEN-NAME EQ 'ZTBKPF-GSBER'.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
    ENDIF.

    IF ZTBKPF-ZFPOSYN EQ 'N' AND
     ( SCREEN-NAME EQ 'ZTBKPF-ZFFIYR' OR
       SCREEN-NAME EQ 'ZTBKPF-ZFACDO' ).
      SCREEN-INPUT     = '0'.
      SCREEN-INVISIBLE = '1'.
    ENDIF.
    IF ZTBKPF-ZFPOSYN EQ 'N' AND
     ( SCREEN-NAME EQ 'ZTBKPF-ZFCLYR' OR
       SCREEN-NAME EQ 'ZTBKPF-ZFCLNO' ).
      SCREEN-INPUT     = '0'.
      SCREEN-INVISIBLE = '1'.
    ENDIF.
    IF ZTBKPF-ZFPOSYN EQ 'N' AND
     ( SCREEN-NAME EQ 'ZTBKPF-ZFPYYR' OR
       SCREEN-NAME EQ 'ZTBKPF-ZFPYNO' ).
      SCREEN-INPUT     = '0'.
      SCREEN-INVISIBLE = '1'.
    ENDIF.

    IF ZTIMIMG00-ZFAVPT NE 'X' AND
     ( SCREEN-NAME EQ 'ZTBKPF-ZFCLYR' OR
       SCREEN-NAME EQ 'ZTBKPF-ZFCLNO' ).
      SCREEN-INPUT     = '0'.
      SCREEN-INVISIBLE = '1'.
    ENDIF.

    IF ZTIMIMG00-ZFPYPT NE 'X' AND
     ( SCREEN-NAME EQ 'ZTBKPF-ZFPYYR' OR
       SCREEN-NAME EQ 'ZTBKPF-ZFPYNO' ).
      SCREEN-INPUT     = '0'.
      SCREEN-INVISIBLE = '1'.
    ENDIF.

*      READ TABLE IT_ZSBSEG INDEX 1.
*      IF ZTBKPF-ZFCSTGRP EQ '003' AND IT_ZSBSEG-ZFCD EQ '1AB'.
*         IF SCREEN-NAME EQ 'ROW_INSERT' OR
*            SCREEN-NAME EQ 'DELETE_ONE_LINE'.
*            SCREEN-INPUT   =  '0'.
*         ENDIF.
*      ENDIF.
    IF ZTBKPF-WAERS EQ ZTBKPF-HWAER AND
       SCREEN-NAME EQ 'ZTBKPF-ZFPCUR'.
      SCREEN-INPUT   =  '0'.
    ENDIF.
*> 생성이 아닐 경우, ==> 회사코드 막음.
    IF SY-TCODE NE 'ZIMY1' AND SCREEN-NAME EQ 'ZTBKPF-BUKRS'.
      SCREEN-INPUT   =  '0'.
    ENDIF.

    IF SCREEN-NAME EQ 'W_ROW_MARK'.
      SCREEN-INPUT   =  '1'.
    ENDIF.

    IF ZTIMIMG00-ZFAVPT NE 'X'.
      IF SCREEN-NAME EQ 'ZTBKPF-HKONT' OR
         SCREEN-NAME EQ 'ZTBKPF-ZFADVPT'.
        SCREEN-INVISIBLE =  '1'.
        SCREEN-INPUT     =  '0'.
      ENDIF.
    ENDIF.

    IF ZTIMIMG00-ZFPYPT NE 'X'.
      IF SCREEN-NAME EQ 'ZTBKPF-ZFPYN' OR
         SCREEN-NAME EQ 'ZTBKPF-ZFPYPT'.
        SCREEN-INVISIBLE =  '1'.
        SCREEN-INPUT     =  '0'.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " TC_0100_ENABLE_SET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_INIT_FIELD_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_INIT_FIELD_SCR0100 OUTPUT.

  IF W_FIRST_SCR0100 EQ 'Y'.

    W_EDIT_CHECK = 'N'.

*>> IMG 유지보수.
    SELECT SINGLE * FROM ZTIMIMG00.
    IF SY-SUBRC NE 0.
      MESSAGE S961.
      LEAVE TO SCREEN 0.
    ELSE.
      IF ZTIMIMG00-ZFPSMS NE '2'.
        MESSAGE S573.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.
*>> STATUS FIELD SET.
    CASE SY-TCODE.
      WHEN 'ZIMY1'.   "생성.
        MOVE : C_REQ_C   TO   W_STATUS.
      WHEN 'ZIMY2'.   "변경.
        MOVE : C_REQ_U   TO   W_STATUS.
      WHEN 'ZIMY3'.   "조회.
        MOVE : C_REQ_D   TO   W_STATUS.
      WHEN 'ZIMY4'.
      WHEN OTHERS.
    ENDCASE.

*>> INITIALIZE.
    PERFORM P2000_DATA_INITIALIZE.
*>>
    CASE SY-TCODE.
      WHEN 'ZIMY1'.
        PERFORM   P2000_SET_CREATE_FIELD_VALUE.
      WHEN 'ZIMY2' OR 'ZIMY3'.   ">변경 및 조회일 경우...
        GET PARAMETER ID 'BUK'    FIELD ZSBKPF-BUKRS.
        GET PARAMETER ID 'GJR'    FIELD ZSBKPF-GJAHR.
        GET PARAMETER ID 'ZPBENR' FIELD ZSBKPF-BELNR.

        ZTBKPF-BUKRS = ZSBKPF-BUKRS.

        IF NOT ZTBKPF-BUKRS IS INITIAL.
*>> IMG 비용계정코드.
          SELECT SINGLE * FROM ZTIMIMG11
                 WHERE    BUKRS  EQ   ZTBKPF-BUKRS.
          IF SY-SUBRC NE 0.
            MESSAGE S987 WITH ZTBKPF-BUKRS.
            LEAVE TO SCREEN 0.
          ENDIF.
*>> COMMPANY CODE CHECK.
          PERFORM  P1000_GET_COMPANY_CODE USING ZTBKPF-BUKRS.
        ENDIF.

        IF ZSBKPF-GJAHR IS INITIAL OR ZSBKPF-GJAHR EQ '0000' OR
           ZSBKPF-GJAHR EQ ' 000'  OR ZSBKPF-GJAHR EQ '  00' OR
           ZSBKPF-GJAHR EQ '   0'  OR ZSBKPF-GJAHR EQ SPACE.
          ZSBKPF-GJAHR = SY-DATUM(4).
        ENDIF.

        IF ZTBKPF-HWAER IS INITIAL.
          MOVE : 'USD' TO ZTBKPF-HWAER.
        ENDIF.

        IF ZSBKPF-BUKRS IS INITIAL OR
           ZSBKPF-BELNR IS INITIAL OR
           ZSBKPF-GJAHR IS INITIAL.
          PERFORM   P2000_SET_CREATE_FIELD_VALUE.
          EXIT.
        ENDIF.
*> CHARGE DOCUMENT READ.
        PERFORM   P1000_GET_CHARGE_DOCUMENT  USING   'S'.
        IF W_READ_ERROR = 'N'.
          PERFORM   P2000_BALANCE_CALCULATE.
        ENDIF.

      WHEN OTHERS.

    ENDCASE.

    MOVE : 'N'               TO W_FIRST_SCR0100,
           ZTBKPF-WAERS      TO RF05A-UBAZW.
  ENDIF.

ENDMODULE.                 " SET_INIT_FIELD_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0100_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0100_SCR0100 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0100-CURRENT_LINE GT TC_0100-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSBSEG INDEX TC_0100-CURRENT_LINE.
  IF SY-SUBRC = 0.                                  " READ SUCCESS..
    MOVE-CORRESPONDING IT_ZSBSEG   TO ZSBSEG.      " DATA MOVE
    MOVE: IT_ZSBSEG-ZFMARK         TO W_ROW_MARK.  " MARK SET
    IF ZSBSEG-NEWKO IS INITIAL AND NOT ZSBSEG-ZFIMDNO IS INITIAL.
      PERFORM  P2000_SET_NEWKO  USING  ZSBSEG-NEWKO
                                       ZSBSEG-ZFCD
                                       ZSBSEG-ZFIMDNO.
    ENDIF.
    IF ZSBSEG-MWSKZ IS INITIAL.
      MOVE ZTBKPF-MWSKZ   TO ZSBSEG-MWSKZ.
    ENDIF.
*     IF NOT ZSBSEG-ZFCD    IS INITIAL AND
*        NOT ZSBSEG-ZFIMDNO IS INITIAL AND
*        NOT ZSBSEG-WRBTR   IS INITIAL AND
*        NOT ZSBSEG-NEWKO   IS INITIAL.
*
*        MOVE : RF05A-AMPEL
*        text_080 = '오류 없음'.
*        PERFORM SET_OBJ_ICON  USING  'ICON_DISPLAY_MORE'
*                                      IT_ZSBSEG-STATE text_080.
*        MOVE IT_ZSBSEG-STATE TO ZSBSEG-STATE.
*     ELSE.
*        text_080 = '미입력필드 존재'.
*        PERFORM SET_OBJ_ICON  USING  'ICON_ENTER_MORE'
*                                      ZSBSEG-STATE text_080.
*     ENDIF.
  ELSE.
    IF NOT ZSBSEG-ZFIMDNO IS INITIAL AND
       NOT ZSBSEG-ZFCD    IS INITIAL.
      PERFORM  P2000_SET_NEWKO  USING ZSBSEG-NEWKO
                                      ZSBSEG-ZFCD
                                      ZTBKPF-ZFIMDNO.
    ENDIF.

    MOVE: ZTBKPF-WWERT   TO ZSBSEG-WWERT,
          ZTBKPF-KURSF   TO ZSBSEG-KURSF,
          '40'           TO ZSBSEG-NEWBS,
          'S'            TO ZSBSEG-SHKZG,
          0              TO ZSBSEG-DMBTR,
          ZTBKPF-ZFPOYN  TO ZSBSEG-ZFPOYN,       ">유환무환여부.
          ZTBKPF-MWSKZ   TO ZSBSEG-MWSKZ,
          ZTBKPF-ZFIMDNO TO ZSBSEG-ZFIMDNO,
          W_ZFDCNM       TO ZSBSEG-ZFDCNM.
    IF ZTBKPF-ZFCSTGRP EQ '008'.
      MOVE '001'        TO ZSBSEG-ZFCD.
    ENDIF.
    IF NOT ZSBSEG-ZFCD IS INITIAL.
      READ TABLE IT_ZSIMIMG08
              WITH KEY  ZFCDTY   =   ZTBKPF-ZFCSTGRP
                        ZFCD     =   ZSBSEG-ZFCD.
      IF SY-SUBRC EQ 0.
        MOVE: IT_ZSIMIMG08-ZFCDNM    TO ZSBSEG-ZFCDNM,
              IT_ZSIMIMG08-COND_TYPE TO ZSBSEG-COND_TYPE.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0100_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0014_STATUS_SCR0014 OUTPUT.
  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
    WHEN 'POPU'.
      SET TITLEBAR 'POPU' WITH 'Result Message'.
    WHEN 'BLHELP'.
      SET TITLEBAR 'POPU' WITH 'Multi-Referring  LIST'.
    WHEN 'TAXBKPO'.
      SET TITLEBAR 'POPU' WITH 'Duplicate CTM LIST'.
    WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUPPRESS_DIALOG_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SUPPRESS_DIALOG_SCR0110 OUTPUT.

  SUPPRESS DIALOG.

ENDMODULE.                 " SET_SUPPRESS_DIALOG_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_BUKRS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_BUKRS OUTPUT.
  CLEAR : ZTIMIMG00, P_BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
    MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
  ENDIF.

*>> 회사코드 SET.
  MOVE  P_BUKRS      TO ZTBKPF-BUKRS.

ENDMODULE.                 " SET_BUKRS  OUTPUT
