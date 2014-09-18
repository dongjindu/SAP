*&---------------------------------------------------------------------*
*& INCLUDE MZIM04I01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : L/C 원장관리 Main PAI MODULE Include.
*&      작성자 : 나신호 INFOLINK Ltd.                                  *
*&      작성일 : 2002.11.25                                            *
*&  적용회사PJT: Poong-San                                             *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&   DESC : 공통 모듈( CANCEL일 경우, PROGRAM 종료 모듈 )
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.

  IF W_STATUS EQ 'D'.
    SET SCREEN 0.   LEAVE SCREEN.
  ELSE.
    IF SY-DYNNR EQ '0100'.
      SET SCREEN 0.   LEAVE SCREEN.
    ELSE.
      PERFORM P2000_SET_MESSAGE USING  SY-UCOMM.
    ENDIF.
  ENDIF.

ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE READ_DOC_SCR0100 INPUT.
  PERFORM  P2000_OK_CODE_PROCESS.
 IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
    MESSAGE E066.
  ENDIF.

  IF NOT ZSREQHD-ZFAMDNO IS INITIAL.
    SELECT SINGLE *
             FROM ZVREQHD_ST
            WHERE ZFREQNO = ZSREQHD-ZFREQNO
              AND ZFAMDNO = ZSREQHD-ZFAMDNO.
    IF SY-SUBRC NE 0. MESSAGE E009. ENDIF.
  ELSE.
    SELECT SINGLE *
             FROM ZVREQHD_ST
            WHERE ZFREQNO = ZSREQHD-ZFREQNO.
  ENDIF.

  SELECT * INTO TABLE IT_ZSBSEG
           FROM ZTBSEG
          WHERE ZFIMDNO  = ZSREQHD-ZFREQNO
            AND ZFCSTGRP = '003'
            AND ZFCD    NE '04'
            AND ZFCD    NE '13'
            AND ZFCD    NE '14'.

  SELECT * INTO TABLE IT_ZSBSEG1
           FROM ZTBSEG
          WHERE ZFIMDNO = ZSREQHD-ZFREQNO
            and zfcstgrp = '003'
            AND ZFCD    = '04'.

  SELECT * INTO TABLE IT_ZSPMTHD
           FROM ZTPMTHD
          WHERE ZFREQNO = ZSREQHD-ZFREQNO.

  SELECT * INTO TABLE IT_ZSIMIMG08
           FROM ZTIMIMG08
          WHERE ZFCDTY = '003'.

* Beneficiary Name Get.
  IF NOT ZVREQHD_ST-ZFBENI IS INITIAL.
    CLEAR: LFA1.
    SELECT SINGLE *
             FROM LFA1
            WHERE LIFNR = ZVREQHD_ST-ZFBENI.
    MOVE LFA1-NAME1 TO W_ZFBENI_NM.
  ENDIF.

* Offer Name Get.
  IF NOT ZVREQHD_ST-LLIEF IS INITIAL.
    CLEAR: LFA1.
    SELECT SINGLE *
             FROM LFA1
            WHERE LIFNR = ZVREQHD_ST-LLIEF.
    MOVE LFA1-NAME1 TO W_LLIEF_NM.
  ENDIF.

* Open Bank Name Get.
  IF NOT ZVREQHD_ST-ZFOPBN IS INITIAL.
    CLEAR: LFA1.
    SELECT SINGLE *
             FROM LFA1
            WHERE LIFNR = ZVREQHD_ST-ZFOPBN.
    MOVE LFA1-NAME1 TO W_OPEN_NM.
  ENDIF.

ENDMODULE.                 " READ_DOC_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0101  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0101 INPUT.
  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_SCR0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0100 INPUT.

  MOVE C_REQ_D TO W_STATUS.
  SET SCREEN 0101.  LEAVE SCREEN.
ENDMODULE.                 " USER_COMMAND_SCR0100  INPUT
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
      ANTWORT = 'Y'.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
