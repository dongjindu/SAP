*----------------------------------------------------------------------*
*  INCLUDE ZRIMRPTCOM .
*----------------------------------------------------------------------*
*&  프로그램명 : Report Common Include Program
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.10.11                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* 비용관련 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA:    BEGIN OF IT_ZTIMIMG08 OCCURS 0.
         INCLUDE STRUCTURE ZTIMIMG08.
DATA     END OF IT_ZTIMIMG08.

DATA : W_POPOP_CHK    VALUE   'N',
       ANTWORT,
       W_MOD          TYPE     I,
       W_ZFCD3        LIKE     ZTIMIMG08-ZFCD3,
       W_STATUS_CHK.

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_PAY_TERM
*&---------------------------------------------------------------------*
FORM P1000_GET_PAY_TERM USING    P_ZTERM.
TABLES : T052.

    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = P_ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.

   IF SY-SUBRC NE 0.
      MESSAGE S177(06) WITH P_ZTERM.
      EXIT.
   ENDIF.

   IF T052-ZTERM NE SPACE.
      P_ZTERM = T052-ZTERM.
  ENDIF.

ENDFORM.                    " P1000_GET_PAY_TERM

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_SUMMARY_CODE
*&---------------------------------------------------------------------*
FORM P1000_GET_SUMMARY_CODE USING    P_CODE.

   REFRESH : IT_ZTIMIMG08.
   SELECT ZFCD3  ZFCD4
            INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG08
            FROM ZTIMIMG08
            WHERE ZFCDTY  EQ  '003'
            AND   ZFCD3   NE   SPACE
            GROUP BY ZFCD3 ZFCD4.

   DESCRIBE TABLE IT_ZTIMIMG08 LINES W_LINE.
   CHECK W_LINE NE 0.

   ANTWORT = 'N'.
   W_POPOP_CHK =  'Y'.
   CALL SCREEN 0020 STARTING AT  10 5
                    ENDING   AT  35 15.
   W_POPOP_CHK =  'N'.
   CHECK ANTWORT EQ 'Y'.

   P_CODE = W_ZFCD3.

ENDFORM.                    " P1000_GET_SUMMARY_CODE

*&---------------------------------------------------------------------*
*&      Module  STATUS_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*MODULE STATUS_SCR0020 OUTPUT.
*
*  IF W_STATUS_CHK = 'D'.
*     SET PF-STATUS 'STDLISA'.
*  ELSE.
*     SET PF-STATUS 'STDLISW'.
*  ENDIF.
*
*  SET TITLEBAR 'POPU' WITH '집계구분 코드 HELP'.
*
*  SUPPRESS DIALOG.
*
*ENDMODULE.                 " STATUS_SCR0020  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  LIST_CHECK_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LIST_CHECK_SCR0020 INPUT.
* list-processing
  LEAVE TO LIST-PROCESSING.
* list Write
  LOOP AT IT_ZTIMIMG08.
     AT FIRST.
        FORMAT COLOR COL_HEADING INTENSIFIED OFF.
        WRITE : / SY-ULINE(20).
        WRITE : / SY-VLINE, 'Cod', SY-VLINE, '   Desc.  ', SY-VLINE.
        WRITE : / SY-ULINE(20).
     ENDAT.
     W_MOD = SY-TABIX MOD 2.
     WRITE : / SY-VLINE,
               IT_ZTIMIMG08-ZFCD3 COLOR COL_KEY INTENSIFIED,
            SY-VLINE.
     IF W_MOD EQ 0.
        FORMAT COLOR COL_NORMAL INTENSIFIED ON.
     ELSE.
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
     ENDIF.
     WRITE : IT_ZTIMIMG08-ZFCD4, SY-VLINE.
     HIDE IT_ZTIMIMG08.
     AT LAST.
        WRITE : / SY-ULINE(20).
     ENDAT.
  ENDLOOP.
  CLEAR : IT_ZTIMIMG08.
ENDMODULE.                 " LIST_CHECK_SCR0020  INPUT
