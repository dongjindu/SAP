*&---------------------------------------------------------------------*
*& Report ZRIMPOCRT                                                    *
*&---------------------------------------------------------------------*
*&  프로그램명 : P/O Due List                                          *
*&      작성자 : 나신호 INFOLINK Ltd.                                  *
*&      작성일 : 2001.01.09                                            *
*&---------------------------------------------------------------------*
*&   DESC. : 1. 나신호의 세번째 레포트 프로그램 씨익~ .
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

REPORT  ZRIMPOCRT NO STANDARD PAGE HEADING MESSAGE-ID ZIM
                  LINE-SIZE 126.

TABLES: EKKO,                " ABAP Standard Header Table..
        ZTREQHD,             " 수입의뢰 Header Table..
        ZTIMIMG01.           " Payment Term Configuration.

DATA: BEGIN OF IT_IMG01 OCCURS 1000,
         ZTERM     LIKE   ZTIMIMG01-ZTERM,
         ZFREQTY   LIKE   ZTIMIMG01-ZFREQTY,
      END OF IT_IMG01.

DATA: BEGIN OF IT_PO OCCURS 1000.
      INCLUDE STRUCTURE EKKO.
DATA  END OF IT_PO.

DATA: TAB_INDEX    TYPE   I,
      TEMP         TYPE   F,
      W_ERR_CHK    TYPE   C   VALUE 'N'.
      TAB_INDEX = 0.

*-----------------------------------------------------------------------
* 검색조건 Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_ZTERM   FOR ZTIMIMG01-ZTERM,       " 지급조건키.
               S_REQTY   FOR ZTIMIMG01-ZFREQTY.     " 수입의뢰 Type.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
               S_EBELN   FOR EKKO-EBELN,    " P/O No.
               S_EKORG   FOR EKKO-EKORG,    " Purch. Org.
               S_EKGRP   FOR EKKO-EKGRP,    " Purch. Grp.
               S_LIFNR   FOR ZTREQHD-LIFNR.    " Vendor.
SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
INITIALIZATION.
   SET TITLEBAR 'TIT1'.

TOP-OF-PAGE.

   PERFORM P3000_TITLE_WRITE.             "헤더 출력...

*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
START-OF-SELECTION.
* Payment Term Configuration Select.
   PERFORM P1000_READ_ZTERM.
   CHECK W_ERR_CHK EQ 'N'.

   PERFORM P1000_READ_PO_DATA.
   CHECK W_ERR_CHK EQ 'N'.

   PERFORM P3000_BOTTOM_WRITE.

*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
END-OF-SELECTION.

*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'PODP'.
        IF NOT IT_PO-EBELN IS INITIAL.
            SET PARAMETER ID 'BES' FIELD IT_PO-EBELN.
*            SET PARAMETER ID 'BSP' FIELD IT_PO-EBELP.
            CALL TRANSACTION 'ME23' AND SKIP FIRST SCREEN.
         ENDIF.
      WHEN 'LCCR'.
        IF NOT IT_PO-EBELN IS INITIAL.
            SET PARAMETER ID 'BES' FIELD IT_PO-EBELN.
            CALL TRANSACTION 'ZIM01' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'PAYT'.
            SET PARAMETER ID 'DFD' FIELD IT_PO-ZTERM.
            CALL TRANSACTION 'ZIMG01' AND SKIP FIRST SCREEN.
   ENDCASE.
   CLEAR: IT_PO.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTERM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTERM.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_IMG01
            FROM ZTIMIMG01
           WHERE ZTERM     IN   S_ZTERM
             AND ZFREQTY   IN   S_REQTY.
      IF SY-SUBRC NE 0.
         W_ERR_CHK = 'Y'.
         MESSAGE S353.
         EXIT.
      ENDIF.

ENDFORM.                    " P1000_READ_ZTERM

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_PO_DATA.


SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_PO
         FROM EKKO FOR ALL ENTRIES IN IT_IMG01
        WHERE ZTERM          EQ   IT_IMG01-ZTERM
        AND   AUTLF          EQ   ''
        AND   LOEKZ          EQ   ''
        AND   BSTYP          EQ   'F'
        AND   ZTERM          IN   S_ZTERM
        AND   EBELN          IN   S_EBELN              " P/O No.
        AND   EKORG          IN   S_EKORG              " Purch. Org.
        AND   EKGRP          IN   S_EKGRP              " Purch. Grp.
        AND   LIFNR          IN   S_LIFNR              " Vendor.
        AND   NOT EBELN      IN
                 ( SELECT EBELN FROM ZTREQHD ).

   IF SY-SUBRC EQ 0.
      SET TITLEBAR 'TIT1'.
      SET PF-STATUS 'ZIM01N'.

      LOOP AT IT_PO.
      READ TABLE IT_IMG01 WITH KEY   ZTERM = IT_PO-ZTERM.
         TEMP = TAB_INDEX MOD 2.
         IF TEMP EQ 0.
            FORMAT COLOR COL_NORMAL INTENSIFIED ON.
         ELSE.
            FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
         ENDIF.
            WRITE: / SY-VLINE, IT_PO-EBELN, SY-VLINE, IT_IMG01-ZFREQTY,
                  22 SY-VLINE, IT_PO-LIFNR, 35 SY-VLINE, IT_PO-WAERS,
                  46 SY-VLINE, IT_PO-ZTERM, 61 SY-VLINE, IT_PO-BEDAT,
                     SY-VLINE, IT_PO-EKORG, 85 SY-VLINE, IT_PO-EKGRP,
                  96 SY-VLINE, IT_PO-ERNAM, SY-VLINE, IT_PO-BSART,
                 126 SY-VLINE.

            FORMAT RESET.

         HIDE: IT_PO.
         CLEAR: IT_PO.
      TAB_INDEX = TAB_INDEX + 1.
      ENDLOOP.
   ELSE.
      EXIT.
   ENDIF.
ENDFORM.                    " P1000_READ_PO_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

   SKIP 2.
   FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
   WRITE: /55 '[ P/O Due List ]'
              COLOR COL_HEADING INTENSIFIED OFF.

   WRITE: / 'Date: ' COLOR COL_NORMAL INTENSIFIED ON,
             SY-DATUM COLOR COL_NORMAL INTENSIFIED OFF.
   WRITE: / SY-ULINE.
   FORMAT COLOR COL_HEADING INTENSIFIED ON.
   WRITE: / SY-VLINE, ' P/O No.  ',   SY-VLINE, 'Type.',
            SY-VLINE, '거래처    ',   SY-VLINE, '통화단위',
            SY-VLINE, 'Payment Term', SY-VLINE, ' P/O Date ',
            SY-VLINE, '구매조직',     SY-VLINE, '구매그룹',
            SY-VLINE, '담당자      ', SY-VLINE, '구매문서범주',
            SY-VLINE, SY-ULINE.
   FORMAT RESET.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_BOTTOM_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_BOTTOM_WRITE.
ULINE.
WRITE: 108 '총:', TAB_INDEX, '건'.

ENDFORM.                    " P3000_BOTTOM_WRITE
