*----------------------------------------------------------------------*
*   INCLUDE ZRIM10O01                                                  *
*----------------------------------------------------------------------*
*&  Program Name : Import Customs Clearance                            *
*&  Created by   : Na Hyun Joo                                         *
*&  Created on   : 2003.10.08                                          *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  IMG_CHECK  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IMG_CHECK OUTPUT.

  SELECT  SINGLE *  FROM  ZTIMIMG00.
  IF ZTIMIMG00-ZFIMPATH EQ '3' AND SY-TCODE EQ 'ZIMCD2'.
     MESSAGE  S564. LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                 " IMG_CHECK  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PF_STATUS_SCRCOM OUTPUT.

  REFRESH : IT_EXCL.

*-----------------------------------------------------------------------
* GUI TITLE TEXT SETTING
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_GUI_TEXT.

*-----------------------------------------------------------------------
* PF-STATUS Setting
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_PF_STATUS.

*-----------------------------------------------------------------------
* Inactive Function Setting ( TRANSACTION 별 )
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_STATUS_TCODE_DISABLE.

*-----------------------------------------------------------------------
* GUI TITLE SETTING( SCREEN )
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_STATUS_SCR_DISABLE.

  IF ZTBKPF-ZFPOSYN EQ 'N'.
     MOVE 'CSDL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
     MOVE 'PSCL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
  ELSE.
     MOVE 'POST' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
  ENDIF.

  DESCRIBE TABLE IT_ZSIMCOST LINES W_LINE.
  IF W_LINE EQ 0.
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
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.

  MOVE OK-CODE TO W_OK_CODE.
  CLEAR : OK-CODE.

ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCRCOM OUTPUT.

   PERFORM P2000_SCR_MODE_SET.

ENDMODULE.                 " OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR0101 OUTPUT.

  IF PROGRAM IS INITIAL.  PROGRAM = SY-CPROG.  ENDIF.

  IF DYNPRO IS INITIAL.
    CASE SY-TCODE.
       WHEN 'ZIMCD2' OR 'ZIMCD3'.
          DYNPRO  =  '0102'.
       WHEN 'ZIMCC1' OR 'ZIMCC2' OR 'ZIMCC3'.
          DYNPRO  =  '1102'.
       WHEN 'ZIMT1'  OR 'ZIMT2'  OR 'ZIMT3'.
          DYNPRO  =  '2102'.
    ENDCASE.
  ENDIF.

  IF SY-TCODE EQ 'ZIMCD2' OR SY-TCODE EQ 'ZIMCD3' OR
     SY-TCODE EQ 'ZIMCC1' OR SY-TCODE EQ 'ZIMCC2' OR
     SY-TCODE EQ 'ZIMCC3' OR SY-TCODE EQ 'ZIMT1'  OR
     SY-TCODE EQ 'ZIMT2'  OR SY-TCODE EQ 'ZIMT3'.

     IF TABSTRIP-ACTIVETAB EQ 'ITM1' OR TABSTRIP-ACTIVETAB EQ 'ITM2'.
        IF OK-CODE(3) NE 'ITM'.
           OK-CODE = TABSTRIP-ACTIVETAB.
        ENDIF.
     ENDIF.

  ENDIF.

  IF W_STATUS EQ 'D'.
     IF OK-CODE EQ 'ENTR'.
        CASE TABSTRIP-ACTIVETAB.
           WHEN 'ITM1'.
             OK-CODE = 'ITM2'.
           WHEN 'ITM2'.
             OK-CODE = 'ITM'.
           WHEN OTHERS.
             OK-CODE = 'ITM1'.
        ENDCASE.
     ENDIF.
  ENDIF.

  IF OK-CODE IS INITIAL.   OK-CODE = 'ITM1'.  ENDIF.
  IF TABSTRIP-ACTIVETAB IS INITIAL. TABSTRIP-ACTIVETAB = 'ITM1'. ENDIF.

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
           WHEN '1101'.
              DYNPRO = '1102'.
              SET TITLEBAR  'BLST'  WITH <FS_F> <FS_DOC> 'General'.
           WHEN '2101'.
              DYNPRO = '2102'.
              SET TITLEBAR  'TRST'  WITH <FS_F> <FS_DOC> 'General'.
        ENDCASE.
     WHEN 'ITM2'.
        CASE SY-DYNNR.
           WHEN '0101'.
              DYNPRO = '0103'.
              SET TITLEBAR 'BLST' WITH <FS_F> <FS_DOC> 'HS Code'.
           WHEN '1101'.
              DYNPRO = '1103'.
              SET TITLEBAR  'BLST' WITH <FS_F> <FS_DOC> 'HS Code'.
           WHEN '2101'.
              DYNPRO = '2103'.
              SET TITLEBAR  'TRST' WITH <FS_F> <FS_DOC> 'Items'.
           WHEN OTHERS.
       ENDCASE.
  ENDCASE.

  TABSTRIP-ACTIVETAB = OK-CODE.
  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0103 OUTPUT.

  " HS DATA
  DESCRIBE TABLE IT_ZSIDRUSH LINES LINE.
  TC_0103_1-LINES = LINE.

  " HS DETAIL DATA
  DESCRIBE TABLE IT_ZSIDRUSD_SEL LINES LINE2.
  TC_0103_2-LINES = LINE2.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_HS_TO_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_HS_TO_SCR0103 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT

*Internal Table Read ( Line별 )
  READ TABLE IT_ZSIDRUSH  INDEX TC_0103_1-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING IT_ZSIDRUSH  TO ZSIDRUSH.     " DATA MOVE
  ENDIF.

ENDMODULE.                 " IT_HS_TO_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_HSDE_TO_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_HSDE_TO_SCR0103 OUTPUT.

  W_LOOPLINES2 = SY-LOOPC.                             " LOOPING COUNT

*Internal Table Read ( Line별 )
  READ TABLE IT_ZSIDRUSD_SEL  INDEX TC_0103_2-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING IT_ZSIDRUSD_SEL  TO ZSIDRUSD.     " DATA MOVE
  ENDIF.

ENDMODULE.                 " IT_HSDE_TO_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1103 OUTPUT.

  " HS DATA
  DESCRIBE TABLE IT_ZSIDSUSH LINES LINE.
  TC_1103_1-LINES = LINE.

  " HS DETAIL DATA
  DESCRIBE TABLE IT_ZSIDSUSD_SEL LINES LINE2.
  TC_1103_2-LINES = LINE2.

ENDMODULE.                 " TOTAL_LINE_GET_SCR1103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_HS_TO_SCR1103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_HS_TO_SCR1103 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT

*Internal Table Read ( Line별 )
  READ TABLE IT_ZSIDSUSH  INDEX TC_1103_1-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING IT_ZSIDSUSH  TO ZSIDSUSH.     " DATA MOVE
  ENDIF.

ENDMODULE.                 " IT_HS_TO_SCR1103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_HSDE_TO_SCR1103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_HSDE_TO_SCR1103 OUTPUT.

  W_LOOPLINES2 = SY-LOOPC.                             " LOOPING COUNT

*Internal Table Read ( Line별 )
  READ TABLE IT_ZSIDSUSD_SEL  INDEX TC_1103_2-CURRENT_LINE.
  IF SY-SUBRC = 0.                                       " READ SUCCESS?
     MOVE-CORRESPONDING IT_ZSIDSUSD_SEL  TO ZSIDSUSD.    " DATA MOVE
  ENDIF.

ENDMODULE.                 " IT_HSDE_TO_SCR1103  OUTPUT
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
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0001 OUTPUT.

  W_SY_SUBRC = SY-SUBRC.

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
*&      Module  SET_SUPPRESS_DIALOG_SCR0050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SUPPRESS_DIALOG_SCR0050 OUTPUT.
  SUPPRESS DIALOG.
ENDMODULE.                 " SET_SUPPRESS_DIALOG_SCR0050  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
MODULE D0014_STATUS_SCR0014 OUTPUT.

  IF W_STATUS_CHK = 'D'.
*    SET PF-STATUS 'STDLISA'.
  ELSE.
    SET PF-STATUS 'STDLISW'.
  ENDIF.

  CASE INCLUDE.
    WHEN 'POPU'.
      SET TITLEBAR 'POPU' WITH 'Message status'.
    WHEN 'TRCHANGE' OR 'TRDISPLY'.
      SET TITLEBAR 'POPU' WITH 'The list of delivery order duplication'.
    WHEN OTHERS.
      SET TITLEBAR 'POPU' WITH 'Partial referance LIST'.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR2103 OUTPUT.

  " Container DATA
  DESCRIBE TABLE IT_DOHD LINES LINE.
  TC_2103_1-LINES = LINE.

  " Container Detail(Case) Data
  DESCRIBE TABLE IT_DOIT_SEL LINES LINE2.
  TC_2103_2-LINES = LINE2.

ENDMODULE.                 " TOTAL_LINE_GET_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_DOHD_TO_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_DOHD_TO_SCR2103 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT

*Internal Table Read ( Line )
  READ TABLE IT_DOHD  INDEX TC_2103_1-CURRENT_LINE.
  IF SY-SUBRC = 0.                                    " READ SUCCESS?
     MOVE-CORRESPONDING IT_DOHD  TO ZSDOHD.           " DATA MOVE
     MOVE IT_DOHD-ZFMARK  TO  W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_DOHD_TO_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_DOIT_TO_SCR2103  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_DOIT_TO_SCR2103 OUTPUT.

  W_LOOPLINES2 = SY-LOOPC.                             " LOOPING COUNT

*Internal Table Read ( Line )
  READ TABLE IT_DOIT_SEL  INDEX TC_2103_2-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
     MOVE-CORRESPONDING IT_DOIT_SEL  TO ZSDOIT.        " DATA MOVE
     MOVE  IT_DOIT_SEL-ZFMARK    TO  W_ROW_MARK1.
  ENDIF.

ENDMODULE.                 " IT_DOIT_TO_SCR2103  OUTPUT
