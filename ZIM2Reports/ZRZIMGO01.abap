*&---------------------------------------------------------------------*
*& INCLUDE ZRZIMGO01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입시스템 Configuration 관리 PBO Module Inculde      *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.01.26                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCRCOM OUTPUT.

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

      IF SY-DYNNR EQ '9301' OR SY-DYNNR EQ '9500'.
         IF NOT ( ZTIMIMG08-ZFCDTY EQ '003' OR      " L/C COST
                  ZTIMIMG08-ZFCDTY EQ '004' OR      " B/L Freight COST
                  ZTIMIMG08-ZFCDTY EQ '005' OR      " B/L Bonded COST
                  ZTIMIMG08-ZFCDTY EQ '006' OR      " Customs Clearance
                  ZTIMIMG08-ZFCDTY EQ '007' OR      " Cargo
                  ZTIMIMG08-ZFCDTY EQ '009' ).      " Transportation Fe.

            IF SCREEN-NAME  EQ 'ZSIMIMG08-COND_TYPE'.
               SCREEN-INVISIBLE = '1'.
            ENDIF.
            IF SCREEN-NAME  EQ 'ZSIMIMG08-KTOSL'.
               SCREEN-INVISIBLE = '1'.
            ENDIF.
            IF SCREEN-NAME  EQ 'ZSIMIMG08-BLART'.
               SCREEN-INPUT = '0'.
            ENDIF.
         ELSE.
            IF SCREEN-NAME  EQ 'ZSIMIMG08-COND_TYPE'.
               SCREEN-INVISIBLE = '0'.
            ENDIF.
            IF SCREEN-NAME  EQ 'ZSIMIMG08-KTOSL'.
               SCREEN-INVISIBLE = '0'.
            ENDIF.
            IF SCREEN-NAME  EQ 'ZSIMIMG08-BLART' AND W_STATUS NE 'D'.
               SCREEN-INPUT = '1'.
            ENDIF.
         ENDIF.

         IF NOT ( ZTIMIMG08-ZFCDTY EQ '801' OR
                  ZTIMIMG08-ZFCDTY EQ '809' OR
                  ZTIMIMG08-ZFCDTY EQ '006' ).
            IF SCREEN-NAME  EQ 'ZSIMIMG08-ZFIOCAC'.
               SCREEN-INVISIBLE = '1'.
               SCREEN-INPUT     = '0'.
            ENDIF.
         ENDIF.
      ENDIF.

*>> Phanta Vendor Check.
      IF SY-DYNNR EQ '0500' AND ZTIMIMG00-ZFCSTMD NE 'I'.
         IF SCREEN-NAME EQ 'ZTIMIMG11-ZFPVEN1' OR
            SCREEN-NAME EQ 'W_ZFPVEN1_NM'      OR
            SCREEN-NAME EQ 'ZTIMIMG11-ZFPVEN2' OR
            SCREEN-NAME EQ 'W_ZFPVEN2_NM'      OR
            SCREEN-NAME EQ 'ZTIMIMG11-ZFPVEN3' OR
            SCREEN-NAME EQ 'W_ZFPVEN3_NM'      OR
            SCREEN-NAME EQ 'ZTIMIMG11-ZFPVEN4' OR
            SCREEN-NAME EQ 'W_ZFPVEN4_NM'      OR
            SCREEN-NAME EQ 'ZTIMIMG11-ZFPVEN5' OR
            SCREEN-NAME EQ 'W_ZFPVEN5_NM'      OR
            SCREEN-NAME EQ 'ZTIMIMG11-ZFPVEN6' OR
            SCREEN-NAME EQ 'W_ZFPVEN6_NM'       OR
            SCREEN-NAME EQ 'ZTIMIMG11-ZFPVEN7' OR
            SCREEN-NAME EQ 'W_ZFPVEN7_NM'      OR
            SCREEN-NAME EQ 'BLOCK0'.
            SCREEN-INPUT     = '0'.
            SCREEN-INVISIBLE = '1'.
         ENDIF.
      ENDIF.

      IF SY-DYNNR EQ '0500' AND ZTIMIMG00-ZFMSYN NE 'X'.
         IF SCREEN-NAME EQ 'ZTIMIMG11-ZFMSCST3' OR
            SCREEN-NAME EQ 'ZTIMIMG11-ZFMSCST4' OR
            SCREEN-NAME EQ 'BLOCK10'.
            SCREEN-INPUT     = '0'.
            SCREEN-INVISIBLE = '1'.
         ENDIF.
      ENDIF.
      IF SY-DYNNR EQ '0200'.
         IF SCREEN-NAME EQ 'W_PASSWT' AND
            W_STATUS EQ 'D'.
            SCREEN-INVISIBLE = '1'.
         ENDIF.
      ENDIF.
      MODIFY SCREEN.
   ENDLOOP.

ENDMODULE.                 " OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0100 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_0100-TOP_LINE.
    TC_0100-LINES = G_PARAM_LINE + 17.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG01 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0100-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE MARK_SET_SCR0100 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_0100-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR0100 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0100-CURRENT_LINE GT TC_0100-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG01   INDEX TC_0100-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG01 TO  ZSIMIMG01.
     MOVE : IT_ZSIMIMG01-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.

   MOVE OK-CODE TO W_OK_CODE.
   CLEAR : OK-CODE.
   CLEAR : POSI_GB.

ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR0100 OUTPUT.

 IF W_FIRST_FLAG_0100 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG01 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG01.

    MOVE 'N' TO W_FIRST_FLAG_0100.
    MOVE 'D' TO W_STATUS.
    TC_0100-TOP_LINE = 1.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE PF_STATUS_SCRCOM OUTPUT.

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
      WHEN 'ZIMG01' OR 'ZIMG02' OR 'ZIMG03' OR 'ZIMG04' OR
           'ZIMG05' OR 'ZIMG10' OR
           'ZIMG11' OR 'ZIMG12' OR 'ZIMG14' OR 'ZIMG15' OR
           'ZIMG16' OR 'ZIMG17' OR
           'ZIMGC1' OR 'ZIMGC2' OR 'ZIMGC3' OR 'ZIMGC4' OR
           'ZEXZ23' OR
           'ZIMG25' OR
           'ZIMGA1' OR 'ZIMG06' OR 'ZIMG21' OR 'ZIMG22' OR
           'ZIMG08' OR 'ZIMG20' OR 'ZIMG24' OR 'ZIMG24N'.
         MOVE 'EULG' TO W_PFSTAT.
      WHEN 'ZIMGM'.
         MOVE 'EULG' TO W_PFSTAT.
      WHEN OTHERS.
         EXIT.
   ENDCASE.

*-----------------------------------------------------------------------
* PF-STATUS Inactive Function Setting
*-----------------------------------------------------------------------
   CASE SY-TCODE.
      WHEN 'ZIMG01' OR 'ZIMG02' OR 'ZIMG03' OR 'ZIMG04' OR
           'ZIMG05' OR 'ZIMG10' OR 'ZIMG11' OR 'ZIMG12' OR
           'ZIMG14' OR 'ZIMG15' OR 'ZIMG16' OR 'ZIMG17' OR
           'ZIMGC1' OR 'ZIMGC2' OR 'ZIMGC4' OR
           'ZEXZ23' OR
           'ZIMG25' OR
           'ZIMG06' OR 'ZIMG21' OR 'ZIMG22' OR
           'ZIMG08' OR 'ZIMG20' OR 'ZIMG24' OR 'ZIMG24N'.
* Disable Menu Status Setting
         PERFORM  P2000_SET_DISABLE_MENU.
* GUI TITLE SETTING
         PERFORM  P2000_SET_GUI_TITLE.
      WHEN 'ZIMGC3'.
         IF SY-DYNNR EQ '9300'.
            MOVE 'DETA' TO IT_EXCL-FCODE. APPEND IT_EXCL.
            MOVE 'NEWL' TO IT_EXCL-FCODE. APPEND IT_EXCL.
            MOVE 'CHDC' TO IT_EXCL-FCODE. APPEND IT_EXCL.
            MOVE 'DELT' TO IT_EXCL-FCODE. APPEND IT_EXCL.
            MOVE 'COPY' TO IT_EXCL-FCODE. APPEND IT_EXCL.
            SET PF-STATUS 'EUL1' EXCLUDING IT_EXCL.
            SET TITLEBAR  '9300'.
         ELSEIF SY-DYNNR EQ '9301'.
* Disable Menu Status Setting
            PERFORM  P2000_SET_DISABLE_MENU.
            SET TITLEBAR '9301' WITH <FS_F>.
         ENDIF.
      WHEN 'ZIMGM'.
         SET PF-STATUS 'EUL1'.
         SET TITLEBAR  '0010'.
      WHEN 'ZIMGA1'.
         IF SY-DYNNR EQ '9400'.
            SET PF-STATUS 'EUL1'.
            SET TITLEBAR  '9400'.
         ELSEIF SY-DYNNR EQ '9401'.
* Disable Menu Status Setting
            PERFORM  P2000_SET_DISABLE_MENU.
            SET TITLEBAR '9401'.
         ENDIF.
      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " PF_STATUS_SCRCOM  OUTPUT
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
*&      Module  GET_DATA_SCR0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR0200 OUTPUT.

 IF W_FIRST_FLAG_0200 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG00 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMGTX.
    MOVE 'N' TO W_FIRST_FLAG_0200.
    MOVE 'D' TO W_STATUS.

 ENDIF.

ENDMODULE.                 " GET_DATA_SCR0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR1200  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR1200 OUTPUT.

 IF W_FIRST_FLAG_1200 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG04 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG04.

    MOVE 'N' TO W_FIRST_FLAG_1200.
    MOVE 'D' TO W_STATUS.
    TC_1200-TOP_LINE = 1.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR1200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1200  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1200 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_1200-TOP_LINE.
    TC_1200-LINES = G_PARAM_LINE + 18.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG04 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_1200-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR1200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR1200  OUTPUT
*&---------------------------------------------------------------------*
MODULE MARK_SET_SCR1200 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_1200-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR1200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR1200  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR1200 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_1200-CURRENT_LINE GT TC_1200-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG04 INDEX TC_1200-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG04  TO  ZSIMIMG04.
     MOVE : IT_ZSIMIMG04-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR1200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR1100 OUTPUT.

 IF W_FIRST_FLAG_1100 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG04 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG06.

    MOVE 'N' TO W_FIRST_FLAG_1100.
    MOVE 'D' TO W_STATUS.
    TC_1100-TOP_LINE = 1.
 ENDIF.


ENDMODULE.                 " GET_DATA_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1100 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_1100-TOP_LINE.
    TC_1100-LINES = G_PARAM_LINE + 18.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG06 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_1100-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
MODULE MARK_SET_SCR1100 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_1100-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR1100 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_1100-CURRENT_LINE GT TC_1100-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG06 INDEX TC_1100-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG06  TO  ZSIMIMG06.
     MOVE : IT_ZSIMIMG06-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR1400  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR1400 OUTPUT.

 IF W_FIRST_FLAG_1400 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG07 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG07.

    MOVE 'N' TO W_FIRST_FLAG_1400.
    MOVE 'D' TO W_STATUS.
    TC_1400-TOP_LINE = 1.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR1400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR2400  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR2400 OUTPUT.

 IF W_FIRST_FLAG_2400 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG07 TABLE SELECT
*-----------------------------------------------------------------------
   PERFORM  P1000_READ_ZTIMIMG24.
   MOVE 'N' TO W_FIRST_FLAG_2400.
   MOVE 'D' TO W_STATUS.
   TC_2400-TOP_LINE = 1.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR2400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1400  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1400 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_1400-TOP_LINE.
    TC_1400-LINES = G_PARAM_LINE + 18.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG07 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_1400-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR1400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR2400  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR2400 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_2400-TOP_LINE.
    TC_2400-LINES = G_PARAM_LINE + 20.
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG24 LINES G_PARAM_LINE.
    TC_2400-LINES = G_PARAM_LINE.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR2400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR1400  OUTPUT
*&---------------------------------------------------------------------*
MODULE MARK_SET_SCR1400 OUTPUT.
  IF OK-CODE = 'SELA'                  " mark all
     AND TC_1400-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR1400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR2400  OUTPUT
*&---------------------------------------------------------------------*
MODULE MARK_SET_SCR2400 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_2400-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR2400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR1400  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR1400 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE Expiry Check.
  IF TC_1400-CURRENT_LINE GT TC_1400-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line )
  READ TABLE IT_ZSIMIMG07 INDEX TC_1400-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    SELECT SINGLE *
             FROM ZTIMIMG10
            WHERE ZFCUT EQ IT_ZSIMIMG07-ZFCUT.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE *
               FROM LFA1
              WHERE LIFNR = ZTIMIMG10-ZFVEN.
      MOVE LFA1-NAME1 TO IT_ZSIMIMG07-NAME1.
    ENDIF.
    MOVE-CORRESPONDING  IT_ZSIMIMG07  TO  ZSIMIMG07.
    MOVE : IT_ZSIMIMG07-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR1400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR2400  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR2400 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* Verify Validation of Line.
  IF TC_2400-CURRENT_LINE GT TC_2400-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line )
  READ TABLE IT_ZSIMIMG24 INDEX TC_2400-CURRENT_LINE.
    MOVE-CORRESPONDING  IT_ZSIMIMG24  TO  ZSIMIMG24.
    MOVE : IT_ZSIMIMG24-ZFMARK        TO  W_ROW_MARK.  " MARK SET

ENDMODULE.                 " FILL_TC_SCR2400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR9301  OUTPUT
*&---------------------------------------------------------------------*
MODULE MARK_SET_SCR9301 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_9301-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR9301  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR9301  OUTPUT
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR9301 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_9301-CURRENT_LINE GT TC_9301-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG08 INDEX TC_9301-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG08  TO  ZSIMIMG08.
     MOVE : IT_ZSIMIMG08-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.
* Cursor Setting
  IF TC_9301-CURRENT_LINE = TOP_LINE.
     SET CURSOR FIELD ZSIMIMG08-ZFCDNM LINE TOP_LINE.
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR9301  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR9301  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR9301 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_9301-TOP_LINE.
    TC_9301-LINES = G_PARAM_LINE + 18.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG08 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_9301-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

* Top Line 정의
  IF POSI_GB = 'Y'.
     MOVE TOP_LINE TO TC_9301-TOP_LINE.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR9301  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR9100 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_9100-TOP_LINE.
    TC_9100-LINES = G_PARAM_LINE + 18.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG02 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_9100-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE MARK_SET_SCR9100 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_9100-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR9100 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_9100-CURRENT_LINE GT TC_9100-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG02 INDEX TC_9100-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG02  TO  ZSIMIMG02.
     MOVE : IT_ZSIMIMG02-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR9100 OUTPUT.

 IF W_FIRST_FLAG_9100 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG02 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG02.

    MOVE 'N' TO W_FIRST_FLAG_9100.
    MOVE 'D' TO W_STATUS.
    TC_9100-TOP_LINE = 1.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR9200  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR9200 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_9200-TOP_LINE.
    TC_9200-LINES = G_PARAM_LINE + 18.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG03 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_9200-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR9200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR9200  OUTPUT
*&---------------------------------------------------------------------*
MODULE MARK_SET_SCR9200 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_9200-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR9200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR9200  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR9200 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_9200-CURRENT_LINE GT TC_9200-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG03 INDEX TC_9200-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG03  TO  ZSIMIMG03.
     MOVE : IT_ZSIMIMG03-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR9200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR9200  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR9200 OUTPUT.

 IF W_FIRST_FLAG_9200 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG03 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG03.

    MOVE 'N' TO W_FIRST_FLAG_9200.
    MOVE 'D' TO W_STATUS.
    TC_9200-TOP_LINE = 1.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR9200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR1500  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR1500 OUTPUT.

 IF W_FIRST_FLAG_1500 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG05 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG05.

    MOVE 'N' TO W_FIRST_FLAG_1500.
    MOVE 'D' TO W_STATUS.
    TC_1500-TOP_LINE = 1.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR1500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1500  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1500 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_1500-TOP_LINE.
    TC_1500-LINES = G_PARAM_LINE + 18.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG05 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_1500-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR1500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR1500  OUTPUT
*&---------------------------------------------------------------------*
MODULE MARK_SET_SCR1500 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_1500-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR1500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR1500  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR1500 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_1500-CURRENT_LINE GT TC_1500-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG05 INDEX TC_1500-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG05  TO  ZSIMIMG05.
     MOVE : IT_ZSIMIMG05-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR1500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR1600  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR1600 OUTPUT.

 IF W_FIRST_FLAG_1600 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG05 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG09.

    MOVE 'N' TO W_FIRST_FLAG_1600.
    MOVE 'D' TO W_STATUS.
    TC_1600-TOP_LINE = 1.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR1600  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1600  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1600 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_1600-TOP_LINE.
    TC_1600-LINES = G_PARAM_LINE + 18.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG09 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_1600-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR1600  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR1600  OUTPUT
*&---------------------------------------------------------------------*
MODULE MARK_SET_SCR1600 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_1600-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR1600  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR1600  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR1600 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_1600-CURRENT_LINE GT TC_1600-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG09 INDEX TC_1600-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG09  TO  ZSIMIMG09.
     MOVE : IT_ZSIMIMG09-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR1600  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR1700  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR1700 OUTPUT.

 IF W_FIRST_FLAG_1700 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG17 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG17.

    MOVE 'N' TO W_FIRST_FLAG_1700.
    MOVE 'D' TO W_STATUS.
    TC_1700-TOP_LINE = 1.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR1700  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1700  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1700 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_1700-TOP_LINE.
    TC_1700-LINES = G_PARAM_LINE + 18.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG17 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_1700-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR1700  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR1700  OUTPUT
*&---------------------------------------------------------------------*
MODULE MARK_SET_SCR1700 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_1700-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR1700  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR1700  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR1700 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_1700-CURRENT_LINE GT TC_1700-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG17 INDEX TC_1700-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG17  TO  ZSIMIMG17.
     MOVE : IT_ZSIMIMG17-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR1700  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR1000  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR1000 OUTPUT.

 IF W_FIRST_FLAG_1000 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG02 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG10.

    MOVE 'N' TO W_FIRST_FLAG_1000.
    MOVE 'D' TO W_STATUS.
    TC_1000-TOP_LINE = 1.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR1000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1000  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1000 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_1000-TOP_LINE.
    TC_1000-LINES = G_PARAM_LINE + 18.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG10 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_1000-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR1000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR1000  OUTPUT
*&---------------------------------------------------------------------*
MODULE MARK_SET_SCR1000 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_1000-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR1000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR1000  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR1000 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_1000-CURRENT_LINE GT TC_1000-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG10 INDEX TC_1000-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG10  TO  ZSIMIMG10.
     MOVE : IT_ZSIMIMG10-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR1000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_INIT_SCR9400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_INIT_SCR9400 OUTPUT.

   IF ZSREQHD-ZFDATE IS INITIAL.
      ZSREQHD-ZFDATE  = SY-DATUM.
   ENDIF.
   IF ZSREQHD-ZFEXPDT IS INITIAL.
      ZSREQHD-ZFEXPDT = SY-DATUM + 7.
   ENDIF.
*>>> BDC MODE SETTING
   CLEAR : RADIO_BDCE, RADIO_BDCA, RADIO_BDCN.
   CASE DISP_MODE.
      WHEN 'N'.     RADIO_BDCN = 'X'.
      WHEN 'E'.     RADIO_BDCE = 'X'.
      WHEN 'A'.     RADIO_BDCA = 'X'.
      WHEN OTHERS.  RADIO_BDCN = 'X'.
   ENDCASE.

ENDMODULE.                 " SET_INIT_SCR9400  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR9401  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR9401 OUTPUT.

    DESCRIBE TABLE IT_CURR LINES G_PARAM_LINE.   " LINE 수 GET
    TC_9401-LINES = G_PARAM_LINE.                     " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR9401  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR9401  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MARK_SET_SCR9401 OUTPUT.

  IF OK-CODE = 'MKAL'                  " mark all
     AND TC_9401-CURRENT_LINE <= G_PARAM_LINE.
     ZSTCURR-ZFMARK1 = 'X'.
  ENDIF.
  IF OK-CODE = 'MKLO'.                " delete all marks
     CLEAR ZSTCURR-ZFMARK1.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR9401  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR9401  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR9401 OUTPUT.
  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_9401-CURRENT_LINE GT TC_9401-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_CURR INDEX TC_9401-CURRENT_LINE.
  IF SY-SUBRC EQ 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_CURR       TO  ZSTCURR.
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR9401  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR0500  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR0500 OUTPUT.

 IF W_FIRST_FLAG_0500 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG11 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG11.

    MOVE 'N' TO W_FIRST_FLAG_0500.
    MOVE 'D' TO W_STATUS.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR0500  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR0600  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_DATA_SCR0600 OUTPUT.

 IF W_FIRST_FLAG_0600 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG02 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG12.

    MOVE 'N' TO W_FIRST_FLAG_0600.
    MOVE 'D' TO W_STATUS.
    TC_0600-TOP_LINE = 1.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR0600  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0600  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0600 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_0600-TOP_LINE.
    TC_0600-LINES = G_PARAM_LINE + 18.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG12 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0600-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0600  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR0600  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MARK_SET_SCR0600 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_0600-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR0600  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR0600  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR0600 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0600-CURRENT_LINE GT TC_0600-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG12 INDEX TC_0600-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG12  TO  ZSIMIMG12.
     MOVE : IT_ZSIMIMG12-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR0600  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMGTX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMGTX.

* Clearing
   CLEAR: ZTIMIMGTX, W_BUTXT1, W_ZFCDNM, *ZTIMIMGTX.
* Table Single-Select
   SELECT SINGLE * FROM ZTIMIMGTX WHERE BUKRS EQ W_BUKRS.
   IF SY-SUBRC NE 0.
      MOVE  W_BUKRS  TO  ZTIMIMGTX-BUKRS.
   ENDIF.
   MOVE-CORRESPONDING  ZTIMIMGTX   TO    *ZTIMIMGTX.

*>>종합상사 회사코드 DISPLAY
   SELECT SINGLE BUTXT INTO W_BUTXT1 FROM T001
   WHERE  BUKRS  EQ    ZTIMIMGTX-ZFCORP.

*>>보험회사명 DISPLAY
   SELECT SINGLE ZFCDNM INTO W_ZFCDNM FROM ZTIMIMG08
   WHERE  ZFCDTY EQ     '010'
   AND    ZFCD   EQ     ZTIMIMGTX-ZFINSC.

ENDFORM.                    " P1000_READ_ZTIMIMGTX
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_DATA_SCR0300 OUTPUT.

 IF W_FIRST_FLAG_0300 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG00 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG00.
    MOVE 'N' TO W_FIRST_FLAG_0300.
    MOVE 'D' TO W_STATUS.
    IF ZTIMIMG00-ZFUSD IS INITIAL.
       MOVE: 'USD' TO ZTIMIMG00-ZFUSD.
    ENDIF.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TREE_CONTROL_CALL_SCR9500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TREE_CONTROL_CALL_SCR9500 OUTPUT.

  IF G_TREE IS INITIAL.
* The Tree Control has not been created yet.
* Create a Tree Control and insert nodes into it.
    PERFORM TREE_CREATE_AND_INIT USING SY-SUBRC.
    IF SY-SUBRC <> 0.
      MESSAGE S206 WITH 'Column Tree'.                      "#EC NOTEXT
    ENDIF.
  ENDIF.

ENDMODULE.                 " TREE_CONTROL_CALL_SCR9500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_COLUMN_VISIBLE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_COLUMN_VISIBLE OUTPUT.

   LOOP AT TC_9301-COLS INTO WA_COLS.
      CASE W_ZFCDTY.
*> 통합메뉴 유지보수.
         WHEN '000'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD5'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
*> 수입거래 구분.
         WHEN '001'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD2'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD4'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD5'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
         WHEN '002'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD1'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
*> 각종 비용코드.
         WHEN '003'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD2'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD4'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'  OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'   .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
         WHEN '004'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'   .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
         WHEN '005'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'  OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'   .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
         WHEN '006'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD2'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD4'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
            IF W_ZFCDTY EQ '006'.
               IF SY-TCODE EQ 'ZEXZ23'.
                  IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD2'     OR
                     WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'     OR
                     WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD4'     OR
                     WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
                     WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
                     WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE'   OR
                     WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
                     WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
                     WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
                     WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
                     WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
                     WA_COLS-INVISIBLE = 1.
                  ELSE.
                     CLEAR : WA_COLS-INVISIBLE.
                  ENDIF.
               ENDIF.
            ENDIF.
*> 손보사 코드.
         WHEN '010'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD1'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD2'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
         WHEN '012' OR '997' OR '999' OR
              '803' OR '804' OR '806'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD1'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD2'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD4'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD5'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
*> EDI 표준문서.
         WHEN '013'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD2'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD5'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
*> 해상운송 선적지.
         WHEN '014'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD2'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD4'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD5'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
*> 항공운송 선적지.
         WHEN '015'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD1'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD2'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD4'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD5'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
*> 해상운임 기본요율.
         WHEN '016'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD1'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD2'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD4'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD5'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
*> 항공운임 기본요율.
         WHEN '017'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD1'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD2'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD4'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD5'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
         WHEN '802'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD1'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD3'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD5'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
         WHEN '805' OR '807'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD4'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD5'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
         WHEN '810'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD2'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD4'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCD5'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
         WHEN '801' OR '809'.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
         WHEN OTHERS.
            IF WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-BLART'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-COND_TYPE' OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-KTOSL'     OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMRATE'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFCURR'    OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFIOCAC'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMNAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-ZFMXAMT'   OR
               WA_COLS-SCREEN-NAME EQ 'ZSIMIMG08-WAERS'     .
               WA_COLS-INVISIBLE = 1.
            ELSE.
               CLEAR : WA_COLS-INVISIBLE.
            ENDIF.
      ENDCASE.
      MODIFY TC_9301-COLS FROM WA_COLS.
   ENDLOOP.

ENDMODULE.                 " SET_COLUMN_VISIBLE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_DOCK_CONTROL_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_DOCK_CONTROL_SCRCOM OUTPUT.

  IF TREE_CONTROL_CREATED = 'X'.
    CALL METHOD DOCKING_CONTAINER->LINK
      EXPORTING
        REPID = REPID
        DYNNR = DYNNR
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        LIFETIME_DYNPRO_DYNPRO_LINK = 3
        OTHERS                      = 4.
    IF SY-SUBRC <> 0.
       MESSAGE I001.
    ENDIF.
  ELSE.
*    CREATE OBJECT G_APPLICATION.
    REPID = sy-repid.
    DYNNR = sy-dynnr.

    CREATE OBJECT DOCKING_CONTAINER
      EXPORTING
        REPID = REPID
        DYNNR = DYNNR
        SIDE  = DOCKING_CONTAINER->dock_at_left
        extension = 180
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    IF SY-SUBRC <> 0.
       MESSAGE I002.
    ENDIF.
  ENDIF.

  IF TREE_CONTROL_CREATED IS INITIAL.
*     PERFORM CREATE_AND_INIT_TREE_CONTROL CHANGING ERROR_FLAG.
      PERFORM CREATE_AND_INIT_TREE_CONTROL.
*      PERFORM CREATE_IMAGE_CONTROL.
*     IF ERROR_FLAG = 'X'.
*        USE_SUBSCREEN = 'X'. RAISE CREATE_ERROR.
*     ENDIF.
     TREE_CONTROL_CREATED = 'X'.
  ELSE.
*     PERFORM ACTUALIZE_HIDDEN_OBJECTS.
  ENDIF.

*  CALL METHOD CL_GUI_CFW=>FLUSH
*    EXCEPTIONS
*      CNTL_ERROR        = 1
*      CNTL_SYSTEM_ERROR = 2.

ENDMODULE.                 " SET_DOCK_CONTROL_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_EDIT_CONTROL_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_EDIT_CONTROL_SCR0010 OUTPUT.
  IF EDITOR IS INITIAL.
     REPID = SY-REPID.
     DYNNR = SY-DYNNR.

     CREATE OBJECT EDITCONTAINER
                   EXPORTING CONTAINER_NAME = 'SRC'
                   REPID  = REPID
                   DYNNR  = DYNNR.





     CREATE OBJECT EDITOR EXPORTING PARENT = EDITCONTAINER
                                    wordwrap_mode =
                     cl_gui_textedit=>WORDWRAP_AT_FIXED_POSITION
                                    wordwrap_position = 72
                                    MAX_NUMBER_CHARS = 100000.
     CLEAR SRC.
     DO 10 TIMES.
        SRC = 'TTTT'.
        APPEND SRC.
     ENDDO.
     CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE EXPORTING TABLE = SRC[].
     CALL METHOD cl_gui_cfw=>FLUSH.
     CALL METHOD editor->set_dragdrop
                 EXPORTING dragdrop = behaviour_right.
  ENDIF.



ENDMODULE.                 " SET_EDIT_CONTROL_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  P1000_GET_COMPANY_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE P1000_GET_COMPANY_DATA OUTPUT.

  IF W_FIRST_FLAG_0020 EQ 'Y'.
     PERFORM  P1000_GET_COMPANY_DATA USING SY-TCODE.
     W_FIRST_FLAG_0020 = 'N'.
  ENDIF.

ENDMODULE.                 " P1000_GET_COMPANY_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  P1000_GET_PLANT_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE P1000_GET_PLANT_DATA OUTPUT.

  IF W_FIRST_FLAG_0080 EQ 'Y'.

    SELECT DISTINCT BUKRS ZFAPLDT
           INTO CORRESPONDING FIELDS OF TABLE IT_TRPHD
           FROM ZTIMIMG20.

     W_FIRST_FLAG_0080 = 'N'.
  ENDIF.

ENDMODULE.                 " P1000_GET_PLANT_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0020 OUTPUT.

    DESCRIBE TABLE IT_T001 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0020-LINES = G_PARAM_LINE.                     " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0080  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0080 OUTPUT.

    DESCRIBE TABLE IT_TRPHD LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0080-LINES = G_PARAM_LINE.                     " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0080  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0800 OUTPUT.

    DESCRIBE TABLE IT_ZSIMIMG20 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0800-LINES = G_PARAM_LINE.                 " LINE 수 정?

    IF W_STATUS NE 'D'.
      TC_0800-LINES = TC_0800-LINES + 19 .
    ELSE.
      TC_0800-LINES = TC_0800-LINES .
    ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR0020 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0020-CURRENT_LINE GT TC_0020-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_T001 INDEX TC_0020-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_T001  TO  T001.
     MOVE : IT_T001-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR0080  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR0080 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0080-CURRENT_LINE GT TC_0080-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_TRPHD INDEX TC_0080-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_TRPHD  TO  ZTIMIMG20.
     MOVE : IT_TRPHD-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR0080  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR0800 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0800-CURRENT_LINE GT TC_0800-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  CLEAR : ZSIMIMG20, IT_ZSIMIMG20.
  READ TABLE IT_ZSIMIMG20 INDEX TC_0800-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG20  TO  ZSIMIMG20.
     MOVE : IT_ZSIMIMG20-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.
  MOVE 'USD' TO ZSIMIMG20-ZFUSD.

ENDMODULE.                 " FILL_TC_SCR0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_COMPANY_CODE_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_COMPANY_CODE_SCR0200 INPUT.

  IF W_STATUS EQ 'I'.
     IF ZTIMIMGTX-BUKRS IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMGTX' 'BUKRS'.
     ELSE.
        SELECT SINGLE * INTO W_ZTIMIMGTX
               FROM ZTIMIMGTX
               WHERE BUKRS EQ ZTIMIMGTX-BUKRS.
        IF SY-SUBRC EQ 0.
           MESSAGE E608(ZIM1) WITH ZTIMIMGTX-BUKRS.
        ENDIF.
     ENDIF.
  ENDIF.


ENDMODULE.                 " CHECK_COMPANY_CODE_SCR0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_COMPANY_CODE_SCR0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_COMPANY_CODE_SCR0500 INPUT.

  IF W_STATUS EQ 'I'.
     IF ZTIMIMG11-BUKRS IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG11' 'BUKRS'.
     ELSE.
        SELECT SINGLE * INTO W_ZTIMIMG11
               FROM ZTIMIMG11
               WHERE BUKRS EQ ZTIMIMG11-BUKRS.
        IF SY-SUBRC EQ 0.
           MESSAGE E608(ZIM1) WITH ZTIMIMG11-BUKRS.
        ENDIF.
        SELECT SINGLE * FROM T001
               WHERE BUKRS EQ ZTIMIMG11-BUKRS.
        SELECT SINGLE * FROM T005
               WHERE LAND1 EQ T001-LAND1.
        W_BUTXT = T001-BUTXT.
     ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_COMPANY_CODE_SCR0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_0002 OUTPUT.

  SET TITLEBAR   'POSI'.
  SET PF-STATUS  'POSI'.

ENDMODULE.                 " SET_STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_DATA_SCR0800 OUTPUT.


 IF W_FIRST_FLAG_0800 EQ 'Y'.

    SELECT *
           INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG20
           FROM ZTIMIMG20
          WHERE BUKRS  = ZTIMIMG20-BUKRS
            AND ZFAPLDT = ZTIMIMG20-ZFAPLDT.

    MOVE 'N' TO W_FIRST_FLAG_0800.
    MOVE 'D' TO W_STATUS.

 ENDIF.

ENDMODULE.                 " GET_DATA_SCR0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR2100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_DATA_SCR2100 OUTPUT.

 IF W_FIRST_FLAG_2100 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG21 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG21.

    MOVE 'N' TO W_FIRST_FLAG_2100.
    MOVE 'D' TO W_STATUS.
    TC_2100-TOP_LINE = 1.

 ENDIF.

ENDMODULE.                 " GET_DATA_SCR2100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG21
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG21.

   REFRESH : IT_ZSIMIMG21, IT_ZSIMIMG21_DEL.
* Table Multi-Select
   SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG21
            FROM  ZTIMIMG21
            WHERE BUKRS      EQ  ZTIMIMG21-BUKRS
            AND   ZFAPLDT    EQ  ZTIMIMG21-ZFAPLDT
            AND   ZFRAGB     EQ  ZTIMIMG21-ZFRAGB.

   IF SY-SUBRC NE 0.
      MESSAGE I967 WITH 'ZTIMIMG21'.
   ENDIF.

   IT_ZSIMIMG21_ORG[] = IT_ZSIMIMG21[].

ENDFORM.                    " P1000_READ_ZTIMIMG21
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR2100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR2100 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_2100-TOP_LINE.
    TC_2100-LINES = G_PARAM_LINE + 18.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG21 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_2100-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR2100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR2100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MARK_SET_SCR2100 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_2100-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR2100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR2100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR2100 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_2100-CURRENT_LINE GT TC_2100-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG21 INDEX TC_2100-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG21  TO  ZSIMIMG21.
     MOVE : IT_ZSIMIMG21-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR2100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  P1000_GET_DATA_SCR0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE P1000_GET_DATA_SCR0210 OUTPUT.

  IF W_FIRST_FLAG_0210 EQ 'Y'.
     PERFORM  P1000_GET_DATA_ZTIMIMG21.
     W_FIRST_FLAG_0210 = 'N'.
  ENDIF.

ENDMODULE.                 " P1000_GET_DATA_SCR0210  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0210 OUTPUT.

    DESCRIBE TABLE IT_0210 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0210-LINES = G_PARAM_LINE.                     " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0210  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR0210 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0210-CURRENT_LINE GT TC_0210-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_0210 INDEX TC_0210-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_0210  TO  ZTIMIMG21.
     MOVE : IT_0210-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR0210  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  P1000_GET_DATA_SCR0220  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE P1000_GET_DATA_SCR0220 OUTPUT.

  IF W_FIRST_FLAG_0220 EQ 'Y'.
     PERFORM  P1000_GET_DATA_ZTIMIMG22.
     W_FIRST_FLAG_0220 = 'N'.
  ENDIF.

ENDMODULE.                 " P1000_GET_DATA_SCR0220  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0220  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0220 OUTPUT.

  DESCRIBE TABLE IT_0220 LINES G_PARAM_LINE.        " LINE 수 GET
  TC_0220-LINES = G_PARAM_LINE.                     " LINE 수 정의.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0220  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR0220  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR0220 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0220-CURRENT_LINE GT TC_0220-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_0220 INDEX TC_0220-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_0220  TO  ZTIMIMG22.
     MOVE : IT_0220-ZFMARK        TO  W_ROW_MARK.    " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR0220  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR2200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR2200 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_2200-TOP_LINE.
    TC_2200-LINES = G_PARAM_LINE + 18.                " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG22 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_2200-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR2200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR2200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MARK_SET_SCR2200 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_2200-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR2200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR2200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR2200 OUTPUT.


  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_2200-CURRENT_LINE GT TC_2200-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG22 INDEX TC_2200-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG22  TO  ZSIMIMG22.
     MOVE : IT_ZSIMIMG22-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.
  MOVE  'USD'   TO  ZSIMIMG22-ZFUSD.

ENDMODULE.                 " FILL_TC_SCR2200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR0230  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR0230 OUTPUT.

 IF W_FIRST_FLAG_0100 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG20 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG01.

    MOVE 'N' TO W_FIRST_FLAG_0100.
    MOVE 'D' TO W_STATUS.
    TC_0230-TOP_LINE = 1.
 ENDIF.


ENDMODULE.                 " GET_DATA_SCR0230  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR2000  OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_DATA_SCR2000 OUTPUT.

 IF W_FIRST_FLAG_2000 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG01 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG20.

    MOVE 'N' TO W_FIRST_FLAG_2000.
    MOVE 'D' TO W_STATUS.
    TC_2000-TOP_LINE = 1.
 ENDIF.

ENDMODULE.                 " GET_DATA_SCR2000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR2000  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR2000 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_2000-TOP_LINE.
    TC_2000-LINES = G_PARAM_LINE + 17.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG20 LINES G_PARAM_LINE.   " LINE 수 GET
    TC_2000-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR2000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR2000  OUTPUT
*&---------------------------------------------------------------------*
MODULE MARK_SET_SCR2000 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_2000-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR2000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR2000  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR2000 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_2000-CURRENT_LINE GT TC_2000-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG20   INDEX TC_2000-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIMIMG20 TO  ZSIMIMG20.
     MOVE : IT_ZSIMIMG20-ZFMARK       TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR2000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR2300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_DATA_SCR2300 OUTPUT.

 IF W_FIRST_FLAG_2300 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIEPORT TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIEPORT.

    MOVE 'N' TO W_FIRST_FLAG_2300.
    MOVE 'D' TO W_STATUS.
    TC_2300-TOP_LINE = 1.

 ENDIF.

ENDMODULE.                 " GET_DATA_SCR2300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR2300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR2300 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_2300-TOP_LINE.
    TC_2300-LINES = G_PARAM_LINE + 24.              " LINE 수 정?
  ELSE.
    DESCRIBE TABLE IT_ZSIEPORT LINES G_PARAM_LINE.   " LINE 수 GET
    TC_2300-LINES = G_PARAM_LINE.                     " LINE 수 정?
  ENDIF.

* Top Line definition.
  IF POSI_GB = 'Y'.
     MOVE TOP_LINE TO TC_2300-TOP_LINE.
  ENDIF.

ENDMODULE.
*----------------------------------------------------------------------*
*&      Module  MARK_SET_SCR2300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MARK_SET_SCR2300 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_2300-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR2300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR2300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR2300 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_2300-CURRENT_LINE GT TC_2300-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIEPORT INDEX TC_2300-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_ZSIEPORT  TO  ZTIEPORT.
     MOVE : IT_ZSIEPORT-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.
* Top Line definition.
  IF POSI_GB = 'Y'.
     MOVE TOP_LINE TO TC_2300-TOP_LINE.
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR2300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  POSITION_INFO_TC_2300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE POSITION_INFO_TC_2300 OUTPUT.

  PERFORM P2000_POSITION_INFO_SCR2300 USING TC_2300-CURRENT_LINE
                                            G_PARAM_LINE.

ENDMODULE.                 " POSITION_INFO_TC_2300  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG23
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG23.

   REFRESH : IT_ZSIMIMG23_ORG, IT_ZSIMIMG23, IT_ZSIMIMG23_DEL.
* Table Multi-Select
   SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG23
            FROM  ZTIMIMG23.
   IF SY-SUBRC NE 0.
      MESSAGE I967 WITH 'ZTIMIMG23'.
   ENDIF.
   IT_ZSIMIMG23_ORG[] = IT_ZSIMIMG23[].

ENDFORM.                    " P1000_READ_ZTIMIMG23
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_0003 OUTPUT.

  SET TITLEBAR   'POSI'.
  SET PF-STATUS  'POSI'.

ENDMODULE.                 " SET_STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_0004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_0004 OUTPUT.

  SET TITLEBAR   'POSI'.
  SET PF-STATUS  'POSI'.
  CLEAR : ZSIMIMG23.

ENDMODULE.                 " SET_STATUS_0004  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR2500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_DATA_SCR2500 OUTPUT.

  IF W_FIRST_FLAG_2500 EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG23 TABLE SELECT
*-----------------------------------------------------------------------
    PERFORM  P1000_READ_ZTIMIMG23.
    MOVE 'N' TO W_FIRST_FLAG_2500.
    MOVE 'D' TO W_STATUS.
    TC_2500-TOP_LINE = 1.
  ENDIF.
ENDMODULE.                 " GET_DATA_SCR2500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR2500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR2500 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_2500-TOP_LINE.
    TC_2500-LINES = G_PARAM_LINE + 20.
  ELSE.
    DESCRIBE TABLE IT_ZSIMIMG23 LINES G_PARAM_LINE.
    TC_2500-LINES = G_PARAM_LINE.
  ENDIF.

* Top Line 정의.
  IF POSI_GB = 'Y'.
     MOVE TOP_LINE TO TC_2500-TOP_LINE.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR2500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_SET_SCR2500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MARK_SET_SCR2500 OUTPUT.

  IF OK-CODE = 'SELA'                  " mark all
     AND TC_2500-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR2500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR2500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR2500 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 검증.
  IF TC_2500-CURRENT_LINE GT TC_2500-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSIMIMG23 INDEX TC_2500-CURRENT_LINE.
  IF SY-SUBRC = 0.
     MOVE-CORRESPONDING  IT_ZSIMIMG23  TO  ZTIMIMG23.
     MOVE : IT_ZSIMIMG23-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.
* Top Line 정의.
  IF POSI_GB = 'Y'.
     MOVE TOP_LINE TO TC_2500-TOP_LINE.
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR2500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  POSITION_INFO_TC_2500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE POSITION_INFO_TC_2500 OUTPUT.

  PERFORM P2000_POSITION_INFO_SCR2300 USING TC_2500-CURRENT_LINE
                                              G_PARAM_LINE.

ENDMODULE.                 " POSITION_INFO_TC_2500  OUTPUT
