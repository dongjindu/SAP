*&---------------------------------------------------------------------*
*& INCLUDE ZRZIMGF01 .
*&---------------------------------------------------------------------*
*&  ÇÁ·Î±×·¥¸í : ¼öÀÔ½Ã½ºÅÛ Configuration °ü¸® Sub Module Inculde      *
*&      ÀÛ¼ºÀÚ : °­¼®ºÀ INFOLINK Ltd.                                  *
*&      ÀÛ¼ºÀÏ : 2000.01.26                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG01
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG01.

  REFRESH IT_ZSIMIMG01.

* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG01
                                               FROM ZTIMIMG01
            ORDER BY ZTERM ZFAPLDT.


  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG01'.
  ENDIF.
  IT_ZSIMIMG01_ORG[] = IT_ZSIMIMG01[].

ENDFORM.                    " P1000_READ_ZTIMIMG01
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG01
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG01.
  CLEAR : W_ZTERM, W_ZFAPLDT, W_BSART, W_BSTYP.

* Internal Table Sort
  SORT IT_ZSIMIMG01  BY BSTYP BSART ZTERM ZFAPLDT.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSIMIMG01.
    IF W_BSART   EQ IT_ZSIMIMG01-BSART AND
       W_BSTYP   EQ IT_ZSIMIMG01-BSTYP AND
       W_ZTERM   EQ IT_ZSIMIMG01-ZTERM AND
       W_ZFAPLDT EQ IT_ZSIMIMG01-ZFAPLDT.
      MESSAGE E956 WITH W_ZTERM   W_ZFAPLDT.
    ENDIF.

    MOVE : IT_ZSIMIMG01-BSART   TO W_BSART,
           IT_ZSIMIMG01-BSTYP   TO W_BSTYP,
           IT_ZSIMIMG01-ZTERM   TO W_ZTERM,
           IT_ZSIMIMG01-ZFAPLDT TO W_ZFAPLDT,
           SY-TABIX             TO W_TABIX.

* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG01-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG01_ORG WITH KEY
                        BSART   = IT_ZSIMIMG01-BSART
                        BSTYP   = IT_ZSIMIMG01-BSTYP
                        ZTERM   = IT_ZSIMIMG01-ZTERM
                        ZFAPLDT = IT_ZSIMIMG01-ZFAPLDT.
        IF SY-SUBRC EQ 0.
          MESSAGE E956 WITH W_ZTERM W_ZFAPLDT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Delete.
  LOOP AT IT_ZSIMIMG01_DEL.
    DELETE FROM ZTIMIMG01
           WHERE BSART   EQ IT_ZSIMIMG01_DEL-BSART
           AND   BSTYP   EQ IT_ZSIMIMG01_DEL-BSTYP
           AND   ZTERM   EQ IT_ZSIMIMG01_DEL-ZTERM
           AND   ZFAPLDT EQ IT_ZSIMIMG01_DEL-ZFAPLDT.
* Change Document.
    CLEAR : *ZTIMIMG01.
    MOVE-CORRESPONDING IT_ZSIMIMG01_DEL TO  ZTIMIMG01.
    PERFORM   P3000_ZTIMIMG01_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG01_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG01.
    SELECT SINGLE * FROM ZTIMIMG01
           WHERE BSART   EQ IT_ZSIMIMG01-BSART
           AND   BSTYP   EQ IT_ZSIMIMG01-BSTYP
           AND   ZTERM   EQ IT_ZSIMIMG01-ZTERM
           AND   ZFAPLDT EQ IT_ZSIMIMG01-ZFAPLDT.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG01-ZFREQTY  EQ ZTIMIMG01-ZFREQTY AND
               IT_ZSIMIMG01-ZFUSPR   EQ ZTIMIMG01-ZFUSPR  AND
               IT_ZSIMIMG01-ZFUSAT   EQ ZTIMIMG01-ZFUSAT  AND
               IT_ZSIMIMG01-ZFBACD   EQ ZTIMIMG01-ZFBACD  AND
               IT_ZSIMIMG01-ZFTRMB   EQ ZTIMIMG01-ZFTRMB  AND
               IT_ZSIMIMG01-ZFLCKN   EQ ZTIMIMG01-ZFLCKN  AND
               IT_ZSIMIMG01-MWSKZ    EQ ZTIMIMG01-MWSKZ   AND
               IT_ZSIMIMG01-ZFTRTX1  EQ ZTIMIMG01-ZFTRTX1 AND
               IT_ZSIMIMG01-ZFTRTX2  EQ ZTIMIMG01-ZFTRTX2 AND
               IT_ZSIMIMG01-ZFTRTX3  EQ ZTIMIMG01-ZFTRTX3 AND
               IT_ZSIMIMG01-ZFTRTX4  EQ ZTIMIMG01-ZFTRTX4 AND
               IT_ZSIMIMG01-ZFPREPAY EQ ZTIMIMG01-ZFPREPAY ).

        MOVE-CORRESPONDING ZTIMIMG01    TO *ZTIMIMG01.
        MOVE-CORRESPONDING IT_ZSIMIMG01 TO  ZTIMIMG01.
        MOVE : SY-UNAME         TO   ZTIMIMG01-UNAM,
               SY-DATUM         TO   ZTIMIMG01-UDAT.
        UPDATE ZTIMIMG01.
        IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.
* Change Document..
        PERFORM   P3000_ZTIMIMG01_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG01-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG01 TO ZTIMIMG01.
      MOVE : SY-UNAME  TO   ZTIMIMG01-UNAM,
             SY-DATUM  TO   ZTIMIMG01-UDAT,
             SY-UNAME  TO   ZTIMIMG01-ERNAM,
             SY-DATUM  TO   ZTIMIMG01-CDAT.
      INSERT ZTIMIMG01.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.
* Change Document..
      CLEAR : *ZTIMIMG01.
      PERFORM   P3000_ZTIMIMG01_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.                  " END LOOP

ENDFORM.                    " P3000_WRITE_ZTIMIMG01
*&---------------------------------------------------------------------*
*&      Form  P1000_VENDOR_ACCOUNT_GRP
*&---------------------------------------------------------------------*
FORM P1000_VENDOR_ACCOUNT_GRP.

*  CALL FUNCTION 'ZIM_GET_VENDOR_ACC_GRP'
*       EXPORTING
*           KTOKK  =   IT_TC_0100-KTOKK
*       IMPORTING
*           TXT30  =   IT_TC_0100-TXT30
*       EXCEPTIONS
*           KEY_INCOMPLETE = 0
*           NOT_FOUND      = 1
*           NOT_FOUND_TEXT = 2.
*
*  CASE SY-SUBRC.
*     WHEN 1.
**           MESSAGE E023 WITH IT_TC_0100-KTOKK.
*        MESSAGE E046(F2) WITH IT_TC_0100-KTOKK 'T077K'.
*     WHEN 2.
*        MESSAGE I046(F2) WITH IT_TC_0100-KTOKK 'T077Y'.
*     WHEN OTHERS.
*  ENDCASE.

ENDFORM.                    " P1000_VENDOR_ACCOUNT_GRP
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_TERM_OF_PAYMENT
*&---------------------------------------------------------------------*
FORM P1000_GET_TERM_OF_PAYMENT.
  DATA : WL_TEXT1 LIKE  V_T052-TEXT1.

  CALL FUNCTION 'ZIM_GET_TERM_OF_PAYMENT'
       EXPORTING
            ZTERM          = IT_ZSIMIMG01-ZTERM
       IMPORTING
            TEXT1          = WL_TEXT1
       EXCEPTIONS
            KEY_INCOMPLETE = 0
            NOT_FOUND      = 1
            NOT_FOUND_TEXT = 2.

  CASE SY-SUBRC.
    WHEN 1.
      MESSAGE E024     WITH IT_ZSIMIMG01-ZTERM 'T052K'.
    WHEN 2.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P1000_GET_TERM_OF_PAYMENT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SET_MESSAGE USING    P_SY_UCOMM.

  CASE P_SY_UCOMM.
    WHEN 'ANZG'.      " CHANGE  ==> DISPLAY
      PERFORM  P2000_ANZG_MESSAGE.
    WHEN 'SAVE'.      " Save
      PERFORM  P2000_SAVE_MESSAGE.
    WHEN 'CANC'.      " Cancel
      PERFORM  P2000_CANCEL_MESSAGE.
    WHEN 'DELT'.      " Delete
      PERFORM  P2000_DELETE_MESSAGE.
    WHEN 'BACK' OR 'EXIT'.   " Back or Exit
      PERFORM  P2000_EXIT_MESSAGE.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " P2000_SET_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_CANCEL_MESSAGE.
  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING 'Ãë¼Ò È®ÀÎ'             " Å¸ÀÌÆ²...
                                   'º¯°æµÈ ³»¿ëÀ» ÀúÀå¾øÀÌ Á¾·áµË´Ï´Ù.'
                                    'Á¾·áÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                                    'N'                 " Ãë¼Ò ¹öÆ° À¯/?
                                   '2'.                 " default button
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING
           'Cancel confirm'
           'Being ended without saving the changed item.'
           'Do you want to end?'
           'N'
           '2'.

  ENDIF.
  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      MESSAGE  S957.
      LEAVE TO SCREEN 0.  " " PROGRAM LEAVING
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_BOX
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

  CALL SCREEN 0001 STARTING AT 30 6
                   ENDING   AT 78 10.

  IF ANTWORT = 'C'.       " Cancel
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_EXIT_MESSAGE.
  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING 'Á¾·á È®ÀÎ'             " Å¸ÀÌÆ²...
                           'ÇöÀç ÀÔ·Â³»¿ªÀ» ÀúÀåÇÏÁö ¾Ê½À´Ï´Ù.'" MSG1
                            'ÀúÀå ÈÄ Á¾·áÇÏ½Ã°Ú½À´Ï±î?'     " MSG2
                            'Y'                         " Ãë¼Ò ¹öÆ° ?
                            '1'.                        " default button
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING
           'End confirm'
           'Do not save the entered item.'
           'Do you want to end after save?'
           'Y'
           '1'.
  ENDIF.

ENDFORM.                    " P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SAVE_MESSAGE.
  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING 'ÀúÀå È®ÀÎ'             " Å¸ÀÌÆ²...
                                    'ÀÔ·ÂµÈ ³»¿ªÀ» ÀúÀåÇÕ´Ï´Ù.'
                                    'ÀúÀåÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                                    'Y'                 " Ãë¼Ò ¹öÆ° À¯/?
                                   '1'.                 " default button
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING
           'Save confirm '
           'Being saved entered item.'
           'Do you want to save?'
           'Y'
           '1'.

  ENDIF.
ENDFORM.                    " P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_ANZG_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_ANZG_MESSAGE.
  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING 'ÀÌµ¿ È®ÀÎ'             " Å¸ÀÌÆ²...
                                   'ÇöÀç ÀÔ·Â³»¿ªÀ» ÀúÀåÇÏÁö ¾Ê½À´Ï´Ù.'
                                    'ÀúÀå ÈÄ ÀÌµ¿ÇÏ½Ã°Ú½À´Ï±î?'" MSG2
                                    'Y'                 " Ãë¼Ò ¹öÆ° À¯/?
                                   '1'.                 " default button
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING
           'Move confirm'
           'Do not save entered item.'
           'Do you want to move another screen after save?'
           'Y'
           '1'.
  ENDIF.
ENDFORM.                    " P2000_ANZG_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG00
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG00.
* Clearing
  CLEAR: ZTIMIMG00.
* Table Single-Select
  SELECT SINGLE * FROM ZTIMIMG00.
  MOVE-CORRESPONDING  ZTIMIMG00   TO    *ZTIMIMG00.

ENDFORM.                    " P1000_READ_ZTIMIMG00
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG00
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG00.
  DATA: L_ZTIMIMG00   LIKE    ZTIMIMG00.

  IF ZTIMIMG00-ZFBKYN = 'X'.
  ENDIF.

  IF ZTIMIMG00-ZFUSD IS INITIAL.
    MOVE : 'USD' TO ZTIMIMG00-ZFUSD.
  ENDIF.

  IF  ZTIMIMG00-ERNAM IS INITIAL.
    MOVE: SY-DATUM  TO  ZTIMIMG00-CDAT,
          SY-UNAME  TO  ZTIMIMG00-ERNAM,
          SY-MANDT  TO  ZTIMIMG00-MANDT.
  ENDIF.
  MOVE: SY-DATUM   TO   ZTIMIMG00-UDAT,
        SY-UNAME   TO   ZTIMIMG00-UNAM.

  SELECT SINGLE * INTO *ZTIMIMG00
         FROM     ZTIMIMG00.

  IF SY-SUBRC EQ 0.
    UPDATE ZTIMIMG00.
* Change Document Object
    PERFORM   P3000_ZTIMIMG00_CHANGE_DOC        USING  'U'.
  ELSE.
* Change Document Object
    INSERT ZTIMIMG00.
    PERFORM   P3000_ZTIMIMG00_CHANGE_DOC        USING  'I'.
  ENDIF.

  IF SY-SUBRC NE 0.
    MESSAGE E952.
  ENDIF.

ENDFORM.                    " P3000_WRITE_ZTIMIMG00
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG04
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG04.

  REFRESH : IT_ZSIMIMG04, IT_ZSIMIMG04_DEL.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG04
                                               FROM ZTIMIMG04
            ORDER BY ZFWERKS ZFMATGB ZFAPLDT.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG04'.
  ENDIF.

  IT_ZSIMIMG04_ORG[] = IT_ZSIMIMG04[].
* Table Multi-Select
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG04_ORG
*                                               FROM ZTIMIMG04
*            ORDER BY ZFWERKS ZFMATGB ZFAPLDT.

ENDFORM.                    " P1000_READ_ZTIMIMG04
*&---------------------------------------------------------------------*
*&      Form  P3000_DATABASE_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_DATABASE_MODIFY.
  CASE SY-TCODE.
    WHEN 'ZIMG01'.
      PERFORM  P3000_WRITE_ZTIMIMG01.
    WHEN 'ZIMG10'.
      PERFORM  P3000_WRITE_ZTIMIMG10.
    WHEN 'ZIMG06'.
      PERFORM  P3000_WRITE_ZTIMIMG12.
    WHEN 'ZIMG11'.
      PERFORM  P3000_WRITE_ZTIMIMG06.
    WHEN 'ZIMG12'.
      PERFORM  P3000_WRITE_ZTIMIMG04.
    WHEN 'ZIMG14'.
      PERFORM  P3000_WRITE_ZTIMIMG07.
    WHEN 'ZIMG15'.
      PERFORM  P3000_WRITE_ZTIMIMG05.
    WHEN 'ZIMG16'.
      PERFORM  P3000_WRITE_ZTIMIMG09.
    WHEN 'ZIMG17'.
      PERFORM  P3000_WRITE_ZTIMIMG17.
    WHEN 'ZIMG20'.
      PERFORM  P3000_WRITE_ZTIMIMG20.
    WHEN 'ZIMG21'.
      PERFORM  P3000_WRITE_ZTIMIMG21.
    WHEN 'ZIMG22'.
      PERFORM  P3000_WRITE_ZTIMIMG22.
    WHEN 'ZIMG24'.           " Manage Port by Country.
      PERFORM  P3000_WRITE_ZTIEPORT.
    WHEN 'ZIMG25'.           " Port/Freight area match code.
      PERFORM  P3000_WRITE_ZTIMIMG23.
    WHEN 'ZIMG24N'.
      PERFORM  P3000_WRITE_ZTIMIMG24.
    WHEN 'ZIMGC1'.
      PERFORM  P3000_WRITE_ZTIMIMG02.
    WHEN 'ZIMGC2'.
      PERFORM  P3000_WRITE_ZTIMIMG03.
    WHEN 'ZIMGC3' OR 'ZIMGC4' OR 'ZEXZ23'.
      PERFORM  P3000_WRITE_ZTIMIMG08.
    WHEN 'ZIMGA1'.
      IF STANDARD_UP EQ 'X'.
        PERFORM  P3000_CALL_TRANSACTION_OB08.
      ELSE.
        W_COUNT = 0.
        LOOP AT IT_CURR WHERE ZFMARK = 'X'.
          ADD    1    TO    W_COUNT.
        ENDLOOP.
        W_SY_SUBRC = 0.
      ENDIF.

      IF W_SY_SUBRC EQ 0.
        PERFORM  P3000_ZTIMIMG06_UPDATE.
      ENDIF.
    WHEN OTHERS.

  ENDCASE.
ENDFORM.                    " P3000_DATABASE_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG04
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG04.
  CLEAR : W_ZFWERKS, W_ZFMATGB, W_ZFAPLDT.

* Internal Table Sort( °èÁ¤ÄÚµå ¹× Payment Terms )
  SORT IT_ZSIMIMG04  BY ZFWERKS   ZFMATGB   ZFAPLDT.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSIMIMG04.
    IF W_ZFWERKS EQ IT_ZSIMIMG04-ZFWERKS AND
       W_ZFMATGB EQ IT_ZSIMIMG04-ZFMATGB AND
       W_ZFAPLDT EQ IT_ZSIMIMG04-ZFAPLDT.
      MESSAGE E968 WITH W_ZFWERKS W_ZFMATGB W_ZFAPLDT.
    ENDIF.

    MOVE : IT_ZSIMIMG04-ZFWERKS TO W_ZFWERKS,
           IT_ZSIMIMG04-ZFMATGB TO W_ZFMATGB,
           IT_ZSIMIMG04-ZFAPLDT TO W_ZFAPLDT,
           SY-TABIX             TO W_TABIX.

* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG04-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG04_ORG WITH KEY
                         ZFWERKS = IT_ZSIMIMG04-ZFWERKS
                         ZFMATGB = IT_ZSIMIMG04-ZFMATGB
                         ZFAPLDT = IT_ZSIMIMG04-ZFAPLDT.
        IF SY-SUBRC EQ 0.
          MESSAGE E968 WITH W_ZFWERKS W_ZFMATGB W_ZFAPLDT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG04_DEL.
    DELETE FROM ZTIMIMG04
           WHERE ZFWERKS EQ IT_ZSIMIMG04_DEL-ZFWERKS
           AND   ZFMATGB EQ IT_ZSIMIMG04_DEL-ZFMATGB
           AND   ZFAPLDT EQ IT_ZSIMIMG04_DEL-ZFAPLDT.

* Change Document..
    CLEAR : *ZTIMIMG04.
    MOVE-CORRESPONDING IT_ZSIMIMG04_DEL TO  ZTIMIMG04.
    PERFORM   P3000_ZTIMIMG04_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG04_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG04.
    SELECT SINGLE * FROM ZTIMIMG04
                    WHERE ZFWERKS EQ IT_ZSIMIMG04-ZFWERKS
                    AND   ZFMATGB EQ IT_ZSIMIMG04-ZFMATGB
                    AND   ZFAPLDT EQ IT_ZSIMIMG04-ZFAPLDT.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG04-ZFPLRTE  EQ ZTIMIMG04-ZFPLRTE ).
        MOVE-CORRESPONDING ZTIMIMG04    TO *ZTIMIMG04.
        MOVE-CORRESPONDING IT_ZSIMIMG04 TO  ZTIMIMG04.
        MOVE : SY-UNAME         TO   ZTIMIMG04-UNAM,
               SY-DATUM         TO   ZTIMIMG04-UDAT.
        UPDATE ZTIMIMG04.
        IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
        PERFORM   P3000_ZTIMIMG04_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG04-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG04 TO  ZTIMIMG04.
      MOVE : SY-UNAME  TO   ZTIMIMG04-ERNAM,
             SY-DATUM  TO   ZTIMIMG04-CDAT,
             SY-UNAME  TO   ZTIMIMG04-UNAM,
             SY-DATUM  TO   ZTIMIMG04-UDAT.
      INSERT ZTIMIMG04.

* Change Document..
      CLEAR : *ZTIMIMG04.
      PERFORM   P3000_ZTIMIMG04_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG04
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SAVE_PROCESS.

* Message..
  W_OK_CODE = OK-CODE.
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.
* Pop-Up Screen..
  CASE ANTWORT.
    WHEN 'Y'.              " Yes
      PERFORM  P3000_DATABASE_MODIFY.
      MESSAGE  S953.
      CASE W_OK_CODE.
        WHEN 'ANZG' OR 'SAVE'.   " CHANGE ==> DISPLAY or SAVE
          PERFORM P2000_SET_LOCK_MODE    USING    'U'.
          IF SY-TCODE EQ 'ZIMGA1'.
            PERFORM 2000_BACK_SCREEN_DEFINE.
            EXIT.
          ENDIF.
          MOVE 'D' TO W_STATUS.
        WHEN OTHERS.
      ENDCASE.
      PERFORM  P1000_DATA_REREAD.    " DATA READ
    WHEN 'N'.              " No...
      IF W_OK_CODE EQ 'BACK' OR W_OK_CODE EQ 'EXIT'.
        MESSAGE  S957.
        PERFORM 2000_BACK_SCREEN_DEFINE.
      ENDIF.
      CASE W_OK_CODE.
        WHEN 'ANZG'.     " CHANGE ==> DISPLAY
          MOVE 'D' TO W_STATUS.
      ENDCASE.
      PERFORM  P1000_DATA_REREAD.    " DATA READ
    WHEN 'C'.              " Cancel
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SAVE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_DEL_MARK
*&---------------------------------------------------------------------*
FORM P2000_SET_DEL_MARK.
  IF OK-CODE EQ 'DELC'.
    W_ROW_MARK  = ''.
  ELSEIF OK-CODE EQ 'DELE'.
    W_ROW_MARK  = 'X'.
  ENDIF.

  CASE SY-TCODE.
    WHEN 'ZIMG01'.
      LOOP AT IT_ZSIMIMG01 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG01-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG01.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG06'.
      LOOP AT IT_ZSIMIMG12 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG12-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG12.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG10'.
      LOOP AT IT_ZSIMIMG10 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG10-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG10.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

      IF W_STATUS EQ 'I'.
        DELETE IT_ZSIMIMG10 WHERE LOEKZ = 'D'.
      ENDIF.

    WHEN 'ZIMG11'.
      LOOP AT IT_ZSIMIMG06 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG06-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG06.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

      IF W_STATUS EQ 'I'.
        DELETE IT_ZSIMIMG06 WHERE LOEKZ = 'D'.
      ENDIF.

    WHEN 'ZIMG12'.
      LOOP AT IT_ZSIMIMG04 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG04-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG04.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG14'.
      LOOP AT IT_ZSIMIMG07 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG07-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG07.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG15'.
      LOOP AT IT_ZSIMIMG05 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG05-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG05.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG16'.
      LOOP AT IT_ZSIMIMG09 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG09-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG09.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG17'.
      LOOP AT IT_ZSIMIMG17 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG17-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG17.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG21'.
      LOOP AT IT_ZSIMIMG21 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG21-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG21.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG24'.
      LOOP AT IT_ZSIEPORT WHERE ZFMARK = 'X'.
        IT_ZSIEPORT-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIEPORT.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG24N'.
      LOOP AT IT_ZSIMIMG24 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG24-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG24.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMGC1'.
      LOOP AT IT_ZSIMIMG02 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG02-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG02.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMGC2'.
      LOOP AT IT_ZSIMIMG03 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG03-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG03.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMGC3' OR 'ZIMGC4' OR 'ZEXZ23'.
      LOOP AT IT_ZSIMIMG08 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG08-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSIMIMG08.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_DEL_MARK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ROW_MARK
*&---------------------------------------------------------------------*
FORM P2000_SET_ROW_MARK.
  IF OK-CODE EQ 'MKAL'.      " ÀüÃ¼ ¼±?
    W_ROW_MARK = 'X'.
  ELSEIF OK-CODE EQ 'MKLO'.  " ¼±ÅÃ ÇØ?
    CLEAR : W_ROW_MARK.
  ENDIF.
  CASE SY-TCODE.
    WHEN 'ZIMG01'.
      LOOP AT IT_ZSIMIMG01.
        IT_ZSIMIMG01-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG01.
      ENDLOOP.
    WHEN 'ZIMG06'.
      LOOP AT IT_ZSIMIMG12.
        IT_ZSIMIMG12-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG12.
      ENDLOOP.
    WHEN 'ZIMG10'.
      LOOP AT IT_ZSIMIMG10.
        IT_ZSIMIMG10-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG10.
      ENDLOOP.
    WHEN 'ZIMG11'.
      LOOP AT IT_ZSIMIMG06.
        IT_ZSIMIMG06-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG06.
      ENDLOOP.
    WHEN 'ZIMG12'.
      LOOP AT IT_ZSIMIMG04.
        IT_ZSIMIMG04-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG04.
      ENDLOOP.
    WHEN 'ZIMG14'.
      LOOP AT IT_ZSIMIMG07.
        IT_ZSIMIMG07-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG07.
      ENDLOOP.
    WHEN 'ZIMG15'.
      LOOP AT IT_ZSIMIMG05.
        IT_ZSIMIMG05-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG05.
      ENDLOOP.
    WHEN 'ZIMG16'.
      LOOP AT IT_ZSIMIMG09.
        IT_ZSIMIMG09-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG09.
      ENDLOOP.
    WHEN 'ZIMG17'.
      LOOP AT IT_ZSIMIMG17.
        IT_ZSIMIMG17-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG17.
      ENDLOOP.
    WHEN 'ZIMG20'.
      LOOP AT IT_ZSIMIMG20.
        IT_ZSIMIMG20-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG20.
      ENDLOOP.
    WHEN 'ZIMG21'.
      LOOP AT IT_ZSIMIMG21.
        IT_ZSIMIMG21-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG21.
      ENDLOOP.
    WHEN 'ZIMG24'.
      LOOP AT IT_ZSIEPORT.
        IT_ZSIEPORT-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIEPORT.
      ENDLOOP.
    WHEN 'ZIMG24N'.
      LOOP AT IT_ZSIMIMG24.
        IT_ZSIMIMG24-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG24.
      ENDLOOP.
    WHEN 'ZIMGC1'.
      LOOP AT IT_ZSIMIMG02.
        IT_ZSIMIMG02-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG02.
      ENDLOOP.
    WHEN 'ZIMGC2'.
      LOOP AT IT_ZSIMIMG03.
        IT_ZSIMIMG03-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG03.
      ENDLOOP.
    WHEN 'ZIMGC3' OR 'ZIMGC4' OR 'ZEXZ23'.
      LOOP AT IT_ZSIMIMG08.
        IT_ZSIMIMG08-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSIMIMG08.
      ENDLOOP.
    WHEN 'ZIMGA1'.
      LOOP AT IT_CURR.
        IT_CURR-ZFMARK = W_ROW_MARK.    MODIFY IT_CURR.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_ROW_MARK
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_TAB_REFRESH
*&---------------------------------------------------------------------*
FORM P2000_IT_TAB_REFRESH.

  CASE SY-TCODE.
    WHEN 'ZIMG01'.
      REFRESH : IT_ZSIMIMG01, IT_ZSIMIMG01_DEL.
    WHEN 'ZIMG06'.
      REFRESH : IT_ZSIMIMG12, IT_ZSIMIMG12_DEL.
    WHEN 'ZIMG10'.
      REFRESH : IT_ZSIMIMG10, IT_ZSIMIMG10_DEL.
    WHEN 'ZIMG11'.
      REFRESH : IT_ZSIMIMG06, IT_ZSIMIMG06_DEL.
    WHEN 'ZIMG12'.
      REFRESH : IT_ZSIMIMG04, IT_ZSIMIMG04_DEL.
    WHEN 'ZIMG14'.
      REFRESH : IT_ZSIMIMG07, IT_ZSIMIMG07_DEL.
    WHEN 'ZIMG15'.
      REFRESH : IT_ZSIMIMG05, IT_ZSIMIMG05_DEL.
    WHEN 'ZIMG16'.
      REFRESH : IT_ZSIMIMG09, IT_ZSIMIMG09_DEL.
    WHEN 'ZIMG17'.
      REFRESH : IT_ZSIMIMG17, IT_ZSIMIMG17_DEL.
    WHEN 'ZIMG20'.
      REFRESH : IT_ZSIMIMG20, IT_ZSIMIMG20_DEL.
    WHEN 'ZIMG21'.
      REFRESH : IT_ZSIMIMG21, IT_ZSIMIMG21_DEL.
    WHEN 'ZIMG22'.
      REFRESH : IT_ZSIMIMG22, IT_ZSIMIMG22_DEL.
    WHEN 'ZIMG24'.
      REFRESH : IT_ZSIEPORT, IT_ZSIEPORT_DEL.
    WHEN 'ZIMG25'.
      REFRESH : IT_ZSIMIMG23, IT_ZSIMIMG23_DEL.
    WHEN 'ZIMG24N'.
      REFRESH : IT_ZSIMIMG24, IT_ZSIMIMG24_DEL.
    WHEN 'ZIMGC1'.
      REFRESH : IT_ZSIMIMG02, IT_ZSIMIMG02_DEL.
    WHEN 'ZIMGC2'.
      REFRESH : IT_ZSIMIMG03, IT_ZSIMIMG03_DEL.
    WHEN 'ZIMGC3' OR 'ZIMGC4'.
      REFRESH : IT_ZSIMIMG08, IT_ZSIMIMG08_DEL.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_IT_TAB_REFRESH
*&---------------------------------------------------------------------*
*&      Form  P1000_DATA_REREAD
*&---------------------------------------------------------------------*
FORM P1000_DATA_REREAD.
  IF W_STATUS EQ 'I'.
    EXIT.
  ENDIF.
  CASE SY-TCODE.
    WHEN  'ZIMG01'.
      PERFORM  P1000_READ_ZTIMIMG01.    "
    WHEN  'ZIMG06'.
      PERFORM  P1000_READ_ZTIMIMG12.    "
    WHEN  'ZIMG10'.
      PERFORM  P1000_READ_ZTIMIMG10.    "
    WHEN  'ZIMG11'.
      PERFORM  P1000_READ_ZTIMIMG06.    "
    WHEN  'ZIMG12'.
      PERFORM  P1000_READ_ZTIMIMG04.    "
    WHEN  'ZIMG14'.
      PERFORM  P1000_READ_ZTIMIMG07.    "
    WHEN  'ZIMG15'.
      PERFORM  P1000_READ_ZTIMIMG05.    "
    WHEN  'ZIMG16'.
      PERFORM  P1000_READ_ZTIMIMG09.    "
    WHEN  'ZIMG17'.
      PERFORM  P1000_READ_ZTIMIMG17.    "
    WHEN  'ZIMG20'.
      PERFORM  P1000_READ_ZTIMIMG20.    "
    WHEN  'ZIMG21'.
      PERFORM  P1000_READ_ZTIMIMG21.    "
    WHEN  'ZIMG22'.
      PERFORM  P1000_READ_ZTIMIMG22.    "
    WHEN  'ZIMG24'.
      PERFORM  P1000_READ_ZTIEPORT.    "
    WHEN  'ZIMG25'.
      PERFORM  P1000_READ_ZTIMIMG23.    " Port/Freight area match code.
    WHEN  'ZIMG24N'.
      PERFORM  P1000_READ_ZTIMIMG24.    "
    WHEN  'ZIMGC1'.
      PERFORM  P1000_READ_ZTIMIMG02.    "
    WHEN  'ZIMGC2'.
      PERFORM  P1000_READ_ZTIMIMG03.    "
    WHEN  'ZIMGC3' OR 'ZIMGC4' OR 'ZEXZ23'.
      PERFORM  P1000_READ_ZTIMIMG08.    "
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P1000_DATA_REREAD
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG06
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG06.

  REFRESH : IT_ZSIMIMG06, IT_ZSIMIMG06_DEL.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG06
                                              FROM ZTIMIMG06
            ORDER BY WAERS   ZFAPLDT.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG06'.
  ENDIF.

  IT_ZSIMIMG06_ORG[] = IT_ZSIMIMG06[].

ENDFORM.                    " P1000_READ_ZTIMIMG06
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG06
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG06.
  CLEAR : W_WAERS, W_ZFAPLDT.

* Internal Table Sort
  SORT IT_ZSIMIMG06  BY WAERS   ZFAPLDT.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSIMIMG06.
    IF W_WAERS   EQ IT_ZSIMIMG06-WAERS AND
       W_ZFAPLDT EQ IT_ZSIMIMG06-ZFAPLDT.
      MESSAGE E956 WITH W_WAERS   W_ZFAPLDT.
    ENDIF.

    MOVE : IT_ZSIMIMG06-WAERS   TO W_WAERS,
           IT_ZSIMIMG06-ZFAPLDT TO W_ZFAPLDT,
           SY-TABIX             TO W_TABIX.

* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG06-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG06_ORG WITH KEY
                        WAERS   = IT_ZSIMIMG06-WAERS
                        ZFAPLDT = IT_ZSIMIMG06-ZFAPLDT.
        IF SY-SUBRC EQ 0.
          MESSAGE E956 WITH W_WAERS W_ZFAPLDT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG06_DEL.
    DELETE FROM ZTIMIMG06
           WHERE WAERS   EQ IT_ZSIMIMG06_DEL-WAERS
           AND   ZFAPLDT EQ IT_ZSIMIMG06_DEL-ZFAPLDT.

* Change Document..
    CLEAR : *ZTIMIMG06.
    MOVE-CORRESPONDING IT_ZSIMIMG06_DEL TO  ZTIMIMG06.
    PERFORM   P3000_ZTIMIMG06_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG06_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG06.
    SELECT SINGLE * FROM ZTIMIMG06
                    WHERE WAERS   EQ IT_ZSIMIMG06-WAERS
                    AND   ZFAPLDT EQ IT_ZSIMIMG06-ZFAPLDT.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG06-ZFEXRT   EQ ZTIMIMG06-ZFEXRT AND
               IT_ZSIMIMG06-ZFEXPDT  EQ ZTIMIMG06-ZFEXPDT ).
        MOVE-CORRESPONDING ZTIMIMG06    TO *ZTIMIMG06.
        MOVE-CORRESPONDING IT_ZSIMIMG06 TO  ZTIMIMG06.
        MOVE : SY-UNAME         TO   ZTIMIMG06-UNAM,
               SY-DATUM         TO   ZTIMIMG06-UDAT.
        UPDATE ZTIMIMG06.
        IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
        PERFORM   P3000_ZTIMIMG06_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG06-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG06 TO  ZTIMIMG06.
      MOVE : SY-UNAME  TO   ZTIMIMG06-ERNAM,
             SY-DATUM  TO   ZTIMIMG06-CDAT,
             SY-UNAME  TO   ZTIMIMG06-UNAM,
             SY-DATUM  TO   ZTIMIMG06-UDAT.

      INSERT ZTIMIMG06.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
      CLEAR : *ZTIMIMG06.
      PERFORM   P3000_ZTIMIMG06_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG06
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG07
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG07.

  REFRESH : IT_ZSIMIMG07, IT_ZSIMIMG07_DEL.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG07
                                               FROM ZTIMIMG07.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG07'.
  ENDIF.

*  SORT IT_ZSIMIMG07 BY BUKRS ZFCUT ZFPONC ZFITKD ZFAPLDT.

  IT_ZSIMIMG07_ORG[] = IT_ZSIMIMG07[].

ENDFORM.                    " P1000_READ_ZTIMIMG07
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG24
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG24.

  REFRESH : IT_ZSIMIMG24, IT_ZSIMIMG24_DEL.

* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG24
                                               FROM ZTIMIMG24.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG24'.
  ENDIF.
  LOOP AT IT_ZSIMIMG24.
    SELECT SINGLE MAKTX
             INTO IT_ZSIMIMG24-MAKTX
             FROM MAKT
            WHERE MATNR = IT_ZSIMIMG24-MATNR
              AND SPRAS = SY-LANGU.
    MODIFY IT_ZSIMIMG24 INDEX SY-TABIX.
  ENDLOOP.
  IT_ZSIMIMG24_ORG[] = IT_ZSIMIMG24[].

ENDFORM.                    " P1000_READ_ZTIMIMG24
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG07
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG07.
  CLEAR : W_ZFAPLDT, W_ZFCUT, W_ZFSTDOC, W_ZFSTAMT, W_ZFSTHS, W_ZFADAMT.

*>> Internal Table Sort
  SORT IT_ZSIMIMG07  BY ZFCUT ZFENTP ZFAPLDT.

*>> Internal Table Looping( Verify Duplication )
  LOOP AT IT_ZSIMIMG07.
    IF W_BUKRS   EQ IT_ZSIMIMG07-BUKRS   AND
       W_ZFCUT   EQ IT_ZSIMIMG07-ZFCUT   AND
       W_ZFENTP  EQ IT_ZSIMIMG07-ZFENTP  AND
       W_ZFAPLDT EQ IT_ZSIMIMG07-ZFAPLDT.
      MESSAGE E972 WITH W_ZFCUT W_ZFAPLDT.
    ENDIF.

    MOVE: IT_ZSIMIMG07-BUKRS   TO W_BUKRS,
          IT_ZSIMIMG07-ZFCUT   TO W_ZFCUT,
          IT_ZSIMIMG07-ZFENTP  TO W_ZFENTP,
          IT_ZSIMIMG07-ZFAPLDT TO W_ZFAPLDT,
          SY-TABIX             TO W_TABIX.

*>> Duplicate Error Check
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG07-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG07_ORG WITH KEY
                         BUKRS   = IT_ZSIMIMG07-BUKRS
                         ZFCUT   = IT_ZSIMIMG07-ZFCUT
                         ZFENTP  = IT_ZSIMIMG07-ZFENTP
                         ZFAPLDT = IT_ZSIMIMG07-ZFAPLDT.
        IF SY-SUBRC EQ 0.
          MESSAGE E972 WITH W_ZFCUT W_ZFENTP W_ZFAPLDT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG07_DEL.
    DELETE FROM ZTIMIMG07
           WHERE BUKRS   EQ IT_ZSIMIMG07_DEL-BUKRS
           AND   ZFCUT   EQ IT_ZSIMIMG07_DEL-ZFCUT
           AND   ZFENTP  EQ IT_ZSIMIMG07_DEL-ZFENTP
           AND   ZFAPLDT EQ IT_ZSIMIMG07_DEL-ZFAPLDT.

*>> Change Document..
    CLEAR : *ZTIMIMG07.
    MOVE-CORRESPONDING IT_ZSIMIMG07_DEL TO  ZTIMIMG07.
    PERFORM   P3000_ZTIMIMG07_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG07_DEL.

*>> Insert Data..
  LOOP AT IT_ZSIMIMG07.
    SELECT SINGLE * FROM ZTIMIMG07
                    WHERE BUKRS   EQ IT_ZSIMIMG07-BUKRS
                    AND   ZFCUT   EQ IT_ZSIMIMG07-ZFCUT
                    AND   ZFENTP  EQ IT_ZSIMIMG07-ZFENTP
                    AND   ZFAPLDT EQ IT_ZSIMIMG07-ZFAPLDT.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG07-ZFSTAMT  EQ ZTIMIMG07-ZFSTAMT AND
               IT_ZSIMIMG07-ZFSTHS   EQ ZTIMIMG07-ZFSTHS  AND
               IT_ZSIMIMG07-ZFENTP   EQ ZTIMIMG07-ZFENTP  AND
               IT_ZSIMIMG07-ZFADAMT  EQ ZTIMIMG07-ZFADAMT ).
        MOVE-CORRESPONDING ZTIMIMG07    TO *ZTIMIMG07.
        MOVE-CORRESPONDING IT_ZSIMIMG07 TO  ZTIMIMG07.
        MOVE : SY-UNAME         TO   ZTIMIMG07-UNAM,
               SY-DATUM         TO   ZTIMIMG07-UDAT.
        UPDATE ZTIMIMG07.

        IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

*>> Change Document..
        PERFORM   P3000_ZTIMIMG07_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG07-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG07 TO ZTIMIMG07.
      MOVE : SY-UNAME  TO   ZTIMIMG07-CNAM,
             SY-DATUM  TO   ZTIMIMG07-CDAT,
             SY-UNAME  TO   ZTIMIMG07-UNAM,
             SY-DATUM  TO   ZTIMIMG07-UDAT.
      INSERT ZTIMIMG07.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

*>> Change Document..
      CLEAR : *ZTIMIMG07.
      PERFORM   P3000_ZTIMIMG07_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG07
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG24
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG24.
  CLEAR : W_MATNR.

*>> Internal Table Sort
  SORT IT_ZSIMIMG24  BY MATNR ZFMID.

*>> Internal Table Looping( Verify Duplication )
  LOOP AT IT_ZSIMIMG24.
    IF W_MATNR   EQ IT_ZSIMIMG24-MATNR AND
       W_ZFMID   EQ IT_ZSIMIMG24-ZFMID .
       MESSAGE E972 WITH W_MATNR.
    ENDIF.

    MOVE: IT_ZSIMIMG24-MATNR   TO W_MATNR,
          IT_ZSIMIMG24-ZFMID   TO W_ZFMID,
          SY-TABIX             TO W_TABIX.

*>> New Entry -> DB Check( Duplicate Error )
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG24-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG24_ORG WITH KEY
                         MATNR   = IT_ZSIMIMG24-MATNR
                         ZFMID   = IT_ZSIMIMG24-ZFMID.

        IF SY-SUBRC EQ 0.
          MESSAGE E972 WITH W_MATNR.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG24_DEL.
    DELETE FROM ZTIMIMG24
           WHERE MATNR   EQ IT_ZSIMIMG24_DEL-MATNR
           AND   ZFMID   EQ IT_ZSIMIMG24_DEL-ZFMID.

*>> Change Document..
    CLEAR : *ZTIMIMG24.
    MOVE-CORRESPONDING IT_ZSIMIMG24_DEL TO  ZTIMIMG24.
*    PERFORM   P3000_ZTIMIMG24_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG24_DEL.

*>> Insert Data..
  LOOP AT IT_ZSIMIMG24.
    SELECT SINGLE * FROM ZTIMIMG24
                    WHERE MATNR   EQ IT_ZSIMIMG24-MATNR
                    AND   ZFMID   EQ IT_ZSIMIMG24-ZFMID.

    IF SY-SUBRC EQ 0.
      MOVE-CORRESPONDING ZTIMIMG24    TO *ZTIMIMG24.
      MOVE-CORRESPONDING IT_ZSIMIMG24 TO  ZTIMIMG24.
      MOVE : SY-UNAME                 TO  ZTIMIMG24-UNAME,
             SY-DATUM                 TO  ZTIMIMG24-UDAT.
      UPDATE ZTIMIMG24.

      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

*>> Change Document..
*        PERFORM   P3000_ZTIMIMG24_CHANGE_DOC        USING  'U'.
    ELSE.
      IF IT_ZSIMIMG24-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG24 TO ZTIMIMG24.
      MOVE : SY-UNAME  TO   ZTIMIMG24-ERNAM,
             SY-DATUM  TO   ZTIMIMG24-CDAT,
             SY-UNAME  TO   ZTIMIMG24-UNAME,
             SY-DATUM  TO   ZTIMIMG24-UDAT.
      INSERT ZTIMIMG24.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

*>> Change Document..
      CLEAR : *ZTIMIMG24.
*      PERFORM   P3000_ZTIMIMG24_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG24

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_DISABLE_MENU
*&---------------------------------------------------------------------*
FORM P2000_SET_DISABLE_MENU.

  IF SY-TCODE EQ 'ZIMGA1'.
    MOVE 'CHDC' TO IT_EXCL-FCODE. APPEND IT_EXCL. " CHANGE DOC.
    MOVE 'DELT' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
    MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
    MOVE 'DELC' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE CANCE
    MOVE 'ANZG' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DISPLAY
    MOVE 'NEWL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " NEW ENTRY
    MOVE 'AEND' TO IT_EXCL-FCODE. APPEND IT_EXCL. " CHANGE
    MOVE 'COPY' TO IT_EXCL-FCODE. APPEND IT_EXCL. " COPY
    MOVE 'REFR' TO IT_EXCL-FCODE. APPEND IT_EXCL. " REFRESH
    SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.
    EXIT.
  ENDIF.

  IF SY-DYNNR EQ '0020' OR SY-DYNNR EQ '0050'.
    MOVE 'EUL1' TO W_PFSTAT.
    SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.
    EXIT.
  ENDIF.

  IF SY-DYNNR EQ '0210'.
    MOVE 'EUL1'  TO  W_PFSTAT.
    MOVE 'CHDC'  TO  IT_EXCL-FCODE. APPEND IT_EXCL.
    SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.
    EXIT.
  ENDIF.

  IF SY-DYNNR EQ '0220'.
    MOVE 'EUL1'  TO  W_PFSTAT.
    MOVE 'CHDC'  TO  IT_EXCL-FCODE. APPEND IT_EXCL.
    SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.
    EXIT.
  ENDIF.

  IF SY-DYNNR EQ '0080'. ">>¼ö¼Û¿äÀ² (JSY20020911)
    MOVE 'EUL1' TO W_PFSTAT.
    MOVE 'CHDC' TO IT_EXCL-FCODE. APPEND IT_EXCL.
    SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.
    EXIT.
  ENDIF.
  IF SY-TCODE NE 'ZIMG24N'.
    MOVE 'XLS'    TO IT_EXCL-FCODE. APPEND IT_EXCL. " EXCEL DOWNLOAD
  ENDIF.
  IF W_STATUS EQ 'D'.
    IF SY-TCODE NE 'ZIMG24N'.
      MOVE 'XLS'    TO IT_EXCL-FCODE. APPEND IT_EXCL. " EXCEL DOWNLOAD
    ENDIF.
    IF SY-TCODE EQ 'ZIMG25'.
      MOVE 'AEND' TO IT_EXCL-FCODE. APPEND IT_EXCL. " CHANGE
      MOVE 'ANZG' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DISPLAY
      MOVE 'MKAL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " SELECT ALL
      MOVE 'MKLO' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DESELECT
      MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
      MOVE 'DELC' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE CANCE
*     MOVE 'SAVE' TO IT_EXCL-FCODE. APPEND IT_EXCL. " SAVE
      MOVE 'COPY' TO IT_EXCL-FCODE. APPEND IT_EXCL. " COPY
    ELSE.
      MOVE 'DELT' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
      MOVE 'ANZG' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DISPLAY
      MOVE 'MKAL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " SELECT ALL
      MOVE 'MKLO' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DESELECT
      MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
      MOVE 'DELC' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE CANCE
      MOVE 'SAVE' TO IT_EXCL-FCODE. APPEND IT_EXCL. " SAVE
      MOVE 'COPY' TO IT_EXCL-FCODE. APPEND IT_EXCL. " COPY
      MOVE 'REFR' TO IT_EXCL-FCODE. APPEND IT_EXCL. " REFRESH
      MOVE 'PRST' TO IT_EXCL-FCODE. APPEND IT_EXCL. " PRINT
    ENDIF.
    IF SY-TCODE EQ 'ZIMG02' OR SY-TCODE EQ 'ZIMG03' OR
       SY-TCODE EQ 'ZIMG04' OR SY-TCODE EQ 'ZIMG05' OR
       SY-TCODE EQ 'ZIMG08' OR
       SY-TCODE EQ 'ZIMG21' OR SY-TCODE EQ 'ZIMG22'.

      MOVE 'NEWL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " NEW ENTRY
    ENDIF.

    IF SY-TCODE EQ 'ZIMG06' OR SY-TCODE EQ 'ZIMG08' OR
       SY-TCODE EQ 'ZIMG21' OR SY-TCODE EQ 'ZIMG20' OR
       SY-TCODE EQ 'ZIMG22'.
      MOVE 'CHDC' TO IT_EXCL-FCODE. APPEND IT_EXCL.
    ENDIF.
  ELSEIF W_STATUS EQ 'C'.
    MOVE 'XLS'    TO IT_EXCL-FCODE. APPEND IT_EXCL. " EXCEL DOWNLOAD
    IF SY-TCODE EQ 'ZIMG02' OR SY-TCODE EQ 'ZIMG03' OR
       SY-TCODE EQ 'ZIMG04' OR SY-TCODE EQ 'ZIMG05' OR
       SY-TCODE EQ 'ZIMG08' OR
*      SY-TCODE EQ 'ZIMG20' OR
       SY-TCODE EQ 'ZIMG21' OR SY-TCODE EQ 'ZIMG22'.

      MOVE 'DELT' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
      MOVE 'MKAL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " SELECT ALL
      MOVE 'MKLO' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DESELECT
      MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
      MOVE 'DELC' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DEL CANCEL
    ENDIF.
    MOVE 'COPY' TO IT_EXCL-FCODE. APPEND IT_EXCL.
    MOVE 'NEWL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " NEW ENTRY
    MOVE 'AEND' TO IT_EXCL-FCODE. APPEND IT_EXCL.   " CHANGE
    MOVE 'REFR' TO IT_EXCL-FCODE. APPEND IT_EXCL. " REFRESH
    IF SY-TCODE EQ 'ZIMG06' OR SY-TCODE EQ 'ZIMG08' OR
       SY-TCODE EQ 'ZIMG21' OR
       SY-TCODE EQ 'ZIMG20' OR
       SY-TCODE EQ 'ZIMG22'.
      MOVE 'CHDC' TO IT_EXCL-FCODE. APPEND IT_EXCL.
      MOVE 'COPY' TO IT_EXCL-FCODE. APPEND IT_EXCL.
    ENDIF.
  ELSEIF W_STATUS EQ 'I'.
    MOVE 'XLS'    TO IT_EXCL-FCODE. APPEND IT_EXCL. " EXCEL DOWNLOAD
    IF SY-TCODE NE 'ZIMGC1'.
      MOVE 'COPY' TO IT_EXCL-FCODE. APPEND IT_EXCL.
    ENDIF.
*     MOVE 'DELT' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
    MOVE 'NEWL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " NEW ENTRY
    MOVE 'AEND' TO IT_EXCL-FCODE. APPEND IT_EXCL. " CHANGE
    MOVE 'REFR' TO IT_EXCL-FCODE. APPEND IT_EXCL. " REFRESH
    MOVE 'CHDC' TO IT_EXCL-FCODE. APPEND IT_EXCL. " CHANGE DOC.
    MOVE 'DELT' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
    IF  SY-TCODE EQ 'ZIMG25'.
      MOVE 'DELT' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE.
    ENDIF.
    IF  SY-TCODE EQ 'ZIMG08' OR
*       SY-TCODE EQ 'ZIMG20' OR
        SY-TCODE EQ 'ZIMG21' OR SY-TCODE EQ 'ZIMG22'.
      MOVE 'DELT' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
      MOVE 'MKAL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " SELECT ALL
      MOVE 'MKLO' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DESELECT
      MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
      MOVE 'DELC' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DEL CANCEL
    ENDIF.
  ENDIF.
*-----------------------------------------------------------------------
* PF-STATUS SETTING
*-----------------------------------------------------------------------
  SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.

ENDFORM.                    " P2000_SET_DISABLE_MENU
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_GUI_TITLE
*&---------------------------------------------------------------------*
FORM P2000_SET_GUI_TITLE.
*-----------------------------------------------------------------------
* GUI TITLE SETTING
*-----------------------------------------------------------------------
  CASE SY-TCODE.
    WHEN 'ZIMG01'.   SET TITLEBAR  '0100' WITH <FS_F>.
    WHEN 'ZIMG03'.   SET TITLEBAR  '0300' WITH <FS_F>.
    WHEN 'ZIMG04'.   SET TITLEBAR  '0400' WITH <FS_F>.
    WHEN 'ZIMG10'.   SET TITLEBAR  '1000' WITH <FS_F>.
    WHEN 'ZIMG11'.   SET TITLEBAR  '1100' WITH <FS_F>.
    WHEN 'ZIMG12'.   SET TITLEBAR  '1200' WITH <FS_F>.
    WHEN 'ZIMG14'.   SET TITLEBAR  '1400' WITH <FS_F>.
    WHEN 'ZIMG15'.   SET TITLEBAR  '1500' WITH <FS_F>.
    WHEN 'ZIMG16'.   SET TITLEBAR  '1600' WITH <FS_F>.
    WHEN 'ZIMG17'.   SET TITLEBAR  '1700' WITH <FS_F>.
    WHEN 'ZIMG20'.   SET TITLEBAR  '2000' WITH <FS_F>.
    WHEN 'ZIMG22'.   SET TITLEBAR  '2200' WITH <FS_F>.
    WHEN 'ZIMG24'.   SET TITLEBAR  '2300' WITH <FS_F>.
    WHEN 'ZIMG25'.   SET TITLEBAR  '2500' WITH <FS_F>.
    WHEN 'ZIMG24N'.  SET TITLEBAR  '2400' WITH <FS_F>.
    WHEN 'ZIMGC1'.   SET TITLEBAR  '9100' WITH <FS_F>.
    WHEN 'ZIMGC2'.   SET TITLEBAR  '9200' WITH <FS_F>.
    WHEN 'ZIMGC4'.   SET TITLEBAR  '9301' WITH <FS_F>.
    WHEN 'ZEXZ23'.   SET TITLEBAR  '9302' WITH <FS_F>.
    WHEN 'ZIMG06'.   SET TITLEBAR  '0600' WITH <FS_F>.
    WHEN 'ZIMG02'.
      IF SY-DYNNR EQ '0020'.
        SET TITLEBAR  '0200' WITH 'Initial Screen'.
      ELSE.
        SET TITLEBAR  '0200' WITH <FS_F>.
      ENDIF.
    WHEN 'ZIMG05'.
      IF SY-DYNNR EQ '0050'.
        SET TITLEBAR  '0500' WITH 'Initial Screen'.
      ELSE.
        SET TITLEBAR  '0500' WITH <FS_F>.
      ENDIF.
    WHEN 'ZIMG08'.
      IF SY-DYNNR EQ '0080'.
        SET TITLEBAR  '0800' WITH 'Initial Screen'.
      ELSE.
        SET TITLEBAR  '0800' WITH <FS_F>.
      ENDIF.
    WHEN 'ZIMG21'.
      IF SY-DYNNR EQ '0210'.
        SET TITLEBAR  '2100' WITH 'Initial Screen'.
      ELSE.
        SET TITLEBAR  '2100' WITH <FS_F>.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_GUI_TITLE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG08
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG08.
  REFRESH IT_ZSIMIMG08_DEL.
  REFRESH : IT_ZSIMIMG08, IT_ZSIMIMG08_DEL.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG08
                                        FROM ZTIMIMG08
                                        WHERE ZFCDTY EQ W_ZFCDTY
                                        ORDER BY ZFCD.

  IF SY-SUBRC NE 0.
    MESSAGE S967 WITH 'ZTIMIMG08'.
  ENDIF.

  CASE W_ZFCDTY.
    WHEN '002'.
      SORT IT_ZSIMIMG08  BY   ZFCD2 ZFCD.
    WHEN '004' OR '005'.
      SORT IT_ZSIMIMG08  BY   ZFCD4 ZFCD.
    WHEN '998'.
      SORT IT_ZSIMIMG08  BY   ZFCD3 ZFCD.
    WHEN OTHERS.
      SORT IT_ZSIMIMG08  BY   ZFCD  ZFCD1 ZFCD2 ZFCD3 ZFCD4 ZFCD5.
  ENDCASE.

  IT_ZSIMIMG08_ORG[] = IT_ZSIMIMG08[].

ENDFORM.                    " P1000_READ_ZTIMIMG08
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG08
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG08.
  CLEAR : W_ZFCD, W_ZFCD5.

* Internal Table Sort
  SORT IT_ZSIMIMG08  BY ZFCD.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSIMIMG08.
    IF W_ZFCD  EQ IT_ZSIMIMG08-ZFCD.
       MESSAGE E969 WITH W_ZFCD.
    ENDIF.
    IF IT_ZSIMIMG08-ZFCDTY EQ '010'.
       IF W_ZFCD5 NE SPACE  AND W_ZFCD5 EQ IT_ZSIMIMG08-ZFCD5.
          MESSAGE E969 WITH W_ZFCD5.
       ENDIF.
    ENDIF.

    MOVE : IT_ZSIMIMG08-ZFCD  TO W_ZFCD,
           IT_ZSIMIMG08-ZFCD5 TO W_ZFCD5,
           SY-TABIX           TO W_TABIX.

* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG08-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG08_ORG WITH KEY
                         ZFCDTY = W_ZFCDTY
                         ZFCD   = IT_ZSIMIMG08-ZFCD.
        IF SY-SUBRC EQ 0.
          MESSAGE E969 WITH W_ZFCD.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG08_DEL.
    DELETE FROM ZTIMIMG08
           WHERE ZFCDTY  EQ W_ZFCDTY
           AND   ZFCD    EQ IT_ZSIMIMG08_DEL-ZFCD.

* Change Document..
    CLEAR : *ZTIMIMG08.
    MOVE-CORRESPONDING IT_ZSIMIMG08_DEL TO  ZTIMIMG08.
    MOVE : W_ZFCDTY                     TO  ZTIMIMG08-ZFCDTY.
    PERFORM   P3000_ZTIMIMG08_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG08_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG08.
    SELECT SINGLE * FROM ZTIMIMG08
                    WHERE ZFCDTY  EQ  W_ZFCDTY
                    AND   ZFCD    EQ IT_ZSIMIMG08-ZFCD.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG08-ZFCDNM    EQ  ZTIMIMG08-ZFCDNM
         AND   IT_ZSIMIMG08-ZFCD1     EQ  ZTIMIMG08-ZFCD1
         AND   IT_ZSIMIMG08-ZFCD2     EQ  ZTIMIMG08-ZFCD2
         AND   IT_ZSIMIMG08-ZFCD3     EQ  ZTIMIMG08-ZFCD3
         AND   IT_ZSIMIMG08-ZFCD4     EQ  ZTIMIMG08-ZFCD4
         AND   IT_ZSIMIMG08-ZFCD5     EQ  ZTIMIMG08-ZFCD5
         AND   IT_ZSIMIMG08-COND_TYPE EQ  ZTIMIMG08-COND_TYPE
         AND   IT_ZSIMIMG08-KTOSL     EQ  ZTIMIMG08-KTOSL
         AND   IT_ZSIMIMG08-ZFIOCAC   EQ  ZTIMIMG08-ZFIOCAC
         AND   IT_ZSIMIMG08-ZFMRATE   EQ  ZTIMIMG08-ZFMRATE
         AND   IT_ZSIMIMG08-ZFCURR    EQ  ZTIMIMG08-ZFCURR
         AND   IT_ZSIMIMG08-BLART     EQ  ZTIMIMG08-BLART
         AND   IT_ZSIMIMG08-ZFMNAMT   EQ  ZTIMIMG08-ZFMNAMT
         AND   IT_ZSIMIMG08-ZFMXAMT   EQ  ZTIMIMG08-ZFMXAMT
         AND   IT_ZSIMIMG08-WAERS     EQ  ZTIMIMG08-WAERS ).

         MOVE-CORRESPONDING ZTIMIMG08    TO *ZTIMIMG08.
         MOVE-CORRESPONDING IT_ZSIMIMG08 TO ZTIMIMG08.
         MOVE : SY-UNAME         TO   ZTIMIMG08-UNAM,
                SY-DATUM         TO   ZTIMIMG08-UDAT,
                W_ZFCDTY         TO   ZTIMIMG08-ZFCDTY.
         UPDATE ZTIMIMG08.
         IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
         PERFORM   P3000_ZTIMIMG08_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG08-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG08 TO ZTIMIMG08.
      MOVE : SY-UNAME  TO   ZTIMIMG08-ERNAM,
             SY-DATUM  TO   ZTIMIMG08-CDAT,
             SY-UNAME  TO   ZTIMIMG08-UNAM,
             SY-DATUM  TO   ZTIMIMG08-UDAT,
             W_ZFCDTY  TO   ZTIMIMG08-ZFCDTY.

      INSERT ZTIMIMG08.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
      CLEAR : *ZTIMIMG08.
      PERFORM   P3000_ZTIMIMG08_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG08
*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_DELETE
*&---------------------------------------------------------------------*
FORM P2000_DATA_DELETE.
  CASE SY-TCODE.
    WHEN 'ZIMG01'.    " Terms of Payment
      LOOP AT IT_ZSIMIMG01 WHERE ZFMARK = 'X'.
*-----------------------------------------------------------------------
* Refrenced Import Request Data Confirm.
        IF W_STATUS NE 'I'.
          CLEAR : W_CNT.
          SELECT COUNT( * ) INTO W_CNT FROM ZTREQHD
                            WHERE ZTERM EQ IT_ZSIMIMG01-ZTERM.
          IF W_CNT GT 0.
            MESSAGE E232 WITH W_CNT.
          ENDIF.
          IT_ZSIMIMG01_DEL = IT_ZSIMIMG01.
          APPEND IT_ZSIMIMG01_DEL.
        ENDIF.
*-----------------------------------------------------------------------
        DELETE  IT_ZSIMIMG01.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG10'.        " Broker/Vendor Match
      LOOP AT IT_ZSIMIMG10 WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG10_DEL = IT_ZSIMIMG10.
          APPEND IT_ZSIMIMG10_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG10.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG11'.
      LOOP AT IT_ZSIMIMG06 WHERE ZFMARK = 'X'.
*-----------------------------------------------------------------------
* Exchange Rate Management Table Not Exist
* Desc : Local Currency Amount Convert
*-----------------------------------------------------------------------
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG06_DEL = IT_ZSIMIMG06.
          APPEND IT_ZSIMIMG06_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG06.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG12'.        "  Planned Cost Rate
      LOOP AT IT_ZSIMIMG04 WHERE ZFMARK = 'X'.
*-----------------------------------------------------------------------
*  Planned Cost Rate
* Desc : [Report] Invoice Processing - Expense Dis, I/V, G/R
*-----------------------------------------------------------------------
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG04_DEL = IT_ZSIMIMG04.
          APPEND IT_ZSIMIMG04_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG04.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG14'.
      LOOP AT IT_ZSIMIMG07 WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG07_DEL = IT_ZSIMIMG07.
          APPEND IT_ZSIMIMG07_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG07.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG15'.
      LOOP AT IT_ZSIMIMG05 WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG05_DEL = IT_ZSIMIMG05.
          APPEND IT_ZSIMIMG05_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG05.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG16'.
      LOOP AT IT_ZSIMIMG09 WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG09_DEL = IT_ZSIMIMG09.
          APPEND IT_ZSIMIMG09_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG09.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG17'.
      LOOP AT IT_ZSIMIMG17 WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG17_DEL = IT_ZSIMIMG17.
          APPEND IT_ZSIMIMG17_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG17.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG20'.     " Inland Freight
      LOOP AT IT_ZSIMIMG20 WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG20_DEL = IT_ZSIMIMG20.
          APPEND IT_ZSIMIMG20_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG20.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMG21'.
      LOOP AT IT_ZSIMIMG21 WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG21_DEL = IT_ZSIMIMG21.
          APPEND IT_ZSIMIMG21_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG21.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.
    WHEN 'ZIMG24'.   " Manage Port by Country
      LOOP AT IT_ZSIEPORT WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          IT_ZSIEPORT_DEL = IT_ZSIEPORT.
          APPEND IT_ZSIEPORT_DEL.
        ENDIF.
        DELETE  IT_ZSIEPORT.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.
*-----------------------------------------------------------------------
* Port/Freight applicable area match code
*-----------------------------------------------------------------------
    WHEN 'ZIMG25'.     "Port/Freight applicable area match code
      LOOP AT IT_ZSIMIMG23 WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG23_DEL = IT_ZSIMIMG23.
          APPEND IT_ZSIMIMG23_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG23.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.
    WHEN 'ZIMG24N'.       " Broker Fee..
      LOOP AT IT_ZSIMIMG24 WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG24_DEL = IT_ZSIMIMG24.
          APPEND IT_ZSIMIMG24_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG24.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.
    WHEN 'ZIMGC1'.
      LOOP AT IT_ZSIMIMG02 WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG02_DEL = IT_ZSIMIMG02.
          APPEND IT_ZSIMIMG02_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG02.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMGC2'.
      LOOP AT IT_ZSIMIMG03 WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          PERFORM  P2000_BONDED_AREA_CHK
                              USING     IT_ZSIMIMG03-ZFBNARCD
                                        W_TEXT40
                                        W_CNT.
          IF W_CNT GT 0.
            MESSAGE E233 WITH IT_ZSIMIMG03-ZFBNARCD W_TEXT40 W_CNT.
          ENDIF.
          IT_ZSIMIMG03_DEL = IT_ZSIMIMG03.
          APPEND IT_ZSIMIMG03_DEL.
        ENDIF.
*-----------------------------------------------------------------------
        DELETE  IT_ZSIMIMG03.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN 'ZIMGC3' OR 'ZIMGC4' OR 'ZEXZ23'.
      LOOP AT IT_ZSIMIMG08 WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          PERFORM  P2000_GET_OTEHRS_CODE
                              CHANGING IT_ZSIMIMG08-ZFCDTY
                                       IT_ZSIMIMG08-ZFCD
                                        W_TEXT40    W_CNT.
          IF W_CNT GT 0.
            MESSAGE E235 WITH IT_ZSIMIMG08-ZFCD W_TEXT40 W_CNT.
          ENDIF.
          IT_ZSIMIMG08_DEL = IT_ZSIMIMG08.
          APPEND IT_ZSIMIMG08_DEL.
        ENDIF.
*-----------------------------------------------------------------------
        DELETE  IT_ZSIMIMG08.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.
    WHEN 'ZIMG06'.
      LOOP AT IT_ZSIMIMG12 WHERE ZFMARK = 'X'.
        IT_ZSIMIMG12_DEL = IT_ZSIMIMG12.
        APPEND IT_ZSIMIMG12_DEL.
*-----------------------------------------------------------------------
        DELETE  IT_ZSIMIMG12.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_DATA_DELETE
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG02
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG02.
  CLEAR : W_ZFCOTM,  W_ZFWERKS.

* Internal Table Sort
  SORT IT_ZSIMIMG02  BY ZFCOTM ZFWERKS.

* Internal Table Looping( Duplicate Check )
  LOOP AT IT_ZSIMIMG02.

    IF W_ZFCOTM  EQ IT_ZSIMIMG02-ZFCOTM.
      IF W_ZFWERKS EQ IT_ZSIMIMG02-ZFWERKS.
        MESSAGE E956 WITH W_ZFCOTM W_ZFWERKS.
      ENDIF.
    ENDIF.

    MOVE : IT_ZSIMIMG02-ZFCOTM  TO W_ZFCOTM,
           IT_ZSIMIMG02-ZFWERKS TO W_ZFWERKS,
           SY-TABIX             TO W_TABIX.

* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG02-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG02_ORG WITH KEY
                        ZFCOTM  = IT_ZSIMIMG02-ZFCOTM
                        ZFWERKS = IT_ZSIMIMG02-ZFWERKS.
        IF SY-SUBRC EQ 0.
          MESSAGE E956 WITH W_ZFCOTM W_ZFWERKS.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG02_DEL.
    DELETE FROM ZTIMIMG02
           WHERE ZFCOTM  EQ IT_ZSIMIMG02_DEL-ZFCOTM
           AND   ZFWERKS EQ IT_ZSIMIMG02_DEL-ZFWERKS.

* Change Document..
    CLEAR : *ZTIMIMG02.
    MOVE-CORRESPONDING IT_ZSIMIMG02_DEL TO  ZTIMIMG02.
    PERFORM   P3000_ZTIMIMG02_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG02_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG02.
    SELECT SINGLE * FROM ZTIMIMG02
                    WHERE ZFCOTM   EQ IT_ZSIMIMG02-ZFCOTM
                    AND   ZFWERKS  EQ IT_ZSIMIMG02-ZFWERKS.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG02-ZFVEN    EQ ZTIMIMG02-ZFVEN
         AND   IT_ZSIMIMG02-ZFTDNO   EQ ZTIMIMG02-ZFTDNO
         AND   IT_ZSIMIMG02-ZFTDNM1  EQ ZTIMIMG02-ZFTDNM1
         AND   IT_ZSIMIMG02-ZFTDAD1  EQ ZTIMIMG02-ZFTDAD1
         AND   IT_ZSIMIMG02-ZFTDAD2  EQ ZTIMIMG02-ZFTDAD2
         AND   IT_ZSIMIMG02-ZFTDTC   EQ ZTIMIMG02-ZFTDTC
         AND   IT_ZSIMIMG02-ZFACTNO  EQ ZTIMIMG02-ZFACTNO ).
        MOVE-CORRESPONDING ZTIMIMG02    TO *ZTIMIMG02.
        MOVE-CORRESPONDING IT_ZSIMIMG02 TO  ZTIMIMG02.
        MOVE : SY-UNAME         TO   ZTIMIMG02-UNAM,
               SY-DATUM         TO   ZTIMIMG02-UDAT.
        UPDATE ZTIMIMG02.
        IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
        PERFORM   P3000_ZTIMIMG02_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG02-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG02 TO  ZTIMIMG02.
      MOVE : SY-UNAME  TO   ZTIMIMG02-ERNAM,
             SY-DATUM  TO   ZTIMIMG02-CDAT,
             SY-UNAME  TO   ZTIMIMG02-UNAM,
             SY-DATUM  TO   ZTIMIMG02-UDAT.
      INSERT ZTIMIMG02.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
      PERFORM   P3000_ZTIMIMG02_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG02
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG02
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG02.

  REFRESH: IT_ZSIMIMG02, IT_ZSIMIMG02_ORG, IT_ZSIMIMG02_DEL.

* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG02
                                               FROM ZTIMIMG02
            ORDER BY ZFCOTM.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG02'.
  ENDIF.

  LOOP AT IT_ZSIMIMG02.
    CLEAR : IT_ZSIMIMG02-NAME1.
    SELECT SINGLE NAME1 INTO IT_ZSIMIMG02-NAME1 FROM LFA1
                        WHERE LIFNR EQ IT_ZSIMIMG02-ZFVEN.
    MODIFY IT_ZSIMIMG02.
  ENDLOOP.

  IT_ZSIMIMG02_ORG[] = IT_ZSIMIMG02[].

ENDFORM.                    " P1000_READ_ZTIMIMG02
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG03
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG03.

  REFRESH : IT_ZSIMIMG03, IT_ZSIMIMG03_DEL.

* Table Multi-Select
  SELECT  *
  INTO    CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG03
  FROM    ZTIMIMG03 AS H LEFT OUTER JOIN LFA1  AS  I
  ON      H~LIFNR   EQ  I~LIFNR
  ORDER BY
          H~ZFBNAR .
  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG03'.
  ENDIF.

  IT_ZSIMIMG03_ORG[] = IT_ZSIMIMG03[].

ENDFORM.                    " P1000_READ_ZTIMIMG03
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG03
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG03.
  CLEAR : W_ZFBNAR.

* ³»ºÎÄÚµå Áßº¹ °ËÁõ....
  IF W_STATUS EQ 'I'.
    IT_ZSIMIMG03_TMP[] = IT_ZSIMIMG03_ORG[].
    LOOP AT IT_ZSIMIMG03.
      MOVE-CORRESPONDING  IT_ZSIMIMG03  TO   IT_ZSIMIMG03_TMP.
      APPEND  IT_ZSIMIMG03_TMP.
    ENDLOOP.
  ELSE.
    IT_ZSIMIMG03_TMP[] = IT_ZSIMIMG03[].
  ENDIF.
  SORT IT_ZSIMIMG03_TMP  BY ZFBNARCD.
  CLEAR : W_ZFBNARCD.
  LOOP AT IT_ZSIMIMG03_TMP.
    IF W_ZFBNARCD EQ IT_ZSIMIMG03_TMP-ZFBNARCD.
      MESSAGE E976 WITH W_ZFBNARCD.
    ENDIF.
    W_ZFBNARCD = IT_ZSIMIMG03_TMP-ZFBNARCD.
  ENDLOOP.

* Internal Table Sort
  SORT IT_ZSIMIMG03  BY ZFBNAR.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSIMIMG03.
    IF W_ZFBNAR  EQ IT_ZSIMIMG03-ZFBNAR.
      MESSAGE E969 WITH W_ZFBNAR.
    ENDIF.

    MOVE : IT_ZSIMIMG03-ZFBNAR  TO W_ZFBNAR,
           SY-TABIX             TO W_TABIX.

* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG03-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG03_ORG WITH KEY
                        ZFBNAR = IT_ZSIMIMG03-ZFBNAR.
        IF SY-SUBRC EQ 0.   MESSAGE E969 WITH W_ZFBNAR.   ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG03_DEL.
    DELETE FROM ZTIMIMG03
           WHERE ZFBNAR  EQ IT_ZSIMIMG03_DEL-ZFBNAR.

* Change Document..
    CLEAR : *ZTIMIMG03.
    MOVE-CORRESPONDING IT_ZSIMIMG03_DEL TO  ZTIMIMG03.
    PERFORM   P3000_ZTIMIMG03_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG03_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG03.
    SELECT SINGLE * FROM ZTIMIMG03
                    WHERE ZFBNAR EQ IT_ZSIMIMG03-ZFBNAR.
    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG03-ZFBNARM  EQ ZTIMIMG03-ZFBNARM
         AND   IT_ZSIMIMG03-ZFCOTM   EQ ZTIMIMG03-ZFCOTM
         AND   IT_ZSIMIMG03-ZFBNARC  EQ ZTIMIMG03-ZFBNARC
         AND   IT_ZSIMIMG03-ZFBNARCD EQ ZTIMIMG03-ZFBNARCD
         AND   IT_ZSIMIMG03-ZFEDIID  EQ ZTIMIMG03-ZFEDIID
         AND   IT_ZSIMIMG03-LIFNR    EQ ZTIMIMG03-LIFNR ).
        MOVE-CORRESPONDING ZTIMIMG03    TO *ZTIMIMG03.
        MOVE-CORRESPONDING IT_ZSIMIMG03 TO ZTIMIMG03.
        MOVE : SY-UNAME         TO   ZTIMIMG03-UNAM,
               SY-DATUM         TO   ZTIMIMG03-UDAT.
        UPDATE ZTIMIMG03.
        IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.
* Change Document..
        PERFORM   P3000_ZTIMIMG03_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG03-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG03 TO ZTIMIMG03.
      MOVE : SY-UNAME  TO   ZTIMIMG03-ERNAM,
             SY-DATUM  TO   ZTIMIMG03-CDAT,
             SY-UNAME  TO   ZTIMIMG03-UNAM,
             SY-DATUM  TO   ZTIMIMG03-UDAT.

      INSERT ZTIMIMG03.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
      CLEAR : *ZTIMIMG03.
      PERFORM   P3000_ZTIMIMG03_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG03
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG05
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG05.

  REFRESH : IT_ZSIMIMG05, IT_ZSIMIMG05_DEL.

* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG05
                                               FROM ZTIMIMG05
            ORDER BY ZFPORT ZFBNARCD ZFTRANS ZFCOSTCD ZFAPLDT.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG05'.
  ENDIF.

  IT_ZSIMIMG05_ORG[] = IT_ZSIMIMG05[].

ENDFORM.                    " P1000_READ_ZTIMIMG05
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG05
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG05.
  CLEAR : W_ZFPORT, W_ZFBNARCD, W_ZFTRANS, W_ZFCOSTCD, W_ZFAPLDT.

* Internal Table Sort
  SORT IT_ZSIMIMG05  BY  ZFPORT ZFBNARCD ZFTRANS ZFCOSTCD ZFAPLDT.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSIMIMG05.
    IF W_ZFPORT   EQ IT_ZSIMIMG05-ZFPORT   AND
       W_ZFBNARCD EQ IT_ZSIMIMG05-ZFBNARCD AND
       W_ZFTRANS  EQ IT_ZSIMIMG05-ZFTRANS  AND
       W_ZFCOSTCD EQ IT_ZSIMIMG05-ZFCOSTCD AND
       W_ZFAPLDT  EQ IT_ZSIMIMG05-ZFAPLDT.

      MESSAGE E972 WITH W_ZFPORT  W_ZFBNARCD
                        W_ZFTRANS W_ZFCOSTCD.
    ENDIF.

    MOVE : IT_ZSIMIMG05-ZFPORT   TO W_ZFPORT,
           IT_ZSIMIMG05-ZFBNARCD TO W_ZFBNARCD,
           IT_ZSIMIMG05-ZFTRANS  TO W_ZFTRANS,
           IT_ZSIMIMG05-ZFCOSTCD TO W_ZFCOSTCD,
           IT_ZSIMIMG05-ZFAPLDT  TO W_ZFAPLDT,
           SY-TABIX              TO W_TABIX.

* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG05-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG05_ORG WITH KEY
                         ZFPORT   = IT_ZSIMIMG05-ZFPORT
                         ZFBNARCD = IT_ZSIMIMG05-ZFBNARCD
                         ZFTRANS  = IT_ZSIMIMG05-ZFTRANS
                         ZFCOSTCD = IT_ZSIMIMG05-ZFCOSTCD
                         ZFAPLDT  = IT_ZSIMIMG05-ZFAPLDT.
        IF SY-SUBRC EQ 0.
          MESSAGE E972 WITH W_ZFPORT  W_ZFBNARCD
                            W_ZFTRANS W_ZFCOSTCD.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG05_DEL.
    DELETE FROM ZTIMIMG05
           WHERE ZFPORT   EQ IT_ZSIMIMG05_DEL-ZFPORT
           AND   ZFBNARCD EQ IT_ZSIMIMG05_DEL-ZFBNARCD
           AND   ZFTRANS  EQ IT_ZSIMIMG05_DEL-ZFTRANS
           AND   ZFCOSTCD EQ IT_ZSIMIMG05_DEL-ZFCOSTCD
           AND   ZFAPLDT  EQ IT_ZSIMIMG05_DEL-ZFAPLDT.

* Change Document..
    CLEAR : *ZTIMIMG05.
    MOVE-CORRESPONDING IT_ZSIMIMG05_DEL TO  ZTIMIMG05.
    PERFORM   P3000_ZTIMIMG05_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG05_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG05.
    SELECT SINGLE * FROM ZTIMIMG05
                    WHERE ZFPORT   EQ IT_ZSIMIMG05-ZFPORT
                    AND   ZFBNARCD EQ IT_ZSIMIMG05-ZFBNARCD
                    AND   ZFTRANS  EQ IT_ZSIMIMG05-ZFTRANS
                    AND   ZFCOSTCD EQ IT_ZSIMIMG05-ZFCOSTCD
                    AND   ZFAPLDT  EQ IT_ZSIMIMG05-ZFAPLDT.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG05-NETPR    EQ ZTIMIMG05-NETPR    AND
               IT_ZSIMIMG05-PEINH    EQ ZTIMIMG05-PEINH    AND
               IT_ZSIMIMG05-BPRME    EQ ZTIMIMG05-BPRME    AND
               IT_ZSIMIMG05-WAERS    EQ ZTIMIMG05-WAERS    AND
               IT_ZSIMIMG05-ZFFIXED  EQ ZTIMIMG05-ZFFIXED  ).
        MOVE-CORRESPONDING ZTIMIMG05    TO *ZTIMIMG05.
        MOVE-CORRESPONDING IT_ZSIMIMG05 TO  ZTIMIMG05.
        MOVE : SY-UNAME         TO   ZTIMIMG05-UNAM,
               SY-DATUM         TO   ZTIMIMG05-UDAT.
        UPDATE ZTIMIMG05.
        IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
        PERFORM   P3000_ZTIMIMG05_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG05-LOEKZ EQ 'X'.      CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG05 TO  ZTIMIMG05.
      MOVE : SY-UNAME  TO   ZTIMIMG05-ERNAM,
             SY-DATUM  TO   ZTIMIMG05-CDAT,
             SY-UNAME  TO   ZTIMIMG05-UNAM,
             SY-DATUM  TO   ZTIMIMG05-UDAT.
      INSERT ZTIMIMG05.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
      CLEAR : *ZTIMIMG05.
      PERFORM   P3000_ZTIMIMG05_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG05
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG09
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG09.

  REFRESH : IT_ZSIMIMG09, IT_ZSIMIMG09_ORG, IT_ZSIMIMG09_DEL.

* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG09
                                               FROM ZTIMIMG09
            ORDER BY  STAWN ZFAPLDT.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG09'.
  ENDIF.

  IT_ZSIMIMG09_ORG[] = IT_ZSIMIMG09[].

ENDFORM.                    " P1000_READ_ZTIMIMG09
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG09
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG09.
  CLEAR : W_WAERS, W_ZFAPLDT.

* Internal Table Sort
  SORT IT_ZSIMIMG09  BY  STAWN ZFAPLDT.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSIMIMG09.
    IF W_STAWN   EQ IT_ZSIMIMG09-STAWN   AND
       W_ZFAPLDT EQ IT_ZSIMIMG09-ZFAPLDT.
      MESSAGE E956 WITH W_STAWN W_ZFAPLDT.
    ENDIF.

    MOVE : IT_ZSIMIMG09-STAWN   TO W_STAWN,
           IT_ZSIMIMG09-ZFAPLDT TO W_ZFAPLDT,
           SY-TABIX             TO W_TABIX.

* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG09-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG09_ORG WITH KEY
                         STAWN   = IT_ZSIMIMG09-STAWN
                         ZFAPLDT = IT_ZSIMIMG09-ZFAPLDT.
        IF SY-SUBRC EQ 0.
          MESSAGE E956 WITH W_STAWN W_ZFAPLDT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG09_DEL.
    DELETE FROM ZTIMIMG09
           WHERE STAWN   EQ IT_ZSIMIMG09_DEL-STAWN
           AND   ZFAPLDT EQ IT_ZSIMIMG09_DEL-ZFAPLDT.

* Change Document..
    CLEAR : *ZTIMIMG09.
    MOVE-CORRESPONDING IT_ZSIMIMG09_DEL TO  ZTIMIMG09.
    PERFORM   P3000_ZTIMIMG09_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG09_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG09.
    SELECT SINGLE * FROM ZTIMIMG09
                    WHERE STAWN   EQ IT_ZSIMIMG09-STAWN
                    AND   ZFAPLDT EQ IT_ZSIMIMG09-ZFAPLDT.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG09-ZFTEXTE  EQ ZTIMIMG09-ZFTEXTE  AND
               IT_ZSIMIMG09-ZFTEXTK  EQ ZTIMIMG09-ZFTEXTK  AND
               IT_ZSIMIMG09-ZFTXCD   EQ ZTIMIMG09-ZFTXCD   AND
               IT_ZSIMIMG09-ZFBASIC  EQ ZTIMIMG09-ZFBASIC  AND
               IT_ZSIMIMG09-ZFEXEC   EQ ZTIMIMG09-ZFEXEC   AND
               IT_ZSIMIMG09-ZFSPECR  EQ ZTIMIMG09-ZFSPECR   AND
               IT_ZSIMIMG09-ZFIRLW   EQ ZTIMIMG09-ZFIRLW   ).
        MOVE-CORRESPONDING ZTIMIMG09    TO *ZTIMIMG09.
        MOVE-CORRESPONDING IT_ZSIMIMG09 TO  ZTIMIMG09.
        MOVE : SY-UNAME         TO   ZTIMIMG09-UNAM,
               SY-DATUM         TO   ZTIMIMG09-UDAT.
        UPDATE ZTIMIMG09.
        IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
        PERFORM   P3000_ZTIMIMG09_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG09-LOEKZ EQ 'X'.  CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG09 TO ZTIMIMG09.
      MOVE : SY-UNAME  TO   ZTIMIMG09-ERNAM,
             SY-DATUM  TO   ZTIMIMG09-CDAT,
             SY-UNAME  TO   ZTIMIMG09-UNAM,
             SY-DATUM  TO   ZTIMIMG09-UDAT.
      INSERT ZTIMIMG09.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
      CLEAR : *ZTIMIMG09.
      PERFORM   P3000_ZTIMIMG09_CHANGE_DOC        USING  'I'.
    ENDIF.

    IF SY-SUBRC NE 0.
      MESSAGE E952.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG09
*&---------------------------------------------------------------------*
*&      Form  SET_CURR_CONV_TO_INTERNAL
*&---------------------------------------------------------------------*
FORM SET_CURR_CONV_TO_INTERNAL USING    P_AMOUNT
                                        P_WAERS.

  BAPICURR-BAPICURR = P_AMOUNT.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
       EXPORTING
            CURRENCY             = P_WAERS
            AMOUNT_EXTERNAL      = BAPICURR-BAPICURR
            MAX_NUMBER_OF_DIGITS = DIGITS
       IMPORTING
            AMOUNT_INTERNAL      = P_AMOUNT
       EXCEPTIONS
            OTHERS               = 1.

ENDFORM.                    " SET_CURR_CONV_TO_INTERNAL
*&---------------------------------------------------------------------*
*&      Form  2000_BACK_SCREEN_DEFINE
*&---------------------------------------------------------------------*
FORM 2000_BACK_SCREEN_DEFINE.
  CASE SY-TCODE.
    WHEN 'ZIMG02'.
      MOVE 'Y' TO W_FIRST_FLAG_0020.
      SET SCREEN 0020.  LEAVE SCREEN.
    WHEN 'ZIMGC3'.        " ±âÅ¸ °ü¸®ÄÚ?
      SET SCREEN 9300.  LEAVE SCREEN.
    WHEN 'ZIMGA1'.        " Åë°üÈ¯?
      PERFORM   SET_V_CURR_LOCK      USING   'U'.
      SET SCREEN 9400.  LEAVE SCREEN.
    WHEN 'ZIMG05'.
      MOVE 'Y' TO W_FIRST_FLAG_0020.
      SET SCREEN 0020. LEAVE SCREEN.
*         SET SCREEN 0050. LEAVE SCREEN.
    WHEN 'ZIMG08'.
      MOVE 'Y' TO W_FIRST_FLAG_0080.
      SET SCREEN 0080. LEAVE SCREEN.
    WHEN 'ZIMG21'.
      MOVE 'Y' TO W_FIRST_FLAG_0210.
      SET SCREEN 0210. LEAVE SCREEN.
    WHEN 'ZIMG22'.
      MOVE 'Y' TO W_FIRST_FLAG_0220.
      SET SCREEN 0220. LEAVE SCREEN.
    WHEN OTHERS.
      SET SCREEN 0.  LEAVE SCREEN.
  ENDCASE.

ENDFORM.                    " 2000_BACK_SCREEN_DEFINE
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG00
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG00    USING     PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG00'
         EXPORTING
              MANDT          = SY-MANDT
         EXCEPTIONS
              FOREIGN_LOCK   = 4
              SYSTEM_FAILURE = 8.
    CASE SY-SUBRC.
      WHEN  4.
        MESSAGE E512
                WITH SY-UNAME  'Client' SY-MANDT ''.
      WHEN  8.    MESSAGE E511.
    ENDCASE.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG00'
         EXPORTING
              MANDT = SY-MANDT.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG00
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_MODIFY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_SET_MODIFY_CHECK   USING    W_GUBUN.

  W_GUBUN = 'Y'.

  CASE SY-TCODE.
    WHEN 'ZIMG01'.                 " Payment Terms
      PERFORM   P2000_ZTIMIMG01_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMG02'.
      IF ZTIMIMGTX EQ *ZTIMIMGTX.
        W_GUBUN = 'N'.
      ENDIF.
    WHEN 'ZIMG03'.
      IF SY-TCODE EQ 'ZIMG04'.  " Number Range Check
        PERFORM   P2000_NUMBER_RANGE_CHECK  USING ''.
      ENDIF.
      IF ZTIMIMG00 EQ *ZTIMIMG00.
        W_GUBUN = 'N'.
      ENDIF.
    WHEN 'ZIMG05'.
      IF ZTIMIMG11 EQ *ZTIMIMG11.
        W_GUBUN = 'N'.
      ENDIF.
    WHEN 'ZIMG06'.
      PERFORM   P2000_ZTIMIMG12_MODIFY_CHK  USING  W_GUBUN.
    WHEN 'ZIMG08'.
      PERFORM   P2000_ZTIMIMG20_MODIFY_CHK  USING  W_GUBUN.
    WHEN 'ZIMG10'.
      PERFORM   P2000_ZTIMIMG10_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMG11'.
      PERFORM   P2000_ZTIMIMG06_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMG12'.
      PERFORM   P2000_ZTIMIMG04_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMG14'.
      PERFORM   P2000_ZTIMIMG07_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMG15'.
      PERFORM   P2000_ZTIMIMG05_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMG16'.
      PERFORM   P2000_ZTIMIMG09_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMG17'.
      PERFORM   P2000_ZTIMIMG17_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMG20'.
      PERFORM   P2000_ZTIMIMG20_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMG21'.
      PERFORM   P2000_ZTIMIMG21_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMG22'.
      PERFORM   P2000_ZTIMIMG22_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMG24'.                 " Manage Port by Country
      PERFORM   P2000_ZTIEPORT_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMG25'.
      PERFORM   P2000_ZTIMIMG23_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMG24N'.                 " Manage Port by Country
      PERFORM   P2000_ZTIMIMG24_MODIFY_CHK  USING  W_GUBUN.
    WHEN 'ZIMGC1'.
      PERFORM   P2000_ZTIMIMG02_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMGC2'.
      PERFORM   P2000_ZTIMIMG03_MODIFY_CHK   USING  W_GUBUN.
    WHEN 'ZIMGC3' OR 'ZIMGC4' OR 'ZEXZ23'.
      PERFORM   P2000_ZTIMIMG08_MODIFY_CHK   USING  W_GUBUN.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_MODIFY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG01_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG01_MODIFY_CHK    USING  W_GUBUN.
  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG01     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG01_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG01_ORG.
        READ TABLE IT_ZSIMIMG01 WITH KEY
                                BSTYP   = IT_ZSIMIMG01_ORG-BSTYP
                                BSART   = IT_ZSIMIMG01_ORG-BSART
                                ZTERM   = IT_ZSIMIMG01_ORG-ZTERM
                                ZFAPLDT = IT_ZSIMIMG01_ORG-ZFAPLDT
                                BINARY SEARCH.

        IF IT_ZSIMIMG01_ORG NE IT_ZSIMIMG01.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG01_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG06_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG06_MODIFY_CHK    USING  W_GUBUN.
  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG06     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG06_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG06_ORG.
        READ TABLE IT_ZSIMIMG06 WITH KEY
                                WAERS   = IT_ZSIMIMG06_ORG-WAERS
                                ZFAPLDT = IT_ZSIMIMG06_ORG-ZFAPLDT
                                BINARY SEARCH.

        IF IT_ZSIMIMG06_ORG NE IT_ZSIMIMG06.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG06_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG04_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG04_MODIFY_CHK   USING  W_GUBUN.
  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG04     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG04_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG04_ORG.
        READ TABLE IT_ZSIMIMG04 WITH KEY
                              ZFWERKS   = IT_ZSIMIMG04_ORG-ZFWERKS
                              ZFMATGB   = IT_ZSIMIMG04_ORG-ZFMATGB
                              ZFAPLDT   = IT_ZSIMIMG04_ORG-ZFAPLDT
                                BINARY SEARCH.

        IF IT_ZSIMIMG04_ORG NE IT_ZSIMIMG04.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG04_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_LOCK_MODE
*&---------------------------------------------------------------------*
FORM P2000_SET_LOCK_MODE USING    PA_MODE.
  CASE SY-TCODE.
    WHEN  'ZIMG01'.                  " payment terms config.
      PERFORM SET_LOCK_ZTIMIMG01     USING  PA_MODE.
    WHEN  'ZIMG06'.                  " payment terms config.
      PERFORM SET_LOCK_ZTIMIMG12     USING  PA_MODE.
    WHEN  'ZIMG10'.       " Vendor/Broker Match code
      PERFORM SET_LOCK_ZTIMIMG10     USING  PA_MODE.
    WHEN  'ZIMG11' OR 'ZIMGA1'.      " °ü¼¼Ã» °í½ÃÈ¯À²(Åë°üÈ¯À²)
      PERFORM SET_LOCK_ZTIMIMG06     USING  PA_MODE.
    WHEN  'ZIMG12'.       " Planned Cost Rate
      PERFORM SET_LOCK_ZTIMIMG04     USING  PA_MODE.
    WHEN  'ZIMG14'.                 " °ü¼¼»ç Åë°ü ¼ö¼ö·á?
      PERFORM  SET_LOCK_ZTIMIMG07    USING  PA_MODE.
    WHEN  'ZIMG15'.                 " º¸¼¼¿î¼Û ¿îÀÓ´Ü?
      PERFORM  SET_LOCK_ZTIMIMG05    USING  PA_MODE.
    WHEN  'ZIMG16'.                 " H/S CODEº° °ü¼¼?
      PERFORM  SET_LOCK_ZTIMIMG09    USING  PA_MODE.
    WHEN  'ZIMG17'.                 " Ç×°øÈ­¹° ÇØ¿Ü¿î¼Û ¿ä?
      PERFORM  SET_LOCK_ZTIMIMG17    USING  PA_MODE.
    WHEN  'ZIMG20'.                 " ³»·úÈ­¹° ¿î¼Û
      PERFORM  SET_LOCK_ZTIMIMG20    USING  PA_MODE.
    WHEN  'ZIMG24'.                 " Manage Port by Country
      PERFORM  SET_LOCK_ZTIEPORT     USING  PA_MODE.
    WHEN  'ZIMG25'.                 " Port/Freight area match code.
      PERFORM  SET_LOCK_ZTIMIMG23   USING  PA_MODE.
    WHEN  'ZIMG24N'.                " Manage MID Code.
*      PERFORM  SET_LOCK_ZTIMIMG24    USING  PA_MODE.
    WHEN  'ZIMGC1'.                 " °ü¼¼»ç/°ø±Þ¾÷Ã¼ Match CODE
      PERFORM  SET_LOCK_ZTIMIMG02    USING  PA_MODE.
    WHEN  'ZIMGC2'.                 " º¸¼¼±¸¿ª CODE
      PERFORM  SET_LOCK_ZTIMIMG03    USING  PA_MODE.
    WHEN  'ZIMGC3' OR 'ZIMGC4' OR 'ZEXZ23'.     " °ü¸® CODE
      PERFORM  SET_LOCK_ZTIMIMG08    USING  PA_MODE.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_LOCK_MODE
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG06
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG06 USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG06'.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG06'.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG06
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG07_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG07_MODIFY_CHK USING    W_GUBUN.
*  W_GUBUN = 'Y'.
*  DESCRIBE TABLE IT_ZSIMIMG07     LINES W_COUNTER.
*  DESCRIBE TABLE IT_ZSIMIMG07_ORG LINES W_COUNTER1.
*  W_LOOP_CNT = 0.
*
*  IF W_STATUS EQ 'I'.     " New Entry
*    IF W_COUNTER EQ 0.
*      W_GUBUN = 'N'.
*    ENDIF.
*  ELSE.                  " Change Mode
*    IF W_COUNTER EQ W_COUNTER1.
*      LOOP AT IT_ZSIMIMG07_ORG.
*        READ TABLE IT_ZSIMIMG07 WITH KEY
*                              BUKRS     = IT_ZSIMIMG07_ORG-BUKRS
*                              ZFPONC    = IT_ZSIMIMG07_ORG-ZFPONC
*                              ZFITKD    = IT_ZSIMIMG07_ORG-ZFITKD
*                              ZFCUT     = IT_ZSIMIMG07_ORG-ZFCUT
*                              ZFAPLDT   = IT_ZSIMIMG07_ORG-ZFAPLDT
*                              BINARY SEARCH.
*
*        IF IT_ZSIMIMG07_ORG NE IT_ZSIMIMG07.
*          W_LOOP_CNT = 1.    EXIT.
*        ENDIF.
*      ENDLOOP.
*      IF W_LOOP_CNT EQ 0.
*        W_GUBUN = 'N'.
*      ENDIF.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG07_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG24_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG24_MODIFY_CHK USING    W_GUBUN.

ENDFORM.                    " P2000_ZTIMIMG24_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG05_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG05_MODIFY_CHK USING    W_GUBUN.
  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG05     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG05_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG05_ORG.
        READ TABLE IT_ZSIMIMG05 WITH KEY
                              ZFPORT    = IT_ZSIMIMG05_ORG-ZFPORT
                              ZFBNARCD  = IT_ZSIMIMG05_ORG-ZFBNARCD
                              ZFTRANS   = IT_ZSIMIMG05_ORG-ZFTRANS
                              ZFCOSTCD  = IT_ZSIMIMG05_ORG-ZFCOSTCD
                              ZFAPLDT   = IT_ZSIMIMG05_ORG-ZFAPLDT
                              BINARY SEARCH.

        IF IT_ZSIMIMG05_ORG NE IT_ZSIMIMG05.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG05_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG09_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG09_MODIFY_CHK USING    W_GUBUN.
  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG09     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG09_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG09_ORG.
        READ TABLE IT_ZSIMIMG09 WITH KEY
                              STAWN     = IT_ZSIMIMG09_ORG-STAWN
                              ZFAPLDT   = IT_ZSIMIMG09_ORG-ZFAPLDT
                              BINARY SEARCH.

        IF IT_ZSIMIMG09_ORG NE IT_ZSIMIMG09.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG09_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG02_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG02_MODIFY_CHK USING    W_GUBUN.
  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG02     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG02_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG02_ORG.
        READ TABLE IT_ZSIMIMG02 WITH KEY
                              ZFCOTM    = IT_ZSIMIMG02_ORG-ZFCOTM
                              ZFWERKS   = IT_ZSIMIMG02_ORG-ZFWERKS
                              BINARY SEARCH.

        IF IT_ZSIMIMG02_ORG NE IT_ZSIMIMG02.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG02_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG03_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG03_MODIFY_CHK USING    W_GUBUN.
  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG03     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG03_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG03_ORG.
        READ TABLE IT_ZSIMIMG03 WITH KEY
                              ZFBNAR    = IT_ZSIMIMG03_ORG-ZFBNAR
                              BINARY SEARCH.

        IF IT_ZSIMIMG03_ORG NE IT_ZSIMIMG03.
          W_LOOP_CNT = W_LOOP_CNT + 1.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG03_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG08_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG08_MODIFY_CHK USING    W_GUBUN.

  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG08     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG08_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG08_ORG.
        READ TABLE IT_ZSIMIMG08 WITH KEY
                              ZFCD      = IT_ZSIMIMG08_ORG-ZFCD
                              BINARY SEARCH.

        IF IT_ZSIMIMG08_ORG NE IT_ZSIMIMG08.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG08_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG04
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG04 USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG04'.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG04'.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG04
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG07
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG07 USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG07'.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG07'.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG07
**&---------------------------------
**------------------------------------*
**&      Form  SET_LOCK_ZTIMIMG24
**&---------------------------------
**------------------------------------*
*FORM SET_LOCK_ZTIMIMG24 USING    PA_MODE.
*
*  IF PA_MODE EQ 'L'.
*    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG24'.
*  ELSEIF PA_MODE EQ 'U'.
*    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG24'.
*  ENDIF.
*
*ENDFORM.                    " SET_LOCK_ZTIMIMG24
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG05
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG05 USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG05'.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG05'.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG05
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG09
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG09 USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG09'.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG09'.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG09
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG02
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG02 USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG02'.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG02'.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG02
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG03
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG03 USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG03'.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG03'.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG03
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG08
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG08 USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG08'
         EXPORTING
              ZFCDTY = W_ZFCDTY.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG08'
         EXPORTING
              ZFCDTY = W_ZFCDTY.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG08
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG01
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG01 USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG01'.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG01'.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG01
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG17
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG17.

  REFRESH : IT_ZSIMIMG17, IT_ZSIMIMG17_DEL.

* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG17
                                        FROM ZTIMIMG17.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG17'.
  ENDIF.

  IT_ZSIMIMG17_ORG[] = IT_ZSIMIMG17[].

ENDFORM.                    " P1000_READ_ZTIMIMG17
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG17_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG17_MODIFY_CHK USING    W_GUBUN.
  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG17     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG17_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG17_ORG.
        READ TABLE IT_ZSIMIMG17 WITH KEY
                              LAND1     = IT_ZSIMIMG17_ORG-LAND1
                              WAERS     = IT_ZSIMIMG17_ORG-WAERS
                              ZFAPLDT   = IT_ZSIMIMG17_ORG-ZFAPLDT
                              BINARY SEARCH.

        IF IT_ZSIMIMG17_ORG NE IT_ZSIMIMG17.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG17_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG17
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG17.
  CLEAR : W_WAERS, W_ZFAPLDT, W_LAND1.

* Internal Table Sort
  SORT IT_ZSIMIMG17  BY  LAND1 WAERS ZFAPLDT.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSIMIMG17.
    IF W_LAND1   EQ IT_ZSIMIMG17-LAND1   AND
       W_WAERS   EQ IT_ZSIMIMG17-WAERS   AND
       W_ZFAPLDT EQ IT_ZSIMIMG17-ZFAPLDT.

      MESSAGE E968 WITH W_LAND1 W_WAERS W_ZFAPLDT.

    ENDIF.

    MOVE : IT_ZSIMIMG17-LAND1   TO W_LAND1,
           IT_ZSIMIMG17-WAERS   TO W_WAERS,
           IT_ZSIMIMG17-ZFAPLDT TO W_ZFAPLDT,
           SY-TABIX             TO W_TABIX.

* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG17-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG17_ORG WITH KEY
                         LAND1   = IT_ZSIMIMG17-LAND1
                         WAERS   = IT_ZSIMIMG17-WAERS
                         ZFAPLDT = IT_ZSIMIMG17-ZFAPLDT.
        IF SY-SUBRC EQ 0.
          MESSAGE E968 WITH W_LAND1 W_WAERS W_ZFAPLDT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG17_DEL.
    DELETE FROM ZTIMIMG17
           WHERE LAND1   EQ IT_ZSIMIMG17_DEL-LAND1
           AND   WAERS   EQ IT_ZSIMIMG17_DEL-WAERS
           AND   ZFAPLDT EQ IT_ZSIMIMG17_DEL-ZFAPLDT.

* Change Document..
    CLEAR : *ZTIMIMG17.
    MOVE-CORRESPONDING IT_ZSIMIMG17_DEL TO  ZTIMIMG17.
    PERFORM   P3000_ZTIMIMG17_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG17_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG17.
    SELECT SINGLE * FROM ZTIMIMG17
                    WHERE LAND1   EQ IT_ZSIMIMG17-LAND1
                    AND   WAERS   EQ IT_ZSIMIMG17-WAERS
                    AND   ZFAPLDT EQ IT_ZSIMIMG17-ZFAPLDT.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG17-ZFMINPR   EQ ZTIMIMG17-ZFMINPR   AND
               IT_ZSIMIMG17-ZFMINBS   EQ ZTIMIMG17-ZFMINBS   AND
               IT_ZSIMIMG17-ZFCD      EQ ZTIMIMG17-ZFCD      AND
               IT_ZSIMIMG17-ZF45KLT   EQ ZTIMIMG17-ZF45KLT   AND
               IT_ZSIMIMG17-ZF100KLT  EQ ZTIMIMG17-ZF100KLT  AND
               IT_ZSIMIMG17-ZF1000KLT EQ ZTIMIMG17-ZF1000KLT AND
               IT_ZSIMIMG17-ZF3000KLT EQ ZTIMIMG17-ZF3000KLT AND
               IT_ZSIMIMG17-ZF3000KGE EQ ZTIMIMG17-ZF3000KGE ).
        MOVE-CORRESPONDING ZTIMIMG17    TO *ZTIMIMG17.
        MOVE-CORRESPONDING IT_ZSIMIMG17 TO  ZTIMIMG17.
        MOVE : SY-UNAME         TO   ZTIMIMG17-UNAM,
               SY-DATUM         TO   ZTIMIMG17-UDAT.
        UPDATE ZTIMIMG17.
        IF SY-SUBRC NE 0.   MESSAGE E952.   ENDIF.

* Change Document..
        PERFORM   P3000_ZTIMIMG17_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG17-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG17 TO  ZTIMIMG17.
      MOVE : SY-UNAME  TO   ZTIMIMG17-ERNAM,
             SY-DATUM  TO   ZTIMIMG17-CDAT,
             SY-UNAME  TO   ZTIMIMG17-UNAM,
             SY-DATUM  TO   ZTIMIMG17-UDAT.
      INSERT ZTIMIMG17.
      IF SY-SUBRC NE 0.   MESSAGE E952.   ENDIF.

* Change Document..
      CLEAR : *ZTIMIMG17.
      PERFORM   P3000_ZTIMIMG17_CHANGE_DOC        USING  'I'.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG17
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG17
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG17 USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG17'.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG17'.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG17
*&---------------------------------------------------------------------*
*&      Form  GET_DD07T_SELECT
*&---------------------------------------------------------------------*
FORM GET_DD07T_SELECT USING    P_DOMNAME
                               P_FIELD
                      CHANGING P_W_NAME.

  CLEAR : DD07T, P_W_NAME.

  SELECT * FROM DD07T WHERE DOMNAME     EQ P_DOMNAME
                      AND   DDLANGUAGE  EQ SY-LANGU
                      AND   AS4LOCAL    EQ 'A'
                      AND   DOMVALUE_L  EQ P_FIELD
                      ORDER BY AS4VERS DESCENDING.
    EXIT.
  ENDSELECT.

  IF SY-SUBRC NE 0.   EXIT.   ENDIF.

  P_W_NAME   = DD07T-DDTEXT.

ENDFORM.                    " GET_DD07T_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_CHANGE_DOCUMENT
*&---------------------------------------------------------------------*
FORM P2000_CHANGE_DOCUMENT.

  CASE SY-TCODE.
    WHEN 'ZIMG01'.                 " Payment Terms
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG01 WHERE ZFMARK = 'X'.
        ZTIMIMG01 = IT_ZSIMIMG01.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG01'.
      OBJECID       =    ZTIMIMG01(20).
    WHEN 'ZIMG02'.
      OBJECTCLASS   =   'ZTIMIMGTX'.
      CONCATENATE SY-MANDT ZTIMIMGTX-BUKRS INTO OBJECID.
    WHEN 'ZIMG03' OR 'ZIMG04' OR 'ZEXZ23'.
      OBJECTCLASS   =   'ZTIMIMG00'.
      OBJECID       =    ZTIMIMG00(3).
    WHEN 'ZIMG05'.
      OBJECTCLASS   =   'ZTIMIMG11'.
      CONCATENATE SY-MANDT ZTIMIMG11-BUKRS INTO OBJECID.
    WHEN 'ZIMG08'.
      OBJECTCLASS   =   'ZTIMIMG20'.
      CONCATENATE SY-MANDT
                  ZTIMIMG20-BUKRS
                  ZTIMIMG20-ZFAPLDT   INTO OBJECID.
    WHEN 'ZIMG10'.
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG10 WHERE ZFMARK = 'X'.
        ZTIMIMG10 = IT_ZSIMIMG10.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG10'.
      OBJECID       =    ZTIMIMG10(08).
    WHEN 'ZIMG11'.
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG06 WHERE ZFMARK = 'X'.
        ZTIMIMG06 = IT_ZSIMIMG06.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG06'.
      OBJECID       =    ZTIMIMG06(16).
    WHEN 'ZIMG12'.                 " Planned cost rate
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG04 WHERE ZFMARK = 'X'.
        ZTIMIMG04 = IT_ZSIMIMG04.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG04'.
      OBJECID       =    ZTIMIMG04(16).
    WHEN 'ZIMG14'.
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG07 WHERE ZFMARK = 'X'.
        ZTIMIMG07 = IT_ZSIMIMG07.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG07'.
      OBJECID       =    ZTIMIMG07(24).
    WHEN 'ZIMG15'.
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG05 WHERE ZFMARK = 'X'.
        ZTIMIMG05 = IT_ZSIMIMG05.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG05'.
      OBJECID       =    ZTIMIMG05(20).
    WHEN 'ZIMG16'.
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG09 WHERE ZFMARK = 'X'.
        ZTIMIMG09 = IT_ZSIMIMG09.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG09'.
      OBJECID       =    ZTIMIMG09(28).
    WHEN 'ZIMG17'.
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG17 WHERE ZFMARK = 'X'.
        ZTIMIMG17 = IT_ZSIMIMG17.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG17'.
      OBJECID       =    ZTIMIMG17(19).
    WHEN 'ZIMG21'.
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG21 WHERE ZFMARK = 'X'.
        ZTIMIMG21 = IT_ZSIMIMG21.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG21'.
      OBJECID       =    ZTIMIMG21(21).
    WHEN 'ZIMG22'.
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG22 WHERE ZFMARK = 'X'.
        ZTIMIMG22 = IT_ZSIMIMG22.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG22'.
      OBJECID       =    ZTIMIMG22(21).
    WHEN 'ZIMG24'.                 " Manage Port by Country.
      W_CNT = 0.
      LOOP AT IT_ZSIEPORT WHERE ZFMARK = 'X'.
        ZTIEPORT = IT_ZSIEPORT.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIEPORT'.
      OBJECID       =    ZTIEPORT(9).
   WHEN 'ZIMG25'.
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG23 WHERE ZFMARK = 'X'.
        ZTIMIMG23 = IT_ZSIMIMG23.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG23'.
      OBJECID       =    ZTIMIMG23(15).
    WHEN 'ZIMGC1'.
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG02 WHERE ZFMARK = 'X'.
        ZTIMIMG02 = IT_ZSIMIMG02.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG02'.
      OBJECID       =    ZTIMIMG02(06).
    WHEN 'ZIMGC2'.
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG03 WHERE ZFMARK = 'X'.
        ZTIMIMG03 = IT_ZSIMIMG03.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG03'.
      OBJECID       =    ZTIMIMG03(13).
    WHEN 'ZIMGC3'.                 " °ü¸® CODE
      W_CNT = 0.
      LOOP AT IT_ZSIMIMG08 WHERE ZFMARK = 'X'.
        ZTIMIMG08 = IT_ZSIMIMG08.   W_CNT = W_CNT + 1.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.          MESSAGE I951.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE I965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIMIMG08'.
      OBJECID       =    ZTIMIMG08(09).
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  SUBMIT RCS00120 WITH  OBJEKT   =   OBJECTCLASS
                  WITH  OBJEKTID =   OBJECID
                  AND   RETURN.

ENDFORM.                    " P2000_CHANGE_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG00_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG00_CHANGE_DOC USING  UPD_CHNGIND.

  CONCATENATE SY-MANDT SY-DYNNR INTO OBJECID.

  CALL FUNCTION 'ZTIMIMG00_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG00        =    ZTIMIMG00
           O_ZTIMIMG00        =   *ZTIMIMG00
           UPD_ZTIMIMG00      =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG00    = ''
     TABLES
           ICDTXT_ZTIMIMG00   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG00_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_NUMBER_RANGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_NUMBER_RANGE_CHECK USING    P_TYPE.

  IF W_STATUS NE 'D'.
    CALL FUNCTION 'ZIM_CHECK_NUMBER_RANGE'
         EXPORTING
              ZFREQTY     = P_TYPE
         CHANGING
              W_ZTIMIMG00 = ZTIMIMG00.
  ENDIF.

ENDFORM.                    " P2000_NUMBER_RANGE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG01_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG01_CHANGE_DOC USING    UPD_CHNGIND.

  OBJECID = ZTIMIMG01(20).

  CALL FUNCTION 'ZTIMIMG01_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG01        =    ZTIMIMG01
           O_ZTIMIMG01        =   *ZTIMIMG01
           UPD_ZTIMIMG01      =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG01    = ''
     TABLES
           ICDTXT_ZTIMIMG01   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG01_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG06_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG06_CHANGE_DOC USING    UPD_CHNGIND.

  OBJECID = ZTIMIMG06(16).

  CALL FUNCTION 'ZTIMIMG06_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG06        =    ZTIMIMG06
           O_ZTIMIMG06        =   *ZTIMIMG06
           UPD_ZTIMIMG06      =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG06    = ''
     TABLES
           ICDTXT_ZTIMIMG06   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG06_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG04_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG04_CHANGE_DOC USING    UPD_CHNGIND.

  OBJECID = ZTIMIMG04(16).

  CALL FUNCTION 'ZTIMIMG04_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG04        =    ZTIMIMG04
           O_ZTIMIMG04        =   *ZTIMIMG04
           UPD_ZTIMIMG04      =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG04    = ''
     TABLES
           ICDTXT_ZTIMIMG04   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG04_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG07_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG07_CHANGE_DOC USING    UPD_CHNGIND.

  OBJECID = ZTIMIMG07(24).

  CALL FUNCTION 'ZTIMIMG07_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG07        =    ZTIMIMG07
           O_ZTIMIMG07        =   *ZTIMIMG07
           UPD_ZTIMIMG07      =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG07    = ''
     TABLES
           ICDTXT_ZTIMIMG07   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG07_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG24_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG24_CHANGE_DOC USING    UPD_CHNGIND.

  OBJECID = ZTIMIMG24(24).

  CALL FUNCTION 'ZTIMIMG24_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG24        =    ZTIMIMG24
           O_ZTIMIMG24        =   *ZTIMIMG24
           UPD_ZTIMIMG24      =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG24    = ''
     TABLES
           ICDTXT_ZTIMIMG24   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG24_CHANGE_DOC

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG05_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG05_CHANGE_DOC USING    UPD_CHNGIND.

  OBJECID = ZTIMIMG05(20).

  CALL FUNCTION 'ZTIMIMG05_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG05        =    ZTIMIMG05
           O_ZTIMIMG05        =   *ZTIMIMG05
           UPD_ZTIMIMG05      =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG05    = ''
     TABLES
           ICDTXT_ZTIMIMG05   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG05_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG09_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG09_CHANGE_DOC USING    UPD_CHNGIND.

  OBJECID = ZTIMIMG09(28).

  CALL FUNCTION 'ZTIMIMG09_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG09        =    ZTIMIMG09
           O_ZTIMIMG09        =   *ZTIMIMG09
           UPD_ZTIMIMG09      =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG09    = ''
     TABLES
           ICDTXT_ZTIMIMG09   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG09_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG17_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG17_CHANGE_DOC USING    UPD_CHNGIND.

  OBJECID = ZTIMIMG17(19).

  CALL FUNCTION 'ZTIMIMG17_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG17        =    ZTIMIMG17
           O_ZTIMIMG17        =   *ZTIMIMG17
           UPD_ZTIMIMG17      =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG17    = ''
     TABLES
           ICDTXT_ZTIMIMG17   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG17_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG02_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG02_CHANGE_DOC USING    UPD_CHNGIND.

  OBJECID = ZTIMIMG02(06).

  CALL FUNCTION 'ZTIMIMG02_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG02        =    ZTIMIMG02
           O_ZTIMIMG02        =   *ZTIMIMG02
           UPD_ZTIMIMG02      =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG02    = ''
     TABLES
           ICDTXT_ZTIMIMG02   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG02_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG03_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG03_CHANGE_DOC USING    UPD_CHNGIND.

  OBJECID = ZTIMIMG03(13).

  CALL FUNCTION 'ZTIMIMG03_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG03        =    ZTIMIMG03
           O_ZTIMIMG03        =   *ZTIMIMG03
           UPD_ZTIMIMG03      =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG03    = ''
     TABLES
           ICDTXT_ZTIMIMG03   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG03_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG08_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG08_CHANGE_DOC USING     UPD_CHNGIND.

  OBJECID = ZTIMIMG08(09).

  CALL FUNCTION 'ZTIMIMG08_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG08        =    ZTIMIMG08
           O_ZTIMIMG08        =   *ZTIMIMG08
           UPD_ZTIMIMG08      =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG08    = ''
     TABLES
           ICDTXT_ZTIMIMG08   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG08_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_REMARK_TEXT
*&---------------------------------------------------------------------*
FORM P2000_SET_REMARK_TEXT.
  DATA :         W_NAME(20) TYPE C.
  FIELD-SYMBOLS: <LOC>.

  DO 5 TIMES.
    CLEAR : W_NAME.
    W_NAME(1) = SY-INDEX.
    CONCATENATE 'TEXT-' W_ZFCDTY '-' W_NAME INTO W_NAME.
    ASSIGN:             (W_NAME) TO    <LOC>.
    CASE SY-INDEX.
      WHEN 1.   W_ZFCD_TEXT1 = <LOC>.
      WHEN 2.   W_ZFCD_TEXT2 = <LOC>.
      WHEN 3.   W_ZFCD_TEXT3 = <LOC>.
      WHEN 4.   W_ZFCD_TEXT4 = <LOC>.
      WHEN 5.   W_ZFCD_TEXT5 = <LOC>.
    ENDCASE.
  ENDDO.

ENDFORM.                    " P2000_SET_REMARK_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_BONDED_AREA_CHK
*&---------------------------------------------------------------------*
FORM P2000_BONDED_AREA_CHK USING    P_CODE
                                    P_TABLE
                                    P_CNT.
  CLEAR : P_CNT, P_TABLE.
  IF P_CODE IS INITIAL.   EXIT.   ENDIF.
* B/L
  SELECT COUNT( * ) INTO P_CNT FROM ZTBL
         WHERE  ZFBNARCD EQ P_CODE.

  IF P_CNT GT 0.
    P_TABLE = 'B/L(ZTBL)'.   EXIT.
  ENDIF.

* º¸¼¼¿î¼Û ¿îÀÓ´Ü?
  SELECT COUNT( * ) INTO P_CNT FROM ZTIMIMG05
         WHERE  ZFBNARCD EQ P_CODE.
  IF P_CNT GT 0.
    P_TABLE = 'º¸¼¼¿î¼Û ¿îÀÓ´Ü°¡(ZTIMIMG05)'.   EXIT.
  ENDIF.

* ±ä±Þº¸¼¼¿î¼Û ÀÇ?
  SELECT COUNT( * ) INTO P_CNT FROM ZTBLUG
         WHERE  ZFBNARCD EQ P_CODE.
  IF P_CNT GT 0.
    P_TABLE = '±ä±Þº¸¼¼¿î¼Û ÀÇ·Ú(ZTBLUG)'.   EXIT.
  ENDIF.

*  B/L º¸¼¼¿î¼Û(¹ÝÀÔ¿¹Á¤Á¤º¸)
  SELECT COUNT( * ) INTO P_CNT FROM ZTBLINOU
         WHERE ( ZFDBNARC  EQ P_CODE
         OR      ZFABNARC  EQ P_CODE ).

  IF P_CNT GT 0.
    P_TABLE = '¹ÝÀÔ¿¹Á¤Á¤º¸(ZTBLINOU)'.   EXIT.
  ENDIF.

* ¹ÝÀÔ ½Å°í.
  SELECT COUNT( * ) INTO P_CNT FROM ZTBLINR
         WHERE ZFBNARCD   EQ P_CODE.
  IF P_CNT GT 0.
    P_TABLE = '¹ÝÀÔ½Å°í(ZTBLINR)'.   EXIT.
  ENDIF.

* ¹ÝÃâ ½Å°í.
  SELECT COUNT( * ) INTO P_CNT FROM ZTBLOUR
         WHERE ZFBNARCD   EQ P_CODE.
  IF P_CNT GT 0.
    P_TABLE = '¹ÝÀÔ½Å°í(ZTBLOUR)'.   EXIT.
  ENDIF.
ENDFORM.                    " P2000_BONDED_AREA_CHK
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_OTEHRS_CODE
*&---------------------------------------------------------------------*
FORM P2000_GET_OTEHRS_CODE CHANGING P_CDTY     P_CODE
                                    P_TABLE    P_CNT.
  CLEAR : P_CNT, P_TABLE.
  IF P_CDTY IS INITIAL.   EXIT.   ENDIF.
  IF P_CODE IS INITIAL.   EXIT.   ENDIF.

  CASE P_CDTY.
    WHEN '000'.    " ±â?

* LOGIC ¹Ì¹Ý¿µÀ¸·Î 2000/06/05 ÇöÀç ¹Ì»ç¿ë....
    WHEN '001'.    " ¼öÀÔ°Å·¡±¸?

* B/L
      SELECT COUNT( * ) INTO P_CNT FROM ZTBL
                        WHERE ZFPONC EQ P_CODE.
      IF P_CNT GT 0.
        P_TABLE = 'B/L(ZTBL)'.   EXIT.
      ENDIF.

* INVOICE HEADER
      SELECT COUNT( * ) INTO P_CNT FROM ZTIV
                        WHERE ZFPONC EQ P_CODE.
      IF P_CNT GT 0.
        P_TABLE = 'Invoice Header(ZTBL)'.   EXIT.
      ENDIF.
    WHEN '002'.    " ¼±ÀûÇ×/µµÂø?

* B/L
      SELECT COUNT( * ) INTO P_CNT FROM ZTBL
                        WHERE ZFSPRTC EQ P_CODE
                        OR    ZFAPRTC EQ P_CODE.
      IF P_CNT GT 0.
        P_TABLE = 'B/L(ZTBL)'.   EXIT.
      ENDIF.

* º¸¼¼¿î¼Û ¿îÀÓ´Ü°¡.
      SELECT COUNT( * ) INTO P_CNT FROM ZTIMIMG05
                        WHERE ZFPORT EQ P_CODE.
      IF P_CNT GT 0.
        P_TABLE = 'º¸¼¼¿î¼Û ¿îÀÓ´Ü°¡(ZTIMIMG05)'. EXIT.
      ENDIF.
    WHEN '003'.    " L/C ºñ¿ëÄÚ?
      SELECT COUNT( * ) INTO P_CNT FROM ZTRECST
                        WHERE ZFCSCD EQ P_CODE.
      IF P_CNT GT 0.
        P_TABLE = '¼öÀÔÀÇ·Ú ºñ¿ë(ZTRECST)'.   EXIT.
      ENDIF.
    WHEN '004'.    " B/L ÇØ¿Ü¿îÀÓ ºñ¿ëÄÚ?
      SELECT COUNT( * ) INTO P_CNT FROM ZTBLCST
                        WHERE ZFCSCD EQ P_CODE.
      IF P_CNT GT 0.
        P_TABLE = 'B/L ÇØ¿Ü¿î¼Ûºñ¿ë(ZTBL)'.   EXIT.
      ENDIF.
    WHEN '005'.    " B/L º¸¼¼¿îÀÓ ºñ¿ëÄÚ?
      SELECT COUNT( * ) INTO P_CNT FROM ZTBLCST
                        WHERE ZFCSCD EQ P_CODE.
      IF P_CNT GT 0.
        P_TABLE = 'B/L º¸¼¼¿îÀÓºñ¿ë(ZTBL)'.   EXIT.
      ENDIF.
    WHEN '006'.    " Åë°ü ºñ¿ëÄÚ?

    WHEN '010'.    " ¼Õº¸»ç Ç¥ÁØÄÚµå ¹× ½Äº°?
      SELECT COUNT( * ) INTO P_CNT FROM ZTINSSG5
                        WHERE ZFINSC EQ P_CODE.
      IF P_CNT GT 0.
        P_TABLE = '¼Õº¸»ç Ç¥ÁØÄÚµå ¹× ½Äº°ÀÚ(ZTINSSG5)'.   EXIT.
      ENDIF.
    WHEN '011'.    " º¸¼¼¿î¼Û Location
      SELECT COUNT( * ) INTO P_CNT FROM ZTBLINR
                        WHERE ZFLOC EQ P_CODE.
      IF P_CNT GT 0.
        P_TABLE = 'º¸¼¼±¸¿ª¹ÝÀÔ(ZTBLINR)'.   EXIT.
      ENDIF.
    WHEN '999'.    " MASTER L/C ±âÅ¸±¸ºñ¼­·ù (---> »èÁ¦½Ã »ó°ü¹« )
    WHEN OTHERS.  " ±â?
  ENDCASE.

ENDFORM.                    " P2000_GET_OTEHRS_CODE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG10
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG10.

  REFRESH: IT_ZSIMIMG10, IT_ZSIMIMG10_ORG, IT_ZSIMIMG10_DEL.

* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG10
                                              FROM ZTIMIMG10
            ORDER BY ZFCUT.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG10'.
  ENDIF.

  LOOP AT IT_ZSIMIMG10.
    CLEAR : IT_ZSIMIMG10-NAME1.
    SELECT SINGLE NAME1 INTO IT_ZSIMIMG10-NAME1 FROM LFA1
                        WHERE LIFNR EQ IT_ZSIMIMG10-ZFVEN.
    MODIFY IT_ZSIMIMG10.
  ENDLOOP.

  IT_ZSIMIMG10_ORG[] = IT_ZSIMIMG10[].

ENDFORM.                    " P1000_READ_ZTIMIMG10
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG10_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG10_MODIFY_CHK USING    W_GUBUN.
  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG10     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG10_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG10_ORG.
        READ TABLE IT_ZSIMIMG10 WITH KEY
                              ZFCUT     = IT_ZSIMIMG10_ORG-ZFCUT
                              BINARY SEARCH.

        IF IT_ZSIMIMG10_ORG NE IT_ZSIMIMG10.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG10_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG10
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG10.
  CLEAR : W_ZFCUT.

* Internal Table Sort
  SORT IT_ZSIMIMG10  BY ZFCUT.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSIMIMG10.
    IF W_ZFCUT   EQ IT_ZSIMIMG10-ZFCUT.
      MESSAGE E969 WITH W_ZFCUT.
    ENDIF.

    MOVE : IT_ZSIMIMG10-ZFCUT   TO W_ZFCUT,
           SY-TABIX             TO W_TABIX.

* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG10-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG10_ORG WITH KEY
                        ZFCUT  = IT_ZSIMIMG10-ZFCUT.
        IF SY-SUBRC EQ 0.
          MESSAGE E969 WITH W_ZFCUT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG10_DEL.
    DELETE FROM ZTIMIMG10
           WHERE ZFCUT   EQ IT_ZSIMIMG10_DEL-ZFCUT.

* Change Document..
    CLEAR : *ZTIMIMG10.
    MOVE-CORRESPONDING IT_ZSIMIMG10_DEL TO  ZTIMIMG10.
    PERFORM   P3000_ZTIMIMG10_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG10_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG10.
    SELECT SINGLE * FROM ZTIMIMG10
                    WHERE ZFCUT  =  IT_ZSIMIMG10-ZFCUT.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG10-ZFVEN    EQ ZTIMIMG10-ZFVEN ).
        MOVE-CORRESPONDING ZTIMIMG10    TO *ZTIMIMG10.
        MOVE-CORRESPONDING IT_ZSIMIMG10 TO  ZTIMIMG10.
        MOVE : SY-UNAME         TO   ZTIMIMG10-UNAM,
               SY-DATUM         TO   ZTIMIMG10-UDAT.
        UPDATE ZTIMIMG10.
        IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
        PERFORM   P3000_ZTIMIMG10_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG10-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG10 TO  ZTIMIMG10.
      MOVE : SY-UNAME  TO   ZTIMIMG10-ERNAM,
             SY-DATUM  TO   ZTIMIMG10-CDAT,
             SY-UNAME  TO   ZTIMIMG10-UNAM,
             SY-DATUM  TO   ZTIMIMG10-UDAT.
      INSERT ZTIMIMG10.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
      PERFORM   P3000_ZTIMIMG10_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG10
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG10_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG10_CHANGE_DOC USING   UPD_CHNGIND.

  OBJECID = ZTIMIMG10(08).

  CALL FUNCTION 'ZTIMIMG10_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG10        =    ZTIMIMG10
           O_ZTIMIMG10        =   *ZTIMIMG10
           UPD_ZTIMIMG10      =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG10    = ''
     TABLES
           ICDTXT_ZTIMIMG10   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG10_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG10
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG10 USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG10'.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG10'.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG10
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_ZTIMIMG08_TEXT
*&---------------------------------------------------------------------*
FORM P2000_GET_ZTIMIMG08_TEXT USING P_GUBUN   P_CODE   P_ERR_MODE
                              CHANGING P_TEXT.

  CLEAR : ZTIMIMG08.
  SELECT SINGLE * FROM ZTIMIMG08 WHERE ZFCDTY   EQ   P_GUBUN
                                 AND   ZFCD     EQ   P_CODE.

  IF SY-SUBRC NE 0.
    MESSAGE ID 'ZIM' TYPE P_ERR_MODE NUMBER 308 WITH P_CODE.
  ENDIF.

  P_TEXT = ZTIMIMG08-ZFCDNM.

ENDFORM.                    " P2000_GET_ZTIMIMG08_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK USING    P_OK_CODE.

*  IF SY-TCODE(6) EQ 'ZIMG10'.
*>>> °ü¼¼»ç/°ø±Þ¾÷Ã¼ MATCH CODE
*    CASE P_OK_CODE.
*      WHEN 'DELT' OR 'NEWL' OR 'AEND' .
*         AUTHORITY-CHECK OBJECT 'ZIM_ZIMG10'
*                                ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
* ELSEIF SY-TCODE(6) EQ 'ZIMG12'.
*>>> Planed Cost Rate °ü¸®
*    CASE P_OK_CODE.
*      WHEN 'DELT' OR 'NEWL' OR 'AEND' .
*         AUTHORITY-CHECK OBJECT 'ZIM_ZIMG12'
*                                ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
* ELSEIF SY-TCODE(6) EQ 'ZIMG11'.
*>>> °ü¼¼Ã» °í½ÃÈ¯À²
*    CASE P_OK_CODE.
*      WHEN 'DELT' OR 'NEWL' OR 'AEND' .
*         AUTHORITY-CHECK OBJECT 'ZIM_ZIMG11'
*                                ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
* ELSEIF SY-TCODE(6) EQ 'ZIMGA1'.
*>>> Åë°üÈ¯À² °ü¸®
*    CASE P_OK_CODE.
*      WHEN 'DELT' OR 'NEWL' OR 'AEND' .
*         AUTHORITY-CHECK OBJECT 'ZIM_ZIMGA1'
*                                ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(6) EQ 'ZIMG01'.
*>>> Payment Terms Cofiguration
*    CASE P_OK_CODE.
*      WHEN 'DELT' OR 'NEWL' OR 'AEND' .
*         AUTHORITY-CHECK OBJECT 'ZIM_ZIMG01'
*                                ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
* ELSEIF SY-TCODE(6) EQ 'ZIMG14'.
*>>> Åë°ü¼ö¼ö·áÀ²
*    CASE P_OK_CODE.
*      WHEN 'DELT' OR 'NEWL' OR 'AEND' .
*         AUTHORITY-CHECK OBJECT 'ZIM_ZIMG14'
*                                ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
* ELSEIF SY-TCODE(6) EQ 'ZIMG15'.
*>>> º¸¼¼¿î¼Û ¿îÀÓ´Ü°¡
*    CASE P_OK_CODE.
*      WHEN 'DELT' OR 'NEWL' OR 'AEND' .
*         AUTHORITY-CHECK OBJECT 'ZIM_ZIMG15'
*                                ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
* ELSEIF SY-TCODE(6) EQ 'ZIMG16'.
*>>> H/S CODE º° °ü¼¼À²
*    CASE P_OK_CODE.
*      WHEN 'DELT' OR 'NEWL' OR 'AEND' .
*         AUTHORITY-CHECK OBJECT 'ZIM_ZIMG16'
*                                ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(6) EQ 'ZIMG17'.
*>>> Ç×°øÈ­¹° ÇØ¿Ü¿î¼ÛÀ²
*    CASE P_OK_CODE.
*      WHEN 'DELT' OR 'NEWL' OR 'AEND' .
*         AUTHORITY-CHECK OBJECT 'ZIM_ZIMG17'
*                                ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
* ELSEIF SY-TCODE(6) EQ 'ZIMGC1'.
*>>> ¼¼°üÄÚµå
*    CASE P_OK_CODE.
*      WHEN 'DELT' OR 'NEWL' OR 'AEND' .
*         AUTHORITY-CHECK OBJECT 'ZIM_ZIMGC1'
*                                ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
* ELSEIF SY-TCODE(6) EQ 'ZIMGC2'.
*>>> º¸¼¼±¸¿ªÄÚµå
*    CASE P_OK_CODE.
*      WHEN 'DELT' OR 'NEWL' OR 'AEND' .
*         AUTHORITY-CHECK OBJECT 'ZIM_ZIMGC2'
*                                ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
* ELSEIF SY-TCODE(6) EQ 'ZIMG06'.
*>>> ¿î¼Û¼ö´Ü MATCH CODE °ü¸®
*    CASE P_OK_CODE.
*      WHEN 'DELT' OR 'NEWL' OR 'AEND' .
*         AUTHORITY-CHECK OBJECT 'ZIM_ZIMG06'
*                                ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(6) EQ 'ZIMGC3' OR
*        SY-TCODE(6) EQ 'ZIMGC4' OR
*        SY-TCODE(6) EQ 'ZEXZ23'.
*>>> ±âÅ¸°ü¸® ÄÚµå
*    CASE P_OK_CODE.
*      WHEN 'DELT' OR 'NEWL' OR 'AEND' .
*         AUTHORITY-CHECK OBJECT 'ZIM_ZIMGC3'
*                                ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
* ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_DATE_CONVERT
*&---------------------------------------------------------------------*
FORM P2000_DATE_CONVERT USING    P_DATE.

  CPOSITION  = 0.

  DO 10 TIMES.
    IF P_DATE+CPOSITION(1) EQ '/'.
      P_DATE+CPOSITION(1) = '.'.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_DATE_CONVERT
*&---------------------------------------------------------------------*
*&      Form  P3000_CALL_TRANSACTION_OB08
*&---------------------------------------------------------------------*
FORM P3000_CALL_TRANSACTION_OB08.

*>>> ¼±ÅÃÇÑ ·¹ÄÚµå¸¸ ÀúÀå( OB08 BDC )
  W_COUNT = 0.
  LOOP AT IT_CURR WHERE ZFMARK = 'X'.
    ADD    1    TO    W_COUNT.

*>>> ³¯Â¥ ³»ºÎº¯È¯ÇüÅÂ·Î º¯È¯.
    WRITE : IT_CURR-ZFDATE   TO   W_CDATE.
    PERFORM P2000_DATE_CONVERT    USING  W_CDATE.
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
         EXPORTING
              INPUT  = W_CDATE
         IMPORTING
              OUTPUT = W_GDATU.

*>>> STANDARD Exchange Rates SELECT
    SELECT SINGLE * FROM TCURR
                    WHERE KURST EQ 'CC'
                    AND   FCURR EQ IT_CURR-FCURR
                    AND   TCURR EQ 'KRW'
                    AND   GDATU EQ W_GDATU.

*>>> Update..
    IF SY-SUBRC EQ 0.
      PERFORM    P3000_CALL_OB08_UPDATE.
    ELSE.

*>>> Insert..
      PERFORM    P3000_CALL_OB08_CREATE.
    ENDIF.
  ENDLOOP.

  IF W_COUNT EQ 0.
    MESSAGE E032.
  ELSE.
    PERFORM BDC_DYNPRO       USING 'SAPL0SAP'        '0020'.
    PERFORM BDC_FIELD        USING 'BDC_OKCODE'      '=SAVE'.
    PERFORM BDC_DYNPRO       USING 'SAPL0SAP'        '0020'.
    PERFORM BDC_FIELD        USING 'BDC_OKCODE'      '=BACK'.
    PERFORM BDC_TRANSACTION  USING 'OB08'
                             CHANGING W_SY_SUBRC.
  ENDIF.

ENDFORM.                    " P3000_CALL_TRANSACTION_OB08
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG06_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG06_UPDATE.

  IF W_COUNT GT 0.
    REFRESH : IT_ZTIMIMG06.
    LOOP AT IT_CURR WHERE ZFMARK = 'X'.
      MOVE : SY-MANDT        TO   IT_ZTIMIMG06-MANDT,
             IT_CURR-FCURR   TO   IT_ZTIMIMG06-WAERS,
             IT_CURR-ZFDATE  TO   IT_ZTIMIMG06-ZFAPLDT,
             IT_CURR-ZFEXPDT TO   IT_ZTIMIMG06-ZFEXPDT,
             IT_CURR-UKURS   TO   IT_ZTIMIMG06-ZFEXRT,
             IT_CURR-ERNAM   TO   IT_ZTIMIMG06-ERNAM,
             IT_CURR-CDAT    TO   IT_ZTIMIMG06-CDAT,
             IT_CURR-UNAM    TO   IT_ZTIMIMG06-UNAM,
             IT_CURR-UDAT    TO   IT_ZTIMIMG06-UDAT.
      APPEND  IT_ZTIMIMG06.
    ENDLOOP.

* Insert Data..
    LOOP AT IT_ZTIMIMG06.
      SELECT SINGLE * FROM ZTIMIMG06
                      WHERE WAERS   EQ IT_ZTIMIMG06-WAERS
                      AND   ZFAPLDT EQ IT_ZTIMIMG06-ZFAPLDT.

      IF SY-SUBRC EQ 0.
        IF NOT ( IT_ZTIMIMG06-ZFEXRT   EQ ZTIMIMG06-ZFEXRT AND
                 IT_ZTIMIMG06-ZFEXPDT  EQ ZTIMIMG06-ZFEXPDT ).
          MOVE-CORRESPONDING ZTIMIMG06    TO *ZTIMIMG06.
          MOVE-CORRESPONDING IT_ZTIMIMG06 TO  ZTIMIMG06.
          MOVE : SY-UNAME         TO   ZTIMIMG06-UNAM,
                 SY-DATUM         TO   ZTIMIMG06-UDAT.
          UPDATE ZTIMIMG06.
          IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
          PERFORM   P3000_ZTIMIMG06_CHANGE_DOC        USING  'U'.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING IT_ZTIMIMG06 TO  ZTIMIMG06.
        MOVE : SY-UNAME  TO   ZTIMIMG06-ERNAM,
               SY-DATUM  TO   ZTIMIMG06-CDAT,
               SY-UNAME  TO   ZTIMIMG06-UNAM,
               SY-DATUM  TO   ZTIMIMG06-UDAT.

        INSERT ZTIMIMG06.
        IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.

* Change Document..
        CLEAR : *ZTIMIMG06.
        PERFORM   P3000_ZTIMIMG06_CHANGE_DOC        USING  'I'.
      ENDIF.
    ENDLOOP.
  ELSE.
    MESSAGE E032.
  ENDIF.

ENDFORM.                    " P3000_ZTIMIMG06_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P3000_CALL_OB08_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_CALL_OB08_UPDATE.
  DATA : TEXT11(11)     TYPE    C.

  PERFORM BDC_DYNPRO       USING 'SAPL0SAP'        '0020'.
  PERFORM BDC_FIELD        USING 'BDC_OKCODE'      '=POSI'.

  PERFORM BDC_DYNPRO       USING 'SAPLSPO4'        '0300'.
  PERFORM BDC_FIELD        USING 'SVALD-VALUE(01)' 'CC'.
  PERFORM BDC_FIELD        USING 'SVALD-VALUE(01)' IT_CURR-FCURR.
  PERFORM BDC_FIELD        USING 'SVALD-VALUE(01)' 'KRW'.
  PERFORM BDC_FIELD        USING 'SVALD-VALUE(01)' W_CDATE.
  PERFORM BDC_FIELD        USING 'BDC_OKCODE'      '=FURT'.

  WRITE : IT_CURR-UKURS    TO    TEXT11.
  PERFORM BDC_DYNPRO       USING 'SAPL0SAP'        '0020'.
  PERFORM BDC_FIELD        USING 'RFCU9-KURSP(01)' TEXT11.
  PERFORM BDC_FIELD        USING 'BDC_OKCODE'      '/00'.

ENDFORM.                    " P3000_CALL_OB08_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P3000_CALL_OB08_CREATE
*&---------------------------------------------------------------------*
FORM P3000_CALL_OB08_CREATE.
  DATA : TEXT11(11)     TYPE    C.

  PERFORM BDC_DYNPRO       USING 'SAPL0SAP'          '0020'.
  PERFORM BDC_FIELD        USING 'BDC_OKCODE'        '=NEWL'.

  WRITE : IT_CURR-UKURS    TO    TEXT11.
  PERFORM BDC_DYNPRO       USING 'SAPL0SAP'          '0020'.
  PERFORM BDC_FIELD        USING 'V_TCURR-KURST(01)' 'CC'.
  PERFORM BDC_FIELD        USING 'V_TCURR-GDATU(01)'  W_CDATE.
  PERFORM BDC_FIELD        USING 'V_TCURR-FCURR(01)'  IT_CURR-FCURR.
  PERFORM BDC_FIELD        USING 'RFCU9-KURSP(01)'    TEXT11.
  PERFORM BDC_FIELD        USING 'V_TCURR-TCURR(01)'  'KRW'.
  PERFORM BDC_FIELD        USING 'BDC_OKCODE'         '=BACK'.

ENDFORM.                    " P3000_CALL_OB08_CREATE
*&---------------------------------------------------------------------*
*&      Form  SET_V_CURR_LOCK
*&---------------------------------------------------------------------*
FORM SET_V_CURR_LOCK USING    P_MODE.

  DATA : L_VIEW_NAME  LIKE    OCUS-TABLE.
  DATA : L_ACTION.

*>>> Check if Standard Update Selected..
  CHECK STANDARD_UP EQ 'X'.

  L_VIEW_NAME = 'V_TCURR'.

  IF P_MODE EQ 'L'.
    L_ACTION = 'E'.
  ELSEIF P_MODE EQ 'U'.
    L_ACTION = 'D'.
  ENDIF.

  CALL FUNCTION 'VIEW_ENQUEUE'
          EXPORTING
               VIEW_NAME        = L_VIEW_NAME
               ACTION           = L_ACTION
               ENQUEUE_MODE     = 'E'
               ENQUEUE_RANGE    = ''
          EXCEPTIONS
               FOREIGN_LOCK     = 1
               SYSTEM_FAILURE   = 2
               TABLE_NOT_FOUND  = 5
               CLIENT_REFERENCE = 7.
  CASE SY-SUBRC.
    WHEN 1.
      MESSAGE E049(SV) WITH SY-MSGV1.
    WHEN 2.
      MESSAGE A050(SV) WITH L_VIEW_NAME.
    WHEN 5.
      MESSAGE A028(SV) WITH L_VIEW_NAME.
    WHEN 7.
      MESSAGE W054(SV) WITH SY-MANDT.
  ENDCASE.

ENDFORM.                    " SET_V_CURR_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_KOMV_KSCHL_HELP
*&---------------------------------------------------------------------*
FORM P2000_KOMV_KSCHL_HELP USING    P_KSCHL
                                    P_KPOSN.

  DATA: DISPLAY, ITEM_HEAD.

  DATA: DA_KSCHL LIKE KOMV-KSCHL.

  CLEAR DISPLAY.
  IF W_STATUS EQ 'D'.
    DISPLAY = 'X'.
  ENDIF.

  IF P_KPOSN CO '0'.
    ITEM_HEAD = 'B'.
  ELSE.
    ITEM_HEAD = 'A'.
  ENDIF.

  KOMK-KALSM = 'RM0000'.

  CALL FUNCTION 'RV_CONDITION_RECORD_DISPLAY'
       EXPORTING
            CONDITION_USE         = 'A'
            APPLICATION           = 'M'
            CALCULATION_PROCEDURE = KOMK-KALSM
            GET_CONDITION_TYPE    = 'X'
            ITEM_HEADER           = ITEM_HEAD
            CHANGE_MANUALLY       = ' ABC'
            F4_DISPLAY_ONLY       = DISPLAY
       IMPORTING
            CONDITION_TYPE        = DA_KSCHL.

  IF DA_KSCHL NE SPACE.
    MOVE DA_KSCHL TO P_KSCHL.
  ENDIF.

ENDFORM.                    " P2000_KOMV_KSCHL_HELP
*&---------------------------------------------------------------------*
*&      Form  P2000_HELP_PAYMENT_TERM
*&---------------------------------------------------------------------*
FORM P2000_HELP_PAYMENT_TERM USING    P_ZTERM.
  DATA : L_SHOW    LIKE    RFCU4-FLAGX.

  IF W_STATUS EQ 'D'.
    L_SHOW = 'X'.
  ELSE.
    L_SHOW = ' '.
  ENDIF.

  CALL FUNCTION 'FI_F4_ZTERM'
       EXPORTING
            I_KOART       = 'K'
            I_ZTERM       = P_ZTERM
            I_XSHOW       = L_SHOW
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

ENDFORM.                    " P2000_HELP_PAYMENT_TERM
*&---------------------------------------------------------------------*
*&      Form  P2000_TBTKZ_HELP
*&---------------------------------------------------------------------*
FORM P2000_TBTKZ_HELP USING    P_TBTKZ.

  DATA: F_BEWTP LIKE T163B-BEWTP.

  SUBMIT rm08rl10 AND RETURN.
  IMPORT f_bewtp FROM MEMORY ID 'RM08RL10_BEWTP'.
  IF W_STATUS NE 'D'.
    P_TBTKZ = f_bewtp.
  ENDIF.

ENDFORM.                    " P2000_TBTKZ_HELP
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG12
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG12.

  REFRESH: IT_ZSIMIMG12, IT_ZSIMIMG12_ORG, IT_ZSIMIMG12_DEL.
* Table Multi-Select
  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG12
  FROM ZTIMIMG12
  ORDER BY EXPVZ.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG12'.
  ENDIF.

  LOOP AT IT_ZSIMIMG12.

    SELECT SINGLE BEZEI
    INTO   W_BEZEI
    FROM   T618T
    WHERE  SPRAS  =  SY-LANGU
    AND    LAND1  =  'KR'
    AND    EXPVZ  =   IT_ZSIMIMG12-EXPVZ.

    IF SY-SUBRC NE 0.
      MOVE '  '  TO  IT_ZSIMIMG12-BEZEI.
    ELSE.
      MOVE W_BEZEI TO IT_ZSIMIMG12-BEZEI.
    ENDIF.
    MODIFY IT_ZSIMIMG12.
  ENDLOOP.

  IT_ZSIMIMG12_ORG[] = IT_ZSIMIMG12[].

ENDFORM.                    " P1000_READ_ZTIMIMG12
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG12_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG12_MODIFY_CHK USING    P_W_GUBUN.
  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG12     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG12_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG12_ORG.
        READ TABLE IT_ZSIMIMG12 WITH KEY
                              EXPVZ     = IT_ZSIMIMG12_ORG-EXPVZ
                              BINARY SEARCH.

        IF IT_ZSIMIMG12_ORG NE IT_ZSIMIMG12.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG12_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG20_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG20_MODIFY_CHK USING    P_W_GUBUN.
  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG20     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG20_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG20_ORG.
        READ TABLE IT_ZSIMIMG20 WITH KEY
                                BUKRS     = IT_ZSIMIMG20-BUKRS
                                ZFAPLDT   = IT_ZSIMIMG20-ZFAPLDT
*                                ZFCD      = IT_ZSIMIMG20_ORG-ZFCD
*                                BSTYP   = IT_ZSIMIMG20_ORG-BSTYP
*                                BSART   = IT_ZSIMIMG20_ORG-BSART
*                                ZTERM   = IT_ZSIMIMG20_ORG-ZTERM
*                                ZFAPLDT = IT_ZSIMIMG20_ORG-ZFAPLDT
                                BINARY SEARCH.

        IF IT_ZSIMIMG20_ORG NE IT_ZSIMIMG20.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

*    IF W_COUNTER EQ W_COUNTER1.
*      LOOP AT IT_ZSIMIMG20_ORG.
*        READ TABLE IT_ZSIMIMG20 WITH KEY
*                          ZFWTFR   =   IT_ZSIMIMG20_ORG-ZFWTFR
*                          BINARY SEARCH.
*
*        IF  SY-SUBRC EQ 0 AND
*         ( IT_ZSIMIMG20-ZFWTTO NE IT_ZSIMIMG20_ORG-ZFWTTO OR
*           IT_ZSIMIMG20-NETPR  NE IT_ZSIMIMG20_ORG-NETPR  OR
*           IT_ZSIMIMG20-ZFCFRATE NE IT_ZSIMIMG20_ORG-ZFCFRATE ).
*          W_LOOP_CNT = 1.    EXIT.
*        ENDIF.
*      ENDLOOP.
*      IF W_LOOP_CNT EQ 0.
*        W_GUBUN = 'N'.
*      ENDIF.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG20_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG12
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG12.
  CLEAR : W_EXPVZ.

* Internal Table Sort
  SORT IT_ZSIMIMG12  BY EXPVZ.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSIMIMG12.
    IF W_EXPVZ   EQ IT_ZSIMIMG12-EXPVZ.
      MESSAGE E969 WITH W_EXPVZ.
    ENDIF.

    MOVE : IT_ZSIMIMG12-EXPVZ   TO W_EXPVZ,
           SY-TABIX             TO W_TABIX.

* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG12-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG12_ORG WITH KEY
                        EXPVZ  = IT_ZSIMIMG12-EXPVZ.
        IF SY-SUBRC EQ 0.
          MESSAGE E969 WITH W_EXPVZ.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG12_DEL.
    DELETE FROM ZTIMIMG12
           WHERE EXPVZ   EQ IT_ZSIMIMG12_DEL-EXPVZ.
    CLEAR : *ZTIMIMG12.
    MOVE-CORRESPONDING IT_ZSIMIMG12_DEL TO  ZTIMIMG12.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG12_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG12.
    SELECT SINGLE * FROM ZTIMIMG12
                    WHERE EXPVZ  =  IT_ZSIMIMG12-EXPVZ.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG12-ZFTRANS    EQ ZTIMIMG12-ZFTRANS ).
        MOVE-CORRESPONDING ZTIMIMG12    TO *ZTIMIMG12.
        MOVE-CORRESPONDING IT_ZSIMIMG12 TO  ZTIMIMG12.
        MOVE : SY-UNAME         TO   ZTIMIMG12-UNAM,
               SY-DATUM         TO   ZTIMIMG12-UDAT.
        UPDATE ZTIMIMG12.
        IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG12-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG12 TO  ZTIMIMG12.
      MOVE : SY-UNAME  TO   ZTIMIMG12-ERNAM,
             SY-DATUM  TO   ZTIMIMG12-CDAT,
             SY-UNAME  TO   ZTIMIMG12-UNAM,
             SY-DATUM  TO   ZTIMIMG12-UDAT.
      INSERT ZTIMIMG12.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG12
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG12
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG12 USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG12'.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG12'.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG12
*&---------------------------------------------------------------------*
*&      Module  CHECK_CARGO_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_CARGO_CHECK_SCR0200 INPUT.
  PERFORM   P2000_NUMBER_RANGE_CHECK  USING 'CG'.
ENDMODULE.                 " CHECK_CARGO_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_COPY
*&---------------------------------------------------------------------*
FORM P2000_DATA_COPY.

  LOOP AT IT_ZSIMIMG02 WHERE ZFMARK = 'X'.
    W_TABIX  =  SY-TABIX.
    INSERT IT_ZSIMIMG02 INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P2000_DATA_COPY
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_ZTIMIMGTX
*&---------------------------------------------------------------------*
FORM P2000_SAVE_ZTIMIMGTX.

  W_OK_CODE = OK-CODE.
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
*-----------------------------------------------------------------------
* ZTIMIMGTX TABLE WRITE
*-----------------------------------------------------------------------
      PERFORM  P3000_WRITE_ZTIMIMGTX.
      MESSAGE  S953.
      IF W_OK_CODE NE 'SAVE'.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'N'.              " No...
      MESSAGE  S957.
      IF W_OK_CODE NE 'SAVE'.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'C'.              " Cancel
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SAVE_ZTIMIMGTX
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMGTX
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMGTX.

  DATA: L_ZTIMIMGTX   LIKE    ZTIMIMGTX.

  SELECT SINGLE * INTO  *ZTIMIMGTX  FROM  ZTIMIMGTX
                  WHERE BUKRS        EQ    ZTIMIMGTX-BUKRS.

  IF SY-SUBRC EQ 0.
    UPDATE ZTIMIMGTX.

* Change Document Object
    PERFORM   P3000_ZTIMIMGTX_CHANGE_DOC        USING  'U'.
  ELSE.

* Change Document Object
    INSERT ZTIMIMGTX.
    PERFORM   P3000_ZTIMIMGTX_CHANGE_DOC        USING  'I'.
  ENDIF.

  IF SY-SUBRC NE 0.
    MESSAGE E952.
  ENDIF.

ENDFORM.                    " P3000_WRITE_ZTIMIMGTX
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMGTX
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMGTX USING  PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_ZTIMIMGTX'
         EXPORTING
              MANDT = SY-MANDT
              BUKRS = ZTIMIMGTX-BUKRS.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_ZTIMIMGTX'
         EXPORTING
              MANDT = SY-MANDT
              BUKRS = ZTIMIMGTX-BUKRS.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMGTX
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMGTX_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMGTX_CHANGE_DOC USING UPD_CHNGIND.

  CONCATENATE SY-MANDT ZTIMIMGTX-BUKRS INTO OBJECID.

  CALL FUNCTION 'ZTIMIMGTX_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID                = OBJECID
            TCODE                   = SY-TCODE
            UTIME                   = SY-UZEIT
            UDATE                   = SY-DATUM
            USERNAME                = SY-UNAME
            OBJECT_CHANGE_INDICATOR = UPD_CHNGIND
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZTIMIMGTX    = ''
            N_ZTIMIMGTX             = ZTIMIMGTX
            O_ZTIMIMGTX             = *ZTIMIMGTX
            UPD_ZTIMIMGTX           = UPD_CHNGIND
       TABLES
            ICDTXT_ZTIMIMGTX        = IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMGTX_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P1000_VEN_GET_NAME
*&---------------------------------------------------------------------*
FORM P1000_VEN_GET_NAME.

  CLEAR : W_ZFPVEN1_NM, W_ZFPVEN2_NM, W_ZFPVEN3_NM, W_ZFPVEN4_NM,
          W_ZFPVEN5_NM, W_ZFPVEN6_NM, W_ZFPVEN7_NM, W_INSCOMP_NM..

  IF NOT ZTIMIMG11-ZFPVEN1 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE *  FROM LFA1
    WHERE  LIFNR EQ ZTIMIMG11-ZFPVEN1.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN1.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN1_NM.
    ENDIF.
  ENDIF.
  IF NOT ZTIMIMG11-ZFPVEN2 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE * FROM LFA1
    WHERE  LIFNR = ZTIMIMG11-ZFPVEN2.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN2.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN2_NM.
    ENDIF.
  ENDIF.
  IF NOT ZTIMIMG11-ZFPVEN3 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE * FROM LFA1
    WHERE LIFNR = ZTIMIMG11-ZFPVEN3.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN3.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN3_NM.
    ENDIF.
  ENDIF.
  IF NOT ZTIMIMG11-ZFPVEN4 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE * FROM LFA1
    WHERE LIFNR = ZTIMIMG11-ZFPVEN4.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN4.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN4_NM.
    ENDIF.
  ENDIF.
  IF NOT ZTIMIMG11-ZFPVEN5 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE * FROM LFA1
    WHERE LIFNR = ZTIMIMG11-ZFPVEN5.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN5.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN5_NM.
    ENDIF.
  ENDIF.
  IF NOT ZTIMIMG11-ZFPVEN6 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE * FROM LFA1
    WHERE LIFNR = ZTIMIMG11-ZFPVEN6.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN6.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN6_NM.
    ENDIF.
  ENDIF.
  IF NOT ZTIMIMG11-ZFPVEN7 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE * FROM LFA1
    WHERE LIFNR = ZTIMIMG11-ZFPVEN7.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN7.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN7_NM.
    ENDIF.
  ENDIF.

  IF NOT ZTIMIMG11-ZFINSCP IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE * FROM LFA1
    WHERE LIFNR = ZTIMIMG11-ZFINSCP.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFINSCP.
    ELSE.
      MOVE LFA1-NAME1        TO W_INSCOMP_NM.
    ENDIF.
  ENDIF.

ENDFORM.                    " P1000_VEN_GET_NAME
*&---------------------------------------------------------------------*
*&      Form  TREE_CREATE_AND_INIT
*&---------------------------------------------------------------------*
FORM TREE_CREATE_AND_INIT USING    P_SY_SUBRC.

  DATA:  NODE_TABLE TYPE TREEV_NTAB,
         ITEM_TABLE TYPE ITEM_TABLE_TYPE,
         EVENT TYPE CNTL_SIMPLE_EVENT,
         EVENTS TYPE CNTL_SIMPLE_EVENTS.
*        HIERARCHY_HEADER TYPE TREEV_HHDR.
  DATA: LV_CURSOR_NODE_KEY TYPE TREEV_NODE-NODE_KEY.
  DATA: NODE_WA LIKE MTREESNODE_FT.

  CREATE OBJECT G_APPLICATION.
* create a container for the tree control
  CREATE OBJECT G_CUSTOM_CONTAINER
    EXPORTING
* the container is linked to the custom control with the
* name 'TREE_CONTAINER' on the dynpro
      CONTAINER_NAME = 'TREE_SCR9500'       "'TREE_CONTAINER'
    EXCEPTIONS
      CNTL_ERROR = 1
      CNTL_SYSTEM_ERROR = 2
      CREATE_ERROR = 3
      LIFETIME_ERROR = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

* setup the hierarchy header
*   HIERARCHY_HEADER-HEADING = 'DOC. SELECT'. "#EC NOTEXT
*                                        " heading
*   HIERARCHY_HEADER-WIDTH = 30.         " width: 30 characters

* create a tree control

* After construction, the control contains one column in the
* hierarchy area. The name of this column
* is defined via the constructor parameter HIERACHY_COLUMN_NAME.
  CREATE OBJECT G_TREE
    EXPORTING
      PARENT              = G_CUSTOM_CONTAINER
      NODE_SELECTION_MODE = CL_GUI_LIST_TREE=>NODE_SEL_MODE_SINGLE
      ITEM_SELECTION     = 'X'
      WITH_HEADERS       = ' '
    EXCEPTIONS
      CNTL_SYSTEM_ERROR           = 1
      CREATE_ERROR                = 2
      FAILED                      = 3
      ILLEGAL_NODE_SELECTION_MODE = 4
      LIFETIME_ERROR              = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

* define the events which will be passed to the backend
* node double click
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_NODE_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'.                                   "
  APPEND EVENT TO EVENTS.

* item double click
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " expand no children
*  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_EXPAND_NO_CHILDREN.
*  event-appl_event = 'X'.
*  append event to events.
*
*                                       " link click
*  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_LINK_CLICK.
*  event-appl_event = 'X'.
*  append event to events.

*                                       " button click
*  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_BUTTON_CLICK.
*  event-appl_event = 'X'.
*  append event to events.
*
*                                       " checkbox change
*  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_CHECKBOX_CHANGE.
*  event-appl_event = 'X'.
*  append event to events.

  CALL METHOD G_TREE->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS = EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.
  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

* assign event handlers in the application class to each desired event
*  IF NOT G_APPLICATION IS INITIAL.

*-------------------------------------------------------------------
*> KSB MODIFY 2001/12/03
*-------------------------------------------------------------------
*    SET HANDLER G_APPLICATION->HANDLE_NODE_DOUBLE_CLICK FOR G_TREE.
*    SET HANDLER G_APPLICATION->HANDLE_ITEM_DOUBLE_CLICK FOR G_TREE.
*-------------------------------------------------------------------

*    SET HANDLER G_APPLICATION->HANDLE_EXPAND_NO_CHILDREN FOR G_TREE.
*    SET HANDLER G_APPLICATION->HANDLE_LINK_CLICK FOR G_TREE.
*    SET HANDLER G_APPLICATION->HANDLE_BUTTON_CLICK FOR G_TREE.
*    SET HANDLER G_APPLICATION->HANDLE_CHECKBOX_CHANGE FOR G_TREE.
*  ENDIF.

* add some nodes to the tree control
* NOTE: the tree control does not store data at the backend. If an
* application wants to access tree data later, it must store the
* tree data itself.

  PERFORM BUILD_NODE_AND_ITEM_TABLE
          CHANGING NODE_TABLE ITEM_TABLE.

  CALL METHOD G_TREE->ADD_NODES_AND_ITEMS
    EXPORTING
      NODE_TABLE = NODE_TABLE
      ITEM_TABLE = ITEM_TABLE
      ITEM_TABLE_STRUCTURE_NAME = 'MTREEITM'
    EXCEPTIONS
      FAILED = 1
      CNTL_SYSTEM_ERROR = 3
      ERROR_IN_TABLES = 4
      DP_ERROR = 5
      TABLE_STRUCTURE_NAME_NOT_FOUND = 6.
  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

* expand tree nodes
  CALL METHOD G_TREE->EXPAND_ROOT_NODES
    EXPORTING
      LEVEL_COUNT                   = 2
      EXPAND_SUBTREE                = ' '
    EXCEPTIONS
      FAILED                        = 1
      ILLEGAL_LEVEL_COUNT           = 2
      CNTL_SYSTEM_ERROR             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE E000(TREE_CONTROL_MSG).
  ENDIF.

** set cursor at first entry
*  READ TABLE ITEM_TABLE WITH KEY NODE_KEY = 'CI00'
*                             INTO NODE_WA.
*  IF SY-SUBRC IS INITIAL.
*    CALL METHOD G_TREE->SET_SELECTED_NODE
*      EXPORTING
*        NODE_KEY                   = NODE_WA-NODE_KEY
*      EXCEPTIONS
*        FAILED                     = 1
*        SINGLE_NODE_SELECTION_ONLY = 2
*        NODE_NOT_FOUND             = 3
*        CNTL_SYSTEM_ERROR          = 4.
*    IF NOT SY-SUBRC IS INITIAL.
*    ENDIF.
*  ENDIF.
  G_NODE_KEY_OLD = NODE_WA-NODE_KEY.

ENDFORM.                    " TREE_CREATE_AND_INIT
*&---------------------------------------------------------------------*
*&      Form  BUILD_NODE_AND_ITEM_TABLE
*&---------------------------------------------------------------------*
FORM BUILD_NODE_AND_ITEM_TABLE   CHANGING
                                       NODE_TABLE TYPE TREEV_NTAB
                                       ITEM_TABLE TYPE ITEM_TABLE_TYPE.

**>>READ NODE SOURCE.
*   PERFORM READ_NODE_SOURCE.

* Build the node table.

* Caution: The nodes are inserted into the tree according to the order
* in which they occur in the table. In consequence, a node must not
* must not occur in the node table before its parent node.

* Node with key 'Root'
  NODE-NODE_KEY = 'Root'.                                   "#EC NOTEXT
  " Key of the node
  CLEAR NODE-RELATKEY.      " Special case: A root node has no parent
  CLEAR NODE-RELATSHIP.     " node.

  NODE-HIDDEN = ' '.        " The node is visible,
  NODE-DISABLED = 'X'.      " selectable,
  NODE-ISFOLDER = 'X'.      " a folder.
  CLEAR NODE-N_IMAGE.       " Folder-/ Leaf-Symbol in state "closed":
  " use default.
  CLEAR NODE-EXP_IMAGE.     " Folder-/ Leaf-Symbol in state "open":
  " use default
  CLEAR NODE-EXPANDER.      " see below.
  APPEND NODE TO NODE_TABLE.

  LOOP AT IT_V_TVKN.
    CLEAR NODE.
    NODE-NODE_KEY = IT_V_TVKN-NODE. "IT_ZSIMIMG08-ZFCD.
    NODE-RELATKEY = 'Root'.
    NODE-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
    APPEND NODE TO NODE_TABLE.
  ENDLOOP.
* The items of the nodes:
  CLEAR ITEM.
  ITEM-NODE_KEY = 'Root'.
  ITEM-ITEM_NAME = '1'.
  ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
  ITEM-LENGTH = 21.
  IF SY-LANGU EQ '3'.
    ITEM-TEXT = '±âÅ¸°ü¸®ÄÚµå'.
  ELSEIF SY-LANGU EQ 'E'.
    ITEM-TEXT = 'Other Code Management'.
  ELSE.
    ITEM-TEXT = 'Other Code Management'.
  ENDIF.
  APPEND ITEM TO ITEM_TABLE.

  LOOP AT IT_V_TVKN.
* Node with key 'Child1'
    CLEAR ITEM.
    ITEM-NODE_KEY = IT_V_TVKN-NODE."IT_ZSIMIMG08-ZFCD.
    ITEM-ITEM_NAME = '1'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
    ITEM-LENGTH = 50.
*    ITEM-LENGTH = STRLEN( IT_V_TVKN-TEXT ).
    ITEM-TEXT = IT_V_TVKN-TEXT.    "IT_ZSIMIMG08-ZFCDNM.
    APPEND ITEM TO ITEM_TABLE.
  ENDLOOP.

ENDFORM.                    " BUILD_NODE_AND_ITEM_TABLE
*&---------------------------------------------------------------------*
*&      Form  P2000_READ_NEW_NODE
*&---------------------------------------------------------------------*
FORM P2000_READ_NEW_NODE CHANGING G_KEY_NODE_NEW.

  DATA:  ITEMNAME_AC TYPE TV_ITMNAME.

  CALL METHOD G_TREE->GET_SELECTED_NODE
             IMPORTING
                  NODE_KEY = G_NODE_KEY
             EXCEPTIONS
                  FAILED                     = 1
                  SINGLE_NODE_SELECTION_ONLY = 2
                  CNTL_SYSTEM_ERROR          = 3
                  OTHERS                     = 4.

  CALL METHOD CL_GUI_CFW=>FLUSH
       EXCEPTIONS
            CNTL_SYSTEM_ERROR = 1
            CNTL_ERROR        = 2
            OTHERS            = 3.

  IF G_NODE_KEY IS INITIAL.
    CALL METHOD G_TREE->GET_SELECTED_ITEM
         IMPORTING
              NODE_KEY  = G_NODE_KEY
              ITEM_NAME = ITEMNAME_AC
         EXCEPTIONS
              FAILED            = 1
              CNTL_SYSTEM_ERROR = 2
              NO_ITEM_SELECTION = 3
              OTHERS            = 4.

    CALL METHOD CL_GUI_CFW=>FLUSH
         EXCEPTIONS
              CNTL_SYSTEM_ERROR = 1
              CNTL_ERROR        = 2
              OTHERS            = 3.
  ENDIF.

  G_NODE_KEY_NEW = G_NODE_KEY+0(3).

ENDFORM.                    " P2000_READ_NEW_NODE
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_TREE_CONTROL
*&---------------------------------------------------------------------*
FORM CREATE_AND_INIT_TREE_CONTROL.

*LOCAL: VCL_STRUC_TAB.
  DATA: HEADER LIKE TREEV_HHDR,
        event type cntl_simple_event,
        EVENTS TYPE CNTL_SIMPLE_EVENTS,
        ALIGNMENT TYPE I.

*  CLEAR ERROR.
** style = ws_visible + ws_child + ws_clipsiblings.
  SUBSCR_TITLE = 'TITLE'.
  HEADER-HEADING = SUBSCR_TITLE.
  HEADER-WIDTH = 50.                       "vclstruct-objecttext

  CREATE OBJECT NAVI_TREE
    EXPORTING
      PARENT                = DOCKING_CONTAINER
      NODE_SELECTION_MODE   = NAVI_TREE->NODE_SEL_MODE_SINGLE
      ITEM_SELECTION        = 'X'
      HIERARCHY_COLUMN_NAME = 'Column1'
      HIERARCHY_HEADER      = HEADER
    EXCEPTIONS
      CNTL_SYSTEM_ERROR           = 1
      CREATE_ERROR                = 2
      FAILED                      = 3
      ILLEGAL_NODE_SELECTION_MODE = 4
      ILLEGAL_COLUMN_NAME         = 5
      LIFETIME_ERROR              = 6
      OTHERS                      = 7.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

  ALIGNMENT = CL_GUI_COLUMN_TREE=>ALIGN_AT_LEFT +
              CL_GUI_COLUMN_TREE=>ALIGN_AT_BOTTOM +
              CL_GUI_COLUMN_TREE=>ALIGN_AT_TOP +
              CL_GUI_COLUMN_TREE=>ALIGN_AT_RIGHT.

  CALL METHOD NAVI_TREE->SET_ALIGNMENT
       EXPORTING ALIGNMENT = alignment
       EXCEPTIONS OTHERS    = 1.


* define the events which will be passed to the backend
* -> DOUBLE_CLICK
  event-eventid = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
  event-appl_event = 'X'.
  append event to events.
* -> F1 auf Item
  event-eventid = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_KEYPRESS.
  event-appl_event = ' '.  " no PAI if event occurs
  append event to events.
* -> node context menu request
  event-eventid = CL_GUI_COLUMN_TREE=>eventid_item_context_menu_req.
  event-appl_event = ' '. " no PAI if event occurs
  APPEND event TO events.

* -> context menu select event
  call method NAVI_TREE->SET_CTX_MENU_SELECT_EVENT_APPL
    exporting appl_event = 'X'.

  CALL METHOD NAVI_TREE->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS = EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3
      OTHERS                    = 4.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

  CREATE OBJECT event_handler.

  SET HANDLER EVENT_HANDLER->HANDLE_ITEM_DOUBLE_CLICK FOR NAVI_TREE.
  SET HANDLER EVENT_HANDLER->HANDLE_ITEM_KEYPRESS FOR NAVI_TREE.
  SET HANDLER EVENT_HANDLER->HANDLE_ITEM_CONTEXT_MENU_REQ FOR NAVI_TREE.
  SET HANDLER EVENT_HANDLER->HANDLE_ITEM_CONTEXT_MENU_SEL FOR NAVI_TREE.

* assign event handlers in the application class to each desired event
*  SET HANDLER EVENT_HANDLER->HANDLE_NODE_DOUBLE_CLICK FOR NAVI_TREE.
*  SET HANDLER EVENT_HANDLER->HANDLE_ITEM_DOUBLE_CLICK FOR NAVI_TREE.
*  SET HANDLER EVENT_HANDLER->HANDLE_EXPAND_NO_CHILDREN FOR NAVI_TREE.
*  SET HANDLER EVENT_HANDLER->HANDLE_LINK_CLICK FOR NAVI_TREE.
*  SET HANDLER EVENT_HANDLER->HANDLE_BUTTON_CLICK FOR NAVI_TREE.
*  SET HANDLER EVENT_HANDLER->HANDLE_CHECKBOX_CHANGE FOR NAVI_TREE.
*  SET HANDLER EVENT_HANDLER->HANDLE_HEADER_CLICK FOR NAVI_TREE.

  CALL METHOD NAVI_TREE->ADD_KEY_STROKE   "F1 für ITEM_KEYPRESS
       EXPORTING KEY = CL_TREE_CONTROL_BASE=>KEY_F1.

  PERFORM BUILD_NODE_TABLE
                           CHANGING NODE_TABLE ITEM_TABLE.

  CALL METHOD NAVI_TREE->ADD_NODES_AND_ITEMS
    EXPORTING
      NODE_TABLE = NODE_TABLE
      ITEM_TABLE = ITEM_TABLE
      ITEM_TABLE_STRUCTURE_NAME = 'MTREEITM'
    EXCEPTIONS
      FAILED            = 1
      CNTL_SYSTEM_ERROR = 3
      ERROR_IN_TABLES   = 4
      DP_ERROR          = 5
      TABLE_STRUCTURE_NAME_NOT_FOUND = 6
      OTHERS            = 7.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

* alle Knoten expandieren
*  CALL METHOD NAVI_TREE->EXPAND_ROOT_NODES
  CALL METHOD NAVI_TREE->EXPAND_NODE
    EXPORTING
      NODE_KEY = 'Root'                                     "#EC NOTEXT
*    EXPORTING
*      LEVEL_COUNT         = 2
*      EXPAND_SUBTREE      = ' '
    EXCEPTIONS
      OTHERS              = 1.
* if sy-subrc <> 0.
*   kein Handler nötig, da kein Problem, falls Tree nicht expandiert
* endif.

ENDFORM.                    " CREATE_AND_INIT_TREE_CONTROL
*&---------------------------------------------------------------------*
*&      Form  BUILD_NODE_TABLE
*&---------------------------------------------------------------------*
FORM BUILD_NODE_TABLE CHANGING NODE_TAB TYPE TREEV_NTAB
                               ITEM_TAB TYPE ITEM_TABLE_TYPE.

*LOCAL: VCL_STRUC_TAB.
*DATA: NODE TYPE TREEV_NODE,
*      ITEM type VCLTREEC,
*      STRUC_WA LIKE VCL_STRUC_TAB.
* Caution: The nodes are inserted into the tree according to the order
* in which they occur in the table. In consequence, a node
* must not occur in the node table before its parent node.

*  LOOP AT VCL_STRUC_TAB.  " where suppress = space.
*    IF VCL_STRUC_TAB-OLD_SUPPRESS <> VCL_STRUC_TAB-SUPPRESS.
*      VCL_STRUC_TAB-OLD_SUPPRESS = VCL_STRUC_TAB-SUPPRESS.
*      MODIFY VCL_STRUC_TAB.
*    ENDIF.
*    CLEAR NODE.
*   NODE-NODE_KEY = VCL_STRUC_TAB-OBJPOS.           "-object ist zu lang
*    IF VCL_STRUC_TAB-DEPENDENCY <> DEP_ROOT.
*     READ TABLE VCL_STRUC_TAB
*          WITH KEY OBJECT = VCL_STRUC_TAB-PREDOBJECT INTO STRUC_WA.
*     NODE-RELATKEY = STRUC_WA-OBJPOS.              "-predobject zu lang
*     NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
*   ENDIF.
*   NODE-HIDDEN = VCL_STRUC_TAB-SUPPRESS.
*   NODE-DISABLED = ' '.      " selectable,
*   NODE-ISFOLDER = 'X'.      " a folder.
*   NODE-N_IMAGE = 'BNONE'.   "hide Folder-Symbol
*   NODE-EXP_IMAGE = 'BNONE'. "hide Leaf-Symbol
**  clear node-expander.
*   CLEAR ITEM.
*   ITEM-NODE_KEY = NODE-NODE_KEY.
*   ITEM-ITEM_NAME = 'Column1'.                              "#EC NOTEXT
*   ITEM-T_IMAGE = '@FN@'.    "closed folder
**                 '@FO@'     "open folder
*   ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
*   ITEM-TEXT = VCL_STRUC_TAB-OBJECTTEXT.
*   APPEND NODE TO NODE_TAB.
*   APPEND ITEM TO ITEM_TAB.
* ENDLOOP.

*  CLEAR : NODE.
*  NODE-NODE_KEY = 'Root'.                                   "#EC NOTEXT
*  " Key of the node
*  CLEAR NODE-RELATKEY.      " Special case: A root node has no parent
*  CLEAR NODE-RELATSHIP.     " node.
*
*  NODE-HIDDEN = ' '.        " The node is visible,
*  NODE-DISABLED = ' '.      " selectable,
*  NODE-ISFOLDER = 'X'.      " a folder.
*  CLEAR NODE-N_IMAGE.       " Folder-/ Leaf-Symbol in state "closed":
*  " use default.
*  CLEAR NODE-EXP_IMAGE.     " Folder-/ Leaf-Symbol in state "open":
*  " use default
*  CLEAR NODE-EXPANDER.      " see below.
*  APPEND NODE TO NODE_TABLE.
*
*  LOOP AT IT_V_TVKN.
*    CLEAR NODE.
*    NODE-NODE_KEY = IT_V_TVKN-NODE. "IT_ZSIMIMG08-ZFCD.
*    NODE-RELATKEY = 'Root'.
*    NODE-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
*    NODE-HIDDEN = ' '.
*    NODE-DISABLED = ' '.
*    NODE-ISFOLDER = ' '.
*    CLEAR NODE-N_IMAGE.
*    CLEAR NODE-EXP_IMAGE.
*    NODE-EXPANDER = ' '. " The node is marked with a '+', although
*    APPEND NODE TO NODE_TABLE.
*  ENDLOOP.

* The items of the nodes:
*  CLEAR ITEM.
*  ITEM-NODE_KEY = 'Root'.
*  ITEM-ITEM_NAME = 'Column1'.
*  ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
*  ITEM-LENGTH = 13.
*  ITEM-TEXT = '±âÅ¸°ü¸®ÄÚµå'.
*  APPEND ITEM TO ITEM_TABLE.
*
*  LOOP AT IT_V_TVKN.
** Node with key 'Child1'
*    CLEAR ITEM.
*    ITEM-NODE_KEY = IT_V_TVKN-NODE."IT_ZSIMIMG08-ZFCD.
*    ITEM-ITEM_NAME = 'Column1'.
*    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
*    ITEM-LENGTH = 50.
**    ITEM-LENGTH = STRLEN( IT_V_TVKN-TEXT ).
*    ITEM-TEXT = IT_V_TVKN-TEXT.    "IT_ZSIMIMG08-ZFCDNM.
*    APPEND ITEM TO ITEM_TABLE.
*  ENDLOOP.


  NODE-NODE_KEY = 'Root'.                                   "#EC NOTEXT
  " Key of the node
  CLEAR NODE-RELATKEY.      " Special case: A root node has no parent
  CLEAR NODE-RELATSHIP.     " node.

  NODE-HIDDEN = ' '.        " The node is visible,
  NODE-DISABLED = ' '.      " selectable,
  NODE-ISFOLDER = 'X'.      " a folder.
  CLEAR NODE-N_IMAGE.       " Folder-/ Leaf-Symbol in state "closed":
  " use default.
  CLEAR NODE-EXP_IMAGE.     " Folder-/ Leaf-Symbol in state "open":
  " use default
  CLEAR NODE-EXPANDER.      " see below.
  APPEND NODE TO NODE_TABLE.

* Node with key 'Child1'
  NODE-NODE_KEY = 'Child1'.                                 "#EC NOTEXT
  " Key of the node
  " Node is inserted as child of the node with key 'Root'.
  NODE-RELATKEY = 'Root'.
  NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.

  NODE-HIDDEN = ' '.
  NODE-DISABLED = ' '.
  NODE-ISFOLDER = 'X'.
  CLEAR NODE-N_IMAGE.
  CLEAR NODE-EXP_IMAGE.
  NODE-EXPANDER = 'X'. " The node is marked with a '+', although
  " it has no children. When the user clicks on the
  " + to open the node, the event expand_nc is
  " fired. The programmerr can
  " add the children of the
  " node within the event handler of the expand_nc
  " event  (see callback handle_expand_nc).
  APPEND NODE TO NODE_TABLE.

* The items of the nodes:

* Node with key 'Root'
  CLEAR ITEM.
  ITEM-NODE_KEY = 'Root'.
  ITEM-ITEM_NAME = 'Column1'.     " Item of Column 'Column1'
  ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
  ITEM-TEXT = 'Root Col. 1'.                                "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

*  CLEAR ITEM.
*  ITEM-NODE_KEY = 'Root'.
*  ITEM-ITEM_NAME = 'Column2'.     " Item of Column 'Column2'
*  ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
*  ITEM-TEXT = 'Root Col. 2'. "#EC NOTEXT
*  APPEND ITEM TO ITEM_TABLE.

  CLEAR ITEM.
*  ITEM-NODE_KEY = 'Root'.
*  ITEM-ITEM_NAME = 'Column3'.     " Item of Column 'Column3'
*  " Item is a link (click on link fires event LINK_CLICK)
*  ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
*  ITEM-TEXT = 'Root Col. 3'. "#EC NOTEXT "
*  APPEND ITEM TO ITEM_TABLE.

* Node with key 'Child1'
  CLEAR ITEM.
  ITEM-NODE_KEY = 'Child1'.
  ITEM-ITEM_NAME = 'Column1'.
  ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
  ITEM-TEXT = 'Child1 Col. 1'.                              "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

*  CLEAR ITEM.
*  ITEM-NODE_KEY = 'Child1'.
*  ITEM-ITEM_NAME = 'Column2'.     "
* ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_BUTTON. " Item is a button
* ITEM-TEXT = 'Child1 Col. 2'. "#EC NOTEXT
* ITEM-T_IMAGE = '@0B@'.
* APPEND ITEM TO ITEM_TABLE.

*  CLEAR ITEM.
*  ITEM-NODE_KEY = 'Child1'.
*  ITEM-ITEM_NAME = 'Column3'.
*  " Item is a checkbox
*  ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_CHECKBOX.
*  ITEM-EDITABLE = 'X'.
*  ITEM-TEXT = 'Child1 Col. 3'. "#EC NOTEXT
*  ITEM-T_IMAGE = '@0C@'.
*  APPEND ITEM TO ITEM_TABLE.

ENDFORM.                    " BUILD_NODE_TABLE
*&---------------------------------------------------------------------*
*&      Form  CREATE_IMAGE_CONTROL
*&---------------------------------------------------------------------*
FORM CREATE_IMAGE_CONTROL.
* create the picture container
  CREATE OBJECT picture
                EXPORTING parent = DOCKING_CONTAINER.

* Request an URL from the data provider by exporting the pic_data.
  DATA url(255).
  CLEAR url.
  DATA query_table LIKE w3query OCCURS 1 WITH HEADER LINE.
  DATA html_table LIKE w3html OCCURS 1.
  DATA return_code LIKE  w3param-ret_code.
  DATA content_type LIKE  w3param-cont_type.
  DATA content_length LIKE  w3param-cont_len.
  DATA pic_data LIKE w3mime OCCURS 0.
  DATA pic_size TYPE i.

  REFRESH query_table.
  query_table-name = '_OBJECT_ID'.
  query_table-value = 'ENJOYSAP_LOGO'.
  APPEND query_table.

  CALL FUNCTION 'WWW_GET_MIME_OBJECT'
       TABLES
            query_string        = query_table
            html                = html_table
            mime                = pic_data
       CHANGING
            return_code         = return_code
            content_type        = content_type
            content_length      = content_length
       EXCEPTIONS
            OBJECT_NOT_FOUND    = 1
            parameter_not_found = 2
            OTHERS              = 3.
  IF sy-subrc = 0.
    pic_size = content_length.
  ENDIF.

  CALL FUNCTION 'DP_CREATE_URL'
       EXPORTING
            type     = 'image'
            subtype  = cndp_sap_tab_unknown
            size     = pic_size
            lifetime = cndp_lifetime_transaction
       TABLES
            data     = pic_data
       CHANGING
            url      = url
       EXCEPTIONS
            OTHERS   = 1.

* Load Picture
  CALL METHOD picture->load_picture_from_url
      EXPORTING url = url.

ENDFORM.                    " CREATE_IMAGE_CONTROL
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_COMPANY_DATA
*&---------------------------------------------------------------------*
FORM P1000_GET_COMPANY_DATA USING    P_TCODE.

  REFRESH : IT_T001.
  IF P_TCODE EQ 'ZIMG05'.     ">ºñ¿ë°èÁ¤ Á¤ÀÇ.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BUKRS
             FROM ZTIMIMG11.

  ELSEIF P_TCODE EQ 'ZIMG02'. ">EDI °ü·Ã TEXTÁ¤ÀÇ.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BUKRS
             FROM ZTIMIMGTX.
  ELSEIF P_TCODE EQ 'ZIMG14'. ">Åë°ü¼ö¼ö·áÀ² °ü¸®.
    SELECT BUKRS
           INTO CORRESPONDING FIELDS OF TABLE IT_BUKRS
           FROM ZTIMIMGTX
           GROUP BY BUKRS.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_T001
           FROM T001
           FOR ALL ENTRIES IN IT_BUKRS
           WHERE   BUKRS   EQ IT_BUKRS-BUKRS.

ENDFORM.                    " P1000_GET_COMPANY_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_DELETE_MESSAGE.
  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING '»èÁ¦ È®ÀÎ'             " Å¸ÀÌÆ²...
                                    '¼±ÅÃµÈ ³»¿ëÀ» »èÁ¦ÇÕ´Ï´Ù.'
                                    'Á¤¸»·Î »èÁ¦ÇÏ½Ã°Ú½À´Ï±î?' "
                                    'N'                 " Ãë¼Ò ¹öÆ° À¯/?
                                   '2'.                 " default button

  ELSE.

    PERFORM P2000_MESSAGE_BOX USING 'DELETE CONFIRMATION'   " Å¸ÀÌÆ²...
                                    'Being deleted the selected data'
                                    'Do you want to delete?'
                                    'N'                 " Ãë¼Ò ¹öÆ° À¯/?
                                   '2'.                 " default button

  ENDIF.
*   CASE ANTWORT.
*     WHEN 'Y'.              " Yes...
*        MESSAGE  S957.
*        LEAVE TO SCREEN 0.  " " PROGRAM LEAVING
*      WHEN OTHERS.
*  ENDCASE.

ENDFORM.                    " P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG21_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG21_MODIFY_CHK USING    W_GUBUN.

  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG21     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG21_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG21_ORG.
        READ TABLE IT_ZSIMIMG21 WITH KEY
                              ZFAPLDT   = IT_ZSIMIMG21-ZFAPLDT
                              ZFRAGB    = IT_ZSIMIMG21-ZFRAGB
                              WAERS     = IT_ZSIMIMG21_ORG-WAERS
                              MEINS     = IT_ZSIMIMG21_ORG-MEINS
                              BINARY SEARCH.

        IF IT_ZSIMIMG21_ORG NE IT_ZSIMIMG21.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG21_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG21
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG21.

  CLEAR : W_WAERS, W_ZFAPLDT, W_LAND1.

* KEY INSERT.
  LOOP AT IT_ZSIMIMG21.
    W_TABIX = SY-TABIX.
    MOVE : ZTIMIMG21-BUKRS    TO  IT_ZSIMIMG21-BUKRS,
           ZTIMIMG21-ZFAPLDT  TO  IT_ZSIMIMG21-ZFAPLDT,
           ZTIMIMG21-ZFRAGB   TO  IT_ZSIMIMG21-ZFRAGB.
    MODIFY  IT_ZSIMIMG21  INDEX  W_TABIX.
  ENDLOOP.

* Internal Table Sort
  SORT IT_ZSIMIMG21  BY ZFCD WAERS MEINS.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSIMIMG21.
    IF W_ZFCD    EQ IT_ZSIMIMG21-ZFCD    AND
       W_WAERS   EQ IT_ZSIMIMG21-WAERS   AND
       W_MEINS   EQ IT_ZSIMIMG21-MEINS   .
      MESSAGE E968 WITH W_ZFCD W_WAERS W_MEINS.
    ENDIF.
    MOVE : IT_ZSIMIMG21-ZFCD    TO W_ZFCD,
           IT_ZSIMIMG21-WAERS   TO W_WAERS,
           IT_ZSIMIMG21-MEINS   TO W_MEINS,
           SY-TABIX             TO W_TABIX.

* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG21-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG21_ORG WITH KEY
                         ZFRAGB  = IT_ZSIMIMG21-ZFRAGB
                         ZFCD    = IT_ZSIMIMG21-ZFCD
                         WAERS   = IT_ZSIMIMG21-WAERS
                         MEINS   = IT_ZSIMIMG21-MEINS.
        IF SY-SUBRC EQ 0.
          MESSAGE E972 WITH W_ZFCD W_WAERS W_MEINS.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG21_DEL.
    DELETE FROM ZTIMIMG21
           WHERE ZFAPLDT EQ IT_ZSIMIMG21_DEL-ZFAPLDT
           AND   ZFRAGB  EQ IT_ZSIMIMG21_DEL-ZFRAGB
           AND   ZFCD    EQ IT_ZSIMIMG21_DEL-ZFCD
           AND   WAERS   EQ IT_ZSIMIMG21_DEL-WAERS
           AND   MEINS   EQ IT_ZSIMIMG21_DEL-MEINS.

* Change Document..
    CLEAR : *ZTIMIMG21.
    MOVE-CORRESPONDING IT_ZSIMIMG21_DEL TO  ZTIMIMG21.
    PERFORM   P3000_ZTIMIMG21_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG21_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG21.
    SELECT SINGLE * FROM  ZTIMIMG21
                    WHERE BUKRS    EQ IT_ZSIMIMG21-BUKRS
                    AND   ZFAPLDT  EQ IT_ZSIMIMG21-ZFAPLDT
                    AND   ZFRAGB   EQ IT_ZSIMIMG21-ZFRAGB
                    AND   ZFCD     EQ IT_ZSIMIMG21-ZFCD
                    AND   WAERS    EQ IT_ZSIMIMG21-WAERS
                    AND   MEINS    EQ IT_ZSIMIMG21-MEINS.
    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG21-ZFMINBS   EQ ZTIMIMG21-ZFMINBS   AND
               IT_ZSIMIMG21-ZF45KLT   EQ ZTIMIMG21-ZF45KLT   AND
               IT_ZSIMIMG21-ZF100KLT  EQ ZTIMIMG21-ZF100KLT  AND
               IT_ZSIMIMG21-ZF300KLT  EQ ZTIMIMG21-ZF300KLT  AND
               IT_ZSIMIMG21-ZF500KLT  EQ ZTIMIMG21-ZF500KLT  AND
               IT_ZSIMIMG21-ZF1000KLT EQ ZTIMIMG21-ZF1000KLT AND
               IT_ZSIMIMG21-ZF4000KLT EQ ZTIMIMG21-ZF4000KLT ).
        MOVE-CORRESPONDING ZTIMIMG21    TO *ZTIMIMG21.
        MOVE-CORRESPONDING IT_ZSIMIMG21 TO  ZTIMIMG21.
        MOVE : SY-UNAME         TO   ZTIMIMG21-UNAM,
               SY-DATUM         TO   ZTIMIMG21-UDAT.
        UPDATE ZTIMIMG21.
        IF SY-SUBRC NE 0.   MESSAGE E952.   ENDIF.

* Change Document..
        PERFORM   P3000_ZTIMIMG21_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG21-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG21 TO  ZTIMIMG21.
      MOVE : SY-UNAME  TO   ZTIMIMG21-ERNAM,
             SY-DATUM  TO   ZTIMIMG21-CDAT,
             SY-UNAME  TO   ZTIMIMG21-UNAM,
             SY-DATUM  TO   ZTIMIMG21-UDAT.
      INSERT ZTIMIMG21.
      IF SY-SUBRC NE 0.   MESSAGE E952.   ENDIF.

* Change Document..
      CLEAR : *ZTIMIMG21.
      PERFORM   P3000_ZTIMIMG21_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG21
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG21_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG21_CHANGE_DOC USING   UPD_CHNGIND  .

  OBJECID = ZTIMIMG21(21).

  CALL FUNCTION 'ZTIMIMG21_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID                = OBJECID
            TCODE                   = SY-TCODE
            UTIME                   = SY-UZEIT
            UDATE                   = SY-DATUM
            USERNAME                = SY-UNAME
            N_ZTIMIMG21             = ZTIMIMG21
            O_ZTIMIMG21             = *ZTIMIMG21
            UPD_ZTIMIMG21           = UPD_CHNGIND
            OBJECT_CHANGE_INDICATOR = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZTIMIMG21    = ''
       TABLES
            ICDTXT_ZTIMIMG21        = IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG21_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_DATA_ZTIMIMG21
*&---------------------------------------------------------------------*
FORM P1000_GET_DATA_ZTIMIMG21.

  REFRESH : IT_0210.
  SELECT BUKRS AS BUKRS ZFAPLDT AS ZFAPLDT ZFRAGB AS ZFRAGB
  INTO   CORRESPONDING FIELDS OF TABLE IT_0210 FROM ZTIMIMG21
  GROUP  BY
         BUKRS ZFAPLDT ZFRAGB.

ENDFORM.                    " P1000_GET_DATA_ZTIMIMG21
*&---------------------------------------------------------------------*
*&      Form  P2000_DELETE_LINE
*&---------------------------------------------------------------------*
FORM P2000_DELETE_LINE.
  CASE SY-TCODE.
    WHEN 'ZIMG21'.     " Ç×°øÈ­¹°ÇØ¿Ü¿î¼Û ¿ä?
      LOOP AT IT_ZSIMIMG21 WHERE ZFMARK = 'X'.
*-----------------------------------------------------------------------
* B/L  Ç×°øÈ­¹°ÇØ¿Ü¿î¼Û ¿ä?
* DESC : 2000/06/05 ÇöÀç »ç¿ëÇÏ´Â P/G¾øÀ½.
*-----------------------------------------------------------------------
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG21_DEL = IT_ZSIMIMG21.
          APPEND IT_ZSIMIMG21_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG21.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.
    WHEN 'ZIMG22'.     " ÇØ»óÈ­¹°ÇØ¿Ü¿î¼Û ¿ä?
      LOOP AT IT_ZSIMIMG22 WHERE ZFMARK = 'X'.
        IF W_STATUS NE 'I'.
          IT_ZSIMIMG22_DEL = IT_ZSIMIMG22.
          APPEND IT_ZSIMIMG22_DEL.
        ENDIF.
        DELETE  IT_ZSIMIMG22.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_DELETE_LINE
*&---------------------------------------------------------------------*
*&      Form  P2000_INSERT_LINE
*&---------------------------------------------------------------------*
FORM P2000_INSERT_LINE.
  CASE  SY-TCODE.
    WHEN 'ZIMG21'.
      W_COUNT = 0.
      LOOP AT IT_ZSIMIMG21   WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        ADD   1    TO    W_COUNT.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.         MESSAGE S962.   EXIT.
        WHEN 1.
        WHEN OTHERS.    MESSAGE S965.   EXIT.
      ENDCASE.
      CLEAR : IT_ZSIMIMG21.
      INSERT INITIAL LINE INTO IT_ZSIMIMG21  INDEX  W_TABIX.
    WHEN 'ZIMG22'.
      W_COUNT = 0.
      LOOP AT IT_ZSIMIMG22   WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        ADD   1    TO    W_COUNT.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.         MESSAGE S962.   EXIT.
        WHEN 1.
        WHEN OTHERS.    MESSAGE S965.   EXIT.
      ENDCASE.
      CLEAR : IT_ZSIMIMG22.
      INSERT INITIAL LINE INTO IT_ZSIMIMG22  INDEX  W_TABIX.
  ENDCASE.

ENDFORM.                    " P2000_INSERT_LINE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_DATA_ZTIMIMG22
*&---------------------------------------------------------------------*
FORM P1000_GET_DATA_ZTIMIMG22.

  REFRESH : IT_0220.
  SELECT BUKRS AS BUKRS ZFAPLDT AS ZFAPLDT
  INTO   CORRESPONDING FIELDS OF TABLE IT_0220 FROM ZTIMIMG22
  GROUP  BY
         BUKRS ZFAPLDT.

ENDFORM.                    " P1000_GET_DATA_ZTIMIMG22
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG22_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG22_CHANGE_DOC USING    UPD_CHNGIND.

  OBJECID = ZTIMIMG22(21).

  CALL FUNCTION 'ZTIMIMG22_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID                = OBJECID
            TCODE                   = SY-TCODE
            UTIME                   = SY-UZEIT
            UDATE                   = SY-DATUM
            USERNAME                = SY-UNAME
            N_ZTIMIMG22             = ZTIMIMG22
            O_ZTIMIMG22             = *ZTIMIMG22
            UPD_ZTIMIMG22           = UPD_CHNGIND
            OBJECT_CHANGE_INDICATOR = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZTIMIMG22    = ''
       TABLES
            ICDTXT_ZTIMIMG22        = IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG22_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG22
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG22.

  REFRESH : IT_ZSIMIMG22, IT_ZSIMIMG22_DEL.
* Table Multi-Select
  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG22
           FROM  ZTIMIMG22
           WHERE BUKRS      EQ  ZTIMIMG22-BUKRS
           AND   ZFAPLDT    EQ  ZTIMIMG22-ZFAPLDT.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG21'.
  ENDIF.

  IT_ZSIMIMG22_ORG[] = IT_ZSIMIMG22[].

ENDFORM.                    " P1000_READ_ZTIMIMG22
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG22_MODIFY_CHK
*&---------------------------------------------------------------------*
FORM P2000_ZTIMIMG22_MODIFY_CHK USING    W_GUBUN.

  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG22     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG22_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIMIMG22_ORG.
        READ TABLE IT_ZSIMIMG22 WITH KEY
                              BUKRS     = IT_ZSIMIMG22-BUKRS
                              ZFAPLDT   = IT_ZSIMIMG22-ZFAPLDT
                              ZFCD      = IT_ZSIMIMG22_ORG-ZFCD
                              BINARY SEARCH.

        IF IT_ZSIMIMG22_ORG NE IT_ZSIMIMG22.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIMIMG22_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG22
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG22.

  CLEAR : W_WAERS, W_ZFAPLDT, W_LAND1, W_ZFCD.

* KEY INSERT.
  LOOP AT IT_ZSIMIMG22.
    W_TABIX = SY-TABIX.
    MOVE : ZTIMIMG22-BUKRS    TO  IT_ZSIMIMG22-BUKRS,
           ZTIMIMG22-ZFAPLDT  TO  IT_ZSIMIMG22-ZFAPLDT.

    CLEAR : ZTIMIMG08.
    SELECT SINGLE * FROM ZTIMIMG08 WHERE ZFCDTY EQ '014'
                                   AND   ZFCD   EQ IT_ZSIMIMG22-ZFCD.
    IF NOT ( IT_ZSIMIMG22-ZF20MLB IS INITIAL   AND
             IT_ZSIMIMG22-ZF40MLB IS INITIAL ) AND
             ZTIMIMG08-ZFCD       IS INITIAL.
      MESSAGE  E217(ZIM1) WITH W_TABIX.
      EXIT.
    ENDIF.
    MODIFY  IT_ZSIMIMG22  INDEX  W_TABIX.
  ENDLOOP.

* Internal Table Sort
  SORT IT_ZSIMIMG22  BY  ZFCD.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSIMIMG22.
    IF W_ZFCD    EQ IT_ZSIMIMG22-ZFCD   .
      MESSAGE E969 WITH W_ZFCD.
    ENDIF.

    MOVE : IT_ZSIMIMG22-ZFCD    TO W_ZFCD,
           SY-TABIX             TO W_TABIX.

    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG22-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG22_ORG WITH KEY
                         ZFCD    = IT_ZSIMIMG22-ZFCD.
        IF SY-SUBRC EQ 0.
          MESSAGE E969 WITH W_ZFCD.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG22_DEL.
    DELETE FROM ZTIMIMG22
           WHERE BUKRS   EQ IT_ZSIMIMG22_DEL-BUKRS
           AND   ZFAPLDT EQ IT_ZSIMIMG22_DEL-ZFAPLDT
           AND   ZFCD    EQ IT_ZSIMIMG22_DEL-ZFCD.

* Change Document..
    CLEAR : *ZTIMIMG22.
    MOVE-CORRESPONDING IT_ZSIMIMG22_DEL TO  ZTIMIMG22.
    PERFORM   P3000_ZTIMIMG22_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG22_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG22.

    SELECT SINGLE * FROM  ZTIMIMG22
                    WHERE BUKRS    EQ IT_ZSIMIMG22-BUKRS
                    AND   ZFAPLDT  EQ IT_ZSIMIMG22-ZFAPLDT
                    AND   ZFCD     EQ IT_ZSIMIMG22-ZFCD.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG22-ZFLCL     EQ ZTIMIMG22-ZFLCL     AND
               IT_ZSIMIMG22-ZF20MLB   EQ ZTIMIMG22-ZF20MLB   AND
               IT_ZSIMIMG22-ZF40MLB   EQ ZTIMIMG22-ZF40MLB   AND
               IT_ZSIMIMG22-ZF45MLB   EQ ZTIMIMG22-ZF45MLB   AND
               IT_ZSIMIMG22-ZFHQ      EQ ZTIMIMG22-ZFHQ      ).
        MOVE-CORRESPONDING ZTIMIMG22    TO *ZTIMIMG22.
        MOVE-CORRESPONDING IT_ZSIMIMG22 TO  ZTIMIMG22.
        MOVE : SY-UNAME         TO   ZTIMIMG22-UNAM,
               SY-DATUM         TO   ZTIMIMG22-UDAT.
        UPDATE ZTIMIMG22.
        IF SY-SUBRC NE 0.   MESSAGE E952.   ENDIF.

* Change Document..
        PERFORM   P3000_ZTIMIMG22_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG22-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG22 TO  ZTIMIMG22.
      MOVE : SY-UNAME  TO   ZTIMIMG22-ERNAM,
             SY-DATUM  TO   ZTIMIMG22-CDAT,
             SY-UNAME  TO   ZTIMIMG22-UNAM,
             SY-DATUM  TO   ZTIMIMG22-UDAT.
      INSERT ZTIMIMG22.
      IF SY-SUBRC NE 0.   MESSAGE E952.   ENDIF.

* Change Document..
      CLEAR : *ZTIMIMG22.
      PERFORM   P3000_ZTIMIMG22_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG22
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIEPORT_MODIFY_CHK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_GUBUN  text
*----------------------------------------------------------------------*
FORM P2000_ZTIEPORT_MODIFY_CHK USING    W_GUBUN.

  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIEPORT     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIEPORT_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSIEPORT_ORG.
        READ TABLE IT_ZSIEPORT WITH KEY
                              LAND1     = IT_ZSIEPORT-LAND1
                              PORT      = IT_ZSIEPORT-PORT
                              BINARY SEARCH.

        IF IT_ZSIEPORT_ORG NE IT_ZSIEPORT.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_ZTIEPORT_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIEPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_ZTIEPORT.

  CLEAR : W_LAND1, W_ZFPORT.
  DATA : W_ERR(30).

  SORT IT_ZSIEPORT  BY LAND1 PORT.

* KEY INSERT.
  LOOP AT IT_ZSIMIMG22.
     IF W_LAND1 EQ IT_ZSIEPORT-LAND1 AND W_ZFPORT EQ IT_ZSIEPORT-PORT.
       CONCATENATE W_LAND1 W_ZFPORT INTO W_ERR
          SEPARATED BY SPACE.
        MESSAGE E969 WITH W_ERR.
     ENDIF.

     MOVE : IT_ZSIEPORT-LAND1   TO  W_LAND1,
            IT_ZSIEPORT-PORT    TO  W_ZFPORT,
            SY-TABIX            TO  W_TABIX.

     CONCATENATE W_LAND1 W_ZFPORT INTO W_ERR
        SEPARATED BY SPACE.
    ">> Duplicate Check.
    IF W_STATUS EQ 'I'.
      IF IT_ZSIEPORT-LOEKZ NE 'X'.
        READ TABLE IT_ZSIEPORT WITH KEY
                         LAND1  = IT_ZSIEPORT-LAND1
                         PORT   = IT_ZSIEPORT-PORT.
        IF SY-SUBRC EQ 0.
          MESSAGE E969 WITH W_ERR.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIEPORT_DEL.
    DELETE FROM ZTIEPORT
           WHERE LAND1   EQ IT_ZSIEPORT_DEL-LAND1
           AND   PORT    EQ IT_ZSIEPORT_DEL-PORT.

* Change Document..
    CLEAR : *ZTIEPORT.
    MOVE-CORRESPONDING IT_ZSIEPORT_DEL TO  ZTIEPORT.
    PERFORM   P3000_ZTIEPORT_CHANGE_DOC  USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIEPORT_DEL.

* Insert Data..
  LOOP AT IT_ZSIEPORT.

    SELECT SINGLE * FROM  ZTIEPORT
                    WHERE LAND1    EQ IT_ZSIEPORT-LAND1
                    AND   PORT     EQ IT_ZSIEPORT-PORT.

    IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING ZTIEPORT    TO *ZTIEPORT.
        MOVE-CORRESPONDING IT_ZSIEPORT TO  ZTIEPORT.
        MOVE : SY-UNAME         TO   ZTIEPORT-UNAM,
               SY-DATUM         TO   ZTIEPORT-UDAT.
        UPDATE ZTIEPORT.
        IF SY-SUBRC NE 0.   MESSAGE E952.   ENDIF.

* Change Document..
        PERFORM   P3000_ZTIEPORT_CHANGE_DOC        USING  'U'.
    ELSE.
      IF IT_ZSIEPORT-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIEPORT TO  ZTIEPORT.
      MOVE : SY-UNAME  TO   ZTIEPORT-CNAM,
             SY-DATUM  TO   ZTIEPORT-CDAT,
             SY-UNAME  TO   ZTIEPORT-UNAM,
             SY-DATUM  TO   ZTIEPORT-UDAT.
      INSERT ZTIEPORT.
      IF SY-SUBRC NE 0.   MESSAGE E952.   ENDIF.

* Change Document..
      CLEAR : *ZTIEPORT.
      PERFORM   P3000_ZTIEPORT_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIEPORT

*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIEPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA_MODE  text
*----------------------------------------------------------------------*
FORM SET_LOCK_ZTIEPORT USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIEPORT'.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIEPORT'.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIEPORT

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIEPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTIEPORT.

   REFRESH : IT_ZSIEPORT_ORG, IT_ZSIEPORT, IT_ZSIEPORT_DEL.
* Table Multi-Select
   SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZSIEPORT
            FROM  ZTIEPORT.

   IF SY-SUBRC NE 0.
      MESSAGE I967 WITH 'ZTIEPORT'.
   ENDIF.

   IT_ZSIEPORT_ORG[] = IT_ZSIEPORT[].

ENDFORM.                    " P1000_READ_ZTIEPORT
*&---------------------------------------------------------------------*
*&      Form  P2000_POSITION_INFO_SCR2300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_POSITION_INFO_SCR2300 USING   VALUE(S_CURRENT_LINE) TYPE I
                                         VALUE(S_MAX_LINE) TYPE I.

  DATA : HF1 TYPE I,
         HF2 TYPE I VALUE 10,
         HF3 TYPE I,
         HF4 TYPE I,
         HF5 TYPE I.
  CLEAR : HF1, HF2, HF3, HF4, HF5.
  HF1 = 6.
  HF2 = 10.
  WRITE S_CURRENT_LINE TO POS_TEXT1+HF1(HF2) NO-SIGN.
  HF3 = 3.
  WRITE S_MAX_LINE TO POS_TEXT2+HF3(HF2) NO-SIGN.
  MOVE POS_TEXT1 TO VIM_POSITION_INFO_MASK.
  HF4 = STRLEN( VIM_POSITION_INFO_MASK ) + 1.
  HF5 = STRLEN( POS_TEXT2 ).
  WRITE POS_TEXT2 TO VIM_POSITION_INFO_MASK+HF4(HF5).
  MOVE VIM_POSITION_INFO_MASK TO VIM_POSITION_INFO.
  CONDENSE VIM_POSITION_INFO.

ENDFORM.                    " P2000_POSITION_INFO_SCR2300
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG23
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG23 USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG23'.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG23'.
  ENDIF.

ENDFORM.                    " SET_LOCK_ZTIMIMG23
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIMIMG23_MODIFY_CHK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_ZTIMIMG23_MODIFY_CHK USING    W_GUBUN.

  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSIMIMG23     LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSIMIMG23_ORG LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.     " New Entry
      IF W_COUNTER EQ 0.
         W_GUBUN = 'N'.
      ENDIF.
   ENDIF.

ENDFORM.                    " P2000_ZTIMIMG23_MODIFY_CHK
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG23
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG23.

  CLEAR : W_LAND1, W_ZFPORT, W_ZFGUBN, W_ZFCD.
  DATA : W_ERR(30).

  SORT IT_ZSIMIMG23  BY ZFGUBN ZFCD LAND1 PORT.

* Internal Table Looping(Áßº¹°ËÁõ)
  LOOP AT IT_ZSIMIMG23.
     IF W_LAND1 EQ IT_ZSIMIMG23-LAND1 AND W_ZFPORT EQ IT_ZSIMIMG23-PORT
        AND W_ZFGUBN EQ IT_ZSIMIMG23-ZFGUBN
        AND W_ZFCD EQ IT_ZSIMIMG23-ZFCD.
       CONCATENATE W_ZFGUBN W_ZFCD W_LAND1 W_ZFPORT INTO W_ERR
          SEPARATED BY SPACE.
        MESSAGE E969 WITH W_ERR.
     ENDIF.
     MOVE : IT_ZSIMIMG23-ZFGUBN  TO  W_ZFGUBN,
            IT_ZSIMIMG23-ZFCD    TO  W_ZFCD,
            IT_ZSIMIMG23-LAND1   TO  W_LAND1,
            IT_ZSIMIMG23-PORT    TO  W_ZFPORT,
            SY-TABIX             TO  W_TABIX.

* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõÀÛ¾÷( Áßº¹ ¿À·ù )
     CONCATENATE W_ZFGUBN W_ZFCD W_LAND1 W_ZFPORT INTO W_ERR
        SEPARATED BY SPACE.
    IF W_STATUS EQ 'I'.
      IF IT_ZSIMIMG23-LOEKZ NE 'X'.
        READ TABLE IT_ZSIMIMG23_ORG WITH KEY
                         ZFGUBN = IT_ZSIMIMG23-ZFGUBN
                         ZFCD   = IT_ZSIMIMG23-ZFCD
                         LAND1  = IT_ZSIMIMG23-LAND1
                         PORT   = IT_ZSIMIMG23-PORT.
        IF SY-SUBRC EQ 0.
          MESSAGE E969 WITH W_ERR.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSIMIMG23_DEL.
    DELETE FROM ZTIMIMG23
           WHERE ZFGUBN  EQ IT_ZSIMIMG23_DEL-ZFGUBN
           AND   ZFCD    EQ IT_ZSIMIMG23_DEL-ZFCD
           AND   LAND1   EQ IT_ZSIMIMG23_DEL-LAND1
           AND   PORT    EQ IT_ZSIMIMG23_DEL-PORT.

* Change Document..
    CLEAR : *ZTIMIMG23.
    MOVE-CORRESPONDING IT_ZSIMIMG23_DEL TO  ZTIMIMG23.
    PERFORM   P3000_ZTIMIMG23_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG23_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG23.

    SELECT SINGLE * FROM  ZTIMIMG23
           WHERE ZFGUBN  EQ IT_ZSIMIMG23-ZFGUBN
           AND   ZFCD    EQ IT_ZSIMIMG23-ZFCD
           AND   LAND1   EQ IT_ZSIMIMG23-LAND1
           AND   PORT    EQ IT_ZSIMIMG23-PORT.

    IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING ZTIMIMG23    TO *ZTIMIMG23.
        MOVE-CORRESPONDING IT_ZSIMIMG23 TO  ZTIMIMG23.
        MOVE : SY-UNAME         TO   ZTIMIMG23-UNAME,
               SY-DATUM         TO   ZTIMIMG23-UDAT.
        UPDATE ZTIMIMG23.
        IF SY-SUBRC NE 0.   MESSAGE E952.   ENDIF.
* Change Document..
        PERFORM   P3000_ZTIMIMG23_CHANGE_DOC        USING  'U'.
    ELSE.
      IF IT_ZSIMIMG23-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG23 TO  ZTIMIMG23.
      MOVE : SY-UNAME  TO   ZTIMIMG23-ERNAM,
             SY-DATUM  TO   ZTIMIMG23-CDAT,
             SY-UNAME  TO   ZTIMIMG23-UNAME,
             SY-DATUM  TO   ZTIMIMG23-UDAT.
      INSERT ZTIMIMG23.
      IF SY-SUBRC NE 0.   MESSAGE E952.   ENDIF.
* Change Document..
      CLEAR : *ZTIMIMG23.
      PERFORM   P3000_ZTIMIMG23_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG23

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG23_CHANGE_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_ZTIMIMG23_CHANGE_DOC USING   UPD_CHNGIND.

  OBJECID = ZTIMIMG23(15).

  CALL FUNCTION 'ZTIMIMG23_WRITE_DOCUMENT'
    EXPORTING
      OBJECTID                      = OBJECID
      TCODE                         = SY-TCODE
      UTIME                         = SY-UZEIT
      UDATE                         = SY-DATUM
      USERNAME                      = SY-UNAME
      OBJECT_CHANGE_INDICATOR       = 'U'
      PLANNED_OR_REAL_CHANGES       = ''
      NO_CHANGE_POINTERS            = ''
      UPD_ICDTXT_ZTIMIMG23          = ''
      N_ZTIMIMG23                   = ZTIMIMG23
      O_ZTIMIMG23                   = *ZTIMIMG23
      UPD_ZTIMIMG23                 = UPD_CHNGIND
    TABLES
      ICDTXT_ZTIMIMG23              = IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG23_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_CHANGE_POSITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_CHANGE_POSITION.
  CASE SY-TCODE.
    WHEN 'ZIMG24'.                         " Manage Port by Country.
        CALL SCREEN 0003 STARTING AT 30 6
                       ENDING   AT 55 8.
    WHEN 'ZIMG25'.
        CALL SCREEN 0004 STARTING AT 30 6
                       ENDING   AT 50 8.
  ENDCASE.
ENDFORM.                    " P2000_CHANGE_POSITION
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0003 INPUT.

  CASE SY-UCOMM.
    WHEN 'PICK'.
      IF SY-TCODE EQ 'ZIMG24'.
        IF ZSIEPORT-PORT IS INITIAL.
          READ TABLE IT_ZSIEPORT WITH KEY LAND1 = ZSIEPORT-LAND1.
        ELSE.
          READ TABLE IT_ZSIEPORT WITH KEY PORT = ZSIEPORT-PORT.
        ENDIF.
      ELSE.
        IF  ZSIMIMG23-ZFGUBN IS INITIAL
            AND ZSIMIMG23-ZFCD   IS INITIAL.
           READ TABLE IT_ZSIMIMG23 WITH KEY LAND1 = ZSIMIMG23-LAND1
                                            PORT  = ZSIMIMG23-PORT.
        ELSEIF ZSIMIMG23-ZFCD IS INITIAL.
           READ TABLE IT_ZSIMIMG23 WITH KEY ZFGUBN = ZSIMIMG23-ZFGUBN.
        ELSEIF ZSIMIMG23-ZFGUBN IS INITIAL.
           READ TABLE IT_ZSIMIMG23 WITH KEY ZFCD = ZSIMIMG23-ZFCD.
        ELSE.
           READ TABLE IT_ZSIMIMG23 WITH KEY ZFGUBN = ZSIMIMG23-ZFGUBN
                                            ZFCD = ZSIMIMG23-ZFCD.
        ENDIF.
      ENDIF.
      IF SY-SUBRC EQ 0.
        MOVE SY-TABIX  TO TOP_LINE.
        MOVE 'Y'       TO POSI_GB.
      ELSE.
        MOVE '1'       TO TOP_LINE.
        MOVE 'N'       TO POSI_GB.
      ENDIF.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'CANC'.
      MOVE '1'       TO TOP_LINE.
      MOVE 'N'       TO POSI_GB.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " GET_OK_CODE_SCR0003  INPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIEPORT_CHANGE_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_ZTIEPORT_CHANGE_DOC USING    UPD_CHNGIND.

  OBJECID = ZTIEPORT(9).
  CALL FUNCTION 'ZTIEPORT_WRITE_DOCUMENT'
    EXPORTING
      OBJECTID                      = OBJECID
      TCODE                         = SY-TCODE
      UTIME                         = SY-UZEIT
      UDATE                         = SY-DATUM
      USERNAME                      = SY-UNAME
      OBJECT_CHANGE_INDICATOR       = 'U'
      PLANNED_OR_REAL_CHANGES       = ''
      NO_CHANGE_POINTERS            = ''
      UPD_ICDTXT_ZTIEPORT           = ''
      N_ZTIEPORT                    = ZTIEPORT
      O_ZTIEPORT                    = *ZTIEPORT
      UPD_ZTIEPORT                  = UPD_CHNGIND
    TABLES
      ICDTXT_ZTIEPORT               = IT_CDTXT.

ENDFORM.                    " P3000_ZTIEPORT_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_REFRESH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_REFRESH_DATA.

   REFRESH : IT_ZSIMIMG23_ORG, IT_ZSIMIMG23, IT_ZSIMIMG23_DEL.
   SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG23
            FROM  ZTIMIMG23.
   IF SY-SUBRC NE 0.
      MESSAGE I977 WITH 'Faile refresh data.'.
   ENDIF.
   IT_ZSIMIMG23_ORG[] = IT_ZSIMIMG23[].

ENDFORM.                    " P2000_REFRESH_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_DOWNLOAD_DATA_AS_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_DOWNLOAD_DATA_AS_EXCEL.
   LOOP AT IT_ZSIMIMG24.
     MOVE-CORRESPONDING IT_ZSIMIMG24 TO IT_DOWN.
     APPEND IT_DOWN.
   ENDLOOP.
   CALL FUNCTION 'DOWNLOAD'
        EXPORTING
        FILENAME = 'C:\TEMP.xls'
        FILETYPE = 'WK1'
   TABLES
       DATA_TAB = IT_DOWN.

ENDFORM.                    " P2000_DOWNLOAD_DATA_AS_EXCEL
