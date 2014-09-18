*&---------------------------------------------------------------------*
*& Report  ZRIMCNIO                                                    *
*&---------------------------------------------------------------------*
*&   ÇÁ·Î±×·¥¸í : Container ¹Ý/ÃâÀÔ °ü¸®                               *
*&       ÀÛ¼ºÀÚ : ÀÌ¼®Ã¶ INFOLINK.Ltd                                  *
*&       ÀÛ¼ºÀÏ : 2000.07.08                                           *
*&       ¼öÁ¤ÀÏ : 2001.08.02                                           *
*&---------------------------------------------------------------------*
*& DESC:                                                               *
*&---------------------------------------------------------------------*
*& [º¯°æ³»¿ë]                                                          *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT  ZRIMCNIO  MESSAGE-ID ZIM
                  LINE-SIZE 120
                  NO STANDARD PAGE HEADING.
*&----------------------------------------------------------------------
*&    Tables ¹× º¯¼ö Define
*&----------------------------------------------------------------------
TABLES : ZTBL,          " Bill of Lading Header.
         ZTBLCON,       " B/L Container
         ZTIDS,         " ¼öÀÔ¸éÇã.
         LFA1,          " VENDOR MASTER.
         T001W.         " PLANT NAME.

*& INTERNAL TABLE Á¤ÀÇ-------------------------------------------------*

DATA : IT_TAB      LIKE    ZTBL        OCCURS 0 WITH HEADER LINE.
DATA : IT_FORD     LIKE    LFA1        OCCURS 0 WITH HEADER LINE.
DATA : IT_TRK      LIKE    LFA1        OCCURS 0 WITH HEADER LINE.
DATA : IT_T001W    LIKE    T001W       OCCURS 0 WITH HEADER LINE.
DATA : IT_ZTBLCON  LIKE    ZTBLCON     OCCURS 0 WITH HEADER LINE.

*DATA : BEGIN OF IT_SELECTED  OCCURS 0,
*       ZFHBLNO     LIKE    ZTBL-ZFHBLNO,
*       END OF DATA.

DATA : W_TEM(13),
       INCLUDE(10),
       W_SELECTED        LIKE  ZTBL-ZFHBLNO,
       W_ERR_CHK(1)      TYPE C,
       W_SEQ(2)          TYPE P,
       W_SELECTED_LINES  TYPE P,             " ¼±ÅÃ LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " ÆäÀÌÁö´ç LINE COUNT
       W_COUNT           TYPE I,           " ÀüÃ¼ COUNT
       W_MOD             TYPE I,             " ³ª¸ÓÁö °è»ê¿ë.
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " ÇÊµå¸í.
       W_TABIX           LIKE SY-TABIX.      " TABLE INDEX

*INCLUDE   ZRIMSORTCOM.    " Sort¸¦ À§ÇÑ Include

*INCLUDE   ZRIMUTIL01.     " Utility function ¸ðµâ.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR   ZTBL-BUKRS NO-EXTENSION NO INTERVALS,
                S_TRCK    FOR   ZTBL-ZFTRCK,        " Trucker
                S_WERKS   FOR   ZTBL-ZFWERKS,       " Plant
                S_REBELN  FOR   ZTBL-ZFREBELN,      " ´ëÇ¥ P/O No.
                S_HBLNO   FOR   ZTBL-ZFHBLNO,       " House B/L No.
                S_BLNO    FOR   ZTBL-ZFBLNO,        " B/L °ü¸®¹øÈ£.
                S_MBLNO   FOR   ZTBL-ZFMBLNO,       " ´ëÇ¥ L/C No.
                S_RGDSR   FOR   ZTBL-ZFRGDSR,       " Ç°¸ñ.
                S_ETA     FOR   ZTBL-ZFETA,         " ÀÔÇ×ÀÏ.
                S_BNDT    FOR   ZTBL-ZFBNDT,        " º¸¼¼¿î¼ÛÀÏ.
                S_TRQDT   FOR   ZTBL-ZFTRQDT DEFAULT
                                SY-DATUM,       " ¿î¼Û¿äÃ»ÀÏ.
                S_BLSDP   FOR   ZTBL-ZFBLSDP.       " ¼±Àû¼­·ù ¼ÛºÎÃ³.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
  SET  TITLEBAR  'ZRIM18'.               " GUI TITLE  SETTING
TOP-OF-PAGE.
  IF INCLUDE NE 'CONLST'.
     PERFORM P3000_TITLE_WRITE.           "Çì´õ Ãâ·Â.
  ENDIF.
*&----------------------------------------------------------------------
*&    START-OF-SELECTION Àý.
*&----------------------------------------------------------------------

START-OF-SELECTION.

* ÆÄ¶ó¸ÞÅ¸ ¼³Á¤.

* ·¹Æ÷Æ® °ü·Ã TEXT TABLE SELECT
  PERFORM   P1000_READ_DATA        USING   W_ERR_CHK.
     IF W_ERR_CHK EQ 'Y'.   MESSAGE S738. EXIT.    ENDIF.

* ·¹Æ÷Æ® Write
  PERFORM P3000_DATA_WRITE    USING W_ERR_CHK.
    IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*&----------------------------------------------------------------------
*&      AT USER-COMMAND
*&----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
     WHEN 'DISP'.
           IF IT_TAB-ZFBLNO IS INITIAL.
              MESSAGE S962.
           ELSE.
              SET PARAMETER ID 'ZPHBLNO' FIELD SPACE.
              SET PARAMETER ID 'ZPBLNO'  FIELD IT_TAB-ZFBLNO.
              CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.
           ENDIF.

       WHEN 'CONL'.
           IF IT_TAB-ZFBLNO IS INITIAL.
              MESSAGE S962.
           ELSE.
*              SET PARAMETER ID 'ZPHBLNO' FIELD SPACE.
*              SET PARAMETER ID 'ZPBLNO'  FIELD IT_TAB-ZFBLNO.
               INCLUDE = 'CONLST'.
               CALL SCREEN '0014' STARTING AT 5 5
                                  ENDING   AT 120 20.
           ENDIF.
       WHEN 'CANC'.
           LEAVE TO SCREEN 0.
       WHEN OTHERS.
  ENDCASE.                              "AT USER-COMMMAND
  CLEAR IT_TAB.

*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*FORM P2000_MULTI_SELECTION.
*
*    REFRESH IT_SELECTED.
*    CLEAR   IT_SELECTED.
*    CLEAR W_SELECTED_LINES.
*
*          DO.
*             CLEAR MARKFIELD.
*             READ LINE SY-INDEX FIELD VALUE MARKFIELD.
*
*             IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
*             IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
*                MOVE : IT_ZTBL-ZFBLNO    TO IT_SELECTED-ZFBLNO,
*                       IT_ZTBL-ZFHBLNO   TO IT_SELECTED-ZFHBLNO.
*                APPEND IT_SELECTED.
*                ADD 1 TO W_SELECTED_LINES.
*             ENDIF.
*          ENDDO.
*ENDFORM.                    " P2000_MULTI_SELECTION
*&----------------------------------------------------------------------
*&    Form P3000_DATA_WRITE.        " ¿î¼Û ³»¿ª Ãâ·Â.
*&----------------------------------------------------------------------
FORM P3000_DATA_WRITE   USING W_ERR_CHK.

  SET  PF-STATUS 'ZRIM18'.               " GUI STATUS SETTING
  SET  TITLEBAR  'ZRIM18'.               " GUI TITLE  SETTING

  W_SEQ = 0.    W_LINE = 1.   W_COUNT = 0. W_PAGE = 1.
   LOOP AT IT_TAB.
      PERFORM P2000_PAGE_CHECK.
      PERFORM P3000_LINE_WRITE.

      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.
   ENDLOOP.
   CLEAR IT_TAB.
ENDFORM.                                  "FORM P3000_DATA_WRITE

*&----------------------------------------------------------------------
*&    Form P2000_PAGE_CHECK.
*&----------------------------------------------------------------------
FORM P2000_PAGE_CHECK.

  IF W_LINE >= 20.
     W_PAGE = W_PAGE + 1.  W_LINE = 0.
     NEW-PAGE.
   ENDIF.

ENDFORM.                                  "FORM P2000_PAGE_CHECK.

*&----------------------------------------------------------------------
*&    Form P3000_LAST_WRITE.
*&----------------------------------------------------------------------
FORM P3000_LAST_WRITE.

  IF  W_COUNT GT 0.
      FORMAT RESET.
*      WRITE : W_COUNT TO W_COUNT LEFT-JUSTIFIED.
*      CONCATENATE 'ÃÑ' W_COUNT '°Ç' INTO W_COUNT
*                  SEPARATED BY SPACE.
      WRITE : /103 'ÃÑ', W_COUNT,'°Ç'.

  ENDIF.
ENDFORM.                                 "FORM P3000_LAST_WRITE.

*&----------------------------------------------------------------------
*&    Form P1000_READ_DATA           " ÀÇ·Ú ³»¿ª Á¶È¸.
*&----------------------------------------------------------------------

FORM P1000_READ_DATA             USING W_ERR_CHK.

   MOVE 'N' TO W_ERR_CHK.

  CLEAR   :  IT_TAB, IT_ZTBLCON, IT_FORD, IT_TRK, IT_T001W.
  REFRESH :  IT_TAB, IT_ZTBLCON, IT_FORD, IT_TRK, IT_T001W.

  SELECT * INTO TABLE IT_TAB FROM ZTBL
                WHERE   ZFTRCK   IN   S_TRCK
                  AND   BUKRS    IN   S_BUKRS
                  AND   ZFWERKS  IN   S_WERKS
                  AND   ZFHBLNO  IN   S_HBLNO
                  AND   ZFMBLNO  IN   S_MBLNO
                  AND   ZFBLNO   IN   S_BLNO
                  AND   ZFRGDSR  IN   S_RGDSR
                  AND   ZFETA    IN   S_ETA
                  AND   ZFBNDT   IN   S_BNDT
                  AND   ZFTRQDT  IN   S_TRQDT
                  AND NOT ( ZFTRQDT  IS   NULL
                  OR        ZFTRQDT  EQ   '00000000' )
                  AND   ZFTRCK   IN   S_TRCK
                  AND   ZFBLSDP  IN   S_BLSDP
                  AND   ZFSHTY   EQ   'F'
                  AND   ZFBLNO   IN
                        ( SELECT ZFBLNO
                                 FROM ZTBLCON
                                 GROUP BY ZFBLNO ).
   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'. EXIT.
   ENDIF.

   SORT IT_TAB BY ZFREBELN.

   SELECT * INTO TABLE IT_FORD FROM LFA1
            FOR ALL ENTRIES IN IT_TAB
            WHERE  LIFNR EQ IT_TAB-ZFFORD.

   SELECT * INTO TABLE IT_TRK FROM LFA1
            FOR ALL ENTRIES IN IT_TAB
            WHERE  LIFNR EQ IT_TAB-ZFTRCK.

   SELECT * INTO TABLE IT_T001W FROM T001W
            FOR ALL ENTRIES IN IT_TAB
            WHERE  WERKS EQ IT_TAB-ZFWERKS.

   SELECT * INTO TABLE IT_ZTBLCON FROM ZTBLCON
            FOR ALL ENTRIES IN IT_TAB
            WHERE  ZFBLNO EQ IT_TAB-ZFBLNO.

   SORT  IT_ZTBLCON BY ZFBLNO.

ENDFORM.                                  "FORM P1000_READ_DATA

*&----------------------------------------------------------------------
*&    Form P3000_TITLE_WRITE         "Çì´õ Ãâ·Â.
*&----------------------------------------------------------------------
FORM P3000_TITLE_WRITE.
  SKIP 2.
  WRITE: /55 '[ ÄÁÅ×ÀÌ³Ê ¿î¼ÛÀÇ·Ú ÇöÈ²]'  COLOR 1 INTENSIFIED.
  WRITE: /109 SY-DATUM, / SY-ULINE.
  FORMAT COLOR 1 INTENSIFIED.
  WRITE: / SY-VLINE NO-GAP,
           (13)'P/O No.'      NO-GAP,   SY-VLINE NO-GAP,
           (17)'House B/L NO' NO-GAP,   SY-VLINE NO-GAP,
           (10)'ÀÔÇ×ÀÏ'       NO-GAP,   SY-VLINE NO-GAP,
           (15)'Forwader'     NO-GAP,   SY-VLINE NO-GAP,
           (12)'Trucker'      NO-GAP,   SY-VLINE NO-GAP,
           (8)'½Å°íÇüÅÂ'      NO-GAP,   SY-VLINE NO-GAP,
           (4)'20ft.'         NO-GAP,   SY-VLINE NO-GAP,
           (4)'40ft.'         NO-GAP,   SY-VLINE NO-GAP,
           (20)'Plant'        NO-GAP,   SY-VLINE NO-GAP,
           (6)'ºñ°í'          NO-GAP,   SY-VLINE NO-GAP,
           SY-ULINE NO-GAP.
ENDFORM.                                 "FORM P3000_TITLE_WRITE.
*&----------------------------------------------------------------------
*&    Form RESET_LIST
*&----------------------------------------------------------------------
FORM RESET_LIST.

   MOVE 0 TO SY-LSIND.

      W_PAGE  = 1.
      W_LINE  = 1.
      W_COUNT = 0.

ENDFORM.                                  "FORM RESET_LIST.

*&---------------------------------------------------------------------*
*&    Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
DATA : L_TEXT(08).

  W_MOD = W_LINE MOD 2.

  IF IT_TAB-ZFSHNO IS INITIAL.
      WRITE : IT_TAB-ZFREBELN TO W_TEM LEFT-JUSTIFIED.
  ELSE.
      WRITE : IT_TAB-ZFREBELN TO W_TEM LEFT-JUSTIFIED.
      CONCATENATE W_TEM '-' IT_TAB-ZFSHNO INTO W_TEM.
  ENDIF.

  READ TABLE IT_FORD  WITH KEY LIFNR = IT_TAB-ZFFORD.
  READ TABLE IT_T001W WITH KEY WERKS = IT_TAB-ZFWERKS.
  READ TABLE IT_TRK WITH KEY LIFNR = IT_TAB-ZFTRCK.

  CASE IT_TAB-ZFRPTTY.
     WHEN 'A'.     L_TEXT = 'ÀÔÇ×Àü'.
     WHEN 'B'.     L_TEXT = 'º¸¼¼¿î¼Û'.
     WHEN 'D'.     L_TEXT = 'ºÎµÎÅë°ü'.
     WHEN 'N'.     L_TEXT = 'ÀÚ°¡Ã¢°í'.
     WHEN 'W'.     L_TEXT = '¿µ¾÷Ã¢°í'.
     WHEN 'I'.     L_TEXT = 'In-Bulk'.
     WHEN ''.      L_TEXT = '¹ÌÅë°ü'.
     WHEN OTHERS.  CLEAR : L_TEXT.
  ENDCASE.

  IF W_MOD NE 0.
     FORMAT RESET.
     FORMAT COLOR COL_NORMAL INTENSIFIED ON.
       WRITE:/ SY-VLINE NO-GAP,
               (13)W_TEM            NO-GAP,   SY-VLINE NO-GAP,
               (17)IT_TAB-ZFHBLNO   NO-GAP,   SY-VLINE NO-GAP,
               (10)IT_TAB-ZFETA     NO-GAP,   SY-VLINE NO-GAP,
               (15)IT_FORD-NAME1    NO-GAP,   SY-VLINE NO-GAP,
               (12)IT_TRK-NAME1     NO-GAP,   SY-VLINE NO-GAP,
               (8) L_TEXT           NO-GAP,   SY-VLINE NO-GAP,
               (4)IT_TAB-ZF20FT     NO-GAP,   SY-VLINE NO-GAP,
               (4)IT_TAB-ZF40FT     NO-GAP,   SY-VLINE NO-GAP,
               (20)IT_T001W-NAME1   NO-GAP,   SY-VLINE NO-GAP,
               '      '             NO-GAP,   SY-VLINE NO-GAP.
      HIDE IT_TAB.
      HIDE W_TEM.
      HIDE IT_FORD.
      HIDE IT_TRK.
      HIDE IT_T001W.
  ELSE.
     FORMAT RESET.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE:/ SY-VLINE NO-GAP,
               (13)W_TEM            NO-GAP,   SY-VLINE NO-GAP,
               (17)IT_TAB-ZFHBLNO   NO-GAP,   SY-VLINE NO-GAP,
               (10)IT_TAB-ZFETA     NO-GAP,   SY-VLINE NO-GAP,
               (15)IT_FORD-NAME1    NO-GAP,   SY-VLINE NO-GAP,
               (12)IT_TRK-NAME1     NO-GAP,   SY-VLINE NO-GAP,
               (8) L_TEXT           NO-GAP,   SY-VLINE NO-GAP,
               (4)IT_TAB-ZF20FT     NO-GAP,   SY-VLINE NO-GAP,
               (4)IT_TAB-ZF40FT     NO-GAP,   SY-VLINE NO-GAP,
               (20)IT_T001W-NAME1   NO-GAP,   SY-VLINE NO-GAP,
               '      '             NO-GAP,   SY-VLINE NO-GAP.
      HIDE IT_TAB.
      HIDE W_TEM.
      HIDE IT_FORD.
      HIDE IT_TRK.
      HIDE IT_T001W.
   ENDIF.
      W_COUNT = W_COUNT + 1.
      W_LINE  = W_LINE  + 1.
  WRITE : / SY-ULINE.
ENDFORM.                                 "FORM P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Module  D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
MODULE D0014_STATUS_SCR0014 OUTPUT.

  SET PF-STATUS 'CONLST'.
  SET TITLEBAR  'CONLST'.
*  CASE INCLUDE.
*     WHEN 'CONLST'.
*        SET TITLEBAR 'POPU' WITH 'Message status'.
*     WHEN 'BLHELP'.
*        SET TITLEBAR 'POPU' WITH 'Partial reference LIST'.
*     WHEN OTHERS.
*  ENDCASE.
  SUPPRESS DIALOG.
ENDMODULE.                 " D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
MODULE D0014_LIST_CHECK_SCR0014 INPUT.

   LEAVE TO LIST-PROCESSING.
*   LOOP AT IT_ZTBLCON WHERE ZFBLNO = ZPBLNO.

       SKIP 1.
       WRITE:/50 '[ Container lis ]'   COLOR 1 INTENSIFIED.
       SKIP 1.
* HEADER ITEM.--------------------------------------------

     FORMAT RESET.
     FORMAT COLOR 1 INTENSIFIED OFF.
       WRITE:/2 '',
               (15)W_TEM            ,
               (19)IT_TAB-ZFHBLNO    ,
               (35)IT_FORD-NAME1     ,
               (35)IT_T001W-NAME1    , SY-ULINE+(110).

*     FORMAT RESET.
*     FORMAT COLOR 1 INTENSIFIED.
*       WRITE:/ SY-ULINE+(110)        ,
*             / SY-VLINE              ,
*               (15)'´ëÇ¥ P/O No.' CENTERED,  SY-VLINE,
*               (19)'B/L No.'      CENTERED,  SY-VLINE,
*               (31)'Forwader'     CENTERED,  SY-VLINE,
*               (32)'Plant'        CENTERED,  SY-VLINE, SY-ULINE+(110).

     FORMAT RESET.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE:/ SY-VLINE              ,
               (15)'Container No.'    CENTERED,  SY-VLINE,
                (8)'Type'             CENTERED,  SY-VLINE,
                (8)'Size'             CENTERED,  SY-VLINE,
               (31)'Container Yard'   CENTERED,  SY-VLINE,
               (10)'Transportation'         CENTERED,  SY-VLINE,
               (19)'Person(TEL)' CENTERED,
               SY-VLINE, SY-ULINE+(110).


   CLEAR W_COUNT.
   LOOP AT IT_ZTBLCON WHERE ZFBLNO = IT_TAB-ZFBLNO.

     W_MOD = W_COUNT MOD 2.
     IF W_MOD NE 0.
       FORMAT RESET.
       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
         WRITE:/ SY-VLINE              ,
                 (15)IT_ZTBLCON-ZFCONNO,      SY-VLINE,
                  (8)IT_ZTBLCON-ZFCOTP ,      SY-VLINE,
                  (8)IT_ZTBLCON-ZFCOSZ ,      SY-VLINE,
                 (31)IT_ZTBLCON-ZFWHCYT,      SY-VLINE,
                 (10)IT_ZTBLCON-ZFCOTR ,      SY-VLINE,
                 (19)IT_ZTBLCON-ZFRCVER,      SY-VLINE, SY-ULINE+(110).
         W_COUNT  = W_COUNT  + 1.
     ELSE.
       FORMAT RESET.
       FORMAT COLOR COL_NORMAL INTENSIFIED ON.
         WRITE:/ SY-VLINE              ,
                 (15)IT_ZTBLCON-ZFCONNO,      SY-VLINE,
                  (8)IT_ZTBLCON-ZFCOTP ,      SY-VLINE,
                  (8)IT_ZTBLCON-ZFCOSZ ,      SY-VLINE,
                 (31)IT_ZTBLCON-ZFWHCYT,      SY-VLINE,
                 (10)IT_ZTBLCON-ZFCOTR ,      SY-VLINE,
                 (19)IT_ZTBLCON-ZFRCVER,      SY-VLINE, SY-ULINE+(110).
         W_COUNT  = W_COUNT  + 1.
     ENDIF.
   ENDLOOP.
   FORMAT RESET.
   WRITE:/100 'Total', W_COUNT, 'CaseÇ'.
ENDMODULE.                 " D0014_LIST_CHECK_SCR0014  INPUT
