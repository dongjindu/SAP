*&---------------------------------------------------------------------*
*& Report  ZRIMBLNLIST                                                 *
*&---------------------------------------------------------------------*
*&     Program : Shipment information not created                      *
*&  Created By : SC, Lee INFOLINK Ltd.                                 *
*&  Created On : 2001.08.24                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :                                                       *
*&---------------------------------------------------------------------*
*& [Change History]                                                    *
*& 2003.07.14 SY, Jung(INFOLINK Ltd.)                                  *
*&            List up with Header item type by Import request.         *
*&---------------------------------------------------------------------*
REPORT  ZRIMBLNLIST  MESSAGE-ID ZIM
                     LINE-SIZE 176
                     NO STANDARD PAGE HEADING.

* TABLES
TABLES : ZTREQIT, ZTBLIT, ZTREQHD, ZTREQST, LFA1, ZTIMIMG00.
TABLES : ZVREQHD_IT.
TABLES : ZSREQHD, SPOP.

* BLIT INTERNAL TABLE.
DATA : BEGIN OF IT_ZTBLIT OCCURS 0,
       ZFREQNO   LIKE    ZTBLIT-ZFREQNO,
       ZFITMNO   LIKE    ZTBLIT-ZFITMNO,
       BLMENGE   LIKE    ZTBLIT-BLMENGE.
DATA : END OF IT_ZTBLIT.

* Internal Table of View Read.
DATA : BEGIN OF IT_REQNO OCCURS 0,
       ZFREQNO   LIKE    ZTREQST-ZFREQNO.
DATA : END OF IT_REQNO.

* Internal Table of List
DATA : BEGIN OF IT_TAB OCCURS 0.
        INCLUDE STRUCTURE ZVREQHD_IT.
DATA : LLIEF_NM    LIKE    LFA1-NAME1,
       SUM_BL      LIKE    ZTBLIT-BLMENGE,
       SUM_REQ     LIKE    ZTBLIT-BLMENGE,
       RE_ITEM     LIKE    ZTBLIT-BLMENGE,
       EKGRP_NM(30) TYPE C,
       BEZEI       LIKE    T618T-BEZEI.
DATA : END OF IT_TAB.

DATA : IT_ZSBLIT      LIKE ZSBLIT    OCCURS 100 WITH HEADER LINE.

DATA : ANTWORT(1),
       OK-CODE    LIKE  SY-UCOMM,
       W_ERR_CHK(1),
       W_TABIX    LIKE   SY-TABIX,
       W_FIELD_NM(20),
       W_COUNT    TYPE I,
       W_MOD      TYPE I,
       W_SELECTED_LINES  TYPE P,
       W_EKGRP    LIKE ZTREQST-EKGRP,
       P_BUKRS    LIKE ZTREQHD-BUKRS.

DATA : W_ELIKZ   LIKE  EKPO-ELIKZ,   "Indicator of Delivery Completed
       W_LOEKZ   LIKE  EKPO-LOEKZ.   "Indicator of Deletion

DATA: BEGIN OF IT_SELECTED OCCURS 0.
        INCLUDE STRUCTURE IT_TAB.
DATA: END OF IT_SELECTED.

DATA: BEGIN OF IT_SELECTED_TEMP OCCURS 0.
        INCLUDE STRUCTURE IT_TAB.
DATA: END OF IT_SELECTED_TEMP.

*----------------------------------------------------------------------*
* INCLUDE.                                                             *
*----------------------------------------------------------------------*
INCLUDE   ZRIMSORTCOM.

*----------------------------------------------------------------------*
* SELECTION SCREEN.                                                    *
*----------------------------------------------------------------------*
SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS
                              NO-EXTENSION NO INTERVALS,
                S_EBELN   FOR ZTREQHD-EBELN,    " P/O Number
                S_LIFNR   FOR ZTREQHD-LIFNR,    " vendor
                S_EKGRP   FOR ZTREQST-EKGRP,    " Pur. Group.
                S_MATNR   FOR ZTREQIT-MATNR,    " Material Code.
                S_TXZ01   FOR ZTREQIT-TXZ01,    " Description.
                S_OPNNO   FOR ZTREQHD-ZFOPNNO,  " L/C No.
                S_REQTY   FOR ZTREQHD-ZFREQTY,  " Import Request Type
                S_REQNO   FOR ZTREQHD-ZFREQNO,  " Import Request Doc.
                S_OPNNM   FOR ZTREQST-ZFOPNNM,  " In Charge of Opnning .
                S_CDAT    FOR ZTREQST-CDAT,     " Requested Date
                S_SHCU    FOR ZTREQHD-ZFSHCU,   " Loading Port.
                S_INCO1   FOR ZTREQHD-INCO1.    " Incoterms.
SELECTION-SCREEN END OF BLOCK B1.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS : P_ITEM AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.
*----------------------------------------------------------------------*
* INITIALIZATION.                                                      *
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM  P1000_SET_BUKRS.
  SET TITLEBAR  'ZIMR21'.

TOP-OF-PAGE.
  IF P_ITEM EQ 'X'.
    IF SY-LANGU EQ '3'.
      PERFORM   P3000_TITLE_WRITE.
    ELSE.
      PERFORM   P3000_TITLE_WRTE_EN.
    ENDIF.
  ELSE.
    IF SY-LANGU EQ '3'.
      PERFORM   P3000_TITLE_WRITE_HEADER.
    ELSE.
      PERFORM   P3000_TITLE_WRTE_EN_HEADER.
    ENDIF.
  ENDIF.
*----------------------------------------------------------------------*
* START OF SELECTION.                                                  *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM   P1000_READ_DATA   USING  W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.
    MESSAGE S966. EXIT.
  ENDIF.

  IF P_ITEM EQ 'X'.
    PERFORM   P3000_DATA_WRITE.
  ELSE.
    PERFORM   P3000_DATA_WRITE_HEADER.
  ENDIF.
  PERFORM   P3000_LAST_WRITE.
*----------------------------------------------------------------------*
* USER COMMAND.                                                        *
*----------------------------------------------------------------------*
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.

*   Sort Ascending, Sort Descending
    WHEN 'STUP' OR 'STDN'.
      W_FIELD_NM = 'EBELN'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES IT_TAB USING SY-UCOMM.

*   B/L Create
    WHEN 'CRBL'.
      IF IT_TAB IS INITIAL.
        MESSAGE S962.
      ELSE.
*       < 2003.7.4 ljh > markfield 추가.
        PERFORM P2000_MULTI_SELECTION.
        PERFORM P2000_DATA_COMBINE_CHECK.
        IF W_SELECTED_LINES GE 1.
          SPOP-TITEL = 'House B/L No. Input Screen'.
          CALL SCREEN 0014 STARTING AT 27 6
                           ENDING   AT 70 8.

          IF ANTWORT EQ 'Y'.

            EXPORT IT_ZSBLIT           TO MEMORY ID 'BLIT'.
            SET PARAMETER ID 'ZPHBLNO' FIELD ZSREQHD-ZFHBLNO.
            SET PARAMETER ID 'ZPREQNO' FIELD IT_SELECTED-ZFREQNO.
            CALL TRANSACTION 'ZIM21' AND SKIP FIRST SCREEN.
            SY-LSIND = SY-LSIND - 1.
            PERFORM P1000_READ_DATA   USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.
              LEAVE TO SCREEN 0.
            ENDIF.

            PERFORM RESET_LIST.
          ENDIF.
        ENDIF.
      ENDIF.

*  Display Import Request.
    WHEN 'DISP'.
      IF IT_TAB IS INITIAL.
        MESSAGE S962.
      ELSE.

        PERFORM P2000_MULTI_SELECTION.

        IF W_SELECTED_LINES EQ 1.
          SET PARAMETER ID 'ZPOPNNO'   FIELD  SPACE.
          SET PARAMETER ID 'BES'       FIELD  SPACE.
          SET PARAMETER ID 'ZPREQNO'   FIELD  IT_SELECTED-ZFREQNO.

          CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.

          PERFORM P1000_READ_DATA   USING W_ERR_CHK.
          IF W_ERR_CHK EQ 'Y'.
            LEAVE TO SCREEN 0.
          ENDIF.

          PERFORM RESET_LIST.
        ELSEIF W_SELECTED_LINES GT 1.
          MESSAGE E965.
        ENDIF.
      ENDIF.

*   Refresh
    WHEN 'REFR'.
      SY-LSIND = 0.
      PERFORM P1000_READ_DATA   USING W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.

      PERFORM RESET_LIST.
    WHEN OTHERS.
  ENDCASE.

  CLEAR IT_TAB.

*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0014  OUTPUT
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0014 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_BL_DOCUMENT_SCR0014  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_BL_DOCUMENT_SCR0014 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC' OR 'NO'.   EXIT.
    WHEN OTHERS.
  ENDCASE.

  IF ZSREQHD-ZFHBLNO IS INITIAL.   " B/L NO를 입력하지 않았을 경우.
    MESSAGE E304.
  ELSE.
* B/L NO에 Count
*        SELECT COUNT( * ) INTO  W_COUNT
*                          FROM  ZTBL
*                          WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
*        CASE W_COUNT.
*           WHEN 0.     MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
*           WHEN 1.
*              SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
*                            FROM ZTBL
*                            WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
*                   EXIT.
*              ENDSELECT.
*           WHEN OTHERS.
*              PERFORM P2000_BL_DOC_ITEM_SELECT.
*              IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
*              PERFORM P2000_SEARCH_FIELD_MOVE.
*        ENDCASE.
  ENDIF.

ENDMODULE.                 " CHECK_BL_DOCUMENT_SCR0014  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0014  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0014 INPUT.

  CASE OK-CODE.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN OTHERS.   EXIT.
  ENDCASE.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " GET_OK_CODE_SCR3512  INPUT
*&---------------------------------------------------------------------*
*&      Form  p2000_multi_selection
*&---------------------------------------------------------------------*
* < 2003.7.4 ljh > p2000_multi_selection 추가.
FORM P2000_MULTI_SELECTION.

  CLEAR IT_SELECTED. REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE-CORRESPONDING IT_TAB TO IT_SELECTED.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  W_ERR_CHK = 'N'.
  IF W_SELECTED_LINES EQ 0.
    MESSAGE S951.
    W_ERR_CHK = 'Y'.
  ENDIF.

  IF P_ITEM IS INITIAL.
     IT_SELECTED_TEMP[] = IT_SELECTED[].
     LOOP AT IT_SELECTED_TEMP.
        LOOP AT IT_TAB WHERE ZFREQNO EQ IT_SELECTED_TEMP-ZFREQNO.
           IF IT_TAB-ZFITMNO EQ IT_SELECTED_TEMP-ZFITMNO.
              CONTINUE.
           ENDIF.
           MOVE-CORRESPONDING IT_TAB  TO  IT_SELECTED.
           APPEND  IT_SELECTED.
        ENDLOOP.
     ENDLOOP.
  ENDIF.

ENDFORM.                    " p2000_multi_selection
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRTE_EN
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRTE_EN.

  FORMAT RESET.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  '[ Shipment information not created ]'
               COLOR COL_HEADING INTENSIFIED OFF.

  SKIP 1.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED.
  WRITE : /2 'Date : ' NO-GAP, SY-DATUM NO-GAP.
  WRITE : / SY-ULINE(116).

  WRITE:/ SY-VLINE NO-GAP, '   '         NO-GAP.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:  SY-VLINE NO-GAP, (16) 'Imp.Req.No'      NO-GAP,
          SY-VLINE NO-GAP, (20) 'Vendor'          NO-GAP,
          SY-VLINE NO-GAP, (30) 'L/C Approve No'  NO-GAP,
          SY-VLINE NO-GAP, (20) 'Loading Port'    NO-GAP,
          SY-VLINE NO-GAP, (20) 'Arriving Port'   NO-GAP,
          SY-VLINE NO-GAP.

  FORMAT COLOR COL_BACKGROUND INTENSIFIED.
  WRITE:/ SY-VLINE NO-GAP, '   '         NO-GAP.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:  SY-VLINE NO-GAP, (16) 'P/O No - Item'    NO-GAP,
          SY-VLINE NO-GAP, (20) 'Material'         NO-GAP,
          SY-VLINE NO-GAP, (30) 'Material Desc.'   NO-GAP,
          SY-VLINE NO-GAP, (20) 'Imp.Req.Qty'      NO-GAP,
          SY-VLINE NO-GAP, (20) 'B/L not ref quan' NO-GAP,
          SY-VLINE NO-GAP,
          SY-ULINE(116) NO-GAP.

ENDFORM.                    " P3000_TITLE_WRTE_EN
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRTE_EN_HEADER.
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRTE_EN_HEADER.

  FORMAT RESET.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  '[ Shipment information not created ]'
               COLOR COL_HEADING INTENSIFIED OFF.

  SKIP 1.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED.
  WRITE : /2 'Date : ' NO-GAP, SY-DATUM NO-GAP.
  WRITE : / SY-ULINE.

  WRITE:/ SY-VLINE NO-GAP, '   '         NO-GAP.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:  SY-VLINE NO-GAP, (10) 'Imp.Req.No'           NO-GAP,
          SY-VLINE NO-GAP, (20) 'Vendor'               NO-GAP,
          SY-VLINE NO-GAP, (30) 'L/C Approve No'       NO-GAP,
          SY-VLINE NO-GAP, (10) 'P/O No.'              NO-GAP,
          SY-VLINE NO-GAP, (18) 'Material'             NO-GAP,
          SY-VLINE NO-GAP, (35) 'Material Discription' NO-GAP,
          SY-VLINE NO-GAP, (20) 'Loading Port'         NO-GAP,
          SY-VLINE NO-GAP, (20) 'Arriving Port'        NO-GAP,
          SY-VLINE NO-GAP,                    SY-ULINE NO-GAP.

ENDFORM.                    " P3000_TITLE_WRTE_EN_HEADER
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

  CLEAR : ZTIMIMG00, P_BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
    MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
  ENDIF.

*>> 회사코드 SET.
  MOVE: 'I'          TO S_BUKRS-SIGN,
        'EQ'         TO S_BUKRS-OPTION,
        P_BUKRS      TO S_BUKRS-LOW.
  APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
*----------------------------------------------------------------------*
* FORM  P1000_READ_DATA.                                               *
*----------------------------------------------------------------------*
FORM P1000_READ_DATA     USING W_ERR_CHK.

  RANGES : R_REQNO  FOR   ZTREQHD-ZFREQNO  OCCURS 0.

  REFRESH : IT_TAB, IT_REQNO, IT_ZTBLIT.   ", IT_ZVREQHD_IT.

  W_ERR_CHK = 'N'.

  SELECT DISTINCT H~ZFREQNO
              INTO CORRESPONDING FIELDS OF TABLE IT_REQNO
              FROM  ZTREQHD AS H INNER JOIN  ZTREQST AS S
              ON    H~ZFREQNO = S~ZFREQNO
                    WHERE H~BUKRS    IN  S_BUKRS
                      AND S~EKGRP    IN  S_EKGRP
                      AND H~ZFOPNNO  IN  S_OPNNO
                      AND H~LIFNR    IN  S_LIFNR
                      AND H~ZFREQTY  IN  S_REQTY
                      AND H~ZFREQNO  IN  S_REQNO
                      AND H~ZFSHCU   IN  S_SHCU
                      AND H~INCO1    IN  S_INCO1
                      AND S~ZFOPNNM  IN  S_OPNNM
                      AND S~CDAT     IN  S_CDAT
                      AND H~ZFCLOSE  EQ  SPACE.

  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'. EXIT.
  ENDIF.

  SORT IT_REQNO BY ZFREQNO.

  LOOP AT IT_REQNO.
    IF NOT S_EBELN[] IS INITIAL.
      SELECT COUNT( * ) INTO W_COUNT
             FROM ZTREQIT
            WHERE ZFREQNO  EQ  IT_REQNO-ZFREQNO
              AND EBELN    IN  S_EBELN.
      IF W_COUNT IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    MOVE : 'I'              TO R_REQNO-SIGN,
           'EQ'             TO R_REQNO-OPTION,
           IT_REQNO-ZFREQNO TO R_REQNO-LOW,
           SPACE            TO R_REQNO-HIGH.
    APPEND  R_REQNO.
    CLEAR : W_COUNT.
  ENDLOOP.

  IF  R_REQNO IS INITIAL.
    W_ERR_CHK = 'Y'. EXIT.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
           FROM    ZVREQHD_IT
          WHERE    ZFREQNO   IN   R_REQNO
            AND    MATNR     IN   S_MATNR
            AND    TXZ01     IN   S_TXZ01.

  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'. EXIT.
  ENDIF.

  SELECT ZFREQNO ZFITMNO SUM( BLMENGE ) AS BLMENGE
         INTO CORRESPONDING FIELDS OF TABLE IT_ZTBLIT
         FROM ZTBLIT
         WHERE  ZFREQNO   IN   R_REQNO
         GROUP BY ZFREQNO ZFITMNO.

  LOOP AT IT_TAB.
    CLEAR W_EKGRP.
    W_TABIX = SY-TABIX.

*-------<P/O has Deletion Mark, Delevery Completed Mark -> Deleted>
    SELECT SINGLE  ELIKZ     LOEKZ
           INTO (W_ELIKZ,  W_LOEKZ)
           FROM   EKPO
           WHERE  EBELN   EQ   IT_TAB-EBELN
           AND    EBELP   EQ   IT_TAB-EBELP.

    IF SY-SUBRC EQ 0.
      IF W_LOEKZ NE SPACE.
        DELETE IT_TAB INDEX W_TABIX.
        CONTINUE.
      ENDIF.
*      IF W_ELIKZ EQ 'X'.
*        DELETE IT_TAB INDEX W_TABIX.
*        CONTINUE.
*      ENDIF.
    ENDIF.
*------------------------------------------------------------>
    SELECT SINGLE EKGRP
      INTO W_EKGRP
      FROM ZTREQST
     WHERE ZFREQNO = IT_TAB-ZFREQNO.

    SELECT SINGLE EKNAM INTO IT_TAB-EKGRP_NM
      FROM T024
     WHERE EKGRP = W_EKGRP.

    MODIFY  IT_TAB INDEX W_TABIX.
  ENDLOOP.

  SORT IT_TAB BY ZFREQNO EBELN EBELP.

ENDFORM.                                       "P1000_READ_DATA.
*----------------------------------------------------------------------*
* FORM P3000_DATA_WRITE                                                *
*----------------------------------------------------------------------*
FORM P3000_DATA_WRITE.

  DATA W_ZFREQNO LIKE ZTREQIT-ZFREQNO.

  SET TITLEBAR  'ZIMR21'.
  SET PF-STATUS 'ZIMR21'.                      " GUI STATUS SETTING

  FORMAT RESET.
  CLEAR : W_TABIX, W_COUNT, W_MOD, W_ZFREQNO.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    READ TABLE IT_ZTBLIT WITH KEY ZFREQNO = IT_TAB-ZFREQNO
                                  ZFITMNO = IT_TAB-ZFITMNO.
    IF SY-SUBRC NE 0.
      IT_TAB-SUM_REQ = IT_TAB-MENGE.
      IT_TAB-SUM_BL  = 0.
      IT_TAB-RE_ITEM = IT_TAB-MENGE.
      MODIFY IT_TAB INDEX W_TABIX.
    ELSE.
      IT_TAB-SUM_REQ = IT_TAB-MENGE.
      IT_TAB-SUM_BL  = IT_ZTBLIT-BLMENGE.
      IT_TAB-RE_ITEM = IT_TAB-MENGE - IT_ZTBLIT-BLMENGE.
      MODIFY IT_TAB INDEX W_TABIX.
    ENDIF.

    IF IT_TAB-RE_ITEM LE 0.
      DELETE IT_TAB INDEX W_TABIX.
      CONTINUE.
    ELSE.

      IF W_ZFREQNO NE IT_TAB-ZFREQNO.
        CLEAR : LFA1.
        SELECT SINGLE * FROM LFA1
                WHERE LIFNR = IT_TAB-LLIEF.

        IF W_COUNT NE 0.
          WRITE: SY-ULINE(116).
        ENDIF.

        FORMAT COLOR COL_BACKGROUND INTENSIFIED.
        WRITE:/ SY-VLINE NO-GAP, (03) ''              NO-GAP.

        FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
        WRITE:  SY-VLINE NO-GAP, (16) IT_TAB-ZFREQNO   NO-GAP,
                SY-VLINE NO-GAP, (20) LFA1-NAME1       NO-GAP,
                SY-VLINE NO-GAP, (30) IT_TAB-ZFOPNNO   NO-GAP,
                SY-VLINE NO-GAP, (03) IT_TAB-ZFSHCU    NO-GAP,
                                 (02) '/ '             NO-GAP,
                                 (15) IT_TAB-ZFSPRT    NO-GAP,
                SY-VLINE NO-GAP, (20) IT_TAB-ZFAPRT    NO-GAP,
                SY-VLINE NO-GAP.

        W_COUNT = W_COUNT + 1.
        W_MOD = 0.
        HIDE : IT_TAB.

        W_ZFREQNO = IT_TAB-ZFREQNO.
      ENDIF.

      W_MOD = W_MOD MOD 2.

      FORMAT COLOR COL_BACKGROUND INTENSIFIED.
      WRITE:  / SY-VLINE NO-GAP, ' ' NO-GAP,
              MARKFIELD AS CHECKBOX NO-GAP, ' '      NO-GAP.

      IF W_MOD = 0.
        FORMAT COLOR COL_NORMAL INTENSIFIED ON.
      ELSE.
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      ENDIF.

      WRITE:  SY-VLINE NO-GAP, (10) IT_TAB-EBELN    NO-GAP,
                               (01) '-'             NO-GAP,
                               (05) IT_TAB-EBELP    NO-GAP,
              SY-VLINE NO-GAP, (20) IT_TAB-MATNR    NO-GAP,
              SY-VLINE NO-GAP, (30) IT_TAB-TXZ01    NO-GAP,
              SY-VLINE NO-GAP, (17) IT_TAB-SUM_REQ
                                    UNIT IT_TAB-MEINS NO-GAP,
                               (03) IT_TAB-MEINS    NO-GAP,
              SY-VLINE NO-GAP, (17) IT_TAB-RE_ITEM
                                    UNIT IT_TAB-MEINS NO-GAP,
                               (03) IT_TAB-MEINS    NO-GAP,
              SY-VLINE NO-GAP.

      W_MOD   = W_MOD   + 1.
      HIDE IT_TAB.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " P3000_DATA_WRITE
*----------------------------------------------------------------------*
* FORM P3000_DATA_WRITE_HEADER.                                        *
*----------------------------------------------------------------------*
FORM P3000_DATA_WRITE_HEADER.

  DATA W_ZFREQNO LIKE ZTREQIT-ZFREQNO.

  SET TITLEBAR  'ZIMR21'.
  SET PF-STATUS 'ZIMR21'.                      " GUI STATUS SETTING

  FORMAT RESET.
  CLEAR : W_TABIX, W_COUNT, W_MOD, W_ZFREQNO.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    READ TABLE IT_ZTBLIT WITH KEY ZFREQNO = IT_TAB-ZFREQNO
                                  ZFITMNO = IT_TAB-ZFITMNO.
    IF SY-SUBRC NE 0.
      IT_TAB-SUM_REQ = IT_TAB-MENGE.
      IT_TAB-SUM_BL  = 0.
      IT_TAB-RE_ITEM = IT_TAB-MENGE.
      MODIFY IT_TAB INDEX W_TABIX.
    ELSE.
      IT_TAB-SUM_REQ = IT_TAB-MENGE.
      IT_TAB-SUM_BL  = IT_ZTBLIT-BLMENGE.
      IT_TAB-RE_ITEM = IT_TAB-MENGE - IT_ZTBLIT-BLMENGE.
      MODIFY IT_TAB INDEX W_TABIX.
    ENDIF.

    IF IT_TAB-RE_ITEM LE 0.
      DELETE IT_TAB INDEX W_TABIX.
      CONTINUE.
    ELSE.

*     < 2003.7.11 JSY > 출력형식 변경.
      IF W_ZFREQNO NE IT_TAB-ZFREQNO.
        CLEAR : LFA1.
        SELECT SINGLE * FROM LFA1
                WHERE LIFNR = IT_TAB-LLIEF.

        IF W_COUNT NE 0.
          WRITE: SY-ULINE(176).
        ENDIF.

        FORMAT COLOR COL_BACKGROUND INTENSIFIED.
        WRITE:  SY-VLINE NO-GAP, ' ' NO-GAP,
                MARKFIELD AS CHECKBOX NO-GAP, ' '      NO-GAP.

        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
        WRITE:  SY-VLINE NO-GAP, (10) IT_TAB-ZFREQNO   NO-GAP,
                SY-VLINE NO-GAP, (20) LFA1-NAME1       NO-GAP,
                SY-VLINE NO-GAP, (30) IT_TAB-ZFOPNNO   NO-GAP,
                SY-VLINE NO-GAP, (10) IT_TAB-EBELN     NO-GAP,
                SY-VLINE NO-GAP, (18) IT_TAB-MATNR     NO-GAP,
                SY-VLINE NO-GAP, (35) IT_TAB-TXZ01     NO-GAP,
                SY-VLINE NO-GAP, (03) IT_TAB-ZFSHCU    NO-GAP,
                                 (02) '/ '             NO-GAP,
                                 (15) IT_TAB-ZFSPRT    NO-GAP,
                SY-VLINE NO-GAP, (20) IT_TAB-ZFAPRT    NO-GAP,
                SY-VLINE NO-GAP.

        W_COUNT = W_COUNT + 1.
        W_MOD = 0.
        HIDE : IT_TAB.

        W_ZFREQNO = IT_TAB-ZFREQNO.
      ENDIF.

      HIDE IT_TAB.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " P3000_DATA_WRITE_HEADER.
*----------------------------------------------------------------------*
* FORM P3000_LAST_WRITE                                                *
*----------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED.

  IF W_COUNT GT 0.
    IF P_ITEM EQ 'X'.
      WRITE: SY-ULINE(116).
    ELSE.
      WRITE: SY-ULINE(176).
    ENDIF.
    IF SY-LANGU EQ '3'.
      WRITE : /2 '총 :'NO-GAP , W_COUNT NO-GAP, '건' NO-GAP.
    ELSE.
      WRITE : /2 'Total :'NO-GAP , W_COUNT NO-GAP, 'Case' NO-GAP.
    ENDIF.
  ELSE.
    MESSAGE S009. EXIT.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*----------------------------------------------------------------------*
* P3000_TITLE_WRITE.                                                   *
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  FORMAT RESET.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  '[ 선적정보 미등록 현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.

  SKIP 1.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED.
  WRITE : /2 'Date : ' NO-GAP, SY-DATUM NO-GAP.
  WRITE : / SY-ULINE(176).

  WRITE:/ SY-VLINE NO-GAP, '   '         NO-GAP.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:  SY-VLINE NO-GAP, (10) '수입의뢰번호'  NO-GAP,
          SY-VLINE NO-GAP, (20) 'Vendor'        NO-GAP,
          SY-VLINE NO-GAP, (30) 'L/C 개설번호'  NO-GAP,
          SY-VLINE NO-GAP, (10) 'P/O No.'       NO-GAP,
          SY-VLINE NO-GAP, (18) '자재코드'      NO-GAP,
          SY-VLINE NO-GAP, (35) '자재명'        NO-GAP,
          SY-VLINE NO-GAP, (20) '선적항'        NO-GAP,
          SY-VLINE NO-GAP, (20) '도착항'        NO-GAP,
          SY-VLINE NO-GAP,       SY-ULINE(176)  NO-GAP.

ENDFORM.                    " P3000_TITLE_WRITE.
*----------------------------------------------------------------------*
* P3000_TITLE_WRITE_HEADER.                                            *
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE_HEADER.

  FORMAT RESET.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  '[ 선적정보 미등록 현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.

  SKIP 1.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED.
  WRITE : /2 'Date : ' NO-GAP, SY-DATUM NO-GAP.
  WRITE : / SY-ULINE(176).

  WRITE:/ SY-VLINE NO-GAP, '   '         NO-GAP.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:  SY-VLINE NO-GAP, (10) '수입의뢰번호'  NO-GAP,
          SY-VLINE NO-GAP, (20) 'Vendor'        NO-GAP,
          SY-VLINE NO-GAP, (30) 'L/C 개설번호'  NO-GAP,
          SY-VLINE NO-GAP, (10) 'P/O No.'       NO-GAP,
          SY-VLINE NO-GAP, (18) '자재코드'      NO-GAP,
          SY-VLINE NO-GAP, (35) '자재명'        NO-GAP,
          SY-VLINE NO-GAP, (20) '선적항'        NO-GAP,
          SY-VLINE NO-GAP, (20) '도착항'        NO-GAP,
          SY-VLINE NO-GAP,       SY-ULINE(176)  NO-GAP.

ENDFORM.                    " P3000_TITLE_WRITE_HEADER.
*----------------------------------------------------------------------*
* FORM RESET_LIST                                                      *
*----------------------------------------------------------------------*
FORM RESET_LIST.

  IF P_ITEM EQ 'X'.
    IF SY-LANGU EQ '3'.
      PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
    ELSE.
      PERFORM   P3000_TITLE_WRTE_EN.
    ENDIF.
  ELSE.
    IF SY-LANGU EQ '3'.
      PERFORM   P3000_TITLE_WRITE_HEADER.           " 해더 출력...
    ELSE.
      PERFORM   P3000_TITLE_WRTE_EN_HEADER.
    ENDIF.
  ENDIF.

  IF P_ITEM EQ 'X'.
    PERFORM   P3000_DATA_WRITE.
  ELSE.
    PERFORM   P3000_DATA_WRITE_HEADER.
  ENDIF.
  PERFORM   P3000_LAST_WRITE.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_COMBINE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_DATA_COMBINE_CHECK.

  DATA : WL_CURRENCY  LIKE ZTREQHD-WAERS,
         WL_VENDOR    LIKE ZTREQHD-LIFNR,
         WL_BENI      LIKE ZTREQHD-LIFNR,
         WL_BUKRS     LIKE ZTREQHD-BUKRS.

  READ TABLE IT_SELECTED INDEX 1.
  MOVE : IT_SELECTED-WAERS  TO  WL_CURRENCY,
         IT_SELECTED-LIFNR  TO  WL_VENDOR,
         IT_SELECTED-ZFBENI TO  WL_BENI,
         IT_SELECTED-BUKRS  TO  WL_BUKRS.

  REFRESH : IT_ZSBLIT.
  LOOP AT IT_SELECTED.
     ">> Currency Check
     IF WL_CURRENCY NE IT_SELECTED-WAERS.
        MESSAGE I379 WITH WL_CURRENCY IT_SELECTED-ZFREQNO
                                      IT_SELECTED-WAERS.
        EXIT.
     ENDIF.
     ">> Vendor Check
     IF WL_VENDOR  NE IT_SELECTED-LIFNR.
        MESSAGE I380 WITH WL_VENDOR IT_SELECTED-ZFREQNO
                                    IT_SELECTED-LIFNR.
        EXIT.
     ENDIF.
     ">> Beneficiary Check
     IF WL_BENI  NE IT_SELECTED-ZFBENI.
        MESSAGE E381 WITH WL_BENI IT_SELECTED-ZFREQNO
                                  IT_SELECTED-ZFBENI.
        EXIT.
     ENDIF.
     ">> Company Code Check.
     IF WL_BUKRS  NE  IT_SELECTED-BUKRS.
        MESSAGE E382 WITH WL_BUKRS IT_SELECTED-ZFREQNO
                                   IT_SELECTED-BUKRS.
        EXIT.
     ENDIF.
     ">> Item Append.
     MOVE-CORRESPONDING IT_SELECTED  TO IT_ZSBLIT.
     APPEND IT_ZSBLIT.
  ENDLOOP.

  SORT IT_ZSBLIT BY ZFREQNO ZFITMNO.

ENDFORM.                    " P2000_DATA_COMBINE_CHECK
