*&----------------------------------------------------------------
*& Report  ZRIMBWGRLST
*&----------------------------------------------------------------
*& Program      : Carry-in Declaration List without B/L.
*& Created by   : Nashinho INFOLINK Ltd.
*& Created Date : 2001.09.14
*&----------------------------------------------------------------
*& Description  :
*&----------------------------------------------------------------
*& [Change Info]
*&----------------------------------------------------------------
REPORT  ZRIMBWGRLST  MESSAGE-ID ZIM
                     LINE-SIZE 122
                     NO STANDARD PAGE HEADING.
TABLES:  ZTBL,ZTBLINR_TMP,*ZTBLINR_TMP,LFA1,ZTIMIMG03.
*-----------------------------------------------------------------
*  Internal Table for List Display.
*-----------------------------------------------------------------

DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFBLNO     LIKE     ZTBL-ZFBLNO,         " B/L No.
       ZFTBLNO    LIKE     ZTBLINR_TMP-ZFTBLNO, " B/L No.
       ZFREBELN   LIKE     ZTBL-ZFREBELN,       " P/O No
       ZFHBLNO    LIKE     ZTBL-ZFHBLNO,        " HOUSE B/L No.
       ZFINDT     LIKE     ZTBLINR_TMP-ZFINDT,  " Carry-in Date.
       ZFSHNO     LIKE     ZTBL-ZFSHNO,         " Shipment Sequence.
*       ZFLOC      LIKE     ZTBLINR_TMP-ZFLOC,
       ZFSPOS     LIKE     ZTBLINR_TMP-ZFSPOS,  "
       ZFABNAR    LIKE     ZTBLINR_TMP-ZFABNAR,
       ZFBNARM    LIKE     ZTIMIMG03-ZFBNARM,
       ZFMCYN     LIKE     ZTBLINR_TMP-ZFMCYN,
       ZFBTSEQ    LIKE     ZTBLINR_TMP-ZFBTSEQ,
       LIFNR      LIKE     ZTBLINR_TMP-LIFNR,
       NAME1      LIKE     LFA1-NAME1,
       ZFINRNO    LIKE     ZTBLINR_TMP-ZFINRNO,
       ZFCARNM    LIKE     ZTBLINR_TMP-ZFCARNM,
*       ZFPKCN     LIKE     ZTBLINR_TMP-ZFPKCN,
       ZFOKPK     LIKE     ZTBLINR_TMP-ZFOKPK,
       ZFPKCNM    LIKE     ZTBLINR_TMP-ZFPKCNM,
*       ZFTOWT     LIKE     ZTBLINR_TMP-ZFTOWT,
       ZFINWT     LIKE     ZTBLINR_TMP-ZFINWT,
       ZFTOWTM    LIKE     ZTBLINR_TMP-ZFTOWTM,
       ERNAM      LIKE     ZTBLINR_TMP-ERNAM,
       CDAT       LIKE     ZTBLINR_TMP-CDAT.
DATA :  END OF IT_TAB.

DATA : BEGIN OF IT_SELECTED OCCURS 0,
       ZFBLNO     LIKE     ZTBL-ZFBLNO,            " B/L 관리번?
       ZFTBLNO    LIKE     ZTBLINR_TMP-ZFTBLNO,    " 관리번?
       EBELN      LIKE     ZTBL-ZFREBELN,          " P/O No
       ZFHBLNO    LIKE     ZTBL-ZFHBLNO.           " HOUSE B/L
DATA :  END OF IT_SELECTED.

DATA : BEGIN OF IT_SELECTED1 OCCURS 0,
       ZFBLNO     LIKE     ZTBL-ZFBLNO,            " B/L 관리번?
       ZFHBLNO    LIKE     ZTBL-ZFHBLNO.           " HOUSE B/L
DATA :  END OF IT_SELECTED1.

DATA : IT_ZSREQHD  LIKE ZSREQHD OCCURS 50 WITH HEADER LINE.
DATA: BEGIN OF IT_EXCL OCCURS 20,
      FCODE    LIKE RSMPE-FUNC.
DATA: END   OF IT_EXCL.

DATA :  W_ERR_CHK     TYPE C,
        W_FIELD_NM    TYPE C,
        W_MCYN        LIKE  ZTBLINR_TMP-ZFMCYN,
        W_SELECTED_LINES TYPE I,
        W_PAGE        TYPE I,
        W_DOM_TEX1     LIKE DD07T-DDTEXT,
        W_FNAME        LIKE ZTIMIMG08-ZFCDNM,
        W_LINE        TYPE I,
        W_ZFSTATUS,
        W_COUNT       TYPE I,
        W_DESC(12),
        W_BUTTON_ANSWER   TYPE C,
        W_LCOUNT       TYPE I,
        W_TABIX       LIKE SY-TABIX,
        W_LIST_INDEX  LIKE SY-TABIX,
        W_ZFBLNO      LIKE ZTBL-ZFBLNO.
DATA:   W_STATUS_CHK,
        INCLUDE(08),
        W_MOD             TYPE   I.
DATA  ANTWORT(1).

INCLUDE   ZRIMSORTCOM.    " REPORT Sort
INCLUDE   ZRIMUTIL01.     " Utility Function
INCLUDE   ZRIMMESSAGE.    " Import System Message function Include

*-----------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_EBELN   FOR ZTBL-ZFREBELN,     "
                S_HBLNO   FOR ZTBL-ZFHBLNO,      "
                S_BLNO    FOR ZTBL-ZFBLNO,
                S_ERNAM   FOR ZTBLINR_TMP-ERNAM,
                S_CDAT    FOR ZTBLINR_TMP-CDAT.
*PARAMETERS : P_MCYN    AS CHECKBOX.              " 매치여부.

SELECTION-SCREEN END OF BLOCK B1.

*>> Match Y/N .
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN : BEGIN OF LINE.
SELECTION-SCREEN : COMMENT 12(8) TEXT-021, POSITION 23.
PARAMETERS :       P_NO    AS CHECKBOX.                 " 출고미처리.
SELECTION-SCREEN : COMMENT 50(8) TEXT-022, POSITION 60.
PARAMETERS :       P_YES   AS CHECKBOX.                 " 출고처리완료.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P2000_INIT.

*title Text Write
TOP-OF-PAGE.
  IF INCLUDE NE 'BLLST'.
     PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
  ENDIF.
*-----------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------
START-OF-SELECTION.
* B/L 테이블 SELECT
  PERFORM   P1000_READ_DATA USING W_ERR_CHK.
  IF W_ERR_CHK = 'Y'.
    MESSAGE S738. EXIT.
  ENDIF.
* 레포트 Write
  PERFORM  P3000_DATA_WRITE.

*-----------------------------------------------------------------
* User Command
*-----------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN 'STUP' OR 'STDN'.         " SORT 선택?
      W_FIELD_NM = 'ZFHBLNO'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
    WHEN 'REFR'.
      PERFORM   P1000_READ_DATA USING W_ERR_CHK.
      PERFORM RESET_LIST.
    WHEN 'DISP'.                       " 반입신고.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
         READ TABLE IT_SELECTED INDEX 1.
         PERFORM P2000_DISP_ZTBLINR_TMP USING
                              IT_SELECTED-ZFBLNO
                              IT_SELECTED-ZFHBLNO
                              IT_SELECTED-EBELN.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'DISP1'.                       " B/L 조회.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_DISP_ZTBL(SAPMZIM09) USING
                             IT_SELECTED-ZFBLNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'EXCU'.                         " UPDATE LOGIC.
      PERFORM P2000_MULTI_SELECTION .
      IF W_SELECTED_LINES EQ 1.
         READ TABLE IT_SELECTED INDEX 1.
         PERFORM P2000_UDATE_ZFBLINR_INOU USING
                             IT_SELECTED-ZFTBLNO
                             IT_SELECTED-ZFHBLNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
      PERFORM RESET_LIST.
    WHEN 'DELE'.                        " UPDATE LOGIC.
      PERFORM P2000_MULTI_SELECTION .
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_DELETE_ZFBLINR_INOU USING
                                          IT_SELECTED-ZFTBLNO.

      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
      PERFORM RESET_LIST.
    WHEN 'DOWN'.          " FILE DOWNLOAD....
      PERFORM P3000_TO_PC_DOWNLOAD.
    WHEN 'CANC'.
       ANTWORT = 'C'.
       LEAVE TO SCREEN 0.
   ENDCASE.
*---------------------------------------------------------
*AT LINE-SELECTION.
*---------------------------------------------------------
AT LINE-SELECTION.
  CASE INCLUDE.
    WHEN 'BLLST'.
      PERFORM P2000_MULTI_SELECTION1.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED1 INDEX 1.
        W_ZFBLNO   = IT_SELECTED1-ZFBLNO.
        ANTWORT = 'Y'.
        LEAVE TO SCREEN 0.
       ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
        EXIT.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
*&----------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&----------------------------------------------------------------
  FORM P3000_TITLE_WRITE.

  SKIP 1.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50 '[ Carry-in Declaration List]'
              COLOR COL_HEADING INTENSIFIED OFF.
  SKIP 1.
  WRITE :/ 'Date :',SY-DATUM.

  WRITE : / SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,'',
            SY-VLINE,(12) 'P/O No.'          CENTERED,
            SY-VLINE,(24) 'House B/L No'     CENTERED,
            SY-VLINE,(10) 'Car-in Dat'       CENTERED,
            SY-VLINE,(23) 'Vessel Name'      CENTERED,
            SY-VLINE,(20) 'Bonded Area'      CENTERED,
            SY-VLINE,(10) 'Create.Dat'       CENTERED,
            SY-VLINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: /  SY-VLINE,'',
            SY-VLINE,(12) 'B/L No.'          CENTERED,
            SY-VLINE,(24) 'Carry-in Declaration No.'     CENTERED,
            SY-VLINE,(10) 'Create.Dat'       CENTERED,
            SY-VLINE,(23) 'Weight(M/T)'      CENTERED,
            SY-VLINE,(20) 'Vendor'           CENTERED,
            SY-VLINE,(10) 'Storg.Loc'        CENTERED,
            SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&----------------------------------------------------------------
*&      Form  P1000_GET_IT_TAB
*&----------------------------------------------------------------
FORM P1000_READ_DATA USING W_ERR_CHK.

 W_ERR_CHK = 'N'.

 RANGES: R_ZFMCYN FOR ZTBLINR_TMP-ZFMCYN OCCURS 5.

 IF P_YES IS INITIAL AND P_NO IS INITIAL.
    MESSAGE  S977 WITH 'Choose MATCH classification.'.
    W_ERR_CHK  =  'Y'. EXIT.
 ENDIF.

 IF P_YES EQ 'X'.      " MATCH 여부.
    MOVE :  'I'       TO  R_ZFMCYN-SIGN,
            'EQ'      TO  R_ZFMCYN-OPTION,
            'X'       TO  R_ZFMCYN-LOW,
            SPACE     TO  R_ZFMCYN-HIGH.
    APPEND  R_ZFMCYN.
  ENDIF.
  IF P_NO  EQ 'X'.
    MOVE :    'I'       TO  R_ZFMCYN-SIGN,
              'EQ'      TO  R_ZFMCYN-OPTION,
              SPACE     TO  R_ZFMCYN-LOW,
              SPACE     TO  R_ZFMCYN-HIGH.
    APPEND  R_ZFMCYN.
  ENDIF.

*>>  B/L
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
    FROM ZTBLINR_TMP
   WHERE ZFREBELN  IN  S_EBELN
     AND ZFHBLNO   IN  S_HBLNO
     AND ZFBLNO    IN  S_BLNO
     AND ERNAM     IN  S_ERNAM
     AND ZFMCYN    IN  R_ZFMCYN
     AND CDAT      IN  S_CDAT.

  IF SY-SUBRC <> 0.  W_ERR_CHK = 'Y'.   EXIT.    ENDIF.

  LOOP AT IT_TAB.
     W_TABIX = SY-TABIX.
     CLEAR ZTIMIMG03.
     SELECT SINGLE *
            FROM  ZTIMIMG03
            WHERE ZFBNAR = IT_TAB-ZFABNAR.
     MOVE ZTIMIMG03-ZFBNARM TO IT_TAB-ZFBNARM.
     CLEAR LFA1.
     SELECT SINGLE *
          FROM  LFA1
          WHERE LIFNR = IT_TAB-LIFNR.
     MOVE LFA1-NAME1 TO IT_TAB-NAME1.
     MODIFY  IT_TAB INDEX W_TABIX.
   ENDLOOP.
   DESCRIBE TABLE IT_TAB LINES W_LINE.
   IF W_LINE EQ 0.
      W_ERR_CHK = 'Y'.
   ENDIF.

ENDFORM.                    " P1000_READ_DATA
*&----------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&----------------------------------------------------------------
FORM P3000_DATA_WRITE .

  SET TITLEBAR  'ZIMR66' WITH 'Carry-in declaration status'.
  SET PF-STATUS 'ZIMR66' EXCLUDING IT_EXCL.
  CLEAR W_COUNT.
  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    PERFORM   P3000_LINE_WRITE.
    AT LAST.
      PERFORM P3000_LINE_TOTAL.
    ENDAT.
  ENDLOOP.
  CLEAR: IT_TAB,W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&----------------------------------------------------------------
*&      Form  P2000_INIT
*&----------------------------------------------------------------
FORM P2000_INIT.

  SET TITLEBAR  'ZIMR66' WITH 'Carry-in declaration status'.
  P_NO   =  'X'.

ENDFORM.                    " P2000_INIT
*&----------------------------------------------------------------
*&      Form  P3000_LINE_WRITE
*&----------------------------------------------------------------
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE : / SY-VLINE,MARKFIELD  AS CHECKBOX,
            SY-VLINE,(12) IT_TAB-ZFREBELN,
            SY-VLINE,(24) IT_TAB-ZFHBLNO,
            SY-VLINE,(10) IT_TAB-ZFINDT,
            SY-VLINE,(23) IT_TAB-ZFCARNM,
            SY-VLINE,(20) IT_TAB-ZFBNARM,
            SY-VLINE,(10) IT_TAB-ERNAM,
            SY-VLINE.
  HIDE : IT_TAB, W_TABIX.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE: /  SY-VLINE,'',
            SY-VLINE,(12) IT_TAB-ZFBLNO,
            SY-VLINE,(24) IT_TAB-ZFINRNO,
            SY-VLINE,(10) IT_TAB-CDAT,
            SY-VLINE,(19) IT_TAB-ZFINWT UNIT
                          IT_TAB-ZFTOWTM,
                     (03) IT_TAB-ZFTOWTM,
            SY-VLINE,(20) IT_TAB-NAME1,
            SY-VLINE,(10) IT_TAB-ZFSPOS,
            SY-VLINE.
  HIDE : IT_TAB, W_TABIX.
  WRITE : / SY-ULINE.
  W_COUNT = W_COUNT + 1.

ENDFORM.
*&----------------------------------------------------------------
*&      Form  RESET_LIST
*&----------------------------------------------------------------
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.
  PERFORM   P1000_READ_DATA USING W_ERR_CHK.
  PERFORM   P3000_TITLE_WRITE.     " 해더 출력...
  PERFORM   P3000_DATA_WRITE.

ENDFORM.                    " RESET_LIST
*&----------------------------------------------------------------
*&      Form  P3000_LINE_TOTAL
*&----------------------------------------------------------------
FORM P3000_LINE_TOTAL.

  WRITE:/ 'Total:',W_COUNT, 'Case'.

ENDFORM.                    " P3000_LINE_TOTAL
*&----------------------------------------------------------------
*&      Form  P2000_MULTI_SELECTION
*&----------------------------------------------------------------
FORM P2000_MULTI_SELECTION.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.
  DO.
   CLEAR MARKFIELD.
   READ LINE SY-INDEX FIELD VALUE MARKFIELD.
   IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
   IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
        MOVE : IT_TAB-ZFBLNO   TO IT_SELECTED-ZFBLNO,
               IT_TAB-ZFHBLNO  TO IT_SELECTED-ZFHBLNO,
               IT_TAB-ZFTBLNO  TO IT_SELECTED-ZFTBLNO,
               IT_TAB-ZFREBELN TO IT_SELECTED-EBELN.
        APPEND IT_SELECTED.
        ADD 1 TO W_SELECTED_LINES.
   ENDIF.
  ENDDO.
  IF W_SELECTED_LINES = 0.
     MESSAGE E951.
  ENDIF.

ENDFORM.                    " P2000_MULTI_SELECTION
*&----------------------------------------------------------------
*&      Form  P2000_DISP_ZTBLINR_TMP
*&----------------------------------------------------------------
FORM P2000_DISP_ZTBLINR_TMP USING    P_ZFBLNO P_ZFHBLNO P_EBELN.

  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPHBLNO' FIELD P_ZFHBLNO.
  SET PARAMETER ID 'BES '    FIELD P_EBELN.

  CALL TRANSACTION 'ZIMI8'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTBLINR_TMP
*&----------------------------------------------------------------
*&      Form  P2000_UDATE_ZFBLINR_INOU
*&----------------------------------------------------------------
FORM P2000_UDATE_ZFBLINR_INOU USING  P_ZFTBLNO P_ZFHBLNO.

 DATA: W_SY_UCOMM LIKE SY-UCOMM.
 W_SY_UCOMM = SY-UCOMM.
*>> 한수원 수정.ㅣ
* SELECT COUNT( * ) INTO W_COUNT
*        FROM ZTBL
*       WHERE ZFHBLNO = P_ZFHBLNO.
 IF NOT P_ZFHBLNO IS INITIAL.
    MESSAGE  S977 WITH
    'This is already matched with B/L Information.'.
    EXIT.
 ENDIF.

 SELECT COUNT( * ) INTO W_COUNT
        FROM ZTBL
       WHERE ZFBLST EQ '2'
         AND ZFINDT EQ '00000000'
         AND ZFRPTTY EQ 'N'.

 CASE  W_COUNT.
   WHEN 0.
      MESSAGE S977 WITH
      'There is no Self Bond B/L without refered'.
      EXIT.
   WHEN OTHERS.
      PERFORM P2000_BLDATA_SELECT  USING P_ZFHBLNO.
 ENDCASE.
 IF ANTWORT EQ 'Y'.       " 확인일 경우.
     CLEAR ZTBLINR_TMP.
     SELECT SINGLE *
          FROM ZTBLINR_TMP
         WHERE ZFTBLNO = P_ZFTBLNO.
      *ZTBLINR_TMP = ZTBLINR_TMP.
      MOVE W_ZFBLNO TO ZTBLINR_TMP-ZFBLNO.
*>> P/O NUMBER, 구매처 매치.
      CLEAR ZTBL.
      SELECT SINGLE *
             FROM ZTBL
            WHERE ZFBLNO = W_ZFBLNO.
        MOVE : ZTBL-BUKRS     TO   ZTBLINR_TMP-BUKRS,
               ZTBL-ZFBLNO    TO   ZTBLINR_TMP-ZFBLNO,
               ZTBL-ZFMATGB   TO   ZTBLINR_TMP-ZFMATGB,
               ZTBL-LIFNR     TO   ZTBLINR_TMP-LIFNR,
               ZTBL-ZFREBELN  TO   ZTBLINR_TMP-ZFREBELN,
               ZTBL-ZFSHNO    TO   ZTBLINR_TMP-ZFSHNO,
               ZTBL-ZFPKCN    TO   ZTBLINR_TMP-ZFPKCN,
               ZTBL-ZFTOWT    TO   ZTBLINR_TMP-ZFTOWT,
               ZTBL-ZFTOWTM   TO   ZTBLINR_TMP-ZFTOWTM,
               ZTBL-ZFTOVL    TO   ZTBLINR_TMP-ZFTOVL,
               ZTBL-ZFTOVLM   TO   ZTBLINR_TMP-ZFTOVLM,
               SY-UNAME       TO   ZTBLINR_TMP-ZFGINM,
               ZTBL-ZF20FT    TO   ZTBLINR_TMP-ZF20FT,
               ZTBL-ZF40FT    TO   ZTBLINR_TMP-ZF40FT,
               ZTBL-ZFSHTY    TO   ZTBLINR_TMP-ZFSHTY,
               ZTBL-ZFFORD    TO   ZTBLINR_TMP-ZFFORD.
      PERFORM P2000_CALL_FUNCTION USING W_SY_UCOMM.
 ENDIF.

ENDFORM.                    " P2000_UDATE_ZFBLINR_INOU
*&----------------------------------------------------------------
*&      Form  P2000_BLDATA_SELECT
*&----------------------------------------------------------------
FORM P2000_BLDATA_SELECT USING P_ZFHBLNO.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
*> 한수원 수정.
*  SELECT *
*     FROM ZTBL
*    WHERE ZFHBLNO EQ P_ZFHBLNO
*    ORDER  BY ZFBLNO.

   SELECT * FROM ZTBL
           WHERE ZFBLST EQ '2'
             AND ZFINDT EQ '00000000'
             AND ZFRPTTY EQ 'N'
           ORDER BY ZFBLNO.

    MOVE: ZTBL-ZFREBELN TO IT_ZSREQHD-EBELN,
          ZTBL-ZFBLNO   TO IT_ZSREQHD-ZFBLNO,
          ZTBL-ZFSHNO   TO IT_ZSREQHD-ZFSHNO,
          ZTBL-ZFHBLNO  TO IT_ZSREQHD-ZFHBLNO,
          ZTBL-LIFNR    TO IT_ZSREQHD-LLIEF,
          ZTBL-ZFETA    TO IT_ZSREQHD-ZFETA,
          ZTBL-ZFBLST   TO IT_ZSREQHD-ZFBLST.
     APPEND IT_ZSREQHD.
  ENDSELECT.

  DESCRIBE TABLE IT_ZSREQHD LINES W_LINE.
  IF W_LINE = 0.
     MESSAGE E406.
  ENDIF.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'BLLST'.                 ">B/L 조회.
  CALL SCREEN 0114 STARTING AT  07  3
                   ENDING   AT  110 20.

ENDFORM.                    " P2000_BLDATA_SELECT
*&----------------------------------------------------------------
*&      Module  D0114_STATUS_SCR0114  OUTPUT
*&----------------------------------------------------------------
MODULE D0114_STATUS_SCR0114 OUTPUT.

  IF W_STATUS_CHK EQ 'U' .
     MOVE 'UMAT' TO IT_EXCL-FCODE.
     APPEND IT_EXCL.
  ENDIF.
  IF INCLUDE = 'POLST' OR INCLUDE =  'BLLST'.
     SET PF-STATUS 'BLLST' EXCLUDING IT_EXCL.
  ENDIF.
  CASE INCLUDE.
     WHEN 'BLLST'.
       SET TITLEBAR 'POPU' WITH 'Not Carred in Shipment List'.
     WHEN OTHERS.
   ENDCASE.
   SUPPRESS DIALOG.

ENDMODULE.                 " D0114_STATUS_SCR0114  OUTPUT
*&----------------------------------------------------------------
*&      Module  D0114_LIST_CHECK_SCR0114  INPUT
*&----------------------------------------------------------------
MODULE D0114_LIST_CHECK_SCR0114 INPUT.

  LEAVE TO LIST-PROCESSING.
* list Write
  PERFORM P2000_DATA_BL_LISTING.

ENDMODULE.                 " D0114_LIST_CHECK_SCR0114  INPUT
*&----------------------------------------------------------------
*&      Form  P2000_DATA_B/L_LISTING
*&----------------------------------------------------------------
FORM P2000_DATA_BL_LISTING.

  CASE INCLUDE.

    WHEN 'BLLST'.            "> 반입신고 (HOUSE B/L 입력시)
       PERFORM P3000_ZTBL_TITLELIST.
   LOOP AT IT_ZSREQHD.
        W_MOD = SY-TABIX MOD 2.
        PERFORM P2000_ZTBL_DUP_LIST_1.
   ENDLOOP.
   WRITE : / SY-ULINE(101).
*        CLEAR : IT_ZSREQHD.
   WHEN OTHERS.

   ENDCASE.

ENDFORM.                    " P2000_DATA_B/L_LISTING
*&----------------------------------------------------------------
*&      Form  P2000_ZTBL_DUP_LIST_1
*&----------------------------------------------------------------
FORM P2000_ZTBL_DUP_LIST_1.

  DATA: W_DOM_TEX1  LIKE DD07T-DDTEXT.
  FORMAT RESET.
  IF W_MOD EQ 0.
     FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  CLEAR LFA1.
  SELECT SINGLE *
        FROM LFA1
       WHERE LIFNR = IT_ZSREQHD-LLIEF.

  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDBLST'
                                   IT_ZSREQHD-ZFBLST
                               CHANGING   W_DOM_TEX1.
  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
          SY-VLINE, (10) IT_ZSREQHD-EBELN,
          SY-VLINE, (04) IT_ZSREQHD-ZFSHNO,
          SY-VLINE, (24) IT_ZSREQHD-ZFHBLNO,
          SY-VLINE, (15) LFA1-NAME1,
          SY-VLINE, (10) IT_ZSREQHD-ZFETA,
          SY-VLINE, (15) W_DOM_TEX1,
          SY-VLINE.
  HIDE: IT_ZSREQHD.

ENDFORM.                    " P2000_ZTBL_DUP_LIST_1
*&----------------------------------------------------------------
*&      Form  P3000_ZTBL_TITLELIST
*&----------------------------------------------------------------
FORM P3000_ZTBL_TITLELIST.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-ULINE(101).
  WRITE : / SY-VLINE, ' ',
            SY-VLINE, (10) 'P/O No.',
            SY-VLINE, (04) 'Seq.',
            SY-VLINE, (24) 'House B/L No',
            SY-VLINE, (15) 'VENDOR',
            SY-VLINE, (10) 'ETA',
            SY-VLINE, (15) 'B/L Status',
            SY-VLINE.
  WRITE : / SY-ULINE(101).

ENDFORM.                    " P3000_ZTBL_TITLELIST
*&----------------------------------------------------------------
*&      Form  P2000_MULTI_SELECTION1
*&----------------------------------------------------------------
FORM P2000_MULTI_SELECTION1.

  REFRESH IT_SELECTED1.
  CLEAR W_SELECTED_LINES.
  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
        MOVE : IT_ZSREQHD-ZFBLNO   TO IT_SELECTED1-ZFBLNO.
        APPEND IT_SELECTED1.
        ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.
  IF W_SELECTED_LINES = 0.
     MESSAGE E951.
  ENDIF.

ENDFORM.                    " P2000_MULTI_SELECTION1
*&--------------------------------------------------------------------
*&      Form  P2000_DELETE_ZFBLINR_INOU
*&--------------------------------------------------------------------
FORM P2000_DELETE_ZFBLINR_INOU USING    P_ZFTBLNO.

 DATA: W_SY_UCOMM LIKE SY-UCOMM.
 W_SY_UCOMM = SY-UCOMM.

  CLEAR W_BUTTON_ANSWER.
  PERFORM P2000_POPUP_MESSAGE USING
       'Confirmation'
       ' Do you delete the selected document?'
*       '선택한 문서를  삭제하시겠습니까?'
       '  Yes   '
       '   No   '
       '1'
       W_BUTTON_ANSWER.
  IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?

     CLEAR ZTBLINR_TMP.
     SELECT SINGLE *
          FROM ZTBLINR_TMP
         WHERE ZFTBLNO = P_ZFTBLNO.
      *ZTBLINR_TMP = ZTBLINR_TMP.
      PERFORM P2000_CALL_FUNCTION USING W_SY_UCOMM.
  ENDIF.

ENDFORM.                    " P2000_DELETE_ZFBLINR_INOU
*&--------------------------------------------------------------------
*&      Form  P2000_CALL_FUNCTION
*&--------------------------------------------------------------------
FORM P2000_CALL_FUNCTION USING    P_SY_UCOMM.

  CALL FUNCTION 'ZIM_BLINRTMP_DOC_MODIFY'
           EXPORTING
              W_OK_CODE         = P_SY_UCOMM
              ZFTBLNO           = ZTBLINR_TMP-ZFTBLNO
              ZFSTATUS          = 'I'
              W_ZTBLINR_TMP     = ZTBLINR_TMP
              W_ZTBLINR_TMP_OLD = *ZTBLINR_TMP
           EXCEPTIONS
              ERROR_UPDATE      = 4
              NOT_MODIFY        = 8.
  IF SY-SUBRC EQ 0.
     COMMIT WORK.
     IF P_SY_UCOMM = 'EXCU'.
       MESSAGE S977 WITH 'Carry-in Declaration Data Saved normaly'.
     ELSE.
       MESSAGE S977 WITH 'Carry-out Declaration Data Deleted normaly'.
     ENDIF.
  ELSE.
   ROLLBACK WORK.
   IF P_SY_UCOMM = 'EXCU'.
     MESSAGE S977 WITH
     'An error occured during Carry-in declaration data saving.'.
   ELSE.
     MESSAGE S977 WITH
     'An error occured during Carry-out declaration data deleting.'.
   ENDIF.
  ENDIF.

ENDFORM.                    " P2000_CALL_FUNCTION
