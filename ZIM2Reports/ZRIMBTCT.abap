*&---------------------------------------------------------------------*
*& Report  ZRIMBTCT
*&---------------------------------------------------------------------*
*&  프로그램명 : 보세운송 CYCLE-TIME 관리.
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2000.11.15                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMBTCT    MESSAGE-ID ZIM
                     LINE-SIZE 124
                     NO STANDARD PAGE HEADING.
TABLES: ZTBLINR, ZTBL,LFA1,ZTBLINOU.
*-----------------------------------------------------------------------
* 수입의뢰 리스트용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFBTSEQ  LIKE  ZTBLINR-ZFBTSEQ,
       ZFBLNO   LIKE  ZTBL-ZFBLNO,     " B/L 관리번?
       ZFhBLNO  LIKE ZTBL-ZFhBLNO,     " HOUSE B/L 관리번?
       ZFBNDT   LIKE  ZTBL-ZFBNDT , " 보세운송?
       ZFBNTM   LIKE  ZTBL-ZFBNTM , " 보세운송시?
       ZFAVDT   LIKE  ZTBL-ZFAVDT , " 보세운송도착?
       ZFAVTM   LIKE  ZTBL-ZFAVTM , " 보세운송도착시?
       ZFVIA    LIKE  ZTBL-ZFVIA  , "VIA
       ZFFORD   LIKE  ZTBL-ZFFORD,  "FWDR
       NAME1    LIKE      LFA1-NAME1,  "FWDR ?
       ZFCAGTY  LIKE     ZTBL-ZFCAGTY, "화물종?
       ZFBLADT  LIKE     ZTBL-ZFBLADT, "B/L 입수?
       ZFINDT   LIKE     ZTBLINR-ZFINDT, " 반입?
       ZFWERKS  LIKE     ZTBL-ZFWERKS, "PLANT
       NAME2    LIKE     LFA1-NAME1,  "PLANT ?
       ZFSHTY   LIKE     ZTBL-ZFSHTY,   " 선적유?
       ZFREBELN LIKE     ZTBL-ZFREBELN, " P/O NO
       ZFETA    LIKE     ZTBL-ZFETA,    " 입항?
       ZFCARNM  LIKE     ZTBL-ZFCARNM,  "선기?
       ZFTRCK   LIKE     ZTBL-ZFTRCK,  "TRUCKER
       NAME3    LIKE     LFA1-NAME1, "TRUCKER 이?
       C_TIME   TYPE         I. " CYCLE-TIME
 DATA :   END OF IT_TAB.

DATA : BEGIN OF IT_TAB2 OCCURS 0,
               ZFBLNO      LIKE ZTBLINOU-ZFBLNO,
               ZFBTRNO     LIKE ZTBLINOU-ZFBTRNO,
               ZFTDDT      LIKE ZTBLINOU-ZFTDDT.
DATA : END   OF IT_TAB2.

DATA : BEGIN OF IT_TAB3 OCCURS 0,
               ZFBLNO      LIKE ZTBLINR-ZFBLNO,
               ZFINDT      LIKE ZTBLINR-ZFINDT,
               ZFGIRNM     LIKE ZTBLINR-ZFGIRNM.
DATA : END   OF IT_TAB3.

DATA : BEGIN OF IT_LIFNR OCCURS 0,
               LIFNR       LIKE  LFA1-LIFNR,
               NAME1       LIKE  LFA1-NAME1,"FORWARD NAME
               NAME3       LIKE  LFA1-NAME1."TRUCKER NAME
DATA : END   OF IT_LIFNR.

DATA : OLD_ZFVIA    LIKE    ZTBL-ZFVIA,
       OLD_ZFSHTY   LIKE  ZTBL-ZFSHTY,
       C_TOTAL  TYPE I VALUE 0 , "  개별 소요시?
       T_TOTAL  TYPE I VALUE 0 , "  총 소요시?
       C_AVG  TYPE I , "  개별 평균 소요시?
       T_AVG  TYPE I , "  총  평균 소요시?
        W_ERR_CHK TYPE C,
        W_FIELD_NM TYPE C,
        W_PAGE TYPE I,
        W_LINE TYPE I,
        W_COUNT TYPE I,
        W_SUBRC LIKE SY-SUBRC,
        W_TABIX LIKE SY-TABIX,
        W_LIST_INDEX LIKE SY-TABIX.

INCLUDE   ZRIMSORTCOM.   "보세운송 CYCLE-TIME 관리 REPORT Sort
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_WERKS   FOR ZTBL-ZFWERKS,     " 대표 PLANT
                   S_VIA     FOR ZTBL-ZFVIA,      " VIA
                   S_FORD    FOR ZTBL-ZFFORD,      " Forwarder
                   S_TRCK    FOR ZTBL-ZFTRCK,      " TRUCKER
                   S_CAGTY   FOR ZTBL-ZFCAGTY.    " Cargo Type
 SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.                          " 초기값 SETTING
   SET  TITLEBAR 'ZIM24'.          " TITLE BAR


*title Text Write
TOP-OF-PAGE.
 PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* B/L 테이블 SELECT
   PERFORM   P1000_READ_DATA.
   IF SY-SUBRC = 4.
      MESSAGE S738.  EXIT.
   ENDIF.

* 레포트 Write
    PERFORM   P3000_DATA_WRITE.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
         W_FIELD_NM = 'ZFVIA'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
    WHEN 'BLDISP'.
           PERFORM P2000_SHOW_BL USING IT_TAB-ZFHBLNO." B/L 조?
     WHEN 'PODISP'.
           PERFORM P2000_SHOW_PO USING IT_TAB-ZFREBELN." P/O 조?
    WHEN 'TKDISP'.
           PERFORM P2000_SHOW_TK USING IT_TAB-ZFHBLNO." 반입정보조?
    WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
  ENDCASE.

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
  WRITE : /53  ' [ 보세운송   Cycle-Time 관리   ] '
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM,101 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  WRITE : / SY-VLINE,(20) 'HOUSE B/L No',
            SY-VLINE,(10) '보세도착일',
            SY-VLINE,(15) '보세운송일',
            SY-VLINE,(12) 'VIA',
            SY-VLINE,(20) 'FWDR',
            SY-VLINE,(15)'화물종류',
            SY-VLINE,(10) 'B/L입수일',
            SY-VLINE.

 FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE,(20) ' PLANT',
            SY-VLINE,(10) '반입일 ',
            SY-VLINE,(15) 'P/O NO',
            SY-VLINE,(12) ' ETA' ,
            SY-VLINE,(20) 'FLT/V.NAME',
            SY-VLINE,(15) 'TRUCKER',
            SY-VLINE,(10) 'C/TIME', SY-VLINE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK USING    W_ERR_CHK.

   W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*-----------------------------------------------------------------------
*   AUTHORITY-CHECK OBJECT 'ZI_BL_MGT'
*           ID 'ACTVT' FIELD '*'.
*
*   IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME 'B/L Doc transaction'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_DATA.
**  B/L
  SELECT  R~ZFhBLNO R~ZFBNDT R~ZFVIA R~ZFFORD R~ZFTRCK R~ZFAVDT
          R~ZFCAGTY R~ZFWERKS R~ZFREBELN R~ZFETA R~ZFCARNM R~ZFBLADT
          R~ZFTRCK R~ZFCAGTY R~ZFSHTY INTO CORRESPONDING FIELDS OF
          TABLE IT_TAB
        FROM ZTBL  AS R INNER JOIN ZTBLINR AS I
               ON  R~ZFBLNO = I~ZFBLNO
             WHERE R~ZFWERKS IN S_WERKS AND
                   R~ZFVIA   IN S_VIA   AND
                   R~ZFFORD  IN S_FORD  AND
                   R~ZFTRCK IN S_TRCK  AND
                   I~ZFBTSEQ EQ '00001' AND
                   R~ZFCAGTY IN S_CAGTY .

   CLEAR : W_SUBRC.
  IF SY-SUBRC <> 0.  W_SUBRC = 4.   EXIT.    ENDIF.


** GET Trucker FORWARD Name
  SELECT LIFNR NAME1 INTO CORRESPONDING FIELDS OF TABLE IT_LIFNR
     FROM LFA1 FOR ALL ENTRIES IN IT_TAB
         WHERE LIFNR = IT_TAB-ZFFORD.

SELECT LIFNR NAME1 appending CORRESPONDING FIELDS OF TABLE IT_LIFNR
     FROM LFA1 FOR ALL ENTRIES IN IT_TAB
         WHERE LIFNR = IT_TAB-ZFTRCK.

  LOOP AT IT_TAB.
     W_TABIX = SY-TABIX.
        READ TABLE IT_LIFNR WITH KEY LIFNR = IT_TAB-ZFFORD.
        IF SY-SUBRC = 0.
           IT_TAB-NAME1 = IT_LIFNR-NAME1.
        ELSE.
           CLEAR IT_TAB-NAME1.
        ENDIF.

        READ TABLE IT_LIFNR WITH KEY LIFNR = IT_TAB-ZFTRCK.
        IF SY-SUBRC = 0.
           IT_TAB-NAME3 = IT_LIFNR-NAME1.
        ELSE.
           CLEAR IT_TAB-NAME3.
        ENDIF.
        IF NOT ( IT_TAB-ZFAVDT IS INITIAL
              OR IT_TAB-ZFBNDT IS INITIAL ).
           IT_TAB-C_TIME = IT_TAB-ZFAVDT - IT_TAB-ZFBNDT.
        ENDIF.
         MODIFY IT_TAB INDEX W_TABIX.
   ENDLOOP.


ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P3000_DATA_WRITE .

DATA : L_COUNT    TYPE    I,
       C_TOTAL  TYPE I VALUE 0 . "  총 소요시?
 SET PF-STATUS  'PF24'.
 SET  TITLEBAR 'ZIM24'.          " TITLE BAR
DESCRIBE  TABLE IT_TAB   LINES L_COUNT.
W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

SORT IT_TAB BY ZFVIA.
  LOOP AT IT_TAB.
   W_LINE = W_LINE + 1.
   PERFORM P2000_PAGE_CHECK.
*>>> FIELD CHANGE?
     ON CHANGE OF IT_TAB-ZFVIA OR IT_TAB-ZFSHTY.
        IF SY-TABIX NE 1 AND SY-TABIX NE L_COUNT.
           PERFORM SUB_TOTAL.
        ENDIF.
     ENDON.
*>>> LINE WRITE
     PERFORM   P3000_LINE_WRITE.

*>> LAST RECORD
     at last.
      PERFORM SUB_TOTAL.
*>> TOTAL WRITE
IF L_COUNT EQ  0 .
T_AVG = 0.
ENDIF.
    T_AVG = T_TOTAL / L_COUNT.

     FORMAT  COLOR COL_TOTAL INTENSIFIED ON.


     WRITE:/ SY-VLINE,(20)'TOTAL:',SY-VLINE, L_COUNT ,(16)'건   ',
             SY-VLINE,(25) T_TOTAL,'HOUR      ',
             SY-VLINE,(10)'AVG',(11)T_AVG ,'HOUR' ,SY-VLINE.
     FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
          WRITE:/ SY-ULINE.
     endat.


*>> OLD FIELD VALUE MOVE
   MOVE : IT_TAB-ZFSHTY     TO  OLD_ZFSHTY,
          IT_TAB-ZFVIA      TO  OLD_ZFVIA.

ENDLOOP.

ENDFORM.                    " P3000_DATA_WRITE



*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SELECTED_ZFHBLNO  text
*----------------------------------------------------------------------*
FORM P2000_SHOW_BL USING P_ZFHBLNO.
MOVE IT_TAB-ZFHBLNO TO P_ZFHBLNO.
SET PARAMETER ID 'ZPHBLNO'   FIELD P_ZFHBLNO.
   EXPORT 'ZPHBLNO'       TO MEMORY ID 'ZPHBLNO'.
   CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
ENDFORM.                    " P2000_SHOW_BL


FORM RESET_LIST.
MOVE 0 TO SY-LSIND.

  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SUB_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUB_TOTAL.
FORMAT COLOR 3  .
      WRITE : /  SY-ULINE.
   IF OLD_ZFVIA EQ 'AIR'.    " AIR
      WRITE :/ SY-VLINE,(20)' AIR  SUMMERY:       '.


   ELSEIF OLD_ZFVIA EQ 'VSL'.   " OCEAN
       CASE  OLD_ZFSHTY.
         WHEN 'L'.
          WRITE :/ SY-VLINE, (20)'VSL(LCL) SUMMERY:  ' .
         WHEN 'F'.
           WRITE :/ SY-VLINE,(20)'VSL(FCL)  SUMMERY:   '.
         WHEN 'B'.
          WRITE :/ SY-VLINE, (20)'VSL(BULK) SUMMERY:   '.
         WHEN '  ' .
           WRITE :/ SY-VLINE, (20)'VSL(기타) SUMMERY:   '.
      ENDCASE.
    ELSE.
           WRITE :/ SY-VLINE, (20)' NOT AIR AND VSL:    '.

   ENDIF.


*>> SUMMARY TOTAL WRITE

IF w_COUNT EQ  0 .
C_AVG = 0.
ENDIF.
C_AVG = C_TOTAL / W_COUNT.

 WRITE :  SY-VLINE,(5)W_COUNT,'건  ', SY-VLINE,(10)C_TOTAL ,'HOUR',
                 SY-VLINE,(7)C_AVG, 'HOUR' ,SY-VLINE,(51)' ',SY-VLINE.
                 WRITE : /  SY-ULINE.
*>>> SUMMARY TOTAL CLEAR
   CLEAR : W_COUNT , C_TOTAL, C_AVG.
FORMAT COLOR OFF.
ENDFORM.                   " SUB_TOTAL

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

      WRITE:/  SY-VLINE,(20) IT_TAB-ZFHBLNO,
               SY-VLINE,(10) IT_TAB-ZFAVDT,
               SY-VLINE,(15) IT_TAB-ZFBNDT ,
               SY-VLINE,(12) IT_TAB-ZFVIA,
               SY-VLINE,(20) IT_TAB-NAME1 ,
               SY-VLINE,(15) IT_TAB-ZFCAGTY,
               SY-VLINE,(10) IT_TAB-ZFBLADT,SY-VLINE.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
* Hide
   MOVE SY-TABIX  TO W_LIST_INDEX.
   HIDE: W_LIST_INDEX, IT_TAB.
   HIDE:  IT_TAB-ZFBLNO,IT_TAB-ZFREBELN.

    WRITE:/    SY-VLINE,(20) IT_TAB-ZFWERKS,
               SY-VLINE,(10) IT_TAB-ZFINDT,
               SY-VLINE,(15) IT_TAB-ZFREBELN,
               SY-VLINE,(12) IT_TAB-ZFETA,
               SY-VLINE,(20) IT_TAB-ZFCARNM,
               SY-VLINE,(15) IT_TAB-NAME3,
               SY-VLINE,(10) IT_TAB-C_TIME ,SY-VLINE.

   FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

* Hide
   MOVE SY-TABIX  TO W_LIST_INDEX.
   HIDE: W_LIST_INDEX, IT_TAB.
   HIDE:  IT_TAB-ZFBLNO,IT_TAB-ZFREBELN.

*>>> TOTAL / SUMMARY를 누적한다.
 W_COUNT = W_COUNT + 1.
 ADD  IT_TAB-C_TIME  TO   C_TOTAL.
 ADD   IT_TAB-C_TIME     TO     T_TOTAL.
ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
*       text PAGE CHECK를 하기 위?
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

IF W_LINE >= 53.
      WRITE : / SY-ULINE.
      W_PAGE = W_PAGE + 1.    W_LINE = 0.
      NEW-PAGE.
   ENDIF.
ENDFORM.                    " P2000_PAGE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_ZFREBELN  text
*----------------------------------------------------------------------*
FORM P2000_SHOW_PO USING    P_ZFREBELN.
MOVE IT_TAB-ZFREBELN  TO P_ZFREBELN.

SET PARAMETER ID 'BES'   FIELD P_ZFREBELN.
   EXPORT 'BES'       TO MEMORY ID 'BES'.
   CALL TRANSACTION 'ME23' AND SKIP  FIRST SCREEN.
ENDFORM.                    " P2000_SHOW_PO

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_TK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_ZFHBLNO  text
*----------------------------------------------------------------------*
FORM P2000_SHOW_TK USING    P_ZFHBLNO.

MOVE IT_TAB-ZFHBLNO  TO P_ZFHBLNO.

SET PARAMETER ID 'ZPHBLNO'   FIELD P_ZFHBLNO.
   EXPORT 'ZPHBLNO'      TO MEMORY ID 'ZPHBLNO'.
   CALL TRANSACTION 'ZIMI8' AND SKIP  FIRST SCREEN.
ENDFORM.                    " P2000_SHOW_TK
