*&---------------------------------------------------------------------
*& Report  ZRIMSUBBIZFEE
*&---------------------------------------------------------------------
*&  프로그램명 : 업무대행수수료.
*"
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.10.07
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&   1. 2001/10/11  KSB INSERT
*&     - 일괄전기기능 추?
*&     - 사업장별로 그룹핑 로직추?
*&     - 소트 기능 막?
*&
*&---------------------------------------------------------------------
REPORT  ZRIMSUBBIZFEE  MESSAGE-ID ZIM
                       LINE-SIZE 125
                       NO STANDARD PAGE HEADING.

TABLES: UF05A,
        BSIS,
        ZTBLINR,
        T134G,
        T001,
        T005,
        ZTREQHD,
        ZTCGHD,
        ZTMSHD,
        ZTTAXBKHD,
        ZTIMIMG11,
        ZTIMIMG00,
        ZTIMIMG08,
        J_1BBRANCH,
        ZTIV,
        T001W,
        ZTIDR,
        BAPICURR,
        KONP,
        T007A,
        VF_KRED,
        t074u,
        TBSL,
        TBSLT,
       *LFA1,
        ZTBKPF,
       *ZTBKPF,
        ZTBSEG,
        ZTBL,
        ZTIDS,
        ZTCUCLIV,
        LFA1,
        BKPF.


DATA : IT_ZSBSEG      LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.

*>>> ERROR 처리용.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON         LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C.
DATA : END OF IT_ERR_LIST.


DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.



*----------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*----------------------------------------------------------------------
DATA : BEGIN OF IT_BL OCCURS 0,
       ZFREBELN   LIKE     ZTBL-ZFREBELN,      " P/O NO
       ZFSHNO     LIKE    ZTBL-ZFSHNO,        " 선적차?
       ZFBLNO     LIKE     ZTBL-ZFBLNO,     " B/L 관리번?
       ZFHBLNO    LIKE     ZTBL-ZFHBLNO,    " HOUSE B/L 관리번?
       ZFBLSDP    LIKE     ZTBL-ZFBLSDP,    " 송부처.
       ZFBNDT     LIKE     ZTBL-ZFBNDT,     " 보세운송일.
       ZFPOYN     LIKE     ZTBL-ZFPOYN,
       ZFPOTY     LIKE     ZTBL-ZFPOTY,
       ZFWERKS    LIKE     ZTBL-ZFWERKS.
DATA : END OF IT_BL.

DATA : BEGIN OF IT_IDS OCCURS 0,
       ZFREBELN   LIKE     ZTBL-ZFREBELN,      " P/O NO
       ZFBLNO     LIKE     ZTBL-ZFBLNO,     " B/L 관리?
       ZFCLSEQ    LIKE     ZTIDS-ZFCLSEQ,
       ZFHBLNO    LIKE     ZTBL-ZFHBLNO,    " HOUSE B/L 관리번?
       ZFIDSDT    LIKE     ZTIDS-ZFIDSDT,    " 신고일.
       ZFBLSDP    LIKE     ZTBL-ZFBLSDP,    " 송부처.
       ZFRPTTY    LIKE     ZTBL-ZFRPTTY.    " 수입신고형태.
DATA : END OF IT_IDS.

DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFBLNO     LIKE    ZTBL-ZFBLNO,
       ZFCLSEQ    LIKE    ZTIDS-ZFCLSEQ,
       ZFIVNO     LIKE    ZTIV-ZFIVNO,
       BUKRS      LIKE    ZTBKPF-BUKRS,
       BELNR      LIKE    ZTBKPF-BELNR,
       GJAHR      LIKE    ZTBKPF-GJAHR,
       ZFACDO     LIKE    ZTBKPF-ZFACDO,      " 회계전표번호.
       ZFREBELN   LIKE    ZTBL-ZFREBELN,      " P/O NO
       ZFSHNO     LIKE    ZTBL-ZFSHNO,        " 선적차?
       ZFHBLNO    LIKE    ZTBL-ZFHBLNO,       " HOUSE B/L 관리번?
       ZFIDSDT    LIKE    ZTIDS-ZFIDSDT,      " 신고일.
       ZFBNDT     LIKE    ZTBL-ZFBNDT,        " 보세운송일.
       ZFPOSYN    LIKE    ZTBKPF-ZFPOSYN,     " 전표처리여부.
       ZFPOYN     LIKE    ZTBL-ZFPOYN,
       ZFPOTY     LIKE    ZTBL-ZFPOTY,
       ZFBTSEQ    LIKE    ZTBLINR-ZFBTSEQ,
       ZFINDT     LIKE    ZTBLINR-ZFINDT,
       ZFRPTTY    LIKE    ZTBL-ZFRPTTY,
       POSYN(12),
       POYN(4),
       WERKS      LIKE    ZTBL-ZFWERKS,
       BUPLA      LIKE    ZTBKPF-BUPLA,
       ZFCSTGRP   LIKE    ZTBKPF-ZFCSTGRP,
       KRWAMT     TYPE    P DECIMALS 0.     " 거래금액.
DATA : END OF IT_TAB.

DATA : BEGIN OF IT_SELECTED OCCURS 0,
       BUKRS      LIKE    ZTBKPF-BUKRS,
       BELNR      LIKE    ZTBKPF-BELNR,
       GJAHR      LIKE    ZTBKPF-GJAHR,
       ZFBLNO     LIKE    ZTBL-ZFBLNO,
       ZFCLSEQ    LIKE    ZTIDS-ZFCLSEQ,
       ZFIVNO     LIKE    ZTIV-ZFIVNO,
       KRWAMT     LIKE    ZSREQHD-ZFKRWAMT,
       ZFBTSEQ    LIKE    ZTBLINR-ZFBTSEQ,
       ZFPOYN     LIKE    ZTBL-ZFPOYN,
       ZFPOTY     LIKE    ZTBL-ZFPOTY,
       ZFBNDT     LIKE    ZTBL-ZFBNDT,
       BUPLA      LIKE    ZTBKPF-BUPLA,
       WERKS      LIKE    ZTBL-ZFWERKS,
       ZFCSTGRP   LIKE    ZTBKPF-ZFCSTGRP.
DATA : END OF IT_SELECTED.

DATA : BEGIN OF IT_BUPLA OCCURS 0,
       BUPLA       LIKE    ZTBKPF-BUPLA.
DATA : END   OF IT_BUPLA.

DATA : BEGIN OF IT_ZFCSTGRP OCCURS 0,
       ZFCSTGRP    LIKE    ZTBKPF-ZFCSTGRP.
DATA : END   OF IT_ZFCSTGRP.



DATA :  W_ERR_CHK     TYPE C,
        W_SUBRC       LIKE SY-SUBRC,
        W_SELECTED_LINES  TYPE P,                 " 선택 LINE COUNT
        W_PAGE        TYPE I,
        EGRKZ               LIKE     T007A-EGRKZ,
        W_TITLE(32),
        W_TITLE1(50),
        W_DOM_TEX1     LIKE DD07T-DDTEXT,
        W_FNAME        LIKE ZTIMIMG08-ZFCDNM,
        W_CHK_TITLE,
        W_LINE        TYPE I,
        W_GUBUN(50),
        W_KRWAMT(18),
        W_COUNT       TYPE I,
        W_TABIX       LIKE SY-TABIX,
        W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드명.
        W_LIST_INDEX  LIKE SY-TABIX,
        W_MOD         TYPE I,
        LINE(3)           TYPE N,             " 페이지당 LINE COUNT
        W_ZFBTSEQ     LIKE ZTBLINR-ZFBTSEQ.

TABLES:  SPOP,ZSREQHD.

DATA : OPTION(1)       TYPE C,             "  popup Screen에서 사?
       ANTWORT(1)      TYPE C,             "  popup Screen에서 사?
       CANCEL_OPTION   TYPE C,             "  popup Screen에서 사?
       TEXTLEN         TYPE I.             "  popup Screen에서 사?

DATA: W_KBETR             LIKE     KONP-KBETR,
      W_KBETR1            LIKE     KONP-KBETR,
      W_COST_TYPE         LIKE     DD07T-DDTEXT,
      W_CODE_TYPE(30),
      W_CHG_CHK,
      INCLUDE(8)          TYPE C,
      W_KONWA             LIKE     KONP-KONWA,
      W_WMWST             LIKE     ZTBKPF-WMWST,
      W_WMWST1            LIKE     ZTBKPF-WMWST,
      INVOICEDOCNUMBER    LIKE    BAPI_INCINV_FLD-INV_DOC_NO,
      FISCALYEAR          LIKE    BAPI_INCINV_FLD-FISC_YEAR.

INCLUDE   ZRIMSORTCOM.    " REPORT Sort
INCLUDE   ZRIMUTIL01.     " Utility function 모?
INCLUDE   <ICON>.

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   PARAMETERS :    P_BUKRS  LIKE ZTBL-BUKRS OBLIGATORY.
   SELECT-OPTIONS: S_IDSDT  FOR ZTIDS-ZFIDSDT OBLIGATORY,     " 기준일.
                   S_BLSDP  FOR ZTBL-ZFBLSDP OBLIGATORY,      " 송부처.
                   S_WERKS  FOR ZTBL-ZFWERKS. " OBLIGATORY
*                                NO INTERVALS NO-EXTENsION.
   PARAMETERS : P_YES AS CHECKBOX DEFAULT SPACE. "전기데이타포함.
 SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
   PARAMETERS    : P_LIFNR   LIKE ZTBKPF-LIFNR OBLIGATORY,
                   P_BLART   LIKE ZTBKPF-BLART OBLIGATORY
                                  DEFAULT  'I6',
                   P_MWSKZ   LIKE ZTBKPF-MWSKZ OBLIGATORY
                                  DEFAULT  'XX',
                   P_AMT     TYPE P DECIMALS 0 DEFAULT '50000'
                                               OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B2.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BLSDP-LOW.
   PERFORM   P1000_BL_SDP_HELP(ZRIMBWGILST)  USING  S_BLSDP-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BLSDP-HIGH.
   PERFORM   P1000_BL_SDP_HELP(ZRIMBWGILST)  USING  S_BLSDP-HIGH.

INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.
*title Text Write
TOP-OF-PAGE.
  IF INCLUDE NE 'POPU'.
     PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
  ENDIF.

*----------------------------------------------------------------------
* START OF SELECTION ?
*----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* B/L 테이블 SELECT
   PERFORM   P1000_READ_DATA  USING   W_ERR_CHK.
   IF W_ERR_CHK = 'Y'.
      MESSAGE S738.  EXIT.
   ENDIF.
* 레포트 Write
   PERFORM  P3000_DATA_WRITE.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
         W_FIELD_NM = 'ZFWERKS'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                              USING   SY-UCOMM.

* 전체 선택 및 선택해제.
      WHEN 'MKAL' OR 'MKLO'.          " 전체 선택 및 선택해제.
         PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.

      WHEN 'REFR'.
        IF W_TABIX IS INITIAL.
           MESSAGE S962.    EXIT.
        ENDIF.
        PERFORM   P1000_READ_DATA  USING   W_ERR_CHK.
        PERFORM RESET_LIST.
     WHEN 'DISP1'. " B/L.
        PERFORM P2000_MULTI_SELECTION.
        IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            PERFORM P2000_DISP_ZTBL(SAPMZIM09) USING IT_SELECTED-ZFBLNO.
        ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
        ENDIF.

     WHEN 'DISP2'. " 통관요청.
        PERFORM P2000_MULTI_SELECTION.
        IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            IF IT_SELECTED-ZFIVNO IS INITIAL.
               MESSAGE S962.  EXIT.
            ENDIF.
            PERFORM P2000_DISP_ZTIV USING IT_SELECTED-ZFIVNO.
        ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
        ENDIF.

     WHEN 'DISP3'. " 수입면허.
        PERFORM P2000_MULTI_SELECTION.
        IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            IF IT_SELECTED-ZFCLSEQ IS INITIAL.
               MESSAGE S962.   EXIT.
            ENDIF.
            PERFORM P2000_DISP_ZTIDS(SAPMZIM09)
                    USING IT_SELECTED-ZFBLNO
                          IT_SELECTED-ZFCLSEQ.
        ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
        ENDIF.

     WHEN 'DISP4'. " 보세운송.
        PERFORM P2000_MULTI_SELECTION.
        IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            IF IT_SELECTED-ZFBNDT IS INITIAL.
               MESSAGE S962.  EXIT.
            ENDIF.
            PERFORM P2000_DISP_ZTBLINR USING IT_SELECTED-ZFBLNO.
        ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
        ENDIF.
     WHEN 'ZIMY3'.                   " 비용문서조회.
        PERFORM P2000_MULTI_SELECTION.
        IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            IF IT_SELECTED-BELNR IS INITIAL.
               MESSAGE S962. EXIT.
            ENDIF.
            PERFORM P2000_SHOW_COST_DOCUMENT USING  IT_SELECTED-BUKRS
                                                    IT_SELECTED-GJAHR
                                                    IT_SELECTED-BELNR.
        ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
        ENDIF.

     WHEN 'FB03'.   " 일괄처리.
         PERFORM P2000_MULTI_SELECTION.
         IF W_ERR_CHK NE 'Y'.
            ZSREQHD-ZFINSEQ = W_SELECTED_LINES.

            PERFORM P2000_HEADER_DATA_MAKE.
            PERFORM P2000_MESSAGE_POSTING.

            IF ANTWORT EQ 'Y'.
               *ZTBKPF = ZTBKPF.
               PERFORM P2000_FIPOSTING.
               LEAVE TO SCREEN 0.
            ENDIF.
         ENDIF.
     WHEN 'DELE'.
        PERFORM P2000_MULTI_SELECTION.
        IF W_ERR_CHK NE 'Y'.
*           ZSREQHD-ZFINSEQ = W_SELECTED_LINES.

*>> POPUP WINDOWS-->전기취소용.
           MOVE '보험비용 전기취소' TO SPOP-TITEL.
           IF BSIS-BUDAT IS INITIAL.
              MOVE SY-DATUM    TO BSIS-BUDAT.
           ENDIF.

           CALL SCREEN 0010 STARTING AT 15 1
                            ENDING   AT 52 9.

           IF ANTWORT EQ 'Y'.
              *ZTBKPF = ZTBKPF.
              PERFORM P2000_POSTING_CANCLE.
              LEAVE TO SCREEN 0.
           ENDIF.
        ENDIF.

     WHEN 'DOWN'.          " FILE DOWNLOAD....
          PERFORM P3000_TO_PC_DOWNLOAD.
*------- Abbrechen (CNCL) ----------------------------------------------
     WHEN 'CNCL'.
         SET SCREEN 0.    LEAVE SCREEN.
*------- Suchen (SUCH) -------------------------------------------------
     WHEN 'SUCH'.
*------- Sortieren nach Feldbezeichnung (SORB) -------------------------
     WHEN 'SORB'.
*------- Sortieren nach Feldname (SORF) --------------------------------
     WHEN 'SORF'.
*------- Techn. Name ein/aus (TECH) ------------------------------------
     WHEN 'TECH'.
*------- Weiter suchen (WESU) ------------------------------------------
     WHEN 'WESU'.

  ENDCASE.
  CLEAR : IT_ERR_LIST, IT_TAB, W_TABIX.

*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
   CASE INCLUDE.
      WHEN 'POPU'.
         IF NOT IT_ERR_LIST-MSGTYP IS INITIAL.
*            MESSAGE ID IT_ERR_LIST-MSGID TYPE IT_ERR_LIST-MSGTYP
*                    NUMBER IT_ERR_LIST-MSGNR
*                    WITH   IT_ERR_LIST-MSGV1
*                           IT_ERR_LIST-MSGV2
*                           IT_ERR_LIST-MSGV3
*                           IT_ERR_LIST-MSGV4.
            CALL FUNCTION 'MASS_MESSAGE_SHOW_LONGTEXT'
                 EXPORTING
                    SPRSL     = SY-LANGU
                    ARBGB     = IT_ERR_LIST-MSGID
                    MSGNR     = IT_ERR_LIST-MSGNR
                    MSGV1     = IT_ERR_LIST-MSGV1
                    MSGV2     = IT_ERR_LIST-MSGV2
                    MSGV3     = IT_ERR_LIST-MSGV3
                    MSGV4     = IT_ERR_LIST-MSGV4
                 EXCEPTIONS
                    NOT_FOUND = 1
                    OTHERS    = 2.

         ENDIF.
     WHEN OTHERS.
  ENDCASE.
  CLEAR : IT_ERR_LIST, IT_TAB, W_TABIX.


*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

  CONCATENATE  '[' S_IDSDT-LOW(4) '년'
               S_IDSDT-LOW+4(2) '월'
              '업무대행수수료' ']' INTO W_TITLE
              SEPARATED BY SPACE.

  WRITE:/40 W_TITLE COLOR COL_HEADING INTENSIFIED OFF.
  SKIP 1.
  CLEAR LFA1.
  SELECT SINGLE *
         FROM LFA1
        WHERE LIFNR = P_LIFNR.

  SELECT SINGLE * FROM J_1BBRANCH
         WHERE BUKRS   EQ P_BUKRS
         AND   BRANCH  EQ IT_TAB-BUPLA.


  WRITE:/ '대행업체:', P_LIFNR, LFA1-NAME1,
          '사업장:',   IT_TAB-BUPLA, J_1BBRANCH-NAME.


  WRITE : / SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, '',
            SY-VLINE,(12) '회계문서번호' CENTERED,
            SY-VLINE,(12) 'P / O   N o'  CENTERED,
            SY-VLINE,(20) 'B / L   N o'  CENTERED,
            SY-VLINE,(12) '면   허   일' CENTERED,
            SY-VLINE,(12) '보세 운송일'  CENTERED,
            SY-VLINE,(12) '문서상태'     CENTERED,
            SY-VLINE,(12) '금       액'  CENTERED,
            SY-VLINE,(04) '구분',
            SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------
FORM P2000_AUTHORITY_CHECK USING    W_ERR_CHK.

   W_ERR_CHK = 'N'.
*----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*----------------------------------------------------------------------
*   AUTHORITY-CHECK OBJECT 'ZI_BL_MGT'
*           ID 'ACTVT' FIELD '*'.
*
*   IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME 'B/L 관리 트랜잭션'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK

*&---------------------------------------------------------------------
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------
FORM P1000_READ_DATA USING  W_ERR_CHK.

RANGES : R_ZFRPTTY FOR ZTBL-ZFRPTTY OCCURS 5.

  W_ERR_CHK = 'N'.

  MOVE : 'I'    TO    R_ZFRPTTY-SIGN,
         'EQ'   TO    R_ZFRPTTY-OPTION,
         'N'    TO    R_ZFRPTTY-LOW,
         SPACE  TO    R_ZFRPTTY-HIGH.
  APPEND R_ZFRPTTY.

  MOVE : 'I'    TO    R_ZFRPTTY-SIGN,
         'EQ'   TO    R_ZFRPTTY-OPTION,
         'B'    TO    R_ZFRPTTY-LOW,
         SPACE  TO    R_ZFRPTTY-HIGH.
  APPEND R_ZFRPTTY.

*>> 이채경 변경.
* 송부처구분 면허데이타에만 상관 있고, 보세운?
* 데이타에는 상관없이 변경.2001.10.26
*-----------------------------------------------
*>> 보세운송 데이타 SELECT.
*>> B/L---> 보세운송건인 B/L만을 대상으로 SELECT함.
*-----------------------------------------------
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BL
      FROM ZTBL
     WHERE BUKRS   EQ P_BUKRS
       AND ZFBNDT  IN S_IDSDT      " 보세운송일.
       AND ZFRPTTY IN R_ZFRPTTY    " 보세운송건..
       AND ZFWERKS IN S_WERKS.
*  IF SY-SUBRC NE 0.  W_ERR_CHK = 'Y'.   EXIT.    ENDIF.

*-----------------------------------------------
*>> 수입면허건 SELECT.
*-----------------------------------------------
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_IDS
      FROM ZTIDS
      WHERE BUKRS     EQ P_BUKRS
      AND   ZFIDSDT   IN S_IDSDT.

*-----------------------------------------------
*>면허건 조회.
*-----------------------------------------------
  LOOP AT IT_IDS.
     CLEAR IT_TAB.
*>>  통관요청에 대한 수수료.
     SELECT SINGLE * FROM ZTBL
            WHERE    ZFBLNO  EQ  IT_IDS-ZFBLNO
              AND    ZFBLSDP IN S_BLSDP      " 송부처.
              AND    ZFWERKS IN  S_WERKS.
     IF SY-SUBRC NE 0.
        CONTINUE.
     ENDIF.

     CLEAR ZTCUCLIV.
     SELECT SINGLE * FROM ZTCUCLIV
            WHERE ZFBLNO  = IT_IDS-ZFBLNO
            AND   ZFCLSEQ = IT_IDS-ZFCLSEQ.

     IF SY-SUBRC EQ 0.
        MOVE ZTCUCLIV-ZFIVNO TO IT_TAB-ZFIVNO.
     ELSE.
        CONTINUE.
     ENDIF.

     SELECT SINGLE * FROM ZTIV
            WHERE    ZFIVNO  EQ  IT_TAB-ZFIVNO.
     IF SY-SUBRC NE 0.
        CONTINUE.
     ENDIF.

*>> 이미 전기처리된건에 대해서.
     SELECT SINGLE *
            FROM  ZTBSEG
            WHERE ZFIMDNO = IT_TAB-ZFIVNO
            AND   ZFCSTGRP = '006'
            AND   ZFCD     = '999'.

     IF SY-SUBRC EQ 0.
         SELECT SINGLE *
                FROM  ZTBKPF
                WHERE BUKRS   = ZTBSEG-BUKRS
                AND   BELNR   = ZTBSEG-BELNR
                AND   GJAHR   = ZTBSEG-GJAHR.

         MOVE: ZTBKPF-ZFPOSYN  TO IT_TAB-ZFPOSYN,
               ZTBKPF-BUKRS    TO IT_TAB-BUKRS,
               ZTBKPF-BELNR    TO IT_TAB-BELNR,
               ZTBKPF-GJAHR    TO IT_TAB-GJAHR.
     ELSE.
        IT_TAB-ZFPOSYN = 'C'.    " 비용문서 미등록상태.
     ENDIF.

     IF P_YES = ' '.
        IF IT_TAB-ZFPOSYN = 'Y'.
           CONTINUE.
        ENDIF.
     ENDIF.

     CASE IT_TAB-ZFPOSYN.
        WHEN 'Y'.
           IT_TAB-POSYN  = '전기처리상태'.
        WHEN 'N'.
           IT_TAB-POSYN  = '미전기 상태'.
        WHEN 'C'.
           IT_TAB-POSYN  = '미등록 상태'.
        WHEN OTHERS.
     ENDCASE.

*> 사업장.
     SELECT SINGLE * FROM T001W
            WHERE    WERKS   EQ   ZTBL-ZFWERKS.
     IF SY-SUBRC EQ 0.
        MOVE T001W-J_1BBRANCH  TO IT_TAB-BUPLA.
     ELSE.
        CONTINUE.
     ENDIF.

     MOVE-CORRESPONDING IT_IDS TO IT_TAB.
     MOVE P_AMT                TO IT_TAB-KRWAMT.
     MOVE ZTIV-ZFPOYN          TO IT_TAB-ZFPOYN.
     MOVE ZTBL-ZFWERKS         TO IT_TAB-WERKS.
     MOVE ZTBL-ZFREBELN        TO IT_TAB-ZFREBELN.
     MOVE ZTBL-ZFSHNO          TO IT_TAB-ZFSHNO.
     MOVE ZTBL-ZFPOTY          TO IT_TAB-ZFPOTY.
     MOVE '006'                TO IT_TAB-ZFCSTGRP.

     CASE IT_TAB-ZFPOYN.
        WHEN 'Y'.
           IT_TAB-POYN = '유환'.
        WHEN 'N'.
           IT_TAB-POYN = '무환'.
        WHEN 'M'.
           IT_TAB-POYN = '혼합'.
        WHEN OTHERS.
           IT_TAB-POYN = '****'.
     ENDCASE.

     APPEND IT_TAB.
 ENDLOOP.

*-----------------------------------------------
*>> 보세운송 데이타 SELECT.
*-----------------------------------------------
 LOOP AT IT_BL.
     CLEAR IT_TAB.


*>> 이미 전기처리된건에 대해서.
     SELECT SINGLE *
           FROM ZTBSEG
          WHERE ZFIMDNO = IT_BL-ZFBLNO
            AND ZFCSTGRP = '005'
            AND ZFCD     = '999'.

     IF SY-SUBRC EQ 0.
        SELECT SINGLE *
          FROM ZTBKPF
         WHERE BUKRS   = ZTBSEG-BUKRS
           AND BELNR   = ZTBSEG-BELNR
           AND GJAHR   = ZTBSEG-GJAHR.

         MOVE: ZTBKPF-ZFPOSYN  TO IT_TAB-ZFPOSYN,
               ZTBKPF-BUKRS    TO IT_TAB-BUKRS,
               ZTBKPF-BELNR    TO IT_TAB-BELNR,
               ZTBKPF-GJAHR    TO IT_TAB-GJAHR.
     ELSE.
        IT_TAB-ZFPOSYN = 'C'.    " 비용문서 미등록상태.
     ENDIF.

     IF P_YES = ' '.
        IF IT_TAB-ZFPOSYN = 'Y'.
           CONTINUE.
        ENDIF.
     ENDIF.

     CASE IT_TAB-ZFPOSYN.
        WHEN 'Y'.
           IT_TAB-POSYN  = '전기처리상태'.
        WHEN 'N'.
           IT_TAB-POSYN  = '미전기 상태'.
        WHEN 'C'.
           IT_TAB-POSYN  = '미등록 상태'.
        WHEN OTHERS.
     ENDCASE.
*>> 보세운송건에 대한 수수료추가.
     SELECT MAX( ZFBTSEQ ) INTO W_ZFBTSEQ
            FROM ZTBLINR
            WHERE ZFBLNO = IT_BL-ZFBLNO.

     CLEAR ZTBLINR.
     SELECT  SINGLE *
             FROM ZTBLINR
             WHERE ZFBLNO  = IT_BL-ZFBLNO
             AND ZFBTSEQ   = W_ZFBTSEQ.
     IF SY-SUBRC NE 0.
        CONTINUE.
     ENDIF.
*> B/L상에 보세운송 반입일이 있는 경우만....
     IF IT_BL-ZFBNDT IS INITIAL.
        CONTINUE.
     ENDIF.

*> 사업장.
     SELECT SINGLE * FROM T001W
            WHERE    WERKS   EQ   IT_BL-ZFWERKS.
     IF SY-SUBRC EQ 0.
        MOVE T001W-J_1BBRANCH  TO IT_TAB-BUPLA.
     ELSE.
        CONTINUE.
     ENDIF.

     MOVE-CORRESPONDING IT_BL TO IT_TAB.
     MOVE P_AMT                TO IT_TAB-KRWAMT.
     MOVE W_ZFBTSEQ            TO IT_TAB-ZFBTSEQ.
     MOVE ZTBLINR-ZFINDT       TO IT_TAB-ZFINDT.
     MOVE IT_BL-ZFPOYN         TO IT_TAB-ZFPOYN.
     MOVE IT_BL-ZFPOTY         TO IT_TAB-ZFPOTY.
     MOVE IT_BL-ZFWERKS        TO IT_TAB-WERKS.
     MOVE '005'                TO IT_TAB-ZFCSTGRP.

     CASE IT_TAB-ZFPOYN.
        WHEN 'Y'.
           IT_TAB-POYN = '유환'.
        WHEN 'N'.
           IT_TAB-POYN = '무환'.
        WHEN 'M'.
           IT_TAB-POYN = '혼합'.
        WHEN OTHERS.
           IT_TAB-POYN = '****'.
     ENDCASE.

     APPEND IT_TAB.

*     IF SY-SUBRC EQ 0.
*           MOVE-CORRESPONDING IT_BL TO IT_TAB.
*           MOVE P_AMT               TO IT_TAB-KRWAMT.
*           MOVE IT_BL               TO IT_TAB-WERKS.
*           APPEND IT_TAB.
*     ENDIF.

 ENDLOOP.

 DESCRIBE TABLE IT_TAB LINES W_LINE.
 IF W_LINE = 0.   W_ERR_CHK = 'Y'.   EXIT.    ENDIF.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE .

   SET TITLEBAR  'ZIMR83'.
   SET PF-STATUS 'ZIMR83'.

   SORT IT_TAB BY BUPLA ZFBLNO ZFIVNO.

   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      W_MOD = SY-TABIX MOD 2.
      ON CHANGE OF IT_TAB-BUPLA.
         IF W_TABIX NE 1.
            WRITE : / SY-ULINE.
            NEW-PAGE.
         ENDIF.
      ENDON.

      PERFORM   P3000_LINE_WRITE.
      AT LAST.
         PERFORM P3000_LINE_TOTAL.
      ENDAT.

   ENDLOOP.
   CLEAR: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

 SET TITLEBAR  'ZIMR83'.

 DATA: v_aa(02).
   CALL FUNCTION 'END_OF_MONTH_DETERMINE'
        EXPORTING
           datum = sy-datum
        IMPORTING
           tt = v_aa
        EXCEPTIONS
        OTHERS = 1.


*>> 기준일.
  MOVE :    'I'          TO  S_IDSDT-SIGN,
            'BT'         TO  S_IDSDT-OPTION.

  CONCATENATE SY-DATUM(6) '01' INTO S_IDSDT-LOW.
  CONCATENATE SY-DATUM(6) v_aa INTO S_IDSDT-HIGH.
  APPEND S_IDSDT.
*>> 송부처.
  MOVE :    'I'           TO  S_BLSDP-SIGN,
            'BT'          TO  S_BLSDP-OPTION,
            '001'         TO  S_BLSDP-LOW,
            '002'         TO  S_BLSDP-HIGH.
  APPEND S_BLSDP.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------
FORM P3000_LINE_WRITE.
  DATA: W_TEXT1(12),
        W_TEXT2(8) TYPE I.

  IF IT_TAB-ZFSHNO IS INITIAL.
     MOVE IT_TAB-ZFREBELN TO W_TEXT1.
  ELSE.
     CONCATENATE IT_TAB-ZFREBELN '-' IT_TAB-ZFSHNO INTO W_TEXT1.
  ENDIF.

  FORMAT RESET.
  IF W_MOD EQ 0.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
     FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ENDIF.
  WRITE : / SY-VLINE, MARKFIELD  AS CHECKBOX,
            SY-VLINE,(12)IT_TAB-BELNR,              " 회계전표번호.
            SY-VLINE,(12)W_TEXT1,                   " P/O NO
            SY-VLINE,(20)IT_TAB-ZFHBLNO.           " HOUSE B/L 관리번?

  CLEAR W_TEXT2.
  MOVE IT_TAB-ZFIDSDT TO W_TEXT2.
  IF W_TEXT2 IS INITIAL.
      WRITE: SY-VLINE,(12) SPACE.
  ELSE.
      WRITE: SY-VLINE,(12)IT_TAB-ZFIDSDT.             " 신고일.
  ENDIF.

  CLEAR W_TEXT2.
  MOVE IT_TAB-ZFBNDT TO W_TEXT2.
  IF W_TEXT2 IS INITIAL.
      WRITE: SY-VLINE,(12) SPACE.
  ELSE.
      WRITE: SY-VLINE,(12)IT_TAB-ZFBNDT.               " 보세운송일.
  ENDIF.

  WRITE :   SY-VLINE,(12)IT_TAB-POSYN,
            SY-VLINE,(12)IT_TAB-KRWAMT CURRENCY 'KRW', " 거래금액.
            SY-VLINE,(04)IT_TAB-POYN,
            SY-VLINE.
  HIDE: IT_TAB, W_TABIX.
  W_COUNT = W_COUNT + 1.

ENDFORM.

*&---------------------------------------------------------------------
*
*&      Form  P3000_LINE_TOTAL
*&---------------------------------------------------------------------
*
FORM P3000_LINE_TOTAL.

   FORMAT RESET.
   FORMAT COLOR COL_TOTAL INTENSIFIED ON.
   WRITE:/ SY-ULINE.
   SUM.
   WRITE :/ SY-VLINE, '',
            SY-VLINE,(12) 'TOTAL',              " 회계전표번호.
            SY-VLINE,(09)  W_COUNT,(02)'건' ,            " P/O NO
            SY-VLINE,(20) '',             " HOUSE B/L 관리번?
            SY-VLINE,(12) '',             " 신고일.
            SY-VLINE,(12) '',             " 보세운송일.
            SY-VLINE,(12) '금액단위:원',
            SY-VLINE,(12) IT_TAB-KRWAMT CURRENCY 'KRW', " 거래금액.
            SY-VLINE,(04) SPACE,
            SY-VLINE.
  WRITE:/ SY-ULINE.
  FORMAT COLOR OFF.

ENDFORM.                    " P3000_LINE_TOTAL
*&---------------------------------------------------------------------
*&      Form  RESET_LIST
*&---------------------------------------------------------------------
FORM RESET_LIST.

  W_CHK_TITLE = 1.
  MOVE 0 TO SY-LSIND.
  PERFORM   P3000_TITLE_WRITE.     " 해더 출력...
  PERFORM   P3000_DATA_WRITE.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------
*&      Form  P2000_FIPOSTING
*&---------------------------------------------------------------------
FORM P2000_FIPOSTING.

  CLEAR W_COUNT.
  CLEAR ZSREQHD-ZFKRWAMT.

  REFRESH : IT_ERR_LIST.
  REFRESH : IT_ZSBSEG.

  LOOP AT IT_SELECTED.
     REFRESH : IT_ZSBSEG.
     CLEAR : IT_ZSBSEG, ZTBKPF.

*>>> 진행상태바..
     line = ( sy-tabix / w_selected_lines ) * 100.
     out_text = 'JOB PROGRESS %99999%%'.
     replace '%99999%' with line into out_text.
     perform p2000_show_bar using out_text line.

     CLEAR : ZTBKPF.
     MOVE-CORRESPONDING *ZTBKPF TO ZTBKPF.

     IF IT_SELECTED-ZFCSTGRP EQ '005'.   ">보운 대행료.
        SELECT SINGLE * FROM ZTBL
               WHERE    ZFBLNO  EQ IT_SELECTED-ZFBLNO.

        MOVE : IT_SELECTED-ZFBLNO      TO IT_ZSBSEG-ZFIMDNO,
               ZTBL-ZFHBLNO            TO IT_ZSBSEG-ZUONR,
               ZTBL-KOSTL              TO IT_ZSBSEG-KOSTL,
               ZTBL-PS_POSID           TO IT_ZSBSEG-PS_POSID,
               ZTBL-ZFPOYN             TO IT_ZSBSEG-ZFPOYN,
               IT_SELECTED-ZFCSTGRP    TO IT_ZSBSEG-ZFCSTGRP.
     ELSE.                               ">통관 대행료.
        SELECT SINGLE * FROM ZTIDS
               WHERE    ZFBLNO  EQ IT_SELECTED-ZFBLNO
               AND      ZFCLSEQ EQ IT_SELECTED-ZFCLSEQ.

        SELECT SINGLE * FROM ZTBL
               WHERE    ZFBLNO  EQ IT_SELECTED-ZFBLNO.

         MOVE : IT_SELECTED-ZFIVNO     TO IT_ZSBSEG-ZFIMDNO,
               ZTIDS-ZFIDRNO           TO IT_ZSBSEG-ZUONR,
               ZTBL-KOSTL              TO IT_ZSBSEG-KOSTL,
               ZTBL-PS_POSID           TO IT_ZSBSEG-PS_POSID,
               ZTBL-ZFPOYN             TO IT_ZSBSEG-ZFPOYN,
               IT_SELECTED-ZFCSTGRP    TO IT_ZSBSEG-ZFCSTGRP.
     ENDIF.

     MOVE IT_ZSBSEG-ZFCSTGRP TO ZTBKPF-ZFCSTGRP.

*>>>>
     SELECT SINGLE * FROM ZTIMIMG08
            WHERE    ZFCDTY   EQ   IT_SELECTED-ZFCSTGRP
            AND      ZFCD     EQ   '999'.
     IF SY-SUBRC NE 0.
        MESSAGE E601(ZIM1).
        PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                    USING  'E'.
        CONTINUE.
     ENDIF.

     MOVE : ZTIMIMG08-COND_TYPE        TO IT_ZSBSEG-COND_TYPE,
            ZTIMIMG08-ZFCD             TO IT_ZSBSEG-ZFCD,
            P_MWSKZ                    TO IT_ZSBSEG-MWSKZ,
            '40'                       TO IT_ZSBSEG-NEWBS,
            'S'                        TO IT_ZSBSEG-SHKZG,
            IT_SELECTED-KRWAMT         TO IT_ZSBSEG-WRBTR,
            0                          TO IT_ZSBSEG-WMWST,
            IT_SELECTED-KRWAMT         TO IT_ZSBSEG-DMBTR,
            1                          TO IT_ZSBSEG-KURSF,
            SY-DATUM                   TO IT_ZSBSEG-WWERT,
            IT_ZSBSEG-ZFPOYN           TO IT_ZSBSEG-ZFPOYN,
*            IT_ZSBSEG-ZUONR            TO IT_ZSBSEG-ZUONR,
            SPACE                      TO IT_ZSBSEG-ZFDCSTX,
            ZTIMIMG08-ZFCDNM           TO IT_ZSBSEG-SGTXT.

      IF IT_ZSBSEG-WRBTR IS INITIAL.
         IT_ZSBSEG-WRBTR = IT_ZSBSEG-DMBTR.
      ENDIF.

      PERFORM P1000_IMPORT_DOC_CHEKC   USING ZTBKPF-ZFCSTGRP
                                             IT_ZSBSEG-ZFIMDNO
                                             IT_ZSBSEG-ZFDCNM
                                             IT_ZSBSEG-ZFPOYN
                                             'I'
                                             IT_ZSBSEG-KOSTL
                                             ZTBKPF-GSBER
                                             ZTBKPF-BUPLA.

      MOVE : IT_ZSBSEG-ZFDCNM TO IT_ZSBSEG-ZUONR.


      PERFORM  P2000_SET_NEWKO  USING IT_ZSBSEG-ZFCSTGRP
                                      IT_ZSBSEG-NEWKO
                                      IT_ZSBSEG-ZFCD
                                      IT_ZSBSEG-ZFIMDNO.

      APPEND IT_ZSBSEG.

*>사업영역.
*     SELECT MAX( GSBER ) INTO ZTBKPF-GSBER
*                         FROM T134G
*                         WHERE WERKS EQ IT_SELECTED-WERKS.
*
*     MOVE : IT_SELECTED-BUPLA TO ZTBKPF-BUPLA,
      MOVE : P_BUKRS           TO ZTBKPF-BUKRS.

*      ZTBKPF-ZTERM = VF_KRED-ZTERM.
*      ZTBKPF-AKONT = VF_KRED-AKONT.
*      *LFA1-NAME1  = VF_KRED-NAME1.

*> 전기년도 체크.
*     BKPF-BUKRS = ZTBKPF-BUKRS.
     PERFORM NUMMERNKREIS_LESEN(SAPFF001)
             USING ZTBKPF-GJAHR.

     MOVE: SY-MANDT        TO ZTBKPF-MANDT,
           'X'             TO ZTBKPF-ZFPCUR,
*           'Y'             TO ZTBKPF-ZFPOYN,
           P_BUKRS         TO ZTBKPF-BUKRS,
           SY-DATUM(4)     TO ZTBKPF-GJAHR,
*          *ZTBKPF-BUDAT    TO ZTBKPF-BUDAT,
*          *ZTBKPF-BLDAT    TO ZTBKPF-BLDAT,
           SY-DATUM        TO ZTBKPF-ZFBDT,
           P_BLART         TO ZTBKPF-BLART,
           IT_ZSBSEG-ZFCSTGRP TO ZTBKPF-ZFCSTGRP,
           IT_ZSBSEG-ZFPOYN   TO ZTBKPF-ZFPOYN,
           SY-DATUM+4(2)   TO ZTBKPF-MONAT,
           P_MWSKZ         TO ZTBKPF-MWSKZ,
           1               TO ZTBKPF-KURSF,
           SY-DATUM        TO ZTBKPF-WWERT,
*          'A'             TO ZTBKPF-ZLSPR,
*          '업무대행료'    TO ZTBKPF-BKTXT,
           T001-WAERS      TO ZSREQHD-ZFKRW,
           T001-WAERS      TO ZTBKPF-HWAER,
           IT_SELECTED-KRWAMT  TO ZTBKPF-WRBTR,
           IT_SELECTED-KRWAMT  TO ZTBKPF-DMBTR,
           T001-WAERS          TO ZTBKPF-WAERS,
           'N'                 TO ZTBKPF-ZFPOSYN,
           P_LIFNR             TO ZTBKPF-LIFNR,
           P_LIFNR             TO ZTBKPF-ZFVEN,
           'X'                 TO ZTBKPF-ZFAUTO,
           'X'                 TO ZTBKPF-ZFATPT.


     PERFORM P3000_FI_POSTING.
     W_COUNT = W_COUNT + 1.

  ENDLOOP.

  DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
  IF W_LINE GT 0.
     INCLUDE = 'POPU'.
     CALL SCREEN 0100 STARTING AT  05   3
                      ENDING   AT  100 12.
     CLEAR : INCLUDE.
  ENDIF.

ENDFORM.                    " P2000_FIPOSTING
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTIV
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTIV USING    P_ZFIVNO.

   SET PARAMETER ID 'ZPIVNO'  FIELD  P_ZFIVNO.
   SET PARAMETER ID 'ZPHBLNO' FIELD ''.
   SET PARAMETER ID 'ZPBLNO'  FIELD ''.
   CALL TRANSACTION 'ZIM33'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTIV
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTBLINR
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTBLINR USING    P_ZFBLNO.

   SET PARAMETER ID 'BES'     FIELD ''.
   SET PARAMETER ID 'ZPHBLNO' FIELD ''.
   SET PARAMETER ID 'ZPBLNO'  FIELD  P_ZFBLNO.
   SET PARAMETER ID 'ZPBTSEQ'  FIELD ''.
   CALL TRANSACTION 'ZIMI8'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTBLINR
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  REFRESH : IT_SELECTED, IT_BUPLA, IT_ZFCSTGRP.
  CLEAR W_SELECTED_LINES.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
       MOVE: IT_TAB-ZFBLNO  TO  IT_SELECTED-ZFBLNO,
             IT_TAB-ZFCLSEQ TO  IT_SELECTED-ZFCLSEQ,
             IT_TAB-ZFIVNO  TO  IT_SELECTED-ZFIVNO,
             IT_TAB-KRWAMT  TO  IT_SELECTED-KRWAMT,     " 거래금액.
             IT_TAB-ZFPOYN  TO  IT_SELECTED-ZFPOYN,
             IT_TAB-WERKS   TO  IT_SELECTED-WERKS,
             IT_TAB-ZFPOTY  TO  IT_SELECTED-ZFPOTY,
             IT_TAB-ZFPOYN  TO  IT_SELECTED-ZFPOYN,
             IT_TAB-BUPLA   TO  IT_SELECTED-BUPLA,
             IT_TAB-ZFCSTGRP TO IT_SELECTED-ZFCSTGRP,
             IT_TAB-BUKRS    TO IT_SELECTED-BUKRS,
             IT_TAB-BELNR    TO IT_SELECTED-BELNR,
             IT_TAB-ZFBNDT   TO IT_SELECTED-ZFBNDT,
             IT_TAB-GJAHR    TO IT_SELECTED-GJAHR.

       PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
               USING  IT_SELECTED-KRWAMT
                      'KRW'.
       APPEND IT_SELECTED.

       IF NOT IT_TAB-BELNR IS INITIAL AND SY-UCOMM EQ 'FB03'.
          MESSAGE E578.
       ENDIF.

       IF IT_TAB-BELNR IS INITIAL AND SY-UCOMM EQ 'DELE'.
          MESSAGE E403(ZIM1).
       ENDIF.

       ADD IT_SELECTED-KRWAMT TO *ZTBKPF-WRBTR.

       ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  W_ERR_CHK = 'N'.
  IF W_SELECTED_LINES EQ 0.
     MESSAGE S951.
     W_ERR_CHK = 'Y'.
  ENDIF.

*  DESCRIBE TABLE IT_BUPLA LINES W_LINE.
*  IF W_LINE GT 1.
*     MESSAGE S600(ZIM1).
*     W_ERR_CHK = 'Y'.
*  ENDIF.
*
ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0001 OUTPUT.

   SET TITLEBAR 'POPU'.
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
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'ENTR'.   EXIT.
    WHEN 'YES'.    ANTWORT = 'Y'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.     ANTWORT = 'N'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
       EXIT.
  ENDCASE.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_POSTING
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_POSTING.

  PERFORM P2000_MESSAGE_BOX USING '전기 확인'             " 타이틀...
                          '선택된자료를 전기합니다 '
                          '전기하시겠습니까?'               " MSG2
                          'N'                 " 취소 버튼 유/?
                          '1'.                " default button


ENDFORM.                    " P2000_MESSAGE_POSTING
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

  CALL SCREEN 0001 STARTING    AT 10 2
                      ENDING   AT 90 16.

ENDFORM.                    " P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
*&      Module  PERIOD_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PERIOD_CHECK_SCR0001 INPUT.
  DATA: S_MONAT LIKE BKPF-MONAT.      "Save field for input period

*> 증빙일.
  IF ZTBKPF-BLDAT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BLDAT'.
  ENDIF.
  IF ZTBKPF-BUDAT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUDAT'.
  ENDIF.

  IF ZTBKPF-ZFBDT IS INITIAL.
     ZTBKPF-ZFBDT = ZTBKPF-BUDAT.
  ENDIF.

  CLEAR ZTBKPF-GJAHR.
  S_MONAT = ZTBKPF-MONAT.

  PERFORM PERIODE_ERMITTELN(SAPMZIM02) USING ZTBKPF-BUDAT
                                             ZTBKPF-GJAHR
                                             ZTBKPF-MONAT.

  IF NOT S_MONAT IS INITIAL
  AND    S_MONAT NE ZTBKPF-MONAT
  AND ( SY-BINPT = SPACE AND SY-CALLD = SPACE ).
    MESSAGE W000(F5) WITH S_MONAT ZTBKPF-BUDAT ZTBKPF-MONAT.
  ENDIF.

ENDMODULE.                 " PERIOD_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  BELEGART_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BELEGART_CHECK_SCR0001 INPUT.

   IF ZTBKPF-BLART IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BLART'.
   ENDIF.
*> 문서 종류 체크.
   PERFORM BELEGART_PRUEFEN(SAPFF001)
           USING ZTBKPF-BLART ZTBKPF-GJAHR.
*> 전기년도 체크.
   BKPF-BUKRS = ZTBKPF-BUKRS.
   PERFORM NUMMERNKREIS_LESEN(SAPFF001)
           USING ZTBKPF-GJAHR.

ENDMODULE.                 " BELEGART_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.
   ANTWORT = 'C'.
   SET SCREEN 0.   LEAVE SCREEN.
ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  VENDOR_ACCOUNT_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VENDOR_ACCOUNT_CHECK_SCR0001 INPUT.

*   IF ZTBKPF-BUPLA IS INITIAL.    ">
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUPLA'.
*   ENDIF.
*
*   IF ZTBKPF-GSBER IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'GSBER'.
*   ENDIF.

   IF ZTBKPF-LIFNR IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'LIFNR'.
   ENDIF.

   CALL FUNCTION 'FI_POSTING_KEY_DATA'
         exporting
              i_bschl       = '31'
              i_umskz       = SPACE       ">bseg-umskz
         importing
              e_t074u       = t074u
              e_tbsl        = tbsl
              e_tbslt       = tbslt
         exceptions
              error_message = 1.

* 1. PBO: no message if bschl request umskz
    if sy-subrc = 1.
* (del) if not ( firstcall = 'X'                           "Note 352492
      if tbsl-xsonu ne space.
        message id sy-msgid type sy-msgty number sy-msgno with
                sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.

*>> VENDOR MASTER DEFINE.
    CLEAR : *LFA1.
    SELECT SINGLE * INTO *LFA1 FROM LFA1
                    WHERE LIFNR EQ ZTBKPF-LIFNR.
    IF SY-SUBRC NE 0.
       MESSAGE E023 WITH ZTBKPF-LIFNR.
    ENDIF.

    CALL FUNCTION 'FI_VENDOR_DATA'
         EXPORTING
            i_bukrs = ZTBKPF-BUKRS
            i_lifnr = ZTBKPF-LIFNR
         IMPORTING
            e_kred  = vf_kred.

*    IF ZTBKPF-ZTERM IS INITIAL.
*       ZTBKPF-ZTERM = VF_KRED-ZTERM.
*    ELSEIF ZTBKPF-ZTERM NE VF_KRED-ZTERM.
    IF ZTBKPF-ZTERM NE VF_KRED-ZTERM.
       IF ZTBKPF-ZTERM IS INITIAL.
          ZTBKPF-ZTERM = VF_KRED-ZTERM.
       ELSE.
          MESSAGE W574 WITH  ZTBKPF-LIFNR VF_KRED-ZTERM ZTBKPF-ZTERM.
       ENDIF.
    ENDIF.

*    if lfb1-bukrs is initial.
*       move-corresponding vf_kred to lfa1.
*    else.
*       move-corresponding vf_kred to lfa1.
*       move-corresponding vf_kred to lfb1.
*       lfb1-sperr = vf_kred-sperr_b.
*       lfb1-loevm = vf_kred-loevm_b.
*       lfb1-begru = vf_kred-begru_b.
*   endif.

   IF ZTBKPF-MWSKZ IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'MWSKZ'.
   ENDIF.

   IF ZTBKPF-AKONT IS INITIAL.
      ZTBKPF-AKONT = vf_kred-AKONT.
   ENDIF.
   IF ZTBKPF-AKONT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'AKONT'.
   ENDIF.
*>> TAX CODE CHECK.
   call function 'FI_TAX_INDICATOR_CHECK'
        exporting
            i_bukrs  = ZTBKPF-BUKRS
            i_hkont  = vf_kred-AKONT
            i_koart  = 'K'
            i_mwskz  = ZTBKPF-MWSKZ
            i_stbuk  = SPACE
            x_dialog = 'X'
       importing
            e_egrkz  = egrkz.
*> ??????.
*   IF ZTBKPF-WRBTR IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WRBTR'.
*   ENDIF.
*> ??.
*   IF ZTBKPF-WAERS IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WAERS'.
*   ENDIF.

*>> ??? ??.
*   IF ZTBKPF-XMWST EQ 'X'.
*      SELECT SINGLE * FROM T007A
*             WHERE KALSM EQ 'TAXKR'
*             AND   MWSKZ EQ  ZTBKPF-MWSKZ.
*      IF SY-SUBRC NE 0.
*         MESSAGE E495 WITH 'TAXKR' ZTBKPF-MWSKZ.
*      ENDIF.
*
*      SELECT * FROM  KONP
*               WHERE KAPPL EQ 'TX'       ">??.
*               AND   KSCHL EQ 'KRIT'     ">?????.
*               AND   MWSK1 EQ ZTBKPF-MWSKZ.
*
*         MOVE: KONP-KBETR   TO   W_KBETR,         ">??.
*               KONP-KONWA   TO   W_KONWA.         ">??.
*         IF NOT W_KBETR IS INITIAL.
*            W_KBETR = W_KBETR / 10.
*         ENDIF.
*      ENDSELECT.
*
*      IF SY-SUBRC EQ 0.
*         IF NOT W_KBETR IS INITIAL.
*            PERFORM SET_CURR_CONV_TO_EXTERNAL(SAPMZIM01)
*                    USING ZTBKPF-WRBTR ZTBKPF-WAERS ZTBKPF-WMWST.
**>>>> ?? : (100 + %) =  X : % ======>
*            W_WMWST = ZTBKPF-WMWST.
*            BAPICURR-BAPICURR = ZTBKPF-WMWST * W_KBETR * 1000.
*            W_KBETR1 = W_KBETR.
*            W_KBETR = ( W_KBETR + 100 ).
*            BAPICURR-BAPICURR = BAPICURR-BAPICURR / W_KBETR.
*
**          ZTBKPF-WMWST = W_WMWST - ( BAPICURR-BAPICURR * 100 / 1000 ).
**          ZTBKPF-WMWST = ZTBKPF-WMWST / 10.
*            BAPICURR-BAPICURR = BAPICURR-BAPICURR / 1000.
*            ZTBKPF-WMWST = BAPICURR-BAPICURR.
*            COMPUTE ZTBKPF-WMWST = TRUNC( BAPICURR-BAPICURR ).
*            ZTBKPF-WMWST = ZTBKPF-WMWST * 10.
*
*            PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
*                    USING ZTBKPF-WMWST ZTBKPF-WAERS.
*         ELSE.
*            CLEAR : ZTBKPF-WMWST.
*         ENDIF.
*      ELSE.
*         CLEAR : ZTBKPF-WMWST.
*      ENDIF.
*   ENDIF.

ENDMODULE.                 " VENDOR_ACCOUNT_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_HEADER_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INPUT_HEADER_CHECK_SCR0001 INPUT.
*> 사업장.
*  IF ZTBKPF-BUPLA IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUPLA'.
*  ENDIF.
*> 지급처.
*  IF ZTBKPF-LIFNR IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'LIFNR'.
*  ENDIF.

**> 코스트 센터..
*  IF ZTBKPF-ZFPOYN NE 'Y'.
*     IF ZTBSEG-KOSTL IS INITIAL.
*        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBSEG' 'KOSTL'.
*     ENDIF.
*  ENDIF.
*> 전표통화금액.
*  IF ZTBKPF-WRBTR IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WRBTR'.
*  ENDIF.
*> 통화.
*  IF ZTBKPF-WAERS IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WAERS'.
*  ENDIF.

ENDMODULE.                 " INPUT_HEADER_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_FI_POSTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_FI_POSTING.
DATA : L_ZTBKPF LIKE ZTBKPF.

  SET UPDATE TASK LOCAL.
  CLEAR : L_ZTBKPF, ZTBKPF-ZFACDO, ZTBKPF-ZFFIYR,
                    ZTBKPF-ZFCLYR, ZTBKPF-ZFCLNO.

  CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
       EXPORTING
                  W_OK_CODE           =   'SAVE'
                  BUKRS               =   ZTBKPF-BUKRS
                  GJAHR               =   ZTBKPF-GJAHR
                  ZFSTATUS            =   'C'
                  W_ZTBKPF_OLD        =   L_ZTBKPF
                  W_ZTBKPF            =   ZTBKPF
       TABLES
                  IT_ZSBSEG           =   IT_ZSBSEG
       CHANGING
                  BELNR               =   ZTBKPF-BELNR
       EXCEPTIONS
                 ERROR_UPDATE         =   4.

  W_SUBRC = SY-SUBRC.
  IF SY-SUBRC NE 0.
     MESSAGE S494.
     PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                 USING  'E'.
     ROLLBACK WORK.
     EXIT.
  ENDIF.

  REFRESH : RETURN.
  CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_POST'
       EXPORTING
          BUKRS             =     ZTBKPF-BUKRS
          BELNR             =     ZTBKPF-BELNR
          GJAHR             =     ZTBKPF-GJAHR
       IMPORTING
          INVOICEDOCNUMBER  =     INVOICEDOCNUMBER
          FISCALYEAR        =     FISCALYEAR
       TABLES
          RETURN            =     RETURN
       EXCEPTIONS
          POST_ERROR        =     4.

  IF SY-SUBRC EQ 0.
     PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                   USING   'S'.
     COMMIT WORK.
  ELSE.
     ROLLBACK WORK.

     CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
          EXPORTING
                    W_OK_CODE           =   'DELE'
                    BUKRS               =   ZTBKPF-BUKRS
                    GJAHR               =   ZTBKPF-GJAHR
                    ZFSTATUS            =   'U'
                    W_ZTBKPF_OLD        =   L_ZTBKPF
                    W_ZTBKPF            =   ZTBKPF
          TABLES
                    IT_ZSBSEG           =   IT_ZSBSEG
          CHANGING
                    BELNR               =   ZTBKPF-BELNR
          EXCEPTIONS
                    ERROR_UPDATE         =   4.

     IF RETURN[] IS INITIAL.
        MESSAGE S494.
        PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                    USING  'E'.
     ELSE.
        PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
     ENDIF.
  ENDIF.

ENDFORM.                    " P3000_FI_POSTING
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ERR_LIST  text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST
                         USING   P_MSGTY.

   MOVE : P_MSGTY             TO     IT_ERR_LIST-MSGTYP,
          SY-MSGID            TO     IT_ERR_LIST-MSGID,
          SY-MSGNO            TO     IT_ERR_LIST-MSGNR,
          SY-MSGV1            TO     IT_ERR_LIST-MSGV1,
          SY-MSGV2            TO     IT_ERR_LIST-MSGV2,
          SY-MSGV3            TO     IT_ERR_LIST-MSGV3,
          SY-MSGV4            TO     IT_ERR_LIST-MSGV4.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
                MSGID               = IT_ERR_LIST-MSGID
                MSGNR               = IT_ERR_LIST-MSGNR
                MSGV1               = IT_ERR_LIST-MSGV1
                MSGV2               = IT_ERR_LIST-MSGV2
                MSGV3               = IT_ERR_LIST-MSGV3
                MSGV4               = IT_ERR_LIST-MSGV4
         IMPORTING
                MESSAGE_TEXT_OUTPUT = IT_ERR_LIST-MESSTXT.

   CASE IT_ERR_LIST-MSGTYP.
      WHEN 'E' OR 'A'.
         MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
      WHEN 'I'.
         MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'S'.
         MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'W'.
         MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
   ENDCASE.

   APPEND  IT_ERR_LIST.

ENDFORM.                    " P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ERR_LIST  text
*----------------------------------------------------------------------*
FORM P2000_MULTI_MSG_MAKE TABLES   IT_ERR_LIST STRUCTURE IT_ERR_LIST.

   LOOP AT  RETURN.

      MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
             RETURN-ID           TO     IT_ERR_LIST-MSGID,
             RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
             RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
             RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
             RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
             RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
             RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT.

      CASE IT_ERR_LIST-MSGTYP.
         WHEN 'E' OR 'A'.
            MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
         WHEN 'I'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'S'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'W'.
            MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
      ENDCASE.

      APPEND  IT_ERR_LIST.

   ENDLOOP.

ENDFORM.                    " P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH '메시지 LIST'.
     WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.

   LEAVE TO LIST-PROCESSING.
   CASE INCLUDE.
      WHEN 'POPU'.
         FORMAT COLOR COL_HEADING INTENSIFIED OFF.
         WRITE : / SY-ULINE(105), / SY-VLINE NO-GAP,
                   '유형'   NO-GAP, SY-VLINE NO-GAP,
                   '메세지 텍스트', 103 SY-VLINE NO-GAP,
                   'T'      NO-GAP, SY-VLINE,
                 / SY-ULINE(105).

         LOOP AT IT_ERR_LIST.
            W_MOD  =  SY-TABIX MOD 2.
            FORMAT RESET.
            IF W_MOD EQ 0.
               FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
            ELSE.
               FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
            ENDIF.
            WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4)   NO-GAP,
                      SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(96) NO-GAP,
                      SY-VLINE NO-GAP.

            CASE IT_ERR_LIST-MSGTYP.
               WHEN 'E' OR 'A'.
                  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
               WHEN 'W'.
                  FORMAT COLOR COL_KEY      INTENSIFIED OFF.
               WHEN 'I'.
                  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
               WHEN 'S'.
                  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
            ENDCASE.

            WRITE : IT_ERR_LIST-MSGTYP(1) NO-GAP, SY-VLINE NO-GAP.
*                   / SY-ULINE(96).
            HIDE:IT_ERR_LIST.
         ENDLOOP.
         WRITE : / SY-ULINE(105).
         CLEAR : IT_ERR_LIST.
      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0010 OUTPUT.

   SET TITLEBAR 'POPU' WITH SPOP-TITEL.
   SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE P2000_INIT_VALUE_CHECK INPUT.

  IF UF05A-STGRD IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'UF05A' 'STGRD'.
  ENDIF.

  IF BSIS-BUDAT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'BSIS' 'BUDAT'.
  ENDIF.

ENDMODULE.                 " P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_HEADER_DATA_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_HEADER_DATA_MAKE.
  CLEAR : ZTBKPF.

*> POSTING DATA MOVE.
  SELECT SINGLE * FROM  T001
                  WHERE BUKRS EQ P_BUKRS.

  SELECT SINGLE * FROM  T005
                  WHERE LAND1 EQ T001-LAND1.

  SELECT SINGLE * FROM ZTIMIMG11
                  WHERE BUKRS EQ P_BUKRS.

     CALL FUNCTION 'FI_VENDOR_DATA'
           EXPORTING
              i_bukrs = P_BUKRS
              i_lifnr = P_LIFNR
           IMPORTING
              e_kred  = vf_kred
           EXCEPTIONS
              OTHERS  = 4.
     IF SY-SUBRC NE 0.
        EXIT.
     ENDIF.

     ZTBKPF-ZTERM = VF_KRED-ZTERM.
     ZTBKPF-AKONT = VF_KRED-AKONT.
     *LFA1-NAME1  = VF_KRED-NAME1.


     MOVE: SY-MANDT        TO ZTBKPF-MANDT,
           'X'             TO ZTBKPF-ZFPCUR,
*          'Y'             TO ZTBKPF-ZFPOYN,
           P_BUKRS         TO ZTBKPF-BUKRS,
           SY-DATUM        TO ZTBKPF-BUDAT,
           SY-DATUM        TO ZTBKPF-BLDAT,
           SY-DATUM        TO ZTBKPF-ZFBDT,
           P_BLART         TO ZTBKPF-BLART,
*           '003'           TO ZTBKPF-ZFCSTGRP,
           SY-DATUM+4(2)   TO ZTBKPF-MONAT,
           P_MWSKZ         TO ZTBKPF-MWSKZ,
           1               TO ZTBKPF-KURSF,
           SY-DATUM        TO ZTBKPF-WWERT,
           'A'             TO ZTBKPF-ZLSPR,
           '업무대행료'    TO ZTBKPF-BKTXT,
           T001-WAERS      TO ZSREQHD-ZFKRW,
           T001-WAERS      TO ZTBKPF-HWAER,
*           *ZTBKPF-WRBTR   TO ZTBKPF-WRBTR,
*           *ZTBKPF-WRBTR   TO *ZTBKPF-WRBTR,
           T001-WAERS      TO *ZTBKPF-WAERS,
*           *ZTBKPF-WRBTR   TO ZTBKPF-DMBTR,
           T001-WAERS      TO ZTBKPF-WAERS,
           P_LIFNR         TO ZTBKPF-LIFNR,
           P_LIFNR         TO ZTBKPF-ZFVEN.


ENDFORM.                    " P2000_HEADER_DATA_MAKE
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_COST_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SHOW_COST_DOCUMENT USING    P_BUKRS
                                       P_GJAHR
                                       P_BELNR.
   IF P_BELNR IS INITIAL.
      MESSAGE S588.
      EXIT.
   ENDIF.
   SET  PARAMETER ID  'BUK'       FIELD   P_BUKRS.
   SET  PARAMETER ID  'GJR'       FIELD   P_GJAHR.
   SET  PARAMETER ID  'ZPBENR'    FIELD   P_BELNR.
   CALL TRANSACTION 'ZIMY3'.

ENDFORM.                    " P2000_SHOW_COST_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  P2000_POSTING_CANCLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_POSTING_CANCLE.
  REFRESH : IT_ERR_LIST.

  LOOP AT IT_SELECTED.

*>>> 진행상태바..
     line = ( sy-tabix / w_selected_lines ) * 100.
     out_text = 'JOB PROGRESS %99999%%'.
     replace '%99999%' with line into out_text.
     perform p2000_show_bar using out_text line.


     SELECT  SINGLE * FROM ZTBKPF
     WHERE   BUKRS    EQ   IT_SELECTED-BUKRS
     AND     GJAHR    EQ   IT_SELECTED-GJAHR
     AND     BELNR    EQ   IT_SELECTED-BELNR.

     IF ZTBKPF-ZFPOSYN EQ 'N'.
        MESSAGE  S577.
        PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                     USING   'E'.
        CONTINUE.
     ENDIF.

*>> AP POSTING FUNCTION CALL
     CALL FUNCTION 'ZIM_BAPI_COST_INVOICES_CANCEL'
          EXPORTING
            P_ZFIMDTY        =     'CS'
            P_BUKRS          =     ZTBKPF-BUKRS
            INVOICEDOCNUMBER =     ZTBKPF-ZFACDO
            FISCALYEAR       =     ZTBKPF-ZFFIYR
            REASONREVERSAL   =     UF05A-STGRD
            POSTINGDATE      =     BSIS-BUDAT
         IMPORTING
            INVOICEDOCNUMBER_REVERSAL =     INVOICEDOCNUMBER
            FISCALYEAR_REVERSAL       =     FISCALYEAR
         TABLES
            RETURN           =     RETURN
         EXCEPTIONS
            LIV_ERROR        =     4.

     IF SY-SUBRC NE 0.           ">> 오류 발생시...
        IF RETURN[] IS INITIAL.
           MESSAGE  S691.
           PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'E'.
        ELSE.
           PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
        ENDIF.
     ELSE.
        PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'S'.
     ENDIF.

  ENDLOOP.

  DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
  IF W_LINE GT 0.
     INCLUDE = 'POPU'.
     CALL SCREEN 0100 STARTING AT  05   3
                      ENDING   AT  100 12.
     CLEAR : INCLUDE.
  ENDIF.


ENDFORM.                    " P2000_POSTING_CANCLE
*&---------------------------------------------------------------------*
*&      Form  P1000_IMPORT_DOC_CHEKC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_IMPORT_DOC_CHEKC USING    ZTBKPF-ZFCSTGRP
                                     P_ZFIMDNO
                                     P_ZFDCNM
                                     P_ZFPOYN
                                     P_GUBUN
                                     P_KOSTL
                                     ZTBKPF-GSBER
                                     ZTBKPF-BUPLA.

   CLEAR : P_ZFDCNM, ZTREQHD, ZTBL, ZTIV, ZTCGHD, ZTMSHD.

   IF P_ZFIMDNO IS INITIAL.
      EXIT.
   ENDIF.

   CASE ZTBKPF-ZFCSTGRP.
      WHEN '003'.           ">수입의뢰.
         SELECT SINGLE * FROM ZTREQHD
                         WHERE ZFREQNO EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.
         MOVE: ZTREQHD-ZFOPNNO TO P_ZFDCNM.
         P_ZFPOYN = 'Y'.
         IF W_SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
                 EXPORTING
                    UNAME   =    SY-UNAME
                    WERKS   =    ZTREQHD-ZFWERKS
                 IMPORTING
                    GSBER   =    ZTBKPF-GSBER
                    BUPLA   =    ZTBKPF-BUPLA.
         ENDIF.
      WHEN '004' OR '005'.  ">B/L 관리번호.
         SELECT SINGLE * FROM ZTBL
                         WHERE ZFBLNO  EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.
         IF W_SUBRC EQ 0.
            P_ZFPOYN = ZTBL-ZFPOYN.
            P_KOSTL  = ZTBL-KOSTL.
         ELSE.
            P_ZFPOYN = 'Y'.
         ENDIF.
*> 1. 화물관리번호.
         IF NOT ZTBL-ZFGMNO IS INITIAL.
            MOVE: ZTBL-ZFGMNO TO P_ZFDCNM.
            IF NOT ZTBL-ZFMSN IS INITIAL.
               CONCATENATE P_ZFDCNM '-' ZTBL-ZFMSN INTO P_ZFDCNM.
            ENDIF.
            IF NOT ZTBL-ZFHSN IS INITIAL.
               CONCATENATE P_ZFDCNM '-' ZTBL-ZFHSN INTO P_ZFDCNM.
            ENDIF.
         ELSE.
*> 2. HOUSE B/L No.
            IF NOT ZTBL-ZFHBLNO IS INITIAL.
               MOVE: ZTBL-ZFHBLNO TO P_ZFDCNM.
            ELSE.
*> 3. MASTER B/L No.
               IF NOT ZTBL-ZFMBLNO IS INITIAL.
                  MOVE: ZTBL-ZFMBLNO TO P_ZFDCNM.
               ELSE.
*> 4. 선사 B/L No.
                  IF NOT ZTBL-ZFCGHNO IS INITIAL.
                     MOVE: ZTBL-ZFCGHNO TO P_ZFDCNM.
                  ENDIF.
               ENDIF.
            ENDIF.
         ENDIF.
         IF W_SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
                 EXPORTING
                    UNAME   =    SY-UNAME
                    WERKS   =    ZTBL-ZFWERKS
                 IMPORTING
                    GSBER   =    ZTBKPF-GSBER
                    BUPLA   =    ZTBKPF-BUPLA.
         ENDIF.

      WHEN '006'.           ">통관관리번호.
         SELECT SINGLE * FROM ZTIV
                         WHERE ZFIVNO  EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.

         IF W_SUBRC EQ 0.
            P_ZFPOYN = ZTIV-ZFPOYN.
         ELSE.
            P_ZFPOYN = 'Y'.
         ENDIF.

         IF W_SUBRC EQ 0.
            IF ZTIV-ZFCUST EQ '3' OR ZTIV-ZFCUST EQ 'Y'.
               SELECT SINGLE * FROM ZTCUCLIV
                               WHERE ZFIVNO EQ P_ZFIMDNO.
               IF SY-SUBRC EQ 0.
                  SELECT SINGLE * FROM ZTIDR
                                  WHERE ZFBLNO  EQ ZTCUCLIV-ZFBLNO
                                  AND   ZFCLSEQ EQ ZTCUCLIV-ZFCLSEQ.
                  IF SY-SUBRC EQ 0.
                     IF ZTIDR-ZFIDRNO IS INITIAL.
                        MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
                     ELSE.
                        MOVE: ZTIDR-ZFIDRNO TO P_ZFDCNM.
                     ENDIF.
                  ELSE.
                     MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
                  ENDIF.
               ELSE.
                  MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
               ENDIF.
            ELSE.
               MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
            ENDIF.
         ENDIF.
         SELECT SINGLE * FROM ZTBL
                WHERE ZFBLNO EQ ZTIV-ZFBLNO.

         IF SY-SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
            P_KOSTL  = ZTBL-KOSTL.
            P_ZFPOYN = ZTBL-ZFPOYN.

            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
                 EXPORTING
                    UNAME   =    SY-UNAME
                    WERKS   =    ZTBL-ZFWERKS
                 IMPORTING
                    GSBER   =    ZTBKPF-GSBER
                    BUPLA   =    ZTBKPF-BUPLA.
         ENDIF.

      WHEN '007'.           ">하역관리번호.
         SELECT SINGLE * FROM ZTCGHD
                         WHERE ZFCGNO  EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.
         P_ZFPOYN = 'Y'.
         IF SY-SUBRC EQ 0.
            IF NOT ZTCGHD-ZFMSNO IS INITIAL.
               SELECT SINGLE * FROM  ZTMSHD
                               WHERE ZFMSNO  EQ  ZTCGHD-ZFMSNO.
               IF SY-SUBRC EQ 0.
                  MOVE ZTMSHD-ZFMSNM  TO  P_ZFDCNM.
               ENDIF.
            ENDIF.
         ENDIF.
      WHEN '008'.           ">기납증리번호.
         SELECT SINGLE * FROM ZTTAXBKHD
                         WHERE ZFTBNO  EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.
         IF SY-SUBRC EQ 0.
            IF ZTTAXBKHD-BASISNO IS INITIAL.
               MOVE ZTTAXBKHD-EBELN    TO  P_ZFDCNM.
            ELSE.
               MOVE ZTTAXBKHD-BASISNO  TO  P_ZFDCNM.
            ENDIF.
         ELSE.
            CLEAR : P_ZFDCNM.
         ENDIF.
         SELECT SINGLE * FROM ZTREQHD
                WHERE ZFREQNO EQ ZTTAXBKHD-ZFREQNO.
*          IF SY-SUBRC EQ 0.
         IF SY-SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.

            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
                 EXPORTING
                    UNAME   =    SY-UNAME
                    WERKS   =    ZTREQHD-ZFWERKS
                 IMPORTING
                    GSBER   =    ZTBKPF-GSBER
                    BUPLA   =    ZTBKPF-BUPLA.
         ENDIF.
      WHEN OTHERS.
         EXIT.
   ENDCASE.

*>>오류가 발생했을 경우.
   IF SY-SUBRC NE 0.
*      IF P_GUBUN EQ 'H'.
*         PERFORM P2000_NO_INPUT USING 'ZTBKPF' 'ZFIMDNO'.
*      ELSE.
*         PERFORM P2000_NO_INPUT USING 'ZSBSEG' 'ZFIMDNO'.
*      ENDIF.

      CASE ZTBKPF-ZFCSTGRP.
         WHEN '003'.
            MESSAGE E585 WITH '수입의뢰번호' P_ZFIMDNO.
         WHEN '004' OR '005'.
            MESSAGE E585 WITH 'B/L 관리번호' P_ZFIMDNO.
         WHEN '006'.
            MESSAGE E585 WITH '통관관리번호' P_ZFIMDNO.
         WHEN '007'.
            MESSAGE E585 WITH '하역관리번호' P_ZFIMDNO.
         WHEN '008'.
            MESSAGE E585 WITH '기납증관리번호' P_ZFIMDNO.
      ENDCASE.
   ENDIF.

ENDFORM.                    " P1000_IMPORT_DOC_CHEKC
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_NEWKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SET_NEWKO   USING ZSBSEG-ZFCSTGRP
                             ZSBSEG-NEWKO
                             ZSBSEG-ZFCD
                             ZSBSEG-ZFIMDNO.
*> 계정결정 함수.
   CALL FUNCTION 'ZIM_GET_NODRAFT_ACCOUNT'
        EXPORTING
           ZFCSTGRP   =     ZSBSEG-ZFCSTGRP
           ZFCD       =     ZSBSEG-ZFCD
           ZFIMDNO    =     ZSBSEG-ZFIMDNO
        IMPORTING
           NEWKO      =     ZSBSEG-NEWKO.

ENDFORM.                    " P2000_SET_NEWKO
