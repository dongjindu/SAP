*&---------------------------------------------------------------------
*& Report  ZRIMBSEGLST
*&---------------------------------------------------------------------
*&  프로그램명 : 제세사후납부 LIST
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.09.18
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&
*&---------------------------------------------------------------------
REPORT  ZRIMBSEGLST  MESSAGE-ID ZIM
                     LINE-SIZE 128
                     NO STANDARD PAGE HEADING.

TABLES: ZTBKPF,
        ZTBSEG,
        ZTBL,
        ZTIDS,
        ZTIMIMG10,
        LFA1,
        ZTIV,
        ZTCUCLIV,
        T001,
        ZTIMIMG02,
        ZTIMIMG08.

*----------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*----------------------------------------------------------------------
DATA : BEGIN OF IT_IMG02 OCCURS 0,
       ZFCOTM  LIKE ZTIMIMG02-ZFCOTM, " 관세사.
       ZFACTNO LIKE ZTIMIMG02-ZFACTNO,
       ZFVEN   LIKE ZTIMIMG02-ZFVEN. " 구매처.
DATA : END OF IT_IMG02.

DATA : BEGIN OF IT_BSEG OCCURS 0,
       ZFPONO(13),                            " FILE NO.
       ZFBLNO    LIKE    ZTIDS-ZFBLNO,
       ZFCLSEQ   LIKE    ZTIDS-ZFCLSEQ,
       ZFINRC    LIKE    ZTIDS-ZFINRC,       " 세관.
       ZFACDO    LIKE    ZTBKPF-ZFACDO,       " 회계전표번호.
       ZFWERKS   LIKE    ZTBL-ZFWERKS,       " 플랜트
       ZFACTNO  LIKE     ZTIMIMG02-ZFACTNO,  " 계좌번호.
       LIFNR     LIKE    ZTBKPF-LIFNR,       " 구매처.
       STCD2     LIKE    LFA1-STCD2,         " 사업자등록번호.
       ZFBLSDP   LIKE    ZTBL-ZFBLSDP,       " 송부처.
       ZFIDRNO   LIKE    ZTIDS-ZFIDRNO,      " 수입신고번호.
       ZFRFFNO   LIKE    ZTIDS-ZFRFFNO,      " 납부자번호.
       ZFCODE    LIKE    ZTIDS-ZFCOCD,       " 징수형태.
       ZFCD      LIKE    ZTBSEG-ZFCD,
       ZFREBELN  LIKE    ZTBL-ZFREBELN,      " P/O NO
       ZFSHNO    LIKE    ZTBL-ZFSHNO,        " 선적차수.
       ZFIMDNO   LIKE    ZTBSEG-ZFIMDNO,	    " 수입문서번호.
       BUKRS	   LIKE    ZTBSEG-BUKRS,       "  회사코드.
       BELNR	   LIKE    ZTBKPF-BELNR,	    " 수입비용 문서번호.
       ZFRVSX   LIKE    ZTBKPF-ZFRVSX,
       WAERS    LIKE    ZTBKPF-WAERS,
       HWAER    LIKE    ZTBKPF-HWAER,
       GJAHR	   LIKE    ZTBSEG-GJAHR,	    " 회계연도.
       ZFCSTGRP  LIKE   ZTBSEG-ZFCSTGRP,    " 비용그룹.
       ZFCD#   LIKE     ZTBSEG-ZFCD,        " 관리코드.
       WRBTR	   LIKE    ZTBSEG-WRBTR,	    " 전표통화금액.
       DMBTR    LIKE    ZTBSEG-DMBTR,
       WMWST    LIKE    ZTBSEG-WMWST,
       FWBAS    LIKE    ZTBSEG-FWBAS,
*      ZFBDT	   LIKE    ZTBKPF-ZFBDT,       " 전표내 증빙일.
       ZFBDT     LIKE    ZTBKPF-ZFBDT,       " 기산일.
       BUDAT	   LIKE    ZTBKPF-BUDAT,       " 전표전기일
       ZFVAAMTS  LIKE     ZTIDS-ZFVAAMTS,    " 총부가세.
       ZFCUAMTS  LIKE     ZTIDS-ZFCUAMTS,    " 총관세,
       ZFTOT     LIKE     ZTIDS-ZFCUAMTS,    " 총액.
       ZFRGDSR   LIKE    ZTBL-ZFRGDSR,       " 대표품명.
       ZFPCUR    LIKE    ZTBKPF-ZFPCUR.      " 전기여부
DATA : END OF IT_BSEG.

DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFBLNO    LIKE    ZTIDS-ZFBLNO,
       ZFCLSEQ   LIKE    ZTIDS-ZFCLSEQ,      "
*       BUKRS     LIKE    ZTBSEG-BUKRS,      "  회사코드.
*       BELNR     LIKE    ZTBKPF-BELNR,      " 수입비용 문서번호.
*       GJAHR     LIKE    ZTBSEG-GJAHR,      " 회계연도.
*      ZFACDO    LIKE    ZTBKPF-ZFACDO,      " 회계전표번호.
       ZFINRC    LIKE    ZTIDS-ZFINRC,      " 세관.
*      LIFNR     LIKE    ZTBKPF-LIFNR,       " 구매처.
       ZFREBELN  LIKE    ZTBL-ZFREBELN,      " P/O NO
       STCD2     LIKE    LFA1-STCD2,         " 사업자등록번호.
       ZFACTNO   LIKE    ZTIMIMG02-ZFACTNO,  " 계좌번호.
       ZFRFFNO   LIKE    ZTIDS-ZFRFFNO,      " 납부자번호.
       ZFPONO(13),                           " FILE NO.
       ZFRGDSR    LIKE     ZTBL-ZFRGDSR,     " 대표품명.
       W_COUNT    TYPE I,                    " 건수.
       ZFIDRNO    LIKE     ZTIDS-ZFIDRNO,    " 수입신고번호.
       ZFBLSDP    LIKE     ZTBL-ZFBLSDP,     " 송부처.
       ZFVAAMTS   LIKE     ZTIDS-ZFVAAMTS,   " 총부가세.
       ZFCUAMTS   LIKE     ZTIDS-ZFCUAMTS,   " 총관세,
       ZFTOT      LIKE     ZTIDS-ZFCUAMTS.   " 총액.
DATA : END OF IT_TAB.

DATA : W_ZFVAAMTS   LIKE     ZTIDS-ZFVAAMTS,
       W_ZFCUAMTS   LIKE     ZTIDS-ZFCUAMTS,   " 총관세,
       W_ZFTOT      LIKE     ZTIDS-ZFCUAMTS,
       W_ZFVAAMTST  LIKE     ZTIDS-ZFVAAMTS,
       W_ZFCUAMTST  LIKE     ZTIDS-ZFCUAMTS,   " 총관세,
       W_ZFTOTT     LIKE     ZTIDS-ZFCUAMTS.

DATA : W_STCD2   LIKE    LFA1-STCD2,         " 사업자등록번호.
       W_ZFACTNO LIKE    ZTIMIMG02-ZFACTNO.  " 계좌번호.

DATA :  W_ERR_CHK     TYPE C,
        W_LCOUNT      TYPE I,
        W_FIELD_NM    TYPE C,
        W_PAGE        TYPE I,
        W_TITLE(50),
        W_DOM_TEX1     LIKE DD07T-DDTEXT,
        W_FNAME        LIKE ZTIMIMG08-ZFCDNM,
        W_LINE        TYPE I,
        W_COUNT       TYPE I,
        W_SUBRC       LIKE SY-SUBRC,
        W_TABIX       LIKE SY-TABIX,
        W_ZFCLSEQ     LIKE ZTIDS-ZFCLSEQ,
        W_LIST_INDEX  LIKE SY-TABIX.
*>> SUTOTAL.
 DATA: S_COUNT     TYPE I,
       S_ZFTOWT    LIKE  ZTBL-ZFTOWT,
       S_ZFVAAMTS  LIKE  ZTIDS-ZFVAAMTS,
       S_ZFTXAMTS  LIKE  ZTIDS-ZFTXAMTS.

DATA : BEGIN OF IT_BLSDP_HELP OCCURS 0,
       ZFBLSDP   LIKE ZTIMIMG08-ZFCD,
       ZFCDNM    LIKE ZTIMIMG08-ZFCDNM,
       END OF IT_BLSDP_HELP.

DATA: DYNPRO             LIKE SY-REPID,
      DYNNR              LIKE SY-DYNNR.


INCLUDE   ZRIMSORTCOM.    " REPORT Sort
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

   PARAMETERS : P_BUKRS   LIKE ZTBKPF-BUKRS DEFAULT 'KHNP',
                P_ZFBDT   LIKE ZTBKPF-ZFBDT OBLIGATORY. " 전표내 기준일

   SELECT-OPTIONS: S_CUT     FOR  ZTIMIMG02-ZFCOTM,
                   S_BLSDP   FOR  ZTBL-ZFBLSDP.         " 송부처.

 SELECTION-SCREEN END OF BLOCK B1.

 AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BLSDP-LOW.
   PERFORM   P1000_BLSDP_HELP  USING  S_BLSDP-LOW 'S_BLSDP_LOW'.

 AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BLSDP-HIGH.
   PERFORM   P1000_BLSDP_HELP  USING  S_BLSDP-HIGH 'S_BLSDP_HIGH'.


 INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.

*title Text Write
TOP-OF-PAGE.
 PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*----------------------------------------------------------------------
* START OF SELECTION ?
*----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* B/L 테이블 SELECT
   PERFORM   P1000_READ_DATA    USING  W_ERR_CHK.
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
         W_FIELD_NM = 'ZFIDRNO'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
      WHEN 'REFR'.
          PERFORM P1000_READ_DATA USING W_ERR_CHK.
          PERFORM RESET_LIST.
      WHEN 'DISP'.
          IF W_TABIX IS INITIAL.
            MESSAGE S962.    EXIT.
          ENDIF.
           PERFORM P2000_PO_DOC_DISPLAY(SAPMZIM01)
                                     USING IT_TAB-ZFREBELN ''.
      WHEN 'DISP1'.
        IF W_TABIX IS INITIAL.
           MESSAGE S962.    EXIT.
        ENDIF.
        PERFORM P2000_DISP_ZTIDS USING IT_TAB-ZFBLNO
                                       IT_TAB-ZFCLSEQ.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
          PERFORM P3000_TO_PC_DOWNLOAD.
  ENDCASE.
  CLEAR: IT_TAB, W_TABIX.

*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

  SKIP 2.
  CLEAR T001.
  SELECT SINGLE *
          FROM T001
          WHERE BUKRS = P_BUKRS.
  CONCATENATE '[' T001-BUTXT '제세사후납부 LIST]' INTO W_TITLE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  W_TITLE.
  SKIP 2.
  WRITE : / '납부일자 :',P_ZFBDT.

  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-ULINE.
  WRITE : / SY-VLINE NO-GAP,(13) '구매문서'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(14) '신고 번호'    NO-GAP CENTERED,
*            SY-VLINE NO-GAP,(25) '품명'       NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) '관      세'   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) '부  가  세'   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) '합      계'   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(08) '계좌번호'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(15) '납부고지번호' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '비 고'        NO-GAP CENTERED,
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
*      MESSAGE S960 WITH SY-UNAME 'B/L Doc transaction'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK

*&---------------------------------------------------------------------
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------
FORM P1000_READ_DATA USING W_ERR_CHK.

  W_ERR_CHK = 'N'.
  IF  P_ZFBDT IS INITIAL.
       W_ERR_CHK = 'Y'. MESSAGE S193. EXIT.
  ENDIF.
  IF  P_BUKRS IS INITIAL.
     W_ERR_CHK = 'Y'. MESSAGE S977 WITH 'Input company code'. EXIT.
  ENDIF.
  RANGES: R_ZFVEN FOR ZTIMIMG02-ZFVEN OCCURS 100.
*>>  ZTIMIMG10.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_IMG02
      FROM ZTIMIMG02
     WHERE  ZFCOTM IN S_CUT.
  IF SY-SUBRC <> 0.  W_ERR_CHK = 'Y'.   EXIT.    ENDIF.

  LOOP AT IT_IMG02.
    MOVE : 'I'               TO  R_ZFVEN-SIGN,
           'EQ'              TO  R_ZFVEN-OPTION,
           IT_IMG02-ZFVEN    TO  R_ZFVEN-LOW,
           SPACE             TO  R_ZFVEN-HIGH.
    APPEND R_ZFVEN.
  ENDLOOP.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BSEG
            FROM ZTBKPF AS R INNER JOIN ZTBSEG AS I
             ON R~BUKRS = I~BUKRS
            AND R~BELNR = I~BELNR
            AND R~GJAHR = I~GJAHR
         WHERE  R~ZFPOSYN  = 'Y'
           AND  R~ZFBDT   = P_ZFBDT          " 기준일.
           AND  I~ZFCSTGRP = '006'
           AND  R~BUKRS    = P_BUKRS
           AND  I~ZFCD     IN ('001', '003')
           AND  R~LIFNR    IN R_ZFVEN.

  LOOP AT IT_BSEG.

     W_TABIX = SY-TABIX.
*>> 수입신고번호.--------------------------------------------------
*     CLEAR ZTCUCLIV.
*    SELECT SINGLE *
*       FROM  ZTCUCLIV
*       WHERE ZFIVNO = IT_BSEG-ZFIMDNO.
     CLEAR ZTIDS.
     SELECT SINGLE *
        FROM ZTIDS
        WHERE ZFIVNO  = IT_BSEG-ZFIMDNO
*          AND ZFCLSEQ = ZTCUCLIV-ZFCLSEQ
          AND ZFCOCD  IN ('13', '33').
     IF SY-SUBRC NE 0.
        DELETE IT_BSEG INDEX W_TABIX.
        CONTINUE.
     ENDIF.
*>> 징수형태.
*     IF ZTIDS-ZFCOCD = '33'.
     IF ZTIDS-ZFCOCD = '13'.
          IF IT_BSEG-ZFCD = '001'. " 관세.
              CLEAR IT_BSEG-WRBTR.
          ENDIF.
     ENDIF.

*---------------B/L DATA MOVE--------------------------------------
     CLEAR ZTIV.
     SELECT  SINGLE *
        FROM  ZTIV
        WHERE ZFIVNO = IT_BSEG-ZFIMDNO.
     CLEAR ZTBL.
     SELECT SINGLE *
        FROM ZTBL
        WHERE ZFBLNO  = ZTIV-ZFBLNO
          AND ZFBLSDP IN S_BLSDP.
    IF SY-SUBRC NE 0.
       DELETE IT_BSEG INDEX W_TABIX.
       CONTINUE.
    ENDIF.
*>> 계좌번호.
    CLEAR ZTIMIMG02.
    SELECT * FROM  ZTIMIMG02 UP TO 1 ROWS
             WHERE ZFCOTM  = ZTIDS-ZFINRC.

*             AND   ZFWERKS = ZTBL-ZFWERKS.
       MOVE ZTIMIMG02-ZFACTNO TO IT_BSEG-ZFACTNO.
    ENDSELECT.
*>> 수입면허번호.
    MOVE ZTIDS-ZFIDRNO TO IT_BSEG-ZFIDRNO.
    MOVE ZTIDS-ZFINRC  TO IT_BSEG-ZFINRC.
    MOVE ZTIDS-ZFRFFNO TO IT_BSEG-ZFRFFNO.      " 납부자번호.
    MOVE ZTIDS-ZFBLNO  TO IT_BSEG-ZFBLNO.
    MOVE ZTIDS-ZFCLSEQ TO IT_BSEG-ZFCLSEQ.

*>> 송부처.
    MOVE ZTBL-ZFBLSDP TO IT_BSEG-ZFBLSDP.
*>> 대표품명.
    MOVE ZTBL-ZFRGDSR TO IT_BSEG-ZFRGDSR.
*>> P/O No.
    IF NOT ZTBL-ZFSHNO IS INITIAL.
        CONCATENATE ZTBL-ZFREBELN '-' ZTBL-ZFSHNO INTO IT_BSEG-ZFPONO.
    ELSE.
*    MOVE ZTBL-ZFREBELN TO  IT_BSEG-ZFREBELN.
        MOVE ZTBL-ZFREBELN TO IT_BSEG-ZFPONO.
    ENDIF.
*-----------------------------------------------------------------
*>> 사업자 등록 번호.
    CLEAR LFA1.
    SELECT SINGLE *
        FROM LFA1
      WHERE  LIFNR  = IT_BSEG-LIFNR .
    MOVE  LFA1-STCD2 TO IT_BSEG-STCD2.         " 사업자등록번호.

*> 역기표 전표..
    IF IT_BSEG-ZFRVSX = 'X'.
       IT_BSEG-WRBTR = IT_BSEG-WRBTR * -1.
       IT_BSEG-DMBTR = IT_BSEG-DMBTR * -1.
       IT_BSEG-FWBAS = IT_BSEG-FWBAS * -1.
       IT_BSEG-WMWST = IT_BSEG-WMWST * -1.
    ENDIF.

*>> 관세만 있어도 부가세는 전기가 안되어도 보여 준다.
    IF IT_BSEG-ZFCD = '001'.
       IT_BSEG-ZFCUAMTS =  IT_BSEG-WRBTR.
       IT_BSEG-ZFVAAMTS =  ZTIDS-ZFVAAMTS.
    ENDIF.
*>> 부가세.
    IF IT_BSEG-ZFCD = '003'.
       IT_BSEG-ZFVAAMTS =  IT_BSEG-WRBTR.
    ENDIF.

    IT_BSEG-ZFTOT =  IT_BSEG-ZFCUAMTS + IT_BSEG-ZFVAAMTS.

    MODIFY IT_BSEG INDEX W_TABIX.

    MOVE-CORRESPONDING IT_BSEG TO IT_TAB.
    COLLECT IT_TAB.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE = 0.
      W_ERR_CHK = 'Y'.
  ENDIF.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE .
DATA : L_ZFINRC LIKE IT_TAB-ZFINRC,
       O_ZFINRC LIKE IT_TAB-ZFINRC.

   SORT IT_TAB BY ZFINRC.
   SET TITLEBAR  'ZIMR67'.
   SET PF-STATUS 'ZIMR67'.
   CLEAR : W_COUNT, L_ZFINRC.
   CLEAR : W_ZFVAAMTS,  W_ZFCUAMTS,  W_ZFTOT,
           W_ZFVAAMTST, W_ZFCUAMTST, W_ZFTOTT.

   DESCRIBE TABLE IT_TAB LINES W_LINE.

   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      PERFORM   P3000_LINE_WRITE.
      W_STCD2 = IT_TAB-STCD2.
*      AT END OF ZFINRC.
*         PERFORM   P3000_SUB_TOTOL_WRITE.
*      ENDAT.
      IF O_ZFINRC NE IT_TAB-ZFINRC AND W_TABIX NE 1.
         PERFORM   P3000_SUB_TOTOL_WRITE1.
      ENDIF.
      MOVE IT_TAB-ZFINRC TO L_ZFINRC.
      AT LAST.
         IF O_ZFINRC NE L_ZFINRC.
            PERFORM   P3000_SUB_TOTOL_WRITE1.
         ENDIF.
         PERFORM P3000_LINE_TOTAL.
      ENDAT.
      O_ZFINRC = IT_TAB-ZFINRC.
   ENDLOOP.
   CLEAR: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

  SET TITLEBAR  'ZIMR67'.
  P_ZFBDT = SY-DATUM.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE : / SY-VLINE NO-GAP,(13) IT_TAB-ZFPONO NO-GAP,     " P/O NO
            SY-VLINE NO-GAP,(14) IT_TAB-ZFIDRNO  NO-GAP,
*           SY-VLINE NO-GAP,(25) WA_IT_TAB-ZFRGDSR NO-GAP,
            SY-VLINE NO-GAP,(19) IT_TAB-ZFCUAMTS CURRENCY 'KRW'
                                                 NO-GAP,
            SY-VLINE NO-GAP,(19) IT_TAB-ZFVAAMTS CURRENCY 'KRW'
                                                 NO-GAP,
            SY-VLINE NO-GAP,(19) IT_TAB-ZFTOT CURRENCY 'KRW'
                                                 NO-GAP,
            SY-VLINE NO-GAP,(08) IT_TAB-ZFACTNO  NO-GAP,
            SY-VLINE NO-GAP,(15) IT_TAB-ZFRFFNO  NO-GAP,
            SY-VLINE NO-GAP,(12) ' '  NO-GAP,
            SY-VLINE.
  HIDE : IT_TAB,W_TABIX.
*  WRITE : / SY-ULINE.
  W_ZFACTNO = IT_TAB-ZFACTNO.
  W_COUNT = W_COUNT + 1.
  S_COUNT = S_COUNT + 1.

  ADD : IT_TAB-ZFVAAMTS TO W_ZFVAAMTS,
        IT_TAB-ZFCUAMTS TO W_ZFCUAMTS,
        IT_TAB-ZFTOT    TO W_ZFTOT,
        IT_TAB-ZFVAAMTS TO W_ZFVAAMTST,
        IT_TAB-ZFCUAMTS TO W_ZFCUAMTST,
        IT_TAB-ZFVAAMTS TO W_ZFTOTT.

ENDFORM.

*&---------------------------------------------------------------------
*
*&      Form  P3000_LINE_TOTAL
*&---------------------------------------------------------------------
*
FORM P3000_LINE_TOTAL.

*  SKIP 1.
  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED ON.
  WRITE:/ SY-ULINE.
  SUM.
  WRITE : / SY-VLINE NO-GAP,(14) 'TOTAL:'   NO-GAP,     " P/O NO
                            (06) W_COUNT,(05)'건', ''  NO-GAP,
*           SY-VLINE NO-GAP,(25) IT_TAB-ZFRGDSR NO-GAP,
            SY-VLINE NO-GAP,(19) W_ZFCUAMTST CURRENCY 'KRW'
                                                 NO-GAP,
            SY-VLINE NO-GAP,(19) W_ZFVAAMTST CURRENCY 'KRW'
                                                 NO-GAP,
            SY-VLINE NO-GAP,(19) W_ZFTOTT CURRENCY 'KRW'
                                                 NO-GAP,
            SY-VLINE NO-GAP,(08) ''  NO-GAP,
                     (16)   ''  NO-GAP,
                     (13) ' '  NO-GAP,
            SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LINE_TOTAL
*&---------------------------------------------------------------------
*
*&      Form  P3000_SUB_TOTOL_WRITE
*&---------------------------------------------------------------------
FORM P3000_SUB_TOTOL_WRITE.

  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCOTM' IT_TAB-ZFINRC
                                      CHANGING   W_DOM_TEX1.
  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE : / SY-ULINE.
  SUM.
  WRITE : / SY-VLINE NO-GAP,(13) W_DOM_TEX1 NO-GAP,     " P/O NO
                            (06) S_COUNT,(05)'건', ''  NO-GAP,
*           SY-VLINE NO-GAP,(25) IT_TAB-ZFRGDSR NO-GAP,
            SY-VLINE NO-GAP,(19) IT_TAB-ZFCUAMTS CURRENCY 'KRW'
                                                 NO-GAP,
            SY-VLINE NO-GAP,(19) IT_TAB-ZFVAAMTS CURRENCY 'KRW'
                                                 NO-GAP,
            SY-VLINE NO-GAP,(19) IT_TAB-ZFTOT CURRENCY 'KRW'
                                                 NO-GAP,
            SY-VLINE NO-GAP,
            (15) '사업자등록번호:'   NO-GAP,
            (13)  W_STCD2 NO-GAP,(08) '',
            SY-VLINE.
  WRITE:/ SY-ULINE.


  CLEAR: S_COUNT,W_STCD2.

 ENDFORM.
*&---------------------------------------------------------------------
*&      Form  RESET_LIST
*&---------------------------------------------------------------------
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.
  PERFORM   P3000_TITLE_WRITE.     " 해더 출력...
  PERFORM   P3000_DATA_WRITE.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------
*
*&      Form  P2000_DISP_ZTIDS
*&---------------------------------------------------------------------
*
FORM P2000_DISP_ZTIDS USING    P_ZFBLNO P_ZFCLSEQ.

  SET PARAMETER ID 'ZPHBLNO' FIELD SPACE.
  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPCLSEQ' FIELD P_ZFCLSEQ.
  SET PARAMETER ID 'ZPIDRNO' FIELD SPACE.

  CALL TRANSACTION 'ZIM76' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTIDS
*&---------------------------------------------------------------------*
*&      Form  P1000_BLSDP_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_ZTERM_LOW  text
*----------------------------------------------------------------------*
FORM P1000_BLSDP_HELP USING  P_ZFBLSDP P_FIELDNAME .
DATA : L_DISPLAY.
DATA: WINDOW_TITLE(20) TYPE C.

   REFRESH : IT_BLSDP_HELP.
   SELECT *
          FROM   ZTIMIMG08
          WHERE  ZFCDTY   EQ   '012'.
      MOVE : ZTIMIMG08-ZFCD   TO   IT_BLSDP_HELP-ZFBLSDP,
             ZTIMIMG08-ZFCDNM TO   IT_BLSDP_HELP-ZFCDNM.
      APPEND IT_BLSDP_HELP.
   ENDSELECT.

   IF SY-SUBRC NE 0.
      MESSAGE S406.
      EXIT.
   ENDIF.

   DYNPRO  = SY-REPID.
   DYNNR   = SY-DYNNR.

   WINDOW_TITLE = 'B/L 송부처'.
   CONCATENATE WINDOW_TITLE '코드 HELP' INTO WINDOW_TITLE
               SEPARATED BY SPACE.

*   IF W_STATUS EQ C_REQ_D.
*      L_DISPLAY = 'X'.
*   ELSE.
*      CLEAR: L_DISPLAY.
*   ENDIF.

   CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
                RETFIELD        = 'ZFBLSDP'
                DYNPPROG        = DYNPRO
                DYNPNR          = DYNNR
                DYNPROFIELD     = P_FIELDNAME
                WINDOW_TITLE    = WINDOW_TITLE
                VALUE_ORG       = 'S'
*                DISPLAY         = L_DISPLAY
        TABLES
                VALUE_TAB       = IT_BLSDP_HELP
        EXCEPTIONS
                PARAMETER_ERROR = 1
                NO_VALUES_FOUND = 2
                OTHERS          = 3.

   IF SY-SUBRC <> 0.
      EXIT.
   ENDIF.


ENDFORM.                    " P1000_BLSDP_HELP
*&---------------------------------------------------------------------*
*&      Form  P3000_SUB_TOTOL_WRITE1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_SUB_TOTOL_WRITE1.

  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCOTM' IT_TAB-ZFINRC
                                      CHANGING   W_DOM_TEX1.
  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE : / SY-ULINE.
  SUM.
  WRITE : / SY-VLINE NO-GAP,(14) W_DOM_TEX1 NO-GAP,     " P/O NO
                            (06) S_COUNT,(05)'건', ''  NO-GAP,
*           SY-VLINE NO-GAP,(25) IT_TAB-ZFRGDSR NO-GAP,
            SY-VLINE NO-GAP,(19) W_ZFCUAMTS CURRENCY 'KRW'
                                                 NO-GAP,
            SY-VLINE NO-GAP,(19) W_ZFVAAMTS CURRENCY 'KRW'
                                                 NO-GAP,
            SY-VLINE NO-GAP,(19) W_ZFTOT CURRENCY 'KRW'
                                                 NO-GAP,
            SY-VLINE NO-GAP,
            (15) '사업자등록번호:'   NO-GAP,
            (13)  W_STCD2 NO-GAP,(08) '',
            SY-VLINE.
  WRITE:/ SY-ULINE.

  CLEAR: S_COUNT,W_STCD2, W_ZFCUAMTS, W_ZFVAAMTS, W_ZFTOT.
  CLEAR : W_ZFVAAMTS,  W_ZFCUAMTS,  W_ZFTOT.

ENDFORM.                    " P3000_SUB_TOTOL_WRITE1
