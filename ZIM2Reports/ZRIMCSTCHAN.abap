*&---------------------------------------------------------------------
*& Report  ZRIMCSTCHAN
*&---------------------------------------------------------------------
*&  프로그램명 : 수입비용분석(무환)
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2002.01.08
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&
*&---------------------------------------------------------------------
REPORT  ZRIMCSTCHAN  MESSAGE-ID ZIM
                     LINE-SIZE 130
                     NO STANDARD PAGE HEADING.

TABLES: ZTBKPF,
        ZTBSEG,
        ZTBL,
        ZTIDS,
        ZTIMIMG10,
        LFA1,
        ZTIMIMG08,
        ZTIV,
        ZTCUCLIV,
        T001,
        ZTIMIMG02.

*----------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*----------------------------------------------------------------------

DATA : BEGIN OF IT_TAB OCCURS 0,
       BUKRS     LIKE    ZTBKPF-BUKRS,
       BELNR     LIKE    ZTBKPF-BELNR,
       GJAHR     LIKE    ZTBKPF-GJAHR,
       ZFIMDNO   LIKE    ZTBSEG-ZFIMDNO,     " 수입문서번호.
       ZFACDO    LIKE    ZTBKPF-ZFACDO,      " 회계전표번호.
       ZFRVSX    LIKE    ZTBKPF-ZFRVSX,      " 역기표 여부.
       ZFBLNO    LIKE    ZTBL-ZFBLNO,
       ZFCLSEQ   LIKE    ZTCUCLIV-ZFCLSEQ,
       ZFHBLNO   LIKE    ZTBL-ZFHBLNO,       " B/L NO
       KOSTL     LIKE    ZTBL-KOSTL,         " COSTCENTER
       ZFPOTY    LIKE    ZTBL-ZFPOTY,        "  무환종류.
       POTY      LIKE    DD07T-DDTEXT,       "
       PS_POSID  LIKE    ZTBL-PS_POSID,      " WBS ID.
       ZFCD      LIKE    ZTBSEG-ZFCD,        " 관리코드.
       ZFCDNM    LIKE    ZTIMIMG08-ZFCDNM,   " 비용명.
       LIFNR     LIKE    ZTBKPF-LIFNR,       " 구매처.
       NAME1     LIKE    LFA1-NAME1,         " 거래선.
       BUDAT     LIKE    ZTBKPF-BUDAT,       " 전기일.
       ZFBDT     LIKE    ZTBKPF-ZFBDT,       " 지불일.
       ZFCSTGRP  LIKE    ZTBSEG-ZFCSTGRP,    " 비용그룹.
       CSTGRP    LIKE    DD07T-DDTEXT,
       WRBTR     LIKE    ZTBSEG-WRBTR,       " 비용금액.
       WMWST     LIKE    ZTBSEG-WMWST,       " 전표통화세액.
       WAERS     LIKE    ZTBKPF-WAERS,       " 통화.
       DMBTR     LIKE    ZTBSEG-DMBTR,       " 현지통화금액.
       FWBAS     LIKE    ZTBSEG-FWBAS,       " 현지통화금액.
       SHKZG     LIKE    ZTBSEG-SHKZG,       " 대/차변.
       ZFIDRNO   LIKE    ZTIDS-ZFIDRNO.
DATA : END OF IT_TAB.
DATA : BEGIN OF IT_SUB OCCURS 0,
       ZFHBLNO   LIKE    ZTBL-ZFHBLNO,       " B/L NO
*      WRBTR     LIKE    ZTBSEG-WRBTR,       " 비용금액.
       DMBTR     LIKE    ZTBSEG-DMBTR,       " 현지통화금액.
       WMWST     LIKE    ZTBSEG-WMWST,       " 전표통화세액.
       WAERS     LIKE    ZTBKPF-WAERS.       " 통화.
DATA : END OF IT_SUB.

DATA : BEGIN OF IT_TOTAL OCCURS 0,
*      WRBTR     LIKE    ZTBSEG-WRBTR,       " 비용금액.
       DMBTR     LIKE    ZTBSEG-DMBTR,       " 현지통화금액.
       WMWST     LIKE    ZTBSEG-WMWST,       " 전표통화세액.
       WAERS     LIKE    ZTBKPF-WAERS.       " 통화.
DATA : END OF IT_TOTAL.


DATA :  W_ERR_CHK     TYPE C,
        W_LCOUNT      TYPE I,
        W_FIELD_NM    TYPE C,
        W_PAGE        TYPE I,
        W_CHECK_PAGE(1) TYPE C,
        W_LINE        TYPE I,
        W_COUNT       TYPE I,
        W_TABIX       LIKE SY-TABIX.

DATA  W_CHECK(1).
DATA  W_TITLE(06).
DATA: TEMP_HBLNO LIKE IT_TAB-ZFHBLNO.
DATA  W_MOD.

DATA  W_PS_POSID  LIKE ZTBL-PS_POSID.
DATA  W_KOSTL     LIKE ZTBL-KOSTL.
*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

   SELECT-OPTIONS: S_BUKRS   FOR  ZTBKPF-BUKRS NO-EXTENSION
                                               NO INTERVALS,
                   S_HBLNO   FOR  ZTBL-ZFHBLNO,
                   S_BLNO    FOR  ZTBL-ZFBLNO,
                   S_ETA     FOR  ZTBL-ZFETA,
                   S_BLDT    FOR  ZTBL-ZFBLDT,
                   S_FNR     FOR  ZTBL-LIFNR,
                   S_GRP     FOR  ZTBL-EKGRP,
                   S_ORG     FOR  ZTBL-EKORG,
                   S_WERKS   FOR  ZTBL-ZFWERKS,
                   S_KOSTL   FOR  ZTBL-KOSTL,
                   S_WBS     FOR  ZTBL-PS_POSID,
                   S_POTY    FOR  ZTBL-ZFPOTY,    " 무환구분.
                   S_RPTTY   FOR  ZTBL-ZFRPTTY,   " 수입신고형태.
                   S_MATGB   FOR  ZTBL-ZFMATGB,   " 자재구분.
                   S_IMTRD   FOR  ZTBL-IMTRD,     " 수입자구분.
                   S_POSYN   FOR  ZTBKPF-ZFPOSYN NO-EXTENSION
                                                 NO INTERVALS.
 SELECTION-SCREEN END OF BLOCK B1.
 INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.

*title Text Write
W_CHECK_PAGE = 'X'.
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
     WHEN 'DISP1' OR 'DISP3'. " 비용문서.
       IF W_TABIX IS INITIAL.
          MESSAGE S962.
       ELSE.
          IF NOT IT_TAB-BELNR IS INITIAL.
              PERFORM P2000_DISPLAY_COST_DOCUMENT USING  IT_TAB-BUKRS
                                                         IT_TAB-GJAHR
                                                         IT_TAB-BELNR
                                                         IT_TAB-ZFACDO.
          ELSE.
             MESSAGE S962.
          ENDIF.
       ENDIF.
     WHEN 'DISP2'.
       IF W_TABIX IS INITIAL.
          MESSAGE S962.
          EXIT.
       ENDIF.
       CASE IT_TAB-ZFCSTGRP.
           WHEN '004' OR '005'.
              PERFORM P2000_DISPLY_BL USING IT_TAB-ZFBLNO.
           WHEN '006'.
              PERFORM P2000_DISP_IDS USING IT_TAB-ZFBLNO IT_TAB-ZFCLSEQ.
           WHEN OTHERS.
       ENDCASE.

   ENDCASE.
   CLEAR : W_TABIX, IT_TAB.

*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

  SKIP 2.
  WRITE:/60 '[무환 비용분석]'  COLOR COL_HEADING INTENSIFIED OFF.
  SKIP 2.
  WRITE:/ 'Date:',SY-DATUM.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/ SY-ULINE.
  WRITE:/ SY-VLINE,(15)'B/L No',
          SY-VLINE,(14) '무환종류',
          SY-VLINE,(15) '조건범주',
          SY-VLINE,(14) '비용종류',
          SY-VLINE,(10) '문서번호',
          SY-VLINE,(20) '현지통화금액',
          SY-VLINE,(20) '부가세',
          SY-VLINE.
  WRITE:/ SY-ULINE.

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
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
            FROM ZTBKPF AS H INNER JOIN ZTBSEG AS I
             ON H~BUKRS = I~BUKRS
            AND H~BELNR = I~BELNR
            AND H~GJAHR = I~GJAHR
         WHERE  H~ZFPOSYN  IN S_POSYN              " 전기여부.
           AND  H~BUKRS    IN  S_BUKRS
           AND  I~ZFPOYN   = 'N'
           AND  I~ZFCSTGRP IN ('006','005','004').
  IF SY-SUBRC NE 0.  W_ERR_CHK = 'Y'. EXIT.  ENDIF.

  SORT IT_TAB BY ZFIMDNO.

  LOOP AT IT_TAB.
     W_TABIX = SY-TABIX.
*>> 통관관련비용일 경우.
   IF IT_TAB-ZFCSTGRP EQ '006'.
      CLEAR ZTIV.
      SELECT  SINGLE *
          FROM  ZTIV
          WHERE ZFIVNO = IT_TAB-ZFIMDNO.
      CLEAR ZTCUCLIV.
      SELECT SINGLE *
        FROM ZTCUCLIV
       WHERE ZFIVNO = ZTIV-ZFIVNO.
*>> 통관 관련비용.
      IF SY-SUBRC  EQ 0.
         CLEAR IT_TAB-ZFIMDNO.
         MOVE  ZTCUCLIV-ZFBLNO TO IT_TAB-ZFIMDNO.
      ENDIF.
    ENDIF.
*>> B/L
    CLEAR ZTBL.
    SELECT SINGLE *
       FROM ZTBL
      WHERE ZFBLNO = IT_TAB-ZFIMDNO
        AND ZFBLNO   IN S_BLNO
        AND ZFHBLNO  IN S_HBLNO
        AND ZFETA    IN S_ETA
        AND ZFPOYN   = 'N'
        AND ZFBLDT   IN S_BLDT
        AND LIFNR    IN S_FNR
        AND EKGRP    IN S_GRP
        AND EKORG    IN S_ORG
        AND ZFWERKS  IN S_WERKS
        AND KOSTL    IN S_KOSTL
        AND PS_POSID IN S_WBS
        AND ZFPOTY   IN S_POTY
        AND ZFRPTTY  IN S_RPTTY
        AND IMTRD    IN S_IMTRD
        AND ZFMATGB  IN S_MATGB.
    IF SY-SUBRC NE 0.
       DELETE IT_TAB INDEX W_TABIX.
       CONTINUE.
    ENDIF.
*>> B/L DATA MOVE
    MOVE: ZTBL-ZFHBLNO  TO IT_TAB-ZFHBLNO,
          ZTCUCLIV-ZFCLSEQ TO IT_TAB-ZFCLSEQ,
          ZTBL-ZFBLNO   TO IT_TAB-ZFBLNO,
          ZTBL-KOSTL    TO IT_TAB-KOSTL,
          ZTBL-PS_POSID TO IT_TAB-PS_POSID,
          ZTBL-ZFPOTY   TO IT_TAB-ZFPOTY.
*>> 무환종류.
    PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZEPOTY'
                                        IT_TAB-ZFPOTY
                                        CHANGING IT_TAB-POTY.

    PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCSTGRP'
                                        IT_TAB-ZFCSTGRP
                                        CHANGING  IT_TAB-CSTGRP.
    CLEAR  ZTIMIMG08.
    SELECT SINGLE *
         FROM ZTIMIMG08
        WHERE ZFCDTY = IT_TAB-ZFCSTGRP
          AND ZFCD     = IT_TAB-ZFCD.
    MOVE ZTIMIMG08-ZFCDNM TO IT_TAB-ZFCDNM.

*> 역기표 여부.
    IF IT_TAB-ZFRVSX EQ 'X'.
       IT_TAB-WRBTR =        IT_TAB-WRBTR * -1.
       IT_TAB-WMWST =        IT_TAB-WMWST * -1.
       IT_TAB-DMBTR =        IT_TAB-DMBTR * -1.
       IT_TAB-FWBAS =        IT_TAB-FWBAS * -1.
    ENDIF.

    MODIFY IT_TAB INDEX W_TABIX.
    MOVE-CORRESPONDING IT_TAB TO IT_SUB.
    MOVE-CORRESPONDING IT_TAB TO IT_TOTAL.
    COLLECT IT_SUB.
    COLLECT IT_TOTAL.
  ENDLOOP.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE .

   SET TITLEBAR  'ZIMY6'.
   SET PF-STATUS 'ZIMY6'.
   CLEAR TEMP_HBLNO.
   SORT IT_TAB BY  ZFHBLNO.
   LOOP AT IT_TAB.
        W_TABIX = SY-TABIX.
         W_MOD = SY-TABIX MOD 2.
        IF W_TABIX NE 1.
        IF TEMP_HBLNO NE IT_TAB-ZFHBLNO.
           LOOP AT IT_SUB WHERE ZFHBLNO = TEMP_HBLNO.
                PERFORM P3000_SUB_TOTAL_WRITE.
           ENDLOOP.
        ENDIF.
        ENDIF.
        TEMP_HBLNO = IT_TAB-ZFHBLNO.
        PERFORM   P3000_LINE_WRITE.
        AT LAST.
           LOOP AT IT_SUB WHERE ZFHBLNO = TEMP_HBLNO.
             PERFORM P3000_SUB_TOTAL_WRITE.
           ENDLOOP.
           LOOP AT IT_TOTAL.
             PERFORM P3000_LAST_WRITE.
           ENDLOOP.
        ENDAT.
   ENDLOOP.
   WRITE:/ SY-ULINE.
   CLEAR: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

  SET TITLEBAR  'ZIMY6'.
  MOVE 'Y' TO S_POSYN-LOW.
  APPEND S_POSYN.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------
*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------
*
FORM P3000_LINE_WRITE.

  DATA: ZFHBLNO  LIKE  IT_TAB-ZFHBLNO,
        PS_POSID LIKE  IT_TAB-PS_POSID, " WBS No
        KOSTL    LIKE  IT_TAB-KOSTL,    " Cost Center',
        POTY     LIKE  IT_TAB-POTY.     " 무환종류',


  IF W_MOD EQ 1.
     FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  ON CHANGE OF IT_TAB-ZFHBLNO.
     ZFHBLNO =  IT_TAB-ZFHBLNO.
     PS_POSID =  IT_TAB-PS_POSID. " WBS No
     KOSTL    =  IT_TAB-KOSTL.    " Cost Center',
     POTY     =  IT_TAB-POTY.     " 무환종류',
  ENDON.

  WRITE:/ SY-VLINE,(15)ZFHBLNO,
*          SY-VLINE,(24)PS_POSID, " WBS No
*          SY-VLINE,(11)KOSTL,    " Cost Center',
          SY-VLINE,(14)POTY,     " 무환종류',
          SY-VLINE,(15)IT_TAB-CSTGRP,
          SY-VLINE,(14)IT_TAB-ZFCDNM,  " 비용종류',
          SY-VLINE,(10)IT_TAB-ZFACDO,  " 문서번호',
          SY-VLINE,(16)IT_TAB-DMBTR CURRENCY 'KRW',
                   (03)'KRW',
          SY-VLINE,(16)IT_TAB-WMWST CURRENCY IT_TAB-WAERS,
                   (03)IT_TAB-WAERS,
          SY-VLINE.
  HIDE: IT_TAB, W_TABIX.
  W_PS_POSID =  IT_TAB-PS_POSID. " WBS No
  W_KOSTL    =  IT_TAB-KOSTL.    " Cost Center',

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------
*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------
*
FORM P3000_LAST_WRITE.

  IF W_CHECK NE 'Y'.
     W_TITLE = 'TOTAL:'.
  ELSE.
    CLEAR W_TITLE.
  ENDIF.
  WRITE:/ SY-VLINE,(15) W_TITLE,SY-VLINE,
          (55)'',
          (25)IT_TOTAL-DMBTR CURRENCY 'KRW',
          (03)'KRW',SY-VLINE,
          (16)IT_TOTAL-WMWST CURRENCY IT_TOTAL-WAERS,
          (03)IT_TOTAL-WAERS,SY-VLINE.
  W_CHECK = 'Y'.

ENDFORM.
*&----------------------------------------------------------------
*& FORM P2000_DISPLAY_COST_DOCUMENT
*&----------------------------------------------------------------
FORM P2000_DISPLAY_COST_DOCUMENT USING    P_BUKRS
                                          P_GJAHR
                                          P_BELNR
                                          P_ZFACDO.

 SET  PARAMETER ID  'BUK'       FIELD   P_BUKRS.
 SET  PARAMETER ID  'GJR'       FIELD   P_GJAHR.
 SET  PARAMETER ID  'ZPBENR'    FIELD   P_BELNR.
 IF SY-UCOMM EQ 'DISP1'.
   CALL TRANSACTION 'ZIMY3' AND SKIP  FIRST SCREEN.
 ELSE.
   SET  PARAMETER ID  'BLN'    FIELD    P_ZFACDO.
   CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
 ENDIF.

ENDFORM.                    " P2000_DISPLAY_COST_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  P3000_SUB_TOTAL_WRITE
*&---------------------------------------------------------------------*
FORM P3000_SUB_TOTAL_WRITE.

  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE,(15) 'SUB_TOTAL:',SY-VLINE,
*          SY-VLINE,(24)'',     " WBS No
*          SY-VLINE,(11)'',    " Cost Center',
*          SY-VLINE,(14)'',     " 무환종류',
*          SY-VLINE,(14)'',
*          SY-VLINE,(14)'',  " 비용종류',
*          SY-VLINE,(10)'',  " 문서번호',
           'WBS No:', W_PS_POSID,
           SY-VLINE, 'Cost Center:',W_KOSTL,
*          (55)'',
           (22)IT_SUB-DMBTR CURRENCY 'KRW',
          (03)'KRW',SY-VLINE,
          (16)IT_SUB-WMWST CURRENCY IT_SUB-WAERS,
          (03)IT_SUB-WAERS,SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_SUB_TOTAL_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_DISPLY_BL
*&---------------------------------------------------------------------*
FORM P2000_DISPLY_BL USING    P_ZFBLNO.

  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD  P_ZFBLNO.
  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISPLY_BL

*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_IDS
*&---------------------------------------------------------------------*
FORM P2000_DISP_IDS USING    P_ZFBLNO
                             P_ZFCLSEQ.

  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD  P_ZFBLNO.
  SET PARAMETER ID 'ZPCLSEQ' FIELD  P_ZFCLSEQ.
  SET PARAMETER ID 'ZPIDRNO' FIELD ''.
  CALL TRANSACTION 'ZIM76' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_IDS
