*&---------------------------------------------------------------------*
*& Report  ZRIMCLSCN                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입통관관리현황                                      *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.10.06                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : Invoice 자료를 조회하여 수입신고자료(입항지통관)?
*&               생성한다.
*&---------------------------------------------------------------------*
*& [변경내용]
*&             : 입항지통관 과 수입통관을 함께 처리하도록 수정.
*&             : 신고희망일.
*&---------------------------------------------------------------------*
REPORT  ZRIMCLSC    MESSAGE-ID ZIM
                    LINE-SIZE 134
                    NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMCLSCTOP.
INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_IDWDT  FOR ZTIDR-ZFIDWDT      ">신고희망일.
                                OBLIGATORY
                                DEFAULT SY-DATUM,
                   S_ZFCUT  FOR ZTIDR-ZFCUT,       " 관세사.
                   S_HBLNO  FOR ZTBL-ZFHBLNO,      " House B/L No
                   S_MBLNO  FOR ZTBL-ZFMBLNO,      " Master B/L No
                   S_BLNO   FOR ZTIV-ZFBLNO,       " B/L 관리번?
                   S_IVNO   FOR ZTIV-ZFIVNO,       " Invoice  관리번?
                   S_ETA    FOR ZTBL-ZFETA.        " 입항일(ETA)

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
* 유환여?
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(14) TEXT-003, POSITION 1.
     SELECTION-SCREEN : COMMENT 36(4) TEXT-031, POSITION 43.
     PARAMETERS : P_POY    AS CHECKBOX.              " 유?
     SELECTION-SCREEN : COMMENT 46(4) TEXT-032, POSITION 55.
     PARAMETERS : P_PON    AS CHECKBOX.              " 무?
     SELECTION-SCREEN : COMMENT 60(7) TEXT-033, POSITION 68.
     PARAMETERS : P_POM    AS CHECKBOX.              " 무?
  SELECTION-SCREEN END OF LINE.
* 통관상?
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(19) TEXT-004, POSITION 1.
     SELECTION-SCREEN : COMMENT 32(8) TEXT-041, POSITION 43.
     PARAMETERS : P_CU1    AS CHECKBOX.              " 생성대상.
     SELECTION-SCREEN : COMMENT 46(8) TEXT-042, POSITION 55.
     PARAMETERS : P_CU2    AS CHECKBOX.              " 의뢰대상.
     SELECTION-SCREEN : COMMENT 60(6) TEXT-043, POSITION 67.
     PARAMETERS : P_CU3    AS CHECKBOX.              " 의뢰중.
     SELECTION-SCREEN : COMMENT 70(8) TEXT-044, POSITION 79.
     PARAMETERS : P_CUY    AS CHECKBOX.              " 통관완료.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B2.

INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.
   SET  TITLEBAR 'ZIMR60'.           " GUI TITLE SETTING..
   SELECT  SINGLE *  FROM  ZTIMIMG00.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 레포트 관련 TEXT TABLE SELECT
  CLEAR  W_GUBN.
* IF ZTIMIMG00-ZFIMPATH NE '1'.
*    MESSAGE  S564.  EXIT.
* ENDIF.
  PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE      USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.


*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
     WHEN 'STUP' OR 'STDN'.         " SORT 선택?
           W_FIELD_NM = 'ZFIVNO'.
           ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
           PERFORM HANDLE_SORT TABLES  IT_TAB
                               USING   SY-UCOMM.
     WHEN 'DISP'.                    " Invoice Application 조?
           IF W_TABIX EQ 0.
              MESSAGE S962. EXIT.
           ENDIF.
           PERFORM P2000_SHOW_IV USING IT_TAB-ZFIVNO.
    WHEN 'DSBL'.                    " B/L 조?
          IF W_TABIX EQ 0.
              MESSAGE S962. EXIT.
          ENDIF.
          PERFORM P2000_SHOW_BL USING IT_TAB-ZFBLNO.
     WHEN 'DISP2'.
          PERFORM P2000_SHOW_IDR USING IT_TAB-ZFBLNO IT_TAB-ZFCLSEQ.

     WHEN 'DOWN'.          " FILE DOWNLOAD....
            PERFORM P3000_TO_PC_DOWNLOAD.
     WHEN 'REFR'.
          PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
          IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
          PERFORM RESET_LIST.
     WHEN 'DSCS'.
          IF W_TABIX EQ 0.
              MESSAGE S962. EXIT.
          ENDIF.

          PERFORM P2000_SHOW_IDR USING IT_TAB-ZFBLNO IT_TAB-ZFCLSEQ.
     WHEN OTHERS.
     CLEAR: IT_TAB,W_TABIX.
   ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_INIT
*&---------------------------------------------------------------------*
FORM P2000_INIT.

  P_POY = 'X'.
  P_CU1 = 'X'.
  P_CU2 = 'X'.
  P_CU3 = 'X'.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /60  '[ 수입통관 관리 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / ' Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE NO-GAP,(12)'통관번호'     NO-GAP,
            SY-VLINE NO-GAP,(25)'House B/L NO' NO-GAP,
            SY-VLINE NO-GAP,(20)'관세사'       NO-GAP,
            SY-VLINE NO-GAP,(24)'통관금액'     NO-GAP,
            SY-VLINE NO-GAP,(06)'유무환'   NO-GAP,
            SY-VLINE NO-GAP,(21)'수입자'       NO-GAP,
            SY-VLINE NO-GAP,(18)'선임'         NO-GAP,
            SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE NO-GAP,(12)'대표P/O번호'  NO-GAP,
            SY-VLINE NO-GAP,(25)'Vendor '      NO-GAP,
            SY-VLINE NO-GAP,(20)'세관'         NO-GAP,
            SY-VLINE NO-GAP,(24)'징수형태'     NO-GAP,
            SY-VLINE NO-GAP,(06)'Inco'    NO-GAP,
            SY-VLINE NO-GAP,(21)'수입신고형태' NO-GAP,
            SY-VLINE NO-GAP,(18)'보험료'       NO-GAP,
            SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING     W_ERR_CHK.

   MOVE 'N' TO W_ERR_CHK.

   SET PF-STATUS 'ZIMR60'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMR60'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      PERFORM P3000_LINE_WRITE.
      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.

   ENDLOOP.
   CLEAR: W_TABIX, IT_TAB.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
  DATA: W_NEWPO(15).
  CLEAR W_NEWPO.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE : / SY-VLINE NO-GAP,(12) IT_TAB-ZFIVNO NO-GAP,
            SY-VLINE NO-GAP,(25) IT_TAB-ZFHBLNO NO-GAP,
            SY-VLINE NO-GAP,(20) IT_TAB-NAME1 NO-GAP,   " 관세사.
            SY-VLINE NO-GAP,(03) IT_TAB-ZFIVAMC,
            (20) IT_TAB-ZFIVAMT
                        CURRENCY IT_TAB-ZFIVAMC NO-GAP, " 통관금액'
            SY-VLINE NO-GAP,(06) IT_TAB-POYN NO-GAP,    " 유무환구분'
            SY-VLINE NO-GAP,(21) IT_TAB-BUTXT  NO-GAP,  " 수입자.
            SY-VLINE NO-GAP,(03) IT_TAB-ZFTFAC,
                            (14) IT_TAB-ZFTFA CURRENCY
                                 IT_TAB-ZFTFAC NO-GAP,    " 선임.
            SY-VLINE NO-GAP.

  HIDE: W_TABIX, IT_TAB.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  CONCATENATE IT_TAB-ZFREBELN '-' IT_TAB-ZFSHNO INTO W_NEWPO.
  WRITE : / SY-VLINE NO-GAP,(12) W_NEWPO NO-GAP, " 대표P/O번호'
            SY-VLINE NO-GAP,(25) IT_TAB-NAME2 NO-GAP,    " Vendor '
            SY-VLINE NO-GAP,(20) IT_TAB-INRC NO-GAP,     " 세관'
            SY-VLINE NO-GAP,(24) IT_TAB-COCD NO-GAP,     " 징수형태.
            SY-VLINE NO-GAP,(06) IT_TAB-INCO1 NO-GAP,    " INCOTERMS.
            SY-VLINE NO-GAP,(21) IT_TAB-RPTTY NO-GAP,    " 수입신고형태'
            SY-VLINE NO-GAP,(03) 'KRW',
                            (14) IT_TAB-ZFINAMT
                                    CURRENCY 'KRW' NO-GAP,   " G/R 상태'
            SY-VLINE NO-GAP.
  HIDE: W_TABIX,IT_TAB.

  W_COUNT = W_COUNT + 1.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : / ' 총', W_COUNT, '건'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  W_GUBN  = 'X'.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  REFRESH IT_TAB.
  CLEAR   IT_TAB.
  IF P_POY = ' ' AND P_PON = ' ' AND P_POM = ' '.
     MESSAGE S738. EXIT.
  ENDIF.
  IF P_CU1 = ' ' AND P_CU2 = ' ' AND P_CU3 = ' ' AND P_CUY = ' '.
     MESSAGE S738. EXIT.
  ENDIF.

* 유환, 무환 여부 SETTING!
  REFRESH : R_TERM1.
  IF P_POY = 'X'.
    MOVE : 'I'       TO       R_TERM1-SIGN,
           'EQ'      TO       R_TERM1-OPTION,
           'Y'       TO       R_TERM1-LOW,
           SPACE     TO       R_TERM1-HIGH.
    APPEND    R_TERM1.
  ENDIF.
  IF P_PON = 'X'.
    MOVE : 'I'       TO       R_TERM1-SIGN,
           'EQ'      TO       R_TERM1-OPTION,
           'N'       TO       R_TERM1-LOW,
           SPACE     TO       R_TERM1-HIGH.
    APPEND    R_TERM1.
  ENDIF.
  IF P_POM = 'X'.
    MOVE : 'I'       TO       R_TERM1-SIGN,
           'EQ'      TO       R_TERM1-OPTION,
           'M'       TO       R_TERM1-LOW,
           SPACE     TO       R_TERM1-HIGH.
    APPEND    R_TERM1.
  ENDIF.

* 통관 상태 SETTING!
  REFRESH R_TERM2.
  IF P_CU1 = 'X'.
    MOVE : 'I'       TO       R_TERM2-SIGN,
           'EQ'      TO       R_TERM2-OPTION,
           '1'       TO       R_TERM2-LOW,
           SPACE     TO       R_TERM2-HIGH.
    APPEND    R_TERM2.
  ENDIF.
  IF P_CU2 = 'X'.
    MOVE : 'I'       TO       R_TERM2-SIGN,
           'EQ'      TO       R_TERM2-OPTION,
           '2'       TO       R_TERM2-LOW,
           SPACE     TO       R_TERM2-HIGH.
    APPEND    R_TERM2.
  ENDIF.
  IF P_CU3 = 'X'.
    MOVE : 'I'       TO       R_TERM2-SIGN,
           'EQ'      TO       R_TERM2-OPTION,
           '3'       TO       R_TERM2-LOW,
           SPACE     TO       R_TERM2-HIGH.
    APPEND    R_TERM2.
  ENDIF.
  IF P_CUY = 'X'.
    MOVE : 'I'       TO       R_TERM2-SIGN,
           'EQ'      TO       R_TERM2-OPTION,
           'Y'       TO       R_TERM2-LOW,
           SPACE     TO       R_TERM2-HIGH.
    APPEND    R_TERM2.
  ENDIF.

*>> 조건에 해당하는 DATA SELECT.
  SELECT  H~ZFIVNO  H~ZFBLNO  I~ZFHBLNO  H~ZFIVAMT
          H~ZFIVAMC H~ZFIVAMK H~ZFEXRT   H~ZFPOYN
          H~ZFPONC  H~LIFNR   H~ZFPHVN   H~ZFCLCD   I~ZFBLNO
          H~ZFCUST  H~ZFGRST  H~ZFCIVST  I~ZFREBELN I~ZFRPTTY
          I~BUKRS   I~IMTRD   I~ZFSHNO
  INTO    CORRESPONDING FIELDS OF TABLE IT_TAB
  FROM    ZTIV AS H INNER JOIN ZTBL AS I
  ON      H~ZFBLNO  EQ    I~ZFBLNO
  WHERE   H~ZFIVNO  IN    S_IVNO
  AND     H~ZFBLNO  IN    S_BLNO
  AND     I~ZFHBLNO IN    S_HBLNO
  AND     I~ZFMBLNO IN    S_MBLNO
  AND     I~ZFETA   IN    S_ETA
  AND     H~ZFPOYN  IN    R_TERM1
  AND     H~ZFCUST  IN    R_TERM2.
  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT = 0.
     W_ERR_CHK = 'Y'.
     IF W_GUBN IS INITIAL. MESSAGE S738. ENDIF.
  ENDIF.

  LOOP AT IT_TAB.
     W_TABIX = SY-TABIX.
*    CLEAR ZTCUCLIV.
*    SELECT SINGLE *
*       FROM ZTCUCLIV
*      WHERE ZFIVNO = IT_TAB-ZFIVNO.

     CLEAR ZTIDR.
     SELECT SINGLE *
        FROM ZTIDR
       WHERE ZFBLNO  = IT_TAB-ZFBLNO
         AND ZFIVNO = IT_TAB-ZFIVNO
*        AND ZFCLSEQ = ZTCUCLIV-ZFCLSEQ
         AND ZFIDWDT IN S_IDWDT     ">신고희망일.
         AND ZFCUT   IN S_ZFCUT.
     IF SY-SUBRC EQ 0.
        IT_TAB-ZFINRC  =  ZTIDR-ZFINRC.
        IT_TAB-ZFCLSEQ =  ZTIDR-ZFCLSEQ.
        IT_TAB-ZFINRCD =  ZTIDR-ZFINRCD.
        IT_TAB-ZFITKD  =  ZTIDR-ZFITKD.
        IT_TAB-ZFCUT   =  ZTIDR-ZFCUT.
        IT_TAB-ZFCOCD  =  ZTIDR-ZFCOCD.        " 징수형태.
        IT_TAB-INCO1   =  ZTIDR-INCO1.         " INCOTERMS.
        IT_TAB-ZFINAMT =  ZTIDR-ZFINAMT.       " 보험료.
        IT_TAB-ZFTFA   =  ZTIDR-ZFTFA.         " 선운임.
        IT_TAB-ZFTFAC  =  ZTIDR-ZFTFAC.        " 선운임.

     ELSE.
        DELETE IT_TAB INDEX W_TABIX.
        CONTINUE.
     ENDIF.
*>> 수입자.
      CALL FUNCTION 'ZIM_GET_COMPANY_DATA'
       EXPORTING
          BUKRS       =    IT_TAB-BUKRS
          IMTRD       =    IT_TAB-IMTRD
       IMPORTING
          XT001           =    T001.
     IT_TAB-BUTXT   = T001-BUTXT.
*>> 세관.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCOTM' IT_TAB-ZFINRC
                               CHANGING   IT_TAB-INRC.
*>> 담당과부호.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDINRCO'
                                          IT_TAB-ZFINRCD
                                        CHANGING IT_TAB-INRCD.
*>> 수입신고형태.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDRPTTY'
                                          IT_TAB-ZFRPTTY
                                        CHANGING IT_TAB-RPTTY.
*>> 수입신고종류.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDITKD'
                                          IT_TAB-ZFITKD
                                        CHANGING IT_TAB-ITKD.
*>> 관세사.
     CLEAR ZTIMIMG10.
     SELECT SINGLE *
      FROM ZTIMIMG10
     WHERE ZFCUT = IT_TAB-ZFCUT.

     CLEAR LFA1.
     SELECT SINGLE *
       FROM LFA1
      WHERE LIFNR = ZTIMIMG10-ZFVEN.
     IF SY-SUBRC EQ 0.
         MOVE  LFA1-NAME1 TO IT_TAB-NAME1.
     ENDIF.
*>>  징수형태.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCOCD'
                                          IT_TAB-ZFCOCD
                                        CHANGING IT_TAB-COCD.

*>> 수입거래구분.
     PERFORM  GET_ZTIIMIMG08_SELECT USING '001' IT_TAB-ZFPONC
                                  CHANGING   IT_TAB-PONC.
*>> 보세구역.
     CLEAR ZTIMIMG03.
     SELECT SINGLE *
       FROM ZTIMIMG03
      WHERE  ZFBNARCD = IT_TAB-ZFBNARCD.
     IF SY-SUBRC EQ 0.
         IT_TAB-ZFBNARM = ZTIMIMG03-ZFBNARM.
     ENDIF.
*>>Vendor 명 Select
     CLEAR LFA1.
     SELECT SINGLE NAME1 INTO IT_TAB-NAME2
              FROM LFA1
             WHERE LIFNR = IT_TAB-LIFNR .
*>> 유환, 무환 구분.
     IF IT_TAB-ZFPOYN = 'Y'.
        IT_TAB-POYN = '유환'.
     ELSEIF IT_TAB-ZFPOYN = 'M'.
        IT_TAB-POYN = 'Combine'.
     ELSEIF IT_TAB-ZFPOYN = 'N'.
        IT_TAB-POYN = '무환'.
     ENDIF.
*>> 통관상태.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCUST'
                                          IT_TAB-ZFCUST
                                        CHANGING IT_TAB-CUST.
*>> 입고상태.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDGRST'
                                          IT_TAB-ZFGRST
                                        CHANGING IT_TAB-GRST.
     MODIFY IT_TAB INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_IV
*&---------------------------------------------------------------------*
FORM P2000_SHOW_IV USING  P_ZFIVNO.

   SET PARAMETER ID 'ZPIVNO'   FIELD P_ZFIVNO.
   SET PARAMETER ID 'ZPBLNO'   FIELD ''.
   SET PARAMETER ID 'ZPHBLNO'  FIELD ''.

   CALL TRANSACTION 'ZIM33' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_IV
*&---------------------------------------------------------------------*
*&      Form  P2000_CREATE_CUCL
*&---------------------------------------------------------------------*
FORM P4000_CREATE_CUCL USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  REFRESH IT_CUCL.
  CLEAR   IT_CUCL.
  LOOP AT IT_SELECTED.
       IF IT_SELECTED-ZFCUST NE '1'.
          CONTINUE.
       ENDIF.

       CLEAR : ZTCUCLIV, ZTCUCL.
       SELECT SINGLE *
         FROM ZTCUCLIV
        WHERE ZFIVNO = IT_SELECTED-ZFIVNO.
       IF SY-SUBRC NE 0.
          CONTINUE.
       ENDIF.

       MOVE SY-UNAME TO ZTCUCLIV-UNAM.
       MOVE SY-DATUM TO ZTCUCLIV-UDAT.
       MOVE '2'      TO ZTCUCLIV-ZFCUST.
       UPDATE ZTCUCLIV.
       IF SY-SUBRC NE 0.
          MESSAGE E739 WITH ZTCUCLIV-ZFIVNO.
          MOVE 'Y' TO W_ERR_CHK.
          EXIT.
       ENDIF.

       SELECT SINGLE *
         FROM ZTCUCL
        WHERE ZFBLNO = ZTCUCLIV-ZFBLNO
          AND ZFCLSEQ = ZTCUCLIV-ZFCLSEQ.
       IF SY-SUBRC = 0.
                 " 신고금액등 add..
       ELSE.
          MOVE ZTCUCLIV-ZFBLNO  TO ZTCUCL-ZFBLNO.
          MOVE ZTCUCLIV-ZFCLSEQ TO ZTCUCL-ZFCLSEQ.
          MOVE 'USD'            TO ZTCUCL-ZFUSD.
          MOVE 'KRW'            TO ZTCUCL-ZFKRW.
          MOVE ZTCUCLIV-ZFCLCD TO ZTCUCL-ZFCLCD.   " 통관구분 = 수입통?
          MOVE '2'              TO ZTCUCL-ZFCUST.  " 통관상태 = 의뢰대?
          MOVE SY-UNAME         TO ZTCUCL-ERNAM.
          MOVE SY-DATUM         TO ZTCUCL-CDAT.
          MOVE SY-UNAME         TO ZTCUCL-UNAM.
          MOVE SY-DATUM         TO ZTCUCL-UDAT.
          INSERT ZTCUCL.
          IF SY-SUBRC NE 0.
             MESSAGE E739 WITH ZTCUCLIV-ZFIVNO.
             MOVE 'Y' TO W_ERR_CHK.
             EXIT.
          ENDIF.
          MOVE ZTCUCLIV-ZFBLNO  TO IT_CUCL-ZFBLNO.
          MOVE ZTCUCLIV-ZFCLSEQ TO IT_CUCL-ZFCLSEQ.
          APPEND IT_CUCL.
       ENDIF.

  ENDLOOP.

ENDFORM.                    " P4000_CREATE_CUCL
*&---------------------------------------------------------------------*
*&      Form  P2000_CREATE_CUCLIV
*&---------------------------------------------------------------------*
FORM P4000_CREATE_CUCLIV USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR W_PROC_CNT.
* 통관환율 및 Invoice 원화금액 계산. -> 통관완료시에 결?
  LOOP AT IT_SELECTED.
       IF IT_SELECTED-ZFCUST NE '1'.
          CONTINUE.
       ENDIF.
       ADD 1 TO W_PROC_CNT.
       SELECT SINGLE *
         FROM ZTIV
        WHERE ZFIVNO = IT_SELECTED-ZFIVNO.
       IF SY-SUBRC NE 0.
          CONTINUE.
       ENDIF.
       MOVE SY-UNAME TO ZTIV-UNAM.
       MOVE SY-DATUM TO ZTIV-UDAT.
       MOVE '2'      TO ZTIV-ZFCUST.
       UPDATE ZTIV.
       IF SY-SUBRC NE 0.
          MESSAGE E739 WITH ZTIV-ZFIVNO.
          MOVE 'Y' TO W_ERR_CHK.
          EXIT.
       ENDIF.
*
  ENDLOOP.

  REFRESH IT_SELECTED_TMP.
  LOOP AT IT_SELECTED.                                  "통관순번부?
       IF IT_SELECTED-ZFCUST NE '1'.
          CONTINUE.
       ENDIF.
       SELECT SINGLE * FROM ZTIV
              WHERE ZFIVNO = IT_SELECTED-ZFIVNO.
       IF SY-SUBRC NE 0.
          CONTINUE.
       ENDIF.

       SELECT MAX( ZFCLSEQ ) INTO IT_SELECTED-ZFCLSEQ
              FROM ZTCUCLIV
       WHERE ZFBLNO = ZTIV-ZFBLNO.

       ADD  1               TO IT_SELECTED-ZFCLSEQ.
       CLEAR   IT_SELECTED_TMP.
       MOVE-CORRESPONDING IT_SELECTED TO IT_SELECTED_TMP.
       APPEND IT_SELECTED_TMP.
  ENDLOOP.
  REFRESH IT_SELECTED.
  LOOP AT IT_SELECTED_TMP.
       CLEAR   IT_SELECTED.
       MOVE-CORRESPONDING IT_SELECTED_TMP TO IT_SELECTED.
       APPEND IT_SELECTED.
  ENDLOOP.

  LOOP AT IT_SELECTED.
       IF IT_SELECTED-ZFCUST NE '1'.
          CONTINUE.
       ENDIF.
       CLEAR : ZTIV, ZTCUCLIV.
       SELECT SINGLE *
         FROM ZTIV
        WHERE ZFIVNO = IT_SELECTED-ZFIVNO.
       IF SY-SUBRC NE 0.
          CONTINUE.
       ENDIF.

       MOVE-CORRESPONDING ZTIV    TO ZTCUCLIV.
       MOVE IT_SELECTED-ZFCLSEQ   TO ZTCUCLIV-ZFCLSEQ.
       MOVE 'A'                   TO ZTCUCLIV-ZFCLCD.
       MOVE '1'                   TO ZTCUCLIV-ZFCUST.
       MOVE SY-UNAME              TO ZTCUCLIV-UNAM.
       MOVE SY-DATUM              TO ZTCUCLIV-UDAT.
       MOVE SY-UNAME              TO ZTCUCLIV-ERNAM.
       MOVE SY-DATUM              TO ZTCUCLIV-CDAT.
       INSERT ZTCUCLIV.
       IF SY-SUBRC NE 0.
          MESSAGE E739 WITH ZTIV-ZFIVNO.
          MOVE 'Y' TO W_ERR_CHK.
          EXIT.
       ENDIF.

       SELECT *
         FROM ZTIVIT
        WHERE ZFIVNO = ZTIV-ZFIVNO.
              MOVE-CORRESPONDING ZTIVIT TO ZTCUCLIVIT.
              INSERT ZTCUCLIVIT.
              IF SY-SUBRC NE 0.
                 MESSAGE E739 WITH ZTIV-ZFIVNO.
                 MOVE 'Y' TO W_ERR_CHK.
                 EXIT.
              ENDIF.
       ENDSELECT.

  ENDLOOP.

ENDFORM.                    " P4000_CREATE_CUCLIV
*&---------------------------------------------------------------------*
*&      Form  P4000_CREATE_CUIDR
*&---------------------------------------------------------------------*
FORM P4000_CREATE_CUIDR USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR W_CRET_CNT.
  SELECT SINGLE *
    FROM ZTIMIMG00.
*
  LOOP AT IT_CUCL.                              " 수입신고 자료 형?
       CLEAR ZTIDR.
       MOVE IT_CUCL-ZFBLNO      TO ZTIDR-ZFBLNO.         " B/L 관리번?
       MOVE IT_CUCL-ZFCLSEQ     TO ZTIDR-ZFCLSEQ.        " 통관순?
       MOVE 'KRW'               TO ZTIDR-ZFINAMTC.       " 보험료통?
       MOVE 'B'                 TO ZTIDR-ZFITKD.         " 수입신고종?
       MOVE 'ETC'               TO ZTIDR-ZFTRCN.         " 운송용?
*       MOVE ZTIMIMG00-ZFTDNO    TO ZTIDR-ZFTDNO.         " 납세자 통관?
       MOVE ZTIMIMGTX-ZFTDNM1   TO ZTIDR-ZFTDNM1.        " 납세자 상?
       MOVE ZTIMIMGTX-ZFTDNM2   TO ZTIDR-ZFTDNM2.        " 납세자 성?
*       MOVE ZTIMIMG00-ZFTDAD1   TO ZTIDR-ZFTDAD1.        " 납세자 주소1
*       MOVE ZTIMIMG00-ZFTDAD2   TO ZTIDR-ZFTDAD2.        " 납세자 주소2
*       MOVE ZTIMIMG00-ZFTDTC    TO ZTIDR-ZFTDTC.         " 납세자 사업?
       MOVE 'KRW'               TO ZTIDR-ZFKRW.          " 원화통?
       MOVE 'USD'               TO ZTIDR-ZFUSD.          " 미화통?
       MOVE 'N'                 TO ZTIDR-ZFDNCD.         " Download 여?
       MOVE SY-UNAME            TO ZTIDR-ERNAM.
       MOVE SY-DATUM            TO ZTIDR-CDAT.
       MOVE SY-UNAME            TO ZTIDR-UNAM.
       MOVE SY-DATUM            TO ZTIDR-UDAT.
*
       CLEAR ZTBL.
       SELECT SINGLE *
         FROM ZTBL
        WHERE ZFBLNO = IT_CUCL-ZFBLNO.

       SELECT SINGLE * FROM ZTIMIMGTX
              WHERE BUKRS EQ ZTBL-BUKRS.
       IF SY-SUBRC NE 0.
          MESSAGE E949 WITH ZTBL-BUKRS.
       ENDIF.

       MOVE ZTBL-ZFPONC         TO ZTIDR-ZFPONC.          "수입거래구?
       MOVE ZTBL-ZFAPRTC        TO ZTIDR-ZFAPRTC.         "도착?
       MOVE ZTBL-ZFCARC         TO ZTIDR-ZFSCON.          "적출?
       MOVE ZTBL-ZFETA          TO ZTIDR-ZFENDT.          "입항?
*>> 유환.
       IF ZTBL-ZFPOYN = 'Y'.
          MOVE 'B'              TO ZTIDR-ZFIMCD.          "수입자구?
*          MOVE ZTIMIMGTX-ZFAPNO1 TO ZTIDR-ZFAPNO.   "수입자 무역업등록?
*          MOVE ZTIMIMGTX-ZFIAPNM1 TO ZTIDR-ZFIAPNM.  "수입자 상?
          MOVE ZTIMIMGTX-ZFAPNO2 TO ZTIDR-ZFAPNO.   "수입자 무역업등록?
          MOVE ZTIMIMGTX-ZFIAPNM2 TO ZTIDR-ZFIAPNM.  "수입자 상?
       ENDIF.
       IF ZTBL-ZFPOYN = 'N'.
          MOVE 'A'              TO ZTIDR-ZFIMCD.
          MOVE ZTIMIMGTX-ZFAPNO2 TO ZTIDR-ZFAPNO.   "수입자 무역업등록?
          MOVE ZTIMIMGTX-ZFIAPNM2 TO ZTIDR-ZFIAPNM.  "수입자 상?
       ENDIF.
       MOVE ZTBL-ZFHBLNO        TO ZTIDR-ZFHBLNO.         "House B/L No
       IF ZTBL-ZFVIA = 'AIR'.
          MOVE '40'             TO ZTIDR-ZFTRMET.         "운송수?
       ENDIF.
       IF ZTBL-ZFVIA = 'VSL'.
          MOVE '10'             TO ZTIDR-ZFTRMET.
       ENDIF.
       MOVE ZTBL-ZFCARNM        TO ZTIDR-ZFCARNM.         "선기?
*
       MOVE ZTBL-ZFGMNO    TO ZTIDR-ZFGOMNO.             "화물관리번?
*       MOVE ZTBL-ZFPKCN    TO ZTIDR-ZFPKCNT.             "총포장갯?
       MOVE ZTBL-ZFPKCNM   TO ZTIDR-ZFPKNM.              "포장종?
*       MOVE ZTBL-ZFTOWT    TO ZTIDR-ZFTOWT.              "총중?
       MOVE ZTBL-ZFTOWTM   TO ZTIDR-ZFTOWTM.             "총중량단?

       SELECT MAX( WAERS ) INTO ZTIDR-ZFTFAC               " 운임 A 통?
         FROM ZTBLCST
        WHERE ZFBLNO = ZTIDR-ZFBLNO
          AND WAERS NE 'KRW'
          AND ZFCSCD IN ('ABC', 'ACF').
*          AND ZFCSCD IN ( SELECT ZFCD
*                            FROM ZTIMIMG08
*                           WHERE ZFCDTY = '004').
       SELECT SUM( ZFCAMT ) INTO ZTIDR-ZFTFA               " 운임 A 금?
         FROM ZTBLCST
        WHERE ZFBLNO = ZTIDR-ZFBLNO
          AND WAERS = ZTIDR-ZFTFAC
          AND ZFCSCD IN ('ABC', 'ACF').
*          AND ZFCSCD IN ( SELECT ZFCD
*                            FROM ZTIMIMG08
*                           WHERE ZFCDTY = '004').
       SELECT MAX( WAERS ) INTO ZTIDR-ZFTFBC               " 운임 B 통?
         FROM ZTBLCST
        WHERE ZFBLNO = ZTIDR-ZFBLNO
          AND WAERS NE ZTIDR-ZFTFAC
          AND WAERS NE 'KRW'
          AND ZFCSCD IN ('ABC', 'ACF').
*          AND ZFCSCD IN ( SELECT ZFCD
*                            FROM ZTIMIMG08
*                           WHERE ZFCDTY = '004').
       SELECT SUM( ZFCAMT ) INTO ZTIDR-ZFTFB               " 운임 B 금?
         FROM ZTBLCST
        WHERE ZFBLNO = ZTIDR-ZFBLNO
          AND WAERS = ZTIDR-ZFTFBC
          AND ZFCSCD IN ('ABC', 'ACF').
*          AND ZFCSCD IN ( SELECT ZFCD
*                            FROM ZTIMIMG08
*                           WHERE ZFCDTY = '004').

       INSERT ZTIDR.
       IF SY-SUBRC NE 0.
          MESSAGE E749 WITH ZTIDR-ZFBLNO.
          MOVE 'Y' TO W_ERR_CHK.
          EXIT.
       ENDIF.
       ADD 1       TO W_CRET_CNT.
*
       REFRESH IT_IDRIT.
       SELECT *
         FROM ZTCUCLIV
        WHERE ZFBLNO = IT_CUCL-ZFBLNO
          AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.
              SELECT *
                FROM ZTCUCLIVIT
               WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
                     CLEAR IT_IDRIT.
                     IF ZTCUCLIV-ZFCLCD = 'A'.
                        SELECT SINGLE *
                          FROM ZTIV
                         WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
*                        MOVE ZTIV-ZFREQNO TO IT_IDRIT-ZFREQNO.
                     ENDIF.
                     MOVE-CORRESPONDING ZTCUCLIVIT  TO IT_IDRIT.
                     APPEND IT_IDRIT.
              ENDSELECT.
       ENDSELECT.
*
       LOOP AT IT_IDRIT.
            CLEAR ZTIDRHS.
            SELECT SINGLE *
              FROM ZTIDRHS
             WHERE ZFBLNO = IT_CUCL-ZFBLNO
               AND ZFCLSEQ = IT_CUCL-ZFCLSEQ
               AND STAWN = IT_IDRIT-STAWN.

            IF SY-SUBRC NE 0.
               SELECT MAX( ZFCONO ) INTO ZTIDRHS-ZFCONO
                 FROM ZTIDRHS
                WHERE ZFBLNO = IT_CUCL-ZFBLNO
                  AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.
               MOVE IT_CUCL-ZFBLNO      TO ZTIDRHS-ZFBLNO.
               MOVE IT_CUCL-ZFCLSEQ     TO ZTIDRHS-ZFCLSEQ.
               ADD  1                   TO ZTIDRHS-ZFCONO.    "?
               MOVE IT_IDRIT-STAWN      TO ZTIDRHS-STAWN.     "HS Code
               SELECT MAX( TEXT1 ) INTO ZTIDRHS-ZFTGDNM       "거래품?
                 FROM T604T
                WHERE SPRAS = SY-LANGU
                  AND STAWN = IT_IDRIT-STAWN.
               MOVE 'KRW'               TO ZTIDRHS-ZFKRW.
               MOVE 'USD'               TO ZTIDRHS-ZFUSD.
               MOVE ZTBL-ZFTOWTM   TO ZTIDR-ZFTOWTM.        "중량단?

               INSERT ZTIDRHS.
               IF SY-SUBRC NE 0.
                  MESSAGE E749 WITH IT_CUCL-ZFBLNO.
                  MOVE 'Y' TO W_ERR_CHK.
                  EXIT.
               ENDIF.
            ENDIF.

            CLEAR ZTIDRHSD.
            SELECT MAX( ZFRONO ) INTO ZTIDRHSD-ZFRONO
              FROM ZTIDRHSD
             WHERE ZFBLNO = ZTIDRHS-ZFBLNO
               AND ZFCLSEQ = ZTIDRHS-ZFCLSEQ
               AND ZFCONO = ZTIDRHS-ZFCONO.
            MOVE ZTIDRHS-ZFBLNO      TO ZTIDRHSD-ZFBLNO.
            MOVE ZTIDRHS-ZFCLSEQ     TO ZTIDRHSD-ZFCLSEQ.
            MOVE ZTIDRHS-ZFCONO      TO ZTIDRHSD-ZFCONO.      "?
            ADD  1                   TO ZTIDRHSD-ZFRONO.      "?
            MOVE IT_IDRIT-ZFIVNO     TO ZTIDRHSD-ZFIVNO.     "관리번?
            MOVE IT_IDRIT-ZFIVDNO    TO ZTIDRHSD-ZFIVDNO.    "일련번?
            MOVE IT_IDRIT-MATNR      TO ZTIDRHSD-ZFSTCD.     "규격코?
            MOVE IT_IDRIT-TXZ01      TO ZTIDRHSD-ZFGDDS1.     "규격1
            MOVE IT_IDRIT-MENGE      TO ZTIDRHSD-ZFQNT.       "수?
            MOVE IT_IDRIT-MEINS      TO ZTIDRHSD-ZFQNTM.      "수량단?
            MOVE IT_IDRIT-NETPR      TO ZTIDRHSD-NETPR.       "단?
            MOVE IT_IDRIT-PEINH      TO ZTIDRHSD-PEINH.       "Price uni
            MOVE IT_IDRIT-BPRME      TO ZTIDRHSD-BPRME.       "Order pri
*            IF ZTIDRHSD-PEINH > 0.
*               ZTIDRHSD-ZFAMT = ZTIDRHSD-NETPR * ZTIDRHSD-ZFQNT
*                                               / ZTIDRHSD-PEINH. "금?
*            ELSE.
*               ZTIDRHSD-ZFAMT = 0.
*            ENDIF.
            MOVE IT_IDRIT-ZFIVAMT    TO ZTIDRHSD-ZFAMT.       "금?
            MOVE IT_IDRIT-ZFIVAMC    TO ZTIDRHSD-ZFCUR.       "통?
            MOVE IT_IDRIT-STAWN      TO ZTIDRHSD-STAWN.       "HS Code
            IF NOT ( IT_IDRIT-ZFREQNO IS INITIAL ).       " 수입의뢰수?
               SELECT SINGLE MENGE INTO ZTIDRHSD-ZFMENGE
                 FROM ZTREQIT
                WHERE ZFREQNO = IT_IDRIT-ZFREQNO
                  AND ZFITMNO = IT_IDRIT-ZFIVDNO.
            ENDIF.
            INSERT ZTIDRHSD.
            IF SY-SUBRC NE 0.
               MESSAGE E749 WITH IT_CUCL-ZFBLNO.
               MOVE 'Y' TO W_ERR_CHK.
               EXIT.
            ENDIF.
       ENDLOOP.                      " IT_IDRIT

  ENDLOOP.                           " IT_CUCL

  LOOP AT IT_CUCL.

       SELECT SINGLE *
         FROM ZTIDR
        WHERE ZFBLNO = IT_CUCL-ZFBLNO
          AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.

       CLEAR W_MENGE.
       SELECT *
         FROM ZTIV
        WHERE ZFBLNO = IT_CUCL-ZFBLNO.
              SELECT *
                FROM ZTIVIT
               WHERE ZFIVNO = ZTIV-ZFIVNO.
                     ADD ZTIVIT-CCMENGE  TO W_MENGE.
              ENDSELECT.
       ENDSELECT.
       CLEAR ZTBL.
       SELECT SINGLE *
         FROM ZTBL
        WHERE ZFBLNO = IT_CUCL-ZFBLNO.
*
       CLEAR ZTIDRHS.
       SELECT *
         FROM ZTIDRHS
        WHERE ZFBLNO = IT_CUCL-ZFBLNO
          AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.
              SELECT SUM( ZFQNT ) MAX( ZFQNTM )            " 란사항 수?
                INTO (ZTIDRHS-ZFQNT, ZTIDRHS-ZFQNTM)
                FROM ZTIDRHSD
               WHERE ZFBLNO  = ZTIDRHS-ZFBLNO
                 AND ZFCLSEQ = ZTIDRHS-ZFCLSEQ
                 AND ZFCONO  = ZTIDRHS-ZFCONO.
              IF ( ZTIDRHS-ZFQNTM NE 'L'  ) AND
                 ( ZTIDRHS-ZFQNTM NE 'KG' ) AND
                 ( ZTIDRHS-ZFQNTM NE 'G'  ).
                 MOVE 'U'    TO ZTIDRHS-ZFQNTM.
              ENDIF.
              IF W_MENGE > 0.                              " 란사항 중?
                 ZTIDRHS-ZFWET =
                         ZTBL-ZFTOWT * ZTIDRHS-ZFQNT / W_MENGE.
              ELSE.
                 ZTIDRHS-ZFWET = 0.
              ENDIF.
              UPDATE ZTIDRHS.
              IF SY-SUBRC NE 0.
                 MESSAGE E749 WITH IT_CUCL-ZFBLNO.
                 MOVE 'Y' TO W_ERR_CHK.
                 EXIT.
              ENDIF.
       ENDSELECT.
*

       SELECT SUM( ZFWET )  SUM( ZFQNT )         "공통사항 총중?
         INTO (ZTIDR-ZFTOWT, W_ZFQNT)
         FROM ZTIDRHS
        WHERE ZFBLNO = IT_CUCL-ZFBLNO
          AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.

       ZTIDR-ZFPKCNT = ZTBL-ZFPKCN * W_ZFQNT / W_MENGE. "포장수?

       SELECT SUM( ZFAMT ) MAX( ZFCUR )  "공통사항 결제금액/통?
         INTO (ZTIDR-ZFSTAMT, ZTIDR-ZFSTAMC)
         FROM ZTIDRHSD
        WHERE ZFBLNO = IT_CUCL-ZFBLNO
          AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.
       MOVE ZTIDR-ZFSTAMC TO ZTIDR-ZFADAMCU. " 가산금액 통?
       MOVE ZTIDR-ZFSTAMC TO ZTIDR-ZFDUAMCU. " 공제금액 통?
       CLEAR W_ZFIVAMT.
       SELECT SUM( ZFIVAMT ) INTO W_ZFIVAMT
         FROM ZTCUCLIV
        WHERE ZFBLNO = IT_CUCL-ZFBLNO.
       IF W_ZFIVAMT > 0.
          ZTIDR-ZFTFA = ZTIDR-ZFTFA * ZTIDR-ZFSTAMT / W_ZFIVAMT. " 운임A
          ZTIDR-ZFTFB = ZTIDR-ZFTFB * ZTIDR-ZFSTAMT / W_ZFIVAMT. " 운임B
       ENDIF.

       REFRESH IT_REQHD.
       SELECT SINGLE *
         FROM ZTCUCL
        WHERE ZFBLNO = IT_CUCL-ZFBLNO
          AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.
       IF ZTCUCL-ZFCLCD = 'A'.
          SELECT *
            FROM ZTCUCLIV
           WHERE ZFBLNO = IT_CUCL-ZFBLNO
             AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.
                 CLEAR ZTIV.
                 SELECT SINGLE *
                   FROM ZTIV
                  WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
                 ADD ZTIV-ZFPKCHG       TO ZTIDR-ZFADAM. " 가산금?
                 ADD ZTIV-ZFHDCHG       TO ZTIDR-ZFADAM.
                 CLEAR W_ZFDUAM.
                 SELECT SUM( KWERT )  " 공제금?
                   INTO W_ZFDUAM
                   FROM ZTIVIT
                  WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
                 ADD W_ZFDUAM TO ZTIDR-ZFDUAM.
                 IF ZTIV-ZFPOYN = 'Y'.
*                   READ TABLE IT_REQHD WITH KEY ZFREQNO = ZTIV-ZFREQNO.
                    IF SY-SUBRC NE 0.
*                       MOVE ZTIV-ZFREQNO    TO IT_REQHD-ZFREQNO.
                       APPEND IT_REQHD.
                    ENDIF.
                 ENDIF.
          ENDSELECT.

          CLEAR : W_ZFINAMT, W_ZFLASTAM, W_LOOP_CNT.
          MOVE 'GN'      TO ZTIDR-ZFAMCD.         "대금결제방?
          LOOP AT IT_REQHD.
               CLEAR ZTREQIL.
               SELECT *
                 FROM ZTREQIL
                WHERE ZFREQNO = IT_REQHD-ZFREQNO.
                      IF NOT ( ZTREQIL-ZFRECNO IS INITIAL ).
                         CLEAR ZTIDRHSL.
                         MOVE IT_CUCL-ZFBLNO  TO ZTIDRHSL-ZFBLNO.
                         MOVE IT_CUCL-ZFCLSEQ TO ZTIDRHSL-ZFCLSEQ.
                         MOVE 1               TO ZTIDRHSL-ZFCONO.
                         MOVE '911'           TO ZTIDRHSL-ZFCNDC.
                         MOVE ZTREQIL-ZFRECNO TO ZTIDRHSL-ZFCNNO.
                         INSERT ZTIDRHSL.
                         IF SY-SUBRC NE 0.
                            MESSAGE E749 WITH IT_CUCL-ZFBLNO.
                            MOVE 'Y' TO W_ERR_CHK.
                            EXIT.
                         ENDIF.
                      ENDIF.
               ENDSELECT.
               SELECT MAX( ZFAPLDT ) INTO W_ZFAPLDT
                 FROM ZTIMIMG01
                WHERE ZTERM = ZTREQHD-ZTERM.
               SELECT SINGLE *
                 FROM ZTIMIMG01
                 WHERE ZTERM = ZTREQHD-ZTERM
                   AND ZFAPLDT = W_ZFAPLDT.
               IF ZTIMIMG01-ZFREQTY = 'DP'.
                  MOVE 'DP'      TO ZTIDR-ZFAMCD.
               ENDIF.

               SELECT SINGLE *
                 FROM ZTREQHD
                WHERE ZFREQNO = IT_REQHD-ZFREQNO.
               MOVE ZTREQHD-EBELN    TO ZTIDR-ZFREBELN.     "대표P/O번?
*>>>KSB
*               SELECT SINGLE ZZBUSTYPE INTO ZTIDR-ZZBUSTYPE " 용도구?
*                 FROM EKKO
*                WHERE EBELN = ZTIDR-ZFREBELN.
               MOVE ZTREQHD-ZFOPNNO  TO ZTIDR-ZFOPNNO.      "대표L/C번?
               MOVE ZTREQHD-INCO1    TO ZTIDR-INCO1.        "Incoterms
               MOVE ZTREQHD-ZFPRNAM  TO ZTIDR-ZFPRNAM.      "P/R담당?
               MOVE ZTREQHD-ZFMATGB  TO ZTIDR-ZFMATGB.      "자재구?
               IF ZTIDR-ZFMATGB = '1'.
                 MOVE 'B'   TO ZTIDR-ZFITKD.     " 수입신고종?
               ELSE.
                 MOVE 'K'   TO ZTIDR-ZFITKD.     " 수입신고종?
               ENDIF.
               ADD ZTREQHD-ZFLASTAM  TO W_ZFLASTAM.
               ADD  1  TO  W_LOOP_CNT.
               IF W_LOOP_CNT = 1.
                  CONCATENATE ZTREQHD-EBELN '-' ZTREQHD-ZFOPNNO
                                                INTO ZTIDR-ZFCTW1.
               ENDIF.
               IF W_LOOP_CNT = 2.
                  CONCATENATE ZTREQHD-EBELN '-' ZTREQHD-ZFOPNNO
                                                INTO ZTIDR-ZFCTW2.
               ENDIF.
               IF W_LOOP_CNT = 3.
                  CONCATENATE ZTREQHD-EBELN '-' ZTREQHD-ZFOPNNO
                                                INTO ZTIDR-ZFCTW3.
               ENDIF.
               IF W_LOOP_CNT = 4.
                  CONCATENATE ZTREQHD-EBELN '-' ZTREQHD-ZFOPNNO
                                                INTO ZTIDR-ZFCTW4.
               ENDIF.
               IF W_LOOP_CNT = 5.
                  CONCATENATE ZTREQHD-EBELN '-' ZTREQHD-ZFOPNNO
                                                INTO ZTIDR-ZFCTW5.
               ENDIF.

               CLEAR W_ZFCKAMT.
               SELECT SUM( ZFCKAMT ) INTO W_ZFCKAMT
                 FROM ZTRECST
                WHERE ZFREQNO = IT_REQHD-ZFREQNO
                  AND ZFCSCD = '1AB'.
               ADD W_ZFCKAMT    TO W_ZFINAMT. " 보험?
          ENDLOOP.
          ZTIDR-ZFINAMT = W_ZFINAMT * ZTIDR-ZFSTAMT / W_ZFLASTAM.
       ENDIF.

       UPDATE ZTIDR.
       IF SY-SUBRC NE 0.
          MESSAGE E749 WITH IT_CUCL-ZFBLNO.
          MOVE 'Y' TO W_ERR_CHK.
          EXIT.
       ENDIF.

  ENDLOOP.

ENDFORM.                    " P4000_CREATE_CUIDR
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFIVNO.

  SELECT SINGLE *
    FROM ZTIV
   WHERE ZFIVNO = P_ZFIVNO.
*  SET PARAMETER ID 'ZPREQNO' FIELD ZTIV-ZFREQNO.
*  SET PARAMETER ID 'ZPREQNO' FIELD IT_TAB-ZFREQNO.
  EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.

ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_BL USING    P_ZFBLNO.

  SET PARAMETER ID 'ZPBLNO'  FIELD   P_ZFBLNO.
  SET PARAMETER ID 'ZPHBLNO'  FIELD   ''.

  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_BL
*&---------------------------------------------------------------------*
*&      Form  P4000_CREATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P4000_CREATE_DATA USING    P_W_ERR_CHK.
  MOVE 'N' TO W_ERR_CHK.
  CLEAR W_PROC_CNT.

  LOOP AT IT_SELECTED.
    IF IT_SELECTED-ZFCUST NE '1'. CONTINUE. ENDIF.
    W_PROC_CNT = W_PROC_CNT + 1.
    CALL FUNCTION 'ZIM_CUDATA_CREATE'
       EXPORTING
             W_ZFIVNO            =   IT_SELECTED-ZFIVNO
       IMPORTING
             W_ZFBLNO            =   W_ZFBLNO
             W_ZFCLSEQ           =   W_ZFCLSEQ
       EXCEPTIONS
             ERROR_INSERT.

    IF SY-SUBRC NE 0.
       MOVE 'Y' TO W_ERR_CHK.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P4000_CREATE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ZTIIMIMG08_SELECT
*&---------------------------------------------------------------------*
FORM GET_ZTIIMIMG08_SELECT USING  P_KEY
                                  P_ZFCD
                           CHANGING P_NAME.
  CLEAR ZTIMIMG08.
  SELECT SINGLE *
         FROM ZTIMIMG08
         WHERE ZFCDTY = P_KEY
           AND ZFCD =  P_ZFCD.

  P_NAME = ZTIMIMG08-ZFCDNM.

ENDFORM.                    " GET_ZTIIMIMG08_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_IDR
*&---------------------------------------------------------------------*
FORM P2000_SHOW_IDR USING    P_ZFBLNO
                             P_ZFCLSEQ.
  IF P_ZFBLNO IS INITIAL OR P_ZFCLSEQ IS INITIAL.
      STOP.
  ENDIF.

  SET PARAMETER ID 'ZPHBLNO' FIELD  ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD  P_ZFBLNO.
  SET PARAMETER ID 'ZPCLSEQ' FIELD  P_ZFCLSEQ.
  SET PARAMETER ID 'ZPIDRNO' FIELD  ''.
  CALL TRANSACTION 'ZIM63' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_IDS
