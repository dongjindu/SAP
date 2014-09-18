*----------------------------------------------------------------------*
*   INCLUDE ZRIM09F01                                                  *
*----------------------------------------------------------------------*
*&  프로그램명 : 보세창고 출고관련 SUB MODULE Include                  *
*&      작성자 : 이채경 INFOLINK Ltd.
*&
*&      작성일 : 2001.08.17                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_GUI_TEXT
*&---------------------------------------------------------------------*
FORM P2000_SET_GUI_TEXT.

   CASE W_STATUS.
      WHEN C_REQ_C.   ASSIGN W_CREATE  TO <FS_F>.
      WHEN C_REQ_U.   ASSIGN W_CHANGE  TO <FS_F>.
      WHEN C_REQ_D.   ASSIGN W_DISPLAY TO <FS_F>.
      WHEN OTHERS.
   ENDCASE.

ENDFORM. "  P2000_SET_GUI_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM P2000_SET_PF_STATUS.

   CASE SY-TCODE.
      WHEN 'ZIMBG1' OR 'ZIMBG2' OR 'ZIMBG3' .      " 생성,변경,조회.
         MOVE '0101' TO W_PFSTAT.
      WHEN OTHERS.
   ENDCASE.

ENDFORM.                    " P2000_SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_STATUS_SCR_DISABLE
*&---------------------------------------------------------------------*
FORM P2000_SET_STATUS_SCR_DISABLE.

  CASE SY-DYNNR.
     WHEN  0101.              " 세부화면.
            IF SY-TCODE = 'ZIMBG1'.
               MOVE 'Create bonded warehouse G/I : details' TO W_TITLE.
               SET TITLEBAR  'TITLE' WITH W_TITLE.
               MOVE 'HIST' TO IT_EXCL-FCODE. APPEND IT_EXCL. " 헤더변경.
               MOVE 'HIIT' TO IT_EXCL-FCODE. APPEND IT_EXCL.   " 아이템.
               MOVE 'CRDC' TO IT_EXCL-FCODE. APPEND IT_EXCL.   " 생성.
               MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL.   " 삭제.
            ENDIF.
            IF SY-TCODE = 'ZIMBG2'.
               MOVE 'Change bonded warehouse G/I : details' TO W_TITLE.
               MOVE 'CHDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 변경.
               SET TITLEBAR  'TITLE' WITH W_TITLE.
            ENDIF.
            IF SY-TCODE = 'ZIMBG3'.
               MOVE 'Display bonded warehouse G/I : details' TO W_TITLE.
               SET TITLEBAR  'TITLE' WITH W_TITLE.
               MOVE 'DISP' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
               MOVE 'DELE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 삭제.
               MOVE 'SAVE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 저장.
            ENDIF.
      WHEN  0100.
            MOVE 'Create bonded warehouse G/I : initials' TO W_TITLE.
            SET TITLEBAR  'TITLE' WITH W_TITLE.
            MOVE 'HIST' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 헤더변경.
            MOVE 'HIIT' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 아이템.
            MOVE 'CRDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 생성.
            MOVE 'DELE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 삭제.
            MOVE 'SAVE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 저장.
            MOVE 'DISP1' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
            MOVE 'DISP2' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
            MOVE 'DISP3' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
            MOVE 'DISP4' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
            MOVE 'PRI'   TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.

      WHEN  0200.
            MOVE 'Change bonded warehouse G/I : initials' TO W_TITLE.
            SET TITLEBAR  'TITLE' WITH W_TITLE.
            MOVE 'HIST' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 헤더변경.
            MOVE 'HIIT' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 아이템.
            MOVE 'CHDC' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 변경.
            MOVE 'DELE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 삭제.
            MOVE 'SAVE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 저장.
            MOVE 'DISP1' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
            MOVE 'DISP2' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
            MOVE 'DISP3' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
            MOVE 'DISP4' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
            MOVE 'PRI'   TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
      WHEN  0300.
            MOVE 'Display bonded warehouse G/I : initials' TO W_TITLE.
            SET TITLEBAR  'TITLE' WITH W_TITLE.
            MOVE 'HIST' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 헤더변경.
            MOVE 'HIIT' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 아이템.
            MOVE 'DISP' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
            MOVE 'DELE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 삭제.
            MOVE 'SAVE' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 저장.
            MOVE 'DISP1' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
            MOVE 'DISP2' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
            MOVE 'DISP3' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
            MOVE 'DISP4' TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.
            MOVE 'PRI'   TO IT_EXCL-FCODE.  APPEND IT_EXCL.   " 조회.

      ENDCASE.

ENDFORM.                    " P2000_SET_STATUS_SCR_DISABLE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_BG_INIT_SCR
*&---------------------------------------------------------------------*
FORM P2000_SET_BG_INIT_SCR.

   MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " IMPORT IMG
   MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 저장.

ENDFORM.                    " P2000_SET_BG_INIT_SCR
*&---------------------------------------------------------------------*
*&      Form  P2000_CC_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
FORM P2000_CC_DOC_ITEM_SELECT.

   W_ZFIVNO    = ZSIV-ZFIVNO.

   REFRESH IT_ZSIV.
* Table Multi-Select
   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIV
            FROM   ZVBL_IV
            WHERE  ZFHBLNO EQ ZSIV-ZFHBLNO.

  DESCRIBE TABLE IT_ZSIV LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'CCHBL'.                 ">통관요청 조회.
  CALL SCREEN 0014 STARTING AT  07 3
                   ENDING   AT  70 15.

ENDFORM.                    " P2000_CC_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_CHECK_ZTIV
*&---------------------------------------------------------------------*
FORM P2000_CHECK_VALUES USING    P_ZFIVNO.

*>> 보세운송
  SELECT SINGLE *
          FROM ZTIV
         WHERE ZFIVNO = P_ZFIVNO.
  IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE E679 WITH ZTIV-ZFBLNO.
  ENDIF.

  ZSIV-ZFBLNO = ZTIV-ZFBLNO.
  IF ZTIV-ZFCLCD NE 'A'.
      W_ERR_CHK = 'Y'.
      MESSAGE E445 WITH ZTIV-ZFBLNO.
  ENDIF.
*>> 통관상태.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCUST' ZTIV-ZFCUST
                             CHANGING   W_DOM_TEX1.
  IF NOT ( ZTIV-ZFCUST = 'Y' OR ZTIV-ZFCUST = '3' ) .
     W_ERR_CHK = 'Y'.
     MESSAGE E419 WITH ZSIV-ZFIVNO W_DOM_TEX1 'Follow-up'.
  ENDIF.
*>> 입고상태.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDGRST' ZTIV-ZFGRST
                             CHANGING   W_DOM_TEX1.
  IF ZTIV-ZFGRST EQ 'Y'.
     W_ERR_CHK = 'Y'.
     MESSAGE E422 WITH ZSIV-ZFIVNO W_DOM_TEX1 'Follow-up'.
  ENDIF.

ENDFORM.                    " P2000_CHECK_VALUES
*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_LISTING
*&---------------------------------------------------------------------*
FORM P2000_DATA_LISTING.

  CASE INCLUDE.
    WHEN 'CCHBL' OR 'CCBL'.   "> 통관요청.(HOUSE B/L 입력시)
       PERFORM P3000_ZTIV_TITLELIST.
       LOOP AT IT_ZSIV.
          W_MOD = SY-TABIX MOD 2.
          PERFORM P2000_ZTIV_DUP_LIST_1.
       ENDLOOP.
       WRITE : / SY-ULINE(61).
       CLEAR : IT_ZSIV.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " P2000_DATA_LISTING
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIV_DUP_LIST_1
*&---------------------------------------------------------------------*
FORM P2000_ZTIV_DUP_LIST_1.

  FORMAT RESET.
  WRITE : / SY-VLINE, IT_ZSIV-ZFIVNO COLOR COL_KEY INTENSIFIED,
             SY-VLINE.
  IF W_MOD EQ 0.
     FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  WRITE : IT_ZSIV-ZFCCDT,   SY-VLINE.

*>> 통관 유형.
  CASE IT_ZSIV-ZFCLCD.
     WHEN 'A'.
        WRITE : '수입통관', SY-VLINE.
     WHEN 'B'.
        WRITE : '과세통관', SY-VLINE.
     WHEN 'C'.
        WRITE : '입항지  ', SY-VLINE.
     WHEN 'X'.
        WRITE : '미통관  ', SY-VLINE.
     WHEN OTHERS.
        WRITE : '        ', SY-VLINE.
  ENDCASE.
*>> 통관상태.
  CASE IT_ZSIV-ZFCUST.
     WHEN '1'.
        WRITE : '의뢰생성', SY-VLINE.
     WHEN '2'.
        WRITE : '의뢰대상', SY-VLINE.
     WHEN '3'.
        WRITE : '의뢰 중 ', SY-VLINE.
     WHEN 'Y'.
        WRITE : '통관완료', SY-VLINE.
     WHEN 'N'.
        WRITE : '통관불가', SY-VLINE.
  ENDCASE.
*>> 비용배부 상태.
*  CASE IT_ZSIV-ZFCDST.
*     WHEN 'N'.
*        WRITE : '배부 대상', SY-VLINE.
*     WHEN 'Y'.
*        WRITE : '배부 완료', SY-VLINE.
*     WHEN 'X'.
*        WRITE : '배부 불가', SY-VLINE.
*  ENDCASE.
*>> 입고상태.
  CASE IT_ZSIV-ZFGRST.
     WHEN 'Y'.
        WRITE : '입고 완료', SY-VLINE.
     WHEN 'N'.
        WRITE : '입고 대상', SY-VLINE.
     WHEN 'P'.
        WRITE : '분할 입고', SY-VLINE.
     WHEN 'X'.
        WRITE : '입고 불가', SY-VLINE.
  ENDCASE.
  HIDE IT_ZSIV.

ENDFORM.                    " P2000_ZTIV_DUP_LIST_1
*&---------------------------------------------------------------------*
*&      Form  P2000_SEARCH_FIELD_MOVE
*&---------------------------------------------------------------------*
FORM P2000_SEARCH_FIELD_MOVE.

  ZSIV-ZFIVNO  = W_ZFIVNO.

ENDFORM.                    " P2000_SEARCH_FIELD_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_CC_DOC_ITEM_SELECT_1
*&---------------------------------------------------------------------*
FORM P2000_CC_DOC_ITEM_SELECT_1.

  W_ZFIVNO    = ZSIV-ZFIVNO.

  REFRESH IT_ZSIV.
* Table Multi-Select
   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIV
            FROM   ZTIV
            WHERE  ZFBLNO EQ ZSIV-ZFBLNO.
  DESCRIBE TABLE IT_ZSIV LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'CCBL'.                 ">통관요청 조회.

  CALL SCREEN 0014 STARTING AT  07 3
                   ENDING   AT  70 10.

ENDFORM.                    " P2000_CC_DOC_ITEM_SELECT_1
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IV_DOC
*&---------------------------------------------------------------------*
FORM P1000_READ_BW_DOC.

  CASE SY-TCODE.
    WHEN 'ZIMBG1'.
         PERFORM P1000_READ_DATA_SRREEN100.
    WHEN 'ZIMBG2' OR 'ZIMBG3'.
         PERFORM P1000_READ_DATA_SRREEN200_300.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P1000_READ_BW_DOC
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA_SRREEN100
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA_SRREEN100.

  IF OK-CODE NE 'REF1'.  " ITEM 재선택시.
     CLEAR ZTBL.
     CLEAR ZTIV.
     SELECT SINGLE *
            FROM  ZTIV
            WHERE ZFIVNO = ZSIV-ZFIVNO.

     IF SY-SUBRC NE 0.
        MESSAGE E413 WITH ZSIV-ZFIVNO.
     ENDIF.
*>> 2002.4.table 변경. 되어서 ZTCUCLIV ZTCUCL  ZTCUCLHST 사용 안함.
*     CLEAR ZTCUCLIV.
*     SELECT SINGLE *
*            FROM  ZTCUCLIV
*            WHERE ZFIVNO  = ZSIV-ZFIVNO.
*
     CLEAR ZTIDR.
     SELECT SINGLE *
            FROM  ZTIDR
            WHERE ZFIVNO = ZSIV-ZFIVNO.
*---------------------------------------------------------------------
     CLEAR ZTBL.
     SELECT SINGLE *
            FROM  ZTBL
            WHERE ZFBLNO = ZTIDR-ZFBLNO.
     ZSIV-ZFHBLNO = ZTBL-ZFHBLNO.

     CLEAR ZTBWHD.
     SELECT *
       FROM ZTBWHD
      WHERE ZFIVNO = ZSIV-ZFIVNO
        AND ZFBLNO = ZTBL-ZFBLNO.
        ADD ZTBWHD-ZFPKCN TO  W_ZFPKCN.
     ENDSELECT.
     CLEAR ZTBWHD.
     SELECT SUM( ZFTOWT )
            INTO W_ZFTOWT
            FROM ZTBWHD
            WHERE ZFIVNO = ZSIV-ZFIVNO
              AND ZFBLNO = ZTBL-ZFBLNO .
     SELECT SUM( ZFTOVL )
            INTO W_ZFTOVL
            FROM ZTBWHD
            WHERE ZFIVNO = ZSIV-ZFIVNO
              AND ZFBLNO = ZTBL-ZFBLNO .
     CLEAR ZTBLINR.
*>> 총중량, 포장개수,종류. B/L에서 가져오던게 반입변경.(2001.10.13)
     SELECT MAX( ZFBTSEQ )
            INTO W_ZFBTSEQ
            FROM ZTBLINR
            WHERE ZFBLNO = ZTBL-ZFBLNO.
     SELECT SINGLE *
             FROM  ZTBLINR
            WHERE ZFBLNO  = ZTBL-ZFBLNO
              AND ZFBTSEQ = W_ZFBTSEQ.

*     CLEAR ZTIDR.
*     SELECT SINGLE *
*        FROM ZTIDR
*       WHERE ZFBLNO  = ZTBL-ZFBLNO
*         AND ZFCLSEQ = ZTCUC-ZFCLSEQ.

     CLEAR ZTIDS.
     SELECT SINGLE *
           FROM  ZTIDS
           WHERE ZFIVNO  = ZSIV-ZFIVNO.

     ZTBWHD-ZFPKCN = ZTBLINR-ZFPKCN - W_ZFPKCN.
     IF ZTBWHD-ZFPKCN < 0.
        ZTBWHD-ZFPKCN = 0.
     ENDIF.
     ZTBWHD-ZFTOVL = ZTBL-ZFTOVL - W_ZFTOVL.
     IF ZTBWHD-ZFTOVL < 0.
        ZTBWHD-ZFTOVL = 0.
     ENDIF.
     ZTBWHD-ZFTOWT = ZTBLINR-ZFINWT - W_ZFTOWT.
     IF ZTBWHD-ZFTOWT < 0.
        ZTBWHD-ZFTOWT = 0.
     ENDIF.
     MOVE: ZSIV-ZFIVNO       TO  ZTBWHD-ZFIVNO,
           ZTIV-ZFIVAMC      TO  ZTBWHD-WAERS,     " NO
           ZTBL-ZFBLNO       TO  ZTBWHD-ZFBLNO,
           ZTBL-BUKRS        TO  ZTBWHD-BUKRS,
           ZTIDS-ZFCLSEQ     TO  ZTBWHD-ZFCLSEQ,
           ZTBL-ZFREBELN     TO  ZTBWHD-ZFREBELN,
           ZTBL-ZFBLSDP      TO  ZTBWHD-ZFBLSDP,   " 송부처.
           ZTIDR-ZFBNARCD    TO  ZTBWHD-ZFBNARCD,  " 보세구역코드.
           ZTBLINR-ZFABNAR   TO  ZTBWHD-ZFABNAR,   " 보세구역내부
           ZTIDS-ZFIDWDT     TO  ZTBWHD-ZFIDWDT,   " 신고희망일.
           ZTBL-ZFSHNO       TO  ZTBWHD-ZFSHNO,    " NO.
           ZTIDS-ZFIDSDT     TO  ZTBWHD-ZFIDSDT,   " NO.
           ZTBL-ZFPKCNM      TO  ZTBWHD-ZFPKCNM,   " 포장단위.
           ZTBL-ZFTOWTM      TO  ZTBWHD-ZFTOWTM,   " 총중량단위.
           ZTBL-ZFTOVLM      TO  ZTBWHD-ZFTOVLM,   " 총용적단위.
           SY-DATUM          TO  ZTBWHD-ZFGIDT.
      *ZTBWHD  = ZTBWHD.
  ENDIF.
*>> ITEM SELECT.
  REFRESH: IT_ZSBWIT.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBWIT
         FROM ZTIVIT
         WHERE ZFIVNO = ZTBWHD-ZFIVNO.


  LOOP AT IT_ZSBWIT.
     W_TABIX = SY-TABIX.

*>> 통관수량.
        SELECT SUM( CCMENGE ) INTO IT_ZSBWIT-CCMENGE
            FROM ZTIVIT
            WHERE ZFIVNO  = IT_ZSBWIT-ZFIVNO
              AND ZFIVDNO = IT_ZSBWIT-ZFIVDNO.
*>> 입고수량.
        SELECT SUM( GRMENGE ) INTO IT_ZSBWIT-GRMENGE
            FROM ZTIVIT
            WHERE ZFIVNO  = IT_ZSBWIT-ZFIVNO
              AND ZFIVDNO = IT_ZSBWIT-ZFIVDNO.

*>> B/L 수량.
        SELECT SUM( BLMENGE )  INTO  IT_ZSBWIT-BLMENGE
           FROM ZTBLIT
           WHERE ZFBLNO = IT_ZSBWIT-ZFBLNO
             AND ZFBLIT = IT_ZSBWIT-ZFBLIT.

*>> 기출고 수량.
        SELECT SUM( GIMENGE ) INTO  IT_ZSBWIT-BWMENGE2
           FROM ZTBWIT
           WHERE ZFIVNO  = IT_ZSBWIT-ZFIVNO
             AND ZFIVDNO = IT_ZSBWIT-ZFIVDNO.

*>> 잔량계산.(2001.11.08 무환샘플때문에 입고수량 ->> 통관수량)
        IT_ZSBWIT-BWMENGE1 =
             IT_ZSBWIT-CCMENGE     " 통관수량.
           - IT_ZSBWIT-BWMENGE2.   " 기출고량

        IF IT_ZSBWIT-BWMENGE1 =< 0.
           DELETE IT_ZSBWIT INDEX W_TABIX.
           CONTINUE.
        ENDIF.

*>> 출고량을 기본적으로 잔량으로 넣었다.
        MOVE  IT_ZSBWIT-BWMENGE1 TO IT_ZSBWIT-GIMENGE.
        CLEAR IT_ZSBWIT-BWMENGE1.
        MODIFY IT_ZSBWIT INDEX W_TABIX.
   ENDLOOP.

   DESCRIBE TABLE IT_ZSBWIT LINES W_LINE.
   IF W_LINE = 0.
      MESSAGE E676 WITH IT_ZSBWIT-ZFIVNO.
   ENDIF.
*>> 첫번째 아이템 플랜트 가져온다.
   READ TABLE IT_ZSBWIT INDEX 1.
   SELECT SINGLE *
        FROM ZTBLIT
        WHERE ZFBLNO = IT_ZSBWIT-ZFBLNO
          AND ZFBLIT = IT_ZSBWIT-ZFBLIT.

   MOVE ZTBLIT-WERKS TO ZTBWHD-WERKS.
   IT_ZSBWIT_OLD[] = IT_ZSBWIT[].

ENDFORM.                    " P1000_READ_DATA_SRREEN100
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA_SRREEN200_300
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA_SRREEN200_300.

  IF OK-CODE NE 'REF1'.    " ITEM REFRESH 시.
     IF ZTBWHD-ZFGISEQ IS INITIAL.
        SELECT MAX( ZFGISEQ ) INTO ZTBWHD-ZFGISEQ
           FROM  ZTBWHD
           WHERE ZFIVNO = ZSIV-ZFIVNO.
     ENDIF.
     MOVE ZTBWHD-ZFGISEQ TO W_ZFGISEQ.

     CLEAR ZTBWHD.
     SELECT SINGLE *
            FROM  ZTBWHD
            WHERE ZFIVNO  = ZSIV-ZFIVNO
              AND ZFGISEQ = W_ZFGISEQ.
     ZTBWHD-ZFGISEQ = W_ZFGISEQ.
     IF SY-SUBRC NE 0.
        MESSAGE E673 WITH ZSIV-ZFIVNO ZTBWHD-ZFGISEQ.
     ENDIF.

*> LOCK OBJECT.....
     PERFORM  P2000_SET_LOCK_MODE USING 'L'.
     *ZTBWHD = ZTBWHD.

  ENDIF.
  REFRESH IT_ZSBWIT.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBWIT
            FROM  ZTBWIT
            WHERE ZFIVNO  = ZSIV-ZFIVNO
              AND ZFGISEQ = ZTBWHD-ZFGISEQ.

  LOOP AT IT_ZSBWIT.
        W_TABIX = SY-TABIX.
*>> 통관수량.
        SELECT SUM( CCMENGE ) INTO IT_ZSBWIT-CCMENGE
            FROM ZTIVIT
            WHERE ZFIVNO  = IT_ZSBWIT-ZFIVNO
              AND ZFIVDNO = IT_ZSBWIT-ZFIVDNO.
*>> 입고수량.
        SELECT SUM( GRMENGE ) INTO IT_ZSBWIT-GRMENGE
            FROM ZTIVIT
            WHERE ZFIVNO  = IT_ZSBWIT-ZFIVNO
              AND ZFIVDNO = IT_ZSBWIT-ZFIVDNO.

*>> B/L 수량.
        SELECT SUM( BLMENGE )  INTO  IT_ZSBWIT-BLMENGE
           FROM ZTBLIT
           WHERE ZFBLNO = IT_ZSBWIT-ZFBLNO
             AND ZFBLIT = IT_ZSBWIT-ZFBLIT.
*>> 기출고 수량.
        SELECT SUM( GIMENGE ) INTO  IT_ZSBWIT-BWMENGE2
           FROM ZTBWIT
           WHERE ZFIVNO  = IT_ZSBWIT-ZFIVNO
             AND ZFIVDNO  = IT_ZSBWIT-ZFIVDNO.
*>> 잔량.(2001.11.9 이채경 무환 입고수량 => 통관수량)
        IT_ZSBWIT-BWMENGE1 = IT_ZSBWIT-CCMENGE -
                             IT_ZSBWIT-BWMENGE2.

        MODIFY IT_ZSBWIT INDEX W_TABIX.
  ENDLOOP.
  IT_ZSBWIT_OLD[] = IT_ZSBWIT[].  " CHANGE DOCUENT 용.

ENDFORM.                    " P1000_READ_DATA_SRREEN200_300
*&---------------------------------------------------------------------*
*&      Form  OK_CODE_0100
*&---------------------------------------------------------------------*
FORM OK_CODE_0100.

  CASE OK-CODE.
     WHEN  'CRDC'.
        LEAVE TO TRANSACTION 'ZIMBG1'.
     WHEN  'CHDC'.
        LEAVE TO TRANSACTION 'ZIMBG2'.
     WHEN 'DISP'.
        LEAVE TO TRANSACTION 'ZIMBG3'.
     WHEN 'DELE'.
        PERFORM  P2000_EXIT_PROCESS.
     WHEN 'DISP1'.  " B/L.
        PERFORM  P2000_DISP_ZTBL USING ZTBWHD-ZFBLNO.
     WHEN  'DISP2'.  " 통관요청.
        PERFORM  P2000_DISP_ZTIV USING ZTBWHD-ZFIVNO.
     WHEN  'DISP3'.  " 수입신고.
        PERFORM  P2000_DISP_ZTIDR USING ZTBWHD-ZFBLNO ZTBWHD-ZFCLSEQ.
     WHEN  'DISP4'.  " 수입면허.
        PERFORM  P2000_DISP_ZTIDS USING ZTBWHD-ZFBLNO ZTBWHD-ZFCLSEQ.
     WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " OK_CODE_0100
*&---------------------------------------------------------------------*
*&      Form  OK_CODE_BACK_EXIT
*&---------------------------------------------------------------------*
FORM OK_CODE_BACK_EXIT.

  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT'.
       IF SY-DYNNR EQ '0101'.
          PERFORM  P2000_EXIT_PROCESS.
       ELSE.
          SET SCREEN 0. LEAVE TO SCREEN 0.
       ENDIF.
    WHEN  'PRI'.
        IF SY-TCODE EQ 'ZIMBG3'.
            PERFORM P2000_PRINT_ZTBW USING ZTBWHD-ZFIVNO ZTBWHD-ZFGISEQ.
        ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " OK_CODE_BACK_EXIT
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR_MODE_SET
*&---------------------------------------------------------------------*
FORM P2000_SCR_MODE_SET.

  LOOP AT SCREEN.
    CASE W_STATUS.
       WHEN C_REQ_C OR C_REQ_U.   " 생성, 변경.
         IF SCREEN-GROUP1 = 'IO'.   SCREEN-INPUT   = '1'.
         ELSE.                      SCREEN-INPUT   = '0'.
         ENDIF.
         IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
       WHEN C_REQ_D.              " 조회.
         IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.
         ELSE.                      SCREEN-INPUT   = '0'.
         ENDIF.
       WHEN OTHERS.
    ENDCASE.
    IF SCREEN-NAME(10) EQ 'W_ROW_MARK'.
       SCREEN-INPUT   = '1'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " P2000_SCR_MODE_SET
*&---------------------------------------------------------------------*
*&      Form  P200_LEAV_SCREEN
*&---------------------------------------------------------------------*
FORM P200_LEAV_SCREEN.

  CASE SY-DYNNR.
     WHEN '100'.
       LEAVE TO TRANSACTION 'ZIMBG1'.
     WHEN '200'.
       LEAVE TO TRANSACTION 'ZIMBG2'.
     WHEN '300'.
       LEAVE TO TRANSACTION 'ZIMBG3'.
     WHEN OTHERS.
  ENDCASE.


ENDFORM.                    " P200_LEAV_SCREEN
*&---------------------------------------------------------------------*
*&      Form  P3000_DELETE_SCR0100
*&---------------------------------------------------------------------*
FORM P3000_DELETE_SCR0100.

ENDFORM.                    " P3000_DELETE_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTBL
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTBL USING    P_ZFBLNO.

   SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
   SET PARAMETER ID 'ZPHBLNO' FIELD ''.
   CALL TRANSACTION 'ZIM23'  AND SKIP  FIRST SCREEN.


ENDFORM.                    " P2000_DISP_ZTBL
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTIV
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTIV USING    P_ZFIVNO.

   SET PARAMETER ID 'ZPIVNO'  FIELD P_ZFIVNO.
   SET PARAMETER ID 'ZPHBLNO' FIELD ''.
   SET PARAMETER ID 'ZPBLNO'  FIELD ''.

   CALL TRANSACTION 'ZIM33'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTIV
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTIDR
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTIDR USING    P_ZFBLNO
                               P_ZFCLSEQ.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD  P_ZFBLNO.
  SET PARAMETER ID 'ZPCLSEQ' FIELD  P_ZFCLSEQ.
  CALL TRANSACTION 'ZIMCD2' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTIDR
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTIDS
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTIDS USING    P_ZFBLNO
                               P_ZFCLSEQ.

  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD  P_ZFBLNO.
  SET PARAMETER ID 'ZPCLSEQ' FIELD  P_ZFCLSEQ.
  SET PARAMETER ID 'ZPENTNO' FIELD ' '.
  CALL TRANSACTION 'ZIMCC3' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTIDS
*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_EXIT_PROCESS.

  IF NOT W_STATUS EQ C_REQ_D.
     PERFORM P2000_SET_MESSAGE USING  OK-CODE.
     CASE ANTWORT.
        WHEN 'Y'.              " Yes...
*-----------------------------------------------------------------------
* DB Write
*-----------------------------------------------------------------------
           PERFORM  P3000_DB_MODIFY_SCRCOM.
           IF W_OK_CODE = 'PRI'.
              PERFORM P2000_PRINT_ZTBW USING
                                      ZTBWHD-ZFIVNO ZTBWHD-ZFGISEQ.
           ENDIF.
           CLEAR OK-CODE.
           PERFORM  P2000_SET_LOCK_MODE USING 'U'.
           PERFORM  P2000_SET_SCREEN_SCRCOM.
           LEAVE SCREEN.
        WHEN 'N'.              " No...
           MESSAGE  S957.
           CLEAR OK-CODE.
           IF W_STATUS NE C_REQ_D.
              PERFORM  P2000_SET_LOCK_MODE USING 'U'.
           ENDIF.
           PERFORM  P2000_SET_SCREEN_SCRCOM.
           LEAVE SCREEN.
        WHEN 'C'.              " Cancel
        WHEN OTHERS.
     ENDCASE.

  ELSE.
     CLEAR OK-CODE.
     PERFORM  P2000_SET_SCREEN_SCRCOM.
     LEAVE SCREEN.
  ENDIF.

ENDFORM.                    " P2000_EXIT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SET_MESSAGE USING     P_SY_UCOMM.

  W_OK_CODE =  P_SY_UCOMM.
  CASE P_SY_UCOMM.
    WHEN 'SAVE' OR 'PRI'.      " 저장?
      PERFORM  P2000_SAVE_MESSAGE.
    WHEN 'CANC'.      " 취소?
      PERFORM  P2000_CANCEL_MESSAGE.
    WHEN 'BACK' OR 'EXIT'.   " 앞으로 or 종료.
      PERFORM  P2000_EXIT_MESSAGE.
    WHEN 'DELE'.      " 삭제?
      PERFORM  P2000_DELETE_MESSAGE.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SAVE_MESSAGE.

  PERFORM P2000_MESSAGE_BOX USING '저장 확인'             " 타이틀...
                                  '입력된 내역을 저장합니다.'
                                  '저장하시겠습니까?' " Message #2
                                  'Y'                 " 취소 버?
                                  '1'.                      " default

ENDFORM.                    " P2000_SAVE_MESSAGE
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

  IF ANTWORT = 'C'.                                         " Cancel
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
*&      Form  P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_CANCEL_MESSAGE.

  PERFORM P2000_MESSAGE_BOX USING '취소 확인'             " 타이틀...
                                  '변경된 내용을 저장없이 종료됩니다.'
                                  '종료하시겠습니까?' " Message #2
                                  'N'                 " 취소 버?
                                  '2'.                      " default

  CASE ANTWORT.
    WHEN 'Y'.                                               " Yes...
      MESSAGE  S957.
      LEAVE TO SCREEN 0.  " " PROGRAM LEAVING
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_EXIT_MESSAGE.

  PERFORM P2000_MESSAGE_BOX USING '종료 확인'             " 타이틀...
                          '현재 입력내역을 저장하지 않습니다.'   "
                          '저장 후 종료하시겠습니까?'       " MSG2
                          'Y'                         " 취소 버튼 ?
                          '1'.                        " default button


ENDFORM.                    " P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_DELETE_MESSAGE.

   PERFORM P2000_MESSAGE_BOX USING '삭제 확인'             " 타이틀...
                          '현재 Document를 삭제합니다.'
                          '삭제하시겠습니까?'               " MSG2
                          'N'                 " 취소 버튼 유/?
                          '1'.                " default button


ENDFORM.                    " P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_DB_MODIFY_SCRCOM
*&---------------------------------------------------------------------*
FORM P3000_DB_MODIFY_SCRCOM.

   CASE SY-TCODE.
      WHEN 'ZIMBG1' OR 'ZIMBG2'.
         PERFORM P3000_CHARGE_DOC_MODIFY.
      WHEN OTHERS.
   ENDCASE.

ENDFORM.                    " P3000_DB_MODIFY_SCRCOM
*&---------------------------------------------------------------------*
*&      Form  P3000_CHARGE_DOC_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_CHARGE_DOC_MODIFY.


  CLEAR W_COUNT.
  LOOP AT IT_ZSBWIT.
    W_TABIX = SY-TABIX.
    W_COUNT = W_COUNT + 1.
    IF IT_ZSBWIT-GIMENGE IS INITIAL.
       DELETE IT_ZSBWIT INDEX W_TABIX.
    ENDIF.

  ENDLOOP.
  DESCRIBE TABLE IT_ZSBWIT LINES W_LINE.
  IF W_LINE EQ 0.
     MESSAGE E977 WITH 'Input quantity!'.
  ENDIF.

  CALL FUNCTION 'ZIM_BW_DOC_MODIFY'
      EXPORTING
             ZFIVNO        =   ZTBWHD-ZFIVNO
             ZFGISEQ       =   ZTBWHD-ZFGISEQ
             ZFSTATUS      =   W_STATUS
             W_ZTBWHD_OLD  =   *ZTBWHD
             W_ZTBWHD      =   ZTBWHD
             W_OK_CODE     =   W_OK_CODE
      TABLES
             IT_ZSBWIT     =   IT_ZSBWIT
             IT_ZSBWIT_OLD =   IT_ZSBWIT_OLD
      CHANGING
             P_ZFGISEQ       =   ZTBWHD-ZFGISEQ

      EXCEPTIONS
             ERROR_UPDATE  = 9
             NOT_MODIFY    = 8.

  IF SY-SUBRC EQ 0.
     MESSAGE S765.
     EXIT.
  ELSE.
     ROLLBACK WORK.
     MESSAGE S208.
     EXIT.
  ENDIF.

ENDFORM.                    " P3000_CHARGE_DOC_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_LOCK_MODE
*&---------------------------------------------------------------------*
FORM P2000_SET_LOCK_MODE USING     PA_MODE.

  CASE SY-TCODE.
     WHEN 'ZIMBG1' OR 'ZIMBG2' OR 'ZIMBG3'.    " CHARGE DOC.
        PERFORM P2000_SET_CHARGE_DOC_LOCK    USING   PA_MODE.
     WHEN OTHERS.
  ENDCASE.


ENDFORM.                    " P2000_SET_LOCK_MODE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_CHARGE_DOC_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_CHARGE_DOC_LOCK USING PA_MODE.


   IF PA_MODE EQ 'L'.
      CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBWHD'
         EXPORTING
             ZFIVNO   =  ZTBWHD-ZFIVNO
             ZFGISEQ  =  ZTBWHD-ZFGISEQ
         EXCEPTIONS
             OTHERS        = 1.

      IF SY-SUBRC <> 0.
         MESSAGE E510 WITH SY-MSGV1
                              'Bonded warehouse G/I Document'
                              ZTBWHD-ZFIVNO
                              ZTBWHD-ZFGISEQ.
*                     RAISING DOCUMENT_LOCKED.
      ENDIF.
   ELSE.
      CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBWHD'
         EXPORTING
             ZFIVNO   =  ZTBWHD-ZFIVNO
             ZFGISEQ  =  ZTBWHD-ZFGISEQ.

   ENDIF.

ENDFORM.                    " P2000_SET_CHARGE_DOC_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SCREEN_SCRCOM
*&---------------------------------------------------------------------*
FORM P2000_SET_SCREEN_SCRCOM.

  CASE SY-TCODE.
       WHEN 'ZIMBG1'.     SET SCREEN 0100.
       WHEN 'ZIMBG2'.     SET SCREEN 0200.
       WHEN 'ZIMBG3'.     SET SCREEN 0300.
       WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_SCREEN_SCRCOM
*&---------------------------------------------------------------------*
*&      Form  P2000_HEADER_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P2000_HEADER_CHANGE_DOC.

  CASE SY-DYNNR.
     WHEN '0101'.          "> CHARGE DOC.
         OBJECTCLASS   =   'ZTBWHD'.
         OBJEKTID      =   ZTBWHD+3(13).
     WHEN OTHERS.   EXIT.
  ENDCASE.

  SUBMIT  RCS00120 WITH  OBJEKT   =   OBJECTCLASS
                   WITH  OBJEKTID =   OBJEKTID
                   AND   RETURN.

ENDFORM.                    " P2000_HEADER_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_SELECT_ITEM
*&---------------------------------------------------------------------*
FORM P2000_SELECT_ITEM.

   CLEAR : W_COUNT.
   LOOP AT IT_ZSBWIT WHERE ZFMARK = 'X'.
      ADD 1   TO   W_COUNT.
      MOVE-CORRESPONDING IT_ZSBWIT  TO   ZTBWIT.
      MOVE: ZTBWHD-ZFIVNO           TO   ZTBWIT-ZFIVNO.
   ENDLOOP.

   CASE W_COUNT.
      WHEN 0.        MESSAGE W951.
      WHEN 1.
      WHEN OTHERS.   MESSAGE W965.
   ENDCASE.

ENDFORM.                    " P2000_SELECT_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_ITEM_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P2000_ITEM_CHANGE_DOC.

  CHECK : W_COUNT EQ 1.
  CASE SY-DYNNR.
     WHEN '0101'.          "> CHARGE DOC.
         OBJECTCLASS   =   'ZTBWIT'.
         OBJEKTID      =   ZTBWIT+3(18).
     WHEN OTHERS.   EXIT.
  ENDCASE.

  SUBMIT  RCS00120 WITH  OBJEKT   =   OBJECTCLASS
                   WITH  OBJEKTID =   OBJEKTID
                   AND   RETURN.

ENDFORM.                    " P2000_ITEM_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_PRINT_ZTBW
*&---------------------------------------------------------------------*
FORM P2000_PRINT_ZTBW USING    P_ZFIVNO
                               P_ZFGISEQ.

  SUBMIT ZRIMBWPRT WITH P_IVNO  EQ P_ZFIVNO
                   WITH P_GISEQ EQ P_ZFGISEQ
                   AND RETURN.

ENDFORM.                    " P2000_PRINT_ZTBW
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIV_TITLELIST
*&---------------------------------------------------------------------*
FORM P3000_ZTIV_TITLELIST.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-ULINE(61).
  WRITE : / SY-VLINE, '통관요청No',
            SY-VLINE, ' 통 관 일 ',
            SY-VLINE, '통관유형',
            SY-VLINE, '통관상태',
            SY-VLINE, '입고 상태',SY-VLINE.

  WRITE:/ SY-ULINE(61).

ENDFORM.                    " P3000_ZTIV_TITLELIST
