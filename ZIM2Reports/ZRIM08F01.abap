*&---------------------------------------------------------------------*
*& INCLUDE ZRIM08F01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 기납증 Sub-Module                                     *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.25                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_GUI_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SET_GUI_TEXT.

  CASE W_STATUS.
    WHEN C_REQ_C.   ASSIGN W_CREATE  TO <FS_F>.
    WHEN C_REQ_U.   ASSIGN W_CHANGE  TO <FS_F>.
    WHEN C_REQ_D.   ASSIGN W_DISPLAY TO <FS_F>.
    WHEN C_OPEN_C.  ASSIGN W_OPEN    TO <FS_F>.
    WHEN C_OPEN_U.  ASSIGN W_STAUTS  TO <FS_F>.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_GUI_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SET_PF_STATUS.

  CASE SY-TCODE.
    WHEN 'ZIMZ1' OR 'ZIMZ2' OR 'ZIMZ3'.
      MOVE 'REQ' TO W_PFSTAT.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " P2000_SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SET_MESSAGE USING    P_SY_UCOMM.

  CASE P_SY_UCOMM.
    WHEN 'ANZG'.      " CHANGE  ==> DISPLAY
      PERFORM  P2000_ANZG_MESSAGE.
    WHEN 'SAVE'.      " 저장?
      PERFORM  P2000_SAVE_MESSAGE.
    WHEN 'CANC'.      " 취소?
      PERFORM  P2000_CANCEL_MESSAGE.
    WHEN 'BACK' OR 'EXIT'.   " 앞으로 or 종?
      PERFORM  P2000_EXIT_MESSAGE.
    WHEN 'DELE'.      " 삭제?
      PERFORM  P2000_DELETE_MESSAGE.
    WHEN 'POST'.      " 전기.
      PERFORM  P2000_POSTING_MESSAGE.
    WHEN 'DCDE'.      " 전기취소.
      PERFORM  P2000_FI_CANCLE_MESSAGE.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_ANZG_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_ANZG_MESSAGE.

  PERFORM P2000_MESSAGE_BOX USING '이동 확인'             " 타이틀...
                                  '현재 입력내역을 저장하지 않습니다.'
                                  '저장 후 이동하시겠습니까?'" MSG2
                                  'Y'                 " 취소 버?
                                  '1'.                      " default


ENDFORM.                    " P2000_ANZG_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SAVE_MESSAGE.

  PERFORM P2000_MESSAGE_BOX USING '저장 확인'             " 타이틀...
                                  '입력된 내역을 저장합니다.'
                                  '저장하시겠습니까?' " Message #2
                                  'Y'                 " 취소 버?
                                  '1'.                      " default

ENDFORM.                    " P2000_SAVE_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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
*&      Form  P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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

  IF OK-CODE EQ 'POST'.
     PERFORM P2000_SET_POST_VALUE.
     CALL SCREEN 0020   STARTING AT 7   5
                        ENDING  AT 118 20.
  ELSEIF OK-CODE EQ 'DCDE'.
     IF BSIS-BUDAT IS INITIAL.
        BSIS-BUDAT = SY-DATUM.
     ENDIF.

     CALL SCREEN 0030 STARTING AT 30 2
                      ENDING   AT 78 11.
  ELSE.
     CALL SCREEN 0001 STARTING AT 30 6
                      ENDING   AT 78 10.
  ENDIF.

  IF ANTWORT = 'C'.                                         " Cancel
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_MESSAGE_BOX

*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_EXIT_MESSAGE.

  PERFORM P2000_MESSAGE_BOX USING '종 확인'             " 타이틀...
                          '현재 입력내역을 저장하지 않습니다.'   "
                          '저장 후 종료하시겠습니까?'       " MSG2
                          'Y'                         " 취소 버튼 ?
                          '1'.                        " default button

ENDFORM.                    " P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_DELETE_MESSAGE.

  PERFORM P2000_MESSAGE_BOX USING '삭제 확인'             " 타이틀...
                          '현재 Document를 삭제합니다.'
                          '삭제하시겠습니까?'               " MSG2
                          'N'                 " 취소 버튼 유/?
                          '1'.                " default button

ENDFORM.                    " P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_OK_CODE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_OK_CODE_PROCESS.

  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'ENTR' OR 'DDLC'.
    WHEN 'COPY'.
    WHEN 'CRDC'.          " 생성.
      CASE SY-TCODE.
        WHEN 'ZIMZ1' OR 'ZIMZ2' OR 'ZIMZ3'.
          LEAVE TO TRANSACTION 'ZIMZ1'.
      ENDCASE.
    WHEN 'CHDC'.          " 변경.
      CASE SY-TCODE.
        WHEN 'ZIMZ1' OR 'ZIMZ2' OR 'ZIMZ3'.
          LEAVE TO TRANSACTION 'ZIMZ2'.
      ENDCASE.
    WHEN 'DISP'.
      CASE SY-TCODE.
        WHEN 'ZIMZ1' OR 'ZIMZ2' OR 'ZIMZ3'.
          LEAVE TO  TRANSACTION 'ZIMZ3'.
      ENDCASE.
    WHEN OTHERS.
      LEAVE TO TRANSACTION OK-CODE.
  ENDCASE.

ENDFORM.                    " P2000_OK_CODE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_DOC_ITEM_SELECT.

  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFTBNO  = ZSREQHD-ZFTBNO.
* INPUT VALUE 보존을 위해...
  W_ZSREQHD = ZSREQHD.

  DESCRIBE TABLE IT_ZSTAXBKHD LINES TFILL.
  IF TFILL = 0.   MESSAGE E406.   ENDIF.

  W_STATUS_CHK = 'C'.
  INCLUDE = 'TAXBKPO'.                 " L/C 변?
  CALL SCREEN 0014 STARTING AT  10 3
                   ENDING   AT  90 15.

ENDFORM.                    " P2000_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_LISTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_DATA_LISTING.

  IF INCLUDE EQ 'TAXBKPO'.
     LOOP AT IT_ZSTAXBKHD.
       W_MOD = SY-TABIX MOD 2.
       PERFORM   P2000_TAXPO_DISPLAY_LIST.
       HIDE:IT_ZSREQHD, IT_ZSTAXBKHD.
     ENDLOOP.
  ELSEIF INCLUDE EQ 'POPU'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / SY-ULINE(96), / SY-VLINE NO-GAP,
                '유형'   NO-GAP, SY-VLINE NO-GAP,
*                   ' 문서번호 ' NO-GAP, SY-VLINE NO-GAP,
                '메세지 텍스트', 94 SY-VLINE NO-GAP,
                'T'      NO-GAP, SY-VLINE,
              / SY-ULINE(96).
*         MESSAGE
      LOOP AT IT_ERR_LIST.
         W_MOD  =  SY-TABIX MOD 2.
         FORMAT RESET.
         IF W_MOD EQ 0.
            FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
         ELSE.
            FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
         ENDIF.
         WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4)     NO-GAP,
*                      SY-VLINE NO-GAP, IT_ERR_LIST-BELNR       NO-GAP,
                   SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(87) NO-GAP,
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
      WRITE : / SY-ULINE(96).
      CLEAR : IT_ERR_LIST.
  ELSE.
     LOOP AT IT_ZSREQHD.
       W_MOD = SY-TABIX MOD 2.
       CASE INCLUDE.
         WHEN 'LCCHANGE'.    " L/C  변경시 ( P/O번호 중복시 )
           PERFORM   P2000_LC_CHANGE_LIST.
         WHEN 'LCDISPLY'.    " L/C  조회시 ( L/C번호 중복시 )
           PERFORM   P2000_LC_DISPLAY_LIST.
       ENDCASE.
       HIDE:IT_ZSREQHD, IT_ZSTAXBKHD.
     ENDLOOP.
   ENDIF.
   CLEAR : IT_ZSREQHD, IT_ZSTAXBKHD.
ENDFORM.                    " P2000_DATA_LISTING

*&---------------------------------------------------------------------*
*&      Form  P2000_LC_CHANGE_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_LC_CHANGE_LIST.

  WRITE : / IT_ZSREQHD-ZFREQNO NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZSREQHD-ZFAMDNO NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFREQTY NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-EKORG   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-EKGRP   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-WAERS   NO-GAP,    SY-VLINE NO-GAP,
*          IT_ZSREQHD-ZFLASTAM CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          IT_ZSREQHD-ZFOPAMT CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFREQDT NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRVDT  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFDOCST NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRLST1 NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRLST2 NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRTNYN NO-GAP.

ENDFORM.                    " P2000_LC_CHANGE_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_LC_DISPLAY_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_LC_DISPLAY_LIST.

  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : / IT_ZSREQHD-EBELN   NO-GAP,    SY-VLINE NO-GAP,
            IT_ZSREQHD-ZFREQNO NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZSREQHD-ZFAMDNO NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFREQTY NO-GAP,    SY-VLINE NO-GAP,
*         IT_ZSREQHD-LIFNR   NO-GAP,    SY-VLINE NO-GAP,
*         IT_ZSREQHD-NAME1   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-EKORG   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-EKGRP   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-WAERS   NO-GAP,    SY-VLINE NO-GAP,
*          IT_ZSREQHD-ZFLASTAM CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          IT_ZSREQHD-ZFOPAMT CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFREQDT NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRVDT  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFDOCST NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRLST1 NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRLST2 NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRTNYN NO-GAP.

ENDFORM.                    " P2000_LC_DISPLAY_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_SEARCH_FIELD_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SEARCH_FIELD_MOVE.

  ZSREQHD-ZFREQNO = W_ZFREQNO.
  ZSREQHD-ZFAMDNO = W_ZFAMDNO.
  ZSREQHD-ZFOPNNO = W_ZFOPNNO.
  ZSREQHD-EBELN   = W_EBELN.
  ZSREQHD-ZFINSEQ = W_ZFINSEQ.
  ZSREQHD-ZFTBNO  = W_ZFTBNO.

ENDFORM.                    " P2000_SEARCH_FIELD_MOVE
*&---------------------------------------------------------------------*
*&      Form  P1000_REQ_DOC_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_REQ_DOC_READ.
  W_READ_CHK = 'N'.

  IF NOT ZSREQHD-ZFREQNO IS INITIAL.
* 수입의뢰 문서 조회.
    PERFORM   P1000_READ_REQ_DOC.

*  ELSE.
*    MESSAGE E000.
  ENDIF.

ENDFORM.                    " P1000_REQ_DOC_READ

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_REQ_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_REQ_DOC.

   SELECT MAX( ZFAMDNO ) INTO ZSREQHD-ZFAMDNO FROM ZTREQST
                         WHERE ZFREQNO  EQ    ZSREQHD-ZFREQNO.
   W_AMDNO   =    ZSREQHD-ZFAMDNO.

* 수입의뢰 Header Table
  CALL FUNCTION 'ZIM_GET_REQ_DOC_HEADER'
       EXPORTING
            ZFREQNO         = ZSREQHD-ZFREQNO
            ZFAMDNO         = W_AMDNO
       IMPORTING
            W_ZTREQHD       = ZTREQHD
            W_ZTREQST       = ZTREQST
       TABLES
            IT_ZTREQORJ     = IT_ZTREQORJ
            IT_ZTREQORJ_ORG = IT_ZTREQORJ_ORG
            IT_ZSREQIL      = IT_ZSREQIL
            IT_ZSREQIL_ORG  = IT_ZSREQIL_ORG
       EXCEPTIONS
            NOT_FOUND       = 4
            NOT_INPUT       = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E018 WITH ZSREQHD-ZFREQNO.
    WHEN 8.
      MESSAGE E019.
  ENDCASE.

* 문서 상태 체크.
  CASE SY-TCODE.
    WHEN 'ZIMZ1' OR 'ZIMZ2' OR 'ZIMZ3'.
      PERFORM   P2000_CHECK_DOC_STATUS.
    WHEN OTHERS.
*        EXIT.
  ENDCASE.


* P/O Header Select
  SELECT SINGLE * FROM EKKO WHERE EBELN EQ ZTREQHD-EBELN.
*----------------------------------------------------------------------
* 릴리즈 체크.
*----------------------------------------------------------------------
*  IF NOT ( EKKO-FRGKE EQ '2' OR EKKO-FRGKE EQ SPACE ).
  IF EKKO-FRGRL EQ 'X'.
    MESSAGE E390(ME) WITH ZSREQHD-EBELN.
  ENDIF.
* CLOSE 구분 체크.
  IF ZTREQHD-ZFCLOSE NE SPACE.
    IF NOT ZTREQHD-ZFSUBYN IS INITIAL.
      PERFORM   GET_DD07T_SELECT USING      'ZESUBYN'
                                            ZTREQHD-ZFSUBYN
                                 CHANGING   W_TEXT40.
      IF W_STATUS EQ C_REQ_D.
        MESSAGE W254 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_TEXT40.
      ELSE.
        MESSAGE E254 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_TEXT40.
      ENDIF.
    ELSE.
      IF W_STATUS EQ C_REQ_D.
        MESSAGE W255 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
      ELSE.
        MESSAGE E255 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
      ENDIF.
    ENDIF.
  ENDIF.

* 수입문서의 릴리즈(승인) 여부를 검증함.( 릴리즈만 진행 가능 )
  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
    MESSAGE E963.
  ENDIF.

*>> 생성일 경우....
  IF SY-TCODE EQ 'ZIMZ1'.
* 수입 품목 ITEM SELECT( ZTREQIT )
     CALL FUNCTION 'ZIM_GET_REQ_DOC_ITEM'
         EXPORTING
            EBELN            =   ZTREQHD-EBELN
            KNUMV            =   EKKO-KNUMV
*            KPOSN            =   '000000'
            KSCHL            =   ZTIMIMG00-ZFKSCHL3  ">Installing CHG.
            ZFREQNO          =   ZSREQHD-ZFREQNO
         IMPORTING
            W_ITEM_CNT      =   W_ITEM_CNT
            W_TOT_AMOUNT    =   W_TOT_AMOUNT
         TABLES
            IT_ZSREQIT      =   IT_ZSREQIT
            IT_ZSREQIT_ORG  =   IT_ZSREQIT_ORG
         EXCEPTIONS
            NOT_FOUND      =   1
            NOT_INPUT      =   2
            NO_REFERENCE   =   3
            NO_AMOUNT      =   4.

     CASE SY-SUBRC.
       WHEN 1.    MESSAGE W026 WITH ZSREQHD-ZFREQNO.
       WHEN 2.    MESSAGE E019.
       WHEN 3.    MESSAGE W026 WITH ZSREQHD-ZFREQNO.
       WHEN 4.    MESSAGE W027 WITH ZSREQHD-ZFREQNO.
     ENDCASE.
  ENDIF.

*> 송장내역 검증.
  IF ZTIMIMG00-LOGRIV EQ 'A' OR ZTIMIMG00-LOGRIV EQ 'I'.
     SELECT COUNT( * ) INTO W_COUNT1
            FROM  ZTCIVHD
            WHERE ZFCIVRN  IN
                ( SELECT ZFCIVRN FROM  ZTCIVIT
                                 WHERE ZFREQNO EQ  ZSREQHD-ZFREQNO )
            AND   ZFIVST   EQ 'Y'.

     IF W_COUNT1 LE 0.
        MESSAGE E681.
     ENDIF.
  ELSE.
     IF EKKO-BSTYP EQ 'K'.   ">일괄계약.
        SELECT * INTO TABLE IT_EKAB
               FROM  EKAB
               FOR ALL ENTRIES IN IT_ZSREQIT
               WHERE KONNR EQ IT_ZSREQIT-EBELN
               AND   KTPNR EQ IT_ZSREQIT-EBELP.
        IF SY-SUBRC NE 0.
           MESSAGE E681.
        ENDIF.

        SELECT * FROM   EKBE UP TO 1 ROWS
                 FOR ALL ENTRIES IN IT_EKAB
                 WHERE  EBELN EQ IT_EKAB-EBELN
                 AND    EBELP EQ IT_EKAB-EBELP
                 AND    VGABE EQ '2'
                 AND    BEWTP EQ 'Q'.
        ENDSELECT.
        IF SY-SUBRC NE 0.
*     IF W_COUNT1 LE 0.
           MESSAGE E681.
        ENDIF.

     ELSE.
        SELECT * FROM   EKBE UP TO 1 ROWS
                 FOR ALL ENTRIES IN IT_ZSREQIT
                 WHERE  EBELN EQ IT_ZSREQIT-EBELN
                 AND    EBELP EQ IT_ZSREQIT-EBELP
                 AND    VGABE EQ '2'
                 AND    BEWTP EQ 'Q'.
        ENDSELECT.
        IF SY-SUBRC NE 0.
*     IF W_COUNT1 LE 0.
           MESSAGE E681.
        ENDIF.

     ENDIF.
  ENDIF.

ENDFORM.                    " P1000_READ_REQ_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_OPEN_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_OPEN_DOC_ITEM_SELECT.

  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZVREQHD_ST
           WHERE  ZFOPNNO   EQ ZSREQHD-ZFOPNNO
           AND    ZFAMDNO EQ '00000'
           ORDER  BY ZFREQNO.

*----------------------------------------------------------------------
* position
*----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.

  W_STATUS_CHK = 'C'.
  INCLUDE = 'LCDISPLY'.                 " L/C 변?
  CALL SCREEN 0014 STARTING AT  04 3
                   ENDING   AT  97 16.

ENDFORM.                    " P2000_OPEN_DOC_ITEM_SELECT

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_REQ_SCR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SET_REQ_SCR.

  CASE SY-TCODE.
    WHEN 'ZIMZ1'.
      W_STATUS = C_REQ_C.
    WHEN 'ZIMZ2'.
      W_STATUS = C_REQ_U.
    WHEN 'ZIMZ3'.
      W_STATUS = C_REQ_D.
  ENDCASE.

  SET SCREEN 0110.  LEAVE TO SCREEN 0110.

ENDFORM.                    " P2000_SET_REQ_SCR
*&---------------------------------------------------------------------*
*&      Form  GET_DD07T_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DD07T_SELECT USING    P_DOMNAME
                               P_FIELD
                      CHANGING P_W_NAME.
  CLEAR : DD07T, P_W_NAME.
  IF P_FIELD IS INITIAL.   EXIT.   ENDIF.

  SELECT * FROM DD07T WHERE DOMNAME     EQ P_DOMNAME
                      AND   DDLANGUAGE  EQ SY-LANGU
                      AND   AS4LOCAL    EQ 'A'
                      AND   DOMVALUE_L  EQ P_FIELD
                      ORDER BY AS4VERS DESCENDING.
    EXIT.
  ENDSELECT.
*   TRANSLATE DD07T-DDTEXT TO UPPER CASE.
  P_W_NAME   = DD07T-DDTEXT.
  TRANSLATE P_W_NAME TO UPPER CASE.

ENDFORM.                    " GET_DD07T_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_CHECK_DOC_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_CHECK_DOC_STATUS.

  CASE ZTREQHD-ZFREQTY.
     WHEN 'LO' OR 'PU'.
     WHEN OTHERS.
        MESSAGE E678 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                          ZTREQHD-ZFREQTY.
  ENDCASE.

  CASE ZTREQST-ZFDOCST.
     WHEN 'O'.
     WHEN OTHERS.
        PERFORM   GET_DD07T_SELECT USING  'ZDDOCST'  ZTREQST-ZFDOCST
                                   CHANGING  W_TEXT12.
        MESSAGE E677 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_TEXT12.
  ENDCASE.

ENDFORM.                    " P2000_CHECK_DOC_STATUS
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_REQDOC_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SET_REQDOC_LOCK USING     PA_MODE.

  IF PA_MODE EQ 'L'.
     CALL FUNCTION 'ENQUEUE_EZ_ZTTAXBKHD'
          EXPORTING
               ZFTBNO = ZTTAXBKHD-ZFTBNO
          EXCEPTIONS
               OTHERS  = 1.
     IF SY-SUBRC <> 0.
        MESSAGE E510 WITH SY-MSGV1 'CTM'
                          ZTTAXBKHD-ZFTBNO SPACE
                     RAISING DOCUMENT_LOCKED.
     ENDIF.

  ELSE.
     CALL FUNCTION 'DEQUEUE_EZ_ZTTAXBKHD'
          EXPORTING
               ZFTBNO  = ZTTAXBKHD-ZFTBNO.
  ENDIF.

ENDFORM.                    " P2000_SET_REQDOC_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_STATUS_SCR_DISABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_STATUS_SCR_DISABLE.

  CASE SY-DYNNR.
    WHEN   '0100'.                " 수입의뢰 생성 초기화면.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'REQ' WITH W_CREATE 'Initial screen'.
      W_STATUS = C_REQ_C.
    WHEN   '0200'.                " 수입의뢰 변경 초기화면.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      IF SY-TCODE EQ 'ZIMZ2'.
         SET TITLEBAR  'REQ' WITH W_CHANGE 'Initial screen'.
         W_STATUS = C_REQ_U.
      ELSE.
         SET TITLEBAR  'REQ' WITH W_DISPLAY 'Initial screen'.
         W_STATUS = C_REQ_D.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_STATUS_SCR_DISABLE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_STATUS_TCODE_DISABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_STATUS_TCODE_DISABLE.
  CASE SY-TCODE.
     WHEN 'ZIMZ1'.
        MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 생성.
        MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 삭제.
        MOVE 'HIST' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " HEAD CHAN
        MOVE 'HIIT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ITEM CHAN
*        MOVE 'POST' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 전기.
     WHEN 'ZIMZ2'.
        MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 변경.
     WHEN 'ZIMZ3'.
        MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 조회.
        MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 저장.
     WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_STATUS_TCODE_DISABLE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_REQ_INIT_SCR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SET_REQ_INIT_SCR.
* document
  MOVE 'HIST' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " HEADER CHANGE DOC
  MOVE 'HIIT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ITEM CHANGE DOC.
  MOVE 'ZIMG' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " IMPORT IMG
  MOVE 'DDLC' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " DOUBLE CLICK
  MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 저장.
  MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 다른문서.
  MOVE 'POST' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " POSTING.
  MOVE 'DCDE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 전기취소..
  MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " 삭제.
*  ENDIF.
  MOVE 'ME23'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. "> P.O.
  MOVE 'MK03'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Vendor
  MOVE 'MK23'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. "
  MOVE 'ZIM03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 수입의뢰.
  MOVE 'ZIM93' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " L/C history
  MOVE 'ZIBK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 개설은?
  MOVE 'ZIBE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Beneficiary
  MOVE 'ZICI'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 차입기?
* item
  MOVE 'MM03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 자?
  MOVE 'MD04' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 자재/소요?
  MOVE 'MMBE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 자재 개?
  MOVE 'MB51' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 자재별 자재문?
  MOVE 'ME2M' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 자재별 구매문?
  MOVE 'ME03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 소스 리스?
* env
  MOVE 'FB03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">전기조회..
  MOVE 'ZIMY3' TO IT_EXCL-FCODE.   APPEND IT_EXCL. ">비용문서..

ENDFORM.                    " P2000_SET_REQ_INIT_SCR
*&---------------------------------------------------------------------*
*&      Form  P3000_MOVE_REQ_TO_TAXBK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_MOVE_REQ_TO_TAXBK.

  DATA : L_NAME1(255),
         L_NAME2(255),
         L_NAME3(255),
         L_NAME4(255),
         L_ZFTDTC(12).

   CHECK : W_STATUS NE C_REQ_D.
   CLEAR : ZTTAXBKHD, *ZTTAXBKHD.
   REFRESH : IT_ZSTAXBKIT, IT_ZSTAXBKIT_ORG.

   CALL FUNCTION 'ZIM_GET_COMPANY_DATA'
        EXPORTING
           BUKRS       =    ZTREQHD-BUKRS
           IMTRD       =    ZTREQHD-IMTRD
        IMPORTING
           XT001       =    T001
           XZTIMIMG00  =    ZTIMIMG00
           XZTIMIMGTX  =    ZTIMIMGTX
        EXCEPTIONS
           NOT_FOUND   =    4.

*> 대표 플랜트...
   SELECT * FROM ZTIMIMG02 UP TO 1 ROWS
            WHERE ZFWERKS EQ ZTREQHD-ZFWERKS.
   ENDSELECT.

   CALL FUNCTION 'ZIM_GET_VENDOR_ADDRESS_FORMAT'
        EXPORTING
             LIFNR     = ZTREQHD-ZFBENI
        IMPORTING
             NAME1     = L_NAME1
             NAME2     = L_NAME2
             NAME3     = L_NAME3
             NAME4     = L_NAME4
             P_LFA1    = W_LFA1
             P_ADRC    = W_ADRC
        EXCEPTIONS
             NO_INPUT  = 01
             NOT_FOUND = 03.

   CASE SY-SUBRC.
      WHEN 01.     MESSAGE I025.
      WHEN 03.     MESSAGE E020   WITH    ZTREQHD-ZFBENI.
   ENDCASE.

   TRANSLATE : L_NAME1 TO UPPER CASE,
               L_NAME2 TO UPPER CASE,
               L_NAME3 TO UPPER CASE,
               L_NAME4 TO UPPER CASE.

   MOVE : L_NAME1      TO W_LFA1-NAME1,
          L_NAME2      TO W_LFA1-NAME2,
          L_NAME3      TO W_LFA1-NAME3,
          L_NAME4      TO W_LFA1-NAME4.

*> HEADER DATA MOVE.
   IF ZTIMIMG02-ZFTDAD1  IS INITIAL AND
      ZTIMIMG02-ZFTDAD2  IS INITIAL.
*      MOVE ZTIMIMGTX-ZFTDNM2 TO   ZTTAXBKHD-ZFSELAD.
   ELSE.
      CONCATENATE ZTIMIMG02-ZFTDAD1 ZTIMIMG02-ZFTDAD2
                  INTO ZTTAXBKHD-ZFSELAD
                  SEPARATED BY SPACE.
   ENDIF.

   CONCATENATE ZTIMIMG02-ZFTDTC(3) ZTIMIMG02-ZFTDTC+4(2)
               ZTIMIMG02-ZFTDTC+7(5) INTO L_ZFTDTC.

   MOVE :  SY-MANDT           TO   ZTTAXBKHD-MANDT,    "클라이언트.
           'G'                TO   ZTTAXBKHD-ZFDCGB,   ">문서종류.
           ZTREQHD-ZFBENI     TO   ZTTAXBKHD-ZFTRNCD,  ">양도자 코드.
           W_LFA1-NAME1       TO   ZTTAXBKHD-ZFTRNSNM, "->상호.
           W_LFA1-STCD2       TO   ZTTAXBKHD-ZFTRNSTCD,"->사업자번호.
           W_LFA1-NAME2       TO   ZTTAXBKHD-ZFTRNSAD, "->주소.
*           W_LFA1-STCD4       TO   ZTTAXBKHD-ZFTRNSRNM,"->대표자명.
           W_LFA1-J_1KFREPRE  TO   ZTTAXBKHD-ZFTRNSRNM,"->대표자명.
           ZTREQHD-BUKRS      TO   ZTTAXBKHD-BUKRS,    ">회사코드.
           ZTREQHD-EBELN      TO   ZTTAXBKHD-EBELN,    ">구매문서.
           ZTREQHD-ZFREQNO    TO   ZTTAXBKHD-ZFREQNO,  ">수입의뢰.
           ZTIMIMGTX-ZFAPPNML TO   ZTTAXBKHD-ZFSELSNM, "-->상호.
*          ZSIMIMG02-ZFTDAD1  TO   ZTTAXBKHD-ZFSELAD,  "-->주소.
           ZTIMIMGTX-ZFTDNM2  TO   ZTTAXBKHD-ZFSELRNM, "-->대표자성명.
*           ZTIMIMG02-ZFTDTC   TO   ZTTAXBKHD-ZFSELSTCD,"-->사업자번호.
           L_ZFTDTC           TO   ZTTAXBKHD-ZFSELSTCD,"-->사업자번호.
           ZTIMIMG02-ZFTDNO   TO   ZTTAXBKHD-ZFSELCCNO,"-->통관고유부호.
           '3'                TO   ZTTAXBKHD-PROOFCD,  ">증명구분.
           ZTREQST-ZFOPNNO    TO   ZTTAXBKHD-BASISNO,  ">근거서류번호.
           SY-DATUM           TO   ZTTAXBKHD-ZFRVDT,   ">접수일자.
           T001-WAERS         TO   ZTTAXBKHD-ZFKRW,    ">원화통화.
           ZTREQHD-WAERS      TO   ZTTAXBKHD-WAERS,    ">통화코드.
           'KG'               TO   ZTTAXBKHD-MEINS,    ">단위.
           ZTREQST-ZFOPNDT    TO   ZTTAXBKHD-ZFBUYDT,  ">양도(매입)일자.
           '02'               TO   ZTTAXBKHD-ZFCCMK,   ">부호.
           'N'                TO   ZTTAXBKHD-ZFPOSYN.

*> ITEM 수량 MOVE.
   PERFORM   P3000_REQIT_MENGE_MOVE.

ENDFORM.                    " P3000_MOVE_REQ_TO_TAXBK
*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_EXIT_PROCESS.

   IF SY-TCODE EQ 'ZIMZ3'.
      CLEAR OK-CODE.
      PERFORM  P2000_SET_SCREEN_SCRCOM.
      LEAVE SCREEN.
      EXIT.
   ENDIF.

    W_OK_CODE = OK-CODE.
    PERFORM P2000_SET_MESSAGE USING  OK-CODE.
    CASE ANTWORT.
      WHEN 'Y'.                                             " Yes...
*----------------------------------------------------------------------
* database WRITE
*----------------------------------------------------------------------
        PERFORM  P2000_COMPARE_ITEM_HEADER.
        PERFORM  P3000_DB_MODIFY_SCRCOM.
        IF W_SUBRC EQ 0.
           COMMIT WORK.
        ENDIF.
        CLEAR OK-CODE.
        IF W_STATUS EQ C_REQ_U .
           PERFORM  P2000_SET_REQDOC_LOCK USING 'U'.
        ENDIF.
        PERFORM  P2000_SET_SCREEN_SCRCOM.
        LEAVE SCREEN.
      WHEN 'N'.              " No...
        MESSAGE  S957.
        CLEAR OK-CODE.
        IF W_STATUS EQ C_REQ_U .
           PERFORM  P2000_SET_REQDOC_LOCK USING 'U'.
        ENDIF.
        PERFORM  P2000_SET_SCREEN_SCRCOM.
        LEAVE SCREEN.
      WHEN 'C'.                                             " Cancel
      WHEN OTHERS.
    ENDCASE.

ENDFORM.                    " P2000_EXIT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_HEADER_CHANGE_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_HEADER_CHANGE_DOC.

  CASE SY-DYNNR.
     WHEN '0110'.          "> CHARGE DOC.
         OBJECTCLASS   =   'ZTTAXBKHD'.
         OBJEKTID      =   ZTTAXBKHD+3(10).
     WHEN OTHERS.   EXIT.
  ENDCASE.

  SUBMIT  RCS00120 WITH  OBJEKT   =   OBJECTCLASS
                   WITH  OBJEKTID =   OBJEKTID
                   AND   RETURN.

ENDFORM.                    " P2000_HEADER_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_SELECT_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SELECT_ITEM.

   CLEAR : W_COUNT.
   LOOP AT IT_ZSTAXBKIT WHERE ZFMARK = 'X'.
      W_TABIX = SY-TABIX.
      ADD 1   TO   W_COUNT.
      MOVE-CORRESPONDING IT_ZSTAXBKIT  TO   ZTTAXBKIT.
*      MOVE : ZTBKPF-BUKRS              TO   ZTBSEG-BUKRS,
*             ZTBKPF-GJAHR              TO   ZTBSEG-GJAHR,
*             ZTBKPF-BELNR              TO   ZTBSEG-BELNR.
   ENDLOOP.

   CASE W_COUNT.
      WHEN 0.        MESSAGE S951. EXIT.
      WHEN 1.
      WHEN OTHERS.   MESSAGE S965. EXIT.
   ENDCASE.

ENDFORM.                    " P2000_SELECT_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_ITEM_CHANGE_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_ITEM_CHANGE_DOC.

  CHECK : W_COUNT EQ 1.

  CASE SY-DYNNR.
     WHEN '0110'.          "> CHARGE DOC.
         OBJECTCLASS   =   'ZTTAXBKIT'.
         OBJEKTID      =   ZTTAXBKIT+3(15).
     WHEN OTHERS.   EXIT.
  ENDCASE.

  SUBMIT  RCS00120 WITH  OBJEKT   =   OBJECTCLASS
                   WITH  OBJEKTID =   OBJEKTID
                   AND   RETURN.

ENDFORM.                    " P2000_ITEM_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SCREEN_SCRCOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_SCREEN_SCRCOM.
* CALL Transaction일 경우, 프로그램 Leave
  IF SY-CALLD EQ 'X'.
    LEAVE.
  ENDIF.

  CASE W_STATUS.
    WHEN C_REQ_C.
*       PERFORM   P2000_PO_UNLOCK.
       SET SCREEN 100.           " 생성.
    WHEN C_REQ_U.                " 변경.
*       PERFORM   P2000_PO_UNLOCK.
       SET SCREEN 200.
    WHEN C_REQ_D.                " 조회.
       SET SCREEN 200.
    WHEN OTHERS.
      SET SCREEN 0.
  ENDCASE.

ENDFORM.                    " P2000_SET_SCREEN_SCRCOM
*&---------------------------------------------------------------------*
*&      Form  P3000_DB_MODIFY_SCRCOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_DB_MODIFY_SCRCOM.

  IF W_STATUS EQ C_REQ_C.
     PERFORM   P2000_GET_NUMBER_NEXT(SAPMZIM00)
               USING 'TB' ZTTAXBKHD-ZFTBNO.
  ENDIF.

  CALL FUNCTION 'ZIM_ZTTAXBOOK_DOC_MODIFY'
       EXPORTING
            W_OK_CODE        = W_OK_CODE
            ZFSTATUS         = W_STATUS
            ZFTBNO           = ZTTAXBKHD-ZFTBNO
            NZTTAXBKHD       = ZTTAXBKHD
            OZTTAXBKHD       = *ZTTAXBKHD
       TABLES
            IT_ZSTAXBKIT     = IT_ZSTAXBKIT
            IT_ZSTAXBKIT_ORG = IT_ZSTAXBKIT_ORG
       EXCEPTIONS
            ERROR_UPDATE     = 4.

  W_SUBRC = SY-SUBRC.

  IF W_SUBRC EQ  0.
     SET PARAMETER ID 'ZPTBNO' FIELD ZTTAXBKHD-ZFTBNO.
     MESSAGE  S688 WITH ZTTAXBKHD-ZFTBNO.
     COMMIT WORK.
  ELSE.
     ROLLBACK WORK.
     MESSAGE  E952.
  ENDIF.

ENDFORM.                    " P3000_DB_MODIFY_SCRCOM
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTO_CALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_AUTO_CALC.

*   IF ZTTAXBKHD-ZFBUYMN IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTAXBKHD' 'ZFBUYMN'.
*   ENDIF.
*
*   IF ZTTAXBKHD-ZFTBAK IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTAXBKHD' 'ZFTBAK'.
*   ENDIF.
*
*   IF ZTTAXBKHD-ZFCUAMT IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTAXBKHD' 'ZFCUAMT'.
*   ENDIF.

*   ZTTAXBKHD-ZFTXAMTS = ZTTAXBKHD-ZFCUAMT
*                      + ZTTAXBKHD-ZFHMAMT
*                      + ZTTAXBKHD-ZFEDAMT
*                      + ZTTAXBKHD-ZFAGAMT.


*   PERFORM  P2000_PRE_AUTO_CALC.
*
**> 금액 배부 작업.
*   LOOP AT IT_ZSTAXBKIT.
*      W_TABIX = SY-TABIX.
*
*      IF IT_ZSTAXBKIT-BKMENGE GT 0.
*
*         L_TABIX = W_TABIX.
*         L_RATE = ( IT_ZSTAXBKIT-BKMENGE / ZTTAXBKHD-ZFBUYMN ).
*
*         IT_ZSTAXBKIT-ZFTBAK  = ZTTAXBKHD-ZFTBAK  * L_RATE.
*         IT_ZSTAXBKIT-ZFCUAMT = ZTTAXBKHD-ZFCUAMT * L_RATE.
*         IT_ZSTAXBKIT-ZFHMAMT = ZTTAXBKHD-ZFHMAMT * L_RATE.
*         IT_ZSTAXBKIT-ZFEDAMT = ZTTAXBKHD-ZFEDAMT * L_RATE.
*         IT_ZSTAXBKIT-ZFAGAMT = ZTTAXBKHD-ZFAGAMT * L_RATE.
*
*         IT_ZSTAXBKIT-ZFTXAMTS = IT_ZSTAXBKIT-ZFCUAMT
*                               + IT_ZSTAXBKIT-ZFHMAMT
*                               + IT_ZSTAXBKIT-ZFEDAMT
*                               + IT_ZSTAXBKIT-ZFAGAMT.
*
*         MODIFY IT_ZSTAXBKIT INDEX W_TABIX.
*
*         ADD IT_ZSTAXBKIT-ZFTBAK   TO L_ZFTBAK.
*         ADD IT_ZSTAXBKIT-ZFCUAMT  TO L_ZFCUAMT.
*         ADD IT_ZSTAXBKIT-ZFHMAMT  TO L_ZFHMAMT.
*         ADD IT_ZSTAXBKIT-ZFEDAMT  TO L_ZFEDAMT.
*         ADD IT_ZSTAXBKIT-ZFAGAMT  TO L_ZFAGAMT.
*         ADD IT_ZSTAXBKIT-ZFTXAMTS TO L_ZFTXAMTS.
*         IT_ZSTAXBKIT-ZFMARK = 'X'.
*      ELSE.
*         CLEAR : IT_ZSTAXBKIT-ZFMARK.
*      ENDIF.
*   ENDLOOP.
*
*   IF L_ZFTBAK   NE ZTTAXBKHD-ZFTBAK  OR
*      L_ZFCUAMT  NE ZTTAXBKHD-ZFCUAMT OR
*      L_ZFHMAMT  NE ZTTAXBKHD-ZFHMAMT OR
*      L_ZFEDAMT  NE ZTTAXBKHD-ZFEDAMT OR
*      L_ZFAGAMT  NE ZTTAXBKHD-ZFAGAMT OR
*      L_ZFTXAMTS NE ZTTAXBKHD-ZFTXAMTS.
*
*      READ TABLE IT_ZSTAXBKIT INDEX L_TABIX.
*
*      IF L_ZFTBAK GT ZTTAXBKHD-ZFTBAK.
*         IT_ZSTAXBKIT-ZFTBAK = IT_ZSTAXBKIT-ZFTBAK
*                             - ( L_ZFTBAK - ZTTAXBKHD-ZFTBAK ).
*      ELSEIF ZTTAXBKHD-ZFTBAK GT L_ZFTBAK.
*         IT_ZSTAXBKIT-ZFTBAK = IT_ZSTAXBKIT-ZFTBAK
*                             + ( ZTTAXBKHD-ZFTBAK - L_ZFTBAK ).
*      ENDIF.
*
*      IF L_ZFCUAMT GT ZTTAXBKHD-ZFCUAMT.
*         IT_ZSTAXBKIT-ZFCUAMT = IT_ZSTAXBKIT-ZFCUAMT
*                              - ( L_ZFCUAMT - ZTTAXBKHD-ZFCUAMT ).
*      ELSEIF ZTTAXBKHD-ZFCUAMT GT L_ZFCUAMT.
*         IT_ZSTAXBKIT-ZFCUAMT = IT_ZSTAXBKIT-ZFCUAMT
*                              + ( ZTTAXBKHD-ZFCUAMT - L_ZFCUAMT ).
*      ENDIF.
*
*      IF L_ZFHMAMT GT ZTTAXBKHD-ZFHMAMT.
*         IT_ZSTAXBKIT-ZFHMAMT = IT_ZSTAXBKIT-ZFHMAMT
*                              - ( L_ZFHMAMT - ZTTAXBKHD-ZFHMAMT ).
*      ELSEIF ZTTAXBKHD-ZFHMAMT GT L_ZFHMAMT.
*         IT_ZSTAXBKIT-ZFHMAMT = IT_ZSTAXBKIT-ZFHMAMT
*                              + ( ZTTAXBKHD-ZFHMAMT - L_ZFHMAMT ).
*      ENDIF.
*
*      IF L_ZFEDAMT GT ZTTAXBKHD-ZFEDAMT.
*         IT_ZSTAXBKIT-ZFEDAMT = IT_ZSTAXBKIT-ZFEDAMT
*                              - ( L_ZFEDAMT - ZTTAXBKHD-ZFEDAMT ).
*      ELSEIF ZTTAXBKHD-ZFEDAMT GT L_ZFEDAMT.
*         IT_ZSTAXBKIT-ZFEDAMT = IT_ZSTAXBKIT-ZFEDAMT
*                              + ( ZTTAXBKHD-ZFEDAMT - L_ZFEDAMT ).
*      ENDIF.
*
*      IF L_ZFAGAMT GT ZTTAXBKHD-ZFAGAMT.
*         IT_ZSTAXBKIT-ZFAGAMT = IT_ZSTAXBKIT-ZFAGAMT
*                              - ( L_ZFAGAMT - ZTTAXBKHD-ZFAGAMT ).
*      ELSEIF ZTTAXBKHD-ZFAGAMT GT L_ZFAGAMT.
*         IT_ZSTAXBKIT-ZFAGAMT = IT_ZSTAXBKIT-ZFAGAMT
*                              + ( ZTTAXBKHD-ZFAGAMT - L_ZFAGAMT ).
*      ENDIF.
*
*      IF L_ZFTXAMTS GT ZTTAXBKHD-ZFTXAMTS.
*         IT_ZSTAXBKIT-ZFTXAMTS = IT_ZSTAXBKIT-ZFTXAMTS
*                              - ( L_ZFTXAMTS - ZTTAXBKHD-ZFTXAMTS ).
*      ELSEIF ZTTAXBKHD-ZFTXAMTS GT L_ZFTXAMTS.
*         IT_ZSTAXBKIT-ZFTXAMTS = IT_ZSTAXBKIT-ZFTXAMTS
*                              + ( ZTTAXBKHD-ZFTXAMTS - L_ZFTXAMTS ).
*      ENDIF.
*
*      MODIFY IT_ZSTAXBKIT INDEX L_TABIX.
*   ENDIF.


   CLEAR : ZTTAXBKHD-ZFBUYMN, ZTTAXBKHD-ZFTBAK, ZTTAXBKHD-ZFCUAMT,
           ZTTAXBKHD-ZFHMAMT, ZTTAXBKHD-ZFEDAMT, ZTTAXBKHD-ZFAGAMT,
           ZTTAXBKHD-ZFTXAMTS.

   LOOP AT IT_ZSTAXBKIT.
      ADD : IT_ZSTAXBKIT-BKMENGE  TO ZTTAXBKHD-ZFBUYMN,
            IT_ZSTAXBKIT-ZFTBAK   TO ZTTAXBKHD-ZFTBAK,
            IT_ZSTAXBKIT-ZFCUAMT  TO ZTTAXBKHD-ZFCUAMT,
            IT_ZSTAXBKIT-ZFHMAMT  TO ZTTAXBKHD-ZFHMAMT,
            IT_ZSTAXBKIT-ZFEDAMT  TO ZTTAXBKHD-ZFEDAMT,
            IT_ZSTAXBKIT-ZFAGAMT  TO ZTTAXBKHD-ZFAGAMT,
            IT_ZSTAXBKIT-ZFTXAMTS TO ZTTAXBKHD-ZFTXAMTS.
   ENDLOOP.


ENDFORM.                    " P2000_AUTO_CALC
*&---------------------------------------------------------------------*
*&      Form  P3000_REQIT_MENGE_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_REQIT_MENGE_MOVE.
   DATA: L_MENGE       LIKE EKAB-MENGE,
         L_MENGE_H     LIKE EKAB-MENGE,
         L_MENGE_H_TOT LIKE EKAB-MENGE,
         L_MENGE_S     LIKE EKAB-MENGE,
         L_MENGE_S_TOT LIKE EKAB-MENGE.


   REFRESH : IT_ZSTAXBKIT, IT_ZSTAXBKIT_ORG.

   IF EKKO-BSTYP EQ 'K'.   ">일괄계약.

*      LOOP AT IT_EKAB.
*         W_TABIX = SY-TABIX.
*         READ TABLE IT_ZSREQIT
*                    WITH KEY EBELN = IT_EKAB-EBELN
*                             EBELP = IT_EKAB-EBELP.
*
*         IF W_TABIX EQ 1.
*            MOVE : IT_ZSREQIT-STAWN TO ZTTAXBKHD-STAWN.
*         ENDIF.
*         CLEAR : IT_ZSTAXBKIT.
*         MOVE-CORRESPONDING   IT_ZSREQIT   TO   IT_ZSTAXBKIT.
*         MOVE : IT_EKAB-MENGE              TO   IT_ZSTAXBKIT-MENGE,
*                IT_EKAB-EBELN              TO   IT_ZSTAXBKIT-EBELN,
*                IT_EKAB-EBELP              TO   IT_ZSTAXBKIT-EBELP.
*
**> 기납증 수량
*         SELECT SUM( BKMENGE ) INTO IT_ZSTAXBKIT-RMENGE
*                FROM  ZTTAXBKIT
*                WHERE EBELN EQ IT_EKAB-EBELN
*                AND   EBELP EQ IT_EKAB-EBELP.
*         IF IT_ZSTAXBKIT-RMENGE GE IT_ZSTAXBKIT-MENGE.
*            IT_ZSTAXBKIT-BKMENGE = 0.
*         ELSE.
*            IT_ZSTAXBKIT-BKMENGE =
*                         IT_ZSTAXBKIT-MENGE - IT_ZSTAXBKIT-RMENGE.
*         ENDIF.
*
*         APPEND IT_ZSTAXBKIT.
*      ENDLOOP.

*>> 김영광 수정. ( 기납증 수량: Contract에서 P/O전환 후 송장처리 한 *
*   ITEM을 SUM, 기납증의 ITEM은 Contract의 item만큼 display.

      LOOP AT IT_ZSREQIT.
         W_TABIX = SY-TABIX.

         IF W_TABIX EQ 1.
            MOVE : IT_ZSREQIT-STAWN TO ZTTAXBKHD-STAWN.
         ENDIF.

         CLEAR : IT_ZSTAXBKIT.

         MOVE-CORRESPONDING   IT_ZSREQIT   TO   IT_ZSTAXBKIT.

         SELECT * FROM EKAB
                  WHERE KONNR EQ IT_ZSREQIT-EBELN
                    AND KTPNR EQ IT_ZSREQIT-EBELP.

             SELECT SUM( MENGE ) INTO L_MENGE_H
               FROM EKBE
              WHERE EBELN EQ EKAB-EBELN
                AND EBELP EQ EKAB-EBELP
                AND VGABE EQ '1'
                AND BEWTP EQ 'E'
                AND SHKZG EQ 'H'.

*> 강석봉 수정 작업.
*             IF SY-SUBRC EQ 0.
*                 IT_ZSTAXBKIT-EBELN = EKAB-EBELN.
*                 IT_ZSTAXBKIT-EBELP = EKAB-EBELP.
*             ENDIF.

             SELECT SUM( MENGE ) INTO L_MENGE_S
               FROM EKBE
              WHERE EBELN EQ EKAB-EBELN
                AND EBELP EQ EKAB-EBELP
                AND VGABE EQ '1'
                AND BEWTP EQ 'E'
                AND SHKZG EQ 'S'.

             L_MENGE_H_TOT = L_MENGE_H_TOT + L_MENGE_H.
             L_MENGE_S_TOT = L_MENGE_S_TOT + L_MENGE_S.
         ENDSELECT.

*-------------------------------------------------------------------
*> 입고수량.
         L_MENGE = L_MENGE_S_TOT - L_MENGE_H_TOT.
         IT_ZSTAXBKIT-GRMENGE = L_MENGE.


*> 기납증 수량 -> 기참조 수량.
         SELECT SUM( BKMENGE ) INTO IT_ZSTAXBKIT-RMENGE
                FROM  ZTTAXBKCST
                WHERE ZFREQNO EQ IT_ZSREQIT-ZFREQNO
                AND   ZFITMNO EQ IT_ZSREQIT-ZFITMNO.

         IF IT_ZSTAXBKIT-RMENGE GE L_MENGE.
            IT_ZSTAXBKIT-BKMENGE = 0.
         ELSE.
            IT_ZSTAXBKIT-BKMENGE =
                         L_MENGE - IT_ZSTAXBKIT-RMENGE.
         ENDIF.

         APPEND IT_ZSTAXBKIT.
         CLEAR: L_MENGE, L_MENGE_H, L_MENGE_S, L_MENGE_H_TOT,
                L_MENGE_S_TOT.
      ENDLOOP.

   ELSE.
      LOOP AT IT_ZSREQIT.
         W_TABIX = SY-TABIX.
         IF W_TABIX EQ 1.
            MOVE : IT_ZSREQIT-STAWN TO ZTTAXBKHD-STAWN.
         ENDIF.
         CLEAR : IT_ZSTAXBKIT.
         MOVE-CORRESPONDING   IT_ZSREQIT   TO   IT_ZSTAXBKIT.
*> 기납증 수량
         SELECT SUM( BKMENGE ) INTO IT_ZSTAXBKIT-RMENGE
                FROM  ZTTAXBKIT
                WHERE ZFREQNO EQ IT_ZSREQIT-ZFREQNO
                AND   ZFITMNO EQ IT_ZSREQIT-ZFITMNO.

         IF IT_ZSTAXBKIT-RMENGE GE IT_ZSTAXBKIT-MENGE.
            IT_ZSTAXBKIT-BKMENGE = 0.
         ELSE.
            IT_ZSTAXBKIT-BKMENGE =
                         IT_ZSTAXBKIT-MENGE - IT_ZSTAXBKIT-RMENGE.
         ENDIF.

* 입고수량 계산.
         SELECT SUM( MENGE ) INTO L_MENGE_H
               FROM EKBE
              WHERE EBELN EQ IT_ZSREQIT-EBELN
                AND EBELP EQ IT_ZSREQIT-EBELP
                AND VGABE EQ '1'
                AND BEWTP EQ 'E'
                AND SHKZG EQ 'H'.

         SELECT SUM( MENGE ) INTO L_MENGE_S
               FROM EKBE
              WHERE EBELN EQ IT_ZSREQIT-EBELN
                AND EBELP EQ IT_ZSREQIT-EBELP
                AND VGABE EQ '1'
                AND BEWTP EQ 'E'
                AND SHKZG EQ 'S'.

         IT_ZSTAXBKIT-GRMENGE = L_MENGE_S - L_MENGE_H.
         IT_ZSTAXBKIT-BKMENGE = IT_ZSTAXBKIT-GRMENGE
                              - IT_ZSTAXBKIT-RMENGE.
         IF IT_ZSTAXBKIT-BKMENGE LT 0.
            IT_ZSTAXBKIT-BKMENGE = 0.
         ENDIF.

         APPEND IT_ZSTAXBKIT.
      ENDLOOP.
   ENDIF.

*> 사업영역 찾기.
   REFRESH : IT_GSBER.
   LOOP AT IT_ZSTAXBKIT WHERE BKMENGE GT 0.
       PERFORM  P1000_GET_ITEM_GSBER(SAPMZIM01)
                                     TABLES IT_GSBER
                                     USING  IT_ZSTAXBKIT-MATNR
                                            IT_ZSTAXBKIT-WERKS.
   ENDLOOP.

   IT_ZSTAXBKIT_ORG[] = IT_ZSTAXBKIT[].

ENDFORM.                    " P3000_REQIT_MENGE_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SET_INIT_SCREEN.

  W_OK_CODE = OK-CODE.
  ANTWORT = 'N'.

  IF W_EDIT_CHECK EQ 'Y'.
     IF NOT ( W_STATUS EQ C_REQ_D ).
        PERFORM P2000_SET_MESSAGE USING  'ANZG'.
        CASE ANTWORT.
           WHEN 'Y'.              " Yes...
              IF W_EDIT_CHECK NE 'Y'.
                 MESSAGE S211.
              ELSE.
                 PERFORM  P2000_COMPARE_ITEM_HEADER.
                 PERFORM  P3000_DB_MODIFY_SCRCOM.
                 IF W_SUBRC EQ 0.
                    COMMIT WORK.
                 ENDIF.
              ENDIF.

              CLEAR OK-CODE.
           WHEN 'N'.              " No...
              MESSAGE  S957.
              CLEAR OK-CODE.
           WHEN OTHERS.
              EXIT.
        ENDCASE.
     ENDIF.
  ELSE.
     MESSAGE S211.
  ENDIF.
* LEAVE TO TRANSACTION
  PERFORM   P2000_SET_TRANSACTION.


ENDFORM.                    " P2000_SET_INIT_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SET_MODIFY_FIELD_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_MODIFY_FIELD_CHECK.

  W_EDIT_CHECK = 'N'.

  IF ZTTAXBKHD NE *ZTTAXBKHD.
     W_EDIT_CHECK = 'Y'.
  ENDIF.
  CHECK : W_EDIT_CHECK  NE 'Y'.

  LOOP AT IT_ZSTAXBKIT.
     W_TABIX = SY-TABIX.
     READ TABLE IT_ZSTAXBKIT_ORG INDEX W_TABIX.
     IF SY-SUBRC EQ 0.
        IF IT_ZSTAXBKIT-TXZ01 NE IT_ZSTAXBKIT_ORG-TXZ01.
           W_EDIT_CHECK = 'Y'.  EXIT.
        ENDIF.
        IF IT_ZSTAXBKIT-BKMENGE NE IT_ZSTAXBKIT_ORG-BKMENGE.
           W_EDIT_CHECK = 'Y'.  EXIT.
        ENDIF.
        IF IT_ZSTAXBKIT-ZFCUAMT NE IT_ZSTAXBKIT_ORG-ZFCUAMT.
           W_EDIT_CHECK = 'Y'.  EXIT.
        ENDIF.
        IF IT_ZSTAXBKIT-ZFTBAK NE IT_ZSTAXBKIT_ORG-ZFTBAK.
           W_EDIT_CHECK = 'Y'.  EXIT.
        ENDIF.
        IF IT_ZSTAXBKIT-MEINS NE IT_ZSTAXBKIT_ORG-MEINS.
           W_EDIT_CHECK = 'Y'.  EXIT.
        ENDIF.
        IF IT_ZSTAXBKIT-ZFHMAMT NE IT_ZSTAXBKIT_ORG-ZFHMAMT.
           W_EDIT_CHECK = 'Y'.  EXIT.
        ENDIF.
        IF IT_ZSTAXBKIT-ZFEDAMT NE IT_ZSTAXBKIT_ORG-ZFEDAMT.
           W_EDIT_CHECK = 'Y'.  EXIT.
        ENDIF.
        IF IT_ZSTAXBKIT-ZFAGAMT NE IT_ZSTAXBKIT_ORG-ZFAGAMT.
           W_EDIT_CHECK = 'Y'.  EXIT.
        ENDIF.
     ELSE.
        W_EDIT_CHECK = 'Y'.  EXIT.
     ENDIF.
  ENDLOOP.
  CHECK : W_EDIT_CHECK  NE 'Y'.

  LOOP AT IT_ZSTAXBKIT_ORG.
     W_TABIX = SY-TABIX.
     READ TABLE IT_ZSTAXBKIT INDEX W_TABIX.
     IF SY-SUBRC NE 0.
        W_EDIT_CHECK = 'Y'.  EXIT.
     ENDIF.
  ENDLOOP.

ENDFORM.                    " SET_MODIFY_FIELD_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_TRANSACTION.
  CASE SY-TCODE.
    WHEN 'ZIMZ1' OR 'ZIMZ2' OR 'ZIMZ3'.          " B/L
      CASE W_OK_CODE.
         WHEN 'OTDC'.   " OTHERS DOCUMENT
            LEAVE TO TRANSACTION SY-TCODE.
         WHEN 'CRDC'.   " CREATE
            LEAVE TO TRANSACTION 'ZIMZ1'.
         WHEN 'CHDC'.   " CHANGE
            LEAVE TO TRANSACTION 'ZIMZ2'.
         WHEN 'DISP'.   " DISPLAY
*            LEAVE TO TRANSACTION 'ZIMZ3' AND SKIP FIRST SCREEN.
            LEAVE TO TRANSACTION 'ZIMZ3'.
         WHEN OTHERS.
      ENDCASE.
  ENDCASE.

ENDFORM.                    " P2000_SET_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  P2000_COMPARE_ITEM_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_COMPARE_ITEM_HEADER.

   CLEAR : WZTTAXBKHD-ZFBUYMN, WZTTAXBKHD-ZFTBAK, WZTTAXBKHD-ZFCUAMT,
           WZTTAXBKHD-ZFHMAMT, WZTTAXBKHD-ZFEDAMT, WZTTAXBKHD-ZFAGAMT,
           WZTTAXBKHD-ZFTXAMTS.

   LOOP AT IT_ZSTAXBKIT.
      W_TABIX = SY-TABIX.
      IF IT_ZSTAXBKIT-BKMENGE IS INITIAL.
         PERFORM  P2000_NO_INPUT(SAPMZIM01)
                                USING 'ZSTAXBKIT' 'ZFBUYMN'
                                       dfies-scrtext_m W_SUBRC.
         MESSAGE E693 WITH W_TABIX  dfies-scrtext_m.
      ENDIF.
      IF IT_ZSTAXBKIT-ZFCUAMT IS INITIAL.
         PERFORM  P2000_NO_INPUT(SAPMZIM01)
                                USING 'ZSTAXBKIT' 'ZFCUAMT'
                                       dfies-scrtext_m W_SUBRC.
         MESSAGE E693 WITH W_TABIX dfies-scrtext_m.
      ENDIF.
      IF IT_ZSTAXBKIT-ZFTBAK IS INITIAL.
         PERFORM  P2000_NO_INPUT(SAPMZIM01)
                                USING 'ZTTAXBKHD' 'ZFTBAK'
                                       dfies-scrtext_m W_SUBRC.
         MESSAGE E693 WITH W_TABIX dfies-scrtext_m.
      ENDIF.

      ADD : IT_ZSTAXBKIT-BKMENGE  TO WZTTAXBKHD-ZFBUYMN,
            IT_ZSTAXBKIT-ZFTBAK   TO WZTTAXBKHD-ZFTBAK,
            IT_ZSTAXBKIT-ZFCUAMT  TO WZTTAXBKHD-ZFCUAMT,
            IT_ZSTAXBKIT-ZFHMAMT  TO WZTTAXBKHD-ZFHMAMT,
            IT_ZSTAXBKIT-ZFEDAMT  TO WZTTAXBKHD-ZFEDAMT,
            IT_ZSTAXBKIT-ZFAGAMT  TO WZTTAXBKHD-ZFAGAMT,
            IT_ZSTAXBKIT-ZFTXAMTS TO WZTTAXBKHD-ZFTXAMTS.
   ENDLOOP.
*> 양도(매입)물량.
   IF WZTTAXBKHD-ZFBUYMN NE ZTTAXBKHD-ZFBUYMN.
      PERFORM  P2000_NO_INPUT(SAPMZIM01)
                             USING 'ZTTAXBKHD' 'ZFBUYMN'
                                    dfies-scrtext_m W_SUBRC.
      MESSAGE E682 WITH dfies-scrtext_m.
   ENDIF.
*>과세가격-원화.
   IF WZTTAXBKHD-ZFTBAK NE ZTTAXBKHD-ZFTBAK.
      PERFORM  P2000_NO_INPUT(SAPMZIM01)
                             USING 'ZTTAXBKHD' 'ZFTBAK'
                                    dfies-scrtext_m W_SUBRC.
      MESSAGE E682 WITH dfies-scrtext_m.
   ENDIF.
*> 관세.
   IF WZTTAXBKHD-ZFCUAMT NE ZTTAXBKHD-ZFCUAMT.
      PERFORM  P2000_NO_INPUT(SAPMZIM01)
                             USING 'ZTTAXBKHD' 'ZFCUAMT'
                                    dfies-scrtext_m W_SUBRC.
      MESSAGE E682 WITH dfies-scrtext_m.
   ENDIF.
*> 내국세.
   IF WZTTAXBKHD-ZFHMAMT NE ZTTAXBKHD-ZFHMAMT.
      PERFORM  P2000_NO_INPUT(SAPMZIM01)
                             USING 'ZTTAXBKHD' 'ZFHMAMT'
                                    dfies-scrtext_m W_SUBRC.
      MESSAGE E682 WITH dfies-scrtext_m.
   ENDIF.
*> 교육세.
   IF WZTTAXBKHD-ZFEDAMT NE ZTTAXBKHD-ZFEDAMT.
      PERFORM  P2000_NO_INPUT(SAPMZIM01)
                             USING 'ZTTAXBKHD' 'ZFEDAMT'
                                    dfies-scrtext_m W_SUBRC.
      MESSAGE E682 WITH dfies-scrtext_m.
   ENDIF.
*> 농특세.
   IF WZTTAXBKHD-ZFAGAMT NE ZTTAXBKHD-ZFAGAMT.
      PERFORM  P2000_NO_INPUT(SAPMZIM01)
                             USING 'ZTTAXBKHD' 'ZFAGAMT'
                                    dfies-scrtext_m W_SUBRC.
      MESSAGE E682 WITH dfies-scrtext_m.
   ENDIF.
*> 총세액.
   IF WZTTAXBKHD-ZFTXAMTS NE ZTTAXBKHD-ZFTXAMTS.
      PERFORM  P2000_NO_INPUT(SAPMZIM01)
                             USING 'ZTTAXBKHD' 'ZFTXAMTS'
                                    dfies-scrtext_m W_SUBRC.
      MESSAGE E682 WITH dfies-scrtext_m.
   ENDIF.




ENDFORM.                    " P2000_COMPARE_ITEM_HEADER
*&---------------------------------------------------------------------*
*&      Form  P2000_TAXPO_DISPLAY_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_TAXPO_DISPLAY_LIST.

  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  WRITE : / IT_ZSTAXBKHD-EBELN   NO-GAP,    SY-VLINE NO-GAP,
            IT_ZSTAXBKHD-ZFREQNO NO-GAP,    SY-VLINE NO-GAP,
            IT_ZSTAXBKHD-BASISNO NO-GAP,    SY-VLINE NO-GAP,
            IT_ZSTAXBKHD-ZFBUYDT NO-GAP,    SY-VLINE NO-GAP,
            IT_ZSTAXBKHD-ZFTBNO  NO-GAP,    SY-VLINE NO-GAP,
            IT_ZSTAXBKHD-ZFTBPNO NO-GAP,    SY-VLINE.

ENDFORM.                    " P2000_TAXPO_DISPLAY_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_DOC_ITEM_SELECT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_DOC_ITEM_SELECT1.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
* INPUT VALUE 보존을 위해...
  W_ZSREQHD = ZSREQHD.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZVREQHD_ST
           WHERE  EBELN   EQ ZSREQHD-EBELN
           AND    ZFAMDNO EQ '00000'
           ORDER  BY ZFREQNO.

*----------------------------------------------------------------------
* position
*----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.   MESSAGE E406.   ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'LCCHANGE'.                 " L/C 변?
  CALL SCREEN 0014 STARTING AT  09 3
                   ENDING   AT  90 15.

ENDFORM.                    " P2000_DOC_ITEM_SELECT1
*&---------------------------------------------------------------------*
*&      Form  P1000_TAXBOOK_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_TAXBOOK_READ.
* 수입의뢰 Header Table
  CALL FUNCTION 'ZIM_GET_TAX_BOOK'
       EXPORTING
            ZFTBNO           = ZSREQHD-ZFTBNO
       IMPORTING
            ZTTAXBKHD        = ZTTAXBKHD
       TABLES
            IT_ZSTAXBKIT     = IT_ZSTAXBKIT
            IT_ZSTAXBKIT_ORG = IT_ZSTAXBKIT_ORG
       EXCEPTIONS
            NOT_FOUND       = 4
            NOT_INPUT       = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E685 WITH ZSREQHD-ZFREQNO.
    WHEN 8.
      MESSAGE E686.
  ENDCASE.

  *ZTTAXBKHD = ZTTAXBKHD.

*> 상태 체크.
  IF SY-TCODE EQ 'ZIMZ2' AND ZTTAXBKHD-ZFPOSYN EQ 'Y'.
     MESSAGE E687 WITH ZTTAXBKHD-ZFTBNO 'change'.
  ENDIF.

  IF SY-TCODE EQ 'ZIMZ2'.
     PERFORM P2000_SET_REQDOC_LOCK USING 'L'.
  ENDIF.

*> 사업영역 찾기.
   REFRESH : IT_GSBER.
   LOOP AT IT_ZSTAXBKIT WHERE BKMENGE GT 0.
       PERFORM  P1000_GET_ITEM_GSBER(SAPMZIM01)
                                     TABLES IT_GSBER
                                     USING  IT_ZSTAXBKIT-MATNR
                                            IT_ZSTAXBKIT-WERKS.
   ENDLOOP.

ENDFORM.                    " P1000_TAXBOOK_READ
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INDICATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SET_INDICATE.

  W_OK_CODE = OK-CODE.
*-----------------------------------------------------------------------
* AUTHORITY CHECK
  PERFORM P2000_AUTHORITY_CHECK.
*-----------------------------------------------------------------------
* Lock Object Set
  IF W_STATUS EQ C_REQ_D.
     PERFORM P2000_SET_REQDOC_LOCK    USING    'L'.
  ENDIF.
* MESSAGE BOX
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.
  CASE ANTWORT.
     WHEN 'Y'.
        IF W_OK_CODE EQ 'POST'.
           IF W_STATUS EQ C_REQ_D.
              PERFORM  P3000_FI_POSTING.
           ELSE.
              PERFORM  P2000_COMPARE_ITEM_HEADER.
              PERFORM  P3000_DB_MODIFY_SCRCOM.
              IF W_SUBRC EQ 0.
                 PERFORM  P3000_FI_POSTING.
              ENDIF.
           ENDIF.
        ELSEIF W_OK_CODE EQ 'DCDE'.
           PERFORM  P3000_FI_DOC_CANCLE.
        ELSEIF W_OK_CODE EQ 'DELE'.
           PERFORM  P3000_DB_MODIFY_SCRCOM.
        ENDIF.
        CLEAR OK-CODE.
        IF W_STATUS NE C_REQ_C.
           PERFORM P2000_SET_REQDOC_LOCK    USING    'U'.
        ENDIF.
        PERFORM  P2000_SET_SCREEN_SCRCOM.
        LEAVE SCREEN.
     WHEN OTHERS.
        IF W_STATUS EQ C_REQ_D.
           PERFORM P2000_SET_REQDOC_LOCK    USING    'U'.
        ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_SET_INDICATE

*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_OK_CODE  text
*----------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK.

*   AUTHORITY-CHECK OBJECT 'ZIM_ZIMZ2'
*                      ID 'ZIACTVT' FIELD '*'.
*
*  IF SY-SUBRC NE 0.
*     MESSAGE E077 WITH SY-UNAME 'CTM change'.
*  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_PO_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_PO_DOCUMENT_DISPLAY.

   SELECT SINGLE * FROM EKKO
          WHERE    EBELN EQ ZTTAXBKHD-EBELN.

   IF EKKO-BSTYP EQ 'L'.
      SET PARAMETER ID 'SAG' FIELD ZTTAXBKHD-EBELN.
      CALL TRANSACTION 'ME33L' AND SKIP  FIRST SCREEN.
   ELSE.
      SET PARAMETER ID 'BES' FIELD ZTTAXBKHD-EBELN.
      CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_PO_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_LC_DOC_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTREQHD_ZFREQNO  text
*      -->P_0380   text
*----------------------------------------------------------------------*
FORM P2000_LC_DOC_DISPLAY USING    P_ZFREQNO
                                   P_ZFOPNNO.

  IF P_ZFREQNO IS INITIAL AND  P_ZFOPNNO IS INITIAL.
    MESSAGE S063.   EXIT.
  ENDIF.

  SET PARAMETER ID 'BES' FIELD ''.
  SET PARAMETER ID 'ZPREQNO' FIELD P_ZFREQNO.
  SET PARAMETER ID 'ZPOPNNO' FIELD P_ZFOPNNO.

  CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_LC_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_VENDOR_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_VENDOR_DISPLAY.

  SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
*  SET PARAMETER ID 'KDY' FIELD '/320/310/130/120/110'.
  SET PARAMETER ID 'LIF' FIELD ZTTAXBKHD-ZFTRNCD.
*  SET PARAMETER ID 'EKO' FIELD ZTREQST-EKORG.
  SET PARAMETER ID 'EKO' FIELD ''.
*  EXPORT 'LIF'   TO MEMORY ID 'LIF'.
*  EXPORT 'EKO'   TO MEMORY ID 'EKO'.

  CALL TRANSACTION 'MK03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_VENDOR_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_MATERIAL_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MATERIAL_DISPLAY.
  CHECK : W_COUNT EQ 1.

  PERFORM P1000_EKPO_SELECT_ITEM.

  SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
  SET PARAMETER ID 'BUK' FIELD EKPO-BUKRS.
  SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
  SET PARAMETER ID 'LAG' FIELD ''.
  SET PARAMETER ID 'MXX' FIELD MM03_START_SICHT.

  CALL TRANSACTION 'MM03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_MATERIAL_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P1000_EKPO_SELECT_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_EKPO_SELECT_ITEM.

  SELECT SINGLE * FROM EKPO WHERE EBELN EQ ZTTAXBKIT-EBELN
                            AND   EBELP EQ ZTTAXBKIT-EBELP.

  IF SY-SUBRC EQ 0.
    IF EKPO-MATNR IS INITIAL.
      MESSAGE E336(ME) WITH ZTTAXBKIT-EBELP.
    ENDIF.
  ELSE.
    MESSAGE E071 WITH ZTTAXBKIT-EBELN ZTTAXBKIT-EBELP.
  ENDIF.

ENDFORM.                    " P1000_EKPO_SELECT_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_MATERIAL_MD04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MATERIAL_MD04.
  CHECK : W_COUNT EQ 1.

  PERFORM P1000_EKPO_SELECT_ITEM.

  SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
  SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
  CALL TRANSACTION 'MD04' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_MATERIAL_MD04
*&---------------------------------------------------------------------*
*&      Form  P2000_MATERIAL_MMBE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MATERIAL_MMBE.
  CHECK : W_COUNT EQ 1.

  PERFORM P1000_EKPO_SELECT_ITEM.

  SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
  SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
  SET PARAMETER ID 'LAG' FIELD ''.          " 저장위?
  SET PARAMETER ID 'CHA' FIELD ''.          " Batch 번?
  CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_MATERIAL_MMBE
*&---------------------------------------------------------------------*
*&      Form  P2000_MATERIAL_MB51
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MATERIAL_MB51.
  CHECK : W_COUNT EQ 1.

  PERFORM P1000_EKPO_SELECT_ITEM.

  SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
  SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
  SET PARAMETER ID 'BWA' FIELD ''.                   " 자재이동 유?
  SET PARAMETER ID 'LIF' FIELD ZTTAXBKHD-ZFTRNCD.    " 거래처코?
  SET PARAMETER ID 'LAG' FIELD ''.                   " 저장위?
  SET PARAMETER ID 'CHA' FIELD ''.                   " Batch 번?
  CALL TRANSACTION 'MB51' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_MATERIAL_MB51
*&---------------------------------------------------------------------*
*&      Form  P2000_MATERIAL_ME2M
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MATERIAL_ME2M.
  CHECK : W_COUNT EQ 1.

  SUBMIT RM06EM00
         WITH EM_MATNR EQ EKPO-MATNR
         WITH EM_WERKS EQ EKPO-WERKS
         WITH LISTU    EQ 'BEST'
         WITH SELPA    EQ 'WE101'         " 매개변?
         AND RETURN.

ENDFORM.                    " P2000_MATERIAL_ME2M
*&---------------------------------------------------------------------*
*&      Form  P2000_MATERIAL_ME03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MATERIAL_ME03.
   CHECK : W_COUNT EQ 1.

   PERFORM P1000_EKPO_SELECT_ITEM.

   SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
   SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
   CALL TRANSACTION 'ME03' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_MATERIAL_ME03
*&---------------------------------------------------------------------*
*&      Form  P2000_POSTING_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_POSTING_MESSAGE.

  PERFORM P2000_MESSAGE_BOX USING '전기 확인'             " 타이틀...
                          '현재 Document를 전기합니다.'
                          '회계 전기하시겠습니까?'      " MSG2
                          'N'                 " 취소 버튼 유/?
                          '1'.                " default button

ENDFORM.                    " P2000_POSTING_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_POST_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_POST_VALUE.

   CLEAR : ZTBKPF.

*   GET PARAMETER ID 'BUK'    FIELD ZTBKPF-BUKRS.
   SELECT * FROM ZTIMIMG08 UP TO 1 ROWS
            WHERE ZFCDTY    EQ  '008'.
   ENDSELECT.

   PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCDTY'
                                               '008'
                                        CHANGING   W_COST_TYPE.
*>> 지급조건 가져오기.
   IF NOT ZTTAXBKHD-BUKRS IS INITIAL.
      CALL FUNCTION 'FI_VENDOR_DATA'
                EXPORTING
                  i_bukrs = ZTTAXBKHD-BUKRS
                  i_lifnr = ZTTAXBKHD-ZFTRNCD
                IMPORTING
                  e_kred  = vf_kred.

      ZTBKPF-ZTERM = VF_KRED-ZTERM.
      ZTBKPF-AKONT = VF_KRED-AKONT.
   ENDIF.

*> 사업장 Match 작업.
   READ TABLE IT_ZSTAXBKIT INDEX 1.
   IF SY-SUBRC EQ 0.
      SELECT SINGLE J_1BBRANCH INTO ZTBKPF-BUPLA
             FROM T001W
             WHERE WERKS   EQ
                 ( SELECT WERKS FROM  EKPO
                          WHERE EBELN EQ IT_ZSTAXBKIT-EBELN
                          AND   EBELP EQ IT_ZSTAXBKIT-EBELP ).
   ENDIF.

   MOVE : SY-MANDT           TO      ZTBKPF-MANDT,
          ZTTAXBKHD-BUKRS    TO      ZTBKPF-BUKRS,
          'N'                TO      ZTBKPF-ZFPOSYN,
          SY-DATUM           TO      ZTBKPF-BLDAT,
          SY-DATUM           TO      ZTBKPF-BUDAT,
          'X'                TO      ZTBKPF-TBTKZ,
          SPACE              TO      ZTBKPF-ZFDCSTX,
          ZTIMIMG08-BLART    TO      ZTBKPF-BLART,
          ZTTAXBKHD-ZFTRNCD  TO      ZTBKPF-LIFNR,
          ZTTAXBKHD-ZFTRNCD  TO      ZTBKPF-ZFVEN,
          ZTTAXBKHD-ZFTRNSNM TO      LFA1-NAME1,
          '008'              TO      ZTBKPF-ZFCSTGRP,
          ZTIMIMG08-ZFCD     TO      ZTIMIMG08-ZFCD,
          ZTIMIMG08-ZFCDNM   TO      W_CODE_TYPE,
          ZTIMIMG08-ZFCD5    TO      ZTBKPF-MWSKZ,
          'X'                TO      ZTBKPF-ZFPCUR,
          ZTTAXBKHD-ZFTXAMTS TO      ZTBKPF-WRBTR,
          ZTTAXBKHD-ZFTXAMTS TO      ZTBKPF-DMBTR,
          ZTTAXBKHD-ZFKRW    TO      ZTBKPF-WAERS,
          ZTTAXBKHD-ZFKRW    TO      ZTBKPF-HWAER,
          'Y'                TO      ZTBKPF-ZFPOYN,
          ZTTAXBKHD-BASISNO  TO      ZTBKPF-XBLNR,
          'X'                TO      ZTBKPF-ZFAUTO,
          'X'                TO      ZTBKPF-ZFATPT,
          '[수입] 기납증 관세전가액' TO     ZTBKPF-BKTXT.

*>
   REFRESH : IT_GSBER.
   LOOP AT IT_ZSTAXBKIT WHERE BKMENGE GT 0.
       PERFORM  P1000_GET_ITEM_GSBER(SAPMZIM01)
                                     TABLES IT_GSBER
                                     USING  IT_ZSTAXBKIT-MATNR
                                            IT_ZSTAXBKIT-WERKS.
   ENDLOOP.

   DESCRIBE TABLE IT_GSBER LINES W_LINE.
   IF W_LINE EQ 1.
      READ TABLE IT_GSBER INDEX 1.
      MOVE IT_GSBER-GSBER TO ZTBKPF-GSBER.
   ELSE.
      CLEAR ZTBKPF-GSBER.
   ENDIF.

   REFRESH : IT_ZSBSEG, IT_ZSBSEG_OLD.
   CLEAR :  IT_ZSBSEG, IT_ZSBSEG_OLD, ZTIMIMG08.

   SELECT * FROM ZTIMIMG08 UP TO 1 ROWS
            WHERE ZFCDTY   EQ '008'.
   ENDSELECT.

   SELECT SINGLE * FROM ZTIMIMG11
          WHERE    BUKRS EQ ZTBKPF-BUKRS.

   MOVE: ZTBKPF-BUKRS            TO IT_ZSBSEG-BUKRS,
         ZTBKPF-GJAHR            TO IT_ZSBSEG-GJAHR,
         ZTBKPF-BELNR            TO IT_ZSBSEG-BELNR,
         ZTTAXBKHD-ZFTBNO        TO IT_ZSBSEG-ZFIMDNO,
         '008'                   TO IT_ZSBSEG-ZFCSTGRP,
         SPACE                   TO IT_ZSBSEG-ZFDCSTX,
         ZTIMIMG08-COND_TYPE     TO IT_ZSBSEG-COND_TYPE,
         ZTIMIMG08-ZFCD          TO IT_ZSBSEG-ZFCD,
         ZTIMIMG08-ZFCD5         TO IT_ZSBSEG-MWSKZ,
         '40'                    TO IT_ZSBSEG-NEWBS,
         ZTIMIMG11-ZFIOCAC13     TO IT_ZSBSEG-NEWKO,
         'S'                     TO IT_ZSBSEG-SHKZG,
         ZTTAXBKHD-ZFTXAMTS      TO IT_ZSBSEG-WRBTR,   " 전표통화금액.
         '0'                     TO IT_ZSBSEG-WMWST,
         ZTTAXBKHD-ZFTXAMTS      TO IT_ZSBSEG-DMBTR,   " 현지통화금액.
         SY-DATUM                TO IT_ZSBSEG-WWERT,
         '0'                     TO IT_ZSBSEG-ZFVPR,
         ZTTAXBKHD-BASISNO       TO IT_ZSBSEG-ZUONR,   " 지정번호.
         ZTIMIMG08-ZFCDNM        TO IT_ZSBSEG-SGTXT,   " 품목텍스트.
         ' '                     TO IT_ZSBSEG-ZFINSEQ,
         'Y'                     TO IT_ZSBSEG-ZFPOYN,  " 유무환여부.
         '1'                     TO IT_ZSBSEG-KURSF.   " 환율.
   APPEND IT_ZSBSEG.

ENDFORM.                    " P2000_SET_POST_VALUE
*&---------------------------------------------------------------------*
*&      Form  P3000_FI_POSTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_FI_POSTING.
   REFRESH : IT_ERR_LIST, RETURN.

   SET UPDATE TASK LOCAL.

   *ZTTAXBKHD = ZTTAXBKHD.

   IF W_STATUS EQ C_REQ_C.
      READ TABLE IT_ZSBSEG INDEX 1.
      MOVE ZTTAXBKHD-ZFTBNO        TO IT_ZSBSEG-ZFIMDNO.
      MODIFY IT_ZSBSEG INDEX 1.
   ENDIF.

   CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
          EXPORTING
             W_OK_CODE           =   'SAVE'
             BUKRS               =   ZTBKPF-BUKRS
             GJAHR               =   ZTBKPF-GJAHR
             ZFSTATUS            =   C_REQ_C
             W_ZTBKPF_OLD        =  *ZTBKPF
             W_ZTBKPF            =   ZTBKPF
          TABLES
             IT_ZSBSEG_OLD       =   IT_ZSBSEG_OLD
             IT_ZSBSEG           =   IT_ZSBSEG
          CHANGING
             BELNR               =   ZTBKPF-BELNR
          EXCEPTIONS
             ERROR_UPDATE        =   4.

   W_SUBRC = SY-SUBRC.

   IF W_SUBRC EQ 0.
      MOVE: ZTBKPF-BELNR   TO ZTTAXBKHD-ZFACDO,
            ZTBKPF-GJAHR   TO ZTTAXBKHD-ZFFIYR.
   ELSE.
      ROLLBACK WORK.
      MESSAGE S494.
      EXIT.
   ENDIF.

   CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_POST'
          EXPORTING
            BUKRS             =     ZTTAXBKHD-BUKRS
            BELNR             =     ZTTAXBKHD-ZFACDO
            GJAHR             =     ZTTAXBKHD-ZFFIYR
         IMPORTING
            INVOICEDOCNUMBER  =     INVOICEDOCNUMBER
            FISCALYEAR        =     FISCALYEAR
         TABLES
            RETURN            =     RETURN
         EXCEPTIONS
            POST_ERROR        =     4.

   W_SUBRC = SY-SUBRC.

   IF W_SUBRC NE 0.
      ROLLBACK WORK.
      IF RETURN[] IS INITIAL.
         PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'E'.
      ELSE.
         PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
      ENDIF.
   ELSE.
      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'S'.
      COMMIT WORK.
*>> 전표상태 바꾸고.
      ZTTAXBKHD-ZFPOSYN = 'Y'.
      MOVE: INVOICEDOCNUMBER TO ZTTAXBKHD-BELNR,
            FISCALYEAR       TO ZTTAXBKHD-GJAHR,
            SY-UNAME         TO ZTTAXBKHD-UNAM,
            SY-DATUM         TO ZTTAXBKHD-UDAT.
      UPDATE  ZTTAXBKHD.

      CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTTAXBKHD'
           EXPORTING
                      UPD_CHNGIND    =    'U'
                      N_ZTTAXBKHD    =    ZTTAXBKHD
                      O_ZTTAXBKHD    =   *ZTTAXBKHD.

      CLEAR: INVOICEDOCNUMBER ,FISCALYEAR.
*      REFRESH : RETURN.
   ENDIF.

  IF W_SUBRC EQ 0.
     MESSAGE S580 WITH ZTTAXBKHD-BUKRS  ZTTAXBKHD-ZFFIYR
                       ZTTAXBKHD-ZFACDO.
  ELSE.
     DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
     IF W_LINE GT 0.
        INCLUDE = 'POPU'.
        CALL SCREEN 0014 STARTING AT  04   3
                         ENDING   AT  100 12.
        CLEAR : INCLUDE.
     ENDIF.
  ENDIF.

ENDFORM.                    " P3000_FI_POSTING
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
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
*          IT_SELECTED-BELNR   TO     IT_ERR_LIST-BELNR,
*          IT_SELECTED-GJAHR   TO     IT_ERR_LIST-GJAHR.

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
*             IT_SELECTED-GJAHR   TO     IT_ERR_LIST-GJAHR,
*             IT_SELECTED-BELNR   TO     IT_ERR_LIST-BELNR.

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
*&      Form  P2000_FI_CANCLE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_FI_CANCLE_MESSAGE.

  PERFORM P2000_MESSAGE_BOX USING '전기취소 확인'
                                  '전기된 내용의 역분개를 수행합니다.'
                                  '전기하시겠습니까?'
                                  'N'
                                  '1'.

ENDFORM.                    " P2000_FI_CANCLE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_FI_DOC_CANCLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_FI_DOC_CANCLE.
   REFRESH : IT_ERR_LIST, RETURN.

   CALL FUNCTION 'ZIM_GET_CHARGE_DOCUMENT'
        EXPORTING
                    BUKRS           =    ZTTAXBKHD-BUKRS
                    BELNR           =    ZTTAXBKHD-ZFACDO
                    GJAHR           =    ZTTAXBKHD-ZFFIYR
        IMPORTING
                    W_ZTBKPF        =    ZTBKPF
        TABLES
                    IT_ZSBSEG       =    IT_ZSBSEG
                    IT_ZSBSEG_OLD   =    IT_ZSBSEG_OLD
        EXCEPTIONS
                    NOT_FOUND               =    4
                    COMPANDYCODE_NOT_INPUT  =    6
                    DOCUMENT_NO_NOT_INPUT   =    8
                    FISC_YEAR_NOT_INPUT     =   10.

  IF SY-SUBRC NE 0.
     MESSAGE E582 WITH ZTTAXBKHD-BUKRS ZTTAXBKHD-ZFFIYR
                       ZTTAXBKHD-ZFACDO.
  ENDIF.

  CALL FUNCTION 'ZIM_BAPI_COST_INVOICES_CANCEL'
       EXPORTING
          P_ZFIMDTY        =     'CS'
          P_BUKRS          =     ZTTAXBKHD-BUKRS
          INVOICEDOCNUMBER =     ZTTAXBKHD-BELNR " 부가세전표번호.
          FISCALYEAR       =     ZTTAXBKHD-GJAHR " 부가세전표연도.
          REASONREVERSAL   =     UF05A-STGRD
          POSTINGDATE      =     BSIS-BUDAT
*       IMPORTING
*          INVOICEDOCNUMBER =     ZFVTAN
*          FISCALYEAR       =     ZFFYVT
       TABLES
          RETURN           =     RETURN
       EXCEPTIONS
          LIV_ERROR        =     4.

   W_SUBRC = SY-SUBRC.
   *ZTTAXBKHD = ZTTAXBKHD.

  IF W_SUBRC NE 0.           ">> 오류 발생시...
      IF RETURN[] IS INITIAL.
         PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'E'.
      ELSE.
         PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
      ENDIF.

      INCLUDE = 'POPU'.
      CALL SCREEN 0014 STARTING AT  04   3
                       ENDING   AT  100 12.
      CLEAR : INCLUDE.
      EXIT.
  ELSE.
      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'S'.
      CLEAR : ZTTAXBKHD-BELNR, ZTTAXBKHD-GJAHR.
  ENDIF.

   CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
          EXPORTING
             W_OK_CODE           =   'DELE'
             BUKRS               =   ZTTAXBKHD-BUKRS
             GJAHR               =   ZTTAXBKHD-ZFFIYR
             ZFSTATUS            =   C_REQ_D
             W_ZTBKPF_OLD        =  *ZTBKPF
             W_ZTBKPF            =   ZTBKPF
          TABLES
             IT_ZSBSEG_OLD       =   IT_ZSBSEG_OLD
             IT_ZSBSEG           =   IT_ZSBSEG
          CHANGING
             BELNR               =   ZTTAXBKHD-ZFACDO
          EXCEPTIONS
             ERROR_UPDATE        =   4.

   W_SUBRC = SY-SUBRC.

   IF W_SUBRC EQ 0.
      CLEAR : ZTTAXBKHD-ZFACDO, ZTTAXBKHD-ZFFIYR.
      MOVE : 'N'         TO     ZTTAXBKHD-ZFPOSYN.
      UPDATE ZTTAXBKHD.

      CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTTAXBKHD'
           EXPORTING
                      UPD_CHNGIND    =    'U'
                      N_ZTTAXBKHD    =    ZTTAXBKHD
                      O_ZTTAXBKHD    =   *ZTTAXBKHD.
      READ TABLE IT_ERR_LIST INDEX 1.
      MESSAGE S977 WITH IT_ERR_LIST-MESSTXT.
      COMMIT WORK.
   ELSE.
      ROLLBACK WORK.
      MESSAGE S691.
      EXIT.
   ENDIF.

ENDFORM.                    " P3000_FI_DOC_CANCLE
*&---------------------------------------------------------------------*
*&      Form  P2000_PRE_AUTO_CALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_PRE_AUTO_CALC.
   CLEAR : L_BKMENGE,  L_ZFTBAK,
           L_ZFCUAMT,  L_ZFHMAMT,
           L_ZFEDAMT,  L_ZFAGAMT,
           L_ZFTXAMTS.

   L_CHECK = 'N'.

   LOOP AT IT_ZSTAXBKIT.
      W_TABIX = SY-TABIX.
      CLEAR : IT_ZSTAXBKIT-BKMENGE,  IT_ZSTAXBKIT-ZFTBAK,
              IT_ZSTAXBKIT-ZFCUAMT,  IT_ZSTAXBKIT-ZFHMAMT,
              IT_ZSTAXBKIT-ZFEDAMT,  IT_ZSTAXBKIT-ZFAGAMT,
              IT_ZSTAXBKIT-ZFTXAMTS.
      IF L_CHECK = 'Y'.
         MODIFY IT_ZSTAXBKIT INDEX W_TABIX.
         CONTINUE.
      ENDIF.
*> 수량 조정 작업. ( 수입의뢰수량 - 기참조 수량 )
      IT_ZSTAXBKIT-BKMENGE = IT_ZSTAXBKIT-MENGE - IT_ZSTAXBKIT-RMENGE.
      ADD IT_ZSTAXBKIT-BKMENGE TO L_BKMENGE.
      IF L_BKMENGE GT ZTTAXBKHD-ZFBUYMN.
         IT_ZSTAXBKIT-BKMENGE = IT_ZSTAXBKIT-BKMENGE
                              - ( L_BKMENGE - ZTTAXBKHD-ZFBUYMN ).
         L_BKMENGE = ZTTAXBKHD-ZFBUYMN.
         L_CHECK = 'Y'.
      ENDIF.
      MODIFY IT_ZSTAXBKIT INDEX W_TABIX.
   ENDLOOP.

   IF ZTTAXBKHD-ZFBUYMN GT L_BKMENGE.
      MESSAGE E210(ZIM1).
   ENDIF.


ENDFORM.                    " P2000_PRE_AUTO_CALC
