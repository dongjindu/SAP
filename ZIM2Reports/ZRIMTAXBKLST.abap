*&---------------------------------------------------------------------*
*& Report  ZRIMBWGILST                                                 *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  프로그램명 : 기납증 현황                                           *
*&      작성자 : 이승준 INFOLINK Ltd.                                  *
*&      작성일 : 2001.09.19                                            *
*&---------------------------------------------------------------------*
*&  DESC.      :
*&                LIST-UP
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

REPORT  ZRIMTAXBKLST   MESSAGE-ID ZIM
                       LINE-SIZE 130
                       NO STANDARD PAGE HEADING.
*----------------------------------------------------------------------*
*          TABLE,DATA, DEFINE                                          *
*----------------------------------------------------------------------*

TABLES : ZTTAXBKHD,
         EKKO,
         EKPO,
         ZTTAXBKIT.

DATA   : W_ERR_CHK TYPE C VALUE 'N',   "ERROR CHECK.
         W_LINE  TYPE I,               "IT_TAB LINE 수.
         W_LINE1 TYPE I,              "소계용 LINE 수.
         W_SUM   LIKE ZTREQHD-ZFLASTAM, "원화 소계용.
         W_TOTAL LIKE ZTREQHD-ZFLASTAM, "원화 총계용.
         W_WERKS LIKE EKPO-WERKS,     "사업장.
         W_PRI LIKE ZTREQIT-NETPR,    "단가.
         W_FOR LIKE ZTREQHD-ZFLASTAM, "외화.
         W_WON LIKE ZTREQHD-ZFLASTAM, "외화.
         W_MOD LIKE SY-TABIX,         "홀짝.
         W_TABIX LIKE SY-TABIX.


DATA   : BEGIN OF IT_TAB OCCURS 0,
        ZFTBNO   LIKE ZTTAXBKHD-ZFTBNO,    "CHAR10기납증 관리번호
*>>출력 MAIN.
        ZFEBELN  LIKE ZTTAXBKHD-EBELN,     "10구매문서번호
        EBELN    LIKE ZTTAXBKHD-EBELN,     "10구매문서번호
        BASISNO  LIKE ZTTAXBKHD-BASISNO,   "22근거서류번호(승인서 번호)
        ZFTRNSNM LIKE ZTTAXBKHD-ZFTRNSNM,  "40양도자 상호
        ZFRVDT   LIKE ZTTAXBKHD-ZFRVDT,    "8접수일
        ZFBUYMN  LIKE ZTTAXBKHD-ZFBUYMN,   "QUAN13.3양도(매입)물량
        MEINS    LIKE ZTTAXBKHD-MEINS,	  "UNIT3기본단위
        ZFTBAK   LIKE ZTTAXBKHD-ZFTBAK, "CURR14.2과세가격-원화(공급가격)
        ZFKRW    LIKE ZTTAXBKHD-ZFKRW,     "CUKY5원화통화
        ZFREQNO  LIKE ZTTAXBKHD-ZFREQNO,   "10수입의뢰 관리번호
        ZFTBPNO  LIKE ZTTAXBKHD-ZFTBPNO,
                            "12접수번호[세관(3)+연도(2)+일련번호(7)]
        ZFSELSNM LIKE ZTTAXBKHD-ZFSELSNM,  "40양수자 상호
        ZFBUYDT  LIKE ZTTAXBKHD-ZFBUYDT,   "8양도(매입)일자
        STAWN    LIKE ZTTAXBKHD-STAWN,     "17해외무역의상품수입코드번호
        ZFCUAMT  LIKE ZTTAXBKHD-ZFCUAMT,   "CURR14.2관세
*>> HD 참조
        BUKRS    LIKE ZTTAXBKHD-BUKRS,	  "CHAR4회사코드
        ZFPOSYN  LIKE ZTTAXBKHD-ZFPOSYN,   "1  전표처리여부
        ZFACDO   LIKE ZTTAXBKHD-ZFACDO,    "10 회계전표 번호
        BELNR    LIKE ZTTAXBKHD-BELNR,     "10 회계전표번호
        ZFFIYR   LIKE ZTTAXBKHD-ZFFIYR,    "회계전표 연도
        GJAHR    LIKE ZTTAXBKHD-GJAHR,     "회계연도
*>> KEY---ZTTAXBKIT
        ZFTBIT   LIKE ZTTAXBKIT-ZFTBIT,    "기납증 Item No.(행번호)
*>>출력 SUB.
        MATNR    LIKE ZTTAXBKIT-MATNR,     "18 자재번호
        TXZ01    LIKE ZTTAXBKIT-TXZ01,     "40 내역(품명)
        BKMENGE  LIKE ZTTAXBKIT-BKMENGE,   "QUAN13.3기납증 수량
*> 동일 참조.
        MEINS_IT  LIKE ZTTAXBKIT-MEINS,     "UNIT3기본단위
        ZFTBAK_IT LIKE ZTTAXBKIT-ZFTBAK,"CURR14.2과세가격-원화(공급가격)
        ZFCUAMT_IT LIKE ZTTAXBKIT-ZFCUAMT,   "관세
*>> IT 참조
        EBELP    LIKE ZTTAXBKIT-EBELP.     "구매문서 품목번호
*>> 동일 미 참조.
*       EBELN     구매문서번호
*       ZFREQNO  수입의뢰 관리번호
*       ZFITMNO  수입문서 품목번호
*       STAWN    해외무역의 상품코드/수입코드번호
DATA   : END OF IT_TAB.


*----------------------------------------------------------------------*
*          SELECTION-SCREEN                                            *
*----------------------------------------------------------------------*

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS :
            S_BUKRS  FOR ZTTAXBKHD-BUKRS,    "회사코드
            S_EBELN  FOR ZTTAXBKHD-EBELN,    "P/O
            S_EKGRP  FOR EKKO-EKGRP,
            S_WERKS  FOR EKPO-WERKS,
            S_REQNO  FOR ZTTAXBKHD-ZFREQNO,  "수입의뢰
            S_BASIS  FOR ZTTAXBKHD-BASISNO,  "근거서류(승인번호)
            S_TBPNO  FOR ZTTAXBKHD-ZFTBPNO,  "접수번호
            S_TRNSNM FOR ZTTAXBKHD-ZFTRNSNM, "양도자

            S_ACDO   FOR ZTTAXBKHD-ZFACDO,   "비용문서 번호
            S_BELNR  FOR ZTTAXBKHD-BELNR,    "전표번호
            S_BUYDT  FOR ZTTAXBKHD-ZFBUYDT,  "매입일자
            S_POSYN  FOR ZTTAXBKHD-ZFPOSYN.  "전기여부

SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
*          INITIALIZATION.                                             *
*----------------------------------------------------------------------*

INITIALIZATION.                          " 초기값 SETTING
  CLEAR IT_TAB.
  REFRESH IT_TAB.

*----------------------------------------------------------------------*
*          START-OF-SELECTION.                                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*>>> READ TABLE
  PERFORM   P1000_READ_TAB.
*>>> SORT-GROUP:
  SORT IT_TAB BY ZFTBNO EBELN ZFREQNO ZFTBIT.
*>>> WRITE LIST
  PERFORM   P3000_WRITE_IT.

*----------------------------------------------------------------------*
*           TOP-OF-PAGE                                                *
*----------------------------------------------------------------------*
TOP-OF-PAGE.

  FORMAT RESET.
  WRITE : /50 '[   기 납 증   현 황   ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  SKIP.
      FORMAT COLOR 1 INTENSIFIED ON.
      WRITE :/ SY-ULINE(130).
      WRITE :/ SY-VLINE NO-GAP,(10) 'P/O 번호'  CENTERED  NO-GAP,
               SY-VLINE NO-GAP,(22) '근거서류'  CENTERED  NO-GAP,
               SY-VLINE NO-GAP,(35) '양 도 자'  CENTERED  NO-GAP,
               SY-VLINE NO-GAP, (14) '접수일자'  CENTERED  NO-GAP,
               SY-VLINE NO-GAP,(18) '매입물량'  CENTERED  NO-GAP,
               SY-VLINE NO-GAP,(21) '공급가격' CENTERED  NO-GAP.
      WRITE : 130 SY-VLINE.
      FORMAT COLOR 1 INTENSIFIED OFF.
      WRITE :/
               SY-VLINE NO-GAP,(10) '수입의뢰' CENTERED   NO-GAP,
               SY-VLINE NO-GAP,(22) '접수번호' CENTERED   NO-GAP,
               SY-VLINE NO-GAP,(35) '양 수 자'   CENTERED   NO-GAP,
               SY-VLINE NO-GAP,(14)  '매입일자' CENTERED   NO-GAP,
               SY-VLINE NO-GAP,(18) 'H/S CODE' CENTERED   NO-GAP,
               SY-VLINE NO-GAP,(21) '관    세' CENTERED   NO-GAP.
      WRITE : 130 SY-VLINE.

      FORMAT COLOR 2 INTENSIFIED ON.
      WRITE :/
               SY-VLINE NO-GAP,(10) 'Item'                      NO-GAP,
               SY-VLINE NO-GAP,(22) '자재코드'   CENTERED   NO-GAP,
               SY-VLINE NO-GAP,(35) '품    명'   CENTERED   NO-GAP,
               SY-VLINE NO-GAP,(14) '수    량'   CENTERED   NO-GAP,
               SY-VLINE NO-GAP,(18) '공급가격'   CENTERED   NO-GAP,
               SY-VLINE NO-GAP,(21) '관    세'   CENTERED   NO-GAP.
      WRITE : 130 SY-VLINE.
      WRITE :/ SY-ULINE(130).


*-----------------------------------------------------------------------
* User Command
*
*-----------------------------------------------------------------------
AT USER-COMMAND.

 CASE SY-UCOMM.
*>>>>P/O CALL
      WHEN 'PODP'.
          IF W_TABIX EQ 0. MESSAGE E962 .
          ELSE.
             IF NOT IT_TAB-EBELN IS INITIAL.
                PERFORM  P2000_PO_DOC_DISPLAY(SAPMZIM01)
                         USING IT_TAB-EBELN ''.
             ENDIF.
          ENDIF.
*>>>>수입의뢰 CALL
      WHEN 'LCDP'.
          IF W_TABIX EQ 0. MESSAGE E962.
          ELSE.
          PERFORM P2000_SHOW_LC USING  IT_TAB-ZFREQNO.
          ENDIF.
*>> 기납증 CALL
      WHEN 'TBDP'.
          IF W_TABIX EQ 0. MESSAGE E962.
          ELSE.
             SET PARAMETER ID 'BES'     FIELD ''.
             SET PARAMETER ID 'ZPREQNO' FIELD ''.
             SET PARAMETER ID 'ZPOPNNO' FIELD ''.
             SET PARAMETER ID 'ZPTBNO'  FIELD IT_TAB-ZFTBNO.
             CALL TRANSACTION 'ZIMZ3' AND SKIP FIRST SCREEN.
          ENDIF.

*>>>>비용문서 CALL
*--> BUKRS  CHAR4회사코드
*--> ZFACDO CHAR10 회계전표 번호
*--> ZFFIYR 회계전표 연도


      WHEN 'ZIMY'.
          IF W_TABIX EQ 0. MESSAGE E962.
          ELSE.
          PERFORM P2000_SHOW_ZIMY3 USING IT_TAB-BUKRS
                                         IT_TAB-ZFFIYR
                                         IT_TAB-ZFACDO.
          ENDIF.

*>>>>회계전표 CALL
*--> BELNR CHAR10 회계전표번호
*--> GJAHR 회계연도
      WHEN 'FIDS'.
          IF W_TABIX EQ 0. MESSAGE E962.
          ELSE.
          PERFORM P2000_FI_MIR4_FB03 USING IT_TAB-BUKRS
                                           IT_TAB-GJAHR
                                           IT_TAB-BELNR.
          ENDIF.
 ENDCASE.
 CLEAR: IT_TAB ,W_TABIX.

*>>>>P/O CALL
*--->EBELN     구매문서번호
*--->EBELP     구매문서 품목번호
*WHEN 'PODP'.
*        IF NOT IT_TAB-EBELN IS INITIAL.
*            SET PARAMETER ID 'BES' FIELD IT_TAB-EBELN.
*            CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
*         ENDIF.
*

*>>>>수입의뢰 CALL
*--->ZFREQNO  수입의뢰 관리번호
*PERFORM P2000_SHOW_LC USING  IT_SELECTED-ZFREQNO
*                                         IT_SELECTED-ZFAMDNO.
*FORM P2000_SHOW_LC USING    P_ZFREQNO P_ZFAMDNO.
*
*  SET PARAMETER ID 'BES'       FIELD ''.
*  SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
*  SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
*  SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.
*  EXPORT 'BES'           TO MEMORY ID 'BES'.
*  EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
*  EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.
*  EXPORT 'ZPAMDNO'       TO MEMORY ID 'ZPAMDNO'.
*
*  IF P_ZFAMDNO = '00000'.
*      CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
*  ELSE.
*      CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
*  ENDIF.
*
*ENDFORM.                    " P2000_SHOW_LC

*>>>>비용문서 CALL
*--->*s BUKRS	  "CHAR4회사코드
*--->#*s ZFACDO         "CHAR10 회계전표 번호
*---> ZFFIYR회계전표 연도
*---> GJAHR회계연도
*FORM P4000_DISPLAY_DOCUMENT.
*
*   LOOP  AT  IT_SELECTED.
*
*      SET  PARAMETER ID  'BUK'       FIELD   IT_SELECTED-BUKRS.
*      SET  PARAMETER ID  'GJR'       FIELD   IT_SELECTED-GJAHR.
*      SET  PARAMETER ID  'ZPBENR'    FIELD   IT_SELECTED-BELNR.
*      CALL TRANSACTION 'ZIMY3'.
*
*   ENDLOOP.
*
*ENDFORM.                    " P4000_DISPLAY_DOCUMENT

*>>>>회계전표 CALL
*--->BELNR          "CHAR10 회계전표번호


*FORM P2000_FI_DOCUMENT_DISPLAY USING    P_BUKRS
*                                        P_GJAHR
*                                        P_BELNR.
*
*   IF P_BELNR IS INITIAL.
*      MESSAGE S589.   EXIT.
*   ELSE.
**>>> LIV 전표번호인지, 회계전표인지를 구분.
*      SELECT * FROM EKBZ UP TO 1 ROWS
*               WHERE BELNR EQ P_BELNR
*               AND   GJAHR EQ P_GJAHR.
*      ENDSELECT.
*      IF SY-SUBRC NE 0.
*         SELECT * FROM EKBE UP TO 1 ROWS
*                  WHERE BELNR EQ P_BELNR
*                  AND   GJAHR EQ P_GJAHR.
*         ENDSELECT.
*      ENDIF.
*      IF SY-SUBRC EQ 0.
*         SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
*         SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
*         SET PARAMETER ID 'RBN'    FIELD P_BELNR.
*         CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
*      ELSE.
*         SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
*         SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
*         SET PARAMETER ID 'BLN'    FIELD P_BELNR.
*         CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
*      ENDIF.
*   ENDIF.
*
*ENDFORM.                    " P2000_FI_DOCUMENT_DISPLAY











*DATA : W_COUNT    TYPE I,
*       saupja_no(15),
*       pre_dty_val(17),
*       zfblno(10) type c,
*       bank_nm(20) type c,
*       cntr_nm(50) type c,
*       objt_cd(840) type c,
*       arr_area_txt(70) type c,
*       st_area_txt(70) type c,
*       last_area_txt(70) type c,
*       clm_agent(300) type c,
*       svy_agent(300) type c,
*       yak_cd(005)    type c,
*       yak_nm_yak(30) type c,
*       inga_nm(300)   type c,
*
*       cont_date(9)   type c,
*       prt_bal_date   type d,
*       pre_inv_val(30)    type c.
**  EXEC SQL PERFORMING  loop_output.
**     SELECT count( * )
**            INTO :w_count
**            FROM  ztbl
**            WHERE zfblno >= :'1000000000'
**  ENDEXEC.
**   EXEC SQL PERFORMING  loop_output1.
**      SELECT yak_id, yak_nm_yak, inga_nm
**            INTO :yak_cd, :yak_nm_yak, :inga_nm
**            FROM zlgitmva0
**   ENDEXEC.
*   EXEC SQL PERFORMING  loop_output.
*     SELECT  saupja_no,
*             bank_nm, cntr_nm, objt_cd, arr_area_txt, st_area_txt,
*             last_area_txt, clm_agent, svy_agent, cont_date,
*             pre_inv_val, prt_bal_date, pre_dty_val
*         INTO :saupja_no,
*              :bank_nm, :cntr_nm, :objt_cd, :arr_area_txt
*              :st_area_txt, :last_area_txt, :clm_agent,
*              :svy_agent, :cont_date, :pre_inv_val, :prt_bal_date,
*              :pre_dty_val
*         FROM zlgitmv20
*   ENDEXEC.
**         FROM tmv20@lgi.world
**            FROM tmv20@lgc_to_lgi
**         FROM tmv20@lgc_to_lgi
*form loop_output.
*  WRITE : / saupja_no, cont_date, pre_inv_val, prt_bal_date,
*            pre_dty_val.
**      bank_nm, cntr_nm, objt_cd, arr_area_txt, st_area_txt,
**              last_area_txt, clm_agent, svy_agent.
*endform.
*
*form loop_output1.
*  WRITE : / inga_nm.
*
*endform.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_TAB.
*  CLEAR IT_TAB.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
           FROM ZTTAXBKHD AS H JOIN ZTTAXBKIT AS I
             ON H~ZFTBNO EQ I~ZFTBNO
           WHERE H~BUKRS    IN S_BUKRS
             AND H~EBELN    IN S_EBELN
             AND H~ZFREQNO  IN S_REQNO
             AND H~BASISNO  IN S_BASIS
             AND H~ZFTBPNO  IN S_TBPNO
             AND H~ZFTRNSNM IN S_TRNSNM
             AND H~ZFACDO   IN S_ACDO
             AND H~BELNR    IN S_BELNR
             AND H~ZFBUYDT  IN S_BUYDT
             AND H~ZFPOSYN  IN S_POSYN.
  IF SY-SUBRC NE 0.  MESSAGE S738. EXIT. ENDIF.
  LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      CLEAR ZTTAXBKIT.
      SELECT SINGLE * FROM ZTTAXBKIT
              WHERE ZFTBNO EQ IT_TAB-ZFTBNO
                AND ZFTBIT EQ IT_TAB-ZFTBIT.
     CLEAR EKKO.
     SELECT SINGLE *
            FROM  EKKO
           WHERE EBELN = IT_TAB-EBELN
             AND EKGRP IN S_EKGRP.
     IF SY-SUBRC NE 0.
        DELETE IT_TAB INDEX W_TABIX.
        CONTINUE.
     ENDIF.
     SELECT SINGLE *
            FROM EKPO
           WHERE EBELN = IT_TAB-EBELN
             AND EBELP = IT_TAB-EBELP
             AND WERKS  IN S_WERKS.
     IF SY-SUBRC NE 0.
         DELETE IT_TAB INDEX W_TABIX.
         CONTINUE.
     ENDIF.

     SELECT SINGLE * FROM ZTTAXBKHD
            WHERE  ZFTBNO EQ IT_TAB-ZFTBNO.

     MOVE  ZTTAXBKHD-EBELN TO IT_TAB-ZFEBELN.
     MOVE  ZTTAXBKIT-MEINS TO IT_TAB-MEINS_IT.
     MOVE  ZTTAXBKIT-ZFTBAK TO IT_TAB-ZFTBAK_IT.
     MOVE  ZTTAXBKIT-ZFCUAMT TO IT_TAB-ZFCUAMT_IT.
     MODIFY IT_TAB INDEX W_TABIX.
     CLEAR W_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_TAB
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_IT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_IT.

  SET TITLEBAR 'ZIMZ4'.
  SET PF-STATUS 'ZIMZ4'.
  DESCRIBE TABLE IT_TAB LINES W_LINE.
  CLEAR : IT_TAB,W_TABIX.
  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    FORMAT RESET.
    ON CHANGE OF IT_TAB-ZFTBNO.
      FORMAT COLOR 1 INTENSIFIED ON.
      IF W_TABIX NE 1.
         WRITE :/ SY-ULINE(130).
      ENDIF.
      WRITE :/ SY-VLINE NO-GAP,(10) IT_TAB-ZFEBELN    NO-GAP,
               SY-VLINE NO-GAP,(22) IT_TAB-BASISNO    NO-GAP,
               SY-VLINE NO-GAP,(35) IT_TAB-ZFTRNSNM   NO-GAP,
               SY-VLINE NO-GAP, (14) IT_TAB-ZFRVDT     NO-GAP,
               SY-VLINE NO-GAP,(15) IT_TAB-ZFBUYMN
                               UNIT IT_TAB-MEINS      NO-GAP,
                                (3) IT_TAB-MEINS      NO-GAP,
               SY-VLINE NO-GAP,(16) IT_TAB-ZFTBAK
                               CURRENCY IT_TAB-ZFKRW  NO-GAP,
                                (5) IT_TAB-ZFKRW      NO-GAP.
       WRITE : 130 SY-VLINE.
      HIDE: IT_TAB, W_TABIX.
      FORMAT COLOR 1 INTENSIFIED OFF.
      WRITE :/
               SY-VLINE NO-GAP,(10) IT_TAB-ZFREQNO    NO-GAP,
               SY-VLINE NO-GAP,(22) IT_TAB-ZFTBPNO    NO-GAP,
               SY-VLINE NO-GAP,(35) IT_TAB-ZFSELSNM   NO-GAP,
               SY-VLINE NO-GAP, (14) IT_TAB-ZFBUYDT    NO-GAP,
               SY-VLINE NO-GAP,(18) IT_TAB-STAWN      NO-GAP,
               SY-VLINE NO-GAP,(16) IT_TAB-ZFCUAMT
                               CURRENCY IT_TAB-ZFKRW  NO-GAP,
                                (5) IT_TAB-ZFKRW      NO-GAP.
      WRITE : 130 SY-VLINE.

      HIDE: IT_TAB, W_TABIX.
    ENDON.
    W_MOD = SY-TABIX MOD 2.
    IF W_MOD = 1.
       FORMAT RESET.
       FORMAT COLOR 2 INTENSIFIED ON.
    ELSE.
       FORMAT RESET.
       FORMAT COLOR 2 INTENSIFIED OFF.
    ENDIF.
    WRITE :/
             SY-VLINE NO-GAP,(10) IT_TAB-ZFTBIT      NO-GAP,
             SY-VLINE NO-GAP,(22) IT_TAB-MATNR       NO-GAP,
             SY-VLINE NO-GAP,(35) IT_TAB-TXZ01 NO-GAP, "ZFTRNSNM
             SY-VLINE NO-GAP,(12) IT_TAB-BKMENGE
                                UNIT IT_TAB-MEINS    NO-GAP,
                              (2) IT_TAB-MEINS_IT    NO-GAP,
             SY-VLINE NO-GAP,(15) IT_TAB-ZFTBAK_IT
                               CURRENCY IT_TAB-ZFKRW NO-GAP,
                               (3) IT_TAB-ZFKRW      NO-GAP,
             SY-VLINE NO-GAP,(16) IT_TAB-ZFCUAMT_IT
                               CURRENCY IT_TAB-ZFKRW NO-GAP,
                              (3) IT_TAB-ZFKRW       NO-GAP.
    WRITE : 130 SY-VLINE.
    HIDE: IT_TAB, W_TABIX.
    AT LAST.
       WRITE :/ SY-ULINE(130).
       WRITE : /5 W_LINE , '건'.
    ENDAT.
  ENDLOOP.
  CLEAR: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_WRITE_IT
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_ZFREQNO  text
*      -->P_ENDIF  text
*----------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.
*                            P_ZFAMDNO.

  SET PARAMETER ID 'BES'       FIELD ''.
  SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
  SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
  SET PARAMETER ID 'ZPAMDNO'   FIELD ''.
  EXPORT 'BES'           TO MEMORY ID 'BES'.
  EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
  EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.
  EXPORT 'ZPAMDNO'       TO MEMORY ID 'ZPAMDNO'.

*  IF P_ZFAMDNO = '00000'.
      CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
*  ELSE.
*      CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
*  ENDIF.


ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_ZIMY3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_BUKRS  text
*      -->P_IT_TAB_ZFFIYR  text
*      -->P_IT_TAB_ZFACDO  text
*----------------------------------------------------------------------*
FORM P2000_SHOW_ZIMY3 USING    P_BUKRS
                               P_ZFFIYR
                               P_ZFACDO.
       IF P_ZFACDO IS INITIAL.
          MESSAGE S589.   EXIT.
       ELSE.
       SET  PARAMETER ID  'BUK'       FIELD   P_BUKRS.
       SET  PARAMETER ID  'GJR'       FIELD   P_ZFFIYR.
       SET  PARAMETER ID  'ZPBENR'    FIELD   P_ZFACDO.
       CALL TRANSACTION 'ZIMY3'.
       ENDIF.

ENDFORM.                    " P2000_SHOW_ZIMY3
*&---------------------------------------------------------------------*
*&      Form  P2000_FI_MIR4_FB03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_BUKRS  text
*      -->P_IT_TAB_GJAHR  text
*      -->P_IT_TAB_BELNR  text
*----------------------------------------------------------------------*
FORM P2000_FI_MIR4_FB03 USING    P_BUKRS
                                 P_GJAHR
                                 P_BELNR.
   IF P_BELNR IS INITIAL.
      MESSAGE S589.   EXIT.
   ELSE.
*>>> LIV 전표번호인지, 회계전표인지를 구분.
*      SELECT * FROM EKBZ UP TO 1 ROWS
*               WHERE BELNR EQ P_BELNR
*               AND   GJAHR EQ P_GJAHR.
*      ENDSELECT.
*      IF SY-SUBRC NE 0.
*         SELECT * FROM EKBE UP TO 1 ROWS
*                  WHERE BELNR EQ P_BELNR
*                  AND   GJAHR EQ P_GJAHR.
*         ENDSELECT.
*      ENDIF.
*      IF SY-SUBRC EQ 0.
*         SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
*         SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
*         SET PARAMETER ID 'RBN'    FIELD P_BELNR.
*         CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
*      ELSE.
         SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
         SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
         SET PARAMETER ID 'BLN'    FIELD P_BELNR.
         CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
*      ENDIF.
   ENDIF.
ENDFORM.                    " P2000_FI_MIR4_FB03
