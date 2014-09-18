*&---------------------------------------------------------------------*
*& Report  ZRIMBLLIST                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입관련비용 현황                                     *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.10.13                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&     1. EXCEL FILE로 DOWNLOAD를 위해 ALV 기능 활용.
*&     2. 해당 비용문서, 회계문서, 관련문서 등으로 .
*&        CALL Transaction 기능 삽입.
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMCOSTLST  MESSAGE-ID ZIM
                     NO STANDARD PAGE HEADING.

TYPE-POOLS: SLIS.

INCLUDE RVREUSE_GLOBAL_DATA.
INCLUDE RVREUSE_LOCAL_DATA.
INCLUDE RVREUSE_FORMS.
INCLUDE <ICON>.

*> 테이블 선언.
TABLES : ZTBKPF,
         ZTBSEG,
         ZTBDIV,
         ZTBHIS,
         ZTREQHD,
         EKBZ,
         EKBE,
         ZTBL,
         ZTIV,
         ZTIDS,
         LFA1,
         ZVIMCOST,
         ZVCOSTDIV,
         ZTIDR,
         ZTCUCLIV,
         ZTTAXBKHD,
         ZTCGHD,
         ZTMSHD,
         ZTBLCST.

* 수입비용 Internal Table Define..
DATA : IT_ZSIMCOST LIKE ZSIMCOSTDN OCCURS 0 WITH HEADER LINE.

*> 데이타 선언.
DATA : PT_FIELDCAT    TYPE SLIS_T_FIELDCAT_ALV with header line.

DATA: G_REPID   LIKE SY-REPID.
DATA  G_SAVE(1) TYPE C.
DATA  G_VARIANT LIKE DISVARIANT.
DATA  G_USER_COMMAND   TYPE SLIS_FORMNAME VALUE 'P2000_ALV_COMMAND'.
DATA  G_STATUS         TYPE SLIS_FORMNAME VALUE 'P2000_ALV_PF_STATUS'.


DATA : W_TABIX       LIKE   SY-TABIX,
       W_SUBRC       LIKE   SY-SUBRC.


*-----------------------------------------------------------------------
* Selection Screen 절.
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS     FOR    ZVIMCOST-BUKRS,
                   S_GJAHR     FOR    ZVIMCOST-GJAHR
                                      DEFAULT SY-DATUM(4),
                   S_MONAT     FOR    ZVIMCOST-MONAT,
*                                      DEFAULT SY-DATUM+4(2),
                   S_POSTYN    FOR    ZVIMCOST-ZFPOSYN
                                      DEFAULT 'Y',
                   S_LIFNR     FOR    ZVIMCOST-LIFNR,     "지불처
                   S_ZFVEN     FOR    ZVIMCOST-ZFVEN,     "구매처
                   S_CSTGRP    FOR    ZVIMCOST-ZFCSTGRP,
                   S_ZFCD      FOR    ZVIMCOST-ZFCD,
                   S_CNAME     FOR    ZVIMCOST-ZFCNAME,
                   S_BUPLA     FOR    ZVIMCOST-BUPLA,
                   S_BLDAT     FOR    ZVIMCOST-BLDAT,
                   S_BUDAT     FOR    ZVIMCOST-BUDAT,
                   S_GSBER     FOR    ZVIMCOST-GSBER,
                   S_KOSTL     FOR    ZVIMCOST-KOSTL,
                   S_POSID     FOR    ZVIMCOST-PS_POSID,
                   S_MWSKZ     FOR    ZVIMCOST-MWSKZ,
                   S_COND      FOR    ZVIMCOST-COND_TYPE,
                   S_IMDNO     FOR    ZVIMCOST-ZFIMDNO,
                   S_ZTERM     FOR    ZVIMCOST-ZTERM,
                   S_ZFBDT     FOR    ZVIMCOST-ZFBDT,
                   S_ZUONR     FOR    ZVIMCOST-ZUONR,
                   S_ZLSCH     FOR    ZVIMCOST-ZLSCH,
                   S_ZLSPR     FOR    ZVIMCOST-ZLSPR,
                   S_ADVPT     FOR    ZVIMCOST-ZFADVPT,
                   S_HKONT     FOR    ZVIMCOST-HKONT,
                   S_DCSTX     FOR    ZVIMCOST-ZFDCSTX,
                   S_TBTKZ     FOR    ZVIMCOST-TBTKZ,
                   S_ZFPOYN    FOR    ZVIMCOST-ZFPOYN,
                   S_DOCNOR    FOR    ZVIMCOST-ZFDOCNOR,
                   S_NEWBS     FOR    ZVIMCOST-NEWBS,
                   S_SHKZG     FOR    ZVIMCOST-SHKZG,
                   S_ZFRVSX    FOR    ZVIMCOST-ZFRVSX,
                   S_USNAM     FOR    ZVIMCOST-USNAM,
                   S_CPUDT     FOR    ZVIMCOST-CPUDT,
                   S_CPUTM     FOR    ZVIMCOST-CPUTM.
SELECTION-SCREEN END OF BLOCK B1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZFCD-LOW.
   PERFORM   P1000_COST_CODE_HELP  USING  S_ZFCD-LOW 'S_ZFCD-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZFCD-HIGH.
   PERFORM   P1000_COST_CODE_HELP  USING  S_ZFCD-HIGH 'S_ZFCD-HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-LOW.
   PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-HIGH.
   PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-HIGH.




* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM  P1000_GET_INITIAL_VALUE.

START-OF-SELECTION.

   PERFORM  P1000_GET_COST_DATA.

   PERFORM  P2000_CALL_ALV_FUNCTION.


*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_ALV_PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.

  SET TITLEBAR 'ZIMR85'.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING EXTAB.

ENDFORM.                    " P2000_ALV_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_ALV_COMMAND USING R_UCOMM      LIKE SY-UCOMM
                              RS_SELFIELD TYPE SLIS_SELFIELD.
*
  CASE R_UCOMM.
    WHEN  '&IC1' OR 'ZIMY3'.             " 비용문서 & doubleclick
       READ TABLE IT_ZSIMCOST INDEX RS_SELFIELD-TABINDEX. "cursorposit.
       IF SY-SUBRC EQ 0.
         PERFORM  P2000_COST_DOCUMENT_DISPLAY
                                USING   IT_ZSIMCOST-BUKRS
                                        IT_ZSIMCOST-BELNR
                                        IT_ZSIMCOST-GJAHR.
       ELSE.
          MESSAGE S962.
       ENDIF.
    WHEN 'DISP'.
       READ TABLE IT_ZSIMCOST INDEX RS_SELFIELD-TABINDEX. "cursorposit.
       IF SY-SUBRC EQ 0.
          PERFORM  P2000_REF_DOC_DISPLAY USING IT_ZSIMCOST-ZFCSTGRP
                                               IT_ZSIMCOST-ZFCD
                                               IT_ZSIMCOST-ZFIMDNO
                                               IT_ZSIMCOST-ZFAMDNO
                                               IT_ZSIMCOST-ZFINSEQ.

       ELSE.
          MESSAGE S962.
       ENDIF.
     WHEN 'FB03'.      ">회계문서.
       READ TABLE IT_ZSIMCOST INDEX RS_SELFIELD-TABINDEX. "cursorposit.
       IF SY-SUBRC EQ 0.
          PERFORM  P2000_FI_DOCUMENT_DISPLAY
                                USING   IT_ZSIMCOST-BUKRS
                                        IT_ZSIMCOST-ZFFIYR
                                        IT_ZSIMCOST-ZFACDO.
       ELSE.
          MESSAGE S962.
       ENDIF.

  ENDCASE.
  CLEAR R_UCOMM.

ENDFORM.                    " P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_COST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_GET_COST_DATA.

*>>> 진행상태바..
   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
*           PERCENTAGE = 0
           TEXT       = TEXT-002.


   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMCOST
            FROM ZVIMCOST
            WHERE  BUKRS     IN S_BUKRS
            AND    BUPLA     IN S_BUPLA
            AND    BLDAT     IN S_BLDAT
            AND    BUDAT     IN S_BUDAT
            AND    GSBER     IN S_GSBER
            AND    LIFNR     IN S_LIFNR
            AND    ZFVEN     IN S_ZFVEN
            AND    MWSKZ     IN S_MWSKZ
            AND    ZFCSTGRP  IN S_CSTGRP
            AND    ZFCD      IN S_ZFCD
            AND    COND_TYPE IN S_COND
            AND    ZFIMDNO   IN S_IMDNO
            AND    ZTERM     IN S_ZTERM
            AND    GJAHR     IN S_GJAHR
            AND    MONAT     IN S_MONAT
            AND    ZFBDT     IN S_ZFBDT
            AND    ZUONR     IN S_ZUONR
            AND    ZLSCH     IN S_ZLSCH
            AND    ZLSPR     IN S_ZLSPR
            AND    ZFPOSYN   IN S_POSTYN
            AND    ZFCNAME   IN S_CNAME
            AND    ZFADVPT   IN S_ADVPT
            AND    HKONT     IN S_HKONT
            AND    ZFDCSTX   IN S_DCSTX
            AND    TBTKZ     IN S_TBTKZ
            AND    KOSTL     IN S_KOSTL
            AND    PS_POSID  IN S_POSID
            AND    NEWBS     IN S_NEWBS
            AND    SHKZG     IN S_SHKZG
            AND    ZFDOCNOR  IN S_DOCNOR
            AND    ZFPOYN    IN S_ZFPOYN
            AND    ZFRVSX    IN S_ZFRVSX
            AND    USNAM     IN S_USNAM
            AND    CPUDT     IN S_CPUDT
            AND    CPUTM     IN S_CPUTM.

   LOOP AT IT_ZSIMCOST.
      W_TABIX = SY-TABIX.
*>거래처명 SELECT.
      SELECT SINGLE NAME1 STCD2
             INTO (IT_ZSIMCOST-NAME1, IT_ZSIMCOST-STCD2)
             FROM  LFA1
             WHERE LIFNR EQ IT_ZSIMCOST-LIFNR.

*>비용그룹 TEXT
      PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                USING      'ZDCSTGRP'  IT_ZSIMCOST-ZFCSTGRP
                CHANGING   IT_ZSIMCOST-ZFGRPTX.

*>비용항목.
      SELECT SINGLE ZFCDNM
             INTO  IT_ZSIMCOST-ZFCDNM
             FROM  ZTIMIMG08
             WHERE ZFCDTY EQ IT_ZSIMCOST-ZFCSTGRP
             AND   ZFCD   EQ IT_ZSIMCOST-ZFCD.


*>문서명.
      PERFORM P1000_IMPORT_DOC_CHEKC   USING IT_ZSIMCOST-ZFCSTGRP
                                             IT_ZSIMCOST-ZFIMDNO
                                             IT_ZSIMCOST-ZFDCNM.
      IF IT_ZSIMCOST-SHKZG EQ 'H'.
         IT_ZSIMCOST-WRBTR = IT_ZSIMCOST-WRBTR * -1.
         IT_ZSIMCOST-DMBTR = IT_ZSIMCOST-DMBTR * -1.
         IT_ZSIMCOST-FWBAS = IT_ZSIMCOST-FWBAS * -1.
         IT_ZSIMCOST-WMWST = IT_ZSIMCOST-WMWST * -1.
      ENDIF.
*>변경.
      MODIFY IT_ZSIMCOST INDEX W_TABIX.
   ENDLOOP.

ENDFORM.                    " P1000_GET_COST_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_INITIAL_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_GET_INITIAL_VALUE.

   SET TITLEBAR 'ZIMR85'.

ENDFORM.                    " P1000_GET_INITIAL_VALUE
*&---------------------------------------------------------------------*
*&      Form  P2000_CALL_ALV_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_CALL_ALV_FUNCTION.

   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
*           PERCENTAGE = 0
           TEXT       = TEXT-003.

   G_REPID = SY-REPID.
   CLEAR G_VARIANT.
   G_VARIANT-REPORT = G_REPID.

   CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = G_REPID
            I_STRUCTURE_NAME         = 'ZSIMCOSTDN'
            I_CALLBACK_PF_STATUS_SET = G_STATUS
            I_CALLBACK_USER_COMMAND  = G_USER_COMMAND
            I_SAVE                   = G_SAVE
            IS_VARIANT               = G_VARIANT
       TABLES
            T_OUTTAB                 = IT_ZSIMCOST.


ENDFORM.                    " P2000_CALL_ALV_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  P1000_IMPORT_DOC_CHEKC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSIMCOST_ZFCSTGRP  text
*      -->P_IT_ZSIMCOST_ZFIMDNO  text
*      -->P_IT_ZSIMCOST_ZFDCNM  text
*----------------------------------------------------------------------*
FORM P1000_IMPORT_DOC_CHEKC USING    P_ZFCSTGRP
                                     P_ZFIMDNO
                                     P_ZFDCNM.

   CLEAR : P_ZFDCNM, ZTREQHD, ZTBL, ZTIV, ZTCGHD, ZTMSHD.

   IF P_ZFIMDNO IS INITIAL.
      EXIT.
   ENDIF.

   CASE P_ZFCSTGRP.
      WHEN '003'.           ">수입의뢰.
         SELECT SINGLE * FROM ZTREQHD
                         WHERE ZFREQNO EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.
         MOVE: ZTREQHD-ZFOPNNO TO P_ZFDCNM.
      WHEN '004' OR '005'.  ">B/L 관리번호.
         SELECT SINGLE * FROM ZTBL
                         WHERE ZFBLNO  EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.
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

      WHEN '006'.           ">통관관리번호.
         SELECT SINGLE * FROM ZTIV
                         WHERE ZFIVNO  EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.
*MODIFIED BY SEUNGYEON(2002.08.30)
         IF W_SUBRC EQ 0.
            IF ZTIV-ZFCUST EQ '3' OR ZTIV-ZFCUST EQ 'Y'.
*               SELECT SINGLE * FROM ZTCUCLIV
*                               WHERE ZFIVNO EQ P_ZFIMDNO.
*               IF SY-SUBRC EQ 0.
                  SELECT SINGLE * FROM ZTIDR
                                  WHERE ZFIVNO EQ P_ZFIMDNO.
*                                  WHERE ZFBLNO  EQ ZTCUCLIV-ZFBLNO
*                                  AND   ZFCLSEQ EQ ZTCUCLIV-ZFCLSEQ.
                  IF SY-SUBRC EQ 0.
                     IF ZTIDR-ZFIDRNO IS INITIAL.
                        MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
                     ELSE.
                        MOVE: ZTIDR-ZFIDRNO TO P_ZFDCNM.
                     ENDIF.
                  ELSE.
                     MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
                  ENDIF.
*               ELSE.
*                  MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
*               ENDIF.
            ELSE.
               MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
            ENDIF.
         ENDIF.

      WHEN '007'.           ">하역관리번호.
         SELECT SINGLE * FROM ZTCGHD
                         WHERE ZFCGNO  EQ P_ZFIMDNO.
         W_SUBRC = SY-SUBRC.
         IF SY-SUBRC EQ 0.
            IF NOT ZTCGHD-ZFMSNO IS INITIAL.
               SELECT SINGLE * FROM  ZTMSHD
                               WHERE ZFMSNO  EQ  ZTCGHD-ZFMSNO.
               IF SY-SUBRC EQ 0.
                  MOVE ZTMSHD-ZFMSNM  TO  P_ZFDCNM.
               ENDIF.
            ENDIF.
         ENDIF.
      WHEN '008'.           ">기납증.
         SELECT SINGLE * FROM ZTTAXBKHD
                         WHERE ZFTBNO  EQ P_ZFIMDNO.
         IF SY-SUBRC EQ 0.
            IF ZTTAXBKHD-BASISNO IS INITIAL.
               MOVE ZTTAXBKHD-EBELN    TO  P_ZFDCNM.
            ELSE.
               MOVE ZTTAXBKHD-BASISNO  TO  P_ZFDCNM.
            ENDIF.
         ELSE.
            CLEAR : P_ZFDCNM.
         ENDIF.
      WHEN OTHERS.
         EXIT.
   ENDCASE.

ENDFORM.                    " P1000_IMPORT_DOC_CHEKC
*&---------------------------------------------------------------------*
*&      Form  P2000_FI_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTBKPF_BUKRS  text
*      -->P_ZTBKPF_ZFFIYR  text
*      -->P_ZTBKPF_ZFACDO  text
*----------------------------------------------------------------------*
FORM P2000_FI_DOCUMENT_DISPLAY USING    P_BUKRS
                                        P_GJAHR
                                        P_BELNR.

   IF P_BELNR IS INITIAL.
      MESSAGE S589.   EXIT.
   ELSE.
*>>> LIV 전표번호인지, 회계전표인지를 구분.
      SELECT * FROM EKBZ UP TO 1 ROWS
               WHERE BELNR EQ P_BELNR
               AND   GJAHR EQ P_GJAHR.
      ENDSELECT.
      IF SY-SUBRC NE 0.
         SELECT * FROM EKBE UP TO 1 ROWS
                  WHERE BELNR EQ P_BELNR
                  AND   GJAHR EQ P_GJAHR.
         ENDSELECT.
      ENDIF.
      IF SY-SUBRC EQ 0.
         SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
         SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
         SET PARAMETER ID 'RBN'    FIELD P_BELNR.
         CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
      ELSE.
         SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
         SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
         SET PARAMETER ID 'BLN'    FIELD P_BELNR.
         CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
      ENDIF.
   ENDIF.


ENDFORM.                    " P2000_FI_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_COST_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSIMCOST_BUKRS  text
*      -->P_IT_ZSIMCOST_BELNR  text
*      -->P_IT_ZSIMCOST_GJAHR  text
*----------------------------------------------------------------------*
FORM P2000_COST_DOCUMENT_DISPLAY USING    P_BUKRS
                                          P_BELNR
                                          P_GJAHR.

   SET PARAMETER ID 'ZPBENR' FIELD P_BELNR.
   SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
   SET PARAMETER ID 'BUK'    FIELD P_BUKRS.

   CALL TRANSACTION 'ZIMY3' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_COST_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_REF_DOC_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSIMCOST_ZFIMDNO  text
*----------------------------------------------------------------------*
FORM P2000_REF_DOC_DISPLAY USING  P_ZFCSTGRP
                                  P_ZFCD
                                  P_ZFIMDNO
                                  P_ZFAMDNO
                                  P_ZFINSEQ.

  CHECK : NOT P_ZFIMDNO IS INITIAL.

  CASE P_ZFCSTGRP.
     WHEN '003'.
        SET PARAMETER ID 'BES'     FIELD ''.
        SET PARAMETER ID 'ZPREQNO' FIELD P_ZFIMDNO.
        SET PARAMETER ID 'ZPOPNNO' FIELD ''.
        SET PARAMETER ID 'ZPINSEQ' FIELD P_ZFINSEQ.
        SET PARAMETER ID 'ZPAMDNO' FIELD P_ZFAMDNO.
        IF P_ZFCD EQ '1AB'.
           IF P_ZFAMDNO IS INITIAL.
              CALL TRANSACTION 'ZIM43' AND SKIP  FIRST SCREEN.
           ELSE.
              CALL TRANSACTION 'ZIM47' AND SKIP  FIRST SCREEN.
           ENDIF.
        ELSE.
           CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
        ENDIF.

     WHEN '004' OR '005'.
        SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFIMDNO.
        SET PARAMETER ID 'ZPHBLNO' FIELD ''.

        CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
     WHEN '006'.
        CLEAR : ZTIV.
        SELECT SINGLE * FROM  ZTIV
                        WHERE ZFIVNO  EQ  P_ZFIMDNO.
        IF SY-SUBRC EQ 0.
           CASE ZTIV-ZFCUST.
              WHEN '1' OR 'X'.
                 SET PARAMETER ID 'ZPIVNO'  FIELD P_ZFIMDNO.
                 SET PARAMETER ID 'ZPBLNO'  FIELD ''.
                 SET PARAMETER ID 'ZPHBLNO' FIELD ''.

                 CALL TRANSACTION 'ZIM33' AND SKIP  FIRST SCREEN.
              WHEN '2' OR '3' OR 'Y'.
*MODIFIED BY SEUNGYEON(2002.08.30)
*                 SELECT SINGLE * FROM  ZTCUCLIV
*                        WHERE ZFIVNO  EQ  P_ZFIMDNO.
                 CLEAR ZTCUCLIV.
                 IF ZTIV-ZFCUST EQ 'Y'.
                      SELECT SINGLE * FROM ZTIDS
                             INTO CORRESPONDING FIELDS OF ZTCUCLIV
                             WHERE ZFIVNO  EQ  P_ZFIMDNO.
                 ELSE.
                      SELECT SINGLE * FROM ZTIDR
                             INTO CORRESPONDING FIELDS OF ZTCUCLIV
                             WHERE ZFIVNO  EQ  P_ZFIMDNO.
                 ENDIF.
                 IF SY-SUBRC EQ 0.
                    SET PARAMETER ID 'ZPBLNO'  FIELD ZTCUCLIV-ZFBLNO.
                    SET PARAMETER ID 'ZPCLSEQ' FIELD ZTCUCLIV-ZFCLSEQ.
                    SET PARAMETER ID 'ZPHBLNO' FIELD ''.
                    SET PARAMETER ID 'ZPIDRNO' FIELD ''.

                    IF ZTIV-ZFCUST EQ 'Y'.
                       CALL TRANSACTION 'ZIM76' AND SKIP  FIRST SCREEN.
                    ELSE.
                       CALL TRANSACTION 'ZIM63' AND SKIP  FIRST SCREEN.
                    ENDIF.
                 ENDIF.
              WHEN OTHERS.
                 EXIT.
           ENDCASE.
        ELSE.
        ENDIF.

     WHEN '007'.
        SET PARAMETER ID 'ZPBLNO'  FIELD ''.
        SET PARAMETER ID 'ZPHBLNO' FIELD ''.
        SET PARAMETER ID 'ZPCGPT'  FIELD ''.
        SET PARAMETER ID 'ZPCGNO'  FIELD P_ZFIMDNO.

        CALL TRANSACTION 'ZIM83' AND SKIP  FIRST SCREEN.
     WHEN '008'.
        SET PARAMETER ID 'BES'     FIELD ''.
        SET PARAMETER ID 'ZPREQNO' FIELD ''.
        SET PARAMETER ID 'ZPOPNNO' FIELD ''.
        SET PARAMETER ID 'ZPINSEQ' FIELD ''.
        SET PARAMETER ID 'ZPTBNO'  FIELD P_ZFIMDNO.
        CALL TRANSACTION 'ZIMZ3' AND SKIP  FIRST SCREEN.
     WHEN OTHERS.
        EXIT.
  ENDCASE.

ENDFORM.                    " P2000_REF_DOC_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  P1000_COST_CODE_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_ZFCD_LOW  text
*      -->P_0245   text
*----------------------------------------------------------------------*
FORM P1000_COST_CODE_HELP USING    P_ZFCD P_FIELDNAME.

DATA : L_DISPLAY.

DATA: DYNPROG            LIKE SY-REPID,
      DYNNR              LIKE SY-DYNNR,
      WINDOW_TITLE(30)   TYPE C.
*>> 비용코드 HELP.
DATA : BEGIN OF IT_COST_HELP OCCURS 0,
       ZFCD      LIKE ZTIMIMG08-ZFCD,
       ZFCDNM    LIKE ZTIMIMG08-ZFCDNM,
       ZFCD1     LIKE ZTIMIMG08-ZFCD1,
       ZFCD5     LIKE ZTIMIMG08-ZFCD5,
       COND_TYPE LIKE ZTIMIMG08-COND_TYPE,
       END OF IT_COST_HELP.

  IF S_CSTGRP-LOW IS INITIAL.
      SELECT *
             INTO CORRESPONDING FIELDS OF TABLE IT_COST_HELP
             FROM   ZTIMIMG08
             WHERE  ZFCDTY   IN   ('003', '004', '005', '006', '007').
  ELSE.
      SELECT *
             INTO CORRESPONDING FIELDS OF TABLE IT_COST_HELP
             FROM   ZTIMIMG08
             WHERE  ZFCDTY   EQ   S_CSTGRP-LOW.
  ENDIF.
  IF SY-SUBRC NE 0.
     MESSAGE S406.
     EXIT.
  ENDIF.
  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.
*  W_FIELDNAME = 'ZTBSEG-ZFCD'.
*  W_FIELDNAME = P_FIELDNAME.
  WINDOW_TITLE = '비용코드 Help'.
  CLEAR: L_DISPLAY.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
           EXPORTING
**                RETFIELD        = 'OTYPE'
                RETFIELD        = 'ZFCD'
                DYNPPROG        = DYNPROG
                DYNPNR          = DYNNR
                DYNPROFIELD     = P_FIELDNAME
                WINDOW_TITLE    = WINDOW_TITLE
                VALUE_ORG       = 'S'
*                DISPLAY         = L_DISPLAY
           TABLES
                VALUE_TAB       = IT_COST_HELP
           EXCEPTIONS
                PARAMETER_ERROR = 1
                NO_VALUES_FOUND = 2
                OTHERS          = 3.
  IF SY-SUBRC <> 0.
     EXIT.
  ENDIF.

ENDFORM.                    " P1000_COST_CODE_HELP
*&---------------------------------------------------------------------*
*&      Form  P1000_PAY_TERM_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_ZTERM_LOW  text
*----------------------------------------------------------------------*
FORM P1000_PAY_TERM_HELP USING    P_ZTERM.

   TABLES : T052.

   CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = P_ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.

  IF SY-SUBRC NE 0.
*   message e177 with ekko-zterm.
    MESSAGE S177(06) WITH P_ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
     P_ZTERM = T052-ZTERM.
  ENDIF.

ENDFORM.                    " P1000_PAY_TERM_HELP
