*&---------------------------------------------------------------------*
*& Report  ZRIMCSTDOC                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 비용그룹별 수입관련문서 번호 Get
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.06.21                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : 비용그룹별 수입관련문서
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMCSTDOC   MESSAGE-ID ZIM
                     NO STANDARD PAGE HEADING.

TABLES : ZTREQHD,
         ZTREQST,
         ZTBL,
         ZTCGHD,
         ZTIV,
         ZVREQHD_ST,
         ZTIDR,
         ZVBL_IV,
         ZTCUCLIV,
         ZTIMIMG00,
         USR02,
         LFA1.

DATA : IT_ZVREQ      LIKE  ZVREQHD_ST OCCURS 0 WITH HEADER LINE.
DATA : IT_BLIV       LIKE  ZVBL_IV    OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_TAB OCCURS 0,
*>> 수입의뢰.
       ZFAPPDT    LIKE ZTREQST-ZFAPPDT,          " 요개설일?
       ZFREQDT    LIKE ZTREQST-ZFREQDT,          " 요개설일?
       ZFMAUD     LIKE ZTREQHD-ZFMAUD,           " 자재납기?
       ZFMATGB    LIKE ZTREQHD-ZFMATGB,          " 자재구?
       ZFREQTY    LIKE ZTREQST-ZFREQTY,          " Import Document Type
       EBELN      LIKE ZTREQHD-EBELN,            " Purchasing document
       LIFNR      LIKE ZTREQHD-LIFNR,            " Vendor Code
       NAME1(17),                                " Name 1
       ZFBENI     LIKE ZTREQHD-ZFBENI,           " Beneficairy
       NAME2(17),                                " Name 2
       ZFOPBN     LIKE ZTREQHD-ZFOPBN,           " Open Bank
       NAME3(17),                                " Name 1
       ZFLEVN     LIKE ZTREQHD-ZFLEVN,           " Open Bank
       NAME4(11),                                " Name 4.
       ZFREQNO    LIKE ZTREQHD-ZFREQNO,          " 수입의뢰 번?
       ZFOPNNO    LIKE ZTREQST-ZFOPNNO,          "> L/C No.
       ZTERM      LIKE ZTREQHD-ZTERM,
       ZFSPRT(18) TYPE C,                        " 선적?
       INCO1      LIKE ZTREQHD-INCO1,
       ZFAMDNO    LIKE ZTREQST-ZFAMDNO,          " Amend Seq.
       ZFOPAMT1(18) TYPE C,                      " 개설금액 TEXT
       WAERS      LIKE ZTREQST-WAERS,            " Currency
       ZFUSD      LIKE ZTREQST-ZFUSD,            " Currency
       ZFUSDAM1(18) TYPE C,                      " USD 환산금액 TEXT
       ZFOPAMT    LIKE ZTREQST-ZFOPAMT,          " 개설금액 TEXT
       ZFUSDAM    LIKE ZTREQST-ZFUSDAM,          " 개설금액 TEXT
       EKORG      LIKE ZTREQST-EKORG,            " Purchasing organizati
       EKGRP      LIKE ZTREQST-EKGRP,            " Purchasing group
       ZFBACD     LIKE ZTREQHD-ZFBACD,
       ZFWERKS    LIKE ZTREQHD-ZFWERKS,
       ZFAPRT(18),
       ZFTRANS    LIKE ZTREQHD-ZFTRANS,
       ZFREF1(11),
       ZFRLST1    LIKE ZTREQST-ZFRLST1,          " 의뢰 Release 상?
       ZFRLDT1    LIKE ZTREQST-ZFRLDT1,          " 의뢰 Release 일?
       ZFRLNM1    LIKE ZTREQST-ZFRLNM1,          " 의뢰 Release 담당?
       ZFCLOSE    LIKE ZTREQHD-ZFCLOSE,          " 수입의뢰 종료여?
       ZFRLST2    LIKE ZTREQST-ZFRLST2,          " 개설 Release 상?
       ZFDOCST    LIKE ZTREQST-ZFDOCST,          " 개설 Release 상?
       ERNAM      LIKE ZTREQST-ERNAM,            " 구매담?
       PS_POSID    LIKE  ZTBL-PS_POSID,
       ZFBLNO      LIKE  ZTBL-ZFBLNO,
       ZFHBLNO     LIKE  ZTBL-ZFHBLNO,
       ZFBLDT      LIKE  ZTBL-ZFBLDT,
       ZFETD       LIKE  ZTBL-ZFETD,
       ZFSPRTC     LIKE  ZTBL-ZFSPRTC,
       SPRTCNM(14) TYPE  C,
       ZFTOVL      LIKE  ZTBL-ZFTOVL,
       ZFTOVLM     LIKE  ZTBL-ZFTOVLM,
       BASIC       LIKE  ZTBL-ZFBLAMT,
       BASICC      LIKE  ZTBL-ZFBLAMC,
       ZFBLAMT     LIKE  ZTBL-ZFBLAMT,
       ZFBLAMC     LIKE  ZTBL-ZFBLAMC,
       ZFCARNM     LIKE  ZTBL-ZFCARNM,
       ZFETA       LIKE  ZTBL-ZFETA,
       ZFFORD      LIKE  ZTBL-ZFFORD,
       ZFAPRTC     LIKE  ZTBL-ZFAPRTC,
       KOSTL       LIKE  ZTBL-KOSTL,
       ZFUPT       LIKE  ZTBL-ZFUPT,
       APRTCNM(14) TYPE  C,
       ZFPKCN      LIKE  ZTBL-ZFPKCN,
       OTHER       LIKE  ZTBL-ZFBLAMT,
       OTHERC      LIKE  ZTBL-ZFBLAMC,
       ZFTRTE      LIKE  ZTBL-ZFTRTE,
       ZFTRTEC     LIKE  ZTBL-ZFTRTEC,
       ZFMBLNO     LIKE  ZTBL-ZFMBLNO,
       ZFBNDT      LIKE  ZTBL-ZFBNDT,
       ZFTRCK      LIKE  ZTBL-ZFTRCK,
       ZFVIA       LIKE  ZTBL-ZFVIA,
       ZFTRTPM     LIKE  ZTBL-ZFTRTPM,
       ZFOTHPM     LIKE  ZTBL-ZFOTHPM,
       ZFPOYN      LIKE  ZTBL-ZFPOYN,
       ZFTOWT      LIKE  ZTBL-ZFTOWT,
       ZFTOWTM     LIKE  ZTBL-ZFTOWTM,
       TOTAL       LIKE  ZTBL-ZFBLAMT,
       TOTALC      LIKE  ZTBL-ZFBLAMC,
       ZFSHTY      LIKE  ZTBL-ZFSHTY,
       SHTYNM(4)   TYPE  C,
       ZFRGDSR     LIKE  ZTBL-ZFRGDSR,
*>> 통관...
       ZFCLSEQ         LIKE   ZTCUCLIV-ZFCLSEQ,    " 통관순번...
       ZFIVNO          LIKE   ZTIV-ZFIVNO,         " 통관요청관리번호.
       ZFCCDT          LIKE   ZTIV-ZFCCDT,         " 통관요청일자.
       ZFPHVN          LIKE   ZTIV-ZFPHVN,         " 가벤처.
       PVNAME1         LIKE   LFA1-NAME1,          ">TEST
       ZFPOYN_NM(05),                              ">유환여부 TEXT.
       ZFCLCD          LIKE   ZTIV-ZFCLCD,         " 통관구분.
       ZFCLCD_NM(10),                              ">통관구분 TEXT.
       ZFCUST          LIKE   ZTIV-ZFCUST,         " 통관상태.
       ZFCUST_NM(12),                              ">통관상태 TEXT.
       ZFCDST          LIKE   ZTIV-ZFCDST,         " 부대비용 배부상태.
       ZFCDST_NM(11),                              ">배부상태 TEXT.
       ZFGRST          LIKE   ZTIV-ZFGRST,         " Good Receipt 상태.
       ZFGRST_NM(08),                              ">G/R STATUS TEXT.
       ZFCIVST         LIKE   ZTIV-ZFCIVST,        " 수입제비용 IV 상태.
       ZFCIVST_NM(10),                             ">CIV STATUS TEXT.
       ZFPONMA         LIKE   ZTIV-ZFPONMA,        " 무환수출입여부.
       CDAT            LIKE   ZTIV-CDAT,           " 통관 입력일.
       NAME11          LIKE   LFA1-NAME1,          " Vendor 명.
       WERKS           LIKE   ZTIVIT-WERKS,        " PLANT
       LGORT           LIKE   ZTIVIT-LGORT,        " 저장위치.
       LGOBE           LIKE   T001L-LGOBE,         " 저장위치내역.
       ZFIDRNO         LIKE   ZTIDS-ZFIDRNO,       " 수입면호번호.
       NAME12          LIKE   T001W-NAME1,         " PLANT 명.

       END   OF IT_TAB.

DATA : BEGIN OF IT_BL OCCURS 0.
              INCLUDE STRUCTURE  ZTBL.
DATA : END   OF IT_BL.

DATA : BEGIN OF IT_CD OCCURS 0,
               LAND1    LIKE ZTIEPORT-LAND1,
               PORT     LIKE ZTIEPORT-PORT,
               PORTT    LIKE ZTIEPORT-PORTT.
DATA : END   OF IT_CD.

DATA : BEGIN OF IT_CST OCCURS 0,
               ZFBLNO      LIKE  ZTBLCST-ZFBLNO,
               ZFCSCD      LIKE  ZTBLCST-ZFCSCD,
               ZFCAMT      LIKE  ZTBLCST-ZFCAMT,
               WAERS       LIKE  ZTBLCST-WAERS.
DATA : END   OF IT_CST.

*>> 수입관련문서 다중 선택시 MEMORY ID로 GET하는 INTERNAL TABLE.
DATA : BEGIN OF    IT_ZFIMDNO OCCURS 0,
       ZFCSTGRP    LIKE ZTBSEG-ZFCSTGRP,
       ZFIMDNO     LIKE ZTBSEG-ZFIMDNO,
       ZFDCNM      LIKE ZSBSEG-ZFDCNM,
       ZUONR       LIKE ZTBSEG-ZUONR,
       PS_POSID    LIKE ZSBSEG-PS_POSID,
       KOSTL       LIKE  ZTBL-KOSTL,
       ZFUPT       LIKE  ZTBL-ZFUPT,
       ZFPOYN    LIKE ZSBSEG-ZFPOYN,
       END   OF IT_ZFIMDNO.

DATA:  W_ERR_CHK,
       W_SUBRC           LIKE SY-SUBRC,
       W_COUNT           TYPE I,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_ZFCSTGRP        LIKE ZTBKPF-ZFCSTGRP,
       W_SELECTED_LINES  TYPE I,
       W_BUTTON_ANSWER   TYPE C,
       W_LINE            TYPE I,
       W_LFA1            LIKE LFA1,
       W_LFA12           LIKE LFA1.

DATA: INDXKEY1 LIKE INDX-SRTFD VALUE 'ZKEYVALUE1',
      INDXKEY  LIKE INDX-SRTFD VALUE 'ZKEYVALUE',
      WA_INDX  TYPE INDX.

*>> INCLUDE
INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

*-----------------------------------------------------------------------
* Selection Screen 절.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*> 수입의뢰.
   SELECT-OPTIONS: S_OPNNO   FOR ZTREQST-ZFOPNNO
                             MODIF ID REQ,
                   S_EBELN   FOR ZTREQHD-EBELN     " P/O Number
                             MODIF ID REQ,
                   S_REQNO   FOR ZTREQHD-ZFREQNO   " 수입의뢰 관리번?
                             MODIF ID REQ,
                   S_REQTY   FOR ZTREQHD-ZFREQTY   " 수입의뢰 Type
                             MODIF ID REQ,
                   S_REQDT   FOR ZTREQST-ZFREQDT   " 요개설일?
                             MODIF ID REQ,
                   S_MATGB   FOR ZTREQHD-ZFMATGB   " 자재구?
                             MODIF ID REQ,
                   S_EKORG   FOR ZTREQST-EKORG     " Purch. Org.
                             MODIF ID CM1,
                   S_EKGRP   FOR ZTREQST-EKGRP     " Purch. Grp.
                             MODIF ID CM1,
                   S_LIFNR   FOR ZTREQHD-LIFNR     " vendor
                             MODIF ID CM1,
                   S_ZFBENI  FOR ZTREQHD-ZFBENI    " Beneficiary
                             MODIF ID REQ,
                   S_NAME    FOR USR02-BNAME       " 담당?
                             MODIF ID REQ.
*> B/L
   SELECT-OPTIONS:
                   S_HBLNO  FOR ZTBL-ZFHBLNO       " House B/L No
                             MODIF ID CM2,
                   S_BLNO   FOR ZTIV-ZFBLNO        " B/L 관리번호.
                             MODIF ID CM2,
                   S_BLSDT   FOR ZTBL-ZFBLSDT      " B/L 송부?
                             MODIF ID BL,
                   S_BLADT   FOR ZTBL-ZFBLADT      " B/L 입수?
                             MODIF ID BL,
                   S_ZFTRCK  FOR ZTBL-ZFTRCK       " TRUCKER
                             MODIF ID BL,
                   S_ETA     FOR ZTBL-ZFETA        " ETA
                             MODIF ID CM2,
                   S_SPRTC   FOR ZTBL-ZFSPRTC      " 선적?
                             MODIF ID BL,
                   S_VIA     FOR  ZTBL-ZFVIA       " VIA
                             MODIF ID BL,
                   S_FORD    FOR ZTBL-ZFFORD       " Forwarder
                             MODIF ID BL,
                   S_CAGTY   FOR ZTBL-ZFCAGTY      " Cargo Type
                             MODIF ID BL,
                   S_POYN    FOR  ZTBL-ZFPOYN      " 유환여?
                             MODIF ID CM2.
   SELECT-OPTIONS: S_SHTY    FOR ZTBL-ZFSHTY       " 해상운송구분.
                             MODIF ID BL,
                   S_WERKS   FOR ZTBL-ZFWERKS     " 대표 PLANT
                             MODIF ID COM.
*> 통관.
   SELECT-OPTIONS: S_ZFCLCD FOR ZTIV-ZFCLCD        ">통관종류.
                             MODIF ID CC,
                   S_ZFCUST  FOR ZTIV-ZFCUST       ">통관상태.
                             MODIF ID CC,
                   S_ZFGRST  FOR ZTIV-ZFGRST       ">통관상태.
                             MODIF ID CC,
                   S_CDAT   FOR ZTIV-CDAT          " 통관요청 입력일자.
                             MODIF ID CC,
                   S_CCDT   FOR ZTIV-ZFCCDT        " 통관요청 입력일자.
                             MODIF ID CC,
                   S_IVNO   FOR ZTIV-ZFIVNO        " 통관요청  관리번호.
                             MODIF ID CC,
                   S_MBLNO  FOR ZTBL-ZFMBLNO      " Master B/L No
                             MODIF ID CC.
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN OUTPUT.
   PERFORM  P2000_SCREEN_FIELD_SET.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_INIT_VALUE.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION 건.
*-----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM   P1000_GET_READ_DATA    USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

  PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.

*-----------------------------------------------------------------------
* START OF SELECTION 건.
*-----------------------------------------------------------------------
AT USER-COMMAND.
   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택시.
         W_FIELD_NM = 'ZFREQDT'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해제.
         PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DISP'.
         PERFORM P2000_MULTI_SELECTION.
         CASE W_SELECTED_LINES.
            WHEN 0.           MESSAGE S962.
            WHEN 1.           PERFORM P2000_DISPLAY_DOCUMENT.
            WHEN OTHERS.      MESSAGE S965.
         ENDCASE.

      WHEN 'SELE'.
         PERFORM P2000_MULTI_SELECTION.
         CASE W_SELECTED_LINES.
            WHEN 0.
               MESSAGE S962.
            WHEN OTHERS.
               EXPORT IT_ZFIMDNO = IT_ZFIMDNO
                      TO DATABASE INDX(ST)  FROM WA_INDX ID INDXKEY1.
               LEAVE.
         ENDCASE.
   ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SCREEN_FIELD_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SCREEN_FIELD_SET.

*>> SCREEN MODIFY
   LOOP AT SCREEN.
      CASE W_ZFCSTGRP.
         WHEN '003'.
            CASE SCREEN-GROUP1.
               WHEN 'REQ' OR 'CM1' OR 'COM'.
               WHEN OTHERS.
                  SCREEN-INVISIBLE = '1'.
                  SCREEN-INPUT     = '0'.
                  MODIFY SCREEN.
            ENDCASE.
         WHEN '004' OR '005'.
            CASE SCREEN-GROUP1.
               WHEN 'BL' OR 'CM2' OR 'COM'.
               WHEN OTHERS.
                  SCREEN-INVISIBLE = '1'.
                  SCREEN-INPUT     = '0'.
                  MODIFY SCREEN.
            ENDCASE.
         WHEN '006'.
            CASE SCREEN-GROUP1.
               WHEN 'CC' OR 'CM2' OR 'COM'.
               WHEN OTHERS.
                  SCREEN-INVISIBLE = '1'.
                  SCREEN-INPUT     = '0'.
                  MODIFY SCREEN.
            ENDCASE.
         WHEN OTHERS.
      ENDCASE.

   ENDLOOP.

ENDFORM.                    " P2000_SCREEN_FIELD_SET

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INIT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_INIT_VALUE.

  IMPORT ZFCSTGRP = W_ZFCSTGRP ">>FROM MEMORY ID INDXKEY.
         FROM DATABASE INDX(ST)  ID INDXKEY TO WA_INDX.

  EXPORT ZFCSTGRP = SPACE
         TO DATABASE INDX(ST)  FROM WA_INDX ID INDXKEY.
  FREE MEMORY ID INDXKEY.

*>> MEMORY CLEAR
  REFRESH : IT_ZFIMDNO. CLEAR : IT_ZFIMDNO.
  EXPORT IT_ZFIMDNO = IT_ZFIMDNO
         TO DATABASE INDX(ST)  FROM WA_INDX ID INDXKEY1.
  FREE MEMORY ID INDXKEY1.

  CASE W_ZFCSTGRP.
     WHEN '003'.     ">수입의뢰관리.
        SET TITLEBAR 'SELDOC' WITH 'Import Request'.
     WHEN '004' OR '005' OR '007'.     ">B/L 관리.
        SET TITLEBAR 'SELDOC' WITH 'B/L'.
     WHEN '006'.     ">통관관리.
        SET TITLEBAR 'SELDOC' WITH 'Customs Clearance'.
     WHEN OTHERS.
        MESSAGE S621.
        LEAVE  PROGRAM.
  ENDCASE.

ENDFORM.                    " P2000_SET_INIT_VALUE
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  CASE W_ZFCSTGRP.
     WHEN '003'.
        SET TITLEBAR 'SELDOC' WITH 'Import Request'.
     WHEN '004' OR '005'.
        SET TITLEBAR 'SELDOC' WITH 'B/L'.
     WHEN '006'.
        SET TITLEBAR 'SELDOC' WITH 'Customs Clearance'.
     WHEN '007'.
        SET TITLEBAR 'SELDOC' WITH 'Cargo'.
     WHEN OTHERS.
        LEAVE.
  ENDCASE.

  SET PF-STATUS 'STATUS'.

  CASE W_ZFCSTGRP.
     WHEN '003'.
        SKIP 2.
        FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
        WRITE : / SY-ULINE(132).
        FORMAT COLOR COL_HEADING INTENSIFIED ON.
        WRITE : / SY-VLINE, ' ',           SY-VLINE,
                  'Req.open'    ,          SY-VLINE NO-GAP,
                  'P/O Number'    NO-GAP,  SY-VLINE NO-GAP,
                  'CUR. '         NO-GAP,  SY-VLINE NO-GAP,
       '    Open Amount   '       NO-GAP,  SY-VLINE NO-GAP,
                  'Ty'            NO-GAP,  SY-VLINE NO-GAP,
                  'Mat'           NO-GAP,  SY-VLINE NO-GAP,
                  'Pay.'          NO-GAP,  SY-VLINE NO-GAP,
          '    Loading Port    '  NO-GAP,  SY-VLINE NO-GAP,
                  'Inc'           NO-GAP,  SY-VLINE NO-GAP,
                  'Vendor    '    NO-GAP,  SY-VLINE NO-GAP,
                  'Name',              118 SY-VLINE NO-GAP,
                  'S'             NO-GAP,  SY-VLINE NO-GAP,
                  ' Loan Org. '   NO-GAP,  SY-VLINE NO-GAP.
        FORMAT COLOR COL_HEADING INTENSIFIED OFF.
        WRITE : / SY-VLINE, ' ',  SY-VLINE,
                  'Mat.Req '   ,  SY-VLINE NO-GAP,
                  'Imp.Req.No'    NO-GAP,  SY-VLINE NO-GAP,
                  '     '         NO-GAP,  SY-VLINE NO-GAP,
       'USD Convert Amount'       NO-GAP,  SY-VLINE NO-GAP,
                  'TT'            NO-GAP,  SY-VLINE NO-GAP,
                  'PGr'           NO-GAP,  SY-VLINE NO-GAP,
                  'Plnt'          NO-GAP,  SY-VLINE NO-GAP,
          '   Arriving Port    '  NO-GAP,  SY-VLINE NO-GAP,
                  'VIA'           NO-GAP,  SY-VLINE NO-GAP,
                  'Bene.     '    NO-GAP,  SY-VLINE NO-GAP,
                  ' Approve No      ', 132 SY-VLINE NO-GAP.
        WRITE : / SY-ULINE(132).
        FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

     WHEN '004' OR '005'.
        PERFORM  P3000_BL_TITLE_WRITE.
     WHEN '006'.
        PERFORM  P3000_CC_TITLE_WRITE.
     WHEN '007'.

     WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " P3000_TITLE_WRITE


*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.
  MOVE 0 TO SY-LSIND.

*  W_PAGE = 1.
*  W_LINE = 1.
*  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    P_ERR_CHK.

  CASE W_ZFCSTGRP.
     WHEN '003'.
        LOOP AT IT_TAB.
           PERFORM   P3000_IMPORT_REQ_WRITE.
           AT LAST.
              FORMAT RESET.
              WRITE : / 'Total', W_COUNT, 'Case'.
           ENDAT.
        ENDLOOP.

     WHEN '004' OR '005' .
        PERFORM   P3000_IMPORT_BL_WRITE.
     WHEN '006'.
        PERFORM   P3000_IMPORT_CC_WRITE.
     WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P1000_GET_READ_DATA USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting

  CASE W_ZFCSTGRP.
     WHEN '003'.
        PERFORM  P1000_GET_ZTREQHD.
     WHEN '004' OR '005' OR '007'.
        PERFORM  P1000_GET_ZTBL_DATA.
     WHEN '006'.
        PERFORM  P1000_GET_ZTIV_DATA.
     WHEN OTHERS.
        EXIT.
  ENDCASE.

  IF W_SUBRC NE 0.               " Not Found?
     W_ERR_CHK = 'Y'.  MESSAGE S966.    EXIT.
  ENDIF.

ENDFORM.                    " P1000_GET_READ_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_IMPORT_REQ_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_IMPORT_REQ_WRITE.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
       SY-VLINE NO-GAP,
       IT_TAB-ZFAPPDT NO-GAP,            " 개설신청?
       SY-VLINE NO-GAP,
       IT_TAB-EBELN   NO-GAP,            " 구매문?
       SY-VLINE NO-GAP,
       IT_TAB-WAERS NO-GAP,              " currency
       SY-VLINE NO-GAP,
       IT_TAB-ZFOPAMT CURRENCY IT_TAB-WAERS NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFREQTY NO-GAP,            " 결제 구?
       SY-VLINE,
       IT_TAB-ZFMATGB,                   " 자재 구?
       SY-VLINE NO-GAP,
       IT_TAB-ZTERM NO-GAP,              " Payment Terms
       SY-VLINE NO-GAP,
       IT_TAB-ZFSPRT  NO-GAP,               " 선적?
    85 SY-VLINE NO-GAP,
       IT_TAB-INCO1   NO-GAP,               " Incoterms
       SY-VLINE NO-GAP,
       IT_TAB-LIFNR   NO-GAP,
       SY-VLINE NO-GAP,
       (20)IT_TAB-NAME1   NO-GAP,
   118 SY-VLINE NO-GAP.

  CASE IT_TAB-ZFDOCST.
     WHEN 'C'.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
     WHEN 'O' OR 'A'.
        FORMAT COLOR COL_TOTAL    INTENSIFIED OFF.
     WHEN 'N'.
        FORMAT COLOR COL_GROUP    INTENSIFIED OFF.
     WHEN OTHERS.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  ENDCASE.
  WRITE : IT_TAB-ZFDOCST NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE : IT_TAB-NAME4 NO-GAP, SY-VLINE.         " 차입기관?

* hide
  HIDE: IT_TAB.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE, ' ',
       SY-VLINE NO-GAP,
*       IT_TAB-NAME3 ,                       " 개설 은?
       IT_TAB-ZFMAUD,                       " 자재납?
    16 SY-VLINE NO-GAP,
       IT_TAB-ZFREQNO NO-GAP,               " 수입의?
       SY-VLINE NO-GAP,
       IT_TAB-ZFUSD NO-GAP,                 " 통화 단?
       SY-VLINE NO-GAP,
       IT_TAB-ZFUSDAM  CURRENCY IT_TAB-ZFUSD NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFBACD,                       " 사전 / 사후 구?
       SY-VLINE NO-GAP,
       IT_TAB-EKGRP NO-GAP,                 " Purchasing Group
       SY-VLINE NO-GAP,
       IT_TAB-ZFWERKS NO-GAP,               " 대표 plant
       SY-VLINE NO-GAP,
       IT_TAB-ZFAPRT  NO-GAP,             " 도착?
    85 SY-VLINE,
       IT_TAB-ZFTRANS,                    " VIA
       SY-VLINE NO-GAP,
       IT_TAB-ZFBENI  NO-GAP,             " Beneficiary
       SY-VLINE NO-GAP,
       IT_TAB-ZFOPNNO(31) NO-GAP,
*   118 SY-VLINE NO-GAP,
*       ' ' NO-GAP,
*       SY-VLINE NO-GAP,
*       IT_TAB-ZFREF1 NO-GAP,               " 연락사?
       SY-VLINE NO-GAP.
* stored value...
  HIDE: IT_TAB.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE(132).

ENDFORM.                    " P3000_IMPORT_REQ_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_DISPLAY_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_DISPLAY_DOCUMENT.

  READ TABLE IT_ZFIMDNO INDEX 1.

  CASE W_ZFCSTGRP.
     WHEN '003'.
        SET PARAMETER ID 'BES'     FIELD ''.
        SET PARAMETER ID 'ZPREQNO' FIELD IT_ZFIMDNO-ZFIMDNO.
        SET PARAMETER ID 'ZPOPNNO' FIELD ''.
*        IF ZTBSEG-ZFCD EQ '1AB'.
*           CALL TRANSACTION 'ZIM43' AND SKIP  FIRST SCREEN.
*        ELSE.
           CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
*        ENDIF.

     WHEN '004' OR '005'.
        SET PARAMETER ID 'ZPBLNO'  FIELD IT_ZFIMDNO-ZFIMDNO.
        SET PARAMETER ID 'ZPHBLNO' FIELD ''.

        CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
     WHEN '006'.
        CLEAR : ZTIV.
        SELECT SINGLE * FROM  ZTIV
                        WHERE ZFIVNO  EQ  IT_ZFIMDNO-ZFIMDNO.
        IF SY-SUBRC EQ 0.
           CASE ZTIV-ZFCUST.
              WHEN '1' OR 'X'.
                 SET PARAMETER ID 'ZPIVNO'  FIELD IT_ZFIMDNO-ZFIMDNO.
                 SET PARAMETER ID 'ZPBLNO'  FIELD ''.
                 SET PARAMETER ID 'ZPHBLNO' FIELD ''.

                 CALL TRANSACTION 'ZIM33' AND SKIP  FIRST SCREEN.
              WHEN '2' OR '3' OR 'Y'.
                 SELECT SINGLE * FROM  ZTCUCLIV
                        WHERE ZFIVNO  EQ  IT_ZFIMDNO-ZFIMDNO.
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
        SET PARAMETER ID 'ZPCGNO'  FIELD IT_ZFIMDNO-ZFIMDNO.

        CALL TRANSACTION 'ZIM83' AND SKIP  FIRST SCREEN.
*     WHEN '003'.

*WHEN '004' OR '005'.
*WHEN '006'.
*     WHEN '007'.

     WHEN OTHERS.
        LEAVE.
  ENDCASE.

ENDFORM.                    " P2000_DISPLAY_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  REFRESH IT_ZFIMDNO.
  CLEAR W_SELECTED_LINES.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
       CASE W_ZFCSTGRP.
          WHEN '003'.
             MOVE : IT_TAB-ZFREQNO   TO IT_ZFIMDNO-ZFIMDNO,
                    W_ZFCSTGRP       TO IT_ZFIMDNO-ZFCSTGRP,
                    'Y'              TO IT_ZFIMDNO-ZFPOYN.
             IF IT_TAB-ZFOPNNO IS INITIAL.
                MOVE: IT_TAB-ZFREQNO TO IT_ZFIMDNO-ZFDCNM,
                      IT_TAB-ZFREQNO TO IT_ZFIMDNO-ZUONR.
             ELSE.
                MOVE: IT_TAB-ZFOPNNO TO IT_ZFIMDNO-ZFDCNM,
                      IT_TAB-ZFOPNNO TO IT_ZFIMDNO-ZUONR.
             ENDIF.

             APPEND IT_ZFIMDNO.
          WHEN '004' OR '005'.
             MOVE : IT_TAB-ZFBLNO    TO IT_ZFIMDNO-ZFIMDNO,
                    W_ZFCSTGRP       TO IT_ZFIMDNO-ZFCSTGRP,
                    IT_TAB-KOSTL     TO IT_ZFIMDNO-KOSTL,
                    IT_TAB-ZFUPT     TO IT_ZFIMDNO-ZFUPT,
                    IT_TAB-ZFPOYN    TO IT_ZFIMDNO-ZFPOYN.
             SELECT SINGLE * FROM  ZTBL
                             WHERE ZFBLNO  EQ  IT_TAB-ZFBLNO.
*> 1. 화물관리번호.
            IF NOT ZTBL-ZFGMNO IS INITIAL.
               MOVE: ZTBL-ZFGMNO TO IT_ZFIMDNO-ZFDCNM.
               IF NOT ZTBL-ZFMSN IS INITIAL.
                  CONCATENATE IT_ZFIMDNO-ZFDCNM '-' ZTBL-ZFMSN
                              INTO IT_ZFIMDNO-ZFDCNM.
               ENDIF.
               IF NOT ZTBL-ZFHSN IS INITIAL.
                  CONCATENATE IT_ZFIMDNO-ZFDCNM '-' ZTBL-ZFHSN
                              INTO IT_ZFIMDNO-ZFDCNM.
               ENDIF.
            ELSE.
*> 2. HOUSE B/L No.
               IF NOT ZTBL-ZFHBLNO IS INITIAL.
                  MOVE: ZTBL-ZFHBLNO TO IT_ZFIMDNO-ZFDCNM.
               ELSE.
*> 3. MASTER B/L No.
                  IF NOT ZTBL-ZFMBLNO IS INITIAL.
                     MOVE: ZTBL-ZFMBLNO TO IT_ZFIMDNO-ZFDCNM.
                  ELSE.
*> 4. 선사 B/L No.
                     IF NOT ZTBL-ZFCGHNO IS INITIAL.
                        MOVE: ZTBL-ZFCGHNO TO IT_ZFIMDNO-ZFDCNM.
                     ENDIF.
                  ENDIF.
               ENDIF.
            ENDIF.
            MOVE IT_ZFIMDNO-ZFDCNM   TO IT_ZFIMDNO-ZUONR.
*            MOVE IT_ZFIMDNO-PS_POSID TO IT_ZFIMDNO-PS_POSID.
            MOVE ZTBL-PS_POSID       TO IT_ZFIMDNO-PS_POSID.
            APPEND IT_ZFIMDNO.
*>> 통관.....
          WHEN '006'.
             MOVE : IT_TAB-ZFIVNO    TO IT_ZFIMDNO-ZFIMDNO,
                    W_ZFCSTGRP       TO IT_ZFIMDNO-ZFCSTGRP,
                    IT_TAB-ZFPOYN    TO IT_ZFIMDNO-ZFPOYN.
             SELECT SINGLE * FROM ZTIV
                             WHERE ZFIVNO  EQ IT_TAB-ZFIVNO.
             W_SUBRC = SY-SUBRC.

             IF W_SUBRC EQ 0.
                IT_TAB-ZFPOYN = ZTIV-ZFPOYN.
             ELSE.
                IT_TAB-ZFPOYN = 'Y'.
             ENDIF.

             SELECT SINGLE * FROM ZTBL
                    WHERE ZFBLNO  EQ ZTIV-ZFBLNO.

             MOVE : ZTBL-KOSTL     TO IT_ZFIMDNO-KOSTL,
                    ZTBL-ZFUPT     TO IT_ZFIMDNO-ZFUPT.

             IF W_SUBRC EQ 0.
                IF ZTIV-ZFCUST EQ '3' OR ZTIV-ZFCUST EQ 'Y'.
                   SELECT SINGLE * FROM ZTCUCLIV
                                   WHERE ZFIVNO EQ IT_TAB-ZFIVNO.
                   IF SY-SUBRC EQ 0.
                      SELECT SINGLE * FROM ZTIDR
                                      WHERE ZFBLNO  EQ ZTCUCLIV-ZFBLNO
                                      AND   ZFCLSEQ EQ ZTCUCLIV-ZFCLSEQ.
                      IF SY-SUBRC EQ 0.
                         IF ZTIDR-ZFIDRNO IS INITIAL.
                            MOVE: ZTIV-ZFIVNO TO IT_ZFIMDNO-ZFDCNM.
                         ELSE.
                            MOVE: ZTIDR-ZFIDRNO TO IT_ZFIMDNO-ZFDCNM.
                         ENDIF.
                      ELSE.
                         MOVE: ZTIV-ZFIVNO TO IT_ZFIMDNO-ZFDCNM.
                      ENDIF.
                   ELSE.
                      MOVE: ZTIV-ZFIVNO TO IT_ZFIMDNO-ZFDCNM.
                   ENDIF.
                ELSE.
                   MOVE: ZTIV-ZFIVNO TO IT_ZFIMDNO-ZFDCNM.
                ENDIF.
             ELSE.
                MOVE: ZTIV-ZFIVNO TO IT_ZFIMDNO-ZFDCNM.
             ENDIF.
             MOVE IT_ZFIMDNO-ZFDCNM   TO IT_ZFIMDNO-ZUONR.
*            MOVE IT_ZFIMDNO-PS_POSID TO IT_ZFIMDNO-PS_POSID.
             MOVE ZTBL-PS_POSID       TO IT_ZFIMDNO-PS_POSID.

             APPEND IT_ZFIMDNO.

          WHEN '007'.

          WHEN OTHERS.
             LEAVE.
        ENDCASE.
        ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTREQHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_GET_ZTREQHD.

     SELECT * INTO TABLE IT_ZVREQ FROM ZVREQHD_ST
                             WHERE ZFOPNNO    IN     S_OPNNO
                             AND   ZFREQDT    IN     S_REQDT
                             AND   ZFMATGB    IN     S_MATGB
                             AND   ZFREQTY    IN     S_REQTY
                             AND   ZFWERKS    IN     S_WERKS
                             AND   EKORG      IN     S_EKORG
                             AND   ERNAM      IN     S_NAME
                             AND   EBELN      IN     S_EBELN
                             AND   LIFNR      IN     S_LIFNR
                             AND   ZFBENI     IN     S_ZFBENI
                             AND   EKGRP      IN     S_EKGRP
                             AND   ZFREQNO    IN     S_REQNO
                             AND ( ZFDOCST    EQ     'O'
                             OR    ZFDOCST    EQ     'A' )
                             AND   ZFAMDNO    EQ     '00000'
                             AND   ZFCLOSE    EQ     SPACE.
        W_SUBRC = SY-SUBRC.

        REFRESH : IT_TAB.
        LOOP AT IT_ZVREQ.
           CLEAR : IT_TAB.

           W_TABIX = SY-TABIX.

           MOVE-CORRESPONDING IT_ZVREQ  TO  IT_TAB.
           MOVE : IT_ZVREQ-ZFLEVN       TO  IT_TAB-ZFLEVN,
                  IT_ZVREQ-ZFOPBN       TO  IT_TAB-ZFOPBN.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
           CLEAR : LFA1.
           CALL FUNCTION 'READ_LFA1'
                EXPORTING
                     XLIFNR          = IT_TAB-LIFNR
                IMPORTING
                     XLFA1           = LFA1
                EXCEPTIONS
                      KEY_INCOMPLETE  = 01
                      NOT_AUTHORIZED  = 02
                      NOT_FOUND       = 03.

           CASE SY-SUBRC.
              WHEN 01.     MESSAGE E022.
              WHEN 02.     MESSAGE E950.
              WHEN 03.     MESSAGE E020   WITH    IT_TAB-LIFNR.
           ENDCASE.
           MOVE: LFA1-NAME1   TO   IT_TAB-NAME1.
*-----------------------------------------------------------------------
* Bene. MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
           CLEAR : LFA1.
           CALL FUNCTION 'READ_LFA1'
                EXPORTING
                    XLIFNR          = IT_TAB-ZFBENI
                IMPORTING
                    XLFA1           = LFA1
                EXCEPTIONS
                    KEY_INCOMPLETE  = 01
                    NOT_AUTHORIZED  = 02
                    NOT_FOUND       = 03.

           CASE SY-SUBRC.
              WHEN 01.     MESSAGE E022.
              WHEN 02.     MESSAGE E950.
              WHEN 03.     MESSAGE E020   WITH    IT_TAB-LIFNR.
           ENDCASE.
           MOVE: LFA1-NAME1   TO   IT_TAB-NAME2.

*-----------------------------------------------------------------------
* Opeb Bank. MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
           CLEAR : LFA1.
           CALL FUNCTION 'READ_LFA1'
                EXPORTING
                      XLIFNR          = IT_TAB-ZFOPBN
                IMPORTING
                      XLFA1           = LFA1
                EXCEPTIONS
                      KEY_INCOMPLETE  = 01
                      NOT_AUTHORIZED  = 02
                      NOT_FOUND       = 03.

           CASE SY-SUBRC.
              WHEN 02.     MESSAGE E950.
              WHEN 03.     MESSAGE E020   WITH    IT_TAB-ZFOPBN.
           ENDCASE.
           MOVE: LFA1-NAME1   TO   IT_TAB-NAME3.
*-----------------------------------------------------------------------
* 차입기관   MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
           CLEAR : LFA1.
           CALL FUNCTION 'READ_LFA1'
                EXPORTING
                      XLIFNR          = IT_TAB-ZFLEVN
                IMPORTING
                      XLFA1           = LFA1
                EXCEPTIONS
                      KEY_INCOMPLETE  = 01
                      NOT_AUTHORIZED  = 02
                      NOT_FOUND       = 03.

           CASE SY-SUBRC.
              WHEN 02.     MESSAGE E950.
              WHEN 03.     MESSAGE E020   WITH    IT_TAB-ZFLEVN.
           ENDCASE.
           MOVE: LFA1-NAME1   TO   IT_TAB-NAME4.

           WRITE : IT_TAB-ZFOPAMT
                             CURRENCY IT_TAB-WAERS TO IT_TAB-ZFOPAMT1,
                   IT_TAB-ZFUSDAM
                             CURRENCY IT_TAB-ZFUSD TO IT_TAB-ZFUSDAM1.

           APPEND  IT_TAB.
        ENDLOOP.

ENDFORM.                    " P1000_GET_ZTREQHD
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTBL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_GET_ZTBL_DATA.
** B/L
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BL
         FROM  ZTBL
         WHERE ZFBLNO  IN  S_BLNO   "
         AND   ZFHBLNO IN  S_HBLNO  "
         AND   ZFBLSDT IN  S_BLSDT  "
         AND   ZFBLADT IN  S_BLADT  "
         AND   ZFTRCK  IN  S_ZFTRCK "
         AND   ZFETA   IN  S_ETA    "
         AND   ZFSPRTC IN  S_SPRTC  "
         AND   ZFVIA   IN  S_VIA    "
         AND   ZFFORD  IN  S_FORD   "
         AND   ZFCAGTY IN  S_CAGTY  "
         AND   ZFPOYN  IN  S_POYN   "
         AND   ZFSHTY  IN  S_SHTY   "
         AND   ZFWERKS IN  S_WERKS. "

  W_SUBRC = SY-SUBRC.

  IF W_SUBRC NE 0.
     EXIT.
  ENDIF.
   CLEAR : IT_CD.
   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_CD
            FROM ZTIEPORT
            FOR ALL ENTRIES IN IT_BL
            WHERE  ( PORT  EQ   IT_BL-ZFSPRTC
            AND      LAND1 EQ   IT_BL-ZFCARC )
            OR     ( PORT  EQ   IT_BL-ZFAPRTC
            AND      LAND1 EQ   IT_BL-ZFAPPC ).

** B/L 비?
   SELECT ZFBLNO ZFCSCD ZFCAMT WAERS
     INTO CORRESPONDING FIELDS OF TABLE IT_CST
     FROM ZTBLCST FOR ALL ENTRIES IN IT_BL
    WHERE ZFBLNO = IT_BL-ZFBLNO AND ZFCAMT <> 0.

  LOOP AT IT_BL.

    MOVE-CORRESPONDING  IT_BL  TO IT_TAB.

    CLEAR : IT_CD.
    READ TABLE IT_CD WITH KEY LAND1 = IT_BL-ZFCARC
                              PORT  = IT_BL-ZFSPRTC.
    MOVE IT_CD-PORTT   TO  IT_TAB-SPRTCNM.

    CLEAR : IT_CD.
    READ TABLE IT_CD WITH KEY LAND1 = IT_BL-ZFAPPC
                              PORT  = IT_BL-ZFAPRTC.
    MOVE IT_CD-PORTT  TO  IT_TAB-APRTCNM.

    CASE IT_BL-ZFSHTY.
      WHEN 'L'.     IT_TAB-SHTYNM = 'LCL'.
      WHEN 'F'.     IT_TAB-SHTYNM = 'FCL'.
      WHEN 'B'.     IT_TAB-SHTYNM = 'Bulk'.
      WHEN ' '.     IT_TAB-SHTYNM = ''.
      WHEN OTHERS.  IT_TAB-SHTYNM = '***'.
    ENDCASE.

    LOOP AT IT_CST WHERE ZFBLNO = IT_BL-ZFBLNO.
       IF IT_CST-ZFCSCD = 'ABC'.
          IT_TAB-BASIC = IT_TAB-BASIC + IT_CST-ZFCAMT.
          IT_TAB-TOTAL = IT_TAB-TOTAL + IT_CST-ZFCAMT.
          IT_TAB-BASICC = IT_CST-WAERS.
          IT_TAB-TOTALC = IT_CST-WAERS.
       ELSEIF IT_CST-ZFCSCD = 'AHC'.
       ELSE.
          IT_TAB-OTHER = IT_TAB-OTHER + IT_CST-ZFCAMT.
          IT_TAB-TOTAL = IT_TAB-TOTAL + IT_CST-ZFCAMT.
          IT_TAB-OTHERC = IT_CST-WAERS.
          IT_TAB-TOTALC = IT_CST-WAERS.
       ENDIF.
    ENDLOOP.

    APPEND IT_TAB.  CLEAR IT_TAB.

  ENDLOOP.
  SORT IT_TAB BY ZFWERKS ZFFORD ZFETA ZFBLNO.


ENDFORM.                    " P1000_GET_ZTBL_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_GET_ZTIV_DATA.
   REFRESH :IT_TAB, IT_BLIV.

   W_ERR_CHK = 'N'.
   SELECT H~ZFBLNO  I~ZFHBLNO  I~ZFBLDT  I~ZFETA  I~ZFMBLNO
          I~EKORG   I~EKGRP    I~ZFMSNO  H~ZFIVNO H~ZFCCDT
          H~ZFPHVN  H~ZFPOYN   H~ZFCLCD  H~ZFCUST H~ZFCDST
          H~ZFGRST  H~ZFCIVST  H~ZFPONMA H~CDAT   H~LIFNR
          I~PS_POSID
   INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
   FROM   ZTIV  AS  H  LEFT OUTER JOIN  ZTBL AS I
   ON     H~ZFBLNO     EQ   I~ZFBLNO
   WHERE  H~ZFIVNO     IN   S_IVNO     ">
   AND    H~ZFCCDT     IN   S_CCDT     ">
   AND    H~CDAT       IN   S_CDAT
   AND    H~ZFBLNO     IN   S_BLNO
   AND    H~LIFNR      IN   S_LIFNR
   AND    H~ZFCLCD     IN   S_ZFCLCD
   AND    H~ZFPOYN     IN   S_POYN
   AND    H~ZFCUST     IN   S_ZFCUST
   AND    H~ZFGRST     IN   S_ZFGRST.
   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.   MESSAGE S966.   EXIT.
   ENDIF.

   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
*>> 플랜트가 입력되었을 경우....
      IF NOT S_WERKS[] IS INITIAL.
         CLEAR: W_COUNT.
         SELECT COUNT( * ) INTO W_COUNT
                FROM   ZTIVIT
                WHERE  ZFIVNO   EQ    IT_TAB-ZFIVNO
                AND    WERKS    IN    S_WERKS.
         IF W_COUNT LE  0.
            DELETE  IT_TAB  INDEX  W_TABIX.
            CONTINUE.
         ENDIF.
      ENDIF.

      IF NOT ( S_HBLNO[] IS INITIAL AND S_MBLNO[] IS INITIAL AND
               S_ETA[]   IS INITIAL AND S_EKORG[] IS INITIAL AND
               S_EKGRP[] IS INITIAL ).
         IF IT_TAB-ZFBLNO IS INITIAL.
            DELETE  IT_TAB  INDEX  W_TABIX.
            CONTINUE.
         ELSE.
*>> 조건여부 CHECK!
            CLEAR  W_COUNT.
            SELECT COUNT( * )  INTO  W_COUNT
            FROM   ZTBL
            WHERE  ZFHBLNO     IN    S_HBLNO
            AND    ZFMBLNO     IN    S_MBLNO
            AND    ZFETA       IN    S_ETA
            AND    EKORG       IN    S_EKORG
            AND    EKGRP       IN    S_EKGRP
            AND    ZFBLNO      EQ    IT_TAB-ZFBLNO.

            IF W_COUNT LE  0.
               DELETE  IT_TAB  INDEX  W_TABIX.
               CONTINUE.
            ENDIF.
         ENDIF.
      ENDIF.

*>> 플랜트.
      SELECT SINGLE WERKS LGORT
             INTO (IT_TAB-WERKS, IT_TAB-LGORT)
             FROM  ZTIVIT
             WHERE ZFIVNO  EQ IT_TAB-ZFIVNO
             AND   ZFIVDNO EQ '00010'.
      IF NOT IT_TAB-WERKS IS INITIAL.
         SELECT SINGLE NAME1 INTO IT_TAB-NAME12
                FROM   T001W
                WHERE  WERKS EQ IT_TAB-WERKS.
         IF NOT IT_TAB-LGORT IS INITIAL.
            SELECT SINGLE LGOBE INTO IT_TAB-LGOBE
                   FROM T001L
                   WHERE WERKS  EQ IT_TAB-WERKS
                   AND   LGORT  EQ IT_TAB-LGORT.
         ENDIF.
      ENDIF.

*>> VENDOR가 바뀔 때 마다..
      IF IT_TAB-LIFNR IS INITIAL.
         CLEAR : W_LFA1.
      ENDIF.
      ON CHANGE OF IT_TAB-LIFNR.
         SELECT SINGLE * INTO W_LFA1 FROM LFA1
                WHERE LIFNR = IT_TAB-LIFNR.
      ENDON.
      MOVE W_LFA1-NAME1    TO    IT_TAB-NAME11.
*>> 가 VENDOR가 바뀔 때 마다..
      IF IT_TAB-ZFPHVN IS INITIAL.
         CLEAR : W_LFA12.
      ENDIF.
      ON CHANGE OF IT_TAB-ZFPHVN.
         SELECT SINGLE * INTO W_LFA12 FROM LFA1
                WHERE LIFNR = IT_TAB-ZFPHVN.
      ENDON.
      MOVE W_LFA12-NAME1    TO    IT_TAB-PVNAME1.

*>> 유환/무환 여부 TEXT.
      IF IT_TAB-ZFPOYN EQ 'Y'.
         IT_TAB-ZFPOYN_NM = 'Mone'.
      ELSEIF IT_TAB-ZFPOYN EQ 'Y'.
         IT_TAB-ZFPOYN_NM = 'Non'.
      ENDIF.
*>> 통관 여부 TEXT.
      CASE IT_TAB-ZFCLCD.
         WHEN 'A'.
            IT_TAB-ZFCLCD_NM = 'Customs'.
         WHEN 'B'.
            IT_TAB-ZFCLCD_NM = 'Duty C/C'.
         WHEN 'C'.
            IT_TAB-ZFCLCD_NM = 'In-Quay'.
         WHEN 'X'.
            IT_TAB-ZFCLCD_NM = 'No C/C'.
         WHEN OTHERS.
            IT_TAB-ZFCLCD_NM = 'None'.
      ENDCASE.
*>> 통관 상태 TEXT.
      CASE IT_TAB-ZFCUST.
         WHEN 'Y'.
            IT_TAB-ZFCUST_NM = 'Comp. C/C'.
         WHEN '1'.
            IT_TAB-ZFCUST_NM = 'Create'.
         WHEN '2'.
            IT_TAB-ZFCUST_NM = 'For Decl.'.
         WHEN '3'.
            IT_TAB-ZFCUST_NM = 'In Decl.'.
         WHEN 'N'.
            IT_TAB-ZFCUST_NM = 'No Object'.
      ENDCASE.

*>> 비용배부 상태 TEXT.
      CASE IT_TAB-ZFCDST.
         WHEN 'Y'.
            IT_TAB-ZFCDST_NM = 'Complete'.
         WHEN 'N'.
            IT_TAB-ZFCDST_NM = 'Distribute'.
         WHEN 'X'.
            IT_TAB-ZFCDST_NM = 'No Object'.
      ENDCASE.

*>> G/R 상태 TEXT.
      CASE IT_TAB-ZFGRST.
         WHEN 'Y'.
            IT_TAB-ZFGRST_NM = 'Complete'.
         WHEN 'N'.
            IT_TAB-ZFGRST_NM = 'G/R'.
         WHEN 'X'.
            IT_TAB-ZFGRST_NM = 'No G/R'.
      ENDCASE.

*>> CIV 상태 TEXT.
      CASE IT_TAB-ZFCIVST.
         WHEN 'Y'.
            IT_TAB-ZFCIVST_NM = 'Complete'.
         WHEN 'N'.
            IT_TAB-ZFCIVST_NM = 'I/V'.
         WHEN 'X'.
            IT_TAB-ZFCIVST_NM = 'No Object'.
      ENDCASE.

*>> 수입면허 번호 SEELCT.
      CLEAR : IT_TAB-ZFIDRNO.
      SELECT SINGLE ZFIDRNO  INTO  IT_TAB-ZFIDRNO
      FROM   ZTIDS
      WHERE  ZFIVNO          EQ    IT_TAB-ZFIVNO.

      MODIFY  IT_TAB INDEX W_TABIX.
   ENDLOOP.

   DESCRIBE TABLE IT_TAB LINES  W_LINE.
   IF W_LINE EQ 0.
      W_ERR_CHK = 'Y'.   MESSAGE S966.   EXIT.
   ENDIF.

ENDFORM.                    " P1000_GET_ZTIV_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTCGHD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_GET_ZTCGHD_DATA.

ENDFORM.                    " P1000_GET_ZTCGHD_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_IMPORT_BL_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_IMPORT_BL_WRITE.
  CLEAR : W_SUBRC, W_TABIX.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.

    IF W_SUBRC = 1.
       W_SUBRC = 2.    FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
       W_SUBRC = 1.    FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.
    WRITE:/'|',
           MARKFIELD  AS CHECKBOX,
          7(04) IT_TAB-ZFWERKS,
           (10) IT_TAB-ZFBLNO,
           (10) IT_TAB-ZFETD,
           (10) IT_TAB-LIFNR,
           (03) IT_TAB-ZFSPRTC,
           (13) IT_TAB-SPRTCNM,
           (15) IT_TAB-ZFTOVL UNIT IT_TAB-ZFTOVLM,
           (15) IT_TAB-BASIC  CURRENCY IT_TAB-BASICC NO-GAP,
           (04) IT_TAB-BASICC,
           (16) IT_TAB-ZFBLAMT CURRENCY IT_TAB-ZFBLAMC NO-GAP,
           (04) IT_TAB-ZFBLAMC,
           (20) IT_TAB-ZFCARNM NO-GAP, '|' NO-GAP.
    HIDE : IT_TAB.
    WRITE:/'|' NO-GAP,
         23(10) IT_TAB-ZFETA,
           (10) IT_TAB-ZFFORD,
           (03) IT_TAB-ZFAPRTC,
           (13) IT_TAB-APRTCNM,
           (15) IT_TAB-ZFPKCN,
           (15) IT_TAB-OTHER  CURRENCY IT_TAB-OTHERC NO-GAP,
           (04) IT_TAB-OTHERC,
           (16) IT_TAB-ZFTRTE CURRENCY IT_TAB-ZFTRTEC NO-GAP,
           (04) IT_TAB-ZFTRTEC,
           (20) IT_TAB-ZFMBLNO NO-GAP, '|' NO-GAP.

    WRITE:/'|' NO-GAP,
         23(10) IT_TAB-ZFBNDT,
           (10) IT_TAB-ZFTRCK,
           (01) IT_TAB-ZFMATGB,
           (03) IT_TAB-ZFVIA,
           (03) IT_TAB-INCO1,
           (02) IT_TAB-ZFTRTPM,
           (02) IT_TAB-ZFOTHPM,
           (01) IT_TAB-ZFPOYN,
           (15) IT_TAB-ZFTOWT UNIT  IT_TAB-ZFTOWTM,
           (15) IT_TAB-TOTAL  CURRENCY IT_TAB-TOTALC NO-GAP,
           (04) IT_TAB-TOTALC,
           (01) IT_TAB-ZFSHTY,
           (05) IT_TAB-SHTYNM,
           (33) IT_TAB-ZFRGDSR NO-GAP, '|' NO-GAP.
    HIDE : IT_TAB.

    WRITE : / SY-ULINE(140).

    AT LAST.
       SUM.
       FORMAT COLOR 3 INTENSIFIED OFF.
       WRITE:/'|' NO-GAP,
              4(20) 'Total',
             59     'Weight ', (15) IT_TAB-ZFTOVL UNIT IT_TAB-ZFTOVLM
                                                LEFT-JUSTIFIED,
             85     'Carry-in Qty ', (15) IT_TAB-ZFPKCN LEFT-JUSTIFIED,
            140 '|' NO-GAP.
    WRITE : / SY-ULINE(140).
    ENDAT.
  ENDLOOP.
  CLEAR : IT_TAB.

ENDFORM.                    " P3000_IMPORT_BL_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_BL_TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_BL_TITLE_WRITE.
  SKIP 1.
  WRITE:/70 '  Shipping Document List   ' COLOR 1.
  WRITE:/120 'DATE :', SY-DATUM.
  WRITE:/5 'M:Material Type Inc:Incoterms  B:Basic Charge Payment Mth',
           'O:Other Charge Payment method  Y:Monetary',
        120 'PGAE :', SY-PAGNO.

  FORMAT COLOR 1 INTENSIFIED OFF.
  WRITE:/ SY-ULINE(140).
  WRITE:/'|' NO-GAP,
         (03) 'Sel',
         (06) 'Plant',
         (09) 'B/L Doc.'.
  SET LEFT SCROLL-BOUNDARY.
  WRITE: (10) 'ETD',
         (10) 'Vendor',
         (17) 'Loading Port',
         (15) 'Total Weight' RIGHT-JUSTIFIED,
         (14) 'Freight Charge' RIGHT-JUSTIFIED,
         (04) 'CURR',
         (15) 'B/L Amount' RIGHT-JUSTIFIED,
         (04) 'CURR',
         (20) 'Vessel/Fight Name' NO-GAP, '|' NO-GAP.
  WRITE:/'|' NO-GAP,
       23(10) 'ETA',
         (10) 'Forwarder',
         (17) 'Arriving Port',
         (15) 'Carry-in Qty' RIGHT-JUSTIFIED,
         (14) 'Other Charge' RIGHT-JUSTIFIED,
         (04) 'CURR',
         (15) 'Freight Rate' RIGHT-JUSTIFIED,
         (04) 'CURR',
         (20) 'Master B/L No.' NO-GAP, '|' NO-GAP.

  WRITE:/'|' NO-GAP,
       23(10) 'Bonded DT',
         (10) 'Trucker',
         (01) 'M',                        " 자재구?
         (03) 'Via',                      " Via
         (03) 'Inc',                      " Incoterms
         (02) 'B',                         " Basic Charge 지불방?
         (02) 'O',                         " Other Charge 지불방?
         (01) 'Y',                         " 유한구?
         (15) 'Total Volume' RIGHT-JUSTIFIED,
         (14) 'Total Charge' RIGHT-JUSTIFIED,
         (04) 'CURR',
         (08) 'Shipment Type',
         (32) 'Represent Goods Description' NO-GAP, '|' NO-GAP.
  WRITE:/ SY-ULINE(140).

ENDFORM.                    " P3000_BL_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_CC_TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_CC_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /60  ' [ Customs Clearance List ] '
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE(140).
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',                    SY-VLINE NO-GAP,
            'House B/L No '       NO-GAP,  30 SY-VLINE NO-GAP,
            'B/L Doc.No'          NO-GAP,     SY-VLINE NO-GAP,
            ' Seq.'               NO-GAP,     SY-VLINE NO-GAP,
            'B/L Iss.DT'          NO-GAP,     SY-VLINE NO-GAP,
            'Rep. Plant '         NO-GAP,  83 SY-VLINE NO-GAP,
            'POrg'                NO-GAP,     SY-VLINE NO-GAP,
            'Pgp'                 NO-GAP,     SY-VLINE NO-GAP,
            'Vendor '             NO-GAP, 140 SY-VLINE NO-GAP.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
            ' Req. C/C No.'       NO-GAP, SY-VLINE NO-GAP,
            'Req. C/C'            NO-GAP, SY-VLINE NO-GAP,
            'Created on'          NO-GAP, SY-VLINE NO-GAP,
            'Mo/NM'               NO-GAP, SY-VLINE NO-GAP,
            ' C/C Type '          NO-GAP, SY-VLINE NO-GAP,
            ' C/C Status '        NO-GAP, SY-VLINE NO-GAP,
            'Cost Dist. '         NO-GAP, SY-VLINE NO-GAP,
            'G/R Sta.'            NO-GAP, SY-VLINE NO-GAP,
            ' Imp. Cost'          NO-GAP, SY-VLINE NO-GAP,
            '   Entry No   '      NO-GAP, 140 SY-VLINE NO-GAP.
  WRITE : / SY-ULINE(140).

ENDFORM.                    " P3000_CC_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_IMPORT_CC_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_IMPORT_CC_WRITE.
   LOOP AT IT_TAB.
      FORMAT RESET.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.

      WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
           SY-VLINE NO-GAP,
           IT_TAB-ZFHBLNO  NO-GAP,
           SY-VLINE NO-GAP,               " House B/L No
           IT_TAB-ZFBLNO   NO-GAP,
           SY-VLINE NO-GAP,               " B/L 관리번호.
*           IT_TAB-ZFETA    NO-GAP,
           IT_TAB-ZFCLSEQ    NO-GAP,
           SY-VLINE NO-GAP,               " 순번( 도착일 )
           IT_TAB-ZFBLDT   NO-GAP,
           SY-VLINE NO-GAP,               " B/L 발행일자.
           IT_TAB-WERKS        NO-GAP,
           '-'                 NO-GAP,
           IT_TAB-NAME12(19)    NO-GAP,
           SY-VLINE NO-GAP,               " 플랜트
           IT_TAB-EKORG    NO-GAP,
           SY-VLINE NO-GAP,               " 구매조직.
           IT_TAB-EKGRP    NO-GAP,
           SY-VLINE NO-GAP,               " 구매그룹.
           IT_TAB-LIFNR    NO-GAP,
           SY-VLINE NO-GAP,               " VENDOR.
           (20)IT_TAB-NAME11   NO-GAP,
       140 SY-VLINE NO-GAP.               " VENDOR NAME.
* Hide
       HIDE: IT_TAB.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE : / SY-VLINE, ' ', SY-VLINE,
       8 IT_TAB-ZFIVNO,
       SY-VLINE NO-GAP,               " 통관요청 관리번호.
       IT_TAB-ZFCCDT    NO-GAP,
       SY-VLINE NO-GAP,               " 통관요청 일자.
       IT_TAB-CDAT      NO-GAP,
       SY-VLINE NO-GAP,               " 통관요청 생성일자.
       IT_TAB-ZFPOYN_NM NO-GAP,
       SY-VLINE NO-GAP,               " 유무환 구분.
       IT_TAB-ZFCLCD_NM NO-GAP,
       SY-VLINE NO-GAP.               " 통관구분.

*>> 통관 상태.
       IF IT_TAB-ZFCUST EQ 'Y' OR IT_TAB-ZFCUST EQ 'N'.
          FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
       ELSE.
          FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
       ENDIF.
       WRITE : IT_TAB-ZFCUST_NM NO-GAP,
           SY-VLINE NO-GAP.               " 통관상태.

*>> 비용배부 상태.
       IF IT_TAB-ZFCDST EQ 'Y' OR IT_TAB-ZFCDST EQ 'X'.
          FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
       ELSE.
          FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
       ENDIF.
       WRITE : IT_TAB-ZFCDST_NM NO-GAP,
           SY-VLINE NO-GAP.               " 배용배부상태.

*>> 비용배부 상태.
       IF IT_TAB-ZFGRST EQ 'Y'.
          FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
       ELSE.
          FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
       ENDIF.
       WRITE : IT_TAB-ZFGRST_NM NO-GAP,
           SY-VLINE NO-GAP.               " Good Receipt 상태.

*>> 제비용 상태.
       IF IT_TAB-ZFCIVST EQ 'Y' OR IT_TAB-ZFCIVST EQ 'X'.
          FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
       ELSE.
          FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
       ENDIF.
       WRITE : IT_TAB-ZFCIVST_NM NO-GAP,
           SY-VLINE NO-GAP.               " 제비용 상태.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE : IT_TAB-ZFIDRNO NO-GAP COLOR COL_NORMAL INTENSIFIED OFF,
           140 SY-VLINE NO-GAP.           " 수입면허번호.

       FORMAT RESET.

* Stored value...
       HIDE: IT_TAB.
       W_COUNT = W_COUNT + 1.

       WRITE : / SY-ULINE(140).

   ENDLOOP.
ENDFORM.                    " P3000_IMPORT_CC_WRITE
