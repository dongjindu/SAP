FUNCTION ZIM_CUDATA_CREATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(W_ZFIVNO) LIKE  ZTIV-ZFIVNO
*"  EXPORTING
*"     REFERENCE(W_ZFBLNO) LIKE  ZTBL-ZFBLNO
*"     REFERENCE(W_ZFCLSEQ) LIKE  ZTIDR-ZFCLSEQ
*"     REFERENCE(W_EDI_CHK) LIKE  ZTIDR-ZFEDICK
*"  EXCEPTIONS
*"      ERROR_INSERT
*"----------------------------------------------------------------------

  DATA : L_RATE TYPE F.
  DATA : W_PONO LIKE ZTIVIT-EBELN,
         W_SIBG LIKE ZTBLINR_TMP-ZFSIBG.
*-----------------------------------------------------------------------
* INTERNAL TABLE DEFINE
*-----------------------------------------------------------------------
  DATA: BEGIN OF IT_IVIT OCCURS 0.
  INCLUDE  STRUCTURE ZTIVIT.
  DATA: END OF IT_IVIT.

  DATA: BEGIN OF IT_IVIT_TMP OCCURS 0.
  INCLUDE  STRUCTURE ZTIVIT.
  DATA: END OF IT_IVIT_TMP.

  DATA: BEGIN OF IT_REQIL OCCURS 0.
  INCLUDE  STRUCTURE ZTREQIL.
  DATA: END OF IT_REQIL.

  DATA: BEGIN OF IT_ZTIDRHS1 OCCURS 0.
  INCLUDE  STRUCTURE ZTIDRHS.
  DATA: END OF IT_ZTIDRHS1.

  DATA: BEGIN OF IT_ZTIDRHSD1 OCCURS 0.
  INCLUDE  STRUCTURE ZTIDRHSD.
  DATA: END OF IT_ZTIDRHSD1.

  DATA: BEGIN OF IT_ZTIDRHSL1 OCCURS 0.
  INCLUDE  STRUCTURE ZTIDRHSL.
  DATA: END OF IT_ZTIDRHSL1.

*-----------------------------------------------------------------------
* 변수 DEFINE
*-----------------------------------------------------------------------
  DATA : W_ZFINAMT    LIKE  ZTIDR-ZFINAMT ,
         W_ZFLASTAM   LIKE  ZTREQHD-ZFLASTAM,
         W_MENGE      LIKE  ZTIVIT-CCMENGE,
         W_ZFQNT      LIKE  ZTIDRHS-ZFQNT,
         W_ZFDUAM     LIKE  ZTIDR-ZFDUAM,
         W_ZFREQNO    LIKE  ZTREQHD-ZFREQNO,
         W_ZFCONO     LIKE  ZTIDRHSD-ZFCONO,
         W_ZFCKAMT    LIKE  ZTRECST-ZFCKAMT,
         W_ZFWERKS    LIKE  ZTIVIT-WERKS,
         W_ZFRONO     LIKE  ZTIDRHSD-ZFRONO,
*       W_ZFCLSEQ    LIKE  ZTIDR-ZFCLSEQ,
         W_STATUS     TYPE  C,
         W_LOOP_CNT   TYPE  I.

  DATA : W_ZFIDRNO         LIKE   ZTIDR-ZFIDRNO,
         W_COUNT           TYPE   I,
         W_SEQ(6)          TYPE   N,
         W_TMP(6)          TYPE   N,
         W_TMP_1(1)        TYPE   N,
         W_CHK(1)          TYPE   N,
         W_YEAR(2)         TYPE   C,
         W_YYYY(4)         TYPE   C,
         W_YYYYMMDD_FROM   LIKE   ZTIDR-ZFIDWDT,
         W_YYYYMMDD_TO     LIKE   ZTIDR-ZFIDWDT,
         L_ZFIDRNO         LIKE   ZTIDR-ZFIDRNO.

  TYPES: IMIS_TYPE_C12(18) TYPE   C,
         IMIS_TYPE_C24(18) TYPE   C.

  DATA:  W_TEXT12          TYPE   IMIS_TYPE_C12,
         W_TEXT24          TYPE   IMIS_TYPE_C24.

*  CLEAR : ZTCUCL,    ZTCUCLIV,
  CLEAR : ZTIDR, ZTIV.
  CLEAR : W_ZFINAMT, W_ZFLASTAM, W_LOOP_CNT, W_MENGE.
  CLEAR : W_ZFREQNO, W_ZFWERKS.

  REFRESH : IT_ZTIDRHS, IT_ZTIDRHSD, IT_ZTIDRHSL.

*>> 수입 IMG 사항 CHECK.
  SELECT  SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFIMPATH  EQ  '3'.
    W_STATUS  =  '3'.
  ELSE.
    W_STATUS  =  '2'.
  ENDIF.

*>> BL NO GET
  SELECT  SINGLE * FROM ZTIV  WHERE  ZFIVNO = W_ZFIVNO.

*>> BL의 총 수량 구하기.
  SELECT  SUM( BLMENGE )
  INTO    W_MENGE
  FROM    ZTBLIT
  WHERE   ZFBLNO    EQ    ZTIV-ZFBLNO .

* 통관요청 TABLE 의 통관상태 UPDATE.
  SELECT  SINGLE *  FROM ZTIV
  WHERE   ZFIVNO = W_ZFIVNO.

*  SELECT MAX( ZFCLSEQ ) INTO ZTCUCL-ZFCLSEQ
*  FROM   ZTCUCLIV
*  WHERE  ZFBLNO = ZTIV-ZFBLNO.

  SELECT MAX( ZFCLSEQ ) INTO W_ZFCLSEQ
  FROM   ZTIDR
  WHERE  ZFBLNO = ZTIV-ZFBLNO.

  IF W_ZFCLSEQ IS INITIAL.
    W_ZFCLSEQ = 1.
  ELSE.
    W_ZFCLSEQ = W_ZFCLSEQ + 1.
  ENDIF.

  SET PARAMETER ID 'ZPBLNO'   FIELD ZTIV-ZFBLNO.
  SET PARAMETER ID 'ZPCLSEQ'  FIELD W_ZFCLSEQ.

*  MOVE-CORRESPONDING ZTIV      TO  ZTCUCLIV.
*  MOVE   ZTCUCL-ZFCLSEQ        TO  ZTCUCLIV-ZFCLSEQ.
*  MOVE   'A'                   TO  ZTCUCLIV-ZFCLCD.
*  MOVE   W_STATUS              TO  ZTCUCLIV-ZFCUST.
*  MOVE   SY-UNAME              TO  ZTCUCLIV-UNAM.
*  MOVE   SY-DATUM              TO  ZTCUCLIV-UDAT.
*  MOVE   SY-UNAME              TO  ZTCUCLIV-ERNAM.
*  MOVE   SY-DATUM              TO  ZTCUCLIV-CDAT.
*  INSERT ZTCUCLIV.

**>> ERROR 발생시 IV 상태 변경.
*  IF SY-SUBRC NE 0.
**     UPDATE  ZTIV
**     SET     ZFCUST    =   '1'
**             UNAM      =   SY-UNAME
**             UDAT      =   SY-DATUM
**     WHERE   ZFIVNO    =   W_ZFIVNO.
*     ROLLBACK WORK.
*     RAISE ERROR_INSERT.
*  ENDIF.

* 통관용 INVOICE 자재 TABLE INSERT!
  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_IVIT
  FROM   ZTIVIT
  WHERE  ZFIVNO  =  W_ZFIVNO.

*  LOOP AT IT_IVIT.
*    CLEAR  ZTCUCLIVIT.
*    IF SY-TABIX = 1.
*       MOVE  IT_IVIT-WERKS TO W_ZFWERKS.
*    ENDIF.
*    MOVE  SY-MANDT         TO  ZTCUCLIVIT-MANDT.
*    MOVE  IT_IVIT-ZFIVNO   TO  ZTCUCLIVIT-ZFIVNO.
*    MOVE  IT_IVIT-ZFIVDNO  TO  ZTCUCLIVIT-ZFIVDNO.
*    MOVE  IT_IVIT-MATNR    TO  ZTCUCLIVIT-MATNR.
*    MOVE  IT_IVIT-STAWN    TO  ZTCUCLIVIT-STAWN.
*    MOVE  IT_IVIT-CCMENGE  TO  ZTCUCLIVIT-MENGE.
*    MOVE  IT_IVIT-MEINS    TO  ZTCUCLIVIT-MEINS.
*    MOVE  IT_IVIT-NETPR    TO  ZTCUCLIVIT-NETPR.
*    MOVE  IT_IVIT-PEINH    TO  ZTCUCLIVIT-PEINH.
*    MOVE  IT_IVIT-BPRME    TO  ZTCUCLIVIT-BPRME.
*    MOVE  IT_IVIT-TXZ01    TO  ZTCUCLIVIT-TXZ01.
*    MOVE  IT_IVIT-ZFIVAMT  TO  ZTCUCLIVIT-ZFIVAMT.
*    MOVE  IT_IVIT-ZFIVAMC  TO  ZTCUCLIVIT-ZFIVAMC.
*    INSERT ZTCUCLIVIT.
**>> ERROR 발생시 DATA ROLLBACK
*    IF SY-SUBRC NE 0.
**       UPDATE  ZTIV
**       SET     ZFCUST    =   '1'
**               UNAM      =   SY-UNAME
**               UDAT      =   SY-DATUM
**       WHERE   ZFIVNO    =   W_ZFIVNO.
**       DELETE  FROM   ZTCUCLIV
**       WHERE   ZFIVNO EQ  W_ZFIVNO.
**
**       DELETE  FROM   ZTCUCLIV
**       WHERE   ZFIVNO EQ  W_ZFIVNO.
*       ROLLBACK WORK.
*       RAISE ERROR_INSERT.
*    ENDIF.
*
*  ENDLOOP.
*
** 통관 TABLE INSERT!
*  SELECT SINGLE *
*  FROM   ZTCUCL
*  WHERE  ZFBLNO  =  ZTIV-ZFBLNO
*  AND    ZFCLSEQ =  ZTCUCLIV-ZFCLSEQ .
*
*  IF SY-SUBRC NE 0.
*     MOVE  SY-MANDT       TO  ZTCUCL-MANDT.
*     MOVE  ZTIV-ZFBLNO    TO  ZTCUCL-ZFBLNO.
*     MOVE  ZTCUCL-ZFCLSEQ TO  ZTCUCL-ZFCLSEQ.
*     MOVE  'USD'          TO  ZTCUCL-ZFUSD.
*     MOVE  'KRW'          TO  ZTCUCL-ZFKRW.
*     MOVE  ZTIV-ZFCLCD    TO  ZTCUCL-ZFCLCD.
*     MOVE  W_STATUS       TO  ZTCUCL-ZFCUST.
*     MOVE  SY-UNAME       TO  ZTCUCL-ERNAM.
*     MOVE  SY-DATUM       TO  ZTCUCL-CDAT.
*     MOVE  SY-UNAME       TO  ZTCUCL-UNAM.
*     MOVE  SY-DATUM       TO  ZTCUCL-UDAT.
*     INSERT ZTCUCL.
**>> ERROR 발생시 DATA ROLLBACK
*    IF SY-SUBRC NE 0.
**       UPDATE  ZTIV
**       SET     ZFCUST    =   '1'
**               UNAM      =   SY-UNAME
**               UDAT      =   SY-DATUM
**       WHERE   ZFIVNO    =   W_ZFIVNO.
**
**       DELETE  FROM   ZTCUCLIV
**       WHERE   ZFIVNO EQ  W_ZFIVNO.
**
**       DELETE  FROM   ZTCUCLIVIT
**       WHERE   ZFIVNO EQ  W_ZFIVNO.
*       ROLLBACK WORK.
*       RAISE ERROR_INSERT.
*    ENDIF.
*  ENDIF.

* 수입신고 관련 TABLE INSERT!
  SELECT SINGLE *  FROM ZTBL
  WHERE  ZFBLNO    EQ   ZTIV-ZFBLNO.

  SELECT SINGLE * FROM ZTIMIMGTX
         WHERE    BUKRS  EQ  ZTBL-BUKRS.

* 납세자 관련 자료 TABLE INSERT!
*  SELECT SINGLE * FROM ZTIMIMG02
  SELECT * FROM ZTIMIMG02 UP TO 1 ROWS
           WHERE  ZFWERKS  =  W_ZFWERKS.
  ENDSELECT.

* 수입신고 번호 생성.
  IF W_STATUS = '3'.

    MOVE ZTIV-ZFCCDT      TO  ZTIDR-ZFIDWDT.
*     MOVE ZTIV-ZFCCDT+0(4) TO W_YYYY.
*     CONCATENATE W_YYYY '0101' INTO W_YYYYMMDD_FROM.
*     CONCATENATE W_YYYY '1231' INTO W_YYYYMMDD_TO.
*     CLEAR W_ZFIDRNO.
*
*     CONCATENATE '_____' SY-DATUM+2(2) '8______' INTO L_ZFIDRNO.
*--------------> INFOLINKE DREAMKSB <-----------------------------------
*---> 연도별?
*                          AND ZFIDRNO LIKE '_______8_____'
*     SELECT MAX( ZFIDRNO ) INTO W_ZFIDRNO FROM ZTIDR
*                           WHERE ZFCUT = ZTIV-ZFCUT
*                           AND ZFIDRNO LIKE L_ZFIDRNO
*                           AND ZFIDWDT >= W_YYYYMMDD_FROM
*                           AND ZFIDWDT <= W_YYYYMMDD_TO
*                           AND ( ZFBLNO NE ZTIDR-ZFBLNO
*                           OR    ZFCLSEQ NE ZTIDR-ZFCLSEQ ).
*     IF W_ZFIDRNO IS INITIAL.
*        MOVE ZTIDR-ZFIDWDT+2(2) TO W_YEAR.
*        MOVE '800000'           TO W_SEQ.
*     ELSE.
*        MOVE W_ZFIDRNO+5(2)     TO W_YEAR.
*        MOVE W_ZFIDRNO+7(6)     TO W_SEQ.
*     ENDIF.
*     ADD  1                     TO W_SEQ.  " 수입신고번?
*     W_TMP = ZTIV-ZFCUT+0(1) * 7 + ZTIV-ZFCUT+1(1) * 3
*           + ZTIV-ZFCUT+2(1) * 1 + ZTIV-ZFCUT+3(1) * 7
*           + ZTIV-ZFCUT+4(1) * 3
*           + W_YEAR+0(1) * 1 + W_YEAR+1(1) * 7
*           + W_SEQ+0(1) * 3 + W_SEQ+1(1) * 1
*           + W_SEQ+2(1) * 7 + W_SEQ+3(1) * 3
*           + W_SEQ+4(1) * 1 + W_SEQ+5(1) * 7.
*     W_TMP_1 = W_TMP MOD 10.
*     W_CHK   = 10 - W_TMP_1.
*     CONCATENATE ZTIV-ZFCUT W_YEAR W_SEQ W_CHK INTO ZTIDR-ZFIDRNO.
*>> 수입신고 번호 중복 CHECK!
*     SELECT  COUNT( DISTINCT ZFIDRNO )  INTO  W_COUNT
*     FROM    ZTIDR
*     WHERE   ZFIDRNO  =  ZTIDR-ZFIDRNO.
*     IF W_COUNT >  0.
*        W_SEQ  =  W_SEQ  +  1.
*        CONCATENATE ZTIV-ZFCUT W_YEAR W_SEQ W_CHK INTO ZTIDR-ZFIDRNO.
*     ENDIF.
*
*     CONCATENATE ZTIDR-ZFREBELN
*                 W_YEAR W_SEQ INTO ZTIDR-ZFIMCR. " 무역업체참조번?
*     SET PARAMETER ID 'ZPIDRNO' FIELD ZTIDR-ZFIDRNO.
  ENDIF.

  MOVE  SY-MANDT           TO  ZTIDR-MANDT.
  MOVE  ZTBL-BUKRS         TO  ZTIDR-BUKRS.          " 회사코드.
*  MOVE  SY-DATUM           TO  ZTIDR-ZFIDWDT.        " 신고일.
  MOVE  ZTIV-ZFBLNO        TO  ZTIDR-ZFBLNO.         " B/L 관리번호.
  MOVE  ZTIV-ZFIVNO        TO  ZTIDR-ZFIVNO.         " 통관입고요청번호.
  MOVE  ZTIV-ZFCUT         TO  ZTIDR-ZFCUT.          " 관세사
  MOVE  W_ZFCLSEQ          TO  ZTIDR-ZFCLSEQ.        " 통관순번.
  MOVE  'KRW'              TO  ZTIDR-ZFINAMTC.       " 보험료통화.
*  MOVE  'K'                TO  ZTIDR-ZFITKD.         " 수입신고종류.
*  MOVE  'ETC'              TO  ZTIDR-ZFTRCN.         " 운송용기.
*  MOVE  ZTIMIMG02-ZFCOTM   TO  ZTIDR-ZFINRC.         " 신고지세관.
*  MOVE  ZTIMIMG02-ZFTDNO   TO  ZTIDR-ZFTDNO.         " 납세자 통관번호.
  MOVE  ZTIMIMGTX-ZFTDNM1  TO  ZTIDR-ZFTDNM1.        " 납세자 상호.
  MOVE  ZTIMIMGTX-ZFTDNM2  TO  ZTIDR-ZFTDNM2.        " 납세자 성명.
*  MOVE  ZTIMIMG02-ZFTDAD1  TO  ZTIDR-ZFTDAD1.        " 납세자 주소1
*  MOVE  ZTIMIMG02-ZFTDAD2  TO  ZTIDR-ZFTDAD2.        " 납세자 주소2
*  MOVE  ZTIMIMG02-ZFTDTC   TO  ZTIDR-ZFTDTC.         " 납세자 사업?
  MOVE  'KRW'              TO  ZTIDR-ZFKRW.          " 원화통화.
  MOVE  'USD'              TO  ZTIDR-ZFUSD.          " 미화통화.
  MOVE  'N'                TO  ZTIDR-ZFDNCD.         " Download 여부.
  MOVE  'N'                TO  ZTIDR-ZFEDIST.        " EDI 상태.
  MOVE  'N'                TO  ZTIDR-ZFDOCST.        " DOC 상태.
  MOVE  SPACE              TO  ZTIDR-ZFDOCNO.        " 전자문서번호.
  MOVE  'X'                TO  ZTIDR-ZFEDICK.        " EDI CHECK
  MOVE  SY-UNAME           TO  ZTIDR-ERNAM.          " 생성인.
  MOVE  SY-DATUM           TO  ZTIDR-CDAT.           " 생성일자.
  MOVE  SY-UNAME           TO  ZTIDR-UNAM.           " 변경자.
  MOVE  SY-DATUM           TO  ZTIDR-UDAT.           " 변경일자.
  MOVE  ZTIMIMGTX-ZFAPNO2  TO  ZTIDR-ZFAPNO.         " 무역업등록번호.
  MOVE  ZTIMIMGTX-ZFIAPNM2 TO  ZTIDR-ZFIAPNM.        " 수입자 상호.
  MOVE  ZTBL-ZFHBLNO       TO  ZTIDR-ZFHBLNO.        " House B/L No
  MOVE  ZTBL-ZFWERKS       TO  ZTIDR-ZFWERKS.        " PLANT
  MOVE  ZTBL-ZFBLSDP       TO  ZTIDR-ZFBLSDP.        " 송부처.
  MOVE  ZTBL-ZFTRCK        TO  ZTIDR-ZFTRCK.         " Trucker
  MOVE  ZTBL-ZFTRQDT       TO  ZTIDR-ZFTRQDT.        " 운송요청일.
  MOVE  ZTIV-ZFEXRT        TO  ZTIDR-ZFEXRT.         " 환율.
  MOVE  ZTIV-FFACT         TO  ZTIDR-FFACT.          " RATIO.
  MOVE  ZTIV-ZFPONC        TO  ZTIDR-ZFPONC.         " 수입거래구분.
  MOVE  ZTBL-ZFAPRTC       TO  ZTIDR-ZFAPRTC.        " 도착항.
  MOVE  ZTBL-ZFCARC        TO  ZTIDR-ZFSCON.         " 적출국.
  MOVE  ZTBL-ZFETA         TO  ZTIDR-ZFENDT.         " 입항일.
  MOVE  ZTBL-ZFMATGB       TO  ZTIDR-ZFMATGB.        " 자재구분.
  MOVE  ZTBL-ZFREBELN      TO  ZTIDR-ZFREBELN.       " HOUSE B/L
  MOVE  ZTBL-ZFOPNNO       TO  ZTIDR-ZFOPNNO.        " L/C NO.

  MOVE: ZTBL-INCO1         TO  ZTIDR-INCO1,          " INCOTERMS
        'USD'              TO  ZTIDR-ZFTFAC,         " 운임 USD.
        'CT'               TO  ZTIDR-ZFPKNM,         " 포장종류.
        'KR'               TO  ZTIDR-ZFCAC,          " 선기국적.
        '10'               TO  ZTIDR-ZFINRCD.        " 세관 신고과.

*> 운송방법.., 포장종류.
  IF ZTBL-ZFVIA EQ 'VSL'.
    ZTIDR-ZFTRMET =  '10'.
    IF ZTBL-ZFSHTY EQ 'L' OR ZTBL-ZFSHTY EQ 'F'.
      ZTIDR-ZFTRCN = 'CN'.
    ELSEIF ZTBL-ZFSHTY EQ 'I' OR ZTBL-ZFSHTY EQ 'B'.
      ZTIDR-ZFTRCN = 'BU'.
    ELSE.
      ZTIDR-ZFTRCN = 'ETC'.
    ENDIF.
  ELSEIF ZTBL-ZFVIA EQ 'AIR'.
    ZTIDR-ZFTRMET =  '40'.
*     ZTIDR-ZFTRCN  =  'UN'.
    ZTIDR-ZFTRCN  =  'ETC'.
  ENDIF.

  IF NOT ZTBL-ZFREBELN IS INITIAL.
    SELECT SINGLE * FROM ZTREQHD
          WHERE    EBELN  EQ   ZTBL-ZFREBELN.
    CASE ZTREQHD-ZFREQTY.
      WHEN 'LC'.
        IF ZTREQHD-ZFLCKN EQ '1'.
          ZTIDR-ZFAMCD = 'LS'.
        ELSE.
          ZTIDR-ZFAMCD = 'LU'.
        ENDIF.
      WHEN 'TT'.
        ZTIDR-ZFAMCD = 'TT'.
      WHEN 'DP'.
        ZTIDR-ZFAMCD = 'DP'.
      WHEN 'DA'.
        ZTIDR-ZFAMCD = 'DA'.
      WHEN 'GS'.
        ZTIDR-ZFAMCD = 'LU'.
    ENDCASE.
  ENDIF.
  IF ZTBL-ZFPOYN EQ 'N'.   ">무환일 경우.
    ZTIDR-ZFAMCD = 'GN'.
  ENDIF.

*>> 수입자 구분.
  IF ZTBL-IMTRD = 'S'.
    MOVE  'A'             TO  ZTIDR-ZFIMCD.          "수입자구분.
  ELSEIF ZTBL-IMTRD = 'F'.
    MOVE  'B'             TO  ZTIDR-ZFIMCD.
  ELSE.
    CLEAR : ZTIDR-ZFIMCD.
  ENDIF.

  CALL FUNCTION 'ZIM_GET_COMPANY_DATA'
       EXPORTING
          BUKRS       =    ZTBL-BUKRS
          IMTRD       =    ZTBL-IMTRD
       IMPORTING
*          XT001       =    T001
*          XZTIMIMG00  =    ZTIMIMG00
           XZTIMIMGTX  =    ZTIMIMGTX
           OZTIMIMGTX  =    *ZTIMIMGTX
           XZTIMIMG11  =    ZTIMIMG11
       EXCEPTIONS
          NOT_FOUND   =    4.

  MOVE: ZTIMIMGTX-ZFIAPNM2  TO ZTIDR-ZFIAPNM,  ">수입자.
        ZTIMIMGTX-ZFAPNO2   TO ZTIDR-ZFAPNO.

  SELECT SINGLE * FROM LFA1
         WHERE LIFNR EQ ZTBL-LIFNR.
  MOVE :LFA1-NAME1 TO ZTIDR-ZFSUPNM,            ">공급자.
        LFA1-LAND1 TO ZTIDR-ZFSUPC.             ">공급자국적.

*--> 무역대리점..  --> OFFER 상호...추가.
  IF NOT ZTBL-ZFREBELN IS INITIAL.
    SELECT * FROM ZTREQHD UP TO 1 ROWS
             WHERE    EBELN EQ ZTBL-ZFREBELN.
    ENDSELECT.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE NAME1 INTO  ZTIDR-ZFTRDNM
                          FROM  LFA1
                          WHERE LIFNR EQ ZTREQHD-LLIEF.
    ENDIF.
  ENDIF.

  IF ZTBL-ZFVIA = 'AIR'.
    MOVE  '40'            TO  ZTIDR-ZFTRMET.          "운송수단.
  ENDIF.
  IF ZTBL-ZFVIA = 'VSL'.
    MOVE  '10'            TO  ZTIDR-ZFTRMET.
  ENDIF.

*>> 관세징수형태, 수입신고종류.
  IF ZTIDR-ZFMATGB EQ '1'.    ">수출용 원자재.
    MOVE : 'A'   TO  ZTIDR-ZFITKD,
           '33'  TO  ZTIDR-ZFCOCD.
  ELSE.
    MOVE : 'B'   TO  ZTIDR-ZFITKD,
           '13'  TO  ZTIDR-ZFCOCD.
  ENDIF.

*> 적하목록 관리번호, 포장종류, 총중량 단위,
  IF ZTBL-ZFRPTTY EQ 'B' OR ZTBL-ZFRPTTY EQ 'N' OR ZTBL-ZFRPTTY EQ 'W'.
    IF ZTIMIMG00-ZFINOU IS INITIAL.
      SELECT * FROM ZTBLINR_TMP UP TO 1 ROWS
               WHERE   ZFBLNO EQ ZTBL-ZFBLNO.
      ENDSELECT.

      IF SY-SUBRC EQ 0.
        CONCATENATE  ZTBLINR_TMP-ZFGMNO ZTBLINR_TMP-ZFMSN
                                        ZTBLINR_TMP-ZFHSN
                                  INTO ZTIDR-ZFGOMNO.
        MOVE  ZTBLINR_TMP-ZFABNAR(3)   TO  ZTIDR-ZFINRC.  ">세관.
        MOVE  ZTBLINR_TMP-ZFPKCNM      TO  ZTIDR-ZFPKNM.  ">포장종류.
        MOVE  ZTBLINR_TMP-ZFTOWTM      TO  ZTIDR-ZFTOWTM. ">중량단위.
        MOVE  ZTBLINR_TMP-ZFTOWT       TO  ZTIDR-ZFTOWT.  ">총중량.
        MOVE  ZTBLINR_TMP-ZFCARNM      TO  ZTIDR-ZFCARNM. ">선기명.
        MOVE  ZTBLINR_TMP-ZFINDT       TO  ZTIDR-ZFINDT.  ">반입일자.
        MOVE  ZTBLINR_TMP-ZFINRNO      TO  ZTIDR-ZFINRNO. ">반입신고.
        MOVE  ZTBLINR_TMP-ZFBNARCD     TO  ZTIDR-ZFBNARCD.">보세구역.
        MOVE  ZTBLINR_TMP-ZFBTRNO      TO  ZTIDR-ZFBTRNO. ">보운신고.
      ENDIF.
    ELSE.
      CLEAR : ZTBLINR, ZTBLINOU.
      SELECT * FROM  ZTBLINR UP TO 1 ROWS
               WHERE ZFBLNO  EQ ZTBL-ZFBLNO.
      ENDSELECT.
      SELECT SINGLE * FROM  ZTBLINOU
                      WHERE ZFBLNO   EQ  ZTBL-ZFBLNO
                      AND   ZFBTSEQ  EQ  ZTBLINR-ZFBTSEQ.

      IF SY-SUBRC EQ 0.
        CONCATENATE  ZTBLINOU-ZFGMNO ZTBLINOU-ZFMSN
                                     ZTBLINOU-ZFHSN
                                  INTO ZTIDR-ZFGOMNO.
        MOVE  ZTBLINR-ZFPKCNM   TO  ZTIDR-ZFPKNM.  ">포장종류.
        MOVE  ZTBLINR-ZFKG      TO  ZTIDR-ZFTOWTM.    ">총중량단위.
        MOVE  ZTBLINR-ZFINWT    TO  ZTIDR-ZFTOWT.     ">총중량.
        MOVE  ZTBL-ZFCARNM      TO  ZTIDR-ZFCARNM.    ">선기명.
        MOVE  ZTBLINR-ZFINDT    TO  ZTIDR-ZFINDT.     ">반입일자.
        MOVE  ZTBLINR-ZFINRNO   TO  ZTIDR-ZFINRNO.    ">반입신고번호.
        MOVE  ZTBLINR-ZFBNARCD  TO  ZTIDR-ZFBNARCD.   ">보세구역.
        MOVE  ZTBLINOU-ZFBTRNO  TO  ZTIDR-ZFBTRNO.    ">보운신고번호.
      ENDIF.
    ENDIF.
    IF SY-SUBRC NE 0.
      CONCATENATE  ZTBL-ZFGMNO ZTBL-ZFMSN ZTBL-ZFHSN
                  INTO  ZTIDR-ZFGOMNO.

      MOVE  ZTBL-ZFPKCNM       TO  ZTIDR-ZFPKNM.       ">포장종류.
      MOVE  ZTBL-ZFNEWTM       TO  ZTIDR-ZFTOWTM.      ">총중량단위.
      MOVE  ZTBL-ZFNEWT        TO  ZTIDR-ZFTOWT.       ">총중량.
      MOVE  ZTBL-ZFCARNM       TO  ZTIDR-ZFCARNM.      ">선기명.
    ENDIF.
  ELSE.
    CONCATENATE  ZTBL-ZFGMNO ZTBL-ZFMSN ZTBL-ZFHSN
                                  INTO  ZTIDR-ZFGOMNO.
    MOVE  ZTBL-ZFPKCNM       TO  ZTIDR-ZFPKNM.         ">포장종류.
    MOVE  ZTBL-ZFNEWTM       TO  ZTIDR-ZFTOWTM.        ">총중량단위.
    MOVE  ZTBL-ZFNEWT        TO  ZTIDR-ZFTOWT.         ">총중량
    MOVE  ZTBL-ZFCARNM       TO  ZTIDR-ZFCARNM.        ">선기명.
  ENDIF.

*>> 총중량, 총포장갯수 SETTING!
  SELECT SUM( CCMENGE )
  INTO   W_ZFQNT
  FROM   ZTIVIT
  WHERE  ZFIVNO  =  W_ZFIVNO.

  IF W_MENGE > 0 AND W_ZFQNT > 0.
    ZTIDR-ZFTOWT  = ZTBL-ZFNEWT * ( W_ZFQNT / W_MENGE ).
    ZTIDR-ZFPKCNT = ZTBL-ZFPKCN * ( W_ZFQNT / W_MENGE ).
  ENDIF.
*> 결제금액 SET.
  MOVE : ZTIV-ZFIVAMT   TO  ZTIDR-ZFSTAMT,
         ZTIV-ZFIVAMC   TO  ZTIDR-ZFSTAMC.

  MOVE ZTIDR-ZFSTAMC TO ZTIDR-ZFADAMCU.                 " 가산금액 통화.
  MOVE ZTIDR-ZFSTAMC TO ZTIDR-ZFDUAMCU.                 " 공제금액 통화.

**>> 비용방법에 따라서 비용금액 SUM TABLE 변경.
*  IF ZTIMIMG00-ZFPSMS EQ '1'.
**>> 해외 운임 통화 및 운임료 SETTING!
*     SELECT MAX( WAERS ) INTO ZTIDR-ZFTFAC               " 운임 A 통화.
*     FROM   ZTBLCST
*     WHERE  ZFBLNO EQ  ZTIDR-ZFBLNO
*     AND    WAERS  NE  'KRW'
*     AND    ZFCSQ  GT  10000.
*
*     SELECT SUM( ZFCAMT ) INTO ZTIDR-ZFTFA               " 운임 A 금액.
*     FROM   ZTBLCST
*     WHERE  ZFBLNO EQ  ZTIDR-ZFBLNO
*     AND    WAERS  EQ  ZTIDR-ZFTFAC
*     AND    ZFCSQ  GT  10000.
**>> 운임 B 통화 및 운임료 SUM.
*     SELECT MAX( WAERS ) INTO ZTIDR-ZFTFBC               " 운임 B 통?
*     FROM   ZTBLCST
*     WHERE  ZFBLNO EQ  ZTIDR-ZFBLNO
*     AND    WAERS  NE  ZTIDR-ZFTFAC
*     AND    WAERS  NE  'KRW'
*     AND    ZFCSQ  GT  10000.
*
*     SELECT SUM( ZFCAMT ) INTO ZTIDR-ZFTFB               " 운임 B 금?
*     FROM   ZTBLCST
*     WHERE  ZFBLNO EQ  ZTIDR-ZFBLNO
*     AND    WAERS  EQ  ZTIDR-ZFTFBC
*     AND    ZFCSQ  GT  10000.
*  ELSE.
**>> 운임 A 통화 및 운임료 SUM.
*     SELECT MAX( B~WAERS )    INTO ZTIDR-ZFTFAC          " 운임 A 통화.
*     FROM   ZTBSEG AS A INNER JOIN ZTBKPF AS B
*     ON     A~BUKRS     EQ    B~BUKRS
*     AND    A~GJAHR     EQ    B~GJAHR
*     AND    A~BELNR     EQ    B~BELNR
*     WHERE  A~ZFIMDNO   EQ    ZTIDR-ZFBLNO
*     AND    B~WAERS     NE    'KRW'
*     AND    A~ZFCSTGRP  EQ    '004'.
*
*     SELECT MAX( A~WRBTR )    INTO ZTIDR-ZFTFA           " 운임 A 통화.
*     FROM   ZTBSEG AS A INNER JOIN ZTBKPF AS B
*     ON     A~BUKRS     EQ    B~BUKRS
*     AND    A~GJAHR     EQ    B~GJAHR
*     AND    A~BELNR     EQ    B~BELNR
*     WHERE  A~ZFIMDNO   EQ    ZTIDR-ZFBLNO
*     AND    B~WAERS     EQ    ZTIDR-ZFTFAC
*     AND    A~ZFCSTGRP  EQ    '004'.
**>> 운임 B 통화 및 운임.
*     SELECT MAX( B~WAERS )    INTO ZTIDR-ZFTFBC          " 운임 B 통화.
*     FROM   ZTBSEG AS A INNER JOIN ZTBKPF AS B
*     ON     A~BUKRS     EQ    B~BUKRS
*     AND    A~GJAHR     EQ    B~GJAHR
*     AND    A~BELNR     EQ    B~BELNR
*     WHERE  A~ZFIMDNO   EQ    ZTIDR-ZFBLNO
*     AND    B~WAERS     NE    'KRW'
*     AND    B~WAERS     NE    ZTIDR-ZFTFAC
*     AND    A~ZFCSTGRP  EQ    '004'.
*
*     SELECT MAX( A~WRBTR )    INTO ZTIDR-ZFTFB           " 운임 B.
*     FROM   ZTBSEG AS A INNER JOIN ZTBKPF AS B
*     ON     A~BUKRS     EQ    B~BUKRS
*     AND    A~GJAHR     EQ    B~GJAHR
*     AND    A~BELNR     EQ    B~BELNR
*     WHERE  A~ZFIMDNO   EQ    ZTIDR-ZFBLNO
*     AND    B~WAERS     EQ    ZTIDR-ZFTFBC
*     AND    A~ZFCSTGRP  EQ    '004'.
*  ENDIF.
*
**>> 운임료 SETTING!
*  IF W_MENGE > 0.
*     ZTIDR-ZFTFA = ZTIDR-ZFTFA * ( W_ZFQNT / W_MENGE ). " 운임A
*     ZTIDR-ZFTFB = ZTIDR-ZFTFB * ( W_ZFQNT / W_MENGE ). " 운임B
*  ENDIF.

**수입신고인 경우는 보세운송 관련된 자료 SETTING!
*  IF ZTIV-ZFCLCD = 'A'.
*     SELECT SINGLE *
*     FROM   ZTBLINR
*     WHERE  ZFBLNO  =  ZTIV-ZFBLNO
*     AND    ZFBTSEQ =  ( SELECT MAX( ZFBTSEQ )
*                         FROM   ZTBLINR
*                         WHERE  ZFBLNO  =  ZTIV-ZFBLNO ).
*     IF SY-SUBRC EQ 0.
*        MOVE ZTBLINR-ZFINRC  TO ZTIDR-ZFINRC.
*        MOVE '10'            TO ZTIDR-ZFINRCD.
*        MOVE ZTBLINR-ZFGIRDT TO ZTIDR-ZFINDT.
*        IF ZTBLINR-ZFTXYN = 'X'.
*           MOVE '00'         TO ZTIDR-ZFCOCD.
*        ELSE.
*           MOVE '13'         TO ZTIDR-ZFCOCD.
*        ENDIF.
*        CONCATENATE ZTBLINR-ZFABNAR+0(8) ZTBLINR-ZFYR ZTBLINR-ZFSEQ
*               INTO ZTIDR-ZFISPL.
*        MOVE ZTBLINR-ZFINRNO  TO ZTIDR-ZFINRNO.
*        MOVE ZTBLINR-ZFBNARCD TO ZTIDR-ZFBNARCD.
*     ENDIF.
*
*     SELECT SINGLE *
*     FROM   ZTBLINOU
*     WHERE  ZFBLNO  =  ZTIV-ZFBLNO
*     AND    ZFBTSEQ = ( SELECT  MAX( ZFBTSEQ )
*                        FROM    ZTBLINOU
*                        WHERE   ZFBLNO  =  ZTIV-ZFBLNO ).
*     IF SY-SUBRC EQ 0.
*        MOVE  ZTBLINOU-ZFGMNO  TO  ZTIDR-ZFGOMNO.
*        MOVE  ZTBLINOU-ZFPKCN  TO  ZTIDR-ZFPKCNT.
*        MOVE  ZTBLINOU-ZFPKCNM TO  ZTIDR-ZFPKNM.
*        MOVE  ZTBLINOU-ZFWEINM TO  ZTIDR-ZFTOWTM.
*        MOVE  ZTBLINOU-ZFBTRNO TO  ZTIDR-ZFBTRNO.
*     ENDIF.
*  ENDIF.

  IF ZTIV-ZFCLCD NE 'B'.
*>> 가산금 SETTING!
*     MOVE ZTIV-ZFPKCHG       TO ZTIDR-ZFADAM. " 가산금?
*>> 공제금 SETTING!
*     SELECT SUM( KWERT )
*     INTO   W_ZFDUAM
*     FROM   ZTIVIT
*     WHERE  ZFIVNO  =  W_ZFIVNO.

*     MOVE W_ZFDUAM          TO ZTIDR-ZFDUAM.
*     MOVE 'GN'              TO ZTIDR-ZFAMCD.         "대금결제방?

    SORT IT_IVIT BY ZFREQNO.
    CLEAR : W_ZFREQNO, W_ZFINAMT.
    LOOP AT IT_IVIT.
      IF W_ZFREQNO NE IT_IVIT-ZFREQNO.
        REFRESH IT_REQIL.
        W_ZFCONO = W_ZFCONO + 1.
        ADD  1  TO  W_LOOP_CNT.
* 수입의뢰문서 SELECT!
        SELECT SINGLE * FROM ZTREQHD WHERE ZFREQNO = IT_IVIT-ZFREQNO.

* 관세사 기재란 SETTING!(위탁 통관일경우)
        IF ZTIMIMG00-ZFCUMTD EQ '1'.
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
        ENDIF.
        ADD ZTREQHD-ZFLASTAM  TO W_ZFLASTAM.
        CLEAR W_ZFCKAMT.
        SELECT SUM( ZFKRWAMT ) INTO W_ZFCKAMT
               FROM ZTINS
               WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.

        ADD W_ZFCKAMT    TO W_ZFINAMT. " 보험?

* 수입추천 TABLE SELECT 해서 수입신고 요건 확인 SETTING!
        SELECT *
        INTO   CORRESPONDING FIELDS OF TABLE IT_REQIL
        FROM   ZTREQIL
        WHERE  ZFREQNO  =  IT_IVIT-ZFREQNO .

        LOOP AT IT_REQIL.
          IF NOT ( IT_REQIL-ZFRECNO IS INITIAL ).
            CLEAR ZTIDRHSL.
            MOVE SY-MANDT         TO ZTIDRHSL-MANDT.
            MOVE ZTIV-ZFBLNO      TO ZTIDRHSL-ZFBLNO.
            MOVE W_ZFCLSEQ        TO ZTIDRHSL-ZFCLSEQ.
            MOVE W_ZFCONO         TO ZTIDRHSL-ZFCONO.
            MOVE '911'            TO ZTIDRHSL-ZFCNDC.
            MOVE IT_REQIL-ZFRECNO TO ZTIDRHSL-ZFCNNO.

            MOVE-CORRESPONDING ZTIDRHSL TO IT_ZTIDRHSL.
            APPEND  IT_ZTIDRHSL.
          ENDIF.

        ENDLOOP.
        W_ZFREQNO = IT_IVIT-ZFREQNO.
      ENDIF.
      IF W_ZFCONO NE 1. CONTINUE. ENDIF.
    ENDLOOP.

    ZTIDR-ZFINAMTS = W_ZFINAMT.

    IF W_ZFLASTAM  > 0.
      ZTIDR-ZFINAMT = W_ZFINAMT * ( ZTIDR-ZFSTAMT / W_ZFLASTAM ).
    ENDIF.
  ENDIF.

*> 총보험료.
  ZTIDR-ZFINAMTS = W_ZFINAMT.
  ZTIDR-ZFTOTAMT = W_ZFLASTAM.

*------------------------------------------------------------------
*>> 수입신고 란 사항 INSERT!
*------------------------------------------------------------------
*  IF ZTBL-ZFSVYN NE 'X'.
  W_ZFCONO = 0.
  REFRESH : IT_IVIT_TMP.
  APPEND LINES OF IT_IVIT TO IT_IVIT_TMP.

  LOOP AT IT_IVIT.
*    SELECT  SINGLE * FROM ZTIDRHS
*    WHERE   ZFBLNO  =  IT_IVIT-ZFBLNO
*    AND     ZFCLSEQ =  ZTCUCLIV-ZFCLSEQ
*    AND     STAWN   =  IT_IVIT-STAWN.
    READ TABLE IT_ZTIDRHS WITH KEY ZFBLNO  = IT_IVIT-ZFBLNO
                                   ZFCLSEQ = W_ZFCLSEQ
                                   STAWN   = IT_IVIT-STAWN.

    IF SY-SUBRC NE 0.
      CLEAR  W_ZFRONO.
      CLEAR  ZTIDRHS.
      SELECT MAX( ZFCONO ) INTO ZTIDRHS-ZFCONO
      FROM   ZTIDRHS
      WHERE  ZFBLNO  =  IT_IVIT-ZFBLNO
      AND    ZFCLSEQ =  W_ZFCLSEQ.

      MOVE SY-MANDT            TO ZTIDRHS-MANDT.
      MOVE IT_IVIT-ZFBLNO      TO ZTIDRHS-ZFBLNO.
      MOVE W_ZFCLSEQ           TO ZTIDRHS-ZFCLSEQ.
      ADD  1                   TO W_ZFCONO.
      MOVE W_ZFCONO            TO ZTIDRHS-ZFCONO.    "란사항.
      MOVE IT_IVIT-STAWN       TO ZTIDRHS-STAWN.     "HS Code

*> 한수원 주석처리 ( 수량과 중량 넣지 않음 ).
*      SELECT  SUM( CCMENGE )   MAX( MEINS )
*      INTO    (ZTIDRHS-ZFQNT, ZTIDRHS-ZFQNTM)
*      FROM    ZTIVIT
*      WHERE   ZFIVNO    EQ    W_ZFIVNO
*      AND     STAWN     EQ    IT_IVIT-STAWN .

      ZTIDRHS-ZFGDNM = IT_IVIT-TXZ01.                  "품명.

      SELECT  MAX( TEXT1 )  INTO ZTIDRHS-ZFTGDNM       "거래품?
      FROM    T604T
      WHERE   SPRAS = SY-LANGU
      AND     STAWN = IT_IVIT-STAWN.

      IF ZTIDRHS-ZFTGDNM IS INITIAL.
        ZTIDRHS-ZFTGDNM = ZTIDRHS-ZFGDNM.
      ENDIF.

*> 과세가격 외화.
      L_RATE = ZTIDRHS-ZFQNT / W_ZFQNT.
      ZTIDRHS-ZFTBAU = L_RATE * ZTIDR-ZFSTAMT.

*> 과세가격 원화.
      IF ZTIDR-FFACT IS INITIAL.
        ZTIDR-FFACT  =  1.
      ENDIF.
*>과세가격 외화 출력용으로 전환.
      PERFORM SET_CURR_CONV_TO_EXTERNAL(SAPMZIM01)
                                   USING ZTIDRHS-ZFTBAU
                                        'USD' ZTIDRHS-ZFTBAK.

      ZTIDRHS-ZFTBAK = ZTIDRHS-ZFTBAK *
                      ( ZTIDR-ZFEXRT / ZTIDR-FFACT ).

      PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
                                   USING ZTIDRHS-ZFTBAK 'KRW'.

*> 수량.
*       IF NOT ZTBL-ZFPKCNM IS INITIAL.
*          ZTIDRHS-ZFQNTM = ZTBL-ZFPKCNM.
*       ENDIF.

*              ZTIDRHS-ZFQNTM
*       IF ( ZTIDRHS-ZFQNTM NE 'L'  ) AND
*          ( ZTIDRHS-ZFQNTM NE 'KG' ) AND
*          ( ZTIDRHS-ZFQNTM NE 'G'  ).
*          MOVE 'U'    TO ZTIDRHS-ZFQNTM.
*       ENDIF.

*>> 한수원 주석처리.
*      IF W_MENGE > 0.                              " 란사항 중량.
*        ZTIDRHS-ZFWET = ZTBL-ZFNEWT * ( ZTIDRHS-ZFQNT / W_MENGE ).
*      ELSE.
        ZTIDRHS-ZFWET = 0.
*      ENDIF.
*      MOVE ZTBL-ZFNEWTM TO ZTIDRHS-ZFWETM.
*> 원산지...
      MOVE : ZTIDR-ZFSCON TO ZTIDRHS-ZFORIG.

      MOVE 'KRW'     TO ZTIDRHS-ZFKRW.
      MOVE 'USD'     TO ZTIDRHS-ZFUSD.

*>> 한수원 추가. JSY20021126.-------------------------->
      MOVE : '0'     TO ZTIDRHS-ZFCDPGB,  " 관세감면구분(과세).
             'A'     TO ZTIDRHS-ZFVTXCD,  " 부가세 구분(과세).
             'E'     TO ZTIDRHS-ZFORYN.   " 원산지 표시유무.
*------------------------------------------------------>
*>>> 자가 통관일 경우 ==============================>
      IF ZTIMIMG00-ZFCUMTD EQ '2'.
        MOVE : 'A'     TO ZTIDRHS-ZFTRRL,   " 거래관계.
               'D'     TO ZTIDRHS-ZFGDAL,   " 품명규격세번.
               'A'     TO ZTIDRHS-ZFEXOP.   " 검사의견.

        CLEAR : W_LOOP_CNT, W_PONO, W_SIBG.
        SORT IT_IVIT_TMP BY ZFBLNO STAWN EBELN.

        LOOP AT IT_IVIT_TMP  WHERE ZFBLNO  = IT_IVIT-ZFBLNO
                               AND STAWN   = IT_IVIT-STAWN.
*       반입번호읽어오기.
          CLEAR ZTBLINR_TMP.
          SELECT SINGLE * FROM ZTBLINR_TMP
                         WHERE ZFBLNO = IT_IVIT-ZFBLNO.

          IF IT_IVIT_TMP-EBELN NE W_PONO OR
             ZTBLINR_TMP-ZFSIBG NE W_SIBG.

            ADD 1 TO W_LOOP_CNT .

            IF W_LOOP_CNT = 1.
              ZTIDRHS-ZFCTW1 = IT_IVIT_TMP-EBELN.
              IF NOT ZTBLINR_TMP-ZFSIBG IS INITIAL.
                CONCATENATE  ZTIDRHS-ZFCTW1 '-'
                      ZTBLINR_TMP-ZFSIBG INTO ZTIDRHS-ZFCTW1.
              ENDIF.
            ENDIF.

            IF W_LOOP_CNT = 2.
              CONCATENATE ZTIDRHS-ZFCTW1 ', ' IT_IVIT_TMP-EBELN
                                            INTO ZTIDRHS-ZFCTW1.
              IF NOT ZTBLINR_TMP-ZFSIBG IS INITIAL.
                CONCATENATE  ZTIDRHS-ZFCTW1 '-'
                      ZTBLINR_TMP-ZFSIBG INTO ZTIDRHS-ZFCTW1.
              ENDIF.
            ENDIF.

            IF W_LOOP_CNT = 3.
              ZTIDRHS-ZFCTW2 = IT_IVIT_TMP-EBELN.
              IF NOT ZTBLINR_TMP-ZFSIBG IS INITIAL.
                CONCATENATE  ZTIDRHS-ZFCTW2 '-'
                      ZTBLINR_TMP-ZFSIBG INTO ZTIDRHS-ZFCTW2.
              ENDIF.
            ENDIF.

            IF W_LOOP_CNT = 4.
              CONCATENATE ZTIDRHS-ZFCTW2 ', ' IT_IVIT_TMP-EBELN
                                            INTO ZTIDRHS-ZFCTW2.
              IF NOT ZTBLINR_TMP-ZFSIBG IS INITIAL.
                CONCATENATE  ZTIDRHS-ZFCTW2 '-'
                      ZTBLINR_TMP-ZFSIBG INTO ZTIDRHS-ZFCTW2.
              ENDIF.
            ENDIF.

            W_PONO = IT_IVIT_TMP-EBELN .
            W_SIBG = ZTBLINR_TMP-ZFSIBG.
          ENDIF.
        ENDLOOP.
      ENDIF.

      MOVE-CORRESPONDING ZTIDRHS TO IT_ZTIDRHS.
      APPEND IT_ZTIDRHS.
    ENDIF.

*------------------------------------------------------------------
* 수입신고 규격 TABLE INSERT!
*------------------------------------------------------------------
    CLEAR ZTIDRHSD.
*   SELECT MAX( ZFRONO ) INTO ZTIDRHSD-ZFRONO
*   FROM   ZTIDRHSD
*   WHERE  ZFBLNO   =  ZTIDRHS-ZFBLNO
*   AND    ZFCLSEQ  =  ZTIDRHS-ZFCLSEQ
*   AND    ZFCONO   =  ZTIDRHS-ZFCONO.

    MOVE SY-MANDT            TO ZTIDRHSD-MANDT.
    MOVE ZTIDRHS-ZFBLNO      TO ZTIDRHSD-ZFBLNO.
    MOVE ZTIDRHS-ZFCLSEQ     TO ZTIDRHSD-ZFCLSEQ.
    MOVE IT_IVIT-ZFIVNO      TO ZTIDSHSD-ZFIVNO.
    MOVE IT_IVIT-ZFIVDNO     TO ZTIDSHSD-ZFIVDNO.
    MOVE IT_IVIT-MATNR       TO ZTIDSHSD-MATNR.
    MOVE ZTIDRHS-ZFCONO      TO ZTIDRHSD-ZFCONO.

    ADD  1                   TO W_ZFRONO.
    MOVE W_ZFRONO            TO ZTIDRHSD-ZFRONO.      "?

    MOVE IT_IVIT-ZFIVNO      TO ZTIDRHSD-ZFIVNO.     "관리번?
    MOVE IT_IVIT-ZFIVDNO     TO ZTIDRHSD-ZFIVDNO.    "일련번?
    MOVE IT_IVIT-TXZ01       TO ZTIDRHSD-ZFGDDS1.           "규격1
    MOVE IT_IVIT-CCMENGE     TO ZTIDRHSD-ZFQNT.       "수?
    MOVE IT_IVIT-MEINS       TO ZTIDRHSD-ZFQNTM.      "수량단?
    MOVE IT_IVIT-NETPR       TO ZTIDRHSD-NETPR.       "단?
    MOVE IT_IVIT-PEINH       TO ZTIDRHSD-PEINH.       "Price uni
    MOVE IT_IVIT-BPRME       TO ZTIDRHSD-BPRME.       "Order pri
    MOVE IT_IVIT-ZFIVAMT     TO ZTIDRHSD-ZFAMT.       "금?
    MOVE IT_IVIT-ZFIVAMC     TO ZTIDRHSD-ZFCUR.       "통?
    MOVE IT_IVIT-STAWN       TO ZTIDRHSD-STAWN.       "HS Code
    MOVE IT_IVIT-MATNR       TO ZTIDRHSD-MATNR.
    MOVE IT_IVIT-MATNR       TO W_TEXT12.

*   IF ZTIDRHSD-ZFAMT IS INITIAL.
*   ENDIF.

*>> 자재코드의 '0' 문자 제거.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = W_TEXT12
      IMPORTING
        OUTPUT = W_TEXT24.

    MOVE  W_TEXT24    TO  ZTIDRHSD-ZFSTCD.

    IF NOT ( IT_IVIT-ZFREQNO IS INITIAL ).            "수입의뢰수?
      SELECT SINGLE MENGE INTO ZTIDRHSD-ZFMENGE
      FROM   ZTREQIT
      WHERE  ZFREQNO = IT_IVIT-ZFREQNO
      AND    ZFITMNO = IT_IVIT-ZFITMNO.
    ENDIF.
    MOVE-CORRESPONDING ZTIDRHSD TO IT_ZTIDRHSD.
    APPEND IT_ZTIDRHSD.
  ENDLOOP.
*  ENDIF.
**>>란별 금액 계산.
*  LOOP AT IT_ZTIDRHS.
*     W_TABIX = SY-TABIX.
*     CLEAR : IT_ZTIDRHS-ZFTBAU, IT_ZTIDRHS-ZFTBAK.
*     LOOP AT IT_ZTIDRHSD WHERE ZFCONO EQ IT_ZTIDRHS-ZFCONO.
*        ADD IT_ZTIDRHSD-ZFAMT TO IT_ZTIDRHS-ZFTBAU.
*     ENDLOOP.
*
*     PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZTIDRHS-ZFTBAU
*                                             ZTIDR-ZFSTAMC
*                                             BAPICURR-BAPICURR.
*
*
*        PERFORM SET_CURR_CONV_TO_INTERNAL USING
*                 W_ZFCAMTK 'KRW'.
*
*
*
*
*
*     IT_ZTIDRHS-ZFTBAK = IT_ZTIDRHS-ZFTBAU * ( ZTIDR-ZFEXRT /
*                                               ZTIDR-FFACT ).
*     MODIFY IT_ZTIDRHS.
*  ENDLOOP.
*

*>>> EDI Status check...
*>> EDI CHECK FUNCTION CALL!
  CALL FUNCTION 'ZIM_CUDATA_EDI_CHK'
    TABLES
      IT_ZTIDRHS  = IT_ZTIDRHS
      IT_ZTIDRHSD = IT_ZTIDRHSD
      IT_ZTIDRHSL = IT_ZTIDRHSL
    CHANGING
      ZTIDR       = ZTIDR.

*  SET UPDATE TASK LOCAL.

  INSERT ZTIDR.
*>> ERROR 발생시 DATA ROLLBACK
  IF SY-SUBRC NE 0.
*     ROLLBACK WORK.
    RAISE ERROR_INSERT.
  ENDIF.

  REFRESH : IT_ZTIDRHS1.
  LOOP AT IT_ZTIDRHS.
    MOVE-CORRESPONDING IT_ZTIDRHS TO IT_ZTIDRHS1.
    APPEND IT_ZTIDRHS1.
  ENDLOOP.

  IF SY-SUBRC EQ 0.
    INSERT ZTIDRHS FROM TABLE IT_ZTIDRHS1.
    IF SY-SUBRC NE 0.
*        ROLLBACK WORK.
      RAISE ERROR_INSERT.
    ENDIF.
  ENDIF.

  REFRESH : IT_ZTIDRHSD1.
  LOOP AT IT_ZTIDRHSD.
    MOVE-CORRESPONDING IT_ZTIDRHSD TO IT_ZTIDRHSD1.
    APPEND IT_ZTIDRHSD1.
  ENDLOOP.

  IF SY-SUBRC EQ 0.
    INSERT ZTIDRHSD FROM TABLE IT_ZTIDRHSD1.
    IF SY-SUBRC NE 0.
*        ROLLBACK WORK.
      RAISE ERROR_INSERT.
    ENDIF.
  ENDIF.

  REFRESH : IT_ZTIDRHSL1.
  LOOP AT IT_ZTIDRHSL.
    MOVE-CORRESPONDING IT_ZTIDRHSL TO IT_ZTIDRHSL1.
    APPEND IT_ZTIDRHSL1.
  ENDLOOP.

  IF SY-SUBRC EQ 0.
    INSERT ZTIDRHSL FROM TABLE IT_ZTIDRHSL1.
    IF SY-SUBRC NE 0.
*        ROLLBACK WORK.
      RAISE ERROR_INSERT.
    ENDIF.
  ENDIF.

*  MOVE  SY-UNAME TO ZTIV-UNAM.
*  MOVE  SY-DATUM TO ZTIV-UDAT.
*  MOVE  W_STATUS TO ZTIV-ZFCUST.
*
  UPDATE ZTIV
         SET  ZFCUST = W_STATUS
              UNAM   = SY-UNAME
              UDAT   = SY-DATUM
         WHERE ZFIVNO EQ W_ZFIVNO.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    RAISE ERROR_INSERT.
  ENDIF.

*  COMMIT WORK.

ENDFUNCTION.
