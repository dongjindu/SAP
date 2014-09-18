FUNCTION ZIM_CUDATA_PAY_CREATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(W_ZFIVNO) LIKE  ZTIV-ZFIVNO
*"  EXPORTING
*"     REFERENCE(W_ZFBLNO) LIKE  ZTBL-ZFBLNO
*"     REFERENCE(W_ZFCLSEQ) LIKE  ZTIDR-ZFCLSEQ
*"  EXCEPTIONS
*"      ERROR_INSERT
*"----------------------------------------------------------------------
*-----------------------------------------------------------------------
* INTERNAL TABLE DEFINE
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IVIT OCCURS 0.
      INCLUDE  STRUCTURE ZTCUCLIVIT.
DATA: END OF IT_IVIT.
DATA: BEGIN OF IT_REQIL OCCURS 0.
      INCLUDE  STRUCTURE ZTREQIL.
DATA: END OF IT_REQIL.

*-----------------------------------------------------------------------
* 변수 DEFINE
*-----------------------------------------------------------------------
DATA : W_ZFINAMT    LIKE  ZTIDR-ZFINAMT ,
       W_STAMT      LIKE  ZTIDRHSD-ZFAMT,
       W_ZFLASTAM   LIKE  ZTREQHD-ZFLASTAM,
       W_MENGE      LIKE  ZTCUCLIVIT-MENGE,
       W_ZFQNT      LIKE  ZTIDRHS-ZFQNT,
       W_ZFDUAM     LIKE  ZTIDR-ZFDUAM,
       W_ZFREQNO    LIKE  ZTREQHD-ZFREQNO,
       W_ZFCONO     LIKE  ZTIDRHSD-ZFCONO,
       W_ZFCKAMT    LIKE  ZTRECST-ZFCKAMT,
       W_LOOP_CNT   TYPE  I,
       W_STATUS     TYPE  C.

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

  CLEAR : ZTCUCL,    ZTCUCLIV,   ZTIDR, ZTIV.
  CLEAR : W_ZFINAMT, W_ZFLASTAM, W_LOOP_CNT, W_MENGE..
  CLEAR : W_ZFREQNO.

*>> IMG 사항 SELECT.
  SELECT  SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFIMPATH EQ '3'.
     W_STATUS  =  '3'.
  ELSE.
     W_STATUS  =  '2'.
  ENDIF.

*>> BL NO GET
  SELECT  SINGLE * FROM ZTIV  WHERE  ZFIVNO = W_ZFIVNO.

*>> BL 수량 SUM.
    SELECT  SUM( BLMENGE ) INTO W_MENGE
    FROM    ZTBLIT
    WHERE   ZFBLNO      EQ   ZTIV-ZFBLNO .

*>> 통관용 INVOICE TABLE 통관 상태 UPDATE.
    CLEAR : ZTCUCLIV, ZTCUCL.
    SELECT SINGLE *
    FROM   ZTCUCLIV
    WHERE  ZFIVNO  =  W_ZFIVNO.
    IF SY-SUBRC NE 0. RAISE  ERROR_INSERT. ENDIF.

    MOVE SY-UNAME TO ZTCUCLIV-UNAM.
    MOVE SY-DATUM TO ZTCUCLIV-UDAT.
    MOVE W_STATUS TO ZTCUCLIV-ZFCUST.
    UPDATE ZTCUCLIV.
    IF SY-SUBRC NE 0.
       RAISE  ERROR_INSERT.
    ENDIF.
*>> 통관 TABLE INSERT!
    SELECT SINGLE *
    FROM   ZTCUCL
    WHERE  ZFBLNO   EQ  ZTCUCLIV-ZFBLNO
    AND    ZFCLSEQ  EQ  ZTCUCLIV-ZFCLSEQ.
    IF SY-SUBRC NE 0.
       MOVE ZTCUCLIV-ZFBLNO  TO ZTCUCL-ZFBLNO.     "BL NO.
       MOVE ZTCUCLIV-ZFCLSEQ TO ZTCUCL-ZFCLSEQ.    "SEQ
       MOVE 'USD'            TO ZTCUCL-ZFUSD.      "USD 통화.
       MOVE 'KRW'            TO ZTCUCL-ZFKRW.      "KRW 통화.
       MOVE ZTCUCLIV-ZFCLCD  TO ZTCUCL-ZFCLCD.     "통관구분 = 수입통?
       MOVE W_STATUS         TO ZTCUCL-ZFCUST.     "통관상태 = 의뢰대?
       MOVE SY-UNAME         TO ZTCUCL-ERNAM.      "생성인.
       MOVE SY-DATUM         TO ZTCUCL-CDAT.       "생성일.
       MOVE SY-UNAME         TO ZTCUCL-UNAM.       "변경인.
       MOVE SY-DATUM         TO ZTCUCL-UDAT.       "변경일.
       INSERT ZTCUCL.
*>> ERROR 발생시 기존 자료 ROLLBACK
       IF SY-SUBRC NE 0.
          UPDATE  ZTCUCLIV
          SET     ZFCUST   =  '1'
                  UNAM     =  SY-UNAME
                  UDAT     =  SY-DATUM
          WHERE   ZFIVNO   =  W_ZFIVNO.
          RAISE   ERROR_INSERT.
       ENDIF.
    ENDIF.
* 수입신고 TABLE, 수입신고 란사항, 요건확인, 규격 TABLE INSERT!
    CLEAR : ZTIDR, ZTIMIMG00.
    SELECT SINGLE * FROM ZTIMIMG00.
*>> EDI용 TEXT.
    SELECT SINGLE * FROM ZTIMIMGTX
           WHERE    BUKRS  EQ   ZTIV-BUKRS.

* 납세자 관련 자료 TABLE INSERT!
    SELECT * FROM ZTIMIMG02 UP TO 1 ROWS
             WHERE  ZFWERKS  =  ZTBL-ZFWERKS.
    ENDSELECT.

* 수입신고 번호 생성.
    IF W_STATUS = '3'.
       MOVE ZTCUCLIV-ZFCCDT      TO   ZTIDR-ZFIDWDT.
       MOVE ZTCUCLIV-ZFCCDT+0(4) TO   W_YYYY.
       CONCATENATE W_YYYY '0101' INTO W_YYYYMMDD_FROM.
       CONCATENATE W_YYYY '1231' INTO W_YYYYMMDD_TO.
       CLEAR W_ZFIDRNO.

       CONCATENATE '_____' SY-DATUM+2(2) '8______' INTO L_ZFIDRNO.
*--------------> INFOLINKE DREAMKSB <-----------------------------------
*---> 2000/10/02 안덕기 과장 요청 ( 800001 이후 번호로 채번 )
*---> 연도별?
*                          AND ZFIDRNO LIKE '_______8_____'
       SELECT MAX( ZFIDRNO ) INTO W_ZFIDRNO FROM ZTIDR
                             WHERE ZFCUT = ZTCUCLIV-ZFCUT
                             AND ZFIDRNO LIKE L_ZFIDRNO
                             AND ZFIDWDT >= W_YYYYMMDD_FROM
                             AND ZFIDWDT <= W_YYYYMMDD_TO
                             AND ( ZFBLNO NE ZTIDR-ZFBLNO
                             OR    ZFCLSEQ NE ZTIDR-ZFCLSEQ ).
       IF W_ZFIDRNO IS INITIAL.
          MOVE ZTIDR-ZFIDWDT+2(2) TO W_YEAR.
          MOVE '800000'           TO W_SEQ.
       ELSE.
          MOVE W_ZFIDRNO+5(2)     TO W_YEAR.
          MOVE W_ZFIDRNO+7(6)     TO W_SEQ.
       ENDIF.
       ADD  1                     TO W_SEQ.  " 수입신고번?
*       W_TMP = ZTCUCLIV-ZFCUT+0(1) * 7 + ZTCUCLIV-ZFCUT+1(1) * 3
*             + ZTCUCLIV-ZFCUT+2(1) * 1 + ZTCUCLIV-ZFCUT+3(1) * 7
*             + ZTCUCLIV-ZFCUT+4(1) * 3
*             + W_YEAR+0(1) * 1 + W_YEAR+1(1) * 7
*             + W_SEQ+0(1) * 3 + W_SEQ+1(1) * 1
*             + W_SEQ+2(1) * 7 + W_SEQ+3(1) * 3
*             + W_SEQ+4(1) * 1 + W_SEQ+5(1) * 7.
       W_TMP_1 = W_TMP MOD 10.
       W_CHK   = 10 - W_TMP_1.
       CONCATENATE ZTCUCLIV-ZFCUT W_YEAR W_SEQ W_CHK INTO ZTIDR-ZFIDRNO.
*>> 수입신고 번호 중복 CHECK!
       SELECT  COUNT( DISTINCT ZFIDRNO )  INTO  W_COUNT
       FROM    ZTIDR
       WHERE   ZFIDRNO  =  ZTIDR-ZFIDRNO.
       IF W_COUNT >  0.
          W_SEQ  =  W_SEQ  +  1.
          CONCATENATE ZTCUCLIV-ZFCUT W_YEAR W_SEQ W_CHK
                                     INTO ZTIDR-ZFIDRNO.
       ENDIF.

       CONCATENATE ZTIDR-ZFREBELN
                   W_YEAR W_SEQ INTO ZTIDR-ZFIMCR. " 무역업체참조번?

    ENDIF.

    MOVE SY-MANDT            TO ZTIDR-MANDT.
    MOVE ZTCUCL-BUKRS        TO ZTIDR-BUKRS.
    MOVE SY-DATUM            TO ZTIDR-ZFIDWDT.    " 신고일.
    MOVE ZTCUCL-ZFBLNO       TO ZTIDR-ZFBLNO.     " B/L 관리번호.
    MOVE ZTCUCL-ZFCLSEQ      TO ZTIDR-ZFCLSEQ.    " 통관순번.
    MOVE ZTCUCLIV-ZFCUT      TO ZTIDR-ZFCUT.
    MOVE 'B'                 TO ZTIDR-ZFITKD.     " 수입신고종류.
    MOVE 'ETC'               TO ZTIDR-ZFTRCN.     " 운송용.
    MOVE ZTIMIMG02-ZFCOTM    TO ZTIDR-ZFINRC.     " 신고지 세관.
    MOVE ZTIMIMG02-ZFTDNO    TO ZTIDR-ZFTDNO.     " 납세자 통관.
    MOVE ZTIMIMGTX-ZFTDNM1   TO ZTIDR-ZFTDNM1.    " 납세자 상.
    MOVE ZTIMIMGTX-ZFTDNM2   TO ZTIDR-ZFTDNM2.    " 납세자 성명.
    MOVE ZTIMIMG02-ZFTDAD1   TO ZTIDR-ZFTDAD1.    " 납세자 주소1
    MOVE ZTIMIMG02-ZFTDAD2   TO ZTIDR-ZFTDAD2.    " 납세자 주소2
    MOVE ZTIMIMG02-ZFTDTC    TO ZTIDR-ZFTDTC.     " 납세자 사업.
    MOVE 'KRW'               TO ZTIDR-ZFKRW.      " 원화통화.
    MOVE 'USD'               TO ZTIDR-ZFUSD.      " 미화통화.
    MOVE 'N'                 TO ZTIDR-ZFDNCD.     " Download 여부.
    MOVE SY-UNAME            TO ZTIDR-ERNAM.      " 생성인.
    MOVE SY-DATUM            TO ZTIDR-CDAT.       " 생성일.
    MOVE SY-UNAME            TO ZTIDR-UNAM.       " 변경자.
    MOVE SY-DATUM            TO ZTIDR-UDAT.       " 변경일.
*>> BL 관련 정보 READ 해서 DATA SET!
    SELECT SINGLE * FROM ZTBL WHERE ZFBLNO = ZTCUCL-ZFBLNO.
    MOVE ZTBL-ZFPONC         TO ZTIDR-ZFPONC.     " 수입거래구?
    MOVE ZTBL-ZFAPRTC        TO ZTIDR-ZFAPRTC.    " 도착항.
    MOVE ZTBL-ZFCARC         TO ZTIDR-ZFSCON.     " 적출항.
    MOVE ZTBL-ZFETA          TO ZTIDR-ZFENDT.     " 입항.
*>> 유환일 경우.
    IF ZTBL-ZFPOYN = 'Y'.
*       MOVE 'B'                TO ZTIDR-ZFIMCD.   "수입자구?
*      MOVE ZTIMIMGTX-ZFAPNO1  TO ZTIDR-ZFAPNO.   "수입자 무역업등록?
*      MOVE ZTIMIMGTX-ZFIAPNM1 TO ZTIDR-ZFIAPNM.  "수입자 상?
       MOVE ZTIMIMGTX-ZFAPNO2  TO ZTIDR-ZFAPNO.   "수입자 무역업등록?
       MOVE ZTIMIMGTX-ZFIAPNM2 TO ZTIDR-ZFIAPNM.  "수입자 상?
    ENDIF.
*>> 무환일 경우.
    IF ZTBL-ZFPOYN = 'N'.
*       MOVE 'A'                TO ZTIDR-ZFIMCD.
       MOVE ZTIMIMGTX-ZFAPNO2  TO ZTIDR-ZFAPNO.   "수입자 무역업등록?
       MOVE ZTIMIMGTX-ZFIAPNM2 TO ZTIDR-ZFIAPNM.  "수입자 상?
    ENDIF.
    MOVE ZTBL-ZFHBLNO        TO ZTIDR-ZFHBLNO.    "House B/L No
    IF ZTBL-ZFVIA = 'AIR'.
       MOVE '40'             TO ZTIDR-ZFTRMET.    "운송수단.
    ENDIF.
    IF ZTBL-ZFVIA = 'VSL'.
       MOVE '10'             TO ZTIDR-ZFTRMET.
    ENDIF.
    MOVE ZTBL-ZFCARNM        TO ZTIDR-ZFCARNM.     "선기?
    MOVE ZTBL-ZFREBELN       TO ZTIDR-ZFREBELN.    "대표P/O번호.
    MOVE ZTBL-ZFOPNNO        TO ZTIDR-ZFOPNNO.     "대표L/C번호.
    MOVE ZTBL-ZFPRNAM        TO ZTIDR-ZFPRNAM.     "P/R담당자.
    MOVE ZTBL-ZFMATGB        TO ZTIDR-ZFMATGB.     "자재구분.
    MOVE ZTBL-ZFPKCNM        TO ZTIDR-ZFPKNM.      "포장종류.
    MOVE ZTBL-ZFTOWTM        TO ZTIDR-ZFTOWTM.     "총중량단위.
*>> 운임 통화, 운임료 SUM.
    SELECT MAX( WAERS )  INTO ZTIDR-ZFTFAC
    FROM   ZTBLCST
    WHERE  ZFBLNO  EQ  ZTIDR-ZFBLNO
    AND    WAERS   NE  'KRW'
    AND    ZFCSCD  GT  10000.

    SELECT SUM( ZFCAMT ) INTO ZTIDR-ZFTFA
    FROM   ZTBLCST
    WHERE  ZFBLNO  EQ  ZTIDR-ZFBLNO
    AND    WAERS   EQ  ZTIDR-ZFTFAC
    AND    ZFCSCD  GT  10000.

    SELECT MAX( WAERS )  INTO  ZTIDR-ZFTFBC
    FROM   ZTBLCST
    WHERE  ZFBLNO   EQ  ZTIDR-ZFBLNO
    AND    WAERS    NE  ZTIDR-ZFTFAC
    AND    WAERS    NE  'KRW'
    AND    ZFCSCD   GT  10000.

    SELECT SUM( ZFCAMT )  INTO ZTIDR-ZFTFB
    FROM   ZTBLCST
    WHERE  ZFBLNO   EQ   ZTIDR-ZFBLNO
    AND    WAERS    EQ   ZTIDR-ZFTFBC
    AND    ZFCSCD   GT   10000.
*>> 총중량, 포장갯수 SETTING!
    SELECT  SUM( I~MENGE )  INTO    W_ZFQNT
    FROM    ZTCUCLIV AS H INNER JOIN ZTCUCLIVIT AS I
    ON      H~ZFIVNO    EQ   I~ZFIVNO
    WHERE   ZFBLNO      EQ   ZTCUCL-ZFBLNO
    AND     ZFCLSEQ     EQ   ZTCUCL-ZFCLSEQ.

    IF W_MENGE > 0.
       ZTIDR-ZFTOWT  = ZTBL-ZFTOWT * ( W_ZFQNT / W_MENGE ).
       ZTIDR-ZFPKCNT = ZTBL-ZFPKCN * ( W_ZFQNT / W_MENGE ). "포장수?
    ENDIF.
*>> 수입신고 란 사항 INSERT!
    SELECT *
    INTO   CORRESPONDING FIELDS OF TABLE IT_IVIT
    FROM   ZTCUCLIVIT
    WHERE  ZFIVNO   EQ   W_ZFIVNO.

    LOOP  AT  IT_IVIT.

      SELECT SINGLE * FROM ZTIDRHS
      WHERE  ZFBLNO  EQ ZTCUCL-ZFBLNO
      AND    ZFCLSEQ EQ ZTCUCL-ZFCLSEQ
      AND    STAWN   EQ IT_IVIT-STAWN.

      IF SY-SUBRC NE 0.
         SELECT MAX( ZFCONO ) INTO ZTIDRHS-ZFCONO
         FROM   ZTIDRHS
         WHERE  ZFBLNO  EQ ZTCUCL-ZFBLNO
         AND    ZFCLSEQ EQ ZTCUCL-ZFCLSEQ.
* 수량,단위 SETTING!
         SELECT  SUM( I~MENGE ) MAX( I~MEINS )
         INTO    (ZTIDRHS-ZFQNT, ZTIDRHS-ZFQNTM)
         FROM    ZTCUCLIV AS H INNER JOIN ZTCUCLIVIT AS I
         ON      H~ZFIVNO    EQ   I~ZFIVNO
         WHERE   H~ZFBLNO    EQ   ZTCUCL-ZFBLNO
         AND     H~ZFCLSEQ   EQ   ZTCUCL-ZFCLSEQ
         AND     I~STAWN     EQ   IT_IVIT-STAWN.
*         IF ( ZTIDRHS-ZFQNTM NE 'L'  ) AND
*            ( ZTIDRHS-ZFQNTM NE 'KG' ) AND
*            ( ZTIDRHS-ZFQNTM NE 'G'  ).
*            MOVE 'U'    TO ZTIDRHS-ZFQNTM.
*         ENDIF.
         IF W_MENGE > 0.                              " 란사항 중?
            ZTIDRHS-ZFWET =
                    ZTBL-ZFTOWT * ZTIDRHS-ZFQNT / W_MENGE.
         ELSE.
            ZTIDRHS-ZFWET = 0.
         ENDIF.
         MOVE ZTCUCL-ZFBLNO      TO ZTIDRHS-ZFBLNO.
         MOVE ZTCUCL-ZFCLSEQ     TO ZTIDRHS-ZFCLSEQ.
         ADD  1                  TO ZTIDRHS-ZFCONO.    "?
         MOVE IT_IVIT-STAWN      TO ZTIDRHS-STAWN.     "HS Code

         SELECT MAX( TEXT1 ) INTO ZTIDRHS-ZFTGDNM       "거래품?
         FROM   T604T
         WHERE  SPRAS = SY-LANGU
         AND    STAWN = IT_IVIT-STAWN.

         MOVE ZTIMIMGTX-ZFAPPNM   TO ZTIDRHS-ZFGCNM. "상표품?
         MOVE 'KRW'               TO ZTIDRHS-ZFKRW.
         MOVE 'USD'               TO ZTIDRHS-ZFUSD.
         MOVE ZTBL-ZFTOWTM        TO ZTIDR-ZFTOWTM.  "중량단?

         INSERT ZTIDRHS.
*>> ERROR 발생시 기존 자료 ROLLBACK
         IF SY-SUBRC NE 0.
            UPDATE  ZTCUCLIV
            SET     ZFCUST   =  '1'
                    UNAM     =  SY-UNAME
                    UDAT     =  SY-DATUM
            WHERE   ZFIVNO   =  W_ZFIVNO.

            DELETE  FROM  ZTCUCL
            WHERE   ZFBLNO  =  ZTCUCL-ZFBLNO
            AND     ZFCLSEQ =  ZTCUCL-ZFCLSEQ.

            DELETE  FROM  ZTIDRHS
            WHERE   ZFBLNO  EQ  ZTCUCL-ZFBLNO
            AND     ZFCLSEQ EQ  ZTCUCL-ZFCLSEQ.
            RAISE   ERROR_INSERT.
         ENDIF.
       ENDIF.
*>> 수입신고 규격 TABLE INSERT!
       CLEAR  ZTIDRHSD.
       SELECT MAX( ZFRONO ) INTO ZTIDRHSD-ZFRONO
       FROM   ZTIDRHSD
       WHERE  ZFBLNO  EQ ZTIDRHS-ZFBLNO
       AND    ZFCLSEQ EQ ZTIDRHS-ZFCLSEQ
       AND    ZFCONO  EQ ZTIDRHS-ZFCONO.

       MOVE ZTIDRHS-ZFBLNO      TO ZTIDRHSD-ZFBLNO.
       MOVE ZTIDRHS-ZFCLSEQ     TO ZTIDRHSD-ZFCLSEQ.
       MOVE ZTIDRHS-ZFCONO      TO ZTIDRHSD-ZFCONO.      "?
       ADD  1                   TO ZTIDRHSD-ZFRONO.      "?
       MOVE IT_IVIT-ZFIVNO      TO ZTIDRHSD-ZFIVNO.     "관리번?
       MOVE IT_IVIT-ZFIVDNO     TO ZTIDRHSD-ZFIVDNO.    "일련번?
       MOVE IT_IVIT-TXZ01       TO ZTIDRHSD-ZFGDDS1.     "규격1
       MOVE IT_IVIT-MENGE       TO ZTIDRHSD-ZFQNT.       "수?
       MOVE IT_IVIT-MEINS       TO ZTIDRHSD-ZFQNTM.      "수량단?
       MOVE IT_IVIT-NETPR       TO ZTIDRHSD-NETPR.       "단?
       MOVE IT_IVIT-PEINH       TO ZTIDRHSD-PEINH.       "Price uni
       MOVE IT_IVIT-BPRME       TO ZTIDRHSD-BPRME.       "Order pri
       IF ZTIDRHSD-PEINH > 0.
          ZTIDRHSD-ZFAMT = ZTIDRHSD-NETPR * ZTIDRHSD-ZFQNT
                                          / ZTIDRHSD-PEINH. "금?
          W_STAMT  =  W_STAMT + ZTIDRHSD-ZFAMT.
       ELSE.
          ZTIDRHSD-ZFAMT = 0.
       ENDIF.
       MOVE IT_IVIT-ZFIVAMC    TO ZTIDRHSD-ZFCUR.       "통?
       MOVE IT_IVIT-STAWN      TO ZTIDRHSD-STAWN.       "HS Code
       MOVE IT_IVIT-MATNR       TO ZTIDRHSD-MATNR.
       MOVE IT_IVIT-MATNR       TO W_TEXT12.
*>> 자재코드의 '0' 문자 제거.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
           EXPORTING
                INPUT   = W_TEXT12
           IMPORTING
                OUTPUT  = W_TEXT24.

       MOVE  W_TEXT24    TO  ZTIDRHSD-ZFSTCD.

* 수입의뢰 번호 SELECT!
       SELECT SINGLE *
       FROM   ZTBLIT
       WHERE  ZFBLNO  =  ZTIDRHSD-ZFBLNO
       AND    ZFITMNO =  IT_IVIT-ZFIVDNO.

       IF NOT ( ZTBLIT-ZFREQNO IS INITIAL ).
          SELECT SINGLE MENGE INTO ZTIDRHSD-ZFMENGE
          FROM   ZTREQIT
          WHERE  ZFREQNO  =  ZTBLIT-ZFREQNO
          AND    ZFITMNO  =  ZTBLIT-ZFITMNO.
       ENDIF.
       INSERT ZTIDRHSD.
*>> ERROR 발생시 기존 자료 ROLLBACK
       IF SY-SUBRC NE 0.
          UPDATE  ZTCUCLIV
          SET     ZFCUST   =  '1'
                  UNAM     =  SY-UNAME
                  UDAT     =  SY-DATUM
          WHERE   ZFIVNO   =  W_ZFIVNO.

          DELETE  FROM  ZTCUCL
          WHERE   ZFBLNO  =  ZTCUCL-ZFBLNO
          AND     ZFCLSEQ =  ZTCUCL-ZFCLSEQ.

          DELETE  FROM  ZTIDRHS
          WHERE   ZFBLNO  EQ  ZTCUCL-ZFBLNO
          AND     ZFCLSEQ EQ  ZTCUCL-ZFCLSEQ.

          DELETE  FROM  ZTIDRHSD
          WHERE   ZFBLNO  EQ  ZTCUCL-ZFBLNO
          AND     ZFCLSEQ EQ  ZTCUCL-ZFCLSEQ.
          RAISE   ERROR_INSERT.
       ENDIF.
    ENDLOOP.

    MOVE  W_STAMT  TO  ZTIDR-ZFSTAMT.
    INSERT ZTIDR.
    IF SY-SUBRC NE 0.
       UPDATE  ZTCUCLIV
       SET     ZFCUST   =  '1'
               UNAM     =  SY-UNAME
               UDAT     =  SY-DATUM
       WHERE   ZFIVNO   =  W_ZFIVNO.

       DELETE  FROM  ZTCUCL
       WHERE   ZFBLNO  =  ZTCUCL-ZFBLNO
       AND     ZFCLSEQ =  ZTCUCL-ZFCLSEQ.

       DELETE  FROM  ZTIDRHS
       WHERE   ZFBLNO  EQ  ZTCUCL-ZFBLNO
       AND     ZFCLSEQ EQ  ZTCUCL-ZFCLSEQ.

       DELETE  FROM  ZTIDRHSD
       WHERE   ZFBLNO  EQ  ZTCUCL-ZFBLNO
       AND     ZFCLSEQ EQ  ZTCUCL-ZFCLSEQ.
       RAISE   ERROR_INSERT.
    ENDIF.

ENDFUNCTION.
