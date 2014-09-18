*&---------------------------------------------------------------------
*& Report  ZRIMCOSTLST2
*&---------------------------------------------------------------------
*&  프로그램명 : 수입부대비용 현황
*&      작성자 : 강석봉 INFOLINK Ltd.
*&      작성일 : 2001.10.25
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&
*&---------------------------------------------------------------------
REPORT  ZRIMCOSTLST2 MESSAGE-ID ZIM
                     NO STANDARD PAGE HEADING.

TABLES: T001W,
        ZTREQHD,
        ZTBKPF,
        ZTBSEG,
        ZVIMCOST,
        ZTBL,
        ZTIDS,
        ZTCGHD,
        ZTIMIMG10,
        ZTIMIMG00,
        ZTTAXBKHD,
        LFA1,
        ZTIV,
        ZTCUCLIV,
        T001,
        ZTIMIMG02.

*----------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*----------------------------------------------------------------------
DATA : BEGIN OF IT_COST OCCURS 0,
       ZFCDTY   LIKE   ZTIMIMG08-ZFCDTY,
       ZFCD     LIKE   ZTIMIMG08-ZFCD,
       ZFCDNM   LIKE   ZTIMIMG08-ZFCDNM,
       POSITION TYPE   I,
       END OF IT_COST.

DATA : BEGIN OF IT_BSEG OCCURS 0.
       INCLUDE STRUCTURE ZVIMCOST  .
       DATA : WERKS        LIKE  ZTBL-ZFWERKS,
              NAME1        LIKE  LFA1-NAME1,
              ZFCDNM       LIKE  ZTIMIMG08-ZFCDNM,
              ZFADVPTX(08) TYPE  C,
       END OF IT_BSEG.

DATA : BEGIN OF IT_TAB OCCURS 0,
       WERKS     LIKE    ZTBL-ZFWERKS,
       NAME1     LIKE    T001W-NAME1,
       WRBTR1    LIKE    ZTBSEG-WRBTR,
       WRBTR2    LIKE    ZTBSEG-WRBTR,
       WRBTR3    LIKE    ZTBSEG-WRBTR,
       WRBTR4    LIKE    ZTBSEG-WRBTR,
       WRBTR5    LIKE    ZTBSEG-WRBTR,
       WRBTR6    LIKE    ZTBSEG-WRBTR,
       WRBTR7    LIKE    ZTBSEG-WRBTR,
       WRBTR8    LIKE    ZTBSEG-WRBTR,
       WRBTR9    LIKE    ZTBSEG-WRBTR,
       WRBTR10   LIKE    ZTBSEG-WRBTR,
       WRBTR11   LIKE    ZTBSEG-WRBTR,
       WRBTR12   LIKE    ZTBSEG-WRBTR,
       WRBTR13   LIKE    ZTBSEG-WRBTR,
       WRBTR14   LIKE    ZTBSEG-WRBTR,
       WRBTR15   LIKE    ZTBSEG-WRBTR,
       WRBTR16   LIKE    ZTBSEG-WRBTR,
       WRBTR17   LIKE    ZTBSEG-WRBTR,
       WRBTR18   LIKE    ZTBSEG-WRBTR,
       WRBTR19   LIKE    ZTBSEG-WRBTR,
       WRBTR20   LIKE    ZTBSEG-WRBTR,
       WRBTR21   LIKE    ZTBSEG-WRBTR,
       WRBTR22   LIKE    ZTBSEG-WRBTR,
       WRBTR23   LIKE    ZTBSEG-WRBTR,
       WRBTR24   LIKE    ZTBSEG-WRBTR,
       WRBTR25   LIKE    ZTBSEG-WRBTR,
       WRBTR26   LIKE    ZTBSEG-WRBTR,
       WRBTR27   LIKE    ZTBSEG-WRBTR,
       WRBTR28   LIKE    ZTBSEG-WRBTR,
       WRBTR29   LIKE    ZTBSEG-WRBTR,
       WRBTR30   LIKE    ZTBSEG-WRBTR,
       WRBTR31   LIKE    ZTBSEG-WRBTR,
       WRBTR32   LIKE    ZTBSEG-WRBTR,
       WRBTR33   LIKE    ZTBSEG-WRBTR,
       WRBTR34   LIKE    ZTBSEG-WRBTR,
       WRBTR35   LIKE    ZTBSEG-WRBTR,
       WRBTR36   LIKE    ZTBSEG-WRBTR,
       WRBTR37   LIKE    ZTBSEG-WRBTR,
       WRBTR38   LIKE    ZTBSEG-WRBTR,
       WRBTR39   LIKE    ZTBSEG-WRBTR,
       WRBTR40   LIKE    ZTBSEG-WRBTR,
       WRBTR41   LIKE    ZTBSEG-WRBTR,
       WRBTR42   LIKE    ZTBSEG-WRBTR,
       WRBTR43   LIKE    ZTBSEG-WRBTR,
       WRBTR44   LIKE    ZTBSEG-WRBTR,
       WRBTR45   LIKE    ZTBSEG-WRBTR,
       WRBTR46   LIKE    ZTBSEG-WRBTR,
       WRBTR47   LIKE    ZTBSEG-WRBTR,
       WRBTR48   LIKE    ZTBSEG-WRBTR,
       WRBTR49   LIKE    ZTBSEG-WRBTR,
       WRBTR50   LIKE    ZTBSEG-WRBTR,
       WRBTR_SUM LIKE    ZTBSEG-WRBTR.
DATA : END OF IT_TAB.

DATA :  W_ERR_CHK     TYPE C,
        P_BUKRS       LIKE ZTBL-BUKRS,
        W_LCOUNT      TYPE I,
        W_FIELD_NM    TYPE C,
        W_PAGE        TYPE I,
        W_POS         LIKE SY-TABIX,
        W_WRBTR       LIKE ZTBKPF-WRBTR,
        W_TITLE(50),
        W_DOM_TEX1    LIKE DD07T-DDTEXT,
        W_FNAME       LIKE ZTIMIMG08-ZFCDNM,
        W_ZFCSTGRP    LIKE ZTBSEG-ZFCSTGRP,
        W_ZFCD        LIKE ZTBSEG-ZFCD,
        W_LINE        TYPE I,
        W_COUNT       TYPE I,
        W_NAME(20)    TYPE C,
        W_SUBRC       LIKE SY-SUBRC,
        W_TABIX       LIKE SY-TABIX,
        W_INDEX       LIKE SY-INDEX,
        W_ZFCLSEQ     LIKE ZTIDS-ZFCLSEQ,
        W_FIELD_CNT   TYPE I,
        W_MOD         TYPE I,
        L_POSITION    TYPE I,
        Y             TYPE I,
        LEN           TYPE I,
        W_LIST_INDEX  LIKE SY-TABIX.

FIELD-SYMBOLS: <FIELD>.

INCLUDE   ZRIMSORTCOM.    " REPORT Sort
INCLUDE   ZRIMUTIL01.     " Utility function 모?
INCLUDE   ZRIMOLECOM.     " OLE 공통모듈.

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS     FOR    ZTBKPF-BUKRS NO INTERVALS
                                                   NO-EXTENSION,
                   S_BUPLA     FOR    ZVIMCOST-BUPLA,
                   S_GSBER     FOR    ZVIMCOST-GSBER,
                   S_CNAME     FOR    ZVIMCOST-ZFCNAME,
                   S_BLDAT     FOR    ZVIMCOST-BLDAT,
                   S_BUDAT     FOR    ZVIMCOST-BUDAT,
                   S_MWSKZ     FOR    ZVIMCOST-MWSKZ,
                   S_LIFNR     FOR    ZTBKPF-LIFNR,
                   S_WERKS     FOR    ZTBL-ZFWERKS,
                   S_CSTGRP    FOR    ZTBSEG-ZFCSTGRP,
                   S_ZFCD      FOR    ZTBSEG-ZFCD.
 SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZFCD-LOW.
   PERFORM   P1000_COST_CODE_HELP  USING  S_ZFCD-LOW 'S_ZFCD-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZFCD-HIGH.
   PERFORM   P1000_COST_CODE_HELP  USING  S_ZFCD-HIGH 'S_ZFCD-HIGH'.

*>>
 INITIALIZATION.                          " 초기값 SETTING
 PERFORM   P1000_SET_BUKRS.
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
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
   PERFORM  P3000_DATA_WRITE.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.
   CASE SY-UCOMM.
      WHEN 'REFR'.
* B/L 테이블 SELECT
         PERFORM  P1000_READ_DATA    USING  W_ERR_CHK.
         IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
* 레포트 Write
         PERFORM  P3000_TITLE_WRITE.                  " 해더 출력...
         PERFORM  P3000_DATA_WRITE.
      WHEN 'XLS'.          " EXCEL DOWNLOAD....
          PERFORM P3000_EXCEL_DOWNLOAD.
      WHEN 'FB03'.
         IF SY-LSIND NE 1.
            IF IT_BSEG IS INITIAL.
               MESSAGE S962.
            ELSE.
               IF IT_BSEG-BELNR IS INITIAL.
                  MESSAGE S962.
               ELSE.
                  PERFORM  P2000_FI_DOCUMENT_DISPLAY(SAPMZIM02)
                                USING   IT_BSEG-BUKRS
                                        IT_BSEG-ZFFIYR
                                        IT_BSEG-ZFACDO.
               ENDIF.
            ENDIF.
         ENDIF.

  ENDCASE.
  CLEAR: IT_TAB.

*---------------------------------------------------------------------
* AT LINE-SELECTION.
*----------------------------------------------------------------------
AT LINE-SELECTION.
   IF SY-LSIND NE 1.
      IF IT_BSEG IS INITIAL.
         MESSAGE S962.
      ELSE.
         IF IT_BSEG-BELNR IS INITIAL.
            MESSAGE S962.
         ELSE.
            SET  PARAMETER ID  'BUK'       FIELD   IT_BSEG-BUKRS.
            SET  PARAMETER ID  'GJR'       FIELD   IT_BSEG-GJAHR.
            SET  PARAMETER ID  'ZPBENR'    FIELD   IT_BSEG-BELNR.
            CALL TRANSACTION 'ZIMY3'.
         ENDIF.
      ENDIF.
      CLEAR : IT_TAB, IT_BSEG.
      EXIT.
   ENDIF.

   IF NOT IT_TAB-WERKS IS INITIAL.
      NEW-PAGE LINE-SIZE 154 NO-HEADING NO-TITLE.

      SORT IT_BSEG BY ZFCSTGRP ZFCD.
      CLEAR : W_TABIX, W_INDEX, W_COUNT, W_WRBTR.
      CLEAR : W_ZFCSTGRP, W_ZFCD.
      GET CURSOR FIELD W_NAME.
      IF W_NAME(03) NE 'SY-'.
         IF W_NAME+7(05) EQ 'WRBTR'.
            IF W_NAME+13(03) NE 'SUM'.
               L_POSITION = W_NAME+12(05).
               READ TABLE IT_COST WITH KEY POSITION = L_POSITION.
               IF SY-SUBRC EQ 0.
                  MOVE : IT_COST-ZFCDTY TO W_ZFCSTGRP,
                         IT_COST-ZFCD   TO W_ZFCD.
               ENDIF.
            ENDIF.
         ENDIF.
      ENDIF.

      LOOP AT IT_BSEG.
         W_TABIX = SY-TABIX.

         IF IT_TAB-WERKS NE 'ZZZ'.
            IF IT_BSEG-WERKS NE IT_TAB-WERKS.
               CONTINUE.
            ENDIF.
         ENDIF.

         IF NOT W_ZFCSTGRP IS INITIAL.
            IF IT_BSEG-ZFCSTGRP NE W_ZFCSTGRP.
               CONTINUE.
            ENDIF.
         ENDIF.
         IF NOT W_ZFCD IS INITIAL.
            IF IT_BSEG-ZFCD NE W_ZFCD.
               CONTINUE.
            ENDIF.
         ENDIF.

         ADD 1 TO W_INDEX.

         IF W_INDEX EQ 1.
            SET TITLEBAR  'ZIMR67_1'.
            SET PF-STATUS 'ZIMR67_1'.

            SKIP 2.
            WRITE : / 'Plant :', IT_TAB-WERKS, '-',
                                  IT_TAB-NAME1.
            WRITE : / SY-ULINE.
            FORMAT COLOR COL_HEADING INTENSIFIED OFF.
            WRITE : / SY-VLINE,
                   (10) 'Account Dc',      SY-VLINE,
                   (10) 'Posting DT',      SY-VLINE,
                   (10) 'Document DT',     SY-VLINE,
                   (20) 'Charge Category', SY-VLINE,
                   (33) 'Vendor',          SY-VLINE,
                   (08) 'Pay. MT.',        SY-VLINE,
                   (10) 'Created by',      SY-VLINE,
                   (02) 'Tx',              SY-VLINE,
                   (23) 'Amount',          SY-VLINE.
            WRITE : / SY-ULINE.
         ENDIF.

         SELECT SINGLE * FROM LFA1
                WHERE  LIFNR EQ IT_BSEG-LIFNR.
         MOVE : LFA1-NAME1 TO IT_BSEG-NAME1.

         IF IT_BSEG-ZFADVPT EQ 'X'.
            MOVE: 'Advanced Payment' TO IT_BSEG-ZFADVPTX.
         ELSE.
            MOVE: 'on Credit'        TO IT_BSEG-ZFADVPTX.
         ENDIF.

         IF IT_BSEG-ZFRVSX EQ 'X'.
            MOVE: 'Reverse' TO IT_BSEG-ZFADVPTX.
         ENDIF.

         READ TABLE IT_COST WITH KEY ZFCDTY = IT_BSEG-ZFCSTGRP
                                     ZFCD   = IT_BSEG-ZFCD.

         W_MOD = W_INDEX MOD 2.
         IF W_MOD EQ 1.
            FORMAT COLOR COL_NORMAL INTENSIFIED ON.
         ELSE.
            FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
         ENDIF.

         WRITE : / SY-VLINE,
                   IT_BSEG-ZFACDO,    SY-VLINE,
                   IT_BSEG-BUDAT,     SY-VLINE,
                   IT_BSEG-BLDAT,     SY-VLINE,
              (20) IT_COST-ZFCDNM,    SY-VLINE,
                   IT_BSEG-LIFNR,     SY-VLINE,
              (20) IT_BSEG-NAME1,     SY-VLINE,

                   IT_BSEG-ZFADVPTX CENTERED,  SY-VLINE,
                   IT_BSEG-ZFCNAME,   SY-VLINE,
                   IT_BSEG-MWSKZ,     SY-VLINE,
              (23) IT_BSEG-DMBTR CURRENCY IT_BSEG-HWAER, SY-VLINE.
*                   IT_BSEG-BUPLA,     SY-VLINE.

         HIDE : IT_BSEG.

         ADD IT_BSEG-DMBTR TO W_WRBTR.
         MODIFY IT_BSEG INDEX W_TABIX.
      ENDLOOP.
      IF W_INDEX EQ 0.
         MESSAGE S100(ZIM1).
      ELSE.
         WRITE : / SY-ULINE.
         FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
         WRITE : / SY-VLINE,
               130 W_WRBTR CURRENCY IT_BSEG-HWAER, 154 SY-VLINE.
         WRITE : / SY-ULINE.
      ENDIF.
   ELSE.
      MESSAGE S962.
   ENDIF.
   CLEAR : IT_TAB, IT_BSEG.



*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

DATA : L_LEN TYPE I.

  SKIP 1.
  CLEAR T001.
  SELECT SINGLE *
          FROM T001
          WHERE BUKRS = S_BUKRS-LOW.

  L_LEN = ( LEN / 2 ) - 10.
  CONCATENATE '[' T001-BUTXT 'Import Expense List]' INTO W_TITLE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE:/L_LEN  W_TITLE.
  SKIP 2.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-ULINE.
  WRITE : / SY-VLINE NO-GAP,(35) 'Plant' NO-GAP, SY-VLINE NO-GAP.
  SET LEFT SCROLL-BOUNDARY.

  W_POS =  39.
  Y = SY-LINNO.
  LOOP AT IT_COST.
     SKIP TO LINE Y.    POSITION W_POS.
     WRITE : (19) IT_COST-ZFCDNM NO-GAP, SY-VLINE NO-GAP.
     W_POS = W_POS + 21.
  ENDLOOP.
  SKIP TO LINE Y.    POSITION W_POS.
  WRITE : (19) 'Plant expense Sum'  NO-GAP, SY-VLINE.

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

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BSEG
           FROM   ZVIMCOST
           WHERE  ZFPOSYN  EQ 'Y'
           AND    BUKRS    IN S_BUKRS
           AND    BLDAT    IN S_BLDAT          " 증빙일.
           AND    BUDAT    IN S_BUDAT
           AND    BUKRS    IN S_BUKRS
           AND    LIFNR    IN S_LIFNR
           AND    ZFCSTGRP IN S_CSTGRP
           AND    ZFCD     IN S_ZFCD
           AND    BUPLA    IN S_BUPLA
           AND    GSBER    IN S_GSBER
           AND    ZFCNAME  IN S_CNAME
           AND    MWSKZ    IN S_MWSKZ  .
  IF SY-SUBRC NE 0.
     MESSAGE S738.      W_ERR_CHK = 'Y'.     EXIT.
  ENDIF.

*> 해당 비용 SELECT.
  REFRESH : IT_COST.
  LOOP AT IT_BSEG.
     CLEAR : IT_COST.
     MOVE :  IT_BSEG-ZFCSTGRP    TO   IT_COST-ZFCDTY,
             IT_BSEG-ZFCD        TO   IT_COST-ZFCD.
     COLLECT IT_COST.
  ENDLOOP.

  DESCRIBE  TABLE IT_COST  LINES W_FIELD_CNT.
  IF W_FIELD_CNT EQ 0.
     MESSAGE S966.      W_ERR_CHK = 'Y'.     EXIT.
  ELSEIF W_FIELD_CNT GT 50.
     MESSAGE S103(ZIM1) WITH 50.      W_ERR_CHK = 'Y'.     EXIT.
  ENDIF.

  L_POSITION = 10.

  LOOP AT IT_COST.
     W_TABIX = SY-TABIX.
     SELECT SINGLE ZFCDNM INTO IT_COST-ZFCDNM
            FROM  ZTIMIMG08
            WHERE ZFCDTY   EQ  IT_COST-ZFCDTY
            AND   ZFCD     EQ  IT_COST-ZFCD.
     ADD 1 TO L_POSITION.
     CASE IT_COST-ZFCDTY.
        WHEN '004'.
           CASE IT_COST-ZFCD.
              WHEN 'OBC'. "> OCEAN 선임.
                 MOVE 2          TO IT_COST-POSITION.
              WHEN 'ABC'. "> AIR 항공료.
                 MOVE 3          TO IT_COST-POSITION.
              WHEN OTHERS.
                 MOVE L_POSITION TO IT_COST-POSITION.
             ENDCASE.
        WHEN '006'.
           CASE IT_COST-ZFCD.
              WHEN '001'. "> 관세.
                 MOVE 1 TO IT_COST-POSITION.
              WHEN OTHERS.
                 MOVE L_POSITION TO IT_COST-POSITION.
           ENDCASE.
        WHEN OTHERS.
           MOVE L_POSITION TO IT_COST-POSITION.
     ENDCASE.

     MODIFY IT_COST INDEX W_TABIX.
  ENDLOOP.

  SORT IT_COST BY POSITION.
  CLEAR : L_POSITION.

  LOOP AT IT_COST.
     W_TABIX = SY-TABIX.
     ADD 1 TO L_POSITION.
     MOVE L_POSITION TO IT_COST-POSITION.
     MODIFY IT_COST INDEX W_TABIX.
  ENDLOOP.

  REFRESH : IT_TAB.
  LOOP AT IT_BSEG.
     CLEAR : IT_TAB.
     W_TABIX = SY-TABIX.

     CASE IT_BSEG-ZFCSTGRP.
        WHEN '003'.
           SELECT SINGLE * FROM ZTREQHD
                  WHERE    ZFREQNO EQ IT_BSEG-ZFIMDNO
                  AND      ZFWERKS IN S_WERKS.
           IF SY-SUBRC EQ 0.
              MOVE : ZTREQHD-ZFWERKS TO IT_TAB-WERKS.
           ELSE.
              CONTINUE.
           ENDIF.
        WHEN '004' OR '005'.
           SELECT SINGLE * FROM ZTBL
                  WHERE    ZFBLNO EQ IT_BSEG-ZFIMDNO
                  AND      ZFWERKS IN S_WERKS.

           IF SY-SUBRC EQ 0.
              MOVE : ZTBL-ZFWERKS TO IT_TAB-WERKS.
           ELSE.
              CONTINUE.
           ENDIF.
        WHEN '006'.
           SELECT SINGLE * FROM ZTIV
                  WHERE    ZFIVNO EQ IT_BSEG-ZFIMDNO.
           IF SY-SUBRC EQ 0.
              SELECT SINGLE * FROM ZTBL
                     WHERE ZFBLNO  EQ ZTIV-ZFBLNO
                     AND   ZFWERKS IN S_WERKS.
              IF SY-SUBRC EQ 0.
                 MOVE : ZTBL-ZFWERKS TO IT_TAB-WERKS.
              ELSE.
                 CONTINUE.
              ENDIF.
           ELSE.
              CONTINUE.
           ENDIF.
        WHEN '007'.
           SELECT SINGLE * FROM ZTCGHD
                  WHERE    ZFCGNO EQ IT_BSEG-ZFIMDNO
                  AND      WERKS  IN S_WERKS.

           IF SY-SUBRC EQ 0.
              MOVE : ZTCGHD-WERKS TO IT_TAB-WERKS.
           ELSE.
              CONTINUE.
           ENDIF.
        WHEN '008'.
           SELECT SINGLE * FROM ZTTAXBKHD
                  WHERE    ZFTBNO EQ IT_BSEG-ZFIMDNO.
           IF SY-SUBRC EQ 0 AND
              NOT ZTTAXBKHD-ZFREQNO IS INITIAL.
              SELECT SINGLE * FROM ZTREQHD
                     WHERE ZFREQNO  EQ ZTTAXBKHD-ZFREQNO
                     AND   ZFWERKS IN S_WERKS.
              IF SY-SUBRC EQ 0.
                 MOVE : ZTREQHD-ZFWERKS TO IT_TAB-WERKS.
              ELSE.
                 CONTINUE.
              ENDIF.
           ELSE.
              CONTINUE.
           ENDIF.
     ENDCASE.

     READ TABLE IT_COST WITH KEY ZFCDTY = IT_BSEG-ZFCSTGRP
                                 ZFCD   = IT_BSEG-ZFCD.
     IF SY-SUBRC NE 0.
        CONTINUE.
     ENDIF.

     CLEAR : W_NAME.
     IF IT_COST-POSITION GT 9.
        W_NAME(2) = IT_COST-POSITION.
     ELSE.
        W_NAME(1) = IT_COST-POSITION.
     ENDIF.

     CONCATENATE 'IT_TAB-WRBTR' W_NAME INTO W_NAME.
     ASSIGN:  (W_NAME) TO    <FIELD>.

*> 역기표 처리.
     IF IT_BSEG-SHKZG EQ 'H'.
        IT_BSEG-DMBTR = IT_BSEG-DMBTR * -1.
     ENDIF.

     <FIELD>          = IT_BSEG-DMBTR.
     IT_TAB-WRBTR_SUM = IT_BSEG-DMBTR.

     COLLECT IT_TAB.

     MOVE : IT_TAB-WERKS TO IT_BSEG-WERKS.

*> 소계..
     IT_TAB-WERKS = 'ZZZ'.
     COLLECT IT_TAB.
*>
     MODIFY IT_BSEG INDEX W_TABIX.
  ENDLOOP.

  SORT IT_TAB  BY WERKS.
  SORT IT_BSEG BY WERKS.

  LOOP AT IT_TAB.
     W_TABIX = SY-TABIX.
     IF NOT IT_TAB-WERKS IS INITIAL.
        SELECT SINGLE * FROM T001W
               WHERE WERKS EQ IT_TAB-WERKS.
        IF SY-SUBRC EQ 0.
           MOVE : T001W-NAME1 TO IT_TAB-NAME1.
        ELSE.
           MOVE 'Total'    TO IT_TAB-NAME1.
        ENDIF.
     ENDIF.
     MODIFY IT_TAB INDEX W_TABIX.
  ENDLOOP.

  LEN = W_FIELD_CNT * 21 + 37 + 21.
*  SET
  NEW-PAGE LINE-SIZE LEN NO-HEADING NO-TITLE.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE .

   MOVE 0 TO SY-LSIND.

   SET TITLEBAR  'ZIMR67'.
   SET PF-STATUS 'ZIMR67'.

   CLEAR W_COUNT.

   DESCRIBE TABLE IT_TAB LINES W_COUNT.

   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      PERFORM   P3000_LINE_WRITE.
   ENDLOOP.

   CLEAR: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

   SET TITLEBAR  'ZIMR67'.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------
FORM P3000_LINE_WRITE.

  FORMAT RESET.

  W_MOD = W_TABIX MOD 2.
  IF W_MOD EQ 1.
     FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  IF W_TABIX EQ W_COUNT.
     FORMAT COLOR COL_TOTAL INTENSIFIED ON.
     WRITE : / SY-ULINE.
     WRITE : / SY-VLINE NO-GAP,
              (34) IT_TAB-NAME1 CENTERED, SY-VLINE NO-GAP.
  ELSE.
     WRITE : / SY-VLINE NO-GAP,
              (04) IT_TAB-WERKS,     " P/O NO
                   '-',
              (28) IT_TAB-NAME1   NO-GAP, SY-VLINE NO-GAP.
  ENDIF.

  Y = SY-LINNO.
  W_POS =  39.
  LOOP AT IT_COST.
     CLEAR : W_NAME.
     IF IT_COST-POSITION GT 9.
        W_NAME(2) = IT_COST-POSITION.
     ELSE.
        W_NAME(1) = IT_COST-POSITION.
     ENDIF.

     CONCATENATE 'IT_TAB-WRBTR' W_NAME INTO W_NAME.
     ASSIGN:  (W_NAME) TO    <FIELD>.

     SKIP TO LINE Y.    POSITION W_POS.

     WRITE : (19) <FIELD> CURRENCY 'USD' NO-GAP, SY-VLINE NO-GAP.
     W_POS = W_POS + 21.
  ENDLOOP.

  SKIP TO LINE Y.    POSITION W_POS.
  IF W_MOD EQ 1.
     FORMAT COLOR COL_TOTAL INTENSIFIED ON.
  ELSE.
     FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  ENDIF.

  IF W_TABIX EQ W_COUNT.
     FORMAT COLOR COL_TOTAL INTENSIFIED ON.
     HIDE : IT_TAB.
  ELSE.
     HIDE : IT_TAB.
  ENDIF.

  WRITE : (19) IT_TAB-WRBTR_SUM CURRENCY 'USD' NO-GAP, SY-VLINE.
  FORMAT RESET.

  IF W_TABIX EQ W_COUNT.
     WRITE : SY-ULINE.
  ENDIF.

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

  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LINE_TOTAL
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
FORM P2000_DISP_ZTIDS USING    P_ZFIDRNO.

  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD ''.
  SET PARAMETER ID 'ZPCLSEQ' FIELD ''.
  SET PARAMETER ID 'ZPIDRNO' FIELD P_ZFIDRNO.
  CALL TRANSACTION 'ZIM76' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTIDS
*&---------------------------------------------------------------------*
*&      Form  P3000_EXCEL_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_EXCEL_DOWNLOAD.

DATA : L_COL        TYPE I,
       L_ROW        TYPE I,
       L_WRBTR(20)  TYPE C.

   PERFORM P2000_EXCEL_INITIAL  USING  '굴림체'
                                        10.

   PERFORM P2000_FIT_CELL       USING 1 04 'C'.  "COLUMN조정.
*   PERFORM P2000_FILL_CELL_FONT USING 1 1 'B' 10.
   PERFORM P2000_FILL_CELL      USING 1 1 'Plnt'.
   PERFORM P2000_FIT_CELL       USING 2 30 'C'.  "COLUMN조정.
*   PERFORM P2000_FILL_CELL_FONT USING 2 1 'B' 10.
   PERFORM P2000_FILL_CELL      USING 1 2 '플랜트명'.

   DO W_FIELD_CNT TIMES.
      L_COL = SY-INDEX + 2.
      PERFORM P2000_FIT_CELL       USING L_COL 20 'C'.  "COLUMN조정.
*      PERFORM P2000_FILL_CELL_FONT USING 1 L_COL 'B' 10.
      READ TABLE IT_COST WITH KEY POSITION = SY-INDEX.
      IF SY-SUBRC EQ 0.
         PERFORM P2000_FILL_CELL      USING 1 L_COL IT_COST-ZFCDNM.
      ENDIF.
   ENDDO.
   ADD 1 TO L_COL.
   PERFORM P2000_FIT_CELL       USING L_COL 20 'C'.  "COLUMN조정.
   PERFORM P2000_FILL_CELL      USING 1 L_COL '플랜트 계'.

   LOOP AT IT_TAB.
      L_ROW = SY-TABIX + 1.

      PERFORM P2000_FILL_CELL      USING L_ROW 1 IT_TAB-WERKS.
      PERFORM P2000_FILL_CELL      USING L_ROW 2 IT_TAB-NAME1.

      LOOP AT IT_COST.
         CLEAR : W_NAME.
         IF IT_COST-POSITION GT 9.
            W_NAME(2) = IT_COST-POSITION.
         ELSE.
            W_NAME(1) = IT_COST-POSITION.
         ENDIF.

         CONCATENATE 'IT_TAB-WRBTR' W_NAME INTO W_NAME.
         ASSIGN:  (W_NAME) TO    <FIELD>.

         WRITE : <FIELD> CURRENCY 'KRW' TO L_WRBTR.
         L_COL = IT_COST-POSITION + 2.
         PERFORM P2000_FILL_CELL      USING L_ROW L_COL L_WRBTR.
      ENDLOOP.

      ADD 1 TO L_COL.
      WRITE : IT_TAB-WRBTR_SUM CURRENCY 'KRW' TO L_WRBTR.
      PERFORM P2000_FILL_CELL      USING L_ROW L_COL L_WRBTR.

      AT LAST.
         PERFORM P2000_FILL_CELL      USING L_ROW 1 SPACE.
      ENDAT.
   ENDLOOP.

*   PERFORM P2000_FILL_CELL_FONT USING 3 3 'B' 18.
*   PERFORM P2000_FILL_CELL_MERGE USING 3 3 3 4 'TEXT'.
*   PERFORM P2000_FILL_LINE  USING 1 1 1 L_COL  9.
*   PERFORM P2000_FILL_LINE  USING 2 L_ROW  1 L_COL  3.
*   PERFORM P2000_CONCATNATE_CELL USING IT_DOWN_G1-CELL132 W_SHIP_CNT.

    SET PROPERTY OF EXCEL 'VISIBLE' = 1.

*   PERFORM P2000_SAVEAS_EXCEL   USING 'C:\TEST.XLS'.

ENDFORM.                    " P3000_EXCEL_DOWNLOAD

*&---------------------------------------------------------------------*
*&      Form  P1000_COST_CODE_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_ZFCD_LOW  text
*      -->P_0491   text
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
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

   CLEAR : ZTIMIMG00, P_BUKRS.
   SELECT SINGLE * FROM ZTIMIMG00.
   IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
   ENDIF.

*>> Company code SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
