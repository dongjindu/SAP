FUNCTION ZIM_IMPORT_COST_DIVISION .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFIVNO) LIKE  ZTIV-ZFIVNO
*"  EXCEPTIONS
*"      DIV_ERROR
*"----------------------------------------------------------------------
*--------------------------------------------------------------
* 수입부대비용(제비용) 배부용 FUNCTION
*   - 2001/03/03   INFOLINK LTD.  KANG SEUG BONG
*--------------------------------------------------------------
DATA : L_TABIX    LIKE  SY-TABIX,
       L_DWBTR    LIKE  ZTRECST,
       W_ZFOPAMT  LIKE  ZTBL-ZFBLAMT,
       W_ZFBLAMT  LIKE  ZTBL-ZFBLAMT,
       W_ZFCGAMT  LIKE  ZTBL-ZFBLAMT,
       W_ZFCCAMT  LIKE  ZTBL-ZFBLAMT,
       W_ZFCCAMTH LIKE  ZTBL-ZFBLAMT,
       W_TMPAMT1  LIKE  ZTBL-ZFBLAMT,
       W_TMPAMT2  LIKE  ZTBL-ZFBLAMT,
       W_TMPAMT3  LIKE  ZTBL-ZFBLAMT,
       W_ZFPCST   LIKE  ZTIV-ZFPCST,
       W_ZFUPCST  LIKE  ZTIV-ZFUPCST,
       L_ZFUPCST  LIKE  ZTIV-ZFUPCST,
       W_ZFDAMT   LIKE  ZTIV-ZFDAMT,   ">관세.
       W_TEXT70(70).


DATA : BEGIN OF IT_REQNO OCCURS 0,
       ZFREQNO    LIKE  ZTREQHD-ZFREQNO,
       ZFCSQ      LIKE  ZTRECST-ZFCSQ,
       ZFOPAMT    LIKE  ZTREQHD-ZFOPAMT.
DATA : END OF IT_REQNO.


DATA : BEGIN OF IT_BLNO OCCURS 0,
       ZFBLNO     LIKE  ZTBL-ZFBLNO,
       ZFCSQ      LIKE  ZTRECST-ZFCSQ,
       ZFBLAMT    LIKE  ZTBL-ZFBLAMT.
DATA : END OF IT_BLNO.

DATA : BEGIN OF IT_CGNO OCCURS 0,
       ZFCGNO     LIKE  ZTCGHD-ZFCGNO,
       ZFCSQ      LIKE  ZTCGCST-ZFCSQ,
       ZFCGAMT    LIKE  ZTBL-ZFBLAMT.
DATA : END OF IT_CGNO.

DATA : BEGIN OF IT_CCNO OCCURS 0,
       ZFBLNO     LIKE  ZTCUCLCST-ZFBLNO,
       ZFCLSEQ    LIKE  ZTCUCLCST-ZFCLSEQ,
       ZFCSQ      LIKE  ZTCUCLCST-ZFCSQ,
       ZFCCAMT    LIKE  ZTBL-ZFBLAMT.
DATA : END OF IT_CCNO.

*--------------------------------------------------------------
*>> INTERNAL TABLE CLEARING.
   CLEAR : IT_ZTRECST, IT_ZTBLCST, IT_ZTCGCST,
           IT_ZTCUCLCST.

   REFRESH : IT_ZTRECST,   IT_ZTBLCST, IT_ZTCGCST,
             IT_ZTCUCLCST, IT_ZSIVIT,
             IT_ZTIMIMG08, IT_ZTIMIMG04,
             IT_ZTIVCD,    IT_ZTIVCD_OLD.

*--------------------------------------------------------------
*>>> INPUT KEY VALUE CHECK.
   IF ZFIVNO IS INITIAL.
      MESSAGE E412 RAISING DIV_ERROR.
   ENDIF.

*>> 통관요청 READ CHECK.
   SELECT SINGLE * FROM ZTIV
          WHERE ZFIVNO  EQ  ZFIVNO.
   IF SY-SUBRC NE 0.
      MESSAGE E413 WITH ZFIVNO  RAISING DIV_ERROR.
   ENDIF.

*>>> 통관상태 체크.
  CASE ZTIV-ZFCUST.
     WHEN '1' OR '2' OR '3' OR 'Y' OR 'X'.
        PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                  USING      'ZDCUST'  ZTIV-ZFCUST
                  CHANGING    W_TEXT70.
        MESSAGE E419 WITH ZFIVNO W_TEXT70 '비용배부'
                           RAISING DIV_ERROR.
     WHEN 'N'.
  ENDCASE.

*>>> 비용배부 상태 체크.
  CASE ZTIV-ZFCDST.
     WHEN 'X'.     ">배부불가..(오류)
        PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                  USING      'ZDCDST'  ZTIV-ZFCDST
                  CHANGING    W_TEXT70.
        MESSAGE E420 WITH ZFIVNO W_TEXT70 '비용배부'
                           RAISING DIV_ERROR.
     WHEN 'Y'.     ">배부완료. (MESSAGE)
     WHEN 'N'.     ">배부대상. (NONE)
     WHEN OTHERS.  ">상태 오류.(오류)
        MESSAGE E420 WITH ZFIVNO '미입력상태' '비용배부'
                           RAISING DIV_ERROR.
   ENDCASE.
*>>> 입고 상태 체크.
   CASE ZTIV-ZFGRST.
      WHEN 'Y'.     ">입고완료.(오류)
         PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                   USING      'ZDGRST'  ZTIV-ZFGRST
                   CHANGING    W_TEXT70.
         MESSAGE E422 WITH ZFIVNO W_TEXT70 '비용배부'
                           RAISING DIV_ERROR.
      WHEN 'N'.     ">입고대상.(NONE)
      WHEN 'X'.     ">입고불가.(NONE)
   ENDCASE.
*>>> 제비용 상태 체크.
   CASE ZTIV-ZFCIVST.
      WHEN 'Y' OR 'X'.     ">처리완료.(오류) OR 불가상태(오류).
         PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                   USING      'ZDCIVST'  ZTIV-ZFCIVST
                   CHANGING    W_TEXT70.

         MESSAGE E423 WITH ZFIVNO W_TEXT70 '비용배부'
                           RAISING DIV_ERROR.
      WHEN 'N'.     ">제비용처리대상.(NONE)
   ENDCASE.

**>> 통관용 INVOICE.
*   SELECT SINGLE * FROM ZTCUCLIV
*          WHERE ZFIVNO  EQ  ZFIVNO.
*
*   SELECT * INTO TABLE IT_ZTCUCLIVIT
*          FROM ZTCUCLIVIT
*          WHERE ZFIVNO  EQ  ZFIVNO.
**--------------------------------------------------------------
*   CLEAR : W_ZFCCAMTH.
*
*   LOOP AT IT_ZTCUCLIVIT.
*      W_ZFCCAMTH  =  W_ZFCCAMTH +
*               ( IT_ZTCUCLIVIT-MENGE *
*               ( IT_ZTCUCLIVIT-NETPR / IT_ZTCUCLIVIT-PEINH ) ).
*   ENDLOOP.
*
**-----------------------------------------------------------
*>> 통관요청 자재 READ CHECK.
   SELECT * INTO TABLE IT_ZSIVIT
            FROM  ZTIVIT
            WHERE ZFIVNO EQ  ZFIVNO.
   IF SY-SUBRC NE 0.
      MESSAGE E417 WITH ZFIVNO  RAISING DIV_ERROR.
   ENDIF.

*--------------------------------------------------------------
*>> 통관요청 자재 SORT(수입의뢰, B/L, 하역, 통관).
   SORT IT_ZSIVIT BY ZFREQNO ZFITMNO
                     ZFBLNO  ZFBLIT
                     ZFCGNO  ZFCGIT
                     ZFIVNO  ZFIVDNO.
*--------------------------------------------------------------

*-----< 자재별 총금액 집계 >------------------------------
   LOOP AT IT_ZSIVIT.
      W_TABIX = SY-TABIX.

*>>>>> 수입의뢰 HEADER
*      SELECT SINGLE * FROM ZTREQHD
*             WHERE    ZFREQNO EQ IT_ZSIVIT-ZFREQNO.
      CLEAR : W_ZFOPAMT.
      IF NOT IT_ZSIVIT-ZFREQNO IS INITIAL.
         SELECT * INTO TABLE IT_ZTREQIT
                  FROM ZTREQIT
                  WHERE ZFREQNO EQ IT_ZSIVIT-ZFREQNO.

         LOOP AT IT_ZTREQIT.
            W_ZFOPAMT  =  W_ZFOPAMT +
                   ( IT_ZTREQIT-MENGE *
                   ( IT_ZTREQIT-NETPR / IT_ZTREQIT-PEINH ) ).
         ENDLOOP.
      ENDIF.

*>>>>> B/L NO CHECK.
      CLEAR : W_ZFBLAMT.
      IF NOT IT_ZSIVIT-ZFBLNO IS INITIAL.
*>>>>>-----> B/L ITEM SELECT
         SELECT * INTO TABLE IT_ZTBLIT
                FROM ZTBLIT
                WHERE    ZFBLNO EQ IT_ZSIVIT-ZFBLNO
                AND      BLOEKZ NE 'X'.

         W_ZFBLAMT = 0.
         LOOP AT IT_ZTBLIT.
            W_ZFBLAMT  =  W_ZFBLAMT +
                   ( IT_ZTBLIT-BLMENGE *
                   ( IT_ZTBLIT-NETPR / IT_ZTBLIT-PEINH ) ).
         ENDLOOP.
      ENDIF.

*--------------------------------------------------------------
      CLEAR : W_ZFCGAMT.
      IF NOT IT_ZSIVIT-ZFCGNO IS INITIAL.
*>>>>>-----> 하역 ITEM SELECT.
         SELECT * INTO TABLE IT_ZTCGIT
                FROM ZTCGIT
                WHERE    ZFCGNO  EQ IT_ZSIVIT-ZFCGNO
                AND      CGLOEKZ NE 'X'.

         W_ZFCGAMT = 0.
         LOOP AT IT_ZTCGIT.
            W_ZFCGAMT  =  W_ZFCGAMT +
                    ( IT_ZTCGIT-CGMENGE *
                    ( IT_ZTCGIT-NETPR / IT_ZTCGIT-PEINH ) ).
         ENDLOOP.
      ENDIF.

*--------------------------------------------------------------
*>> FIELD MOVE...
*>>>  통관비용(자재별).
      W_ZFCCAMT =  IT_ZSIVIT-CCMENGE *
                   ( IT_ZSIVIT-NETPR / IT_ZSIVIT-PEINH ).

      MOVE : W_ZFOPAMT         TO  IT_ZSIVIT-ZFOPAMT,
             W_ZFBLAMT         TO  IT_ZSIVIT-ZFBLAMT,
             W_ZFCGAMT         TO  IT_ZSIVIT-ZFCGAMT,
             W_ZFCCAMT         TO  IT_ZSIVIT-ZFCCAMT,
             W_ZFCCAMTH        TO  IT_ZSIVIT-ZFCCAMTH.

      MODIFY IT_ZSIVIT  INDEX  W_TABIX.
   ENDLOOP.
*-----------------------------------------------------------

*>> 비용코드 SELECT.
   SELECT * INTO TABLE IT_ZTIMIMG08
            FROM   ZTIMIMG08
            WHERE  ZFCDTY  IN ('003', '004', '005', '006', '007').

*>> Planned Cost 코드 SELECT.
   SELECT * INTO TABLE IT_ZTIMIMG04
            FROM   ZTIMIMG04
            WHERE  ZFAPLDT <= SY-DATUM.
*--------------------------------------------------------------

*--------------------------------------------------------------
*>> 수입의뢰 비용.
   SELECT * INTO TABLE IT_ZTRECST
             FROM  ZTRECST
             FOR ALL ENTRIES IN IT_ZSIVIT
             WHERE ZFREQNO  EQ  IT_ZSIVIT-ZFREQNO
             AND   ZFCKAMT  NE  0.

*>> B/L 비용.
   SELECT * INTO TABLE IT_ZTBLCST
             FROM  ZTBLCST
             FOR ALL ENTRIES IN IT_ZSIVIT
             WHERE ZFBLNO   EQ  IT_ZSIVIT-ZFBLNO
             AND   ZFCKAMT  NE  0.

*>> 하역 비용.
   SELECT * INTO TABLE IT_ZTCGCST
             FROM  ZTCGCST
             FOR ALL ENTRIES IN IT_ZSIVIT
             WHERE ZFCGNO   EQ  IT_ZSIVIT-ZFCGNO
             AND   ZFCKAMT  NE  0.

**>> 통관 비용.
*   SELECT * INTO TABLE IT_ZTCUCLCST
*             FROM  ZTCUCLCST
*             WHERE ZFBLNO  EQ  ZTCUCLIV-ZFBLNO
*             AND   ZFCLSEQ EQ  ZTCUCLIV-ZFCLSEQ
*             AND   ZFCAMT  NE  0.

*--------------------------------------------------------------

*--------------------------------------------------------------
*>> 통관요청 자재 SORT(수입의뢰, B/L, 하역, 통관).
   SORT IT_ZSIVIT BY ZFREQNO ZFITMNO
                     ZFBLNO  ZFBLIT
                     ZFCGNO  ZFCGIT
                     ZFIVNO  ZFIVDNO.
*--------------------------------------------------------------


*--------------------------------------------------------------
*>> Planned Cost Calculation.
* DESC : 아이템별 Planned Cost 계산.
*--------------------------------------------------------------
   CLEAR : W_ZFPCST.
   LOOP AT IT_ZSIVIT.
      W_TABIX = SY-TABIX.
      READ TABLE IT_ZTIMIMG04
                 WITH KEY ZFWERKS =  IT_ZSIVIT-WERKS
                          ZFMATGB =  IT_ZSIVIT-ZFMATGB.
      IF SY-SUBRC EQ 0.
         IT_ZSIVIT-ZFPLRTE = IT_ZTIMIMG04-ZFPLRTE.
         IT_ZSIVIT-ZFPCST  =
                    ( IT_ZSIVIT-ZFIVAMK * IT_ZTIMIMG04-ZFPLRTE )
                        / 100.
         W_ZFPCST = W_ZFPCST + IT_ZSIVIT-ZFPCST.
         MODIFY IT_ZSIVIT INDEX W_TABIX.
      ELSE.
         MESSAGE E418 WITH IT_ZSIVIT-WERKS IT_ZSIVIT-ZFMATGB
                           RAISING DIV_ERROR.
      ENDIF.
   ENDLOOP.

*--------------------------------------------------------------
*>> 수입의뢰 비용배부.
*--------------------------------------------------------------
   CLEAR : W_TMPAMT3.
   REFRESH : IT_REQNO.
   LOOP AT IT_ZTRECST WHERE ZFCKAMT NE 0.
      W_TABIX = SY-TABIX.

*>> 통관요청 자재별..
      L_ZFUPCST = 0.
      LOOP AT IT_ZSIVIT WHERE ZFREQNO EQ IT_ZTRECST-ZFREQNO.
         CLEAR : IT_ZTIVCD.

*>>>>> Planned Cost 여부.
         READ TABLE IT_ZTIMIMG08
                    WITH KEY ZFCDTY = '003'
                             ZFCD   = IT_ZTRECST-ZFCSCD.
         IF SY-SUBRC NE 0.
            MESSAGE E909 WITH IT_ZTRECST-ZFCSCD  RAISING DIV_ERROR.
         ENDIF.

*>> Planned Cost Rate.
         IF IT_ZTIMIMG08-ZFCD1 EQ 'Y'.       ">Planned Cost
            CONTINUE.
*>> UnPlanned Cost.
         ELSEIF IT_ZTIMIMG08-ZFCD1 EQ 'N'.   ">UnPlan Cost
            CLEAR : IT_ZTIVCD-ZFPLRTE, IT_ZTIVCD-ZFPCST.
            IT_ZTIVCD-ZFUPCST =
                         ( IT_ZSIVIT-ZFCCAMT / IT_ZSIVIT-ZFOPAMT )
                         * IT_ZTRECST-ZFCKAMT.

*>> 통관요청비용배부 테이블.
            CLEAR : W_TMPAMT1.
            SELECT SUM( ZFUPCST ) INTO (W_TMPAMT1)
                   FROM  ZTIVCD
                   WHERE ZFIVNO   NE IT_ZSIVIT-ZFIVNO
                   AND   ZFIVDNO  NE IT_ZSIVIT-ZFIVDNO
                   AND   ZFIMDTY  EQ 'RD'
                   AND   ZFIMDNO  EQ IT_ZTRECST-ZFREQNO
                   AND   ZFCSQ    EQ IT_ZTRECST-ZFCSQ.

            CLEAR : IT_REQNO.
            READ TABLE IT_REQNO  WITH KEY ZFREQNO = IT_ZTRECST-ZFREQNO
                                          ZFCSQ   = IT_ZTRECST-ZFCSQ.

*>>>> 기존 배부 금액 + 현재 배부금액 + 같은 문서의 배부금액.
            IF SY-SUBRC EQ 0.
               W_TMPAMT2 = W_TMPAMT1 +
                           IT_ZTIVCD-ZFUPCST + IT_REQNO-ZFOPAMT.
            ELSE.
               W_TMPAMT2 = W_TMPAMT1 + IT_ZTIVCD-ZFUPCST.
            ENDIF.

            IF W_TMPAMT2 GT IT_ZTRECST-ZFCKAMT.
               IT_ZTIVCD-ZFUPCST = IT_ZTRECST-ZFCKAMT -  W_TMPAMT1
                                 - IT_REQNO-ZFOPAMT.
               IF IT_ZTIVCD-ZFUPCST  LT  0.
                  IT_ZTIVCD-ZFUPCST = 0.
               ENDIF.
            ENDIF.
         ENDIF.

         MOVE : IT_ZSIVIT-ZFIVNO       TO IT_ZTIVCD-ZFIVNO,
                IT_ZSIVIT-ZFIVDNO      TO IT_ZTIVCD-ZFIVDNO,
                'RD'                   TO IT_ZTIVCD-ZFIMDTY,
                IT_ZSIVIT-ZFREQNO      TO IT_ZTIVCD-ZFIMDNO,
                SPACE                  TO IT_ZTIVCD-ZFCLSEQ,
                IT_ZTRECST-ZFCSQ       TO IT_ZTIVCD-ZFCSQ,
                IT_ZTRECST-ZFCSCD      TO IT_ZTIVCD-ZFCSCD,
                IT_ZTRECST-ZFCKAMT     TO IT_ZTIVCD-ZFCKAMT,
                'KRW'                  TO IT_ZTIVCD-ZFKRW,
                IT_ZTRECST-ZFVAT       TO IT_ZTIVCD-ZFVAT,
                SY-UNAME               TO IT_ZTIVCD-ERNAM,
                SY-DATUM               TO IT_ZTIVCD-CDAT,
                SY-UNAME               TO IT_ZTIVCD-UNAM,
                SY-DATUM               TO IT_ZTIVCD-UDAT,
                SY-MANDT               TO IT_ZTIVCD-MANDT.

         MOVE : IT_ZTRECST-ZFREQNO  TO  IT_REQNO-ZFREQNO,
                IT_ZTRECST-ZFCSQ    TO  IT_REQNO-ZFCSQ,
                IT_ZTIVCD-ZFUPCST   TO  IT_REQNO-ZFOPAMT.
         COLLECT IT_REQNO.

*>> 비용 TABLE APPEND.
         APPEND IT_ZTIVCD.
         ADD  IT_ZTIVCD-ZFUPCST    TO L_ZFUPCST.
      ENDLOOP.

*>> INTERNAL TABLE MODIFY.
      IT_ZTRECST-ZFUPCST = IT_ZTRECST-ZFUPCST + L_ZFUPCST.
      MODIFY  IT_ZTRECST  INDEX  W_TABIX.
   ENDLOOP.
*--------------------------------------------------------------


*--------------------------------------------------------------
*>> B/L 비용배부.
*--------------------------------------------------------------
   REFRESH : IT_BLNO.
   LOOP AT IT_ZTBLCST WHERE ZFCKAMT NE 0.
      W_TABIX = SY-TABIX.

*>> 통관요청 자재별..
      L_ZFUPCST = 0.
      LOOP AT IT_ZSIVIT WHERE ZFBLNO EQ IT_ZTBLCST-ZFBLNO.
         CLEAR : IT_ZTIVCD.

*>>>>> Planned Cost 여부.
         IF IT_ZTBLCST-ZFCSQ GT '10000'.   ">해외운임.
            READ TABLE IT_ZTIMIMG08
                       WITH KEY ZFCDTY = '004'
                                ZFCD   = IT_ZTBLCST-ZFCSCD.
         ELSE.                             ">보세운임.
            READ TABLE IT_ZTIMIMG08
                       WITH KEY ZFCDTY = '005'
                                ZFCD   = IT_ZTBLCST-ZFCSCD.
         ENDIF.

         IF SY-SUBRC NE 0.
            MESSAGE E909 WITH IT_ZTBLCST-ZFCSCD  RAISING DIV_ERROR.
         ENDIF.

*>> Planned Cost Rate.
         IF IT_ZTIMIMG08-ZFCD1 EQ 'Y'.       ">Planned Cost
            CONTINUE.
*>> UnPlanned Cost.
         ELSEIF IT_ZTIMIMG08-ZFCD1 EQ 'N'.   ">UnPlan Cost
            CLEAR : IT_ZTIVCD-ZFPLRTE, IT_ZTIVCD-ZFPCST.
            IT_ZTIVCD-ZFUPCST =
                         ( IT_ZSIVIT-ZFCCAMT / IT_ZSIVIT-ZFBLAMT )
                         * IT_ZTBLCST-ZFCKAMT.

*>> 통관요청비용배부 테이블.
            CLEAR : W_TMPAMT1.
            SELECT SUM( ZFUPCST ) INTO (W_TMPAMT1)
                   FROM  ZTIVCD
                   WHERE ZFIVNO   NE IT_ZSIVIT-ZFIVNO
                   AND   ZFIVDNO  NE IT_ZSIVIT-ZFIVDNO
                   AND   ZFIMDTY  EQ 'BL'
                   AND   ZFIMDNO  EQ IT_ZTBLCST-ZFBLNO
                   AND   ZFCSQ    EQ IT_ZTBLCST-ZFCSQ.

            CLEAR : IT_BLNO.
            READ TABLE IT_BLNO  WITH KEY ZFBLNO = IT_ZTBLCST-ZFBLNO
                                         ZFCSQ  = IT_ZTBLCST-ZFCSQ.

*>>>> 기존 배부 금액 + 현재 배부금액 + 같은 문서의 배부금액.
            IF SY-SUBRC EQ 0.
               W_TMPAMT2 = W_TMPAMT1 +
                          IT_ZTIVCD-ZFUPCST + IT_BLNO-ZFBLAMT.
            ELSE.
               W_TMPAMT2 = W_TMPAMT1 + IT_ZTIVCD-ZFUPCST.
            ENDIF.

            IF W_TMPAMT2 GT IT_ZTRECST-ZFCKAMT.
               IT_ZTIVCD-ZFUPCST = IT_ZTBLCST-ZFCKAMT -  W_TMPAMT1
                                 - IT_BLNO-ZFBLAMT.
               IF IT_ZTIVCD-ZFUPCST  LT  0.
                  IT_ZTIVCD-ZFUPCST = 0.
               ENDIF.
            ENDIF.
         ENDIF.

         MOVE : IT_ZSIVIT-ZFIVNO       TO IT_ZTIVCD-ZFIVNO,
                IT_ZSIVIT-ZFIVDNO      TO IT_ZTIVCD-ZFIVDNO,
                'BL'                   TO IT_ZTIVCD-ZFIMDTY,
                IT_ZSIVIT-ZFBLNO       TO IT_ZTIVCD-ZFIMDNO,
                SPACE                  TO IT_ZTIVCD-ZFCLSEQ,
                IT_ZTBLCST-ZFCSQ       TO IT_ZTIVCD-ZFCSQ,
                IT_ZTBLCST-ZFCSCD      TO IT_ZTIVCD-ZFCSCD,
                IT_ZTBLCST-ZFCKAMT     TO IT_ZTIVCD-ZFCKAMT,
                'KRW'                  TO IT_ZTIVCD-ZFKRW,
                IT_ZTBLCST-ZFVAT       TO IT_ZTIVCD-ZFVAT,
                SY-UNAME               TO IT_ZTIVCD-ERNAM,
                SY-DATUM               TO IT_ZTIVCD-CDAT,
                SY-UNAME               TO IT_ZTIVCD-UNAM,
                SY-DATUM               TO IT_ZTIVCD-UDAT,
                SY-MANDT               TO IT_ZTIVCD-MANDT.

         MOVE : IT_ZTBLCST-ZFBLNO   TO  IT_BLNO-ZFBLNO,
                IT_ZTBLCST-ZFCSQ    TO  IT_BLNO-ZFCSQ,
                IT_ZTIVCD-ZFUPCST   TO  IT_BLNO-ZFBLAMT.
         COLLECT IT_BLNO.

*>> 비용 TABLE APPEND.
         APPEND IT_ZTIVCD.
         ADD  IT_ZTIVCD-ZFUPCST    TO L_ZFUPCST.
      ENDLOOP.

*>> INTERNAL TABLE MODIFY.
      IT_ZTBLCST-ZFUPCST = IT_ZTBLCST-ZFUPCST + L_ZFUPCST.
      MODIFY  IT_ZTBLCST  INDEX  W_TABIX.
   ENDLOOP.
*--------------------------------------------------------------

*--------------------------------------------------------------
*>> 하역 비용배부.
*--------------------------------------------------------------
   REFRESH : IT_CGNO.
   LOOP AT IT_ZTCGCST WHERE ZFCKAMT NE 0.
      W_TABIX = SY-TABIX.

*>> 통관요청 자재별..
      L_ZFUPCST = 0.
      LOOP AT IT_ZSIVIT WHERE ZFCGNO EQ IT_ZTCGCST-ZFCGNO.
         CLEAR : IT_ZTIVCD.

*>>>>> Planned Cost 여부.
         READ TABLE IT_ZTIMIMG08
                    WITH KEY ZFCDTY = '007'
                             ZFCD   = IT_ZTCGCST-ZFCSCD.

         IF SY-SUBRC NE 0.
            MESSAGE E909 WITH IT_ZTCGCST-ZFCSCD  RAISING DIV_ERROR.
         ENDIF.

*>> Planned Cost Rate.
         IF IT_ZTIMIMG08-ZFCD1 EQ 'Y'.       ">Planned Cost
            CONTINUE.
*>> UnPlanned Cost.
         ELSEIF IT_ZTIMIMG08-ZFCD1 EQ 'N'.   ">UnPlan Cost
            CLEAR : IT_ZTIVCD-ZFPLRTE, IT_ZTIVCD-ZFPCST.
            IT_ZTIVCD-ZFUPCST =
                         ( IT_ZSIVIT-ZFCCAMT / IT_ZSIVIT-ZFCGAMT )
                         * IT_ZTCGCST-ZFCKAMT.

*>> 통관요청비용배부 테이블.
            CLEAR : W_TMPAMT1.
            SELECT SUM( ZFUPCST ) INTO (W_TMPAMT1)
                   FROM  ZTIVCD
                   WHERE ZFIVNO   NE IT_ZSIVIT-ZFIVNO
                   AND   ZFIVDNO  NE IT_ZSIVIT-ZFIVDNO
                   AND   ZFIMDTY  EQ 'CW'
                   AND   ZFIMDNO  EQ IT_ZTCGCST-ZFCGNO
                   AND   ZFCSQ    EQ IT_ZTCGCST-ZFCSQ.

            CLEAR : IT_CGNO.
            READ TABLE IT_CGNO  WITH KEY ZFCGNO = IT_ZTCGCST-ZFCGNO
                                         ZFCSQ  = IT_ZTCGCST-ZFCSQ.

*>>>> 기존 배부 금액 + 현재 배부금액 + 같은 문서의 배부금액.
            IF SY-SUBRC EQ 0.
               W_TMPAMT2 = W_TMPAMT1 +
                          IT_ZTIVCD-ZFUPCST + IT_CGNO-ZFCGAMT.
            ELSE.
               W_TMPAMT2 = W_TMPAMT1 + IT_ZTIVCD-ZFUPCST.
            ENDIF.

            IF W_TMPAMT2 GT IT_ZTCGCST-ZFCKAMT.
               IT_ZTIVCD-ZFUPCST = IT_ZTCGCST-ZFCKAMT -  W_TMPAMT1
                                 - IT_CGNO-ZFCGAMT.
               IF IT_ZTIVCD-ZFUPCST  LT  0.
                  IT_ZTIVCD-ZFUPCST = 0.
               ENDIF.
            ENDIF.
         ENDIF.

         MOVE : IT_ZSIVIT-ZFIVNO       TO IT_ZTIVCD-ZFIVNO,
                IT_ZSIVIT-ZFIVDNO      TO IT_ZTIVCD-ZFIVDNO,
                'CW'                   TO IT_ZTIVCD-ZFIMDTY,
                IT_ZSIVIT-ZFCGNO       TO IT_ZTIVCD-ZFIMDNO,
                SPACE                  TO IT_ZTIVCD-ZFCLSEQ,
                IT_ZTCGCST-ZFCSQ       TO IT_ZTIVCD-ZFCSQ,
                IT_ZTCGCST-ZFCSCD      TO IT_ZTIVCD-ZFCSCD,
                IT_ZTCGCST-ZFCKAMT     TO IT_ZTIVCD-ZFCKAMT,
                'KRW'                  TO IT_ZTIVCD-ZFKRW,
                IT_ZTCGCST-ZFVAT       TO IT_ZTIVCD-ZFVAT,
                SY-UNAME               TO IT_ZTIVCD-ERNAM,
                SY-DATUM               TO IT_ZTIVCD-CDAT,
                SY-UNAME               TO IT_ZTIVCD-UNAM,
                SY-DATUM               TO IT_ZTIVCD-UDAT,
                SY-MANDT               TO IT_ZTIVCD-MANDT.

         MOVE : IT_ZTCGCST-ZFCGNO   TO  IT_CGNO-ZFCGNO,
                IT_ZTCGCST-ZFCSQ    TO  IT_CGNO-ZFCSQ,
                IT_ZTIVCD-ZFUPCST   TO  IT_CGNO-ZFCGAMT.
         COLLECT IT_CGNO.

*>> 비용 TABLE APPEND.
         APPEND IT_ZTIVCD.
         ADD  IT_ZTIVCD-ZFUPCST    TO L_ZFUPCST.
      ENDLOOP.

*>> INTERNAL TABLE MODIFY.
      IT_ZTCGCST-ZFUPCST = IT_ZTCGCST-ZFUPCST + L_ZFUPCST.
      MODIFY  IT_ZTCGCST  INDEX  W_TABIX.
   ENDLOOP.
*--------------------------------------------------------------


*--------------------------------------------------------------
*>> 통관 비용배부.
*--------------------------------------------------------------
   REFRESH : IT_CCNO.
   CLEAR : W_ZFDAMT, IT_CCNO.
   LOOP AT IT_ZTCUCLCST WHERE ZFCAMT NE 0.
      W_TABIX = SY-TABIX.

*>> 통관요청 자재별..
      L_ZFUPCST = 0.
      LOOP AT IT_ZSIVIT WHERE ZFIVNO EQ ZTIV-ZFIVNO.
         CLEAR : IT_ZTIVCD.

*>>>>> Planned Cost 여부.
         READ TABLE IT_ZTIMIMG08
                    WITH KEY ZFCDTY = '006'
                             ZFCD   = IT_ZTCUCLCST-ZFCSCD.

         IF SY-SUBRC NE 0.
            MESSAGE E909 WITH IT_ZTCUCLCST-ZFCSCD  RAISING DIV_ERROR.
         ENDIF.

         IF IT_ZTCUCLCST-ZFCSCD EQ '001'.
            W_ZFDAMT = IT_ZTCUCLCST-ZFCAMT.
         ENDIF.

*>> Planned Cost Rate.
         IF IT_ZTIMIMG08-ZFCD1 EQ 'Y'.       ">Planned Cost
            CONTINUE.
*>> UnPlanned Cost.
         ELSEIF IT_ZTIMIMG08-ZFCD1 EQ 'N'.   ">UnPlan Cost
            CLEAR : IT_ZTIVCD-ZFPLRTE, IT_ZTIVCD-ZFPCST.
            IT_ZTIVCD-ZFUPCST =
                         ( IT_ZSIVIT-ZFCCAMT / IT_ZSIVIT-ZFCCAMTH )
                         * IT_ZTCUCLCST-ZFCAMT.

*>> 통관요청비용배부 테이블.
            CLEAR : W_TMPAMT1.
            SELECT SUM( ZFUPCST ) INTO (W_TMPAMT1)
                   FROM  ZTIVCD
                   WHERE ZFIVNO   NE IT_ZSIVIT-ZFIVNO
                   AND   ZFIVDNO  NE IT_ZSIVIT-ZFIVDNO
                   AND   ZFIMDTY  EQ 'CC'
                   AND   ZFIMDNO  EQ IT_ZTCUCLCST-ZFBLNO
                   AND   ZFCLSEQ  EQ IT_ZTCUCLCST-ZFCLSEQ
                   AND   ZFCSQ    EQ IT_ZTCUCLCST-ZFCSQ.

            CLEAR : IT_CGNO.
            READ TABLE IT_CCNO  WITH KEY ZFBLNO  = IT_ZTCUCLCST-ZFBLNO
                                         ZFCLSEQ = IT_ZTCUCLCST-ZFCLSEQ
                                         ZFCSQ   = IT_ZTCUCLCST-ZFCSQ.

*>>>> 기존 배부 금액 + 현재 배부금액 + 같은 문서의 배부금액.
            IF SY-SUBRC EQ 0.
               W_TMPAMT2 = W_TMPAMT1 +
                          IT_ZTIVCD-ZFUPCST + IT_CCNO-ZFCCAMT.
            ELSE.
               W_TMPAMT2 = W_TMPAMT1 + IT_ZTIVCD-ZFUPCST.
            ENDIF.

            IF W_TMPAMT2 GT IT_ZTCUCLCST-ZFCAMT.
               IT_ZTIVCD-ZFUPCST = IT_ZTCUCLCST-ZFCAMT -  W_TMPAMT1
                                   - IT_CCNO-ZFCCAMT.
               IF IT_ZTIVCD-ZFUPCST  LT  0.
                  IT_ZTIVCD-ZFUPCST = 0.
               ENDIF.
            ENDIF.
         ENDIF.

         MOVE : IT_ZSIVIT-ZFIVNO       TO IT_ZTIVCD-ZFIVNO,
                IT_ZSIVIT-ZFIVDNO      TO IT_ZTIVCD-ZFIVDNO,
                'CC'                   TO IT_ZTIVCD-ZFIMDTY,
*                ZTCUCLIV-ZFBLNO        TO IT_ZTIVCD-ZFIMDNO,
*                ZTCUCLIV-ZFCLSEQ       TO IT_ZTIVCD-ZFCLSEQ,
                IT_ZTCUCLCST-ZFCSQ     TO IT_ZTIVCD-ZFCSQ,
                IT_ZTCUCLCST-ZFCSCD    TO IT_ZTIVCD-ZFCSCD,
                IT_ZTCUCLCST-ZFCAMT    TO IT_ZTIVCD-ZFCKAMT,
                'KRW'                  TO IT_ZTIVCD-ZFKRW,
                IT_ZTCUCLCST-ZFVAT     TO IT_ZTIVCD-ZFVAT,
                SY-UNAME               TO IT_ZTIVCD-ERNAM,
                SY-DATUM               TO IT_ZTIVCD-CDAT,
                SY-UNAME               TO IT_ZTIVCD-UNAM,
                SY-DATUM               TO IT_ZTIVCD-UDAT,
                SY-MANDT               TO IT_ZTIVCD-MANDT.

         MOVE : IT_ZTCUCLCST-ZFBLNO   TO  IT_CCNO-ZFBLNO,
                IT_ZTCUCLCST-ZFCLSEQ  TO  IT_CCNO-ZFCLSEQ,
                IT_ZTCUCLCST-ZFCSQ    TO  IT_CCNO-ZFCSQ,
                IT_ZTIVCD-ZFUPCST     TO  IT_CCNO-ZFCCAMT.
         COLLECT IT_CCNO.

*>> 비용 TABLE APPEND.
         APPEND IT_ZTIVCD.
         ADD  IT_ZTIVCD-ZFUPCST    TO L_ZFUPCST.
      ENDLOOP.

*>> INTERNAL TABLE MODIFY.
      IT_ZTCUCLCST-ZFUPCST = IT_ZTCUCLCST-ZFUPCST + L_ZFUPCST.
      MODIFY  IT_ZTCUCLCST  INDEX  W_TABIX.
   ENDLOOP.
*--------------------------------------------------------------


*--------------------------------------------------------------
*>> 배부금액 보정.
*--------------------------------------------------------------
   LOOP AT IT_ZSIVIT.
      L_TABIX = SY-TABIX.

      CLEAR : W_ZFPCST, W_ZFUPCST.
      LOOP AT IT_ZTIVCD WHERE ZFIVNO  = IT_ZSIVIT-ZFIVNO
                        AND   ZFIVDNO = IT_ZSIVIT-ZFIVDNO.

         W_ZFUPCST = W_ZFUPCST + IT_ZTIVCD-ZFUPCST.

      ENDLOOP.

      IF SY-SUBRC NE 0.
*         CLEAR : IT_ZTIVCD.
*         MOVE : IT_ZSIVIT-ZFIVNO       TO IT_ZTIVCD-ZFIVNO,
*                IT_ZSIVIT-ZFIVDNO      TO IT_ZTIVCD-ZFIVDNO,
*                'RM'                   TO IT_ZTIVCD-ZFIMDTY,
*                'KRW'                  TO IT_ZTIVCD-ZFKRW,
*                SY-UNAME               TO IT_ZTIVCD-ERNAM,
*                SY-DATUM               TO IT_ZTIVCD-CDAT,
*                SY-UNAME               TO IT_ZTIVCD-UNAM,
*                SY-DATUM               TO IT_ZTIVCD-UDAT,
*                SY-MANDT               TO IT_ZTIVCD-MANDT,
**                IT_ZSIVIT-ZFUPCST      TO IT_ZTIVCD-ZFUPCST,
*                IT_ZSIVIT-ZFPCST       TO IT_ZTIVCD-ZFPCST.
*
*         APPEND IT_ZTIVCD.
      ELSE.
*         IF IT_ZSIVIT-ZFPCST  NE W_ZFPCST.
*            IF IT_ZSIVIT-ZFPCST  GT W_ZFPCST.
*               W_TMPAMT1 = IT_ZSIVIT-ZFPCST - W_ZFPCST.
*               READ TABLE IT_ZTIVCD INDEX L_TABIX.
*               IT_ZTIVCD-ZFPCST = IT_ZTIVCD-ZFPCST + W_TMPAMT1.
*            ELSE.
*               W_TMPAMT1 = W_ZFPCST - IT_ZSIVIT-ZFPCST.
*               READ TABLE IT_ZTIVCD INDEX L_TABIX.
*               IT_ZTIVCD-ZFPCST = IT_ZTIVCD-ZFPCST - W_TMPAMT1.
*            ENDIF.
*            MODIFY IT_ZTIVCD INDEX  L_TABIX.
*         ENDIF.

         IF IT_ZSIVIT-ZFUPCST NE W_ZFUPCST.
            IF IT_ZSIVIT-ZFUPCST GT W_ZFUPCST.
               W_TMPAMT1 = IT_ZSIVIT-ZFUPCST - W_ZFUPCST.
               READ TABLE IT_ZTIVCD INDEX L_TABIX.
               IT_ZTIVCD-ZFUPCST = IT_ZTIVCD-ZFUPCST + W_TMPAMT1.
            ELSE.
               W_TMPAMT1 = W_ZFUPCST - IT_ZSIVIT-ZFUPCST.
               READ TABLE IT_ZTIVCD INDEX L_TABIX.
               IT_ZTIVCD-ZFUPCST = IT_ZTIVCD-ZFUPCST - W_TMPAMT1.
            ENDIF.
            MODIFY IT_ZTIVCD INDEX  L_TABIX.
         ENDIF.
      ENDIF.
   ENDLOOP.

*--------------------------------------------------------------
*>> 비용배부내역 INSERT.
*--------------------------------------------------------------
   W_LINE = 0.
   DELETE FROM ZTIVCD WHERE ZFIVNO EQ ZFIVNO.

   DESCRIBE  TABLE  IT_ZTIVCD LINES W_LINE.
   IF W_LINE EQ 0.
      MESSAGE E604 RAISING DIV_ERROR.
   ELSE.
      MODIFY  ZTIVCD   FROM TABLE  IT_ZTIVCD.
      IF SY-SUBRC NE 0.
         MESSAGE E429 WITH '수입제비용 배부내역' RAISING DIV_ERROR.
      ENDIF.
   ENDIF.

*--------------------------------------------------------------
*>> 수입의뢰 비용배부.
*--------------------------------------------------------------
   W_LINE = 0.
   DESCRIBE  TABLE  IT_ZTRECST LINES W_LINE.
   IF W_LINE GT 0.
      LOOP AT IT_ZTRECST.
         W_TABIX = SY-TABIX.
         SELECT SUM( ZFUPCST )
                INTO  IT_ZTRECST-ZFUPCST
                FROM  ZTIVCD
                WHERE ZFIMDTY EQ 'RD'
                AND   ZFIMDNO EQ IT_ZTRECST-ZFREQNO
                AND   ZFCSQ   EQ IT_ZTRECST-ZFCSQ.

         IF IT_ZTRECST-ZFPCST  IS INITIAL AND
            IT_ZTRECST-ZFUPCST IS INITIAL.
            CLEAR : IT_ZTRECST-ZFCSTYN.
         ELSE.
            IT_ZTRECST-ZFCSTYN = 'X'.
         ENDIF.
         MODIFY IT_ZTRECST INDEX W_TABIX.
      ENDLOOP.
      MODIFY  ZTRECST   FROM TABLE  IT_ZTRECST.
      IF SY-SUBRC NE 0.
         MESSAGE E429 WITH '수입의뢰 비용'  RAISING DIV_ERROR.
      ENDIF.
   ENDIF.

*--------------------------------------------------------------
*>> B/L 비용배부.
*--------------------------------------------------------------
   W_LINE = 0.
   DESCRIBE  TABLE  IT_ZTBLCST LINES W_LINE.
   IF W_LINE GT 0.
      LOOP AT IT_ZTBLCST.
         W_TABIX = SY-TABIX.
         SELECT SUM( ZFUPCST )
                INTO  IT_ZTBLCST-ZFUPCST
                FROM  ZTIVCD
                WHERE ZFIMDTY EQ 'BL'
                AND   ZFIMDNO EQ IT_ZTBLCST-ZFBLNO
                AND   ZFCSQ   EQ IT_ZTBLCST-ZFCSQ.

         IF IT_ZTBLCST-ZFPCST  IS INITIAL AND
            IT_ZTBLCST-ZFUPCST IS INITIAL.
            CLEAR : IT_ZTBLCST-ZFCSTYN.
         ELSE.
            IT_ZTBLCST-ZFCSTYN = 'X'.
         ENDIF.

         MODIFY IT_ZTBLCST INDEX W_TABIX.

      ENDLOOP.
      MODIFY  ZTBLCST   FROM TABLE  IT_ZTBLCST.
      IF SY-SUBRC NE 0.
         MESSAGE E429 WITH 'B/L 비용'  RAISING DIV_ERROR.
      ENDIF.
   ENDIF.

*--------------------------------------------------------------
*>> 하역 비용배부.
*--------------------------------------------------------------
   W_LINE = 0.
   DESCRIBE  TABLE  IT_ZTCGCST LINES W_LINE.
   IF W_LINE GT 0.
      LOOP AT IT_ZTCGCST.
         W_TABIX = SY-TABIX.
         SELECT SUM( ZFUPCST )
                INTO  IT_ZTCGCST-ZFUPCST
                FROM  ZTIVCD
                WHERE ZFIMDTY EQ 'CW'
                AND   ZFIMDNO EQ IT_ZTCGCST-ZFCGNO
                AND   ZFCSQ   EQ IT_ZTCGCST-ZFCSQ.

         IF IT_ZTCGCST-ZFPCST  IS INITIAL AND
            IT_ZTCGCST-ZFUPCST IS INITIAL.
            CLEAR : IT_ZTCGCST-ZFCSTYN.
         ELSE.
            IT_ZTCGCST-ZFCSTYN = 'X'.
         ENDIF.

         MODIFY IT_ZTCGCST INDEX W_TABIX.

      ENDLOOP.
      MODIFY  ZTCGCST   FROM TABLE  IT_ZTCGCST.
      IF SY-SUBRC NE 0.
         MESSAGE E429 WITH '하역 비용'  RAISING DIV_ERROR.
      ENDIF.
   ENDIF.

*--------------------------------------------------------------
*>> 통관 비용배부.
*--------------------------------------------------------------
   W_LINE = 0.
   DESCRIBE  TABLE  IT_ZTCUCLCST LINES W_LINE.
   IF W_LINE GT 0.
      LOOP AT IT_ZTCUCLCST.
         W_TABIX = SY-TABIX.
         SELECT SUM( ZFUPCST )
                INTO  IT_ZTCUCLCST-ZFUPCST
                FROM  ZTIVCD
                WHERE ZFIMDTY EQ 'CC'
                AND   ZFIMDNO EQ IT_ZTCUCLCST-ZFBLNO
                AND   ZFCLSEQ EQ IT_ZTCUCLCST-ZFCLSEQ
                AND   ZFCSQ   EQ IT_ZTCUCLCST-ZFCSQ.

         IF IT_ZTCUCLCST-ZFPCST  IS INITIAL AND
            IT_ZTCUCLCST-ZFUPCST IS INITIAL.
            CLEAR : IT_ZTCUCLCST-ZFCSTYN.
         ELSE.
            IT_ZTCUCLCST-ZFCSTYN = 'X'.
         ENDIF.

         MODIFY IT_ZTCUCLCST INDEX W_TABIX.

      ENDLOOP.
      MODIFY  ZTCUCLCST   FROM TABLE  IT_ZTCUCLCST.
      IF SY-SUBRC NE 0.
         MESSAGE E429 WITH '통관 비용'  RAISING DIV_ERROR.
      ENDIF.
   ENDIF.

*-----------------------------------------------------------------------
*>>> 배부내역 UPDATE.
   REFRESH : IT_ZTIVIT.
   CLEAR : W_ZFPCST, W_ZFUPCST.
   LOOP AT IT_ZSIVIT.
      MOVE-CORRESPONDING  IT_ZSIVIT   TO   IT_ZTIVIT.

      W_ZFPCST  = W_ZFPCST  + IT_ZTIVIT-ZFPCST.
      W_ZFUPCST = W_ZFUPCST + IT_ZTIVIT-ZFUPCST.

      APPEND IT_ZTIVIT.
   ENDLOOP.
   MODIFY  ZTIVIT   FROM TABLE  IT_ZTIVIT.

*--------------------------------------------------------------
*> 통관 정보 SET...
*--------------------------------------------------------------
   SELECT SINGLE * FROM ZTIV WHERE ZFIVNO EQ ZFIVNO.

   MOVE : W_ZFUPCST      TO    ZTIV-ZFUPCST,
          W_ZFPCST       TO    ZTIV-ZFPCST,
          W_ZFDAMT       TO    ZTIV-ZFDAMT,
          'Y'            TO    ZTIV-ZFCDST,
          SY-UNAME       TO    ZTIV-UNAM,
          SY-DATUM       TO    ZTIV-UDAT.

   UPDATE ZTIV.

ENDFUNCTION.
