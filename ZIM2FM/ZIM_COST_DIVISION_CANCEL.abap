FUNCTION ZIM_COST_DIVISION_CANCEL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(ZFIVNO) LIKE  ZTIV-ZFIVNO
*"  EXCEPTIONS
*"      DIV_ERROR
*"----------------------------------------------------------------------
*--------------------------------------------------------------
* 수입부대비용(제비용) 배부용 FUNCTION
*   - 2001/03/03   INFOLINK LTD.  KANG SEUG BONG
*--------------------------------------------------------------
DATA : L_TABIX    LIKE  SY-TABIX,
       L_DWBTR    LIKE  ZTRECST,
       W_ZFBLAMT  LIKE  ZTBL-ZFBLAMT,
       W_ZFCGAMT  LIKE  ZTBL-ZFBLAMT,
       W_ZFCCAMT  LIKE  ZTBL-ZFBLAMT,
       W_ZFCCAMTH LIKE  ZTBL-ZFBLAMT,
       W_TMPAMT1  LIKE  ZTBL-ZFBLAMT,
       W_TMPAMT2  LIKE  ZTBL-ZFBLAMT,
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
     WHEN '1' OR '2' OR '3' OR 'X' OR 'N'.
        PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                  USING      'ZDCUST'  ZTIV-ZFCUST
                  CHANGING    W_TEXT70.
        MESSAGE E419 WITH ZFIVNO W_TEXT70 '비용배부'
                           RAISING DIV_ERROR.
     WHEN 'Y'.
  ENDCASE.

*>>> 비용배부 상태 체크.
  CASE ZTIV-ZFCDST.
     WHEN 'X' OR 'N'.     ">배부불가. or 배부대상(NONE)
        PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                  USING      'ZDCDST'  ZTIV-ZFCDST
                  CHANGING    W_TEXT70.
        MESSAGE E420 WITH ZFIVNO W_TEXT70 '비용배부'
                           RAISING DIV_ERROR.
     WHEN 'Y'.     ">배부완료. (MESSAGE)
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


*-----------------------------------------------------------
*>> 통관요청 자재 READ CHECK.
*-----------------------------------------------------------
   SELECT * INTO TABLE IT_ZSIVIT
            FROM  ZTIVIT
            WHERE ZFIVNO EQ  ZFIVNO.
   IF SY-SUBRC NE 0.
      MESSAGE E417 WITH ZFIVNO  RAISING DIV_ERROR.
   ENDIF.

*>> 비용배부 READ CHECK.
   SELECT * INTO TABLE IT_ZTIVCD
            FROM ZTIVCD
            WHERE ZFIVNO EQ  ZFIVNO.

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

*>> 통관 비용.
   SELECT * INTO TABLE IT_ZTCUCLCST
             FROM  ZTCUCLCST
             WHERE ZFBLNO  EQ  ZTCUCLIV-ZFBLNO
             AND   ZFCLSEQ EQ  ZTCUCLIV-ZFCLSEQ
             AND   ZFCAMT  NE  0.
*--------------------------------------------------------------

*--------------------------------------------------------------
*>> 기본 비용배부 금액 감산.
   LOOP AT IT_ZTIVCD.
      W_TABIX = SY-TABIX.

      CASE IT_ZTIVCD-ZFIMDTY.
         WHEN 'RD'.           ">> 수입의뢰비용.
            READ TABLE  IT_ZTRECST
                 WITH KEY ZFREQNO = IT_ZTIVCD_OLD-ZFIMDNO
                          ZFCSQ   = IT_ZTIVCD_OLD-ZFCSQ.
            L_TABIX = SY-TABIX.
            IF SY-SUBRC EQ 0.
               SUBTRACT IT_ZTIVCD-ZFUPCST
                                  FROM  IT_ZTRECST-ZFUPCST.
               MODIFY  IT_ZTRECST  INDEX  L_TABIX.
            ENDIF.
         WHEN 'BL'.           ">> B/L 비용.
            READ TABLE  IT_ZTBLCST
                 WITH KEY ZFBLNO  = IT_ZTIVCD_OLD-ZFIMDNO
                          ZFCSQ   = IT_ZTIVCD_OLD-ZFCSQ.
            L_TABIX = SY-TABIX.
            IF SY-SUBRC EQ 0.
               SUBTRACT IT_ZTIVCD-ZFUPCST
                                  FROM IT_ZTBLCST-ZFUPCST.
               MODIFY IT_ZTBLCST  INDEX  L_TABIX.
            ENDIF.
         WHEN 'CW'.           ">> 하역비용.
            READ TABLE  IT_ZTCGCST
                 WITH KEY ZFCGNO  = IT_ZTIVCD_OLD-ZFIMDNO
                          ZFCSQ   = IT_ZTIVCD_OLD-ZFCSQ.
            L_TABIX = SY-TABIX.
            IF SY-SUBRC EQ 0.
               SUBTRACT IT_ZTIVCD-ZFUPCST
                                  FROM  IT_ZTCGCST-ZFUPCST.
               MODIFY IT_ZTCGCST  INDEX  L_TABIX.
            ENDIF.
         WHEN 'CC'.           ">> 통관비용.
            READ TABLE  IT_ZTCUCLCST
                 WITH KEY ZFBLNO  = IT_ZTIVCD_OLD-ZFIMDNO
                          ZFCLSEQ = IT_ZTIVCD_OLD-ZFCLSEQ
                          ZFCSQ   = IT_ZTIVCD_OLD-ZFCSQ.
            L_TABIX = SY-TABIX.
            IF SY-SUBRC EQ 0.
               SUBTRACT IT_ZTIVCD-ZFUPCST
                                  FROM IT_ZTCUCLCST-ZFUPCST.
               MODIFY IT_ZTCUCLCST  INDEX  L_TABIX.
            ENDIF.
      ENDCASE.
   ENDLOOP.
*--------------------------------------------------------------

*--------------------------------------------------------------
*>> 비용배부내역 INSERT.
*--------------------------------------------------------------
   W_LINE = 0.
   DELETE ZTIVCD FROM TABLE IT_ZTIVCD.

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
*> 통관요청 아이템 정보 UPDATE.
*-----------------------------------------------------------------------
   LOOP AT IT_ZSIVIT.
      MOVE : 0         TO    IT_ZSIVIT-ZFUPCST,
             0         TO    IT_ZSIVIT-ZFPCST,
             SY-DATUM  TO    IT_ZSIVIT-UDAT,
             SY-UNAME  TO    IT_ZSIVIT-UDAT.
      MODIFY IT_ZSIVIT  INDEX  SY-TABIX.
   ENDLOOP.

*--------------------------------------------------------------
*> 통관요청 헤더 정보 UPDATE...
*--------------------------------------------------------------

   UPDATE ZTIV
          SET : ZFUPCST   = 0
                ZFPCST    = 0
                ZFDAMT    = 0
                ZFCDST    = 'N'
                UDAT      = SY-DATUM
                UNAM      = SY-UNAME
          WHERE ZFIVNO EQ ZFIVNO.

ENDFUNCTION.
