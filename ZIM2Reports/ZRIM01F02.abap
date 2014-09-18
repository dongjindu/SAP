*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM01F02                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입 B/L 관련 Sub MODULE Include
*&      작성자 : 강석봉 INFOLINK Ltd.
*&      작성일 : 2000.02.21
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_MSHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_MSHD.
  REFRESH  R_TERM.

* 해당하는 모선명으로 모선관리 Head 정보 SELECT.
  IF NOT W_ETA_FROM IS INITIAL.
    MOVE : 'I'         TO       R_TERM-SIGN,
           'GE'        TO       R_TERM-OPTION,
           W_ETA_FROM  TO       R_TERM-LOW,
           SPACE       TO       R_TERM-HIGH.
    APPEND    R_TERM.
  ENDIF.
  IF NOT W_ETA_TO IS INITIAL.
    MOVE : 'I'         TO       R_TERM-SIGN,
           'LE'        TO       R_TERM-OPTION,
           W_ETA_TO    TO       R_TERM-LOW,
           SPACE       TO       R_TERM-HIGH.
    APPEND    R_TERM.
  ENDIF.

  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_ZSMSHD
  FROM   ZTMSHD
  WHERE  ZFMSNM  EQ       ZTMSHD-ZFMSNM
  AND    EKGRP   EQ       ZTMSHD-EKGRP
  AND    ZFETA   IN       R_TERM.

  IF W_OK_CODE NE 'DELE'.
    DESCRIBE TABLE IT_ZSMSHD LINES W_COUNT.
    IF W_COUNT = 0.
      MESSAGE  S738.
    ENDIF.
  ENDIF.

ENDFORM.                    " P1000_READ_MSHD
*&---------------------------------------------------------------------*
*&      Form  P1000_SORT_MSHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_SORT_MSHD.

  SORT  IT_ZSMSHD  BY  ZFSHSDF  ZFETA.

ENDFORM.                    " P1000_SORT_MSHD
*&---------------------------------------------------------------------*
*&      Form  P2000_READ_MSHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_READ_MSHD.

  REFRESH : IT_ZSMSIT,
            IT_ZSMSCST,
            IT_ZSMSIT_ORG,
            IT_ZSMSCST_ORG.

* ZFMSHD TABLE READ.
  SELECT  SINGLE *
  FROM    ZTMSHD
  WHERE   ZFMSNO  =  IT_ZSMSHD-ZFMSNO.

  IF SY-SUBRC NE 0.
    MESSAGE E466 WITH IT_ZSMSHD-ZFMSNO.
  ENDIF.

  CLEAR  *ZTMSHD.
  MOVE-CORRESPONDING  ZTMSHD  TO  *ZTMSHD.

  IF OK-CODE EQ 'CHGE'.
    PERFORM P2000_SET_LOCK_ZTMSHD USING 'L'.
  ENDIF.

* ZFMSIT TABLE READ
  SELECT  *
  INTO  CORRESPONDING FIELDS OF TABLE IT_ZSMSIT
  FROM  ZTMSIT
  WHERE ZFMSNO  =  IT_ZSMSHD-ZFMSNO.

  CLEAR  IT_ZSMSIT.

* LC NO & HOUSE BL NO SELECT.
  LOOP  AT  IT_ZSMSIT.

    W_TABIX  =  SY-TABIX.

    CLEAR  ZTREQHD.

    SELECT  SINGLE  *
    FROM    ZTREQHD
    WHERE   ZFREQNO  =  IT_ZSMSIT-ZFREQNO.

    IF  SY-SUBRC  EQ  0.
      MOVE  ZTREQHD-ZFOPNNO  TO  IT_ZSMSIT-ZFOPNNO.
      MOVE  ZTREQHD-ZFREQTY  TO  IT_ZSMSIT-ZFREQTY.
      MOVE  ZTREQHD-EBELN    TO  IT_ZSMSIT-EBELN.
      MOVE  ZTREQHD-ZFLASTAM TO  IT_ZSMSIT-ZFLASTAM.
      MOVE  ZTREQHD-WAERS    TO  IT_ZSMSIT-WAERS.
      MOVE  ZTREQHD-ZFLASTSD TO  IT_ZSMSIT-ZFLASTSD.
      MOVE  ZTREQHD-ZFLASTED TO  IT_ZSMSIT-ZFLASTED.
    ENDIF.

    MODIFY  IT_ZSMSIT  INDEX  W_TABIX.

  ENDLOOP.

  IT_ZSMSIT_ORG[]   =  IT_ZSMSIT[].

* ZFMSCST TABLE READ
  SELECT  *
  INTO    CORRESPONDING FIELDS OF TABLE IT_ZSMSCST
  FROM    ZTMSCST
  WHERE   ZFMSNO  =  IT_ZSMSHD-ZFMSNO.

  IT_ZSMSCST_ORG[]  =  IT_ZSMSCST[].

ENDFORM.                    " P2000_READ_MSHD
*&---------------------------------------------------------------------*
*&      Form  P2000_SORT_MSHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SORT_MSHD.

  SORT  IT_ZSMSCST     BY ZFAPRTC.
  SORT  IT_ZSMSCST_ORG BY ZFAPRTC.
  SORT  IT_ZSMSIT      BY ZFMSSEQ.
  SORT  IT_ZSMSIT_ORG  BY ZFMSSEQ.

ENDFORM.                    " P2000_SORT_MSHD

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_MODIFY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_GUBUN  text
*----------------------------------------------------------------------*
FORM P2000_SET_MODIFY_CHECK USING    P_W_GUBUN.

  W_GUBUN = 'Y'.
  DESCRIBE TABLE IT_ZSMSIT      LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSMSIT_ORG  LINES W_COUNTER1.
  DESCRIBE TABLE IT_ZSMSCST     LINES W_COUNTER2.
  DESCRIBE TABLE IT_ZSMSCST_ORG LINES W_COUNTER3.

  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'C'.     " New Entry
    IF ( W_COUNTER EQ 0 ) AND ( W_COUNTER2 EQ 0 ) AND
       ( ZTMSHD-ZFSHSDF IS INITIAL ).
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode

    IF ZTMSHD NE *ZTMSHD.
      W_LOOP_CNT = W_LOOP_CNT + 1.
    ENDIF.

* 화면상의 INTERNAL TABLE 과 ORIGINAL INTERNAL TABLE 비교.
    IF IT_ZSMSIT[] NE IT_ZSMSIT_ORG[].
      W_LOOP_CNT = W_LOOP_CNT + 1.
    ENDIF.

    IF IT_ZSMSCST[] NE IT_ZSMSCST_ORG[].
      W_LOOP_CNT = W_LOOP_CNT + 1 .
    ENDIF.

    IF  W_LOOP_CNT = 0.
      W_GUBUN = 'N'.
    ENDIF.

  ENDIF.

ENDFORM.                    " P2000_SET_MODIFY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_MSDATA_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_MSDATA_MODIFY.

  CLEAR : W_ZFMSNO, W_ZFREQNO, W_ZFBLNO, W_ZFAPRTC.

* Internal Table Sort
  SORT IT_ZSMSIT  BY ZFMSNO   ZFMSSEQ.
  SORT IT_ZSMSCST BY ZFMSNO   ZFAPRTC.

* Internal Table Looping( 중복 검증 )
* 모선관리 ITEM TABLE 중복검증.
  LOOP AT IT_ZSMSIT.

    IF W_ZFREQNO  EQ IT_ZSMSIT-ZFREQNO.
      MESSAGE E956 WITH 'Import request' W_ZFREQNO.
    ENDIF.

    MOVE : IT_ZSMSIT-ZFREQNO    TO W_ZFREQNO,
           SY-TABIX             TO W_TABIX.
* 신규일 경우 DB와 검증 작업( 중복 오류 )
    IF W_STATUS EQ 'C'.
      IF IT_ZSMSIT-ELOEK NE 'X'.
        READ TABLE IT_ZSMSIT_ORG WITH KEY
                   ZFREQNO  = IT_ZSMSIT-ZFREQNO.
        IF SY-SUBRC EQ 0.
          MESSAGE E956 WITH 'Import request' W_ZFREQNO.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR W_ZFMSNO.

* 조출/체선료 TABLE 중복 검증.
  LOOP AT IT_ZSMSCST.

    IF W_ZFMSNO   EQ IT_ZSMSCST-ZFMSNO AND
       W_ZFAPRTC  EQ IT_ZSMSCST-ZFAPRTC.
      MESSAGE E956 WITH W_ZFMSNO  W_ZFAPRTC.
    ENDIF.

    MOVE : IT_ZSMSCST-ZFMSNO     TO W_ZFMSNO,
           IT_ZSMSCST-ZFAPRTC    TO W_ZFAPRTC,
           SY-TABIX              TO W_TABIX.
* 신규일 경우 DB와 검증 작업( 중복 오류 )
    IF W_STATUS EQ 'C'.
      IF IT_ZSMSCST-ELOEK NE 'X'.
        READ TABLE IT_ZSMSCST_ORG WITH KEY
                        ZFMSNO  = IT_ZSMSCST-ZFMSNO
                        ZFAPRTC = IT_ZSMSCST-ZFAPRTC.
        IF SY-SUBRC EQ 0.
          MESSAGE E956 WITH W_ZFMSNO W_ZFAPRTC.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* DELETE

* 모선관리 ITEM TABLE DELETE.
  LOOP AT IT_ZSMSIT_DEL.
    DELETE FROM ZTMSIT
           WHERE ZFMSNO   EQ IT_ZSMSIT_DEL-ZFMSNO
           AND   ZFMSSEQ  EQ IT_ZSMSIT_DEL-ZFMSSEQ.

  ENDLOOP.
  REFRESH : IT_ZSMSIT_DEL.

* 모선관리 조출/체선료 TABLE DELETE.
  LOOP AT IT_ZSMSCST_DEL.
    DELETE FROM  ZTMSCST
           WHERE ZFMSNO   EQ IT_ZSMSCST_DEL-ZFMSNO
           AND   ZFAPRTC  EQ IT_ZSMSCST_DEL-ZFAPRTC.

  ENDLOOP.
  REFRESH : IT_ZSMSCST_DEL.

* HEADER TABLE DELETE 여부에 따라서 DELETE.
  IF W_HEAD_DEL = 'X'.
    DELETE FROM  ZTMSHD
           WHERE ZFMSNO  EQ  *ZTMSHD-ZFMSNO.
  ENDIF.

* 데이타를 INSERT

* 모선관리 HEAD TABLE INSERT.
  IF NOT ( ZTMSHD-ZFMSNO  IS INITIAL )        AND
     NOT ( ZTMSHD-ZFFORD  EQ *ZTMSHD-ZFFORD   AND
           ZTMSHD-ZFSPRTC EQ *ZTMSHD-ZFSPRTC  AND
           ZTMSHD-ZFAPRTC EQ *ZTMSHD-ZFAPRTC  AND
           ZTMSHD-ZFSHSDF EQ *ZTMSHD-ZFSHSDF  AND
           ZTMSHD-ZFSHSDT EQ *ZTMSHD-ZFSHSDT  AND
           ZTMSHD-ZFETA   EQ *ZTMSHD-ZFETA    AND
           ZTMSHD-ZFREF1  EQ *ZTMSHD-ZFREF1   AND
           ZTMSHD-ZFREF2  EQ *ZTMSHD-ZFREF2   AND
           ZTMSHD-BUKRS   EQ *ZTMSHD-BUKRS    AND
           ZTMSHD-EKORG   EQ *ZTMSHD-EKORG    AND
           ZTMSHD-EKGRP   EQ *ZTMSHD-EKGRP    AND
           ZTMSHD-ZFLCAG  EQ *ZTMSHD-ZFLCAG   ).

    MOVE : SY-UNAME  TO   ZTMSCST-UNAM,
           SY-DATUM  TO   ZTMSCST-UDAT.
    UPDATE ZTMSHD.
    IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.
  ENDIF.

  IF ZTMSHD-ZFMSNO  IS  INITIAL.

    PERFORM P2000_GET_NUMBER_NEXT USING 'MS' ZTMSHD-ZFMSNO.
    MOVE : SY-UNAME  TO   ZTMSHD-ERNAM,
           SY-DATUM  TO   ZTMSHD-CDAT,
           SY-UNAME  TO   ZTMSHD-UNAM,
           SY-DATUM  TO   ZTMSHD-UDAT.

    INSERT ZTMSHD.
    IF SY-SUBRC  NE  0.  MESSAGE E952.  ENDIF.
  ENDIF.

  SET PARAMETER ID 'ZPMSNO' FIELD ZTMSHD-ZFMSNO.

* 모선관리 ITEM TABLE INSERT & UPDATE.
  LOOP AT IT_ZSMSIT.

    IT_ZSMSIT-ZFMSNO = ZTMSHD-ZFMSNO.

    W_TABIX  =  SY-TABIX.
    IF IT_ZSMSIT-ZFMSSEQ IS INITIAL.
      IF IT_ZSMSIT-ELOEK EQ 'X'.   CONTINUE.   ENDIF.
* ITEM SEQ 증가.
      SELECT  MAX( ZFMSSEQ )
      INTO    W_MAX_SEQ
      FROM    ZTMSIT
      WHERE   ZFMSNO  =  IT_ZSMSIT-ZFMSNO .

      IF SY-SUBRC  EQ  0.
        W_MAX_SEQ  =  W_MAX_SEQ  + 1.
      ELSE.
        W_MAX_SEQ  =  1.
      ENDIF.

      IT_ZSMSIT-ZFMSSEQ  =  W_MAX_SEQ.
      MODIFY  IT_ZSMSIT  INDEX  W_TABIX.
      MOVE-CORRESPONDING IT_ZSMSIT TO ZTMSIT.
      INSERT ZTMSIT.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.
    ELSE.
      IF NOT ( IT_ZSMSIT-ZFREQNO  EQ ZTMSIT-ZFREQNO ).
        MOVE-CORRESPONDING IT_ZSMSIT TO  ZTMSIT.
        UPDATE ZTMSIT.
        IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.                  " END LOOP

* 조출/체선료 TABLE INSERT & UPDATE.
  LOOP AT IT_ZSMSCST.

    IT_ZSMSCST-ZFMSNO = ZTMSHD-ZFMSNO.
    IT_ZSMSCST-ZFKRW  = 'KRW'.

    IF IT_ZSMSCST-ZFAPRTC IS INITIAL.
      DELETE IT_ZSMSCST.
      CONTINUE.
    ENDIF.

    SELECT SINGLE * FROM ZTMSCST
                    WHERE ZFMSNO   EQ IT_ZSMSCST-ZFMSNO
                    AND   ZFAPRTC  EQ IT_ZSMSCST-ZFAPRTC.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSMSCST-ZFETA   EQ ZTMSCST-ZFETA
         AND   IT_ZSMSCST-LIFNR   EQ ZTMSCST-LIFNR
         AND   IT_ZSMSCST-ZFCARGO EQ ZTMSCST-ZFCARGO
         AND   IT_ZSMSCST-ZFPER   EQ ZTMSCST-ZFPER
         AND   IT_ZSMSCST-ZFCCGB  EQ ZTMSCST-ZFCCGB
         AND   IT_ZSMSCST-ZFSHAMT EQ ZTMSCST-ZFSHAMT
         AND   IT_ZSMSCST-ZFUNCO  EQ ZTMSCST-ZFUNCO
         AND   IT_ZSMSCST-ZFNOPY  EQ ZTMSCST-ZFNOPY
         AND   IT_ZSMSCST-ZFCST   EQ ZTMSCST-ZFCST
         AND   IT_ZSMSCST-ZFPROF  EQ ZTMSCST-ZFPROF
         AND   IT_ZSMSCST-ZFOCDT  EQ ZTMSCST-ZFOCDT
         AND   IT_ZSMSCST-BELNR   EQ ZTMSCST-BELNR
         AND   IT_ZSMSCST-GJAHR   EQ ZTMSCST-GJAHR
         AND   IT_ZSMSCST-ZFPSDT  EQ ZTMSCST-ZFPSDT
         AND   IT_ZSMSCST-ZTERM   EQ ZTMSCST-ZTERM
         AND   IT_ZSMSCST-MWSKZ   EQ ZTMSCST-MWSKZ
         AND   IT_ZSMSCST-ZFEXRT  EQ ZTMSCST-ZFEXRT
         AND   IT_ZSMSCST-ZFCAMT  EQ ZTMSCST-ZFCAMT
         AND   IT_ZSMSCST-ZFVAT   EQ ZTMSCST-ZFVAT
         AND   IT_ZSMSCST-ZFWERKS EQ ZTMSCST-ZFWERKS
         AND   IT_ZSMSCST-BUPLA   EQ ZSMSCST-BUPLA
         AND   IT_ZSMSCST-ZFKOSTL EQ ZTMSCST-ZFKOSTL ).

        MOVE-CORRESPONDING IT_ZSMSCST TO  ZTMSCST.
        MOVE : SY-UNAME  TO   ZTMSCST-UNAM,
               SY-DATUM  TO   ZTMSCST-UDAT.

        UPDATE ZTMSCST.
        IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.
      ENDIF.
    ELSE.
      IF IT_ZSMSCST-ELOEK EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSMSCST TO ZTMSCST.
      MOVE : ZTMSHD-BUKRS TO ZTMSCST-BUKRS,
             SY-UNAME     TO ZTMSCST-ERNAM,
             SY-DATUM     TO ZTMSCST-CDAT,
             SY-UNAME     TO ZTMSCST-UNAM,
             SY-DATUM     TO ZTMSCST-UDAT.

      INSERT ZTMSCST.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.
    ENDIF.
  ENDLOOP.                  " END LOOP

  SORT IT_ZSMSIT  BY ZFMSSEQ.
  SORT IT_ZSMSCST BY ZFAPRTC.
ENDFORM.                    " P3000_MSDATA_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P2000_DEL_ALL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_GUBUN  text
*----------------------------------------------------------------------*
FORM P2000_DEL_ALL_DATA USING    P_W_GUBUN.

  W_GUBUN = 'Y'.

  PERFORM P2000_MESSAGE_BOX USING '삭제 확인'             " 타이틀...
                                  '모선정보 모두 삭제합니다.'
                                  '삭제하시겠습니까?' " Message #2
                                  'Y'                 " 취소 버튼 유/?
                                  '1'.                 " default button

  IF ANTWORT = 'Y'.
* DISPLAY MODE 인 경우 LOCK 걸기.
    IF W_STATUS = C_REQ_D.
      PERFORM P2000_SET_LOCK_ZTMSHD USING 'L'.
    ENDIF.

    LOOP  AT  ZTMSCST.
      IF NOT ( ZTMSCST-BELNR  IS  INITIAL ).
        IF W_STATUS = C_REQ_D.
          PERFORM P2000_SET_LOCK_ZTMSHD USING 'U'.
        ENDIF.
        W_GUBUN = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    DELETE  FROM  ZTMSIT
            WHERE ZFMSNO = ZTMSHD-ZFMSNO.
    DELETE  FROM  ZTMSCST
            WHERE ZFMSNO = ZTMSHD-ZFMSNO.
    DELETE  FROM  ZTMSHD
            WHERE ZFMSNO = ZTMSHD-ZFMSNO.

    PERFORM P2000_SET_LOCK_ZTMSHD USING 'U'.

  ENDIF.
ENDFORM.                    " P2000_DEL_ALL_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_DATA_REFRESH.

  IF W_OK_GUBN = '1'.
    IT_ZSMSIT[] = IT_ZSMSIT_ORG[].
  ENDIF.

  IF W_OK_GUBN = '2'.
    IT_ZSMSCST[] = IT_ZSMSCST_ORG[].
  ENDIF.

  IF W_OK_GUBN = '3'.
    ZTMSHD       = *ZTMSHD.
    IT_ZSMSIT[]  = IT_ZSMSIT_ORG[].
    IT_ZSMSCST[] = IT_ZSMSCST_ORG[].
  ENDIF.

ENDFORM.                    " P2000_DATA_REFRESH
*&---------------------------------------------------------------------*
*&      Form  P3000_MSDATA_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_MSDATA_DELETE.

  CLEAR ANTWORT.
  PERFORM P2000_MESSAGE_BOX USING '삭제 확인'             " 타이틀...
                                  '모선정보 모두 삭제합니다.'
                                  '삭제하시겠습니까?' " Message #2
                                  'Y'                 " 취소 버튼 유/?
                                  '1'.                 " default button

  IF ANTWORT = 'Y'.

    READ TABLE IT_ZSMSHD WITH KEY ZFMARK = 'X'.
    IF SY-SUBRC NE 0.
      MESSAGE E766.
      EXIT.
    ENDIF.

    SELECT COUNT(*)
    INTO   W_DATA_CNT
    FROM   ZTMSCST
    WHERE  ZFMSNO  =  IT_ZSMSHD-ZFMSNO
    AND    BELNR   NE SPACE.

    IF W_DATA_CNT NE 0.
      MESSAGE E462.
      EXIT.
    ENDIF.

* DATA LOCK
    PERFORM P2000_SET_LOCK_ZTMSHD USING 'L'.

* 모선정보 HEAD, ITEM, 조출/체선료 TABLE DELETE.
    DELETE  FROM  ZTMSIT
            WHERE ZFMSNO = IT_ZSMSHD-ZFMSNO.
    DELETE  FROM  ZTMSCST
            WHERE ZFMSNO = IT_ZSMSHD-ZFMSNO.
    DELETE  FROM  ZTMSHD
            WHERE ZFMSNO = IT_ZSMSHD-ZFMSNO.

    PERFORM P2000_SET_LOCK_ZTMSHD USING 'U'.
  ENDIF.

ENDFORM.                    " P3000_MSDATA_DELETE
*&---------------------------------------------------------------------*
*&      Form  P2000_MSNM_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MSNM_LIST.

  WRITE : / IT_MSNM-ZFMSNM NO-GAP .
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

ENDFORM.                    " P2000_MSNM_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_LOCK_ZTMSHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0095   text
*----------------------------------------------------------------------*
FORM P2000_SET_LOCK_ZTMSHD USING  P_PARA.

  IF P_PARA = 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_ZTMSHD'
         EXPORTING
              ZFMSNO = ZTMSHD-ZFMSNO
         EXCEPTIONS
              OTHERS = 1.

    IF SY-SUBRC NE 0.
      MESSAGE E510 WITH SY-MSGV1 'Mothership management'
      ZTMSHD-ZFMSNO ''
      RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF P_PARA = 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_ZTMSHD'
         EXPORTING
              ZFMSNO = ZTMSHD-ZFMSNO.
  ENDIF.

ENDFORM.                    " P2000_SET_LOCK_ZTMSHD
*&---------------------------------------------------------------------*
*&      Form  P2000_FI_DOC_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSMSCST_BELNR  text
*      -->P_IT_ZSMSCST_BUKRS  text
*      -->P_IT_ZSMSCST_GJAHR  text
*----------------------------------------------------------------------*
FORM P2000_FI_DOC_DISPLAY USING    P_BELNR
                                   P_BUKRS
                                   P_GJAHR.
  IF P_BELNR IS INITIAL.
    MESSAGE S814.
    EXIT.
  ENDIF.
  IF P_BUKRS IS INITIAL.
    MESSAGE S167 WITH 'Company code'.
    EXIT.
  ENDIF.
  IF P_GJAHR IS INITIAL.
    MESSAGE S167 WITH 'Fiscal year'.
    EXIT.
  ENDIF.

  SET PARAMETER ID 'BLN' FIELD P_BELNR.
  SET PARAMETER ID 'BUK' FIELD P_BUKRS.
  SET PARAMETER ID 'GJR' FIELD P_GJAHR.

  CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_FI_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_DOC_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_DOC_CHANGE.

  IF SY-DYNNR = '9912'.
    SPOP-TITEL = 'Dispatch/demurrage condition change & display'.
    OPTION = 1.

    CALL SCREEN 9913 STARTING AT 5  6
                     ENDING   AT 85 18.

    IF ANTWORT EQ 'C' OR ANTWORT EQ 'N'.       " Cancel Or No
      SET SCREEN SY-DYNNR.
    ENDIF.
  ELSEIF SY-DYNNR EQ '0106' OR SY-DYNNR EQ '0104'.
    SPOP-TITEL = 'BL expense condition change & display'.
    OPTION = 1.

    CALL SCREEN 0199 STARTING AT 5  6
                     ENDING   AT 85 22.
  ELSEIF SY-DYNNR EQ '0813'.
    SPOP-TITEL = 'Loading/Unloading expense condition change & display'.
    OPTION     =  1.

    CALL SCREEN 0899 STARTING AT 5   6
                     ENDING   AT 85 18.
  ELSEIF SY-DYNNR EQ '7420'.
    SPOP-TITEL = 'Clearance expense condition change & display'.
    OPTION     =  1.

    CALL SCREEN 7499 STARTING AT 5  6
                     ENDING   AT 105 16.

  ENDIF.
ENDFORM.                    " P2000_DOC_CHANGE
*&---------------------------------------------------------------------*
*&      Module  GET_NAME_SCR7420  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_NAME_SCR7420 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZSCUCLCST-ZFCSCD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCUCLCST' 'ZFCSCD'.
  ENDIF.
  IF ZSCUCLCST-ZFCAMT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCUCLCST' 'ZFCAMT'.
  ENDIF.
  IF ZSCUCLCST-ZFVEN IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCUCLCST' 'ZFVEN'.
  ENDIF.

* Internal Table Read
  READ TABLE IT_ZSCUCLCST INDEX TC_7420-CURRENT_LINE.
*                           WITH KEY ZFCSQ = ZSCGCST-ZFCSQ.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSCUCLCST   TO IT_ZSCUCLCST.
  IT_ZSCUCLCST-ZFKRW = W_KRW.
  IT_ZSCUCLCST-BUKRS = ZTIV-BUKRS.

*  IF ZSCUCLCST-ZFCAMT IS INITIAL.
*     IF W_SY_SUBRC EQ 0.
*        MODIFY IT_ZSCUCLCST  INDEX W_TABIX.
*     ELSE.
**        IT_ZSCUCLCST-ZFCSQ   = ( TC_-CURRENT_LINE * 10 ).
*        APPEND IT_ZSCUCLCST.
*     ENDIF.
*  ENDIF.

  CHECK  NOT ZSCUCLCST-ZFCAMT IS INITIAL.
*>> 비용코드 체크.

  SELECT SINGLE ZFCDNM
         INTO IT_ZSCUCLCST-ZFCDNM
         FROM ZTIMIMG08
         WHERE ZFCDTY EQ '006'
         AND   ZFCD   EQ IT_ZSCUCLCST-ZFCSCD.
  IF SY-SUBRC NE 0.
    PERFORM  P2000_NO_INPUT  USING 'ZSCUCLCST' 'ZFCDNM'
                                    DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE E909 WITH IT_ZSCUCLCST-ZFCSCD.
  ENDIF.

* 필수 입력 검증.
  CLEAR: LFA1, IT_ZSCUCLCST-ZFVENNM.
  SELECT SINGLE * FROM LFA1
         WHERE LIFNR = IT_ZSCUCLCST-ZFVEN.
  IF SY-SUBRC NE 0.
    PERFORM  P2000_NO_INPUT  USING 'ZSCUCLCST' 'ZFVEN'
                                    DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE E025.
  ENDIF.
  IT_ZSCUCLCST-ZFVENNM = LFA1-NAME1.

  IF IT_ZSCUCLCST-ZFPAY IS INITIAL.
    IF LFA1-LNRZA IS INITIAL.
      MOVE IT_ZSCUCLCST-ZFVEN  TO IT_ZSCUCLCST-ZFPAY.   ">지불처.
    ELSE.
      MOVE LFA1-LNRZA          TO IT_ZSCUCLCST-ZFPAY.   ">지불처.
    ENDIF.
    IF IT_ZSCUCLCST-ZFPAY IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCUCLCST' 'ZFPAY'.
    ENDIF.
  ENDIF.

  IF IT_ZSCUCLCST-ZTERM IS INITIAL.
    SELECT SINGLE ZTERM INTO IT_ZSCUCLCST-ZTERM        ">Payment Term
           FROM LFB1
           WHERE LIFNR = IT_ZSCUCLCST-ZFVEN
           AND BUKRS   = IT_ZSCUCLCST-BUKRS.
    IF IT_ZSCUCLCST-ZTERM IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCUCLCST' 'ZTERM'.
    ENDIF.
  ENDIF.

  IF NOT ( IT_ZSCUCLCST-ZFPAY IS INITIAL ).
    CLEAR : IT_ZSCUCLCST-ZFPAYNM.
    SELECT SINGLE * FROM LFA1
           WHERE LIFNR = IT_ZSCUCLCST-ZFPAY.
    IF SY-SUBRC NE 0.
      MESSAGE E341.
    ENDIF.
    IT_ZSCUCLCST-ZFPAYNM = LFA1-NAME1.
  ELSE.
    CLEAR : IT_ZSCUCLCST-ZFPAYNM.
  ENDIF.

  IF IT_ZSCUCLCST-ZFOCDT IS INITIAL.   " 발생일.
    IT_ZSCUCLCST-ZFOCDT    =    SY-DATUM.
  ENDIF.

  IF IT_ZSCUCLCST-MWSKZ IS INITIAL.    " TAX CODE
    SELECT SINGLE ZFCD5 ZFCDNM
           INTO (IT_ZSCUCLCST-MWSKZ, IT_ZSCUCLCST-ZFCDNM)
           FROM ZTIMIMG08
           WHERE ZFCDTY EQ '006'
           AND   ZFCD   EQ IT_ZSCUCLCST-ZFCSCD.

    IF IT_ZSCUCLCST-MWSKZ IS INITIAL.    " TAX CODE
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCUCLCST' 'MWSKZ'.
**       MESSAGE W167 WITH 'Tax Code'.
    ELSE.
      SELECT SINGLE * FROM T007A
             WHERE KALSM EQ 'TAXKR'
             AND   MWSKZ EQ  IT_ZSCUCLCST-MWSKZ.
      IF SY-SUBRC NE 0.
        MESSAGE E495 WITH 'TAXKR' IT_ZSCUCLCST-MWSKZ.
      ENDIF.

    ENDIF.
  ENDIF.
*  IF IT_ZSCUCLCST-ZFWERKS IS INITIAL.       "> PLANT
*     IT_ZSCUCLCST-ZFWERKS  =    ZTBL-WERKS.
*  ENDIF.

* ===> TAX RATE
  IF IT_ZSCUCLCST-MWSKZ IS INITIAL.
    CLEAR : IT_ZSCUCLCST-KBETR, IT_ZSCUCLCST-KONWA, IT_ZSCUCLCST-ZFVAT.
  ELSE.
    PERFORM   P1000_READ_KONP   USING   IT_ZSCUCLCST-MWSKZ
                                        IT_ZSCUCLCST-KBETR
                                        IT_ZSCUCLCST-KONWA.
  ENDIF.

  IF NOT IT_ZSCUCLCST-ZFCAMT IS INITIAL.
    W_AMOUNT = IT_ZSCUCLCST-ZFCAMT.
*-----------------------------------------------------------------------
* TAX CODE  ===> 부가세 원단위 절사.
    IF IT_ZSCUCLCST-ZFVAT IS INITIAL.
      IF NOT IT_ZSCUCLCST-KBETR IS INITIAL.
        W_AMOUNT = W_AMOUNT * IT_ZSCUCLCST-KBETR / 10.
        COMPUTE W_AMOUNT = TRUNC( W_AMOUNT ).
        W_AMOUNT = W_AMOUNT * 10.
        IT_ZSCUCLCST-ZFVAT = W_AMOUNT.

        PERFORM    SET_CURR_CONV_TO_INTERNAL
                                   USING IT_ZSCUCLCST-ZFVAT
                                         'KRW'.
      ENDIF.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------------------
*>> 부대비용 정산후 금액 수정시 CHECK!
  CLEAR W_TOT_AMT.
  IF W_SY_SUBRC EQ 0 AND IT_ZSCUCLCST-ZFCSTYN = 'X'.
    W_TOT_AMT = IT_ZSCUCLCST-ZFUPCST.
    IF W_TOT_AMT > 0.
      IF IT_ZSCUCLCST-ZFCKAMT < W_TOT_AMT.
        MESSAGE E471 WITH IT_ZSCUCLCST-ZFCSQ.
      ELSEIF IT_ZSCUCLCST-ZFCKAMT > W_TOT_AMT.
        MESSAGE W472 WITH IT_ZSCUCLCST-ZFCSQ.
      ENDIF.
    ENDIF.
  ENDIF.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSCUCLCST  INDEX W_TABIX.
  ELSE.
*     IT_ZSCUCLCST-ZFCSQ   = ( TC_0813-CURRENT_LINE * 10 ).
    APPEND IT_ZSCUCLCST.
  ENDIF.

ENDMODULE.                 " GET_NAME_SCR7420  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_B/L_LISTING
*&---------------------------------------------------------------------*
FORM P2000_DATA_BL_LISTING.

  PERFORM P3000_ZTBL_TITLELIST.
  CASE INCLUDE.
    WHEN 'BLLST' OR 'POLST'.   "> 반입신고 (HOUSE B/L 입력시)
      LOOP AT IT_ZSREQHD.
        W_MOD = SY-TABIX MOD 2.
        PERFORM P2000_ZTBL_DUP_LIST_1.
      ENDLOOP.
      WRITE : / SY-ULINE(101).
    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " P2000_DATA_B/L_LISTING
*&---------------------------------------------------------------------*
*&      Form  P2000_READ_DATA_ZTBLINR_TMP
*&---------------------------------------------------------------------*
FORM P2000_READ_DATA_ZTBLINR_TMP.

  IF NOT ZSREQHD-EBELN IS INITIAL.
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZTBLINR_TMP
                      WHERE ZFREBELN = ZSREQHD-EBELN.
    CASE W_COUNT.
      WHEN 0.
        MESSAGE E661.
        EXIT.
      WHEN 1.  " 한건일때.
        SELECT  ZFTBLNO INTO ZSREQHD-ZFTBLNO UP TO 1 ROWS
          FROM  ZTBLINR_TMP
         WHERE  ZFREBELN = ZSREQHD-EBELN.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_BLINR_TMP_SELECT1.
        IF ANTWORT NE 'Y'.
          W_ERR_CHK = 'Y'.
          EXIT.
        ENDIF.
        ZSREQHD-ZFTBLNO = W_TBLNO.
        ZSREQHD-ZFBLNO =  W_BLNO.
    ENDCASE.
    W_TBLNO = ZSREQHD-ZFTBLNO.
    W_BLNO =  ZSREQHD-ZFBLNO.
    PERFORM P1000_SELECT_DATA_ZTBLINR.
    EXIT.
  ENDIF.
  IF  NOT ZSREQHD-ZFHBLNO IS INITIAL.
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZTBLINR_TMP
                      WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
    CASE W_COUNT.
      WHEN 0.
        MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
        EXIT.
      WHEN OTHERS.
        PERFORM P2000_BLINR_TMP_ITEM_SELECT2.
        IF ANTWORT NE 'Y'.  W_ERR_CHK = 'Y'.  EXIT. ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
        ZSREQHD-ZFTBLNO = W_TBLNO.
    ENDCASE.
    W_TBLNO = ZSREQHD-ZFTBLNO.
    W_BLNO  = ZSREQHD-ZFBLNO.
    PERFORM P1000_SELECT_DATA_ZTBLINR.
    EXIT.
  ENDIF.
  IF NOT ZSREQHD-ZFBLNO IS INITIAL.
    PERFORM P1000_READ_DATA_ZTBLINR_TMP.
    W_TBLNO = ZTBLINR_TMP-ZFTBLNO.
    EXIT.
  ENDIF.

ENDFORM.                    " P2000_READ_DATA_ZTBLINR_TMP
*&---------------------------------------------------------------------*
*&      Form  P2000_BLINR_TMP_SELECT1
*&---------------------------------------------------------------------*
FORM P2000_BLINR_TMP_SELECT1.

  W_TBLNO    = ZSREQHD-ZFTBLNO.
  W_EBELN    = ZSREQHD-EBELN.
  W_HBLNO    = ZSREQHD-ZFHBLNO.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZTBL
           WHERE  ZFREBELN EQ ZSREQHD-EBELN
           ORDER  BY ZFBLNO.
*>> 미참조 생성일 경우.
  LOOP AT IT_ZSREQHD.
    W_TABIX = SY-TABIX.
    CLEAR ZTBLINR_TMP.
    SELECT SINGLE *
       FROM ZTBLINR_TMP
      WHERE ZFREBELN EQ ZSREQHD-EBELN.

    MOVE: ZTBLINR_TMP-ZFREBELN TO IT_ZSREQHD-EBELN,
            ZTBLINR_TMP-ZFTBLNO  TO IT_ZSREQHD-ZFTBLNO,
            ZTBLINR_TMP-ZFSHNO   TO IT_ZSREQHD-ZFSHNO,
            ZTBLINR_TMP-ZFHBLNO   TO IT_ZSREQHD-ZFHBLNO,
            ZTBLINR_TMP-LIFNR    TO IT_ZSREQHD-LLIEF.
    MODIFY IT_ZSREQHD INDEX W_TABIX.
  ENDLOOP.
*>> 미참조 생성일 경우.
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL EQ 0.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
             FROM   ZTBLINR_TMP
             WHERE  ZFREBELN EQ ZSREQHD-EBELN
             ORDER  BY ZFBLNO.

  ENDIF.
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
  W_STATUS_CHK = 'U'.
  INCLUDE = 'POLST'.                 ">B/L 조회.
  CALL SCREEN 0114 STARTING AT  07   3
                   ENDING   AT  110  20.


ENDFORM.                    " P2000_BLINR_TMP_SELECT1
*&---------------------------------------------------------------------*
*&      Form  P2000_BLINR_TMP_ITEM_SELECT2
*&---------------------------------------------------------------------*
FORM P2000_BLINR_TMP_ITEM_SELECT2.

  W_TBLNO    = ZSREQHD-ZFTBLNO.
  W_BLNO     = ZSREQHD-ZFBLNO.
  W_EBELN    = ZSREQHD-EBELN.
  W_HBLNO    = ZSREQHD-ZFHBLNO.

  REFRESH IT_ZSREQHD.
*Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZTBL
           WHERE  ZFHBLNO EQ ZSREQHD-ZFHBLNO
           ORDER  BY ZFBLNO.


  LOOP AT IT_ZSREQHD.
    W_TABIX = SY-TABIX.
    CLEAR ZTBLINR_TMP.
    SELECT SINGLE *
       FROM ZTBLINR_TMP
      WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.

    MOVE: ZTBLINR_TMP-ZFREBELN TO IT_ZSREQHD-EBELN,
          ZTBLINR_TMP-ZFTBLNO  TO IT_ZSREQHD-ZFTBLNO,
          ZTBLINR_TMP-ZFSHNO   TO IT_ZSREQHD-ZFSHNO,
          ZTBLINR_TMP-ZFHBLNO  TO IT_ZSREQHD-ZFHBLNO,
          ZTBLINR_TMP-LIFNR    TO IT_ZSREQHD-LLIEF.

    MODIFY IT_ZSREQHD INDEX W_TABIX.
  ENDLOOP.

  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
        FROM ZTBLINR_TMP
         WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO
         ORDER  BY ZFBLNO.
    DESCRIBE TABLE IT_ZSREQHD LINES W_LINE.
    IF W_LINE EQ 0.
      MESSAGE E046.
    ENDIF.
  ENDIF.
  W_STATUS_CHK = 'U'.
  INCLUDE = 'BLLST'.                 ">B/L 조회.
  CALL SCREEN 0114 STARTING AT  07 3
                   ENDING   AT  110 20.


ENDFORM.                    " P2000_BLINR_TMP_ITEM_SELECT2
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA_ZTBLINR_TMP
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA_ZTBLINR_TMP.

  IF ZSREQHD-ZFBTSEQ IS INITIAL.
    SELECT MAX( ZFBTSEQ ) INTO ZSREQHD-ZFBTSEQ
       FROM ZTBLINR_TMP
       WHERE ZFBLNO = ZSREQHD-ZFBLNO.
  ENDIF.
  REFRESH IT_ZSBLIT.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBLIT
        FROM ZTBLIT
        WHERE ZFBLNO = ZSREQHD-ZFBLNO.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZTBLINR_TMP
           WHERE  ZFBLNO EQ ZSREQHD-ZFBLNO
           ORDER  BY ZFBLNO.

  SELECT SINGLE *
         FROM ZTBLINR_TMP
        WHERE ZFBLNO  = ZSREQHD-ZFBLNO
          AND ZFBTSEQ = ZSREQHD-ZFBTSEQ.
   *ZTBLINR_TMP = ZTBLINR_TMP.
  IF SY-SUBRC NE 0.
    MESSAGE E446 WITH  ZSREQHD-ZFBLNO ZSREQHD-ZFBTSEQ .
  ENDIF.
  W_TOWTM = ZTBLINR_TMP-ZFTOWTM.
  W_PKCNM = ZTBLINR_TMP-ZFPKCNM.
*구매처명
  PERFORM  P1000_GET_VENDOR   USING  ZTBLINR_TMP-LIFNR
                           CHANGING  LFA1-NAME1.

ENDFORM.                    " P1000_READ_DATA_ZTBLINR_TMP
*&---------------------------------------------------------------------*
*&      Form  P2000_READ_DATA_ZTBL
*&---------------------------------------------------------------------*
FORM P2000_READ_DATA_ZTBL.

*>> 생성시.
  IF NOT ZSREQHD-EBELN IS INITIAL.
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZTBL
                      WHERE ZFREBELN = ZSREQHD-EBELN.

    CASE W_COUNT.
      WHEN 0.
        MESSAGE W661.
        EXIT.
      WHEN 1.  " 한건일때.
        PERFORM P2000_GET_BLNO.
      WHEN OTHERS.
        PERFORM P2000_BL_DOC_ITEM_SELECT1.
        IF ANTWORT NE 'Y'.
          W_ERR_CHK = 'Y'.
          EXIT.
        ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
    W_BLNO = ZSREQHD-ZFBLNO.
    PERFORM P1000_SELECT_DATA_ZTBLINR.
    EXIT.
  ENDIF.
  IF  NOT ZSREQHD-ZFHBLNO IS INITIAL.
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZTBL
                      WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
    CASE W_COUNT.
      WHEN 0.
        MESSAGE W305 WITH ZSREQHD-ZFHBLNO.
        MOVE    ZSREQHD-ZFHBLNO TO ZTBLINR_TMP-ZFHBLNO.
        EXIT.
      WHEN OTHERS.
        PERFORM P2000_BL_DOC_ITEM_SELECT2.
        IF ANTWORT NE 'Y'.  W_ERR_CHK = 'Y'.  EXIT. ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
    PERFORM P1000_SELECT_DATA_ZTBLINR.
    EXIT.
  ENDIF.
  IF NOT ZSREQHD-ZFBLNO IS INITIAL.
    PERFORM P2000_CHECK_ZTBLINR_ZTBLIOU.
    PERFORM P1000_READ_DATA_ZTBL.
    W_BLNO = ZSREQHD-ZFBLNO.
    EXIT.
  ENDIF.


ENDFORM.                    " P2000_READ_DATA_ZTBL
*&---------------------------------------------------------------------*
*&      Form  P2000_LC_DOC_DISPLAY1
*&---------------------------------------------------------------------*
FORM P2000_LC_DOC_DISPLAY1 USING  P_ZFREBELN.

  SELECT MAX( ZFREQNO ) INTO W_ZFREQNO
         FROM ZTREQHD
         WHERE EBELN = P_ZFREBELN.
  SELECT MAX( ZFAMDNO ) INTO W_ZFAMDNO
         FROM ZTREQST
         WHERE ZFREQNO = W_ZFREQNO.

  SET PARAMETER ID 'BES'       FIELD ''.
  SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
  SET PARAMETER ID 'ZPREQNO'   FIELD  W_ZFREQNO.
  SET PARAMETER ID 'ZPAMDNO'   FIELD  W_ZFAMDNO.

  IF W_ZFAMDNO = '00000'.
    CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
  ELSE.
    CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
  ENDIF.

ENDFORM.                    " P2000_LC_DOC_DISPLAY1
*&---------------------------------------------------------------------*
*&      Form  P2000_MODIFY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_MODIFY_CHECK USING    W_MODIF_BIT.

  W_MODIF_BIT = 'N'.

  CASE SY-TCODE.
    WHEN 'ZIM22' OR 'ZIM23' OR 'ZIM221' OR 'ZIM222' OR 'ZIM223'.
      PERFORM  P2000_BL_DATA_MODIFY_CHECK.
    WHEN OTHERS.
      W_MODIF_BIT = 'Y'.
      EXIT.
  ENDCASE.

ENDFORM.                    " P2000_MODIFY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_BLCOST_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_BLCOST_POST.
  W_OK_CODE = OK-CODE.
*-----------------------------------------------------------------------
* AUTHORITY CHECK
  PERFORM P2000_AUTHORITY_CHECK USING W_OK_CODE.
*-----------------------------------------------------------------------
* Lock Object Set
  IF W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D.
    PERFORM P2000_SET_LOCK.
  ENDIF.
* MESSAGE BOX
  IF W_OK_CODE EQ 'POST' AND W_STATUS EQ C_REQ_D.
    ANTWORT = 'Y'.
  ELSE.
*> Modify Yes/No Check
    PERFORM P2000_MODIFY_CHECK USING W_MODIF_BIT.
    IF W_MODIF_BIT EQ 'Y'.
      PERFORM P2000_SET_MESSAGE USING  OK-CODE.
    ELSE.
      ANTWORT = 'Y'.
    ENDIF.
  ENDIF.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      IF W_STATUS    NE C_REQ_D AND
         W_MODIF_BIT EQ 'Y'.
        PERFORM  P3000_DB_MODIFY_SCRCOM.
      ENDIF.
      PERFORM  P2000_POST_DATA_MOVE.
      PERFORM  P2000_SET_UNLOCK.
** Changed by Furong on 12/04/09
      IF SY-TCODE = 'ZIM22'.
        DATA: L_ANSWER(1).

        CALL FUNCTION 'POPUP_WITH_2_BUTTONS_TO_CHOOSE'
          EXPORTING
*             DEFAULTOPTION       = '1'
            DIAGNOSETEXT1       = ' '
             DIAGNOSETEXT2       = 'Do you want to process insurance?'
*             DIAGNOSETEXT3       = ' '
            TEXTLINE1           = ' '
*             TEXTLINE2           = ' '
*             TEXTLINE3           = ' '
            TEXT_OPTION1        = 'Yes'
            TEXT_OPTION2        = 'No'
            TITEL               = ' '
         IMPORTING
           ANSWER              = L_ANSWER
                  .
        IF L_ANSWER = '1'.
          CALL TRANSACTION 'ZIMY1'.
        ENDIF.
      ENDIF.
** End of change.

      PERFORM  P2000_SET_SCREEN_SCRCOM.
      LEAVE SCREEN.
      EXIT.
    WHEN 'N'.              " No...
  ENDCASE.

ENDFORM.                    " P3000_BLCOST_POST
