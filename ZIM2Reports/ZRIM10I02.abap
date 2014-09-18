*----------------------------------------------------------------------*
*   INCLUDE ZRIM10I02                                                  *
*----------------------------------------------------------------------*
*&  프로그램명 : [Include] 수입 수송 Main PAI MODULE(2)                *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.09.24                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0103 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0103-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0103  INPUT

*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0104 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0104-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET2_LINE_SCR0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET2_LINE_SCR0104 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE2 = TC_0104_2-CURRENT_LINE + LINE2 - 1.

ENDMODULE.                 " IT_GET2_LINE_SCR0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSTRIT_UPDATE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSTRIT_UPDATE_SCR0103 INPUT.

* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK NOT ZSTRIT-ZFIVNO IS INITIAL.
* Internal Table Read
  READ TABLE IT_ZSTRIT INDEX TC_0103-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

*>> 출고 수량.
  IF ZSTRIT-GIMENGE IS INITIAL.
    MESSAGE S083(ME) WITH 'Bonded warehouse G/I quantity' SPACE.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSTRIT' 'GIMENGE'.
  ELSE.
    CLEAR : W_OLD_MENGE.

    SELECT SINGLE GIMENGE INTO W_OLD_MENGE
           FROM   ZTTRIT
           WHERE  ZFTRNO   EQ   ZSTRIT-ZFTRNO
           AND    ZFTRIT   EQ   ZSTRIT-ZFTRIT.

    W_MENGE = ZSTRIT-CCMENGE - ZSTRIT-ZFTRTOT + W_OLD_MENGE.
    W_MENGE1 = W_MENGE - ZSTRIT-GIMENGE.
    IF W_MENGE1 < 0 AND ZSTRIT-GIMENGE > 0.
      MESSAGE E301(ZIM1) WITH W_MENGE ZSTRIT-MEINS.
    ENDIF.
  ENDIF.

*>>> 기본 단위..
  IF ZSTRIT-MEINS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSTRIT' 'MEINS'.
  ENDIF.

*>>> Storage Location.
*  IF ZSTRIT-LGORT IS INITIAL.
*    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSTRIT' 'LGORT'.
*  ENDIF.

  MOVE : ZSTRIT-GIMENGE TO IT_ZSTRIT-GIMENGE,
         ZSTRIT-MEINS   TO IT_ZSTRIT-MEINS,
         ZSTRIT-LGORT   TO IT_ZSTRIT-LGORT.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSTRIT   INDEX W_TABIX.
  ELSE.
    APPEND IT_ZSTRIT.
    CLEAR  IT_ZSTRIT.
  ENDIF.

ENDMODULE.                 " IT_ZSTRIT_UPDATE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0103_MARK_TC_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0103_MARK_TC_0103 INPUT.

  READ TABLE IT_ZSTRIT  INDEX TC_0103-CURRENT_LINE.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSTRIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSTRIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSTRIT INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0103_MARK_TC_0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0023_MARK_TC_0023  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0023_MARK_TC_0023 INPUT.

  READ TABLE IT_ZSTRCSTIT  INDEX TC_0023-CURRENT_LINE.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK1 IS INITIAL ).
      IT_ZSTRCSTIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSTRCSTIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSTRCSTIT INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0023_MARK_TC_0023  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0104_MARK_TC_0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0104_MARK_TC_0104 INPUT.

  READ TABLE IT_ZSTRCST  INDEX TC_0104-CURRENT_LINE.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSTRCST-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSTRCST-ZFMARK.
    ENDIF.
    MODIFY IT_ZSTRCST INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0104_MARK_TC_0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0104_MARK_TC_0104_2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0104_MARK_TC_0104_2 INPUT.

  READ TABLE IT_ZSBSEG  INDEX TC_0104_2-CURRENT_LINE.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK2 IS INITIAL ).
      IT_ZSBSEG-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSBSEG-ZFMARK.
    ENDIF.
    MODIFY IT_ZSBSEG INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0104_MARK_TC_0104_2  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_CHECK_ZFTOWT_SCR0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_CHECK_ZFTOWT_SCR0103 INPUT.

*>>> 총중량.
  IF ZTTRHD-ZFTOWT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTRHD' 'ZFTOWT'.
  ENDIF.

ENDMODULE.                 " IT_CHECK_ZFTOWT_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0103 INPUT.

  W_OK_CODE = SY-UCOMM.

  IF SY-UCOMM EQ 'MKA1'.   " 전체 선택.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1'.  " 선택 해제.
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1'.
      IF SY-DYNNR = '0103'.
        LOOP AT IT_ZSTRIT WHERE ZFMARK NE SPACE.
          DELETE IT_ZSTRIT INDEX SY-TABIX.
        ENDLOOP.
      ELSEIF SY-DYNNR = '0104'.
        LOOP AT IT_ZSTRCST WHERE ZFMARK NE SPACE.
          W_TABIX = SY-TABIX.
*>        산출내역에서도 지우기.
          IF IT_ZSTRCST-ZFSEQ NE 0.
            LOOP AT IT_ZTTRCSTIT WHERE ZFSEQ = IT_ZSTRCST-ZFSEQ.
              DELETE IT_ZTTRCSTIT INDEX SY-TABIX.
            ENDLOOP.
          ENDIF.
          DELETE IT_ZSTRCST INDEX W_TABIX.
        ENDLOOP.
        PERFORM CALCUL_SUM_TRANSCOST.
      ENDIF.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해제.
      IF SY-DYNNR = '0103'.
        LOOP AT IT_ZSTRIT.
          IT_ZSTRIT-ZFMARK = W_MARK.
          MODIFY IT_ZSTRIT INDEX SY-TABIX.
        ENDLOOP.
      ELSEIF SY-DYNNR = '0104'.
        LOOP AT IT_ZSTRCST.
          IT_ZSTRCST-ZFMARK = W_MARK.
          MODIFY IT_ZSTRCST INDEX SY-TABIX.
        ENDLOOP.
      ENDIF.
    WHEN 'SU01' OR 'PODP' OR 'IMDP' OR 'BLDP' OR 'IDDP'.   " 문서조회.
      PERFORM   P2000_LINE_SELECT_TR_ITEM.
      PERFORM   P2000_LINE_CALL_TCODE_TR_ITEM.
*    WHEN 'CALC'.
*      PERFORM   P2000_COST_CALCULATE.
    WHEN 'DET'. " 산출내역 화면띄우기.
      PERFORM   P2000_CALL_DETAIL_SCR23.
    WHEN 'REF'.
      PERFORM   P1000_READ_CHARGE_RECORD.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  IDS_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IDS_DATA_CHECK INPUT.

* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

* Internal Table Read
  READ TABLE IT_ZSTRIT   INDEX TC_0103-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

*>> B/L관리 번호가 입력되지 않았을 경우.
  IF ZSTRIT-ZFBLNO IS INITIAL.
    MESSAGE W304(ZIM1).   EXIT.
  ELSE.
*>> B/L관리 번호가 입력되었을 경우.
    SELECT SINGLE * FROM ZTBL
                   WHERE ZFBLNO EQ ZSTRIT-ZFBLNO.
    IF SY-SUBRC NE 0.
      MESSAGE W302(ZIM1).
      CLEAR : ZSTRIT.
      EXIT.
    ENDIF.

    SELECT SINGLE * FROM  ZTIV
                    WHERE ZFBLNO EQ ZSTRIT-ZFBLNO.
    IF SY-SUBRC NE 0
       OR ZTIV-ZFCUST NE 'Y'.
      MESSAGE W303(ZIM1).
      CLEAR : ZSTRIT.
      EXIT.
    ENDIF.

*>> B/L ITEM번호가 입력되었을 경우.
    IF NOT ZSTRIT-ZFBLIT IS INITIAL.
*-----------------------------------------------------------------------
* 기존에 해당 B/L 아이템이 입력되었을 경우 체크.
      READ TABLE  IT_ZSTRIT WITH KEY ZFBLNO  = ZSTRIT-ZFBLNO
                                     ZFBLIT = ZSTRIT-ZFBLIT.
      IF SY-SUBRC EQ 0 .
        MESSAGE S358 WITH ZSTRIT-ZFBLNO ZSTRIT-ZFBLIT
                          IT_ZSTRIT-ZFTRIT.
        CLEAR : ZSTRIT, IT_ZSTRIT.
        EXIT.
      ENDIF.
*-----------------------------------------------------------------------
      MOVE-CORRESPONDING ZSTRIT  TO IT_ZSTRIT.
*----> B/L ITEM번호가 입력되었을 경우, 발췌하여 메세지.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF IT_ZSTRIT
             FROM   ZTIVIT
            WHERE  ZFBLNO  EQ ZSTRIT-ZFBLNO
             AND   ZFBLIT  EQ ZSTRIT-ZFBLIT.

      IF SY-SUBRC NE 0.
        MESSAGE W357 WITH ZSTRIT-ZFBLNO ZSTRIT-ZFBLIT.
        CLEAR : ZSTRIT, IT_ZSTRIT.
        EXIT.
      ENDIF.

*>> P/O DATA 조회.
      SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                    WERKS LGORT MATKL BPUMZ BPUMN
             INTO (IT_ZSTRIT-MENGE_PO, IT_ZSTRIT-UEBTO,
                   IT_ZSTRIT-UEBTK,    IT_ZSTRIT-WEPOS,
                   IT_ZSTRIT-ELIKZ,    IT_ZSTRIT-LOEKZ,
                   IT_ZSTRIT-UNTTO,    IT_ZSTRIT-WERKS,
                   IT_ZSTRIT-LGORT,    IT_ZSTRIT-MATKL,
                   IT_ZSTRIT-BPUMZ,    IT_ZSTRIT-BPUMN)
             FROM   EKPO
             WHERE  EBELN   EQ   IT_ZSTRIT-EBELN
             AND    EBELP   EQ   IT_ZSTRIT-EBELP.

      IF SY-SUBRC EQ 0.
        IF IT_ZSTRIT-LOEKZ NE SPACE.
          MESSAGE W069 WITH IT_ZSTRIT-EBELN IT_ZSTRIT-EBELP.
          CLEAR : ZSTRIT.
          EXIT.
        ENDIF.
        IF IT_ZSTRIT-ELIKZ EQ 'X'.
          MESSAGE W359 WITH IT_ZSTRIT-EBELN IT_ZSTRIT-EBELP.
          CLEAR : ZSTRIT.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE W071 WITH IT_ZSTRIT-EBELN IT_ZSTRIT-ZFITMNO.
*        CLEAR : ZSTRIT.
*        EXIT.
      ENDIF.

*>>> 수입의뢰 수량.
      IF NOT IT_ZSTRIT-ZFREQNO IS INITIAL.

        SELECT SINGLE MENGE KBETR KWERT KPEIN KMEIN
               INTO (IT_ZSTRIT-MENGE, IT_ZSTRIT-KBETR,
                     IT_ZSTRIT-KWERT, IT_ZSTRIT-KPEIN,
                     IT_ZSTRIT-KMEIN)
               FROM  ZTREQIT
              WHERE  ZFREQNO EQ IT_ZSTRIT-ZFREQNO
                AND  ZFITMNO EQ IT_ZSTRIT-ZFITMNO.

*>>   수입승인번호.
        SELECT SINGLE ZFOPNNO INTO IT_ZSTRIT-ZFOPNNO
               FROM   ZTREQHD
               WHERE  ZFREQNO EQ IT_ZSTRIT-ZFREQNO.

      ENDIF.
*>>> B/L 수량 .
      IF NOT IT_ZSTRIT-ZFBLNO IS INITIAL.
        SELECT SINGLE BLMENGE INTO IT_ZSTRIT-BLMENGE
                  FROM ZTBLIT
                 WHERE ZFBLNO EQ IT_ZSTRIT-ZFBLNO
                   AND ZFBLIT EQ IT_ZSTRIT-ZFBLIT.

        READ TABLE IT_ZSTRCST WITH KEY ZFBLNO = IT_ZSTRIT-ZFBLNO.
        IF SY-SUBRC NE 0.
          CLEAR IT_ZSTRCST.
          MOVE IT_ZSTRIT-ZFBLNO TO IT_ZSTRCST-ZFBLNO.
          APPEND IT_ZSTRCST.
        ENDIF.
      ENDIF.

*>>> 하역수량.
      IF NOT IT_ZSTRIT-ZFCGNO IS INITIAL.
        SELECT SINGLE CGMENGE  INTO IT_ZSTRIT-CGMENGE
               FROM  ZTCGIT
               WHERE ZFCGNO   EQ IT_ZSTRIT-ZFCGNO
               AND   ZFCGIT   EQ IT_ZSTRIT-ZFCGIT
               AND   CGLOEKZ  NE 'X'.
      ENDIF.

*>>> 수송 자재내역(기입력건)
      IF NOT IT_ZSTRIT-ZFIVNO IS INITIAL.
        SELECT SUM( GIMENGE ) INTO IT_ZSTRIT-ZFTRTOT
                    FROM  ZTTRIT
                    WHERE ZFIVNO EQ IT_ZSTRIT-ZFIVNO
                    AND   ZFIVDNO EQ IT_ZSTRIT-ZFIVDNO.
      ENDIF.
*>>>수송 가능 수량.
      IT_ZSTRIT-GIMENGE = IT_ZSTRIT-CCMENGE - IT_ZSTRIT-ZFTRTOT.
      IF IT_ZSTRIT-GIMENGE LE 0.
        IT_ZSTRIT-GIMENGE = 0.
      ENDIF.
      IT_ZSTRIT-EBELP = IT_ZSTRIT-EBELP.

*>>> 선적차수.
      SELECT MAX( ZFSHNO ) INTO IT_ZSTRIT-ZFSHNO
             FROM  ZTBLIT
             WHERE EBELN EQ IT_ZSTRIT-EBELN
             AND   EBELP EQ IT_ZSTRIT-EBELP.

*----> 마지막에 추가.
      IF W_SY_SUBRC EQ 0.
        MODIFY IT_ZSTRIT  INDEX W_TABIX.
      ELSE.
        MOVE : SY-UNAME    TO   IT_ZSTRIT-ERNAM,
               SY-DATUM    TO   IT_ZSTRIT-CDAT,
               SY-UNAME    TO   IT_ZSTRIT-UNAM,
               SY-DATUM    TO   IT_ZSTRIT-UDAT.
        APPEND IT_ZSTRIT.
      ENDIF.
    ENDIF.
    CLEAR IT_ZSTRIT.
  ENDIF.

ENDMODULE.                 " IDS_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  BL_DATA_CHECK_SCR0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BL_DATA_CHECK_SCR0104 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

*>> B/L관리 번호가 입력되지 않았을 경우.
  IF ZSTRCST-ZFBLNO IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTRCST' 'ZFBLNO'.
  ELSE.
*>> B/L관리 번호가 입력되었을 경우.
*--> 아이템 내역에 있는 B/L인지 체크.
    READ TABLE IT_ZSTRIT WITH KEY ZFBLNO = ZSTRCST-ZFBLNO.

    IF SY-SUBRC NE 0.
      MESSAGE E321(ZIM1) WITH ZSTRCST-ZFBLNO.
      CLEAR : ZSTRCST.
      EXIT.
    ENDIF.
  ENDIF.
ENDMODULE.                 " BL_DATA_CHECK_SCR0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  BL_DATA_CHECK_SCR0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PLANT_DATA_CHECK_SCR0104 INPUT.

*--> PLANT가 입력되지 않았을 경우.
  IF ZSTRCST-WERKS IS INITIAL.
    SELECT SINGLE ZFWERKS
                  INTO ZSTRCST-WERKS
                  FROM ZTBL
                 WHERE ZFBLNO = ZSTRCST-ZFBLNO.
  ENDIF.
*----->PLANT 가 입력되었을 경우.
*--> 아이템 내역에 있는 B/L의 PLANT와 같은지 CHECK.
  READ TABLE IT_ZSTRIT WITH KEY ZFBLNO = ZSTRCST-ZFBLNO
                                WERKS  = ZSTRCST-WERKS.

  IF SY-SUBRC NE 0.
    MESSAGE E322(ZIM1) WITH ZSTRCST-WERKS.
    EXIT.
  ENDIF.

ENDMODULE.                 " PLANT_DATA_CHECK_SCR0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  CAPACITY_DATA_CHECK_SCR0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CAPACITY_DATA_CHECK_SCR0104 INPUT.

* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

*--> 가로가 입력되지 않았을 경우.
  IF ZSTRCST-ZFGARO IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTRCST' 'ZFGARO'.
  ENDIF.

*--> 세로가 입력되지 않았을 경우.
  IF ZSTRCST-ZFSERO IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTRCST' 'ZFSERO'.
  ENDIF.

*--> 높이가 입력되지 않았을 경우.
  IF ZSTRCST-ZFNOPI IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTRCST' 'ZFNOPI'.
  ENDIF.

ENDMODULE.                 " CAPACITY_DATA_CHECK_SCR0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZTTRCST_CALCULATE_SCR0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZTTRCST_CALCULATE_SCR0104 INPUT.

* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

*-> 실용적 계산. (가로*세로*높이/1000000)
  IF NOT ZSTRCST-ZFGARO IS INITIAL AND
     NOT ZSTRCST-ZFSERO IS INITIAL AND
     NOT ZSTRCST-ZFNOPI IS INITIAL.
    ZSTRCST-ZFRWET = ( ZSTRCST-ZFGARO * ZSTRCST-ZFSERO
                             * ZSTRCST-ZFNOPI ) / 1000000.
  ELSE.
    EXIT.
  ENDIF.

*--> 용적톤 계산. ( 실용적/3.571)
  ZSTRCST-ZFYTON =  ZSTRCST-ZFRWET / ( 3571 / 1000 ) .

*--> 계산톤 (용적톤과 중량톤 중 값이 큰것)
  IF ZSTRCST-ZFYTON GT ZSTRCST-ZFWTON . " 용적톤이 클때.
    ZSTRCST-ZFCTON = ZSTRCST-ZFYTON.
  ELSE.                                 " 중량톤이 클때.
    ZSTRCST-ZFCTON = ZSTRCST-ZFWTON.
  ENDIF.

*--> 단가 가져오기.
  SELECT SINGLE ZFNETPR WAERS INTO (ZSTRCST-NETPR, ZSTRCST-WAERS)
            FROM ZTIMIMG20
           WHERE ZFYEAR  =  ZTTRHD-ZFGIDT(4)
             AND WERKS   =  ZSTRCST-WERKS
             AND ZFTRGB  =  ZTTRHD-ZFTRGB
             AND ZFWTFR  <  ZSTRCST-ZFCTON
             AND ZFWTTO  >=  ZSTRCST-ZFCTON.

  IF SY-SUBRC NE 0.
    CLEAR ZSTRCST-NETPR.
    MESSAGE W323(ZIM1) WITH ZTTRHD-ZFGIDT(4) ZSTRCST-WERKS
                            ZTTRHD-ZFTRGB    ZSTRCST-ZFCTON .
  ENDIF.

** 할증은 단위가 [할]이므로 계산시에는 할증을 10으로 나눠야 함.
**--> 운반비 계산. (단가 * (1 + 운반비할증/10) * 계산톤 )
*  IF ZSTRCST-ZFTRAMT IS INITIAL.
*    ZSTRCST-ZFTRAMT = ZSTRCST-NETPR * ( 1 + ZSTRCST-ZFTADD / 10 )
*                                    * ZSTRCST-ZFCTON.
*  ENDIF.
**--> 인건비 계산. (단가 * (1 + 인건비할증/10) * 계산톤 )
*  IF ZSTRCST-ZFMAMT IS INITIAL.
*    ZSTRCST-ZFMAMT = ZSTRCST-NETPR * ( 1 + ZSTRCST-ZFMADD / 10 )
*                                   * ZSTRCST-ZFCTON.
*  ENDIF.

ENDMODULE.                 " ZTTRCST_CALCULATE_SCR0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZTTRCSTIT_CALCULATE_SCR0023  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZTTRCSTIT_CALCULATE_SCR0023 INPUT.

* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

*--> 단가, 할증 가져오기.
  MOVE : ZTTRCST-NETPR  TO ZSTRCSTIT-NETPR,
         ZTTRCST-WAERS  TO ZSTRCSTIT-WAERS,
         ZTTRCST-ZFTADD TO ZSTRCSTIT-ZFTADD,  " 운반비할증.
         ZTTRCST-ZFMADD TO ZSTRCSTIT-ZFMADD.  " 인건비할증.

* 할증은 단위가 [할]이므로 계산시에는 할증을 10으로 나눠야 함.
*--> 운반비 계산. (단가 * (1 + 운반비할증/10) * 계산톤 ) * 내역(%)
  IF ZSTRCSTIT-ZFTRAMT IS INITIAL.
    ZSTRCSTIT-ZFTRAMT = ZSTRCSTIT-NETPR
                             * ( 1 + ZSTRCSTIT-ZFTADD / 10 )
                             * ZSTRCSTIT-ZFDTON
                             * ( ZSTRCSTIT-ZFTRATE / 100 ).
  ENDIF.
*--> 인건비 계산. (단가 * (1 + 인건비할증/10) * 계산톤 ) * 내역(%)
  IF ZSTRCSTIT-ZFMAMT IS INITIAL.
    ZSTRCSTIT-ZFMAMT = ZSTRCSTIT-NETPR
                             * ( 1 + ZSTRCSTIT-ZFMADD / 10 )
                             * ZSTRCSTIT-ZFDTON
                             * ( ZSTRCSTIT-ZFTRATE / 100 ).
  ENDIF.

ENDMODULE.                 " ZTTRCSTIT_CALCULATE_SCR0023  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSTRCSTIT_UPDATE_SCR0023  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSTRCSTIT_UPDATE_SCR0023 INPUT.

* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK NOT ZSTRCSTIT-ZFTRATE IS INITIAL AND
        NOT ZSTRCSTIT-ZFDTON  IS INITIAL.

* Internal Table Read
  READ TABLE IT_ZSTRCSTIT INDEX TC_0023-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE : ZTTRCST-ZFSEQ      TO IT_ZSTRCSTIT-ZFSEQ,
*         ZSTRCSTIT-ZFITSEQ  TO IT_ZSTRCSTIT-ZFITSEQ,
         ZSTRCSTIT-ZFTRATE  TO IT_ZSTRCSTIT-ZFTRATE,
         ZSTRCSTIT-ZFDTON   TO IT_ZSTRCSTIT-ZFDTON,
         ZSTRCSTIT-NETPR    TO IT_ZSTRCSTIT-NETPR,
         ZSTRCSTIT-WAERS    TO IT_ZSTRCSTIT-WAERS,
         ZSTRCSTIT-ZFTADD   TO IT_ZSTRCSTIT-ZFTADD,
         ZSTRCSTIT-ZFMADD   TO IT_ZSTRCSTIT-ZFMADD,
         ZSTRCSTIT-ZFTRAMT  TO IT_ZSTRCSTIT-ZFTRAMT,
         ZSTRCSTIT-ZFMAMT   TO IT_ZSTRCSTIT-ZFMAMT,
         ZSTRCSTIT-UNAM     TO IT_ZSTRCSTIT-UNAM,
         ZSTRCSTIT-UDAT     TO IT_ZSTRCSTIT-UDAT.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSTRCSTIT   INDEX W_TABIX.
  ELSE.
    APPEND IT_ZSTRCSTIT.
    CLEAR  IT_ZSTRCSTIT.
  ENDIF.

ENDMODULE.                 " IT_ZSTRCSTIT_UPDATE_SCR0023  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSTRCST_UPDATE_SCR0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSTRCST_UPDATE_SCR0104 INPUT.

* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK NOT ZSTRCST-ZFBLNO IS INITIAL AND
        NOT ZSTRCST-WERKS  IS INITIAL.

  SELECT SINGLE ZFHBLNO  INTO ZSTRCST-ZFHBLNO
                FROM ZTBL
               WHERE ZFBLNO = ZSTRCST-ZFBLNO.

* Internal Table Read
  READ TABLE IT_ZSTRCST INDEX TC_0104-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE : ZSTRCST-ZFBLNO   TO IT_ZSTRCST-ZFBLNO,
         ZSTRCST-ZFHBLNO  TO IT_ZSTRCST-ZFHBLNO,
         ZSTRCST-WERKS    TO IT_ZSTRCST-WERKS,
         ZSTRCST-ZFGARO   TO IT_ZSTRCST-ZFGARO,
         ZSTRCST-ZFSERO   TO IT_ZSTRCST-ZFSERO,
         ZSTRCST-ZFNOPI   TO IT_ZSTRCST-ZFNOPI,
         ZSTRCST-ZFRWET   TO IT_ZSTRCST-ZFRWET,
         ZSTRCST-ZFYTON   TO IT_ZSTRCST-ZFYTON,
         ZSTRCST-ZFWTON   TO IT_ZSTRCST-ZFWTON,
         ZSTRCST-ZFCTON   TO IT_ZSTRCST-ZFCTON,
         ZSTRCST-NETPR    TO IT_ZSTRCST-NETPR,
         ZSTRCST-WAERS    TO IT_ZSTRCST-WAERS,
         ZSTRCST-ZFTADD   TO IT_ZSTRCST-ZFTADD,
         ZSTRCST-ZFMADD   TO IT_ZSTRCST-ZFMADD,
         ZSTRCST-ZFTRAMT  TO IT_ZSTRCST-ZFTRAMT,
         ZSTRCST-ZFMAMT   TO IT_ZSTRCST-ZFMAMT.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSTRCST   INDEX W_TABIX.
  ELSE.
    APPEND IT_ZSTRCST.
    CLEAR  IT_ZSTRCST.
  ENDIF.

ENDMODULE.                 " IT_ZSTRCST_UPDATE_SCR0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALCUL_SUM_SCR0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CALCUL_SUM_SCR0104 INPUT.

  PERFORM CALCUL_SUM_TRANSCOST.
ENDMODULE.                 " CALCUL_SUM_SCR0104  INPUT

*&---------------------------------------------------------------------*
*&      Module  IT_ZSBSEG_UPDATE_SCR0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSBSEG_UPDATE_SCR0104 INPUT.

* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

* Internal Table Read
  READ TABLE IT_ZSBSEG  INDEX TC_0104_2-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC EQ 0 AND ZSBSEG IS INITIAL.
    EXIT.
  ENDIF.

*> MOVE.
  MOVE-CORRESPONDING  ZSBSEG  TO   IT_ZSBSEG.

*> INPUT VALUE CHECK.
*> 관리번호.
  IF IT_ZSBSEG-ZFCD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'ZFCD'.
  ELSE.
    IF ZSBLCST-ZFCDNM IS INITIAL.
      SELECT SINGLE ZFCDNM
             INTO IT_ZSBSEG-ZFCDNM
             FROM ZTIMIMG08
             WHERE ZFCDTY EQ '009'
             AND   ZFCD   EQ IT_ZSBSEG-ZFCD.
      IF SY-SUBRC NE 0.
        MESSAGE E909 WITH IT_ZSBSEG-ZFCD.
      ENDIF.
    ENDIF.
  ENDIF.

*> 지정.
  IF IT_ZSBSEG-ZUONR IS INITIAL.
    IT_ZSBSEG-ZUONR = IT_ZSBSEG-ZFDCNM.
  ENDIF.
*> 품목 TEXT.
  IF IT_ZSBSEG-SGTXT IS INITIAL.
    IT_ZSBSEG-SGTXT = IT_ZSBSEG-ZFCDNM.
  ENDIF.


  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSBSEG INDEX W_TABIX.
  ELSE.
    APPEND IT_ZSBSEG.
  ENDIF.

ENDMODULE.                 " IT_ZSBSEG_UPDATE_SCR0104  INPUT

*&----------------------------------------------------------------------
*&      Module  PERIOD_CHECK_SCR0020  INPUT
*&----------------------------------------------------------------------
MODULE PERIOD_CHECK_SCR0020 INPUT.
  DATA: S_MONAT LIKE BKPF-MONAT.      "Save field for input period

*> 증빙일.
  IF ZTBKPF-BLDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BLDAT'.
  ENDIF.
  IF ZTBKPF-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUDAT'.
  ENDIF.

  IF ZTBKPF-ZFBDT IS INITIAL.
    ZTBKPF-ZFBDT = ZTBKPF-BUDAT.
  ENDIF.

  CLEAR ZTBKPF-GJAHR.
  S_MONAT = ZTBKPF-MONAT.

  PERFORM PERIODE_ERMITTELN(SAPMZIM02) USING ZTBKPF-BUDAT
                                             ZTBKPF-GJAHR
                                             ZTBKPF-MONAT.

  IF NOT S_MONAT IS INITIAL
  AND    S_MONAT NE ZTBKPF-MONAT
  AND ( SY-BINPT = SPACE AND SY-CALLD = SPACE ).
    MESSAGE W000(F5) WITH S_MONAT ZTBKPF-BUDAT ZTBKPF-MONAT.
  ENDIF.

ENDMODULE.                 " PERIOD_CHECK_SCR0020  INPUT
*&----------------------------------------------------------------------
*&      Module  BELEGART_CHECK_SCR0020  INPUT
*&----------------------------------------------------------------------
MODULE BELEGART_CHECK_SCR0020 INPUT.

  IF ZTBKPF-BLART IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BLART'.
  ENDIF.
*> 문서 종류 체크.
  PERFORM BELEGART_PRUEFEN(SAPFF001)
          USING ZTBKPF-BLART ZTBKPF-GJAHR.
*> 전기년도 체크.
  BKPF-BUKRS = ZTBKPF-BUKRS.
  PERFORM NUMMERNKREIS_LESEN(SAPFF001)
          USING ZTBKPF-GJAHR.


ENDMODULE.                 " BELEGART_CHECK_SCR0020  INPUT
*&----------------------------------------------------------------------
*&      Module  VENDOR_ACCOUNT_CHECK_SCR0020  INPUT
*&----------------------------------------------------------------------
MODULE VENDOR_ACCOUNT_CHECK_SCR0020 INPUT.

*   IF ZTBKPF-BUPLA IS INITIAL.    ">
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUPLA'.
*   ENDIF.
*
*   IF ZTBKPF-GSBER IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'GSBER'.
*   ENDIF.

  IF ZTBKPF-LIFNR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'LIFNR'.
  ENDIF.

  CALL FUNCTION 'FI_POSTING_KEY_DATA'
    EXPORTING
      I_BSCHL       = '31'
      I_UMSKZ       = SPACE  ">bseg-umskz
    IMPORTING
      E_T074U       = T074U
      E_TBSL        = TBSL
      E_TBSLT       = TBSLT
    EXCEPTIONS
      ERROR_MESSAGE = 1.

* 1. PBO: no message if bschl request umskz
  IF SY-SUBRC = 1.
* (del) if not ( firstcall = 'X'                           "Note 352492
    IF TBSL-XSONU NE SPACE.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH
              SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

*>> VENDOR MASTER DEFINE.
  CLEAR : *LFA1.
  SELECT SINGLE * INTO *LFA1 FROM LFA1
                  WHERE LIFNR EQ ZTBKPF-LIFNR.
  IF SY-SUBRC NE 0.
    MESSAGE E023 WITH ZTBKPF-LIFNR.
  ENDIF.

  CALL FUNCTION 'FI_VENDOR_DATA'
    EXPORTING
      I_BUKRS = ZTBKPF-BUKRS
      I_LIFNR = ZTBKPF-LIFNR
    IMPORTING
      E_KRED  = VF_KRED.

*    IF ZTBKPF-ZTERM IS INITIAL.
*       ZTBKPF-ZTERM = VF_KRED-ZTERM.
*    ELSEIF ZTBKPF-ZTERM NE VF_KRED-ZTERM.
  IF ZTBKPF-ZTERM NE VF_KRED-ZTERM.
    IF ZTBKPF-ZTERM IS INITIAL.
      ZTBKPF-ZTERM = VF_KRED-ZTERM.
    ELSE.
      MESSAGE W574 WITH  ZTBKPF-LIFNR VF_KRED-ZTERM ZTBKPF-ZTERM.
    ENDIF.
  ENDIF.

*    if lfb1-bukrs is initial.
*       move-corresponding vf_kred to lfa1.
*    else.
*       move-corresponding vf_kred to lfa1.
*       move-corresponding vf_kred to lfb1.
*       lfb1-sperr = vf_kred-sperr_b.
*       lfb1-loevm = vf_kred-loevm_b.
*       lfb1-begru = vf_kred-begru_b.
*   endif.

  IF ZTBKPF-MWSKZ IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'MWSKZ'.
  ENDIF.

  IF ZTBKPF-AKONT IS INITIAL.
    ZTBKPF-AKONT = VF_KRED-AKONT.
  ENDIF.
  IF ZTBKPF-AKONT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'AKONT'.
  ENDIF.
*>> TAX CODE CHECK.
  CALL FUNCTION 'FI_TAX_INDICATOR_CHECK'
    EXPORTING
      I_BUKRS  = ZTBKPF-BUKRS
      I_HKONT  = VF_KRED-AKONT
      I_KOART  = 'K'
      I_MWSKZ  = ZTBKPF-MWSKZ
      I_STBUK  = SPACE
      X_DIALOG = 'X'
    IMPORTING
      E_EGRKZ  = EGRKZ.
*> ??????.
  IF ZTBKPF-WRBTR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WRBTR'.
  ENDIF.
*> ??.
  IF ZTBKPF-WAERS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WAERS'.
  ENDIF.

*>> ??? ??.
  IF ZTBKPF-XMWST EQ 'X'.
    SELECT SINGLE * FROM T007A
           WHERE KALSM EQ 'TAXKR'
           AND   MWSKZ EQ  ZTBKPF-MWSKZ.
    IF SY-SUBRC NE 0.
      MESSAGE E495 WITH 'TAXKR' ZTBKPF-MWSKZ.
    ENDIF.

    SELECT * FROM  KONP
             WHERE KAPPL EQ 'TX'       ">??.
             AND   KSCHL EQ 'KRIT'     ">?????.
             AND   MWSK1 EQ ZTBKPF-MWSKZ.

      MOVE: KONP-KBETR   TO   W_KBETR,         ">??.
            KONP-KONWA   TO   W_KONWA.         ">??.
      IF NOT W_KBETR IS INITIAL.
        W_KBETR = W_KBETR / 10.
      ENDIF.
    ENDSELECT.

    IF SY-SUBRC EQ 0.
      IF NOT W_KBETR IS INITIAL.
        PERFORM SET_CURR_CONV_TO_EXTERNAL(SAPMZIM01)
                USING ZTBKPF-WRBTR ZTBKPF-WAERS ZTBKPF-WMWST.
*>>>> ?? : (100 + %) =  X : % ======>
        W_WMWST = ZTBKPF-WMWST.
        BAPICURR-BAPICURR = ZTBKPF-WMWST * W_KBETR * 1000.
        W_KBETR1 = W_KBETR.
        W_KBETR = ( W_KBETR + 100 ).
        BAPICURR-BAPICURR = BAPICURR-BAPICURR / W_KBETR.

*          ZTBKPF-WMWST = W_WMWST - ( BAPICURR-BAPICURR * 100 / 1000 ).
*           ZTBKPF-WMWST = ZTBKPF-WMWST / 10.
        BAPICURR-BAPICURR = BAPICURR-BAPICURR / 1000.
        ZTBKPF-WMWST = BAPICURR-BAPICURR.
*            COMPUTE ZTBKPF-WMWST = TRUNC( BAPICURR-BAPICURR ).
*            ZTBKPF-WMWST = ZTBKPF-WMWST * 10.

        PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
                USING ZTBKPF-WMWST ZTBKPF-WAERS.
      ELSE.
        CLEAR : ZTBKPF-WMWST.
      ENDIF.
    ELSE.
      CLEAR : ZTBKPF-WMWST.
    ENDIF.
  ENDIF.


ENDMODULE.                 " VENDOR_ACCOUNT_CHECK_SCR0020  INPUT
*&----------------------------------------------------------------------
*&      Module  INPUT_HEADER_CHECK_SCR0020  INPUT
*&----------------------------------------------------------------------
*       text
*-----------------------------------------------------------------------
MODULE INPUT_HEADER_CHECK_SCR0020 INPUT.
*> 사업장.
*  IF ZTBKPF-BUPLA IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUPLA'.
*  ENDIF.

*> 지급처.
*  IF ZTBKPF-LIFNR IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'LIFNR'.
*  ENDIF.

**> 코스트 센터..
*  IF ZTBKPF-ZFPOYN NE 'Y'.
*     IF ZTBSEG-KOSTL IS INITIAL.
*        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBSEG' 'KOSTL'.
*     ENDIF.
*  ENDIF.
*> 전표통화금액.
  IF ZTBKPF-WRBTR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WRBTR'.
  ENDIF.
*> 통화.
  IF ZTBKPF-WAERS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WAERS'.
  ENDIF.

ENDMODULE.                 " INPUT_HEADER_CHECK_SCR0020  INPUT
*&----------------------------------------------------------------------
*&      Module  GET_OK_CODE_SCR0020  INPUT
*&----------------------------------------------------------------------
MODULE GET_OK_CODE_SCR0020 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'ENTR'.   EXIT.
    WHEN 'YES'.    ANTWORT = 'Y'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.     ANTWORT = 'N'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDMODULE.                 " GET_OK_CODE_SCR0020  INPUT
*&----------------------------------------------------------------------
*&      Module  P2000_INIT_VALUE_CHECK  INPUT
*&----------------------------------------------------------------------
*       text
*-----------------------------------------------------------------------
MODULE P2000_INIT_VALUE_CHECK INPUT.

  IF UF05A-STGRD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'UF05A' 'STGRD'.
  ENDIF.

  IF BSIS-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'BSIS' 'BUDAT'.
  ENDIF.

ENDMODULE.                 " P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_TOTAMOUNT_SCR104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_TOTAMOUNT_SCR104 INPUT.

  CLEAR : ZTBKPF-WRBTR.

  LOOP AT IT_ZSBSEG.
    ADD IT_ZSBSEG-WRBTR TO ZTBKPF-WRBTR.
  ENDLOOP.

  ZTBKPF-WRBTR = ZTBKPF-WRBTR + ZTBKPF-WMWST.

ENDMODULE.                 " SET_TOTAMOUNT_SCR104  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR0104 INPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZTBKPF-ZTERM'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF SCREEN-INPUT EQ '1'.
    CALL FUNCTION 'FI_F4_ZTERM'
      EXPORTING
        I_KOART       = 'K'
        I_ZTERM       = ZTBKPF-ZTERM
        I_XSHOW       = ' '
      IMPORTING
        E_ZTERM       = T052-ZTERM
      EXCEPTIONS
        NOTHING_FOUND = 01.
  ELSE.
    CALL FUNCTION 'FI_F4_ZTERM'
      EXPORTING
        I_KOART       = 'K'
        I_ZTERM       = ZTBKPF-ZTERM
        I_XSHOW       = 'X'
      IMPORTING
        E_ZTERM       = T052-ZTERM
      EXCEPTIONS
        NOTHING_FOUND = 01.
  ENDIF.
  IF SY-SUBRC NE 0.
    MESSAGE S177(06) WITH ZTBKPF-ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    ZTBKPF-ZTERM = T052-ZTERM.
  ENDIF.

ENDMODULE.                 " HELP_ZTERM_SCR0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR0104 INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF ZTBKPF-ZTERM IS INITIAL.
    SELECT SINGLE ZTERM INTO ZTBKPF-ZTERM   " Payment Term
           FROM LFB1
           WHERE LIFNR = ZTTRHD-ZFTRCO
           AND BUKRS   = ZTTRHD-BUKRS.
  ENDIF.

  IF ZTBKPF-MWSKZ IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'MWSKZ'.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_SCR0023  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_SCR0023 INPUT.

  IF ZSTRCSTIT-ZFTRATE IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTRCSTIT' 'ZFTRATE'.
  ENDIF.

  IF ZSTRCSTIT-ZFDTON IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTRCSTIT' 'ZFDTON'.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_SCR0023  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SEQNO_TC_0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SEQNO_TC_0104 INPUT.

  DATA : W_ZFSEQ       LIKE   ZTTRCST-ZFSEQ.

  W_ZFSEQ = 0.
  LOOP AT IT_ZSTRCST.
    W_TABIX = SY-TABIX.
    W_ZFSEQ = W_ZFSEQ + 10.
    IF IT_ZSTRCST-ZFSEQ NE 0.
      LOOP AT IT_ZTTRCSTIT WHERE ZFSEQ = IT_ZSTRCST-ZFSEQ.
        IT_ZTTRCSTIT-ZFSEQ = W_ZFSEQ.
        MODIFY IT_ZTTRCSTIT INDEX SY-TABIX.
      ENDLOOP.
    ENDIF.
    IT_ZSTRCST-ZFSEQ = W_ZFSEQ.
    MODIFY IT_ZSTRCST INDEX W_TABIX.
  ENDLOOP.

ENDMODULE.                 " SET_SEQNO_TC_0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_OVER_WEIGHT_SCR0023  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_OVER_WEIGHT_SCR0023 INPUT.

*  DATA : W_TOTWT LIKE ZTTRCST-ZFCTON.

*  LOOP AT IT_ZSTRCSTIT.
*     ADD IT_ZSTRCSTIT-ZFDTON TO W_TOTWT.
*  ENDLOOP.

*  IF W_TOTWT GT ZTTRCST-ZFCTON.
*     MESSAGE E434(ZIM1) WITH ZTTRCST-ZFCTON.
*  ENDIF.

ENDMODULE.                 " CHECK_OVER_WEIGHT_SCR0023  INPUT
