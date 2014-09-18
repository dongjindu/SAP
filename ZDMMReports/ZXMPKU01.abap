*----------------------------------------------------------------------*
* Customer-Specific Functions when Changing Kanban Status
*----------------------------------------------------------------------*
*   INCLUDE ZXMPKU01                                                   *
*----------------------------------------------------------------------*
*"  IMPORTING
*"     VALUE(I_SFGSV) LIKE  TPKAK-SFGSV
*"     VALUE(I_SFGSN) LIKE  TPKAK-SFGSN
*"     VALUE(I_PKHD) LIKE  PKHD STRUCTURE  PKHD
*"     VALUE(I_PKPS) LIKE  PKPS STRUCTURE  PKPS
*"     VALUE(I_PKPSHD) LIKE  PKPSHD STRUCTURE  PKPSHD
*"     VALUE(I_PVBE) LIKE  PVBE STRUCTURE  PVBE
*"     VALUE(I_TPKSF) LIKE  TPKSF STRUCTURE  TPKSF
*"     VALUE(I_PKER) LIKE  PKER STRUCTURE  PKER

**S__ PAUL Insert Logic
  DATA: I_BODY   LIKE ZMMT0041,
        I_BODY38 LIKE ZMMT0038,
        I_BODY41 LIKE ZMMT0041.
  DATA: L_DATE TYPE ZDATETIME.
*        L_PKKEY LIKE ZMMT0042-PKKEY .
  DATA: LT_ZMMT0042 LIKE TABLE OF ZMMT0042 WITH HEADER LINE.
  CLEAR : I_BODY, I_BODY38, I_BODY41.

*  BREAK-POINT.

*  IMPORT TEXIDV FROM MEMORY ID 'TABLF'.
*  READ TABLE TEXIDV INDEX 1.

  I_BODY-PKKEY   = I_PKPS-PKKEY.
  I_BODY-RSNUM   = I_PKPS-RSNUM.
  I_BODY-RSPOS   = I_PKPS-RSPOS.
  I_BODY-MATNR   = I_PKHD-MATNR.
  I_BODY-WERKS   = I_PKHD-WERKS.
  I_BODY-PRVBE   = I_PKHD-PRVBE.
  I_BODY-LGORT   = I_PKHD-UMLGO.
  I_BODY-LGPRO   = I_PVBE-LGORT.
  I_BODY-ABLAD   = I_PKHD-ABLAD.
*  I_BODY-PKBMG   = I_PKHD-BEHMG.
*  I_BODY-PKBMG   = I_PKPS-PKIMG.
  I_BODY-KWBZM   = I_PKHD-KWBZM.
  I_BODY-MEINS   = I_PKHD-MEINS.
  I_BODY-SAEDT   = I_PKPS-SAEDT.
  I_BODY-SAEUZ   = I_PKPS-SAEUZ.
  I_BODY-ZFEEDER = I_PKHD-ZFEEDER.
  I_BODY-RKSTA   = I_PKHD-RKSTA.
  I_BODY-SFGSN   = I_SFGSN.
  I_BODY-ETNAM   = SY-UNAME.
  I_BODY-ETDAT   = SY-DATUM.
  I_BODY-ETTIM   = SY-UZEIT.

*Manual/Full/Engine plant

*  CASE I_SFGSV. "Old
  CASE I_SFGSN.  "New Kanban Status
    WHEN 2.      "Empty
      I_BODY-PKBMG   = I_PKHD-BEHMG.

*-----Engine Shop: BOX# FROM ZMMT0042 TABLE
* by ig.moon - Engine Plant Split {
      IF I_PKHD-WERKS = 'E001' or I_PKHD-WERKS = 'E002'.
** Changed on 11/02/12 for casting DOES NOT need to update zmmt0042
       if I_PKHD-prvbe = 'CLT'.
       else.
** End
        MOVE-CORRESPONDING I_BODY TO I_BODY41.
        SELECT * INTO TABLE LT_ZMMT0042
          FROM ZMMT0042
         WHERE PKKEY EQ I_PKPS-PKKEY
           AND EXIDV NE ''
           AND UPDATE_FLAG = ' '.
        SORT LT_ZMMT0042 BY UPDATE_DATE DESCENDING.
        READ TABLE LT_ZMMT0042 INDEX 1.
        I_BODY41-EXIDV = LT_ZMMT0042-EXIDV.
        L_DATE = LT_ZMMT0042-UPDATE_DATE.
        MODIFY ZMMT0041 FROM I_BODY41. "to send Glovis GCS
      endif.
*-----GA Shop: BOX# FROM ZMMT0042 TABLE
      ELSE.
        MOVE-CORRESPONDING I_BODY TO I_BODY38.

        CALL FUNCTION 'Z_MM_IF_OB_02_003_DB'
             EXPORTING
                  I_BODY = I_BODY38.
      ENDIF.


    WHEN 5 OR 6 OR 9.  "full, in use, incorrect

**----Engine Plant
* by ig.moon - Engine Plant Split {
*      IF  I_PKHD-WERKS = 'E001' AND I_SFGSN = '5'.
       IF  ( I_PKHD-WERKS = 'E001' or I_PKHD-WERKS = 'E002' )
           AND I_SFGSN = '5'.
** Changed on 11/02/12 for casting DOES NOT need to update zmmt0042
       if I_PKHD-prvbe = 'CLT'.
       else.
** End on 11/02/12
        I_BODY-PKBMG   = I_PKPS-PKIMG.
        MOVE-CORRESPONDING I_BODY TO I_BODY41.
        SELECT * INTO TABLE LT_ZMMT0042
           FROM ZMMT0042
          WHERE PKKEY EQ I_PKPS-PKKEY
            AND EXIDV NE ''
            AND UPDATE_FLAG = ' '.
        SORT LT_ZMMT0042 BY UPDATE_DATE DESCENDING.
        READ TABLE LT_ZMMT0042 INDEX 1.
        I_BODY41-EXIDV  = LT_ZMMT0042-EXIDV.
** Change by Park on 11/20/13
        I_BODY41-ACSHOP = LT_ZMMT0042-ACSHOP.
        I_BODY41-USRID  = LT_ZMMT0042-USRID.
** End change.
        L_DATE = LT_ZMMT0042-UPDATE_DATE.

        MODIFY ZMMT0041 FROM I_BODY41.
      endif.
      ENDIF.

      IF I_PKHD-RKSTA = 'I'.  "event driven kanban
*        IF I_SFGSN IS INITIAL OR
*           I_SFGSN EQ '5'     OR
*           I_SFGSN EQ '6'     OR
*           I_SFGSN EQ '9'.
        I_SFGSN = 1.

        CALL FUNCTION 'BAPI_KANBAN_CHANGESTATUS'
             EXPORTING
                  KANBANIDNUMBER = I_PKPS-PKKEY
                  NEXTSTATUS     = I_SFGSN.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
             EXPORTING
                  WAIT = 'X'.

*        ENDIF.
      ENDIF.
  ENDCASE.

* DO NOT DELETE if ONE-STEP EMPTY
*  IF ( I_PKHD-PRVBE = 'TEF' OR I_PKHD-PRVBE = 'RPK' )
*  AND  I_SFGSN = 2.
*
*  ELSE.
*
*AFTER WORK DELETE Cbo Table
  IF NOT I_BODY41-EXIDV IS INITIAL.
** Changed on 08/25/11
*      DELETE FROM ZMMT0042 WHERE PKKEY = I_PKPS-PKKEY
*                             AND EXIDV = I_BODY41-EXIDV.
    UPDATE ZMMT0042 SET UPDATE_FLAG = 'X'
                      WHERE PKKEY = I_PKPS-PKKEY
                          AND EXIDV = I_BODY41-EXIDV
                          AND UPDATE_DATE = L_DATE.
** end on 08/25/11
  ENDIF.
*
*  ENDIF.
