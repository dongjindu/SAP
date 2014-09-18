*&---------------------------------------------------------------------*
*& Report  ZRIMDBUPDATE                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZRIMDBUPDATE   MESSAGE-ID ZIM          .

TABLES : ZTIVIT,
         ZTBDIV,
        *ZTIDSUSD,
         ZTIDSUSD.

DATA : W_BUKRS   LIKE  ZTBDIV-BUKRS,
       W_GJAHR   LIKE  ZTBDIV-GJAHR,
       W_BELNR   LIKE  ZTBDIV-BELNR,
       W_WRBTR   LIKE  ZTBDIV-WRBTR,
       W_DMBTR   LIKE  ZTBDIV-DMBTR,
       W_HMF     LIKE  ZTIDSUSD-ZFHMAMT,
       W_MPF     LIKE  ZTIDSUSD-ZFMPAMT,
       W_DUTY    LIKE  ZTIDSUSD-ZFDUTY.

SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_EBELN  FOR ZTIVIT-EBELN   NO-EXTENSION
                                               NO INTERVALS,
                   S_EBELP  FOR ZTIVIT-EBELP   NO-EXTENSION
                                               NO INTERVALS.
SELECTION-SCREEN END OF BLOCK B1.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

   IF S_EBELN[] IS INITIAL.
      MESSAGE E977 WITH 'Input P/O No'.
      EXIT.
   ENDIF.
   IF S_EBELP[] IS INITIAL.
      MESSAGE E977 WITH 'Input P/O ITEM No!'.
      EXIT.
   ENDIF.

*   SELECT *
*     FROM ZTIVIT
*    WHERE EBELN   IN S_EBELN
*      AND EBELP   IN S_EBELP.
*
*      SELECT SINGLE *
*        INTO *ZTIDSUSD
*        FROM ZTIDSUSD
*       WHERE ZFIVNO  EQ  ZTIVIT-ZFIVNO
*         AND ZFIVDNO EQ  ZTIVIT-ZFIVDNO.
*      W_DUTY  =  *ZTIDSUSD-ZFDUTY.
*      W_HMF   =  *ZTIDSUSD-ZFHMAMT.
*      W_MPF   =  *ZTIDSUSD-ZFMPAMT.
*
*      SELECT SINGLE *
*        FROM ZTIDSUSD
*       WHERE ZFIVNO  EQ  *ZTIDSUSD-ZFIVNO
*         AND ZFCLSEQ EQ  *ZTIDSUSD-ZFCLSEQ
*         AND ZFCONO  EQ  *ZTIDSUSD-ZFCONO
*         AND ZFRONO  NE  *ZTIDSUSD-ZFRONO.
*      IF SY-SUBRC EQ 0.
*         ZTIDSUSD-ZFDUTY  =  ZTIDSUSD-ZFDUTY     + W_DUTY.
*         ZTIDSUSD-ZFMPAMT   =  ZTIDSUSD-ZFMPAMT  + W_MPF.
*         ZTIDSUSD-ZFHMAMT   =  ZTIDSUSD-ZFHMAMT  + W_HMF.
*         UPDATE  ZTIDSUSD.
*
*      ELSE.
*         SELECT SINGLE *
*           FROM ZTIDSUSD
*          WHERE ZFIVNO  EQ  *ZTIDSUSD-ZFIVNO
*            AND ZFCLSEQ EQ  *ZTIDSUSD-ZFCLSEQ.
*         ZTIDSUSD-ZFDUTY  =  ZTIDSUSD-ZFDUTY + W_DUTY.
*         ZTIDSUSD-ZFMPAMT   =  ZTIDSUSD-ZFMPAMT  + W_MPF.
*         ZTIDSUSD-ZFHMAMT   =  ZTIDSUSD-ZFHMAMT  + W_HMF.
*         UPDATE  ZTIDSUSD.
*      ENDIF.
*
*      DELETE FROM ZTIDSUSD
*       WHERE ZFIVNO  EQ  *ZTIDSUSD-ZFIVNO
*         AND ZFCLSEQ EQ  *ZTIDSUSD-ZFCLSEQ
*         AND ZFCONO  EQ  *ZTIDSUSD-ZFCONO
*         AND ZFRONO  EQ  *ZTIDSUSD-ZFRONO.
*
*   ENDSELECT.

   SELECT *
     FROM ZTBDIV
    WHERE EBELN    IN  S_EBELN
      AND EBELP    IN  S_EBELP
      AND ZFCSTGRP EQ  '006'
      AND ZFCD     EQ  '001'.

      W_BUKRS  =  ZTBDIV-BUKRS.
      W_BELNR  =  ZTBDIV-BELNR.
      W_GJAHR  =  ZTBDIV-GJAHR.
      W_DMBTR  =  ZTBDIV-DMBTR.
      W_WRBTR  =  ZTBDIV-WRBTR.

      SELECT SINGLE *
        FROM ZTBDIV
       WHERE BUKRS    EQ W_BUKRS
         AND BELNR    EQ W_BELNR
         AND GJAHR    EQ W_GJAHR
         AND ZFCSTGRP EQ '006'
         AND ZFCD     EQ '001'
         AND EBELN    NE ''
         AND ZFIMDNO  NE ''.
      ZTBDIV-DMBTR = ZTBDIV-DMBTR +  W_DMBTR.
      ZTBDIV-WRBTR = ZTBDIV-WRBTR +  W_WRBTR.
      UPDATE  ZTBDIV.
   ENDSELECT.

   DELETE  FROM ZTBDIV
    WHERE  EBELN    IN  S_EBELN
      AND  EBELP    IN  S_EBELP
      AND  ZFCSTGRP EQ  '006'
      AND  ZFCD     EQ  '001'.

   IF SY-SUBRC EQ 0.
      MESSAGE S977 WITH 'Update Success!'.
      COMMIT WORK.
   ELSE.
      MESSAGE S977 WITH 'Update Fail!.'.
      ROLLBACK WORK.
   ENDIF.
