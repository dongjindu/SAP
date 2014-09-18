FUNCTION ZIM_GET_PO_ITEM.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EBELN) LIKE  EKKO-EBELN
*"     VALUE(KNUMV) LIKE  EKKO-KNUMV
*"     VALUE(KSCHL) LIKE  KONV-KSCHL OPTIONAL
*"     VALUE(LOEKZ) LIKE  EKPO-LOEKZ DEFAULT ' '
*"     VALUE(ELIKZ) LIKE  EKPO-ELIKZ DEFAULT ' '
*"  EXPORTING
*"     VALUE(W_ITEM_CNT) LIKE  SY-TABIX
*"     VALUE(W_TOT_AMOUNT) LIKE  ZTREQHD-ZFLASTAM
*"     VALUE(W_ZFMAUD) LIKE  SY-DATUM
*"     VALUE(W_ZFWERKS) LIKE  ZTREQHD-ZFWERKS
*"     VALUE(W_BEDNR) LIKE  EKPO-BEDNR
*"     VALUE(W_MATNR) LIKE  EKPO-MATNR
*"     VALUE(W_TXZ01) LIKE  EKPO-TXZ01
*"  TABLES
*"      IT_ZSREQIT STRUCTURE  ZSREQIT OPTIONAL
*"      IT_ZSREQIT_ORG STRUCTURE  ZSREQIT OPTIONAL
*"      IT_ZTREQORJ STRUCTURE  ZSMLCSG7O OPTIONAL
*"      IT_ZTREQORJ_ORG STRUCTURE  ZSMLCSG7O OPTIONAL
*"  EXCEPTIONS
*"      KEY_INCOMPLETE
*"      NOT_FOUND
*"      NO_REFERENCE
*"      NO_AMOUNT
*"----------------------------------------------------------------------
DATA : WL_DATE   LIKE SY-DATUM.
DATA : W_KWERT   LIKE KONV-KWERT.

  REFRESH : IT_ZSREQIT, IT_ZSREQIT_ORG.
  REFRESH : IT_ZTREQORJ, IT_ZTREQORJ_ORG.
  CLEAR : W_ITEM_CNT, W_TOT_AMOUNT, W_ZFMAUD, W_ZFWERKS, W_TOT_MENGE,
          W_MATNR, W_TXZ01.

  IF EBELN IS INITIAL.
     RAISE KEY_INCOMPLETE.
  ENDIF.

*>> P/O Header SELECT(2001.06.12).
  SELECT SINGLE * FROM EKKO  WHERE  EBELN EQ EBELN.

* P/O ITEM SELECT
  SELECT * INTO TABLE IT_EKPO FROM EKPO
                              WHERE EBELN EQ EBELN
                              AND   LOEKZ EQ LOEKZ
                              AND   PSTYP NE 'T'
                              AND   NETPR GT  0
                              AND   ELIKZ EQ ''.

  IF SY-SUBRC NE 0.
     RAISE NOT_FOUND.
  ENDIF.

  SORT IT_EKPO BY EBELP.

*-----------------------------------------------------------------------
* P/O ITEM LOOP
*-----------------------------------------------------------------------
  LOOP AT IT_EKPO.
*-----------------------------------------------------------------------
* Delivery Date
     SELECT * FROM  EKET UP TO 1 ROWS
                                WHERE EBELN EQ EBELN
                                AND   EBELP EQ IT_EKPO-EBELP
                                ORDER BY EINDT.
        EXIT.
     ENDSELECT.
     WL_DATE   = EKET-EINDT.

     CLEAR IT_ZSREQIT.
* data Move
     MOVE-CORRESPONDING IT_EKPO TO IT_ZSREQIT.
     MOVE : IT_EKPO-EBELP TO IT_ZSREQIT-ZFITMNO.

     IF EKKO-BSTYP EQ 'F'.       ">Purchase Order
        MOVE IT_EKPO-MENGE TO IT_ZSREQIT-ZFPOMENGE.
     ELSEIF EKKO-BSTYP EQ 'L'.
        MOVE IT_EKPO-KTMNG TO IT_ZSREQIT-ZFPOMENGE.
     ELSEIF EKKO-BSTYP EQ 'K'.
        MOVE IT_EKPO-KTMNG TO IT_ZSREQIT-ZFPOMENGE.
     ENDIF.

*-----------------------------------------------------------------------
* Import Request Loop
*-----------------------------------------------------------------------
     SELECT ZFREQNO   INTO W_ZFREQNO
                      FROM ZTREQHD
                      WHERE EBELN EQ EBELN.

*-----------------------------------------------------------------------
* Deleted Marking Check
*-----------------------------------------------------------------------
         SELECT MAX( ZFAMDNO ) INTO W_ZFAMDNO
                               FROM ZTREQST
                               WHERE ZFREQNO EQ W_ZFREQNO.

         SELECT SINGLE * FROM ZTREQST
                         WHERE ZFREQNO EQ W_ZFREQNO
                         AND   ZFAMDNO EQ W_ZFAMDNO.

*-----------------------------------------------------------------------
* Import Request Max Amend Sequence Display.
*-----------------------------------------------------------------------
         IF NOT ( W_ZFREQNO IS INITIAL ).
*-----------------------------------------------------------------------
* Import Request Item Summary
*-----------------------------------------------------------------------
            W_MENGE = 0.
            SELECT SUM( MENGE ) INTO W_MENGE
                                FROM ZTREQIT
                                WHERE ZFREQNO EQ W_ZFREQNO
                                AND   ZFITMNO EQ IT_EKPO-EBELP.

            W_TOT_MENGE = W_TOT_MENGE + W_MENGE.
         ENDIF.
       ENDSELECT.

       IT_ZSREQIT-ZFLCMENGE = W_TOT_MENGE.
       IT_ZSREQIT-MENGE = IT_ZSREQIT-ZFPOMENGE - IT_ZSREQIT-ZFLCMENGE.
       W_TOT_MENGE = 0.
       IF IT_ZSREQIT-MENGE <= 0.
          IT_ZSREQIT-MENGE = 0.
       ENDIF.
       MOVE: WL_DATE          TO   IT_ZSREQIT-ZFEEIND,
             EKET-EINDT       TO   IT_ZSREQIT-EEIND,
             EKET-LPEIN       TO   IT_ZSREQIT-LPEIN.
      IF W_ZFWERKS IS INITIAL  AND W_MATNR IS INITIAL AND
         W_TXZ01   IS INITIAL  AND W_BEDNR IS INITIAL.
        IF IT_ZSREQIT-MENGE GT 0.
           W_MATNR   = IT_EKPO-MATNR.
           W_TXZ01   = IT_EKPO-TXZ01.
           W_ZFWERKS = IT_EKPO-WERKS.
           IF W_ZFMAUD IS INITIAL.   W_ZFMAUD = WL_DATE.   ENDIF.
           IF WL_DATE LT W_ZFMAUD.
              W_ZFMAUD = WL_DATE.
           ENDIF.
        ENDIF.
     ENDIF.
     SELECT KWERT WAERS KPEIN KMEIN KBETR
         INTO (IT_ZSREQIT-KWERT, IT_ZSREQIT-WAERS,
                IT_ZSREQIT-KPEIN, IT_ZSREQIT-KMEIN, IT_ZSREQIT-KBETR)
         FROM  KONV UP  TO 1 ROWS
         WHERE KNUMV    EQ    KNUMV
         AND   KPOSN    EQ    IT_ZSREQIT-ZFITMNO
         AND   KSCHL    EQ    KSCHL.
       EXIT.
     ENDSELECT.

     IF IT_ZSREQIT-KPEIN EQ 0.
        IT_ZSREQIT-KWERT = 0.
     ELSE.
        IT_ZSREQIT-KWERT = ( IT_ZSREQIT-KBETR / IT_ZSREQIT-KPEIN ) *
                             IT_ZSREQIT-MENGE.
     ENDIF.
*-----------------------------------------------------------------------
* Import Request Amount
     IF IT_ZSREQIT-PEINH NE 0.
       W_TOT_AMOUNT = W_TOT_AMOUNT +
           ( IT_ZSREQIT-MENGE * ( IT_EKPO-BPUMZ / IT_EKPO-BPUMN )
         * ( IT_ZSREQIT-NETPR / IT_ZSREQIT-PEINH ) ).
     ENDIF.
*-----------------------------------------------------------------------
     IF EKET-EINDT GT 0.
        CALL FUNCTION 'PERIOD_AND_DATE_CONVERT_OUTPUT'
             EXPORTING
                  INTERNAL_DATE   = EKET-EINDT
                  INTERNAL_PERIOD = EKET-LPEIN
             IMPORTING
                  EXTERNAL_DATE   = IT_ZSREQIT-EEIND
                  EXTERNAL_PERIOD = IT_ZSREQIT-LPEIN.
     ENDIF.

*>> P/O H/S code GET.
     SELECT SINGLE STAWN
       INTO IT_ZSREQIT-STAWN
       FROM EIPO
      WHERE EXNUM  EQ  EKKO-EXNUM
        AND EXPOS  EQ  IT_EKPO-EBELP.

*>> If P/O has No HS Code => Material Master HS CODE Get.
     IF IT_ZSREQIT-STAWN IS INITIAL.
         SELECT SINGLE STAWN
           INTO IT_ZSREQIT-STAWN
           FROM MARC
          WHERE MATNR EQ IT_EKPO-EMATN
            AND WERKS EQ IT_EKPO-WERKS.
     ENDIF.

*-----------------------------------------------------------------------
* INTERNAL TABLE APPEND
*-----------------------------------------------------------------------
       APPEND IT_ZSREQIT.
       IT_ZSREQIT_ORG = IT_ZSREQIT.

       W_ITEM_CNT = W_ITEM_CNT + 1.
  ENDLOOP.

  IT_ZSREQIT_ORG[]  =   IT_ZSREQIT[].

ENDFUNCTION.
