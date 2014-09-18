*&---------------------------------------------------------------------*
*& Report ZREP_DETECT_CONDITION                                        *
*&---------------------------------------------------------------------*
*& Report to detect records in table EKBZ without condition key in     *
*& KSCHL                                                               *
*&                                                                     *
*& Select options:                                                     *
*& EBELN = PO                                                          *
*& EBELP = PO item                                                     *
*& GJAHR = document year                                               *
*& BELNR = MM invoice document                                         *
*& BUZEI = item in material document                                   *
*&---------------------------------------------------------------------*

REPORT zrep_detect_condition .

*--- define constants -------------------------------------------------*
DATA: c_bewtp_m       TYPE bewtp VALUE 'M', " freight costs
      c_bewtp_n       TYPE bewtp VALUE 'N', " subsequent debit
      c_bewtp_p       TYPE bewtp VALUE 'P', " freight costs, subs.debit
      c_bewtp_q       TYPE bewtp VALUE 'Q', " normal invoice
      c_bewtp_g       TYPE bewtp VALUE 'G',
      c_bewtp_r       TYPE bewtp VALUE 'R',
      c_bewtp_k       TYPE bewtp VALUE 'K',
      c_vgabe_2       TYPE vgabe VALUE '2', " invoice
      c_vgabe_3       TYPE vgabe VALUE '3'. " susequent credit/debit

*--- define tables, structures and variables --------------------------*
TABLES: ekbe,
        ekbz,
        rseg,
        ekko,
        konv.

DATA: BEGIN OF tab_ekbe OCCURS 0.
        INCLUDE STRUCTURE ekbe.
DATA: END OF tab_ekbe.


TYPES: BEGIN OF bkpf_key,
         bukrs        TYPE bukrs,
         belnr        TYPE belnr_d,
         gjahr        TYPE gjahr,
       END OF bkpf_key.
TYPES: tbkpf_key      TYPE TABLE OF bkpf_key.
DATA: tab_bkpf_key TYPE tbkpf_key.


TYPES: BEGIN OF ekbz_wa,
         ebeln        TYPE ebeln,
         ebelp        TYPE ebelp,
         stunr        TYPE stunr,
         zaehk        TYPE dzaehk,
         vgabe        TYPE vgabe,
         gjahr        TYPE gjahr,
         belnr        TYPE mblnr,
         buzei        TYPE mblpo,
         bewtp        TYPE bewtp,
         kschl        TYPE kschl,
       END OF ekbz_wa.

TYPES: tekbz_wa       TYPE TABLE OF ekbz_wa.
DATA:  tab_ekbz_wa    TYPE tekbz_wa.
DATA:  tab_ekbz_rt    TYPE tekbz_wa.
DATA:  s_ekbz_wa      TYPE ekbz_wa.


TYPES: BEGIN OF rseg_wa,
        belnr         TYPE belnr_d,
        gjahr         TYPE gjahr,
        buzei         TYPE rblgp,
        kschl         TYPE kschl,
        lifnr         TYPE lifnr,
      END OF rseg_wa.
TYPES: trseg_wa       TYPE TABLE OF rseg_wa.
DATA:  tab_rseg_wa    TYPE trseg_wa.
DATA:  tab_rseg_rt    TYPE trseg_wa.
DATA:  s_rseg_wa      TYPE rseg_wa.

DATA: number_of_records TYPE i.


DATA: BEGIN OF tab_ekko OCCURS 0.
        INCLUDE STRUCTURE ekko.
DATA: END OF tab_ekko.


TYPES: BEGIN OF konv_wa,
        knumv         TYPE knumv,
        kposn         TYPE kposn,
        stunr         TYPE stunr,
        zaehk         TYPE dzaehk,
        kschl         TYPE kscha,
        lifnr         TYPE lifnr,
      END OF konv_wa.
TYPES: tkonv_wa   TYPE TABLE OF konv_wa.
DATA: tab_konv_wa TYPE tkonv_wa.
DATA: s_konv_wa   TYPE konv_wa.


*--- set up start screen ----------------------------------------------*
SELECT-OPTIONS:  ebeln FOR ekbz-ebeln,
                 ebelp FOR ekbz-ebelp,
                 gjahr FOR ekbz-gjahr,
                 belnr FOR ekbz-belnr,
                 buzei FOR ekbz-buzei.

*--- Start-of-selection -----------------------------------------------*
START-OF-SELECTION.

*--- select affected entries from table EKBZ where KSCHL lost ---------*
  SELECT * INTO CORRESPONDING FIELDS OF TABLE tab_ekbz_wa
         FROM   ekbz
         WHERE  ebeln IN ebeln
         AND    ebelp IN ebelp
         AND    ( vgabe EQ c_vgabe_2 OR vgabe EQ c_vgabe_3 )
         AND    gjahr IN gjahr
         AND    belnr IN belnr
         AND    buzei IN buzei
         AND    kschl EQ space
         ORDER BY ebeln ebelp.

  IF sy-subrc NE 0.
    WRITE: 'No relevant entries found in table EKBZ.'.
    EXIT.
  ENDIF.

*--- select relevant records in table RSEG ----------------------------*
  LOOP AT tab_ekbz_wa INTO s_ekbz_wa.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE tab_rseg_wa
           FROM rseg
           WHERE belnr = s_ekbz_wa-belnr
           AND   gjahr = s_ekbz_wa-gjahr
           AND   buzei = s_ekbz_wa-buzei.

    IF sy-subrc EQ 0.
      LOOP AT tab_rseg_wa INTO s_rseg_wa.
        APPEND s_rseg_wa TO tab_rseg_rt.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

  SORT tab_rseg_rt BY belnr gjahr buzei.

*--- check Logistic Invoice Verification only -------------------------*
  LOOP AT tab_ekbz_wa INTO s_ekbz_wa.

    CASE s_ekbz_wa-bewtp.

      WHEN c_bewtp_m OR
           c_bewtp_n OR
           c_bewtp_p OR
           c_bewtp_q.

        FORMAT COLOR 3 ON.
        WRITE:/ s_ekbz_wa-ebeln, s_ekbz_wa-ebelp, s_ekbz_wa-stunr,
                s_ekbz_wa-zaehk, s_ekbz_wa-vgabe, s_ekbz_wa-gjahr,
                s_ekbz_wa-belnr, s_ekbz_wa-buzei, s_ekbz_wa-kschl.
        FORMAT COLOR 3 OFF.

        READ TABLE tab_rseg_rt INTO s_rseg_wa
                 WITH KEY belnr = s_ekbz_wa-belnr
                          gjahr = s_ekbz_wa-gjahr
                          buzei = s_ekbz_wa-buzei
                          BINARY SEARCH.
        IF sy-subrc EQ 0.
          FORMAT COLOR 5 ON. WRITE:/ 'RSEG: '. FORMAT COLOR 5 OFF.
          FORMAT COLOR 2 ON.
          WRITE: s_rseg_wa-kschl,'Vendor is ', s_rseg_wa-lifnr.
          FORMAT COLOR 2 OFF.
        ELSE.
          FORMAT COLOR 6 ON.
          WRITE:/ 'No condition in RSEG as well -> check manualy!'.
          FORMAT COLOR 6 OFF.
        ENDIF.

*--- check the conditions and condition vendors in table KONV ---------*
        CALL FUNCTION 'ME_EKKO_SINGLE_READ'
             EXPORTING
                  pi_ebeln = s_ekbz_wa-ebeln
             IMPORTING
                  po_ekko  = tab_ekko.

        IF sy-subrc = 0.

          SELECT * INTO CORRESPONDING FIELDS OF TABLE tab_konv_wa
                 FROM konv
                 WHERE knumv = tab_ekko-knumv
                 AND   kposn = s_ekbz_wa-ebelp
                 AND   stunr = s_ekbz_wa-stunr
                 AND   zaehk = s_ekbz_wa-zaehk.

          DESCRIBE TABLE tab_konv_wa LINES number_of_records.

          IF number_of_records > 0.
            LOOP AT tab_konv_wa INTO s_konv_wa.
              FORMAT COLOR 5 ON. WRITE:/ 'KONV: '. FORMAT COLOR 5 ON.
              FORMAT COLOR 2 ON.
              WRITE: s_konv_wa-knumv,
                     s_konv_wa-kposn,
                     s_konv_wa-stunr,
                     s_konv_wa-zaehk,
                     s_konv_wa-kschl,
                     s_konv_wa-lifnr.
              FORMAT COLOR 2 OFF.
            ENDLOOP.
          ELSE.
            FORMAT COLOR 6 ON.
            WRITE:/ 'No condition in KONV as well -> check manualy!'.
            FORMAT COLOR 6 OFF.
          ENDIF.

        ENDIF.

      WHEN OTHERS.

    ENDCASE.
  ENDLOOP.

*--- finish the report ------------------------------------------------*
  DESCRIBE TABLE tab_ekbz_wa LINES number_of_records.

  FORMAT COLOR 2 ON.
  WRITE:/.
  WRITE:/ 'Report finished with ', number_of_records,
          'record(s) found in table EKBZ.'.
  FORMAT COLOR 2 OFF.
