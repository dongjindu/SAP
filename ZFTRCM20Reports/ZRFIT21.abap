*&--------------------------------------------------------------------
*& Author                 : JIPARK
*& Creation Date          : 14/01/2004
*& Specification By       : JIPARK
*& Pattern                : Report 1-17
*& Development Request No : UD1K905780
*& Addl documentation     :
*& Description  : Display & Update L&F Lineitem
*&                (Original P/G : ZRFIT21_UPDATE)
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT  zrfit21  NO STANDARD PAGE HEADING LINE-SIZE 154.

************************************************************************
*     DATA DECLARATION
************************************************************************
TABLES : ztfi_drdw, fdsb, fdes, tgsb, t035,
         rfpdo1, skat, t001, lfb1, knb1.

DATA : BEGIN OF sztfi_drdw,
       grupp  LIKE ztfi_drdw-grupp,
       ebene  LIKE ztfi_drdw-ebene,
       bukrs  LIKE ztfi_drdw-bukrs,
       belnr  LIKE ztfi_drdw-belnr,
       gjahr  LIKE ztfi_drdw-gjahr,
       buzei  LIKE ztfi_drdw-buzei,
       gsber  LIKE ztfi_drdw-gsber,
       datum  LIKE ztfi_drdw-datum,
       koart  LIKE ztfi_drdw-koart,
       umskz  LIKE ztfi_drdw-umskz,
       hkont  LIKE ztfi_drdw-hkont,
       lifnr  LIKE ztfi_drdw-lifnr,
       kunnr  LIKE ztfi_drdw-kunnr,
       bstat  LIKE ztfi_drdw-bstat,
       dispw  LIKE ztfi_drdw-dispw,
       wrshb  LIKE ztfi_drdw-wrshb,
       dmbtr  LIKE ztfi_drdw-dmbtr,
       avdat  LIKE ztfi_drdw-avdat,
       END OF sztfi_drdw.
DATA : iitab   LIKE bdcdata OCCURS 0 WITH HEADER LINE,
       it_msg  LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
       iztfi_drdw   LIKE sztfi_drdw OCCURS 0 WITH HEADER LINE,
       iztfi_drdw1  LIKE sztfi_drdw OCCURS 0 WITH HEADER LINE,
       iaztfi_drdw  LIKE sztfi_drdw OCCURS 0 WITH HEADER LINE,
       irztfi_drdw  LIKE sztfi_drdw OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF ibkpf OCCURS 0,
       bukrs  LIKE bkpf-bukrs,
       belnr  LIKE bkpf-belnr,
       gjahr  LIKE bkpf-gjahr,
       stblg  LIKE bkpf-stblg,
       bstat  LIKE bkpf-bstat,
       END OF ibkpf.
DATA : BEGIN OF ibseg OCCURS 0,
       bukrs  LIKE bseg-bukrs,
       belnr  LIKE bseg-belnr,
       gjahr  LIKE bseg-gjahr,
       buzei  LIKE bseg-buzei,
       stblg  LIKE bkpf-stblg,
       bstat  LIKE bkpf-bstat,
       augdt  LIKE bseg-augdt,
       augbl  LIKE bseg-augbl,
       END OF ibseg.
DATA : it_ztfi_drdw LIKE ztfi_drdw OCCURS 0 WITH HEADER LINE.

DATA : wa_gwrshb  LIKE ztfi_drdw-wrshb,
       wa_twrshb  LIKE ztfi_drdw-wrshb,
       wa_gdmbtr  LIKE ztfi_drdw-dmbtr,
       wa_tdmbtr  LIKE ztfi_drdw-dmbtr,
       wa_hkont   LIKE ztfi_drdw-hkont,
       sv_hkontn  LIKE skat-txt20,
       sv_bukrs   LIKE ztfi_drdw-bukrs VALUE 'H201',
       sv_ktopl   LIKE t001-ktopl,
       sv_waers   LIKE t001-waers,
       iztfi_drdw_cnt  TYPE i,
       iaztfi_drdw_cnt TYPE i,
       irztfi_drdw_cnt TYPE i,
       mode LIKE sy-tabix.

CONSTANTS : t_gu        VALUE '|',
            t_tag       VALUE 'F',
            t_grupp(10) VALUE 'Pln.Grp.',
            t_ebene(2)  VALUE 'LV',
            t_bukrs(4)  VALUE 'CoCd',
            t_belnr(10) VALUE 'Document',
            t_gsber(4) VALUE 'B/A',
            t_buzei(3)  VALUE 'NO.',
            t_gjahr(4)  VALUE 'Year',
            t_datum(10) VALUE 'Date',
            t_koart(2)  VALUE 'AT',
            t_umskz(2)  VALUE 'SP',
            t_hkont(10) VALUE 'Account',
            t_hkontn(20) VALUE 'Acct.Desc.',
            t_zuonr(18) VALUE 'Assignmnt.',
            t_dispw(5)  VALUE 'Curr.',
            t_wrshb(18) VALUE '      Doc.Amt.',
            t_dmbtr(16) VALUE ' Loc.Amt.',
            t_avdat(10) VALUE 'ClearDate',
            t_bstat(6)  VALUE 'Status'.

RANGES : r_kwaer FOR ztfi_drdw-dispw,
         r_grupp FOR ztfi_drdw-grupp,
         r_ebene FOR ztfi_drdw-ebene.

*EXPORT (iztfi_drdw) TO MEMORY ID 'IZID'.
************************************************************************
*     SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-059.
SELECT-OPTIONS:  s_bukrs FOR  fdsb-bukrs DEFAULT 'H201',
                 s_gsber FOR  fdsb-gsber,
                 s_datum FOR  fdsb-datum OBLIGATORY.
SELECTION-SCREEN SKIP 1.
PARAMETERS:      p_ebene LIKE fdsb-ebene,
                 p_grupp LIKE fdsb-bnkko.
SELECTION-SCREEN END OF BLOCK 1.
SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE text-058.
SELECTION-SCREEN SKIP.
*PARAMETERS :     vondt   LIKE fdsb-datum,
*                 bisdt   LIKE fdsb-datum.
*                 azart(2) TYPE c.
SELECTION-SCREEN END OF BLOCK 2.

SELECTION-SCREEN BEGIN OF BLOCK 3 WITH FRAME TITLE text-060.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) text-001.
SELECTION-SCREEN POSITION 34.
PARAMETERS     : p_skalv  LIKE rfpdo1-ts70skal.
SELECTION-SCREEN COMMENT 36(1) text-002.
SELECTION-SCREEN POSITION 38.
PARAMETERS     : p_skaln  LIKE rfpdo1-ts70skal.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK 3.

SELECTION-SCREEN BEGIN OF BLOCK 4 WITH FRAME TITLE text-061.
PARAMETERS : kwaer LIKE ztfi_drdw-dispw DEFAULT 'USD'.
*            awaer LIKE ztfi_drdw-dispw.
SELECTION-SCREEN END OF BLOCK 4.


************************************************************************
*     START-OF-SELECTION.
************************************************************************
START-OF-SELECTION.
  SELECT SINGLE waers ktopl INTO (sv_waers, sv_ktopl)
                FROM t001
                WHERE bukrs EQ sv_bukrs.

  PERFORM  select_ztfi_drdw_rtn.
  iztfi_drdw1[] = iztfi_drdw[].
*  PERFORM  DISTRIBUTE_Iztfi_drdw_RTN.

************************************************************************
*     END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  SORT iztfi_drdw1 BY grupp ebene.
  PERFORM    write-rtn.

************************************************************************
*     TOP-OF-PAGE.
************************************************************************
TOP-OF-PAGE.
  SET PF-STATUS 'STAT'.
  ULINE.
* FORMAT COLOR 4.
  FORMAT COLOR COL_HEADING.
  WRITE : / "sy-vline NO-GAP, t_tag   NO-GAP,
            t_gu NO-GAP, t_grupp NO-GAP,
            t_gu NO-GAP, t_ebene NO-GAP,
            t_gu NO-GAP, t_bukrs NO-GAP,
            t_gu NO-GAP, t_belnr NO-GAP,
            t_gu NO-GAP, t_buzei NO-GAP,
            t_gu NO-GAP, t_gsber NO-GAP,
            t_gu NO-GAP, t_gjahr NO-GAP,
            t_gu NO-GAP, t_datum NO-GAP,
            t_gu NO-GAP, t_koart NO-GAP,
            t_gu NO-GAP, t_hkont NO-GAP,
            t_gu NO-GAP, t_umskz NO-GAP,
            t_gu NO-GAP, t_hkontn NO-GAP,
            t_gu NO-GAP, t_dispw NO-GAP,
            t_gu NO-GAP, t_wrshb NO-GAP LEFT-JUSTIFIED,
            t_gu NO-GAP, t_dmbtr NO-GAP LEFT-JUSTIFIED,
            t_gu NO-GAP, t_avdat NO-GAP,
            t_gu NO-GAP, t_bstat NO-GAP,
            t_gu NO-GAP.
  FORMAT COLOR OFF.
  ULINE.

************************************************************************
*     TOP-OF-PAGE DURING LINE-SELECTION.
************************************************************************
TOP-OF-PAGE DURING LINE-SELECTION.
  ULINE.
* FORMAT COLOR 4.
  FORMAT COLOR COL_HEADING.
  WRITE : / "sy-vline NO-GAP, t_tag   NO-GAP,
            t_gu NO-GAP, t_grupp NO-GAP,
            t_gu NO-GAP, t_ebene NO-GAP,
            t_gu NO-GAP, t_bukrs NO-GAP,
            t_gu NO-GAP, t_belnr NO-GAP,
            t_gu NO-GAP, t_buzei NO-GAP,
            t_gu NO-GAP, t_gsber NO-GAP,
            t_gu NO-GAP, t_gjahr NO-GAP,
            t_gu NO-GAP, t_datum NO-GAP,
            t_gu NO-GAP, t_koart NO-GAP,
*           t_gu NO-GAP, t_umskz NO-GAP,
*           t_gu NO-GAP, t_hkont NO-GAP,
            t_gu NO-GAP, t_hkont NO-GAP,
            t_gu NO-GAP, t_umskz NO-GAP,
            t_gu NO-GAP, t_hkontn NO-GAP,
            t_gu NO-GAP, t_dispw NO-GAP,
            t_gu NO-GAP, t_wrshb NO-GAP LEFT-JUSTIFIED,
            t_gu NO-GAP, t_dmbtr NO-GAP LEFT-JUSTIFIED,
            t_gu NO-GAP, t_avdat NO-GAP,
            t_gu NO-GAP, t_bstat NO-GAP,
            t_gu NO-GAP.
  FORMAT COLOR OFF.
  ULINE.

************************************************************************
*     AT LINE-SELECTION.
************************************************************************
AT LINE-SELECTION.
  SET PARAMETER ID 'BLN' FIELD iztfi_drdw1-belnr.
  SET PARAMETER ID 'BUK' FIELD iztfi_drdw1-bukrs.
  SET PARAMETER ID 'GJR' FIELD iztfi_drdw1-gjahr.
  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

************************************************************************
*     AT USER-COMMAND.
************************************************************************
AT USER-COMMAND.
* EXPORT iztfi_drdw TO MEMORY ID 'IZID'.
  CASE sy-ucomm.
    WHEN 'REFR'.
*     SUBMIT zrfit21 AND RETURN.
      PERFORM refresh_document.
      CLEAR    iztfi_drdw[].
      PERFORM  select_ztfi_drdw_rtn.
      PERFORM  distribute_iztfi_drdw_rtn.
      SORT iztfi_drdw1 BY grupp ebene.
      PERFORM  write-rtn.
    WHEN 'SORTA'.  PERFORM exec_sort  USING 'A'.
    WHEN 'SORTD'.  PERFORM exec_sort  USING 'D'.
    WHEN 'CANCEL'. LEAVE PROGRAM.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  refresh_document.
*&---------------------------------------------------------------------*
FORM refresh_document.
  LOOP AT iztfi_drdw.
    SELECT SINGLE bukrs belnr gjahr stblg bstat
      INTO CORRESPONDING FIELDS OF ibkpf
      FROM bkpf
     WHERE bukrs = iztfi_drdw-bukrs
       AND belnr = iztfi_drdw-belnr
       AND gjahr = iztfi_drdw-gjahr.

    SELECT SINGLE
      bukrs belnr gjahr buzei augdt augbl
      INTO CORRESPONDING FIELDS OF ibseg
      FROM bseg
     WHERE bukrs = ibkpf-bukrs
       AND belnr = ibkpf-belnr
       AND gjahr = ibkpf-gjahr
       AND buzei = iztfi_drdw-buzei.

    CHECK  sy-subrc = 0.
    MOVE   ibkpf-stblg   TO  ibseg-stblg.
    MOVE   ibkpf-bstat   TO  ibseg-bstat.

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF it_ztfi_drdw
      FROM ztfi_drdw
     WHERE bukrs = ibseg-bukrs
       AND belnr = ibseg-belnr
       AND gjahr = ibseg-gjahr
       AND buzei = ibseg-buzei.
    CHECK sy-subrc = 0.

    IF ibseg-stblg <> space OR ibseg-augbl <> space.
      IF ibseg-augbl <> space.
        it_ztfi_drdw-bstat = 'A'.
        it_ztfi_drdw-augbl = ibseg-augbl.
        it_ztfi_drdw-avdat = ibseg-augdt.
      ENDIF.
      IF ibseg-stblg <> space.
*        IT_ztfi_drdw-BSTAT = 'R'.
*        IT_ztfi_drdw-STBLG = IBSEG-STBLG.
*        IT_ztfi_drdw-AVDAT = IBSEG-AUGDT.
        PERFORM delete_ztfi_drdw.
        CONTINUE.
      ENDIF.
    ELSE.
      it_ztfi_drdw-bstat = space.
      it_ztfi_drdw-augbl = space.
*     IT_ztfi_drdw-STBLG = SPACE.
      it_ztfi_drdw-avdat = 0.
    ENDIF.
    IF ibseg-bstat = 'B'.
      it_ztfi_drdw-bstat = space.
      it_ztfi_drdw-augbl = space.
*     IT_ztfi_drdw-STBLG = SPACE.
      it_ztfi_drdw-avdat = 0.
    ENDIF.
    PERFORM update_ztfi_drdw.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  delete_ztfi_drdw
*&---------------------------------------------------------------------*
FORM delete_ztfi_drdw.
  DELETE FROM ztfi_drdw
   WHERE bukrs = ibseg-bukrs
     AND belnr = ibseg-belnr
     AND gjahr = ibseg-gjahr
     AND buzei = ibseg-buzei.
ENDFORM.                    " delete_ztfi_drdw
*&---------------------------------------------------------------------*
*&      Form  update_ztfi_drdw
*&---------------------------------------------------------------------*
FORM update_ztfi_drdw.
  UPDATE ztfi_drdw
     SET bstat = it_ztfi_drdw-bstat
         augbl = it_ztfi_drdw-augbl
         avdat = it_ztfi_drdw-avdat
*        STBLG = IT_ztfi_drdw-STBLG
   WHERE bukrs = ibseg-bukrs
     AND belnr = ibseg-belnr
     AND gjahr = ibseg-gjahr
     AND buzei = ibseg-buzei.
ENDFORM.                    " update_ztfi_drdw
*&---------------------------------------------------------------------*
*&      Form  select_ztfi_drdw_rtn
*&---------------------------------------------------------------------*
FORM select_ztfi_drdw_rtn.
  CLEAR : r_kwaer[], r_kwaer, r_grupp[], r_grupp, r_ebene[], r_ebene.
  IF kwaer  NE SPACE.
    r_kwaer-sign = 'I'.
    r_kwaer-option = 'EQ'.
    r_kwaer-low  = kwaer.
    APPEND r_kwaer.
  ENDIF.
  IF p_grupp NE SPACE.
    r_grupp-sign = 'I'.
    r_grupp-option = 'EQ'.
    r_grupp-low = p_grupp.
    APPEND r_grupp.
  ENDIF.
  IF p_ebene NE SPACE.
    r_ebene-sign = 'I'.
    r_ebene-option = 'EQ'.
    r_ebene-low = p_ebene.
    APPEND r_ebene.
  ENDIF.

  SELECT grupp ebene bukrs belnr gjahr buzei umskz datum koart gsber
               hkont lifnr kunnr bstat dispw wrshb dmbtr avdat
    INTO TABLE iztfi_drdw
    FROM ztfi_drdw
*   WHERE BUKRS IN  S_BUKRS
    WHERE bukrs IN ( select BUKRS from T001 WHERE bukrs IN s_bukrs )
     and GRUPP  in R_GRUPP
     AND ebene  IN r_ebene
*    AND dispw  IN r_kwaer
*    AND datum  BETWEEN vondt AND bisdt
     AND datum  IN s_datum
     AND gsber  IN s_gsber.

  DESCRIBE TABLE iztfi_drdw LINES iztfi_drdw_cnt.

ENDFORM.                    " select_ztfi_drdw_rtn
*&---------------------------------------------------------------------*
*&      Form  distribute_iztfi_drdw_rtn
*&---------------------------------------------------------------------*
FORM distribute_iztfi_drdw_rtn.

  CHECK iztfi_drdw_cnt > 0.
  CLEAR iztfi_drdw.
  CLEAR : iaztfi_drdw[], irztfi_drdw[], iztfi_drdw1[].
  LOOP AT iztfi_drdw.
    IF  iztfi_drdw-bstat = 'A'.
      MOVE-CORRESPONDING iztfi_drdw TO iaztfi_drdw.
      APPEND iaztfi_drdw.
      CLEAR  iaztfi_drdw.
    ELSEIF iztfi_drdw-bstat = 'R'.
      MOVE-CORRESPONDING iztfi_drdw TO irztfi_drdw.
      APPEND irztfi_drdw.
      CLEAR  irztfi_drdw.
    ELSE.
      MOVE-CORRESPONDING iztfi_drdw TO iztfi_drdw1.
      APPEND iztfi_drdw1.
      CLEAR  iztfi_drdw1.
    ENDIF.
    CLEAR iztfi_drdw.
  ENDLOOP.


ENDFORM.                    " distribute_iztfi_drdw_rtn
*&---------------------------------------------------------------------*
*&      Form  write-rtn
*&---------------------------------------------------------------------*
FORM write-rtn.
  sy-lsind = 0.
  PERFORM write_iztfi_drdw_rtn.
*  PERFORM WRITE_IAztfi_drdw_RTN.
*  PERFORM WRITE_IRztfi_drdw_RTN.
ENDFORM.                    " write-rtn
*&---------------------------------------------------------------------*
*&      Form  write_iztfi_drdw_rtn
*&---------------------------------------------------------------------*
FORM write_iztfi_drdw_rtn.
  DATA : l_sum_wrshb LIKE iztfi_drdw1-wrshb,
         l_sum_dmbtr LIKE iztfi_drdw1-dmbtr.

  CLEAR iztfi_drdw1.
* SORT iztfi_drdw1 BY grupp ebene.

  FORMAT COLOR 2.

  LOOP AT iztfi_drdw1 WHERE bstat NA 'AR'. "a:normal doc / r:reverse?
    mode = sy-tabix MOD 2.
    PERFORM set_mode USING mode.

*   CHECK iztfi_drdw1-bstat NA 'AR'.  "??

    CLEAR : wa_hkont, sv_hkontn, sv_bukrs.
    MOVE    iztfi_drdw1-bukrs  TO  sv_bukrs.
    CASE iztfi_drdw1-koart.
      WHEN 'S'.
        MOVE iztfi_drdw1-hkont TO wa_hkont.
        PERFORM read_hkont.
      WHEN 'K'.
        MOVE iztfi_drdw1-lifnr TO wa_hkont.
        PERFORM read_lifnr.
      WHEN 'D'.
        MOVE iztfi_drdw1-kunnr TO wa_hkont.
        PERFORM read_kunnr.
    ENDCASE.
    WRITE : /   t_gu NO-GAP, iztfi_drdw1-grupp UNDER t_grupp NO-GAP,
                t_gu NO-GAP, iztfi_drdw1-ebene UNDER t_ebene NO-GAP,
                t_gu NO-GAP, iztfi_drdw1-bukrs UNDER t_bukrs NO-GAP,
                t_gu NO-GAP, iztfi_drdw1-belnr UNDER t_belnr NO-GAP,
                t_gu NO-GAP, iztfi_drdw1-buzei UNDER t_buzei NO-GAP,
                t_gu NO-GAP, iztfi_drdw1-gsber UNDER t_gsber NO-GAP,
                t_gu NO-GAP, iztfi_drdw1-gjahr UNDER t_gjahr NO-GAP,
                t_gu NO-GAP, iztfi_drdw1-datum UNDER t_datum NO-GAP,
                t_gu NO-GAP, (2)iztfi_drdw1-koart UNDER t_koart NO-GAP,
                t_gu NO-GAP, (2)iztfi_drdw1-umskz UNDER t_umskz NO-GAP,
                t_gu NO-GAP, wa_hkont     UNDER t_hkont NO-GAP,
                t_gu NO-GAP, sv_hkontn    UNDER t_hkontn NO-GAP,
                t_gu NO-GAP, iztfi_drdw1-dispw UNDER t_dispw NO-GAP,
                t_gu NO-GAP, iztfi_drdw1-wrshb UNDER t_wrshb NO-GAP
                             CURRENCY iztfi_drdw-dispw ROUND p_skalv
                             DECIMALS p_skaln NO-ZERO,
                t_gu NO-GAP, iztfi_drdw1-dmbtr UNDER t_dmbtr NO-GAP
                             CURRENCY sv_waers ROUND p_skalv
                             DECIMALS p_skaln NO-ZERO,
                t_gu NO-GAP, iztfi_drdw-avdat
                             UNDER t_avdat NO-GAP NO-ZERO,
                t_gu NO-GAP, (6)iztfi_drdw1-bstat UNDER t_bstat NO-GAP,
                t_gu NO-GAP.
    HIDE : iztfi_drdw1-bukrs, iztfi_drdw1-belnr, iztfi_drdw1-gjahr.
*   CLEAR : iztfi_drdw1, wa_hkont.
    CLEAR wa_hkont.

    l_sum_wrshb = l_sum_wrshb + iztfi_drdw1-wrshb.
    l_sum_dmbtr = l_sum_dmbtr + iztfi_drdw1-dmbtr.
    AT LAST.
*     SUM.

*     FORMAT COLOR 2.
      FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

      ULINE.
      WRITE :/  t_gu NO-GAP, 'Total' UNDER t_grupp,
                l_sum_wrshb UNDER t_wrshb CURRENCY sv_waers
                            ROUND p_skalv DECIMALS p_skaln NO-ZERO,
                l_sum_dmbtr UNDER t_dmbtr
                            CURRENCY iztfi_drdw1-dispw
                            ROUND p_skalv DECIMALS p_skaln NO-ZERO,
             (16) space, t_gu NO-GAP.
    ENDAT.
  ENDLOOP.
  FORMAT COLOR OFF.
  ULINE.
  CLEAR : wa_twrshb, wa_tdmbtr.
ENDFORM.                    " write_iztfi_drdw_rtn
**&---------------------------------------------------------------------
*
**&      Form  write_iaztfi_drdw_rtn
**&---------------------------------------------------------------------
*
*FORM write_iaztfi_drdw_rtn.
*  CLEAR : iaztfi_drdw, wa_hkont, wa_gwrshb,
*          wa_twrshb, wa_gdmbtr, wa_tdmbtr.
*  SORT iaztfi_drdw BY grupp ebene.
*  FORMAT COLOR 2.
*  LOOP AT iaztfi_drdw.
*    mode = sy-tabix MOD 2.
*    PERFORM set_mode USING mode.
*    CHECK iaztfi_drdw-bstat = 'A'.
*    CLEAR : wa_hkont, sv_hkontn, sv_bukrs.
*    MOVE    iaztfi_drdw-bukrs  TO  sv_bukrs.
*    CASE iaztfi_drdw-koart.
*      WHEN 'S'.
*        MOVE iaztfi_drdw-hkont TO wa_hkont.
*        PERFORM read_hkont.
*      WHEN 'K'.
*        MOVE iaztfi_drdw-lifnr TO wa_hkont.
*        PERFORM read_lifnr.
*      WHEN 'D'.
*        MOVE iaztfi_drdw-kunnr TO wa_hkont.
*        PERFORM read_kunnr.
*    ENDCASE.
*    AT FIRST.
*      NEW-LINE.
*      WRITE : '????'.
*      ULINE.
*    ENDAT.
*    WRITE : /   t_gu NO-GAP, iaztfi_drdw-grupp UNDER t_grupp NO-GAP,
*                t_gu NO-GAP, iaztfi_drdw-ebene UNDER t_ebene NO-GAP,
*                t_gu NO-GAP, iaztfi_drdw-bukrs UNDER t_bukrs NO-GAP,
*                t_gu NO-GAP, iaztfi_drdw-belnr UNDER t_belnr NO-GAP,
*                t_gu NO-GAP, iaztfi_drdw-buzei UNDER t_buzei NO-GAP,
*                t_gu NO-GAP, iaztfi_drdw-gsber UNDER t_gsber NO-GAP,
*                t_gu NO-GAP, iaztfi_drdw-gjahr UNDER t_gjahr NO-GAP,
*                t_gu NO-GAP, iaztfi_drdw-datum UNDER t_datum NO-GAP,
*                t_gu NO-GAP, iaztfi_drdw-koart UNDER t_koart NO-GAP,
*                t_gu NO-GAP, iaztfi_drdw-umskz UNDER t_umskz NO-GAP,
*                t_gu NO-GAP, wa_hkont     UNDER t_hkont NO-GAP,
*                t_gu NO-GAP, sv_hkontn    UNDER t_hkontn NO-GAP,
*                t_gu NO-GAP, iaztfi_drdw-dispw UNDER t_dispw NO-GAP,
*                t_gu NO-GAP, iaztfi_drdw-wrshb UNDER t_wrshb NO-GAP
*                                 CURRENCY awaer ROUND p_skalv
*                                 DECIMALS p_skaln NO-ZERO,
*                t_gu NO-GAP, iaztfi_drdw-dmbtr UNDER t_dmbtr NO-GAP
*                                 CURRENCY SV_WAERS ROUND p_skalv
*                                 DECIMALS p_skaln NO-ZERO,
*                t_gu NO-GAP, iaztfi_drdw-avdat
*                             UNDER t_avdat NO-GAP NO-ZERO,
*                t_gu NO-GAP, iaztfi_drdw-bstat UNDER t_bstat NO-GAP,
*                t_gu NO-GAP.
*    HIDE : iaztfi_drdw-bukrs, iaztfi_drdw-belnr, iaztfi_drdw-gjahr.
*    ADD iaztfi_drdw-wrshb TO wa_gwrshb.
*    ADD iaztfi_drdw-wrshb TO wa_twrshb.
*    ADD iaztfi_drdw-dmbtr TO wa_gdmbtr.
*    ADD iaztfi_drdw-dmbtr TO wa_tdmbtr.
*    CLEAR : iaztfi_drdw-bukrs, iaztfi_drdw-belnr,
*            iaztfi_drdw-gjahr, wa_hkont.
*  ENDLOOP.
*  ULINE.
**  WRITE : / T_GU NO-GAP.
*  FORMAT COLOR OFF.
*  FORMAT COLOR 4.
*  WRITE :/  '???', wa_twrshb UNDER t_wrshb CURRENCY awaer
*                         ROUND p_skalv DECIMALS p_skaln NO-ZERO,
*                     wa_tdmbtr UNDER t_dmbtr CURRENCY SV_WAERS
*                         ROUND p_skalv DECIMALS p_skaln NO-ZERO.
*  FORMAT COLOR OFF.
**  WRITE : T_GU NO-GAP.
*  ULINE.
*  CLEAR : wa_twrshb, wa_tdmbtr.
*ENDFORM.                    " write_iaztfi_drdw_rtn
**&---------------------------------------------------------------------
*
**&      Form  write_irztfi_drdw_rtn
**&---------------------------------------------------------------------
*
*FORM write_irztfi_drdw_rtn.
*  CLEAR : irztfi_drdw, wa_hkont, wa_gwrshb, wa_twrshb.
*  SORT irztfi_drdw BY grupp ebene.
*  FORMAT COLOR 2.
*  LOOP AT irztfi_drdw.
*    mode = sy-tabix MOD 2.
*    PERFORM set_mode USING mode.
*    CHECK irztfi_drdw-bstat = 'R'.
*    CLEAR : wa_hkont, sv_hkontn, sv_bukrs.
*    MOVE    irztfi_drdw-bukrs  TO  sv_bukrs.
*    CASE irztfi_drdw-koart.
*      WHEN 'S'.
*        MOVE irztfi_drdw-hkont TO wa_hkont.
*        PERFORM read_hkont.
*      WHEN 'K'.
*        MOVE irztfi_drdw-lifnr TO wa_hkont.
*        PERFORM read_lifnr.
*      WHEN 'D'.
*        MOVE irztfi_drdw-kunnr TO wa_hkont.
*        PERFORM read_kunnr.
*    ENDCASE.
*    AT FIRST.
*      NEW-LINE.
*      WRITE : 'Reverse'.
*      ULINE.
*    ENDAT.
*    WRITE : /   t_gu NO-GAP, irztfi_drdw-grupp UNDER t_grupp NO-GAP,
*                t_gu NO-GAP, irztfi_drdw-ebene UNDER t_ebene NO-GAP,
*                t_gu NO-GAP, irztfi_drdw-bukrs UNDER t_bukrs NO-GAP,
*                t_gu NO-GAP, irztfi_drdw-belnr UNDER t_belnr NO-GAP,
*                t_gu NO-GAP, irztfi_drdw-buzei UNDER t_buzei NO-GAP,
*                t_gu NO-GAP, irztfi_drdw-gsber UNDER t_gsber NO-GAP,
*                t_gu NO-GAP, irztfi_drdw-gjahr UNDER t_gjahr NO-GAP,
*                t_gu NO-GAP, irztfi_drdw-datum UNDER t_datum NO-GAP,
*                t_gu NO-GAP, irztfi_drdw-koart UNDER t_koart NO-GAP,
*                t_gu NO-GAP, irztfi_drdw-umskz UNDER t_umskz NO-GAP,
*                t_gu NO-GAP, wa_hkont     UNDER t_hkont NO-GAP,
*                t_gu NO-GAP, sv_hkontn    UNDER t_hkontn NO-GAP,
*                t_gu NO-GAP, irztfi_drdw-dispw UNDER t_dispw NO-GAP,
*                t_gu NO-GAP, irztfi_drdw-wrshb UNDER t_wrshb NO-GAP
*                                 CURRENCY awaer ROUND p_skalv
*                                 DECIMALS p_skaln NO-ZERO,
*                t_gu NO-GAP, irztfi_drdw-dmbtr UNDER t_dmbtr NO-GAP
*                                 CURRENCY SV_WAERS ROUND p_skalv
*                                 DECIMALS p_skaln NO-ZERO,
*                t_gu NO-GAP, irztfi_drdw-avdat
*                             UNDER t_avdat NO-GAP NO-ZERO,
*                t_gu NO-GAP, irztfi_drdw-bstat UNDER t_bstat NO-GAP,
*                t_gu NO-GAP.
*    HIDE : irztfi_drdw-bukrs, irztfi_drdw-belnr, irztfi_drdw-gjahr.
*    ADD irztfi_drdw-wrshb TO wa_gwrshb.
*    ADD irztfi_drdw-wrshb TO wa_twrshb.
*    ADD irztfi_drdw-dmbtr TO wa_gdmbtr.
*    ADD irztfi_drdw-dmbtr TO wa_tdmbtr.
*    CLEAR : irztfi_drdw-bukrs,
*            irztfi_drdw-belnr,
*            irztfi_drdw-gjahr,
*            wa_hkont.
*  ENDLOOP.
*  ULINE.
**  WRITE : / T_GU NO-GAP.
*  FORMAT COLOR OFF.
*  FORMAT COLOR 4.
*  WRITE : / 'Total Reverse', wa_twrshb UNDER t_wrshb CURRENCY awaer
*                         ROUND p_skalv DECIMALS p_skaln NO-ZERO,
*                       wa_tdmbtr UNDER t_dmbtr CURRENCY SV_WAERS
*                         ROUND p_skalv DECIMALS p_skaln NO-ZERO.
*  FORMAT COLOR OFF.
**  WRITE : T_GU NO-GAP.
*  ULINE.
*  CLEAR : wa_twrshb, wa_tdmbtr.
*ENDFORM.                    " write_irztfi_drdw_rtn
*&---------------------------------------------------------------------*
*&      Form  SET_MODE
*&---------------------------------------------------------------------*
FORM set_mode USING    p_mode.
  IF p_mode EQ 0.
    FORMAT INTENSIFIED ON.
  ELSE.
    FORMAT INTENSIFIED OFF.
  ENDIF.
ENDFORM.                    " SET_MODE
*&---------------------------------------------------------------------*
*&      Form  READ_HKONT
*&---------------------------------------------------------------------*
FORM read_hkont.
  CLEAR sv_hkontn.
  SELECT SINGLE txt20 INTO sv_hkontn FROM skat WHERE spras = sy-langu
                              AND ktopl = sv_ktopl
                              AND saknr = wa_hkont.
ENDFORM.                    " READ_HKONT
*&---------------------------------------------------------------------*
*&      Form  READ_LIFNR
*&---------------------------------------------------------------------*
FORM read_lifnr.
  SELECT SINGLE name1 INTO sv_hkontn FROM lfa1
   WHERE lifnr = wa_hkont AND land1 = 'KR'.
ENDFORM.                    " READ_LIFNR
*&---------------------------------------------------------------------*
*&      Form  READ_KUNNR
*&---------------------------------------------------------------------*
FORM read_kunnr.
  SELECT SINGLE name1 INTO sv_hkontn FROM kna1
   WHERE kunnr = wa_hkont  AND land1 = 'KR'.
ENDFORM.                    " READ_KUNNR
*&---------------------------------------------------------------------*
*&      Form  EXEC_SORT
*&---------------------------------------------------------------------*
FORM exec_sort USING   up_dn.
  DATA:fld_name(20),
       name(10).
* GET CURSOR FIELD fld_name.
* name = fld_name+7(10).
  GET CURSOR FIELD fld_name.
  IF sy-lilli EQ '2'.  "Header
    name = fld_name+2(8).
  ELSE.
    name = fld_name+12(8).
  ENDIF.
  CHECK name NE space.

  CASE up_dn.
    WHEN 'A'.                              "..Sort Up
      SORT iztfi_drdw1 BY (name).
      SORT iaztfi_drdw BY (name).
      SORT irztfi_drdw BY (name).
    WHEN 'D'.                              "..Sort Down
      SORT iztfi_drdw1 BY (name) DESCENDING.
      SORT iaztfi_drdw BY (name) DESCENDING.
      SORT irztfi_drdw BY (name) DESCENDING.
  ENDCASE.
  sy-lsind = sy-lsind - 1.
  PERFORM write-rtn.

ENDFORM.                    " EXEC_SORT
