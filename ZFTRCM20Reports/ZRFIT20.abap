*&--------------------------------------------------------------------
*& Author                 : JIPARK
*& Creation Date          : 14/01/2004
*& Specification By       : JIPARK
*& Pattern                : Report 1-15
*& Development Request No : UD1K905774
*& Addl documentation     :
*& Description  : Create L&F Lineitem
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------

REPORT  zrfit20 NO STANDARD PAGE HEADING.

TABLES: t001,
        bkpf,                    " ??????
        bseg,                    " ????????
        ztfi_drcm,               " ???? ??(BSEG -> ztfi_drdw) ??
        ztfi_drdw,               " TR-CM G/L line item
        skb1,                    " G/L ????? (????)
        vbsegs,                  " G/L ??? ?? ???? ??????
        vbsegd,                  " ??????? ?? ??????
        vbsegk.                  " ???? ?? ???? ??????

DATA: BEGIN OF ibkpf OCCURS 0,
        bukrs  LIKE bkpf-bukrs,
        belnr  LIKE bkpf-belnr,
        gjahr  LIKE bkpf-gjahr,
        cpudt  LIKE bkpf-cpudt,
        cputm  LIKE bkpf-cputm,
        aedat  LIKE bkpf-aedat,
        waers  LIKE bkpf-waers,
        bstat  LIKE bkpf-bstat,
        stblg  LIKE bkpf-stblg,
      END   OF ibkpf.

DATA: BEGIN OF ibkpf1 OCCURS 0,
        bukrs  LIKE bkpf-bukrs,
        belnr  LIKE bkpf-belnr,
        gjahr  LIKE bkpf-gjahr,
        cpudt  LIKE bkpf-cpudt,
        cputm  LIKE bkpf-cputm,
        aedat  LIKE bkpf-aedat,
        waers  LIKE bkpf-waers,
        bstat  LIKE bkpf-bstat,
        stblg  LIKE bkpf-stblg,
      END   OF ibkpf1.

DATA: BEGIN OF ibseg OCCURS 0,
        bukrs  LIKE bseg-bukrs,
        belnr  LIKE bseg-belnr,
        gjahr  LIKE bseg-gjahr,
        buzei  LIKE bseg-buzei,
        fdgrp  LIKE bseg-fdgrp,
        fdlev  LIKE bseg-fdlev,
        fdtag  LIKE bseg-fdtag, "???
        zfbdt  LIKE bseg-zfbdt, "?????
        gsber  LIKE bseg-gsber,
        pswsl  LIKE bseg-pswsl,
        augdt  LIKE bseg-augdt,
        koart  LIKE bseg-koart,
        hkont  LIKE bseg-hkont,
        lifnr  LIKE bseg-lifnr,
        kunnr  LIKE bseg-kunnr,
        fdwbt  LIKE bseg-fdwbt,
        dmbtr  LIKE bseg-dmbtr,
        xopvw  LIKE bseg-xopvw,
        umskz  LIKE bseg-umskz,
        augbl  LIKE bseg-augbl,
        stblg  LIKE bkpf-stblg,
        bstat  LIKE bkpf-bstat,
      END   OF ibseg.

DATA: w_bseg LIKE ibseg OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF idisp OCCURS 0,
        bukrs  LIKE bseg-bukrs,
        belnr  LIKE bseg-belnr,
        gjahr  LIKE bseg-gjahr,
        buzei  LIKE bseg-buzei,
        augbl  LIKE bseg-augbl,
*       STBLG  LIKE BKPF-STBLG,
        datum  LIKE bseg-FDTAG, "planning date
        koart  LIKE bseg-koart,
        bstat  LIKE bkpf-bstat,
        umskz  LIKE bseg-umskz,
        grupp  LIKE bseg-fdgrp,
        ebene  LIKE bseg-fdlev,
        lifnr  LIKE bseg-lifnr,
        kunnr  LIKE bseg-kunnr,
        hkont  LIKE bseg-hkont,
        dispw  LIKE fdsr-dispw,
        wrshb  LIKE fdsr-wrshb,
        dmbtr  LIKE ztfi_drdw-dmbtr,
*        CREAD,
*        CDELE,
*        CINSE,
*        CREVE,
*        CAUGB,
      END   OF idisp.


DATA: BEGIN OF iztfi_drdw OCCURS 0.
        INCLUDE STRUCTURE ztfi_drdw.
DATA: END   OF iztfi_drdw.
DATA: BEGIN OF dztfi_drdw OCCURS 0.
        INCLUDE STRUCTURE ztfi_drdw.
DATA: END   OF dztfi_drdw.

*????
DATA: BEGIN OF i_t035 OCCURS 0,
        grupp  LIKE t035-grupp,
        ebene  LIKE t035-ebene,
      END OF i_t035.
* Special GL
DATA: BEGIN OF i_t074 OCCURS 0,
        koart  LIKE t074-koart,
        umskz  LIKE t074-umskz,
        skont  LIKE t074-hkont,
        ebene  LIKE t074-ebene,
      END OF i_t074.


*plan level
DATA: BEGIN OF i_skb1 OCCURS 0,
        bukrs LIKE skb1-bukrs,
        saknr LIKE skb1-saknr,
        fdlev LIKE skb1-fdlev,
      END OF i_skb1.
*??????...
DATA: BEGIN OF i_t037s OCCURS 0,
        rantyp   LIKE t037s-rantyp,
        rrefkont LIKE t037s-rrefkont,
        hkont    LIKE t037s-hkont,
      END OF i_t037s.


DATA: tstlo(14),
      old_last_run_tstlo(14),
      old_last_run_tstlo_p(14),
      btstlo(14),
      cputm                LIKE bkpf-cputm,
      cpudt                LIKE bkpf-cpudt,
*     old_cputm            like bkpf-cputm,
      old_cpudt            LIKE bkpf-cpudt,
      old_cpudt_p          LIKE bkpf-cpudt,
      chk_date(14).
DATA: t_cnt                TYPE i   ,
      u_cnt                TYPE i   ,
      u1_cnt               TYPE i   ,
      d_cnt                TYPE i   ,
      bkpf_cnt             TYPE i   ,
      bkpf1_cnt            TYPE i   ,
      bseg_cnt             TYPE i   ,
      t_count              TYPE i   ,
      cnt_dele             TYPE i   ,
      cnt_inse             TYPE i   ,
      cnt_reve             TYPE i   ,
      cnt_augb             TYPE i   ,
      real_cnt             TYPE i   VALUE 0,
      park_cnt             TYPE i   VALUE 0,
      answer.
DATA : h_gu          VALUE  '|',
       h_bukrs(4)    VALUE  'CoCd',
       h_belnr(10)   VALUE  'Document',
       h_gjahr(4)    VALUE  'Year',
       h_buzei(3)    VALUE  'ID',
       h_augbl(10)   VALUE  'ClearDoc',
       h_fdtag(10)   VALUE  'Pln.Date',
       h_stblg(10)   VALUE  'RevsDoc',
       h_koart(1)    VALUE  'A',
       h_accnt(10)   VALUE  'Account',
       h_umskz(1)    VALUE  'S',
       h_bstat(4)    VALUE  'Stat',
       h_fdgrp(10)   VALUE  'Pln.Grp.',
       h_fdlev(4)    VALUE  'Lvl.',
       h_cdele(4)    VALUE  'DELE',
       h_cinse(4)    VALUE  'INSE'.
*      H_CREVE(4)    VALUE  'REVE',
*      H_CAUGB(4)    VALUE  'AUGB'.

CONSTANTS: l_amt TYPE i VALUE 16,
           l_04  TYPE i VALUE 04.

*&====================================================================&*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME  TITLE text-011.
PARAMETERS:      p_bukrs  LIKE  bkpf-bukrs OBLIGATORY MEMORY ID buk.
SELECTION-SCREEN ULINE.
PARAMETERS:     p_real    AS CHECKBOX DEFAULT 'X',   " Real
                p_park    AS CHECKBOX.   " Include Park Doc
*ARAMETERS : P_CLD    AS CHECKBOX.       "???? ????...
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-012.
SELECT-OPTIONS: p_gjahr FOR bkpf-gjahr NO INTERVALS,
                p_belnr FOR bkpf-belnr, "NO INTERVALS,
                p_budat FOR bkpf-budat.
*..(Data Conversion)
PARAMETERS:p_conv  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl2.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
SELECT-OPTIONS s_tstlo FOR syst-datum.
SELECTION-SCREEN END OF BLOCK b2.
*DB read
SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-013.
*DB read
PARAMETERS: p_dbread AS CHECKBOX.
SELECT-OPTIONS: p_grupp FOR ztfi_drdw-grupp,
                p_ebene FOR ztfi_drdw-ebene,
                p_datum FOR ztfi_drdw-datum,
                p_lifnr FOR ztfi_drdw-lifnr,
                p_kunnr FOR ztfi_drdw-kunnr.

SELECTION-SCREEN END OF BLOCK bl3.
*&====================================================================&*
*TOP-OF-PAGE.
*
*  WRITE : / h_bukrs, h_belnr, h_gjahr, h_buzei, h_augbl, h_stblg,
*            h_koart, h_bstat, h_fdgrp, h_fdlev, h_cdele, h_cinse.
**            h_creve, h_caugb.

*&====================================================================&*
AT SELECTION-SCREEN.
  IF NOT s_tstlo-low IS INITIAL.
    IF s_tstlo-high IS INITIAL.
      MESSAGE e398(00) WITH 'Input Executing date FROM - TO.'.
    ENDIF.
  ENDIF.
  IF NOT s_tstlo-high IS INITIAL.
    IF s_tstlo-low IS INITIAL.
      MESSAGE e398(00) WITH 'Input Executing date FROM - TO.'.
    ENDIF.
  ENDIF.
  IF  s_tstlo-high > sy-datum OR s_tstlo-low > sy-datum.
    MESSAGE e398(00) WITH 'Check Executing date!'.
  ENDIF.

START-OF-SELECTION.
  IF p_dbread = 'X'.
    SELECT *  INTO CORRESPONDING FIELDS OF TABLE idisp
    FROM ztfi_drdw
    WHERE bukrs = p_bukrs
      AND gjahr IN p_gjahr
      AND belnr IN p_belnr
      AND datum IN p_datum
      AND grupp IN p_grupp
      AND ebene IN p_ebene.
    PERFORM display_data.

  ELSE.
    PERFORM  read_setting.
    PERFORM  set_cpudt_cputm_tstlo.      " set date/time
    PERFORM  bkpf_moveto_ibkpf.          " select bkpf data
    PERFORM  loop_at_ibkpf.              " select bseg data
    PERFORM  loop_at_ibseg.              " delete/insert ztfi_drdw
    PERFORM  insert_ztfi_drcm.                " insert ztfi_drcm

*  check P_DISP <> 'X'.
    NEW-PAGE NO-HEADING.
    WRITE:/ text-001  , ' : ',   p_bukrs    ,
          / text-002  , ' : ',   tstlo      ,
          / text-003  , '   : ', syst-uname ,
          / text-004  , ' : ',   real_cnt LEFT-JUSTIFIED,
          / text-005  , ' : ',   park_cnt LEFT-JUSTIFIED.

  ENDIF.
  SET PF-STATUS 'MAIN'.

************************************************************************
* AT LINE-SELECTION                                                    *
************************************************************************
AT LINE-SELECTION.
  DATA:fld_name(20).
  GET CURSOR FIELD fld_name.

  CASE fld_name.
    WHEN 'IDISP-BELNR'.
      SET PARAMETER ID:'BLN' FIELD idisp-belnr,
                       'BUK' FIELD idisp-bukrs,
                       'GJR' FIELD idisp-gjahr.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDCASE.
************************************************************************
* AT USER-COMMAND                                                     *
************************************************************************
AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'SOUP'.  PERFORM exec_sort  USING 'U'.
    WHEN 'SODN'.  PERFORM exec_sort  USING 'D'.
*    WHEN 'DOWN'.  PERFORM DATA_DOWNLOAD.
  ENDCASE.

*&---------------------------------------------------------------------*
*&  Form  LOOP_AT_IBKPF
*&---------------------------------------------------------------------*
*&  ????? ????? ??? ??? ??.
*&---------------------------------------------------------------------*
FORM loop_at_ibkpf.
  CHECK bkpf_cnt > 0.
  CLEAR ibkpf.
  LOOP AT ibkpf.
    IF  p_real = 'X' AND ibkpf-bstat <> 'V'.
      PERFORM select_from_bseg.
    ENDIF.

    CHECK p_park = 'X' AND ibkpf-bstat = 'V'.
    PERFORM select_from_vbsegs.  "parked doc.- GL
    PERFORM select_from_vbsegd.  "parked doc.- Customer
    PERFORM select_from_vbsegk.  "parked doc.- Vendor
  ENDLOOP.

  DESCRIBE TABLE ibseg LINES bseg_cnt .

ENDFORM.                               " LOOP_AT_IBKPF
*&---------------------------------------------------------------------*
*&  Form  LOOP_AT_IBSEG
*&---------------------------------------------------------------------*
*&  ztfi_drdw? ?? ????? ???? ????
*&---------------------------------------------------------------------*
FORM loop_at_ibseg.

  DATA: BEGIN OF ibelnr OCCURS 0,
          bukrs  LIKE bseg-bukrs,
          belnr  LIKE bseg-belnr,
          gjahr  LIKE bseg-gjahr,
        END   OF ibelnr.
  CHECK bkpf_cnt > 0 AND bseg_cnt > 0.
  CLEAR ibseg.
  LOOP AT ibseg.
    CLEAR ztfi_drdw.

*???? ??? ?????
    IF ibseg-fdtag IS INITIAL.
      ibseg-fdtag = ibseg-zfbdt.
    ENDIF.

    SELECT  SINGLE *  FROM  ztfi_drdw
                          WHERE  bukrs   =  ibseg-bukrs
                            AND  belnr   =  ibseg-belnr
                            AND  gjahr   =  ibseg-gjahr
                            AND  buzei   =  ibseg-buzei.
    IF sy-subrc = 0.
      IF  ibseg-fdgrp = ztfi_drdw-grupp  AND
          ibseg-fdlev = ztfi_drdw-ebene  AND
          ibseg-fdtag = ztfi_drdw-datum  AND
          ibseg-gsber = ztfi_drdw-gsber  AND
          ibseg-bstat = ztfi_drdw-bstat.
        CONTINUE.
      ELSE.
        IF ibseg-bstat = 'V'.
          CHECK p_park = 'X'.
          park_cnt = park_cnt + 1.
        ELSE.
          CHECK p_real = 'X'.
          real_cnt = real_cnt + 1.
        ENDIF.
*        MOVE-CORRESPONDING ztfi_drdw TO Dztfi_drdw. "??? ?? table
*        APPEND Dztfi_drdw.
        PERFORM modify_iztfi_drdw.
      ENDIF.
    ELSE.
      IF ibseg-bstat = 'V'.
        CHECK p_park = 'X'.
        park_cnt = park_cnt + 1.
      ELSE.
        CHECK p_real = 'X'.
        real_cnt = real_cnt + 1.
      ENDIF.
      PERFORM append_iztfi_drdw.
    ENDIF.
  ENDLOOP.


  PERFORM insert_ztfi_drdw. " ON COMMIT.

*  PERFORM UPDATE_ztfi_drdw. " ON COMMIT.     "????? ?? UPDATE.

ENDFORM.                               " LOOP_AT_IBSEG
*&---------------------------------------------------------------------*
*&  Form  BKPF_MOVETO_IBKPF
*&  ?????? ?? ???? ??? ?? ???? ???? ??? ??
*&    ?? ??(??,?? ???) ?? ? ?? DATA READ
*&      (???????? aedat?? ??? ?? ?? ??)
*&---------------------------------------------------------------------*
FORM bkpf_moveto_ibkpf.

  CHECK p_real = 'X' OR p_park = 'X'.
  CLEAR: ibkpf.

  IF p_conv = 'X'.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE ibkpf
      FROM bkpf
      WHERE bukrs EQ p_bukrs
        AND gjahr IN p_gjahr
        AND belnr IN p_belnr
        AND budat IN p_budat
        AND stblg = space.
  ELSE.
* Delta?? ????.
    SELECT bukrs belnr gjahr cpudt cputm aedat waers bstat stblg
                       FROM bkpf
                       INTO CORRESPONDING FIELDS OF ibkpf
                      WHERE bukrs  = p_bukrs
*                      AND AWTYP NE 'MKPF'     "????
*                     AND ( CPUDT => OLD_CPUDT OR AEDAT <> '00000000' ).
                        AND ( ( cpudt >= old_cpudt AND cpudt <= cpudt )
                         OR ( aedat >= old_cpudt AND aedat <= cpudt ) )
                        AND stblg = space.
      IF ibkpf-aedat IS INITIAL.
        CONCATENATE ibkpf-cpudt ibkpf-cputm INTO btstlo.
        IF ibkpf-bstat = 'V'.            " ????
          CHECK btstlo > old_last_run_tstlo_p  AND  btstlo < tstlo.
        ELSE.
          CHECK  btstlo > old_last_run_tstlo    AND  btstlo < tstlo.
        ENDIF.
      ELSE.
* Since there is no time field you have to have a larger set
        IF ibkpf-bstat = 'V'.
          CHECK ibkpf-aedat => old_cpudt_p AND ibkpf-aedat <= cpudt.
        ELSE.
          CHECK ibkpf-aedat => old_cpudt   AND ibkpf-aedat <= cpudt.
        ENDIF.
      ENDIF.
      APPEND ibkpf.
    ENDSELECT.
  ENDIF.

  DESCRIBE TABLE ibkpf LINES bkpf_cnt .

ENDFORM.                               " BKPF_MOVETO_IBKPF
*&---------------------------------------------------------------------*
*&      Form  SET_CPUDT_CPUTM_TSTLO
*&  ???? & ????? ????? ?? ??&??? ???.
*&  ?? ????&??? ??? ??(ztfi_drcm)
*&---------------------------------------------------------------------*
FORM set_cpudt_cputm_tstlo.
  CONCATENATE sy-datum sy-uzeit INTO chk_date.
  CHECK  p_real = 'X' OR p_park = 'X'.
  IF p_real = 'X'.
    CLEAR: old_cpudt, cpudt, cputm, tstlo, old_last_run_tstlo.
    SELECT  okcod tstlo   UP TO 1 ROWS
                        INTO (ztfi_drcm-okcod, old_last_run_tstlo)
                        FROM  ztfi_drcm
                       WHERE bukrs = p_bukrs
                         AND reald = 'X'
*                         AND OKCOD = 'X'
                         AND tstlo < chk_date
                       ORDER BY tstlo DESCENDING.
      IF ztfi_drcm-okcod = space.
*        MESSAGE S420(FB) WITH TEXT-006.
*        STOP.
*       PERFORM confirm_exit_rtn.
*       IF answer <> 'J'. STOP. ENDIF.
        CLEAR : ztfi_drcm-okcod,  old_last_run_tstlo.
        SELECT  okcod tstlo   UP TO 1 ROWS
          INTO (ztfi_drcm-okcod, old_last_run_tstlo)
          FROM  ztfi_drcm
         WHERE bukrs = p_bukrs
           AND okcod = 'X'
           AND ( reald = 'X' OR parkd = 'X' )
         ORDER BY tstlo DESCENDING.
          EXIT.
        ENDSELECT.
        IF    sy-subrc  <> 0.
          old_last_run_tstlo = '00000000000000'.
        ENDIF.
      ENDIF.
      EXIT.
    ENDSELECT.
    IF NOT s_tstlo-low IS INITIAL.
      CONCATENATE s_tstlo-low '000000' INTO old_last_run_tstlo.
    ENDIF.
*    IF SY-SUBRC = 0 AND  OLD_LAST_RUN_TSTLO <> '00000000000000'   .
*      OLD_CPUDT = OLD_LAST_RUN_TSTLO+0(8).
*    ENDIF.
    IF sy-subrc = 0.
      old_cpudt = old_last_run_tstlo+0(8).
    ENDIF.
  ENDIF.
  IF p_park = 'X'.
    CLEAR old_cpudt_p.
    SELECT  okcod tstlo   UP TO 1 ROWS
                        INTO (ztfi_drcm-okcod, old_last_run_tstlo_p)
                        FROM  ztfi_drcm
                       WHERE bukrs = p_bukrs
                         AND parkd = 'X'
                       ORDER BY tstlo DESCENDING.
      IF ztfi_drcm-okcod = space.
*       PERFORM confirm_exit_rtn.
*       IF answer <> 'J'. STOP. ENDIF.
        CLEAR : ztfi_drcm-okcod,  old_last_run_tstlo_p.
        SELECT  okcod tstlo   UP TO 1 ROWS
          INTO (ztfi_drcm-okcod, old_last_run_tstlo_p)
          FROM  ztfi_drcm
         WHERE bukrs = p_bukrs
           AND okcod = 'X'
           AND parkd = 'X'
         ORDER BY tstlo DESCENDING.
          EXIT.
        ENDSELECT.
        IF    sy-subrc  <> 0.
          old_last_run_tstlo_p = '00000000000000'.
        ENDIF.
      ENDIF.
      EXIT.
    ENDSELECT.
    IF NOT s_tstlo-low IS INITIAL.
      CONCATENATE s_tstlo-low '000000' INTO old_last_run_tstlo_p.
    ENDIF.
*    IF SY-SUBRC = 0 AND  OLD_LAST_RUN_TSTLO_P <> '00000000000000'.
*      OLD_CPUDT_P = OLD_LAST_RUN_TSTLO_P+0(8).
*    ENDIF.
    IF sy-subrc = 0.
      old_cpudt_p = old_last_run_tstlo_p+0(8).
    ENDIF.
  ENDIF.
  IF      NOT  s_tstlo-low  IS INITIAL
     AND  NOT  s_tstlo-high IS INITIAL.
    cpudt = s_tstlo-high.
    cputm = '999999'.
  ELSE.
    cpudt = syst-datum.
    cputm = syst-uzeit.
  ENDIF.
  CONCATENATE cpudt cputm INTO tstlo.
  CLEAR ztfi_drcm.
  ztfi_drcm-bukrs = p_bukrs.
  ztfi_drcm-tstlo = tstlo.
  ztfi_drcm-uname = syst-uname.
  ztfi_drcm-okcod = space.
  IF p_real = 'X'.
    ztfi_drcm-reald = 'X'.
  ENDIF.
  IF p_park = 'X'.
    ztfi_drcm-parkd = 'X'.
  ENDIF.
  INSERT ztfi_drcm.
  COMMIT WORK.

ENDFORM.                               " SET_CPUDT_CPUTM_TSTLO
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_EXIT_RTN
*&---------------------------------------------------------------------*
FORM confirm_exit_rtn.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
*   DEFAULTOPTION        = 'Y'
      textline1            = '??? ??? ?? ?? ?? ?????.'
      textline2            = '????? ???????'
      titel                = ' '
*   START_COLUMN         = 25
*   START_ROW            = 6
     cancel_display       = ''
   IMPORTING
     answer               =  answer .

ENDFORM.                    " CONFIRM_EXIT_RTN
*&---------------------------------------------------------------------*
*&      Form  INSERT_ztfi_drcm
*&---------------------------------------------------------------------*
FORM insert_ztfi_drcm.
  IF  p_real = 'X' OR p_park = 'X'.
    PERFORM update_ztfi_drcm.
    COMMIT WORK.
  ENDIF.

ENDFORM.                               " INSERT_ztfi_drcm
*&---------------------------------------------------------------------*
*&      Form  APPEND_Iztfi_drdw
*&---------------------------------------------------------------------*
FORM append_iztfi_drdw.

  CLEAR iztfi_drdw.
  MOVE-CORRESPONDING ibseg TO iztfi_drdw.

  iztfi_drdw-grupp = ibseg-fdgrp.            " ????
  iztfi_drdw-ebene = ibseg-fdlev.            " ????
  iztfi_drdw-dispw = ibseg-pswsl.            " ????

  IF ibseg-fdtag IS INITIAL.           " ???/???? ??
    iztfi_drdw-datum = ibseg-augdt.          " ????? = ???
  ELSE.
    iztfi_drdw-datum = ibseg-fdtag.          " ????? = ?????
  ENDIF.
  iztfi_drdw-avdat = ibseg-augdt.            " ??? = ???
  iztfi_drdw-dmbtr = ibseg-dmbtr.            " ??????
  iztfi_drdw-wrshb = ibseg-fdwbt.            " ????

  APPEND iztfi_drdw.

ENDFORM.                               " APPEND_Iztfi_drdw
*&---------------------------------------------------------------------*
*&      Form  MODIFY_Iztfi_drdw
*&---------------------------------------------------------------------*
FORM modify_iztfi_drdw.

  CLEAR dztfi_drdw.
  MOVE-CORRESPONDING ibseg TO dztfi_drdw.

  dztfi_drdw-grupp = ibseg-fdgrp.            " ????
  dztfi_drdw-ebene = ibseg-fdlev.            " ????
  dztfi_drdw-dispw = ibseg-pswsl.            " ????

  IF ibseg-fdtag IS INITIAL.           " ???/???? ??
    dztfi_drdw-datum = ibseg-augdt.          " ????? = ???
  ELSE.
    dztfi_drdw-datum = ibseg-fdtag.          " ????? = ?????
  ENDIF.
  dztfi_drdw-avdat = ibseg-augdt.            " ??? = ???
  dztfi_drdw-dmbtr = ibseg-dmbtr.            " ??????
  dztfi_drdw-wrshb = ibseg-fdwbt.            " ????

  APPEND dztfi_drdw.

ENDFORM.                               " MODIFY_Iztfi_drdw
*&---------------------------------------------------------------------*
*& Form  SELECT_BSEG
*&---------------------------------------------------------------------*
*& 2.??????? ???? ????? ????? ?? ????? ??
*&   ??? ??????? ???? ??.
*& 3.????? G/L??? ??? G/L?????? ???? ??
*& 4.????, ?????? ????? 'A', 'R'? ??
*&---------------------------------------------------------------------*
FORM fill_other_info.
  IF ibkpf-waers <> ibseg-pswsl.       " Update currency
    MOVE ibkpf-waers TO ibseg-pswsl.
  ENDIF.
* 3-------------------------------------------------------------------*
*   ?? : G/L(???? ???)      -> G/L??? ??(local ??)
*          G/L(???? = open item) -> ????
*          AP/AR                     -> ????
* 3-------------------------------------------------------------------*
  IF ibseg-koart = 'S' AND ibseg-xopvw = ''.       " Account type
    SELECT SINGLE waers   FROM  skb1  INTO  ibseg-pswsl
     WHERE bukrs   =  ibseg-bukrs  AND  saknr   =  ibseg-hkont .
  ENDIF.
  IF ( NOT ibseg-augdt IS INITIAL ) OR ( ibseg-augbl <> space ).
    ibseg-bstat = 'A'.
  ENDIF.
  IF ibkpf-stblg <> space OR ibseg-stblg <> space.
    ibseg-bstat = 'R'.
    ibseg-stblg = ibkpf-stblg.
  ENDIF.

  APPEND ibseg.
  CLEAR : ibseg.

*  IF  P_DISP = 'X'.
*    MOVE-CORRESPONDING IBSEG TO IDISP.
*    APPEND IDISP.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSTAT
*&---------------------------------------------------------------------*
*FORM UPDATE_BSTAT.
*
**  CHECK p_status = 'X'.                     " ???? status ??
*  SELECT * FROM ztfi_drdw WHERE BUKRS  = P_BUKRS
*                       AND BSTAT  = SPACE
*                       AND XOPVW  = 'X' .   "??????
*    SELECT SINGLE  AUGBL FROM BSEG  INTO BSEG-AUGBL
*                                   WHERE BUKRS = ztfi_drdw-BUKRS
*                                     AND BELNR = ztfi_drdw-BELNR
*                                     AND GJAHR = ztfi_drdw-GJAHR
*                                     AND BUZEI = ztfi_drdw-BUZEI.
*    CHECK BSEG-AUGBL <> SPACE.
*    PERFORM MODIFY_ztfi_drdw.
*    COMMIT WORK.
*
*    CHECK P_DISP = 'X'.
*    READ TABLE IDISP WITH KEY BUKRS = ztfi_drdw-BUKRS
*                              BELNR = ztfi_drdw-BELNR
*                              GJAHR = ztfi_drdw-GJAHR
*                              BUZEI = ztfi_drdw-BUZEI.
*    CHECK SY-SUBRC = 0.
*    MOVE   'V'   TO  IDISP-CAUGB.
*    MODIFY IDISP TRANSPORTING CAUGB WHERE   BUKRS = IDISP-BUKRS
*                                      AND   BELNR = IDISP-BELNR
*                                      AND   GJAHR = IDISP-GJAHR
*                                      AND   BUZEI = IDISP-BUZEI.
*
*
*  ENDSELECT.
*
*ENDFORM.                               " UPDATE_BSTAT
*&---------------------------------------------------------------------*
*&      Form  SELECT_FDGRP_FDLEV
*&---------------------------------------------------------------------*
*& ?? ??? ??
*&   ???? ????? ???? ???? ?????? ???? ??
*&---------------------------------------------------------------------*
FORM select_fdgrp_fdlev.

  IF  ibseg-koart  = 'D'.
    SELECT SINGLE fdgrv FROM knb1 INTO ibseg-fdgrp  "?????
                                  WHERE kunnr  = ibseg-kunnr
                                    AND bukrs  = ibseg-bukrs.
  ELSE.
    SELECT SINGLE fdgrv FROM lfb1  INTO ibseg-fdgrp  "??????
                                  WHERE lifnr  = ibseg-lifnr
                                    AND bukrs  = ibseg-bukrs.
  ENDIF.

  CHECK ibseg-fdlev = space.
* ??GL ??.
  IF ibseg-umskz <> space.
    READ TABLE i_t074 WITH KEY koart = ibseg-koart
                               skont = ibseg-hkont
                               umskz = ibseg-umskz.
    IF sy-subrc = 0.
      ibseg-fdlev = i_t074-ebene.
    ENDIF.
  ENDIF.

  IF ibseg-fdlev = space.
    READ TABLE i_t035 WITH KEY grupp = ibseg-fdgrp.
    IF sy-subrc = 0.
      ibseg-fdlev = i_t035-ebene.
    ENDIF.
  ENDIF.

ENDFORM.                               " SELECT_FDGRP_FDLEV
*&---------------------------------------------------------------------*
*&      Form  SELECT_FDLEV
*& If G/L Account, Plan Group = G/L Account, Plan Level = i_skb1-fdlev
*&---------------------------------------------------------------------*
FORM select_fdlev.

  READ TABLE i_skb1 WITH KEY bukrs = ibseg-bukrs
                             saknr = ibseg-hkont.
  CHECK sy-subrc = 0.
  ibseg-fdgrp = ibseg-hkont.
  ibseg-fdlev = i_skb1-fdlev.


*  SELECT SINGLE XGKON FDLEV   FROM SKB1
*                              INTO CORRESPONDING FIELDS OF SKB1
*                             WHERE BUKRS = IBSEG-BUKRS
*                               AND SAKNR = IBSEG-HKONT.
*  IBSEG-FDGRP = SPACE.
*  IBSEG-FDLEV = SPACE.
*  CHECK SKB1-XGKON = 'X'. "??????/??????
*  IBSEG-FDGRP = IBSEG-HKONT.
*  IBSEG-FDLEV = SKB1-FDLEV.

ENDFORM.                               " SELECT_FDLEV
*&---------------------------------------------------------------------*
*&      Form  SELECT_FROM_VBSEGS
*&---------------------------------------------------------------------*
FORM select_from_vbsegs.
  SELECT bukrs belnr gjahr buzei
         fdgrp fdlev fdtag gsber saknr koart fdwbt
           FROM  vbsegs  INTO CORRESPONDING FIELDS OF vbsegs
                        WHERE  ausbk  =  ibkpf-bukrs
                          AND  belnr  =  ibkpf-belnr
                          AND  gjahr  =  ibkpf-gjahr.
    CHECK sy-subrc = 0.
    CHECK vbsegs-fdlev  <>  ' '.      " Planning level
    MOVE-CORRESPONDING vbsegs TO ibseg.
    ibseg-bstat = ibkpf-bstat.
    ibseg-hkont = vbsegs-saknr.
    ibseg-koart = 'S'.
    PERFORM fill_other_info.
  ENDSELECT.

ENDFORM.                               " SELECT_FROM_VBSEGS
*&---------------------------------------------------------------------*
*&      Form  SELECT_FROM_VBSEGD
*&---------------------------------------------------------------------*
FORM select_from_vbsegd.

  SELECT bukrs belnr gjahr buzei
         fdgrp fdlev fdtag zfbdt gsber hkont  kunnr fdwbt
           FROM  vbsegd   INTO CORRESPONDING FIELDS OF vbsegd
                         WHERE  ausbk  =  ibkpf-bukrs
                           AND  belnr  =  ibkpf-belnr
                           AND  gjahr  =  ibkpf-gjahr.
    CHECK sy-subrc = 0.
    CHECK vbsegd-fdlev  <>  '  '.      " Planning level
    MOVE-CORRESPONDING vbsegd TO ibseg.
    ibseg-bstat = ibkpf-bstat.
    ibseg-koart = 'D'.
    PERFORM fill_other_info.
  ENDSELECT.

ENDFORM.                               " SELECT_FROM_VBSEGD
*&---------------------------------------------------------------------*
*&      Form  SELECT_FROM_VBSEGK
*&---------------------------------------------------------------------*
FORM select_from_vbsegk.

  SELECT bukrs belnr gjahr buzei
         fdgrp fdlev fdtag zfbdt gsber hkont lifnr fdwbt
           FROM  vbsegk   INTO CORRESPONDING FIELDS OF vbsegk
                         WHERE  ausbk  =  ibkpf-bukrs
                           AND  belnr  =  ibkpf-belnr
                           AND  gjahr  =  ibkpf-gjahr.
    CHECK sy-subrc = 0.
    CHECK vbsegk-fdlev  <>  ''.      " Planning level
    MOVE-CORRESPONDING vbsegk TO ibseg.
    ibseg-bstat = ibkpf-bstat.
    ibseg-koart = 'K'.
    PERFORM fill_other_info.
  ENDSELECT.

ENDFORM.                               " SELECT_FROM_VBSEGK
*&---------------------------------------------------------------------*
*&  Form  SELECT_FROM_BSEG
*&  ????? ????????? ????? ????? ??
*&  ???&????? ???? ztfi_drdw?? ????? ????? ??? ?
*&    ??, ztfi_drdw? ???? ??? ???&??????? ??? ???
*&    ?? ????(T035)?? ????? ??? ?.
*&---------------------------------------------------------------------*
FORM select_from_bseg.
  DATA: l_augdt LIKE bseg-augdt.
*????? ?????.
  SELECT bukrs belnr gjahr buzei
         fdgrp fdlev fdtag zfbdt gsber pswsl augdt koart
         hkont lifnr kunnr umskz fdwbt dmbtr xopvw augbl
    FROM bseg   INTO CORRESPONDING FIELDS OF TABLE w_bseg
                       WHERE  bukrs    =   ibkpf-bukrs
                         AND  belnr    =   ibkpf-belnr
                         AND  gjahr    =   ibkpf-gjahr
                         AND  augdt    =   l_augdt.
  LOOP AT w_bseg.
*   IF IBKPF-STBLG <> SPACE AND IBSEG-AUGBL <> SPACE. "??? ?? ??

    MOVE w_bseg      TO ibseg.
    MOVE ibkpf-stblg TO ibseg-stblg.
    MOVE ibkpf-bstat TO ibseg-bstat.
    MOVE ibkpf-waers TO ibseg-pswsl.

    IF ibseg-koart CA 'KD'.         " Vendor or Customer
      PERFORM select_fdgrp_fdlev.
    ELSE.
      PERFORM select_fdlev.          " G/L
    ENDIF.

    CHECK ibseg-fdlev    <>  space.
    PERFORM fill_other_info.
  ENDLOOP.

ENDFORM.                               " SELECT_FROM_BSEG
*&---------------------------------------------------------------------*
*&      Form  MODIFY_ztfi_drdw
*&---------------------------------------------------------------------*
*FORM MODIFY_ztfi_drdw.
*  CHECK D_CNT > 0.
*  DELETE ztfi_drdw FROM TABLE Dztfi_drdw.
*  IF SY-SUBRC <> 0.
*    DELETE FROM  ztfi_drcm WHERE  BUKRS = P_BUKRS
*                        AND  TSTLO = TSTLO.
*    MESSAGE E437(DS) WITH TEXT-007.
*  ELSE.
*    COMMIT WORK.
*  ENDIF.
*
*  CHECK P_DISP = 'X'.
*  LOOP AT Dztfi_drdw.
*    READ TABLE IDISP WITH KEY BUKRS = Dztfi_drdw-BUKRS
*                              BELNR = Dztfi_drdw-BELNR
*                              GJAHR = Dztfi_drdw-GJAHR
*                              BUZEI = Dztfi_drdw-BUZEI.
*    CHECK SY-SUBRC = 0.
*    MOVE 'V'   TO    IDISP-CDELE.
*    MODIFY IDISP TRANSPORTING CDELE WHERE   BUKRS = IDISP-BUKRS
*                                      AND   BELNR = IDISP-BELNR
*                                      AND   GJAHR = IDISP-GJAHR
*                                      AND   BUZEI = IDISP-BUZEI.
*  ENDLOOP.
*  CLEAR Dztfi_drdw[].
*ENDFORM.                    " MODIFY_ztfi_drdw
*&---------------------------------------------------------------------*
*&      Form  INSERT_ztfi_drdw
*&---------------------------------------------------------------------*
FORM insert_ztfi_drdw.

  DESCRIBE TABLE: iztfi_drdw LINES t_cnt,
                  dztfi_drdw LINES d_cnt.

  IF t_cnt > 0.
    INSERT ztfi_drdw FROM TABLE iztfi_drdw.
    IF sy-subrc <> 0.
      DELETE FROM  ztfi_drcm WHERE  bukrs = p_bukrs
                          AND  tstlo = tstlo.
      MESSAGE e437(ds) WITH text-008.
    ENDIF.
  ENDIF.

  IF d_cnt > 0.
    UPDATE ztfi_drdw FROM TABLE dztfi_drdw.
    IF sy-subrc <> 0.
      DELETE FROM  ztfi_drcm WHERE  bukrs = p_bukrs
                          AND  tstlo = tstlo.
      MESSAGE e437(ds) WITH text-008.
    ENDIF.
  ENDIF.

*  CHECK P_DISP = 'X'.
*  LOOP AT Iztfi_drdw.
*    READ TABLE IDISP WITH KEY BUKRS = Iztfi_drdw-BUKRS
*                              BELNR = Iztfi_drdw-BELNR
*                              GJAHR = Iztfi_drdw-GJAHR
*                              BUZEI = Iztfi_drdw-BUZEI.
*    CHECK SY-SUBRC = 0.
*    MOVE 'V'   TO    IDISP-CINSE.
*    MODIFY IDISP TRANSPORTING CINSE WHERE   BUKRS = IDISP-BUKRS
*                                      AND   BELNR = IDISP-BELNR
*                                      AND   GJAHR = IDISP-GJAHR
*                                      AND   BUZEI = IDISP-BUZEI.
*  ENDLOOP.
ENDFORM.                    " INSERT_ztfi_drdw
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ztfi_drdw
*&---------------------------------------------------------------------*
FORM update_ztfi_drdw.
* ????? ?? UPDATE.
  LOOP AT iztfi_drdw WHERE bstat =   'R'.
    SELECT SINGLE * FROM ztfi_drdw
               WHERE bukrs  =  ibseg-bukrs
                 AND belnr  =  ibseg-stblg
                 AND gjahr  =  ibseg-gjahr
                 AND buzei  =  ibseg-buzei.
    CHECK sy-subrc = 0.
    UPDATE ztfi_drdw  SET bstat  =  ibseg-bstat
               WHERE bukrs  =  ibseg-bukrs
                 AND belnr  =  ibseg-stblg
                 AND gjahr  =  ibseg-gjahr
                 AND buzei  =  ibseg-buzei.
*    u_cnt = u_cnt + 1.

  ENDLOOP.
  COMMIT WORK.

ENDFORM.                    " UPDATE_ztfi_drdw
*&---------------------------------------------------------------------*
*&      Form  MODIFY_ztfi_drdw
*&---------------------------------------------------------------------*
*FORM MODIFY_ztfi_drdw.
*
*  ztfi_drdw-BSTAT = 'A'.
*  ztfi_drdw-AUGBL = BSEG-AUGBL.
*  MODIFY ztfi_drdw.
*  U_CNT = U_CNT + 1.
*
*ENDFORM.                    " MODIFY_ztfi_drdw
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ztfi_drcm
*&---------------------------------------------------------------------*
FORM update_ztfi_drcm.

  UPDATE ztfi_drcm    SET okcod = 'X'
               WHERE bukrs = p_bukrs
                 AND tstlo = tstlo.
*                and uname = syst-uname.
  IF sy-subrc <> 0.
    MESSAGE e437(ds) WITH text-010.
  ENDIF.

ENDFORM.                    " UPDATE_ztfi_drcm
*&---------------------------------------------------------------------*
*&      Form  select_grp_lev_from_ztfi_drdw
*&---------------------------------------------------------------------*
FORM select_grp_lev_from_ztfi_drdw.

  DATA : BEGIN OF iitab,
         grupp LIKE ztfi_drdw-grupp,
         ebene LIKE ztfi_drdw-ebene,
         END OF iitab.
  IF ibseg-stblg  <> space.
    SELECT SINGLE grupp ebene INTO CORRESPONDING FIELDS OF iitab
      FROM ztfi_drdw
     WHERE bukrs  =  ibseg-bukrs
       AND belnr  =  ibseg-stblg
       AND gjahr  =  ibseg-gjahr
       AND buzei  =  ibseg-buzei.
  ELSE.
    SELECT SINGLE grupp ebene INTO CORRESPONDING FIELDS OF iitab
      FROM ztfi_drdw
     WHERE bukrs  =  ibseg-bukrs
       AND belnr  =  ibseg-belnr
       AND gjahr  =  ibseg-gjahr
       AND buzei  =  ibseg-buzei.
  ENDIF.
  ibseg-fdgrp = iitab-grupp.
  ibseg-fdlev = iitab-ebene.

ENDFORM.                    " select_fdgrp_fdlev_from_ztfi_drdw
*&---------------------------------------------------------------------*
*&      Form  update_bstat1
*&---------------------------------------------------------------------*
*FORM UPDATE_BSTAT1.
*
**  CHECK p_stat1 = 'X'.
*  CLEAR: IBKPF.
*  SELECT BUKRS BELNR GJAHR CPUDT CPUTM AEDAT WAERS BSTAT STBLG
*                     FROM BKPF
*                     INTO TABLE IBKPF1
*                    WHERE BUKRS  = P_BUKRS
*                      AND AWTYP NE 'MKPF'
*                      AND BSTAT = 'B'.
*
*  DESCRIBE TABLE IBKPF1 LINES BKPF1_CNT .
*  CHECK BKPF1_CNT > 0.
*  U1_CNT = 0.
*  DATA : BEGIN OF I2ITAB,
*         BUKRS LIKE ztfi_drdw-BUKRS,
*         BELNR LIKE ztfi_drdw-BELNR,
*         GJAHR LIKE ztfi_drdw-GJAHR,
*         BUZEI LIKE ztfi_drdw-BUZEI,
*         BSTAT LIKE ztfi_drdw-BSTAT,
*         END OF I2ITAB.
*  LOOP AT IBKPF1.
*    SELECT BUKRS BELNR GJAHR BUZEI BSTAT
*      INTO CORRESPONDING FIELDS OF I2ITAB FROM ztfi_drdw
*     WHERE BUKRS = IBKPF1-BUKRS
*       AND AUGBL = IBKPF1-BELNR
*       AND GJAHR = IBKPF1-GJAHR.
*      CHECK SY-SUBRC = 0 AND I2ITAB-BSTAT <> SPACE.
*      UPDATE ztfi_drdw SET BSTAT = SPACE
*       WHERE BUKRS = I2ITAB-BUKRS
*         AND BELNR = I2ITAB-BELNR
*         AND GJAHR = I2ITAB-GJAHR
*         AND BUZEI = I2ITAB-BUZEI.
*      U1_CNT = U1_CNT + 1.
*    ENDSELECT.
*  ENDLOOP.
*  COMMIT WORK.
*ENDFORM.                    " update_bstat1
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
*&      Form  read_setting
*&---------------------------------------------------------------------*
FORM read_setting.
  SELECT SINGLE * INTO t001 FROM t001           "..Set ?????
   WHERE bukrs EQ p_bukrs.
  IF sy-subrc <> 0. STOP. ENDIF.
* ??????/??????
  SELECT bukrs saknr fdlev
         FROM skb1
         INTO CORRESPONDING FIELDS OF TABLE i_skb1
         WHERE bukrs = p_bukrs
           AND xgkon = 'X'
           AND fdlev <> space.
*????
  SELECT *  FROM t035 INTO CORRESPONDING FIELDS OF TABLE i_t035.
  SELECT *  FROM t037s INTO CORRESPONDING FIELDS OF TABLE i_t037s
                      WHERE bukrs = p_bukrs.
  SELECT *  FROM t074 INTO CORRESPONDING FIELDS OF TABLE i_t074
            WHERE ktopl EQ t001-ktopl.          " ??G/L??


ENDFORM.                    " read_setting
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
FORM display_data.
  FORMAT COLOR 4.
  ULINE AT (118).
  WRITE : / h_gu    NO-GAP,
            h_bukrs NO-GAP, h_gu NO-GAP,
            h_belnr NO-GAP, h_gu NO-GAP,
            h_gjahr NO-GAP, h_gu NO-GAP,
            h_buzei NO-GAP, h_gu NO-GAP,
            h_augbl NO-GAP, h_gu NO-GAP,
*           H_STBLG NO-GAP, H_GU NO-GAP,
            h_fdtag NO-GAP, h_gu NO-GAP,
            h_koart NO-GAP, h_gu NO-GAP,
            h_accnt NO-GAP, h_gu NO-GAP,
            h_umskz NO-GAP, h_gu NO-GAP,
            h_bstat NO-GAP, h_gu NO-GAP,
            h_fdgrp NO-GAP, h_gu NO-GAP,
 AT (l_04)  h_fdlev NO-GAP, h_gu NO-GAP,
 AT (l_amt) 'Doc.Amt' CENTERED NO-GAP, h_gu NO-GAP,
 AT (l_amt) 'Loc.Amt' CENTERED NO-GAP, h_gu NO-GAP.


*            H_CDELE NO-GAP, H_GU NO-GAP,
*            H_CINSE NO-GAP, H_GU NO-GAP.
  FORMAT COLOR OFF.
  ULINE AT (118).
  t_count = 0.
  cnt_dele = 0.
  cnt_inse = 0.
  FORMAT COLOR 2.
  LOOP AT idisp.
    DATA : mode LIKE sy-tabix.
    mode = sy-tabix MOD 2.
    PERFORM set_mode USING mode.
    WRITE :/ h_gu NO-GAP,
             idisp-bukrs UNDER h_bukrs NO-GAP, h_gu NO-GAP,
             idisp-belnr UNDER h_belnr NO-GAP, h_gu NO-GAP,
             idisp-gjahr UNDER h_gjahr NO-GAP, h_gu NO-GAP,
             idisp-buzei UNDER h_buzei NO-GAP, h_gu NO-GAP,
             idisp-augbl UNDER h_augbl NO-GAP, h_gu NO-GAP,
*            IDISP-STBLG UNDER H_STBLG NO-GAP, H_GU NO-GAP,
             idisp-datum UNDER h_fdtag NO-GAP, h_gu NO-GAP,
             idisp-koart UNDER h_koart NO-GAP, h_gu NO-GAP.
    CASE idisp-koart.
      WHEN 'K'.
        WRITE: idisp-lifnr UNDER h_accnt NO-GAP, h_gu NO-GAP.
      WHEN 'D'.
        WRITE: idisp-kunnr UNDER h_accnt NO-GAP, h_gu NO-GAP.
      WHEN 'S'.
        WRITE: idisp-hkont UNDER h_accnt NO-GAP, h_gu NO-GAP.
    ENDCASE.
    WRITE: idisp-umskz UNDER h_umskz NO-GAP, h_gu NO-GAP.

    CASE idisp-bstat.
      WHEN 'V'.    WRITE: 'Park' UNDER h_bstat NO-GAP, h_gu NO-GAP.
      WHEN 'A'.    WRITE: 'Cler' UNDER h_bstat NO-GAP, h_gu NO-GAP.
      WHEN 'R'.    WRITE: 'Revr' UNDER h_bstat NO-GAP, h_gu NO-GAP.
      WHEN OTHERS. WRITE: '    ' UNDER h_bstat NO-GAP, h_gu NO-GAP.
    ENDCASE.


    WRITE: idisp-grupp UNDER h_fdgrp NO-GAP, h_gu NO-GAP,
    AT (l_04) idisp-ebene  NO-GAP, h_gu NO-GAP,
    AT (l_amt) idisp-wrshb  CURRENCY idisp-dispw NO-GAP, h_gu NO-GAP,
    AT (l_amt) idisp-dmbtr  CURRENCY t001-waers  NO-GAP, h_gu NO-GAP.


*             IDISP-CDELE UNDER H_CDELE NO-GAP, H_GU NO-GAP,
*             IDISP-CINSE UNDER H_CINSE NO-GAP, H_GU NO-GAP.
*    IF  IDISP-CDELE = 'V'.
*      CNT_DELE = CNT_DELE + 1.
*    ENDIF.
*    IF  IDISP-CINSE = 'V'.
*      CNT_INSE = CNT_INSE + 1.
*    ENDIF.
    HIDE idisp.

    t_count = t_count + 1.
  ENDLOOP.
  FORMAT COLOR OFF.
  ULINE AT (118).
  CLEAR: idisp.

ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  EXEC_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0985   text
*----------------------------------------------------------------------*
FORM exec_sort USING    up_dn.
  DATA:fld_name(20),
       name(10).
  GET CURSOR FIELD fld_name.
  name = fld_name+6(10).

  CHECK fld_name(5) = 'IDISP'.
  CASE up_dn.
    WHEN 'U'.                              "..Sort Up
      SORT idisp BY (name).
    WHEN 'D'.                              "..Sort Down
      SORT idisp BY (name) DESCENDING.
  ENDCASE.

  sy-lsind = sy-lsind - 1.
  PERFORM display_data.
ENDFORM.                    " EXEC_SORT
