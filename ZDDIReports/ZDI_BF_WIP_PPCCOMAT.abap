*&---------------------------------------------------------------------*
*& Report  ZDI_BF_WIP_PPCCOMAT
*&
*&---------------------------------------------------------------------*
*& spec by Andy Choi
*&
*&---------------------------------------------------------------------*

REPORT  zdi_bf_wip_ppccomat.
TABLES: ppc_comat, cpzp.
TYPES: BEGIN OF ty_cpzp,
         f_objnr  TYPE f_objnr,
         gjper    TYPE co_gjper,
       END OF ty_cpzp.

DATA: it_cpzp TYPE SORTED TABLE OF ty_cpzp
               WITH UNIQUE KEY f_objnr gjper.
DATA: lv_cpzp TYPE ty_cpzp.

DATA: it_comat TYPE SORTED TABLE OF ppc_comat
               WITH NON-UNIQUE KEY matnr werks bwtar .
DATA: lv_comat LIKE ppc_comat.

DATA: it_ckmlhd TYPE SORTED TABLE OF ckmlhd
               WITH NON-UNIQUE KEY matnr bwkey bwtar .
DATA: lv_ckmlhd LIKE ckmlhd.

* by ig.moon 3/25/2013 {
DATA ippc_comat LIKE ppc_comat OCCURS 0 WITH HEADER LINE.
DATA deleted_ippc_comat LIKE ppc_comat OCCURS 0 WITH HEADER LINE.

DATA icpzp LIKE cpzp OCCURS 0 WITH HEADER LINE.
DATA deleted_icpzp LIKE cpzp OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF ppc_matnr OCCURS 0,
         matnr LIKE lv_ckmlhd-matnr,
         werks LIKE lv_ckmlhd-bwkey,
       END   OF  ppc_matnr.

DATA : BEGIN OF del_matnr OCCURS 0,
         matnr LIKE lv_ckmlhd-matnr,
         werks LIKE lv_ckmlhd-bwkey,
       END   OF  del_matnr.

* }

*&---------------------------------------------------------------------*
SELECT-OPTIONS: s_gjper FOR cpzp-gjper,
                s_fobj  FOR cpzp-f_objnr,
                s_matnr FOR ppc_comat-matnr.

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*
  SELECT DISTINCT f_objnr gjper INTO TABLE it_cpzp FROM cpzp
     WHERE f_objnr IN s_fobj
        AND ( f_objnr LIKE 'MK%' AND f_objnr IN s_fobj )
        AND gjper IN s_gjper.

  SELECT * INTO TABLE it_comat
      FROM ppc_comat
      WHERE matnr IN s_matnr
        AND sobkz = space
        AND kdauf EQ space.

  SELECT * INTO TABLE it_ckmlhd FROM ckmlhd
    FOR ALL ENTRIES IN it_comat
    WHERE ckmlhd~matnr = it_comat-matnr
      AND ckmlhd~bwkey = it_comat-werks
      AND ckmlhd~bwtar = it_comat-bwtar.

*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
  WRITE:/ 'PPC_COMAT duplicate records check...'.
  DATA: l_cnt TYPE i.

  CLEAR : ippc_comat,ippc_comat[],ppc_matnr,ppc_matnr[].

  LOOP AT it_ckmlhd INTO lv_ckmlhd.
    CLEAR l_cnt.
    LOOP AT it_comat INTO lv_comat
     WHERE matnr = lv_ckmlhd-matnr
       AND werks = lv_ckmlhd-bwkey.
*      and bwtar = lv_ckmlhd-bwtar.
      l_cnt = l_cnt + 1.
      IF l_cnt > 1.
        WRITE:/ lv_comat-matnr.

*-----   delete duplicate from ppc_comat
        ppc_matnr-matnr = lv_comat-matnr.
        ppc_matnr-werks = lv_comat-werks.
        APPEND ppc_matnr.

*-----   merge cpzp and create single record

        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

* by ig.moon 3/25/2013 {
  SORT ppc_matnr BY matnr werks.
  LOOP AT it_ckmlhd INTO lv_ckmlhd.
    READ TABLE ppc_matnr WITH KEY matnr = lv_ckmlhd-matnr
                                  werks = lv_ckmlhd-bwkey
                                  BINARY SEARCH.
    IF sy-subrc EQ 0.
      SELECT * FROM ppc_comat
                WHERE matnr EQ ppc_matnr-matnr.
        IF sy-subrc EQ 0.
          ippc_comat = ppc_comat.
          APPEND ippc_comat.
        ENDIF.
      ENDSELECT.
    ENDIF.
  ENDLOOP.
  SORT ippc_comat BY werks matnr bwtar objid.
  deleted_ippc_comat[] = ippc_comat[].

  DELETE ADJACENT DUPLICATES FROM ippc_comat COMPARING werks matnr.

  SORT ippc_comat BY werks matnr bwtar objid.

  DATA $ix TYPE i.

* deleted_ippc_comat has all entries so far.
  LOOP AT deleted_ippc_comat.
    $ix = sy-tabix.
    READ TABLE ippc_comat WITH KEY  werks = deleted_ippc_comat-werks
                                    matnr = deleted_ippc_comat-matnr
                                    bwtar = deleted_ippc_comat-bwtar
                                    objid = deleted_ippc_comat-objid
                          BINARY SEARCH.
    IF sy-subrc EQ 0.
      DELETE deleted_ippc_comat INDEX $ix.
    ENDIF.
  ENDLOOP.
* now deleted_ippc_comat has deleted entry only.

  CLEAR : icpzp,icpzp[],deleted_icpzp,deleted_icpzp[], del_matnr, del_matnr[].

  DATA $f_objnr(22).

  WRITE:/ 'To be merged'.

  LOOP AT deleted_ippc_comat.

    CONCATENATE 'MK00' deleted_ippc_comat-objid INTO $f_objnr.
    SELECT COUNT( * ) FROM cpzp
              WHERE f_objnr EQ $f_objnr
                AND gjper IN s_gjper.
    IF sy-dbcnt > 0.

      WRITE:/ $f_objnr.

    ENDIF.
  ENDLOOP.

* }



  WRITE:/ 'PPC_COMAT missing records check...'.
  DATA: l_objnr TYPE ppc_co_objid.
  LOOP AT it_cpzp INTO lv_cpzp.
    l_objnr = lv_cpzp-f_objnr+4(10).
    READ TABLE it_comat INTO lv_comat
         WITH KEY objid = l_objnr.

    IF sy-subrc <> 0.
      WRITE:/ lv_cpzp-f_objnr, lv_cpzp-gjper.
    ENDIF.
  ENDLOOP.

* by ig.moon 3/25/2013 {


*  IF p_upd EQ 'X'.
*    LOOP AT ippc_comat.
*      DELETE FROM ppc_comat WHERE matnr EQ ppc_matnr-matnr.
*    ENDLOOP.
*    COMMIT WORK.
*    INSERT ppc_comat FROM TABLE ippc_comat.
*    COMMIT WORK.
*    WRITE:/ 'PPC_COMAT has been updated.'.
*    u_break.
*  ENDIF.

* }
