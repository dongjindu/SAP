FUNCTION zsapbf_check_aufnr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FIRST_DAY) TYPE  SYDATUM
*"     VALUE(IV_LAST_DAY) TYPE  SYDATUM
*"  TABLES
*"      IT_AUFNR STRUCTURE  QRP002 OPTIONAL
*"      ET_AUFNR STRUCTURE  AUFNR_S OPTIONAL
*"----------------------------------------------------------------------

****** Start : Added by SAPCD08 on 2010.07.08***, Commented 2010.07.19 due to bad performance
*  SELECT a~aufnr FROM qrp002 AS a
**               JOIN ppc_ord_inf AS b
**               ON a~cc_guid = b~accassobj
**               JOIN ppc_head
**               ON ppc_ord_inf~orderid = ppc_head~orderid
*               INTO TABLE et_aufnr   "lt_aufnr
*               FOR ALL ENTRIES IN it_aufnr
*              WHERE a~aufnr = it_aufnr-aufnr
***                AND ppc_ord_inf~dummy_order = space
**                AND ppc_head~flg_del EQ space
**                AND ppc_head~flg_synch = 'X'
**                AND ppc_head~postdate GE lv_first_day
**                AND ppc_head~postdate LE lv_last_day
*                AND EXISTS ( SELECT c~accassobj FROM ppc_head AS c
*                              WHERE c~accassobj   = a~cc_guid
**                                AND c~flg_del     = space
*                                AND c~flg_asynch  = 'X' "flg_asynch : no problem!!
*                                AND c~postdate GE iv_first_day
*                                AND c~postdate LE iv_last_day )
*    %_HINTS ORACLE '&max_blocking_factor  10& &max_in_blocking_factor  10&'.
****** End : Added by SAPCD08 on 2010.07.08***, Commented 2010.07.19 due to bad performance

  DATA lt_aufnr LIKE qrp002 OCCURS 0 WITH HEADER LINE.

***** Start : Added by SAPCD08 on 2010.07.19***, The better performance has been approved
  SORT  it_aufnr BY cc_guid.

  SELECT DISTINCT a~accassobj AS cc_guid
    FROM ppc_head AS a
    INTO CORRESPONDING FIELDS OF TABLE lt_aufnr
     FOR ALL ENTRIES IN it_aufnr
   WHERE a~accassobj = it_aufnr-cc_guid
*     AND a~flg_asynch = 'X' "Commentated by Sung-Kon James Kim 2011/01/26
     AND a~postdate BETWEEN iv_first_day AND iv_last_day
    %_HINTS ORACLE '&max_blocking_factor  20& &max_in_blocking_factor  20&'
            ORACLE 'INDEX_FFS ("PPC_HEAD", "PPC_HEAD~BUD")'.

  LOOP AT lt_aufnr.
    READ TABLE it_aufnr WITH KEY cc_guid = lt_aufnr-cc_guid.

    IF sy-subrc = 0.
      MOVE it_aufnr-aufnr TO et_aufnr-aufnr.
      APPEND et_aufnr.
      CLEAR it_aufnr.
    ENDIF.

  ENDLOOP.
***** End : Added by SAPCD08 on 2010.07.19***, The better performance has been approved


ENDFUNCTION.
