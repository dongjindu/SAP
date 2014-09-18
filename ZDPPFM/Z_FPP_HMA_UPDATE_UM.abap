FUNCTION z_fpp_hma_update_um.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DOCNUM) LIKE  EDIDC-DOCNUM OPTIONAL
*"  TABLES
*"      RETURN STRUCTURE  BAPIRETURN OPTIONAL
*"      ITEM STRUCTURE  ZPOSEG2 OPTIONAL
*"----------------------------------------------------------------------

**REF KMMG ZSDR03500T
  DATA : lt_data LIKE TABLE OF zposeg2 WITH HEADER LINE,
         lt_data_wo  LIKE TABLE OF zposeg2 WITH HEADER LINE.
  DATA : lt_um   LIKE TABLE OF ztsd_um WITH HEADER LINE,
         ll_um   LIKE LINE  OF lt_um ,
         lt_vm   LIKE TABLE OF ztpp_vm WITH HEADER LINE.
  DATA : l_return LIKE return.

** On 02/14/13
*  DATA: l_date LIKE ztsd_um-aedat,
*         l_time LIKE ztsd_um-aezet.
*  DATA: lt_vmaster LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
*        lt_vmas_r LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
*        l_objek(18),
*        l_flag(1),
*        l_date_vm like sy-datum.
*
*  l_date = sy-datum.
*  l_time = sy-uzeit.
** End on 02/14/13

  CLEAR : gt_model[], gt_model, lt_um[], lt_um.

  lt_data[]    = item[].
  lt_data_wo[] = item[].

*-< 02.06.2014 Victor
  SORT lt_data_wo BY prdod  natn  dist  wkexc wkinc.
  DELETE ADJACENT DUPLICATES FROM lt_data_wo
                          COMPARING prdod  natn  dist  wkexc wkinc.
*->

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_model
    FROM ztpp_model_conv.

*** Exitst I/F Data : Function
  IF NOT docnum IS INITIAL.

    DATA : lv_type LIKE ztpp_po_idoc-type.
    SELECT SINGLE type INTO lv_type
      FROM ztpp_po_idoc
     WHERE docnum = docnum.

    IF NOT lt_data[] IS INITIAL.
*# 1. Exist check.
      IF lv_type EQ ''.

        LOOP AT lt_data_wo.  "02.06.2014
          SELECT * APPENDING CORRESPONDING FIELDS OF TABLE lt_um
          FROM ztsd_um
          WHERE wo_serial = lt_data_wo-prdod
            AND wo_nation = lt_data_wo-natn
            AND wo_dealer = lt_data_wo-dist
            AND wo_extc   = lt_data_wo-wkexc   "Victor
            AND wo_intc   = lt_data_wo-wkinc   "Victor
            AND ( status    = '' OR status = 'F' ). "Victor 06.08.201
        ENDLOOP.

*        SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_um
*       FROM ztsd_um
*       FOR ALL ENTRIES IN lt_data
*       WHERE wo_serial = lt_data-prdod
*         AND wo_nation = lt_data-natn
*         AND wo_dealer = lt_data-dist
*         AND wo_extc   = lt_data-wkexc   "Victor
*         AND wo_intc   = lt_data-wkinc   "Victor
*         AND ( status    = '' OR status = 'F' ). "Victor 06.08.201
**              AND STATUS    = ''.

        IF lt_um[] is not INITIAL.   " already exist work order
*        IF sy-subrc = 0 . " already exist work order
          PERFORM set_del_zvin TABLES lt_data lt_um.
        ENDIF.

      ENDIF.

      PERFORM set_add_zvin TABLES lt_data lt_um.

    ENDIF.
  ENDIF.
****

*# * Status S, P Check
  PERFORM check_spstatus_um .

** Changed by Park On 11/14/13
  PERFORM update_ztsd_um.
** End of change 11/14/13

*# . Get Update UM
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_um
      FROM ztsd_um
      WHERE status    = ''
** Furong on 05/21/12 tuning
%_HINTS ORACLE 'RULE'.    "Addition
** End

  gt_um[] = lt_um[].


*# . Dealer Allocation
  DATA : lt_dist LIKE TABLE OF ztpp_so_idoc WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_dist
    FROM ztpp_so_idoc
    FOR ALL ENTRIES IN lt_um
    WHERE nation = lt_um-wo_nation
      AND vin    = lt_um-zvin.

  SORT lt_dist BY nation zvin .


  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_vm
    FROM ztpp_vm
    FOR ALL ENTRIES IN lt_um
    WHERE model_code = lt_um-model_code
      AND body_no    = lt_um-body_no.

  SORT lt_vm BY model_code body_no .

**#2.1.  IF UM Have Body_no, UM Status Check
  LOOP AT lt_um INTO ll_um
          WHERE NOT body_no IS INITIAL
            AND     status IS INITIAL  .

    READ TABLE lt_vm WITH KEY model_code = ll_um-model_code
                              body_no    = ll_um-body_no
                              BINARY SEARCH.
    IF sy-subrc EQ 0 .
*  check spec change
      PERFORM set_spec_vm     USING ll_um lt_vm.
      CASE lt_vm-usg_car .
        WHEN 'S'.
*  check scrap
          PERFORM set_scrap_vm     USING ll_um.
        WHEN 'D'.
*  check disposal
          PERFORM set_disposal_vm  USING ll_um.
      ENDCASE.
*  check finish
      IF lt_vm-rp_cstatus = '25' OR
         lt_vm-rp_cstatus = '27'.
        PERFORM set_finish_vm    USING ll_um.
      ENDIF.

      IF NOT ll_um-status IS INITIAL.
        MOVE-CORRESPONDING ll_um TO gt_um .

*-      Victor added ship_out field on 09.02.2011
** Furong on 05/18/12 SAP Tuning
*        MODIFY gt_um TRANSPORTING status  ship_out aenam aedat aezet
*                             WHERE body_no EQ ll_um-body_no
*                               AND zvin    EQ ll_um-zvin.
        MODIFY gt_um TRANSPORTING status  ship_out aenam aedat aezet
                            WHERE zvin       EQ ll_um-zvin
                              AND model_code EQ ll_um-model_code
                              AND body_no    EQ ll_um-body_no.

      ENDIF.
** End on 05/18/12
    ENDIF.

  ENDLOOP.


**#3.1 Set Dealer Alloc
  SORT : lt_dist BY vin zvin,
         lt_um   BY zvin wo_nation.

  LOOP AT lt_dist .
** Furong on 09/28/12
*    READ TABLE lt_um INTO ll_um WITH KEY zvin = lt_dist-zvin
*                                                BINARY SEARCH.
    READ TABLE lt_um INTO ll_um WITH KEY zvin      = lt_dist-zvin
                                         wo_nation = lt_dist-nation
                                         BINARY SEARCH.
** End
    IF sy-subrc <> 0 .
      CLEAR lt_dist.
    ELSE .
      MOVE-CORRESPONDING ll_um TO gt_um .
      gt_um-wo_dealer1 = lt_dist-lcldr.
      gt_um-urgency    = lt_dist-zpdurg.
      gt_um-urgcdate   = lt_dist-zurgdt.
      gt_um-dealer_dt  = lt_dist-zalcdt.
      gt_um-zdesc      = lt_dist-zdesc.
      MODIFY gt_um
             TRANSPORTING  wo_dealer1 urgency  dealer_dt urgcdate zdesc
             WHERE zvin      = lt_dist-zvin
** Furong on 09/28/12
               AND wo_nation = lt_dist-natn.
** End
    ENDIF.
  ENDLOOP.

** Furong on 05/18/12 SAP Tuning
*  SORT gt_um
*           BY wo_serial intno wo_nation wo_dealer wo_extc wo_intc zvin
* on 05/18/12 SAP Tuning

  MODIFY ztsd_um FROM TABLE gt_um.

  IF sy-subrc EQ 0 .

    l_return-type = 'S'.

    SORT lt_data BY prdod natn dist wkexc wkinc.

    LOOP AT  lt_data.
      AT NEW wkinc.
        UPDATE ztpp_po_idoc SET
          zvin_stt = 'S'
          erdat    = sy-datum
          ertim    = sy-uzeit
          uname    = sy-uname
        WHERE docnum = docnum
          AND status = 'S'
          AND wo_ser = lt_data-prdod
          AND natn   = lt_data-natn
          AND dist   = lt_data-dist
          AND wkexc  = lt_data-wkexc
          AND wkinc  = lt_data-wkinc.
      ENDAT.
    ENDLOOP.
  ELSE.
    l_return-type = 'E'.
  ENDIF.

  APPEND l_return TO return .

ENDFUNCTION.
