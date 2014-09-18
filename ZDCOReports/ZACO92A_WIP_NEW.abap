*&--------------------------------------------------------------------
*& REPORT                 : ZACO92A_WIP
*& Author                 : WSKIM
*& Creation Date          : 01/26/2005
*& Specification By       : Andy Choi
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description            : WIP
*& Modification Log
*& Date     Developer      Request ID      Description
*& 07/13/01 Valerian       UD1K952398      Fix calculation var which is
*&                                         not refreshed properly
* 14/06/2013  T00303       UD1K957377   U1: Apply archiving
*&--------------------------------------------------------------------

*INCLUDE zaco92a_wip_top.
*----------------------------------------------------------------------*
*   INCLUDE ZACO92A_WIP_TOP                                            *
*----------------------------------------------------------------------*
REPORT zaco92a_wip MESSAGE-ID  zmco.

*Table definition
TABLES : ppc_head,ppc_ord_inf,ppc_rp,ppc_rp_vers,ztco_wip,
         afru,afpo,crhd,plpo,mara.

*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.
*---- ALV

*TYPE-POOLS: slis, vrm.
*
*INCLUDE <icon>.
*INCLUDE <symbol>.
*CLASS cl_gui_resources DEFINITION LOAD.
*
*DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
*      gs_layout   TYPE slis_layout_alv,
*      gt_sp_group TYPE slis_t_sp_group_alv,
*      gt_events   TYPE slis_t_event,
*      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
*      gs_prnt     TYPE slis_print_alv.
*DATA: wa_repid LIKE sy-repid,
*      wa_var_save(1) TYPE c             VALUE  'A',
*      wa_default(1)  TYPE c,
*      wa_exit(1) TYPE c,
*      wa_variant LIKE disvariant,
*      wa_var LIKE disvariant,
*      wa_alv_function_name(30) TYPE c VALUE 'REUSE_ALV_GRID_LIST',
*      wa_alv_get_info_name(40) TYPE c.
**--- ALV
*DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
*       w_eventcat TYPE slis_t_event WITH HEADER LINE,
*       w_selfield TYPE slis_selfield,
*       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
*       w_col_pos  TYPE i,
*       w_program  LIKE sy-repid,
*       w_top_of_page TYPE slis_t_listheader,
*       w_line1 TYPE slis_listheader.

*Internal Table definition
DATA : BEGIN OF it_ppc_head OCCURS 0,
        plant         LIKE ppc_ord_inf-plant,
        materialnr    LIKE ppc_ord_inf-materialnr,
        version       LIKE ppc_ord_inf-version,
        reppoint_ext  LIKE ppc_rp-reppoint_ext,
        confquant     LIKE ppc_head-confquant,
        flg_scrap     LIKE ppc_head-flg_scrap,
        flg_reversal  LIKE ppc_head-flg_reversal,
        confunit      LIKE ppc_head-confunit,
        reppoint      LIKE ppc_head-reppoint,
       END OF it_ppc_head.
DATA : it_wip LIKE ztco_wip OCCURS 0 WITH HEADER LINE,
       it_wip_temp LIKE it_wip OCCURS 0 WITH HEADER LINE,
       it_wip_temp2 LIKE it_wip OCCURS 0 WITH HEADER LINE.

*- U1 Start
DATA: gt_ppc_ord_inf TYPE TABLE OF ppc_ord_inf WITH HEADER LINE.
*- U1 End

DATA : BEGIN OF it_wip_adj OCCURS 0,
         plant   LIKE ztco_wip-plant,
         matnr   LIKE ztco_wip-matnr,
         version LIKE ztco_wip-version,
         adjqty LIKE ztco_wip-adjqty,
       END OF it_wip_adj.

DATA : BEGIN OF it_product OCCURS 0,
        plant         LIKE ppc_ord_inf-plant,
        matnr         LIKE ppc_ord_inf-materialnr,
        verid         LIKE ppc_ord_inf-version,
        categ(1)      TYPE c,  "(F)FSC, (M)MIP
       END OF it_product.

DATA : it_ztco_wip LIKE ztco_wip OCCURS 0 WITH HEADER LINE,
       it_fsc_wip LIKE it_ztco_wip OCCURS 0 WITH HEADER LINE,
       it_ztco_fsc LIKE it_ztco_wip OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_afru OCCURS 0,
        werks LIKE afru-werks,
        matnr LIKE afpo-matnr,
        lmnga LIKE afru-lmnga, " YIELD
        xmnga LIKE afru-xmnga, " SCRAP
        meinh LIKE afru-meinh,
        stokz LIKE afru-stokz,
        arbpl LIKE crhd-arbpl, " WORK CENTER
       END OF it_afru.

DATA: BEGIN OF gt_mi_routing OCCURS 0,
        matnr TYPE mara-matnr,
        dispo TYPE marc-dispo,
        plnnr TYPE plko-plnnr,
        plnal TYPE plko-plnal,
        vornr TYPE plpo-vornr,
        arbpl TYPE crhd-arbpl,
        sortb TYPE crhd-sortb,
        datuv TYPE plpo-datuv,
        arbid TYPE plpo-arbid,
      END OF gt_mi_routing.

DATA: BEGIN OF it_mip_routing OCCURS 0,
        matnr    TYPE mapl-matnr,
        plnnr TYPE plko-plnnr,
        plnal TYPE plko-plnal,
        zaehl TYPE plko-zaehl,
        verwe TYPE plko-verwe,
        vornr TYPE plpo-vornr,
        arbpl TYPE crhd-arbpl,  "WC
      END OF it_mip_routing.

*Get endwipqty of Previous month
DATA : BEGIN OF it_prev_wips OCCURS 0,
        gubun   LIKE ztco_wip-gubun,
        plant   LIKE ztco_wip-plant,
        matnr   LIKE ztco_wip-matnr,
        version LIKE ztco_wip-version,
        rppoint LIKE ztco_wip-rppoint,
        ewqty   LIKE ztco_wip-ewqty,
        adjqty LIKE ztco_wip-adjqty,
        cfqty   LIKE ztco_wip-cfqty,
        confunit LIKE ztco_wip-confunit,                    "UD1K952398
       END OF it_prev_wips.


DATA : BEGIN OF it_rp_ver OCCURS 0,
        rp_ext  LIKE ppc_rp-reppoint_ext,
        rp      LIKE ppc_rp-reppoint,
        rp_pred LIKE ppc_rp_vers-reppoint_pred,
      END OF it_rp_ver.

DATA : BEGIN OF it_out OCCURS 0,
        gubun    LIKE ztco_wip-gubun,
        rppoint  LIKE ztco_wip-rppoint,
        shop     LIKE ztco_wip-shop,
        workct   LIKE ztco_wip-workct,
        matnr    LIKE ztco_wip-matnr,
        version  LIKE ztco_wip-version,

*        chkbox       TYPE c,
*        light        TYPE c,
*        tabcolor     TYPE slis_t_specialcol_alv,

        cfqty    LIKE ztco_wip-scrapqty,
        bwqty    LIKE ztco_wip-bwqty,
        inqty    LIKE ztco_wip-inqty,
        outqty   LIKE ztco_wip-outqty,
        ewqty    LIKE ztco_wip-ewqty,
        scrapqty LIKE ztco_wip-scrapqty,
        adjqty  LIKE ztco_wip-adjqty,
       END OF it_out.

*Data definiton
DATA : w_int TYPE i,
       p_check,
       c_yymm(7),
       f_save ,
       p_werks LIKE plpo-werks VALUE 'P001',
       p_prev LIKE sy-datum(6).
DATA : g_startdt TYPE datum,
       g_last_date TYPE datum.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS : "p_werks LIKE plpo-werks DEFAULT 'P001',
             p_yymm  LIKE sy-datum(6).

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECTION-SCREEN : BEGIN OF LINE.
SELECTION-SCREEN :   COMMENT 2(18) text-r04.
PARAMETERS : r_db  RADIOBUTTON GROUP rb DEFAULT 'X'.
SELECTION-SCREEN :   COMMENT 34(13) text-r05.
PARAMETERS : r_bct RADIOBUTTON GROUP rb.
SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN : BEGIN OF LINE.
SELECTION-SCREEN :   COMMENT 2(18) text-r01.
PARAMETERS : r_rp RADIOBUTTON GROUP rdg .
SELECTION-SCREEN :   COMMENT 28(5) text-r02.
PARAMETERS : r_shop  RADIOBUTTON GROUP rdg DEFAULT 'X'.
SELECTION-SCREEN :   COMMENT 40(11) text-r03.
PARAMETERS : r_wct  RADIOBUTTON GROUP rdg.
SELECTION-SCREEN :   COMMENT 58(5) text-r06.
PARAMETERS : r_fsc  RADIOBUTTON GROUP rdg.

SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECT-OPTIONS: s_matnr FOR ppc_ord_inf-materialnr,
                s_verid FOR ppc_ord_inf-version,
                s_arbpl FOR crhd-arbpl.

**- U1 Start
*INCLUDE ziarch_comm01.
**- U1 End

DATA : BEGIN OF it_fsc_rp   OCCURS 0,
        rp    LIKE plpo-usr01,  "RP
        wc    LIKE crhd-arbpl,  "WC
        shop  LIKE plpo-usr02,  "SHOP
       END OF it_fsc_rp.
DATA : g_rp_last     LIKE plpo-usr01,
       g_rp_last_xx  LIKE plpo-usr01,  "body in white
       g_rp_last_xy  LIKE plpo-usr01,  "body in paint
       g_rp_first    LIKE plpo-usr01.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  p_yymm = sy-datum(6).

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.
  PERFORM check_period.
*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF r_bct EQ 'X'.
    PERFORM read_ref_routing_rp.
*DI-Auto
    PERFORM read_ppc_head USING  p_yymm p_check.
    PERFORM collect_fsc_rppoint.

    PERFORM read_mts_mto.

*DI Auto
    PERFORM read_prev_wips.
    PERFORM calc_wip_detail.
*Press & Engin qty
    PERFORM press_engin_qty.

    PERFORM update_ztco_wip.

    PERFORM calc_f_wips.  "gubun F
    PERFORM update_ztco_wip_fsc.


  ELSEIF r_db EQ 'X'.
    PERFORM read_ztco_wip USING p_check.
    CHECK p_check = 'X'.

    PERFORM display.
  ENDIF.
*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  read_ppc_head
*&---------------------------------------------------------------------*
FORM read_ppc_head USING lp_yymm p_check.
  CLEAR : c_yymm.

  CONCATENATE lp_yymm '%' INTO c_yymm.

  SELECT b~plant
         b~materialnr
         b~version
         c~reppoint_ext
         SUM( a~confquant )
         a~flg_scrap
         a~flg_reversal
         a~confunit
         a~reppoint
      INTO TABLE it_ppc_head
      FROM   ( ( ppc_head AS a
        INNER JOIN ppc_ord_inf AS b
           ON a~orderid = b~orderid )
        INNER JOIN ppc_rp AS c
           ON a~reppoint = c~reppoint )
      WHERE postdate    LIKE c_yymm
        AND materialnr  IN   s_matnr
        AND version     IN   s_verid
      GROUP BY
         b~plant
         b~materialnr
         b~version
         c~reppoint_ext
         a~flg_scrap
         a~flg_reversal
         a~confunit
         a~reppoint.

**- U1 Start
*  IF p_arch EQ 'X'.
*    PERFORM archive_read_ppc_ord_inf.
*  ENDIF.
**- U1 End

  DESCRIBE TABLE it_ppc_head LINES w_int.
  IF w_int <> 0.
    p_check = 'X'.
*  ELSE.
*    MESSAGE i026.
*    p_check = ' '.
*    STOP.
  ENDIF.

ENDFORM.                    " read_ppc_head
*&---------------------------------------------------------------------*
*&      Form  collect_fsc_rppoint
*&---------------------------------------------------------------------*
FORM collect_fsc_rppoint.
  REFRESH it_wip.

*by fsc version rp
* SORT it_ppc_head BY materialnr version reppoint_ext.
  LOOP AT it_ppc_head.

    IF      it_ppc_head-materialnr+4(2) =  'XX'
    AND     it_ppc_head-reppoint_ext    > g_rp_last_xx.
      CONTINUE.
    ELSEIF  it_ppc_head-materialnr+4(2) =  'XY'
    AND     it_ppc_head-reppoint_ext    > g_rp_last_xy.
      CONTINUE.
    ENDIF.

    CLEAR it_wip.
    MOVE : it_ppc_head-plant        TO  it_wip-plant,
           it_ppc_head-materialnr   TO  it_wip-matnr,
           it_ppc_head-version      TO  it_wip-version,
           it_ppc_head-reppoint_ext TO  it_wip-rppoint.

    MOVE : 'W'                  TO it_wip-gubun,
           p_yymm               TO it_wip-yymm,
           it_ppc_head-confunit TO it_wip-confunit.

*FIXME
    IF it_ppc_head-flg_reversal EQ 'X'.
      it_ppc_head-confquant = - it_ppc_head-confquant.

*      IF it_ppc_head-reppoint_ext = '01'.  "STARTING
*      else.
*       it_wip-inqty = it_ppc_head-confquant.
*      endif.
    ENDIF.

    it_wip-outqty = it_ppc_head-confquant.

    IF it_ppc_head-flg_scrap EQ 'X'.
      it_wip-scrapqty = it_ppc_head-confquant.
    ENDIF.

    COLLECT it_wip.

    MOVE : it_ppc_head-plant        TO  it_product-plant,
           it_ppc_head-materialnr   TO  it_product-matnr,
           it_ppc_head-version      TO  it_product-verid,
           'F'                      TO  it_product-categ.
    COLLECT it_product. CLEAR it_product.
  ENDLOOP.
ENDFORM.                    " collect_fsc_rppoint
*&---------------------------------------------------------------------*
*&      Form  calc_wip_detail
*&---------------------------------------------------------------------*
FORM calc_wip_detail.

  CLEAR it_wip.REFRESH:it_wip_temp, it_wip_temp2.

  SORT it_wip BY plant matnr version rppoint.
*  it_wip_temp[] = it_wip[].

  LOOP AT it_wip WHERE rppoint < g_rp_last.
*IN Quantity
    PERFORM get_outquantity USING it_wip-matnr it_wip-version
                                  it_wip-rppoint.
  ENDLOOP.

*reverse previous adjustment
  CLEAR it_wip.                                             "UD1K952398
  LOOP AT it_prev_wips WHERE gubun   = 'W'.
    CHECK it_prev_wips-adjqty <> 0.
    it_wip-yymm       = p_yymm.
    it_wip-gubun      = it_prev_wips-gubun  .
    it_wip-plant      = it_prev_wips-plant  .
    it_wip-matnr      = it_prev_wips-matnr  .
    it_wip-version    = it_prev_wips-version.
    it_wip-rppoint    = it_prev_wips-rppoint.
    it_wip-confunit   = it_prev_wips-confunit.              "UD1K952398
    it_wip-cfqty      = - it_prev_wips-adjqty.
    COLLECT it_wip.
  ENDLOOP.

* Begin WIP, End WIP
  LOOP AT it_wip.
*Previous month
    PERFORM get_previous_month USING it_wip-matnr it_wip-version
                                     it_wip-rppoint.
*END WIP Quantity
    it_wip-ewqty = it_wip-bwqty + it_wip-inqty - it_wip-outqty
                 - it_wip-scrapqty + it_wip-cfqty.

*   if it_wip-outqty >  it_wip-inqty  : adjust END wip
    IF it_wip-ewqty < 0 .
      it_wip-adjqty = it_wip-ewqty * -1.
      it_wip-ewqty = 0.

      MOVE-CORRESPONDING it_wip TO it_wip_adj.
      COLLECT it_wip_adj.
    ENDIF.

*...Breakdown to W/C level
    PERFORM get_shop_workcenter USING it_wip.
  ENDLOOP.

  CLEAR w_int.
  DESCRIBE TABLE it_wip_temp2 LINES w_int.
  IF w_int <> 0.
    INSERT LINES OF it_wip_temp2 INTO TABLE it_wip.
  ENDIF.

*in case of no this month data, Filled previous month data
  PERFORM filled_previous_month USING p_prev.


ENDFORM.                    " calc_wip_detail
*&---------------------------------------------------------------------*
*&      Form  previous_month
*&---------------------------------------------------------------------*
FORM previous_month USING p_yymm
                    CHANGING p_prev.

  DATA : from_date TYPE p0001-begda,
         to_date TYPE p0001-begda.

  CLEAR : from_date,to_date.

  CONCATENATE  p_yymm  '01'  INTO  from_date.


  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = from_date
      days      = '00'
      months    = '01'
      signum    = '-'
      years     = '00'
    IMPORTING
      calc_date = to_date.

  p_prev = to_date(6).

ENDFORM.                    " previous_month
*&---------------------------------------------------------------------*
*&      Form  read_ref_routing_rp
*&---------------------------------------------------------------------*
FORM read_ref_routing_rp.
  REFRESH it_fsc_rp.

*reference rate routing
  SELECT DISTINCT a~usr01 b~arbpl a~usr02
       INTO TABLE it_fsc_rp
        FROM plpo AS a
           LEFT OUTER JOIN crhd AS b
              ON b~objid = a~arbid
             AND b~objty = 'A'
           WHERE a~plnty EQ 'M'
             AND a~plnnr EQ 'RP'
             AND a~werks EQ p_werks
             AND a~usr01 <> ''           "nvadd <> 'X'
        ORDER BY a~usr01 b~arbpl.

  READ TABLE it_fsc_rp INDEX sy-dbcnt.
  g_rp_last    = it_fsc_rp-rp.
  g_rp_last_xx = '02'.
  g_rp_last_xy = '06'.

  READ TABLE it_fsc_rp INDEX 1.
  it_fsc_rp-shop = 'MXBX'.         "MXSX for SHOP COST REPORT!!!
  MODIFY it_fsc_rp INDEX 1.

  g_rp_first = it_fsc_rp-rp.

* RP Point
  SELECT a~reppoint_ext a~reppoint b~reppoint_pred
       INTO TABLE it_rp_ver
    FROM ppc_rp AS a
    INNER JOIN ppc_rp_vers AS b
       ON a~reppoint = b~reppoint.


ENDFORM.                    " read_ref_routing_rp
*&---------------------------------------------------------------------*
*&      Form  GET_PREVIOUS_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_WIP_MATNR  text
*      -->P_IT_WIP_VERSION  text
*----------------------------------------------------------------------*
FORM get_previous_month USING    p_matnr
                                 p_version
                                 p_rppoint.
  READ TABLE it_prev_wips WITH KEY
                              gubun   = 'W'
                              matnr   = p_matnr
                              version = p_version
                              rppoint = p_rppoint.
  IF sy-subrc = 0.
    it_wip-bwqty = it_prev_wips-ewqty.
*    it_wip-cfqty = - it_prev_wips-adjqty.
  ENDIF.
  IF p_yymm = '200406'.   "SYSTEM GO-LIVE
    it_wip-bwqty = '0'.
  ENDIF.
ENDFORM.                    " GET_PREVIOUS_MONTH
*&---------------------------------------------------------------------*
*&      Form  get_outquantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_WIP_MATNR    text
*      -->P_IT_WIP_VERSION  text
*      -->P_IT_WIP_RPPOINT  text
*----------------------------------------------------------------------*
FORM get_outquantity USING    p_matnr
                              p_version
                              p_rppoint. " 18

  DATA  :z_rppoint LIKE ztco_wip-rppoint,
         lw_wip    LIKE it_wip.

  CLEAR z_rppoint.

* First RP; In = Out
  IF p_rppoint EQ g_rp_first.
    it_wip-inqty = it_wip-outqty.
    MODIFY it_wip FROM it_wip TRANSPORTING inqty.
  ENDIF.

*In case : XX => out qty of rp 2 = out qty of rp 18
*          XY => Out Qty of rp 6 = out qty of rp 18
  IF ( p_matnr+4(2) EQ 'XX'  ) AND ( p_rppoint >= g_rp_last_xx ).
    EXIT.
*    READ TABLE it_wip_temp WITH KEY matnr   = p_matnr
*                                    version = p_version
*                                    rppoint = g_rp_last.
*    IF sy-subrc = 0.
*      it_wip-inqty = it_wip_temp-outqty.
*    ENDIF.
  ELSEIF ( p_matnr+4(2) EQ 'XY' ) AND  ( p_rppoint >= g_rp_last_xy ).
    EXIT.
*    READ TABLE it_wip_temp WITH KEY matnr   = p_matnr
*                                    version = p_version
*                                    rppoint = g_rp_last.
*    IF sy-subrc = 0.
*      it_wip-inqty = it_wip_temp-outqty.
*    ENDIF.
  ELSE.

*-----get next RP, and determine INPUT
    READ TABLE it_rp_ver WITH KEY rp_ext = p_rppoint.
    IF sy-subrc = 0.
      READ TABLE it_rp_ver WITH KEY rp_pred   = it_rp_ver-rp.
      z_rppoint      = it_rp_ver-rp_ext .
    ENDIF.
    READ TABLE it_wip INTO lw_wip
             WITH KEY   matnr   = p_matnr
                        version = p_version
                        rppoint = z_rppoint.
    IF sy-subrc = 0.
      lw_wip-inqty = lw_wip-inqty + it_wip-outqty.
*    "Update next RP or Insert
      MODIFY it_wip FROM lw_wip INDEX sy-tabix.
    ELSE.
      it_wip-inqty   = it_wip-outqty.
      it_wip-outqty  = 0.
      it_wip-rppoint = z_rppoint.
      APPEND it_wip TO it_wip.
    ENDIF.
  ENDIF.
ENDFORM.                    " get_outquantity
*&---------------------------------------------------------------------*
*&      Form  update_ztco_wip
*&---------------------------------------------------------------------*
FORM update_ztco_wip.
  REFRESH it_ztco_wip.
  IF r_bct EQ 'X'.
*Delete Old data .
    DELETE FROM ztco_wip WHERE yymm    EQ p_yymm
                           AND gubun   EQ 'W'
                           AND matnr   IN s_matnr
                           AND version IN s_verid.
  ENDIF.


  CLEAR: it_wip,f_save.
  SORT it_wip.
  DELETE ADJACENT DUPLICATES FROM it_wip.

  LOOP AT it_wip.
    MOVE-CORRESPONDING it_wip TO it_ztco_wip.
    it_ztco_wip-yymm  = p_yymm.
    it_ztco_wip-erdat = sy-datum.
    it_ztco_wip-erzet = sy-uzeit.
    it_ztco_wip-ernam = sy-uname.
    APPEND it_ztco_wip.CLEAR it_ztco_wip.
  ENDLOOP.

  SORT it_ztco_wip BY matnr version yymm rppoint.

  INSERT ztco_wip FROM TABLE it_ztco_wip
       ACCEPTING DUPLICATE KEYS .

  IF sy-subrc = 0.
    WRITE:/ 'Saved', sy-dbcnt.
    COMMIT WORK.
  ELSE.
    WRITE:/ 'Error during saving(W)'.
    ROLLBACK WORK.
    STOP.
  ENDIF.

ENDFORM.                    " update_ztco_wip
*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD
*&---------------------------------------------------------------------*
FORM check_period.
  IF p_yymm+4(2) LT '01' OR p_yymm+4(2) GT '12'.
    MESSAGE s001 WITH p_yymm.
    STOP.
  ENDIF.

  CONCATENATE p_yymm(6) '01' INTO g_startdt.
* MONTH END DATE
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = g_startdt
    IMPORTING
      last_day_of_month = g_last_date.

ENDFORM.                    " CHECK_PERIOD
*&---------------------------------------------------------------------*
*&      Form  read_ztco_wip
*&---------------------------------------------------------------------*
FORM read_ztco_wip USING f_check.
  REFRESH it_out.
  CLEAR f_check.
  CASE 'X'.
    WHEN r_rp.
      PERFORM read_rppoint.
    WHEN r_shop.
      PERFORM read_shop.
    WHEN r_wct.
      PERFORM read_workcenter.
    WHEN r_fsc.
      PERFORM read_fsc.
  ENDCASE.

  DESCRIBE TABLE it_out LINES w_int.
  IF w_int <> 0.
    f_check = 'X'.
  ELSE.
    MESSAGE i000 WITH 'No entries'.
    f_check = ' '.
    STOP.
  ENDIF.
ENDFORM.                    " read_ztco_wip
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
FORM display.

  PERFORM field_category.
  PERFORM comment_build USING  w_top_of_page[].
  PERFORM set_build_event.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
      it_fieldcat        = gt_fieldcat
      i_save             = 'A'
      it_events          = w_eventcat[]
    TABLES
      t_outtab           = it_out
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.                    " DISPLAY
*&---------------------------------------------------------------------*
*&      Form  field_category
*&---------------------------------------------------------------------*
FORM field_category.


  CASE 'X'.
    WHEN r_rp.
      PERFORM field_setting(zcogsrev) TABLES gt_fieldcat USING :
      'GUBUN'     'Type'          '04' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
      'SHOP'      'Shop'          '04' 'X' 'L'  ' '  ' '  '  ' '  ' ' ',
      'RPPOINT'   'RP'            '03' 'X' 'L'  ' '  ' '  '  ' '  ' ' ',
      'MATNR'     'Product'       '18' 'X' 'L'  ' '  ' '  '  ' '  ' ' ',
      'VERSION'   'Ver'           '5'  'X' 'L'  ' '  ' '  '  ' '  ' ' '.
    WHEN r_shop.
      PERFORM field_setting(zcogsrev) TABLES gt_fieldcat USING :
      'GUBUN'     'Type'          '04' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
      'SHOP'      'Shop'          '04' 'X' 'L'  ' '  ' '  '  ' '  ' ' ',
      'MATNR'     'Product'       '18' 'X' 'L'  ' '  ' '  '  ' '  ' ' ',
      'VERSION'   'Ver'           '5'  'X' 'L'  ' '  ' '  '  ' '  ' ' '.
    WHEN r_wct.
      PERFORM field_setting(zcogsrev) TABLES gt_fieldcat USING :
      'GUBUN'     'Type'          '04' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
      'SHOP'      'Shop'          '04' 'X' 'L'  ' '  ' '  '  ' '  ' ' ',
      'RPPOINT'   'RP'            '03' 'X' 'L'  ' '  ' '  '  ' '  ' ' ',
      'WORKCT'    'WorkCenter'    '10' 'X' 'L'  ' '  ' '  '  ' '  ' ' ',
      'MATNR'     'Product'       '18' 'X' 'L'  ' '  ' '  '  ' '  ' ' ',
      'VERSION'   'Ver'           '5'  'X' 'L'  ' '  ' '  '  ' '  ' ' '.
    WHEN OTHERS.
      PERFORM field_setting(zcogsrev) TABLES gt_fieldcat USING :
      'GUBUN'     'Type'          '04' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
      'MATNR'     'Product'       '18' 'X' 'L'  ' '  ' '  '  ' '  ' ' ',
      'VERSION'   'Ver'           '5'  'X' 'L'  ' '  ' '  '  ' '  ' ' '.
  ENDCASE.

  PERFORM field_setting(zcogsrev) TABLES gt_fieldcat USING :
      'BWQTY'    'Beginning WIP' '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
      'INQTY'    'Input WIP'     '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
      'ADJQTY'   'Adj qty'       '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
      'CFQTY'    'Adj-r qty'     '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
      'OUTQTY'   'Output WIP'    '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
      'EWQTY'    'Ending WIP'    '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
      'SCRAPQTY' 'Scrap'         '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'X'.

ENDFORM.                    " field_category
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
FORM comment_build USING  lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        l_title(50),
        l_date(50).
*-------------- HEADER
  CASE 'X'.
    WHEN r_rp.
      CONCATENATE text-005 text-r01 INTO l_title.
    WHEN r_shop.
      CONCATENATE text-005 text-r02 INTO l_title.
    WHEN r_wct.
      CONCATENATE text-005 text-r03 INTO l_title.
  ENDCASE.
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = l_title .    "HEADER TITLE (H001)
  APPEND ls_line TO lt_top_of_page.

  l_date(6) = 'Year :'.
  l_date+7(4) = p_yymm(4).
  l_date+17(7) = 'Month :'.
  l_date+25(2) = p_yymm+4(2).

  ls_line-typ  = 'S'.
  ls_line-key  = 'Period'.
  ls_line-info = l_date.
  APPEND ls_line TO lt_top_of_page.

ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  set_build_event
*&---------------------------------------------------------------------*
FORM set_build_event.

*  w_eventcat-name = 'TOP_OF_PAGE'.
  w_eventcat-form = 'DISPLAY_HEADER'.
  APPEND w_eventcat.

ENDFORM.                    " set_build_event
*&---------------------------------------------------------------------*
*&      Form  dispaly_heager
*----------------------------------------------------------------------*
FORM display_header.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = 'Z_HYUNDAI_LOGO'
*     i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = w_top_of_page.
ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  read_rppoint
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_rppoint.
  DATA: l_out LIKE it_out.

  SELECT DISTINCT
         gubun rppoint shop
         matnr version
         bwqty inqty outqty ewqty scrapqty
   INTO  (it_out-gubun,it_out-rppoint,it_out-shop,
          it_out-matnr,it_out-version,
          it_out-bwqty,it_out-inqty,it_out-outqty,it_out-ewqty,
          it_out-scrapqty)
      FROM ztco_wip
       WHERE gubun   EQ 'W'
         AND plant   EQ p_werks
         AND matnr   IN s_matnr
         AND version IN s_verid
         AND yymm    EQ p_yymm
         AND rppoint <> space.

    APPEND it_out.

*    CLEAR l_out.
*    READ TABLE it_out INTO l_out
*           WITH KEY rppoint = it_out-rppoint
*                    matnr   = it_out-matnr
*                    version = it_out-version.
*
*    IF sy-subrc = 0.
*    ELSE.
*      APPEND it_out.
*    ENDIF.
  ENDSELECT.


ENDFORM.                    " read_rppoint
*&---------------------------------------------------------------------*
*&      Form  READ_SHOP
*&---------------------------------------------------------------------*
FORM read_shop.
  DATA : BEGIN OF it_shop OCCURS 0,
          gubun    LIKE ztco_wip-gubun,
          shop     LIKE ztco_wip-shop,
          matnr    LIKE ztco_wip-matnr,
          version  LIKE ztco_wip-version,
          bwqty    LIKE ztco_wip-bwqty,
          inqty    LIKE ztco_wip-inqty,
          outqty   LIKE ztco_wip-outqty,
          ewqty    LIKE ztco_wip-ewqty,
          scrapqty LIKE ztco_wip-scrapqty,
          rppoint  LIKE ztco_wip-rppoint,
         END OF it_shop.
  DATA  :z_rp LIKE ztco_wip-rppoint.
  REFRESH it_shop.CLEAR z_rp.

* Sum in ONE RP
  SELECT DISTINCT
         gubun shop matnr version bwqty inqty outqty ewqty scrapqty
         rppoint
   INTO  TABLE it_shop
      FROM ztco_wip
       WHERE gubun   EQ 'W'
         AND plant   EQ p_werks
         AND matnr   IN s_matnr
         AND version IN s_verid
         AND yymm    EQ p_yymm
   ORDER BY matnr version shop  rppoint.

*Collect
*Beginning WIP : Sum of beginning WIPS of the RPs in a shop
*Ending WIP : Sum of ending WIPS of the RPs in a shop
*Scrap : Sum of SCRAP QTY of the RPs in a shop
  SORT it_shop BY matnr version shop rppoint.
  LOOP AT it_shop.
    MOVE : it_shop-gubun   TO it_out-gubun,
           it_shop-shop    TO it_out-shop,
           it_shop-matnr   TO it_out-matnr,
           it_shop-version TO it_out-version,
           it_shop-bwqty   TO it_out-bwqty,
           it_shop-ewqty   TO it_out-ewqty.

    COLLECT it_out.
  ENDLOOP.

*Input QTY : INPUT QTY of the first RP in a shop
*output QTY : OUTPUT QTY of the last RP in a shop
  LOOP AT it_out.
*input qty
    SELECT MIN( rppoint ) INTO z_rp  FROM ztco_wip
        WHERE  shop    EQ it_out-shop
          AND  matnr   EQ it_out-matnr
          AND  version EQ it_out-version.

    READ TABLE it_shop WITH KEY shop    = it_out-shop
                                matnr   = it_out-matnr
                                version = it_out-version
                                rppoint = z_rp.
    MOVE it_shop-inqty TO it_out-inqty.
*output qty
    CLEAR z_rp.
    SELECT MAX( rppoint ) INTO z_rp  FROM ztco_wip
        WHERE  shop    EQ it_out-shop
          AND  matnr   EQ it_out-matnr
          AND  version EQ it_out-version.

    READ TABLE it_shop WITH KEY shop    = it_out-shop
                                matnr   = it_out-matnr
                                version = it_out-version
                                rppoint = z_rp.
    MOVE it_shop-outqty TO it_out-outqty.

    MODIFY it_out FROM it_out.
    CLEAR it_out.
  ENDLOOP.

ENDFORM.                    " READ_SHOP
*&---------------------------------------------------------------------*
*&      Form  READ_WORKCENTER
*&---------------------------------------------------------------------*
FORM read_workcenter.

  SELECT gubun workct shop
         matnr version bwqty inqty outqty ewqty scrapqty
         rppoint
   INTO  CORRESPONDING FIELDS OF TABLE it_out
      FROM ztco_wip
       WHERE gubun   EQ 'W'
         AND plant   EQ p_werks
         AND matnr   IN s_matnr
         AND version IN s_verid
         AND yymm    EQ p_yymm.

ENDFORM.                    " READ_WORKCENTER
*&---------------------------------------------------------------------*
*&      Form  press_engin_qty
*&---------------------------------------------------------------------*
FORM press_engin_qty.
  CLEAR it_wip.

  SORT it_afru BY werks matnr .
  LOOP AT it_afru.
    IF sy-tabix EQ 1.
      MOVE : it_afru-werks   TO  it_wip-plant,
             it_afru-matnr   TO  it_wip-matnr.
    ENDIF.

    IF it_afru-werks  <> it_wip-plant OR
       it_afru-matnr  <>  it_wip-matnr.

      APPEND it_wip.CLEAR it_wip.
      MOVE : it_afru-werks   TO  it_wip-plant,
             it_afru-matnr   TO  it_wip-matnr.
    ENDIF.

    MOVE :'W'                TO it_wip-gubun,
           it_afru-werks      TO it_wip-plant,
           p_yymm             TO it_wip-yymm,
           it_afru-arbpl      TO it_wip-workct,
           it_afru-arbpl(4)   TO it_wip-shop,
           it_afru-meinh      TO it_wip-confunit.

    IF it_afru-stokz = 'X'.
      it_afru-lmnga = it_afru-lmnga * -1.
      it_afru-xmnga = it_afru-xmnga * -1.
    ENDIF.

    it_wip-inqty = it_wip-inqty + it_afru-lmnga + it_afru-xmnga.
    it_wip-outqty = it_wip-outqty + it_afru-lmnga.
    it_wip-scrapqty =  it_wip-scrapqty + it_afru-xmnga.

    AT LAST.
      APPEND it_wip.CLEAR it_wip.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " press_engin_qty
*&---------------------------------------------------------------------*
*&      Form  get_shop_workcenter
*&---------------------------------------------------------------------*
FORM get_shop_workcenter USING lt_wip LIKE it_wip.
*  DATA : f_sortb(10) TYPE c .
*  DATA : it_crhd LIKE crhd OCCURS 0 WITH HEADER LINE.
*  REFRESH it_crhd.
*  CLEAR f_sortb.

  READ TABLE it_fsc_rp WITH KEY rp = lt_wip-rppoint.

*Routing
  DATA: BEGIN OF it_routing OCCURS 0,
          matnr TYPE mara-matnr,
          dispo TYPE marc-dispo,
          plnnr TYPE plko-plnnr,
          plnal TYPE plko-plnal,
          vornr TYPE plpo-vornr,
          arbpl TYPE crhd-arbpl,
          sortb TYPE crhd-sortb,
        END OF it_routing.

  DATA : l_plnnr LIKE mapl-plnnr,
         l_plnal LIKE mapl-plnal.
  DATA : l_matnr LIKE plaf-matnr,
         l_verid LIKE plaf-verid,
         l_plnng LIKE mkal-plnng,
         l_alnag LIKE mkal-alnag.


  CLEAR : it_routing[],l_verid,l_plnnr,l_plnal.

*  SELECT SINGLE dispo INTO l_dispo
*    FROM marc
*     WHERE matnr EQ it_wip-matnr
*       AND werks EQ p_werks .

  l_plnnr = it_wip-matnr+6(7) .
  PERFORM get_mi_routing USING l_plnnr.

  LOOP AT gt_mi_routing WHERE plnnr = l_plnnr
                          AND sortb = lt_wip-rppoint.
    MOVE-CORRESPONDING gt_mi_routing TO it_routing.
    COLLECT it_routing. CLEAR it_routing.
  ENDLOOP.

  SORT it_routing BY matnr arbpl sortb
                      ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_routing
           COMPARING matnr arbpl sortb vornr.


  CLEAR w_int.
  DESCRIBE TABLE it_routing LINES w_int.
  IF w_int <> 0.
    LOOP AT it_routing.
      lt_wip-shop   = it_fsc_rp-shop.
      lt_wip-workct = it_routing-arbpl.
      IF sy-tabix = 1.
        MODIFY it_wip FROM lt_wip.
      ELSE.
        APPEND lt_wip TO it_wip_temp2.
      ENDIF.
    ENDLOOP.
  ELSE.
*---W/C not assigned to RP
    lt_wip-shop = it_fsc_rp-shop.
    MODIFY it_wip FROM lt_wip.
  ENDIF.


*  SELECT SINGLE dispo INTO l_dispo
*    FROM marc
*     WHERE matnr EQ it_wip-matnr
*       AND werks EQ p_werks .
*
*
*  SELECT SINGLE  plnng alnag INTO (l_plnnr,l_plnal)
*       FROM mkal
*        WHERE matnr EQ it_wip-matnr
*          AND verid EQ it_wip-version
*          AND werks EQ p_werks .
*
*  SELECT   ma~matnr  mr~dispo pk~plnnr
*           pk~plnal  pp~vornr ch~arbpl ch~sortb pp~datuv
*           pp~arbid
*    INTO CORRESPONDING FIELDS OF TABLE gt_mi_routing
*      FROM ( ( ( ( ( ( mara AS ma
*        INNER JOIN marc AS mr ON ma~matnr = mr~matnr )
*        INNER JOIN mapl AS mp ON mr~matnr = mp~matnr )
*        INNER JOIN plko AS pk ON mp~plnnr = pk~plnnr AND
*                                 mp~plnal = pk~plnal )
*        INNER JOIN plas AS pa ON mp~plnty = pa~plnty AND
*                                 mp~plnnr = pa~plnnr AND
*                                 mp~plnal = pa~plnal )
*        INNER JOIN plpo AS pp ON pk~plnnr = pp~plnnr AND
*                                 pa~plnty = pp~plnty AND
*                                 pa~plnnr = pp~plnnr AND
*                                 pa~plnkn = pp~plnkn )
**                                 pa~zaehl = pp~zaehl )
*        INNER JOIN crhd AS ch ON pp~arbid = ch~objid )
*      WHERE mp~plnty = 'R' AND
*            mp~loekz = ' ' AND
*            pa~loekz = ' ' AND
*            ma~matnr EQ it_wip-matnr   AND
*            mr~werks EQ p_werks  AND
*            mr~dispo EQ l_dispo  AND
*            pk~plnnr EQ l_plnnr  AND
*            pk~plnal EQ l_plnal  AND
*            pk~datuv <= l_last_date AND
*            pa~plnfl EQ '000000' AND
*            ch~objty EQ 'A' AND
*            ch~sortb EQ it_wip-rppoint.


*
*  LOOP AT it_crhd.
*    IF sy-tabix = 1.
*      MOVE : it_crhd-arbpl TO lt_wip-workct.
*      MODIFY it_wip FROM lt_wip.
*    ELSE.
*      MOVE : it_crhd-arbpl TO lt_wip-workct.
*      APPEND lt_wip TO it_wip_temp2.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " get_shop_workcenter
*&---------------------------------------------------------------------*
*&      Form  filled_previous_month
*&---------------------------------------------------------------------*
FORM filled_previous_month USING p_prev.
  DATA wa_wip LIKE ztco_wip.

  CLEAR wa_wip.
  LOOP AT it_prev_wips WHERE gubun = 'W'
                         AND ewqty <> '0'.
    READ TABLE it_wip WITH KEY matnr   = it_prev_wips-matnr
                               version = it_prev_wips-version
                               rppoint = it_prev_wips-rppoint.
    IF sy-subrc <> 0.
      SELECT  * INTO wa_wip FROM ztco_wip
                WHERE  gubun   = 'W'
                  AND  plant   = it_prev_wips-plant
                  AND  matnr   = it_prev_wips-matnr
                  AND  version = it_prev_wips-version
                  AND  yymm    = p_prev
                  AND  rppoint = it_prev_wips-rppoint.
        MOVE-CORRESPONDING wa_wip TO it_wip.
        MOVE : wa_wip-ewqty TO it_wip-bwqty,
               it_wip-bwqty TO it_wip-ewqty.
        CLEAR :it_wip-mandt,it_wip-inqty,it_wip-outqty,it_wip-scrapqty.
        APPEND it_wip.CLEAR :it_wip,wa_wip.
      ENDSELECT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " filled_previous_month
*&---------------------------------------------------------------------*
*&      Form  calc_f_wips
*&---------------------------------------------------------------------*
FORM calc_f_wips.

  CLEAR :it_ztco_fsc, it_ztco_wip.

  it_fsc_wip[] = it_ztco_wip[].
  SORT it_fsc_wip BY matnr version.
  DELETE ADJACENT DUPLICATES FROM it_fsc_wip COMPARING matnr version.

  LOOP AT it_fsc_wip.
    CLEAR :it_ztco_fsc,it_product.

    READ TABLE it_product WITH KEY matnr = it_fsc_wip-matnr.
    IF it_product-categ = 'F'.
      PERFORM fsc_calculation USING it_fsc_wip.
    ELSE.
      PERFORM halb_calculation USING it_fsc_wip.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " calc_f_wips
*&---------------------------------------------------------------------*
*&      Form  FSC_CALCULATION
*&---------------------------------------------------------------------*
FORM fsc_calculation USING p_fsc_wip LIKE it_ztco_wip.

  MOVE-CORRESPONDING p_fsc_wip TO it_ztco_fsc.

*Begining qty
  CLEAR it_prev_wips.
  READ TABLE it_prev_wips WITH KEY gubun   = 'F'
                                   plant   = p_fsc_wip-plant
                                   matnr   = p_fsc_wip-matnr
                                   version = p_fsc_wip-version.
  IF sy-subrc = 0.
    it_ztco_fsc-bwqty = it_prev_wips-ewqty.
    it_ztco_fsc-cfqty = - it_prev_wips-adjqty.
  ELSE.
    it_ztco_fsc-bwqty = 0.
  ENDIF.
*Input qty
  READ TABLE it_ztco_wip WITH KEY matnr   = p_fsc_wip-matnr
                                  version = p_fsc_wip-version
                                  rppoint = g_rp_first.
*                                 workct  = 'MXBXB1'.
  IF sy-subrc = 0.
    MOVE it_ztco_wip-inqty TO it_ztco_fsc-inqty.
  ELSE.
    it_ztco_fsc-inqty = 0.
  ENDIF.

*Out qty
  IF     ( p_fsc_wip-matnr+4(2) EQ 'XX'  ).
    READ TABLE it_ztco_wip WITH KEY matnr   = p_fsc_wip-matnr
                                    version = p_fsc_wip-version
                                    rppoint = g_rp_last_xx.
  ELSEIF ( p_fsc_wip-matnr+4(2) EQ 'XY'  ).
    READ TABLE it_ztco_wip WITH KEY matnr   = p_fsc_wip-matnr
                                    version = p_fsc_wip-version
                                    rppoint = g_rp_last_xy.
  ELSE.
    READ TABLE it_ztco_wip WITH KEY matnr   = p_fsc_wip-matnr
                                    version = p_fsc_wip-version
                                    rppoint = g_rp_last.
  ENDIF.
  IF sy-subrc = 0.
    MOVE it_ztco_wip-outqty TO it_ztco_fsc-outqty.
  ELSE.
    it_ztco_fsc-outqty = 0.
  ENDIF.

*SPEC CHG
  READ TABLE it_wip_adj WITH KEY matnr   = p_fsc_wip-matnr
                                  version = p_fsc_wip-version.
  IF sy-subrc = 0.
    it_ztco_fsc-adjqty = it_wip_adj-adjqty.
  ENDIF.

*Ending WIP
  it_ztco_fsc-ewqty = it_ztco_fsc-bwqty + it_ztco_fsc-inqty
                      + it_ztco_fsc-adjqty + it_ztco_fsc-cfqty
                      - it_ztco_fsc-outqty - it_ztco_fsc-scrapqty.
*
  it_ztco_fsc-gubun = 'F'.
  it_ztco_fsc-shop  = ' '.
  it_ztco_fsc-workct = ' '.
  it_ztco_fsc-rppoint = ''.
  it_ztco_fsc-yymm  = p_yymm.
  it_ztco_fsc-erdat = sy-datum.
  it_ztco_fsc-erzet = sy-uzeit.
  it_ztco_fsc-ernam = sy-uname.
  APPEND it_ztco_fsc.CLEAR it_ztco_fsc.

ENDFORM.                    " FSC_CALCULATION
*&---------------------------------------------------------------------*
*&      Form  HALB_CALCULATION
*&---------------------------------------------------------------------*
FORM halb_calculation USING  p_fsc_wip LIKE it_ztco_wip.

  MOVE-CORRESPONDING p_fsc_wip TO it_ztco_fsc.

*Begining qty
  CLEAR it_prev_wips.
  READ TABLE it_prev_wips WITH KEY gubun   = 'F'
                                   plant   = p_fsc_wip-plant
                                   matnr   = p_fsc_wip-matnr
                                   version = p_fsc_wip-version.
  IF sy-subrc = 0.
    it_ztco_fsc-bwqty = it_prev_wips-ewqty.
    it_ztco_fsc-cfqty = - it_prev_wips-adjqty.
  ELSE.
    it_ztco_fsc-bwqty = 0.
  ENDIF.
*Input qty
  IF p_fsc_wip-matnr+4(2) EQ 'XX' OR p_fsc_wip-matnr+4(2) EQ 'XY'.
    READ TABLE it_ztco_wip WITH KEY matnr   = p_fsc_wip-matnr
                                    version = p_fsc_wip-version
                                    workct  = 'MXBXB1'.
    IF sy-subrc = 0.
      MOVE it_ztco_wip-inqty TO it_ztco_fsc-inqty.
    ELSE.
      it_ztco_fsc-inqty = 0.
    ENDIF.
  ELSE.
    READ TABLE it_ztco_wip WITH KEY matnr   = p_fsc_wip-matnr
                                  version = p_fsc_wip-version.
    IF sy-subrc = 0.
      MOVE it_ztco_wip-inqty TO it_ztco_fsc-inqty.
    ELSE.
      it_ztco_fsc-inqty = 0.
    ENDIF.
  ENDIF.
*Out qty
  IF p_fsc_wip-matnr+4(2) EQ 'XX'.
    READ TABLE it_ztco_wip WITH KEY matnr   = p_fsc_wip-matnr
                                    version = p_fsc_wip-version
                                    workct  = 'MXBXM1'.
    IF sy-subrc = 0.
      MOVE it_ztco_wip-outqty TO it_ztco_fsc-outqty.
    ELSE.
      it_ztco_fsc-outqty = 0.
    ENDIF.
  ELSEIF  p_fsc_wip-matnr+4(2) EQ 'XY'.
*Out qty
    READ TABLE it_ztco_wip WITH KEY matnr   = p_fsc_wip-matnr
                                    version = p_fsc_wip-version
                                    workct  = 'MXPX30'.
    IF sy-subrc = 0.
      MOVE it_ztco_wip-outqty TO it_ztco_fsc-outqty.
    ELSE.
      it_ztco_fsc-outqty = 0.
    ENDIF.
  ELSE.
    READ TABLE it_ztco_wip WITH KEY matnr   = p_fsc_wip-matnr
                                    version = p_fsc_wip-version.
    IF sy-subrc = 0.
      MOVE it_ztco_wip-outqty TO it_ztco_fsc-outqty.
    ELSE.
      it_ztco_fsc-outqty = 0.
    ENDIF.
  ENDIF.
*Ending WIP
  it_ztco_fsc-ewqty = it_ztco_fsc-bwqty + it_ztco_fsc-inqty -
                      it_ztco_fsc-outqty - it_ztco_fsc-scrapqty.
*
  it_ztco_fsc-gubun = 'F'.
  it_ztco_fsc-shop  = ' '.
  it_ztco_fsc-workct = ' '.
  it_ztco_fsc-rppoint = ''.
  it_ztco_fsc-yymm  = p_yymm.
  it_ztco_fsc-erdat = sy-datum.
  it_ztco_fsc-erzet = sy-uzeit.
  it_ztco_fsc-ernam = sy-uname.
  APPEND it_ztco_fsc.CLEAR it_ztco_fsc.

ENDFORM.                    " HALB_CALCULATION
*&---------------------------------------------------------------------*
*&      Form  UPDATE_zTCO_WIP_FSC
*&---------------------------------------------------------------------*
FORM update_ztco_wip_fsc.
  IF r_bct EQ 'X'.
*Delete Old data .
    DELETE FROM ztco_wip WHERE yymm EQ p_yymm
                           AND gubun EQ 'F'
                           AND matnr   IN s_matnr
                           AND version IN s_verid.
  ENDIF.

  SORT it_ztco_fsc BY matnr version yymm rppoint.

  INSERT ztco_wip FROM TABLE it_ztco_fsc
       ACCEPTING DUPLICATE KEYS .

  IF sy-subrc = 0.
    MESSAGE s009 WITH 'ZTCO_WIP_FSC'.
    COMMIT WORK.
  ELSE.
    MESSAGE s045 WITH 'ZTCO_WIP_FSC'.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " UPDATE_zTCO_WIP_FSC
*&---------------------------------------------------------------------*
*&      Form  READ_FSC
*&---------------------------------------------------------------------*
FORM read_fsc.
  SELECT gubun rppoint matnr version bwqty inqty outqty ewqty scrapqty
   INTO  (it_out-gubun,it_out-rppoint,it_out-matnr,it_out-version,
          it_out-bwqty,it_out-inqty,it_out-outqty,it_out-ewqty,
          it_out-scrapqty)
      FROM ztco_wip
       WHERE gubun   EQ 'F'
         AND plant   EQ p_werks
         AND matnr   IN s_matnr
         AND version IN s_verid
         AND yymm    EQ p_yymm.

    APPEND it_out.
  ENDSELECT.

ENDFORM.                    " READ_FSC
*&---------------------------------------------------------------------*
*&      Form  READ_ROUTING
*&---------------------------------------------------------------------*
FORM read_routing USING p_afru LIKE it_afru
                        l_yymm.
  DATA: l_plnty LIKE mkal-plnty,
        l_sauft LIKE marc-sauft.

  REFRESH it_mip_routing.

  SELECT SINGLE sauft INTO l_sauft FROM marc
        WHERE matnr EQ p_afru-matnr
          AND werks EQ p_afru-werks.

  CHECK sy-subrc = 0.
  IF l_sauft = 'X'.  "REM
    l_plnty = 'R'.
  ELSE.
    l_plnty = 'N'.
  ENDIF.

  SELECT a~matnr b~plnnr b~plnal b~zaehl b~verwe
         d~vornr e~arbpl
      INTO TABLE it_mip_routing
      FROM mapl AS a
        INNER JOIN plko AS b
           ON a~plnty = b~plnty
          AND a~plnnr = b~plnnr
          AND a~plnal = b~plnal
        INNER JOIN plas AS c
           ON b~plnty = c~plnty
          AND b~plnnr = c~plnnr
        INNER JOIN plpo AS d
           ON c~plnty = d~plnty
          AND c~plnnr = d~plnnr
          AND c~plnkn = d~plnkn
        INNER JOIN crhd AS e
           ON d~arbid = e~objid
    WHERE a~plnty = l_plnty         "R:Rate routing N:Product
      AND a~matnr = p_afru-matnr
      AND a~loekz = ''
      AND b~verwe = '1'             "Usage
      AND b~datuv <= g_startdt      "Valid from
      AND b~delkz = ''              "Delete indicator
      AND c~loekz = ''              "Delete indicator
      AND d~loekz = ''
      AND e~objty = 'A'.

  SORT it_mip_routing BY matnr plnnr zaehl DESCENDING.

  DELETE ADJACENT DUPLICATES FROM it_mip_routing
         COMPARING matnr plnnr zaehl .
*  READ TABLE it_routing INDEX 1.
*  MOVE it_routing-ro_arbpl TO p_arbpl.


ENDFORM.                    " READ_ROUTING
*&---------------------------------------------------------------------*
*&      Form  read_prev_wips
*&---------------------------------------------------------------------*
FORM read_prev_wips.
*FIXME... not previous month...
  DATA: l_yymm2 LIKE ztco_wip-yymm.

  REFRESH it_prev_wips.  CLEAR p_prev.
*previous month
  PERFORM previous_month USING p_yymm
                         CHANGING p_prev.
*Previous WIP data
  SELECT DISTINCT
     gubun plant matnr version ewqty rppoint adjqty cfqty
     confunit                                               "UD1K952398
     APPENDING CORRESPONDING FIELDS OF TABLE it_prev_wips
      FROM ztco_wip
        WHERE yymm    EQ p_prev
*            AND plant   = it_product-plant
*            AND matnr   = it_product-matnr
*            AND version = it_product-verid
          AND matnr   IN s_matnr
          AND version IN s_verid
      ORDER BY plant matnr version rppoint.

  IF sy-subrc <> 0.
    BREAK-POINT.
    LOOP AT it_product.
      CLEAR l_yymm2.
      SELECT MAX( yymm ) INTO l_yymm2
          FROM ztco_wip
            WHERE yymm    < p_prev
              AND gubun   EQ 'F'
              AND plant   = it_product-plant
              AND matnr   = it_product-matnr
              AND version = it_product-verid.
      IF sy-subrc = 0.
        SELECT DISTINCT
           gubun plant matnr version ewqty rppoint adjqty cfqty
           APPENDING CORRESPONDING FIELDS OF TABLE it_prev_wips
            FROM ztco_wip
              WHERE yymm    EQ l_yymm2
                AND plant   = it_product-plant
                AND matnr   = it_product-matnr
                AND version = it_product-verid
        ORDER BY plant matnr version rppoint.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT it_prev_wips.
    IF it_prev_wips-cfqty < 0.
      it_prev_wips-cfqty = 0.
      MODIFY it_prev_wips INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
  SORT it_prev_wips BY gubun plant matnr version rppoint.
ENDFORM.                    " read_prev_wips
*&---------------------------------------------------------------------*
*&      Form  read_mts_mto
*&---------------------------------------------------------------------*
FORM read_mts_mto.

  REFRESH it_afru. CLEAR :it_wip.

  CLEAR c_yymm.
  CONCATENATE p_yymm '%' INTO c_yymm.

*Order completion confirmations
  SELECT  a~werks b~matnr SUM( a~lmnga ) SUM( a~xmnga )
          a~meinh a~stokz
   INTO TABLE it_afru
    FROM ( afru AS a
     INNER JOIN afpo AS b
        ON a~aufnr = b~aufnr )
     WHERE a~budat LIKE c_yymm
       AND b~matnr IN s_matnr
     GROUP BY a~werks b~matnr a~meinh a~stokz.

*- U1 Start
*  IF p_arch EQ 'X'.
*    PERFORM archive_read_afpo.
*  ENDIF.
*- U1 End

  LOOP AT it_afru.
    PERFORM read_routing USING it_afru p_yymm.

    LOOP AT it_mip_routing.
      MOVE it_mip_routing-arbpl TO it_afru-arbpl.
      MODIFY it_afru FROM it_afru.

      MOVE : it_afru-werks        TO  it_product-plant,
             it_afru-matnr        TO  it_product-matnr,
             'M'                  TO  it_product-categ.
      COLLECT it_product.
    ENDLOOP.
  ENDLOOP.


ENDFORM.                    " read_mts_mto
*&---------------------------------------------------------------------*
*&      Form  get_mi_routing
*&---------------------------------------------------------------------*
FORM get_mi_routing USING    f_plnnr.

  READ TABLE gt_mi_routing WITH KEY plnnr = f_plnnr.
  IF sy-subrc <> 0.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_mi_routing
      FROM plko AS a
     INNER JOIN plas AS b
        ON a~plnty = b~plnty
       AND a~plnnr = b~plnnr
     INNER JOIN plpo AS c
        ON b~plnty = c~plnty
       AND b~plnnr = c~plnnr
       AND b~plnkn = c~plnkn
     LEFT OUTER JOIN crhd AS d
        ON d~objid = c~arbid
       AND d~objty = 'A'
      WHERE a~plnty = 'M'
        AND a~plnnr = f_plnnr
        AND a~verwe = '1'             "Usage
        AND a~statu IN ('3', '4')     "Status
        AND a~datuv <= g_last_date      "Valid from
        AND a~delkz = ''              "Delete indicator
        AND b~loekz = ''              "Delete indicator
        AND c~loekz = '' .            "Delete indicator

* delete old data; change number
    SORT gt_mi_routing BY plnnr arbid ASCENDING
                          datuv       DESCENDING.
    DATA: w_routing_temp LIKE gt_mi_routing.
    LOOP AT gt_mi_routing.
      IF  gt_mi_routing-plnnr = w_routing_temp-plnnr
      AND gt_mi_routing-arbid = w_routing_temp-arbid.
        DELETE gt_mi_routing.
      ENDIF.

      IF NOT gt_mi_routing-arbpl IN s_arbpl.
        DELETE gt_mi_routing.
      ENDIF.

      w_routing_temp = gt_mi_routing.
    ENDLOOP.

  ENDIF.
ENDFORM.                    " get_mi_routing
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_PPC_ORD_INF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_ppc_ord_inf .

  TYPES: BEGIN OF ty_ppc_ord_inf,
         orderid TYPE ppc_orderid,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ppc_ord_inf.

  DATA: l_handle    TYPE sytabix,
        lt_ppc_ord_inf TYPE TABLE OF ppc_ord_inf WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ppc_ord_inf TYPE TABLE OF ty_ppc_ord_inf,
        ls_inx_ppc_ord_inf TYPE ty_ppc_ord_inf.

  DATA: lt_ppc_head     TYPE TABLE OF ppc_head WITH HEADER LINE,
        lt_ppc_head_tmp TYPE TABLE OF ppc_head WITH HEADER LINE,
        lt_ppc_rp          TYPE TABLE OF ppc_rp   WITH HEADER LINE,
        lt_ppc_ord_inf_tmp TYPE TABLE OF ppc_ord_inf WITH HEADER LINE.

  DATA: lt_ppc_head_2 LIKE it_ppc_head OCCURS 0 WITH HEADER LINE.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZPPC_ORD_INF_01'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ppc_ord_inf[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_ppc_ord_inf
    FROM (l_gentab)
   WHERE materialnr IN s_matnr
     AND version    IN s_verid.

  CHECK NOT lt_inx_ppc_ord_inf[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_ppc_ord_inf, gt_ppc_ord_inf[].
  LOOP AT lt_inx_ppc_ord_inf INTO ls_inx_ppc_ord_inf.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'PPC_ORD_INF'
        archivkey                 = ls_inx_ppc_ord_inf-archivekey
        offset                    = ls_inx_ppc_ord_inf-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_ppc_ord_inf, lt_ppc_ord_inf[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'PPC_ORD_INF'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_ppc_ord_inf
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_ppc_ord_inf[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_ppc_ord_inf INTO TABLE gt_ppc_ord_inf.
  ENDLOOP.

  SORT gt_ppc_ord_inf.
  DELETE ADJACENT DUPLICATES FROM gt_ppc_ord_inf COMPARING ALL FIELDS.

* ppc_head
  CLEAR: lt_ppc_ord_inf_tmp, lt_ppc_ord_inf_tmp[].
  lt_ppc_ord_inf_tmp[] = gt_ppc_ord_inf[].
  SORT lt_ppc_ord_inf_tmp BY orderid.
  DELETE ADJACENT DUPLICATES FROM lt_ppc_ord_inf_tmp COMPARING orderid.
  CLEAR: lt_ppc_head, lt_ppc_head[].
  SELECT *
    FROM ppc_head
    INTO CORRESPONDING FIELDS OF TABLE lt_ppc_head
    FOR ALL ENTRIES IN lt_ppc_ord_inf_tmp
   WHERE postdate LIKE c_yymm
     AND orderid  = lt_ppc_ord_inf_tmp-orderid.

* ppc_rp
  CLEAR: lt_ppc_head_tmp, lt_ppc_head_tmp[].
  lt_ppc_head_tmp[] = lt_ppc_head[].
  SORT lt_ppc_head_tmp BY reppoint.
  DELETE ADJACENT DUPLICATES FROM lt_ppc_head_tmp COMPARING reppoint.
  CLEAR: lt_ppc_rp, lt_ppc_rp[].
  SELECT *
    FROM ppc_rp
    INTO CORRESPONDING FIELDS OF TABLE lt_ppc_rp
    FOR ALL ENTRIES IN lt_ppc_head_tmp
   WHERE reppoint = lt_ppc_head_tmp-reppoint.

  CLEAR: lt_ppc_head_2, lt_ppc_head_2[].
  LOOP AT lt_ppc_head.
    MOVE-CORRESPONDING lt_ppc_head TO lt_ppc_head_2.

    "ppc_ord_inf
    CLEAR gt_ppc_ord_inf.
    READ TABLE gt_ppc_ord_inf WITH KEY orderid = gt_ppc_ord_inf-orderid.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING gt_ppc_ord_inf TO lt_ppc_head_2.
    ELSE.
      CONTINUE.
    ENDIF.

    "ppc_rp
    CLEAR lt_ppc_rp.
    READ TABLE lt_ppc_rp WITH KEY reppoint = lt_ppc_head-reppoint.
    IF sy-subrc = 0.
      lt_ppc_head_2-reppoint_ext = lt_ppc_rp-reppoint_ext.
    ELSE.
      CONTINUE.
    ENDIF.

    APPEND lt_ppc_head_2.  CLEAR lt_ppc_head_2.
  ENDLOOP.

* 5.1 Append archived data table to finally interal table
  LOOP AT lt_ppc_head_2.
    MOVE-CORRESPONDING lt_ppc_head_2 TO it_ppc_head.
    COLLECT it_ppc_head.  CLEAR it_ppc_head.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_PPC_ORD_INF
