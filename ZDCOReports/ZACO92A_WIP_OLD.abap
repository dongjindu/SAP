*&--------------------------------------------------------------------
*& REPORT                 : ZACO92A_WIP
*& Author                 : WSKIM
*& Creation Date          : 01/26/2005
*& Specification By       :
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description            : WIP
*& Modification Log
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------
*This is OLD PROGRAM
*DO NOT USE!!
*INCLUDE zaco92a_wip_top.
*----------------------------------------------------------------------*
*   INCLUDE ZACO92A_WIP_TOP                                            *
*----------------------------------------------------------------------*
REPORT zaco92a_wip MESSAGE-ID  zmco.

*Table definition
TABLES : ppc_head,ppc_ord_inf,ppc_rp,ppc_rp_vers,ztco_wip,
         afru,afpo,crhd,plpo,mara.
TYPE-POOLS: slis, vrm.

INCLUDE <icon>.
INCLUDE <symbol>.
CLASS cl_gui_resources DEFINITION LOAD.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv.
DATA: wa_repid LIKE sy-repid,
      wa_var_save(1) TYPE c             VALUE  'A',
      wa_default(1)  TYPE c,
      wa_exit(1) TYPE c,
      wa_variant LIKE disvariant,
      wa_var LIKE disvariant,
      wa_alv_function_name(30) TYPE c VALUE 'REUSE_ALV_GRID_LIST',
      wa_alv_get_info_name(40) TYPE c.
*--- ALV
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

*Internal Table definition
DATA : BEGIN OF it_ppc_head OCCURS 0,
        plant         LIKE ppc_ord_inf-plant,
        reppoint      LIKE ppc_head-reppoint,
        materialnr    LIKE ppc_ord_inf-materialnr,
        version       LIKE ppc_ord_inf-version,
        reppoint_ext  LIKE ppc_rp-reppoint_ext,
        confquant     LIKE ppc_head-confquant,
        flg_scrap     LIKE ppc_head-flg_scrap,
        flg_reversal  LIKE ppc_head-flg_reversal,
        confunit      LIKE ppc_head-confunit,
       END OF it_ppc_head.
DATA : it_wip LIKE ztco_wip OCCURS 0 WITH HEADER LINE,
       it_wip_temp LIKE it_wip OCCURS 0 WITH HEADER LINE,
       it_wip_temp2 LIKE it_wip OCCURS 0 WITH HEADER LINE.

DATA : it_ztco_wip LIKE ztco_wip OCCURS 0 WITH HEADER LINE,
       it_fsc_wip LIKE it_ztco_wip OCCURS 0 WITH HEADER LINE,
       it_ztco_fsc LIKE it_ztco_wip OCCURS 0 WITH HEADER LINE.

  DATA : BEGIN OF it_afru OCCURS 0,
          werks LIKE afru-werks,
          matnr LIKE afpo-matnr,
          arbpl LIKE crhd-arbpl, " WORK CENTER
          lmnga LIKE afru-lmnga, " YIELD
          xmnga LIKE afru-xmnga, " SCRAP
          meinh LIKE afru-meinh,
          stokz like afru-stokz,
         END OF it_afru.


*Get endwipqty of Previous month
DATA : BEGIN OF it_prev OCCURS 0,
        matnr   LIKE ztco_wip-matnr,
        version LIKE ztco_wip-version,
        rppoint LIKE ztco_wip-rppoint,
        ewqty   LIKE ztco_wip-ewqty,
       END OF it_prev.


DATA : BEGIN OF it_rp_ver OCCURS 0,
        reppoint_ext  LIKE ppc_rp-reppoint_ext,
        reppoint LIKE ppc_rp-reppoint,
        reppoint_pred LIKE ppc_rp_vers-reppoint_pred,
      END OF it_rp_ver.

DATA : BEGIN OF it_out OCCURS 0,
        gubun    LIKE ztco_wip-gubun,
        rppoint  LIKE ztco_wip-rppoint,
        shop     LIKE ztco_wip-shop,
        workct   LIKE ztco_wip-workct,
        matnr    LIKE ztco_wip-matnr,
        version  LIKE ztco_wip-version,
        bwqty    LIKE ztco_wip-bwqty,
        inqty    LIKE ztco_wip-inqty,
        outqty   LIKE ztco_wip-outqty,
        ewqty    LIKE ztco_wip-ewqty,
        scrapqty LIKE ztco_wip-scrapqty,
        chkbox       TYPE c,
        light        TYPE c,
        tabcolor     TYPE slis_t_specialcol_alv,
       END OF it_out.

*Data definiton
DATA : w_int TYPE i,
       p_check,
       c_yymm(7),
       f_save ,
       p_werks LIKE plpo-werks VALUE 'P001',
       p_prev LIKE sy-datum(6).

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
PARAMETERS : r_rp RADIOBUTTON GROUP rdg DEFAULT 'X'.
SELECTION-SCREEN :   COMMENT 28(5) text-r02.
PARAMETERS : r_shop  RADIOBUTTON GROUP rdg.
SELECTION-SCREEN :   COMMENT 40(11) text-r03.
PARAMETERS : r_wct  RADIOBUTTON GROUP rdg.
SELECTION-SCREEN :   COMMENT 58(5) text-r06.
PARAMETERS : r_fsc  RADIOBUTTON GROUP rdg.

SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  p_yymm = sy-datum(6).
* ==> Change Variant saving type
  wa_var_save = 'A'.
* ==> Change first mode   GRID or LIST
  wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
* wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : gt_fieldcat.
  CLEAR   : gs_layout.
  wa_repid = sy-repid.

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
    PERFORM get_wip_data.
    PERFORM fsc_wip_data.
  ELSEIF r_db EQ 'X'.
    PERFORM display.
  ENDIF.
*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  get_wip_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_wip_data.
*PPC head
  PERFORM read_ppc_head USING  p_yymm p_check.
  CHECK p_check EQ 'X'.
*Collect
  PERFORM collect_fsc_rppoint.
*Recalculation
  PERFORM recalculation.
*Update
  PERFORM update_ztco_wip.
ENDFORM.                    " get_wip_data
*&---------------------------------------------------------------------*
*&      Form  read_ppc_head
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0016   text
*----------------------------------------------------------------------*
FORM read_ppc_head USING lp_yymm p_check.
  CLEAR : c_yymm.

  CONCATENATE lp_yymm '%' INTO c_yymm.

  SELECT a~reppoint a~confquant a~flg_scrap a~flg_reversal  a~confunit
         b~materialnr b~plant b~version c~reppoint_ext
         INTO CORRESPONDING FIELDS OF TABLE it_ppc_head
          FROM   ( ( ppc_head AS a INNER JOIN ppc_ord_inf AS b
           ON a~orderid = b~orderid ) INNER JOIN ppc_rp AS c
              ON a~reppoint = c~reppoint )
            WHERE postdate LIKE c_yymm.

  DESCRIBE TABLE it_ppc_head LINES w_int.
  IF w_int <> 0.
    p_check = 'X'.
  ELSE.
    MESSAGE i026.
    p_check = ' '.
    STOP.
  ENDIF.

ENDFORM.                    " read_ppc_head
*&---------------------------------------------------------------------*
*&      Form  collect_fsc_rppoint
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_fsc_rppoint.
  REFRESH it_wip.
*by fsc version rp
  SORT it_ppc_head BY materialnr version reppoint_ext.
  LOOP AT it_ppc_head.
    IF sy-tabix EQ 1.
      MOVE : it_ppc_head-materialnr   TO  it_wip-matnr,
             it_ppc_head-version      TO  it_wip-version,
             it_ppc_head-reppoint_ext TO  it_wip-rppoint.
    ENDIF.

    IF it_ppc_head-materialnr <> it_wip-matnr OR
       it_ppc_head-version  <>  it_wip-version OR
       it_ppc_head-reppoint_ext <> it_wip-rppoint.

      APPEND it_wip.CLEAR it_wip.
      MOVE : it_ppc_head-materialnr   TO  it_wip-matnr,
             it_ppc_head-version      TO  it_wip-version,
             it_ppc_head-reppoint_ext TO  it_wip-rppoint.
    ENDIF.

    MOVE : 'W'                  TO it_wip-gubun,
           it_ppc_head-plant    TO it_wip-plant,
           p_yymm               TO it_wip-yymm,
           it_ppc_head-confunit TO it_wip-confunit.

    IF it_ppc_head-flg_reversal EQ 'X'.
      it_ppc_head-confquant = it_ppc_head-confquant * -1.
    ENDIF.
    it_wip-inqty = it_wip-inqty + it_ppc_head-confquant.
    IF it_ppc_head-flg_scrap EQ 'X'.
      it_wip-scrapqty = it_wip-scrapqty + it_ppc_head-confquant.
    ENDIF.

    AT LAST.
      APPEND it_wip.CLEAR it_wip.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " collect_fsc_rppoint
*&---------------------------------------------------------------------*
*&      Form  recalculation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recalculation.
data :begin of it_plpo occurs 0,
         USR01 like plpo-USR01,  "Rpoint
         USR02 like plpo-USR02,  "Shop
      end of it_plpo.
  REFRESH it_prev.CLEAR p_prev.
*previous month
  PERFORM previous_month USING p_yymm
                         CHANGING p_prev.
*Previous data
  SELECT matnr version ewqty rppoint
     INTO CORRESPONDING FIELDS OF TABLE it_prev
      FROM ztco_wip
        WHERE yymm  EQ p_prev
          AND gubun EQ 'W'.
*            plant EQ p_werks

* RP Point
  SELECT a~reppoint_ext a~reppoint b~reppoint_pred
       INTO TABLE it_rp_ver
    FROM ppc_rp AS a INNER JOIN ppc_rp_vers AS b
      ON a~reppoint = b~reppoint.


* Select RP Point & Shop
  SELECT usr01 usr02  INTO  CORRESPONDING FIELDS OF TABLE it_plpo
     FROM plpo
      WHERE plnty = 'M'
        AND plnty = 'RP'
        AND datuv <= sy-datum.

  CLEAR it_wip.REFRESH:it_wip_temp, it_wip_temp2.

  it_wip_temp[] = it_wip[].
  LOOP AT it_wip.
**Get  shop and workcenter from Reference rate routing 'PLPO Table'
*    PERFORM reference_rate_routing USING it_wip-matnr it_wip-rppoint.
*Previous month
    PERFORM get_previous_month USING it_wip-matnr it_wip-version
                                     it_wip-rppoint.
*OUT Quantity
    PERFORM get_outquantity USING it_wip-matnr it_wip-version
                                  it_wip-rppoint.
*END WIP Quantity
    it_wip-ewqty = it_wip-bwqty + it_wip-inqty - it_wip-outqty
                 - it_wip-scrapqty.
*Get  Shop and Workcenter from WorkCenter
    PERFORM get_shop_workcenter USING it_wip.

    clear it_plpo.
    read table it_plpo with key usr01 = it_wip-rppoint.
    if sy-subrc = 0 .
       it_wip-shop = it_plpo-usr02.
    else.
       it_wip-shop =  it_wip-workct(4).
    endif.
  ENDLOOP.


  CLEAR w_int.
  DESCRIBE TABLE it_wip_temp2 LINES w_int.
  IF w_int <> 0.
    INSERT LINES OF it_wip_temp2 INTO TABLE it_wip.
  ENDIF.
*in case of no this month data, Filled previous month data
  PERFORM filled_previous_month USING p_prev.
*Press & Engin qty
  PERFORM press_engin_qty.

ENDFORM.                    " recalculation
*&---------------------------------------------------------------------*
*&      Form  previous_month
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
*&      Form  reference_rate_routing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_WIP_MATNR  text
*----------------------------------------------------------------------*
FORM reference_rate_routing USING    p_matnr p_rppoint.
  DATA : BEGIN OF it_plpo,
          usr02 LIKE plpo-usr02,
          usr03 LIKE plpo-usr03,
         END OF it_plpo.

  CLEAR it_plpo.

*reference rate routing
  SELECT SINGLE a~usr02 a~usr03
       INTO  (it_plpo-usr02,it_plpo-usr03)
        FROM plpo AS a INNER JOIN crhd AS b
           ON a~arbid = b~objid
           WHERE a~plnty EQ 'M'
             AND a~plnnr EQ 'RP'
             AND a~werks EQ p_werks
             AND a~nvadd <> 'X'
             AND b~objty EQ 'A'
             AND a~usr01 EQ p_rppoint.

  MOVE : it_plpo-usr02 TO it_wip-shop,
         it_plpo-usr03 TO it_wip-workct.
ENDFORM.                    " reference_rate_routing
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
  READ TABLE it_prev WITH KEY matnr   = p_matnr
                              version = p_version
                              rppoint = p_rppoint.
  IF sy-subrc = 0.
    MOVE it_prev-ewqty TO it_wip-bwqty.
  ENDIF.
  IF p_yymm = '200406'.
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

  DATA  :z_rppoint LIKE ztco_wip-rppoint.
  CLEAR z_rppoint.
  IF p_rppoint EQ '18'.
    it_wip-outqty = it_wip-inqty.
  ELSE.
*In case : XX => out qty of rp 2 = out qty of rp 18
*          XY => Out Qty of rp 6 = out qty of rp 18
    IF ( p_matnr+4(2) EQ 'XX'  ) AND ( p_rppoint EQ '02' ).
      READ TABLE it_wip_temp WITH KEY matnr   = p_matnr
                                      version = p_version
                                      rppoint = '18'.
      IF sy-subrc = 0.
        it_wip-outqty = it_wip_temp-inqty.
      ENDIF.
    ELSEIF ( p_matnr+4(2) EQ 'XY' ) AND  ( p_rppoint EQ '06' ).
      READ TABLE it_wip_temp WITH KEY matnr   = p_matnr
                                      version = p_version
                                      rppoint = '18'.
      IF sy-subrc = 0.
        it_wip-outqty = it_wip_temp-inqty.
      ENDIF.
    ELSE.
      READ TABLE it_rp_ver WITH KEY reppoint_ext = p_rppoint.
      IF sy-subrc = 0.
       READ TABLE it_rp_ver WITH KEY reppoint_pred = it_rp_ver-reppoint.
        z_rppoint = it_rp_ver-reppoint_ext . " 18
      ENDIF.

      READ TABLE it_wip_temp WITH KEY   matnr   = p_matnr
                                        version = p_version
                                        rppoint = z_rppoint.
      IF sy-subrc = 0.
        it_wip-outqty = it_wip_temp-inqty.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " get_outquantity
*&---------------------------------------------------------------------*
*&      Form  update_ztco_wip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_ztco_wip.
  REFRESH it_ztco_wip.
  IF r_bct EQ 'X'.
*Delete Old data .
    DELETE FROM ztco_wip WHERE yymm EQ p_yymm
                           AND gubun EQ 'W'.
  ENDIF.

  CLEAR: it_wip,f_save.
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
    MESSAGE s009 WITH 'ZTCO_WIP'.
    COMMIT WORK.
  ELSE.
    MESSAGE s045 WITH 'ZTCO_WIP'.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " update_ztco_wip
*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_period.
  IF p_yymm+4(2) LT '01' OR p_yymm+4(2) GT '12'.
    MESSAGE s001 WITH p_yymm.
    STOP.
  ENDIF.

ENDFORM.                    " CHECK_PERIOD
*&---------------------------------------------------------------------*
*&      Form  read_ztco_wip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display.
* ==> 5. build field category
  PERFORM field_category.
* ==> 1. select data from db
  PERFORM read_ztco_wip USING p_check.
  CHECK p_check = 'X'.
* ==> 2. set variant default
  PERFORM set_variant CHANGING wa_var.
* ==> 3. set layout for alv style
  PERFORM set_layout CHANGING gs_layout.
* ==> 4. set events for alv
  PERFORM set_events CHANGING gt_events.
*===> 5. set event for top-of-page grid.
  PERFORM set_build_event.
*===>
  PERFORM comment_build USING  w_top_of_page[].

* ==> 7. call function display alv.

  CALL FUNCTION wa_alv_function_name
    EXPORTING
         i_callback_program      = wa_repid
         i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
         i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
         is_layout               = gs_layout
         it_fieldcat             = gt_fieldcat[]
         it_special_groups       = gt_sp_group[]
         it_sort                 = gt_sorts[]
*         IT_FILTER               =
         i_default               = wa_default
         i_save                  = wa_var_save
         is_variant              = wa_var
*         it_events               = gt_events[]
         it_events               =  w_eventcat[]
         is_print                = gs_prnt
*        IT_EVENT_EXIT           =
*           I_SCREEN_START_COLUMN   = 10
*           I_SCREEN_START_LINE     = 2
*           I_SCREEN_END_COLUMN     = 80
*           I_SCREEN_END_LINE       = 23
    TABLES
         t_outtab                = it_out.

ENDFORM.                    " DISPLAY
*&---------------------------------------------------------------------*
*&      Form  field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM field_category.

  PERFORM build_field_category  USING :
   'GUBUN'     'GUBUN'   '2' 'X' 'L'  ' '  ' '  '  ' '  ' .

  CASE 'X'.
    WHEN r_rp.
      PERFORM build_field_category  USING :
       'RPPOINT'   'RP'            '10' 'X' 'L'  ' '  ' '  '  ' '  ' .
    WHEN r_shop.
      PERFORM build_field_category  USING :
       'SHOP'      'Shop'          '10' 'X' 'L'  ' '  ' '  '  ' '  ' .
    WHEN r_wct.
      PERFORM build_field_category  USING :
       'WORKCT'    'WorkCenter'    '10' 'X' 'L'  ' '  ' '  '  ' '  ' .
  ENDCASE.

  PERFORM build_field_category  USING :
   'MATNR'     'Material No'   '18' 'X' 'L'  ' '  ' '  '  ' '  ' ,
   'VERSION'   'Version'       '7'  'X' 'L'  ' '  ' '  '  ' '  ' ,
   'BWQTY'     'Beginning WIP' '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'INQTY'     'Input WIP'     '15' ' ' ' '  ' '  ' '  '  ' '  ' ,
   'OUTQTY'    'Output WIP'    '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'EWQTY'     'Ending WIP'    '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'SCRAPQTY'  'Scrap'         '15' ' ' 'R'  ' '  ' '  '  ' '  ' .

ENDFORM.                    " field_category
*&---------------------------------------------------------------------*
*&      Form  build_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0712   text
*      -->P_0713   text
*      -->P_0714   text
*      -->P_0715   text
*      -->P_0716   text
*      -->P_0717   text
*      -->P_0718   text
*      -->P_0719   text
*      -->P_0720   text
*----------------------------------------------------------------------*
FORM build_field_category USING
                                  p_fieldname       " field name
                                  p_title           " field title
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_edit            "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l = p_title.
  ls_fieldcat-outputlen = p_outputlen.
  ls_fieldcat-key       = p_key.
  ls_fieldcat-just      = p_just.
  ls_fieldcat-edit      = p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  IF p_fieldname = 'TOT'.
    ls_fieldcat-emphasize = 'C700'.
  ENDIF.
  APPEND ls_fieldcat TO gt_fieldcat.


ENDFORM.                    " build_field_category
*&---------------------------------------------------------------------*
*&      Form  set_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_VAR  text
*----------------------------------------------------------------------*
FORM set_variant CHANGING cs_vari TYPE disvariant.

ENDFORM.                    " set_variant
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
FORM set_layout CHANGING cs_layo TYPE slis_layout_alv.

*... Display options
  cs_layo-colwidth_optimize      = space.
  cs_layo-no_colhead             = space.
  cs_layo-no_hotspot             = space.
  cs_layo-zebra                  = ' '.
  cs_layo-no_vline               = space.
  cs_layo-cell_merge             = space.
  cs_layo-no_min_linesize        = space.
  cs_layo-min_linesize           = space.
  cs_layo-max_linesize           = space.
  cs_layo-window_titlebar        = space.
  cs_layo-no_uline_hs            = space.
*... Edit
  cs_layo-edit                   = ' '."space.
  cs_layo-edit_mode              = ' '."space.
*... Exceptions
  cs_layo-lights_fieldname       = ' '.
  "=> ??? ??? ???
  cs_layo-lights_tabname         = space.
  cs_layo-lights_rollname        = space.
  cs_layo-lights_condense        = space.
*... Sums
  cs_layo-no_sumchoice           = space.
  cs_layo-no_totalline           = space.
  cs_layo-totals_before_items    = space.
  cs_layo-totals_only            = space.
  cs_layo-totals_text            = space.
  cs_layo-no_subchoice           = space.
  cs_layo-no_subtotals           = space.
  cs_layo-subtotals_text         = space.
  cs_layo-numc_sum               = 'X'.
  cs_layo-no_unit_splitting      = space.
*... Interaction
  cs_layo-box_fieldname          = 'CHKBOX'.
  cs_layo-box_tabname            = space.
  cs_layo-box_rollname           = space.
  cs_layo-expand_fieldname       = space.
  cs_layo-hotspot_fieldname      = space.
  cs_layo-no_input               = ' '.
  cs_layo-f2code                 = space.
  cs_layo-confirmation_prompt    = space.
  cs_layo-key_hotspot            = space.
  cs_layo-flexible_key           = space.
  cs_layo-reprep                 = space.
  cs_layo-group_buttons          = 'X'.
  cs_layo-no_keyfix              = space.
  cs_layo-get_selinfos           = space.
  cs_layo-group_change_edit      = 'X'.
  cs_layo-no_scrolling           = space.
  cs_layo-expand_all             = space.
  cs_layo-no_author              = space.
*... Detailed screen
  cs_layo-detail_popup           = 'X'.
  cs_layo-detail_initial_lines   = space.
  cs_layo-detail_titlebar        = space.
*... PF-status
  cs_layo-def_status             = space.
*... Display variants
  cs_layo-header_text            = space.
  cs_layo-item_text              = space.
  cs_layo-default_item           = space.
*... colour
  cs_layo-info_fieldname         = space.
  cs_layo-coltab_fieldname       = 'TABCOLOR'.
*... others
  cs_layo-list_append            = space.


ENDFORM.                    " set_layout
*&---------------------------------------------------------------------*
*&      Form  set_build_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_build_event.

  w_eventcat-name = 'TOP_OF_PAGE'.
  w_eventcat-form = 'DISPLAY_HEADER'.
  APPEND w_eventcat.

ENDFORM.                    " set_build_event
*
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
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
*&      Form  set_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_EVENTS  text
*----------------------------------------------------------------------*
FORM set_events CHANGING ct_events TYPE slis_t_event.

  FIELD-SYMBOLS: <ls_event> TYPE slis_alv_event.

  DATA: l_event TYPE lvc_fname.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type     = 0
       IMPORTING
            et_events       = ct_events
       EXCEPTIONS
            list_type_wrong = 1
            OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    DELETE ct_events WHERE name NE 'END_OF_PAGE'
                       AND name NE 'TOP_OF_PAGE'
                       AND name NE 'TOP_OF_LIST'
                       AND name NE 'END_OF_LIST'.
    LOOP AT ct_events ASSIGNING <ls_event>.
      CONCATENATE 'ALV_EVENT_'
                  <ls_event>-name
                  INTO <ls_event>-form.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " set_events
*---------------------------------------------------------------------*
*  FORM alv_event_pf_status_set
*---------------------------------------------------------------------*
FORM alv_event_pf_status_set USING rt_extab TYPE slis_t_extab.

  IF wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    SET PF-STATUS 'STANDARD_GRID' EXCLUDING rt_extab.
  ELSE.
    SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
  ENDIF.
  SET TITLEBAR  'STANDARD'.


ENDFORM.                    "alv_event_pf_status_set

*---------------------------------------------------------------------*
*  FORM alv_event_user_command
*---------------------------------------------------------------------*
FORM alv_event_user_command USING r_ucomm     LIKE sy-ucomm
                                      rs_selfield TYPE slis_selfield.

  DATA : seltab LIKE rsparams OCCURS 0 WITH HEADER LINE.
  REFRESH seltab.
*  CASE r_ucomm.
**   ---------------------------------- processing on double click.
*    WHEN '&IC1'.
**      READ TABLE it_out INDEX rs_selfield-tabindex.
**      CASE rs_selfield-fieldname.
**        WHEN 'EBELN'.
**
**          seltab-selname = 'EN_EBELN'.
**          seltab-sign =  'I'.
**          seltab-option = 'EQ'.
**          seltab-low = it_out-ebeln.
**          APPEND seltab.
**
**          seltab-selname = 'LISTU'.
**          seltab-sign =  'I'.
**          seltab-option = 'EQ'.
**          seltab-low = 'ALLES'.
**          APPEND seltab.
**
**          seltab-selname = 'S_MATNR'.
**          seltab-sign =  'I'.
**          seltab-option = 'EQ'.
**          seltab-low = it_out-matnr(18).
**          APPEND seltab.
**
**          SUBMIT rm06en00 WITH SELECTION-TABLE seltab
**                              AND RETURN.
*
*      ENDCASE.
**---------------------------------- switching view type grid or list
*    WHEN 'LIST' OR 'GRID'.
*      PERFORM switch_list_or_grid USING r_ucomm.
*  ENDCASE.
*
*  CHECK r_ucomm EQ 'LIST' OR
*        r_ucomm EQ 'GRID'.
*
*  rs_selfield-exit = 'X'.

ENDFORM.                    "alv_event_user_command
*---------------------------------------------------------------------*
*  FORM f01_alv_event_top_of_page
*---------------------------------------------------------------------*
FORM alv_event_top_of_page.
*  WRITE : /(10) 'nvestment Program' , p_prnam.
*          /(10) 'BBBBBBB',  BKPF-BUKRS INVERSE COLOR 1 INPUT ON,
*           (20) 'CCCCCCC',  BKPF-BELNR INPUT ON.
ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*       FORM alv_event_top_of_LIST                                    *
*---------------------------------------------------------------------*
FORM alv_event_top_of_list.


ENDFORM.                    "alv_event_top_of_page
*&---------------------------------------------------------------------*
*&      Form  dispaly_heager
*----------------------------------------------------------------------*
FORM display_header.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
         EXPORTING
*           i_logo             = 'Z_HYUNDAI_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
              it_list_commentary = w_top_of_page.
ENDFORM.                    " top_of_page

*&---------------------------------------------------------------------
*&      Form  switch_list_or_grid
*&---------------------------------------------------------------------
FORM switch_list_or_grid USING r_ucomm.

  DATA: ls_vari      TYPE disvariant,
       ls_slis_layo TYPE slis_layout_alv,
       lt_slis_fcat TYPE slis_t_fieldcat_alv,
       lt_slis_sort TYPE slis_t_sortinfo_alv,
       lt_slis_filt TYPE slis_t_filter_alv,
       ls_slis_prnt TYPE slis_print_alv.


  IF r_ucomm = 'LIST' AND
     wa_alv_function_name = 'REUSE_ALV_LIST_DISPLY'.
    EXIT.
  ENDIF.
  IF r_ucomm = 'GRID' AND
     wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    EXIT.
  ENDIF.
  CASE wa_alv_function_name.
    WHEN 'REUSE_ALV_LIST_DISPLAY'.
      wa_alv_get_info_name = 'REUSE_ALV_LIST_LAYOUT_INFO_GET'.
    WHEN 'REUSE_ALV_GRID_DISPLAY'.
      wa_alv_get_info_name = 'REUSE_ALV_GRID_LAYOUT_INFO_GET'.

  ENDCASE.

  CALL FUNCTION wa_alv_get_info_name
       IMPORTING
            es_layout     = ls_slis_layo
            et_fieldcat   = lt_slis_fcat
            et_sort       = lt_slis_sort
            et_filter     = lt_slis_filt
            es_variant    = ls_vari
       EXCEPTIONS
            no_infos      = 1
            program_error = 2
            OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF r_ucomm = 'LIST'.
    wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
    CALL FUNCTION wa_alv_function_name
         EXPORTING
              i_callback_program       = wa_repid
              i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
              i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
              is_layout                = ls_slis_layo
              it_fieldcat              = lt_slis_fcat
              it_sort                  = lt_slis_sort
              it_filter                = lt_slis_filt
              i_default                = ' '  "gs_test-vari_default
              i_save                   = wa_var_save
              is_variant               = ls_vari
              is_print                 = ls_slis_prnt
              it_events                = gt_events[]
         TABLES
              t_outtab                 = it_out
         EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.
  ENDIF.
  IF r_ucomm = 'GRID'.
    wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    CALL FUNCTION wa_alv_function_name
         EXPORTING
              i_callback_program       = wa_repid
              i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
              i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
              is_layout                = ls_slis_layo
              it_fieldcat              = lt_slis_fcat
              it_sort                  = lt_slis_sort
              it_filter                = lt_slis_filt
              i_default                = ' '  "gs_test-vari_default
              i_save                   = wa_var_save
              is_variant               = ls_vari
              is_print                 = ls_slis_prnt
*                it_events               = gt_events[]
         TABLES
              t_outtab                 = it_out
         EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.

  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " switch_list_or_grid
*&---------------------------------------------------------------------*
*&      Form  f4_variant
*&---------------------------------------------------------------------*
FORM f4_variant CHANGING c_variant TYPE disvariant-variant.

  DATA: ls_variant TYPE disvariant,
        l_exit     TYPE char1.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            is_variant          = ls_variant
            i_save              = 'A'
*           it_default_fieldcat =
       IMPORTING
            e_exit              = l_exit
            es_variant          = ls_variant
       EXCEPTIONS
            not_found = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF l_exit EQ space.
      c_variant = ls_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " f4_variant
*&---------------------------------------------------------------------*
*&      Form  build_sort_table
*&---------------------------------------------------------------------*
FORM build_sort_table USING  p_spos
                             p_fieldname
                             p_up
                             p_subtot
                             p_group.
  DATA: ls_sort TYPE slis_sortinfo_alv.

  ls_sort-spos      = p_spos.
  ls_sort-fieldname = p_fieldname.
  ls_sort-up        = p_up.
  ls_sort-subtot    = p_subtot.
  ls_sort-group     = p_group.
  APPEND ls_sort TO gt_sorts.
ENDFORM.                    " build_sort_table
*&---------------------------------------------------------------------*
*&      Form  set_line_color
*&---------------------------------------------------------------------*
FORM set_line_color USING    p_color.
  DATA: ls_fieldcat   TYPE slis_fieldcat_alv,
        lt_color      TYPE slis_t_specialcol_alv,
        ls_color      TYPE slis_specialcol_alv.

  REFRESH lt_color.
  CLEAR   lt_color.
  LOOP AT gt_fieldcat INTO ls_fieldcat.
    ls_color-fieldname = ls_fieldcat-fieldname.
    ls_color-color-col = p_color.
*    "cl_gui_resources=>list_col_positive.
    ls_color-color-int = cl_gui_resources=>list_intensified.
    ls_color-color-inv = 0.
    ls_color-nokeycol  = 'X'.
    APPEND ls_color TO lt_color.
*    gt_out-tabcolor = lt_color.
  ENDLOOP.

ENDFORM.                    " set_line_color
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

  SELECT gubun rppoint matnr version bwqty inqty outqty ewqty scrapqty
   INTO  (it_out-gubun,it_out-rppoint,it_out-matnr,it_out-version,
          it_out-bwqty,it_out-inqty,it_out-outqty,it_out-ewqty,
          it_out-scrapqty)
      FROM ztco_wip
       WHERE plant EQ p_werks
         AND yymm EQ p_yymm
         AND gubun EQ 'W'.

    CLEAR l_out.
    READ TABLE it_out INTO l_out
           WITH KEY rppoint = it_out-rppoint
                    matnr   = it_out-matnr
                    version = it_out-version.

    IF sy-subrc = 0.
    ELSE.
      APPEND it_out.
    ENDIF.
  ENDSELECT.


ENDFORM.                    " read_rppoint
*&---------------------------------------------------------------------*
*&      Form  READ_SHOP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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

  SELECT gubun shop matnr version bwqty inqty outqty ewqty scrapqty
         rppoint
   INTO  (it_shop-gubun,it_shop-shop,it_shop-matnr,it_shop-version,
          it_shop-bwqty,it_shop-inqty,it_shop-outqty,it_shop-ewqty,
          it_shop-scrapqty,it_shop-rppoint)
      FROM ztco_wip
       WHERE plant EQ p_werks
         AND yymm EQ p_yymm
         AND gubun EQ 'W'.

    APPEND it_shop.
  ENDSELECT.
*Collect
*Beginning WIP : Sum of beginning WIPS of the RPs in a shop
*Ending WIP : Sum of ending WIPS of the RPs in a shop
*Scrap : Sum of SCRAP QTY of the RPs in a shop
  SORT it_shop BY shop matnr version.
  LOOP AT it_shop.
    AT END OF version.
      MOVE : it_shop-gubun   TO it_out-gubun,
             it_shop-shop    TO it_out-shop,
             it_shop-matnr   TO it_out-matnr,
             it_shop-version TO it_out-version.
      SUM.
      MOVE : it_shop-bwqty    TO it_out-bwqty,
             it_shop-ewqty    TO it_out-ewqty,
             it_shop-scrapqty TO it_out-scrapqty.
      APPEND it_out.
    ENDAT.
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
    MOVE it_shop-outqty TO it_wip-outqty.

    MODIFY it_out FROM it_out.
    CLEAR it_out.
  ENDLOOP.

ENDFORM.                    " READ_SHOP
*&---------------------------------------------------------------------*
*&      Form  READ_WORKCENTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_workcenter.
  DATA : BEGIN OF it_work OCCURS 0,
          gubun    LIKE ztco_wip-gubun,
          workct   LIKE ztco_wip-workct,
          matnr    LIKE ztco_wip-matnr,
          version  LIKE ztco_wip-version,
          bwqty    LIKE ztco_wip-bwqty,
          inqty    LIKE ztco_wip-inqty,
          outqty   LIKE ztco_wip-outqty,
          ewqty    LIKE ztco_wip-ewqty,
          scrapqty LIKE ztco_wip-scrapqty,
          rppoint  LIKE ztco_wip-rppoint,
         END OF it_work.
  DATA  :z_rp LIKE ztco_wip-rppoint.

  REFRESH it_work.

  SELECT gubun workct matnr version bwqty inqty outqty ewqty scrapqty
         rppoint
   INTO  (it_work-gubun,it_work-workct,it_work-matnr,it_work-version,
          it_work-bwqty,it_work-inqty, it_work-outqty,it_work-ewqty,
          it_work-scrapqty,it_work-rppoint)
      FROM ztco_wip
       WHERE plant EQ p_werks
         AND yymm EQ p_yymm
         AND gubun EQ 'W'.

    APPEND it_work.
  ENDSELECT.
*Collect
*Beginning WIP : Sum of beginning WIPS of the RPs in a shop
*Ending WIP : Sum of ending WIPS of the RPs in a shop
*Scrap : Sum of SCRAP QTY of the RPs in a shop
  SORT it_work BY workct matnr version.
  LOOP AT it_work.
    AT END OF version.
      MOVE : it_work-gubun    TO it_out-gubun,
             it_work-workct   TO it_out-workct,
             it_work-matnr    TO it_out-matnr,
             it_work-version  TO it_out-version.
      SUM.
      MOVE : it_work-bwqty    TO it_out-bwqty,
             it_work-ewqty    TO it_out-ewqty,
             it_work-scrapqty TO it_out-scrapqty.
      APPEND it_out.
    ENDAT.
  ENDLOOP.
*Input QTY : INPUT QTY of the first RP in a shop
*output QTY : OUTPUT QTY of the last RP in a shop
  LOOP AT it_out.
*input qty
    SELECT MIN( rppoint ) INTO z_rp  FROM ztco_wip
        WHERE  workct  EQ it_out-workct
          AND  matnr   EQ it_out-matnr
          AND  version EQ it_out-version.

    READ TABLE it_work WITH KEY workct  = it_out-workct
                                matnr   = it_out-matnr
                                version = it_out-version
                                rppoint = z_rp.
    MOVE it_work-inqty TO it_out-inqty.
*output qty
    CLEAR z_rp.
    SELECT MAX( rppoint ) INTO z_rp  FROM ztco_wip
        WHERE  workct  EQ it_out-workct
          AND  matnr   EQ it_out-matnr
          AND  version EQ it_out-version.

    READ TABLE it_work WITH KEY workct  = it_out-workct
                                matnr   = it_out-matnr
                                version = it_out-version
                                rppoint = z_rp.
    MOVE it_work-outqty TO it_wip-outqty.

    MODIFY it_out FROM it_out.
    CLEAR it_out.
  ENDLOOP.

ENDFORM.                    " READ_WORKCENTER
*&---------------------------------------------------------------------*
*&      Form  press_engin_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM press_engin_qty.
  DATA :c_arbpl LIKE crhd-arbpl.

  REFRESH it_afru. CLEAR :it_wip,c_arbpl.

  CLEAR c_yymm.
  CONCATENATE p_yymm '%' INTO c_yymm.

  SELECT  a~werks a~lmnga a~xmnga a~meinh a~stokz b~matnr
   INTO CORRESPONDING FIELDS OF TABLE it_afru
    FROM ( afru AS a INNER JOIN afpo AS b
      ON a~aufnr = b~aufnr )
     WHERE a~budat LIKE c_yymm.

  LOOP AT it_afru.
    PERFORM read_routing USING it_afru p_yymm
                         CHANGING c_arbpl.
    MOVE c_arbpl TO it_afru-arbpl.
    MODIFY it_afru FROM it_afru.
  ENDLOOP.


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
*       text
*----------------------------------------------------------------------*
*      -->P_IT_WIP_MATNR  text
*      -->P_IT_WIP_RPPOINT  text
*----------------------------------------------------------------------*
FORM get_shop_workcenter USING lt_wip LIKE it_wip.
*  DATA : f_sortb(10) TYPE c .
*  DATA : it_crhd LIKE crhd OCCURS 0 WITH HEADER LINE.
*  REFRESH it_crhd.
*  CLEAR f_sortb.

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
         l_dispo LIKE plaf-dispo,
         l_plnng LIKE mkal-plnng,
         l_alnag LIKE mkal-alnag.

  CLEAR : it_routing[],l_verid,l_dispo,l_plnnr,l_plnal.

  SELECT SINGLE dispo INTO l_dispo
    FROM marc
     WHERE matnr EQ it_wip-matnr
       AND werks EQ p_werks .

  SELECT SINGLE  plnng alnag INTO (l_plnnr,l_plnal)
       FROM mkal
        WHERE matnr EQ it_wip-matnr
          AND verid EQ it_wip-version
          AND werks EQ p_werks .

  SELECT   ma~matnr  mr~dispo pk~plnnr
           pk~plnal  pp~vornr ch~arbpl ch~sortb
    INTO CORRESPONDING FIELDS OF TABLE it_routing
      FROM ( ( ( ( ( ( mara AS ma
        INNER JOIN marc AS mr ON ma~matnr = mr~matnr )
        INNER JOIN mapl AS mp ON mr~matnr = mp~matnr )
        INNER JOIN plko AS pk ON mp~plnnr = pk~plnnr AND
                                 mp~plnal = pk~plnal )
        INNER JOIN plas AS pa ON mp~plnty = pa~plnty AND
                                 mp~plnnr = pa~plnnr AND
                                 mp~plnal = pa~plnal )
        INNER JOIN plpo AS pp ON pk~plnnr = pp~plnnr AND
                                 pa~plnty = pp~plnty AND
                                 pa~plnnr = pp~plnnr AND
                                 pa~plnkn = pp~plnkn AND
                                 pa~zaehl = pp~zaehl )
        INNER JOIN crhd AS ch ON pp~arbid = ch~objid )

      WHERE mp~plnty = 'R' AND
            mp~loekz = ' ' AND
            pa~loekz = ' ' AND
            ma~matnr EQ it_wip-matnr   AND
            mr~werks EQ p_werks  AND
            mr~dispo EQ l_dispo  AND
            pk~plnnr EQ l_plnnr  AND
            pk~plnal EQ l_plnal  AND
            pa~plnfl EQ '000000' AND
            ch~objty EQ 'A' AND
            ch~sortb EQ it_wip-rppoint.

  SORT it_routing BY matnr arbpl sortb
                      ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_routing
           COMPARING matnr arbpl sortb vornr.
  CLEAR w_int.
  DESCRIBE TABLE it_routing LINES w_int.
  IF w_int <> 0.
    LOOP AT it_routing.
      IF sy-tabix = 1.
        MOVE : "it_routing-arbpl(4) TO it_wip-shop,
               it_routing-arbpl TO lt_wip-workct.
        MODIFY it_wip FROM lt_wip.
      ELSE.
        MOVE : "it_routing-arbpl(4) TO it_wip-shop,
               it_routing-arbpl TO lt_wip-workct.
        APPEND lt_wip TO it_wip_temp2.
      ENDIF.
    ENDLOOP.
  ELSE.
    MODIFY it_wip FROM lt_wip.
  ENDIF.

*  SELECT SINGLE usr01 usr02 INTO (f_sortb,lt_wip-shop)
*   FROM zvpp_rp1
*   WHERE sortb EQ lt_wip-rppoint
*     AND plnnr   EQ 'RP'.
*
*
*  SELECT * INTO TABLE it_crhd
*     FROM crhd
*      WHERE sortb EQ f_sortb
*        AND verwe EQ '0007'
*        AND objty EQ 'A'.
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
*       text
*----------------------------------------------------------------------*
*      -->P_P_PREV  text
*----------------------------------------------------------------------*
FORM filled_previous_month USING p_prev.
  DATA wa_wip LIKE ztco_wip.

  CLEAR wa_wip.
  LOOP AT it_prev WHERE ewqty <> '0'.
    READ TABLE it_wip WITH KEY matnr   = it_prev-matnr
                               version = it_prev-version
                               rppoint = it_prev-rppoint.
    IF sy-subrc <> 0.
      SELECT  * INTO wa_wip FROM ztco_wip
                WHERE  matnr   = it_prev-matnr
                  AND  version = it_prev-version
                  AND  rppoint = it_prev-rppoint
                  AND  yymm    = p_prev
                  AND  gubun   = 'W'.

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
*&      Form  FSC_WIP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fsc_wip_data.

  CLEAR :it_ztco_fsc,it_ztco_wip.

  it_fsc_wip[] = it_ztco_wip[].
  SORT it_fsc_wip BY matnr version.
  DELETE ADJACENT DUPLICATES FROM it_fsc_wip COMPARING matnr version.

  REFRESH it_prev.
**previous month
*  PERFORM previous_month USING p_yymm
*                         CHANGING p_prev.
*Previous data
  SELECT matnr version ewqty rppoint
     INTO CORRESPONDING FIELDS OF TABLE it_prev
      FROM ztco_wip
        WHERE yymm  EQ p_prev
          AND gubun EQ 'F'.
*            plant EQ p_werks

  LOOP AT it_fsc_wip.
    CLEAR mara.
    SELECT SINGLE * FROM mara WHERE matnr EQ it_fsc_wip-matnr.
    CASE mara-mtart.
      WHEN 'FERT'.
        PERFORM fsc_calculation USING it_fsc_wip.
      WHEN 'HALB'.
        PERFORM halb_calculation USING it_fsc_wip.
    ENDCASE.
    CLEAR :it_fsc_wip,it_ztco_fsc.
  ENDLOOP.
*Update
  PERFORM update_ztco_wip_fsc.
ENDFORM.                    " FSC_WIP_DATA
*&---------------------------------------------------------------------*
*&      Form  FSC_CALCULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZTCO_WIP  text
*----------------------------------------------------------------------*
FORM fsc_calculation USING p_fsc_wip LIKE it_ztco_wip.

  DATA : l_first_rppoint LIKE ztco_wip-rppoint,
         l_last_rppoint LIKE ztco_wip-rppoint.


  MOVE-CORRESPONDING p_fsc_wip TO it_ztco_fsc.

*Begining qty
  READ TABLE it_prev WITH KEY matnr   = p_fsc_wip-matnr
                              version = p_fsc_wip-version.
  IF sy-subrc = 0.
    MOVE it_prev-ewqty     TO it_ztco_fsc-bwqty.
  ELSE.
    it_ztco_fsc-bwqty = 0.
  ENDIF.
*Input qty
  CLEAR : l_first_rppoint ,l_last_rppoint .

  LOOP AT it_ztco_wip WHERE matnr   = p_fsc_wip-matnr
                        AND version = p_fsc_wip-version.

*   'W' the first RP input => 'F' input
    IF l_first_rppoint IS INITIAL.
      l_first_rppoint      = it_ztco_wip-rppoint.
      it_ztco_fsc-inqty    = it_ztco_wip-inqty.
    ELSE.
      IF l_first_rppoint   < it_ztco_wip-rppoint.
        l_first_rppoint    = it_ztco_wip-rppoint.
        it_ztco_fsc-inqty  = it_ztco_wip-inqty.
      ENDIF.
    ENDIF.

*   'W' the last RP output => 'F' output
    IF l_last_rppoint IS INITIAL.
      l_last_rppoint       = it_ztco_wip-rppoint.
      it_ztco_fsc-outqty   = it_ztco_wip-outqty.
    ELSE.
      IF l_last_rppoint    > it_ztco_wip-rppoint.
        l_last_rppoint     = it_ztco_wip-rppoint.
        it_ztco_fsc-outqty = it_ztco_wip-outqty.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  READ TABLE it_ztco_wip WITH KEY matnr   = p_fsc_wip-matnr
*                                  version = p_fsc_wip-version
*                                  workct  = 'MXBXB1'.
*  IF sy-subrc = 0.
*    MOVE it_ztco_wip-inqty TO it_ztco_fsc-inqty.
*  ELSE.
*    it_ztco_fsc-inqty = 0.
*  ENDIF.
*Out qty
*  READ TABLE it_ztco_wip WITH KEY matnr   = p_fsc_wip-matnr
*                                  version = p_fsc_wip-version
*                                  workct  = 'MXTX53'.
*  IF sy-subrc = 0.
*    MOVE it_ztco_wip-outqty TO it_ztco_fsc-outqty.
*  ELSE.
*    it_ztco_fsc-outqty = 0.
*  ENDIF.



*Ending WIP
  it_ztco_fsc-ewqty = it_ztco_fsc-bwqty + it_ztco_fsc-inqty -
                      it_ztco_fsc-outqty - it_ztco_fsc-scrapqty.
*
  it_ztco_fsc-gubun = 'F'.
  it_ztco_fsc-shop  = ' '.
  it_ztco_fsc-workct = ' '.
  it_ztco_fsc-yymm  = p_yymm.
  it_ztco_fsc-erdat = sy-datum.
  it_ztco_fsc-erzet = sy-uzeit.
  it_ztco_fsc-ernam = sy-uname.
  APPEND it_ztco_fsc.CLEAR it_ztco_fsc.

ENDFORM.                    " FSC_CALCULATION
*&---------------------------------------------------------------------*
*&      Form  HALB_CALCULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FSC_WIP  text
*----------------------------------------------------------------------*
FORM halb_calculation USING  p_fsc_wip LIKE it_ztco_wip.

  MOVE-CORRESPONDING p_fsc_wip TO it_ztco_fsc.

*Begining qty
  READ TABLE it_prev WITH KEY matnr   = p_fsc_wip-matnr
                              version = p_fsc_wip-version.
  IF sy-subrc = 0.
    MOVE it_prev-ewqty     TO it_ztco_fsc-bwqty.
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
  it_ztco_fsc-yymm  = p_yymm.
  it_ztco_fsc-erdat = sy-datum.
  it_ztco_fsc-erzet = sy-uzeit.
  it_ztco_fsc-ernam = sy-uname.
  APPEND it_ztco_fsc.CLEAR it_ztco_fsc.

ENDFORM.                    " HALB_CALCULATION
*&---------------------------------------------------------------------*
*&      Form  UPDATE_zTCO_WIP_FSC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_ztco_wip_fsc.
  IF r_bct EQ 'X'.
*Delete Old data .
    DELETE FROM ztco_wip WHERE yymm EQ p_yymm
                           AND gubun EQ 'F'.
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
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_fsc.
  SELECT gubun rppoint matnr version bwqty inqty outqty ewqty scrapqty
   INTO  (it_out-gubun,it_out-rppoint,it_out-matnr,it_out-version,
          it_out-bwqty,it_out-inqty,it_out-outqty,it_out-ewqty,
          it_out-scrapqty)
      FROM ztco_wip
       WHERE plant EQ p_werks
         AND yymm EQ p_yymm
         AND gubun EQ 'F'.

    APPEND it_out.
  ENDSELECT.

ENDFORM.                    " READ_FSC
*&---------------------------------------------------------------------*
*&      Form  READ_ROUTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_AFRU  text
*----------------------------------------------------------------------*
FORM read_routing USING p_afru LIKE it_afru
                        l_yymm
                  CHANGING p_arbpl.

  DATA: BEGIN OF it_routing OCCURS 0,
          matnr    TYPE mara-matnr,
          ro_plnnr TYPE plko-plnnr,
          ro_plnal TYPE plko-plnal,
          ro_zaehl TYPE plko-zaehl,
          ro_verwe TYPE plko-verwe,
          ro_vornr TYPE plpo-vornr,
          ro_arbpl TYPE crhd-arbpl,
        END OF it_routing.
  DATA : from_date(8).
  REFRESH it_routing.CLEAR from_date.


  SELECT SINGLE * FROM mara
        WHERE matnr EQ p_afru-matnr.

  CHECK sy-subrc = 0.

  CONCATENATE l_yymm(4) l_yymm+4(2) '01' INTO from_date.
  SELECT ma~matnr pk~plnnr pk~plnal pk~zaehl pk~verwe
         pp~vornr ch~arbpl
      INTO TABLE it_routing
        FROM  ( ( ( ( ( ( mara AS ma
             INNER JOIN marc AS mr ON ma~matnr = mr~matnr )
             INNER JOIN mapl AS mp ON mr~matnr = mp~matnr )
             INNER JOIN plko AS pk ON mp~plnnr = pk~plnnr AND
                                      mp~plnal = pk~plnal )
             INNER JOIN plas AS pa ON mp~plnty = pa~plnty AND
                                      mp~plnnr = pa~plnnr AND
                                      mp~plnal = pa~plnal )
             INNER JOIN plpo AS pp ON pk~plnnr = pp~plnnr AND
                                      pa~plnty = pp~plnty AND
                                      pa~plnnr = pp~plnnr AND
                                      pa~plnkn = pp~plnkn AND
                                      pa~zaehl = pp~zaehl )
             INNER JOIN crhd AS ch ON pp~arbid = ch~objid )
          WHERE
*                  mp~plnty = mkal-plnty   AND
                mp~loekz = ' ' AND
                pa~loekz = ' ' AND
                ma~matnr EQ p_afru-matnr AND
                ma~mtart EQ mara-mtart   AND
*                mr~werks EQ p_werks      AND
                pk~verwe EQ '1'         AND
                pk~loekz NE 'X' AND
                pk~datuv <= from_date AND
                ch~objty = 'A' .

  SORT it_routing BY matnr ro_plnnr ro_zaehl DESCENDING.

  DELETE ADJACENT DUPLICATES FROM it_routing
         COMPARING matnr ro_plnnr ro_zaehl .
  READ TABLE it_routing INDEX 1.
  MOVE it_routing-ro_arbpl TO p_arbpl.

ENDFORM.                    " READ_ROUTING
