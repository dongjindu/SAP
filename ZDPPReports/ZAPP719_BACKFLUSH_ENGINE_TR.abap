************************************************************************
* Program Name      : ZAPP719_BACKFLUSH_ENGINE_TR
* Author            : Won-seob Kim
* Creation Date     : 2004.03.10.
* Specifications By :
* Pattern           : Report 1-1
* Development Request No :
* Addl Documentation:
* Description       : BackFlush ENGINE TRANSFER
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************

INCLUDE zapp719_bf_engine_top.

INCLUDE zapp_719_bf_engine_para.

INITIALIZATION.

START-OF-SELECTION.
*  PERFORM determine_date.
  PERFORM get_bf_objects.

END-OF-SELECTION.
  PERFORM write_list.
*&---------------------------------------------------------------------*
*&      Form  get_bf_objects
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bf_objects.
*Select data for engine transfer

*tuning - start
  DATA: BEGIN OF lt_ppc_head OCCURS 0,
           headid LIKE ppc_head-headid,
        END OF lt_ppc_head.
  REFRESH lt_ppc_head.
  MESSAGE s000(zmpp) WITH 'PPC_HEAD reading...'.
  SELECT headid INTO TABLE lt_ppc_head  "headid
            FROM ppc_head
*            INNER JOIN ppc_rp
*               ON ppc_rp~reppoint = ppc_head~reppoint
             UP TO 5000 ROWS
             WHERE ppc_head~flg_asynch EQ  ' '   "space.
*               AND ppc_rp~reppoint_ext EQ '11'.

** Furong on 11/26/11
 %_HINTS ORACLE 'INDEX("PPC_HEAD" "PPC_HEAD~ASY")'.
** End 11/26/11

  DESCRIBE TABLE lt_ppc_head LINES sy-tabix.
  CHECK sy-tabix > 0.

  MESSAGE s000(zmpp) WITH 'PPC_MAT reading...'.

** Changed by Furong on 04/19/13 - query by Engine group
*  SELECT MATNR PLANT LGORT PRVBE KOMP_QUANT POSTDATE
*       INTO CORRESPONDING FIELDS OF TABLE IT_PPC1_ALL
*            FROM PPC1_ALL
*            UP TO 2000 ROWS
**            INNER JOIN ppc_rp
**               ON ppc_rp~reppoint = ppc1_all~reppoint
*            FOR ALL ENTRIES IN LT_PPC_HEAD
*             WHERE HEADID = LT_PPC_HEAD-HEADID  "flg_asynch EQ  space
**               AND ppc_rp~reppoint_ext EQ '11'
*               AND PRVBE  = P_PRVBE.

  SELECT matnr plant lgort prvbe komp_quant postdate
       INTO CORRESPONDING FIELDS OF TABLE it_ppc1_all  UP TO 2000 ROWS
            FROM ppc1_all AS a
           INNER JOIN pgmi AS b
             ON a~matnr = b~nrmit
            FOR ALL ENTRIES IN lt_ppc_head
             WHERE headid = lt_ppc_head-headid  "flg_asynch EQ  space
               AND ( prgrp = 'MIP-ENG' OR prgrp = 'KD-ENG').
** End on 04/19/13

*  %_HINTS ORACLE 'FIRST_ROWS'.
  MESSAGE s000(zmpp) WITH 'PPC_MAT read complete'.

  DESCRIBE TABLE it_ppc1_all LINES w_int.
  IF w_int <> 0.
    PERFORM gathering_components TABLES it_ppc1_all.
  ENDIF.
ENDFORM.                    " get_bf_objects
*&---------------------------------------------------------------------*
*&      Form  gathering_components
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PPC1_ALL  text
*----------------------------------------------------------------------*
FORM gathering_components TABLES lp_ppc1_all STRUCTURE it_ppc1_all.
  DATA: l_werks LIKE marc-werks,
        l_sobsl LIKE marc-sobsl.
  l_werks = 'E%'.
*Find source plant from IMG table using SP type & plant from table of
*previous screens

** 03/15/12 - for source plant from E002 need to be consideration
*  CLEAR p_wrk02.
*  SELECT SINGLE wrk02 INTO p_wrk02
*         FROM t460a
*           WHERE werks EQ p_plant
*             AND sobsl EQ p_sobsl.
** end on 03/15/12

*read special procument  of material
  LOOP AT lp_ppc1_all INTO st_ppc1_all.
    SELECT SINGLE * FROM marc
        WHERE matnr EQ st_ppc1_all-matnr
          AND werks EQ st_ppc1_all-plant
** 03/19/12 - 42 need to be consideration for e002
*          AND SOBSL EQ P_SOBSL.
          AND ( sobsl = '40' OR sobsl = '42' ).
** end on 03/19/12

    IF sy-subrc NE 0.
      DELETE TABLE lp_ppc1_all FROM st_ppc1_all.
    ELSE.
** 03/15/12 - for source plant from E002 need to be consideration
*      SELECT SINGLE lgpro INTO st_ppc1_all-alort
*           FROM marc
*            WHERE werks EQ p_wrk02
*              AND matnr EQ st_ppc1_all-matnr.

** 05/02/13 - for getting source plant from t460a

*      SELECT SINGLE werks lgpro
*        INTO (st_ppc1_all-plant_s, st_ppc1_all-alort)
*              FROM marc
*               WHERE matnr EQ st_ppc1_all-matnr
*                 AND werks LIKE l_werks
*                 AND lvorm = ' '.*
      SELECT SINGLE sobsl INTO l_sobsl
         FROM marc
        WHERE werks = p_plant
          AND matnr =  st_ppc1_all-matnr.

      SELECT SINGLE wrk02 INTO st_ppc1_all-plant_s
              FROM t460a
               WHERE werks EQ p_plant
                     AND sobsl EQ l_sobsl.

**  Source storage location search.
      SELECT SINGLE lgpro INTO st_ppc1_all-alort
                FROM marc
                 WHERE werks EQ st_ppc1_all-plant_s
                   AND matnr EQ st_ppc1_all-matnr.

** End on 05/02/13
** end on 03/15/12
      MODIFY  lp_ppc1_all FROM st_ppc1_all.
    ENDIF.
  ENDLOOP.

*Collect quantity each material
  PERFORM collect_quantity TABLES lp_ppc1_all .
*stock transfer
  PERFORM stock_transfer TABLES lp_ppc1_all[]
                                it_quantity[].
** 03/15/12 - for source plant from E002 need to be consideration
*                         USING p_wrk02.
** end on 03/15/12

ENDFORM.                    " gathering_components
*&---------------------------------------------------------------------*
*&      Form  collect_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LP_PPC1_ALL  text
*----------------------------------------------------------------------*
FORM collect_quantity TABLES  ls_ppc1_all STRUCTURE it_ppc1_all.

  CLEAR : w_int,it_quantity[].
  DESCRIBE TABLE ls_ppc1_all LINES w_int.
  IF w_int <> 0.
*    SORT ls_ppc1_all BY matnr plant alort.
*    LOOP AT ls_ppc1_all.
*      AT END OF alort.    "Source :location
*        MOVE ls_ppc1_all-matnr     TO it_quantity-matnr.
*        MOVE ls_ppc1_all-plant     TO it_quantity-plant.
*        MOVE ls_ppc1_all-lgort     TO it_quantity-lgort.
*        MOVE ls_ppc1_all-alort     TO it_quantity-alort.
*        SUM.
*        MOVE ls_ppc1_all-komp_quant
*                               TO it_quantity-komp_quant.
*        APPEND it_quantity.
*      ENDAT.
*    ENDLOOP.

** on 03/15/12 new E002 plant
    SORT ls_ppc1_all BY matnr plant plant_s alort.
    LOOP AT ls_ppc1_all.
*     AT END OF PLANT_S.            "Source :plant
*        MOVE LS_PPC1_ALL-PLANT_S   TO IT_QUANTITY-PLANT_S.
*        MOVE LS_PPC1_ALL-MATNR     TO IT_QUANTITY-MATNR.
*        MOVE LS_PPC1_ALL-PLANT     TO IT_QUANTITY-PLANT.
*        MOVE LS_PPC1_ALL-LGORT     TO IT_QUANTITY-LGORT.
*        MOVE LS_PPC1_ALL-ALORT     TO IT_QUANTITY-ALORT.
*        SUM.
*        MOVE LS_PPC1_ALL-KOMP_QUANT
*                               TO IT_QUANTITY-KOMP_QUANT.
*        APPEND IT_QUANTITY.
*      ENDAT.
      AT END OF alort.            "Source :location
        MOVE ls_ppc1_all-plant_s   TO it_quantity-plant_s.
        MOVE ls_ppc1_all-matnr     TO it_quantity-matnr.
        MOVE ls_ppc1_all-plant     TO it_quantity-plant.
        MOVE ls_ppc1_all-lgort     TO it_quantity-lgort.
        MOVE ls_ppc1_all-alort     TO it_quantity-alort.
        SUM.
        MOVE ls_ppc1_all-komp_quant
                               TO it_quantity-komp_quant.
        APPEND it_quantity.
      ENDAT.

    ENDLOOP.
** End on 05/15/12

  ENDIF.
ENDFORM.                    " collect_quantity
*&---------------------------------------------------------------------*
*&      Form  stock_transfer
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_QUANTITY  text
*----------------------------------------------------------------------*
FORM stock_transfer TABLES
                       ls_ppc1_all STRUCTURE it_ppc1_all
                       lp_quantity STRUCTURE  it_quantity.
*                    USING p_wrk02.
  DATA :z_labst LIKE mard-labst,  "engine plant stock
        g_labst LIKE mard-labst,  "assembly plant stock
        f_labst LIKE mard-labst,
        t_quantity LIKE it_quantity-komp_quant,
        g_psting_date LIKE bapi2017_gm_head_01.


  DATA : it_item LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE,
         return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

*Currently engine stock check "E001, E110
  LOOP AT lp_quantity.
    CLEAR : z_labst,g_labst,f_labst,t_quantity,it_item[].


** on 03/15/12 for E002
*    SELECT SINGLE labst INTO z_labst
*           FROM mard
*              WHERE matnr EQ lp_quantity-matnr
*                AND werks EQ p_wrk02
*                AND lgort EQ lp_quantity-alort.
    SELECT SINGLE labst INTO z_labst
           FROM mard
              WHERE matnr EQ lp_quantity-matnr
                AND werks EQ lp_quantity-plant_s
                AND lgort EQ lp_quantity-alort.

** End on 03/15/12

*Currently assembly stock check "P001, P500
    SELECT SINGLE labst INTO g_labst
           FROM mard
              WHERE matnr EQ lp_quantity-matnr
                AND werks EQ lp_quantity-plant
                AND lgort EQ lp_quantity-lgort.
*If transfered stock is grater than  assembly stock
    IF lp_quantity-komp_quant > g_labst.
      f_labst =  lp_quantity-komp_quant - g_labst.

      IF f_labst > z_labst.
        it_item-entry_qnt = z_labst.
      ELSE.
        it_item-entry_qnt = f_labst.
      ENDIF.

*read posting date
      READ TABLE ls_ppc1_all INDEX 1.
      g_psting_date-pstng_date = ls_ppc1_all-postdate.
      g_psting_date-doc_date = ls_ppc1_all-postdate.

*move item information
      MOVE lp_quantity-matnr      TO it_item-material.
** On 03/15/12 for E002
*      MOVE p_wrk02                TO it_item-plant.     " source plant
      MOVE lp_quantity-plant_s    TO it_item-plant.     " source plant
** End on 03/15/12
      MOVE lp_quantity-alort      TO it_item-stge_loc.  " source stl
      MOVE p_mtype                TO it_item-move_type. "
      MOVE 'EA'                   TO it_item-entry_uom.
      MOVE lp_quantity-plant      TO it_item-move_plant.
      MOVE lp_quantity-lgort      TO it_item-move_stloc.
      APPEND it_item.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
         goodsmvt_header            = g_psting_date
         goodsmvt_code              = gm_code
*    TESTRUN                     = ' '
*  IMPORTING
*    GOODSMVT_HEADRET            =
*    MATERIALDOCUMENT            =
*    MATDOCUMENTYEAR             =
        TABLES
          goodsmvt_item               = it_item
*    GOODSMVT_SERIALNUMBER       =
          return                      = return.

      READ TABLE return WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        MOVE return-type    TO lp_quantity-type.
        MOVE return-message TO lp_quantity-text.
        MODIFY lp_quantity FROM lp_quantity.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        MOVE 'S'       TO lp_quantity-type.
        MOVE 'GOOD!!!' TO lp_quantity-text.
        MODIFY lp_quantity FROM lp_quantity.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " stock_transfer
*&---------------------------------------------------------------------*
*&      Form  write_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_list.
*Call job : PPCGO
  IF p_a EQ 'X'.
    PERFORM call_ppcgo_backjob.
  ENDIF.
*Write log
  PERFORM write_log.

ENDFORM.                    " write_list
*&---------------------------------------------------------------------*
*&      Form  WRITE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_log.
  DATA : wa_itab LIKE itab WITH HEADER LINE .
  CLEAR : wa_itab,w_int.
  DESCRIBE TABLE it_quantity LINES w_int.
  IF w_int <> 0.
    LOOP AT it_quantity.
      WRITE : / it_quantity-matnr, it_quantity-plant,it_quantity-lgort,
                it_quantity-alort, it_quantity-komp_quant UNIT 'EA',
                it_quantity-type,  it_quantity-text.
    ENDLOOP.
  ELSE.
    WRITE : / text-001.
  ENDIF.
  LOOP AT itab INTO wa_itab.
    WRITE : / wa_itab-tcode, wa_itab-msgtyp.
  ENDLOOP.
ENDFORM.                    " WRITE_LOG
*&---------------------------------------------------------------------*
*&      Form  call_ppcgo_backjob
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_ppcgo_backjob.
  DATA: bdcdata TYPE TABLE OF bdcdata.
  DATA: program LIKE sy-repid,
        p_mode TYPE c VALUE 'N',
        wa_bdcdata TYPE bdcdata.
  DATA: ctu_params LIKE ctu_params.

  CLEAR :wa_bdcdata, bdcdata[],ctu_params.

  wa_bdcdata-program  = 'PPC_CONF_GO'.
  wa_bdcdata-dynpro   = '1000'.
  wa_bdcdata-dynbegin = 'X'.
  APPEND wa_bdcdata TO bdcdata.

  CLEAR wa_bdcdata.
  wa_bdcdata-fnam = 'BDC_OKCODE'.
  wa_bdcdata-fval = '=ONLI'.
  APPEND wa_bdcdata TO bdcdata.

  CLEAR wa_bdcdata.
  wa_bdcdata-fnam = 'P_PARA_F'.
  wa_bdcdata-fval = 'X'.
  APPEND wa_bdcdata TO bdcdata.

  CLEAR wa_bdcdata.
  wa_bdcdata-fnam = 'P_SERVGR'.
  wa_bdcdata-fval = 'PG_BF'.
  APPEND wa_bdcdata TO bdcdata.

  CLEAR wa_bdcdata.
  wa_bdcdata-fnam = 'P_WPS_RE'.
  wa_bdcdata-fval = '10'.
  APPEND wa_bdcdata TO bdcdata.

  CLEAR wa_bdcdata.
  wa_bdcdata-fnam = 'SO_DATE-LOW'.
  wa_bdcdata-fval = ' '.
  APPEND wa_bdcdata TO bdcdata.

  wa_bdcdata-fnam = 'SO_USER-LOW'.
  wa_bdcdata-fval = ' '.
  APPEND wa_bdcdata TO bdcdata.

  wa_bdcdata-fnam = 'P_SYNC'.
  wa_bdcdata-fval = 'X'.
  APPEND wa_bdcdata TO bdcdata.

  ctu_params-dismode  = 'N'.
  ctu_params-updmode  = 'S'.
  ctu_params-cattmode = ' '.
  ctu_params-defsize  = ' '.
  ctu_params-racommit = 'X'.
  ctu_params-nobinpt  = 'X'.
  ctu_params-nobiend  = 'X'.

  CALL TRANSACTION 'PPCGO'  USING bdcdata
                            OPTIONS FROM ctu_params
                            MESSAGES INTO itab.

ENDFORM.                    " call_ppcgo_backjob
*&---------------------------------------------------------------------*
*&      Form  determine_date
*&---------------------------------------------------------------------*
FORM determine_date.
  DATA: l_datum LIKE sy-datum.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date             = sy-datum
    IMPORTING
      ev_month_begin_date = l_datum.


  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = l_datum
      days      = '00'
      months    = '01'
      signum    = '-'
      years     = '00'
    IMPORTING
      calc_date = gv_bdate.

ENDFORM.                    " determine_date
