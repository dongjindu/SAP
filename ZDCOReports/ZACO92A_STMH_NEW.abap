*&--------------------------------------------------------------------
*& REPORT                 : ZACO92A_STMH
*& Author                 : WSKIM
*& Creation Date          : 01/12/2005
*& Specification By       :
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description            : Standard M/H each MIP Material FSC
*& Modification Log
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------
REPORT zaco92a_stmh MESSAGE-ID  zmco.
*Tabls definition
TABLES : ztco_shopcost,ztco_stmh,ztco_stmh_at.


*Internal table definition
DATA : BEGIN OF it_shopcost OCCURS 0,
         level,
         kokrs     LIKE ztco_shopcost-kokrs,
         bdatj     LIKE ztco_shopcost-bdatj,
         klvar     LIKE ztco_shopcost-klvar,
         fsc_matnr LIKE ztco_shopcost-fsc_matnr,
         shop      LIKE ztco_shopcost-shop,
         llv_matnr LIKE ztco_shopcost-llv_matnr,
         kstar     LIKE ztco_shopcost-kstar,
         kostl     LIKE ztco_shopcost-kostl,
         lstar     LIKE ztco_shopcost-lstar,
         menge     LIKE ztco_stmh-menge,
         elemt     LIKE ztco_shopcost-elemt,
         hwaer     LIKE ztco_shopcost-hwaer,
         meeht     LIKE ztco_shopcost-meeht,
         poper     LIKE ztco_stmh-poper,
        END OF it_shopcost.

DATA : BEGIN OF it_temp_st OCCURS 0.
        INCLUDE STRUCTURE it_shopcost.
DATA:   END OF it_temp_st.
DATA : it_st_source LIKE it_shopcost OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_shopcost_at OCCURS 0,
         kokrs        LIKE ztco_shopcost_at-kokrs,
         bdatj        LIKE ztco_shopcost_at-bdatj,
         aufnr        LIKE ztco_shopcost_at-aufnr,
         fsc_matnr    LIKE ztco_shopcost_at-fsc_matnr,
         shop         LIKE ztco_shopcost_at-shop,
         llv_matnr    LIKE ztco_shopcost_at-llv_matnr,
         kstar        LIKE ztco_shopcost_at-kstar,
         kostl        LIKE ztco_shopcost_at-kostl,
         lstar        LIKE ztco_shopcost_at-lstar,
         mbgbtr       LIKE ztco_shopcost_at-mbgbtr,
         add_mbgbtr   LIKE ztco_shopcost_at-add_mbgbtr,
         wip_quantity LIKE ztco_shopcost_at-wip_quantity,
         actual_scrap LIKE ztco_shopcost_at-actual_scrap,
         manu_qty     LIKE ztco_shopcost_at-manu_qty,
         gr_qty       LIKE ztco_stmh_at-gr_qty,
         elemt        LIKE ztco_shopcost_at-elemt,
         poper        LIKE ztco_stmh_at-poper,
       END OF it_shopcost_at.

DATA :BEGIN OF it_temp OCCURS 0,
         level,
         aufnr        LIKE ztco_shopcost_at-aufnr,
         fsc_matnr    LIKE ztco_shopcost_at-fsc_matnr,
         shop         LIKE ztco_shopcost_at-shop,
         llv_matnr    LIKE ztco_shopcost_at-llv_matnr,
         kstar        LIKE ztco_shopcost_at-kstar,
         kostl        LIKE ztco_shopcost_at-kostl,
         lstar        LIKE ztco_shopcost_at-lstar,
         mbgbtr       LIKE ztco_shopcost_at-mbgbtr,
         add_mbgbtr   LIKE ztco_shopcost_at-add_mbgbtr,
         wip_quantity LIKE ztco_shopcost_at-wip_quantity,
         actual_scrap LIKE ztco_shopcost_at-actual_scrap,
         manu_qty     LIKE ztco_shopcost_at-manu_qty,
         gr_qty       LIKE ztco_stmh_at-gr_qty,
      END OF it_temp.
DATA : it_collect LIKE it_temp OCCURS 0 WITH HEADER LINE.
DATA : it_collect_temp LIKE it_collect OCCURS 0 WITH HEADER LINE.
DATA : it_calcu LIKE it_collect OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_wip OCCURS 0,
        matnr    LIKE ztco_wip-matnr,
        workct   LIKE ztco_wip-workct,
        bwqty    LIKE ztco_wip-bwqty,
        inqty    LIKE ztco_wip-inqty,
        outqty   LIKE ztco_wip-outqty,
        ewqty    LIKE ztco_wip-ewqty,
        scrapqty LIKE ztco_wip-scrapqty,
       END OF it_wip.
DATA : it_fsc LIKE it_wip OCCURS 0 WITH HEADER LINE.
DATA : it_ztco_stmh_at LIKE ztco_stmh_at OCCURS 0 WITH HEADER LINE.
DATA : it_ztco_stmh LIKE ztco_stmh OCCURS 0 WITH HEADER LINE.

data : begin of it_ckmlmv003 occurs 0,
         matnr      like ckmlmv001-matnr,
         bwkey      like ckmlmv001-bwkey,
         out_menge  like ckmlmv003-out_menge,
       end of  it_ckmlmv003.

*Data definition
DATA : p_curtp LIKE covja-kwaer VALUE '10'.
DATA : it_out LIKE it_shopcost OCCURS 0 WITH HEADER LINE.
DATA : w_int TYPE i.
DATA : p_klvar LIKE cki64a-klvar.
DATA : f_save .
DATA : p_versn LIKE rkpln-versn VALUE '0',
       p_tvers LIKE keko-tvers  VALUE '01',
       p_elehk LIKE tckh4-elehk VALUE 'H1',
       p_werks LIKE pbim-werks  VALUE 'P001'.
*Constants
CONSTANTS :cons1 LIKE ztco_shopcost-kstar VALUE '0000836001',
           cons2 LIKE ztco_shopcost-kstar VALUE '0000540300'.
RANGES r_level FOR it_shopcost-level.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
* General Info.
PARAMETERS : p_kokrs LIKE csks-kokrs   MEMORY ID cac  OBLIGATORY
               DEFAULT 'H201'.
SELECTION-SCREEN SKIP 1.
* Posted Yr.
PARAMETERS : p_bdatj LIKE keko-bdatj MEMORY ID bdtj OBLIGATORY.
*             DEFAULT '2004'.
* periods
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(30) text-021. "From
SELECTION-SCREEN POSITION 33.
PARAMETERS: p_poper LIKE covja-perab MEMORY ID vpe
            MODIF ID per OBLIGATORY ."DEFAULT '10'.
SELECTION-SCREEN END OF LINE.
* Costing Type
SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(10)  text-031. "Business Plan
SELECTION-SCREEN POSITION 12.
PARAMETERS : p_bpl             RADIOBUTTON GROUP ra01
             USER-COMMAND  cty.
SELECTION-SCREEN COMMENT  20(10) text-032. "Standard Plan
SELECTION-SCREEN POSITION 32.
PARAMETERS : p_std DEFAULT 'X' RADIOBUTTON GROUP ra01.
*SELECTION-SCREEN COMMENT  40(10) text-033. "Actual
*SELECTION-SCREEN POSITION 52.
*PARAMETERS : p_act DEFAULT 'X' RADIOBUTTON GROUP ra01.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bl2.
SELECTION-SCREEN END OF BLOCK bl1.

*INCLUDE zaco11r_ml02_f01.
*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  p_bdatj = sy-datum(4).
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM period_check.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_out.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_std EQ 'X'.
*Read : Material ledger (GR qty)
    PERFORM read_zcor07.
*Read Standard :ZTCO_SHOPCOST
    PERFORM read_ztco_shopcost_standard.
    PERFORM update_ztco_stmh.
*Read Wip data (WIP)
    PERFORM read_wip.
*Read WIP data (FSC)
    PERFORM read_wip_fsc.
*Read Actual : ZTCO_SHOPCOST_AT
    PERFORM read_ztco_shopcost_at.
    PERFORM update_ztco_stmh_at.
*Annual Business
  ELSEIF p_bpl EQ 'X'.
*Read :ZTCO_SHOPCOST
    PERFORM read_ztco_shopcost_business.
    PERFORM update_ztco_stmh.
  ENDIF.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  read_zcor07
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_zcor07.

  PERFORM progress_indicator USING  '10' ' '
                   'Read Material leadger data'.

** Read MLCD
*  PERFORM read_mlcd.
** Merge to IT_MLCD
*  PERFORM merge_mlcd_data.
***// Mod. By Hyung Jin Youn 2004.07.01
** Read Valuation Class. (from IT_MACKU).
*  PERFORM read_val_class.
***// End of Mod.
***// Mod. By Hyung Jin Youn 2004.07.02
** Data in MLCD could not be generated if any transaction
** is executied at specific period
** Set Beginning Inv. from Ending Inv. data of previous period.
*  PERFORM set_beginning_data_for_no_tr.
***// End of Mod.
** Not Distributed
*  PERFORM cal_not_dis.


*Extract material and quantity
  PERFORM extract_material.
ENDFORM.                    " read_zcor07
*&---------------------------------------------------------------------*
*&      Form  extract_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM extract_material.

  SELECT  a~bwkey a~matnr   "a~verid_nd
          sum( b~out_menge )
    INTO table it_ckmlmv003
    FROM ckmlmv001 AS a
   INNER JOIN ckmlmv003 AS b
      ON a~kalnr    =  b~kalnr_bal
   WHERE b~gjahr    =  P_BDATJ
     AND b~perio    =  P_POPER
*     AND a~bwkey    =  it_proc_gr-par_werks
*     AND a~matnr    =  it_proc_gr-artnr
*     AND a~verid_nd =  it_proc_gr-verid
     and a~BTYP     = 'BF'
     and out_menge > 0
   GROUP BY a~matnr a~bwkey.

ENDFORM.                    " extract_material
*&---------------------------------------------------------------------*
*&      Form  read_ztco_shopcost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_ztco_shopcost_standard.

  PERFORM progress_indicator USING  '30' ' '
                   'Read Standard data'.

*Gathering data from ztco_shopcost_at
  PERFORM gathering_data_shopcost USING 'PPC1'.
*Define level SHOPCOST
  PERFORM define_level_shopcost.
*Calculate quantity
  PERFORM calculate_quantity_st.

ENDFORM.                    " read_ztco_shopcost
*&---------------------------------------------------------------------*
*&      Form  update_ztco_stmh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_ztco_stmh.
  REFRESH it_ztco_stmh.
  PERFORM progress_indicator USING  '70' ' '
                 'UPdating'.

*Delete Old data .
  IF p_std EQ 'X'.
    DELETE FROM ztco_stmh WHERE kokrs EQ p_kokrs
                            AND bdatj EQ p_bdatj
                            AND poper EQ p_poper
                            AND klvar EQ 'PPC1'.
  ELSEIF p_bpl EQ 'X'.
    DELETE FROM ztco_stmh WHERE kokrs EQ p_kokrs
                            AND bdatj EQ p_bdatj
                            AND poper EQ p_poper
                            AND klvar EQ 'ZPCP'.

  ENDIF.

  CLEAR: it_out,f_save.REFRESH it_ztco_stmh.


  LOOP AT it_out.
    MOVE-CORRESPONDING it_out TO it_ztco_stmh.
    it_ztco_stmh-erdat = sy-datum.
    it_ztco_stmh-erzet = sy-uzeit.
    it_ztco_stmh-ernam = sy-uname.
    COLLECT it_ztco_stmh.CLEAR it_ztco_stmh.
  ENDLOOP.
**fill up GR_QTY
*  PERFORM ml_qty_filled.
  IF p_std EQ 'X'.
    LOOP AT it_ztco_stmh.
      READ TABLE it_fsc WITH KEY matnr = it_ztco_stmh-fsc_matnr.
      IF sy-subrc = 0.
        MOVE it_fsc-outqty TO it_ztco_stmh-gr_qty.

        MODIFY it_ztco_stmh FROM it_ztco_stmh.
      ENDIF.
    ENDLOOP.
  ENDIF.
*Get QTY OF PIR
  IF p_bpl EQ 'X'.
    PERFORM get_pir_qty TABLES it_ztco_stmh.
  ENDIF.

  LOOP AT it_ztco_stmh.
  it_ztco_stmh-menget = it_ztco_stmh-menge * it_ztco_stmh-gr_qty.
    MODIFY it_ztco_stmh FROM it_ztco_stmh.
  ENDLOOP.

  INSERT ztco_stmh FROM TABLE it_ztco_stmh ."ACCEPTING DUPLICATE KEYS .

  IF sy-subrc = 0.
    MESSAGE s009 WITH 'ZTCO_STMH'.
    COMMIT WORK.
  ELSE.
    MESSAGE s045  WITH 'ZTCO_STMH'.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " update_ztco_stmh
*&---------------------------------------------------------------------*
*&      Form  read_ztco_shopcost_business
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_ztco_shopcost_business.

  PERFORM progress_indicator USING  '30' ' '
                   'Read Business data'.

*Gathering data from ztco_shopcost_at
  PERFORM gathering_data_shopcost USING 'ZPCP'.
*Define level SHOPCOST
  PERFORM define_level_shopcost.
*Calculate quantity
  PERFORM calculate_quantity_st.
ENDFORM.                    " read_ztco_shopcost_business
*&---------------------------------------------------------------------*
*&      Form  read_ztco_shopcost_at
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_ztco_shopcost_at.

  PERFORM progress_indicator USING  '50' ' '
                   'Read Actual data'.

*Gathering data from ztco_shopcost_at
  PERFORM gathering_data_at.
*Define level
  PERFORM define_level.
*Calculate quantity
  PERFORM calculate_quantity.

ENDFORM.                    " read_ztco_shopcost_at
*&---------------------------------------------------------------------*
*&      Form  gathering_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gathering_data_at.
  CLEAR : w_int.
  REFRESH it_shopcost_at.
  SELECT kokrs bdatj aufnr fsc_matnr shop llv_matnr kstar elemt
         kostl lstar mbgbtr add_mbgbtr wip_quantity actual_scrap
         manu_qty poper
         INTO CORRESPONDING FIELDS OF TABLE it_shopcost_at
          FROM ztco_shopcost_at
            FOR ALL ENTRIES IN it_ckmlmv003
           WHERE kokrs EQ p_kokrs
             AND bdatj EQ p_bdatj
             AND poper EQ p_poper
             AND typps IN ('M','V', 'E')
              AND elemt EQ '070'
             AND fsc_matnr EQ it_ckmlmv003-matnr.

  LOOP AT it_shopcost_at.
    IF it_shopcost_at-kstar EQ '0000836001' OR
       it_shopcost_at-kstar EQ '0000540300'.
      IF it_shopcost_at-shop EQ space.
        DELETE TABLE it_shopcost_at FROM it_shopcost_at.
      ELSE.
        CONTINUE.
      ENDIF.
    ELSE.
      DELETE TABLE it_shopcost_at FROM it_shopcost_at.
    ENDIF.
  ENDLOOP.

  SORT it_shopcost_at BY  kokrs bdatj aufnr fsc_matnr
                       shop llv_matnr kstar kostl lstar.

  DESCRIBE TABLE it_shopcost_at LINES w_int.
  IF w_int = 0.
    MESSAGE i000 WITH 'No exist actual data in ztco_shopcost_at'.
    STOP.
  ENDIF.

*Collect by aufnr
  SORT it_shopcost_at BY  kokrs bdatj aufnr fsc_matnr
                        shop llv_matnr kstar kostl lstar.

  REFRESH it_temp.

  LOOP AT it_shopcost_at.
    MOVE-CORRESPONDING it_shopcost_at TO it_temp.
    APPEND it_temp.
  ENDLOOP.

  LOOP AT it_temp.
    MOVE-CORRESPONDING it_temp TO it_collect.
    COLLECT  it_collect.
  ENDLOOP.

  LOOP AT it_collect.
    READ TABLE it_fsc WITH KEY matnr = it_collect-fsc_matnr.
    IF sy-subrc = 0.
      MOVE it_fsc-outqty TO it_collect-gr_qty.
    ELSE.
      it_collect-gr_qty = 0.
    ENDIF.
    MODIFY it_collect FROM it_collect.
  ENDLOOP.

ENDFORM.                    " gathering_data
*&---------------------------------------------------------------------*
*&      Form  define_level
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM define_level.
  DATA : c_level.
  DATA : f_level,s_level.
  CLEAR : it_collect, c_level,f_level,s_level.
  REFRESH it_collect_temp[].

  it_collect_temp[] = it_collect[].
  f_level = 1.  s_level = 2.
*1 Level
  LOOP AT it_collect.
    READ TABLE it_collect_temp WITH KEY
                                   llv_matnr = it_collect-fsc_matnr.
    IF sy-subrc <> 0.
      c_level = f_level.
      it_collect-level = c_level.
    ELSE.
      c_level = s_level.
      it_collect-level = c_level.
    ENDIF.
    MODIFY  it_collect FROM it_collect.
  ENDLOOP.

  REFRESH it_collect_temp.
  it_collect_temp[] = it_collect[].

  DO 8 TIMES.
    f_level = f_level + 1.
    s_level = s_level + 1.
*Sub Level
    PERFORM define_level_detail USING f_level s_level.
  ENDDO.

ENDFORM.                    " define_level
*&---------------------------------------------------------------------*
*&      Form  define_level_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0847   text
*      -->P_0848   text
*----------------------------------------------------------------------*
FORM define_level_detail USING  p_l1 p_l2.
  DATA : f_matnr LIKE it_collect-llv_matnr.
  CLEAR f_matnr.
  LOOP AT it_collect WHERE level EQ p_l1.
    READ TABLE it_collect_temp WITH KEY
                                   level = p_l1
                                   llv_matnr = it_collect-fsc_matnr.
    IF sy-subrc  <> 0.
      it_collect-level = p_l1.
    ELSE.
      it_collect-level = p_l2.
    ENDIF.

    MODIFY  it_collect FROM it_collect.
  ENDLOOP.

ENDFORM.                    " define_level_detail
*&---------------------------------------------------------------------*
*&      Form  supplement_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM supplement_actual.
  DATA : f_poper LIKE ztco_stmh_at-poper,
         f_date LIKE sy-datum,
         t_date LIKE sy-datum,
         c_month LIKE t5a4a-dlymo.
  CLEAR :it_collect,f_poper.

  LOOP AT it_collect WHERE gr_qty = 0.
*    READ TABLE it_zcor07 WITH KEY matnr = it_collect-fsc_matnr.
*    IF sy-subrc <> 0.
*      MOVE it_zcor07-zu_lbkum TO it_collect-gr_qty.
*    ELSE.
* IF material has not actual quantity, must read a previous quantity
*    from  table 'ztco_stmh_at'

    DO 12 TIMES.
      CONCATENATE p_bdatj p_poper '01' INTO f_date.
      c_month = c_month + 1.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
           EXPORTING
                date      = f_date
                days      = '00'
                months    = c_month
                signum    = '-'
                years     = ' '
           IMPORTING
                calc_date = t_date.

      f_poper = t_date+4(2).

      SELECT SINGLE gr_qty INTO it_collect-gr_qty
         FROM ztco_stmh_at
          WHERE kokrs EQ p_kokrs
            AND bdatj EQ p_bdatj
            AND poper EQ f_poper
            AND fsc_matnr EQ it_collect-fsc_matnr.
      IF it_collect-gr_qty NE space.
        EXIT.
      ENDIF.
    ENDDO.
*    ENDIF.
    MODIFY  it_collect FROM it_collect.
  ENDLOOP.
ENDFORM.                    " supplement_actual
*&---------------------------------------------------------------------*
*&      Form  calculate_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_quantity.
*Supplement Actual Quantity
  PERFORM supplement_actual.
*Calculate quantity
  PERFORM calculate_quantity_item.

ENDFORM.                    " calculate_quantity
*&---------------------------------------------------------------------*
*&      Form  calculate_quantity_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_quantity_item.
*
  DATA : last_flag(1),c_flag,p_flag,
         f_manu_qty LIKE it_collect-manu_qty,
         f_gr_qty LIKE it_collect-gr_qty,
         f_llv_matnr LIKE it_shopcost-llv_matnr,
         it_cal_temp LIKE it_calcu.

  CLEAR :last_flag,c_flag,p_flag,
         f_manu_qty,f_gr_qty.

  SORT it_collect BY level DESCENDING.

  REFRESH it_collect_temp.
  it_collect_temp[] = it_collect[].

  READ TABLE it_collect INDEX  1.
  last_flag = it_collect-level.
  c_flag = last_flag.
  DO .
    IF c_flag = last_flag.
      LOOP AT it_collect WHERE level EQ c_flag.
        IF it_collect-llv_matnr EQ space.
          MOVE-CORRESPONDING it_collect TO it_calcu.
          MOVE c_flag                   TO it_calcu-level.
          APPEND it_calcu.CLEAR it_calcu.
        ELSE.
          READ TABLE it_collect_temp WITH KEY fsc_matnr =
                                          it_collect-llv_matnr.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING it_collect TO it_calcu.
            MOVE c_flag                   TO it_calcu-level.
            APPEND it_calcu.CLEAR it_calcu.
          ELSE.
*            PERFORM get_last_month USING c_flag it_collect-llv_matnr.
            PERFORM get_last_month_data2 USING it_collect-llv_matnr
                                               c_flag  it_collect.
          ENDIF.
        ENDIF.
      ENDLOOP.
      c_flag = c_flag - 1.
      CHECK c_flag = '0'.
      EXIT.
    ELSE.
      LOOP AT it_collect WHERE level EQ c_flag.
        IF it_collect-llv_matnr EQ space AND it_collect-kstar EQ cons1.
          MOVE-CORRESPONDING it_collect TO it_calcu.
          MOVE c_flag                   TO it_calcu-level.
          APPEND it_calcu.CLEAR it_calcu.
        ELSE.
          p_flag = c_flag + 1.

          MOVE-CORRESPONDING it_collect TO it_cal_temp.
          MOVE it_collect-llv_matnr TO f_llv_matnr.
          READ TABLE it_collect_temp WITH KEY fsc_matnr =
                                              f_llv_matnr.
          IF sy-subrc = 0.

            LOOP AT it_collect_temp WHERE fsc_matnr = f_llv_matnr
                                      AND level     EQ p_flag.
              MOVE-CORRESPONDING it_cal_temp TO it_calcu.
              IF it_collect_temp-llv_matnr EQ space ."AND
*               it_collect_temp-kstar EQ cons1.
                MOVE : it_collect_temp-fsc_matnr TO it_calcu-llv_matnr,
                       c_flag                    TO it_calcu-level,
                       cons1                     TO it_calcu-kstar.
              ELSE.
                MOVE : it_collect_temp-llv_matnr TO it_calcu-llv_matnr,
                       c_flag                    TO it_calcu-level,
                       cons1                     TO it_calcu-kstar.
              ENDIF.
              MOVE : it_collect_temp-kostl       TO it_calcu-kostl,
                     it_collect_temp-lstar       TO it_calcu-lstar.

*Calculate
              MOVE : it_collect_temp-manu_qty TO f_manu_qty,
                     it_collect_temp-gr_qty   TO f_gr_qty.
              IF f_gr_qty EQ 0.
                f_gr_qty = 1.
              ENDIF.
*Actual Manufactur QTY =
* ( below level actual manufacture qty / actual qty ) * each leve
*                                     quantity
*Current QTY
              it_calcu-mbgbtr = ( f_manu_qty / f_gr_qty ) *
                                  it_collect-mbgbtr.
*Additional issue QTY
              it_calcu-add_mbgbtr = ( f_manu_qty / f_gr_qty ) *
                                     it_collect-add_mbgbtr.
*WIP QTY
              it_calcu-wip_quantity = ( f_manu_qty / f_gr_qty ) *
                                        it_collect-wip_quantity.
*Scrap QTY
              it_calcu-actual_scrap = ( f_manu_qty / f_gr_qty ) *
                                        it_collect-actual_scrap.
*Actual Manufacture QTY
              it_calcu-manu_qty = ( f_manu_qty / f_gr_qty ) *
                                    it_collect-manu_qty.
              APPEND it_calcu.
              CLEAR : it_calcu,f_manu_qty,f_gr_qty.
            ENDLOOP.
          ELSE.
            PERFORM get_last_month_data2 USING f_llv_matnr c_flag
                                               it_collect.
          ENDIF.
        ENDIF.
        CLEAR  it_collect.
      ENDLOOP.

      IF c_flag EQ '1'.
        EXIT.
      ENDIF.
      c_flag = c_flag - 1.
    ENDIF.
    REFRESH it_collect_temp.
    it_collect_temp[] = it_calcu[].
  ENDDO.

ENDFORM.                    " calculate_quantity_item
*&---------------------------------------------------------------------*
*&      Form  update_ztco_stmh_at
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_ztco_stmh_at.
  CLEAR :it_calcu,f_save.
  IF p_std EQ 'X'.
    DELETE FROM ztco_stmh_at WHERE kokrs EQ p_kokrs
                               AND bdatj EQ p_bdatj
                               AND poper EQ p_poper
                               AND gubun EQ 'A'.
  ENDIF.

  REFRESH it_ztco_stmh_at.CLEAR it_calcu.
  SORT  it_calcu BY aufnr  fsc_matnr shop llv_matnr kostl lstar .

  LOOP AT it_calcu.
    CLEAR: it_ztco_stmh_at.

    READ TABLE it_ztco_stmh_at WITH KEY kokrs     = p_kokrs
                                        gubun     = 'A'
                                        bdatj     = p_bdatj
                                        poper     = p_poper
                                        fsc_matnr = it_calcu-fsc_matnr
                                        shop      = it_calcu-shop
                                        llv_matnr = it_calcu-llv_matnr
                                        kstar     = space
                                        kostl     = it_calcu-kostl
                                        lstar     = it_calcu-lstar.
    IF sy-subrc NE 0.
      MOVE: p_kokrs            TO it_ztco_stmh_at-kokrs,
            'A'                TO it_ztco_stmh_at-gubun,
            p_bdatj            TO it_ztco_stmh_at-bdatj,
            p_poper            TO it_ztco_stmh_at-poper,
            it_calcu-fsc_matnr TO it_ztco_stmh_at-fsc_matnr,
            it_calcu-shop      TO it_ztco_stmh_at-shop,
            it_calcu-llv_matnr TO it_ztco_stmh_at-llv_matnr,
            it_calcu-kostl     TO it_ztco_stmh_at-kostl,
            it_calcu-lstar     TO it_ztco_stmh_at-lstar.

      MOVE it_calcu-gr_qty TO it_ztco_stmh_at-gr_qty.
      MOVE : sy-uname TO it_ztco_stmh_at-ernam,
             sy-datum TO it_ztco_stmh_at-erdat,
             sy-uzeit TO it_ztco_stmh_at-erzet.

      it_ztco_stmh_at-mbgbtr       = it_ztco_stmh_at-mbgbtr +
                                     it_calcu-mbgbtr.
      it_ztco_stmh_at-add_mbgbtr   = it_ztco_stmh_at-add_mbgbtr +
                                     it_calcu-add_mbgbtr.
      it_ztco_stmh_at-wip_quantity = it_ztco_stmh_at-wip_quantity +
                                     it_calcu-wip_quantity.
      it_ztco_stmh_at-actual_scrap = it_ztco_stmh_at-actual_scrap +
                                     it_calcu-actual_scrap.
      it_ztco_stmh_at-manu_qty     = it_ztco_stmh_at-manu_qty +
                                     it_calcu-manu_qty.

      APPEND it_ztco_stmh_at.
    ELSE.
      MOVE it_calcu-gr_qty    TO it_ztco_stmh_at-gr_qty.
      it_ztco_stmh_at-mbgbtr       = it_ztco_stmh_at-mbgbtr +
                                     it_calcu-mbgbtr.
      it_ztco_stmh_at-add_mbgbtr   = it_ztco_stmh_at-add_mbgbtr +
                                     it_calcu-add_mbgbtr.
      it_ztco_stmh_at-wip_quantity = it_ztco_stmh_at-wip_quantity +
                                     it_calcu-wip_quantity.
      it_ztco_stmh_at-actual_scrap = it_ztco_stmh_at-actual_scrap +
                                     it_calcu-actual_scrap.
      it_ztco_stmh_at-manu_qty     = it_ztco_stmh_at-manu_qty +
                                     it_calcu-manu_qty.
      MODIFY it_ztco_stmh_at INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  LOOP AT it_ztco_stmh_at.
    READ TABLE it_fsc WITH KEY matnr = it_ztco_stmh_at-fsc_matnr.
    IF sy-subrc = 0.
      MOVE it_fsc-outqty TO it_ztco_stmh_at-gr_qty.
      MODIFY it_ztco_stmh_at FROM it_ztco_stmh_at.
    ENDIF.
  ENDLOOP.

  INSERT ztco_stmh_at FROM TABLE it_ztco_stmh_at
                      ACCEPTING DUPLICATE KEYS .

  IF sy-subrc = 0.
    MESSAGE s009 WITH 'ZTCO_STMH_AT' 'Actual'.
    COMMIT WORK.
*Final Data Calculation
    PERFORM final_calculation.
  ELSE.
    MESSAGE s045 WITH 'ZTCO_STMH_AT' 'Actual'.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " update_ztco_stmh_at
*&---------------------------------------------------------------------*
*&      Form  gathering_data_SHOPCOST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gathering_data_shopcost USING p_klvar.
  CLEAR it_ckmlmv003. REFRESH :it_st_source,it_shopcost.
  CASE p_klvar.
    WHEN 'PPC1'.
      SELECT kokrs bdatj poper klvar fsc_matnr shop llv_matnr
             kstar elemt kostl lstar menge hwaer menge
           INTO CORRESPONDING FIELDS OF TABLE it_shopcost
            FROM ztco_shopcost
             WHERE kokrs EQ p_kokrs
               AND klvar EQ p_klvar
               AND bdatj EQ p_bdatj
               AND poper EQ p_poper
               AND typps IN ('M','V', 'E')
               AND elemt EQ '070'.

      it_st_source[] = it_shopcost[].
*Read WIP data (FSC)
      PERFORM read_wip_fsc.

    WHEN 'ZPCP'.
      SELECT kokrs bdatj poper klvar fsc_matnr shop llv_matnr kstar
             elemt kostl lstar menge hwaer menge
              INTO CORRESPONDING FIELDS OF TABLE it_shopcost
                FROM ztco_shopcost
                WHERE kokrs EQ p_kokrs
                  AND klvar EQ p_klvar
                  AND poper EQ p_poper
                  AND bdatj EQ p_bdatj
                  AND elemt EQ '070'.

  ENDCASE.


  LOOP AT it_shopcost.
    IF it_shopcost-kstar EQ '0000836001' OR
       it_shopcost-kstar EQ '0000540300'.
      CONTINUE.
    ELSE.
      DELETE TABLE it_shopcost FROM it_shopcost.
    ENDIF.
  ENDLOOP.
  IF p_bpl EQ 'X'.
    LOOP AT it_shopcost WHERE kstar EQ '0000540300' AND
                              llv_matnr EQ space.
      DELETE TABLE it_shopcost FROM it_shopcost.
    ENDLOOP.
  ENDIF.
  SORT it_shopcost BY  kokrs bdatj klvar fsc_matnr
                       shop llv_matnr kstar kostl lstar.

  DESCRIBE TABLE it_shopcost LINES w_int.
  IF w_int = 0.
    MESSAGE i026.
    STOP.
  ENDIF.

ENDFORM.                    " gathering_data_SHOPCOST
*&---------------------------------------------------------------------*
*&      Form  define_level_SHOPCOST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM define_level_shopcost.
  DATA : c_level.
  DATA : f_level,s_level.

  CLEAR : it_shopcost, c_level,f_level,s_level.
  REFRESH it_temp_st[].
  it_temp_st[] = it_shopcost[].

  f_level = 1.  s_level = 2.
*1 Level
  LOOP AT it_shopcost.
    READ TABLE it_temp_st WITH KEY
                       llv_matnr = it_shopcost-fsc_matnr.
    IF sy-subrc <> 0.
      c_level = f_level.
      it_shopcost-level = c_level.
    ELSE.
      c_level = s_level.
      it_shopcost-level = c_level.
    ENDIF.
    MODIFY  it_shopcost FROM it_shopcost.
  ENDLOOP.

  REFRESH it_temp_st[].
  it_temp_st[] = it_shopcost[].
  DO 7 TIMES.
    f_level = f_level + 1.
    s_level = s_level + 1.
*Sub Level
    PERFORM define_level_detail_shopcost USING f_level s_level.

  ENDDO.

ENDFORM.                    " define_level_SHOPCOST
*&---------------------------------------------------------------------*
*&      Form  define_level_detail_SHOPCOST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_F_LEVEL  text
*      -->P_S_LEVEL  text
*----------------------------------------------------------------------*
FORM define_level_detail_shopcost USING  p_l1 p_l2.
  DATA : f_matnr LIKE it_collect-llv_matnr.
  CLEAR f_matnr.
  LOOP AT it_shopcost WHERE level EQ p_l1.
    READ TABLE it_temp_st WITH KEY
                                   level = p_l1
                                   llv_matnr = it_shopcost-fsc_matnr.
    IF sy-subrc  <> 0.
      it_shopcost-level = p_l1.
    ELSE.
      it_shopcost-level = p_l2.
    ENDIF.

    MODIFY  it_shopcost FROM it_shopcost.


*    it_shopcost-level = p_l1.
*    MODIFY  it_shopcost FROM it_shopcost.
*
*    READ TABLE it_temp_st WITH KEY
*                        fsc_matnr = it_shopcost-llv_matnr.
*    IF sy-subrc = 0.
*      it_shopcost-level = p_l2.
*      MODIFY  it_shopcost TRANSPORTING level
*          WHERE fsc_matnr EQ it_shopcost-llv_matnr.
*    ENDIF.
  ENDLOOP.

ENDFORM.                    " define_level_detail_SHOPCOST
*&---------------------------------------------------------------------*
*&      Form  calculate_quantity_st
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_quantity_st.
  DATA : last_flag(1),c_flag,p_flag,
         f_manu_qty LIKE it_collect-manu_qty,
         f_gr_qty LIKE it_collect-gr_qty,
         it_out_temp LIKE it_shopcost,
         f_llv_matnr LIKE it_shopcost-llv_matnr.

  CLEAR :last_flag,c_flag,p_flag,f_manu_qty,f_gr_qty,f_llv_matnr.

  SORT it_shopcost BY level DESCENDING.

  REFRESH it_temp_st.
  it_temp_st[] = it_shopcost[].

  READ TABLE it_shopcost INDEX  1.
  last_flag = it_shopcost-level.
  c_flag = last_flag.
  DO .
    IF c_flag = last_flag.
      LOOP AT it_shopcost WHERE level EQ c_flag.
        MOVE-CORRESPONDING it_shopcost TO it_out.
        APPEND it_out.CLEAR it_out.
      ENDLOOP.
      c_flag = c_flag - 1.
      CHECK c_flag = '0'.
      EXIT.
    ELSE.
*in case of standard M/H
      IF p_std EQ 'X'.
        p_flag = c_flag + 1.
        LOOP AT it_shopcost WHERE level EQ c_flag.
          IF it_shopcost-llv_matnr EQ space
             AND it_shopcost-kstar EQ cons1.
            MOVE-CORRESPONDING it_shopcost TO it_out.
            APPEND it_out.CLEAR it_out.
          ELSE.
            MOVE-CORRESPONDING it_shopcost TO it_out.
            LOOP AT it_temp_st WHERE level     = p_flag
                                 AND fsc_matnr = it_shopcost-llv_matnr.
              MOVE-CORRESPONDING it_shopcost TO it_out.
              IF it_temp_st-llv_matnr EQ space.
                MOVE : it_temp_st-fsc_matnr TO it_out-llv_matnr,
                       cons1                TO it_out-kstar,
                       c_flag               TO it_out-level,
                       it_temp_st-kostl     TO it_out-kostl,
                       it_temp_st-lstar     TO it_out-lstar.
                it_out-menge = it_temp_st-menge * it_shopcost-menge.

              ELSE.
                MOVE : it_temp_st-llv_matnr TO it_out-llv_matnr,
                       cons1                TO it_out-kstar,
                       c_flag               TO it_out-level,
                       it_temp_st-kostl     TO it_out-kostl,
                       it_temp_st-lstar     TO it_out-lstar.
                it_out-menge = it_temp_st-menge * it_shopcost-menge.
              ENDIF.
              APPEND it_out.
              CLEAR it_out.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
* in case of Annual Business
      ELSEIF p_bpl EQ 'X'.
        LOOP AT it_shopcost WHERE level EQ c_flag.
          IF it_shopcost-llv_matnr EQ space
             AND it_shopcost-kstar EQ cons1.
            MOVE-CORRESPONDING it_shopcost TO it_out.
            APPEND it_out.CLEAR it_out.
          ELSE.
            p_flag = c_flag + 1.

            MOVE-CORRESPONDING it_shopcost TO it_out_temp.
            MOVE it_out_temp-llv_matnr TO f_llv_matnr.

            LOOP AT it_temp_st WHERE fsc_matnr = f_llv_matnr
                                  AND level EQ p_flag.
              MOVE-CORRESPONDING it_out_temp TO it_out.
              IF it_temp_st-llv_matnr EQ space
                 AND it_temp_st-kstar EQ cons1.
                MOVE : it_temp_st-fsc_matnr  TO it_out-llv_matnr,
                       c_flag                TO it_out-level,
                       cons1                 TO it_out-kstar.
              ELSE.
                MOVE : it_temp_st-llv_matnr  TO it_out-llv_matnr,
                       c_flag                TO it_out-level,
                       cons1                 TO it_out-kstar.
              ENDIF.

              MOVE : it_temp_st-kostl        TO it_out-kostl,
                     it_temp_st-lstar        TO it_out-lstar.

*              IF it_shopcost-kstar EQ cons2.
              it_out-menge = it_temp_st-menge * it_out_temp-menge.

*                READ TABLE it_shopcost WITH KEY fsc_matnr =
*                                          it_shopcost-llv_matnr
*                                               kstar = cons1.
*                IF sy-subrc = 0.
*                  MOVE : it_shopcost-kostl       TO it_out-kostl,
*                         it_shopcost-lstar       TO it_out-lstar.
*                ENDIF.
*              ENDIF.
              APPEND it_out.
              CLEAR it_out.
            ENDLOOP.
          ENDIF.
        ENDLOOP.

      ENDIF.
      IF c_flag EQ '1'.
        EXIT.
      ENDIF.
      c_flag = c_flag - 1.
    ENDIF.
    REFRESH it_temp_st.
    it_temp_st[] = it_out[].
  ENDDO.
ENDFORM.                    " calculate_quantity_st
*&---------------------------------------------------------------------*
*&      Form  read_wip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_wip.

  DATA : it_wip_temp LIKE ztco_wip OCCURS 0 WITH HEADER LINE.
  DATA : z_yymm(6).
  REFRESH :it_wip_temp,it_wip.
  CLEAR z_yymm.

  z_yymm(4) = p_bdatj .
  z_yymm+4(2) = p_poper+1(2).

  SELECT * INTO TABLE it_wip_temp FROM ztco_wip
   WHERE yymm EQ z_yymm
     AND workct <> space
     AND gubun EQ 'W'.

  SORT it_wip_temp BY matnr workct .

  LOOP AT it_wip_temp.
    IF sy-tabix = 1.
      MOVE : it_wip_temp-matnr  TO it_wip-matnr,
             it_wip_temp-workct TO it_wip-workct.
    ENDIF.

    IF it_wip_temp-matnr <> it_wip-matnr OR
       it_wip_temp-workct <> it_wip-workct.

      APPEND it_wip.CLEAR it_wip.
      MOVE : it_wip_temp-matnr  TO it_wip-matnr,
           it_wip_temp-workct TO it_wip-workct.
    ENDIF.
    it_wip-bwqty     = it_wip-bwqty    + it_wip_temp-bwqty.
    it_wip-inqty     = it_wip-inqty    + it_wip_temp-inqty.
    it_wip-outqty    = it_wip-outqty   + it_wip_temp-outqty.
    it_wip-ewqty     = it_wip-ewqty    + it_wip_temp-ewqty.
    it_wip-scrapqty  = it_wip-scrapqty + it_wip_temp-scrapqty.

    AT LAST.
      APPEND it_wip.CLEAR it_wip.
    ENDAT.
    CLEAR it_wip_temp.
  ENDLOOP.

  DESCRIBE TABLE it_wip LINES w_int.
  IF w_int = 0.
    MESSAGE i001 WITH 'WIP DATA'.
  ENDIF.
ENDFORM.                    " read_wip
*&---------------------------------------------------------------------*
*&      Form  final_calculation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM final_calculation.
  tables: mara.
  DATA : it_sum LIKE  it_ztco_stmh_at OCCURS 0 WITH HEADER LINE.

  PERFORM progress_indicator USING  '80' ' '
                   'Final Calculation'.

  CLEAR f_save.
*delete old data : table
  DELETE FROM ztco_stmh_at WHERE kokrs EQ p_kokrs
                             AND bdatj EQ p_bdatj
                             AND poper EQ p_poper
                             AND gubun EQ 'F'.

  REFRESH it_ztco_stmh_at.
*  LOOP AT it_calcu.it_wip
  LOOP AT it_calcu.
    SELECT SINGLE * FROM mara WHERE matnr EQ it_calcu-fsc_matnr.
    CASE mara-mtart.
*in case of 'FERT'
      WHEN 'FERT'.
        PERFORM fert_calculation USING it_calcu.
*in case of 'HALB'
      WHEN 'HALB'.
        PERFORM halb_calculation USING it_calcu.
      WHEN OTHERS .
        CONTINUE.
    ENDCASE.

    PERFORM basic_data.
*Gr qty
    it_ztco_stmh_at-gr_qty = it_calcu-gr_qty.

    APPEND it_ztco_stmh_at.CLEAR it_ztco_stmh_at.
  ENDLOOP.
*Summary
  SORT it_ztco_stmh_at BY fsc_matnr shop llv_matnr kostl lstar.
  LOOP AT it_ztco_stmh_at.
    IF sy-tabix = 1.
      MOVE :it_ztco_stmh_at-fsc_matnr  TO it_sum-fsc_matnr,
            it_ztco_stmh_at-shop       TO it_sum-shop,
            it_ztco_stmh_at-llv_matnr  TO it_sum-llv_matnr,
            it_ztco_stmh_at-kostl      TO it_sum-kostl,
            it_ztco_stmh_at-lstar      TO it_sum-lstar.
    ENDIF.
    IF ( it_ztco_stmh_at-fsc_matnr <> it_sum-fsc_matnr ) OR
       ( it_ztco_stmh_at-shop <> it_sum-shop ) OR
       ( it_ztco_stmh_at-llv_matnr <>  it_sum-llv_matnr ) OR
       ( it_ztco_stmh_at-kostl <>  it_sum-kostl ) OR
       ( it_ztco_stmh_at-lstar <> it_sum-lstar ).

      it_sum-kokrs = p_kokrs.
      it_sum-gubun = 'F'.
      it_sum-bdatj = p_bdatj.
      it_sum-poper = p_poper.
      it_sum-erdat = sy-datum.
      it_sum-erzet = sy-uzeit.
      it_sum-ernam = sy-uname.

      APPEND it_sum.CLEAR it_sum.
      MOVE :it_ztco_stmh_at-fsc_matnr  TO it_sum-fsc_matnr,
            it_ztco_stmh_at-shop       TO it_sum-shop,
            it_ztco_stmh_at-llv_matnr  TO it_sum-llv_matnr,
            it_ztco_stmh_at-kostl      TO it_sum-kostl,
            it_ztco_stmh_at-lstar      TO it_sum-lstar,
            it_ztco_stmh_at-gr_qty     TO it_sum-gr_qty.
    ENDIF.
*    MOVE-CORRESPONDING it_calcu TO it_ztco_stmh_at.
    it_sum-mbgbtr     =   it_sum-mbgbtr + it_ztco_stmh_at-mbgbtr.
    it_sum-add_mbgbtr = it_sum-add_mbgbtr +
                                it_ztco_stmh_at-add_mbgbtr.
    it_sum-wip_quantity = it_sum-wip_quantity +
                                it_ztco_stmh_at-wip_quantity.
    it_sum-actual_scrap =  it_sum-actual_scrap +
                                it_ztco_stmh_at-actual_scrap.
    it_sum-manu_qty   =   it_sum-manu_qty +
                               it_ztco_stmh_at-manu_qty.

    AT LAST.
      it_sum-kokrs = p_kokrs.
      it_sum-gubun = 'F'.
      it_sum-gr_qty = it_ztco_stmh_at-gr_qty.
      it_sum-bdatj = p_bdatj.
      it_sum-poper = p_poper.
      it_sum-erdat = sy-datum.
      it_sum-erzet = sy-uzeit.
      it_sum-ernam = sy-uname.

      APPEND it_sum.CLEAR it_sum.
    ENDAT.

  ENDLOOP.

  LOOP AT it_sum.
    READ TABLE it_fsc WITH KEY matnr = it_sum-fsc_matnr.
    IF sy-subrc = 0.
      MOVE it_fsc-outqty TO it_sum-gr_qty.
      MODIFY it_sum FROM it_sum.
    ENDIF.
  ENDLOOP.

  INSERT ztco_stmh_at FROM TABLE it_sum
       ACCEPTING DUPLICATE KEYS .

  IF sy-subrc = 0.
    MESSAGE s009 WITH 'ZTCO_STMH_AT' 'Final'.
    COMMIT WORK.
  ELSE.
    MESSAGE s045 WITH 'ZTCO_STMH_AT' 'Final'.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " final_calculation
*&---------------------------------------------------------------------*
*&      Form  FERT_CALCULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fert_calculation USING lt_calcu LIKE it_calcu.

*  it_ztco_stmh_at-fsc_matnr = lt_calcu-fsc_matnr.
*  it_ztco_stmh_at-shop = lt_calcu-shop.
*  it_ztco_stmh_at-llv_matnr = lt_calcu-llv_matnr.
**in case of FSC have no MIP CODE(llv_matnr) --------------------
*  IF lt_calcu-llv_matnr EQ space.
*    it_ztco_stmh_at-kostl = lt_calcu-kostl.
*    READ TABLE it_wip WITH KEY matnr  = lt_calcu-fsc_matnr
*                               workct = lt_calcu-kostl.
*    IF sy-subrc = 0.
*      PERFORM fert_calculation_quantity USING lt_calcu.
*    ENDIF.
*  ELSE.
**in case  FSC have MIP CODE(llv_matnr )----------------------
*    SELECT SINGLE * FROM ztco_stmh_at_cov
*          WHERE  shop EQ lt_calcu-shop.
*    IF sy-subrc = 0.
*      READ TABLE it_wip WITH KEY matnr  = lt_calcu-fsc_matnr
*                                 workct = ztco_stmh_at_cov-kostl.
*      IF sy-subrc = 0.
*        it_ztco_stmh_at-kostl = lt_calcu-kostl.
*        PERFORM fert_calculation_quantity USING lt_calcu.
*      ENDIF.
*    ELSE.
*      it_ztco_stmh_at-kostl = lt_calcu-kostl.
*      READ TABLE it_wip WITH KEY matnr  = lt_calcu-fsc_matnr
*                                 workct = lt_calcu-kostl.
*      IF sy-subrc = 0.
*        PERFORM fert_calculation_quantity USING lt_calcu.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
ENDFORM.                    " FERT_CALCULATION
*&---------------------------------------------------------------------*
*&      Form  BASIC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM basic_data.
  it_ztco_stmh_at-kokrs = p_kokrs.
  it_ztco_stmh_at-gubun = 'F'.
  it_ztco_stmh_at-bdatj = p_bdatj.
  it_ztco_stmh_at-poper = p_poper.
  it_ztco_stmh_at-erdat = sy-datum.
  it_ztco_stmh_at-erzet = sy-uzeit.
  it_ztco_stmh_at-ernam = sy-uname.

ENDFORM.                    " BASIC_DATA
*&---------------------------------------------------------------------*
*&      Form  FERT_CALCULATION_QUANTITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fert_calculation_quantity USING lt_calcu LIKE it_calcu.
*Current Quantity
  IF it_wip-inqty = 0.
    it_ztco_stmh_at-mbgbtr = 0.
  ELSE.
    it_ztco_stmh_at-mbgbtr = lt_calcu-mbgbtr / it_wip-inqty .
  ENDIF.
*Additional Issue Quantity
  IF it_wip-inqty = 0.
    it_ztco_stmh_at-add_mbgbtr = 0.
  ELSE.
    it_ztco_stmh_at-add_mbgbtr =  lt_calcu-add_mbgbtr / it_wip-inqty.
  ENDIF.
*READ WIP QTY : FSC
  READ TABLE it_fsc WITH KEY matnr = lt_calcu-fsc_matnr.
  IF sy-subrc = 0.
*Wip Quantity
    IF it_fsc-ewqty = 0.
      it_ztco_stmh_at-wip_quantity = 0.
    ELSE.
    it_ztco_stmh_at-wip_quantity = lt_calcu-wip_quantity / it_fsc-ewqty.
    ENDIF.
*Scarp Quantity
    IF it_fsc-scrapqty = 0.
      it_ztco_stmh_at-actual_scrap  = 0.
    ELSE.
      it_ztco_stmh_at-actual_scrap =
                            lt_calcu-actual_scrap / it_fsc-scrapqty.
    ENDIF.

*Actual Manufacture Quantity
    IF it_fsc-outqty = 0.
      it_ztco_stmh_at-manu_qty = 0.
    ELSE.
      it_ztco_stmh_at-manu_qty =  lt_calcu-manu_qty / it_fsc-outqty.
    ENDIF.
  ENDIF.
ENDFORM.                    " FERT_CALCULATION_QUANTITY
*&---------------------------------------------------------------------*
*&      Form  halb_calculation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CALCU  text
*----------------------------------------------------------------------*
FORM halb_calculation USING lt_calcu LIKE it_calcu.

*  it_ztco_stmh_at-fsc_matnr = lt_calcu-fsc_matnr.
*  it_ztco_stmh_at-shop = lt_calcu-shop.
*  it_ztco_stmh_at-llv_matnr = lt_calcu-llv_matnr.
*
*  IF lt_calcu-llv_matnr <> space.
*    IF ( it_calcu-fsc_matnr+4(2) EQ 'XY' ) OR
*        ( it_calcu-fsc_matnr+4(2) EQ 'XX' ).
*
*      SELECT SINGLE * FROM ztco_stmh_at_cov
*            WHERE  shop EQ lt_calcu-shop.
*      lt_calcu-kostl = lt_calcu-kostl.
*    ENDIF.
*  ENDIF.
*  it_ztco_stmh_at-kostl = lt_calcu-kostl.
*
*  READ TABLE it_wip WITH KEY matnr  = lt_calcu-fsc_matnr
*                             workct = lt_calcu-kostl.
*  IF sy-subrc = 0.
*    IF ( it_calcu-fsc_matnr+4(2) EQ 'XY' ) OR
*       ( it_calcu-fsc_matnr+4(2) EQ 'XX' ).
**Current Quantity
*      IF lt_calcu-gr_qty = 0.
*        it_ztco_stmh_at-mbgbtr  = 0.
*      ELSE.
*        it_ztco_stmh_at-mbgbtr =  lt_calcu-mbgbtr / it_wip-inqty.
*      ENDIF.
**Additional Issue Quantity
*      IF lt_calcu-gr_qty = 0.
*        it_ztco_stmh_at-add_mbgbtr = 0.
*      ELSE.
*        it_ztco_stmh_at-add_mbgbtr = lt_calcu-add_mbgbtr / it_wip-inqty
.
*      ENDIF.
*    ELSE.
**READ WIP QTY : FSC
*      READ TABLE it_fsc WITH KEY matnr = lt_calcu-fsc_matnr.
*      IF sy-subrc = 0.
*
**Current Quantity
*        IF it_fsc-inqty = 0.
*          it_ztco_stmh_at-mbgbtr  = 0.
*        ELSE.
*          it_ztco_stmh_at-mbgbtr =  lt_calcu-mbgbtr / it_fsc-inqty.
*        ENDIF.
**Additional Issue Quantity
*        IF it_fsc-inqty = 0.
*          it_ztco_stmh_at-add_mbgbtr = 0.
*        ELSE.
*        it_ztco_stmh_at-add_mbgbtr = lt_calcu-add_mbgbtr / it_fsc-inqty
.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
**READ WIP QTY : FSC
*  READ TABLE it_fsc WITH KEY matnr = lt_calcu-fsc_matnr.
*  IF sy-subrc = 0.
*
**Wip Quantity
*    IF it_fsc-ewqty = 0.
*      it_ztco_stmh_at-wip_quantity =  0.
*    ELSE.
*      it_ztco_stmh_at-wip_quantity =  lt_calcu-wip_quantity
*           /  it_fsc-ewqty.
*    ENDIF.
**Scarp Quantity
*    IF it_fsc-scrapqty = 0.
*      it_ztco_stmh_at-actual_scrap  = 0.
*    ELSE.
*      it_ztco_stmh_at-actual_scrap =
*                            lt_calcu-actual_scrap / it_fsc-scrapqty.
*    ENDIF.
*
**Actual Manufacture Quantity
*    IF it_fsc-outqty = 0.
*      it_ztco_stmh_at-manu_qty  = 0.
*    ELSE.
*      it_ztco_stmh_at-manu_qty = lt_calcu-manu_qty / it_fsc-outqty.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " halb_calculation
*&---------------------------------------------------------------------*
*&      Form  progress_indicator
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0079   text
*----------------------------------------------------------------------*
FORM progress_indicator  USING   p_%  p_wint p_text.

  DATA : l_text(40).
  CONCATENATE p_text  p_% '%' INTO l_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = p_%
            text       = l_text.

ENDFORM.                    " progress_indicator
*&---------------------------------------------------------------------*
*&      Form  period_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM period_check.
  CHECK p_bdatj = '2004'.
  CHECK p_poper = '006'.
  MESSAGE s001 WITH p_bdatj p_poper.
  STOP.

ENDFORM.                    " period_check
*&---------------------------------------------------------------------*
*&      Form  READ_WIP_FSC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_wip_fsc.
  DATA : it_wip_temp LIKE ztco_wip OCCURS 0 WITH HEADER LINE.
  DATA : z_yymm(6).
  REFRESH :it_wip_temp,it_fsc.
  CLEAR z_yymm.

  z_yymm(4) = p_bdatj .
  z_yymm+4(2) = p_poper+1(2).

  SELECT * INTO TABLE it_wip_temp FROM ztco_wip
   WHERE yymm EQ z_yymm
     AND gubun EQ 'F'.

  SORT it_wip_temp BY matnr .

  LOOP AT it_wip_temp.
    IF sy-tabix = 1.
      MOVE : it_wip_temp-matnr  TO it_fsc-matnr.
    ENDIF.

    IF it_wip_temp-matnr <> it_fsc-matnr .

      APPEND it_fsc.CLEAR it_fsc.
      MOVE : it_wip_temp-matnr  TO it_fsc-matnr.
    ENDIF.
    it_fsc-bwqty     = it_fsc-bwqty    + it_wip_temp-bwqty.
    it_fsc-inqty     = it_fsc-inqty    + it_wip_temp-inqty.
    it_fsc-outqty    = it_fsc-outqty   + it_wip_temp-outqty.
    it_fsc-ewqty     = it_fsc-ewqty    + it_wip_temp-ewqty.
    it_fsc-scrapqty  = it_fsc-scrapqty + it_wip_temp-scrapqty.

    AT LAST.
      APPEND it_fsc.CLEAR it_fsc.
    ENDAT.
    CLEAR it_wip_temp.
  ENDLOOP.

  DESCRIBE TABLE it_fsc LINES w_int.
  IF w_int = 0.
    MESSAGE i001 WITH 'WIP DATA ( FSC) '.
  ENDIF.

ENDFORM.                    " READ_WIP_FSC
*&---------------------------------------------------------------------*
*&      Form  ML_QTY_FILLED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ml_qty_filled.
  LOOP AT it_ztco_stmh.
    READ TABLE it_ckmlmv003 WITH KEY matnr = it_ztco_stmh-fsc_matnr.
    IF sy-subrc = 0.
*      MOVE it_zcor07-zu_lbkum TO  it_ztco_stmh-
    ELSE.
      DELETE TABLE it_ztco_stmh FROM it_ztco_stmh.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " ML_QTY_FILLED
*&---------------------------------------------------------------------*
*&      Form  SCREEN_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_out.
*  LOOP AT SCREEN.
*    IF  p_bpl EQ 'X' .
*      IF screen-name = 'P_POPER'.
*        screen-input = '0'.
*      ENDIF.
*    ELSEIF p_std EQ 'X' .
*      IF screen-name = 'P_POPER'.
*        screen-input = '1'.
*      ENDIF.
*    ENDIF.
*    MODIFY SCREEN.
*  ENDLOOP.
ENDFORM.                    " SCREEN_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_LAST_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0781   text
*----------------------------------------------------------------------*
FORM get_last_month USING  p_flag p_matnr.
  DATA : f_poper LIKE ztco_stmh_at-poper,
         f_date LIKE p0001-begda,
         t_date LIKE p0001-begda,
         c_month LIKE t5a4a-dlymo,
         it_collect LIKE ztco_stmh_at,
         f_bdatj LIKE p_bdatj.
  CLEAR f_bdatj.

  DO 12 TIMES.
    CONCATENATE p_bdatj p_poper+1(2) '01' INTO f_date.
    c_month = c_month + 1.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
         EXPORTING
              date      = f_date
              days      = '00'
              months    = c_month
              signum    = '-'
              years     = '00'
         IMPORTING
              calc_date = t_date.

    f_poper = t_date+4(2).

    IF p_poper < f_poper.
      f_bdatj = p_bdatj - 1.
    ENDIF.

    SELECT * INTO it_collect FROM ztco_stmh_at
      WHERE kokrs EQ p_kokrs
        AND gubun EQ 'A'
        AND bdatj EQ f_bdatj
        AND poper EQ f_poper
        AND fsc_matnr EQ p_matnr.

      MOVE-CORRESPONDING it_collect TO it_calcu.
      MOVE p_flag                   TO it_calcu-level.
      APPEND it_calcu.CLEAR it_calcu.
    ENDSELECT.

    IF it_collect-gr_qty NE space.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " GET_LAST_MONTH
*&---------------------------------------------------------------------*
*&      Form  GET_LAST_MONTH_DATA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_F_LLV_MATNR  text
*      -->P_C_FLAG  text
*----------------------------------------------------------------------*
FORM get_last_month_data2 USING    p_llv_matnr
                                   pc_flag
                                   it_collect LIKE it_collect.
  DATA : f_poper  LIKE ztco_stmh_at-poper,
         f_date   LIKE p0001-begda,
         t_date   LIKE p0001-begda,
         c_month  LIKE t5a4a-dlymo,
         it_collect_temp LIKE ztco_stmh_at,
         f_manu_qty      LIKE it_collect-manu_qty,
         f_gr_qty LIKE it_collect-gr_qty,
         f_bdatj  LIKE p_bdatj.
  CLEAR f_bdatj.

  DO 12 TIMES.
    CONCATENATE p_bdatj p_poper+1(2) '01' INTO f_date.
    c_month = c_month + 1.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
         EXPORTING
              date      = f_date
              days      = '00'
              months    = c_month
              signum    = '-'
              years     = '00'
         IMPORTING
              calc_date = t_date.

    f_poper = t_date+4(2).

    IF p_poper < f_poper.
      f_bdatj = p_bdatj - 1.
    ENDIF.

    SELECT * INTO it_collect_temp FROM ztco_stmh_at
      WHERE kokrs EQ p_kokrs
        AND gubun EQ 'A'
        AND bdatj EQ f_bdatj
        AND poper EQ f_poper
        AND fsc_matnr EQ p_llv_matnr.

      MOVE-CORRESPONDING it_collect_temp TO it_calcu.
      MOVE it_collect-fsc_matnr TO it_calcu-fsc_matnr.
      IF it_collect_temp-llv_matnr EQ space ."AND
*               it_collect_temp-kstar EQ cons1.
        MOVE : it_collect_temp-fsc_matnr TO it_calcu-llv_matnr,
               pc_flag                   TO it_calcu-level,
               cons1                     TO it_calcu-kstar.
      ELSE.
        MOVE : it_collect_temp-llv_matnr TO it_calcu-llv_matnr,
               pc_flag                   TO it_calcu-level,
               cons1                     TO it_calcu-kstar.
      ENDIF.
      MOVE : it_collect_temp-kostl       TO it_calcu-kostl,
             it_collect_temp-lstar       TO it_calcu-lstar.

*Calculate
      MOVE : it_collect_temp-manu_qty TO f_manu_qty,
             it_collect_temp-gr_qty   TO f_gr_qty.
      IF f_gr_qty EQ 0.
        f_gr_qty = 1.
      ENDIF.
*Actual Manufactur QTY =
* ( below level actual manufacture qty / actual qty ) * each leve
*                                     quantity
*Current QTY
      it_calcu-mbgbtr = ( f_manu_qty / f_gr_qty ) *
                          it_collect-mbgbtr.
*Additional issue QTY
      it_calcu-add_mbgbtr = ( f_manu_qty / f_gr_qty ) *
                             it_collect-add_mbgbtr.
*WIP QTY
      it_calcu-wip_quantity = ( f_manu_qty / f_gr_qty ) *
                                it_collect-wip_quantity.
*Scrap QTY
      it_calcu-actual_scrap = ( f_manu_qty / f_gr_qty ) *
                                it_collect-actual_scrap.
*Actual Manufacture QTY
      it_calcu-manu_qty = ( f_manu_qty / f_gr_qty ) *
                            it_collect-manu_qty.
      APPEND it_calcu.
      CLEAR : it_calcu,f_manu_qty,f_gr_qty.
    ENDSELECT.

    IF it_collect_temp-gr_qty NE space.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " GET_LAST_MONTH_DATA2
*&---------------------------------------------------------------------*
*&      Form  get_pir_Qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZTCO_STMH  text
*----------------------------------------------------------------------*
FORM get_pir_qty TABLES lp_ztco_stmh STRUCTURE it_ztco_stmh.
  DATA : BEGIN OF it_pbim OCCURS 0,
          matnr LIKE pbim-matnr,
          perxx LIKE pbed-perxx,
          plnmg LIKE pbed-plnmg,
         END OF it_pbim.
  DATA : l_perid LIKE pbed-perxx.

  REFRESH it_pbim.CLEAR l_perid.

  CONCATENATE p_bdatj p_poper+1(2) INTO l_perid.

  SELECT a~matnr b~perxx b~plnmg
    INTO CORRESPONDING FIELDS OF TABLE it_pbim
      FROM pbim AS a INNER JOIN pbed AS b
       ON a~bdzei = b~bdzei
        WHERE a~werks EQ p_werks
          AND a~versb EQ 'Y1'
          AND b~perxx EQ l_perid.
  LOOP AT it_pbim.
    LOOP AT lp_ztco_stmh WHERE fsc_matnr EQ it_pbim-matnr.
      MOVE it_pbim-plnmg TO lp_ztco_stmh-gr_qty.

      MODIFY lp_ztco_stmh FROM lp_ztco_stmh.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " get_pir_Qty
