FUNCTION zsapbf_ppc1_comp_ord_part_get.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IF_ORDERID) LIKE  PPC_HEAD-ORDERID
*"     VALUE(IF_HEADQUANT) TYPE  PPC_HEADCONFQUANT
*"     VALUE(IF_REPPOINT) TYPE  PPC_REPPOINT_INT
*"     VALUE(IF_BW_CALL) TYPE  PPC_FLG_BW_CONF DEFAULT SPACE
*"     VALUE(I_DATE_LOW) TYPE  BUDAT OPTIONAL
*"     VALUE(I_DATE_HIGH) TYPE  BUDAT OPTIONAL
*"  EXPORTING
*"     VALUE(ET_MATERIAL_COMPONENTS) TYPE  PPC_T_MATERIAL_COMPONENTS
*"     VALUE(ET_ACTIVITY_COMPONENTS) TYPE  PPC_T_ACTIVITY_COMPONENTS
*"  CHANGING
*"     VALUE(CF_ORDERID) LIKE  PPC_HEAD-ORDERID OPTIONAL
*"     VALUE(C_GJPER) TYPE  CO_GJPER OPTIONAL
*"  EXCEPTIONS
*"      GET_COMP_ERROR
*"      INPUT_ERROR
*"      NO_RP_FROM_BW
*"----------------------------------------------------------------------


*-----------------------------------------------------------------------
* ET_MATERIAL_COMPONENTS contains the actual quantities (field QUANTITY)
* and the delta to the planned quantities (field DELTA_QUANTITY)
*-----------------------------------------------------------------------


  DATA: ls_material_components TYPE ppc_material_components,
        ls_rev_mat_components TYPE ppc_material_components,
        ls_mat_comp_var TYPE ppc_material_components,
        ls_rev_mat_comp_var TYPE ppc_material_components.
  DATA: lt_rev_mat_components TYPE ppc_material_components OCCURS 0,
        lt_mat_comp_var TYPE ppc_material_components OCCURS 0,
        lt_rev_mat_comp_var TYPE ppc_material_components OCCURS 0.
  DATA: ls_rmprofile TYPE ppc_profile_gen.
  DATA: ls_flag_pkosa TYPE c VALUE space.
  DATA: ls_reppoints TYPE ppc_reppoints,
        lt_reppoints TYPE ppc_t_reppoints,
        ls_rev_reppoints TYPE ppc_reppoints,
        lt_rev_reppoints TYPE ppc_t_reppoints.
  DATA: ls_tabix LIKE sy-tabix.
  DATA: ls_activity_components TYPE ppc_activity_components,
        ls_rev_act_comp TYPE ppc_activity_components,
        ls_act_comp_var TYPE ppc_activity_components,
        ls_rev_act_comp_var TYPE ppc_activity_components.
  DATA: lt_rev_act_comp TYPE ppc_t_activity_components,
        lt_act_comp_var TYPE ppc_t_activity_components,
        lt_rev_act_comp_var TYPE ppc_t_activity_components.
  DATA: lf_dummy_order TYPE c.

***--- Start; Added by James Sung-Kon Kim 2011/02/21
  DATA : BEGIN OF ls_reppoints_flag.
          INCLUDE STRUCTURE ppc_reppoints.
  DATA : case(12).  "'NORMAL', 'EQUAL', 'ONLY_REVERSAL', 'ONLY_FORWARD'
  DATA : END OF ls_reppoints_flag.

  DATA : lt_reppoints_flag LIKE TABLE OF ls_reppoints_flag.
***--- End; Added by James Sung-Kon Kim 2011/02/21

*=========================================================  C5024598 beg
  DATA: l_line_header  TYPE ppc_ippe_header_int,
        l_line_version TYPE ppc_line_version.
  DATA: lt_rp_pred_liste TYPE  ppc_t_reppoint.
  DATA: lt_rp_rel TYPE  ppc_t_reppoint_rel,                 "C5024598
        ls_rp_rel TYPE ppc_reppoint_rel.                    "C5024598
  DATA: l_hq TYPE ppc_headconfquant,
        l_zp TYPE ppc_reppoint_int.
* reporting point not optional any more (DI46C2)
  IF if_reppoint IS INITIAL.
    IF if_bw_call NE con_true.
      MESSAGE e024 RAISING input_error.
    ELSE.
* in case of BW-Call don't break off
      MESSAGE w024 RAISING no_rp_from_bw.
    ENDIF.
  ENDIF.


* Read line_header line_version for FB:PPC1RP_RP_PREDECESSORS_GET
  CLEAR lf_dummy_order.
  CALL FUNCTION 'PPC1DC_ORD_INF_READ'
    EXPORTING
      if_orderid         = if_orderid
    IMPORTING
      ef_line_header     = l_line_header
      ef_line_version    = l_line_version
      ef_dummy_order     = lf_dummy_order
    EXCEPTIONS
      insufficient_input = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


* Beende Baustein, wenn ein Dummy-Auftrag übergeben wurde
  IF lf_dummy_order = abap_true.
    EXIT.
  ENDIF.


*Wenn wir nur den Ausschuß-ZP bekommen, dann ermitteln alle Vorgängern
*ausfiltern die MAP/ALG-Objecte.
* IF_REPPOINT ist der Ausschuß-ZP, also der letzte ZP
*  => LT_RP_PRED_LISTE -alle vorherliegende ZP, auch die alternative ZPe
*  => LT_RP_REL -Vorg.Nachfolger Beziehungen
  CALL FUNCTION 'PPC1RP_RP_PREDECESSORS_GET'
    EXPORTING
      if_nodeid           = l_line_header
      if_counter          = l_line_version
      if_reppoint         = if_reppoint
      if_add_conf_rp      = 'X'
      if_direct_pred_only = ' '
      if_read_alt_zp      = 'X'
    IMPORTING
      et_rp_pred          = lt_rp_pred_liste
      et_rp_rel           = lt_rp_rel
    EXCEPTIONS
      read_error          = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING get_comp_error.
  ENDIF.

*=========================================================  C5024598 end

* ... Determine repetitive manufacturing profile
  IF if_bw_call IS INITIAL.
    CALL FUNCTION 'PPC1DM_RMPROFILE_NAME_GET'
      EXPORTING
        if_orderid     = if_orderid
        if_dummy_order = ' '
      IMPORTING
        ef_rmprofile   = ls_rmprofile
      EXCEPTIONS
        wrong_input    = 1
        no_profile     = 2
        material_error = 3
        OTHERS         = 4.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING get_comp_error.
    ENDIF.
  ENDIF.


* ... Read posted components of the order
*  CALL FUNCTION 'PPC1DC_COMP_ORD_READ_NEW'
  CALL FUNCTION 'ZSAPBF_PPC1_COMP_ORD_READ_NEW'
       EXPORTING
            if_orderid             = if_orderid
*           it_reppoints           = it_reppoints           C5024598 del
            it_reppoints           = lt_rp_pred_liste       "C5024598
            i_date_low             = i_date_low
            i_date_high            = i_date_high
       IMPORTING
            et_material_components = et_material_components
            et_rev_mat_components  = lt_rev_mat_components
            et_mat_comp_var        = lt_mat_comp_var
            et_rev_mat_comp_var    = lt_rev_mat_comp_var
            et_act_comp            = et_activity_components
            et_rev_act_comp        = lt_rev_act_comp
            et_act_comp_var        = lt_act_comp_var
            et_rev_act_comp_var    = lt_rev_act_comp_var
            et_reppoints           = lt_reppoints
            et_rev_reppoints       = lt_rev_reppoints
       EXCEPTIONS
            lock_error             = 1
            OTHERS                 = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING get_comp_error.
  ENDIF.

****** Addition; Start Line ; 2010/09/13  *******
* If there is no forward confirmation records,
* and there are only reversal confirmation/variance confirmation,
* then, the original SAP logic can not calculate the WIP-Credit Quantities.
* Therefore, Here, We add the reversal confirmation records, variance confirmation
* record with the quantity field as zero to the forward confirmation records tables.
* Added by Sung-Kon James Kim, 2010/09/13

  LOOP AT lt_rev_mat_components INTO ls_rev_mat_components.
    CLEAR : ls_rev_mat_components-quantity,
            ls_rev_mat_components-delta_quantity.
    COLLECT ls_rev_mat_components INTO et_material_components.
    CLEAR ls_rev_mat_components.
  ENDLOOP.

  LOOP AT lt_mat_comp_var INTO ls_mat_comp_var.
    CLEAR : ls_mat_comp_var-quantity,
            ls_mat_comp_var-delta_quantity.
    COLLECT ls_mat_comp_var INTO et_material_components.
    CLEAR ls_mat_comp_var.
  ENDLOOP.

  LOOP AT lt_rev_mat_comp_var INTO ls_rev_mat_comp_var.
    CLEAR : ls_rev_mat_comp_var-quantity,
            ls_rev_mat_comp_var-delta_quantity.
    COLLECT ls_rev_mat_comp_var INTO et_material_components.
    CLEAR ls_rev_mat_comp_var.
  ENDLOOP.

  LOOP AT lt_rev_act_comp INTO ls_rev_act_comp.
    CLEAR : ls_rev_act_comp-duration_var,
            ls_rev_act_comp-duration_fix,
            ls_rev_act_comp-delta_dur_var,
            ls_rev_act_comp-delta_dur_fix.
    COLLECT ls_rev_act_comp INTO et_activity_components.
    CLEAR ls_rev_act_comp.
  ENDLOOP.

  LOOP AT lt_act_comp_var INTO ls_act_comp_var.
    CLEAR : ls_act_comp_var-duration_var,
            ls_act_comp_var-duration_fix,
            ls_act_comp_var-delta_dur_var,
            ls_act_comp_var-delta_dur_fix.
    COLLECT ls_act_comp_var INTO et_activity_components.
    CLEAR ls_act_comp_var.
  ENDLOOP.

  LOOP AT lt_rev_act_comp_var INTO ls_rev_act_comp_var.
    CLEAR : ls_rev_act_comp_var-duration_var,
            ls_rev_act_comp_var-duration_fix,
            ls_rev_act_comp_var-delta_dur_var,
            ls_rev_act_comp_var-delta_dur_fix.
    COLLECT ls_rev_act_comp_var INTO et_activity_components.
    CLEAR ls_rev_act_comp_var.
  ENDLOOP.
*
*  LOOP AT lt_rev_reppoints INTO ls_rev_reppoints.
*    CLEAR : ls_rev_reppoints-quantity,
*            ls_rev_reppoints-head_quantity.
*    COLLECT ls_rev_reppoints INTO lt_reppoints.
*    CLEAR ls_rev_reppoints.
*  ENDLOOP.

****** Addition; End Line ; 2010/09/13  *******

* ... Reduce reporting point quantities by the cancel quantities

  LOOP AT lt_rev_reppoints INTO ls_rev_reppoints.
    READ TABLE lt_reppoints INTO ls_reppoints
    WITH KEY reppoint = ls_rev_reppoints-reppoint.
    MOVE sy-tabix TO ls_tabix.

    IF sy-subrc EQ 0.
      IF ls_reppoints-quantity > ls_rev_reppoints-quantity.     "Added by James Sung-Kon Kim 2011.02.21
        ls_reppoints-quantity = ls_reppoints-quantity -         "These are origianl source codes
                                ls_rev_reppoints-quantity.      "These are origianl source codes

***** Important Remark; by Sung Kon James Kime 2011/02/21 ********************************************
* LT_REPPOINTS_FLAG-CASE : 'NORMAL', 'EQUAL', 'ONLY_REVERSE'
*-----------------------------------------------------------------------------------------------------
* In Original Source Codes,
* If the forward confirmation quantity is equal to the reversal confirmation quantity,
* Then, the reporting points' quantity will be zero,
* both the material components' quantities and the activity components' quantities will be zero also.
*
* Under this circumstance,
* If the forward confirmed posting period and the reversal confirmed Posting period ard different,
* Then, for the reversal confirmed posting period, neither material components' quantities nor
* activities' component quantities can be reflected to the yield quantities(CPZP-GMSUM).
*
* Therefore, as of now,
* If the forward confirmation quantity is greater than the reversal confirmation quantity,
* then, this case will be treated as "NORMAL",
* In this case,
* the reporting points' qty will be dtermined by by deducing reversal qty from forward qty,
* and the component/activity quantities will be determined by deducing reversal qty from forward qty, also.
*
* If only forward confirmation exists,
* then this case will be treated as "ONLY_FOWARD",
* In this case,
* the reporting points' qty will be dtermined by by deducing reversal qty(0) from forward qty,
* and the component/activity quantities will be determined by deducing reversal qty(0) from forward qty, also.
*
* If the forward confirmation quantity is eqaul to the reversal confirmation quantity,
* then, this case will be treated as "EQUAL",
* In this case,
* the reporting points' qty will be dtermined using only forward qty itself without regard for reversal qty,
* and the component/activity quantities will be dtermined
* using only forward qty itself without regard for reversal qty, also.
*
* If the forward confirmation quantity is less than the reversal confirmation quantity,
* then, this case will be treated as "ONLY_REVERSE".
* But, rationally, THIS CASE CAN NOT HAPPEN UNDER NORMAL SYSTEM BEHAVIOR, IF THERE IS NO BUG.
* In this case,
* the reporting points' qty will be dtermined using only reversal qty itself without regard for forward qty,
* and the component/activity quantities will be dtermined
* using only reversal qty itself without regard for forward qty, also.
*
******************************************************************************************************

        ls_reppoints_flag = ls_reppoints.                       "Added by James Sung-Kon Kim 2011.02.21
        ls_reppoints_flag-case = 'NORMAL'.                      "Added by James Sung-Kon Kim 2011.02.21

      ELSEIF ls_reppoints-quantity = ls_rev_reppoints-quantity. "Added by James Sung-Kon Kim 2011.02.21
        ls_reppoints-quantity = ls_reppoints-quantity - 0.      "Added by James Sung-Kon Kim 2011.02.21

        ls_reppoints_flag = ls_reppoints.                       "Added by James Sung-Kon Kim 2011.02.21
        ls_reppoints_flag-case = 'EQUAL'.                       "Added by James Sung-Kon Kim 2011.02.21
      ELSE.                                                     "Added by James Sung-Kon Kim 2011.02.21
        ls_reppoints-reppoint = ls_rev_reppoints-reppoint.      "Added by James Sung-Kon Kim 2011.02.21

        ls_reppoints_flag = ls_reppoints.                       "Added by James Sung-Kon Kim 2011.02.21
        ls_reppoints_flag-case = 'ONLY_REVERSE'."Impossible     "Added by James Sung-Kon Kim 2011.02.21
      ENDIF.                                                    "Added by James Sung-Kon Kim 2011.02.21
      MODIFY lt_reppoints FROM ls_reppoints INDEX ls_tabix.

    ELSE. " Start ---Bellow lines added by Sung-Kon James Kim 2010.09.15
      ls_reppoints-reppoint = ls_rev_reppoints-reppoint.
*      ls_reppoints-quantity = 0 -
*                             ls_rev_reppoints-quantity.
*      ls_reppoints-confunit = ls_rev_reppoints-confunit.
*      ls_reppoints-head_quantity = ls_reppoints-quantity.
      ls_reppoints_flag = ls_reppoints.                         "Added by James Sung-Kon Kim 2011.02.21
      ls_reppoints_flag-case = 'ONLY_REVERSE'. "Impossible      "Added by James Sung-Kon Kim 2011.02.21
      APPEND ls_reppoints TO lt_reppoints.
      CLEAR ls_reppoints.
    ENDIF. " End ---Bellow lines added by Sung-Kon James Kim 2010.09.15

    APPEND ls_reppoints_flag TO lt_reppoints_flag.              "Added by James Sung-Kon Kim 2011.02.21

  ENDLOOP.

***--- Start; Added by James Sung-Kon Kim 2011/02/21
  SORT lt_reppoints_flag BY reppoint.

  LOOP AT lt_reppoints INTO ls_reppoints.

    READ TABLE lt_reppoints_flag INTO ls_reppoints_flag
    WITH KEY reppoint = ls_reppoints-reppoint.

    IF sy-subrc NE 0.
      ls_reppoints_flag = ls_reppoints.
      ls_reppoints_flag-case = 'ONLY_FORWARD'. "There is no reversal confirmation for the reporting point.
      APPEND ls_reppoints_flag TO lt_reppoints_flag.
    ENDIF.

  ENDLOOP.

  CLEAR ls_reppoints_flag.
  SORT lt_reppoints_flag BY reppoint.
***--- End; Added by James Sung-Kon Kim 2011/02/21


***--- Start; Added by James Sung-Kon Kim 2011/02/21

*=========================================================  C5024598
* Reduce reporting point quantities depending on alternative Path
* HEAD QUANTITY entsprechend dem alternativen Weg modifizieren!
* Ausgangs ZP IF_REPPOINT
  l_hq = if_headquant.
  l_zp = if_reppoint.
  IF l_hq = if_headquant.
    PERFORM new_determine_head_quantity USING lt_rp_rel
                                              if_headquant
                                              l_zp
                                        CHANGING lt_reppoints.
  ELSE.
    PERFORM determine_head_quantity
                  USING     lt_rp_rel
                            if_headquant
                  CHANGING
                            l_zp
                            l_hq
                            lt_reppoints.
  ENDIF.
*=========================================================  C5024598 end


* ... next steps:
* ... (A) handle the material components
* ... (B) handle the activity components

* ... ----------------------------------
* ... (A) handle the material components
* ... ----------------------------------

* ... Sort the internal tables to be able
* ... to use the 'WITH KEY' statement
  SORT lt_rev_mat_components BY reppoint
                                mat_number
                                plant
                                storage_loc
                                batch
                                supply_area
                                special_stock
                                sales_doc
                                sales_doc_item
                                wbs_elem
                                costing_num
                                special_stock_val
                                consumpt_posting
                                accass_category
                                unit_of_measure. "Added by James Sung-Kon Kim 2011.02.21

  SORT lt_mat_comp_var BY reppoint
                           mat_number
                           plant
                           storage_loc
                           batch
                           supply_area
                           special_stock
                           sales_doc
                           sales_doc_item
                           wbs_elem
                           costing_num
                           special_stock_val
                           consumpt_posting
                           accass_category
                           unit_of_measure. "Added by James Sung-Kon Kim 2011.02.21

  SORT lt_rev_mat_comp_var BY reppoint
                              mat_number
                              plant
                              storage_loc
                              batch
                              supply_area
                              special_stock
                              sales_doc
                              sales_doc_item
                              wbs_elem
                              costing_num
                              special_stock_val
                              consumpt_posting
                              accass_category
                              unit_of_measure. "Added by James Sung-Kon Kim 2011.02.21


* ... Compare the tables
  LOOP AT et_material_components INTO ls_material_components.
    ls_tabix = sy-tabix.

* ... Search corresponding entries in the other internal tables
* ... (1) search cancel entry
* ... (2) search variance entry
* ... (3) search variance entry (cancel)

* ... (1) Search a corresponding cancel entry
    READ TABLE lt_rev_mat_components WITH KEY
      reppoint = ls_material_components-reppoint
      mat_number = ls_material_components-mat_number
      plant = ls_material_components-plant
      storage_loc = ls_material_components-storage_loc
      batch = ls_material_components-batch
      supply_area = ls_material_components-supply_area
      special_stock = ls_material_components-special_stock
      sales_doc = ls_material_components-sales_doc
      sales_doc_item = ls_material_components-sales_doc_item
      wbs_elem = ls_material_components-wbs_elem
      costing_num = ls_material_components-costing_num
      special_stock_val = ls_material_components-special_stock_val
      consumpt_posting = ls_material_components-consumpt_posting
      accass_category = ls_material_components-accass_category
      unit_of_measure = ls_material_components-unit_of_measure "Added by James Sung-Kon Kim 2011.02.21
    INTO ls_rev_mat_components.


* ... Cancel entry has been found
    IF sy-subrc EQ 0.

***--- Start; Added by James Sung-Kon Kim 2011/02/21
      CLEAR ls_reppoints_flag.
      READ TABLE lt_reppoints_flag WITH KEY
          reppoint = ls_material_components-reppoint
      INTO ls_reppoints_flag.

      IF sy-subrc EQ 0.
        CASE ls_reppoints_flag-case.
          WHEN 'NORMAL'.
            ls_material_components-quantity =
            ls_material_components-quantity -
            ls_rev_mat_components-quantity.

          WHEN 'EQUAL' OR 'ONLY_FOWARD'.
            ls_material_components-quantity =
            ls_material_components-quantity - 0.

          WHEN 'ONLY_REVERSE'.  "Impossible
            ls_material_components-quantity =
            ls_rev_mat_components-quantity.
        ENDCASE.
      ENDIF.

**          IF ls_material_components-quantity >  " Commentated by Sung-Kon James Kim 2010.09.15
**             ls_rev_mat_components-quantity.    " Commentated by Sung-Kon James Kim 2010.09.15
*              ls_material_components-quantity =     " Commentated by Sung-Kon James Kim 2011.02.21
*              ls_material_components-quantity -     " Commentated by Sung-Kon James Kim 2011.02.21
*              ls_rev_mat_components-quantity.       " Commentated by Sung-Kon James Kim 2011.02.21
**          ELSE.                                 " Commentated by Sung-Kon James Kim 2010.09.15
**             ls_material_components-quantity = 0.  " Commentated by Sung-Kon James Kim 2010.09.15
**          ENDIF.                                " Commentated by Sung-Kon James Kim 2010.09.15
***--- End; Added by James Sung-Kon Kim 2011/02/21
    ENDIF.


* ... (2) Search a corresponding variance entry
    READ TABLE lt_mat_comp_var WITH KEY
      reppoint = ls_material_components-reppoint
      mat_number = ls_material_components-mat_number
      plant = ls_material_components-plant
      storage_loc = ls_material_components-storage_loc
      batch = ls_material_components-batch
      supply_area = ls_material_components-supply_area
      special_stock = ls_material_components-special_stock
      sales_doc = ls_material_components-sales_doc
      sales_doc_item = ls_material_components-sales_doc_item
      wbs_elem = ls_material_components-wbs_elem
      costing_num = ls_material_components-costing_num
      special_stock_val = ls_material_components-special_stock_val
      consumpt_posting = ls_material_components-consumpt_posting
      accass_category = ls_material_components-accass_category
      unit_of_measure = ls_material_components-unit_of_measure "Added by James Sung-Kon Kim 2011.02.21
    INTO ls_mat_comp_var.


* ... Variance entry has been found
    IF sy-subrc EQ 0.
***--- Start; Added by James Sung-Kon Kim 2011/02/21
      CASE ls_reppoints_flag-case.
        WHEN 'NORMAL' OR 'ONLY_FOWARD'.
          MOVE ls_mat_comp_var-quantity
            TO ls_material_components-delta_quantity.

        WHEN 'EQUAL'.
          MOVE ls_mat_comp_var-quantity
            TO ls_material_components-delta_quantity.

        WHEN 'ONLY_REVERSE'. "Impossible
*              MOVE ls_mat_comp_var-quantity
*                TO ls_material_components-delta_quantity.
      ENDCASE.
***--- End; Added by James Sung-Kon Kim 2011/02/21

*        MOVE ls_mat_comp_var-quantity                 " Commentated by Sung-Kon James Kim 2011.02.21
*          TO ls_material_components-delta_quantity.   " Commentated by Sung-Kon James Kim 2011.02.21
    ENDIF.


* ... (3) Search a corresponding variance entry (cancel)
    READ TABLE lt_rev_mat_comp_var WITH KEY
      reppoint = ls_material_components-reppoint
      mat_number = ls_material_components-mat_number
      plant = ls_material_components-plant
      storage_loc = ls_material_components-storage_loc
      batch = ls_material_components-batch
      supply_area = ls_material_components-supply_area
      special_stock = ls_material_components-special_stock
      sales_doc = ls_material_components-sales_doc
      sales_doc_item = ls_material_components-sales_doc_item
      wbs_elem = ls_material_components-wbs_elem
      costing_num = ls_material_components-costing_num
      special_stock_val = ls_material_components-special_stock_val
      consumpt_posting = ls_material_components-consumpt_posting
      accass_category = ls_material_components-accass_category
      unit_of_measure = ls_material_components-unit_of_measure "Added by James Sung-Kon Kim 2011.02.21
    INTO ls_rev_mat_comp_var.


* ... Variance entry (cancel) has been found
    IF sy-subrc EQ 0.
***--- Start; Added by James Sung-Kon Kim 2011/02/21
      CASE ls_reppoints_flag-case.
        WHEN 'NORMAL'.
          ls_material_components-delta_quantity =
          ls_material_components-delta_quantity -
          ls_rev_mat_comp_var-quantity.

        WHEN 'EQUAL' OR 'ONLY_FOWARD'.
          ls_material_components-delta_quantity =
          ls_material_components-delta_quantity - 0.

        WHEN 'ONLY_REVERSE'. "Impossible
          ls_material_components-delta_quantity =
          ls_rev_mat_comp_var-quantity.
      ENDCASE.
***--- End; Added by James Sung-Kon Kim 2011/02/21

*        ls_material_components-delta_quantity =  " Commentated by Sung-Kon James Kim 2011.02.21
*        ls_material_components-delta_quantity -  " Commentated by Sung-Kon James Kim 2011.02.21
*        ls_rev_mat_comp_var-quantity.            " Commentated by Sung-Kon James Kim 2011.02.21

    ENDIF.


* ... Reduce the component quantities in proportion to the head quantity
* ... ( head quantity = reporting point quantity )
    CLEAR ls_reppoints.
    READ TABLE lt_reppoints INTO ls_reppoints
    WITH KEY reppoint = ls_material_components-reppoint.

    IF sy-subrc = 0. " Added by Sung-Kon James Kim 2010.09.15
*      IF ls_reppoints-quantity GT 0. " Commentated by Sung-Kon James Kim 2010.09.15
      IF ls_reppoints-quantity NE 0. " Added by Sung-Kon James Kim 2010.09.15
        " Cannot delete because Avoiding devision by ZERO 2011/02/21
        ls_material_components-quantity =
        ls_material_components-quantity *
*      if_headquant /                                      "C5024598 del
        ls_reppoints-head_quantity /                        "C5024598
        ls_reppoints-quantity.
        ls_material_components-delta_quantity =
        ls_material_components-delta_quantity *
*      if_headquant /                                      "C5024598 del
        ls_reppoints-head_quantity /                        "C5024598
        ls_reppoints-quantity.
      ELSE.
        ls_material_components-quantity = 0.
        ls_material_components-delta_quantity = 0.
      ENDIF.
    ENDIF. " Added by Sung-Kon James Kim 2010.09.15

* ... Determine the movement type
    IF ( ls_material_components-sales_doc IS INITIAL AND
         ls_material_components-sales_doc_item IS INITIAL AND
         ls_material_components-wbs_elem IS INITIAL ) OR
       ( NOT ls_material_components-special_stock_val IS INITIAL ).
      MOVE 'X' TO ls_flag_pkosa.
    ENDIF.

    IF if_bw_call IS INITIAL.
      CALL FUNCTION 'PPC1PR_BWART_DET'
        EXPORTING
          if_flg_reversal       = space
          if_gmove_ind          = ls_material_components-gmove_ind
          if_pkosa              = ls_flag_pkosa
          if_kzvbr              = ls_material_components-consumpt_posting
          if_profile            = ls_rmprofile
        IMPORTING
          ef_bwart              = ls_material_components-movement_type
        EXCEPTIONS
          input_profile_missing = 1
          profile_not_found     = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING get_comp_error.
      ENDIF.


* ... Determine the debit credit indicator
      IF NOT ls_material_components-movement_type IS INITIAL.
        CALL FUNCTION 'PPC1PR_SHKZG_DET'
          EXPORTING
            if_bwart       = ls_material_components-movement_type
          IMPORTING
            ef_shkzg       = ls_material_components-debit_credit_ind
          EXCEPTIONS
            t156_not_found = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                  RAISING get_comp_error.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT ( ls_material_components-quantity IS INITIAL AND
             ls_material_components-delta_quantity IS INITIAL ).
      MODIFY et_material_components FROM ls_material_components.
    ELSE.
      DELETE et_material_components INDEX ls_tabix.
    ENDIF.
  ENDLOOP.


* ... ----------------------------------
* ... (B) handle the activity components
* ... ----------------------------------
* ... Sort the internal tables to be able
* ... to use the 'WITH KEY' statement
  SORT lt_rev_act_comp BY reppoint
                          ressource_guid
                          mode_guid  ".
*                          mode_aennr
*                          res_counter.
                          durunit. "Added by James Sung-Kon Kim 2011/02/21

  SORT lt_act_comp_var BY reppoint
                          ressource_guid
                          mode_guid ".
*                          mode_aennr
*                          res_counter.
                          durunit. "Added by James Sung-Kon Kim 2011/02/21

  SORT lt_rev_act_comp_var BY reppoint
                              ressource_guid
                              mode_guid ".
*                              mode_aennr
*                              res_counter.
                          durunit. "Added by James Sung-Kon Kim 2011/02/21

* ... Compare the tables
  LOOP AT et_activity_components INTO ls_activity_components.
    ls_tabix = sy-tabix.

* ... Search corresponding entries in the other internal tables
* ... (1) search cancel entry
* ... (2) search variance entry
* ... (3) search variance entry (cancel)

* ... (1) Search a corresponding cancel entry
    READ TABLE lt_rev_act_comp WITH KEY
      reppoint = ls_activity_components-reppoint
      ressource_guid = ls_activity_components-ressource_guid
      mode_guid = ls_activity_components-mode_guid
*      mode_aennr = ls_activity_components-mode_aennr
*      res_counter = ls_activity_components-res_counter
      durunit = ls_activity_components-durunit "Added by James Sung-Kon Kim 2011/02/21
    INTO ls_rev_act_comp.


* ... Cancel entry has been found
    IF sy-subrc EQ 0.
***--- Start; Added by James Sung-Kon Kim 2011/02/21
      CLEAR ls_reppoints_flag.
      READ TABLE lt_reppoints_flag WITH KEY
          reppoint = ls_activity_components-reppoint
      INTO ls_reppoints_flag.

      IF sy-subrc EQ 0.
        CASE ls_reppoints_flag-case.
          WHEN 'NORMAL'.
            ls_activity_components-duration_var =
            ls_activity_components-duration_var -
            ls_rev_act_comp-duration_var.

            ls_activity_components-duration_fix =
            ls_activity_components-duration_fix -
            ls_rev_act_comp-duration_fix.

          WHEN 'EQUAL' OR 'ONLY_FOWARD'.
            ls_activity_components-duration_var =
            ls_activity_components-duration_var -
            0.

            ls_activity_components-duration_fix =
            ls_activity_components-duration_fix -
            0.

          WHEN 'ONLY_REVERSE'. "Impossible
            ls_activity_components-duration_var =
            ls_rev_act_comp-duration_var.

            ls_activity_components-duration_fix =
            ls_rev_act_comp-duration_fix.

        ENDCASE.
      ENDIF.
***--- End; Added by James Sung-Kon Kim 2011/02/21
**      IF ls_activity_components-duration_var >    " Commentated by Sung-Kon James Kim 2010.09.15
**         ls_rev_act_comp-duration_var.            " Commentated by Sung-Kon James Kim 2010.09.15
*      ls_activity_components-duration_var =      " Commentated by Sung-Kon James Kim 2011.02.21
*      ls_activity_components-duration_var -      " Commentated by Sung-Kon James Kim 2011.02.21
*      ls_rev_act_comp-duration_var.              " Commentated by Sung-Kon James Kim 2011.02.21
**      ELSE.                                       " Commentated by Sung-Kon James Kim 2010.09.15
**        ls_activity_components-duration_var = 0.  " Commentated by Sung-Kon James Kim 2010.09.15
**      ENDIF.                                      " Commentated by Sung-Kon James Kim 2010.09.15
**      IF ls_activity_components-duration_fix >    " Commentated by Sung-Kon James Kim 2010.09.15
**         ls_rev_act_comp-duration_fix.            " Commentated by Sung-Kon James Kim 2010.09.15
*      ls_activity_components-duration_fix =      " Commentated by Sung-Kon James Kim 2011.02.21
*      ls_activity_components-duration_fix -      " Commentated by Sung-Kon James Kim 2011.02.21
*      ls_rev_act_comp-duration_fix.              " Commentated by Sung-Kon James Kim 2011.02.21
**      ELSE.                                       " Commentated by Sung-Kon James Kim 2010.09.15
**        ls_activity_components-duration_fix = 0.  " Commentated by Sung-Kon James Kim 2010.09.15
**      ENDIF.                                      " Commentated by Sung-Kon James Kim 2010.09.15
    ENDIF.


* ... (2) Search a corresponding variance entry
    READ TABLE lt_act_comp_var WITH KEY
      reppoint = ls_activity_components-reppoint
      ressource_guid = ls_activity_components-ressource_guid
      mode_guid = ls_activity_components-mode_guid
*      mode_aennr = ls_activity_components-mode_aennr
*      res_counter = ls_activity_components-res_counter
      durunit = ls_activity_components-durunit "Added by James Sung-Kon Kim 2011/02/21
    INTO ls_act_comp_var.


* ... Variance entry has been found
    IF sy-subrc EQ 0.
***--- Start; Added by James Sung-Kon Kim 2011/02/21
      CASE ls_reppoints_flag-case.
        WHEN 'NORMAL' OR 'ONLY_FOWARD'.
          MOVE ls_act_comp_var-duration_var
            TO ls_activity_components-delta_dur_var.
          MOVE ls_act_comp_var-duration_fix
            TO ls_activity_components-delta_dur_fix.

        WHEN 'EQUAL'.
          MOVE ls_act_comp_var-duration_var
            TO ls_activity_components-delta_dur_var.
          MOVE ls_act_comp_var-duration_fix
            TO ls_activity_components-delta_dur_fix.

        WHEN 'ONLY_REVERSE'. "Impossible
*          MOVE ls_act_comp_var-duration_var
*            TO ls_activity_components-delta_dur_var.
*          MOVE ls_act_comp_var-duration_fix
*            TO ls_activity_components-delta_dur_fix.
      ENDCASE.
***--- End; Added by James Sung-Kon Kim 2011/02/21
*          MOVE ls_act_comp_var-duration_var           " Commentated by Sung-Kon James Kim 2011.02.21
*            TO ls_activity_components-delta_dur_var.  " Commentated by Sung-Kon James Kim 2011.02.21
*          MOVE ls_act_comp_var-duration_fix           " Commentated by Sung-Kon James Kim 2011.02.21
*            TO ls_activity_components-delta_dur_fix.  " Commentated by Sung-Kon James Kim 2011.02.21
    ENDIF.


* ... (3) Search a corresponding variance entry (cancel)
    READ TABLE lt_rev_act_comp_var WITH KEY
      reppoint = ls_activity_components-reppoint
      ressource_guid = ls_activity_components-ressource_guid
      mode_guid = ls_activity_components-mode_guid
*      mode_aennr = ls_activity_components-mode_aennr
*      res_counter = ls_activity_components-res_counter
      durunit = ls_activity_components-durunit "Added by James Sung-Kon Kim 2011/02/21
    INTO ls_rev_act_comp_var.


* ... Variance entry (cancel) has been found
    IF sy-subrc EQ 0.

***--- Start; Added by James Sung-Kon Kim 2011/02/21
      CASE ls_reppoints_flag-case.
        WHEN 'NORMAL'.
          ls_activity_components-delta_dur_var =
          ls_activity_components-delta_dur_var -
          ls_rev_act_comp_var-duration_var.
          ls_activity_components-delta_dur_fix =
          ls_activity_components-delta_dur_fix -
          ls_rev_act_comp_var-duration_fix.

        WHEN 'EQUAL' OR 'ONLY_FOWARD'.
          ls_activity_components-delta_dur_var =
          ls_activity_components-delta_dur_var -
          0.
          ls_activity_components-delta_dur_fix =
          ls_activity_components-delta_dur_fix -
          0.

        WHEN 'ONLY_REVERSE'. "Impossible
          ls_activity_components-delta_dur_var =
          ls_rev_act_comp_var-duration_var.
          ls_activity_components-delta_dur_fix =
          ls_rev_act_comp_var-duration_fix.

      ENDCASE.
***--- End; Added by James Sung-Kon Kim 2011/02/21

*      ls_activity_components-delta_dur_var =   " Commentated by Sung-Kon James Kim 2011.02.21
*      ls_activity_components-delta_dur_var -   " Commentated by Sung-Kon James Kim 2011.02.21
*      ls_rev_act_comp_var-duration_var.        " Commentated by Sung-Kon James Kim 2011.02.21
*      ls_activity_components-delta_dur_fix =   " Commentated by Sung-Kon James Kim 2011.02.21
*      ls_activity_components-delta_dur_fix -   " Commentated by Sung-Kon James Kim 2011.02.21
*      ls_rev_act_comp_var-duration_fix.        " Commentated by Sung-Kon James Kim 2011.02.21
    ENDIF.


* ... Reduce the component quantities in proportion to the head quantity
* ... ( head quantity = reporting point quantity )
    CLEAR ls_reppoints.
    READ TABLE lt_reppoints INTO ls_reppoints
    WITH KEY reppoint = ls_activity_components-reppoint.

    IF sy-subrc = 0.                                  " Added by Sung-Kon James Kim 2010.09.15
*    IF ls_reppoints-quantity GT 0.                   " Commentated by Sung-Kon James Kim 2010.09.15
      IF ls_reppoints-quantity NE 0.                  " Added by Sung-Kon James Kim 2010.09.15
        " Cannot delete because Avoiding devision by ZERO 2011/02/21
        ls_activity_components-duration_var =
        ls_activity_components-duration_var *
*      if_headquant /                                      "C5024598 del
        ls_reppoints-head_quantity /                        "C5024598
        ls_reppoints-quantity.
        ls_activity_components-duration_fix =
        ls_activity_components-duration_fix *
*      if_headquant /                                      "C5024598 del
        ls_reppoints-head_quantity /                        "C5024598
        ls_reppoints-quantity.
        ls_activity_components-delta_dur_var =
        ls_activity_components-delta_dur_var *
*      if_headquant /                                      "C5024598 del
        ls_reppoints-head_quantity /                        "C5024598
        ls_reppoints-quantity.
        ls_activity_components-delta_dur_fix =
        ls_activity_components-delta_dur_fix *
*      if_headquant /                                      "C5024598 del
        ls_reppoints-head_quantity /                        "C5024598
        ls_reppoints-quantity.
      ELSE.
        ls_activity_components-duration_var = 0.
        ls_activity_components-duration_fix = 0.
        ls_activity_components-delta_dur_var = 0.
        ls_activity_components-delta_dur_fix = 0.
      ENDIF.
    ENDIF.  " Added by Sung-Kon James Kim 2010.09.15


    IF NOT ( ls_activity_components-duration_var IS INITIAL AND
             ls_activity_components-duration_fix IS INITIAL AND
             ls_activity_components-delta_dur_var IS INITIAL AND
             ls_activity_components-delta_dur_fix IS INITIAL ).
      MODIFY et_activity_components FROM ls_activity_components.
    ELSE.
      DELETE et_activity_components INDEX ls_tabix.
    ENDIF.
  ENDLOOP.

**** Start; Added by James Sung-Kon Kim 2011.03.02
  cf_orderid = if_orderid.
  c_gjper    = i_date_high(4) * 1000 + i_date_high+4(2).
****** End; Added by James Sung-Kon Kim 2011.03.02

ENDFUNCTION.
