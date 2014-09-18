FUNCTION z_fpp_order_change.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_WERKS) LIKE  T001W-WERKS DEFAULT 'P001'
*"     VALUE(I_ZTPPVR) LIKE  ZSPPVR STRUCTURE  ZSPPVR
*"     REFERENCE(I_SD) TYPE  C
*"  TABLES
*"      I_RETURN STRUCTURE  ZSPP_FUNC_MESSAGE
*"  EXCEPTIONS
*"      NO_EQUI
*"      ERROR_BEFORE_STATUS
*"      NO_ZTPP_WOSUM
*"      NO_ZTPP_BFST
*"      ERROR_TYPE
*"      ERROR_SAVE
*"----------------------------------------------------------------------
  TABLES equi.

  DATA : wa_datum(10),
         wa_uzeit    TYPE  sy-uzeit.
  DATA : wa_mitu.
  DATA : wa_max_nb   TYPE  ztpp_change-serial,
         wa_actual_datetime(14),
         wa_update_flg.

  CLEAR : equi, it_vmread,   it_vmread[],
          it_vmupdate, it_vmupdate[],
          i_return,    i_return[].

  CLEAR : i_return-message.
  GET TIME FIELD wa_uzeit.
  WRITE sy-datum  TO  wa_datum.
  WRITE wa_uzeit  TO  i_return-message.
  CONCATENATE 'START/' wa_datum i_return-message
              INTO i_return-message SEPARATED BY space.
  i_return-result_type = 'S'.
  APPEND i_return.

*> Check Vehicle Master Number
  CONCATENATE i_ztppvr-p_model i_ztppvr-p_body_serial INTO wa_equnr.

  MOVE wa_equnr  TO  it_ret-vm_no.
  MOVE wa_equnr  TO  i_return-vm_no.

  SELECT SINGLE *
              FROM equi
              WHERE equnr EQ wa_equnr
                AND eqtyp EQ 'V'.
  IF sy-subrc NE 0.
    i_return-message = 'Vehicle Master does not exists.'.
    i_return-result_type = 'E'.
    APPEND i_return.
    RAISE no_equi.
  ENDIF.

*> Check Plant
  IF i_werks IS INITIAL.
    MOVE  'P001'    TO    wa_werks .
  ELSE.
    MOVE  i_werks   TO    wa_werks .
  ENDIF.

  CLEAR : wa_bcstat_tx, wa_bcstat_nb, wa_custat_tx,
          wa_custat_nb, wa_rp_text, wa_rp_date.

  CLEAR : ztpp_wosum, wa_wo_ser, wa_nation,
          wa_dealer, ztpp_change.

*> Before status from P_VM
  PERFORM select_classification USING 'VM' 'P_STATUS' wa_bcstat_tx.
  PERFORM select_rp_number USING wa_bcstat_tx
                           CHANGING wa_bcstat_nb wa_bcwo_nb.
*> Current status
  MOVE   i_ztppvr-p_status   TO  wa_custat_tx.
  PERFORM select_rp_number USING wa_custat_tx
                           CHANGING wa_custat_nb wa_cuwo_nb.

*> Planned Order from P_VM
  PERFORM select_classification USING 'VM' 'P_PLAN_ORDER' wa_plnum.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = wa_plnum
       IMPORTING
            output = wa_plnum.
  MOVE wa_plnum    TO  i_return-plan_no.

  CONCATENATE 'Vehicle Master Before RP =' wa_bcstat_nb ','
              'Vehicle Master After RP ='  wa_custat_nb ','
              'W/O Before RP =' wa_bcwo_nb ','
              'W/O After RP ='  wa_cuwo_nb INTO i_return-message
              SEPARATED BY space.
  i_return-result_type = 'S'.
  APPEND i_return.

  IF wa_bcstat_nb LT wa_custat_nb.
    CONCATENATE 'Error Change Status : Before -'
                wa_bcstat_nb
                ', After -'
                wa_custat_nb
                INTO i_return-message SEPARATED BY space.
    i_return-result_type = 'E'.
    APPEND i_return.
    RAISE error_before_status.
  ENDIF.

*> Before Actual date & Before Shop date from P_VM
  IF wa_bcstat_nb NE '00'.
    CONCATENATE 'P_RP' wa_bcstat_nb '_ACTUAL_DATE' INTO wa_rp_text.
    PERFORM select_classification USING 'VM' wa_rp_text wa_rp_date.
    CONCATENATE 'P_RP' wa_bcstat_nb '_SHOP_DATE'   INTO wa_shop_tx.
    PERFORM select_classification USING 'VM' wa_shop_tx wa_shop_date.
  ENDIF.
  wa_wo_ser = i_ztppvr-p_work_order+0(9).   "WORK ORDER SERIAL
  wa_nation = i_ztppvr-p_dest_code+0(3).    "NATION
  wa_dealer = i_ztppvr-p_dest_code+3(2).    "DEALER
  wa_extc   = i_ztppvr-p_ext_color.         "EXT COLOR
  wa_intc   = i_ztppvr-p_int_color.         "INT COLOR

*> Workorder Header to Search P_WOHD
  CONCATENATE i_ztppvr-p_work_order+0(9)
              i_ztppvr-p_dest_code   INTO  wa_wohd.
  MOVE wa_wohd                 TO  i_return-woh_no.
*> Workorder Color to Search P_WOCL
  CONCATENATE i_ztppvr-p_work_order+0(9)
              i_ztppvr-p_dest_code
              i_ztppvr-p_ext_color
              i_ztppvr-p_int_color  INTO wa_wocl.
  MOVE wa_wocl                 TO  i_return-woc_no.

*---------------> PROGRESS CHANGE (LINE BACK)
  IF i_ztppvr-flag EQ 'LP'.

*---> Change Vehicle Master
*> CHANGE BEFORE STATUS of P_VM
    CLEAR it_vmupdate.
    PERFORM change_vehicle_master USING 'P_BC_RP'
                                        wa_bcstat_tx.
*> CHANGE CURRENT STATUS of P_VM
    PERFORM change_vehicle_master USING 'P_STATUS'
                                        wa_custat_tx.

*> Change current RP status of P_VM
    PERFORM change_vehicle_master USING 'P_RP_STATUS'
                                        wa_custat_nb.

*>  Insert Change Date Information  of P_VM
    PERFORM change_vehicle_master USING 'P_BC_CHANGE_DATE'
                                         i_ztppvr-k04pdat .

*> CHANGE P_RPxx_ACTUAL_DATE, P_RPxx_SHOP_DATE, P_RPxx_SERIAL of P_VM
    CONCATENATE i_ztppvr-p_rp_actual_date i_ztppvr-p_rp_actual_time
                INTO wa_actual_datetime.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              input  = i_ztppvr-p_rp_serial
         IMPORTING
              output = i_ztppvr-p_rp_serial.

    IF i_sd = 'X'.       " Processing the SD Line-Back..
      DATA: l_fname(40),
            l_num(2)    TYPE   n.
      l_num = wa_bcstat_nb .
      CONCATENATE 'P_RP'   l_num  '_ACTUAL_DATE' INTO l_fname.
      PERFORM change_vehicle_master USING l_fname space.
      CONCATENATE 'P_RP'   l_num  '_SERIAL'      INTO l_fname.
      PERFORM change_vehicle_master USING l_fname space.
      CONCATENATE 'P_RP'   l_num  '_SHOP_DATE'   INTO l_fname.
      PERFORM change_vehicle_master USING l_fname space.
    ELSE.                " Normal Line-Back
      PERFORM change_rp_date USING 'P_RP'
                                   wa_custat_nb
                                   wa_actual_datetime
                                   i_ztppvr-p_rp_serial.
    ENDIF.

*---> Change Work order summary table
    IF  wa_cuwo_nb EQ '00' AND wa_custat_nb NE '00'.
    ELSE.
      SELECT SINGLE *
        FROM ztpp_wosum
       WHERE wo_ser  EQ wa_wo_ser    "WO SER
         AND nation  EQ wa_nation    "NATION
         AND dealer  EQ wa_dealer    "DEALER
         AND extc    EQ wa_extc      "EXT COLOR
         AND intc    EQ wa_intc.     "INT COLOR
      IF sy-subrc NE 0.
        it_ret-result_type = 'E'.
        MOVE text-020 TO  it_ret-message .
        MOVE : it_ret-result_type   TO   i_return-result_type,
               it_ret-message       TO   i_return-message.
        APPEND i_return.
        RAISE no_ztpp_wosum.
      ENDIF.
      IF i_sd = 'X'.
        CONCATENATE 'ZTPP_WOSUM-RP' wa_bcwo_nb 'TQ' INTO l_fname.
        ASSIGN (l_fname) TO <f_rp>.
        <f_rp> = <f_rp> - 1.
      ELSE.
        PERFORM change_ztpp_wosum USING i_ztppvr-flag.  " I_ZTPPVR-DVRS.
        IF it_ret-result_type EQ 'E'.
          MOVE : it_ret-result_type   TO   i_return-result_type,
                 it_ret-message       TO   i_return-message.
          APPEND i_return.
          RAISE no_ztpp_wosum.
        ENDIF.
      ENDIF.
    ENDIF.

*-----> Order change history
    MOVE 'B'    TO   ztpp_change-cflag.  "Line Back
    CONCATENATE i_ztppvr-p_model i_ztppvr-p_body_serial
                INTO ztpp_change-bodyno.
*> Change Request Date
    MOVE : i_ztppvr-p_rp_actual_date  TO ztpp_change-cdate,
           i_ztppvr-p_rp_serial       TO ztpp_change-serial.

    MOVE :
*     I_ZTPPVR-DVRS         TO ZTPP_CHANGE-CHG_TYP,  "Change type (A,M)
     wa_wo_ser             TO ztpp_change-ordno,    "Work Order No(NEW)
     wa_nation             TO ztpp_change-nation,   "Nation(NEW)
     wa_dealer             TO ztpp_change-dealer,   "Dealer(NEW)
    i_ztppvr-p_ext_color   TO ztpp_change-extc,     "External Color(NEW)
    i_ztppvr-p_int_color   TO ztpp_change-intc,     "Internal Color(NEW)
    i_ztppvr-p_vin(11)     TO ztpp_change-vin,      "VIN No(NEW)
    wa_custat_nb           TO ztpp_change-sta,      "Status RP(NEW)
    wa_custat_tx           TO ztpp_change-crp.      "Status(NEW)

*> MODEL YEAR
    PERFORM select_classification USING 'VM' 'P_MODEL_YEAR'
                 ztpp_change-moyr. "Model Year(NEW)
*> MODEL INDEX
    PERFORM select_classification USING 'VM' 'P_MI'
                 ztpp_change-mi.   "Model Index(NEW)
*> OCN
    PERFORM select_classification USING 'VM' 'P_OCN'
                 ztpp_change-ocn.  "Option Combination Number(NEW)
*> PROD VERSION
    PERFORM select_classification USING 'VM' 'P_VERSION'
                 ztpp_change-vers. "Version(NEW)
*> BODY IN DATE
    IF wa_bcstat_nb GE '01'.
      PERFORM select_classification USING 'VM' 'P_RP01_ACTUAL_DATE'
                   ztpp_change-bodyin_dat. "Body in date
    ENDIF.
*> TRIM IN DATE
    IF wa_bcstat_nb GE '07'.
      PERFORM select_classification USING 'VM' 'P_RP07_ACTUAL_DATE'
                   ztpp_change-trimin_dat. "Trim in date
    ENDIF.
*> SIGN OFF DATE
    IF wa_bcstat_nb GE '18'.
      PERFORM select_classification USING 'VM' 'P_RP18_ACTUAL_DATE'
                   ztpp_change-soff_dat.   "Sign off date
    ENDIF.

*> BEFORE
    MOVE :
    ztpp_change-ordno      TO ztpp_change-b_ordno,  "Work Order No(OLD)
    ztpp_change-nation     TO ztpp_change-b_nation, "Nation(OLD)
    ztpp_change-dealer     TO ztpp_change-b_dealer, "Dealer(OLD)
    ztpp_change-extc       TO ztpp_change-b_extc,   "External Color(OLD)
    ztpp_change-intc       TO ztpp_change-b_intc,   "Internal Color(OLD)
    ztpp_change-vin        TO ztpp_change-b_vin,    "VIN No(OLD)
    wa_bcstat_nb           TO ztpp_change-b_sta,    "Before Status RP
    wa_bcstat_tx           TO ztpp_change-b_rp.     "Status(OLD)

    MOVE :
      ztpp_change-moyr       TO ztpp_change-b_moyr,   "Model Year(OLD)
      ztpp_change-mi         TO ztpp_change-b_mi,     "Model Index(OLD)
      ztpp_change-ocn        TO ztpp_change-b_ocn,    "OCN Number(OLD)
      ztpp_change-vers       TO ztpp_change-b_vers.   "Version(OLD)
    IF NOT wa_shop_date IS INITIAL.
      "Before RP Shop date
      MOVE wa_shop_date(8)   TO ztpp_change-b_rp_shopdat.
    ENDIF.
    IF NOT wa_rp_date IS INITIAL.
      "Before RP Act date
      MOVE wa_rp_date(12)    TO ztpp_change-b_rp_actdat.
    ENDIF.

*---> Change Backflush Status
    IF i_sd NE 'X'.
      PERFORM change_ztpp_bfst USING i_ztppvr-p_model
                                     i_ztppvr-p_body_serial
                               CHANGING wa_update_flg.
    ENDIF.
*---------------> MITU CHANGE
  ELSEIF i_ztppvr-flag EQ 'LM'.

*> Check P_MITU is already 'Y'
    PERFORM select_classification USING 'VM' 'P_MITU'
                 wa_mitu. "Model Year(NEW)
    IF wa_mitu EQ 'Y'.
      i_return-result_type = 'E'.
      CONCATENATE'''P_MITU''' wa_equnr 'is already ''Y'''
         INTO i_return-message SEPARATED BY space.
      APPEND i_return.
      RAISE error_type.
    ENDIF.
*-----> Order change history
    MOVE 'Y'    TO   ztpp_change-mitu.
    MOVE 'M'    TO   ztpp_change-cflag.
    CONCATENATE i_ztppvr-p_model i_ztppvr-p_body_serial
                INTO ztpp_change-bodyno.
*> Change Request Date
    MOVE : i_ztppvr-p_rp_actual_date  TO ztpp_change-cdate,
           i_ztppvr-p_rp_serial       TO ztpp_change-serial.

*-----> Change Vehicle Master
*> CHANGE STATUS of P_VM
    CLEAR it_vmupdate.
    PERFORM change_vehicle_master USING 'P_STATUS'
                                        wa_custat_tx.

*> Change current RP status of P_VM
    PERFORM change_vehicle_master USING 'P_RP_STATUS'
                                        wa_custat_nb.

*> CHANGE P_MITU='Y' of P_VM
    PERFORM change_vehicle_master USING 'P_MITU'
                                        'Y'.
*> CHANGE P_MITU_DATE of P_VM
    PERFORM change_vehicle_master USING 'P_MITU_DATE'
*                                        I_ZTPPVR-K04PDAT.
                                        i_ztppvr-p_rp_actual_date.

*---> Change Work order summary table
    SELECT SINGLE *
                FROM ztpp_wosum
                WHERE wo_ser  EQ wa_wo_ser    "WO SER
                  AND nation  EQ wa_nation    "NATION
                  AND dealer  EQ wa_dealer    "DEALER
                  AND extc    EQ wa_extc      "EXT COLOR
                  AND intc    EQ wa_intc.     "INT COLOR
    IF sy-subrc NE 0.
      it_ret-result_type = 'E'.
      MOVE text-020 TO  it_ret-message .
      MOVE : it_ret-result_type   TO   i_return-result_type,
             it_ret-message       TO   i_return-message.
      APPEND i_return.
      RAISE no_ztpp_wosum.
    ELSE.
      PERFORM change_ztpp_wosum USING i_ztppvr-flag.
    ENDIF.

*---> Change Workorder header & Workorder color
    CLEAR : it_wohd, it_wocl, it_wohd[], it_wocl[].
    PERFORM change_wohd_wocl USING 'WOHD'.
    PERFORM change_wohd_wocl USING 'WOCL'.
  ENDIF.

*-------> UPDATE DATA
*> Update Vehicle Master
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = wa_equnr
            mode         = 'W'
            ctype        = '002'
       TABLES
            val_table    = it_vmupdate
       EXCEPTIONS
            no_data      = 1
            error_mode   = 2
            error_object = 3
            error_value  = 4
            OTHERS       = 5.

  IF sy-subrc NE 0.
    i_return-result_type = 'E'.
    MOVE 'Vehicle Master is not Updated' TO i_return-message.
    APPEND i_return.
    RAISE error_save.
  ELSE.
    i_return-result_type = 'S'.
    MOVE 'Vehicle Master is Updated' TO i_return-message.
    APPEND i_return.

    IF i_ztppvr-flag EQ 'LM'.
*> Update Workorder Header
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = wa_wohd
                mode         = 'W'
                ctype        = '001'
           TABLES
                val_table    = it_wohd
           EXCEPTIONS
                no_data      = 1
                error_mode   = 2
                error_object = 3
                error_value  = 4
                OTHERS       = 5.

      IF sy-subrc EQ 0.
        i_return-result_type = 'S'.
        MOVE 'Workorder Header is Updated' TO i_return-message.
        APPEND i_return.
      ELSE.
        i_return-result_type = 'E'.
        MOVE 'Workorder Header is not Updated' TO i_return-message.
        APPEND i_return.
        RAISE error_save.
      ENDIF.
*> Update Workorder Color
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = wa_wocl
                mode         = 'W'
                ctype        = '001'
           TABLES
                val_table    = it_wocl
           EXCEPTIONS
                no_data      = 1
                error_mode   = 2
                error_object = 3
                error_value  = 4
                OTHERS       = 5.

      IF sy-subrc EQ 0.
        i_return-result_type = 'S'.
        MOVE 'Workorder Color is updated' TO i_return-message.
        APPEND i_return.
      ELSE.
        i_return-result_type = 'E'.
        MOVE 'Workorder Color is not updated' TO i_return-message.
        APPEND i_return.
        RAISE error_save.
      ENDIF.
    ENDIF.

    IF  wa_cuwo_nb EQ '00' AND wa_custat_nb NE '00'.
    ELSE.
*> Update Workorder summary
      ztpp_wosum-aedat = sy-datum.
      ztpp_wosum-aezet = sy-uzeit.
      ztpp_wosum-aenam = sy-uname.
      UPDATE ztpp_wosum .
      IF sy-subrc EQ 0.
        i_return-result_type = 'S'.
        MOVE 'Workorder summary table is Updated' TO i_return-message.
        APPEND i_return.
      ELSE.
        i_return-result_type = 'E'.
        MOVE 'Workorder summary table is not Updated'
               TO i_return-message.
        APPEND i_return.
        RAISE error_save.
      ENDIF.
    ENDIF.

    IF wa_update_flg EQ 'X'.
*> Update Backflush point status
      ztpp_bfst-aedat = sy-datum.
      ztpp_bfst-aezet = sy-uzeit.
      ztpp_bfst-aenam = sy-uname.
      UPDATE ztpp_bfst .
      IF sy-subrc EQ 0.
        i_return-result_type = 'S'.
        MOVE 'Backflush Status table is Updated' TO i_return-message.
        APPEND i_return.
      ELSE.
        i_return-result_type = 'E'.
        MOVE 'Backflush Status table is not Updated'
               TO i_return-message.
        APPEND i_return.
        RAISE error_save.
      ENDIF.
    ENDIF.

*> Insert Change log history
    MOVE : sy-datum TO ztpp_change-erdat,
           sy-uzeit TO ztpp_change-erzet,
           sy-uname TO ztpp_change-ernam.
    INSERT ztpp_change.
    IF sy-subrc EQ 0.
      i_return-result_type = 'S'.
      MOVE 'Change History table is Updated'
             TO i_return-message.
      APPEND i_return.
      CLEAR : i_return-message.
      GET TIME FIELD wa_uzeit.
      WRITE sy-datum  TO  wa_datum.
      WRITE wa_uzeit  TO  i_return-message.
      CONCATENATE 'END/' wa_datum i_return-message
                  INTO i_return-message SEPARATED BY space.
      i_return-result_type = 'S'.
      APPEND i_return.

    ELSE.
      i_return-result_type = 'E'.
      MOVE 'Change History table is not Updated'
             TO i_return-message.
      APPEND i_return.
      RAISE error_save.
    ENDIF.
  ENDIF.
ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  SELECT_CLASSIFICATION
*&---------------------------------------------------------------------*
FORM select_classification USING p_ind p_atnam p_atwrt.
  DATA : l_atinn   TYPE   ausp-atinn,
         l_atwrt   TYPE   ausp-atwrt,
         l_atflv   TYPE   ausp-atflv,
         l_atfor   TYPE   cabn-atfor,
         l_numeric(15) TYPE n,
         l_integer     TYPE i.

  CLEAR p_atwrt.
  SELECT SINGLE atinn
                atfor
                INTO (l_atinn, l_atfor)
            FROM cabn
            WHERE atnam EQ p_atnam.

  CASE p_ind.
    WHEN 'VM'.
      SELECT SINGLE atwrt
                    atflv
                  INTO (l_atwrt, l_atflv)
                  FROM ausp
                  WHERE objek EQ wa_equnr
                    AND atinn EQ l_atinn
                    AND klart EQ '002'.
    WHEN 'WOHD'.
      SELECT SINGLE atwrt
                    atflv
                  INTO (l_atwrt, l_atflv)
                  FROM ausp
                 WHERE objek EQ wa_wohd
                   AND atinn EQ l_atinn
                   AND klart EQ '001'.
    WHEN 'WOCL'.
      SELECT SINGLE atwrt
                    atflv
                  INTO (l_atwrt, l_atflv)
                  FROM ausp
                  WHERE objek EQ wa_wocl
                    AND atinn EQ l_atinn
                    AND klart EQ '001'.
  ENDCASE.

  IF l_atfor EQ 'CHAR'.
    CONDENSE l_atwrt.
    p_atwrt = l_atwrt.
  ELSE.
    l_numeric = l_atflv.
    l_integer = l_numeric.
    WRITE l_integer TO p_atwrt LEFT-JUSTIFIED NO-GROUPING.
  ENDIF.

ENDFORM.                    " SELECT_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ATINN
*&---------------------------------------------------------------------*
FORM conversion_exit_atinn.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = it_vmupdate-atnam
       IMPORTING
            output = it_vmupdate-atinn.
ENDFORM.                    " CONVERSION_EXIT_ATINN
*&---------------------------------------------------------------------*
*&      Form  CHANGE_VEHICLE_MASTER
*&---------------------------------------------------------------------*
FORM change_vehicle_master USING  p_atnam p_atwrt.
  MOVE  p_atnam    TO    it_vmupdate-atnam .
*  PERFORM CONVERSION_EXIT_ATINN.
  MOVE  p_atwrt    TO    it_vmupdate-atwrt .
  APPEND it_vmupdate. CLEAR it_vmupdate.
ENDFORM.                    " CHANGE_VEHICLE_MASTER

*&---------------------------------------------------------------------*
*&      Form  CHANGE_RP_QTTY
*&---------------------------------------------------------------------*
FORM change_rp_qtty USING p_fname_s
                          p_fname_e
                          p_endnum.
  DATA : l_fname(40),
         l_rpnum(2)    TYPE   n,
         l_num         TYPE   i,
         l_stop        TYPE   i.
  l_stop = p_endnum.
  l_num = wa_bcwo_nb + 1.
  DO.
    l_num = l_num - 1.
    IF l_num EQ l_stop.
      EXIT.
    ELSE.
      l_rpnum = l_num.
      CONCATENATE p_fname_s  l_rpnum  p_fname_e
                                   INTO l_fname.
      ASSIGN (l_fname) TO <f_rp>.
      <f_rp> = <f_rp> - 1.
    ENDIF.
  ENDDO.
ENDFORM.                    " CHANGE_RP_QTTY

*&---------------------------------------------------------------------*
*&      Form  CHANGE_RP_DATE
*&---------------------------------------------------------------------*
FORM change_rp_date USING p_fname
                          p_endnum
                          p_rp_actual_date
                          p_rp_serial.
  DATA : l_fname(40),
         l_rpnum(2)    TYPE   n,
         l_num         TYPE   i,
         l_stop        TYPE   i.
*  IF WA_BCSTAT_NB GT '00'.
  l_stop = p_endnum.
  l_num = wa_bcstat_nb + 1.

  DATA : l_atnam  LIKE zspp_vin_value-atnam.
  DO.
    l_num = l_num - 1.
    IF l_num EQ l_stop.
*---> TO RP00
      l_rpnum = l_num.
      EXIT.
    ELSE.
      l_rpnum = l_num.
      CONCATENATE p_fname  l_rpnum  '_ACTUAL_DATE' INTO l_fname.
      PERFORM change_vehicle_master USING l_fname space.
      CONCATENATE p_fname  l_rpnum  '_SERIAL'      INTO l_fname.
      PERFORM change_vehicle_master USING l_fname space.
      CONCATENATE p_fname  l_rpnum  '_SHOP_DATE'   INTO l_fname.
      PERFORM change_vehicle_master USING l_fname space.
    ENDIF.
  ENDDO.
ENDFORM.                    " CHANGE_RP_DATE

*&---------------------------------------------------------------------*
*&      Form  CHANGE_RP_STATUS
*&---------------------------------------------------------------------*
FORM change_rp_status  USING p_fname_s
                             p_fname_e
                             p_endnum.
  DATA : l_fname(40),
         l_rpnum(2)    TYPE   n,
         l_num         TYPE   i,
         l_stop        TYPE   i.
  l_stop = p_endnum.
*----> BFST POINT Greater than 18 point
  IF wa_bcstat_nb GT '18'.
    l_num = 19 .
  ELSE.
    l_num = wa_bcstat_nb + 1.
  ENDIF.
  DO.
    l_num = l_num - 1.
    IF l_num LE l_stop.   " L_NUM EQ L_STOP.
      EXIT.
    ELSE.
      l_rpnum = l_num.
      CONCATENATE p_fname_s  l_rpnum  p_fname_e
                                   INTO l_fname.
      ASSIGN (l_fname) TO <f_rp>.
      CASE <f_rp>.
        WHEN 'Y'.
          MOVE '09'  TO  <f_rp>.
        WHEN OTHERS.
*          MOVE '00'  TO  <F_RP>.
      ENDCASE.
    ENDIF.
  ENDDO.
ENDFORM.                    " CHANGE_RP_STATUS

*&---------------------------------------------------------------------*
*&      Form  CHANGE_ZTPP_WOSUM
*&---------------------------------------------------------------------*
FORM change_ztpp_wosum USING p_flag.
  IF p_flag EQ 'LM'.
*> Change MITU Qtty
    ztpp_wosum-mituqty = ztpp_wosum-mituqty + 1.
  ELSEIF p_flag EQ 'LP'.
*> Change RP Process present condition total
    PERFORM change_rp_qtty USING 'ZTPP_WOSUM-RP'
                                 'TQ'
                                 wa_cuwo_nb.
  ENDIF.
ENDFORM.                    " CHANGE_ZTPP_WOSUM

*&---------------------------------------------------------------------*
*&      Form  CHANGE_ZTPP_BFST
*&---------------------------------------------------------------------*
FORM change_ztpp_bfst USING p_model p_body_serial
                      CHANGING p_flag.
  CLEAR ztpp_bfst.
  SELECT SINGLE *
              FROM ztpp_bfst
              WHERE plant     EQ  wa_werks       "PLANT
                AND model     EQ  p_model        "MODEL
                AND body_ser  EQ  p_body_serial  "BODY SERIAL
                AND plan_ord  EQ  wa_plnum.      "PLANNED ORDER
  IF sy-subrc EQ 0.
*    RAISE NO_ZTPP_BFST.
*  ELSE.
    PERFORM change_rp_status USING 'ZTPP_BFST-BFP'
                                 '_FLG'
                                 wa_custat_nb.
*    PERFORM CHANGE_RP_STATUS USING 'ZTPP_BFST-BFA'
*                                 '_FLG'
*                                 WA_CUSTAT_NB.
    MOVE 'X'   TO   p_flag.
  ENDIF.
ENDFORM.                    " CHANGE_ZTPP_BFST
*&---------------------------------------------------------------------*
*&      Form  SELECT_RP_NUMBER
*&---------------------------------------------------------------------*
FORM select_rp_number USING    p_arbpl
                      CHANGING p_rp_nb p_wo_nb.
  DATA : l_vm_id     TYPE ztpp_status-vm_id,
         l_id_point  TYPE ztpp_status-id_point,
         l_rp(2).

*  CASE P_IND.
*    WHEN 'BEFORE'.
*      P_RP_NB = P_ARBPL.
*      CONCATENATE 'RP' P_RP_NB INTO L_VM_ID.
*      SELECT SINGLE ID_POINT
*                    INTO L_ID_POINT
*                    FROM ZTPP_STATUS
*                    WHERE ID EQ P_ARBPL.
*      P_WO_NB = L_ID_POINT.
*
*    WHEN 'AFTER'.
  SELECT SINGLE vm_id
                id_point
                INTO (l_vm_id, l_id_point)
                FROM ztpp_status
                WHERE id EQ p_arbpl.

  l_rp = l_vm_id+2(2).
  p_rp_nb = l_rp.
  p_wo_nb = l_id_point.
*  ENDCASE.
ENDFORM.                    " SELECT_RP_NUMBER

*&---------------------------------------------------------------------*
*&      Form  CHANGE_WOHD_WOCL
*&---------------------------------------------------------------------*
FORM change_wohd_wocl USING    p_ind.
  DATA : l_atwrt     TYPE  ausp-atwrt,
         l_atnum(5)  TYPE  n,
         l_atint     TYPE  i.

*> Mitu qty of WOHD or WOCL
  PERFORM select_classification USING p_ind 'P_MITU_QTY' l_atwrt.
  MOVE : l_atwrt      TO    l_atnum,
         l_atnum      TO    l_atint.
  l_atint = l_atint + 1.
  WRITE  l_atint  TO  l_atwrt NO-GROUPING LEFT-JUSTIFIED.

  CASE p_ind.
    WHEN 'WOHD'.
      MOVE  'P_MITU_QTY'    TO    it_wohd-atnam .
      MOVE  l_atwrt         TO    it_wohd-atwrt .
      APPEND it_wohd. CLEAR it_wohd.
    WHEN 'WOCL'.
      MOVE  'P_MITU_QTY'    TO    it_wocl-atnam .
      MOVE  l_atwrt         TO    it_wocl-atwrt .
      APPEND it_wocl. CLEAR it_wocl.
  ENDCASE.
ENDFORM.                    " CHANGE_WOHD_WOCL
