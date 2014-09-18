************************************************************************
* Program Name      : ZIPP102I_SEQ_JOB1
* Author            : Bobby
* Creation Date     : 2003.09.04.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No :
* Addl Documentation:
* Description       : Create the Vehicle Master(SUB PROGRAM)
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  zipp102i_seq_job1    LINE-SIZE 300
                             MESSAGE-ID zmpp  .

*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: equi ,
        ausp .

*----------------------------------------------------------------------*
* WORKING-AREA VARIABLES DECLARATION
*----------------------------------------------------------------------*
DATA: wa_material             LIKE mara-matnr                 ,
      wa_fsc                  LIKE mara-matnr                 ,
      wa_7jb                  LIKE ztpp_pmt07jb_b             ,
*     wa_plnum                LIKE plaf-plnum                 ,
      wa_equnr                LIKE equi-equnr                 ,
      wa_number               LIKE ztpp_pp_log_head-logkey,
      l_evcode(5)             TYPE c                          ,
      wa_lines                TYPE i                          ,
      wa_msg(70)              TYPE c                          ,
      wa_mng                  TYPE i                          ,
      wa_tabix                LIKE sy-tabix                   ,
      wa_seq                  LIKE ztpp_pp_log_deta-sequence,
      wa_flag                 TYPE c                          ,
*     wa_date                 TYPE d                          ,
      wa_error                TYPE c                          ,
      wa_mode                 TYPE c   VALUE   'N'            .

*----------------------------------------------------------------------*
* Field-Symbols VARIABLES DECLARATION
*----------------------------------------------------------------------*
FIELD-SYMBOLS: <field1>       TYPE ANY                        .

*----------------------------------------------------------------------*
* INTERNAL TABLES DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_7jb            LIKE ztpp_pmt07jb_b,
            p_log            LIKE ztpp_pp_log_head-logkey,
            p_date           TYPE d                      .
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM write_test       .
  PERFORM set_variables    .
  PERFORM record_processing.

*&---------------------------------------------------------------------*
*&      Form  RECORD_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM record_processing.
  DATA: l_date(10)           TYPE c          ,
        l_salesorder         LIKE vbak-vbeln ,
        l_fsc                LIKE mara-matnr ,
        l_vin                LIKE mara-matnr ,
        l_mode               LIKE ztpp_common_vals-key2.

  l_mode = 'EMF'.      " wa_7jb-modl .
* MOVE-CORRESPONDING wa_7jb  TO     wa_vehicle .
* wa_vehicle-matnr = wa_material .
  CONCATENATE l_mode wa_7jb-vinn+11(7) INTO wa_equnr.

  SELECT SINGLE fsc sales INTO (l_fsc, l_salesorder)
    FROM ztpp_wosum
   WHERE wo_ser = wa_7jb-ordr
     AND nation = wa_7jb-dist(3)
     AND dealer = wa_7jb-dist+3(2)
     AND extc   = wa_7jb-extc
     AND intc   = wa_7jb-intc     .

  " Processing of the Transaction MD11
  WRITE wa_7jb-sqdt TO l_date .
* wa_vehicle-sorder = l_salesorder .

  CONCATENATE wa_7jb-moye wa_7jb-dist  wa_7jb-bmdl   INTO wa_fsc     .
  CONCATENATE wa_fsc      wa_7jb-ocnn INTO wa_fsc    SEPARATED BY ' '.

  " Processing the VINGeneration & Vehicla Master Creation
* CONCATENATE  sy-datum+4(4) sy-uzeit    INTO wa_plnum      .
  PERFORM vin_vm_creation USING l_salesorder  l_fsc .

  if wa_error = 'X'.  exit.  endif.

  PERFORM class_assign .
ENDFORM.                    " RECORD_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  VIN_VM_CREATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0588   text
*      -->P_0589   text
*----------------------------------------------------------------------*
FORM vin_vm_creation USING    pa_sorder  pa_fsc.
  DATA: l_vartable        LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_conf            LIKE TABLE OF conf_out       WITH HEADER LINE,
        l_name(30)        TYPE c           ,
        l_no(03)          TYPE n           ,
        l_seq(6)          TYPE n           ,
        l_workorder       LIKE mara-matnr  ,
        l_eqfnr           LIKE itob-eqfnr  ,
        l_equnr           LIKE equi-equnr  ,
        l_rsnum           LIKE resb-rsnum  ,
        l_general         LIKE bapi_itob ,
        l_specific        LIKE bapi_itob_eq_only,
        l_return          LIKE bapiret2.

  l_seq  = wa_7jb-vinn+11(6) .
* CONCATENATE 'EMF'           l_seq INTO wa_equnr .
* CONCATENATE wa_7jb-bmdl(3)  l_seq INTO l_equnr .

  SELECT SINGLE *
    FROM equi
   WHERE equnr = wa_equnr.

  IF sy-subrc = 0.
    " Create Error Log for the COMMON_VALS incorrect!!!
    wa_error = 'X' .
    PERFORM create_log  using text-001          .
    EXIT.
  ENDIF.

  " Create the Vehicle Master
  CONCATENATE  wa_7jb-ordr  wa_7jb-dist  wa_7jb-extc  wa_7jb-intc
         INTO l_workorder.
  CONCATENATE pa_fsc wa_7jb-vers+1(2) l_workorder
         INTO l_general-descript SEPARATED BY space     .

  PERFORM get_workcenter      USING l_general-pp_wkctr  .

  l_general-objecttype       = '1000' .
  l_general-manfacture       = 'HMMA' .
  l_general-mancountry       = 'US'   .
  l_general-countr_iso       = 'US'   .
  l_general-manserno         =  l_seq .
  l_general-manmodel         =  wa_7jb-bmdl(3)          .
  l_general-constyear        =  sy-datum(4)             .
  l_general-constmonth       =  sy-datum+4(2)           .
  l_general-start_from       =  sy-datum                .
  l_general-planplant        = 'P001'                   .
  l_general-manparno         =  wa_7jb-vinn             .
  l_general-descript         =  wa_equnr                .
* l_general-sortfield        =  l_rsnum                 .
  l_general-maintplant       = 'P001'                   .
*  l_general-pp_wkctr         =  l_workcenter           .
  l_general-read_crdat       = sy-datum.
  l_general-read_crnam       = sy-uname.

  l_specific-equicatgry      = 'V' .
  WRITE: / 'BAPI-HEAD Create ==>', wa_equnr .

  CALL FUNCTION 'BAPI_EQUI_CREATE'
       EXPORTING
            external_number = wa_equnr
            data_general    = l_general
            data_specific   = l_specific
            valid_date      = sy-datum
       IMPORTING
            return          = l_return.

  " Check the Result of the BAPI Function...
  IF l_return-type = 'A' OR l_return-type = 'E' .
    PERFORM create_log  using  l_return-MESSAGE .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
           WAIT    = 'X' .
*   WAIT UP TO 3  SECONDS.
  ENDIF.
ENDFORM.                    " VIN_VM_CREATION

*&---------------------------------------------------------------------*
*&      Form  GET_WORKCENTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_WORKCENTER  text
*----------------------------------------------------------------------*
FORM get_workcenter USING    pa_workcenter.
  SELECT SINGLE objid
               INTO pa_workcenter
               FROM crhd
               WHERE arbpl EQ 'T'.
ENDFORM.                    " GET_WORKCENTER

*&---------------------------------------------------------------------*
*&      Form  CLASS_ASSIGN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM class_assign.
  DATA: l_equi            LIKE  v_equi  .

  l_equi-equnr = wa_equnr.
  WRITE: / 'BAPI-CLASS Create ==>', wa_equnr .

  CALL FUNCTION 'EQUIPMENT_CLASS_ALLOCATE'
       EXPORTING
            eq_class      = 'P_VEHICLE_MASTER'
            eq_class_type = '002'
            is_standard   = 'X'
            init_new      = 'X'
            lock_new      = 'X'
            update_new    = 'X'
            commit_new    = 'X'
       CHANGING
            s_equi        = l_equi.
ENDFORM.                    " CLASS_ASSIGN

*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0274   text
*      -->P_TXT_001  text
*----------------------------------------------------------------------*
FORM create_log  using pa_msg .
  DATA: l_log                LIKE ztpp_rep_seq .

  CLEAR: l_log.
  SELECT MAX( sequence ) INTO l_log-sequence
    FROM ztpp_rep_seq
   WHERE wk_date  = p_date  .

  l_log-wk_date   = p_date             .
  l_log-sequence  = l_log-sequence + 1 .
  l_log-MSG       = pa_msg             .
  l_log-step      = '4'                .
  l_log-status    = 'E'                .
  l_log-logtype   = 'E'                .
  MOVE-CORRESPONDING p_7jb    TO l_log .
  INSERT INTO ztpp_rep_seq VALUES l_log.
  wa_error = 'X'                       .
*
*  " Log Detail Creation
*  ztpp_pp_log_deta-logkey   = wa_number    .
*  ztpp_pp_log_deta-sequence = wa_seq      .
*  ztpp_pp_log_deta-logtype  = pa_type     .
*  ztpp_pp_log_deta-keydata  = pa_key      .
*  INSERT INTO ztpp_pp_log_deta VALUES ztpp_pp_log_deta .
ENDFORM.                    " create_log

*&---------------------------------------------------------------------*
*&      Form  SET_VARIABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_variables.
  DATA: l_atinn             LIKE cabn-atinn,
        l_material          LIKE mara-matnr.

  wa_7jb = p_7jb .
  CONCATENATE wa_7jb-ordr wa_7jb-dist INTO wa_material .
ENDFORM.                    " SET_VARIABLES

*&---------------------------------------------------------------------*
*&      Form  WRITE_TEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_test.
  WRITE: / 'Called Program...(Vehicle Master)'.
  WRITE AT: /001(010) p_7jb-ordr ,
             011(006) p_7jb-dist ,
             017(020) p_7jb-vinn ,
             037(200) p_7jb      .
ENDFORM.                    " WRITE_TEST
