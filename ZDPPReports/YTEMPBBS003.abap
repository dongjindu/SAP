*&---------------------------------------------------------------------*
*& Report  YTEMPBBS003
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ytempbbs003.
DATA: BEGIN OF it_vm OCCURS 0,
        wo_serial  LIKE ztsd_um-wo_serial,
        wo_nation  LIKE ztsd_um-wo_nation,
        wo_dealer  LIKE ztsd_um-wo_dealer,
        wo_extc    LIKE ztsd_um-wo_extc,
        wo_intc    LIKE ztsd_um-wo_intc,
        intno      LIKE ztsd_um-intno,
        zvin       LIKE ztsd_um-zvin,
        model_code LIKE ztsd_um-model_code,
        body_no    LIKE ztsd_um-body_no,
        rp_cstatus LIKE ztpp_vm-rp_cstatus,
        rp25_sdate LIKE zvpp_vm-rp25_sdate,
        rp27_sdate LIKE zvpp_vm-rp27_sdate,
      END   OF it_vm.

DATA: v_mod TYPE i.

PARAMETERS: p_chk.

START-OF-SELECTION.
  CHECK p_chk EQ 'X'.

  SELECT a~wo_serial a~wo_nation a~wo_dealer a~wo_extc a~wo_intc
         a~intno a~zvin b~model_code b~body_no b~rp_cstatus
         b~rp25_sdate b~rp27_sdate
    INTO CORRESPONDING FIELDS OF TABLE it_vm
    FROM ztsd_um AS a INNER JOIN zvpp_vm AS b
                        ON b~model_code = a~model_code
                       AND b~body_no    = a~body_no
   WHERE a~status   = 'F'
     AND a~ship_out < '19000000'.

  LOOP AT it_vm.
    CASE it_vm-rp_cstatus.
      WHEN '25'.
        UPDATE ztsd_um
           SET ship_out = it_vm-rp25_sdate
         WHERE model_code = it_vm-model_code
           AND body_no    = it_vm-body_no
           AND status = 'F'.
      WHEN '27'.
        UPDATE ztsd_um
           SET ship_out = it_vm-rp27_sdate
         WHERE model_code = it_vm-model_code
           AND body_no    = it_vm-body_no
           AND status = 'F'.
    ENDCASE.

    v_mod = sy-tabix MOD 10000.
    IF v_mod EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDLOOP.
