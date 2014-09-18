*&---------------------------------------------------------------------*
*& Report  YISD_ZVIN_CLEANSING
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  yisd_zvin_cleansing.

TABLES: ztsd_um.

DATA: BEGIN OF it_veh_cnt OCCURS 0,
        model_code LIKE ztsd_um-model_code,
        body_no    LIKE ztsd_um-body_no,
        cnt        TYPE i,
      END   OF it_veh_cnt.

DATA: it_um     LIKE ztsd_um OCCURS 0 WITH HEADER LINE,
      it_um_ok  LIKE ztsd_um OCCURS 0 WITH HEADER LINE,
      it_um_err LIKE ztsd_um OCCURS 0 WITH HEADER LINE.

DATA: v_first.

PARAMETERS: p_run    AS CHECKBOX,
            p_update AS CHECKBOX.
SELECT-OPTIONS: s_serial FOR ztsd_um-wo_serial.

START-OF-SELECTION.
  CHECK p_run EQ 'X'.

  SELECT model_code body_no COUNT( * ) AS cnt
    INTO CORRESPONDING FIELDS OF TABLE it_veh_cnt
    FROM ztsd_um
   WHERE wo_serial IN s_serial
     AND status = 'F'
   GROUP BY model_code body_no
   HAVING COUNT( * ) >= 2.

  CHECK it_veh_cnt[] IS NOT INITIAL.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_um
    FROM ztsd_um
    FOR ALL ENTRIES IN it_veh_cnt
   WHERE model_code = it_veh_cnt-model_code
     AND body_no    = it_veh_cnt-body_no
     AND status     = 'F'.

  CLEAR: v_first.
  SORT it_um BY model_code body_no aedat aezet
                wo_serial wo_nation wo_dealer wo_extc wo_intc.

  LOOP AT it_um.
    ON CHANGE OF it_um-model_code OR it_um-body_no.
      MOVE: 'X' TO v_first.
    ENDON.

    IF v_first IS INITIAL AND p_update EQ 'X'.
      CLEAR: it_um-body_no,it_um-plan_date,
             it_um-status, it_um-ship_out.

      UPDATE ztsd_um FROM it_um.
    ENDIF.

    WRITE:/ it_um-model_code,
            it_um-body_no,
            it_um-zvin,
            it_um-wo_serial,
            it_um-wo_nation,
            it_um-wo_dealer,
            it_um-wo_extc,
            it_um-wo_intc,
            it_um-aenam,
            it_um-aedat,
            it_um-aezet.

    IF v_first EQ 'X'.
      WRITE: 'OK'.
    ELSE.
      WRITE: 'Err'.
    ENDIF.

    CLEAR: v_first.
  ENDLOOP.
