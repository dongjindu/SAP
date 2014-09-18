FUNCTION z_fca_get_vehicle_master.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_ATNAM) LIKE  CABN-ATNAM
*"     REFERENCE(I_ATWRT_S) LIKE  AUSP-ATWRT OPTIONAL
*"     REFERENCE(I_ATWRT_E) LIKE  AUSP-ATWRT OPTIONAL
*"     REFERENCE(I_OBJEK) LIKE  AUSP-OBJEK OPTIONAL
*"     REFERENCE(I_COUNT) TYPE  I DEFAULT 1000000
*"  EXPORTING
*"     REFERENCE(E_HIT_COUNT) TYPE  I
*"  TABLES
*"      T_CONDITION STRUCTURE  ZSCA_CHARACTERISTIC_VALUE
*"      T_VALUE STRUCTURE  ZSCA_CHAR_VALUE
*"      T_VEHICLE STRUCTURE  ZSCA_VEHICLE_CHAR_VALUE
*"  EXCEPTIONS
*"      DATE_OVERFLOW
*"      INVALID_DATE
*"      CONDITION_DOES_NOT_EXIST
*"      CHARACTERISTIC_DOES_NOT_EXIST
*"----------------------------------------------------------------------

  DATA: lw_day     TYPE i,                        " Date Term
        lw_atinn   LIKE cabn-atinn,               " Characteristic
        lw_atfor   LIKE cabn-atfor,               " Type
        lw_lines1  TYPE i,                        " IT_VEHICLE lines
        lw_lines2  TYPE i,                        " Coundition count
        lw_atwrt_s LIKE ausp-atwrt,               " Base condition(CHAR)
        lw_atwrt_e LIKE ausp-atwrt,               " Base condition(CHAR)
        lw_atflv_s LIKE ausp-atflv,               " Base condition
        lw_atflv_e LIKE ausp-atflv.               " Base condition

  CLEAR: it_condition,  it_condition[],
         it_vehicle,    it_vehicle[],
         it_cabn,       it_cabn[],
         r_atinn,       r_atinn[].

*----- Read all of characteristic
  SELECT atinn atnam atfor
    INTO TABLE it_cabn
    FROM cabn
  ORDER BY atinn.

*----- read information of base characteristic
  SELECT SINGLE atinn atfor
    INTO (lw_atinn, lw_atfor)
    FROM cabn
   WHERE atnam = i_atnam.
  IF sy-subrc NE 0.
    RAISE characteristic_does_not_exist.
  ENDIF.

*----- Set output characteristic
  MOVE: 'I'      TO r_atinn-sign,
        'EQ'     TO r_atinn-option,
        lw_atinn TO r_atinn-low.

  APPEND r_atinn.

  LOOP AT t_condition.
    MOVE-CORRESPONDING t_condition TO it_condition.

    READ TABLE it_cabn WITH KEY atnam = t_condition-atnam.
    IF sy-subrc NE 0. CONTINUE. ENDIF.

    MOVE: it_cabn-atinn TO it_condition-atinn.

    APPEND it_condition.

    MOVE: 'I'                TO r_atinn-sign,
          'EQ'               TO r_atinn-option,
          it_condition-atinn TO r_atinn-low.

    APPEND r_atinn.
  ENDLOOP.

  LOOP AT t_value.
    READ TABLE it_cabn WITH KEY atnam = t_value-atnam.
    IF sy-subrc NE 0. CONTINUE. ENDIF.

    MOVE: 'I'               TO r_atinn-sign,
          'EQ'              TO r_atinn-option,
          it_cabn-atinn     TO r_atinn-low.

    COLLECT r_atinn.
  ENDLOOP.

*----- Count of Vehicle Master
  DESCRIBE TABLE r_atinn LINES lw_lines2.
  w_count = lw_lines2 * i_count.

*----- Conversion I_ATWRT
  IF     i_atwrt_s EQ ' ' AND i_atwrt_e EQ ' '.
    lw_atwrt_e = 'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ'.
  ELSEIF i_atwrt_s NE ' ' AND i_atwrt_e EQ ' '.
    lw_atwrt_s = lw_atwrt_e = i_atwrt_s.
  ELSEIF i_atwrt_s EQ ' ' AND i_atwrt_e NE ' '.
    lw_atwrt_e = i_atwrt_e.
  ELSEIF i_atwrt_s NE ' ' AND i_atwrt_e NE ' '.
    lw_atwrt_s = i_atwrt_s. lw_atwrt_e = i_atwrt_e.
  ENDIF.

  IF lw_atfor EQ 'NUM' OR
     lw_atfor EQ 'DATE'.
    MOVE: lw_atwrt_s TO lw_atflv_s,
          lw_atwrt_e TO lw_atflv_e.
  ENDIF.

*----- Read Vehicle Master with Sequence Date
  CASE lw_atfor.
    WHEN 'NUM' OR 'DATE'.
      IF i_objek IS INITIAL.
        EXEC SQL PERFORMING APPEND_IT_VEHICLE.
          SELECT A.OBJEK
            INTO :W_OBJEK
            FROM AUSP A
           WHERE A.MANDT = :sy-mandt
             AND A.ATINN = :LW_ATINN
             AND A.ATFLV BETWEEN :LW_ATFLV_S AND :LW_ATFLV_E
             AND A.KLART = '002'
           ORDER BY OBJEK
        ENDEXEC.
      ELSE.
        EXEC SQL PERFORMING APPEND_IT_VEHICLE.
          SELECT A.OBJEK
            INTO :W_OBJEK
            FROM AUSP A
           WHERE A.MANDT = :sy-mandt
             and A.OBJEK >= :I_OBJEK
             AND A.ATINN = :LW_ATINN
             AND A.ATFLV BETWEEN :LW_ATFLV_S AND :LW_ATFLV_E
             AND A.KLART = '002'
           ORDER BY OBJEK
        ENDEXEC.
      ENDIF.
    WHEN OTHERS.
      IF i_objek IS INITIAL.
        EXEC SQL PERFORMING APPEND_IT_VEHICLE.
          SELECT A.OBJEK
            INTO :W_OBJEK
            FROM AUSP A
           WHERE A.MANDT = :sy-mandt
             AND A.ATINN = :LW_ATINN
             AND A.ATWRT BETWEEN :LW_ATWRT_S AND :LW_ATWRT_E
             AND A.KLART = '002'
           ORDER BY OBJEK
        ENDEXEC.
      ELSE.
        EXEC SQL PERFORMING APPEND_IT_VEHICLE.
          SELECT A.OBJEK
            INTO :W_OBJEK
            FROM AUSP A
           WHERE A.MANDT = :sy-mandt
             and A.OBJEK >= :I_OBJEK
             AND A.ATINN = :LW_ATINN
             AND A.ATWRT BETWEEN :LW_ATWRT_S AND :LW_ATWRT_E
             AND A.KLART = '002'
           ORDER BY OBJEK
        ENDEXEC.
      ENDIF.
  ENDCASE.

  t_vehicle[]   = it_vehicle[].

  DESCRIBE TABLE it_vehicle LINES lw_lines1.
  e_hit_count = lw_lines1 / lw_lines2.
ENDFUNCTION.
