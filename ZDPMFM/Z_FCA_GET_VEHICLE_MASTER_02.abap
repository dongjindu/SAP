FUNCTION Z_FCA_GET_VEHICLE_MASTER_02.
*"----------------------------------------------------------------------
*"*"Local interface:
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

  DATA: lw_day     TYPE i,                            " Date Term
        lw_atinn   LIKE cabn-atinn,                   " Characteristic
        lw_lines1  TYPE i,                            " IT_VEHICLE lines
        lw_lines2  TYPE i.                            " Coundition count

  CLEAR: it_condition,  it_condition[],
         it_vehicle,    it_vehicle[],
         it_cabn,       it_cabn[],
         r_atinn,       r_atinn[].

  SELECT atinn atnam
    INTO TABLE it_cabn
    FROM cabn
  ORDER BY atinn.

  SELECT SINGLE atinn INTO lw_atinn
    FROM cabn
   WHERE atnam = i_atnam.
  IF sy-subrc NE 0.
    RAISE characteristic_does_not_exist.
  ENDIF.

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

  DESCRIBE TABLE r_atinn LINES lw_lines2.
  w_count = lw_lines2 * i_count.

*----- Conversion I_ATWRT
*  IF     I_ATWRT_S EQ ' ' AND I_ATWRT_E EQ ' '.
*  ELSEIF I_ATWRT_S NE ' ' AND I_ATWRT_E EQ ' '.
*  ELSEIF I_ATWRT_S EQ ' ' AND I_ATWRT_E NE ' '.
*  ELSEIF I_ATWRT_S NE ' ' AND I_ATWRT_E NE ' '.
*  ENDIF.

*----- Read Vehicle Master with Sequence Date
  IF i_objek IS INITIAL.
    EXEC SQL PERFORMING APPEND_IT_VEHICLE.
      SELECT A.OBJEK
        INTO :W_OBJEK
        FROM AUSP A
       WHERE A.MANDT = :sy-mandt
         AND A.ATINN = :LW_ATINN
*         AND A.ATWRT BETWEEN :I_ATWRT_S AND :I_ATWRT_E
         AND A.ATWRT in ('19','20','21','22','23','24','26')
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
         AND A.ATWRT BETWEEN :I_ATWRT_S AND :I_ATWRT_E
         AND A.KLART = '002'
       ORDER BY OBJEK
    ENDEXEC.
  ENDIF.

  t_vehicle[]   = it_vehicle[].

  DESCRIBE TABLE it_vehicle LINES lw_lines1.
  e_hit_count = lw_lines1 / lw_lines2.
ENDFUNCTION.
