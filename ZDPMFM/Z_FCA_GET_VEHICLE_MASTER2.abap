FUNCTION z_fca_get_vehicle_master2.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_RP_DATE_S) LIKE  AUSP-ATWRT
*"     REFERENCE(I_RP_DATE_E) LIKE  AUSP-ATWRT
*"     REFERENCE(I_OBJEK) LIKE  AUSP-OBJEK OPTIONAL
*"     REFERENCE(I_COUNT) TYPE  I DEFAULT 1000000
*"  TABLES
*"      T_CONDITION STRUCTURE  ZSCA_CHARACTERISTIC_VALUE
*"      T_VEHICLE STRUCTURE  ZSCA_VEHICLE_CHAR_VALUE
*"      T_VALUE STRUCTURE  ZSCA_CHAR_VALUE
*"  EXCEPTIONS
*"      DATE_OVERFLOW
*"      INVALID_DATE
*"      CONDITION_DOES_NOT_EXIST
*"----------------------------------------------------------------------
*
*  DATA: lw_day        TYPE i,            " Date Term
*        lw_rp_date_s LIKE sy-datum,      " PR date
*        lw_rp_date_e LIKE sy-datum,      " PR date
*        lw_erdat_s    LIKE sy-datum,     " Create date
*        lw_erdat_e    LIKE sy-datum.     " Create date
*
*  CLEAR: it_condition,  it_condition[],
*         it_vehicle,    it_vehicle[],
*         it_cabn,       it_cabn[],
*         r_atinn,       r_atinn[].
*
*  MOVE: i_rp_date_s TO lw_rp_date_s,
*        i_rp_date_e TO lw_rp_date_e.
*
*  lw_erdat_s = lw_rp_date_s - 30.
*  lw_erdat_e = lw_rp_date_e + 30.
*
*  lw_day = lw_rp_date_e - lw_rp_date_s.
*  IF lw_day > 31.
*    RAISE date_overflow.
*  ENDIF.
*
*  IF i_rp_date_e < i_rp_date_s.
*    RAISE invalid_date.
*  ENDIF.
*
*  SELECT atinn atnam
*    INTO TABLE it_cabn
*    FROM cabn
*  ORDER BY atinn.
*
*  LOOP AT t_condition.
*    MOVE-CORRESPONDING t_condition TO it_condition.
*
*    READ TABLE it_cabn WITH KEY atnam = t_condition-atnam.
*    IF sy-subrc NE 0. CONTINUE. ENDIF.
*
*    MOVE: it_cabn-atinn TO it_condition-atinn.
*
*    APPEND it_condition.
*
*    MOVE: 'I'                TO r_atinn-sign,
*          'EQ'               TO r_atinn-option,
*          it_condition-atinn TO r_atinn-low.
*
*    APPEND r_atinn.
*  ENDLOOP.
*
*  READ TABLE it_condition INDEX 1.
*  IF sy-subrc NE 0.
*    RAISE condition_does_not_exist.
*  ENDIF.
*
*
*  LOOP AT t_value.
*    READ TABLE it_cabn WITH KEY atnam = t_value-atnam.
*    IF sy-subrc NE 0. CONTINUE. ENDIF.
*
*    MOVE: 'I'               TO r_atinn-sign,
*          'EQ'              TO r_atinn-option,
*          it_cabn-atinn     TO r_atinn-low.
*
*    COLLECT r_atinn.
*  ENDLOOP.
**>>>>> for JOKIM
**----- Read Vehicle Master with Sequence Date
*  EXEC SQL PERFORMING APPEND_IT_VEHICLE.
**      SELECT  /*+ INDEX(B EQUI______Z03)*/
*    SELECT A.OBJEK
*      INTO :W_OBJEK
*      FROM AUSP A
*     WHERE A.MANDT = :SY-MANDT
**       and A.OBJEK = B.EQUNR
*       AND A.ATINN = '0000000750'
*       AND A.ATWRT BETWEEN :i_RP_date_s AND :i_RP_date_e
*       AND A.KLART = '002'
*  ENDEXEC.
**<<< for JOKIM
**<<<<< DYHAN
*
*  EXEC SQL PERFORMING APPEND_IT_CHAR.
*    SELECT  /*+ ORDERED*/
*           A.OBJEK, B.ATINN, B.ATWRT, C.ATNAM
*      INTO :IT_CHAR
*      FROM (SELECT WORDER AS OBJEK
*              FROM ZTPP_SPEC
*             WHERE MANDT = :SY-MANDT
*               AND OPDAT BETWEEN :i_RP_date_s AND :i_RP_date_e
*               AND MARK = 'R'
*             GROUP BY WORDER) A,
*             AUSP B, CABN C
*     WHERE B.MANDT = :SY-MANDT
*       and B.OBJEK = A.OBJEK
*       AND B.KLART = '002'
*       AND C.MANDT = B.MANDT
*       AND C.ATINN = B.ATINN
*  ENDEXEC.
*
*
**  IF i_objek IS INITIAL.
**    EXEC SQL PERFORMING APPEND_IT_VEHICLE.
**      SELECT A.OBJEK
**        INTO :W_OBJEK
**        FROM AUSP A
**       WHERE A.MANDT = :sy-mandt
**         AND A.ATINN = '0000000750'
**         AND A.ATWRT BETWEEN :i_seq_date_s AND :i_seq_date_e
**         AND A.KLART = '002'
**         AND ROWNUM <= :I_COUNT
**    ENDEXEC.
**  ELSE.
**    EXEC SQL PERFORMING APPEND_IT_VEHICLE.
**      SELECT A.OBJEK
**        INTO :W_OBJEK
**        FROM AUSP A, EQUI B
**       WHERE B.MANDT = :SY-MANDT
**         AND B.EQUTY =
**         AND A.MANDT = :SY-MANDT
**         and A.OBJEK >= :I_OBJEK
**         AND A.ATINN = '0000000750'
**         AND A.ATWRT BETWEEN :i_seq_date_s AND :i_seq_date_e
**         AND A.KLART = '002'
**         AND ROWNUM <= :I_COUNT
**    ENDEXEC.
**  ENDIF.
*
*  t_vehicle[]   = it_vehicle[].

ENDFUNCTION.
