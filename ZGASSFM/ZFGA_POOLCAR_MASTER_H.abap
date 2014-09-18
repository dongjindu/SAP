FUNCTION ZFGA_POOLCAR_MASTER_H.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_MODE) TYPE  ZEMODE
*"     VALUE(I_INDEX) TYPE  INT1 DEFAULT 1
*"     VALUE(I_POOLCARID) TYPE  ZTGA_PCMASTER-POOLCARID OPTIONAL
*"     VALUE(I_LIST) TYPE  CHAR20 OPTIONAL
*"     VALUE(I_INPUT) TYPE  CHAR40 OPTIONAL
*"     VALUE(I_POOL) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_INACTIVE) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_FIRST) TYPE  PA0002-VORNA OPTIONAL
*"     VALUE(I_LAST) TYPE  PA0002-NACHN OPTIONAL
*"     VALUE(I_DRIVERID) TYPE  ZSGA_PCMASTER-DRIVERID OPTIONAL
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  BAPIRETURN STRUCTURE  BAPIRETURN
*"     VALUE(E_TOTAL) TYPE  INT4
*"  TABLES
*"      T_DATA STRUCTURE  ZSGA_PCMASTER_H OPTIONAL..
*"----------------------------------------------------------------------

DATA:  l_cond(40), l_cond2(40).

  CLEAR : it_master[], it_master, it_detail[], it_detail,
          wa_master, wa_detail, l_poolcarid, it_emp_info[],
          it_emp_info,v_conditions, v_conditions2, v_conditions3,
          v_conditions4, l_cond, l_cond2.

  CASE i_mode.

    WHEN 'D'.   "Detail

*-   single line info
      IF i_poolcarid IS INITIAL AND i_driverid IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Pool Car ID/Driver ID is missing'.
        EXIT.
      ENDIF.
      IF i_driverid IS NOT INITIAL. "Find with driver ID
        SELECT a~poolcarid  a~autoyear  a~automake  a~automodel
               a~vin        a~autocolor a~autotrim  a~origination
               b~tagyear    b~tag       b~poolcar_no b~purpose
               b~zdate      b~zseq
               b~current_stat b~driverid c~purpose AS zpurpose
               c~destination c~checkout_id  c~checkout_dt
               c~checkin_id  c~checkin_dt   c~begin_mile
               c~end_mile    "D~VNAMC AS FIRSTNAME D~NCHMC AS LASTNAME
            INTO CORRESPONDING FIELDS OF TABLE t_data
        FROM ztga_pcmaster AS a INNER JOIN  ztga_pcmaster_l AS b
                             ON a~poolcarid = b~poolcarid
                                LEFT OUTER JOIN ztga_pcrequest AS c
                             ON b~poolcarid = c~poolcarid
                            AND b~zdate     = c~zdate
                            AND b~zseq      = c~zseq
*                              INNER JOIN pa0002 AS D
*                              ON B~DRIVERID = D~PERNR
        WHERE b~driverid =  i_driverid.

      ELSE.   "find with Poolcar ID
        SELECT a~poolcarid  a~autoyear  a~automake  a~automodel
               a~vin        a~autocolor a~autotrim  a~origination
               b~tagyear    b~tag       b~poolcar_no b~purpose
               b~zdate      b~zseq
               b~current_stat b~driverid c~purpose AS zpurpose
               c~destination c~checkout_id  c~checkout_dt
               c~checkin_id  c~checkin_dt   c~begin_mile
               c~end_mile    "D~VNAMC AS FIRSTNAME D~NCHMC AS LASTNAME
            INTO CORRESPONDING FIELDS OF TABLE t_data
        FROM ztga_pcmaster AS a INNER JOIN  ztga_pcmaster_l AS b
                             ON a~poolcarid = b~poolcarid
                                LEFT OUTER JOIN ztga_pcrequest AS c
                             ON b~poolcarid = c~poolcarid
                            AND b~zdate     = c~zdate
                            AND b~zseq      = c~zseq
*                              INNER JOIN pa0002 AS D
*                              ON B~DRIVERID = D~PERNR
        WHERE a~poolcarid =  i_poolcarid.

      ENDIF.

      LOOP AT t_data.
        CALL FUNCTION 'ZFGA_POOLCAR_EMP_INFO'
          EXPORTING
            i_pernr = t_data-driverid
          TABLES
            t_data  = it_emp_info.
        READ TABLE it_emp_info INDEX 1.
        IF sy-subrc = 0.
          t_data-lastname  = it_emp_info-lastname.
          t_data-firstname = it_emp_info-firstname.
        ENDIF.
        MODIFY t_data.
      ENDLOOP.

      IF t_data[] IS INITIAL .
        e_return-type    = 'E'.
        e_return-message = 'Data does not exist'.
        EXIT.
      ELSE.
        PERFORM get_display_page TABLES t_data USING i_index
                                               CHANGING e_total.
        e_return-type    = 'S'.
      ENDIF.


    WHEN 'H'.   "Head
      IF i_index IS INITIAL .
        e_return-type    = 'E'.
        e_return-message = 'No Page No.'.
        EXIT.
      ENDIF.
      IF i_list IS INITIAL AND i_input IS INITIAL
                                AND i_first IS INITIAL AND i_last IS INITIAL.
      ELSE.
        IF i_list = 'TAG'.
          l_cond = 'B~TAG LIKE' .
        ELSEIF i_list = 'VIN'.
          l_cond = 'A~VIN LIKE' .
        ELSEIF i_list = 'POOLCARID'.
          l_cond = 'A~POOLCARID LIKE' .
        ELSEIF i_list = 'AUTOYEAR'.
          l_cond = 'A~AUTOYEAR LIKE' .
        ELSEIF i_list = 'AUTOMODEL'.
          l_cond = 'A~AUTOMODEL LIKE' .
        ELSEIF i_list = 'DRIVERID'.
          IF i_first IS NOT INITIAL.
            CONDENSE i_first.
            TRANSLATE i_first TO UPPER CASE.
            CONCATENATE `'%`  i_first  `%'` INTO l_cond.
            CONCATENATE 'C~VNAMC LIKE ' l_cond INTO v_conditions
                                                    SEPARATED BY space.
          ENDIF.

          IF i_last IS NOT INITIAL.
            CONDENSE i_last.
            TRANSLATE i_last TO UPPER CASE.
            CONCATENATE `'%`  i_last  `%'` INTO l_cond2.
            CONCATENATE 'C~NCHMC LIKE ' l_cond2 INTO v_conditions4
                                                    SEPARATED BY space.
          ENDIF.

        ELSE.
          e_return-type    = 'E'.
          e_return-message = 'Filter List is incorrect. Please check'.
          EXIT.
        ENDIF.

        IF i_list <> 'DRIVERID'.
          CONCATENATE `'%` i_input `%'` INTO l_cond2.
          CONCATENATE l_cond l_cond2 INTO v_conditions
                                        SEPARATED BY space.
        ENDIF.
      ENDIF.

      IF i_pool = 'X'.    "Pool Car Only
        CONCATENATE 'B~PURPOSE ='  `'P'` INTO v_conditions2
                                              SEPARATED BY space.
      ENDIF.

      IF i_inactive IS INITIAL.    "Active Car Only
        CONCATENATE 'B~CURRENT_STAT ='  `'A'` INTO v_conditions3
                                              SEPARATED BY space.
      ENDIF.

      IF i_list = 'DRIVERID'.
        TRY.
            SELECT * INTO CORRESPONDING FIELDS OF TABLE t_data
            FROM ztga_pcmaster AS a INNER JOIN ztga_pcmaster_l AS b
                              ON a~poolcarid  =  b~poolcarid
                              INNER JOIN pa0002 AS c
                              ON b~driverid  = c~pernr
            WHERE (v_conditions)
              AND (v_conditions4)
              AND c~endda = '99991231'
              AND (v_conditions2)
              AND (v_conditions3).
          CATCH cx_sy_dynamic_osql_semantics.
        ENDTRY.
      ELSE.
        TRY.
            SELECT * INTO CORRESPONDING FIELDS OF TABLE t_data
            FROM ztga_pcmaster AS a INNER JOIN ztga_pcmaster_l AS b
                              ON a~poolcarid  =  b~poolcarid
            WHERE (v_conditions)
              AND (v_conditions2)
              AND (v_conditions3).

          CATCH cx_sy_dynamic_osql_semantics. "Ignore ABAP Dump
            "MESSAGE E000
        ENDTRY.

      ENDIF.

      SORT t_data BY poolcarid ASCENDING
                     zdate     DESCENDING
                     zseq      DESCENDING.
      DELETE ADJACENT DUPLICATES FROM t_data
                                COMPARING poolcarid.

      LOOP AT t_data.
        CLEAR : it_emp_info[], it_emp_info.
        CALL FUNCTION 'ZFGA_POOLCAR_EMP_INFO'
          EXPORTING
            i_pernr = t_data-driverid
          TABLES
            t_data  = it_emp_info.

        READ TABLE it_emp_info INDEX 1.
        IF sy-subrc = 0.
          t_data-lastname   = it_emp_info-lastname.
          t_data-firstname  = it_emp_info-firstname.
        ENDIF.
        MODIFY t_data.
      ENDLOOP.

      IF t_data[] IS INITIAL .
        e_return-type    = 'E'.
        e_return-message = 'Data does not exist'.
        EXIT.
      ELSE.
        PERFORM get_display_page TABLES t_data USING i_index
                                               CHANGING e_total.
        e_return-type    = 'S'.
      ENDIF.

  ENDCASE.


ENDFUNCTION.
