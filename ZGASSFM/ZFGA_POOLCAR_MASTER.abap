FUNCTION zfga_poolcar_master.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_MODE) TYPE  ZEMODE
*"     VALUE(I_INDEX) TYPE  INT1 DEFAULT 1
*"     VALUE(I_POOLCARID) TYPE  ZTGA_PCMASTER-POOLCARID OPTIONAL
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  BAPIRETURN STRUCTURE  BAPIRETURN
*"     VALUE(E_TOTAL) TYPE  INT4
*"  TABLES
*"      T_DATA STRUCTURE  ZSGA_PCMASTER..
*"----------------------------------------------------------------------

  CLEAR : it_master[], it_master, it_detail[], it_detail,
          wa_master, wa_detail, l_poolcarid, l_zseq, it_emp_info[],
          it_emp_info.

  CASE i_mode.
    WHEN 'I'.  "Insert - always single line
      LOOP AT t_data.

*-      Check logic

        IF t_data-poolcarid IS INITIAL.
          SELECT MAX( poolcarid ) INTO l_poolcarid
          FROM ztga_pcmaster.

          IF l_poolcarid IS INITIAL.
            t_data-poolcarid  =  '10001'.
          ELSE.
            t_data-poolcarid  =   l_poolcarid + 1.
          ENDIF.

          t_data-zdate  = sy-datum.
          t_data-ztime  = sy-uzeit.
          t_data-zseq   = 1.

          MOVE-CORRESPONDING t_data TO it_master.
          MOVE-CORRESPONDING t_data TO it_detail.

        ELSE.
          e_return-type    = 'E'.
          e_return-message = 'Data input Error'.
          EXIT.
        ENDIF.

        IF e_return IS INITIAL.
          INSERT ztga_pcmaster   FROM  it_master.
          INSERT ztga_pcmaster_l FROM  it_detail.
          IF sy-subrc = 0.
            COMMIT WORK.
            e_return-type    = 'S'.
            e_return-message = 'Vehicle was updated successfully'.
          ELSE.
            ROLLBACK WORK.
            e_return-type    = 'E'.
            e_return-message = 'Error occured during update'.
          ENDIF.
        ENDIF.

      ENDLOOP.

    WHEN 'U'.   "Update
      LOOP AT t_data.
        IF t_data-poolcarid IS INITIAL.
          e_return-type    = 'E'.
          e_return-message = 'Data input Error(Pool car ID)'.
          EXIT.
        ENDIF.

        IF e_return IS INITIAL.
          SELECT  zseq INTO l_zseq
             FROM  ztga_pcmaster_l
               UP TO 1 ROWS
             WHERE poolcarid = t_data-poolcarid
               AND zdate     = sy-datum
             ORDER BY zseq DESCENDING.
          ENDSELECT.

          MOVE-CORRESPONDING t_data TO it_master.
          MOVE-CORRESPONDING t_data TO it_detail.

          it_detail-zdate = sy-datum.
          it_detail-ztime = sy-uzeit.
          it_detail-zseq  = l_zseq + 1.

          MODIFY ztga_pcmaster   FROM  it_master.
          MODIFY ztga_pcmaster_l FROM  it_detail.
          IF sy-subrc = 0.
            COMMIT WORK.
            e_return-type    = 'S'.
            e_return-message = 'Vehicle was updated successfully'.
          ELSE.
            ROLLBACK WORK.
            e_return-type    = 'E'.
            e_return-message = 'Error occured during update'.
          ENDIF.
        ENDIF.

      ENDLOOP.

    WHEN 'S'.   "Read single
      IF i_poolcarid IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Pool Car ID is missing'.
        EXIT.
      ENDIF.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF wa_master
      FROM ztga_pcmaster
      WHERE poolcarid =  i_poolcarid.

      SELECT  * INTO CORRESPONDING FIELDS OF  wa_detail
      FROM ztga_pcmaster_l
       UP TO 1 ROWS
      WHERE   poolcarid =  i_poolcarid
      ORDER BY zdate DESCENDING zseq DESCENDING.
      ENDSELECT.

      IF wa_master IS INITIAL OR wa_detail IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Pool Car Info does not exist'.
        EXIT.
      ELSE.
        MOVE-CORRESPONDING wa_master TO t_data.
        MOVE-CORRESPONDING wa_detail TO t_data.

        CALL FUNCTION 'ZFGA_POOLCAR_EMP_INFO'
          EXPORTING
            i_pernr = t_data-driverid
          TABLES
            t_data  = it_emp_info.
        READ TABLE it_emp_info INDEX 1.
        IF sy-subrc = 0.
          t_data-lastname  = it_emp_info-lastname.
          t_data-firstname = it_emp_info-firstname.
          t_data-orgtx     = it_emp_info-orgtx.
        ENDIF.

        APPEND t_data.
        e_return-type    = 'S'.

      ENDIF.

    WHEN 'A'.   "Read all
      IF i_index IS INITIAL .
        e_return-type    = 'E'.
        e_return-message = 'No Page No.'.
        EXIT.
      ENDIF.

      SELECT * INTO CORRESPONDING FIELDS OF TABLE t_data
      FROM ztga_pcmaster AS a INNER JOIN ztga_pcmaster_l AS b
                        ON a~poolcarid  =  b~poolcarid.

      SORT t_data BY poolcarid ASCENDING
                     zdate     DESCENDING
                     zseq      DESCENDING.
      DELETE ADJACENT DUPLICATES FROM t_data
                                COMPARING poolcarid.
      IF t_data[] IS INITIAL .
        e_return-type    = 'E'.
        e_return-message = 'Pool Car Info does not exist'.
        EXIT.
      ELSE.
        PERFORM get_display_page TABLES t_data USING i_index
                                               CHANGING e_total.
        e_return-type    = 'S'.
      ENDIF.

  ENDCASE.

ENDFUNCTION.
