FUNCTION zpp_check_condition_1.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      P_PHARSE STRUCTURE  ZSPP_PHARSE
*"      P_SUM STRUCTURE  ZSPP_INPUT_SUM
*"----------------------------------------------------------------------
  DATA : f_oper LIKE p_pharse-oper,
         p_error TYPE c,
        l_worder TYPE matnr,
        l_extc TYPE zextc,
        l_intc TYPE zintc,
        l_status TYPE zpp_status1.
  .
*  DATA: lp_sum LIKE TABLE OF p_sum WITH HEADER LINE.
  DATA: BEGIN OF lp_sum OCCURS 0,
        worder TYPE matnr,
        extc TYPE zextc,
        intc TYPE zintc,
        status TYPE zpp_status1,
        rp TYPE zseq,
        cond TYPE ztext,
        hours LIKE zspp_input_sum-hours,
        mitu LIKE zspp_input_sum-mitu,
        mitucnt LIKE zspp_input_sum-mitucnt,
        cnt LIKE zspp_input_sum-cnt,
  END OF lp_sum.

*  data: lt_PHARSE like table of zspp_PHARSE with header line.
*
*  lt_PHARSE[] = p_PHARSE[].
*  TABLES: ztpp_fwo.
  LOOP AT p_sum.
    MOVE-CORRESPONDING p_sum TO lp_sum.
    APPEND lp_sum.
    CLEAR: p_sum.
  ENDLOOP.

  SORT lp_sum BY worder extc intc status.

  LOOP AT lp_sum.

    IF lp_sum-worder = l_worder AND
        lp_sum-extc = l_extc AND
        lp_sum-intc = l_intc AND
        lp_sum-status = l_status.
      lp_sum-mitu   = p_error .
      MODIFY lp_sum.
      CLEAR: lp_sum.
    ELSE.

      CONCATENATE lp_sum-worder lp_sum-extc lp_sum-intc INTO l_worder.

      CLEAR: f_oper, p_error.
      LOOP AT p_pharse.
* In case of 'Not'
        IF p_pharse-op = '!'.
          IF lp_sum-status <> 'B'.   "
            SELECT SINGLE *  FROM ausp
             WHERE objek = l_worder
             AND atinn = p_pharse-atinn
               AND klart = '001'
               AND atwrt = p_pharse-atwrt
               AND atflv = p_pharse-atflv .
            IF sy-subrc EQ 0.
              p_error  = 'X'.
              CONTINUE.
            ELSE.
              CLEAR p_error.
            ENDIF.
          ELSE.
            IF l_worder(1) EQ 'E'.
              SELECT SINGLE *  FROM ausp
                WHERE objek = l_worder
                  AND klart = '001'.
              IF sy-subrc = 0.
                SELECT SINGLE *  FROM ausp
                 WHERE objek = l_worder
                   AND atinn = p_pharse-atinn
                   AND klart = '001'
                   AND atwrt = p_pharse-atwrt
                   AND atflv = p_pharse-atflv .
                IF sy-subrc EQ 0.
                  p_error  = 'X'.
                  CONTINUE.
                ELSE.
                  CLEAR p_error.
                ENDIF.
              ELSE.
                p_error  = 'X'.
                CONTINUE.
              ENDIF.
            ELSE. " F
              SELECT SINGLE * FROM ztpp_fwo
                WHERE worder EQ l_worder.

              SELECT SINGLE *  FROM ausp
                WHERE objek = ztpp_fwo-o_worder(18)
                  AND atinn = p_pharse-atinn
                  AND klart = '001'
                  AND atwrt = p_pharse-atwrt
                  AND atflv = p_pharse-atflv .
              IF sy-subrc EQ 0.
                p_error  = 'X'.
                CONTINUE.
              ELSE.
                CLEAR p_error.
              ENDIF.
            ENDIF.
          ENDIF.
* In case of 'OR'
        ELSE.
*      IF it_pharse-oper = '@'
          IF f_oper EQ 'X' AND p_error EQ ' '.
            READ TABLE p_pharse WITH KEY oper =  '@'.
            IF sy-subrc = 0.
              EXIT.
            ENDIF.
          ENDIF.
          IF p_error = 'X'.
            READ TABLE p_pharse WITH KEY oper =  '&'.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          IF lp_sum-status <> 'B'.
            SELECT SINGLE *  FROM ausp
             WHERE objek = l_worder
             AND atinn = p_pharse-atinn
               AND klart = '001'
               AND atwrt = p_pharse-atwrt
               AND atflv = p_pharse-atflv .
            IF sy-subrc NE 0.
              p_error  = 'X'.
            ELSE.
              CLEAR p_error.  f_oper = 'X'.
*          EXIT.
            ENDIF.
          ELSE."WEEKLY "B
            IF l_worder(1) EQ 'E'.
              SELECT SINGLE *  FROM ausp
                WHERE objek = l_worder
                 AND klart = '001'.
              IF sy-subrc = 0.
                SELECT SINGLE *  FROM ausp
                 WHERE objek = l_worder
                   AND atinn = p_pharse-atinn
                   AND klart = '001'
                   AND atwrt = p_pharse-atwrt
                   AND atflv = p_pharse-atflv .
                IF sy-subrc NE 0.
                  p_error  = 'X'. CONTINUE.
                ELSE.
                  CLEAR p_error.f_oper = 'X'.
                ENDIF.
              ELSE.
                p_error  = 'X'.
                CONTINUE.
              ENDIF.
            ELSE. "F
              SELECT SINGLE * FROM ztpp_fwo
                WHERE worder EQ l_worder.
              IF sy-subrc = 0.
                SELECT SINGLE *  FROM ausp
                  WHERE objek = ztpp_fwo-o_worder(18)
                    AND atinn = p_pharse-atinn
                    AND klart = '001'
                    AND atwrt = p_pharse-atwrt
                    AND atflv = p_pharse-atflv.

                IF sy-subrc <> 0.
                  p_error  = 'X'.
                  CONTINUE.
                ELSE.
                  CLEAR p_error.f_oper = 'X'.
                ENDIF.
              ELSE.
                p_error  = 'X'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      lp_sum-mitu   = p_error .
      MODIFY lp_sum.
      CLEAR: lp_sum.
    ENDIF.
  ENDLOOP.

  REFRESH: p_sum.

  LOOP AT lp_sum.
    MOVE-CORRESPONDING lp_sum TO p_sum.
    APPEND p_sum.
    CLEAR: lp_sum.
  ENDLOOP.

*  p_sum[] = lp_sum[].


ENDFUNCTION.                    " check_condition
