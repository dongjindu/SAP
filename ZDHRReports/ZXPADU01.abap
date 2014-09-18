*----------------------------------------------------------------------*
*   INCLUDE ZXPADU01                                                   *
*----------------------------------------------------------------------*
*05/14/2008   IG.MOON   UD1K943614    Create INCLUDE ZXPADU01
*05/14/2008   IG.MOON   UD1K943614    Delimit STD with
*                                     Emp.Sub Group Change
*02/25/2011   Valerian  UD1K950950    Correct 401K loan balance issue
*09/30/2011   Valerian  UP1K920223    Correct Unicode issue(BY KDM)

* by ig.moon UD1K943614 {
  DATA: i0001 LIKE p0001.
  DATA: BEGIN OF w_pa0168.
          INCLUDE STRUCTURE pa0168.
  DATA: END OF w_pa0168.

* BEGIN OF UD1K950950
  DATA: i0015          TYPE p0015,
        it_rgdir       TYPE STANDARD TABLE OF pc261
                            WITH NON-UNIQUE DEFAULT KEY,
        wa_rgdir       TYPE pc261,
        wa_t500l       TYPE t500l,
        payroll_result TYPE pay99_result,
        inter          TYPE pay99_international,
        it_rt          TYPE hrpay99_rt,
        wa_rt          TYPE pc207,
        l_molga        TYPE molga,
        l_lines        TYPE i,
        l_balded       TYPE subty.
* END OF UD1K950950

  DATA: l_PS0001 TYPE PS0001,
        l_PS0015 TYPE PS0015,
        l_cont(250) TYPE C.

  CASE innnn-infty.
    WHEN '0001'.
      IF tclas EQ 'A'.      " Employee

*        MOVE innnn TO i0001.
        MOVE-corresponding innnn TO i0001.

*** >> Code Add Unicode Issue(09/30/2011, KDM).
          call method cl_abap_container_utilities=>read_container_c
            exporting  IM_CONTAINER           = innnn-data1
            importing  EX_VALUE               = l_PS0001
            exceptions ILLEGAL_PARAMETER_TYPE = 1
                       others                 = 2.

          MOVE-CORRESPONDING l_PS0001 TO i0001.


*        IF ipsyst-persk EQ 'U2' AND                       "new
*        ( i0001-persk EQ 'U0' OR i0001-persk EQ 'U3' ).   "old

        IF ipsyst-persk EQ 'U2'.

*          BREAK-POINT.

          SELECT SINGLE * INTO w_pa0168
                  FROM pa0168
                   WHERE pernr EQ innnn-pernr
                     AND subty EQ 'STD'
                     AND begda LE innnn-begda
                     AND endda GE innnn-begda.

          IF sy-subrc EQ 0.
            UPDATE pa0168
                   SET endda = innnn-begda
                       aedtm = sy-datum
                       uname = sy-uname
                   WHERE pernr EQ w_pa0168-pernr
                     AND subty EQ w_pa0168-subty
                     AND begda LE w_pa0168-begda
                     AND endda GE w_pa0168-endda.
          ENDIF.

        ENDIF.

        IF i0001-persk EQ 'U2' AND                         "old
        ( ipsyst-persk EQ 'U0' OR  ipsyst-persk EQ 'U3' ). "new

*          BREAK-POINT.

          SELECT SINGLE * INTO w_pa0168
                  FROM pa0168
                   WHERE pernr EQ innnn-pernr
                     AND subty EQ 'STD'
                     AND begda LE innnn-begda
                     AND endda GE innnn-begda.

          IF sy-subrc EQ 0.

            UPDATE pa0168
                   SET endda = innnn-begda
                       aedtm = sy-datum
                       uname = sy-uname
                   WHERE pernr EQ w_pa0168-pernr
                     AND subty EQ w_pa0168-subty
                     AND begda LE w_pa0168-begda
                     AND endda GE w_pa0168-endda.

            IF sy-subrc EQ 0.
              w_pa0168-endda = innnn-begda.
              w_pa0168-aedtm = sy-datum.
              w_pa0168-uname = sy-uname.

              INSERT pa0168 FROM w_pa0168.
            ENDIF.

          ELSE.

            w_pa0168-pernr =  innnn-pernr.
            w_pa0168-subty =  'STD'.
            w_pa0168-endda =  '99991231'.
            w_pa0168-begda =  innnn-begda.
            w_pa0168-aedtm =  sy-datum.
            w_pa0168-uname =  sy-uname.
            w_pa0168-barea =  '10'.
            w_pa0168-pltyp =  'STD'.
            w_pa0168-bplan =  'STD'.
            w_pa0168-bcovr =  'STD'.

            INSERT pa0168 FROM w_pa0168.

          ENDIF.


        ENDIF.
      ENDIF.

* BEGIN OF UD1K950950.
    WHEN '0015'.
      IF tclas = 'A'.
        IF innnn-subty = '2019' OR innnn-subty = '2023'.

*          MOVE innnn TO i0015.
          MOVE-corresponding innnn TO i0015.
*** >> Code Add Unicode Issue(09/30/2011, KDM).
          call method cl_abap_container_utilities=>read_container_c
            exporting  IM_CONTAINER           = innnn-data1
            importing  EX_VALUE               = l_PS0015
            exceptions ILLEGAL_PARAMETER_TYPE = 1
                       others                 = 2.

          MOVE-CORRESPONDING l_PS0015 TO i0015.

*         Get Balance Deduction Code
          CASE innnn-subty.
            WHEN '2019'.
              l_balded = '3019'.
            WHEN '2023'.
              l_balded = '3023'.
          ENDCASE.

*         Read RGDIR
          CALL FUNCTION 'CU_READ_RGDIR'
               EXPORTING
                    persnr          = innnn-pernr
               IMPORTING
                    molga           = l_molga
               TABLES
                    in_rgdir        = it_rgdir
               EXCEPTIONS
                    no_record_found = 1
                    OTHERS          = 2.

          IF sy-subrc = 0.

*           Filter RGDIR
            DELETE it_rgdir WHERE voidr <> space OR
                                  srtza <> 'A'   OR
                                  fpper =  '000000'.
            SORT it_rgdir BY seqnr.
            DESCRIBE TABLE it_rgdir LINES l_lines.
            READ TABLE it_rgdir INTO wa_rgdir INDEX l_lines.

*           Determine cluster ID
            CALL FUNCTION 'HRPY_READ_T500L'
                 EXPORTING
                      molga          = l_molga
                 IMPORTING
                      t500l_entry    = wa_t500l
                 EXCEPTIONS
                      no_entry_found = 1
                      OTHERS         = 2.

            IF sy-subrc = 0.

*             Read payroll results
              CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
                   EXPORTING
                        clusterid                    = wa_t500l-relid
                        employeenumber               = innnn-pernr
                        sequencenumber               = wa_rgdir-seqnr
                        read_only_international      = 'X'
                   CHANGING
                        payroll_result               = payroll_result
                   EXCEPTIONS
                        illegal_isocode_or_clusterid = 1
                        error_generating_import      = 2
                        import_mismatch_error        = 3
                        subpool_dir_full             = 4
                        no_read_authority            = 5
                        no_record_found              = 6
                        versions_do_not_match        = 7
                        error_reading_archive        = 8
                        error_reading_relid          = 9
                        OTHERS                       = 10.

              IF sy-subrc <> 0.
              ENDIF.
            ENDIF.
          ENDIF.

*         Check for payroll result and read previous amount
          IF NOT payroll_result IS INITIAL.
            inter = payroll_result-inter.
            it_rt = inter-rt.
            READ TABLE it_rt INTO wa_rt
                             WITH KEY lgart = l_balded.
            IF sy-subrc = 0.
              i0015-betrg = wa_rt-betrg * -1.
            ELSE.
              CALL FUNCTION 'POPUP_FOR_INTERACTION'
                EXPORTING
                headline = 'Warning'
                text1 = 'No 401K loan balance from last payroll result'
                ticon    = 'W'
                button_1 = 'OK'.
              EXIT.
            ENDIF.
          ENDIF.

          i0015-preas = '03'.

*          MOVE i0015 TO innnn.
*           MOVE-corresponding i0015 TO innnn.
*** >> Code Change Unicode Issue(09/30/2011, KDM).
           MOVE-corresponding i0015 TO innnn.
           MOVE-corresponding i0015 TO l_PS0015.

        class CL_ABAP_CONTAINER_UTILITIES definition load.

        call method CL_ABAP_CONTAINER_UTILITIES=>FILL_CONTAINER_C
          exporting  IM_VALUE               = l_PS0015
          importing  EX_CONTAINER           = innnn-data1
          exceptions ILLEGAL_PARAMETER_TYPE = 1
                     others                 = 2.

        call method cl_abap_container_utilities=>read_container_c
          exporting  IM_CONTAINER           = innnn-data1
          importing  EX_VALUE               = l_PS0015
          exceptions ILLEGAL_PARAMETER_TYPE = 1
                     others                 = 2.

        ENDIF.
      ENDIF.
* END OF UD1K950950

    WHEN OTHERS.
  ENDCASE.

* }
