*----------------------------------------------------------------------*
*   INCLUDE ZESD11L_BILLING_HAC_F01                                    *
*----------------------------------------------------------------------*

** Modified by Haseeb Mohammad on 08-01-2006 to remove Duplicate ******
** Billing in HAC multiple routings. Requested by Lance Younce/  ******
** Kevin Able                                                    ******

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.
  DATA: l_atinn25 LIKE cabn-atinn,
        l_atinn27 LIKE cabn-atinn.

  REFRESH : r_kunag, it_list, it_list_h.
  CLEAR   : r_kunag, it_list, it_list_h.

  r_kunag-sign = 'I'.
  r_kunag-option = 'BT'.

  r_kunag-low  = 'B06AA'.
  r_kunag-high = 'B06ZZ'.
  APPEND r_kunag.

 SELECT k~lifex k~vbeln k~route p~matnr p~arktx p~lfimg p~vrkme p~vbelv
         INTO (it_list-lifex, it_list-vbeln, it_list-route,
               it_list-matnr, it_list-arktx, it_list-lfimg,
               it_list-vrkme, it_list-vbelv)
         FROM lips AS p INNER JOIN likp AS k
         ON p~vbeln EQ k~vbeln
      WHERE k~lddat IN s_fkdat
      AND   k~kunnr IN r_kunag
      AND   k~wadat_ist EQ '00000000' "NOT POST G/I
      AND   k~lifex NE ''.
    APPEND it_list. CLEAR it_list.
  ENDSELECT.

  SELECT SINGLE atinn INTO l_atinn25
           FROM cabn
          WHERE atnam = 'P_RP25_ACTUAL_DATE'.
  SELECT SINGLE atinn INTO l_atinn27
             FROM cabn
            WHERE atnam = 'P_RP27_ACTUAL_DATE'.
* CHECK G/I
  LOOP AT it_list.
    w_tabix = sy-tabix.

    SELECT SINGLE *
           FROM ausp
          WHERE objek EQ it_list-vbeln
          AND   klart = '002'
          AND   atinn EQ l_atinn25.

    IF sy-subrc = 0 AND NOT ausp-atwrt IS INITIAL.
    ELSE.

      SELECT SINGLE *
             FROM ausp
            WHERE objek EQ it_list-vbeln
            AND   klart = '002'
            AND   atinn EQ l_atinn27.
      IF sy-subrc = 0 AND NOT ausp-atwrt IS INITIAL.
      ELSE.
        DELETE it_list INDEX w_tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT it_list.
    w_tabix = sy-tabix.

    SELECT SINGLE * FROM tvrot
           WHERE spras = 'E'
           AND   route = it_list-route.
    IF sy-subrc = 0.
      it_list-bezei = tvrot-bezei.
    ELSE.
      it_list-bezei = ''.
    ENDIF.

    SELECT SINGLE * FROM vbak
           WHERE vbeln = it_list-vbelv.
    IF sy-subrc = 0.
      it_list-netwr = vbak-netwr.
      it_list-waerk = vbak-waerk.
      MODIFY it_list INDEX w_tabix.
    ELSE.
      DELETE it_list INDEX w_tabix.
    ENDIF.
  ENDLOOP.

  LOOP AT it_list.
    MOVE-CORRESPONDING it_list TO it_list_h.
    it_list_h-flag = 'X'.
    COLLECT it_list_h.

    MOVE-CORRESPONDING it_list TO it_list_m.
    COLLECT it_list_m.
  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM display_data.
  DESCRIBE TABLE it_list LINES w_cnt.
  IF w_cnt = 0.
    MESSAGE s000 WITH text-m01.
  ELSE.
    PERFORM display_list.
  ENDIF.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST
*&---------------------------------------------------------------------*
FORM display_list.
  DATA : w_first(1).

  SORT : it_list, it_list_h, it_list_m.

** Modified by Haseeb Mohammad on 08-01-2006 to remove Duplicate ******
** Billing in HAC multiple routings. Requested by Lance Younce/  ******
** Kevin Able                                                    ******

  DELETE ADJACENT DUPLICATES FROM it_list_m COMPARING lifex.

** END of change by Haseeb Mohammad.                            *******


  LOOP AT it_list_h.
    WRITE:/ sy-vline,      it_list_h-flag AS CHECKBOX NO-GAP, '',
                      (15) it_list_h-lifex,
                      (04) '',
            sy-vline, (10) '',
            sy-vline, (10) '',
            sy-vline, (18) '',
            sy-vline, (40) '',
            sy-vline, (04) '',
            sy-vline, (04) '',
            sy-vline, (12) '',
            sy-vline, (04) '',
            sy-vline, (10) '',
            sy-vline.
    LOOP AT it_list_m WHERE lifex EQ it_list_h-lifex.
      WRITE:/ sy-vline, (02) '',
                        (20) it_list_m-bezei+0(20).
      w_first = 'Y'.
      LOOP AT it_list WHERE lifex EQ it_list_m-lifex
                      AND   bezei EQ it_list_m-bezei.
        IF w_first = 'Y'.
          WRITE:  sy-vline, (10) it_list-vbeln.
          w_first = 'N'.
        ELSE.
          WRITE:/ sy-vline, (02) '',
                            (20) '',
                  sy-vline, (10) it_list-vbeln.
        ENDIF.
        WRITE:  sy-vline, (10) it_list-vbeln_b,
                sy-vline, (18) it_list-matnr,
                sy-vline, (40) it_list-arktx,
                sy-vline, (04) it_list-lfimg UNIT it_list-vrkme,
                sy-vline, (04) it_list-vrkme,
                sy-vline, (12) it_list-netwr CURRENCY it_list-waerk,
                sy-vline, (04) it_list-waerk,
                sy-vline, (10) it_list-vbelv,
                sy-vline.
      ENDLOOP.
    ENDLOOP.
    WRITE:/(166) sy-uline.
  ENDLOOP.
ENDFORM.                    " DISPLAY_LIST
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  FORMAT COLOR COL_HEADING.
  WRITE:/(166) sy-uline.
  WRITE:/ sy-vline, (02) '',
                    (15) 'Rail Car No',
                    (04) '',
          sy-vline, (10) '',
          sy-vline, (10) '',
          sy-vline, (18) '',
          sy-vline, (40) '',
          sy-vline, (04) '',
          sy-vline, (04) '',
          sy-vline, (12) '',
          sy-vline, (04) '',
          sy-vline, (10) '',
          sy-vline.
  WRITE:/ sy-vline, (02) '',
                    (15) 'Destination',
                    (04) '',
          sy-vline, (10) 'Delivery',
          sy-vline, (10) 'Billing',
          sy-vline, (18) 'Material',
          sy-vline, (40) 'Description',
          sy-vline, (04) 'Qty.',
          sy-vline, (04) 'Unit',
          sy-vline, (12) 'Net value',
          sy-vline, (04) 'CurK',
          sy-vline, (10) 'SalesOrder',
          sy-vline.
  WRITE:/(166) sy-uline.
  FORMAT COLOR COL_HEADING OFF.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command.
  DATA : ok_code(4).
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SELE'.
      PERFORM select_all.
    WHEN 'DSEL'.
      PERFORM deselect_all.
    WHEN 'PROC'.
      PERFORM process_data.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DOC
*&---------------------------------------------------------------------*
FORM display_doc.
  GET CURSOR FIELD w_field
             VALUE w_value.

  CHECK NOT w_value IS INITIAL.

  CASE w_field.
    WHEN 'IT_LIST-VBELN'.   "DELIVERY
      SET PARAMETER ID 'VL'  FIELD w_value.
      CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
    WHEN 'IT_LIST-VBELN_B'. "BILLING
      CHECK w_value+0(1) NE 'E'.
      SET PARAMETER ID 'VF'  FIELD w_value.
      CALL TRANSACTION 'VF03'  AND SKIP FIRST SCREEN.
    WHEN 'IT_LIST-VBELV'.   "SALES ORDER
      SET PARAMETER ID 'AUN' FIELD w_value.
      CALL TRANSACTION 'VA03'  AND SKIP FIRST SCREEN.
  ENDCASE.
ENDFORM.                    " DISPLAY_DOC
*&---------------------------------------------------------------------*
*&      Form  SELECT_ALL
*&---------------------------------------------------------------------*
FORM select_all.
  LOOP AT it_list_h.
    it_list_h-flag = 'X'.
    MODIFY it_list_h.
  ENDLOOP.

  sy-lsind = sy-lsind - 1.
  PERFORM display_list.
ENDFORM.                    " SELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  DESELECT_ALL
*&---------------------------------------------------------------------*
FORM deselect_all.
  LOOP AT it_list_h.
    it_list_h-flag = ''.
    MODIFY it_list_h.
  ENDLOOP.

  sy-lsind = sy-lsind - 1.
  PERFORM display_list.
ENDFORM.                    " DESELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  GET_MARK
*&---------------------------------------------------------------------*
FORM get_mark.
  PERFORM del_mark.

  CLEAR : w_flag, w_lifex.

  DO.
    READ LINE sy-index FIELD VALUE it_list_h-flag  INTO w_flag
                                   it_list_h-lifex INTO w_lifex.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    IF w_flag = 'X'.
      LOOP AT it_list_h WHERE lifex = w_lifex.
        it_list_h-flag = w_flag.
        MODIFY it_list_h INDEX sy-tabix.
      ENDLOOP.
    ENDIF.
  ENDDO.
ENDFORM.                    " GET_MARK
*&---------------------------------------------------------------------*
*&      Form  DEL_MARK
*&---------------------------------------------------------------------*
FORM del_mark.
  LOOP AT it_list_h.
    it_list_h-flag = ''.
    MODIFY it_list_h INDEX sy-tabix.
  ENDLOOP.
ENDFORM.                    " DEL_MARK
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
FORM process_data.
  PERFORM get_mark.

  READ TABLE it_list_h WITH KEY flag = 'X'.
  IF sy-subrc <> 0.
    MESSAGE i000 WITH text-m02.
    EXIT.
  ENDIF.

  LOOP AT it_list_h WHERE flag = 'X'.
    LOOP AT it_list_m WHERE lifex = it_list_h-lifex.

      READ TABLE it_list WITH KEY lifex = it_list_m-lifex
                                  bezei = it_list_m-bezei.

      CASE it_list-vbeln_b.
        WHEN 'E:BILL.DUE'.
          www = 'E'.
          PERFORM seek_sammg.
          CHECK w_result = 'Y'.

          PERFORM billing_index.
        WHEN 'E:POST.G/I'.
          www = 'E'.
          PERFORM seek_sammg.
          CHECK w_result = 'Y'.

          PERFORM post_gi.
          CHECK w_result = 'Y'.

          PERFORM billing_index.
        WHEN 'E:LOAD.GRP'.
          www = 'E'.
          PERFORM creat_loading_grp.
          CHECK w_result = 'Y'.

          PERFORM post_gi.
          CHECK w_result = 'Y'.

          PERFORM billing_index.
        WHEN OTHERS. " ''
          www = 'N'.
          PERFORM creat_loading_grp.
          CHECK w_result = 'Y'.

          PERFORM post_gi.
          CHECK w_result = 'Y'.

          PERFORM billing_index.
      ENDCASE.

    ENDLOOP.
  ENDLOOP.

  sy-lsind = sy-lsind - 1.
  PERFORM display_list.
ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  CREAT_LOADING_GRP
*&---------------------------------------------------------------------*
FORM creat_loading_grp.

  REFRESH : bdc_tab, mess_tab.
  CLEAR   : bdc_tab, mess_tab.

*** : ECC BDC Logic change(11/28/2011 BY KDM)
*  LOOP AT it_list WHERE lifex = it_list_m-lifex
*                  AND   bezei = it_list_m-bezei.
*
*    PERFORM bdc_fill USING :
*            'X' 'SAPMV08A'             '0210',
*            ' ' l_field          it_list-vbeln,
*            ' ' 'BDC_OKCODE'           '/00',
*              'X' 'SAPMV08A'             '0210',
*              ' ' 'BDC_OKCODE'           '=P+'.
*  ENDLOOP.

  DATA : l_field(14),
         l_cnt(02) TYPE N.

  l_cnt = '00'.

  LOOP AT it_list WHERE lifex = it_list_m-lifex
                  AND   bezei = it_list_m-bezei.

    l_cnt = l_cnt + 1.
    CONCATENATE 'VBSS-VBELN(' l_cnt ')' INTO l_field.

    PERFORM bdc_fill USING :
            'X' 'SAPMV08A'             '0210',
            ' ' l_field          it_list-vbeln,
            ' ' 'BDC_OKCODE'           '/00'.

    IF l_cnt >= 28.
      PERFORM bdc_fill USING :
              'X' 'SAPMV08A'             '0210',
              ' ' 'BDC_OKCODE'           '=P+'.
      l_cnt = 1.
    ENDIF.
  ENDLOOP.
*** : ECC BDC Logic change(11/28/2011 BY KDM) - End

  PERFORM bdc_fill USING :
          'X' 'SAPMV08A'             '0210',
          ' ' 'BDC_OKCODE'           '=PICK',
          'X' 'SAPMV08A'             '0205',
          ' ' 'VBSK-VTEXT'           it_list-bezei+0(30),
          ' ' 'VBSK-VSTEL'           'P100',
          ' ' 'BDC_OKCODE'           '=SICH'.

  CALL TRANSACTION 'VGM1' USING bdc_tab MODE www "'N'
                                UPDATE 'S'
                                MESSAGES INTO mess_tab.

  READ TABLE mess_tab WITH KEY msgtyp = 'S'
                               msgid  = 'VL'
                               msgnr  = '804'.
  IF sy-subrc = 0.
    w_result = 'Y'.
    w_sammg = mess_tab-msgv1.
    PERFORM conversion_exit_alpha_input USING w_sammg.
  ELSE.
    LOOP AT it_list WHERE lifex = it_list_m-lifex
                    AND   bezei = it_list_m-bezei.
      it_list-vbeln_b = 'E:LOAD.GRP'.
      MODIFY it_list INDEX sy-tabix.
    ENDLOOP.
    w_result = 'N'.
    w_sammg = ''.
  ENDIF.
ENDFORM.                    " CREAT_LOADING_GRP
*&---------------------------------------------------------------------*
*&      Form  POST_GI
*&---------------------------------------------------------------------*
FORM post_gi.
  REFRESH : bdc_tab, mess_tab.
  CLEAR   : bdc_tab, mess_tab.

  PERFORM get_act_gidate.

  PERFORM bdc_fill USING :
          'X' 'WS_MONITOR_OUTB_DEL_GDSI' '1000',
          ' ' 'IT_WADAT-LOW'             '',
          ' ' 'IT_WADAT-HIGH'            '',
          ' ' 'IT_SAMMG-LOW'             w_sammg,
          ' ' 'BDC_OKCODE'               '=ONLI',
          'X' 'SAPMSSY0'                 '0120',
          ' ' 'BDC_OKCODE'               '=&ALL',
          'X' 'SAPMSSY0'                 '0120',
          ' ' 'BDC_OKCODE'               '=WABU',
          'X' 'SAPLV50Q'                 '1100',
          ' ' 'LIKP-WADAT_IST'           w_c_8_gi,
          ' ' 'BDC_OKCODE'               '=WEIT',
          'X' 'SAPMSSY0'                 '0120',
          ' ' 'BDC_OKCODE'               '=&F03',
          'X' 'WS_MONITOR_OUTB_DEL_GDSI' '1000',
          ' ' 'BDC_OKCODE'               '/EE'.

  DATA: X_PARAMS LIKE CTU_PARAMS.
  X_PARAMS-DISMODE = 'N'.
  X_PARAMS-UPDMODE = 'S'.
  X_PARAMS-RACOMMIT = 'X'.

*  CALL TRANSACTION 'VL06G' USING bdc_tab MODE www "'N'
*                                 UPDATE 'S'
*                                 OPTIONS FROM X_PARAMS
*                                 MESSAGES INTO mess_tab.

  CALL TRANSACTION 'VL06G' USING bdc_tab OPTIONS FROM X_PARAMS
                                         MESSAGES INTO mess_tab.

  READ TABLE mess_tab WITH KEY msgtyp = 'S'
                               msgid  = 'VLA'
                               msgnr  = '019'.
  IF sy-subrc = 0.
    w_result = 'Y'.
  ELSE.
    LOOP AT it_list WHERE lifex = it_list_m-lifex
                    AND   bezei = it_list_m-bezei.
      it_list-vbeln_b = 'E:POST.G/I'.
      MODIFY it_list INDEX sy-tabix.
    ENDLOOP.
    w_result = 'N'.
  ENDIF.
ENDFORM.                    " POST_GI
*&---------------------------------------------------------------------*
*&      Form  GET_ACT_GIDATE
*&---------------------------------------------------------------------*
FORM get_act_gidate.
  SELECT SINGLE *
         FROM usr01
        WHERE bname = sy-uname.
  CASE usr01-datfm.
    WHEN '1'. "DD.MM.YYYY
      w_c_8_gi+4(4) = s_fkdat-low+0(4).
      w_c_8_gi+2(2) = s_fkdat-low+4(2).
      w_c_8_gi+0(2) = s_fkdat-low+6(2).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      w_c_8_gi+4(4) = s_fkdat-low+0(4).
      w_c_8_gi+0(2) = s_fkdat-low+4(2).
      w_c_8_gi+2(2) = s_fkdat-low+6(2).
  ENDCASE.
ENDFORM.                    " GET_ACT_GIDATE
*&---------------------------------------------------------------------*
*&      Form  BILLING_INDEX
*&---------------------------------------------------------------------*
FORM billing_index.
  REFRESH : bdc_tab, mess_tab.
  CLEAR   : bdc_tab, mess_tab.

** : BDC Logic modify(11/28/2011 BY KDM)
*  PERFORM bdc_fill USING :
*          'X' 'SDBILLDL'             '1000',
*          ' ' 'P_SORT-LOW'           w_sammg,
*          ' ' 'BDC_OKCODE'           '=ONLI',
*          'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*          ' ' 'BDC_OKCODE'           '=&ALL',
*          'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*          ' ' 'BDC_OKCODE'           '=SAMH',
*          'X' 'SAPMV60A'             '0104',
*          ' ' 'BDC_OKCODE'           '=SICH',
*          'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*          ' ' 'BDC_OKCODE'           '=&F03',
*          'X' 'SDBILLDL'             '1000',
*          ' ' 'BDC_OKCODE'           '/EERW'.

  PERFORM bdc_fill USING :
          'X' 'SDBILLDL'             '1000',
          ' ' 'P_SORT-LOW'           w_sammg,
          ' ' 'P_ALLEL'              'X',
          ' ' 'BDC_OKCODE'           '=ONLI',
          'X' 'SAPMSSY0'             '0120',
          ' ' 'BDC_OKCODE'           '=SAMH',
          'X' 'SAPMV60A'             '0104',
          ' ' 'BDC_OKCODE'           '=SICH',
          'X' 'SAPMSSY0'             '0120',
          ' ' 'BDC_OKCODE'           '=&F03',
          'X' 'SDBILLDL'             '1000',
          ' ' 'BDC_OKCODE'           '/EERW'.
** : BDC Logic modify(11/28/2011 BY KDM) - End

  CALL TRANSACTION 'VF04' USING bdc_tab MODE www "'N'
                                UPDATE 'S'
                                MESSAGES INTO mess_tab.

  READ TABLE mess_tab WITH KEY msgtyp = 'S'
                               msgid  = 'VF'.
  " MSGNR  = '050', '051', '311'
  IF sy-subrc = 0.
    LOOP AT it_list WHERE lifex = it_list_m-lifex
                    AND   bezei = it_list_m-bezei.
      it_list-vbeln_b = mess_tab-msgv1.
      MODIFY it_list INDEX sy-tabix.
    ENDLOOP.
    w_result = 'Y'.
  ELSE.
    LOOP AT it_list WHERE lifex = it_list_m-lifex
                    AND   bezei = it_list_m-bezei.
      it_list-vbeln_b = 'E:BILL.DUE'.
      MODIFY it_list INDEX sy-tabix.
    ENDLOOP.
    w_result = 'N'.
  ENDIF.
ENDFORM.                    " BILLING_INDEX
*&---------------------------------------------------------------------*
*&      Form  SEEK_SAMMG
*&---------------------------------------------------------------------*
FORM seek_sammg.
  SELECT * FROM vbss
          WHERE vbeln EQ it_list-vbeln.
  ENDSELECT.
  IF sy-subrc = 0.
    w_result = 'Y'.
    w_sammg = vbss-sammg.
    PERFORM conversion_exit_alpha_input USING w_sammg.
  ELSE.
    w_result = 'N'.
    w_sammg = ''.
  ENDIF.
ENDFORM.                    " SEEK_SAMMG
*&---------------------------------------------------------------------*
*&      Form  BDC_FILL
*&---------------------------------------------------------------------*
FORM bdc_fill USING    p1 p2 p3.
  CLEAR bdc_tab.
  IF p1 = 'X'.
    bdc_tab-dynbegin = p1.
    bdc_tab-program  = p2.
    bdc_tab-dynpro   = p3.
  ELSE.
    bdc_tab-dynbegin = p1.
    bdc_tab-fnam     = p2.
    bdc_tab-fval     = p3.
  ENDIF.
  APPEND bdc_tab.
ENDFORM.                    " BDC_FILL
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
FORM conversion_exit_alpha_input USING ppppp.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = ppppp
       IMPORTING
            output = ppppp.
ENDFORM.                    " CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_OUTPUT
*&---------------------------------------------------------------------*
FORM conversion_exit_alpha_output USING ppppp.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = ppppp
       IMPORTING
            output = ppppp.
ENDFORM.                    " CONVERSION_EXIT_ALPHA_OUTPUT
