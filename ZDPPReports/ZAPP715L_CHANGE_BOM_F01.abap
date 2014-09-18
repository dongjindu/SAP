*----------------------------------------------------------------------*
*   INCLUDE ZAPP715L_CHANGE_BOM_F01                                    *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  MATNR_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM matnr_value_request USING p_ind.
  DATA : BEGIN OF it_matnr OCCURS 0,
           matnr  TYPE  ztpp_comppartvin-matnr,
           verid  TYPE  ztpp_comppartvin-verid,
           maktx  TYPE  makt-maktx.
  DATA : END OF it_matnr.
  DATA : l_size   TYPE  i ,
         l_matnr  TYPE  ztpp_comppartvin-matnr.

  CLEAR it_dynpread. REFRESH it_dynpread.
  CLEAR it_valuetab. REFRESH it_valuetab.
  CLEAR it_fields.   REFRESH it_fields.

  CASE p_ind.
    WHEN 'MATNR'.
      SELECT matnr  INTO CORRESPONDING FIELDS OF TABLE it_matnr
        FROM mara
       WHERE mtart  =  'FERT'
          OR mtart  =  'HALB' .
      LOOP AT it_matnr.
        l_size = strlen( it_matnr-matnr ) .
        IF l_size NE 18 .
          DELETE it_matnr .
        ENDIF.
      ENDLOOP.
    WHEN 'VERID'.
      PERFORM value_read USING 'P_MATNR'.
      LOOP AT it_dynpread.
        CASE sy-tabix.
          WHEN 1.
            CONCATENATE it_dynpread-fieldvalue '%' INTO l_matnr.
        ENDCASE.
      ENDLOOP.
      SELECT matnr verid  INTO CORRESPONDING FIELDS OF TABLE it_matnr
        FROM mkal
       WHERE matnr  =  l_matnr
         AND werks  =  'P001' .
  ENDCASE.

  CLEAR it_dynpread. REFRESH it_dynpread.

  SORT it_matnr BY matnr verid.
  DELETE ADJACENT DUPLICATES FROM it_matnr.
  LOOP AT it_matnr.
    it_valuetab-value = it_matnr-matnr.
    APPEND it_valuetab. CLEAR it_valuetab.
    it_valuetab-value = it_matnr-verid.
    APPEND it_valuetab. CLEAR it_valuetab.
    SELECT SINGLE maktx
               INTO it_valuetab-value
               FROM makt
               WHERE matnr EQ it_matnr-matnr
                 AND spras EQ sy-langu.
    APPEND it_valuetab. CLEAR it_valuetab.
  ENDLOOP.

  PERFORM add_fields USING: 'MKAL'             'MATNR' 'X',
                            'MKAL'             'VERID' 'X',
                            'MAKT'             'MAKTX' ' '.

  PERFORM help_values_get.

  IF wa_select_ix > 0.
    READ TABLE it_matnr   INDEX wa_select_ix.
    PERFORM value_update USING:
            ' '   'P_MATNR' it_matnr-matnr 0,
            'X'   'P_VERID' it_matnr-verid 0.
  ENDIF.
ENDFORM.                    " MATNR_VALUE_REQUEST

*&---------------------------------------------------------------------*
*&      Form  AENNR_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM aennr_value_request USING p_ind.
  DATA : BEGIN OF it_matnr OCCURS 0,
           aennr  LIKE  ztpp_plan_comp-aennr,
           matnr  LIKE  ztpp_plan_comp-matnr,
           fsc    LIKE  ztpp_plan_comp-fsc  ,
           maktx  TYPE  makt-maktx,
         END OF it_matnr.

  CLEAR it_dynpread. REFRESH it_dynpread.
  CLEAR it_valuetab. REFRESH it_valuetab.
  CLEAR it_fields.   REFRESH it_fields.

  CASE p_ind.
    WHEN 'AENNR'.
      SELECT *      INTO CORRESPONDING FIELDS OF TABLE it_matnr
        FROM ztpp_plan_comp  .
    WHEN 'MATNR'.
      PERFORM value_read USING 'ST5000-AENNR'.
      READ TABLE it_dynpread INDEX 1.
      SELECT *      INTO CORRESPONDING FIELDS OF TABLE it_matnr
        FROM ztpp_plan_comp
       WHERE aennr  =  it_dynpread-fieldvalue .
  ENDCASE.

  CLEAR it_dynpread. REFRESH it_dynpread.

  SORT it_matnr BY aennr matnr.
  DELETE ADJACENT DUPLICATES FROM it_matnr.
  LOOP AT it_matnr.
    it_valuetab-value = it_matnr-aennr.
    APPEND it_valuetab. CLEAR it_valuetab.
    it_valuetab-value = it_matnr-matnr.
    APPEND it_valuetab. CLEAR it_valuetab.
    it_valuetab-value = it_matnr-fsc  .
    APPEND it_valuetab. CLEAR it_valuetab.
    SELECT SINGLE maktx
               INTO it_valuetab-value
               FROM makt
               WHERE matnr EQ it_matnr-fsc
                 AND spras EQ sy-langu.
    APPEND it_valuetab. CLEAR it_valuetab.
  ENDLOOP.

  PERFORM add_fields USING: 'ZTPP_PLAN_COMP'   'AENNR' 'X',
                            'ZTPP_PLAN_COMP'   'MATNR' 'X',
                            'ZTPP_PLAN_COMP'   'FSC'   'X',
                            'ZTPP_PLAN_COMP'   'MAKTX' ' '.

  PERFORM help_values_get.

  IF wa_select_ix > 0.
    READ TABLE it_matnr   INDEX wa_select_ix.
    PERFORM value_update USING:
            'X'   'ST5000-AENNR' it_matnr-aennr 0,
            ' '   'ST5000-MATNR' it_matnr-matnr 0.
  ENDIF.
ENDFORM.                    " AENNR_VALUE_REQUEST

*&---------------------------------------------------------------------*
*&      Form  VALUE_READ
*&---------------------------------------------------------------------*
FORM value_read USING  p_name.
  it_dynpread-fieldname = p_name. APPEND it_dynpread.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname     = sy-cprog
            dynumb     = sy-dynnr
       TABLES
            dynpfields = it_dynpread.
ENDFORM.                    " VALUE_READ

*&---------------------------------------------------------------------*
*&      Form  HELP_VALUES_GET
*&---------------------------------------------------------------------*
FORM help_values_get.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            display                   = ' '
       IMPORTING
            index                     = wa_select_ix
       TABLES
            fields                    = it_fields
            select_values             = it_select_values
            valuetab                  = it_valuetab
       EXCEPTIONS
            field_not_in_ddic         = 1
            more_then_one_selectfield = 2
            no_selectfield            = 3
            OTHERS                    = 4.
ENDFORM.                    " HELP_VALUES_GET

*&---------------------------------------------------------------------*
*&      Form  VALUE_UPDATE
*&---------------------------------------------------------------------*
FORM value_update USING  p_process
                         p_fieldname
                         p_fieldvalue
                         p_stepl.
  CLEAR it_dynpfields.
  it_dynpfields-fieldname = p_fieldname.
  it_dynpfields-fieldvalue = p_fieldvalue.
  IF p_stepl > 0.
    it_dynpfields-stepl = p_stepl.
  ENDIF.
  APPEND it_dynpfields.      CLEAR it_dynpfields.

  IF p_process EQ 'X'.
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
         EXPORTING
              dyname               = sy-cprog
              dynumb               = sy-dynnr
         TABLES
              dynpfields           = it_dynpfields
         EXCEPTIONS
              invalid_abapworkarea = 1
              invalid_dynprofield  = 2
              invalid_dynproname   = 3
              invalid_dynpronummer = 4
              invalid_request      = 5
              no_fielddescription  = 6
              undefind_error       = 7
              OTHERS               = 8.
    REFRESH it_dynpfields.
  ENDIF.
ENDFORM.                    " VALUE_UPDATE

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data.
  LOOP AT it_screen.
    it_screen-chk = 'X'.
    MODIFY it_screen .
  ENDLOOP.
ENDFORM.                    " SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  DESELECT_DATA
*&---------------------------------------------------------------------*
FORM deselect_data .
  LOOP AT it_screen.
    it_screen-chk = ' '.
    MODIFY it_screen .
  ENDLOOP.
ENDFORM.                    " DESELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  CHANGE_PROCESS
*&---------------------------------------------------------------------*
FORM change_process.
  DATA: l_qty                 TYPE p DECIMALS 3,
        l_count               TYPE i           ,
        l_rsnum               LIKE resb-rsnum  ,
        lw_resb               LIKE resb        ,
        lt_resb               LIKE TABLE OF resb       WITH HEADER LINE.

  LOOP AT it_screen WHERE chk = c_mark.
    " Checking the Already processing the Data....
    SELECT SINGLE *
      FROM ztpp_plan_comp
     WHERE aennr = wa_ecm_no
       AND matnr = wa_newno
       AND plnum = it_screen-plnum.

    IF sy-subrc = 0.
      it_screen-msg    = text-202.
      it_screen-change = 'X'.
      MODIFY it_screen.
      CONTINUE.
    ENDIF.
    " First: Checking the Planorder has the OLD Material...
    "        and How many items..
    CLEAR: l_rsnum, l_count, lt_resb, lt_resb[].
    SELECT SINGLE rsnum INTO l_rsnum
      FROM plaf
     WHERE plnum = it_screen-plnum  .
    IF sy-subrc NE 0.
      MESSAGE i002 WITH text-303 it_screen-plnum  .
      CONCATENATE  text-303 it_screen-plnum  INTO it_screen-msg.
      it_screen-change = 'X'.
      MODIFY it_screen.
      PERFORM save_log_process      USING l_qty  .
      CONTINUE.
    ELSE.
      " Check the OLD Material in RESB Table..
      SELECT *  INTO TABLE lt_resb
        FROM resb
       WHERE rsnum = l_rsnum
         AND matnr = wa_oldno
         AND enmng = 0       .

      DESCRIBE TABLE lt_resb LINES l_count.
      IF l_count = 0.
        SELECT SINGLE * INTO lw_resb
          FROM resb
         WHERE rsnum = l_rsnum
           AND matnr = wa_oldno .
        IF sy-subrc = 0.
          MESSAGE i001 WITH text-305.
          CONCATENATE it_screen-plnum ':' text-305 INTO it_screen-msg.
        ELSE.
*          MESSAGE i002 WITH it_screen-plnum text-304.
          CONCATENATE it_screen-plnum ':' text-304 INTO it_screen-msg.
        ENDIF.
        it_screen-change = 'X'.
        it_screen-qty    = wa_qty.
        MODIFY it_screen.
        PERFORM save_log_process      USING l_qty  .
        CONTINUE.
      ELSE.
        l_qty = wa_qty / l_count .

*---> SET BDC MODE
        PERFORM set_mode.

*---> Set-up the BDC Data..
        PERFORM generate_bdc_data_new USING l_qty  .
        IF l_count > 1 .
*---> If the Record is multiple...
          PERFORM generate_bdc_data   USING l_qty  .
        ENDIF.
        PERFORM generate_bdc_data_end.
*---> Call the Transaction and Processing about the result..
        PERFORM call_transaction.
        MODIFY it_screen.
        PERFORM save_log_process      USING l_qty  .
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHANGE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  SET_MODE
*&---------------------------------------------------------------------*
FORM set_mode.
*----> SET BDC MODE OPTION
  CLEAR : wa_option_ds.
  wa_option_ds-dismode = c_mode.
  wa_option_ds-defsize = 'X'.
  wa_option_ds-updmode = 'S'.
ENDFORM.                    " SET_MODE

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM bdc_dynpro USING  p_program
                       p_dynpro.
  CLEAR it_bdcdata.
  it_bdcdata-program  = p_program.
  it_bdcdata-dynpro   = p_dynpro.
  it_bdcdata-dynbegin = 'X'.
  APPEND it_bdcdata.
ENDFORM.                    " BDC_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM bdc_field USING    p_fnam  p_fval.
*  IF P_FVAL <> Nodata.
  CLEAR it_bdcdata.
  it_bdcdata-fnam = p_fnam.
  it_bdcdata-fval = p_fval.
  APPEND it_bdcdata.
*  ENDIF.
ENDFORM.                    " BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA_NEW
*&---------------------------------------------------------------------*
FORM generate_bdc_data_new  USING pa_qty .
  DATA: l_qty(40).

  l_qty = pa_qty.  CONDENSE l_qty.

*----> PLANNED NUMBER
  PERFORM bdc_dynpro USING  'SAPMM61P'          '0101'.
  PERFORM bdc_field USING : 'BDC_CURSOR'        'RM61P-PLNUM',
                            'BDC_OKCODE'        '/00',
                            'RM61P-PLNUM'       it_screen-plnum.

*----> COMPONENT
  PERFORM bdc_dynpro USING  'SAPLM61O'          '0110'.
  PERFORM bdc_field USING : 'BDC_CURSOR'        'PLAF-MATNR',
                            'BDC_OKCODE'        '=POSU'.

**----> SORT
*  PERFORM bdc_dynpro USING  'SAPLM61Q'          '0115'.
*  PERFORM bdc_field USING : 'BDC_OKCODE'        '=SORT'.
*
**----> SORT KEY SETTING
*  PERFORM bdc_dynpro USING  'SAPLM61K'          '0650'.
*  PERFORM bdc_field USING : 'MDSORT-SOPRI(01)'  '1',
*                            'BDC_OKCODE'        '=DOSO'.

*----> Search Material..
  PERFORM bdc_dynpro USING  'SAPLM61Q'          '0115'.
  PERFORM bdc_field USING : 'BDC_CURSOR'        'MDPM-MATNR(01)' ,
                            'BDC_OKCODE'        '=SUCH'.

*----> Search POP-UP Screen..
  PERFORM bdc_dynpro USING  'SAPLM61Q'          '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'        'SUCHL-MATNR',
                            'SUCHL-MATNR'       wa_oldno     ,
                            'BDC_OKCODE'        '=SUCH'.

*----> Change the Material and Quantity..
  PERFORM bdc_dynpro USING  'SAPLM61Q'          '0115'.
  PERFORM bdc_field USING : 'MDPM-ERFMG(01)'    l_qty       ,
                            'MDPM-MATNR(01)'    wa_newno    .
ENDFORM.                    " GENERATE_BDC_DATA_NEW

*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
FORM generate_bdc_data  USING pa_qty .
  DATA: l_qty(40).

  l_qty = pa_qty.  CONDENSE l_qty.

*----> Search Material..
  PERFORM bdc_field USING   'BDC_OKCODE'        '=WESU'.
  PERFORM bdc_dynpro USING  'SAPLM61Q'          '0115'.
  PERFORM bdc_field USING : 'MDPM-MATNR(01)'    wa_newno    ,
                            'MDPM-ERFMG(01)'    l_qty       .
ENDFORM.                    " GENERATE_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA_END
*&---------------------------------------------------------------------*
FORM generate_bdc_data_end.
*---> Remain Function Code..
  PERFORM bdc_field USING   'BDC_OKCODE'        '=BACK'.

*---> SAVE
  PERFORM bdc_dynpro USING  'SAPLM61O'          '0110'.
  PERFORM bdc_field USING   'BDC_OKCODE'        '=HZPL'.
ENDFORM.                    " GENERATE_BDC_DATA_END

*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM call_transaction.
  CALL TRANSACTION 'MD12' USING it_bdcdata
                          OPTIONS FROM wa_option_ds.
  PERFORM error_text.
  CLEAR it_bdcdata.
  REFRESH it_bdcdata.
ENDFORM.                    " CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  ERROR_TEXT
*&---------------------------------------------------------------------*
FORM error_text.
  DATA : l_msg    LIKE cfgnl-msglin,
         l_tabix  TYPE sy-tabix.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       IMPORTING
            msg_lin = l_msg
       EXCEPTIONS
            OTHERS  = 1.

  CASE sy-msgty.
    WHEN 'E' OR 'A' OR 'X' OR 'W'.
      it_screen-change = 'X' .
      CONCATENATE text-306 l_msg ')'  INTO it_screen-msg.
    WHEN OTHERS.     " 'I', 'S' :SUCCESS
      it_screen-change = 'O' .
      it_screen-msg    = text-307 .
  ENDCASE.
ENDFORM.                    " ERROR_TEXT

*&---------------------------------------------------------------------*
*&      Module  SET_LISTBOX  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_listbox OUTPUT.
  IF wa_flag = ' '.
    CLEAR: xlist, xlist[], xvalue.
    wa_flag = 'X'.     wa_name = 'WA_RP'     .
    PERFORM set_field_plant   USING wa_name   wa_rp   .
    CLEAR: xlist, xlist[], xvalue.
    wa_name = 'WA_RP_TO'     .
    PERFORM set_field_plant   USING wa_name   wa_rp_to.
  ENDIF.
ENDMODULE.                 " SET_LISTBOX  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  set_field_plant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAME  text
*      -->P_WA_PLANT  text
*----------------------------------------------------------------------*
FORM set_field_plant USING p_name  p_parameter .
  CLEAR xvalue.
  MOVE '01' TO xvalue-key.
  MOVE 'Body In' TO xvalue-text.
  APPEND xvalue TO xlist.

  CLEAR xvalue.
  MOVE '02' TO xvalue-key.
  MOVE 'Paint Input' TO xvalue-text.
  APPEND xvalue TO xlist.

  CLEAR xvalue.
  MOVE '03' TO xvalue-key.
  MOVE 'Top Coating' TO xvalue-text.
  APPEND xvalue TO xlist.

  CLEAR xvalue.
  MOVE '04' TO xvalue-key.
  MOVE 'Paint Out' TO xvalue-text.
  APPEND xvalue TO xlist.

  CLEAR xvalue.
  MOVE '05' TO xvalue-key.
  MOVE 'PBS Input' TO xvalue-text.
  APPEND xvalue TO xlist.

* LIST BOX SETTING
  PERFORM list_box_function USING p_name.
  IF p_parameter IS INITIAL.
    READ TABLE xlist INTO xvalue  INDEX 1.
    p_parameter = xvalue-key.
  ENDIF.
ENDFORM.                    " set_field_plant

*&---------------------------------------------------------------------*
*&      Form  list_box_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NAME  text
*----------------------------------------------------------------------*
FORM list_box_function USING   pa_name .
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id              = pa_name  " list box
            values          = xlist
       EXCEPTIONS
            id_illegal_name = 1
            OTHERS          = 2.
ENDFORM.                    " list_box_function

*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDS
*&---------------------------------------------------------------------*
FORM add_fields USING  p_tabname p_fieldname p_flag.
  it_fields-tabname = p_tabname.
  it_fields-fieldname = p_fieldname.
  it_fields-selectflag = p_flag.
  APPEND it_fields.      CLEAR it_fields.
ENDFORM.                    " ADD_FIELDS

*&---------------------------------------------------------------------*
*&      Form  READ_PLAN_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_plan_order.
  DATA: l_modelyear       LIKE ausp-atinn,
        l_mi              LIKE ausp-atinn,
        l_ecolor          LIKE ausp-atinn,
        l_icolor          LIKE ausp-atinn,
        l_worder          LIKE ausp-atinn,
        l_sdate           LIKE ausp-atinn,
        l_bdate           LIKE ausp-atinn,
        l_rpdate          LIKE ausp-atinn,
        l_ocn             LIKE ausp-atinn,
        l_version         LIKE ausp-atinn,
        l_porder          LIKE ausp-atinn,
        l_usage_car       LIKE ausp-atinn,
        l_car(1),
        l_fsc             LIKE mara-matnr,
        l_rsnum           LIKE resb-rsnum,
        l_char(40)        TYPE c,
        l_count           type i,
        lw_resb           LIKE resb,
        lt_resb           LIKE TABLE OF resb WITH HEADER LINE.


  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp AS a INNER JOIN cabn AS c
      ON a~atinn = c~atinn
   WHERE c~atnam = 'P_RP_STATUS'
     AND a~klart = '002'
** Changed by Furong on 05/22/06, Requested by Mr Hur
*     AND a~atwrt = wa_rp .
     AND a~atwrt BETWEEN wa_rp AND wa_rp_to.

  DESCRIBE TABLE it_ausp      LINES wa_lines.
  IF wa_lines = 0.
    CLEAR: wa_dis1.  EXIT.
  ENDIF.

  CONCATENATE 'P_RP' wa_rp '_SHOP_DATE'           INTO l_char.
  PERFORM read_atinn    USING 'P_WORK_ORDER'      l_worder   .
  PERFORM read_atinn    USING 'P_MODEL_YEAR'      l_modelyear.
  PERFORM read_atinn    USING 'P_MI'              l_mi       .
  PERFORM read_atinn    USING 'P_OCN'             l_ocn      .
  PERFORM read_atinn    USING 'P_VERSION'         l_version  .
  PERFORM read_atinn    USING 'P_SEQUENCE_DATE'   l_sdate    .
  PERFORM read_atinn    USING 'P_EXT_COLOR'       l_ecolor   .
  PERFORM read_atinn    USING 'P_INT_COLOR'       l_icolor   .
  PERFORM read_atinn    USING 'P_RP01_SHOP_DATE'  l_bdate    .
  PERFORM read_atinn    USING  l_char             l_rpdate   .
  PERFORM read_atinn    USING 'P_PLAN_ORDER'      l_porder   .
  PERFORM read_atinn    USING 'P_USAGE_CAR'       l_usage_car.

  " Save the Data to Internal Table (It_Screen)
  CLEAR: it_screen, it_screen[].    wa_dis1 = 'X'.
  LOOP AT it_ausp.
    CLEAR: it_screen, l_char.
    it_screen-objek     = it_ausp-objek .
    it_screen-status    = it_ausp-atwrt .
    PERFORM read_ausp_char   USING l_usage_car l_car.
    IF l_car = 'S' OR l_car = 'D'.
      CONTINUE.
    ENDIF.
** changed on 10/19/2006 by Furong to get planed order which only
** include old material.
    PERFORM read_ausp_char   USING l_porder    it_screen-plnum .
    SELECT SINGLE rsnum INTO l_rsnum
      FROM plaf
     WHERE plnum = it_screen-plnum  .
    IF sy-subrc = 0.
      SELECT *  INTO TABLE lt_resb
       FROM resb
      WHERE rsnum = l_rsnum
        AND matnr = wa_oldno.
      DESCRIBE TABLE lt_resb LINES l_count.
      IF l_count = 0.
        delete it_ausp.
        CONTINUE.
      ENDIF.
    ELSE.
      delete it_ausp.
      CONTINUE.
    ENDIF.
** end of change

    PERFORM read_ausp_char   USING l_worder    it_screen-worder.
    PERFORM read_ausp_char   USING l_ecolor    it_screen-ecolor.
    PERFORM read_ausp_char   USING l_icolor    it_screen-icolor.
    PERFORM read_ausp_num    USING l_sdate     it_screen-sdate .
    PERFORM read_ausp_num    USING l_bdate     it_screen-bodydate.
    PERFORM read_ausp_num    USING l_rpdate    it_screen-rpdate  .
    " Check the Full-Spec-code Value..
    PERFORM read_ausp_char   USING l_modelyear l_fsc           .
    PERFORM read_ausp_char   USING l_mi        l_char          .
    CONCATENATE l_fsc it_screen-worder+9(5) l_char INTO l_fsc  .
    PERFORM read_ausp_char   USING l_ocn       l_char          .
    CONCATENATE l_fsc l_char INTO l_fsc SEPARATED BY space    .
*    IF l_fsc NE p_matnr.     CONTINUE.         ENDIF.
    PERFORM read_ausp_char   USING l_version   l_char          .
*    IF p_verid NE l_char+1(2). CONTINUE.         ENDIF.
    APPEND it_screen.
  ENDLOOP.
ENDFORM.                    " READ_PLAN_ORDER

*&---------------------------------------------------------------------*
*&      Form  READ_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1897   text
*      -->P_L_WORDER  text
*----------------------------------------------------------------------*
FORM read_atinn USING    p_char  pa_atinn .
  SELECT SINGLE atinn INTO pa_atinn
    FROM cabn
   WHERE atnam = p_char .
ENDFORM.                    " READ_ATINN

*&---------------------------------------------------------------------*
*&      Form  READ_AUSP_CHAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_WORDER  text
*----------------------------------------------------------------------*
FORM read_ausp_char USING    pa_atinn  pa_atwrt.
  SELECT SINGLE atwrt INTO pa_atwrt
    FROM ausp
   WHERE objek = it_ausp-objek
     AND atinn = pa_atinn
     AND klart = '002'   .
ENDFORM.                    " READ_AUSP_CHAR

*&---------------------------------------------------------------------*
*&      Form  READ_AUSP_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_WORDER  text
*----------------------------------------------------------------------*
FORM read_ausp_num  USING    pa_atinn  pa_atflv.
  DATA: l_num(8)    TYPE     n,
        l_atflv     LIKE     ausp-atflv.

  SELECT SINGLE atflv INTO l_atflv
    FROM ausp
   WHERE objek = it_ausp-objek
     AND atinn = pa_atinn
     AND klart = '002'   .

  IF sy-subrc = 0.
    pa_atflv  = l_num = l_atflv.
  ELSE.
    CLEAR: pa_atflv.
  ENDIF.
ENDFORM.                    " READ_AUSP_NUM

*&---------------------------------------------------------------------*
*&      Form  CHECK_ECMNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ecmno.
  DATA: lw_aenr           LIKE aenr      ,
        l_matnr           LIKE mara-matnr.

  wa_ecm_result = 'FAIL'  .
  SELECT SINGLE *  INTO lw_aenr
    FROM aenr
   WHERE aennr = wa_ecm_no .

  CHECK sy-subrc = 0.
  SELECT SINGLE idnrk INTO l_matnr
    FROM stpo
   WHERE aennr = wa_ecm_no
     AND idnrk = wa_newno .

  CHECK sy-subrc = 0.
  wa_dis2 = 'X'.   wa_ecm_result = 'OK' .
ENDFORM.                    " CHECK_ECMNO

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_log_process     USING  l_qty .
  " Save the History of the Change....
  CLEAR: ztpp_plan_comp.
  ztpp_plan_comp-aennr      = wa_ecm_no .
  ztpp_plan_comp-matnr      = wa_newno  .
  ztpp_plan_comp-plnum      = it_screen-plnum.
  ztpp_plan_comp-fsc        = p_matnr   .
  ztpp_plan_comp-verid      = p_verid   .
  ztpp_plan_comp-rp         = wa_rp     .
  ztpp_plan_comp-oldmat     = wa_oldno  .
  ztpp_plan_comp-qty        = l_qty     .
  ztpp_plan_comp-equnr      = it_screen-objek.
  ztpp_plan_comp-worder     = it_screen-worder.
  ztpp_plan_comp-extc       = it_screen-ecolor.
  ztpp_plan_comp-intc       = it_screen-icolor.
  ztpp_plan_comp-ch_result  = it_screen-change.
  ztpp_plan_comp-ch_msg     = it_screen-msg   .
  ztpp_plan_comp-uname      = sy-uname  .
  ztpp_plan_comp-datum      = sy-datum  .
  ztpp_plan_comp-uzeit      = sy-uzeit  .
  INSERT INTO ztpp_plan_comp VALUES ztpp_plan_comp.
ENDFORM.                    " SAVE_LOG_PROCESS

*&---------------------------------------------------------------------*
*&      Form  read_history
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_history.
  RANGES: r_aennr       FOR ztpp_plan_comp-aennr,
          r_matnr       FOR ztpp_plan_comp-matnr.

  IF st5000-aennr NE space.
    CONCATENATE 'IEQ' st5000-aennr  INTO r_aennr.   APPEND r_aennr.
  ENDIF.

  IF st5000-matnr NE space.
    CONCATENATE 'IEQ' st5000-matnr  INTO r_matnr.   APPEND r_matnr.
  ENDIF.

  CLEAR: it_disp, it_disp[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_disp
    FROM ztpp_plan_comp
   WHERE aennr IN r_aennr
     AND matnr IN r_matnr .
ENDFORM.                    " read_history

*&---------------------------------------------------------------------*
*&      Form  SORT_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0203   text
*----------------------------------------------------------------------*
FORM sort_process USING    pa_stype .
  DATA: lw_screen          TYPE TABLE OF cxtab_column  WITH HEADER LINE,
        field_name01(40).
*
  CLEAR:  field_name01.
  LOOP AT tc_5000-cols    INTO lw_screen.
    IF lw_screen-selected = 'X' .
      field_name01 = lw_screen-screen-name .
      field_name01 = field_name01+8        .
      EXIT.
    ENDIF.
  ENDLOOP.

  CASE pa_stype.
    WHEN 'A'.
      SORT it_disp      ASCENDING  BY (field_name01).
    WHEN 'D'.
      SORT it_disp      DESCENDING BY (field_name01).
  ENDCASE.
ENDFORM.                    " SORT_PROCESS
