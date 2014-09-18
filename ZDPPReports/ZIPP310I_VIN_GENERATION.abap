************************************************************************
* Program Name      : ZIPP310I_VIN_GENERATION
* Author            : Bobby
* Creation Date     : 2003.09.04.
* Specifications By : Bobby
* Development Request No :
* Addl Documentation:
* Description       : Vehicle Order(Planned Order) Creation
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT  zipp102i_002  MESSAGE-ID zmpp  LINE-SIZE 700 .

TYPES: BEGIN OF zstruc                       ,
         rcode                LIKE sy-subrc  ,
         equnr                LIKE equi-equnr,
         model(3)             TYPE c   ,
         serial(6)            TYPE n   ,
         wo                   LIKE mara-matnr,
         col(4)               TYPE c   ,
         my                   TYPE n   ,
         mi(7)                TYPE c   ,
         ocn(4)               TYPE n   ,
         ver(3)               TYPE n   ,
         vin                  LIKE mara-matnr,
         stas(2)              TYPE n   ,
         seq_day              LIKE sy-datum  ,
         seq_ser(6)           TYPE n   ,
         seq_cod(2)           TYPE c   ,
         sorder               LIKE vbap-vbeln,
         porder               LIKE plaf-plnum,
         r01_a                LIKE sy-datum  ,
         r01_s                TYPE n   ,
         r01_sd               LIKE sy-datum  ,
         r02_a                LIKE sy-datum  ,
         r02_s                TYPE n   ,
         r02_sd               LIKE sy-datum  ,
         r03_a                LIKE sy-datum  ,
         r03_s                TYPE n   ,
         r03_sd               LIKE sy-datum  ,
         r04_a                LIKE sy-datum  ,
         r04_s                TYPE n   ,
         r04_sd               LIKE sy-datum  ,
         r05_a                LIKE sy-datum  ,
         r05_s                TYPE n   ,
         r05_sd               LIKE sy-datum  ,
         r06_a                LIKE sy-datum  ,
         r06_s                TYPE n   ,
         r06_sd               LIKE sy-datum  ,
         r07_a                LIKE sy-datum  ,
         r07_s                TYPE n   ,
         r07_sd               LIKE sy-datum  ,
         r08_a                LIKE sy-datum  ,
         r08_s                TYPE n   ,
         r08_sd               LIKE sy-datum  ,
         r09_a                LIKE sy-datum  ,
         r09_s                TYPE n   ,
         r09_sd               LIKE sy-datum  ,
         r10_a                LIKE sy-datum  ,
         r10_s                TYPE n   ,
         r10_sd               LIKE sy-datum  ,
         r11_a                LIKE sy-datum  ,
         r11_s                TYPE n   ,
         r11_sd               LIKE sy-datum  ,
         r12_a                LIKE sy-datum  ,
         r12_s                TYPE n   ,
         r12_sd               LIKE sy-datum  ,
         r13_a                LIKE sy-datum  ,
         r13_s                TYPE n   ,
         r13_sd               LIKE sy-datum  ,
         r14_a                LIKE sy-datum  ,
         r14_s                TYPE n   ,
         r14_sd               LIKE sy-datum  ,
         r15_a                LIKE sy-datum  ,
         r15_s                TYPE n   ,
         r15_sd               LIKE sy-datum  ,
         r16_a                LIKE sy-datum  ,
         r16_s                TYPE n   ,
         r16_sd               LIKE sy-datum  ,
         r17_a                LIKE sy-datum  ,
         r17_s                TYPE n   ,
         r17_sd               LIKE sy-datum  ,
         r18_a                LIKE sy-datum  ,
         r18_s                TYPE n   ,
         r18_sd               LIKE sy-datum  ,
         s219(219)            TYPE c   ,
       END OF zstruc                   .

DATA: BEGIN OF it_wo          OCCURS 0       ,
         mark ,
         wo                   LIKE mara-matnr,
      END OF it_wo                           ,

      BEGIN OF it_col         OCCURS 0       ,
         mark ,
         col(4)               TYPE c         ,
      END OF it_col                          ,

      BEGIN OF it_status      OCCURS 0       ,
         mark ,
         stas(2)              TYPE n         ,
      END OF it_status                       ,

      BEGIN OF it_mi          OCCURS 0       ,
         mark ,
         mi(8)                TYPE c         ,
         ocn(4)               TYPE c         ,
      END OF it_mi                           .

DATA: wA_RECORD               TYPE I         ,      " Total Count
      WA_EA                   TYPE I         ,      " Record count
      wa_219(219)             TYPE c         ,
      it_create               TYPE TABLE OF zstruc     WITH HEADER LINE.

FIELD-SYMBOLS: <field1>       TYPE ANY,
               <field2>       TYPE ANY,
               <field3>       TYPE ANY.

CONTROLS: tc_9000  TYPE  TABLEVIEW USING SCREEN 9000 .

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_times          TYPE i          .
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  clear: wa_record.
  PERFORM create_initial    .
  PERFORM test_creation_vin .



*&---------------------------------------------------------------------*
*&      Form  GET_EQFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_EQFNR  text
*----------------------------------------------------------------------*
FORM get_eqfnr USING    pa_eqfnr.
  pa_eqfnr = 'A' .
ENDFORM.                    " GET_EQFNR

*&---------------------------------------------------------------------*
*&      Form  GET_WORKCENTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_WORKCENTER  text
*----------------------------------------------------------------------*
FORM get_workcenter USING    pa_workcenter.
  pa_workcenter = '10000064' .
ENDFORM.                    " GET_WORKCENTER

*&---------------------------------------------------------------------*
*&      Form  GENERATE_CHARACTERISITC_VM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_characterisitc_vm.
  DATA: l_vartable        LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_conf            LIKE TABLE OF conf_out       WITH HEADER LINE,
        l_data            LIKE TABLE OF BAPI1003_ALLOC_VALUES_CHAR
                                                       WITH HEADER LINE,
        l_return          LIKE TABLE OF bapiret2       WITH HEADER LINE,
        l_name(30)        TYPE c           ,
        l_name1(30)       TYPE c           ,
        l_name2(30)       TYPE c           ,
        l_name3(30)       TYPE c           ,
        l_no(03)          TYPE n           ,
        l_res(02)         TYPE n           ,
        l_cnt             TYPE i           ,
        l_pos             TYPE i           ,
        l_instance        LIKE inob-cuobj  ,
        l_workcenter      LIKE crhd-arbpl  ,
        l_workorder       LIKE mara-matnr  ,
        l_eqfnr           LIKE itob-eqfnr  ,
        l_mode            LIKE ztpp_common_vals-key2,
        l_equnr           LIKE equi-equnr  .

  DATA: L_OBJECT          LIKE BAPI1003_KEY-OBJECT      ,
        L_TABLE           LIKE BAPI1003_KEY-OBJECTTABLE ,
        L_CLASSNUM        LIKE BAPI1003_KEY-CLASSNUM    ,
        L_CLASSTYPE       LIKE BAPI1003_KEY-CLASSTYPE   ,
        l_dat2            like table of BAPI1003_ALLOC_VALUES_NUM
                                                       WITH HEADER LINE,
        l_dat1            like table of BAPI1003_ALLOC_VALUES_CURR
                                                       WITH HEADER LINE.

   clear: wa_ea .
  l_vartable-atnam = 'P_SALES_ORDER' .
  l_vartable-atwrt =  it_create-sorder.        APPEND l_vartable.
  l_vartable-atnam = 'P_PLAN_ORDER'  .
  l_vartable-atwrt =  it_create-porder.        APPEND l_vartable.
  l_vartable-atnam = 'P_MODEL'       .
  l_vartable-atwrt =  it_create-model.         APPEND l_vartable.
  l_vartable-atnam = 'P_BODY_SERIAL' .
  l_vartable-atwrt =  it_create-serial.        APPEND l_vartable.
  l_vartable-atnam = 'P_WORK_ORDER'  .
  l_vartable-atwrt =  it_create-wo   .         APPEND l_vartable.
  l_vartable-atnam = 'P_EXT_COLOR'   .
  l_vartable-atwrt =  it_create-col(2).        APPEND l_vartable.
  l_vartable-atnam = 'P_INT_COLOR'.
  l_vartable-atwrt =  it_create-col+2(2).      APPEND l_vartable.
  l_vartable-atnam = 'P_MODEL_YEAR'     .
  l_vartable-atwrt =  it_create-my      .      APPEND l_vartable.
  l_vartable-atnam = 'P_MI'             .
  l_vartable-atwrt =  it_create-mi      .      APPEND l_vartable.
  l_vartable-atnam = 'P_OCN'.
  l_vartable-atwrt =  it_create-ocn  .         APPEND l_vartable.
  l_vartable-atnam = 'P_VERSION'.
  l_vartable-atwrt =  it_create-ver  .         APPEND l_vartable.
  l_vartable-atnam = 'P_DESTINATION_CODE'.
  l_vartable-atwrt = 'B28AA'         .         APPEND l_vartable.
  l_vartable-atnam = 'P_VIN'  .
  l_vartable-atwrt =  it_create-vin  .         APPEND l_vartable.
  l_vartable-atnam = 'P_STATUS'   .
  l_vartable-atwrt =  it_create-stas .         APPEND l_vartable.
  l_vartable-atnam = 'P_SEQUENCE_DATE'  .
  l_vartable-atwrt =  it_create-seq_day .      APPEND l_vartable.
  l_vartable-atnam = 'P_SEQUENCE_SERIAL'.
  l_vartable-atwrt =  it_create-seq_ser+1(5).  APPEND l_vartable.
  l_vartable-atnam = 'P_SEQUENCE_CODE'  .
  l_vartable-atwrt =  it_create-seq_cod .      APPEND l_vartable.

  l_cnt = it_create-stas      .                CLEAR: l_res .

*  DO l_cnt TIMES.
*    l_res = l_res + 1 .
*    CONCATENATE 'IT_CREATE-R'  l_res        '_A'   INTO  l_name1.
*    CONCATENATE 'P_RP' l_res     '_ACTUAL_DATE'    INTO  l_name.
*    l_vartable-atnam = l_name .
*    ASSIGN  (l_name1)       TO   <field1>   .
*    l_vartable-atwrt = <field1>              .     APPEND l_vartable .
*    CONCATENATE 'IT_CREATE-R'  l_res        '_S'   INTO  l_name2.
*    CONCATENATE 'P_RP' l_res     '_SERIAL'         INTO  l_name.
*    l_vartable-atnam = l_name .
*    ASSIGN  (l_name2)       TO   <field2>   .
*    l_vartable-atwrt = <field2>              .     APPEND l_vartable .
*    CONCATENATE 'IT_CREATE-R'  l_res        '_SD'  INTO  l_name3.
*    CONCATENATE 'P_RP' l_res     '_SHOP_DATE'      INTO  l_name.
*    l_vartable-atnam = l_name .
*    ASSIGN  (l_name3)       TO   <field3>   .
*    l_vartable-atwrt = <field3>              .     APPEND l_vartable .
*  ENDDO.

  l_no = 0 .

  DO  9 TIMES.
    l_pos = l_no .
    l_no = l_no + 1.
   CONCATENATE 'P_219_' l_no+2(1)   INTO l_name .  CLEAR: l_conf-atwrt.
*    READ TABLE l_conf    WITH KEY    atnam = l_name.
    l_vartable-atwrt = it_create-s219+l_pos(1)  .
    l_vartable-atnam = l_name.  "     l_vartable-atwrt = l_conf-atwrt .
    APPEND l_vartable        .
  ENDDO.

  DO 90 TIMES.
    l_pos = l_no .
    l_no = l_no + 1.
   CONCATENATE 'P_219_' l_no+1(2)   INTO l_name .  CLEAR: l_conf-atwrt.
*    READ TABLE l_conf    WITH KEY    atnam = l_name.
    l_vartable-atwrt = it_create-s219+l_pos(1)  .
    l_vartable-atnam = l_name.    "   l_vartable-atwrt = l_conf-atwrt .
    APPEND l_vartable        .
  ENDDO.

  DO 120 TIMES.
    l_pos = l_no .
    l_no = l_no + 1.
   CONCATENATE 'P_219_' l_no        INTO l_name .  CLEAR: l_conf-atwrt.
*    READ TABLE l_conf    WITH KEY    atnam = l_name.
    l_vartable-atwrt = it_create-s219+l_pos(1)  .
    l_vartable-atnam = l_name.    "   l_vartable-atwrt = l_conf-atwrt .
    APPEND l_vartable        .
  ENDDO.

  describe table l_vartable lines wa_ea.
  wa_record = wa_record + wa_ea        .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object     = it_create-equnr
            mode       = 'W'
       TABLES
            val_table  = l_vartable
       EXCEPTIONS
            no_data    = 1
            error_mode = 2
            OTHERS     = 3.

  CLEAR: l_data, l_data[].
  LOOP AT l_vartable .
    l_data-charact      =  l_vartable-atnam .
    l_data-value_char   = l_vartable-atwrt .
    WRITE: / IT_CREATE-EQUNR, L_data-charact, l_data-value_char, '**' .
    APPEND l_data .
  ENDLOOP.

*  L_TABLE = 'EQUI'.                L_CLASSNUM = 'P_VEHICLE_MASTER'.
*  L_OBJECT = it_create-equnr .     L_CLASSTYPE = '002'.

*  CALL FUNCTION 'BAPI_OBJCL_CHANGE'
*    EXPORTING
*      objectkey             =  L_OBJECT
*      objecttable           =  L_TABLE
*      classnum              =  L_CLASSNUM
*      classtype             =  L_CLASSTYPE
**     STATUS                = '1'
**     STANDARDCLASS         =
**     CHANGENUMBER          =
**     KEYDATE               = SY-DATUM
*    TABLES
*      ALLOCVALUESNUMNEW     = l_dat2
*      allocvaluescharnew    = l_data
*      ALLOCVALUESCURRnew    = l_dat1
*      return                = l_return .

  LOOP AT l_return  .
    WRITE: / l_return-type, l_RETurn-message.
  ENDLOOP.

  it_create-rcode = sy-subrc .
ENDFORM.                    " GENERATE_CHARACTERISITC_VM

*&---------------------------------------------------------------------*
*&      Form  TEST_CREATION_VIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM test_creation_vin.
  DATA: l_219(219)             TYPE c          ,
        l_219a(219)            TYPE c          ,
        l_219b(219)            TYPE c          ,
        l_219c(219)            TYPE c          ,
        l_219d(219)            TYPE c          ,
        l_text(100)            TYPE c          ,
        l_no(5)                TYPE n          ,
        l_data                 TYPE zstruc     .

  WRITE AT: /001(90) ' Start of the Creation(VIN Master) ' ,
            /020(10)   sy-uzeit ,
             035(10)   sy-datum .

  DO p_times TIMES.
    PERFORM change_value              USING l_data            .
    PERFORM test_data_vin_vm_creation USING l_data            .
  ENDDO.

  IF sy-batch = ' '.
*    MESSAGE i001(zmpp) WITH 'End of Header Createion..'.
  ELSE.
    WRITE AT: /001(060) 'End of Header Createion..'.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .

  PERFORM assign_class
                                                            .
  IF sy-batch = ' '.
*    MESSAGE i001(zmpp) WITH 'End of Class Assignment..'.
  ELSE.
    WRITE AT: /001(060) 'End of Class Assignment..'.
  ENDIF.

  COMMIT WORK AND WAIT.

   l_219a = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ112345678902345678909870' .
   l_219b = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ112345678902345678909870' .
   l_219c = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ112345678902345678909870' .
   l_219d = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ112345678902345678909870' .
   CONCATENATE l_219a l_219b l_219c l_219d 'QWERTYUIOPDKSLFIEKD'
          INTO l_219  .

  CLEAR: l_no .
  LOOP AT it_create .
    it_create-s219 = l_219.
    l_no =  l_no + 1 .
    IF sy-batch = ' ' .
      CONCATENATE 'Data Processing: ' l_no ' Records!!!' INTO l_text .
      MESSAGE s001(zmpp) WITH l_text .
    ELSE.
      WRITE AT: /001(018) 'Data Processing: ' ,
                 019(005)  l_no               ,
                 025(080) 'Records!!!(Assign the Caracteristics Value)'.
    ENDIF.

    PERFORM generate_characterisitc_vm .
    SHIFT l_219  CIRCULAR .
    MODIFY it_create      .
  ENDLOOP.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .

  GET TIME.
  WRITE AT: /001(90) ' End of the Creation(VIN Master) ' ,
            /020(10)   sy-uzeit ,
             035(10)   sy-datum ,
            /005(20) ' Total Record(Created) ' ,
             025(10)   wa_record               .

  IF sy-batch = ' ' .
    CALL SCREEN 9000 .
  ENDIF.
ENDFORM.                    " TEST_CREATION_VIN

*&---------------------------------------------------------------------*
*&      Form  TEST_DATA_VIN_VM_CREATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0588   text
*      -->P_0589   text
*----------------------------------------------------------------------*
FORM test_data_vin_vm_creation USING   pa_data TYPE zstruc .
  DATA: l_res                TYPE i           ,
        l_workcenter         LIKE crhd-objid  ,
        l_workorder          LIKE mara-matnr  ,
        l_vin                LIKE mara-matnr  ,
        l_eqfnr              LIKE itob-eqfnr  ,
        l_equnr              LIKE equi-equnr  ,
        l_general            LIKE  bapi_itob  ,
        l_specific           LIKE  bapi_itob_eq_only,
        l_return             LIKE  bapiret2   ,
        l_equi               LIKE  v_equi     ,
        l_text(100)          TYPE c        ,
        l_name1(30)          TYPE c        ,
        l_name2(30)          TYPE c        ,
        l_name3(30)          TYPE c        ,
        l_pos(2)             TYPE n        ,
        l_no(05)             TYPE n        ,
        l_cnt                TYPE i        .

  l_vin  = 'KMHWF35H-3A'.

  CALL FUNCTION 'Z_FPP_VIN_GENERATION'
    EXPORTING
      w_order         = l_vin
      MODE            = 'EMF'
    IMPORTING
      P_LASTID        = pa_data-vin
    EXCEPTIONS
      NOT_FOUND       = 1
      OTHERS          = 2 .

  IF SY-SUBRC NE 0.
  ENDIF.

  CONCATENATE 'V310'  pa_data-vin+11(6)   INTO    pa_data-sorder .
  pa_data-seq_ser = pa_data-porder  =   pa_data-vin+11(6) .

  CLEAR: l_no.   l_no = sy-tabix .
  IF sy-batch = ' ' .
    CONCATENATE 'Data Processing: ' l_no ' Records!!!'
                ' Serial Value is ' pa_data-vin+11(6) INTO l_text .
    MESSAGE s001(zmpp) WITH l_text .
  ELSE.
*    WRITE AT: /001(018) 'Data Processing: ' ,
*               019(003)  l_no               ,
*               022(027) 'Records!!! Serial Value is ' ,
*               049(006)  pa_data-vin+11(6)  .

    WRITE AT: /001(40) 'Create the Vehicle Master with..' ,
               041(15) 'Sales Order is ' ,
               056(15)  pa_data-sorder   ,
               060(15) 'Plan Order is  ' ,
               075(10)  pa_data-porder   ,   " pa_plnum        ,
               086(17) 'VIN Spec COde is',
               105(20)  pa_data-vin      .
  ENDIF.

  " Create the Vehicle Master
  l_workorder = pa_data-wo    .
  pa_data-serial = pa_data-vin+11(6) .

*  l_cnt = pa_data-stas      .               CLEAR: l_pos .
*  l_res = 18 - pa_data-stas .
*  DO l_cnt TIMES.
*    l_pos = l_pos + 1 .
*    CONCATENATE 'PA_DATA-R'  l_pos          '_A'   INTO  l_name1.
*    ASSIGN  (l_name1)       TO   <field1>   .
*    <field1> = sy-datum - l_cnt + l_pos     .
*    CONCATENATE 'PA_DATA-R'  l_pos          '_S'   INTO  l_name2.
*    ASSIGN  (l_name2)       TO   <field2>   .
*    <field2> = pa_data-serial               .
*    CONCATENATE 'PA_DATA-R'  l_pos          '_SD'  INTO  l_name3.
*    ASSIGN  (l_name3)       TO   <field3>   .
*    <field3> = sy-datum - l_cnt + l_pos     .
*  ENDDO.

  CONCATENATE 'EMF'           pa_data-serial       INTO pa_data-equnr.

  PERFORM get_workcenter      USING l_workcenter   .
  PERFORM get_eqfnr           USING l_eqfnr        .

  l_general-objecttype       = '1000' .
  l_general-manfacture       = 'HMMA' .
  l_general-mancountry       = 'US'   .
  l_general-countr_iso       = 'US'   .
  l_general-manserno         =  pa_data-equnr           .
  l_general-manmodel         = 'EMF'  .
  l_general-constyear        =  sy-datum(4)             .
  l_general-constmonth       =  sy-datum+4(2)           .
  l_general-planplant        = 'P001'                   .
  l_general-manparno         =  pa_data-vin             .
  l_general-descript         =  pa_data-equnr           .
  l_general-sortfield        =  pa_data-serial          .
  l_general-maintplant       = 'P001'                   .
  l_general-pp_wkctr         =  l_workcenter            .
* L_GENERAL-COMP_CODE        = 'H201' .
  l_general-read_crdat       = sy-datum.
  l_general-read_crnam       = sy-uname.

  l_specific-equicatgry      = 'V' .

  CALL FUNCTION 'BAPI_EQUI_CREATE'
       EXPORTING
            external_number = pa_data-equnr
            data_general    = l_general
            data_specific   = l_specific
            valid_date      = sy-datum
       IMPORTING
            return          = l_return.

* WAIT UP TO 1 SECONDS.

  l_equi-equnr = pa_data-equnr .
*

  MOVE-CORRESPONDING pa_data TO it_create   .
  it_create-equnr         = pa_data-equnr   .    APPEND it_create .
ENDFORM.                    " TEST_DATA_VIN_VM_CREATION

*&---------------------------------------------------------------------*
*&      Form  CHANGE_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_SERIAL  text
*      -->P_L_DATA  text
*----------------------------------------------------------------------*
FORM change_value USING      pa_data  TYPE zstruc .
  DATA: idx_n                LIKE sy-tabix,
        l_index              LIKE sy-tabix.

  READ TABLE it_wo WITH KEY mark = 'X' .
  idx_n = l_index = sy-tabix .      pa_data-wo  =  it_wo-wo .
  it_wo-mark = ' ' .  idx_n = idx_n + 1.
  IF idx_n   = 3   .  idx_n = 1.                   ENDIF.
  MODIFY it_wo INDEX l_index TRANSPORTING mark .
  it_wo-mark = 'X' .
  MODIFY it_wo INDEX idx_n   TRANSPORTING mark .

  READ TABLE it_col WITH KEY mark = 'X' .
  idx_n = l_index = sy-tabix .      pa_data-col =  it_col-col .
  it_col-mark = ' ' .  idx_n = idx_n + 1.
  IF idx_n   = 5   .  idx_n = 1.                   ENDIF.
  MODIFY it_col INDEX l_index TRANSPORTING mark .
  it_col-mark = 'X' .
  MODIFY it_col INDEX idx_n   TRANSPORTING mark .

  READ TABLE it_status WITH KEY mark = 'X' .
  idx_n = l_index = sy-tabix .      pa_data-stas  =  it_status-stas .
  it_status-mark = ' ' .  idx_n = idx_n + 1.
  IF idx_n   = 19 .   idx_n = 1.                   ENDIF.
  MODIFY it_status INDEX l_index TRANSPORTING mark .
  it_status-mark = 'X' .
  MODIFY it_status INDEX idx_n   TRANSPORTING mark .

  READ TABLE it_mi WITH KEY mark = 'X' .
  idx_n = l_index = sy-tabix .            pa_data-mi  = it_mi-mi.
  it_mi-mark = ' ' .  idx_n = idx_n + 1.  pa_data-ocn = it_mi-ocn.
  IF idx_n   = 3   .  idx_n = 1.                   ENDIF.
  MODIFY it_mi INDEX l_index TRANSPORTING mark .
  it_mi-mark = 'X' .
  MODIFY it_mi INDEX idx_n   TRANSPORTING mark .

  pa_data-model    = 'EMF'   .
  pa_data-my       =  4      .
  pa_data-ver      =  13     .
  pa_data-seq_cod  = 'AA'    .
  pa_data-seq_day  = sy-datum.
ENDFORM.                    " CHANGE_VALUE

*&---------------------------------------------------------------------*
*&      Form  CREATE_INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_initial.
  it_wo-wo       = 'E0309A001' .                       APPEND it_wo .
  it_wo-wo       = 'E0310A001' .                       APPEND it_wo .
  it_col-col     = 'MOTI'      .                       APPEND it_col.
  it_col-col     = 'MOLK'      .                       APPEND it_col.
  it_col-col     = 'EBLK'      .                       APPEND it_col.
  it_col-col     = 'NWLK'      .                       APPEND it_col.
  it_mi-mi       = 'EMFDT6B '  . it_mi-ocn = '1200' .  APPEND it_mi .
  it_mi-mi       = 'EMFGH6B '  . it_mi-ocn = '0923' .  APPEND it_mi .
  it_status-stas = 18          .                       APPEND it_status.
  it_status-stas = 17          .                       APPEND it_status.
  it_status-stas = 16          .                       APPEND it_status.
  it_status-stas = 15          .                       APPEND it_status.
  it_status-stas = 14          .                       APPEND it_status.
  it_status-stas = 13          .                       APPEND it_status.
  it_status-stas = 12          .                       APPEND it_status.
  it_status-stas = 11          .                       APPEND it_status.
  it_status-stas = 10          .                       APPEND it_status.
  it_status-stas = 09          .                       APPEND it_status.
  it_status-stas = 08          .                       APPEND it_status.
  it_status-stas = 07          .                       APPEND it_status.
  it_status-stas = 06          .                       APPEND it_status.
  it_status-stas = 05          .                       APPEND it_status.
  it_status-stas = 04          .                       APPEND it_status.
  it_status-stas = 03          .                       APPEND it_status.
  it_status-stas = 02          .                       APPEND it_status.
  it_status-stas = 01          .                       APPEND it_status.
  it_status-mark = it_wo-mark = it_col-mark = it_mi-mark = 'X'    .
  MODIFY it_wo     INDEX 1  TRANSPORTING mark       .
  MODIFY it_mi     INDEX 1  TRANSPORTING mark       .
  MODIFY it_status INDEX 1  TRANSPORTING mark       .
  MODIFY it_col    INDEX 1  TRANSPORTING mark       .
ENDFORM.                    " CREATE_INITIAL

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'RES_STAT'.
  SET TITLEBAR 'RES'.
ENDMODULE.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE TO SCREEN 0 .
ENDMODULE.                 " exit  INPUT

*&---------------------------------------------------------------------*
*&      Form  assign_class
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_class.
  DATA:  l_text(100)          TYPE  c          ,
         l_no(5)              TYPE  n          ,
         l_equi               LIKE  v_equi     .

  LOOP AT it_create.
    CLEAR: l_no.   l_no = sy-tabix .
    IF sy-batch = ' ' .
      CONCATENATE 'Data Processing: ' l_no ' Records!!!' INTO l_text .
      MESSAGE s001(zmpp) WITH l_text .
    ELSE.
      WRITE AT: /001(018) 'Data Processing: ' ,
                 019(005)  l_no               ,
                 025(050) 'Records!!!(ASSIGN THE CLASS)' .
    ENDIF.

    l_equi-equnr = it_create-equnr.
    CALL FUNCTION 'EQUIPMENT_CLASS_ALLOCATE'
         EXPORTING
              eq_class      = 'P_VEHICLE_MASTER'
              eq_class_type = '002'
*            IS_STANDARD    = 'X'
              init_new      = 'X'
              lock_new      = 'X'
              update_new    = 'X'
              commit_new    = 'X'
         CHANGING
              s_equi        = l_equi.
  ENDLOOP.
ENDFORM.                    " assign_class
