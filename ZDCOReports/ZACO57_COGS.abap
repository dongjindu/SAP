************************************************************************
* Program Name      : ZACO57_COGS_NEW
* Author            : Chris Li
* Specification     : Andy Choi
* Creation Date     : 06/07/2005
* Addl Documentation:
* Description       : Allocate discordance between FI&CO COGS
*                     and post the difference by bapi
* Modification Logs
* Date       Developer    RequestNo    Description
*01/11/05    Manjunath    UD1K918886   Program bug fixing (Ref:
*                                      Help desk 61AG216A82)
*02/09/06    Manjunath    UD1K919300   Program changes (if Processing
*                                      of account is 5 then Discard
*                                      FI record and Copy CO amt as FI
*                                      amount and change sign. Don't
*                                      consider cancelled document.
************************************************************************
*Notice:
* - Maintain ztco_gl_pa
************************************************************************
report zaco57_cogs    message-id zmco.

include: <icon>.
type-pools: vrm,rkea1.     "//Value Request Manager: Types & Constants
tables: zsco_cogs_new,
        ztco_cogs,
        ztco_model_map,
        ztco_gl_pa,
        ce1h201, ce4h201,
        mara,
        mvke,
        ekpo,
        a004,
        a005,
        bkpf,
        kna1,
        t001,
        t001w,
        bsis,
        ska1,cskb,
        rbkp.

*----- Internal tables
data: it_gl_pa    like ztco_gl_pa occurs 0 with header line.

*PA sales
data: begin of i_sales occurs 1,
        artnr  like ce1h201-artnr,
        erlos  like ce1h201-erlos,
      end   of i_sales.

ranges: r_hkont  for bsis-hkont,
        r_hkont2 for bsis-hkont,
        r_vrgar  for ce1h201-vrgar.


data: begin of it_value_field occurs 0,
        field       like   dd03l-fieldname,
        value_field like   dd03l-fieldname,
        fieldname   like   dd03l-fieldname,
        ddtext      like    dd04t-ddtext,
      end   of it_value_field.

data: begin of i_chars occurs 0,
        field   like   ztco_gl_pa-fieldname,
      end   of i_chars.

data: begin of it_cogs_schima occurs 0,
        field   like   dd03l-fieldname,
      end   of it_cogs_schima.

data: begin of it_cogs occurs 0.
        include structure zsco_cogs_new.
*       post_line  type char1    ,    "Post line
*       vrgar      type rke_vrgar,
*       fieldname  type fieldname,    "Field name
*       ddtext     type as4text ,     "Short text
*       kmland     type land1_gp,     "Country key
*       kndnr      type kunnr   ,     "Customer number
*       artnr      type artnr   ,     "Material number
*       model      type rkeg_paph2,   "Model Code
*       hkont      type hkont   ,     "General ledger account
*       fiamt      type zfiamt  ,     "G/L Amount
*       coamt      type zcoamt  ,     "CO Amount
*       diff       type zdiff   ,     "Diffrence Amount
*       post       type zpost   ,     "Posting amount
*       belnr      type rke_belnr,    "Document number
*       rbeln      type rkerfbelnr,   "Reference document
*       vftyp      type zvftyp   ,    "Value Field Type(char1)
*       flag       type zflag01  ,    "COGS Flag
*       icon       type icon_d   ,    "Icon
*       msg        type  zmsg100 ,    "Message
data:   end of it_cogs.
data: it_cogs_tmp like it_cogs occurs 0 with header line.
data: it_cogs_tar like it_cogs occurs 0 with header line.

data: begin of it_fi_cogs occurs 0,
        kunnr	type kunnr   ,
        kmland  type land1_gp,
        model	type zmodl   ,
        artnr	type matnr   ,
        matnr	type matnr   ,
        maktx	type maktx   ,
        hkont	type hkont   ,
        fname   type fieldname,
        vrgar   type rke_vrgar, "rec.type.
        paobjnr type rkeobjnr ,
        pasubnr type rkesubnr ,
        txt20	type txt20   ,
        bklas	type bklas   ,
        vftyp	type zvftyp  ,
        vkorg	type vkorg   ,
        vtweg	type vtweg   ,
        mvgr3	type mvgr3   ,
        mvgr4	type mvgr3   ,
        mvgr5	type mvgr5   ,
        mtart	type mtart   ,
        spart	type spart   ,
        prdha	type prodh_d ,
        dmbtr	type dmbtr   ,
      end of it_fi_cogs.  "like zsco_cogs_fi occurs 0 with header line.

*----- If value field count is 50 over,
*----- increase structure ZSCO_COGS_CO's fields
data: begin of it_co_cogs occurs 0,
        paobjnr type rkeobjnr,
        pasubnr type rkesubnr,

        kndnr   type kunnr     , "Customer number
        kmland  type land1_gp  , "Country key

        artnr   type artnr     , "Material number
        paph1   type rkeg_paph1, "type
        paph2   type rkeg_paph2, "Vehicle Model
        prodh   type prodh_d   , "Product hierarchy

        vkorg   type vkorg     , "Sales organization
        vtweg   type vtweg     , "Distribution channel

        vrgar   type rke_vrgar , "rec.type.

        fd001   type zfd001    , "Value Field Amount
        fd002   type zfd001    , "Value Field Amount
        fd003   type zfd001    , "Value Field Amount
        fd004   type zfd001    , "Value Field Amount
        fd005   type zfd001    , "Value Field Amount
        fd006   type zfd001    , "Value Field Amount
        fd007   type zfd001    , "Value Field Amount
        fd008   type zfd001    , "Value Field Amount
        fd009   type zfd001    , "Value Field Amount
        fd010   type zfd001    , "Value Field Amount
        fd011   type zfd001    , "Value Field Amount
        fd012   type zfd001    , "Value Field Amount
        fd013   type zfd001    , "Value Field Amount
        fd014   type zfd001    , "Value Field Amount
        fd015   type zfd001    , "Value Field Amount
        fd016   type zfd001    , "Value Field Amount
        fd017   type zfd001    , "Value Field Amount
        fd018   type zfd001    , "Value Field Amount
        fd019   type zfd001    , "Value Field Amount
        fd020   type zfd001    , "Value Field Amount
        fd021   type zfd001    , "Value Field Amount
        fd022   type zfd001    , "Value Field Amount
        fd023   type zfd001    , "Value Field Amount
        fd024   type zfd001    , "Value Field Amount
        fd025   type zfd001    , "Value Field Amount
        fd026   type zfd001    , "Value Field Amount
        fd027   type zfd001    , "Value Field Amount
        fd028   type zfd001    , "Value Field Amount
        fd029   type zfd001    , "Value Field Amount
        fd030   type zfd001    , "Value Field Amount
        fd031   type zfd001    , "Value Field Amount
        fd032   type zfd001    , "Value Field Amount
        fd033   type zfd001    , "Value Field Amount
        fd034   type zfd001    , "Value Field Amount
        fd035   type zfd001    , "Value Field Amount
        fd036   type zfd001    , "Value Field Amount
        fd037   type zfd001    , "Value Field Amount
        fd038   type zfd001    , "Value Field Amount
        fd039   type zfd001    , "Value Field Amount
        fd040   type zfd001    , "Value Field Amount
        vftyp   type zvftyp    , "Value Field Type
      end of it_co_cogs. "LIKE zsco_cogs_co OCCURS 0 WITH HEADER LINE.

data: it_inputdata like bapi_copa_data  occurs 0 with header line,
      it_fieldlist like bapi_copa_field occurs 0 with header line,
      it_return    like bapiret2        occurs 0 with header line.

*----- Global variables & Structures


data: begin of it_bsis occurs 0,
          hkont  like bsis-hkont,
          zuonr  like bsis-zuonr,
          gjahr  like bsis-gjahr,
          belnr  like bsis-belnr,
          buzei  like bsis-buzei,
*          monat  LIKE bsis-monat,
          shkzg  like bsis-shkzg,
          dmbtr  like bsis-dmbtr,
*          budat  LIKE bsis-budat,
          stblg  like bkpf-stblg,
          awtyp  like bkpf-awtyp,
          awkey  like bkpf-awkey,
      end of it_bsis.
data: w_kokrs     like   tka01-kokrs,
      w_perbl     like   ce3h201-perbl,    "Period
      w_cnt       type   i,                "Field count
      w_fisum_new like   zsco_cogs-fiamt,  "For calclulation
      w_fisum_old like   zsco_cogs-fiamt,  "For calclulation
      w_cosum     like   zsco_cogs-fiamt,  "For calclulation
      w_diff      like   zsco_cogs-fiamt,  "For calclulation
      w_op_concern like  bapi0017-op_concern,
      w_top_line  type   i,                "ALV top line
      w_record    like   bapi_copa_data-record_id,   "Record ID
      w_matnr_f   like   mara-matnr,       "Material
      w_matnr_t   like   mara-matnr,       "Material
      w_budat_f   like   sy-datum,         "Posting date
      w_budat_t   like   sy-datum,         "Posting date
      w_hkont_f   like   bsis-hkont,       "GL Account
      w_hkont_t   like   bsis-hkont,       "GL Account
      w_select    type   i,                "Selected items
      w_success   type   i,                "Success items
      w_fail      type   i,                "Failed items
      w_msg(50).                           "Message
data: wa_error,
      wa_message(80).
*----- Constants
constants: c_bukrs   like   t001-bukrs   value   'H201',
           c_werks   like   t001w-werks  value   'P001',
           c_posnr   like   afpo-posnr   value   '0001',
           c_mark                        value   'X',
           c_vftyp_1 like   ztco_gl_pa  value   '1', "Not assign
           c_vftyp_2 like   ztco_gl_pa  value   '2', "PURCHASE
           c_vftyp_3 like   ztco_gl_pa  value   '3', "NO ITEM
           c_error   type   i            value   1,
           c_bdcerr  type   i            value   2,
           c_ready   type   i            value   3,
           c_postable type  c            value   'Y',
           c_finish  type   i            value   4,
           " Cost of Sales & Variance
           c_hkont_f like   bsis-hkont   value   '0000510000',
           c_hkont_t like   bsis-hkont   value   '0000531999'.

*/// ALV Control....: Start
* Control Framework Basic Class
class cl_gui_cfw      definition load.

*// Declare reference variables, the container and internal table
data: wa_custom_control type        scrfname value 'ALV_CONTAINER',
      alv_grid          type ref to cl_gui_alv_grid,
      grid_container    type ref to cl_gui_custom_container.


* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
class lcl_event_receiver definition deferred. "/ALV Event Handling

data : event_receiver type ref to lcl_event_receiver.

* Global variables for attributes or etc of ALV GRID
data : it_fieldcat     type lvc_t_fcat with header line,
       it_fieldcat_fi  type lvc_t_fcat with header line,
       it_fieldcat_co  type lvc_t_fcat with header line,
       it_fieldname    type slis_t_fieldcat_alv,
       it_sort         type lvc_t_sort with header line,
       it_fieldcat_det type lvc_t_fcat with header line. "/Detail

data : wa_is_layout type lvc_s_layo, "/The Layout Structure
       w_fieldname    like line of it_fieldcat.

data: wa_save    type c   value 'A',   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

      wa_variant type disvariant.      "for parameter IS_VARIANT

*/// ALV Control....: End

*-- User Confirm for pop-up Message

data : w_repid like sy-repid,
       w_dynnr like sy-dynnr.


****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
class lcl_event_receiver definition.
  public section.
    methods:

    handle_double_click
        for event double_click of cl_gui_alv_grid
            importing e_row
                      e_column
                      es_row_no,

    handle_user_command
        for event user_command of cl_gui_alv_grid
            importing e_ucomm.
endclass.
* lcl_event_receiver (Definition)

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
class lcl_event_receiver implementation.
  method handle_double_click.
    perform dbl_click_9000 using e_column-fieldname
                                 es_row_no-row_id.

  endmethod.                           "handle_double_click

  method handle_user_command.
  endmethod.
endclass.
* lcl_event_receiver (Implementation)


*====================================================================
*----- Selection screens
selection-screen begin of block bl1 with frame title text-t01.
parameters: p_bukrs  like t001-bukrs  default c_bukrs obligatory,
            p_month  like s001-spmon default '200507'.
*                         sy-datum(6) obligatory.

selection-screen skip.
select-options: s_hkont  for  bsis-hkont,
                s_model  for  ztco_model_map-modl1 default 'EM',
                s_artnr  for  ce4h201-artnr.


*PARAMETERS: p_vrgar   LIKE ce1h201-vrgar DEFAULT 'Y' OBLIGATORY.
*PARAMETERS: p_test AS CHECKBOX DEFAULT 'X' .
selection-screen end   of block bl1.

*====================================================================

ranges: r_racct for glt0-racct.

*----- Default Value
initialization.
  perform set_initial_value.
  refresh r_racct.
  r_racct-option = 'BT'.
  r_racct-sign   = 'I'.
  r_racct-low    = '0000530100'.
  r_racct-high   = '0000530919'.
  append r_racct.

*----- Check Input value
at selection-screen.
  check sy-ucomm eq 'ONLI'.
  perform check_rtn.

*&---------------------------------------------------------------------*
*& start-of-selection.
*&---------------------------------------------------------------------*
start-of-selection.
  perform read_data.

  if wa_error = 'X'.
    perform write_message.
  else.
    perform calculate_it_cogs.
    perform display_it_cogs.
  endif.

*&---------------------------------------------------------------------*
*&      Form  set_initial_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_initial_value.
*----- Set Controlling area
  call function 'K_KOKRS_SET'
       importing
            e_kokrs   = w_kokrs
       exceptions
            not_found = 1
            others    = 2.
  if sy-subrc <> 0.
    if sy-msgty = 'E' or sy-msgty = 'A' or sy-msgty = 'X'.
      message id sy-msgid type 'S' number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      leave program.
    else.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.

endform.                    " set_initial_value
*&---------------------------------------------------------------------*
*&      Form  CHECK_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_rtn.
  perform check_bukrs.
  perform check_month.
endform.                    " CHECK_RTN
*&---------------------------------------------------------------------*
*&      Form  CHECK_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_bukrs.
  select single * from t001 where bukrs = p_bukrs.
  if sy-subrc ne 0.
    message e000(zz) with text-m02.
  endif.
endform.                    " CHECK_BUKRS
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_data.
  perform read_sales.

  perform read_gl_mapping.
  perform read_fi_cogs.
  check wa_error eq space.
  perform derive_chars.
  perform convert_fi_to_pa.

*   perform read_value_field_new.
  perform read_value_field.
  perform read_pa_data.
  perform calculate_co_cogs.

endform.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_month.
  concatenate p_month '01' into w_budat_f.

  call function 'RP_LAST_DAY_OF_MONTHS'
       exporting
            day_in            = w_budat_f
       importing
            last_day_of_month = w_budat_t
       exceptions
            day_in_no_date    = 1
            others            = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  concatenate p_month(4) '0' p_month+4(2)
         into w_perbl.
endform.                    " CHECK_MONTH
*&---------------------------------------------------------------------*
*&      Form  read_value_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_value_field.
  data: lw_index(3) type n.

  ranges: lr_field for dd03l-fieldname.

* SET THE FIELD NAME NEED TO READ
  i_chars-field = 'A~PAOBJNR'.  append i_chars.
  i_chars-field = 'A~PASUBNR'.  append i_chars.

  i_chars-field = 'KNDNR'  .  append i_chars.
  i_chars-field = 'KMLAND' .  append i_chars.

  i_chars-field = 'ARTNR'  .  append i_chars.
  i_chars-field = 'PRODH'  .  append i_chars.
  i_chars-field = 'PAPH1'  .  append i_chars.
  i_chars-field = 'PAPH2'  .  append i_chars.

  i_chars-field = 'VKORG'  .  append i_chars.
  i_chars-field = 'VTWEG'  .  append i_chars.

  i_chars-field = 'B~VRGAR'  .  append i_chars.
* GET THE VALUE FIELDS
  loop at it_gl_pa.
    move: 'I'  to lr_field-sign,
          'CP' to lr_field-option.

    concatenate it_gl_pa-fieldname '*' into lr_field-low.
    collect lr_field.
  endloop.
  if sy-subrc ne 0.
  endif.
* READ THE VALUE FIELDS
  select fieldname ddtext
    into corresponding fields of table it_value_field
    from dd03l as a inner join dd04t as b
      on a~rollname = b~rollname
     and b~ddlanguage = sy-langu
     and b~as4local   = 'A'
*     AND b~as4vers    = 0
   where a~tabname   =  'CE3H201'
     and a~fieldname in lr_field
     and a~as4local  =  'A'.
*     AND a~as4vers   =  0.

  sort it_value_field by ddtext.

* APPEND THE VALUE FIELDS
  loop at it_value_field.
    move: sy-tabix to lw_index.

    move: it_value_field-fieldname to i_chars-field.
    append i_chars.

    concatenate 'FD' lw_index into it_value_field-field.
    modify it_value_field.
  endloop.


  data: lw_field like it_gl_pa-fieldname.
  loop at it_gl_pa.
    concatenate it_gl_pa-fieldname '*' into lw_field.

    loop at it_value_field where fieldname cp lw_field.
      it_value_field-value_field = it_gl_pa-fieldname.
      modify it_value_field.
    endloop.
  endloop.


*what is this ?
*----- Read Structure 'ZSCO_COGS_CO' Schima
  select fieldname into table it_cogs_schima
    from dd03l
   where tabname   =  'ZSCO_COGS_CO'
*     AND fieldname LIKE  'FD%'
     and as4local  =  'A'
     and as4vers   =  0.
endform.                    " read_value_field
*&---------------------------------------------------------------------*
*&      Form  calculate_co_cogs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_co_cogs.
  data: lw_field(100),
        lw_index(3) type n.
  data: l_model like ztco_model_map-modl1.
  field-symbols: <field>.
  data: ls_item   like ce0h201,
        ls_item_e like ce0h201.


* derive the co characteristics
  loop at it_co_cogs.
*  set the value of input structure
    ls_item-perde       = p_month+4(2).
    ls_item-gjahr       = p_month(4).
    ls_item-hzdat       = sy-datum.
    ls_item-budat       = w_budat_t.
    ls_item-bukrs       = c_bukrs.
    ls_item-kokrs       = w_kokrs.
*...rec.type
    ls_item-vrgar       = it_co_cogs-artnr.
*...product
    ls_item-artnr       = it_co_cogs-artnr.
    ls_item-prodh       = it_co_cogs-prodh.
    ls_item-paph1       = it_co_cogs-paph1.
    ls_item-paph2       = it_co_cogs-paph2.
*...customer
    ls_item-kndnr       = it_co_cogs-kndnr.
    ls_item-kmland      = it_co_cogs-kmland.
*...
    ls_item-vkorg       = it_co_cogs-vkorg.
    ls_item-vtweg       = it_co_cogs-vtweg.

*    LS_ITEM-SPART       = '10'.

    call function 'KEDR_COPA_DERIVE'
      exporting
        i_erkrs                        = 'H201'
        i_item                         = ls_item
*       I_TAB_EXCEPTIONS               =
        i_derivation_date              = sy-datum
*       I_DERIVE_ANYWAY                =
*       I_TRACE_MODE                   = ' '
        i_tabname                      = 'CE0H201'
*       I_GLOBAL_FIELDS                =
        i_mass_processing              = 'X'
*       I_CHECK_CHIER_ONLY             =
      importing
        e_item                         = ls_item_e
*       E_TRACE_HANDLE                 =
*       E_TAB_FIELDS_MODIFIED          =
*       E_TAB_USED_SOURCE_FIELDS       =
      exceptions
        derivation_failed              = 1
        check_chier_failed             = 2
        check_values_failed            = 3
        others                         = 4
              .
    if sy-subrc eq 0.
*    Read the characteritics

      it_co_cogs-artnr     = ls_item_e-artnr.
* Begin of changes - UD1K919300
*      if ls_item_e-paph2 is initial.
*      if not ( it_co_cogs-paph1 is initial ) and
*         not ( it_co_cogs-paph1 is initial ).
*          concatenate it_co_cogs-paph1 it_co_cogs-paph2
*          into ls_item_e-paph2.
*       else.
*       ls_item_e-paph2 = it_co_cogs-prodh.
*      endif.
*      endif.
*
* End of changes - UD1K919300
      perform model_mapping_co using ls_item_e-paph2 l_model.

      it_co_cogs-prodh     = ls_item_e-prodh.
      it_co_cogs-paph1     = ls_item_e-paph1.
      it_co_cogs-paph2     = l_model.
      it_co_cogs-kndnr     = ls_item_e-kndnr.
      it_co_cogs-kmland    = ls_item_e-kmland.
** Begin of changes - UD1K918886
** CLear Vehicle Model code if there is no sales Qty
*      if ls_item_e-ABSmg <= 0.
*        clear it_co_cogs-paph2.
*      endif.
** End   of changes - UD1K918886
      modify it_co_cogs.
    endif.

  endloop.


*  LOOP AT it_co_cogs.
*    CLEAR: it_gl_pa.
*
**Mapping vehicle model
*    IF NOT it_co_cogs-paph2 IS INITIAL.
*      SELECT SINGLE modl1 INTO l_model
*       FROM ztco_model_map
*       WHERE paph2 = it_co_cogs-paph2.
*      IF sy-subrc NE 0.
*        CLEAR: it_co_cogs-paph2.
*        MESSAGE s000 WITH 'No model mapping for ' it_co_cogs-paph2.
*      ELSE.
*        it_co_cogs-paph2 = l_model.
*      ENDIF.
*    ENDIF.
*
*
**----- No material
*    IF it_co_cogs-matnr IS INITIAL.
*      MOVE: c_vftyp_3 TO it_co_cogs-vftyp.
*      MODIFY it_co_cogs.
*      CONTINUE.
*    ENDIF.
*
**----- Purchasing material
*    READ TABLE it_gl_pa WITH KEY bklas = it_co_cogs-bklas.
*    IF sy-subrc EQ 0.
*      MOVE: c_vftyp_2 TO it_co_cogs-vftyp.
*      MODIFY it_co_cogs.
*      CONTINUE.
*    ENDIF.
*
**----- FERT, HALB
*    MOVE: c_vftyp_1 TO it_co_cogs-vftyp.
*    MODIFY it_co_cogs.
*  ENDLOOP.
endform.                    " calculate_co_cogs
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_IT_COGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_it_cogs.
  perform collect_co_cogs.
  perform compare_co_fi.
  perform set_status.
endform.                    " CALCULATE_IT_COGS
*&---------------------------------------------------------------------*
*&      Form  display_it_cogs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_it_cogs.
  sort it_cogs by fieldname kmland kndnr model artnr.
  call screen 9000.
endform.                    " display_it_cogs
*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status output.
  case sy-dynnr.
    when '9000'.
*      IF p_test = 'X'.
*        SET PF-STATUS '9100'.
*      ELSE.
      set pf-status '9000'.
*      ENDIF.
      set titlebar  '9000'.
    when '9100'.
      set pf-status '9100'.
      set titlebar  '9100'.
    when '9200'.
      set pf-status '9200'.
      set titlebar  '9200'.
    when '9300'.
      set pf-status '9300'.
      set titlebar  '9300'.
  endcase.
endmodule.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module create_alv_object output.
  if grid_container is initial. "/Not Created Control for ALV GRID
    perform create_container_n_object_9000.
    perform set_attributes_alv_grid.
    perform build_field_catalog using 'IT_COGS'.
    perform assign_itab_to_alv_9000.
    perform sssign_event_9000.
  endif.
endmodule.                 " CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes_alv_grid.
*-- Prepare Setting Attributes and etc of ALV Object

  data : lw_s_dragdrop type lvc_s_dd01. "/ Drag&Drop control settings

  clear : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = c_mark.   "/optimizes the column width
  wa_is_layout-no_merging = c_mark.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.
endform.                    " SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  INITIAL_SET_FIELD_STYLE
*&---------------------------------------------------------------------*
form initial_set_field_style.

endform.                    " INITIAL_SET_FIELD_STYLE
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
form build_field_catalog using p_itab.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  data: lw_itab type slis_tabname.

  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  w_repid = sy-repid.
  lw_itab = p_itab.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       exporting
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       changing
            ct_fieldcat        = it_fieldname.

  perform setting_fieldcat tables it_fieldcat using :
*                                  'S' 'POST_LINE'   ' ',
*                                  ' ' 'KEY'         'X',
*                                  'E' 'OUTPUTLEN'   '6',
*
                                  'S' 'VRGAR'       ' ',
                                  ' ' 'KEY'         'X',
                                  'E' 'OUTPUTLEN'   '1',

                                  'S' 'FIELDNAME'       ' ',
                                  ' ' 'KEY'         'X',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'DDTEXT'       ' ',
                                  ' ' 'KEY'         'X',
                                  'E' 'OUTPUTLEN'   '35',

                                  'S' 'KMLAND'       ' ',
                                  ' ' 'KEY'         'X',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'KNDNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MODEL'       ' ',
                                  ' ' 'KEY'         'X',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'ARTNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'HKONT'       ' ',
                                  ' ' 'KEY'         'X',
                                  'E' 'OUTPUTLEN'   '10',

*                                  'S' 'MAKTX'       ' ',
*                                  ' ' 'KEY'         'X',
*                                  'E' 'COLTEXT'     'Description',


                                  'S' 'FIAMT'       ' ',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'CURRENCY'    t001-waers,

                                  'S' 'COAMT'       ' ',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'CURRENCY'    t001-waers,

                                  'S' 'DIFF'       ' ',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'CURRENCY'    t001-waers,

                                  'S' 'ICON'        ' ',
                                  ' ' 'ICON'        'X',
                                  'E' 'COLTEXT'     'Status',

                                  'S' 'POST'       ' ',
                                  ' ' 'NO_OUT'      'X',
                                  'E' 'CURRENCY'    t001-waers,

                                  'S' 'BELNR'        ' ',
                                  ' ' 'NO_OUT'      'X',
                                  'E' 'CURRENCY'    t001-waers,

                                  'S' 'BELNR'        ' ',
                                  ' ' 'NO_OUT'      'X',
                                  'E' 'CURRENCY'    t001-waers,

                                  'S' 'RBELN'        ' ',
                                  ' ' 'NO_OUT'      'X',
                                  'E' 'CURRENCY'    t001-waers,

                                  'S' 'VFTYP'        ' ',
                                  ' ' 'NO_OUT'      'X',
                                  'E' 'CURRENCY'    t001-waers,

                                  'S' 'FLAG'        ' ',
                                  ' ' 'NO_OUT'      'X',
                                  'E' 'CURRENCY'    t001-waers.



endform.                    " BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_1873   text
*      -->P_1874   text
*      -->P_1875   text
*----------------------------------------------------------------------*
form setting_fieldcat tables   p_fieldcat structure it_fieldcat
                      using    p_gubun
                               p_field
                               p_value.
  data : l_col(40).

  field-symbols <fs>.

* START - FIELD ATTRIBUTE SETTING
  if p_gubun = 'S'.
    clear: p_fieldcat.

    read table it_fieldname into w_fieldname
                            with key fieldname  = p_field.
    if sy-subrc ne 0.
      message e000(zz) with 'Check filed catalog'.
    endif.

    move: w_fieldname-fieldname to p_fieldcat-fieldname.
    exit.
  endif.

* Setting The Field's Attributes
  concatenate 'P_FIELDCAT-' p_field  into l_col.
  assign (l_col) to <fs>.
  move   p_value to <fs>.

* END - FIELD ATTRIBUTE SETTING
  if p_gubun = 'E'.
    add 1 to w_cnt.
    p_fieldcat-col_pos = w_cnt.
    append p_fieldcat.
  endif.
endform.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV_GRID_DATA_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form refresh_alv_grid_data_disp.

endform.                    " REFRESH_ALV_GRID_DATA_DISP
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit input.
  case sy-ucomm.
    when 'EXIT' or 'CANC'.
      leave to screen 0.
  endcase.
endmodule.                 " EXIT  INPUT SET_DIFFRENCE
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9000 input.
  case sy-ucomm.
    when 'BACK'.
      clear: sy-ucomm.
      leave to screen 0.
    when 'POSTING'.
      clear: sy-ucomm.
      perform posting_rtn.
  endcase.
endmodule.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  DBL_CLICK_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
form dbl_click_9000 using p_column_name             "Column Name
                          ps_row_no  like sy-tabix. "Numeric Row ID
  data : lw_sel_index like sy-tabix.

  move: ps_row_no to lw_sel_index.

  read table it_cogs index lw_sel_index.
  if sy-subrc ne 0.
    exit.
  endif.

  move: it_cogs to zsco_cogs_new.

  case p_column_name.
    when 'MATNR' or 'MAKTX'.
      set parameter id 'MAT' field it_cogs-artnr.
      call transaction 'MM03' and skip first screen.
    when others.
      exit.
  endcase.
endform.                    " DBL_CLICK_9000
*&---------------------------------------------------------------------*
*&      Form  SETTING_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_2495   text
*----------------------------------------------------------------------*
form setting_sort tables   p_sort structure it_sort
                  using    p_gubun
                           p_field
                           p_value.
  data : l_col(40).

  field-symbols <fs>.

* START - SORT ATTRIBUTE SETTING
  if p_gubun = 'S'.
    clear: p_sort.

    read table it_fieldname into w_fieldname
                            with key fieldname  = p_field.
    if sy-subrc ne 0.
      message e000(zz) with 'Check filed catalog'.
    endif.

    move: w_fieldname-fieldname to p_sort-fieldname.
    exit.
  endif.

* Setting The Field's Attributes
  concatenate 'P_SORT-' p_field  into l_col.
  assign (l_col) to <fs>.
  move   p_value to <fs>.

* END - FIELD ATTRIBUTE SETTING
  if p_gubun = 'E'.
    add 1 to w_cnt.
    p_sort-spos = w_cnt.
    append p_sort.
  endif.
endform.                    " SETTING_SORT
*&---------------------------------------------------------------------*
*&      Form  POSTING_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form posting_rtn.
  "/Indexes of Selected Rows
  data: lt_rows type lvc_t_row with header line,
        lt_row_no type lvc_t_roid. "/Numeric IDs of Selected Rows
  data: l_line type i.

  call method alv_grid->get_selected_rows
           importing et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  call method cl_gui_cfw=>flush.

  if sy-subrc ne 0.
    w_repid = sy-repid.
    call function 'POPUP_TO_INFORM'
         exporting
              titel = w_repid
              txt2  = sy-subrc
              txt1 =
                 'Error founded during flushing of ALV Grid Control'.
    exit.
  endif.

  clear: w_select, w_success, w_fail.

  read table lt_rows index 1.
  if sy-subrc ne 0.
    message e000(zz) with text-m12.
  endif.

  perform set_target_data tables lt_rows.

  describe table it_cogs_tar lines l_line.
  if l_line eq 0.
    message s000 with 'Not postable records'.
    exit.
  endif.

  perform posting_target_data.

  perform call_bapi_rtn.

  clear: w_msg.
  write: text-m13  to w_msg(11),
         w_select  to w_msg+11(4),
         text-m14  to w_msg+16(10),
         w_success to w_msg+26(4),
         text-m15  to w_msg+31(09),
         w_fail    to w_msg+41(4).
  message s000(zz) with w_msg.
endform.                    " POSTING_RTN


*&---------------------------------------------------------------------*
*&      Form  BDC_ERROR_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form bdc_error_rtn.
  loop at it_cogs_tar.
    read table it_return with key row = sy-tabix.
    if sy-subrc eq 0.
      move it_return-message to it_cogs_tar-msg.
    else.
      move text-m07 to it_cogs_tar-msg.
    endif.

    it_cogs_tar-flag = c_bdcerr.
    it_cogs_tar-icon = icon_yellow_light.

    modify it_cogs_tar.
  endloop.

  describe table it_cogs_tar lines w_fail.
endform.                    " BDC_ERROR_RTN
*&---------------------------------------------------------------------*
*&      Form  SUCCESSFULLY_UPDATED_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form successfully_updated_rtn.
  clear: it_return, it_return[].

  call function 'BAPI_TRANSACTION_COMMIT'
       exporting
            wait   = 'X'
       importing
            return = it_return.
  loop at it_return where type ca 'AEX'.
    perform bdc_error_rtn.
    exit.
  endloop.
  if sy-subrc ne 0.
    perform table_update_for_posting.
  endif.
endform.                    " SUCCESSFULLY_UPDATED_RTN
*&---------------------------------------------------------------------*
*&      Form  TABLE_UPDATE_FOR_POSTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form table_update_for_posting.
  clear: it_cogs-msg.

  loop at it_cogs_tar.
    select max( belnr ) into it_cogs_tar-belnr
      from ce1h201
     where rbeln    = it_cogs_tar-rbeln
       and paledger = '01'
       and vrgar    = it_cogs_tar-vrgar   "p_vrgar
       and perio    = w_perbl
       and hzdat    = sy-datum
       and usnam    = sy-uname
       and bukrs    = p_bukrs
       and kokrs    = w_kokrs
       and artnr    = it_cogs_tar-artnr.
    if it_cogs_tar-belnr is initial.
      move: text-m08     to it_cogs_tar-msg,
            'ZZZZZZZZZZ' to it_cogs_tar-belnr.
    endif.

    it_cogs_tar-flag  = c_finish.
    it_cogs_tar-icon  = icon_green_light.
    it_cogs_tar-post  = it_cogs_tar-diff.

    if it_cogs_tar-rbeln is initial.
      move: it_cogs_tar-belnr to it_cogs_tar-rbeln.
    endif.

    clear: ztco_cogs.
    move-corresponding it_cogs_tar to ztco_cogs.
    move: p_bukrs  to ztco_cogs-bukrs,
          p_month  to ztco_cogs-spmon,
          sy-uname to ztco_cogs-ernam,
          sy-datum to ztco_cogs-erdat,
          sy-uzeit to ztco_cogs-erzet,
          sy-uname to ztco_cogs-aenam,
          sy-datum to ztco_cogs-aedat,
          sy-uzeit to ztco_cogs-aezet.
    modify ztco_cogs.

    modify it_cogs_tar.
  endloop.

  commit work and wait.

  describe table it_cogs_tar lines w_success.
endform.                    " TABLE_UPDATE_FOR_POSTING
*&---------------------------------------------------------------------*
*&      Form  call_bapi_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_bapi_rtn.
  clear: it_return, it_return[].

  move: p_bukrs to w_op_concern.

  call function 'BAPI_COPAACTUALS_POSTCOSTDATA'
       exporting
            operatingconcern = w_op_concern
            testrun          = ' '
       tables
            inputdata        = it_inputdata
            fieldlist        = it_fieldlist
            return           = it_return.
  loop at it_return where type ca 'AEX'.
    call function 'BAPI_TRANSACTION_ROLLBACK'.

    perform bdc_error_rtn.

    exit.
  endloop.
  if sy-subrc ne 0.
    perform successfully_updated_rtn.
  endif.

  perform set_it_cogs.

  perform assign_itab_to_alv_9000.
endform.                    " call_bapi_rtn



*&---------------------------------------------------------------------*
*&      Form  read_gl_mapping
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_gl_mapping.
  select *
    into table it_gl_pa
    from ztco_gl_pa
    where hkont in s_hkont
       or hkont = '530XXX'.
  if sy-subrc ne 0.
    message e000(zz) with text-m11.
  endif.

  perform filter_gl_pa.

  sort it_gl_pa by hkont fieldname.

* get the G/L account
  move: 'I'  to r_hkont-sign,  'EQ' to r_hkont-option,
        'I'  to r_hkont2-sign, 'EQ' to r_hkont2-option,
        'I'  to r_vrgar-sign,  'EQ' to r_vrgar-option.
  clear: r_hkont-high, r_vrgar-high.

  loop at it_gl_pa where hkont <> '530XXX'.

    if it_gl_pa-vftyp = '5'.  "PA segment
      r_hkont2-low = it_gl_pa-hkont.
      collect r_hkont2.
    endif.

    r_hkont-low = it_gl_pa-hkont.
    collect r_hkont.

    r_vrgar-low = it_gl_pa-vrgar.
    collect r_vrgar.
  endloop.
endform.                    " read_gl_mapping

*&---------------------------------------------------------------------*
*&      Form  SET_INPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_input_data.


  perform set_header_data_fert.
  perform set_value_field_fert.

*    WHEN c_vftyp_2.
*      PERFORM set_input_data_pur_mat.
*    WHEN c_vftyp_3.
*      PERFORM set_input_data_no_itme.

endform.                    " SET_INPUT_DATA

*&---------------------------------------------------------------------*
*&      Form  set_target_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_target_data tables pt_rows structure lvc_s_row.
  clear: it_cogs_tar, it_cogs_tar[].

  loop at pt_rows where index ne 0.
    read table it_cogs index pt_rows-index.
    if sy-subrc ne 0.
      message e000(zz) with text-m01.
    endif.

*    CHECK it_cogs-flag EQ c_ready OR
*          it_cogs-flag EQ c_bdcerr.
    if it_cogs-diff ne 0 and
       it_cogs-flag ne 4 and
       it_cogs-post_line = c_postable.
      move it_cogs to it_cogs_tar.

      append it_cogs_tar.

      w_select = w_select + 1.
    endif.
  endloop.
endform.                    " set_target_data
*&---------------------------------------------------------------------*
*&      Form  posting_target_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form posting_target_data.
  clear: it_cogs_tar, w_record.

  perform set_field_list.

  sort it_cogs_tar by fieldname kmland kndnr artnr model.

  loop at it_cogs_tar.
    w_record = w_record + 1.

    perform set_input_data.
  endloop.
endform.                    " posting_target_data
*&---------------------------------------------------------------------*
*&      Form  set_field_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_list.
  clear: it_fieldlist, it_fieldlist[],
         it_inputdata, it_inputdata[].

  move: 'VRGAR'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'BUDAT'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'PERDE'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'GJAHR'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'BUKRS'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'KOKRS'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'FRWAE'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'KNDNR'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'ARTNR'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'PRODH'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'PAPH2'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'RBELN'  to it_fieldlist-fieldname.
  append it_fieldlist.
  move: 'BELNR'  to it_fieldlist-fieldname.
  append it_fieldlist.

*----- Calculate separated diffrence amount
  loop at it_value_field.
    read table it_gl_pa with key fieldname = it_value_field-value_field
     .
    if sy-subrc ne 0.
      message e000(zz) with text-m01.
    endif.
    move: it_gl_pa-fieldname to it_fieldlist-fieldname.
    collect it_fieldlist.
  endloop.
endform.                    " set_field_list
*&---------------------------------------------------------------------*
*&      Form  set_header_data_fert
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_header_data_fert.
  data: l_model like ce4h201-paph2.

  move: w_record           to it_inputdata-record_id,
        'VRGAR'            to it_inputdata-fieldname,
        it_cogs_tar-vrgar  to it_inputdata-value.
  append it_inputdata.

  move: w_record     to it_inputdata-record_id,
        'BUDAT'      to it_inputdata-fieldname,
        w_budat_t    to it_inputdata-value.
  append it_inputdata.

  move: w_record     to it_inputdata-record_id,
        'PERDE'      to it_inputdata-fieldname,
        p_month+4(2) to it_inputdata-value.
  append it_inputdata.

  move: w_record   to it_inputdata-record_id,
        'GJAHR'    to it_inputdata-fieldname,
        p_month(4) to it_inputdata-value.
  append it_inputdata.

  move: w_record to it_inputdata-record_id,
        'BUKRS'  to it_inputdata-fieldname,
        p_bukrs  to it_inputdata-value.
  append it_inputdata.

  move: w_record to it_inputdata-record_id,
        'KOKRS'  to it_inputdata-fieldname,
        w_kokrs  to it_inputdata-value.
  append it_inputdata.

  move: w_record    to it_inputdata-record_id,
        'FRWAE'     to it_inputdata-fieldname,
        t001-waers  to it_inputdata-value.
  append it_inputdata.

  move: w_record      to it_inputdata-record_id,
        'KNDNR'       to it_inputdata-fieldname,
        it_cogs_tar-kndnr to it_inputdata-value.
  append it_inputdata.

  move: w_record      to it_inputdata-record_id,
        'ARTNR'       to it_inputdata-fieldname,
        it_cogs_tar-artnr to it_inputdata-value.
  append it_inputdata.


  move: w_record      to it_inputdata-record_id,
        'PRODH'       to it_inputdata-fieldname.

*       read the hierarchy by then model
  select single paph2  into l_model
    from ztco_model_map
    where modl1 = it_cogs_tar-model.
  move  l_model       to it_inputdata-value.
  append it_inputdata.

  move: w_record      to it_inputdata-record_id,
        'PAPH2'       to it_inputdata-fieldname.
  move: l_model       to it_inputdata-value.
  append it_inputdata.

  move: w_record      to it_inputdata-record_id,
        'RBELN'       to it_inputdata-fieldname,
        it_cogs_tar-rbeln to it_inputdata-value.
  append it_inputdata.

  move: w_record      to it_inputdata-record_id,
        'BELNR'       to it_inputdata-fieldname,
        space to it_inputdata-value.
  append it_inputdata.
endform.                    " set_header_data_fert
*&---------------------------------------------------------------------*
*&      Form  set_value_field_fert
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_value_field_fert.
  clear: w_cosum, w_diff.

*  PERFORM summarize_co_cogs_amount USING it_cogs_tar.

  perform append_value_field.
endform.                    " set_value_field_fert
*&---------------------------------------------------------------------*
*&      Form  append_value_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_value_field.


  loop at it_value_field.

* get the value field name
    if it_value_field-value_field = it_cogs_tar-fieldname.

      move: w_record                   to it_inputdata-record_id,
            it_cogs_tar-diff           to it_inputdata-value,
            it_value_field-value_field to it_inputdata-fieldname,
            t001-waers                 to it_inputdata-currency.
      append it_inputdata.
    else.
      move: w_record                   to it_inputdata-record_id,
            0                          to it_inputdata-value,
            it_value_field-value_field  to it_inputdata-fieldname,
            t001-waers                 to it_inputdata-currency.
      append it_inputdata.

    endif.


  endloop .

endform.                    " append_value_field

*&---------------------------------------------------------------------*
*&      Form  SET_IT_COGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_it_cogs.
  loop at it_cogs_tar.
    read table it_cogs with key fieldname = it_cogs_tar-fieldname
                                kmland = it_cogs_tar-kmland
                                kndnr  = it_cogs_tar-kndnr
                                artnr  = it_cogs_tar-artnr
                                model  = it_cogs_tar-model
                                hkont  = it_cogs_tar-hkont .
    if sy-subrc ne 0.
      message e000(zz) with text-m01.
    endif.

    move: it_cogs_tar to it_cogs.

    modify it_cogs index sy-tabix.
  endloop.
endform.                    " SET_IT_COGS
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_container_n_object_9000.
*- Create Container('GRID_CONTAINER') with Custom Control on screen
  create object grid_container
         exporting container_name = wa_custom_control
         exceptions
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
          lifetime_dynpro_dynpro_link = 5.

  if sy-subrc ne 0.
    w_repid = sy-repid.
    call function 'POPUP_TO_INFORM'
         exporting
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  endif.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  create object alv_grid
         exporting i_parent = grid_container
                   i_appl_events = 'X'.
endform.                    " create_container_n_object_9000
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form assign_itab_to_alv_9000.
*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'

  call method alv_grid->set_table_for_first_display
     exporting i_structure_name = 'ZSCO_COGS_NEW'
               is_layout        = wa_is_layout
               i_save           = wa_save
               is_variant       = wa_variant
               i_default        = space
*               it_toolbar_excluding = it_toolbar_excluding[]
     changing  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_cogs[].
endform.                    " assign_itab_to_alv_9000
*&---------------------------------------------------------------------*
*&      Form  sssign_event_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sssign_event_9000.
*--  Regist event for Edit
  if sy-batch is initial.
    call method alv_grid->register_edit_event
        exporting i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  endif.

*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
  create object event_receiver.
*    SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALV_GRID.
**-   toolbar control event
*    SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALV_GRID.
  set handler event_receiver->handle_double_click  for alv_grid.
**    SET HANDLER EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALV_GRID.
**    SET HANDLER EVENT_RECEIVER->HANDLE_ONF4          FOR ALV_GRID.
**    SET HANDLER EVENT_RECEIVER->HANDLE_MENU_BUTTON  FOR ALV_GRID.
**    SET HANDLER EVENT_RECEIVER->HANDLE_AFTER_USER_COMMAND FOR ALV_GRID
  .
**    SET HANDLER EVENT_RECEIVER->HANDLE_BUTTON_CLICK FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_BEFORE_USER_COMMAND FOR ALV_GRID
  .
*  SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED FOR ALV_GRID
  .

*- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
*    CALL METHOD alv_grid->set_toolbar_interactive.
*
*    CALL METHOD cl_gui_control=>set_focus
*                        EXPORTING control = alv_grid.
endform.                    " sssign_event_9000
*&---------------------------------------------------------------------*
*&      Form  COLLECT_INPUTDATA_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form collect_inputdata_value.
  data: lw_field(50),
        lw_index(3) type n.

  field-symbols: <field>.

  loop at it_fi_cogs .
*                       WHERE  = c_vftyp_2
*                       AND bklas = it_gl_pa-bklas
*                       AND vkorg = it_cogs_tar-vkorg
*                       AND vtweg = it_cogs_tar-vtweg.
    it_inputdata-value = it_inputdata-value + it_fi_cogs-dmbtr.
  endloop.

  loop at it_co_cogs .
*                       WHERE vftyp = c_vftyp_2
*                       AND bklas = it_gl_pa-bklas
*                       AND vkorg = it_cogs_tar-vkorg
*                       AND vtweg = it_cogs_tar-vtweg.
    do 999 times.
      move: sy-index to lw_index.

      concatenate: 'IT_CO_COGS-FD' lw_index into lw_field.

      assign: (lw_field)     to <field>.
      if sy-subrc ne 0.
        exit.
      endif.

      it_inputdata-value = it_inputdata-value - <field>.
    enddo.
  endloop.
endform.                    " COLLECT_INPUTDATA_VALUE
*&---------------------------------------------------------------------*
*&      Module  create_alv_object_9300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module create_alv_object_9300 output.

endmodule.                 " create_alv_object_9300  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  read_fi_cogs_by_plant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_WERKS  text
*----------------------------------------------------------------------*
form read_fi_cogs.
  data: lw_month(6),
        lw_gjahr      like bsis-gjahr,
        lw_monat      like bsis-monat,
        wa_awkey      like bkpf-awkey.

  move: p_month       to lw_month,
        lw_month(4)   to lw_gjahr,
        lw_month+4(2) to lw_monat.

* read the G/L account for the COGS
  data: l_stgrd like bkpf-stgrd.
  select a~hkont a~zuonr a~gjahr
         a~shkzg a~dmbtr b~stblg
         b~awtyp b~awkey                                    "UD1K919300
     into corresponding fields of it_bsis
     from bsis as a inner join bkpf as b
       on a~bukrs = b~bukrs and
          a~gjahr = b~gjahr and
          a~belnr = b~belnr
     where a~bukrs = p_bukrs
      and  a~hkont in r_hkont
      and  a~gjahr = lw_gjahr
      and  a~monat = lw_monat
      and  b~stgrd <> '03'.

    if it_bsis-stblg <> space.
      select single stgrd into l_stgrd
        from  bkpf
        where bukrs = p_bukrs
          and gjahr = lw_gjahr
          and belnr = it_bsis-stblg.
      if l_stgrd = '03'.
        continue.
      endif.
* Begin of changes - UD1K919300
    elseif  it_bsis-awtyp eq 'RMRP'.
      select single * from rbkp
          where belnr eq it_bsis-awkey(10).
      if sy-subrc eq 0.
        clear wa_awkey.
        concatenate rbkp-stblg  rbkp-stjah into wa_awkey.
        select single * from bkpf
         where  awkey = wa_awkey.
        if sy-subrc eq '0' and ( bkpf-stgrd eq '03' or
                                  bkpf-stgrd eq '01' ).
          continue.
        endif.
      endif.
* End of changes - UD1K919300
    endif.

* temp
    check it_bsis-zuonr in   s_artnr.

    if it_bsis-shkzg = 'H'.
      it_bsis-dmbtr = it_bsis-dmbtr * -1.
    endif.

    it_bsis-shkzg = 'S'.
    it_bsis-awkey = ''.  it_bsis-awtyp = ''.                "UD1K919300

    collect it_bsis.
  endselect.

*  CLEAR: it_bsis.
*  DATA: l_cnt TYPE i.
** read the G/L account for PA segment
*  DESCRIBE TABLE r_hkont2 LINES l_cnt.
*  IF l_cnt > 0.
*    SELECT hkont zuonr gjahr belnr buzei
*           monat shkzg dmbtr budat
*       APPENDING CORRESPONDING FIELDS OF TABLE it_bsis
*       FROM bsis
*       WHERE bukrs = p_bukrs
*        AND  hkont IN r_hkont2
*        AND  gjahr = lw_gjahr
*        AND  monat = lw_monat.
*  ENDIF.

*530XXX
  perform collect_cogs_interim.


  describe table it_bsis lines sy-index.
  if sy-index = 0.
    wa_message =  'No FI posting exists'.
    wa_error = 'X'.
    exit.
  endif.


endform.                    " read_fi_cogs_by_plant
*&---------------------------------------------------------------------*
*&      Form  WRITE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_message.
  skip.
  write / wa_message.
endform.                    " WRITE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  derive_characteriscs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_BASIS  text
*----------------------------------------------------------------------*
form derive_characteriscs using    pt_bsis like it_bsis.
  data: l_flag  .

* get the processing method for G/L account
  read table it_gl_pa with key hkont = pt_bsis-hkont
                           binary search.
  if sy-subrc ne 0.
    message e000 with 'Check program'.
  endif.
  it_fi_cogs-fname = it_gl_pa-fieldname.
  it_fi_cogs-vrgar = it_gl_pa-vrgar.


  check it_bsis-hkont <> '530XXX'.

* derive the characteristic
  case it_gl_pa-vftyp.

    when '1'.
*     Raw material
      perform derive_by_material using pt_bsis.

    when '2'.
      perform check_sales_view using pt_bsis l_flag.
      if l_flag = 'S'.
        perform derive_by_product using pt_bsis.
      elseif l_flag = 'R'.
        perform derive_by_material using pt_bsis.
      elseif l_flag = 'E'.
        perform derive_by_blank using pt_bsis.
      endif.

    when '4'.
*      by purchase order
      perform derive_by_po using pt_bsis.

    when '5'.
*      by PA segment
      perform derive_by_pa using pt_bsis.

    when '9'.
*      blank , get company code
      perform derive_by_blank using pt_bsis.

  endcase.


endform.                    " derive_characteriscs
*&---------------------------------------------------------------------*
*&      Form  DERIVE_BY_MATERIAL
*&---------------------------------------------------------------------*
*   for ROH material, following characteristcs are need to derived:
*          1. material number
*          2. model
*          3.
*----------------------------------------------------------------------*
*      -->P_PT_BSIS  text
*----------------------------------------------------------------------*
form derive_by_material using   pt_ss like it_bsis.
  data: wa_mara like mara.
  data: l_model like ztco_model_map-modl1.

* check if the material number exist
  select single * from mara
    where matnr = pt_ss-zuonr .

  if sy-subrc ne 0.
*   if not exist, only company code can be derived
    perform derive_by_blank using pt_ss.

  else.
* save the material number
*    it_fi_cogs-matnr = mara-matnr.

* get model
    it_fi_cogs-model = mara-matkl(2).

*   check the model
    select single modl1 into l_model
      from ztco_model_map
      where modl2 = it_fi_cogs-model
        and modl1 in s_model.

    if sy-subrc ne 0.
      clear: it_fi_cogs-model.
    else.
      it_fi_cogs-model = l_model.
    endif.
  endif.

endform.                    " DERIVE_BY_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  CHECK_SALES_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_BSIS  text
*      -->P_L_FLAG  text
*----------------------------------------------------------------------*
form check_sales_view using    pt_bsis like it_bsis
                               p_flag.
*S - product
*R - raw material
*E - empty
  p_flag = 'E'.

  read table i_sales with key artnr = pt_bsis-zuonr.
  if sy-subrc = 0 and i_sales-erlos > 0.
    p_flag = 'S'.
  else.
    select single * from mara
       where matnr = pt_bsis-zuonr .
    if sy-subrc = 0.
      p_flag = 'R'.
    endif.
  endif.

** check if the material exist
*  SELECT SINGLE * FROM mara
*    WHERE matnr = pt_bsis-zuonr .
*  IF sy-subrc = 0.
*    SELECT SINGLE * FROM mvke
*      WHERE matnr = pt_bsis-zuonr.
*    IF sy-subrc EQ 0.
*      p_flag = 'S'.   "salable product
*
*      IF mara-MTART <> 'FERT'.
**    check info-record.
*        SELECT SINGLE * FROM a005
*               WHERE kappl = 'V'
*                 AND kschl = 'ZP00'
*                 AND matnr = pt_bsis-zuonr
*                 AND datab <= pt_bsis-budat
*                 AND datbi >= pt_bsis-budat.
*        IF sy-subrc <> 0.
*          p_flag = 'R'.   "salable product
*        ENDIF.
*      ENDIF.
*    ELSE.
*      p_flag = 'R'.   "raw material
*    ENDIF.
*  ELSE.
*    p_flag  = 'E'.    "material does not exist
*  ENDIF.


endform.                    " CHECK_SALES_VIEW
*&---------------------------------------------------------------------*
*&      Form  DERIVE_BY_PRODUCT
*&---------------------------------------------------------------------*
*       get following charateristics by salable product
*          1. company code
*          2. product name( material number)
*          3. customer
*          4.vehicle model
*          5.
*----------------------------------------------------------------------*
*      -->P_PT_BSIS  text
*----------------------------------------------------------------------*
form derive_by_product using    pt_bsis like it_bsis .
  data: l_model like ztco_model_map-modl1.
* check if the material exist
  select single * from mara
    where matnr = pt_bsis-zuonr .
  if sy-subrc ne 0.
*   material does not exist
    perform derive_by_blank using pt_bsis.
    exit.
  endif.

* salable product
* get product name
  it_fi_cogs-artnr  = pt_bsis-zuonr.
  it_fi_cogs-matnr  = pt_bsis-zuonr.

* model
  if mara-mtart = 'FERT'.
    it_fi_cogs-model = pt_bsis-zuonr+6(2).
    select single modl1 into l_model
      from ztco_model_map
      where modl1 = it_fi_cogs-model
        and modl1 in s_model.

    if sy-subrc ne 0.
      clear: it_fi_cogs-model.
    endif.
  else.
    it_fi_cogs-model  = mara-matkl(2).
    select single modl1 into l_model
      from ztco_model_map
      where modl2 = it_fi_cogs-model
        and modl1 in s_model.

    if sy-subrc ne 0.
      clear: it_fi_cogs-model.
    else.
      it_fi_cogs-model = l_model.
    endif.

  endif.


* country and customer
* spare part/customer: table a005
* vehicle /:   table a004
* valid in end date of the month
  select single * from a005
    where matnr = pt_bsis-zuonr
     and  datbi ge w_budat_t
     and  datab le w_budat_t.

  if sy-subrc eq 0.

*   spare parts customer
    it_fi_cogs-kunnr = a005-kunnr.
*   get the customer country
    perform get_customer_county using it_fi_cogs-kunnr.

  elseif mara-mtart = 'FERT'.

*    get vehicle customer
*    it_fi_cogs-kunnr = it_bsis-zuonr+1(5).
    data: l_kunnr  like kna1-kunnr.
    if it_bsis-zuonr+13(1) = ' '.  "old BOM
      l_kunnr = it_bsis-zuonr+1(5).
    else.
      select single old_dealer into l_kunnr from ztebpp_deal_conv
            where new_dealer = it_bsis-zuonr+4(1).
      concatenate it_bsis-zuonr+1(3) l_kunnr(2) into l_kunnr.
    endif.

    it_fi_cogs-kunnr = l_kunnr.

*    get the country
    perform get_customer_county using it_fi_cogs-kunnr.

  endif.




endform.                    " DERIVE_BY_PRODUCT
*&---------------------------------------------------------------------*
*&      Form  DERIVE_BY_PO
*&---------------------------------------------------------------------*
*     get the follwoing characterirics by purchase order
*----------------------------------------------------------------------*
*      -->P_PT_BSIS  text
*----------------------------------------------------------------------*
form derive_by_po using    pt_bsis like it_bsis.
  data: wa_mara  like mara.

* check if the order is invoice or po number
  select single matnr into mara-matnr
    from ekpo
    where ebeln = pt_bsis-zuonr(10)
      and ebelp = pt_bsis-zuonr+10(5).
  if sy-subrc ne 0.
*   this purchase number does not exist
    perform derive_by_blank using pt_bsis.
  else.
*   p/o number exist, get material master
    select single *
      from mara
      where matnr = wa_mara-matnr.
    if sy-subrc ne 0.
      message e000 with 'Something wrong, check program'.
    else.
      it_fi_cogs-model = mara-matkl(2).
    endif.

  endif.


endform.                    " DERIVE_BY_PO
*&---------------------------------------------------------------------*
*&      Form  BERIVE_BLANK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_BSIS  text
*----------------------------------------------------------------------*
form derive_by_blank using    pt_bsis.

endform.                    " BERIVE_BLANK
*&---------------------------------------------------------------------*
*&      Form  get_customer_county
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FI_COGS_KUNNR  text
*----------------------------------------------------------------------*
form get_customer_county using    p_kunnr.
  check p_kunnr <> space.

  select single * from kna1
    where kunnr = p_kunnr.
  if sy-subrc ne 0.
    message s000 with 'Customer' p_kunnr
         'does not exist'.
    clear: it_fi_cogs-kmland.
  else.
    it_fi_cogs-kmland = kna1-land1.
  endif.
endform.                    " get_customer_county
*&---------------------------------------------------------------------*
*&      Form  collect_co_cogs
*&---------------------------------------------------------------------*
*    summarize the co cogs total amount by characteritic
*----------------------------------------------------------------------*
form collect_co_cogs.
  field-symbols: <vfield>.
  data: l_text(30).

*  DATA: lt_co LIKE it_co_cogs OCCURS 0 WITH HEADER LINE.
** summarize the co cogs amount.
*  LOOP AT it_co_cogs.
*    COLLECT it_co_cogs INTO lt_co.
*  ENDLOOP.
*  REFRESH it_co_cogs.
*  CLEAR: it_co_cogs.
*  it_co_cogs[] = lt_co[].
*
* SEPARATE VALUE FILEDS TO DIFFRENT RECORDS
  clear: it_cogs[], it_cogs.

  loop at it_co_cogs.
    it_cogs-vrgar  = it_co_cogs-vrgar.
    it_cogs-kndnr  = it_co_cogs-kndnr.
    it_cogs-kmland = it_co_cogs-kmland.
    it_cogs-model  = it_co_cogs-paph2.
    it_cogs-artnr  = it_co_cogs-artnr.
    loop at it_value_field.
      it_cogs-fieldname = it_value_field-value_field.
*      it_cogs-ddtext    = it_value_field-ddtext.
      concatenate 'IT_CO_COGS-' it_value_field-field into l_text.
      assign (l_text) to <vfield>.
      it_cogs-coamt = <vfield>.
      if it_cogs-coamt <> 0.
        collect it_cogs. " APPEND it_cogs.
      endif.
    endloop.
  endloop.

  delete it_cogs where coamt = 0.
endform.                    " collect_co_cogs
*&---------------------------------------------------------------------*
*&      Form  COMPARE_CO_FI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form compare_co_fi.
  data: lt_vfield like it_gl_pa occurs 0 with header line.
  data: l_flag .
  data: l_total like it_cogs-fiamt.
  data: l_total_co like it_cogs-coamt.
  data: wa_cogs like it_cogs.
  data: l_tabix like sy-tabix.

  data: l_hkont like bsis-hkont.


* Attach the value field name to FI VALUE
  clear: it_cogs_tmp[], it_cogs_tmp.
  loop at it_fi_cogs.
*  check the value field
    read table it_gl_pa with key hkont = it_fi_cogs-hkont.
* Begin of changes - UD1K919300
    check not  it_gl_pa-vftyp eq '5'.
* End  of  changes - UD1K919300
*  collect FI value with vale field attached
    it_cogs_tmp-fieldname = it_gl_pa-fieldname.
*    perform read_vf_desc using it_cogs_tmp-fieldname
*                               it_cogs_tmp-ddtext.

    it_cogs_tmp-kmland    = it_fi_cogs-kmland.
    it_cogs_tmp-kndnr     = it_fi_cogs-kunnr.
    it_cogs_tmp-artnr     = it_fi_cogs-artnr.
    it_cogs_tmp-model     = it_fi_cogs-model.

    it_cogs_tmp-fiamt     = it_fi_cogs-dmbtr.
    it_cogs_tmp-hkont     = it_fi_cogs-hkont.
    it_cogs_tmp-vrgar     = it_fi_cogs-vrgar.

    collect it_cogs_tmp.

  endloop.

*
  sort it_cogs_tmp by fieldname kmland kndnr artnr model hkont.

* combine CO and FI value together

  loop at it_cogs.
    clear: it_cogs_tmp.
    read table it_cogs_tmp with key
                     fieldname = it_cogs-fieldname
                     kmland = it_cogs-kmland
                     kndnr  = it_cogs-kndnr
                     artnr  = it_cogs-artnr
                     model  = it_cogs-model.

    if sy-subrc = 0.
      l_tabix = sy-tabix.

* multiple account can go single value field.
*      it_cogs_tmp-hkont = it_cogs-hkont.
      it_cogs_tmp-coamt = it_cogs-coamt.
      modify it_cogs_tmp index l_tabix
          transporting hkont coamt.
    else.
** Begin of changes - UD1K918886
*      READ TABLE it_cogs_tmp WITH KEY
*                      fieldname = it_cogs-fieldname
*                      kmland = it_cogs-kmland
*                      kndnr  = it_cogs-kndnr.
*      if sy-subrc eq 0.
*        l_tabix = sy-tabix.
*        it_cogs_tmp-coamt = it_cogs-coamt.
*        MODIFY it_cogs_tmp INDEX l_tabix TRANSPORTING hkont coamt.
*      else.
** End of changes - UD1K918886

      it_cogs_tmp       = it_cogs.
      it_cogs_tmp-hkont = it_cogs-hkont.
      it_cogs_tmp-coamt = it_cogs-coamt.
      append it_cogs_tmp.
*     endif.                                                "UD1K918886
    endif.

  endloop.

* Changes as per ANDY - Copy CO Amount as FI AMT and multiply by -1
* for all the accounts which have processing TYPE as 5 in ZTCO_GL_PA
* Table - Since CO is not having any account No. So i have match by
*field Name to find processing type

* Begin of changes - UD1K919300

  loop at it_cogs_tmp.
    read table it_gl_pa with key fieldname = it_cogs_tmp-fieldname.
    if sy-subrc eq 0 and it_gl_pa-vftyp eq '5'.
      it_cogs_tmp-fiamt =   it_cogs_tmp-coamt * -1.
      modify it_cogs_tmp transporting fiamt.
    endif.
  endloop.

* End of changes - UD1K919300

* calculate the difference of CO and FI

  sort it_cogs_tmp by fieldname kmland kndnr artnr model hkont.

  loop at it_cogs_tmp.
    at new model.
      l_tabix = sy-tabix.
    endat.
    at end of model.
      sum.
      it_cogs_tmp-diff = it_cogs_tmp-fiamt - it_cogs_tmp-coamt.
      it_cogs_tmp-post_line  = c_postable.
      modify it_cogs_tmp index l_tabix transporting post_line diff .
    endat.

  endloop.

*
  refresh: it_cogs[].
  clear:   it_cogs.

  it_cogs[] = it_cogs_tmp[].


endform.                    " COMPARE_CO_FI
*&---------------------------------------------------------------------*
*&      Form  SET_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_status.
  data: l_ddtext like dd04t-ddtext.

  loop at it_cogs.
    if it_cogs-diff eq 0 or
       it_cogs-post_line  ne c_postable.
      continue.
    endif.
    if it_cogs-belnr is initial .
      move: c_ready to it_cogs-flag.
      move: icon_light_out to it_cogs-icon.
      modify it_cogs.
      continue.
    endif.

    if it_cogs-diff eq it_cogs-post .
      move: c_finish to it_cogs-flag.
      move: icon_green_light to it_cogs-icon.
      modify it_cogs.
    else.
      move: c_error to it_cogs-flag.
      move: icon_red_light to it_cogs-icon.
      modify it_cogs.
    endif.
  endloop.

* get the value field description
  loop at it_cogs.
    perform read_vf_desc using it_cogs-fieldname
                               it_cogs-ddtext.
    modify it_cogs.
  endloop.
endform.                    " SET_STATUS
*&---------------------------------------------------------------------*
*&      Form  model_mapping_fi
*&---------------------------------------------------------------------*
form model_mapping_fi using    p_model_in
                            p_model_out.

  select single modl1 into p_model_out
    from ztco_model_map
    where paph2 = p_model_in
      and modl1 in s_model.

  if sy-subrc ne 0.
    clear: p_model_out.
  endif.


endform.                    " model_mapping_fi
*&---------------------------------------------------------------------*
*&      Form  model_mapping_co
*&---------------------------------------------------------------------*
form model_mapping_co using    p_model_in
                               p_model_out.

  select single modl1 into p_model_out
    from ztco_model_map
    where paph2 = p_model_in.

  if sy-subrc ne 0.
    clear: p_model_out.
  endif.


endform.                    " model_mapping_co
*&---------------------------------------------------------------------*
*&      Form  MAP_MODEL_R
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FI_COGS_MODEL  text
*      -->P_LS_ITEM_PAPH2  text
*----------------------------------------------------------------------*
form map_model_r using    p_model
                          p_paph2.
  select single paph2 into p_paph2
    from ztco_model_map
    where modl1  = p_model
      and modl1 in s_model.
  if sy-subrc ne 0.
    clear: p_paph2.
  endif.

endform.                    " MAP_MODEL_R
*&---------------------------------------------------------------------*
*&      Form  read_vf_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_VALUE_FIELD_VALE_FIELD  text
*      -->P_IT_COGS_DDTEXT  text
*----------------------------------------------------------------------*
form read_vf_desc using    p_vfield
                           p_ddtext.

  read table it_value_field with key value_field = p_vfield.
  if sy-subrc eq 0.
    p_ddtext = it_value_field-ddtext.
  else.
    clear: p_ddtext.
  endif.


endform.                    " read_vf_desc
*&---------------------------------------------------------------------*
*&      Form  collect_cogs_interim
*&---------------------------------------------------------------------*
form collect_cogs_interim.
  data: lw_gjahr      like bsis-gjahr,
        lw_dmbtr      like bsis-dmbtr.
  data: i_glt0 like glt0 occurs 0 with header line.
  tables: glt0.

  move: p_month(4)    to lw_gjahr.
  clear lw_dmbtr.

  select * from glt0
     where rldnr =  '00'
      and  rrcty =  '0'
      and  bukrs = p_bukrs
      and  racct in r_racct
      and  ryear =  lw_gjahr.
*   i_glt0-racct = glt0-racct.
    case p_month+4(2).
      when  '01'.  lw_dmbtr = lw_dmbtr + glt0-hsl01.
      when  '02'.  lw_dmbtr = lw_dmbtr + glt0-hsl02.
      when  '03'.  lw_dmbtr = lw_dmbtr + glt0-hsl03.
      when  '04'.  lw_dmbtr = lw_dmbtr + glt0-hsl04.
      when  '05'.  lw_dmbtr = lw_dmbtr + glt0-hsl05.
      when  '06'.  lw_dmbtr = lw_dmbtr + glt0-hsl06.
      when  '07'.  lw_dmbtr = lw_dmbtr + glt0-hsl07.
      when  '08'.  lw_dmbtr = lw_dmbtr + glt0-hsl08.
      when  '09'.  lw_dmbtr = lw_dmbtr + glt0-hsl09.
      when  '10'.  lw_dmbtr = lw_dmbtr + glt0-hsl10.
      when  '11'.  lw_dmbtr = lw_dmbtr + glt0-hsl11.
      when  '12'.  lw_dmbtr = lw_dmbtr + glt0-hsl12.
    endcase.
  endselect.

  clear it_bsis.
  it_bsis-hkont = '530XXX'.
  it_bsis-gjahr = lw_gjahr.
*  it_bsis-monat = p_month+4(2).
  it_bsis-shkzg = 'S'.
  it_bsis-dmbtr = lw_dmbtr.
  append it_bsis.

endform.                    " collect_cogs_interim
*&---------------------------------------------------------------------*
*&      Form  derive_by_pa
*&---------------------------------------------------------------------*
form derive_by_pa using    f_bsis like it_bsis.

* TO BE IMPLEMENTED...

  select single paobjnr pasubnr
    into (it_fi_cogs-paobjnr, it_fi_cogs-pasubnr)
    from bseg
    where bukrs  = p_bukrs
      and gjahr  = f_bsis-gjahr
      and belnr  = f_bsis-belnr
      and buzei  = f_bsis-buzei.


* not working...
*  CALL FUNCTION 'COPA_CALL_PROF_SEGMENT'
*       EXPORTING
*            i_display = ' '
*            i_dialog  = ' '
*            i_paobjnr = it_fi_cogs-paobjnr
*            i_kokrs   = p_bukrs
*            i_gjahr   = p_month(4)
*       IMPORTING
*            e_paobjnr = it_fi_cogs-paobjnr.


endform.                    " derive_by_pa
*&---------------------------------------------------------------------*
*&      Form  read_pa_data
*&---------------------------------------------------------------------*
form read_pa_data.
* Actual = CE1xxxx
* Segment Table
  select (i_chars)
    into  table it_co_cogs
    from ce4h201 as a inner join ce3h201 as b
                         on a~paobjnr = b~paobjnr
                        and b~perbl   = w_perbl
   where a~aktbo    =  'X'
     and a~bukrs    =  p_bukrs
     and a~kokrs    =  w_kokrs
*     AND a~werks    =  p_werks
     and a~artnr    in s_artnr
     and b~paledger =  '01'
     and b~vrgar    in  r_vrgar
     and b~plikz    =  '0'.
*     AND b~versi    =  '0'.

  loop at it_co_cogs.
    if  it_co_cogs-fd001 = 0 and it_co_cogs-fd002 = 0
    and it_co_cogs-fd003 = 0 and it_co_cogs-fd004 = 0
    and it_co_cogs-fd005 = 0 and it_co_cogs-fd006 = 0
    and it_co_cogs-fd007 = 0 and it_co_cogs-fd008 = 0
    and it_co_cogs-fd009 = 0 and it_co_cogs-fd010 = 0
    and it_co_cogs-fd011 = 0 and it_co_cogs-fd012 = 0
    and it_co_cogs-fd013 = 0 and it_co_cogs-fd014 = 0
    and it_co_cogs-fd015 = 0 and it_co_cogs-fd016 = 0
    and it_co_cogs-fd017 = 0 and it_co_cogs-fd018 = 0
    and it_co_cogs-fd019 = 0 and it_co_cogs-fd020 = 0
    and it_co_cogs-fd021 = 0 and it_co_cogs-fd022 = 0
    and it_co_cogs-fd023 = 0 and it_co_cogs-fd024 = 0
    and it_co_cogs-fd025 = 0 and it_co_cogs-fd026 = 0
    and it_co_cogs-fd027 = 0 and it_co_cogs-fd028 = 0
    and it_co_cogs-fd029 = 0 and it_co_cogs-fd030 = 0
    and it_co_cogs-fd031 = 0 and it_co_cogs-fd032 = 0
    and it_co_cogs-fd033 = 0 and it_co_cogs-fd034 = 0
    and it_co_cogs-fd035 = 0 and it_co_cogs-fd036 = 0
    and it_co_cogs-fd037 = 0 and it_co_cogs-fd038 = 0
    and it_co_cogs-fd039 = 0 and it_co_cogs-fd040 = 0.
      delete it_co_cogs.
    endif.
  endloop.
endform.                    " read_pa_data
*&---------------------------------------------------------------------*
*&      Form  convert_fi_to_pa
*&---------------------------------------------------------------------*
form convert_fi_to_pa.
  data: ls_item       like ce0h201,
        ls_item_e     like ce0h201,
        l_model       like ztco_model_map-modl1.

* Derive the characteristic by using CO-PA method
  loop at it_fi_cogs.
    clear ls_item.

*  set the value of input structure
    ls_item-perde       = p_month+4(2).
    ls_item-gjahr       = p_month(4).
    ls_item-artnr       = it_fi_cogs-artnr.
    ls_item-hzdat       = sy-datum.
    ls_item-budat       = w_budat_t.
    ls_item-bukrs       = c_bukrs.
    ls_item-kokrs       = w_kokrs.

* record type
    ls_item-vrgar       = it_fi_cogs-vrgar.
    perform map_model_r using it_fi_cogs-model ls_item-paph2.
*    ls_item-paph2       = it_fi_cogs-model.
    ls_item-kndnr       = it_fi_cogs-kunnr.
    ls_item-kmland      = it_fi_cogs-kmland.

*    LS_ITEM-SPART       = '10'.
    if it_fi_cogs-paobjnr <> space.
      select single * into corresponding fields of ls_item
         from ce4h201_acct
         where aktbo = 'X'
           and paobjnr = it_fi_cogs-paobjnr.
*          AND PASUBNR = '0001'.
    endif.

    call function 'KEDR_COPA_DERIVE'
      exporting
        i_erkrs                        = 'H201'
        i_item                         = ls_item
*       I_TAB_EXCEPTIONS               =
        i_derivation_date              = sy-datum
*       I_DERIVE_ANYWAY                =
*       I_TRACE_MODE                   = ' '
        i_tabname                      = 'CE0H201'
*       I_GLOBAL_FIELDS                =
        i_mass_processing              = 'X'
*       I_CHECK_CHIER_ONLY             =
      importing
        e_item                         = ls_item_e
*       E_TRACE_HANDLE                 =
*       E_TAB_FIELDS_MODIFIED          =
*       E_TAB_USED_SOURCE_FIELDS       =
      exceptions
        derivation_failed              = 1
        check_chier_failed             = 2
        check_values_failed            = 3
        others                         = 4
              .
    if sy-subrc eq 0.
*    Read the characteritics
      it_fi_cogs-artnr     = ls_item_e-artnr.
      perform model_mapping_fi using ls_item_e-paph2 l_model.
      it_fi_cogs-model     = l_model.
      it_fi_cogs-kunnr     = ls_item_e-kndnr.
      it_fi_cogs-kmland    = ls_item_e-kmland.
      it_fi_cogs-paobjnr   = ls_item-paobjnr.
      it_fi_cogs-pasubnr   = ls_item-pasubnr.
      modify it_fi_cogs.
    endif.

  endloop.

endform.                    " convert_fi_to_pa
*&---------------------------------------------------------------------*
*&      Form  read_sales
*&---------------------------------------------------------------------*
form read_sales.

  select artnr sum( erlos )
   into table i_sales
   from ce1h201
   where paledger = '01'
*     AND versi    = '0'
     and perio    = w_perbl
     and bukrs    = p_bukrs
     and kokrs    = w_kokrs
     and artnr    in s_artnr
   group by artnr.

  delete i_sales where erlos = 0.

endform.                    " read_sales
*&---------------------------------------------------------------------*
*&      Form  derive_chars
*&---------------------------------------------------------------------*
form derive_chars.

* derive the characteriscs for the FI posting
  refresh it_fi_cogs.
  clear   it_fi_cogs.
  loop at it_bsis.
    if it_bsis-dmbtr = 0.
      delete it_bsis.
    else.

      clear: it_fi_cogs.

      it_fi_cogs-hkont = it_bsis-hkont.
      it_fi_cogs-dmbtr = it_bsis-dmbtr.

      perform derive_characteriscs using it_bsis.

      collect it_fi_cogs.
    endif.

  endloop.
endform.                    " derive_chars
*&---------------------------------------------------------------------*
*&      Form  filter_gl_pa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form filter_gl_pa.
  data $ix type i.
  data $valdate like sy-datum.
  concatenate p_month '01' into $valdate.

  loop at it_gl_pa.
    $ix = sy-tabix.
    select single * from cskb
                   where kokrs eq w_kokrs
                     and kstar eq it_gl_pa-hkont
                     and datbi >= $valdate
                     and katyp eq '1'.
    if sy-subrc eq 0.
      delete it_gl_pa index $ix.
    endif.
  endloop.


endform.                    " filter_gl_pa
