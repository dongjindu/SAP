************************************************************************
* Program Name      : ZRPP806R_BOM_RP_COLLECTION
* Author            : Bobby
* Creation Date     : 2004.01.11.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'Sort String Change in BOM .
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 01/06/2005 wskim        UD1K913705   Logic change -issue #20050105-002
* 07/19/2007 iG.mOOn      UD1K941054   Bug fix for gathering due list
************************************************************************
report zrpp806r_bom_rp_collection no standard page heading
                                  line-size  1023  line-count 65
                                  message-id zmpp.

*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
tables: ztpp_nation_def,mara,marc,stpo.
" Nation Table(Inclu. Emission Table)

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
data: it_nation          like table of ztpp_nation_def with header line,
      begin of it_ausp        occurs 0   .
        include structure     ausp  .
data:   nation                like ztpp_nation_def-nation,
      end of it_ausp,
      begin of wa_print       occurs 0       ,
        matnr                 like mara-matnr,
        idnrk(40)             type c         ,
        aennr                 like stpo-aennr,
        vspvb                 like marc-vspvb,
        sortf                 like stpo-sortf,
        cdate                 like sy-datum  ,
        rdate                 like sy-datum  ,
      end of wa_print         .
data : it_plpo like plpo.
*----------------------------------------------------------------------*
* Working AREA
*----------------------------------------------------------------------*
data: wa_atinn                 like ausp-atinn,
      wa_atflv                 like ausp-atflv,
      wa_worder                like ausp-atinn,
      wa_emission              like ausp-atinn,
      wa_size                  type i         ,
      wa_bom                   type c         ,
*Issue # 20050105-002 requested by  choi
*chaned by wskim , on 0106/2005
*-----start
*      wa_s18                   TYPE c         ,
*      wa_mat                   TYPE c  VALUE 'X',
      wa_error                 type c         ,   " Error Flag.
      l_sort                   like stpo-sortf.
*-----end
data: ok_code                  like sy-ucomm  ,
      sv_code                  like sy-ucomm  .
data : c_mode type c value 'N'.
*----------------------------------------------------------------------*
* BDC Variables Definition
*----------------------------------------------------------------------*
data: begin of it_bdc occurs 0.
        include structure bdcdata.
data: end of it_bdc.

*&---------------------------------------------------------------------*
* Constants Variables Definition.
*&---------------------------------------------------------------------*
constants: c_tcode(4)         value 'CS02' .

**----------------------------------------------------------------------
* SELECTION-SCREEN
*----------------------------------------------------------------------
selection-screen begin of block b1 with frame title text-000.
selection-screen begin of block taxha with frame title text-555.
*SELECTION-SCREEN BEGIN OF LINE.
parameter wa_mat radiobutton group tati.
select-options : r_bdate for stpo-datuv,
                 r_matnr for mara-matnr.

parameter :wa_s18 radiobutton group tati,
           wa_int  radiobutton group tati.
select-options : r_matall for mara-matnr.

*SELECTION-SCREEN END OF LINE.
selection-screen end of block taxha.

selection-screen end of block b1.

*&---------------------------------------------------------------------*
start-of-selection.
*&---------------------------------------------------------------------*
*Issue # 20050105-002 requested by  choi
*chaned by wskim , on 0106/2005
*-----start
*  CALL SCREEN 9000             .
  perform get_data.
*-----END
end-of-selection.
  perform write_list.
*  PERFORM check_option         .
*  PERFORM set_atinn            .
  perform update_process       .
*  PERFORM display_process      .
*&---------------------------------------------------------------------*
*&      Form  CHECK_OPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_option   .
  perform read_atinn   using  'P_VM_DATE'         wa_atinn   .
  perform read_atinn   using  'P_SEQUENCE_DATE'   wa_atinn   .
endform.                       " ENDFORM

*&---------------------------------------------------------------------*
*&      Form  SET_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_atinn      .
  perform read_atinn   using  'P_WORK_ORDER'      wa_worder  .
  perform read_atinn   using  'P_EMISSION'        wa_emission.
endform.                       " SET_ATINN

*&---------------------------------------------------------------------*
*&      Form  UPDATE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_process.
  if wa_error = 'X'.
    call function 'BAPI_TRANSACTION_ROLLBACK' .
  else.
    call function 'BAPI_TRANSACTION_COMMIT'
         exporting
              wait = 'X'.
  endif.
endform.                    " UPDATE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_process.
endform.                    " DISPLAY_PROCESS

*&---------------------------------------------------------------------*
*&      Form  READ_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0113   text
*      -->P_L_WO  text
*----------------------------------------------------------------------*
form read_atinn using    pa_char  pa_atinn .
  select single atinn  into pa_atinn
    from cabn
   where atnam = pa_char.
endform.                    " READ_ATINN

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9000 output.
  set pf-status 'PF9000' .
  set titlebar 'ZRPP806R'.
endmodule.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SCREEN_MODIFY_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module screen_modify_9000 output.
endmodule.                 " SCREEN_MODIFY_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit input.
  case ok_code.
    when 'BACK' or 'EXIT' or 'CANC' .
      leave program.
  endcase.
endmodule.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9000 input.
  sv_code = ok_code.
  clear: ok_code.
  case sv_code.
    when 'BACK'.
      leave to screen 0.
    when 'SHOT'.
      case 'X' .
        when wa_bom.
          perform check_duration using 'BOM'.
          check wa_error is initial.
          perform run_bom_process.
          perform write_result   using 'B'  .
        when wa_s18.
          perform run_str_process.
          perform write_result   using 'S'  .
        when wa_mat.
          perform check_duration using 'MAT'.
          check wa_error is initial.
          perform run_mat_process.
          perform write_result   using 'M'  .
      endcase  .
  endcase.
endmodule.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Form  RUN_BOM_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form run_bom_process.
  data: lt_stb       like table of stpox               with header line,
        lt_mat       like table of cscmat              with header line,
        lt_top       like table of stpov               with header line,
        lt_stpov     like table of stpov               with header line,
        lw_marc      like marc       ,
        l_skip       type c          ,
        l_vspvb      like marc-vspvb ,
        begin of lt_mast        occurs 0 .
          include structure     mast     .
  data:   aennr      like stpo-aennr     ,
        end of lt_mast.

  "1. Read the active BOM..(Using the MAST, STPO..)
  clear: wa_print, wa_print[].
  select * into corresponding fields of table lt_mast
    from mast as m inner join stpo as s
      on m~stlnr = s~stlnr
   where m~werks = 'P001'
     and m~stlan = '1'
     and s~stlty = 'M'
     and s~lkenz = space
     and ( s~aedat in r_bdate or s~andat in r_bdate ).

  "2. Loop the BOM and Choice the current alive material and the current
  " available item and sort-string
  delete adjacent duplicates from lt_mast comparing matnr.

  loop at lt_mast.
    call function 'CS_BOM_EXPL_MAT_V2'
      exporting
        capid                       = 'PP01'
        datuv                       = sy-datum
        emeng                       = 1
        mmory                       = ' '
        mehrs                       = ' '
        mtnrv                       = lt_mast-matnr
*        AENNR                       = LT_STPOV-AENNR
        stlal                       = lt_mast-stlal
        stlan                       = '1'
        werks                       = 'P001'
      tables
        stb                         = lt_stb
        matcat                      = lt_mat
      exceptions
        alt_not_found               = 1
        call_invalid                = 2
        material_not_found          = 3
        missing_authorization       = 4
        no_bom_found                = 5
        no_plant_data               = 6
        no_suitable_bom_found       = 7
        conversion_error            = 8
        others                      = 9 .

    if sy-subrc = 0.
      " Check the Valid Duration..
      loop at lt_stb where datub >= sy-datum and datuv <= sy-datum  and
                         ( andat in r_bdate  or  aedat in r_bdate ) and
                ( mtart = 'ROH' or mtart = 'ROH1' or mtart = 'HALB' ).
        clear: l_skip.
        select single * into corresponding fields of lw_marc
          from marc
         where matnr = lt_stb-idnrk
           and werks = 'P001'
           and lvorm = space        .

        check sy-subrc = 0          .
        if lt_stb-mtart = 'HALB'.
          if lw_marc-beskz = 'E' .
          else.
            l_skip = 'X'.
          endif.
        endif.

        check l_skip = space        .
        l_vspvb   = lw_marc-vspvb.
        wa_print-matnr = lt_mast-matnr .
        wa_print-idnrk = lt_stb-idnrk  .
        wa_print-aennr = lt_stb-aennr  .
        wa_print-cdate = lt_stb-andat  .
        wa_print-rdate = lt_stb-aedat.
*        PERFORM read_sortsting  USING  l_vspvb.
        perform read_sortsting  using  l_vspvb
                                       lt_stb-idnrk lt_stb-werks
                                changing l_sort.

        wa_print-sortf = l_vspvb       .
        append wa_print.
        if l_vspvb ne lt_stb-sortf  .
*          " 1. Find the BOM's Top Material...
*          CLEAR: lt_stpov,   lt_top ,  lt_stpov[], lt_top[].
*
*          CALL FUNCTION 'Z_FBM_FIND_TOP_BOM'
*               EXPORTING
*                    pa_matnr       = lt_stb-idnrk(18)
*                    pa_datum       = sy-datum
*               TABLES
*                    t_upper        = lt_stpov
*                    t_top          = lt_top
*               EXCEPTIONS
*                    mat_not_found  = 1
*                    bom_not_found  = 2
*                    invalid_period = 3
*                    no_selection   = 4
*                    OTHERS         = 5.

*          " 2. Update the BOM's Sort String Value..
*          LOOP AT lt_stpov WHERE level = 1 .
          perform bdc_cs02     using lt_stb lt_mast-matnr l_vspvb.
*          ENDLOOP.
        endif.
      endloop.
    else.
      " Write Error Message...
    endif.
  endloop.
endform.                    " RUN_BOM_PROCESS

*&---------------------------------------------------------------------*
*&      Form  RUN_STR_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form run_str_process.
  data: lt_mast      like table of mast                with header line,
        lt_stb       like table of stpox               with header line,
        lt_mat       like table of cscmat              with header line,
        l_vspvb      like marc-vspvb .

  "1. Read the active BOM..(Using the MAST, STPO..)
  clear: wa_print, wa_print[].
  select * into table lt_mast
    from mast
   where werks = 'P001'
     and stlan = '1'.
*Issue # 20050105-002 requested by  choi
*chaned by wskim , on 0106/2005
*-----Start
*     AND stlal = '01'.
*-----End
  "2. Loop the BOM and Choice the current alive material and the current
  " available item and sort-string
  loop at lt_mast.
    call function 'CS_BOM_EXPL_MAT_V2'
      exporting
        capid                       = 'PP01'
        datuv                       = sy-datum
        emeng                       = 1
        mmory                       = ' '
        mtnrv                       = lt_mast-matnr
*        AENNR                       = LT_STPOV-AENNR
        stlal                       = lt_mast-stlal
        stlan                       = '1'
        werks                       = 'P001'
      tables
        stb                         = lt_stb
        matcat                      = lt_mat
      exceptions
        alt_not_found               = 1
        call_invalid                = 2
        material_not_found          = 3
        missing_authorization       = 4
        no_bom_found                = 5
        no_plant_data               = 6
        no_suitable_bom_found       = 7
        conversion_error            = 8
        others                      = 9 .

    if sy-subrc = 0.
      loop at lt_stb where sortf = '17' .
        select single vspvb into l_vspvb
          from marc
         where matnr = lt_stb-idnrk
           and werks = lt_stb-werks
           and lvorm = space       .
        if sy-subrc = 0 .
*Issue # 20050105-002 requested by  choi
*chaned by wskim , on 0106/2005
*-----Start
*          PERFORM read_sortsting  USING  l_vspvb .
          perform read_sortsting  using  l_vspvb
                                         lt_stb-idnrk lt_stb-werks
                                  changing l_sort.
*-----End
          if l_sort ne '17'.
            perform bdc_cs02    using lt_stb lt_mast-matnr l_sort.
            wa_print-matnr = lt_mast-matnr .
            wa_print-idnrk = lt_stb-idnrk  .
            wa_print-aennr = lt_stb-aennr  .
            wa_print-vspvb = l_vspvb.
            wa_print-sortf = l_sort.
            wa_print-cdate = lt_stb-andat.
            wa_print-rdate = lt_stb-aedat.
            append wa_print.
          endif.
        else.
        endif.
      endloop.
    else.
      " Write Error Message...
    endif.
  endloop.
endform.                    " RUN_STR_PROCESS

*&---------------------------------------------------------------------*
*&      Form  RUN_MAT_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form run_mat_process.
  " Data Select.. ROH, ROH1, HALB..
  data: l_count          type i             ,   " Count of Data..
        l_string         like zvpp_rp1-usr01,
        lw_marc          like marc          ,
        lt_mara          like table of mara with header line,
        l_changenr       like cdhdr-changenr,
        l_value_new      like cdpos-value_new.

* UD1K941054 by mOOn 07/19/2007
* {
  data $lt_mara like lt_mara occurs 0 with header line.
* }

*Issue # 20050105-002 requested by  choi
*chaned by wskim , on 0106/2005
*----Start
  if r_matnr-low <> space.
    loop at r_matnr.
      select single * from mara where matnr = r_matnr-low.
      case mara-mtart.
        when 'HALB'.
*" 1. HALB & Procurement Type = 'E'
          clear: wa_print, wa_print[].
          select * into corresponding fields of table lt_mara
            from mara as m inner join cdhdr as c
              on m~matnr = c~objectid
           where c~objectclas = 'MATERIAL'
             and c~udate in r_bdate
             and m~mtart = 'HALB'
             and m~matnr = r_matnr-low
             and m~lvorm ne 'X' .

          loop at lt_mara.
            select single * into lw_marc
              from marc
             where matnr = lt_mara-matnr
               and beskz = 'E'          .
            if sy-subrc = 0.
              continue.
            else.
              delete lt_mara.
            endif.
          endloop.

* 2. ROH Material..
        when 'ROH'.
          select * appending corresponding fields of table lt_mara
            from mara as m inner join cdhdr as c
              on m~matnr = c~objectid
           where c~objectclas = 'MATERIAL'
             and c~udate in r_bdate
             and m~mtart = 'ROH'
             and m~matnr = r_matnr-low
             and m~lvorm ne 'X' .

* 3. ROH1 Material
        when 'ROH1'.
          select * appending corresponding fields of table lt_mara
            from mara as m inner join cdhdr as c
              on m~matnr = c~objectid
           where c~objectclas = 'MATERIAL'
             and c~udate in r_bdate
             and m~mtart = 'ROH1'
             and m~matnr = r_matnr-low
             and m~lvorm ne 'X' .
      endcase.
    endloop.
  else.
    " 1. HALB & Procurement Type = 'E'
    clear: wa_print, wa_print[].
    select * into corresponding fields of table lt_mara
      from mara as m inner join cdhdr as c
        on m~matnr = c~objectid
     where c~objectclas = 'MATERIAL'
       and c~udate in r_bdate
       and m~mtart = 'HALB'
       and m~lvorm ne 'X' .

    loop at lt_mara.
      select single * into lw_marc
        from marc
       where matnr = lt_mara-matnr
         and beskz = 'E'          .
      if sy-subrc = 0.
        continue.
      else.
        delete lt_mara.
      endif.
    endloop.

    " 2. ROH Material..
    select * appending corresponding fields of table lt_mara
      from mara as m inner join cdhdr as c
        on m~matnr = c~objectid
     where c~objectclas = 'MATERIAL'
       and c~udate in r_bdate
       and m~mtart = 'ROH'
       and m~lvorm ne 'X' .

    " 3. ROH1 Material
    select * appending corresponding fields of table lt_mara
      from mara as m inner join cdhdr as c
        on m~matnr = c~objectid
     where c~objectclas = 'MATERIAL'
       and c~udate in r_bdate
       and m~mtart = 'ROH1'
       and m~lvorm ne 'X' .
  endif.

*-----End

* UD1K941054 by mOOn 07/19/2007
* {
*  loop at lt_mara.
*    clear: l_value_new, l_changenr.
*    select single changenr into l_changenr
*         from cdhdr
*         where objectclas = 'MATERIAL'
*           and objectid = lt_mara-matnr
*           and udate in r_bdate.
*    select single value_new into l_value_new
*         from cdpos
*         where objectclas = 'MATERIAL'
*           and objectid = lt_mara-matnr
*           and changenr =  l_changenr
*           and tabname = 'MARC'
*           and fname = 'VSPVB'.
*    if sy-subrc ne 0.
*      delete lt_mara.
*    endif.
*  endloop.

*  sort lt_mara by matnr.
*  delete adjacent duplicates from lt_mara comparing matnr.

  loop at lt_mara.
    clear: l_value_new, l_changenr.

*  check all change item from cdhdr & cdpos
* {
    select changenr into l_changenr
         from cdhdr
         where objectclas = 'MATERIAL'
           and objectid = lt_mara-matnr
           and udate in r_bdate.
      select single value_new into l_value_new
           from cdpos
           where objectclas = 'MATERIAL'
             and objectid = lt_mara-matnr
             and changenr =  l_changenr
             and tabname = 'MARC'
             and fname = 'VSPVB'.
      if sy-subrc eq 0.
        move lt_mara to $lt_mara.
        append $lt_mara.clear $lt_mara.
      endif.
    endselect.
*  }

  endloop.

  sort $lt_mara by matnr.
  delete adjacent duplicates from $lt_mara comparing matnr.

  refresh lt_mara.clear lt_mara.
  lt_mara[] = $lt_mara[].

* }

  describe table lt_mara  lines l_count.
  if l_count = 0.
    message s001  with text-801 .
    exit.
  endif.

  loop at lt_mara.
    " Read the Default Supply Area in Material Master..(MARC-VSPVB)
    clear: lw_marc.
    select single * into corresponding fields of lw_marc
      from marc
     where matnr = lt_mara-matnr
       and werks = 'P001'       .
*Issue # 20050105-002 requested by  choi
*chaned by wskim , on 0106/2005
*-----Start
*    IF lw_marc-vspvb IS INITIAL .
*      l_string = '18'      .
*      wa_print-vspvb = lw_marc-vspvb .
*      wa_print-sortf = l_string.
*      wa_print-matnr = lt_mara-matnr .
*      PERFORM get_text_mat   USING  lt_mara-matnr  wa_print-idnrk .
*      PERFORM get_rdate_mat  USING  lt_mara-matnr  wa_print-rdate .
*      wa_print-cdate = lt_mara-ersda .
*      APPEND wa_print.
*    ELSE.
    l_string = lw_marc-vspvb.
*      PERFORM read_sortsting  USING  l_string .
    perform read_sortsting  using  l_string
                              lt_mara-matnr 'P001'
                             changing l_sort.
    wa_print-vspvb = lw_marc-vspvb .
    wa_print-sortf = l_sort.
    wa_print-matnr = lt_mara-matnr .
    perform get_text_mat   using  lt_mara-matnr  wa_print-idnrk .
    perform get_rdate_mat  using  lt_mara-matnr  wa_print-rdate .
    wa_print-cdate = lt_mara-ersda .
    append wa_print.
*    ENDIF.
*-----End

    perform update_from_mat  using lt_mara-matnr lt_mara-mtart l_sort .
  endloop.
endform.                    " RUN_MAT_PROCESS

*&---------------------------------------------------------------------*
*&      Form  CHECK_DURATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0274   text
*----------------------------------------------------------------------*
form check_duration using   pa_type.
  data: l_days      type    i      .

  clear: wa_error, l_days.

  case pa_type.
    when 'MAT'.
      if r_bdate-low is initial and r_matnr-low is initial.
        message e001 with text-902.
      elseif not r_bdate-low is initial. "AND NOT r_bdate-high IS
        "INITIAL
        l_days = r_bdate-high - r_bdate-low.
        if l_days >= 7.
          message e001 with text-904.
          wa_error = 'X'.
        endif.
      endif.
  endcase.
endform.                    " CHECK_DURATION

*&---------------------------------------------------------------------*
*&      Form  UPDATE_FROM_MAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_MARC_VSPVB  text
*----------------------------------------------------------------------*
form update_from_mat using    pa_matnr  pa_mtart pa_string.
  data: lt_top       like table of stpov               with header line,
        lt_stb       like table of stpox               with header line,
        lt_mat       like table of cscmat              with header line,
        lt_stpov     like table of stpov               with header line.

  " 1. Find the BOM's Top Material...
  clear: lt_stpov,   lt_top ,   lt_stb,   lt_mat,
         lt_stpov[], lt_top[],  lt_stb[], lt_mat[].

  call function 'Z_FBM_FIND_TOP_BOM'
       exporting
            pa_matnr       = pa_matnr
            pa_datum       = sy-datum
            pa_mehrs       = ' '
       tables
            t_upper        = lt_stpov
            t_top          = lt_top
       exceptions
            mat_not_found  = 1
            bom_not_found  = 2
            invalid_period = 3
            no_selection   = 4
            others         = 5.

  " 2. Update the BOM's Sort String Value..
*Issue # 20050105-002 requested by  choi
*Chaned by wskim , on 0106/2005
*-----Start
*  DELETE ADJACENT DUPLICATES FROM lt_stpov COMPARING level matnr.
*-----End
  loop at lt_stpov where level = 1 .
    select single mtart mstae into (mara-mtart, mara-mstae)
              from mara
              where matnr = lt_stpov-matnr.
    if mara-mtart = 'FERT' and mara-mstae = '14'.
      continue.
    endif.
    perform bdc_cs02_up     using lt_stpov  pa_mtart pa_string.
  endloop.
endform.                    " UPDATE_FROM_MAT

*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
form dynpro using dynbegin name value.
  if dynbegin = 'X'.
    clear it_bdc.
    move: name to it_bdc-program,
          value to it_bdc-dynpro,
          dynbegin to it_bdc-dynbegin.
    append it_bdc.
  else.
    clear it_bdc.
    move: name to it_bdc-fnam,
          value to it_bdc-fval.
    append it_bdc.
  endif.
endform.

*&---------------------------------------------------------------------*
*&      Form  READ_SORTSTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_VSPVB  text
*----------------------------------------------------------------------*
form read_sortsting using    pa_vspvb p_idnrk p_werks
                    changing  pa_sort.
*  SELECT SINGLE usr01 INTO pa_vspvb
*    FROM zvpp_rp1
*   WHERE plnnr = 'RP'
*     AND usr00 = pa_vspvb.
*
*  IF sy-subrc NE 0.
*    " Error Value.. DISPLAY & VALUE IS '18'.
*    pa_vspvb = '18'      .
*  ENDIF.
  select single  *  from mara
          where matnr eq p_idnrk
            and ( mtart eq 'ROH' or mtart eq 'ROH1'
                 or mtart eq 'HALB' ).
  if sy-subrc = 0.
    select single * from marc
         where matnr eq p_idnrk
           and werks eq p_werks.
** Changed on 05/19/2006 by Furong, Requested by Mr. Hong
*    IF ( mara-mtart EQ 'ROH' OR mara-mtart EQ 'ROH1' )
*      AND marc-disgr EQ 'P010'.
*      pa_sort  = '  '.
*    ELSEIF  mara-mtart EQ 'HALB' AND p_idnrk(1) EQ 'B'.
*      pa_sort  = '  '.
*    ELSEIF marc-sobsl = '50' .
*      pa_sort  = ' '.
*    ELSE.
** end of change
    if marc-vspvb ne space.
*reference rate routing
      select single plnkn usr01
           into corresponding fields of it_plpo
                  from plpo
                     where  plnty eq 'M'
                        and plnnr eq 'RP'
                        and werks eq p_werks
                        and usr00 eq marc-vspvb.
* UD1K941049 by IG.MOON 7/18/2007 {
*        pa_sort  = it_plpo-usr01.
      if sy-subrc eq 0.
        pa_sort  = it_plpo-usr01.
      else.
        pa_sort  = '17'.
      endif.
* }
    else.
** Changed on 05/19/2006 by Furong, Requested by Mr. Hong
*        pa_sort  = '18'.
      pa_sort  = '17'.
** end of change
    endif.
*    ENDIF.
  else.
    pa_sort  = ' '.
  endif.
endform.                    " READ_SORTSTING

*&---------------------------------------------------------------------*
*&      Form  BDC_CS02_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_STPOV  text
*----------------------------------------------------------------------*
form bdc_cs02_up using    pa_stpov  pa_mtart pa_string.
  data: lt_stpov       like stpov.
  tables : stko.
  clear : stko,it_bdc, it_bdc[].
  lt_stpov = pa_stpov.

  select single * from stko
    where stlty eq 'M'
      and stlnr eq lt_stpov-stlnr
      and stkoz eq lt_stpov-stkoz.

  perform dynpro using:
          'X' 'SAPLCSDI'        '0100',
          ' ' 'BDC_OKCODE'      '=FCPU',
          ' ' 'RC29N-MATNR'     lt_stpov-matnr,
          ' ' 'RC29N-WERKS'     lt_stpov-werks,
          ' ' 'RC29N-STLAN'     lt_stpov-stlan,
          ' ' 'RC29N-STLAL'     stko-stlal.
*  IF wa_int <> 'X'.
  perform dynpro using:
     ' ' 'RC29N-AENNR'     lt_stpov-aennr.
*  ENDIF.
  perform dynpro using:
           'X' 'SAPLCSDI'        '0150',
           ' ' 'BDC_OKCODE'      '=SETP',

           'X' 'SAPLCSDI'        '0708',
           ' ' 'BDC_OKCODE'      '=CLWI',
           ' ' 'RC29P-SELPO'     lt_stpov-posnr,
           ' ' 'RC29P-SELID'     lt_stpov-idnrk,
*          ' ' 'RC29P-SELPI'     lt_stpov-stlkn,
           ' ' 'RC29P-SELSB'     ' '           ,

           'X' 'SAPLCSDI'        '0150',
           ' ' 'RC29P-AUSKZ(01)' 'X'        ,
           ' ' 'RC29P-SORTF(01)' pa_string  ,
           ' ' 'BDC_OKCODE'      '=FCBU',

           'X' 'SAPLCSDI'        '0130',
           ' ' 'BDC_OKCODE'      '/00'  ,

           'X' 'SAPLCSDI'        '0131',
           ' ' 'BDC_OKCODE'      '/00'  ,

           'X' 'SAPLCSDI'        '0138',
           ' ' 'BDC_OKCODE'      '/00'  .

  call transaction c_tcode  using it_bdc  mode c_mode  update 'S' .

*ENDIF.
endform.                    " BDC_CS02_UP

*&---------------------------------------------------------------------*
*&      Form  BDC_CS02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB  text
*      -->P_LT_MAST_MATNR  text
*----------------------------------------------------------------------*
form bdc_cs02 using    pa_stb  pa_matnr pa_string.
  data: lt_stpov       like stpox.

  lt_stpov = pa_stb  .         clear: it_bdc, it_bdc[].

  perform dynpro using:
          'X' 'SAPLCSDI'        '0100',
          ' ' 'BDC_OKCODE'      '=FCPU',
          ' ' 'RC29N-MATNR'     pa_matnr,
          ' ' 'RC29N-WERKS'     lt_stpov-werks,
          ' ' 'RC29N-STLAN'     lt_stpov-stlan,
          ' ' 'RC29N-STLAL'     '1'           ,
          ' ' 'RC29N-AENNR'     lt_stpov-aennr,

          'X' 'SAPLCSDI'        '0150',
          ' ' 'BDC_OKCODE'      '=SETP',

          'X' 'SAPLCSDI'        '0708',
          ' ' 'BDC_OKCODE'      '=CLWI',
          ' ' 'RC29P-SELPO'     lt_stpov-posnr,
          ' ' 'RC29P-SELID'     lt_stpov-idnrk,
*          ' ' 'RC29P-SELPI'     lt_stpov-stlkn,
          ' ' 'RC29P-SELSB'     ' '           ,

          'X' 'SAPLCSDI'        '0150',
          ' ' 'RC29P-AUSKZ(01)' 'X'        ,
          ' ' 'RC29P-SORTF(01)' pa_string  ,
          ' ' 'BDC_OKCODE'      '=FCBU',

          'X' 'SAPLCSDI'        '0130',
          ' ' 'BDC_OKCODE'      '/00'  ,

          'X' 'SAPLCSDI'        '0131',
          ' ' 'BDC_OKCODE'      '/00'  ,

          'X' 'SAPLCSDI'        '0138',
          ' ' 'BDC_OKCODE'      '/00'  .

  call transaction c_tcode  using it_bdc  mode c_mode  update 'S' .
endform.                                                    " BDC_CS02

*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0288   text
*----------------------------------------------------------------------*
form write_result using    pa_type .
  clear: wa_size.

  delete adjacent duplicates from wa_print.
  describe table wa_print  lines wa_size.
  check wa_size > 0.

  call screen 5000.
endform.                    " WRITE_RESULT

*&---------------------------------------------------------------------*
*&      Module  CALL_LIST  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module call_list output.
  data: l_text(50)         type c  .
  set pf-status space.
  case 'X'    .
    when wa_bom.  l_text = text-100.
    when wa_s18.  l_text = text-200.
    when wa_mat.  l_text = text-300.
  endcase.

  " Headline Display...
  leave to list-processing and return to screen 0.

  suppress dialog.

  write at: /001(55)  l_text,
             075(10)  wa_size.

  skip 3.

  if wa_mat = 'X'.
    write at: /001(20)  text-081,
               021(40)  text-082,
               061(20)  text-083,
               081(20)  text-094,
               101(20)  text-095.
  else.
    write at: /001(20)  text-090,
               021(20)  text-091,
               041(20)  text-092,
               061(20)  text-093,
               081(20)  text-094,
               101(20)  text-095.
  endif.
  uline at /(121).
  skip 1.

  loop at wa_print.
    if wa_mat = 'X'.
      write at: /001(20)  wa_print-matnr,
                 021(20)  wa_print-idnrk,
                 061(20)  wa_print-vspvb,
                 081(20)  wa_print-cdate,
                 101(20)  wa_print-rdate.
    else.
      write at: /001(20)  wa_print-matnr,
                 021(20)  wa_print-idnrk,
                 041(20)  wa_print-aennr,
                 061(20)  wa_print-sortf,
                 081(20)  wa_print-cdate,
                 101(20)  wa_print-rdate.
    endif.
  endloop.
endmodule.                 " CALL_LIST  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  get_text_mat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MARA_MATNR  text
*      -->P_WA_PRINT_IDNRK  text
*----------------------------------------------------------------------*
form get_text_mat using    pa_matnr  pa_maktx.
  select single maktx into pa_maktx
    from makt
   where matnr = pa_matnr
     and spras = sy-langu.
endform.                    " get_text_mat

*&---------------------------------------------------------------------*
*&      Form  get_rdate_mat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MARA_MATNR  text
*      -->P_WA_PRINT_RDATE  text
*----------------------------------------------------------------------*
form get_rdate_mat using    pa_matnr  pa_udate.
  select single udate into pa_udate
    from cdhdr
   where objectclas = 'MATERIAL'
     and objectid   = pa_matnr  .
endform.                    " get_rdate_mat
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data.
  if wa_s18 eq 'X'.
    perform run_str_process.
*    PERFORM write_result   USING 'S'  .
  elseif wa_mat eq 'X'.
    perform check_duration using 'MAT'.
    check wa_error is initial.
    perform run_mat_process.
*    PERFORM write_result   USING 'M'  .
  elseif wa_int eq 'X'.
    perform run_mat_all_process.
  endif.

endform.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  WRITE_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_list.
  data: l_text(50)         type c  .
  case 'X'    .
    when wa_bom.  l_text = text-100.
    when wa_s18.  l_text = text-200.
    when wa_mat.  l_text = text-300.
  endcase.

  " Headline Display...

  write at: /001(55)  l_text,
             075(10)  wa_size.

  skip 3.

  if wa_mat = 'X'.
    write at: /001(20)  text-081,
               021(40)  text-082,
               061(20)  text-083,
               082(20)  text-093,
               103(20)  text-095.
  else.
    write at: /001(20)  text-090,
               021(20)  text-091,
               041(20)  text-092,
               061(4)   text-083,
               066(20)  text-093,
               087(20)  text-094,
               108(20)  text-095.
  endif.
  uline at /(121).
  skip 1.

  loop at wa_print.
    if wa_mat = 'X'.
      write at: /001(20)  wa_print-matnr,
                 021(20)  wa_print-idnrk,
                 061(20)  wa_print-vspvb,
                 082(20)  wa_print-sortf,
                 101(20)  wa_print-cdate.
    else.
      write at: /001(20)  wa_print-matnr,
                 021(20)  wa_print-idnrk,
                 041(20)  wa_print-aennr,
                 061(4)   wa_print-vspvb,
                 066(20)  wa_print-sortf,
                 087(20)  wa_print-cdate,
                 108(20)  wa_print-rdate.
    endif.
  endloop.
endform.                    " WRITE_LIST
*&---------------------------------------------------------------------*
*&      Form  RUN_MAT_ALL_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form run_mat_all_process.
  " Data Select.. ROH, ROH1, HALB..
  data: l_count          type i             ,   " Count of Data..
        l_string         like zvpp_rp1-usr01,
        lw_marc          like marc          ,
        lt_mara          like table of mara            with header line.

  if r_matall-low <> space.
    loop at r_matall.
      select single * from mara where matnr = r_matall-low.
      case mara-mtart.
        when 'HALB'.
*" 1. HALB & Procurement Type = 'E'
          clear: wa_print, wa_print[].
          select * into corresponding fields of table lt_mara
            from mara
             where mtart = 'HALB'
             and   matnr = r_matall-low
             and   lvorm ne 'X' .

          loop at lt_mara.
            select single * into lw_marc
              from marc
             where matnr = lt_mara-matnr
               and beskz = 'E'          .
            if sy-subrc = 0.
              continue.
            else.
              delete lt_mara.
            endif.
          endloop.
* 2. ROH Material..
        when 'ROH'.
          select * appending corresponding fields of table lt_mara
            from mara
           where mtart = 'ROH'
             and matnr = r_matall-low
             and lvorm ne 'X' .

* 3. ROH1 Material
        when 'ROH1'.
          select * appending corresponding fields of table lt_mara
            from mara
           where mtart = 'ROH1'
             and matnr = r_matall-low
             and lvorm ne 'X' .
      endcase.
    endloop.
  endif.

  sort lt_mara by matnr.
  delete adjacent duplicates from lt_mara comparing matnr.

  describe table lt_mara  lines l_count.
  if l_count = 0.
    message s001  with text-801 .
    exit.
  endif.

  loop at lt_mara.
    " Read the Default Supply Area in Material Master..(MARC-VSPVB)
    clear: lw_marc.
    select single * into corresponding fields of lw_marc
      from marc
     where matnr = lt_mara-matnr
       and werks = 'P001'       .

    l_string = lw_marc-vspvb.
*      PERFORM read_sortsting  USING  l_string .
    perform read_sortsting  using  l_string
                              lt_mara-matnr 'P001'
                             changing l_sort.
    wa_print-vspvb = lw_marc-vspvb .
    wa_print-sortf = l_sort.
    wa_print-matnr = lt_mara-matnr .
    perform get_text_mat   using  lt_mara-matnr  wa_print-idnrk .
    perform get_rdate_mat  using  lt_mara-matnr  wa_print-rdate .
    wa_print-cdate = lt_mara-ersda .
    append wa_print.
*Issue # 20050105-002 requested by  choi
*Chaned by wskim , on 0106/2005
*-----Start
    perform update_from_mat_all using lt_mara-matnr
                                      lt_mara-mtart l_sort .
*-----End
  endloop.

endform.                    " RUN_MAT_ALL_PROCESS
*&---------------------------------------------------------------------*
*&      Form  update_from_mat_all
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MARA_MATNR  text
*      -->P_L_SORT  text
*----------------------------------------------------------------------*
form update_from_mat_all using    pa_matnr pa_mtart pa_string.
  data: lt_top       like table of stpov               with header line,
        lt_stb       like table of stpox               with header line,
        lt_mat       like table of cscmat              with header line,
        lt_stpov     like table of stpov               with header line.

  " 1. Find the BOM's Top Material...
  clear: lt_stpov,   lt_top ,   lt_stb,   lt_mat,
         lt_stpov[], lt_top[],  lt_stb[], lt_mat[].


  call function 'Z_FBM_FIND_TOP_BOM_01'
       exporting
            pa_matnr       = pa_matnr
*            pa_datum       = sy-datum
            pa_mehrs       = ' '
       tables
            t_upper        = lt_stpov
            t_top          = lt_top
       exceptions
            mat_not_found  = 1
            bom_not_found  = 2
            invalid_period = 3
            no_selection   = 4
            others         = 5.

  " 2. Update the BOM's Sort String Value..
*  DELETE ADJACENT DUPLICATES FROM lt_stpov COMPARING level matnr.
  loop at lt_stpov where level = 1 .
    perform bdc_cs02_up  using lt_stpov pa_mtart pa_string.
  endloop.

endform.                    " update_from_mat_all
