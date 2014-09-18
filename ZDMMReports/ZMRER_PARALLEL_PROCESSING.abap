* REPORT ZMRER_PARALLEL_PROCESSING .
* This report is for improving the performance of MRER thru parallel
* processing concept. The basis is dividing the total no of purchase
* orders into equal no of packets containing several POs
* and passing each packet to a function module which creates a
* job running the MRER program for that pack.
* Several jobs equal to the no of packets are run in background
* at the same time and preformance is improved.

* Suitable for settlement per PO or PO item.
* Per vendor settlement will create multiple invoices.

INCLUDE R1MRRTOP                                .
**********************************************
* Data declaration
**********************************************

DATA: LT_EKRS     LIKE EKRS     OCCURS 0,
      CT_EKRS     LIKE EKRS     OCCURS 0.

Data: Begin of IT_STRUC occurs 0 ,
      Ebeln like ekpo-ebeln,
      Ebelp like ekpo-ebelp,
      gjahr like ekbe-gjahr,
      belnr like ekbe-belnr,
      buzei like ekbe-buzei,
      lfgja like ekbe-lfgja,
      lfbnr like ekbe-lfbnr,
      lfpos like ekbe-lfpos,
      end of it_struc.

Data :sum type i,
      lin like sum,
      sum2 like sum,
      lin2 like sum,
      ebeln_ref like ekpo-ebeln,
      ref value 'X'.

data: task like sy-subrc, taskname(24),
      Group like RZLLITAB-CLASSNAME value ' ',
      WP_FREE type i.

Data: begin of it_pass occurs 0,
        ebeln like ekpo-ebeln,
        sum like sum,
      end of it_pass,
      begin of it_pass2 occurs 0,
        lfbnr like ekrs-lfbnr,
        sum like sum,
      end of it_pass2.

data: begin of it_ekpo_pass occurs 0.
        include structure rsparams.
data: end of it_ekpo_pass.

data : it_submit_pass like it_ekpo_pass occurs 0 with header line.
data : rt_submit_pass like it_ekpo_pass occurs 0 with header line.
data : MSG_TEXT(80), MSG(80).
data : o_send_task like sy-subrc, o_return_task like sy-subrc.
data : wait_time type i.
data : job type i,
       job_no type TBTCJOB-JOBNAME value 'MRERJOB_000'.
**** No of jobs desired
Parameters : Jobs type i default 5.
Parameters : Job_wait type i default 100.
*---------------------------------------------------------------------**
* Initialization.
*---------------------------------------------------------------------**
INITIALIZATION.
  perform report_init.

*---------------------------------------------------------------------**
* at selection-screen output.
*---------------------------------------------------------------------**
AT SELECTION-SCREEN OUTPUT.
* get text for type of document selection
  PERFORM GET_ERSBA_TEXT USING    PA_ERSBA
                         CHANGING ERSBA_TX.
* F4-Help
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_ERSBA.
  PERFORM F4_ERSBA.                    " changing ersba.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_VARIA.
  PERFORM F4_VARIA CHANGING PA_VARIA.
* PAI
AT SELECTION-SCREEN.
* determine layout variant for ALV list
  PERFORM VARIANT_DETERMINE.
* get document type for hsc document from table T169F
  PERFORM determine_doctype USING TCODE_mrer.

*************************************************+
*INcludes
*************************************************
  include R1MRRF04.
  include R1MRRF03.
  include R1MRRF01.
  include R1MRRF36.

****************************************************
* Start of Selection
******************************************************
Start-of-selection.

  PERFORM READ_EKRS_PP TABLES  CT_EKRS.

*parallel processing
  PERFORM PARALLEL_PROCESSING tables ct_ekrs.

**************************************************
*end of selection.
*************************************************+
end-of-selection.

*****
  CALL FUNCTION 'POPUP_TO_INFORM'
       EXPORTING
            TITEL = 'INFORMATION'
            TXT1  = 'PARALLEL PROCESSING IN PROGRESS'
            TXT2  = 'PLEASE WAIT'.
*****
  wait until o_return_task ge o_send_task." up to wait_time seconds.
  Write : MSG_TEXT, MSG.

*&---------------------------------------------------------------------*
*&      Form  READ_EKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EKRS  text
*      -->P_GT_EKKO  text
*      -->P_GT_EKPO  text
*----------------------------------------------------------------------*
FORM READ_EKRS_PP TABLES   CT_EKRS STRUCTURE EKRS.


* 1. access with data from selection screen
  SELECT * FROM EKRS INTO TABLE LT_EKRS
    WHERE  BUDAT IN SO_BUDAT
      AND  LIFNR IN SO_LIFNR
      AND  BELNR IN SO_MBLNR
      AND  GJAHR IN SO_MJAHR
      AND  EBELN IN SO_EBELN
      AND  EBELP IN SO_EBELP
      AND  BUKRS IN SO_BUKRS
      AND  WERKS IN SO_WERKS.
  if sy-subrc ne 0.
    clear sy-subrc.
    exit.
  endif.

* 2. access to find depend material documents
  IF NOT ( LT_EKRS[] IS INITIAL ).
    SELECT * INTO TABLE CT_EKRS FROM EKRS
       FOR ALL ENTRIES IN LT_EKRS
           WHERE lifnr = LT_EKRS-lifnr and
                 ebeln = LT_EKRS-ebeln and
                 ebelp = LT_EKRS-ebelp and
                 LFGJA = LT_EKRS-LFGJA AND
                 LFBNR = LT_EKRS-LFBNR AND
                 LFPOS = LT_EKRS-LFPOS.
  ENDIF.

  Sort lt_ekrs by ebeln ebelp .



ENDFORM.                                                    " READ_EKRS
*&---------------------------------------------------------------------*
*&      Form  PARALLEL_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PARALLEL_PROCESSING tables ct_ekrs structure ekrs.

  data: sum3 type i,
        lf_lfbnr like ekrs-lfbnr,
        max type i value 800.

* initialise SPBT and find the no fo free workprocess available
  CALL FUNCTION 'SPBT_INITIALIZE'
       EXPORTING
            GROUP_NAME                     = Group
       IMPORTING
            FREE_PBT_WPS                   = WP_FREE
       EXCEPTIONS
            INVALID_GROUP_NAME             = 1
            INTERNAL_ERROR                 = 2
            PBT_ENV_ALREADY_INITIALIZED    = 3
            CURRENTLY_NO_RESOURCES_AVAIL   = 4
            NO_PBT_RESOURCES_FOUND         = 5
            CANT_INIT_DIFFERENT_PBT_GROUPS = 6
            OTHERS                         = 7.

  wait_time = wp_free * 60.

  loop at ct_ekrs.
    move-corresponding ct_ekrs to it_struc.
    append it_struc.
  endloop.

  If wp_free ge jobs.
    wp_free = jobs.
  else.
* wpfree = wpfree.
    message e083(me) with 'Jobs less than' wp_free.
  endif.
  describe table it_struc lines lin.
  lin2 = lin / WP_FREE.

  if pa_ersba = '4'.
    sort it_struc by lfgja lfbnr gjahr belnr.
    clear sum.
    loop at it_struc.
      sum = sum + 1.
      if it_struc-lfbnr <> lf_lfbnr and ( sum <= lin2 and sum <= max ).
        lf_lfbnr = it_struc-lfbnr.
      endif.
*     IMPORTANT: all lines with the same lfbnr must be in the same job
      if ( sum >= lin2  or sum >= max ) and lf_lfbnr <> it_struc-lfbnr.
        clear sum.
        perform submit_job.
      endif.
      move 'SO_MBLNR'     to it_ekpo_pass-selname.
      move 'S'            to it_ekpo_pass-kind.
      move it_struc-belnr to it_ekpo_pass-low.
      move 'I'            to it_ekpo_pass-sign.
      move 'EQ'           to it_ekpo_pass-option.
      collect it_ekpo_pass.
    endloop.
    if sum > 0.
      perform submit_job.
      clear sum.
    endif.

  else.                                 "if pa_ersba = '4'
    sort it_struc by ebeln ebelp.

    loop at it_struc.
      add 1 to sum.
      at end of ebeln.
        move it_struc-ebeln to it_pass-ebeln.
        it_pass-sum = sum.
        clear sum.
        append it_pass.
      endat.
      clear it_pass.
    endloop.

    clear sum3.
    loop at it_pass.

      if ref = 'X'.
        move it_pass-ebeln to ebeln_ref.
        clear ref.
      endif.

      move 'SO_EBELN' to it_ekpo_pass-selname.
      move 'S' to it_ekpo_pass-kind.
      move ebeln_ref to it_ekpo_pass-low.
      move 'I' to it_ekpo_pass-sign.
      move 'BT' to it_ekpo_pass-option.

      sum2 = sum2 + it_pass-sum.
      sum3 = sum3 + 1.

      if sum2 ge lin2.
        move it_pass-ebeln to it_ekpo_pass-high.
        append it_ekpo_pass.
        write: / 'No. of EKRS entries:', it_ekpo_pass-low(12),
                 it_ekpo_pass-high(12), sum2.
        write: / 'No. of POs:        ', it_ekpo_pass-low(12),
                 it_ekpo_pass-high(12), sum3.
        clear sum3.
        clear sum2.
        move 'X' to ref.
      endif.

    endloop.
*   Check for remaining items.
    If sum2 gt 0.
      move it_pass-ebeln to it_ekpo_pass-high.
      append it_ekpo_pass.
      write: / 'No. of EKRS entries:', it_ekpo_pass-low(12),
               it_ekpo_pass-high(12), sum2.
      write: / 'No. of POs:        ', it_ekpo_pass-low(12),
               it_ekpo_pass-high(12), sum3.
      clear sum3.
      clear sum2.
    endif.
*   fill internal table with no of POs and range

    loop at it_ekpo_pass.

      move it_ekpo_pass-selname to it_submit_pass-selname.
      move it_ekpo_pass-kind to    it_submit_pass-kind.
      move it_ekpo_pass-low to  it_submit_pass-low.
      move it_ekpo_pass-high to  it_submit_pass-high.
      move it_ekpo_pass-sign to        it_submit_pass-sign.
      move it_ekpo_pass-option to       it_submit_pass-option.
      append it_submit_pass.
      clear it_submit_pass.

      if not so_bukrs is initial.
        loop at so_bukrs.
          move 'SO_BUKRS'      to it_submit_pass-selname.
          move 'S' to    it_submit_pass-kind.
          move so_bukrs-low to  it_submit_pass-low.
          move so_bukrs-high to  it_submit_pass-high.
          move so_bukrs-sign to   it_submit_pass-sign.
          move so_bukrs-option  to  it_submit_pass-option.
          append it_submit_pass.
          clear it_submit_pass.
        endloop.
      endif.
      if not so_werks is initial.
        loop at so_werks.
          move 'SO_WERKS'      to it_submit_pass-selname.
          move 'S' to    it_submit_pass-kind.
          move so_werks-low to  it_submit_pass-low.
          move so_werks-high to  it_submit_pass-high.
          move so_werks-sign   to        it_submit_pass-sign.
          move so_werks-option  to       it_submit_pass-option.
          append it_submit_pass.
          clear it_submit_pass.
        endloop.
      endif.
      if not so_budat is initial.
        loop at so_budat.
          move 'SO_BUDAT'      to it_submit_pass-selname.
          move 'S' to    it_submit_pass-kind.
          move so_budat-low to  it_submit_pass-low.
          move so_budat-high to  it_submit_pass-high.
          move so_budat-sign  to        it_submit_pass-sign.
          move so_budat-option  to       it_submit_pass-option.
          append it_submit_pass.
          clear it_submit_pass.
        endloop.
      endif.
      if not so_mblnr is initial.
        loop at so_mblnr.
          move 'SO_MBLNR'      to it_submit_pass-selname.
          move 'S' to    it_submit_pass-kind.
          move so_mblnr-low to  it_submit_pass-low.
          move so_mblnr-high to  it_submit_pass-high.
          move so_mblnr-sign  to        it_submit_pass-sign.
          move so_mblnr-option to       it_submit_pass-option.
          append it_submit_pass.
          clear it_submit_pass.
        endloop.

      endif.

      if not so_mjahr is initial.
        loop at so_mjahr.
          move 'SO_MJAHR'      to it_submit_pass-selname.
          move 'S' to    it_submit_pass-kind.
          move so_mjahr-low to  it_submit_pass-low.
          move so_mjahr-high to  it_submit_pass-high.
          move so_mjahr-sign to        it_submit_pass-sign.
          move so_mjahr-option to       it_submit_pass-option.
          append it_submit_pass.
          clear it_submit_pass.
        endloop.

      endif.
      if not so_lifnr is initial.
        loop at so_lifnr.
          move 'SO_LIFNR'      to it_submit_pass-selname.
          move 'S' to    it_submit_pass-kind.
          move so_lifnr-low to  it_submit_pass-low.
          move so_lifnr-high to  it_submit_pass-high.
          move so_lifnr-sign  to        it_submit_pass-sign.
          move so_lifnr-option to       it_submit_pass-option.
          append it_submit_pass.
          clear it_submit_pass.
        endloop.

      endif.
      if not so_ebelp is initial.
        loop at so_ebelp.
          move 'SO_EBELP'      to it_submit_pass-selname.
          move 'S' to    it_submit_pass-kind.
          move so_ebelp-low to  it_submit_pass-low.
          move so_ebelp-high to  it_submit_pass-high.
          move so_ebelp-sign  to        it_submit_pass-sign.
          move so_ebelp-option  to       it_submit_pass-option.
          append it_submit_pass.
          clear it_submit_pass.
        endloop.

      endif.

      move 'PA_XTEST'      to it_submit_pass-selname.
      move 'P' to    it_submit_pass-kind.
      move pa_xtest to  it_submit_pass-low.
      move 'I'   to        it_submit_pass-sign.
      move 'EQ'  to       it_submit_pass-option.
      append it_submit_pass.
      clear it_submit_pass.

      move 'PA_ERSBA'      to it_submit_pass-selname.
      move 'P' to    it_submit_pass-kind.
      move pa_ersba to  it_submit_pass-low.
      move 'I'   to        it_submit_pass-sign.
      move 'EQ'  to       it_submit_pass-option.
      append it_submit_pass.
      clear it_submit_pass.

      move 'PA_BUDAT'      to it_submit_pass-selname.
      move 'P' to    it_submit_pass-kind.
      move pa_budat to  it_submit_pass-low.
      move 'I'   to        it_submit_pass-sign.
      move 'EQ'  to       it_submit_pass-option.
      append it_submit_pass.
      clear it_submit_pass.

      move 'PA_EMESS'      to it_submit_pass-selname.
      move 'P' to    it_submit_pass-kind.
      move pa_emess to  it_submit_pass-low.
      move 'I'   to        it_submit_pass-sign.
      move 'EQ'  to       it_submit_pass-option.
      append it_submit_pass.
      clear it_submit_pass.

      move 'PA_DELNU'      to it_submit_pass-selname.
      move 'P' to    it_submit_pass-kind.
      move pa_delnu to  it_submit_pass-low.
      move 'I'   to        it_submit_pass-sign.
      move 'EQ'  to       it_submit_pass-option.
      append it_submit_pass.
      clear it_submit_pass.

      move 'PA_NOMES'      to it_submit_pass-selname.
      move 'P' to    it_submit_pass-kind.
      move pa_nomes to  it_submit_pass-low.
      move 'I'   to        it_submit_pass-sign.
      move 'EQ'  to       it_submit_pass-option.
      append it_submit_pass.
      clear it_submit_pass.

      move 'PA_NOVAL'      to it_submit_pass-selname.
      move 'P' to    it_submit_pass-kind.
      move pa_noval to  it_submit_pass-low.
      move 'I'   to        it_submit_pass-sign.
      move 'EQ'  to       it_submit_pass-option.
      append it_submit_pass.
      clear it_submit_pass.

      move 'PA_VARIA'      to it_submit_pass-selname.
      move 'P' to    it_submit_pass-kind.
      move pa_varia to  it_submit_pass-low.
      move 'I'   to        it_submit_pass-sign.
      move 'EQ'  to       it_submit_pass-option.
      append it_submit_pass.
      clear it_submit_pass.

      move 'PA_PRICE'      to it_submit_pass-selname.
      move 'P' to    it_submit_pass-kind.
      move pa_price to  it_submit_pass-low.
      move 'I'   to        it_submit_pass-sign.
      move 'EQ'  to       it_submit_pass-option.
      append it_submit_pass.
      clear it_submit_pass.

      job = job + 1.
      job_no+8(3) = job.
      taskname = job_no.
*
*   call function module for parallel processing
      CALL FUNCTION 'ZMRER_PARALLEL_PROCESSING'
       STARTING NEW TASK taskname DESTINATION IN GROUP Group
       performing get_results on end of task
       exporting
         JOB                  =  job_no
       tables
         IT_SUBMIT_PASS       = it_submit_pass
       EXCEPTIONS
         COMMUNICATION_FAILURE = 1 MESSAGE MSG_TEXT
         SYSTEM_FAILURE        = 2 MESSAGE MSG_TEXT.

      IF SY-SUBRC NE 0.
        WRITE: MSG_TEXT.
      ELSE.
        Wait up to job_wait seconds.
        add 1 to o_send_task.
        WRITE: 'O.K.'.
      ENDIF.


      clear it_submit_pass.
      refresh it_submit_pass.

    endloop.
  endif.

ENDFORM.                    " PARALLEL_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  get_results
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SUBMIT_PASS  text
*      -->P_=  text
*      -->P_IT_SUBMIT_PASS  text
*----------------------------------------------------------------------*
FORM get_results using taskname.

  RECEIVE RESULTS FROM FUNCTION 'ZMRER_PARALLEL_PROCESSING'
       tables
         it_submit_pass          = rt_submit_pass
       EXCEPTIONS
           COMMUNICATION_FAILURE = 1 MESSAGE MSG
           SYSTEM_FAILURE        = 2 MESSAGE MSG.
  If Sy-subrc = 0.
    add 1 to o_return_task.
  else.
    add 1 to o_return_task.
    write : 'ERROR', MSG.
  endif.


ENDFORM.                    " get_results


*----------------------------------------------------------------------*
*   INCLUDE R1MRRF02 : variant handling                                *
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  VARIANT_INIT
*&---------------------------------------------------------------------*
FORM VARIANT_INIT.

  CLEAR GS_VARIANT.
  GS_VARIANT-REPORT = 'RMMR1MRR'.
  GS_VARIANT-HANDLE = c_hlist.

ENDFORM.                               " VARIANT_INIT
*&---------------------------------------------------------------------*
*&      Form  VARIANT_get
*&---------------------------------------------------------------------*
FORM VARIANT_get changing cf_subrc like sy-subrc.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
       EXPORTING
            I_SAVE        = C_signa
       CHANGING
            CS_VARIANT    = GS_VARIANT
       EXCEPTIONS
            wrong_input   = 1
            not_found     = 2
            program_error = 3
            others        = 4.
  if sy-subrc ne 0.
    cf_subrc = sy-subrc.
  endif.

ENDFORM.                               " VARIANT_get
*&---------------------------------------------------------------------*
*&      Form  VARIANT_DETERMINE
*&---------------------------------------------------------------------*
FORM VARIANT_DETERMINE .

  data: lf_subrc like sy-subrc.
* switch for variants: save user specific or common
  PERFORM VARIANT_INIT.
* Get default variant
* 1. variant from selection screen
  if not pa_varia is initial.
    GS_VARIANT-VARIANT =  PA_VARIA.
    PERFORM VARIANT_get changing lf_subrc.
  endif.
* 2. look for user or company specific variant
  if pa_varia is initial or
     lf_subrc = 2.
    PERFORM VARIANT_get changing lf_subrc.
* 3. if none found, then look explicitly for SAP standard.
    if lf_subrc = 2.
      GS_VARIANT-variant = c_var01.
      PERFORM VARIANT_get changing lf_subrc.
    endif.
  ENDIF.

ENDFORM.                               " VARIANT_DETERMINE
*&---------------------------------------------------------------------*
*&      Form  submit_job
*&---------------------------------------------------------------------*
form submit_job.
*   fill internal table with no of POs and range

    clear it_submit_pass.
    it_submit_pass[] = it_ekpo_pass[].
    if not so_bukrs is initial.
      loop at so_bukrs.
        move 'SO_BUKRS'      to it_submit_pass-selname.
        move 'S' to    it_submit_pass-kind.
        move so_bukrs-low to  it_submit_pass-low.
        move so_bukrs-high to  it_submit_pass-high.
        move so_bukrs-sign to   it_submit_pass-sign.
        move so_bukrs-option  to  it_submit_pass-option.
        append it_submit_pass.
        clear it_submit_pass.
      endloop.
    endif.
    if not so_werks is initial.
      loop at so_werks.
        move 'SO_WERKS'      to it_submit_pass-selname.
        move 'S' to    it_submit_pass-kind.
        move so_werks-low to  it_submit_pass-low.
        move so_werks-high to  it_submit_pass-high.
        move so_werks-sign   to        it_submit_pass-sign.
        move so_werks-option  to       it_submit_pass-option.
        append it_submit_pass.
        clear it_submit_pass.
      endloop.
    endif.
    if not so_budat is initial.
      loop at so_budat.
        move 'SO_BUDAT'      to it_submit_pass-selname.
        move 'S' to    it_submit_pass-kind.
        move so_budat-low to  it_submit_pass-low.
        move so_budat-high to  it_submit_pass-high.
        move so_budat-sign  to        it_submit_pass-sign.
        move so_budat-option  to       it_submit_pass-option.
        append it_submit_pass.
        clear it_submit_pass.
      endloop.
    endif.

    if not so_mjahr is initial.
      loop at so_mjahr.
        move 'SO_MJAHR'      to it_submit_pass-selname.
        move 'S' to    it_submit_pass-kind.
        move so_mjahr-low to  it_submit_pass-low.
        move so_mjahr-high to  it_submit_pass-high.
        move so_mjahr-sign to        it_submit_pass-sign.
        move so_mjahr-option to       it_submit_pass-option.
        append it_submit_pass.
        clear it_submit_pass.
      endloop.

    endif.
    if not so_lifnr is initial.
      loop at so_lifnr.
        move 'SO_LIFNR'      to it_submit_pass-selname.
        move 'S' to    it_submit_pass-kind.
        move so_lifnr-low to  it_submit_pass-low.
        move so_lifnr-high to  it_submit_pass-high.
        move so_lifnr-sign  to        it_submit_pass-sign.
        move so_lifnr-option to       it_submit_pass-option.
        append it_submit_pass.
        clear it_submit_pass.
      endloop.

    endif.

    if not so_ebeln is initial.
      loop at so_ebeln.
        move 'SO_EBELN'      to it_submit_pass-selname.
        move 'S' to    it_submit_pass-kind.
        move so_ebeln-low to  it_submit_pass-low.
        move so_ebeln-high to  it_submit_pass-high.
        move so_ebeln-sign  to        it_submit_pass-sign.
        move so_ebeln-option  to       it_submit_pass-option.
        append it_submit_pass.
        clear it_submit_pass.
      endloop.

    endif.

    if not so_ebelp is initial.
      loop at so_ebelp.
        move 'SO_EBELP'      to it_submit_pass-selname.
        move 'S' to    it_submit_pass-kind.
        move so_ebelp-low to  it_submit_pass-low.
        move so_ebelp-high to  it_submit_pass-high.
        move so_ebelp-sign  to        it_submit_pass-sign.
        move so_ebelp-option  to       it_submit_pass-option.
        append it_submit_pass.
        clear it_submit_pass.
      endloop.

    endif.

    move 'PA_XTEST'      to it_submit_pass-selname.
    move 'P' to    it_submit_pass-kind.
    move pa_xtest to  it_submit_pass-low.
    move 'I'   to        it_submit_pass-sign.
    move 'EQ'  to       it_submit_pass-option.
    append it_submit_pass.
    clear it_submit_pass.

    move 'PA_ERSBA'      to it_submit_pass-selname.
    move 'P' to    it_submit_pass-kind.
    move pa_ersba to  it_submit_pass-low.
    move 'I'   to        it_submit_pass-sign.
    move 'EQ'  to       it_submit_pass-option.
    append it_submit_pass.
    clear it_submit_pass.

    move 'PA_BUDAT'      to it_submit_pass-selname.
    move 'P' to    it_submit_pass-kind.
    move pa_budat to  it_submit_pass-low.
    move 'I'   to        it_submit_pass-sign.
    move 'EQ'  to       it_submit_pass-option.
    append it_submit_pass.
    clear it_submit_pass.

    move 'PA_EMESS'      to it_submit_pass-selname.
    move 'P' to    it_submit_pass-kind.
    move pa_emess to  it_submit_pass-low.
    move 'I'   to        it_submit_pass-sign.
    move 'EQ'  to       it_submit_pass-option.
    append it_submit_pass.
    clear it_submit_pass.

    move 'PA_DELNU'      to it_submit_pass-selname.
    move 'P' to    it_submit_pass-kind.
    move pa_delnu to  it_submit_pass-low.
    move 'I'   to        it_submit_pass-sign.
    move 'EQ'  to       it_submit_pass-option.
    append it_submit_pass.
    clear it_submit_pass.

    move 'PA_NOMES'      to it_submit_pass-selname.
    move 'P' to    it_submit_pass-kind.
    move pa_nomes to  it_submit_pass-low.
    move 'I'   to        it_submit_pass-sign.
    move 'EQ'  to       it_submit_pass-option.
    append it_submit_pass.
    clear it_submit_pass.

    move 'PA_NOVAL'      to it_submit_pass-selname.
    move 'P' to    it_submit_pass-kind.
    move pa_noval to  it_submit_pass-low.
    move 'I'   to        it_submit_pass-sign.
    move 'EQ'  to       it_submit_pass-option.
    append it_submit_pass.
    clear it_submit_pass.

    move 'PA_VARIA'      to it_submit_pass-selname.
    move 'P' to    it_submit_pass-kind.
    move pa_varia to  it_submit_pass-low.
    move 'I'   to        it_submit_pass-sign.
    move 'EQ'  to       it_submit_pass-option.
    append it_submit_pass.
    clear it_submit_pass.

    move 'PA_PRICE'      to it_submit_pass-selname.
    move 'P' to    it_submit_pass-kind.
    move pa_price to  it_submit_pass-low.
    move 'I'   to        it_submit_pass-sign.
    move 'EQ'  to       it_submit_pass-option.
    append it_submit_pass.
    clear it_submit_pass.

    job = job + 1.
    job_no+8(3) = job.
    taskname = job_no.
**   call function module for parallel processing
    CALL FUNCTION 'ZMRER_PARALLEL_PROCESSING'
     STARTING NEW TASK taskname DESTINATION IN GROUP Group
     performing get_results on end of task
     exporting
       JOB                  =  job_no
     tables
       IT_SUBMIT_PASS       = it_submit_pass
     EXCEPTIONS
       COMMUNICATION_FAILURE = 1 MESSAGE MSG_TEXT
       SYSTEM_FAILURE        = 2 MESSAGE MSG_TEXT.

    IF SY-SUBRC NE 0.
      WRITE: MSG_TEXT.
    ELSE.
      Wait up to job_wait seconds.
      add 1 to o_send_task.
      WRITE: 'O.K.'.
    ENDIF.

    clear:   it_submit_pass, it_ekpo_pass.
    refresh: it_submit_pass, it_ekpo_pass.

endform.                    " submit_job
