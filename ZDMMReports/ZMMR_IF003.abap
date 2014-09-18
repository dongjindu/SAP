*&---------------------------------------------------------------------*
*& Report  ZMMR_IF003                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT zmmr_if003 MESSAGE-ID zmm_if
                  NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
* Date        Request        Developer Description
* 10/30/2006  UD1K922788     Manju     Add Debit/Credit Indicator to
*                                      GR outbound interface
* 10/31/2006  UD1K922836     Manju     Pass HMMA as company
*                                      instead of H201
* 11/03/2006  UD1K922902     Manju     Add Material Document number to
*                                      selection screen
* 11/09/2006  UD1K922961     Manju     Add optin to selection screen
*                                      to re-send old GR data.
* 11/20/2006  UD1K923157     Manju     Change Error message to *
*                                      Information
*----------------------------------------------------------------------*


************************************************************************
* SAP Tables                                                           *
************************************************************************
TABLES : ekpo,
         ekbe,
         mara,
         adrp,
         sscrfields,
         ztmm_if018.
************************************************************************
* Include                                                              *
************************************************************************
.
INCLUDE <icon>.

************************************************************************
* TYPES and TYPE-POOLS                                                 *
************************************************************************
TYPE-POOLS: slis,   "> Globale Type(ALV)
            vrm .
TYPES:
*-- Single Value in Value Set
       BEGIN OF vrm_value,
         key(40) TYPE c,
         text(80) TYPE c,
       END OF vrm_value,
*-- Table of Values
       vrm_values TYPE vrm_value OCCURS 0,
*-- Id of Value Set
       vrm_id TYPE vrm_value-text,
*-- table of Ids of Value Set
       vrm_ids TYPE vrm_id OCCURS 0,
*-- QueueRow
       BEGIN OF vrm_queuerow,
         tag,
         value TYPE vrm_value,
       END   OF vrm_queuerow,
*-- Queue
       vrm_queue TYPE vrm_queuerow OCCURS 0.

************************************************************************
*  CONTROL                                                             *
************************************************************************
DATA: gs_layout TYPE lvc_s_layo,
      gs_fdcat  TYPE lvc_s_fcat,
      gt_fdcat  TYPE lvc_t_fcat.

************************************************************************
* Internal Tables                                                      *
************************************************************************
DATA: BEGIN OF it_tab OCCURS 0.
        INCLUDE STRUCTURE zsmm_if016.
DATA: gjahr LIKE ekbe-gjahr,
      elikz LIKE ekpo-elikz.
DATA: END OF it_tab.

DATA: BEGIN OF it_item OCCURS 0.
        INCLUDE STRUCTURE zsmm_if016.
DATA: END OF it_item.

DATA: wa_user LIKE usaddress.
DATA: it_head_log LIKE TABLE OF ztmm_if018 WITH HEADER LINE.
*DATA: IT_RETURN   LIKE TABLE OF ZTMM_IF019 WITH HEADER LINE.

*---- LIST BOX DATA
DATA: wa_fld_name  TYPE vrm_id,
      it_list_box  TYPE vrm_values,
      wa_value LIKE LINE OF it_list_box.

data : begin of it_comp occurs 10,                          "UD1K922836
       BUKRS type t001-bukrs,
       RCOMP type t001-rcomp,
       end of it_comp.


************************************************************************
* RANGES                                                               *
************************************************************************

*---// Number range
DATA number LIKE ztmm_if018-serno.

*---// Screen Range internal table
RANGES: ra_zztype FOR ekpo-zztype,
        ra_ebeln  FOR ekpo-ebeln.

*------------RANGE_MACRO----------------*
DEFINE range_macro.
  if   &2 ne '' and &3 ne ''
    or &2 eq '' and &3 ne ''.
    move: 'I'    to  &1-sign,
          'BT'   to  &1-option,
           &2    to  &1-low,
           &3    to  &1-high.
    append &1.
  elseif &2 ne '' and &3 eq ''.
    move : 'I'    to  &1-sign,
           'EQ'   to  &1-option,
            &2    to  &1-low.
    append &1.
  endif.
END-OF-DEFINITION.
*--------END OF RANGE_MACRO-------------*

DATA: gf_ebeln  TYPE ekpo-ebeln,
      gt_ebeln  TYPE ekpo-ebeln.

************************************************************************
* CONSTANTS                                                            *
************************************************************************
CONSTANTS : gt_zztype TYPE ekpo-zztype VALUE 'Z'.

************************************************************************
* VARIANTS                                                             *
************************************************************************
*---// Move index
DATA g_tabix LIKE sy-tabix.
*---// Select data checking field
DATA c_flag   TYPE c.
*---// item count check
DATA: cunt   LIKE ztmm_if018-cunt,
      l_zcnt LIKE ztmm_if018-zcnt.

************************************************************************
* RANGES                                                               *
************************************************************************
*---// Ranges internal table
RANGES: r_ebeln  FOR ztmm_if018-ebeln.
RANGES: r_ebelp  FOR ztmm_if018-ebelp.
RANGES: r_number FOR ztmm_if018-serno.
*---// Number Ranges variables
DATA:   v_number LIKE ztmm_if018-serno.

************************************************************************
* SELECT-OPTIONS / PARAMETERS                                          *
************************************************************************
*Search condition
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:   s_ebeln FOR ztmm_if018-ebeln MODIF ID iog,
                  s_ebelp FOR ztmm_if018-ebelp MODIF ID iog,
                  s_serno FOR ztmm_if018-serno,
                  s_cpudt for ekbe-cpudt default sy-datum,
                  s_belnr for ekbe-belnr.
selection-screen skip 1.
parameters :  p_chk as checkbox.                            "UD1K922961
SELECTION-SCREEN END   OF BLOCK box1.

*Division
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-004.
PARAMETERS: p_batch     RADIOBUTTON GROUP ab3 DEFAULT 'X',
            p_manual    RADIOBUTTON GROUP ab3.

SELECTION-SCREEN END OF BLOCK box2.

************************************************************************
* AT SELECTION-SCREEN                                                  *
************************************************************************
AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'FC01'.
    CALL SCREEN 0100 STARTING AT 1  1
                     ENDING   AT 48 5.
  ENDIF.

************************************************************************
* INITIALIZATION                                                       *
************************************************************************
INITIALIZATION.
  sscrfields-functxt_01 = icon_locked.
  SELECTION-SCREEN FUNCTION KEY 1.

************************************************************************
* START-OF-SELECTION                                                   *
************************************************************************
START-OF-SELECTION.

*---// Create number range
  PERFORM number_create.

*---// Table inner join selection
  PERFORM get_data_table.
  IF c_flag = 'X'.
    MESSAGE s001.
    EXIT.
  ENDIF.

*---// Transfer data to EAI
  PERFORM get_eai_transer.

*---// G/R Data search Report program call 'ZMMR_IF005'
  CHECK p_manual EQ 'X'.
  PERFORM submit_data.

************************************************************************
* END-OF-SELECTION                                                   *
************************************************************************



*&---------------------------------------------------------------------*
*&      Form  GET_DATA_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_table .
  DATA l_lgort LIKE mseg-lgort.

  CLEAR:   it_tab, it_item, it_head_log, g_tabix.
  REFRESH: it_tab, it_item, it_head_log.

*---// A~ EKKO
*---// B~ EKPO
*---// C~ EKBE
  if p_chk is initial.                                      "UD1K922961
    SELECT a~lifnr a~waers a~bukrs
           b~ebeln b~ebelp b~matnr b~elikz
           b~meins b~lgort b~txz01 b~werks
           b~netpr AS net_price
           b~menge AS menge c~menge AS zmenge
           c~bwart c~dmbtr c~belnr c~buzei c~gjahr
           c~bpmng c~SHKZG                                  "UD1K922788
           c~budat c~cpudt c~cputm                          "UD1K922961
           c~ERNAM as usnam                                 "UD1K922961
         FROM ekko AS a INNER JOIN ekpo AS b
           ON a~ebeln = b~ebeln
                       INNER JOIN ekbe AS c
           ON b~ebeln = c~ebeln
          AND b~ebelp = c~ebelp
         INTO CORRESPONDING FIELDS OF TABLE it_tab
          WHERE a~bsart  EQ 'ZB'
          AND b~zztype NOT IN ('F', 'Z')
          AND b~ebeln  IN s_ebeln
          AND b~ebelp  IN s_ebelp
          and c~belnr  in s_belnr
          AND c~bewtp  EQ 'E'
          and c~cpudt in s_cpudt.
  else.
* Begin of changes -  UD1K922961
    SELECT a~lifnr a~waers a~bukrs
          b~ebeln b~ebelp b~matnr b~elikz
          b~meins b~lgort b~txz01 b~werks
          b~netpr AS net_price
          b~menge AS menge c~menge AS zmenge
          c~bwart c~dmbtr c~belnr c~buzei c~gjahr
          c~bpmng c~SHKZG                                   "UD1K922788
          c~budat c~cpudt c~cputm                           "UD1K922961
          c~ERNAM as usnam                                  "UD1K922961
       FROM ekko AS a INNER JOIN ekpo AS b
          ON a~ebeln = b~ebeln
                      INNER JOIN ekbe AS c
          ON b~ebeln = c~ebeln
         AND b~ebelp = c~ebelp
        INTO CORRESPONDING FIELDS OF TABLE it_tab
         WHERE a~bsart  EQ 'ZB'
         AND b~ebeln  IN s_ebeln
         AND b~ebelp  IN s_ebelp
         and c~belnr  in s_belnr
         AND c~bewtp  EQ 'E'
         and c~cpudt in s_cpudt.
* End of changes - UD1K922961
  endif.

  if it_tab[] is initial and p_manual  is initial.
    message i000 with  'No data found'.                    "UD1K923157
    exit.
  endif.
* Select Company code - UD1K922836
  select BUKRS RCOMP into table it_comp                     "UD1K922836
         from t001                                          "UD1K922836
         where RCOMP ne space.                              "UD1K922836


  IF NOT it_tab[] IS INITIAL.

    DATA: lv_konts LIKE t030-konts.

    LOOP AT it_tab.
      g_tabix = sy-tabix.
*---// MARA table checking for 'ROH1'
* In Prod. since none of the materials have
* basic material desc. so the below code
* is okay otherwise it needs to be modified
      SELECT SINGLE wrkst
               FROM mara
               INTO it_item-wrkst
              WHERE matnr =  it_tab-matnr
                AND mtart =  'ROH1'.
      IF sy-subrc EQ 0.
        CONTINUE.
      ELSE.
*---< GR Outbound additional transfer data implementation start
*---// Changed by YWYANG
        CLEAR: lv_konts.

        SELECT SINGLE isocode INTO it_tab-zmeins
                              FROM t006
                              WHERE msehi = it_tab-meins.

* Begin of changes - UD1K922961
*        SELECT SINGLE budat FROM mkpf       " Posting date
*                            INTO it_tab-budat
*                            WHERE mblnr = it_tab-belnr
*                            AND   mjahr = it_tab-gjahr.
* End of changes - UD1K922961


        PERFORM get_asset_account CHANGING lv_konts.

        IF it_tab-dmbtr IS INITIAL.                         "UD1K922961
          PERFORM get_amount_in_lc.
        endif.

        MOVE: lv_konts TO it_tab-konts.

        MOVE: it_tab-zmenge TO it_tab-zgrqty.
*---> 2006/02/16 - end of change

        MOVE-CORRESPONDING it_tab TO it_item.
*---< Move GR Qty to transfer data
*        MOVE: it_tab-zmenge TO it_item-zmenge,
        move  it_tab-zmenge TO it_item-zgrqty.

**--// Net Price   - UD1K922961
*        SELECT SINGLE netpr
*        FROM ekpo
*        INTO it_item-net_price
*        WHERE ebeln = it_tab-ebeln
*        AND ebelp   = it_tab-ebelp.
* - UD1K922961

*---// MBEW table checking
*--// Standard Price.
        SELECT SINGLE stprs
                 FROM mbew
                 INTO it_item-stprs
                WHERE matnr = it_tab-matnr
                  AND bwkey = it_tab-werks
                  AND vprsv = 'S'.

*---// MARD table checking
*--// Storage Bin
        IF it_tab-lgort IS INITIAL.              " Storage location

          CLEAR it_tab-lgort.
          SELECT SINGLE lgort
                   INTO it_item-lgort
                   FROM mseg
                  WHERE mblnr = it_tab-belnr
                    AND mjahr = it_tab-gjahr
                    AND zeile = it_tab-buzei.
        ENDIF.

        SELECT SINGLE lgpbe
                 FROM mard
                 INTO it_item-lgpbe
                WHERE matnr = it_tab-matnr
                  AND werks = it_tab-werks
                  AND lgort = it_tab-lgort.


*---// MKPF table checking  _ begin of changes - UD1K922961
*        SELECT SINGLE cpudt cputm usnam
*                 FROM mkpf
*                 INTO (it_item-cpudt,it_item-cputm,it_item-usnam)
*                WHERE mblnr = it_tab-belnr
*                  AND mjahr = it_tab-gjahr.
* End of changes - UD1K922961

*---// HR User name select call function
        PERFORM name_select.
      ENDIF.

* Company Code.
      read table it_comp with key bukrs = it_tab-bukrs.     "UD1K922836
      if sy-subrc eq 0.                                     "UD1K922836
        it_item-RCOMP = it_comp-rcomp.                      "UD1K922836
      endif.                                                "UD1K922836


*---// Log data save

      ADD 1 TO cunt.
      MOVE-CORRESPONDING it_item TO it_head_log.
      MOVE: number               TO it_head_log-serno,
            cunt                 TO it_head_log-cunt,
            sy-datum             TO it_head_log-tran_date,
            sy-uzeit             TO it_head_log-tran_time,
            sy-uname             TO it_head_log-tran_name.

      l_zcnt = l_zcnt + 1.
      it_item-zcnt = l_zcnt.
*-- Currency = 'USD'
*      it_item-waers = 'USD'.

      APPEND: it_head_log, it_item.
      CLEAR:  it_head_log, it_item.

      AT END OF ebelp.
        CLEAR l_zcnt.
      ENDAT.

    ENDLOOP.
  ELSE.
*---// Log data save not found
    c_flag = 'X'.
    EXIT.
  ENDIF.

ENDFORM.                    " GET_DATA_TABLE
*&---------------------------------------------------------------------*
*&      Form  GET_EAI_TRANSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_eai_transer.

  CLEAR:   it_head_log, it_item, cunt,
           g_tabix,     ztmm_if018.
  data:  L_errtext(50) type c.

  IF NOT it_item[] IS INITIAL.

    CALL FUNCTION 'ZMMF_IF_GR_OUTBOUND' DESTINATION 'VAATZ_HMMA'
      TABLES
        it_outbound = it_item
*      E_RETURN    = IT_RETURN.
    EXCEPTIONS
     communication_failure = 1
     system_failure        = 2.
  ENDIF.
  case sy-subrc.
    when 1.
      l_errtext = 'Communication failure'.
    when 2.
      l_errtext = 'System failure '.
  endcase.

  IF sy-subrc NE 0.
    LOOP AT it_item.
      ADD 1 TO cunt.
      MOVE: 'E'                     TO it_head_log-type,
*           'COMMUNICATION_FAILURE' TO it_head_log-message.
            l_errtext               TO it_head_log-message.
      MODIFY it_head_log TRANSPORTING type message
                      WHERE serno = number
                        AND cunt  = cunt.
      CLEAR: it_head_log, g_tabix.
    ENDLOOP.
  ELSE.
    check   p_chk is initial and
            s_belnr is initial.
    LOOP AT it_item.
      ADD 1 TO cunt.
*---// Transfer success data save Log table
      MOVE: it_item-type    TO it_head_log-type,
            it_item-message TO it_head_log-message.

      IF it_item-type = 'S'.
*---// Update Table EKPO to Enhancement's field (message type)
        READ TABLE it_tab WITH KEY ebeln = it_item-ebeln
                                   ebelp = it_item-ebelp
                                   elikz = 'X'.

*---// ELIKZ value 'X' Transmission is finish
        IF sy-subrc = 0.
          UPDATE ekpo   SET   zztype = 'F'
                      WHERE   ebeln = it_item-ebeln
                        AND   ebelp = it_item-ebelp.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.

*---// Transfer error data modify Log table
      MODIFY it_head_log TRANSPORTING type message
                                WHERE serno = number
                                  AND cunt  = cunt.
      CLEAR: it_head_log, g_tabix.

    ENDLOOP.
  ENDIF.

  INSERT ztmm_if018 FROM TABLE it_head_log.
  COMMIT WORK AND WAIT.
ENDFORM.                    " GET_EAI_TRANSER
*&---------------------------------------------------------------------*
*&      Form  NUMBER_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM number_create.

  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = '05'
            object                  = 'ZSERNO4'
            quantity                = '1'
       IMPORTING
            number                  = number
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.

ENDFORM.                    " NUMBER_CREATE

*&---------------------------------------------------------------------*
*&      Form  SUBMIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM submit_data .
  CLEAR:   r_number, r_ebeln, r_ebelp.
  REFRESH: r_number, r_ebeln, r_ebelp.

  MOVE: 'I'      TO r_number-sign,
        'EQ'     TO r_number-option,
        s_serno  TO r_number-low,
        space    TO r_number-high.

  MOVE: 'I'      TO r_ebeln-sign,
        'EQ'     TO r_ebeln-option,
        s_ebeln  TO r_ebeln-low,
        space    TO r_ebeln-high.

  MOVE: 'I'      TO r_ebelp-sign,
        'EQ'     TO r_ebelp-option,
        s_ebelp  TO r_ebelp-low,
        space    TO r_ebelp-high.


  APPEND:  r_number, r_ebeln, r_ebelp.
  CLEAR:   r_number, r_ebeln, r_ebelp.

  SUBMIT zmmr_if005 AND RETURN
         WITH s_ebeln IN r_ebeln
         WITH s_ebelp IN r_ebelp
         WITH s_serno IN r_number.
*         WITH P_ALL   EQ 'X'.
ENDFORM.                    " SUBMIT_DATA
*&---------------------------------------------------------------------*
*&      Form  NAME_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM name_select .
  CLEAR wa_user.
  CALL FUNCTION 'SUSR_USER_READ'
    EXPORTING
      user_name                  = it_item-usnam
*     WITH_TEXT                  =
*     READ_UCLASS                = ' '
   IMPORTING
*     USER_LOGONDATA             =
*     USER_DEFAULTS              =
      user_address               = wa_user
*     REF_USER                   =
*     ALIAS                      =
*     UCLASS                     =
*   TABLES
*     USER_PARAMETERS            = IT_PARA.
*     UCLASSSYS                  =
   EXCEPTIONS
     user_name_not_exists       = 1
     internal_error             = 2
     OTHERS                     = 3.

*  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF

  SELECT SINGLE name_last
           FROM adrp
           INTO it_item-zname
          WHERE persnumber = wa_user-persnumber
            AND date_from  = '00010101'
            AND nation     = space.

ENDFORM.                    " NAME_SELECT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  IF sy-ucomm = 'CANC'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXEC'.
      PERFORM update_type.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_type.
  PERFORM call_range.

  UPDATE ekpo SET zztype = gt_zztype
            WHERE ebeln IN ra_ebeln.
  COMMIT WORK AND WAIT.
ENDFORM.                    " UPDATE_TYPE
*&---------------------------------------------------------------------*
*&      Form  CALL_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_range.

*-- Transfer attribute type
  REFRESH ra_zztype.
  IF NOT gt_zztype IS INITIAL.
    ra_zztype-sign   = 'I'.
    ra_zztype-option = 'EQ'.
    ra_zztype-low    = gt_zztype.
    APPEND ra_zztype.
  ENDIF.

*-- P/R number condition
  REFRESH ra_ebeln.
  range_macro ra_ebeln gf_ebeln gt_ebeln.
  CLEAR: gf_ebeln, gt_ebeln.
ENDFORM.                    " CALL_RANGE
*&---------------------------------------------------------------------*
*&      Form  get_amount_local_currency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_WAERS  text
*      <--P_LV_LAMOUNT  text
*----------------------------------------------------------------------*
FORM get_amount_local_currency USING    p_lv_waers
                               CHANGING p_lv_lamount.
*  DATA: lv_budat1 LIKE sy-datum,
*        lv_budat2(10).

  DATA: budat_old LIKE mkpf-budat,
        budat_new LIKE mkpf-budat.

*  MOVE: it_tab-budat TO lv_budat1.
*  WRITE: lv_budat1 TO lv_budat2.

*  DO 20 TIMES.
  CLEAR: p_lv_lamount.

  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
    EXPORTING
*     CLIENT                  = SY-MANDT
      date                    = it_tab-budat
      foreign_amount          = it_tab-net_price
      foreign_currency        = it_tab-waers
      local_currency          = p_lv_waers
      rate                    = 0
      type_of_rate            = 'M'
      read_tcurr              = 'X'
   IMPORTING
*     exchange_rate           =
*     FOREIGN_FACTOR          =
      local_amount            = p_lv_lamount
*     LOCAL_FACTOR            =
*     EXCHANGE_RATEX          =
*     FIXED_RATE              =
*     DERIVED_RATE_TYPE       =
   EXCEPTIONS
     no_rate_found           = 1
     overflow                = 2
     no_factors_found        = 3
     no_spread_found         = 4
     derived_2_times         = 5
     OTHERS                  = 6
            .
  IF sy-subrc EQ 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ELSE.
    CLEAR p_lv_lamount.
*      CLEAR: budat_old, budat_new.
*
*      MOVE: it_tab-budat TO budat_old.
*
*      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*        EXPORTING
*          date            = budat_old
*          days            = '01'
*          months          = '00'
*          signum          = '-'
*          years           = '00'
*        IMPORTING
*          calc_date       = budat_new
*                .
*      IF NOT budat_new IS INITIAL.
*        MOVE: budat_new TO it_tab-budat.
*      ENDIF.
  ENDIF.
*  ENDDO.
ENDFORM.                    " get_amount_local_currency
*&---------------------------------------------------------------------*
*&      Form  get_asset_account
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_KONTS  text
*----------------------------------------------------------------------*
FORM get_asset_account CHANGING p_lv_konts.

  DATA: lv_bklas LIKE mbew-bklas,
        lv_ktopl LIKE t001-ktopl.

  CLEAR: lv_bklas, lv_ktopl.
  CLEAR: p_lv_konts.

  SELECT SINGLE bklas FROM mbew            " Valuation class
                      INTO lv_bklas
                      WHERE matnr = it_tab-matnr
                      AND   bwkey = it_tab-werks.

  SELECT SINGLE ktopl FROM t001            " Chart of account
                      INTO lv_ktopl
                      WHERE bukrs = it_tab-bukrs.

  SELECT SINGLE konts FROM t030            " Asset account
                      INTO p_lv_konts
                      WHERE bklas = lv_bklas
                      AND   ktopl = lv_ktopl
                      AND   ktosl = 'BSX'.

ENDFORM.                    " get_asset_account
*&---------------------------------------------------------------------*
*&      Form  get_amount_in_lc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_amount_in_lc.
  DATA: lv_menge   LIKE mseg-menge,
        lv_waers   LIKE t001-waers,
        lv_lamount LIKE mseg-menge.

  CLEAR: lv_menge, lv_waers, lv_lamount.

*        SELECT SINGLE netpr FROM ekpo          " Net price
*                            INTO lv_netpr
*                            WHERE ebeln = it_tab-ebeln
*                            AND   ebelp = it_tab-ebelp.

  SELECT SINGLE menge FROM mseg               " GR Qty
                      INTO lv_menge
                      WHERE mblnr = it_tab-belnr
                        AND mjahr = it_tab-gjahr
                        AND zeile = it_tab-buzei.

  SELECT SINGLE waers FROM t001               " Currency unit
                      INTO lv_waers
                      WHERE bukrs = it_tab-bukrs.

  IF it_tab-waers NE lv_waers.
    PERFORM get_amount_local_currency USING    lv_waers
                                      CHANGING lv_lamount.
    IF lv_lamount IS INITIAL.
      CLEAR it_tab-dmbtr.
    ELSE.
      it_tab-dmbtr = lv_lamount * lv_menge.
    ENDIF.
  ELSE.
    it_tab-dmbtr = it_tab-net_price * lv_menge.
  ENDIF.

ENDFORM.                    " get_amount_in_lc
