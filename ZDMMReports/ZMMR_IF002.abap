*&---------------------------------------------------------------------*
*& Report  ZMMR_IF002                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Date        Developer      Request           Description
*& 08/29/06    Manju          UD1K921930        Send  User Name / Dept.
*&                                              details of the person
*&                                              who created PR to VAATZ
*& 09/01/06    Manju          UD1K921983        Bug fix to avoid those
*&                                              extra records into log
*&                                              table incase of RFC
*&                                              Failure.
*& 09/13/06    Manju          UD1K922124        Remove Prefix from Name
*& 10/31/06    Manju          UD1K922828        Add message field to *
*                                               capture return error
*                                               message from Vaatz
* 10/31/06     Manju          UD1K922836        Pass HMMA as company
*                                               instead of H201
* 11/14/06     Manju          UD1K923009        Add re-sending Option
* 11/12/07     Rakesh         UD1K942140        Replace field RCOMP with
*              Gandhi                           SORT1 of table ADRC
*&---------------------------------------------------------------------*

REPORT  zmmr_if002 MESSAGE-ID zmm_if
                   NO STANDARD PAGE HEADING.
*------------RANGE----------------*
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
************************************************************************
* SAP Tables                                                           *
************************************************************************
TABLES : eban,
         ebkn,
         sscrfields,
         ztmm_if005,
         ztmm_if006.
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
*---// EBAN & EBKN join internal table
DATA : BEGIN OF it_tab OCCURS 0.
        INCLUDE STRUCTURE zsmm_if001.
DATA :    frgkz  LIKE eban-frgkz,     "< Release indicator.
          frgzu  LIKE eban-frgzu,     "< Release status
          frgst  LIKE eban-frgst,     "< Release code
          zztype LIKE eban-zztype,    "< I/F Message type
          estkz LIKE eban-estkz.      "Creation indicator
DATA : END OF it_tab.

DATA it_ebkn  LIKE TABLE OF ebkn WITH HEADER LINE.

DATA : BEGIN OF  it_st_eban OCCURS 0,
        banfn LIKE eban-banfn,
       END OF it_st_eban.

DATA : BEGIN OF  it_st_eban1 OCCURS 0,
        banfn LIKE eban-banfn,
        bnfpo LIKE eban-bnfpo,
        zztype1 LIKE eban-zztype1,
        zztype LIKE eban-zztype,
       END OF it_st_eban1.


*---// P/R outbound transfer internal table
DATA : it_outbound LIKE TABLE OF zsmm_if001 WITH HEADER LINE.

*---// P/R outbound Log data save internal table
DATA : it_out_log  LIKE TABLE OF ztmm_if005 WITH HEADER LINE.

*---// RETURN message internal table
DATA : it_return         LIKE TABLE OF zsmm_if012 WITH HEADER LINE.
* DATA : IT_RELEASE_RETURN LIKE TABLE OF BAPIRETURN WITH HEADER LINE.

*---- LIST BOX DATA
DATA: wa_fld_name  TYPE vrm_id,
      it_list_box  TYPE vrm_values,
      wa_value LIKE LINE OF it_list_box.

*---- RECIPIENT
DATA: lv_ernam TYPE zsmm_if001-ernam.

DATA : BEGIN OF it_comp OCCURS 10,                          "UD1K922836
       bukrs TYPE t001-bukrs,
*       RCOMP TYPE T001-RCOMP,              " -UD1K942140
       adrnr TYPE t001-adrnr,               " +UD1K942140
       sort1 TYPE adrc-sort1,               " +UD1K942140
       END OF it_comp.

*-Start of changes +UD1K942140
DATA : BEGIN OF it_adrc OCCURS 10,
         addrnumber TYPE adrc-addrnumber,
         sort1 TYPE adrc-sort1,
       END OF it_adrc.
*-End of changes +UD1K942140

************************************************************************
* RANGES                                                               *
************************************************************************
*---// Number Ranges variables
DATA:   v_number LIKE ztmm_if005-serno.

*---// Ranges internal table
RANGES: r_number FOR ztmm_if005-serno.

*---// Screen Range internal table
RANGES: ra_zztype FOR eban-zztype,
        ra_banfn  FOR eban-banfn.

DATA: gf_banfn  TYPE eban-banfn,
      gt_banfn  TYPE eban-banfn.

************************************************************************
* CONSTANTS                                                            *
************************************************************************
*---// P/R outbound flag.
CONSTANTS : v_outbound_flag TYPE ztmm_if005-flag VALUE '3',
            gt_zztype TYPE eban-zztype VALUE 'Z'.


************************************************************************
* VARIANTS                                                             *
************************************************************************
*---// Table index check.
DATA l_tabix  LIKE sy-tabix.
*---// Count field
DATA count    LIKE ztmm_if005-cunt.
*---// Select data checking field
DATA c_flag   TYPE c.
*---// Log Item sequential no
DATA: l_sernoi  LIKE ztmm_if006-sernoi.

************************************************************************
* SELECT-OPTIONS / PARAMETERS                                          *
************************************************************************
*Search condition
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:   s_werks FOR ztmm_if005-werks 	            "Plant code.
                          NO-EXTENSION NO INTERVALS,
                  s_banfn FOR eban-banfn,
                  s_erdat FOR eban-erdat.
SELECTION-SCREEN END OF BLOCK box1.

*Division
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
PARAMETERS: p_batch     RADIOBUTTON GROUP ab3 DEFAULT 'X',
            p_manual    RADIOBUTTON GROUP ab3.
*selection-screen skip 1.
PARAMETERS  : p_rsend AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK box2.

************************************************************************
* AT SELECTION-SCREEN                                                  *
************************************************************************
AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'FC01'.
    CALL SCREEN 0100 STARTING AT 1  1
                     ENDING   AT 40 5     .
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
  PERFORM number_range.
*---// Selection table for condition EBAN & EBKN
  PERFORM get_data_for_table.
*---// Search data empty check.
  IF c_flag        = 'X'.
    EXIT.
  ELSE.

*---// Transfer data to EAI
    PERFORM get_eai_transer.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  get_data_for_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_for_table.
  DATA: l_cunt LIKE ztmm_if005-cunt,
       l_bukrs LIKE pa0001-bukrs.

  DATA: l_tabix LIKE sy-tabix.      " +UD1K942140

  DATA :  st TYPE i,
          st1 TYPE i.
  FIELD-SYMBOLS: <fs>.


  CLEAR c_flag.
  CLEAR:   it_tab, it_outbound, it_out_log, it_ebkn.
  REFRESH: it_tab, it_outbound, it_out_log, it_ebkn.

  IF p_rsend  EQ 'X'.
    SELECT banfn  bnfpo bsart loekz
          knttp  pstyp matnr txz01
          werks  lgort matkl menge
          meins  lpein lfdat preis
          peinh  ekgrp ekorg flief
          dispo  afnam bednr lifnr
          frgkz  frgzu frgst waers
          zztype geber ernam estkz
          INTO CORRESPONDING FIELDS OF TABLE it_tab
          FROM eban
        WHERE bsart  IN ('NB', 'PM', 'ZB')
           AND matkl  NE 'AM'
           AND erdat IN s_erdat
*         AND zztype IN (space, 'E', 'W')
           AND werks  IN s_werks
           AND banfn  IN s_banfn
           AND loekz  NE 'X'.
  ELSE.
    SELECT banfn  bnfpo bsart loekz
          knttp  pstyp matnr txz01
          werks  lgort matkl menge
          meins  lpein lfdat preis
          peinh  ekgrp ekorg flief
          dispo  afnam bednr lifnr
          frgkz  frgzu frgst waers
          zztype geber ernam estkz
          INTO CORRESPONDING FIELDS OF TABLE it_tab
          FROM eban
        WHERE bsart  IN ('NB', 'PM', 'ZB')
           AND matkl  NE 'AM'
*          AND zztype  IN (space, 'E', 'W')
           AND zztype1 IN (space, 'E', 'W')
           AND werks  IN s_werks
           AND banfn  IN s_banfn
           AND loekz  NE 'X'.
  ENDIF.
  IF NOT it_tab[] IS INITIAL.
    SELECT banfn bnfpo anln1 geber
           aufnr sakto kokrs kostl
           FROM  ebkn
           INTO CORRESPONDING FIELDS OF TABLE it_ebkn
           FOR ALL ENTRIES IN it_tab
*---// release condition index check
           WHERE banfn = it_tab-banfn
             AND bnfpo = it_tab-bnfpo.
  ENDIF.

* Select Company code - UD1K922836
*-Start of changes UD1K942140
*  SELECT bukrs rcomp INTO TABLE it_comp                     "UD1K922836
*         FROM t001                                          "UD1K922836
*         WHERE rcomp NE space.                              "UD1K922836

  SELECT bukrs adrnr
         INTO TABLE it_comp
         FROM t001
         WHERE adrnr NE space.

  IF NOT it_comp[] IS INITIAL.
    SELECT addrnumber sort1
           INTO TABLE it_adrc
           FROM adrc
           FOR ALL ENTRIES IN it_comp
           WHERE addrnumber = it_comp-adrnr.
    SORT it_adrc BY addrnumber.
    LOOP AT it_comp.
      l_tabix = sy-tabix.
      READ TABLE it_adrc WITH KEY addrnumber = it_comp-adrnr
                                              BINARY SEARCH.
      IF sy-subrc = 0.
        it_comp-sort1 = it_adrc-sort1.
        MODIFY it_comp INDEX l_tabix TRANSPORTING sort1.
      ENDIF.
    ENDLOOP.
  ENDIF.
*-End of changes ud1k942140

  IF NOT it_tab[] IS INITIAL.
    CLEAR l_cunt.
    LOOP AT it_tab.
      CLEAR lv_ernam.
*--2006.02.02 Modification by YWYANG----------------------------
* Get recipient ID number using plant and MRP controller from a table
      IF it_tab-estkz EQ 'B'.
        SELECT SINGLE mempf FROM t024d
                            INTO lv_ernam
                            WHERE werks = it_tab-werks
                            AND   dispo = it_tab-dispo.

        it_tab-ernam = lv_ernam.
      ENDIF.
*---------------------------------------------------------------

*--Old statement-----------------------------------------------
      IF it_tab-zztype NE 'W'.
        IF it_tab-frgkz = '5'.
*--------------------------------------------------------------
          MOVE-CORRESPONDING it_tab TO it_outbound.
          MOVE-CORRESPONDING it_tab TO it_out_log.

** Changed by Furong on 05/19/10
          IF it_tab-KNTTP = 'A'.
            it_outbound-KNTTP = 'F'.
          ENDIF.
** End of change

*---// RECIPIENT ID NUMBER MOVE
          IF NOT lv_ernam IS INITIAL.
            MOVE: lv_ernam TO it_outbound-ernam,
                  lv_ernam TO it_out_log-ernam.
          ENDIF.

* Begin of  changes - UD1K921930
          IF NOT it_outbound-ernam IS INITIAL.
            SELECT SINGLE ename kostl bukrs INTO
                                    (it_outbound-uname,
                                     it_outbound-udept,l_bukrs)
                FROM pa0001 WHERE pernr = it_outbound-ernam AND
                                  begda <= sy-datum AND
                                  endda >= sy-datum.
            READ TABLE it_comp WITH KEY bukrs =  l_bukrs.   "UD1K922836
            IF sy-subrc EQ 0.                               "UD1K922836
*              it_outbound-rcomp = it_comp-rcomp.            "UD1K922836
              it_outbound-rcomp = it_comp-sort1.         " + UD1K942140
            ENDIF.                                          "UD1K922836

* Begin of changes - UD1K922124
* To Remove Prefix from Name
            st = strlen( it_outbound-uname ) .
            st1 = 0.
            WHILE st > 0.
              ASSIGN it_outbound-uname+st1(1) TO <fs>.
              IF <fs> EQ '' .
                st1 = st1 + 1.
                EXIT.
              ENDIF.
              st1 = st1 + 1.
              st = st - 1.
            ENDWHILE.
            SHIFT it_outbound-uname BY st1 PLACES LEFT.
* End of changes - UD1K922124

            it_out_log-uname = it_outbound-uname.
            it_out_log-udept = it_outbound-udept.
            IF sy-subrc EQ 0 AND
                    NOT it_outbound-udept IS INITIAL.
              SELECT SINGLE ltext INTO it_outbound-dname
                  FROM  cskt
                 WHERE spras = sy-langu AND
                       kokrs = l_bukrs  AND
                       kostl = it_outbound-udept AND
                       datbi >= sy-datum.
              it_out_log-dname =  it_outbound-dname.
            ENDIF.
          ENDIF.
* End   of changes -  UD1K921930

*2006.02.14 modification by sgcho
*          IF IT_TAB-KNTTP EQ 'F'.
          READ TABLE it_ebkn WITH KEY banfn = it_tab-banfn
                                      bnfpo = it_tab-bnfpo.
          IF sy-subrc = 0.
            it_outbound-aufnr = it_ebkn-aufnr.
            it_outbound-sakto = it_ebkn-sakto.
            READ TABLE it_comp WITH KEY bukrs =  it_ebkn-kokrs."
            IF sy-subrc EQ 0.                               "UD1K922836
*              it_outbound-rcomp = it_comp-rcomp.            "UD1K922836
              it_outbound-rcomp = it_comp-sort1.         " + UD1K942140
            ENDIF.                                          "UD1K922836
            it_outbound-kokrs = it_ebkn-kokrs.
            it_outbound-kostl = it_ebkn-kostl.
            it_outbound-anln1 = it_ebkn-anln1.
            it_outbound-geber = it_ebkn-geber.
*--2006.01.16 Modification-------------------------------------
            it_outbound-fistl = it_ebkn-fistl.
            it_outbound-fipos = it_ebkn-fipos.
*--------------------------------------------------------------
            it_out_log-aufnr = it_ebkn-aufnr.
            it_out_log-sakto = it_ebkn-sakto.
            it_out_log-kokrs = it_ebkn-kokrs.
            it_out_log-kostl = it_ebkn-kostl.
            it_out_log-anln1 = it_ebkn-anln1.
            it_out_log-geber = it_ebkn-geber.
*--2006.01.16 Modification-------------------------------------
            it_out_log-fistl = it_ebkn-fistl.
            it_out_log-fipos = it_ebkn-fipos.
*--------------------------------------------------------------
          ENDIF.
* For Temporary Employee's NO HR data is maintained
* So Default Values for uname, udept and dname.
          IF it_outbound-uname IS INITIAL.
            it_outbound-uname = 'N/A'.
            it_out_log-uname = it_outbound-uname.
          ENDIF.
          IF it_outbound-udept IS INITIAL.
            it_outbound-udept = '11111'.
            it_out_log-udept = it_outbound-udept.
          ENDIF.
          IF it_outbound-dname IS INITIAL.
            it_outbound-dname = 'ASK to Buyer'.
            it_out_log-dname =  it_outbound-dname.
          ENDIF.

          APPEND it_outbound.
          CLEAR  it_outbound.

          it_out_log-serno = v_number.

          ADD 1 TO l_cunt.
          it_out_log-cunt = l_cunt.
          APPEND it_out_log. CLEAR it_out_log.
        ENDIF.

      ELSE.
        it_out_log-banfn = it_tab-banfn.
        it_out_log-bnfpo = it_tab-bnfpo.
*---// RECIPIENT ID NUMBER MOVE
        IF NOT lv_ernam IS INITIAL.
          MOVE: lv_ernam TO it_out_log-ernam.
        ENDIF.

* Transfer success and realase fale.
*---Modification 2006.01.20----------------------
        it_out_log-type  = 'W'.
*------------------------------------------------

        it_out_log-serno = v_number.
        ADD 1 TO l_cunt.
        it_out_log-cunt = l_cunt.
        APPEND it_out_log. CLEAR it_out_log.
      ENDIF.
    ENDLOOP.
  ELSE.
    c_flag = 'X'.
    EXIT.
  ENDIF.
ENDFORM.                    " get_data_for_table

*&---------------------------------------------------------------------*
*&      Form  GET_EAI_TRANSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_eai_transer .
  CLEAR:   it_return,
*--Modification 2006.01.20---------------------------------------------
           it_out_log.
*----------------------------------------------------------------------
  REFRESH: it_return.

  IF NOT it_outbound[] IS INITIAL.
*---// P/R Outbound data transfer Vaatz
    CALL FUNCTION 'ZMMF_IF_PR_OUTBOUND' DESTINATION 'VAATZ_HMMA'
      TABLES
        it_zsmm_if001         = it_outbound
        return                = it_return
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2.
** Change by Furong on 10/11/07 '7A5F3A4914
    IF sy-subrc NE 0 AND sy-batch EQ 'X'.
      PERFORM send_email.
    ENDIF.
** End of change
  ENDIF.
* 2006.01.16 Modification---------------------------------------------
  LOOP AT it_return.
*   UPDATE eban SET zztype  = it_return-type
    UPDATE eban SET zztype1 = it_return-type
              WHERE  banfn = it_return-banfn
                AND  bnfpo = it_return-bnfpo.
    COMMIT WORK AND WAIT.
*--Modification 2006.01.20--------------------------------------------
    READ TABLE it_out_log WITH KEY banfn = it_return-banfn
                                   bnfpo = it_return-bnfpo.
    IF sy-subrc = 0.
      MOVE: v_outbound_flag   TO it_out_log-flag,
            it_return-type    TO it_out_log-type,
            sy-datum          TO it_out_log-tran_date,
            sy-uzeit          TO it_out_log-tran_time,
            it_return-message TO it_out_log-message.        "UD1K922828
*           IT_RETURN-BANFN   TO IT_OUT_LOG-BANFN,
*           IT_RETURN-BNFPO   TO IT_OUT_LOG-BNFPO.
      MODIFY it_out_log INDEX sy-tabix.
      CLEAR  it_out_log.
    ENDIF.
  ENDLOOP.

* Begin of changes - UD1K921983
  IF NOT it_return[] IS INITIAL.
    INSERT ztmm_if005 FROM TABLE it_out_log.
    COMMIT WORK AND WAIT.
 MESSAGE s999(zmmm) WITH 'Log table entries created with Serial number'
             v_number.
  ENDIF.
* End of changes - UD1K921983

* Begin of changes - UD1K923009
* if the problem occurs due to delayed update's ( Asynchoronous update)
* then we might have to put wait statement to overcome inconstiency.

* if partial line items of PR are interfaced to Vaatz i.e if some of
*line items PR are marked for deletion or not released, set flag of
*line  items  which are not interfaced to VAATZ as Z.
  DELETE it_return WHERE type NE 'S'.
  it_st_eban[] = it_return[].
  DELETE ADJACENT DUPLICATES FROM it_st_eban.
  IF NOT it_st_eban[] IS INITIAL.
    SELECT banfn  bnfpo
            zztype1 zztype
            INTO  TABLE it_st_eban1
            FROM eban FOR ALL ENTRIES IN it_st_eban
          WHERE banfn = it_st_eban-banfn AND
              bsart  IN ('NB', 'PM', 'ZB')
*             AND matkl  NE 'AM'
*             and erdat in s_erdat
              AND zztype1 NE 'S' .
*             AND loekz  eq 'X' or
*                 frgkz  ne '5' .
  ENDIF.
  LOOP AT it_st_eban1.
    UPDATE eban SET zztype1 = 'Z'
              WHERE  banfn = it_st_eban1-banfn
                AND  bnfpo = it_st_eban1-bnfpo.
  ENDLOOP.
  COMMIT WORK AND WAIT.

* End of changes - UD1K923009
*---------------------------------------------------------------------

*---// Outbound data save log table and BAPI release
*  PERFORM GET_RELEASE.
*---// HMMA Release step delete 2006.01.16

*---// P/R Data search Report program call 'ZMMR_IF001'
  CHECK p_manual EQ 'X'.
  PERFORM submit_data.

ENDFORM.                    " GET_EAI_TRANSER
*&---------------------------------------------------------------------*
*&      Form  GET_RELEASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_RELEASE .
*
*  CLEAR   IT_RELEASE_RETURN.
*  REFRESH IT_RELEASE_RETURN.
*
*  LOOP AT IT_OUT_LOG.
*    MOVE SY-TABIX TO L_TABIX.
**---// type check 'S' or 'E',
**---// 'W' mean is data success for transfer vaatz, but not release.
*    IF IT_OUT_LOG-TYPE NE 'W'.
*      READ TABLE IT_RETURN WITH KEY BANFN = IT_OUT_LOG-BANFN
*                                    BNFPO = IT_OUT_LOG-BNFPO.
*
*      IF SY-SUBRC = 0.
*
**---// data transfer vaatz success check and release
**        IF IT_OUT_LOG-KNTTP ='F' AND IT_RETURN-TYPE = 'S'.
*        READ TABLE IT_TAB WITH KEY BANFN = IT_OUT_LOG-BANFN
*                                   BNFPO = IT_OUT_LOG-BNFPO
*                                   KNTTP = 'F'.
*
*        IF SY-SUBRC EQ 0.
**---// release BAPI function
*          PERFORM BAPI_RELEASE USING IT_OUT_LOG-BANFN
*                                     IT_OUT_LOG-BNFPO
*                                     IT_TAB-FRGST
*                            CHANGING IT_OUT_LOG-TYPE
*                                     IT_OUT_LOG-MESSAGE.
**---// Update Table EBAN to Enhancement's field (message type)
*          UPDATE EBAN SET    ZZTYPE = IT_OUT_LOG-TYPE
*                      WHERE  BANFN  = IT_OUT_LOG-BANFN
*                        AND  BNFPO  = IT_OUT_LOG-BNFPO.
*          COMMIT WORK AND WAIT.
*
*        ELSE.
*          IT_OUT_LOG-TYPE = IT_RETURN-TYPE.
*          UPDATE EBAN SET   ZZTYPE = IT_OUT_LOG-TYPE
*                      WHERE  BANFN = IT_OUT_LOG-BANFN
*                        AND  BNFPO = IT_OUT_LOG-BNFPO.
*          COMMIT WORK AND WAIT.
*        ENDIF.
*      ENDIF.
*    ELSE.
**---// release BAPI function
*      PERFORM BAPI_RELEASE USING IT_OUT_LOG-BANFN
*                                 IT_OUT_LOG-BNFPO
*                                 IT_TAB-FRGST
*                        CHANGING IT_OUT_LOG-TYPE
*                                 IT_OUT_LOG-MESSAGE.
*
**---// Update Table EBAN to Enhancement's field (message type)
*      UPDATE EBAN SET    ZZTYPE = IT_OUT_LOG-TYPE
*                  WHERE  BANFN  = IT_OUT_LOG-BANFN
*                    AND  BNFPO  = IT_OUT_LOG-BNFPO.
*
*      COMMIT WORK AND WAIT.
*    ENDIF.
*
*    MOVE: V_OUTBOUND_FLAG TO IT_OUT_LOG-FLAG,
*          SY-DATUM        TO IT_OUT_LOG-TRAN_DATE,
*          SY-UZEIT        TO IT_OUT_LOG-TRAN_TIME.
*
*    MODIFY IT_OUT_LOG INDEX L_TABIX.
*    CLEAR L_TABIX.
*
*  ENDLOOP.
**---// data save for log table
*  INSERT ZTMM_IF005 FROM TABLE IT_OUT_LOG.
*  COMMIT WORK AND WAIT.
*
*ENDFORM.                    " GET_RELEASE
*&---------------------------------------------------------------------*
*&      Form  BAPI_RELEASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_LOG_BANFN  text
*      -->P_IT_TAB_FRGST  text
*      -->P_CHANGEING  text
*      -->P_V_STATUS_NEW  text
*      -->P_V_INDICATOR_NEW  text
*      -->P_IT_RELEASE_RETURN  text
*----------------------------------------------------------------------*
*FORM BAPI_RELEASE  USING    P_BANFN
*                            P_BNFPO
*                            P_FRGST
*                CHANGING
*                            P_TYPE
*                            P_MESSAGE.
**---// Get release success message save internal table.
*  DATA : L_STATUS_NEW    LIKE BAPIMMPARA-REL_STATUS.
*  DATA : L_INDICATOR_NEW LIKE BAPIMMPARA-REL_IND.
*
*  CLEAR : L_INDICATOR_NEW, L_INDICATOR_NEW,
*          IT_RELEASE_RETURN.
*  REFRESH IT_RELEASE_RETURN.
*
*  CALL FUNCTION 'BAPI_REQUISITION_RELEASE'
*       EXPORTING
*            NUMBER                 = P_BANFN
*            REL_CODE               = P_FRGST
*            ITEM                   = P_BNFPO
*            USE_EXCEPTIONS         = 'X'
*       IMPORTING
*            REL_STATUS_NEW         = L_STATUS_NEW
*            REL_INDICATOR_NEW      = L_INDICATOR_NEW
*       TABLES
*            RETURN                 = IT_RELEASE_RETURN
*       EXCEPTIONS
*            AUTHORITY_CHECK_FAIL   = 1
*            REQUISITION_NOT_FOUND  = 2
*            ENQUEUE_FAIL           = 3
*            PREREQUISITE_FAIL      = 4
*            RELEASE_ALREADY_POSTED = 5
*            RESPONSIBILITY_FAIL    = 6
*            OTHERS                 = 7.
*
**  IF SY-SUBRC <> 0.
**    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**  ENDIF.
*
*  IF L_STATUS_NEW    = 'X' AND
*     L_INDICATOR_NEW = '5' AND
*     IT_RELEASE_RETURN[] IS INITIAL.
*
*    MOVE: 'S'                       TO P_TYPE,
*          TEXT-004                  TO P_MESSAGE.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*         EXPORTING
*              WAIT = 'X'.
*
**---// release error check
*  ELSE.
*    READ TABLE IT_RELEASE_RETURN WITH KEY TYPE = 'E'.
*    IF SY-SUBRC = 0.
**---// release trasnfer error for type change 'E' to 'W'
*      MOVE: 'W'                       TO P_TYPE,
*            IT_RELEASE_RETURN-MESSAGE TO P_MESSAGE.
*    ENDIF.
*  ENDIF.
*ENDFORM.                    " BAPI_RELEASE
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM submit_data .

  CLEAR   r_number.
  REFRESH r_number.

  MOVE: 'I'      TO r_number-sign,
        'EQ'     TO r_number-option,
        v_number TO r_number-low,
        space    TO r_number-high.

  APPEND r_number.
  CLEAR  r_number.

  SUBMIT zmmr_if001 AND RETURN
         WITH s_serno       IN r_number
         WITH p_outbnd      EQ 'X'
         WITH p_all         EQ ' '.

ENDFORM.                    " SUBMIT_DATA
*&---------------------------------------------------------------------*
*&      Form  NUMBER_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM number_range.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '04'
      object                  = 'ZSERNO3'
      quantity                = '1'
    IMPORTING
      number                  = v_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      OTHERS                  = 7.

ENDFORM.                    " NUMBER_RANGE
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
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM layout.
  CLEAR gs_layout.
  gs_layout-cwidth_opt = 'X'.  "??? ???
  gs_layout-zebra      = 'X'.
ENDFORM.                    " LAYOUT
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

  IF NOT ra_banfn IS INITIAL.
    UPDATE eban SET zztype = gt_zztype
              WHERE banfn IN ra_banfn.
    COMMIT WORK AND WAIT.
  ENDIF.
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

*-- P/R number condition
  REFRESH ra_banfn.
  range_macro ra_banfn gf_banfn gt_banfn.

ENDFORM.                    " CALL_RANGE
*&---------------------------------------------------------------------*
*&      Form  get_recipient_id
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_recipient_id.
  CLEAR: lv_ernam.
ENDFORM.                    " get_recipient_id

*---------------------------------------------------------------------*
*       FORM send_email                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM send_email.
  DATA:     it_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
            it_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
            it_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
            it_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
            gd_cnt TYPE i,
            gd_sent_all(1) TYPE c,
            gd_doc_data LIKE sodocchgi1,
            gd_error TYPE sy-subrc,
            psubject(40) TYPE c VALUE  'PR OUTBOUND Error ',
            p_email(40)   TYPE c VALUE 'pshrewsbury@hmmausa.com' .
*           p_email(40)   type c value 'RF_GR_ERROR' .

  DATA:   it_message TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
                  WITH HEADER LINE.

* Populate Message Body .

  APPEND 'PR Outbound Data Transfer to VAATZ System Failed'
                      TO it_message.

  APPEND '------------------------------------------' TO it_message.
  APPEND  'RFC Failure ' TO it_message.
  APPEND '------------------------------------------' TO it_message.
*  loop at it_error.
  MOVE  '' TO it_message(10).
  MOVE  '/'            TO it_message+11(1).
  MOVE  '' TO it_message+13(20).
  MOVE   ''   TO it_message+35(80).
  APPEND it_message. CLEAR it_message.
*  endloop.

* Fill the document data.
  gd_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
  gd_doc_data-obj_langu = sy-langu.
  gd_doc_data-obj_name  = 'SAPRPT'.
  gd_doc_data-obj_descr = psubject.
  gd_doc_data-sensitivty = 'F'.

* Describe the body of the message
  CLEAR it_packing_list.
  REFRESH it_packing_list.
  it_packing_list-transf_bin = space.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 0.
  it_packing_list-body_start = 1.
  DESCRIBE TABLE it_message LINES it_packing_list-body_num.
  it_packing_list-doc_type = 'RAW'.
  APPEND it_packing_list.

* Add the recipients email address
  CLEAR it_receivers.
  REFRESH it_receivers.
*  it_receivers-receiver = p_email.
*  it_receivers-rec_type = 'U'.
*  it_receivers-com_type = 'INT'.
*  it_receivers-notif_del = 'X'.
*  it_receivers-notif_ndel = 'X'.

  it_receivers-receiver = p_email.
  it_receivers-rec_type = 'C'.
  it_receivers-com_type = ''.

  APPEND it_receivers.

* Call the FM to send the message to SAPMAIL
* asynchronously
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
*  STARTING NEW TASK  'T1'
       EXPORTING
            document_data              = gd_doc_data
*            PUT_IN_OUTBOX              = 'X'
            COMMIT_WORK                = 'X'
*       IMPORTING
*            sent_to_all                = gd_sent_all
       TABLES
            packing_list               = it_packing_list
            contents_txt               = it_message
            receivers                  = it_receivers
       EXCEPTIONS
            too_many_receivers         = 1
            document_not_sent          = 2
            document_type_not_exist    = 3
            operation_no_authorization = 4
            parameter_error            = 5
            x_error                    = 6
            enqueue_error              = 7
            OTHERS                     = 8.
  IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.

  ENDIF.

ENDFORM.                    " send_email
