************************************************************************
* Program Name      : ZIPP202I_ZTPPVS
* Author            : DongYeop, Han
* Creation Date     : 2003.11.12.
* Specifications By : DongYeop, Han
* Pattern           : 1.1
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Transfer of Production Spec from PP to ALC
*                     (ztppvs)
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  zipp202i_ztppvs   NO STANDARD PAGE HEADING
                          LINE-SIZE 1023
                          MESSAGE-ID zdpp.

************************************************************************
*              D A T A     A R E A                                     *
************************************************************************
TABLES:ztpp_spec.

DATA : BEGIN OF it_spec OCCURS 0,
         worder   LIKE  ztpp_spec-worder,    "W/O HEADER
         extc     LIKE  ztpp_spec-extc,      "EXT COLOR
         intc     LIKE  ztpp_spec-intc,      "INT COLOR
         mark     LIKE  ztpp_spec-mark.      "IR/RP/DL
DATA : END OF it_spec.

DATA : it_wohd      LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
       it_wocl      LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
       it_char      LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
       it_specvs    LIKE TABLE OF ztppvs    WITH HEADER LINE,
       it_zsppvs    LIKE TABLE OF zsppvs    WITH HEADER LINE,
       it_ztppvs    LIKE TABLE OF ztppvs    WITH HEADER LINE,
       it_zsppTECH  LIKE TABLE OF zsppTECH  WITH HEADER LINE,
       it_ztppTECH  LIKE TABLE OF ztpp_TECH WITH HEADER LINE.

DATA : BEGIN OF it_message_s OCCURS 0,
       funcname(20),
       msgtxt(100).
DATA : END OF it_message_s.

DATA : it_bdcdata    LIKE TABLE OF bdcdata  WITH HEADER LINE.

DATA : BEGIN OF it_bdce_s OCCURS 0,
       msg(100).
DATA : END OF it_bdce_s.

DATA: BEGIN OF it_status OCCURS 0,
        fcode LIKE rsmpe-func,
      END OF it_status.

* Field Symbol
FIELD-SYMBOLS : <219>,       " for Workorder 219 Code
                <alc_u>,     " for Workorder Head Unique Part
                <alc_c>.     " for Workorder Color Color Part
FIELD-SYMBOLS : <hpcq>.      " for HPCC Color Part
FIELD-SYMBOLS : <hpcp>.      " for HPCC Unique Part
FIELD-SYMBOLS : <hpcb>.      " for HPCC Body Part
FIELD-SYMBOLS : <tab>.

*RANGES
RANGES r_mark FOR ztpp_spec-mark.

*WORK AREA
DATA : wa_matnr_cl           LIKE  mara-matnr,
       wa_matnr_hd           LIKE  mara-matnr,
       wa_model              LIKE  conf_out-atwrt.

DATA : wa_ir_vs              LIKE  sy-tabix,
       wa_rp_vs              LIKE  sy-tabix,
       wa_dl_vs              LIKE  sy-tabix.

DATA: WA_FLAG,                          " No Data check flag.
      wa_mode,                          "BDC MODE
      wa_active(1).                     "ACTIVE CHECKBOX (Y:'X',N:'')
DATA: wa_option_ds   LIKE ctu_params.   "BDC OPTION

*CONSTANTS
CONSTANTS: c_mode    VALUE  'A',
           c_dest(10) VALUE 'WMPP01'.   "Outbound Interface Destination

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
POSITION 8.
PARAMETERS: c_trans AS CHECKBOX .
SELECT-OPTIONS: S_DATE FOR SY-DATUM NO-EXTENSION.
SELECTION-SCREEN: BEGIN OF LINE,
                  POSITION 8.
PARAMETERS : c_ir AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: COMMENT 12(10)  text-001  FOR FIELD c_ir,
                  POSITION 28.
PARAMETERS : c_rp AS CHECKBOX.
SELECTION-SCREEN: COMMENT 34(10)  text-002  FOR FIELD c_rp,
                  POSITION 50.
PARAMETERS : c_dl AS CHECKBOX.
SELECTION-SCREEN: COMMENT 56(10)  text-003  FOR FIELD c_dl,
                  END OF LINE,
                  END OF BLOCK b1.

************************************************************************
*              INITIALIZATION                                          *
************************************************************************
INITIALIZATION.

************************************************************************
*              AT SELECTION SCREEN                                     *
************************************************************************
AT SELECTION-SCREEN.

************************************************************************
*              START-OF-SELECTION PROCESSING                           *
************************************************************************
START-OF-SELECTION.
  PERFORM data_select.
  PERFORM data_join.
  PERFORM write_or_transfer.
  SET PF-STATUS 'SUB_LIST'.

END-OF-SELECTION.

************************************************************************
*              TOP-OF-PAGE                                             *
************************************************************************
TOP-OF-PAGE.
  PERFORM list_head.

************************************************************************
*             AT USER-COMMAND                                          *
************************************************************************
AT USER-COMMAND.
  PERFORM user_commad.

************************************************************************
*             TOP-OF-PAGE DURING LINE-SELECTION                        *
************************************************************************
TOP-OF-PAGE DURING LINE-SELECTION.
  IF sy-ucomm NE 'TRANS'.
    CASE sy-lsind.
      WHEN '1'.
        PERFORM list_head2.
      WHEN '2'.
        PERFORM list_head3.
    ENDCASE.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM data_select.
  clear: wa_flag .
  PERFORM progress_indicator USING 'CREATING SPEC DATA ....'.
  PERFORM ranges_mark.
* DELETE FROM ZTPP_SPEC WHERE OPDATE NE L_DATA.
*  SELECT WORDER EXTC INTC MARK
*     INTO TABLE IT_SPEC
*     FROM ZTPP_SPEC
*     WHERE MARK IN R_MARK
*       and opdate = L_DATA.

  SORT it_spec.
  IF it_spec[] IS INITIAL.
    MESSAGE e001 WITH text-004.
  ENDIF.
ENDFORM.                    " DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  RANGES_MARK
*&---------------------------------------------------------------------*
FORM ranges_mark.
  DATA: l_data                LIKE sy-datum.

  CLEAR: l_data, IT_SPEC, IT_SPEC[].
  IF c_ir = 'X'.
    r_mark-sign   = 'I'.      r_mark-option = 'EQ'.
    r_mark-low    = 'I'.      APPEND r_mark.
    SELECT worder extc intc mark
       INTO TABLE it_spec
       FROM ztpp_spec
       WHERE mark IN r_mark
         AND opdate = l_data.
  ENDIF.

  IF c_rp = 'X'.
    " Read Data in the Table ZTPPVS for the IR Data betwwen Date range..
    r_mark-sign   = 'I'.      r_mark-option = 'EQ'.
    r_mark-low    = 'R'.      APPEND r_mark.
    SELECT *  INTO TABLE it_specvs
       FROM ztppvs
       WHERE zsdat in s_date.

    loop at it_specvs  where flg = 'IR' .
       it_spec-worder  = it_specvs-matnr(14).
       it_spec-extc    = it_specvs-matnr+14(02).
       it_spec-intc    = it_specvs-matnr+16(02).
       it_spec-mark    = 'I'                   .
       append it_spec.
    endloop.
  ENDIF.

  IF c_dl = 'X'.
    r_mark-sign   = 'I'.      r_mark-option = 'EQ'.
    r_mark-low    = 'D'.      APPEND r_mark.
    SELECT worder extc intc mark
       INTO TABLE it_spec
       FROM ztpp_spec
       WHERE mark IN r_mark
         AND opdate = l_data.
  ENDIF.
ENDFORM.                    " RANGES_MARK

*&---------------------------------------------------------------------*
*&      Form  DATA_JOIN
*&---------------------------------------------------------------------*
FORM data_join.
  DATA: l_worder             LIKE it_spec-worder.

  LOOP AT it_spec.
    CLEAR : wa_matnr_cl, wa_matnr_hd, IT_ZSPPTECH.
    CONCATENATE it_spec-worder it_spec-extc it_spec-intc
                                         INTO wa_matnr_cl.
    IF l_worder NE it_spec-worder .
      wa_matnr_hd = it_spec-worder.
      CLEAR : it_wohd, it_wohd[].
      PERFORM ftp_handling_master TABLES it_wohd
                                  USING wa_matnr_hd
                                        'R'.
    ENDIF.

    CLEAR : it_wocl, it_wocl[].
    PERFORM ftp_handling_master TABLES it_wocl
                                USING wa_matnr_cl
                                       'R'.
*    ENDAT.
    IF it_spec-mark = 'I'.
      IT_ZSPPTECH-FLG = it_zsppvs-flg = 'IR'.
    ELSEIF it_spec-mark = 'R'.
      IT_ZSPPTECH-FLG = it_zsppvs-flg = 'RP'.
    ELSEIF it_spec-mark = 'D'.
      IT_ZSPPTECH-FLG = it_zsppvs-flg = 'DL'.
    ENDIF.
    PERFORM move_to_it_zsppvs.
    IF NOT it_zsppvs-flg IS INITIAL AND NOT it_zsppvs-matnr IS INITIAL.
      CASE it_zsppvs-flg.
        WHEN 'IR'.
          wa_ir_vs = wa_ir_vs + 1.
        WHEN 'RP'.
          wa_rp_vs = wa_rp_vs + 1.
        WHEN 'DL'.
          wa_dl_vs = wa_dl_vs + 1.
      ENDCASE.
      APPEND it_zsppvs.   CLEAR it_zsppvs.
      APPEND IT_ZSPPTECH. CLEAR IT_ZSPPTECH.
    ENDIF.
  ENDLOOP.
  SORT it_zsppvs.  SORT IT_ZSPPTECH.
  IF it_zsppvs[] IS INITIAL.
    MESSAGE W001 WITH text-005.
    WA_FLAG  = 'X' .
  ENDIF.
ENDFORM.                    " DATA_JOIN
*&---------------------------------------------------------------------*
*&      Form  FTP_HANDLING_MASTER
*&---------------------------------------------------------------------*
FORM ftp_handling_master TABLES   itab
                         USING    p_matnr
                                  p_mode.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = p_matnr
            mode         = p_mode
            ctype        = '001'
            display      = 'X'
       TABLES
            val_table    = itab
       EXCEPTIONS
            no_data      = 1
            error_mode   = 2
            error_object = 3
            OTHERS       = 4.

  IF sy-subrc <> 0.
    it_message_s-funcname = 'Z_FPP_HANDLING_MASTER'.
    it_message_s-msgtxt   = 'Classification Update Error'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MOVE_TO_IT_ZSPPVS
*&---------------------------------------------------------------------*
FORM move_to_it_zsppvs.
  DATA : l_ordr   LIKE  ztpp_pmt07gb-ordr,
         L_NAT(3) TYPE  C                ,
         L_DEA(2) TYPE  C                ,
         l_dist   LIKE  ztpp_pmt07gb-dist,
         l_extc   LIKE  ztpp_pmt07gb-extc,
         l_intc   LIKE  ztpp_pmt07gb-intc,
         l_date   LIKE  conf_out-atwrt.

  CLEAR: l_ordr, L_NAT, L_DEA, l_dist, l_extc, l_intc, l_date.

  PERFORM read_wohd_wocl USING 'WOHD' 'P_MODEL'  CHANGING wa_model.
  PERFORM read_wohd_wocl USING 'WOCL' 'P_WO_SER' CHANGING l_ordr.

  PERFORM READ_WOHD_WOCL USING 'WOCL' 'P_NATION' CHANGING L_NAT .
  PERFORM READ_WOHD_WOCL USING 'WOCL' 'P_DEALER' CHANGING L_DEA .
  PERFORM read_wohd_wocl USING 'WOCL' 'COLORINT' CHANGING l_intc.
  PERFORM read_wohd_wocl USING 'WOCL' 'COLOREXT' CHANGING l_extc.
  PERFORM read_wohd_wocl USING 'WOCL' 'P_MI'    CHANGING it_zsppvs-bmdl.
  PERFORM read_wohd_wocl USING 'WOCL' 'P_OCN'   CHANGING it_zsppvs-ocnn.
  PERFORM read_wohd_wocl USING 'WOCL' 'P_VERSION'
                         CHANGING it_zsppvs-pver. "VERSION
  PERFORM read_wohd_wocl USING 'WOCL' 'P_DESTINATION_CODE'
                         CHANGING it_zsppvs-dist. "DISTRIBUTOR
  PERFORM read_wohd_wocl USING 'WOCL' 'P_REGION_PORT'
                         CHANGING it_zsppvs-t219_019.
  PERFORM read_wohd_wocl USING 'WOCL' 'P_WO_CREATE_DATE1'
                         CHANGING l_date.
  it_zsppvs-p_wo_create_date = l_date(8).
  it_zsppvs-p_wo_create_time = l_date+8(6).
*-----> Work Order Color
 IF l_ordr IS INITIAL OR L_NAT IS INITIAL OR
    l_extc IS INITIAL OR L_DEA IS INITIAL.
*                        it_zsppvs-dist IS INITIAL OR l_extc IS INITIAL.
    it_zsppvs-flg = ' '.
  ELSE.
    CONCATENATE l_ordr L_NAT L_DEA l_extc l_intc INTO it_zsppvs-matnr.
*                       it_zsppvs-dist l_extc l_intc
*                                      INTO it_zsppvs-matnr.
    PERFORM read_219_alc USING 'WOHD'.
    PERFORM read_219_alc USING 'WOCL'.
  ENDIF.
  it_zsppvs-pack             = it_zsppvs-matnr+3(2).

*-----> Tech. Information
  IT_ZSPPTECH-MATNR = it_zsppvs-matnr.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_001'
                         CHANGING it_zsppTECH-TECH_001.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_002'
                         CHANGING it_zsppTECH-TECH_002.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_003'
                         CHANGING it_zsppTECH-TECH_003.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_004'
                         CHANGING it_zsppTECH-TECH_004.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_005'
                         CHANGING it_zsppTECH-TECH_005.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_006'
                         CHANGING it_zsppTECH-TECH_006.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_007'
                         CHANGING it_zsppTECH-TECH_007.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_008'
                         CHANGING it_zsppTECH-TECH_008.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_009'
                         CHANGING it_zsppTECH-TECH_009.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_010'
                         CHANGING it_zsppTECH-TECH_010.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_011'
                         CHANGING it_zsppTECH-TECH_011.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_012'
                         CHANGING it_zsppTECH-TECH_012.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_013'
                         CHANGING it_zsppTECH-TECH_013.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_014'
                         CHANGING it_zsppTECH-TECH_014.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_015'
                         CHANGING it_zsppTECH-TECH_015.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_016'
                         CHANGING it_zsppTECH-TECH_016.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_017'
                         CHANGING it_zsppTECH-TECH_017.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_018'
                         CHANGING it_zsppTECH-TECH_018.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_019'
                         CHANGING it_zsppTECH-TECH_019.
  PERFORM read_wohd_wocl USING 'WOHD' 'P_TECH_SPEC_020'
                         CHANGING it_zsppTECH-TECH_020.
ENDFORM.                    " MOVE_TO_IT_ZSPPVS
*&---------------------------------------------------------------------*
*&      Form  READ_WOHD_WOCL
*&---------------------------------------------------------------------*
FORM read_wohd_wocl USING    p_ind p_atnam
                    CHANGING p_atwrt.
  CASE p_ind.
    WHEN 'WOHD'.
      READ TABLE it_wohd WITH KEY atnam = p_atnam.
      IF sy-subrc EQ 0.
        p_atwrt = it_wohd-atwrt.
      ELSE.
        CLEAR: P_ATWRT.
      ENDIF.
    WHEN 'WOCL'.
      READ TABLE it_wocl WITH KEY atnam = p_atnam.
      IF sy-subrc EQ 0.
        p_atwrt = it_wocl-atwrt.
      ELSE.
        CLEAR: P_ATWRT.
      ENDIF.
  ENDCASE.
ENDFORM.                    " READ_WOHD_WOCL
*&---------------------------------------------------------------------*
*&      Form  READ_219_ALC
*&---------------------------------------------------------------------*
FORM read_219_alc USING p_ind.
  DATA : l_no1_nb(1)    TYPE  n,
         l_no2_nb(2)    TYPE  n,
         l_no3_nb(3)    TYPE  n.
  DATA : l_219_fname(40),
         l_alc_u_fname(40),
         l_alc_c_fname(40).
  DATA : l_219_search(40),
         l_alc_u_search(40),
         l_alc_c_search(40).
  DATA : l_num          TYPE  i.

  IF p_ind EQ 'WOHD'.
    l_num = 219.
  ELSEIF p_ind EQ 'WOCL'.
    l_num = 50.
  ENDIF.

  DO l_num TIMES.
    CLEAR : l_219_search, l_alc_u_search, l_alc_c_search.
    IF p_ind EQ 'WOHD'.
      l_no3_nb = sy-index.
      CONCATENATE 'IT_ZSPPVS-P_219_'   l_no3_nb
                                       INTO l_219_fname.
      ASSIGN (l_219_fname)    TO <219>.

      IF l_no3_nb LE '009'.
        l_no1_nb = l_no3_nb.
        CONCATENATE 'P_219_'    l_no1_nb  INTO l_219_search.
        CONCATENATE 'P_ALC_U_'  l_no1_nb  INTO l_alc_u_search.
      ELSEIF l_no3_nb GE '010' AND l_no3_nb LE '099'.
        l_no2_nb = l_no3_nb.
        CONCATENATE 'P_219_'    l_no2_nb  INTO l_219_search.
        CONCATENATE 'P_ALC_U_'  l_no2_nb  INTO l_alc_u_search.
      ELSE.
        CONCATENATE 'P_219_'    l_no3_nb  INTO l_219_search.
        CONCATENATE 'P_ALC_U_'  l_no3_nb  INTO l_alc_u_search.
      ENDIF.

*-----> 219 CODE
      PERFORM read_wohd_wocl USING p_ind l_219_search
                             CHANGING <219>.
      IF l_no3_nb LE '200'.
        CONCATENATE 'IT_ZSPPVS-P_ALC_U_' l_no3_nb
                                         INTO l_alc_u_fname.
        ASSIGN (l_alc_u_fname)  TO <alc_u>.
        PERFORM read_wohd_wocl USING p_ind l_alc_u_search
                               CHANGING <alc_u>.
      ENDIF.

*-----> 219 VALUE
      IF l_no3_nb EQ '002'.
        SELECT SINGLE clnm
                      INTO it_zsppvs-t219_002
                      FROM ztbm_abxopvdt
                      WHERE carx   EQ wa_model(2)
                        AND clno   EQ l_no3_nb
                        AND valu   EQ it_zsppvs-p_219_002.

      ELSEIF l_no3_nb EQ '003'.
        SELECT SINGLE clnm
                      INTO it_zsppvs-t219_003
                      FROM ztbm_abxopvdt
                      WHERE carx   EQ wa_model(2)
                        AND clno   EQ l_no3_nb
                        AND valu   EQ it_zsppvs-p_219_003.
      ELSEIF l_no3_nb EQ '004'.
        SELECT SINGLE clnm
                      INTO it_zsppvs-t219_004
                      FROM ztbm_abxopvdt
                      WHERE carx   EQ wa_model(2)
                        AND clno   EQ l_no3_nb
                        AND valu   EQ it_zsppvs-p_219_004.

      ELSEIF l_no3_nb EQ '005'.
        SELECT SINGLE clnm
                      INTO it_zsppvs-t219_005
                      FROM ztbm_abxopvdt
                      WHERE carx   EQ wa_model(2)
                        AND clno   EQ l_no3_nb
                        AND valu   EQ it_zsppvs-p_219_005.

      ELSEIF l_no3_nb EQ '019'.
        SELECT SINGLE clnm
                      INTO it_zsppvs-t219_019
                      FROM ztbm_abxopvdt
                      WHERE carx   EQ wa_model(2)
                        AND clno   EQ l_no3_nb
                        AND valu   EQ it_zsppvs-p_219_019.

      ELSEIF l_no3_nb EQ '219'.
        SELECT SINGLE clnm
                      INTO it_zsppvs-t219_219
                      FROM ztbm_abxopvdt
                      WHERE carx   EQ wa_model(2)
                        AND clno   EQ l_no3_nb
                        AND valu   EQ it_zsppvs-p_219_219.
      ENDIF.

    ELSEIF p_ind EQ 'WOCL'.
      l_no3_nb = sy-index.
      CONCATENATE 'IT_ZSPPVS-P_ALC_C_' l_no3_nb
                                       INTO l_alc_c_fname.
      ASSIGN (l_alc_c_fname)  TO <alc_c>.
      IF l_no3_nb LE '009'.
        l_no1_nb = l_no3_nb.
        CONCATENATE 'P_ALC_C_'  l_no1_nb  INTO l_alc_c_search.
      ELSEIF l_no3_nb GE '010' AND l_no3_nb LE '099'.
        l_no2_nb = l_no3_nb.
        CONCATENATE 'P_ALC_C_'  l_no2_nb  INTO l_alc_c_search.
      ENDIF.
      PERFORM read_wohd_wocl USING p_ind l_alc_c_search
                             CHANGING <alc_c>.
    ENDIF.
  ENDDO.
ENDFORM.                    " READ_219_ALC
*&---------------------------------------------------------------------*
*&      Form  WRITE_OR_TRANSFER
*&---------------------------------------------------------------------*
FORM write_or_transfer.
  check WA_FLAG  = space .
  IF c_trans = 'X'.
    PERFORM transfer_to_alc.
  ELSE.
    PERFORM data_write.
  ENDIF.
ENDFORM.                    " WRITE_OR_TRANSFER
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_TO_ALC
*&---------------------------------------------------------------------*
FORM transfer_to_alc.
  DATA: LT_zsppvs         LIKE TABLE OF it_zsppvs      WITH HEADER LINE,
        LT_VALS           LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE,
        L_WORDER          LIKE MARA-MATNR,
        L_ATINN1          LIKE AUSP-ATINN,
        L_ATINN2          LIKE AUSP-ATINN,
        L_ATFLV           LIKE AUSP-ATFLV,
        l_matnr           LIKE mara-matnr,
        l_msgtxt(100).

  PERFORM progress_indicator USING 'TRANSFER FROM SPEC TO ALC'.
*----> EAI TRANSFER
  IF NOT it_zsppvs[] IS INITIAL.
    CALL FUNCTION 'Z_FPP_SET_ZTPPVS'
      DESTINATION c_dest
      EXPORTING
        ir            = wa_ir_vs
        rp            = wa_rp_vs
        dl            = wa_dl_vs
      TABLES
        i_zsppvs      = it_zsppvs
        I_ZSPPTECH    = IT_ZSPPTECH
      EXCEPTIONS
        communication_failure = 1  MESSAGE l_msgtxt
        system_failure        = 2  MESSAGE l_msgtxt.

    IF sy-subrc <> 0.
      it_message_s-funcname = 'Z_FPP_SET_ZTPPVS'.
      it_message_s-msgtxt   = l_msgtxt.
      APPEND it_message_s. CLEAR it_message_s.
    ELSE.
*     DELETE FROM ztpp_spec CLIENT SPECIFIED WHERE mandt = sy-mandt.
      COMMIT WORK.
      SELECT SINGLE ATINN INTO L_ATINN1
        FROM CABN
       WHERE ATNAM = 'P_UPDATE_ALC_DATE1'.

      SELECT SINGLE ATINN INTO L_ATINN2
        FROM CABN
       WHERE ATNAM = 'P_UPDATE_ALC_DATE2'.
*      PERFORM SET_MODE.
*      LOOP AT IT_ZSPPVS." WHERE ZZRET EQ 'S'.
*        PERFORM GENERATE_BDC_DATA.
*        PERFORM CALL_TRANSACTION.
*      ENDLOOP.
      LT_zsppvs[] = IT_ZSPPVS[].
      LOOP AT it_zsppvs WHERE zzret EQ 'S'.
        REFRESH it_char.
        CLEAR it_char.
        CASE it_zsppvs-flg.
          WHEN 'IR'.
            it_char-atnam = 'P_UPDATE_ALC_DATE1'.
            it_char-atwrt = sy-datum.
            APPEND it_char. CLEAR it_char.
            it_char-atnam = 'P_HPC_STATUS'.
            it_char-atwrt = 'Y'.
            APPEND it_char. CLEAR it_char.
            PERFORM ftp_handling_master TABLES it_char
                                         USING it_zsppvs-matnr
                                               'W'.
            DELETE it_char INDEX 1 .
            l_matnr = it_zsppvs-matnr(14).
            PERFORM ftp_handling_master TABLES it_char
                                         USING l_matnr
                                               'W'.
            PERFORM error_text.
          WHEN 'DL'.
            it_char-atnam = 'P_UPDATE_ALC_DATE2'.
            it_char-atwrt = sy-datum.
            APPEND it_char. CLEAR it_char.
            PERFORM ftp_handling_master TABLES it_char
                                         USING it_zsppvs-matnr
                                               'W'.
            PERFORM error_text.
          WHEN 'RP'.
            it_char-atnam = 'P_UPDATE_ALC_DATE2'.
            it_char-atwrt = sy-datum.
            APPEND it_char. CLEAR it_char.
            PERFORM ftp_handling_master TABLES it_char
                                         USING it_zsppvs-matnr
                                               'W'.
            PERFORM error_text.
        ENDCASE.
      ENDLOOP.

      " Update the Work-Order Header's Information..
      LOOP AT LT_zsppvs WHERE zzret NE 'S'.
        L_WORDER = LT_ZSPPVS-MATNR .
        DELETE LT_ZSPPVS WHERE MATNR = L_WORDER .
      ENDLOOP.

      SORT LT_ZSPPVS BY MATNR   .
      CLEAR: L_WORDER, LT_VALS[].
      APPEND LT_VALS            .
      LOOP AT LT_ZSPPVS       .
        IF L_WORDER = LT_ZSPPVS-MATNR(14).
           CONTINUE.
        ELSE.
           L_WORDER = LT_ZSPPVS-MATNR(14).
           SELECT SINGLE ATFLV INTO L_ATFLV
             FROM AUSP
            WHERE OBJEK = L_WORDER
              AND ATINN = L_ATINN2
              AND KLART = '001'   .
           IF SY-SUBRC = 0 AND L_ATFLV > 0.
              LT_VALS-ATNAM = 'P_UPDATE_ALC_DATE2' .
              LT_VALS-ATWRT =  sy-datum            .
              MODIFY LT_VALS  INDEX 1              .
           ELSE .
              LT_VALS-ATNAM = 'P_UPDATE_ALC_DATE1' .
              LT_VALS-ATWRT =  sy-datum            .
              MODIFY LT_VALS  INDEX 1              .
           ENDIF.

           CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
             EXPORTING
               object             = L_WORDER
               MODE               = 'W'
               CTYPE              = '001'
             tables
               val_table          = LT_VALS
             EXCEPTIONS
               NO_DATA            = 1
               ERROR_MODE         = 2
               ERROR_OBJECT       = 3
               ERROR_VALUE        = 4
               OTHERS             = 5 .

           IF sy-subrc <> 0.
           ENDIF.
        ENDIF.
      ENDLOOP.

      IF NOT it_ztppvs[] IS INITIAL.
        MODIFY ztppvs FROM TABLE it_ztppvs.
        IF sy-subrc = 0.
          it_message_s-funcname = 'ZTPPVS'.
          it_message_s-msgtxt   = 'Completed Update successfully'.
          APPEND it_message_s. CLEAR it_message_s.
        ELSE.
          ROLLBACK WORK.
          it_message_s-funcname = 'ZTPPVS'.
          it_message_s-msgtxt   = 'Failed Update '.
          APPEND it_message_s. CLEAR it_message_s.
        ENDIF.
      ELSE.
        it_message_s-funcname = 'ZTPPVS'.
        it_message_s-msgtxt   = 'It is no Return value'.
        APPEND it_message_s. CLEAR it_message_s.
      ENDIF.

      LOOP AT IT_ZSPPTECH.
        MOVE-CORRESPONDING IT_ZSPPTECH TO IT_ZTPPTECH.
        APPEND IT_ZTPPTECH.
      ENDLOOP.

      IF NOT it_ztppTECH[] IS INITIAL.
        MODIFY ztpp_TECH FROM TABLE it_ztppTECH.
        IF sy-subrc = 0.
          it_message_s-funcname = 'ZTPP_TECH'.
          it_message_s-msgtxt   = 'Completed Update successfully'.
          APPEND it_message_s. CLEAR it_message_s.
        ELSE.
          ROLLBACK WORK.
          it_message_s-funcname = 'ZTPP_TECH'.
          it_message_s-msgtxt   = 'Failed Update '.
          APPEND it_message_s. CLEAR it_message_s.
        ENDIF.
      ELSE.
        it_message_s-funcname = 'ZTPP_TECH'.
        it_message_s-msgtxt   = 'It is no Return value'.
        APPEND it_message_s. CLEAR it_message_s.
      ENDIF.
    ENDIF.
  ELSE.
    it_message_s-funcname = 'ZTPPVS'.
    it_message_s-msgtxt   = 'NO DATA'.
    APPEND it_message_s. CLEAR it_message_s.
  ENDIF.

  IF sy-ucomm = 'TRANS'.
    MOVE 'TRANS' TO it_status-fcode.
    APPEND it_status. CLEAR it_status.
    SET PF-STATUS 'SUB_LIST' EXCLUDING it_status.
    LOOP AT it_message_s.
      WRITE:/ it_message_s.
    ENDLOOP.
    LOOP AT it_bdce_s.
      WRITE:/ it_bdce_s.
    ENDLOOP.
  ELSE.
    EXPORT it_message_s
           it_bdce_s    TO MEMORY ID 'MEMO'.
  ENDIF.
ENDFORM.                    " TRANSFER_TO_ALC
*&---------------------------------------------------------------------*
*&      Form  ERROR_TEXT
*&---------------------------------------------------------------------*
FORM error_text.
  DATA l_msg  LIKE cfgnl-msglin.
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

*  MOVE : WA_MATNR_CL      TO  IT_ERROR-MATNR,
*         SY-MSGTY         TO  IT_ERROR-MSGTY.
  CASE sy-msgty.
    WHEN 'E' OR 'A' OR 'X' OR 'W'.
      MOVE l_msg   TO  it_bdce_s-msg.
      APPEND it_bdce_s. CLEAR it_bdce_s.
    WHEN OTHERS.   " 'I', 'S' :SUCCESS
      PERFORM update_ztable.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTABLE
*&---------------------------------------------------------------------*
FORM update_ztable.
  MOVE-CORRESPONDING it_zsppvs TO  it_ztppvs.
  MOVE sy-uname    TO  it_ztppvs-zuser.
  MOVE sy-datum    TO  it_ztppvs-zsdat.
  MOVE sy-uzeit    TO  it_ztppvs-zstim.
  APPEND it_ztppvs.
ENDFORM.                    " UPDATE_ZTABLE

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
FORM progress_indicator USING   p_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = p_text.
ENDFORM.                    " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  DATA_WRITE
*&---------------------------------------------------------------------*
FORM data_write.
  DATA:zebra.
*  PERFORM HEAD_LINE.
  LOOP AT it_zsppvs.
    zebra = sy-tabix MOD 2.
    IF zebra = 0.
      FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.
    WRITE AT:/1(4)   it_zsppvs-flg,
              6(18)  it_zsppvs-matnr,
              25(11) it_zsppvs-bmdl,
              37(4)  it_zsppvs-ocnn,
              41(8)  it_zsppvs-pver,
              50(12) it_zsppvs-t219_019,
              63(20) it_zsppvs-t219_002,
              84(16) it_zsppvs-t219_004,
             101(6)  it_zsppvs-t219_003,
             102(10) it_zsppvs-t219_005,
             113(4)  it_zsppvs-pack,
             118(20) it_zsppvs-t219_219,                    " (40)
             139(3)  it_zsppvs-p_219_001,
             143(3)  it_zsppvs-p_219_002,
             147(3)  it_zsppvs-p_219_003,
             151(3)  it_zsppvs-p_219_004,
             155(3)  it_zsppvs-p_219_005,
             159(3)  it_zsppvs-p_219_006,
             163(3)  it_zsppvs-p_219_007,
             167(3)  it_zsppvs-p_219_008,
             171(3)  it_zsppvs-p_219_009,
             175(3)  it_zsppvs-p_219_010,
             179(3)  it_zsppvs-p_219_011,
             183(3)  it_zsppvs-p_219_012,
             187(3)  it_zsppvs-p_219_013,
             191(3)  it_zsppvs-p_219_014,
             195(3)  it_zsppvs-p_219_015,
             199(3)  it_zsppvs-p_219_016,
             203(3)  it_zsppvs-p_219_017,
             207(3)  it_zsppvs-p_219_018,
             211(3)  it_zsppvs-p_219_019,
             215(3)  it_zsppvs-p_219_020,
             219(3)  it_zsppvs-p_219_021,
             223(3)  it_zsppvs-p_219_022,
             227(3)  it_zsppvs-p_219_023,
             231(3) it_zsppvs-p_219_024,
             235(3) it_zsppvs-p_219_025,
             239(3) it_zsppvs-p_219_026,
             243(3) it_zsppvs-p_219_027,
             247(3) it_zsppvs-p_219_028,
             251(3) it_zsppvs-p_219_029,
             255(3) it_zsppvs-p_219_030,
             259(3) it_zsppvs-p_219_031,
             263(3) it_zsppvs-p_219_032,
             267(3) it_zsppvs-p_219_033,
             271(3) it_zsppvs-p_219_034,
             275(3) it_zsppvs-p_219_035,
             279(3) it_zsppvs-p_219_036,
             283(3) it_zsppvs-p_219_037,
             287(3) it_zsppvs-p_219_038,
             291(3) it_zsppvs-p_219_039,
             295(3) it_zsppvs-p_219_040,
             299(3) it_zsppvs-p_219_041,
             303(3) it_zsppvs-p_219_042,
             307(3) it_zsppvs-p_219_043,
             311(3) it_zsppvs-p_219_044,
             315(3) it_zsppvs-p_219_045,
             319(3) it_zsppvs-p_219_046,
             323(3) it_zsppvs-p_219_047,
             327(3) it_zsppvs-p_219_048,
             331(3) it_zsppvs-p_219_049,
             335(3) it_zsppvs-p_219_050,
             339(3) it_zsppvs-p_219_051,
             343(3) it_zsppvs-p_219_052,
             347(3) it_zsppvs-p_219_053,
             351(3) it_zsppvs-p_219_054,
             355(3) it_zsppvs-p_219_055,
             359(3) it_zsppvs-p_219_056,
             363(3) it_zsppvs-p_219_057,
             367(3) it_zsppvs-p_219_058,
             371(3) it_zsppvs-p_219_059,
             375(3) it_zsppvs-p_219_060,
             379(3) it_zsppvs-p_219_061,
             383(3) it_zsppvs-p_219_062,
             387(3) it_zsppvs-p_219_063,
             391(3) it_zsppvs-p_219_064,
             395(3) it_zsppvs-p_219_065,
             399(3) it_zsppvs-p_219_066,
             403(3) it_zsppvs-p_219_067,
             407(3) it_zsppvs-p_219_068,
             411(3) it_zsppvs-p_219_069,
             415(3) it_zsppvs-p_219_070,
             419(3) it_zsppvs-p_219_071,
             423(3) it_zsppvs-p_219_072,
             427(3) it_zsppvs-p_219_073,
             431(3) it_zsppvs-p_219_074,
             435(3) it_zsppvs-p_219_075,
             439(3) it_zsppvs-p_219_076,
             443(3) it_zsppvs-p_219_077,
             447(3) it_zsppvs-p_219_078,
             451(3) it_zsppvs-p_219_079,
             455(3) it_zsppvs-p_219_080,
             459(3) it_zsppvs-p_219_081,
             463(3) it_zsppvs-p_219_082,
             467(3) it_zsppvs-p_219_083,
             471(3) it_zsppvs-p_219_084,
             475(3) it_zsppvs-p_219_085,
             479(3) it_zsppvs-p_219_086,
             483(3) it_zsppvs-p_219_087,
             487(3) it_zsppvs-p_219_088,
             491(3) it_zsppvs-p_219_089,
             495(3) it_zsppvs-p_219_090,
             499(3) it_zsppvs-p_219_091,
             503(3) it_zsppvs-p_219_092,
             507(3) it_zsppvs-p_219_093,
             511(3) it_zsppvs-p_219_094,
             515(3) it_zsppvs-p_219_095,
             519(3) it_zsppvs-p_219_096,
             523(3) it_zsppvs-p_219_097,
             527(3) it_zsppvs-p_219_098,
             531(3) it_zsppvs-p_219_099,
             535(3) it_zsppvs-p_219_100,
             539(3)  it_zsppvs-p_219_101,
             543(3)  it_zsppvs-p_219_102,
             547(3)  it_zsppvs-p_219_103,
             551(3)  it_zsppvs-p_219_104,
             555(3)  it_zsppvs-p_219_105,
             559(3)  it_zsppvs-p_219_106,
             563(3)  it_zsppvs-p_219_107,
             567(3)  it_zsppvs-p_219_108,
             571(3)  it_zsppvs-p_219_109,
             575(3)  it_zsppvs-p_219_110,
             579(3)  it_zsppvs-p_219_111,
             583(3)  it_zsppvs-p_219_112,
             587(3)  it_zsppvs-p_219_113,
             591(3)  it_zsppvs-p_219_114,
             595(3)  it_zsppvs-p_219_115,
             599(3)  it_zsppvs-p_219_116,
             603(3)  it_zsppvs-p_219_117,
             607(3)  it_zsppvs-p_219_118,
             611(3)  it_zsppvs-p_219_119,
             615(3)  it_zsppvs-p_219_120,
             619(3)  it_zsppvs-p_219_121,
             623(3)  it_zsppvs-p_219_122,
             627(3)  it_zsppvs-p_219_123,
             631(3)  it_zsppvs-p_219_124,
             635(3)  it_zsppvs-p_219_125,
             639(3)  it_zsppvs-p_219_126,
             643(3)  it_zsppvs-p_219_127,
             647(3)  it_zsppvs-p_219_128,
             651(3)  it_zsppvs-p_219_129,
             655(3)  it_zsppvs-p_219_130,
             659(3)  it_zsppvs-p_219_131,
             663(3)  it_zsppvs-p_219_132,
             667(3)  it_zsppvs-p_219_133,
             671(3)  it_zsppvs-p_219_134,
             675(3)  it_zsppvs-p_219_135,
             679(3)  it_zsppvs-p_219_136,
             683(3)  it_zsppvs-p_219_137,
             687(3)  it_zsppvs-p_219_138,
             691(3)  it_zsppvs-p_219_139,
             695(3)  it_zsppvs-p_219_140,
             699(3)  it_zsppvs-p_219_141,
             703(3)  it_zsppvs-p_219_142,
             707(3)  it_zsppvs-p_219_143,
             711(3)  it_zsppvs-p_219_144,
             715(3)  it_zsppvs-p_219_145,
             719(3)  it_zsppvs-p_219_146,
             723(3)  it_zsppvs-p_219_147,
             727(3)  it_zsppvs-p_219_148,
             731(3)  it_zsppvs-p_219_149,
             735(3)  it_zsppvs-p_219_150,
             739(3)  it_zsppvs-p_219_151,
             743(3)  it_zsppvs-p_219_152,
             747(3)  it_zsppvs-p_219_153,
             751(3)  it_zsppvs-p_219_154,
             755(3)  it_zsppvs-p_219_155,
             759(3)  it_zsppvs-p_219_156,
             763(3)  it_zsppvs-p_219_157,
             767(3)  it_zsppvs-p_219_158,
             771(3)  it_zsppvs-p_219_159,
             775(3)  it_zsppvs-p_219_160,
             779(3)  it_zsppvs-p_219_161,
             783(3)  it_zsppvs-p_219_162,
             787(3)  it_zsppvs-p_219_163,
             791(3)  it_zsppvs-p_219_164,
             795(3)  it_zsppvs-p_219_165,
             799(3)  it_zsppvs-p_219_166,
             803(3)  it_zsppvs-p_219_167,
             807(3)  it_zsppvs-p_219_168,
             811(3)  it_zsppvs-p_219_169,
             815(3)  it_zsppvs-p_219_170,
             819(3)  it_zsppvs-p_219_171,
             823(3)  it_zsppvs-p_219_172,
             827(3)  it_zsppvs-p_219_173,
             831(3)  it_zsppvs-p_219_174,
             835(3)  it_zsppvs-p_219_175,
             839(3)  it_zsppvs-p_219_176,
             843(3)  it_zsppvs-p_219_177,
             847(3)  it_zsppvs-p_219_178,
             851(3)  it_zsppvs-p_219_179,
             855(3)  it_zsppvs-p_219_180,
             859(3)  it_zsppvs-p_219_181,
             863(3)  it_zsppvs-p_219_182,
             867(3)  it_zsppvs-p_219_183,
             871(3)  it_zsppvs-p_219_184,
             875(3)  it_zsppvs-p_219_185,
             879(3)  it_zsppvs-p_219_186,
             883(3)  it_zsppvs-p_219_187,
             887(3)  it_zsppvs-p_219_188,
             891(3)  it_zsppvs-p_219_189,
             895(3)  it_zsppvs-p_219_190,
             899(3)  it_zsppvs-p_219_191,
             903(3)  it_zsppvs-p_219_192,
             907(3)  it_zsppvs-p_219_193,
             911(3)  it_zsppvs-p_219_194,
             915(3)  it_zsppvs-p_219_195,
             919(3)  it_zsppvs-p_219_196,
             923(3)  it_zsppvs-p_219_197,
             927(3)  it_zsppvs-p_219_198,
             931(3)  it_zsppvs-p_219_199,
             935(3)  it_zsppvs-p_219_200,
             939(3)  it_zsppvs-p_219_201,
             943(3)  it_zsppvs-p_219_202,
             947(3)  it_zsppvs-p_219_203,
             951(3)  it_zsppvs-p_219_204,
             955(3)  it_zsppvs-p_219_205,
             959(3)  it_zsppvs-p_219_206,
             963(3)  it_zsppvs-p_219_207,
             967(3)  it_zsppvs-p_219_208,
             971(3)  it_zsppvs-p_219_209,
             975(3)  it_zsppvs-p_219_210,
             979(3)  it_zsppvs-p_219_211,
             983(3)  it_zsppvs-p_219_212,
             987(3)  it_zsppvs-p_219_213,
             991(3)  it_zsppvs-p_219_214,
             995(3)  it_zsppvs-p_219_215,
             999(3)  it_zsppvs-p_219_216,
            1003(3)  it_zsppvs-p_219_217,
            1007(3)  it_zsppvs-p_219_218,
            1011(3)  it_zsppvs-p_219_219.

  ENDLOOP.

  SKIP 2.

  LOOP AT IT_ZTPPTECH.
    zebra = sy-tabix MOD 2.
    IF zebra = 0.
      FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.
    WRITE AT:/1(4)   it_zsppTECH-flg,
              6(18)  it_zsppTECH-matnr,
              25(20) it_zsppTECH-TECH_001,
              46(20) it_zsppTECH-TECH_002,
              67(20) it_zsppTECH-TECH_003,
              88(20) it_zsppTECH-TECH_004,
             109(20) it_zsppTECH-TECH_005,
             130(20) it_zsppTECH-TECH_006,
             151(20) it_zsppTECH-TECH_007,
             172(20) it_zsppTECH-TECH_008,
             193(20) it_zsppTECH-TECH_009,
             214(20) it_zsppTECH-TECH_010,
             235(20) it_zsppTECH-TECH_011,
             256(20) it_zsppTECH-TECH_012,
             277(20) it_zsppTECH-TECH_013,
             298(20) it_zsppTECH-TECH_014,
             319(20) it_zsppTECH-TECH_015,
             330(20) it_zsppTECH-TECH_016,
             351(20) it_zsppTECH-TECH_017,
             372(20) it_zsppTECH-TECH_018,
             393(20) it_zsppTECH-TECH_019,
             414(20) it_zsppTECH-TECH_020.
  ENDLOOP.

  SET LEFT SCROLL-BOUNDARY COLUMN 24.
ENDFORM.                    " DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  LSIT_HEAD
*&---------------------------------------------------------------------*
FORM list_head.
  FORMAT COLOR 5 INTENSIFIED OFF.
  WRITE AT:/1(4)     text-006,	
              6(18)  text-007,	
             25(11)  text-008,	
             37(4)   text-009,	
             41(8)   text-010,	
             50(12)  text-011,	
             63(20)  text-012,	
             84(16)  text-013,	
            101(6)   text-014,	
            102(10)  text-015,	
            113(4)   text-016,	
            118(20)  text-017,                              " (40)	
            139(3)   text-018,	
            143(3)   text-019,	
            147(3)   text-020,	
            151(3)   text-021,	
            155(3)   text-022,
            159(3)   text-023,
            163(3)   text-024,
            167(3)   text-025,
            171(3)   text-026,
            175(3)   text-027,
            179(3)   text-028,
            183(3)   text-029,
            187(3)   text-030,
            191(3)   text-031,
            195(3)   text-032,
            199(3)   text-033,
            203(3)   text-034,
            207(3)   text-035,
            211(3)   text-036,
            215(3)   text-037,
            219(3)   text-038,
            223(3)   text-039,
            227(3)   text-040,
            231(3)   text-041,
            235(3)   text-042,
            239(3)   text-043,
            243(3)   text-044,
            247(3)   text-045,
            251(3)   text-046,
            255(3)   text-047,
            259(3)   text-048,
            263(3)   text-049,
            267(3)   text-050,
            271(3)   text-051,
            275(3)   text-052,
            279(3)   text-053,
            283(3)   text-054,
            287(3)   text-055,
            291(3)   text-056,
            295(3)   text-057,
            299(3)   text-058,
            303(3)   text-059,
            307(3)   text-060,
            311(3)   text-061,
            315(3)   text-062,
            319(3)   text-063,
            323(3)   text-064,
            327(3)   text-065,
            331(3)   text-066,
            335(3)   text-067,
            339(3)   text-068,
            343(3)   text-069,
            347(3)   text-070,
            351(3)   text-071,
            355(3)   text-072,
            359(3)   text-073,
            363(3)   text-074,
            367(3)   text-075,
            371(3)   text-076,
            375(3)   text-077,
            379(3)   text-078,
            383(3)   text-079,
            387(3)   text-080,
            391(3)   text-081,
            395(3)   text-082,
            399(3)   text-083,
            403(3)   text-084,
            407(3)   text-085,
            411(3)   text-086,
            415(3)   text-087,
            419(3)   text-088,
            423(3)   text-089,
            427(3)   text-090,
            431(3)   text-091,
            435(3)   text-092,
            439(3)   text-093,
            443(3)   text-094,
            447(3)   text-095,
            451(3)   text-096,
            455(3)   text-097,
            459(3)   text-098,
            463(3)   text-099,
            467(3)   text-100,
            471(3)   text-101,
            475(3)   text-102,
            479(3)   text-103,
            483(3)   text-104,
            487(3)   text-105,
            491(3)   text-106,
            495(3)   text-107,
            499(3)   text-108,
            503(3)   text-109,
            507(3)   text-110,
            511(3)   text-111,
            515(3)   text-112,
            519(3)   text-113,
            523(3)   text-114,
            527(3)   text-115,
            531(3)   text-116,
            535(3)   text-117,
            539(3)   text-118,
            543(3)   text-119,
            547(3)   text-120,
            551(3)   text-121,
            555(3)   text-122,
            559(3)   text-123,
            563(3)   text-124,
            567(3)   text-125,
            571(3)   text-126,
            575(3)   text-127,
            579(3)   text-128,
            583(3)   text-129,
            587(3)   text-130,
            591(3)   text-131,
            595(3)   text-132,
            599(3)   text-133,
            603(3)   text-134,
            607(3)   text-135,
            611(3)   text-136,
            615(3)   text-137,
            619(3)   text-138,
            623(3)   text-139,
            627(3)   text-140,
            631(3)   text-141,
            635(3)   text-142,
            639(3)   text-143,
            643(3)   text-144,
            647(3)   text-145,
            651(3)   text-146,
            655(3)   text-147,
            659(3)   text-148,
            663(3)   text-149,
            667(3)   text-150,
            671(3)   text-151,
            675(3)   text-152,
            679(3)   text-153,
            683(3)   text-154,
            687(3)   text-155,
            691(3)   text-156,
            695(3)   text-157,
            699(3)   text-158,
            703(3)   text-159,
            707(3)   text-160,
            711(3)   text-161,
            715(3)   text-162,
            719(3)   text-163,
            723(3)   text-164,
            727(3)   text-165,
            731(3)   text-166,
            735(3)   text-167,
            739(3)   text-168,
            743(3)   text-169,
            747(3)   text-170,
            751(3)   text-171,
            755(3)   text-172,
            759(3)   text-173,
            763(3)   text-174,
            767(3)   text-175,
            771(3)   text-176,
            775(3)   text-177,
            779(3)   text-178,
            783(3)   text-179,
            787(3)   text-180,
            791(3)   text-181,
            795(3)   text-182,
            799(3)   text-183,
            803(3)   text-184,
            807(3)   text-185,
            811(3)   text-186,
            815(3)   text-187,
            819(3)   text-188,
            823(3)   text-189,
            827(3)   text-190,
            831(3)   text-191,
            835(3)   text-192,
            839(3)   text-193,
            843(3)   text-194,
            847(3)   text-195,
            851(3)   text-196,
            855(3)   text-197,
            859(3)   text-198,
            863(3)   text-199,
            867(3)   text-200,
            871(3)   text-201,
            875(3)   text-202,
            879(3)   text-203,
            883(3)   text-204,
            887(3)   text-205,
            891(3)   text-206,
            895(3)   text-207,
            899(3)   text-208,
            903(3)   text-209,
            907(3)   text-210,
            911(3)   text-211,
            915(3)   text-212,
            919(3)   text-213,
            923(3)   text-214,
            927(3)   text-215,
            931(3)   text-216,
            935(3)   text-217,
            939(3)   text-218,
            943(3)   text-219,
            947(3)   text-220,
            951(3)   text-221,
            955(3)   text-222,
            959(3)   text-223,
            963(3)   text-224,
            967(3)   text-225,
            971(3)   text-226,
            975(3)   text-227,
            979(3)   text-228,
            983(3)   text-229,
            987(3)   text-230,
            991(3)   text-231,
            995(3)   text-232,
            999(3)   text-233,
           1003(3)   text-234,
           1007(3)   text-235,
           1011(3)   text-236.




ENDFORM.                    " HEAD_LINE
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAD
*&---------------------------------------------------------------------*
FORM user_commad.
  CASE sy-ucomm.
    WHEN 'NEXT'.
      IF sy-lsind = '1'.
        PERFORM p_alc_u_list.
      ELSEIF sy-lsind = '2'.
        PERFORM p_alc_c_list.
      ENDIF.
    WHEN 'TRANS'.
      PERFORM transfer_to_alc.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " USER_COMMAD
*&---------------------------------------------------------------------*
*&      Form  P_ALC_U_LIST
*&---------------------------------------------------------------------*
FORM p_alc_u_list.
  DATA:zebra.
*  PERFORM LIST_HEAD2.
  LOOP AT it_zsppvs.
    zebra = sy-tabix MOD 2.
    IF zebra = 0.
      FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.
    WRITE AT:/1(4)  it_zsppvs-flg,
              6(18) it_zsppvs-matnr,
              25(4) it_zsppvs-p_alc_u_001,
              30(4) it_zsppvs-p_alc_u_002,
              35(4) it_zsppvs-p_alc_u_003,
              40(4) it_zsppvs-p_alc_u_004,
              45(4) it_zsppvs-p_alc_u_005,
              50(4) it_zsppvs-p_alc_u_006,
              55(4) it_zsppvs-p_alc_u_007,
              60(4) it_zsppvs-p_alc_u_008,
              65(4) it_zsppvs-p_alc_u_009,
              70(4) it_zsppvs-p_alc_u_010,
              75(4) it_zsppvs-p_alc_u_011,
              80(4) it_zsppvs-p_alc_u_012,
              85(4) it_zsppvs-p_alc_u_013,
              90(4) it_zsppvs-p_alc_u_014,
              95(4) it_zsppvs-p_alc_u_015,
             100(4) it_zsppvs-p_alc_u_016,
             105(4) it_zsppvs-p_alc_u_017,
             110(4) it_zsppvs-p_alc_u_018,
             115(4) it_zsppvs-p_alc_u_019,
             120(4) it_zsppvs-p_alc_u_020,
             125(4) it_zsppvs-p_alc_u_021,
             130(4) it_zsppvs-p_alc_u_022,
             135(4) it_zsppvs-p_alc_u_023,
             140(4) it_zsppvs-p_alc_u_024,
             145(4) it_zsppvs-p_alc_u_025,
             150(4) it_zsppvs-p_alc_u_026,
             155(4) it_zsppvs-p_alc_u_027,
             160(4) it_zsppvs-p_alc_u_028,
             165(4) it_zsppvs-p_alc_u_029,
             170(4) it_zsppvs-p_alc_u_030,
             175(4) it_zsppvs-p_alc_u_031,
             180(4) it_zsppvs-p_alc_u_032,
             185(4) it_zsppvs-p_alc_u_033,
             190(4) it_zsppvs-p_alc_u_034,
             195(4) it_zsppvs-p_alc_u_035,
             200(4) it_zsppvs-p_alc_u_036,
             205(4) it_zsppvs-p_alc_u_037,
             210(4) it_zsppvs-p_alc_u_038,
             215(4) it_zsppvs-p_alc_u_039,
             220(4) it_zsppvs-p_alc_u_040,
             225(4) it_zsppvs-p_alc_u_041,
             230(4) it_zsppvs-p_alc_u_042,
             235(4) it_zsppvs-p_alc_u_043,
             240(4) it_zsppvs-p_alc_u_044,
             245(4) it_zsppvs-p_alc_u_045,
             250(4) it_zsppvs-p_alc_u_046,
             255(4) it_zsppvs-p_alc_u_047,
             260(4) it_zsppvs-p_alc_u_048,
             265(4) it_zsppvs-p_alc_u_049,
             270(4) it_zsppvs-p_alc_u_050,
             275(4) it_zsppvs-p_alc_u_051,
             280(4) it_zsppvs-p_alc_u_052,
             285(4) it_zsppvs-p_alc_u_053,
             290(4) it_zsppvs-p_alc_u_054,
             295(4) it_zsppvs-p_alc_u_055,
             300(4) it_zsppvs-p_alc_u_056,
             305(4) it_zsppvs-p_alc_u_057,
             310(4) it_zsppvs-p_alc_u_058,
             315(4) it_zsppvs-p_alc_u_059,
             320(4) it_zsppvs-p_alc_u_060,
             325(4) it_zsppvs-p_alc_u_061,
             330(4) it_zsppvs-p_alc_u_062,
             335(4) it_zsppvs-p_alc_u_063,
             340(4) it_zsppvs-p_alc_u_064,
             345(4) it_zsppvs-p_alc_u_065,
             350(4) it_zsppvs-p_alc_u_066,
             355(4) it_zsppvs-p_alc_u_067,
             360(4) it_zsppvs-p_alc_u_068,
             365(4) it_zsppvs-p_alc_u_069,
             370(4) it_zsppvs-p_alc_u_070,
             375(4) it_zsppvs-p_alc_u_071,
             380(4) it_zsppvs-p_alc_u_072,
             385(4) it_zsppvs-p_alc_u_073,
             390(4) it_zsppvs-p_alc_u_074,
             395(4) it_zsppvs-p_alc_u_075,
             400(4) it_zsppvs-p_alc_u_076,
             405(4) it_zsppvs-p_alc_u_077,
             410(4) it_zsppvs-p_alc_u_078,
             415(4) it_zsppvs-p_alc_u_079,
             420(4) it_zsppvs-p_alc_u_080,
             425(4) it_zsppvs-p_alc_u_081,
             430(4) it_zsppvs-p_alc_u_082,
             435(4) it_zsppvs-p_alc_u_083,
             440(4) it_zsppvs-p_alc_u_084,
             445(4) it_zsppvs-p_alc_u_085,
             450(4) it_zsppvs-p_alc_u_086,
             455(4) it_zsppvs-p_alc_u_087,
             460(4) it_zsppvs-p_alc_u_088,
             465(4) it_zsppvs-p_alc_u_089,
             470(4) it_zsppvs-p_alc_u_090,
             475(4) it_zsppvs-p_alc_u_091,
             480(4) it_zsppvs-p_alc_u_092,
             485(4) it_zsppvs-p_alc_u_093,
             490(4) it_zsppvs-p_alc_u_094,
             495(4) it_zsppvs-p_alc_u_095,
             500(4) it_zsppvs-p_alc_u_096,
             505(4) it_zsppvs-p_alc_u_097,
             510(4) it_zsppvs-p_alc_u_098,
             515(4) it_zsppvs-p_alc_u_099,
             520(4) it_zsppvs-p_alc_u_100,
             525(4) it_zsppvs-p_alc_u_101,
             530(4) it_zsppvs-p_alc_u_102,
             535(4) it_zsppvs-p_alc_u_103,
             540(4) it_zsppvs-p_alc_u_104,
             545(4) it_zsppvs-p_alc_u_105,
             550(4) it_zsppvs-p_alc_u_106,
             555(4) it_zsppvs-p_alc_u_107,
             560(4) it_zsppvs-p_alc_u_108,
             565(4) it_zsppvs-p_alc_u_109,
             570(4) it_zsppvs-p_alc_u_110,
             575(4) it_zsppvs-p_alc_u_111,
             580(4) it_zsppvs-p_alc_u_112,
             585(4) it_zsppvs-p_alc_u_113,
             590(4) it_zsppvs-p_alc_u_114,
             595(4) it_zsppvs-p_alc_u_115,
             600(4) it_zsppvs-p_alc_u_116,
             605(4) it_zsppvs-p_alc_u_117,
             610(4) it_zsppvs-p_alc_u_118,
             615(4) it_zsppvs-p_alc_u_119,
             620(4) it_zsppvs-p_alc_u_120,
             625(4) it_zsppvs-p_alc_u_121,
             630(4) it_zsppvs-p_alc_u_122,
             635(4) it_zsppvs-p_alc_u_123,
             640(4) it_zsppvs-p_alc_u_124,
             645(4) it_zsppvs-p_alc_u_125,
             650(4) it_zsppvs-p_alc_u_126,
             655(4) it_zsppvs-p_alc_u_127,
             660(4) it_zsppvs-p_alc_u_128,
             665(4) it_zsppvs-p_alc_u_129,
             670(4) it_zsppvs-p_alc_u_130,
             675(4) it_zsppvs-p_alc_u_131,
             680(4) it_zsppvs-p_alc_u_132,
             685(4) it_zsppvs-p_alc_u_133,
             690(4) it_zsppvs-p_alc_u_134,
             695(4) it_zsppvs-p_alc_u_135,
             670(4) it_zsppvs-p_alc_u_136,
             675(4) it_zsppvs-p_alc_u_137,
             680(4) it_zsppvs-p_alc_u_138,
             685(4) it_zsppvs-p_alc_u_139,
             690(4) it_zsppvs-p_alc_u_140,
             695(4) it_zsppvs-p_alc_u_141,
             700(4) it_zsppvs-p_alc_u_142,
             705(4) it_zsppvs-p_alc_u_143,
             710(4) it_zsppvs-p_alc_u_144,
             715(4) it_zsppvs-p_alc_u_145,
             720(4) it_zsppvs-p_alc_u_146,
             725(4) it_zsppvs-p_alc_u_147,
             730(4) it_zsppvs-p_alc_u_148,
             735(4) it_zsppvs-p_alc_u_149,
             740(4) it_zsppvs-p_alc_u_150,
             745(4) it_zsppvs-p_alc_u_151,
             750(4) it_zsppvs-p_alc_u_152,
             755(4) it_zsppvs-p_alc_u_153,
             760(4) it_zsppvs-p_alc_u_154,
             765(4) it_zsppvs-p_alc_u_155,
             770(4) it_zsppvs-p_alc_u_156,
             775(4) it_zsppvs-p_alc_u_157,
             780(4) it_zsppvs-p_alc_u_158,
             785(4) it_zsppvs-p_alc_u_159,
             790(4) it_zsppvs-p_alc_u_160,
             795(4) it_zsppvs-p_alc_u_161,
             800(4) it_zsppvs-p_alc_u_162,
             805(4) it_zsppvs-p_alc_u_163,
             810(4) it_zsppvs-p_alc_u_164,
             815(4) it_zsppvs-p_alc_u_165,
             820(4) it_zsppvs-p_alc_u_166,
             825(4) it_zsppvs-p_alc_u_167,
             830(4) it_zsppvs-p_alc_u_168,
             835(4) it_zsppvs-p_alc_u_169,
             840(4) it_zsppvs-p_alc_u_170,
             845(4) it_zsppvs-p_alc_u_171,
             850(4) it_zsppvs-p_alc_u_172,
             855(4) it_zsppvs-p_alc_u_173,
             860(4) it_zsppvs-p_alc_u_174,
             865(4) it_zsppvs-p_alc_u_175,
             870(4) it_zsppvs-p_alc_u_176,
             845(4) it_zsppvs-p_alc_u_177,
             845(4) it_zsppvs-p_alc_u_178,
             845(4) it_zsppvs-p_alc_u_179,
             845(4) it_zsppvs-p_alc_u_180,
             845(4) it_zsppvs-p_alc_u_181,
             845(4) it_zsppvs-p_alc_u_182,
             850(4) it_zsppvs-p_alc_u_183,
             855(4) it_zsppvs-p_alc_u_184,
             860(4) it_zsppvs-p_alc_u_185,
             865(4) it_zsppvs-p_alc_u_186,
             870(4) it_zsppvs-p_alc_u_187,
             875(4) it_zsppvs-p_alc_u_188,
             880(4) it_zsppvs-p_alc_u_189,
             885(4) it_zsppvs-p_alc_u_190,
             890(4) it_zsppvs-p_alc_u_191,
             895(4) it_zsppvs-p_alc_u_192,
             900(4) it_zsppvs-p_alc_u_193,
             905(4) it_zsppvs-p_alc_u_194,
             910(4) it_zsppvs-p_alc_u_195,
             915(4) it_zsppvs-p_alc_u_196,
             920(4) it_zsppvs-p_alc_u_197,
             925(4) it_zsppvs-p_alc_u_198,
             930(4) it_zsppvs-p_alc_u_199,
             935(4) it_zsppvs-p_alc_u_200.
  ENDLOOP.
  SET LEFT SCROLL-BOUNDARY COLUMN 24.
ENDFORM.                    " ALC_LIST
*&---------------------------------------------------------------------*
*&      Form  LIST_HEAD2
*&---------------------------------------------------------------------*
FORM list_head2.
  FORMAT COLOR 5 INTENSIFIED OFF.
  WRITE AT:/1(4)'TYPE',
            6(18)'Material Number',
            25(4) '001',
            30(4) '002',
            35(4) '003',
            40(4) '004',
            45(4) '005',
            50(4) '006',
            55(4) '007',
            60(4) '008',
            65(4) '009',
            70(4) '010',
            75(4) '011',
            80(4) '012',
            85(4) '013',
            90(4) '014',
            95(4) '015',
           100(4) '016',
           105(4) '017',
           110(4) '018',
           115(4) '019',
           120(4) '020',
           125(4) '021',
           130(4) '022',
           135(4) '023',
           140(4) '024',
           145(4) '025',
           150(4) '026',
           155(4) '027',
           160(4) '028',
           165(4) '029',
           170(4) '030',
           175(4) '031',
           180(4) '032',
           185(4) '033',
           190(4) '034',
           195(4) '035',
           200(4) '036',
           205(4) '037',
           210(4) '038',
           215(4) '039',
           220(4) '040',
           225(4) '041',
           230(4) '042',
           235(4) '043',
           240(4) '044',
           245(4) '045',
           250(4) '046',
           255(4) '047',
           260(4) '048',
           265(4) '049',
           270(4) '050',
           275(4) '051',
           280(4) '052',
           285(4) '053',
           290(4) '054',
           295(4) '055',
           300(4) '056',
           305(4) '057',
           310(4) '058',
           315(4) '059',
           320(4) '060',
           325(4) '061',
           330(4) '062',
           335(4) '063',
           340(4) '064',
           345(4) '065',
           350(4) '066',
           355(4) '067',
           360(4) '068',
           365(4) '069',
           370(4) '070',
           375(4) '071',
           380(4) '072',
           385(4) '073',
           390(4) '074',
           395(4) '075',
           400(4) '076',
           405(4) '077',
           410(4) '078',
           415(4) '079',
           420(4) '080',
           425(4) '081',
           430(4) '082',
           435(4) '083',
           440(4) '084',
           445(4) '085',
           450(4) '086',
           455(4) '087',
           460(4) '088',
           465(4) '089',
           470(4) '090',
           475(4) '091',
           480(4) '092',
           485(4) '093',
           490(4) '094',
           495(4) '095',
           500(4) '096',
           505(4) '097',
           510(4) '098',
           515(4) '099',
           520(4) '100',
           525(4) '101',
           530(4) '102',
           535(4) '103',
           540(4) '104',
           545(4) '105',
           550(4) '106',
           555(4) '107',
           560(4) '108',
           565(4) '109',
           570(4) '110',
           575(4) '111',
           580(4) '112',
           585(4) '113',
           590(4) '114',
           595(4) '115',
           600(4) '116',
           605(4) '117',
           610(4) '118',
           615(4) '119',
           620(4) '120',
           625(4) '121',
           630(4) '122',
           635(4) '123',
           640(4) '124',
           645(4) '125',
           650(4) '126',
           655(4) '127',
           660(4) '128',
           665(4) '129',
           670(4) '130',
           675(4) '131',
           680(4) '132',
           685(4) '133',
           690(4) '134',
           695(4) '135',
           670(4) '136',
           675(4) '137',
           680(4) '138',
           685(4) '139',
           690(4) '140',
           695(4) '141',
           700(4) '142',
           705(4) '143',
           710(4) '144',
           715(4) '145',
           720(4) '146',
           725(4) '147',
           730(4) '148',
           735(4) '149',
           740(4) '150',
           745(4) '151',
           750(4) '152',
           755(4) '153',
           760(4) '154',
           765(4) '155',
           770(4) '156',
           775(4) '157',
           780(4) '158',
           785(4) '159',
           790(4) '160',
           795(4) '161',
           800(4) '162',
           805(4) '163',
           810(4) '164',
           815(4) '165',
           820(4) '166',
           825(4) '167',
           830(4) '168',
           835(4) '169',
           840(4) '170',
           845(4) '171',
           850(4) '172',
           855(4) '173',
           860(4) '174',
           865(4) '175',
           870(4) '176',
           845(4) '177',
           845(4) '178',
           845(4) '179',
           845(4) '180',
           845(4) '181',
           845(4) '182',
           850(4) '183',
           855(4) '184',
           860(4) '185',
           865(4) '186',
           870(4) '187',
           875(4) '188',
           880(4) '189',
           885(4) '190',
           890(4) '191',
           895(4) '192',
           900(4) '193',
           905(4) '194',
           910(4) '195',
           915(4) '196',
           920(4) '197',
           925(4) '198',
           930(4) '199',
           935(4) '200'.
ENDFORM.                    " LIST_HEAD2
*&---------------------------------------------------------------------*
*&      Form  P_ALC_C_LIST
*&---------------------------------------------------------------------*
FORM p_alc_c_list.
  DATA zebra.
*  PERFORM LIST_HEAD3.
  LOOP AT it_zsppvs.
    zebra = sy-tabix MOD 2.
    IF zebra = 0.
      FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.
    WRITE AT:/1(4)  it_zsppvs-flg,
              6(18) it_zsppvs-matnr,
              25(4) it_zsppvs-p_alc_c_001,
              30(4) it_zsppvs-p_alc_c_002,
              35(4) it_zsppvs-p_alc_c_003,
              40(4) it_zsppvs-p_alc_c_004,
              45(4) it_zsppvs-p_alc_c_005,
              50(4) it_zsppvs-p_alc_c_006,
              55(4) it_zsppvs-p_alc_c_007,
              60(4) it_zsppvs-p_alc_c_008,
              65(4) it_zsppvs-p_alc_c_009,
              70(4) it_zsppvs-p_alc_c_010,
              75(4) it_zsppvs-p_alc_c_011,
              80(4) it_zsppvs-p_alc_c_012,
              85(4) it_zsppvs-p_alc_c_013,
              90(4) it_zsppvs-p_alc_c_014,
              95(4) it_zsppvs-p_alc_c_015,
             100(4) it_zsppvs-p_alc_c_016,
             105(4) it_zsppvs-p_alc_c_017,
             110(4) it_zsppvs-p_alc_c_018,
             115(4) it_zsppvs-p_alc_c_019,
             120(4) it_zsppvs-p_alc_c_020,
             125(4) it_zsppvs-p_alc_c_021,
             130(4) it_zsppvs-p_alc_c_022,
             135(4) it_zsppvs-p_alc_c_023,
             140(4) it_zsppvs-p_alc_c_024,
             145(4) it_zsppvs-p_alc_c_025,
             150(4) it_zsppvs-p_alc_c_026,
             155(4) it_zsppvs-p_alc_c_027,
             160(4) it_zsppvs-p_alc_c_028,
             165(4) it_zsppvs-p_alc_c_029,
             170(4) it_zsppvs-p_alc_c_030,
             175(4) it_zsppvs-p_alc_c_031,
             180(4) it_zsppvs-p_alc_c_032,
             185(4) it_zsppvs-p_alc_c_033,
             190(4) it_zsppvs-p_alc_c_034,
             195(4) it_zsppvs-p_alc_c_035,
             200(4) it_zsppvs-p_alc_c_036,
             205(4) it_zsppvs-p_alc_c_037,
             210(4) it_zsppvs-p_alc_c_038,
             215(4) it_zsppvs-p_alc_c_039,
             220(4) it_zsppvs-p_alc_c_040,
             225(4) it_zsppvs-p_alc_c_041,
             230(4) it_zsppvs-p_alc_c_042,
             235(4) it_zsppvs-p_alc_c_043,
             240(4) it_zsppvs-p_alc_c_044,
             245(4) it_zsppvs-p_alc_c_045,
             250(4) it_zsppvs-p_alc_c_046,
             255(4) it_zsppvs-p_alc_c_047,
             260(4) it_zsppvs-p_alc_c_048,
             265(4) it_zsppvs-p_alc_c_049,
             270(4) it_zsppvs-p_alc_c_050,
             275(5) it_zsppvs-dist,
             281(11) it_zsppvs-p_wo_create_date,
             293(11) it_zsppvs-p_wo_create_time.
  ENDLOOP.
ENDFORM.                    " P_ALC_C_LIST
*&---------------------------------------------------------------------*
*&      Form  LIST_HEAD3
*&---------------------------------------------------------------------*
FORM list_head3.
  FORMAT COLOR 5 INTENSIFIED OFF.
  WRITE AT:/1(4)'TYPE',
            6(18)'Material Number',
            25(4) '001',
            30(4) '002',
            35(4) '003',
            40(4) '004',
            45(4) '005',
            50(4) '006',
            55(4) '007',
            60(4) '008',
            65(4) '009',
            70(4) '010',
            75(4) '011',
            80(4) '012',
            85(4) '013',
            90(4) '014',
            95(4) '015',
           100(4) '016',
           105(4) '017',
           110(4) '018',
           115(4) '019',
           120(4) '020',
           125(4) '021',
           130(4) '022',
           135(4) '023',
           140(4) '024',
           145(4) '025',
           150(4) '026',
           155(4) '027',
           160(4) '028',
           165(4) '029',
           170(4) '030',
           175(4) '031',
           180(4) '032',
           185(4) '033',
           190(4) '034',
           195(4) '035',
           200(4) '036',
           205(4) '037',
           210(4) '038',
           215(4) '039',
           220(4) '040',
           225(4) '041',
           230(4) '042',
           235(4) '043',
           240(4) '044',
           245(4) '045',
           250(4) '046',
           255(4) '047',
           260(4) '048',
           265(4) '049',
           270(4) '050',
           275(5) 'DIST',
           281(11) 'CREATE DATE',
           293(11) 'CREATE TIME'.
ENDFORM.                    " LIST_HEAD3
