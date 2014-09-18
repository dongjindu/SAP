FUNCTION z_fmm_6025_in_mk02 .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CTU) LIKE  APQI-PUTACTIVE DEFAULT 'X'
*"     VALUE(MODE) LIKE  APQI-PUTACTIVE DEFAULT 'N'
*"     VALUE(UPDATE) LIKE  APQI-PUTACTIVE DEFAULT 'L'
*"     VALUE(GROUP) LIKE  APQI-GROUPID OPTIONAL
*"     VALUE(USER) LIKE  APQI-USERID OPTIONAL
*"     VALUE(KEEP) LIKE  APQI-QERASE OPTIONAL
*"     VALUE(HOLDDATE) LIKE  APQI-STARTDATE OPTIONAL
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE DEFAULT '/'
*"     VALUE(LIFNR_001) LIKE  BDCDATA-FVAL DEFAULT 'VENDOR003'
*"     VALUE(EKORG_002) LIKE  BDCDATA-FVAL DEFAULT 'pu01'
*"     VALUE(D0110_003) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(D0310_004) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(USE_ZAV_005) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(NAME1_006) LIKE  BDCDATA-FVAL DEFAULT 'NAME1'
*"     VALUE(SORT1_007) LIKE  BDCDATA-FVAL DEFAULT 'SEARCHTERM 1/2'
*"     VALUE(STREET_008) LIKE  BDCDATA-FVAL DEFAULT
*"       'Street/House number'
*"     VALUE(POST_CODE1_009) LIKE  BDCDATA-FVAL DEFAULT '12345'
*"     VALUE(CITY1_010) LIKE  BDCDATA-FVAL DEFAULT 'New York'
*"     VALUE(COUNTRY_011) LIKE  BDCDATA-FVAL DEFAULT 'US'
*"     VALUE(REGION_012) LIKE  BDCDATA-FVAL DEFAULT 'AL'
*"     VALUE(LANGU_014) LIKE  BDCDATA-FVAL DEFAULT 'EN'
*"     VALUE(TEL_NUMBER_015) LIKE  BDCDATA-FVAL DEFAULT 'TELEPHONE1'
*"     VALUE(FAX_NUMBER_016) LIKE  BDCDATA-FVAL DEFAULT 'FAXNUMBER'
*"     VALUE(SMTP_ADDR_017) LIKE  BDCDATA-FVAL DEFAULT 'email@emil.com'
*"     VALUE(WAERS_018) LIKE  BDCDATA-FVAL DEFAULT 'USD'
*"     VALUE(ZTERM_019) LIKE  BDCDATA-FVAL DEFAULT 'N030'
*"     VALUE(VERKF_020) LIKE  BDCDATA-FVAL DEFAULT 'SALESPERSON'
*"     VALUE(TELF1_021) LIKE  BDCDATA-FVAL DEFAULT '123-123-123'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

*&--------------------------------------------------------------------&*
*&   Program: Z_FMM_6025_IN_MK02.
*&   Specification: Don't change the email address in MK02. Save after
*&                  all screen in the transaction are filled.
*&--------------------------------------------------------------------&*
*& Date        User         Transport        Description
*& 07/20/2004  100471       UD1K911517       issuelogno: MM-20040719
*&--------------------------------------------------------------------&*

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0108'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'USE_ZAV'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RF02K-LIFNR'
                                lifnr_001.
  PERFORM bdc_field       USING 'RF02K-EKORG'
                                ekorg_002.
  PERFORM bdc_field       USING 'RF02K-D0110'
                                d0110_003.
  PERFORM bdc_field       USING 'RF02K-D0310'
                                d0310_004.
  PERFORM bdc_field       USING 'USE_ZAV'
                                use_zav_005.
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0111'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VW'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'ADDR1_DATA-SORT1'.
  PERFORM bdc_field       USING 'ADDR1_DATA-NAME1'
                                name1_006.
  PERFORM bdc_field       USING 'ADDR1_DATA-SORT1'
                                sort1_007.

*/Begin of Added by Hakchin(20040325)(Needed by KBLEE)
  PERFORM bdc_field       USING 'ADDR1_DATA-NAME_CO'
                                street_008(40). "Changed for Size Fix
  PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL1'
                                city1_010.

  DATA: lv_str_suppl2 LIKE bdcdata-fval. CLEAR: lv_str_suppl2.
  IF region_012 IS INITIAL.
    lv_str_suppl2 = post_code1_009.
  ELSE.
    CONCATENATE region_012 ',' INTO lv_str_suppl2.
    CONCATENATE lv_str_suppl2 post_code1_009 INTO lv_str_suppl2
                                  SEPARATED BY space.
    CONDENSE lv_str_suppl2.
  ENDIF.
  PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL2'
                                lv_str_suppl2.
*/End of Added by Hakchin(20040325)(Needed by KBLEE)

*/Begin of Commented by Hakchin(20040325)(Needed by KBLEE)
*  PERFORM bdc_field       USING 'ADDR1_DATA-STREET'
*                                street_008.
*  PERFORM bdc_field       USING 'ADDR1_DATA-POST_CODE1'
*                                post_code1_009.
*  PERFORM bdc_field       USING 'ADDR1_DATA-CITY1'
*                                city1_010.
*  PERFORM bdc_field       USING 'ADDR1_DATA-COUNTRY'
*                                country_011.
*  PERFORM bdc_field       USING 'ADDR1_DATA-REGION'
*                                region_012.
*/End of Commented by Hakchin(20040325)(Needed by KBLEE)
  PERFORM bdc_field       USING 'ADDR1_DATA-LANGU'
                                langu_014.
  PERFORM bdc_field       USING 'SZA1_D0100-TEL_NUMBER'
                                tel_number_015.
  PERFORM bdc_field       USING 'SZA1_D0100-FAX_NUMBER'
                                fax_number_016.
*&---Don't change email info. in change mode.
*  PERFORM bdc_field       USING 'SZA1_D0100-SMTP_ADDR'
*                                smtp_addr_017.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VW'.
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0310'.
*&---save at the end after filling all screens.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=UPDA'.
  PERFORM bdc_field       USING 'LFM1-WAERS'
                                waers_018.
  PERFORM bdc_field       USING 'LFM1-ZTERM'
                                zterm_019.
  PERFORM bdc_field       USING 'LFM1-VERKF'
                                verkf_020.
  PERFORM bdc_field       USING 'LFM1-TELF1'
                                telf1_021.
*&---Moved from before screen save.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=UPDA'.

  PERFORM bdc_transaction TABLES messtab
  USING                         'MK02'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.


ENDFUNCTION.
