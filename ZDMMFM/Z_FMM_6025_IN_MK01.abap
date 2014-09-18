FUNCTION z_fmm_6025_in_mk01 .
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
*"     VALUE(EKORG_002) LIKE  BDCDATA-FVAL DEFAULT 'PU01'
*"     VALUE(KTOKK_003) LIKE  BDCDATA-FVAL DEFAULT 'Y020'
*"     VALUE(USE_ZAV_004) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(NAME1_005) LIKE  BDCDATA-FVAL DEFAULT 'NAME1'
*"     VALUE(SORT1_006) LIKE  BDCDATA-FVAL DEFAULT 'SEARCHTERM 1/2'
*"     VALUE(STREET_007) LIKE  BDCDATA-FVAL DEFAULT
*"       'Street/House number'
*"     VALUE(POST_CODE1_008) LIKE  BDCDATA-FVAL DEFAULT '12345'
*"     VALUE(CITY1_009) LIKE  BDCDATA-FVAL DEFAULT 'New York'
*"     VALUE(COUNTRY_010) LIKE  BDCDATA-FVAL DEFAULT 'US'
*"     VALUE(REGION_011) LIKE  BDCDATA-FVAL DEFAULT 'AL'
*"     VALUE(LANGU_013) LIKE  BDCDATA-FVAL DEFAULT 'EN'
*"     VALUE(TEL_NUMBER_014) LIKE  BDCDATA-FVAL DEFAULT 'TELEPHONE1'
*"     VALUE(FAX_NUMBER_015) LIKE  BDCDATA-FVAL DEFAULT 'FAXNUMBER'
*"     VALUE(SMTP_ADDR_016) LIKE  BDCDATA-FVAL DEFAULT
*"       'email@email.com'
*"     VALUE(WAERS_017) LIKE  BDCDATA-FVAL DEFAULT 'USD'
*"     VALUE(ZTERM_018) LIKE  BDCDATA-FVAL DEFAULT 'N030'
*"     VALUE(VERKF_019) LIKE  BDCDATA-FVAL DEFAULT 'SALESPERSON'
*"     VALUE(TELF1_020) LIKE  BDCDATA-FVAL DEFAULT '123-123-123'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

*&--------------------------------------------------------------------&*
*& Date        User     Transport       Description
*& 09/29/2005  Shiva    UD1K917767      default authorization group
*&                                      field to '1'.
*&--------------------------------------------------------------------&*

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0107'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'USE_ZAV'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RF02K-LIFNR'
                                lifnr_001.
  PERFORM bdc_field       USING 'RF02K-EKORG'
                                ekorg_002.
  PERFORM bdc_field       USING 'RF02K-KTOKK'
                                ktokk_003.
  PERFORM bdc_field       USING 'USE_ZAV'
                                use_zav_004.
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0111'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VW'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'SZA1_D0100-SMTP_ADDR'.
  PERFORM bdc_field       USING 'ADDR1_DATA-NAME1'
                                name1_005.
  PERFORM bdc_field       USING 'ADDR1_DATA-SORT1'
                                sort1_006.

*/Begin of Added by Hakchin(20040325)(Needed by KBLEE)
  PERFORM bdc_field       USING 'ADDR1_DATA-NAME_CO'
                                street_007(40).
  PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL1'
                                city1_009.

  DATA: lv_str_suppl2 LIKE bdcdata-fval. CLEAR: lv_str_suppl2.
  IF region_011 IS INITIAL.
    lv_str_suppl2 = post_code1_008.
  ELSE.
    CONCATENATE region_011 ',' INTO lv_str_suppl2.
    CONCATENATE lv_str_suppl2 post_code1_008 INTO lv_str_suppl2
                                  SEPARATED BY space.
    CONDENSE lv_str_suppl2.
  ENDIF.
  PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL2'
                                lv_str_suppl2.
*/End of Added by Hakchin(20040325)(Needed by KBLEE)


  PERFORM bdc_field       USING 'ADDR1_DATA-STREET'
                                street_007.
  PERFORM bdc_field       USING 'ADDR1_DATA-POST_CODE1'
                                post_code1_008.
  PERFORM bdc_field       USING 'ADDR1_DATA-CITY1'
                                city1_009.
  PERFORM bdc_field       USING 'ADDR1_DATA-COUNTRY'
                                country_010.
  PERFORM bdc_field       USING 'ADDR1_DATA-REGION'
                                region_011.
  PERFORM bdc_field       USING 'ADDR1_DATA-LANGU'
                                langu_013.
  PERFORM bdc_field       USING 'SZA1_D0100-TEL_NUMBER'
                                tel_number_014.
  PERFORM bdc_field       USING 'SZA1_D0100-FAX_NUMBER'
                                fax_number_015.
  PERFORM bdc_field       USING 'SZA1_D0100-SMTP_ADDR'
                                smtp_addr_016.
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0120'.
  perform bdc_field       using  'LFA1-BEGRU'
                                 '1'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VW'.
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VW'.
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0310'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=UPDA'.
  PERFORM bdc_field       USING 'LFM1-WAERS'
                                waers_017.
  PERFORM bdc_field       USING 'LFM1-ZTERM'
                                zterm_018.
  PERFORM bdc_field       USING 'LFM1-VERKF'
                                verkf_019.
  PERFORM bdc_field       USING 'LFM1-TELF1'
                                telf1_020.

*/Begin of Added by Hakchin(20040407)
*Indicator: GR-based invoice verification
  IF ktokk_003 = 'Y020'. "Account Group: Domestic vendors
    PERFORM bdc_field       USING 'LFM1-WEBRE'
                                  'X'.
  ENDIF.
*/End of Added by Hakchin(20040407)

  PERFORM bdc_transaction TABLES messtab
  USING                         'MK01'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.


ENDFUNCTION.
INCLUDE bdcrecxy .
