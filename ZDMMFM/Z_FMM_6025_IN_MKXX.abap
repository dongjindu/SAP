FUNCTION z_fmm_6025_in_mkxx.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      IMT_ZTMM_6025_01 STRUCTURE  ZTMM_6025_01 OPTIONAL
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
*&--------------------------------------------------------------------&*
*& Date        User      Transport         Description                &*
*& 11/23/2004  Shiva     UD1K913154       Changed the program to
*&                                        create multiple vendor.
*& 06/23/2006  Manju     UD1K921150       For Korean Vendors if length
*&                                        of postal code is 6 and '-'
*&                                        is missing then insert '-'
*&                                        as 4th character to make
*&                                        postal code as 7 character val
*&--------------------------------------------------------------------&*

  subrc = 0.
  data: w_if_suc type i,
        w_if_fai type i.
*/ I omit unblock logging because it is not important.
*/ Otherwise, Source could be complicated.

  CHECK NOT imt_ztmm_6025_01[] IS INITIAL.
*  READ TABLE imt_ztmm_6025_01 INDEX 1. "To use header of itab
  loop at imt_ztmm_6025_01.
    PERFORM get_interface_data USING imt_ztmm_6025_01.

* App Doc No.
    PERFORM number_get_next USING    nro_nr_09     "NRO Interval
                                     nro_object    "NRO Object
                            CHANGING w_zdocno.     "App Doc No
    COMMIT WORK.


* VENDOR EXISTENCECHECK
    DATA: lifnr LIKE lfa1-lifnr.  "Vendor
    DATA: sperm LIKE lfa1-sperm.  "Purchasing Block
    SELECT SINGLE lifnr
      INTO lifnr
      FROM lfa1
      WHERE lifnr = imt_ztmm_6025_01-lifnr.

    IF sy-subrc <> 0.   "Vendor does not exists..
* Create Vendor
      imt_ztmm_6025_01-zmode = 'C'.  "Create
      wa_ztca_if_log-tcode = 'MK01'. "Present Transaction Code
      CALL FUNCTION 'Z_FMM_6025_IN_MK01'
           EXPORTING
                ctu            = 'X'
                mode           = 'N'
                UPDATE         = 'L'
*              group          =
*              user           =
*              keep           =
*              holddate       =
                nodata         = '/'
                lifnr_001      = w_lifnr
                ekorg_002      = w_ekorg                    "'PU01'
                ktokk_003      = w_ktokk                    "'Y020'
                use_zav_004    = 'X'  "Central Address Management
                name1_005      = w_name1                    "'NAME1'
                sort1_006      = w_sort1 "'SEARCHTERM 1/2'
                street_007     = w_street
                post_code1_008 = w_post_code1               "'12345'
                city1_009      = w_city1      "'New York'
                country_010    = w_country    "'US'
                region_011     = w_region     "'AL'
                langu_013      = w_langu      "'EN'
                tel_number_014 = w_tel_number "'TELEPHONE1'
                fax_number_015 = w_fax_number "'FAXNUMBER'
                smtp_addr_016  = w_smtp_addr
                                                   "'hakchin@jinlab.com'
                waers_017      = w_waers   "'USD'
                zterm_018      = w_zterm                    "'N030'
                verkf_019      = w_verkf   "'SALESPERSON'
                telf1_020      = w_telf1   "'2345-12121'
           IMPORTING
                subrc          = subrc
           TABLES
                messtab        = messtab.

* BDC Logging
      PERFORM bdclog_to_ztlog
                        TABLES messtab
                        USING  w_zdocno
                               sy-tcode  "tcode
                               sy-cprog. "cprog.

    ELSE.                        "Vendor exists
* Change Vendor
      imt_ztmm_6025_01-zmode = 'U'.  "Update

      IF imt_ztmm_6025_01-block_flg = 'X'. "Block Flag
        wa_ztca_if_log-tcode = 'MK05'. "Block
* Block
        CALL FUNCTION 'Z_FMM_6025_IN_MK05'
          EXPORTING
            ctu             = 'X'
            mode            = 'N'
            UPDATE          = 'L'
*       GROUP           =
*       USER            =
*       KEEP            =
*       HOLDDATE        =
            nodata          = '/'
            lifnr_001       = w_lifnr                       "'N001'
            ekorg_002       = w_ekorg                       "'PU01'
            sperm_003       = 'X'
            sperm_004       = 'X'
            sperq_005       = 'X'
          IMPORTING
            subrc           = subrc
          TABLES
            messtab         = messtab.
* BDC Logging
        PERFORM bdclog_to_ztlog
                          TABLES messtab
                          USING  w_zdocno
                                 sy-tcode  "tcode
                                 sy-cprog. "cprog.
      ELSE.
        wa_ztca_if_log-tcode = 'MK05'. "Unblock
* Unblock
        CALL FUNCTION 'Z_FMM_6025_IN_MK05'
          EXPORTING
            ctu             = 'X'
            mode            = 'N'
            UPDATE          = 'L'
*       GROUP           =
*       USER            =
*       KEEP            =
*       HOLDDATE        =
            nodata          = '/'
            lifnr_001       = w_lifnr                       "'N001'
            ekorg_002       = w_ekorg                       "'PU01'
            sperm_003       = ' '
            sperm_004       = ' '
            sperq_005       = ' '
          IMPORTING
            subrc           = subrc
          TABLES
            messtab         = messtab.
* BDC Logging
        PERFORM bdclog_to_ztlog
                          TABLES messtab
                          USING  w_zdocno
                                 sy-tcode  "tcode
                                 sy-cprog. "cprog.

* Here it is useless to check above subrc. Anyway we can go.
        wa_ztca_if_log-tcode = 'MK02'. "Present Transaction Code
        CALL FUNCTION 'Z_FMM_6025_IN_MK02'
         EXPORTING
           ctu                  = 'X'
           mode                 = 'N'
           UPDATE               = 'L'
*   GROUP                =
*   USER                 =
*   KEEP                 =
*   HOLDDATE             =
           nodata               = '/'
           lifnr_001            = w_lifnr "'VENDOR003'
           ekorg_002            = w_ekorg                   "'pu01'
           d0110_003            = 'X'   "Change flg of Address
           d0310_004            = 'X'   "Change flg of Purchasing Data
           use_zav_005          = 'X'   "Central Address Management
           name1_006            = w_name1                   "'NAME1'
           sort1_007            = w_sort1 "'SEARCHTERM 1/2'
           street_008           = w_street
           post_code1_009       = w_post_code1              "'12345'
           city1_010            = w_city1       "'New York'
           country_011          = w_country     "'US'
           region_012           = w_region      "'AL'
           langu_014            = w_langu       "'EN'
           tel_number_015       = w_tel_number  "'TELEPHONE1'
           fax_number_016       = w_fax_number  "'FAXNUMBER'
           smtp_addr_017        = w_smtp_addr   "'hakchin@jinlab.com'
           waers_018            = w_waers  "'USD'
           zterm_019            = w_zterm                   "'N030'
           verkf_020            = w_verkf  "'SALESPERSON'
           telf1_021            = w_telf1  "'2345-12121'
         IMPORTING
           subrc                = subrc
         TABLES
           messtab              = messtab.
* BDC Logging
        PERFORM bdclog_to_ztlog
                          TABLES messtab
                          USING  w_zdocno
                                 sy-tcode  "tcode
                                 sy-cprog. "cprog.
      ENDIF.
    ENDIF.

**** (Begin) of Logging to the table ZTMM_6025_01
    IF subrc <> 0.
      imt_ztmm_6025_01-zzret = 'E'.    "Failure
      w_if_fai = w_if_fai + 1.
*/Begin of Added by Hakchin(20040505)
      DATA:   l_mstring(480).
      TABLES: t100.
      READ TABLE messtab WITH KEY msgtyp = 'E'.
      IF sy-subrc = 0.
        SELECT SINGLE * FROM t100
           WHERE sprsl = messtab-msgspra AND
                 arbgb = messtab-msgid   AND
                 msgnr = messtab-msgnr.
        IF sy-subrc = 0.
          l_mstring = t100-text.
          IF l_mstring CS '&1'.
            REPLACE '&1' WITH messtab-msgv1 INTO l_mstring.
            REPLACE '&2' WITH messtab-msgv2 INTO l_mstring.
            REPLACE '&3' WITH messtab-msgv3 INTO l_mstring.
            REPLACE '&4' WITH messtab-msgv4 INTO l_mstring.
          ELSE.
            REPLACE '&' WITH messtab-msgv1 INTO l_mstring.
            REPLACE '&' WITH messtab-msgv2 INTO l_mstring.
            REPLACE '&' WITH messtab-msgv3 INTO l_mstring.
            REPLACE '&' WITH messtab-msgv4 INTO l_mstring.
          ENDIF.
          CONDENSE l_mstring.
        ENDIF.
        imt_ztmm_6025_01-zmsg = l_mstring. CLEAR: l_mstring.
      ENDIF.
*/End of Added by Hakchin(20040505)
    ELSE.
      imt_ztmm_6025_01-zzret = 'S'.     "Success
      w_if_suc = w_if_suc + 1.
    ENDIF.
    MOVE w_zdocno TO imt_ztmm_6025_01-zdocno. "App Doc No.

* Begin of Timestap
    MOVE sy-uname  TO imt_ztmm_6025_01-zuser.   "Created User ID
    MOVE sy-datum  TO imt_ztmm_6025_01-zsdat.   "SEND FILE CREATED DATE
    MOVE sy-uzeit  TO imt_ztmm_6025_01-zstim.   "SEND FILE CREATED TIME
    MOVE sy-datum  TO imt_ztmm_6025_01-zedat.   "SAP INTERFACE DATE
    MOVE sy-uzeit  TO imt_ztmm_6025_01-zetim.   "SAP INTERFACE TIME
    MOVE sy-datum  TO imt_ztmm_6025_01-zbdat.   "SAP BDC EXECUTED DATE
    MOVE sy-uzeit  TO imt_ztmm_6025_01-zbtim.   "SAP BDC EXECUTED TIME
    MOVE sy-uname  TO imt_ztmm_6025_01-zbnam.   "BDC User ID
*  MOVE           TO imt_ztmm_6025_01-zmode.   "(Create/Update/Delete)
*Result of the Processing
    MOVE imt_ztmm_6025_01-zzret TO imt_ztmm_6025_01-zresult.
*  MOVE space     TO imt_ztmm_6025_01-zmsg.    "Message text
* End of Timestap

    MODIFY imt_ztmm_6025_01.  "Update Internal table

    INSERT INTO ztmm_6025_01 VALUES imt_ztmm_6025_01.
**** (End) of Logging
  endloop.
*/ Interface log
  wa_ztca_if_log-zsucc = w_if_suc.        "Success Quantity
  wa_ztca_if_log-error = w_if_fai.        "Fail Quantity

  PERFORM z_fca_eai_interface_log.

ENDFUNCTION.
