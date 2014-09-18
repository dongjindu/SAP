************************************************************************
* Program Name      : ZEMMGM05E_6001
* Author            : Hakchin Kim
* Creation Date     : 2002.08.06.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : EMMGM05
* Addl Documentation: F/S - EMMGM05 Automatic Info Record Update Pgm
* Description : EMMGM05 Automatic Info Record Update Program in P/O,
*                       RFQ, Contract
*               This is a BDC program. Scheduled once a day.
*               BDC Transaction: ME11, ME12
*
* Modification Log
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT  zemmgm05e_6001.
************************************************************************
INCLUDE zemmgm05e_6001top.       "Data Declaration
INCLUDE zemmgm05e_6001bdcrec.    "Include for Batch Input Programs
INCLUDE zemmgm05e_6001f01.       "Perform Library.
************************************************************************
START-OF-SELECTION.
  PERFORM get_data_from_table.            "get data

**********BDC PROCESSING************************************************
*App. Doc. No.
  PERFORM number_get_next USING    nro_nr_09     "NRO Interval
                                   nro_object    "NRO Object
                          CHANGING w_zdocno.     "NRO Next(AppDocNo)
  COMMIT WORK.

  CLEAR: i_ekko.
  LOOP AT it_i_ekko INTO i_ekko
              WHERE frgke = 'F'. "Rel.Indicator(Released PO)
    LOOP AT xekpo INTO wa_xekpo WHERE ebeln = i_ekko-ebeln AND
                                      zzinforeccre <> space.
      CLEAR wa_eine.
      SELECT SINGLE * INTO wa_eine
        FROM eine
        WHERE infnr = wa_xekpo-infnr AND     "Info Record
              ekorg = i_ekko-ekorg   AND     "Purchasing organization
              esokz = '0'.            "AND     "Info record category
*Issue :MM-20040722
*Info-Record is created without data of plant (WERKS)
*-----Start
*No more consider plant
*              werks = wa_xekpo-werks.        "Plant
*-----End
      IF sy-subrc = 0.
        PERFORM me12 USING wa_xekpo   "Make bdcdata, Change Info Record
                           i_ekko.
        PERFORM bdc_transaction USING 'ME12'.  "Change Info Record
      ELSE.
        PERFORM me11 USING wa_xekpo   "Make bdcdata, Create Info Record
                           i_ekko.
        PERFORM bdc_transaction USING 'ME11'.  "Create Info Record
      ENDIF.
**** (Begin)BDC Log to the table ZTLOG
      DATA: logno_h TYPE num10.
      PERFORM number_get_next USING  nro_nr_00    "Log Header
                                     'ZMMNRO0002'
                            CHANGING logno_h.

      DATA: w_ztcode    TYPE tcode.
      DATA: w_zprogramm TYPE programm.
      w_ztcode    = 'ZMME94'.
      w_zprogramm = 'ZEMMGM05E_6001'.
      PERFORM bdc_log_to_ztlog TABLES messtab
                               USING  logno_h
                                      w_zdocno
                                      w_ztcode
                                      w_zprogramm.
**** (End)BDC Log to the table ZTLOG

    ENDLOOP.
  ENDLOOP.
