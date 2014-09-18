* XZQP9CK114420 17.01.01 Persönliche Vorlagen in CATS
* 4.6C
FUNCTION ZCATS_BUILD_CATSD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(I_PERNR) LIKE  CATSDB-PERNR
*"             VALUE(I_SIMULATE) LIKE  CATSFIELDS-YESORNO OPTIONAL
*"       EXPORTING
*"             VALUE(E_DATETO) LIKE  CATSFIELDS-DATETO
*"             VALUE(E_DATEFROM) LIKE  CATSFIELDS-DATEFROM
*"       TABLES
*"              EXT_INTERFACE_IN STRUCTURE  CATSDB_EXT OPTIONAL
*"              E_CATSD STRUCTURE  CATSD
*"              E_CATSDB STRUCTURE  CATSDB OPTIONAL
*"----------------------------------------------------------------------
  CONSTANTS: highcounter LIKE catsdb-counter VALUE 999999999999.
  DATA: catscell TYPE catscell.
  REFRESH icatsdb.
  CLEAR icatsdb.                                            "YIK
  CLEAR e_datefrom.
  CLEAR e_dateto.

* set some internal variables
  PERFORM set_week USING start_date catsfields-catsweek.

* fill date fields on dynp
  PERFORM get_boundaries USING
                     start_date catsfields-datefrom catsfields-dateto
                     catsfields-catsweek days_on_screen.

  e_datefrom = catsfields-datefrom.
  e_dateto = catsfields-dateto.

  IF i_simulate IS INITIAL.                                 "YIK
* read data from catsdb
    CALL FUNCTION 'CATS_READ_CATSDB'
         EXPORTING
              catspernr     = i_pernr
              fromdate      = catsfields-datefrom
              todate        = catsfields-dateto
              void          = yx
              changed       = yx
              approved      = yx
              free          = yx
              locked        = yx
              rejected      = yx
         TABLES
              i_catsdbcomm  = icatsdb
         EXCEPTIONS
              error_message = 1
              OTHERS        = 2.

    CHECK sy-subrc = 0.

    PERFORM actions_based_on_read_data.

* build internal table with counter and refcounter
    PERFORM build_ancestors_tab.

    REFRESH e_catsdb.
    LOOP AT icatsdb.
      MOVE-CORRESPONDING icatsdb TO e_catsdb.
      APPEND e_catsdb.
    ENDLOOP.
* begin of insertion
  ELSE.
    LOOP  AT ext_interface_in.
      MOVE-CORRESPONDING ext_interface_in TO icatsdb.
* records without counter should be in the last castd line
      IF icatsdb-counter IS INITIAL.
        icatsdb-counter = highcounter.
      ENDIF.
      APPEND icatsdb.
    ENDLOOP.
    PERFORM actions_based_on_read_data.
* build internal table with counter and refcounter
    PERFORM build_ancestors_tab.
  ENDIF.
* end of insertion YIK
  catsfields-pernr = i_pernr.          "XZQP9CK114420
* fill table icatsd
  PERFORM read_data_from_inttab USING catsfields-datefrom
                                      catsfields-dateto
                                      days_on_screen
                                      i_simulate.           "YIK
  REFRESH e_catsd.
* convert for output
  LOOP AT icatsd.
    MOVE-CORRESPONDING icatsd TO e_catsd.
    APPEND e_catsd.
  ENDLOOP.

ENDFUNCTION.
*eject
