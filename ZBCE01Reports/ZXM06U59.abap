*----------------------------------------------------------------------*
*   INCLUDE ZXM06U59                                                   *
*----------------------------------------------------------------------*

*&--------------------------------------------------------------------&*
*    Program: ZXM06U59.
*    Author : Shiva Gnanaguru
*    Specification: Replace Schd.line qty with MRP Qty as exact qty.
*---------------------------------------------------------------------&*
* Date        Userid   Transport   Description
* 03/12/2004  100471  UD1K908101   initial program.
* 01/21/2005  100471  UD1K913926   Fix 21 week calculation, when it
*                                  reaches 52nd week at the year end.
* 01/27/2005  100471  UD1K913994   Delete zero quantity forecast data
*                                 and don't send the IDoc if no forecast
*                                  data exists.
* 02/07/2005  100471  UD1K914233  Send Net Forecast only. ie send the
*                                 requirements only if available stock
*                                 less than or equal to "0" as per
*                                 tcode "MS04".
* 02/07/2005  100471 UD1K914243  Set back to standard SAP functionality.
* 02/07/2005  100471 UD1K914248  Use requirements value from long term
*                                planning tcode "MS04" and delete the
*                             week's information less than current week.
* 02/08/2005  100471 UD1K914252  Reset the logic to initial and delete
*                                as per last change.
* 02/16/2005  100471 UD1K914436 In 830 if the start and end date is
*                       same then change end date to the week end date.
* 02/17/2005  100471 UD1K914446 Delete the Schedule line information
*                               and add segments for 21 weeks to pass
*                               LTP values as Schedule line.
*03/31/2006   Manju  UD1K919923  Populate DFABL & VBRST in segment
*                                E1EDP10 from MARD instead of schedule
*                                Agreement.
*&--------------------------------------------------------------------&*
data: wa_edidd like edidd,                                  "UD1K919923
      wa_e1edp10 like e1edp10,                              "UD1K919923
      w_matnr like mard-matnr,                              "UD1K919923
      w_werks like mard-werks,                              "UD1K919923
      w_lgort like mard-lgort,                              "UD1K919923
      w_dfabl like e1edp10-dfabl,                           "UD1K919923
      w_vbrst like e1edp10-vbrst..                          "UD1K919923

data: begin of wa_mard,                                     "UD1K919923
       matnr like mard-matnr,                               "UD1K919923
       werks like mard-werks,                               "UD1K919923
       lgort like mard-lgort,                               "UD1K919923
       lgpbe like mard-lgpbe,                               "UD1K919923
      end of wa_mard.                                       "UD1K919923

data: it_mard like table of wa_mard.                        "UD1K919923


perform replace_qty_mrpqty tables dint_edidd using 'FOR'.

* Begin of changes -  UD1K919923
loop at dint_edidd into wa_edidd
          where segnam = 'E1EDP10'.
  clear: wa_e1edp10, it_mard.
  wa_e1edp10 = wa_edidd-sdata.
  w_matnr = wa_e1edp10-idnkd.
  w_werks = wa_e1edp10-kwerk.
  w_lgort = wa_e1edp10-klgor.

  select t1~matnr t1~werks t1~lgort t1~lgpbe
                             into table it_mard
                            from mard as t1
                            inner join mara as t2
                            on t1~matnr = t2~matnr
                            where t1~matnr = w_matnr
                              and t1~werks = w_werks
                              and t1~lgort = w_lgort
                              and t2~profl = 'V'.     "local parts

  read table it_mard into wa_mard with key matnr = w_matnr
                                           werks = w_werks
                                           lgort = w_lgort
                                           transporting lgpbe.
  if sy-subrc ne 0.
    wa_e1edp10-dfabl = space.
    wa_e1edp10-vbrst = space.
  else.
    split wa_mard-lgpbe at '*' into w_dfabl w_vbrst.
    wa_e1edp10-dfabl = w_dfabl.
    wa_e1edp10-vbrst = w_vbrst.
    clear: w_dfabl, w_vbrst.
  endif.
  wa_edidd-sdata = wa_e1edp10.
  modify dint_edidd from wa_edidd transporting sdata.

endloop.

* End of changes -   UD1K919923
