* XZQP9CK114420 17.01.01 Persönliche Vorlagen in CATS
* XZQP9CK076216 14.12.00 Ergebnisobjekt in CATS
* 4.6C
* 4.6A
* XQPALRK169209 170198 Überspringen Einstiegsbild
*eject
*&---------------------------------------------------------------------*
*&      Form  PROCESS_OK_CODE
*&---------------------------------------------------------------------*
*       OK-Code-Control                                                *
*----------------------------------------------------------------------*
FORM process_ok_code.
  MOVE ok_code TO save_ok_code.
  IF ok_code EQ fc-pchk or
     ok_code eq 'CBOX ' or  "Dummy OK code for Checkboxes
     ok_code eq 'RADIO'.    "Dummy OK code for Radio button
*   Do nothing - for Tab strip
  else.
    MOVE ok_code TO ts_profile-activetab.
  ENDIF.
  CLEAR ok_code.
  PERFORM get_cursor.

  CASE save_ok_code.
* if the user clicked on 'Quota Overview'.
    when 'QOVW'.
      clear icatsd-pernr.
      LOOP AT icatsd where mark eq 'X'.
        SET PARAMETER ID 'PER' FIELD icatsd-PERNR.
        EXIT.
      ENDLOOP.
      if not icatsd-pernr is initial.
        CALL TRANSACTION 'PT50' AND SKIP FIRST SCREEN.
      else.
        message i084 with 'Please select an employee'.
      endif.
* if the user has clicked on 'Distribute OT' button then distribute the
* hours to all the selected employees.
    when 'DSOT'.
      perform distribute_OT_hours.

* view approval
*   WHEN FC-APPR.
*     PERFORM FC_APPR.
* approve data
*   WHEN FC-APRO.
*     PERFORM FC_APRO.
* Agents
    WHEN fc-agen.
      PERFORM fc_agen.
* Back
    WHEN fc-back.
      PERFORM fc_back.
* Jump to maintanance-Transaction
    WHEN fc-cat2.
      PERFORM fc_cat2.
* Jump to display-Transaction
    WHEN fc-cat3.
      PERFORM fc_cat3.
* Check entries
    WHEN fc-chck.
      PERFORM fc_chck.
* Copy lines from worklist or entry list
    WHEN fc-copy.
      PERFORM fc_copy.
* Copy previous period
    WHEN fc-cppp.
      PERFORM fc_cppp.
* Display days with or witout text
    WHEN fc-dayt.
      PERFORM fc_dayt.
* Delete
    WHEN fc-dele.
      PERFORM delete_marked_lines.
* Detail screen
    WHEN fc-deta.
      PERFORM fc_deta.
* End
    WHEN fc-ende.
      PERFORM fc_ende.
* Proceed
    WHEN fc-ente.
      PERFORM fc_ente.
* F4 help for work flow object ID on popup - ITS use only   "LUX
*   WHEN FC-F4WF.                                           "LUX
*     PERFORM FC_F4WF.                                      "LUX
* view: release data
    WHEN fc-free.
      PERFORM fc_free.
* Jump to initial screen                                    "XQPK169209
    WHEN fc-iscr.                                           "XQPK169209
      PERFORM fc_iscr.                                      "XQPK169209
* Insert line
    WHEN fc-inse.
      PERFORM fc_inse.
* controlling area
    WHEN fc-koks.
      PERFORM set_kokrs USING '1'.
* scroll in time range left
    WHEN fc-lef1.
      PERFORM scroll_left1.
* scroll in time range left
    WHEN fc-lef2.
      PERFORM scroll_left2.
* legend
    WHEN fc-lege.
      PERFORM fc_lege.
* mark all entries
    WHEN fc-maal.
      PERFORM fc_maal.
* delete mark all entries on status popup
    WHEN fc-maa1.
      PERFORM fc_maa1.
* mark all entries on status popup
    WHEN fc-maa2.
      PERFORM fc_maa2.
* Search and mark entries
    WHEN fc-masl.
      PERFORM fc_masl.
* set mf
    WHEN fc-mast.
      PERFORM fc_mast.
* New lines
    WHEN fc-newl.
      PERFORM fc_newl.
* Next intervall
    WHEN fc-next.
      PERFORM fc_next.
* Jump to report for person selection                       "XQPK160176
    WHEN fc-pers.                                           "XQPK160176
      PERFORM fc_pers.                                      "XQPK160176
* pick cell
    WHEN fc-pick.
      PERFORM fc_pick.
* Filter personnel numbers
    WHEN fc-pfil.
*     perform fc_pfil.
* Mark all personnel numbers
    WHEN fc-pmal.
      PERFORM fc_pmal.
* Profile (Checks)
    WHEN fc-prck.
      PERFORM fc_prck.
* Profile (Variante Kostenrechnung)                       "LUX
    WHEN fc-prco.                                           "LUX
      PERFORM fc_prco.                                      "LUX
* Predecessor (on popup 8200)
    WHEN fc-pred.
      PERFORM fc_pred.
* Profile (Defaultvalues)
    WHEN fc-prde.
      PERFORM fc_prde.
* Previous intervall
    WHEN fc-prev.
      PERFORM fc_prev.
* Profile (General)
    WHEN fc-prge.
      PERFORM fc_prge.
* Print
    WHEN fc-pri.
      PERFORM fc_pri.
* Check protocol
    WHEN fc-prot.
      PERFORM fc_prot.
* Profile (Persons)
    WHEN fc-prpe.
      PERFORM fc_prpe.
* Profile (Time)
    WHEN fc-prti.
      PERFORM fc_prti.
* Profile (Workflow).                                     "LUX
    WHEN fc-prwf.                                           "LUX
      PERFORM fc_prwf.                                      "LUX
* Profile (Worklist).
    WHEN fc-prwl.
      PERFORM fc_prwl.
* Jump to report for person selection                       "XQPK160176
*   WHEN FC-PERS.                                           "XQPK160176
* Sort persons downwards
    WHEN fc-psrd.
      PERFORM fc_psrd.
* Sort persons upwards
    WHEN fc-psru.
      PERFORM fc_psru.
* Unmark all personnel numbers
    WHEN fc-puma.
      PERFORM fc_puma.
* First page
    WHEN fc-pmm.
      PERFORM scroll USING save_ok_code tc_catsd-top_line
                           save_sy_loopc real_number_of_lines.
* Privious page
    WHEN fc-pm.
      PERFORM scroll USING save_ok_code tc_catsd-top_line
                           save_sy_loopc real_number_of_lines.
* Next page
    WHEN fc-pp.
      PERFORM scroll USING save_ok_code tc_catsd-top_line
                           save_sy_loopc real_number_of_lines.
* Last Page
    WHEN fc-ppp.
      PERFORM scroll USING save_ok_code tc_catsd-top_line
                           save_sy_loopc real_number_of_lines.
* Reset entries
    WHEN fc-rese.
      PERFORM fc_rese.
* scroll in time range right
    WHEN fc-rig1.
      PERFORM scroll_right1.
* scroll in time range right
    WHEN fc-rig2.
      PERFORM scroll_right2.
* Save for approval
    WHEN fc-saap.
      PERFORM fc_saap.
* Save
    WHEN fc-save.
      PERFORM fc_save.
* Show time data
    WHEN fc-show.
      PERFORM fc_show.
* ENTER
    WHEN space.
      PERFORM fc_space.
* split line
    WHEN fc-spli.
      PERFORM fc_spli.
* search
    WHEN fc-srch.
      PERFORM fc_srch.
* continue search
    WHEN fc-srcp.
      PERFORM fc_srcp.
* Sort (ascending)
    WHEN fc-srtu.
      PERFORM fc_srtu.
* Sort (descending)
    WHEN fc-srtd.
      PERFORM fc_srtd.
* Standardview to data
    WHEN fc-stan.
      PERFORM fc_stan.
* Generel customer filter
    WHEN fc-stat.
      PERFORM fc_stat.
* Summation on/off
    WHEN fc-sum.
      PERFORM fc_sum.
    WHEN fc-text.
* Jump to the text processing
      PERFORM fc_text.
    WHEN fc-tex1.
* Display texts
      PERFORM fc_tex1.
* unmark all entries
    WHEN fc-umal.
      PERFORM fc_umal.
* Jump to the target document
* this is done on popup 8200
*   WHEN FC-TARG.
*     PERFORM FC_TARG.
* Distribution of hours
    WHEN fc-tifx.
      PERFORM fc_tifx.
* Distribution of hours
    WHEN fc-tihr.
      PERFORM fc_tihr.
* Process time data
    WHEN fc-time.
      PERFORM fc_time.
* Travel expenses
    WHEN fc-trav.
      PERFORM fc_trav.
* Profile
    WHEN fc-vari.
      PERFORM fc_vari.
* Target hours on/off
    WHEN fc-vson.
      PERFORM fc_vson.
* Withdrawal
    WHEN fc-with.
      PERFORM fc_with.
* Workdays on/off
    WHEN fc-wday.
      PERFORM fc_wday.
* Customer function 1: additional fields
    WHEN fc-zcu1.
      PERFORM fc_zcu1.
* Customer functions on main screen and entry screen        "XQPK158983
    WHEN fc-zcu2 OR fc-zcu3 OR fc-zcu4 OR fc-zcu5           "XQPK158983
                 OR fc-zcu6 OR fc-zcu7.                     "XDEK158983
      PERFORM fc_zcun USING save_ok_code.                   "XQPK158983
    WHEN fc-paob.                      "XZQP9CK076216
      PERFORM fc-paob.                 "XZQP9CK076216
*--------------------Beginn XZQP9CK114420----------------------
    WHEN FC-TMSV.
      PERFORM FC-TMSV.
    WHEN FC-TMDE.
      PERFORM FC-TMDE.
*--------------------Ende XZQP9CK114420----------------------

  ENDCASE.
ENDFORM.                               " PROCESS_OK_CODE
