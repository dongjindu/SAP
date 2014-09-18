*&----------------------------------------------------------------------
*& Program ID        : ZECO_DUTY_DISTRIBUTION
*& Title             : [CO] - Duty Distribution Report
*& Created by        : Valerian Utama
*& Created on        : 11/29/2012
*& Specifications By : An Hyung Ki
*& Reference Pgm     : N/A
*& Description       : Distribute duty to Sold Material
*&
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*& 11/29/2012  Valerian  UD1K955915  Initial Program Development
*&
*&----------------------------------------------------------------------

REPORT  zeco_duty_distribution.

TYPE-POOLS: slis.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      gs_layout TYPE slis_layout_alv,
      gt_fieldcat2 TYPE slis_t_fieldcat_alv,
      gt_sort TYPE slis_t_sortinfo_alv,
      wa_sort TYPE slis_sortinfo_alv.

DATA: g_frstday TYPE sy-datum,
      g_lastday TYPE sy-datum,
      g_saved(1) TYPE c,
      g_srctot  TYPE bseg-wrbtr,
      g_sldtot  TYPE bseg-wrbtr,

      BEGIN OF it_mseg OCCURS 0,
        matnr TYPE mseg-matnr,
        kunnr TYPE mseg-kunnr,
        menge TYPE mseg-menge,
      END OF it_mseg,

      BEGIN OF it_mseg_sub OCCURS 0,
        matnr TYPE mseg-matnr,
        ummat TYPE mseg-ummat,
      END OF it_mseg_sub,

      BEGIN OF it_bseg OCCURS 0,
        matnr TYPE bseg-matnr,
        wrbtr TYPE bseg-wrbtr,
      END OF it_bseg,

      BEGIN OF it_matledger OCCURS 0,
        matnr TYPE ckmlhd-matnr,
        lbkum TYPE mlcd-lbkum,
        pmatn TYPE ckmlmv013-pmatn,
      END OF it_matledger,

      BEGIN OF it_data OCCURS 0,
        matn1 LIKE bseg-matnr,                   "Source Material
        wrbtr LIKE bseg-wrbtr,                   "Duty Amount
        matn2 LIKE ckmlhd-matnr,                 "Target Material
        lbkum LIKE mlcd-lbkum,                   "Consumed Qty
        aloc1 LIKE bseg-wrbtr,                   "Allocated Amt-1
        matn3 LIKE mseg-matnr,                   "Sold Material
        menge LIKE mseg-menge,                   "Sold Qty
        kunnr LIKE mseg-kunnr,                   "Customer
        class(10) TYPE c,                        "Class
        aloc2 LIKE bseg-wrbtr,                   "Allocated Amt-2
      END OF it_data,

      BEGIN OF it_summ OCCURS 0,
        matn3 LIKE mseg-matnr,                   "Sold Material
        menge LIKE mseg-menge,                   "Sold Qty
        kunnr LIKE mseg-kunnr,                   "Customer
        class(10) TYPE c,                        "Class
        aloc2 LIKE bseg-wrbtr,                   "Allocated Amt-2
      END OF it_summ,

      BEGIN OF it_total OCCURS 0,
        matn1 TYPE bseg-matnr,                   "Source Material
        lbkum TYPE mlcd-lbkum,                   "Total Consumed Qty
        menge TYPE mseg-menge,                   "Total Sold Qty
        aloc1 TYPE bseg-wrbtr,                   "Unallocated Amt/Mat
        used(1) TYPE c,                          "Check if it's used
      END OF it_total,

      BEGIN OF it_matlevel OCCURS 0,
        himat TYPE bseg-matnr,                   "Child Material
        lbkum TYPE mlcd-lbkum,                   "Total Consumed Qty
        lomat TYPE bseg-matnr,                   "Parent Material
      END OF it_matlevel,

it_duty_dist TYPE ztco_duty_dist OCCURS 0 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
PARAMETERS: p_bukrs TYPE bkpf-bukrs OBLIGATORY DEFAULT 'H201',
            p_gjahr TYPE bkpf-gjahr OBLIGATORY DEFAULT sy-datum(4),
            p_monat TYPE bkpf-monat OBLIGATORY DEFAULT sy-datum+4(2),
            p_hkont TYPE bseg-hkont OBLIGATORY DEFAULT '0000532160'.
SELECT-OPTIONS: s_matnr FOR it_mseg-matnr.
SELECTION-SCREEN END OF BLOCK blk1.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_HKONT'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  CONCATENATE p_gjahr p_monat '01' INTO g_frstday.

  CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = g_frstday
    IMPORTING
      e_date = g_lastday.

START-OF-SELECTION.
* Get required data
  PERFORM get_data.

* Process data
  PERFORM process_data.

* Display the result
  PERFORM display_data.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       Display the result
*----------------------------------------------------------------------*
FORM display_data.
  DATA: l_repid TYPE sy-repid.

  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = l_repid
      i_internal_tabname = 'IT_DATA'
      i_inclname         = l_repid
    CHANGING
      ct_fieldcat        = gt_fieldcat.

  PERFORM change_desc USING 'MATN1' 'Src.Material'.
  PERFORM change_desc USING 'WRBTR' 'Duty Amt'.
  PERFORM change_desc USING 'MATN2' 'Target Material'.
  PERFORM change_desc USING 'LBKUM' 'Consum.Qty'.
  PERFORM change_desc USING 'ALOC1' 'Alloc.Amt1'.
  PERFORM change_desc USING 'MATN3' 'Sold Material'.
  PERFORM change_desc USING 'MENGE' 'Sold Qty'.
  PERFORM change_desc USING 'CLASS' 'Class'.
  PERFORM change_desc USING 'ALOC2' 'Alloc.Amt2'.

  PERFORM change_cat USING 'ALOC2' 'DO_SUM' 'X'.

  PERFORM set_sorting USING: 'MATN1' 'X'.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = l_repid
      i_callback_pf_status_set = 'SET_MAIN'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      it_sort                  = gt_sort
    TABLES
      t_outtab                 = it_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  CHANGE_DESC
*&---------------------------------------------------------------------*
*       Change ALV field description
*----------------------------------------------------------------------*
FORM change_desc  USING    p_field TYPE c
                           p_desc  TYPE c.

  READ TABLE gt_fieldcat INTO wa_fieldcat
                         WITH KEY fieldname = p_field.
  IF sy-subrc = 0.
    wa_fieldcat-seltext_l    = p_desc.
    wa_fieldcat-seltext_m    = p_desc.
    wa_fieldcat-seltext_s    = p_desc.
    wa_fieldcat-reptext_ddic = p_desc.
    MODIFY gt_fieldcat FROM wa_fieldcat INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " CHANGE_DESC

*&---------------------------------------------------------------------*
*&      Form  CHANGE_DESC2
*&---------------------------------------------------------------------*
*       Change ALV field description
*----------------------------------------------------------------------*
FORM change_desc2  USING    p_field TYPE c
                            p_desc  TYPE c.

  READ TABLE gt_fieldcat2 INTO wa_fieldcat
                          WITH KEY fieldname = p_field.
  IF sy-subrc = 0.
    wa_fieldcat-seltext_l    = p_desc.
    wa_fieldcat-seltext_m    = p_desc.
    wa_fieldcat-seltext_s    = p_desc.
    wa_fieldcat-reptext_ddic = p_desc.
    MODIFY gt_fieldcat2 FROM wa_fieldcat INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " CHANGE_DESC2

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SUMM
*&---------------------------------------------------------------------*
*       Display the summary
*----------------------------------------------------------------------*
FORM display_summ.
  DATA: l_repid TYPE sy-repid.

  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = l_repid
      i_internal_tabname = 'IT_SUMM'
      i_inclname         = l_repid
    CHANGING
      ct_fieldcat        = gt_fieldcat2.

  PERFORM change_desc2 USING 'MATN3' 'Sold Material'.
  PERFORM change_desc2 USING 'MENGE' 'Sold Qty'.
  PERFORM change_desc2 USING 'CLASS' 'Class'.
  PERFORM change_desc2 USING 'ALOC2' 'Alloc.Amt2'.

  PERFORM change_cat2 USING 'ALOC2' 'DO_SUM' 'X'.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = l_repid
      i_callback_pf_status_set = 'SET_SUMM'
      i_callback_user_command  = 'USER_COMMAND2'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat2
    TABLES
      t_outtab                 = it_summ
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_SUMM

*&---------------------------------------------------------------------*
*&      Form  SET_MAIN
*&---------------------------------------------------------------------*
*       Set Main Menu
*----------------------------------------------------------------------*
FORM set_main USING rt_extab TYPE slis_t_extab.             "#EC CALLED
  SET PF-STATUS 'MAIN' EXCLUDING rt_extab.
ENDFORM.                    "SET_MAIN

*&---------------------------------------------------------------------*
*&      Form  SET_SUMM
*&---------------------------------------------------------------------*
*       Set Summary Menu
*----------------------------------------------------------------------*
FORM set_summ USING rt_extab TYPE slis_t_extab.             "#EC CALLED
  SET PF-STATUS 'SUMM' EXCLUDING rt_extab.
ENDFORM.                    "SET_SUMM

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       User Command for Main Menu
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.     "#EC CALLED
  CASE r_ucomm.
    WHEN '&SUMM'.

      IF it_summ[] IS INITIAL.
        LOOP AT it_data WHERE matn3 <> ' '.
          MOVE-CORRESPONDING it_data TO it_summ.
          COLLECT it_summ.
        ENDLOOP.

        SORT it_summ BY matn3.
      ENDIF.

      PERFORM display_summ.

  ENDCASE.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  user_command2
*&---------------------------------------------------------------------*
*       User Command for Summary Menu
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command2 USING r_ucomm LIKE sy-ucomm
                         rs_selfield TYPE slis_selfield.    "#EC CALLED
  CASE r_ucomm.
    WHEN '&SAVE'.

      IF NOT s_matnr[] IS INITIAL.
        MESSAGE 'Re-run program for entire Source Materials'
           TYPE 'I'.
        EXIT.
      ENDIF.

      IF g_saved IS INITIAL.
        LOOP AT it_summ.
          it_duty_dist-bukrs = p_bukrs.
          it_duty_dist-gjahr = p_gjahr.
          it_duty_dist-monat = p_monat.
          it_duty_dist-matnr = it_summ-matn3.
          it_duty_dist-kunnr = it_summ-kunnr.
          it_duty_dist-menge = it_summ-menge.
          it_duty_dist-class = it_summ-class.
          it_duty_dist-wrbtr = it_summ-aloc2.

          APPEND it_duty_dist.
        ENDLOOP.

        DELETE FROM ztco_duty_dist WHERE bukrs = p_bukrs
                                     AND gjahr = p_gjahr
                                     AND monat = p_monat.

        INSERT ztco_duty_dist FROM TABLE it_duty_dist.
        IF sy-subrc = 0.
          COMMIT WORK.
          g_saved = 'X'.
          MESSAGE 'Data Saved' TYPE 'I'.
        ELSE.
          ROLLBACK WORK.
          MESSAGE 'Error' TYPE 'I'.
        ENDIF.
      ELSE.
        MESSAGE 'Data Has Already Been Saved' TYPE 'I'.
      ENDIF.

  ENDCASE.
ENDFORM.                    "user_command2

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get required data from the system
*----------------------------------------------------------------------*
FORM get_data.
* Get Sold Materials
  SELECT matnr kunnr SUM( menge ) AS menge
    INTO CORRESPONDING FIELDS OF TABLE it_mseg
    FROM mseg
   WHERE zbudat BETWEEN g_frstday AND g_lastday
     AND ( bwart = '601'
      OR   bwart = '602'
      OR   bwart = '961'
      OR   bwart = '962' )
   GROUP BY matnr kunnr.

  IF it_mseg[] IS INITIAL.
    MESSAGE 'No Sold Material Found' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

* Get Substitute Materials
  SELECT DISTINCT matnr ummat
    INTO CORRESPONDING FIELDS OF TABLE it_mseg_sub
    FROM mseg
   WHERE zbudat BETWEEN g_frstday AND g_lastday
     AND ( bwart = '309'
      OR   bwart = '310' )
     AND ( werks = 'E001'
      OR   werks = 'E002' ).

  SELECT a~matnr SUM( a~dstamt ) AS wrbtr
    INTO CORRESPONDING FIELDS OF TABLE it_bseg
    FROM ztcou124 AS a JOIN rbkp AS b
                       ON b~belnr = a~belum AND
                          b~gjahr = a~gjahr

   WHERE a~pdate BETWEEN g_frstday AND g_lastday
     AND a~matnr IN s_matnr
     AND a~bukrs = p_bukrs
     AND a~gjahr = p_gjahr
     AND a~hkont = p_hkont
     AND b~stblg = ' '
   GROUP BY a~matnr.

  IF it_bseg[] IS INITIAL.
    MESSAGE 'No Source Material Found' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

* Get Target Materials
  SORT: it_bseg BY matnr,
        it_mseg BY matnr kunnr,
        it_mseg_sub BY matnr.

  CLEAR g_srctot.
  LOOP AT it_bseg.
    g_srctot = g_srctot + it_bseg-wrbtr.

    CLEAR: it_matlevel, it_matlevel[].

    PERFORM get_matlevel TABLES it_matlevel
                         USING  it_bseg-matnr.
* Get BOM Material
    LOOP AT it_matlevel.
      READ TABLE it_mseg WITH KEY matnr = it_matlevel-himat
                              BINARY SEARCH.

      IF sy-subrc = 0.
        it_matledger-matnr = it_bseg-matnr.
        it_matledger-lbkum = it_matlevel-lbkum.
        it_matledger-pmatn = it_matlevel-himat.

        COLLECT it_matledger.
        CONTINUE.
      ENDIF.

      PERFORM get_matlevel TABLES it_matlevel
                           USING it_matlevel-himat.

    ENDLOOP.
  ENDLOOP.

  IF it_matledger[] IS INITIAL.
    MESSAGE 'No Target Material Found' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       Process data
*----------------------------------------------------------------------*
FORM process_data .
  DATA: l_diff  TYPE bseg-wrbtr,
        l_tabix TYPE sy-tabix VALUE 1,
        l_wrbtr TYPE bseg-wrbtr,
        l_menge TYPE mseg-menge.

* Populate structure for calculation
  SORT: it_bseg BY matnr,
        it_matledger BY matnr.

  LOOP AT it_bseg.
    it_data-matn1 = it_bseg-matnr.
    it_data-wrbtr = it_bseg-wrbtr.

    LOOP AT it_matledger FROM l_tabix WHERE matnr = it_bseg-matnr.
      l_tabix = sy-tabix.

      it_data-matn2 = it_matledger-pmatn.
      it_data-lbkum = it_matledger-lbkum.

      READ TABLE it_mseg WITH KEY matnr = it_matledger-pmatn
                              BINARY SEARCH.
      IF sy-subrc = 0.
        it_data-matn3 = it_mseg-matnr.
        it_data-kunnr = it_mseg-kunnr.
        it_data-menge = it_mseg-menge.
      ENDIF.

      APPEND it_data.
    ENDLOOP.
    l_tabix = l_tabix + 1.

    IF sy-subrc <> 0.
      APPEND it_data.
    ENDIF.

    CLEAR it_data.
  ENDLOOP.

  FREE: it_bseg, it_matledger, it_mseg.

* Get Total Quantity and Unallocated Amount
  CLEAR it_total.
  LOOP AT it_data WHERE matn2 <> ' '.
    MOVE-CORRESPONDING it_data TO it_total.
    COLLECT it_total. CLEAR it_total.

    l_menge = l_menge + it_data-menge.
  ENDLOOP.

* Calculate Allocated Amount-1 and populate the structure
  SORT it_total BY matn1.
  LOOP AT it_data WHERE lbkum <> 0.
    l_tabix = sy-tabix.

    READ TABLE it_total WITH KEY matn1 = it_data-matn1
                             BINARY SEARCH.
    IF sy-subrc = 0.
      it_data-aloc1 = it_data-wrbtr * it_data-lbkum / it_total-lbkum.
      MODIFY it_data INDEX l_tabix TRANSPORTING aloc1.
    ENDIF.
  ENDLOOP.

* Calculate Unallocated Amount-1/Source Material
  CLEAR it_total.
  LOOP AT it_data WHERE matn3 = ' '.
    IF it_data-matn2 <> ' '.
      it_total-matn1 = it_data-matn1.
      it_total-aloc1 = it_data-aloc1.
    ELSE.
      l_wrbtr = l_wrbtr + it_data-wrbtr.         "Unallocated Src.Mat.
    ENDIF.

    COLLECT it_total. CLEAR it_total.
  ENDLOOP.

* Calculate Allocated Amount-2 and populate class.
  CLEAR g_sldtot.
  LOOP AT it_data WHERE matn3 <> ' '.
    l_tabix = sy-tabix.

    CLEAR it_total.
    READ TABLE it_total WITH KEY matn1 = it_data-matn1
                             BINARY SEARCH.

    it_data-aloc2 = it_data-aloc1 +
                    it_total-aloc1 * it_data-menge / it_total-menge +
                    l_wrbtr * it_data-menge / l_menge.

    g_sldtot = g_sldtot + it_data-aloc2.

    CASE it_data-kunnr.
      WHEN 'AKNH'.
        it_data-class = 'SFG'.
      WHEN 'MOBIS'.
        it_data-class = 'SFG'.
      WHEN OTHERS.
        IF it_data-kunnr+1(2) = '28'.
          it_data-class = 'FG DOM'.
        ELSEIF NOT it_data-kunnr IS INITIAL.
          it_data-class = 'FG EXP'.
        ENDIF.
    ENDCASE.

    MODIFY it_data INDEX l_tabix TRANSPORTING class aloc2.
  ENDLOOP.

  FREE it_total.

* Correct the total Sold to total amount.
* Distribute the difference proportionaly by Sold Qty
  IF g_srctot <> g_sldtot.
    l_diff = g_srctot - g_sldtot.
    CLEAR: g_sldtot.
    LOOP AT it_data.
      it_data-aloc2 = it_data-aloc2 +
                      l_diff * it_data-menge / l_menge.
      g_sldtot = g_sldtot + it_data-aloc2.
      MODIFY it_data INDEX sy-tabix TRANSPORTING aloc2.
    ENDLOOP.
  ENDIF.

* Refine the process.
* Put the difference in the largest amount.
  IF g_srctot <> g_sldtot.
    SORT it_data BY aloc2 DESCENDING.
    READ TABLE it_data INDEX 1.
    IF sy-subrc = 0.
      it_data-aloc2 = it_data-aloc2 + ( g_srctot - g_sldtot ).
      MODIFY it_data INDEX 1 TRANSPORTING aloc2.
    ENDIF.
  ENDIF.

  SORT it_data BY matn1 aloc2 DESCENDING.

** Furong on 12/20/12  * -1
  LOOP AT it_data.
      l_tabix = sy-tabix.
      it_data-aloc2 = it_data-aloc2 * -1.
      it_data-wrbtr = it_data-wrbtr * -1.
      MODIFY it_data index l_tabix.
  ENDLOOP.
** End
ENDFORM.                    " PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  CHANGE_CAT
*&---------------------------------------------------------------------*
*       Change field catalog
*----------------------------------------------------------------------*
*      -->P_FIELDNAME   Field Name
*      -->P_PROPERTY    Property
*      -->P_VALUE       Value
*----------------------------------------------------------------------*
FORM change_cat  USING    p_fieldname TYPE c
                          p_property  TYPE c
                          p_value     TYPE c.

  FIELD-SYMBOLS: <fs> TYPE any.

  READ TABLE gt_fieldcat INTO wa_fieldcat
                    WITH KEY fieldname = p_fieldname.
  IF sy-subrc = 0.
    ASSIGN COMPONENT p_property OF STRUCTURE wa_fieldcat TO <fs>.
    IF sy-subrc = 0.
      <fs> = p_value.
      MODIFY gt_fieldcat FROM wa_fieldcat INDEX sy-tabix.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHANGE_CAT

*&---------------------------------------------------------------------*
*&      Form  CHANGE_CAT2
*&---------------------------------------------------------------------*
*       Change field catalog
*----------------------------------------------------------------------*
*      -->P_FIELDNAME   Field Name
*      -->P_PROPERTY    Property
*      -->P_VALUE       Value
*----------------------------------------------------------------------*
FORM change_cat2  USING    p_fieldname TYPE c
                           p_property  TYPE c
                           p_value     TYPE c.

  FIELD-SYMBOLS: <fs> TYPE any.

  READ TABLE gt_fieldcat2 INTO wa_fieldcat
                    WITH KEY fieldname = p_fieldname.
  IF sy-subrc = 0.
    ASSIGN COMPONENT p_property OF STRUCTURE wa_fieldcat TO <fs>.
    IF sy-subrc = 0.
      <fs> = p_value.
      MODIFY gt_fieldcat2 FROM wa_fieldcat INDEX sy-tabix.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHANGE_CAT2

*&---------------------------------------------------------------------*
*&      Form  SET_SORTING
*&---------------------------------------------------------------------*
*       Set report sorting
*----------------------------------------------------------------------*
*      -->P_FIELDNAME  Fieldname
*----------------------------------------------------------------------*
FORM set_sorting USING p_fieldname TYPE c p_subtot TYPE c.
  STATICS: l_spos(2) TYPE n.
  l_spos = l_spos + 1.

  wa_sort-spos = l_spos.
  wa_sort-fieldname = p_fieldname.
  wa_sort-up = 'X'.
  wa_sort-subtot = p_subtot.
  APPEND wa_sort TO gt_sort.
ENDFORM.                    " SET_SORTING
*&---------------------------------------------------------------------*
*&      Form  GET_MATLEVEL
*&---------------------------------------------------------------------*
*       Get Level Material
*----------------------------------------------------------------------*
*      -->PT_MATLEVEL  List of BOM Material
*      -->P_MATNR      Child Material
*----------------------------------------------------------------------*
FORM get_matlevel  TABLES pt_matlevel STRUCTURE it_matlevel
                   USING  p_matnr     LIKE bseg-matnr.

  SELECT c~pmatn AS himat b~lbkum a~matnr AS lomat
    APPENDING CORRESPONDING FIELDS OF TABLE pt_matlevel
    FROM ckmlhd AS a JOIN mlcd AS b
                       ON b~kalnr = a~kalnr
                      AND b~bdatj = p_gjahr
                      AND b~poper = p_monat
                      AND b~categ = 'VN'

                      JOIN ckmlmv013 AS c
                        ON c~kalnr_proc = b~bvalt
    WHERE a~matnr = p_matnr.

* No Material found, find substitute materials
  IF sy-subrc <> 0.
    READ TABLE it_mseg_sub WITH KEY matnr = p_matnr.
    IF sy-subrc = 0.
      SELECT c~pmatn AS himat b~lbkum a~matnr AS lomat
        APPENDING CORRESPONDING FIELDS OF TABLE pt_matlevel
        FROM ckmlhd AS a JOIN mlcd AS b
                           ON b~kalnr = a~kalnr
                          AND b~bdatj = p_gjahr
                          AND b~poper = p_monat
                          AND b~categ = 'VN'

                          JOIN ckmlmv013 AS c
                            ON c~kalnr_proc = b~bvalt
        WHERE a~matnr = it_mseg_sub-ummat.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_MATLEVEL
