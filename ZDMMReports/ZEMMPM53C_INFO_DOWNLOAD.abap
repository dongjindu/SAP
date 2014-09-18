*&--------------------------------------------------------------------
*& REPORT                 : ZEMMPM53C_INFO_DOWNLOAD
*& Author                 : WSKIM
*& Creation Date          : 02/17/2005
*& Specification By       :
*& Pattern                : Report 1-1 1
*& Development Request No :
*& Addl documentation     :
*& Description            :  Info record download
*& Modification Log
*& Date       Developer    Request ID      Description
*&
*&--------------------------------------------------------------------

INCLUDE zemmpm53c_info__download_top.
*----------------------------------------------------------------------
* INITIALIZATION
*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM selection_output.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN .
*  PERFORM selection_screen.
*---------------------------------------------------------------------
*    M   A   I   N
*---------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM get_data.
  PERFORM download.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  REFRESH it_eina.
  CLEAR w_int.
  SELECT a~infnr a~matnr a~lifnr a~urzzt c~ekorg c~aplfz
         c~ekgrp c~norbm c~uebtk c~bstae c~webre c~mwskz
         c~netpr c~peinh c~waers a~loekz c~werks
    INTO CORRESPONDING FIELDS OF TABLE it_eina
      FROM ( eina AS a INNER JOIN mara AS b
              ON a~matnr = b~matnr )
            INNER JOIN eine AS c
              ON a~infnr = c~infnr
         WHERE b~mtart EQ 'ROH'
           AND c~ekorg EQ 'PU01'
           AND a~lifnr IN s_lifnr.

  DESCRIBE TABLE it_eina LINES w_int.
  IF w_int <> 0.
    PERFORM filled_data_condition.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  filled_data_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM filled_data_condition.
  DATA : it_a018 LIKE a018 OCCURS 0 WITH HEADER LINE,
         it_ztmm_cond LIKE ztmm_cond OCCURS 0 WITH HEADER LINE.

  LOOP AT it_eina.
    REFRESH it_a018.
*Price history
    SELECT * INTO TABLE it_a018
      FROM a018
       WHERE kappl EQ 'M'
         AND lifnr EQ it_eina-lifnr
         AND matnr EQ it_eina-matnr
         AND ekorg EQ it_eina-ekorg.
*Amortization Quantity
    REFRESH it_ztmm_cond.
    SELECT * INTO TABLE it_ztmm_cond
      FROM ztmm_cond
       WHERE lifnr EQ it_eina-lifnr
         AND matnr EQ it_eina-matnr
         AND ekorg EQ it_eina-ekorg.

    CLEAR w_int.
    DESCRIBE TABLE it_a018 LINES w_int.
    IF w_int <> 0.
      LOOP AT it_a018.

        MOVE: it_a018-datbi TO it_out-datbi,
              it_a018-datab TO it_out-datab.

        CLEAR w_int.REFRESH it_konp.
        SELECT a~kosrt a~kzust a~ernam a~erdat b~kschl b~kbetr
               b~loevm_ko
           INTO TABLE it_konp
         FROM konh AS a INNER JOIN konp AS b
          ON a~knumh = b~knumh
          WHERE a~knumh EQ it_a018-knumh.

        DESCRIBE TABLE it_konp LINES w_int.
        IF w_int <> 0.
          MOVE-CORRESPONDING it_eina TO it_out.

          LOOP AT it_konp.
            MOVE-CORRESPONDING it_konp TO it_out.
            IF it_konp-loevm_ko EQ 'X'.
              it_out-del_con = 'X'.
            ENDIF.
            PERFORM match_field_conditon USING it_konp
                                         CHANGING it_out.
          ENDLOOP.
*Amortization Quantity
          LOOP AT it_ztmm_cond.
            PERFORM qty_mapping USING it_ztmm_cond.
          ENDLOOP.
*deletion flag check:GRNERAL
          IF it_eina-loekz EQ 'X'. "
            it_out-del_inf = 'X'.
          ENDIF.
*check : only exist general view
          SELECT SINGLE * FROM eine
            WHERE infnr EQ it_eina-infnr
              AND ekorg EQ it_eina-ekorg
              AND werks EQ space.
          IF sy-subrc <> 0.
            it_out-only_gen = 'X'.
          ENDIF.

          APPEND it_out.CLEAR it_out.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " filled_data_condition
*&---------------------------------------------------------------------*
*&      Form  MATCH_FIELD_CONDITON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_KONP  text
*----------------------------------------------------------------------*
FORM match_field_conditon USING lt_konp LIKE it_konp
                          CHANGING lt_out LIKE it_out.
  CASE lt_konp-kschl.
    WHEN 'PB00'.
      MOVE lt_konp-kbetr TO lt_out-netpr.
    WHEN 'FRA1'.
      MOVE lt_konp-kbetr TO lt_out-fra1.
      lt_out-fra1 = lt_out-fra1 / 10.
*   WHEN 'FRA2'.
    WHEN 'ZOTH'.
      MOVE lt_konp-kbetr TO lt_out-zoth.
      lt_out-zoth = lt_out-zoth / 10.
    WHEN 'ZOTI'.
      MOVE lt_konp-kbetr TO lt_out-zoti.
      lt_out-zoti = lt_out-zoti / 10.
    WHEN 'ZP01'.
      MOVE lt_konp-kbetr TO lt_out-zp01.
    WHEN 'ZP02'.
      MOVE lt_konp-kbetr TO lt_out-zp02.
    WHEN 'ZP03'.
      MOVE lt_konp-kbetr TO lt_out-zp03.
    WHEN 'ZP04'.
      MOVE lt_konp-kbetr TO lt_out-zp04.
    WHEN 'ZP05'.
      MOVE lt_konp-kbetr TO lt_out-zp05.
    WHEN 'ZP06'.
      MOVE lt_konp-kbetr TO lt_out-zp06.
    WHEN 'ZP07'.
      MOVE lt_konp-kbetr TO lt_out-zp07.
    WHEN 'ZP08'.
      MOVE lt_konp-kbetr TO lt_out-zp08.
    WHEN 'ZP09'.
      MOVE lt_konp-kbetr TO lt_out-zp09.
    WHEN 'ZP10'.
      MOVE lt_konp-kbetr TO lt_out-zp10.
    WHEN 'ZP11'.
      MOVE lt_konp-kbetr TO lt_out-zp11.
    WHEN 'ZP12'.
      MOVE lt_konp-kbetr TO lt_out-zp12.
    WHEN 'ZP13'.
      MOVE lt_konp-kbetr TO lt_out-zp13.
    WHEN 'ZP14'.
      MOVE lt_konp-kbetr TO lt_out-zp14.
    WHEN 'ZP15'.
      MOVE lt_konp-kbetr TO lt_out-zp15.
    WHEN 'ZP16'.
      MOVE lt_konp-kbetr TO lt_out-zp16.
  ENDCASE.
ENDFORM.                    " MATCH_FIELD_CONDITON
*&---------------------------------------------------------------------*
*&      Form  download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download.
  IF p_e EQ 'X'.
    PERFORM excel_download.
  ELSEIF p_t EQ 'X'.
    PERFORM text_download.
  ENDIF.

ENDFORM.                    " download
*&---------------------------------------------------------------------*
*&      Form  FILL_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROW  text
*      -->P_4      text
*      -->P_T_VALUE  text
*----------------------------------------------------------------------*
FORM fill_cell USING i j val.
  CALL METHOD OF excel 'CELLS' = cell EXPORTING #1 = i #2 = j.
  SET PROPERTY OF cell 'VALUE' = val.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONVERT_TO_CHAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB2_ZZAMT3  text
*      -->P_T_VALUE  text
*----------------------------------------------------------------------*
FORM convert_to_char USING amount char.
  char = amount.
  IF amount < 0.
    CONCATENATE '-' char INTO char.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  qty_mapping
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZTMM_COND  text
*----------------------------------------------------------------------*
FORM qty_mapping USING    p_ztmm_cond LIKE ztmm_cond.
  CASE p_ztmm_cond-kschl.
    WHEN 'ZP01'.
      MOVE p_ztmm_cond-menge TO it_out-qty1.
    WHEN 'ZP02'.
      MOVE p_ztmm_cond-menge TO it_out-qty2.
    WHEN 'ZP03'.
      MOVE p_ztmm_cond-menge TO it_out-qty3.
  ENDCASE.
ENDFORM.                    " qty_mapping
*&---------------------------------------------------------------------*
*&      Form  progress_indicator
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0674   text
*----------------------------------------------------------------------*
FORM progress_indicator USING   p_%  p_wint.
  DATA : text(30),
         indi(2) TYPE n,
         indi_half TYPE i.

  indi = ( p_% / p_wint ) * 100.

  IF p_% = 1.
    text(10) = 'Extracting'.
    text+10(3) = p_%.
    text+13(8) = '% ing...'.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              percentage = p_%
              text       = text.

  ELSEIF indi+1(1) = 0.

    text(10) = 'Extracting'.
    text+10(3) = indi.
    text+13(8) = '% ing...'.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              percentage = indi
              text       = text.
  ENDIF.
  IF p_% = p_wint.
    MESSAGE s999 WITH 'Completed download'.
  ENDIF.
ENDFORM.                    " progress_indicator
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_download.
  DATA : row TYPE i,
         c_num TYPE i.
  CREATE OBJECT excel 'EXCEL.APPLICATION'.
  CALL METHOD OF excel 'WORKBOOKS' = books.
  CALL METHOD OF books 'ADD' = book.

* TITLE.
  PERFORM fill_cell USING 2 1  'Vendor'.
  PERFORM fill_cell USING 2 2  'Material'.
  PERFORM fill_cell USING 2 3  'Pur Org'.
  PERFORM fill_cell USING 2 4  'Number'.
  PERFORM fill_cell USING 2 5  'Plnd Time'.
  PERFORM fill_cell USING 2 6  'Pur Group'.
  PERFORM fill_cell USING 2 7  'Standard Qty'.
  PERFORM fill_cell USING 2 8  'Unlimited'.
  PERFORM fill_cell USING 2 9  'Confcontrk'.
  PERFORM fill_cell USING 2 10 'GR Based'.
  PERFORM fill_cell USING 2 11 'Tax code'.
  PERFORM fill_cell USING 2 12 'Net Price'.
  PERFORM fill_cell USING 2 13 'Price Unit'.
  PERFORM fill_cell USING 2 14 'Currency'.
  PERFORM fill_cell USING 2 15 'Valid From'.
  PERFORM fill_cell USING 2 16 'Valid To'.
  PERFORM fill_cell USING 2 17 'Del con'.
  PERFORM fill_cell USING 2 18 'Del info'.
  PERFORM fill_cell USING 2 19 'Only Gen'.
  PERFORM fill_cell USING 2 20 'FRA1'.
  PERFORM fill_cell USING 2 21 'ZOTH'.
  PERFORM fill_cell USING 2 22 'ZOTI'.
  PERFORM fill_cell USING 2 23 'ZP01'.
  PERFORM fill_cell USING 2 24 'ZP02'.
  PERFORM fill_cell USING 2 25 'ZP03'.
  PERFORM fill_cell USING 2 26 'ZP04'.
  PERFORM fill_cell USING 2 27 'ZP05'.
  PERFORM fill_cell USING 2 28 'ZP06'.
  PERFORM fill_cell USING 2 29 'ZP07'.
  PERFORM fill_cell USING 2 30 'ZP08'.
  PERFORM fill_cell USING 2 31 'ZP09'.
  PERFORM fill_cell USING 2 32 'ZP10'.
  PERFORM fill_cell USING 2 33 'ZP11'.
  PERFORM fill_cell USING 2 34 'ZP12'.
  PERFORM fill_cell USING 2 35 'ZP03'.
  PERFORM fill_cell USING 2 36 'ZP14'.
  PERFORM fill_cell USING 2 37 'ZP15'.
  PERFORM fill_cell USING 2 38 'ZP16'.
  PERFORM fill_cell USING 2 39 'Approval'.
  PERFORM fill_cell USING 2 40 'Reason'.
  PERFORM fill_cell USING 2 41 'Creater'.
  PERFORM fill_cell USING 2 42 'Creation date'.
  PERFORM fill_cell USING 2 43 'Pack Amorti'.
  PERFORM fill_cell USING 2 44 'Tooling Amorti'.
  PERFORM fill_cell USING 2 45 'Develop Amort'.

* Filled data
  CLEAR w_int.
  DESCRIBE TABLE it_out LINES w_int.

  row = 2.
  LOOP AT it_out.
    row = row + 1.
    PERFORM progress_indicator USING row w_int.

    PERFORM fill_cell USING row 1  it_out-lifnr.
    PERFORM fill_cell USING row 2  it_out-matnr.
    PERFORM fill_cell USING row 3  it_out-ekorg.
    PERFORM fill_cell USING row 4  it_out-urzzt.
    PERFORM fill_cell USING row 5  it_out-aplfz.
    PERFORM fill_cell USING row 6  it_out-ekgrp.
    PERFORM fill_cell USING row 7  it_out-norbm.
    PERFORM fill_cell USING row 8  it_out-uebtk.
    PERFORM fill_cell USING row 9  it_out-bstae.
    PERFORM fill_cell USING row 10 it_out-webre.
    PERFORM fill_cell USING row 11 it_out-mwskz.
    PERFORM fill_cell USING row 12 it_out-netpr.
    PERFORM fill_cell USING row 13 it_out-peinh.
    PERFORM fill_cell USING row 14 it_out-waers.
    PERFORM fill_cell USING row 15 it_out-datab.
    PERFORM fill_cell USING row 16 it_out-datbi.
    PERFORM fill_cell USING row 17 it_out-del_con.
    PERFORM fill_cell USING row 18 it_out-del_inf.
    PERFORM fill_cell USING row 19 it_out-only_gen.
    PERFORM fill_cell USING row 20 it_out-fra1.
    PERFORM fill_cell USING row 21 it_out-zoth.
    PERFORM fill_cell USING row 22 it_out-zoti.
    PERFORM fill_cell USING row 23 it_out-zp01.
    PERFORM fill_cell USING row 24 it_out-zp02.
    PERFORM fill_cell USING row 25 it_out-zp03.
    PERFORM fill_cell USING row 26 it_out-zp04.
    PERFORM fill_cell USING row 27 it_out-zp05.
    PERFORM fill_cell USING row 28 it_out-zp06.
    PERFORM fill_cell USING row 29 it_out-zp07.
    PERFORM fill_cell USING row 30 it_out-zp08.
    PERFORM fill_cell USING row 31 it_out-zp09.
    PERFORM fill_cell USING row 32 it_out-zp10.
    PERFORM fill_cell USING row 33 it_out-zp11.
    PERFORM fill_cell USING row 34 it_out-zp12.
    PERFORM fill_cell USING row 35 it_out-zp13.
    PERFORM fill_cell USING row 36 it_out-zp14.
    PERFORM fill_cell USING row 37 it_out-zp15.
    PERFORM fill_cell USING row 38 it_out-zp16.
    PERFORM fill_cell USING row 39 it_out-kosrt.
    PERFORM fill_cell USING row 40 it_out-kzust.
    PERFORM fill_cell USING row 41 it_out-ernam.
    PERFORM fill_cell USING row 42 it_out-erdat.
    PERFORM fill_cell USING row 43 it_out-qty1.
    PERFORM fill_cell USING row 44 it_out-qty2.
    PERFORM fill_cell USING row 45 it_out-qty3.
  ENDLOOP.

* Make the excel sheet visible on the desktop.
  IF p_v EQ 'X'.
    SET PROPERTY OF excel 'VISIBLE' = 1.
  ELSEIF p_i EQ 'X'.
    SET PROPERTY OF excel 'VISIBLE' = 0.
  ENDIF.
  CALL METHOD OF excel 'QUIT'.

  FREE OBJECT cell.
  FREE OBJECT books.
  FREE OBJECT excel.

ENDFORM.                    " EXCEL_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  TEXT_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM text_download.

  CALL FUNCTION 'DOWNLOAD'
   EXPORTING
    filetype                      = 'DAT'
    mode                          = '0'
    filemask_mask                 = ',*.txt,*.txt.'
* IMPORTING
    TABLES
      data_tab                      = it_out
   EXCEPTIONS
     invalid_filesize              = 1
     invalid_table_width           = 2
     invalid_type                  = 3
     no_batch                      = 4
     unknown_error                 = 5
     gui_refuse_filetransfer       = 6
     customer_error                = 7
     OTHERS                        = 8
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSEIF sy-subrc = 0.
    MESSAGE s001 WITH 'Completed download'.
  ENDIF.

ENDFORM.                    " TEXT_DOWNLOAD
