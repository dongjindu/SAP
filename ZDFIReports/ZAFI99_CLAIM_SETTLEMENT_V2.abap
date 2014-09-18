************************************************************************
* Program Name      : ZAFI99_CLAIM_SETTLEMENT
* Author            : Furong Wang
* Creation Date     : 12/06/2005
* Specifications By : Andy Choi
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 06/12/06   Manju        UD1K921052   Report changes
* 04/02/07   Manju        UD1K940245   Program corrections
* 01/20/11   Valerian     UD1K950637   Fix logic so it will recognize
*                                      debit/credit posting
* 08/26/11   Valerian     UD1K952971   Fix issue with material that has
*                                      more than 1 supplier.
* 09/26/11   Yn.kim       UP1K920005   Upgrade Ecc6.+ BDC change.
* 10/11/11   KDM                     BDC change : When input PA segment,
*                                           Add One step(screen 0002) .
*                                      --> Index : KDM01
* 11/11/11   KDM                     Selection data control,
*                                      --> Index : KDM02
* 10/19/11   Valerian     UD1K953244   Get additional G/L account in
*                                      data selections
* 12/06/11   Valerian     UD1K953393   Remove G/L account '0000123201'
*                                      in data selections
* 10/25/12   Valerian     UD1K955730   Implement Damage Claim Receivable
*                                      Supplier to Supplier
************************************************************************
REPORT zafi99_claim_settlement_v2 MESSAGE-ID zmpp.


TABLES: ekko, mbewh, ekbe,
        ztfi_claim_rate.                                    "UD1K921052
TABLES: qmel,jest.        " UD1K951235

TYPE-POOLS slis .
CONSTANTS: c_return(05) TYPE c VALUE '122',        " return to vendor
           c_return_reverse(05) TYPE c VALUE '123',"reverse of return
           c_scrap(05)  TYPE c VALUE '201',        "scrapped
           c_scrap_reverse(05) TYPE c VALUE '202', "reverse of scrapped
           c_gr         TYPE c VALUE '1',          "transaction type: GR
           c_inv        TYPE c VALUE '2',
           c_hkont      LIKE bsis-hkont VALUE '0000123200',
           c_hkont2     LIKE bsis-hkont VALUE '0000123201', "UD1K953244
           c_hkont3     LIKE bsis-hkont VALUE '0000123206', "UD1K955730

* ig.moon 5/4/2010 {
*           c_hkont_2    LIKE bsis-hkont VALUE '0000123205',
* }
           c_ekorg      LIKE ekko-ekorg VALUE 'PU01'.
*-->
DATA: BEGIN OF it_data OCCURS 0,
      bwart LIKE ekbe-bwart,
      lifnr LIKE ekko-lifnr,
      ebeln LIKE ekbe-ebeln,
      ebelp LIKE ekbe-ebelp,
      vgabe LIKE ekbe-vgabe,
      gjahr LIKE ekbe-gjahr,
      belnr LIKE ekbe-belnr,
      buzei LIKE ekbe-buzei,
      budat LIKE ekbe-budat,
      menge LIKE ekpo-menge,
      meins LIKE ekpo-meins,
      webre LIKE ekpo-webre,
      matnr LIKE ekbe-matnr,
      werks LIKE ekbe-werks,
      dmbtr LIKE bsis-dmbtr,
      lfbnr LIKE ekbe-lfbnr,
      lfpos LIKE ekbe-lfpos,
      shkzg LIKE ekbe-shkzg,
      netpr LIKE ekpo-netpr,
      peinh LIKE ekpo-peinh,
      hkont LIKE bsis-hkont,                                "UD1K955730
      symsg LIKE syst-msgv1,                                "UD1K952971
      END OF it_data.

DATA: BEGIN OF it_output OCCURS 0,
      check(1),
      bwart LIKE ekbe-bwart,
      clmtype LIKE mara-ernam,
      lifnr LIKE ekko-lifnr,
      ebeln LIKE ekbe-ebeln,
      ebelp LIKE ekbe-ebelp,
      vgabe LIKE ekbe-vgabe,
      gjahr LIKE ekbe-gjahr,
      belnr LIKE ekbe-belnr,
      buzei LIKE ekbe-buzei,
      budat LIKE ekbe-budat,
      meins LIKE ekpo-meins,
      menge LIKE ekpo-menge,
      matnr LIKE ekbe-matnr,
* by ig.moon 3/28/2011 {
      notif LIKE ekbe-matnr,
* }
      maktx LIKE makt-maktx,
      werks LIKE ekbe-werks,
      dmbtr LIKE bsis-dmbtr,
      lfbnr LIKE ekbe-lfbnr,
      lfpos LIKE ekbe-lfpos,
      kbetr LIKE konp-kbetr,  "rate
      kpein LIKE konp-kpein,  "Price unit
      kmein LIKE konp-kmein,  "EA
      stprs LIKE mbew-stprs,
      peinh LIKE mbew-peinh,  "price unit
      amount LIKE bsis-dmbtr,
      clmrate LIKE ztfi_claim_rate-rate,
      claim_amount LIKE bsis-dmbtr,
      webre LIKE ekpo-webre,  "GR based
      hkont LIKE bsis-hkont,                                "UD1K955730
      symsg LIKE syst-msgv1,                                "UD1K952971
      END OF it_output.

DATA: BEGIN OF it_notif OCCURS 0,
      matnr LIKE ekbe-matnr,
      notif LIKE ekbe-matnr,
      belnr LIKE bseg-belnr,                                "UD1K955730
      rkmng(20),
      END OF it_notif.

DATA: BEGIN OF it_return_inv OCCURS 0,
*      check(1),
*      bwart LIKE ekbe-bwart,
*      clmtype LIKE mara-ernam,
      lifnr LIKE ekko-lifnr,
      ebeln LIKE ekbe-ebeln,
*      EBELP LIKE EKBE-EBELP,
      meins LIKE ekpo-meins,
      lfbnr LIKE ekbe-lfbnr,
      lfpos LIKE ekbe-lfpos,
*      belnr LIKE ekbe-belnr,
*      buzei LIKE ekbe-buzei,
      matnr LIKE ekbe-matnr,
      ebelp LIKE ekbe-ebelp,

      budat LIKE ekbe-budat,
      maktx LIKE makt-maktx,
*      vgabe LIKE ekbe-vgabe,
*      gjahr LIKE ekbe-gjahr,
*      budat LIKE ekbe-budat,
      menge LIKE ekpo-menge,
      werks LIKE ekbe-werks,
      dmbtr LIKE bsis-dmbtr,
*      kbetr LIKE konp-kbetr,
*      stprs LIKE mbew-stprs,
      amount LIKE bsis-dmbtr,
*      clmrate LIKE ztfi_claim_rate-rate,
      claim_amount LIKE bsis-dmbtr,
      END OF it_return_inv.

DATA: BEGIN OF it_scrap_inv OCCURS 0,
      lifnr LIKE ekko-lifnr,
      meins LIKE ekpo-meins,
      matnr LIKE ekbe-matnr,
      maktx LIKE makt-maktx,
      hkont LIKE bsis-hkont,                                "UD1K955730
      menge LIKE ekpo-menge,
      dmbtr LIKE bsis-dmbtr,
*      kbetr LIKE konp-kbetr,
      stprs LIKE mbew-stprs,
      amount LIKE bsis-dmbtr,
*      clmrate LIKE ztfi_claim_rate-rate,
      claim_amount LIKE bsis-dmbtr,
      END OF it_scrap_inv.

DATA: it_scrap_inv2 LIKE it_scrap_inv OCCURS 0              "UD1K955730
                    WITH HEADER LINE.                       "UD1K955730

DATA: BEGIN OF lt_invoice OCCURS 0,
      lifnr LIKE ekko-lifnr,
      ebeln LIKE ekbe-ebeln,
      ebelp LIKE ekbe-ebelp,
      lfbnr LIKE ekbe-lfbnr,
      lfpos LIKE ekbe-lfpos,
*      belnr LIKE ekbe-belnr,
*      buzei LIKE ekbe-buzei,
      matnr LIKE ekbe-matnr,
      budat LIKE ekbe-budat,
      meins LIKE ekpo-meins,
      menge LIKE ekpo-menge,
      dmbtr LIKE bsis-dmbtr,
      amount LIKE bsis-dmbtr,
      claim_amount LIKE bsis-dmbtr,
      webre LIKE ekpo-webre,
      END OF lt_invoice.

DATA: it_ztfi_claim_stl LIKE TABLE OF ztfi_claim_stl2       "VALERIAN
       WITH HEADER LINE.

DATA: it_ekbe LIKE ekbe OCCURS 0 WITH HEADER LINE.
DATA: it_bsis LIKE bsis OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_bseg OCCURS 0,
        bukrs  LIKE bseg-bukrs,
        belnr  LIKE bseg-belnr,
        gjahr  LIKE bseg-gjahr,
        buzei  LIKE bseg-buzei,
        matnr  LIKE bseg-matnr,
        menge  LIKE bseg-menge,
        meins  LIKE bseg-meins,
        lifnr  LIKE bseg-lifnr,
        werks  LIKE bseg-werks,

      END OF it_bseg.


DATA: no_data TYPE c.
DATA: w_first_date TYPE d,
      w_last_date  TYPE d,
      ok_code      LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt       TYPE   i,
      w_date(10),
      w_matkl LIKE mara-matkl,
      w_waers LIKE t001-waers,
      w_dmbtr LIKE lt_invoice-dmbtr,
      w_claim_amount LIKE lt_invoice-claim_amount,
      w_model_nf LIKE lt_invoice-dmbtr,
      w_model_cm LIKE lt_invoice-dmbtr,
      w_model_yf LIKE lt_invoice-dmbtr. " by ig.moon 01/08/2010

DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.
DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : headerdata LIKE bapi_incinv_create_header,
       invoicedocnumber LIKE bapi_incinv_fld-inv_doc_no,
       itemdata LIKE TABLE OF bapi_incinv_create_item WITH HEADER LINE,
       return LIKE TABLE OF bapiret2 WITH HEADER LINE,
       taxdata LIKE bapi_incinv_create_tax,
       fiscalyear LIKE bapi_incinv_fld-fisc_year.


DATA : it_message LIKE it_mess OCCURS 0 WITH HEADER LINE.

DATA : w_mode LIKE ctu_params-dismode VALUE 'E'. "'E'. "A-display 'N' *
*def 'E'

*&spwizard: declaration of tablecontrol 'TC_100' itself
CONTROLS: tc_100 TYPE TABLEVIEW USING SCREEN 0100.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_fi  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

*ALV refresh?
DATA: stable        TYPE lvc_s_stbl.

* -------------------------------------------------------------
* EVent class
*-----------------------------------------------------------
* local class to handle semantic checks
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: g_event_receiver TYPE REF TO lcl_event_receiver.

*************************************************************
* LOCAL CLASS Definition
**************************************************************
*§4.Define and implement event handler to handle event DATA_CHANGED.
*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
              IMPORTING er_data_changed.

    DATA: error_in_data TYPE c.

ENDCLASS.
DATA :it_lvc  LIKE lvc_s_row.
*************************************************************
* LOCAL CLASS IMPLEMENTATION
**************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_data_changed.

    DATA: ls_good TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          l_lifnr LIKE ekko-lifnr,
          w_lifnr LIKE ekko-lifnr,
          lvc_t_row TYPE lvc_t_row.

    error_in_data = space.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      CASE ls_good-fieldname.
* check if column Name1 of this row was changed
        WHEN 'LIFNR'.
          CALL METHOD er_data_changed->get_cell_value
                     EXPORTING
                        i_row_id  = ls_good-row_id
                        i_fieldname = ls_good-fieldname
                     IMPORTING
                        e_value =   lv_value.
          CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                       i_row_id = ls_good-row_id
                       i_fieldname = ls_good-fieldname
                       i_value     = lv_value.

          w_lifnr = lv_value.
          SELECT SINGLE lifnr INTO l_lifnr FROM lfa1
            WHERE lifnr = w_lifnr.
          IF sy-subrc <> 0.
            CALL METHOD er_data_changed->add_protocol_entry
                        EXPORTING
                              i_msgid =  'SU'
                              i_msgno =  '000'
                              i_msgty =  'E'
         i_msgv1 = 'Please input correct vendor code'
         i_msgv2 = '(Value->)'
         i_msgv3 = lv_value
                             i_fieldname = ls_good-fieldname
                              i_row_id  =  ls_good-row_id.
          ELSE.
            CALL METHOD er_data_changed->modify_cell
                    EXPORTING
                         i_row_id = ls_good-row_id
                         i_fieldname = ls_good-fieldname
                         i_value     = lv_value.
            CALL METHOD er_data_changed->modify_cell
                    EXPORTING
                         i_row_id = ls_good-row_id
                         i_fieldname = 'CHECK'
                        i_value     = 'X'.

          ENDIF.

      ENDCASE.
    ENDLOOP.

*§7.Display application log if an error has occured.
    IF error_in_data EQ 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.

ENDCLASS.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-007.
PARAMETERS:   p_bukrs  TYPE bukrs OBLIGATORY MEMORY ID buk,
          p_gjahr  TYPE gjahr OBLIGATORY MEMORY ID gja,
          p_monat  TYPE monat OBLIGATORY.
SELECT-OPTIONS: s_date  FOR ekbe-budat NO-EXTENSION. "    OBLIGATORY.
SELECT-OPTIONS: s_matnr FOR ekbe-matnr.
SELECT-OPTIONS: s_lifnr FOR ekko-lifnr.
PARAMETERS: p_clgl LIKE bseg-hkont DEFAULT '126001'.
*SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_1 RADIOBUTTON GROUP gp1 ."DEFAULT 'X'.
SELECTION-SCREEN COMMENT 8(8) text-m13.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_2 RADIOBUTTON GROUP gp1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 8(8) text-m14.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

*SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-008.
PARAMETERS: p_pdate LIKE ekbe-budat DEFAULT sy-datum.
*            p_pterm  TYPE dzterm.
PARAMETERS: p_max_l TYPE i DEFAULT 120.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-009.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_3 RADIOBUTTON GROUP gp2 .
SELECTION-SCREEN COMMENT 8(8) text-m15.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_4 RADIOBUTTON GROUP gp2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 8(8) text-m16.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

AT SELECTION-SCREEN.
  PERFORM read_period_date.

START-OF-SELECTION.
  PERFORM initial_varible.
  PERFORM read_data.
  IF it_data[] IS INITIAL.
    MESSAGE i001 WITH text-004.
    EXIT.
  ENDIF.
  PERFORM get_price_data.
  PERFORM process_data.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  IF r_1 = 'X'.
    PERFORM read_return_materials.
  ELSE.
    PERFORM read_scrapped_materials.
  ENDIF.
ENDFORM.                    " read_data

*&---------------------------------------------------------------------*
*&      Form  read_return_materials
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_return_materials.

  IF s_date IS INITIAL.
    SELECT  a~bwart b~lifnr a~ebeln a~ebelp a~vgabe
            a~gjahr a~belnr a~buzei a~budat a~menge
            a~matnr a~werks a~dmbtr a~lfbnr a~lfpos
            c~meins c~webre a~shkzg c~netpr c~peinh
     INTO CORRESPONDING FIELDS OF TABLE it_data
      FROM ekbe AS a
      INNER JOIN ekko AS b
         ON b~ebeln = a~ebeln
      INNER JOIN ekpo AS c
         ON c~ebeln = a~ebeln
        AND c~ebelp = a~ebelp
      WHERE   vgabe  = c_gr         "GR
      AND   gjahr  = p_gjahr      "FISCAL YEAR
      AND   bwart  IN (c_return, c_return_reverse)
      AND   bewtp  = 'E'            "GR
      AND   budat  GE w_first_date  " PERIOD
      AND   budat  LE w_last_date   "period
      AND   b~lifnr IN s_lifnr
      AND   b~bukrs = p_bukrs
      AND   a~matnr IN s_matnr
      AND c~knttp = space
      AND c~loekz = space.
  ELSE.
    SELECT  a~bwart b~lifnr a~ebeln a~ebelp a~vgabe
            a~gjahr a~belnr a~buzei a~budat a~menge
            a~matnr a~werks a~lfbnr a~lfpos
            c~meins c~webre a~shkzg  c~netpr c~peinh
     INTO CORRESPONDING FIELDS OF TABLE it_data
      FROM ekbe AS a
      INNER JOIN ekko AS b
         ON a~ebeln = b~ebeln
      INNER JOIN ekpo AS c
         ON c~ebeln = a~ebeln
        AND c~ebelp = a~ebelp
      WHERE   vgabe  = c_gr         "GR
      AND   gjahr  = p_gjahr      "FISCAL YEAR
      AND   bwart  IN (c_return, c_return_reverse)
      AND   bewtp  = 'E'            "GR
      AND   budat IN s_date
      AND   b~lifnr IN s_lifnr
      AND   b~bukrs = p_bukrs
      AND   a~matnr IN s_matnr
      AND c~knttp = space
      AND c~loekz = space.
  ENDIF.

  DATA: BEGIN OF lt_ekbe OCCURS 0,
          bewtp  TYPE bewtp,
          shkzg  TYPE shkzg,
          menge  TYPE menge_d,
        END OF lt_ekbe.

  DATA: l_index TYPE i.

  LOOP AT it_data.
    l_index = sy-tabix.
    REFRESH lt_ekbe.
    SELECT bewtp shkzg SUM( menge )
      INTO TABLE lt_ekbe
      FROM ekbe
      WHERE ebeln = it_data-ebeln
        AND ebelp = it_data-ebelp
        AND ( bewtp = 'E' OR bewtp = 'Q' )
      GROUP by bewtp shkzg.

    LOOP AT lt_ekbe.
      IF lt_ekbe-bewtp = 'E' AND lt_ekbe-shkzg = 'H'.
        lt_ekbe-menge = - lt_ekbe-menge.
      ELSEIF lt_ekbe-bewtp = 'Q' AND lt_ekbe-shkzg = 'S'.
        lt_ekbe-menge = - lt_ekbe-menge.
      ENDIF.
      MODIFY lt_ekbe INDEX sy-tabix TRANSPORTING menge.
    ENDLOOP.

    LOOP AT lt_ekbe.
      AT LAST.
        SUM.
        IF lt_ekbe-menge = 0.
          DELETE it_data INDEX l_index.
        ENDIF.
      ENDAT.
    ENDLOOP.

  ENDLOOP.
ENDFORM.                    " read_return_materials
*&---------------------------------------------------------------------*
*&      Form  read_scrapped_materials
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_scrapped_materials.
  DATA: l_line TYPE i,
        lt_a018 LIKE TABLE OF a018 WITH HEADER LINE.
* reading the scraped material doc

  SELECT * INTO TABLE it_bsis
    FROM bsis
    WHERE bukrs  = p_bukrs  "company code

* ig.moon 5/4/2010 {
*     AND hkont  = c_hkont  "G/L account
      AND ( hkont  = c_hkont3 )                             "VALERIAN
*     AND ( hkont  = c_hkont OR hkont  = c_hkont3 )         "UD1K955730
*     AND hkont  = c_hkont                                  "UD1K953393
*     AND ( hkont  = c_hkont OR hkont  = c_hkont2 )         "UD1K953393
*     AND hkont  = c_hkont                                  "UD1K953244
*             or hkont  = c_hkont_2 ) "G/L account
* }

      AND gjahr  = p_gjahr  "fiscal year
      AND monat = p_monat   "posting period
      AND blart = 'WC'
      AND   budat  GE w_first_date  " PERIOD
      AND   budat  LE w_last_date.   "period

  IF it_bsis[] IS NOT INITIAL.    ""KDM02(11/11/2011)
    SELECT bukrs belnr gjahr buzei matnr menge meins lifnr werks
    INTO CORRESPONDING FIELDS OF TABLE it_bseg
      FROM bseg
      FOR ALL ENTRIES IN it_bsis
      WHERE bukrs = it_bsis-bukrs
        AND belnr = it_bsis-belnr
        AND gjahr = it_bsis-gjahr
        AND buzei = it_bsis-buzei
*        AND KOART = 'M'
        AND matnr IN s_matnr.
    SORT it_bseg BY bukrs belnr gjahr buzei.
  ENDIF.

  LOOP AT it_bsis.
    READ TABLE it_bseg WITH KEY bukrs = it_bsis-bukrs
                                belnr = it_bsis-belnr
                                gjahr = it_bsis-gjahr
*                                MATNR = IT_BSIS-XREF3.
                                buzei = it_bsis-buzei
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
*      lifnr LIKE ekko-lifnr,
*      ebeln LIKE ekbe-ebeln,
*      ebelp LIKE ekbe-ebelp,
*      vgabe LIKE ekbe-vgabe,
      it_data-hkont = it_bsis-hkont.
      it_data-gjahr = it_bsis-gjahr.
      it_data-belnr = it_bsis-belnr.
      it_data-buzei = it_bsis-buzei.
      it_data-budat = it_bsis-budat.
      IF it_bsis-shkzg = 'H'.
        it_data-dmbtr = it_bsis-dmbtr * -1.
        it_data-menge = it_bseg-menge * -1.
      ELSE.
        it_data-dmbtr = it_bsis-dmbtr.
        it_data-menge = it_bseg-menge.
      ENDIF.
      it_data-meins = it_bseg-meins.
      it_data-matnr = it_bseg-matnr.
      it_data-werks = it_bseg-werks.

*      lfbnr LIKE ekbe-lfbnr.
*      lfpos LIKE ekbe-lfpos.
      IF it_data-matnr ='R16N'.
        SELECT SINGLE lifnr INTO it_data-lifnr
        FROM eord
        WHERE matnr = 'R18N'
*        AND werks = it_data-werks
          AND vdatu <= it_data-budat
          AND bdatu >= it_data-budat
          AND lifnr IN s_lifnr.
        IF it_data-lifnr IS INITIAL.
          SELECT * INTO TABLE lt_a018
                       FROM a018
                      WHERE kappl EQ 'M'
                        AND kschl EQ 'PB00'
                        AND matnr EQ 'R18N'
                        AND ekorg = 'PU01'
                        AND datab <= it_data-budat
                        AND datbi >= it_data-budat.
          DESCRIBE TABLE lt_a018 LINES l_line.
          IF l_line = 1.
            it_data-lifnr = lt_a018-lifnr.
          ENDIF.
        ENDIF.
      ELSE.
*       SELECT SINGLE lifnr INTO it_data-lifnr              "UD1K952971
        SELECT DISTINCT lifnr INTO it_data-lifnr            "UD1K952971
        FROM eord
        WHERE matnr = it_data-matnr
*        AND werks = it_data-werks
          AND vdatu <= it_data-budat
          AND bdatu >= it_data-budat.
*         AND lifnr IN s_lifnr.                             "UD1K952971
        ENDSELECT.                                          "UD1K952971
* BEGIN OF UD1K952971
        IF sy-dbcnt > 1.
          it_data-symsg = 'More than one supplier has been found'.
        ENDIF.
* END OF UD1K952971
        IF it_data-lifnr IS INITIAL.
          SELECT * INTO TABLE lt_a018
                       FROM a018
                      WHERE kappl EQ 'M'
                        AND kschl EQ 'PB00'
                        AND matnr EQ it_data-matnr
                        AND ekorg = 'PU01'
                        AND datab <= it_data-budat
                        AND datbi >= it_data-budat.
          DESCRIBE TABLE lt_a018 LINES l_line.
          IF l_line = 1.
            it_data-lifnr = lt_a018-lifnr.
          ENDIF.
        ENDIF.
      ENDIF.

      CHECK it_data-lifnr IN s_lifnr.      ""KDM02(11/11/2011)

      APPEND it_data.
      CLEAR: it_data.
      CLEAR: lt_a018,lt_a018[].
    ENDIF.
  ENDLOOP.
ENDFORM.                    " read_scrapped_materials
*&---------------------------------------------------------------------*
*&      Form  initial_varible
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial_varible.
  CLEAR: it_ekbe, it_ekbe[],
         it_bsis, it_bsis[],
         no_data.


ENDFORM.                    " initial_varible
*&---------------------------------------------------------------------*
*&      Form  read_period_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_period_date.
  DATA: l_month(02) TYPE n,
        l_date TYPE d.

  l_month = p_monat.
  CONCATENATE p_gjahr l_month '01' INTO w_first_date.
  l_date = w_first_date.


  CALL FUNCTION 'SG_PS_GET_LAST_DAY_OF_MONTH'
       EXPORTING
            day_in            = w_first_date
       IMPORTING
            last_day_of_month = w_last_date.
*   EXCEPTIONS
*     DAY_IN_NOT_VALID        = 1
*     OTHERS                  = 2
  .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " read_period_date
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: BEGIN OF lt_temp OCCURS 0,
     clmtype LIKE mara-ernam,
     lifnr LIKE ekko-lifnr,
     ebeln LIKE ekbe-ebeln,
     ebelp LIKE ekbe-ebelp,
     matnr LIKE ekbe-matnr,
     menge LIKE ekpo-menge,
     END OF lt_temp.
  DATA:wa_output LIKE it_output.
  DATA: l_index LIKE sy-tabix,
        l_nindex LIKE sy-tabix,
        l_total LIKE it_output-menge.

  DATA: l_low_d TYPE datum,
        l_high_d TYPE datum.

* BEGIN OF UD1K952971
  DATA: BEGIN OF it_supp OCCURS 0,
          lifnr type eord-lifnr,
        END OF it_supp.
* END OF UD1K952971

  CONCATENATE p_gjahr p_monat '01' INTO l_low_d.

  CALL FUNCTION 'SG_PS_ADD_MONTH_TO_DATE'
       EXPORTING
            months  = '1'
            olddate = l_low_d
       IMPORTING
            newdate = l_high_d.

  l_high_d = l_high_d - 1.
  data $flag.

  clear it_notif.
  refresh it_notif.

  SORT it_output BY clmtype lifnr ebeln ebelp matnr.
  LOOP AT it_output.
    at new matnr.
      $flag = 'X'.
    endat.
    if $flag eq 'X'.
* BEGIN OF UD1K955730
  DATA: l_rkmng type qmel-rkmng.

      SELECT a~rkmng a~objnr INTO (l_rkmng, qmel-objnr)
        FROM qmel AS a JOIN qmfe AS b
                         on a~qmnum = b~qmnum
                     WHERE a~matnr EQ it_output-matnr
                     AND ( a~qmdab BETWEEN l_low_d AND l_high_d )
                     AND b~fecod = '0015'.                  "UD1K955730

*      SELECT * FROM qmel WHERE matnr EQ it_output-matnr
*                     AND ( qmdab BETWEEN l_low_d AND l_high_d ).
* END OF UD1K955730
        SELECT * FROM jest WHERE objnr EQ qmel-objnr
                            AND  stat = 'E0004'.
*                            AND  chgnr EQ qmel-rkmng.

          it_notif-matnr = it_output-matnr.
          it_notif-notif = jest-objnr.
          it_notif-rkmng = l_rkmng.                         "UD1K955730
*         it_notif-rkmng = qmel-rkmng.                      "UD1K955730
          collect it_notif.

        endselect.
      endselect.
      clear $flag.
    endif.

  ENDLOOP.

* BEGIN OF UD1K955730
  data: l_tline like tline occurs 0 with header line,
        $awkey  like bkpf-awkey,
        l_tabix like sy-tabix,
        l_qnum  type qmnum.

  loop at it_notif.
    l_tabix = sy-tabix.
    l_qnum  = it_notif-notif+2.

    clear: l_tline, l_tline[], $awkey.

    CALL FUNCTION 'Z_GET_MBLNR_FROM_QMNUM'
       EXPORTING
         QMNUM = l_qnum
       TABLES
         TLINE = l_tline.
     if sy-subrc eq 0.
       read table l_tline index 2.
       if sy-subrc eq 0.
         $AWKEY = l_tline+2.
         condense $awkey no-gaps  .
         select single BELNR into it_notif-belnr from bkpf
         where awkey eq $awkey.
       endif.
     endif.

     modify it_notif index l_tabix transporting belnr.
  endloop.
* END OF UD1K955730

  sort it_notif by matnr notif rkmng.
  data $index type i.

  LOOP AT it_output.
    l_index = sy-tabix.
    l_nindex = l_index + 1.
    READ TABLE it_output INTO wa_output INDEX l_nindex.
    IF it_output-clmtype = wa_output-clmtype AND
       it_output-lifnr = wa_output-lifnr AND
       it_output-ebeln = wa_output-ebeln AND
       it_output-ebelp = wa_output-ebelp AND
       it_output-matnr = wa_output-matnr.
      l_total = it_output-menge + wa_output-menge.
      IF l_total = 0.
        DELETE it_output FROM l_index TO l_nindex.
        CONTINUE.
      ENDIF.
    ENDIF.

    read table it_notif with key matnr = it_output-matnr
                                 belnr = it_output-belnr.   "UD1K955730
*                                rkmng = it_output-menge.   "UD1K955730
    if sy-subrc eq 0.
      $index = sy-tabix.
      it_output-notif = it_notif-notif+4.
* BEGIN OF UD1K952971 - Correct Supplier if more than 1
      SELECT DISTINCT lifnr INTO TABLE it_supp
        FROM eord
        WHERE matnr = it_output-matnr
          AND vdatu <= it_output-budat
          AND bdatu >= it_output-budat.
      IF sy-dbcnt > 1 OR it_output-hkont = c_hkont3.        "UD1K955730
*     IF sy-dbcnt > 1.                                      "UD1K955730
        SELECT parnr into it_output-lifnr
          FROM ihpa
        UP TO 1 ROWS
        WHERE objnr = it_notif-notif
          AND PARVW = 'Z5'.
          CLEAR it_output-symsg.
        ENDSELECT.
      ENDIF.
* END OF UD1K952971
      MODIFY it_output INDEX l_index TRANSPORTING notif
                                     lifnr symsg            "UD1K952971
                                     hkont.                 "UD1K955730

      clear  it_output-notif.
      delete it_notif index $index.
    endif.
  ENDLOOP.

  CALL SCREEN 200.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  read_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_price_data.
  DATA: l_knumh LIKE a018-knumh.
  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_output.
** Added by Furong on 11/15/07
    IF r_1 = 'X' AND it_data-shkzg = 'S'.
      it_data-menge = it_data-menge * -1.
      it_output-menge = it_output-menge * -1.
      it_output-dmbtr = it_output-dmbtr * -1.
    ENDIF.
** end of addition
** for scrap only
    IF r_2 = 'X'.
      IF it_data-matnr ='R16N'.
        SELECT SINGLE knumh INTO l_knumh
        FROM a018
        WHERE kschl = 'PB00'
          AND lifnr = it_data-lifnr
          AND matnr = 'R18N'
          AND ekorg = c_ekorg
          AND datab <= it_data-budat
          AND datbi >= it_data-budat.
      ELSE.
        SELECT SINGLE knumh INTO l_knumh
           FROM a018
           WHERE kschl = 'PB00'
             AND lifnr = it_data-lifnr
             AND matnr = it_data-matnr
             AND ekorg = c_ekorg
             AND datab <= it_data-budat
             AND datbi >= it_data-budat.
      ENDIF.
      IF sy-subrc EQ 0.
        SELECT SINGLE kbetr kpein kmein
        INTO (it_output-kbetr, it_output-kpein, it_output-kmein )
          FROM konp
          WHERE knumh = l_knumh
           AND kschl = 'PB00'
           AND loevm_ko = space.
      ENDIF.

*-- standard price
      SELECT SINGLE stprs peinh INTO (it_output-stprs, it_output-peinh)
        FROM ckmlcr INNER JOIN ckmlhd
                       ON ckmlcr~kalnr  = ckmlhd~kalnr
        WHERE ckmlhd~matnr  = it_data-matnr
          AND ckmlhd~bwkey  = it_data-werks
          AND ckmlcr~bdatj  = p_gjahr
          AND ckmlcr~poper  = p_monat
          AND ckmlcr~untper = space
          AND ckmlcr~curtp  = '10'.

*    SELECT SINGLE stprs peinh INTO (it_output-stprs, it_output-peinh)
*       FROM mbew
*       WHERE matnr = it_data-matnr
*         AND bwkey = it_data-werks
*         AND lvorm = space
*         AND lfgja = p_gjahr
*         AND lfmon <= p_monat.
*
*    IF sy-subrc <> 0.
*      CLEAR mbewh.
*      SELECT *
*       FROM mbewh
*       WHERE matnr = it_data-matnr
*         AND bwkey = it_data-werks
*         AND lfgja  = p_gjahr
*         AND lfmon <= p_monat
*       ORDER BY lfgja DESCENDING
*                lfmon DESCENDING.
*        EXIT.
*      ENDSELECT.
*      IF sy-subrc <> 0.
*        SELECT *
*           FROM mbewh
*           WHERE matnr = it_data-matnr
*             AND bwkey = it_data-werks
*             AND lfgja < p_gjahr
*           ORDER BY lfgja DESCENDING
*                    lfmon DESCENDING.
*          EXIT.
*        ENDSELECT.
*      ENDIF.
*      it_output-stprs = mbewh-stprs.

*      SELECT SINGLE stprs INTO it_output-stprs
*      FROM mbewh
*      WHERE matnr = it_data-matnr
*        AND bwkey = it_data-werks
*        AND lfgja = p_gjahr
*        AND lfmon = p_monat.
*   ENDIF.
* BEGIN OF UD1K955730
* Ignore conversion if condition type UoM is "ROL"
      IF it_output-kmein = 'ROL'.
        CLEAR it_output-kmein.
      ENDIF.
* END OF UD1K955730
      DATA: l_outqty LIKE it_data-menge.
      IF it_output-kmein IS INITIAL.

      ELSE.
        PERFORM unit_converion(zaco19u_shop_new)
                            USING    it_data-menge
                                     it_data-meins
                                     it_output-kmein
                            CHANGING l_outqty.

        it_output-amount = l_outqty *
                           it_output-kbetr / it_output-kpein.
      ENDIF.
    ELSE.
      it_output-kbetr = it_data-netpr.
      it_output-kpein = it_data-peinh.
      it_output-kmein = it_data-meins.
      it_output-amount = it_data-menge *
                         it_output-kbetr / it_output-kpein.


    ENDIF.

* Begin of changes - UD1K921052
*    SELECT SINGLE MAX( rate ) INTO it_output-clmrate
*      FROM ztfi_claim_rate
*       WHERE bukrs = p_bukrs
*        AND lifnr = it_data-lifnr
*        AND date_from <= it_data-budat.
    SELECT   *
      FROM ztfi_claim_rate
       WHERE bukrs = p_bukrs
        AND lifnr = it_data-lifnr
        AND date_from <= it_data-budat
        ORDER BY date_from  DESCENDING.
      it_output-clmrate =  ztfi_claim_rate-rate.
      EXIT.
    ENDSELECT.
* End of changes - UD1K921052
    it_output-claim_amount = it_output-amount *
                            ( 1 + it_output-clmrate / 100 ).
    SELECT SINGLE maktx INTO it_output-maktx
    FROM makt
    WHERE matnr = it_data-matnr.
    IF it_output-bwart = c_return OR
       it_output-bwart = c_return_reverse.
      it_output-clmtype = 'Return'.
    ELSE.
      it_output-clmtype = 'Scrap'.
    ENDIF.
    APPEND it_output.
    CLEAR it_output.
  ENDLOOP.

ENDFORM.                    " read_info_record

***INCLUDE ZAFI99_CLAIM_SETTLEMENT_FM .
*&spwizard: output module for tc 'TC_100'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
MODULE tc_100_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_output LINES tc_100-lines.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: save_okcode LIKE sy-ucomm.
  save_okcode = ok_code.
  CLEAR: ok_code.
  CASE save_okcode.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'POST'.
      PERFORM post_invocie.
    WHEN 'SELALL'.
      PERFORM sel_all.
    WHEN 'DESELECT'.
      PERFORM desel_all.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  post_invocie
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_invocie.
  LOOP AT it_output WHERE check  = 'X'.
  ENDLOOP.
ENDFORM.                    " post_invocie
*&---------------------------------------------------------------------*
*&      Module  set_status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_status OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR 'T_100'.
ENDMODULE.                 " set_status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  SEL_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sel_all.
  LOOP AT it_output.
    it_output-check = 'X'.
    MODIFY it_output TRANSPORTING check.
  ENDLOOP.
ENDFORM.                    " SEL_ALL
*&---------------------------------------------------------------------*
*&      Form  DESEL_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM desel_all.
  LOOP AT it_output.
    CLEAR: it_output-check.
    MODIFY it_output TRANSPORTING check.
  ENDLOOP.
ENDFORM.

*
**&spwizard: declaration of tablecontrol 'TC_100' itself
*CONTROLS: tc_100 TYPE TABLEVIEW USING SCREEN 0100.
*
** Includes inserted by Screen Painter Wizard. DO NOT CHANGE THIS LINE!
*INCLUDE zafi99_claim_settlement_fm .
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_200 OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object_9000.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_OUTPUT'.
    PERFORM assign_itab_to_alv_9000.
*    PERFORM sssign_event_9000.
  ELSE.
    stable-row = 'X'.
    stable-col = 'X'.
    CALL METHOD alv_grid->refresh_table_display
         EXPORTING is_stable = stable.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object_9000.
*- Create Container('GRID_CONTAINER') with Custom Control on screen
  CREATE OBJECT grid_container
         EXPORTING container_name = wa_custom_control
         EXCEPTIONS
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
          lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT alv_grid
         EXPORTING i_parent = grid_container
                   i_appl_events = 'X'.

ENDFORM.                    " create_container_n_object_9000
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid.

  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1070   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.

  DATA: lw_itab TYPE slis_tabname,
        lw_waers LIKE t001-waers.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.
  SELECT SINGLE waers INTO lw_waers FROM t001
     WHERE bukrs = p_bukrs.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
            I_BYPASSING_BUFFER = 'X'
       CHANGING
            ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                  'S' 'CLMTYPE'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Type',
                                  'E' 'OUTPUTLEN'   '5'.
  IF r_2 = 'X'.
    PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                    'S' 'LIFNR'       ' ',
                                    ' ' 'KEY'         'X',
                                    ' ' 'EDIT'        'X',
                                    ' ' 'COLTEXT'     'Vendor',
                                    'E' 'OUTPUTLEN'   '10'.
  ELSE.
    PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                     'S' 'LIFNR'       ' ',
                                     ' ' 'KEY'         'X',
                                     ' ' 'COLTEXT'     'Vendor',
                                     'E' 'OUTPUTLEN'   '10'.
  ENDIF.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                   'S' 'EBELN'       ' ',
                                   ' ' 'KEY'         'X',
                                   ' ' 'COLTEXT'     'PO Number',
                                   'E' 'OUTPUTLEN'   '10',

                                   'S' 'EBELP'       ' ',
                                   ' ' 'KEY'         ' ',
                                   ' ' 'COLTEXT'     'I/No',
                                   'E' 'OUTPUTLEN'   '4',

*                                   'S' 'WEBRE'       ' ',
*                                   ' ' 'COLTEXT'     'GR based IV',
*                                   'E' 'OUTPUTLEN'   '01',

                                   'S' 'MATNR'       ' ',
                                   ' ' 'KEY'         'X',
                                   ' ' 'COLTEXT'     'Material',
                                   'E' 'OUTPUTLEN'   '18',

                                   'S' 'NOTIF'       ' ',
*                                   ' ' 'KEY'         'X',
                                   ' ' 'COLTEXT'     'Notification',
                                   'E' 'OUTPUTLEN'   '18',

                                   'S' 'MAKTX'       ' ',
                                   ' ' 'KEY'         'X',
                                   ' ' 'COLTEXT'     'Text',
                                   'E' 'OUTPUTLEN'   '30',

                                   'S' 'MENGE'       ' ',
*                                  ' ' 'KEY'         ' ',
                                   ' ' 'COLTEXT'     'Quantity',
                                   'E' 'OUTPUTLEN'   '13',

                                   'S' 'MEINS'       ' ',
*                                  ' ' 'KEY'         ' ',
                                   ' ' 'COLTEXT'     'UoM',
                                   'E' 'OUTPUTLEN'   '05',

                                   'S' 'DMBTR'       ' ',
                                   ' ' 'DO_SUM'      'X',
                                   ' ' 'COLTEXT'     'Trn Amount',
                                   'E' 'CURRENCY'    lw_waers,

                                   'S' 'KBETR'       ' ',
                                   ' ' 'COLTEXT'     'Info Price',
                                   'E' 'CURRENCY'
                                   lw_waers,

                                   'S' 'KPEIN'       ' ',
                                   ' ' 'COLTEXT'     'Per',
                                   'E' 'OUTPUTLEN'   '04',

                                   'S' 'KMEIN'       ' ',
                                   ' ' 'COLTEXT'     'PUoM',
                                   'E' 'OUTPUTLEN'   '05',

*                                  'S' 'MAKTX'       ' ',
*                                  ' ' 'KEY'         'X',
*                                  'E' 'COLTEXT'     'Description',


                                   'S' 'STPRS'       ' ',
                                   ' ' 'COLTEXT'     'Std Price',
                                   'E' 'CURRENCY'    lw_waers,
                                   'S' 'PEINH'       ' ',
                                   ' ' 'COLTEXT'     'CstLot',
                                   'E' 'OUTPUTLEN'   '04',


                                   'S' 'AMOUNT'       ' ',
                                   ' ' 'DO_SUM'      'X',
                                   ' ' 'COLTEXT'     'Amount(Info)',
                                   'E' 'CURRENCY'    lw_waers,

                                   'S' 'CLMRATE'       ' ',
                                   ' ' 'COLTEXT'     'Rate',
                                   'E' 'CURRENCY'    lw_waers.
  IF r_2 = 'X'.
    PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                     'S' 'CLAIM_AMOUNT'       ' ',
                                      ' ' 'EDIT'        'X',
                                     ' ' 'DO_SUM'      'X',
                                     ' ' 'COLTEXT'     'Claim Amount',
                                      '' 'DECIMALS'     '2',
                                      'E' 'CURRENCY'    lw_waers.

  ELSE.
    PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                     'S' 'CLAIM_AMOUNT'       ' ',
                                     ' ' 'DO_SUM'      'X',
                                     ' ' 'COLTEXT'     'Claim Amount',
                                      '' 'DECIMALS'     '2',
                                     'E' 'CURRENCY'    lw_waers.

  ENDIF.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                 'S' 'BELNR'       ' ',
                                 ' ' 'COLTEXT'     'Org.Document',
                                 'E' 'OUTPUTLEN'   '10'.

* BEGIN OF UD1K955730
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                   'S' 'HKONT'       ' ',
                                   ' ' 'COLTEXT'     'G/L Acct',
                                   'E' 'OUTPUTLEN'   '10'.
* END OF UD1K955730

* BEGIN OF UD1K952971
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                   'S' 'SYMSG'       ' ',
                                   ' ' 'COLTEXT'     'Message',
                                   'E' 'OUTPUTLEN'   '50'.
* END OF UD1K952971

ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv_9000.

  CALL METHOD alv_grid->set_table_for_first_display

*     i_structure_name = 'ZSCO_COGS_NEW'
   EXPORTING   is_layout        = wa_is_layout
               i_save           = wa_save
               is_variant       = wa_variant
               i_default        = space
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_output[]
               it_sort          = it_sort[].

** ENTER
  CALL METHOD alv_grid->register_edit_event
                EXPORTING
                   i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* Cursor----
  CALL METHOD alv_grid->register_edit_event
                EXPORTING
                   i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CREATE OBJECT g_event_receiver.
  SET HANDLER g_event_receiver->handle_data_changed FOR alv_grid.




ENDFORM.                    " assign_itab_to_alv_9000
*&---------------------------------------------------------------------*
*&      Form  sssign_event_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sssign_event_9000.
  IF sy-batch IS INITIAL.
    CALL METHOD alv_grid->register_edit_event
        EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  ENDIF.

ENDFORM.                    " sssign_event_9000
*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check filed catalog'.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'POST'.
      PERFORM post_invoice.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

*---------------------------------------------------------------------*
*       FORM display_progress_bar                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_TEXT                                                        *
*---------------------------------------------------------------------*
FORM display_progress_bar USING    p_text.
  DATA: lw_text(50).

  MOVE: p_text TO lw_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = lw_text.

ENDFORM.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  post_invoice
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_invoice.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  PERFORM get_posting_data TABLES lt_rows.

  DESCRIBE TABLE it_return_inv LINES l_line.
  IF l_line EQ 0.
    DESCRIBE TABLE it_scrap_inv LINES l_line.
    IF l_line EQ 0.
*      MESSAGE s000 WITH text-m01.
*      EXIT.
    ENDIF.
  ENDIF.

  PERFORM bapi_bdc_invoice.

ENDFORM.                    " post_invoice

*---------------------------------------------------------------------*
*       FORM get_posting_data                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PT_ROWS                                                       *
*---------------------------------------------------------------------*
FORM get_posting_data TABLES pt_rows STRUCTURE lvc_s_row.
  DATA: l_year(4),
  l_budat LIKE sy-datum,
        l_stblg LIKE rbkp-stblg.

  CLEAR: it_return_inv, it_return_inv[],
         it_scrap_inv, it_scrap_inv[].

  LOOP AT pt_rows WHERE index NE 0.
    READ TABLE it_output INDEX pt_rows-index.
    IF sy-subrc NE 0.
      MESSAGE e000 WITH text-m01.
    ENDIF.

    IF it_output-clmtype = 'Return'.
      SELECT SINGLE inv_no budat                            "VALERIAN
         INTO (it_return_inv-lifnr, l_budat) FROM ztfi_claim_stl2
         WHERE gjahr = p_gjahr
          AND lfmon = p_monat
          AND lifnr = it_output-lifnr
          AND matnr = it_output-matnr
          AND ebeln = it_output-ebeln
          AND ebelp = it_output-ebelp
          AND lfbnr = it_output-lfbnr
          AND lfpos = it_output-lfpos.
      IF sy-subrc = 0.
        l_year = l_budat+0(4).
        SELECT SINGLE stblg INTO l_stblg
          FROM rbkp
         WHERE belnr = it_return_inv-lifnr
          AND gjahr = l_year.
        IF l_stblg IS INITIAL.
          w_repid = sy-repid.
          CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                    titel = w_repid
                    txt1  = 'This return has been posted:'
                    txt2  = it_output-matnr
                    txt3  = 'Invoice Number is '
                    txt4  = it_return_inv-lifnr.
          EXIT.
        ELSE.
          MOVE-CORRESPONDING it_output TO it_return_inv.
          COLLECT it_return_inv.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING it_output TO it_return_inv.
        COLLECT it_return_inv.
      ENDIF.
      CLEAR: it_return_inv.
    ELSE.                                                   "VALERIAN
      SELECT SINGLE inv_no INTO it_scrap_inv-lifnr FROM ztfi_claim_stl2
          WHERE gjahr = p_gjahr
           AND lfmon = p_monat
           AND lifnr = it_output-lifnr
           AND matnr = it_output-matnr.
      IF sy-subrc = 0.
        w_repid = sy-repid.
        CALL FUNCTION 'POPUP_TO_INFORM'
             EXPORTING
                  titel = w_repid
                  txt1  = 'This scrap has been posted'
                  txt2  = it_output-matnr
                  txt3  = 'Invoice Number is '
                  txt4  = it_scrap_inv-lifnr.
        EXIT.
      ELSE.
        MOVE-CORRESPONDING it_output TO it_scrap_inv.
        COLLECT it_scrap_inv.
      ENDIF.
      CLEAR: it_scrap_inv.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  process_invoice_post
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_bdc_invoice.
  DATA: wa_invoice LIKE it_return_inv,
        wa_scrap LIKE it_scrap_inv,
        l_line TYPE i.
  CLEAR: w_dmbtr, w_claim_amount.

  CLEAR: it_ztfi_claim_stl, it_ztfi_claim_stl[].
  WRITE: p_pdate TO w_date.

  SELECT SINGLE waers INTO w_waers FROM t001
    WHERE bukrs = p_bukrs.

  IF r_1 = 'X'.
** Return
*  SORT it_return_inv BY lifnr ebeln ebelp belnr buzei matnr.

    SORT it_return_inv BY lifnr ebeln ebelp lfbnr lfpos matnr.
    LOOP AT it_return_inv INTO wa_invoice.
      AT NEW lifnr.
        lt_invoice-lifnr = wa_invoice-lifnr.
      ENDAT.
      AT NEW ebeln.
        lt_invoice-ebeln = wa_invoice-ebeln.
      ENDAT.
*    AT NEW belnr.
*      lt_invoice-belnr = wa_invoice-belnr.
*    ENDAT.
      AT NEW lfbnr.
        lt_invoice-lfbnr = wa_invoice-lfbnr.
      ENDAT.

      AT END OF ebelp. "MATNR.
        SUM.
        IF wa_invoice-dmbtr = 0.
        ELSE.
          lt_invoice-menge = wa_invoice-menge.
          lt_invoice-meins = wa_invoice-meins.
          lt_invoice-dmbtr = wa_invoice-dmbtr.
          lt_invoice-amount = wa_invoice-amount.
          lt_invoice-claim_amount = wa_invoice-claim_amount.
          lt_invoice-ebelp = wa_invoice-ebelp.
*      lt_invoice-buzei = wa_invoice-buzei.
          lt_invoice-lfpos = wa_invoice-lfpos.
          lt_invoice-matnr = wa_invoice-matnr.
          APPEND lt_invoice.
        ENDIF.

        CLEAR: lt_invoice-menge,lt_invoice-dmbtr,
               lt_invoice-claim_amount,lt_invoice-matnr.
      ENDAT.
      AT END OF lifnr.
        SUM.
        w_dmbtr = wa_invoice-dmbtr.
        w_claim_amount = wa_invoice-claim_amount.
        DESCRIBE TABLE lt_invoice LINES l_line.
        IF l_line > p_max_l.
          PERFORM call_inv_split.
        ELSE.
          PERFORM call_bapi_inv.
        ENDIF.

*       PERFORM call_bapi_inv.
        CLEAR: lt_invoice, lt_invoice[], w_claim_amount, w_dmbtr.
      ENDAT.
      CLEAR: wa_invoice.
    ENDLOOP.

    IF NOT it_ztfi_claim_stl[] IS INITIAL.
      MODIFY ztfi_claim_stl2 FROM TABLE it_ztfi_claim_stl.  "VALERIAN
*      INSERT ZTFI_CLAIM_STL FROM TABLE IT_ZTFI_CLAIM_STL.
**                 ACCEPTING DUPLICATE KEYS.
      IF sy-subrc <> 0.
        MESSAGE i001 WITH 'Updating Z-table error'.
        ROLLBACK WORK.
      ELSE.
        COMMIT WORK.
      ENDIF.
    ENDIF.

** Scrap
  ELSE.

* BEGIN OF UD1K955730 - Processing Account "123206"
    LOOP AT it_scrap_inv WHERE hkont = c_hkont3.
      DELETE it_scrap_inv INDEX sy-tabix.
      APPEND it_scrap_inv TO it_scrap_inv2.
    ENDLOOP.

    SORT it_scrap_inv2 BY lifnr matnr.
    LOOP AT it_scrap_inv2 INTO wa_scrap.
      AT NEW lifnr.
        lt_invoice-lifnr = wa_scrap-lifnr.
      ENDAT.
      AT END OF matnr.
        SUM.
        lt_invoice-menge = wa_scrap-menge.
        lt_invoice-meins = wa_scrap-meins.
        lt_invoice-dmbtr = wa_scrap-dmbtr.
        lt_invoice-amount = wa_scrap-amount.
        lt_invoice-claim_amount = wa_scrap-claim_amount.
        lt_invoice-matnr = wa_scrap-matnr.
        APPEND lt_invoice.
        CLEAR: lt_invoice-menge,lt_invoice-dmbtr,
               lt_invoice-claim_amount,lt_invoice-matnr.
      ENDAT.
      AT END OF lifnr.
        SUM.
        w_dmbtr = wa_scrap-dmbtr.
        w_claim_amount = wa_scrap-claim_amount.
        DESCRIBE TABLE lt_invoice LINES l_line.
        IF l_line > p_max_l.
          PERFORM call_inv_split2.
        ELSE.
          PERFORM call_bdc_inv2.
        ENDIF.
        CLEAR: lt_invoice, lt_invoice[], w_claim_amount, w_dmbtr.
      ENDAT.
      CLEAR: wa_scrap.
    ENDLOOP.
    IF NOT it_ztfi_claim_stl[] IS INITIAL.
      INSERT ztfi_claim_stl2 FROM TABLE it_ztfi_claim_stl.  "VALERIAN
*                 ACCEPTING DUPLICATE KEYS.
*      MODIFY ZTFI_CLAIM_STL FROM TABLE IT_ZTFI_CLAIM_STL.

      IF sy-subrc <> 0.
        MESSAGE i001 WITH 'Updating Z-table error'.
        ROLLBACK WORK.
      ELSE.
        COMMIT WORK.
      ENDIF.

    ENDIF.

    CLEAR: it_ztfi_claim_stl, it_ztfi_claim_stl[].
* END OF UD1K955730

    SORT it_scrap_inv BY lifnr matnr.
    LOOP AT it_scrap_inv INTO wa_scrap.
      AT NEW lifnr.
        lt_invoice-lifnr = wa_scrap-lifnr.
      ENDAT.
      AT END OF matnr.
        SUM.
        lt_invoice-menge = wa_scrap-menge.
        lt_invoice-meins = wa_scrap-meins.
        lt_invoice-dmbtr = wa_scrap-dmbtr.
        lt_invoice-amount = wa_scrap-amount.
        lt_invoice-claim_amount = wa_scrap-claim_amount.
        lt_invoice-matnr = wa_scrap-matnr.
        APPEND lt_invoice.
        CLEAR: lt_invoice-menge,lt_invoice-dmbtr,
               lt_invoice-claim_amount,lt_invoice-matnr.
      ENDAT.
      AT END OF lifnr.
        SUM.
        w_dmbtr = wa_scrap-dmbtr.
        w_claim_amount = wa_scrap-claim_amount.
        DESCRIBE TABLE lt_invoice LINES l_line.
        IF l_line > p_max_l.
          PERFORM call_inv_split.
        ELSE.
          PERFORM call_bdc_inv.
        ENDIF.
        CLEAR: lt_invoice, lt_invoice[], w_claim_amount, w_dmbtr.
      ENDAT.
      CLEAR: wa_scrap.
    ENDLOOP.
    IF NOT it_ztfi_claim_stl[] IS INITIAL.
      INSERT ztfi_claim_stl2 FROM TABLE it_ztfi_claim_stl.  "VALERIAN
*                 ACCEPTING DUPLICATE KEYS.
*      MODIFY ZTFI_CLAIM_STL FROM TABLE IT_ZTFI_CLAIM_STL.

      IF sy-subrc <> 0.
        MESSAGE i001 WITH 'Updating Z-table error'.
        ROLLBACK WORK.
      ELSE.
        COMMIT WORK.
      ENDIF.

    ENDIF.
  ENDIF.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " process_invoice_post
*&---------------------------------------------------------------------*
*&      Form  call_bdc_inv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc_inv.
  DATA: l_tabix TYPE sy-tabix.                              "UD1K950637
  DATA: lw_menge_c(13),
        lw_dmbtr_c(13),
        l_meins LIKE ekpo-meins,
        lw_claim_amount_c(13),
        lw_diff_c(13),
        lw_diff_nf_c(13),
        lw_diff_cm_c(13),
        lw_diff_yf_c(13),
        lw_diff LIKE konp-kbetr,
        lw_todate(10),
        l_text(30),
        l_monat(2),
        l_dmbtr LIKE lt_invoice-dmbtr,
        lw_last_rec(1).
  DATA: lw_paph2 LIKE ztco_model_map-paph2.

  CLEAR: it_bdc, it_bdc[], w_model_nf, w_model_cm, it_mess, it_mess[].
  CLEAR: w_model_yf.

  lw_diff =  w_claim_amount - w_dmbtr.
*  LW_DIFF_C = LW_DIFF.
* lw_diff_c = abs( lw_diff_c ).
  lw_claim_amount_c = abs( w_claim_amount ).
  lw_dmbtr_c = w_dmbtr.
  WRITE: sy-datum TO lw_todate.
  l_monat = p_pdate+4(2).

  READ TABLE lt_invoice INDEX 1.


  IF r_3 = 'X'.  "Parking
    PERFORM bdc_dynpro      USING 'SAPLF040' '0100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05V-NEWKO'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BKPF-BLDAT'
                                  w_date.
    PERFORM bdc_field       USING 'BKPF-BLART'
** Changed by Furong on 06/29/07
*                                  'KG'.
                                  'DG'.
** End of change
    PERFORM bdc_field       USING 'BKPF-BUKRS'
                                  p_bukrs.
    PERFORM bdc_field       USING 'BKPF-BUDAT'
                                 w_date.
    PERFORM bdc_field       USING 'BKPF-MONAT'
                                  p_monat.
    PERFORM bdc_field       USING 'BKPF-WAERS'
                                  w_waers.
    PERFORM bdc_field       USING 'BKPF-XBLNR'
                                  '123200'.
    PERFORM bdc_field       USING 'BKPF-BKTXT'
                                  'CLAIM SETTLEMENT'.
    PERFORM bdc_field       USING 'VBKPF-XBWAE'
                                  'X'.
*  PERFORM bdc_field       USING 'FS006-DOCID'
*                                record-docid_010.
** Change by Furong on 06/29/07
*    PERFORM BDC_FIELD       USING 'RF05V-NEWBS'
*                                  '24'.
    PERFORM bdc_field       USING 'RF05V-NEWBS'
                                  '04'.
** end of change
    PERFORM bdc_field       USING 'RF05V-NEWKO'
                                  lt_invoice-lifnr.
** Changed By Furong on 06/29*07
*    PERFORM BDC_DYNPRO      USING 'SAPLF040' '0302'.
    PERFORM bdc_dynpro      USING 'SAPLF040' '0301'.
** end of change
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05V-NEWKO'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BSEG-HKONT'
*                                  '123201'.     "claim A/R
                                  p_clgl.
    PERFORM bdc_field       USING 'BSEG-WRBTR'
                                  lw_claim_amount_c.
    PERFORM bdc_field       USING 'BSEG-ZFBDT'
                                  w_date.       "baseline date LW_TODATE
    PERFORM bdc_field       USING 'BSEG-ZUONR'
                                  '123200'.          "Assign
    PERFORM bdc_field       USING 'BSEG-SGTXT'
                                  '123200 CLAIM SETTLEMENT'.

*    PERFORM bdc_field       USING 'RF05V-NEWBS'
*                                  '50'.
*    PERFORM bdc_field       USING 'RF05V-NEWKO'
*                                  '123200'.
    CLEAR: lw_claim_amount_c.

    LOOP AT lt_invoice.
      l_dmbtr =  lt_invoice-dmbtr.
      AT FIRST .
        IF l_dmbtr < 0 .
          PERFORM bdc_field       USING 'RF05V-NEWBS'
                                        '40'.
        ELSE.
          PERFORM bdc_field       USING 'RF05V-NEWBS'
                                     '50'.
        ENDIF.

        PERFORM bdc_field       USING 'RF05V-NEWKO'
                                   '123200'.
      ENDAT.
      CLEAR: lw_paph2, w_matkl.

      lw_menge_c = abs( lt_invoice-menge ).
      lw_dmbtr_c = abs( lt_invoice-dmbtr ).
      PERFORM derive_model USING lt_invoice-matnr
                           CHANGING lw_paph2.

      CASE lw_paph2.
        WHEN '0000100001'.
          w_model_nf = w_model_nf + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        WHEN '0000100002'.
          w_model_cm = w_model_cm + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        WHEN '0000100004'.
          w_model_yf = w_model_yf + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        WHEN OTHERS.
* ??????????????????????????????????????????????????????????

      ENDCASE.

      PERFORM bdc_dynpro      USING 'SAPLF040' '0300'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'BSEG-MENGE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ZK'.
      PERFORM bdc_field       USING 'BSEG-WRBTR'
                                    lw_dmbtr_c.
      PERFORM bdc_field       USING 'BSEG-MENGE'
                                    lw_menge_c.
      PERFORM bdc_field       USING 'BSEG-MEINS'
                                    lt_invoice-meins.
      PERFORM bdc_field       USING 'BSEG-ZUONR'
                                    lt_invoice-lifnr.
      PERFORM bdc_field       USING 'BSEG-SGTXT'
                                    lt_invoice-matnr.
      PERFORM bdc_field       USING 'DKACB-FMORE'
                                    'X'.             "rd-fmore_024.

      PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'COBL-MATNR'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ENTE'.
      PERFORM bdc_field       USING 'COBL-MATNR'
                                    lt_invoice-matnr.
      PERFORM bdc_dynpro      USING 'SAPLF040' '0330'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RF05V-NEWKO'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      AT LAST.
        lw_last_rec = 'X'.
      ENDAT.

      IF lw_last_rec IS INITIAL.
        IF lt_invoice-dmbtr < 0.
          PERFORM bdc_field       USING 'RF05V-NEWBS'
                                           '40'.
        ELSE.
          PERFORM bdc_field       USING 'RF05V-NEWBS'
                                                '50'.
        ENDIF.
        PERFORM bdc_field       USING 'RF05V-NEWKO'
                                      '123200'.
      ELSE.
        CLEAR: lw_last_rec.

        IF w_model_cm = 0 OR w_model_nf = 0
        OR w_model_yf = 0.

          IF lw_diff > 0.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                          '50'.
            lw_diff_c = lw_diff.
          ELSE.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                         '40'.
            lw_diff_c = abs( lw_diff ).
          ENDIF.
          PERFORM bdc_field       USING 'RF05V-NEWKO'
                                        '531100'.
          PERFORM bdc_dynpro      USING 'SAPLF040' '0300'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'BSEG-SGTXT'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ZK'.
          PERFORM bdc_field       USING 'BSEG-WRBTR'
                                        lw_diff_c.
          PERFORM bdc_field       USING 'BSEG-ZUONR'
                                        lt_invoice-lifnr.
          PERFORM bdc_field       USING 'DKACB-FMORE'
                                        'X'.

          PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'COBL-KOSTL'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ENTE'.
          PERFORM bdc_field       USING 'COBL-KOSTL'
                                        '55101'.
          PERFORM bdc_dynpro      USING 'SAPLF040' '0330'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'BSEG-CCBTC'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=PBBP'.
          PERFORM bdc_dynpro      USING 'SAPLKEAK' '0300'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'RKEAK-FIELD(04)'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=WEIT'.
          IF w_model_cm = 0.
            PERFORM bdc_field       USING 'RKEAK-FIELD(04)'
                                          '0000100001'.     "lw_paph2.
          ENDIF.
          IF w_model_nf = 0.
            PERFORM bdc_field       USING 'RKEAK-FIELD(04)'
                                          '0000100002'.     "lw_paph2.
          ENDIF.
          IF w_model_yf = 0.
            PERFORM bdc_field       USING 'RKEAK-FIELD(04)'
                                          '0000100004'.     "lw_paph2.
          ENDIF.

        ELSE.

          IF w_model_nf > 0.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                          '50'.
            lw_diff_nf_c = w_model_nf.
          ELSE.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                         '40'.
            lw_diff_nf_c = abs( w_model_nf ).

          ENDIF.
          PERFORM bdc_field       USING 'RF05V-NEWKO'
                                        '531100'.


          PERFORM bdc_dynpro      USING 'SAPLF040' '0300'.

          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'BSEG-SGTXT'.

          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ZK'.
          PERFORM bdc_field       USING 'BSEG-WRBTR'
                                            lw_diff_nf_c.
          PERFORM bdc_field       USING 'BSEG-ZUONR'
                                        lt_invoice-lifnr.
          PERFORM bdc_field       USING 'DKACB-FMORE'
                                        'X'.             "rd-fmore_024.


          PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'COBL-KOSTL'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ENTE'.
          PERFORM bdc_field       USING 'COBL-KOSTL'
                                        '55101'.

          PERFORM bdc_dynpro      USING 'SAPLF040' '0330'.

*          PERFORM bdc_field       USING 'BDC_CURSOR'
*                                        'RF05V-NEWKO'.

          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '/00'.

** CM 553110
          IF w_model_cm  > 0.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                           '50'.
            lw_diff_cm_c = w_model_cm.
          ELSE.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                           '40'.
            lw_diff_cm_c = abs( w_model_cm ).
          ENDIF.

          PERFORM bdc_field       USING 'RF05V-NEWKO'
                                        '531100'.
          PERFORM bdc_dynpro      USING 'SAPLF040' '0300'.
          PERFORM bdc_field       USING 'BDC_CURSOR'

                                        'BSEG-WRBTR'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=BP'.
*

          PERFORM bdc_field       USING 'BSEG-WRBTR'
                                        lw_diff_cm_c.
          PERFORM bdc_field       USING 'DKACB-FMORE'
                                        'X'.

          PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'COBL-KOSTL'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ENTE'.
          PERFORM bdc_field       USING 'COBL-KOSTL'
                                        '55101'.

          PERFORM bdc_dynpro      USING 'SAPLF040' '0330'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '/00'.

** CM 553110
* YF
          IF w_model_yf  > 0.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                           '50'.
            lw_diff_yf_c = w_model_yf.
          ELSE.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                           '40'.
            lw_diff_yf_c = abs( w_model_yf ).
          ENDIF.

          PERFORM bdc_field       USING 'RF05V-NEWKO'
                                        '531100'.
          PERFORM bdc_dynpro      USING 'SAPLF040' '0300'.
          PERFORM bdc_field       USING 'BDC_CURSOR'

                                        'BSEG-WRBTR'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=BP'.
*

          PERFORM bdc_field       USING 'BSEG-WRBTR'
                                        lw_diff_yf_c.
          PERFORM bdc_field       USING 'DKACB-FMORE'
                                        'X'.

          PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'COBL-KOSTL'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ENTE'.
          PERFORM bdc_field       USING 'COBL-KOSTL'
                                        '55101'.

          PERFORM bdc_dynpro      USING 'SAPLF040' '0330'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '/00'.
* end

          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'BSEG-CCBTC'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=PBBP'.
          PERFORM bdc_dynpro      USING 'SAPLKEAK' '0300'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'RKEAK-FIELD(04)'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=WEIT'.
          IF w_model_cm = 0.
            PERFORM bdc_field       USING 'RKEAK-FIELD(04)'
                                          '0000100001'.     "lw_paph2.
          ENDIF.

          IF w_model_nf = 0.
            PERFORM bdc_field       USING 'RKEAK-FIELD(04)'
                                          '0000100002'.     "lw_paph2.
          ENDIF.

          IF w_model_yf = 0.
            PERFORM bdc_field       USING 'RKEAK-FIELD(04)'
                                          '0000100004'.     "lw_paph2.
          ENDIF.

        ENDIF.
      ENDIF.

*** CM 553110
    ENDLOOP.

    PERFORM bdc_dynpro      USING 'SAPLF040' '0330'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'BSEG-CCBTC'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                            '=BU'.

    CALL TRANSACTION 'F-66' USING it_bdc
                            MODE w_mode
                            UPDATE 'S'
                            MESSAGES INTO it_mess.

  ELSE.
** f-41
    PERFORM bdc_dynpro      USING 'SAPMF05A' '0100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05A-NEWKO'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BKPF-BLDAT'
                                  w_date.
    PERFORM bdc_field       USING 'BKPF-BLART'
** Changed by Furong on 06/29/07
*                                  'KG'.
                                  'DG'.
** End of change
    PERFORM bdc_field       USING 'BKPF-BUKRS'
                                  p_bukrs.
    PERFORM bdc_field       USING 'BKPF-BUDAT'
                                 w_date.
    PERFORM bdc_field       USING 'BKPF-MONAT'
                                  p_monat.
    PERFORM bdc_field       USING 'BKPF-WAERS'
                                  w_waers.
    PERFORM bdc_field       USING 'BKPF-XBLNR'
                                  '123200'.
    PERFORM bdc_field       USING 'BKPF-BKTXT'
                                  'CLAIM SETTLEMENT'.
*      PERFORM bdc_field       USING 'VBKPF-XBWAE'
*                                    'X'.
** Changed by Furong on 06/29/07
*    PERFORM BDC_FIELD       USING 'RF05A-NEWBS'
*                                  '24'.
* BEGIN OF UD1K950637
    IF w_claim_amount > 0.
      PERFORM bdc_field       USING 'RF05A-NEWBS'
                                    '04'.
    ELSE.
      PERFORM bdc_field       USING 'RF05A-NEWBS'
                                    '14'.
    ENDIF.
* END OF UD1K950637

** End of change
    PERFORM bdc_field       USING 'RF05A-NEWKO'
                                  lt_invoice-lifnr.

*    PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0302'.
    PERFORM bdc_dynpro      USING 'SAPMF05A' '0301'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05A-NEWKO'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BSEG-HKONT'
*                                  '123201'.     "claim A/R
                                  p_clgl.
    PERFORM bdc_field       USING 'BSEG-WRBTR'
                                  lw_claim_amount_c.
    PERFORM bdc_field       USING 'BSEG-ZFBDT'
                                w_date.       "baseline date LW_TODATE
    PERFORM bdc_field       USING 'BSEG-ZUONR'
                                  '123200'.          "Assign
    PERFORM bdc_field       USING 'BSEG-SGTXT'
                                  '123200 CLAIM SETTLEMENT'.

    CLEAR: lw_claim_amount_c.

    LOOP AT lt_invoice.
      l_tabix = sy-tabix.                                   "UD1K950637

      CLEAR: lw_paph2, w_matkl.
      l_dmbtr =  lt_invoice-dmbtr.
      AT FIRST .
        IF l_dmbtr < 0 .
          PERFORM bdc_field       USING 'RF05A-NEWBS'
                                        '40'.
        ELSE.
          PERFORM bdc_field       USING 'RF05A-NEWBS'
                                     '50'.
        ENDIF.
        PERFORM bdc_field       USING 'RF05A-NEWKO'
                                   '123200'.
      ENDAT.

      lw_menge_c = abs( lt_invoice-menge ).
      lw_dmbtr_c = abs( lt_invoice-dmbtr ).
      PERFORM derive_model USING lt_invoice-matnr
                           CHANGING lw_paph2.

*      SELECT SINGLE meins INTO l_meins
*      FROM mara
*      WHERE matnr = lt_invoice-matnr.

*FIXME... WHY hard coding...
      CASE lw_paph2.
        WHEN '0000100001'.
          w_model_nf = w_model_nf + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        WHEN '0000100002'.
          w_model_cm = w_model_cm + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        WHEN '0000100004'.
          w_model_yf = w_model_yf + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.

      ENDCASE.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'BSEG-MENGE'.

      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ZK'.
      PERFORM bdc_field       USING 'BSEG-WRBTR'
                                    lw_dmbtr_c.
      PERFORM bdc_field       USING 'BSEG-MENGE'
                                    lw_menge_c.
      PERFORM bdc_field       USING 'BSEG-MEINS'
                                    lt_invoice-meins.
      PERFORM bdc_field       USING 'BSEG-ZUONR'
                                    lt_invoice-lifnr.
      PERFORM bdc_field       USING 'BSEG-SGTXT'
                                    lt_invoice-matnr.

      PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'COBL-MATNR'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ENTE'.
      PERFORM bdc_field       USING 'COBL-MATNR'
                                    lt_invoice-matnr.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '0330'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RF05A-NEWKO'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      AT LAST.
        lw_last_rec = 'X'.
      ENDAT.

      IF lw_last_rec IS INITIAL.
        l_tabix = l_tabix + 1.                              "UD1K950637
        READ TABLE lt_invoice INDEX l_tabix.                "UD1K950637

        IF lt_invoice-dmbtr < 0.
          PERFORM bdc_field       USING 'RF05A-NEWBS'
                                               '40'.
        ELSE.
          PERFORM bdc_field       USING 'RF05A-NEWBS'
                                                '50'.
        ENDIF.
        PERFORM bdc_field       USING 'RF05A-NEWKO'
                                      '123200'.
      ELSE.
        CLEAR: lw_last_rec.
        IF lw_diff <> 0.


          IF w_model_cm = 0 OR w_model_nf = 0
          OR w_model_yf = 0.

            IF lw_diff > 0.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                            '50'.
              lw_diff_c = lw_diff.
            ELSE.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                           '40'.
              lw_diff_c = abs( lw_diff ).
            ENDIF.

            PERFORM bdc_field       USING 'RF05A-NEWKO'
                                          '531100'.
            PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'BSEG-SGTXT'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ZK'.
            PERFORM bdc_field       USING 'BSEG-WRBTR'
                                          lw_diff_c.
            PERFORM bdc_field       USING 'BSEG-ZUONR'
                                          lt_invoice-lifnr.

            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=COBL_XERGO'.
            PERFORM bdc_field       USING 'COBL-KOSTL'
                                          '55101'.
            PERFORM bdc_field       USING 'DKACB-XERGO'
                                          'X'.

            PERFORM bdc_dynpro      USING 'SAPLKEAK' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RKEAK-FIELD(01)'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=WEIT'.

*** : KDM01(ADD) - Start
            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
*** : KDM01(ADD) - End
          ELSE.

            IF w_model_nf > 0.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                            '50'.
              lw_diff_nf_c = w_model_nf.
            ELSE.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                           '40'.
              lw_diff_nf_c = abs( w_model_nf ).
            ENDIF.

            PERFORM bdc_field       USING 'RF05A-NEWKO'
                                          '531100'.
            PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'BSEG-SGTXT'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ZK'.
            PERFORM bdc_field       USING 'BSEG-WRBTR'
                                          lw_diff_nf_c.
            PERFORM bdc_field       USING 'BSEG-ZUONR'
                                          lt_invoice-lifnr.
            PERFORM bdc_field       USING 'BSEG-SGTXT'
                                         lt_invoice-matnr.

            IF w_model_cm > 0.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                             '50'.
              lw_diff_cm_c = w_model_cm.
            ELSE.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                           '40'.
              lw_diff_cm_c = abs( w_model_cm ).
            ENDIF.

            PERFORM bdc_field       USING 'RF05A-NEWKO'
                                          '531100'.

            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
            PERFORM bdc_field       USING 'COBL-KOSTL'
                                          '55101'.
            PERFORM bdc_field       USING 'DKACB-XERGO'
                                           'X'.

            PERFORM bdc_dynpro      USING 'SAPLKEAK' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RKEAK-FIELD(04)'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=WEIT'.
*** : KDM01(ADD) - Start
            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
*** : KDM01(ADD) - End

            PERFORM bdc_dynpro      USING 'SAPMF05A' '0330'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RF05A-NEWKO'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '/00'.

            PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'BSEG-ZUONR'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ZK'.
            PERFORM bdc_field       USING 'BSEG-WRBTR'
                                          lw_diff_cm_c.
            PERFORM bdc_field       USING 'BSEG-ZUONR'
                                          lt_invoice-lifnr.

*
            IF w_model_yf > 0.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                             '50'.
              lw_diff_yf_c = w_model_yf.
            ELSE.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                           '40'.
              lw_diff_yf_c = abs( w_model_yf ).
            ENDIF.

            PERFORM bdc_field       USING 'RF05A-NEWKO'
                                          '531100'.

            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
            PERFORM bdc_field       USING 'COBL-KOSTL'
                                          '55101'.
            PERFORM bdc_field       USING 'DKACB-XERGO'
                                           'X'.
*            PERFORM bdc_field       USING 'BDC_OKCODE'
*                                          '=ENTE'.

            PERFORM bdc_dynpro      USING 'SAPLKEAK' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RKEAK-FIELD(04)'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=WEIT'.
*** : KDM01(ADD) - Start
            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
*** : KDM01(ADD) - End

            PERFORM bdc_dynpro      USING 'SAPMF05A' '0330'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RF05A-NEWKO'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '/00'.

            PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'BSEG-ZUONR'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ZK'.
            PERFORM bdc_field       USING 'BSEG-WRBTR'
                                          lw_diff_yf_c.
            PERFORM bdc_field       USING 'BSEG-ZUONR'
                                          lt_invoice-lifnr.

*
            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
            PERFORM bdc_field       USING 'COBL-KOSTL'
                                          '55101'.
            PERFORM bdc_field       USING 'DKACB-XERGO'
                                           'X'.

            PERFORM bdc_dynpro      USING 'SAPLKEAK' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RKEAK-FIELD(04)'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=WEIT'.

*** : KDM01(ADD) - Start
            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
*** : KDM01(ADD) - End

          ENDIF.

        ENDIF. " lw_diff <> 0.


      ENDIF.
    ENDLOOP.
    PERFORM bdc_dynpro      USING 'SAPMF05A' '0330'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'BSEG-CCBTC'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                            '=BU'.

    CALL TRANSACTION 'F-41' USING it_bdc
                            MODE w_mode
                            UPDATE 'S'
                            MESSAGES INTO it_mess.

  ENDIF.

  READ TABLE it_mess WITH KEY msgtyp = 'E'.

  IF sy-subrc EQ 0.
    READ TABLE lt_invoice INDEX 1.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt1  = 'Error for parking of Credit Memo (Scrap)'
              txt2  = it_mess-msgv1
              txt3  = lt_invoice-lifnr
              txt4  = it_mess-msgv2.
  ELSE.
    IF r_3 = 'X'.
*    READ TABLE it_mess WITH KEY msgtyp = 'S' msgnr = '001'.
      READ TABLE it_mess WITH KEY msgtyp = 'S' dynumb = '0300'.
      IF sy-subrc <> 0.
        READ TABLE it_mess WITH KEY msgtyp = 'S' dynumb = '0330'.
      ENDIF.
    ELSE.
      READ TABLE it_mess WITH KEY msgtyp = 'S' dynumb = '0700'.
      IF sy-subrc <> 0.
        READ TABLE it_mess WITH KEY msgtyp = 'S' dynumb = '0312'.
      ENDIF.
    ENDIF.
    IF sy-subrc = 0.
      CLEAR: it_ztfi_claim_stl.
**  change by Furong on 06/28/07
*      READ TABLE LT_INVOICE INDEX 1.
      LOOP AT lt_invoice.
        it_ztfi_claim_stl-lifnr = lt_invoice-lifnr.
        it_ztfi_claim_stl-matnr = lt_invoice-matnr.
*      it_ztfi_claim_stl-ebeln = it_SCRAP_inv-ebeln.
*      it_ztfi_claim_stl-ebelp = it_SCRAP_inv-ebelp.
        it_ztfi_claim_stl-gjahr = p_gjahr.
        it_ztfi_claim_stl-lfmon = p_monat.
*      it_ztfi_claim_stl-lfbnr = it_SCRAP_inv-lfbnr.
*      it_ztfi_claim_stl-lfpos = it_SCRAP_inv-lfpos.
        it_ztfi_claim_stl-ctype = 'SCRAP'.
        it_ztfi_claim_stl-meins = lt_invoice-meins.
        it_ztfi_claim_stl-menge = lt_invoice-menge.
*        IT_ZTFI_CLAIM_STL-DMBTR = W_DMBTR.
        it_ztfi_claim_stl-dmbtr = lt_invoice-dmbtr.
        it_ztfi_claim_stl-amount = lt_invoice-amount.
*        IT_ZTFI_CLAIM_STL-CLAIM_AMOUNT = W_CLAIM_AMOUNT.
        it_ztfi_claim_stl-claim_amount = lt_invoice-claim_amount.
        it_ztfi_claim_stl-budat =  p_pdate.
        it_ztfi_claim_stl-inv_no = it_mess-msgv1.
        it_ztfi_claim_stl-ernam = sy-uname.
        it_ztfi_claim_stl-erdat = sy-datum.
        it_ztfi_claim_stl-erzet = sy-uzeit.
        APPEND it_ztfi_claim_stl.
      ENDLOOP.
*    CONCATENATE IT_MESS-MSGV1 ' was created' INTO l_text.
*    MESSAGE s000 WITH l_text.
    ENDIF.
  ENDIF.
  CLEAR: it_mess, it_mess[].
ENDFORM.                    " call_bdc_inv

* BEGIN OF UD1K955730
*&---------------------------------------------------------------------*
*&      Form  call_bdc_inv2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc_inv2.
  DATA: l_tabix TYPE sy-tabix.                              "UD1K950637
  DATA: lw_menge_c(13),
        lw_dmbtr_c(13),
        l_meins LIKE ekpo-meins,
        lw_claim_amount_c(13),
        lw_diff_c(13),
        lw_diff_nf_c(13),
        lw_diff_cm_c(13),
        lw_diff_yf_c(13),
        lw_diff LIKE konp-kbetr,
        lw_todate(10),
        l_text(30),
        l_monat(2),
        l_dmbtr LIKE lt_invoice-dmbtr,
        lw_last_rec(1).
  DATA: lw_paph2 LIKE ztco_model_map-paph2.

  CLEAR: it_bdc, it_bdc[], w_model_nf, w_model_cm, it_mess, it_mess[].
  CLEAR: w_model_yf.

  lw_diff =  w_claim_amount - w_dmbtr.
*  LW_DIFF_C = LW_DIFF.
* lw_diff_c = abs( lw_diff_c ).
  lw_claim_amount_c = abs( w_claim_amount ).
  lw_dmbtr_c = w_dmbtr.
  WRITE: sy-datum TO lw_todate.
  l_monat = p_pdate+4(2).

  READ TABLE lt_invoice INDEX 1.


  IF r_3 = 'X'.  "Parking
    PERFORM bdc_dynpro      USING 'SAPLF040' '0100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05V-NEWKO'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BKPF-BLDAT'
                                  w_date.
    PERFORM bdc_field       USING 'BKPF-BLART'
** Changed by Furong on 06/29/07
*                                  'KG'.
                                  'DG'.
** End of change
    PERFORM bdc_field       USING 'BKPF-BUKRS'
                                  p_bukrs.
    PERFORM bdc_field       USING 'BKPF-BUDAT'
                                 w_date.
    PERFORM bdc_field       USING 'BKPF-MONAT'
                                  p_monat.
    PERFORM bdc_field       USING 'BKPF-WAERS'
                                  w_waers.
    PERFORM bdc_field       USING 'BKPF-XBLNR'
                                  '123200'.
    PERFORM bdc_field       USING 'BKPF-BKTXT'
                                  'CLAIM SETTLEMENT'.
    PERFORM bdc_field       USING 'VBKPF-XBWAE'
                                  'X'.
*  PERFORM bdc_field       USING 'FS006-DOCID'
*                                record-docid_010.
** Change by Furong on 06/29/07
*    PERFORM BDC_FIELD       USING 'RF05V-NEWBS'
*                                  '24'.
    PERFORM bdc_field       USING 'RF05V-NEWBS'
                                  '04'.
** end of change
    PERFORM bdc_field       USING 'RF05V-NEWKO'
                                  lt_invoice-lifnr.
** Changed By Furong on 06/29*07
*    PERFORM BDC_DYNPRO      USING 'SAPLF040' '0302'.
    PERFORM bdc_dynpro      USING 'SAPLF040' '0301'.
** end of change
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05V-NEWKO'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BSEG-HKONT'
*                                  '123201'.     "claim A/R
                                  p_clgl.
    PERFORM bdc_field       USING 'BSEG-WRBTR'
                                  lw_claim_amount_c.
    PERFORM bdc_field       USING 'BSEG-ZFBDT'
                                  w_date.       "baseline date LW_TODATE
    PERFORM bdc_field       USING 'BSEG-ZUONR'
                                  '123200'.          "Assign
    PERFORM bdc_field       USING 'BSEG-SGTXT'
                                  '123200 CLAIM SETTLEMENT'.

*    PERFORM bdc_field       USING 'RF05V-NEWBS'
*                                  '50'.
*    PERFORM bdc_field       USING 'RF05V-NEWKO'
*                                  '123200'.
    CLEAR: lw_claim_amount_c.

    LOOP AT lt_invoice.
      l_dmbtr =  lt_invoice-dmbtr.
      AT FIRST .
        IF l_dmbtr < 0 .
          PERFORM bdc_field       USING 'RF05V-NEWBS'
                                        '40'.
        ELSE.
          PERFORM bdc_field       USING 'RF05V-NEWBS'
                                     '50'.
        ENDIF.

        PERFORM bdc_field       USING 'RF05V-NEWKO'
                                   '123200'.
      ENDAT.
      CLEAR: lw_paph2, w_matkl.

      lw_menge_c = abs( lt_invoice-menge ).
      lw_dmbtr_c = abs( lt_invoice-dmbtr ).
      PERFORM derive_model USING lt_invoice-matnr
                           CHANGING lw_paph2.

      CASE lw_paph2.
        WHEN '0000100001'.
          w_model_nf = w_model_nf + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        WHEN '0000100002'.
          w_model_cm = w_model_cm + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        WHEN '0000100004'.
          w_model_yf = w_model_yf + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        WHEN OTHERS.
* ??????????????????????????????????????????????????????????

      ENDCASE.

      PERFORM bdc_dynpro      USING 'SAPLF040' '0300'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'BSEG-MENGE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ZK'.
      PERFORM bdc_field       USING 'BSEG-WRBTR'
                                    lw_dmbtr_c.
      PERFORM bdc_field       USING 'BSEG-MENGE'
                                    lw_menge_c.
      PERFORM bdc_field       USING 'BSEG-MEINS'
                                    lt_invoice-meins.
      PERFORM bdc_field       USING 'BSEG-ZUONR'
                                    lt_invoice-lifnr.
      PERFORM bdc_field       USING 'BSEG-SGTXT'
                                    lt_invoice-matnr.
      PERFORM bdc_field       USING 'DKACB-FMORE'
                                    'X'.             "rd-fmore_024.

      PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'COBL-MATNR'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ENTE'.
      PERFORM bdc_field       USING 'COBL-MATNR'
                                    lt_invoice-matnr.
      PERFORM bdc_dynpro      USING 'SAPLF040' '0330'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RF05V-NEWKO'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      AT LAST.
        lw_last_rec = 'X'.
      ENDAT.

      IF lw_last_rec IS INITIAL.
        IF lt_invoice-dmbtr < 0.
          PERFORM bdc_field       USING 'RF05V-NEWBS'
                                           '40'.
        ELSE.
          PERFORM bdc_field       USING 'RF05V-NEWBS'
                                                '50'.
        ENDIF.
        PERFORM bdc_field       USING 'RF05V-NEWKO'
                                      '123200'.
      ELSE.
        CLEAR: lw_last_rec.

        IF w_model_cm = 0 OR w_model_nf = 0
        OR w_model_yf = 0.

          IF lw_diff > 0.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                          '50'.
            lw_diff_c = lw_diff.
          ELSE.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                         '40'.
            lw_diff_c = abs( lw_diff ).
          ENDIF.
          PERFORM bdc_field       USING 'RF05V-NEWKO'
                                        '531100'.
          PERFORM bdc_dynpro      USING 'SAPLF040' '0300'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'BSEG-SGTXT'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ZK'.
          PERFORM bdc_field       USING 'BSEG-WRBTR'
                                        lw_diff_c.
          PERFORM bdc_field       USING 'BSEG-ZUONR'
                                        lt_invoice-lifnr.
          PERFORM bdc_field       USING 'DKACB-FMORE'
                                        'X'.

          PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'COBL-KOSTL'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ENTE'.
          PERFORM bdc_field       USING 'COBL-KOSTL'
                                        '55101'.
          PERFORM bdc_dynpro      USING 'SAPLF040' '0330'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'BSEG-CCBTC'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=PBBP'.
          PERFORM bdc_dynpro      USING 'SAPLKEAK' '0300'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'RKEAK-FIELD(04)'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=WEIT'.
          IF w_model_cm = 0.
            PERFORM bdc_field       USING 'RKEAK-FIELD(04)'
                                          '0000100001'.     "lw_paph2.
          ENDIF.
          IF w_model_nf = 0.
            PERFORM bdc_field       USING 'RKEAK-FIELD(04)'
                                          '0000100002'.     "lw_paph2.
          ENDIF.
          IF w_model_yf = 0.
            PERFORM bdc_field       USING 'RKEAK-FIELD(04)'
                                          '0000100004'.     "lw_paph2.
          ENDIF.

        ELSE.

          IF w_model_nf > 0.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                          '50'.
            lw_diff_nf_c = w_model_nf.
          ELSE.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                         '40'.
            lw_diff_nf_c = abs( w_model_nf ).

          ENDIF.
          PERFORM bdc_field       USING 'RF05V-NEWKO'
                                        '531100'.


          PERFORM bdc_dynpro      USING 'SAPLF040' '0300'.

          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'BSEG-SGTXT'.

          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ZK'.
          PERFORM bdc_field       USING 'BSEG-WRBTR'
                                            lw_diff_nf_c.
          PERFORM bdc_field       USING 'BSEG-ZUONR'
                                        lt_invoice-lifnr.
          PERFORM bdc_field       USING 'DKACB-FMORE'
                                        'X'.             "rd-fmore_024.


          PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'COBL-KOSTL'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ENTE'.
          PERFORM bdc_field       USING 'COBL-KOSTL'
                                        '55101'.

          PERFORM bdc_dynpro      USING 'SAPLF040' '0330'.

*          PERFORM bdc_field       USING 'BDC_CURSOR'
*                                        'RF05V-NEWKO'.

          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '/00'.

** CM 553110
          IF w_model_cm  > 0.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                           '50'.
            lw_diff_cm_c = w_model_cm.
          ELSE.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                           '40'.
            lw_diff_cm_c = abs( w_model_cm ).
          ENDIF.

          PERFORM bdc_field       USING 'RF05V-NEWKO'
                                        '531100'.
          PERFORM bdc_dynpro      USING 'SAPLF040' '0300'.
          PERFORM bdc_field       USING 'BDC_CURSOR'

                                        'BSEG-WRBTR'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=BP'.
*

          PERFORM bdc_field       USING 'BSEG-WRBTR'
                                        lw_diff_cm_c.
          PERFORM bdc_field       USING 'DKACB-FMORE'
                                        'X'.

          PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'COBL-KOSTL'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ENTE'.
          PERFORM bdc_field       USING 'COBL-KOSTL'
                                        '55101'.

          PERFORM bdc_dynpro      USING 'SAPLF040' '0330'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '/00'.

** CM 553110
* YF
          IF w_model_yf  > 0.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                           '50'.
            lw_diff_yf_c = w_model_yf.
          ELSE.
            PERFORM bdc_field       USING 'RF05V-NEWBS'
                                           '40'.
            lw_diff_yf_c = abs( w_model_yf ).
          ENDIF.

          PERFORM bdc_field       USING 'RF05V-NEWKO'
                                        '531100'.
          PERFORM bdc_dynpro      USING 'SAPLF040' '0300'.
          PERFORM bdc_field       USING 'BDC_CURSOR'

                                        'BSEG-WRBTR'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=BP'.
*

          PERFORM bdc_field       USING 'BSEG-WRBTR'
                                        lw_diff_yf_c.
          PERFORM bdc_field       USING 'DKACB-FMORE'
                                        'X'.

          PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'COBL-KOSTL'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ENTE'.
          PERFORM bdc_field       USING 'COBL-KOSTL'
                                        '55101'.

          PERFORM bdc_dynpro      USING 'SAPLF040' '0330'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '/00'.
* end

          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'BSEG-CCBTC'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=PBBP'.
          PERFORM bdc_dynpro      USING 'SAPLKEAK' '0300'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'RKEAK-FIELD(04)'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=WEIT'.
          IF w_model_cm = 0.
            PERFORM bdc_field       USING 'RKEAK-FIELD(04)'
                                          '0000100001'.     "lw_paph2.
          ENDIF.

          IF w_model_nf = 0.
            PERFORM bdc_field       USING 'RKEAK-FIELD(04)'
                                          '0000100002'.     "lw_paph2.
          ENDIF.

          IF w_model_yf = 0.
            PERFORM bdc_field       USING 'RKEAK-FIELD(04)'
                                          '0000100004'.     "lw_paph2.
          ENDIF.

        ENDIF.
      ENDIF.

*** CM 553110
    ENDLOOP.

    PERFORM bdc_dynpro      USING 'SAPLF040' '0330'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'BSEG-CCBTC'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                            '=BU'.

    CALL TRANSACTION 'F-66' USING it_bdc
                            MODE w_mode
                            UPDATE 'S'
                            MESSAGES INTO it_mess.

  ELSE.
** f-41
    PERFORM bdc_dynpro      USING 'SAPMF05A' '0100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05A-NEWKO'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BKPF-BLDAT'
                                  w_date.
    PERFORM bdc_field       USING 'BKPF-BLART'
** Changed by Furong on 06/29/07
*                                  'KG'.
                                  'DG'.
** End of change
    PERFORM bdc_field       USING 'BKPF-BUKRS'
                                  p_bukrs.
    PERFORM bdc_field       USING 'BKPF-BUDAT'
                                 w_date.
    PERFORM bdc_field       USING 'BKPF-MONAT'
                                  p_monat.
    PERFORM bdc_field       USING 'BKPF-WAERS'
                                  w_waers.
    PERFORM bdc_field       USING 'BKPF-XBLNR'
                                  '123206'.
    PERFORM bdc_field       USING 'BKPF-BKTXT'
                                  'CLAIM SETTLEMENT'.
*      PERFORM bdc_field       USING 'VBKPF-XBWAE'
*                                    'X'.
** Changed by Furong on 06/29/07
*    PERFORM BDC_FIELD       USING 'RF05A-NEWBS'
*                                  '24'.
* BEGIN OF UD1K950637
    IF w_claim_amount > 0.
      PERFORM bdc_field       USING 'RF05A-NEWBS'
                                    '04'.
    ELSE.
      PERFORM bdc_field       USING 'RF05A-NEWBS'
                                    '14'.
    ENDIF.
* END OF UD1K950637

** End of change
    PERFORM bdc_field       USING 'RF05A-NEWKO'
                                  lt_invoice-lifnr.

*    PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0302'.
    PERFORM bdc_dynpro      USING 'SAPMF05A' '0301'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05A-NEWKO'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BSEG-HKONT'
*                                  '123201'.     "claim A/R
                                  p_clgl.
    PERFORM bdc_field       USING 'BSEG-WRBTR'
                                  lw_claim_amount_c.
    PERFORM bdc_field       USING 'BSEG-ZFBDT'
                                w_date.       "baseline date LW_TODATE
    PERFORM bdc_field       USING 'BSEG-ZUONR'
                                  '123206'.          "Assign
    PERFORM bdc_field       USING 'BSEG-SGTXT'
                                  '123206 CLAIM SETTLEMENT'.

    CLEAR: lw_claim_amount_c.

    LOOP AT lt_invoice.
      l_tabix = sy-tabix.                                   "UD1K950637

      CLEAR: lw_paph2, w_matkl.
      l_dmbtr =  lt_invoice-dmbtr.
      AT FIRST .
        IF l_dmbtr < 0 .
          PERFORM bdc_field       USING 'RF05A-NEWBS'
                                        '40'.
        ELSE.
          PERFORM bdc_field       USING 'RF05A-NEWBS'
                                     '50'.
        ENDIF.
        PERFORM bdc_field       USING 'RF05A-NEWKO'
                                   '123206'.
      ENDAT.

      lw_menge_c = abs( lt_invoice-menge ).
      lw_dmbtr_c = abs( lt_invoice-dmbtr ).
      PERFORM derive_model USING lt_invoice-matnr
                           CHANGING lw_paph2.

*      SELECT SINGLE meins INTO l_meins
*      FROM mara
*      WHERE matnr = lt_invoice-matnr.

*FIXME... WHY hard coding...
      CASE lw_paph2.
        WHEN '0000100001'.
          w_model_nf = w_model_nf + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        WHEN '0000100002'.
          w_model_cm = w_model_cm + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        WHEN '0000100004'.
          w_model_yf = w_model_yf + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.

      ENDCASE.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'BSEG-MENGE'.

      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ZK'.
      PERFORM bdc_field       USING 'BSEG-WRBTR'
                                    lw_dmbtr_c.
      PERFORM bdc_field       USING 'BSEG-MENGE'
                                    lw_menge_c.
      PERFORM bdc_field       USING 'BSEG-MEINS'
                                    lt_invoice-meins.
      PERFORM bdc_field       USING 'BSEG-ZUONR'
                                    lt_invoice-lifnr.
      PERFORM bdc_field       USING 'BSEG-SGTXT'
                                    lt_invoice-matnr.

      PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'COBL-MATNR'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ENTE'.
      PERFORM bdc_field       USING 'COBL-MATNR'
                                    lt_invoice-matnr.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '0330'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RF05A-NEWKO'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      AT LAST.
        lw_last_rec = 'X'.
      ENDAT.

      IF lw_last_rec IS INITIAL.
        l_tabix = l_tabix + 1.                              "UD1K950637
        READ TABLE lt_invoice INDEX l_tabix.                "UD1K950637

        IF lt_invoice-dmbtr < 0.
          PERFORM bdc_field       USING 'RF05A-NEWBS'
                                               '40'.
        ELSE.
          PERFORM bdc_field       USING 'RF05A-NEWBS'
                                                '50'.
        ENDIF.
        PERFORM bdc_field       USING 'RF05A-NEWKO'
                                      '123206'.
      ELSE.
        CLEAR: lw_last_rec.
        IF lw_diff <> 0.


          IF w_model_cm = 0 OR w_model_nf = 0
          OR w_model_yf = 0.

            IF lw_diff > 0.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                            '50'.
              lw_diff_c = lw_diff.
            ELSE.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                           '40'.
              lw_diff_c = abs( lw_diff ).
            ENDIF.

            PERFORM bdc_field       USING 'RF05A-NEWKO'
                                          '531100'.
            PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'BSEG-SGTXT'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ZK'.
            PERFORM bdc_field       USING 'BSEG-WRBTR'
                                          lw_diff_c.
            PERFORM bdc_field       USING 'BSEG-ZUONR'
                                          lt_invoice-lifnr.

            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=COBL_XERGO'.
            PERFORM bdc_field       USING 'COBL-KOSTL'
                                          '55101'.
            PERFORM bdc_field       USING 'DKACB-XERGO'
                                          'X'.

            PERFORM bdc_dynpro      USING 'SAPLKEAK' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RKEAK-FIELD(01)'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=WEIT'.

*** : KDM01(ADD) - Start
            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
*** : KDM01(ADD) - End
          ELSE.

            IF w_model_nf > 0.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                            '50'.
              lw_diff_nf_c = w_model_nf.
            ELSE.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                           '40'.
              lw_diff_nf_c = abs( w_model_nf ).
            ENDIF.

            PERFORM bdc_field       USING 'RF05A-NEWKO'
                                          '531100'.
            PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'BSEG-SGTXT'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ZK'.
            PERFORM bdc_field       USING 'BSEG-WRBTR'
                                          lw_diff_nf_c.
            PERFORM bdc_field       USING 'BSEG-ZUONR'
                                          lt_invoice-lifnr.
            PERFORM bdc_field       USING 'BSEG-SGTXT'
                                         lt_invoice-matnr.

            IF w_model_cm > 0.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                             '50'.
              lw_diff_cm_c = w_model_cm.
            ELSE.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                           '40'.
              lw_diff_cm_c = abs( w_model_cm ).
            ENDIF.

            PERFORM bdc_field       USING 'RF05A-NEWKO'
                                          '531100'.

            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
            PERFORM bdc_field       USING 'COBL-KOSTL'
                                          '55101'.
            PERFORM bdc_field       USING 'DKACB-XERGO'
                                           'X'.

            PERFORM bdc_dynpro      USING 'SAPLKEAK' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RKEAK-FIELD(04)'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=WEIT'.
*** : KDM01(ADD) - Start
            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
*** : KDM01(ADD) - End

            PERFORM bdc_dynpro      USING 'SAPMF05A' '0330'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RF05A-NEWKO'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '/00'.

            PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'BSEG-ZUONR'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ZK'.
            PERFORM bdc_field       USING 'BSEG-WRBTR'
                                          lw_diff_cm_c.
            PERFORM bdc_field       USING 'BSEG-ZUONR'
                                          lt_invoice-lifnr.

*
            IF w_model_yf > 0.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                             '50'.
              lw_diff_yf_c = w_model_yf.
            ELSE.
              PERFORM bdc_field       USING 'RF05A-NEWBS'
                                           '40'.
              lw_diff_yf_c = abs( w_model_yf ).
            ENDIF.

            PERFORM bdc_field       USING 'RF05A-NEWKO'
                                          '531100'.

            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
            PERFORM bdc_field       USING 'COBL-KOSTL'
                                          '55101'.
            PERFORM bdc_field       USING 'DKACB-XERGO'
                                           'X'.
*            PERFORM bdc_field       USING 'BDC_OKCODE'
*                                          '=ENTE'.

            PERFORM bdc_dynpro      USING 'SAPLKEAK' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RKEAK-FIELD(04)'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=WEIT'.
*** : KDM01(ADD) - Start
            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
*** : KDM01(ADD) - End

            PERFORM bdc_dynpro      USING 'SAPMF05A' '0330'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RF05A-NEWKO'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '/00'.

            PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'BSEG-ZUONR'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ZK'.
            PERFORM bdc_field       USING 'BSEG-WRBTR'
                                          lw_diff_yf_c.
            PERFORM bdc_field       USING 'BSEG-ZUONR'
                                          lt_invoice-lifnr.

*
            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
            PERFORM bdc_field       USING 'COBL-KOSTL'
                                          '55101'.
            PERFORM bdc_field       USING 'DKACB-XERGO'
                                           'X'.

            PERFORM bdc_dynpro      USING 'SAPLKEAK' '0300'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RKEAK-FIELD(04)'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=WEIT'.

*** : KDM01(ADD) - Start
            PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=ENTE'.
*** : KDM01(ADD) - End

          ENDIF.

        ENDIF. " lw_diff <> 0.


      ENDIF.
    ENDLOOP.
    PERFORM bdc_dynpro      USING 'SAPMF05A' '0330'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'BSEG-CCBTC'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                            '=BU'.

    CALL TRANSACTION 'F-41' USING it_bdc
                            MODE w_mode
                            UPDATE 'S'
                            MESSAGES INTO it_mess.

  ENDIF.

  READ TABLE it_mess WITH KEY msgtyp = 'E'.

  IF sy-subrc EQ 0.
    READ TABLE lt_invoice INDEX 1.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt1  = 'Error for parking of Credit Memo (Scrap)'
              txt2  = it_mess-msgv1
              txt3  = lt_invoice-lifnr
              txt4  = it_mess-msgv2.
  ELSE.
    IF r_3 = 'X'.
*    READ TABLE it_mess WITH KEY msgtyp = 'S' msgnr = '001'.
      READ TABLE it_mess WITH KEY msgtyp = 'S' dynumb = '0300'.
      IF sy-subrc <> 0.
        READ TABLE it_mess WITH KEY msgtyp = 'S' dynumb = '0330'.
      ENDIF.
    ELSE.
      READ TABLE it_mess WITH KEY msgtyp = 'S' dynumb = '0700'.
      IF sy-subrc <> 0.
        READ TABLE it_mess WITH KEY msgtyp = 'S' dynumb = '0312'.
      ENDIF.
    ENDIF.
    IF sy-subrc = 0.
      CLEAR: it_ztfi_claim_stl.
**  change by Furong on 06/28/07
*      READ TABLE LT_INVOICE INDEX 1.
      LOOP AT lt_invoice.
        it_ztfi_claim_stl-lifnr = lt_invoice-lifnr.
        it_ztfi_claim_stl-matnr = lt_invoice-matnr.
*      it_ztfi_claim_stl-ebeln = it_SCRAP_inv-ebeln.
*      it_ztfi_claim_stl-ebelp = it_SCRAP_inv-ebelp.
        it_ztfi_claim_stl-gjahr = p_gjahr.
        it_ztfi_claim_stl-lfmon = p_monat.
*      it_ztfi_claim_stl-lfbnr = it_SCRAP_inv-lfbnr.
*      it_ztfi_claim_stl-lfpos = it_SCRAP_inv-lfpos.
        it_ztfi_claim_stl-ctype = 'SCRAP'.
        it_ztfi_claim_stl-meins = lt_invoice-meins.
        it_ztfi_claim_stl-menge = lt_invoice-menge.
*        IT_ZTFI_CLAIM_STL-DMBTR = W_DMBTR.
        it_ztfi_claim_stl-dmbtr = lt_invoice-dmbtr.
        it_ztfi_claim_stl-amount = lt_invoice-amount.
*        IT_ZTFI_CLAIM_STL-CLAIM_AMOUNT = W_CLAIM_AMOUNT.
        it_ztfi_claim_stl-claim_amount = lt_invoice-claim_amount.
        it_ztfi_claim_stl-budat =  p_pdate.
        it_ztfi_claim_stl-inv_no = it_mess-msgv1.
        it_ztfi_claim_stl-ernam = sy-uname.
        it_ztfi_claim_stl-erdat = sy-datum.
        it_ztfi_claim_stl-erzet = sy-uzeit.
        APPEND it_ztfi_claim_stl.
      ENDLOOP.
*    CONCATENATE IT_MESS-MSGV1 ' was created' INTO l_text.
*    MESSAGE s000 WITH l_text.
    ENDIF.
  ENDIF.
  CLEAR: it_mess, it_mess[].
ENDFORM.                    " call_bdc_inv2
* END OF UD1K955730

*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat_display.
*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'CLMTYPE'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
* it_sort-spos           = 2.
*  it_sort-fieldname      = 'LIFNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 3.
*  it_sort-fieldname      = 'EBELN'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = 'X'.
*  APPEND it_sort.
*
*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.

ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM BDC_DYNPRO                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PROGRAM                                                       *
*  -->  DYNPRO                                                        *
*---------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR it_bdc.
  it_bdc-program  = program.
  it_bdc-dynpro   = dynpro.
  it_bdc-dynbegin = 'X'.
  APPEND it_bdc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM BDC_FIELD                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FNAM                                                          *
*  -->  FVAL                                                          *
*---------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR it_bdc.
  it_bdc-fnam = fnam.
  it_bdc-fval = fval.
  APPEND it_bdc.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  derive_model
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM derive_model USING p_matnr
          CHANGING p_model LIKE ztco_model_map-paph2.

  DATA: l_model LIKE ztco_model_map-modl1.

  SELECT SINGLE matkl INTO w_matkl FROM mara
    WHERE matnr = p_matnr.

*  l_model = w_matkl(2).
  CONCATENATE w_matkl(2) '%' INTO l_model.
*   check the model
  SELECT SINGLE paph2 INTO p_model
    FROM ztco_model_map
    WHERE modl2 LIKE l_model.

  IF p_model IS INITIAL.
    SELECT SINGLE maktx INTO w_matkl FROM makt
      WHERE matnr = p_matnr
      AND spras EQ sy-langu.
    IF sy-subrc EQ 0.

      CONCATENATE w_matkl(2) '%' INTO l_model.
*   check the model
      SELECT SINGLE paph2 INTO p_model
        FROM ztco_model_map
        WHERE modl2 LIKE l_model.

    ENDIF.
  ENDIF.

  IF p_model IS INITIAL.
*   break-point.
*********************************************************************
***** Please contact HISNA SAP Consultant ***************************
*********************************************************************
  ENDIF.

ENDFORM.                    " derive_model
*&---------------------------------------------------------------------*
*&      Form  call_bapi_inv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi_inv.
  DATA: lw_diff LIKE lt_invoice-dmbtr,
        lw_line TYPE i,
        l_webre LIKE ekpo-webre,
        l_text(30).

  CLEAR: headerdata, itemdata,itemdata[], return, return[],
         it_ztfi_claim_stl.

  READ TABLE lt_invoice INDEX 1.
  IF w_claim_amount < 0.
    headerdata-invoice_ind = 'X'.
    headerdata-gross_amount = - w_claim_amount.
  ELSE.
    headerdata-invoice_ind = ' '.
    headerdata-gross_amount = w_claim_amount.
  ENDIF.
  headerdata-doc_date = p_pdate.
  headerdata-pstng_date = p_pdate.
  headerdata-comp_code = p_bukrs.
  headerdata-currency_iso = w_waers.
  headerdata-diff_inv = lt_invoice-lifnr.
** Added by Furong on 11/15/07
  IF r_1 = 'X'.
    CONCATENATE 'Return stl:' p_monat '/' p_gjahr
           INTO headerdata-header_txt.
  ELSE.
    CONCATENATE 'Claim stl: ' p_monat '/' p_gjahr
           INTO headerdata-header_txt.
  ENDIF.
** end of addition
  LOOP AT lt_invoice.
    lw_line = lw_line + 1.
*    SELECT SINGLE meins webre INTO (itemdata-po_unit, l_webre)
*      FROM ekpo
*      WHERE ebeln = lt_invoice-ebeln
*        AND ebelp = lt_invoice-ebelp.
    itemdata-invoice_doc_item = lw_line.
    itemdata-po_number = lt_invoice-ebeln.
    itemdata-po_item = lt_invoice-ebelp.
*    itemdata-ref_doc = lt_invoice-belnr.
*    itemdata-ref_doc_it = lt_invoice-buzei.
    IF lt_invoice-webre = 'X'.
      itemdata-ref_doc = lt_invoice-lfbnr.
      itemdata-ref_doc_it = lt_invoice-lfpos.
      itemdata-ref_doc_year = p_gjahr.
    ENDIF.
    itemdata-tax_code = 'U0'.

    IF lt_invoice-claim_amount < 0.
      itemdata-item_amount = - lt_invoice-claim_amount.
      itemdata-quantity    = - lt_invoice-menge.
    ELSE.
      itemdata-item_amount = lt_invoice-claim_amount.
      itemdata-quantity    = lt_invoice-menge.
    ENDIF.
    itemdata-po_unit     = lt_invoice-meins.
    itemdata-po_unit_iso = itemdata-po_unit.
    IF r_1 = 'X'.
      itemdata-item_text = 'Return Posting'.
    ENDIF.
    APPEND itemdata.
    CLEAR itemdata.
  ENDLOOP.

  IF r_3 = 'X'.
    CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
     EXPORTING
       headerdata                = headerdata
*     ADDRESSDATA               =
    IMPORTING
      invoicedocnumber          = invoicedocnumber
      fiscalyear                = fiscalyear
     TABLES
       itemdata                  = itemdata
*     ACCOUNTINGDATA            =
*     GLACCOUNTDATA             =
*     MATERIALDATA              =
*     TAXDATA                   =
*     WITHTAXDATA               =
*     VENDORITEMSPLITDATA       =
       return                    = return.
  ELSE.
    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
    EXPORTING
      headerdata                = headerdata
*     ADDRESSDATA               =
   IMPORTING
     invoicedocnumber          = invoicedocnumber
     fiscalyear                = fiscalyear
    TABLES
      itemdata                  = itemdata
*     ACCOUNTINGDATA            =
*     GLACCOUNTDATA             =
*     MATERIALDATA              =
*     TAXDATA                   =
*     WITHTAXDATA               =
*     VENDORITEMSPLITDATA       =
      return                    = return.

  ENDIF.
  READ TABLE return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              msgid               = return-id
              msgnr               = return-number
              msgv1               = return-message_v1
              msgv2               = return-message_v2
              msgv3               = return-message_v3
              msgv4               = return-message_v4
         IMPORTING
              message_text_output = return-message.

    w_repid = sy-repid.

    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt1  = 'Error for parking of Credit Memo (Return)'
              txt2  = return-id
              txt3  = lt_invoice-lifnr
              txt4  = return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
    CLEAR: it_ztfi_claim_stl.
** changed by Furong on 06/28/07 for saving all posted data
*    READ TABLE LT_INVOICE INDEX 1.
    LOOP AT lt_invoice.
      it_ztfi_claim_stl-lifnr = lt_invoice-lifnr.
      it_ztfi_claim_stl-matnr = lt_invoice-matnr.
      it_ztfi_claim_stl-ebeln = lt_invoice-ebeln.
      it_ztfi_claim_stl-ebelp = lt_invoice-ebelp.
      it_ztfi_claim_stl-gjahr = p_gjahr.
      it_ztfi_claim_stl-lfmon = p_monat.
      it_ztfi_claim_stl-lfbnr = lt_invoice-lfbnr.
      it_ztfi_claim_stl-lfpos = lt_invoice-lfpos.
      it_ztfi_claim_stl-ctype = 'RETURN'.
      it_ztfi_claim_stl-meins = lt_invoice-meins.
      it_ztfi_claim_stl-menge = lt_invoice-menge.
      it_ztfi_claim_stl-claim_amount = lt_invoice-claim_amount.
      it_ztfi_claim_stl-dmbtr = lt_invoice-dmbtr.
      it_ztfi_claim_stl-budat =  p_pdate.
      it_ztfi_claim_stl-inv_no = invoicedocnumber.
      it_ztfi_claim_stl-ernam = sy-uname.
      it_ztfi_claim_stl-erdat = sy-datum.
      it_ztfi_claim_stl-erzet = sy-uzeit.
      APPEND it_ztfi_claim_stl.
    ENDLOOP.
    CONCATENATE invoicedocnumber ' was created' INTO l_text.
    MESSAGE s000 WITH l_text.
  ENDIF.
ENDFORM.                    " call_bapi_inv
*&---------------------------------------------------------------------*
*&      Form  call_bdc_inv_split
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_inv_split.
  DATA: lw_invoice LIKE TABLE OF lt_invoice WITH HEADER LINE.
  DATA: l_cn TYPE i,
        l_dmbtr LIKE lt_invoice-dmbtr,
        l_claim_amount LIKE lt_invoice-dmbtr.

  lw_invoice[] = lt_invoice[].
  CLEAR: lt_invoice, lt_invoice[].
  CLEAR: w_dmbtr, w_claim_amount, l_cn.
  LOOP AT lw_invoice.
    l_cn = l_cn + 1.
    lt_invoice = lw_invoice.
    APPEND lt_invoice.
    w_dmbtr = w_dmbtr + lt_invoice-dmbtr.
    w_claim_amount = w_claim_amount + lt_invoice-claim_amount.
    IF l_cn >= p_max_l.
      IF r_1 = 'X'.
        PERFORM call_bapi_inv.
      ELSE.
        PERFORM call_bdc_inv.
      ENDIF.
      CLEAR: lt_invoice, lt_invoice[].
      CLEAR: w_dmbtr, w_claim_amount, l_cn.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " call_bdc_inv_split

* BEGIN OF UD1K955730
*&---------------------------------------------------------------------*
*&      Form  call_bdc_inv_split2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_inv_split2.
  DATA: lw_invoice LIKE TABLE OF lt_invoice WITH HEADER LINE.
  DATA: l_cn TYPE i,
        l_dmbtr LIKE lt_invoice-dmbtr,
        l_claim_amount LIKE lt_invoice-dmbtr.

  lw_invoice[] = lt_invoice[].
  CLEAR: lt_invoice, lt_invoice[].
  CLEAR: w_dmbtr, w_claim_amount, l_cn.
  LOOP AT lw_invoice.
    l_cn = l_cn + 1.
    lt_invoice = lw_invoice.
    APPEND lt_invoice.
    w_dmbtr = w_dmbtr + lt_invoice-dmbtr.
    w_claim_amount = w_claim_amount + lt_invoice-claim_amount.
    IF l_cn >= p_max_l.
      IF r_1 = 'X'.
        PERFORM call_bapi_inv.
      ELSE.
        PERFORM call_bdc_inv2.
      ENDIF.
      CLEAR: lt_invoice, lt_invoice[].
      CLEAR: w_dmbtr, w_claim_amount, l_cn.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " call_bdc_inv_split2
* END OF UD1K955730
