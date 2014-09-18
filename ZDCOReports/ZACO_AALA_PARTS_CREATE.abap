************************************************************************
* Program Name      : ZACO_AALA_PARTS_CREATE
* Author            : Chris Li
* Creation Date     : 01/07/2004
* Specifications By :

* Pattern           : Report 1-1
* Development Request No : UD1K913
* Addl Documentation:
* Description       : Generate the AALA parts and price based on
*                     annual biz plan or std plan.
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 3/28/2005  Chris        UD1K915193  Material gross price (type PB00)
*                                     has been changed to the sum of
*                                     type PB00 and type ZTIR because
*                                     Tire and steel wheel module part
*                                     has been split into two part:
*                                     steel wheel and tire.
*
*
*
************************************************************************

REPORT zaco_aala_parts_cteate MESSAGE-ID zmco.

TABLES: marc,
        a018,
        konp,
        ztco_shopcost.
CONSTANTS: C_MIP_COS_ELE(10)        VALUE '0000540300',
           c_mark                   VALUE 'X',   "Marker
           c_aala(4)                VALUE 'ZP13', "AALA PRICE CONDITION
           c_gross(4)               VALUE 'PB00',"GROSS PRICE CONDITION
           c_ekorg LIKE ekko-ekorg  VALUE 'PU01'."Purchase Org.

DATA: p_delete_old.  " 'X': OLD DATA FOR THIS VERSION WILL BE DELETED.
DATA: p_record_type LIKE ztco_shopcost-record_type.
DATA: filestring    TYPE string.
DATA: s_error. "
DATA: g_message(50).

*----------------------------------------------------------------------*
*   Internal TABLES                                               *
*----------------------------------------------------------------------*
DATA: BEGIN OF it_mat OCCURS 0,
        matnr LIKE mara-matnr,
        werks LIKE marc-werks,
      END OF it_mat.
DATA: BEGIN OF it_altfsc OCCURS 0,
        matnr LIKE mara-matnr,
        altnr LIKE mara-matnr,
      END OF it_altfsc.
DATA: it_shopcost LIKE ztco_shopcost OCCURS 10 WITH HEADER LINE.
DATA: it_aala LIKE ztco_aala_source OCCURS 10 WITH HEADER LINE.
DATA: BEGIN OF it_info OCCURS 0,
         knumh  LIKE a018-knumh,  "CONDITION RECORD NO
         lifnr  LIKE a018-lifnr,  "VENDOR
         matnr  LIKE a018-matnr,  "MATERIAL
         datbi  LIKE a018-datbi,  "EFFECTIVE END DATE
         datab  LIKE a018-datab,  "EFFECTIVE START DATE
         kopos  LIKE konp-kopos,  "SEQUENTIAL NUMBER
         kschl  LIKE konp-kschl,  "CONDITION TYPE
         kbetr  LIKE konp-kbetr,  "RATE
         konwa  LIKE konp-konwa,  "CURRENCY KEY
         kpein  LIKE konp-kpein,  "PRICING UNIT
         kumza  LIKE konp-kumza,  "NUMERATOR FOR CONVERTING TO BASE UNIT
         kumne  LIKE konp-kumne,  "DENOMINATIOR
         meins  LIKE konp-meins,  "UNIT
         urzla  LIKE eina-urzla,  "COUNTRY
         ekgrp  LIKE eine-ekgrp,  "PURCHASING GROUP
      END OF it_info.
DATA: it_price LIKE it_info OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_output OCCURS 0,
        item(40),
        value(30),
      END OF it_output.
DATA: BEGIN OF it_colnames OCCURS 10,
       name(20),                       "Column names for download
      END OF it_colnames.
DATA: BEGIN OF it_dl OCCURS 0,
        kokrs(10)  , "TYPE    ZTCO_AALA_SOURCE-KOKRS,
        matnr(18)  , "TYPE    ZTCO_AALA_SOURCE-MATNR,
        gjahr(4)   , "type    ZTCO_AALA_SOURCE-GJAHR,
        versn(6)   , "TYPE    ZTCO_AALA_SOURCE-VERSN,
        lifnr(10)  , "TYPE    ZTCO_AALA_SOURCE-LIFNR,
        klvar(10)  , "TYPE    ZTCO_AALA_SOURCE-KLVAR,
        kstar(12)  , "TYPE    ZTCO_AALA_SOURCE-KSTAR,
*        KOSTL   ,TYPE    ZTCO_AALA_SOURCE-KOSTL,
        stprs(12)  , "TYPE    ZTCO_AALA_SOURCE-STPRS,
        peinh(5)  , "TYPE    ZTCO_AALA_SOURCE-PEINH,
        waers(10)  , "TYPE    ZTCO_AALA_SOURCE-WAERS,
        netpr(12)  , "TYPE    ZTCO_AALA_SOURCE-NETPR,
        aaprs(12)  , "TYPE    ZTCO_AALA_SOURCE-AAPRS,
        zwaers(10) , "TYPE    ZTCO_AALA_SOURCE-ZWAERS,
        urzla(10)  , "TYPE    ZTCO_AALA_SOURCE-URZLA,
        ekgrp(10)  , "TYPE    ZTCO_AALA_SOURCE-EKGRP,
        mtart(10)  , "TYPE    ZTCO_AALA_SOURCE-MTART,
        datbi(10)  , "TYPE    ZTCO_AALA_SOURCE-DATBI,
        datab(10)  , "TYPE    ZTCO_AALA_SOURCE-DATAB,
        zver_des(40),   "TYPE ZTCO_AALA_SOURCE-ZVER_DES,
      END OF it_dl.
*          steel wheel and tire moduel price has been split into two
*          parts: one price for tire(ztir) and one price for steel
*          wheel(pb00)
DATA: C_TIRE_PRICE_TYPE LIKE EKKO-EKORG VALUE 'ZTIR'. "TIRE PRICE

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
* Costing Type
SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(13)  text-031. "Business Plan
SELECTION-SCREEN POSITION 15.
PARAMETERS : p_bpl RADIOBUTTON GROUP ra01.
SELECTION-SCREEN COMMENT  25(13) text-032. "Standard Plan
SELECTION-SCREEN POSITION 39.
PARAMETERS : p_std RADIOBUTTON GROUP ra01.
SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN:SKIP.
* Posted Yr.
PARAMETERS : p_bdatj LIKE keko-bdatj MEMORY ID bdtj OBLIGATORY.
* periods
PARAMETERS: p_perab LIKE covja-perab MEMORY ID vpe
            MODIF ID per OBLIGATORY.

SELECTION-SCREEN END OF BLOCK bl2.
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
*version
PARAMETERS:  p_aalayr  LIKE ztco_aala_source-gjahr,
             p_versn   LIKE ztco_aala_source-versn OBLIGATORY,
             p_tvers   LIKE ztco_aala_source-zver_des.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-003.
*Effective date
PARAMETERS:  p_efect LIKE sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK bl3.
SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE text-004.
*Effective date
SELECT-OPTIONS:  p_matnr FOR marc-matnr.
SELECTION-SCREEN END OF BLOCK bl4.
SELECTION-SCREEN BEGIN OF BLOCK bl6 WITH FRAME TITLE text-005.
PARAMETERS:

  p_file  LIKE rlgrap-filename DEFAULT 'C:\       .TXT' OBLIGATORY,
  p_filety LIKE rlgrap-filetype DEFAULT 'DAT' NO-DISPLAY.
*  P_TCODE LIKE TSTC-TCODE DEFAULT 'MM01'.
SELECTION-SCREEN END   OF BLOCK bl6.

*----------------------------------------------------------------------*
*   End of Selection Condition                                         *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*   EVENTS                                                             *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_versn.
  PERFORM get_ver_des.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file 'O'.

AT SELECTION-SCREEN.
  PERFORM initial_input.
  PERFORM check_version.

START-OF-SELECTION.
  PERFORM get_mat_stdcost.
  PERFORM get_mat_purinfo.
  PERFORM update_table.

END-OF-SELECTION.
  PERFORM download_file.
  PERFORM write_output.

*&---------------------------------------------------------------------*
*&      Form  READ_FSC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_fsc.
* Read ALL FSC MATERIALS
  CLEAR : it_mat, it_mat[].
  SELECT matnr  INTO TABLE  it_mat
     FROM  macku
     WHERE mtart = 'FERT' AND
           matnr IN p_matnr.
  IF sy-subrc NE 0.
*    MESSAGE e000 WITH 'No FSC Material Exist!'.
    s_error = 'X'.
    g_message = 'No FSC Material Exist!'.
    EXIT.
  ENDIF.
  DELETE ADJACENT DUPLICATES FROM it_mat COMPARING matnr.
ENDFORM.                    " READ_FSC
*&---------------------------------------------------------------------*
*&      Form  GET_MAT_STDCOST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_mat_stdcost.
  PERFORM read_shopcost.
  PERFORM read_fsc.
  IF s_error = 'X'.
    EXIT.
  ENDIF.
  PERFORM check_fsc_in_shopcost.
  PERFORM save_part_cost.
ENDFORM.                    " GET_MAT_STDCOST
*&---------------------------------------------------------------------*
*&      Form  CHECK_FSC_IN_SHOPCOST
*&---------------------------------------------------------------------*
*       CHECK IF THE ALL FSC EXIST IN SHOPCOST TABLE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_fsc_in_shopcost.
  DATA: i_lines TYPE i.
  DATA: lines(5).
* CHECK IF FSC COSTING EXIST IN SELECTED PLAN/TIME
  CLEAR: it_altfsc, it_altfsc[].
  LOOP AT it_mat.
    READ TABLE it_shopcost WITH KEY fsc_matnr = it_mat-matnr.
    IF sy-subrc NE 0.
      it_altfsc-matnr = it_mat-matnr.
      APPEND it_altfsc.
      DELETE it_mat.
      CLEAR it_altfsc.
*      MESSAGE i000 WITH it_mat-matnr 'DOES NOT HAVE COSTING RESULT!'.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE it_mat LINES i_lines.
  lines = i_lines.
  IF i_lines EQ 0.
*  MESSAGE e000 WITH 'NO COSTING EXIST FOR SPECIFIED FSC IN SHOP COST.'.
    s_error = 'X'.
    g_message = 'NO COSTING EXIST FOR SPECIFIED FSC IN SHOP COST.'.
    EXIT.
  ELSE.
*  SAVE FSC CREATED
    it_output-item = 'Total FSC Created for'.
    it_output-value = lines.
    APPEND it_output.
  ENDIF.
ENDFORM.                    " CHECK_FSC_IN_SHOPCOST
*&---------------------------------------------------------------------*
*&      Form  READ_SHOPCOST
*&---------------------------------------------------------------------*
*       READING FSC SHOP COST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_shopcost.
  SELECT * INTO TABLE it_shopcost
   FROM ztco_shopcost
   WHERE fsc_matnr IN p_matnr AND
         bdatj = p_bdatj             AND
         poper = p_perab             AND
         record_type = p_record_type AND
         kokrs = 'H201'              AND
         typps = 'M'.
  IF sy-subrc NE 0.
*    MESSAGE e000 WITH 'NO COSTING EXIST FOR SPECIFIED FSC.'.
    s_error = 'X'.
    g_message = 'NO COSTING EXIST FOR SPECIFIED FSC.'.
    EXIT.
  ENDIF.
* DELETE DUPLICATES PARTS
  SORT it_shopcost BY llv_matnr.
  DELETE ADJACENT DUPLICATES FROM it_shopcost
    COMPARING llv_matnr.
* SEARCH FOR THE CHILD RAW MATERIALS OF MIP PARTS
  PERFORM SERACH_MIP_RAW.

  SORT IT_SHOPCOST BY LLV_MATNR.
  DELETE ADJACENT DUPLICATES FROM it_shopcost
    COMPARING llv_matnr.

* GET THE MATERIAL TYPE
  PERFORM get_material_type.
ENDFORM.                    " READ_SHOPCOST
*&---------------------------------------------------------------------*
*&      Form  INITIAL_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial_input.
  IF p_bpl EQ 'X'.
    p_record_type = 'B'.
  ELSE.
    p_record_type = 'S'.
  ENDIF.
ENDFORM.                    " INITIAL_INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_PART_COST
*&---------------------------------------------------------------------*
*       teXT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_part_cost.
  DATA: i_lines TYPE i.
  DATA: lines(5).
  CLEAR: it_aala, it_aala[].
  DATA: l_waers LIKE tka01-waers.

  IF s_error = 'X'.
    EXIT.
  ENDIF.
* reading the control area currency key
  READ TABLE it_shopcost INDEX 1.
  IF NOT it_shopcost-kokrs IS INITIAL.
    SELECT SINGLE waers INTO l_waers
      FROM tka01
      WHERE kokrs = it_shopcost-kokrs.
  ENDIF.
  CLEAR it_shopcost.
  LOOP AT it_shopcost.
    it_aala-matnr = it_shopcost-llv_matnr.
    it_aala-gjahr = sy-datum(4).
    it_aala-versn = p_versn.
    it_aala-zver_des = p_tvers.
    it_aala-klvar = it_shopcost-klvar.
    it_aala-kokrs = it_shopcost-kokrs.
    it_aala-kstar = it_shopcost-kstar.
    it_aala-kostl = it_shopcost-kostl.
    it_aala-stprs = it_shopcost-wertn / it_shopcost-menge.
    it_aala-peinh = 1.
    it_aala-waers = l_waers.
    it_aala-mtart = it_shopcost-mtart.
    APPEND it_aala.
  ENDLOOP.
* SAVE TOTAL PARTS
  DESCRIBE TABLE it_aala LINES i_lines.
  lines = i_lines.
  it_output-item = 'Total Parts Created'.
  it_output-value = lines.
  APPEND it_output.
ENDFORM.                    " SAVE_PART_COST
*&---------------------------------------------------------------------*
*&      Form  CHECK_VERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_version.
  DATA: w_ver LIKE ztco_aala_source-versn.
  DATA: w_verdes LIKE ztco_aala_source-zver_des.
  DATA: answer.
  DATA: l_text(30).
  SELECT versn zver_des INTO (w_ver, w_verdes)
    FROM ztco_aala_source
    WHERE versn = p_versn AND
          gjahr = p_aalayr.
    IF sy-subrc = 0.
      CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
       defaultoption = 'Y'
       diagnosetext1 = 'The version already exist, continue running'
       diagnosetext2 = 'this program will delete old data for this'
       diagnosetext3 = 'version and replace with new data.'
       textline1 = 'Are you sure you want to replace it?'
*       TEXTLINE2 = ' '
       titel = 'Please confirm'
      IMPORTING
       answer = answer.
      IF answer = 'J'.
*        PERFORM DELETE_VERSION.
        p_delete_old = 'X'.
        it_output-item = 'Version'.
        it_output-value = p_versn.
        APPEND it_output.
*    SAVE THE PLAN USED
        IF p_bpl = 'X'.
          CONCATENATE p_bdatj p_perab 'Business Plan'
            INTO l_text SEPARATED BY  '.'.
        ELSE.
          CONCATENATE p_bdatj p_perab 'Standard Plan'
            INTO l_text SEPARATED BY  '.'.
        ENDIF.
        it_output-item = 'Costing Plan Used'.
        it_output-value = l_text.
        APPEND it_output.
      ELSE.
        CLEAR: p_delete_old.
        MESSAGE e000 WITH 'PLEASE CHECK THE VERSION!'.

      ENDIF.
      EXIT.
    ENDIF.

  ENDSELECT.
ENDFORM.                    " CHECK_VERSION
*&---------------------------------------------------------------------*
*&      Form  DELETE_VERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_version.
  DELETE FROM ztco_aala_source WHERE versn = p_versn AND
                                     GJAHR = p_aalayr.
  COMMIT WORK AND WAIT.
ENDFORM.                    " DELETE_VERSION
*&---------------------------------------------------------------------*
*&      Form  GET_MAT_PURINFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_mat_purinfo.
  DATA: lt_info LIKE it_info OCCURS 0 WITH HEADER LINE.

  IF s_error = 'X'.
    EXIT.
  ENDIF.

* READING ALL MATERIAL INFO RECORD.
  SELECT lifnr matnr datbi datab knumh
    INTO CORRESPONDING FIELDS OF TABLE it_info
    FROM a018
    FOR ALL ENTRIES IN it_aala
    WHERE matnr = it_aala-matnr AND
          kschl = 'PB00'        AND
          ekorg =  c_ekorg      AND
          esokz =  '0'          AND
          kappl = 'M'    .

  IF sy-subrc NE 0.
    MESSAGE i000 WITH 'NO INFO RECORD EXISTS!'.
    EXIT.
  ENDIF.
* CHECK IF THE RECORD HAS BEEN DELETED
* AND GET THE ORIGIN COUNTRY/PURCHASE GROUP
  DATA: lw_matnr LIKE mara-matnr,
        lw_ekgrp LIKE eine-ekgrp,
        lw_urzla LIKE eina-urzla.
  LOOP AT it_info.
    SELECT SINGLE a~matnr b~ekgrp a~urzla
      INTO (lw_matnr,lw_ekgrp, lw_urzla)
      FROM eina AS a INNER JOIN eine AS b
        ON a~infnr = b~infnr
     WHERE a~matnr = it_info-matnr
       AND a~lifnr = it_info-lifnr
       AND a~loekz = ' '
       AND b~werks = ' '
       AND b~ekorg = c_ekorg
       AND b~loekz = ' '.
    IF sy-subrc NE 0.
      DELETE it_info.
    ELSE.
      it_info-urzla = lw_urzla.
      it_info-ekgrp = lw_ekgrp.
      MODIFY it_info.
      CLEAR it_info.
    ENDIF.
  ENDLOOP.
* READ THE CONDITION PRICE
  SELECT  knumh kopos kschl kbetr konwa kpein kumza kumne meins
    INTO CORRESPONDING FIELDS OF TABLE it_price
    FROM konp
    FOR ALL ENTRIES IN it_info
    WHERE knumh = it_info-knumh AND
          loevm_ko NE c_mark    AND
          ( kschl = c_aala OR
          kschl = c_gross  OR
          kschl = c_tire_price_type ).                    "UD1K915193
  IF sy-subrc NE 0.
    MESSAGE i000 WITH 'NO PRICES FOR INFO RECORD!'.
    EXIT.
  ENDIF.
*    change CTS no: UD1K915193
*    the gross price include type PB00 and ZTIR
*    COMBINE TYPE PB00 AND ZTIR PRICE TOGETHER
  DATA: WA_PRIC LIKE IT_PRICE.
  LOOP AT it_price.
     IF it_price-KSCHL = C_TIRE_PRICE_TYPE.
       CLEAR: WA_pric.
       READ TABLE it_price INTO WA_pric
            WITH KEY KNUMH = it_price-KNUMH
                     KSCHL = 'PB00'.
       WA_pric-KBETR = WA_pric-KBETR + it_price-KBETR.
       MODIFY TABLE it_price FROM WA_pric
            TRANSPORTING KBETR.
       DELETE it_price.
     ENDIF.
  ENDLOOP.

*    end of change ON 03/28/2005


  SORT it_info BY matnr lifnr datbi datab.
* CHECK THE INFO RECORD EFFICTIVE DATE
  PERFORM process_inforecord.
* SAVE THE PRICE OF INFO RECORDS
  PERFORM save_into_price.
* ATTACH THE TIME STAMP
  PERFORM get_time_stamp.
ENDFORM.                    " GET_MAT_PURINFO
*&---------------------------------------------------------------------*
*&      Form  PROCESS_INFORECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_inforecord.
  DATA: lt_info LIKE it_info OCCURS 0 WITH HEADER LINE.
  DATA: l_tabix LIKE sy-tabix.
  DATA: wa_price LIKE it_price.
  DATA: wa_info LIKE it_info.
  DATA: winer.
* COMBINE THE CONDITION AND PRICE
  LOOP AT it_price.
    READ TABLE it_info WITH KEY knumh = it_price-knumh.
    IF sy-subrc EQ 0.
      it_price-lifnr = it_info-lifnr.
      it_price-matnr = it_info-matnr.
      it_price-datbi = it_info-datbi.
      it_price-datab = it_info-datab.
      it_price-urzla = it_info-urzla.
      it_price-ekgrp = it_info-ekgrp.
      MODIFY it_price.
      CLEAR it_price.
    ENDIF.
  ENDLOOP.
  CLEAR it_info.
  REFRESH it_info.
*

* PULL UP THE GROSS PRICE RECORD AND PUT IT TO IT_INFO
  it_info[] = it_price[].
  DELETE it_info WHERE kschl NE 'PB00'.
*
  SORT it_price BY matnr lifnr datbi datab.
  SORT it_info BY matnr lifnr datbi datab.
* CHECK THE EFFECTIVE DATE FOR EACH RECORD.
  LOOP AT it_info.
    l_tabix = sy-tabix + 1.
    CLEAR wa_info.
    READ TABLE it_info INTO wa_info INDEX l_tabix.
    IF sy-subrc EQ 0.
*     IF ONLY ONE RECORD FOR ONE MATERIAL, KEEP IT
      IF it_info-matnr = wa_info-matnr.
        PERFORM compare_records USING it_info wa_info winer.
*       DELETE THE LOSER
        IF winer = '1'.
          DELETE it_info INDEX l_tabix.
          CLEAR winer.
        ELSEIF winer = '2'.
          DELETE it_info.
          CLEAR winer.
        ELSE.
          MESSAGE e000 WITH 'CHECK PROGRAM. MUST AND ONLY ONE WINNER!'.
        ENDIF.
      ELSE.
        CONTINUE.   "KEEP IT IF ONLY ONE RECORD
      ENDIF.
    ENDIF.
  ENDLOOP.
* COMBINE THE ZP13 TYPE PRICE (AALA PRICE)
  CLEAR: lt_info, lt_info[].
  LOOP AT it_info.

    READ TABLE it_price WITH KEY kschl = c_aala
                                 knumh = it_info-knumh
                                 datbi = it_info-datbi
                                 datab = it_info-datab
                                 lifnr = it_info-lifnr
                                 matnr = it_info-matnr.
*  APPEND THE AALA PRICE
    IF sy-subrc EQ 0.
      APPEND it_price TO lt_info.
    ENDIF.
  ENDLOOP.

  APPEND LINES OF lt_info TO it_info.

* CONVERT THE PRICE TO BASE UNIT PRICE

  LOOP AT it_info.
    IF it_info-kschl = c_gross.
      it_info-kbetr = it_info-kbetr * it_info-kumza / it_info-kumne.
      it_info-kumza = 1.
      it_info-kumza = 1.
*    ELSEIF IT_INFO-KSCHL = C_AALA.
    ENDIF.
    MODIFY it_info.
  ENDLOOP.
  CLEAR it_price.
  REFRESH it_price.
ENDFORM.                    " PROCESS_INFORECORD
*&---------------------------------------------------------------------*
*&      Form  COMPARE_RECORDS
*&---------------------------------------------------------------------*
*       COMPARE TWO INFORECORD AND FOUND THE WINNER
*----------------------------------------------------------------------*
*      -->P_IT_INFO  text
*      -->P_WA_INFO  text
*      -->P_WINNER  text
*----------------------------------------------------------------------*
FORM compare_records USING    p1_info LIKE it_info
                              p2_info LIKE it_info
                              p_winer.
*  CHECK EFFECTIVE DATE
*  KEEP THE CURRENT EFFECTIVE RECORD IF ONLY ONE
  IF ( p_efect GE p1_info-datab AND
       p_efect LE p1_info-datbi ) AND NOT
     ( p_efect GE p2_info-datab AND
       p_efect LE p2_info-datbi ).
    p_winer = '1'.
    EXIT.
  ELSEIF ( p_efect GE p2_info-datab AND
           p_efect LE p2_info-datbi ) AND NOT
         ( p_efect GE p1_info-datab AND
           p_efect LE p1_info-datbi ).
    p_winer = '2'.
    EXIT.
  ENDIF.
*  IF BOTH ARE EFFECTIVE, FIRST CHECK THE LATEST EFFECTIVE DATE
   IF    ( p_efect GE p1_info-datab AND
          p_efect LE p1_info-datbi ) AND
        ( p_efect GE p2_info-datab AND
          p_efect LE p2_info-datbi ).
      IF P1_INFO-DATAB GT P2_INFO-DATAB.
        P_WINER = '1'.
        EXIT.
      ELSEIF P2_INFO-DATAB GT P1_INFO-DATAB.
        P_WINER = '2'.
        EXIT.
      ENDIF.

   ENDIF.

*  IF BOTH ARE EFFECTIVE AND HAVE SAME EFFECTIVE BEGIN DATE
*  CHECK IF ONE IS US/CANADA, IF ONLY ONE, IT'S WINNER

  IF    ( p_efect GE p1_info-datab AND
          p_efect LE p1_info-datbi ) AND
        ( p_efect GE p2_info-datab AND
          p_efect LE p2_info-datbi ).

    IF ( p1_info-urzla = 'US' OR
         p1_info-urzla = 'CA' ) AND NOT
       ( p2_info-urzla = 'US' OR
         p2_info-urzla = 'CA' ) .
      p_winer = '1'.
      EXIT.
    ELSEIF ( p2_info-urzla = 'US' OR
             p2_info-urzla = 'CA' ) AND NOT
           ( p1_info-urzla = 'US' OR
             p1_info-urzla = 'CA' ) .
      p_winer = '2'.
      EXIT.
    ELSEIF ( p2_info-urzla = 'US' OR
             p2_info-urzla = 'CA' ) AND
           ( p1_info-urzla = 'US' OR
             p1_info-urzla = 'CA' ) .
*     BOTH ARE EFFECTIVE AND US/CA.
*     CHOOSE CHEAP ONE
      IF p1_info-kbetr GE p2_info-kbetr.
        p_winer = '2'.
        EXIT.
      ELSE.
        p_winer = '1'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.
*  BOTH ARE NOT EFFECTIVE
*  IF ONE IS FUTURE CHOOSE FUTURE PRICE
  IF p1_info-datab GT p_efect AND
     p2_info-datab LE p_efect.
    p_winer = '1'.
    EXIT.
  ELSEIF p2_info-datab GT p_efect AND
     p1_info-datab LE p_efect.
    p_winer = '2'.
    EXIT.
  ELSEIF p1_info-datab GT p_efect AND
     p2_info-datab GT p_efect.
*     BOTH ARE IN THE FUTURE CHOOSE CLOSEST FUTURE PRICE
    IF p1_info-datab GT p2_info-datab.
      p_winer = '2'.
      EXIT.
    ELSE.
      p_winer = '1'.
      EXIT.
    ENDIF.
  ELSEIF p1_info-datab LT p_efect AND
     p2_info-datab LT p_efect.
*     BOTH ARE IN THE PAST, CHOOSE CLOSEST DATE
    IF p1_info-datab LT p2_info-datab.
      p_winer = '2'.
      EXIT.
    ELSE.
      p_winer = '1'.
      EXIT.
    ENDIF.
  ENDIF.
* IF STILL NOT FOUND THE WINNER
* COMPARE THE PRICE, CHOOSE CHEAP PRICE.
  IF p1_info-kbetr GT p2_info-kbetr.
    p_winer = '2'.
    EXIT.
  ELSE.
    p_winer = '1'.
    EXIT.
  ENDIF.
ENDFORM.                    " COMPARE_RECORDS
*&---------------------------------------------------------------------*
*&      Form  SAVE_INTO_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        TABLE IT_AALA, IT_INFO
*  <--  p2        IT_AALA WITH THE INFO RECORD PRICES
*----------------------------------------------------------------------*
FORM save_into_price.
  DATA: i_inf TYPE i.
  DATA: i_aala TYPE i.
  DATA: lines(5).
*  IT_INFO SHOULD HAVE ONE OR TWO RECORDS FOR EACH MATERIAL
*  ONE FOR GROSS PRICE AND MAY HAVE ONE FOR AALA PRICE
  LOOP AT it_aala.
*    READING THE GROSS PRICE
    CLEAR it_info.
    READ TABLE it_info WITH KEY matnr = it_aala-matnr
                                kschl = c_gross.
    IF sy-subrc = 0.
      i_inf = i_inf + 1.
      it_aala-lifnr = it_info-lifnr.
      it_aala-datbi = it_info-datbi.
      it_aala-datab = it_info-datab.
      it_aala-netpr = it_info-kbetr.

      it_aala-zwaers = it_info-konwa.
      it_aala-urzla = it_info-urzla.
      it_aala-ekgrp = it_info-ekgrp.
    ELSE.
      it_aala-datbi = '99991231'.
      it_aala-datab = SY-DATUM.
    ENDIF..
*  READING THE AALA PRICE
    CLEAR: it_info.
    READ TABLE it_info WITH KEY matnr = it_aala-matnr
                                kschl = c_aala.
    IF sy-subrc EQ 0.
      i_aala = i_aala + 1.
      it_aala-aaprs = it_info-kbetr.
    ENDIF.
    MODIFY it_aala.
  ENDLOOP.
*  total parts that have info record.
  lines = i_inf.
  it_output-item = 'Total Parts That Have Inforecord'.
  it_output-value = lines.
  APPEND it_output.

* total AALA PRICE PARTS
  lines = i_aala.
  it_output-item = 'Total Parts that Have AALA Price'.
  it_output-value = lines.
  APPEND it_output.
ENDFORM.                    " SAVE_INTO_PRICE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
  DATA: L_TEXT(50).
  IF s_error = 'X'.
    EXIT.
  ENDIF.
* FOR DOWNLOAD DATA CONSISTANT REASON
* SET THE VALID DATE FOR THOSE DAT EMPTY RECORD
*  LOOP AT it_aala.
*    IF it_aala-datbi IS INITIAL.
*      it_aala-datbi = '99991231'.
*    ENDIF.
*    IF it_aala-datab IS INITIAL.
*      it_aala-datab = sy-datum.
*    ENDIF.

*  ENDLOOP.
  PERFORM delete_version.
  INSERT ztco_aala_source FROM TABLE it_aala.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    L_TEXT = 'UPDATE DATABASE PROCESS FAILED'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
  ENDIF.

ENDFORM.                    " UPDATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  DELETE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_table.

ENDFORM.                    " DELETE_TABLE
*&---------------------------------------------------------------------*
*&      Form  GET_MATERIAL_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material_type.
  DATA: BEGIN OF lt_type OCCURS 0,
         matnr LIKE mara-matnr,
         mtart LIKE mara-mtart,
        END OF lt_type.

  SELECT matnr mtart INTO TABLE lt_type
    FROM mara
    FOR ALL ENTRIES IN it_shopcost
    WHERE matnr = it_shopcost-llv_matnr.
  LOOP AT it_shopcost.
    READ TABLE lt_type WITH KEY matnr = it_shopcost-llv_matnr.
    IF sy-subrc = 0.
      it_shopcost-mtart = lt_type-mtart.
      MODIFY it_shopcost.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " GET_MATERIAL_TYPE
*&---------------------------------------------------------------------*
*&      Form  GET_TIME_STAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_time_stamp.
  LOOP AT it_aala.
    it_aala-erdat = sy-datum.
    it_aala-erzet = sy-uzeit.
    it_aala-ernam = sy-uname.
    MODIFY it_aala.
  ENDLOOP.
ENDFORM.                    " GET_TIME_STAMP
*&---------------------------------------------------------------------*
*&      Form  WRITE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_output.
  IF s_error = 'X'.
    WRITE: / g_message.
    EXIT.
  ENDIF.

  LOOP AT it_output.
    WRITE: /(40) it_output-item, 42(30) it_output-value.
  ENDLOOP.
ENDFORM.                    " WRITE_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_file.
  IF s_error = 'X'.
    EXIT.
  ENDIF.

  PERFORM make_colnames.
  PERFORM call_dl_function.
ENDFORM.                    " DOWNLOAD_FILE

*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM at_sel_screen_on_value_request USING def_path LIKE rlgrap-filename
                                          mode     TYPE c.

  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: fieldln TYPE i.
  FIELD-SYMBOLS: <tmp_sym>.

* Build Filter for Fileselektor

*  IF GLOBAL_FILEMASK_MASK IS INITIAL.
  tmp_mask = ',*.*,*.*.'.
*  ELSE.
*    TMP_MASK = ','.
*    WRITE GLOBAL_FILEMASK_TEXT TO TMP_MASK+1.
*    WRITE ',' TO TMP_MASK+21.
*    WRITE GLOBAL_FILEMASK_MASK TO TMP_MASK+22.
*    WRITE '.' TO TMP_MASK+42.
*    CONDENSE TMP_MASK NO-GAPS.
*  ENDIF.

*  IF NOT GLOBAL_FILEMASK_ALL IS INITIAL.
*    TMP_MASK = GLOBAL_FILEMASK_ALL.
*  ENDIF.
*
  fieldln = strlen( def_path ) - 1.
  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
    CLEAR <tmp_sym>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_filename     = p_file
            def_path         = def_path
*           MASK             = ',*.*,*.*.'
            mask             = tmp_mask
            mode             = mode
*           TITLE            = ' '
       IMPORTING
            filename         = tmp_filename
*         RC               =
       EXCEPTIONS
            inv_winsys       = 01
            no_batch         = 02
            selection_cancel = 03
            selection_error  = 04.

  IF sy-subrc = 0.
    p_file = tmp_filename.
  ELSE.
* IF SY-SUBRC = 01.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
  ENDIF.

ENDFORM.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  make_colnames
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_colnames.
* MAKE COLIMN HEADER
  it_colnames-name = 'CTRL AREA'.
  APPEND it_colnames.
  it_colnames-name = 'PART NUMBER'.
  APPEND it_colnames.
  it_colnames-name = 'YEAR'.
  APPEND it_colnames.
  it_colnames-name = 'VERSN'.
  APPEND it_colnames.
  it_colnames-name = 'VENDOR'.
  APPEND it_colnames.
  it_colnames-name = 'CST VARNT'.
  APPEND it_colnames.
  it_colnames-name = 'COST ELMT'.
  APPEND it_colnames.
  it_colnames-name = 'STD PRICE'.
  APPEND it_colnames.
  it_colnames-name = 'UNIT'.
  APPEND it_colnames.
  it_colnames-name = 'CURR KEY'.
  APPEND it_colnames.
  it_colnames-name = 'GROSS PRICE'.
  APPEND it_colnames.
  it_colnames-name = 'AALA PRICE'.
  APPEND it_colnames.
  it_colnames-name = 'CURR KEY'.
  APPEND it_colnames.
  it_colnames-name = 'CTRY KEY'.
  APPEND it_colnames.
  it_colnames-name = 'PURCH GRP'.
  APPEND it_colnames.
  it_colnames-name = 'MAT TYPE'.
  APPEND it_colnames.
  it_colnames-name = 'VALID-TO'.
  APPEND it_colnames.
  it_colnames-name = 'VALID-FROM'.
  APPEND it_colnames.


* MAKE DOWMLOAD DATA
  CLEAR: it_dl, it_dl[].
  LOOP AT it_aala.
    MOVE-CORRESPONDING it_aala TO it_dl.
    APPEND it_dl.
  ENDLOOP.
ENDFORM.                    " make_colnames
*&---------------------------------------------------------------------*
*&      Form  call_dl_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_dl_function.
  DATA:l_text(30).
  CALL FUNCTION 'WS_DOWNLOAD'
      EXPORTING
           filename                = p_file
           filetype                = 'DAT'
*           col_select              = 'X'
      TABLES
           data_tab                = it_dl
           fieldnames              = it_colnames
    EXCEPTIONS
         file_open_error         = 1
         file_write_error        = 2
         invalid_filesize        = 3
         invalid_table_width     = 4
         invalid_type            = 5
         no_batch                = 6
         unknown_error           = 7
*         GUI_REFUSE_FILETRANSFER = 8
         OTHERS                  = 9
       .
  IF sy-subrc <> 0.
    l_text = 'File Download Not Success'.
  ELSE.
    l_text = 'File Download Success '.
  ENDIF.
  it_output-item = 'File Dowload'.
  it_output-value = l_text.
  APPEND it_output.
ENDFORM.                    " call_dl_function
*&---------------------------------------------------------------------*
*&      Form  GET_VER_DES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ver_des.
  DATA: l_des LIKE ztco_aala_source-zver_des.
  p_aalayr = sy-datum(4).
  SELECT SINGLE zver_des INTO l_des
    FROM ztco_aala_source
    WHERE versn = p_versn AND
          gjahr = p_aalayr.
  IF sy-subrc EQ 0.
    LOOP AT SCREEN.
      IF screen-name = 'P_TVERS'.
        p_tvers = l_des.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_VER_DES
*&---------------------------------------------------------------------*
*&      Form  SERACH_MIP_RAW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form SERACH_MIP_RAW.

  DATA: LT_SHOPMIP LIKE IT_SHOPCOST OCCURS 0 WITH HEADER LINE.
  DATA: MT_SHOPMIP LIKE IT_SHOPCOST OCCURS 0 WITH HEADER LINE.
  DATA: I_LINES TYPE I.

  CLEAR: LT_SHOPMIP[].
  LOOP AT IT_SHOPCOST WHERE KSTAR = C_MIP_COS_ELE.

    APPEND IT_SHOPCOST TO LT_SHOPMIP.
    DELETE IT_SHOPCOST.

  ENDLOOP.

  DESCRIBE TABLE LT_SHOPMIP LINES I_LINES.
  IF I_LINES EQ 0.
    EXIT.
  ENDIF.

  SELECT * INTO TABLE MT_SHOPMIP
   FROM ZTCO_SHOPCOST
   FOR ALL ENTRIES IN LT_SHOPMIP
   WHERE FSC_MATNR = LT_SHOPMIP-LLV_MATNR AND
         bdatj = p_bdatj             AND
         poper = p_perab             AND
         record_type = p_record_type AND
         kokrs = 'H201'              AND
         typps = 'M'.
   IF SY-SUBRC NE 0.
     EXIT.    "NO MORE MATERIALS
   ENDIF.

   SORT MT_SHOPMIP BY LLV_MATNR.
   DELETE ADJACENT DUPLICATES FROM MT_SHOPMIP
      COMPARING LLV_MATNR.

*  POST THESE MATERIALS TO IT_SHOPCOST.
   APPEND LINES OF MT_SHOPMIP TO IT_SHOPCOST.
   PERFORM SERACH_MIP_RAW.

endform.                    " SERACH_MIP_RAW
