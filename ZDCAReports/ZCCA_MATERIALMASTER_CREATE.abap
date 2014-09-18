************************************************************************
* Program Name      : ZCCA_MATERIALMASTER_CREATE
* Author            : Won Seob Kim
* Creation Date     : 2003.12.22.
* Specifications By :
* Pattern           : 2.2 Call BAPI Function + 2.3 Call Transaction
* Development Request No :
* Addl Documentation:
* Description       : Material Master Upload
*   - Using BAPI Function : BAPI_EQUI_CREATE
*    -> If the BAPI runs successfully, table RETURN contains
*       no messages of type 'E'.
*   - <Caution>
*     This BAPI does not change the database. To change the database,
*     call BAPI BapiService.TransactionCommit afterwards.
*      -> 'BAPI_TRANSACTION_COMMIT' or 'BAPI_TRANSACTION_ROLLBACK'
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


*include ZCCA_MATERIALMASTER_CREATE_top.
REPORT ZCCA_MATERIALMASTER_CREATE   NO STANDARD PAGE HEADING
                                  MESSAGE-ID zmqm LINE-SIZE 200.
TYPE-POOLS: vrm.

*-- Tables :
TABLES : mara,marc. "/Equipment master

**-- Bapi return message table : LOG TABLE
DATA: it_return	  LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

DATA   BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA   END OF it_msg.

* BDC Tables
DATA : BEGIN OF bdc_tab OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF bdc_tab.

****For Error Message
DATA: wa_return LIKE bapiret2 .

* EXCEL UPLOAD
DATA: BEGIN OF IT_EXCL OCCURS 0,
*       Material master initial screen
*       ORGANIZATIONAL LEVELS
        MATNR(18), " TYPE RMMG1-MATNR,  "Material	
        WERKS(04), " TYPE RMMG1-WERKS,  "Plant	
        LGORT(04), " TYPE RMMG1-LGORT,  "Storage Location	
        LGNUM(03), " TYPE RMMG1-LGNUM,  "Warehouse	
        LGTYP(03), " TYPE RMMG1-LGTYP,  "Storage Type	
        MTART(04), " TYPE RMMG1-MTART,  "Material type	
        MBRSH(01), " TYPE RMMG1-MBRSH,  "Industry sector	
        NORMT(18), " TYPE MARA-NORMT,   "Vendor	
*       Basic Data 1
        MAKTX(40), " TYPE MAKT-MAKTX,  "Material Description	
        MEINS(03), " TYPE MARA-MEINS,  "Base unit of measure
        MATKL(09), " TYPE MARA-MATKL,  "Material Group	
        EXTWG(18), " TYPE MARA-EXTWG,  "Ext. material group
        MSTAE(2),                      "Xplant matl status
        SPART1(2),                     "Division
        MTPOS_MARA(04), " TYPE MARA-MTPOS_MARA,  "GenItem CatGroup	
        BRGEW(20),                     "Gross weight
        GEWEI(03),                     "Weight unit
        NTGEW(20),                     "Net weight
        VOLUM(20),                     "Volume
        VOLEH(03),                     "Volume unit
        GROES(32),                     "Size/dimensions
*REFERENCE MATERIAL FOR BACK FLUSH
        MATNR1(18),
*       Additional data
        UMREN(05),                     "
        MEINH(03),                     "Unit
        UMREZ(05),                     "
*       Basic Data 2
        FERTH(18), " TYPE MARA-FERTH,  "Production Memo	
        PROFL(03), " TYPE MARA-PROFL,  "KD/LP/MIP	
        KZKFG(01), " TYPE MARA-KZKFG,  "VMaterial is configurable	
*       MRP 1
        MEINS1(03),                    "Base unit of measure		
        DISMM(02), " TYPE MARC-DISMM,  "MRP type	
        DISGR(04), " TYPE MARC-DISGR,  "MRP Group	
        MINBE(20), " TYPE MARC-MINBE,  "Reorder Point	
        DISPO(03), " TYPE MARC-DISPO,  "MRP controller	
        DISLS(02), " TYPE MARC-DISLS,  "Lot size	"
        BSTMI(20), " TYPE MARC-BSTMI,  "Minimum lot size"	
        BSTMA(20), " TYPE MARC-BSTMA,  "Maximum lot size"	
        BSTRF(20), " TYPE MARC-BSTRF,  "Rounding Value	"
        AUSSS(20), " TYPE MARC-AUSSS,  "Assembly scrap (%)"
*       MRP 2								
        BESKZ(01), " TYPE MARC-BESKZ,  "Procurement type	
        LGPRO(04), " TYPE MARC-LGPRO,  "Issue stor.location	
        LGFSB(04), " TYPE MARC-LGFSB,  "Storage loc. for EP	
        SOBSL(02), " TYPE MARC-SOBSL,  "Special procurement 	
        VSPVB(10), " TYPE MARC-VSPVB,  "Default Supply Area	
        RGEKZ(01), " TYPE MARC-RGEKZ,  "Backflush	
        SCHGT(01), " TYPE MARC-SCHGT,  "Bulk Material	
        PLIFZ(10), " TYPE MARC-PLIFZ,  "Plnd delivery time 	
        WEBAZ(10), " TYPE MARC-WEBAZ,  "GR Processing Time	
        FHORI(03), " TYPE MARC-FHORI,  "SchedMargin key	
        EISBE(20), " TYPE MARC-EISBE,  "Safety stock	
        RWPRO(03), " TYPE MARC-RWPRO,  "Coverage profile	
*       MRP 3								
        MTVFP(02), " TYPE MARC-MTVFP,  "Availability check	
        STRGR(02), " TYPE MARC-STRGR,  "Strategy group	
        VRMOD(01), " TYPE MARC-VRMOD,  "Consumption mode	
        VINT1(03), " TYPE MARC-VINT1,  "Bwd consumption per.	
        VINT2(03), " TYPE MARC-VINT2,  "Fwd consumption per.	
        STDPD(18), " TYPE MARC-STDPD,  "Configurable material 	
*       MRP 4								
        ALTSL(01), " TYPE MARC-ALTSL,  "Selection method	
        SBDKZ(01), " TYPE MARC-SBDKZ,  "individual / Collective	
        VERKZ(01), " TYPE MARC-VERKZ,  "Version indicator	
        SAUFT(01), " TYPE MARC-SAUFT,  "Repetitive mfg	
        SFEPR(04), " TYPE MARC-SFEPR,  "REM profile	
        PROFIL(04),                    "Backfl.Profile
*       PRODUCTION VERSION OVERVIEW
        VERID(04), " TYPE MKAL-VERID,  "Pro.Version
        TEXT1(40),                     "text on the production version
        PLNTY(01),                     "Detailed planning
        PLTYG(01), " TYPE MKAL-PLTYG,  "Rate-based planning	
        PLNNG(08), " TYPE MKAL-PLNNG,  "Group	"
        ALNAG(02), " TYPE MKAL-ALNAG,  "Group Counter"	
        STLAL(02), " TYPE MKAL-STLAL,  "Alternative BOM	"
        STLAN(01), " TYPE MKAL-STLAN,  "BOM usage"	"
        SERKZ(01), " TYPE MKAL-SERKZ,  "REM Allowed"	"
        MDV01(08), " TYPE MKAL-MDV01,  "Production  line"	
        ELPRO(04), " TYPE MKAL-ELPRO,  "Issue stor. location
        ALORT(04), " TYPE MKAL-ALORT,  "Receiv. locationALORT(04),
*       Workscheduling	
        FEVOR(03), " TYPE MARC-FEVOR,  "Production scheduler"	
        UNETO(05), " TYPE MARC-UNETO,  "Underdely tol.
        UEETO(05),                     "Overdely tol.
*       Sales : Sales Org 1				
        VKORG(04), " TYPE RMMG1-VKORG,  "Sales org.	
        VTWEG(02), " TYPE RMMG1-VTWEG,  "Distr. Channel	
        SPART(02), " TYPE MARA-SPART,   "Division	
        DWERK(04), " TYPE MVKE-DWERK,   "Delivering plant	
        TAXKM(01), " TYPE MG03STEUER-TAXKM,  "Tax classfication	
*       Sales : Sales Org 2		
        VERSG(01), " TYPE MVKE-VERSG,  "Matl statistics grp	
        KTGRM(02), " TYPE MVKE-KTGRM,  "Acct assignment group 	
        MTPOS(04), " TYPE MVKE-MTPOS,  "Item category group	
*       Sales : General/Plant data			
        SERKS(04), " TYPE RMMG1-WERKS, " SALES Plant	
        STVFP(02), " TYPE MARC-MTVFP,  "SALES Availability check	
        TRAGR(04), " TYPE MARA-TRAGR,  "Trans. Grp	
        LADGR(04), " TYPE MARC-LADGR,  "LoadingGrp	"
*       Plant/Storage 1
*        LGPBE(10),                     "Storage bin			
        RAUBE(02), " TYPE MARA-RAUBE,  "Storage conditions(SHOP)"	"
        TEMPB(02), " TYPE MARA-TEMPB,  "Backflush Cycle)"
	   ABCIN(01), " TYPE MARC-ABCIN,   "CC phys. inv. ind.	
        CCFIX(01), " TYPE MARC-CCFIX,   "CC Fixed	
*       Plant/Storage 2
        XMCNG(01), " TYPE MARC-XMCNG,  "Negative stocks in plant	
*       Quality management	
        TEXT01(18), " TYPE C,  "Inspection type	
        TEXT02(18), " TYPE C,  "Active	
*       Accounting				
        BKLAS(04), " TYPE MBEW-BKLAS,  "Valuation class	
        VPRSV_1(01), " TYPE CKMMAT_DISPLAY-VPRSV_1,  "Price control	
        VERPR(18), " TYPE MBEW-VERPR,  "Mov. avg. price	
        STPRS_1(18), " TYPE CKMMAT_DISPLAY-STPRS_1,  "Standard price	
        PEING_1(06), " TYPE CKMMAT_DISPLAY-PEINH_1,  "Price unit	
*       Costing				
        ZPLP1(18), " TYPE MBEW-ZPLP1,  "Planned Price1	
        ZPLD1(08), " TYPE MBEW-ZPLD1,  "Planned price date 1
        ZPLP3(18), " TYPE MBEW-ZPLP1,  "Planned Price3	
        ZPLD3(08), " TYPE MBEW-ZPLD1,  "Planned price date 3
        EKALR(01), " TYPE MBEW-EKALR,  "with qty structure	
        LOSGR(18), " TYPE MARC-LOSGR,  "costing lot size	
        STPRS(18), " TYPE MBEW-STPRS,  "standard price	
*       Warehouse 1									
        TEXT03(18), " TYPE C,  "WM unit	
        TEXT04(18), " TYPE C,  "Unit of Issue	
        TEXT05(18), " TYPE C,  "Gross Weight	
        LTKZA(03), " TYPE C,  "Stock Removal 	
        LTKZE(03), " TYPE C,  "Stock Placement 	
        TEXT08(18), " TYPE C,  "Storage Section	
        TEXT09(18), " TYPE C,  "2 step Picking	
        TEXT10(18), " TYPE C,  "Allow addition to stock	
        TEXT11(18), " TYPE C,  "Gross Weight	
        TEXT12(18), " TYPE C,  "Capacity Usage	
*       Warehouse 2									
        TEXT13(18), " TYPE C,  "LE quantity 1	
        TEXT14(18), " TYPE C,  "LE quantity 2	
        TEXT15(18), " TYPE C,  "LE quantity 3	
        LGPLA(10), " TYPE MLGT-LGPLA,  "Storage bin	
        TEXT16(18), " TYPE C,  "Maximum bin quantity	
        LPMIN(20), " TYPE MLGT-LPMIN,  "Minimum bin quantity	
        RDMNG(20), " TYPE MLGT-RDMNG,  "Rounding qty	
        TEXT17(18), " TYPE C,  "Control quantity	
        TEXT18(18), " TYPE C,  "Replenishment qty	
        TEXT19(18), " TYPE C,  "Un 1	
        TEXT20(18), " TYPE C,  "Un 2	
        TEXT21(18), " TYPE C,  "Un 3	
        TEXT22(18), " TYPE C,  "SUT 1	
        TEXT23(18), " TYPE C,  "SUT 2	
        TEXT24(18), " TYPE C,  "SUT 3	
*       Classification			
        TEXT25(18), " TYPE C,  "Class	
        TEXT26(18), " TYPE C,  "Description	
        TEXT27(18), " TYPE C,  "Std. Class	
        TEXT28(18), " TYPE C,  "Classification status	
*       Purchasing									
        TEXT29(18), " TYPE C,  "Base unit of measure	
        TEXT30(18), " TYPE C,  "Order unit	
        TEXT31(18), " TYPE C,  "Var. OUn	
        EKGRP(03), " TYPE MARC-EKGRP,  "Purchasing group	
        TEXT32(18), " TYPE C,  "Material group	
        MMSTA(02),  " TYPE C,  "Plant-sp.matl status	
        KAUTB(01), " TYPE MARC-KAUTB,  "Autom.PO	
        TEXT34(18), " TYPE C,  "Batch management	
        TEXT35(18), " TYPE C,  "Critical part	
        TEXT36(18), " TYPE C,  "Quota arr. usage	
        KORDB(01), " TYPE MARC-KORDB,  "Source list	
        FABKZ(01), " TYPE MARC-FABKZ,  "JIT sched. indicator	
*       Foreign Trade/Import data	
        TEXT37(18), " TYPE C,  "Comm./imp.code no.	
        TEXT38(18), " TYPE C,  "Country of origin
        MSGTY TYPE SY-MSGTY,
        MESSG LIKE CFGNL-MSGLIN,
      END   OF IT_EXCL.

DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.
DATA: BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESS.
  DATA: BEGIN OF WA_OPT OCCURS 0.
          INCLUDE STRUCTURE CTU_PARAMS.
  DATA: END OF WA_OPT.

*----------------------------------------------------------------------

* DATA
*----------------------------------------------------------------------

DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I.

*** Uploading Data Fields
DATA: BEGIN OF it_equi OCCURS 0,
       eqtyp  TYPE eqtyp, "Equipment category
       equnr  TYPE equnr, "/Equipment No.=> Blank: Internal
       shtxt  TYPE ktx01, "/Description of technical object
       eqart  TYPE eqart, "/Type of Technical Object
       brgew(20),"  TYPE obj_weight, "/Weight of object
       gewei  TYPE weight_unit, "/Unit of weight
       groes  TYPE gross, "/Size/dimension
       invnr  TYPE invnr, "/Inventory number
       inbdt  TYPE ilom_datab, "/Start-up Date of the Technical Object
       answt(20),  "TYPE answt, "/Acquisition value
       waers  TYPE waers, "/Currency Key
       ansdt  TYPE andti, "/Acquisition date
       herst  TYPE herst, "/Manufacturer of asset
       herld  TYPE herld, "/Country of manufacture
       typbz  TYPE typbz, "/Manufacturer model number
       baujj  TYPE baujj, "/Year of construction
       baumm  TYPE baumm, "/Month of construction
       serge  TYPE serge, "/Manufacturer serial number
       swerk  TYPE swerk, "/Maintenance plant
       arbpl  TYPE arbpl, "/Work Center
       abckz  TYPE abckz, "/ABC indicator for technical object
       bukrs  TYPE bukrs, "/Company Code
       anlnr  TYPE anln1, "/Main asset number
       anlun  TYPE anln2, "/Asset sub-number
       kostl  TYPE kostl, "/Cost Center
       gewrk  TYPE gewrk, "/PP work center

*-- Maintenace Address window data for BDC : Start
       title_medi TYPE ad_titletx,  "/Text Title
       name1      TYPE ad_name1,  "/Name of address mainternace window
       street     TYPE ad_street, "/Street
*       HOUSE_NUM1 type AD_HSNM1,  "/House No.
       post_code1 TYPE ad_pstcd1, "/Postal code
       city1      TYPE ad_city1,  "/City
       country    TYPE land1,     "/Country
       region     TYPE regio,     "/Region
       po_box     TYPE ad_pobx,   "/PO Box
       post_code2 TYPE ad_pstcd2, "/Postab code
       post_code3 TYPE ad_pstcd3, "/Company postal code
       langu      TYPE spras,     "/Language
       tel_number TYPE ad_tlnmbr1, "/Telephone
       tel_extens TYPE ad_tlxtns1, "/First telephone no.: extension
       fax_number TYPE ad_fxnmbr1, "/First fax no.: dialling
"code+number
       fax_extens TYPE ad_fxxtns1, "/First fax no.: extension
       smtp_addr  TYPE ad_smtpadr, "/Internet mail (SMTP) address
*-- Maintenace Address window data for BDC : End

*-- Class Overview data for BDC : Start
       class      TYPE klasse_d,  "/Class
       atnam      TYPE atnam,     "/Characteristic Name
       atwrt      TYPE atwrt,     "/Characteristic value
*-- Class Overview data for BDC : End
*-- Partners data for BDC : Start
       parvw_ab   TYPE parvw,   "/Department resp. Partner Function
       parnr_ab   TYPE parnr,   "/Department resp.
       parvw_vw   TYPE parvw,   "/Person respons. Partner Function
       parnr_vw   TYPE parnr,   "/Person respons.
*-- Partners data for BDC : End
      END OF it_equi.


*//Data(Global Fileds) ;(WA_)  == Local? ?? ; (LO_)
*                 Flag ;(FL_), Counter;(CT_), Temp;(TMP_)
DATA : wa_repid LIKE syst-repid,
       wa_dynnr LIKE syst-dynnr.

*/ Result variables
DATA : wa_success_cnt TYPE i,
       wa_failed_cnt  TYPE i,
       wa_total_entr  TYPE i.

**-- Constants
CONSTANTS : c_mark  TYPE c VALUE 'X'.

**-- List box variables
DATA: wa_name  TYPE vrm_id,
      it_list  TYPE vrm_values,
      wa_value LIKE LINE OF it_list.
DATA : p_number TYPE equnr.
DATA   BEGIN OF lt_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA   END OF lt_msg.
include ZCCA_MM_CREATE_para.


AT SELECTION-SCREEN OUTPUT.
 PERFORM SCREEN_OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM get_select_excel_file  USING p_file.



START-OF-SELECTION.

  PERFORM f_excel_upload  TABLES   it_excl
                          USING    p_file.
  IF it_excl[] IS INITIAL.
    MESSAGE e000(zmqm)
            WITH 'Entries not founded. Please check File'(e15).
    EXIT.
  ENDIF.

END-OF-SELECTION.

  PERFORM EXECUTE_BAPI_FUNC. "/execute BAPI Function

  PERFORM bdc_process.

  PERFORM write_log_message.

TOP-OF-PAGE.

  PERFORM write_header.

END-OF-PAGE.

*&------------------------------------------------------------------*
*&      Form  get_select_Excel_file
*&------------------------------------------------------------------*
FORM get_select_excel_file   USING pw_file.
  wa_repid = sy-repid.
  wa_dynnr = sy-dynnr.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
       EXPORTING
            program_name  = wa_repid
            dynpro_number = wa_dynnr
            field_name    = ' '
            static        = ' '
            mask          = ' '
       CHANGING
            file_name     = pw_file
       EXCEPTIONS
            mask_too_long = 1
            OTHERS        = 2.
  IF sy-subrc < 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " get_select_Excel_file
*&------------------------------------------------------------------*
*&      Form  EXECUTE_BAPI_FUNC
*&------------------------------------------------------------------*
FORM execute_bapi_func.

*** For Cerate Functional Location BAPI
  DATA: lw_external_number	LIKE	bapi_itob_parms-equipment,
        lw_data_general	        LIKE	bapi_itob,
        lw_data_specific	LIKE	bapi_itob_eq_only,
        lw_valid_date	        LIKE	bapi_itob_parms-inst_date,
        lw_data_install	        LIKE	bapi_itob_eq_install,
        lw_return               LIKE    bapiret2.

  DATA : lw_subrc LIKE sy-subrc.
  DATA : lw_return2 LIKE bapiret2.

  DATA : lw_equi_index LIKE sy-tabix,
         lw_prog_text(132) TYPE c.

  DESCRIBE TABLE it_excl LINES wa_total_entr."/Total Equipment

  LOOP AT it_excl.
*    lw_equi_index = sy-tabix.
*
*    CLEAR : lw_external_number,  lw_data_general,  lw_data_specific,
*            lw_valid_date,   lw_data_install,   lw_return.
**///-- Fill Interface parameter for BAPI Function
*
***** Number of Equipment to be Created (Initial => Internal Assignment)
*    MOVE :
*     it_equi-eqtyp TO lw_data_specific-equicatgry, "/Equipment category
*     it_equi-eqart TO lw_data_general-objecttype,  "/Type of Tech
*Object
*     it_equi-invnr TO lw_data_general-inventory, "/Inventory number
*     it_equi-groes TO lw_data_general-obj_size, "/Size/dimension
*     it_equi-brgew TO lw_data_general-obj_weight, "/Weight of object
*     it_equi-gewei TO lw_data_general-unit_of_wt, "/Unit of weight
*     it_equi-ansdt TO lw_data_general-acqdate, "/Acquisition date
*     it_equi-answt TO lw_data_general-acquisval, "/Acquisition value
*     it_equi-waers TO lw_data_general-currency, "/Currency Key
*     it_equi-herst TO lw_data_general-manfacture, "/Man. of asset
*     it_equi-herld TO lw_data_general-mancountry, "/Country of man.
*     it_equi-serge TO lw_data_general-manserno, "/Man serial number
*     it_equi-typbz TO lw_data_general-manmodel, "/Man. model number
*     it_equi-baujj TO lw_data_general-constyear, "/Year of construction
*     it_equi-baumm TO lw_data_general-constmonth, "/Month of const.
*     it_equi-inbdt TO lw_data_general-start_from. "/Start-up Date of
*the
*    "  Technical Object
**    MOVE :
**     IT_EQUI-GEWRK TO LW_DATA_GENERAL-PP_WKCTR "/Obj.ID of PP W.C
**     IT_EQUI-ARBPL TO LW_DATA_GENERAL-WORK_CTR "/Object ID of the W.C
*    PERFORM get_workcenter_objid   USING : it_equi-arbpl
*                                           lw_data_general-work_ctr,
*                                           it_equi-gewrk
*                                           lw_data_general-pp_wkctr.
*
*
*    MOVE :
*     it_equi-shtxt TO lw_data_general-descript, "/Descript of tech. obj
.
*     it_equi-abckz TO lw_data_general-abcindic, "/ABC indicator
*     it_equi-swerk TO lw_data_general-maintplant. "/Maintenance plant
*
**    MOVE :
**    IT_EQUI-KOSTL TO LW_DATA_GENERAL-COSTCENTER.  "/Cost Center
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*         EXPORTING
*              input  = it_equi-kostl
*         IMPORTING
*              output = lw_data_general-costcenter.
*
*
*
*    MOVE :
*      it_equi-bukrs TO lw_data_general-comp_code, "/Company Code
*      it_equi-anlnr TO lw_data_general-asset_no, "/Main asset number
*      it_equi-anlun TO lw_data_general-sub_number, "/Asset sub-number
*
*      sy-datum TO lw_data_general-read_crdat,
*      sy-uname TO lw_data_general-read_crnam.
*
*
*
**-- Execute BAPI Function.
*    CALL FUNCTION 'BAPI_EQUI_CREATE'
*         EXPORTING
*              external_number   = lw_external_number
*              data_general      = lw_data_general
*              data_specific     = lw_data_specific
*              valid_date        = it_equi-inbdt
*              data_install      = lw_data_install
*         IMPORTING
*              equipment         = lw_external_number
*              data_general_exp  = lw_data_general
*              data_specific_exp = lw_data_specific
*              return            = lw_return.
*
*
**-/// After BAPI execution////
*    CLEAR it_return.
***      insert division message for each Class Upload
*    CONCATENATE it_equi-shtxt
*                ':'
*                   INTO  it_return-message SEPARATED BY space.
*
*
**/--    Error Check and Log Message Backup.
**   -> If the BAPI runs successfully, table RETURN contains no messages
**      of type 'E'.
*
*    IF sy-subrc NE 0.
*      PERFORM bapi_service_rollback.
*      MESSAGE e000(zmqm) WITH 'Error founded in BAPI Processing. '(e02)
.
*      CONCATENATE it_return-message
*                  '=>'
*                  text-e02
*      INTO  it_return-message SEPARATED BY space.
*      wa_failed_cnt = wa_failed_cnt + 1.
*    ENDIF.
*
*
*    IF lw_return-type  = 'E' OR lw_return-type  = 'A'.
*      PERFORM bapi_service_rollback.
*      CONCATENATE it_return-message
*                   '=> Failed'
*                     INTO  it_return-message SEPARATED BY space.
*      wa_failed_cnt = wa_failed_cnt + 1.
*    ELSE.
*
**--- Maintenance Address and Class /Characteristic view
*      PERFORM bapi_service_commit.
*
*      CONCATENATE  lw_external_number
*                   '-'
*                   it_equi-shtxt
*                   INTO lw_prog_text  SEPARATED BY space.
*
*      PERFORM display_progress_bar  USING  lw_prog_text
*                                           lw_equi_index
*                                           wa_total_entr.
*
*      CLEAR : lw_subrc, lw_return2.
*      MOVE : lw_external_number TO it_equi-equnr.
*
**      PERFORM ADD_DATA_EQUIP_PARVW_MA  USING LW_EXTERNAL_NUMBER
**                                             IT_EQUI
**                                             LW_SUBRC
**                                             LW_RETURN2.
*
*      IF lw_subrc IS INITIAL.
**-----------------------------------------------------------
*        CONCATENATE it_return-message
*                     '=> Success '
*                     lw_external_number
*                       INTO  it_return-message SEPARATED BY space.
*        wa_success_cnt = wa_success_cnt + 1.
*
*      ELSE.
*
*        PERFORM bapi_service_rollback.
*        CONCATENATE it_return-message
*             '=> Failed.'
*                       INTO  it_return-message SEPARATED BY space.
*        wa_failed_cnt = wa_failed_cnt + 1.
*
*      ENDIF.
*    ENDIF.
*
*    APPEND it_return.
*    APPEND lw_return TO it_return.
*    APPEND lw_return2 TO it_return.
*    APPEND INITIAL LINE TO  it_return.

  ENDLOOP.

ENDFORM.                    " EXECUTE_BAPI_FUNC
*&------------------------------------------------------------------*
*&      Form  F_EXCEL_UPLOAD
*&------------------------------------------------------------------*
FORM f_excel_upload  TABLES   p_table
                      USING   p_filename  LIKE rlgrap-filename.



  DATA : lt_itab TYPE alsmex_tabline OCCURS 0 WITH HEADER LINE.
  DATA : lw_index LIKE sy-tabix.
  DATA : lw_start_col TYPE i VALUE '1',
         lw_start_row TYPE i VALUE '1',
         lw_end_col   TYPE i VALUE '256',
         lw_end_row   TYPE i VALUE '65536'.
  FIELD-SYMBOLS : <lw_fs>.

  DATA : lw_field_type.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            filename                = p_filename
            i_begin_col             = lw_start_col
            i_begin_row             = lw_start_row
            i_end_col               = lw_end_col
            i_end_row               = lw_end_row
       TABLES
            intern                  = lt_itab
       EXCEPTIONS
            inconsistent_parameters = 1
            upload_ole              = 2
            OTHERS                  = 3.


  IF sy-subrc NE 0.
    MESSAGE e000(zmqm) WITH 'File Upload Failed !'(e10).
    STOP.
  ENDIF.


  CHECK NOT lt_itab[] IS INITIAL.

*-- Delete Header line: row from 1 to 2
  DELETE lt_itab WHERE row LE 2.


  SORT lt_itab BY row col.
  REFRESH p_table.

  LOOP AT lt_itab.
    MOVE : lt_itab-col TO lw_index.
    ASSIGN COMPONENT lw_index OF STRUCTURE p_table TO <lw_fs>.

    DESCRIBE FIELD <lw_fs> TYPE lw_field_type.

    IF lw_field_type = 'D'.  "'MM/DD/YYYY" " Date type Conversion
      CONCATENATE lt_itab-value+6(4)    "YEAR  (YYYY)
                  lt_itab-value+0(2)    "MONTH (MM)
                  lt_itab-value+3(2)    "DAY   (DD)
                              INTO <lw_fs>.
    ELSE.
      MOVE : lt_itab-value TO <lw_fs>.
    ENDIF.

    AT END OF row.
      APPEND p_table.
      CLEAR p_table.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " F_EXCEL_UPLOAD
*&------------------------------------------------------------------*
*&      Form  WRITE_HEADER
*&------------------------------------------------------------------*
FORM write_header.
  WRITE :
    'Equipment Master Uploading  Log'(h01).
  ULINE.
  WRITE : 'Total of Equipment Master : '(h02) ,
          wa_total_entr.
  NEW-LINE.
  WRITE : 'Success entries           : '(h03) ,
          wa_success_cnt.
  NEW-LINE.
  WRITE : 'Failed entries            : '(h04),
          wa_failed_cnt.
  ULINE.
ENDFORM.                    " WRITE_HEADER
*&-----------------------------------------------------------------*
*&      Form  WRITE_LOG_MESSAGE
*&-----------------------------------------------------------------*
FORM write_log_message.

  LOOP AT it_return WHERE type = ' '.
    WRITE it_return-message.
    NEW-LINE.
  ENDLOOP.

  ULINE.

  LOOP AT it_return.
    IF    it_return-type IS INITIAL AND
      NOT it_return-message IS INITIAL.
      NEW-LINE.
    ENDIF.

    WRITE : it_return-type,
            it_return-id,
            it_return-number,
  AT (150)  it_return-message,
*            IT_RETURN-LOG_NO,
*            IT_RETURN-LOG_MSG_NO,
*            IT_RETURN-MESSAGE_V1,
*            IT_RETURN-MESSAGE_V2,
*            IT_RETURN-MESSAGE_V3,
*            IT_RETURN-MESSAGE_V4,
            it_return-parameter,
*            IT_RETURN-ROW,
            it_return-field.
*            IT_RETURN-SYSTEM.
    NEW-LINE.
  ENDLOOP.
ENDFORM.                    " WRITE_LOG_MESSAGE

*&------------------------------------------------------------------*
*&      Form  BAPI_SERVICE_ROLLBACK
*&------------------------------------------------------------------*
FORM bapi_service_rollback.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
* IMPORTING
*   RETURN        =     .

ENDFORM.                    " BAPI_SERVICE_ROLLBACK
*&------------------------------------------------------------------*
*&      Form  BAPI_SERVICE_COMMIT
*&------------------------------------------------------------------*
FORM bapi_service_commit.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            wait = c_mark.
* IMPORTING
*   RETURN        = .

ENDFORM.                    " BAPI_SERVICE_COMMIT
*&------------------------------------------------------------------*
*&      Form  ADD_DATA_EQUIP_PARVW_MA
*&------------------------------------------------------------------*
FORM add_data_equip_parvw_ma USING   p_equnr    TYPE equnr
                                     pw_equi     STRUCTURE it_equi
                                     p_subrc     LIKE sy-subrc.
  "PW_RETURN2  TYPE BAPIRET2.

  DATA : lw_equnr TYPE equnr.

  REFRESH : bdc_tab, lt_msg.



*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*       EXPORTING
*            input  = p_equnr
*       IMPORTING
*            output = lw_equnr.
*
*
**- IE02-Change Equipment
*  PERFORM dynpro  USING:
*      'X'   'SAPMIEQ0'          '0100',
*      ' '   'BDC_OKCODE'        '/00',
*      ' '   'RM63E-EQUNR'       lw_equnr.
*
**-- Click Maintenance Address
*  PERFORM dynpro  USING:
*      'X'   'SAPMIEQ0'          '0101',
*      ' '   'BDC_OKCODE'        '=ADRE'.
*
**-- Maintenance Address POPUP Window
*  PERFORM dynpro  USING:
*      'X'   'SAPLSZA1'          '0201',
*      ' '   'BDC_OKCODE'        '=CONT',
*      ' '   'SZA1_D0100-TITLE_MEDI'  pw_equi-title_medi, "/Company
*      ' '   'ADDR1_DATA-NAME1'       pw_equi-name1,    "/Company name
**      ' '   'ADDR1_DATA-SORT1'       PW_EQUI-SORT1, "/
*      ' '   'ADDR1_DATA-STREET'      pw_equi-street, "/STREET
*      ' '   'ADDR1_DATA-POST_CODE1'  pw_equi-post_code1, "/Postal code
*      ' '   'ADDR1_DATA-CITY1'       pw_equi-city1,     "/
*      ' '   'ADDR1_DATA-COUNTRY'     pw_equi-country, "/Country
*      ' '   'ADDR1_DATA-REGION'      pw_equi-region, "/ REGION
*      ' '   'ADDR1_DATA-PO_BOX'      pw_equi-po_box, "/PO_BOX
*      ' '   'ADDR1_DATA-POST_CODE2'  pw_equi-post_code2, "/POST_CODE2
*      ' '   'ADDR1_DATA-POST_CODE3'  pw_equi-post_code3, "/Comp. p.
*code
*      ' '   'ADDR1_DATA-LANGU'       pw_equi-langu, "/Language
*      ' '   'SZA1_D0100-TEL_NUMBER'  pw_equi-tel_number, "/TEL_NUMBER
*      ' '   'SZA1_D0100-TEL_EXTENS'  pw_equi-tel_extens, "/TEL_EXTENS
*      ' '   'SZA1_D0100-FAX_NUMBER'  pw_equi-fax_number, "/FAX_NUMBER
*      ' '   'SZA1_D0100-FAX_EXTENS'  pw_equi-fax_extens, "/FAX_EXTENS
*      ' '   'SZA1_D0100-SMTP_ADDR'   pw_equi-smtp_addr. "/SMTP_ADDR
*
**-- Click Partner
*  PERFORM dynpro  USING:
*      'X'   'SAPMIEQ0'          '0101',
*      ' '   'BDC_OKCODE'        '=PART'.
**-- Change Equipment Partner
*  PERFORM dynpro  USING:
*      'X'   'SAPLIPAR'          '0200',
*      ' '   'BDC_OKCODE'        '=BACK',
*      ' '   'IHPA-PARVW(01)'      pw_equi-parvw_ab, "/
*      ' '   'IHPA-PARNR(01)'      pw_equi-parnr_ab, "/
*      ' '   'IHPA-PARVW(02)'      pw_equi-parvw_vw, "/
*      ' '   'IHPA-PARNR(02)'      pw_equi-parnr_vw. "/
*
*
**-- Click Class Overview
*  PERFORM dynpro  USING:
*      'X'   'SAPMIEQ0'          '0101',
*      ' '   'BDC_OKCODE'        '=KL'.
**-  Class
*  PERFORM dynpro  USING:
*      'X'   'SAPLCLCA'          '0602',
*      ' '   'BDC_OKCODE'        '=ENTE',
*      ' '   'RMCLF-KLART'        '002'. "/
*
*  PERFORM dynpro  USING:
*      'X'   'SAPLCLFM'          '0500',
*      ' '   'BDC_OKCODE'        '=AUSW',
*      ' '   'RMCLF-KREUZ(01)'   'X',
*      ' '   'RMCLF-CLASS(01)'   pw_equi-class. "/
*
*  PERFORM dynpro  USING:
*      'X'   'SAPLCTMS'          '0109',
*      ' '   'BDC_OKCODE'        '=BACK',
*      ' '   'RCTMS-MNAME(01)'    pw_equi-atnam, "/
*      ' '   'RCTMS-MWERT(01)'    pw_equi-atwrt. "/
*
*  PERFORM dynpro  USING:
*      'X'   'SAPLCLFM'          '0500',
*      ' '   'BDC_OKCODE'        '=ENDE'.
*
**-- Save
*  PERFORM dynpro  USING:
*      'X'   'SAPMIEQ0'          '0101',
*      ' '   'BDC_OKCODE'        '=BU'.
*
*
**  SET PARAMETER ID 'EQN' FIELD ''.
**  SET PARAMETER ID 'EQN' FIELD lw_equnr.
*  CLEAR: LT_MSG[].
*  CALL TRANSACTION 'IE02'    USING bdc_tab
*                           UPDATE 'S'
*                           MODE p_mode
*                        MESSAGES INTO lt_msg.
*
*  READ TABLE lt_msg WITH KEY msgtyp = 'E'.
*
*  IF sy-subrc = 0.
*    p_subrc = '4'.
*
*  ELSE.
*    READ TABLE lt_msg WITH KEY msgtyp = 'A'.
*
*    IF sy-subrc = 0.
*      p_subrc = '8'.
*
*    ELSE.
*      READ TABLE lt_msg WITH KEY msgtyp = 'S'
*                                 msgnr  = '817'.
*
*    ENDIF.
*  ENDIF.
*
*
**  PERFORM GET_BAPI_RETURN_MSG  USING LT_MSG-MSGTYP
**                                     LT_MSG-MSGID
**                                     LT_MSG-MSGNR
**                                     LT_MSG-MSGV1
**                                     LT_MSG-MSGV2
**                                     LT_MSG-MSGV3
**                                     LT_MSG-MSGV4
**                               CHANGING PW_RETURN2.
**



ENDFORM.                    " ADD_DATA_EQUIP_PARVW_MA

*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.
  IF dynbegin = 'X'.
    CLEAR bdc_tab.
    MOVE : name  TO bdc_tab-program,
           value TO bdc_tab-dynpro,
           'X'   TO bdc_tab-dynbegin.
    APPEND bdc_tab.
  ELSE.
    CLEAR bdc_tab.
    MOVE : name  TO bdc_tab-fnam,
           value TO bdc_tab-fval.
    APPEND bdc_tab.
  ENDIF.
ENDFORM.                    "DYNPRO
*&------------------------------------------------------------------*
*&      Form  GET_WORKCENTER_OBJID
*&-----------------------------------------------------------------*
FORM get_workcenter_objid USING    p_arbpl
                                   p_objid.

  SELECT SINGLE objid INTO p_objid
     FROM crhd
       WHERE objty = 'A'
         AND arbpl = p_arbpl.

ENDFORM.                    " GET_WORKCENTER_OBJID
*&---------------------------------------------------------------------*
*&      Form  GET_BAPI_RETURN_MSG
*&---------------------------------------------------------------------*
FORM get_bapi_return_msg USING    p_msgtyp
                                  p_msgid
                                  p_msgnr
                                  p_msgv1
                                  p_msgv2
                                  p_msgv3
                                  p_msgv4
                         CHANGING pw_return2 STRUCTURE bapiret2.

  DATA : lw_type	  LIKE	bapireturn-type,
         lw_cl	  LIKE	sy-msgid,
         lw_number  LIKE	sy-msgno,
         lw_par1	  LIKE	sy-msgv1,
         lw_par2	  LIKE	sy-msgv2,
         lw_par3	  LIKE	sy-msgv3,
         lw_par4	  LIKE	sy-msgv4.


  lw_type   = p_msgtyp.
  lw_cl     = p_msgid.
  lw_number = p_msgnr.
  lw_par1   = p_msgv1.
  lw_par2   = p_msgv2.
  lw_par3   = p_msgv3.
  lw_par4   = p_msgv4.

  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
       EXPORTING
            type   = lw_type
            cl     = lw_cl
            number = lw_number
            par1   = lw_par1
            par2   = lw_par2
            par3   = lw_par3
            par4   = lw_par4
       IMPORTING
            return = pw_return2.

ENDFORM.                    " GET_BAPI_RETURN_MSG
*&-----------------------------------------------------------------*
*&      Form  DISPLAY_PROGRESS_BAR
*&-----------------------------------------------------------------*
FORM display_progress_bar USING p_text
                                p_cur
                                p_tot.

  DATA : lw_percentage TYPE i.
  IF p_tot IS INITIAL.
    lw_percentage = 50.
  ELSE.
    lw_percentage = p_cur / p_tot * 100.
  ENDIF.
  CALL FUNCTION 'TB_PROGRESS_INDICATOR'
       EXPORTING
            percentage = lw_percentage
            text       = p_text.

ENDFORM.                    " DISPLAY_PROGRESS_BAR
*&---------------------------------------------------------------------*
*&      Form  bdc_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_process.
  LOOP AT it_equi.
    CLEAR p_number.
    PERFORM bdc_head USING it_equi
                     CHANGING p_number.

    PERFORM add_data_equip_parvw_ma  USING p_number
                                           it_equi
                                           sy-subrc.
  ENDLOOP.                                           "LW_RETURN2.

ENDFORM.                    " bdc_process
*&---------------------------------------------------------------------*
*&      Form  bdc_head
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EQUI  text
*      <--P_P_NUMBER  text
*----------------------------------------------------------------------*
FORM bdc_head USING    p_equi
              CHANGING pp_number.

  DATA   BEGIN OF lt_msg OCCURS 0.
          INCLUDE STRUCTURE bdcmsgcoll.
  DATA   END OF lt_msg.
  DATA : g_date(10).

  REFRESH : bdc_tab, lt_msg.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**       EXPORTING
**            input  = sy-datum
**       IMPORTING
**            output = g_date.
*  DATA  : a(2),b(2),c(4).
*  MOVE sy-datum+4(2) TO a.
*  MOVE sy-datum+6(2) TO b.
*  MOVE sy-datum(4) TO c.
*  CONCATENATE a b c INTO g_date.
*
*  PERFORM dynpro  USING:
*       'X'   'SAPMIEQ0'          '0100',
*       ' '   'BDC_OKCODE'        '/00',
*       ' '   'RM63E-DATSL'       g_date,
*       ' '   'RM63E-EQtyp'       'Z',
*
*       'X'   'SAPMIEQ0'          '0101',                    " 1 VIEW
*       ' '   'BDC_OKCODE'        '=T\02',
*       ' '   'ITOB-BRGEW'        it_equi-brgew,
*       ' '   'ITOB-EQART'        it_equi-eqart,
*       ' '   'ITOB-GEWEI'        it_equi-gewei,
*       ' '   'ITOB-EQART'        it_equi-eqart,
*       ' '   'ITOB-ANSWT'        it_equi-answt,
*       ' '   'ITOB-WAERS'        it_equi-waers,
*       ' '   'ITOB-HERST'        it_equi-herst,
*       ' '   'ITOB-HERLD'        it_equi-herld,
*       ' '   'ITOB-INVNR'        it_equi-invnr,
*       ' '   'ITOB-INBDT'        g_date,
*       ' '   'ITOB-ANSDT'        g_date,
*       ' '   'ITOB-TYPBZ'        it_equi-typbz,
*       ' '   'ITOB-BAUJJ'        it_equi-baujj,
*       ' '   'ITOB-BAUMM'        it_equi-baumm,
*       ' '   'ITOB-SERGE'        it_equi-serge,
*       ' '   'ITOB-GROES'        it_equi-groes,
*       ' '   'ITOB-SHTXT'        it_equi-shtxt,
*       ' '   'ITOB-DATAB'        g_date,
*
*       'X'   'SAPMIEQ0'          '0101',
*       ' '   'BDC_OKCODE'        '=T\03',
*       ' '   'ITOB-SWERK'        it_equi-swerk,
*       ' '   'ITOBATTR-ARBPL'    it_equi-arbpl,
*       ' '   'ITOB-ABCKZ'        it_equi-abckz,
*       ' '   'ITOB-SHTXT'        it_equi-shtxt,
*       ' '   'ITOB-DATAB'        g_date,
*
*       'X'   'SAPMIEQ0'          '0101',
*       ' '   'BDC_OKCODE'        '=BU',
*       ' '   'ITOB-BUKRS'        it_equi-bukrs,
*       ' '   'ITOB-KOSTL'        it_equi-kostl,
**       ' '   'ITOB-IWERK'        it_equi-iwerk,
*       ' '   'ITOBATTR-GEWRK'        it_equi-gewrk,
**       ' '   'ITOB-WERGW'        it_equi-wergw,
*       ' '   'ITOB-SHTXT'        it_equi-shtxt,
*       ' '   'ITOB-DATAB'        g_date.
*
*
*  CLEAR lt_msg[].
*  CALL TRANSACTION 'IE01'    USING bdc_tab
*                           UPDATE 'S'
*                           MODE p_mode
*                        MESSAGES INTO lt_msg.
*
*  READ TABLE lt_msg WITH KEY MSGTYP = 'S'.
*  MOVE lt_msg-msgv1 TO pp_number.



ENDFORM.                    " bdc_head
*&---------------------------------------------------------------------*
*&      Form  SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form SCREEN_OUTPUT.
  wa_name = 'P_TEST'.
  wa_value-key = 'X'. wa_value-text = 'Simulation'.
  APPEND wa_value TO it_list.
  wa_value-key = ' '. wa_value-text = 'Write'.
  APPEND wa_value TO it_list.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = wa_name
            values = it_list.

endform.                    " SCREEN_OUTPUT
