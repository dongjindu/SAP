*----------------------------------------------------------------------*
*   INCLUDE ZEPP317L_MAT_INIT_BDC_01_T                                 *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: MARC,
        MARA.
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*

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
*        MATNR1(18),
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
        BSTME(03),  " TYPE C,   "Order unit	
        TEXT31(18), " TYPE C,  "Var. OUn	
        EKGRP(03),  " TYPE MARC-EKGRP,  "Purchasing group	
        TEXT32(18), " TYPE C,  "Material group	
        MMSTA(02),  " TYPE C,  "Plant-sp.matl status	
        KAUTB(01),  " TYPE MARC-KAUTB,  "Autom.PO	
        TEXT34(18), " TYPE C,  "Batch management	
        TEXT35(18), " TYPE C,  "Critical part	
        TEXT36(18), " TYPE C,  "Quota arr. usage	
        KORDB(01),  " TYPE MARC-KORDB,  "Source list	
        FABKZ(01),  " TYPE MARC-FABKZ,  "JIT sched. indicator	
*       Foreign Trade/Import data	
        STAWN(17), " TYPE C,  "Comm./imp.code no.	
        HERKL(03), " TYPE C,  "Country of origin
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

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I.


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_RDO1 RADIOBUTTON GROUP R1 USER-COMMAND UCOM,
            P_RDO2 RADIOBUTTON GROUP R1 DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK B2.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT',
  P_TCODE LIKE TSTC-TCODE DEFAULT 'MM01'.
SELECTION-SCREEN END   OF BLOCK B1.
