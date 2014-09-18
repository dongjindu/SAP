************************************************************************
* Program Name      : ZACO20R_SHOP
* Author            : Hyung Jin Youn
* Creation Date     : 19/05/2004
* Specifications By : Hae Sung Cho
* Pattern           : Report 1-1
* Development Request No:UD1K909481
* Add documentation :
* Description       : Report to display the result of SHOP data
*                     Actual
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
REPORT zaco20r_shop
   LINE-SIZE 253 NO STANDARD PAGE HEADING LINE-COUNT 000(001).

INCLUDE <symbol>.
INCLUDE <icon>.
SELECTION-SCREEN: BEGIN OF BLOCK prog
                           WITH FRAME TITLE text-f58.

* %FA00000 ZTCO_SHOPCOST_AT-PAR_KADKY
* %FA00001 ZTCO_SHOPCOST_AT-PAR_KALNR
* %FA00002 ZTCO_SHOPCOST_AT-RECORD_TYPE
* %FA00003 ZTCO_SHOPCOST_AT-LLV_MATNR
* %FA00004 ZTCO_SHOPCOST_AT-FSC_MATNR
* %FA00005 ZTCO_SHOPCOST_AT-WIP_QUANTITY
* %FA00006 ZTCO_SHOPCOST_AT-SCRAP_AMT
* %FA00007 ZTCO_SHOPCOST_AT-ACTUAL_SCRAP
* %FA00008 ZTCO_SHOPCOST_AT-ADD_WKGBTR
* %FA00009 ZTCO_SHOPCOST_AT-ADD_MBGBTR
* %FA00010 ZTCO_SHOPCOST_AT-CHD_PROC_KALNR
* %FA00011 ZTCO_SHOPCOST_AT-PAR_PROC_KALNR
* %FA00012 ZTCO_SHOPCOST_AT-CHD_KALNR
* %FA00013 ZTCO_SHOPCOST_AT-ML_ACT_PREIS
TABLES ztco_shopcost_at.
DATA %count-ztco_shopcost_at(4) TYPE x.
DATA %linr-ztco_shopcost_at(2).

TABLES aqldb.

INCLUDE rsaqexcd.

DATA: BEGIN OF %st_liste OCCURS 100,
          head(1),
          tab(3),
          line(6) TYPE n,
          cont(1) TYPE n,
          fint(1),
          finv(1),
          fcol(1) TYPE n,
          text(0253),
      END OF %st_liste.

DATA %data_selected(1).
DATA %glframe(1)  VALUE 'X' .
DATA %uflag(1).
DATA %ustflag(1).
DATA %grst_text(255).
DATA %glline TYPE i.
DATA %tabix LIKE sy-tabix.
DATA %prflag(1) TYPE x VALUE '02'.


DATA %perc(4) TYPE p DECIMALS 3.
DATA %p100(4) TYPE p DECIMALS 3 VALUE '100.000'.
DATA %rangct TYPE i.
DATA %rangcc(8).
DATA %subrc LIKE sy-subrc.

DATA: BEGIN OF %wa020 OCCURS 10,
            ztco_shopcost_at-wkgbtr(16) TYPE p DECIMALS 02,
            %fa00008(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-wip_amt(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-preis(16) TYPE p DECIMALS 02,
            %fa00006(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-mbgbtr(16) TYPE p DECIMALS 03,
            %fa00009(16) TYPE p DECIMALS 03,
            %fa00007(16) TYPE p DECIMALS 03,
            %fa00005(16) TYPE p DECIMALS 03,
            BEGIN OF ztco_shopcost_at,
                  ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
                  ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            END OF ztco_shopcost_at,
      END OF %wa020.

DATA: BEGIN OF %wa030 OCCURS 10,
            %fa00013(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-manu_amt(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-manu_qty(16) TYPE p DECIMALS 03,
            BEGIN OF ztco_shopcost_at,
                  ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
                  ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            END OF ztco_shopcost_at,
      END OF %wa030.

DATA: BEGIN OF %w0100 OCCURS 20,
            ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
            ztco_shopcost_at-wkgbtr(16) TYPE p DECIMALS 02,
            %fa00008(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-wip_amt(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-preis(16) TYPE p DECIMALS 02,
            %fa00006(16) TYPE p DECIMALS 02,
            %fa00013(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-manu_amt(16) TYPE p DECIMALS 02,
      END OF %w0100.

DATA: BEGIN OF %w0101 OCCURS 20,
            ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
            ztco_shopcost_at-wkgbtr(16) TYPE p DECIMALS 02,
            %fa00008(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-wip_amt(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-preis(16) TYPE p DECIMALS 02,
            %fa00006(16) TYPE p DECIMALS 02,
            %fa00013(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-manu_amt(16) TYPE p DECIMALS 02,
      END OF %w0101.

DATA: BEGIN OF %w0102 OCCURS 20,
            ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
            ztco_shopcost_at-wkgbtr(16) TYPE p DECIMALS 02,
            %fa00008(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-wip_amt(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-preis(16) TYPE p DECIMALS 02,
            %fa00006(16) TYPE p DECIMALS 02,
            %fa00013(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-manu_amt(16) TYPE p DECIMALS 02,
      END OF %w0102.

DATA: BEGIN OF %w0103 OCCURS 20,
            ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
            ztco_shopcost_at-wkgbtr(16) TYPE p DECIMALS 02,
            %fa00008(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-wip_amt(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-preis(16) TYPE p DECIMALS 02,
            %fa00006(16) TYPE p DECIMALS 02,
            %fa00013(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-manu_amt(16) TYPE p DECIMALS 02,
      END OF %w0103.

DATA: BEGIN OF %w0104 OCCURS 20,
            ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
            ztco_shopcost_at-wkgbtr(16) TYPE p DECIMALS 02,
            %fa00008(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-wip_amt(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-preis(16) TYPE p DECIMALS 02,
            %fa00006(16) TYPE p DECIMALS 02,
            %fa00013(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-manu_amt(16) TYPE p DECIMALS 02,
      END OF %w0104.

DATA: BEGIN OF %w0105 OCCURS 20,
            ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
            ztco_shopcost_at-wkgbtr(16) TYPE p DECIMALS 02,
            %fa00008(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-wip_amt(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-preis(16) TYPE p DECIMALS 02,
            %fa00006(16) TYPE p DECIMALS 02,
            %fa00013(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-manu_amt(16) TYPE p DECIMALS 02,
      END OF %w0105.

DATA: BEGIN OF %w0106 OCCURS 20,
            ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
            ztco_shopcost_at-wkgbtr(16) TYPE p DECIMALS 02,
            %fa00008(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-wip_amt(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-preis(16) TYPE p DECIMALS 02,
            %fa00006(16) TYPE p DECIMALS 02,
            %fa00013(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-manu_amt(16) TYPE p DECIMALS 02,
      END OF %w0106.

DATA: BEGIN OF %w0107 OCCURS 20,
            ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
            ztco_shopcost_at-wkgbtr(16) TYPE p DECIMALS 02,
            %fa00008(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-wip_amt(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-preis(16) TYPE p DECIMALS 02,
            %fa00006(16) TYPE p DECIMALS 02,
            %fa00013(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-manu_amt(16) TYPE p DECIMALS 02,
      END OF %w0107.

DATA: BEGIN OF %w0108 OCCURS 20,
            ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
            ztco_shopcost_at-wkgbtr(16) TYPE p DECIMALS 02,
            %fa00008(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-wip_amt(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-preis(16) TYPE p DECIMALS 02,
            %fa00006(16) TYPE p DECIMALS 02,
            %fa00013(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-manu_amt(16) TYPE p DECIMALS 02,
      END OF %w0108.

DATA: BEGIN OF %w0109 OCCURS 20,
            ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
            ztco_shopcost_at-wkgbtr(16) TYPE p DECIMALS 02,
            %fa00008(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-wip_amt(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-preis(16) TYPE p DECIMALS 02,
            %fa00006(16) TYPE p DECIMALS 02,
            %fa00013(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-manu_amt(16) TYPE p DECIMALS 02,
      END OF %w0109.

DATA: BEGIN OF %w0110 OCCURS 20,
            ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
            ztco_shopcost_at-wkgbtr(16) TYPE p DECIMALS 02,
            %fa00008(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-wip_amt(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-preis(16) TYPE p DECIMALS 02,
            %fa00006(16) TYPE p DECIMALS 02,
            %fa00013(16) TYPE p DECIMALS 02,
            ztco_shopcost_at-manu_amt(16) TYPE p DECIMALS 02,
      END OF %w0110.

DATA: BEGIN OF %w0200 OCCURS 20,
            ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-mbgbtr(16) TYPE p DECIMALS 03,
            %fa00009(16) TYPE p DECIMALS 03,
            %fa00007(16) TYPE p DECIMALS 03,
            %fa00005(16) TYPE p DECIMALS 03,
            ztco_shopcost_at-manu_qty(16) TYPE p DECIMALS 03,
      END OF %w0200.

DATA: BEGIN OF %w0201 OCCURS 20,
            ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-mbgbtr(16) TYPE p DECIMALS 03,
            %fa00009(16) TYPE p DECIMALS 03,
            %fa00007(16) TYPE p DECIMALS 03,
            %fa00005(16) TYPE p DECIMALS 03,
            ztco_shopcost_at-manu_qty(16) TYPE p DECIMALS 03,
      END OF %w0201.

DATA: BEGIN OF %w0202 OCCURS 20,
            ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-mbgbtr(16) TYPE p DECIMALS 03,
            %fa00009(16) TYPE p DECIMALS 03,
            %fa00007(16) TYPE p DECIMALS 03,
            %fa00005(16) TYPE p DECIMALS 03,
            ztco_shopcost_at-manu_qty(16) TYPE p DECIMALS 03,
      END OF %w0202.

DATA: BEGIN OF %w0203 OCCURS 20,
            ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-mbgbtr(16) TYPE p DECIMALS 03,
            %fa00009(16) TYPE p DECIMALS 03,
            %fa00007(16) TYPE p DECIMALS 03,
            %fa00005(16) TYPE p DECIMALS 03,
            ztco_shopcost_at-manu_qty(16) TYPE p DECIMALS 03,
      END OF %w0203.

DATA: BEGIN OF %w0204 OCCURS 20,
            ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-mbgbtr(16) TYPE p DECIMALS 03,
            %fa00009(16) TYPE p DECIMALS 03,
            %fa00007(16) TYPE p DECIMALS 03,
            %fa00005(16) TYPE p DECIMALS 03,
            ztco_shopcost_at-manu_qty(16) TYPE p DECIMALS 03,
      END OF %w0204.

DATA: BEGIN OF %w0205 OCCURS 20,
            ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-mbgbtr(16) TYPE p DECIMALS 03,
            %fa00009(16) TYPE p DECIMALS 03,
            %fa00007(16) TYPE p DECIMALS 03,
            %fa00005(16) TYPE p DECIMALS 03,
            ztco_shopcost_at-manu_qty(16) TYPE p DECIMALS 03,
      END OF %w0205.

DATA: BEGIN OF %w0206 OCCURS 20,
            ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-mbgbtr(16) TYPE p DECIMALS 03,
            %fa00009(16) TYPE p DECIMALS 03,
            %fa00007(16) TYPE p DECIMALS 03,
            %fa00005(16) TYPE p DECIMALS 03,
            ztco_shopcost_at-manu_qty(16) TYPE p DECIMALS 03,
      END OF %w0206.

DATA: BEGIN OF %w0207 OCCURS 20,
            ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-mbgbtr(16) TYPE p DECIMALS 03,
            %fa00009(16) TYPE p DECIMALS 03,
            %fa00007(16) TYPE p DECIMALS 03,
            %fa00005(16) TYPE p DECIMALS 03,
            ztco_shopcost_at-manu_qty(16) TYPE p DECIMALS 03,
      END OF %w0207.

DATA: BEGIN OF %w0208 OCCURS 20,
            ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-mbgbtr(16) TYPE p DECIMALS 03,
            %fa00009(16) TYPE p DECIMALS 03,
            %fa00007(16) TYPE p DECIMALS 03,
            %fa00005(16) TYPE p DECIMALS 03,
            ztco_shopcost_at-manu_qty(16) TYPE p DECIMALS 03,
      END OF %w0208.

DATA: BEGIN OF %w0209 OCCURS 20,
            ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-mbgbtr(16) TYPE p DECIMALS 03,
            %fa00009(16) TYPE p DECIMALS 03,
            %fa00007(16) TYPE p DECIMALS 03,
            %fa00005(16) TYPE p DECIMALS 03,
            ztco_shopcost_at-manu_qty(16) TYPE p DECIMALS 03,
      END OF %w0209.

DATA: BEGIN OF %w0210 OCCURS 20,
            ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-mbgbtr(16) TYPE p DECIMALS 03,
            %fa00009(16) TYPE p DECIMALS 03,
            %fa00007(16) TYPE p DECIMALS 03,
            %fa00005(16) TYPE p DECIMALS 03,
            ztco_shopcost_at-manu_qty(16) TYPE p DECIMALS 03,
      END OF %w0210.
SELECT-OPTIONS sp$00001 FOR ztco_shopcost_at-kokrs MEMORY ID cac.
SELECT-OPTIONS sp$00002 FOR ztco_shopcost_at-bdatj MEMORY ID bdtj.
SELECT-OPTIONS sp$00003 FOR ztco_shopcost_at-poper MEMORY ID popr.
SELECT-OPTIONS sp$00004 FOR ztco_shopcost_at-aufnr MEMORY ID anr.
SELECT-OPTIONS sp$00005 FOR ztco_shopcost_at-versn MEMORY ID kvs.
SELECT-OPTIONS sp$00006 FOR ztco_shopcost_at-record_type.
SELECT-OPTIONS sp$00007 FOR ztco_shopcost_at-fsc_matnr MEMORY ID mat.
SELECT-OPTIONS sp$00008 FOR ztco_shopcost_at-shop.
SELECT-OPTIONS sp$00009 FOR ztco_shopcost_at-llv_matnr MEMORY ID mat.
SELECT-OPTIONS sp$00010 FOR ztco_shopcost_at-typps.
SELECT-OPTIONS sp$00011 FOR ztco_shopcost_at-kstar MEMORY ID kat.
SELECT-OPTIONS sp$00012 FOR ztco_shopcost_at-elemt.
SELECT-OPTIONS sp$00013 FOR ztco_shopcost_at-kostl MEMORY ID kos.
SELECT-OPTIONS sp$00014 FOR ztco_shopcost_at-lstar MEMORY ID lar.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK direct
                  WITH FRAME TITLE text-f59.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %alv RADIOBUTTON GROUP func USER-COMMAND outbut
                         DEFAULT 'X' .
SELECTION-SCREEN: COMMENT 4(26) text-f72 FOR FIELD %alv.
PARAMETERS:       %alvl TYPE slis_vari.
SELECTION-SCREEN: PUSHBUTTON 72(4) pb%exco USER-COMMAND expcol.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %nofunc RADIOBUTTON GROUP func MODIF ID old.
SELECTION-SCREEN: COMMENT 4(26) text-f66 FOR FIELD %nofunc
                                         MODIF ID old.
PARAMETERS:       %tview RADIOBUTTON GROUP func MODIF ID old.
SELECTION-SCREEN: COMMENT 34(26) text-f68 FOR FIELD %tview
                                          MODIF ID old,
                  END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %graph RADIOBUTTON GROUP func MODIF ID old.
SELECTION-SCREEN: COMMENT 4(26) text-f61 FOR FIELD %graph
                                         MODIF ID old.
PARAMETERS:       %text RADIOBUTTON GROUP func MODIF ID old.
SELECTION-SCREEN: COMMENT 34(26) text-f69 FOR FIELD %text
                                          MODIF ID old,
                  END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %abc RADIOBUTTON GROUP func MODIF ID old.
SELECTION-SCREEN: COMMENT 4(26) text-f70 FOR FIELD %abc
                                         MODIF ID old.
PARAMETERS:       %excel RADIOBUTTON GROUP func MODIF ID old.
SELECTION-SCREEN: COMMENT 34(26) text-f60 FOR FIELD %excel
                                         MODIF ID old,
                  END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %eis RADIOBUTTON GROUP func MODIF ID old.
SELECTION-SCREEN: COMMENT 4(26) text-f63 FOR FIELD %eis
                                         MODIF ID old.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %xint RADIOBUTTON GROUP func MODIF ID xin.
SELECTION-SCREEN: COMMENT 4(26) text-f73 FOR FIELD %xint
                                         MODIF ID xin.
PARAMETERS:       %xintk(30) LOWER CASE MODIF ID xin.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %down RADIOBUTTON GROUP func MODIF ID old.
SELECTION-SCREEN: COMMENT 4(26) text-f64 FOR FIELD %down
                                         MODIF ID old.
PARAMETERS:       %path(132) LOWER CASE MODIF ID old.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %save RADIOBUTTON GROUP func MODIF ID old.
SELECTION-SCREEN: COMMENT 4(26) text-f62 FOR FIELD %save
                                         MODIF ID old.
PARAMETERS:       %listid(40) LOWER CASE MODIF ID old.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: END OF BLOCK direct.
SELECTION-SCREEN: END OF BLOCK prog.

*ANDY
*ARAMETERS: p_sum AS CHECKBOX.

DATA: BEGIN OF %g00 OCCURS 100,
            ztco_shopcost_at-kokrs LIKE ztco_shopcost_at-kokrs,
            ztco_shopcost_at-bdatj LIKE ztco_shopcost_at-bdatj,
            ztco_shopcost_at-poper LIKE ztco_shopcost_at-poper,
            ztco_shopcost_at-aufnr LIKE ztco_shopcost_at-aufnr,
            ztco_shopcost_at-versn LIKE ztco_shopcost_at-versn,
            %fa00002 LIKE ztco_shopcost_at-record_type,
            %fa00004 LIKE ztco_shopcost_at-fsc_matnr,
            ztco_shopcost_at-shop LIKE ztco_shopcost_at-shop,
            %fa00003 LIKE ztco_shopcost_at-llv_matnr,
            ztco_shopcost_at-typps LIKE ztco_shopcost_at-typps,
            ztco_shopcost_at-kstar LIKE ztco_shopcost_at-kstar,
            ztco_shopcost_at-kalnr LIKE ztco_shopcost_at-kalnr,
            ztco_shopcost_at-elemt LIKE ztco_shopcost_at-elemt,
            ztco_shopcost_at-kostl LIKE ztco_shopcost_at-kostl,
            ztco_shopcost_at-lstar LIKE ztco_shopcost_at-lstar,
            ztco_shopcost_at-hwaer LIKE ztco_shopcost_at-hwaer,
            ztco_shopcost_at-meeht LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-beskz LIKE ztco_shopcost_at-beskz,
            ztco_shopcost_at-sobsl LIKE ztco_shopcost_at-sobsl,
            ztco_shopcost_at-vspvb LIKE ztco_shopcost_at-vspvb,
            %fa00001 LIKE ztco_shopcost_at-par_kalnr,
            %fa00000 LIKE ztco_shopcost_at-par_kadky,
            ztco_shopcost_at-mtart LIKE ztco_shopcost_at-mtart,
            ztco_shopcost_at-wkgbtr LIKE ztco_shopcost_at-wkgbtr,
            ztco_shopcost_at-mbgbtr LIKE ztco_shopcost_at-mbgbtr,
            ztco_shopcost_at-meeht-0203 LIKE ztco_shopcost_at-meeht,
            %fa00008 LIKE ztco_shopcost_at-add_wkgbtr,
            %fa00009 LIKE ztco_shopcost_at-add_mbgbtr,
            ztco_shopcost_at-meeht-0205 LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-wip_amt LIKE ztco_shopcost_at-wip_amt,
            %fa00005 LIKE ztco_shopcost_at-wip_quantity,
            ztco_shopcost_at-meeht-0207 LIKE ztco_shopcost_at-meeht,
            %fa00006 LIKE ztco_shopcost_at-scrap_amt,
            %fa00007 LIKE ztco_shopcost_at-actual_scrap,
            ztco_shopcost_at-meeht-0209 LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-preis LIKE ztco_shopcost_at-preis,
            %fa00013 LIKE ztco_shopcost_at-ml_act_preis,
            ztco_shopcost_at-manu_amt LIKE ztco_shopcost_at-manu_amt,
            ztco_shopcost_at-manu_qty LIKE ztco_shopcost_at-manu_qty,
            ztco_shopcost_at-meeht-0303 LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-werks LIKE ztco_shopcost_at-werks,
            ztco_shopcost_at-bwkey LIKE ztco_shopcost_at-bwkey,
            ztco_shopcost_at-bwtar LIKE ztco_shopcost_at-bwtar,
            ztco_shopcost_at-verid LIKE ztco_shopcost_at-verid,
            ztco_shopcost_at-objnr LIKE ztco_shopcost_at-objnr,
            %fa00012 LIKE ztco_shopcost_at-chd_kalnr,
            %fa00011 LIKE ztco_shopcost_at-par_proc_kalnr,
            %fa00010 LIKE ztco_shopcost_at-chd_proc_kalnr,
            ztco_shopcost_at-erdat LIKE ztco_shopcost_at-erdat,
            ztco_shopcost_at-erzet LIKE ztco_shopcost_at-erzet,
            ztco_shopcost_at-ernam LIKE ztco_shopcost_at-ernam,
            ztco_shopcost_at-aedat LIKE ztco_shopcost_at-aedat,
            ztco_shopcost_at-aezet LIKE ztco_shopcost_at-aezet,
            ztco_shopcost_at-aenam LIKE ztco_shopcost_at-aenam,
      END OF %g00.
DATA: BEGIN OF %%g00,
            ztco_shopcost_at-kokrs(004),
            ztco_shopcost_at-bdatj(004),
            ztco_shopcost_at-poper(003),
            ztco_shopcost_at-aufnr(012),
            ztco_shopcost_at-versn(003),
            %fa00002(001),
            %fa00004(040),
            ztco_shopcost_at-shop(010),
            %fa00003(040),
            ztco_shopcost_at-typps(001),
            ztco_shopcost_at-kstar(010),
            ztco_shopcost_at-kalnr(012),
            ztco_shopcost_at-elemt(003),
            ztco_shopcost_at-kostl(010),
            ztco_shopcost_at-lstar(006),
            ztco_shopcost_at-hwaer(005),
            ztco_shopcost_at-meeht(003),
            ztco_shopcost_at-beskz(001),
            ztco_shopcost_at-sobsl(002),
            ztco_shopcost_at-vspvb(010),
            %fa00001(012),
            %fa00000(010),
            ztco_shopcost_at-mtart(004),
            ztco_shopcost_at-wkgbtr(021),
            ztco_shopcost_at-mbgbtr(020),
            ztco_shopcost_at-meeht-0203 LIKE ztco_shopcost_at-meeht,
            %fa00008(021),
            %fa00009(020),
            ztco_shopcost_at-meeht-0205 LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-wip_amt(021),
            %fa00005(018),
            ztco_shopcost_at-meeht-0207 LIKE ztco_shopcost_at-meeht,
            %fa00006(021),
            %fa00007(018),
            ztco_shopcost_at-meeht-0209 LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-preis(014),
            %fa00013(021),
            ztco_shopcost_at-manu_amt(021),
            ztco_shopcost_at-manu_qty(020),
            ztco_shopcost_at-meeht-0303 LIKE ztco_shopcost_at-meeht,
            ztco_shopcost_at-werks(004),
            ztco_shopcost_at-bwkey(004),
            ztco_shopcost_at-bwtar(010),
            ztco_shopcost_at-verid(004),
            ztco_shopcost_at-objnr(018),
            %fa00012(012),
            %fa00011(012),
            %fa00010(012),
            ztco_shopcost_at-erdat(010),
            ztco_shopcost_at-erzet(008),
            ztco_shopcost_at-ernam(012),
            ztco_shopcost_at-aedat(010),
            ztco_shopcost_at-aezet(008),
            ztco_shopcost_at-aenam(012),
      END OF %%g00.
DATA %znr TYPE i.
DATA %lznr TYPE i VALUE 99999.
FIELD-GROUPS header.
DATA %group01.
DATA %%ztco_shopcost_at-kokrs LIKE ztco_shopcost_at-kokrs.
DATA %%%ztco_shopcost_at-kokrs(1).
DATA %group0101.
DATA %group0201.
DATA %group0301.
DATA %group02.
DATA %%ztco_shopcost_at-bdatj LIKE ztco_shopcost_at-bdatj.
DATA %%%ztco_shopcost_at-bdatj(1).
DATA %group0102.
DATA %group0202.
DATA %group0302.
DATA %group03.
DATA %%ztco_shopcost_at-poper LIKE ztco_shopcost_at-poper.
DATA %%%ztco_shopcost_at-poper(1).
DATA %group0103.
DATA %group0203.
DATA %group0303.
DATA %group04.
DATA %%ztco_shopcost_at-aufnr LIKE ztco_shopcost_at-aufnr.
DATA %%%ztco_shopcost_at-aufnr(1).
DATA %group0104.
DATA %group0204.
DATA %group0304.
DATA %group05.
DATA %%ztco_shopcost_at-versn LIKE ztco_shopcost_at-versn.
DATA %%%ztco_shopcost_at-versn(1).
DATA %group0105.
DATA %group0205.
DATA %group0305.
DATA %group06.
DATA %%%fa00002 LIKE ztco_shopcost_at-record_type.
DATA %%%%fa00002(1).
DATA %group0106.
DATA %group0206.
DATA %group0306.
DATA %group07.
DATA %%%fa00004 LIKE ztco_shopcost_at-fsc_matnr.
DATA %%%%fa00004(1).
DATA %group0107.
DATA %group0207.
DATA %group0307.
DATA %group08.
DATA %%ztco_shopcost_at-shop LIKE ztco_shopcost_at-shop.
DATA %%%ztco_shopcost_at-shop(1).
DATA %group0108.
DATA %group0208.
DATA %group0308.
DATA %group09.
DATA %%%fa00003 LIKE ztco_shopcost_at-llv_matnr.
DATA %%%%fa00003(1).
DATA %group0109.
DATA %group0209.
DATA %group0309.
DATA %group10.
DATA %%ztco_shopcost_at-typps LIKE ztco_shopcost_at-typps.
DATA %%%ztco_shopcost_at-typps(1).
DATA %group0110.
DATA %group0210.
DATA %group0310.
FIELD-GROUPS %fg01.
DATA %any-01.
FIELD-GROUPS %fg02.
DATA %any-02.
DATA %ext-ztco_shopcost_at02.
FIELD-GROUPS %fg03.
DATA %any-03.
DATA %ext-ztco_shopcost_at03.
FIELD-GROUPS %fgwrztco_shopcost_at02.
FIELD-GROUPS %fgwrztco_shopcost_at03.

CONTROLS tview100 TYPE TABLEVIEW USING SCREEN 100.

AT SELECTION-SCREEN .
  PERFORM alvl_check(rsaqexce) USING %alvl 'G00'.
  PERFORM testmode(rsaqexce).
  PERFORM check_expcol(rsaqexce) USING %alv.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR %alvl .
  PERFORM alvl_value_request(rsaqexce) USING %alvl 'G00'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR %xintk .
  PERFORM xint_value_request(rsaqexce).

AT SELECTION-SCREEN OUTPUT .

  PERFORM rinit(rsaqbrst).
  PERFORM set_expcol(rsaqexce) USING %alv pb%exco.
  PERFORM alvl_set_invisible(rsaqexce).
  PERFORM set_xint_params(rsaqexce).

INITIALIZATION.
  PERFORM init_xint(rsaqexce).
  PERFORM set_www_flags(rsaqexce).
  PERFORM init_print_params(rsaqexce).

START-OF-SELECTION.
  INSERT ztco_shopcost_at-meeht INTO %fgwrztco_shopcost_at02.
  INSERT ztco_shopcost_at-wip_quantity INTO %fgwrztco_shopcost_at02.
  INSERT ztco_shopcost_at-actual_scrap INTO %fgwrztco_shopcost_at02.
  INSERT ztco_shopcost_at-add_mbgbtr INTO %fgwrztco_shopcost_at02.
  INSERT ztco_shopcost_at-meeht INTO %fgwrztco_shopcost_at03.
  INSERT ztco_shopcost_at-manu_qty INTO %fgwrztco_shopcost_at03.
  INSERT ztco_shopcost_at-mbgbtr INTO %fgwrztco_shopcost_at02.
  INSERT ztco_shopcost_at-hwaer INTO %fgwrztco_shopcost_at02.
  INSERT ztco_shopcost_at-wkgbtr INTO %fgwrztco_shopcost_at02.
  INSERT ztco_shopcost_at-hwaer INTO %fgwrztco_shopcost_at03.
  INSERT ztco_shopcost_at-ml_act_preis INTO %fgwrztco_shopcost_at03.
  INSERT ztco_shopcost_at-preis INTO %fgwrztco_shopcost_at02.
  INSERT ztco_shopcost_at-scrap_amt INTO %fgwrztco_shopcost_at02.
  INSERT ztco_shopcost_at-wip_amt INTO %fgwrztco_shopcost_at02.
  INSERT ztco_shopcost_at-manu_amt INTO %fgwrztco_shopcost_at03.
  INSERT ztco_shopcost_at-add_wkgbtr INTO %fgwrztco_shopcost_at02.
  INSERT ztco_shopcost_at-kokrs INTO header.
  INSERT ztco_shopcost_at-bdatj INTO header.
  INSERT ztco_shopcost_at-poper INTO header.
  INSERT ztco_shopcost_at-aufnr INTO header.
  INSERT ztco_shopcost_at-versn INTO header.
  INSERT ztco_shopcost_at-record_type INTO header.
  INSERT ztco_shopcost_at-fsc_matnr INTO header.
  INSERT ztco_shopcost_at-shop INTO header.
  INSERT ztco_shopcost_at-llv_matnr INTO header.
  INSERT ztco_shopcost_at-typps INTO header.
  INSERT %count-ztco_shopcost_at INTO header.
  INSERT %linr-ztco_shopcost_at INTO header.
  INSERT ztco_shopcost_at-kstar INTO %fg01.
  INSERT ztco_shopcost_at-kalnr INTO %fg01.
  INSERT ztco_shopcost_at-elemt INTO %fg01.
  INSERT ztco_shopcost_at-kostl INTO %fg01.
  INSERT ztco_shopcost_at-lstar INTO %fg01.
  INSERT ztco_shopcost_at-hwaer INTO %fg01.
  INSERT ztco_shopcost_at-meeht INTO %fg01.
  INSERT ztco_shopcost_at-beskz INTO %fg01.
  INSERT ztco_shopcost_at-sobsl INTO %fg01.
  INSERT ztco_shopcost_at-vspvb INTO %fg01.
  INSERT ztco_shopcost_at-par_kalnr INTO %fg01.
  INSERT ztco_shopcost_at-par_kadky INTO %fg01.
  INSERT ztco_shopcost_at-mtart INTO %fg02.
  INSERT ztco_shopcost_at-wkgbtr INTO %fg02.
  INSERT ztco_shopcost_at-hwaer INTO %fg02.
  INSERT ztco_shopcost_at-mbgbtr INTO %fg02.
  INSERT ztco_shopcost_at-meeht INTO %fg02.
  INSERT ztco_shopcost_at-add_wkgbtr INTO %fg02.
  INSERT ztco_shopcost_at-hwaer INTO %fg02.
  INSERT ztco_shopcost_at-add_mbgbtr INTO %fg02.
  INSERT ztco_shopcost_at-meeht INTO %fg02.
  INSERT ztco_shopcost_at-wip_amt INTO %fg02.
  INSERT ztco_shopcost_at-hwaer INTO %fg02.
  INSERT ztco_shopcost_at-wip_quantity INTO %fg02.
  INSERT ztco_shopcost_at-meeht INTO %fg02.
  INSERT ztco_shopcost_at-scrap_amt INTO %fg02.
  INSERT ztco_shopcost_at-hwaer INTO %fg02.
  INSERT ztco_shopcost_at-actual_scrap INTO %fg02.
  INSERT ztco_shopcost_at-meeht INTO %fg02.
  INSERT ztco_shopcost_at-preis INTO %fg02.
  INSERT ztco_shopcost_at-hwaer INTO %fg02.
  INSERT ztco_shopcost_at-ml_act_preis INTO %fg03.
  INSERT ztco_shopcost_at-hwaer INTO %fg03.
  INSERT ztco_shopcost_at-manu_amt INTO %fg03.
  INSERT ztco_shopcost_at-hwaer INTO %fg03.
  INSERT ztco_shopcost_at-manu_qty INTO %fg03.
  INSERT ztco_shopcost_at-meeht INTO %fg03.
  INSERT ztco_shopcost_at-werks INTO %fg03.
  INSERT ztco_shopcost_at-bwkey INTO %fg03.
  INSERT ztco_shopcost_at-bwtar INTO %fg03.
  INSERT ztco_shopcost_at-verid INTO %fg03.
  INSERT ztco_shopcost_at-objnr INTO %fg03.
  INSERT ztco_shopcost_at-chd_kalnr INTO %fg03.
  INSERT ztco_shopcost_at-par_proc_kalnr INTO %fg03.
  INSERT ztco_shopcost_at-chd_proc_kalnr INTO %fg03.
  INSERT ztco_shopcost_at-erdat INTO %fg03.
  INSERT ztco_shopcost_at-erzet INTO %fg03.
  INSERT ztco_shopcost_at-ernam INTO %fg03.
  INSERT ztco_shopcost_at-aedat INTO %fg03.
  INSERT ztco_shopcost_at-aezet INTO %fg03.
  INSERT ztco_shopcost_at-aenam INTO %fg03.
  PERFORM init_texthandling(rsaqexce) USING 'CL_TEXT_IDENTIFIER' ' '
          'SYSTQV000000000000000030'.
  PERFORM authority_begin(rsaqexce) USING 'CL_QUERY_TAB_ACCESS_AUTHORITY'.
  PERFORM authority(rsaqexce) USING 'ZTCO_SHOPCOST_AT'
                                    'CL_QUERY_TAB_ACCESS_AUTHORITY'.
  PERFORM authority_end(rsaqexce) USING 'CL_QUERY_TAB_ACCESS_AUTHORITY'.
  PERFORM %comp_ldesc.
SELECT actual_scrap add_mbgbtr add_wkgbtr aedat aenam aezet aufnr bdatj
         beskz bwkey bwtar chd_kalnr chd_proc_kalnr elemt erdat ernam
         erzet fsc_matnr hwaer kalnr kokrs kostl kstar llv_matnr lstar
         manu_amt manu_qty mbgbtr meeht ml_act_preis mtart objnr
         par_kadky par_kalnr par_proc_kalnr poper preis record_type
         scrap_amt shop sobsl typps verid versn vspvb werks wip_amt
         wip_quantity wkgbtr
         INTO CORRESPONDING FIELDS OF ztco_shopcost_at
         FROM ztco_shopcost_at
         WHERE aufnr IN sp$00004
           AND bdatj IN sp$00002
           AND elemt IN sp$00012
           AND fsc_matnr IN sp$00007
           AND kokrs IN sp$00001
           AND kostl IN sp$00013
           AND kstar IN sp$00011
           AND llv_matnr IN sp$00009
           AND lstar IN sp$00014
           AND poper IN sp$00003
           AND record_type IN sp$00006
           AND shop IN sp$00008
           AND typps IN sp$00010
           AND versn IN sp$00005.

    %dbacc = %dbacc - 1.
    IF %dbacc = 0.
      STOP.
    ENDIF.
    ADD 1 TO %count-ztco_shopcost_at.
    %linr-ztco_shopcost_at = '01'.
    EXTRACT %fg01.
    %linr-ztco_shopcost_at = '02'.
    EXTRACT %fg02.
    %ext-ztco_shopcost_at02 = 'X'.
    EXTRACT %fgwrztco_shopcost_at02.
    %linr-ztco_shopcost_at = '03'.
    EXTRACT %fg03.
    %ext-ztco_shopcost_at03 = 'X'.
    EXTRACT %fgwrztco_shopcost_at03.
  ENDSELECT.

END-OF-SELECTION.
  SORT AS TEXT BY
          ztco_shopcost_at-kokrs
          ztco_shopcost_at-bdatj
          ztco_shopcost_at-poper
          ztco_shopcost_at-aufnr
          ztco_shopcost_at-versn
          ztco_shopcost_at-record_type
          ztco_shopcost_at-fsc_matnr
          ztco_shopcost_at-shop
          ztco_shopcost_at-llv_matnr
          ztco_shopcost_at-typps
          %count-ztco_shopcost_at
          %linr-ztco_shopcost_at.
  %diact = space.
  %batch = sy-batch.
  IF %batch <> space.
    IF %eis <> space.
      %diact = 'E'.
      IF %eisprotocol = space.
        NEW-PAGE PRINT ON DESTINATION 'NULL' NO DIALOG
                 LINE-SIZE 0253 LINE-COUNT 0065.
      ELSE.
        NEW-PAGE PRINT ON NO DIALOG
                 PARAMETERS %init_pri_params.
      ENDIF.
    ENDIF.
    IF %alv <> space.
      %diact = 'V'.
      %alv_layout = %alvl.
      NEW-PAGE PRINT ON DESTINATION 'NULL' NO DIALOG
               LINE-SIZE 0253 LINE-COUNT 0065.
    ENDIF.
    IF %save <> space.
      %diact = 'S'.
      NEW-PAGE PRINT ON DESTINATION 'NULL' NO DIALOG
               LINE-SIZE 0253 LINE-COUNT 0065.
    ENDIF.
  ELSEIF %called_by_www <> space.
    %diact = space.
  ELSEIF %called_by_www_alv <> space.
    %diact = 'V'.
  ELSE.
    PERFORM init_print_params(rsaqexce).
    IF %save  <> space. %diact = 'S'. ENDIF.
    IF %xint  <> space. %diact = 'I'. ENDIF.
    IF %tview <> space. %diact = 'T'. ENDIF.
    IF %alv   <> space. %diact = 'V'. ENDIF.
    IF %down  <> space. %diact = 'D'. ENDIF.
    IF %eis   <> space. %diact = 'E'. ENDIF.
    IF %graph <> space. %diact = 'G'. ENDIF.
    IF %excel <> space. %diact = 'X'. ENDIF.
    IF %text  <> space. %diact = 'W'. ENDIF.
    IF %abc   <> space. %diact = 'A'. ENDIF.
    IF %diact <> space AND %diact <> 'S' AND %diact <> 'W'.
      NEW-PAGE PRINT ON DESTINATION 'NULL' NO DIALOG
               LINE-SIZE 0253 LINE-COUNT 0065.
    ENDIF.
    %pathname = %path.
    IF %diact = 'I'.
      %functionkey = %xintk.
    ENDIF.
    IF %diact = 'V'.
      %alv_layout = %alvl.
    ENDIF.
  ENDIF.
  FREE MEMORY ID 'AQLISTDATA'.
  IF %memmode <> space.
    IF %batch <> space.
      NEW-PAGE PRINT ON DESTINATION 'NULL' NO DIALOG
               LINE-SIZE 0253 LINE-COUNT 0065.
    ENDIF.
    %diact = '1'.
  ENDIF.
  %titel = ' '.
  IF sy-subty O %prflag AND %titel = space.
    NEW-PAGE WITH-TITLE.
  ENDIF.
  %tvsize = 0200.
  %pline = 1.
  %pzgr  = 1.
  %first = 'X'.
  PERFORM %output.
  %first = space.
  IF %diact <> space AND %diact <> 'S'.
    IF %batch = space.
      NEW-PAGE PRINT OFF.
      IF NOT ( %diact = 'V' AND %ucomm = 'PRIN' ).
        NEW-PAGE NO-HEADING NO-TITLE.
        WRITE space.
      ENDIF.
    ENDIF.
  ELSE.
    PERFORM pf-status(rsaqexce) USING 'XXX X '.
  ENDIF.
  CLEAR: %tab, %line, %cont.
  IF %data_selected = space.
    IF %diact = '1'.
      EXPORT empty FROM %empty TO MEMORY ID 'AQLISTDATA'.
      LEAVE.
    ELSE.
      IF %batch = space AND
         %called_by_www = space AND
         %called_by_www_alv = space.
        MESSAGE s260(aq).
        LEAVE LIST-PROCESSING.
      ELSE.
        IF %called_by_www_alv = space.
          %diact = space.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  IF %diact = 'S'.
    PERFORM %save_list.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF %diact = 'V' AND %batch <> space.
    NEW-PAGE PRINT OFF.
    PERFORM set_print_params(rsaqexce).
    PERFORM %download USING 'ALV'.
    LEAVE.
  ENDIF.
  IF %diact = 'V' AND %called_by_www_alv <> space.
    PERFORM %download USING 'ALV'.
    LEAVE.
  ENDIF.
  IF %diact = 'V' AND %ucomm = 'PRIN'.
    NEW-PAGE PRINT OFF.
    PERFORM set_print_params(rsaqexce).
    PERFORM %download USING 'ALV'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF %diact = 'P' AND %batch <> space.
    PERFORM %download USING '+DAT'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF %diact = 'E' AND %batch <> space.
    PERFORM %download USING 'EIS'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF %diact = '1'.
    PERFORM %download USING '+MEM'.
    LEAVE.
  ENDIF.
  IF %diact = 'X'.
    SET USER-COMMAND 'XXL'.
  ELSEIF %diact = 'W'.
    SET USER-COMMAND 'TEXT'.
  ELSEIF %diact = 'V'.
    SET USER-COMMAND 'ALV'.
  ELSEIF %diact = 'T'.
    SET USER-COMMAND 'VIEW'.
  ELSEIF %diact = 'G'.
    SET USER-COMMAND 'GRAF'.
  ELSEIF %diact = 'A'.
    SET USER-COMMAND 'ABCA'.
  ELSEIF %diact = 'E'.
    SET USER-COMMAND 'EIS'.
  ELSEIF %diact = 'D'.
    SET USER-COMMAND 'DOWN'.
  ELSEIF %diact = 'I'.
    SET USER-COMMAND 'XINT'.
  ELSEIF %diact = 'P'.
    SET USER-COMMAND '+DAT'.
  ENDIF.

TOP-OF-PAGE.
  PERFORM %top-of-page.

END-OF-PAGE.
  PERFORM page_foot(rsaqexce).
  PERFORM %save_page.

TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM %top-of-page.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'RETN'.
      PERFORM return(rsaqexce).
    WHEN 'CANC'.
      PERFORM return(rsaqexce).
    WHEN 'WEIT'.
      PERFORM return(rsaqexce).
    WHEN 'INHA'.
      PERFORM catalogue(rsaqexce).
    WHEN 'AUSL'.
      PERFORM pickup(rsaqexce).
    WHEN 'AUSW'.
      PERFORM pickup(rsaqexce).
    WHEN 'RCAA'.
      PERFORM rchain(rsaqbrst).
    WHEN 'RCAL'.
      PERFORM rcall(rsaqbrst).
    WHEN 'VGLI'.
      PERFORM change(rsaqexce).
    WHEN 'VGLE'.
      PERFORM change(rsaqexce).
    WHEN 'TOTO'.
      PERFORM change(rsaqexce).
    WHEN 'VSTA'.
      PERFORM change(rsaqexce).
    WHEN 'VSTE'.
      PERFORM return(rsaqexce).
    WHEN 'SAVL'.
      PERFORM %save_list.
    WHEN 'ODRU'.
      PERFORM print_list(rsaqexce).
    WHEN 'COPA'.
      PERFORM print_cover_page(rsaqexce).
    WHEN 'TEXT'.
      PERFORM %download USING 'TEXT'.
    WHEN 'ALV'.
      PERFORM %download USING 'ALV'.
    WHEN 'VIEW'.
      PERFORM %view.
    WHEN 'XXL'.
      PERFORM %download USING 'XXL'.
    WHEN 'GRAF'.
      PERFORM %download USING 'GRAF'.
    WHEN 'ABCA'.
      PERFORM %download USING 'ABCA'.
    WHEN 'EIS'.
      PERFORM %download USING 'EIS'.
    WHEN 'DOWN'.
      PERFORM %download USING 'DOWN'.
    WHEN 'XINT'.
      PERFORM %download USING 'XINT'.
  ENDCASE.
  CLEAR: %cline, %zgr.
  CLEAR: %tab, %line, %cont.
  IF %diact <> space.
    LEAVE LIST-PROCESSING.
  ENDIF.


*---------------------------------------------------------------------*
*       FORM %comp_ldesc                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %comp_ldesc.

  REFRESH %ldesc.
  REFRESH %gdesc.
  PERFORM ldesc(rsaqexce) USING 'G00010000X004       01  98'
    text-a00 text-b00 text-h00 'ZTCO_SHOPCOST_AT-KOKRS'
    ztco_shopcost_at-kokrs 'ZTCO_SHOPCOST_AT-KOKRS'.
  PERFORM ldesc(rsaqexce) USING 'G00020000X004       02  98'
    text-a01 text-b01 text-h00 'ZTCO_SHOPCOST_AT-BDATJ'
    ztco_shopcost_at-bdatj 'ZTCO_SHOPCOST_AT-BDATJ'.
  PERFORM ldesc(rsaqexce) USING 'G00030000X003       03  98'
    text-a02 text-b02 text-h00 'ZTCO_SHOPCOST_AT-POPER'
    ztco_shopcost_at-poper 'ZTCO_SHOPCOST_AT-POPER'.
  PERFORM ldesc(rsaqexce) USING 'G00040000X012       04  98'
    text-a03 text-b03 text-h00 'ZTCO_SHOPCOST_AT-AUFNR'
    ztco_shopcost_at-aufnr 'ZTCO_SHOPCOST_AT-AUFNR'.
  PERFORM ldesc(rsaqexce) USING 'G00050000X003       05  98'
    text-a04 text-b04 text-h00 'ZTCO_SHOPCOST_AT-VERSN'
    ztco_shopcost_at-versn 'ZTCO_SHOPCOST_AT-VERSN'.
  PERFORM ldesc(rsaqexce) USING 'G00060000X001       06  98'
    text-a05 text-b05 text-h00 'ZTCO_SHOPCOST_AT-RECORD_TYPE'
    ztco_shopcost_at-record_type '%FA00002'.
  PERFORM ldesc(rsaqexce) USING 'G00070000X040       07  98'
    text-a06 text-b06 text-h00 'ZTCO_SHOPCOST_AT-FSC_MATNR'
    ztco_shopcost_at-fsc_matnr '%FA00004'.
  PERFORM ldesc(rsaqexce) USING 'G00080000X010       08  98'
    text-a07 text-b07 text-h00 'ZTCO_SHOPCOST_AT-SHOP'
    ztco_shopcost_at-shop 'ZTCO_SHOPCOST_AT-SHOP'.
  PERFORM ldesc(rsaqexce) USING 'G00090000X040       09  98'
    text-a08 text-b08 text-h00 'ZTCO_SHOPCOST_AT-LLV_MATNR'
    ztco_shopcost_at-llv_matnr '%FA00003'.
  PERFORM ldesc(rsaqexce) USING 'G00100000X001       10  98'
    text-a09 text-b09 text-h00 'ZTCO_SHOPCOST_AT-TYPPS'
    ztco_shopcost_at-typps 'ZTCO_SHOPCOST_AT-TYPPS'.
  PERFORM ldesc(rsaqexce) USING 'G00110000X010       00  98'
    text-a10 text-b10 text-h00 'ZTCO_SHOPCOST_AT-KSTAR'
    ztco_shopcost_at-kstar 'ZTCO_SHOPCOST_AT-KSTAR'.
  PERFORM ldesc(rsaqexce) USING 'G00120000X012       00  98'
    text-a11 text-b11 text-h00 'ZTCO_SHOPCOST_AT-KALNR'
    ztco_shopcost_at-kalnr 'ZTCO_SHOPCOST_AT-KALNR'.
  PERFORM ldesc(rsaqexce) USING 'G00130000X003       00  98'
    text-a12 text-b12 text-h00 'ZTCO_SHOPCOST_AT-ELEMT'
    ztco_shopcost_at-elemt 'ZTCO_SHOPCOST_AT-ELEMT'.
  PERFORM ldesc(rsaqexce) USING 'G00140000X010       00  98'
    text-a13 text-b13 text-h00 'ZTCO_SHOPCOST_AT-KOSTL'
    ztco_shopcost_at-kostl 'ZTCO_SHOPCOST_AT-KOSTL'.
  PERFORM ldesc(rsaqexce) USING 'G00150000X006       00  98'
    text-a14 text-b14 text-h00 'ZTCO_SHOPCOST_AT-LSTAR'
    ztco_shopcost_at-lstar 'ZTCO_SHOPCOST_AT-LSTAR'.
  PERFORM ldesc(rsaqexce) USING 'G00160000X005       00  98'
    text-a15 text-b15 text-h00 'ZTCO_SHOPCOST_AT-HWAER'
    ztco_shopcost_at-hwaer 'ZTCO_SHOPCOST_AT-HWAER'.
  PERFORM ldesc(rsaqexce) USING 'G00170000X003       00  98'
    text-a16 text-b16 text-h00 'ZTCO_SHOPCOST_AT-MEEHT'
    ztco_shopcost_at-meeht 'ZTCO_SHOPCOST_AT-MEEHT'.
  PERFORM ldesc(rsaqexce) USING 'G00180000X001       00  98'
    text-a17 text-b17 text-h00 'ZTCO_SHOPCOST_AT-BESKZ'
    ztco_shopcost_at-beskz 'ZTCO_SHOPCOST_AT-BESKZ'.
  PERFORM ldesc(rsaqexce) USING 'G00190000X002       00  98'
    text-a18 text-b18 text-h00 'ZTCO_SHOPCOST_AT-SOBSL'
    ztco_shopcost_at-sobsl 'ZTCO_SHOPCOST_AT-SOBSL'.
  PERFORM ldesc(rsaqexce) USING 'G00200000X010       00  98'
    text-a19 text-b19 text-h00 'ZTCO_SHOPCOST_AT-VSPVB'
    ztco_shopcost_at-vspvb 'ZTCO_SHOPCOST_AT-VSPVB'.
  PERFORM ldesc(rsaqexce) USING 'G00210000X012       00  98'
    text-a20 text-b20 text-h00 'ZTCO_SHOPCOST_AT-PAR_KALNR'
    ztco_shopcost_at-par_kalnr '%FA00001'.
  PERFORM ldesc(rsaqexce) USING 'G00220000X010       00  98'
    text-a21 text-b21 text-h00 'ZTCO_SHOPCOST_AT-PAR_KADKY'
    ztco_shopcost_at-par_kadky '%FA00000'.
  PERFORM ldesc(rsaqexce) USING 'G00231000X004       00  98'
    text-a22 text-b22 text-h00 'ZTCO_SHOPCOST_AT-MTART'
    ztco_shopcost_at-mtart 'ZTCO_SHOPCOST_AT-MTART'.
  PERFORM ldesc(rsaqexce) USING 'G00241007 021     X 00  98'
    text-a23 text-b23 text-h00 'ZTCO_SHOPCOST_AT-WKGBTR'
    ztco_shopcost_at-wkgbtr 'ZTCO_SHOPCOST_AT-WKGBTR'.
  PERFORM ldesc(rsaqexce) USING 'G00251035 020M    X 00  98'
    text-a24 text-b24 text-h00 'ZTCO_SHOPCOST_AT-MBGBTR'
    ztco_shopcost_at-mbgbtr 'ZTCO_SHOPCOST_AT-MBGBTR'.
  PERFORM ldesc(rsaqexce) USING 'G00261000 003E      00  98'
    text-a25 text-b25 text-h00 'ZTCO_SHOPCOST_AT-MEEHT'
    ztco_shopcost_at-meeht 'ZTCO_SHOPCOST_AT-MEEHT-0203'.
  PERFORM ldesc(rsaqexce) USING 'G00271060 021     X 00  98'
    text-a26 text-b26 text-h00 'ZTCO_SHOPCOST_AT-ADD_WKGBTR'
    ztco_shopcost_at-add_wkgbtr '%FA00008'.
  PERFORM ldesc(rsaqexce) USING 'G00281088 020M    X 00  98'
    text-a27 text-b27 text-h00 'ZTCO_SHOPCOST_AT-ADD_MBGBTR'
    ztco_shopcost_at-add_mbgbtr '%FA00009'.
  PERFORM ldesc(rsaqexce) USING 'G00291000 003E      00  98'
    text-a28 text-b28 text-h00 'ZTCO_SHOPCOST_AT-MEEHT'
    ztco_shopcost_at-meeht 'ZTCO_SHOPCOST_AT-MEEHT-0205'.
  PERFORM ldesc(rsaqexce) USING 'G00301113 021     X 00  98'
    text-a29 text-b29 text-h00 'ZTCO_SHOPCOST_AT-WIP_AMT'
    ztco_shopcost_at-wip_amt 'ZTCO_SHOPCOST_AT-WIP_AMT'.
  PERFORM ldesc(rsaqexce) USING 'G00311141 018M    X 00  98'
    text-a30 text-b30 text-h00 'ZTCO_SHOPCOST_AT-WIP_QUANTITY'
    ztco_shopcost_at-wip_quantity '%FA00005'.
  PERFORM ldesc(rsaqexce) USING 'G00321000 003E      00  98'
    text-a31 text-b31 text-h00 'ZTCO_SHOPCOST_AT-MEEHT'
    ztco_shopcost_at-meeht 'ZTCO_SHOPCOST_AT-MEEHT-0207'.
  PERFORM ldesc(rsaqexce) USING 'G00331164 021     X 00  98'
    text-a32 text-b32 text-h00 'ZTCO_SHOPCOST_AT-SCRAP_AMT'
    ztco_shopcost_at-scrap_amt '%FA00006'.
  PERFORM ldesc(rsaqexce) USING 'G00341192 018M    X 00  98'
    text-a33 text-b33 text-h00 'ZTCO_SHOPCOST_AT-ACTUAL_SCRAP'
    ztco_shopcost_at-actual_scrap '%FA00007'.
  PERFORM ldesc(rsaqexce) USING 'G00351000 003E      00  98'
    text-a34 text-b34 text-h00 'ZTCO_SHOPCOST_AT-MEEHT'
    ztco_shopcost_at-meeht 'ZTCO_SHOPCOST_AT-MEEHT-0209'.
  PERFORM ldesc(rsaqexce) USING 'G00361215 014     X 00  98'
    text-a35 text-b35 text-h00 'ZTCO_SHOPCOST_AT-PREIS'
    ztco_shopcost_at-preis 'ZTCO_SHOPCOST_AT-PREIS'.
  PERFORM ldesc(rsaqexce) USING 'G00372002 021     X 00  98'
    text-a36 text-b36 text-h00 'ZTCO_SHOPCOST_AT-ML_ACT_PREIS'
    ztco_shopcost_at-ml_act_preis '%FA00013'.
  PERFORM ldesc(rsaqexce) USING 'G00382030 021     X 00  98'
    text-a37 text-b37 text-h00 'ZTCO_SHOPCOST_AT-MANU_AMT'
    ztco_shopcost_at-manu_amt 'ZTCO_SHOPCOST_AT-MANU_AMT'.
  PERFORM ldesc(rsaqexce) USING 'G00392058 020M    X 00  98'
    text-a38 text-b38 text-h00 'ZTCO_SHOPCOST_AT-MANU_QTY'
    ztco_shopcost_at-manu_qty 'ZTCO_SHOPCOST_AT-MANU_QTY'.
  PERFORM ldesc(rsaqexce) USING 'G00402000 003E      00  98'
    text-a39 text-b39 text-h00 'ZTCO_SHOPCOST_AT-MEEHT'
    ztco_shopcost_at-meeht 'ZTCO_SHOPCOST_AT-MEEHT-0303'.
  PERFORM ldesc(rsaqexce) USING 'G00412000X004       00  98'
    text-a40 text-b40 text-h00 'ZTCO_SHOPCOST_AT-WERKS'
    ztco_shopcost_at-werks 'ZTCO_SHOPCOST_AT-WERKS'.
  PERFORM ldesc(rsaqexce) USING 'G00422000X004       00  98'
    text-a41 text-b41 text-h00 'ZTCO_SHOPCOST_AT-BWKEY'
    ztco_shopcost_at-bwkey 'ZTCO_SHOPCOST_AT-BWKEY'.
  PERFORM ldesc(rsaqexce) USING 'G00432000X010       00  98'
    text-a42 text-b42 text-h00 'ZTCO_SHOPCOST_AT-BWTAR'
    ztco_shopcost_at-bwtar 'ZTCO_SHOPCOST_AT-BWTAR'.
  PERFORM ldesc(rsaqexce) USING 'G00442000X004       00  98'
    text-a43 text-b43 text-h00 'ZTCO_SHOPCOST_AT-VERID'
    ztco_shopcost_at-verid 'ZTCO_SHOPCOST_AT-VERID'.
  PERFORM ldesc(rsaqexce) USING 'G00452000X018       00  98'
    text-a44 text-b44 text-h00 'ZTCO_SHOPCOST_AT-OBJNR'
    ztco_shopcost_at-objnr 'ZTCO_SHOPCOST_AT-OBJNR'.
  PERFORM ldesc(rsaqexce) USING 'G00462000X012       00  98'
    text-a45 text-b45 text-h00 'ZTCO_SHOPCOST_AT-CHD_KALNR'
    ztco_shopcost_at-chd_kalnr '%FA00012'.
  PERFORM ldesc(rsaqexce) USING 'G00472000X012       00  98'
    text-a46 text-b46 text-h00 'ZTCO_SHOPCOST_AT-PAR_PROC_KALNR'
    ztco_shopcost_at-par_proc_kalnr '%FA00011'.
  PERFORM ldesc(rsaqexce) USING 'G00482000X012       00  98'
    text-a47 text-b47 text-h00 'ZTCO_SHOPCOST_AT-CHD_PROC_KALNR'
    ztco_shopcost_at-chd_proc_kalnr '%FA00010'.
  PERFORM ldesc(rsaqexce) USING 'G00492000X010       00  98'
    text-a48 text-b48 text-h00 'ZTCO_SHOPCOST_AT-ERDAT'
    ztco_shopcost_at-erdat 'ZTCO_SHOPCOST_AT-ERDAT'.
  PERFORM ldesc(rsaqexce) USING 'G00502000X008       00  98'
    text-a49 text-b49 text-h00 'ZTCO_SHOPCOST_AT-ERZET'
    ztco_shopcost_at-erzet 'ZTCO_SHOPCOST_AT-ERZET'.
  PERFORM ldesc(rsaqexce) USING 'G00512000X012       00  98'
    text-a50 text-b50 text-h00 'ZTCO_SHOPCOST_AT-ERNAM'
    ztco_shopcost_at-ernam 'ZTCO_SHOPCOST_AT-ERNAM'.
  PERFORM ldesc(rsaqexce) USING 'G00522000X010       00  98'
    text-a51 text-b51 text-h00 'ZTCO_SHOPCOST_AT-AEDAT'
    ztco_shopcost_at-aedat 'ZTCO_SHOPCOST_AT-AEDAT'.
  PERFORM ldesc(rsaqexce) USING 'G00532000X008       00  98'
    text-a52 text-b52 text-h00 'ZTCO_SHOPCOST_AT-AEZET'
    ztco_shopcost_at-aezet 'ZTCO_SHOPCOST_AT-AEZET'.
  PERFORM ldesc(rsaqexce) USING 'G00542000X012       00  98'
    text-a53 text-b53 text-h00 'ZTCO_SHOPCOST_AT-AENAM'
    ztco_shopcost_at-aenam 'ZTCO_SHOPCOST_AT-AENAM'.
  PERFORM gdesc(rsaqexce) USING 'G00' 5 20 ' ' ' ' 'X'.
  PERFORM complete_ldesc(rsaqexce) TABLES %ldesc.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %output                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %output.

  DESCRIBE TABLE %prlist LINES %max_prlist.
  %head = 'AAA'.
  %keyempty = space.
  NEW-PAGE.
  PERFORM %output_gl.
  PERFORM complete_page(rsaqexce).
  %head = 'ZZZ'.
  PERFORM last_ptab_entry(rsaqexce).
  NEW-PAGE.
  IF %keyempty <> space.
    MESSAGE s894(aq).
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM %top-of-page                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %top-of-page.

  IF sy-ucomm = 'INHA'. EXIT. ENDIF.
  IF sy-ucomm = 'COPA'. EXIT. ENDIF.
  IF %head    = space.  EXIT. ENDIF.
  IF %head = 'DDD'.
    PERFORM tviewpage(rsaqexce).
    EXIT.
  ENDIF.
  IF %head = 'GGG'.
    PERFORM page(rsaqexce) USING 'G00' text-grl 252 %glframe 001.
    SET LEFT SCROLL-BOUNDARY COLUMN 002.
    PERFORM set_scroll_boundary(rsaqexce) USING 002.
    IF %toto <> space. EXIT. ENDIF.
  ELSE.
    CASE %head.
    ENDCASE.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM %newline                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %newline.

  %uflag = space.
  NEW-LINE.
  WRITE: '|', 252 '|'.
  POSITION 2.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %skip                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  COUNT                                                         *
*---------------------------------------------------------------------*
FORM %skip USING count.

  IF sy-linno > 1.
    %uflag = space.
    DO count TIMES.
      NEW-LINE.
      FORMAT RESET.
      WRITE: '|', 252 '|'.
    ENDDO.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %uline                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %uline.

  IF %uflag = space.
    IF sy-linno > 1.
      ULINE /1(252).
    ENDIF.
    %uflag = 'X'.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %hide                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %hide.

  IF %batch <> space AND %diact = 'S'.
    PERFORM hide(rsaqexce).
  ELSE.
    HIDE: %tab, %line, %cont.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %hide_color                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %hide_color.

  IF %batch <> space AND %diact = 'S'.
    PERFORM hide_color(rsaqexce).
  ELSE.
    HIDE: %fint, %fcol.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %rcall                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  NAME                                                          *
*  -->  VALUE                                                         *
*---------------------------------------------------------------------*
FORM %rcall USING name value.

  FIELD-SYMBOLS <field>.

  ASSIGN (name) TO <field>.
  READ CURRENT LINE FIELD VALUE <field> INTO value.
  IF sy-subrc <> 0.
    value = space.
    EXIT.
  ENDIF.
  IF value = space AND %tab = 'G00' AND %ldesc-fcur NA 'FM'.
    READ TABLE %g00 INDEX %line.
    IF sy-subrc = 0.
      ASSIGN COMPONENT %ldesc-fnameint OF STRUCTURE %g00
                                       TO <field>.
      IF sy-subrc = 0.
        WRITE <field> TO value(%ldesc-folen).
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %save_page                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %save_page.

  IF %batch <> space AND %diact = 'S'.
    PERFORM save_page(rsaqexce) TABLES %st_liste.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %replace_var                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TEXT                                                          *
*---------------------------------------------------------------------*
FORM %replace_var USING text.

  FIELD-SYMBOLS <var>.

  ASSIGN text+1(*) TO <var>.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %save_list                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %save_list.

  DATA: %sflag,
        qreport LIKE sy-repid.

  IF %diact = 'S'. %sflag = 'X'. ENDIF.
  qreport = sy-repid.
  PERFORM save_list(rsaqexce) TABLES %st_liste
                              USING qreport %sflag %listid.
  IF %ql_id <> space.
    %dlflag = 'X'.
    %listsize = 0253.
    PERFORM comp_selection_screen(rsaqexce).
    EXPORT %st_liste %ptab %ldesc %gdesc %dlflag %listsize
           %selections
           %g00
           TO DATABASE aqldb(aq) ID %ql_id.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %refresh                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %refresh.

  CASE %tab.
    WHEN 'G00'.
      IMPORT %g00 FROM DATABASE aqldb(aq) ID %ql_id.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %download                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  CODE                                                          *
*---------------------------------------------------------------------*
FORM %download USING code.

  DATA: qreport LIKE sy-repid.

  PERFORM init_download(rsaqexce).
  qreport = sy-repid.
  CASE %tab.
    WHEN 'G00'.
      PERFORM download(rsaqexce)
              TABLES %g00 USING code qreport text-grl.
    WHEN OTHERS.
      MESSAGE s860(aq).
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %set_data                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  L_LINES                                                       *
*---------------------------------------------------------------------*
FORM %set_data CHANGING l_lines TYPE i.

  IMPORT ldata TO %g00 FROM MEMORY ID 'AQLISTDATA'.
  DESCRIBE TABLE %g00 LINES l_lines.
  FREE MEMORY ID 'AQLISTDATA'.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %get_data                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  DATATAB                                                       *
*  -->  FIRST                                                         *
*  -->  LAST                                                          *
*---------------------------------------------------------------------*
FORM %get_data TABLES datatab STRUCTURE %g00
               USING  first TYPE i
                      last  TYPE i.

  APPEND LINES OF %g00 FROM first TO last TO datatab.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %get_ref_to_table                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  LID                                                           *
*  -->  REF_TO_ITAB                                                   *
*---------------------------------------------------------------------*
FORM %get_ref_to_table USING lid         LIKE rsaqldesc-lid
                             ref_to_itab TYPE REF TO data
                             subrc       LIKE sy-subrc.

  subrc = 0.
  CASE lid.
    WHEN 'G00'.
      CREATE DATA ref_to_itab LIKE %g00[].
    WHEN OTHERS.
      subrc = 4.
      MESSAGE s860(aq).
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM %view                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %view.

  DATA: ret TYPE i.

  PERFORM check_wingui(rsaqsyst) USING ret.
  IF ret <> 0.
    MESSAGE s841(aq).
    PERFORM %download USING 'ALV'.
    EXIT.
  ENDIF.

  DATA: anz TYPE i,
        prog LIKE sy-repid.

  prog = sy-repid.
  PERFORM init_download(rsaqexce).
  CASE %tab.
    WHEN 'G00'.
      PERFORM generate_view_dynpro(rsaqexce)
              USING prog text-grl.
      DESCRIBE TABLE %g00 LINES anz.
      tview100-lines = anz.
      PERFORM init_view(rsaqexce) TABLES %g00 USING tview100.
      CALL SCREEN 100.
      PERFORM reset_view_dynpro(rsaqexce).
    WHEN OTHERS.
      MESSAGE s860(aq).
  ENDCASE.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM %clear02                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %clear02.

  CLEAR %g00-ztco_shopcost_at-mtart.
  CLEAR %g00-ztco_shopcost_at-wkgbtr.
  CLEAR %g00-ztco_shopcost_at-mbgbtr.
  CLEAR %g00-ztco_shopcost_at-meeht-0203.
  CLEAR %g00-%fa00008.
  CLEAR %g00-%fa00009.
  CLEAR %g00-ztco_shopcost_at-meeht-0205.
  CLEAR %g00-ztco_shopcost_at-wip_amt.
  CLEAR %g00-%fa00005.
  CLEAR %g00-ztco_shopcost_at-meeht-0207.
  CLEAR %g00-%fa00006.
  CLEAR %g00-%fa00007.
  CLEAR %g00-ztco_shopcost_at-meeht-0209.
  CLEAR %g00-ztco_shopcost_at-preis.
  PERFORM %clear03.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM %clear03                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %clear03.

  CLEAR %g00-%fa00013.
  CLEAR %g00-ztco_shopcost_at-manu_amt.
  CLEAR %g00-ztco_shopcost_at-manu_qty.
  CLEAR %g00-ztco_shopcost_at-meeht-0303.
  CLEAR %g00-ztco_shopcost_at-werks.
  CLEAR %g00-ztco_shopcost_at-bwkey.
  CLEAR %g00-ztco_shopcost_at-bwtar.
  CLEAR %g00-ztco_shopcost_at-verid.
  CLEAR %g00-ztco_shopcost_at-objnr.
  CLEAR %g00-%fa00012.
  CLEAR %g00-%fa00011.
  CLEAR %g00-%fa00010.
  CLEAR %g00-ztco_shopcost_at-erdat.
  CLEAR %g00-ztco_shopcost_at-erzet.
  CLEAR %g00-ztco_shopcost_at-ernam.
  CLEAR %g00-ztco_shopcost_at-aedat.
  CLEAR %g00-ztco_shopcost_at-aezet.
  CLEAR %g00-ztco_shopcost_at-aenam.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM %output_gl                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM %output_gl.

  IF %max_prlist <> 0.
    READ TABLE %prlist WITH KEY tab = 'GGG'.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ENDIF.
  SET MARGIN 00.
  PERFORM complete_page(rsaqexce).
  %nochange = space.
  NEW-PAGE.
  REFRESH %wa020.
  REFRESH %wa030.
  REFRESH %w0100.
  REFRESH %w0100.
  REFRESH %w0100.
  REFRESH %w0100.
  REFRESH %w0100.
  REFRESH %w0100.
  REFRESH %w0100.
  REFRESH %w0104.
  REFRESH %w0104.
  REFRESH %w0104.
  REFRESH %w0104.
  REFRESH %w0104.
  REFRESH %w0104.
  REFRESH %w0104.
  REFRESH %w0102.
  REFRESH %w0102.
  REFRESH %w0102.
  REFRESH %w0102.
  REFRESH %w0102.
  REFRESH %w0102.
  REFRESH %w0102.
  REFRESH %w0107.
  REFRESH %w0107.
  REFRESH %w0107.
  REFRESH %w0107.
  REFRESH %w0107.
  REFRESH %w0107.
  REFRESH %w0107.
  REFRESH %w0101.
  REFRESH %w0101.
  REFRESH %w0101.
  REFRESH %w0101.
  REFRESH %w0101.
  REFRESH %w0101.
  REFRESH %w0101.
  REFRESH %w0109.
  REFRESH %w0109.
  REFRESH %w0109.
  REFRESH %w0109.
  REFRESH %w0109.
  REFRESH %w0109.
  REFRESH %w0109.
  REFRESH %w0103.
  REFRESH %w0103.
  REFRESH %w0103.
  REFRESH %w0103.
  REFRESH %w0103.
  REFRESH %w0103.
  REFRESH %w0103.
  REFRESH %w0106.
  REFRESH %w0106.
  REFRESH %w0106.
  REFRESH %w0106.
  REFRESH %w0106.
  REFRESH %w0106.
  REFRESH %w0106.
  REFRESH %w0108.
  REFRESH %w0108.
  REFRESH %w0108.
  REFRESH %w0108.
  REFRESH %w0108.
  REFRESH %w0108.
  REFRESH %w0108.
  REFRESH %w0110.
  REFRESH %w0110.
  REFRESH %w0110.
  REFRESH %w0110.
  REFRESH %w0110.
  REFRESH %w0110.
  REFRESH %w0110.
  REFRESH %w0105.
  REFRESH %w0105.
  REFRESH %w0105.
  REFRESH %w0105.
  REFRESH %w0105.
  REFRESH %w0105.
  REFRESH %w0105.
  REFRESH %w0200.
  REFRESH %w0200.
  REFRESH %w0200.
  REFRESH %w0200.
  REFRESH %w0200.
  REFRESH %w0204.
  REFRESH %w0204.
  REFRESH %w0204.
  REFRESH %w0204.
  REFRESH %w0204.
  REFRESH %w0202.
  REFRESH %w0202.
  REFRESH %w0202.
  REFRESH %w0202.
  REFRESH %w0202.
  REFRESH %w0207.
  REFRESH %w0207.
  REFRESH %w0207.
  REFRESH %w0207.
  REFRESH %w0207.
  REFRESH %w0201.
  REFRESH %w0201.
  REFRESH %w0201.
  REFRESH %w0201.
  REFRESH %w0201.
  REFRESH %w0209.
  REFRESH %w0209.
  REFRESH %w0209.
  REFRESH %w0209.
  REFRESH %w0209.
  REFRESH %w0203.
  REFRESH %w0203.
  REFRESH %w0203.
  REFRESH %w0203.
  REFRESH %w0203.
  REFRESH %w0206.
  REFRESH %w0206.
  REFRESH %w0206.
  REFRESH %w0206.
  REFRESH %w0206.
  REFRESH %w0208.
  REFRESH %w0208.
  REFRESH %w0208.
  REFRESH %w0208.
  REFRESH %w0208.
  REFRESH %w0210.
  REFRESH %w0210.
  REFRESH %w0210.
  REFRESH %w0210.
  REFRESH %w0210.
  REFRESH %w0205.
  REFRESH %w0205.
  REFRESH %w0205.
  REFRESH %w0205.
  REFRESH %w0205.
  %glline   = 0.
  %tab      = 'G00'.
  %line     = 0.
  %cont     = '0'.
  %fint     = space.
  %fcol     = '0'.
  %head     = 'GGG'.
  %cline    = 0.
  %outflag  = space.
  %outcomp  = space.
  %outtotal = space.
  %rflag    = 'AA'.
  IF %diact <> space AND %diact NA 'SWE'. WRITE space. ENDIF.
  FORMAT RESET.
  LOOP.
    %data_selected = 'X'.
    AT %fg01.
      %znr = '01'.
      %zgr = '01'.
      %cline = %cline + 1.
      PERFORM %clear02.
      %g00-ztco_shopcost_at-kokrs = ztco_shopcost_at-kokrs.
      %g00-ztco_shopcost_at-bdatj = ztco_shopcost_at-bdatj.
      %g00-ztco_shopcost_at-poper = ztco_shopcost_at-poper.
      %g00-ztco_shopcost_at-aufnr = ztco_shopcost_at-aufnr.
      %g00-ztco_shopcost_at-versn = ztco_shopcost_at-versn.
      %g00-%fa00002 = ztco_shopcost_at-record_type.
      %g00-%fa00004 = ztco_shopcost_at-fsc_matnr.
      %g00-ztco_shopcost_at-shop = ztco_shopcost_at-shop.
      %g00-%fa00003 = ztco_shopcost_at-llv_matnr.
      %g00-ztco_shopcost_at-typps = ztco_shopcost_at-typps.
      %g00-ztco_shopcost_at-kstar = ztco_shopcost_at-kstar.
      %g00-ztco_shopcost_at-kalnr = ztco_shopcost_at-kalnr.
      %g00-ztco_shopcost_at-elemt = ztco_shopcost_at-elemt.
      %g00-ztco_shopcost_at-kostl = ztco_shopcost_at-kostl.
      %g00-ztco_shopcost_at-lstar = ztco_shopcost_at-lstar.
      %g00-ztco_shopcost_at-hwaer = ztco_shopcost_at-hwaer.
      %g00-ztco_shopcost_at-meeht = ztco_shopcost_at-meeht.
      %g00-ztco_shopcost_at-beskz = ztco_shopcost_at-beskz.
      %g00-ztco_shopcost_at-sobsl = ztco_shopcost_at-sobsl.
      %g00-ztco_shopcost_at-vspvb = ztco_shopcost_at-vspvb.
      %g00-%fa00001 = ztco_shopcost_at-par_kalnr.
      %g00-%fa00000 = ztco_shopcost_at-par_kadky.
      IF %first <> space. APPEND %g00. ENDIF.
      %glline = %glline + 1.
      %lznr = %znr.
      IF %diact <> space AND %diact NA 'SWE'. CONTINUE. ENDIF.
      PERFORM check(rsaqexce) USING ' '.
      IF %rflag = 'E'. EXIT. ENDIF.
      IF %rflag = 'AA'.
        PERFORM reserve(rsaqexce) USING 003.
      ENDIF.
      IF ztco_shopcost_at-kokrs <> %%ztco_shopcost_at-kokrs OR
      %%%ztco_shopcost_at-kokrs = space.
        %%ztco_shopcost_at-kokrs = ztco_shopcost_at-kokrs.
        %%%ztco_shopcost_at-kokrs ='X'.
        CLEAR %%ztco_shopcost_at-bdatj.
        CLEAR %%%ztco_shopcost_at-bdatj.
        CLEAR %%ztco_shopcost_at-poper.
        CLEAR %%%ztco_shopcost_at-poper.
        CLEAR %%ztco_shopcost_at-aufnr.
        CLEAR %%%ztco_shopcost_at-aufnr.
        CLEAR %%ztco_shopcost_at-versn.
        CLEAR %%%ztco_shopcost_at-versn.
        CLEAR %%%fa00002.
        CLEAR %%%%fa00002.
        CLEAR %%%fa00004.
        CLEAR %%%%fa00004.
        CLEAR %%ztco_shopcost_at-shop.
        CLEAR %%%ztco_shopcost_at-shop.
        CLEAR %%%fa00003.
        CLEAR %%%%fa00003.
        CLEAR %%ztco_shopcost_at-typps.
        CLEAR %%%ztco_shopcost_at-typps.
      ENDIF.
      IF ztco_shopcost_at-bdatj <> %%ztco_shopcost_at-bdatj OR
      %%%ztco_shopcost_at-bdatj = space.
        %%ztco_shopcost_at-bdatj = ztco_shopcost_at-bdatj.
        %%%ztco_shopcost_at-bdatj ='X'.
        CLEAR %%ztco_shopcost_at-poper.
        CLEAR %%%ztco_shopcost_at-poper.
        CLEAR %%ztco_shopcost_at-aufnr.
        CLEAR %%%ztco_shopcost_at-aufnr.
        CLEAR %%ztco_shopcost_at-versn.
        CLEAR %%%ztco_shopcost_at-versn.
        CLEAR %%%fa00002.
        CLEAR %%%%fa00002.
        CLEAR %%%fa00004.
        CLEAR %%%%fa00004.
        CLEAR %%ztco_shopcost_at-shop.
        CLEAR %%%ztco_shopcost_at-shop.
        CLEAR %%%fa00003.
        CLEAR %%%%fa00003.
        CLEAR %%ztco_shopcost_at-typps.
        CLEAR %%%ztco_shopcost_at-typps.
      ENDIF.
      IF ztco_shopcost_at-poper <> %%ztco_shopcost_at-poper OR
      %%%ztco_shopcost_at-poper = space.
        %%ztco_shopcost_at-poper = ztco_shopcost_at-poper.
        %%%ztco_shopcost_at-poper ='X'.
        CLEAR %%ztco_shopcost_at-aufnr.
        CLEAR %%%ztco_shopcost_at-aufnr.
        CLEAR %%ztco_shopcost_at-versn.
        CLEAR %%%ztco_shopcost_at-versn.
        CLEAR %%%fa00002.
        CLEAR %%%%fa00002.
        CLEAR %%%fa00004.
        CLEAR %%%%fa00004.
        CLEAR %%ztco_shopcost_at-shop.
        CLEAR %%%ztco_shopcost_at-shop.
        CLEAR %%%fa00003.
        CLEAR %%%%fa00003.
        CLEAR %%ztco_shopcost_at-typps.
        CLEAR %%%ztco_shopcost_at-typps.
      ENDIF.
      IF ztco_shopcost_at-aufnr <> %%ztco_shopcost_at-aufnr OR
      %%%ztco_shopcost_at-aufnr = space.
        %%ztco_shopcost_at-aufnr = ztco_shopcost_at-aufnr.
        %%%ztco_shopcost_at-aufnr ='X'.
        CLEAR %%ztco_shopcost_at-versn.
        CLEAR %%%ztco_shopcost_at-versn.
        CLEAR %%%fa00002.
        CLEAR %%%%fa00002.
        CLEAR %%%fa00004.
        CLEAR %%%%fa00004.
        CLEAR %%ztco_shopcost_at-shop.
        CLEAR %%%ztco_shopcost_at-shop.
        CLEAR %%%fa00003.
        CLEAR %%%%fa00003.
        CLEAR %%ztco_shopcost_at-typps.
        CLEAR %%%ztco_shopcost_at-typps.
      ENDIF.
      IF ztco_shopcost_at-versn <> %%ztco_shopcost_at-versn OR
      %%%ztco_shopcost_at-versn = space.
        %%ztco_shopcost_at-versn = ztco_shopcost_at-versn.
        %%%ztco_shopcost_at-versn ='X'.
        CLEAR %%%fa00002.
        CLEAR %%%%fa00002.
        CLEAR %%%fa00004.
        CLEAR %%%%fa00004.
        CLEAR %%ztco_shopcost_at-shop.
        CLEAR %%%ztco_shopcost_at-shop.
        CLEAR %%%fa00003.
        CLEAR %%%%fa00003.
        CLEAR %%ztco_shopcost_at-typps.
        CLEAR %%%ztco_shopcost_at-typps.
      ENDIF.
   IF ztco_shopcost_at-record_type <> %%%fa00002 OR %%%%fa00002 = space.
        %%%fa00002 = ztco_shopcost_at-record_type.
        %%%%fa00002 ='X'.
        CLEAR %%%fa00004.
        CLEAR %%%%fa00004.
        CLEAR %%ztco_shopcost_at-shop.
        CLEAR %%%ztco_shopcost_at-shop.
        CLEAR %%%fa00003.
        CLEAR %%%%fa00003.
        CLEAR %%ztco_shopcost_at-typps.
        CLEAR %%%ztco_shopcost_at-typps.
      ENDIF.
     IF ztco_shopcost_at-fsc_matnr <> %%%fa00004 OR %%%%fa00004 = space.
        %%%fa00004 = ztco_shopcost_at-fsc_matnr.
        %%%%fa00004 ='X'.
        CLEAR %%ztco_shopcost_at-shop.
        CLEAR %%%ztco_shopcost_at-shop.
        CLEAR %%%fa00003.
        CLEAR %%%%fa00003.
        CLEAR %%ztco_shopcost_at-typps.
        CLEAR %%%ztco_shopcost_at-typps.
      ENDIF.
      IF ztco_shopcost_at-shop <> %%ztco_shopcost_at-shop OR
      %%%ztco_shopcost_at-shop = space.
        %%ztco_shopcost_at-shop = ztco_shopcost_at-shop.
        %%%ztco_shopcost_at-shop ='X'.
        CLEAR %%%fa00003.
        CLEAR %%%%fa00003.
        CLEAR %%ztco_shopcost_at-typps.
        CLEAR %%%ztco_shopcost_at-typps.
      ENDIF.
     IF ztco_shopcost_at-llv_matnr <> %%%fa00003 OR %%%%fa00003 = space.
        %%%fa00003 = ztco_shopcost_at-llv_matnr.
        %%%%fa00003 ='X'.
        CLEAR %%ztco_shopcost_at-typps.
        CLEAR %%%ztco_shopcost_at-typps.
      ENDIF.
      IF ztco_shopcost_at-typps <> %%ztco_shopcost_at-typps OR
      %%%ztco_shopcost_at-typps = space.
        %%ztco_shopcost_at-typps = ztco_shopcost_at-typps.
        %%%ztco_shopcost_at-typps ='X'.
      ENDIF.
      IF %rflag(1) = 'A'.
        FORMAT RESET.
        %fint = 'F'. %fcol = '0'.
        FORMAT COLOR 2. %fcol = '2'.
        PERFORM %newline.
        WRITE 002(004) ztco_shopcost_at-kokrs.
        %line = %glline.
        PERFORM %hide.
        %line = 0.
        IF %rflag = 'AA'. HIDE %cline. ENDIF.
        HIDE %zgr.
        PERFORM %hide_color.
        WRITE 007(004) ztco_shopcost_at-bdatj.
        WRITE 012(003) ztco_shopcost_at-poper.
        WRITE 016(012) ztco_shopcost_at-aufnr.
        WRITE 029(003) ztco_shopcost_at-versn.
        WRITE 033(001) ztco_shopcost_at-record_type.
        WRITE 035(040) ztco_shopcost_at-fsc_matnr.
        WRITE 076(010) ztco_shopcost_at-shop.
        WRITE 087(040) ztco_shopcost_at-llv_matnr.
        WRITE 128(001) ztco_shopcost_at-typps.
        WRITE 130(010) ztco_shopcost_at-kstar.
        WRITE 141(012) ztco_shopcost_at-kalnr.
        WRITE 154(003) ztco_shopcost_at-elemt.
        WRITE 158(010) ztco_shopcost_at-kostl.
        WRITE 169(006) ztco_shopcost_at-lstar.
        WRITE 176(005) ztco_shopcost_at-hwaer.
        WRITE 182(003) ztco_shopcost_at-meeht.
        WRITE 186(001) ztco_shopcost_at-beskz.
        WRITE 188(002) ztco_shopcost_at-sobsl.
        WRITE 191(010) ztco_shopcost_at-vspvb.
        WRITE 202(012) ztco_shopcost_at-par_kalnr.
        WRITE 215(010) ztco_shopcost_at-par_kadky.
      ENDIF.
    ENDAT.
    AT %fg02.
      %znr = '02'.
      %zgr = '01'.
      IF %znr > %lznr.
        READ TABLE %g00 INDEX %glline.
      ENDIF.
      PERFORM %clear03.
      %g00-ztco_shopcost_at-mtart = ztco_shopcost_at-mtart.
      %g00-ztco_shopcost_at-wkgbtr = ztco_shopcost_at-wkgbtr.
      %g00-ztco_shopcost_at-mbgbtr = ztco_shopcost_at-mbgbtr.
      %g00-ztco_shopcost_at-meeht-0203 = ztco_shopcost_at-meeht.
      %g00-%fa00008 = ztco_shopcost_at-add_wkgbtr.
      %g00-%fa00009 = ztco_shopcost_at-add_mbgbtr.
      %g00-ztco_shopcost_at-meeht-0205 = ztco_shopcost_at-meeht.
      %g00-ztco_shopcost_at-wip_amt = ztco_shopcost_at-wip_amt.
      %g00-%fa00005 = ztco_shopcost_at-wip_quantity.
      %g00-ztco_shopcost_at-meeht-0207 = ztco_shopcost_at-meeht.
      %g00-%fa00006 = ztco_shopcost_at-scrap_amt.
      %g00-%fa00007 = ztco_shopcost_at-actual_scrap.
      %g00-ztco_shopcost_at-meeht-0209 = ztco_shopcost_at-meeht.
      %g00-ztco_shopcost_at-preis = ztco_shopcost_at-preis.
      IF %znr > %lznr.
        IF %first <> space. MODIFY %g00 INDEX %glline. ENDIF.
      ELSE.
        IF %first <> space. APPEND %g00. ENDIF.
        %glline = %glline + 1.
      ENDIF.
      %lznr = %znr.
      IF %diact <> space AND %diact NA 'SWE'. CONTINUE. ENDIF.
      PERFORM check(rsaqexce) USING ' '.
      IF %rflag = 'E'. EXIT. ENDIF.
      IF %rflag(1) = 'A'.
        FORMAT RESET.
        %fint = 'F'. %fcol = '0'.
        FORMAT COLOR 2. %fcol = '2'.
        PERFORM %newline.
        WRITE 002(004) ztco_shopcost_at-mtart.
        %line = %glline.  %cont = '1'.
        PERFORM %hide.
        %line = 0.  %cont = '0'.
        IF %rflag = 'AA'. HIDE %cline. ENDIF.
        HIDE %zgr.
        PERFORM %hide_color.
        WRITE 007(021) ztco_shopcost_at-wkgbtr
          CURRENCY ztco_shopcost_at-hwaer.
        WRITE 035(020) ztco_shopcost_at-mbgbtr
          UNIT ztco_shopcost_at-meeht.
        WRITE 056(003) ztco_shopcost_at-meeht.
        WRITE 060(021) ztco_shopcost_at-add_wkgbtr
          CURRENCY ztco_shopcost_at-hwaer.
        WRITE 088(020) ztco_shopcost_at-add_mbgbtr
          UNIT ztco_shopcost_at-meeht.
        WRITE 109(003) ztco_shopcost_at-meeht.
        WRITE 113(021) ztco_shopcost_at-wip_amt
          CURRENCY ztco_shopcost_at-hwaer.
        WRITE 141(018) ztco_shopcost_at-wip_quantity
          UNIT ztco_shopcost_at-meeht.
        WRITE 160(003) ztco_shopcost_at-meeht.
        WRITE 164(021) ztco_shopcost_at-scrap_amt
          CURRENCY ztco_shopcost_at-hwaer.
        WRITE 192(018) ztco_shopcost_at-actual_scrap
          UNIT ztco_shopcost_at-meeht.
        WRITE 211(003) ztco_shopcost_at-meeht.
        WRITE 215(014) ztco_shopcost_at-preis
          CURRENCY ztco_shopcost_at-hwaer.
      ENDIF.
    ENDAT.
    AT %fg03.
      %znr = '03'.
      %zgr = '01'.
      IF %znr > %lznr.
        READ TABLE %g00 INDEX %glline.
      ENDIF.
      %g00-%fa00013 = ztco_shopcost_at-ml_act_preis.
      %g00-ztco_shopcost_at-manu_amt = ztco_shopcost_at-manu_amt.
      %g00-ztco_shopcost_at-manu_qty = ztco_shopcost_at-manu_qty.
      %g00-ztco_shopcost_at-meeht-0303 = ztco_shopcost_at-meeht.
      %g00-ztco_shopcost_at-werks = ztco_shopcost_at-werks.
      %g00-ztco_shopcost_at-bwkey = ztco_shopcost_at-bwkey.
      %g00-ztco_shopcost_at-bwtar = ztco_shopcost_at-bwtar.
      %g00-ztco_shopcost_at-verid = ztco_shopcost_at-verid.
      %g00-ztco_shopcost_at-objnr = ztco_shopcost_at-objnr.
      %g00-%fa00012 = ztco_shopcost_at-chd_kalnr.
      %g00-%fa00011 = ztco_shopcost_at-par_proc_kalnr.
      %g00-%fa00010 = ztco_shopcost_at-chd_proc_kalnr.
      %g00-ztco_shopcost_at-erdat = ztco_shopcost_at-erdat.
      %g00-ztco_shopcost_at-erzet = ztco_shopcost_at-erzet.
      %g00-ztco_shopcost_at-ernam = ztco_shopcost_at-ernam.
      %g00-ztco_shopcost_at-aedat = ztco_shopcost_at-aedat.
      %g00-ztco_shopcost_at-aezet = ztco_shopcost_at-aezet.
      %g00-ztco_shopcost_at-aenam = ztco_shopcost_at-aenam.
      IF %znr > %lznr.
        IF %first <> space. MODIFY %g00 INDEX %glline. ENDIF.
      ELSE.
        IF %first <> space. APPEND %g00. ENDIF.
        %glline = %glline + 1.
      ENDIF.
      %lznr = %znr.
      IF %diact <> space AND %diact NA 'SWE'. CONTINUE. ENDIF.
      PERFORM check(rsaqexce) USING ' '.
      IF %rflag = 'E'. EXIT. ENDIF.
      IF %rflag(1) = 'A'.
        FORMAT RESET.
        %fint = 'F'. %fcol = '0'.
        FORMAT COLOR 2. %fcol = '2'.
        PERFORM %newline.
        WRITE 002(021) ztco_shopcost_at-ml_act_preis
          CURRENCY ztco_shopcost_at-hwaer.
        %line = %glline.  %cont = '2'.
        PERFORM %hide.
        %line = 0.  %cont = '0'.
        IF %rflag = 'AA'. HIDE %cline. ENDIF.
        HIDE %zgr.
        PERFORM %hide_color.
        WRITE 030(021) ztco_shopcost_at-manu_amt
          CURRENCY ztco_shopcost_at-hwaer.
        WRITE 058(020) ztco_shopcost_at-manu_qty
          UNIT ztco_shopcost_at-meeht.
        WRITE 079(003) ztco_shopcost_at-meeht.
        WRITE 083(004) ztco_shopcost_at-werks.
        WRITE 088(004) ztco_shopcost_at-bwkey.
        WRITE 093(010) ztco_shopcost_at-bwtar.
        WRITE 104(004) ztco_shopcost_at-verid.
        WRITE 109(018) ztco_shopcost_at-objnr.
        WRITE 128(012) ztco_shopcost_at-chd_kalnr.
        WRITE 141(012) ztco_shopcost_at-par_proc_kalnr.
        WRITE 154(012) ztco_shopcost_at-chd_proc_kalnr.
        WRITE 167(010) ztco_shopcost_at-erdat.
        WRITE 178(008) ztco_shopcost_at-erzet.
        WRITE 187(012) ztco_shopcost_at-ernam.
        WRITE 200(010) ztco_shopcost_at-aedat.
        WRITE 211(008) ztco_shopcost_at-aezet.
        WRITE 220(012) ztco_shopcost_at-aenam.
      ENDIF.
    ENDAT.
    AT %fgwrztco_shopcost_at03.
      CLEAR %w0110.
      %w0110-ztco_shopcost_at-hwaer = ztco_shopcost_at-hwaer.
      %w0110-%fa00013 = ztco_shopcost_at-ml_act_preis.
      %w0110-ztco_shopcost_at-manu_amt = ztco_shopcost_at-manu_amt.
      COLLECT %w0110.
    ENDAT.
    AT %fgwrztco_shopcost_at02.
      CLEAR %w0110.
      %w0110-ztco_shopcost_at-hwaer = ztco_shopcost_at-hwaer.
      %w0110-ztco_shopcost_at-wkgbtr = ztco_shopcost_at-wkgbtr.
      %w0110-ztco_shopcost_at-preis = ztco_shopcost_at-preis.
      %w0110-%fa00006 = ztco_shopcost_at-scrap_amt.
      %w0110-ztco_shopcost_at-wip_amt = ztco_shopcost_at-wip_amt.
      %w0110-%fa00008 = ztco_shopcost_at-add_wkgbtr.
      COLLECT %w0110.
    ENDAT.
    AT %fgwrztco_shopcost_at02.
      CLEAR %w0210.
      %w0210-ztco_shopcost_at-meeht = ztco_shopcost_at-meeht.
      %w0210-%fa00005 = ztco_shopcost_at-wip_quantity.
      %w0210-%fa00007 = ztco_shopcost_at-actual_scrap.
      %w0210-%fa00009 = ztco_shopcost_at-add_mbgbtr.
      %w0210-ztco_shopcost_at-mbgbtr = ztco_shopcost_at-mbgbtr.
      COLLECT %w0210.
    ENDAT.
    AT %fgwrztco_shopcost_at03.
      CLEAR %w0210.
      %w0210-ztco_shopcost_at-meeht = ztco_shopcost_at-meeht.
      %w0210-ztco_shopcost_at-manu_qty = ztco_shopcost_at-manu_qty.
      COLLECT %w0210.
    ENDAT.
    AT END OF ztco_shopcost_at-typps.
      %zgr = '01'.
      PERFORM check(rsaqexce) USING 'X'.
      IF %rflag = 'E'. EXIT. ENDIF.
      LOOP AT %w0110.
        %w0109 = %w0110.
        COLLECT %w0109.
      ENDLOOP.
      REFRESH %w0110.
      LOOP AT %w0210.
        %w0209 = %w0210.
        COLLECT %w0209.
      ENDLOOP.
      REFRESH %w0210.
    ENDAT.
    AT END OF ztco_shopcost_at-llv_matnr.
      %zgr = '01'.
      PERFORM check(rsaqexce) USING 'X'.
      IF %rflag = 'E'. EXIT. ENDIF.
      LOOP AT %w0109.
        %w0108 = %w0109.
        COLLECT %w0108.
      ENDLOOP.
      REFRESH %w0109.
      LOOP AT %w0209.
        %w0208 = %w0209.
        COLLECT %w0208.
      ENDLOOP.
      REFRESH %w0209.
    ENDAT.
    AT END OF ztco_shopcost_at-shop.
      %zgr = '01'.
      PERFORM check(rsaqexce) USING 'X'.
      IF %rflag = 'E'. EXIT. ENDIF.
      LOOP AT %w0108.
        %w0107 = %w0108.
        COLLECT %w0107.
      ENDLOOP.
      REFRESH %w0108.
      LOOP AT %w0208.
        %w0207 = %w0208.
        COLLECT %w0207.
      ENDLOOP.
      REFRESH %w0208.
    ENDAT.
    AT END OF ztco_shopcost_at-fsc_matnr.
      %zgr = '01'.
      PERFORM check(rsaqexce) USING 'X'.
      IF %rflag = 'E'. EXIT. ENDIF.
      LOOP AT %w0107.
        %w0106 = %w0107.
        COLLECT %w0106.
      ENDLOOP.
      REFRESH %w0107.
      LOOP AT %w0207.
        %w0206 = %w0207.
        COLLECT %w0206.
      ENDLOOP.
      REFRESH %w0207.
    ENDAT.
    AT END OF ztco_shopcost_at-record_type.
      %zgr = '01'.
      PERFORM check(rsaqexce) USING 'X'.
      IF %rflag = 'E'. EXIT. ENDIF.
      LOOP AT %w0106.
        %w0105 = %w0106.
        COLLECT %w0105.
      ENDLOOP.
      REFRESH %w0106.
      LOOP AT %w0206.
        %w0205 = %w0206.
        COLLECT %w0205.
      ENDLOOP.
      REFRESH %w0206.
    ENDAT.
    AT END OF ztco_shopcost_at-versn.
      %zgr = '01'.
      PERFORM check(rsaqexce) USING 'X'.
      IF %rflag = 'E'. EXIT. ENDIF.
      LOOP AT %w0105.
        %w0104 = %w0105.
        COLLECT %w0104.
      ENDLOOP.
      REFRESH %w0105.
      LOOP AT %w0205.
        %w0204 = %w0205.
        COLLECT %w0204.
      ENDLOOP.
      REFRESH %w0205.
    ENDAT.
    AT END OF ztco_shopcost_at-aufnr.
      %zgr = '01'.
      PERFORM check(rsaqexce) USING 'X'.
      IF %rflag = 'E'. EXIT. ENDIF.
      LOOP AT %w0104.
        %w0103 = %w0104.
        COLLECT %w0103.
      ENDLOOP.
      REFRESH %w0104.
      LOOP AT %w0204.
        %w0203 = %w0204.
        COLLECT %w0203.
      ENDLOOP.
      REFRESH %w0204.
    ENDAT.
    AT END OF ztco_shopcost_at-poper.
      %zgr = '01'.
      PERFORM check(rsaqexce) USING 'X'.
      IF %rflag = 'E'. EXIT. ENDIF.
      LOOP AT %w0103.
        %w0102 = %w0103.
        COLLECT %w0102.
      ENDLOOP.
      REFRESH %w0103.
      LOOP AT %w0203.
        %w0202 = %w0203.
        COLLECT %w0202.
      ENDLOOP.
      REFRESH %w0203.
    ENDAT.
    AT END OF ztco_shopcost_at-bdatj.
      %zgr = '01'.
      PERFORM check(rsaqexce) USING 'X'.
      IF %rflag = 'E'. EXIT. ENDIF.
      LOOP AT %w0102.
        %w0101 = %w0102.
        COLLECT %w0101.
      ENDLOOP.
      REFRESH %w0102.
      LOOP AT %w0202.
        %w0201 = %w0202.
        COLLECT %w0201.
      ENDLOOP.
      REFRESH %w0202.
    ENDAT.
    AT END OF ztco_shopcost_at-kokrs.
      %zgr = '01'.
      PERFORM check(rsaqexce) USING 'X'.
      IF %rflag = 'E'. EXIT. ENDIF.
      LOOP AT %w0101.
        %w0100 = %w0101.
        COLLECT %w0100.
      ENDLOOP.
      REFRESH %w0101.
      LOOP AT %w0201.
        %w0200 = %w0201.
        COLLECT %w0200.
      ENDLOOP.
      REFRESH %w0201.
    ENDAT.
    AT LAST.
      %znr = 0.
      %rflag = 'AA'.
      %outtotal = 'X'.
      PERFORM reserve(rsaqexce) USING 2.
      PERFORM %uline.
      FORMAT RESET.
      FORMAT INTENSIFIED ON COLOR 3.
      %fint = 'N'. %fcol = '3'.
      %nochange = 'X'.
      PERFORM %newline.
      %nochange = space.
      WRITE (13) text-f02.
      PERFORM %hide.
      PERFORM %hide_color.
      SORT %w0100 BY ztco_shopcost_at-hwaer.
      SORT %w0200 BY ztco_shopcost_at-meeht.
      PERFORM %newline.
      WRITE 230 '*'.
      REFRESH %wa020.
      DO.
        %subrc = 4.
        CLEAR %wa020.
        READ TABLE %w0100 INDEX sy-index.
        IF sy-subrc = 0.
          %subrc = 0.
          %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer =
          %w0100-ztco_shopcost_at-hwaer.
        %wa020-ztco_shopcost_at-wkgbtr = %w0100-ztco_shopcost_at-wkgbtr.
          %wa020-ztco_shopcost_at-preis = %w0100-ztco_shopcost_at-preis.
          %wa020-%fa00006 = %w0100-%fa00006.
      %wa020-ztco_shopcost_at-wip_amt = %w0100-ztco_shopcost_at-wip_amt.
          %wa020-%fa00008 = %w0100-%fa00008.
        ENDIF.
        READ TABLE %w0200 INDEX sy-index.
        IF sy-subrc = 0.
          %subrc = 0.
          %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht =
          %w0200-ztco_shopcost_at-meeht.
          %wa020-%fa00005 = %w0200-%fa00005.
          %wa020-%fa00007 = %w0200-%fa00007.
          %wa020-%fa00009 = %w0200-%fa00009.
        %wa020-ztco_shopcost_at-mbgbtr = %w0200-ztco_shopcost_at-mbgbtr.
        ENDIF.
        IF %subrc = 4.
          EXIT.
        ENDIF.
        APPEND %wa020.
      ENDDO.
      LOOP AT %wa020.
        IF sy-tabix <> 1.
          PERFORM %newline.
        ENDIF.
        IF %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer = space.
          WRITE 007(021) %wa020-ztco_shopcost_at-wkgbtr
CURRENCY %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer NO-ZERO.
          IF NOT %wa020-ztco_shopcost_at-wkgbtr IS INITIAL.
            %keyempty = 'X'.
          ENDIF.
        ELSE.
          WRITE 007(021) %wa020-ztco_shopcost_at-wkgbtr
                CURRENCY %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer.
        ENDIF.
        IF %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht = space.
          WRITE 035(020) %wa020-ztco_shopcost_at-mbgbtr
UNIT %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht NO-ZERO.
          IF NOT %wa020-ztco_shopcost_at-mbgbtr IS INITIAL.
            %keyempty = 'X'.
          ENDIF.
        ELSE.
          WRITE 035(020) %wa020-ztco_shopcost_at-mbgbtr
                UNIT %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht.
        ENDIF.
        WRITE 056(003) %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht.
        IF %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer = space.
          WRITE 060(021) %wa020-%fa00008
CURRENCY %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer NO-ZERO.
          IF NOT %wa020-%fa00008 IS INITIAL.
            %keyempty = 'X'.
          ENDIF.
        ELSE.
          WRITE 060(021) %wa020-%fa00008
                CURRENCY %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer.
        ENDIF.
        IF %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht = space.
          WRITE 088(020) %wa020-%fa00009
UNIT %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht NO-ZERO.
          IF NOT %wa020-%fa00009 IS INITIAL.
            %keyempty = 'X'.
          ENDIF.
        ELSE.
          WRITE 088(020) %wa020-%fa00009
                UNIT %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht.
        ENDIF.
        WRITE 109(003) %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht.
        IF %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer = space.
          WRITE 113(021) %wa020-ztco_shopcost_at-wip_amt
CURRENCY %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer NO-ZERO.
          IF NOT %wa020-ztco_shopcost_at-wip_amt IS INITIAL.
            %keyempty = 'X'.
          ENDIF.
        ELSE.
          WRITE 113(021) %wa020-ztco_shopcost_at-wip_amt
                CURRENCY %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer.
        ENDIF.
        IF %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht = space.
          WRITE 141(018) %wa020-%fa00005
UNIT %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht NO-ZERO.
          IF NOT %wa020-%fa00005 IS INITIAL.
            %keyempty = 'X'.
          ENDIF.
        ELSE.
          WRITE 141(018) %wa020-%fa00005
                UNIT %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht.
        ENDIF.
        WRITE 160(003) %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht.
        IF %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer = space.
          WRITE 164(021) %wa020-%fa00006
CURRENCY %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer NO-ZERO.
          IF NOT %wa020-%fa00006 IS INITIAL.
            %keyempty = 'X'.
          ENDIF.
        ELSE.
          WRITE 164(021) %wa020-%fa00006
                CURRENCY %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer.
        ENDIF.
        IF %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht = space.
          WRITE 192(018) %wa020-%fa00007
UNIT %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht NO-ZERO.
          IF NOT %wa020-%fa00007 IS INITIAL.
            %keyempty = 'X'.
          ENDIF.
        ELSE.
          WRITE 192(018) %wa020-%fa00007
                UNIT %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht.
        ENDIF.
        WRITE 211(003) %wa020-ztco_shopcost_at-ztco_shopcost_at-meeht.
        IF %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer = space.
          WRITE 215(014) %wa020-ztco_shopcost_at-preis
CURRENCY %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer NO-ZERO.
          IF NOT %wa020-ztco_shopcost_at-preis IS INITIAL.
            %keyempty = 'X'.
          ENDIF.
        ELSE.
          WRITE 215(014) %wa020-ztco_shopcost_at-preis
                CURRENCY %wa020-ztco_shopcost_at-ztco_shopcost_at-hwaer.
        ENDIF.
        PERFORM %hide.
        PERFORM %hide_color.
      ENDLOOP.
      PERFORM %newline.
      WRITE 230 '*'.
      REFRESH %wa030.
      DO.
        %subrc = 4.
        CLEAR %wa030.
        READ TABLE %w0100 INDEX sy-index.
        IF sy-subrc = 0.
          %subrc = 0.
          %wa030-ztco_shopcost_at-ztco_shopcost_at-hwaer =
          %w0100-ztco_shopcost_at-hwaer.
    %wa030-ztco_shopcost_at-manu_amt = %w0100-ztco_shopcost_at-manu_amt.
          %wa030-%fa00013 = %w0100-%fa00013.
        ENDIF.
        READ TABLE %w0200 INDEX sy-index.
        IF sy-subrc = 0.
          %subrc = 0.
          %wa030-ztco_shopcost_at-ztco_shopcost_at-meeht =
          %w0200-ztco_shopcost_at-meeht.
    %wa030-ztco_shopcost_at-manu_qty = %w0200-ztco_shopcost_at-manu_qty.
        ENDIF.
        IF %subrc = 4.
          EXIT.
        ENDIF.
        APPEND %wa030.
      ENDDO.
      LOOP AT %wa030.
        IF sy-tabix <> 1.
          PERFORM %newline.
        ENDIF.
        IF %wa030-ztco_shopcost_at-ztco_shopcost_at-hwaer = space.
          WRITE 002(021) %wa030-%fa00013
CURRENCY %wa030-ztco_shopcost_at-ztco_shopcost_at-hwaer NO-ZERO.
          IF NOT %wa030-%fa00013 IS INITIAL.
            %keyempty = 'X'.
          ENDIF.
        ELSE.
          WRITE 002(021) %wa030-%fa00013
                CURRENCY %wa030-ztco_shopcost_at-ztco_shopcost_at-hwaer.
        ENDIF.
        IF %wa030-ztco_shopcost_at-ztco_shopcost_at-hwaer = space.
          WRITE 030(021) %wa030-ztco_shopcost_at-manu_amt
CURRENCY %wa030-ztco_shopcost_at-ztco_shopcost_at-hwaer NO-ZERO.
          IF NOT %wa030-ztco_shopcost_at-manu_amt IS INITIAL.
            %keyempty = 'X'.
          ENDIF.
        ELSE.
          WRITE 030(021) %wa030-ztco_shopcost_at-manu_amt
                CURRENCY %wa030-ztco_shopcost_at-ztco_shopcost_at-hwaer.
        ENDIF.
        IF %wa030-ztco_shopcost_at-ztco_shopcost_at-meeht = space.
          WRITE 058(020) %wa030-ztco_shopcost_at-manu_qty
UNIT %wa030-ztco_shopcost_at-ztco_shopcost_at-meeht NO-ZERO.
          IF NOT %wa030-ztco_shopcost_at-manu_qty IS INITIAL.
            %keyempty = 'X'.
          ENDIF.
        ELSE.
          WRITE 058(020) %wa030-ztco_shopcost_at-manu_qty
                UNIT %wa030-ztco_shopcost_at-ztco_shopcost_at-meeht.
        ENDIF.
        WRITE 079(003) %wa030-ztco_shopcost_at-ztco_shopcost_at-meeht.
        PERFORM %hide.
        PERFORM %hide_color.
      ENDLOOP.
      IF %rflag = 'AA'. PERFORM %uline. ENDIF.
    ENDAT.
  ENDLOOP.
  %rflag = 'AA'.
  PERFORM %uline.
  CLEAR: %cline, %zgr.

ENDFORM.



*---------------------------------------------------------------------*
*       MODULE %init_view OUTPUT                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE %init_view OUTPUT.

  CASE %tab.
    WHEN 'G00'.
      PERFORM init_pbo(rsaqexce) TABLES %g00 USING tview100 'X'.
    WHEN OTHERS.
      MESSAGE s860(aq).
  ENDCASE.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE %pbo_view OUTPUT                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE %pbo_view OUTPUT.

  CASE %tab.
    WHEN 'G00'.
      PERFORM loop_pbo(rsaqexce) TABLES %g00 USING %%g00 tview100.
  ENDCASE.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE %pai_view INPUT                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE %pai_view INPUT.

  CASE %tab.
    WHEN 'G00'.
      PERFORM loop_pai(rsaqexce) TABLES %g00 USING %%g00 tview100.
  ENDCASE.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE %okcode_view INPUT                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE %okcode_view INPUT.

  CASE %tab.
    WHEN 'G00'.
      PERFORM okcode(rsaqexce) TABLES %g00 USING tview100.
  ENDCASE.

ENDMODULE.
