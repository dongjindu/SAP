*----------------------------------------------------------------------*
*   INCLUDE ZXCKAU00_DATA                                              *
*----------------------------------------------------------------------*

data:  lf_bqpim like bqpim,
       lf_bqpex like bqpex.

*BOM
*BOM1
data: begin of lt_upg occurs 0,
         idnrk like stpo-idnrk,
         maktx like makt-maktx,
      end of lt_upg.

data: begin of stb occurs 1000.
        include structure stpox.
data: end of stb.
data: begin of lt_bom occurs 0,
         index  like stpox-index,
         stufe  like stpox-stufe,
         disst  like stpox-disst, "low-level code
         idnrk  like stpox-idnrk, "Object(Mat)
         posnr  like stpox-posnr,
         hdnfo  like stpox-hdnfo,
         mtart  like stpox-mtart, "mat type
         xchar  like stpox-xchar, "batch-mgt
         dumps  like stpox-dumps, "Phantom.
         stkkz  like stpox-stkkz, "assemble ind
         schgt  like stpox-schgt, "ind-bulk material
         mstae  like stpox-mstae, "mat-status
         mmsta  like stpox-mmsta, "mat-status(plant)
         menge  like stpox-menge, "Qty
         ausss  like stpox-ausss, "assembly scrap
         kausf  like stpox-kausf, "component scrap
         bausf  like stpox-bausf, "assembly scrap%
         meins  like stpox-meins, "UoM
         sobsl  like stpox-sobsl, "Special.Proc.
         rgekz  like stpox-rgekz, "b/f ind.
         lgpro  like stpox-lgpro, "S.Loc
         matmk  like stpox-matmk, "Mat.Group
         postp  like stpox-postp, "Type
         sortf  like stpox-sortf, "SortString
         stawn  like stpox-stawn, "Duty Code
         xtlty  like stpox-xtlty, "BOM category (next level)
         xtlnr  like stpox-xtlnr, "BOM no.
         eitm   like stpox-eitm,  "EndItem
         stgb   like stpox-stgb,  "Str.Type
         ojtxb  like stpox-ojtxb, "description
         upgn   like stpox-upgn,  " user field - upg
      end of lt_bom.
*output
data: begin of itab occurs 0,
         kokrs  like ckiuser-kokrs,
         klvar  like ckiuser-klvar,
         bdatj  like ckiuser-bdatj,
         poper  like ckiuser-poper,

         werks  like ckiuser-werks,
         artnr  like ckiuser-matnr,
         stalt  like ckiuser-stalt, "Alternative BOM
         bwdat  like ckiuser-bwdat, "valuation date
         aldat  like ckiuser-aldat, "quantity date
         verid  like keko-verid,    "Production Version

         compn  like stpox-idnrk,   "BOM item
         indx   like stpox-index,   "BOM index
         kstar  like kis1-kstar ,
         typps  like kis1-typps , "Item category
         ltext  like kis1-ltext , "comp.Description
         reqqt  like kis1-menge ,  "BOM Qty
         meeht  like kis1-meeht ,  "UoM
         wertn  like kis1-wertn ,  "comp.value
         gpreis like kis1-gpreis,  "unit price
         amtdt  like kis1-wertn ,  "duty/unit
         amtft  like kis1-wertn ,  "freight/unit
         amtot  like kis1-wertn ,  "other/unit
         total  like kis1-wertn ,  "total
         peinh  like kis1-peinh,   "price unit
         lifnr  like kis1-lifnr ,  "Vendor
         splnt  type werkq ,       "Supplying plant (source)
         infnr  like kis1-infnr ,  "Info-record
         strat  type ck_strat,     "Valuation Strategy

         land1  like lfa1-land1 ,  "country
         name1  like lfa1-name1 ,  "vendor name
*BOM info
         upgvc  like kis1-matnr ,  "UPG
         upgtx  like kis1-ltext ,  "UPG TEXT
         chk,

         bklas  type bklas,
         verpr  type verpr,
         kalnr  type ck_kalnr.
*Item info
        include structure lt_bom.
*         mtart  LIKE stpox-mtart, "mat type
*         xchar  LIKE stpox-xchar, "batch-mgt
*         mstae  LIKE stpox-mstae, "mat-status
*         mmsta  LIKE stpox-mmsta, "mat-status(plant)
*         sobsl  LIKE stpox-sobsl, "Special.Proc.
*         rgekz  LIKE stpox-rgekz, "b/f ind.
*         lgpro  LIKE stpox-lgpro, "S.Loc
*         matmk  LIKE stpox-matmk, "Mat.Group
*         postp  LIKE stpox-postp, "Type
*         sortf  LIKE stpox-sortf, "SortString
*         xtlty  LIKE stpox-xtlty, "BOM category (next level)
*         AUSSS  like stpox-AUSSS, "assembly scrap
*         KAUSF  like stpox-kausf, "component scrap
*         BAUSF  like stpox-BAUSF, "assembly scrap%
*         meins  LIKE stpox-meins, "UoM
*         index  LIKE stpox-index,
*         stufe  LIKE stpox-stufe,
data: end of itab.

data: l_cnt   type i,
      l_cnt2  type i.
data l_index like sy-tabix.


data: lt_stpov1 like stpov occurs 0 with header line.
data: lt_stpov2 like stpov occurs 0 with header line.

*DATA: lt_mast LIKE mast_api02 OCCURS 0 WITH HEADER LINE.
data: lt_mast like mast occurs 0 with header line.
data: lt_stpo like stpo occurs 0 with header line.

data: w_upg like lt_bom.
data l_capid type capid.

data: begin of t_lfa1 occurs 0,
        lifnr like lfa1-lifnr,
        land1 like lfa1-land1,
        name1 like lfa1-name1,
      end   of t_lfa1.


* Internal Table for ABP LDC Rate
types: begin of ty_ldc,
         kokrs type kokrs,
         bdatj type bdatj,
         ver   type zver1,
         land1 type land1_gp,
         matnr type matnr,
         fra1  type zfrg1,
         zoth  type zoth,
         zoti  type zoti,
       end of ty_ldc.

types: begin of ty_a902,
         stawn type stawn,
         kbetr type kbetr,
       end of ty_a902.

data   gt_ldc       type table of ty_ldc      with header line.
data   gt_a902 type table of ty_a902 with header line.

data   ln  type i.
data   $p_duty like gt_a902-kbetr value '2.5'.

type-pools: ckmv0.

data   lt_kalnr type ckmv0_laobj_tbl with header line.
ranges :ir_prtyp for mlprkeph-prtyp,
        ir_curtp for tkel-curtp.

data : it_prkeph_fsc_temp  type mlccs_t_prkeph.
data : it_prkeph like ckmlprkeph occurs 0 with header line.

data: w_ckis  like ckis.
data: w_wertn like kis1-wertn.
data: w_ukaln like ckis-ukaln.
