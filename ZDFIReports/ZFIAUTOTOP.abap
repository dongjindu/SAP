*----------------------------------------------------------------------*
*   INCLUDE ZFIAUTOTOP    .
*----------------------------------------------------------------------*

*---------------------------------------------------------------------*
* TABLES
*---------------------------------------------------------------------*
Tables: ekbz,
        ekko,
        a018,
        A016,
        KONP,
        s001,
        ekpo,
        ekbe.
*----------------------------------------------------------------------*
* Types-Pools                                                          *
*----------------------------------------------------------------------*
TYPE-POOLS:
  slis.
INCLUDE: <icon>.
*-----------------------------------------------------------------------
* ALV Data Declarations
*-----------------------------------------------------------------------
CLASS cl_gui_cfw      DEFINITION LOAD.

*// Declare reference variables, the container and internal table
DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

* Global variables for attributes or etc of ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_fi  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

data : wa_variant TYPE disvariant. "for parameter IS_VARIANT

DATA: wa_save    TYPE c   VALUE 'A'.   "for Parameter I_SAVE



*----------------------------------------------------------------*
* Data declaration
*----------------------------------------------------------------*
DATA : w_repid LIKE sy-repid,
       l_cnt type i.

data :  gt_fieldcat type         slis_t_fieldcat_alv,
        gt_list                  TYPE TABLE OF ZFI_AUTO_INVOICE,
        gs_variant               TYPE disvariant,
        GT_SORTCAT               TYPE TABLE OF SLIS_SORTINFO_ALV,
        GT_filter                type table of slis_filter_alv.


data : flag type c,
       flag1 type c,
       w_line type i,
       l_index type i,
       Ind type c,
       wa_qty like ekbz-menge,
       wa_qty1 like ekbz-menge,
       wa_dmtr like ekbz-DMBTR,
       f_date  like ekbz-budat,
       T_date  like ekbz-budat,
       p_park TYPE C,
       P_POST TYPE C.

* BAPI
DATA : HEADERDATA        LIKE    BAPI_INCINV_CREATE_HEADER,
       I_INVOICE         LIKE    RBKP-XRECH,
       I_CREDITMEMO      LIKE    RBKP-XRECH,
       INVOICEDOCNUMBER  LIKE    BAPI_INCINV_FLD-INV_DOC_NO,
       FISCALYEAR        LIKE    BAPI_INCINV_FLD-FISC_YEAR.

DATA:   BEGIN OF ITEMDATA OCCURS 0.
        INCLUDE STRUCTURE   BAPI_INCINV_CREATE_ITEM.
DATA:   END   OF ITEMDATA.

DATA:   BEGIN OF TAXDATA OCCURS 0.
        INCLUDE STRUCTURE   BAPI_INCINV_CREATE_TAX.
DATA:   END   OF TAXDATA.

DATA:   BEGIN OF RETURN OCCURS 0.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

DATA:   BEGIN OF XRETURN OCCURS 0.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF XRETURN.

*--------------------------------------------------------*
* Internal table declaration
*--------------------------------------------------------*
data : begin of it_po occurs 0,
*       key(16) type c,
       ebeln like ekbz-ebeln,
       EBELP like ekbz-EBELP,
*       BELNR like ekbz-BELNR,
*       budat like ekbz-budat,
       menge like ekbz-menge,
       dmbtr like ekbz-dmbtr,
       WAERS like ekbz-WAERS,
       shkzg like ekbz-shkzg,
*       BEWTP like ekbz-BEWTP,
       lifnr_ekko  like ekbz-lifnr,
       lifnr       like ekbz-lifnr,
       matnr like ekpo-matnr,
       MEINS like ekpo-meins,
*       stunr like ekbz-stunr,
*       zaehk like ekbz-zaehk,
*       GJAHR like ekbz-GJAHR,
*       BUZEI like ekbz-BUZEI,
       lfbnr like ekbe-lfbnr,
       end of it_po.
*
data : begin of it_po1 occurs 0,
*       key(16) type c,
       ebeln like ekbz-ebeln,
       EBELP like ekbz-EBELP,
*       budat like ekbz-budat,
       menge like ekbz-menge,
       dmbtr like ekbz-dmbtr,
       WAERS like ekbz-WAERS,
       shkzg like ekbz-shkzg,
       BEWTP like ekbz-BEWTP,
       lifnr_ekko  like ekbz-lifnr,
       lifnr       like ekbz-lifnr,
       matnr like ekpo-matnr,
       MEINS like ekpo-meins,
       BELNR like ekbz-BELNR,
*       stunr like ekbz-stunr,
*       zaehk like ekbz-zaehk,
*       GJAHR like ekbz-GJAHR,
*       BUZEI like ekbz-BUZEI,
       lfbnr like ekbe-lfbnr,
       end of it_po1.

data : begin of it_tab occurs 0,
       ebeln like ekbz-ebeln,
       EBELP like ekbz-EBELP,
*       BELNR like ekbz-BELNR,
       budat like ekbz-budat,
       menge like ekbz-menge,
       dmbtr like ekbz-dmbtr,
       WAERS like ekbz-WAERS,
       shkzg like ekbz-shkzg,
       lifnr like ekbz-lifnr,
       matnr like ekpo-matnr,
       MEINS like ekpo-meins,
       icon  type icon_d,
       BELUM like mseg-BELUM,
       MSG   like ZFI_AUTO_INVOICE-msg,
       stunr like ekbz-stunr,
       zaehk like ekbz-zaehk,
       GJAHR like ekbz-GJAHR,
       BUZEI like ekbz-BUZEI,
       lfbnr like ekbe-lfbnr,
       end of it_tab.

data : begin of out_tab occurs 0,
      ebeln like ekbz-ebeln,
      EBELP like ekbz-EBELP,
*      BELNR like ekbz-BELNR,
      budat like ekbz-budat,
      menge like ekbz-menge,
      dmbtr like ekbz-dmbtr,
      WAERS like ekbz-WAERS,
      shkzg like ekbz-shkzg,
      lifnr like ekbz-lifnr,
      matnr like ekpo-matnr,
      MEINS like ekpo-meins,
      icon  type icon_d,
      BELUM like mseg-BELUM,
      MSG   like ZFI_AUTO_INVOICE-msg,
      stunr like ekbz-stunr,
      zaehk like ekbz-zaehk,
      GJAHR like ekbz-GJAHR,
      BUZEI like ekbz-BUZEI,
      end of out_tab.

data : begin of it_tabpo occurs 0,
       ebeln like ekbz-ebeln,
       EBELP like ekbz-EBELP,
       menge like ekbz-menge,
       dmbtr like ekbz-dmbtr,
       WAERS like ekbz-WAERS,
       shkzg like ekbz-shkzg,
       lifnr like ekbz-lifnr,
       matnr like ekpo-matnr,
       MEINS like ekpo-meins,
       lfbnr like ekbe-lfbnr,
       end of it_tabpo.

data : begin of it_tabpo1 occurs 0,
       ebeln like ekbz-ebeln,
       EBELP like ekbz-EBELP,
       menge like ekbz-menge,
       dmbtr like ekbz-dmbtr,
       WAERS like ekbz-WAERS,
       shkzg like ekbz-shkzg,
       lifnr like ekbz-lifnr,
       matnr like ekpo-matnr,
       MEINS like ekpo-meins,
       lfbnr like ekbe-lfbnr,
       end of it_tabpo1.

* BDC Internal table
DATA:BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA:END OF BDC_TAB.

data  begin of messtab occurs 0.     " BDC MESSAGE TABLE.
        include structure bdcmsgcoll.
data  end of messtab.

DATA : WA_OPTION_DS   LIKE CTU_PARAMS.

data : begin of po_line occurs 0,
        line_no like ekpo-ebelp,
       end   of po_line.

CONSTANTS: C_MARK   VALUE 'X',         " MARK
           C_DATE   VALUE '1',         " DATE TYPE
           C_MODE   VALUE 'D'.         " BDC MODE
