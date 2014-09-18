************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZCORREKESGRRTV_NEW
*& Type   : Report                                                     *
*& Author : Manju                                                      *
*& Title  : Prorgam to Correct MRP Qty (Copy of ZCORREKESGRRTV)        *
*&---------------------------------------------------------------------*
* Help Desk Request No  :     681G261745
*
*                                                                      *
*   Requested by:        Richard Davis
*
*   Assigned to:                                                       *
*   Original Request #:                                                *
*   ABAP Analyst:                                                      *
*                                                                      *
* Business Users:                                                      *
*                                                                      *
* Business Requirement Description:                                    *
*                                                                      *
*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >                *
*                                                                      *
* Configuration Requirements:                                          *
*     < Document any special config requirements that must exist for   *
*       this program to work correctly >                               *
*                                                                      *
* Program Inputs:                                                      *
*     < Input File Path & Name >                                       *
*     < Any variants program would be typically run with >             *
*                                                                      *
* Program Outputs:                                                     *
*       Online Report                                                  *
*                                                                      *
* Authorization Checks:                                                *
*     < Document if any authorization objects are checked >            *
*                                                                      *
* Direct Update Database Tables:                                       *
*   < No direct updates to SAP tables are allowed.List custom tables > *
*                                                                      *
* Outstanding Issues:                                                  *
*     < If the program is being delivered with any known open issues   *
*       document them here; they could be planned for next release >   *
*                                                                      *
* Instructions on how to test this program:                            *
*     < If this program needs any special inputs like an inbound file  *
*       from an EDI subsystem for testing, document it here >          *
*                                                                      *
* Instructions on how to re-start this program:                        *
*                                                                      *
* Volume Estimates:                                                    *
*                                                                      *
* Frequency of Execution:                                              *
*   o On demand                                                        *
*                                                                      *
* Execution Mode:                                                      *
*   o Online      - Transaction Code -                                 *
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*&----------------------------------------------------------------------
* Modification Logs
************************************************************************
* Date        Developer    RequestNo    Description
* 08/26/06    Manju        UD1K921881   Initial Coding
* 08/28/06    Manju        UD1K921887   Program changes
* 08/28/06    Manju        UD1K921897   Perfomance fine tuning
************************************************************************
REPORT ZCORREKESGRRTV_NEW LINE-SIZE 132 LINE-COUNT 65
                              message-id db .

* BUSINESS PROCESS FOR RETURN DELIVERIES OF OSS NOTE 491785 IS USED
**********************************************************************
* Search P.O. confirmation records with confirmation key
* and GR assignment
* Compute GR quantity and check against DABMG
* Compute Total of GR
* correct EKES-DABMG, if not correct
*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*

TABLES: ekes,
        ekbe, t163g,
        ekpo.

*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*

DATA: t_ekpo LIKE ekpo OCCURS 0 WITH HEADER LINE.
DATA: t_ekes LIKE ekes OCCURS 0 WITH HEADER LINE.
*DATA: BEGIN OF t_ekbe OCCURS 20.
*        INCLUDE STRUCTURE ekbe                        .
*DATA: END OF t_ekbe.

data : begin of t_ekbe occurs 0,
       ebeln LIKE ekbe-ebeln,
       ebelp LIKE ekbe-ebelp,
       etens LIKE ekbe-etens,
       menge LIKE ekbe-menge,
       shkzg like ekbe-shkzg,
       et_upd like ekbe-et_upd,
       end of t_ekbe.

data : begin of t_ekbe_tot occurs 0,
       ebeln LIKE ekbe-ebeln,
       ebelp LIKE ekbe-ebelp,
       etens LIKE ekbe-etens,
       menge LIKE ekbe-menge,
       shkzg like ekbe-shkzg,
       et_upd like ekbe-et_upd,
       end of t_ekbe_tot.

DATA: BEGIN OF t_ekes_upd OCCURS 20,
       ebeln LIKE ekes-ebeln,
       ebelp LIKE ekes-ebelp,
       etens LIKE ekes-etens,
       menge LIKE ekes-menge,
       dabmg_old LIKE ekes-dabmg,
       dabmg_new LIKE ekes-dabmg,
       gr_total LIKE ekes-dabmg,
           END OF t_ekes_upd.                          .
DATA: BEGIN OF xt163g OCCURS 5.
        INCLUDE STRUCTURE t163g.
DATA: END OF xt163g.
DATA: h_ebtyp LIKE t163g-ebtyp,
      h_wemng LIKE ekbe-menge,
      h_wemng_total LIKE ekbe-menge,
      ekbe_shkz(1) TYPE c.

DATA: int_eket LIKE beket OCCURS 0,
      wa_eket TYPE beket,
      eket_updkz(1) TYPE c,
      update type c.

*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: po_nr FOR ekpo-ebeln obligatory.
SELECT-OPTIONS: po_item FOR ekpo-ebelp.
Parameters : p_gjahr like ekbe-gjahr default sy-datum(4) obligatory.
*PARAMETERS:  update AS CHECKBOX default  ''  .
SELECTION-SCREEN end  OF BLOCK b1 .

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:  p_p1  radiobutton group g1,
             p_p2  radiobutton group g1.

SELECTION-SCREEN end  OF BLOCK b2 .


initialization.
update = ''.

*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
START-OF-SELECTION.
  REFRESH  t_ekes_upd.

  SELECT * FROM t163g INTO TABLE xt163g
           WHERE wezuo NE space.


  h_ebtyp = 'ZP'.

* Select Data from PO History Table for Given Input
  if p_p1 eq 'X' .
    SELECT *  INTO corresponding fields of  TABLE t_ekbe
              FROM ekbe
         WHERE ebeln in po_nr and
               ebelp in po_item
         AND   zekkn EQ '00'
         AND   vgabe EQ '1'
         and   GJAHR eq p_gjahr
         and   BWART <> '' .
  else.
    SELECT * INTO corresponding fields of  TABLE t_ekbe
          FROM ekbeh
         WHERE ebeln in po_nr
         AND   ebelp in po_item
         AND   zekkn EQ '00'
         AND   vgabe EQ '1'
         and   GJAHR eq p_gjahr
         and   BWART <> '' .
  endif.

* Get GR Totals Per Each PO .
Sort t_ekbe by ebeln ebelp etens.
  lOOP AT t_ekbe.
    CASE t_ekbe-shkzg.
      WHEN 'S'.                "Goods receipt
        h_wemng_total = h_wemng_total + t_ekbe-menge.
      WHEN 'H'.                "Reversal
        h_wemng_total = h_wemng_total - t_ekbe-menge.
      WHEN OTHERS.
    ENDCASE.
    at end of ebeln.
      t_ekbe_tot-ebeln = t_ekbe-ebeln.
      t_ekbe_tot-menge = h_wemng_total.
      append t_ekbe_tot.
      clear h_wemng_total.
    endat.
  ENDLOOP.

* Compare EKBE entries with EKES to find mismatch Qty for Sequence
*Number

  LOOP AT t_ekbe.
    CASE t_ekbe-shkzg.
      WHEN 'S'.                "Goods receipt
        h_wemng = h_wemng + t_ekbe-menge.
      WHEN 'H'.                "Reversal
        h_wemng = h_wemng - t_ekbe-menge.
      WHEN OTHERS.
    ENDCASE.

    at end of etens.
      if h_wemng  > 0 .
*          Select from EKES
        SELECT * FROM ekes INTO TABLE t_ekes
                 WHERE ebeln EQ t_ekbe-ebeln
                  AND   ebelp EQ t_ekbe-ebelp
                  AND   ebtyp EQ h_ebtyp
                  and   etens EQ t_ekbe-etens.
        if sy-subrc eq 0.

          clear h_wemng_total.
          read table t_ekbe_tot with key ebeln = t_ekbe-ebeln.
          if sy-subrc eq 0.
            h_wemng_total = t_ekbe_tot-menge.
          endif.

          loop at t_ekes.
            IF h_wemng NE t_ekes-dabmg.
              t_ekes_upd-ebeln       = t_ekes-ebeln.
              t_ekes_upd-ebelp       = t_ekes-ebelp.
              t_ekes_upd-etens       = t_ekes-etens.
              t_ekes_upd-menge       = t_ekes-menge.
              t_ekes_upd-dabmg_old   = t_ekes-dabmg.
              t_ekes_upd-dabmg_new   = h_wemng.
              t_ekes_upd-gr_total    = h_wemng_total.
              APPEND  t_ekes_upd.
            ENDIF.
          endloop.
        else.
          t_ekes_upd-ebeln = t_ekbe-ebeln.
          t_ekes_upd-ebelp = t_ekbe-ebelp.
          t_ekes_upd-etens = t_ekbe-etens.
          t_ekes_upd-menge = t_ekbe-menge.
          t_ekes_upd-dabmg_old = 0.
          t_ekes_upd-dabmg_new = h_wemng.
          t_ekes_upd-gr_total    = h_wemng_total.
          APPEND  t_ekes_upd.
          clear h_wemng_total.
        endif.
      endif.
      clear  h_wemng.
    endat.
  ENDLOOP.

* Print Difference's
  LOOP AT t_ekes_upd.
    WRITE: / t_ekes_upd-ebeln, t_ekes_upd-ebelp, t_ekes_upd-etens,
              t_ekes_upd-menge,
              t_ekes_upd-dabmg_old, t_ekes_upd-dabmg_new,
              t_ekes_upd-gr_total.
*    IF NOT update IS INITIAL.
*      CLEAR ekes.
*      MOVE-CORRESPONDING t_ekes_upd TO ekes.
*      UPDATE ekes
*        SET dabmg = t_ekes_upd-dabmg_new
*         WHERE ebeln = ekes-ebeln
*         AND   ebelp = ekes-ebelp
*         AND   etens = ekes-etens.
*      IF sy-subrc NE 0.
**     raise update_mistake.
*      ENDIF.
*    ENDIF.
  ENDLOOP.

* redistribution

*  LOOP AT t_ekes_upd.
*
*    REFRESH int_eket.
*
*  SELECT * FROM ekpo INTO TABLE t_ekpo
*           WHERE ebeln eq t_ekes_upd-ebeln
*           AND   ebelp eq t_ekes_upd-ebelp
*           AND   bstae NE space.
*
*    READ TABLE t_ekpo WITH KEY ebeln = t_ekes_upd-ebeln
*                               ebelp = t_ekes_upd-ebelp
*                          INTO ekpo.
*
*    SELECT * FROM eket INTO TABLE int_eket
*                  WHERE ebeln EQ ekpo-ebeln
*                    AND ebelp EQ ekpo-ebelp.
*
*    CALL FUNCTION 'ME_CONFIRMATION_MAINTAIN'
*         EXPORTING
*              i_bstae    = ekpo-bstae
*              i_ebeln    = ekpo-ebeln
*              i_ebelp    = ekpo-ebelp
*              i_funkt    = 'DV'
*              i_werks    = ekpo-werks
*         IMPORTING
*              e_vb_updkz = eket_updkz
*         TABLES
*              xeket      = int_eket.
*    IF NOT eket_updkz IS INITIAL AND NOT update IS INITIAL.
*      LOOP AT int_eket INTO wa_eket WHERE updkz EQ 'U'.
*        UPDATE eket
*           SET dabmg = wa_eket-dabmg
*           WHERE ebeln EQ wa_eket-ebeln
*             AND ebelp EQ wa_eket-ebelp
*             AND etenr EQ wa_eket-etenr.
*      ENDLOOP.
*    ENDIF.

*  endloop.
