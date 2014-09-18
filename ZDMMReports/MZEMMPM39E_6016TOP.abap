*&---------------------------------------------------------------------*
*& Include MZEMMPM39E_6016TOP                                          *
*&                                                                     *
*&---------------------------------------------------------------------*
TABLES: ltak,        "WM transfer order header
        ltap,        "Transfer order item
        *ltap,       "Transfer order item
        t333t,       "Texts for WM Movement Types
        t307t,       "Text for Storage Unit Type
        t301t.       "Storage Type Descriptions

* For PF-STATUS and Titlebar
CLASS lcl_ps DEFINITION DEFERRED.
DATA: crv_ps TYPE REF TO lcl_ps.

**** Begin of Menu
* Itab & WA For FOR Dynamic Menu
DATA:  it_func TYPE STANDARD TABLE OF rsmpe-func.
*DATA: BEGIN OF itabfunc OCCURS 0,
*         fcode LIKE rsmpe-func,
*       END OF itabfunc.

DATA:  wa_func LIKE LINE OF it_func.

* Title
DATA: w_title(80).         " Title

*Push Button Name
DATA: w_pb_name(20).
**** End of Menu


* For OK code
DATA: ok_code LIKE sy-ucomm,  save_ok_code LIKE ok_code.

* For Return code
DATA: w_subrc LIKE sy-subrc.

* Confirmation Option
DATA: w_confirmation(10).   " 'PICK' or 'TRANSFER'.

* Confirm all & Confirm each button adjust
DATA: w_call_ceac_deletion_flg(4).
" 'CALL':CALL FUNCTION KEY DELETED, 'CEAC':CEAC FUNCTION KEY DELETED,

* For Error index
DATA: w_error_idx TYPE i.

*/ Begin of List Processing
* Itab & WA for List
DATA: BEGIN OF wa_ltxx,
       lgnum LIKE ltak-lgnum,
       "Warehouse Number
       tanum LIKE ltak-tanum,
       "Transfer order number
       matnr LIKE ltap-matnr,
       "Material number
       maktx LIKE makt-maktx,
       "Material description
       meins LIKE ltap-meins,
       "Base unit of measure
       altme LIKE ltap-altme,
       "Alternative unit of measure for stockkeeping unit
       nsolm LIKE ltap-nsolm,
       "Destination target quantity in stockkeeping unit
       nsola LIKE ltap-nsola,
        "Destination target quantity in alternative unit
        "--> No. of Boxes.
       works LIKE ztmm_mast-works,
       "Workstation
       rh_lh LIKE ztmm_mast-rh_lh,
       "RH/LH
       refnr LIKE ltak-refnr,
       "Group
       stdat LIKE ltak-stdat,
       "Start date of the transfer order
       stuzt LIKE ltak-stuzt,
       "Start time of the transfer order
       endat LIKE ltak-endat,
       "Transfer order end date
       enuzt LIKE ltak-enuzt,
       "Transfer order end time
       vltyp LIKE ltap-vltyp,
       "Source storage type
       vlpla LIKE ltap-vlpla,
       "Source storage bin
       nltyp LIKE ltap-nltyp,
       "Destination storage type
       nlpla LIKE ltap-nlpla,
       "Destination storage bin
       pquit LIKE ltap-pquit,
       "Indicator: confirmation complete
       pvqui LIKE ltap-pvqui,
       "Indicator: Material pick has been confirmed
       "Related to /nZMME88. Will be excluded
       executed_flg,
       "Space:Not Executed, 'X':Executed
      END OF wa_ltxx.
DATA: it_ltxx LIKE TABLE OF wa_ltxx.
DATA: it_ltxx_result LIKE TABLE OF wa_ltxx. "Result Table

*/ End of List Processing


*-For Step Loop
DATA: w_top_line TYPE i VALUE '1',
      w_loopc  LIKE sy-loopc,  "Loop Count
      w_lines  TYPE i.         "Total lines of Itab


**** For Screen 0120 or 0130.
DATA: io_cmatnr LIKE wa_ltxx-matnr. "Compared Qty

DATA: io_sqty LIKE wa_ltxx-nsolm.  "Scanned Qty
DATA: io_cqty LIKE wa_ltxx-nsolm.  "Compared Qty
DATA: io_dqty TYPE mensh.          "Difference Qty (+- possible)



* For BDC message
DATA: it_bdcmsgcoll LIKE TABLE OF bdcmsgcoll.
DATA: wa_bdcmsgcoll LIKE LINE OF it_bdcmsgcoll.

**** Constants&Vars for Number range object ****************************
CONSTANTS: nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
CONSTANTS: nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
CONSTANTS: nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc. No.
* Number range object
DATA:      nro_object  VALUE 'ZMMNRO0002'
                               LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
DATA:      nro_number  TYPE num10.      " Same type of nro_object
**** App Doc No
DATA: w_zdocno TYPE num10.     "Application Doc no.



*---------- List Procession Events ------------------------------------*
TOP-OF-PAGE.
*  PERFORM make_col_heading.
*  PERFORM make_col_heading2.
