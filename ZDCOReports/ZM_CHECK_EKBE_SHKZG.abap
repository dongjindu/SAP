*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120061532 0000238009                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 46B          All Support Package Levels                   $*
*$  Release 46C          All Support Package Levels                   $*
*$------------------------------------------------------------------$*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS ZM_CHECK_EKBE_AREWR
*& Object Header   PROG ZM_CHECK_EKBE_AREWR
*&-------------------------------------------------------------------*
*& REPORT ZM_CHECK_EKBE_AREWR
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
REPORT ZM_CHECK_EKBE_AREWR .

*---------------------------------------------------------------------*
* select options:
* SO_BELNR = MM invoice document
* SO_GJAHR = Document year
* SO_BUDAT = Posting date of MM invoice document
* SO_BUKRS = Company Code of Purchase Order
*---------------------------------------------------------------------*

tables: EKKO, EKPO, RBKP, RSEG.

data: tab_rbkp like rbkp occurs 10,
      s_rbkp like rbkp.

data: tab_rseg like rseg occurs 10,
      s_rseg like rseg.

data: begin of tab_check_ebeln occurs 10,
      ebeln like ekpo-ebeln,
      ebelp like ekpo-ebelp.
data: end of tab_check_ebeln.

data: tab_ekko like ekko occurs 10,
      s_ekko like ekko.

data: tab_ekpo like ekpo occurs 10,
      s_ekpo like ekpo.

data: tab_ek08rn like ek08rn occurs 10.
data: s_ek08rn like ek08rn.

data: begin of tab_hist_error_qty_zero occurs 10,
       ebeln like ekko-ebeln,
       ebelp like ekpo-ebelp,
       lfbnr like ek08rn-lfbnr,
       wemng like ek08rn-wemng,
       remng like ek08rn-remng,
       wewrt like ek08rn-wewrt,
       arewr like ek08rn-arewr.
data: end of tab_hist_error_qty_zero.

data: begin of tab_hist_error_different_sign occurs 10,
       ebeln like ekko-ebeln,
       ebelp like ekpo-ebelp,
       lfbnr like ek08rn-lfbnr,
       wemng like ek08rn-wemng,
       remng like ek08rn-remng,
       wewrt like ek08rn-wewrt,
       arewr like ek08rn-arewr.
data: end of tab_hist_error_different_sign.

data: begin of tab_hist_probably_wrong occurs 10,
       ebeln like ekko-ebeln,
       ebelp like ekpo-ebelp,
       lfbnr like ek08rn-lfbnr,
       wemng like ek08rn-wemng,
       remng like ek08rn-remng,
       wewrt like ek08rn-wewrt,
       arewr like ek08rn-arewr.
data: end of tab_hist_probably_wrong.

Data: f_lines type i,
      f_diff_menge like ek08rn-remng,
      f_diff_value like ek08rn-wewrt,
      f_diff_theoretical_value like ek08rn-wewrt,
      f_quot_we type p decimals 2,
      f_quot_re type p decimals 2,
      f_quot_compare type p decimals 2,
      f_quot type p decimals 2 value 5,
      f_add_check type boole-boole.

Select-options: so_belnr for rbkp-belnr,
                so_gjahr for rbkp-gjahr,
                so_cpudt for rbkp-cpudt,
                so_bukrs for ekko-bukrs.
                                                                 .
* ---- selection ------------------------------------------------------*

SELECT * FROM  RBKP  into table tab_rbkp
       WHERE  BELNR  in so_belnr
       AND    GJAHR  in so_gjahr
       AND    CPUDT  in so_cpudt.

if sy-subrc ne 0.
  write: 'No RBKP entries found'.
  exit.
endif.

select * from rseg into table tab_rseg
                   for all entries in tab_rbkp
       WHERE  BELNR  = tab_rbkp-belnr
       AND    GJAHR  = tab_rbkp-gjahr.

if sy-subrc ne 0.
  write: 'No RSEG entries found'.
  exit.
endif.

* ---- fill tab_check_ebeln -------------------------------------------*
loop at tab_rseg into s_rseg.
   tab_check_ebeln-ebeln = s_rseg-ebeln.
   tab_check_ebeln-ebelp = s_rseg-ebelp.
   collect tab_check_ebeln.
endloop.

select * from ekko into table tab_ekko
                   for all entries in tab_check_ebeln
       WHERE  EBELN  = tab_check_ebeln-ebeln
       AND    BUKRS  in so_bukrs.

if sy-subrc ne 0.
  write: 'No EKKO entries found'.
  exit.
endif.

select * from ekpo into table tab_ekpo
                   for all entries in tab_check_ebeln
       WHERE  EBELN  = tab_check_ebeln-ebeln
       AND    EBELP  = tab_check_ebeln-ebelp
       AND    BUKRS  in so_bukrs
       AND    pstyp  ne 9.

if sy-subrc ne 0.
  write: 'No EKPO entries found'.
  exit.
endif.

* ---- analyzation ----------------------------------------------------*

loop at tab_ekpo into s_ekpo.
  check not s_ekpo-wepos is initial.
  check s_ekpo-weunb is initial.
  read table tab_ekko into s_ekko with key ebeln = s_ekpo-ebeln.
  CALL FUNCTION 'ME_READ_ITEM_INVOICE'
        EXPORTING
             DISPLAY        = 'X'
             EBELP          = s_ekpo-ebelp
             IEKKO          = s_ekko
             RE_KURSF       = s_ekko-wkurs
             RE_WAERS       = s_ekko-waers
             RE_WWERT       = sy-datum
        TABLES
             XEK08RN        =  tab_ek08rn
        EXCEPTIONS
             NOT_FOUND_ANY  = 1
             NOT_FOUND_ONE  = 2
             NOT_VALID_ANY  = 3
             NOT_VALID_ONE  = 4
             ENQUEUE_FAILED = 5
             OTHERS         = 6
          .
   IF SY-SUBRC <> 0.
     continue.
   ENDIF.

   loop at tab_ek08rn into s_ek08rn.
     perform check_differences.
   endloop.
endloop.


*-------- List of wrong PO histories ---------------------------------*
*-----
 clear f_lines.
 describe table tab_hist_error_qty_zero lines f_lines.

 Write: /  'PO history total invoiced eq total delivered but ' ,
           'GR/IR account is not cleared' .
         uline .
 if f_lines eq 0.
    write: / 'No PO items with a wrong PO history found' .
 else.
   perform write_header.                        .
   loop at tab_hist_error_qty_zero.
      write:  /1  tab_hist_error_qty_zero-ebeln ,
               11  tab_hist_error_qty_zero-ebelp ,
               17  tab_hist_error_qty_zero-lfbnr ,
               28  tab_hist_error_qty_zero-wemng ,
               43  tab_hist_error_qty_zero-remng ,
               58  tab_hist_error_qty_zero-wewrt ,
               73  tab_hist_error_qty_zero-arewr.
   endloop.
 endif.
       uline.
       write:  / , / , / .
       uline.
*--------
 clear f_lines.
 describe table tab_hist_error_different_sign lines f_lines.
 Write: /  'PO history different sign for difference qty and value'.
         uline .
 if f_lines eq 0.
    write: / 'No PO items with a wrong PO history found' .
 else.
   perform write_header.
   loop at tab_hist_error_different_sign.
     write:  /1  tab_hist_error_different_sign-ebeln ,
              11  tab_hist_error_different_sign-ebelp ,
              17  tab_hist_error_different_sign-lfbnr ,
              28  tab_hist_error_different_sign-wemng ,
              43  tab_hist_error_different_sign-remng ,
              58  tab_hist_error_different_sign-wewrt ,
              73  tab_hist_error_different_sign-arewr.
   endloop.
 endif.
       uline.
       write:  / , / , / .
       uline.
*--------
 clear f_lines.
 check not f_add_check is initial.
 describe table tab_hist_probably_wrong lines f_lines.
 Write: /  'PO histories with unrealistic differences' .
         uline .
 if f_lines eq 0.
    write: / 'No PO items with a wrong PO history found' .
 else.
   perform write_header.
   loop at tab_hist_probably_wrong.
      write:  /1  tab_hist_probably_wrong-ebeln ,
               11  tab_hist_probably_wrong-ebelp ,
               17  tab_hist_probably_wrong-lfbnr ,
               28  tab_hist_probably_wrong-wemng ,
               43  tab_hist_probably_wrong-remng ,
               58  tab_hist_probably_wrong-wewrt ,
               73  tab_hist_probably_wrong-arewr.

   endloop.
 endif.

*---------------------------------------------------------------------*


form check_differences.

*------ calculate differences
       F_diff_menge = s_ek08rn-wemng - s_ek08rn-remng.
       F_diff_value = s_ek08rn-wewrt - s_ek08rn-arewr.
*------ check and fill error_tables---------------------------------*
       check not ( F_diff_menge eq 0 and F_diff_value eq 0 ) .
       if F_diff_menge eq 0 and  F_diff_value ne 0.
          tab_hist_error_qty_zero-ebeln = s_ekpo-ebeln.
          tab_hist_error_qty_zero-ebelp = s_ekpo-ebelp.
          tab_hist_error_qty_zero-lfbnr = s_ek08rn-lfbnr.
          tab_hist_error_qty_zero-wemng = s_ek08rn-wemng.
          tab_hist_error_qty_zero-remng = s_ek08rn-remng.
          tab_hist_error_qty_zero-wewrt = s_ek08rn-wewrt.
          tab_hist_error_qty_zero-arewr = s_ek08rn-arewr.
          append tab_hist_error_qty_zero.
       elseif F_diff_menge lt 0 and F_diff_value gt 0
        or    F_diff_menge gt 0 and F_diff_value lt 0.
          tab_hist_error_different_sign-ebeln = s_ekpo-ebeln.
          tab_hist_error_different_sign-ebelp = s_ekpo-ebelp.
          tab_hist_error_different_sign-lfbnr = s_ek08rn-lfbnr.
          tab_hist_error_different_sign-wemng = s_ek08rn-wemng.
          tab_hist_error_different_sign-remng = s_ek08rn-remng.
          tab_hist_error_different_sign-wewrt = s_ek08rn-wewrt.
          tab_hist_error_different_sign-arewr = s_ek08rn-arewr.
          append tab_hist_error_different_sign.
      else.
          check not f_add_check is initial.
          check s_ek08rn-remng gt 0 and s_ek08rn-wemng gt 0.
          F_quot_re  = s_ek08rn-arewr / s_ek08rn-remng.
          F_quot_we  = s_ek08rn-wewrt / s_ek08rn-wemng.
          if F_quot_re ne 0.
             if F_quot_re lt F_quot_we.
                F_quot_compare = F_quot_we / F_quot_re.
             elseif F_quot_we ne 0.
                F_quot_compare = F_quot_re / F_quot_we.
             endif.
          endif.

          if F_quot_compare gt f_quot OR F_quot_re eq 0.
               tab_hist_probably_wrong-ebeln = s_ekpo-ebeln.
               tab_hist_probably_wrong-ebelp = s_ekpo-ebelp.
               tab_hist_probably_wrong-lfbnr = s_ek08rn-lfbnr.
               tab_hist_probably_wrong-wemng = s_ek08rn-wemng.
               tab_hist_probably_wrong-remng = s_ek08rn-remng.
               tab_hist_probably_wrong-wewrt = s_ek08rn-wewrt.
               tab_hist_probably_wrong-arewr = s_ek08rn-arewr.
               append tab_hist_probably_wrong.
          endif.
      endif.
endform.                        "check_differences.

form write_header.

   write:  /1   'PO number' ,
            11  'item' ,
            17  'Material doc' ,
            30  'Delivered qty' ,
            46  'Invoiced qty' ,
            61  'Deliv. value' ,
            74  'Invoices value' .

   write:  /1   'EBELN' ,
            11  'EBELP' ,
            17  'LFBNR' ,
            30  'EK08RN-WEMNG' ,
            46  'EK08RN-REMNG' ,
            61  'EK08RN-WEWRT' ,
            74  'EK08RN-AREWR' .



endform.                        "write_header.

*>>>> END OF INSERTION <<<<<<
