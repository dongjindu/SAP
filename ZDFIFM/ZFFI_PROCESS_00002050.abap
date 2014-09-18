FUNCTION ZFFI_PROCESS_00002050.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_REGUH) TYPE  REGUH
*"     VALUE(I_GJAHR) TYPE  REGUD-GJAHR
*"     VALUE(I_NACHA) TYPE  FINAA-NACHA
*"     VALUE(I_AFORN) TYPE  T042B-AFORN
*"  CHANGING
*"     VALUE(C_ITCPO) TYPE  ITCPO
*"     VALUE(C_ARCHIVE_INDEX) TYPE  TOA_DARA OPTIONAL
*"     VALUE(C_ARCHIVE_PARAMS) TYPE  ARC_PARAMS OPTIONAL
*"----------------------------------------------------------------------
DATA: lv_date(10) type c,
      lv_Subline type string.

lv_date = C_ITCPO-TDTITLE+25(10).

Concatenate 'Payment Advice from HMMA' " Added XXXXX to the subject line
            lv_date
       into lv_Subline
  separated by space.

IF I_NACHA EQ 'I'.
  C_ITCPO-TDTITLE = lv_Subline.
ENDIF.

ENDFUNCTION.
