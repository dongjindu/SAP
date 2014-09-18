* reset FM document
REPORT zrfit90 NO STANDARD PAGE HEADING
               LINE-SIZE 126
               MESSAGE-ID zmfi.

tables: bkpf, fmifiit.

parameters: P_bukrs like bkpf-bukrs memory id buk,
            p_GJAHR like bkpf-GJAHR memory id GJR.
select-options: s_belnr for fmifiit-KNBELNR.
select-options: s_volnr for fmifiit-VOBELNR.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
parameters: p_payflg as checkbox default 'X',
            p_cf     as checkbox default 'X',  "undo
*            p_cf1    as checkbox default 'X',  "select
*            p_cf2    as checkbox default 'X'.  "c/f
            p_fmn0   as checkbox default 'X',
            p_fmf0   as checkbox default ' '.
SELECTION-SCREEN END OF BLOCK b1.
parameters: p_log    as checkbox default ' '.

*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
*parameters:
*SELECTION-SCREEN END OF BLOCK b2.

data: i_bkpf like bkpf occurs 0 with header line.

select * into table i_bkpf from bkpf
   where bukrs = p_bukrs
     and gjahr = p_gjahr
     and belnr in s_belnr.
describe table i_bkpf lines sy-tabix.
check sy-tabix > 0.

if p_payflg = 'X'.
  SUBMIT RFFMDLPF
          WITH P_BUKRS  = p_bukrs
          WITH P_GJAHR  = p_gjahr
          WITH SO_BELNR in s_belnr

          WITH P_BUNDLE = 'X'
          WITH P_SINGLE = ' '

          WITH P_PAYFL  = ' '
          WITH P_DELETE = 'X'
          WITH P_AUSWHL = ' '

          WITH P_TEST   = ' '
          WITH P_S200   = ' '
  and return.
endif.

if p_cf = 'X'.
  SUBMIT RFFMC040
          WITH P_FIKRS = p_bukrs
          WITH P_GJAHR = p_gjahr

          WITH P_PAY   = 'X'
          WITH P_COM   = ' '
*           WITH S_FICTR ...
*           WITH S_FIPEX ...
*           WITH S_FONDS ...
          WITH P_WRTTP = '54'  "invoice
          WITH S_BELNR in s_belnr
          WITH P_BUKRS  = p_bukrs
          WITH P_KNGJHR = p_gjahr

          WITH P_TEST   = ' '
          WITH P_DLIST  = p_log
  and return.

*           WITH P_AVCUP ...
*           WITH P_A_ALL ...
*           WITH P_A_OPEN ...
*           WITH P_A_ZERO ...
*           WITH P_COMPL ...
*           WITH P_FUNSEL ...

endif.



if p_fmn0 = 'X'.

  SUBMIT RFFMRPFI
          WITH P_BUKRS  = p_bukrs
          WITH P_GJAHR  = p_gjahr
          WITH SO_BELNR in s_belnr
          WITH P_TEST   = ' '
          WITH P_LIST   = p_log
          WITH P_PRUEF  = ' '
          WITH P_LOESCH = 'X'
  and return.
*          WITH SO_ACTIV ...
*          WITH SO_AWTYP ...
*          WITH P_PERIOD ...
*          WITH SO_BLDAT ...
*          WITH SO_BUDAT ...
*          WITH SO_CPUDT ...

endif.

if p_cf = 'X'.
  SUBMIT RFFMC001
          WITH P_FIKRS = p_bukrs
          WITH P_GJAHR = p_gjahr

          WITH P_PAY   = 'X'
          WITH P_COM   = ' '
*           WITH S_FICTR ...
*           WITH S_FIPEX ...
*           WITH S_FONDS ...
          WITH P_WRTTP = '54'  "invoice
          WITH S_BELNR in s_belnr
          WITH P_BUKRS  = p_bukrs
          WITH P_KNGJHR = p_gjahr

          WITH P_TEST   = ' '
          WITH P_DLIST  = p_log
  and return.

*           WITH P_AVCUP ...
*           WITH P_A_ALL ...
*           WITH P_A_OPEN ...
*           WITH P_A_ZERO ...
*           WITH P_COMPL ...
*           WITH P_FUNSEL ...

  SUBMIT RFFMC010
          WITH P_FIKRS = p_bukrs
          WITH P_GJAHR = p_gjahr

          WITH P_PAY   = 'X'
          WITH P_COM   = ' '
*           WITH S_FICTR ...
*           WITH S_FIPEX ...
*           WITH S_FONDS ...
          WITH P_WRTTP = '54'  "invoice
          WITH S_BELNR in s_belnr
          WITH P_BUKRS  = p_bukrs
          WITH P_KNGJHR = p_gjahr

          WITH P_TEST   = ' '
          WITH P_DLIST  = p_log
  and return.

*           WITH P_AVCUP ...
*           WITH P_A_ALL ...
*           WITH P_A_OPEN ...
*           WITH P_A_ZERO ...
*           WITH P_COMPL ...
*           WITH P_FUNSEL ...

endif.

if p_fmf0 = 'X'.
  loop at s_volnr.
*find invoices
  endloop.

  SUBMIT RFFMS200
          WITH P_BUKRS = p_bukrs
          WITH P_GJAHR = p_gjahr
          WITH S_BELNR in s_belnr

          WITH P_PSTDAT = sy-datum

          WITH P_TEST  = ' '

          WITH P_ERR   = 'X'
          WITH P_LISTE = ' '
          WITH P_LOPEN = ' '
  and return.

*          WITH S_BUDAT ...
*          WITH P_CC ...
*          WITH P_ARCH ...
*          WITH P_CCPUDT ...
*          WITH P_CHECK ...
*          WITH P_CLEAR ...
*          WITH P_DCC ...
*          WITH P_DLCLR ...
*          WITH P_DLOPEN ...
*          WITH P_FIKRS ...
*          WITH P_HHM ...
*          WITH P_NO_PL ...
*          WITH P_OPEN ...
*          WITH P_PP_GL ...
*          WITH P_SPLIT ...
*          WITH S_BNS201 ...
*          WITH S_CPUDT ...

endif.
