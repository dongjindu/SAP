FUNCTION ZIM_CHANGE_DOCUMENT_LG.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTLG) LIKE  ZTLG STRUCTURE  ZTLG
*"     REFERENCE(O_ZTLG) LIKE  ZTLG STRUCTURE  ZTLG
*"----------------------------------------------------------------------
   DATA : OBJECID      LIKE    CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR : IT_CDTXT.
   OBJECID = N_ZTLG+3(15).

   CALL FUNCTION 'ZILGDOC_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    OBJECID
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTLG             =    N_ZTLG
            O_ZTLG             =    O_ZTLG
            UPD_ZTLG           =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZILGDOC      = ''
      tables
            ICDTXT_ZILGDOC     =    IT_CDTXT.

ENDFUNCTION.
