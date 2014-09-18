FUNCTION ZIM_CHANGE_DOCUMENT_CIV.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTCIVHD) LIKE  ZTCIVHD STRUCTURE  ZTCIVHD
*"     REFERENCE(O_ZTCIVHD) LIKE  ZTCIVHD STRUCTURE  ZTCIVHD
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : IT_CDTXT.
   clear : IT_CDTXT.
   objecid = N_ZTCIVHD+3(10).

   CALL FUNCTION 'ZICIVHD_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTCIVHD          =    N_ZTCIVHD
            O_ZTCIVHD          =    O_ZTCIVHD
            UPD_ZTCIVHD        =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZICIVHD      = ''
      tables
            ICDTXT_ZICIVHD          =    it_CdtXT.

ENDFUNCTION.
