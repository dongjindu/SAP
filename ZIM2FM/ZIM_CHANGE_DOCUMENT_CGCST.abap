FUNCTION ZIM_CHANGE_DOCUMENT_CGCST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTCGCST) LIKE  ZTCGCST STRUCTURE  ZTCGCST
*"     REFERENCE(O_ZTCGCST) LIKE  ZTCGCST STRUCTURE  ZTCGCST
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTCGCST+3(15).

   CALL FUNCTION 'ZICGCST_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTCGCST          =    N_ZTCGCST
            O_ZTCGCST          =    O_ZTCGCST
            UPD_ZTCGCST        =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZICGCST       = ''
      tables
            ICDTXT_ZICGCST     =    it_CdtXT.

ENDFUNCTION.
