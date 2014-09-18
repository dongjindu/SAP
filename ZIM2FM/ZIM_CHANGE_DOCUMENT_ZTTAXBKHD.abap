FUNCTION ZIM_CHANGE_DOCUMENT_ZTTAXBKHD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTTAXBKHD) LIKE  ZTTAXBKHD STRUCTURE  ZTTAXBKHD
*"     REFERENCE(O_ZTTAXBKHD) LIKE  ZTTAXBKHD STRUCTURE  ZTTAXBKHD
*"----------------------------------------------------------------------
   DATA : OBJECID   LIKE   CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR : IT_CDTXT.
   OBJECID = N_ZTTAXBKHD+3(10).

   CALL FUNCTION 'ZTTAXBKHD_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    OBJECID
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTTAXBKHD        =    N_ZTTAXBKHD
            O_ZTTAXBKHD        =    O_ZTTAXBKHD
            UPD_ZTTAXBKHD      =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZTTAXBKHD    = ''
      tables
            ICDTXT_ZTTAXBKHD     =    it_CdtXT.

ENDFUNCTION.
