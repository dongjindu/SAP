FORM CD_CALL_ZTIDSHS                       .
   IF   ( UPD_ZTIDSHS                        NE SPACE )
     OR ( UPD_ICDTXT_ZTIDSHS         NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTIDSHS_WRITE_DOCUMENT        ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIDSHS
                      = *ZTIDSHS
                  N_ZTIDSHS
                      = ZTIDSHS
                  UPD_ZTIDSHS
                      = UPD_ZTIDSHS
                  UPD_ICDTXT_ZTIDSHS
                      = UPD_ICDTXT_ZTIDSHS
          TABLES  ICDTXT_ZTIDSHS
                      = ICDTXT_ZTIDSHS
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
