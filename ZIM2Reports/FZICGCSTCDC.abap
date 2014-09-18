FORM CD_CALL_ZICGCST                       .
   IF   ( UPD_ZTCGCST                        NE SPACE )
     OR ( UPD_ICDTXT_ZICGCST         NE SPACE )
   .
     CALL FUNCTION 'ZICGCST_WRITE_DOCUMENT        ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTCGCST
                      = *ZTCGCST
                  N_ZTCGCST
                      = ZTCGCST
                  UPD_ZTCGCST
                      = UPD_ZTCGCST
                  UPD_ICDTXT_ZICGCST
                      = UPD_ICDTXT_ZICGCST
          TABLES  ICDTXT_ZICGCST
                      = ICDTXT_ZICGCST
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
