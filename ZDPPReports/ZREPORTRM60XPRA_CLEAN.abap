*REPORT ZREPORTRM60XPRA_CLEAN .
REPORT ZRM60XPRA_CLEAN message-id 6P.


 RANGES: R_TABLES FOR CABN-ATTAB.

*....... Hilfstabelle für Merkmalstamm ................................*
 DATA: BEGIN OF CABN_TAB OCCURS 0.
         INCLUDE STRUCTURE CABN.
 DATA: END OF CABN_TAB.

 data: begin of lt_cuobj occurs 1000,
         cuobj like ausp-objek,
      end of lt_cuobj.

 data: lt_ausp type standard table of ausp,
      init_cuobj like pbed-cuobj.


*-->init Range
 R_TABLES-OPTION = 'EQ'.
 R_TABLES-SIGN   = 'I'.
 R_TABLES-LOW    = 'CUVTAB'.
 APPEND R_TABLES.


*--> search char
 CALL FUNCTION 'CLSE_SELECT_CABN_VIA_OBJECTTAB'
     EXPORTING
          KEY_DATE       = sy-datlo
     TABLES
          T_CABN         = CABN_TAB
          T_TABLES       = R_TABLES
     EXCEPTIONS
          NO_ENTRY_FOUND = 01.


 READ TABLE CABN_TAB WITH KEY ATFEL = 'VTINT'.
 IF SY-SUBRC NE 0.
   MESSAGE E852 WITH 'SAP_VTINT'.
 ENDIF.

 select cuobj from inob into  table lt_cuobj
                        where obtab eq 'PBKO'.

 select cuobj from pbed appending table lt_cuobj
                        where     cuobj ne init_cuobj.

 read table lt_cuobj index 1.
 if sy-subrc is initial.
   sort lt_cuobj.
   delete ADJACENT DUPLICATES FROM lt_cuobj.

   select * from ausp into table lt_ausp
                     for all entries in lt_cuobj
                     where objek eq lt_cuobj-cuobj
                     and   atinn ne CABN_TAB-atinn.

   if sy-subrc is initial.
     delete ausp from table lt_ausp.
     WRITE:/ text-002, ' AUSP:', SY-DBCNT.
   endif.
 endif.
