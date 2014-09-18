*&-------------------------------------------------------------*
*& Include ZTSD_ACM_I_T
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Include ZTSD_ACM_I_T
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
***** Tables used in delete program
TABLES: ZTSD_ACM_I .       " Object Table
TABLES: ARC_BUFFER, V_ARC_USR, ADMI_RUN, ADMI_FILES.

* the handles for the archiv operations
DATA: HANDLE LIKE SY-TABIX,
      READ   LIKE SY-TABIX,
      WRITE  LIKE SY-TABIX.
DATA: COMMCNT   LIKE ARCH_USR-ARCH_COMIT,
      OBJCNT    LIKE ARCH_USR-ARCH_COMIT,
      INDEX     LIKE ARCH_USR-ARCH_INDEX,
      OBJECT_ID LIKE ARCH_IDX-OBJECT_ID,
      ARKEY     LIKE ARCH_IDX-ARCHIVEKEY,
      OFFSET    TYPE I,
      NUMBER_OF_RECORDS_READ TYPE I,
      DATA_CNT  TYPE I.
DATA  G_CURSOR     TYPE CURSOR.
DATA  G_PACKAGE    TYPE I VALUE 10000 .
DATA  DELETE_FROM_TABLE.
DATA DATA_OBJECT_ID LIKE ARCH_IDX_S-OBJ_ID.

*****   Below the object that is wrong by few
* internal tables used in delete and reload program
DATA: T_ITAB  LIKE ZTSD_ACM_I OCCURS 0 WITH HEADER LINE,
      BUFFER  TYPE ARC_BUFFER,
      S_ITAB  LIKE ZTSD_ACM_I .
DATA  ARC_STRUCT    LIKE ARC_BUFFER-RNAME VALUE
'ZTSD_ACM_I' . "Layout
DATA  ARC_TABLE     LIKE ARC_BUFFER-RNAME VALUE
'ZTSD_ACM_I' . "Table
