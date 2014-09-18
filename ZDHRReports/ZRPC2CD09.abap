*---------------------------------------------------------------------*
* Name: RPC2CD09.                                                     *
*       Cluster Directory data definition                             *
*                                                                     *
*    Country-developers are not!!! allowed to change anything in this *
*    include!!!!!!!!!!                                                *
*---------------------------------------------------------------------*
* 4.6A
* XTWPH9K000955 22.04.1999 new structure for cd-version
* 4.0C
* XDOAHRK000000 26.01.1998 definition of cd-table included
* XDOAHRK000408 18.12.1997 Table dir2 in rp-imp and rp-exp included
* 3.0A                                                                *
* YUIK114295 950101 new definition for cluster CD (CU temporarily)    *
*    Key to Cluster directory                                         *
*---------------------------------------------------------------------*
DATA: BEGIN OF CD-KEY,
        PERNR LIKE P0001-PERNR,             "key to cluster directory
      END OF CD-KEY.
*---------------------------------------------------------------------*
*    Input and Output fields used by Cluster Directory Manager        *
*---------------------------------------------------------------------*
DATA:
  BEGIN OF CD-VERSION.
*   INCLUDE STRUCTURE PC201.                             "XTWPH9K000955
    include structure pc2_cd.                            "XTWPH9K000955
*ATA:                                                    "XTWPH9K000955
*   MOLGA  LIKE T001P-MOLGA,                             "XTWPH9K000955
* END OF CD-VERSION.                                     "XTWPH9K000955
data: end of cd-version.                                 "XTWPH9K000955
*---- Cluster directory fields                                    ----*
DATA:
  CD-NEXT_SEQ    TYPE I,               "Next available seq number
  CD-LAST_PAY    TYPE D,               "Last payroll run date
  CD-TYPE.                             "Cluster type identifier

*---- Table data containing directory to PCL2 payroll results file----*
DATA: BEGIN OF RGDIR OCCURS 100.
        INCLUDE STRUCTURE PC261.
DATA: END OF RGDIR.
*---- Table data containing directory to PCL2 alternate pay data file *
DATA: BEGIN OF DIR2 OCCURS 100.                          "XDOAHRK000408
*       include structure pc260.                                     "!
        INCLUDE STRUCTURE PC261.                                     "!
DATA:   TYPE(4).                "clust.direct.type                   "!
DATA: END OF DIR2.                                       "XDOAHRK000408

DATA: BEGIN OF %%_DIR2 OCCURS 100.                       "XDOAHRK000000
*        include structure pc260.                        "XDOAHRK000555
        INCLUDE STRUCTURE PC260_OLD.                     "XDOAHRK000555
DATA:   TYPE(4).                "clust.direct.type                   "!
DATA: END OF %%_DIR2.                                    "XDOAHRK000000

DATA: BEGIN OF CD_TABLE OCCURS 100.                      "XDOAHRK000000
*       include structure pc260.                                     "!
        INCLUDE STRUCTURE PC261.                                     "!
DATA:   TYPE(4).                     "clust.direct.type              "!
DATA: END OF CD_TABLE.                                   "XDOAHRK000000

*---- Output data from directory manager                          ----*
DATA:
  BEGIN OF OCD-VERSION.
*   INCLUDE STRUCTURE PC201.                             "XTWPH9K000955
    INCLUDE STRUCTURE PC2_CD.
*ATA:                                                    "XTWPH9K000955
*   MOLGA  LIKE T001P-MOLGA,                             "XTWPH9K000955
* END OF OCD-VERSION.                                    "XTWPH9K000955
data: end of ocd-version.                                "XTWPH9K000955

DATA: BEGIN OF ORGDIR OCCURS 100.
        INCLUDE STRUCTURE PC261.
DATA:   TYPE(4).                 "clust.direct.type      "XDOAHRK000000
DATA: END OF ORGDIR.

DATA: RP-IMP-CD-SUBRC LIKE SY-SUBRC.        "return code from import
DATA: CD-SUBRC LIKE SY-SUBRC.               "return code from manager
DATA: CD-INDEX LIKE SY-INDEX.               "index of current RGDIR line

INCLUDE RPCCCD09.                      "constants for CD
INCLUDE RPCVCD09.                                         "YUIK114295
CONSTANTS VERSION_NUMBER_CU LIKE PCL2-VERSN VALUE '99'.   "XTWALRK008043
************************************************************************
* Import macro for payroll directory   (new with YKMALRK008041)
* In case of chages here, you have to change RP-IMP-C2-CU-NOBUFF also
************************************************************************
DEFINE RP-IMP-C2-CU.
  IMPORT CD-VERSION TO OCD-VERSION
         CD-NEXT_SEQ
         CD-LAST_PAY
         RGDIR
         DIR2 TO %%_DIR2                                 "XDOAHRK000000
*         dir2                                            "XDOAHRK000408
  FROM DATABASE PCL2(CU)
  ID CD-KEY USING PCL2_EXP_IMP.
  CD-VERSION-NUMBER = VERSION_NUMBER_CU.                  "XTWALRK049198
  RP-IMP-CD-SUBRC = SY-SUBRC.
  IF SY-SUBRC NE 0.
    CLEAR: CD-NEXT_SEQ,
           RGDIR,
           DIR2.                                         "XDOAHRK000408
    REFRESH: RGDIR, DIR2.                                "XDOAHRK000408
    CLEAR: %%_DIR2. REFRESH %%_DIR2.                     "XDOAHRK000000
  ELSE.
    %%_CONVERT_IMPORT_DIR2.                              "XDOAHRK000000
  ENDIF.
END-OF-DEFINITION.

************************************************************************
* Import macro for payroll directory   (new with XULALRK041164)
* Macro only !!!!!!!!! for archiving
************************************************************************
DEFINE RP-IMP-C2-CU-NOBUFF.

  IMPORT CD-VERSION TO OCD-VERSION
         CD-NEXT_SEQ
         CD-LAST_PAY
         RGDIR
         DIR2 TO %%_DIR2                                 "XDOAHRK000000
*         dir2                                            "XDOAHRK000408
  FROM DATABASE PCL2(CU)
  ID CD-KEY. "using pcl2_exp_imp.    "XULALRK041164
  RP-IMP-CD-SUBRC = SY-SUBRC.
  IF SY-SUBRC NE 0.
    CLEAR: CD-NEXT_SEQ,
           RGDIR,
           DIR2.                                         "XDOAHRK000408
    REFRESH: RGDIR, DIR2.                                "XDOAHRK000408
    CLEAR: %%_DIR2. REFRESH %%_DIR2.                     "XDOAHRK000000
  ELSE.
    %%_CONVERT_IMPORT_DIR2.                              "XDOAHRK000000
  ENDIF.
END-OF-DEFINITION.
************************************************************************
* Export macro for payroll directory   (new with YKMALRK008041)
* In case of chages here, you have to change RP-EXP-C2_CU-NOBUFF also
************************************************************************
DEFINE RP-EXP-C2-CU.
  CD-VERSION-NUMBER = VERSION_NUMBER_CU.                  "XTWALRK008043
  PCL2-VERSN = VERSION_NUMBER_CU.                         "XTWALRK008043
  MOVE: SY-SAPRL    TO CD-VERSION-SAPRL,
        SY-UNAME    TO CD-VERSION-UNAME,
        SY-DATUM    TO CD-VERSION-DATUM,
        SY-UZEIT    TO CD-VERSION-UZEIT,
        SY-REPID    TO CD-VERSION-PGMID,
        VERSC-MOLGA TO CD-VERSION-MOLGA.
  %%_CONVERT_EXPORT_DIR2.                                "XDOAHRK000000
  EXPORT CD-VERSION
         CD-NEXT_SEQ
         CD-LAST_PAY
         RGDIR
         DIR2 FROM %%_DIR2                               "XDOAHRK000000
*         dir2                                            "XDOAHRK000408
  TO DATABASE PCL2(CU)
  ID CD-KEY USING PCL2_EXP_IMP.
  IF SY-SUBRC NE 0.
    CLEAR: CD-NEXT_SEQ,
           RGDIR,
           DIR2.                                         "XDOAHRK000408
    REFRESH: RGDIR, DIR2.                                "XDOAHRK000408
    CLEAR: %%_DIR2. REFRESH %%_DIR2.                     "XDOAHRK000000
  ENDIF.
END-OF-DEFINITION.
************************************************************************
* Export macro for payroll directory   (new with XULALRK041164)
* Macro only !!!!!!!!! for archiving
************************************************************************
DEFINE RP-EXP-C2-CU-NOBUFF.
  CD-VERSION-NUMBER = VERSION_NUMBER_CU.                  "XTWALRK008043
  PCL2-VERSN = VERSION_NUMBER_CU.                         "XTWALRK008043
  MOVE: SY-SAPRL    TO CD-VERSION-SAPRL,
        SY-UNAME    TO CD-VERSION-UNAME,
        SY-DATUM    TO CD-VERSION-DATUM,
        SY-UZEIT    TO CD-VERSION-UZEIT,
        SY-REPID    TO CD-VERSION-PGMID,
        VERSC-MOLGA TO CD-VERSION-MOLGA.
  %%_CONVERT_EXPORT_DIR2.                                "XDOAHRK000000
  EXPORT CD-VERSION
         CD-NEXT_SEQ
         CD-LAST_PAY
         RGDIR
         DIR2 FROM %%_DIR2                               "XDOAHRK000000
*         dir2                                            "XDOAHRK000408
  TO DATABASE PCL2(CU)
  ID CD-KEY. "using pcl2_exp_imp.        "XULALRK041164
  IF SY-SUBRC NE 0.
    CLEAR: CD-NEXT_SEQ,
           RGDIR,
           DIR2.                                         "XDOAHRK000408
    REFRESH: RGDIR,DIR2.                                 "XDOAHRK000408
    CLEAR: %%_DIR2. REFRESH %%_DIR2.                     "XDOAHRK000000
  ENDIF.
END-OF-DEFINITION.

DEFINE %%_CONVERT_IMPORT_DIR2.                           "XDOAHRK000000
  REFRESH DIR2.                                                      "!
  LOOP AT %%_DIR2.                                                   "!
    CLEAR: DIR2.                                                     "!
    MOVE-CORRESPONDING %%_DIR2 TO DIR2.                              "!
    APPEND DIR2.                                                     "!
  ENDLOOP.                                                           "!
END-OF-DEFINITION.                                                   "!
                                                                     "!
DEFINE %%_CONVERT_EXPORT_DIR2.                                       "!
  REFRESH %%_DIR2.                                                   "!
  LOOP AT DIR2.                                                      "!
    CLEAR: %%_DIR2.                                                  "!
    MOVE-CORRESPONDING DIR2 TO %%_DIR2.                              "!
    APPEND %%_DIR2.                                                  "!
  ENDLOOP.                                                           "!
END-OF-DEFINITION.                                       "XDOAHRK000000

DEFINE RP-IMP-C2-CD.                                     "XDOAHRK000000
  RP-IMP-C2-CU.                                                      "!
END-OF-DEFINITION.                                                   "!
                                                                     "!
DEFINE RP-IMP-C2-CD-NOBUFF.                                          "!
  RP-IMP-C2-CU-NOBUFF.                                               "!
END-OF-DEFINITION.                                                   "!
                                                                     "!
DEFINE RP-EXP-C2-CD.                                                 "!
  RP-EXP-C2-CU.                                                      "!
END-OF-DEFINITION.                                       "XDOAHRK000000

DEFINE RP-EXP-C2-CD-NOBUFF.
  RP-EXP-C2-CU-NOBUFF.
END-OF-DEFINITION.
