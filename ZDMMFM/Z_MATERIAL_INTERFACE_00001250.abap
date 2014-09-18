FUNCTION Z_MATERIAL_INTERFACE_00001250.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_MARA_NEW) LIKE  MARA STRUCTURE  MARA OPTIONAL
*"     VALUE(I_MARA_OLD) LIKE  MARA STRUCTURE  MARA OPTIONAL
*"     VALUE(UPD_MARA) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MARC_NEW) LIKE  MARC STRUCTURE  MARC OPTIONAL
*"     VALUE(I_MARC_OLD) LIKE  MARC STRUCTURE  MARC OPTIONAL
*"     VALUE(UPD_MARC) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MARD_NEW) LIKE  MARD STRUCTURE  MARD OPTIONAL
*"     VALUE(I_MARD_OLD) LIKE  MARD STRUCTURE  MARD OPTIONAL
*"     VALUE(UPD_MARD) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MBEW_NEW) LIKE  MBEW STRUCTURE  MBEW OPTIONAL
*"     VALUE(I_MBEW_OLD) LIKE  MBEW STRUCTURE  MBEW OPTIONAL
*"     VALUE(UPD_MBEW) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MFHM_NEW) LIKE  MFHM STRUCTURE  MFHM OPTIONAL
*"     VALUE(I_MFHM_OLD) LIKE  MFHM STRUCTURE  MFHM OPTIONAL
*"     VALUE(UPD_MFHM) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MLGN_NEW) LIKE  MLGN STRUCTURE  MLGN OPTIONAL
*"     VALUE(I_MLGN_OLD) LIKE  MLGN STRUCTURE  MLGN OPTIONAL
*"     VALUE(UPD_MLGN) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MLGT_NEW) LIKE  MLGT STRUCTURE  MLGT OPTIONAL
*"     VALUE(I_MLGT_OLD) LIKE  MLGT STRUCTURE  MLGT OPTIONAL
*"     VALUE(UPD_MLGT) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MPGD_NEW) LIKE  MPGD STRUCTURE  MPGD OPTIONAL
*"     VALUE(I_MPGD_OLD) LIKE  MPGD STRUCTURE  MPGD OPTIONAL
*"     VALUE(UPD_MPGD) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MPOP_NEW) LIKE  MPOP STRUCTURE  MPOP OPTIONAL
*"     VALUE(I_MPOP_OLD) LIKE  MPOP STRUCTURE  MPOP OPTIONAL
*"     VALUE(UPD_MPOP) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MVKE_NEW) LIKE  MVKE STRUCTURE  MVKE OPTIONAL
*"     VALUE(I_MVKE_OLD) LIKE  MVKE STRUCTURE  MVKE OPTIONAL
*"     VALUE(UPD_MVKE) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MAKT) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MARM) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MEAN) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MLAN) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_ICDTXT_MATERIAL) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_PROW) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_GESV) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_UNGV) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MAEX) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MAPE) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MKAL) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_QMAT) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(FL_UPD_TASK) TYPE  XFELD OPTIONAL
*"  TABLES
*"      T_ICDTXT_MATERIAL STRUCTURE  CDTXT OPTIONAL
*"      T_MAKT_NEW STRUCTURE  XDMAKT OPTIONAL
*"      T_MAKT_OLD STRUCTURE  XDMAKT OPTIONAL
*"      T_MARM_NEW STRUCTURE  XDMARM OPTIONAL
*"      T_MARM_OLD STRUCTURE  XDMARM OPTIONAL
*"      T_MEAN_NEW STRUCTURE  XDMEAN OPTIONAL
*"      T_MEAN_OLD STRUCTURE  XDMEAN OPTIONAL
*"      T_MLAN_NEW STRUCTURE  XDMLAN OPTIONAL
*"      T_MLAN_OLD STRUCTURE  XDMLAN OPTIONAL
*"      T_MAEX_NEW STRUCTURE  XDMAEX OPTIONAL
*"      T_MAEX_OLD STRUCTURE  XDMAEX OPTIONAL
*"      T_MAPE_NEW STRUCTURE  XDMAPE OPTIONAL
*"      T_MAPE_OLD STRUCTURE  XDMAPE OPTIONAL
*"      T_MKAL_NEW STRUCTURE  XDMKAL OPTIONAL
*"      T_MKAL_OLD STRUCTURE  XDMKAL OPTIONAL
*"      T_PROW_NEW STRUCTURE  XDPROW OPTIONAL
*"      T_PROW_OLD STRUCTURE  XDPROW OPTIONAL
*"      T_QMAT_NEW STRUCTURE  XDQMAT OPTIONAL
*"      T_QMAT_OLD STRUCTURE  XDQMAT OPTIONAL
*"      T_GESV_NEW STRUCTURE  XDGESV OPTIONAL
*"      T_GESV_OLD STRUCTURE  XDGESV OPTIONAL
*"      T_UNGV_NEW STRUCTURE  XDUNGV OPTIONAL
*"      T_UNGV_OLD STRUCTURE  XDUNGV OPTIONAL
*"----------------------------------------------------------------------
*---------------------------------------------------------------------*
* Help Desk Request No  :                                              *
*                                                                      *
*   Requested by:    Richard Davis                                     *
*   ABAP Analyst:    Manju                                             *
*                                                                      *
*                                                                      *
* Business Requirement Description:                                    *
*                                                                      *
*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >                *
*                                                                      *
* Configuration Requirements:                                          *
*     < Document any special config requirements that must exist for   *
*       this program to work correctly >
* In TCODE FIBF assign Function Module Z_MATERIAL_INTERFACE_00001250
* to Event Number 1250 which is part of BTE ( Business Transaction
* Events ). So this function module is invoked automatically whenever
* Material is Created, Changed and Deleted.
*(TCODE FIBF-> Settings ->P/S Function Modules - > of an SAP appl.     *

*                                                                      *
* Program Inputs:                                                      *
*     < Input File Path & Name >                                       *
*     < Any variants program would be typically run with >             *
*                                                                      *
* Program Outputs:                                                     *
*  If Material of Material group 'XYZ' is changed then it stores changes
* in Data CLuster  INDX with ID ZM and System date.
**  and triggers Event  "ZMM_MAT_CHANGES"  which inturn triggers
** RFC program 'XYZ' to push material changes

* Frequency of Execution:                                              *
*   o As and when Material is changed
*
*                                                                      *
* Execution Mode:                                                      *
*   o Online      - Transaction Code -                                 *
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*----------------------------------------------------------------------*
*  Date       Developer     Request        Description
*  09/09/06   Manju                        To trigger  event to push
*                                          material changes to MES
*                                          System.
* ----------------------------------------------------------------------
  DATA: INDXKEY LIKE INDX-SRTFD,    "U-UPDATE,  I-INSERT, D - Delete
        WA_INDX LIKE INDX.

  TYPES: BEGIN OF ITAB_MAT,
         MATNR like mara-matnr,
         maktx like makt-maktx,
         profl like mara-profl,
         END OF ITAB_MAT.

  data : ITAB TYPE STANDARD TABLE OF ITAB_MAT WITH NON-UNIQUE
         DEFAULT KEY INITIAL SIZE 2 with header line .

  Refresh ITAB.
* IF Material Group is "AM" - Steel Material
  if I_MARA_NEW-MATKL  EQ  'AM'.
    ITAB-matnr = I_MARA_NEW-MATNR.
    ITAB-maktx = T_MAKT_NEW-MAKTX.
    ITAB-profl = I_MARA_NEW-profl.
    append ITAB.  CLEAR ITAB.
    write sy-datum to indxkey.

  WA_INDX-AEDAT = SY-DATUM.
  WA_INDX-USERA = SY-UNAME.
  WA_INDX-PGMID = sy-repid.


* Export Material Changes to Data Cluster
 EXPORT ITAB TO DATABASE INDX(ZM) ID INDXKEY from WA_INDX.

* Raise Event
    CALL FUNCTION 'BP_EVENT_RAISE'
         EXPORTING
              EVENTID                = 'ZMM_MAT_CHANGES'
         EXCEPTIONS
              BAD_EVENTID            = 1
              EVENTID_DOES_NOT_EXIST = 2
              EVENTID_MISSING        = 3
              RAISE_FAILED           = 4
              OTHERS                 = 5.

* Give 2 Sec Delay to avoid overwriting of Buffer
* ( In case of Mass Processign Delay is required )
  WAIT UP TO  2 seconds.

  endif.
ENDFUNCTION.
