************************************************************************
* Program Name      : ZAQM11A_INSP_SCH_CORR_ACTION
* Author            : SeungLyong, Lee
* Creation Date     : 2004.04.12.
* Specifications By : SeungLyong, Lee
* Pattern           :
* Development Request No : UD1K909909
* Addl Documentation:
* Description       : Inspection Scheduling - Lot Creation by Background
*                                             job - Scheduling
*   - Create inspection lot using scheduling(Backgroundjob)
*   - same as ZAQM03A_INSP_SCH_BACKJOB_V2
*   - Lot creation date must be input(required field)
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZAQM11A_INSP_SCH_CORR_ACTION  NO STANDARD PAGE HEADING
                                             LINE-SIZE 132  .


*&&& Data Declaration.  &&&*
TYPE-POOLS ZQMT1.  "/QM-Type group for inspection
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants

TYPE-POOLS SLIS.  "/Globale Typen für generische Listbausteine

TABLES : FELD.      "//Screen Object Structure


*-- Include Program ( Include Constants or etc)
INCLUDE : ZQM_INCLUDE_POOL01. "/Inspection Constants and etc

*//Tables;(TABLES : Table_Name  "//Table Description)
TABLES :
  ZTQM_QNS_HDR,   "/Insp. Scheduling Header : ISIR, Regualr,(MS)
  ZTQM_QNS_ITEM,  "/Insp. Scheduling Item(Material) : ISIR, Regular
  ZTQM_QNS_IT_MS. "/Insp. Scheduling Item(Material) : MS

TABLES :
  ZTQM_QNS_IT_D. "/Insp Scheduling Item(Mat.): earliest day of each item

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSCA_TIME_STAMP.   "/Time Stamp Structre.
TABLES : ZSQM_QNS_HDR.  "/Insp. Scheduling Header : ISIR, Regualr
*TABLES : ZSQM_QNS_EX_ISP,  "/Insp. Scheduling (ISIR-P001): Excel Layout
*         ZSQM_QNS_EX_ISE,  "/Insp. Scheduling (ISIR-E001): Excel Layout
*         ZSQM_QNS_EX_REG. "/Insp. Scheduling (Regular): Excel Layout

*TABLES:ZSQM_QNS_ITEM. "/Insp.ITEM for User Interface
TABLES : ZSQM_QNS_IT_D. "/for get earliest date of material item

TABLES : ZSQM_QNS_CH_ISP,  "/Insp. Scheduling (ISIR-P001):Change/Display
         ZSQM_QNS_CH_ISE,  "/Insp. Scheduling (ISIR-E001):Change/Display
         ZSQM_QNS_CH_REG. "/Insp. Scheduling (Regular): Change/Display

*- /Insp. Scheduling Patch Str  for Background Job & etc
TABLES : ZSQM_QNS_BACK. " "/Tunning - 04/06/2004


*//-- Internal tables
*-  for patch data to create inspection lot
DATA : IT_ZTQM_QNS_IT_D LIKE ZTQM_QNS_IT_D OCCURS 0 WITH HEADER LINE.

*- /Patch table  : "/Tunning - 04/06/2004
DATA : IT_ZSQM_QNS_BACK  LIKE ZSQM_QNS_BACK OCCURS 0 WITH HEADER LINE.

DATA: IT_QMAT LIKE QMAT OCCURS 0 WITH HEADER LINE.

DATA : WA_PRUEFLOS	TYPE	QPLOS,	"Inspection Lot Number
       WA_PRUEFLOS_MS	TYPE	QPLOS,	"Inspection Lot Number
       WA_RETURN	TYPE	BAPIRET2, "Return for ISIR/ Regular
       WA_RETURN_MS	TYPE	BAPIRET2. "Return parameter for MS

DATA : IT_BDC_MSG_C	LIKE	BDCMSGCOLL OCCURS 0 WITH HEADER LINE.


DATA : IT_FIELDCAT_ALV  TYPE SLIS_T_FIELDCAT_ALV.
DATA : IT_EVENTS        TYPE	SLIS_T_EVENT.

*-- BDC Mode  control
DATA : WA_BDC_MODE TYPE TB_BDCMODE VALUE 'N'.


DATA :  C_MARK TYPE C VALUE 'X'.


*// Selection screen
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME TITLE TEXT-T01.
PARAMETERS : P_DATE   TYPE DATS  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLK .

AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR '1000'.

*// Event Handling.

START-OF-SELECTION.

*  REFRESH IT_ZTQM_QNS_IT_D.   "/Tunning 04/06/2004
  REFRESH IT_ZSQM_QNS_BACK.

*-  patch records which earliest date is equal to system date.
  PERFORM PATCH_FOR_CREATE_LOT.

*  CHECK NOT IT_ZTQM_QNS_IT_D[] IS INITIAL.    "/Tunning 04/06/2004
*  SORT IT_ZTQM_QNS_IT_D BY IYEAR KATART_VH CODEGRP_VH CODE_VH
*                           ART MATNR  ASCENDING.

  CHECK NOT IT_ZSQM_QNS_BACK[] IS INITIAL.

  SORT IT_ZSQM_QNS_BACK BY IYEAR KATART_VH CODEGRP_VH CODE_VH
                           ART MATNR EONO LIFNR ASCENDING.

  PERFORM ENQUEUE_DB.

*- "/Tunning 04/06/2004
**-  Create inspection lot using customer BDC Function
*  PERFORM CREATE_INSP_LOT.

  PERFORM CREATE_INSP_LOT_V2.  "/Tunning 04/06/2004 - sllee

  PERFORM DEQUEUE_DB.

**-  Display log data. - Don't use.
*  PERFORM DISPLAY_JOB_LOG.


  CHECK NOT IT_BDC_MSG_C[] IS INITIAL.
  SET TITLEBAR 'LOG'.

*- Display job log using ALV
  PERFORM DISPLAY_JOB_LOG_ALV.

*TOP-OF-PAGE.
*  PERFORM DISPLAY_LOG_HEADER.
*
*END-OF-PAGE.

*<<<<<<<<< Start of module and subroutine >>>>>>>>>>>>>>>>>>*
*&-----------------------------------------------------------------*
*&      Form  PATCH_FOR_CREATE_LOT
*&-----------------------------------------------------------------*
FORM PATCH_FOR_CREATE_LOT.

*-- Get data from DB table(ZTQM_QNS_IT_D)
*-- patch records which earliest date is equal to system date.

**// Remarked and change SQL - Tunning 04/06/2004 - sllee : Start

*  SELECT  D~MANDT D~IYEAR D~KATART_VH D~CODEGRP_VH D~CODE_VH
*          D~ART D~MATNR D~DATUV_FD D~DATUV_LD
*       INTO CORRESPONDING FIELDS OF TABLE IT_ZTQM_QNS_IT_D
*        FROM ( ( ZTQM_QNS_HDR AS A INNER JOIN ZTQM_QNS_ITEM AS B
*          ON   A~IYEAR       = B~IYEAR
*           AND A~KATART_VH   = B~KATART_VH
*           AND A~CODEGRP_VH  = B~CODEGRP_VH
*           AND A~CODE_VH     = B~CODE_VH
*           AND A~ART         = B~ART  ) INNER JOIN ZTQM_QNS_IT_MS AS C
*          ON   B~IYEAR       = C~IYEAR
*           AND B~KATART_VH   = C~KATART_VH
*           AND B~CODEGRP_VH  = C~CODEGRP_VH
*           AND B~CODE_VH     = C~CODE_VH
*           AND B~ART         = C~ART
*           AND B~MATNR       = C~MATNR ) INNER JOIN ZTQM_QNS_IT_D AS D
*          ON   B~IYEAR       = D~IYEAR
*           AND B~KATART_VH   = D~KATART_VH
*           AND B~CODEGRP_VH  = D~CODEGRP_VH
*           AND B~CODE_VH     = D~CODE_VH
*           AND B~ART         = D~ART
*           AND B~MATNR       = D~MATNR
*          WHERE  A~H_STAT   = ZQMT1_CREATION
*            AND  B~I_STAT   = ZQMT1_CREATION
*            AND  D~DATUV_FD = P_DATE.  "/= SY-DATUM

  SELECT  *
       INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_QNS_BACK
        FROM ( ( ZTQM_QNS_HDR AS A INNER JOIN ZTQM_QNS_ITEM AS B
          ON   A~IYEAR       = B~IYEAR
           AND A~KATART_VH   = B~KATART_VH
           AND A~CODEGRP_VH  = B~CODEGRP_VH
           AND A~CODE_VH     = B~CODE_VH
           AND A~ART         = B~ART  ) INNER JOIN ZTQM_QNS_IT_MS AS C
          ON   B~IYEAR       = C~IYEAR
           AND B~KATART_VH   = C~KATART_VH
           AND B~CODEGRP_VH  = C~CODEGRP_VH
           AND B~CODE_VH     = C~CODE_VH
           AND B~ART         = C~ART
           AND B~MATNR       = C~MATNR ) INNER JOIN ZTQM_QNS_IT_D AS D
          ON   B~IYEAR       = D~IYEAR
           AND B~KATART_VH   = D~KATART_VH
           AND B~CODEGRP_VH  = D~CODEGRP_VH
           AND B~CODE_VH     = D~CODE_VH
           AND B~ART         = D~ART
           AND B~MATNR       = D~MATNR
          WHERE  A~H_STAT   = ZQMT1_CREATION
            AND  B~I_STAT   = ZQMT1_CREATION
            AND  D~DATUV_FD = P_DATE.  "/= SY-DATUM

*

**// Remarked and change SQL - Tunning 04/06/2004 - sllee : end

* added code by 100565
sort IT_ZSQM_QNS_BACK by matnr lifnr.
loop at IT_ZSQM_QNS_BACK.
delete adjacent duplicates from IT_ZSQM_QNS_BACK comparing all fields.
endloop.


ENDFORM.                    " PATCH_FOR_CREATE_LOT
*&------------------------------------------------------------------*
*&      Form  CREATE_INSP_LOT
*&------------------------------------------------------------------*
FORM CREATE_INSP_LOT.
*  DATA : LT_BDC_MSG     LIKE    BDCMSGCOLL OCCURS 10 WITH HEADER LINE,
*         LT_BDC_MSG_MS  LIKE    BDCMSGCOLL OCCURS 10 WITH HEADER LINE.
*
*  LOOP AT IT_ZTQM_QNS_IT_D.
*
*    CLEAR : ZTQM_QNS_HDR, ZTQM_QNS_ITEM, ZTQM_QNS_IT_MS.
*    CLEAR : WA_PRUEFLOS, WA_PRUEFLOS_MS,
*            WA_RETURN,   WA_RETURN_MS.
*    REFRESH : LT_BDC_MSG, LT_BDC_MSG_MS.
*
**--  Get planned data of Material for creating inspection lot
*    PERFORM GET_SELECTED_ITEM_DATA USING IT_ZTQM_QNS_IT_D-IYEAR
*                                         IT_ZTQM_QNS_IT_D-KATART_VH
*                                         IT_ZTQM_QNS_IT_D-CODEGRP_VH
*                                         IT_ZTQM_QNS_IT_D-CODE_VH
*                                         IT_ZTQM_QNS_IT_D-ART
*                                         IT_ZTQM_QNS_IT_D-MATNR
*                                CHANGING  ZTQM_QNS_HDR
*                                          ZTQM_QNS_ITEM
*                                          ZTQM_QNS_IT_MS.
*
*    CHECK  NOT ZTQM_QNS_HDR   IS INITIAL  AND
*           NOT ZTQM_QNS_ITEM  IS INITIAL  AND
*           NOT ZTQM_QNS_IT_MS IS INITIAL.
*
**-- Create Inspection lot
*    CALL FUNCTION 'Z_FQM_INSPECTION_LOT_CRT_V2'
*         EXPORTING
*              I_INSP_HDR              = ZTQM_QNS_HDR
*              I_INSP_ITEM             = ZTQM_QNS_ITEM
*              I_INSP_IT_MS            = ZTQM_QNS_IT_MS
*              I_BDC_MODE              = WA_BDC_MODE
*         IMPORTING
*              E_PRUEFLOS              = WA_PRUEFLOS
*              E_PRUEFLOS_MS           = WA_PRUEFLOS_MS
*              RETURN                  = WA_RETURN
*              RETURN_MS               = WA_RETURN_MS
*         TABLES
*              T_BDC_MSG               = LT_BDC_MSG
*              T_BDC_MSG_MS            = LT_BDC_MSG_MS
*         EXCEPTIONS
*              ERROR_DURING_CREATE_LOT = 1
*              NO_SUPPORTED_MATERIAL   = 2
*              OTHERS                  = 3.
*
*    IF SY-SUBRC <> 0.
*      PERFORM COLLECT_MSG   TABLES IT_BDC_MSG_C
*                                   LT_BDC_MSG
*                                   LT_BDC_MSG_MS
*                            USING  IT_ZTQM_QNS_IT_D.
*      CONTINUE.
*
*    ENDIF.
*
*    IF      SY-SUBRC = 0               AND
*        NOT WA_PRUEFLOS     IS INITIAL AND
*            WA_RETURN-TYPE = 'S'.
*
**-- DB Update...
*      PERFORM SET_DB_DATA_CHANGE     USING IT_ZTQM_QNS_IT_D-IYEAR
*                                           IT_ZTQM_QNS_IT_D-KATART_VH
*                                           IT_ZTQM_QNS_IT_D-CODEGRP_VH
*                                           IT_ZTQM_QNS_IT_D-CODE_VH
*                                           IT_ZTQM_QNS_IT_D-ART
*                                           IT_ZTQM_QNS_IT_D-MATNR
*                                           WA_PRUEFLOS
*                                           WA_PRUEFLOS_MS.
*
*    ELSE. "/inspection lot wasnt created
*
*      PERFORM COLLECT_MSG_N   TABLES IT_BDC_MSG_C
*                              USING  IT_ZTQM_QNS_IT_D.
*
*    ENDIF.
*
**-- Set Header Status
*    AT END OF ART.
*      PERFORM CHECK_AND_SET_HDR_STATUS
*                                    USING IT_ZTQM_QNS_IT_D-IYEAR
*                                          IT_ZTQM_QNS_IT_D-KATART_VH
*                                          IT_ZTQM_QNS_IT_D-CODEGRP_VH
*                                          IT_ZTQM_QNS_IT_D-CODE_VH
*                                          IT_ZTQM_QNS_IT_D-ART.
*
*    ENDAT.
*
*  ENDLOOP.

ENDFORM.                    " CREATE_INSP_LOT
*&------------------------------------------------------------------*
*&      Form  GET_SELECTED_ITEM_DATA
*&------------------------------------------------------------------*
FORM GET_SELECTED_ITEM_DATA USING    P_IYEAR
                                     P_KATART_VH
                                     P_CODEGRP_VH
                                     P_CODE_VH
                                     P_ART
                                     P_MATNR
                            CHANGING PS_QNS_HDR   TYPE ZTQM_QNS_HDR
                                     PS_QNS_ITEM  TYPE ZTQM_QNS_ITEM
                                     PS_QNS_IT_MS TYPE ZTQM_QNS_IT_MS.

*-   Header Data for creating Inspection lot
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF PS_QNS_HDR
     FROM ZTQM_QNS_HDR
       WHERE IYEAR       = P_IYEAR
         AND KATART_VH   = P_KATART_VH
         AND CODEGRP_VH  = P_CODEGRP_VH
         AND CODE_VH     = P_CODE_VH
         AND ART         = P_ART.

*-   ITEM(ISIR/Regular) Data
  CHECK SY-SUBRC = 0.

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF  PS_QNS_ITEM
    FROM ZTQM_QNS_ITEM
      WHERE IYEAR       = P_IYEAR
         AND KATART_VH   = P_KATART_VH
         AND CODEGRP_VH  = P_CODEGRP_VH
         AND CODE_VH     = P_CODE_VH
         AND ART         = P_ART
         AND MATNR       = P_MATNR.

*-   ITEM(MS) Data
  CHECK SY-SUBRC = 0.

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF PS_QNS_IT_MS
    FROM ZTQM_QNS_IT_MS
      WHERE IYEAR       = P_IYEAR
         AND KATART_VH   = P_KATART_VH
         AND CODEGRP_VH  = P_CODEGRP_VH
         AND CODE_VH     = P_CODE_VH
         AND ART         = P_ART
         AND MATNR       = P_MATNR.


ENDFORM.                    " GET_SELECTED_ITEM_DATA
*&----------------------------------------------------------------*
*&      Form  COLLECT_MSG
*&----------------------------------------------------------------*
FORM COLLECT_MSG TABLES   PT_BDC_MSG_C  STRUCTURE BDCMSGCOLL
                          PT_BDC_MSG    STRUCTURE BDCMSGCOLL
                          PT_BDC_MSG_MS STRUCTURE BDCMSGCOLL
                  USING   PS_QNS_IT  LIKE ZSQM_QNS_BACK.
*                 USING   PS_QNS_IT_D  LIKE IT_ZTQM_QNS_IT_D.

  CLEAR PT_BDC_MSG_C.
  CONCATENATE PS_QNS_IT-IYEAR
              PS_QNS_IT-KATART_VH
              PS_QNS_IT-CODEGRP_VH
              PS_QNS_IT-CODE_VH
              PS_QNS_IT-ART
              PS_QNS_IT-MATNR
              TEXT-E01
                INTO PT_BDC_MSG_C-MSGV1  SEPARATED BY ' : '.

  MOVE : PS_QNS_IT-MATNR TO PT_BDC_MSG_C-MSGV2. "/Material
  MOVE : 'QA01'  TO PT_BDC_MSG_C-TCODE,
         'E'     TO PT_BDC_MSG_C-MSGTYP,
         'MATNR' TO PT_BDC_MSG_C-FLDNAME. "/Item separator
*                                           for display log


  APPEND PT_BDC_MSG_C. CLEAR PT_BDC_MSG_C.

  DELETE PT_BDC_MSG  WHERE ( MSGTYP NE 'E' AND
                             MSGTYP NE 'A' ). "AND
*                             MSGTYP NE 'W' ).
  DELETE PT_BDC_MSG_MS  WHERE ( MSGTYP NE 'E' AND
                                MSGTYP NE 'A' )." AND
*                                MSGTYP NE 'W' ).

  APPEND LINES OF PT_BDC_MSG    TO PT_BDC_MSG_C.
  APPEND LINES OF PT_BDC_MSG_MS TO PT_BDC_MSG_C.

*  CLEAR PT_BDC_MSG_C.
*  MOVE : '------------------------------------' TO PT_BDC_MSG_C-MSGV1.
*  APPEND PT_BDC_MSG_C.

ENDFORM.                    " COLLECT_MSG

*&------------------------------------------------------------------*
*&      Form  ENQUEUE_DB
*&------------------------------------------------------------------*
FORM ENQUEUE_DB.
*-- Lock Header table ZTQM_QNS_HDR.
  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_QNS_HDR'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT
*            IYEAR             = ZSQM_QNS_HDR-IYEAR
*            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
*            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
*            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
*            ART               = ZSQM_QNS_HDR-ART
       EXCEPTIONS
            FOREIGN_LOCK      = 1
            SYSTEM_FAILURE    = 2
            OTHERS            = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*-- Lock Item table ZTQM_QNS_ITEM
  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_QNS_ITEM'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT
*            IYEAR             = ZSQM_QNS_HDR-IYEAR
*            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
*            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
*            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
*            ART               = ZSQM_QNS_HDR-ART
*            MATNR             =
       EXCEPTIONS
            FOREIGN_LOCK      = 1
            SYSTEM_FAILURE    = 2
            OTHERS            = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*-- Lock Item table ZTQM_QNS_IT_MS
  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_QNS_ITMS'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT
*            IYEAR             = ZSQM_QNS_HDR-IYEAR
*            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
*            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
*            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
*            ART               = ZSQM_QNS_HDR-ART
*            MATNR             =
       EXCEPTIONS
            FOREIGN_LOCK      = 1
            SYSTEM_FAILURE    = 2
            OTHERS            = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " ENQUEUE_DB
*&------------------------------------------------------------------*
*&      Form  DEQUEUE_DB
*&------------------------------------------------------------------*
FORM DEQUEUE_DB.
*-- Unlock Header table ZTQM_QNS_HDR.
  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_QNS_HDR'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT.
*            IYEAR             = ZSQM_QNS_HDR-IYEAR
*            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
*            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
*            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
*            ART               = ZSQM_QNS_HDR-ART.

*-- Unlock Item table ZTQM_QNS_ITEM
  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_QNS_ITEM'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT.
*            IYEAR             = ZSQM_QNS_HDR-IYEAR
*            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
*            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
*            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
*            ART               = ZSQM_QNS_HDR-ART.
*            MATNR             =.

*-- Unlock Item table ZTQM_QNS_IT_MS
  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_QNS_ITMS'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT.
*            IYEAR             = ZSQM_QNS_HDR-IYEAR
*            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
*            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
*            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
*            ART               = ZSQM_QNS_HDR-ART.
*            MATNR             =.


ENDFORM.                    " DEQUEUE_DB
*&------------------------------------------------------------------*
*&      Form  SET_DB_DATA_CHANGE
*&------------------------------------------------------------------*
FORM SET_DB_DATA_CHANGE USING    PS_QNS_BACK  LIKE ZSQM_QNS_BACK
                                 P_PRUEFLOS
                                 P_PRUEFLOS_MS.

*  DATA : LW_ZTQM_QNS_ITEM LIKE ZTQM_QNS_ITEM.

*- Inspection Scheduling Item(Material) : ISIR, Regular
  UPDATE  ZTQM_QNS_ITEM  SET I_STAT   = C_RELEASE
                             PRUEFLOS = P_PRUEFLOS
                             ERNAM    = SY-UNAME
                             ERDAT    = SY-DATUM
                             ERZET    = SY-UZEIT
                  WHERE IYEAR      = PS_QNS_BACK-IYEAR
                    AND KATART_VH  = PS_QNS_BACK-KATART_VH
                    AND CODEGRP_VH = PS_QNS_BACK-CODEGRP_VH
                    AND CODE_VH    = PS_QNS_BACK-CODE_VH
                    AND ART        = PS_QNS_BACK-ART
                    AND MATNR      = PS_QNS_BACK-MATNR
                    AND EONO       = PS_QNS_BACK-EONO
                    AND LIFNR      = PS_QNS_BACK-LIFNR.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    EXIT.
  ENDIF.


  CHECK NOT P_PRUEFLOS_MS IS INITIAL.

*- Insp. Scheduling Item(8910(20)-8930) : MS of ISIR/Regular
  UPDATE  ZTQM_QNS_IT_MS  SET PRUEFLOS_MS = P_PRUEFLOS_MS
                  WHERE IYEAR      = PS_QNS_BACK-IYEAR
                    AND KATART_VH  = PS_QNS_BACK-KATART_VH
                    AND CODEGRP_VH = PS_QNS_BACK-CODEGRP_VH
                    AND CODE_VH    = PS_QNS_BACK-CODE_VH
                    AND ART        = PS_QNS_BACK-ART
                    AND MATNR      = PS_QNS_BACK-MATNR
                    AND EONO       = PS_QNS_BACK-EONO
                    AND LIFNR      = PS_QNS_BACK-LIFNR.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    EXIT.
  ENDIF.

  COMMIT WORK AND WAIT.

  PERFORM DISPLAY_PROGRESS_BAR.

ENDFORM.                    " SET_DB_DATA_CHANGE
*&------------------------------------------------------------------*
*&      Form  DISPLAY_JOB_LOG
*&------------------------------------------------------------------*
FORM DISPLAY_JOB_LOG.
  DATA : LW_MSG_LIN	LIKE	CFGNL-MSGLIN.
  DATA : LW_BDC_MSG     LIKE    BDCMSGCOLL.

  LOOP AT IT_BDC_MSG_C.
    MOVE IT_BDC_MSG_C TO LW_BDC_MSG.

    AT FIRST.
      ULINE.
    ENDAT.

    IF LW_BDC_MSG-FLDNAME = 'MATNR'.
      ULINE.
      WRITE : '|', LW_BDC_MSG-MSGTYP,
              '|', LW_BDC_MSG-MSGNR,
              '|', (80) LW_BDC_MSG-MSGV1,
              '|', (40) LW_BDC_MSG-MSGV2,
              AT SY-LINSZ '|'.
      ULINE.
      CONTINUE.
    ENDIF.


    PERFORM CONVERT_BDC_MSG_TO_MSGLINE  USING IT_BDC_MSG_C
                                              LW_MSG_LIN.

    WRITE : '|',  LW_MSG_LIN,
            AT SY-LINSZ '|'.


    AT LAST.
      ULINE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " DISPLAY_JOB_LOG
*&------------------------------------------------------------------*
*&      Form  DISPLAY_LOG_HEADER
*&------------------------------------------------------------------*
FORM DISPLAY_LOG_HEADER.

  WRITE : AT (SY-LINSZ) 'Log of Inspection lot creating'(T05) CENTERED.
  NEW-LINE.
  WRITE : 'DATE : ', SY-DATUM, 'Time : ', SY-UZEIT.
  NEW-LINE.

ENDFORM.                    " DISPLAY_LOG_HEADER
*&------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&------------------------------------------------------------------*
FORM SET_PF_STATUS.

*SET PF-STATUS 'LOG'.

ENDFORM.                    " SET_PF_STATUS
*&------------------------------------------------------------------*
*&      Form  CONVERT_BDC_MSG_TO_MSGLINE
*&------------------------------------------------------------------*
FORM CONVERT_BDC_MSG_TO_MSGLINE USING  PS_BDC_MSG_C LIKE BDCMSGCOLL
                                       PW_MSG_LIN  LIKE CFGNL-MSGLIN.

  DATA : LW_ID	LIKE	SY-MSGID,
         LW_MTYPE	LIKE	SY-MSGTY,
         LW_NUMBER	LIKE	SY-MSGNO,
         LW_PAR1	LIKE	SY-MSGV1,
         LW_PAR2	LIKE	SY-MSGV2,
         LW_PAR3	LIKE	SY-MSGV3,
         LW_PAR4	LIKE	SY-MSGV4.

  LW_ID      = PS_BDC_MSG_C-MSGID.
  LW_MTYPE   = PS_BDC_MSG_C-MSGTYP.
  LW_NUMBER  = PS_BDC_MSG_C-MSGNR.
  LW_PAR1    = PS_BDC_MSG_C-MSGV1.
  LW_PAR2    = PS_BDC_MSG_C-MSGV2.
  LW_PAR3    = PS_BDC_MSG_C-MSGV3.
  LW_PAR4    = PS_BDC_MSG_C-MSGV4.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            ID      = LW_ID
            MTYPE   = LW_MTYPE
            NUMBER  = LW_NUMBER
            PAR1    = LW_PAR1
            PAR2    = LW_PAR2
            PAR3    = LW_PAR3
            PAR4    = LW_PAR4
       IMPORTING
            MSG_LIN = PW_MSG_LIN.

ENDFORM.                    " CONVERT_BDC_MSG_TO_MSGLINE
*&------------------------------------------------------------------*
*&      Form  DISPLAY_JOB_LOG_ALV
*&------------------------------------------------------------------*
FORM DISPLAY_JOB_LOG_ALV.
  DATA : LW_REPID           LIKE SY-REPID,
         LW_STRUCTURE_NAME  LIKE DD02L-TABNAME VALUE 'BDCMSGCOLL'.

  DATA : LW_LAYOUT  TYPE SLIS_LAYOUT_ALV.
  DATA : LT_FIELDCAT TYPE    SLIS_T_FIELDCAT_ALV  WITH HEADER LINE.
  DATA : LT_EVENTS   TYPE    SLIS_T_EVENT WITH HEADER LINE.

  LW_REPID = SY-REPID.

  PERFORM SET_ALV_LAYOUT USING LW_LAYOUT.

*- generate the field catalog automatically or semi-automatically by
*- calling function module 'REUSE_ALV_FIELDCATALOG_MERGE'
  PERFORM GET_FIELDCATEGORY  TABLES LT_FIELDCAT
                              USING LW_STRUCTURE_NAME.

*-- Get Event list and set events for use : List type(0~4).
  PERFORM SET_EVENTS_FOR_ALV   TABLES LT_EVENTS
                                USING '0'.  "/simple list


  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM = LW_REPID
            I_STRUCTURE_NAME   = LW_STRUCTURE_NAME
            IS_LAYOUT          = LW_LAYOUT
            IT_FIELDCAT        = LT_FIELDCAT[]
*            I_SAVE             = 'A'
            IT_EVENTS          = LT_EVENTS[]
       TABLES
            T_OUTTAB           = IT_BDC_MSG_C
       EXCEPTIONS
            PROGRAM_ERROR      = 1
            OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " DISPLAY_JOB_LOG_ALV
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCATEGORY
*&---------------------------------------------------------------------*
FORM GET_FIELDCATEGORY  TABLES PT_FIELDCAT       LIKE IT_FIELDCAT_ALV
                        USING  P_STRUCTURE_NAME  LIKE  DD02L-TABNAME.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME       = P_STRUCTURE_NAME
       CHANGING
            CT_FIELDCAT            = PT_FIELDCAT[]
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT PT_FIELDCAT.

    CASE PT_FIELDCAT-FIELDNAME.
      WHEN 'MSGTYP'.
        PT_FIELDCAT-KEY = C_MARK.
      WHEN 'MSGNR'.

      WHEN 'MSGV1'.
        PT_FIELDCAT-OUTPUTLEN = 100.
        PT_FIELDCAT-SELTEXT_S = 'Message'.
        PT_FIELDCAT-DDICTXT  = 'S'.  "/ (S)hort (M)iddle (L)ong
      WHEN 'MSGV2'.
        PT_FIELDCAT-OUTPUTLEN = 50.
        PT_FIELDCAT-SELTEXT_S = 'Material code'.
        PT_FIELDCAT-DDICTXT  = 'S'.  "/ (S)hort (M)iddle (L)ong
*     WHEN 'MSGV3'.
      WHEN 'FLDNAME'.

      WHEN OTHERS.
        PT_FIELDCAT-NO_OUT = C_MARK.

    ENDCASE.
    MODIFY PT_FIELDCAT.
  ENDLOOP.


ENDFORM.                    " GET_FIELDCATEGORY
*&------------------------------------------------------------------*
*&      Form  SET_ALV_LAYOUT
*&------------------------------------------------------------------*
FORM SET_ALV_LAYOUT USING    PW_LAYOUT TYPE	SLIS_LAYOUT_ALV.

  PW_LAYOUT-COLWIDTH_OPTIMIZE = C_MARK.

ENDFORM.                    " SET_ALV_LAYOUT
*&-------------------------------------------------------------------*
*&      Form  SET_EVENTS_FOR_ALV
*&-------------------------------------------------------------------*
FORM SET_EVENTS_FOR_ALV TABLES   PT_EVENTS LIKE	IT_EVENTS
                        USING    VALUE(P_LIST_TYPE) TYPE N.

  DATA : BEGIN OF LT_EVENT_NAMES OCCURS 10,
          EVENT(30) TYPE C,
         END OF LT_EVENT_NAMES.
  DATA : LW_EVENTS LIKE LINE OF PT_EVENTS.

  FIELD-SYMBOLS : <LW_FS>.

  CALL FUNCTION 'REUSE_ALV_EVENT_NAMES_GET'
       EXPORTING
            I_LIST_TYPE     = P_LIST_TYPE
       TABLES
            T_EVENT_NAMES   = LT_EVENT_NAMES
       EXCEPTIONS
            LIST_TYPE_WRONG = 1
            OTHERS          = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Events

  LOOP AT LT_EVENT_NAMES.
    CLEAR LW_EVENTS.
    ASSIGN (LT_EVENT_NAMES-EVENT) TO <LW_FS>.
    MOVE : <LW_FS>  TO LW_EVENTS-NAME.
    CASE <LW_FS>.
      WHEN SLIS_EV_ITEM_DATA_EXPAND   . "/ 'ITEM_DATA_EXPAND',
      WHEN SLIS_EV_REPREP_SEL_MODIFY  . "/ 'REPREP_SEL_MODIFY',
      WHEN SLIS_EV_CALLER_EXIT_AT_START . "/ 'CALLER_EXIT',
      WHEN SLIS_EV_USER_COMMAND       . "/ 'USER_COMMAND',
      WHEN SLIS_EV_TOP_OF_PAGE        . "/ 'TOP_OF_PAGE',
        LW_EVENTS-FORM = 'DISPLAY_LOG_HEADER'.
      WHEN SLIS_EV_DATA_CHANGED       . "/ 'DATA_CHANGED',
      WHEN SLIS_EV_TOP_OF_COVERPAGE   . "/ 'TOP_OF_COVERPAGE',
      WHEN SLIS_EV_END_OF_COVERPAGE   . "/ 'END_OF_COVERPAGE',
      WHEN SLIS_EV_FOREIGN_TOP_OF_PAGE . "/ 'FOREIGN_TOP_OF_PAGE',
      WHEN SLIS_EV_FOREIGN_END_OF_PAGE . "/ 'FOREIGN_END_OF_PAGE',
      WHEN SLIS_EV_PF_STATUS_SET      . "/ 'PF_STATUS_SET',
*        PT_EVENTS-FORM = 'SET_PF_STATUS'.
      WHEN SLIS_EV_LIST_MODIFY        . "/ 'LIST_MODIFY',
      WHEN SLIS_EV_TOP_OF_LIST        . "/ 'TOP_OF_LIST',
      WHEN SLIS_EV_END_OF_PAGE        . "/ 'END_OF_PAGE',
      WHEN SLIS_EV_END_OF_LIST        . "/ 'END_OF_LIST',
      WHEN SLIS_EV_AFTER_LINE_OUTPUT  . "/ 'AFTER_LINE_OUTPUT',
      WHEN SLIS_EV_BEFORE_LINE_OUTPUT . "/'BEFORE_LINE_OUTPUT',
      WHEN SLIS_EV_SUBTOTAL_TEXT      . "/'SUBTOTAL_TEXT'.
      WHEN OTHERS.
    ENDCASE.
    APPEND LW_EVENTS TO PT_EVENTS.
  ENDLOOP.

ENDFORM.                    " SET_EVENTS_FOR_ALV
*&-------------------------------------------------------------------*
*&      Form  CHECK_AND_SET_HDR_STATUS
*&-------------------------------------------------------------------*
FORM CHECK_AND_SET_HDR_STATUS USING  PS_QNS_BACK  LIKE ZSQM_QNS_BACK.

  DATA : BEGIN OF LW_ITEM ,
          IYEAR       TYPE  ZQYEAR,
          KATART_VH   TYPE  ZQKATART_VH,
          CODEGRP_VH  TYPE  ZQCODEGRP_VH,
          CODE_VH     TYPE  ZQCODE_VH,
          ART         TYPE  QPART,
          MATNR       TYPE  MATNR,
          EONO        TYPE  ZQM_EO_NO,
          LIFNR       TYPE  LIFNR,
          I_STAT      TYPE  ZQINSPSTATUS,
         END OF LW_ITEM.

*-- Check item status, creation.

  SELECT SINGLE A~IYEAR A~KATART_VH A~CODEGRP_VH
         A~CODE_VH A~ART A~MATNR A~EONO A~LIFNR
         A~I_STAT
    INTO CORRESPONDING FIELDS OF  LW_ITEM
      FROM ZTQM_QNS_ITEM AS A INNER JOIN ZTQM_QNS_ITEM AS B
        ON   A~IYEAR      = B~IYEAR
         AND A~KATART_VH  = B~KATART_VH
         AND A~CODEGRP_VH = B~CODEGRP_VH
         AND A~CODE_VH    = B~CODE_VH
         AND A~ART        = B~ART
         AND A~MATNR      = B~MATNR
         AND A~EONO       = B~EONO
         AND A~LIFNR      = B~LIFNR
       WHERE  A~IYEAR      = PS_QNS_BACK-IYEAR
         AND  A~KATART_VH  = PS_QNS_BACK-KATART_VH
         AND  A~CODEGRP_VH = PS_QNS_BACK-CODEGRP_VH
         AND  A~CODE_VH    = PS_QNS_BACK-CODE_VH
         AND  A~ART        = PS_QNS_BACK-ART
         AND  A~I_STAT     = C_CREATION.


  CHECK SY-SUBRC NE 0. "/All items was released

*- Set Header status to released

  UPDATE ZTQM_QNS_HDR   SET H_STAT = C_RELEASE
                  WHERE IYEAR      = PS_QNS_BACK-IYEAR
                    AND KATART_VH  = PS_QNS_BACK-KATART_VH
                    AND CODEGRP_VH = PS_QNS_BACK-CODEGRP_VH
                    AND CODE_VH    = PS_QNS_BACK-CODE_VH
                    AND ART        = PS_QNS_BACK-ART.

  IF  SY-SUBRC = 0.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.
    EXIT.
  ENDIF.


ENDFORM.                    " CHECK_AND_SET_HDR_STATUS
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
FORM DISPLAY_PROGRESS_BAR.
  DATA : LW_PERCENTAGE TYPE I.

  LW_PERCENTAGE = 100.

  CALL FUNCTION 'TB_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = LW_PERCENTAGE
            TEXT       = 'Updating DB!'.
ENDFORM.                    " display_progress_bar
*&-----------------------------------------------------------------*
*&      Form  COLLECT_MSG_N
*&-----------------------------------------------------------------*
FORM COLLECT_MSG_N  TABLES  PT_BDC_MSG_C STRUCTURE BDCMSGCOLL
                    USING   PS_QNS_IT    LIKE ZSQM_QNS_BACK.

  CLEAR PT_BDC_MSG_C.

  IF PS_QNS_IT-ART = C_INSP_TYPE_REGULAR.
  CONCATENATE PS_QNS_IT-IYEAR
*              PS_QNS_IT-KATART_VH
*              PS_QNS_IT-CODEGRP_VH
              PS_QNS_IT-CODE_VH
              PS_QNS_IT-ART
              PS_QNS_IT-MATNR
              PS_QNS_IT-LIFNR
              TEXT-E02                 "/'Can't create inspetion Lot!'
                INTO PT_BDC_MSG_C-MSGV1  SEPARATED BY ' : '.



  CONCATENATE :  PS_QNS_IT-MATNR
                 PS_QNS_IT-LIFNR
                      INTO PT_BDC_MSG_C-MSGV2  SEPARATED BY SPACE.

  ELSEIF PS_QNS_IT-ART = C_INSP_TYPE_ISIR.

  CONCATENATE
*              PS_QNS_IT-KATART_VH
*              PS_QNS_IT-CODEGRP_VH
              PS_QNS_IT-CODE_VH
              PS_QNS_IT-ART
              PS_QNS_IT-MATNR
              PS_QNS_IT-EONO
              PS_QNS_IT-LIFNR
              TEXT-E02                 "/'Can't create inspetion Lot!'
                INTO PT_BDC_MSG_C-MSGV1  SEPARATED BY ' : '.


    CONCATENATE :  PS_QNS_IT-MATNR
                   PS_QNS_IT-EONO
                   PS_QNS_IT-LIFNR
                      INTO PT_BDC_MSG_C-MSGV2  SEPARATED BY SPACE.

  ENDIF.

  MOVE : 'QA01'  TO PT_BDC_MSG_C-TCODE,
         'E'     TO PT_BDC_MSG_C-MSGTYP,
         'MATNR' TO PT_BDC_MSG_C-FLDNAME. "/Item separator
*                                           for display log

ENDFORM.                    " COLLECT_MSG_N
*&------------------------------------------------------------------*
*&      Form  create_insp_lot_v2
*&------------------------------------------------------------------*
FORM CREATE_INSP_LOT_V2.
  DATA : LT_BDC_MSG     LIKE    BDCMSGCOLL OCCURS 10 WITH HEADER LINE,
         LT_BDC_MSG_MS  LIKE    BDCMSGCOLL OCCURS 10 WITH HEADER LINE.

  LOOP AT IT_ZSQM_QNS_BACK.

    CLEAR : ZTQM_QNS_HDR, ZTQM_QNS_ITEM, ZTQM_QNS_IT_MS.
    CLEAR : WA_PRUEFLOS, WA_PRUEFLOS_MS,
            WA_RETURN,   WA_RETURN_MS.
    REFRESH : LT_BDC_MSG, LT_BDC_MSG_MS.

    MOVE-CORRESPONDING IT_ZSQM_QNS_BACK TO : ZTQM_QNS_HDR,
                                             ZTQM_QNS_ITEM,
                                             ZTQM_QNS_IT_MS.

    CHECK  NOT ZTQM_QNS_HDR   IS INITIAL  AND
           NOT ZTQM_QNS_ITEM  IS INITIAL  AND
           NOT ZTQM_QNS_IT_MS IS INITIAL.

*-- Create Inspection lot
    CALL FUNCTION 'Z_FQM_INSPECTION_LOT_CRT_V2'
         EXPORTING
              I_INSP_HDR              = ZTQM_QNS_HDR
              I_INSP_ITEM             = ZTQM_QNS_ITEM
              I_INSP_IT_MS            = ZTQM_QNS_IT_MS
              I_BDC_MODE              = WA_BDC_MODE
         IMPORTING
              E_PRUEFLOS              = WA_PRUEFLOS
              E_PRUEFLOS_MS           = WA_PRUEFLOS_MS
              RETURN                  = WA_RETURN
              RETURN_MS               = WA_RETURN_MS
         TABLES
              T_BDC_MSG               = LT_BDC_MSG
              T_BDC_MSG_MS            = LT_BDC_MSG_MS
         EXCEPTIONS
              ERROR_DURING_CREATE_LOT = 1
              NO_SUPPORTED_MATERIAL   = 2
              OTHERS                  = 3.

    IF SY-SUBRC <> 0.
      PERFORM COLLECT_MSG   TABLES IT_BDC_MSG_C
                                   LT_BDC_MSG
                                   LT_BDC_MSG_MS
                            USING  IT_ZSQM_QNS_BACK.
      CONTINUE.

    ENDIF.

    IF      SY-SUBRC = 0               AND
        NOT WA_PRUEFLOS     IS INITIAL AND
            WA_RETURN-TYPE = 'S'.

*-- DB Update...
*-- Changed : 04/13/2004 - sllee  : Start
      PERFORM SET_DB_DATA_CHANGE     USING IT_ZSQM_QNS_BACK
                                           WA_PRUEFLOS
                                           WA_PRUEFLOS_MS.

*      PERFORM SET_DB_DATA_CHANGE     USING IT_ZSQM_QNS_BACK-IYEAR
*                                           IT_ZSQM_QNS_BACK-KATART_VH
*                                           IT_ZSQM_QNS_BACK-CODEGRP_VH
*                                           IT_ZSQM_QNS_BACK-CODE_VH
*                                           IT_ZSQM_QNS_BACK-ART
*                                           IT_ZSQM_QNS_BACK-MATNR
*                                           WA_PRUEFLOS
*                                           WA_PRUEFLOS_MS.

*-- Changed : 04/13/2004 - sllee  : End

    ELSE. "/inspection lot wasnt created

      PERFORM COLLECT_MSG_N   TABLES IT_BDC_MSG_C
                              USING  IT_ZSQM_QNS_BACK.

    ENDIF.

*-- Set Header Status
    AT END OF ART.
      PERFORM CHECK_AND_SET_HDR_STATUS      USING IT_ZSQM_QNS_BACK.

*      PERFORM CHECK_AND_SET_HDR_STATUS
*                                    USING IT_ZSQM_QNS_BACK-IYEAR
*                                          IT_ZSQM_QNS_BACK-KATART_VH
*                                          IT_ZSQM_QNS_BACK-CODEGRP_VH
*                                          IT_ZSQM_QNS_BACK-CODE_VH
*                                          IT_ZSQM_QNS_BACK-ART.


    ENDAT.

  ENDLOOP.

ENDFORM.                    " create_insp_lot_v2
