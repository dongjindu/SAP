************************************************************************
* Author                 : Haseeb Mohammad
* Creation Date          : 10/06/2005
* Specifications By      : Able Kevin
* Development Request No :
* Addl documentation     :
* Description            : OTD data to HMA
* Modification Log
* Date       Developer    Request ID Description
* Description            : This interface includes data from two tables
*                          MARA and AUSP.
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 01/06/2006 Furong Wang               Creating Z-TABLE to save
*                                      outbound data
************************************************************************

REPORT ZISD_ACTIVE_WO_LIST MESSAGE-ID ZMPP.

TABLES: zkhausp, AUSP, MARA, CABN.

DATA : BEGIN OF IT_AUSP OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        ATINN LIKE AUSP-ATINN,
        ATWRT LIKE AUSP-ATWRT,
        ATFLV LIKE AUSP-ATFLV,
       END OF IT_AUSP.
DATA : BEGIN OF IT_AUSP_tmp OCCURS 0,
        OBJEK(15) type c,
        ATINN LIKE AUSP-ATINN,
        ATWRT LIKE AUSP-ATWRT,
        ATFLV LIKE AUSP-ATFLV,
       END OF IT_AUSP_tmp.

DATA : BEGIN OF it_cabn OCCURS 0,
       atinn LIKE cabn-atinn,
       atnam LIKE cabn-atnam,
       END OF it_cabn.

RANGES R_ATINN FOR AUSP-ATINN OCCURS 0.
RANGES R_MATNR FOR MARA-MATNR OCCURS 0.
RANGES R_MSTDE FOR MARA-MSTDE OCCURS 0.
RANGES r_atflv FOR ausp-atflv.
RANGES r_atnam FOR cabn-atnam.


DATA : BEGIN OF IT_MARA OCCURS 0,
         MATNR LIKE MARA-MATNR,
         MSTDE LIKE MARA-MSTDE,
       END OF IT_MARA.

DATA : W_CNT TYPE I,
       W_ATINN LIKE AUSP-ATINN,
       W_ATNAM LIKE CABN-ATNAM.

DATA: BEGIN OF IT_LIST_out occurs 0,
        REC_TYPE(3) type c,
        WO_NO(15) type c,
        Region(2) type c ,
        Port(2) type c ,
        MI(15) type c,
        OCN(4) type c,
        Extc(3) type c,
        Intc(3) type c,
        Start_PO_yr(4) type c ,
        Start_PO_mo(2) type c ,
        End_PO_yr(6) type c,
        Filler(21) type c ,
      END OF IT_LIST_out.

* Selection Screen which display area for user input

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_DATE  FOR SY-DATUM OBLIGATORY DEFAULT SY-DATUM.
SELECTION-SCREEN END OF BLOCK B1.

* selection screen flow of control.
START-OF-SELECTION.
  PERFORM INIT_MARA.
  PERFORM READ_DATA.



*
END-OF-SELECTION.
  PERFORM MAKE_FILE.
** Added by Furong
  PERFORM SAVE_TO_TABLE.
**end of addition

*&---------------------------------------------------------------------*
*&      Form  MAKE_FILE.  Makes the output file.
*&---------------------------------------------------------------------*
form MAKE_FILE.

  DATA : L_DATE(06).
  DATA : BEGIN OF IT_LIST_HEAD,
         REC_TYPE(3) type c,
         T_DATE type DATS,
         T_TIME type TIMS,
       END OF IT_LIST_HEAD,
       BEGIN OF IT_LIST_TAIL,
         REC_TYPE(3) type c,
         T_COUNT(7) type c,
         Filler(7) type c,
       END OF IT_LIST_TAIL.
  DATA : dsn(120).

  DESCRIBE TABLE it_list_out LINES w_cnt.
  IF w_cnt = 0.
    MESSAGE i000 WITH 'No Data Found'.
    STOP.
  ENDIF.

  L_DATE = sy-datum+2(6).

  CONCATENATE  '/usr/sap/EDI_SAP/'
               'HOTD' L_DATE
               '.txt'
               INTO dsn.
* make HEADER info for file.
  IT_list_head-rec_type = 'O3H'.
  it_list_head-T_date = sy-datum.
  it_list_head-T_time = SY-UZEIT.

  OPEN DATASET dsn IN TEXT MODE FOR OUTPUT.

  TRANSFER IT_LIST_HEAD to dsn.
* Transfer the data to File.
  LOOP AT it_list_out.
    OPEN DATASET dsn IN TEXT MODE FOR APPENDING.
    TRANSFER it_list_out TO dsn.
  ENDLOOP.
* make Tail info for file.
  it_list_tail-rec_type = 'O3T'.
  it_list_tail-t_count = w_cnt.
  it_list_tail-filler = '             '.

  OPEN DATASET dsn IN TEXT MODE FOR APPENDING.
  TRANSFER it_list_tail to dsn.

  CLOSE DATASET dsn.

  IF sy-subrc = 0.

    WRITE: /10 'FILE IS DOWNLOADED SUCCESSFULLY.'.
    SKIP.
    WRITE: /10 'File Name:', dsn.
    SKIP.
    WRITE: /10 'TOTAL RECORDS:', W_CNT.
  ELSE.
    FORMAT COLOR 6.
    WRITE: /10 'TOTAL RECORDS: ', W_CNT.
    SKIP.
    WRITE: /10 'FILE DOWNLOAD FAILED!'.
    FORMAT COLOR OFF.
    MESSAGE E000 WITH 'FILE DOWLOAD FAILED.'.
  ENDIF.



endform.    "MAKE_FILE END.

*&---------------------------------------------------------------------*
*&      Form  INIT_MARA  Read data from MARA.
*&---------------------------------------------------------------------*

form INIT_MARA.

  DATA: LOWTEMP(8) type c.

  REFRESH R_MSTDE. CLEAR R_MSTDE.

  select MATNR MSTDE into table it_mara from MARA  where MTART = 'WOCL'
     and LVORM NE 'X'.
*     and MSTDE in s_date.
*  APPEND it_MARA TO R_MATNR.

endform.                  "INIT_MARA END.

*&---------------------------------------------------------------------*
*&      Form  READ_DATA  Read Data from AUSP and select particular *&
*&                       fields
*&---------------------------------------------------------------------*

form READ_DATA.

  DATA : w_objek LIKE ausp-objek.
  DATA : WA_AUSP LIKE IT_AUSP.
  DATA : L_TABIX LIKE SY-TABIX.
  DATA : L_DATE  TYPE I.
  DATA : w_atwrt LIKE ausp-atwrt.
  DATA : c_date type D.
  DATA:        w_atflv(10).
  DATA : wo_no(3) type c.
  DATA : lenstr type i.
  DATA : BEGIN OF IT_LIST occurs 0,
        REC_TYPE(3) type c value 'O3D',
        WO_NO(15) type c,
        Region(2) type c value '  ',
        Port(2) type c value '  ',
        MI(15) type c,
        OCN(4) type c,
        Extc(3) type c,
        Intc(3) type c,
        PERF_YN(1) type c,
        P_NATION(3) type c,
        wo_create_date(16) type c,
        wo_modi_date(16) type c,
        Start_PO_yr(4) type c value '    ',
        Start_PO_mo(2) type c value '  ',
        End_PO_yr(6) type c,
        Filler(21) type c value '   ',
      END OF IT_LIST.
 DATA : begin of YN_AUSP occurs 0,
        objek  like ausp-objek,
        atinn like ausp-atinn,
        atwrt like ausp-atwrt,
        atflv like ausp-atflv,
        end of yn_ausp.
DATA : begin of YN_AUSP_tmp occurs 0,
        objek  like ausp-objek,

        end of yn_ausp_tmp.

  CLEAR: r_atnam, r_atnam[].

  PERFORM make_atnam USING 'P_MI'.
  PERFORM make_atnam USING 'P_OCN'.
  PERFORM make_atnam USING 'COLOREXT'.
  PERFORM make_atnam USING 'COLORINT'.
  PERFORM make_atnam USING 'P_PERF_YN'.
  PERFORM make_atnam USING 'P_WO_CREATE_DATE'.
  PERFORM make_atnam USING 'P_WO_MODI_DATE'.
  PERFORM make_atnam USING 'P_NATION'.


  REFRESH : it_cabn, r_atinn.
  CLEAR   : it_cabn, r_atinn.
* find the internal characteristic number from CABN table.
  SELECT atinn atnam INTO TABLE it_cabn
     FROM cabn
     WHERE atnam IN r_atnam.



  LOOP AT it_cabn.
    r_atinn-sign = 'I'.
    r_atinn-option = 'EQ'.
    r_atinn-low = it_cabn-atinn.
    APPEND r_atinn.
  ENDLOOP.


  REFRESH it_list. CLEAR it_list.
  c_date = sy-datum - 1.
* select the data from AUSP dependingon characteristics.
  select OBJEK ATINN ATWRT ATFLV into table IT_AUSP from       AUSP
    where  kLART = '001' and ATINN IN r_atinn .

* Convert the date into the form of AUSP-ATFLV

  REFRESH r_atflv. CLEAR r_atflv.
  LOOP AT s_date.
    r_atflv-sign   = s_date-sign.
    r_atflv-option = s_date-option.
    w_atflv = s_date-low.
    r_atflv-low = w_atflv.

    w_atflv = s_date-high.
    r_atflv-high = w_atflv.
    APPEND r_atflv. CLEAR r_atflv.
  ENDLOOP.
* Select the data from AUSP same as Date formated above i.e r_atflv.
  select OBJEK ATINN ATWRT ATFLV into table IT_AUSP_tmp from       AUSP
    where  kLART = '001'   and ATINN in r_atinn and ATFLV in r_atflv .

  SORT IT_AUSP BY OBJEK ATINN.

* MODIFIED ON 11-28-2005, by haseeb with the help of FURONG WANG *
** initiated by KEvin.-----------------------------------
* select the only data which is in production, i.e WO header., zkhausp
**  is a view.
 select objek into table yn_ausp_tmp from zkhausp where class =
'P_WOHD_001' and KLART = '001'  and atnam = 'P_PROD_FLAG' and atwrt ='Y'
.

*describe table it_ausp lines w_cnt.
*write: / 'AUSP', w_cnt.
*uline.
LOOP at yn_ausp_tmp.
  LOOP at it_ausp.
     lenstr = strlen( it_ausp-objek ).
    if lenstr > 14 .
       if it_ausp-objek+0(14) EQ yn_ausp_tmp-objek.
           yn_ausp-objek = it_ausp-objek.
           yn_ausp-atinn = it_ausp-atinn.
           yn_ausp-atwrt = it_ausp-atwrt.
           yn_ausp-atflv = it_ausp-atflv.
           append yn_ausp.
*          write: / yn_ausp-objek, yn_ausp-atinn,yn_ausp-atwrt,
*yn_ausp-atflv.
        endif.
     endif.
  endloop.
endloop.

*describe table yn_ausp_tmp lines w_cnt.
*write: / 'yn_ausp_tmp', w_cnt.
*uline.


* Make the List of all workorder and its characteristics as one record.
  LOOP AT it_MARA.


    LOOP AT yn_ausp.
      IF it_mara-MATNR EQ yn_ausp-objek.
        L_TABIX = SY-TABIX + 1.
        w_atinn = yn_ausp-atinn.
        w_atwrt = yn_ausp-atwrt.

        CASE w_atinn.
          WHEN '2305'.  "check for characteristic P_MI.
            IT_LIST-MI =  yn_ausp-atwrt.
          WHEN '3493'.  "check for characteristic P_OCN.
            IT_LIST-OCN = yn_ausp-atwrt.
          WHEN '3518'.  "check for characteristic colorext.
            IT_LIST-extc = yn_ausp-atwrt.
          WHEN '3522'.  "check for characteristic colorint.
            IT_LIST-intc = yn_ausp-atwrt.
          WHEN '2303'.  "check for characteristic P_PERF_YN.

          if w_atwrt EQ 'Y' OR w_atwrt EQ 'N' OR it_mara-matnr+0(14) =
 it_ausp-objek.
              IT_LIST-perf_yn = yn_ausp-atwrt.
           endif.
          WHEN '2801'.  "check for characteristic P_WO_CREATE_DATE.
            IT_LIST-wo_create_date = yn_ausp-atflv.
          WHEN '2803'.  "check for characteristic P_WO_MODI_DATA.
            IT_LIST-wo_modi_date = yn_ausp-atflv.
          WHEN '2304'.   "P_NATION.
            IT_LIST-p_nation = yn_ausp-atwrt.
        ENDCASE.

        CLEAR: WA_AUSP.
        READ TABLE yn_AUSP INTO WA_AUSP INDEX L_TABIX.
        IF yn_AUSP-OBJEK NE WA_AUSP-OBJEK.
          W_OBJEK = yn_AUSP-OBJEK.
          it_list-WO_NO = yn_ausp-objek.

          it_list-End_PO_yr = it_mara-mstde+0(6).
          it_list-REC_TYPE = 'O3D'.
          it_list-Region = '  '.
          it_list-port = '  '.
          it_list-Start_PO_yr = '    '.
          it_list-start_po_mo = '  '.
          it_list-filler = '                '.
          APPEND IT_LIST.
          CLEAR IT_LIST.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
* Select only those records which has P_PERF_YN property as 'Y' or 'N'

*loop at it_list.
*  write : / it_list-wo_no, it_list-mi, it_list-ocn, it_list-rec_type,
*it_list-p_nation.
*  endloop.
*describe table it_list lines w_cnt.
*write: / 'it_list', w_cnt.
*uline.

*describe table it_ausp_tmp lines w_cnt.
*write: / 'it_ausp_tmp', w_cnt.
*uline.

  LOOP AT IT_LIST.
    wo_no = IT_LIST-p_nation.
*  LOOP AT it_ausp_tmp.
*    READ table it_list with key WO_no = IT_AUSP_tmp-OBJEK .
    if wo_no = 'B28' .
       READ table it_ausp_tmp with key OBJEK = it_list-wo_no .

*      if IT_AUSP_tmp-OBJEK EQ wo_no .
      if sy-subrc = 0.
*       if  it_list-PERF_YN EQ 'Y' OR it_list-PERF_YN EQ 'N' .
*        it_list_out-WO_NO = it_list-wo_no+0(9).
          it_list_out-WO_NO = it_list-wo_no+0(14).

          it_list_out-End_PO_yr = it_list-End_PO_yr.
          it_list_out-REC_TYPE = it_list-rec_type.
          it_list_out-Region = it_list-region.
          it_list_out-port = it_list-port.
          it_list_out-Start_PO_yr = it_list-start_po_yr.
          it_list_out-start_po_mo = it_list-start_po_mo.
          it_list_out-filler = it_list-filler.
          it_list_out-MI = it_list-MI.
          it_list_out-OCN = it_list-OCN.
          it_list_out-extc = it_list-extc.
          it_list_out-intc = it_list-intc.

          APPEND it_list_out.
          CLEAR it_list_out.


*        endif.
      endif.
    endif.
  endloop.
*  ENDLOOP.
endform.  "READ_DATA END.
*&---------------------------------------------------------------------*
*&      Form  MAKE_ATNAM MAKE structure to read charactersitics value
*&                from CABN table
*&---------------------------------------------------------------------*

FORM MAKE_atnam USING  p_atnam.
  r_atnam-sign = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low = p_atnam.
  APPEND r_atnam.
endform. "MAKE_ATNAM END.
*&---------------------------------------------------------------------*
*&      Form  GET_ATNAM
*&---------------------------------------------------------------------*
FORM get_atnam USING p_atinn.
  READ TABLE it_cabn WITH KEY atinn = p_atinn.
  IF sy-subrc = 0.
    w_atnam = it_cabn-atnam.
  ENDIF.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  SAVE_TO_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form SAVE_TO_TABLE.
  DATA: IT_ACT_WOLIST LIKE TABLE OF ZTPP_ACT_WOLIST WITH HEADER LINE.
  lOOP AT it_list_out.
    MOVE-CORRESPONDING IT_LIST_OUT TO IT_ACT_WOLIST.
    IT_ACT_WOLIST-ZSDAT = SY-DATUM.
    IT_ACT_WOLIST-ZSTIM = SY-UZEIT.
    APPEND IT_ACT_WOLIST.
    CLEAR: IT_ACT_WOLIST.
  ENDLOOP.
  INSERT ZTPP_ACT_WOLIST FROM TABLE IT_ACT_WOLIST
         ACCEPTING DUPLICATE KEYS.
  IF SY-SUBRC = 0.
     COMMIT WORK.
  ELSE.
     ROLLBACK WORK.
  ENDIF.
  CLEAR: it_list_out[].
endform.                    " SAVE_TO_TABLE
