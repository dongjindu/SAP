************************************************************************
* Program Name      : ZISD_FTZ_REPORT
* Author            : Haseeb Mohammad
* Creation Date     : 2007-03-08
* Specifications By : Lance younce
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : FTZ Report
*
*
* Modification Logs
* Date           Developer         Transport   Description
* 03/12/2007     Haseeb            UD1K940022  Initial Coding
* 03/22/2007     Manju             UD1K940152  Include Destination
*                                              code in selection screen
************************************************************************


REPORT  ZISD_FTZ_REPORT MESSAGE-ID ZMMM line-size 180          .

TABLES : AUSP,VBFA.

DATA   : ZATINN     LIKE AUSP-ATINN,
         ZRP_M_YEAR LIKE AUSP-ATINN,
         ZRP_MI     LIKE AUSP-ATINN,
         ZRP_VIN    LIKE AUSP-ATINN,
         ZRP_BL     LIKE AUSP-ATINN,
         ZRP_Vc     LIKE AUSP-ATINN,
         ZRP_VN     LIKE AUSP-ATINN,
         ZRP_18     LIKE AUSP-ATINN,
         ZRP_25     LIKE AUSP-ATINN,
         ZRP_27     LIKE AUSP-ATINN,
         ZRP_OCN    LIKE AUSP-ATINN,
         ZRP_DES    LIKE AUSP-ATINN,
         N(12) TYPE C.

data:  w_date_time(14) type c.
ranges: r_dt for w_date_time,
        r_dt1 for w_date_time,
        r_dt2 for w_date_time.



DATA   : BEGIN OF IT_AUSP OCCURS 0,
           OBJEK(10) TYPE C,
           ATINN LIKE AUSP-ATINN,
           ATWRT LIKE AUSP-ATWRT,
         END OF IT_AUSP,


         BEGIN OF IT_OUTPUT OCCURS 0,
           FSC(20)  TYPE C,
           VIN      LIKE AUSP-ATWRT,
           VESLCOD  LIKE AUSP-ATWRT,
           VESLNO   LIKE AUSP-ATWRT,
           SIGNOFF  LIKE AUSP-ATWRT,
           SHIPOUT  LIKE AUSP-ATWRT,
           INVOICE  LIKE VBFA-VBELN,
           BLNO     LIKE AUSP-ATWRT,
         END OF IT_OUTPUT,

         BEGIN OF IT_VBFA OCCURS 0,
          VBELV  LIKE  VBFA-VBELV,
          VBELN  LIKE  VBFA-VBELN,
         END OF IT_VBFA,


         BEGIN OF wa_output OCCURS 0,
          OBJEK(10)  TYPE C,
          M_YEAR LIKE AUSP-ATWRT,
          MI     LIKE AUSP-ATWRT,
          VIN    LIKE AUSP-ATWRT,
          BL     LIKE AUSP-ATWRT,
          VC     LIKE AUSP-ATWRT,
          VN     LIKE AUSP-ATWRT,
          RP18     LIKE AUSP-ATWRT,
          RP25     LIKE AUSP-ATWRT,
          RP27     LIKE AUSP-ATWRT,
          OCN    LIKE AUSP-ATWRT,
          DES    LIKE AUSP-ATWRT,
         END OF wa_output.


SELECTION-SCREEN BEGIN OF BLOCK B1 with frame title text-001.
SELECT-OPTIONS: S_OBJEK FOR AUSP-OBJEK NO INTERVALS,
                S_FSC   FOR AUSP-ATWRT NO INTERVALS,
                s_model FOR AUSP-ATWRT NO INTERVALS,
                s_dest  for AUSP-ATWRT NO INTERVALS, "UD1K940152
                s_VIN   FOR AUSP-ATWRT NO INTERVALS,
                s_VCODE FOR AUSP-ATWRT NO INTERVALS,
                s_VESNO FOR AUSP-ATWRT NO INTERVALS,
                s_BLNO  FOR AUSP-ATWRT NO INTERVALS.
SELECT-OPTIONS :s_RP18 FOR SY-DATUM,
                s_RP25 FOR SY-DATUM,
                s_RP27 FOR SY-DATUM.
SELECTION-SCREEN END OF BLOCK B1.


INITIALIZATION.


START-OF-SELECTION.
* Read Data
  PERFORM READ_DATA.
* Apply filters
  PERFORM FORMAT_DATA.
* Write Output in form of List
  PERFORM WRITE_DATA.


END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM READ_DATA.

  DATA:
         BEGIN OF IT_WO OCCURS 0,
          vbeln like vbfa-vbeln,
         END OF IT_WO,

         FLAG TYPE C.

  DATA: WA_ATNAM LIKE CABN-ATNAM.

* Get Charactertics values
  PERFORM GET_ATINN_VAR.

* If Object Number is entered
  if not S_OBJEK   is initial.
    SELECT OBJEK ATINN ATWRT INTO TABLE IT_AUSP FROM AUSP
*      up to p_no rowS
       WHERE OBJEK in  S_OBJEK  and
             ATINN IN (ZRP_M_YEAR,ZRP_MI,ZRP_VIN,ZRP_BL,
                    ZRP_VN,ZRP_VC,ZRP_18,ZRP_25,
                    ZRP_27,ZRP_OCN,ZRP_DES) and
             klart eq '002'
        order by objek.

* If destination code is entered
 elseif not s_dest[] is initial.

  SELECT OBJEK ATINN ATWRT INTO TABLE IT_AUSP FROM AUSP
      WHERE
          klart eq '002'  and
           objek in ( select objek from ausp
                        where ( klart eq '002'  and
                               ATINN in (ZRP_DES) and
                               atwrt in s_dest )   )  and
            ATINN IN (ZRP_M_YEAR,ZRP_MI,ZRP_VIN,ZRP_BL,
                         ZRP_VN,ZRP_VC,ZRP_18,ZRP_25,
                        ZRP_27,ZRP_OCN,ZRP_DES)
           order by objek.


* if VIN number is entered
  elseif  not   s_VIN[] is initial.

    SELECT OBJEK ATINN ATWRT INTO TABLE IT_AUSP FROM AUSP
      WHERE
          klart eq '002'  and
           objek in ( select objek from ausp
                        where ( klart eq '002'  and
                               ATINN in (ZRP_VIN) and
                               atwrt in s_VIN )   )  and
            ATINN IN (ZRP_M_YEAR,ZRP_MI,ZRP_VIN,ZRP_BL,
                         ZRP_VN,ZRP_VC,ZRP_18,ZRP_25,
                        ZRP_27,ZRP_OCN,ZRP_DES)
           order by objek.

* If Vessel Number is Entered
  elseif not s_VESNO[] is initial.

    SELECT OBJEK ATINN ATWRT INTO TABLE IT_AUSP FROM AUSP
      WHERE
          klart eq '002'  and
           objek in ( select objek from ausp
                        where ( klart eq '002'  and
                               ATINN in (ZRP_VN) and
                               atwrt in s_VESNO )   )  and
            ATINN IN (ZRP_M_YEAR,ZRP_MI,ZRP_VIN,ZRP_BL,
                         ZRP_VN,ZRP_VC,ZRP_18,ZRP_25,
                        ZRP_27,ZRP_OCN,ZRP_DES)
           order by objek.

* If Entry Number is entered
  elseif not s_blNo[] is initial..

    SELECT OBJEK ATINN ATWRT INTO TABLE IT_AUSP FROM AUSP
      WHERE
          klart eq '002'  and
           objek in ( select objek from ausp
                        where ( klart eq '002'  and
                               ATINN in (ZRP_BL) and
                               atwrt in s_blNo )   )  and
            ATINN IN (ZRP_M_YEAR,ZRP_MI,ZRP_VIN,ZRP_BL,
                         ZRP_VN,ZRP_VC,ZRP_18,ZRP_25,
                        ZRP_27,ZRP_OCN,ZRP_DES)
           order by objek.

* if model year is entered

  elseif  not   s_model[] is initial.

    SELECT OBJEK ATINN ATWRT INTO TABLE IT_AUSP FROM AUSP
      WHERE
          klart eq '002'  and
           objek in ( select objek from ausp
                        where ( klart eq '002'  and
                               ATINN in (ZRP_M_YEAR) and
                               atwrt in s_model )   )  and
            ATINN IN (ZRP_M_YEAR,ZRP_MI,ZRP_VIN,ZRP_BL,
                         ZRP_VN,ZRP_VC,ZRP_18,ZRP_25,
                        ZRP_27,ZRP_OCN,ZRP_DES)
           order by objek.

* If Vessel Code is Entered
  elseif not s_VCODE[] is initial.

    SELECT OBJEK ATINN ATWRT INTO TABLE IT_AUSP FROM AUSP
      WHERE
          klart eq '002'  and
           objek in  ( select objek from ausp
                        where ( klart eq '002'  and
                               ATINN in (ZRP_VC) and
                               atwrt in s_VCODE )   )  and
            ATINN IN (ZRP_M_YEAR,ZRP_MI,ZRP_VIN,ZRP_BL,
                         ZRP_VN,ZRP_VC,ZRP_18,ZRP_25,
                        ZRP_27,ZRP_OCN,ZRP_DES)
           order by objek.

* If Reporting point 18 Date is Entered
  elseif not s_RP18[] is initial.
    r_dt-sign = 'I'.
    r_dt-option = 'BT'.
    CONCATENATE s_RP18-low '000000' INTO w_date_time.
    r_dt-low = w_date_time.
    CLEAR w_date_time.
    if not s_RP18-high is initial.
      CONCATENATE s_RP18-high '235959' INTO w_date_time.
      r_dt-high = w_date_time.
      APPEND r_dt.
    else.
      CONCATENATE s_RP18-low '235959' INTO w_date_time.
      r_dt-high = w_date_time.
      APPEND r_dt.
    endif.
    CLEAR: r_dt, w_date_time.

    SELECT OBJEK ATINN ATWRT INTO TABLE IT_AUSP FROM AUSP
      WHERE
          klart eq '002'  and
           objek in  ( select objek from ausp
                        where ( klart eq '002'  and
                               ATINN in (ZRP_18) and
                               atwrt in r_dt )   )  and
            ATINN IN (ZRP_M_YEAR,ZRP_MI,ZRP_VIN,ZRP_BL,
                         ZRP_VN,ZRP_VC,ZRP_18,ZRP_25,
                        ZRP_27,ZRP_OCN,ZRP_DES)
           order by objek.

* If Reporting point 25 Date is Entered
  elseif not s_RP25[] is initial.

    r_dt1-sign = 'I'.
    r_dt1-option = 'BT'.
    CONCATENATE s_RP25-low '000000' INTO w_date_time.
    r_dt1-low = w_date_time.
    CLEAR w_date_time.
    if not s_RP25-high is initial.
      CONCATENATE s_RP25-high '235959' INTO w_date_time.
      r_dt1-high = w_date_time.
      APPEND r_dt1.
    else.
      CONCATENATE s_RP25-low '235959' INTO w_date_time.
      r_dt1-high = w_date_time.
      APPEND r_dt1.
    endif.
    CLEAR: r_dt1, w_date_time.

    SELECT OBJEK ATINN ATWRT INTO TABLE IT_AUSP FROM AUSP
      WHERE
          klart eq '002'  and
           objek in  ( select objek from ausp
                        where ( klart eq '002'  and
                               ATINN in (ZRP_25) and
                               atwrt in r_dt1 )   )  and
            ATINN IN (ZRP_M_YEAR,ZRP_MI,ZRP_VIN,ZRP_BL,
                         ZRP_VN,ZRP_VC,ZRP_18,ZRP_25,
                        ZRP_27,ZRP_OCN,ZRP_DES)
           order by objek.

* If Reporting point 27 Date is Entered
  elseif not s_RP27[] is initial.

    r_dt2-sign = 'I'.
    r_dt2-option = 'BT'.
    CONCATENATE s_RP27-low '000000' INTO w_date_time.
    r_dt2-low = w_date_time.
    CLEAR w_date_time.
    if not s_RP27-high is initial.
      CONCATENATE s_RP27-high '235959' INTO w_date_time.
      r_dt2-high = w_date_time.
      APPEND r_dt2.
    else.
      CONCATENATE s_RP27-low '235959' INTO w_date_time.
      r_dt2-high = w_date_time.
      APPEND r_dt2.
    endif.
    CLEAR: r_dt2, w_date_time.

    SELECT OBJEK ATINN ATWRT INTO TABLE IT_AUSP FROM AUSP
      WHERE
          klart eq '002'  and
           objek in  ( select objek from ausp
                        where ( klart eq '002'  and
                               ATINN in (ZRP_27) and
                               atwrt in r_dt2 )   )  and
            ATINN IN (ZRP_M_YEAR,ZRP_MI,ZRP_VIN,ZRP_BL,
                         ZRP_VN,ZRP_VC,ZRP_18,ZRP_25,
                        ZRP_27,ZRP_OCN,ZRP_DES)
           order by objek.

* If Nothing is entered - Do full Table Scan
  else.
    SELECT OBJEK ATINN ATWRT INTO TABLE IT_AUSP FROM AUSP
       WHERE   ATINN IN (ZRP_M_YEAR,ZRP_MI,ZRP_VIN,ZRP_BL,
                         ZRP_VN,ZRP_VC,ZRP_18,ZRP_25,
                         ZRP_27,ZRP_OCN,ZRP_DES) and
               klart eq '002'
               order by objek.

  endif.


* Collect Unique Object number to get Billing Details
  Loop at it_AUSP.
    it_WO-vbeln = it_ausp-objek.
    collect it_wo.
  endloop.

* Get Latest Billing number for the above selected Object number
  if not it_wo[] is initial.
    SELECT VBELV  VBELN  INTO TABLE IT_VBFA FROM VBFA
     FOR ALL ENTRIES IN IT_WO WHERE VBELV = IT_WO-VBELN  and
*                                    vbeln like '9%'.
                                    VBTYP_N eq 'M'.
  endif.

* Retain latest billing number by deleting all other billing
* number for the same Object NUmber ( Which is Sales Order)
  SORT IT_VBFA DESCENDING BY VBELV VBELN.
  DELETE ADJACENT DUPLICATES FROM IT_VBFA COMPARING VBELV.

  SORT IT_VBFA  BY VBELV .


* Populate Final Output Table
  loop at it_ausp.
    case it_ausp-atinn.
* Model Year
      when ZRP_M_YEAR.
        wa_output-M_YEAR =  it_ausp-ATWRT.
* MI
      when ZRP_MI.
        wa_output-MI =  it_ausp-ATWRT.
* VIN Number
      when ZRP_VIN.
        wa_output-VIN =  it_ausp-ATWRT.
* BOL
      when ZRP_BL.
        wa_output-BL =  it_ausp-ATWRT.
* Vessel Number
      when ZRP_VN.
        wa_output-VN =  it_ausp-ATWRT.
* Vessel code
      when  ZRP_VC.
        wa_output-VC =  it_ausp-ATWRT.
* Reporting Point 18
      when  ZRP_18.
        wa_output-RP18 =  it_ausp-ATWRT.
* Reporting  point 25
      when  ZRP_25.
        wa_output-RP25 =  it_ausp-ATWRT.
* Reporting point 27
      when  ZRP_27.
        wa_output-RP27 =  it_ausp-ATWRT.
* OCN
      when ZRP_OCN.
        wa_output-OCN =  it_ausp-ATWRT.
* Destination code
      when ZRP_DES.
        wa_output-DES =  it_ausp-ATWRT.
    endcase.

    at end of objek.
* FSC CoDE ( Model Year + DESTINATION CODE + MI + OCN )
      concatenate wa_output-M_Year wa_output-DES
                  wa_output-MI wa_output-OCN into it_output-FSC.
      it_output-VIN      =   wa_output-vin.
      it_output-VESLCOD  =   wa_output-VC.
      it_output-VESLNO   =   wa_output-vn.
      it_output-SIGNOFF  =   wa_output-RP18+0(8). "Only Date no Time
      it_output-SHIPOUT  =   wa_output-RP25+0(8). "Only Date No time
* Get Billing number for the given Object Number
      read table IT_VBFA with key vbelv  = it_ausp-objek binary search.
      if sy-subrc eq 0.
        it_output-INVOICE  =   IT_VBFA-VBELN.
      endif.
      it_output-BLNO     =   wa_output-BL.
      if it_output-SHIPOUT  is initial.
        it_output-SHIPOUT = wa_output-RP27+0(8).
      endif.

      append it_output.
      clear : it_output,wa_output.
    endat.


  endloop.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ATINN  " make characteristic table
*&---------------------------------------------------------------------*
FORM GET_ATINN USING    P_ZATINN.
  SELECT SINGLE atinn INTO ZATINN
     FROM cabn
     WHERE atnam = P_ZATINN.

ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  FORMAT_DATA
*&---------------------------------------------------------------------*
FORM FORMAT_DATA.

* Logic :-  First Select data based on the occurence of
* data entered(select-options) from top to bottom and then apply
*filtering to improve performance
* if you try to filter based on select-Option values entered,
* SQL Query on AUSP Table will be slow. B,cos of recursive nature
* of AUSP Table.
  if not s_FSC[] is initial.
    delete it_output where not FSC  in s_fsc.
  endif.

  if not s_vin[] is initial.
    delete it_output where not VIN  in s_VIN.
  endif.

  if not s_VCODE[] is initial.
    delete it_output where not VESLCOD  in s_VCODE.
  endif.

  if not s_VESNO[] is initial.
    delete it_output where not VESLNO   in s_VESNO.
  endif.

  if not s_BLNO[] is initial.
    delete it_output where not BLNO  in s_BLNO.
  endif.

  if not s_RP18[] is initial.
    delete it_output where not SIGNOFF  in s_RP18.
  endif.

  if not s_RP25[] is initial.
    delete it_output where not SHIPOUT  in s_RP25.
  endif.

  if not s_RP27[] is initial.
    delete it_output where not SHIPOUT  in s_RP27.
  endif.

  if not s_model[] is initial.
    delete it_output where not FSC+0(1)  in s_model.
  endif.

  if not s_dest[] is initial.
    delete it_output where not FSC+1(5)  in s_dest.
  endif.



ENDFORM.                    " FORMAT_DATA
*&---------------------------------------------------------------------*
*&      Form  WRITE_DATA
*&---------------------------------------------------------------------*
FORM WRITE_DATA.
* Heading
  format color 1.
  write :/1(20) 'FSC',
          22(20) 'VIN',
          44(20) 'Vessel code',
          66(20) 'Vessel Number',
          90(15) 'Sign off Date',
          117(15) 'Ship Out Date',
          135(15) 'Invoice No.',
          152(15) 'Entry No.'.

  sort it_output by fsc.
  format reset.
  format color 2.
* Output Data
  loop at it_output.
    write :/1(20) it_output-FSC,
           22(20) it_output-VIN,
           44(20) it_output-VESLCOD,
           66(20) it_output-VESLNO,
           90(15) it_output-SIGNOFF,
           117(15) it_output-SHIPOUT,
           135(15) it_output-INVOICE,
           152(15) it_output-BLNO.
  endloop.
ENDFORM.                    " WRITE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ATINN_VAR
*&---------------------------------------------------------------------*
FORM GET_ATINN_VAR.
  PERFORM GET_ATINN  USING 'P_MODEL_YEAR'.
  ZRP_M_YEAR = ZATINN.
  PERFORM GET_ATINN  USING 'P_MI'.
  ZRP_MI = ZATINN.
  PERFORM GET_ATINN  USING 'P_VIN'.
  ZRP_VIN = ZATINN.
  PERFORM GET_ATINN  USING 'P_BL_NO'.
  ZRP_BL = ZATINN.
  PERFORM GET_ATINN  USING 'P_VESL_NO'.
  ZRP_VN = ZATINN.
  PERFORM GET_ATINN  USING 'P_VESL_CODE'.
  ZRP_VC  = ZATINN.
  PERFORM GET_ATINN  USING 'P_RP18_ACTUAL_DATE'.
  ZRP_18 = ZATINN.
  PERFORM GET_ATINN  USING 'P_RP25_ACTUAL_DATE'.
  ZRP_25 = ZATINN.
  PERFORM GET_ATINN  USING 'P_RP27_ACTUAL_DATE'.
  ZRP_27 = ZATINN.
  PERFORM GET_ATINN  USING 'P_OCN'.
  ZRP_OCN = ZATINN.
  PERFORM GET_ATINN  USING 'P_DESTINATION_CODE'.
  ZRP_DES = ZATINN.

ENDFORM.                    " GET_ATINN_VAR
