************************************************************************
* Program Name      : ZASD_PRICE_UPDATE
* Author            : Furong Wang
* Creation Date     : 01/30/2007
* Specifications By : Lance
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Update sales price
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
report ZASD_PRICE_UPDATING
       no standard page heading line-size 255.

include z_bdcrecx1.

tables: VBAK.

data: begin of it_data occurs 0,
   VBELN like vbak-VBELN,
  end of it_data.

select-options: s_VBELN for VBAK-VBELN obligatory.

start-of-selection.

  perform get_data.

  if it_data[] is initial.
    message i000(zmsd) with  'No Data'.
  else.
    loop at it_data.

      if sy-subrc <> 0. exit. endif.

      perform bdc_dynpro      using 'SAPMV45A' '0102'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'VBAK-VBELN'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=UER1'.
      perform bdc_field       using 'VBAK-VBELN'
                                    it_data-VBELN.
      perform bdc_dynpro      using 'SAPMV45A' '4001'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=ITEM'.
*perform bdc_field       using 'VBKD-BSTKD'
*                              record-BSTKD_002.
*perform bdc_field       using 'KUAGV-KUNNR'
*                              record-KUNNR_003.
*perform bdc_field       using 'KUWEV-KUNNR'
*                              record-KUNNR_004.
*perform bdc_field       using 'RV45A-KETDAT'
*                              record-KETDAT_005.
*perform bdc_field       using 'RV45A-KPRGBZ'
*                              record-KPRGBZ_006.
*perform bdc_field       using 'VBKD-PRSDT'
*                              record-PRSDT_007.
*perform bdc_field       using 'VBKD-ZTERM'
*                              record-ZTERM_008.
*perform bdc_field       using 'VBKD-INCO1'
*                              record-INCO1_009.
*perform bdc_field       using 'VBKD-INCO2'
*                              record-INCO2_010.
      perform bdc_field       using 'BDC_CURSOR'
                                    'RV45A-KWMENG(01)'.
      perform bdc_dynpro      using 'SAPMV45A' '4003'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=T\05'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'RV45A-KWMENG'.
*perform bdc_field       using 'RV45A-KWMENG'
*                              record-KWMENG_011.
*perform bdc_field       using 'VBAP-VRKME'
*                              record-VRKME_012.
*perform bdc_field       using 'RV45A-ETDAT'
*                              record-ETDAT_013.
*perform bdc_field       using 'RV45A-PRGBZ'
*                              record-PRGBZ_014.
*perform bdc_field       using 'VBKD-PRSDT'
*                              record-PRSDT_015.
      perform bdc_dynpro      using 'SAPMV45A' '5003'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=V69A_KONY'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'KOMV-KSCHL(05)'.
      perform bdc_dynpro      using 'SAPMSSY0' '0120'.
      perform bdc_field       using 'BDC_CURSOR'
                                    '05/16'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=PICK'.
      perform bdc_dynpro      using 'SAPMV45A' '5003'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '/EBACK'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'KOMV-KSCHL(05)'.
      perform bdc_dynpro      using 'SAPMV45A' '4001'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=ITEM'.
*perform bdc_field       using 'VBKD-BSTKD'
*                              record-BSTKD_016.
*perform bdc_field       using 'KUAGV-KUNNR'
*                              record-KUNNR_017.
*perform bdc_field       using 'KUWEV-KUNNR'
*                              record-KUNNR_018.
*perform bdc_field       using 'RV45A-KETDAT'
*                              record-KETDAT_019.
*perform bdc_field       using 'RV45A-KPRGBZ'
*                              record-KPRGBZ_020.
*perform bdc_field       using 'VBKD-PRSDT'
*                              record-PRSDT_021.
*perform bdc_field       using 'VBKD-ZTERM'
*                              record-ZTERM_022.
*perform bdc_field       using 'VBKD-INCO1'
*                              record-INCO1_023.
*perform bdc_field       using 'VBKD-INCO2'
*                              record-INCO2_024.
      perform bdc_field       using 'BDC_CURSOR'
                                    'RV45A-KWMENG(02)'.
      perform bdc_dynpro      using 'SAPMV45A' '5003'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=V69A_KONY'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'KOMV-KSCHL(05)'.
      perform bdc_dynpro      using 'SAPMSSY0' '0120'.
      perform bdc_field       using 'BDC_CURSOR'
                                    '05/13'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=PICK'.
      perform bdc_dynpro      using 'SAPMV45A' '5003'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=SICH'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'KOMV-KSCHL(05)'.
      perform bdc_transaction using 'VA02' it_data-VBELN.

    endloop.
  endif.
*perform close_group.
*perform close_dataset using dataset.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  select VBELN into table it_Data
    from vbak
    where VBELN in s_VBELN.
ENDFORM.                    " get_data
