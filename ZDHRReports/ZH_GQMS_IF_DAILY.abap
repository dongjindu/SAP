REPORT zh_gqms_if_daily MESSAGE-ID zmco.

*-----------------------------------------------------------------------
* Name: ZH_GQMS_IF_2 - HR GQMS daily Interface
* Tech. Resource: Imtiaz Ahmad
* Desc: HR GQMS daily interface
*----------------------------------------------------------------------
*----------------------------------------------------------------------*
*  Title          : ZH_GQMS_IF_2
*  Author         : ig.moon
*  Creation Data  : 12/05/2007
*  Requirements by: Imtiaz Ahmad
*  Description    : HR GQMS daily interface.

************************************************************************
* CHANGE LOG
*-----------------------------------------------------------------------
* DATE      |  NAME          |Transport | Issue #  |      DESC
*-----------------------------------------------------------------------
* 2011.09.15   yn.kim         UP1K920005            ECC6.0 Upgrade.
***********************************************************************

  include ZH_GQMS_IF_DAILY_t01.
  include ZH_GQMS_IF_DAILY_f01.

INITIALIZATION.

  perform  initial_proc.

*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*

  perform data_select1.

*--------------------------------------------------------------------*
END-OF-SELECTION.
*--------------------------------------------------------------------*

  IF par_r1 EQ true.
    PERFORM create_file.
  ELSE.
    PERFORM gqms_eai.
  ENDIF.
