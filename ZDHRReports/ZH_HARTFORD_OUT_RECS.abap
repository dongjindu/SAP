*----------------------------------------------------------------------*
*   INCLUDE ZH_HARTFORD_OUT_RECS                                       *
*----------------------------------------------------------------------*
*
*-----------------------------------------------------------------------
* Name: ZH_HARTFORD_OUT_RECS - data record definition.
* Tech. Resource: Hassan A. Siddiqui
* Desc: Hartford Single Client Feed (SCF) outbound (from SAP) records.
*       SCF consists of PAYROLL and INDICATIVE (MASTER DATA) files.
*
* Note: Most records will be a concatenation of record CONST and
*       another, though there are noted exceptions.
*-----------------------------------------------------------------------
* Modification Log
*-----------------------------------------------------------------------
* Date      Pgmr ID   mod ID  Description
* --------  --------  ------  ------------------------------------------
*
*-----------------------------------------------------------------------
* Commonly used fields by the interfaces to build records.
*-----------------------------------------------------------------------
data: begin of saved,
  pernr like pernr-pernr,
  perid like p0002-perid.
data: end of saved.


* Constant record, to be used with each of the data records to make the
* actual record for the file. Each record has a 'constant' portion and
* that's what this record is for.
data: begin of const,
  plannum(5),		"DC Plan #
  batch(4),		"batch group ID/filler on some (blank)
  f1(3),		"Filler
  perid(11),		"SSN
  f2(6),		"Filler
  recid(2).		"Record Identifier
data: end of const.
*

*------------------------------------------------------------*
*   Records used on the HARTFORD Feed                        *
*------------------------------------------------------------*
*Hartford Rec.
data: begin of rec_01 occurs 0,
      new_ssn(9),             "S.S
      nachn(30),            "Last Name
      vorna(30),            "First Name
      f1(1),
      addrline(40),	       "Address
      addrline2(40),
      ort01(30),		"City
      state(2),		"State
      pstlz(5),		"Zip Code
      pstlz2(5),		"Zip+4
      country(3),
      f2(2),
      forprovi(30),
      birtdate(8),	        "Date of birth (yyyymmdd)
      f3(10),
      status(20),
      status_date type D,
      hiredate(8),	         "Hire date
      salryclass(2),          "Salary class
      comp_date(8),           "Scheduled annual base pay effective date
      comp_amt(9),	         "Scheduled annual base pay
      comp_code(1),           "Salary Basis
      worktax(2),             "Work State Code
      std_date(8),	         "STD Coverage Effective date
      std_term(8),            "O STD coverage Term Date
      f11(5),                 "O STD Options
      ltd_date(8),            "LTD Coverage Effective Date
      f12(8),                 "O LTD Coverage Term Date
      f13(5),                 "O LTD Options
      f14(24),                "O ID # Team Member #
      f15(24),                "O/R Badge ID
      f16(10),                "O Work Phone
      f17(10),                "O Fax #
      f18(10),                "O Mobile #
      eemail(50),             "O E mail
      jobtitle(25),           "O Job Title
      Exempt(1),              "O Exempt
      f22(11),                "O HR Partner I.D
      f23(1),                 "O Union Member
      superid(10),            "O/R Supevisor's ID
      costcen(10),         "O Department ID
      workcen(10),            "O Division ID
      f27(10),                "O Location ID
      f28(10),                "O Account ID
      f29(10),                "O Region ID
      f30(10),                "O Company ID
      f31(30),                "O/R supervisor Last Name
      f32(30),                "O/R Supervisors First Name
      weekhrs(4),             "O Hours Per Week
      f34(6),                 "O Last 12 Month Hour
      gesch(1),		   "Gender
      highce(1),              "O Key EE Indicator
      f36(4),                 "O Personal Leave Hours Remain
      f37(4),                 "O Vacations Hours Remaining
      f38(1),                 "O/R FMLA Indicator
      f39(10),                "O Payroll Code
      f40(10),                "O Client Specified
      f41(10),                "O Client Specified
      f42(20),                "O Client Specified
      f43(20),                "O Client Specified
      f44(40),                "O Client Specified
      f45(40),                "O Client Specified
*      f46(50),                "O Client Specified
      supermail(50).

data: end of rec_01.
